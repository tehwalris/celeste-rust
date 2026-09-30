//! A frame in STAGES: the search can dedupe INSIDE a game frame.
//!
//! One frame of the cart is `_update()` then `_draw()`, and a kernel is one
//! such frame. Its forks multiply: the player's `move` forks on where the
//! sub-pixel position floors, and every fork after it - the buttons in the
//! player's update, the platforms and the fruit after that - is enumerated
//! once per move configuration. Most of those configurations end in the
//! same few states. Ending the kernel at the player's move and deduping
//! there lets them merge BEFORE the second half runs (room (6,0), level 0:
//! 1.13M kernel bodies -> 46k, the same frame-40 state set; plans/stages.md).
//!
//! A CUT is where a stage ends: a `_hint_normalize()` statement (the cart's
//! own merge hints, nothing added to the Lua) at which the local `obj` is an
//! instance of a chosen object type. In the cart that is the object loop's
//! hint after that object's `move` and before its `update`; the player's is
//! the one that pays. The cuts of a run are process-global (`set_cuts`), like
//! the precision level, and so are the stages: `per_frame()` = one more than
//! the cuts. EVERY state takes exactly that many steps per frame - one that
//! does not reach a cut (a frozen frame, a frame with no player) finishes its
//! frame early and then WAITS, stage by stage, so a search step is always a
//! stage and a frame always `per_frame()` steps.
//!
//! A state between stages carries its CONTINUATION (`Cont`): where it stopped,
//! as the interpreter's own stack - per level the statement it was in, the arm
//! of an `if`, the numeric `for`'s next index, the function a call statement
//! was in - with the scopes holding the locals there. The scopes stay in the
//! heap (they are GC roots of the state, so they merge and canonicalize with
//! it); the locals must be constants or references at the cut (`key`), so a
//! row needs no column for them: they are part of the SHAPE. The continuation
//! enters the shape as its `key` (`heap::Shape::cont`, `Rt2::cont`): states at
//! different places in a frame never share a kernel and never dedupe together.

use std::sync::OnceLock;

use anyhow::{anyhow, bail, ensure, Result};

use super::domain::Domain;
use super::heap::{BodyId, ScopeId, TableId, Value};
use super::iface::{self, Path, Step};
use super::state::State;

static CUTS: OnceLock<Vec<String>> = OnceLock::new();

/// Split every frame at these cuts, in order: object type globals (`player`).
/// Once per process, before any kernel is built or any cut read: the kernels,
/// the search and its trees all assume one set.
pub fn set_cuts(cuts: Vec<String>) -> Result<()> {
    let set = CUTS.get_or_init(|| cuts.clone());
    ensure!(*set == cuts, "the stage cuts are {set:?} already, not {cuts:?}: they are fixed per process");
    Ok(())
}

/// The run's cuts (none unless `set_cuts`).
pub fn cuts() -> &'static [String] {
    CUTS.get_or_init(Vec::new)
}

/// Steps per game frame: one per stage.
pub fn per_frame() -> u32 {
    cuts().len() as u32 + 1
}

/// Parse a `--cut` list: object type names, comma-separated (`player`).
pub fn parse_cuts(spec: &str) -> Vec<String> {
    spec.split(',').map(str::trim).filter(|s| !s.is_empty()).map(str::to_string).collect()
}

/// What the tracer knows while it traces one stage (`Interp::stage`).
pub struct StageCtx<D: Domain> {
    /// The stage traced: 0 from the frame's start, `k` resumed at cut `k`.
    pub stage: u32,
    pub cuts: &'static [String],
    /// `__button_states` as the stage's reset left it: at a cut the buttons
    /// must still be these (`Interp::suspend`).
    pub buttons: Vec<Value<D>>,
}

impl<D: Domain> Clone for StageCtx<D> {
    fn clone(&self) -> Self {
        StageCtx { stage: self.stage, cuts: self.cuts, buttons: self.buttons.clone() }
    }
}

/// One level of a suspended stage's stack, innermost first (`Cont::frames`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum At {
    /// Statement `index` of the enclosing block was running, in `scope`; the
    /// block goes on at `index + 1`.
    Stmt { index: u32, scope: ScopeId },
    /// The statement is an `if`, stopped in arm `0..` (the `else` last).
    Arm(u32),
    /// The statement is a numeric `for`, stopped in its body; it goes on at
    /// index `next` to `to` (raw 16.16).
    Loop { next: i32, to: i32 },
    /// The statement is a call statement, stopped in this function's body.
    Call(BodyId),
}

/// Where a state stands between two stages of its frame (`State::cont`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cont {
    /// The stage this state runs next, `1..per_frame()`.
    pub stage: u32,
    /// The cut it stopped at (1-based, `cuts()[cut - 1]`), with the stack
    /// there, innermost first. `None`: its frame is over and it waits.
    pub cut: Option<u32>,
    pub frames: Vec<At>,
    /// What the shape knows of it: the place and the locals (`key`). Set
    /// when the stage that made it ends; empty while that stage unwinds.
    pub key: String,
}

impl Cont {
    /// The same place in the program: the stack alike but for which heap
    /// scope each level runs in (the merge pairs those by their canonical
    /// order, as it pairs every other heap object).
    pub fn same_place(&self, o: &Cont) -> bool {
        self.stage == o.stage
            && self.cut == o.cut
            && self.frames.len() == o.frames.len()
            && self.frames.iter().zip(&o.frames).all(|(a, b)| match (a, b) {
                (At::Stmt { index: i, .. }, At::Stmt { index: j, .. }) => i == j,
                (a, b) => a == b,
            })
    }

    /// The scopes the stack holds, innermost first: GC roots of the state.
    pub fn scopes(&self) -> impl Iterator<Item = ScopeId> + '_ {
        self.frames.iter().filter_map(|a| match a {
            At::Stmt { scope, .. } => Some(*scope),
            _ => None,
        })
    }

    /// The continuation's hash: `Rt2::cont`, never 0 (0 is a frame boundary).
    pub fn hash(&self) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        h.write(self.key.as_bytes());
        h.finish().max(1)
    }
}

/// The cut a `_hint_normalize()` statement is, if any: the first of `cuts`
/// whose type the local `obj` is an instance of. A hint with no local `obj`,
/// or one that is not an instance of a cut's type, is no cut.
pub fn cut_at<D: Domain>(st: &State<D>, cuts: &[String]) -> Option<u32> {
    let Some(Value::Table(obj)) = st.heap.lookup(st.scope, "obj") else { return None };
    let ty = st.heap.tables.get(obj)?.hash.get("type")?;
    let globals = &st.heap.tables[&st.globals].hash;
    cuts.iter().position(|c| globals.get(c).is_some_and(|g| g == ty)).map(|i| i as u32 + 1)
}

/// The continuation's KEY, when its stage has ended: the place (the stack's
/// statement indices, arms, loop positions and functions) and every local
/// the stack holds - a constant, a string, nil, a builtin, a table by its
/// path from the globals, a closure by its body and the scope it captured.
/// Anything else refuses: a local the key cannot name would need a column of
/// the row, and a row has columns only for what the globals reach.
pub fn key<D: Domain>(st: &State<D>, d: &D, cont: &Cont) -> Result<String> {
    let mut out = format!("stage {} of {}", cont.stage, per_frame());
    let Some(cut) = cont.cut else {
        out.push_str(": frame over");
        return Ok(out);
    };
    let cut_name = cuts().get(cut as usize - 1).ok_or_else(|| anyhow!("cut {cut} of {:?}", cuts()))?;
    out.push_str(&format!(": cut {cut_name} at"));
    // The scopes in canonical order: the stack's, outermost first, then the
    // parents and closure environments they reach - up to the frame's own
    // scope (the outermost level's), which every state has and the globals'
    // row describes.
    let top = match cont.frames.last() {
        Some(At::Stmt { scope, .. }) => *scope,
        other => bail!("a continuation whose outermost level is {other:?}"),
    };
    let mut scopes: Vec<ScopeId> = Vec::new();
    for a in cont.frames.iter().rev() {
        match a {
            At::Stmt { index, scope } => {
                if *scope != top && !scopes.contains(scope) {
                    scopes.push(*scope);
                }
                out.push_str(&format!(" s{index}"));
            }
            At::Arm(i) => out.push_str(&format!(" arm{i}")),
            At::Loop { next, to } => out.push_str(&format!(" for{next:x}..{to:x}")),
            At::Call(b) => out.push_str(&format!(" call{b}")),
        }
    }
    let paths = table_paths(st);
    let mut i = 0;
    let mut locals = String::new();
    while i < scopes.len() {
        let sc = &st.heap.scopes[&scopes[i]];
        let see = |s: ScopeId, scopes: &mut Vec<ScopeId>| -> String {
            if s == top {
                return "top".into();
            }
            let k = scopes.iter().position(|x| *x == s).unwrap_or_else(|| {
                scopes.push(s);
                scopes.len() - 1
            });
            format!("#{k}")
        };
        let parent = match sc.parent {
            Some(p) => see(p, &mut scopes),
            None => "none".into(),
        };
        locals.push_str(&format!(" #{i}<{parent}>{{"));
        for (name, v) in &sc.vars {
            let val = match v {
                Value::Nil => "nil".to_string(),
                Value::Num(n) => match d.as_const(n) {
                    Some(c) => format!("{:x}", c.as_raw_u32()),
                    None => bail!("the local `{name}` at cut {cut_name} is not a constant: it would need a column of the row"),
                },
                Value::Bool(b) => match d.decide(b) {
                    Some(c) => c.to_string(),
                    None => bail!("the local `{name}` at cut {cut_name} is not a constant: it would need a column of the row"),
                },
                Value::Str(s) => format!("{s:?}"),
                Value::Builtin(b) => format!("builtin {b}"),
                Value::Table(t) => match paths.get(t) {
                    Some(p) => iface::show(p),
                    None => bail!("the local `{name}` at cut {cut_name} holds a table the globals do not reach"),
                },
                Value::Func(c) => {
                    let cl = &st.heap.closures[c];
                    let env = see(cl.env, &mut scopes);
                    format!("fn{}<{env}>", cl.body)
                }
            };
            locals.push_str(&format!("{name}={val};"));
        }
        locals.push('}');
        i += 1;
    }
    out.push_str(" |");
    out.push_str(&locals);
    Ok(out)
}

/// Every table the globals reach, by its first path in a breadth-first walk
/// (keys in order): how a key names a table a local holds.
fn table_paths<D: Domain>(st: &State<D>) -> std::collections::HashMap<TableId, Path> {
    let mut out: std::collections::HashMap<TableId, Path> = Default::default();
    let mut queue: std::collections::VecDeque<(TableId, Path)> = [(st.globals, Vec::new())].into();
    while let Some((t, p)) = queue.pop_front() {
        if out.contains_key(&t) {
            continue;
        }
        let tab = &st.heap.tables[&t];
        let mut push = |step: Step, v: &Value<D>| {
            if let Value::Table(u) = v {
                if !out.contains_key(u) {
                    let mut q = p.clone();
                    q.push(step);
                    queue.push_back((*u, q));
                }
            }
        };
        for (k, v) in &tab.hash {
            push(Step::Key(k.clone()), v);
        }
        for (i, v) in tab.arr.iter().enumerate() {
            push(Step::Idx(i), v);
        }
        for (i, v) in &tab.ints {
            push(Step::Int(*i), v);
        }
        out.insert(t, p);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num as P8;
    use crate::trace::domain::Concrete;
    use crate::trace::heap::Heap;
    use crate::trace::interp::{Flow, Interp};

    /// A frame of a toy cart with the cart's shapes: an object loop over a
    /// numeric `for` in a helper, a callback closure per object that captures
    /// a local of the frame, and the hint in an `if` arm of the callback.
    const CART: &str = r#"
        player = {}
        other = {}
        objects = {{type=other, n=1}, {type=player, n=2}, {type=other, n=3}}
        log = {}
        function each(t, f)
          local i = 1
          for step=1,10 do
            local o = t[i]
            if o == nil then break end
            f(o)
            i = i + 1
          end
        end
        function frame()
          local seen = 0
          each(objects, function(obj)
            obj.n = obj.n * 10
            seen = seen + 1
            if obj.n > 0 then
              _hint_normalize()
              log[#log + 1] = obj.n + seen
            end
          end)
          result = seen * 100 + #log
        end
    "#;

    fn fresh(d: &mut Concrete) -> State<Concrete> {
        let mut heap: Heap<Concrete> = Heap::default();
        let globals = heap.new_table();
        let scope = heap.new_scope(None);
        heap.tables.get_mut(&globals).unwrap().hash.insert("_hint_normalize".into(), Value::Builtin("_hint_normalize"));
        let (t, f) = (d.boolean(true), d.boolean(false));
        State { heap, globals, scope, stack: Vec::new(), guard: t, ended: f, path: Vec::new(), key_override: Vec::new(), frag: Vec::new(), cont: None }
    }

    fn one<'a>(it: &mut Interp<'a, Concrete>, block: &'a full_moon::ast::Block, st: State<Concrete>) -> (State<Concrete>, Flow<Concrete>) {
        let mut out = it.exec_block(block, st).expect("exec");
        assert_eq!(out.len(), 1, "one outcome");
        out.pop().unwrap()
    }

    fn read(st: &State<Concrete>) -> (Option<P8>, Vec<Option<P8>>) {
        let g = &st.heap.tables[&st.globals].hash;
        let num = |v: Option<&Value<Concrete>>| match v {
            Some(Value::Num(n)) => Some(*n),
            _ => None,
        };
        let Some(Value::Table(log)) = g.get("log") else { panic!("no log") };
        (num(g.get("result")), st.heap.tables[log].arr.iter().map(|v| num(Some(v))).collect())
    }

    /// A frame cut at the player's hint and resumed ends where it does in one
    /// piece: the stack comes back (the loop's next index, the callback's
    /// `if` arm, the helper's and the frame's statements) with its locals -
    /// the loop's `i`, the captured `seen` - and the rest runs once.
    #[test]
    fn a_frame_in_two_stages_ends_where_it_does_in_one() {
        let cart = full_moon::parse(CART).expect("parse");
        let call = full_moon::parse("frame()").expect("parse");
        let cuts: &'static [String] = Box::leak(vec!["player".to_string()].into_boxed_slice());

        let mut it = Interp::new(Concrete);
        let st = fresh(&mut it.d);
        let (st, _) = one(&mut it, cart.nodes(), st);
        let (whole, f) = one(&mut it, call.nodes(), st.clone());
        assert!(matches!(f, Flow::Normal));

        it.stage = Some(StageCtx { stage: 0, cuts, buttons: Vec::new() });
        let (mut mid, f) = one(&mut it, call.nodes(), st);
        assert!(matches!(f, Flow::Suspend), "stage 0 stops at the player's hint");
        let cont = mid.cont.take().expect("a continuation");
        assert_eq!(cont.cut, Some(1));
        // frame() -> each(..) -> for -> f(o) -> if arm -> the hint.
        let calls = cont.frames.iter().filter(|a| matches!(a, At::Call(_))).count();
        assert_eq!((calls, cont.frames.iter().filter(|a| matches!(a, At::Loop { .. })).count()), (3, 1));
        // Only the objects before the cut have run their callback's tail.
        assert_eq!(read(&mid), (None, vec![Some(P8::from_i16(11))]));

        it.stage = Some(StageCtx { stage: 1, cuts, buttons: Vec::new() });
        let mut out = it.resume(call.nodes(), &cont.frames, mid).expect("resume");
        assert_eq!(out.len(), 1);
        let (split, f) = out.pop().unwrap();
        assert!(matches!(f, Flow::Normal));
        assert!(split.cont.is_none());
        assert_eq!(read(&split), read(&whole));
        assert_eq!(read(&whole), (Some(P8::from_i16(303)), vec![Some(P8::from_i16(11)), Some(P8::from_i16(22)), Some(P8::from_i16(33))]));
    }
}
