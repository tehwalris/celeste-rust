//! LEVEL -1 (plans/level-minus-one.md): a POSITION-ONLY cost-to-go table for
//! the start room, computed from the traced frame graphs. A PROBE: nothing in
//! the search reads it.
//!
//! A node is (heap shape, cell of the located object - the player, or the
//! player spawn). Every other input of the frame is a RANGE (numbers) or
//! unknown (booleans), one per shape and the same for every cell of it: the
//! objects' `rem` its whole [-0.5, 0.5), the player's speed [-S, S], the rest
//! discovered below. One frame from a node is the shape's traced frame with
//! every `move` fork RESOLVED per configuration (`specialize_subset_into`: in
//! a configuration the move amount is one number, so the pixel steps and
//! their collisions are exact at an exact position) and evaluated over the
//! ranges with `Graph::eval_narrow_top_in`: a select whose condition the
//! ranges leave undecided JOINS its arms, the six buttons are unknown cells.
//! Every outcome whose `live` is not definitely false is a successor, at the
//! cells its position hull covers; an outcome in another room is the EXIT.
//! `d(node)` is the fewest frames from the node to an exit over that graph.
//!
//! The table over-approximates the game's transitions only under three
//! premises, and each is CHECKED, never assumed:
//!
//! * INDUCTIVE RANGES. A successor's values must lie in its shape's ranges
//!   and agree with its lattice pins. After a pass over the reachable graph
//!   every observed value is compared; a range that does not contain what
//!   flows into it widens (`widen`), a pin that does not hold is
//!   dropped (the shape re-traced without it), and the pass is redone. A pass
//!   that changes nothing is the answer.
//! * `ok` on every live outcome, read with `Known(..)` and `SplitOk(..)` as
//!   true. `Known(c)` is the kernels' premise that a select reads a DECIDED
//!   condition (`state::merge`, the boundary snaps): this evaluator joins an
//!   undecided select instead, which is sound. `SplitOk(n)` is that a lane's
//!   fork operand spans at most `n` floors: checked directly on every
//!   configuration's operands instead. What is left - pin guards, the static
//!   range premises, the symbolic loops' "finished" obligation, the rem
//!   containment - must evaluate to TRUE, or the node is reported.
//! * Every position hull has whole endpoints (a cell is a whole pixel).
//!
//! And three things it does NOT model, each counted in the report:
//!
//! * THE SPAWN is run as a chain from the start state (`probe`): as a
//!   position-only node it has no bound (`solids=false`, one `state` range per
//!   shape), so its ranges never converge.
//! * A DEATH (an outcome without a located object) is a dead end.
//! * A successor cell outside `WINDOW` is clipped.
//!
//! The recorded transitions of a level-0 tree are the check that none of the
//! three dropped a real move (`probe`'s soundness section).

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::path::{Path as FsPath, PathBuf};

use anyhow::{anyhow, bail, ensure, Result};

use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Path, Step};
use super::kernel::{LatticeWalk, Tracer};
use super::state::State;
use crate::transpile::graph::{Graph, NodeId, Op, Room, Val};

/// A report line, to the report and at once to stderr (a run is minutes).
macro_rules! say {
    ($rep:expr, $($a:tt)*) => {{
        let s = format!($($a)*);
        eprintln!("{s}");
        $rep.push_str(&s);
        $rep.push('\n');
        Ok::<(), anyhow::Error>(())
    }};
}

/// A raw 16.16 range, inclusive.
type Range = (i64, i64);

/// The rem range every object's `rem` is seeded with and must stay in.
const REM: Range = (-0x8000, 0x7fff);

/// More fork configurations than this in one shape is refused, not traced.
const MAX_CONFIGS: usize = 1 << 14;

/// A successor box larger than this (cells) is refused: an imprecision that
/// size is a finding to look at, not a table to build.
const MAX_BOX: i64 = 4096;

/// The window a successor cell is kept in: the room and 16 px around it. A
/// player in the room is at x in [-1, 121] (the draw clamp) and y in [-4,
/// 128] (the exit and the bottom death); what the ranges admit past that is
/// CLIPPED and counted, never silently: the player spawn has `solids=false`
/// and a `state` that is one range for every cell, so at level -1 it can keep
/// rising at any height (plans/level-minus-one.md).
const WINDOW: (i64, i64) = (-16, 143);

/// The "no located object" coordinate (a death's countdown).
const NONE: i16 = i16::MIN;

/// What the probe is asked.
pub struct Opts {
    /// The player's speed range per axis, whole pixels per frame.
    pub spd_px: i32,
    /// A level-0 checkpoint tree to check against and compare with.
    pub level_dir: PathBuf,
    pub ceiling: u32,
    pub from: u32,
    pub to: u32,
    /// The tree's backward marks at horizon `ceiling` (`hNNN/level00.marks.bin`):
    /// states on a level-0 winning path, none of which the table may call
    /// too late.
    pub marks: Option<PathBuf>,
    pub threads: usize,
}

/// What flowed into one input slot of a shape, joined over the edges.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Obs {
    Num(i64, i64),
    Bool { f: bool, t: bool, u: bool },
}

impl Obs {
    fn of(v: Val) -> Obs {
        match v {
            Val::Num(i) => Obs::Num(raw(i.low), raw(i.high)),
            Val::Bool(Some(false)) => Obs::Bool { f: true, t: false, u: false },
            Val::Bool(Some(true)) => Obs::Bool { f: false, t: true, u: false },
            Val::Bool(None) => Obs::Bool { f: false, t: false, u: true },
        }
    }
    fn join(self, o: Obs) -> Result<Obs> {
        Ok(match (self, o) {
            (Obs::Num(a, b), Obs::Num(c, d)) => Obs::Num(a.min(c), b.max(d)),
            (Obs::Bool { f, t, u }, Obs::Bool { f: g, t: s, u: v }) => Obs::Bool { f: f || g, t: t || s, u: u || v },
            _ => bail!("a slot observed as both a number and a boolean"),
        })
    }
}

fn raw(p: P8) -> i64 {
    p.as_raw_u32() as i32 as i64
}

fn px(r: i64) -> f64 {
    r as f64 / 65536.0
}

/// Widen `seed` to contain `obs`: the hull, or - for a slot that has already
/// grown twice (`jump`) - the moved end straight to the 16.16 extreme. The
/// hull first because anything wider can overshoot what a premise allows (a
/// fruit's `y` past its bob band leaves `widen_fruit`'s containment premise
/// undecided); the jump because a range the evaluator cannot bound (an
/// undecided `if freeze>0 then freeze-1`, whose arm is computed over the
/// whole range) would otherwise take one pass per step down.
fn widen(seed: Range, obs: Range, jump: bool) -> Range {
    let lo = if obs.0 < seed.0 { if jump { i32::MIN as i64 } else { obs.0 } } else { seed.0 };
    let hi = if obs.1 > seed.1 { if jump { i32::MAX as i64 } else { obs.1 } } else { seed.1 };
    (lo, hi)
}

/// The object a cell is read from: the player, or during the spawn the
/// player spawn - `pos_graph::player_object`'s rule.
fn located_object(st: &State<Symbolic>) -> Option<Path> {
    if let Some(p) = super::shapes::player_path(st) {
        return Some(p);
    }
    let Some(Value::Table(want)) = iface::get(st, &[iface::key("player_spawn")]) else { return None };
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return None };
    (0..st.heap.tables[&objects].arr.len()).map(|i| vec![iface::key("objects"), Step::Idx(i)]).find(|base| {
        let mut ty = base.clone();
        ty.push(iface::key("type"));
        iface::get(st, &ty) == Some(Value::Table(want))
    })
}

fn with(base: &Path, names: &[&str]) -> Path {
    let mut p = base.clone();
    for n in names {
        p.push(iface::key(n));
    }
    p
}

/// Is `p` an object's `rem.x/y`?
fn is_rem(p: &Path) -> bool {
    p.len() == 4 && p[0] == iface::key("objects") && p[2] == iface::key("rem")
}

/// Every object's `rem.x/y` and `spd.x/y` slots.
fn motion_paths(st: &State<Symbolic>) -> Vec<Path> {
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return Vec::new() };
    let mut out = Vec::new();
    for i in 0..st.heap.tables[&objects].arr.len() {
        let base = vec![iface::key("objects"), Step::Idx(i)];
        for sub in ["rem", "spd"] {
            for ax in ["x", "y"] {
                let p = with(&base, &[sub, ax]);
                if matches!(iface::get(st, &p), Some(Value::Num(_))) {
                    out.push(p);
                }
            }
        }
    }
    out
}

/// How an input slot is given a value.
#[derive(Clone)]
enum Seed {
    Pin(Conc),
    X,
    Y,
    /// A numeric range, by path into the shape's `ranges`.
    Range(Path),
    Unknown,
}

enum Target {
    Exit,
    Shape { id: usize, located: bool },
}

struct OutSpec {
    target: Target,
    /// The observation slot of each output field (`Table::obs_keys`).
    fields: Vec<usize>,
}

struct Roots {
    live: NodeId,
    ok: NodeId,
    xy: Option<(NodeId, NodeId)>,
    fields: Vec<NodeId>,
}

/// One configuration's roots: per outcome, and the fork operands with their
/// arities.
struct Config {
    outs: Vec<Roots>,
    operands: Vec<(u8, NodeId)>,
}

/// A shape traced for level -1.
struct Traced {
    seeds: Vec<Seed>,
    graph: Graph,
    outs: Vec<OutSpec>,
    configs: Vec<Config>,
    /// The bounds it was traced under (the static ranges the arity came from).
    bounds: BTreeMap<Path, Range>,
    pins: BTreeMap<Path, Conc>,
    stats: String,
}

struct Shape {
    key: String,
    ranges: BTreeMap<Path, Range>,
    /// Ranges not yet observed: taken from a blanked representative (0), so
    /// the first observation REPLACES them rather than joining the 0 in.
    unseeded: BTreeSet<Path>,
    demoted: BTreeSet<Path>,
    traced: Option<Traced>,
}

/// One node's frame, as its successors.
#[derive(Default)]
struct NodeOut {
    /// `(target shape, box)`; `usize::MAX` is the exit.
    succ: Vec<(usize, Option<(i16, i16, i16, i16)>)>,
    /// A successor box was clipped to `WINDOW`.
    clipped: bool,
}

/// A worker's accumulated observations and violations.
struct Acc {
    obs: Vec<Option<Obs>>,
    /// Successors clipped to `WINDOW`, per target shape.
    clipped: Vec<u64>,
    violations: Vec<String>,
    n_violations: usize,
}

struct Table<'a> {
    lw: &'a LatticeWalk,
    room: Room,
    room0: (i16, i16),
    spd_px: i32,
    shapes: Vec<Shape>,
    by_key: HashMap<String, usize>,
    obs_keys: Vec<(usize, Path)>,
    obs_index: BTreeMap<(usize, Path), usize>,
}

impl<'a> Table<'a> {
    fn shape_id(&mut self, key: &str) -> Result<usize> {
        if let Some(&i) = self.by_key.get(key) {
            return Ok(i);
        }
        ensure!(self.lw.reps.contains_key(key), "an outcome shape the lattice walk never reached: {key}");
        let i = self.shapes.len();
        self.shapes.push(Shape { key: key.to_string(), ranges: BTreeMap::new(), unseeded: BTreeSet::new(), demoted: BTreeSet::new(), traced: None });
        self.by_key.insert(key.to_string(), i);
        Ok(i)
    }

    fn obs_slot(&mut self, shape: usize, p: &Path) -> usize {
        if let Some(&i) = self.obs_index.get(&(shape, p.clone())) {
            return i;
        }
        let i = self.obs_keys.len();
        self.obs_keys.push((shape, p.clone()));
        self.obs_index.insert((shape, p.clone()), i);
        i
    }

    /// Trace shape `id` under its current ranges and pins, and specialize its
    /// frame on every fork configuration into one shared graph.
    fn trace(&mut self, tr: &mut Tracer, id: usize) -> Result<()> {
        use crate::interpreter::abstraction::{PosPrecision, SpdPrecision};
        let t0 = std::time::Instant::now();
        let key = self.shapes[id].key.clone();
        let st = self.lw.reps[&key].clone();
        let roots = super::shapes::state_paths(&st)?;
        let pins: BTreeMap<Path, Conc> = self.lw.lattice[&key]
            .iter()
            .filter(|(p, _)| roots.iter().any(|r| r == *p) && !self.shapes[id].demoted.contains(*p))
            .map(|(p, c)| (p.clone(), *c))
            .collect();
        let player = super::shapes::player_path(&st);
        let located = located_object(&st);
        let (xp, yp) = match &located {
            Some(o) => (Some(with(o, &["x"])), Some(with(o, &["y"]))),
            None => (None, None),
        };
        // Seed every numeric slot not pinned: a range kept from an earlier
        // pass, else the player's rem half and speed [-S, S], or the value the
        // representative holds (the start state's own, a blanked shape's 0).
        for p in &roots {
            if pins.contains_key(p) || self.shapes[id].ranges.contains_key(p) || Some(p) == xp.as_ref() || Some(p) == yp.as_ref() {
                continue;
            }
            let Some(Value::Num(n)) = iface::get(&st, p) else { continue };
            let is_player = |f: &str| player.as_ref().is_some_and(|pl| *p == with(pl, &[f, "x"]) || *p == with(pl, &[f, "y"]));
            let r = if is_player("rem") {
                REM
            } else if is_player("spd") {
                (-(self.spd_px as i64) << 16, (self.spd_px as i64) << 16)
            } else {
                let v = tr.it.d.as_const(&n).ok_or_else(|| anyhow!("{key}: {} is not a constant in the representative", iface::show(p)))?;
                if key != self.lw.start_key {
                    self.shapes[id].unseeded.insert(p.clone());
                }
                (raw(v), raw(v))
            };
            self.shapes[id].ranges.insert(p.clone(), r);
        }
        // Interval inputs: every object's unpinned rem and speed (so `move`
        // forks, and the fork's arity comes from their static ranges), and
        // the boundary's own (a fruit's band, the walk's discovered slots).
        let mut ival: Vec<Path> = Vec::new();
        let mut bounds: BTreeMap<Path, Range> = BTreeMap::new();
        for p in motion_paths(&st) {
            if pins.contains_key(&p) {
                continue;
            }
            bounds.insert(p.clone(), self.shapes[id].ranges[&p]);
            ival.push(p);
        }
        for p in super::shapes::ival_paths(&st, false, (false, false)).into_iter().chain(self.lw.ival_extra.get(&key).into_iter().flatten().cloned()) {
            if !pins.contains_key(&p) && !ival.contains(&p) {
                ival.push(p);
            }
        }
        let pin_list: Vec<(Path, Conc)> = pins.iter().map(|(p, c)| (p.clone(), *c)).collect();
        let bound_list: Vec<(Path, (i32, i32))> = bounds.iter().map(|(p, r)| (p.clone(), (r.0 as i32, r.1 as i32))).collect();
        // The held trails are unknown booleans read from their cells, not a
        // fork (the walk forks them for the kernels).
        tr.it.d.held_unknown = false;
        let f = super::verify::trace_frame(
            &mut tr.it,
            tr.reset,
            tr.fr,
            st.clone(),
            &roots,
            &pin_list,
            &ival,
            Some(super::widen::WidenMode::Level0(SpdPrecision::Exact, PosPrecision::EXACT)),
            &bound_list,
        )
        .map_err(|e| anyhow!("level -1 trace of shape {id}: {e:#}"))?;
        let t_trace = t0.elapsed();
        let n_slots = f.iface.slots.len();
        let seeds: Vec<Seed> = f
            .iface
            .slots
            .iter()
            .zip(&f.iface.init)
            .map(|(p, c)| {
                if Some(p) == xp.as_ref() {
                    Seed::X
                } else if Some(p) == yp.as_ref() {
                    Seed::Y
                } else if let Some(c) = pins.get(p) {
                    Seed::Pin(*c)
                } else {
                    match c {
                        Conc::Num(_) => Seed::Range(p.clone()),
                        Conc::Bool(_) => Seed::Unknown,
                    }
                }
            })
            .collect();
        ensure!(located.is_none() || seeds.iter().filter(|s| matches!(s, Seed::X | Seed::Y)).count() == 2, "shape {id}: the located object's x/y are not input slots");

        // The outcomes and every root read from them.
        let d = &tr.it.d;
        let mut outs: Vec<OutSpec> = Vec::new();
        let mut roots_old: Vec<Roots> = Vec::new();
        for o in &f.outs {
            if d.decide(&o.ok) == Some(false) {
                continue;
            }
            let room = super::shapes::room_of(&o.st, d);
            ensure!(room.0 >= 0 && room.1 >= 0, "shape {id}: an outcome whose room is not a constant");
            if room != self.room0 {
                outs.push(OutSpec { target: Target::Exit, fields: Vec::new() });
                roots_old.push(Roots { live: o.guard, ok: o.ok, xy: None, fields: Vec::new() });
                continue;
            }
            let tkey = format!("{:?}", o.st.shape()?);
            let tid = self.shape_id(&tkey)?;
            let tloc = located_object(&o.st);
            let node_of = |p: &Path| o.fields.iter().find(|(q, _, _)| q == p).map(|(_, n, _)| *n);
            let xy = match &tloc {
                Some(ob) => Some((
                    node_of(&with(ob, &["x"])).ok_or_else(|| anyhow!("shape {id}: an outcome's located object has no x"))?,
                    node_of(&with(ob, &["y"])).ok_or_else(|| anyhow!("shape {id}: an outcome's located object has no y"))?,
                )),
                None => None,
            };
            let mut fields = Vec::new();
            let mut fnodes = Vec::new();
            for (p, n, _) in &o.fields {
                fields.push(self.obs_slot(tid, p));
                fnodes.push(*n);
            }
            outs.push(OutSpec { target: Target::Shape { id: tid, located: tloc.is_some() }, fields });
            roots_old.push(Roots { live: o.guard, ok: o.ok, xy, fields: fnodes });
        }

        // The cone, copied out of the arena with the buttons as unknown cells
        // past the interface and the kernels' premises read as true (module
        // doc).
        let arena = &d.graph;
        let mut rs: Vec<NodeId> = Vec::new();
        for r in &roots_old {
            rs.push(r.live);
            rs.push(r.ok);
            if let Some((x, y)) = r.xy {
                rs.push(x);
                rs.push(y);
            }
            rs.extend(&r.fields);
        }
        let need = crate::transpile::bdd::reachable(arena, &rs);
        let mut cone = arena.like();
        cone.reset_forks();
        for k in 0..f.forks {
            ensure!(f.fork_tables.get(k as usize).is_none_or(|t| t.is_empty()), "shape {id}: table fork {k} (not at exact speed)");
            cone.set_fork_ways(k, f.fork_ways[k as usize]);
        }
        let mut cmap: Vec<NodeId> = vec![NodeId::MAX; arena.len()];
        let mut forks: BTreeMap<u8, NodeId> = BTreeMap::new();
        for i in 0..arena.len() {
            if !need[i] {
                continue;
            }
            let nd = arena.get(i as NodeId);
            cmap[i] = match nd.op {
                Op::Free(b) => cone.leaf(Op::Cell(n_slots as u32 + b as u32)),
                Op::Known | Op::SplitOk(_) => cone.leaf(Op::ConstBool(true)),
                Op::SplitTab(_) | Op::SplitValidTab(_) | Op::SplitKeyTab(_) | Op::SplitOkTab(_) => bail!("shape {id}: a table fork"),
                _ => {
                    let args: Vec<NodeId> = nd.args.iter().map(|a| cmap[*a as usize]).collect();
                    if let Op::Split(k) | Op::SplitInt(k) = nd.op {
                        forks.entry(k).or_insert(args[0]);
                    }
                    cone.fold(nd.op.clone(), args)
                }
            };
        }
        // `SplitValid` without its `Split` in the cone still names a fork.
        for i in 0..arena.len() {
            if need[i] {
                if let Op::SplitValid(k) = arena.get(i as NodeId).op {
                    forks.entry(k).or_insert(cmap[arena.get(i as NodeId).args[0] as usize]);
                }
            }
        }
        let arities: Vec<(u8, u8)> = forks.keys().map(|k| (*k, f.fork_ways[*k as usize])).collect();
        let n_configs: usize = arities.iter().map(|(_, w)| *w as usize).product();
        ensure!(n_configs <= MAX_CONFIGS, "shape {id}: {n_configs} fork configurations ({arities:?})");
        let mut shared = cone.like();
        let mut configs: Vec<Config> = Vec::new();
        let mut seen: HashSet<Vec<NodeId>> = HashSet::new();
        for c in 0..n_configs {
            let mut splits = vec![0u8; f.forks as usize];
            let mut rest = c;
            for (k, w) in &arities {
                splits[*k as usize] = (rest % *w as usize) as u8;
                rest /= *w as usize;
            }
            let map = cone.specialize_subset_into(0, Some(&splits), None, None, &mut shared);
            let m = |n: NodeId| map[cmap[n as usize] as usize];
            let outs_c: Vec<Roots> = roots_old
                .iter()
                .map(|r| Roots { live: m(r.live), ok: m(r.ok), xy: r.xy.map(|(x, y)| (m(x), m(y))), fields: r.fields.iter().map(|n| m(*n)).collect() })
                .collect();
            let operands: Vec<(u8, NodeId)> = forks.iter().map(|(k, op)| (f.fork_ways[*k as usize], map[*op as usize])).collect();
            let mut sig: Vec<NodeId> = Vec::new();
            for r in &outs_c {
                sig.extend([r.live, r.ok]);
                if let Some((x, y)) = r.xy {
                    sig.extend([x, y]);
                }
                sig.extend(&r.fields);
            }
            sig.extend(operands.iter().map(|o| o.1));
            if seen.insert(sig) {
                configs.push(Config { outs: outs_c, operands });
            }
        }
        let operands: Vec<String> = forks.iter().map(|(k, op)| format!("fork {k}: {}", super::emit::show_tree(&cone, *op, 3))).collect();
        let stats = format!(
            "{} slots ({} pinned, {} interval), {} outcomes, forks {:?}, {} configurations ({} distinct), cone {} nodes, specialized {} nodes, trace {:.1} s, specialize {:.1} s",
            n_slots,
            pins.len(),
            ival.len(),
            outs.len(),
            arities.iter().map(|(_, w)| *w).collect::<Vec<_>>(),
            n_configs,
            configs.len(),
            cone.len(),
            shared.len(),
            t_trace.as_secs_f64(),
            (t0.elapsed() - t_trace).as_secs_f64()
        ) + &operands.iter().map(|o| format!("\n      {o}")).collect::<String>();
        self.shapes[id].traced = Some(Traced { seeds, graph: shared, outs, configs, bounds, pins, stats });
        Ok(())
    }

}

/// One node's frame: evaluate every configuration over the shape's ranges at
/// cell `xy`. Free of the `Table`, whose tracer is not `Sync`.
fn eval_node(shapes: &[Shape], room: &Room, id: usize, xy: (i16, i16), acc: &mut Acc) -> Result<NodeOut> {
    let sh = &shapes[id];
    let t = sh.traced.as_ref().expect("a node's shape is traced before its layer");
    let exact = |v: i64| Val::Num(Pico8NumInterval::new(P8::from_raw(v as i32), P8::from_raw(v as i32)));
    let mut cells: HashMap<u32, Val> = HashMap::with_capacity(t.seeds.len() + 6);
    for (i, s) in t.seeds.iter().enumerate() {
        let v = match s {
            Seed::Pin(Conc::Num(n)) => Val::exact_num(*n),
            Seed::Pin(Conc::Bool(b)) => Val::Bool(Some(*b)),
            Seed::X => exact((xy.0 as i64) << 16),
            Seed::Y => exact((xy.1 as i64) << 16),
            Seed::Range(p) => {
                let r = sh.ranges[p];
                Val::Num(Pico8NumInterval::new(P8::from_raw(r.0 as i32), P8::from_raw(r.1 as i32)))
            }
            Seed::Unknown => Val::Bool(None),
        };
        cells.insert(i as u32, v);
    }
    for b in 0..6u32 {
        cells.insert(t.seeds.len() as u32 + b, Val::Bool(None));
    }
    let vals = t.graph.eval_narrow_top_in(&cells, room)?;
    let mut out = NodeOut::default();
    let violation = |acc: &mut Acc, msg: String| {
        acc.n_violations += 1;
        if acc.violations.len() < 12 {
            acc.violations.push(msg);
        }
    };
    // A position hull as whole pixels, or why not.
    let whole = |v: Val| -> std::result::Result<(i64, i64), String> {
        let Val::Num(i) = v else { return Err("not a number".into()) };
        let (lo, hi) = (raw(i.low), raw(i.high));
        if lo & 0xffff != 0 || hi & 0xffff != 0 {
            return Err(format!("[{}, {}] is not whole pixels", px(lo), px(hi)));
        }
        Ok((lo >> 16, hi >> 16))
    };
    for (ci, cfg) in t.configs.iter().enumerate() {
        for &(ways, n) in &cfg.operands {
            if let Val::Num(i) = vals[n as usize] {
                let floors = (raw(i.high) >> 16) - (raw(i.low) >> 16) + 1;
                if floors > ways as i64 {
                    violation(acc, format!("shape {id} cell {xy:?} configuration {ci}: a fork operand spans {floors} floors, arity {ways}"));
                }
            }
        }
        for (oi, (spec, r)) in t.outs.iter().zip(&cfg.outs).enumerate() {
            if vals[r.live as usize] == Val::Bool(Some(false)) {
                continue;
            }
            if vals[r.ok as usize] != Val::Bool(Some(true)) {
                let why = if acc.violations.len() < 12 { culprit(&t.graph, &vals, r.ok) } else { String::new() };
                violation(acc, format!("shape {id} cell {xy:?} configuration {ci} outcome {oi}: a live outcome's ok is {:?}: {why}", vals[r.ok as usize]));
            }
            for (slot, n) in spec.fields.iter().zip(&r.fields) {
                let o = Obs::of(vals[*n as usize]);
                acc.obs[*slot] = Some(match acc.obs[*slot] {
                    Some(p) => p.join(o)?,
                    None => o,
                });
            }
            match spec.target {
                Target::Exit => out.succ.push((usize::MAX, None)),
                Target::Shape { id: tid, located } => {
                    let bx = match (located, r.xy) {
                        (true, Some((xn, yn))) => {
                            let what = format!("shape {id} cell {xy:?} configuration {ci} outcome {oi} (to shape {tid})");
                            let ((xl, xh), (yl, yh)) = match (whole(vals[xn as usize]), whole(vals[yn as usize])) {
                                (Ok(x), Ok(y)) => (x, y),
                                (Err(e), _) | (_, Err(e)) => {
                                    violation(acc, format!("{what}: a successor position {e}"));
                                    continue;
                                }
                            };
                            let area = (xh - xl + 1) * (yh - yl + 1);
                            if area > MAX_BOX {
                                violation(acc, format!("{what}: a successor box [{xl}, {xh}] x [{yl}, {yh}]"));
                                continue;
                            }
                            let clip = |(l, h): (i64, i64)| (l.max(WINDOW.0), h.min(WINDOW.1));
                            let ((cxl, cxh), (cyl, cyh)) = (clip((xl, xh)), clip((yl, yh)));
                            if (cxl, cxh, cyl, cyh) != (xl, xh, yl, yh) {
                                acc.clipped[tid] += 1;
                                out.clipped = true;
                            }
                            if cxl > cxh || cyl > cyh {
                                continue;
                            }
                            Some((cxl as i16, cxh as i16, cyl as i16, cyh as i16))
                        }
                        _ => None,
                    };
                    out.succ.push((tid, bx));
                }
            }
        }
    }
    Ok(out)
}

/// Which conjunct keeps `n` from being true: down the `And`s and decided
/// selects to the first node that is not, with its operands' values.
fn culprit(g: &Graph, vals: &[Val], mut n: NodeId) -> String {
    loop {
        let nd = g.get(n);
        let not_true = |a: &NodeId| vals[*a as usize] != Val::Bool(Some(true));
        match nd.op {
            Op::And => {
                if let Some(a) = nd.args.iter().find(|a| not_true(a)) {
                    n = *a;
                    continue;
                }
            }
            Op::Sel => match vals[nd.args[0] as usize] {
                Val::Bool(Some(c)) => {
                    n = nd.args[if c { 1 } else { 2 }];
                    continue;
                }
                _ => {
                    if let Some(a) = nd.args[1..].iter().find(|a| not_true(a)) {
                        n = *a;
                        continue;
                    }
                }
            },
            _ => {}
        }
        return format!("{} = {:?}, operands {:?}", super::emit::show_tree(g, n, 4), vals[n as usize], nd.args.iter().map(|a| vals[*a as usize]).collect::<Vec<_>>());
    }
}

/// The table after the passes converged.
struct Graph1 {
    nodes: Vec<(usize, i16, i16)>,
    index: HashMap<(usize, i16, i16), u32>,
    edges: Vec<Vec<u32>>,
    exits: Vec<bool>,
    /// A successor without a located object (a death).
    deaths: Vec<bool>,
    /// A successor clipped to `WINDOW`.
    clipped: Vec<bool>,
}

/// What `build` hands on: per block shape hash its table shape, the converged
/// graph, the sound d per node (`sound_d`), and the spawn chain.
struct Built {
    by_hash: HashMap<u64, usize>,
    g1: Graph1,
    sound: Vec<u32>,
    chain: Vec<(usize, (i16, i16))>,
    start_d: u32,
}

/// A LOWER BOUND on the frames from each node to an exit - what a filter may
/// refuse a row on. A multi-source shortest path backward over the edges,
/// seeded where the graph stops modelling: an exit edge (1); a death successor
/// (1 + the start state's d, `chain_frames` + d of the chain's end: the room
/// restarts and replays the spawn chain, however long its countdown). The death
/// seed reads the end's d, which it cannot lower (a path through a death is
/// longer than the start's d), so the second run is the answer.
///
/// A successor part CLIPPED to `WINDOW` is not a seed: no real player is there
/// at a frame boundary (the draw clamp keeps x in [-1, 121], skipped on a freeze
/// frame by at most one move; below y = 128 the update kills; above y = -4 the
/// room changes), so a clipped part is the evaluator's imprecision. Seeding it
/// as a possible exit (tried first) put every node near a clamped edge within a
/// frame or two of an exit and cut nothing in room (2,0). The claim is checked
/// on every row the filter sees (`CostToGo::too_late`), never assumed.
fn sound_d(g: &Graph1, rev: &[Vec<u32>], chain_frames: u32, end_node: u32) -> Vec<u32> {
    use std::cmp::Reverse;
    let n = g.nodes.len();
    let run = |death: Option<u32>| -> Vec<u32> {
        let mut dist = vec![u32::MAX; n];
        let mut heap = std::collections::BinaryHeap::new();
        for i in 0..n {
            let mut s = if g.exits[i] { 1 } else { u32::MAX };
            if let (true, Some(w)) = (g.deaths[i], death) {
                s = s.min(w.saturating_add(1));
            }
            if s != u32::MAX {
                dist[i] = s;
                heap.push(Reverse((s, i as u32)));
            }
        }
        while let Some(Reverse((dn, i))) = heap.pop() {
            if dn != dist[i as usize] {
                continue;
            }
            for &p in &rev[i as usize] {
                if dn + 1 < dist[p as usize] {
                    dist[p as usize] = dn + 1;
                    heap.push(Reverse((dn + 1, p)));
                }
            }
        }
        dist
    };
    let mut death = None;
    loop {
        let dist = run(death);
        let start = dist[end_node as usize];
        let w = (start != u32::MAX).then(|| start.saturating_add(chain_frames));
        if w == death {
            return dist;
        }
        death = w;
    }
}

/// THE LEVEL -1 FILTER's table (`frame::level_minus_one`): per (block shape
/// hash, player cell), a lower bound on the frames to an exit (`sound_d`).
pub struct CostToGo {
    d: HashMap<(u64, i16, i16), u32>,
    /// The start state's d: no exit is sooner than this.
    pub start_d: u32,
}

impl CostToGo {
    /// Is a row of `shape` at `cell` at `frame` provably unable to exit by
    /// `horizon`? Only a table node is ever refused: a row without a player,
    /// one that has left the room, a shape or a cell the table never reached
    /// is kept. A row in the room OUTSIDE `WINDOW` breaks the premise the table
    /// dropped its clipped successors on (`sound_d`): it panics rather than
    /// filter on a table that does not cover it.
    pub fn too_late(&self, shape: u64, cell: u32, frame: u32, horizon: u32) -> bool {
        let Some((x, y)) = crate::search::pos_graph::cell_xy(cell) else { return false };
        if x >= 128 {
            return false;
        }
        let (x64, y64) = (x as i64, y as i64);
        assert!(
            x64 >= WINDOW.0 && y64 >= WINDOW.0 && y64 <= WINDOW.1,
            "level -1 filter: a row of shape {shape:#x} at ({x}, {y}), frame {frame}, lies outside the window {WINDOW:?} the table assumes a player never leaves"
        );
        match self.d.get(&(shape, x as i16, y as i16)) {
            None => false,
            Some(&d) => d == u32::MAX || frame.saturating_add(d) > horizon,
        }
    }
}

/// Build the level -1 table of the configured start room at player speed bound
/// `spd_px` (its report goes to stderr as it is built).
pub fn cost_to_go(root: &FsPath, spd_px: i32, threads: usize) -> Result<CostToGo> {
    let mut rep = String::new();
    let b = build(root, spd_px, threads, &mut rep)?;
    let mut d = HashMap::new();
    for (&h, &id) in &b.by_hash {
        for (i, &(sid, x, y)) in b.g1.nodes.iter().enumerate() {
            if sid == id {
                d.insert((h, x, y), b.sound[i]);
            }
        }
    }
    Ok(CostToGo { d, start_d: b.start_d })
}

/// The table: the lattice walk, the spawn chain, the passes to inductive ranges,
/// and d.
fn build(root: &FsPath, spd_px: i32, threads: usize, rep: &mut String) -> Result<Built> {
    let t_all = std::time::Instant::now();
    let room0 = celeste_interp::game_runner::start_room();
    let lw = super::kernel::room_constant_lattice(root, super::shapes::WalkOpts::LEVEL0.with_held(true))?;
    say!(rep, "room {room0:?}: lattice walk {} shapes, {:.1} s", lw.lattice.len(), t_all.elapsed().as_secs_f64())?;
    let mut tr = lw.tracer.clone();
    let mut table = Table {
        lw: &lw,
        room: Room { cart: lw.cart.clone(), cache: lw.cache.clone() },
        room0,
        spd_px,
        shapes: Vec::new(),
        by_key: HashMap::new(),
        obs_keys: Vec::new(),
        obs_index: BTreeMap::new(),
    };
    let start_id = table.shape_id(&lw.start_key)?;
    let start_xy = {
        let st = &lw.reps[&lw.start_key];
        let o = located_object(st).ok_or_else(|| anyhow!("the start state has no player or spawn"))?;
        let at = |f: &str| -> Result<i16> {
            let Some(Value::Num(n)) = iface::get(st, &with(&o, &[f])) else { bail!("start object {f} is not a number") };
            let v = lw.tracer.it.d.as_const(&n).ok_or_else(|| anyhow!("start object {f} is not a constant"))?;
            ensure!(raw(v) & 0xffff == 0, "start object {f} is not a whole pixel");
            Ok((raw(v) >> 16) as i16)
        };
        (at("x")?, at("y")?)
    };

    // THE SPAWN PREFIX. The player spawn reads no button and its `state`,
    // `delay` and speed are one range per shape at level -1, so as a
    // position-only node it can rise forever (it has `solids=false`): its
    // ranges never converge. So the prefix is run as a CHAIN instead: each
    // frame evaluated at the previous frame's own values (the same traced
    // frame, the same evaluator), which must give exactly one successor
    // cell, until the successor has a player. That player state seeds the
    // table.
    let t_chain = std::time::Instant::now();
    let mut chain: Vec<(usize, (i16, i16))> = vec![(start_id, start_xy)];
    let (end_id, end_xy) = loop {
        let (id, xy) = *chain.last().expect("the chain starts with the start state");
        ensure!(chain.len() <= 256, "the spawn prefix did not reach a player in 256 frames");
        table.shapes[id].traced = None;
        table.trace(&mut tr, id)?;
        let mut acc = Acc { obs: vec![None; table.obs_keys.len()], clipped: vec![0; table.shapes.len()], violations: Vec::new(), n_violations: 0 };
        let out = eval_node(&table.shapes, &table.room, id, xy, &mut acc)?;
        let f = chain.len() - 1;
        ensure!(acc.n_violations == 0 && acc.clipped.iter().all(|c| *c == 0), "the spawn prefix at frame {f} {xy:?}: {:?} (clipped {:?})", acc.violations, acc.clipped);
        let succ: BTreeSet<(usize, Option<(i16, i16, i16, i16)>)> = out.succ.into_iter().collect();
        ensure!(succ.len() == 1, "the spawn prefix at frame {f} {xy:?}: {} successors {succ:?}, not one", succ.len());
        let (tid, bx) = succ.into_iter().next().expect("one successor");
        let Some((xl, _, yl, _)) = bx.filter(|b| b.0 == b.1 && b.2 == b.3) else {
            bail!("the spawn prefix at frame {f} {xy:?}: the successor is not one cell ({tid}, {bx:?})")
        };
        // The successor's values are its seeds.
        let mut ranges: BTreeMap<Path, Range> = BTreeMap::new();
        for (slot, o) in acc.obs.iter().enumerate() {
            if let (Some(Obs::Num(lo, hi)), (sid, p)) = (o, &table.obs_keys[slot]) {
                if *sid == tid {
                    ranges.insert(p.clone(), (*lo, *hi));
                }
            }
        }
        chain.push((tid, (xl, yl)));
        if let Some(pl) = super::shapes::player_path(&lw.reps[&table.shapes[tid].key]) {
            // The player's speed range joins the successor's own speed.
            let sp = (spd_px as i64) << 16;
            for ax in ["x", "y"] {
                let q = with(&pl, &["spd", ax]);
                let r = ranges.get(&q).copied().unwrap_or((0, 0));
                ranges.insert(q, (r.0.min(-sp), r.1.max(sp)));
            }
            table.shapes[tid].ranges = ranges;
            table.shapes[tid].traced = None;
            break (tid, (xl, yl));
        }
        table.shapes[tid].ranges = ranges;
    };
    say!(rep, "spawn prefix: {} frames, {:?} -> shape {end_id} at {end_xy:?}, {:.1} s", chain.len() - 1, chain.iter().map(|c| c.1).collect::<Vec<_>>(), t_chain.elapsed().as_secs_f64())?;

    let mut pass = 0usize;
    // How often each range has grown (`widen`).
    let mut grown: BTreeMap<(usize, Path), u32> = BTreeMap::new();
    let g1 = loop {
        pass += 1;
        ensure!(pass <= 16, "the level -1 ranges did not converge in 16 passes");
        let t_pass = std::time::Instant::now();
        let mut t_trace = std::time::Duration::ZERO;
        let mut g = Graph1 { nodes: Vec::new(), index: HashMap::new(), edges: Vec::new(), exits: Vec::new(), deaths: Vec::new(), clipped: Vec::new() };
        let mut obs: Vec<Option<Obs>> = Vec::new();
        let mut violations: Vec<String> = Vec::new();
        let mut n_violations = 0usize;
        let mut clipped: Vec<u64> = Vec::new();
        let add = |g: &mut Graph1, k: (usize, i16, i16)| -> (u32, bool) {
            if let Some(&i) = g.index.get(&k) {
                return (i, false);
            }
            let i = g.nodes.len() as u32;
            g.nodes.push(k);
            g.index.insert(k, i);
            g.edges.push(Vec::new());
            g.exits.push(false);
            g.deaths.push(false);
            g.clipped.push(false);
            (i, true)
        };
        let mut frontier: Vec<u32> = vec![add(&mut g, (end_id, end_xy.0, end_xy.1)).0];
        let mut n_deaths = 0u64;
        let mut layers = 0usize;
        while !frontier.is_empty() {
            layers += 1;
            let tt = std::time::Instant::now();
            let ids: BTreeSet<usize> = frontier.iter().map(|n| g.nodes[*n as usize].0).collect();
            for id in ids {
                if table.shapes[id].traced.is_none() {
                    table.trace(&mut tr, id)?;
                    say!(rep, "  pass {pass}: traced shape {id}: {}", table.shapes[id].traced.as_ref().unwrap().stats)?;
                }
            }
            t_trace += tt.elapsed();
            let n_obs = table.obs_keys.len();
            let n_shapes = table.shapes.len();
            clipped.resize(n_shapes, 0);
            obs.resize(n_obs, None);
            let next = std::sync::atomic::AtomicUsize::new(0);
            let (sref, rref, fref, gref) = (&table.shapes, &table.room, &frontier, &g);
            let results: Vec<Result<(Vec<(u32, NodeOut)>, Acc)>> = std::thread::scope(|scope| {
                let hs: Vec<_> = (0..threads)
                    .map(|_| {
                        let next = &next;
                        scope.spawn(move || -> Result<(Vec<(u32, NodeOut)>, Acc)> {
                            let mut acc = Acc { obs: vec![None; n_obs], clipped: vec![0; n_shapes], violations: Vec::new(), n_violations: 0 };
                            let mut done = Vec::new();
                            loop {
                                let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some(&n) = fref.get(i) else { break };
                                let (id, x, y) = gref.nodes[n as usize];
                                done.push((n, eval_node(sref, rref, id, (x, y), &mut acc)?));
                            }
                            Ok((done, acc))
                        })
                    })
                    .collect();
                hs.into_iter().map(|h| h.join().expect("level -1 worker panicked")).collect()
            });
            let mut new_frontier = Vec::new();
            for r in results {
                let (done, acc) = r?;
                for (slot, o) in acc.obs.into_iter().enumerate() {
                    if let Some(o) = o {
                        obs[slot] = Some(match obs[slot] {
                            Some(p) => p.join(o)?,
                            None => o,
                        });
                    }
                }
                n_violations += acc.n_violations;
                for (c, k) in clipped.iter_mut().zip(&acc.clipped) {
                    *c += k;
                }
                violations.extend(acc.violations);
                for (n, out) in done {
                    let mut es: BTreeSet<u32> = BTreeSet::new();
                    if out.clipped {
                        g.clipped[n as usize] = true;
                    }
                    for (tid, bx) in out.succ {
                        if tid == usize::MAX {
                            g.exits[n as usize] = true;
                            continue;
                        }
                        // An outcome without a located object is a death:
                        // no edge (plans/level-minus-one.md); `sound_d` seeds
                        // it with the respawn.
                        let Some((xl, xh, yl, yh)) = bx else {
                            n_deaths += 1;
                            g.deaths[n as usize] = true;
                            continue;
                        };
                        for (x, y) in (xl..=xh).flat_map(|x| (yl..=yh).map(move |y| (x, y))) {
                            let (m, new) = add(&mut g, (tid, x, y));
                            es.insert(m);
                            if new {
                                new_frontier.push(m);
                            }
                        }
                    }
                    g.edges[n as usize] = es.into_iter().collect();
                }
            }
            frontier = new_frontier;
        }
        let n_edges: usize = g.edges.iter().map(|e| e.len()).sum();
        say!(
            rep,
            "pass {pass}: {} nodes, {} edges, {} exit nodes, {n_deaths} death successors, {layers} layers; {:.1} s ({:.1} s tracing); {n_violations} violations",
            g.nodes.len(),
            n_edges,
            g.exits.iter().filter(|e| **e).count(),
            t_pass.elapsed().as_secs_f64(),
            t_trace.as_secs_f64()
        )?;
        if clipped.iter().any(|c| *c > 0) {
            say!(rep, "  successors clipped to the window, per target shape: {:?}", clipped.iter().enumerate().filter(|(_, c)| **c > 0).collect::<Vec<_>>())?;
        }
        for v in violations.iter().take(12) {
            say!(rep, "  VIOLATION {v}")?;
        }

        // The inductive check.
        let mut changes: Vec<String> = Vec::new();
        for (slot, (id, p)) in table.obs_keys.clone().iter().enumerate() {
            let Some(o) = obs[slot] else { continue };
            let sh = &mut table.shapes[*id];
            let Some(t) = sh.traced.as_ref() else {
                // Observed flowing into a shape no node of reached: nothing to
                // check it against.
                continue;
            };
            if let Some(c) = t.pins.get(p) {
                let holds = match (c, o) {
                    (Conc::Num(v), Obs::Num(lo, hi)) => lo == raw(*v) && hi == raw(*v),
                    (Conc::Bool(b), Obs::Bool { f, t, u }) => !u && (if *b { !f } else { !t }),
                    _ => false,
                };
                if !holds {
                    changes.push(format!("shape {id}: pin {} = {c:?} does not hold ({o:?}): dropped", iface::show(p)));
                    if let (Conc::Num(v), Obs::Num(lo, hi)) = (c, o) {
                        sh.ranges.insert(p.clone(), widen((raw(*v), raw(*v)), (lo, hi), false));
                    }
                    sh.demoted.insert(p.clone());
                    sh.traced = None;
                }
                continue;
            }
            // Only the slots seeded from a range (not the enumerated cell).
            if !t.seeds.iter().any(|s| matches!(s, Seed::Range(q) if q == p)) {
                continue;
            }
            if let (Some(r), Obs::Num(lo, hi)) = (sh.ranges.get(p).copied(), o) {
                if sh.unseeded.remove(p) {
                    if (lo, hi) != r {
                        changes.push(format!("shape {id}: {} first observed [{}, {}]", iface::show(p), px(lo), px(hi)));
                        sh.ranges.insert(p.clone(), (lo, hi));
                        if t.bounds.contains_key(p) {
                            sh.traced = None;
                        }
                    }
                    continue;
                }
                if lo < r.0 || hi > r.1 {
                    // A rem stays in [-0.5, 0.5) by `move`'s own arithmetic:
                    // one outside is a finding, not a range to widen.
                    ensure!(!is_rem(p) || (lo >= REM.0 && hi <= REM.1), "shape {id}: {} = [{}, {}] leaves rem's [-0.5, 0.5)", iface::show(p), px(lo), px(hi));
                    let n = grown.entry((*id, p.clone())).or_insert(0);
                    *n += 1;
                    let w = widen(r, (lo, hi), *n > 2);
                    let w = if is_rem(p) { (w.0.max(REM.0), w.1.min(REM.1)) } else { w };
                    changes.push(format!("shape {id}: {} [{}, {}] observed [{}, {}]: widened to [{}, {}]", iface::show(p), px(r.0), px(r.1), px(lo), px(hi), px(w.0), px(w.1)));
                    sh.ranges.insert(p.clone(), w);
                    if t.bounds.contains_key(p) {
                        sh.traced = None;
                    }
                }
            }
        }
        for c in changes.iter().take(40) {
            say!(rep, "  {c}")?;
        }
        if changes.len() > 40 {
            say!(rep, "  ... {} changes in all", changes.len())?;
        }
        if changes.is_empty() {
            ensure!(n_violations == 0, "the converged table has {n_violations} violations (above): it is not sound");
            break g;
        }
    };

    // The final ranges.
    say!(rep, "\nconverged after {pass} passes; the ranges (px), per shape:")?;
    for (id, sh) in table.shapes.iter().enumerate() {
        let Some(t) = &sh.traced else { continue };
        let n = g1.nodes.iter().filter(|k| k.0 == id).count();
        say!(rep, "shape {id} ({:#x}): {n} nodes; {}", lw.frames.iter().find(|((k, _), _)| *k == sh.key).map(|(_, wf)| wf.frame.in_rt2.shape_hash_of()).unwrap_or(0), t.stats)?;
        let rs: Vec<String> = sh
            .ranges
            .iter()
            .filter(|(p, _)| t.seeds.iter().any(|s| matches!(s, Seed::Range(q) if q == *p)))
            .map(|(p, r)| if r.0 == r.1 { format!("{}={}", iface::show(p), px(r.0)) } else { format!("{}=[{},{}]", iface::show(p), px(r.0), px(r.1)) })
            .collect();
        say!(rep, "    {}", rs.join(" "))?;
        if !sh.demoted.is_empty() {
            say!(rep, "    pins dropped: {}", sh.demoted.iter().map(iface::show).collect::<Vec<_>>().join(" "))?;
        }
    }

    // d: fewest frames to an exit, backward from the nodes with an exit edge.
    let n = g1.nodes.len();
    let mut rev: Vec<Vec<u32>> = vec![Vec::new(); n];
    for (s, es) in g1.edges.iter().enumerate() {
        for &e in es {
            rev[e as usize].push(s as u32);
        }
    }
    let mut dist = vec![u32::MAX; n];
    let mut q: VecDeque<u32> = VecDeque::new();
    for i in 0..n {
        if g1.exits[i] {
            dist[i] = 1;
            q.push_back(i as u32);
        }
    }
    while let Some(i) = q.pop_front() {
        let dn = dist[i as usize] + 1;
        for &s in &rev[i as usize] {
            if dist[s as usize] == u32::MAX {
                dist[s as usize] = dn;
                q.push_back(s);
            }
        }
    }
    let end_node = g1.index[&(end_id, end_xy.0, end_xy.1)];
    say!(
        rep,
        "\nd: {} of {n} nodes reach an exit; the prefix's end d = {}, so the start state's d = {}; max finite d = {}",
        dist.iter().filter(|d| **d != u32::MAX).count(),
        dist[end_node as usize],
        dist[end_node as usize].saturating_add(chain.len() as u32 - 1),
        dist.iter().filter(|d| **d != u32::MAX).max().copied().unwrap_or(0)
    )?;
    let sound = sound_d(&g1, &rev, chain.len() as u32 - 1, end_node);
    let start_d = sound[end_node as usize].saturating_add(chain.len() as u32 - 1);
    say!(
        rep,
        "sound d (a death costs a respawn; clipped parts dropped, the window checked on every filtered row): {} of {n} nodes finite; the start state's d = {start_d}; {} clipped and {} death nodes",
        sound.iter().filter(|d| **d != u32::MAX).count(),
        g1.clipped.iter().filter(|c| **c).count(),
        g1.deaths.iter().filter(|c| **c).count()
    )?;
    let mut by_hash = HashMap::new();
    for (h, k) in &lw.by_hash {
        if let Some(&id) = table.by_key.get(k) {
            by_hash.insert(*h, id);
        }
    }
    Ok(Built { by_hash, g1, sound, chain, start_d })
}

pub fn probe(root: &FsPath, opts: &Opts) -> Result<String> {
    let mut rep = String::new();
    let t_all = std::time::Instant::now();
    let Built { by_hash, g1, sound, chain, .. } = build(root, opts.spd_px, opts.threads, &mut rep)?;

    // The soundness check against the recorded transitions.
    use crate::search::pos_graph::{cell_xy, PosGraph, CELL_COUNT, NO_CELL};
    let pg = PosGraph::load(&crate::frame::pos_graph_path(&opts.level_dir))?;
    let mut pos_edges: HashSet<(i16, i16, i16, i16)> = HashSet::new();
    let mut exit_src: HashSet<(i16, i16)> = HashSet::new();
    for w in chain.windows(2) {
        pos_edges.insert((w[0].1 .0, w[0].1 .1, w[1].1 .0, w[1].1 .1));
    }
    for (s, es) in g1.edges.iter().enumerate() {
        let (_, sx, sy) = g1.nodes[s];
        for &e in es {
            let (_, dx, dy) = g1.nodes[e as usize];
            pos_edges.insert((sx, sy, dx, dy));
        }
        if g1.exits[s] {
            exit_src.insert((sx, sy));
        }
    }
    let xy_of = |c: u32| -> (i16, i16) { if c == NO_CELL { (NONE, NONE) } else { cell_xy(c).map(|(x, y)| (x as i16, y as i16)).expect("a grid cell") } };
    let (mut checked, mut crossings, mut deaths, mut bad) = (0u64, 0u64, 0u64, Vec::new());
    for d in 0..CELL_COUNT as u32 {
        let (dx, dy) = xy_of(d);
        for &s in pg.srcs_of(d) {
            let (sx, sy) = xy_of(s);
            if sx != NONE && sx >= 128 {
                continue;
            }
            // A death or a respawn: not modelled (dead ends).
            if sx == NONE || dx == NONE {
                deaths += 1;
                continue;
            }
            if dx != NONE && dx >= 128 {
                crossings += 1;
                if !exit_src.contains(&(sx, sy)) {
                    bad.push(format!("({sx}, {sy}) -> exit"));
                }
                continue;
            }
            checked += 1;
            if !pos_edges.contains(&(sx, sy, dx, dy)) {
                bad.push(format!("({sx}, {sy}) -> ({dx}, {dy})"));
            }
        }
    }
    say!(rep, "\nsoundness vs {}: {} recorded pairs, {checked} in the room checked, {crossings} crossings checked, {deaths} deaths and respawns not modelled; {} VIOLATIONS", opts.level_dir.display(), pg.pairs(), bad.len())?;
    for b in bad.iter().take(20) {
        say!(rep, "  not allowed: {b}")?;
    }

    // The comparison with the tree.
    use crate::search::checkpoint::FrameFile;
    let marks: Option<rustc_hash::FxHashSet<(u64, u32, u64, u64)>> = match &opts.marks {
        Some(p) => {
            let v: Vec<(u64, u32, u64, u64)> = crate::search::checkpoint::load_value_from(p)?;
            say!(rep, "\n{} marked states in {}", v.len(), p.display())?;
            Some(v.into_iter().collect())
        }
        None => None,
    };
    say!(rep, "\nframe | states | too late (f + d > {}) | no exit path | band {},8 cut | death (kept) | left the room | other shape | not in the table | marked | MARKED TOO LATE", opts.ceiling, opts.ceiling)?;
    let mut samples: BTreeSet<String> = BTreeSet::new();
    for f in opts.from..=opts.to {
        let fdir = opts.level_dir.join("frames").join(format!("f{f:03}"));
        let (mut states, mut late, mut nopath, mut band, mut dead, mut left, mut other, mut missing) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
        let (mut marked, mut marked_late) = (0u64, 0u64);
        for e in std::fs::read_dir(&fdir)? {
            let p = e?.path();
            let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("").to_string();
            if !(name.starts_with('s') && name.ends_with(".bin")) {
                continue;
            }
            let ff = FrameFile::open(&p)?;
            let id = by_hash.get(&ff.shape_hash()).copied();
            let mut late_cells: rustc_hash::FxHashSet<u32> = Default::default();
            for (cell, rows) in ff.cell_counts() {
                let rows = rows as u64;
                states += rows;
                // What the time band (`frame::cell_too_late`, 8 px) drops.
                if crate::frame::cell_too_late(cell, f, opts.ceiling, 8) {
                    band += rows;
                }
                if cell == NO_CELL {
                    dead += rows;
                    continue;
                }
                let (x, y) = xy_of(cell);
                if x != NONE && x >= 128 {
                    left += rows;
                    continue;
                }
                let Some(id) = id else {
                    other += rows;
                    if samples.len() < 24 {
                        samples.insert(format!("f{f:03}: {rows} rows of shape {:#x} (not a table shape) at ({x}, {y})", ff.shape_hash()));
                    }
                    continue;
                };
                match g1.index.get(&(id, x, y)) {
                    None => {
                        missing += rows;
                        if samples.len() < 24 {
                            samples.insert(format!("f{f:03}: {rows} rows of shape {id} at ({x}, {y}): not a table node"));
                        }
                    }
                    Some(&i) => {
                        // The filter's d (`sound_d`), what it would drop.
                        let d = sound[i as usize];
                        if d == u32::MAX {
                            nopath += rows;
                            late += rows;
                            late_cells.insert(cell);
                        } else if f + d > opts.ceiling {
                            late += rows;
                            late_cells.insert(cell);
                        }
                    }
                }
            }
            // The level-0 backward's marks: a marked state is on a winning path
            // by the horizon, so the table calling it too late is a finding.
            if let Some(m) = &marks {
                let shape = ff.shape_hash();
                for (cell, key) in ff.cell_keys() {
                    if m.contains(&(shape, cell, key.0, key.1)) {
                        marked += 1;
                        if late_cells.contains(&cell) {
                            marked_late += 1;
                            if samples.len() < 24 {
                                let (x, y) = xy_of(cell);
                                samples.insert(format!("f{f:03}: a MARKED state of shape {shape:#x} at ({x}, {y}) is too late"));
                            }
                        }
                    }
                }
            }
        }
        let pc = |x: u64| 100.0 * x as f64 / states.max(1) as f64;
        say!(rep, "f{f:03} | {states} | {late} ({:.1}%) | {nopath} | {band} ({:.1}%) | {dead} | {left} | {other} | {missing} | {marked} | {marked_late}", pc(late), pc(band))?;
    }
    for x in &samples {
        say!(rep, "  {x}")?;
    }
    say!(rep, "\ntotal {:.1} s", t_all.elapsed().as_secs_f64())?;
    Ok(rep)
}
