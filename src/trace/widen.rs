//! The WIDENING TABLE (`celeste_engine::widening::TABLE`) in the traced
//! graph: at the frame's end every entry of the tracer's level becomes graph
//! ops writing what it stores, the containment it owes being the widening's
//! own per-lane error (`SlotErrors`); at the frame's start (`read_inputs`)
//! every entry's slots are read as its `Input` says. The block side
//! (`Rt2::widen_to`) interprets the same table; nothing here may widen what
//! the table does not name.

use anyhow::{anyhow, bail, Result};

use celeste_core::arith::EngineNum;
use celeste_engine::widening::{constant, range};
use celeste_engine::widening::{floor_player_window, Entry, Flag, Hook, Input, Level, Slot, Stored, Target, FLOOR_HITBOX, PLATFORM_PATH, PLAYER_HITBOX, REM};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;
use crate::transpile::garith::GraphArith;
use crate::transpile::graph::{NodeId, Op};

/// Write every `Stored::AbsentAsZero` field an object lacks as the number 0
/// (every frame's outcome, widened or not: it is heap shape).
pub fn materialize_absent_fields<D: Domain>(st: &mut State<D>, d: &mut D) -> Result<()> {
    for (ty, f) in celeste_engine::widening::absent_as_zero() {
        for obj in objects_of_type(st, ty) {
            let Some(Value::Table(t)) = iface::get(st, &obj) else { bail!("{}: not a table", iface::show(&obj)) };
            match st.heap.tables[&t].hash.get(f) {
                None | Some(Value::Nil) => {
                    let zero = Value::Num(d.int(0));
                    st.heap.tables.get_mut(&t).unwrap().hash.insert(f.to_string(), zero);
                }
                Some(Value::Num(_)) => {}
                Some(other) => bail!("{}.{f} holds {other:?}, not a number", iface::show(&obj)),
            }
        }
    }
    Ok(())
}

/// Objects whose `type` is the global `name` (not a list position: which
/// object is the player changes).
pub(crate) fn objects_of_type<D: Domain>(st: &State<D>, name: &str) -> Vec<Path> {
    let Some(Value::Table(want)) = iface::get(st, &[iface::key(name)]) else {
        return Vec::new();
    };
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else {
        return Vec::new();
    };
    let n = st.heap.tables[&objects].arr.len();
    let mut out = Vec::new();
    for i in 0..n {
        let base = vec![iface::key("objects"), Step::Idx(i)];
        let mut ty = base.clone();
        ty.push(iface::key("type"));
        if iface::get(st, &ty) == Some(Value::Table(want)) {
            out.push(base);
        }
    }
    out
}

pub(crate) fn field(base: &Path, names: &[&str]) -> Path {
    let mut p = base.clone();
    for n in names {
        p.push(iface::key(n));
    }
    p
}

/// Entry `e`'s slots in `st`: per instance, the path of each slot in the
/// entry's slot order.
pub fn entry_paths<D: Domain>(st: &State<D>, e: &Entry) -> Vec<Vec<Path>> {
    let bases = match e.target {
        Target::Objects(ty) => objects_of_type(st, ty),
        Target::Globals => vec![Vec::new()],
    };
    bases.iter().map(|b| e.slots.iter().map(|s| field(b, s.field)).collect()).collect()
}

/// The slots of `st` that `level`'s entries name and `pred` picks (present
/// ones; an optional slot may be absent), entry by entry.
pub fn slots_where<D: Domain>(st: &State<D>, level: Level, pred: impl Fn(&Slot) -> bool) -> Vec<Path> {
    let mut out = Vec::new();
    for e in level.entries() {
        for paths in entry_paths(st, e) {
            for (s, p) in e.slots.iter().zip(paths) {
                if pred(s) && iface::get(st, &p).is_some() {
                    out.push(p);
                }
            }
        }
    }
    out
}

/// Does a slot store something other than one value per state (so it is
/// never a compile-time constant of a shape, whatever a trace computes)?
pub fn stores_widened(s: &Slot) -> bool {
    !matches!(s.stored, Stored::Num(_) | Stored::Bool(_) | Stored::AtLeast(_) | Stored::FullPeriod(_) | Stored::AbsentAsZero)
}

/// Is a slot an INTERVAL input cell (a number slot holding an interval, a
/// boolean slot a lane may hold unknown), read as stored?
pub fn interval_input(s: &Slot) -> bool {
    matches!(s.input, Input::Stored | Input::Literal | Input::Hook)
        && matches!(s.stored, Stored::Range { .. } | Stored::Band { .. } | Stored::Phase { .. } | Stored::UnknownBool | Stored::SameAs(_))
}

/// Is a slot a COUNTDOWN a near level stores as the unknown number (its
/// unknown number a countdown's, `Op::UnknownNum`)?
fn countdown(e: &Entry, s: &Slot) -> bool {
    e.flag == Flag::Near && s.stored == Stored::UnknownNum
}

/// A table RANGE `[lo, hi]` (raw 16.16) as one graph literal, as the block
/// side stores it (`widening::range`: `REM` is half-open).
fn ival<A: GraphArith>(d: &mut Symbolic<A>, lo: i32, hi: i32) -> NodeId {
    let (lo, hi) = range::<A::Num>(lo, hi);
    d.graph.leaf(Op::Const(A::lit(lo), A::lit(hi)))
}

fn ival_of<A: GraphArith>(d: &mut Symbolic<A>, lo: i32, hi: i32) -> NodeId {
    let (lo, hi) = (A::lit(constant::<A::Num>(lo)), A::lit(constant::<A::Num>(hi)));
    d.graph.leaf(Op::Const(lo, hi))
}

/// What the output widenings OWE: per widened slot, the condition that the
/// replaced value was inside what it wrote; a lane where one fails declines.
pub type SlotErrors = Vec<(Path, NodeId)>;

fn owe<A: GraphArith>(errs: &mut SlotErrors, d: &mut Symbolic<A>, p: &Path, holds: NodeId) {
    let e = d.not(&holds);
    errs.push((p.clone(), e));
}

/// A state between the two steps of a split frame (`__phase` holds a table).
pub fn mid_frame<D: Domain>(st: &State<D>) -> bool {
    st.heap.tables[&st.globals].hash.contains_key("__phase")
}

/// Apply the tracer's level's entries (`TABLE`, in order) to `st` at the
/// frame's end: each slot written as its entry stores it, the containment
/// it owes returned per slot. `AbsentAsZero` is `materialize_absent_fields`'.
pub fn widen<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>) -> Result<SlotErrors> {
    let mut errs = SlotErrors::new();
    let mid = mid_frame(st);
    for e in d.level.entries() {
        match e.hook {
            Some(Hook::NearFloor) => widen_near_floors(st, d, e, &mut errs, mid)?,
            _ => widen_entry(st, d, e, &mut errs)?,
        }
    }
    Ok(errs)
}

fn widen_entry<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>, e: &Entry, errs: &mut SlotErrors) -> Result<()> {
    for paths in entry_paths(st, e) {
        // Per slot so far: its value before the widening, and what it owed.
        let mut seen: Vec<(Option<Value<Symbolic<A>>>, Option<NodeId>)> = Vec::new();
        for (s, p) in e.slots.iter().zip(&paths) {
            let old = iface::get(st, p);
            let Some(old_v) = old.clone() else {
                anyhow::ensure!(s.optional, "{}: widening {:?} has no such slot", iface::show(p), e.name);
                seen.push((None, None));
                continue;
            };
            let num = || match &old_v {
                Value::Num(n) => Ok(*n),
                _ => Err(anyhow::anyhow!("{}: not a number (widening {:?})", iface::show(p), e.name)),
            };
            let mut owed = None;
            let new = match s.stored {
                Stored::Range { lo, hi } => {
                    owed = contain(d, num()?, range::<A::Num>(lo, hi));
                    if let Some(h) = owed {
                        owe(errs, d, p, h);
                    }
                    Value::Num(ival(d, lo, hi))
                }
                // A constant centre (a shape's constant): a literal range.
                Stored::Band { around, radius } => {
                    let sp = field(&p[..p.len() - 1].to_vec(), &[around]);
                    let Some(Value::Num(centre)) = iface::get(st, &sp) else { bail!("{}: not a number", iface::show(&sp)) };
                    let Some(c) = d.as_const(&centre) else { bail!("{}: not a constant", iface::show(&sp)) };
                    // As the block side: `s - radius`, `s + radius` in the arithmetic.
                    let r = constant::<A::Num>(radius);
                    let (Some(lo), Some(hi)) = (A::sub(c, r), A::add(c, r)) else {
                        bail!("{}: the band {c:?} +- {radius:#x} leaves {}", iface::show(p), A::RANGE_NOTE)
                    };
                    owed = contain(d, num()?, (lo, hi));
                    if let Some(h) = owed {
                        owe(errs, d, p, h);
                    }
                    Value::Num(d.graph.leaf(Op::Const(A::lit(lo), A::lit(hi))))
                }
                Stored::Phase { lo, hi } => Value::Num(ival_of(d, lo, hi)),
                Stored::AtLeast(k) => {
                    let v = num()?;
                    let k = d.num(constant::<A::Num>(k));
                    Value::Num(d.fun2(super::domain::Fun2::Max, &v, &k)?)
                }
                Stored::UnknownNum => {
                    num()?;
                    Value::Num(d.unknown_num(countdown(e, s)))
                }
                Stored::UnknownBool => {
                    let Value::Bool(_) = old_v else { bail!("{}: not a boolean (widening {:?})", iface::show(p), e.name) };
                    Value::Bool(d.unknown_bool_output())
                }
                Stored::Num(k) => Value::Num(d.num(constant::<A::Num>(k))),
                Stored::Bool(b) => Value::Bool(d.boolean(b)),
                Stored::FullPeriod(period) => {
                    let v = num()?;
                    if !d.is_interval(&v) {
                        seen.push((old, None));
                        continue;
                    }
                    let (lo, hi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
                    let width = d.graph.fold(Op::Sub, vec![hi, lo]);
                    let k = A::lit(constant::<A::Num>(period));
                    let full_period = d.graph.leaf(Op::Const(k, k));
                    let full = d.graph.fold(Op::Ge, vec![width, full_period]);
                    owe(errs, d, p, full);
                    Value::Num(ival_of(d, 0, period))
                }
                // Its twin's stored value; owed: its twin's obligation, the
                // two being one node at every frame's end.
                Stored::SameAs(other) => {
                    let j = e.slots.iter().position(|t| t.field == [other]).expect("SameAs names a slot of its entry");
                    let (twin_old, twin_owed) = seen[j].clone();
                    anyhow::ensure!(twin_old == Some(old_v.clone()), "{}: not its `{other}` at the frame's end (widening {:?})", iface::show(p), e.name);
                    owed = twin_owed;
                    if let Some(h) = owed {
                        owe(errs, d, p, h);
                    }
                    iface::get(st, &paths[j]).expect("the twin, widened")
                }
                Stored::AbsentAsZero => {
                    seen.push((old, None));
                    continue;
                }
            };
            iface::set(st, p, new)?;
            seen.push((old, owed));
        }
    }
    Ok(())
}

fn bounds_inside<A: GraphArith>(d: &mut Symbolic<A>, v: NodeId, lo: A::Num, hi: A::Num) -> NodeId {
    let (lo, hi) = (A::lit(lo), A::lit(hi));
    let (klo, khi) = (d.graph.leaf(Op::Const(lo, lo)), d.graph.leaf(Op::Const(hi, hi)));
    let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
    let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
    let below = d.graph.fold(Op::Le, vec![vhi, khi]);
    d.graph.fold(Op::And, vec![above, below])
}

/// `v` in `[lo, hi]`: `None` if proved statically (`within`), else the
/// per-lane condition.
fn contain<A: GraphArith>(d: &mut Symbolic<A>, v: NodeId, (lo, hi): (A::Num, A::Num)) -> Option<NodeId> {
    if within(d, v, (lo.to_f64(), hi.to_f64()), &mut Vec::new()) {
        return None;
    }
    Some(bounds_inside(d, v, lo, hi))
}

/// The INPUT side: every entry of the tracer's level read as its slots'
/// `Input` says, in table order, before the frame reads anything. Returned:
/// what the platforms' hook owes per lane beyond its pins (the no-player
/// speed), which the caller makes an error of the whole frame.
pub fn read_inputs<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>) -> Result<Vec<NodeId>> {
    let mut obligations = Vec::new();
    let mid = mid_frame(st);
    // A near level reads an absent countdown as the unknown number too: the
    // absent field is 0 (`Stored::AbsentAsZero`), which the unknown holds.
    // Mid split frame the countdowns stay as the first step left them.
    if d.level.floors_near && !mid {
        materialize_absent_fields(st, d)?;
    }
    d.platform_cells.clear();
    d.integral_inputs.clear();
    let mut platforms_read = false;
    for e in d.level.entries() {
        match e.hook {
            Some(Hook::NearFloor) => read_near_floors(st, d, e, mid)?,
            Some(Hook::PlatformInputs) => {
                if !std::mem::replace(&mut platforms_read, true) {
                    obligations.extend(platform_inputs(st, d)?);
                }
            }
            None => {
                for paths in entry_paths(st, e) {
                    for (s, p) in e.slots.iter().zip(&paths) {
                        read_slot(st, d, e, s, p)?;
                    }
                }
            }
        }
    }
    Ok(obligations)
}

/// An input `v` READ AS the literal `[lo, hi]` (`Input::Literal`, a
/// platform's `rem.x`): pinned to it (`Op::Pin`, the input disconnected, its
/// check `v` inside the literal), unless the graph proves it inside.
fn pin_literal<A: GraphArith>(d: &mut Symbolic<A>, v: NodeId, lo: i32, hi: i32) {
    let (l, h) = range::<A::Num>(lo, hi);
    if !within(d, v, (l.to_f64(), h.to_f64()), &mut Vec::new()) {
        d.pin(v, crate::transpile::graph::Pinned::Num(A::lit(l), A::lit(h)));
    }
}

fn read_slot<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>, e: &Entry, s: &Slot, p: &Path) -> Result<()> {
    let what = |kind: &str| anyhow::anyhow!("{}: not a {kind} (widening {:?})", iface::show(p), e.name);
    let new = match (s.input, iface::get(st, p)) {
        (Input::Stored, _) => return Ok(()),
        (Input::Hook, _) => bail!("{}: widening {:?} reads it in a hook it does not have", iface::show(p), e.name),
        (_, None) if s.optional => return Ok(()),
        (Input::BothWays, Some(Value::Bool(_))) => Value::Bool(d.both_values(s.field.last().expect("a field"))),
        (Input::Atom, Some(Value::Bool(_))) => Value::Bool(d.unknown_bool_atom()),
        (Input::BothWays | Input::Atom, _) => return Err(what("boolean")),
        (Input::Unknown, Some(Value::Num(_))) => Value::Num(d.unknown_num(countdown(e, s))),
        (Input::Literal, Some(Value::Num(v))) => {
            let Stored::Range { lo, hi, .. } = s.stored else { bail!("{}: a literal input reads a range", iface::show(p)) };
            pin_literal(d, v, lo, hi);
            let r = ival(d, lo, hi);
            return iface::set(st, p, Value::Num(r));
        }
        (Input::Unknown | Input::Literal, _) => return Err(what("number")),
    };
    iface::set(st, p, new)
}

/// An object's `(x, y)`: constants, floors and springs never move.
fn position<A: GraphArith>(st: &State<Symbolic<A>>, d: &Symbolic<A>, obj: &Path) -> Result<(A::Num, A::Num)> {
    let get = |f: &str| -> Result<A::Num> {
        let p = field(obj, &[f]);
        match iface::get(st, &p) {
            Some(Value::Num(v)) => d.as_const(&v).ok_or_else(|| anyhow::anyhow!("{}: not a constant", iface::show(&p))),
            _ => bail!("{}: not a number", iface::show(&p)),
        }
    };
    Ok((get("x")?, get("y")?))
}

/// The slots a hook stores as an interval on some lanes and exact on others
/// (a near floor's `state`): an interval column in every outcome.
pub fn near_floor_states<A: GraphArith>(st: &State<Symbolic<A>>, level: Level) -> Vec<Path> {
    level.entries().filter(|e| e.hook == Some(Hook::NearFloor)).flat_map(|e| entry_paths(st, e)).map(|paths| paths[0].clone()).collect()
}

/// `Hook::NearFloor`, INPUT: a floor's `state` as stored (split by
/// `split::split_undecided_selects`), `collideable` DERIVED as `state ~= 2`
/// (the cart keeps them in step; an independent unknown `collideable` would
/// admit players inside solid floors). Mid split frame a lane may hold an
/// unknown `collideable`, but a boolean slot binds as a decided cell that
/// would read it as false: the cell where the lane knows it, else a fork of
/// both values.
fn read_near_floors<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>, e: &Entry, mid: bool) -> Result<()> {
    use super::domain::Cmp;
    let two = if mid { None } else { Some(d.int(2)) };
    for paths in entry_paths(st, e) {
        let [ps, pc] = &paths[..] else { bail!("the near floors' entry has two slots") };
        match two {
            None => {
                let Some(Value::Bool(c)) = iface::get(st, pc) else { bail!("{}: not a boolean", iface::show(pc)) };
                let known = d.graph.fold(Op::Known, vec![c]);
                let both = d.both_values(&format!("{} unknown", iface::show(pc)));
                let v = d.sel_bool(&known, &c, &both);
                iface::set(st, pc, Value::Bool(v))?;
            }
            Some(two) => {
                let Some(Value::Num(state)) = iface::get(st, ps) else { bail!("{}: not a number", iface::show(ps)) };
                let hidden = d.compare(Cmp::Eq, &state, &two)?;
                let solid = d.not(&hidden);
                iface::set(st, pc, Value::Bool(solid))?;
            }
        }
    }
    Ok(())
}

/// How far `player.update` probes a fall floor (`is_solid(ox, oy)`, `ox` in
/// -3..=3, `oy` in 0..=1), as offsets to the overlap window's ends.
pub const PLAYER_PROBE: [(i16, i16); 2] = [(-3, 3), (-1, 0)];

/// `Hook::NearFloor`, OUTPUT: every fall floor stores its slots (`state` the
/// range, `collideable` unknown), EXCEPT where the player overlaps it;
/// countdowns are widened regardless (unread while inside). An overlapped
/// floor stores the CART'S INVARIANT (hidden: `state` 2, `collideable`
/// false, the player cannot be inside a solid floor), with the computed
/// value owed (storing the computed value makes the split resolve every
/// floor the player MIGHT overlap: 30x the outcomes); with the platforms
/// unknown, the computed value (below). Mid split frame (`mid`),
/// `collideable` also stays computed in the `PLAYER_PROBE` window: widening
/// there would let the second step reach states the unsplit frame does not.
/// `Rt2::widen_to` agrees.
fn widen_near_floors<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>, e: &Entry, errs: &mut SlotErrors, mid: bool) -> Result<()> {
    use super::domain::Cmp;
    // The hitboxes `floor_player_window` assumes, checked on the state.
    let hitbox = |st: &State<Symbolic<A>>, d: &Symbolic<A>, obj: &Path, want: [i16; 4]| -> Result<()> {
        for (f, w) in ["x", "y", "w", "h"].iter().zip(want) {
            let p = field(obj, &["hitbox", f]);
            let got = match iface::get(st, &p) {
                Some(Value::Num(v)) => d.as_const(&v),
                _ => None,
            };
            anyhow::ensure!(got == Some(A::from_i16(w)), "{}: {got:?}, the near widening assumes {w}", iface::show(&p));
        }
        Ok(())
    };
    let mut players = Vec::new();
    for obj in objects_of_type(st, "player") {
        hitbox(st, d, &obj, PLAYER_HITBOX)?;
        let coord = |f: &str| -> Result<NodeId> {
            let p = field(&obj, &[f]);
            match iface::get(st, &p) {
                Some(Value::Num(v)) => Ok(v),
                _ => bail!("{}: not a number", iface::show(&p)),
            }
        };
        players.push((coord("x")?, coord("y")?));
    }
    let Stored::Range { lo: slo, hi: shi, .. } = e.slots[0].stored else { bail!("the near floors' `state` stores a range") };
    let Target::Objects(ty) = e.target else { bail!("the near floors are objects") };
    for (obj, paths) in objects_of_type(st, ty).into_iter().zip(entry_paths(st, e)) {
        let [ps, pc] = &paths[..] else { bail!("the near floors' entry has two slots") };
        hitbox(st, d, &obj, FLOOR_HITBOX)?;
        let at = position(st, d, &obj)?;
        let [(xlo, xhi), (ylo, yhi)] = floor_player_window(at).ok_or_else(|| anyhow!("{}: a fall floor at {at:?}: its window leaves the arithmetic's range", iface::show(&obj)))?;
        // `lo < v < hi` for every value a lane may hold (straddling is no
        // overlap, the safe side), as `widening::player_overlaps_floor`.
        let inside = |d: &mut Symbolic<A>, v: NodeId, lo: A::Num, hi: A::Num| -> Result<NodeId> {
            let (a, b) = if d.is_interval(&v) { (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v])) } else { (v, v) };
            let (klo, khi) = (d.num(lo), d.num(hi));
            let above = d.compare(Cmp::Gt, &a, &klo)?;
            let below = d.compare(Cmp::Lt, &b, &khi)?;
            Ok(d.and(&above, &below))
        };
        let mut overlap = d.boolean(false);
        // Where the player's update may read the floor (mid-frame only).
        let mut probe = d.boolean(false);
        for &(px, py) in &players {
            let x = inside(d, px, xlo, xhi)?;
            let y = inside(d, py, ylo, yhi)?;
            let both = d.and(&x, &y);
            overlap = d.or(&overlap, &both);
            if mid {
                let [(dxlo, dxhi), (dylo, dyhi)] = PLAYER_PROBE;
                let shift = |v: A::Num, by: i16| A::add(v, A::from_i16(by)).ok_or_else(|| anyhow!("{}: the probe window {v:?} + {by} leaves the arithmetic's range", iface::show(&obj)));
                let x = inside(d, px, shift(xlo, dxlo)?, shift(xhi, dxhi)?)?;
                let y = inside(d, py, shift(ylo, dylo)?, shift(yhi, dyhi)?)?;
                let both = d.and(&x, &y);
                probe = d.or(&probe, &both);
            }
        }
        let Some(Value::Num(state)) = iface::get(st, ps) else { bail!("{}: not a number", iface::show(ps)) };
        if let Some(holds) = contain(d, state, range::<A::Num>(slo, shi)) {
            let held = d.or(&overlap, &holds);
            owe(errs, d, ps, held);
        }
        // Overlapped: hidden, owed as `collideable` false, which carries
        // `state` 2 too. Do not owe `state == 2` separately: a shaking floor's
        // `sel(delay - 1 <= 0, 2, 1)` is undecided and would decline the lane.
        let apart = d.not(&overlap);
        let two = d.int(2);
        let range = ival(d, slo, shi);
        // With the platforms unknown, the overlapped floor stores what the frame
        // COMPUTED instead (exact, nothing owed): there the hidden invariant failed
        // on every lane of the spawn's first player frame (room (2,1);
        // plans/architecture.md "Open issues"). `Rt2::widen_to` agrees (it keeps
        // an overlapped floor's values as they are).
        let computed = d.level.platforms;
        let state = d.sel_num(&overlap, if computed { &state } else { &two }, &range);
        iface::set(st, ps, Value::Num(state))?;
        let Some(Value::Bool(coll)) = iface::get(st, pc) else { bail!("{}: not a boolean", iface::show(pc)) };
        // Owed at the frame's END only: mid-frame the shaking floor's
        // `delay - 1 <= 0` is not split yet and would decline valid lanes.
        if !mid && !computed {
            let passable = d.not(&coll);
            let held = d.or(&apart, &passable);
            owe(errs, d, pc, held);
        }
        let (unknown, absent) = (d.unknown_bool_output(), d.boolean(false));
        // Mid-frame, in the probe window: the computed `collideable`, kept.
        let unread = if mid { d.sel_bool(&probe, &coll, &unknown) } else { unknown };
        let coll = d.sel_bool(&overlap, if computed { &coll } else { &absent }, &unread);
        iface::set(st, pc, Value::Bool(coll))?;
    }
    Ok(())
}

/// `Hook::PlatformInputs`: the platforms' INPUT side at a platforms-unknown
/// level. Which world platform each is, by constant `y` and `dir`; `x` an
/// input cell restricted to the path (`Op::Restrict`) and recorded as the
/// world's cell, so the split decides player-vs-platform comparisons per
/// PLATFORM WORLD, keeping platforms mutually consistent; `last` read as `x`
/// (checked at the output, `Stored::SameAs`); `rem.x` the literal whole
/// remainder (`Input::Literal`: a literal's floor rejoins as fragments, at a
/// pixel of slack); each unpinned `spd.x` through its range over the worlds,
/// with no player as the literal of its one moving speed. Not table slots:
/// all of it is per WORLD, which a slot cannot say. Returned: the per-lane
/// obligation of each no-player speed (`0` or its one moving speed: a
/// two-point set no single `Pin` states).
pub fn platform_inputs<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>) -> Result<Vec<NodeId>> {
    let table = d.worlds.clone().ok_or_else(|| anyhow::anyhow!("the platforms are unknown but there is no world table"))?;
    let worlds = &table.0;
    let platforms = objects_of_type(st, "platform");
    let first = worlds.first().ok_or_else(|| anyhow::anyhow!("no platform world"))?;
    anyhow::ensure!(first.len() == platforms.len(), "a platform world has {} platforms, the state {}", first.len(), platforms.len());
    fn konst<A: GraphArith>(st: &State<Symbolic<A>>, d: &Symbolic<A>, p: &Path) -> Result<A::Num> {
        match iface::get(st, p) {
            Some(Value::Num(n)) => match d.graph.get(n).op {
                Op::Const(a, b) if a == b => Ok(A::from_lit(a)),
                ref o => bail!("{}: a platform's {} is not a known constant ({o:?})", iface::show(p), iface::show(p)),
            },
            _ => bail!("{}: not a number", iface::show(p)),
        }
    }
    let mut taken = vec![false; platforms.len()];
    let mut cells: Vec<Option<NodeId>> = vec![None; platforms.len()];
    let mut obligations = Vec::new();
    for obj in &platforms {
        let (y, dir) = (konst(st, d, &field(obj, &["y"]))?, konst(st, d, &field(obj, &["dir"]))?);
        let j = (0..first.len())
            .find(|&j| !taken[j] && first[j][4] == y && first[j][5] == dir)
            .ok_or_else(|| anyhow::anyhow!("no platform world entry at y {} dir {} left for {}", A::show(y), A::show(dir), iface::show(obj)))?;
        taken[j] = true;
        let spd = field(obj, &["spd", "x"]);
        // Its distinct moving speeds (by bits; a double's `-0` would not
        // be moving either).
        let zero = A::from_i16(0);
        let moving: Vec<A::Num> = {
            let mut v: Vec<A::Num> = Vec::new();
            for s in worlds.iter().map(|w| w[j][3]).filter(|s| !A::num_eq(*s, zero)) {
                if !v.contains(&s) {
                    v.push(s);
                }
            }
            v
        };
        if let (Some(Value::Num(sv)), None, [v]) = (iface::get(st, &spd), super::shapes::player_path(st), moving.as_slice()) {
            // NO PLAYER: a platform's move is unobservable, so read the speed
            // as the literal (fragments, not a fork per platform); asserted
            // per lane that the speed is 0 or that one.
            if matches!(d.graph.get(sv).op, Op::Cell(_)) {
                let (z, k) = (A::lit(zero), A::lit(*v));
                let (k0, kv) = (d.graph.leaf(Op::Const(z, z)), d.graph.leaf(Op::Const(k, k)));
                let (a, b) = (d.graph.fold(Op::Eq, vec![sv, k0]), d.graph.fold(Op::Eq, vec![sv, kv]));
                obligations.push(d.graph.fold(Op::Or, vec![a, b]));
                iface::set(st, &spd, Value::Num(kv))?;
            }
        } else if let Some(Value::Num(sv)) = iface::get(st, &spd) {
            if matches!(d.graph.get(sv).op, Op::Cell(_)) {
                // The hull in the total order (`-0` below `0`), as an
                // interval's ends are.
                let speeds = || worlds.iter().map(|w| w[j][3]);
                let order = |a: &A::Num, b: &A::Num| a.to_f64().total_cmp(&b.to_f64());
                let lo = speeds().min_by(order).expect("a world");
                let hi = speeds().max_by(order).expect("a world");
                let r = d.restrict(sv, A::lit(lo), A::lit(hi));
                iface::set(st, &spd, Value::Num(r))?;
            }
        }
        let (x, last, rem) = (field(obj, &["x"]), field(obj, &["last"]), field(obj, &["rem", "x"]));
        let Some(Value::Num(xv)) = iface::get(st, &x) else { bail!("{}: not a number", iface::show(&x)) };
        anyhow::ensure!(matches!(d.graph.get(xv).op, Op::Cell(_)), "a platform's `x` must be an input cell");
        let (plo, phi) = (A::from_i16(PLATFORM_PATH.0), A::from_i16(PLATFORM_PATH.1));
        let xr = d.restrict(xv, A::lit(plo), A::lit(phi));
        // An integer in every world (checked where the worlds are made).
        d.integral_inputs.insert(xr);
        iface::set(st, &x, Value::Num(xr))?;
        iface::set(st, &last, Value::Num(xr))?;
        // `rem.x` is replaced by the literal whole remainder: sound for a
        // lane whose own lies inside, which is owed.
        let Some(Value::Num(rv)) = iface::get(st, &rem) else { bail!("{}: not a number", iface::show(&rem)) };
        pin_literal(d, rv, REM.0, REM.1);
        let r = ival(d, REM.0, REM.1);
        iface::set(st, &rem, Value::Num(r))?;
        cells[j] = Some(xv);
    }
    d.platform_cells = cells.into_iter().map(|c| c.expect("every world platform matched")).collect();
    Ok(obligations)
}

/// A static range of a value, as exact doubles (16.16's values and a
/// double's both are; infinite ends unbounded). `f64` arithmetic on it is
/// EXACT for 16.16 and the doubles' own rounding for `Float` (`fl` is
/// monotone, so `fl(a + b)` over ranges lies between the ends' sums).
type RangeF64 = (f64, f64);

/// Does `v` provably lie in `[lo, hi]`? Through select arms and under
/// point-split branches with what they say about the compared value (so a
/// wrap `x < -16 ? 128 : ...` is bounded). Static; `false` if unsure.
fn within<A: GraphArith>(d: &Symbolic<A>, v: NodeId, (lo, hi): RangeF64, facts: &mut Vec<(NodeId, f64, f64)>) -> bool {
    // Its own bounds first: the enclosing branch may bound a select whole.
    if let Some((a, b)) = bounds(d, v, facts) {
        if lo <= a && b <= hi {
            return true;
        }
    }
    let node = d.graph.get(v);
    if let Op::Sel = node.op {
        let (c, t, f) = (node.args[0], node.args[1], node.args[2]);
        // A select on `x op k`: each arm with what its answer says about `x`.
        if let Some((x, yes, no)) = comparison_facts(d, c) {
            facts.push((x, yes.0, yes.1));
            let a = within(d, t, (lo, hi), facts);
            facts.pop();
            facts.push((x, no.0, no.1));
            let b = within(d, f, (lo, hi), facts);
            facts.pop();
            return a && b;
        }
        return within(d, t, (lo, hi), facts) && within(d, f, (lo, hi), facts);
    }
    match bounds(d, v, facts) {
        Some((a, b)) => lo <= a && b <= hi,
        None => false,
    }
}

/// `c` as `x op k` (`k` a literal point, either side): `x` and its range
/// `(when true, when false)`; `k`'s neighbours are the arithmetic's
/// (`Arithmetic::below`, and above as `-below(-k)`).
fn comparison_facts<A: GraphArith>(d: &Symbolic<A>, c: NodeId) -> Option<(NodeId, RangeF64, RangeF64)> {
    let node = d.graph.get(c);
    let point = |n: NodeId| match d.graph.get(n).op {
        Op::Const(a, b) if a == b => Some(A::from_lit(a)),
        _ => None,
    };
    if !matches!(node.op, Op::Lt | Op::Le | Op::Gt | Op::Ge) {
        return None;
    }
    // `k op x` is `x op' k` with the operator mirrored.
    let (x, k, op) = match (point(node.args[0]), point(node.args[1])) {
        (None, Some(k)) => (node.args[0], k, node.op.clone()),
        (Some(k), None) => (
            node.args[1],
            k,
            match node.op {
                Op::Lt => Op::Gt,
                Op::Le => Op::Ge,
                Op::Gt => Op::Lt,
                _ => Op::Le,
            },
        ),
        _ => return None,
    };
    let pred = A::below(k)?.to_f64();
    let succ = A::below(A::neg(k)?).and_then(A::neg)?.to_f64();
    let (k, inf) = (k.to_f64(), f64::INFINITY);
    let (yes, no) = match op {
        Op::Lt => ((-inf, pred), (k, inf)),
        Op::Le => ((-inf, k), (succ, inf)),
        Op::Gt => ((succ, inf), (-inf, k)),
        _ => ((k, inf), (-inf, pred)),
    };
    Some((x, yes, no))
}

/// A static range of `n` (literals, a bounded input such as a platform's
/// `x`, sums and differences), narrowed by the enclosing branches.
fn bounds<A: GraphArith>(d: &Symbolic<A>, n: NodeId, facts: &[(NodeId, f64, f64)]) -> Option<RangeF64> {
    let node = d.graph.get(n);
    let num = |l: A::Lit| A::from_lit(l).to_f64();
    let structural = || -> Option<RangeF64> {
        Some(match node.op {
            Op::Const(a, b) => (num(a), num(b)),
            Op::Sel => {
                let (a, b) = (bounds(d, node.args[1], facts)?, bounds(d, node.args[2], facts)?);
                (a.0.min(b.0), a.1.max(b.1))
            }
            Op::Flr => {
                let a = bounds(d, node.args[0], facts)?;
                (a.0.floor(), a.1.floor())
            }
            // A fork's fragment lies inside its operand.
            Op::Fork(..) | Op::Frag(..) => bounds(d, node.args[0], facts)?,
            Op::Add => {
                let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
                (a.0 + b.0, a.1 + b.1)
            }
            Op::Sub => {
                let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
                (a.0 - b.1, a.1 - b.0)
            }
            // A bounded input (`Op::Restrict`; as `GraphArith::pieces_of`).
            Op::Restrict(lo, hi) => {
                let (lo, hi) = (num(lo), num(hi));
                match bounds(d, node.args[0], facts) {
                    Some(a) if a.0.max(lo) <= a.1.min(hi) => (a.0.max(lo), a.1.min(hi)),
                    Some(a) => a,
                    None => (lo, hi),
                }
            }
            _ => return None,
        })
    };
    // From its structure, else only from the enclosing branches.
    let mut r = match structural() {
        Some(r) => r,
        None if facts.iter().any(|f| f.0 == n) => (f64::NEG_INFINITY, f64::INFINITY),
        None => return None,
    };
    for &(m, flo, fhi) in facts {
        if m == n {
            r = (r.0.max(flo), r.1.min(fhi));
        }
    }
    Some(r)
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::Pico8Num as P8;
    use crate::trace::cart;
    use crate::trace::interp::Interp;
    use crate::trace::frame::trace_frame;
    use crate::transpile::graph::Val;
    use std::collections::HashMap;

    fn canonical(st: &State<Symbolic>, p: &Path) -> Option<Val> {
        for e in Level::EXACT.entries() {
            for paths in entry_paths(st, e) {
                for (s, q) in e.slots.iter().zip(paths) {
                    match s.stored {
                        Stored::Num(k) if &q == p => return Some(Val::exact_num(P8::from_raw(k))),
                        Stored::Bool(b) if &q == p => return Some(Val::Bool(Some(b))),
                        _ => {}
                    }
                }
            }
        }
        None
    }

    /// THE DEAD FIELDS are unread: the slots the table stores as a constant
    /// (`Stored::Num`/`Bool`: the timers, the key's sprite) traced as free input
    /// cells, every other slot concrete, are read by no stored field outside
    /// them, no guard, error or raise - so the stored constant loses nothing (an
    /// EXACT canonicalization). The one read is an error's OVERFLOW check of
    /// their own arithmetic (`minutes + 1`), which holds at the stored
    /// constants. Every frame of room (4,0)'s 100% route (the player runs,
    /// dashes, takes the key and opens the chest with it).
    #[test]
    fn dead_fields_are_unread() {
        let s = crate::settings::Settings { room: (4, 0), ..Default::default() };
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        let frame = &chunks.frame;
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let mut st = cart::boot(&mut it, &chunks, s.room).expect("boot");
        let dead = |s: &Slot| matches!(s.stored, Stored::Num(_) | Stored::Bool(_));
        let route = crate::concrete::read_inputs("tas/room_4_0_hundred_frame_152.txt").expect("the route");
        let (mut traced, mut with_key, mut with_player, mut has_key) = (0, 0, 0, false);
        let mut overflow_checks = 0;
        for &byte in &route {
            let roots = slots_where(&st, Level::EXACT, dead);
            with_key += roots.iter().any(|p| p.last() == Some(&iface::key("spr"))) as usize;
            with_player += !objects_of_type(&st, "player").is_empty() as usize;
            let f = trace_frame(&mut it, &chunks, st.clone(), &crate::trace::frame::TraceSpec { roots: &roots, concrete: true, ..Default::default() }).expect("trace");
            // Everything a successor depends on, its booleans decided as far as a
            // BDD over independent atoms can (a guard re-merged after a branch on
            // `frames` is `c or not c`, true).
            let mut roots_in = vec![f.raise];
            let mut what = vec!["the raise".to_string()];
            for (k, o) in f.outs.iter().enumerate() {
                roots_in.extend([o.guard, o.error]);
                what.extend([format!("outcome {k}'s guard"), format!("outcome {k}'s error")]);
                // The dead slots as the outcome names them (objects move in
                // `objects` when one before them is destroyed).
                let dead_out = slots_where(&o.st, Level::EXACT, dead);
                for (p, n, _) in &o.fields {
                    if !dead_out.contains(p) {
                        roots_in.push(*n);
                        what.push(iface::show(p));
                    }
                }
            }
            let (g, map, _) = crate::transpile::bdd::simplify_local(&it.d.graph, &roots_in, crate::transpile::bdd::LOCAL_CAP, crate::transpile::bdd::LOCAL_EXPAND);
            // Every input cell is a dead slot's; at its stored constant.
            let stored: HashMap<u32, Val> = f
                .iface
                .slots
                .iter()
                .enumerate()
                .map(|(i, p)| (i as u32, canonical(&st, p).unwrap_or_else(|| panic!("{} is no dead slot", iface::show(p)))))
                .collect();
            let at_stored = g.eval_lenient(&stored).expect("eval");
            for (r, w) in roots_in.iter().zip(&what) {
                // Down to the cells, but not through an overflow check, which
                // must hold at the stored constants instead.
                let mut seen = vec![false; g.len()];
                let mut stack = vec![map[*r as usize]];
                while let Some(n) = stack.pop() {
                    if std::mem::replace(&mut seen[n as usize], true) {
                        continue;
                    }
                    match g.get(n).op {
                        Op::Cell(_) => panic!("{w} reads a dead field"),
                        Op::NoWrap => {
                            assert_eq!(at_stored[n as usize], Val::Bool(Some(true)), "{w}: an overflow check fails at the stored constants");
                            overflow_checks += 1;
                        }
                        _ => stack.extend(g.get(n).args.iter().copied()),
                    }
                }
            }
            traced += 1;
            for b in 0..6 {
                let v = it.d.boolean(byte >> b & 1 == 1);
                iface::set(&mut st, &[iface::key("__button_states"), Step::Idx(b)], Value::Bool(v)).expect("a button");
            }
            // Until the chest's `rnd` makes the concrete frame two (the key
            // is taken and the chest opened by then).
            let Ok(next) = cart::run_one(&mut it, frame, st) else { break };
            st = next;
            has_key |= matches!(iface::get(&st, &[iface::key("has_key")]), Some(Value::Bool(b)) if it.d.decide(&b) == Some(true));
        }
        assert!(has_key && with_key > 60 && with_player > 60, "{traced} frames traced: {with_key} with the key, {with_player} with the player, the key taken: {has_key}");
        assert!(overflow_checks > 0, "the timers' own arithmetic is checked");
    }
}
