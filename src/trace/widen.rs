//! The WIDENING TABLE (`celeste_engine::widening::TABLE`) in the traced
//! graph: at the frame's end every entry of the tracer's level becomes graph
//! ops writing what the entry stores, with the containment it owes as the
//! widening's own per-lane error (`SlotErrors`); at the frame's start
//! (`read_inputs`) every entry's slots are read as its `Input` says. The
//! block side (`Rt2::widen_to`) interprets the same table; nothing here may
//! widen what the table does not name. The two hooks (`Hook::NearFloor`,
//! `Hook::PlatformInputs`) are implemented below.

use anyhow::{bail, Result};

use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::widening::{floor_player_window, Entry, Flag, Hook, Input, Level, Proof, Slot, Stored, Target, FLOOR_HITBOX, PLATFORM_PATH, PLAYER_HITBOX, REM, TABLE};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;
use crate::transpile::graph::{NodeId, Op};

/// Write every `Stored::AbsentAsZero` field an object lacks as the number 0
/// (every frame's outcome, widened or not: it is heap shape).
pub fn materialize_absent_fields<D: Domain>(st: &mut State<D>, d: &mut D) -> Result<()> {
    for (ty, f) in celeste_engine::widening::absent_as_zero() {
        for obj in objects_of_type(st, ty) {
            let Some(Value::Table(t)) = iface::get(st, &obj) else { bail!("{}: not a table", iface::show(&obj)) };
            match st.heap.tables[&t].hash.get(f) {
                None | Some(Value::Nil) => {
                    let zero = Value::Num(d.num(P8::from_raw(0)));
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

/// Entry `e`'s slots in `st`: per instance (one for `Target::Globals`), the
/// path of each slot, in the entry's slot order.
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

/// The countdown FIELD NAMES a near level stores as the unknown number (the
/// interpreter's hint, `Domain::set_countdown_hint`).
pub fn countdown_fields() -> &'static [&'static str] {
    static F: std::sync::OnceLock<Vec<&'static str>> = std::sync::OnceLock::new();
    F.get_or_init(|| {
        let mut out: Vec<&'static str> = TABLE
            .iter()
            .filter(|e| e.flag == Flag::Near)
            .flat_map(|e| e.slots.iter())
            .filter(|s| s.stored == Stored::UnknownNum)
            .map(|s| *s.field.last().expect("a field"))
            .collect();
        out.sort_unstable();
        out.dedup();
        out
    })
}

/// A constant interval `[lo, hi]` (raw) as one graph node.
fn ival(d: &mut Symbolic, lo: i32, hi: i32) -> NodeId {
    d.graph.leaf(Op::Const(lo, hi))
}

/// What the output widenings OWE: per widened slot, the condition that the
/// replaced value was inside what it wrote; a lane where one fails declines.
pub type SlotErrors = Vec<(Path, <Symbolic as Domain>::Bool)>;

/// The widening of `p` is defined only where `holds`.
fn owe(errs: &mut SlotErrors, d: &mut Symbolic, p: &Path, holds: <Symbolic as Domain>::Bool) {
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
pub fn widen(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<SlotErrors> {
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

/// One entry's slots, per instance, as stored (`Stored`).
fn widen_entry(st: &mut State<Symbolic>, d: &mut Symbolic, e: &Entry, errs: &mut SlotErrors) -> Result<()> {
    for paths in entry_paths(st, e) {
        // Per slot so far: its value before the widening, and what it owed.
        let mut seen: Vec<(Option<Value<Symbolic>>, Option<NodeId>)> = Vec::new();
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
                Stored::Range { lo, hi, proof } => {
                    owed = range_holds(d, num()?, (lo, hi), proof)?;
                    if let Some(h) = owed {
                        owe(errs, d, p, h);
                    }
                    Value::Num(ival(d, lo, hi))
                }
                Stored::Band { around, radius, proof } => {
                    let sp = field(&p[..p.len() - 1].to_vec(), &[around]);
                    let Some(Value::Num(centre)) = iface::get(st, &sp) else { bail!("{}: not a number", iface::show(&sp)) };
                    let v = num()?;
                    match proof {
                        // The band as the centre's own arithmetic, per lane.
                        Proof::PerLane => {
                            let amp = d.num(P8::from_raw(radius));
                            let l = d.arith(super::domain::Arith::Sub, &centre, &amp)?;
                            let h = d.arith(super::domain::Arith::Add, &centre, &amp)?;
                            let a = d.compare(super::domain::Cmp::Ge, &v, &l)?;
                            let b = d.compare(super::domain::Cmp::Le, &v, &h)?;
                            let inside = d.and(&a, &b);
                            owe(errs, d, p, inside);
                            owed = Some(inside);
                            Value::Num(d.graph.fold(Op::Span, vec![l, h]))
                        }
                        // A constant centre: the band is a literal range.
                        Proof::Static | Proof::Literals => {
                            let Some(c) = d.as_const(&centre) else { bail!("{}: not a constant", iface::show(&sp)) };
                            let (lo, hi) = (c.to_bits() as i32 - radius, c.to_bits() as i32 + radius);
                            owed = range_holds(d, v, (lo, hi), proof)?;
                            if let Some(h) = owed {
                                owe(errs, d, p, h);
                            }
                            Value::Num(ival(d, lo, hi))
                        }
                    }
                }
                Stored::Phase { lo, hi } => Value::Num(ival(d, lo, hi)),
                Stored::AtLeast(k) => {
                    let v = num()?;
                    let k = d.num(P8::from_raw(k));
                    Value::Num(d.fun2(super::domain::Fun2::Max, &v, &k)?)
                }
                Stored::UnknownNum => {
                    num()?;
                    Value::Num(d.unknown_num())
                }
                Stored::UnknownBool => {
                    let Value::Bool(_) = old_v else { bail!("{}: not a boolean (widening {:?})", iface::show(p), e.name) };
                    Value::Bool(d.unknown_bool_output())
                }
                Stored::Num(k) => Value::Num(d.num(P8::from_raw(k))),
                Stored::Bool(b) => Value::Bool(d.boolean(b)),
                Stored::FullPeriod(period) => {
                    let v = num()?;
                    if !d.is_interval(&v) {
                        seen.push((old, None));
                        continue;
                    }
                    let (lo, hi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
                    let width = d.graph.fold(Op::Sub, vec![hi, lo]);
                    let full_period = d.graph.leaf(Op::Const(period, period));
                    let full = d.graph.fold(Op::Ge, vec![width, full_period]);
                    owe(errs, d, p, full);
                    Value::Num(ival(d, 0, period))
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

/// `v` inside `[lo, hi]` (raw) as `proof` shows it: `None` if proved, else
/// the per-lane condition.
fn range_holds(d: &mut Symbolic, v: NodeId, (lo, hi): (i32, i32), proof: Proof) -> Result<Option<NodeId>> {
    Ok(match proof {
        Proof::PerLane => {
            let (klo, khi) = (d.num(P8::from_raw(lo)), d.num(P8::from_raw(hi)));
            let a = d.compare(super::domain::Cmp::Ge, &v, &klo)?;
            let b = d.compare(super::domain::Cmp::Le, &v, &khi)?;
            Some(d.and(&a, &b))
        }
        Proof::Static => contain(d, v, (lo as i64, hi as i64)),
        Proof::Literals => {
            let mut arms = Vec::new();
            if literal_arms(&d.graph, v, &mut arms) && arms.iter().all(|(a, b)| lo <= *a && *b <= hi) {
                None
            } else {
                Some(bounds_inside(d, v, lo, hi))
            }
        }
    })
}

/// Every value a lane can hold: a literal, or a select over such values.
fn literal_arms(g: &crate::transpile::graph::Graph, n: NodeId, out: &mut Vec<(i32, i32)>) -> bool {
    let node = g.get(n);
    match node.op {
        Op::Const(a, b) => {
            out.push((a, b));
            true
        }
        Op::Sel => literal_arms(g, node.args[1], out) && literal_arms(g, node.args[2], out),
        _ => false,
    }
}

/// `lo <= Lo(v) and Hi(v) <= hi`, per lane.
fn bounds_inside(d: &mut Symbolic, v: NodeId, lo: i32, hi: i32) -> NodeId {
    let (klo, khi) = (d.graph.leaf(Op::Const(lo, lo)), d.graph.leaf(Op::Const(hi, hi)));
    let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
    let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
    let below = d.graph.fold(Op::Le, vec![vhi, khi]);
    d.graph.fold(Op::And, vec![above, below])
}

/// `v` in `[lo, hi]` (raw): `None` if proved statically, else the per-lane
/// condition.
fn contain(d: &mut Symbolic, v: NodeId, (lo, hi): (i64, i64)) -> Option<<Symbolic as Domain>::Bool> {
    if within(d, v, (lo, hi), &mut Vec::new()) {
        return None;
    }
    Some(bounds_inside(d, v, lo as i32, hi as i32))
}

/// The INPUT side: every entry of the tracer's level read as its slots'
/// `Input` says, in table order, before the frame reads anything. Returned:
/// the per-lane obligations on the raw inputs (a `Literal`'s containment,
/// the platforms' hook's), which the caller makes errors of the whole frame.
pub fn read_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<Vec<NodeId>> {
    let mut obligations = Vec::new();
    let mid = mid_frame(st);
    // A near level reads an absent countdown as the unknown number too: the
    // absent field is 0 (`Stored::AbsentAsZero`), which the unknown holds.
    // Mid split frame the countdowns stay as the first step left them.
    if d.level.floors_near && !mid {
        materialize_absent_fields(st, d)?;
    }
    d.platform_cells.clear();
    for e in d.level.entries() {
        match e.hook {
            Some(Hook::NearFloor) => read_near_floors(st, d, e, mid)?,
            // Once, for both platform entries.
            Some(Hook::PlatformInputs) if e.slots[0].field == ["x"] => obligations.extend(platform_inputs(st, d)?),
            Some(Hook::PlatformInputs) => {}
            None => {
                for paths in entry_paths(st, e) {
                    for (s, p) in e.slots.iter().zip(&paths) {
                        obligations.extend(read_slot(st, d, e, s, p)?);
                    }
                }
            }
        }
    }
    Ok(obligations)
}

/// One slot's input (`Input`), and its obligation if it owes one.
fn read_slot(st: &mut State<Symbolic>, d: &mut Symbolic, e: &Entry, s: &Slot, p: &Path) -> Result<Option<NodeId>> {
    let what = |kind: &str| anyhow::anyhow!("{}: not a {kind} (widening {:?})", iface::show(p), e.name);
    let new = match (s.input, iface::get(st, p)) {
        (Input::Stored, _) => return Ok(None),
        (Input::Hook, _) => bail!("{}: widening {:?} reads it in a hook it does not have", iface::show(p), e.name),
        (_, None) if s.optional => return Ok(None),
        (Input::BothWays, Some(Value::Bool(_))) => Value::Bool(d.both_values(s.field.last().expect("a field"))),
        (Input::Atom, Some(Value::Bool(_))) => Value::Bool(d.unknown_bool_atom()),
        (Input::BothWays | Input::Atom, _) => return Err(what("boolean")),
        (Input::Unknown, Some(Value::Num(_))) => Value::Num(d.unknown_num()),
        (Input::Literal, Some(Value::Num(v))) => {
            let Stored::Range { lo, hi, .. } = s.stored else { bail!("{}: a literal input reads a range", iface::show(p)) };
            let owed = contain(d, v, (lo as i64, hi as i64));
            let r = ival(d, lo, hi);
            iface::set(st, p, Value::Num(r))?;
            return Ok(owed);
        }
        (Input::Unknown | Input::Literal, _) => return Err(what("number")),
    };
    iface::set(st, p, new)?;
    Ok(None)
}

/// An object's `(x, y)`: constants, floors and springs never move.
fn position<D: Domain>(st: &State<D>, d: &D, obj: &Path) -> Result<(P8, P8)> {
    let get = |f: &str| -> Result<P8> {
        let p = field(obj, &[f]);
        match iface::get(st, &p) {
            Some(Value::Num(v)) => d.as_const(&v).ok_or_else(|| anyhow::anyhow!("{}: not a constant", iface::show(&p))),
            _ => bail!("{}: not a number", iface::show(&p)),
        }
    };
    Ok((get("x")?, get("y")?))
}

/// The slots a hook stores as an interval on some lanes and exact on others
/// (a near floor's `state`): an interval column in every outcome (the
/// column type is per shape).
pub fn near_floor_states(st: &State<Symbolic>, level: Level) -> Vec<Path> {
    level.entries().filter(|e| e.hook == Some(Hook::NearFloor)).flat_map(|e| entry_paths(st, e)).map(|paths| paths[0].clone()).collect()
}

/// `Hook::NearFloor`, INPUT: a floor's `state` as stored (split by
/// `verify::split_undecided_selects`), `collideable` DERIVED as `state ~= 2`
/// (the cart keeps them in step; an independent unknown `collideable` would
/// admit players inside solid floors). Mid split frame a lane may hold an
/// unknown `collideable`, but a boolean slot binds as a decided cell that
/// would read it as false: the cell where the lane knows it, else a fork of
/// both values.
fn read_near_floors(st: &mut State<Symbolic>, d: &mut Symbolic, e: &Entry, mid: bool) -> Result<()> {
    use super::domain::Cmp;
    let two = if mid { None } else { Some(d.num(P8::from_i16(2))) };
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
/// value owed; storing the computed value makes the split resolve every
/// floor the player MIGHT overlap (room (6,1): 3,638 outcomes vs 108) -
/// except with the platforms unknown, where it stores the computed value
/// (below). Mid split frame (`mid`), `collideable` also stays computed in the
/// `PLAYER_PROBE` window: widening there would let the second step reach
/// states the unsplit frame does not. `Rt2::widen_to` agrees.
fn widen_near_floors(st: &mut State<Symbolic>, d: &mut Symbolic, e: &Entry, errs: &mut SlotErrors, mid: bool) -> Result<()> {
    use super::domain::Cmp;
    // The hitboxes `floor_player_window` assumes, checked on the state.
    let hitbox = |st: &State<Symbolic>, d: &Symbolic, obj: &Path, want: [i16; 4]| -> Result<()> {
        for (f, w) in ["x", "y", "w", "h"].iter().zip(want) {
            let p = field(obj, &["hitbox", f]);
            let got = match iface::get(st, &p) {
                Some(Value::Num(v)) => d.as_const(&v),
                _ => None,
            };
            anyhow::ensure!(got == Some(P8::from_i16(w)), "{}: {got:?}, the near widening assumes {w}", iface::show(&p));
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
        let [(xlo, xhi), (ylo, yhi)] = floor_player_window(at);
        // `lo < v < hi` for every value a lane may hold (straddling is no
        // overlap, the safe side), as `widening::player_overlaps_floor`.
        let inside = |d: &mut Symbolic, v: NodeId, lo: P8, hi: P8| -> Result<NodeId> {
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
                let x = inside(d, px, xlo + P8::from_i16(dxlo), xhi + P8::from_i16(dxhi))?;
                let y = inside(d, py, ylo + P8::from_i16(dylo), yhi + P8::from_i16(dyhi))?;
                let both = d.and(&x, &y);
                probe = d.or(&probe, &both);
            }
        }
        let Some(Value::Num(state)) = iface::get(st, ps) else { bail!("{}: not a number", iface::show(ps)) };
        if let Some(holds) = contain(d, state, (slo as i64, shi as i64)) {
            let held = d.or(&overlap, &holds);
            owe(errs, d, ps, held);
        }
        // Overlapped: hidden, owed as `collideable` false, which carries
        // `state` 2 too. Do not owe `state == 2` separately: a shaking floor's
        // `sel(delay - 1 <= 0, 2, 1)` is undecided and would decline the lane.
        let apart = d.not(&overlap);
        let two = d.num(P8::from_i16(2));
        let range = d.graph.leaf(Op::Const(slo, shi));
        // With the platforms unknown, the overlapped floor stores what the
        // frame COMPUTED instead (exact, nothing owed): there the hidden
        // invariant failed on every lane of the spawn's first player frame in
        // room (2,1) `r0sxhnp` (a KERNEL COVERAGE GAP at f26: some outcome
        // computes the overlapped floor solid), which the near level alone
        // never meets. A stored computed `state`/`collideable` agrees with
        // `Rt2::widen_to` (it keeps an overlapped floor's values as they are).
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
/// level. Which world platform each is, by constant `y` and `dir` (rows are
/// in canonical order; platforms alike in both are interchangeable); `x` an
/// input cell restricted to the path (`Op::Restrict`) and recorded as the
/// world's cell, so the split decides player-vs-platform comparisons per
/// PLATFORM WORLD, keeping platforms mutually consistent; `last` read as `x`
/// (checked at the output, `Stored::SameAs`); `rem.x` the literal whole
/// remainder (as `Input::Literal`: a literal's floor rejoins as fragments;
/// the price is a pixel of slack at this level); each unpinned `spd.x`
/// through its range over the worlds - with no player, as the literal of its
/// one moving speed. Not table slots: all of it is per WORLD (which world
/// platform an object is, and what its inputs may be there), which a slot
/// cannot say. Returned: per-lane obligations (the no-player speed; `rem.x`
/// inside its literal).
pub fn platform_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<Vec<NodeId>> {
    let worlds = d.worlds.clone().ok_or_else(|| anyhow::anyhow!("the platforms are unknown but there is no world table"))?;
    let platforms = objects_of_type(st, "platform");
    let first = worlds.first().ok_or_else(|| anyhow::anyhow!("no platform world"))?;
    anyhow::ensure!(first.len() == platforms.len(), "a platform world has {} platforms, the state {}", first.len(), platforms.len());
    fn konst(st: &State<Symbolic>, d: &Symbolic, p: &Path) -> Result<i32> {
        match iface::get(st, p) {
            Some(Value::Num(n)) => match d.graph.get(n).op {
                Op::Const(a, b) if a == b => Ok(a),
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
            .ok_or_else(|| anyhow::anyhow!("no platform world entry at y {y:#x} dir {dir:#x} left for {}", iface::show(obj)))?;
        taken[j] = true;
        let spd = field(obj, &["spd", "x"]);
        let moving: Vec<i32> = {
            let mut v: Vec<i32> = worlds.iter().map(|w| w[j][3]).filter(|s| *s != 0).collect();
            v.sort_unstable();
            v.dedup();
            v
        };
        if let (Some(Value::Num(sv)), None, [v]) = (iface::get(st, &spd), super::shapes::player_path(st), moving.as_slice()) {
            // NO PLAYER: a platform's move is unobservable, so read the speed
            // as the literal (fragments, not a fork per platform); asserted
            // per lane that the speed is 0 or that one.
            if matches!(d.graph.get(sv).op, Op::Cell(_)) {
                let (k0, kv) = (d.graph.leaf(Op::Const(0, 0)), d.graph.leaf(Op::Const(*v, *v)));
                let (a, b) = (d.graph.fold(Op::Eq, vec![sv, k0]), d.graph.fold(Op::Eq, vec![sv, kv]));
                obligations.push(d.graph.fold(Op::Or, vec![a, b]));
                iface::set(st, &spd, Value::Num(kv))?;
            }
        } else if let Some(Value::Num(sv)) = iface::get(st, &spd) {
            if matches!(d.graph.get(sv).op, Op::Cell(_)) {
                let lo = worlds.iter().map(|w| w[j][3]).min().expect("a world");
                let hi = worlds.iter().map(|w| w[j][3]).max().expect("a world");
                let r = d.restrict(sv, lo, hi);
                iface::set(st, &spd, Value::Num(r))?;
            }
        }
        let (x, last, rem) = (field(obj, &["x"]), field(obj, &["last"]), field(obj, &["rem", "x"]));
        let Some(Value::Num(xv)) = iface::get(st, &x) else { bail!("{}: not a number", iface::show(&x)) };
        anyhow::ensure!(matches!(d.graph.get(xv).op, Op::Cell(_)), "a platform's `x` must be an input cell");
        let xr = d.restrict(xv, (PLATFORM_PATH.0 as i32) << 16, (PLATFORM_PATH.1 as i32) << 16);
        iface::set(st, &x, Value::Num(xr))?;
        iface::set(st, &last, Value::Num(xr))?;
        // `rem.x` is replaced by the literal whole remainder: sound for a
        // lane whose own lies inside, which is owed.
        let Some(Value::Num(rv)) = iface::get(st, &rem) else { bail!("{}: not a number", iface::show(&rem)) };
        obligations.extend(contain(d, rv, (REM.0 as i64, REM.1 as i64)));
        let r = ival(d, REM.0, REM.1);
        iface::set(st, &rem, Value::Num(r))?;
        cells[j] = Some(xv);
    }
    d.platform_cells = cells.into_iter().map(|c| c.expect("every world platform matched")).collect();
    Ok(obligations)
}

/// Does `v` provably lie in `[lo, hi]` (raw)? Through select arms and under
/// point-split branches with what they say about the compared value (so a
/// wrap `x < -16 ? 128 : ...` is bounded). Static; `false` if unsure.
fn within(d: &Symbolic, v: NodeId, (lo, hi): (i64, i64), facts: &mut Vec<(NodeId, i64, i64)>) -> bool {
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

/// `c` as `x op k` (`k` a literal point, either side): `x` and its raw range
/// `(when true, when false)`.
fn comparison_facts(d: &Symbolic, c: NodeId) -> Option<(NodeId, (i64, i64), (i64, i64))> {
    let node = d.graph.get(c);
    let point = |n: NodeId| match d.graph.get(n).op {
        Op::Const(a, b) if a == b => Some(a as i64),
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
    let (yes, no) = match op {
        Op::Lt => ((i64::MIN, k - 1), (k, i64::MAX)),
        Op::Le => ((i64::MIN, k), (k + 1, i64::MAX)),
        Op::Gt => ((k + 1, i64::MAX), (i64::MIN, k)),
        _ => ((k, i64::MAX), (i64::MIN, k - 1)),
    };
    Some((x, yes, no))
}

/// A static raw range of `n` (literals, a bounded input - a platform's
/// `x` among them -, sums and differences), narrowed by the enclosing branches; `None` otherwise.
fn bounds(d: &Symbolic, n: NodeId, facts: &[(NodeId, i64, i64)]) -> Option<(i64, i64)> {
    let node = d.graph.get(n);
    let structural = || -> Option<(i64, i64)> {
        Some(match node.op {
            Op::Const(a, b) => (a as i64, b as i64),
            Op::Sel => {
                let (a, b) = (bounds(d, node.args[1], facts)?, bounds(d, node.args[2], facts)?);
                (a.0.min(b.0), a.1.max(b.1))
            }
            Op::Flr => {
                let a = bounds(d, node.args[0], facts)?;
                (a.0.div_euclid(1 << 16) << 16, a.1.div_euclid(1 << 16) << 16)
            }
            // A fork's fragment lies inside its operand.
            Op::Split(_) | Op::Frag(_) | Op::IntFrag(_) => bounds(d, node.args[0], facts)?,
            Op::Add => {
                let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
                (a.0 + b.0, a.1 + b.1)
            }
            Op::Sub => {
                let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
                (a.0 - b.1, a.1 - b.0)
            }
            // A bounded input (`Op::Restrict`; as `graph::pieces_of`).
            Op::Restrict(lo, hi) => {
                let (lo, hi) = (lo as i64, hi as i64);
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
        None if facts.iter().any(|f| f.0 == n) => (i64::MIN, i64::MAX),
        None => return None,
    };
    for &(m, flo, fhi) in facts {
        if m == n {
            r = (r.0.max(flo), r.1.min(fhi));
        }
    }
    Some(r)
}
