//! The boundary's widenings, applied INSIDE the traced frame.
//!
//! `Rt2::boundary_canonicalize` widens four things a moment after a
//! frame produces them: the player's `rem` becomes a constant interval,
//! the four timer globals are pinned to zero, the player's
//! `dash_effect_time` is clamped at zero, and a live fruit's `off`/`y`
//! become its whole bob band. The frame computes precise values for all
//! of them and the boundary throws them away.
//!
//! Doing it here instead means the GRAPH knows about the widening, and
//! that is the point (Philippe, 2026-08-23): "the graph should be
//! hashing the thing it is going to store in the future... we shouldn't
//! be storing something that might then get widened in a different way
//! later."
//!
//! What it buys is not saved arithmetic - only ~2.5% of the graph is
//! dead once these are erased, because `rem` still feeds `amount` which
//! still feeds the position. It buys CANONICAL ROWS, which is what lets
//! a kernel dedup its own output: two rows differing only in a `rem`
//! about to be erased compare equal here, and so do rows from different
//! lanes, which is the half a per-lane dedup cannot reach.
//!
//! ## The assertions come with it
//!
//! The boundary does not just widen, it CHECKS: `rem` was already inside
//! the interval, the fruit's `y` was already inside the band. Widening
//! earlier would silently retire those checks, because the boundary
//! would then be handed the widened value and pass trivially.
//!
//! So each one becomes an `ok` conjunct instead - a lane that violates
//! it is refused rather than quietly accepted, which under the
//! never-deopt doctrine stops the run and names itself.

use anyhow::{bail, Result};

use celeste_core::pico8_num::Pico8Num as P8;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;
use crate::transpile::graph::Op;

/// THE ABSENT NUMBER FIELDS (room (3,0), 2026-09-17, plans/room30.md):
/// `(type global, field)`, a field the object's `init` does not set and a
/// later update adds as a number. A fall floor gets `delay` the first time
/// it breaks and keeps it, so "which floors have ever broken" was 12 bits of
/// the HEAP SHAPE (up to 4,096 shapes; the walk found 128 in 150 s).
///
/// Every frame's outcomes (the tracer's `trace_frame`, the reference
/// engine's `run_frame_all`) write the missing field as the number 0, at
/// every level: it is part of the shape, not of the precision. Sound because
/// the cart cannot tell nil from 0 through what it does with the field:
/// `cart::check_absent_fields` refuses a cart in which any read of it is not
/// a direct operand of arithmetic or an ordering comparison - on nil those
/// are runtime errors that halt PICO-8, so a path that reads the missing
/// value is one the real game never continues - and `Interp::index_key`
/// refuses a computed `t[k]` that names it.
// TODO(Philippe, 2026-09-17): writing the missing field as 0 is a hack, good
// enough for now; revisit (a proper nil-or-number, or a derived rule instead
// of this list).
pub const ABSENT_AS_ZERO: &[(&str, &str)] = &[("fall_floor", "delay")];

/// Write every `ABSENT_AS_ZERO` field an object lacks as the number 0.
pub fn materialize_absent_fields<D: Domain>(st: &mut State<D>, d: &mut D) -> Result<()> {
    for (ty, f) in ABSENT_AS_ZERO {
        for obj in objects_of_type(st, ty) {
            let Some(Value::Table(t)) = iface::get(st, &obj) else { bail!("{}: not a table", iface::show(&obj)) };
            match st.heap.tables[&t].hash.get(*f) {
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

/// Objects whose `type` is the global `name` - the rule `mark_walk` uses
/// to find the player, rather than a position in the object list, since
/// which object is the player changes within a room.
fn objects_of_type<D: Domain>(st: &State<D>, name: &str) -> Vec<Path> {
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

fn field(base: &Path, names: &[&str]) -> Path {
    let mut p = base.clone();
    for n in names {
        p.push(iface::key(n));
    }
    p
}

/// A constant interval `[lo, hi]` as one graph node. `Op::Const(lo, hi)`
/// with `lo != hi` IS the interval literal - the same node the emitter
/// renders as an `IV`.
fn ival(d: &mut Symbolic, lo: P8, hi: P8) -> <Symbolic as Domain>::Num {
    d.graph.leaf(Op::Const(lo.as_raw_u32() as i32, hi.as_raw_u32() as i32))
}

/// Conjoin `c` into the state's obligation.
fn require(st: &mut State<Symbolic>, d: &mut Symbolic, c: <Symbolic as Domain>::Bool) {
    let ok = st.ok;
    st.ok = d.and(&ok, &c);
}

/// Which boundary widenings a traced frame bakes into its graph.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum WidenMode {
    /// The full Bits(0) boundary widenings (rem -> [-0.5, 0.5), the
    /// timer globals -> 0, `dash_effect_time` -> max(0, .), a live
    /// fruit's `off`/`y` -> its bob band), all in the graph. The
    /// production level-0 set. Carries the level's spd and position
    /// precisions (the position rung below level 0 is a Level0 set with
    /// a position bucket: `Level::grid_consistent`).
    Level0(crate::interpreter::abstraction::SpdPrecision, crate::interpreter::abstraction::PosPrecision),
    /// ONLY the rem widening, at the configured Bits(k) rung, via a
    /// bucket fork + snap. Phase 1 of moving the ladder widening into
    /// the graph (plans/keying-widening-flow.md): the rung-agnostic
    /// ladder kernel emits EXACT rem and leaves the rung widening to the
    /// campaign boundary; this makes a rung-SPECIFIC variant that emits
    /// the widened rem so the row is keyed on the value it stores.
    /// Everything else (spd, fruit, timers, conservative widenings) is
    /// still left to the boundary in this phase.
    RemRung(crate::interpreter::abstraction::RemPrecision, crate::interpreter::abstraction::SpdPrecision),
}

/// Apply the boundary widenings selected by `mode` to `st`.
///
/// Every widening `make_state_abstract` applies, in the graph, so a
/// widen-in-graph kernel emits a FIXED POINT of it: the campaign boundary
/// re-abstracting the output changes nothing (the assert-noop property).
/// The fruit / dash / timer widenings are rung-INDEPENDENT
/// (`make_state_abstract_rem` widens the fruit at every non-exact rung,
/// `apply_conservative_widenings` clamps dash and pins timers regardless),
/// so both modes apply them; only rem and spd differ by rung.
pub fn widen(st: &mut State<Symbolic>, d: &mut Symbolic, mode: WidenMode) -> Result<()> {
    use crate::interpreter::abstraction::RemPrecision;
    // The level's rem and spd precisions (`abstraction::Level`), explicit
    // so every level's set can be traced at once.
    let (rem, spd, pos) = match mode {
        WidenMode::Level0(spd, pos) => (RemPrecision::Bits(0), spd, pos),
        WidenMode::RemRung(rem, spd) => (rem, spd, crate::interpreter::abstraction::PosPrecision::EXACT),
    };
    widen_rem(st, d, rem)?;
    widen_spd(st, d, spd)?;
    widen_pos(st, d, pos)?;
    widen_dash(st, d)?;
    widen_fruit(st, d)?;
    widen_timers(st, d)?;
    widen_fly_fruit(st, d)?;
    widen_fall_floors(st, d)?;
    widen_platforms(st, d)?;
    canon_balloon_offset(st, d)?;
    widen_held(st, d)?;
    Ok(())
}

/// The balloon's phase `offset` (2026-09-21, room (5,0)): a `rnd` draw, an
/// interval `[a, a + 1)` a full period wide at every level, which advances
/// 0.01 a frame only while the balloon shows - so each pop at a different
/// frame shifts it by another 0.01: a different key for the same future
/// (14 values at room (5,0) f60). Its one reader is `sin(offset)`, and `sin`
/// of any interval a full period wide is `[-1, 1]` (`Symbolic::fun1`), so all
/// of them behave alike and the row stores the canonical `[0, 1)` - EXACT, at
/// every level, and what lets the floors-unknown balloon `timer`
/// (`fall_floor_paths`) actually merge pop histories. The premise that the
/// interval is a full period rides on `ok`: a narrower phase (a concrete draw)
/// declines loudly, never widened. The block model projects the same way
/// (`Rt2::widen_to`), for the mark filter.
fn canon_balloon_offset(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for obj in objects_of_type(st, "balloon") {
        let p = field(&obj, &["offset"]);
        let Some(Value::Num(v)) = iface::get(st, &p) else { bail!("{}: not a number", iface::show(&p)) };
        if !d.is_interval(&v) {
            continue;
        }
        let (lo, hi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
        let width = d.graph.fold(Op::Sub, vec![hi, lo]);
        let period = d.graph.leaf(Op::Const(BALLOON_PERIOD_RAW, BALLOON_PERIOD_RAW));
        let full = d.graph.fold(Op::Ge, vec![width, period]);
        st.ok = d.graph.fold(Op::And, vec![st.ok, full]);
        let canon = d.graph.leaf(Op::Const(0, BALLOON_PERIOD_RAW));
        iface::set(st, &p, Value::Num(canon))?;
    }
    Ok(())
}

use celeste_engine::runtime2::{BALLOON_PERIOD_RAW, PLATFORM_PATH, PLATFORM_REM};

/// Held buttons unknown (plans/held-buttons.md): the player's trails leave the
/// frame unknown - the canonical output unknown (`Symbolic::unknown_bool_output`),
/// stored uniform (`verify::out_fields`), whatever each fork configuration
/// computed.
fn widen_held(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    if !d.held_unknown {
        return Ok(());
    }
    for obj in objects_of_type(st, "player") {
        for f in ["p_jump", "p_dash"] {
            let p = field(&obj, &[f]);
            let Some(Value::Bool(_)) = iface::get(st, &p) else { bail!("{}: not a boolean", iface::show(&p)) };
            let b = d.unknown_bool_output();
            iface::set(st, &p, Value::Bool(b))?;
        }
    }
    Ok(())
}

/// The fields a floors-unknown level widens, per fall floor
/// (plans/fall-floors.md).
pub struct FallFloorPaths {
    /// `state` and `delay`: the unknown number.
    pub unknown: Vec<Path>,
    /// `collideable`: an unknown boolean.
    pub collideable: Vec<Path>,
}

impl FallFloorPaths {
    pub fn all(&self) -> impl Iterator<Item = &Path> {
        self.unknown.iter().chain(self.collideable.iter())
    }
}

pub fn fall_floor_paths<D: Domain>(st: &State<D>) -> FallFloorPaths {
    let mut out = FallFloorPaths { unknown: Vec::new(), collideable: Vec::new() };
    for obj in objects_of_type(st, "fall_floor") {
        out.unknown.push(field(&obj, &["state"]));
        out.unknown.push(field(&obj, &["delay"]));
        out.collideable.push(field(&obj, &["collideable"]));
    }
    // The balloon's respawn `timer` (2026-09-21, room (5,0)): under the same
    // flag, the unknown number. While the balloon shows, the timer is dead (a
    // pop sets it to 60), so the merge is exact; while it is popped, an
    // unknown timer lets it respawn on any frame - the over-approximation the
    // exact-floors levels narrow back. Every pop at a different frame carried
    // its own countdown next to the same player state (room (5,0) level 0: a
    // balloon multiplier of 2.25x at f70).
    for obj in objects_of_type(st, "balloon") {
        out.unknown.push(field(&obj, &["timer"]));
    }
    out
}

/// The fall floors' INPUT side at a floors-unknown level (plans/fall-floors.md):
/// `state` and `delay` the unknown number, `collideable` an undecided atom -
/// every lane alike, no input cell read. A floor that never broke has no
/// `delay` in the post-`_init` start state; it is materialized first
/// (`ABSENT_AS_ZERO`, as every frame's output does), then replaced. A decided
/// input lies inside and only over-approximates, so there is no premise.
///
/// `collideable` is read by other objects' collisions (the player's
/// `is_solid`), whose decisions are the player's data, and an atom there leaves
/// them undecided and their arms apart. But the floor's own update joins its
/// `collideable` writes into a fresh atom, and that one becomes a fork of both
/// values at the update's return (`Domain::escaped_atom`): what the player,
/// updating after the floors, reads. A fork here would be dead, one fork id per
/// floor (a `ChoiceSet` holds 58).
pub fn fork_floor_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    materialize_absent_fields(st, d)?;
    replace_fall_floors(st, d, false)
}

/// The fall floors' OUTPUT side at a floors-unknown level: `state` and `delay`
/// the unknown number (stored `AV::UNum`, `emit::bind`), `collideable` the
/// canonical output unknown (stored `AV::UBool`, `verify::out_fields`). The
/// unknown contains whatever the frame computed, so there is nothing to check.
fn widen_fall_floors(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    if !d.floors_unknown {
        return Ok(());
    }
    replace_fall_floors(st, d, true)
}

/// `output`: the canonical output unknown for `collideable` (never read in
/// the frame), else a fresh atom (read by collisions: independent per floor).
fn replace_fall_floors(st: &mut State<Symbolic>, d: &mut Symbolic, output: bool) -> Result<()> {
    let fp = fall_floor_paths(st);
    for p in &fp.unknown {
        let Some(Value::Num(_)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        let u = d.unknown_num();
        iface::set(st, p, Value::Num(u))?;
    }
    for p in &fp.collideable {
        let Some(Value::Bool(_)) = iface::get(st, p) else { bail!("{}: not a boolean", iface::show(p)) };
        let b = if output { d.unknown_bool_output() } else { d.unknown_bool_atom() };
        iface::set(st, p, Value::Bool(b))?;
    }
    Ok(())
}

/// The fields a platforms-unknown level widens, per moving platform
/// (plans/platforms-unknown.md).
pub struct PlatformPaths {
    pub x: Vec<Path>,
    pub last: Vec<Path>,
    pub rem_x: Vec<Path>,
}

impl PlatformPaths {
    pub fn all(&self) -> impl Iterator<Item = &Path> {
        self.x.iter().chain(self.last.iter()).chain(self.rem_x.iter())
    }
}

pub fn platform_paths<D: Domain>(st: &State<D>) -> PlatformPaths {
    let mut out = PlatformPaths { x: Vec::new(), last: Vec::new(), rem_x: Vec::new() };
    for obj in objects_of_type(st, "platform") {
        out.x.push(field(&obj, &["x"]));
        out.last.push(field(&obj, &["last"]));
        out.rem_x.push(field(&obj, &["rem", "x"]));
    }
    out
}

/// The platforms' INPUT side at a platforms-unknown level
/// (plans/platforms-unknown.md): `x` stays the interval input cell - per-lane
/// data, so a comparison with it is a point split and it keeps its identity -
/// and `last` is bound to that SAME value: `last == x` at every frame
/// boundary, proved by every traced outcome (`widen_platforms`) and the start
/// state. `rem.x` is the whole remainder as a literal, so the move's
/// `__split_by_flr` runs as literal fragments.
pub fn fork_platform_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    let pp = platform_paths(st);
    for (x, last) in pp.x.iter().zip(&pp.last) {
        let Some(Value::Num(xv)) = iface::get(st, x) else { bail!("{}: not a number", iface::show(x)) };
        let Some(Value::Num(_)) = iface::get(st, last) else { bail!("{}: not a number", iface::show(last)) };
        iface::set(st, last, Value::Num(xv))?;
        d.platform_inputs.push(xv);
    }
    for p in &pp.rem_x {
        let Some(Value::Num(_)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        let r = d.graph.leaf(Op::Const(PLATFORM_REM.0, PLATFORM_REM.1));
        iface::set(st, p, Value::Num(r))?;
    }
    Ok(())
}

/// The platforms' OUTPUT side at a platforms-unknown level: `x` and `last` the
/// interval of the whole path, `rem.x` the whole remainder (`Rt2::widen_to`
/// step 8b projects the same way). Checked, never assumed: the output `last`
/// must BE the output `x` (the update ends with `last = x`) - with the start
/// state, what makes the input alias sound by induction - and each widened
/// field must provably lie in its range (`within`, through the wrap's point
/// splits), else a runtime premise in `ok` declines the lane loudly.
fn widen_platforms(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    if !d.platforms_unknown {
        return Ok(());
    }
    let pp = platform_paths(st);
    let path = ((PLATFORM_PATH.0 as i64) << 16, (PLATFORM_PATH.1 as i64) << 16);
    for (x, last) in pp.x.iter().zip(&pp.last) {
        let Some(Value::Num(xv)) = iface::get(st, x) else { bail!("{}: not a number", iface::show(x)) };
        let Some(Value::Num(lv)) = iface::get(st, last) else { bail!("{}: not a number", iface::show(last)) };
        anyhow::ensure!(xv == lv, "{}: a platform's `last` is not its `x` at the frame's end", iface::show(last));
        contain(st, d, xv, path);
        let hull = d.graph.leaf(Op::Const(path.0 as i32, path.1 as i32));
        iface::set(st, x, Value::Num(hull))?;
        iface::set(st, last, Value::Num(hull))?;
    }
    let rem = (PLATFORM_REM.0 as i64, PLATFORM_REM.1 as i64);
    for p in &pp.rem_x {
        let Some(Value::Num(v)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        contain(st, d, v, rem);
        let r = d.graph.leaf(Op::Const(PLATFORM_REM.0, PLATFORM_REM.1));
        iface::set(st, p, Value::Num(r))?;
    }
    Ok(())
}

/// `v` in `[lo, hi]` (raw): proved statically (`within`), else a premise in
/// `ok` (like the fly fruit's containment, `widen_fly_fruit`).
fn contain(st: &mut State<Symbolic>, d: &mut Symbolic, v: crate::transpile::graph::NodeId, (lo, hi): (i64, i64)) {
    if within(d, v, (lo, hi), &mut Vec::new()) {
        return;
    }
    let (klo, khi) = (d.graph.leaf(Op::Const(lo as i32, lo as i32)), d.graph.leaf(Op::Const(hi as i32, hi as i32)));
    let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
    let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
    let below = d.graph.fold(Op::Le, vec![vhi, khi]);
    let inside = d.graph.fold(Op::And, vec![above, below]);
    st.ok = d.graph.fold(Op::And, vec![st.ok, inside]);
}

/// Does `v` provably lie in `[lo, hi]` (raw)? Through the arms of a select,
/// and under a point split's branch with what its answer says about the
/// compared value (`Symbolic::point_cmp`): the wrap `x < -16 ? 128 : (x > 128
/// ? -16 : x)` is on the path whatever `x` was. Static; `false` where it
/// cannot tell.
fn within(d: &Symbolic, v: crate::transpile::graph::NodeId, (lo, hi): (i64, i64), facts: &mut Vec<(crate::transpile::graph::NodeId, i64, i64)>) -> bool {
    use super::domain::Cmp;
    let node = d.graph.get(v);
    if let Op::Sel = node.op {
        let (c, t, f) = (node.args[0], node.args[1], node.args[2]);
        if let Some(&(op, x, k)) = d.point_cmp.get(&c) {
            if let Op::Const(kl, kh) = d.graph.get(k).op {
                if kl == kh {
                    let k = kl as i64;
                    // What each answer says about `x`, as a raw range.
                    let (yes, no) = match op {
                        Cmp::Lt => ((i64::MIN, k - 1), (k, i64::MAX)),
                        Cmp::Le => ((i64::MIN, k), (k + 1, i64::MAX)),
                        Cmp::Gt => ((k + 1, i64::MAX), (i64::MIN, k)),
                        Cmp::Ge => ((k, i64::MAX), (i64::MIN, k - 1)),
                        Cmp::Eq => return within(d, t, (lo, hi), facts) && within(d, f, (lo, hi), facts),
                    };
                    facts.push((x, yes.0, yes.1));
                    let a = within(d, t, (lo, hi), facts);
                    facts.pop();
                    facts.push((x, no.0, no.1));
                    let b = within(d, f, (lo, hi), facts);
                    facts.pop();
                    return a && b;
                }
            }
        }
        return within(d, t, (lo, hi), facts) && within(d, f, (lo, hi), facts);
    }
    match bounds(d, v, facts) {
        Some((a, b)) => lo <= a && b <= hi,
        None => false,
    }
}

/// A static raw range of `n`: literals, a platform's input `x` (its path),
/// sums and differences - narrowed by what the enclosing branches know about
/// `n`. `None` for anything else.
fn bounds(d: &Symbolic, n: crate::transpile::graph::NodeId, facts: &[(crate::transpile::graph::NodeId, i64, i64)]) -> Option<(i64, i64)> {
    let node = d.graph.get(n);
    let mut r = match node.op {
        Op::Const(a, b) => (a as i64, b as i64),
        _ if d.platform_inputs.contains(&n) => ((PLATFORM_PATH.0 as i64) << 16, (PLATFORM_PATH.1 as i64) << 16),
        Op::Add => {
            let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
            (a.0 + b.0, a.1 + b.1)
        }
        Op::Sub => {
            let (a, b) = (bounds(d, node.args[0], facts)?, bounds(d, node.args[1], facts)?);
            (a.0 - b.1, a.1 - b.0)
        }
        _ => return None,
    };
    for &(m, flo, fhi) in facts {
        if m == n {
            r = (r.0.max(flo), r.1.min(fhi));
        }
    }
    Some(r)
}

/// The fly fruit's `spd.y` and `rem.y` ranges: ONE definition, shared with the
/// block model's projection (`Rt2::widen_to`), or the mark filter misses.
use celeste_engine::runtime2::{FLY_FRUIT_REM_Y as FRUIT_REM_Y, FLY_FRUIT_SPD_Y as FRUIT_SPD_Y};

/// The fields a fruit-unknown level widens, per live fly fruit
/// (plans/fly-fruit.md).
pub struct FlyFruitPaths {
    /// `step` and `y`: the unknown number.
    pub unknown: Vec<Path>,
    /// `spd.y` and `rem.y`, with their ranges (raw 16.16, inclusive).
    pub ranges: Vec<(Path, (i32, i32))>,
    /// `fly`: an unknown boolean.
    pub fly: Vec<Path>,
}

impl FlyFruitPaths {
    pub fn all(&self) -> impl Iterator<Item = &Path> {
        self.unknown.iter().chain(self.ranges.iter().map(|(p, _)| p)).chain(self.fly.iter())
    }
}

pub fn fly_fruit_paths<D: Domain>(st: &State<D>) -> FlyFruitPaths {
    let mut out = FlyFruitPaths { unknown: Vec::new(), ranges: Vec::new(), fly: Vec::new() };
    for obj in objects_of_type(st, "fly_fruit") {
        out.unknown.push(field(&obj, &["step"]));
        out.unknown.push(field(&obj, &["y"]));
        out.ranges.push((field(&obj, &["spd", "y"]), FRUIT_SPD_Y));
        out.ranges.push((field(&obj, &["rem", "y"]), FRUIT_REM_Y));
        out.fly.push(field(&obj, &["fly"]));
    }
    out
}

/// The fly fruit's INPUT side at a fruit-unknown level (plans/fly-fruit.md):
/// `step` and `y` are the unknown number, `spd.y` and `rem.y` their whole
/// ranges as literals, `fly` an undecided atom - every lane alike, and no
/// input cell read. Every block at such a level holds them so (the output
/// side, `widen_fly_fruit`); a decided input (the post-`_init` start state)
/// lies inside and only over-approximates, so there is no premise.
pub fn fork_fruit_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    let fp = fly_fruit_paths(st);
    for p in &fp.unknown {
        let Some(Value::Num(_)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        let u = d.unknown_num();
        iface::set(st, p, Value::Num(u))?;
    }
    for (p, (lo, hi)) in &fp.ranges {
        let Some(Value::Num(_)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        let r = d.graph.leaf(Op::Const(*lo, *hi));
        iface::set(st, p, Value::Num(r))?;
    }
    for p in &fp.fly {
        let Some(Value::Bool(_)) = iface::get(st, p) else { bail!("{}: not a boolean", iface::show(p)) };
        let b = d.unknown_bool_atom();
        iface::set(st, p, Value::Bool(b))?;
    }
    Ok(())
}

/// The fly fruit's OUTPUT side at a fruit-unknown level: `step` and `y` the
/// unknown number (stored `AV::UNum`, `emit::bind`), `fly` unknown (stored
/// `AV::UBool`), `spd.y` and `rem.y` their ranges - after checking that the
/// frame computed literals inside each (rule 1: a range only replaces what it
/// visibly contains): a literal, or a select whose arms all are (a lane holds
/// one of them). Literal, so the check holds for every row at once, and the
/// range is a fixed point of the frame itself, not an argument about the game.
fn widen_fly_fruit(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    if !d.fruit_unknown {
        return Ok(());
    }
    // Every value a lane can hold: a literal, or a select over such values
    // (whichever arm a lane takes, it holds one of them).
    fn literal_arms(g: &crate::transpile::graph::Graph, n: crate::transpile::graph::NodeId, out: &mut Vec<(i32, i32)>) -> bool {
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
    let fp = fly_fruit_paths(st);
    for (p, (lo, hi)) in &fp.ranges {
        let Some(Value::Num(v)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        let mut arms = Vec::new();
        match literal_arms(&d.graph, v, &mut arms) {
            true if arms.iter().all(|(a, b)| *lo <= *a && *b <= *hi) => {}
            // Not visibly inside: a rung above level 0, where the fruit's rem
            // is exact and `rem.y - 0.5 - amount` no literal. The containment
            // becomes a runtime premise in `ok` (like the region bounds): a
            // lane outside the range declines loudly, none is widened wrongly.
            _ => {
                let (klo, khi) = (d.graph.leaf(Op::Const(*lo, *lo)), d.graph.leaf(Op::Const(*hi, *hi)));
                let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
                let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
                let below = d.graph.fold(Op::Le, vec![vhi, khi]);
                let inside = d.graph.fold(Op::And, vec![above, below]);
                st.ok = d.graph.fold(Op::And, vec![st.ok, inside]);
            }
        }
        let r = d.graph.leaf(Op::Const(*lo, *hi));
        iface::set(st, p, Value::Num(r))?;
    }
    for p in &fp.unknown {
        let u = d.unknown_num();
        iface::set(st, p, Value::Num(u))?;
    }
    for p in &fp.fly {
        let Some(Value::Bool(_)) = iface::get(st, p) else { bail!("{}: not a boolean", iface::show(p)) };
        let b = d.unknown_bool_output();
        iface::set(st, p, Value::Bool(b))?;
    }
    Ok(())
}

/// The rem widening at `precision`, baked into the graph.
///
/// `Exact` is a no-op (the exact-rem set carries no rem intervals).
/// Bits(0) is the historic full-interval widening (one constant bucket,
/// no fork), byte-identical to the level-0 rem step. Bits(k>0): fork
/// `old / 2^-k` at its integer floors (the SAME `__split_by_flr`
/// primitive the `move` code uses - so a straddling lane splits into one
/// per bucket exactly as `split_rem_straddles` does), then snap each
/// fragment to its full bucket `[flr*2^-k, (flr+1)*2^-k)`.
///
/// The scale is a DIVISION by `width = 2^-k` (a representable P8 down to
/// k=16, raw `2^(16-k)`), never a multiply by `2^k` (unrepresentable at
/// k=15: 2^15 is outside the 16.16 range). The containment premise (rem
/// was inside [-0.5, 0.5)) rides on `ok` exactly as at level 0.
fn widen_rem(
    st: &mut State<Symbolic>,
    d: &mut Symbolic,
    precision: crate::interpreter::abstraction::RemPrecision,
) -> Result<()> {
    use crate::interpreter::abstraction::RemPrecision;
    let half = P8::from_parts(0, 0x8000);
    let neg_half = -half;
    let half_below = half.next_smallest();

    let bits = match precision {
        // The exact rung has its OWN kernel set (no rem intervals at
        // all), so it never asks for this; leaving rem untouched is the
        // correct no-op if it ever does.
        RemPrecision::Exact => return Ok(()),
        RemPrecision::Bits(b) => b,
    };

    for obj in objects_of_type(st, "player") {
        for f in ["x", "y"] {
            let p = field(&obj, &["rem", f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: rem is not a number", iface::show(&p));
            };
            // rem was inside [-0.5, 0.5) - the same premise the boundary
            // checks, conjoined into `ok`.
            let lo = d.num(neg_half);
            let hi = d.num(half_below);
            let a = d.compare(super::domain::Cmp::Ge, &old, &lo)?;
            let b = d.compare(super::domain::Cmp::Le, &old, &hi)?;
            let inside = d.and(&a, &b);
            require(st, d, inside);

            if bits == 0 {
                // One bucket covers the whole range: the historic
                // constant, no fork - identical to level 0.
                let wide = ival(d, neg_half, half_below);
                iface::set(st, &p, Value::Num(wide))?;
                continue;
            }

            let rb = rem_bucket_node(d, old, bits)?;
            let ok = st.ok;
            st.ok = d.and(&ok, &rb.premise);
            iface::set(st, &p, Value::Num(rb.value))?;
        }
    }
    Ok(())
}

/// The widened rem for one value, plus the premise the caller conjoins
/// into `ok`.
pub(crate) struct RemBucket {
    /// The widened rem: `Span(bucket_low, bucket_high)`, the
    /// floor-aligned Bits(k) bucket `rem_bucket(., bits)`.
    pub value: <Symbolic as Domain>::Num,
    /// `old` lies within ONE bucket - `Known(Flr(old / width))`. Rides on
    /// `ok`: a lane whose rem straddles a bucket edge here is REAL and
    /// this body cannot represent it, so it is refused, never widened
    /// past its bucket. By construction it never fires: `move` forked at
    /// the bucket grid (`Graph::fork_bits`), so every fragment's new rem
    /// is one bucket shifted by a multiple of the bucket width.
    pub premise: <Symbolic as Domain>::Bool,
    /// The TIGHT value: the fork's fragment (the input clipped to its
    /// bucket), or the input itself when it needed no fork. The speed
    /// stores this and is keyed on `value` (the speed hull).
    pub tight: <Symbolic as Domain>::Num,
    /// `(valid, premise)` when the value forked here (`spd_bucket_node`:
    /// spd has no earlier split to fold into); `None` for rem.
    pub fork: Option<(<Symbolic as Domain>::Bool, <Symbolic as Domain>::Bool)>,
}

/// Snap `old` (assumed in [-0.5, 0.5)) to its floor-aligned Bits(`bits`)
/// bucket - WITHOUT forking. The straddle split happened once already,
/// at `move`'s `__split_by_flr`, on the bucket grid; here a straddling
/// value is a violated premise, not a case. `bits` in 1..=16.
pub(crate) fn rem_bucket_node(
    d: &mut Symbolic,
    old: <Symbolic as Domain>::Num,
    bits: u8,
) -> Result<RemBucket> {
    debug_assert!((1..=16).contains(&bits), "rem_bucket_node bits {} out of 1..=16", bits);
    // width = 2^-k in real units; raw bits = 2^(16-k), always
    // representable for k in 1..=16.
    let width_raw: i32 = 0x1_0000 >> bits;
    let width = d.num(P8::from_raw(width_raw));
    // scaled = old / width = old * 2^k. The DIVIDEND fits (i64
    // intermediate in P8 Div) and the RESULT fits i32 for old in
    // [-0.5, 0.5): |old*2^k| <= 2^(k-1) <= 2^14. Its integer floors are
    // exactly the bucket boundaries.
    let scaled = d.arith(super::domain::Arith::Div, &old, &width)?;
    // bucket index m = flr(scaled) - single-valued iff `old` is inside
    // one bucket, which `Known` asks of it (the emitter reads
    // `Known(Flr(interval))` as "the floors agree").
    let idx = d.fun1(super::domain::Fun1::Flr, &scaled)?;
    let premise = d.graph.fold(Op::Known, vec![idx]);
    // low = m * width; high = low + (width - 1 raw). Snap rem to the
    // constant-width bucket [low, high] = `rem_bucket(., bits)`.
    let low = d.arith(super::domain::Arith::Mul, &idx, &width)?;
    let span_minus_one = d.num(P8::from_raw(width_raw - 1));
    let high = d.arith(super::domain::Arith::Add, &low, &span_minus_one)?;
    let value = d.graph.fold(Op::Span, vec![low, high]);
    Ok(RemBucket { value, premise, fork: None, tight: value })
}

/// Apply every level-0 boundary widening to `st`, in the boundary's order.
/// `player.dash_effect_time` := max(0, it) - `apply_conservative_widenings`'
/// clamp (the field decrements forever and is only read `> 0`, so every
/// value <= 0 is behaviorally identical). Rung-independent.
fn widen_dash(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    let zero = P8::from_i16(0);
    for obj in objects_of_type(st, "player") {
        let p = field(&obj, &["dash_effect_time"]);
        let Some(Value::Num(old)) = iface::get(st, &p) else {
            bail!("{}: dash_effect_time is not a number", iface::show(&p));
        };
        let z = d.num(zero);
        let clamped = d.fun2(super::domain::Fun2::Max, &old, &z)?;
        iface::set(st, &p, Value::Num(clamped))?;
    }
    Ok(())
}

/// A live fruit's `off` := [0, 39] and `y` := start +- 2.5, TOGETHER -
/// one without the other is a row no interpreter level has.
/// `make_state_abstract_rem` applies this at EVERY non-exact rung, so it
/// is rung-independent.
fn widen_fruit(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    let amplitude = P8::from_parts(2, 0x8000);
    for obj in objects_of_type(st, "fruit") {
        let (po, py, ps) = (
            field(&obj, &["off"]),
            field(&obj, &["y"]),
            field(&obj, &["start"]),
        );
        let Some(Value::Num(start)) = iface::get(st, &ps) else {
            bail!("{}: fruit has no numeric `start`", iface::show(&ps));
        };
        // The band's bounds are EXPRESSIONS of `start`, and `start` is
        // an input column in every room that has a fruit - room (2,0)'s
        // is `Sel(Or, Sel, Cell(28))` - so a constant band would be a
        // band around the wrong value, which is a widening that does not
        // contain what it replaces.
        //
        // `Op::Span` is the node for it: the interval from one computed
        // value to another. Built here rather than approximated, so the
        // band is per-lane exactly as `start` is. When `start` DOES fold
        // to a literal (a room whose fruit has not moved), `Span` of two
        // literals folds back to `Op::Const`, so those rooms emit
        // exactly the constant band they did before.
        let Some(Value::Num(old_y)) = iface::get(st, &py) else {
            bail!("{}: fruit `y` is not a number", iface::show(&py));
        };
        let amp = d.num(amplitude);
        let l = d.arith(super::domain::Arith::Sub, &start, &amp)?;
        let h = d.arith(super::domain::Arith::Add, &start, &amp)?;
        // The premise, unchanged in meaning: `y` was inside the band
        // before the widening replaced it. Symbolic bounds are fine -
        // `require` conjoins into `st.ok`, which is the runtime
        // obligation the kernel checks per lane and declines on.
        let a = d.compare(super::domain::Cmp::Ge, &old_y, &l)?;
        let b = d.compare(super::domain::Cmp::Le, &old_y, &h)?;
        let inside = d.and(&a, &b);
        require(st, d, inside);
        let band = d.graph.fold(Op::Span, vec![l, h]);
        iface::set(st, &py, Value::Num(band))?;
        let all = ival(d, P8::from_i16(0), P8::from_i16(39));
        iface::set(st, &po, Value::Num(all))?;
    }
    Ok(())
}

/// The timer globals (`frames`/`seconds`/`minutes`/`deaths`) pinned to
/// zero - `apply_conservative_widenings`' gameplay-dead pins - and with
/// them each key's `frames`-derived `spr` (to its tile, 8) and `flip.x` (to
/// false): the key's update derives both from `frames` and nothing but its
/// own update and drawing reads them, so pinning `frames` alone left exact
/// key-room states with no widened counterpart (room (4,0), 2026-09-16).
/// Rung-independent.
fn widen_timers(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    let zero = P8::from_i16(0);
    for g in ["frames", "seconds", "minutes", "deaths"] {
        let p = vec![iface::key(g)];
        if iface::get(st, &p).is_none() {
            bail!("timer global {} is missing - the pin would silently not apply", g);
        }
        let z = d.num(zero);
        iface::set(st, &p, Value::Num(z))?;
    }
    for obj in objects_of_type(st, "key") {
        let spr = field(&obj, &["spr"]);
        if iface::get(st, &spr).is_none() {
            bail!("{}: a key without `spr` - the pin would silently not apply", iface::show(&spr));
        }
        let tile = d.num(P8::from_i16(8));
        iface::set(st, &spr, Value::Num(tile))?;
        let fx = field(&obj, &["flip", "x"]);
        if iface::get(st, &fx).is_none() {
            bail!("{}: a key without `flip.x` - the pin would silently not apply", iface::show(&fx));
        }
        let f = d.boolean(false);
        iface::set(st, &fx, Value::Bool(f))?;
    }
    Ok(())
}

/// The spd widening at `precision`, baked into the graph - the analogue
/// of `split_spd_straddles` + `make_state_abstract_spd`. `Exact` is a
/// no-op (today's default and level 0). `WidthLog2(w)`: fork
/// `spd / 2^w_raw` at its integer floors and snap each fragment to its
/// floor-aligned width-`2^w` bucket, exactly the rem shape at a raw-unit
/// width. `player.spd.x/y` is not range-premised the way rem is (its
/// buckets tile the whole `+/-16 px/frame` range by construction), so
/// there is no containment conjunct.
fn widen_spd(
    st: &mut State<Symbolic>,
    d: &mut Symbolic,
    precision: crate::interpreter::abstraction::SpdPrecision,
) -> Result<()> {
    let Some(w) = precision.width_log2() else {
        return Ok(());
    };
    // `WidthLog2X`: only spd.x is bucketed; spd.y stays exact.
    let axes: &[&str] = if precision.buckets_y() { &["x", "y"] } else { &["x"] };
    for obj in objects_of_type(st, "player") {
        for &f in axes {
            let p = field(&obj, &["spd", f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: spd is not a number", iface::show(&p));
            };
            // THE BUCKET DISPATCH (2026-09-15, probe): a body specialized on
            // its input speed bucket knows the static range of its output
            // speed, so the snap forks at exactly the bucket edges that
            // range crosses (`fork_table`), the row stores the fragment and
            // is keyed on its bucket - a per-configuration constant.
            if let Some(range) = d.range_of(old) {
                let ax = if f == "x" { 0 } else { 1 };
                let sb = spd_table_node(d, old, celeste_core::spd_buckets::edges(w, ax), &range)?;
                if let Some((valid, premise)) = sb.fork {
                    st.guard = d.and(&st.guard, &valid);
                    let ok = st.ok;
                    st.ok = d.and(&ok, &premise);
                }
                iface::set(st, &p, Value::Num(sb.tight))?;
                st.key_override.push((p.clone(), sb.value));
                continue;
            }
            // A trace specialized on its input bucket must know its output
            // range - a grid snap here would key on a grid bucket, not the
            // table's. Only the unspecialized base trace (the union
            // templates) snaps to the grid.
            anyhow::ensure!(
                d.ranges.is_empty(),
                "{}: no static range for the output speed of a bucket-specialized trace",
                iface::show(&p)
            );
            let sb = spd_bucket_node(d, old, w)?;
            if let Some((valid, premise)) = sb.fork {
                st.guard = d.and(&st.guard, &valid);
                let ok = st.ok;
                st.ok = d.and(&ok, &premise);
            }
            let ok = st.ok;
            st.ok = d.and(&ok, &sb.premise);
            // THE SPEED HULL (2026-09-15): the row stores the tight
            // fragment and is keyed on the bucket, so states that differ
            // only within a bucket are one state whose interval is only as
            // wide as the speeds actually merged into it (the door keeps
            // the union, `door::Hull`), instead of the full bucket - which
            // fanned out at every threshold the bucket straddled.
            iface::set(st, &p, Value::Num(sb.tight))?;
            st.key_override.push((p.clone(), sb.value));
        }
    }
    Ok(())
}

/// The position widening at `pos` (the OUTPUT side): the player's whole
/// pixel `x`/`y` snapped to its floor-aligned bucket of `w` pixels,
/// `Span(low, low + w - 1)` with `low = w * flr(x / w)`. The analogue of
/// `make_state_abstract_pos`. No fork and no premise: the frame ran an
/// exact integer position (`fork_pos_inputs`), so `x` is a number.
fn widen_pos(
    st: &mut State<Symbolic>,
    d: &mut Symbolic,
    pos: crate::interpreter::abstraction::PosPrecision,
) -> Result<()> {
    if pos.is_exact() {
        return Ok(());
    }
    for obj in objects_of_type(st, "player") {
        for (f, w) in [("x", pos.x), ("y", pos.y)] {
            if w <= 1 {
                continue;
            }
            let p = field(&obj, &[f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: position is not a number", iface::show(&p));
            };
            anyhow::ensure!(!d.is_interval(&old), "{}: position is an interval at the output", iface::show(&p));
            let width = d.num(P8::from_i16(w as i16));
            let scaled = d.arith(super::domain::Arith::Div, &old, &width)?;
            let idx = d.fun1(super::domain::Fun1::Flr, &scaled)?;
            let low = d.arith(super::domain::Arith::Mul, &idx, &width)?;
            let span_minus_one = d.num(P8::from_i16(w as i16 - 1));
            let high = d.arith(super::domain::Arith::Add, &low, &span_minus_one)?;
            let value = d.graph.fold(Op::Span, vec![low, high]);
            iface::set(st, &p, Value::Num(value))?;
        }
    }
    Ok(())
}

/// The position widening's INPUT side: a player `x`/`y` that arrives as a
/// bucket interval is forked into its whole-pixel points (`fork_int`:
/// `Op::SplitInt`, one exact position per configuration), with the fork's
/// validity in the guard and its span premise (at most `w` pixels) in
/// `ok`. Generic over the domain: the reference engine forks the same way
/// through its cursor (`refdriver::run_frame_all`). A position that is
/// already a number is left alone.
pub fn fork_pos_inputs<D: Domain>(
    st: &mut State<D>,
    d: &mut D,
    pos: crate::interpreter::abstraction::PosPrecision,
) -> Result<()> {
    if pos.is_exact() {
        return Ok(());
    }
    for obj in objects_of_type(st, "player") {
        for (f, w) in [("x", pos.x), ("y", pos.y)] {
            if w <= 1 {
                continue;
            }
            let p = field(&obj, &[f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: position is not a number", iface::show(&p));
            };
            if !d.is_interval(&old) {
                if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
                    eprintln!("[build] fork_pos_inputs: {} is not an interval, no fork", iface::show(&p));
                }
                continue;
            }
            if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
                eprintln!("[build] fork_pos_inputs: forking {} {w}-way", iface::show(&p));
            }
            let (v, valid) = d.fork_int(&old, w);
            let premise = d.span_ok(&old, w);
            st.guard = d.and(&st.guard, &valid);
            st.ok = d.and(&st.ok, &premise);
            iface::set(st, &p, Value::Num(v))?;
        }
    }
    Ok(())
}

/// The held-button trails' INPUT side at a held-unknown level
/// (plans/held-buttons.md): a block holds the player's `p_jump` / `p_dash`
/// unknown, so each is replaced by a 2-way fork (`Op::SplitInt` over the
/// constant `[0, 1]`: the configuration's value exactly), and the frame reads
/// no input cell for them. Every configuration is valid for every lane - a
/// lane whose trail is unknown can be either twin, and a decided one only
/// over-approximates - so there is no validity or span premise. Per
/// configuration `jump` / `dash` are decided and the two arms are two bodies.
/// One fork per trail, never memoized: the two trails are independent.
pub fn fork_held_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for obj in objects_of_type(st, "player") {
        for f in ["p_jump", "p_dash"] {
            let p = field(&obj, &[f]);
            let Some(Value::Bool(_)) = iface::get(st, &p) else {
                bail!("{}: not a boolean", iface::show(&p));
            };
            let held = d.both_values(f);
            iface::set(st, &p, Value::Bool(held))?;
        }
    }
    Ok(())
}

/// Snap `old` (a raw 16.16 speed) to its floor-aligned width-`2^w` bucket,
/// forking at bucket edges. `w` in 8..=20 (the `SpdPrecision` range), so
/// `width = 2^w` raw is a representable P8 (2^20 = 16.0 < 32768) and the
/// scale is a DIVISION by it. Same construction as `rem_bucket_node`, only
/// the bucket width lives in raw units rather than a fraction.
pub(crate) fn spd_bucket_node(
    d: &mut Symbolic,
    old: <Symbolic as Domain>::Num,
    w: u8,
) -> Result<RemBucket> {
    anyhow::ensure!(
        (crate::interpreter::abstraction::SPD_MIN_WIDTH_LOG2..=20).contains(&w),
        "spd_bucket_node w {w}: spd / 2^w raw overflows 16.16 below w = {}",
        crate::interpreter::abstraction::SPD_MIN_WIDTH_LOG2
    );
    let width_raw: i32 = 1i32 << w;
    let width = d.num(P8::from_raw(width_raw));
    // The fork is on the RAW value: every fork in the graph cuts at the
    // grid (`Graph::fork_bits`, 2^-k), and the spd bucket at rung k is at
    // least one grid cell (`Level::grid_consistent`: width 2^w raw, w >=
    // 16-k), so a fragment lies within one bucket. (Forking the SCALED value cut it
    // on a 2^-k grid in bucket units - three cells for a one-bucket span
    // - and every rung > 0 declined its lanes, 2026-09-14.)
    let fb = d.graph.fork_bits();
    anyhow::ensure!(
        fb == 0 || w as u32 == 16 - fb as u32,
        "spd bucket width 2^{w} raw does not match the fork grid 2^-{fb}"
    );
    let (frag, fork) = if d.is_interval(&old) {
        // Two-way: the boundary snap sees the frame's OUTPUT speed, at
        // most one grid cell wide plus the frame's shifts (`appr`'s
        // 0.6, gravity's 0.21) - two cells.
        let (frag, valid) = d.fork_flr(&old, 2);
        let premise = d.span_ok(&old, 2);
        (frag, Some((valid, premise)))
    } else {
        (old, None)
    };
    let scaled = d.arith(super::domain::Arith::Div, &frag, &width)?;
    let idx = d.fun1(super::domain::Fun1::Flr, &scaled)?;
    let premise = d.graph.fold(Op::Known, vec![idx]);
    let low = d.arith(super::domain::Arith::Mul, &idx, &width)?;
    let span_minus_one = d.num(P8::from_raw(width_raw - 1));
    let high = d.arith(super::domain::Arith::Add, &low, &span_minus_one)?;
    let value = d.graph.fold(Op::Span, vec![low, high]);
    Ok(RemBucket { value, premise, fork, tight: frag })
}

/// The snap of `old`, statically within `range`, to the buckets of
/// `edges` (sorted raw edges; bucket `j` is `[edges[j], edges[j+1] - 1]`,
/// the ends open): the buckets the range crosses become a table fork
/// (`Op::SplitTab`), one bucket is no fork at all. `value` is the bucket
/// (the key), `tight` the fragment, `premise` the runtime check that the
/// range held.
fn spd_table_node(
    d: &mut Symbolic,
    old: <Symbolic as Domain>::Num,
    edges: &[i32],
    pieces: &[(i64, i64)],
) -> Result<RemBucket> {
    let range = (pieces[0].0, pieces[pieces.len() - 1].1);
    use crate::transpile::graph::Op;
    let bucket = |j: usize| -> (i32, i32) {
        let lo = if j == 0 { i32::MIN } else { edges[j - 1] };
        let hi = if j == edges.len() { i32::MAX } else { edges[j] - 1 };
        (lo, hi)
    };
    let crossed: Vec<(i32, i32)> = (0..=edges.len())
        .map(bucket)
        .filter(|(lo, hi)| pieces.iter().any(|p| (*lo as i64) <= p.1 && (*hi as i64) >= p.0))
        .collect();
    anyhow::ensure!(!crossed.is_empty(), "speed range {range:?} crosses no bucket");
    anyhow::ensure!(
        crossed.len() <= crate::transpile::graph::MAX_WAYS,
        "speed range [{}, {}] crosses {} buckets, more than a fork holds ({})",
        range.0 as f64 / 65536.0, range.1 as f64 / 65536.0, crossed.len(), crate::transpile::graph::MAX_WAYS
    );
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build]   spd_table_node: range [{:.4}, {:.4}] in {} pieces crosses {} buckets: {:?}", range.0 as f64 / 65536.0, range.1 as f64 / 65536.0, pieces.len(), crossed.len(),
            pieces.iter().map(|(a, b)| format!("[{:.3},{:.3}]", *a as f64 / 65536.0, *b as f64 / 65536.0)).collect::<Vec<_>>());
    }
    // The premise: the static range holds. On the graph directly, so
    // the range analysis cannot fold its own obligation away.
    let (rlo, rhi) = (d.graph.leaf(Op::Const(range.0 as i32, range.0 as i32)), d.graph.leaf(Op::Const(range.1 as i32, range.1 as i32)));
    let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![old]), d.graph.fold(Op::Hi, vec![old]));
    let a = d.graph.fold(Op::Ge, vec![vlo, rlo]);
    let b = d.graph.fold(Op::Le, vec![vhi, rhi]);
    let premise = d.graph.fold(Op::And, vec![a, b]);
    if crossed.len() == 1 {
        let (lo, hi) = crossed[0];
        let value = d.graph.leaf(Op::Const(lo, hi));
        return Ok(RemBucket { value, premise, fork: None, tight: old });
    }
    // The RELATIVE fork: its arity is the most buckets one lane can cross
    // (a lane lies within one piece), not every bucket the range reaches;
    // a button rep's own pieces refine it (`lower::specialize_frame`).
    let arity = pieces
        .iter()
        .map(|p| crossed.iter().filter(|(lo, hi)| (*lo as i64) <= p.1 && (*hi as i64) >= p.0).count())
        .max()
        .unwrap_or(1);
    let (frag, valid) = d.fork_table(&old, &crossed, arity as u8);
    let fork = d.forks - 1;
    // The row's bucket, per lane, and the premise that the fragments
    // cover the lane.
    let value = d.graph.fold(Op::SplitKeyTab(fork), vec![old]);
    let covered = d.graph.fold(Op::SplitOkTab(fork), vec![old]);
    let premise = d.graph.fold(Op::And, vec![premise, covered]);
    Ok(RemBucket { value, premise: d.boolean(true), fork: Some((valid, premise)), tight: frag })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transpile::graph::{Op, Val};
    use std::collections::HashMap;

    /// The reference floor-aligned Bits(`bits`) bucket, in raw units -
    /// `abstraction::rem_bucket` re-derived here (it is private) so the
    /// gate depends on the definition, not on an import.
    fn ref_bucket(v_raw: i32, bits: u8) -> (i32, i32) {
        let width = 0x1_0000i32 >> bits;
        let low = v_raw.div_euclid(width) * width;
        (low, low + width - 1)
    }

    fn raw(iv: &celeste_core::pico8_num::Pico8NumInterval) -> (i32, i32) {
        (iv.low.as_raw_u32() as i32, iv.high.as_raw_u32() as i32)
    }

    /// `rem_bucket_node`, on an EXACT rem value (no fork), snaps every
    /// representable rem in [-0.5, 0.5) to exactly `rem_bucket`, at every
    /// rung. This is the "widened the same" half of the A-vs-B gate at
    /// the value level: the graph's snap == `make_state_abstract_rem`'s.
    #[test]
    fn rem_bucket_node_matches_rem_bucket_exact() {
        for bits in 1..=15u8 {
            let mut d = Symbolic::default();
            let old = d.graph.leaf(Op::Cell(0));
            // No ival mark: an exact input, so no fork.
            let rb = rem_bucket_node(&mut d, old, bits).expect("rem_bucket_node");
            assert!(rb.fork.is_none(), "exact input must not fork (bits {})", bits);
            // Sweep the whole rem range; step is coprime-ish to bucket
            // widths so every bucket and both signs are hit.
            for v_raw in (-32768..32768).step_by(97) {
                let v = P8::from_raw(v_raw);
                let cells = HashMap::from([(0u32, Val::exact_num(v))]);
                let out = d.graph.eval(&cells).expect("eval");
                let got = match out[rb.value as usize] {
                    Val::Num(iv) => iv,
                    other => panic!("rem is not a number: {:?}", other),
                };
                assert_eq!(
                    raw(&got),
                    ref_bucket(v_raw, bits),
                    "bits {} value raw {}",
                    bits,
                    v_raw
                );
            }
        }
    }

    /// The snap is a FIXED POINT: re-snapping a value already at a bucket
    /// edge (the low end, which is what `make_state_abstract_rem` would
    /// re-read) lands on the same bucket. The assert-noop property at the
    /// value level - catches UNDER-widening (a bucket that a second
    /// widening would move).
    #[test]
    fn rem_bucket_node_is_idempotent() {
        for bits in 1..=15u8 {
            let mut d = Symbolic::default();
            let old = d.graph.leaf(Op::Cell(0));
            let rb = rem_bucket_node(&mut d, old, bits).expect("rem_bucket_node");
            for v_raw in (-32768..32768).step_by(97) {
                let (lo, hi) = ref_bucket(v_raw, bits);
                // Snapping the low edge and the high edge both stay put.
                for edge in [lo, hi] {
                    let cells = HashMap::from([(0u32, Val::exact_num(P8::from_raw(edge)))]);
                    let out = d.graph.eval(&cells).expect("eval");
                    let got = match out[rb.value as usize] {
                        Val::Num(iv) => iv,
                        other => panic!("rem is not a number: {:?}", other),
                    };
                    assert_eq!(raw(&got), (lo, hi), "bits {} edge {}", bits, edge);
                }
            }
        }
    }

    /// `spd_bucket_node`, on an EXACT spd value (no fork), snaps to the
    /// floor-aligned width-`2^w` bucket - `make_state_abstract_spd`'s
    /// bucket, in raw units. Swept across a wide speed range at every
    /// rung width.
    #[test]
    fn spd_bucket_node_matches_make_state_abstract_spd() {
        // Reference: floor-aligned width-2^w bucket on the raw value.
        let ref_bucket = |v_raw: i32, w: u8| -> (i32, i32) {
            let width = 1i32 << w;
            let low = v_raw.div_euclid(width) * width;
            (low, low + width - 1)
        };
        for w in 8..=20u8 {
            let mut d = Symbolic::default();
            let old = d.graph.leaf(Op::Cell(0));
            let sb = spd_bucket_node(&mut d, old, w).expect("spd_bucket_node");
            assert!(sb.fork.is_none(), "exact spd must not fork (w {})", w);
            // +/-16 px/frame is +/-2^20 raw; sweep it.
            for v_raw in (-(1 << 20)..(1 << 20)).step_by(9973) {
                let v = P8::from_raw(v_raw);
                let cells = HashMap::from([(0u32, Val::exact_num(v))]);
                let out = d.graph.eval(&cells).expect("eval");
                let got = match out[sb.value as usize] {
                    Val::Num(iv) => iv,
                    other => panic!("spd is not a number: {:?}", other),
                };
                assert_eq!(raw(&got), ref_bucket(v_raw, w), "w {} value raw {}", w, v_raw);
            }
        }
    }

    /// The boundary snap does NOT fork (the straddle split happened once,
    /// at `move`, on the bucket grid): an interval inside one bucket snaps
    /// to that bucket, and the one-bucket premise is what a straddling
    /// value would violate.
    #[test]
    fn rem_bucket_node_snaps_without_forking() {
        // bits=2: buckets are 0x4000 wide.
        let bits = 2u8;
        let mut d = Symbolic::default();
        d.ival_cells.insert(0);
        let old = d.graph.leaf(Op::Cell(0));
        let rb = rem_bucket_node(&mut d, old, bits).expect("rem_bucket_node");
        assert!(rb.fork.is_none(), "the boundary snap must not fork");
        assert_eq!(d.forks, 0, "no fork registered");

        // Inside the bucket [0, 0x3fff]: snaps to it.
        let inside = celeste_core::pico8_num::Pico8NumInterval::new(P8::from_raw(100), P8::from_raw(0x3000));
        let cells = HashMap::from([(0u32, Val::Num(inside))]);
        let vals = d.graph.eval_narrow_top(&cells).expect("eval");
        let got = match vals[rb.value as usize] {
            Val::Num(iv) => iv,
            other => panic!("rem is not a number: {:?}", other),
        };
        assert_eq!(raw(&got), (0, 0x3fff), "snapped to its bucket");
    }
}
