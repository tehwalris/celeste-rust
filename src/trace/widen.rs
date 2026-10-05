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
//! So each one becomes the widening's own error on the slot it writes
//! (`SlotErrors`) - a lane that violates it is refused rather than quietly
//! accepted, which under the never-deopt doctrine stops the run and names
//! itself.

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
pub const ABSENT_AS_ZERO: &[(&str, &str)] = &[("fall_floor", "delay"), ("spring", "delay")];

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

/// A constant interval `[lo, hi]` as one graph node. `Op::Const(lo, hi)`
/// with `lo != hi` IS the interval literal - the same node the emitter
/// renders as an `IV`.
fn ival(d: &mut Symbolic, lo: P8, hi: P8) -> <Symbolic as Domain>::Num {
    d.graph.leaf(Op::Const(lo.as_raw_u32() as i32, hi.as_raw_u32() as i32))
}

/// What the output widenings OWE: per widened slot, the widening's own
/// error - where the value it replaced was not inside what it wrote (a rem
/// outside `[-0.5, 0.5)`, a fruit outside its band, a platform off its
/// path). A widening is an operator like any other, and this is its partial
/// part; everything the new value is computed FROM derives its own error
/// (`trace::error`). The slot's row stores the widened value, so a lane
/// where one of these holds declines (`verify::trace_frame`).
pub type SlotErrors = Vec<(Path, <Symbolic as Domain>::Bool)>;

/// The widening of `p` is defined only where `holds`.
fn owe(errs: &mut SlotErrors, d: &mut Symbolic, p: &Path, holds: <Symbolic as Domain>::Bool) {
    let e = d.not(&holds);
    errs.push((p.clone(), e));
}



/// A state BETWEEN the two steps of a split frame (`CELESTE_SPLIT_FRAME`,
/// lua/celeste-minimal-split.lua): `__phase` holds a table there, and at every
/// frame boundary it is absent (`heap::Table::set_global`). The unsplit cart
/// never assigns it.
pub fn mid_frame<D: Domain>(st: &State<D>) -> bool {
    st.heap.tables[&st.globals].hash.contains_key("__phase")
}

/// Apply the boundary widenings to `st`: the player's remainder to
/// [-1/2, 1/2) (the arcs track it exactly beside the row), `dash_effect_time`
/// clamped, the timer globals pinned, and the objects as the level's flags
/// say (each widening below is a no-op where its flag is off).
pub fn widen(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<SlotErrors> {
    let mut errs = SlotErrors::new();
    widen_rem(st, d, &mut errs)?;
    widen_dash(st, d)?;
    widen_fruit(st, d, &mut errs)?;
    widen_timers(st, d)?;
    widen_fly_fruit(st, d, &mut errs)?;
    widen_floor_timers(st, d)?;
    widen_near_floors(st, d, &mut errs, mid_frame(st))?;
    widen_platforms(st, d, &mut errs)?;
    canon_balloon_offset(st, d, &mut errs)?;
    widen_held(st, d)?;
    Ok(errs)
}

/// The player's remainder := [-1/2, 1/2), the whole circle: the row forgets
/// it, and the edge's transfer (`search::arc_edges`) carries what the frame
/// did to it. That the remainder was inside the circle is the widening's own
/// error.
fn widen_rem(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
    let half = P8::from_parts(0, 0x8000);
    let neg_half = -half;
    let half_below = half.next_smallest();
    for obj in objects_of_type(st, "player") {
        for f in ["x", "y"] {
            let p = field(&obj, &["rem", f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: rem is not a number", iface::show(&p));
            };
            let lo = d.num(neg_half);
            let hi = d.num(half_below);
            let a = d.compare(super::domain::Cmp::Ge, &old, &lo)?;
            let b = d.compare(super::domain::Cmp::Le, &old, &hi)?;
            let inside = d.and(&a, &b);
            owe(errs, d, &p, inside);
            let wide = ival(d, neg_half, half_below);
            iface::set(st, &p, Value::Num(wide))?;
        }
    }
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
/// (`fall_floor_paths`) actually merge pop histories. The claim that the
/// interval is a full period is the widening's own error: a narrower phase (a
/// concrete draw) declines loudly, never widened. The block model projects the same way
/// (`Rt2::widen_to`), for the concrete search's node lookup.
fn canon_balloon_offset(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
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
        owe(errs, d, &p, full);
        let canon = d.graph.leaf(Op::Const(0, BALLOON_PERIOD_RAW));
        iface::set(st, &p, Value::Num(canon))?;
    }
    Ok(())
}

use celeste_engine::runtime2::{floor_player_window, BALLOON_PERIOD_RAW, FLOOR_HITBOX, FLOOR_STATE_RANGE, SPRING_SPR_RANGE, BALLOON_SPR_RANGE, BALLOON_BOB_RAW, PLATFORM_PATH, PLATFORM_REM, PLAYER_HITBOX};

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

/// The object PHASES a near level widens (2026-10-01, rooms (7,0)/(0,1)),
/// each with the range it stores it as: the spring's
/// `spr` (0 hidden, 18 ready, 19 compressed), its compressed countdown `delay`
/// and hide countdowns `hide_in`/`hide_for`, and the balloon's `spr` (0
/// popped, 22 present). The spring's update is then "maybe bounce the
/// player", the balloon's "maybe refill the dash where the player overlaps
/// its bob", and nothing else; the floor under a spring is widened like any
/// other. (The balloon's respawn `timer` is a countdown with the floors'.)
///
/// The balloon's `y` too (2026-10-01, room (2,1)): its bob `start +
/// sin(offset) * 2` runs only on the `spr == 22` arm, so with `spr` widened a
/// stored `y = start` (the other arm) had two successors, `start` and the
/// bob band, and every state existed twice. Its range is the band itself,
/// `start +- BALLOON_BOB_RAW` (`PhaseRange::AroundStart`).
///
/// The spring's three COUNTDOWNS are the unknown number at a near level too
/// (`PhaseRange::Countdown`), as the floors' are (`widen_floor_timers`).
pub fn phase_paths<D: Domain>(st: &State<D>) -> Vec<(Path, PhaseRange)> {
    let mut out = Vec::new();
    for obj in objects_of_type(st, "spring") {
        out.push((field(&obj, &["spr"]), PhaseRange::Fixed(SPRING_SPR_RANGE)));
        for f in ["delay", "hide_in", "hide_for"] {
            out.push((field(&obj, &[f]), PhaseRange::Countdown));
        }
    }
    for obj in objects_of_type(st, "balloon") {
        out.push((field(&obj, &["spr"]), PhaseRange::Fixed(BALLOON_SPR_RANGE)));
        out.push((field(&obj, &["y"]), PhaseRange::AroundStart(field(&obj, &["start"]), BALLOON_BOB_RAW)));
    }
    out
}

/// Where a near level stores a phase (`phase_paths`): a fixed range, the
/// object's constant `start` plus or minus a radius (raw 16.16), or - a
/// countdown - the unknown number.
pub enum PhaseRange {
    Fixed((i32, i32)),
    AroundStart(Path, i32),
    Countdown,
}

/// The countdowns a near level widens: every fall floor's `delay` and the
/// balloon's respawn `timer`.
pub fn floor_timer_paths<D: Domain>(st: &State<D>) -> Vec<Path> {
    let mut out = Vec::new();
    for obj in objects_of_type(st, "fall_floor") {
        let p = field(&obj, &["delay"]);
        if iface::get(st, &p).is_some() {
            out.push(p);
        }
    }
    for obj in objects_of_type(st, "balloon") {
        out.push(field(&obj, &["timer"]));
    }
    out
}

/// The countdowns' OUTPUT side at a near level (2026-09-30, room (7,0)): each
/// stored as THE UNKNOWN NUMBER (`Symbolic::unknown_num`, stored `AV::UNum`,
/// `emit::bind`) - the cart only decrements them and compares them with 0, so
/// no range would be more precise. A broken floor only forgets how far into
/// its phase it is: next frame its `delay <= 0` is an undecided atom, "still
/// counting" or "done". The unknown contains whatever the frame computed, so
/// there is no premise. Room (7,0) level 0 at f54: erasing the delays merged
/// 3.1x, all floor fields 4.6x.
///
/// NOT the interval [MIN, MAX] (what it was until 2026-10-03): the cart's
/// `delay - 1` of the whole range overflows, the assembled kernels wrapped the
/// low end to +32767.99, and `delay <= 0` decided "no" - a shaking floor
/// never fell. Interval arithmetic that overflows is now a lane's error
/// (`Op::NoWrap`), so a countdown of the whole range would decline every
/// lane; the unknown number stays unknown under every operation instead.
fn widen_floor_timers(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    if !d.floors_near {
        return Ok(());
    }
    for p in floor_timer_paths(st) {
        let Some(Value::Num(_)) = iface::get(st, &p) else { bail!("{}: not a number", iface::show(&p)) };
        let u = d.unknown_num();
        iface::set(st, &p, Value::Num(u))?;
    }
    Ok(())
}

/// The countdowns a level stores as the unknown number
/// (`widen_floor_timers`, and a near level's `PhaseRange::Countdown` phases):
/// what the next frame reads them as.
pub fn countdown_paths(st: &State<Symbolic>, d: &Symbolic) -> Vec<Path> {
    let mut out = Vec::new();
    if d.floors_near {
        out.extend(floor_timer_paths(st));
        out.extend(phase_paths(st).into_iter().filter(|(_, r)| matches!(r, PhaseRange::Countdown)).map(|(p, _)| p));
    }
    out
}

/// The INPUT side of the countdowns (`countdown_paths`): the unknown number
/// in place of whatever the slot holds, before anything reads it - as a
/// floors-unknown level replaces its floors (`fork_floor_inputs`). A slot
/// the state does not have (a spring that never bounced: no `delay`) stays
/// absent.
pub fn forget_countdown_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for p in countdown_paths(st, d) {
        match iface::get(st, &p) {
            None => {}
            Some(Value::Num(_)) => {
                let u = d.unknown_num();
                iface::set(st, &p, Value::Num(u))?;
            }
            Some(_) => bail!("{}: not a number", iface::show(&p)),
        }
    }
    Ok(())
}

/// The fields a near level widens but where the player overlaps the floor
/// (`Level::floors_near`), per fall floor; its countdowns are
/// `floor_timer_paths`, the objects' phases `phase_paths`.
pub struct NearFloorPaths {
    pub floors: Vec<Path>,
    /// `state`: the interval `FLOOR_STATE_RANGE`, or exact.
    pub state: Vec<Path>,
    /// `collideable`: unknown, or exact.
    pub collideable: Vec<Path>,
}

impl NearFloorPaths {
    pub fn all(&self) -> impl Iterator<Item = &Path> {
        self.state.iter().chain(self.collideable.iter())
    }
}

pub fn near_floor_paths(st: &State<Symbolic>) -> NearFloorPaths {
    let mut out = NearFloorPaths { floors: Vec::new(), state: Vec::new(), collideable: Vec::new() };
    for obj in objects_of_type(st, "fall_floor") {
        out.state.push(field(&obj, &["state"]));
        out.collideable.push(field(&obj, &["collideable"]));
        out.floors.push(obj);
    }
    out
}

/// The objects' phases at a near level (`phase_paths`), as INTERVALS, so
/// `spr == 18` splits like a floor's `state == k` - but the countdowns, which
/// the cart only decrements and compares with 0, as the unknown number
/// (`PhaseRange::Countdown`, `widen_floor_timers`). The output side; the next
/// frame reads the stored intervals as interval inputs and the countdowns as
/// unknown (`forget_countdown_inputs`).
fn widen_near_phases(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
    for (p, range) in phase_paths(st) {
        let (lo, hi) = match range {
            PhaseRange::Countdown => {
                let Some(Value::Num(_)) = iface::get(st, &p) else { bail!("{}: not a number", iface::show(&p)) };
                let u = d.unknown_num();
                iface::set(st, &p, Value::Num(u))?;
                continue;
            }
            PhaseRange::Fixed(r) => r,
            PhaseRange::AroundStart(sp, radius) => {
                let Some(Value::Num(s)) = iface::get(st, &sp) else { bail!("{}: not a number", iface::show(&sp)) };
                let Some(s) = d.as_const(&s) else { bail!("{}: not a constant", iface::show(&sp)) };
                (s.to_bits() as i32 - radius, s.to_bits() as i32 + radius)
            }
        };
        let Some(Value::Num(v)) = iface::get(st, &p) else { bail!("{}: not a number", iface::show(&p)) };
        if let Some(holds) = contain(d, v, (lo as i64, hi as i64)) {
            owe(errs, d, &p, holds);
        }
        let r = d.graph.leaf(Op::Const(lo, hi));
        iface::set(st, &p, Value::Num(r))?;
    }
    Ok(())
}

/// A near level's INPUT side (`Level::floors_near`): a floor's
/// `state` arrives as an interval input (`FLOOR_STATE_RANGE`, or `[n, n]`) and
/// is read as it is - its update's `state == k` is undecided on a widened lane
/// and split (`verify::split_undecided_selects`, the equal side narrowed to
/// `k`). Its `collideable` is not read from the row but DERIVED: `state ~= 2`.
/// The cart keeps the two in step in every state - `init` sets state 0 with
/// `collideable` true, and the only writes are 1 -> 2 with `false` and 2 -> 0
/// with `true` - so an independent unknown `collideable` stood for idle or
/// shaking floors the player passes through, which the game never has
/// (2026-10-01: 36% of the level's states at step 110 were players inside
/// floors). The split of `state == 2` makes it a decided boolean per
/// configuration for every read.
pub fn fork_near_floor_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    use super::domain::Cmp;
    materialize_absent_fields(st, d)?;
    let fp = near_floor_paths(st);
    let two = d.num(P8::from_i16(2));
    for (ps, pc) in fp.state.iter().zip(&fp.collideable) {
        let Some(Value::Num(state)) = iface::get(st, ps) else { bail!("{}: not a number", iface::show(ps)) };
        let hidden = d.compare(Cmp::Eq, &state, &two)?;
        let solid = d.not(&hidden);
        iface::set(st, pc, Value::Bool(solid))?;
    }
    Ok(())
}

/// A near level's floors IN THE MIDDLE OF A SPLIT FRAME: each `collideable`
/// is the value the frame's first step stored (`widen_near_floors`) - computed
/// where the player's half may read it, unknown elsewhere - so a lane may hold
/// it unknown (`AV::UBool`), and the tracer binds a boolean slot as a plain,
/// per-lane decided cell (`iface::symbolize`). Read as it is, an unknown lane
/// took its value bit - false: the floor under the player read as absent, and
/// room (6,1)'s player never stood on its spawn floors once the first step
/// stored them unknown (2026-10-03; before, a split the near owes happened to
/// cause decided them). So: the cell where the lane knows it, else a fork of
/// both values (`Symbolic::both_values`, one read agreeing with every other).
pub fn fork_unknown_near_collideables(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for pc in near_floor_paths(st).collideable {
        let Some(Value::Bool(c)) = iface::get(st, &pc) else { bail!("{}: not a boolean", iface::show(&pc)) };
        let known = d.graph.fold(Op::Known, vec![c]);
        let both = d.both_values(&format!("{} unknown", iface::show(&pc)));
        let v = d.sel_bool(&known, &c, &both);
        iface::set(st, &pc, Value::Bool(v))?;
    }
    Ok(())
}

/// How far the player's own update reaches for a fall floor, as offsets to
/// the overlap window `floor_player_window` (`(x lo, x hi), (y lo, y hi)`,
/// added to the OPEN window's ends): `is_solid(ox, oy)` checks the floors at
/// `ox` in -3..=3 and `oy` in 0..=1 (lua/celeste-minimal.lua, `player.update`:
/// `is_solid(0,1)`, `is_solid(input,0)`, `is_solid(-3,0)`/`is_solid(3,0)`), and
/// `check` at `(ox, oy)` overlaps where the player at `(x + ox, y + oy)` would.
pub const PLAYER_PROBE: [(i16, i16); 2] = [(-3, 3), (-1, 0)];

/// A near level's OUTPUT side (`Level::floors_near`,
/// 2026-09-30, room (7,0)): every fall floor stores its `state`
/// as the interval `FLOOR_STATE_RANGE` and its `collideable` unknown - EXCEPT
/// where a player overlaps it (the cart's `floor.collide(player, 0, 0)`,
/// `runtime2::floor_player_window`, on this outcome's positions), where both
/// stay what the frame computed. The countdowns are `widen_floor_timers`'s,
/// overlap or not: the player cannot enter a floor it collides with, so an
/// overlapped floor is hidden and comes back only `if delay <= 0 and not
/// check(player, 0, 0)` - its `delay` is not read while the player is inside
/// (and an exact one could not be kept anyway: a floor the player enters was
/// widened a frame before, and a concrete row is looked up by its projection,
/// `Rt2::widen_to`, which has no history). The widened `state` contains the
/// computed one, or the widening's own error says where not.
///
/// What an overlapped floor stores is the CART'S INVARIANT, not the computed
/// value: hidden, `state` 2 and `collideable` false - the player cannot step
/// into a collideable floor, and a hidden one comes back only where the
/// player does not overlap it - with the computed value checked against it
/// as the widening's own error (strict: a lane where it may differ declines).
/// Per lane: `Sel(overlap, 2, [0, 2])` and `Sel(overlap, false, unknown)`.
/// Storing the computed value instead made every floor the region's player
/// MIGHT overlap a stored field on every outcome, so the split pass
/// (`verify::split_undecided_selects`) resolved each such floor's whole
/// update - `state == 0 / 1 / 2` times `delay <= 0`, ~5 outcomes a floor -
/// and multiplied them over the floors, though on any one lane all but the
/// (at most two) floors the player overlaps store the widened value whatever
/// they computed. Room (6,1), five floors side by side under its spawn: one
/// region's player half split 4,614 times into 3,638 outcomes (344k bodies),
/// four regions past the 4,096 cap; with the invariant stored, 108 outcomes
/// (5,454 bodies): only what the player's collisions read - each floor solid
/// or not - is still split. Exactly what the projection of a
/// concrete row holds there (`Rt2::widen_to` keeps an overlapped floor as it
/// is, and in the game it is hidden).
///
/// AT THE MIDDLE OF A SPLIT FRAME (`mid`) one more floor stays as computed:
/// its `collideable`, wherever the player's own update - the second step, the
/// buttons - may read it. That update reads the floors only through
/// `is_solid` (`check(fall_floor, ox, oy)` for `ox` in -3..=3, `oy` in 0..=1:
/// the ground below, a step sideways, the wall jump's 3 px), and the floors
/// update in the FIRST step (they stand before the player in `objects`). So
/// widened there, the second step read a floor the first step had just
/// decided as solid-or-not as both, and the split frame reached states the
/// frame does not (room (2,1) `r0sxhn` f34: 496k states against 387k). The
/// window is `PLAYER_PROBE` around the overlap window; `state` is widened as
/// at the frame's end (the second step does not read it, and the frame's end
/// widens it, the player not having moved), and the next step reads the
/// stored `collideable` rather than deriving it (`fork_near_floor_inputs`).
fn widen_near_floors(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors, mid: bool) -> Result<()> {
    use super::domain::Cmp;
    if !d.floors_near {
        return Ok(());
    }
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
        let coord = |f: &str| -> Result<crate::transpile::graph::NodeId> {
            let p = field(&obj, &[f]);
            match iface::get(st, &p) {
                Some(Value::Num(v)) => Ok(v),
                _ => bail!("{}: not a number", iface::show(&p)),
            }
        };
        players.push((coord("x")?, coord("y")?));
    }
    widen_near_phases(st, d, errs)?;
    let fp = near_floor_paths(st);
    let (slo, shi) = FLOOR_STATE_RANGE;
    for ((obj, ps), pc) in fp.floors.iter().zip(&fp.state).zip(&fp.collideable) {
        hitbox(st, d, obj, FLOOR_HITBOX)?;
        let at = position(st, d, obj)?;
        let [(xlo, xhi), (ylo, yhi)] = floor_player_window(at);
        // `lo < v < hi` for every value `v` a lane may hold: an interval by
        // its ends, so a bucket straddling an edge is no overlap (widened,
        // the safe side) - as `runtime2::player_overlaps_floor`.
        let inside = |d: &mut Symbolic, v: crate::transpile::graph::NodeId, lo: P8, hi: P8| -> Result<crate::transpile::graph::NodeId> {
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
        // Overlapped: hidden, as the cart keeps it - owed, not assumed, as
        // `collideable` false below. That one owe carries `state` 2 too: the
        // input `collideable` is DERIVED as `state ~= 2`
        // (`fork_near_floor_inputs`) and the cart's only writes keep the two
        // in step (1 -> 2 with false, 2 -> 0 with true), so at the frame's end
        // "not collideable" IS "state 2". Owing `state == 2` as well declined
        // lanes once the countdowns stopped wrapping (2026-10-03,
        // `asm_interval_overflow_is_the_whole_range`): a shaking floor's
        // `state` is then `sel(delay - 1 <= 0, 2, 1)`, which the player's
        // collision split never reads, so the error's own case analysis took
        // "not done" - a solid floor the player is inside, which the row's
        // collision outcome had already ruled out - and the lane erred.
        let apart = d.not(&overlap);
        let two = d.num(P8::from_i16(2));
        let range = d.graph.leaf(Op::Const(slo, shi));
        let state = d.sel_num(&overlap, &two, &range);
        iface::set(st, ps, Value::Num(state))?;
        let Some(Value::Bool(coll)) = iface::get(st, pc) else { bail!("{}: not a boolean", iface::show(pc)) };
        // Owed at the FRAME'S END only. In the middle of a split frame the
        // player has not moved, so nothing has split a shaking floor's
        // `delay - 1 <= 0`, and the error's case analysis keeps "not done":
        // a solid floor the player is inside, which the cart rules out (a
        // floor the player is inside comes back only `if delay <= 0 and not
        // check(player, 0, 0)`) and which the end step's collision split
        // decides. Owed mid-frame as well, it declined room (2,1)'s lanes at
        // step 48 once the countdowns stopped wrapping (2026-10-03).
        if !mid {
            let passable = d.not(&coll);
            let held = d.or(&apart, &passable);
            owe(errs, d, pc, held);
        }
        let (unknown, absent) = (d.unknown_bool_output(), d.boolean(false));
        // Mid-frame, in the probe window: the computed `collideable`, kept.
        let unread = if mid { d.sel_bool(&probe, &coll, &unknown) } else { unknown };
        let coll = d.sel_bool(&overlap, &absent, &unread);
        iface::set(st, pc, Value::Bool(coll))?;
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

/// The platforms' INPUT side at a platforms-unknown level: each platform's
/// `x` its input cell over the whole path, `last` read as `x` (the update
/// ends with `last = x`, `widen_platforms` checks it), `rem.x` the literal
/// whole remainder. The `x` cells are recorded (`Symbolic::platform_cells`)
/// for the split pass, which decides a comparison of the player against a
/// platform per PLATFORM WORLD (`verify::Points`): every arrangement the
/// search can meet, pinned onto these cells. So the ten platforms stay
/// consistent with each other inside a frame - a path whose answers no one
/// world gives is dropped at compile time - where ten independent
/// intervals admitted the player carried by every platform at once (room
/// (6,0), 2026-09-27), and no lane ever holds a world.
///
/// `rem.x` stays a literal, NOT pinned (decision 2026-09-28, plans/
/// graph-model.md): the platform's own move floors it, and a literal's
/// floor runs as rejoined fragments (`Interp::rejoin_fragments`, no fork)
/// where an interval input cell forks the frame once per platform. The
/// price is a pixel of slack in where a platform stands after its move, at
/// this coarse rung only - the exact-platform levels above it are exact.
///
/// Each platform's `spd.x`, where it is an input cell (not pinned), gets the
/// range of its speeds over the worlds (0 at the load, `dir * 0.65` after):
/// without it a platform's move had no bound, and its containment in the path
/// was left to the lane - which holds the whole path, so it failed (room
/// (6,0)'s first frame, 2026-09-28). Returned as obligations for the frame's
/// admissible inputs, so it is checked per lane, never assumed.
pub fn platform_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<Vec<crate::transpile::graph::NodeId>> {
    let worlds = d.worlds.clone().ok_or_else(|| anyhow::anyhow!("the platforms are unknown but there is no world table"))?;
    let platforms = objects_of_type(st, "platform");
    let first = worlds.first().ok_or_else(|| anyhow::anyhow!("no platform world"))?;
    anyhow::ensure!(first.len() == platforms.len(), "a platform world has {} platforms, the state {}", first.len(), platforms.len());
    // WHICH world platform each of this state's is: by `y` and `dir`, which
    // never change. The state's order is its row's canonical one, not the
    // load's the worlds were recorded in (room (6,0): a pin on one platform's
    // speed read against another's range, 2026-09-28). Platforms alike in
    // both are alike in everything a row keeps (their `x` is the whole
    // path), so any matching among them is the same.
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
    let mut cells: Vec<Option<crate::transpile::graph::NodeId>> = vec![None; platforms.len()];
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
            // NO PLAYER: a platform's move is unobservable - its `x`, `last`
            // and `rem.x` are widened at the frame's end and `update` sets
            // `spd.x` to `dir * 0.65` whatever it was - and the worlds say an
            // unpinned `spd.x` is 0 (the load) or that one speed. So the move
            // reads the speed as the literal, and the floor of the literal
            // remainder plus it rejoins as fragments (`literal_fragments`)
            // where an input cell forked it, once per platform - with the
            // fruit unknown, 2048 configurations per outcome of the room
            // (6,0) spawn shape (decision 2026-09-28). Asserted: the lane's
            // speed is 0 or that one.
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
                d.ranges.insert(sv, (lo as i64, hi as i64));
                let (klo, khi) = (d.graph.leaf(Op::Const(lo, lo)), d.graph.leaf(Op::Const(hi, hi)));
                let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![sv]), d.graph.fold(Op::Hi, vec![sv]));
                let a = d.graph.fold(Op::Ge, vec![vlo, klo]);
                let b = d.graph.fold(Op::Le, vec![vhi, khi]);
                obligations.push(d.graph.fold(Op::And, vec![a, b]));
            }
        }
        let (x, last, rem) = (field(obj, &["x"]), field(obj, &["last"]), field(obj, &["rem", "x"]));
        let Some(Value::Num(xv)) = iface::get(st, &x) else { bail!("{}: not a number", iface::show(&x)) };
        anyhow::ensure!(matches!(d.graph.get(xv).op, Op::Cell(_)), "a platform's `x` must be an input cell");
        d.ranges.insert(xv, ((PLATFORM_PATH.0 as i64) << 16, (PLATFORM_PATH.1 as i64) << 16));
        iface::set(st, &last, Value::Num(xv))?;
        let r = d.graph.leaf(Op::Const(PLATFORM_REM.0, PLATFORM_REM.1));
        iface::set(st, &rem, Value::Num(r))?;
        cells[j] = Some(xv);
    }
    d.platform_cells = cells.into_iter().map(|c| c.expect("every world platform matched")).collect();
    Ok(obligations)
}

/// The platforms' OUTPUT side at a platforms-unknown level: `x` and `last` the
/// interval of the whole path, `rem.x` the whole remainder (`Rt2::widen_to`
/// step 8b projects the same way). Checked, never assumed: the output `last`
/// must BE the output `x` (the update ends with `last = x`) - with the start
/// state, what makes the input alias sound by induction - and each widened
/// field must provably lie in its range (`within`, through the wrap's point
/// splits), else the widening's own error declines the lane loudly.
fn widen_platforms(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
    if !d.platforms_unknown {
        return Ok(());
    }
    let pp = platform_paths(st);
    let path = ((PLATFORM_PATH.0 as i64) << 16, (PLATFORM_PATH.1 as i64) << 16);
    for (x, last) in pp.x.iter().zip(&pp.last) {
        let Some(Value::Num(xv)) = iface::get(st, x) else { bail!("{}: not a number", iface::show(x)) };
        let Some(Value::Num(lv)) = iface::get(st, last) else { bail!("{}: not a number", iface::show(last)) };
        anyhow::ensure!(xv == lv, "{}: a platform's `last` is not its `x` at the frame's end", iface::show(last));
        if let Some(inside) = contain(d, xv, path) {
            owe(errs, d, x, inside);
            owe(errs, d, last, inside);
        }
        let hull = d.graph.leaf(Op::Const(path.0 as i32, path.1 as i32));
        iface::set(st, x, Value::Num(hull))?;
        iface::set(st, last, Value::Num(hull))?;
    }
    let rem = (PLATFORM_REM.0 as i64, PLATFORM_REM.1 as i64);
    for p in &pp.rem_x {
        let Some(Value::Num(v)) = iface::get(st, p) else { bail!("{}: not a number", iface::show(p)) };
        if let Some(inside) = contain(d, v, rem) {
            owe(errs, d, p, inside);
        }
        let r = d.graph.leaf(Op::Const(PLATFORM_REM.0, PLATFORM_REM.1));
        iface::set(st, p, Value::Num(r))?;
    }
    Ok(())
}

/// `v` in `[lo, hi]` (raw): proved statically (`within`, `None`), else the
/// condition to check per lane (like the fly fruit's containment,
/// `widen_fly_fruit`).
fn contain(d: &mut Symbolic, v: crate::transpile::graph::NodeId, (lo, hi): (i64, i64)) -> Option<<Symbolic as Domain>::Bool> {
    if within(d, v, (lo, hi), &mut Vec::new()) {
        return None;
    }
    let (klo, khi) = (d.graph.leaf(Op::Const(lo as i32, lo as i32)), d.graph.leaf(Op::Const(hi as i32, hi as i32)));
    let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
    let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
    let below = d.graph.fold(Op::Le, vec![vhi, khi]);
    Some(d.graph.fold(Op::And, vec![above, below]))
}

/// Does `v` provably lie in `[lo, hi]` (raw)? Through the arms of a select,
/// and under a point split's branch with what its answer says about the
/// compared value (`Symbolic::point_cmp`): the wrap `x < -16 ? 128 : (x > 128
/// ? -16 : x)` is on the path whatever `x` was. Static; `false` where it
/// cannot tell.
fn within(d: &Symbolic, v: crate::transpile::graph::NodeId, (lo, hi): (i64, i64), facts: &mut Vec<(crate::transpile::graph::NodeId, i64, i64)>) -> bool {
    // Its own bounds first, with what the branches know about IT: a select
    // the enclosing branch bounds (the wrap's `not (x < -16)` on `x`, itself
    // a select on whether the platform moved) is bounded as a whole, where
    // its arms alone are not (room (6,0), 2026-09-28).
    if let Some((a, b)) = bounds(d, v, facts) {
        if lo <= a && b <= hi {
            return true;
        }
    }
    let node = d.graph.get(v);
    if let Op::Sel = node.op {
        let (c, t, f) = (node.args[0], node.args[1], node.args[2]);
        // A select on `x op k` (`k` a literal point): each arm with what its
        // answer says about `x`.
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

/// `c` as `x op k` with `k` a literal point (either side): `x`, and the raw
/// range each answer puts it in - `(when true, when false)`.
fn comparison_facts(d: &Symbolic, c: crate::transpile::graph::NodeId) -> Option<(crate::transpile::graph::NodeId, (i64, i64), (i64, i64))> {
    let node = d.graph.get(c);
    let point = |n: crate::transpile::graph::NodeId| match d.graph.get(n).op {
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

/// A static raw range of `n`: literals, a platform's input `x` (its path),
/// sums and differences - narrowed by what the enclosing branches know about
/// `n`. `None` for anything else.
fn bounds(d: &Symbolic, n: crate::transpile::graph::NodeId, facts: &[(crate::transpile::graph::NodeId, i64, i64)]) -> Option<(i64, i64)> {
    let node = d.graph.get(n);
    let structural = || -> Option<(i64, i64)> {
        Some(match node.op {
            Op::Const(a, b) => (a as i64, b as i64),
            // One of its arms.
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
            Op::Cell(_) if d.ranges.contains_key(&n) => d.ranges[&n],
            _ => return None,
        })
    };
    // Known from its structure, else only from the branches around it - also
    // where its structure is known but an operand's is not (a platform's
    // move through a fork's fragment: room (6,0)'s first frame, where the
    // wrap's own branch is what bounds it, 2026-09-28).
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

/// The fly fruit's `spd.y` and `rem.y` ranges: ONE definition, shared with the
/// block model's projection (`Rt2::widen_to`), or the concrete search's node lookup misses.
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
fn widen_fly_fruit(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
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
            // becomes the widening's own error, checked per lane: a lane
            // outside the range declines loudly, none is widened wrongly.
            _ => {
                let (klo, khi) = (d.graph.leaf(Op::Const(*lo, *lo)), d.graph.leaf(Op::Const(*hi, *hi)));
                let (vlo, vhi) = (d.graph.fold(Op::Lo, vec![v]), d.graph.fold(Op::Hi, vec![v]));
                let above = d.graph.fold(Op::Ge, vec![vlo, klo]);
                let below = d.graph.fold(Op::Le, vec![vhi, khi]);
                let inside = d.graph.fold(Op::And, vec![above, below]);
                owe(errs, d, p, inside);
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

/// `player.dash_effect_time` := max(0, it) - a clamp (the field decrements
/// forever and is only read `> 0`, so every value <= 0 is behaviorally
/// identical).
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
/// Applied at EVERY non-exact rung, so it is rung-independent.
fn widen_fruit(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
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
        // The widening's own error: `y` was not inside the band it
        // replaced. Symbolic bounds are fine - it is checked per lane, and
        // the lane declines where it fails.
        let a = d.compare(super::domain::Cmp::Ge, &old_y, &l)?;
        let b = d.compare(super::domain::Cmp::Le, &old_y, &h)?;
        let inside = d.and(&a, &b);
        owe(errs, d, &py, inside);
        let band = d.graph.fold(Op::Span, vec![l, h]);
        iface::set(st, &py, Value::Num(band))?;
        let all = ival(d, P8::from_i16(0), P8::from_i16(39));
        iface::set(st, &po, Value::Num(all))?;
    }
    Ok(())
}

/// The timer globals (`frames`/`seconds`/`minutes`/`deaths`) pinned to
/// zero - gameplay-dead pins - and with
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
