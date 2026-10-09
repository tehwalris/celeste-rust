//! The boundary's widenings, applied INSIDE the traced frame so the graph
//! computes the row it stores (canonical rows; a kernel dedups its own
//! output). Each widening CHECKS that the replaced value lies inside what it
//! writes (`SlotErrors`), so a violating lane declines loudly. `Rt2::widen_to`
//! must store what these output widenings store.

use anyhow::{bail, Result};

use celeste_core::pico8_num::Pico8Num as P8;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;
use crate::transpile::graph::Op;

/// THE ABSENT NUMBER FIELDS: `(type global, field)` that `init` leaves unset
/// and a later update adds as a number (else "which floors have broken" is
/// heap SHAPE). Every outcome writes the missing field as 0. Sound because
/// the cart cannot tell nil from 0 here: `cart::check_absent_fields` refuses
/// any read but arithmetic or ordering (which halt PICO-8 on nil), and
/// `Interp::index_key` refuses a computed `t[k]` naming it.
// TODO(Philippe): writing the missing field as 0 is a hack; revisit.
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

/// A constant interval `[lo, hi]` as one graph node (`Op::Const(lo, hi)`).
fn ival(d: &mut Symbolic, lo: P8, hi: P8) -> <Symbolic as Domain>::Num {
    d.graph.leaf(Op::Const(lo.as_raw_u32() as i32, hi.as_raw_u32() as i32))
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

/// Apply the boundary widenings to `st`: the remainder, the always-on pins
/// and clamps, and the objects as the level's flags say.
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

/// The player's remainder := [-1/2, 1/2); the edge's transfer
/// (`search::arc_edges`) carries what the frame did to it.
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

/// The balloon's `rnd` phase `offset`, a full-period interval. Its one reader
/// is `sin(offset)`, `[-1, 1]` on any full period, so storing the canonical
/// `[0, 1)` is EXACT; that it is a full period is owed. `Rt2::widen_to` agrees.
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

/// Held buttons unknown: the trails leave the frame as the canonical unknown.
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

/// The object PHASES a near level widens, each with its stored range: the
/// spring's `spr` and countdowns, the balloon's `spr` and `y`. Their updates
/// become "maybe bounce" / "maybe refill the dash". The balloon's `y` is the
/// bob band `start +- BALLOON_BOB_RAW`, since its bob runs only when
/// `spr == 22` and an exact `y` would give every state a twin.
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

/// How a near level stores a phase: a fixed range, `start` +- a radius
/// (raw 16.16), or the unknown number.
pub enum PhaseRange {
    Fixed((i32, i32)),
    AroundStart(Path, i32),
    Countdown,
}

/// The countdowns a near level widens: fall floor `delay`, balloon `timer`.
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

/// The countdowns' OUTPUT side at a near level: THE UNKNOWN NUMBER (the cart
/// only decrements them and compares with 0; no premise needed). Not the
/// interval [MIN, MAX]: `delay - 1` of it overflows (`Op::NoWrap`) and every
/// lane would decline.
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

/// The countdowns a level stores as the unknown number.
pub fn countdown_paths(st: &State<Symbolic>, d: &Symbolic) -> Vec<Path> {
    let mut out = Vec::new();
    if d.floors_near {
        out.extend(floor_timer_paths(st));
        out.extend(phase_paths(st).into_iter().filter(|(_, r)| matches!(r, PhaseRange::Countdown)).map(|(p, _)| p));
    }
    out
}

/// The countdowns' INPUT side: the unknown number; an absent slot stays absent.
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

/// Per fall floor, the fields a near level widens except where the player
/// overlaps it.
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

/// The phases' OUTPUT side at a near level: intervals (so `spr == 18` splits
/// like a floor's `state == k`), countdowns unknown; containment is owed.
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

/// A near level's INPUT side: a floor's `state` as stored (split by
/// `verify::split_undecided_selects`), `collideable` DERIVED as `state ~= 2`.
/// The cart keeps them in step; an independent unknown `collideable` would
/// admit players inside solid floors.
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

/// Mid split frame, a lane may hold an unknown `collideable`, but a boolean
/// slot binds as a decided cell that would read it as false. So: the cell
/// where the lane knows it, else a fork of both values.
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

/// How far `player.update` probes a fall floor (`is_solid(ox, oy)`, `ox` in
/// -3..=3, `oy` in 0..=1), as offsets to the overlap window's ends.
pub const PLAYER_PROBE: [(i16, i16); 2] = [(-3, 3), (-1, 0)];

/// A near level's OUTPUT side: every fall floor stores `state` as
/// `FLOOR_STATE_RANGE` and `collideable` unknown, EXCEPT where the player
/// overlaps it; countdowns are widened regardless (unread while inside). An
/// overlapped floor stores the CART'S INVARIANT (hidden: `state` 2,
/// `collideable` false, the player cannot be inside a solid floor), with the
/// computed value owed; storing the computed value makes the split resolve
/// every floor the player MIGHT overlap (room (6,1): 3,638 outcomes vs 108)
/// - except with the platforms unknown, where it stores the computed value
/// (below). Mid split frame (`mid`), `collideable` also stays computed in the
/// `PLAYER_PROBE` window: widening there would let the second step reach
/// states the unsplit frame does not. `Rt2::widen_to` agrees.
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
        // `lo < v < hi` for every value a lane may hold (straddling is no
        // overlap, the safe side), as `runtime2::player_overlaps_floor`.
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
        let computed = d.platforms_unknown;
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

/// The fields a platforms-unknown level widens, per moving platform.
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

/// The platforms' INPUT side at a platforms-unknown level: `x` an input cell
/// over the whole path, `last` read as `x` (checked by `widen_platforms`),
/// `rem.x` the literal whole remainder (a literal's floor rejoins as
/// fragments; the price is a pixel of slack at this level). The `x` cells are
/// recorded so the split decides player-vs-platform comparisons per PLATFORM
/// WORLD, keeping platforms mutually consistent. Each unpinned `spd.x` gets
/// its range over the worlds; returned as per-lane obligations, never assumed.
pub fn platform_inputs(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<Vec<crate::transpile::graph::NodeId>> {
    let worlds = d.worlds.clone().ok_or_else(|| anyhow::anyhow!("the platforms are unknown but there is no world table"))?;
    let platforms = objects_of_type(st, "platform");
    let first = worlds.first().ok_or_else(|| anyhow::anyhow!("no platform world"))?;
    anyhow::ensure!(first.len() == platforms.len(), "a platform world has {} platforms, the state {}", first.len(), platforms.len());
    // WHICH world platform each is, by constant `y` and `dir` (rows are in
    // canonical order); platforms alike in both are interchangeable.
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

/// The platforms' OUTPUT side: `x` and `last` the whole path's interval,
/// `rem.x` the whole remainder (as `Rt2::widen_to`). The output `last` must
/// BE the output `x` (the input alias's induction), and containment is
/// proved (`within`) or owed.
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

/// `v` in `[lo, hi]` (raw): `None` if proved statically, else the per-lane
/// condition.
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

/// Does `v` provably lie in `[lo, hi]` (raw)? Through select arms and under
/// point-split branches with what they say about the compared value (so a
/// wrap `x < -16 ? 128 : ...` is bounded). Static; `false` if unsure.
fn within(d: &Symbolic, v: crate::transpile::graph::NodeId, (lo, hi): (i64, i64), facts: &mut Vec<(crate::transpile::graph::NodeId, i64, i64)>) -> bool {
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

/// A static raw range of `n` (literals, a platform's input `x`, a bounded
/// input, sums and differences), narrowed by the enclosing branches; `None` otherwise.
fn bounds(d: &Symbolic, n: crate::transpile::graph::NodeId, facts: &[(crate::transpile::graph::NodeId, i64, i64)]) -> Option<(i64, i64)> {
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
            Op::Cell(_) if d.ranges.contains_key(&n) => d.ranges[&n],
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

/// The fly fruit's `spd.y`/`rem.y` ranges: ONE definition, shared with
/// `Rt2::widen_to`, or the concrete search's node lookup misses.
use celeste_engine::runtime2::{FLY_FRUIT_REM_Y as FRUIT_REM_Y, FLY_FRUIT_SPD_Y as FRUIT_SPD_Y};

/// The fields a fruit-unknown level widens, per live fly fruit.
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

/// The fly fruit's INPUT side at a fruit-unknown level: `step`/`y` unknown,
/// `spd.y`/`rem.y` their ranges as literals, `fly` an undecided atom. A
/// decided input lies inside, so no premise.
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

/// The fly fruit's OUTPUT side: as the input side. A range replaces only
/// values visibly inside it (literals or selects of them); anything else
/// owes containment per lane.
fn widen_fly_fruit(st: &mut State<Symbolic>, d: &mut Symbolic, errs: &mut SlotErrors) -> Result<()> {
    if !d.fruit_unknown {
        return Ok(());
    }
    // Every value a lane can hold: a literal, or a select over such values.
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
            // Not visibly inside: owed, checked per lane.
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

/// `player.dash_effect_time` := max(0, it): it decrements forever and is
/// only read `> 0`, so every value <= 0 behaves alike.
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

/// A live fruit's `off` := [0, 39] and `y` := start +- 2.5, TOGETHER (one
/// without the other is a row no level has). Applied at every level.
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
        // The band's bounds are expressions of `start` (an input column), so
        // the band is per-lane `Op::Span`.
        let Some(Value::Num(old_y)) = iface::get(st, &py) else {
            bail!("{}: fruit `y` is not a number", iface::show(&py));
        };
        let amp = d.num(amplitude);
        let l = d.arith(super::domain::Arith::Sub, &start, &amp)?;
        let h = d.arith(super::domain::Arith::Add, &start, &amp)?;
        // Owed: `y` inside the band (checked per lane).
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

/// The gameplay-dead timer globals pinned to zero, and with them each key's
/// `frames`-derived `spr` (8) and `flip.x` (false). Every level.
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

/// The held trails' INPUT side at a held-unknown level: `p_jump`/`p_dash` each
/// an independent 2-way fork. Every configuration is valid for every lane (a
/// decided trail only over-approximates), so no premise.
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
