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
    Ok(RemBucket { value, premise, fork: None })
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
/// zero - `apply_conservative_widenings`' gameplay-dead pins.
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
    use crate::interpreter::abstraction::SpdPrecision;
    let SpdPrecision::WidthLog2(w) = precision else {
        return Ok(());
    };
    for obj in objects_of_type(st, "player") {
        for f in ["x", "y"] {
            let p = field(&obj, &["spd", f]);
            let Some(Value::Num(old)) = iface::get(st, &p) else {
                bail!("{}: spd is not a number", iface::show(&p));
            };
            let sb = spd_bucket_node(d, old, w)?;
            if let Some((valid, premise)) = sb.fork {
                st.guard = d.and(&st.guard, &valid);
                let ok = st.ok;
                st.ok = d.and(&ok, &premise);
            }
            let ok = st.ok;
            st.ok = d.and(&ok, &sb.premise);
            iface::set(st, &p, Value::Num(sb.value))?;
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
    // grid (`Graph::fork_bits`, 2^-k), and the spd bucket at rung k is
    // exactly one grid cell (`spd_precision_for`: width 2^(16-k) raw), so
    // a fragment lies within one bucket. (Forking the SCALED value cut it
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
    Ok(RemBucket { value, premise, fork })
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
