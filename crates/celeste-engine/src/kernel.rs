//! Kernel runtime (plans/kernel-plan.md K1): the typed lane primitives the
//! generated steady-shape kernel (`kernel_gen.rs`) is emitted against.
//!
//! Types are STATIC - the emitter proved every value's type at emit time
//! against the shape witness, so nothing here carries a tag. Lanes = ROWS
//! (W = 16 x i32 = one zmm). All ops are per-lane loops over `Pico8Num`'s
//! own operators, so the semantics are inherited from the certified scalar
//! implementations rather than re-derived; LLVM autovectorizes the loops
//! (verified by disassembly in K2), and only measured-hot loops get manual
//! intrinsics.
//!
//! Deopt is a per-lane mask (bit i = lane i must be re-run by the
//! reference interpreter). Nothing in this module panics on abstract-domain
//! limits - a limit is a deopt, not an error; panics are reserved for
//! contract violations (emitter bugs).

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as IV};

pub const W: usize = 16;

pub use crate::runtime2::{cell_mix, mix64};

/// Which of a kernel's `KEY_CELLS` boundary canonicalizes before hashing,
/// as bit masks in KEY_CELLS order. Built per chunk from `Rt2::mark_walk`,
/// because "which cell is the player's rem" is a fact about the heap walk,
/// not about the shape witness the kernel was emitted against.
///
/// - `rem`: boundary replaces the cell with ONE wide interval, so it makes
///   no per-lane contribution and the key skips it entirely;
/// - `det`: boundary clamps the value at 0 (dash_effect_time), so the key
///   clamps before mixing.
///
/// Getting these wrong costs dedup ratio, never soundness - a key that
/// separates two rows boundary would merge just materializes both.
#[derive(Debug, Default, Clone, Copy)]
pub struct KeyPlan {
    pub rem: u64,
    pub det: u64,
}

/// One num column slice: 16 rows.
pub type ZN = [P8; W];
/// One interval column slice: low/high planes.
#[derive(Clone, Copy, Debug)]
pub struct ZI {
    pub lo: ZN,
    pub hi: ZN,
}
/// One tri-state bool column slice. `val` is meaningful where `known` is
/// set; an unknown lane deopts at any use that needs the value.
#[derive(Clone, Copy, Debug)]
pub struct ZB {
    pub val: u16,
    pub known: u16,
}

pub const ALL: u16 = 0xffff;

#[inline(always)]
pub fn zn_splat(v: P8) -> ZN {
    [v; W]
}
#[inline(always)]
pub fn zi_splat(lo: P8, hi: P8) -> ZI {
    ZI { lo: [lo; W], hi: [hi; W] }
}
#[inline(always)]
pub fn zb_splat(b: bool) -> ZB {
    ZB { val: if b { ALL } else { 0 }, known: ALL }
}
#[inline(always)]
pub fn zi_of_zn(v: ZN) -> ZI {
    ZI { lo: v, hi: v }
}

macro_rules! zn_map2 {
    ($name:ident, $op:expr) => {
        #[inline(always)]
        pub fn $name(a: ZN, b: ZN) -> ZN {
            let mut o = [P8::from_i16(0); W];
            let f = $op;
            for i in 0..W {
                o[i] = f(a[i], b[i]);
            }
            o
        }
    };
}
zn_map2!(zn_add, |a: P8, b: P8| a + b);
zn_map2!(zn_sub, |a: P8, b: P8| a - b);
zn_map2!(zn_mul, |a: P8, b: P8| a * b);
zn_map2!(zn_div, |a: P8, b: P8| a / b);
zn_map2!(zn_rem, |a: P8, b: P8| a % b);
zn_map2!(zn_min, |a: P8, b: P8| a.min(b));
zn_map2!(zn_max, |a: P8, b: P8| a.max(b));

#[inline(always)]
pub fn zn_neg(a: ZN) -> ZN {
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        o[i] = -a[i];
    }
    o
}
#[inline(always)]
pub fn zn_abs(a: ZN) -> ZN {
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        o[i] = a[i].abs();
    }
    o
}
#[inline(always)]
pub fn zn_flr(a: ZN) -> ZN {
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        o[i] = a[i].flr();
    }
    o
}
#[inline(always)]
pub fn zn_sin(a: ZN) -> ZN {
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        o[i] = a[i].pico8_sin();
    }
    o
}

// ---- interval ops (per-lane ports of runtime2's av_* interval arms) ----

#[inline(always)]
fn iv(z: &ZI, i: usize) -> IV {
    IV::new(z.lo[i], z.hi[i])
}
#[inline(always)]
fn set(z: &mut ZI, i: usize, v: IV) {
    z.lo[i] = v.low;
    z.hi[i] = v.high;
}

macro_rules! zi_map2 {
    ($name:ident, $op:expr) => {
        #[inline(always)]
        pub fn $name(a: ZI, b: ZI) -> ZI {
            let mut o = a;
            let f = $op;
            for i in 0..W {
                set(&mut o, i, f(iv(&a, i), iv(&b, i)));
            }
            o
        }
    };
}
zi_map2!(zi_add, |a: IV, b: IV| a + b);
zi_map2!(zi_sub, |a: IV, b: IV| a - b);
zi_map2!(zi_min, |a: IV, b: IV| IV::new(a.low.min(b.low), a.high.min(b.high)));
zi_map2!(zi_max, |a: IV, b: IV| IV::new(a.low.max(b.low), a.high.max(b.high)));

#[inline(always)]
pub fn zi_neg(a: ZI) -> ZI {
    let mut o = a;
    for i in 0..W {
        o.lo[i] = -a.hi[i];
        o.hi[i] = -a.lo[i];
    }
    o
}

/// av_abs interval arm.
#[inline(always)]
pub fn zi_abs(a: ZI) -> ZI {
    let zero = P8::from_i16(0);
    let mut o = a;
    for i in 0..W {
        let v = iv(&a, i);
        let r = if v.low >= zero {
            v
        } else if v.high <= zero {
            IV::new(v.high.abs(), v.low.abs())
        } else {
            IV::new(zero, v.low.abs().max(v.high.abs()))
        };
        set(&mut o, i, r);
    }
    o
}

/// av_mul interval arm: interval * positive num, per lane.
///
/// The multiplier's POSITIVITY is a premise, not a branch: the emitter
/// records `b > 0` as a conjunct of the member's validity, and a lane
/// that fails it is filtered out downstream. Lanes that fail it here are
/// left unchanged rather than scaled, because `scale_positive` asserts
/// and the value of a filtered lane is never read.
#[inline(always)]
pub fn zi_mul_pos(a: ZI, b: ZN) -> ZI {
    let zero = P8::from_i16(0);
    let mut o = a;
    for i in 0..W {
        if b[i] > zero {
            set(&mut o, i, iv(&a, i).scale_positive(b[i]));
        }
    }
    o
}
#[inline(always)]
pub fn zi_div_pos(a: ZI, b: ZN) -> ZI {
    let zero = P8::from_i16(0);
    let mut o = a;
    for i in 0..W {
        if b[i] > zero {
            set(&mut o, i, iv(&a, i).div_positive(b[i]));
        }
    }
    o
}

/// av_flr interval arm. Whether the floor is UNIQUE is a premise
/// (`zi_flr_ok`), so this just takes the low endpoint's floor - which is
/// the floor, on every lane that survives the premise.
#[inline(always)]
pub fn zi_flr(a: ZI) -> ZN {
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        o[i] = a.lo[i].flr();
    }
    o
}

/// The premise `zi_flr` is taken under: this lane's interval has ONE
/// floor. `known: ALL` because the answer is a fact about the interval,
/// never itself undecided.
#[inline(always)]
pub fn zi_flr_ok(a: ZI) -> ZB {
    let mut val = 0u16;
    for i in 0..W {
        if a.lo[i].flr() == a.hi[i].flr() {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

/// The premise `zi_fork_flr` is taken under: this lane's interval spans
/// at most TWO floors, so the two fork outcomes can represent it. A
/// boundary-widened interval has width < 1 and always passes.
#[inline(always)]
pub fn zi_span_ok(a: ZI) -> ZB {
    let mut val = 0u16;
    for i in 0..W {
        let (fl, fh) = (a.lo[i].flr(), a.hi[i].flr());
        if fl == fh || fh == fl + P8::from_i16(1) {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

// ---- comparisons ----

macro_rules! zn_cmp {
    ($name:ident, $op:tt) => {
        #[inline(always)]
        pub fn $name(a: ZN, b: ZN) -> ZB {
            let mut val = 0u16;
            for i in 0..W {
                if a[i] $op b[i] {
                    val |= 1 << i;
                }
            }
            ZB { val, known: ALL }
        }
    };
}
zn_cmp!(zn_lt, <);
zn_cmp!(zn_le, <=);
zn_cmp!(zn_gt, >);
zn_cmp!(zn_ge, >=);

#[inline(always)]
pub fn zn_eq(a: ZN, b: ZN) -> ZB {
    let mut val = 0u16;
    for i in 0..W {
        if a[i] == b[i] {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

/// The av_cmp tri-state judge, per lane, on intervals (degenerate
/// intervals give the same answers as plain numbers for ORDERED compares).
#[derive(Clone, Copy)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
}
#[inline(always)]
pub fn zi_cmp(op: Cmp, a: ZI, b: ZI) -> ZB {
    let mut val = 0u16;
    let mut known = 0u16;
    for i in 0..W {
        let (al, ah, bl, bh) = (a.lo[i], a.hi[i], b.lo[i], b.hi[i]);
        let t = match op {
            Cmp::Lt => {
                if ah < bl {
                    Some(true)
                } else if al >= bh {
                    Some(false)
                } else {
                    None
                }
            }
            Cmp::Le => {
                if ah <= bl {
                    Some(true)
                } else if al > bh {
                    Some(false)
                } else {
                    None
                }
            }
            Cmp::Gt => {
                if al > bh {
                    Some(true)
                } else if ah <= bl {
                    Some(false)
                } else {
                    None
                }
            }
            Cmp::Ge => {
                if al >= bh {
                    Some(true)
                } else if ah < bl {
                    Some(false)
                } else {
                    None
                }
            }
        };
        match t {
            Some(true) => {
                val |= 1 << i;
                known |= 1 << i;
            }
            Some(false) => known |= 1 << i,
            None => {}
        }
    }
    ZB { val, known }
}

/// `zi_cmp`'s scalar sibling: one block-uniform interval pair, tri-state
/// out. `Some` when every value pair decides the comparison the same way,
/// `None` when the intervals straddle - the caller (a `K::STri`) deopts
/// the slice on `None` if the result feeds a branch-like consumer.
#[inline(always)]
pub fn si_cmp(op: Cmp, a: (P8, P8), b: (P8, P8)) -> Option<bool> {
    let (al, ah, bl, bh) = (a.0, a.1, b.0, b.1);
    match op {
        Cmp::Lt => {
            if ah < bl {
                Some(true)
            } else if al >= bh {
                Some(false)
            } else {
                None
            }
        }
        Cmp::Le => {
            if ah <= bl {
                Some(true)
            } else if al > bh {
                Some(false)
            } else {
                None
            }
        }
        Cmp::Gt => {
            if al > bh {
                Some(true)
            } else if ah <= bl {
                Some(false)
            } else {
                None
            }
        }
        Cmp::Ge => {
            if al >= bh {
                Some(true)
            } else if ah < bl {
                Some(false)
            } else {
                None
            }
        }
    }
}

// ---- bool ops ----

#[inline(always)]
pub fn zb_not(a: ZB) -> ZB {
    ZB { val: !a.val, known: a.known }
}
/// Tri-state AND, Kleene. Known where BOTH are known, and also where
/// either is known FALSE - `false and anything` is false whether or not
/// the other side is known. That matches `Graph::fold`'s rule for
/// `Op::And`, which is the point: the emitter and the folder have to
/// agree about what an AND means or a folded graph and an emitted one
/// answer differently.
///
/// The old front end never needed this. Every `And` it built was either
/// the `Known(x) AND x` idiom or a validity conjunct that the emitter
/// flattened, so an AND was never rendered as a value. A traced graph
/// builds them freely - guards are `g AND c`, and `or` is De Morgan over
/// them - so they have to lower.
#[inline(always)]
pub fn zb_and(a: ZB, b: ZB) -> ZB {
    let known_false = (a.known & !a.val) | (b.known & !b.val);
    ZB { val: a.val & b.val, known: (a.known & b.known) | known_false }
}

/// av_eq bool arm: equal where both known; unknown where either is not.
#[inline(always)]
pub fn zb_eq(a: ZB, b: ZB) -> ZB {
    ZB { val: !(a.val ^ b.val), known: a.known & b.known }
}

// ---- select / guard / deopt ----

/// Per-lane blend. That the condition is DECIDED is a premise, recorded
/// as a validity conjunct by the emitter (`Known(c)`), so an undecided
/// lane blends as if false and is filtered out downstream.
#[inline(always)]
pub fn zsel_n(c: ZB, t: ZN, f: ZN) -> ZN {
    let mut o = f;
    for i in 0..W {
        if c.val & (1 << i) != 0 {
            o[i] = t[i];
        }
    }
    o
}
#[inline(always)]
pub fn zsel_i(c: ZB, t: ZI, f: ZI) -> ZI {
    let mut o = f;
    for i in 0..W {
        if c.val & (1 << i) != 0 {
            o.lo[i] = t.lo[i];
            o.hi[i] = t.hi[i];
        }
    }
    o
}
#[inline(always)]
pub fn zsel_b(c: ZB, t: ZB, f: ZB) -> ZB {
    ZB {
        val: (c.val & t.val) | (!c.val & f.val),
        known: (c.val & t.known) | (!c.val & f.known),
    }
}

/// The lanes on which `c` is DEFINITELY TRUE - known, and true. Validity
/// is the AND of these masks, so this is the one place a tri-state
/// becomes a plain lane mask.
#[inline(always)]
pub fn zb_holds(c: ZB) -> u16 {
    c.val & c.known
}

// ---- refinement splits ----

/// __split_by_flr as a <=2-way FORK (plans/kernel-plan.md K2): a
/// boundary-widened interval has width < 1, so it spans at most two
/// floors. Fragment 0 is the low-floor part (always non-empty);
/// fragment 1 is the high-floor part (empty on single-floor lanes).
/// Returns (fragment `c` per lane, valid mask); a lane with an empty
/// fragment simply produces no row in this fork configuration. Lanes
/// spanning >2 floors cannot be represented by two outcomes - they stay
/// valid in outcome 0 only, so the driver sees them exactly once, and
/// `zi_span_ok` is the premise that filters them out.
#[inline(always)]
pub fn zi_fork_flr(a: ZI, c: usize) -> (ZI, u16) {
    let mut o = a;
    let mut valid = 0u16;
    for i in 0..W {
        let (fl, fh) = (a.lo[i].flr(), a.hi[i].flr());
        if fl == fh {
            if c == 0 {
                valid |= 1 << i;
            }
        } else {
            if fh != fl + P8::from_i16(1) {
                // >2 floors: stay "valid" in outcome 0 only, so the
                // driver routes the lane onward exactly once; zi_span_ok
                // is what takes it off the kernel.
                if c == 0 {
                    valid |= 1 << i;
                }
                continue;
            }
            valid |= 1 << i;
            let boundary = fh; // first value of the high floor
            if c == 0 {
                o.hi[i] = boundary.next_smallest();
            } else {
                o.lo[i] = boundary;
            }
        }
    }
    (o, valid)
}

// ---- cart / collision builtins (per-lane; x/y vary, w/h/flag uniform) ----

/// mget, per lane, against the raw grid. Semantics identical to
/// `CartData::mget(..).expect(..)`: fractional or out-of-range
/// coordinates panic (the certified trace never produces them).
#[inline(always)]
pub fn zn_mget(cart: &CartData, x: ZN, y: ZN) -> ZN {
    let map = cart.map_grid();
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        let xi = x[i].as_i16().expect("mget: x is not an integer");
        let yi = y[i].as_i16().expect("mget: y is not an integer");
        assert!(
            (0..128).contains(&xi) && (0..64).contains(&yi),
            "mget out of range"
        );
        o[i] = P8::from_i16(map[xi as usize + yi as usize * 128] as i16);
    }
    o
}

/// tile_flag_at with uniform w/h/flag (runtime.rs bi_tile_flag_at):
/// flag != 0 is constant-false; flag 0 goes through the precomputed solid
/// map with the computed fallback.
#[inline(always)]
pub fn zn_tile_flag_at(
    cache: &CollisionCache,
    cart: &CartData,
    x: ZN,
    y: ZN,
    w: P8,
    h: P8,
    flag: P8,
) -> ZB {
    let f = flag.as_i16().expect("tile_flag_at: flag must be integer");
    if f != 0 {
        return zb_splat(false);
    }
    let wi = w.as_i16().expect("tile_flag_at: w");
    let hi = h.as_i16().expect("tile_flag_at: h");
    let mut val = 0u16;
    let solid = cache.solid_map(wi, hi);
    for i in 0..W {
        let xi = x[i].as_i16().expect("tile_flag_at: x must be an integer");
        let yi = y[i].as_i16().expect("tile_flag_at: y must be an integer");
        let b = match &solid {
            Some((map, dx, dy)) => match map.get(xi + dx, yi + dy) {
                Some(v) => v,
                None => cache.solid_at(cart, xi, yi, wi, hi).unwrap_or(false),
            },
            None => cache.solid_at(cart, xi, yi, wi, hi).unwrap_or(false),
        };
        if b {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

// ---- scalar (block-uniform) helpers the generated code leans on ----

/// Scalar interval + interval (the uniform twin of zi_add).
#[inline(always)]
pub fn si_add(a: (P8, P8), b: (P8, P8)) -> (P8, P8) {
    let r = IV::new(a.0, a.1) + IV::new(b.0, b.1);
    (r.low, r.high)
}
#[inline(always)]
pub fn si_sub(a: (P8, P8), b: (P8, P8)) -> (P8, P8) {
    let r = IV::new(a.0, a.1) - IV::new(b.0, b.1);
    (r.low, r.high)
}
