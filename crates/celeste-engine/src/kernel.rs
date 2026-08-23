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

/// One machine-word column slice: the row-key fold's accumulator, and the
/// bits of one cell's value, 16 lanes at a time.
///
/// The ONLY lane type here that is not an abstract game value. It exists
/// so the fold can be part of the graph instead of a scalar loop inside
/// `append`. The fold stays sequential over CELLS - each step depends on
/// the last - while every step is 16 lanes wide, and the lanes are the
/// axis the parallelism actually lives on. Measured 2026-08-23, the
/// scalar-per-lane version was 75% of ALL kernel time: ~190 `imul`s per
/// row in one dependency chain, over 2.17M candidate rows, 87% of which
/// were then dropped as duplicates.
pub type ZW = [u64; W];

#[inline(always)]
pub fn zw_splat(v: u64) -> ZW {
    [v; W]
}

/// The representation BITS of one cell's value - not a hash, just the bit
/// pattern the fold consumes, packed so that two abstract values which
/// differ differ here too.
#[inline(always)]
pub fn zw_bits_n(x: ZN) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = x[i].as_raw_u32() as u64;
    }
    o
}
#[inline(always)]
pub fn zw_bits_i(x: ZI) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = ((x.lo[i].as_raw_u32() as u64) << 32) | (x.hi[i].as_raw_u32() as u64);
    }
    o
}
/// Two bits per lane: the value where it is known, and whether it is.
#[inline(always)]
pub fn zw_bits_b(x: ZB) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = (((x.val >> i) & 1) as u64) | ((((x.known >> i) & 1) as u64) << 1);
    }
    o
}

/// One step of the row-key fold, over the accumulator and one cell's
/// bits. TWO accumulators mixed differently, so the pair is 128 bits: a
/// collision here DROPS a successor rather than merely costing time.
///
/// The cell id goes in as well as the value, so the key is not invariant
/// under moving a value from one field to another.
#[inline(always)]
pub fn zw_mix1(h: ZW, v: ZW, c: u64) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = mix64(h[i] ^ mix64(v[i] ^ c));
    }
    o
}
#[inline(always)]
pub fn zw_mix2(h: ZW, v: ZW, c: u64) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = h[i].wrapping_add(mix64(v[i].wrapping_mul((c << 1) | 1)));
    }
    o
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
//
// SCALAR ON PURPOSE, and this is the second time it has been measured.
//
// `Pico8Num` wraps an `i32` and derives `Ord` from it, so an abstract
// comparison IS a signed 32-bit compare and the 16-lane result is
// exactly the bitmask `ZB` wants - which reads like a standing
// invitation to write `_mm512_cmp_epi32_mask` and delete sixteen
// iterations. It was written that way on 2026-08-23 and the room went
// from 177 ms to 225 ms, a 27% LOSS, identical whether the column was
// loaded through a pointer or transmuted register-to-register.
//
// The reason is in the disassembly: LLVM ALREADY vectorizes these. The
// forced version emitted 355 `vpcmp*` for 629 call sites and zero
// `setg`/`setl` anywhere in `frame` - so the "sixteen compare-shift-or
// sequences" the intrinsic was meant to replace did not exist. What the
// intrinsic removed was LLVM's freedom to decide PER SITE: where a
// column is being built or consumed scalar-wise, keeping the compare
// scalar avoids assembling a vector only to extract a mask from it, and
// forcing the vector form pays `vpinsrd` + `kmovw` instead. The same
// disassembly shows 1,229 `vpblendmd` for 1,442 `zsel_n` sites, so the
// blends are already vectorized too.
//
// Write these as plain loops over `Pico8Num`'s own operators and leave
// the choice to the compiler. See `plans/tracing.md`.

/// Defines `$name`, the `ZB`-returning primitive the kernels call, and
/// `$mask`, the raw lane mask `zi_cmp` combines.
macro_rules! zn_cmp {
    ($name:ident, $mask:ident, $op:tt) => {
        #[inline(always)]
        fn $mask(a: ZN, b: ZN) -> u16 {
            let mut val = 0u16;
            for i in 0..W {
                if a[i] $op b[i] {
                    val |= 1 << i;
                }
            }
            val
        }
        #[inline(always)]
        pub fn $name(a: ZN, b: ZN) -> ZB {
            ZB { val: $mask(a, b), known: ALL }
        }
    };
}
zn_cmp!(zn_lt, mask_lt, <);
zn_cmp!(zn_le, mask_le, <=);
zn_cmp!(zn_gt, mask_gt, >);
zn_cmp!(zn_ge, mask_ge, >=);
zn_cmp!(zn_eq, mask_eq, ==);

/// The comparison the tri-state interval judge is performing.
#[derive(Clone, Copy)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
}

/// The av_cmp tri-state judge, per lane, on intervals (degenerate
/// intervals give the same answers as plain numbers for ORDERED compares).
///
/// Each arm is two of the number masks above. `t` is "definitely true"
/// and `f` is "definitely false"; a lane in neither is unknown. The two
/// are disjoint by construction - for `Lt`, `ah < bl` and `al >= bh`
/// together give `bh <= al <= ah < bl <= bh` - so `val = t` needs no
/// masking against `f`.
#[inline(always)]
pub fn zi_cmp(op: Cmp, a: ZI, b: ZI) -> ZB {
    let (t, f) = match op {
        Cmp::Lt => (mask_lt(a.hi, b.lo), mask_ge(a.lo, b.hi)),
        Cmp::Le => (mask_le(a.hi, b.lo), mask_gt(a.lo, b.hi)),
        Cmp::Gt => (mask_gt(a.lo, b.hi), mask_le(a.hi, b.lo)),
        Cmp::Ge => (mask_ge(a.lo, b.hi), mask_lt(a.hi, b.lo)),
    };
    ZB { val: t, known: t | f }
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
/// builds them freely - a guard is `g AND c` - so they have to lower.
#[inline(always)]
pub fn zb_and(a: ZB, b: ZB) -> ZB {
    let known_false = (a.known & !a.val) | (b.known & !b.val);
    ZB { val: a.val & b.val, known: (a.known & b.known) | known_false }
}

/// Tri-state OR, Kleene - the mirror of `zb_and`. Known where both are
/// known, and also where either is known TRUE.
#[inline(always)]
pub fn zb_or(a: ZB, b: ZB) -> ZB {
    let known_true = (a.known & a.val) | (b.known & b.val);
    ZB { val: a.val | b.val, known: (a.known & b.known) | known_true }
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
/// `tile_flag_at` with a PER-LANE box.
///
/// The box is block-uniform in almost every frame - a hitbox is fixed
/// per object type - but not in the frame an object is CREATED, because
/// `init_object` gives it a default 8x8 and `type.init` may or may not
/// replace it. Whether a given lane's object was just created is a
/// per-lane fact, so the width arrives as a select over it.
///
/// Slower than the uniform form on purpose: `CollisionCache::solid_map`
/// is keyed by (w, h), so a per-lane box cannot use it and every lane
/// takes the general path. Callers should use `zn_tile_flag_at` whenever
/// the box is uniform, which the emitter decides from the operands'
/// representation.
pub fn zn_tile_flag_at_lanes(
    cache: &CollisionCache,
    cart: &CartData,
    x: ZN,
    y: ZN,
    w: ZN,
    h: ZN,
    flag: P8,
) -> ZB {
    let f = flag.as_i16().expect("tile_flag_at: flag must be integer");
    if f != 0 {
        return zb_splat(false);
    }
    let mut val = 0u16;
    for i in 0..W {
        let xi = x[i].as_i16().expect("tile_flag_at: x must be an integer");
        let yi = y[i].as_i16().expect("tile_flag_at: y must be an integer");
        let wi = w[i].as_i16().expect("tile_flag_at: w must be an integer");
        let hi = h[i].as_i16().expect("tile_flag_at: h must be an integer");
        if cache.solid_at(cart, xi, yi, wi, hi).unwrap_or(false) {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

pub fn si_add(a: (P8, P8), b: (P8, P8)) -> (P8, P8) {
    let r = IV::new(a.0, a.1) + IV::new(b.0, b.1);
    (r.low, r.high)
}
#[inline(always)]
pub fn si_sub(a: (P8, P8), b: (P8, P8)) -> (P8, P8) {
    let r = IV::new(a.0, a.1) - IV::new(b.0, b.1);
    (r.low, r.high)
}

/// A slice-local set of row keys, for a kernel deduping its own output.
///
/// Open-addressed, because the key IS already a 128-bit hash: a
/// `HashSet` would hash it a second time, and std's SipHash costs far
/// more than the handful of column pushes the dedup exists to avoid.
/// Measured 2026-08-23: with `HashSet` the append phase went from 175 ms
/// to 279 ms - the dedup paid for itself downstream (boundary 138 -> 24
/// ms) and lost it all again at the door.
///
/// GENERATION-STAMPED rather than cleared. The set is per 16-lane slice,
/// and a room frame has hundreds of slices; allocating or zeroing a
/// table each time cost more than everything else put together
/// (`RowSet::new()` per slice was ~244 MB of allocation per frame, and
/// the append phase sat at 212 ms because of it). `next_slice` bumps a
/// counter instead, so a stale slot is simply one whose stamp is old.
///
/// Sized for one slice: at most 16 lanes x 64 assignments x 2^f fork
/// configurations, and in practice far fewer, so linear probing
/// terminates quickly. A full table degrades to "no dedup" rather than
/// to wrongness - `insert` reports NEW, which appends a row that would
/// have been a duplicate, and the boundary still removes it.
pub struct RowSet {
    slots: Vec<(u64, u64, u32)>,
    mask: usize,
    gen: u32,
}

impl Default for RowSet {
    fn default() -> Self {
        Self::new()
    }
}

impl RowSet {
    pub fn new() -> Self {
        RowSet { slots: vec![(0, 0, 0); 4096], mask: 4095, gen: 1 }
    }

    /// Forget everything, in O(1).
    #[inline(always)]
    pub fn next_slice(&mut self) {
        self.gen = self.gen.wrapping_add(1);
        if self.gen == 0 {
            self.slots.iter_mut().for_each(|s| s.2 = 0);
            self.gen = 1;
        }
    }

    /// True if `k` was not already present in this slice.
    #[inline(always)]
    pub fn insert(&mut self, k: (u64, u64)) -> bool {
        let mut i = (k.0 as usize) & self.mask;
        for _ in 0..8 {
            let s = self.slots[i];
            if s.2 != self.gen {
                self.slots[i] = (k.0, k.1, self.gen);
                return true;
            }
            if s.0 == k.0 && s.1 == k.1 {
                return false;
            }
            i = (i + 1) & self.mask;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A deterministic spread of raw 16.16 patterns: zero, both signs,
    /// the fractional boundaries `flr` cares about, and the extremes -
    /// because a comparison that only ever sees small positive numbers
    /// would not notice a sign or an overflow bug in either direction.
    fn spread() -> Vec<P8> {
        let mut v: Vec<P8> = vec![
            P8::from_raw(0),
            P8::from_raw(1),
            P8::from_raw(-1),
            P8::from_raw(0x0000_ffff),
            P8::from_raw(0x0001_0000),
            P8::from_raw(-0x0001_0000),
            P8::from_raw(i32::MAX),
            P8::from_raw(i32::MIN),
        ];
        // A few more, from a fixed LCG so the set is wide but the test is
        // not flaky.
        let mut s: u32 = 0x1234_5678;
        for _ in 0..24 {
            s = s.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            v.push(P8::from_raw(s as i32));
        }
        v
    }

    /// Sixteen lanes drawn from `spread` at a fixed stride, so successive
    /// columns are not correlated with each other.
    fn column(seed: usize) -> ZN {
        let s = spread();
        let mut o = [P8::from_i16(0); W];
        for i in 0..W {
            o[i] = s[(seed * 7 + i * 5) % s.len()];
        }
        o
    }

    /// The oracle, written independently of the primitives: build the mask
    /// one lane at a time out of `Pico8Num`'s own operators.
    fn oracle(a: ZN, b: ZN, f: impl Fn(P8, P8) -> bool) -> u16 {
        let mut m = 0u16;
        for i in 0..W {
            if f(a[i], b[i]) {
                m |= 1 << i;
            }
        }
        m
    }

    #[test]
    fn number_comparisons_match_a_scalar_oracle() {
        for seed in 0..64 {
            let (a, b) = (column(seed), column(seed + 31));
            assert_eq!(zn_lt(a, b).val, oracle(a, b, |x, y| x < y), "lt {}", seed);
            assert_eq!(zn_le(a, b).val, oracle(a, b, |x, y| x <= y), "le {}", seed);
            assert_eq!(zn_gt(a, b).val, oracle(a, b, |x, y| x > y), "gt {}", seed);
            assert_eq!(zn_ge(a, b).val, oracle(a, b, |x, y| x >= y), "ge {}", seed);
            assert_eq!(zn_eq(a, b).val, oracle(a, b, |x, y| x == y), "eq {}", seed);
            // `known` is unconditional for numbers; a regression that
            // dropped it would make every comparison unknown and deopt.
            assert_eq!(zn_lt(a, b).known, ALL);
        }
        // Same column against itself: every ordered predicate is decided
        // by reflexivity, which no random spread is likely to cover.
        for seed in 0..8 {
            let a = column(seed);
            assert_eq!(zn_lt(a, a).val, 0);
            assert_eq!(zn_gt(a, a).val, 0);
            assert_eq!(zn_le(a, a).val, ALL);
            assert_eq!(zn_ge(a, a).val, ALL);
            assert_eq!(zn_eq(a, a).val, ALL);
        }
    }

    /// The interval judge is now four number masks and an OR. Check it
    /// against the tri-state definition it replaced, spelled out here
    /// rather than shared with the implementation.
    #[test]
    fn interval_comparisons_match_the_tristate_definition() {
        let ops = [Cmp::Lt, Cmp::Le, Cmp::Gt, Cmp::Ge];
        for seed in 0..64 {
            // Build intervals that are genuinely ordered, and let some of
            // them be degenerate - a degenerate interval must give the
            // same answer as the plain number comparison.
            let (p, q) = (column(seed), column(seed + 13));
            let (r, s) = (column(seed + 29), column(seed + 41));
            let a = ZI { lo: zn_min(p, q), hi: zn_max(p, q) };
            let b = ZI { lo: zn_min(r, s), hi: zn_max(r, s) };
            for op in ops {
                let got = zi_cmp(op, a, b);
                let mut val = 0u16;
                let mut known = 0u16;
                for i in 0..W {
                    let (al, ah, bl, bh) = (a.lo[i], a.hi[i], b.lo[i], b.hi[i]);
                    // Enumerate the definition: the comparison is decided
                    // iff every pair drawn from the two intervals agrees.
                    let (t, f) = match op {
                        Cmp::Lt => (ah < bl, al >= bh),
                        Cmp::Le => (ah <= bl, al > bh),
                        Cmp::Gt => (al > bh, ah <= bl),
                        Cmp::Ge => (al >= bh, ah < bl),
                    };
                    assert!(!(t && f), "seed {} lane {}: both arms fired", seed, i);
                    if t {
                        val |= 1 << i;
                        known |= 1 << i;
                    } else if f {
                        known |= 1 << i;
                    }
                }
                assert_eq!(got.val, val, "seed {} val", seed);
                assert_eq!(got.known, known, "seed {} known", seed);
            }
        }
    }

    /// A degenerate interval must judge exactly like the number.
    #[test]
    fn degenerate_intervals_agree_with_numbers() {
        for seed in 0..32 {
            let (a, b) = (column(seed), column(seed + 17));
            let (ia, ib) = (zi_of_zn(a), zi_of_zn(b));
            assert_eq!(zi_cmp(Cmp::Lt, ia, ib).val, zn_lt(a, b).val);
            assert_eq!(zi_cmp(Cmp::Le, ia, ib).val, zn_le(a, b).val);
            assert_eq!(zi_cmp(Cmp::Gt, ia, ib).val, zn_gt(a, b).val);
            assert_eq!(zi_cmp(Cmp::Ge, ia, ib).val, zn_ge(a, b).val);
            for op in [Cmp::Lt, Cmp::Le, Cmp::Gt, Cmp::Ge] {
                assert_eq!(zi_cmp(op, ia, ib).known, ALL, "a point is always decided");
            }
        }
    }
}
