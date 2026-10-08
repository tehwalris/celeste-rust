//! The 16-lane primitives: the reference semantics the ASM codegen's ops
//! are checked against (`transpile::asm::tests`), the call-outs the
//! assembled kernels make (collisions, map reads), and the kernels' per-call
//! dedup cache (`RowCache`).
//!
//! Types are static; lanes are rows (W = 16 x i32 = one zmm). Each op must
//! agree bit-exactly with `Pico8Num`'s scalar operator (the tests check it).

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as IV};

// The whole lane layer is AVX-512.
use std::arch::x86_64::*;

pub const W: usize = 16;

pub use crate::runtime2::{cell_mix, mix64};

/// One num column: 16 `Pico8Num`s = 16 raw `i32`s = one zmm register.
///
/// A register, not `[Pico8Num; 16]`: an array forces the column to be
/// assembled and taken apart around every vector op. A primitive that drops
/// to `to_array` punches that hole, so it needs a reason and a measurement.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ZN(pub __m512i);

#[cfg(not(target_feature = "avx512f"))]
compile_error!(
    "celeste-engine needs AVX-512F/DQ. The workspace's .cargo/config.toml passes \
     `-C target-cpu=native`; on an older machine add \
     `-C target-feature=+avx512f,+avx512dq`."
);

impl ZN {
    /// SAFETY, both directions: `P8` is `#[repr(transparent)]` over `i32`,
    /// so `[P8; 16]` has the size and lane order of `__m512i`. These
    /// transmute values, not references, so alignment does not matter.
    #[inline(always)]
    pub fn from_array(a: [P8; W]) -> ZN {
        ZN(unsafe { std::mem::transmute(a) })
    }
    #[inline(always)]
    pub fn to_array(self) -> [P8; W] {
        unsafe { std::mem::transmute(self.0) }
    }
    /// One lane, for code that really works one row at a time. Not for
    /// arithmetic.
    #[inline(always)]
    pub fn lane(self, i: usize) -> P8 {
        self.to_array()[i]
    }
}

impl std::fmt::Debug for ZN {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ZN{:?}", self.to_array())
    }
}
impl PartialEq for ZN {
    fn eq(&self, other: &Self) -> bool {
        self.to_array() == other.to_array()
    }
}
impl Eq for ZN {}

/// One interval column slice: low/high planes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ZI {
    pub lo: ZN,
    pub hi: ZN,
}
/// One tri-state bool column slice. `val` is meaningful where `known` is
/// set. A pair of `u16` because 16 bits is an AVX-512 mask register: Kleene
/// ops are `kandw`s and `zsel_n` is one `vpblendmd`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ZB {
    pub val: u16,
    pub known: u16,
}

pub const ALL: u16 = 0xffff;

#[inline(always)]
fn m512(v: i32) -> __m512i {
    unsafe { _mm512_set1_epi32(v) }
}

#[inline(always)]
pub fn zn_splat(v: P8) -> ZN {
    ZN(m512(v.as_raw_u32() as i32))
}

// ---- 16-lane PICO-8 arithmetic ----
//
// A `Pico8Num` is its raw `i32` in 16.16 fixed point, so add, sub, min
// and max are the plain integer ops: the scale cancels.

#[inline(always)]
pub fn zn_add(a: ZN, b: ZN) -> ZN {
    ZN(unsafe { _mm512_add_epi32(a.0, b.0) })
}
#[inline(always)]
pub fn zn_sub(a: ZN, b: ZN) -> ZN {
    ZN(unsafe { _mm512_sub_epi32(a.0, b.0) })
}
#[inline(always)]
pub fn zn_min(a: ZN, b: ZN) -> ZN {
    ZN(unsafe { _mm512_min_epi32(a.0, b.0) })
}
#[inline(always)]
pub fn zn_max(a: ZN, b: ZN) -> ZN {
    ZN(unsafe { _mm512_max_epi32(a.0, b.0) })
}
#[inline(always)]
pub fn zn_neg(a: ZN) -> ZN {
    ZN(unsafe { _mm512_sub_epi32(_mm512_setzero_si512(), a.0) })
}
#[inline(always)]
pub fn zn_abs(a: ZN) -> ZN {
    ZN(unsafe { _mm512_abs_epi32(a.0) })
}

/// `flr`: `P8::flr` is `((x >> 16) as i16 as i32) << 16`, and the `as i16`
/// loses nothing after an arithmetic `>> 16`, so it is `x & 0xffff_0000`.
#[inline(always)]
pub fn zn_flr(a: ZN) -> ZN {
    ZN(unsafe { _mm512_and_si512(a.0, m512(0xffff_0000u32 as i32)) })
}

/// 16.16 multiply: `(a as i64 * b as i64) >> 16`, truncated to i32.
///
/// The even/odd split: `vpmuldq` multiplies the sign-extended low i32 of
/// each 64-bit element (the even lanes); an ARITHMETIC `>> 32` brings the
/// odd lanes down for the second multiply. Both products are shifted by 16
/// and the odd ones blended back into the odd slots.
#[inline(always)]
pub fn zn_mul(a: ZN, b: ZN) -> ZN {
    unsafe {
        let ev = _mm512_srai_epi64::<16>(_mm512_mul_epi32(a.0, b.0));
        let od = _mm512_srai_epi64::<16>(_mm512_mul_epi32(
            _mm512_srai_epi64::<32>(a.0),
            _mm512_srai_epi64::<32>(b.0),
        ));
        ZN(_mm512_mask_blend_epi32(0xaaaa, ev, _mm512_slli_epi64::<32>(od)))
    }
}

/// Divide and remainder go out to scalar (x86 has no integer vector
/// divide). A vector version through `f64` plus a rounding correction is
/// possible; the sites are rare enough that it has not been worth it.
#[inline(always)]
pub fn zn_div(a: ZN, b: ZN) -> ZN {
    let (x, y) = (a.to_array(), b.to_array());
    ZN::from_array(std::array::from_fn(|i| x[i] / y[i]))
}
#[inline(always)]
pub fn zn_rem(a: ZN, b: ZN) -> ZN {
    let (x, y) = (a.to_array(), b.to_array());
    ZN::from_array(std::array::from_fn(|i| x[i] % y[i]))
}
/// `sin`, per lane in scalar (rare in the kernels).
#[inline(always)]
pub fn zn_sin(a: ZN) -> ZN {
    let x = a.to_array();
    ZN::from_array(std::array::from_fn(|i| x[i].pico8_sin()))
}

// ---- interval ops ----
//
// Componentwise on the two planes: the interval rules are selections
// between endpoints, never a per-lane branch.
//
// An endpoint must never wrap silently (a widened interval is exactly what
// can run away). `Pico8NumInterval`'s `Add`/`Sub` panic when a result leaves
// `i32`, so each arm computes the signed-overflow predicate and hands the
// whole column to the scalar implementation if any lane trips it.

/// Signed 32-bit add-overflow, per lane: the sign of both operands
/// differs from the sign of the result. `((a^r) & (b^r)) < 0`.
#[inline(always)]
fn add_overflows(a: __m512i, b: __m512i, r: __m512i) -> u16 {
    unsafe {
        let bad = _mm512_and_si512(_mm512_xor_si512(a, r), _mm512_xor_si512(b, r));
        _mm512_cmplt_epi32_mask(bad, _mm512_setzero_si512())
    }
}
/// Signed 32-bit sub-overflow: `((a^b) & (a^r)) < 0`.
#[inline(always)]
fn sub_overflows(a: __m512i, b: __m512i, r: __m512i) -> u16 {
    unsafe {
        let bad = _mm512_and_si512(_mm512_xor_si512(a, b), _mm512_xor_si512(a, r));
        _mm512_cmplt_epi32_mask(bad, _mm512_setzero_si512())
    }
}

/// The scalar interval implementation, per lane: reached only when a vector
/// arm detects a wrap, for its panic.
#[inline(always)]
fn zi_scalar(a: ZI, b: ZI, f: impl Fn(IV, IV) -> IV) -> ZI {
    let (alo, ahi) = (a.lo.to_array(), a.hi.to_array());
    let (blo, bhi) = (b.lo.to_array(), b.hi.to_array());
    let mut lo = [P8::from_i16(0); W];
    let mut hi = [P8::from_i16(0); W];
    for i in 0..W {
        let r = f(IV::new(alo[i], ahi[i]), IV::new(blo[i], bhi[i]));
        lo[i] = r.low;
        hi[i] = r.high;
    }
    ZI { lo: ZN::from_array(lo), hi: ZN::from_array(hi) }
}

/// The lanes where an interval `+` overflows an endpoint: the assembled
/// kernel's error for it (`Op::NoWrap`) and the guard `zi_add` panics on.
#[inline(always)]
pub fn zi_add_wraps(a: ZI, b: ZI) -> u16 {
    unsafe {
        let lo = _mm512_add_epi32(a.lo.0, b.lo.0);
        let hi = _mm512_add_epi32(a.hi.0, b.hi.0);
        add_overflows(a.lo.0, b.lo.0, lo) | add_overflows(a.hi.0, b.hi.0, hi)
    }
}
/// `zi_add_wraps` for `-` (`[a.lo - b.hi, a.hi - b.lo]`).
#[inline(always)]
pub fn zi_sub_wraps(a: ZI, b: ZI) -> u16 {
    unsafe {
        let lo = _mm512_sub_epi32(a.lo.0, b.hi.0);
        let hi = _mm512_sub_epi32(a.hi.0, b.lo.0);
        sub_overflows(a.lo.0, b.hi.0, lo) | sub_overflows(a.hi.0, b.lo.0, hi)
    }
}
/// `zi_add_wraps` for negation: an endpoint at `MIN`.
#[inline(always)]
pub fn zi_neg_wraps(a: ZI) -> u16 {
    unsafe {
        let min = _mm512_set1_epi32(i32::MIN);
        _mm512_cmpeq_epi32_mask(a.lo.0, min) | _mm512_cmpeq_epi32_mask(a.hi.0, min)
    }
}

#[inline(always)]
pub fn zi_add(a: ZI, b: ZI) -> ZI {
    if zi_add_wraps(a, b) != 0 {
        // The scalar `+` panics on the wrap: never silently.
        return zi_scalar(a, b, |x, y| x + y);
    }
    unsafe { ZI { lo: ZN(_mm512_add_epi32(a.lo.0, b.lo.0)), hi: ZN(_mm512_add_epi32(a.hi.0, b.hi.0)) } }
}
#[inline(always)]
pub fn zi_sub(a: ZI, b: ZI) -> ZI {
    // `[a.low - b.high, a.high - b.low]`: the endpoints cross.
    if zi_sub_wraps(a, b) != 0 {
        return zi_scalar(a, b, |x, y| x - y);
    }
    unsafe { ZI { lo: ZN(_mm512_sub_epi32(a.lo.0, b.hi.0)), hi: ZN(_mm512_sub_epi32(a.hi.0, b.lo.0)) } }
}
#[inline(always)]
pub fn zi_min(a: ZI, b: ZI) -> ZI {
    ZI { lo: zn_min(a.lo, b.lo), hi: zn_min(a.hi, b.hi) }
}
#[inline(always)]
pub fn zi_max(a: ZI, b: ZI) -> ZI {
    ZI { lo: zn_max(a.lo, b.lo), hi: zn_max(a.hi, b.hi) }
}

#[inline(always)]
pub fn zi_neg(a: ZI) -> ZI {
    // `-MIN` wraps to `MIN`: panic, as `zi_add` / `zi_sub` do. The
    // assembled kernel declines such a lane instead (`Op::NoWrap`).
    if zi_neg_wraps(a) != 0 {
        return zi_scalar(a, a, |x, _| x.checked_neg().unwrap_or_else(|| panic!("interval negation wraps: -[{:?}, {:?}]", x.low, x.high)));
    }
    ZI { lo: zn_neg(a.hi), hi: zn_neg(a.lo) }
}

/// `abs` of an interval, branch-free: unchanged if non-negative, reflected
/// if non-positive, else `[0, max(|lo|, |hi|)]`. The cases partition the
/// lanes, so each is a mask and the result is blends.
#[inline(always)]
pub fn zi_abs(a: ZI) -> ZI {
    unsafe {
        let zero = _mm512_setzero_si512();
        let pos = _mm512_cmpge_epi32_mask(a.lo.0, zero);
        let neg = _mm512_cmple_epi32_mask(a.hi.0, zero);
        let (al, ah) = (_mm512_abs_epi32(a.lo.0), _mm512_abs_epi32(a.hi.0));
        // Straddling is the fallback; blend the decided cases over it.
        let mut lo = zero;
        let mut hi = _mm512_max_epi32(al, ah);
        lo = _mm512_mask_blend_epi32(neg, lo, ah);
        hi = _mm512_mask_blend_epi32(neg, hi, al);
        lo = _mm512_mask_blend_epi32(pos, lo, a.lo.0);
        hi = _mm512_mask_blend_epi32(pos, hi, a.hi.0);
        ZI { lo: ZN(lo), hi: ZN(hi) }
    }
}

/// Interval `flr`. That the floor is unique is a premise, so this takes the
/// low endpoint's floor - the floor on every lane that survives it.
#[inline(always)]
pub fn zi_flr(a: ZI) -> ZN {
    zn_flr(a.lo)
}

/// The premise `zi_fork_flr` is taken under: the lane's interval spans at
/// most `ways` floors, so the fork's fragments cover it.
#[inline(always)]
pub fn zi_span_ok(a: ZI, ways: u8) -> ZB {
    let (fl, fh) = (zn_flr(a.lo), zn_flr(a.hi));
    // The high end's floor within `ways - 1` of the low end's.
    let top = zn_add(fl, zn_splat(P8::from_raw(STEP * (ways as i32 - 1))));
    ZB { val: mask_le(fh, top), known: ALL }
}

/// One fork-grid step (the integers) in raw 16.16 units.
const STEP: i32 = 1 << 16;

// ---- comparisons ----
//
// `Pico8Num` orders by its raw `i32`, so a comparison is one signed
// `vpcmpd`, which yields exactly the 16-bit mask `ZB` wants.

/// Defines `$name`, the `ZB`-returning primitive the kernels call, and
/// `$mask`, the raw lane mask `zi_cmp` and the interval premises
/// combine. `$imm` is the `vpcmpd` predicate: EQ 0, LT 1, LE 2,
/// NLT (`>=`) 5, NLE (`>`) 6.
macro_rules! zn_cmp {
    ($name:ident, $mask:ident, $imm:expr) => {
        #[inline(always)]
        fn $mask(a: ZN, b: ZN) -> u16 {
            unsafe { _mm512_cmp_epi32_mask::<{ $imm }>(a.0, b.0) }
        }
        #[inline(always)]
        pub fn $name(a: ZN, b: ZN) -> ZB {
            ZB { val: $mask(a, b), known: ALL }
        }
    };
}
zn_cmp!(zn_lt, mask_lt, 1);
zn_cmp!(zn_le, mask_le, 2);
zn_cmp!(zn_gt, mask_gt, 6);
zn_cmp!(zn_ge, mask_ge, 5);
zn_cmp!(zn_eq, mask_eq, 0);

/// The comparison the tri-state interval judge is performing.
#[derive(Clone, Copy)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
}

/// The tri-state interval comparison, per lane (degenerate intervals answer
/// as plain numbers).
///
/// `t` is "definitely true", `f` "definitely false", neither is unknown.
/// They are disjoint (for `Lt`, both would give `bh <= al <= ah < bl <=
/// bh`), so `val = t` needs no masking.
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

/// Interval equality, `Graph::compare`'s rule: decided false where the
/// boxes are disjoint, decided (to the lows' equality) where both are
/// singletons, unknown otherwise.
#[inline(always)]
pub fn zi_eq(a: ZI, b: ZI) -> ZB {
    let both = mask_eq(a.lo, a.hi) & mask_eq(b.lo, b.hi);
    let val = both & mask_eq(a.lo, b.lo);
    let disjoint = mask_gt(a.lo, b.hi) | mask_gt(b.lo, a.hi);
    ZB { val, known: both | disjoint }
}


// ---- bool ops ----

#[inline(always)]
pub fn zb_not(a: ZB) -> ZB {
    ZB { val: !a.val, known: a.known }
}
/// Tri-state AND, Kleene: known where both are known, or either is known
/// false. Must match `Graph::fold`'s rule for `Op::And`, or a folded graph
/// and an emitted one answer differently.
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

/// Bool equality: known where both are known.
#[inline(always)]
pub fn zb_eq(a: ZB, b: ZB) -> ZB {
    ZB { val: !(a.val ^ b.val), known: a.known & b.known }
}

// ---- select ----

/// Per-lane blend, one `vpblendmd`. That the condition is decided is a
/// premise (`Known(c)`), so an undecided lane blends as if false and is
/// filtered out downstream.
#[inline(always)]
pub fn zsel_n(c: ZB, t: ZN, f: ZN) -> ZN {
    ZN(unsafe { _mm512_mask_blend_epi32(c.val, f.0, t.0) })
}
#[inline(always)]
pub fn zsel_i(c: ZB, t: ZI, f: ZI) -> ZI {
    ZI { lo: zsel_n(c, t.lo, f.lo), hi: zsel_n(c, t.hi, f.hi) }
}
#[inline(always)]
pub fn zsel_b(c: ZB, t: ZB, f: ZB) -> ZB {
    ZB {
        val: (c.val & t.val) | (!c.val & f.val),
        known: (c.val & t.known) | (!c.val & f.known),
    }
}

// ---- refinement splits ----

/// `__split_by_flr` as an n-way fork. Fragment `c` is the `c`-th floor
/// from the low end's, clipped to the lane; fragment 0 is never empty.
/// Returns (fragment `c` per lane, valid mask); a lane with an empty
/// fragment produces no row in this fork configuration. The fragments of a
/// fork of arity `n` partition every lane spanning at most `n` floors; a
/// wider lane is what `zi_span_ok` (the premise) takes off the kernel.
#[inline(always)]
pub fn zi_fork_flr(a: ZI, c: usize) -> (ZI, u16) {
    let (fl, fh) = (zn_flr(a.lo), zn_flr(a.hi));
    // Valid iff the interval reaches the `c`-th floor.
    let base = zn_add(fl, zn_splat(P8::from_raw(STEP * c as i32)));
    let top = zn_add(base, zn_splat(P8::from_raw(STEP - 1)));
    let valid = if c == 0 { ALL } else { mask_le(base, fh) };
    (ZI { lo: zn_max(a.lo, base), hi: zn_min(a.hi, top) }, valid)
}

// ---- cart / collision builtins (per-lane; x/y vary, w/h/flag uniform) ----

/// mget, per lane: 0 outside the map (as PICO-8; a branch-free kernel reads
/// there on lanes that do not take the iteration); a fractional coordinate
/// panics (the traced cart never makes one).
#[inline(always)]
pub fn zn_mget(cart: &CartData, x: ZN, y: ZN) -> ZN {
    let (x, y) = (x.to_array(), y.to_array());
    let mut o = [P8::from_i16(0); W];
    for i in 0..W {
        let xi = x[i].as_i16().expect("mget: x is not an integer");
        let yi = y[i].as_i16().expect("mget: y is not an integer");
        o[i] = P8::from_i16(cart.mget_whole(xi, yi) as i16);
    }
    ZN::from_array(o)
}

/// tile_flag_at with uniform w/h/flag: the precomputed map for the flag and
/// box where there is one (`CollisionCache::flag_map`), else the scan.
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
    let wi = w.as_i16().expect("tile_flag_at: w");
    let hi = h.as_i16().expect("tile_flag_at: h");
    let mut val = 0u16;
    let map = cache.flag_map(wi, hi, f);
    let (x, y) = (x.to_array(), y.to_array());
    for i in 0..W {
        let xi = x[i].as_i16().expect("tile_flag_at: x must be an integer");
        let yi = y[i].as_i16().expect("tile_flag_at: y must be an integer");
        let b = match &map {
            Some((map, dx, dy)) => match map.get(xi + dx, yi + dy) {
                Some(v) => v,
                None => cache.flag_at(cart, xi, yi, wi, hi, f).expect("tile_flag_at"),
            },
            None => cache.flag_at(cart, xi, yi, wi, hi, f).expect("tile_flag_at"),
        };
        if b {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

// ---- per-lane box ----

#[inline(always)]
/// `tile_flag_at` with a per-lane box: in the frame an object is created
/// its hitbox depends on whether `type.init` replaced the default 8x8.
/// Always takes the scan (the precomputed maps are keyed by the box); use
/// `zn_tile_flag_at` when the box is uniform.
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
    let mut val = 0u16;
    let (x, y, w, h) = (x.to_array(), y.to_array(), w.to_array(), h.to_array());
    for i in 0..W {
        let xi = x[i].as_i16().expect("tile_flag_at: x must be an integer");
        let yi = y[i].as_i16().expect("tile_flag_at: y must be an integer");
        let wi = w[i].as_i16().expect("tile_flag_at: w must be an integer");
        let hi = h[i].as_i16().expect("tile_flag_at: h must be an integer");
        if cache.flag_at(cart, xi, yi, wi, hi, f).expect("tile_flag_at") {
            val |= 1 << i;
        }
    }
    ZB { val, known: ALL }
}

/// The within-call dedup cache, keyed by the row key, with one `u32` tag
/// per key: what the row's first emission learned that a re-emission needs,
/// so the re-emission materializes nothing (`compiled::asm_kernel`).
///
/// A cache, not a set: fixed capacity, a short probe, a colliding new key
/// evicts. Duplicates come from neighbouring input lanes (inputs are sorted
/// by cell), so a bounded L2-resident table catches most; a miss reaches
/// the door, which is the dedup of record.
pub struct RowCache {
    /// `(key.1, ref, generation, tag)`; the slot index comes from `key.0`.
    /// The ref is the queued row, or once flushed the state's id
    /// (`ID_FLAG`), so a later emission of the key can record its edge.
    slots: Vec<(u64, u64, u32, u32)>,
    mask: usize,
    gen: u32,
}

impl Default for RowCache {
    fn default() -> Self {
        Self::new()
    }
}

impl RowCache {
    /// 32k entries of 24 bytes: 768 KB, most of a core's L2.
    pub const CAPACITY: usize = 1 << 15;
    const PROBES: usize = 4;

    pub fn new() -> Self {
        RowCache { slots: vec![(0, 0, 0, 0); Self::CAPACITY], mask: Self::CAPACITY - 1, gen: 1 }
    }

    /// The ref of a flushed row: the state's id, flagged.
    pub const ID_FLAG: u64 = 1 << 63;
    /// The ref of a row a filter dropped: no edge to record; the low 32 bits
    /// are the smallest horizon level -1 would admit it at (a raise's note).
    pub const DROP_FLAG: u64 = 1 << 62;

    /// Forget every key (O(1): bumps the generation).
    pub fn clear(&mut self) {
        self.gen = self.gen.wrapping_add(1);
        if self.gen == 0 {
            self.slots.iter_mut().for_each(|s| s.1 = 0);
            self.gen = 1;
        }
    }


    /// Set the ref of `k`'s entry. A miss (evicted since) is fine: the
    /// next emission of `k` will push the row again.
    #[inline(always)]
    pub fn set_ref(&mut self, k: (u64, u64), r: u64) {
        let base = k.0 as usize;
        for p in 0..Self::PROBES {
            let i = (base + p) & self.mask;
            let s = &mut self.slots[i];
            if s.2 == self.gen && s.0 == k.1 {
                s.1 = r;
                return;
            }
        }
    }

    /// Insert `k` with `tag` and ref `r`: `None` if it was not present (it
    /// is now, or it evicted the first slot of its probe window), `Some((tag,
    /// ref) of the first insert)` if it was.
    #[inline(always)]
    pub fn insert_ref(&mut self, k: (u64, u64), tag: u32, r: u64) -> Option<(u32, u64)> {
        let base = k.0 as usize;
        let mut victim = base & self.mask;
        for p in 0..Self::PROBES {
            let i = (base + p) & self.mask;
            let s = self.slots[i];
            if s.2 != self.gen {
                victim = i;
                break;
            }
            if s.0 == k.1 {
                return Some((s.3, s.1));
            }
        }
        self.slots[victim] = (k.1, r, self.gen, tag);
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A deterministic spread of raw 16.16 patterns: zero, both signs, the
    /// fractional boundaries `flr` cares about, and the extremes.
    fn spread() -> Vec<P8> {
        let mut v: Vec<P8> = vec![
            P8::from_raw(0),
            P8::from_raw(1),
            P8::from_raw(-1),
            P8::from_raw(0x0000_ffff),
            P8::from_raw(0x0001_0000),
            P8::from_raw(-0x0001_0000),
            P8::from_raw(0x0001_8000),
            P8::from_raw(-0x0001_8000),
            P8::from_raw(i32::MAX),
            P8::from_raw(i32::MIN),
        ];
        let mut s: u32 = 0x1234_5678;
        for _ in 0..40 {
            s = s.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            v.push(P8::from_raw(s as i32));
            // Small values too: the game's are mostly in [-256, 256].
            v.push(P8::from_i16((s % 512) as i16 - 256));
        }
        v
    }

    fn column(seed: usize) -> ZN {
        let s = spread();
        ZN::from_array(std::array::from_fn(|i| s[(seed * 7 + i * 5) % s.len()]))
    }

    /// A column of small values, for the arms that panic on overflow.
    fn small(seed: usize) -> ZN {
        let mut s = (seed as u32).wrapping_mul(2_654_435_761).wrapping_add(1);
        ZN::from_array(std::array::from_fn(|_| {
            s = s.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            P8::from_raw(((s >> 8) as i32 % 0x0080_0000) - 0x0040_0000)
        }))
    }

    fn ival(seed: usize) -> ZI {
        let (a, b) = (small(seed), small(seed + 977));
        ZI { lo: zn_min(a, b), hi: zn_max(a, b) }
    }

    /// The oracle for every `zn_*` arm: the scalar operator, per lane.
    fn ref2(a: ZN, b: ZN, f: impl Fn(P8, P8) -> P8) -> ZN {
        let (x, y) = (a.to_array(), b.to_array());
        ZN::from_array(std::array::from_fn(|i| f(x[i], y[i])))
    }
    fn ref1(a: ZN, f: impl Fn(P8) -> P8) -> ZN {
        let x = a.to_array();
        ZN::from_array(std::array::from_fn(|i| f(x[i])))
    }
    fn refm(a: ZN, b: ZN, f: impl Fn(P8, P8) -> bool) -> u16 {
        let (x, y) = (a.to_array(), b.to_array());
        let mut m = 0u16;
        for i in 0..W {
            if f(x[i], y[i]) {
                m |= 1 << i;
            }
        }
        m
    }

    #[test]
    fn vector_arithmetic_matches_the_scalar_operators() {
        for seed in 0..96 {
            let (a, b) = (small(seed), small(seed + 31));
            assert_eq!(zn_add(a, b), ref2(a, b, |x, y| x + y), "add {}", seed);
            assert_eq!(zn_sub(a, b), ref2(a, b, |x, y| x - y), "sub {}", seed);
            assert_eq!(zn_mul(a, b), ref2(a, b, |x, y| x * y), "mul {}", seed);
            // The total ops get the wide spread, including i32::MIN.
            let (a, b) = (column(seed), column(seed + 31));
            assert_eq!(zn_min(a, b), ref2(a, b, |x, y| x.min(y)), "min {}", seed);
            assert_eq!(zn_max(a, b), ref2(a, b, |x, y| x.max(y)), "max {}", seed);
            assert_eq!(zn_flr(a), ref1(a, |x| x.flr()), "flr {}", seed);
            assert_eq!(zn_div(a, b), ref2(a, b, |x, y| x / y), "div {}", seed);
            assert_eq!(zn_rem(a, b), ref2(a, b, |x, y| x % y), "rem {}", seed);
        }
    }

    /// The even/odd weave of `zn_mul`: every lane a different value, so a
    /// swap shows.
    #[test]
    fn the_multiply_weave_puts_every_lane_back_where_it_came_from() {
        let a = ZN::from_array(std::array::from_fn(|i| P8::from_i16(i as i16 + 1)));
        let b = ZN::from_array(std::array::from_fn(|i| P8::from_i16(100 - i as i16)));
        assert_eq!(zn_mul(a, b), ref2(a, b, |x, y| x * y));
        // Negatives in alternating lanes: the odd-lane shift must be
        // arithmetic.
        let c = ZN::from_array(std::array::from_fn(|i| {
            P8::from_i16(if i % 2 == 0 { -(i as i16) - 1 } else { i as i16 + 1 })
        }));
        assert_eq!(zn_mul(c, b), ref2(c, b, |x, y| x * y));
        assert_eq!(zn_mul(b, c), ref2(b, c, |x, y| x * y));
        assert_eq!(zn_mul(c, c), ref2(c, c, |x, y| x * y));
    }

    #[test]
    fn vector_comparisons_match_the_scalar_operators() {
        for seed in 0..96 {
            let (a, b) = (column(seed), column(seed + 31));
            assert_eq!(zn_lt(a, b).val, refm(a, b, |x, y| x < y), "lt {}", seed);
            assert_eq!(zn_le(a, b).val, refm(a, b, |x, y| x <= y), "le {}", seed);
            assert_eq!(zn_gt(a, b).val, refm(a, b, |x, y| x > y), "gt {}", seed);
            assert_eq!(zn_ge(a, b).val, refm(a, b, |x, y| x >= y), "ge {}", seed);
            assert_eq!(zn_eq(a, b).val, refm(a, b, |x, y| x == y), "eq {}", seed);
            assert_eq!(zn_lt(a, b).known, ALL);
            assert_eq!(zn_lt(a, a).val, 0, "nothing is less than itself");
            assert_eq!(zn_le(a, a).val, ALL);
            assert_eq!(zn_eq(a, a).val, ALL);
        }
    }

    #[test]
    fn the_blend_takes_each_lane_from_the_side_the_mask_says() {
        for seed in 0..64 {
            let (t, f) = (column(seed), column(seed + 13));
            for m in [0u16, ALL, 0x5555, 0xaaaa, 0x00ff, 0xf0f0, 1, 0x8000] {
                let c = ZB { val: m, known: ALL };
                let want = ZN::from_array(std::array::from_fn(|i| {
                    if m & (1 << i) != 0 { t.lane(i) } else { f.lane(i) }
                }));
                assert_eq!(zsel_n(c, t, f), want, "seed {} mask {:04x}", seed, m);
            }
        }
    }

    /// The interval arms against the `Pico8NumInterval` operators. Small
    /// operands: the wrap path has its own tests.
    #[test]
    fn vector_interval_ops_match_the_interval_operators() {
        for seed in 0..96 {
            let (a, b) = (ival(seed), ival(seed + 313));
            let sc = |f: &dyn Fn(IV, IV) -> IV| -> ZI {
                let (al, ah) = (a.lo.to_array(), a.hi.to_array());
                let (bl, bh) = (b.lo.to_array(), b.hi.to_array());
                let mut lo = [P8::from_i16(0); W];
                let mut hi = [P8::from_i16(0); W];
                for i in 0..W {
                    let r = f(IV::new(al[i], ah[i]), IV::new(bl[i], bh[i]));
                    lo[i] = r.low;
                    hi[i] = r.high;
                }
                ZI { lo: ZN::from_array(lo), hi: ZN::from_array(hi) }
            };
            assert_eq!(zi_add(a, b), sc(&|x, y| x + y), "zi_add {}", seed);
            assert_eq!(zi_sub(a, b), sc(&|x, y| x - y), "zi_sub {}", seed);
            assert_eq!(
                zi_min(a, b),
                sc(&|x, y| IV::new(x.low.min(y.low), x.high.min(y.high))),
                "zi_min {}", seed
            );
            assert_eq!(
                zi_max(a, b),
                sc(&|x, y| IV::new(x.low.max(y.low), x.high.max(y.high))),
                "zi_max {}", seed
            );
            // abs, against the three-case definition spelled out.
            let zero = P8::from_i16(0);
            let (al, ah) = (a.lo.to_array(), a.hi.to_array());
            let mut lo = [zero; W];
            let mut hi = [zero; W];
            for i in 0..W {
                let (l, h) = (al[i], ah[i]);
                let r = if l >= zero {
                    IV::new(l, h)
                } else if h <= zero {
                    IV::new(h.abs(), l.abs())
                } else {
                    IV::new(zero, l.abs().max(h.abs()))
                };
                lo[i] = r.low;
                hi[i] = r.high;
            }
            assert_eq!(
                zi_abs(a),
                ZI { lo: ZN::from_array(lo), hi: ZN::from_array(hi) },
                "zi_abs {}", seed
            );
            assert_eq!(zi_neg(a), ZI { lo: zn_neg(a.hi), hi: zn_neg(a.lo) }, "zi_neg {}", seed);
        }
    }

    /// `zi_add` must panic on a wrap, not wrap silently.
    #[test]
    #[should_panic]
    fn an_interval_add_that_wraps_still_panics() {
        let big = zn_splat(P8::from_raw(i32::MAX));
        let a = ZI { lo: big, hi: big };
        let _ = zi_add(a, a);
    }

    /// `zi_sub` too: the whole range minus 1.
    #[test]
    #[should_panic]
    fn an_interval_sub_that_wraps_still_panics() {
        let (min, max) = (zn_splat(P8::from_raw(i32::MIN)), zn_splat(P8::from_raw(i32::MAX)));
        let one = zn_splat(P8::from_i16(1));
        let _ = zi_sub(ZI { lo: min, hi: max }, ZI { lo: one, hi: one });
    }

    /// And negation, which wraps on exactly one endpoint value: `MIN`.
    #[test]
    #[should_panic]
    fn an_interval_negation_that_wraps_still_panics() {
        let (min, zero) = (zn_splat(P8::from_raw(i32::MIN)), zn_splat(P8::from_i16(0)));
        let _ = zi_neg(ZI { lo: min, hi: zero });
    }

    /// Round-tripping a column through an array is the identity, and
    /// `lane` agrees with it.
    #[test]
    fn a_column_survives_a_round_trip_through_an_array() {
        for seed in 0..64 {
            let a = column(seed);
            assert_eq!(ZN::from_array(a.to_array()), a);
            for i in 0..W {
                assert_eq!(a.lane(i), a.to_array()[i]);
            }
        }
        let s = spread();
        for v in s {
            assert_eq!(zn_splat(v).to_array(), [v; W]);
        }
    }

    /// The interval comparison against the tri-state definition, spelled
    /// out here rather than shared with the implementation.
    #[test]
    fn interval_comparisons_match_the_tristate_definition() {
        let ops = [Cmp::Lt, Cmp::Le, Cmp::Gt, Cmp::Ge];
        for seed in 0..64 {
            // Ordered intervals, some degenerate.
            let (p, q) = (column(seed), column(seed + 13));
            let (r, s) = (column(seed + 29), column(seed + 41));
            let a = ZI { lo: zn_min(p, q), hi: zn_max(p, q) };
            let b = ZI { lo: zn_min(r, s), hi: zn_max(r, s) };
            for op in ops {
                let got = zi_cmp(op, a, b);
                let mut val = 0u16;
                let mut known = 0u16;
                for i in 0..W {
                    let (al, ah) = (a.lo.lane(i), a.hi.lane(i));
                    let (bl, bh) = (b.lo.lane(i), b.hi.lane(i));
                    // Decided iff every pair from the two intervals agrees.
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

}
