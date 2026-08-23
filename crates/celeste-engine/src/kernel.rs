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

// The whole lane layer is AVX-512; see `ZN`'s doc for why the
// representation is a register and not an array.
use std::arch::x86_64::*;

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

/// One num column: 16 `Pico8Num`s, which is 16 raw `i32`s, which is one
/// zmm register.
///
/// A REGISTER, not `[Pico8Num; 16]`, and that is the single most
/// consequential line in this file. Measured 2026-08-23 on the array
/// version: `kernel1::frame` was 163,098 instructions of which **61%
/// were scalar** - 34,362 scalar `mov`, 11,804 scalar `imul`, 2,514
/// `vpextrd` whose only job is pulling one lane out of a vector - in a
/// function where every value is a 16-lane column. Writing the
/// primitives as `for i in 0..W` loops over scalar operators and
/// trusting the loop vectorizer got 39% of the way, per site and
/// unpredictably.
///
/// It cannot be fixed one primitive at a time: forcing a single one
/// (`zn_lt`) to AVX-512 while its neighbours stayed arrays was 27%
/// SLOWER, because the column then had to be assembled and taken apart
/// around it. On a synthetic with this kernel's op mix and live-set
/// width, changing only the lane type was 20x on instruction count,
/// runtime AND build time, for the same checksum. See
/// `plans/tracing.md`.
///
/// The consequence for anything added here: a primitive that drops to
/// `to_array` punches a hole in exactly the same way, so it needs a
/// reason and a measurement, not a shrug.
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
    /// SAFETY, both directions: `P8` is `#[repr(transparent)]` over
    /// `i32` (its own doc says so, and says it is for this), so
    /// `[P8; 16]` is 16 contiguous `i32`s - 512 bits, same size and same
    /// lane order as `__m512i`. Alignment does not enter into it: these
    /// transmute VALUES, not references.
    #[inline(always)]
    pub fn from_array(a: [P8; W]) -> ZN {
        ZN(unsafe { std::mem::transmute(a) })
    }
    #[inline(always)]
    pub fn to_array(self) -> [P8; W] {
        unsafe { std::mem::transmute(self.0) }
    }
    /// One lane, for `append` and the row-key walk - which really do
    /// write out one row at a time. NOT for arithmetic: that is the hole
    /// described above.
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
/// set; an unknown lane deopts at any use that needs the value.
///
/// STILL a pair of `u16`, deliberately: 16 bits IS an AVX-512 mask
/// register, a Kleene AND really is two `kandw`, and `zsel_n` is one
/// `vpblendmd` precisely because `val` is already in mask form. The
/// codegen census found the boolean layer was never the problem.
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
#[inline(always)]
pub fn zi_splat(lo: P8, hi: P8) -> ZI {
    ZI { lo: zn_splat(lo), hi: zn_splat(hi) }
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
/// TWO registers, because 16 lanes of `u64` is 1024 bits. Lanes 0-7 in
/// `a`, 8-15 in `b`.
///
/// The ONLY lane type here that is not an abstract game value. It exists
/// so the fold can be part of the graph instead of a scalar loop inside
/// `append`. The fold stays sequential over CELLS - each step depends on
/// the last - while every step is 16 lanes wide, and the lanes are the
/// axis the parallelism actually lives on. Measured 2026-08-23, the
/// scalar-per-lane version was 75% of ALL kernel time.
#[derive(Clone, Copy)]
pub struct ZW(pub __m512i, pub __m512i);

impl ZW {
    #[inline(always)]
    pub fn from_array(a: [u64; W]) -> ZW {
        unsafe {
            let p = a.as_ptr().cast();
            ZW(
                _mm512_loadu_si512(p),
                _mm512_loadu_si512(p.wrapping_add(1)),
            )
        }
    }
    #[inline(always)]
    pub fn to_array(self) -> [u64; W] {
        let mut o = [0u64; W];
        unsafe {
            let p = o.as_mut_ptr().cast();
            _mm512_storeu_si512(p, self.0);
            _mm512_storeu_si512(p.wrapping_add(1), self.1);
        }
        o
    }
    #[inline(always)]
    pub fn lane(self, i: usize) -> u64 {
        self.to_array()[i]
    }
}

#[inline(always)]
pub fn zw_splat(v: u64) -> ZW {
    unsafe {
        let x = _mm512_set1_epi64(v as i64);
        ZW(x, x)
    }
}

/// The representation BITS of one cell's value - not a hash, just the bit
/// pattern the fold consumes, packed so that two abstract values which
/// differ differ here too.
#[inline(always)]
pub fn zw_bits_n(x: ZN) -> ZW {
    unsafe {
        // Zero-extend i32 -> u64. The low 8 lanes are the low 256 bits.
        ZW(
            _mm512_cvtepu32_epi64(_mm512_castsi512_si256(x.0)),
            _mm512_cvtepu32_epi64(_mm512_extracti64x4_epi64::<1>(x.0)),
        )
    }
}
#[inline(always)]
pub fn zw_bits_i(x: ZI) -> ZW {
    unsafe {
        let lo = zw_bits_n(x.lo);
        let hi = zw_bits_n(x.hi);
        ZW(
            _mm512_or_si512(_mm512_slli_epi64::<32>(lo.0), hi.0),
            _mm512_or_si512(_mm512_slli_epi64::<32>(lo.1), hi.1),
        )
    }
}
/// Two bits per lane: the value where it is known, and whether it is.
#[inline(always)]
pub fn zw_bits_b(x: ZB) -> ZW {
    let mut o = [0u64; W];
    for i in 0..W {
        o[i] = (((x.val >> i) & 1) as u64) | ((((x.known >> i) & 1) as u64) << 1);
    }
    ZW::from_array(o)
}

/// One step of the row-key fold, over the accumulator and one cell's
/// bits. TWO accumulators mixed differently, so the pair is 128 bits: a
/// collision here DROPS a successor rather than merely costing time.
///
/// The cell id goes in as well as the value, so the key is not invariant
/// under moving a value from one field to another.
#[inline(always)]
pub fn zw_mix1(h: ZW, v: ZW, c: u64) -> ZW {
    unsafe {
        let k = _mm512_set1_epi64(c as i64);
        let f = |hh, vv| mix64_v(_mm512_xor_si512(hh, mix64_v(_mm512_xor_si512(vv, k))));
        ZW(f(h.0, v.0), f(h.1, v.1))
    }
}
#[inline(always)]
pub fn zw_mix2(h: ZW, v: ZW, c: u64) -> ZW {
    unsafe {
        let k = _mm512_set1_epi64(((c << 1) | 1) as i64);
        let f = |hh, vv| _mm512_add_epi64(hh, mix64_v(_mm512_mullo_epi64(vv, k)));
        ZW(f(h.0, v.0), f(h.1, v.1))
    }
}

/// `mix64` (runtime2), eight lanes at a time. Kept beside the scalar one
/// rather than derived from it: `mix64` is the row key's definition, and
/// the two are checked against each other in `tests`.
#[inline(always)]
unsafe fn mix64_v(x: __m512i) -> __m512i {
    let a = _mm512_xor_si512(x, _mm512_srli_epi64::<30>(x));
    let a = _mm512_mullo_epi64(a, _mm512_set1_epi64(0xbf58_476d_1ce4_e5b9u64 as i64));
    let b = _mm512_xor_si512(a, _mm512_srli_epi64::<27>(a));
    let b = _mm512_mullo_epi64(b, _mm512_set1_epi64(0x94d0_49bb_1331_11ebu64 as i64));
    _mm512_xor_si512(b, _mm512_srli_epi64::<31>(b))
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

/// `flr`, in one instruction.
///
/// `P8::flr` is `from_i16((self.0 >> 16) as i16)`, i.e.
/// `((x >> 16) as i16 as i32) << 16`. The `as i16` cannot lose anything -
/// an arithmetic `>> 16` of an `i32` lands in `[-32768, 32767]`, which is
/// exactly `i16` - so the whole thing is `x & 0xffff_0000`.
#[inline(always)]
pub fn zn_flr(a: ZN) -> ZN {
    ZN(unsafe { _mm512_and_si512(a.0, m512(0xffff_0000u32 as i32)) })
}

/// 16.16 multiply: `(a as i64 * b as i64) >> 16`, truncated to i32.
///
/// x86 has no 32x32 -> shifted-64 instruction, so this is the standard
/// even/odd split. `vpmuldq` multiplies the LOW i32 of each 64-bit
/// element, sign-extended - which is the even lanes; shifting both
/// operands right by 32 (ARITHMETIC, for the sign) brings the odd lanes
/// into that position for the second multiply. Each 64-bit product is
/// shifted down by 16, and the two are woven back by putting the odd
/// results into the odd i32 slots and blending.
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

/// Divide and remainder go out to scalar, and this IS a hole in the
/// representation - see `ZN`'s doc for why that costs something.
///
/// x86 has no integer vector divide. Both are reachable through `f64`
/// (an `i32` and a 47-bit shifted numerator are both exact in a 53-bit
/// mantissa) plus a correction step for round-to-nearest, which is
/// Philippe's point and is right. It is not done yet because the room's
/// kernels contain **4 `zn_div` and 4 `zn_rem` sites against ~13,700
/// nodes**, so the hole is measurable before it is worth closing.
/// Close it with a number, not with an assumption.
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
/// A 16,384-entry table lookup per lane. `vpgatherdd` would do it; there
/// are ZERO `zn_sin` sites in the room's kernels, so it waits.
#[inline(always)]
pub fn zn_sin(a: ZN) -> ZN {
    let x = a.to_array();
    ZN::from_array(std::array::from_fn(|i| x[i].pico8_sin()))
}

// ---- interval ops (per-lane ports of runtime2's av_* interval arms) ----
//
// Componentwise on the two planes, which is what makes them vector at
// all: an interval endpoint is a `Pico8Num` and the interval rules are
// selections between endpoints, never a per-lane branch.
//
// The WRAP CHECK survives. `Pico8NumInterval`'s `Add`/`Sub` go through
// `from_i64_endpoints`, which panics if the result leaves `i32` - a real
// guard, not a formality, because a widened interval is exactly the
// thing that can run away. A plain `vpaddd` would wrap silently. So each
// arm computes the signed-overflow predicate in vector form and hands
// the whole column to the scalar implementation if ANY lane trips it,
// which panics with the message it always did. Four extra instructions
// on a path that is never taken.

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

/// The scalar interval implementation, per lane. Reached only when a
/// vector arm detects a wrap and wants its panic, and by the tests,
/// which use it as the oracle for every arm above.
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

#[inline(always)]
pub fn zi_add(a: ZI, b: ZI) -> ZI {
    unsafe {
        let lo = _mm512_add_epi32(a.lo.0, b.lo.0);
        let hi = _mm512_add_epi32(a.hi.0, b.hi.0);
        if add_overflows(a.lo.0, b.lo.0, lo) | add_overflows(a.hi.0, b.hi.0, hi) != 0 {
            return zi_scalar(a, b, |x, y| x + y);
        }
        ZI { lo: ZN(lo), hi: ZN(hi) }
    }
}
#[inline(always)]
pub fn zi_sub(a: ZI, b: ZI) -> ZI {
    // `[a.low - b.high, a.high - b.low]` - the endpoints CROSS, which is
    // the whole content of interval subtraction.
    unsafe {
        let lo = _mm512_sub_epi32(a.lo.0, b.hi.0);
        let hi = _mm512_sub_epi32(a.hi.0, b.lo.0);
        if sub_overflows(a.lo.0, b.hi.0, lo) | sub_overflows(a.hi.0, b.lo.0, hi) != 0 {
            return zi_scalar(a, b, |x, y| x - y);
        }
        ZI { lo: ZN(lo), hi: ZN(hi) }
    }
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
    ZI { lo: zn_neg(a.hi), hi: zn_neg(a.lo) }
}

/// `abs` of an interval: three cases, and no branch.
///
///   * entirely non-negative -> unchanged;
///   * entirely non-positive -> reflected, endpoints swapping;
///   * straddling zero       -> `[0, max(|lo|, |hi|)]`.
///
/// Each case is a mask and the result is two blends, because the cases
/// PARTITION the lanes.
#[inline(always)]
pub fn zi_abs(a: ZI) -> ZI {
    unsafe {
        let zero = _mm512_setzero_si512();
        let pos = _mm512_cmpge_epi32_mask(a.lo.0, zero);
        let neg = _mm512_cmple_epi32_mask(a.hi.0, zero);
        let (al, ah) = (_mm512_abs_epi32(a.lo.0), _mm512_abs_epi32(a.hi.0));
        // Straddling is the fallback, so build it first and blend the
        // two decided cases over it.
        let mut lo = zero;
        let mut hi = _mm512_max_epi32(al, ah);
        lo = _mm512_mask_blend_epi32(neg, lo, ah);
        hi = _mm512_mask_blend_epi32(neg, hi, al);
        lo = _mm512_mask_blend_epi32(pos, lo, a.lo.0);
        hi = _mm512_mask_blend_epi32(pos, hi, a.hi.0);
        ZI { lo: ZN(lo), hi: ZN(hi) }
    }
}

/// av_mul interval arm: interval * positive num, per lane.
///
/// The multiplier's POSITIVITY is a premise, not a branch: the emitter
/// records `b > 0` as a conjunct of the member's validity, and a lane
/// that fails it is filtered out downstream. Lanes that fail it here are
/// left UNCHANGED rather than scaled, matching the scalar version, whose
/// `scale_positive` asserts - the value of a filtered lane is never read.
#[inline(always)]
pub fn zi_mul_pos(a: ZI, b: ZN) -> ZI {
    unsafe {
        let ok = _mm512_cmpgt_epi32_mask(b.0, _mm512_setzero_si512());
        let lo = zn_mul(a.lo, b);
        let hi = zn_mul(a.hi, b);
        ZI {
            lo: ZN(_mm512_mask_blend_epi32(ok, a.lo.0, lo.0)),
            hi: ZN(_mm512_mask_blend_epi32(ok, a.hi.0, hi.0)),
        }
    }
}

/// The dividing twin of `zi_mul_pos`, and a HOLE for the same reason
/// `zn_div` is: `((a as i64) << 16) / b` has no vector instruction. Four
/// sites in the room's kernels.
#[inline(always)]
pub fn zi_div_pos(a: ZI, b: ZN) -> ZI {
    let zero = P8::from_i16(0);
    let (alo, ahi, bb) = (a.lo.to_array(), a.hi.to_array(), b.to_array());
    let mut lo = alo;
    let mut hi = ahi;
    for i in 0..W {
        if bb[i] > zero {
            let r = IV::new(alo[i], ahi[i]).div_positive(bb[i]);
            lo[i] = r.low;
            hi[i] = r.high;
        }
    }
    ZI { lo: ZN::from_array(lo), hi: ZN::from_array(hi) }
}

/// av_flr interval arm. Whether the floor is UNIQUE is a premise
/// (`zi_flr_ok`), so this just takes the low endpoint's floor - which is
/// the floor, on every lane that survives the premise.
#[inline(always)]
pub fn zi_flr(a: ZI) -> ZN {
    zn_flr(a.lo)
}

/// The premise `zi_flr` is taken under: this lane's interval has ONE
/// floor. `known: ALL` because the answer is a fact about the interval,
/// never itself undecided.
#[inline(always)]
pub fn zi_flr_ok(a: ZI) -> ZB {
    ZB { val: mask_eq(zn_flr(a.lo), zn_flr(a.hi)), known: ALL }
}

/// The premise `zi_fork_flr` is taken under: this lane's interval spans
/// at most TWO floors, so the two fork outcomes can represent it. A
/// boundary-widened interval has width < 1 and always passes.
#[inline(always)]
pub fn zi_span_ok(a: ZI) -> ZB {
    let (fl, fh) = (zn_flr(a.lo), zn_flr(a.hi));
    let one = zn_splat(P8::from_i16(1));
    ZB { val: mask_eq(fl, fh) | mask_eq(fh, zn_add(fl, one)), known: ALL }
}

// ---- comparisons ----
//
// `Pico8Num` derives `Ord` from its raw `i32`, so an abstract comparison
// IS a signed 32-bit compare - and `vpcmpd` produces exactly the 16-bit
// mask `ZB` wants, in one instruction.
//
// NOTE, because the history is instructive: this was tried on 2026-08-23
// while `ZN` was still `[Pico8Num; 16]` and was **27% SLOWER**, which is
// recorded under "REFUTED" in plans/tracing.md. Nothing about the
// instruction was wrong; the column had to be assembled from an array
// and taken apart again around it. With `ZN` a register the assembly
// disappears and the same instruction is free. That is the whole
// argument for changing the representation rather than the primitives.

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
///
/// One `vpblendmd`. `ZB::val` is already a mask register's worth of
/// bits, which is why the boolean layer keeps its lanes as a `u16` -
/// and this is the primitive that decision was made for: the census
/// counts 1,442 of these in `kernel1`, more than twice the comparisons.
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
    let (fl, fh) = (zn_flr(a.lo), zn_flr(a.hi));
    // The one case that actually splits: the interval spans EXACTLY two
    // floors, so fragment 0 is everything below the boundary and
    // fragment 1 everything from it up. One floor needs no split, and
    // more than two cannot be represented by two fragments - those
    // lanes stay whole and go down configuration 0, where `zi_span_ok`
    // is what takes them off the kernel.
    let two = mask_eq(fh, zn_add(fl, zn_splat(P8::from_i16(1))));
    unsafe {
        if c == 0 {
            // `next_smallest` is minus one raw unit.
            let below = _mm512_sub_epi32(fh.0, _mm512_set1_epi32(1));
            (ZI { lo: a.lo, hi: ZN(_mm512_mask_blend_epi32(two, a.hi.0, below)) }, ALL)
        } else {
            (ZI { lo: ZN(_mm512_mask_blend_epi32(two, a.lo.0, fh.0)), hi: a.hi }, two)
        }
    }
}

// ---- cart / collision builtins (per-lane; x/y vary, w/h/flag uniform) ----

/// mget, per lane, against the raw grid. Semantics identical to
/// `CartData::mget(..).expect(..)`: fractional or out-of-range
/// coordinates panic (the certified trace never produces them).
#[inline(always)]
pub fn zn_mget(cart: &CartData, x: ZN, y: ZN) -> ZN {
    let map = cart.map_grid();
    let (x, y) = (x.to_array(), y.to_array());
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
    ZN::from_array(o)
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
    let (x, y) = (x.to_array(), y.to_array());
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
    let (x, y, w, h) = (x.to_array(), y.to_array(), w.to_array(), h.to_array());
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
    /// because an operation that only ever sees small positive numbers
    /// would not notice a sign or an overflow bug in either direction.
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
            // Small values too: the spread above is dominated by huge
            // ones, and the game's numbers are mostly in [-256, 256].
            v.push(P8::from_i16((s % 512) as i16 - 256));
        }
        v
    }

    fn column(seed: usize) -> ZN {
        let s = spread();
        ZN::from_array(std::array::from_fn(|i| s[(seed * 7 + i * 5) % s.len()]))
    }

    /// A column of SMALL values, for the arms that would overflow on the
    /// extremes and whose scalar version panics rather than wrapping.
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

    /// The oracle for every `zn_*` arm: the scalar operator, one lane at
    /// a time. This IS the implementation these primitives had until the
    /// lane type became a register, so a disagreement is a regression
    /// against code that ran for months.
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
            // min/max/neg/abs/flr are total, so they get the WIDE spread
            // including i32::MIN, where `abs` and `neg` are interesting.
            let (a, b) = (column(seed), column(seed + 31));
            assert_eq!(zn_min(a, b), ref2(a, b, |x, y| x.min(y)), "min {}", seed);
            assert_eq!(zn_max(a, b), ref2(a, b, |x, y| x.max(y)), "max {}", seed);
            assert_eq!(zn_flr(a), ref1(a, |x| x.flr()), "flr {}", seed);
            assert_eq!(zn_div(a, b), ref2(a, b, |x, y| x / y), "div {}", seed);
            assert_eq!(zn_rem(a, b), ref2(a, b, |x, y| x % y), "rem {}", seed);
        }
    }

    /// `zn_mul` is the one arm that is not a single instruction - an
    /// even/odd `vpmuldq` split woven back together - so it gets its own
    /// test with the lane pattern that would catch a weave bug: every
    /// lane a different value, so swapping even and odd shows.
    #[test]
    fn the_multiply_weave_puts_every_lane_back_where_it_came_from() {
        let a = ZN::from_array(std::array::from_fn(|i| P8::from_i16(i as i16 + 1)));
        let b = ZN::from_array(std::array::from_fn(|i| P8::from_i16(100 - i as i16)));
        assert_eq!(zn_mul(a, b), ref2(a, b, |x, y| x * y));
        // Negative operands in alternating lanes: the odd-lane path uses
        // an ARITHMETIC shift to bring the high i32 down, and a logical
        // one would only show up on negatives.
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

    /// The interval arms, against the `Pico8NumInterval` operators they
    /// are ports of. Small operands, because `zi_add`/`zi_sub` PANIC on
    /// a wrap rather than wrapping and this checks the ordinary path;
    /// the wrap path has its own test.
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

    /// `zi_add` and `zi_sub` must PANIC on a wrap, not wrap silently.
    /// The vector arms compute the overflow predicate and hand the whole
    /// column to the scalar implementation when any lane trips it - so
    /// this checks the guard still bites, which a `vpaddd` alone would
    /// have removed.
    #[test]
    #[should_panic]
    fn an_interval_add_that_wraps_still_panics() {
        let big = zn_splat(P8::from_raw(i32::MAX));
        let a = ZI { lo: big, hi: big };
        let _ = zi_add(a, a);
    }

    /// The premises the fork is taken under, and the fork itself,
    /// against the case analysis they encode.
    #[test]
    fn the_floor_premises_and_the_fork_agree_with_their_definition() {
        let one = P8::from_i16(1);
        for seed in 0..96 {
            let a = ival(seed);
            let (al, ah) = (a.lo.to_array(), a.hi.to_array());
            let (mut uniq, mut span) = (0u16, 0u16);
            for i in 0..W {
                let (fl, fh) = (al[i].flr(), ah[i].flr());
                if fl == fh {
                    uniq |= 1 << i;
                }
                if fl == fh || fh == fl + one {
                    span |= 1 << i;
                }
            }
            assert_eq!(zi_flr_ok(a).val, uniq, "zi_flr_ok {}", seed);
            assert_eq!(zi_span_ok(a).val, span, "zi_span_ok {}", seed);
            assert_eq!(zi_flr(a), zn_flr(a.lo), "zi_flr {}", seed);

            // The fork, both configurations, against `zi_fork_flr`'s
            // three cases written out.
            for c in 0..2usize {
                let (got, gv) = zi_fork_flr(a, c);
                let (mut lo, mut hi) = (al, ah);
                let mut valid = 0u16;
                for i in 0..W {
                    let (fl, fh) = (al[i].flr(), ah[i].flr());
                    if fh == fl + one {
                        valid |= 1 << i;
                        if c == 0 {
                            hi[i] = fh.next_smallest();
                        } else {
                            lo[i] = fh;
                        }
                    } else if c == 0 {
                        valid |= 1 << i;
                    }
                }
                assert_eq!(gv, valid, "fork {} validity, seed {}", c, seed);
                // Only the VALID lanes' values are ever read.
                for i in 0..W {
                    if valid & (1 << i) != 0 {
                        assert_eq!(got.lo.lane(i), lo[i], "fork {} lo lane {}", c, i);
                        assert_eq!(got.hi.lane(i), hi[i], "fork {} hi lane {}", c, i);
                    }
                }
            }
        }
    }

    /// The row-key fold, eight lanes at a time, against the scalar
    /// `mix64` that DEFINES the key. A disagreement here does not fail
    /// loudly anywhere else - it silently changes which successors are
    /// considered distinct - so it is checked directly.
    #[test]
    fn the_vector_hash_agrees_with_the_scalar_mix64() {
        for seed in 0..64u64 {
            let h = ZW::from_array(std::array::from_fn(|i| {
                (seed + 1).wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ (i as u64)
            }));
            let v = ZW::from_array(std::array::from_fn(|i| {
                (seed + 7).wrapping_mul(0xc2b2_ae3d_27d4_eb4f) ^ ((i as u64) << 40)
            }));
            for c in [0u64, 1, 41, 0xffff_ffff] {
                let (ha, va) = (h.to_array(), v.to_array());
                let w1 = ZW::from_array(std::array::from_fn(|i| {
                    mix64(ha[i] ^ mix64(va[i] ^ c))
                }));
                let w2 = ZW::from_array(std::array::from_fn(|i| {
                    ha[i].wrapping_add(mix64(va[i].wrapping_mul((c << 1) | 1)))
                }));
                assert_eq!(zw_mix1(h, v, c).to_array(), w1.to_array(), "mix1 {} {}", seed, c);
                assert_eq!(zw_mix2(h, v, c).to_array(), w2.to_array(), "mix2 {} {}", seed, c);
            }
        }
    }

    /// The bit extractions the fold consumes. `zw_bits_i` packs two
    /// planes into one word, so a lane swap or a sign-extension instead
    /// of a zero-extension would show here and nowhere else.
    #[test]
    fn the_row_key_bit_extractions_pack_the_lanes_they_say() {
        for seed in 0..64 {
            let a = column(seed);
            let want: [u64; W] = std::array::from_fn(|i| a.lane(i).as_raw_u32() as u64);
            assert_eq!(zw_bits_n(a).to_array(), want, "bits_n {}", seed);

            let iv = ZI { lo: column(seed), hi: column(seed + 5) };
            let want: [u64; W] = std::array::from_fn(|i| {
                ((iv.lo.lane(i).as_raw_u32() as u64) << 32) | (iv.hi.lane(i).as_raw_u32() as u64)
            });
            assert_eq!(zw_bits_i(iv).to_array(), want, "bits_i {}", seed);
        }
        for (val, known) in [(0u16, 0u16), (ALL, ALL), (0x5555, 0xaaaa), (1, 0xffff)] {
            let b = ZB { val, known };
            let want: [u64; W] = std::array::from_fn(|i| {
                (((val >> i) & 1) as u64) | ((((known >> i) & 1) as u64) << 1)
            });
            assert_eq!(zw_bits_b(b).to_array(), want, "bits_b {:04x}/{:04x}", val, known);
        }
    }

    /// Round-tripping a column through an array must be the identity,
    /// which is the premise `append` and the row-key walk rely on when
    /// they read one lane at a time.
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
                    let (al, ah) = (a.lo.lane(i), a.hi.lane(i));
                    let (bl, bh) = (b.lo.lane(i), b.hi.lane(i));
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
