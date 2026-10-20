//! THE GAME'S ARITHMETIC, a compile-time choice (plans/float.md): `Fixed`,
//! PICO-8's 16.16 (`Pico8Num`, the real game), or `Float`, the Lua doubles
//! of the community's TAS tools (`LuaF64`). Generic code monomorphizes to
//! one copy per arithmetic, so values of the two can never meet.
//!
//! A point operation is CHECKED: the exact result, or `None` where the real
//! one wraps, saturates or leaves the finite doubles (the callers raise). An
//! interval's operations are the point operation on the ends; for `Float`
//! that needs NO outward rounding: each operation and rounding are
//! monotone, so the ends bound the IEEE result over the whole box exactly.

use std::fmt::Debug;
use std::hash::Hash;

use anyhow::Result;

use crate::lua_f64::{LuaF64, LuaF64Interval};
use crate::pico8_num::{Pico8Num, Pico8NumInterval};

/// THE ARITHMETIC, on its number type (`Fixed`: `Pico8Num`, `Float`:
/// `LuaF64`). `Eq` and `Hash` are BIT identity (dedup and keys). Also how
/// the engine STORES a number - in blocks, codes and dictionaries, with its
/// raw bits: the blocks and the key space are generic over it
/// (`runtime2::Rt2<N>`, `exact::Code<N>`), so a tree of one arithmetic and a
/// block of the other are different types.
pub trait Arithmetic: 'static + Copy + Default + Debug + Send + Sync + PartialEq + Eq + Hash + serde::Serialize + serde::de::DeserializeOwned {
    /// A closed interval of numbers (the reference domain's value).
    type Ival: Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static;
    /// A number as the kernels' graph IR stores a literal
    /// (`transpile::graph::Op::Const`): 16.16's raw `i32`, a double's bits.
    type Lit: Copy + Debug + PartialEq + Eq + Hash + Send + Sync + 'static;

    /// `fixed` or `float`: the setting's value.
    const NAME: &'static str;
    /// What an overflowing operation leaves, for its error.
    const RANGE_NOTE: &'static str;
    /// Are `__split_by_round`'s fragments of `x` the floor fork's of `x +
    /// 1/2`, shifted back exactly (16.16: yes; doubles: `+ 1/2` rounds)?
    const ROUND_BY_SHIFTED_FLOOR: bool;

    fn from_i16(v: i16) -> Self;
    /// A number as a graph literal, and back (`Lit`).
    fn lit(v: Self) -> Self::Lit;
    fn from_lit(l: Self::Lit) -> Self;
    /// The value as an `i16`, where it is an integer in range.
    fn as_i16(v: Self) -> Option<i16>;
    /// A cart literal, as the arithmetic's console or tool reads it.
    fn parse_literal(text: &str) -> Result<Self>;

    fn add(a: Self, b: Self) -> Option<Self>;
    fn sub(a: Self, b: Self) -> Option<Self>;
    fn mul(a: Self, b: Self) -> Option<Self>;
    fn div(a: Self, b: Self) -> Option<Self>;
    /// The cart's `%`.
    fn rem(a: Self, b: Self) -> Option<Self>;
    fn neg(a: Self) -> Option<Self>;
    fn abs(a: Self) -> Option<Self>;
    fn flr(a: Self) -> Self;
    /// The cart's `sin` (in turns, inverted).
    fn sin(a: Self) -> Option<Self>;
    fn min(a: Self, b: Self) -> Self;
    fn max(a: Self, b: Self) -> Self;
    fn lt(a: Self, b: Self) -> bool;
    fn le(a: Self, b: Self) -> bool;
    /// The cart's `==` on numbers.
    fn num_eq(a: Self, b: Self) -> bool;
    /// The largest value `rnd(x)` returns (`x > 0`): just below `x`.
    fn rnd_top(x: Self) -> Option<Self>;
    /// The largest number below `x`.
    fn below(x: Self) -> Option<Self>;
    /// A 16.16 constant (raw), exactly (every one is a double).
    fn from_fixed_raw(raw: i32) -> Self;
    /// The number's bits (identity: a dedup key).
    fn bits(v: Self) -> u64;
    /// The number of these bits (`bits`' inverse), if they are one.
    fn from_bits(b: u64) -> Option<Self>;
    fn show(v: Self) -> String;
    /// A number as the console's `printh` prints it.
    fn printh(v: Self) -> Result<String>;
    /// A number's exact bits (the probes' `__hex`).
    fn hex(v: Self) -> String;

    fn point(v: Self) -> Self::Ival;
    /// `[lo, hi]`, `lo <= hi`.
    fn ival(lo: Self, hi: Self) -> Self::Ival;
    fn bounds(i: Self::Ival) -> (Self, Self);
    fn as_point(i: Self::Ival) -> Option<Self>;
    /// The unknown number: every value (an operation on it stays unknown).
    fn unknown() -> Self::Ival;
    fn ival_add(a: Self::Ival, b: Self::Ival) -> Option<Self::Ival>;
    fn ival_sub(a: Self::Ival, b: Self::Ival) -> Option<Self::Ival>;
    fn ival_mul(a: Self::Ival, b: Self::Ival) -> Option<Self::Ival>;
    /// `None` also where the divisor's interval holds 0.
    fn ival_div(a: Self::Ival, b: Self::Ival) -> Option<Self::Ival>;
    fn ival_neg(a: Self::Ival) -> Option<Self::Ival>;
    fn ival_abs(a: Self::Ival) -> Option<Self::Ival>;
    /// How many integer floors `i` spans; `None` past 65536.
    fn floors(i: Self::Ival) -> Option<u32>;
    /// The part of `i` in its `k`-th floor (`k < floors(i)`).
    fn floor_fragment(i: Self::Ival, k: u32) -> Self::Ival;
    /// How many values `flr(x + 1/2)` takes over `i`; `None` past 65536.
    fn rounds(i: Self::Ival) -> Result<Option<u32>>;
    /// The part of `i` where `flr(x + 1/2)` takes its `k`-th value (`k <
    /// rounds(i)`): `__split_by_round`'s fragments, its cut points exact in
    /// the arithmetic.
    fn round_fragment(i: Self::Ival, k: u32) -> Result<Self::Ival>;

    // How the engine stores a number.
    /// The bits a code holds: 16.16's `u32` word, a double's `u64`.
    type Raw: Copy + Debug + PartialEq + Eq + PartialOrd + Ord + Hash + Default + Send + Sync + From<u32> + TryInto<u32> + serde::Serialize + serde::de::DeserializeOwned + 'static;
    /// A number interval's dictionary word: its two raws, packed.
    type Word: Copy + Debug + PartialEq + Eq + Hash + Default + Send + Sync + 'static;
    /// The last byte of a tree file's magic (16.16: 0, doubles: `f`): a
    /// tree of one arithmetic is never read as the other's.
    const FILE_TAG: u8;
    fn raw(self) -> Self::Raw;
    /// `raw`'s inverse; `None` for bits no number of the arithmetic has.
    fn from_raw(r: Self::Raw) -> Option<Self>;
    fn word(lo: Self::Raw, hi: Self::Raw) -> Self::Word;
    /// A word folded to 64 bits (a hash table's slot, a fingerprint's
    /// input): 16.16's word itself.
    fn fold(w: Self::Word) -> u64;
    /// A raw's little-endian bytes onto `out` (a state's exact bytes).
    fn push_raw(r: Self::Raw, out: &mut Vec<u8>);
    /// The whole part, as the position cells read it (`floor`, 16.16's
    /// integer bits): `None` outside `i16`.
    fn whole_i16(self) -> Option<i16>;
    /// The value as an `f64`, exactly (16.16 and doubles both fit).
    fn to_f64(self) -> f64;
    /// A position's code part: the value less `whole` whole pixels (the cell
    /// holds those), exactly.
    fn less_whole(self, whole: i16) -> Self::Raw;
    /// `less_whole`'s inverse: the raw of a code part `r` plus `whole`
    /// whole pixels.
    fn plus_whole(r: Self::Raw, whole: i32) -> Self::Raw;
}

/// PICO-8's 16.16 fixed point: the real game.
pub type Fixed = Pico8Num;

/// The Lua doubles of UniversalClassicTas and Celia's float mode.
pub type Float = LuaF64;

type P8 = Pico8Num;
type Iv = Pico8NumInterval;

impl Arithmetic for Fixed {
    type Ival = Iv;
    type Lit = i32;
    const NAME: &'static str = "fixed";
    const RANGE_NOTE: &'static str = "the 16.16 range (PICO-8 would wrap or saturate; overflow is an error)";
    const ROUND_BY_SHIFTED_FLOOR: bool = true;

    fn lit(v: P8) -> i32 {
        v.as_raw_u32() as i32
    }
    fn from_lit(l: i32) -> P8 {
        P8::from_raw(l)
    }
    fn from_i16(v: i16) -> P8 {
        P8::from_i16(v)
    }
    fn as_i16(v: P8) -> Option<i16> {
        v.as_i16()
    }
    fn parse_literal(text: &str) -> Result<P8> {
        text.parse()
    }
    fn add(a: P8, b: P8) -> Option<P8> {
        a.checked_add(b)
    }
    fn sub(a: P8, b: P8) -> Option<P8> {
        a.checked_sub(b)
    }
    fn mul(a: P8, b: P8) -> Option<P8> {
        a.checked_mul(b)
    }
    fn div(a: P8, b: P8) -> Option<P8> {
        a.checked_div(b)
    }
    /// Never overflows: in `[0, |b|)`, or 0 for `b == 0` (the console's).
    fn rem(a: P8, b: P8) -> Option<P8> {
        Some(a % b)
    }
    fn neg(a: P8) -> Option<P8> {
        a.checked_neg()
    }
    fn abs(a: P8) -> Option<P8> {
        a.checked_abs()
    }
    fn flr(a: P8) -> P8 {
        a.flr()
    }
    fn sin(a: P8) -> Option<P8> {
        Some(a.pico8_sin())
    }
    fn min(a: P8, b: P8) -> P8 {
        a.min(b)
    }
    fn max(a: P8, b: P8) -> P8 {
        a.max(b)
    }
    fn lt(a: P8, b: P8) -> bool {
        a < b
    }
    fn le(a: P8, b: P8) -> bool {
        a <= b
    }
    fn num_eq(a: P8, b: P8) -> bool {
        a == b
    }
    fn rnd_top(x: P8) -> Option<P8> {
        (x > P8::from_i16(0)).then(|| P8::from_raw(x.as_raw_u32() as i32 - 1))
    }
    fn below(x: P8) -> Option<P8> {
        (x.as_raw_u32() as i32).checked_sub(1).map(P8::from_raw)
    }
    fn from_fixed_raw(raw: i32) -> P8 {
        P8::from_raw(raw)
    }
    fn bits(v: P8) -> u64 {
        v.to_bits() as u64
    }
    fn from_bits(b: u64) -> Option<P8> {
        u32::try_from(b).ok().map(|r| P8::from_raw(r as i32))
    }
    fn show(v: P8) -> String {
        format!("{}", (v.to_bits() as i32) as f64 / 65536.0)
    }
    /// `printh(1/3)` prints `0.3333`.
    fn printh(v: P8) -> Result<String> {
        if let Some(i) = v.as_i16() {
            return Ok(format!("{}", i));
        }
        let raw = v.as_raw_u32() as i32 as f64 / 65536.0;
        let s = format!("{:.4}", raw);
        Ok(s.trim_end_matches('0').trim_end_matches('.').to_string())
    }
    /// As PICO-8's `tostr(x, true)`: `0x0000.5555`.
    fn hex(v: P8) -> String {
        let raw = v.to_bits();
        format!("0x{:04x}.{:04x}", raw >> 16, raw & 0xffff)
    }

    fn point(v: P8) -> Iv {
        Iv::from_number(v)
    }
    fn ival(lo: P8, hi: P8) -> Iv {
        Iv::new(lo, hi)
    }
    fn bounds(i: Iv) -> (P8, P8) {
        (i.low, i.high)
    }
    fn as_point(i: Iv) -> Option<P8> {
        i.to_number()
    }
    /// The whole 16.16 range.
    fn unknown() -> Iv {
        Iv::new(P8::from_raw(i32::MIN), P8::from_raw(i32::MAX))
    }
    fn ival_add(a: Iv, b: Iv) -> Option<Iv> {
        a.checked_add(b)
    }
    fn ival_sub(a: Iv, b: Iv) -> Option<Iv> {
        a.checked_sub(b)
    }
    fn ival_mul(a: Iv, b: Iv) -> Option<Iv> {
        a.checked_mul(b)
    }
    fn ival_div(a: Iv, b: Iv) -> Option<Iv> {
        a.checked_div(b)
    }
    fn ival_neg(a: Iv) -> Option<Iv> {
        a.checked_neg()
    }
    fn ival_abs(a: Iv) -> Option<Iv> {
        a.checked_abs()
    }
    /// 1 to 65536 (in `i32`: the unknown number's 65536 floors overflow an
    /// `i16` difference).
    fn floors(i: Iv) -> Option<u32> {
        Some((i.high.whole_part_as_i16() as i32 - i.low.whole_part_as_i16() as i32 + 1) as u32)
    }
    fn floor_fragment(i: Iv, k: u32) -> Iv {
        let base = i.low.whole_part_as_i16() as i32 + k as i32;
        // `[base, base + 1 - eps]`: a fragment spans exactly one floor.
        let (floor, top) = (P8::from_raw(base << 16), P8::from_raw((base << 16) | 0xffff));
        Iv::new(i.low.max(floor), i.high.min(top))
    }
    /// The floors of `x + 1/2`: exact in 16.16 (raw `i64`, no wrap).
    fn rounds(i: Iv) -> Result<Option<u32>> {
        let r = |v: P8| (v.to_bits() as i32 as i64 + 0x8000) >> 16;
        let n = r(i.high) - r(i.low) + 1;
        Ok((n <= 65536).then_some(n as u32))
    }
    /// `[t - 1/2, t + 1/2 - eps]` of `flr(x + 1/2) == t`, cut to `i`.
    fn round_fragment(i: Iv, k: u32) -> Result<Iv> {
        let t = ((i.low.to_bits() as i32 as i64 + 0x8000) >> 16) + k as i64;
        let (lo, hi) = ((t << 16) - 0x8000, (t << 16) + 0x7fff);
        let clamp = |v: i64| P8::from_raw(v.clamp(i32::MIN as i64, i32::MAX as i64) as i32);
        Ok(Iv::new(i.low.max(clamp(lo)), i.high.min(clamp(hi))))
    }
    type Raw = u32;
    type Word = u64;
    const FILE_TAG: u8 = 0;
    fn raw(self) -> u32 {
        self.as_raw_u32()
    }
    fn from_raw(r: u32) -> Option<P8> {
        Some(P8::from_raw(r as i32))
    }
    fn word(lo: u32, hi: u32) -> u64 {
        (lo as u64) << 32 | hi as u64
    }
    fn fold(w: u64) -> u64 {
        w
    }
    fn push_raw(r: u32, out: &mut Vec<u8>) {
        out.extend_from_slice(&r.to_le_bytes());
    }
    fn to_f64(self) -> f64 {
        self.as_raw_u32() as i32 as f64 / 65536.0
    }
    fn whole_i16(self) -> Option<i16> {
        Some(self.whole_part_as_i16())
    }
    fn less_whole(self, whole: i16) -> u32 {
        self.as_raw_u32().wrapping_sub((whole as i32 as u32) << 16)
    }
    fn plus_whole(r: u32, whole: i32) -> u32 {
        r.wrapping_add((whole as u32) << 16)
    }
}

type F = LuaF64;
type FI = LuaF64Interval;

impl Arithmetic for Float {
    type Ival = FI;
    type Lit = u64;
    const NAME: &'static str = "float";
    const RANGE_NOTE: &'static str = "the finite doubles (an infinity or NaN; a non-finite result is an error)";
    const ROUND_BY_SHIFTED_FLOOR: bool = false;

    fn lit(v: F) -> u64 {
        v.to_bits()
    }
    /// A graph literal is a finite double (`lit` made it).
    fn from_lit(l: u64) -> F {
        F::new(f64::from_bits(l)).expect("a graph literal is a finite double")
    }
    fn from_i16(v: i16) -> F {
        F::from_i16(v)
    }
    fn as_i16(v: F) -> Option<i16> {
        v.as_i16()
    }
    fn parse_literal(text: &str) -> Result<F> {
        F::parse(text)
    }
    fn add(a: F, b: F) -> Option<F> {
        a.checked_add(b)
    }
    fn sub(a: F, b: F) -> Option<F> {
        a.checked_sub(b)
    }
    fn mul(a: F, b: F) -> Option<F> {
        a.checked_mul(b)
    }
    fn div(a: F, b: F) -> Option<F> {
        a.checked_div(b)
    }
    fn rem(a: F, b: F) -> Option<F> {
        a.checked_rem(b)
    }
    fn neg(a: F) -> Option<F> {
        Some(a.neg())
    }
    fn abs(a: F) -> Option<F> {
        Some(a.abs())
    }
    fn flr(a: F) -> F {
        a.flr()
    }
    fn sin(a: F) -> Option<F> {
        a.lua_sin()
    }
    fn min(a: F, b: F) -> F {
        a.lua_min(b)
    }
    fn max(a: F, b: F) -> F {
        a.lua_max(b)
    }
    fn lt(a: F, b: F) -> bool {
        a.lt(b)
    }
    fn le(a: F, b: F) -> bool {
        a.le(b)
    }
    fn num_eq(a: F, b: F) -> bool {
        a.eq_value(b)
    }
    /// `math.random() * x` with `math.random()` in `[0, 1)`: below `x`
    /// (`fl((1 - 2^-53) x) < x` for every finite `x > 0`).
    fn rnd_top(x: F) -> Option<F> {
        (x.value() > 0.0).then(|| x.pred()).flatten()
    }
    fn below(x: F) -> Option<F> {
        x.pred()
    }
    fn from_fixed_raw(raw: i32) -> F {
        F::new(raw as f64 / 65536.0).expect("a 16.16 constant is a finite double")
    }
    fn bits(v: F) -> u64 {
        v.to_bits()
    }
    fn from_bits(b: u64) -> Option<F> {
        F::new(f64::from_bits(b))
    }
    fn show(v: F) -> String {
        v.show()
    }
    /// LuaJIT's `tostring` (`%.14g`) is not modelled: nothing the float
    /// tools run prints a number the search reads.
    fn printh(v: F) -> Result<String> {
        anyhow::bail!("printh of the double {}: LuaJIT's %.14g is not modelled", v.show())
    }
    fn hex(v: F) -> String {
        format!("{:#018x}", v.to_bits())
    }

    fn point(v: F) -> FI {
        FI::point(v)
    }
    fn ival(lo: F, hi: F) -> FI {
        FI::new(lo, hi)
    }
    fn bounds(i: FI) -> (F, F) {
        (i.lo, i.hi)
    }
    fn as_point(i: FI) -> Option<F> {
        i.as_point()
    }
    fn unknown() -> FI {
        FI::unknown()
    }
    fn ival_add(a: FI, b: FI) -> Option<FI> {
        a.checked_add(b)
    }
    fn ival_sub(a: FI, b: FI) -> Option<FI> {
        a.checked_sub(b)
    }
    fn ival_mul(a: FI, b: FI) -> Option<FI> {
        a.checked_mul(b)
    }
    fn ival_div(a: FI, b: FI) -> Option<FI> {
        a.checked_div(b)
    }
    fn ival_neg(a: FI) -> Option<FI> {
        Some(a.neg())
    }
    fn ival_abs(a: FI) -> Option<FI> {
        Some(a.abs())
    }
    fn floors(i: FI) -> Option<u32> {
        i.floors()
    }
    fn floor_fragment(i: FI, k: u32) -> FI {
        i.floor_fragment(k)
    }
    fn rounds(i: FI) -> Result<Option<u32>> {
        i.rounds()
    }
    fn round_fragment(i: FI, k: u32) -> Result<FI> {
        i.round_fragment(k)
    }
    type Raw = u64;
    type Word = u128;
    const FILE_TAG: u8 = b'f';
    fn raw(self) -> u64 {
        self.to_bits()
    }
    fn from_raw(r: u64) -> Option<F> {
        F::new(f64::from_bits(r))
    }
    fn word(lo: u64, hi: u64) -> u128 {
        (lo as u128) << 64 | hi as u128
    }
    fn fold(w: u128) -> u64 {
        // Two words: the high one odd-multiplied, so (a, b) and (b, a) differ.
        ((w >> 64) as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ w as u64
    }
    fn push_raw(r: u64, out: &mut Vec<u8>) {
        out.extend_from_slice(&r.to_le_bytes());
    }
    fn to_f64(self) -> f64 {
        self.value()
    }
    fn whole_i16(self) -> Option<i16> {
        let w = self.value().floor();
        (i16::MIN as f64..=i16::MAX as f64).contains(&w).then(|| w as i16)
    }
    fn less_whole(self, whole: i16) -> u64 {
        // Exact: an integer at most the value taken off it keeps every bit
        // of its fraction.
        (self.value() - whole as f64).to_bits()
    }
    fn plus_whole(r: u64, whole: i32) -> u64 {
        // Exact: the sum is the value `less_whole` took it from.
        (f64::from_bits(r) + whole as f64).to_bits()
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    /// 16.16's round split: fragments at the half-integers, `flr(x + 1/2)`
    /// one value on each.
    #[test]
    fn fixed_round_fragments_cut_at_the_half_integers() {
        let i = Iv::new(P8::from_raw(-0x8000), P8::from_raw(0x1_7fff));
        assert_eq!(Fixed::rounds(i).unwrap(), Some(2));
        assert_eq!(Fixed::round_fragment(i, 0).unwrap(), Iv::new(P8::from_raw(-0x8000), P8::from_raw(0x7fff)));
        assert_eq!(Fixed::round_fragment(i, 1).unwrap(), Iv::new(P8::from_raw(0x8000), P8::from_raw(0x1_7fff)));
        for k in 0..2 {
            let f = Fixed::round_fragment(i, k).unwrap();
            let r = |v: P8| v.checked_add(P8::from_raw(0x8000)).unwrap().flr();
            assert_eq!(r(f.low), r(f.high), "one value of flr(x + 1/2) on fragment {k}");
        }
    }
}
