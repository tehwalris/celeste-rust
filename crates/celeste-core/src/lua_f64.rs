// Lua's numbers as the community's TAS tools run the cart: IEEE-754
// binary64, round to nearest even, as LuaJIT 2.1 computes them on x86-64
// (plans/float-solver.md, section 1). UniversalClassicTas and Celia's float
// mode both run the cart in LÖVE 11.5's LuaJIT; their `api.lua` maps the
// cart's builtins to the expressions reproduced here, in the same order:
//
//   flr = math.floor, abs = math.abs,
//   min(a, b) = a < b and a or b, max(a, b) = a > b and a or b,
//   sin(x) = -math.sin((x * math.pi) * 2)   (math.sin is the host libm's),
//   a % b = a - floor(a / b) * b            (LuaJIT's vm_mod, and the JIT's IR).
//
// LuaJIT's floating-point folds are bit-exact by design (`lj_opt_fold.c`:
// `x*2 -> x+x`, `x/2^k -> x*2^-k` only for an exact reciprocal, no `x+0`, no
// reassociation) and its x86 backend emits no FMA, so the interpreter and the
// JIT agree on every value, and this file computes each operation once, in
// the cart's order, with no contraction (Rust never fuses `a * b + c`).
//
// MATCH EXACTLY OR RAISE: a result that is not finite (an overflow, a
// division by zero, NaN) has no counterpart we model, so the checked
// operations return `None` and the callers raise. Celeste's values stay far
// from any of them.
//
// `sin` is the one operation that is not basic IEEE: it calls this machine's
// glibc `sin`, the same symbol LuaJIT calls in the tools (the LÖVE AppImage
// links the system libm). Bit-exactness is relative to that libm; a witness's
// portability to other libms is checked separately (section 11).

use anyhow::{bail, Result};
use std::fmt;
use std::hash::{Hash, Hasher};

/// A Lua number of the float tools: a FINITE `f64`. Equality and hashing are
/// BIT identity (`-0.0` and `0.0` are different values to a key or a dedup,
/// as their bits differ); the game's comparisons are `lt`, `le`, `eq_value`.
#[derive(Clone, Copy)]
pub struct LuaF64(f64);

impl PartialEq for LuaF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for LuaF64 {}

impl Hash for LuaF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl fmt::Debug for LuaF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LuaF64({:?} = {:#018x})", self.0, self.0.to_bits())
    }
}

#[link(name = "m")]
extern "C" {
    /// The C library's `sin`: on this machine glibc's, as LuaJIT's
    /// `math.sin` calls it (`vm_x64.dasc`: `math_extern sin`).
    #[link_name = "sin"]
    fn libm_sin(x: f64) -> f64;
}

impl LuaF64 {
    pub const ZERO: LuaF64 = LuaF64(0.0);

    /// A finite double; `None` for an infinity or NaN.
    pub fn new(v: f64) -> Option<Self> {
        v.is_finite().then_some(Self(v))
    }

    pub const fn from_i16(v: i16) -> Self {
        Self(v as f64)
    }

    /// The double itself.
    pub const fn value(self) -> f64 {
        self.0
    }

    pub const fn to_bits(self) -> u64 {
        self.0.to_bits()
    }

    /// The value as an `i16`, where it is an integer in range (`-0.0` is 0).
    pub fn as_i16(self) -> Option<i16> {
        let v = self.0;
        (v == v.floor() && (i16::MIN as f64..=i16::MAX as f64).contains(&v)).then(|| v as i16)
    }

    /// A cart literal as LuaJIT reads it (`lj_strscan`: correctly rounded):
    /// decimal digits with at most one point (`.75`, `0.70710678118`,
    /// `30`). Rust's `str::parse::<f64>` is correctly rounded too, so the two
    /// agree on every such literal; anything else (an exponent, hex, `inf`)
    /// is refused rather than guessed.
    pub fn parse(text: &str) -> Result<Self> {
        let s = text.trim();
        let (int, frac) = s.split_once('.').unwrap_or((s, ""));
        if (int.is_empty() && frac.is_empty()) || !int.bytes().chain(frac.bytes()).all(|b| b.is_ascii_digit()) {
            bail!("{text:?} is not a decimal literal (the float arithmetic reads digits with at most one point)");
        }
        let v: f64 = s.parse()?;
        Self::new(v).ok_or_else(|| anyhow::anyhow!("the literal {text:?} is not a finite double"))
    }

    fn fin(v: f64) -> Option<Self> {
        Self::new(v)
    }

    /// `a + b`, or `None` where it is not finite.
    pub fn checked_add(self, b: Self) -> Option<Self> {
        Self::fin(self.0 + b.0)
    }

    pub fn checked_sub(self, b: Self) -> Option<Self> {
        Self::fin(self.0 - b.0)
    }

    pub fn checked_mul(self, b: Self) -> Option<Self> {
        Self::fin(self.0 * b.0)
    }

    /// `a / b`: IEEE division; `None` for a zero divisor (an infinity or NaN).
    pub fn checked_div(self, b: Self) -> Option<Self> {
        Self::fin(self.0 / b.0)
    }

    /// Lua's `a % b` as LuaJIT computes it: `a - floor(a / b) * b`, three
    /// IEEE operations in that order (`vm_mod`; for `|a / b| >= 2^52` the
    /// quotient is already integral and `floor` is the identity, the same
    /// value). `None` for a zero divisor.
    pub fn checked_rem(self, b: Self) -> Option<Self> {
        let q = Self::fin(self.0 / b.0)?;
        let f = q.0.floor();
        let p = Self::fin(f * b.0)?;
        Self::fin(self.0 - p.0)
    }

    /// Unary minus: exact (`-0.0` for `0.0`).
    pub fn neg(self) -> Self {
        Self(-self.0)
    }

    /// `math.abs`: exact.
    pub fn abs(self) -> Self {
        Self(self.0.abs())
    }

    /// `math.floor`: exact.
    pub fn flr(self) -> Self {
        Self(self.0.floor())
    }

    /// The tools' `sin(x)`: `-math.sin((x * math.pi) * 2)`, the product
    /// left to right as Lua evaluates it, then glibc's `sin`, then the
    /// negation. `math.pi` is the double nearest pi. `None` only for a
    /// non-finite product.
    pub fn lua_sin(self) -> Option<Self> {
        let turns = Self::fin(self.0 * std::f64::consts::PI)?;
        let radians = Self::fin(turns.0 * 2.0)?;
        // `black_box`: the argument is a run-time value, never folded by the
        // compiler with another libm.
        let s = unsafe { libm_sin(std::hint::black_box(radians.0)) };
        Self::fin(-s)
    }

    /// The tools' `min(a, b)`: `a` if `a < b`, else `b` (on equal values,
    /// `-0.0` and `0.0` included, the SECOND argument).
    pub fn lua_min(self, b: Self) -> Self {
        if self.0 < b.0 {
            self
        } else {
            b
        }
    }

    /// The tools' `max(a, b)`: `a` if `a > b`, else `b`.
    pub fn lua_max(self, b: Self) -> Self {
        if self.0 > b.0 {
            self
        } else {
            b
        }
    }

    /// IEEE `<` (finite values: a total preorder, `-0.0 == 0.0`).
    pub fn lt(self, b: Self) -> bool {
        self.0 < b.0
    }

    pub fn le(self, b: Self) -> bool {
        self.0 <= b.0
    }

    /// Lua's `==` on numbers: IEEE equality (`-0.0 == 0.0`).
    pub fn eq_value(self, b: Self) -> bool {
        self.0 == b.0
    }

    /// The next double toward minus infinity (finite in, finite out; `None`
    /// below `-f64::MAX`).
    pub fn pred(self) -> Option<Self> {
        Self::fin(self.0.next_down())
    }

    /// The next double toward plus infinity.
    pub fn succ(self) -> Option<Self> {
        Self::fin(self.0.next_up())
    }

    /// The double in `printh` style for messages: shortest round-trip.
    pub fn show(self) -> String {
        format!("{:?}", self.0)
    }
}

/// A closed interval `[lo, hi]` of `LuaF64`s (`lo <= hi` by value): the
/// float reference domain's number. Its operations bound the IEEE result
/// over the whole box, with the same rounding at the ends (monotone
/// operations; plans/float-solver.md, section 4).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LuaF64Interval {
    pub lo: LuaF64,
    pub hi: LuaF64,
}

impl LuaF64Interval {
    pub fn new(lo: LuaF64, hi: LuaF64) -> Self {
        assert!(lo.le(hi), "interval {lo:?} > {hi:?}");
        Self { lo, hi }
    }

    pub fn point(v: LuaF64) -> Self {
        Self { lo: v, hi: v }
    }

    /// The single value, where the two ends are the same BITS (`[-0.0, 0.0]`
    /// holds two values).
    pub fn as_point(self) -> Option<LuaF64> {
        (self.lo == self.hi).then_some(self.lo)
    }

    /// Every finite double.
    pub fn unknown() -> Self {
        Self { lo: LuaF64(-f64::MAX), hi: LuaF64(f64::MAX) }
    }

    fn of(lo: f64, hi: f64) -> Option<Self> {
        Some(Self::new(LuaF64::new(lo)?, LuaF64::new(hi)?))
    }

    pub fn checked_add(self, b: Self) -> Option<Self> {
        Self::of(self.lo.0 + b.lo.0, self.hi.0 + b.hi.0)
    }

    pub fn checked_sub(self, b: Self) -> Option<Self> {
        Self::of(self.lo.0 - b.hi.0, self.hi.0 - b.lo.0)
    }

    /// The four corner products, each rounded: rounding is monotone, and on
    /// a box the exact product's extremes are corners.
    pub fn checked_mul(self, b: Self) -> Option<Self> {
        let c = [self.lo.0 * b.lo.0, self.lo.0 * b.hi.0, self.hi.0 * b.lo.0, self.hi.0 * b.hi.0];
        Self::of(c.iter().copied().fold(f64::INFINITY, f64::min), c.iter().copied().fold(f64::NEG_INFINITY, f64::max))
    }

    /// `None` where the divisor's interval holds 0 (of either sign).
    pub fn checked_div(self, b: Self) -> Option<Self> {
        if b.lo.0 <= 0.0 && 0.0 <= b.hi.0 {
            return None;
        }
        let c = [self.lo.0 / b.lo.0, self.lo.0 / b.hi.0, self.hi.0 / b.lo.0, self.hi.0 / b.hi.0];
        Self::of(c.iter().copied().fold(f64::INFINITY, f64::min), c.iter().copied().fold(f64::NEG_INFINITY, f64::max))
    }

    pub fn neg(self) -> Self {
        Self { lo: self.hi.neg(), hi: self.lo.neg() }
    }

    pub fn abs(self) -> Self {
        if self.lo.0 >= 0.0 {
            self
        } else if self.hi.0 <= 0.0 {
            self.neg()
        } else {
            Self { lo: LuaF64(0.0), hi: self.lo.abs().lua_max(self.hi) }
        }
    }

    /// How many integer floors the interval spans; `None` past 65536.
    pub fn floors(self) -> Option<u32> {
        let n = self.hi.0.floor() - self.lo.0.floor() + 1.0;
        (n <= 65536.0).then_some(n as u32)
    }

    /// The part in the `k`-th floor: `[max(lo, b), min(hi, pred(b + 1))]`,
    /// `b = floor(lo) + k` (integers: exact).
    pub fn floor_fragment(self, k: u32) -> Self {
        let b = self.lo.0.floor() + k as f64;
        Self { lo: self.lo.lua_max(LuaF64(b)), hi: self.hi.lua_min(LuaF64((b + 1.0).next_down())) }
    }

    /// `flr(fl(x + 1/2))`, as `move` computes the pixel amount.
    fn round_of(x: f64) -> f64 {
        (x + 0.5).floor()
    }

    /// The first double `c` with `flr(fl(c + 1/2)) >= t`: the candidate
    /// `t - 1/2`, moved to its neighbours by the operation itself (one step
    /// at most for the cart's values: `0.5 - 2^-54` rounds up to 1, so the
    /// cut below 1 is there, not at 0.5). More than four steps raise.
    fn round_cut(t: f64) -> Result<f64> {
        let mut c = t - 0.5;
        for _ in 0..4 {
            if Self::round_of(c) < t {
                c = c.next_up();
            } else if Self::round_of(c.next_down()) >= t {
                c = c.next_down();
            } else {
                return Ok(c);
            }
        }
        bail!("the round split's cut for {t} does not settle near {}", t - 0.5)
    }

    /// How many values `flr(fl(x + 1/2))` takes over the interval (it is
    /// monotone in `x`); `None` past 65536.
    pub fn rounds(self) -> Result<Option<u32>> {
        let n = Self::round_of(self.hi.0) - Self::round_of(self.lo.0) + 1.0;
        Ok((n <= 65536.0).then_some(n as u32))
    }

    /// The part where `flr(fl(x + 1/2))` takes its `k`-th value: between the
    /// exact cut points of that value and the next.
    pub fn round_fragment(self, k: u32) -> Result<Self> {
        let t = Self::round_of(self.lo.0) + k as f64;
        let (lo, hi) = (Self::round_cut(t)?, Self::round_cut(t + 1.0)?.next_down());
        Ok(Self { lo: self.lo.lua_max(LuaF64(lo)), hi: self.hi.lua_min(LuaF64(hi)) })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn n(v: f64) -> LuaF64 {
        LuaF64::new(v).unwrap()
    }

    /// Literals are correctly rounded, as LuaJIT's scanner reads them: the
    /// cart's constants, against their known nearest doubles.
    #[test]
    fn literals_are_the_nearest_double() {
        for (text, bits) in [
            ("0.6", 0x3fe3_3333_3333_3333u64),
            (".75", 0x3fe8_0000_0000_0000),
            ("0.70710678118", 0.70710678118f64.to_bits()),
            ("0.21", 0x3fca_e147_ae14_7ae1),
            ("0.05", 0x3fa9_9999_9999_999a),
            ("30", 0x403e_0000_0000_0000),
        ] {
            assert_eq!(LuaF64::parse(text).unwrap().to_bits(), bits, "{text}");
        }
        for bad in ["", ".", "1e5", "0x10", "inf", "nan", "-1", "1.2.3"] {
            assert!(LuaF64::parse(bad).is_err(), "{bad:?} refused");
        }
    }

    /// `%` is three IEEE operations in Lua's order, and refuses a zero
    /// divisor; on the cart's integers it is the usual remainder.
    #[test]
    fn rem_is_lua_floor_mod() {
        assert_eq!(n(7.0).checked_rem(n(3.0)), Some(n(1.0)));
        assert_eq!(n(-7.0).checked_rem(n(3.0)), Some(n(2.0)));
        assert_eq!(n(7.0).checked_rem(n(-3.0)), Some(n(-2.0)));
        assert_eq!(n(5.5).checked_rem(n(8.0)), Some(n(5.5)));
        let (a, b) = (0.1f64, 0.03f64);
        assert_eq!(n(a).checked_rem(n(b)).unwrap().to_bits(), (a - (a / b).floor() * b).to_bits());
        assert_eq!(n(1.0).checked_rem(n(0.0)), None);
    }

    /// `min`/`max` return the second argument on equal values: `-0.0` and
    /// `0.0` show which.
    #[test]
    fn min_max_take_the_second_on_ties() {
        let (pz, nz) = (n(0.0), n(-0.0));
        assert_eq!(pz.lua_min(nz).to_bits(), nz.to_bits());
        assert_eq!(nz.lua_min(pz).to_bits(), pz.to_bits());
        assert_eq!(pz.lua_max(nz).to_bits(), nz.to_bits());
        assert_eq!(n(1.0).lua_min(n(2.0)), n(1.0));
        assert_eq!(n(1.0).lua_max(n(2.0)), n(2.0));
    }

    /// `sin` is the tools' expression: negated, in turns, through libm.
    #[test]
    fn sin_is_negated_turns() {
        assert_eq!(n(0.0).lua_sin().unwrap().to_bits(), (-0.0f64).to_bits());
        assert_eq!(n(0.25).lua_sin(), Some(n(-1.0)));
        assert_eq!(n(0.75).lua_sin(), Some(n(1.0)));
        let x = 0.37f64;
        assert_eq!(n(x).lua_sin().unwrap().to_bits(), (-((x * std::f64::consts::PI) * 2.0).sin()).to_bits());
    }

    /// Non-finite results are refused, never carried.
    #[test]
    fn non_finite_results_raise() {
        let big = n(f64::MAX);
        assert_eq!(big.checked_add(big), None);
        assert_eq!(big.checked_mul(n(2.0)), None);
        assert_eq!(n(1.0).checked_div(n(0.0)), None);
        assert_eq!(LuaF64::new(f64::NAN), None);
    }

    /// The round split's cuts are where `flr(x + 1/2)` really changes:
    /// `k - 1/2` but below 1, where `0.5 - 2^-54 + 0.5` rounds to 1.0 (a
    /// tie, to even); every fragment holds one value of it, and its ends'
    /// neighbours outside hold another.
    #[test]
    fn round_cuts_are_exact() {
        let r = |x: f64| (x + 0.5).floor();
        assert_eq!(LuaF64Interval::round_cut(1.0).unwrap(), 0.5f64.next_down());
        assert_eq!(LuaF64Interval::round_cut(0.0).unwrap(), -0.5);
        assert_eq!(LuaF64Interval::round_cut(2.0).unwrap(), 1.5);
        assert_eq!(LuaF64Interval::round_cut(-3.0).unwrap(), -3.5);
        let i = LuaF64Interval::new(n(-0.5), n(0.5f64.next_down()));
        let k = i.rounds().unwrap().unwrap();
        assert_eq!(k, 2, "0.5 - 2^-54 already rounds to 1");
        for j in 0..k {
            let f = i.round_fragment(j).unwrap();
            assert_eq!(r(f.lo.0), r(f.hi.0));
            if f.lo != i.lo {
                assert!(r(f.lo.0.next_down()) < r(f.lo.0));
            }
            if f.hi != i.hi {
                assert!(r(f.hi.0.next_up()) > r(f.hi.0));
            }
        }
    }

    /// The remainder `move` leaves stays in [-0.5, 0.5): random remainders
    /// and speeds, and every remainder within 64 doubles of the cuts.
    #[test]
    fn the_move_remainder_stays_in_range() {
        let step = |r: f64, ox: f64| {
            let r1 = r + ox;
            let k = (r1 + 0.5).floor();
            r1 - k
        };
        let mut x = 0x2545_f491_4f6c_dd1du64;
        let mut rnd = || {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            (x >> 11) as f64 / (1u64 << 53) as f64
        };
        let mut edges = vec![];
        for c in [-0.5f64, 0.5f64.next_down(), 0.0] {
            let mut v = c;
            for _ in 0..64 {
                edges.push(v);
                v = v.next_down();
            }
            let mut v = c;
            for _ in 0..64 {
                edges.push(v);
                v = v.next_up();
            }
        }
        for _ in 0..200_000 {
            let r = rnd() - 0.5;
            let ox = (rnd() - 0.5) * 16.0;
            let r2 = step(r, ox);
            assert!((-0.5..0.5).contains(&r2), "rem {r:e} + ox {ox:e} left {r2:e}");
        }
        for r in edges.into_iter().filter(|r| (-0.5..0.5).contains(r)) {
            for ox in [0.0, 0.6, -0.6, 1.0, -2.0, 0.21, 5.0, -3.5355339059327378] {
                let r2 = step(r, ox);
                assert!((-0.5..0.5).contains(&r2), "rem {r:e} + ox {ox:e} left {r2:e}");
            }
        }
    }

    #[test]
    fn as_i16_takes_integers_in_range() {
        assert_eq!(n(-0.0).as_i16(), Some(0));
        assert_eq!(n(127.0).as_i16(), Some(127));
        assert_eq!(n(0.5).as_i16(), None);
        assert_eq!(n(40000.0).as_i16(), None);
    }
}
