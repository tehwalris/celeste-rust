// PICO-8's 16.16 fixed-point numbers. Every operation here was verified
// against a real PICO-8 0.2.7a6 (`lua/probe/num_*`, the tests below): literal
// parsing, `+ - * / %`, the range edges (negation wraps, `abs` and `/`
// saturate) and `sin` (a table dumped from the console).
//
// OVERFLOW IS AN ERROR: the game never leaves the 16.16 range, so where the
// console would wrap (`+ - *`, unary minus, a literal's integer part) or
// saturate (`/`, `abs`) the exact result does not exist. No `+ - * /`
// OPERATORS: `checked_*` give the exact result or `None`; the console's own
// `wrapping_*` / `saturating_*` are spelled out for the kernels (which
// decline the lane where the checked operation fails, `Op::NoWrap`) and the
// tests that pin them. `%` never overflows and keeps its operator.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::{fmt, ops::Rem, str::FromStr};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Pico8Num(i32);

/// The console's `sin`, tabulated: little-endian `i32` in 16.16, one entry
/// per distinguishable input of the first half-turn (`pico8_sin`;
/// `cart/pico8_sin_table.p8` dumps it).
static SIN_TABLE: [i32; 16384] = {
    let bytes = *include_bytes!("../../../cart/pico8_sin_table.bin");
    let mut table = [0i32; 16384];
    let mut i = 0;
    while i < 16384 {
        table[i] = i32::from_le_bytes([
            bytes[i * 4],
            bytes[i * 4 + 1],
            bytes[i * 4 + 2],
            bytes[i * 4 + 3],
        ]);
        i += 1;
    }
    table
};

impl Pico8Num {
    pub const fn from_i16(v: i16) -> Self {
        Self((v as i32) << 16)
    }

    /// The inverse of `to_bits`. Every bit pattern is a valid value.
    pub const fn from_raw(bits: i32) -> Self {
        Self(bits)
    }

    /// The raw fixed-point bits. One encoding per value, so comparing bits is
    /// comparing numbers (dedup relies on it).
    pub const fn to_bits(&self) -> u32 {
        self.0 as u32
    }

    pub const fn as_i16(&self) -> Option<i16> {
        if self.0 & 0xffff == 0 {
            Some((self.0 >> 16) as i16)
        } else {
            None
        }
    }

    pub const fn whole_part_as_i16(&self) -> i16 {
        (self.0 >> 16) as i16
    }

    pub const fn fraction_part_as_u16(&self) -> u16 {
        (self.0 & 0xffff) as u16
    }

    pub fn as_i16_or_err(&self) -> Result<i16> {
        self.as_i16()
            .ok_or_else(|| anyhow!("got {:?}, expected integer", self))
    }

    pub fn from_parts(whole_n: i16, fraction_n: u16) -> Self {
        let pico_n = Self(((whole_n as i32) << 16) | (fraction_n as i32));
        assert_eq!(pico_n.whole_part_as_i16(), whole_n);
        assert_eq!(pico_n.fraction_part_as_u16(), fraction_n);
        pico_n
    }

    pub const fn as_raw_u32(&self) -> u32 {
        self.0 as u32
    }

    /// A decimal literal's EXACT raw value, as the console reads it:
    /// multiplied by 65536 and TRUNCATED toward zero (the console then lets
    /// an out-of-range integer part WRAP: `65535` is -1, which `FromStr`
    /// refuses). In `i128` on the digits, not floating point (f32 has too
    /// few bits, and Rust's float-to-int casts saturate), so it is exact for
    /// any input length; `None` for a malformed literal or one past `i128`.
    fn parse_decimal(s: &str) -> Option<i128> {
        let (negative, digits) = match s.strip_prefix('-') {
            Some(rest) => (true, rest),
            None => (false, s),
        };
        let (int_text, frac_text) = match digits.split_once('.') {
            Some((i, f)) => (i, f),
            None => (digits, ""),
        };
        if int_text.is_empty() && frac_text.is_empty() {
            return None;
        }
        if !int_text.bytes().chain(frac_text.bytes()).all(|b| b.is_ascii_digit()) {
            return None;
        }
        let int_part: i128 = if int_text.is_empty() { 0 } else { int_text.parse().ok()? };
        // floor(0.frac * 65536), exactly: the fraction is frac/10^len.
        let mut frac_raw: i128 = 0;
        if !frac_text.is_empty() {
            let scaled: i128 = frac_text.parse().ok()?;
            let denominator = 10i128.checked_pow(u32::try_from(frac_text.len()).ok()?)?;
            frac_raw = scaled.checked_mul(65536)? / denominator;
        }
        let raw = int_part.checked_mul(1 << 16)?.checked_add(frac_raw)?;
        Some(if negative { -raw } else { raw })
    }

    /// The console's reading of a literal, which WRAPS a whole part past the
    /// range (`32768` reads as -32768, `65535` as -1; `FromStr` refuses
    /// those): for the tests that pin the console (`lua/probe`'s overflow
    /// corpora).
    pub fn wrapping_from_str(s: &str) -> Option<Self> {
        Self::parse_decimal(s.trim()).map(|raw| Self(raw as i32))
    }

    /// `i64` raw 16.16 as a number, `None` where it leaves the range.
    fn fit(raw: i64) -> Option<Self> {
        i32::try_from(raw).ok().map(Self)
    }

    /// `+`, the exact sum: `None` where the console WRAPS it.
    pub const fn checked_add(self, rhs: Self) -> Option<Self> {
        match self.0.checked_add(rhs.0) {
            Some(v) => Some(Self(v)),
            None => None,
        }
    }

    /// `-`, the exact difference: `None` where the console WRAPS it.
    pub const fn checked_sub(self, rhs: Self) -> Option<Self> {
        match self.0.checked_sub(rhs.0) {
            Some(v) => Some(Self(v)),
            None => None,
        }
    }

    /// `*`, the 32-bit-shifted product floored (`>> 16`): `None` where the
    /// console WRAPS it.
    pub fn checked_mul(self, rhs: Self) -> Option<Self> {
        Self::fit((self.0 as i64 * rhs.0 as i64) >> 16)
    }

    /// `/`, the quotient truncated toward zero: `None` where the console
    /// SATURATES it, a zero divisor included.
    pub fn checked_div(self, rhs: Self) -> Option<Self> {
        if rhs.0 == 0 {
            return None;
        }
        Self::fit(((self.0 as i64) << 16) / rhs.0 as i64)
    }

    /// Unary minus: `None` for `-32768`, which the console WRAPS to itself.
    pub const fn checked_neg(self) -> Option<Self> {
        match self.0.checked_neg() {
            Some(v) => Some(Self(v)),
            None => None,
        }
    }

    /// `abs`: `None` for `-32768`, which the console SATURATES.
    pub const fn checked_abs(self) -> Option<Self> {
        match self.0.checked_abs() {
            Some(v) => Some(Self(v)),
            None => None,
        }
    }

    /// The console's `+`, which WRAPS. For the kernels' lanes (whose
    /// `NoWrap` declines where `checked_add` fails) and the tests.
    pub const fn wrapping_add(self, rhs: Self) -> Self {
        Self(self.0.wrapping_add(rhs.0))
    }

    /// The console's `-`, which WRAPS.
    pub const fn wrapping_sub(self, rhs: Self) -> Self {
        Self(self.0.wrapping_sub(rhs.0))
    }

    /// The console's `*`, which WRAPS.
    pub const fn wrapping_mul(self, rhs: Self) -> Self {
        let high = (self.0 as i64).wrapping_mul(rhs.0 as i64);
        let low = high >> 16;
        Self(low as i32)
    }

    /// The console's unary minus, which WRAPS: `-(-32768) == -32768`.
    pub const fn wrapping_neg(self) -> Self {
        Self(self.0.wrapping_neg())
    }

    /// The console's `abs`, which SATURATES, unlike negation:
    /// `abs(-32768) == 0x7fff.ffff` while `-(-32768) == -32768`.
    pub const fn saturating_abs(self) -> Self {
        Self(self.0.saturating_abs())
    }

    /// The console's `/`: truncate toward zero, and SATURATE to
    /// +/-0x7fff.ffff if the exact quotient does not fit - including
    /// division by zero (`1/0 = 0x7fff.ffff`, `-1/0 = 0x8000.0001`, `0/0 =
    /// 0x7fff.ffff`). The clamp is for overflow only: `min/1 = 0x8000.0000`
    /// fits and is returned as is.
    pub fn saturating_div(self, rhs: Self) -> Self {
        const LIMIT: i32 = 0x7fff_ffff;
        if rhs.0 == 0 {
            // 0/0 saturates POSITIVE on the console, so `>= 0` not `> 0`.
            return Self(if self.0 >= 0 { LIMIT } else { -LIMIT });
        }
        let quotient = ((self.0 as i64) << 16) / (rhs.0 as i64);
        Self(match i32::try_from(quotient) {
            Ok(exact) => exact,
            Err(_) if quotient > 0 => LIMIT,
            Err(_) => -LIMIT,
        })
    }

    /// The `x` whose `x * k` fits the 16.16 range (`checked_mul`), as `[lo,
    /// hi]`: every `x` for `|k| <= 1`. An interval multiplies without a wrap
    /// iff both its ends lie inside (the product is monotone in `x`). The
    /// kernels' check for it (`Op::NoWrap`) is those compares.
    pub fn mul_fits(k: Self) -> (Self, Self) {
        Self::fitting(|x| x.checked_mul(k).is_some())
    }

    /// `mul_fits` for `x / k` (`checked_div`); `None` for `k == 0`, where
    /// no `x` does (the console saturates).
    pub fn div_fits(k: Self) -> Option<(Self, Self)> {
        (k.0 != 0).then(|| Self::fitting(|x| x.checked_div(k).is_some()))
    }

    /// The `[lo, hi]` where a map of `x` fits: `fits(0)` holds, and moving
    /// away from 0 only grows the image's magnitude, so the set is an
    /// interval about 0. Bisected on the checked operation itself, so it
    /// agrees with it by construction.
    fn fitting(fits: impl Fn(Self) -> bool) -> (Self, Self) {
        assert!(fits(Self(0)), "zero maps to zero");
        // The last `x` from 0 toward `end` that fits (`fits` is a prefix).
        let edge = |end: i32| -> i32 {
            let (mut good, mut bad) = (0i64, end as i64);
            if fits(Self(end)) {
                return end;
            }
            while (bad - good).abs() > 1 {
                let mid = good + (bad - good) / 2;
                if fits(Self(mid as i32)) {
                    good = mid;
                } else {
                    bad = mid;
                }
            }
            good as i32
        };
        (Self(edge(i32::MIN)), Self(edge(i32::MAX)))
    }

    pub const fn flr(self) -> Self {
        Self::from_i16((self.0 >> 16) as i16)
    }

    /// PICO-8 `sin`: the argument is in TURNS and the result is INVERTED
    /// (sin(0.25) == -1). A table dumped from a real console, not a formula
    /// (`-sinf(2πx)` is off by up to 13 ulps).
    ///
    /// 16384 entries cover all 65536 inputs because, measured over every
    /// input, the console's `sin` is constant over pairs of adjacent inputs
    /// and `sin(x + 0.5) == -sin(x)` exactly (but `sin(0.5 - x) == sin(x)`
    /// does NOT hold). Periodicity mod one turn is exact by construction,
    /// which the "strawberry bob" widening depends on.
    pub fn pico8_sin(self) -> Self {
        // Two's-complement masking IS floored mod 1.0: -0.25 -> 0.75.
        let frac = (self.0 & 0xffff) as u32;
        let entry = SIN_TABLE[((frac & 0x7fff) >> 1) as usize];
        Self(if frac < 0x8000 { entry } else { -entry })
    }
}


impl fmt::Debug for Pico8Num {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Pico8Num")
            .field(&format!("{:#06x}", self.0))
            .finish()
    }
}


impl Rem for Pico8Num {
    type Output = Self;

    /// PICO-8's `%` is exactly `i32::rem_euclid` on the raw bits, with
    /// `a % 0 == 0`: total, and never negative. NOT Lua's rule (Lua's
    /// result takes the divisor's sign: `7 % -3` is -2 in Lua, 1 here).
    /// `-32768 % -0x0.0001` is the exact remainder 0 (unmeasured on the
    /// console; `rem_euclid` would panic on its quotient's overflow).
    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            return Self(0);
        }
        Self(self.0.wrapping_rem_euclid(rhs.0))
    }
}

impl FromStr for Pico8Num {
    type Err = anyhow::Error;

    /// A literal's exact value; one whose integer part leaves the 16.16
    /// range (which the console WRAPS: `32768` is -32768) is an error.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let raw = Self::parse_decimal(s.trim()).ok_or_else(|| anyhow::anyhow!("{:?} is not a PICO-8 number literal", s))?;
        i64::try_from(raw)
            .ok()
            .and_then(Self::fit)
            .ok_or_else(|| anyhow::anyhow!("the literal {s:?} leaves the 16.16 range (PICO-8 would wrap it; overflow is an error)"))
    }
}

/// An interval [low, high] of `Pico8Num`s, `low <= high`: an abstract value.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Pico8NumInterval {
    pub low: Pico8Num,
    pub high: Pico8Num,
}

impl Pico8NumInterval {
    pub fn new(low: Pico8Num, high: Pico8Num) -> Self {
        assert!(low <= high, "Interval low must be <= high");
        Self { low, high }
    }

    pub fn from_number(n: Pico8Num) -> Self {
        Self { low: n, high: n }
    }

    pub fn to_number(&self) -> Option<Pico8Num> {
        if self.low == self.high {
            Some(self.low)
        } else {
            None
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            low: std::cmp::min(self.low, other.low),
            high: std::cmp::max(self.high, other.high),
        }
    }

}

impl Pico8NumInterval {
    /// The interval of two `i64` raw endpoints, `None` where one leaves the
    /// 16.16 range. Endpoint arithmetic is sound only when no value in the
    /// interval wraps (one wrapped endpoint makes the true result two
    /// disjoint segments); the `i64` extremes bound every value, so checking
    /// them is a complete wrap detector.
    fn try_from_i64_endpoints(low: i64, high: i64) -> Option<Self> {
        if low >= i32::MIN as i64 && high <= i32::MAX as i64 {
            Some(Self::new(Pico8Num(low as i32), Pico8Num(high as i32)))
        } else {
            None
        }
    }

    /// `+` and `-`, `None` where an endpoint WRAPS.
    pub fn checked_add(self, rhs: Self) -> Option<Self> {
        Self::try_from_i64_endpoints(
            self.low.0 as i64 + rhs.low.0 as i64,
            self.high.0 as i64 + rhs.high.0 as i64,
        )
    }

    pub fn checked_sub(self, rhs: Self) -> Option<Self> {
        Self::try_from_i64_endpoints(
            self.low.0 as i64 - rhs.high.0 as i64,
            self.high.0 as i64 - rhs.low.0 as i64,
        )
    }

    /// Negation, which wraps on exactly one input: `i32::MIN`.
    pub fn checked_neg(self) -> Option<Self> {
        Self::try_from_i64_endpoints(-(self.high.0 as i64), -(self.low.0 as i64))
    }

    /// `*` of any two intervals (points included), `None` where a product
    /// leaves the 16.16 range (the console WRAPS it). The product is
    /// bilinear and the 16.16 shift monotone, so the corners bound the set;
    /// a point's product is `Pico8Num`'s `*` wherever that does not wrap.
    pub fn checked_mul(self, rhs: Self) -> Option<Self> {
        let p = |a: Pico8Num, b: Pico8Num| ((a.0 as i64) * (b.0 as i64)) >> 16;
        let c = [p(self.low, rhs.low), p(self.low, rhs.high), p(self.high, rhs.low), p(self.high, rhs.high)];
        Self::try_from_i64_endpoints(*c.iter().min().expect("four"), *c.iter().max().expect("four"))
    }

    /// `/` of any two intervals, `None` where the divisor may be 0 or a
    /// quotient leaves the 16.16 range (the console SATURATES both). Off
    /// zero the quotient is monotone in each operand, and truncation toward
    /// zero is monotone, so the corners bound the set; a point's quotient is
    /// `Pico8Num`'s `/` wherever that does not saturate.
    pub fn checked_div(self, rhs: Self) -> Option<Self> {
        if rhs.low.0 <= 0 && 0 <= rhs.high.0 {
            return None;
        }
        let q = |a: Pico8Num, b: Pico8Num| ((a.0 as i64) << 16) / (b.0 as i64);
        let c = [q(self.low, rhs.low), q(self.low, rhs.high), q(self.high, rhs.low), q(self.high, rhs.high)];
        Self::try_from_i64_endpoints(*c.iter().min().expect("four"), *c.iter().max().expect("four"))
    }

    /// `abs`, `None` where the set holds `i32::MIN` (the console SATURATES
    /// `abs(-32768)` to `0x7fff.ffff`).
    pub fn checked_abs(self) -> Option<Self> {
        let zero = Pico8Num(0);
        // `low` is not MIN past here, nor is `high <= 0` (`low <= high`).
        let low = self.low.checked_abs()?;
        if self.low >= zero {
            Some(self)
        } else if self.high <= zero {
            Some(Self::new(self.high.checked_abs()?, low))
        } else {
            Some(Self::new(zero, low.max(self.high)))
        }
    }
}

impl fmt::Debug for Pico8NumInterval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Interval[{:?}, {:?}]", self.low, self.high)
    }
}

#[cfg(test)]
mod tests {
    use crate::pico8_num::Pico8Num;

    const fn int(v: i16) -> Pico8Num {
        Pico8Num::from_i16(v)
    }

    /// 0.15 in 16.16.
    const P0_15: Pico8Num = Pico8Num(0x0000_2666);

    /// PICO-8's `%` is Euclidean (never negative), and the fixed-point
    /// fraction participates.
    #[test]
    fn test_checked_rem_is_floored_modulo() {
        let n = Pico8Num::from_i16;
        let eight = n(8);
        assert_eq!(n(10) % eight, n(2));
        assert_eq!(n(-2) % eight, n(6));
        assert_eq!(n(-16) % eight, n(0));
        let half = Pico8Num::from_parts(2, 0x8000); // 2.5
        assert_eq!(half % eight, half);
        let neg_half = half.wrapping_neg(); // -2.5
        assert_eq!(neg_half % eight, Pico8Num::from_parts(5, 0x8000)); // 5.5
        // Zero, negative and fractional divisors, measured on a real
        // PICO-8 0.2.7a6:
        assert_eq!(n(1) % n(0), n(0), "1 % 0");
        assert_eq!(n(0) % n(0), n(0), "0 % 0");
        assert_eq!(n(-1) % n(0), n(0), "-1 % 0");
        assert_eq!(n(1) % n(-8), n(1), "1 % -8");
        assert_eq!(n(1) % half, n(1), "1 % 2.5");
        assert_eq!(n(7) % half, n(2), "7 % 2.5");
        assert_eq!(n(-7) % half, Pico8Num::from_parts(0, 0x8000), "-7 % 2.5");
    }

    #[test]
    fn test_from_i16() {
        let cases: Vec<(i16, u32)> = vec![(-1, 0xffff_0000), (0, 0x0000_0000), (1, 0x0001_0000)];
        for (i16, raw_u32) in cases {
            let pico_n = Pico8Num::from_i16(i16);
            assert_eq!(pico_n.as_raw_u32(), raw_u32);
            assert_eq!(pico_n.whole_part_as_i16(), i16);
            assert_eq!(pico_n.fraction_part_as_u16(), 0);
        }
    }

    /// `a + b`, `a - b`, `a * b`, `a / b`, `abs(a)` that fit.
    fn add(a: Pico8Num, b: Pico8Num) -> Pico8Num {
        a.checked_add(b).expect("fits")
    }
    fn sub(a: Pico8Num, b: Pico8Num) -> Pico8Num {
        a.checked_sub(b).expect("fits")
    }
    fn mul(a: Pico8Num, b: Pico8Num) -> Pico8Num {
        a.checked_mul(b).expect("fits")
    }
    fn div(a: Pico8Num, b: Pico8Num) -> Pico8Num {
        a.checked_div(b).expect("fits")
    }
    fn abs(a: Pico8Num) -> Pico8Num {
        a.checked_abs().expect("fits")
    }

    #[test]
    fn test_mul() {
        assert_eq!(mul(int(25), int(4)).as_i16(), Some(100));
        assert_eq!(mul(int(-25), int(4)).as_i16(), Some(-100));
    }

    #[test]
    fn test_div() {
        assert_eq!(div(int(100), int(4)).as_i16(), Some(25));
        assert_eq!(div(int(-100), int(4)).as_i16(), Some(-25));
        assert_eq!(div(int(-100), int(7)).as_i16(), None);
    }

    #[test]
    fn test_flr() {
        assert_eq!(add(int(4), P0_15).flr(), int(4));
        assert_eq!(int(4).flr(), int(4));
        assert_eq!(sub(int(-2), P0_15).flr(), int(-3));
        assert_eq!(int(-2).flr(), int(-2));
    }

    #[test]
    fn test_abs() {
        assert_eq!(abs(int(4)), int(4));
        assert_eq!(abs(int(-4)), int(4));
        assert_eq!(abs(add(int(4), P0_15)), add(int(4), P0_15));
        assert_eq!(abs(sub(int(-4), P0_15)), add(int(4), P0_15));
    }

    /// Pins the invariance the "strawberry bob" widening relies on: for every
    /// nonnegative integer `off`, `sin(off/40) == sin((off mod 40)/40)`
    /// bit-exactly, checked exhaustively rather than argued.
    #[test]
    fn test_pico8_sin_period_40_bit_exact() {
        let forty = Pico8Num::from_i16(40);
        for off in 0..=i16::MAX {
            let full = div(Pico8Num::from_i16(off), forty);
            let reduced = div(Pico8Num::from_i16(off % 40), forty);
            assert_eq!(
                full.pico8_sin(),
                reduced.pico8_sin(),
                "sin({}/40) != sin({}/40)",
                off,
                off % 40
            );
        }
    }

    #[test]
    fn test_pico8_sin_quarter_turns() {
        // PICO-8 sin is in turns and inverted; the quarter values are the
        // ones the console documents exactly.
        assert_eq!(int(0).pico8_sin(), int(0));
        assert_eq!(Pico8Num::from_parts(0, 0x4000).pico8_sin(), int(-1)); // sin(0.25)
        assert_eq!(Pico8Num::from_parts(0, 0x8000).pico8_sin(), int(0)); // sin(0.5)
        assert_eq!(Pico8Num::from_parts(0, 0xc000).pico8_sin(), int(1)); // sin(0.75)
        assert_eq!(int(1).pico8_sin(), int(0)); // full turn
        // Periodicity across whole turns for a fruit-style argument.
        let x = Pico8Num::from_parts(0, 1638); // off=1 -> 1/40 of a turn
        let y = Pico8Num::from_parts(3, 1638); // three turns later
        assert_eq!(x.pico8_sin(), y.pico8_sin());
        // Sign: just past 0 the inverted sine goes negative.
        assert!((x.pico8_sin().as_raw_u32() as i32) < 0);
    }

    /// Literal parsing, against a real PICO-8 0.2.7a6. `0.0000076` (under
    /// half an ulp) and `0.0000153` show truncation, not rounding; the
    /// console's `65535` and `32768` show the integer part wraps, not
    /// saturates, which is refused here.
    #[test]
    fn literals_parse_the_way_the_console_parses_them() {
        for (text, expected) in [
            ("0.1", 0x0000_1999_u32),
            ("0.3", 0x0000_4ccc),
            ("0.7", 0x0000_b333),
            ("32767.99998", 0x7fff_fffe),
            ("1000.00002", 0x03e8_0001),
            ("0.0000076", 0x0000_0000),
            ("0.0000153", 0x0000_0001),
            ("32767", 0x7fff_0000),
            ("-32768", 0x8000_0000),
            ("0.05", 0x0000_0ccc),
            ("1.5", 0x0001_8000),
        ] {
            let got: Pico8Num = text.parse().expect("a valid literal");
            assert_eq!(
                got.to_bits(),
                expected,
                "literal {} parsed as {:#010x}, console says {:#010x}",
                text,
                got.to_bits(),
                expected
            );
        }
        // The console WRAPS an integer part past the range (`32768` reads
        // as -32768, `65535` as -1): an error here, as any overflow, and the
        // console's reading spelled out.
        for (text, console) in [("32768", 0x8000_0000_u32), ("65535", 0xffff_0000), ("-32768.00002", 0x7fff_ffff), ("40000.5", 0x9c40_8000)] {
            let e = text.parse::<Pico8Num>().expect_err(text).to_string();
            assert!(e.contains("leaves the 16.16 range"), "{text}: {e}");
            assert_eq!(Pico8Num::wrapping_from_str(text).map(|n| n.to_bits()), Some(console), "{text}");
        }
        assert_eq!(Pico8Num::wrapping_from_str("0.1"), "0.1".parse().ok(), "in range, the same");
        assert!("".parse::<Pico8Num>().is_err());
        assert!("1.2.3".parse::<Pico8Num>().is_err());
        assert!("nope".parse::<Pico8Num>().is_err());
    }

    /// The fitting endpoints of a multiply / divide by a constant `k` of
    /// either sign are exactly where the checked operation stops returning
    /// `None`: the edges fit, one past them does not; a factor of magnitude
    /// at most 1 (a divisor at least 1, or below -1) fits everywhere, and
    /// nothing divides by 0.
    #[test]
    fn the_fitting_endpoints_are_where_the_checked_multiply_and_divide_stop_fitting() {
        let raw = |v: i32| Pico8Num(v);
        let fits = |div: bool, x: i32, k: i32| if div { raw(x).checked_div(raw(k)).is_some() } else { raw(x).checked_mul(raw(k)).is_some() };
        let ks = [1, 2, 3, 0x7fff, 0x8000, 0xffff, 0x1_0000, 0x1_0001, 0x1_8000, 0x2_0000, 0x3_0000, 0x40_0000, i32::MAX];
        for k in ks.into_iter().flat_map(|k| [k, -k]).chain([i32::MIN]) {
            for div in [false, true] {
                let (lo, hi) = if div { Pico8Num::div_fits(raw(k)).expect("k != 0") } else { Pico8Num::mul_fits(raw(k)) };
                let (lo, hi) = (lo.0, hi.0);
                assert!(fits(div, lo, k) && fits(div, hi, k) && fits(div, 0, k), "k {k:#x} div {div}: the edges fit");
                if lo > i32::MIN {
                    assert!(!fits(div, lo - 1, k), "k {k:#x} div {div}: below {lo:#x} wraps");
                }
                if hi < i32::MAX {
                    assert!(!fits(div, hi + 1, k), "k {k:#x} div {div}: above {hi:#x} wraps");
                }
                let everywhere = if div { k >= 0x1_0000 || k <= -0x1_0001 } else { (-0xffff..=0x1_0000).contains(&k) };
                assert_eq!((lo, hi) == (i32::MIN, i32::MAX), everywhere, "k {k:#x} div {div}");
            }
        }
        assert_eq!(Pico8Num::div_fits(raw(0)), None, "x / 0 never fits");
        assert_eq!(Pico8Num::mul_fits(raw(0)), (raw(i32::MIN), raw(i32::MAX)));
        // Times 2: [-16384, 16383.99998]. Over 0.5 the same; times -1 all but MIN.
        assert_eq!(Pico8Num::mul_fits(raw(0x2_0000)), (raw(i32::MIN / 2), raw(i32::MAX / 2)));
        assert_eq!(Pico8Num::div_fits(raw(0x8000)), Some((raw(i32::MIN / 2), raw(i32::MAX / 2))));
        assert_eq!(Pico8Num::mul_fits(raw(-0x1_0000)), (raw(i32::MIN + 1), raw(i32::MAX)));
    }

    /// The console's division and the +/-32768 edges, against a real PICO-8
    /// 0.2.7a6. `min/1` vs `min/-1` rules out a blanket clamp; `-min` vs
    /// `abs(min)` shows negation wraps while `abs` saturates.
    #[test]
    fn division_and_the_range_edges_match_the_console() {
        let raw = |bits: u32| Pico8Num(bits as i32);
        let min = int(-32768);
        for (label, got, expected) in [
            ("1/0", int(1).saturating_div(int(0)), 0x7fff_ffff_u32),
            ("-1/0", int(-1).saturating_div(int(0)), 0x8000_0001),
            ("0/0", int(0).saturating_div(int(0)), 0x7fff_ffff),
            ("32767/0.5", int(32767).saturating_div(raw(0x0000_8000)), 0x7fff_ffff),
            ("-32767/0.5", int(-32767).saturating_div(raw(0x0000_8000)), 0x8000_0001),
            ("min/1", min.saturating_div(int(1)), 0x8000_0000),
            ("min/-1", min.saturating_div(int(-1)), 0x7fff_ffff),
            ("min*-1", min.wrapping_mul(int(-1)), 0x8000_0000),
            ("-min", min.wrapping_neg(), 0x8000_0000),
            ("abs(min)", min.saturating_abs(), 0x7fff_ffff),
            ("-7/2", int(-7).saturating_div(int(2)), 0xfffc_8000),
            ("7/-2", int(7).saturating_div(int(-2)), 0xfffc_8000),
            ("1/3", int(1).saturating_div(int(3)), 0x0000_5555),
            ("-1/3", int(-1).saturating_div(int(3)), 0xffff_aaab),
        ] {
            assert_eq!(
                got.to_bits(),
                expected,
                "{} gave {:#010x}, console says {:#010x}",
                label,
                got.to_bits(),
                expected
            );
        }
    }

    /// `%` against a real PICO-8 0.2.7a6, all four sign combinations
    /// (`7 % -3 == 1`, where Lua gives -2).
    #[test]
    fn modulo_matches_the_console_in_every_sign_combination() {
        for (a, b, expected) in [
            (7, 3, 1),
            (-7, 3, 2),
            (7, -3, 1),
            (-7, -3, 2),
            (7, 0, 0),
            (-7, 0, 0),
        ] {
            let got = (int(a) % int(b)).to_bits();
            let want = int(expected).to_bits();
            assert_eq!(got, want, "{} % {} gave {:#010x}, want {:#010x}", a, b, got, want);
        }
        // The result is never negative, whatever the divisor's sign.
        for b in [-9i16, -1, 1, 9] {
            for a in [-100i16, -1, 0, 1, 100] {
                assert!(
                    (int(a) % int(b)).to_bits() as i32 >= 0,
                    "{} % {} came out negative",
                    a,
                    b
                );
            }
        }
    }

    /// Values read off a real PICO-8 0.2.7a6 with `tostr(sin(x), true)`.
    /// Interior points, because quarter turns are what a wrong model gets
    /// right by construction.
    #[test]
    fn pico8_sin_matches_the_console_at_interior_points() {
        // sin(i/40) for i = 1..7 - the fruit bob's own arguments.
        let want: [i32; 7] = [
            0xffffd7ea_u32 as i32,
            0xffffb0e9_u32 as i32,
            0xffff8bc3_u32 as i32,
            0xffff698f_u32 as i32,
            0xffff4afb_u32 as i32,
            0xffff30de_u32 as i32,
            0xffff1be9_u32 as i32,
        ];
        for (index, expected) in want.iter().enumerate() {
            let i = i16::try_from(index + 1).expect("small");
            let turns = div(int(i), int(40));
            assert_eq!(
                turns.pico8_sin().as_raw_u32() as i32,
                *expected,
                "sin({}/40) disagrees with the console",
                i
            );
        }
    }

    /// Raw values at and around every edge the 16.16 operations have: the
    /// range's ends, 0 and +/-1, the 16.16 one, and the products' and
    /// quotients' overflow thresholds (`0xb504f3` is ~sqrt(2^47)).
    const EDGES: [i32; 26] = [
        i32::MIN, i32::MIN + 1, i32::MIN + 2, -0x4000_0000, -0xb5_04f4, -0xb5_04f3,
        -0x1_0001, -0x1_0000, -0xffff, -0x8000, -2, -1, 0, 1, 2, 0x8000, 0xffff,
        0x1_0000, 0x1_0001, 0xb5_04f3, 0xb5_04f4, 0x4000_0000, 0x7fff_0000,
        i32::MAX - 2, i32::MAX - 1, i32::MAX,
    ];

    /// The exact result of `op` on two raw 16.16 values (`None`: division
    /// by zero), as the operation's definition gives it before the console
    /// wraps or saturates: `*` floors the 32-bit-shifted product, `/`
    /// truncates the quotient.
    fn exact(op: char, a: i32, b: i32) -> Option<i128> {
        let (a, b) = (a as i128, b as i128);
        match op {
            '+' => Some(a + b),
            '-' => Some(a - b),
            '*' => Some((a * b) >> 16),
            '/' => (b != 0).then(|| (a << 16) / b),
            _ => unreachable!(),
        }
    }

    fn fits(v: i128) -> bool {
        (i32::MIN as i128..=i32::MAX as i128).contains(&v)
    }

    /// At every pair of edge values, the checked operations on POINTS (and
    /// the interval ones on point intervals) are the console's result
    /// wherever the exact one fits, `None` exactly where it does not - where
    /// the console's differs from the exact result (it wrapped or saturated).
    #[test]
    fn checked_operations_on_points_are_the_consoles_where_they_fit_and_none_where_it_wraps() {
        use super::Pico8NumInterval as IV;
        let pt = |v: i32| IV::from_number(Pico8Num(v));
        for &a in &EDGES {
            for &b in &EDGES {
                for op in ['+', '-', '*', '/'] {
                    let (x, y) = (Pico8Num(a), Pico8Num(b));
                    let (console, point, checked) = match op {
                        '+' => (x.wrapping_add(y), x.checked_add(y), pt(a).checked_add(pt(b))),
                        '-' => (x.wrapping_sub(y), x.checked_sub(y), pt(a).checked_sub(pt(b))),
                        '*' => (x.wrapping_mul(y), x.checked_mul(y), pt(a).checked_mul(pt(b))),
                        _ => (x.saturating_div(y), x.checked_div(y), pt(a).checked_div(pt(b))),
                    };
                    assert_eq!(point.map(IV::from_number), checked, "{a:#x} {op} {b:#x}: the point and the interval operation agree");
                    match exact(op, a, b) {
                        Some(e) if fits(e) => {
                            assert_eq!(console.0 as i128, e, "{a:#x} {op} {b:#x}: the console's is the exact result");
                            assert_eq!(checked, Some(pt(console.0)), "{a:#x} {op} {b:#x}");
                        }
                        e => {
                            assert!(e.is_none_or(|e| console.0 as i128 != e), "{a:#x} {op} {b:#x}: an overflow the console does not show");
                            assert_eq!(checked, None, "{a:#x} {op} {b:#x}: the console gives {:#x}, not the exact result", console.0);
                        }
                    }
                }
            }
            // Negation wraps and `abs` saturates at MIN only.
            let x = Pico8Num(a);
            assert_eq!(x.checked_neg(), (a != i32::MIN).then(|| x.wrapping_neg()), "-{a:#x}");
            assert_eq!(x.checked_abs(), (a != i32::MIN).then(|| x.saturating_abs()), "abs({a:#x})");
            assert_eq!(pt(a).checked_neg(), x.checked_neg().map(IV::from_number), "-{a:#x}");
            assert_eq!(pt(a).checked_abs(), x.checked_abs().map(IV::from_number), "abs({a:#x})");
            if a == i32::MIN {
                assert_eq!((x.wrapping_neg().0, x.saturating_abs().0), (i32::MIN, i32::MAX), "the console wraps -MIN and saturates abs(MIN)");
            }
        }
    }

    /// Small intervals at every edge, against every pair of their points:
    /// a checked operation is `None` exactly when some pair's exact result
    /// does not fit (or divides by zero), and otherwise the hull of the
    /// pairs' results - every concrete value inside, both ends attained.
    #[test]
    fn checked_operations_on_intervals_hold_every_points_result_or_refuse() {
        use super::Pico8NumInterval as IV;
        let mut ivs = Vec::new();
        for &e in &EDGES {
            for w in 0..3 {
                ivs.push((e, e.saturating_add(w)));
                ivs.push((e.saturating_sub(w), e));
            }
        }
        let iv = |(lo, hi): (i32, i32)| IV::new(Pico8Num(lo), Pico8Num(hi));
        for &a in &ivs {
            for &b in &ivs {
                for op in ['+', '-', '*', '/'] {
                    let results: Vec<Option<i128>> = (a.0..=a.1)
                        .flat_map(|x| (b.0..=b.1).map(move |y| exact(op, x, y)))
                        .collect();
                    let got = match op {
                        '+' => iv(a).checked_add(iv(b)),
                        '-' => iv(a).checked_sub(iv(b)),
                        '*' => iv(a).checked_mul(iv(b)),
                        _ => iv(a).checked_div(iv(b)),
                    };
                    if results.iter().all(|r| r.is_some_and(fits)) {
                        let r: Vec<i128> = results.into_iter().flatten().collect();
                        let hull = (*r.iter().min().unwrap() as i32, *r.iter().max().unwrap() as i32);
                        assert_eq!(got, Some(iv(hull)), "{a:x?} {op} {b:x?}");
                    } else {
                        assert_eq!(got, None, "{a:x?} {op} {b:x?}: some pair overflows");
                    }
                }
            }
            let has_min = a.0 == i32::MIN;
            let negs: Vec<i32> = (a.0..=a.1).map(|x| -(x as i64) as i32).collect();
            let neg = (!has_min).then(|| iv((*negs.iter().min().unwrap(), *negs.iter().max().unwrap())));
            assert_eq!(iv(a).checked_neg(), neg, "-{a:x?}");
            let abss: Vec<i32> = (a.0..=a.1).map(|x| (x as i64).abs() as i32).collect();
            let abs = (!has_min).then(|| iv((*abss.iter().min().unwrap(), *abss.iter().max().unwrap())));
            assert_eq!(iv(a).checked_abs(), abs, "abs({a:x?})");
        }
        // A straddling `abs` and a product across zero, by hand.
        assert_eq!(iv((-3, 2)).checked_abs(), Some(iv((0, 3))));
        assert_eq!(iv((-0x2_0000, 0x1_0000)).checked_mul(iv((-0x3_0000, 0x1_0000))), Some(iv((-0x3_0000, 0x6_0000))));
    }

    /// `%` never panics at the range's edges, and stays in `[0, |b|)`
    /// (`MIN % -0x0.0001`: the exact 0, where `rem_euclid` panics).
    #[test]
    fn modulo_is_total_at_the_range_edges() {
        for &a in &EDGES {
            for &b in &EDGES {
                let r = (Pico8Num(a) % Pico8Num(b)).0 as i64;
                if b == 0 {
                    assert_eq!(r, 0, "{a:#x} % 0");
                } else {
                    assert!((0..(b as i64).abs()).contains(&r), "{a:#x} % {b:#x} gave {r:#x}");
                    assert_eq!(r, (a as i64).rem_euclid(b as i64), "{a:#x} % {b:#x}");
                }
            }
        }
    }
}
