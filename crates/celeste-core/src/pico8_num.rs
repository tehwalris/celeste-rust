// PICO-8's 16.16 fixed-point numbers. Every operation here was verified
// against a real PICO-8 0.2.7a6 (`pico8_diff/`, the tests below): literal
// parsing, `+ - * / %`, the range edges (negation wraps, `abs` and `/`
// saturate) and `sin` (a table dumped from the console).

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    str::FromStr,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Pico8Num(i32);

/// The console's `sin`, tabulated: little-endian `i32` in 16.16, one entry
/// per distinguishable input of the first half-turn (`pico8_sin`;
/// `pico8_diff/gen/README.md` for the dump).
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
        // `ok_or_else`: the error message is built lazily.
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

    /// Parse a decimal literal the way the console does: multiply by 65536,
    /// TRUNCATE toward zero, and let an out-of-range integer part WRAP
    /// (`65535` is -1). In `i128` on the digits, not floating point (f32
    /// has too few bits, and Rust's float-to-int casts saturate), so it is
    /// exact for any input length.
    fn parse_decimal(s: &str) -> Option<Self> {
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
        let raw = (int_part << 16).wrapping_add(frac_raw) as i32;
        Some(Self(if negative { raw.wrapping_neg() } else { raw }))
    }

    pub const fn const_mul(&self, rhs: &Self) -> Self {
        let high = (self.0 as i64).wrapping_mul(rhs.0 as i64);
        let low = high >> 16;
        Self(low as i32)
    }

    /// Negation WRAPS: the console gives `-(-32768) == -32768`.
    pub const fn const_neg(self) -> Self {
        Self(self.0.wrapping_neg())
    }

    /// `abs` SATURATES, unlike negation: the console gives
    /// `abs(-32768) == 0x7fff.ffff` while `-(-32768) == -32768`.
    pub const fn abs(self) -> Self {
        Self(self.0.saturating_abs())
    }

    pub const fn flr(self) -> Self {
        Self::from_i16((self.0 >> 16) as i16)
    }

    pub const fn next_smallest(self) -> Self {
        Self(self.0 - 1)
    }

    /// PICO-8 `sin`: the argument is in TURNS and the result is INVERTED
    /// (sin(0.25) == -1). A table dumped from a real console, not a formula
    /// (`-sinf(2πx)` is off by up to 13 ulps).
    ///
    /// 16384 entries cover all 65536 inputs because, measured over every
    /// input, the console's `sin` is constant over pairs of adjacent inputs
    /// and `sin(x + 0.5) == -sin(x)` exactly (but `sin(0.5 - x) == sin(x)`
    /// does NOT hold). Periodicity mod one turn is exact by construction,
    /// which `widen::widen_fruit` depends on.
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

impl Add for Pico8Num {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.wrapping_add(rhs.0))
    }
}

impl Sub for Pico8Num {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0.wrapping_sub(rhs.0))
    }
}

impl Mul for Pico8Num {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self.const_mul(&rhs)
    }
}

impl Div for Pico8Num {
    type Output = Self;

    /// Truncate toward zero, and SATURATE to +/-0x7fff.ffff if the exact
    /// quotient does not fit - including division by zero (`1/0 =
    /// 0x7fff.ffff`, `-1/0 = 0x8000.0001`, `0/0 = 0x7fff.ffff`). The clamp is
    /// for overflow only: `min/1 = 0x8000.0000` fits and is returned as is.
    /// A panic here would be unsound (the search would lose states the
    /// console reaches).
    fn div(self, rhs: Self) -> Self::Output {
        const LIMIT: i64 = 0x7fff_ffff;
        if rhs.0 == 0 {
            // 0/0 saturates POSITIVE on the console, so `>= 0` not `> 0`.
            return Self(if self.0 >= 0 { LIMIT as i32 } else { -(LIMIT as i32) });
        }
        let quotient = ((self.0 as i64) << 16) / (rhs.0 as i64);
        Self(match i32::try_from(quotient) {
            Ok(exact) => exact,
            Err(_) => {
                if quotient > 0 {
                    LIMIT as i32
                } else {
                    -(LIMIT as i32)
                }
            }
        })
    }
}


impl Rem for Pico8Num {
    type Output = Self;

    /// PICO-8's `%` is exactly `i32::rem_euclid` on the raw bits, with
    /// `a % 0 == 0`: total, and never negative. NOT Lua's rule (Lua's
    /// result takes the divisor's sign: `7 % -3` is -2 in Lua, 1 here).
    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.0 == 0 {
            return Self(0);
        }
        Self(self.0.rem_euclid(rhs.0))
    }
}

impl Neg for Pico8Num {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.const_neg()
    }
}

impl FromStr for Pico8Num {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_decimal(s.trim())
            .ok_or_else(|| anyhow::anyhow!("{:?} is not a PICO-8 number literal", s))
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
    /// Endpoint arithmetic is only sound when no value in the interval
    /// wraps: if one endpoint wraps, the true result is two disjoint
    /// segments. The i64 extremes bound every value, so checking them is a
    /// complete wrap detector. Panics on a wrap: it is a modeling surprise
    /// to investigate, not a case to paper over.
    fn from_i64_endpoints(low: i64, high: i64) -> Self {
        Self::try_from_i64_endpoints(low, high).unwrap_or_else(|| {
            panic!(
                "interval arithmetic wrapped: endpoints [{}, {}] leave the 16.16 range",
                low, high
            )
        })
    }

    /// The same, `None` on a wrap, for callers with a sound answer for it
    /// (`transpile::graph`'s evaluator: inputs at top, result `full()`).
    fn try_from_i64_endpoints(low: i64, high: i64) -> Option<Self> {
        if low >= i32::MIN as i64 && high <= i32::MAX as i64 {
            Some(Self::new(Pico8Num(low as i32), Pico8Num(high as i32)))
        } else {
            None
        }
    }

    /// `+` and `-` that report a wrap instead of panicking on it.
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
}

impl Add for Pico8NumInterval {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from_i64_endpoints(
            self.low.0 as i64 + rhs.low.0 as i64,
            self.high.0 as i64 + rhs.high.0 as i64,
        )
    }
}

impl Sub for Pico8NumInterval {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from_i64_endpoints(
            self.low.0 as i64 - rhs.high.0 as i64,
            self.high.0 as i64 - rhs.low.0 as i64,
        )
    }
}

impl Pico8NumInterval {
    /// Scale by a POSITIVE scalar (monotone, so endpoint images bound the
    /// set), with the same loud wrap detection as `Add`/`Sub`.
    pub fn scale_positive(&self, rhs: Pico8Num) -> Self {
        self.checked_scale_positive(rhs)
            .unwrap_or_else(|| panic!("interval scale wrapped by {:?}", rhs))
    }

    /// `scale_positive` that reports a wrap instead of panicking on it.
    pub fn checked_scale_positive(&self, rhs: Pico8Num) -> Option<Self> {
        assert!(rhs.0 > 0, "scale_positive needs a positive scalar");
        let mul = |a: i32| -> i64 { ((a as i64) * (rhs.0 as i64)) >> 16 };
        Self::try_from_i64_endpoints(mul(self.low.0), mul(self.high.0))
    }

    /// Divide by a POSITIVE scalar (monotone), loud on wrap. Mirrors the
    /// concrete `Div` (i64 shifted dividend, truncating division).
    pub fn div_positive(&self, rhs: Pico8Num) -> Self {
        self.checked_div_positive(rhs)
            .unwrap_or_else(|| panic!("interval divide wrapped by {:?}", rhs))
    }

    /// `div_positive` that reports a wrap instead of panicking on it.
    pub fn checked_div_positive(&self, rhs: Pico8Num) -> Option<Self> {
        assert!(rhs.0 > 0, "div_positive needs a positive scalar");
        let div = |a: i32| -> i64 { ((a as i64) << 16).wrapping_div(rhs.0 as i64) };
        Self::try_from_i64_endpoints(div(self.low.0), div(self.high.0))
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
        let neg_half = half.const_neg(); // -2.5
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

    #[test]
    fn test_mul() {
        assert_eq!(
            (Pico8Num::from_i16(25) * Pico8Num::from_i16(4)).as_i16(),
            Some(100)
        );
        assert_eq!(
            (Pico8Num::from_i16(-25) * Pico8Num::from_i16(4)).as_i16(),
            Some(-100)
        );
    }

    #[test]
    fn test_div() {
        assert_eq!(
            (Pico8Num::from_i16(100) / Pico8Num::from_i16(4)).as_i16(),
            Some(25)
        );
        assert_eq!(
            (Pico8Num::from_i16(-100) / Pico8Num::from_i16(4)).as_i16(),
            Some(-25)
        );
        assert_eq!(
            (Pico8Num::from_i16(-100) / Pico8Num::from_i16(7)).as_i16(),
            None
        );
    }

    #[test]
    fn test_flr() {
        assert_eq!((int(4) + P0_15).flr(), int(4));
        assert_eq!(int(4).flr(), int(4));
        assert_eq!((int(-2) - P0_15).flr(), int(-3));
        assert_eq!(int(-2).flr(), int(-2));
    }

    #[test]
    fn test_abs() {
        assert_eq!(int(4).abs(), int(4));
        assert_eq!(int(-4).abs(), int(4));
        assert_eq!(
            (int(4) + P0_15).abs(),
            int(4) + P0_15
        );
        assert_eq!(
            (int(-4) - P0_15).abs(),
            int(4) + P0_15
        );
    }

    /// Pins the invariance `widen::widen_fruit` relies on: for every
    /// nonnegative integer `off`, `sin(off/40) == sin((off mod 40)/40)`
    /// bit-exactly, checked exhaustively rather than argued.
    #[test]
    fn test_pico8_sin_period_40_bit_exact() {
        let forty = Pico8Num::from_i16(40);
        for off in 0..=i16::MAX {
            let full = Pico8Num::from_i16(off) / forty;
            let reduced = Pico8Num::from_i16(off % 40) / forty;
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
    /// half an ulp) and `0.0000153` show truncation, not rounding; `65535`
    /// and `32768` show the integer part wraps, not saturates.
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
            ("32768", 0x8000_0000),
            ("65535", 0xffff_0000),
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
        assert!("".parse::<Pico8Num>().is_err());
        assert!("1.2.3".parse::<Pico8Num>().is_err());
        assert!("nope".parse::<Pico8Num>().is_err());
    }

    /// Division and the +/-32768 edges, against a real PICO-8 0.2.7a6.
    /// `min/1` vs `min/-1` rules out a blanket clamp; `-min` vs `abs(min)`
    /// shows negation wraps while `abs` saturates.
    #[test]
    fn division_and_the_range_edges_match_the_console() {
        let raw = |bits: u32| Pico8Num(bits as i32);
        let min = int(-32768);
        for (label, got, expected) in [
            ("1/0", int(1) / int(0), 0x7fff_ffff_u32),
            ("-1/0", int(-1) / int(0), 0x8000_0001),
            ("0/0", int(0) / int(0), 0x7fff_ffff),
            ("32767/0.5", int(32767) / raw(0x0000_8000), 0x7fff_ffff),
            ("-32767/0.5", int(-32767) / raw(0x0000_8000), 0x8000_0001),
            ("min/1", min / int(1), 0x8000_0000),
            ("min/-1", min / int(-1), 0x7fff_ffff),
            ("min*-1", min * int(-1), 0x8000_0000),
            ("-min", -min, 0x8000_0000),
            ("abs(min)", min.abs(), 0x7fff_ffff),
            ("-7/2", int(-7) / int(2), 0xfffc_8000),
            ("7/-2", int(7) / int(-2), 0xfffc_8000),
            ("1/3", int(1) / int(3), 0x0000_5555),
            ("-1/3", int(-1) / int(3), 0xffff_aaab),
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
            let turns = int(i) / int(40);
            assert_eq!(
                turns.pico8_sin().as_raw_u32() as i32,
                *expected,
                "sin({}/40) disagrees with the console",
                i
            );
        }
    }
}
