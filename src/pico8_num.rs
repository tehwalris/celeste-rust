// Verified against a REAL PICO-8 0.2.7a6 (see `pico8_diff/`): `sin` is now a
// dumped table rather than a model, literal parsing follows the console's
// truncate-and-wrap rule, and `+ - * /` and `%` were measured to agree.
//
// STILL NOT VERIFIED, and each is a place a proof could quietly be wrong:
// `abs(-32768)` (the console saturates, we wrap negative), division by zero
// and division overflow (the console saturates, we panic or wrap), and every
// builtin this interpreter does not implement yet. See
// `pico8_diff/known_fail.txt`.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    str::FromStr,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Pico8Num(i32);

/// The console's `sin`, tabulated. Little-endian `i32` in 16.16, one entry
/// per distinguishable input of the first half-turn; see `pico8_sin` and
/// `pico8_diff/gen/README.md` for how it was dumped and why 16384 entries
/// cover all 65536 inputs exactly.
static SIN_TABLE: [i32; 16384] = {
    let bytes = *include_bytes!("../cart/pico8_sin_table.bin");
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

    /// The raw fixed-point bits. The representation is a plain `i32` with no
    /// padding and no two encodings of one value, so comparing bits is
    /// exactly comparing numbers - which is what lets dedup pack rows of
    /// mixed value types into one word array and compare them wholesale.
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
        // Use ok_or_else for lazy error message construction
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
    /// TRUNCATE toward zero, and let an out-of-range integer part WRAP.
    ///
    /// Measured on a real PICO-8 rather than assumed - `0.0000076` (just
    /// under half an ulp) is 0, `0.0000153` is 1, so it truncates and does
    /// not round; `65535` is `0xffff.0000`, i.e. -1, so the integer part
    /// wraps rather than saturating.
    ///
    /// This replaces a route through `f32`, which was wrong twice over: f32
    /// has 24 mantissa bits for a format that needs 31, and `n as i16`
    /// SATURATES in Rust, so the literal `32768` came out as 32767 and
    /// `-32768` as -32767. Neither bug was reachable from this cart - all 55
    /// of its distinct literals parse identically either way, which is why
    /// this change moves nothing - but "not currently reachable" is a poor
    /// thing to rest a proof on.
    ///
    /// Done in `i128` on the digits rather than in floating point so that
    /// the arithmetic is exact by construction for any input length.
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
    ///
    /// `-self.0` panics on `i32::MIN` in debug builds and wraps in release,
    /// so this was already the release behaviour - but by accident, and with
    /// a debug-only panic waiting in it.
    pub const fn const_neg(self) -> Self {
        Self(self.0.wrapping_neg())
    }

    /// `abs` SATURATES, unlike negation: the console gives
    /// `abs(-32768) == 0x7fff.ffff` while `-(-32768) == -32768`.
    ///
    /// The asymmetry is real and measured, not a guess. `self.0.abs()` gave
    /// a NEGATIVE magnitude here (wrapping back to -32768 in release,
    /// panicking in debug), which is the kind of thing that turns into a
    /// wrong answer rather than a crash.
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
    /// (sin(0.25) == -1). A TABLE DUMPED FROM A REAL CONSOLE, not a formula.
    ///
    /// This used to be `-sinf(2πx)` in f32, and that model was WRONG - by up
    /// to 13 ulps, on 41 of the 52 arguments the fruit's bob actually
    /// evaluates. The old doc comment asked for exactly this dump before
    /// trusting any proof that depends on fruit collection timing; here it
    /// is. There is no approximation left to be wrong: `sin` reduces mod one
    /// turn, so its domain is finite and it is simply tabulated.
    ///
    /// The table is 16384 entries rather than 65536 because two properties
    /// were measured to hold over every input (see
    /// `pico8_diff/gen/README.md`): the console's `sin` is constant over
    /// blocks of two adjacent inputs, and `sin(x + 0.5) == -sin(x)` exactly.
    /// `sin(0.5 - x) == sin(x)` does NOT hold even though real sine says it
    /// must, which is the clearest possible sign that tabulating beats
    /// modelling here.
    ///
    /// Periodicity mod one turn is exact by construction, which the
    /// fruit-off boundary widening in
    /// `abstraction::apply_conservative_widenings` depends on.
    pub fn pico8_sin(self) -> Self {
        // Two's-complement masking IS floored mod 1.0: -0.25 -> 0.75.
        let frac = (self.0 & 0xffff) as u32;
        let entry = SIN_TABLE[((frac & 0x7fff) >> 1) as usize];
        Self(if frac < 0x8000 { entry } else { -entry })
    }
}

pub const fn int(v: i16) -> Pico8Num {
    Pico8Num::from_i16(v)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pico8Vec2 {
    pub x: Pico8Num,
    pub y: Pico8Num,
}

impl Pico8Vec2 {
    pub const fn from_i16s(x: i16, y: i16) -> Self {
        Self {
            x: int(x),
            y: int(y),
        }
    }

    pub fn as_i16s_or_err(&self) -> Result<(i16, i16)> {
        Ok((self.x.as_i16_or_err()?, self.y.as_i16_or_err()?))
    }

    pub const fn zero() -> Pico8Vec2 {
        Self::from_i16s(0, 0)
    }
}

pub mod constants {
    use super::Pico8Num;

    pub const PICO8_NUM_0_6: Pico8Num = Pico8Num(0x0000_9999);
    pub const PICO8_NUM_0_15: Pico8Num = Pico8Num(0x0000_2666);
    pub const PICO8_NUM_0_4: Pico8Num = Pico8Num(0x0000_6666);
    pub const PICO8_NUM_0_21: Pico8Num = Pico8Num(0x0000_35C2);
    pub const PICO8_NUM_0_5: Pico8Num = Pico8Num(0x0000_8000);
    pub const PICO8_NUM_0_70710678118: Pico8Num = Pico8Num(0x0000_B504);
    pub const PICO8_NUM_1_5: Pico8Num = Pico8Num(0x0001_8000);
    pub const PICO8_NUM_0_75: Pico8Num = Pico8Num(0x0000_C000);
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
    /// quotient does not fit - including division by zero.
    ///
    /// Measured on a real console. Three facts pin the rule down and the
    /// obvious implementations get at least one wrong:
    ///
    /// * `1/0 = 0x7fff.ffff`, `-1/0 = 0x8000.0001`, `0/0 = 0x7fff.ffff`.
    ///   Division by zero is a saturating infinity with a sign, not a trap.
    ///   We used to `wrapping_div` by zero, which PANICS - and a panic where
    ///   the console keeps running is a soundness hole, not a safe failure:
    ///   the search would lose states PICO-8 reaches.
    /// * The negative bound is `0x8000.0001`, one ulp above the most
    ///   negative value, so the clamp is SYMMETRIC at +/-0x7fff.ffff.
    /// * But `min/1 = 0x8000.0000` - a quotient that FITS is returned
    ///   unchanged even though it lies outside that symmetric range. So the
    ///   clamp applies to overflow only, and a blanket clamp would be wrong.
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

impl Pico8Num {
    /// `%`, or `None` where this implementation does not model PICO-8's
    /// semantics: negative or fractional operands, and a non-positive divisor.
    ///
    /// Callers interpreting a program must use this rather than `%`. Whether
    /// the operands are in range depends on the values flowing through the
    /// program, and `if_convert` deliberately runs arithmetic on lanes that
    /// would not have reached it, so this is a case the interpreter has to
    /// report rather than abort on.
    ///
    /// PICO-8's `%` is exactly `i32::rem_euclid` on the raw fixed-point
    /// bits, with `a % 0 == 0`. The result is NEVER negative.
    ///
    /// This doc used to say the result "takes the divisor's sign", which is
    /// what Lua does and what PICO-8 does NOT: the console gives
    /// `7 % -3 == 1`, where Lua gives -2. Measured across all four sign
    /// combinations, fractional divisors and zero - 20 of 20 data points
    /// agree with `rem_euclid`. The old `rhs <= 0` and non-integer guards
    /// were therefore refusing cases the same line already computed
    /// correctly.
    ///
    /// Still an `Option` because callers interpreting a program must be able
    /// to handle a case this does not model rather than abort - `if_convert`
    /// deliberately runs arithmetic on lanes that would not have reached it.
    /// Nothing currently returns `None`.
    pub fn checked_rem(self, rhs: Self) -> Option<Self> {
        if rhs.0 == 0 {
            return Some(Self(0));
        }
        Some(Self(self.0.rem_euclid(rhs.0)))
    }
}

impl Rem for Pico8Num {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        self.checked_rem(rhs)
            .expect("Pico8Num::Rem not implemented for negative/non-positive numbers")
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

/// Represents an interval [low, high] of Pico8Num values for abstract interpretation.
/// This is used to track uncertainty in values (e.g., player's sub-pixel position).
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

    pub fn contains_number(&self, n: Pico8Num) -> bool {
        n >= self.low && n <= self.high
    }

    pub fn contains_interval(&self, other: &Self) -> bool {
        other.low >= self.low && other.high <= self.high
    }

    pub fn union(&self, other: &Self) -> Self {
        Self {
            low: std::cmp::min(self.low, other.low),
            high: std::cmp::max(self.high, other.high),
        }
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {
        if self.low <= other.high && other.low <= self.high {
            Some(Self {
                low: std::cmp::max(self.low, other.low),
                high: std::cmp::min(self.high, other.high),
            })
        } else {
            None
        }
    }
}

impl Pico8NumInterval {
    /// Endpoint arithmetic is only sound when no value in the interval
    /// wraps: if exactly one endpoint wraps, the true result set is two
    /// disjoint segments and a single [low, high] pair cannot represent it
    /// (a struct-literal construction would silently produce low > high).
    /// The extremes computed in i64 bound every intermediate sum, so
    /// checking them is a complete wrap detector. Loud on wrap: no current
    /// widening produces values anywhere near the numeric range, so a trip
    /// here is a modeling surprise to investigate, not a case to paper
    /// over. (If a full-range interval ever becomes legitimate, its closure
    /// under wrapping arithmetic is the full interval - add that as an
    /// explicit, deliberate case then.)
    fn from_i64_endpoints(low: i64, high: i64) -> Self {
        assert!(
            low >= i32::MIN as i64 && high <= i32::MAX as i64,
            "interval arithmetic wrapped: endpoints [{}, {}] leave the 16.16 range",
            low,
            high
        );
        Self::new(Pico8Num(low as i32), Pico8Num(high as i32))
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
        assert!(rhs.0 > 0, "scale_positive needs a positive scalar");
        let mul = |a: i32| -> i64 { ((a as i64) * (rhs.0 as i64)) >> 16 };
        Self::from_i64_endpoints(mul(self.low.0), mul(self.high.0))
    }

    /// Divide by a POSITIVE scalar (monotone), loud on wrap. Mirrors the
    /// concrete `Div` (i64 shifted dividend, truncating division).
    pub fn div_positive(&self, rhs: Pico8Num) -> Self {
        assert!(rhs.0 > 0, "div_positive needs a positive scalar");
        let div = |a: i32| -> i64 { ((a as i64) << 16).wrapping_div(rhs.0 as i64) };
        Self::from_i64_endpoints(div(self.low.0), div(self.high.0))
    }
}

impl fmt::Debug for Pico8NumInterval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Interval[{:?}, {:?}]", self.low, self.high)
    }
}

#[cfg(test)]
mod tests {
    use crate::pico8_num::{constants, int, Pico8Num};

    /// PICO-8's `%` is floored: the result takes the divisor's sign, and the
    /// fixed-point fraction participates.
    #[test]
    fn test_checked_rem_is_floored_modulo() {
        let n = Pico8Num::from_i16;
        let eight = n(8);
        assert_eq!(n(10).checked_rem(eight), Some(n(2)));
        assert_eq!(n(-2).checked_rem(eight), Some(n(6)));
        assert_eq!(n(-16).checked_rem(eight), Some(n(0)));
        let half = Pico8Num::from_parts(2, 0x8000); // 2.5
        assert_eq!(half.checked_rem(eight), Some(half));
        let neg_half = half.const_neg(); // -2.5
        assert_eq!(
            neg_half.checked_rem(eight),
            Some(Pico8Num::from_parts(5, 0x8000)) // 5.5
        );
        // These three used to assert `None` - "unmodelled". They are not
        // unmodelled; the console answers them, and the single `rem_euclid`
        // line already computed those answers correctly while the guards
        // above it threw them away. Measured on a real PICO-8 0.2.7a6:
        assert_eq!(n(1).checked_rem(n(0)), Some(n(0)), "1 % 0");
        assert_eq!(n(0).checked_rem(n(0)), Some(n(0)), "0 % 0");
        assert_eq!(n(-1).checked_rem(n(0)), Some(n(0)), "-1 % 0");
        assert_eq!(n(1).checked_rem(n(-8)), Some(n(1)), "1 % -8");
        assert_eq!(n(1).checked_rem(half), Some(n(1)), "1 % 2.5");
        assert_eq!(n(7).checked_rem(half), Some(n(2)), "7 % 2.5");
        assert_eq!(
            n(-7).checked_rem(half),
            Some(Pico8Num::from_parts(0, 0x8000)),
            "-7 % 2.5"
        );
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
        assert_eq!((int(4) + constants::PICO8_NUM_0_15).flr(), int(4));
        assert_eq!(int(4).flr(), int(4));
        assert_eq!((int(-2) - constants::PICO8_NUM_0_15).flr(), int(-3));
        assert_eq!(int(-2).flr(), int(-2));
    }

    #[test]
    fn test_abs() {
        assert_eq!(int(4).abs(), int(4));
        assert_eq!(int(-4).abs(), int(4));
        assert_eq!(
            (int(4) + constants::PICO8_NUM_0_15).abs(),
            int(4) + constants::PICO8_NUM_0_15
        );
        assert_eq!(
            (int(-4) - constants::PICO8_NUM_0_15).abs(),
            int(4) + constants::PICO8_NUM_0_15
        );
    }

    /// Pins the invariance the fruit-off boundary widening relies on
    /// (`abstraction::apply_conservative_widenings`): for every nonnegative
    /// integer `off`, `sin(off/40) == sin((off mod 40)/40)` BIT-EXACTLY in
    /// this implementation. The fixed-point division makes the arguments
    /// differ by exactly 1.0 per period ((off+40)/40 == off/40 + 1 for
    /// nonnegative integer division), but sin runs through an f32 chain
    /// where +1.0 turn is NOT trivially bit-invariant - so the equality is
    /// checked exhaustively over the whole integer range instead of argued.
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

    /// Literal parsing, against a REAL PICO-8 0.2.7a6.
    ///
    /// The two rounding probes are the point: `0.0000076` is just UNDER half
    /// an ulp and `0.0000153` just over one, so together they show the
    /// console truncates rather than rounds. `65535` and `32768` show the
    /// integer part wraps rather than saturating - which is what the old
    /// `n as i16` got wrong, since Rust float-to-int casts saturate.
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

    /// Division and the +/-32768 edges, against a REAL PICO-8 0.2.7a6.
    ///
    /// `min/1` next to `min/-1` is the case that rules out a blanket clamp:
    /// the first FITS and is returned unchanged at `0x8000.0000`, outside
    /// the symmetric range the second saturates into. And `-min` next to
    /// `abs(min)` is the case that rules out treating the two as the same
    /// operation: negation wraps, `abs` saturates.
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

    /// `%` against a REAL PICO-8 0.2.7a6, all four sign combinations.
    ///
    /// `7 % -3 == 1` is the one that matters: Lua gives -2, so anything
    /// that reuses host `%` semantics is wrong here.
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

    /// Values read off a REAL PICO-8 0.2.7a6 with `tostr(sin(x), true)`.
    ///
    /// The point of these is that the old `-sinf(2πx)` model passes the
    /// quarter-turn test above and still gets every one of these wrong -
    /// `sin(1/40)` by 13 ulps. Quarter turns are the values a model is most
    /// likely to get right by construction, so on their own they certify
    /// almost nothing; these are arbitrary interior points, which is exactly
    /// what makes them worth pinning.
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
