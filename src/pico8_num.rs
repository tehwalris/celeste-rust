// WARNING All the functions in this module are not really tested against pico8.

use anyhow::Result;
use std::{
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pico8Num(i32);

impl Pico8Num {
    pub const fn from_i16(v: i16) -> Self {
        Self((v as i32) << 16)
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
            .ok_or(anyhow!("got {:?}, expected integer", self))
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

    pub fn from_f32(n: f32) -> Self {
        // TODO this is probably not accurate
        if n < 0. {
            Self::from_f32(-n).const_neg()
        } else {
            Self::from_parts(n as i16, ((n - n.floor()) * 65536.) as u16)
        }
    }

    pub fn from_str(s: &str) -> Result<Self> {
        Ok(Self::from_f32(s.parse()?))
    }

    pub const fn const_mul(&self, rhs: &Self) -> Self {
        let high = (self.0 as i64).wrapping_mul(rhs.0 as i64);
        let low = high >> 16;
        Self(low as i32)
    }

    pub const fn const_neg(self) -> Self {
        Self(-self.0)
    }

    pub const fn abs(self) -> Self {
        Self(self.0.abs())
    }

    pub const fn flr(self) -> Self {
        Self::from_i16((self.0 >> 16) as i16)
    }

    pub const fn next_smallest(self) -> Self {
        Self(self.0 - 1)
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

    fn div(self, rhs: Self) -> Self::Output {
        let self_high = (self.0 as i64) << 16;
        Self((self_high.wrapping_div(rhs.0 as i64)) as i32)
    }
}

impl Rem for Pico8Num {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        let lhs = self.as_i16().unwrap();
        let rhs = rhs.as_i16().unwrap();
        if lhs < 0 || rhs <= 0 {
            panic!("Pico8Num::Rem not implemented for negative/non-positive numbers");
        }
        Self::from_i16(lhs % rhs)
    }
}

impl Neg for Pico8Num {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.const_neg()
    }
}

#[cfg(test)]
mod tests {
    use crate::pico8_num::{constants, int, Pico8Num};

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
}
