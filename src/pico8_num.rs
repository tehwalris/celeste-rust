// WARNING All the functions in this module are not really tested against pico8.

use std::ops::{Add, Div, Mul, Sub};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pico8Num(i32);

impl Pico8Num {
    pub fn from_i16(v: i16) -> Self {
        Pico8Num((v as i32) << 16)
    }

    pub fn as_i16(&self) -> Option<i16> {
        if self.0 & 0xffff == 0 {
            Some((self.0 >> 16) as i16)
        } else {
            None
        }
    }

    pub fn from_raw_u32(v: u32) -> Self {
        Pico8Num(v as i32)
    }

    pub fn as_raw_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl Add for Pico8Num {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Pico8Num(self.0.wrapping_add(rhs.0))
    }
}

impl Sub for Pico8Num {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Pico8Num(self.0.wrapping_sub(rhs.0))
    }
}

impl Mul for Pico8Num {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let high = (self.0 as i64).wrapping_mul(rhs.0 as i64);
        let low = high >> 16;
        Pico8Num(low as i32)
    }
}

impl Div for Pico8Num {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let self_high = (self.0 as i64) << 16;
        Pico8Num((self_high.wrapping_div(rhs.0 as i64)) as i32)
    }
}

#[cfg(test)]
mod test {
    use crate::pico8_num::Pico8Num;

    #[test]
    fn test_from_i16() {
        let cases: Vec<(i16, u32)> = vec![(-1, 0xffff_0000), (0, 0x0000_0000), (1, 0x0001_0000)];
        for (i16, raw_u32) in cases {
            assert_eq!(Pico8Num::from_i16(i16).as_raw_u32(), raw_u32);
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
}
