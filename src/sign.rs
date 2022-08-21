use crate::pico8_num::{int, Pico8Num, Pico8Vec2};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Sign {
    Negative,
    Zero,
    Positive,
}

impl Sign {
    pub fn from_pico8(v: Pico8Num) -> Sign {
        if v > int(0) {
            Sign::Positive
        } else if v < int(0) {
            Sign::Negative
        } else {
            Sign::Zero
        }
    }

    pub fn smaller_equal_zero(self) -> bool {
        self != Self::Positive
    }

    pub fn greater_equal_zero(self) -> bool {
        self != Self::Negative
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SignVec2 {
    pub x: Sign,
    pub y: Sign,
}

impl SignVec2 {
    pub fn from_pico8(v: &Pico8Vec2) -> Self {
        Self {
            x: Sign::from_pico8(v.x),
            y: Sign::from_pico8(v.y),
        }
    }
}

pub struct SignVec2Map<T>(Vec<T>);

impl<T> SignVec2Map<T> {
    const INDEX_COUNT: usize = 9;

    pub fn new<F>(make_default: F) -> Self
    where
        F: Fn(SignVec2) -> T,
    {
        Self(
            (0..Self::INDEX_COUNT)
                .map(|i| make_default(Self::key_from_index(i)))
                .collect(),
        )
    }

    fn key_from_index(i: usize) -> SignVec2 {
        assert!(i < Self::INDEX_COUNT);
        let sign_from_index = |j| match j {
            0 => Sign::Negative,
            1 => Sign::Zero,
            2 => Sign::Positive,
            _ => panic!("index out of range {}", j),
        };
        SignVec2 {
            x: sign_from_index(i % 3),
            y: sign_from_index(i / 3),
        }
    }

    fn index_from_key(key: SignVec2) -> usize {
        let index_from_sign = |s| match s {
            Sign::Negative => 0,
            Sign::Zero => 1,
            Sign::Positive => 2,
        };
        let i = index_from_sign(key.x) + 3 * index_from_sign(key.y);
        assert!(i >= 0 && i < Self::INDEX_COUNT);
        assert!(Self::key_from_index(i) == key);
        i
    }

    pub fn get(&self, key: SignVec2) -> &T {
        &self.0[Self::index_from_key(key)]
    }
}
