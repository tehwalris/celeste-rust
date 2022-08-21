use bv::{BitVec, Bits, BitsExt};

use crate::pico8_num::{constants, int, Pico8Vec2};

use super::pico8_num::Pico8Num;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayerFlags {
    // Common to all objects
    pub flip_x: bool,

    // Specific to player
    pub p_jump: bool,
    pub p_dash: bool,
    pub grace: Pico8Num,
    pub jbuffer: Pico8Num, // TODO maybe it's possible to remove this
    pub djump: Pico8Num,
    pub dash_time: Pico8Num,
    pub dash_target: Pico8Vec2,
    pub dash_accel: Pico8Vec2,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompressedPlayerFlags(usize);

#[derive(PartialEq, Eq)]
struct DashCombo {
    target: Pico8Vec2,
    accel: Pico8Vec2,
}

const VALID_DASH_COMBOS: [DashCombo; 8] = [
    // input == -1 && v_input == -1
    // spd_x < 0 && spd_y < 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(-2),
            y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
    // input == -1 && v_input == 0
    // spd_x < 0 && spd_y == 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(-2),
            y: Pico8Num::from_i16(0),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5,
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
    // input == -1 && v_input == 1
    // spd_x < 0, spd_y > 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(-2),
            y: Pico8Num::from_i16(2),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
    // input == 0 && v_input == -1
    // spd_x == 0 && spd_y < 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(0),
            y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5,
        },
    },
    // input == 0 && v_input == 0 && this.flip.x
    // spd_x < 0 && spd_y == 0
    // same as (input == -1 && v_input == 0) except for speed
    // input == 0 && v_input == 0 && !this.flip.x
    // spd_x > 0 && spd_y == 0
    // same as (input == 1 && v_input == 0) except for speed
    // input == 0 && v_input == 1
    // spd_x == 0 && spd_y > 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(0),
            y: Pico8Num::from_i16(2),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5,
        },
    },
    // input == 1 && v_input == -1
    // spd_x > 0 && spd_y < 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(2),
            y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
    // input == 1 && v_input == 0
    // spd_x > 0 && spd_y == 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(2),
            y: Pico8Num::from_i16(0),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5,
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
    // input == 1 && v_input == 1
    // spd_x > 0 && spd_y > 0
    DashCombo {
        target: Pico8Vec2 {
            x: Pico8Num::from_i16(2),
            y: Pico8Num::from_i16(2),
        },
        accel: Pico8Vec2 {
            x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
            y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        },
    },
];

const MAX_GRACE: i16 = 6;
const MAX_JBUFFER: i16 = 4;
const MAX_DJUMP: i16 = 1;
const MAX_DASH_TIME: i16 = 4;
const MAX_DASH_I: i16 = ((MAX_DASH_TIME as usize) * VALID_DASH_COMBOS.len()) as i16;

impl PlayerFlags {
    pub fn spawn(max_djump: Pico8Num) -> PlayerFlags {
        PlayerFlags {
            flip_x: false,
            p_jump: false,
            p_dash: false,
            grace: int(0),
            jbuffer: int(0),
            djump: max_djump,
            dash_time: int(0),
            dash_target: Pico8Vec2::zero(),
            dash_accel: Pico8Vec2::zero(),
        }
    }

    pub fn adjust_before_compress(&mut self) {
        if self.dash_time.as_i16() == Some(0) {
            self.dash_target.x = Pico8Num::from_i16(0);
            self.dash_target.y = Pico8Num::from_i16(0);
            self.dash_accel.x = Pico8Num::from_i16(0);
            self.dash_accel.y = Pico8Num::from_i16(0);
        }
    }

    pub fn compress(&self) -> Option<CompressedPlayerFlags> {
        let grace = self.grace.as_i16()?;
        if grace < 0 || grace > MAX_GRACE {
            return None;
        }

        let jbuffer = self.jbuffer.as_i16()?;
        if jbuffer < 0 || jbuffer > MAX_JBUFFER {
            return None;
        }

        let djump = self.djump.as_i16()?;
        if djump < 0 || djump > MAX_DJUMP {
            return None;
        }

        let dash_time = self.dash_time.as_i16()?;
        if dash_time < 0 || dash_time > MAX_DASH_TIME {
            return None;
        }

        let dash_combo_index = if dash_time == 0 {
            None
        } else {
            let dash_combo = DashCombo {
                target: Pico8Vec2 {
                    x: self.dash_target.x,
                    y: self.dash_target.y,
                },
                accel: Pico8Vec2 {
                    x: self.dash_accel.x,
                    y: self.dash_accel.y,
                },
            };
            VALID_DASH_COMBOS.iter().position(|c| c == &dash_combo)
        };
        if dash_time == 0
            && (self.dash_target.x.as_i16() != Some(0)
                || self.dash_target.y.as_i16() != Some(0)
                || self.dash_accel.x.as_i16() != Some(0)
                || self.dash_accel.y.as_i16() != Some(0))
            || dash_time != 0 && dash_combo_index.is_none()
        {
            return None;
        }

        let dash_i = if dash_time == 0 {
            0
        } else {
            i16::try_from(
                1 + (dash_time as usize - 1) * VALID_DASH_COMBOS.len() + dash_combo_index.unwrap(),
            )
            .unwrap()
        };
        assert!(dash_i >= 0 && dash_i <= MAX_DASH_I);

        let mut compressed: usize = 0;
        let parts: [(i16, i16); 7] = [
            (self.flip_x as i16, 1),
            (self.p_jump as i16, 1),
            (self.p_dash as i16, 1),
            (grace, MAX_GRACE),
            (jbuffer, MAX_JBUFFER),
            (djump, MAX_DJUMP),
            (dash_i, MAX_DASH_I),
        ];
        for (part_value, part_max) in parts {
            assert!(part_value >= 0 && part_value <= part_max);
            compressed *= part_max as usize + 1;
            compressed += part_value as usize;
        }

        Some(CompressedPlayerFlags(compressed))
    }
}

impl CompressedPlayerFlags {
    pub const VALID_COUNT: usize = 18480;

    pub fn try_from_raw(compressed: usize) -> Option<CompressedPlayerFlags> {
        if Self::try_decompress(compressed).is_some() {
            Some(CompressedPlayerFlags(compressed))
        } else {
            None
        }
    }

    pub fn decompress(&self) -> PlayerFlags {
        Self::try_decompress(self.0).unwrap()
    }

    fn try_decompress(compressed: usize) -> Option<PlayerFlags> {
        let mut partially_compressed: usize = compressed;
        let mut flip_x = 0;
        let mut p_jump = 0;
        let mut p_dash = 0;
        let mut grace = 0;
        let mut jbuffer = 0;
        let mut djump = 0;
        let mut dash_i = 0;
        let parts: [(&mut i16, i16); 7] = [
            (&mut flip_x, 1),
            (&mut p_jump, 1),
            (&mut p_dash, 1),
            (&mut grace, MAX_GRACE),
            (&mut jbuffer, MAX_JBUFFER),
            (&mut djump, MAX_DJUMP),
            (&mut dash_i, MAX_DASH_I),
        ];
        for (part_value, part_max) in parts.into_iter().rev() {
            let part_count = usize::try_from(part_max).unwrap() + 1;
            *part_value = i16::try_from(partially_compressed % part_count).unwrap();
            partially_compressed /= part_count;
            if *part_value < 0 || *part_value > part_max {
                return None;
            }
        }
        if partially_compressed != 0 {
            return None;
        }

        let (dash_time, dash_combo) = if dash_i == 0 {
            (0, None)
        } else {
            let temp = usize::try_from(dash_i).unwrap() - 1;
            let dash_combo_index = temp % VALID_DASH_COMBOS.len();
            let dash_time = temp / VALID_DASH_COMBOS.len() + 1;
            (dash_time, Some(&VALID_DASH_COMBOS[dash_combo_index]))
        };

        let zero_vec = Pico8Vec2 {
            x: Pico8Num::from_i16(0),
            y: Pico8Num::from_i16(0),
        };
        Some(PlayerFlags {
            flip_x: flip_x != 0,
            p_jump: p_jump != 0,
            p_dash: p_dash != 0,
            grace: Pico8Num::from_i16(grace),
            jbuffer: Pico8Num::from_i16(jbuffer),
            djump: Pico8Num::from_i16(djump),
            dash_time: Pico8Num::from_i16(i16::try_from(dash_time).unwrap()),
            dash_target: dash_combo
                .map(|v| v.target.clone())
                .unwrap_or(zero_vec.clone()),
            dash_accel: dash_combo
                .map(|v| v.accel.clone())
                .unwrap_or(zero_vec.clone()),
        })
    }

    pub fn iter() -> impl Iterator<Item = CompressedPlayerFlags> {
        (0..Self::VALID_COUNT).map(|v| CompressedPlayerFlags(v))
    }
}

#[derive(Clone)]
pub struct PlayerFlagBitVec(BitVec);

impl PlayerFlagBitVec {
    pub fn new() -> Self {
        Self(BitVec::new_fill(
            false,
            CompressedPlayerFlags::VALID_COUNT as u64,
        ))
    }

    pub fn set(&mut self, key: CompressedPlayerFlags, value: bool) {
        self.0.set(key.0 as u64, value);
    }

    pub fn get(&self, key: CompressedPlayerFlags) -> bool {
        self.0.get(key.0 as u64)
    }

    pub fn is_all_false(&self) -> bool {
        for i in 0..self.0.block_len() {
            if self.0.get_block(i) != 0 {
                return false;
            }
        }
        true
    }
}

#[derive(Clone)]
pub struct PlayerFlagBitMatrix {
    dest_vecs_by_src: Vec<PlayerFlagBitVec>,
}

impl PlayerFlagBitMatrix {
    pub fn new() -> Self {
        Self {
            dest_vecs_by_src: vec![PlayerFlagBitVec::new(); CompressedPlayerFlags::VALID_COUNT],
        }
    }

    pub fn set(
        &mut self,
        src_key: CompressedPlayerFlags,
        dst_key: CompressedPlayerFlags,
        value: bool,
    ) {
        self.dest_vecs_by_src[src_key.0].set(dst_key, value);
    }

    pub fn get(&mut self, src_key: CompressedPlayerFlags, dst_key: CompressedPlayerFlags) -> bool {
        self.dest_vecs_by_src[src_key.0].get(dst_key)
    }

    pub fn calculate_reachable(&mut self, src_vec: &PlayerFlagBitVec) -> PlayerFlagBitVec {
        let mut dst_vec = PlayerFlagBitVec::new();
        for i in 0..CompressedPlayerFlags::VALID_COUNT {
            if src_vec.get(CompressedPlayerFlags(i)) {
                dst_vec.0 = self.dest_vecs_by_src[i].0.bit_or(dst_vec.0).to_bit_vec();
            }
        }
        dst_vec
    }
}

#[cfg(test)]
mod tests {
    use super::CompressedPlayerFlags;

    #[test]
    fn test_compress_decompress() {
        for i in 0..CompressedPlayerFlags::VALID_COUNT {
            let compressed = CompressedPlayerFlags::try_from_raw(i).unwrap();
            let decompressed = compressed.decompress();
            let mut adjusted = decompressed.clone();
            adjusted.adjust_before_compress();
            assert_eq!(decompressed, adjusted);
            assert_eq!(decompressed.compress(), Some(compressed));
        }
        for i in CompressedPlayerFlags::VALID_COUNT..2 * CompressedPlayerFlags::VALID_COUNT {
            assert!(CompressedPlayerFlags::try_from_raw(i).is_none());
        }
    }
}
