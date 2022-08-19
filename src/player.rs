use crate::pico8_num::constants;

use super::pico8_num::Pico8Num;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlayerFlags {
    pub p_jump: bool,
    pub p_dash: bool,
    pub grace: Pico8Num,
    pub djump: Pico8Num,
    pub dash_time: Pico8Num,
    pub dash_target_x: Pico8Num,
    pub dash_target_y: Pico8Num,
    pub dash_accel_x: Pico8Num,
    pub dash_accel_y: Pico8Num,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompressedPlayerFlags(usize);

#[derive(PartialEq, Eq)]
struct DashCombo {
    target_x: Pico8Num,
    target_y: Pico8Num,
    accel_x: Pico8Num,
    accel_y: Pico8Num,
}

const VALID_DASH_COMBOS: [DashCombo; 8] = [
    // input == -1 && v_input == -1
    // spd_x < 0 && spd_y < 0
    DashCombo {
        target_x: Pico8Num::from_i16(-2),
        target_y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
    // input == -1 && v_input == 0
    // spd_x < 0 && spd_y == 0
    DashCombo {
        target_x: Pico8Num::from_i16(-2),
        target_y: Pico8Num::from_i16(0),
        accel_x: constants::PICO8_NUM_1_5,
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
    // input == -1 && v_input == 1
    // spd_x < 0, spd_y > 0
    DashCombo {
        target_x: Pico8Num::from_i16(-2),
        target_y: Pico8Num::from_i16(2),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
    // input == 0 && v_input == -1
    // spd_x == 0 && spd_y < 0
    DashCombo {
        target_x: Pico8Num::from_i16(0),
        target_y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5,
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
        target_x: Pico8Num::from_i16(0),
        target_y: Pico8Num::from_i16(2),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5,
    },
    // input == 1 && v_input == -1
    // spd_x > 0 && spd_y < 0
    DashCombo {
        target_x: Pico8Num::from_i16(2),
        target_y: Pico8Num::from_i16(-2).const_mul(&constants::PICO8_NUM_0_75),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
    // input == 1 && v_input == 0
    // spd_x > 0 && spd_y == 0
    DashCombo {
        target_x: Pico8Num::from_i16(2),
        target_y: Pico8Num::from_i16(0),
        accel_x: constants::PICO8_NUM_1_5,
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
    // input == 1 && v_input == 1
    // spd_x > 0 && spd_y > 0
    DashCombo {
        target_x: Pico8Num::from_i16(2),
        target_y: Pico8Num::from_i16(2),
        accel_x: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
        accel_y: constants::PICO8_NUM_1_5.const_mul(&constants::PICO8_NUM_0_70710678118),
    },
];

const MAX_GRACE: i16 = 6;
const MAX_DJUMP: i16 = 1;
const MAX_DASH_TIME: i16 = 4;
const MAX_DASH_I: i16 = ((MAX_DASH_TIME as usize) * VALID_DASH_COMBOS.len()) as i16;

impl PlayerFlags {
    fn adjust_before_compress(&mut self) {
        if self.dash_time.as_i16() == Some(0) {
            self.dash_target_x = Pico8Num::from_i16(0);
            self.dash_target_y = Pico8Num::from_i16(0);
            self.dash_accel_x = Pico8Num::from_i16(0);
            self.dash_accel_y = Pico8Num::from_i16(0);
        }
    }

    fn compress(&self) -> Option<CompressedPlayerFlags> {
        let grace = self.grace.as_i16()?;
        if grace < 0 || grace > MAX_GRACE {
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
                target_x: self.dash_target_x,
                target_y: self.dash_target_y,
                accel_x: self.dash_accel_x,
                accel_y: self.dash_accel_y,
            };
            VALID_DASH_COMBOS.iter().position(|c| c == &dash_combo)
        };
        if dash_time == 0
            && (self.dash_target_x.as_i16() != Some(0)
                || self.dash_target_y.as_i16() != Some(0)
                || self.dash_accel_x.as_i16() != Some(0)
                || self.dash_accel_y.as_i16() != Some(0))
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
        let parts: [(i16, i16); 5] = [
            (self.p_jump as i16, 1),
            (self.p_dash as i16, 1),
            (grace, MAX_GRACE),
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
    pub const VALID_COUNT: usize = 1848;

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
        let mut p_jump = 0;
        let mut p_dash = 0;
        let mut grace = 0;
        let mut djump = 0;
        let mut dash_i = 0;
        let parts: [(&mut i16, i16); 5] = [
            (&mut p_jump, 1),
            (&mut p_dash, 1),
            (&mut grace, MAX_GRACE),
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

        Some(PlayerFlags {
            p_jump: p_jump != 0,
            p_dash: p_dash != 0,
            grace: Pico8Num::from_i16(grace),
            djump: Pico8Num::from_i16(djump),
            dash_time: Pico8Num::from_i16(i16::try_from(dash_time).unwrap()),
            dash_target_x: dash_combo
                .map(|v| v.target_x)
                .unwrap_or(Pico8Num::from_i16(0)),
            dash_target_y: dash_combo
                .map(|v| v.target_y)
                .unwrap_or(Pico8Num::from_i16(0)),
            dash_accel_x: dash_combo
                .map(|v| v.accel_x)
                .unwrap_or(Pico8Num::from_i16(0)),
            dash_accel_y: dash_combo
                .map(|v| v.accel_y)
                .unwrap_or(Pico8Num::from_i16(0)),
        })
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
