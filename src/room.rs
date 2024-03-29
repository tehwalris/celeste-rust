use anyhow::Result;
use std::{any, cmp};

use crate::{
    cart_data::{self, CartData},
    pico8_num::{int, Pico8Num, Pico8Vec2},
    sign::SignVec2,
};

pub struct Room<'a> {
    x: Pico8Num,
    y: Pico8Num,
    cart_data: &'a CartData,
}

impl<'a> Room<'a> {
    pub fn from_position(cart_data: &CartData, x: Pico8Num, y: Pico8Num) -> Result<Room> {
        if x.as_i16() == Some(1) && y.as_i16() == Some(0) {
            Ok(Room { x, y, cart_data })
        } else {
            Err(anyhow!("unsupported room"))
        }
    }

    pub fn spawn_pos(&self) -> Pico8Vec2 {
        // TODO This is only valid for room 2. Generally we'll need to simulate
        // the spawn.
        Pico8Vec2 {
            x: int(8),
            y: int(112),
        }
    }

    pub fn is_world_still(&self) -> bool {
        // TODO This is only valid for room 2
        true
    }

    pub fn tile_at(&self, x: Pico8Num, y: Pico8Num) -> Result<u8> {
        self.cart_data
            .mget(self.x * int(16) + x, self.y * int(16) + y)
    }

    pub fn tile_flag_at(
        &self,
        x: Pico8Num,
        y: Pico8Num,
        w: Pico8Num,
        h: Pico8Num,
        flag: Pico8Num,
    ) -> Result<bool> {
        for i in cmp::max(int(0), (x / int(8)).flr()).as_i16_or_err()?
            ..=cmp::min(int(15), (x + w - int(1)) / int(8))
                .flr()
                .as_i16_or_err()?
        {
            for j in cmp::max(int(0), (y / int(8)).flr()).as_i16_or_err()?
                ..=cmp::min(int(15), (y + h - int(1)) / int(8))
                    .flr()
                    .as_i16_or_err()?
            {
                if self
                    .cart_data
                    .fget(int(i16::from(self.tile_at(int(i), int(j))?)), flag)?
                {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub fn solid_at(&self, x: Pico8Num, y: Pico8Num, w: Pico8Num, h: Pico8Num) -> Result<bool> {
        self.tile_flag_at(x, y, w, h, int(0))
    }

    pub fn spikes_at(
        &self,
        x: Pico8Num,
        y: Pico8Num,
        w: Pico8Num,
        h: Pico8Num,
        spd: SignVec2,
    ) -> Result<bool> {
        for i in cmp::max(int(0), (x / int(8)).flr()).as_i16_or_err()?
            ..=cmp::min(int(15), (x + w - int(1)) / int(8))
                .flr()
                .as_i16_or_err()?
        {
            let i = int(i);
            for j in cmp::max(int(0), (y / int(8)).flr()).as_i16_or_err()?
                ..=cmp::min(int(15), (y + h - int(1)) / int(8))
                    .flr()
                    .as_i16_or_err()?
            {
                let j = int(j);
                let tile = int(i16::from(self.tile_at(i, j)?));
                if tile == int(17)
                    && ((y + h - int(1)) % int(8) >= int(6) || y + h == j * int(8) + int(8))
                    && spd.y.greater_equal_zero()
                {
                    return Ok(true);
                } else if tile == int(27) && y % int(8) <= int(2) && spd.y.smaller_equal_zero() {
                    return Ok(true);
                } else if tile == int(43) && y % int(8) <= int(2) && spd.x.smaller_equal_zero() {
                    return Ok(true);
                } else if tile == int(59)
                    && ((x + w - int(1)) % int(8) >= int(6) || x + w == i * int(8) + int(8))
                    && spd.x.greater_equal_zero()
                {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }
}
