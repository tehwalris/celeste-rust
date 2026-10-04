use std::{fs, path::Path};

use anyhow::Result;

use crate::pico8_num::Pico8Num;

pub struct CartData {
    map_data: Vec<u8>,
    flag_data: Vec<u8>,
}

impl CartData {
    pub fn load<P>(base_path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let base_path = base_path.as_ref();
        let cart = CartData {
            map_data: Self::load_vec(&base_path.join("map-data.txt"), 8192)?,
            flag_data: Self::load_vec(&base_path.join("flag-data.txt"), 256)?,
        };
        Ok(cart)
    }

    fn load_vec(path: &Path, expected_len: usize) -> Result<Vec<u8>> {
        let mut raw = fs::read_to_string(path)?;
        raw.retain(|c| !c.is_whitespace());
        let decoded = hex::decode(raw)?;
        if decoded.len() == expected_len {
            Ok(decoded)
        } else {
            Err(anyhow!(
                "Wrong length: got {}, expected {}",
                decoded.len(),
                expected_len
            ))
        }
    }

    /// The raw 128x64 map grid (row-major), for hot paths that index it
    /// directly with their own bounds handling (the lane kernel: `mget`
    /// through the Result machinery was 13.5% of its profile).
    pub fn map_grid(&self) -> &[u8] {
        &self.map_data
    }

    /// PICO-8's `mget`: the tile at a whole-number map coordinate, and 0
    /// outside the 128x64 map (as PICO-8 returns). The cart's own reads stay
    /// inside (`tile_flag_at` / `spikes_at` clamp to the room), but a
    /// branch-free kernel evaluates an unrolled loop iteration a lane does not
    /// take too - `spikes_at`'s second column for a player at x >= 119 reads
    /// tile column 16 of the room, x = 128 (room (7,0), 2026-09-30). A
    /// fractional coordinate is refused: the traced cart never makes one.
    pub fn mget(&self, x: Pico8Num, y: Pico8Num) -> Result<u8> {
        let x = x.as_i16().ok_or_else(|| anyhow!("mget: x is not an integer"))?;
        let y = y.as_i16().ok_or_else(|| anyhow!("mget: y is not an integer"))?;
        Ok(self.mget_whole(x, y))
    }

    /// `mget` on whole numbers: 0 outside the map.
    pub fn mget_whole(&self, x: i16, y: i16) -> u8 {
        if (0..128).contains(&x) && (0..64).contains(&y) {
            self.map_data[x as usize + y as usize * 128]
        } else {
            0
        }
    }

    pub fn fget(&self, i: Pico8Num, b: Pico8Num) -> Result<bool> {
        let i = Self::as_usize_below(i, "i", self.flag_data.len())?;
        let b = Self::as_usize_below(b, "b", 8)?;
        Ok(self.flag_data[i] & (1 << b) != 0)
    }

    fn as_usize_below(v: Pico8Num, name: &str, high_exclusive: usize) -> Result<usize> {
        // Use ok_or_else for lazy error message construction (significant perf win!)
        let v = v.as_i16().ok_or_else(|| anyhow!("{} is not an integer", name))?;
        if v >= 0 && (v as usize) < high_exclusive {
            Ok(v as usize)
        } else {
            Err(anyhow!("{} is out of range", name))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every level room (31 of them: (0,0)..(7,3) but the summit's (7,3)) has
    /// exactly ONE player spawn tile. Map rows 32-63 share memory with the
    /// lower sprite sheet and are written in a cart's `__gfx__` with each
    /// byte's nibbles swapped; `map-data.txt` held them unswapped until
    /// 2026-10-01, which gave the rooms of rows 2 and 3 two to seven spawns
    /// each (and garbage everywhere else in them) - found when the community
    /// TAS for room (0,2) never exited.
    #[test]
    fn every_level_room_has_one_player_spawn() {
        let cart = CartData::load(concat!(env!("CARGO_MANIFEST_DIR"), "/../../cart")).expect("cart");
        for ry in 0..4i16 {
            for rx in 0..8i16 {
                if (rx, ry) == (7, 3) {
                    continue;
                }
                let n = (0..16i16)
                    .flat_map(|ty| (0..16i16).map(move |tx| (tx, ty)))
                    .filter(|&(tx, ty)| cart.mget_whole(rx * 16 + tx, ry * 16 + ty) == 1)
                    .count();
                assert_eq!(n, 1, "room ({rx},{ry}) has {n} player spawn tiles");
            }
        }
    }
}
