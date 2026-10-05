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
    /// directly with their own bounds handling.
    pub fn map_grid(&self) -> &[u8] {
        &self.map_data
    }

    /// PICO-8's `mget`: the tile at a whole-number map coordinate, 0 outside
    /// the 128x64 map. A branch-free kernel reads outside on loop iterations a
    /// lane does not take. A fractional coordinate is refused: the traced
    /// cart never makes one.
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
        // `ok_or_else`: the error message is built lazily (hot path).
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

    /// Every level room (all but the summit (7,3)) has exactly one player
    /// spawn tile. Guards the map's lower half: rows 32-63 share memory with
    /// the sprite sheet and are stored nibble-swapped in `__gfx__`.
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
