use anyhow::Result;

use crate::pico8_num::Pico8Num;

/// The cart's map and tile flags: one table per process, part of the
/// binary (`CART`).
pub struct CartData {
    map_data: [u8; 8192],
    flag_data: [u8; 256],
}

/// THE CART'S DATA, decoded from `cart/` when the binary is built (as the
/// `sin` table is): what the binary runs is what it was built from, from
/// any directory.
pub static CART: CartData = CartData {
    map_data: hex_bytes(include_str!("../../../cart/map-data.txt")),
    flag_data: hex_bytes(include_str!("../../../cart/flag-data.txt")),
};

/// `text`'s hex digits (whitespace skipped) as exactly `N` bytes; anything
/// else fails the build.
const fn hex_bytes<const N: usize>(text: &str) -> [u8; N] {
    const fn digit(c: u8) -> u8 {
        match c {
            b'0'..=b'9' => c - b'0',
            b'a'..=b'f' => c - b'a' + 10,
            b'A'..=b'F' => c - b'A' + 10,
            _ => panic!("cart data: not a hex digit"),
        }
    }
    let b = text.as_bytes();
    let mut out = [0u8; N];
    let (mut i, mut n) = (0, 0);
    while i < b.len() {
        if !b[i].is_ascii_whitespace() {
            assert!(n < 2 * N, "cart data: too many bytes");
            out[n / 2] = out[n / 2] << 4 | digit(b[i]);
            n += 1;
        }
        i += 1;
    }
    assert!(n == 2 * N, "cart data: too few bytes");
    out
}

impl CartData {
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

    /// `fget` on whole numbers: tile `i`'s flag `b` (0..8).
    pub fn fget_whole(&self, i: u8, b: i16) -> Result<bool> {
        let b = usize::try_from(b).ok().filter(|&b| b < 8).ok_or_else(|| anyhow!("b is out of range"))?;
        Ok(self.flag_data[i as usize] & (1 << b) != 0)
    }

    pub fn fget(&self, i: Pico8Num, b: Pico8Num) -> Result<bool> {
        let i = Self::as_usize_below(i, "i", self.flag_data.len())?;
        let b = Self::as_usize_below(b, "b", 8)?;
        Ok(self.flag_data[i] & (1 << b) != 0)
    }

    fn as_usize_below(v: Pico8Num, name: &str, high_exclusive: usize) -> Result<usize> {
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
        let cart = &CART;
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
