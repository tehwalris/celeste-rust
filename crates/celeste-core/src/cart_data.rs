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

    /// The map and the sprite flags of a PICO-8 cartridge in its text form
    /// (`.p8`): `__map__` holds map rows 0-31 (128 bytes a line, as
    /// written); rows 32-63 share memory with the sprite sheet's lower half,
    /// `__gfx__` lines 64-127, where a line is 64 bytes as 128 PIXELS, one hex
    /// digit each, LOW nibble first (so two lines a map row, each byte's
    /// digits swapped); `__gff__` holds the 256 sprite flags (two lines).
    /// Lines a section leaves out are zero, as PICO-8 loads them; a line
    /// that is present must be whole. `.p8.png` is not read.
    pub fn from_p8(text: &str) -> Result<Self> {
        let section = |name: &str| -> Vec<&str> {
            let head = format!("__{name}__");
            let mut lines = text.lines().map(|l| l.trim_end_matches('\r'));
            if !lines.any(|l| l == head) {
                return Vec::new();
            }
            // A section runs to the next `__x__` header; blank lines are not rows.
            lines.take_while(|l| !(l.len() > 4 && l.starts_with("__") && l.ends_with("__"))).filter(|l| !l.trim().is_empty()).collect()
        };
        anyhow::ensure!(
            text.lines().next().is_some_and(|l| l.starts_with("pico-8 cartridge")),
            "not a PICO-8 text cartridge (.p8): the first line is not `pico-8 cartridge ...`"
        );
        let rows = |name: &str, max: usize, width: usize| -> Result<Vec<Vec<u8>>> {
            let lines = section(name);
            anyhow::ensure!(lines.len() <= max, "__{name}__: {} lines, at most {max}", lines.len());
            lines
                .iter()
                .enumerate()
                .map(|(i, l)| {
                    anyhow::ensure!(l.len() == 2 * width, "__{name}__ line {i}: {} hex digits, expected {}", l.len(), 2 * width);
                    hex::decode(l).map_err(|e| anyhow!("__{name}__ line {i}: {e}"))
                })
                .collect()
        };
        let mut map_data = vec![0u8; 8192];
        for (r, row) in rows("map", 32, 128)?.into_iter().enumerate() {
            map_data[r * 128..(r + 1) * 128].copy_from_slice(&row);
        }
        for (i, row) in rows("gfx", 128, 64)?.into_iter().enumerate().skip(64) {
            // Sprite-sheet line i is bytes 64*i.. of memory: map row 32 + (i-64)/2.
            let at = 32 * 128 + (i - 64) * 64;
            for (k, b) in row.into_iter().enumerate() {
                map_data[at + k] = b.rotate_left(4);
            }
        }
        let mut flag_data = vec![0u8; 256];
        for (r, row) in rows("gff", 2, 128)?.into_iter().enumerate() {
            flag_data[r * 128..(r + 1) * 128].copy_from_slice(&row);
        }
        Ok(CartData { map_data, flag_data })
    }

    /// The cart of a `.p8` file (`from_p8`).
    pub fn load_p8<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();
        let text = fs::read_to_string(path).map_err(|e| anyhow!("{}: {e}", path.display()))?;
        Self::from_p8(&text).map_err(|e| anyhow!("{}: {e}", path.display()))
    }

    /// The sprite flags (256 bytes, `fget`'s table).
    pub fn flag_grid(&self) -> &[u8] {
        &self.flag_data
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

    /// The ORIGINAL cart's `__gfx__`, `__gff__` and `__map__`, cut verbatim
    /// from celeste.p8 (test-fixtures/celeste-original-data.p8), read as
    /// `--cart` reads a mod: exactly cart/map-data.txt and cart/flag-data.txt
    /// (the lower half from the sprite sheet, nibbles swapped).
    #[test]
    fn the_original_p8_gives_the_cart_files() {
        let root = concat!(env!("CARGO_MANIFEST_DIR"), "/../..");
        let txt = CartData::load(format!("{root}/cart")).expect("cart");
        let p8 = CartData::load_p8(format!("{root}/test-fixtures/celeste-original-data.p8")).expect("p8");
        assert!(p8.map_data[..4096] == txt.map_data[..4096], "__map__: map rows 0-31");
        assert!(p8.map_data[4096..] == txt.map_data[4096..], "__gfx__ lines 64-127: map rows 32-63");
        assert_eq!(p8.flag_data, txt.flag_data, "__gff__");
    }

    /// Absent lines are zero; a short line or a non-text cart is refused.
    #[test]
    fn a_p8_reads_absent_lines_as_zero_and_refuses_a_bad_one() {
        let head = "pico-8 cartridge // http://www.pico-8.com\nversion 42\n";
        let empty = CartData::from_p8(&format!("{head}__lua__\n-- nothing\n")).expect("empty");
        assert!(empty.map_data.iter().all(|&b| b == 0) && empty.flag_data.iter().all(|&b| b == 0));
        // One map row, and gfx line 64 (map row 32's left half), nibble-swapped.
        let gfx: String = (0..65).map(|i| if i == 64 { format!("12{}\n", "0".repeat(126)) } else { format!("{}\n", "0".repeat(128)) }).collect();
        let c = CartData::from_p8(&format!("{head}__gfx__\n{gfx}__map__\n01{}\n", "0".repeat(254))).expect("partial");
        assert_eq!((c.mget_whole(0, 0), c.mget_whole(0, 32), c.mget_whole(1, 32)), (1, 0x21, 0));
        assert!(CartData::from_p8(&format!("{head}__map__\n0102\n")).is_err(), "a short map line");
        assert!(CartData::from_p8("not a cart\n__map__\n").is_err(), "not a .p8");
    }

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
