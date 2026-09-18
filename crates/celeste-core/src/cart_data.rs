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
        let mut cart = CartData {
            map_data: Self::load_vec(&base_path.join("map-data.txt"), 8192)?,
            flag_data: Self::load_vec(&base_path.join("flag-data.txt"), 256)?,
        };
        // EXPERIMENT (2026-09-18): with `CELESTE_EXPERIMENT_FLAT_ROOM="x,y"` the
        // room at (x, y) is an empty room with a solid bottom row and the
        // player spawn standing on it - the smallest room with a player, for
        // counting what its kernels should be. A DIFFERENT GAME: nothing it
        // reports is an answer for the real cart.
        // `"x,y;tx:ty,..."` also places a fall floor (`fall_floor.tile`) at
        // each tile (tx, ty) of that room.
        if let Ok(spec) = std::env::var("CELESTE_EXPERIMENT_FLAT_ROOM") {
            const SOLID: u8 = 32; // flag bit 0
            const SPAWN: u8 = 1; // `player_spawn.tile`
            const FALL_FLOOR: u8 = 23;
            let (room, floors) = spec.split_once(';').unwrap_or((spec.as_str(), ""));
            let floors: Vec<(usize, usize)> = floors
                .split(',')
                .filter(|s| !s.trim().is_empty())
                .map(|s| s.split_once(':').and_then(|(a, b)| Some((a.trim().parse::<usize>().ok()?, b.trim().parse::<usize>().ok()?))).filter(|&(x, y)| x < 16 && y < 16))
                .collect::<Option<_>>()
                .ok_or_else(|| anyhow!("CELESTE_EXPERIMENT_FLAT_ROOM={spec:?}: floors are \"tx:ty\" tiles with tx, ty < 16"))?;
            let (rx, ry) = room
                .split_once(',')
                .and_then(|(a, b)| Some((a.trim().parse::<usize>().ok()?, b.trim().parse::<usize>().ok()?)))
                .filter(|&(x, y)| x < 8 && y < 4)
                .ok_or_else(|| anyhow!("CELESTE_EXPERIMENT_FLAT_ROOM={spec:?}: expected a room \"x,y\" with x < 8, y < 4"))?;
            for ty in 0..16 {
                for tx in 0..16 {
                    let tile = if ty == 15 {
                        SOLID
                    } else if (tx, ty) == (4, 14) {
                        SPAWN
                    } else if floors.contains(&(tx, ty)) {
                        FALL_FLOOR
                    } else {
                        0
                    };
                    cart.map_data[(ry * 16 + ty) * 128 + rx * 16 + tx] = tile;
                }
            }
            static BANNER: std::sync::Once = std::sync::Once::new();
            BANNER.call_once(|| eprintln!("[experiment] FLAT ROOM ({rx},{ry}): an empty room, a solid floor, the spawn and fall floors at tiles {floors:?}; this is not the real game"));
        }
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

    pub fn mget(&self, x: Pico8Num, y: Pico8Num) -> Result<u8> {
        let x = Self::as_usize_below(x, "x", 128)?;
        let y = Self::as_usize_below(y, "y", 64)?;
        Ok(self.map_data[x + (y * 128)])
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
