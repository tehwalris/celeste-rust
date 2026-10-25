//! A room's collision: the cart's `tile_flag_at` over the room's tiles,
//! precomputed for the boxes the cart asks with constants (`MAPPED`), the
//! tile scan as the fallback.

use crate::cart_data::CART;
use crate::pico8_num::Pico8Num;
use anyhow::Result;
use std::cmp;

/// The position range a `BoolMap` covers (inclusive).
#[derive(Clone)]
struct PosMapRange {
    min_x: i16,
    max_x: i16,
    min_y: i16,
    max_y: i16,
}

impl PosMapRange {
    fn contains(&self, x: i16, y: i16) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }
}

/// 2D bitmap indexed by (x, y) position. Bits, not bytes, so the maps
/// (probed at random from lane coordinates) stay cache-resident.
pub struct BoolMap {
    words: Vec<u64>,
    range: PosMapRange,
    width: usize,
}

impl BoolMap {
    fn new(range: PosMapRange) -> Self {
        let width = (range.max_x - range.min_x + 1) as usize;
        let height = (range.max_y - range.min_y + 1) as usize;
        Self {
            words: vec![0u64; (width * height).div_ceil(64)],
            range,
            width,
        }
    }

    #[inline]
    fn index(&self, x: i16, y: i16) -> Option<usize> {
        if self.range.contains(x, y) {
            Some(((x - self.range.min_x) as usize) + ((y - self.range.min_y) as usize) * self.width)
        } else {
            None
        }
    }

    #[inline]
    pub fn get(&self, x: i16, y: i16) -> Option<bool> {
        let i = self.index(x, y)?;
        Some((self.words[i >> 6] >> (i & 63)) & 1 != 0)
    }

    fn set(&mut self, x: i16, y: i16, v: bool) {
        if let Some(i) = self.index(x, y) {
            if v {
                self.words[i >> 6] |= 1u64 << (i & 63);
            } else {
                self.words[i >> 6] &= !(1u64 << (i & 63));
            }
        }
    }

    fn fill<F>(&mut self, mut f: F)
    where
        F: FnMut(i16, i16) -> bool,
    {
        for y in self.range.min_y..=self.range.max_y {
            for x in self.range.min_x..=self.range.max_x {
                let v = f(x, y);
                self.set(x, y, v);
            }
        }
    }
}

/// The boxes the cart asks `tile_flag_at` with a constant `(w, h, flag)`
/// often: the player's hitbox against solids and against ice, and an
/// object's default 8x8 hitbox against solids. Every other box (the 10x10,
/// 16x16 and 18x18 hitboxes, a per-lane one) is the scan's.
pub const MAPPED: [(i16, i16, i16); 3] = [(6, 5, 0), (6, 5, ICE_FLAG), (8, 8, 0)];

/// The ice flag: `ice_at(x, y, w, h)` is `tile_flag_at(x, y, w, h, 4)`.
pub const ICE_FLAG: i16 = 4;

/// A ROOM's tiles as the cart's `tile_flag_at` reads them: per `MAPPED`
/// box its answer at every query position near the room, precomputed by
/// the scan, and the scan for every other query.
pub struct Room {
    pub x: i16,
    pub y: i16,
    /// `MAPPED[i]`'s answers, indexed by the QUERY's `(x, y)`.
    maps: [BoolMap; MAPPED.len()],
}

impl Room {
    /// Room `(x, y)`'s tiles, built once in the process: every block,
    /// kernel and interpreter of a room reads the one `Room`.
    pub fn of((x, y): (i16, i16)) -> &'static Room {
        static ROOMS: std::sync::Mutex<Vec<&'static Room>> = std::sync::Mutex::new(Vec::new());
        let mut all = ROOMS.lock().expect("the rooms");
        if let Some(r) = all.iter().find(|r| (r.x, r.y) == (x, y)) {
            return r;
        }
        let r: &'static Room = Box::leak(Box::new(Room::new(x, y)));
        all.push(r);
        r
    }

    fn new(x: i16, y: i16) -> Room {
        // The room plus a margin.
        let range = PosMapRange { min_x: -32, max_x: 160, min_y: -32, max_y: 160 };
        let maps = MAPPED.map(|(w, h, flag)| {
            let mut map = BoolMap::new(range.clone());
            map.fill(|qx, qy| scan(x, y, qx, qy, w, h, flag).unwrap_or_else(|e| panic!("room ({x}, {y}): the tile scan at ({qx}, {qy}): {e}")));
            map
        });
        Room { x, y, maps }
    }

    /// The precomputed answers of `tile_flag_at(_, _, w, h, flag)`, if the
    /// box is `MAPPED`: a lane loop picks the map once per call.
    #[inline]
    pub fn map(&self, w: i16, h: i16, flag: i16) -> Option<&BoolMap> {
        MAPPED.iter().position(|&m| m == (w, h, flag)).map(|i| &self.maps[i])
    }

    /// `tile_flag_at(x, y, w, h, flag)`: the box's map where it covers the
    /// query, else the scan.
    pub fn flag_at(&self, x: i16, y: i16, w: i16, h: i16, flag: i16) -> Result<bool> {
        match self.map(w, h, flag).and_then(|m| m.get(x, y)) {
            Some(v) => Ok(v),
            None => scan(self.x, self.y, x, y, w, h, flag),
        }
    }
}

/// The tile scan: does any tile of room `(rx, ry)` under the box have
/// `flag`. The cart's loop, `for i=max(0,flr(x/8)),min(15,(x+w-1)/8)` (and
/// `j` likewise): its limit is a FRACTION, so the last column is the FLOOR
/// of `(x+w-1)/8`: a box wholly left of the room (`x+w-1` in [-7, -1])
/// scans nothing, not column 0 (rows likewise). The first column truncates
/// or floors alike under the `max`.
fn scan(rx: i16, ry: i16, x: i16, y: i16, w: i16, h: i16, flag: i16) -> Result<bool> {
    let tile_min_x = cmp::max(0, x / 8);
    let tile_max_x = cmp::min(15, (x + w - 1).div_euclid(8));
    let tile_min_y = cmp::max(0, y / 8);
    let tile_max_y = cmp::min(15, (y + h - 1).div_euclid(8));
    for ty in tile_min_y..=tile_max_y {
        for tx in tile_min_x..=tile_max_x {
            let tile = CART.mget(Pico8Num::from_i16(rx * 16 + tx), Pico8Num::from_i16(ry * 16 + ty))?;
            if CART.fget(Pico8Num::from_i16(tile as i16), Pico8Num::from_i16(flag))? {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every `MAPPED` box answers the scan at every query position, in the
    /// maps' range and past it (the scan's), in an ice room and two others;
    /// the maps are not uniform.
    #[test]
    fn a_mapped_box_answers_the_scan_everywhere() {
        for (rx, ry) in [(1, 0), (3, 1), (6, 2)] {
            let room = Room::new(rx, ry);
            for (w, h, flag) in MAPPED {
                let map = room.map(w, h, flag).expect("a mapped box");
                let mut hits = 0;
                for y in -40..=170 {
                    for x in -40..=170 {
                        let want = scan(rx, ry, x, y, w, h, flag).expect("the scan");
                        assert_eq!(room.flag_at(x, y, w, h, flag).expect("flag_at"), want, "room ({rx},{ry}) {w}x{h} flag {flag} at ({x}, {y})");
                        assert!(map.get(x, y).is_none_or(|v| v == want));
                        hits += want as usize;
                    }
                }
                assert!(hits > 0 || (flag == ICE_FLAG && (rx, ry) != (3, 1)), "room ({rx},{ry}) {w}x{h} flag {flag}: no hit");
            }
            assert!(room.map(1, 1, 0).is_none() && room.map(8, 8, ICE_FLAG).is_none(), "only the MAPPED boxes have a map");
        }
    }

    #[test]
    fn bool_map_indexes_and_bounds_check() {
        let range = PosMapRange {
            min_x: -10,
            max_x: 10,
            min_y: -10,
            max_y: 10,
        };
        let mut map = BoolMap::new(range);

        map.set(0, 0, true);
        // A neighbour in the same 64-bit word must be unaffected.
        assert_eq!(map.get(0, 0), Some(true));
        assert_eq!(map.get(1, 0), Some(false));
        assert_eq!(map.get(-1, 0), Some(false));
        assert_eq!(map.get(-10, -10), Some(false));
        assert_eq!(map.get(10, 10), Some(false));
        assert_eq!(map.get(100, 100), None);
        assert_eq!(map.get(-11, 0), None);

        // Clearing a bit must leave its word's other bits alone.
        map.set(1, 0, true);
        map.set(0, 0, false);
        assert_eq!(map.get(0, 0), Some(false));
        assert_eq!(map.get(1, 0), Some(true));

        // Every position round-trips, including across word boundaries.
        let mut map = BoolMap::new(PosMapRange { min_x: -10, max_x: 10, min_y: -10, max_y: 10 });
        for y in -10..=10i16 {
            for x in -10..=10i16 {
                map.set(x, y, (x as i32 + 3 * y as i32).rem_euclid(7) == 0);
            }
        }
        for y in -10..=10i16 {
            for x in -10..=10i16 {
                assert_eq!(
                    map.get(x, y),
                    Some((x as i32 + 3 * y as i32).rem_euclid(7) == 0),
                    "at ({}, {})",
                    x,
                    y
                );
            }
        }
    }
}
