//! Precomputed collision lookup tables for fast solid_at queries.
//!
//! This replaces the interpreted solid_at/tile_flag_at calls with direct table lookups.

use crate::cart_data::CartData;
use crate::pico8_num::Pico8Num;
use anyhow::Result;
use std::cmp;

/// Range for position map
#[derive(Clone)]
pub struct PosMapRange {
    pub min_x: i16,
    pub max_x: i16,
    pub min_y: i16,
    pub max_y: i16,
}

impl PosMapRange {
    pub fn contains(&self, x: i16, y: i16) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }
}

/// 2D bitmap indexed by (x, y) position.
///
/// One bit per position rather than one byte. The three solid maps cover
/// 193x193 positions each; as `Vec<bool>` that is 112 KB of tables that 16
/// frame-worker threads probe at random from lane coordinates, and as bits
/// it is 14 KB - small enough to stay resident next to everything else the
/// frame body is touching. (Philippe: "for tile_flag_at we could
/// significantly compress the memory that it reads"; a room is 16x16 tiles,
/// so the underlying information is 256 bits.)
pub struct BoolMap {
    words: Vec<u64>,
    range: PosMapRange,
    width: usize,
}

impl BoolMap {
    pub fn new(range: PosMapRange) -> Self {
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

/// Precomputed collision cache for a specific room
pub struct CollisionCache {
    /// Room coordinates (for validation)
    pub room_x: i16,
    pub room_y: i16,

    /// tile_flag_at cache for flag 0 (solid), w=6, h=5 (player hitbox)
    /// Indexed by (x + hitbox_x, y + hitbox_y) where hitbox is (1, 3)
    solid_player_hitbox: BoolMap,

    /// tile_flag_at cache for flag 0 (solid), w=1, h=1 (single pixel)
    solid_1x1: BoolMap,

    /// tile_flag_at cache for flag 0 (solid), w=8, h=8 (full tile)
    solid_8x8: BoolMap,
}

impl CollisionCache {
    /// Create a new collision cache for the given room
    pub fn new(cart_data: &CartData, room_x: i16, room_y: i16) -> Result<Self> {
        // Range that covers player movement area with some margin
        let range = PosMapRange {
            min_x: -32,
            max_x: 160,
            min_y: -32,
            max_y: 160,
        };

        let mut solid_player_hitbox = BoolMap::new(range.clone());
        let mut solid_1x1 = BoolMap::new(range.clone());
        let mut solid_8x8 = BoolMap::new(range.clone());

        // Precompute tile_flag_at for different hitbox sizes
        solid_player_hitbox.fill(|x, y| {
            // Player hitbox: x+1, y+3, w=6, h=5
            Self::tile_flag_at_impl(cart_data, room_x, room_y, x + 1, y + 3, 6, 5, 0)
                .unwrap_or(false)
        });

        solid_1x1.fill(|x, y| {
            Self::tile_flag_at_impl(cart_data, room_x, room_y, x, y, 1, 1, 0)
                .unwrap_or(false)
        });

        solid_8x8.fill(|x, y| {
            Self::tile_flag_at_impl(cart_data, room_x, room_y, x, y, 8, 8, 0)
                .unwrap_or(false)
        });

        Ok(Self {
            room_x,
            room_y,
            solid_player_hitbox,
            solid_1x1,
            solid_8x8,
        })
    }

    /// Lookup solid_at for 1x1 area
    #[inline]
    pub fn solid_1x1(&self, x: i16, y: i16) -> Option<bool> {
        self.solid_1x1.get(x, y)
    }

    /// Lookup solid_at for 8x8 area
    #[inline]
    pub fn solid_8x8(&self, x: i16, y: i16) -> Option<bool> {
        self.solid_8x8.get(x, y)
    }

    /// The precomputed map answering `tile_flag_at(x, y, w, h, flag=0)`,
    /// with the offset its rows were built at: look up `(x + dx, y + dy)`.
    ///
    /// Exists so a caller looping over lanes can pick the map ONCE instead
    /// of re-testing the (w, h) pair per lane - w and h are the same
    /// constant for every lane of a call.
    #[inline]
    pub fn solid_map(&self, w: i16, h: i16) -> Option<(&BoolMap, i16, i16)> {
        match (w, h) {
            // The player hitbox map is indexed by the object position, so
            // it undoes the (1, 3) hitbox offset the caller applied.
            (6, 5) => Some((&self.solid_player_hitbox, -1, -3)),
            (1, 1) => Some((&self.solid_1x1, 0, 0)),
            (8, 8) => Some((&self.solid_8x8, 0, 0)),
            _ => None,
        }
    }

    /// Generic solid_at lookup - falls back to computation if not cached
    pub fn solid_at(&self, cart_data: &CartData, x: i16, y: i16, w: i16, h: i16) -> Result<bool> {
        // Try cached lookups first for common cases
        if w == 1 && h == 1 {
            if let Some(v) = self.solid_1x1(x, y) {
                return Ok(v);
            }
        }

        if w == 8 && h == 8 {
            if let Some(v) = self.solid_8x8(x, y) {
                return Ok(v);
            }
        }

        // Fall back to computation
        Self::tile_flag_at_impl(cart_data, self.room_x, self.room_y, x, y, w, h, 0)
    }

    /// Implementation of tile_flag_at - checks if any tile in the rectangle has the given flag
    fn tile_flag_at_impl(
        cart_data: &CartData,
        room_x: i16,
        room_y: i16,
        x: i16,
        y: i16,
        w: i16,
        h: i16,
        flag: i16,
    ) -> Result<bool> {
        let tile_min_x = cmp::max(0, x / 8);
        let tile_max_x = cmp::min(15, (x + w - 1) / 8);
        let tile_min_y = cmp::max(0, y / 8);
        let tile_max_y = cmp::min(15, (y + h - 1) / 8);

        for ty in tile_min_y..=tile_max_y {
            for tx in tile_min_x..=tile_max_x {
                // Get tile at room-relative position
                let world_tx = room_x * 16 + tx;
                let world_ty = room_y * 16 + ty;

                let tile = cart_data.mget(
                    Pico8Num::from_i16(world_tx),
                    Pico8Num::from_i16(world_ty),
                )?;

                if cart_data.fget(
                    Pico8Num::from_i16(tile as i16),
                    Pico8Num::from_i16(flag),
                )? {
                    return Ok(true);
                }
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // A neighbour in the same 64-bit word must be unaffected, which is
        // the failure mode a bitmap has and a byte array does not.
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
