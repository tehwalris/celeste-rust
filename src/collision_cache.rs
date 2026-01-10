//! Precomputed collision lookup tables for fast solid_at queries.
//!
//! This replaces the interpreted solid_at/tile_flag_at calls with direct table lookups.

use crate::cart_data::CartData;
use crate::pico8_num::Pico8Num;
use anyhow::Result;
use std::cmp;

/// Rectangle parameters for tile_flag_at queries
struct TileFlagQuery {
    room_x: i16,
    room_y: i16,
    x: i16,
    y: i16,
    w: i16,
    h: i16,
    flag: i16,
}

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

/// 2D lookup table indexed by (x, y) position
pub struct PosMap<T> {
    data: Vec<T>,
    range: PosMapRange,
    width: usize,
}

impl<T: Clone + Default> PosMap<T> {
    pub fn new(range: PosMapRange) -> Self {
        let width = (range.max_x - range.min_x + 1) as usize;
        let height = (range.max_y - range.min_y + 1) as usize;
        Self {
            data: vec![T::default(); width * height],
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
    pub fn get(&self, x: i16, y: i16) -> Option<&T> {
        self.index(x, y).map(|i| &self.data[i])
    }

    pub fn set(&mut self, x: i16, y: i16, v: T) {
        if let Some(i) = self.index(x, y) {
            self.data[i] = v;
        }
    }

    pub fn fill<F>(&mut self, mut f: F)
    where
        F: FnMut(i16, i16) -> T,
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
    solid_player_hitbox: PosMap<bool>,

    /// tile_flag_at cache for flag 0 (solid), w=1, h=1 (single pixel)
    solid_1x1: PosMap<bool>,

    /// tile_flag_at cache for flag 0 (solid), w=8, h=8 (full tile)
    solid_8x8: PosMap<bool>,
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

        let mut solid_player_hitbox = PosMap::new(range.clone());
        let mut solid_1x1 = PosMap::new(range.clone());
        let mut solid_8x8 = PosMap::new(range.clone());

        // Precompute tile_flag_at for different hitbox sizes
        solid_player_hitbox.fill(|x, y| {
            // Player hitbox: x+1, y+3, w=6, h=5
            let query = TileFlagQuery {
                room_x,
                room_y,
                x: x + 1,
                y: y + 3,
                w: 6,
                h: 5,
                flag: 0,
            };
            Self::tile_flag_at_impl(cart_data, &query).unwrap_or(false)
        });

        solid_1x1.fill(|x, y| {
            let query = TileFlagQuery {
                room_x,
                room_y,
                x,
                y,
                w: 1,
                h: 1,
                flag: 0,
            };
            Self::tile_flag_at_impl(cart_data, &query).unwrap_or(false)
        });

        solid_8x8.fill(|x, y| {
            let query = TileFlagQuery {
                room_x,
                room_y,
                x,
                y,
                w: 8,
                h: 8,
                flag: 0,
            };
            Self::tile_flag_at_impl(cart_data, &query).unwrap_or(false)
        });

        Ok(Self {
            room_x,
            room_y,
            solid_player_hitbox,
            solid_1x1,
            solid_8x8,
        })
    }

    /// Lookup solid_at for player hitbox (w=6, h=5, with hitbox offset x+1, y+3)
    #[inline]
    pub fn solid_player(&self, x: i16, y: i16) -> Option<bool> {
        self.solid_player_hitbox.get(x, y).copied()
    }

    /// Lookup solid_at for 1x1 area
    #[inline]
    pub fn solid_1x1(&self, x: i16, y: i16) -> Option<bool> {
        self.solid_1x1.get(x, y).copied()
    }

    /// Lookup solid_at for 8x8 area
    #[inline]
    pub fn solid_8x8(&self, x: i16, y: i16) -> Option<bool> {
        self.solid_8x8.get(x, y).copied()
    }

    /// Generic solid_at lookup - falls back to computation if not cached
    pub fn solid_at(&self, cart_data: &CartData, x: i16, y: i16, w: i16, h: i16) -> Result<bool> {
        // Try cached lookups first for common cases
        if w == 6 && h == 5 {
            // This is the player hitbox case - but solid_player expects pre-offset coords
            // The caller should use solid_player directly if they know the offset
        }

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
        let query = TileFlagQuery {
            room_x: self.room_x,
            room_y: self.room_y,
            x,
            y,
            w,
            h,
            flag: 0,
        };
        Self::tile_flag_at_impl(cart_data, &query)
    }

    /// Implementation of tile_flag_at - checks if any tile in the rectangle has the given flag
    fn tile_flag_at_impl(cart_data: &CartData, query: &TileFlagQuery) -> Result<bool> {
        let tile_min_x = cmp::max(0, query.x / 8);
        let tile_max_x = cmp::min(15, (query.x + query.w - 1) / 8);
        let tile_min_y = cmp::max(0, query.y / 8);
        let tile_max_y = cmp::min(15, (query.y + query.h - 1) / 8);

        for ty in tile_min_y..=tile_max_y {
            for tx in tile_min_x..=tile_max_x {
                // Get tile at room-relative position
                let world_tx = query.room_x * 16 + tx;
                let world_ty = query.room_y * 16 + ty;

                let tile = cart_data.mget(
                    Pico8Num::from_i16(world_tx),
                    Pico8Num::from_i16(world_ty),
                )?;

                if cart_data.fget(
                    Pico8Num::from_i16(tile as i16),
                    Pico8Num::from_i16(query.flag),
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
    fn test_pos_map() {
        let range = PosMapRange {
            min_x: -10,
            max_x: 10,
            min_y: -10,
            max_y: 10,
        };
        let mut map: PosMap<i32> = PosMap::new(range);

        map.set(0, 0, 42);
        assert_eq!(map.get(0, 0), Some(&42));
        assert_eq!(map.get(-10, -10), Some(&0));
        assert_eq!(map.get(100, 100), None);
    }
}
