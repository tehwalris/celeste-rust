use std::{cmp, mem};

use crate::{pico8_num::Pico8Vec2, player_flags::CompressedPlayerFlags};
use rustc_hash::FxHashSet;

pub type StateTable = PosMap<FxHashSet<(Pico8Vec2, CompressedPlayerFlags)>>;

impl StateTable {
    pub fn print_stats(&self) {
        let mut positions = 0;
        let mut reachable_states = 0;
        for (_pos, spd_player_flags_set) in self.iter() {
            positions += 1;
            reachable_states += spd_player_flags_set.len();
        }
        println!(
            "positions: {}, reachable_states: {}",
            positions, reachable_states
        );
    }

    pub fn extend(&mut self, mut other: StateTable) {
        for pos in other.range().clone().into_positions() {
            if let Some(other_data) = other.take(pos) {
                if self.get(pos).is_some() {
                    // HACK should not really be using _or_insert_with
                    let self_data = self.get_mut_or_insert_with(pos, FxHashSet::default);
                    self_data.extend(other_data.into_iter());
                } else {
                    self.get_mut_or_insert_with(pos, move || other_data);
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        for (pos, spd_player_flag_set) in self.iter() {
            if !spd_player_flag_set.is_empty() {
                return false;
            }
        }
        true
    }
}

impl Clone for StateTable {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            range: self.range.clone(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub struct PosMapRange {
    pub min_x: i16,
    pub max_x: i16,
    pub min_y: i16,
    pub max_y: i16,
}

impl PosMapRange {
    pub fn contains_pos(&self, (x, y): (i16, i16)) -> bool {
        x >= self.min_x && x <= self.max_x && y >= self.min_y && y <= self.max_y
    }

    pub fn chunks<'a>(&'a self, chunk_size: usize) -> impl Iterator<Item = PosMapRange> + 'a {
        assert!(chunk_size >= 1);

        Self::make_range(chunk_size, self.min_y, self.max_y).flat_map(move |(min_y, max_y)| {
            Self::make_range(chunk_size, self.min_x, self.max_x).map(move |(min_x, max_x)| {
                PosMapRange {
                    min_x,
                    max_x,
                    min_y,
                    max_y,
                }
            })
        })
    }

    fn make_range(chunk_size: usize, min: i16, max: i16) -> impl Iterator<Item = (i16, i16)> {
        assert!(min <= max);
        (min..=max)
            .step_by(chunk_size)
            .map(move |v| (v, cmp::min(max, v + i16::try_from(chunk_size).unwrap() - 1)))
    }

    pub fn into_positions(self) -> impl Iterator<Item = (i16, i16)> {
        Self::make_range(1, self.min_y, self.max_y).flat_map(move |(min_y, max_y)| {
            assert!(min_y == max_y);
            let y = min_y;
            Self::make_range(1, self.min_x, self.max_x).map(move |(min_x, max_x)| {
                assert!(min_x == max_x);
                let x = min_x;
                (x, y)
            })
        })
    }
}

pub struct PosMap<T> {
    data: Vec<Option<T>>,
    range: PosMapRange,
}

impl<T> PosMap<T> {
    pub fn new() -> Self {
        Self::new_for_range(PosMapRange {
            min_x: -8,
            max_x: 132,
            min_y: -8,
            max_y: 132,
        })
    }

    pub fn new_wide() -> Self {
        Self::new_for_range(PosMapRange {
            min_x: -16,
            max_x: 144,
            min_y: -16,
            max_y: 144,
        })
    }

    pub fn range(&self) -> &PosMapRange {
        &self.range
    }

    fn new_for_range(range: PosMapRange) -> Self {
        assert!(range.min_x <= range.max_x);
        assert!(range.min_y <= range.max_y);
        let size =
            ((range.max_x - range.min_x) as usize + 1) * ((range.max_y - range.min_y) as usize + 1);
        Self {
            data: (0..size).map(|_| None).collect(),
            range,
        }
    }

    fn index_from_pos(&self, (x, y): (i16, i16)) -> Option<usize> {
        let r = &self.range;
        if r.contains_pos((x, y)) {
            Some(
                ((x - r.min_x) as usize)
                    + ((y - r.min_y) as usize) * ((r.max_x - r.min_x) as usize),
            )
        } else {
            None
        }
    }

    pub fn get(&self, pos: (i16, i16)) -> Option<&T> {
        if let Some(i) = self.index_from_pos(pos) {
            self.data[i].as_ref()
        } else {
            None
        }
    }

    pub fn take(&mut self, pos: (i16, i16)) -> Option<T> {
        if let Some(i) = self.index_from_pos(pos) {
            mem::take(&mut self.data[i])
        } else {
            None
        }
    }

    pub fn set(&mut self, pos: (i16, i16), v: T) {
        if let Some(i) = self.index_from_pos(pos) {
            self.data[i] = Some(v);
        } else {
            panic!(
                "pos {:?} is out of range and resizing is not implemented",
                pos
            )
        }
    }

    pub fn get_mut_or_insert_with<F>(&mut self, pos: (i16, i16), make_default: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        if let Some(i) = self.index_from_pos(pos) {
            let entry = &mut self.data[i];
            if entry.is_none() {
                *entry = Some(make_default());
            }
            entry.as_mut().unwrap()
        } else {
            panic!(
                "pos {:?} is out of range and resizing is not implemented",
                pos
            )
        }
    }

    pub fn fill<F>(&mut self, get_value: F)
    where
        F: Fn(i16, i16) -> T,
    {
        for y in self.range.min_y..=self.range.max_y {
            for x in self.range.min_x..=self.range.max_x {
                let i = self.index_from_pos((x, y)).unwrap();
                self.data[i] = Some(get_value(x, y));
            }
        }
    }

    pub fn iter<'a>(&'a self) -> PosMapIterator<'a, T> {
        PosMapIterator {
            pos_map: self,
            pos: (self.range.min_x, self.range.min_y),
        }
    }
}

pub struct PosMapIterator<'a, T> {
    pos_map: &'a PosMap<T>,
    pos: (i16, i16),
}

impl<'a, T> Iterator for PosMapIterator<'a, T> {
    type Item = ((i16, i16), &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let i = self.pos_map.index_from_pos(self.pos)?;

            let result = self.pos_map.data[i].as_ref().map(|v| (self.pos, v));

            if self.pos.0 == self.pos_map.range.max_x {
                self.pos.0 = self.pos_map.range.min_x;
                self.pos.1 += 1;
            } else {
                self.pos.0 += 1;
            }

            if result.is_some() {
                return result;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, ops::Add, sync::atomic::AtomicU16};

    use super::PosMapRange;

    #[test]
    fn test_pos_map_range_chunks() {
        let whole_range = PosMapRange {
            min_x: -8,
            max_x: 10,
            min_y: -5,
            max_y: 2,
        };

        let mut expected_chunks = HashSet::new();
        expected_chunks.insert(PosMapRange {
            min_x: -8,
            max_x: 1,
            min_y: -5,
            max_y: 2,
        });
        expected_chunks.insert(PosMapRange {
            min_x: 2,
            max_x: 10,
            min_y: -5,
            max_y: 2,
        });

        let actual_chunks: HashSet<_> = whole_range.chunks(10).collect();

        assert_eq!(actual_chunks, expected_chunks);
    }
}
