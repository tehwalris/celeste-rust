use std::collections::HashMap;

use crate::{pico8_num::Pico8Vec2, player_flags::PlayerFlagBitVec};

pub type StateTable = PosMap<Pico8Vec2Map<PlayerFlagBitVec>>;

impl StateTable {
    pub fn print_stats(&self) {
        let mut positions = 0;
        let mut position_speed_pairs = 0;
        let mut reachable_states = 0;
        for (_pos, spd_map) in self.iter() {
            positions += 1;
            for (_spd, vec) in spd_map.iter() {
                position_speed_pairs += 1;
                reachable_states += vec.count_ones();
            }
        }
        println!(
            "positions: {}, position_speed_pairs: {}, reachable_states: {}",
            positions, position_speed_pairs, reachable_states
        );
    }
}

struct PosMapRange {
    min_x: i16,
    max_x: i16,
    min_y: i16,
    max_y: i16,
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
        if x >= r.min_x && x <= r.max_x && y >= r.min_y && y <= r.max_y {
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

pub struct Pico8Vec2Map<T>(HashMap<Pico8Vec2, T>);

impl<T> Pico8Vec2Map<T> {
    pub fn new() -> Self {
        Pico8Vec2Map(HashMap::new())
    }

    pub fn get_mut_or_insert_with<F>(&mut self, key: Pico8Vec2, make_default: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        self.0.entry(key.clone()).or_insert_with(make_default);
        self.0.get_mut(&key).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Pico8Vec2, &T)> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}
