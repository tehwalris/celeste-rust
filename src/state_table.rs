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

    pub fn new_wide() -> Self {
        Self::new_for_range(PosMapRange {
            min_x: -16,
            max_x: 144,
            min_y: -16,
            max_y: 144,
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
