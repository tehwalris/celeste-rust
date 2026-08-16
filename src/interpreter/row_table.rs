//! The row table: every canonical boundary row the search has ever reached,
//! with a dense id assigned in discovery order.
//!
//! A "row" is one lane's complete canonical value tuple at a frame boundary -
//! one distinct game state - identified by two independently-seeded 64-bit
//! hashes with the state's shape hash mixed in (128 bits total; see
//! `subtract_visited` for the collision-risk note).
//!
//! The dense ids are what make the refinement passes cheap:
//!
//! * Ids are assigned in discovery order, and the table records the id
//!   counter at each frame boundary (`watermarks`). Frame f's newly
//!   discovered rows are exactly the ids in `[watermarks[f-1],
//!   watermarks[f])`, so the earliest-arrival frame of a row is a binary
//!   search over ~90 integers, and the over-approximate reachable set
//!   R~(f) = "earliest arrival <= f" is the PREFIX `id < watermarks[f]` -
//!   one integer, not a stored set.
//! * Any other subset of rows (the backward-viability sets B(f), the
//!   precision-refinement bands) is a bitmap indexed by id: ~19 MB per
//!   subset at 1.5e8 rows, with membership = one bit test.
//!
//! R~(f) over-approximates true frame-f reachability unconditionally (a
//! state reachable at f has earliest arrival <= f by definition). Whether
//! the room is delay-monotone (R~ exact) is deliberately not assumed
//! anywhere: every consumer needs only the over-approximation direction,
//! and achievability claims are established by concrete witness replay
//! only.

use rustc_hash::FxHashMap;

/// Seed of the second row hash (the first uses seed 0). Shared by the
/// forward pass and the backward sweep so both compute identical row keys.
pub const ROW_HASH_SEED2: u64 = 0xa076_1d64_78bd_642f;

/// See module docs.
#[derive(Default)]
pub struct RowTable {
    /// 128-bit row key (shape mixed in) -> dense id in discovery order.
    rows: FxHashMap<(u64, u64), u32>,
    /// Id counter at the end of each completed frame.
    watermarks: Vec<u32>,
    /// Keys inserted since the last `end_frame`, in id order - what the
    /// current frame's `.rowkeys` file records (`visited::Visited`).
    /// Empty on a table rebuilt `from_parts`: a resumed run only ever
    /// writes rowkeys for the frames it executes itself.
    recent: Vec<(u64, u64)>,
}

/// Splitmix64 finalizer - used to mix the shape hash into both key halves
/// independently so the combined key keeps its full 128-bit strength.
fn mix(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    x ^ (x >> 31)
}

impl RowTable {
    /// Fold a state's shape hash and a row's two hashes into the table key.
    pub fn key(shape_hash: u64, h1: u64, h2: u64) -> (u64, u64) {
        (h1 ^ mix(shape_hash), h2 ^ mix(shape_hash.wrapping_add(0x9e37_79b9_7f4a_7c15)))
    }

    /// Insert a row if new, returning `Some(id)` exactly when it was new.
    pub fn insert_new(&mut self, key: (u64, u64)) -> Option<u32> {
        let next = self.rows.len();
        debug_assert!(next < u32::MAX as usize, "row id space exhausted");
        match self.rows.entry(key) {
            std::collections::hash_map::Entry::Occupied(_) => None,
            std::collections::hash_map::Entry::Vacant(v) => {
                v.insert(next as u32);
                self.recent.push(key);
                Some(next as u32)
            }
        }
    }

    /// The keys inserted since the last `end_frame`, in id order. Call
    /// BEFORE `end_frame` when persisting the frame's `.rowkeys`.
    pub fn take_recent(&mut self) -> Vec<(u64, u64)> {
        std::mem::take(&mut self.recent)
    }

    pub fn id_of(&self, key: (u64, u64)) -> Option<u32> {
        self.rows.get(&key).copied()
    }

    /// Total distinct rows ever seen.
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Record the frame boundary: all ids assigned since the previous call
    /// belong to the frame that just completed.
    pub fn end_frame(&mut self) {
        self.watermarks.push(self.rows.len() as u32);
    }

    /// Id counter at the end of each completed frame (1-indexed by frame:
    /// `watermarks()[f-1]` is the counter after frame f).
    pub fn watermarks(&self) -> &[u32] {
        &self.watermarks
    }

    /// Earliest-arrival frame (1-indexed) of a row id: the first frame whose
    /// watermark exceeds it.
    pub fn earliest_frame(&self, id: u32) -> Option<u32> {
        let at = self.watermarks.partition_point(|&w| w <= id);
        (at < self.watermarks.len()).then_some(at as u32 + 1)
    }

    /// The rows in id order, for columnar serialization.
    pub fn rows_by_id(&self) -> Vec<(u64, u64)> {
        let mut out = vec![(0u64, 0u64); self.rows.len()];
        for (key, &id) in &self.rows {
            out[id as usize] = *key;
        }
        out
    }

    /// Rebuild from the id-ordered rows and watermarks (checkpoint load).
    pub fn from_parts(rows_by_id: Vec<(u64, u64)>, watermarks: Vec<u32>) -> Self {
        let rows: FxHashMap<(u64, u64), u32> = rows_by_id
            .into_iter()
            .enumerate()
            .map(|(id, key)| (key, id as u32))
            .collect();
        Self { rows, watermarks, recent: Vec::new() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ids_watermarks_and_earliest_frames() {
        let mut t = RowTable::default();
        assert_eq!(t.insert_new((1, 1)), Some(0));
        assert_eq!(t.insert_new((2, 2)), Some(1));
        assert_eq!(t.insert_new((1, 1)), None);
        t.end_frame(); // frame 1: ids 0..2
        assert_eq!(t.insert_new((3, 3)), Some(2));
        t.end_frame(); // frame 2: id 2
        t.end_frame(); // frame 3: nothing new

        assert_eq!(t.earliest_frame(0), Some(1));
        assert_eq!(t.earliest_frame(1), Some(1));
        assert_eq!(t.earliest_frame(2), Some(2));
        assert_eq!(t.earliest_frame(3), None); // never assigned

        let rebuilt = RowTable::from_parts(t.rows_by_id(), t.watermarks().to_vec());
        assert_eq!(rebuilt.id_of((2, 2)), Some(1));
        assert_eq!(rebuilt.len(), 3);
        assert_eq!(rebuilt.watermarks(), &[2, 3, 3]);
    }

    #[test]
    fn shape_mixing_separates_equal_row_hashes() {
        let a = RowTable::key(10, 5, 5);
        let b = RowTable::key(11, 5, 5);
        assert_ne!(a, b, "same row hashes under different shapes must differ");
    }
}
