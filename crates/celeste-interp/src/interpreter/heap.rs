use std::sync::Arc;

use elsa::sync::FrozenVec;
use serde::{Deserialize, Serialize};

use super::value::HeapValue;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct HeapId(usize);

impl HeapId {
    pub fn from_raw(id: usize) -> Self {
        Self(id)
    }

    pub fn raw(&self) -> usize {
        self.0
    }
}

/// A copy-on-write heap implementation optimized for cheap cloning.
///
/// Uses an append-only FrozenVec shared via Arc for stable storage,
/// with a small Vec overlay for local modifications. This provides:
/// - Very cheap cloning: Arc::clone + small Vec clone
/// - Fast lookups: Linear scan of small overlay + O(1) FrozenVec access
/// - Efficient writes: Append to overlay, compact when too large
///
/// The overlay is kept small (< 64 entries) by compacting into the base
/// when it grows too large.
#[derive(Clone)]
pub struct Heap {
    /// Append-only storage shared across all cloned states.
    storage: Arc<FrozenVec<Box<HeapValue>>>,
    /// Small overlay mapping HeapId -> storage index.
    /// Kept sorted by HeapId for binary search.
    /// When this grows too large, we compact into a new storage.
    overlay: Vec<(usize, usize)>,
    /// Base index: maps HeapId -> storage index for values not in overlay.
    /// This is the "frozen" part that's shared via Arc.
    base_index: Arc<Vec<usize>>,
    /// The next HeapId to allocate
    next_id: usize,
}

// Custom Debug implementation since FrozenVec doesn't implement Debug
impl std::fmt::Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap")
            .field("next_id", &self.next_id)
            .field("overlay_len", &self.overlay.len())
            .field("storage_len", &self.storage.len())
            .finish()
    }
}


impl PartialEq for Heap {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for i in 0..self.len() {
            let id = HeapId(i);
            if self.get_opt(id) != other.get_opt(id) {
                return false;
            }
        }
        true
    }
}

impl Eq for Heap {}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            storage: Arc::new(FrozenVec::new()),
            overlay: Vec::new(),
            base_index: Arc::new(Vec::new()),
            next_id: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.next_id
    }

    pub fn alloc(&mut self) -> HeapId {
        let id = HeapId(self.next_id);
        self.next_id += 1;
        id
    }

    /// Look up storage index in overlay using binary search
    #[inline]
    fn overlay_get(&self, id: usize) -> Option<usize> {
        match self.overlay.binary_search_by_key(&id, |&(k, _)| k) {
            Ok(pos) => Some(self.overlay[pos].1),
            Err(_) => None,
        }
    }

    pub fn get_opt(&self, id: HeapId) -> Option<&HeapValue> {
        // First check overlay
        if let Some(storage_idx) = self.overlay_get(id.0) {
            return self.storage.get(storage_idx);
        }

        // Fall back to base index
        if id.0 < self.base_index.len() {
            let storage_idx = self.base_index[id.0];
            if storage_idx != usize::MAX {
                return self.storage.get(storage_idx);
            }
        }

        None
    }

    pub fn get(&self, id: HeapId) -> &HeapValue {
        self.get_opt(id)
            .expect("HeapId should point to a valid value")
    }

    pub fn set(&mut self, id: HeapId, value: HeapValue) {
        // Use push_get_index to atomically get the index while pushing
        // This avoids race conditions when multiple threads share the storage Arc
        let storage_idx = self.storage.push_get_index(Box::new(value));

        // Update overlay, keeping it sorted
        match self.overlay.binary_search_by_key(&id.0, |&(k, _)| k) {
            Ok(pos) => {
                // Update existing entry
                self.overlay[pos].1 = storage_idx;
            }
            Err(pos) => {
                // Insert new entry at sorted position
                self.overlay.insert(pos, (id.0, storage_idx));
            }
        }

        // Compact if overlay is too large
        if self.overlay.len() > 64 {
            self.compact();
        }
    }

    /// Compact the overlay into the base index
    fn compact(&mut self) {
        if self.overlay.is_empty() {
            return;
        }

        // Build new base index by merging overlay with existing base_index
        // Since overlay is sorted, we can do this efficiently without lookups
        let mut new_base = Vec::with_capacity(self.next_id);
        let mut overlay_idx = 0;
        let overlay = &self.overlay;
        let base = &self.base_index;

        for id in 0..self.next_id {
            // Check if this id is in overlay (overlay is sorted)
            if overlay_idx < overlay.len() && overlay[overlay_idx].0 == id {
                new_base.push(overlay[overlay_idx].1);
                overlay_idx += 1;
            } else if id < base.len() {
                new_base.push(base[id]);
            } else {
                new_base.push(usize::MAX);
            }
        }

        self.base_index = Arc::new(new_base);
        self.overlay.clear();
    }

    /// Filter vectors down to the `kept` lanes, only cloning values that
    /// need transformation. This is more efficient than map_in_place for
    /// filter_vectors operations because it avoids cloning values that
    /// don't contain vectors.
    pub fn filter_vectors_in_place(&mut self, kept: &super::value::KeptLanes) {
        // For filter operations, we only need to update values that contain vectors.
        // Non-vector values can keep their existing storage indices.

        // Check if we have any overlay entries - if so, compact first for simplicity
        if !self.overlay.is_empty() {
            self.compact();
        }

        // Now build new base index, only updating entries that need transformation
        let mut new_base = Vec::with_capacity(self.next_id);

        for id in 0..self.next_id {
            let old_storage_idx = if id < self.base_index.len() {
                self.base_index[id]
            } else {
                usize::MAX
            };

            if old_storage_idx == usize::MAX {
                new_base.push(usize::MAX);
                continue;
            }

            let value = self.storage.get(old_storage_idx).expect("valid storage index");

            // Check if this value needs transformation
            if let Some(new_value) = value.filter_vectors_if_needed(kept) {
                // Value was transformed - store the new value
                // Use push_get_index for atomic index assignment
                let new_storage_idx = self.storage.push_get_index(Box::new(new_value));
                new_base.push(new_storage_idx);
            } else {
                // Value unchanged - keep the old storage index
                new_base.push(old_storage_idx);
            }
        }

        self.base_index = Arc::new(new_base);
    }

}
