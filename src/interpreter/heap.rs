use std::sync::Arc;

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

/// A slot in the heap overlay. None means "use base value", Some(v) means "override with v".
type OverlaySlot = Option<Option<HeapValue>>;

/// A copy-on-write heap implementation.
///
/// Uses a two-level structure inspired by the OCaml implementation:
/// - `old_values`: An immutable Arc<Vec> of values that was frozen at some point.
///   This is shared across all cloned states without copying.
/// - `new_values`: A COW Vec overlay that tracks new or changed values.
///   Uses Arc for O(1) cloning and COW on mutation.
///
/// The total size is `next_id`, where values in the overlay can either be
/// new allocations or modifications to old values.
#[derive(Clone, Debug)]
pub struct Heap {
    /// Immutable base values, shared via Arc across cloned states
    old_values: Arc<Vec<Option<HeapValue>>>,
    /// Overlay of new or changed values using Arc<Vec> with COW semantics.
    /// None = use old_values, Some(v) = use v (which can be None for empty slot)
    new_values: Arc<Vec<OverlaySlot>>,
    /// The next HeapId to allocate
    next_id: usize,
}

impl Serialize for Heap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as a simple Vec of Option<HeapValue>
        let values: Vec<Option<HeapValue>> = (0..self.next_id)
            .map(|i| self.get_opt(HeapId(i)).cloned())
            .collect();
        values.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Heap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let values: Vec<Option<HeapValue>> = Vec::deserialize(deserializer)?;
        let next_id = values.len();
        Ok(Heap {
            old_values: Arc::new(values),
            new_values: Arc::new(Vec::new()),
            next_id,
        })
    }
}

impl PartialEq for Heap {
    fn eq(&self, other: &Self) -> bool {
        // For PartialEq we need to compare actual contents
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

impl Heap {
    pub fn new() -> Self {
        Self {
            old_values: Arc::new(Vec::new()),
            new_values: Arc::new(Vec::new()),
            next_id: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.next_id
    }

    pub fn alloc(&mut self) -> HeapId {
        let id = HeapId(self.next_id);
        self.next_id += 1;
        // Don't store anything yet - it will be set via set()
        id
    }

    #[inline]
    pub fn get_opt(&self, id: HeapId) -> Option<&HeapValue> {
        // First check the overlay if index is in range
        if id.0 < self.new_values.len() {
            if let Some(overlay_value) = &self.new_values[id.0] {
                return overlay_value.as_ref();
            }
        }
        // Fall back to old values
        if id.0 < self.old_values.len() {
            self.old_values[id.0].as_ref()
        } else {
            None
        }
    }

    #[inline]
    pub fn get(&self, id: HeapId) -> &HeapValue {
        self.get_opt(id).expect("HeapId should point to a valid value")
    }

    pub fn get_mut(&mut self, id: HeapId) -> &mut HeapValue {
        let new_values = Arc::make_mut(&mut self.new_values);

        // Ensure overlay is large enough
        if id.0 >= new_values.len() {
            new_values.resize(id.0 + 1, None);
        }

        // If not in overlay yet, copy from old_values
        if new_values[id.0].is_none() {
            let value = if id.0 < self.old_values.len() {
                self.old_values[id.0].clone()
            } else {
                None
            };
            new_values[id.0] = Some(value);
        }

        new_values[id.0].as_mut().unwrap().as_mut().unwrap()
    }

    #[inline]
    pub fn set(&mut self, id: HeapId, value: HeapValue) {
        let new_values = Arc::make_mut(&mut self.new_values);

        // Ensure overlay is large enough
        if id.0 >= new_values.len() {
            new_values.resize(id.0 + 1, None);
        }

        new_values[id.0] = Some(Some(value));
    }

    pub fn map_in_place(&mut self, f: impl Fn(HeapValue) -> HeapValue) {
        let new_values = Arc::make_mut(&mut self.new_values);

        // Ensure overlay covers all ids
        if new_values.len() < self.next_id {
            new_values.resize(self.next_id, None);
        }

        for i in 0..self.next_id {
            // Get current value, taking from overlay if present to avoid clone
            let current = if new_values[i].is_some() {
                // Take from overlay to avoid clone
                new_values[i].take().unwrap()
            } else if i < self.old_values.len() {
                // Must clone from old_values
                self.old_values[i].clone()
            } else {
                None
            };

            // Apply f and store in overlay
            if let Some(val) = current {
                new_values[i] = Some(Some(f(val)));
            }
        }
    }

    /// Create a heap directly from a vector of values.
    /// This is more efficient than creating an empty heap and calling set() repeatedly.
    /// Pre-allocates the overlay with same capacity to avoid resizes during execution.
    pub fn from_values(values: Vec<Option<HeapValue>>) -> Self {
        let next_id = values.len();
        // Pre-allocate overlay with capacity to avoid resizes during execution
        let mut overlay = Vec::with_capacity(next_id);
        overlay.resize(next_id, None);
        Self {
            old_values: Arc::new(values),
            new_values: Arc::new(overlay),
            next_id,
        }
    }

    /// Freeze the current state, compacting everything into old_values.
    /// This is useful when you want to establish a new baseline for sharing.
    /// Call this before cloning when the heap won't change much.
    pub fn freeze(&mut self) {
        if self.new_values.is_empty() && self.next_id == self.old_values.len() {
            return; // Already frozen
        }

        let mut new_vec = Vec::with_capacity(self.next_id);
        for i in 0..self.next_id {
            // Check overlay first
            if i < self.new_values.len() {
                if let Some(overlay_value) = &self.new_values[i] {
                    new_vec.push(overlay_value.clone());
                    continue;
                }
            }
            // Fall back to old values
            if i < self.old_values.len() {
                new_vec.push(self.old_values[i].clone());
            } else {
                new_vec.push(None);
            }
        }
        self.old_values = Arc::new(new_vec);
        self.new_values = Arc::new(Vec::new());
    }
}
