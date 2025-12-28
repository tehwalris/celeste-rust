use std::sync::Arc;

use im::HashMap as ImHashMap;
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

/// A copy-on-write heap implementation.
///
/// Uses a two-level structure inspired by the OCaml implementation:
/// - `old_values`: An immutable Arc<Vec> of values that was frozen at some point.
///   This is shared across all cloned states without copying.
/// - `new_values`: A persistent HashMap (from `im` crate) that overlays new or
///   changed values. This allows efficient cloning by structural sharing.
///
/// The total size is `old_values.len() + next_new_id`, where values in the
/// `new_values` overlay can either be new allocations or modifications to old values.
#[derive(Clone, Debug)]
pub struct Heap {
    /// Immutable base values, shared via Arc across cloned states
    old_values: Arc<Vec<Option<HeapValue>>>,
    /// Overlay of new or changed values using a persistent HashMap
    new_values: ImHashMap<usize, Option<HeapValue>>,
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
        Ok(Heap {
            old_values: Arc::new(values.clone()),
            new_values: ImHashMap::new(),
            next_id: values.len(),
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
            new_values: ImHashMap::new(),
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

    pub fn get_opt(&self, id: HeapId) -> Option<&HeapValue> {
        // First check the overlay
        if let Some(value) = self.new_values.get(&id.0) {
            return value.as_ref();
        }
        // Fall back to old values
        if id.0 < self.old_values.len() {
            self.old_values[id.0].as_ref()
        } else {
            None
        }
    }

    pub fn get(&self, id: HeapId) -> &HeapValue {
        self.get_opt(id).expect("HeapId should point to a valid value")
    }

    pub fn get_mut(&mut self, id: HeapId) -> &mut HeapValue {
        // Ensure the value is in the overlay so we can mutate it
        if !self.new_values.contains_key(&id.0) {
            // Copy from old_values into the overlay
            let value = if id.0 < self.old_values.len() {
                self.old_values[id.0].clone()
            } else {
                None
            };
            self.new_values.insert(id.0, value);
        }
        self.new_values.get_mut(&id.0).unwrap().as_mut().unwrap()
    }

    pub fn set(&mut self, id: HeapId, value: HeapValue) {
        self.new_values.insert(id.0, Some(value));
    }

    pub fn map_in_place(&mut self, f: impl Fn(HeapValue) -> HeapValue) {
        // First, apply to old values that aren't overridden
        for i in 0..self.old_values.len() {
            if !self.new_values.contains_key(&i) {
                if let Some(value) = &self.old_values[i] {
                    self.new_values.insert(i, Some(f(value.clone())));
                }
            }
        }
        // Then apply to overlay values
        self.new_values = self.new_values.iter()
            .map(|(k, v)| {
                let new_v = match v {
                    Some(val) => Some(f(val.clone())),
                    None => None,
                };
                (*k, new_v)
            })
            .collect();
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
            if let Some(value) = self.new_values.get(&i) {
                new_vec.push(value.clone());
            } else if i < self.old_values.len() {
                new_vec.push(self.old_values[i].clone());
            } else {
                new_vec.push(None);
            }
        }
        self.old_values = Arc::new(new_vec);
        self.new_values = ImHashMap::new();
    }
}
