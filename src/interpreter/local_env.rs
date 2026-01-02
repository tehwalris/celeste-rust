use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::ir::LocalId;

use super::value::Value;

/// A local environment storing local variable bindings.
///
/// Uses a Vec-based copy-on-write implementation for performance:
/// - O(1) get and set operations
/// - O(1) clone via Arc (structural sharing)
/// - O(n) iteration (much faster than HAMT iteration)
/// - Copy-on-write semantics for mutation
///
/// The Vec stores Option<Value> to allow sparse storage without paying
/// the cost of hash-based lookups.
#[derive(Clone, Debug)]
pub struct LocalEnv {
    /// The underlying storage. Uses Arc for O(1) cloning.
    values: Arc<Vec<Option<Value>>>,
}

impl Serialize for LocalEnv {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as a Vec of (LocalId, Value) pairs (only non-None values)
        let pairs: Vec<(usize, &Value)> = self.iter().collect();
        pairs.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for LocalEnv {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let pairs: Vec<(usize, Value)> = Vec::deserialize(deserializer)?;
        let max_id = pairs.iter().map(|(id, _)| *id).max().unwrap_or(0);
        let mut values = vec![None; max_id + 1];
        for (id, value) in pairs {
            values[id] = Some(value);
        }
        Ok(LocalEnv {
            values: Arc::new(values),
        })
    }
}

impl PartialEq for LocalEnv {
    fn eq(&self, other: &Self) -> bool {
        // Fast path: same Arc means definitely equal
        if Arc::ptr_eq(&self.values, &other.values) {
            return true;
        }
        // Compare contents (only non-None values matter)
        let max_len = self.values.len().max(other.values.len());
        for i in 0..max_len {
            let a = self.values.get(i).and_then(|v| v.as_ref());
            let b = other.values.get(i).and_then(|v| v.as_ref());
            if a != b {
                return false;
            }
        }
        true
    }
}

impl Eq for LocalEnv {}

impl LocalEnv {
    pub fn new() -> Self {
        Self {
            values: Arc::new(Vec::new()),
        }
    }

    pub fn with_capacity(max_locals: usize) -> Self {
        Self {
            values: Arc::new(vec![None; max_locals]),
        }
    }

    /// Ensure the underlying Vec can hold at least `capacity` elements.
    /// Call this before a series of set() operations to avoid repeated resizes.
    pub fn ensure_capacity(&mut self, capacity: usize) {
        let values = Arc::make_mut(&mut self.values);
        if capacity > values.len() {
            values.resize(capacity, None);
        }
    }

    /// Build a LocalEnv from an iterator of (LocalId, Value) pairs.
    /// This is more efficient than creating an empty LocalEnv and calling set() repeatedly.
    pub fn from_iter(iter: impl Iterator<Item = (LocalId, Value)>) -> Self {
        let pairs: Vec<_> = iter.collect();
        if pairs.is_empty() {
            return Self::new();
        }
        let max_id = pairs.iter().map(|(id, _)| usize::from(*id)).max().unwrap();
        let mut values = vec![None; max_id + 1];
        for (id, value) in pairs {
            values[usize::from(id)] = Some(value);
        }
        Self {
            values: Arc::new(values),
        }
    }

    #[inline]
    pub fn get(&self, id: LocalId) -> &Value {
        let idx = usize::from(id);
        self.values
            .get(idx)
            .and_then(|v| v.as_ref())
            .expect("LocalId should be set before get")
    }

    #[inline]
    pub fn set(&mut self, id: LocalId, value: Value) {
        let idx = usize::from(id);

        // Fast path: if we have unique ownership, we can mutate in place
        // Arc::get_mut returns Some only if strong_count == 1 and weak_count == 0
        if let Some(values) = Arc::get_mut(&mut self.values) {
            if idx >= values.len() {
                // Grow by at least 2x to amortize resize costs
                let new_len = (idx + 1).max(values.len() * 2).max(16);
                values.resize(new_len, None);
            }
            values[idx] = Some(value);
            return;
        }

        // Slow path: need COW
        let values = Arc::make_mut(&mut self.values);
        if idx >= values.len() {
            // Grow by at least 2x to amortize resize costs
            let new_len = (idx + 1).max(values.len() * 2).max(16);
            values.resize(new_len, None);
        }
        values[idx] = Some(value);
    }

    pub fn retain(&mut self, f: impl Fn(LocalId) -> bool) {
        let values = Arc::make_mut(&mut self.values);
        for (i, v) in values.iter_mut().enumerate() {
            if v.is_some() && !f(LocalId::from(i)) {
                *v = None;
            }
        }
    }

    pub fn clear(&mut self) {
        self.values = Arc::new(Vec::new());
    }

    pub fn map_in_place(&mut self, f: impl Fn(Value) -> Value) {
        let values = Arc::make_mut(&mut self.values);
        for v in values.iter_mut() {
            if let Some(val) = v.take() {
                *v = Some(f(val));
            }
        }
    }

    /// Iterate over all (raw_id, value) pairs (only non-None values)
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Value)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|val| (i, val)))
    }

    /// Get the capacity (total slots, including None) of the local env
    #[inline]
    pub fn capacity(&self) -> usize {
        self.values.len()
    }

    /// Get value by raw usize id
    pub fn get_by_raw_id(&self, raw_id: usize) -> &Value {
        self.get(LocalId::from(raw_id))
    }

    /// Check if a raw id has a value set
    #[inline]
    pub fn contains_raw_id(&self, raw_id: usize) -> bool {
        self.values.get(raw_id).map_or(false, |v| v.is_some())
    }

    /// Set value by raw usize id
    pub fn set_by_raw_id(&mut self, raw_id: usize, value: Value) {
        self.set(LocalId::from(raw_id), value);
    }

    /// Make the underlying Vec unique, so subsequent mutations don't need COW.
    /// Call this after cloning if you know you'll be mutating the environment.
    #[inline]
    pub fn make_unique(&mut self) {
        Arc::make_mut(&mut self.values);
    }
}
