use std::rc::Rc;
use serde::{Deserialize, Serialize};

use crate::ir::LocalId;

use super::value::Value;

/// A local environment storing local variable bindings.
/// Uses Rc<Vec> with copy-on-write for efficient cloning while
/// maintaining O(1) lookup by LocalId (which are contiguous integers).
/// Note: Rc is used instead of Arc since interpretation is single-threaded,
/// which avoids atomic operation overhead.
#[derive(Clone, Debug)]
pub struct LocalEnv {
    // Copy-on-write vector for storing local variables
    data: Rc<Vec<Option<Value>>>,
}

impl PartialEq for LocalEnv {
    fn eq(&self, other: &Self) -> bool {
        // If same Rc, they're equal
        if Rc::ptr_eq(&self.data, &other.data) {
            return true;
        }
        self.data == other.data
    }
}

impl Eq for LocalEnv {}

impl Serialize for LocalEnv {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as Vec of (LocalId, Value) pairs (only non-None values)
        let pairs: Vec<(LocalId, Value)> = self
            .data
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (LocalId::from(i), v.clone())))
            .collect();
        pairs.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for LocalEnv {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let pairs: Vec<(LocalId, Value)> = Vec::deserialize(deserializer)?;
        let max_id = pairs.iter().map(|(id, _)| usize::from(*id)).max();
        let mut vec = vec![None; max_id.map_or(0, |m| m + 1)];
        for (k, v) in pairs {
            vec[usize::from(k)] = Some(v);
        }
        Ok(LocalEnv { data: Rc::new(vec) })
    }
}

impl LocalEnv {
    pub fn new() -> Self {
        Self {
            data: Rc::new(Vec::new()),
        }
    }

    pub fn with_capacity(_max_locals: usize) -> Self {
        Self::new()
    }

    #[inline]
    pub fn get(&self, id: LocalId) -> &Value {
        let idx = usize::from(id);
        self.data
            .get(idx)
            .and_then(|v| v.as_ref())
            .expect("LocalId should be set before get")
    }

    #[inline]
    pub fn set(&mut self, id: LocalId, value: Value) {
        let idx = usize::from(id);

        // Make data unique if needed (copy-on-write)
        let data = Rc::make_mut(&mut self.data);

        // Extend if needed
        if idx >= data.len() {
            data.resize(idx + 1, None);
        }
        data[idx] = Some(value);
    }

    pub fn retain(&mut self, f: impl Fn(LocalId) -> bool) {
        let data = Rc::make_mut(&mut self.data);
        for (i, v) in data.iter_mut().enumerate() {
            if v.is_some() && !f(LocalId::from(i)) {
                *v = None;
            }
        }
    }

    pub fn clear(&mut self) {
        // Just create a new empty Rc, don't modify shared data
        self.data = Rc::new(Vec::new());
    }

    #[inline]
    pub fn map_in_place(&mut self, f: impl Fn(Value) -> Value) {
        let data = Rc::make_mut(&mut self.data);
        for v in data.iter_mut() {
            if let Some(val) = v.take() {
                *v = Some(f(val));
            }
        }
    }

    /// Filter vectors by mask, only transforming values that are vectors.
    /// This is more efficient than map_in_place for filter_vectors operations.
    #[inline]
    pub fn filter_vectors_in_place(&mut self, mask: &[bool], true_count: usize) {
        let data = Rc::make_mut(&mut self.data);
        for v in data.iter_mut() {
            if let Some(val) = v.as_ref() {
                if let Some(new_val) = val.filter_vectors_if_vector(mask, true_count) {
                    *v = Some(new_val);
                }
                // If None returned, value is unchanged - no modification needed
            }
        }
    }

    /// Iterate over all (raw_id, value) pairs
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Value)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
    }

    /// Get value by raw usize id
    #[inline]
    pub fn get_by_raw_id(&self, raw_id: usize) -> &Value {
        self.get(LocalId::from(raw_id))
    }

    /// Set value by raw usize id
    #[inline]
    pub fn set_by_raw_id(&mut self, raw_id: usize, value: Value) {
        self.set(LocalId::from(raw_id), value);
    }
}
