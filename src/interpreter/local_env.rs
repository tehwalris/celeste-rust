use im::HashMap as ImHashMap;
use serde::{Deserialize, Serialize};

use crate::ir::LocalId;

use super::value::Value;

/// A local environment storing local variable bindings.
/// Uses a persistent HashMap for efficient cloning through structural sharing.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalEnv(ImHashMap<LocalId, Value>);

impl LocalEnv {
    pub fn new() -> Self {
        Self(ImHashMap::new())
    }

    pub fn with_capacity(_max_locals: usize) -> Self {
        // im::HashMap doesn't have a capacity hint, but it doesn't need it
        Self::new()
    }

    /// Build a LocalEnv from an iterator of (LocalId, Value) pairs.
    /// This is more efficient than creating an empty LocalEnv and calling set() repeatedly.
    pub fn from_iter(iter: impl Iterator<Item = (LocalId, Value)>) -> Self {
        Self(iter.collect())
    }

    pub fn get(&self, id: LocalId) -> &Value {
        self.0.get(&id).expect("LocalId should be set before get")
    }

    pub fn set(&mut self, id: LocalId, value: Value) {
        self.0.insert(id, value);
    }

    pub fn retain(&mut self, f: impl Fn(LocalId) -> bool) {
        self.0.retain(|id, _| f(*id));
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn map_in_place(&mut self, f: impl Fn(Value) -> Value) {
        // im::HashMap doesn't have a map_in_place, so we need to collect and update
        self.0 = self.0.iter()
            .map(|(k, v)| (*k, f(v.clone())))
            .collect();
    }

    /// Iterate over all (raw_id, value) pairs
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Value)> {
        self.0.iter().map(|(id, v)| (usize::from(*id), v))
    }

    /// Get value by raw usize id
    pub fn get_by_raw_id(&self, raw_id: usize) -> &Value {
        self.get(LocalId::from(raw_id))
    }

    /// Set value by raw usize id
    pub fn set_by_raw_id(&mut self, raw_id: usize, value: Value) {
        self.set(LocalId::from(raw_id), value);
    }
}
