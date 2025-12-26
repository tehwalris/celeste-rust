use im::HashMap as ImHashMap;

use crate::ir::LocalId;

use super::value::Value;

/// A local environment storing local variable bindings.
/// Uses a persistent HashMap for efficient cloning through structural sharing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalEnv(ImHashMap<LocalId, Value>);

impl LocalEnv {
    pub fn new() -> Self {
        Self(ImHashMap::new())
    }

    pub fn with_capacity(_max_locals: usize) -> Self {
        // im::HashMap doesn't have a capacity hint, but it doesn't need it
        Self::new()
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
}
