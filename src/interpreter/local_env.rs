use crate::ir::LocalId;

use super::value::Value;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalEnv(Vec<Option<Value>>);

impl LocalEnv {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(max_locals: usize) -> Self {
        Self(vec![None; max_locals])
    }

    pub fn get(&self, id: LocalId) -> &Value {
        self.0[usize::from(id)].as_ref().unwrap()
    }

    pub fn set(&mut self, id: LocalId, value: Value) {
        let idx = usize::from(id);
        if idx >= self.0.len() {
            self.0.resize(idx + 1, None);
        }
        self.0[idx] = Some(value);
    }

    pub fn retain(&mut self, f: impl Fn(LocalId) -> bool) {
        for (i, value) in self.0.iter_mut().enumerate() {
            if value.is_some() && !f(LocalId::from(i)) {
                *value = None;
            }
        }
    }

    pub fn clear(&mut self) {
        self.retain(|_| false);
    }

    pub fn map_in_place(&mut self, f: impl Fn(Value) -> Value) {
        for value in self.0.iter_mut() {
            let old_value = value.take();
            if let Some(old_value) = old_value {
                *value = Some(f(old_value));
            }
        }
    }
}
