use crate::ir::LocalId;

use super::value::Value;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalEnv(Vec<Option<Value>>);

impl LocalEnv {
    pub fn new(max_locals: usize) -> Self {
        Self(vec![None; max_locals])
    }

    pub fn get(&self, id: LocalId) -> &Value {
        self.0[usize::from(id)].as_ref().unwrap()
    }

    pub fn set(&mut self, id: LocalId, value: Value) {
        self.0[usize::from(id)] = Some(value);
    }
}
