use std::collections::HashMap;

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    value::{HeapValue, Value},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub heap: Heap,
    pub local_env: LocalEnv,
    pub outer_local_envs: Vec<LocalEnv>,
    pub global_env: HashMap<String, HeapId>,
    pub prints: Vec<String>,
    pub vector_size: usize,
}

impl State {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            local_env: LocalEnv::new(),
            outer_local_envs: Vec::new(),
            global_env: HashMap::new(),
            prints: Vec::new(),
            vector_size: 1,
        }
    }
}

impl State {
    pub fn map_values_in_place(&mut self, f: impl Fn(Value) -> Value) {
        let f = &f;
        self.heap.map_in_place(|v| match v {
            HeapValue::Value(v) => HeapValue::Value(f(v)),
            HeapValue::Closure(id, values) => {
                HeapValue::Closure(id, values.into_iter().map(f).collect())
            }
            HeapValue::ObjectTable(_)
            | HeapValue::ArrayTable(_)
            | HeapValue::UnknownTable
            | HeapValue::BuiltinFun(_) => v,
        });
        self.local_env.map_in_place(f);
        for env in &mut self.outer_local_envs {
            env.map_in_place(f);
        }
    }
}
