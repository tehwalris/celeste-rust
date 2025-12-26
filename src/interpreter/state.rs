use im::HashMap as ImHashMap;

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
    pub global_env: ImHashMap<String, HeapId>,
    pub prints: Vec<String>,
    pub vector_size: usize,
}

impl State {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            local_env: LocalEnv::new(),
            outer_local_envs: Vec::new(),
            global_env: ImHashMap::new(),
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

    /// Filters all vector values in the state by a mask.
    /// The resulting state's vector_size will be the number of true values in the mask.
    pub fn filter_by_mask(&self, mask: &[bool]) -> Self {
        let new_vector_size = mask.iter().filter(|&&b| b).count();

        let mut new_state = self.clone();
        new_state.vector_size = new_vector_size;

        // Filter values in heap
        new_state.heap.map_in_place(|v| match v {
            HeapValue::Value(val) => HeapValue::Value(val.filter_vectors(mask)),
            HeapValue::Closure(id, values) => {
                HeapValue::Closure(id, values.into_iter().map(|v| v.filter_vectors(mask)).collect())
            }
            HeapValue::ObjectTable(_)
            | HeapValue::ArrayTable(_)
            | HeapValue::UnknownTable
            | HeapValue::BuiltinFun(_) => v,
        });

        // Filter values in local env
        new_state.local_env.map_in_place(|v| v.filter_vectors(mask));

        // Filter values in outer local envs
        for env in &mut new_state.outer_local_envs {
            env.map_in_place(|v| v.filter_vectors(mask));
        }

        new_state
    }
}
