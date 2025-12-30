use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    tracing::TraceSpan,
    value::{HeapValue, Value},
};
use crate::ir::LocalId;

// Use FxHash for faster hashing
type FxBuildHasher = BuildHasherDefault<FxHasher>;
type FxHashMap<K, V> = std::collections::HashMap<K, V, FxBuildHasher>;

// OrdMap is a sorted map, so iteration is already in sorted order.
// This eliminates the need to sort in shape_of_state.
type ImOrdMap<K, V> = im::OrdMap<K, V>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub heap: Heap,
    pub local_env: LocalEnv,
    pub outer_local_envs: Vec<LocalEnv>,
    pub global_env: ImOrdMap<String, HeapId>,
    pub prints: Vec<String>,
    pub vector_size: usize,
}

impl Serialize for State {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Create a serializable representation
        #[derive(Serialize)]
        struct StateSerialize<'a> {
            heap: &'a Heap,
            local_env: &'a LocalEnv,
            outer_local_envs: &'a Vec<LocalEnv>,
            global_env: Vec<(String, HeapId)>,
            prints: &'a Vec<String>,
            vector_size: usize,
        }

        let global_env: Vec<(String, HeapId)> = self.global_env.iter()
            .map(|(k, v)| (k.clone(), *v))
            .collect();

        StateSerialize {
            heap: &self.heap,
            local_env: &self.local_env,
            outer_local_envs: &self.outer_local_envs,
            global_env,
            prints: &self.prints,
            vector_size: self.vector_size,
        }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for State {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct StateDeserialize {
            heap: Heap,
            local_env: LocalEnv,
            outer_local_envs: Vec<LocalEnv>,
            global_env: Vec<(String, HeapId)>,
            prints: Vec<String>,
            vector_size: usize,
        }

        let s = StateDeserialize::deserialize(deserializer)?;
        let mut global_env = ImOrdMap::new();
        for (k, v) in s.global_env {
            global_env.insert(k, v);
        }

        Ok(State {
            heap: s.heap,
            local_env: s.local_env,
            outer_local_envs: s.outer_local_envs,
            global_env,
            prints: s.prints,
            vector_size: s.vector_size,
        })
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            local_env: LocalEnv::new(),
            outer_local_envs: Vec::new(),
            global_env: ImOrdMap::new(),
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

    /// Filters all vector values in the state by a mask, consuming self.
    /// The resulting state's vector_size will be the number of true values in the mask.
    pub fn filter_by_mask(mut self, mask: &[bool]) -> Self {
        self.filter_by_mask_in_place(mask);
        self
    }

    /// Filters all vector values in the state by a mask in place.
    /// The resulting state's vector_size will be the number of true values in the mask.
    fn filter_by_mask_in_place(&mut self, mask: &[bool]) {
        let _trace = TraceSpan::new("filter_by_mask", "filter");
        use super::value::count_true;
        let new_vector_size = count_true(mask);
        self.vector_size = new_vector_size;

        // Filter values in heap - use optimized method that only clones vectors
        self.heap.filter_vectors_in_place(mask, new_vector_size);

        // Filter values in local env - use optimized method
        self.local_env.filter_vectors_in_place(mask, new_vector_size);

        // Filter values in outer local envs
        for env in &mut self.outer_local_envs {
            env.filter_vectors_in_place(mask, new_vector_size);
        }
    }

    /// Filters all vector values in the state by a mask, cloning first.
    /// The resulting state's vector_size will be the number of true values in the mask.
    pub fn filter_by_mask_clone(&self, mask: &[bool]) -> Self {
        let mut new_state = self.clone();
        new_state.filter_by_mask_in_place(mask);
        new_state
    }

    /// Garbage collect the heap and renumber HeapIds deterministically.
    /// This ensures that states with the same logical structure will have
    /// the same heap IDs, which is critical for vectorization to work correctly.
    ///
    /// The algorithm:
    /// 1. Visit all reachable heap values from global_env, local_env, outer_local_envs
    /// 2. Assign new HeapIds in the order values are visited
    /// 3. Create a compacted heap with only reachable values
    pub fn gc(&mut self) {
        let _trace = TraceSpan::new("gc", "gc");
        let mut old_to_new: FxHashMap<HeapId, HeapId> = FxHashMap::default();
        let mut new_heap_values: Vec<HeapValue> = Vec::new();

        // Visit a heap ID, assigning a new ID if not yet visited
        // Returns the new ID
        fn visit(
            old_id: HeapId,
            old_heap: &Heap,
            old_to_new: &mut FxHashMap<HeapId, HeapId>,
            new_heap_values: &mut Vec<HeapValue>,
        ) -> HeapId {
            if let Some(&new_id) = old_to_new.get(&old_id) {
                return new_id;
            }

            // Assign new ID
            let new_id = HeapId::from_raw(new_heap_values.len());
            old_to_new.insert(old_id, new_id);

            // Placeholder - will be replaced after recursing
            new_heap_values.push(HeapValue::UnknownTable);

            // Get the old value and recurse on references
            let old_value = old_heap.get(old_id).clone();
            let new_value = map_heap_value_references(&old_value, |ref_id| {
                visit(ref_id, old_heap, old_to_new, new_heap_values)
            });

            // Replace placeholder with actual value
            new_heap_values[new_id.raw()] = new_value;

            new_id
        }

        // Map Value references
        fn map_value_references(value: &Value, f: &mut impl FnMut(HeapId) -> HeapId) -> Value {
            match value {
                Value::Pointer(id) => Value::Pointer(f(*id)),
                Value::Number(_)
                | Value::NumberInterval(_)
                | Value::Bool(_)
                | Value::UnknownBool
                | Value::String(_)
                | Value::Nil(_)
                | Value::NilPointer(_) => value.clone(),
            }
        }

        // Map HeapValue references
        fn map_heap_value_references(
            value: &HeapValue,
            mut f: impl FnMut(HeapId) -> HeapId,
        ) -> HeapValue {
            match value {
                HeapValue::Value(v) => HeapValue::Value(map_value_references(v, &mut f)),
                HeapValue::ObjectTable(table) => {
                    // IMPORTANT: Sort keys for deterministic traversal order!
                    let mut keys: Vec<_> = table.keys().cloned().collect();
                    keys.sort();
                    let new_table: FxHashMap<String, HeapId> = keys
                        .into_iter()
                        .map(|k| {
                            let v = table[&k];
                            (k, f(v))
                        })
                        .collect();
                    HeapValue::ObjectTable(new_table)
                }
                HeapValue::ArrayTable(items) => {
                    HeapValue::ArrayTable(items.iter().map(|id| f(*id)).collect())
                }
                HeapValue::UnknownTable => HeapValue::UnknownTable,
                HeapValue::Closure(id, captures) => {
                    let new_captures: Vec<Value> = captures
                        .iter()
                        .map(|v| map_value_references(v, &mut f))
                        .collect();
                    HeapValue::Closure(id.clone(), new_captures)
                }
                HeapValue::BuiltinFun(name) => HeapValue::BuiltinFun(name.clone()),
            }
        }

        // Visit all roots from global_env (OrdMap is already sorted)
        let mut new_global_env = ImOrdMap::new();
        for (key, &old_id) in self.global_env.iter() {
            let new_id = visit(old_id, &self.heap, &mut old_to_new, &mut new_heap_values);
            new_global_env.insert(key.clone(), new_id);
        }

        // Visit all roots from local_env (sorted for deterministic order)
        let mut local_entries: Vec<_> = self.local_env.iter().collect();
        local_entries.sort_by_key(|(k, _)| *k);
        let mut new_local_env = LocalEnv::new();
        for (raw_id, value) in local_entries {
            let new_value = map_value_references(value, &mut |id| {
                visit(id, &self.heap, &mut old_to_new, &mut new_heap_values)
            });
            new_local_env.set(LocalId::from(raw_id), new_value);
        }

        // Visit all roots from outer_local_envs
        let mut new_outer_local_envs = Vec::new();
        for env in &self.outer_local_envs {
            let mut entries: Vec<_> = env.iter().collect();
            entries.sort_by_key(|(k, _)| *k);
            let mut new_env = LocalEnv::new();
            for (raw_id, value) in entries {
                let new_value = map_value_references(value, &mut |id| {
                    visit(id, &self.heap, &mut old_to_new, &mut new_heap_values)
                });
                new_env.set(LocalId::from(raw_id), new_value);
            }
            new_outer_local_envs.push(new_env);
        }

        // Build the new compacted heap
        let mut new_heap = Heap::new();
        for value in new_heap_values {
            let id = new_heap.alloc();
            new_heap.set(id, value);
        }

        // Update state
        self.heap = new_heap;
        self.global_env = new_global_env;
        self.local_env = new_local_env;
        self.outer_local_envs = new_outer_local_envs;
    }
}
