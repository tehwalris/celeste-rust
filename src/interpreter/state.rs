use std::hash::BuildHasherDefault;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use im::HashMap as ImHashMap;
use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    value::{HeapValue, Value},
};
use crate::ir::LocalId;

/// FxHasher-based BuildHasher for im::HashMap
type FxBuildHasher = BuildHasherDefault<FxHasher>;

/// im::HashMap using FxHasher for faster hashing
type FxImHashMap<K, V> = ImHashMap<K, V, FxBuildHasher>;

/// Wrapper for prints Vec with Arc for O(1) cloning and COW semantics.
/// Uses Arc internally for efficient structural sharing.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Prints {
    inner: Arc<Vec<String>>,
}

impl Prints {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Vec::new()),
        }
    }

    pub fn push(&mut self, s: String) {
        Arc::make_mut(&mut self.inner).push(s);
    }

    pub fn as_slice(&self) -> &[String] {
        &self.inner
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl PartialEq for Prints {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner) || *self.inner == *other.inner
    }
}

impl Eq for Prints {}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct State {
    pub heap: Heap,
    pub local_env: LocalEnv,
    pub outer_local_envs: Vec<LocalEnv>,
    pub global_env: FxImHashMap<String, HeapId>,
    pub prints: Prints,
    pub vector_size: usize,
}

impl State {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            local_env: LocalEnv::new(),
            outer_local_envs: Vec::new(),
            global_env: FxImHashMap::default(),
            prints: Prints::new(),
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

    /// Extract a single lane from a vectorized state at the given index.
    /// This is more efficient than filter_by_mask for single-element extraction.
    pub fn extract_scalar_lane(&self, lane_idx: usize) -> Self {
        if self.vector_size == 1 {
            return self.clone();
        }

        // Build new heap with extracted values directly (avoids im::HashMap overhead)
        let heap_values: Vec<Option<HeapValue>> = (0..self.heap.len())
            .map(|i| {
                let id = HeapId::from_raw(i);
                self.heap.get_opt(id).map(|hv| hv.extract_at_index(lane_idx))
            })
            .collect();
        let new_heap = Heap::from_values(heap_values);

        // Build new local_env with extracted values (avoids repeated im::HashMap inserts)
        let new_local_env = LocalEnv::from_iter(
            self.local_env
                .iter()
                .map(|(raw_id, value)| (LocalId::from(raw_id), value.extract_at_index(lane_idx))),
        );

        // Build new outer_local_envs with extracted values
        let new_outer_local_envs: Vec<LocalEnv> = self
            .outer_local_envs
            .iter()
            .map(|env| {
                LocalEnv::from_iter(
                    env.iter()
                        .map(|(raw_id, value)| (LocalId::from(raw_id), value.extract_at_index(lane_idx))),
                )
            })
            .collect();

        State {
            heap: new_heap,
            local_env: new_local_env,
            outer_local_envs: new_outer_local_envs,
            global_env: self.global_env.clone(),
            prints: self.prints.clone(),
            vector_size: 1,
        }
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

            // Get the old value reference and recurse on references
            // Note: map_heap_value_references takes a reference, avoiding clone
            let old_value_ref = old_heap.get(old_id);
            let new_value = map_heap_value_references(old_value_ref, |ref_id| {
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

        // Visit all roots from global_env (sorted for deterministic order)
        let mut global_keys: Vec<_> = self.global_env.keys().cloned().collect();
        global_keys.sort();
        let mut new_global_env = FxImHashMap::default();
        for key in global_keys {
            let old_id = self.global_env[&key];
            let new_id = visit(old_id, &self.heap, &mut old_to_new, &mut new_heap_values);
            new_global_env.insert(key, new_id);
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
