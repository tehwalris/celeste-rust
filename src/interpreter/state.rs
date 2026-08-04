use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    tracing::TraceSpan,
    value::{HeapValue, MaybeVector, Value},
};

// Use FxHash for faster hashing
type FxBuildHasher = BuildHasherDefault<FxHasher>;
type FxHashMap<K, V> = std::collections::HashMap<K, V, FxBuildHasher>;

// OrdMap is a sorted map, so iteration is already in sorted order.
// This eliminates the need to sort in shape_of_state.
type ImOrdMap<K, V> = im::OrdMap<K, V>;

/// Why a `filter_by_mask` happened. Used as the trace span name so the three
/// very different causes can be told apart in a profile.
pub type FilterReason = &'static str;

/// A conditional branch whose condition differs across lanes: the state is
/// split so each edge sees only its own lanes. Pure overhead - this is what
/// making the program branch-free is meant to eliminate.
pub const FILTER_BRANCH: FilterReason = "filter_branch";

/// `__split_by_flr` refining an interval into integer-floor classes.
/// Semantically necessary; this is the search fanning out, not overhead.
pub const FILTER_SPLIT_FLR: FilterReason = "filter_split_flr";

/// Dropping duplicate lanes during vectorization. Useful work.
pub const FILTER_DEDUP: FilterReason = "filter_dedup";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub heap: Heap,
    pub local_env: LocalEnv,
    pub outer_local_envs: Vec<LocalEnv>,
    pub global_env: ImOrdMap<String, HeapId>,
    pub prints: Vec<String>,
    /// Length of stored vectors (changes when vectors are filtered by branching or GC)
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

impl Default for State {
    fn default() -> Self {
        Self::new()
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
    ///
    /// `reason` names the call site and becomes the trace span name. Filtering is
    /// a large share of runtime, and the three reasons have very different
    /// meanings: `FILTER_BRANCH` is overhead we intend to eliminate by making
    /// the program branch-free, whereas `FILTER_SPLIT_FLR` is semantically
    /// necessary interval refinement. Keeping them apart in traces is the only
    /// way to size that prize. See plans/rewrite-plan.md.
    pub fn filter_by_mask(mut self, mask: &[bool], reason: FilterReason) -> Self {
        self.filter_by_mask_in_place(mask, reason);
        self
    }

    /// Filters all vector values in the state by a mask in place.
    /// The resulting state's vector_size will be the number of true values in the mask.
    fn filter_by_mask_in_place(&mut self, mask: &[bool], reason: FilterReason) {
        let _trace = TraceSpan::new(reason, "filter");
        let t_census = crate::op_census::start();

        // The mask is scanned once here; every vector below gathers the
        // kept lanes directly, O(kept) per vector instead of O(mask).
        let kept: Vec<u32> = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &m)| m.then_some(i as u32))
            .collect();

        // Filter values in heap - use optimized method that only clones vectors
        self.heap.filter_vectors_in_place(&kept);

        // Filter values in local env - use optimized method
        self.local_env.filter_vectors_in_place(&kept);

        // Filter values in outer local envs
        for env in &mut self.outer_local_envs {
            env.filter_vectors_in_place(&kept);
        }

        self.vector_size = kept.len();

        if reason == FILTER_BRANCH {
            crate::op_census::record_branch_filter(mask.len(), kept.len(), t_census);
        }
        if crate::op_census::enabled() {
            let count_env = |env: &LocalEnv| {
                env.iter().filter(|(_, v)| is_vector_value(v)).count()
            };
            crate::op_census::record_filter_heap_cells(self.heap.len());
            crate::op_census::record_filter_columns(
                (0..self.heap.len())
                    .filter_map(|i| self.heap.get_opt(HeapId::from_raw(i)))
                    .filter(|hv| match hv {
                        HeapValue::Value(v) => is_vector_value(v),
                        HeapValue::Closure(_, caps) => caps.iter().any(is_vector_value),
                        _ => false,
                    })
                    .count(),
                count_env(&self.local_env),
                self.outer_local_envs.iter().map(count_env).sum(),
            );
        }
        let reason_index = crate::op_census::REASON_NAMES
            .iter()
            .position(|name| *name == reason)
            .expect("every filter reason must be in the census table");
        crate::op_census::record_filter_reason(reason_index, kept.len(), t_census);
    }

    /// Duplicates every lane of the state: lanes `[l1..lN]` become
    /// `[l1..lN, l1..lN]` and `vector_size` doubles. Scalars broadcast over
    /// any lane count and stay scalar, so only vectors are touched.
    ///
    /// The engine behind `Instruction::Expand`, which follows this with a
    /// per-lane bool that is `true` on the first copy and `false` on the
    /// second - together they enumerate both values of an unknown bool
    /// without splitting the state.
    pub fn expand_lanes(&mut self) {
        let _trace = TraceSpan::new("expand_lanes", "expand");
        let t = crate::op_census::start();
        fn double<T: std::fmt::Debug + Clone + PartialEq + Eq>(
            v: MaybeVector<T>,
        ) -> MaybeVector<T> {
            match v {
                MaybeVector::Scalar(s) => MaybeVector::Scalar(s),
                MaybeVector::Vector(mut arc) => {
                    // Copies only if another state still shares the lanes.
                    std::sync::Arc::make_mut(&mut arc).extend_from_within(..);
                    MaybeVector::Vector(arc)
                }
            }
        }
        self.map_values_in_place(|value| match value {
            Value::Number(v) => Value::Number(double(v)),
            Value::NumberInterval(v) => Value::NumberInterval(double(v)),
            Value::Bool(v) => Value::Bool(double(v)),
            other @ (Value::UnknownBool
            | Value::String(_)
            | Value::Nil(_)
            | Value::Pointer(_)
            | Value::NilPointer(_)) => other,
        });
        self.vector_size *= 2;
        crate::op_census::record(crate::op_census::Cat::Expand, self.vector_size, 0, t);
    }

    /// Filters all vector values in the state by a mask, cloning first.
    /// The resulting state's vector_size will be the number of true values in the mask.
    pub fn filter_by_mask_clone(&self, mask: &[bool], reason: FilterReason) -> Self {
        let mut new_state = self.clone();
        new_state.filter_by_mask_in_place(mask, reason);
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
    ///
    /// Uninitialized cells are preserved as uninitialized. `Instruction::Alloc`
    /// creates a slot with no `HeapValue` in it, and nothing stores to it until
    /// the corresponding `Store` runs - so at most program points there are live
    /// pointers to cells that have no value yet (`local x` with no initializer
    /// never gets one at all). Such a cell has no observable content, but its
    /// identity matters: a later `Store` through that pointer must still work.
    pub fn gc(&mut self) {
        let t_census = crate::op_census::start();
        let before = self.heap.len();
        self.gc_inner(t_census);
        crate::op_census::record_gc_cells(before, self.heap.len());
    }

    // (census guard defined at module scope below)

    fn gc_inner(&mut self, t_census: Option<std::time::Instant>) {
        let _defer = CensusGcGuard(t_census);
        let _trace = TraceSpan::new("gc", "gc");

        let mut old_to_new: FxHashMap<HeapId, HeapId> = FxHashMap::default();
        // `None` means the slot is allocated but holds no value. Also used as
        // the placeholder while recursing, so cycles terminate.
        let mut new_heap_values: Vec<Option<HeapValue>> = Vec::new();

        // Visit a heap ID, assigning a new ID if not yet visited
        // Returns the new ID
        fn visit(
            old_id: HeapId,
            old_heap: &Heap,
            old_to_new: &mut FxHashMap<HeapId, HeapId>,
            new_heap_values: &mut Vec<Option<HeapValue>>,
        ) -> HeapId {
            if let Some(&new_id) = old_to_new.get(&old_id) {
                return new_id;
            }

            // Assign new ID
            let new_id = HeapId::from_raw(new_heap_values.len());
            old_to_new.insert(old_id, new_id);

            // Placeholder - will be replaced after recursing
            new_heap_values.push(None);

            // An allocated-but-unset cell stays unset in the compacted heap.
            let Some(old_value) = old_heap.get_opt(old_id) else {
                return new_id;
            };

            // Get the old value and recurse on references
            // Note: We must clone from heap (FrozenVec doesn't support taking ownership)
            let new_value = map_heap_value(old_value.clone(), |ref_id| {
                visit(ref_id, old_heap, old_to_new, new_heap_values)
            });

            // Replace placeholder with actual value
            new_heap_values[new_id.raw()] = Some(new_value);

            new_id
        }

        // Map Value references - takes ownership, returns unchanged if no HeapIds
        #[inline]
        fn map_value(value: Value, f: &mut impl FnMut(HeapId) -> HeapId) -> Value {
            match value {
                Value::Pointer(id) => Value::Pointer(f(id)),
                // These don't contain HeapIds, pass through unchanged (no clone!)
                v @ (Value::Number(_)
                | Value::NumberInterval(_)
                | Value::Bool(_)
                | Value::UnknownBool
                | Value::String(_)
                | Value::Nil(_)
                | Value::NilPointer(_)) => v,
            }
        }

        // Map HeapValue references - takes ownership
        fn map_heap_value(
            value: HeapValue,
            mut f: impl FnMut(HeapId) -> HeapId,
        ) -> HeapValue {
            match value {
                HeapValue::Value(v) => HeapValue::Value(map_value(v, &mut f)),
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
                    HeapValue::ArrayTable(items.into_iter().map(|id| f(id)).collect())
                }
                HeapValue::UnknownTable => HeapValue::UnknownTable,
                HeapValue::Closure(id, captures) => {
                    let new_captures: Vec<Value> = captures
                        .into_iter()
                        .map(|v| map_value(v, &mut f))
                        .collect();
                    HeapValue::Closure(id, new_captures)
                }
                HeapValue::BuiltinFun(name) => HeapValue::BuiltinFun(name),
            }
        }

        // Visit all roots from global_env (OrdMap is already sorted)
        let mut new_global_env = ImOrdMap::new();
        for (key, &old_id) in self.global_env.iter() {
            let new_id = visit(old_id, &self.heap, &mut old_to_new, &mut new_heap_values);
            new_global_env.insert(key.clone(), new_id);
        }

        // Visit all roots from local_env (sorted for deterministic order)
        // Note: We must clone values since LocalEnv doesn't support draining
        // `iter` yields *slots*, so the rebuild must be positional and must
        // carry the occupant across. Using `set(LocalId::from(slot), ..)` would
        // map the slot through the slot table a second time.
        let mut local_entries: Vec<_> = self.local_env.iter().collect();
        local_entries.sort_by_key(|(k, _)| *k);
        let mut new_local_env = self.local_env.empty_like();
        for (slot, value) in local_entries {
            let new_value = map_value(value.clone(), &mut |id| {
                visit(id, &self.heap, &mut old_to_new, &mut new_heap_values)
            });
            let occupant = self.local_env.occupant_of_slot(slot);
            new_local_env.set_slot(slot, occupant, new_value);
        }

        // Visit all roots from outer_local_envs
        let mut new_outer_local_envs = Vec::new();
        for env in &self.outer_local_envs {
            let mut entries: Vec<_> = env.iter().collect();
            entries.sort_by_key(|(k, _)| *k);
            let mut new_env = env.empty_like();
            for (slot, value) in entries {
                let new_value = map_value(value.clone(), &mut |id| {
                    visit(id, &self.heap, &mut old_to_new, &mut new_heap_values)
                });
                let occupant = env.occupant_of_slot(slot);
                new_env.set_slot(slot, occupant, new_value);
            }
            new_outer_local_envs.push(new_env);
        }

        // Build the new compacted heap
        let mut new_heap = Heap::new();
        for value in new_heap_values {
            let id = new_heap.alloc();
            if let Some(value) = value {
                new_heap.set(id, value);
            }
        }

        // Update state
        self.heap = new_heap;
        self.global_env = new_global_env;
        self.local_env = new_local_env;
        self.outer_local_envs = new_outer_local_envs;
    }
}

#[cfg(test)]
mod send_tests {
    use super::*;
    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    #[test]
    fn test_state_is_send() {
        assert_send::<State>();
        assert_sync::<State>();
    }
}


/// Records GC time to the op census even on early returns.
struct CensusGcGuard(Option<std::time::Instant>);
impl Drop for CensusGcGuard {
    fn drop(&mut self) {
        crate::op_census::record(crate::op_census::Cat::Gc, 0, 0, self.0.take());
    }
}

/// A value the filter has to gather, as opposed to one it can leave alone.
fn is_vector_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Number(MaybeVector::Vector(_))
            | Value::NumberInterval(MaybeVector::Vector(_))
            | Value::Bool(MaybeVector::Vector(_))
    )
}
