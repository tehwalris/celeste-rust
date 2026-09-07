use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use super::{
    heap::{Heap, HeapId},
    value::{HeapValue, Value},
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

/// Frontier-only search: dropping lanes whose canonical row was already
/// reached at an earlier frame (their successors are reachable earlier via
/// the same input suffix, so re-expanding them finds nothing new).
pub const FILTER_VISITED: FilterReason = "filter_visited";

/// Lane-chunking of oversized states before a frame: pure mechanics, no
/// semantic filtering - the chunks re-merge at the boundary.
pub const FILTER_CHUNK: FilterReason = "filter_chunk";

/// Partitioning a MIXED interval comparison: the lanes with a definite
/// answer keep it, the lanes that straddle are carried off into their own
/// state where `UnknownBool` is honest. See `partition_maybe_bool`. Not
/// overhead and not the search fanning out - it is precision being kept
/// that the old whole-value collapse threw away.
pub const FILTER_STRADDLE: FilterReason = "filter_straddle";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub heap: Heap,
    pub global_env: ImOrdMap<String, HeapId>,
    pub prints: Vec<String>,
    /// Length of stored vectors (changes when vectors are filtered by branching or GC)
    pub vector_size: usize,
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
            global_env: ImOrdMap::new(),
            prints: Vec::new(),
            vector_size: 1,
        }
    }

    fn filter_by_mask_in_place(&mut self, mask: &[bool], reason: FilterReason) {
        // The mask is scanned once here; every vector below gathers the
        // kept lanes directly, O(kept) per vector instead of O(mask), and
        // range-at-a-time (see `KeptLanes`).
        let kept = super::value::KeptLanes::from_mask(mask);
        self.filter_by_kept_in_place(&kept, Some(mask.len()), reason);
    }

    /// The one filter body. `lanes_before` is only needed by the branch
    /// census, which is charged per *input* lane; callers that came from a
    /// run list (the frontier subtract, lane chunking) pass `None` because
    /// they are never `FILTER_BRANCH`.
    fn filter_by_kept_in_place(
        &mut self,
        kept: &super::value::KeptLanes,
        lanes_before: Option<usize>,
        reason: FilterReason,
    ) {

        self.heap.filter_vectors_in_place(kept);
        self.vector_size = kept.len();

        if reason == FILTER_BRANCH {
            let _before = lanes_before.expect("a branch filter always comes from a mask");
        }
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
    /// 1. Visit all reachable heap values from global_env
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
        self.gc_inner();
    }

    fn gc_inner(&mut self) {
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
                | Value::MaybeBool(_)
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


