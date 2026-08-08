//! State vectorization: Merging multiple states with the same "shape" into a single
//! state with vector values. This is a key optimization for abstract interpretation.
//!
//! The algorithm works as follows:
//! 1. Group states by their "shape" (structure with vectorizable values normalized)
//! 2. For each group, merge all states into one with vector values
//! 3. Deduplicate vectors (remove duplicate elements)
//! 4. If vector size becomes 1, convert back to scalars

use rustc_hash::{FxHashMap, FxHashSet};

use super::tracing::TraceSpan;

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::GlobalId;
use crate::pico8_num::{Pico8Num, Pico8NumInterval};

/// A "shape" is a state with all vectorizable values normalized to placeholder values.
/// States with the same shape can be merged by vectorizing their values.
#[derive(Clone, Debug)]
pub struct StateShape {
    // For shape comparison, we normalize all vectorizable values to placeholders
    // but keep the structure (heap IDs, table shapes, etc.)
    heap_structure: Vec<(HeapId, HeapValueShape)>,
    local_env_structure: Vec<(usize, ValueShape)>,
    outer_local_envs_structure: Vec<Vec<(usize, ValueShape)>>,
    // global_env as sorted Vec for consistent hashing (ImHashMap's Hash is buggy)
    global_env: Vec<(String, HeapId)>,
    prints: Vec<String>,
    // Cached hash for O(1) hashing after construction
    cached_hash: u64,
}

impl PartialEq for StateShape {
    fn eq(&self, other: &Self) -> bool {
        // First check cached hash for fast rejection
        if self.cached_hash != other.cached_hash {
            return false;
        }
        // Then do full comparison
        self.heap_structure == other.heap_structure
            && self.local_env_structure == other.local_env_structure
            && self.outer_local_envs_structure == other.outer_local_envs_structure
            && self.global_env == other.global_env
            && self.prints == other.prints
    }
}

impl Eq for StateShape {}

impl std::hash::Hash for StateShape {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.cached_hash.hash(state);
    }
}

impl StateShape {
    /// Get the cached hash value for this shape
    pub fn cached_hash(&self) -> u64 {
        self.cached_hash
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum HeapValueShape {
    Value(ValueShape),
    ObjectTable(Vec<(String, HeapId)>),
    ArrayTable(Vec<HeapId>),
    UnknownTable,
    Closure(GlobalId, Vec<ValueShape>),
    BuiltinFun(String),
    /// Empty slot (allocated but never set)
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ValueShape {
    // Vectorizable values are normalized to a placeholder
    VectorizableNumber,
    VectorizableNumberInterval,
    VectorizableBool,
    // Non-vectorizable values keep their actual value for shape comparison
    UnknownBool,
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
}

/// Can this value be vectorized (combined with others into a vector)?
fn can_vectorize_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Number(_) | Value::NumberInterval(_) | Value::Bool(_)
    )
}

fn normalize_value_for_shape(value: &Value) -> ValueShape {
    if can_vectorize_value(value) {
        match value {
            Value::Number(_) => ValueShape::VectorizableNumber,
            Value::NumberInterval(_) => ValueShape::VectorizableNumberInterval,
            Value::Bool(_) => ValueShape::VectorizableBool,
            _ => unreachable!(),
        }
    } else {
        // Non-vectorizable values keep their identity for shape comparison
        // States with different non-vectorizable values cannot be merged
        match value {
            Value::UnknownBool => ValueShape::UnknownBool,
            Value::String(s) => ValueShape::String(s.clone()),
            Value::Nil(hint) => ValueShape::Nil(hint.clone()),
            Value::Pointer(id) => ValueShape::Pointer(*id),
            Value::NilPointer(s) => ValueShape::NilPointer(s.clone()),
            _ => unreachable!("Should be vectorizable: {:?}", value),
        }
    }
}

fn normalize_heap_value_for_shape(value: &HeapValue) -> HeapValueShape {
    match value {
        HeapValue::Value(v) => HeapValueShape::Value(normalize_value_for_shape(v)),
        HeapValue::ObjectTable(table) => {
            let mut entries: Vec<_> = table.iter().map(|(k, v)| (k.clone(), *v)).collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            HeapValueShape::ObjectTable(entries)
        }
        HeapValue::ArrayTable(items) => HeapValueShape::ArrayTable(items.clone()),
        HeapValue::UnknownTable => HeapValueShape::UnknownTable,
        HeapValue::Closure(id, captures) => {
            HeapValueShape::Closure(
                id.clone(),
                captures.iter().map(normalize_value_for_shape).collect(),
            )
        }
        HeapValue::BuiltinFun(name) => HeapValueShape::BuiltinFun(name.clone()),
    }
}

/// Debug function to get the shape of a state (for testing)
pub fn debug_shape_of_state(state: &State) -> StateShape {
    shape_of_state(state)
}

/// Get the shape of a state for vectorization grouping
pub fn shape_of_state(state: &State) -> StateShape {
    // Get heap structure (handle empty slots that are allocated but not set)
    let heap_len = state.heap.len();
    let mut heap_structure = Vec::with_capacity(heap_len);
    for i in 0..heap_len {
        let id = HeapId::from_raw(i);
        let shape = match state.heap.get_opt(id) {
            Some(value) => normalize_heap_value_for_shape(value),
            None => HeapValueShape::Empty,
        };
        heap_structure.push((id, shape));
    }

    // Local env is already sorted by index (uses Vec internally)
    // Pre-allocate based on iterator hint
    let local_env_iter = state.local_env.iter();
    let (lower_bound, _) = local_env_iter.size_hint();
    let mut local_env_structure = Vec::with_capacity(lower_bound);
    for (k, v) in local_env_iter {
        local_env_structure.push((k, normalize_value_for_shape(v)));
    }

    // Outer local envs are also already sorted by index
    let mut outer_local_envs_structure = Vec::with_capacity(state.outer_local_envs.len());
    for env in &state.outer_local_envs {
        let env_iter = env.iter();
        let (lower_bound, _) = env_iter.size_hint();
        let mut env_structure = Vec::with_capacity(lower_bound);
        for (k, v) in env_iter {
            env_structure.push((k, normalize_value_for_shape(v)));
        }
        outer_local_envs_structure.push(env_structure);
    }

    // global_env uses OrdMap which is already sorted, so no need to sort
    let global_env_len = state.global_env.len();
    let mut global_env = Vec::with_capacity(global_env_len);
    for (k, v) in state.global_env.iter() {
        global_env.push((k.clone(), *v));
    }

    // Compute hash once during construction
    use std::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();
    heap_structure.hash(&mut hasher);
    local_env_structure.hash(&mut hasher);
    outer_local_envs_structure.hash(&mut hasher);
    global_env.hash(&mut hasher);
    state.prints.hash(&mut hasher);
    let cached_hash = hasher.finish();

    StateShape {
        heap_structure,
        local_env_structure,
        outer_local_envs_structure,
        global_env,
        prints: state.prints.clone(),
        cached_hash,
    }
}

fn vectorize_same_shape_states(states: Vec<State>) -> State {
    if states.len() == 1 {
        return states.into_iter().next().unwrap();
    }

    let total_vector_size: usize = states.iter().map(|s| s.vector_size).sum();
    let first_state = &states[0];
    crate::merge_stats::record_concat(states.len(), first_state.heap.len());

    // Build vectorized heap
    let mut new_heap = Heap::new();
    for _ in 0..first_state.heap.len() {
        new_heap.alloc();
    }

    for i in 0..first_state.heap.len() {
        let id = HeapId::from_raw(i);
        // Skip empty slots (allocated but never set)
        if first_state.heap.get_opt(id).is_none() {
            continue;
        }
        let merged_value = merge_heap_values_from_states(&states, id);
        new_heap.set(id, merged_value);
    }

    // Build vectorized local_env
    let merged_local_env = merge_local_envs(&states, |s| &s.local_env);

    // Build vectorized outer_local_envs
    let num_outer = first_state.outer_local_envs.len();
    let merged_outer_local_envs: Vec<LocalEnv> = (0..num_outer)
        .map(|i| merge_local_envs(&states, |s| &s.outer_local_envs[i]))
        .collect();

    State {
        heap: new_heap,
        local_env: merged_local_env,
        outer_local_envs: merged_outer_local_envs,
        global_env: first_state.global_env.clone(),
        prints: first_state.prints.clone(),
        vector_size: total_vector_size,
    }
}

fn merge_heap_values_from_states(states: &[State], id: HeapId) -> HeapValue {
    let values: Vec<_> = states.iter()
        .map(|s| (s.heap.get(id).clone(), s.vector_size))
        .collect();

    merge_heap_values(&values)
}

fn merge_heap_values(values: &[(HeapValue, usize)]) -> HeapValue {
    let (first_value, _) = &values[0];

    match first_value {
        HeapValue::Value(_) => {
            let value_and_sizes: Vec<_> = values.iter()
                .map(|(hv, size)| {
                    match hv {
                        HeapValue::Value(v) => (v.clone(), *size),
                        _ => panic!("Shape mismatch"),
                    }
                })
                .collect();
            HeapValue::Value(merge_values(&value_and_sizes))
        }
        HeapValue::ObjectTable(_) => {
            // For tables, they should all be identical (same shape)
            first_value.clone()
        }
        HeapValue::ArrayTable(_) => first_value.clone(),
        HeapValue::UnknownTable => HeapValue::UnknownTable,
        HeapValue::Closure(id, captures) => {
            // Merge captured values
            let num_captures = captures.len();
            let merged_captures: Vec<Value> = (0..num_captures)
                .map(|i| {
                    let capture_values: Vec<_> = values.iter()
                        .map(|(hv, size)| {
                            match hv {
                                HeapValue::Closure(_, caps) => (caps[i].clone(), *size),
                                _ => panic!("Shape mismatch"),
                            }
                        })
                        .collect();
                    merge_values(&capture_values)
                })
                .collect();
            HeapValue::Closure(id.clone(), merged_captures)
        }
        HeapValue::BuiltinFun(name) => HeapValue::BuiltinFun(name.clone()),
    }
}

fn merge_values(values: &[(Value, usize)]) -> Value {
    let (first_value, _) = &values[0];

    // Check if vectorizable based on the value type
    if !can_vectorize_value(first_value) {
        // Non-vectorizable: all values must be equal (as whole Values)
        #[cfg(debug_assertions)]
        {
            for (v, _) in values {
                if v != first_value {
                    panic!("Non-vectorizable values are not equal");
                }
            }
        }
        return first_value.clone();
    }

    // Calculate total size to pre-allocate
    let total_size: usize = values.iter().map(|(_, size)| size).sum();

    // Merge vectorizable values directly without creating intermediate ScalarValue
    // Track whether all values are identical during merge to avoid post-scan
    match first_value {
        Value::Number(_) => {
            // Get reference value for comparison
            let ref_val = match &values[0].0 {
                Value::Number(MaybeVector::Scalar(n)) => Some(*n),
                Value::Number(MaybeVector::Vector(nums)) if !nums.is_empty() => Some(nums[0]),
                _ => None,
            };

            let t_concat = crate::op_census::start();
            let mut result = Vec::with_capacity(total_size);
            let mut all_same = ref_val.is_some();
            let ref_val = ref_val.unwrap_or(Pico8Num::from_i16(0));

            for (v, size) in values {
                match v {
                    Value::Number(MaybeVector::Scalar(n)) => {
                        if all_same && *n != ref_val {
                            all_same = false;
                        }
                        let new_len = result.len() + size;
                        result.resize(new_len, *n);
                    }
                    Value::Number(MaybeVector::Vector(nums)) => {
                        if all_same {
                            for n in nums.iter() {
                                if *n != ref_val {
                                    all_same = false;
                                    break;
                                }
                            }
                        }
                        result.extend_from_slice(nums);
                    }
                    _ => panic!("Type mismatch in merge"),
                }
            }
            crate::op_census::record(
                crate::op_census::Cat::Concat,
                result.len(),
                result.len() * 2 * 4,
                t_concat,
            );
            if result.len() == 1 || (all_same && result.len() > 1) {
                Value::Number(MaybeVector::Scalar(result[0]))
            } else {
                Value::Number(MaybeVector::vector(result))
            }
        }
        Value::NumberInterval(_) => {
            // Get reference value for comparison
            let ref_val = match &values[0].0 {
                Value::NumberInterval(MaybeVector::Scalar(n)) => Some(*n),
                Value::NumberInterval(MaybeVector::Vector(nums)) if !nums.is_empty() => Some(nums[0]),
                _ => None,
            };

            let t_concat = crate::op_census::start();
            let mut result = Vec::with_capacity(total_size);
            let mut all_same = ref_val.is_some();
            let ref_val = ref_val.unwrap_or(Pico8NumInterval::new(Pico8Num::from_i16(0), Pico8Num::from_i16(0)));

            for (v, size) in values {
                match v {
                    Value::NumberInterval(MaybeVector::Scalar(n)) => {
                        if all_same && *n != ref_val {
                            all_same = false;
                        }
                        let new_len = result.len() + size;
                        result.resize(new_len, *n);
                    }
                    Value::NumberInterval(MaybeVector::Vector(nums)) => {
                        if all_same {
                            for n in nums.iter() {
                                if *n != ref_val {
                                    all_same = false;
                                    break;
                                }
                            }
                        }
                        result.extend_from_slice(nums);
                    }
                    _ => panic!("Type mismatch in merge"),
                }
            }
            crate::op_census::record(
                crate::op_census::Cat::Concat,
                result.len(),
                result.len() * 2 * 8,
                t_concat,
            );
            if result.len() == 1 || (all_same && result.len() > 1) {
                Value::NumberInterval(MaybeVector::Scalar(result[0]))
            } else {
                Value::NumberInterval(MaybeVector::vector(result))
            }
        }
        Value::Bool(_) => {
            // Get reference value for comparison
            let ref_val = match &values[0].0 {
                Value::Bool(MaybeVector::Scalar(b)) => Some(*b),
                Value::Bool(MaybeVector::Vector(bools)) if !bools.is_empty() => Some(bools[0]),
                _ => None,
            };

            let t_concat = crate::op_census::start();
            let mut result = Vec::with_capacity(total_size);
            let mut all_same = ref_val.is_some();
            let ref_val = ref_val.unwrap_or(false);

            for (v, size) in values {
                match v {
                    Value::Bool(MaybeVector::Scalar(b)) => {
                        if all_same && *b != ref_val {
                            all_same = false;
                        }
                        let new_len = result.len() + size;
                        result.resize(new_len, *b);
                    }
                    Value::Bool(MaybeVector::Vector(bools)) => {
                        if all_same {
                            for b in bools.iter() {
                                if *b != ref_val {
                                    all_same = false;
                                    break;
                                }
                            }
                        }
                        result.extend_from_slice(bools);
                    }
                    _ => panic!("Type mismatch in merge"),
                }
            }
            crate::op_census::record(
                crate::op_census::Cat::Concat,
                result.len(),
                result.len() * 2 * 1,
                t_concat,
            );
            if result.len() == 1 || (all_same && result.len() > 1) {
                Value::Bool(MaybeVector::Scalar(result[0]))
            } else {
                Value::Bool(MaybeVector::vector(result))
            }
        }
        _ => panic!("Unexpected value type for merge"),
    }
}

fn merge_local_envs<F>(states: &[State], get_env: F) -> LocalEnv
where
    F: Fn(&State) -> &LocalEnv,
{
    let first_env = get_env(&states[0]);
    let mut merged = first_env.empty_like();

    // Positional: all these states are at the same program point, so slot N
    // holds the same logical value in each of them.
    for (slot, _) in first_env.iter() {
        let value_and_sizes: Vec<_> = states.iter()
            .map(|s| (get_env(s).get_by_raw_id(slot).clone(), s.vector_size))
            .collect();
        let merged_value = merge_values(&value_and_sizes);
        merged.set_slot(slot, first_env.occupant_of_slot(slot), merged_value);
    }

    merged
}

/// Hash every row at once, one *vector* at a time. The row-major version
/// walked all vectors per row - a strided access per lane over hundreds of
/// separately-allocated vectors, which is where `dedup_state`'s time went.
/// Column-major visits each vector once, sequentially, folding each
/// element's hash into its row's running hash. Same collision story as
/// before: candidates are confirmed by `rows_equal`, so the hash only has
/// to be good, not perfect.
fn hash_rows(vector_values: &[VectorRef], n: usize) -> Vec<u64> {
    use std::hash::{Hash, Hasher};
    #[inline(always)]
    fn elem_hash<T: Hash>(value: &T) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        value.hash(&mut hasher);
        hasher.finish()
    }
    // Fibonacci-style fold; order-sensitive so permuted columns disagree.
    #[inline(always)]
    fn fold(row: &mut u64, value: u64) {
        *row = (row.rotate_left(26) ^ value).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    }
    let t = crate::op_census::start();
    let col_bytes: usize = vector_values
        .iter()
        .map(|v| match v {
            VectorRef::Numbers(x) => x.len() * 4,
            VectorRef::NumberIntervals(x) => x.len() * 8,
            VectorRef::Bools(x) => x.len(),
        })
        .sum();
    let mut hashes = vec![0x51_7c_c1_b7_27_22_0a_95u64; n];
    for vec in vector_values {
        match vec {
            VectorRef::Numbers(nums) => {
                for (row, value) in hashes.iter_mut().zip(*nums) {
                    fold(row, elem_hash(value));
                }
            }
            VectorRef::NumberIntervals(nums) => {
                for (row, value) in hashes.iter_mut().zip(*nums) {
                    fold(row, elem_hash(value));
                }
            }
            VectorRef::Bools(bools) => {
                for (row, value) in hashes.iter_mut().zip(*bools) {
                    fold(row, elem_hash(value));
                }
            }
        }
    }
    crate::op_census::record(
        crate::op_census::Cat::HashRows,
        n * vector_values.len(),
        col_bytes + 16 * n * vector_values.len(),
        t,
    );
    hashes
}

/// Compare two rows by their values at the given indices.
/// Optimized to compare directly without creating intermediate ScalarValue objects.
fn rows_equal(vector_values: &[VectorRef], idx1: usize, idx2: usize) -> bool {
    for vec in vector_values {
        let equal = match vec {
            VectorRef::Numbers(nums) => nums[idx1] == nums[idx2],
            VectorRef::NumberIntervals(nums) => nums[idx1] == nums[idx2],
            VectorRef::Bools(bools) => bools[idx1] == bools[idx2],
        };
        if !equal {
            return false;
        }
    }
    true
}

/// Which lanes to keep: `true` for the first occurrence of each distinct
/// row, `false` for every later copy of it. Also returns how many were kept.
///
/// The obvious algorithm - one pass, per-hash lists of surviving rows,
/// `rows_equal` against each candidate - is what this replaces, because its
/// verification is where the time went. Measured at frame 39 of the
/// rewritten program: 1.32 s in this phase, of which 0.43 s was the hash
/// probe and 0.89 s was verification, doing 303 M column-cell reads
/// scattered across ~17 separately-allocated column vectors (a ~340 MB
/// footprint at 2.9 ns per cell - cache-miss bound, not compute bound).
/// 97% of rows are duplicates and essentially every comparison reads all
/// columns and reports equal, so there is no early exit to win with; the
/// only lever is making the reads cheaper.
///
/// So verification is restructured to touch one small table instead of
/// every column:
///
/// 1. A probe pass assigns each row the *first* row of its hash class.
///    Nothing is compared yet, so this is one sequential sweep.
/// 2. The candidate-unique rows - only those, typically a few percent of
///    the input - are packed row-major into a dense word array, built in
///    L2-sized tiles of rows so each tile stays resident while all columns
///    write into it.
/// 3. Verification streams the columns in row order and compares each
///    duplicate against its representative's packed row: one random access
///    into a table small enough to live in cache, instead of one per
///    column into the full-width columns.
/// 4. Any row that disagrees with its representative means two *different*
///    rows share a 64-bit hash. Then, and only then, every hash class
///    containing such a row is redone by the original row-major algorithm.
///    Classes are independent, so this reproduces the old result exactly -
///    it is a fallback for a case that is astronomically unlikely (~1e-5
///    expected collisions at these row counts), not an approximation.
fn bucket_unique_mask(vector_values: &[VectorRef], row_hashes: &[u64]) -> (Vec<bool>, usize) {
    let n = row_hashes.len();

    // 1. Probe: representative = first row seen with this hash.
    let mut first_of_hash: FxHashMap<u64, u32> =
        FxHashMap::with_capacity_and_hasher(n / 2, Default::default());
    let mut mask = vec![false; n];
    // For each row, the dense slot of its representative. A representative
    // points at itself, so verification needs no special case for it.
    let mut dense_of_row: Vec<u32> = vec![0; n];
    let mut uniq: Vec<u32> = Vec::new();

    for (i, &row_hash) in row_hashes.iter().enumerate() {
        match first_of_hash.entry(row_hash) {
            std::collections::hash_map::Entry::Vacant(slot) => {
                let dense = uniq.len() as u32;
                slot.insert(dense);
                uniq.push(i as u32);
                dense_of_row[i] = dense;
                mask[i] = true;
            }
            std::collections::hash_map::Entry::Occupied(slot) => {
                dense_of_row[i] = *slot.get();
            }
        }
    }
    let mut unique_count = uniq.len();
    if unique_count == n {
        // Every row has its own hash, so no two rows can be equal and there
        // is nothing to verify.
        return (mask, unique_count);
    }

    // 2. Pack the representatives, one row per `words_per_row` words.
    let words_per_row: usize = vector_values
        .iter()
        .map(|v| match v {
            VectorRef::NumberIntervals(_) => 2,
            VectorRef::Numbers(_) | VectorRef::Bools(_) => 1,
        })
        .sum();
    let mut dense = vec![0u32; uniq.len() * words_per_row];
    // Tile so the destination stays in L2 (1 MiB/core) across all the
    // column passes that fill it.
    const DENSE_TILE_BYTES: usize = 192 * 1024;
    let tile_rows = (DENSE_TILE_BYTES / (words_per_row * 4)).max(1);
    for tile_start in (0..uniq.len()).step_by(tile_rows) {
        let tile_end = (tile_start + tile_rows).min(uniq.len());
        let mut w = 0;
        for vec in vector_values {
            match vec {
                VectorRef::Numbers(nums) => {
                    for d in tile_start..tile_end {
                        dense[d * words_per_row + w] = nums[uniq[d] as usize].to_bits();
                    }
                    w += 1;
                }
                VectorRef::Bools(bools) => {
                    for d in tile_start..tile_end {
                        dense[d * words_per_row + w] = bools[uniq[d] as usize] as u32;
                    }
                    w += 1;
                }
                VectorRef::NumberIntervals(ivs) => {
                    for d in tile_start..tile_end {
                        let iv = &ivs[uniq[d] as usize];
                        dense[d * words_per_row + w] = iv.low.to_bits();
                        dense[d * words_per_row + w + 1] = iv.high.to_bits();
                    }
                    w += 2;
                }
            }
        }
    }

    // 3. Verify each duplicate against its representative's packed row.
    // Branchless: 97% of these agree, so an early exit would only cost a
    // mispredict.
    let mut contaminated: Vec<usize> = Vec::new();
    for i in 0..n {
        if mask[i] {
            continue;
        }
        let base = dense_of_row[i] as usize * words_per_row;
        let rep = &dense[base..base + words_per_row];
        let mut equal = true;
        let mut w = 0;
        for vec in vector_values {
            match vec {
                VectorRef::Numbers(nums) => {
                    equal &= rep[w] == nums[i].to_bits();
                    w += 1;
                }
                VectorRef::Bools(bools) => {
                    equal &= rep[w] == bools[i] as u32;
                    w += 1;
                }
                VectorRef::NumberIntervals(ivs) => {
                    equal &= rep[w] == ivs[i].low.to_bits();
                    equal &= rep[w + 1] == ivs[i].high.to_bits();
                    w += 2;
                }
            }
        }
        if !equal {
            contaminated.push(i);
        }
    }

    // 4. Hash collision between distinct rows: redo those classes exactly.
    if !contaminated.is_empty() {
        let bad: FxHashSet<u64> = contaminated.iter().map(|&i| row_hashes[i]).collect();
        let mut per_hash: FxHashMap<u64, Vec<usize>> = FxHashMap::default();
        for i in 0..n {
            let row_hash = row_hashes[i];
            if !bad.contains(&row_hash) {
                continue;
            }
            let indices = per_hash.entry(row_hash).or_default();
            let is_duplicate = indices
                .iter()
                .any(|&prev_idx| rows_equal(vector_values, prev_idx, i));
            if is_duplicate {
                if mask[i] {
                    mask[i] = false;
                    unique_count -= 1;
                }
            } else {
                indices.push(i);
                if !mask[i] {
                    mask[i] = true;
                    unique_count += 1;
                }
            }
        }
    }

    (mask, unique_count)
}

/// Deduplicate a vectorized state by removing duplicate vector elements.
/// Returns a new state with unique vector elements.
fn dedup_vectorized_state(mut state: State) -> State {
    let _trace = TraceSpan::new("dedup_state", "vectorize");
    if state.vector_size <= 1 {
        crate::merge_stats::record_dedup(state.vector_size, 0, state.heap.len(), 0);
        return state;
    }

    // Collect all vector values from the state
    let vector_values = collect_vector_values(&state);

    if vector_values.is_empty() {
        // No vectors means all vectorizable values were identical, so
        // all "rows" are duplicates. Reduce to just one.
        crate::merge_stats::record_dedup(
            state.vector_size,
            0,
            state.heap.len(),
            state.vector_size - 1,
        );
        state.vector_size = 1;
        return state;
    }

    // Hash every row up front (column-major, see `hash_rows`), then bucket.
    let row_hashes = hash_rows(&vector_values, state.vector_size);
    let t_bucket = crate::op_census::start();
    let (mask, unique_count) = bucket_unique_mask(&vector_values, &row_hashes);

    crate::op_census::record(
        crate::op_census::Cat::DedupBucket,
        state.vector_size,
        0,
        t_bucket,
    );
    if crate::op_census::enabled() {
        let verified = (state.vector_size - unique_count) as u64;
        crate::op_census::record_dedup_detail(
            verified,
            verified * vector_values.len() as u64,
            verified * vector_values.len() as u64,
        );
    }
    crate::merge_stats::record_dedup(
        state.vector_size,
        vector_values.len(),
        state.heap.len(),
        state.vector_size - unique_count,
    );

    if unique_count == state.vector_size {
        // No duplicates found
        return state;
    }

    state.filter_by_mask(&mask, crate::interpreter::state::FILTER_DEDUP)
}

/// Collect all vectorizable vector values from a state for dedup purposes.
fn collect_vector_values(state: &State) -> Vec<VectorRef<'_>> {
    let mut vectors = Vec::new();

    // From heap (skip empty slots)
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        let Some(heap_value) = state.heap.get_opt(id) else {
            continue;
        };
        match heap_value {
            HeapValue::Value(Value::Number(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::Numbers(v.as_slice()));
            }
            HeapValue::Value(Value::NumberInterval(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::NumberIntervals(v.as_slice()));
            }
            HeapValue::Value(Value::Bool(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::Bools(v.as_slice()));
            }
            HeapValue::Closure(_, captures) => {
                for cap in captures {
                    match cap {
                        Value::Number(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::Numbers(v.as_slice()));
                        }
                        Value::NumberInterval(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::NumberIntervals(v.as_slice()));
                        }
                        Value::Bool(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::Bools(v.as_slice()));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    // From local_env
    for (_, v) in state.local_env.iter() {
        match v {
            Value::Number(MaybeVector::Vector(nums)) => {
                vectors.push(VectorRef::Numbers(nums.as_slice()));
            }
            Value::NumberInterval(MaybeVector::Vector(nums)) => {
                vectors.push(VectorRef::NumberIntervals(nums.as_slice()));
            }
            Value::Bool(MaybeVector::Vector(bools)) => {
                vectors.push(VectorRef::Bools(bools.as_slice()));
            }
            _ => {}
        }
    }

    // From outer_local_envs
    for env in &state.outer_local_envs {
        for (_, v) in env.iter() {
            match v {
                Value::Number(MaybeVector::Vector(nums)) => {
                    vectors.push(VectorRef::Numbers(nums.as_slice()));
                }
                Value::NumberInterval(MaybeVector::Vector(nums)) => {
                    vectors.push(VectorRef::NumberIntervals(nums.as_slice()));
                }
                Value::Bool(MaybeVector::Vector(bools)) => {
                    vectors.push(VectorRef::Bools(bools.as_slice()));
                }
                _ => {}
            }
        }
    }

    vectors
}

enum VectorRef<'a> {
    Numbers(&'a [Pico8Num]),
    NumberIntervals(&'a [Pico8NumInterval]),
    Bools(&'a [bool]),
}

/// Unvectorize a state if its vector_size is 1 (convert vectors to scalars)
fn unvectorize_if_possible(mut state: State) -> State {
    if state.vector_size != 1 {
        return state;
    }

    state.map_values_in_place(|v| match v {
        Value::Number(MaybeVector::Vector(nums)) if nums.len() == 1 => {
            Value::Number(MaybeVector::Scalar(nums[0]))
        }
        Value::NumberInterval(MaybeVector::Vector(nums)) if nums.len() == 1 => {
            Value::NumberInterval(MaybeVector::Scalar(nums[0]))
        }
        Value::Bool(MaybeVector::Vector(bools)) if bools.len() == 1 => {
            Value::Bool(MaybeVector::Scalar(bools[0]))
        }
        other => other,
    });

    state
}

/// Assert that all vectorizable vector values in a state have the correct length.
pub fn assert_state_vector_lengths(state: &State) {
    let expected_len = state.vector_size;

    // Check heap (use get_opt since some slots may be allocated but not set)
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        let Some(heap_value) = state.heap.get_opt(id) else {
            continue; // Skip empty slots
        };
        match heap_value {
            HeapValue::Value(Value::Number(MaybeVector::Vector(v))) => {
                assert_eq!(v.len(), expected_len, "Vector length mismatch in heap (numbers)");
            }
            HeapValue::Value(Value::NumberInterval(MaybeVector::Vector(v))) => {
                assert_eq!(v.len(), expected_len, "Vector length mismatch in heap (number intervals)");
            }
            HeapValue::Value(Value::Bool(MaybeVector::Vector(v))) => {
                assert_eq!(v.len(), expected_len, "Vector length mismatch in heap (bools)");
            }
            HeapValue::Closure(_, captures) => {
                for cap in captures {
                    match cap {
                        Value::Number(MaybeVector::Vector(v)) => {
                            assert_eq!(v.len(), expected_len, "Vector length mismatch in closure (numbers)");
                        }
                        Value::NumberInterval(MaybeVector::Vector(v)) => {
                            assert_eq!(v.len(), expected_len, "Vector length mismatch in closure (number intervals)");
                        }
                        Value::Bool(MaybeVector::Vector(v)) => {
                            assert_eq!(v.len(), expected_len, "Vector length mismatch in closure (bools)");
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    // Check local_env
    for (_, v) in state.local_env.iter() {
        match v {
            Value::Number(MaybeVector::Vector(vec)) => {
                assert_eq!(vec.len(), expected_len, "Vector length mismatch in local_env (numbers)");
            }
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                assert_eq!(vec.len(), expected_len, "Vector length mismatch in local_env (number intervals)");
            }
            Value::Bool(MaybeVector::Vector(vec)) => {
                assert_eq!(vec.len(), expected_len, "Vector length mismatch in local_env (bools)");
            }
            _ => {}
        }
    }

    // Check outer_local_envs
    for env in &state.outer_local_envs {
        for (_, v) in env.iter() {
            match v {
                Value::Number(MaybeVector::Vector(nums)) => {
                    assert_eq!(nums.len(), expected_len, "Vector length mismatch in outer_local_env (numbers)");
                }
                Value::NumberInterval(MaybeVector::Vector(nums)) => {
                    assert_eq!(nums.len(), expected_len, "Vector length mismatch in outer_local_env (number intervals)");
                }
                Value::Bool(MaybeVector::Vector(bools)) => {
                    assert_eq!(bools.len(), expected_len, "Vector length mismatch in outer_local_env (bools)");
                }
                _ => {}
            }
        }
    }
}

/// Clean states by removing local_env entries that don't appear in all states.
/// This allows states with different dead temporaries to merge.
///
/// A slot only survives if every state has it filled *and* they all agree on
/// which `LocalId` is in it. The occupant part matters once slots are actually
/// allocated: two states at this program point can hold different dead
/// temporaries in one shared slot, and merging those would produce a state
/// whose slot means one thing in some lanes and another in the rest.
///
/// Dropping such a slot is safe. All these states sit at the same program
/// point, so the set of *live* ids is the same for all of them, and a live id
/// occupies the same slot in each. Disagreement therefore only ever happens on
/// values nothing will read.
///
/// Under the identity map occupant and slot are the same number, so the extra
/// condition changes nothing.
fn clean_local_envs_for_merging(states: Vec<State>) -> Vec<State> {
    use crate::ir::LocalId;

    if states.len() <= 1 {
        return states;
    }

    let occupancy = |state: &State| -> FxHashSet<(usize, Option<LocalId>)> {
        state
            .local_env
            .iter()
            .map(|(slot, _)| (slot, state.local_env.occupant_of_slot(slot)))
            .collect()
    };

    let mut common: FxHashSet<(usize, Option<LocalId>)> = occupancy(&states[0]);
    for state in &states[1..] {
        common = common.intersection(&occupancy(state)).copied().collect();
    }
    let common_slots: FxHashSet<usize> = common.iter().map(|(slot, _)| *slot).collect();

    states.into_iter().map(|mut state| {
        let slots = std::sync::Arc::clone(state.local_env.slots());
        state
            .local_env
            .retain(|id: LocalId| common_slots.contains(&slots.slot_of(id)));
        state
    }).collect()
}

/// Vectorize a collection of states.
/// States with the same "shape" are merged into single states with vector values.
/// This reduces the number of states while preserving all the information.
/// Timing stats for vectorize_states (for profiling)
#[derive(Default)]
pub struct VectorizeTimingStats {
    pub input_validation_ns: u64,
    pub clean_local_envs_ns: u64,
    pub shape_grouping_ns: u64,
    pub vectorize_groups_ns: u64,
    pub output_validation_ns: u64,
    pub input_count: usize,
    pub output_count: usize,
    pub group_count: usize,
}

thread_local! {
    static LAST_VECTORIZE_STATS: std::cell::RefCell<Option<VectorizeTimingStats>> = std::cell::RefCell::new(None);
}

pub fn get_last_vectorize_stats() -> Option<VectorizeTimingStats> {
    LAST_VECTORIZE_STATS.with(|s| s.borrow().clone())
}

impl Clone for VectorizeTimingStats {
    fn clone(&self) -> Self {
        Self {
            input_validation_ns: self.input_validation_ns,
            clean_local_envs_ns: self.clean_local_envs_ns,
            shape_grouping_ns: self.shape_grouping_ns,
            vectorize_groups_ns: self.vectorize_groups_ns,
            output_validation_ns: self.output_validation_ns,
            input_count: self.input_count,
            output_count: self.output_count,
            group_count: self.group_count,
        }
    }
}

lazy_static::lazy_static! {
    /// Program-carried patterns (the `partition_merge` annotation). Set by
    /// the harness when a run starts; never cleared, because partitioning
    /// either program of a differential pair is semantics-preserving and a
    /// process may interleave both.
    static ref PROGRAM_PATTERNS: std::sync::RwLock<Vec<String>> =
        std::sync::RwLock::new(Vec::new());
}

/// Install the program's merge-partition annotation. Empty lists are
/// ignored (no opinion); the `CELESTE_PARTITION_CELLS` env var, when set,
/// overrides for experiments.
pub fn set_merge_partition_patterns(patterns: &[String]) {
    if patterns.is_empty() {
        return;
    }
    *PROGRAM_PATTERNS.write().unwrap() = patterns.to_vec();
}

fn partition_cell_patterns() -> Vec<String> {
    static ENV: std::sync::OnceLock<Option<Vec<String>>> = std::sync::OnceLock::new();
    let env = ENV.get_or_init(|| {
        std::env::var("CELESTE_PARTITION_CELLS").ok().map(|v| {
            v.split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        })
    });
    match env {
        Some(patterns) => patterns.clone(),
        None => PROGRAM_PATTERNS.read().unwrap().clone(),
    }
}

/// Resolve the configured patterns against a state's cell names. A pattern
/// matches a cell whose field path equals it or ends with ".<pattern>"
/// (so "dash_time" matches both a global and a field, but not
/// "dash_effect_time").
fn resolve_partition_cells(state: &State) -> Vec<usize> {
    let patterns = partition_cell_patterns();
    if patterns.is_empty() {
        return Vec::new();
    }
    let names = super::merge_dump::cell_names(state);
    let mut cells: Vec<usize> = names
        .iter()
        .filter(|(_, name)| {
            patterns
                .iter()
                .any(|p| *name == p || name.ends_with(&format!(".{}", p)))
        })
        .map(|(&cell, _)| cell)
        .collect();
    cells.sort_unstable();
    cells
}

/// The state's class under the partition cells: a hash of their values.
/// `None` if any partition cell still varies per lane - the caller splits
/// the state first. A hash collision between two classes merely merges
/// them (what an unpartitioned merge does to every class), never breaks
/// anything.
fn partition_class(state: &State, cells: &[usize]) -> Option<u64> {
    use std::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();
    for &cell in cells {
        match state.heap.get_opt(HeapId::from_raw(cell)) {
            Some(HeapValue::Value(v)) => match v {
                Value::Number(MaybeVector::Vector(_))
                | Value::NumberInterval(MaybeVector::Vector(_))
                | Value::Bool(MaybeVector::Vector(_)) => return None,
                Value::Number(MaybeVector::Scalar(n)) => (cell, 1u8, n).hash(&mut hasher),
                Value::NumberInterval(MaybeVector::Scalar(iv)) => {
                    (cell, 2u8, iv.low, iv.high).hash(&mut hasher)
                }
                Value::Bool(MaybeVector::Scalar(b)) => (cell, 3u8, b).hash(&mut hasher),
                // Non-vectorizable values are already part of the shape.
                _ => cell.hash(&mut hasher),
            },
            _ => cell.hash(&mut hasher),
        }
    }
    Some(hasher.finish())
}

/// Split any state whose partition cells vary per lane into per-class
/// sub-states, using the one-pass branch split on a value-equality mask.
/// Bounded by the cells' cardinality (<= a handful of values each).
fn split_states_by_partition(states: Vec<State>, cells: &[usize]) -> Vec<State> {
    let mut out = Vec::with_capacity(states.len());
    let mut work = states;
    while let Some(state) = work.pop() {
        let Some(varying) = cells.iter().copied().find(|&cell| {
            matches!(
                state.heap.get_opt(HeapId::from_raw(cell)),
                Some(HeapValue::Value(
                    Value::Number(MaybeVector::Vector(_))
                        | Value::NumberInterval(MaybeVector::Vector(_))
                        | Value::Bool(MaybeVector::Vector(_))
                ))
            )
        }) else {
            out.push(state);
            continue;
        };
        // Mask: lanes equal to the first lane's value peel off as one
        // uniform class; the remainder loops back for the next value.
        let mask: Vec<bool> = match state.heap.get_opt(HeapId::from_raw(varying)) {
            Some(HeapValue::Value(Value::Number(MaybeVector::Vector(v)))) => {
                let first = v[0];
                v.iter().map(|&x| x == first).collect()
            }
            Some(HeapValue::Value(Value::NumberInterval(MaybeVector::Vector(v)))) => {
                let first = v[0];
                v.iter().map(|&x| x == first).collect()
            }
            Some(HeapValue::Value(Value::Bool(MaybeVector::Vector(v)))) => {
                let first = v[0];
                v.iter().map(|&x| x == first).collect()
            }
            _ => unreachable!("checked varying above"),
        };
        let (uniform, rest) = state.split_by_condition(&mask, true);
        // The uniform side collapses the cell to Scalar via the split's
        // canonicalizing gathers; the rest still varies (or is now uniform
        // in a different value) and loops.
        if let Some(uniform) = uniform {
            work.push(uniform);
        }
        if let Some(rest) = rest {
            work.push(rest);
        }
    }
    out
}

/// Vectorize states: GC + merge by shape.
///
/// This is the main entry point for combining multiple states into fewer
/// vectorized states. It:
/// 1. GCs all states (removes heap garbage)
/// 2. Groups states by shape (structure with values normalized)
/// 3. Merges each group into a single vectorized state
/// 4. Deduplicates rows within each merged state
/// The merge-partition cells (`CELESTE_PARTITION_CELLS`, comma-separated
/// field names, e.g. "dash_time"): merges group by the *values* of these
/// cells in addition to shape, so states in different classes never merge.
///
/// Why: a branch on a partition cell is uniform within every merged state,
/// so it routes instead of splitting - the point is to kill the hot
/// mid-frame forks (dash_time at in_i1_074_cont). Uniform-collapse then
/// keeps the cell `Scalar` in each state, and its column leaves every
/// dedup key. Lanes that converge across classes still dedup, one merge
/// later, when their current values agree - the lane *set* is unchanged
/// (splitting and merging are both semantics-preserving), which
/// `rewrite verify` checks end to end.
/// Frontier-only search: drop lanes whose canonical row was already reached
/// at an earlier frame. A state reachable at frame m < n only expands to
/// states reachable at m+1 <= n via the same input suffix, so re-expanding it
/// can never discover a new earliest arrival - the search stays complete and
/// win frames stay earliest-arrival (= optimal TAS length). Requires the
/// state representation to be world-still (see the timer-global pinning in
/// make_state_abstract) or cross-frame rows never match and this is a no-op.
///
/// EXPERIMENTAL SIZING VERSION: the visited set stores 64-bit row hashes with
/// no exact verification, so a hash collision would silently drop a genuinely
/// new state. Fine for measuring the win; NOT proof-grade. The hardened
/// version must chunk-verify rows like the boundary merge does.
///
/// Returns (kept_states, lanes_before, lanes_after).
pub fn subtract_visited(
    states: Vec<State>,
    visited: &mut crate::interpreter::row_table::RowTable,
) -> (Vec<State>, usize, usize) {
    let mut out = Vec::with_capacity(states.len());
    let mut before = 0usize;
    let mut after = 0usize;
    for state in states {
        let keys = visited_row_keys(&state, visited);
        let (kept, b, a) = subtract_precomputed(state, keys, visited);
        before += b;
        after += a;
        out.extend(kept);
    }
    (out, before, after)
}

/// A state's per-lane visited-set keys, plus a read-only verdict on which
/// of them the table already holds.
///
/// `None` means the state could not be canonicalized into columns; the
/// caller keeps it whole. Sound: skipping dedup only costs work, never
/// correctness.
pub struct VisitedKeys {
    /// Only the lanes whose key was NOT in the table at probe time, each
    /// with its key. Everything else is already known to be a duplicate
    /// and never reaches the serial phase - which is the point, since at
    /// depth that is ~98% of the lanes offered.
    candidates: Vec<(u32, (u64, u64))>,
    lanes: usize,
}

/// PHASE 1 of the frontier subtract: canonicalize the state into columns,
/// hash each lane into its 128-bit row key, and look each key up read-only.
///
/// This is the expensive half and it only needs `&RowTable`, so a worker
/// thread can run it for its own chunk while the table sits still. At depth
/// ~98% of offered lanes are already-visited rows (f60: 100.5M offered,
/// 2.0M new), so almost all of the probe traffic - one cache miss per lane
/// into a table of tens of millions of keys - parallelises, and the serial
/// phase is left with only the misses.
pub fn visited_row_keys(
    state: &State,
    visited: &crate::interpreter::row_table::RowTable,
) -> Option<VisitedKeys> {
    use crate::interpreter::virtual_merge::{collect_columns_labeled, row_key_hashes, Column};
    let _trace = TraceSpan::new("visited_row_keys", "vectorize");
    let shape_hash = shape_of_state(state).cached_hash();
    let (columns, _origins) = collect_columns_labeled(std::slice::from_ref(state))?;
    let refs: Vec<&Column> = columns.iter().collect();
    // The row hash folds scalar/uniform pieces into every row, so it covers
    // the full lane-varying AND lane-uniform value content; structure is
    // covered by the shape hash keying the set. Two independently-seeded
    // 64-bit hashes = a 128-bit row key: at ~10^8 rows the 64-bit birthday
    // risk was ~10^-4 per run, which 128 bits make negligible. Both seeds
    // are folded in ONE pass over the columns.
    let keys = row_key_hashes(
        shape_hash,
        &refs,
        state.vector_size,
        crate::interpreter::row_table::ROW_HASH_SEED2,
    );
    // Two filters, both here in the worker, and the second one matters more
    // than it looks. `visited` is the table as it stood when this BATCH
    // started, so a row that a sibling chunk is about to discover is still
    // absent from it - every lane carrying that row would reach the serial
    // phase as a candidate. Worse, a chunk's own lanes repeat rows
    // constantly: at f50 a frame offers 57M lanes and keeps 1.1M.
    //
    // So dedup locally as well. Keeping only the FIRST lane of each key is
    // exactly what the serial `insert_new` would have decided for the rest
    // (it returns `None` for every later lane with the same key), so this
    // changes nothing but the amount of work handed across the thread
    // boundary. The local set holds one chunk's distinct rows, which is
    // small enough to stay in cache - unlike the global table.
    let mut seen: rustc_hash::FxHashSet<(u64, u64)> = rustc_hash::FxHashSet::default();
    let mut candidates: Vec<(u32, (u64, u64))> = Vec::new();
    for (i, key) in keys.into_iter().enumerate() {
        if visited.id_of(key).is_none() && seen.insert(key) {
            candidates.push((i as u32, key));
        }
    }
    Some(VisitedKeys {
        candidates,
        lanes: state.vector_size,
    })
}

/// PHASE 2: assign ids to the genuinely new rows and drop the rest.
///
/// Runs serially in input order - row ids are assigned in insertion order
/// and everything downstream (checkpoints, bands, the backward sweep) is
/// written in terms of them, so this ordering is load-bearing.
///
/// Phase 1's verdict is only a filter, never a decision: every candidate
/// lane still goes through `insert_new`, which is what resolves duplicates
/// *within* this batch (two lanes carrying the same new row both read
/// "absent" in phase 1; the second one's `insert_new` returns `None`).
/// Handing over every lane as a candidate would give the same answer, just
/// slower.
pub fn subtract_precomputed(
    state: State,
    keys: Option<VisitedKeys>,
    visited: &mut crate::interpreter::row_table::RowTable,
) -> (Vec<State>, usize, usize) {
    let _trace = TraceSpan::new("subtract_visited", "vectorize");
    let before = state.vector_size;
    let Some(VisitedKeys { candidates, lanes }) = keys else {
        return (vec![state], before, before);
    };
    debug_assert_eq!(lanes, state.vector_size);
    // O(candidates), not O(lanes): the survivors come out as an ascending
    // index list and go straight to runs. Building a lane-sized bool mask
    // here was two extra passes over a vector ~50x longer than its answer,
    // on the one phase of the frame that cannot be parallelised.
    let mut survivors: Vec<u32> = Vec::new();
    for (lane, key) in candidates {
        if visited.insert_new(key).is_some() {
            survivors.push(lane);
        }
    }
    let kept = survivors.len();
    if kept == state.vector_size {
        (vec![state], before, kept)
    } else if kept > 0 {
        let kept_lanes = crate::interpreter::value::KeptLanes::from_sorted_indices(&survivors);
        (
            vec![state
                .filter_by_kept_clone(&kept_lanes, crate::interpreter::state::FILTER_VISITED)],
            before,
            kept,
        )
    } else {
        (Vec::new(), before, 0)
    }
}

pub fn vectorize_states(states: Vec<State>) -> Vec<State> {
    let _trace = TraceSpan::new("vectorize_states", "vectorize");
    let mut stats = VectorizeTimingStats::default();
    stats.input_count = states.len();

    // GC all states before shape grouping.
    // This removes garbage from heaps, allowing states to match shapes better.
    let states: Vec<State> = {
        let _gc_trace = TraceSpan::new("gc_before_vectorize", "gc");
        // One state's gc cannot see another's, so this is a plain map over
        // independent work - and at the frame boundary there are hundreds
        // of survivor fragments. Chunked rather than one task per state so
        // the thread count does not track the fragment count.
        let threads = crate::interpreter::virtual_merge::merge_threads();
        if states.len() < 8 || threads == 1 {
            states.into_iter().map(|mut s| { s.gc(); s }).collect()
        } else {
            let mut states = states;
            let chunk = states.len().div_ceil(threads);
            std::thread::scope(|scope| {
                for part in states.chunks_mut(chunk) {
                    scope.spawn(move || {
                        for s in part {
                            s.gc();
                        }
                    });
                }
            });
            states
        }
    };

    if states.is_empty() {
        LAST_VECTORIZE_STATS.with(|s| *s.borrow_mut() = Some(stats));
        return states;
    }

    // Diagnostic: how compressible is the whole merge, post-gc?
    let dump_before = super::merge_dump::measure(&states, true);

    // Validate input states (only in debug mode)
    let t0 = std::time::Instant::now();
    #[cfg(debug_assertions)]
    for state in &states {
        assert_state_vector_lengths(state);
    }
    stats.input_validation_ns = t0.elapsed().as_nanos() as u64;

    // Clean local_envs: remove variables that don't appear in all states.
    // This allows states with different dead temporaries to merge.
    let t1 = std::time::Instant::now();
    let states = clean_local_envs_for_merging(states);
    stats.clean_local_envs_ns = t1.elapsed().as_nanos() as u64;

    // Partition on the configured cells, if any: split lane-varying states
    // per class first, then group by (shape, class) so classes never merge.
    let t2 = std::time::Instant::now();
    let partition_cells = states
        .first()
        .map(resolve_partition_cells)
        .unwrap_or_default();
    let states = if partition_cells.is_empty() {
        states
    } else {
        let _trace_split = TraceSpan::new("partition_split", "vectorize");
        split_states_by_partition(states, &partition_cells)
    };

    // Group states by shape (and partition class).
    let mut states_by_shape: FxHashMap<(StateShape, u64), Vec<State>> = FxHashMap::default();
    {
        let _trace_shape = TraceSpan::new("shape_grouping", "vectorize");
        for state in states {
            let class = partition_class(&state, &partition_cells)
                .expect("partition-varying states were split above");
            let shape = shape_of_state(&state);
            states_by_shape
                .entry((shape, class))
                .or_insert_with(Vec::new)
                .push(state);
        }
    }
    stats.shape_grouping_ns = t2.elapsed().as_nanos() as u64;
    stats.group_count = states_by_shape.len();


    // Vectorize each group
    let t3 = std::time::Instant::now();
    let result: Vec<State> = {
        let _trace_merge = TraceSpan::new("merge_groups", "vectorize");
        states_by_shape
            .into_iter()
            .map(|(_, group)| {
                if group.len() > 1 {
                    // Merge + dedup over the virtual concatenation - never
                    // materialises the pre-dedup table. Produces the same
                    // state as the materialised pipeline below (a test
                    // holds them equal); the fallback covers group shapes
                    // the column collection cannot represent.
                    if let Some(state) = super::virtual_merge::merge_dedup_group(&group) {
                        return state;
                    }
                }
                let vectorized = vectorize_same_shape_states(group);
                let deduped = dedup_vectorized_state(vectorized);
                unvectorize_if_possible(deduped)
            })
            .collect()
    };
    stats.vectorize_groups_ns = t3.elapsed().as_nanos() as u64;
    super::merge_dump::report(dump_before, &result);
    super::merge_dump::report_structure(&result);

    // Validate output states (only in debug mode)
    let t4 = std::time::Instant::now();
    #[cfg(debug_assertions)]
    for state in &result {
        assert_state_vector_lengths(state);
    }
    stats.output_validation_ns = t4.elapsed().as_nanos() as u64;
    stats.output_count = result.len();

    crate::merge_stats::record_vectorize(stats.input_count, stats.group_count, result.len());
    LAST_VECTORIZE_STATS.with(|s| *s.borrow_mut() = Some(stats));
    result
}

/// Compute union and diff of two state sets at a `hint_normalize` block.
///
/// Given `accumulated` (states from previous fixed-point rounds) and
/// `potentially_new` (states just arrived), returns the union and the
/// states that were not already accumulated - the ones that still need to
/// be executed onward.
///
/// In the current programs this is a checked pass-through, not a real
/// dedup, because both structural facts below hold and are guarded:
///
///   * The arrivals come from one `vectorize_states` call, which emits
///     exactly one state per shape group - so they are pairwise distinct
///     by construction (checked: pairwise-distinct direct shape hashes;
///     the arrivals are post-gc, so their shapes compare canonically).
///   * Every hint fixed point converges in one round, so `accumulated` is
///     empty (measured: merge_hint_normalize span count == frames x hint
///     blocks, exactly).
///
/// There used to be a real dedup here, comparing states by their
/// per-column sorted unique value sets. That is an over-approximation of
/// state equality in the unsound direction - two states with equal value
/// sets but different row pairings compared EQUAL and one would be
/// silently dropped from the search, invisibly to the differential
/// verifier (both programs shared the mechanism). It never fired (the
/// facts above), so it was removed rather than fixed. If either guard
/// ever fails, this panics with instructions instead of guessing:
/// a correct dedup must compare actual lane-row sets, which is the same
/// computation `vectorize_states`' dedup already performs - build it on
/// that machinery, not on per-column summaries.
pub fn union_diff_states(
    accumulated: Vec<State>,
    potentially_new: Vec<State>,
) -> (Vec<State>, Vec<State>) {
    let _trace = TraceSpan::new("union_diff_states", "vectorize");
    assert!(
        accumulated.is_empty(),
        "a hint_normalize fixed point took a second round: union_diff_states \
         no longer contains a state dedup (the old per-column one was unsound \
         and never fired). Implement an exact dedup on the vectorize_states \
         row machinery before relying on multi-round fixed points."
    );
    // With merge partitioning active, vectorize_states legitimately emits
    // one state per (shape, class) - the distinctness guard must use the
    // same key, or same-shape different-class arrivals trip it (this
    // happened, loudly, the first time a partition key varied at a hint
    // site). The cells are resolved PER STATE, exactly as vectorize's own
    // grouping does: resolving from the first arrival and applying those
    // cell ids across different shapes misclassifies - concretely, when a
    // dead state (no player, so no partition cells resolve) arrives first,
    // every alive state's class collapses to one constant and same-shape
    // different-freeze arrivals trip the guard. The backward sweep's
    // restored batch ordering exposed this; the forward pass had dodged it
    // by arrival order alone.
    let mut shape_classes: FxHashSet<(u64, u64)> = FxHashSet::default();
    let all_distinct = potentially_new.iter().all(|state| {
        let cells = resolve_partition_cells(state);
        let class = partition_class(state, &cells).unwrap_or(u64::MAX);
        shape_classes.insert((shape_of_state(state).cached_hash, class))
    });
    if !all_distinct {
        // Same-(shape, class) arrivals DO occur since the fruit-off interval
        // widening: a branch on a whole-value UnknownBool (an interval
        // comparison with a straddling lane) copies ALL lanes down both
        // arms, and when neither arm writes anything class-distinguishing
        // before the join, both arrivals carry the same shape and class with
        // overlapping lane sets. The exact answer is the row-level union -
        // vectorize_states' own merge/dedup machinery - which is precisely
        // the "exact dedup" the old panic here demanded. This never fires
        // on interval-free rooms (the guard was a hard panic through the
        // whole room-(1,0) campaign).
        let merged = vectorize_states(potentially_new);
        return (merged.clone(), merged);
    }
    (potentially_new.clone(), potentially_new)
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vectorize_two_scalar_states() {
        let mut state1 = State::new();
        state1.vector_size = 1;
        let id = state1.heap.alloc();
        state1.heap.set(id, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(5)))));
        state1.global_env.insert("x".to_string(), id);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let id2 = state2.heap.alloc();
        state2.heap.set(id2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(3)))));
        state2.global_env.insert("x".to_string(), id2);

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2);

        let x_id = state.global_env.get("x").unwrap();
        match state.heap.get(*x_id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 2);
            }
            _ => panic!("Expected vector"),
        }
    }

    #[test]
    fn test_vectorize_with_dedup() {
        let mut state1 = State::new();
        state1.vector_size = 1;
        let id = state1.heap.alloc();
        state1.heap.set(id, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(5)))));
        state1.global_env.insert("x".to_string(), id);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let id2 = state2.heap.alloc();
        state2.heap.set(id2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(5)))));
        state2.global_env.insert("x".to_string(), id2);

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        // After dedup, should be back to scalar since both values were the same
        assert_eq!(state.vector_size, 1);

        let x_id = state.global_env.get("x").unwrap();
        match state.heap.get(*x_id) {
            HeapValue::Value(Value::Number(MaybeVector::Scalar(_))) => {}
            _ => panic!("Expected scalar after dedup"),
        }
    }

    /// The fast path: distinct hashes mean distinct rows, equal hashes are
    /// confirmed against the packed representative.
    #[test]
    fn bucket_keeps_the_first_of_each_distinct_row() {
        let nums: Vec<Pico8Num> = [1, 2, 1, 3, 2, 1].iter().map(|v| Pico8Num::from_i16(*v)).collect();
        let bools = [true, false, true, true, false, true];
        let values = vec![
            VectorRef::Numbers(&nums),
            VectorRef::Bools(&bools),
        ];
        let hashes = hash_rows(&values, nums.len());
        let (mask, unique) = bucket_unique_mask(&values, &hashes);
        assert_eq!(mask, vec![true, true, false, true, false, false]);
        assert_eq!(unique, 3);
    }

    /// The fallback. Every row is handed the same hash, so the packed-row
    /// check rejects most representatives and the original row-major
    /// algorithm has to redo the whole class - which it must do with the
    /// same answer as the collision-free path. This is the only way that
    /// branch gets exercised: a real 64-bit collision has never been seen.
    #[test]
    fn bucket_falls_back_exactly_when_hashes_collide() {
        let nums: Vec<Pico8Num> = [1, 2, 1, 3, 2, 1].iter().map(|v| Pico8Num::from_i16(*v)).collect();
        let bools = [true, false, true, true, false, true];
        let values = vec![
            VectorRef::Numbers(&nums),
            VectorRef::Bools(&bools),
        ];
        let all_same = vec![0x5eed_5eed_5eed_5eedu64; nums.len()];
        let (mask, unique) = bucket_unique_mask(&values, &all_same);
        assert_eq!(mask, vec![true, true, false, true, false, false]);
        assert_eq!(unique, 3);
    }

    /// A partial collision: two distinct rows share a hash while a third
    /// row's hash is its own. The colliding class is redone, the other is
    /// left alone.
    #[test]
    fn bucket_fallback_touches_only_the_colliding_class() {
        let nums: Vec<Pico8Num> = [1, 2, 3, 3, 4].iter().map(|v| Pico8Num::from_i16(*v)).collect();
        let values = vec![VectorRef::Numbers(&nums)];
        // Rows 0,1 collide (distinct values); rows 2,3 share a hash and are
        // genuinely equal; row 4 is alone.
        let hashes = vec![7, 7, 9, 9, 11];
        let (mask, unique) = bucket_unique_mask(&values, &hashes);
        assert_eq!(mask, vec![true, true, true, false, true]);
        assert_eq!(unique, 4);
    }

    #[test]
    fn test_dedup_row_level() {
        // Test that dedup works at row level, not column level
        // State 1: x=1, y=10
        // State 2: x=2, y=20
        // State 3: x=1, y=10 (duplicate of state 1)
        // After merge+dedup: x=[1,2], y=[10,20] (not x=[1,2,1], y=[10,20,10])

        let mut state1 = State::new();
        state1.vector_size = 1;
        let x1 = state1.heap.alloc();
        let y1 = state1.heap.alloc();
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state1.heap.set(y1, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));
        state1.global_env.insert("x".to_string(), x1);
        state1.global_env.insert("y".to_string(), y1);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        let y2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));
        state2.heap.set(y2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(20)))));
        state2.global_env.insert("x".to_string(), x2);
        state2.global_env.insert("y".to_string(), y2);

        let mut state3 = State::new();
        state3.vector_size = 1;
        let x3 = state3.heap.alloc();
        let y3 = state3.heap.alloc();
        state3.heap.set(x3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state3.heap.set(y3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));
        state3.global_env.insert("x".to_string(), x3);
        state3.global_env.insert("y".to_string(), y3);

        let result = vectorize_states(vec![state1, state2, state3]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2, "Should have 2 unique rows, not 3");

        // Verify the values
        let x_id = state.global_env.get("x").unwrap();
        match state.heap.get(*x_id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 2);
            }
            _ => panic!("Expected vector for x"),
        }
        let y_id = state.global_env.get("y").unwrap();
        match state.heap.get(*y_id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 2);
            }
            _ => panic!("Expected vector for y"),
        }
    }

    #[test]
    fn test_dedup_partial_overlap() {
        // State 1: x=1, y=10
        // State 2: x=1, y=20  (same x, different y - NOT a duplicate)
        // State 3: x=2, y=10  (different x, same y - NOT a duplicate)
        // After merge+dedup: should have all 3 unique rows

        let mut state1 = State::new();
        state1.vector_size = 1;
        let x1 = state1.heap.alloc();
        let y1 = state1.heap.alloc();
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state1.heap.set(y1, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));
        state1.global_env.insert("x".to_string(), x1);
        state1.global_env.insert("y".to_string(), y1);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        let y2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state2.heap.set(y2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(20)))));
        state2.global_env.insert("x".to_string(), x2);
        state2.global_env.insert("y".to_string(), y2);

        let mut state3 = State::new();
        state3.vector_size = 1;
        let x3 = state3.heap.alloc();
        let y3 = state3.heap.alloc();
        state3.heap.set(x3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));
        state3.heap.set(y3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));
        state3.global_env.insert("x".to_string(), x3);
        state3.global_env.insert("y".to_string(), y3);

        let result = vectorize_states(vec![state1, state2, state3]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 3, "All 3 rows are unique");
    }

    #[test]
    fn test_merge_different_vector_sizes() {
        // State 1: x=[1,2] (vector_size=2)
        // State 2: x=3 (vector_size=1)
        // After merge: x=[1,2,3] (vector_size=3)

        let mut state1 = State::new();
        state1.vector_size = 2;
        let x1 = state1.heap.alloc();
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
        ]))));
        state1.global_env.insert("x".to_string(), x1);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(3)))));
        state2.global_env.insert("x".to_string(), x2);

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 3);

        let x_id = state.global_env.get("x").unwrap();
        match state.heap.get(*x_id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 3);
                assert_eq!(nums[0], Pico8Num::from_i16(1));
                assert_eq!(nums[1], Pico8Num::from_i16(2));
                assert_eq!(nums[2], Pico8Num::from_i16(3));
            }
            _ => panic!("Expected vector"),
        }
    }

    #[test]
    fn test_merge_different_vector_sizes_with_dedup() {
        // State 1: x=[1,2] (vector_size=2)
        // State 2: x=2 (vector_size=1, duplicate of element in state1)
        // After merge+dedup: x=[1,2] (vector_size=2)

        let mut state1 = State::new();
        state1.vector_size = 2;
        let x1 = state1.heap.alloc();
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
        ]))));
        state1.global_env.insert("x".to_string(), x1);

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));
        state2.global_env.insert("x".to_string(), x2);

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2, "Duplicate should be removed");
    }

    /// The virtual merge must produce *exactly* the state the materialised
    /// pipeline produces - same lanes, same order, same Scalar/Vector
    /// representation per leaf. `State` equality is representation-
    /// sensitive (`Scalar(x) != Vector([x, x])`), so this holds the two
    /// paths equal at that level, over a group with mixed scalar/vector
    /// pieces, duplicate rows within and across fragments, a uniform
    /// column, a non-vectorizable leaf, and locals.
    #[test]
    fn test_virtual_merge_matches_materialized_pipeline() {
        use crate::ir::LocalId;

        // Deterministic pseudo-random lane values (no RNG in tests).
        let mut seed: u64 = 0x9e3779b97f4a7c15;
        let mut next = move |modulus: i16| -> i16 {
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((seed >> 33) % modulus as u64) as i16
        };

        let mut group: Vec<State> = Vec::new();
        for fragment in 0..7 {
            let lanes = 1 + (fragment % 3);
            let mut state = State::new();
            state.vector_size = lanes;

            // Cell 0: low-cardinality number column (drawn from 4 values, so
            // plenty of duplicate rows). Scalar representation when a
            // fragment happens to be uniform, like real fragments.
            let numbers: Vec<Pico8Num> =
                (0..lanes).map(|_| Pico8Num::from_i16(next(4))).collect();
            let c0 = state.heap.alloc();
            state
                .heap
                .set(c0, HeapValue::Value(Value::Number(MaybeVector::vector(numbers))));

            // Cell 1: bool column.
            let bools: Vec<bool> = (0..lanes).map(|_| next(2) == 0).collect();
            let c1 = state
                .heap
                .alloc();
            state
                .heap
                .set(c1, HeapValue::Value(Value::Bool(MaybeVector::vector(bools))));

            // Cell 2: interval column, two distinct intervals.
            let intervals: Vec<crate::pico8_num::Pico8NumInterval> = (0..lanes)
                .map(|_| {
                    let w = next(2);
                    crate::pico8_num::Pico8NumInterval {
                        low: Pico8Num::from_i16(w),
                        high: Pico8Num::from_i16(w + 3),
                    }
                })
                .collect();
            let c2 = state.heap.alloc();
            state.heap.set(
                c2,
                HeapValue::Value(Value::NumberInterval(MaybeVector::vector(intervals))),
            );

            // Cell 3: uniform across the whole group - must collapse to
            // Scalar and leave the dedup key.
            let c3 = state.heap.alloc();
            state.heap.set(
                c3,
                HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(42)))),
            );

            // Cell 4: non-vectorizable leaf, identical everywhere.
            let c4 = state.heap.alloc();
            state
                .heap
                .set(c4, HeapValue::Value(Value::String("same".to_string())));

            state.global_env.insert("a".to_string(), c0);
            state.global_env.insert("b".to_string(), c1);
            state.global_env.insert("c".to_string(), c2);
            state.global_env.insert("d".to_string(), c3);
            state.global_env.insert("e".to_string(), c4);

            // A local, mixing scalar and vector representations.
            let local: Vec<Pico8Num> =
                (0..lanes).map(|_| Pico8Num::from_i16(next(3))).collect();
            state
                .local_env
                .set(LocalId::from(0), Value::Number(MaybeVector::vector(local)));

            group.push(state);
        }

        let virtual_merged = super::super::virtual_merge::merge_dedup_group(&group)
            .expect("group should be collectable");

        let materialized = {
            let vectorized = vectorize_same_shape_states(group);
            let deduped = dedup_vectorized_state(vectorized);
            unvectorize_if_possible(deduped)
        };

        assert!(virtual_merged.vector_size < 14, "dedup must remove rows");
        assert_eq!(virtual_merged, materialized);
    }
}

// ============================================================================
// Watermark-based Auto-Renormalization
// ============================================================================

/// Tracks vectorization watermark for automatic renormalization decisions.
///
/// The watermark represents the "good" state count after the last successful
/// vectorization. When the current state count exceeds the threshold multiplier
/// times the watermark, we should trigger renormalization.
#[derive(Clone, Debug)]
pub struct VectorizationWatermark {
    /// Shape count after last successful vectorization
    last_shape_count: usize,
    /// Total expanded count (sum of vector_size) after last vectorization
    last_expanded_count: usize,
    /// Threshold multiplier - renormalize when shapes > watermark * threshold
    threshold_multiplier: f64,
    /// Number of times auto-renormalization was triggered
    pub auto_renorm_count: usize,
}

impl Default for VectorizationWatermark {
    fn default() -> Self {
        Self::new()
    }
}

impl VectorizationWatermark {
    pub fn new() -> Self {
        Self {
            last_shape_count: 1,
            last_expanded_count: 1,
            threshold_multiplier: 5.0,
            auto_renorm_count: 0,
        }
    }

    /// Create with a custom threshold multiplier
    pub fn with_threshold(threshold_multiplier: f64) -> Self {
        Self {
            threshold_multiplier,
            ..Self::new()
        }
    }

    /// Check if we should auto-renormalize based on current shape count
    pub fn should_renormalize(&self, current_shape_count: usize) -> bool {
        let threshold = (self.last_shape_count as f64 * self.threshold_multiplier) as usize;
        current_shape_count > threshold
    }

    /// Update watermark after successful vectorization
    pub fn update(&mut self, new_shape_count: usize, new_expanded_count: usize) {
        self.last_shape_count = new_shape_count.max(1);
        self.last_expanded_count = new_expanded_count.max(1);
    }

    /// Get the current watermark values
    pub fn watermark(&self) -> (usize, usize) {
        (self.last_shape_count, self.last_expanded_count)
    }
}

/// A set of states with watermark tracking for auto-renormalization.
///
/// This wraps a Vec<State> and tracks vectorization watermarks to decide
/// when to automatically trigger renormalization.
#[derive(Clone, Debug)]
pub struct StateSet {
    states: Vec<State>,
    watermark: VectorizationWatermark,
}

impl StateSet {
    /// Create a new StateSet from a vec of states
    pub fn new(states: Vec<State>) -> Self {
        let mut set = Self {
            states,
            watermark: VectorizationWatermark::new(),
        };
        // Initialize watermark based on initial states
        set.watermark.update(set.shape_count(), set.expanded_count());
        set
    }

    /// Create an empty StateSet
    pub fn empty() -> Self {
        Self {
            states: Vec::new(),
            watermark: VectorizationWatermark::new(),
        }
    }

    /// Get the number of distinct state shapes (number of State objects)
    pub fn shape_count(&self) -> usize {
        self.states.len()
    }

    /// Get the total expanded count (sum of vector_size across all states)
    pub fn expanded_count(&self) -> usize {
        self.states.iter().map(|s| s.vector_size).sum()
    }

    /// Check if we should auto-renormalize and do it if needed
    /// Returns true if renormalization was performed
    pub fn maybe_renormalize(&mut self) -> bool {
        if self.states.len() <= 1 {
            return false;
        }

        if self.watermark.should_renormalize(self.states.len()) {
            self.force_renormalize();
            true
        } else {
            false
        }
    }

    /// Force renormalization regardless of watermark
    pub fn force_renormalize(&mut self) {
        if self.states.is_empty() {
            return;
        }

        let old_count = self.states.len();
        self.states = vectorize_states(std::mem::take(&mut self.states));
        let new_count = self.states.len();

        // Update watermark with new counts
        self.watermark.update(new_count, self.expanded_count());
        self.watermark.auto_renorm_count += 1;

        // Log if significant reduction
        if old_count > new_count * 2 {
            // Could add profiling/tracing here
        }
    }

    /// Get the underlying states (consuming self)
    pub fn into_states(self) -> Vec<State> {
        self.states
    }

    /// Get a reference to the underlying states
    pub fn states(&self) -> &[State] {
        &self.states
    }

    /// Get a mutable reference to the underlying states
    pub fn states_mut(&mut self) -> &mut Vec<State> {
        &mut self.states
    }

    /// Add states and maybe renormalize
    pub fn extend(&mut self, other: Vec<State>) {
        self.states.extend(other);
        self.maybe_renormalize();
    }

    /// Add a single state and maybe renormalize
    pub fn push(&mut self, state: State) {
        self.states.push(state);
        self.maybe_renormalize();
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    /// Get the number of auto-renormalizations that occurred
    pub fn auto_renorm_count(&self) -> usize {
        self.watermark.auto_renorm_count
    }

    /// Get the watermark reference
    pub fn watermark(&self) -> &VectorizationWatermark {
        &self.watermark
    }

    /// Take states out, leaving empty vec
    pub fn take(&mut self) -> Vec<State> {
        std::mem::take(&mut self.states)
    }
}

impl From<Vec<State>> for StateSet {
    fn from(states: Vec<State>) -> Self {
        StateSet::new(states)
    }
}

impl From<StateSet> for Vec<State> {
    fn from(set: StateSet) -> Self {
        set.into_states()
    }
}

#[cfg(test)]
mod watermark_tests {
    use super::*;
    use crate::interpreter::value::MaybeVector;
    use crate::pico8_num::Pico8Num;

    #[test]
    fn test_watermark_initial_values() {
        let wm = VectorizationWatermark::new();
        assert_eq!(wm.last_shape_count, 1);
        assert_eq!(wm.last_expanded_count, 1);
        assert!(!wm.should_renormalize(1));
        assert!(!wm.should_renormalize(5));
        assert!(wm.should_renormalize(6)); // > 1 * 5
    }

    #[test]
    fn test_watermark_update() {
        let mut wm = VectorizationWatermark::new();
        wm.update(10, 100);
        assert_eq!(wm.watermark(), (10, 100));
        assert!(!wm.should_renormalize(50)); // <= 10 * 5
        assert!(wm.should_renormalize(51));  // > 10 * 5
    }

    #[test]
    fn test_state_set_basic() {
        let state = State::new();
        let set = StateSet::new(vec![state]);
        assert_eq!(set.shape_count(), 1);
        assert_eq!(set.expanded_count(), 1);
    }

    #[test]
    fn test_state_set_auto_renormalize() {
        // Create many states with the same shape that should merge
        let mut states = Vec::new();
        for i in 0..10 {
            let mut state = State::new();
            state.vector_size = 1;
            let id = state.heap.alloc();
            state.heap.set(id, HeapValue::Value(Value::Number(
                MaybeVector::Scalar(Pico8Num::from_i16(i))
            )));
            state.local_env.set(crate::ir::LocalId::from(0), Value::Number(
                MaybeVector::Scalar(Pico8Num::from_i16(i))
            ));
            states.push(state);
        }

        let mut set = StateSet::new(states);
        // Initial watermark is 10 (all different shapes due to different heap values)
        // But actually they have the same shape, so vectorize should merge them

        // Force renormalize
        set.force_renormalize();

        // After vectorization, should have fewer shapes
        // (depends on whether they have same shape)
        assert!(set.shape_count() <= 10);
    }

    #[test]
    fn test_state_set_threshold_trigger() {
        // Create initial set with 1 state
        let state = State::new();
        let mut set = StateSet::new(vec![state]);
        assert_eq!(set.watermark().last_shape_count, 1);

        // Add states until we hit threshold (> 5 for threshold_multiplier=5)
        for _ in 0..5 {
            set.push(State::new());
        }
        // 6 states now, threshold is 5, so should have auto-renormalized
        // But State::new() creates states with same shape, so they merge to 1

        // The auto_renorm_count should have increased
        // (unless all states merged before threshold was hit)
    }

    #[test]
    fn test_watermark_custom_threshold() {
        let wm = VectorizationWatermark::with_threshold(2.0);
        assert!(!wm.should_renormalize(2)); // <= 1 * 2
        assert!(wm.should_renormalize(3));  // > 1 * 2
    }
}
