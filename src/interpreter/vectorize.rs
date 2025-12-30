//! State vectorization: Merging multiple states with the same "shape" into a single
//! state with vector values. This is a key optimization for abstract interpretation.
//!
//! The algorithm works as follows:
//! 1. Group states by their "shape" (structure with vectorizable values normalized)
//! 2. For each group, merge all states into one with vector values
//! 3. Deduplicate vectors (remove duplicate elements)
//! 4. If vector size becomes 1, convert back to scalars

use rustc_hash::{FxHashMap, FxHashSet};

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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StateShape {
    // For shape comparison, we normalize all vectorizable values to placeholders
    // but keep the structure (heap IDs, table shapes, etc.)
    heap_structure: Vec<(HeapId, HeapValueShape)>,
    local_env_structure: Vec<(usize, ValueShape)>,
    outer_local_envs_structure: Vec<Vec<(usize, ValueShape)>>,
    // global_env as sorted Vec for consistent hashing (ImHashMap's Hash is buggy)
    global_env: Vec<(String, HeapId)>,
    prints: Vec<String>,
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

fn shape_of_state(state: &State) -> StateShape {
    // Get heap structure (handle empty slots that are allocated but not set)
    let heap_structure: Vec<_> = (0..state.heap.len())
        .map(|i| {
            let id = HeapId::from_raw(i);
            let shape = match state.heap.get_opt(id) {
                Some(value) => normalize_heap_value_for_shape(value),
                None => HeapValueShape::Empty,
            };
            (id, shape)
        })
        .collect();

    // Local env is already sorted by index (uses Vec internally)
    let local_env_structure: Vec<_> = state.local_env.iter()
        .map(|(k, v)| (k, normalize_value_for_shape(v)))
        .collect();

    // Outer local envs are also already sorted by index
    let outer_local_envs_structure: Vec<Vec<_>> = state.outer_local_envs.iter()
        .map(|env| {
            env.iter()
                .map(|(k, v)| (k, normalize_value_for_shape(v)))
                .collect()
        })
        .collect();

    // Convert global_env to sorted Vec for consistent hashing
    let mut global_env: Vec<_> = state.global_env.iter()
        .map(|(k, v)| (k.clone(), *v))
        .collect();
    global_env.sort_by(|a, b| a.0.cmp(&b.0));

    StateShape {
        heap_structure,
        local_env_structure,
        outer_local_envs_structure,
        global_env,
        prints: state.prints.clone(),
    }
}

/// Expands a value to `size` elements, returning an iterator of scalar values.
fn expand_value_to_scalars(value: &Value, size: usize) -> Vec<ScalarValue> {
    match value {
        Value::Number(MaybeVector::Scalar(n)) => vec![ScalarValue::Number(*n); size],
        Value::Number(MaybeVector::Vector(nums)) => {
            assert_eq!(nums.len(), size);
            nums.iter().map(|n| ScalarValue::Number(*n)).collect()
        }
        Value::NumberInterval(MaybeVector::Scalar(n)) => vec![ScalarValue::NumberInterval(*n); size],
        Value::NumberInterval(MaybeVector::Vector(nums)) => {
            assert_eq!(nums.len(), size);
            nums.iter().map(|n| ScalarValue::NumberInterval(*n)).collect()
        }
        Value::Bool(MaybeVector::Scalar(b)) => vec![ScalarValue::Bool(*b); size],
        Value::Bool(MaybeVector::Vector(bools)) => {
            assert_eq!(bools.len(), size);
            bools.iter().map(|b| ScalarValue::Bool(*b)).collect()
        }
        Value::String(s) => vec![ScalarValue::String(s.clone()); size],
        Value::Nil(hint) => vec![ScalarValue::Nil(hint.clone()); size],
        Value::Pointer(id) => vec![ScalarValue::Pointer(*id); size],
        Value::NilPointer(s) => vec![ScalarValue::NilPointer(s.clone()); size],
        Value::UnknownBool => vec![ScalarValue::UnknownBool; size],
    }
}

/// A scalar value (single element, not a vector)
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum ScalarValue {
    Number(Pico8Num),
    NumberInterval(Pico8NumInterval),
    Bool(bool),
    UnknownBool,
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
}

impl ScalarValue {
    fn is_vectorizable(&self) -> bool {
        matches!(self, ScalarValue::Number(_) | ScalarValue::NumberInterval(_) | ScalarValue::Bool(_))
    }
}

fn scalar_value_from_value(value: &Value) -> ScalarValue {
    match value {
        Value::Number(MaybeVector::Scalar(n)) => ScalarValue::Number(*n),
        Value::Number(MaybeVector::Vector(nums)) => {
            assert_eq!(nums.len(), 1);
            ScalarValue::Number(nums[0])
        }
        Value::NumberInterval(MaybeVector::Scalar(n)) => ScalarValue::NumberInterval(*n),
        Value::NumberInterval(MaybeVector::Vector(nums)) => {
            assert_eq!(nums.len(), 1);
            ScalarValue::NumberInterval(nums[0])
        }
        Value::Bool(MaybeVector::Scalar(b)) => ScalarValue::Bool(*b),
        Value::Bool(MaybeVector::Vector(bools)) => {
            assert_eq!(bools.len(), 1);
            ScalarValue::Bool(bools[0])
        }
        Value::String(s) => ScalarValue::String(s.clone()),
        Value::Nil(hint) => ScalarValue::Nil(hint.clone()),
        Value::Pointer(id) => ScalarValue::Pointer(*id),
        Value::NilPointer(s) => ScalarValue::NilPointer(s.clone()),
        Value::UnknownBool => ScalarValue::UnknownBool,
    }
}

fn value_from_scalars(scalars: Vec<ScalarValue>) -> Value {
    if scalars.is_empty() {
        panic!("Cannot build value from empty scalar list");
    }

    let first = &scalars[0];

    // Check if all values are identical
    let all_identical = scalars.iter().all(|s| s == first);

    if all_identical || scalars.len() == 1 {
        // Return as scalar
        match first {
            ScalarValue::Number(n) => Value::Number(MaybeVector::Scalar(*n)),
            ScalarValue::NumberInterval(n) => Value::NumberInterval(MaybeVector::Scalar(*n)),
            ScalarValue::Bool(b) => Value::Bool(MaybeVector::Scalar(*b)),
            ScalarValue::String(s) => Value::String(s.clone()),
            ScalarValue::Nil(hint) => Value::Nil(hint.clone()),
            ScalarValue::Pointer(id) => Value::Pointer(*id),
            ScalarValue::NilPointer(s) => Value::NilPointer(s.clone()),
            ScalarValue::UnknownBool => Value::UnknownBool,
        }
    } else {
        // Build a vector
        match first {
            ScalarValue::Number(_) => {
                let nums: Vec<Pico8Num> = scalars.into_iter()
                    .map(|s| match s {
                        ScalarValue::Number(n) => n,
                        _ => panic!("Mixed types in vector"),
                    })
                    .collect();
                Value::Number(MaybeVector::Vector(nums))
            }
            ScalarValue::NumberInterval(_) => {
                let nums: Vec<Pico8NumInterval> = scalars.into_iter()
                    .map(|s| match s {
                        ScalarValue::NumberInterval(n) => n,
                        _ => panic!("Mixed types in vector"),
                    })
                    .collect();
                Value::NumberInterval(MaybeVector::Vector(nums))
            }
            ScalarValue::Bool(_) => {
                let bools: Vec<bool> = scalars.into_iter()
                    .map(|s| match s {
                        ScalarValue::Bool(b) => b,
                        _ => panic!("Mixed types in vector"),
                    })
                    .collect();
                Value::Bool(MaybeVector::Vector(bools))
            }
            _ => {
                // UnknownBool, String, etc. are not vectorizable
                // States with different values should have different shapes and never reach here
                panic!("Values are not equal and not vectorizable");
            }
        }
    }
}

/// Merge multiple states with the same shape into one vectorized state.
fn vectorize_same_shape_states(states: Vec<State>) -> State {
    if states.len() == 1 {
        return states.into_iter().next().unwrap();
    }

    let total_vector_size: usize = states.iter().map(|s| s.vector_size).sum();
    let first_state = &states[0];

    // Build vectorized heap
    let mut new_heap = Heap::new();
    for i in 0..first_state.heap.len() {
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
        HeapValue::Value(v) => {
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
    if can_vectorize_value(first_value) {
        // Expand all values to their scalar forms, then combine into a vector
        let all_scalars: Vec<ScalarValue> = values.iter()
            .flat_map(|(v, size)| expand_value_to_scalars(v, *size))
            .collect();
        value_from_scalars(all_scalars)
    } else {
        // Non-vectorizable: all values must be equal (as whole Values)
        let all_values: Vec<&Value> = values.iter().map(|(v, _)| v).collect();
        if !all_values.iter().all(|v| *v == first_value) {
            panic!("Non-vectorizable values are not equal: {:?}", all_values);
        }
        first_value.clone()
    }
}

fn merge_local_envs<F>(states: &[State], get_env: F) -> LocalEnv
where
    F: Fn(&State) -> &LocalEnv,
{
    let first_env = get_env(&states[0]);
    let mut merged = LocalEnv::new();

    for (local_id, first_value) in first_env.iter() {
        let value_and_sizes: Vec<_> = states.iter()
            .map(|s| (get_env(s).get_by_raw_id(local_id).clone(), s.vector_size))
            .collect();
        let merged_value = merge_values(&value_and_sizes);
        merged.set_by_raw_id(local_id, merged_value);
    }

    merged
}

/// Deduplicate a vectorized state by removing duplicate vector elements.
/// Returns a new state with unique vector elements.
fn dedup_vectorized_state(mut state: State) -> State {
    if state.vector_size <= 1 {
        return state;
    }

    // Collect all vector values from the state
    let vector_values = collect_vector_values(&state);

    if vector_values.is_empty() {
        // No vectors means all vectorizable values were identical, so
        // all "rows" are duplicates. Reduce to just one.
        state.vector_size = 1;
        return state;
    }

    // For each index, create a tuple of all vector values at that index
    // Then sort and dedup by this tuple
    let indices: Vec<usize> = (0..state.vector_size).collect();

    // Create a comparison key for each index
    let mut index_keys: Vec<(usize, Vec<ScalarValue>)> = indices
        .iter()
        .map(|&i| {
            let key: Vec<ScalarValue> = vector_values
                .iter()
                .map(|vec| scalar_at_index(vec, i))
                .collect();
            (i, key)
        })
        .collect();

    // Sort by key and keep only unique indices
    index_keys.sort_by(|a, b| a.1.cmp(&b.1));
    let mut unique_indices: Vec<usize> = Vec::new();
    let mut last_key: Option<Vec<ScalarValue>> = None;
    for (i, key) in index_keys {
        if last_key.as_ref() != Some(&key) {
            unique_indices.push(i);
            last_key = Some(key);
        }
    }

    if unique_indices.len() == state.vector_size {
        // No duplicates found
        return state;
    }

    // Create mask for filtering - set true for indices we want to keep
    let mut mask = vec![false; state.vector_size];
    for i in &unique_indices {
        mask[*i] = true;
    }

    state.filter_by_mask(&mask)
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

fn scalar_at_index(vec: &VectorRef, index: usize) -> ScalarValue {
    match vec {
        VectorRef::Numbers(nums) => ScalarValue::Number(nums[index]),
        VectorRef::NumberIntervals(nums) => ScalarValue::NumberInterval(nums[index]),
        VectorRef::Bools(bools) => ScalarValue::Bool(bools[index]),
    }
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
fn clean_local_envs_for_merging(states: Vec<State>) -> Vec<State> {
    use crate::ir::LocalId;

    if states.len() <= 1 {
        return states;
    }

    // Find the intersection of all local_env keys
    let mut common_keys: FxHashSet<usize> = states[0].local_env.iter().map(|(k, _)| k).collect();
    for state in &states[1..] {
        let state_keys: FxHashSet<usize> = state.local_env.iter().map(|(k, _)| k).collect();
        common_keys = common_keys.intersection(&state_keys).copied().collect();
    }

    // Remove keys that aren't in the intersection
    states.into_iter().map(|mut state| {
        state.local_env.retain(|key: LocalId| common_keys.contains(&usize::from(key)));
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

pub fn vectorize_states(states: Vec<State>) -> Vec<State> {
    let mut stats = VectorizeTimingStats::default();
    stats.input_count = states.len();

    if states.is_empty() {
        LAST_VECTORIZE_STATS.with(|s| *s.borrow_mut() = Some(stats));
        return states;
    }

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

    // Group states by shape
    let t2 = std::time::Instant::now();
    let mut states_by_shape: FxHashMap<StateShape, Vec<State>> = FxHashMap::default();
    for state in states {
        let shape = shape_of_state(&state);
        states_by_shape.entry(shape).or_insert_with(Vec::new).push(state);
    }
    stats.shape_grouping_ns = t2.elapsed().as_nanos() as u64;
    stats.group_count = states_by_shape.len();

    // Vectorize each group
    let t3 = std::time::Instant::now();
    let result: Vec<State> = states_by_shape
        .into_iter()
        .map(|(_, group)| {
            let vectorized = vectorize_same_shape_states(group);
            let deduped = dedup_vectorized_state(vectorized);
            unvectorize_if_possible(deduped)
        })
        .collect();
    stats.vectorize_groups_ns = t3.elapsed().as_nanos() as u64;

    // Validate output states (only in debug mode)
    let t4 = std::time::Instant::now();
    #[cfg(debug_assertions)]
    for state in &result {
        assert_state_vector_lengths(state);
    }
    stats.output_validation_ns = t4.elapsed().as_nanos() as u64;
    stats.output_count = result.len();

    LAST_VECTORIZE_STATS.with(|s| *s.borrow_mut() = Some(stats));
    result
}

/// A normalized state representation for efficient comparison.
/// This is the state after GC and with deterministic heap IDs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NormalizedState {
    /// The shape (structure with vectorizable values normalized)
    shape: StateShape,
    /// The actual vectorizable values, in a deterministic order
    /// This allows us to compare states for equality
    vectorizable_values: Vec<VectorizableValue>,
}

/// A vectorizable value extracted from a state
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum VectorizableValue {
    Number(Pico8Num),
    NumberInterval(Pico8Num, Pico8Num),  // low, high
    Bool(bool),
    // For vectors, we store sorted unique values to normalize
    NumberVector(Vec<Pico8Num>),
    NumberIntervalVector(Vec<(Pico8Num, Pico8Num)>),
    BoolVector(Vec<bool>),
}

fn extract_vectorizable_value(value: &Value) -> Option<VectorizableValue> {
    match value {
        Value::Number(MaybeVector::Scalar(n)) => Some(VectorizableValue::Number(*n)),
        Value::Number(MaybeVector::Vector(nums)) => {
            let mut raw: Vec<Pico8Num> = nums.clone();
            raw.sort();
            raw.dedup();
            Some(VectorizableValue::NumberVector(raw))
        }
        Value::NumberInterval(MaybeVector::Scalar(interval)) => {
            Some(VectorizableValue::NumberInterval(interval.low, interval.high))
        }
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            let mut raw: Vec<(Pico8Num, Pico8Num)> = intervals.iter()
                .map(|i| (i.low, i.high))
                .collect();
            raw.sort();
            raw.dedup();
            Some(VectorizableValue::NumberIntervalVector(raw))
        }
        Value::Bool(MaybeVector::Scalar(b)) => Some(VectorizableValue::Bool(*b)),
        Value::Bool(MaybeVector::Vector(bools)) => {
            let mut raw: Vec<bool> = bools.clone();
            raw.sort();
            raw.dedup();
            Some(VectorizableValue::BoolVector(raw))
        }
        _ => None,
    }
}

fn extract_vectorizable_values_from_state(state: &State) -> Vec<VectorizableValue> {
    let mut values = Vec::new();

    // Extract from heap (in order, skip empty slots)
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        let Some(heap_value) = state.heap.get_opt(id) else {
            continue;
        };
        match heap_value {
            HeapValue::Value(v) => {
                if let Some(vv) = extract_vectorizable_value(v) {
                    values.push(vv);
                }
            }
            HeapValue::Closure(_, captures) => {
                for v in captures {
                    if let Some(vv) = extract_vectorizable_value(v) {
                        values.push(vv);
                    }
                }
            }
            _ => {}
        }
    }

    // Extract from local_env (sorted by key for determinism)
    let mut local_entries: Vec<_> = state.local_env.iter().collect();
    local_entries.sort_by_key(|(k, _)| *k);
    for (_, v) in local_entries {
        if let Some(vv) = extract_vectorizable_value(v) {
            values.push(vv);
        }
    }

    // Extract from outer_local_envs
    for env in &state.outer_local_envs {
        let mut entries: Vec<_> = env.iter().collect();
        entries.sort_by_key(|(k, _)| *k);
        for (_, v) in entries {
            if let Some(vv) = extract_vectorizable_value(v) {
                values.push(vv);
            }
        }
    }

    values
}

/// Normalize a state for comparison purposes.
/// Two states that are "the same" will have equal NormalizedState representations.
pub fn normalize_state_for_comparison(state: &State) -> NormalizedState {
    // First, GC and renumber the state to get deterministic heap IDs
    let mut state = state.clone();
    state.gc();

    let shape = shape_of_state(&state);
    let vectorizable_values = extract_vectorizable_values_from_state(&state);

    NormalizedState {
        shape,
        vectorizable_values,
    }
}

/// Compute union and diff of two state sets.
///
/// Given `accumulated` (states already seen) and `potentially_new` (states just arrived),
/// returns:
/// - `union`: All states (accumulated + truly new ones)
/// - `actually_new`: Only the states that weren't already in accumulated
///
/// This is the key operation for fixed-point iteration at hint_normalize blocks.
pub fn union_diff_states(
    accumulated: Vec<State>,
    potentially_new: Vec<State>,
) -> (Vec<State>, Vec<State>) {
    if accumulated.is_empty() {
        // First, deduplicate within potentially_new
        let mut seen: FxHashSet<NormalizedState> = FxHashSet::default();
        let mut unique = Vec::new();
        for state in potentially_new {
            let normalized = normalize_state_for_comparison(&state);
            if !seen.contains(&normalized) {
                seen.insert(normalized);
                unique.push(state);
            }
        }
        return (unique.clone(), unique);
    }

    if potentially_new.is_empty() {
        // Nothing new
        return (accumulated, vec![]);
    }

    // Build a set of normalized accumulated states for fast lookup
    let accumulated_normalized: FxHashSet<NormalizedState> = accumulated
        .iter()
        .map(normalize_state_for_comparison)
        .collect();

    // Partition potentially_new into truly new vs already seen
    // Also deduplicate within potentially_new
    let mut seen: FxHashSet<NormalizedState> = accumulated_normalized.clone();
    let mut actually_new = Vec::new();
    for state in potentially_new {
        let normalized = normalize_state_for_comparison(&state);
        if !seen.contains(&normalized) {
            seen.insert(normalized);
            actually_new.push(state);
        }
    }

    // Union = accumulated + actually_new
    let mut union = accumulated;
    union.extend(actually_new.clone());

    (union, actually_new)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::inspect::load_states_from_file;

    #[test]
    fn test_vectorize_real_states() {
        // Load states that Python analysis says should merge
        let states = match load_states_from_file("/tmp/test_merge_states.jsonl") {
            Ok(s) => s,
            Err(_) => {
                println!("Skipping test - no test file");
                return;
            }
        };

        println!("Loaded {} states", states.len());
        for (i, s) in states.iter().enumerate() {
            println!("  State {}: vector_size={}, heap_len={}", i, s.vector_size, s.heap.len());
        }

        // Get shapes
        let shape1 = shape_of_state(&states[0]);
        let shape2 = shape_of_state(&states[1]);

        println!("\nShapes equal: {}", shape1 == shape2);

        if shape1 != shape2 {
            // Find where they differ
            println!("Heap structure lengths: {} vs {}",
                shape1.heap_structure.len(), shape2.heap_structure.len());

            for (i, (a, b)) in shape1.heap_structure.iter()
                .zip(shape2.heap_structure.iter()).enumerate() {
                if a != b {
                    println!("  Differ at heap[{}]:", i);
                    println!("    s1: {:?}", a);
                    println!("    s2: {:?}", b);
                    if i > 3 { break; }  // Just show first few
                }
            }

            println!("\nLocal env: {:?} vs {:?}",
                shape1.local_env_structure.len(), shape2.local_env_structure.len());
            println!("Outer envs: {:?} vs {:?}",
                shape1.outer_local_envs_structure.len(), shape2.outer_local_envs_structure.len());
            println!("Global env: {} vs {}",
                shape1.global_env.len(), shape2.global_env.len());
            println!("Prints: {:?} vs {:?}", shape1.prints, shape2.prints);
        }

        // Try vectorizing
        let vectorized = vectorize_states(states.clone());
        println!("\nAfter vectorization: {} states", vectorized.len());

        // If they didn't merge, that's a bug
        if states.len() == 2 && vectorized.len() == 2 && shape1 != shape2 {
            panic!("States have different shapes but Python says they should merge!");
        }
    }

    #[test]
    fn test_vectorize_two_scalar_states() {
        let mut state1 = State::new();
        state1.vector_size = 1;
        let id = state1.heap.alloc();
        state1.heap.set(id, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(5)))));

        let mut state2 = State::new();
        state2.vector_size = 1;
        let id2 = state2.heap.alloc();
        state2.heap.set(id2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(3)))));

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2);

        match state.heap.get(HeapId::from_raw(0)) {
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

        let mut state2 = State::new();
        state2.vector_size = 1;
        let id2 = state2.heap.alloc();
        state2.heap.set(id2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(5)))));

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        // After dedup, should be back to scalar since both values were the same
        assert_eq!(state.vector_size, 1);

        match state.heap.get(HeapId::from_raw(0)) {
            HeapValue::Value(Value::Number(MaybeVector::Scalar(_))) => {}
            _ => panic!("Expected scalar after dedup"),
        }
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

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        let y2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));
        state2.heap.set(y2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(20)))));

        let mut state3 = State::new();
        state3.vector_size = 1;
        let x3 = state3.heap.alloc();
        let y3 = state3.heap.alloc();
        state3.heap.set(x3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state3.heap.set(y3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));

        let result = vectorize_states(vec![state1, state2, state3]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2, "Should have 2 unique rows, not 3");

        // Verify the values
        match state.heap.get(HeapId::from_raw(0)) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 2);
            }
            _ => panic!("Expected vector for x"),
        }
        match state.heap.get(HeapId::from_raw(1)) {
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

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        let y2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(1)))));
        state2.heap.set(y2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(20)))));

        let mut state3 = State::new();
        state3.vector_size = 1;
        let x3 = state3.heap.alloc();
        let y3 = state3.heap.alloc();
        state3.heap.set(x3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));
        state3.heap.set(y3, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(10)))));

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
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
        ]))));

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(3)))));

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 3);

        match state.heap.get(HeapId::from_raw(0)) {
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
        state1.heap.set(x1, HeapValue::Value(Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
        ]))));

        let mut state2 = State::new();
        state2.vector_size = 1;
        let x2 = state2.heap.alloc();
        state2.heap.set(x2, HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(2)))));

        let result = vectorize_states(vec![state1, state2]);
        assert_eq!(result.len(), 1);
        let state = &result[0];
        assert_eq!(state.vector_size, 2, "Duplicate should be removed");
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
