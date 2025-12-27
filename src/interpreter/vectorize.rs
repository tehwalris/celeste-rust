//! State vectorization: Merging multiple states with the same "shape" into a single
//! state with vector values. This is a key optimization for abstract interpretation.
//!
//! The algorithm works as follows:
//! 1. Group states by their "shape" (structure with vectorizable values normalized)
//! 2. For each group, merge all states into one with vector values
//! 3. Deduplicate vectors (remove duplicate elements)
//! 4. If vector size becomes 1, convert back to scalars

use std::collections::HashMap;

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
    // Get heap structure
    let mut heap_structure = Vec::new();
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        let shape = normalize_heap_value_for_shape(state.heap.get(id));
        heap_structure.push((id, shape));
    }

    // Get local env structure (sorted for consistent comparison)
    let mut local_env_structure: Vec<_> = state.local_env.iter()
        .map(|(k, v)| (k, normalize_value_for_shape(v)))
        .collect();
    local_env_structure.sort_by_key(|(k, _)| *k);

    // Get outer local envs structure (sorted for consistent comparison)
    let outer_local_envs_structure: Vec<Vec<_>> = state.outer_local_envs.iter()
        .map(|env| {
            let mut entries: Vec<_> = env.iter()
                .map(|(k, v)| (k, normalize_value_for_shape(v)))
                .collect();
            entries.sort_by_key(|(k, _)| *k);
            entries
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

    // Create mask for filtering
    let mut mask = vec![false; state.vector_size];
    for i in &unique_indices {
        // We need to pick indices in a way that maintains some order
        mask[*i] = true;
    }

    // Actually, we need to filter based on unique_indices properly
    // The indices in unique_indices are the ones we want to keep
    let mask: Vec<bool> = (0..state.vector_size)
        .map(|i| unique_indices.contains(&i))
        .collect();

    state.filter_by_mask(&mask)
}

/// Collect all vectorizable vector values from a state for dedup purposes.
fn collect_vector_values(state: &State) -> Vec<VectorRef> {
    let mut vectors = Vec::new();

    // From heap
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        match state.heap.get(id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::Numbers(v.clone()));
            }
            HeapValue::Value(Value::NumberInterval(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::NumberIntervals(v.clone()));
            }
            HeapValue::Value(Value::Bool(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::Bools(v.clone()));
            }
            HeapValue::Closure(_, captures) => {
                for cap in captures {
                    match cap {
                        Value::Number(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::Numbers(v.clone()));
                        }
                        Value::NumberInterval(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::NumberIntervals(v.clone()));
                        }
                        Value::Bool(MaybeVector::Vector(v)) => {
                            vectors.push(VectorRef::Bools(v.clone()));
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
                vectors.push(VectorRef::Numbers(nums.clone()));
            }
            Value::NumberInterval(MaybeVector::Vector(nums)) => {
                vectors.push(VectorRef::NumberIntervals(nums.clone()));
            }
            Value::Bool(MaybeVector::Vector(bools)) => {
                vectors.push(VectorRef::Bools(bools.clone()));
            }
            _ => {}
        }
    }

    // From outer_local_envs
    for env in &state.outer_local_envs {
        for (_, v) in env.iter() {
            match v {
                Value::Number(MaybeVector::Vector(nums)) => {
                    vectors.push(VectorRef::Numbers(nums.clone()));
                }
                Value::NumberInterval(MaybeVector::Vector(nums)) => {
                    vectors.push(VectorRef::NumberIntervals(nums.clone()));
                }
                Value::Bool(MaybeVector::Vector(bools)) => {
                    vectors.push(VectorRef::Bools(bools.clone()));
                }
                _ => {}
            }
        }
    }

    vectors
}

#[derive(Clone)]
enum VectorRef {
    Numbers(Vec<Pico8Num>),
    NumberIntervals(Vec<Pico8NumInterval>),
    Bools(Vec<bool>),
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

    // Check heap
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        match state.heap.get(id) {
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
/// IMPORTANT: Only cleans scalar states (vector_size=1) to avoid issues with
/// states that have internal vector values.
fn clean_local_envs_for_merging(states: Vec<State>) -> Vec<State> {
    use std::collections::HashSet;
    use crate::ir::LocalId;

    if states.len() <= 1 {
        return states;
    }

    // Only clean states that are all scalar (vector_size=1)
    // Vectorized states might have internal vector values with different lengths
    let all_scalar = states.iter().all(|s| s.vector_size == 1);
    if !all_scalar {
        return states;
    }

    // Find the intersection of all local_env keys
    let mut common_keys: HashSet<usize> = states[0].local_env.iter().map(|(k, _)| k).collect();
    for state in &states[1..] {
        let state_keys: HashSet<usize> = state.local_env.iter().map(|(k, _)| k).collect();
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
pub fn vectorize_states(states: Vec<State>) -> Vec<State> {
    if states.is_empty() {
        return states;
    }

    // Validate input states
    for state in &states {
        assert_state_vector_lengths(state);
    }

    // Clean local_envs: remove variables that don't appear in all states.
    // This allows states with different dead temporaries to merge.
    let states = clean_local_envs_for_merging(states);

    // Group states by shape
    let mut states_by_shape: HashMap<StateShape, Vec<State>> = HashMap::new();
    for state in states {
        let shape = shape_of_state(&state);
        states_by_shape.entry(shape).or_insert_with(Vec::new).push(state);
    }

    // Vectorize each group
    let result: Vec<State> = states_by_shape
        .into_iter()
        .map(|(_, group)| {
            let vectorized = vectorize_same_shape_states(group);
            let deduped = dedup_vectorized_state(vectorized);
            unvectorize_if_possible(deduped)
        })
        .collect();

    // Validate output states
    for state in &result {
        assert_state_vector_lengths(state);
    }

    result
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
