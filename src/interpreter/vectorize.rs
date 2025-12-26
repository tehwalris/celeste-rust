//! State vectorization: Merging multiple states with the same "shape" into a single
//! state with vector values. This is a key optimization for abstract interpretation.
//!
//! The algorithm works as follows:
//! 1. Group states by their "shape" (structure with vectorizable values normalized)
//! 2. For each group, merge all states into one with vector values
//! 3. Deduplicate vectors (remove duplicate elements)
//! 4. If vector size becomes 1, convert back to scalars

use std::collections::HashMap;

use im::HashMap as ImHashMap;

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::GlobalId;
use crate::pico8_num::Pico8Num;

/// A "shape" is a state with all vectorizable values normalized to placeholder values.
/// States with the same shape can be merged by vectorizing their values.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StateShape {
    // For shape comparison, we normalize all vectorizable values to placeholders
    // but keep the structure (heap IDs, table shapes, etc.)
    heap_structure: Vec<(HeapId, HeapValueShape)>,
    local_env_structure: Vec<(usize, ValueShape)>,
    outer_local_envs_structure: Vec<Vec<(usize, ValueShape)>>,
    global_env: ImHashMap<String, HeapId>,
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
    VectorizableBool,
    // Non-vectorizable values keep their actual value for shape comparison
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
    UnknownBool,
}

/// Can this value be vectorized (combined with others into a vector)?
fn can_vectorize_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Number(_) | Value::Bool(MaybeVector::Scalar(_)) | Value::Bool(MaybeVector::Vector(_))
    )
}

fn normalize_value_for_shape(value: &Value) -> ValueShape {
    if !can_vectorize_value(value) {
        // Non-vectorizable values keep their identity for shape comparison
        // States with different non-vectorizable values cannot be merged
        match value {
            Value::String(s) => ValueShape::String(s.clone()),
            Value::Nil(hint) => ValueShape::Nil(hint.clone()),
            Value::Pointer(id) => ValueShape::Pointer(*id),
            Value::NilPointer(s) => ValueShape::NilPointer(s.clone()),
            Value::UnknownBool => ValueShape::UnknownBool,
            _ => unreachable!("Should be vectorizable"),
        }
    } else {
        match value {
            Value::Number(_) => ValueShape::VectorizableNumber,
            Value::Bool(_) => ValueShape::VectorizableBool,
            _ => unreachable!(),
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

    StateShape {
        heap_structure,
        local_env_structure,
        outer_local_envs_structure,
        global_env: state.global_env.clone(),
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
    Bool(bool),
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
    UnknownBool,
}

impl ScalarValue {
    fn is_vectorizable(&self) -> bool {
        matches!(self, ScalarValue::Number(_) | ScalarValue::Bool(_))
    }
}

fn scalar_value_from_value(value: &Value) -> ScalarValue {
    match value {
        Value::Number(MaybeVector::Scalar(n)) => ScalarValue::Number(*n),
        Value::Number(MaybeVector::Vector(nums)) => {
            assert_eq!(nums.len(), 1);
            ScalarValue::Number(nums[0])
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
    // Expand all values to their scalar forms, then combine
    let all_scalars: Vec<ScalarValue> = values.iter()
        .flat_map(|(v, size)| expand_value_to_scalars(v, *size))
        .collect();

    // Check if vectorizable
    let first = &all_scalars[0];
    if first.is_vectorizable() {
        value_from_scalars(all_scalars)
    } else {
        // Non-vectorizable: all must be equal
        if !all_scalars.iter().all(|s| s == first) {
            panic!("Non-vectorizable values are not equal");
        }
        match first {
            ScalarValue::String(s) => Value::String(s.clone()),
            ScalarValue::Nil(hint) => Value::Nil(hint.clone()),
            ScalarValue::Pointer(id) => Value::Pointer(*id),
            ScalarValue::NilPointer(s) => Value::NilPointer(s.clone()),
            ScalarValue::UnknownBool => Value::UnknownBool,
            _ => unreachable!(),
        }
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

fn collect_vector_values(state: &State) -> Vec<VectorRef> {
    let mut vectors = Vec::new();

    // From heap
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        match state.heap.get(id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(v))) => {
                vectors.push(VectorRef::Numbers(v.clone()));
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
    Bools(Vec<bool>),
}

fn scalar_at_index(vec: &VectorRef, index: usize) -> ScalarValue {
    match vec {
        VectorRef::Numbers(nums) => ScalarValue::Number(nums[index]),
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
        Value::Bool(MaybeVector::Vector(bools)) if bools.len() == 1 => {
            Value::Bool(MaybeVector::Scalar(bools[0]))
        }
        other => other,
    });

    state
}

/// Assert that all vector values in a state have the correct length.
fn assert_state_vector_lengths(state: &State) {
    let expected_len = state.vector_size;

    // Check heap
    for i in 0..state.heap.len() {
        let id = HeapId::from_raw(i);
        match state.heap.get(id) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(v))) => {
                assert_eq!(v.len(), expected_len, "Vector length mismatch in heap (numbers)");
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
                Value::Number(MaybeVector::Vector(vec)) => {
                    assert_eq!(vec.len(), expected_len, "Vector length mismatch in outer_local_env (numbers)");
                }
                Value::Bool(MaybeVector::Vector(vec)) => {
                    assert_eq!(vec.len(), expected_len, "Vector length mismatch in outer_local_env (bools)");
                }
                _ => {}
            }
        }
    }
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
}
