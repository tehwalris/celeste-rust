//! Coarse-grained parallel frame processing with hierarchical state partitioning.
//!
//! This module implements a parallelization strategy that:
//! 1. Partitions input states into blocks based on shape + spatial position
//! 2. Processes each block in parallel (including per-block vectorization)
//! 3. Merges and vectorizes results across all blocks
//!
//! The partitioning uses a hierarchical tree where:
//! - Top level branches by state shape (heap structure)
//! - Lower levels branch by player XY position in a quadtree-like manner
//! - The cut point is chosen to create roughly equal-sized blocks

use rayon::prelude::*;
use rustc_hash::FxHashMap;

use super::heap::HeapId;
use super::state::State;
use super::tracing::TraceSpan;
use super::value::{HeapValue, MaybeVector, Value};
use super::vectorize::{shape_of_state, vectorize_states};
use crate::pico8_num::Pico8Num;

/// Configuration for parallel block processing
#[derive(Clone, Debug)]
pub struct ParallelBlockConfig {
    /// Target number of blocks to create
    pub num_blocks: usize,
    /// Minimum states per block (don't create tiny blocks)
    pub min_block_size: usize,
}

impl Default for ParallelBlockConfig {
    fn default() -> Self {
        Self {
            num_blocks: 32,
            min_block_size: 16,
        }
    }
}

/// A spatial key for hierarchical partitioning
/// Uses integer coordinates at various granularities
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SpatialKey {
    /// Player X coordinate (integer part)
    x: i32,
    /// Player Y coordinate (integer part)
    y: i32,
}

impl SpatialKey {
    /// Create a spatial key at a given granularity level
    /// Level 0 = individual pixels, Level 1 = 2x2, Level 2 = 4x4, etc.
    fn at_level(&self, level: u32) -> SpatialKey {
        let divisor = 1 << level;
        SpatialKey {
            x: self.x / divisor,
            y: self.y / divisor,
        }
    }
}

/// Extract player position from a state
/// Returns None if no player found or position can't be extracted
fn extract_player_position(state: &State) -> Option<SpatialKey> {
    // Find "objects" global
    let objects_id = state.global_env.get("objects")?;

    // Dereference the pointer to get array
    let array_id = match state.heap.get(*objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };

    // Get the array
    let objects = match state.heap.get(array_id) {
        HeapValue::ArrayTable(items) => items,
        _ => return None,
    };

    // Find player object (has type field with "player" or type index)
    for &obj_ptr_id in objects {
        let obj_id = match state.heap.get(obj_ptr_id) {
            HeapValue::Value(Value::Pointer(id)) => *id,
            _ => continue,
        };

        let obj = match state.heap.get(obj_id) {
            HeapValue::ObjectTable(table) => table,
            _ => continue,
        };

        // Check if this is a player by looking for type field
        // In PICO-8 Celeste, player type is typically index 1 or has "player" string
        if let Some(&type_ptr) = obj.get("type") {
            let is_player = match state.heap.get(type_ptr) {
                HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => {
                    // Type 1 is typically player
                    n.as_i16() == Some(1)
                }
                HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                    // For vectorized, check if any is type 1
                    nums.iter().any(|n| n.as_i16() == Some(1))
                }
                _ => false,
            };

            if is_player {
                // Extract x and y
                if let (Some(&x_ptr), Some(&y_ptr)) = (obj.get("x"), obj.get("y")) {
                    let x = extract_num_scalar(&state.heap, x_ptr)?;
                    let y = extract_num_scalar(&state.heap, y_ptr)?;
                    return Some(SpatialKey {
                        x: x.whole_part_as_i16() as i32,
                        y: y.whole_part_as_i16() as i32,
                    });
                }
            }
        }
    }

    None
}

/// Extract a scalar number from a heap value pointer
fn extract_num_scalar(heap: &super::heap::Heap, ptr_id: HeapId) -> Option<Pico8Num> {
    match heap.get(ptr_id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
            // For vector, just use first value for partitioning
            nums.first().copied()
        }
        HeapValue::Value(Value::NumberInterval(MaybeVector::Scalar(interval))) => {
            // Use midpoint for intervals
            Some(interval.low)
        }
        HeapValue::Value(Value::NumberInterval(MaybeVector::Vector(intervals))) => {
            intervals.first().map(|i| i.low)
        }
        _ => None,
    }
}

/// A composite key for partitioning: shape + spatial position
#[derive(Clone, Debug)]
struct PartitionKey {
    shape_hash: u64,
    spatial: Option<SpatialKey>,
}

/// Hierarchical tree node for counting states
struct TreeNode {
    /// Count of states at this node
    count: usize,
    /// Children (for spatial subdivisions)
    children: Option<FxHashMap<SpatialKey, TreeNode>>,
}

impl TreeNode {
    fn new() -> Self {
        Self {
            count: 0,
            children: None,
        }
    }

    fn leaf(count: usize) -> Self {
        Self {
            count,
            children: None,
        }
    }
}

/// Partition states into blocks using hierarchical tree
pub fn partition_states_into_blocks(
    states: Vec<State>,
    config: &ParallelBlockConfig,
) -> Vec<Vec<State>> {
    let _trace = TraceSpan::new("partition_states", "parallel");

    if states.is_empty() {
        return vec![];
    }

    if states.len() <= config.min_block_size || config.num_blocks <= 1 {
        return vec![states];
    }

    // Step 1: Compute shape + spatial key for each state
    let states_with_keys: Vec<(State, u64, Option<SpatialKey>)> = states
        .into_iter()
        .map(|state| {
            let shape = shape_of_state(&state);
            let shape_hash = shape.cached_hash();
            let spatial = extract_player_position(&state);
            (state, shape_hash, spatial)
        })
        .collect();

    // Step 2: Group by shape first
    let mut by_shape: FxHashMap<u64, Vec<(State, Option<SpatialKey>)>> = FxHashMap::default();
    for (state, shape_hash, spatial) in states_with_keys {
        by_shape.entry(shape_hash).or_default().push((state, spatial));
    }

    // Step 3: For each shape group, build spatial tree and find cut points
    let target_block_size = (by_shape.values().map(|v| v.len()).sum::<usize>() + config.num_blocks - 1)
        / config.num_blocks;
    let target_block_size = target_block_size.max(config.min_block_size);

    let mut blocks: Vec<Vec<State>> = Vec::new();

    for (_shape_hash, shape_states) in by_shape {
        // If this shape group is small enough, it's one block
        if shape_states.len() <= target_block_size {
            blocks.push(shape_states.into_iter().map(|(s, _)| s).collect());
            continue;
        }

        // Build spatial tree for this shape group
        // Group by spatial key at various levels
        let mut level = 0u32;
        let max_level = 10; // Max 2^10 = 1024 pixel granularity

        // Start with the finest level and coarsen until we have right number of groups
        let mut current_groups: FxHashMap<Option<SpatialKey>, Vec<State>> = FxHashMap::default();
        for (state, spatial) in shape_states {
            let key = spatial.as_ref().map(|s| s.at_level(level));
            current_groups.entry(key).or_default().push(state);
        }

        // Coarsen until groups are large enough or we're at max level
        while current_groups.len() > config.num_blocks && level < max_level {
            level += 1;
            let mut new_groups: FxHashMap<Option<SpatialKey>, Vec<State>> = FxHashMap::default();
            for (key, states) in current_groups {
                let coarser_key = key.map(|k| k.at_level(1)); // One level coarser
                new_groups.entry(coarser_key).or_default().extend(states);
            }
            current_groups = new_groups;
        }

        // Now split any groups that are too large
        for (_key, mut group_states) in current_groups {
            while group_states.len() > target_block_size * 2 {
                let block: Vec<State> = group_states.drain(..target_block_size).collect();
                blocks.push(block);
            }
            if !group_states.is_empty() {
                blocks.push(group_states);
            }
        }
    }

    blocks
}

/// Slice a vectorized state into smaller pieces.
/// Each slice will have at most `max_size` elements.
/// Uses the same filter_by_mask mechanism as control flow branching.
fn slice_vectorized_state(state: &State, max_size: usize) -> Vec<State> {
    let vector_size = state.vector_size;

    if vector_size <= max_size {
        return vec![state.clone()];
    }

    // Calculate number of slices needed
    let num_slices = (vector_size + max_size - 1) / max_size;
    let mut slices = Vec::with_capacity(num_slices);

    for i in 0..num_slices {
        let start = i * max_size;
        let end = ((i + 1) * max_size).min(vector_size);
        let slice_size = end - start;

        // Create a mask for this slice (same as branching uses)
        let mask: Vec<bool> = (0..vector_size)
            .map(|idx| idx >= start && idx < end)
            .collect();

        // Filter the state by this mask (same operation as control flow branching)
        let sliced_state = state.filter_by_mask_clone(&mask);
        debug_assert_eq!(sliced_state.vector_size, slice_size);
        slices.push(sliced_state);
    }

    slices
}

/// Slice all vectorized states into smaller pieces for parallel processing.
fn slice_states_for_parallelism(states: Vec<State>, num_blocks: usize) -> Vec<State> {
    let _trace = TraceSpan::new("slice_states", "parallel");

    // Calculate total expanded size
    let total_expanded: usize = states.iter().map(|s| s.vector_size).sum();

    if total_expanded == 0 || num_blocks <= 1 {
        return states;
    }

    // Target size per slice
    let target_size = (total_expanded + num_blocks - 1) / num_blocks;
    let target_size = target_size.max(16); // Don't create tiny slices

    // Slice all states
    states
        .iter()
        .flat_map(|state| slice_vectorized_state(state, target_size))
        .collect()
}

/// Process a frame in parallel by slicing vectorized input states
///
/// Strategy:
/// 1. Slice large vectorized states into smaller pieces (using filter_by_mask)
/// 2. Process each slice in parallel
/// 3. GC and vectorize results
pub fn process_frame_parallel<F>(
    states: Vec<State>,
    config: &ParallelBlockConfig,
    process_state: F,
) -> Vec<State>
where
    F: Fn(State) -> Vec<State> + Sync,
{
    let _trace = TraceSpan::new("process_frame_parallel", "parallel");

    if states.is_empty() {
        return vec![];
    }

    // Step 1: Slice large vectorized states into smaller pieces
    let sliced_states = slice_states_for_parallelism(states, config.num_blocks);
    let num_slices = sliced_states.len();

    println!("  [parallel] sliced into {} pieces", num_slices);

    if num_slices <= 1 {
        // Single slice, process sequentially
        let state = sliced_states.into_iter().next().unwrap();
        let mut results: Vec<State> = process_state(state);
        for state in &mut results {
            state.gc();
        }
        return vectorize_states(results);
    }

    // Step 2: Process slices in parallel
    let block_results: Vec<Vec<State>> = {
        let _trace_parallel = TraceSpan::new("parallel_interpret", "parallel");

        sliced_states
            .into_par_iter()
            .enumerate()
            .map(|(i, state): (usize, State)| {
                let _trace_block = TraceSpan::new("interpret_slice", "parallel");
                println!("    [slice {}] vector_size={}", i, state.vector_size);

                // Process this slice
                let mut block_output: Vec<State> = process_state(state);

                // GC within block
                block_output.iter_mut().for_each(|s| s.gc());

                // Vectorize within block
                vectorize_states(block_output)
            })
            .collect()
    };

    // Step 3: Merge all block results
    let mut all_results: Vec<State> = {
        let _trace_merge = TraceSpan::new("merge_block_results", "parallel");
        block_results.into_iter().flatten().collect()
    };

    // Final vectorization across all blocks
    let _trace_final_vec = TraceSpan::new("final_vectorize", "parallel");
    all_results.iter_mut().for_each(|s: &mut State| s.gc());
    vectorize_states(all_results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spatial_key_levels() {
        let key = SpatialKey { x: 100, y: 50 };

        // Level 0 = exact
        assert_eq!(key.at_level(0), SpatialKey { x: 100, y: 50 });

        // Level 1 = 2x2 blocks
        assert_eq!(key.at_level(1), SpatialKey { x: 50, y: 25 });

        // Level 2 = 4x4 blocks
        assert_eq!(key.at_level(2), SpatialKey { x: 25, y: 12 });

        // Level 3 = 8x8 blocks
        assert_eq!(key.at_level(3), SpatialKey { x: 12, y: 6 });
    }

    #[test]
    fn test_partition_empty() {
        let config = ParallelBlockConfig::default();
        let blocks = partition_states_into_blocks(vec![], &config);
        assert!(blocks.is_empty());
    }

    #[test]
    fn test_slice_vectorized_state() {
        use crate::interpreter::heap::HeapId;
        use crate::interpreter::value::{MaybeVector, Value};
        use crate::pico8_num::Pico8Num;

        // Create a state with vector_size = 10
        let mut state = State::new();
        state.vector_size = 10;

        // Add a vector number to the heap
        let nums: Vec<Pico8Num> = (0..10).map(|i| Pico8Num::from_i16(i)).collect();
        let id = state.heap.alloc();
        state.heap.set(id, HeapValue::Value(Value::Number(MaybeVector::Vector(nums))));
        state.global_env.insert("test".to_string(), id);

        // Slice into 2 pieces
        let slices = slice_vectorized_state(&state, 5);
        assert_eq!(slices.len(), 2);

        // First slice should have elements 0-4
        assert_eq!(slices[0].vector_size, 5);
        let val0 = slices[0].heap.get(*slices[0].global_env.get("test").unwrap());
        match val0 {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 5);
                for i in 0..5 {
                    assert_eq!(nums[i], Pico8Num::from_i16(i as i16));
                }
            }
            _ => panic!("Expected vector number"),
        }

        // Second slice should have elements 5-9
        assert_eq!(slices[1].vector_size, 5);
        let val1 = slices[1].heap.get(*slices[1].global_env.get("test").unwrap());
        match val1 {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 5);
                for i in 0..5 {
                    assert_eq!(nums[i], Pico8Num::from_i16((i + 5) as i16));
                }
            }
            _ => panic!("Expected vector number"),
        }
    }

    #[test]
    fn test_slice_preserves_scalars() {
        use crate::interpreter::heap::HeapId;
        use crate::interpreter::value::{MaybeVector, Value};
        use crate::pico8_num::Pico8Num;

        // Create a state with vector_size = 10 but some scalar values
        let mut state = State::new();
        state.vector_size = 10;

        // Add a scalar number (same value for all lanes)
        let scalar_id = state.heap.alloc();
        state.heap.set(scalar_id, HeapValue::Value(Value::Number(
            MaybeVector::Scalar(Pico8Num::from_i16(42))
        )));
        state.global_env.insert("scalar".to_string(), scalar_id);

        // Add a vector number (different values per lane)
        let vector_id = state.heap.alloc();
        let nums: Vec<Pico8Num> = (0..10).map(|i| Pico8Num::from_i16(i)).collect();
        state.heap.set(vector_id, HeapValue::Value(Value::Number(MaybeVector::Vector(nums))));
        state.global_env.insert("vector".to_string(), vector_id);

        // Slice into 2 pieces
        let slices = slice_vectorized_state(&state, 5);
        assert_eq!(slices.len(), 2);

        // Check that scalar is preserved in both slices
        for (i, slice) in slices.iter().enumerate() {
            let scalar_val = slice.heap.get(*slice.global_env.get("scalar").unwrap());
            match scalar_val {
                HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => {
                    assert_eq!(*n, Pico8Num::from_i16(42), "Slice {} scalar mismatch", i);
                }
                _ => panic!("Slice {} - Expected scalar number, got {:?}", i, scalar_val),
            }
        }
    }
}
