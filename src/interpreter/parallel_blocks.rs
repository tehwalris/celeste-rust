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

/// Process a frame in parallel using output partitioning
///
/// This is the main entry point for parallel frame processing.
/// Strategy:
/// 1. Process input states sequentially (because vectorized states share heap structure)
/// 2. GC all output states in parallel (each state independently)
/// 3. Vectorize states sequentially (must compare shapes)
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

    // Step 1: Process input states sequentially to get output states
    // (Vectorized states share heap structure and can't be safely split)
    let output_states: Vec<State> = {
        let _trace_interpret = TraceSpan::new("interpret_states", "parallel");
        states.into_iter().flat_map(&process_state).collect()
    };

    let num_output = output_states.len();

    if num_output <= config.min_block_size {
        // Not enough states to parallelize - process sequentially
        let mut results = output_states;
        for state in &mut results {
            state.gc();
        }
        return vectorize_states(results);
    }

    // Step 2: GC all output states in parallel
    // Each state is independent after interpretation, so GC can run in parallel
    let gc_results: Vec<State> = {
        let _trace_parallel_gc = TraceSpan::new("parallel_gc", "parallel");

        output_states
            .into_par_iter()
            .map(|mut state: State| {
                state.gc();
                state
            })
            .collect()
    };

    // Step 3: Vectorize states (must be sequential to compare shapes correctly)
    let _trace_vectorize = TraceSpan::new("vectorize", "parallel");
    vectorize_states(gc_results)
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
}
