//! Runner: executes multiple states through a CFG using symbolic tracing.
//!
//! This orchestrates the execution of many states, managing:
//! - Path exploration (using PathCounter)
//! - Cache lookups and insertions
//! - Tracking which states need more exploration
//!
//! Key design: NO VECTORIZATION. Every state has vector_size = 1.
//! Efficiency comes from cache hits, not from merging states.

use std::collections::{HashSet, VecDeque};
use anyhow::Result;

use crate::interpreter::{
    fixed_env::FixedEnv,
    heap::HeapId,
    state::State,
    value::{HeapValue, MaybeVector, Value},
    vectorize::{StateShape, debug_shape_of_state},
};
use crate::ir::Cfg;

use super::{PathCounter, TraceCache, TracingInterpreter};

/// Split a vectorized state (vector_size > 1) into individual scalar states.
/// Each returned state has vector_size = 1.
fn split_vectorized_state(state: State) -> Vec<State> {
    if state.vector_size <= 1 {
        return vec![state];
    }

    let n = state.vector_size;
    let mut result = Vec::with_capacity(n);

    for idx in 0..n {
        let mut new_state = State {
            heap: state.heap.clone(),
            local_env: state.local_env.clone(),
            outer_local_envs: state.outer_local_envs.clone(),
            global_env: state.global_env.clone(),
            prints: state.prints.clone(),
            vector_size: 1,
        };

        // Extract element at index `idx` from all vectors
        new_state.map_values_in_place(|v| extract_scalar_at_index(v, idx));

        // Also process heap values
        for i in 0..new_state.heap.len() {
            let id = HeapId::from_raw(i);
            if let Some(heap_value) = new_state.heap.get_opt(id) {
                let new_heap_value = match heap_value {
                    HeapValue::Value(v) => HeapValue::Value(extract_scalar_at_index(v.clone(), idx)),
                    HeapValue::Closure(name, captures) => {
                        let new_captures: Vec<Value> = captures.iter()
                            .map(|v| extract_scalar_at_index(v.clone(), idx))
                            .collect();
                        HeapValue::Closure(name.clone(), new_captures)
                    }
                    other => other.clone(),
                };
                new_state.heap.set(id, new_heap_value);
            }
        }

        result.push(new_state);
    }

    result
}

/// Extract the scalar value at index `idx` from a potentially vectorized value.
fn extract_scalar_at_index(value: Value, idx: usize) -> Value {
    match value {
        Value::Number(MaybeVector::Vector(nums)) => {
            Value::Number(MaybeVector::Scalar(nums[idx]))
        }
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            Value::NumberInterval(MaybeVector::Scalar(intervals[idx]))
        }
        Value::Bool(MaybeVector::Vector(bools)) => {
            Value::Bool(MaybeVector::Scalar(bools[idx]))
        }
        // Already scalar or non-vectorizable - return as-is
        other => other,
    }
}

/// Statistics about a run.
#[derive(Clone, Debug, Default)]
pub struct RunStats {
    /// Total states processed (including re-explorations).
    pub states_processed: usize,
    /// Number of new traces recorded.
    pub new_traces: usize,
    /// Number of cache hits.
    pub cache_hits: usize,
    /// Number of cache misses.
    pub cache_misses: usize,
    /// Number of forced choices made.
    pub forced_choices: usize,
    /// Number of output states produced.
    pub output_states: usize,
    /// Number of unique (shape, path) pairs seen
    pub unique_shape_paths: usize,
    /// Number of potential cache hits (same shape+path seen again)
    pub potential_cache_hits: usize,
}

impl RunStats {
    /// Merge two RunStats by adding their fields.
    pub fn merge(&self, other: &RunStats) -> RunStats {
        RunStats {
            states_processed: self.states_processed + other.states_processed,
            new_traces: self.new_traces + other.new_traces,
            cache_hits: self.cache_hits + other.cache_hits,
            cache_misses: self.cache_misses + other.cache_misses,
            forced_choices: self.forced_choices + other.forced_choices,
            output_states: self.output_states + other.output_states,
            unique_shape_paths: self.unique_shape_paths + other.unique_shape_paths,
            potential_cache_hits: self.potential_cache_hits + other.potential_cache_hits,
        }
    }
}

/// Pending work item: a state with its exploration counter.
#[derive(Clone)]
struct PendingState {
    state: State,
    path: PathCounter,
    shape: StateShape,
}

/// Runs multiple states through a CFG using symbolic tracing.
pub fn run_traced(
    cfg: &Cfg,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    cache: &mut TraceCache,
) -> Result<(Vec<State>, RunStats)> {
    let mut stats = RunStats::default();
    let mut output_states: Vec<State> = Vec::new();

    // Track seen (shape, path) pairs for potential cache hit analysis
    let mut seen_shape_paths: HashSet<(StateShape, Vec<(usize, usize)>)> = HashSet::new();

    // Queue of states to process
    let mut pending: VecDeque<PendingState> = VecDeque::new();

    // Initialize with input states - SPLIT any vectorized states first
    for state in input_states {
        // Split vectorized states into individual concrete states
        let concrete_states = split_vectorized_state(state);
        for concrete_state in concrete_states {
            debug_assert_eq!(concrete_state.vector_size, 1, "State should be concrete after splitting");
            let shape = debug_shape_of_state(&concrete_state);
            pending.push_back(PendingState {
                state: concrete_state,
                path: PathCounter::new(),
                shape,
            });
        }
    }

    // Process states
    while let Some(pending_state) = pending.pop_front() {
        stats.states_processed += 1;

        // Track potential cache hits
        let shape_path_key = (pending_state.shape.clone(), pending_state.path.choices().to_vec());
        if seen_shape_paths.contains(&shape_path_key) {
            stats.potential_cache_hits += 1;
        } else {
            seen_shape_paths.insert(shape_path_key);
            stats.unique_shape_paths += 1;
        }

        // CACHE DISABLED - Fundamental design limitation:
        //
        // The cache key (shape, path) is insufficient to guarantee identical execution.
        // The 'path' only records ABSTRACT branches (UnknownBool conditions), but
        // CONCRETE branches (based on actual values like `x > 5`) are not tracked.
        //
        // Two states with the same shape and path can take different concrete branches
        // if their actual values differ. This leads to:
        // 1. Different output heap structures
        // 2. Different types in symbolic expressions (e.g., Number vs Pointer)
        // 3. Type errors when applying cached expressions to different states
        //
        // To fix properly, we'd need to either:
        // - Track ALL branches (concrete + abstract) in the path
        // - Use path conditions that cover concrete comparisons
        // - Only cache when the traced path has NO concrete branches
        let cache_hit = false;

        if cache_hit {
            let cached = cache.get(&pending_state.shape, &pending_state.path).unwrap();
            stats.cache_hits += 1;
            stats.forced_choices += cached.forced_choices;

            // Apply the cached trace to the new input state
            let output = cached.apply(&pending_state.state);
            output_states.push(output);
            stats.output_states += 1;

            // If there were forced choices on this path, explore the next path
            if cached.forced_choices > 0 {
                let mut next_path = cached.path.clone();
                if next_path.increment() {
                    pending.push_back(PendingState {
                        state: pending_state.state,
                        path: next_path,
                        shape: pending_state.shape,
                    });
                }
            }
            continue;
        }
        stats.cache_misses += 1;

        // Execute with tracing
        let tracer = TracingInterpreter::new(fixed_env, Some(pending_state.path.clone()));
        let result = tracer.interpret(cfg, pending_state.state.clone())?;

        stats.forced_choices += result.forced_choices;

        // Debug: show what path was discovered (disabled)
        // if result.forced_choices > 0 || pending_state.path.choices().is_empty() {
        //     eprintln!("DEBUG: Traced path {:?} -> {:?} (forced={})",
        //               pending_state.path.choices(), result.path.choices(), result.forced_choices);
        // }

        // Cache the result with symbolic information
        cache.insert(
            pending_state.shape.clone(),
            result.path.clone(),
            result.forced_choices,
            result.output_state.clone(),
            result.heap_symbols,
            result.input_symbols,
        );
        stats.new_traces += 1;

        // Add output to results - should always be concrete
        debug_assert_eq!(result.output_state.vector_size, 1, "Output state should be concrete");
        output_states.push(result.output_state);
        stats.output_states += 1;

        // If there were forced choices, we need to explore other paths
        if result.forced_choices > 0 {
            let mut next_path = result.path;
            if next_path.increment() {
                // More paths to explore
                pending.push_back(PendingState {
                    state: pending_state.state,
                    path: next_path,
                    shape: pending_state.shape,
                });
            }
        }
    }

    Ok((output_states, stats))
}

/// Runs a single frame (game loop iteration) using symbolic tracing.
pub fn run_frame_traced(
    frame_cfg: &Cfg,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    cache: &mut TraceCache,
) -> Result<(Vec<State>, RunStats)> {
    run_traced(frame_cfg, input_states, fixed_env, cache)
}
