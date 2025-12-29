//! Runner: executes multiple states through a CFG using symbolic tracing.
//!
//! This orchestrates the execution of many states, managing:
//! - Path exploration (using PathCounter)
//! - Cache lookups and insertions
//! - Tracking which states need more exploration
//!
//! Key design: NO VECTORIZATION. Every state has vector_size = 1.
//! Efficiency comes from cache hits, not from merging states.

use std::collections::VecDeque;
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

        // Try cache lookup
        if let Some(cached) = cache.get(&pending_state.shape, &pending_state.path) {
            stats.cache_hits += 1;
            // For now, just use the cached output directly
            // TODO: In Phase 3, substitute symbolic values
            output_states.push(cached.output_state.clone());
            continue;
        }
        stats.cache_misses += 1;

        // Execute with tracing
        let tracer = TracingInterpreter::new(fixed_env, Some(pending_state.path.clone()));
        let result = tracer.interpret(cfg, pending_state.state.clone())?;

        stats.forced_choices += result.forced_choices;

        // Cache the result
        cache.insert(
            pending_state.shape.clone(),
            result.path.clone(),
            result.output_state.clone(),
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
