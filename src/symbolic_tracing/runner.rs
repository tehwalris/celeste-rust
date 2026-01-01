//! Runner: executes multiple states through a CFG using symbolic tracing.
//!
//! This orchestrates the execution of many states, managing:
//! - Path exploration (using PathCounter)
//! - Cache lookups (checking path conditions for cache hits)
//! - Cache insertions (storing traces for reuse)
//! - Tracking which states need more exploration
//!
//! ## Caching Strategy
//!
//! The cache maps (shape, abstract_path) to a list of traces. Each trace includes
//! path conditions (symbolic boolean expressions that were true during tracing).
//! When looking up, we check if the input state satisfies any cached trace's
//! path conditions. If so, we can reuse the trace instead of re-executing.

use std::collections::{HashSet, VecDeque};
use std::sync::Arc;
use anyhow::Result;
use rayon::prelude::*;
use rustc_hash::FxHashMap;

use crate::interpreter::{
    fixed_env::FixedEnv,
    heap::HeapId,
    state::State,
    value::{HeapValue, MaybeVector, Value},
    vectorize::{StateShape, debug_shape_of_state},
};
use crate::ir::Cfg;

use super::{CachedTrace, PathCounter, TraceCache, TracingInterpreter};

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
    /// Number of traces with no concrete branches (reusable).
    pub reusable_traces: usize,
    /// Number of traces with concrete branches (not reusable).
    pub unreusable_traces: usize,
    /// Total concrete branches across all traces (for analysis)
    pub total_concrete_branches: usize,
    /// Number of unique (shape, abstract_path, concrete_path) tuples
    pub unique_full_paths: usize,
    /// Number of template mismatches (different concrete branch counts for same template key)
    pub template_mismatches: usize,
    /// Total function calls across all traces
    pub total_function_calls: usize,
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
            reusable_traces: self.reusable_traces + other.reusable_traces,
            unreusable_traces: self.unreusable_traces + other.unreusable_traces,
            total_concrete_branches: self.total_concrete_branches + other.total_concrete_branches,
            unique_full_paths: self.unique_full_paths + other.unique_full_paths,
            template_mismatches: self.template_mismatches + other.template_mismatches,
            total_function_calls: self.total_function_calls + other.total_function_calls,
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
    cfg: Arc<Cfg>,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    cache: &mut TraceCache,
) -> Result<(Vec<State>, RunStats)> {
    let mut stats = RunStats::default();
    let mut output_states: Vec<State> = Vec::new();

    // Track seen (shape, path) pairs for potential cache hit analysis
    let mut seen_shape_paths: HashSet<(StateShape, Vec<(usize, usize)>)> = HashSet::new();
    // Track seen (shape, abstract_path, concrete_path) tuples
    let mut seen_full_paths: HashSet<(StateShape, Vec<(usize, usize)>, Vec<bool>)> = HashSet::new();

    // Queue of states to process
    let mut pending: VecDeque<PendingState> = VecDeque::new();

    // Initialize with input states - SPLIT any vectorized states first
    for state in input_states {
        // Split vectorized states into individual concrete states
        let concrete_states = split_vectorized_state(state);
        for mut concrete_state in concrete_states {
            debug_assert_eq!(concrete_state.vector_size, 1, "State should be concrete after splitting");
            // Freeze the heap before tracing to make get_opt fast (Vec lookup instead of HashMap)
            concrete_state.heap.freeze();
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

        // NOTE: Template-based caching is disabled because branches are path-dependent.
        // Different traces with the same (shape, pending_path) can have different numbers of
        // concrete branches, making templates invalid.
        // TODO: Consider alternative caching strategies or focus on tracing performance.

        // Always trace (cache disabled)
        stats.cache_misses += 1;

        // Execute with tracing (fast mode - no symbolic tracking since cache is disabled)
        let tracer = TracingInterpreter::new_fast(fixed_env, Some(pending_state.path.clone()));
        let result = tracer.interpret(cfg.clone(), pending_state.state.clone())?;

        stats.forced_choices += result.forced_choices;
        stats.new_traces += 1;
        stats.total_concrete_branches += result.concrete_branches;
        stats.total_function_calls += result.function_calls;
        if result.concrete_branches == 0 {
            stats.reusable_traces += 1;
        } else {
            stats.unreusable_traces += 1;
        }

        // Track unique full paths (shape + abstract_path + concrete_path)
        let full_path_key = (
            pending_state.shape.clone(),
            result.path.choices().to_vec(),
            result.concrete_path.clone(),
        );
        if !seen_full_paths.contains(&full_path_key) {
            seen_full_paths.insert(full_path_key);
            stats.unique_full_paths += 1;
        }

        // NOTE: Cache insert disabled - see note above about path-dependent branches

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
    frame_cfg: Arc<Cfg>,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    cache: &mut TraceCache,
) -> Result<(Vec<State>, RunStats)> {
    run_traced(frame_cfg, input_states, fixed_env, cache)
}

/// Result of tracing a single state (used for parallel processing).
struct TraceResult {
    output_state: State,
    forced_choices: usize,
    concrete_branches: usize,
    function_calls: usize,
    path: PathCounter,
    concrete_path: Vec<bool>,
    /// If forced_choices > 0, this contains the next path and original state for re-exploration
    next_exploration: Option<(State, PathCounter, StateShape)>,
}

/// Runs multiple states through a CFG using symbolic tracing, with parallel execution.
pub fn run_traced_parallel(
    cfg: Arc<Cfg>,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    _cache: &mut TraceCache, // Cache unused but kept for API compatibility
) -> Result<(Vec<State>, RunStats)> {
    let mut stats = RunStats::default();
    let mut output_states: Vec<State> = Vec::new();

    // Track seen (shape, path) pairs for potential cache hit analysis
    let mut seen_shape_paths: HashSet<(StateShape, Vec<(usize, usize)>)> = HashSet::new();
    // Track seen (shape, abstract_path, concrete_path) tuples
    let mut seen_full_paths: HashSet<(StateShape, Vec<(usize, usize)>, Vec<bool>)> = HashSet::new();

    // Initialize pending states
    let mut pending: Vec<PendingState> = Vec::new();
    for state in input_states {
        let concrete_states = split_vectorized_state(state);
        for mut concrete_state in concrete_states {
            debug_assert_eq!(concrete_state.vector_size, 1, "State should be concrete after splitting");
            concrete_state.heap.freeze();
            let shape = debug_shape_of_state(&concrete_state);
            pending.push(PendingState {
                state: concrete_state,
                path: PathCounter::new(),
                shape,
            });
        }
    }

    // Process in batches using parallel execution
    while !pending.is_empty() {
        // Take current batch
        let batch: Vec<_> = std::mem::take(&mut pending);
        let batch_size = batch.len();

        // Update stats for potential cache hits (before parallel processing)
        for ps in &batch {
            let shape_path_key = (ps.shape.clone(), ps.path.choices().to_vec());
            if seen_shape_paths.contains(&shape_path_key) {
                stats.potential_cache_hits += 1;
            } else {
                seen_shape_paths.insert(shape_path_key);
                stats.unique_shape_paths += 1;
            }
        }
        stats.states_processed += batch_size;
        stats.cache_misses += batch_size;

        // Process batch in parallel (fast mode - no symbolic tracking since cache is disabled)
        let results: Vec<Result<TraceResult>> = batch
            .into_par_iter()
            .map(|pending_state| {
                let tracer = TracingInterpreter::new_fast(fixed_env, Some(pending_state.path.clone()));
                let result = tracer.interpret(cfg.clone(), pending_state.state.clone())?;

                // Compute next exploration if needed
                let next_exploration = if result.forced_choices > 0 {
                    let mut next_path = result.path.clone();
                    if next_path.increment() {
                        Some((pending_state.state, next_path, pending_state.shape))
                    } else {
                        None
                    }
                } else {
                    None
                };

                Ok(TraceResult {
                    output_state: result.output_state,
                    forced_choices: result.forced_choices,
                    concrete_branches: result.concrete_branches,
                    function_calls: result.function_calls,
                    path: result.path,
                    concrete_path: result.concrete_path,
                    next_exploration,
                })
            })
            .collect();

        // Collect results and queue new work
        for result in results {
            let result = result?;

            stats.forced_choices += result.forced_choices;
            stats.new_traces += 1;
            stats.total_concrete_branches += result.concrete_branches;
            stats.total_function_calls += result.function_calls;

            if result.concrete_branches == 0 {
                stats.reusable_traces += 1;
            } else {
                stats.unreusable_traces += 1;
            }

            // Track unique full paths
            // Note: We rebuild the shape from next_exploration if available,
            // or compute it from output_state
            let shape = result.next_exploration.as_ref()
                .map(|(_, _, s)| s.clone())
                .unwrap_or_else(|| debug_shape_of_state(&result.output_state));

            let full_path_key = (
                shape,
                result.path.choices().to_vec(),
                result.concrete_path,
            );
            if !seen_full_paths.contains(&full_path_key) {
                seen_full_paths.insert(full_path_key);
                stats.unique_full_paths += 1;
            }

            debug_assert_eq!(result.output_state.vector_size, 1, "Output state should be concrete");
            output_states.push(result.output_state);
            stats.output_states += 1;

            // Queue next exploration if needed
            if let Some((state, path, shape)) = result.next_exploration {
                pending.push(PendingState { state, path, shape });
            }
        }
    }

    Ok((output_states, stats))
}

/// Cache key for simple (shape, abstract_path) caching.
/// Since concrete_branches = 0, this uniquely identifies a trace.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct SimpleCacheKey {
    shape: StateShape,
    abstract_path: Vec<(usize, usize)>,
}

/// Runs multiple states through a CFG using symbolic tracing with full path caching.
///
/// This version caches by (shape, abstract_path, concrete_path) using the template
/// system to compute concrete_path for new states.
pub fn run_traced_parallel_cached(
    cfg: Arc<Cfg>,
    input_states: Vec<State>,
    fixed_env: &FixedEnv,
    cache: &mut TraceCache,
) -> Result<(Vec<State>, RunStats)> {
    let mut stats = RunStats::default();
    let mut output_states: Vec<State> = Vec::new();

    // Track seen (shape, path) pairs for statistics
    let mut seen_shape_paths: HashSet<(StateShape, Vec<(usize, usize)>)> = HashSet::new();
    let mut seen_full_paths: HashSet<(StateShape, Vec<(usize, usize)>, Vec<bool>)> = HashSet::new();

    // Initialize pending states
    let mut pending: Vec<PendingState> = Vec::new();
    for state in input_states {
        let concrete_states = split_vectorized_state(state);
        for mut concrete_state in concrete_states {
            debug_assert_eq!(concrete_state.vector_size, 1, "State should be concrete after splitting");
            concrete_state.heap.freeze();
            let shape = debug_shape_of_state(&concrete_state);
            pending.push(PendingState {
                state: concrete_state,
                path: PathCounter::new(),
                shape,
            });
        }
    }

    // Process states one at a time (to maintain cache consistency)
    while !pending.is_empty() {
        let batch: Vec<_> = std::mem::take(&mut pending);
        let batch_size = batch.len();

        // Track stats for potential cache hits
        for ps in &batch {
            let shape_path_key = (ps.shape.clone(), ps.path.choices().to_vec());
            if seen_shape_paths.contains(&shape_path_key) {
                stats.potential_cache_hits += 1;
            } else {
                seen_shape_paths.insert(shape_path_key);
                stats.unique_shape_paths += 1;
            }
        }
        stats.states_processed += batch_size;

        // Check cache for each state
        let mut cache_hits: Vec<(PendingState, CachedTrace)> = Vec::new();
        let mut cache_misses: Vec<PendingState> = Vec::new();

        for ps in batch {
            // Try to look up using full path cache
            let lookup_result = cache.get_full_path_debug(&ps.shape, &ps.path, &ps.state);
            if let Some(trace) = lookup_result.trace {
                cache_hits.push((ps, trace.clone()));
            } else {
                cache_misses.push(ps);
            }
        }

        stats.cache_hits += cache_hits.len();
        stats.cache_misses += cache_misses.len();

        // Process cache hits: apply cached trace to get output
        for (ps, trace) in cache_hits {
            let output_state = trace.apply(&ps.state);
            debug_assert_eq!(output_state.vector_size, 1, "Output state should be concrete");
            output_states.push(output_state);
            stats.output_states += 1;

            // If the cached trace had forced choices, we need to explore other paths
            if trace.forced_choices > 0 {
                let mut next_path = trace.path.clone();
                if next_path.increment() {
                    pending.push(PendingState {
                        state: ps.state,
                        path: next_path,
                        shape: ps.shape,
                    });
                }
            }
        }

        // Process cache misses in parallel (with symbol tracking for caching)
        if !cache_misses.is_empty() {
            let results: Vec<Result<TraceResultWithCache>> = cache_misses
                .into_par_iter()
                .map(|pending_state| {
                    // Use symbolic tracking mode for caching
                    let tracer = TracingInterpreter::new(fixed_env, Some(pending_state.path.clone()));
                    let result = tracer.interpret(cfg.clone(), pending_state.state.clone())?;

                    // Create cached trace for later reuse
                    let cached_trace = CachedTrace {
                        path: result.path.clone(),
                        concrete_path: result.concrete_path.clone(),
                        forced_choices: result.forced_choices,
                        concrete_branches: result.concrete_branches,
                        output_state: result.output_state.clone(),
                        heap_symbols: result.heap_symbols,
                        input_symbols: result.input_symbols,
                        path_conditions: result.path_conditions,
                        concrete_branch_conditions: result.concrete_branch_conditions,
                        allocated_heap_ids: result.allocated_heap_ids,
                    };

                    // Compute next exploration if needed
                    let next_exploration = if result.forced_choices > 0 {
                        let mut next_path = result.path.clone();
                        if next_path.increment() {
                            Some((pending_state.state, next_path, pending_state.shape.clone()))
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    Ok(TraceResultWithCache {
                        output_state: result.output_state,
                        forced_choices: result.forced_choices,
                        concrete_branches: result.concrete_branches,
                        function_calls: result.function_calls,
                        path: result.path,
                        concrete_path: result.concrete_path,
                        next_exploration,
                        cache_key: SimpleCacheKey {
                            shape: pending_state.shape.clone(),
                            abstract_path: pending_state.path.choices().to_vec(),
                        },
                        shape: pending_state.shape,
                        abstract_path: pending_state.path,
                        cached_trace,
                    })
                })
                .collect();

            // Collect results and update cache
            for result in results {
                let result = result?;

                stats.forced_choices += result.forced_choices;
                stats.new_traces += 1;
                stats.total_concrete_branches += result.concrete_branches;
                stats.total_function_calls += result.function_calls;

                if result.concrete_branches == 0 {
                    stats.reusable_traces += 1;
                } else {
                    stats.unreusable_traces += 1;
                }

                // Track unique full paths
                let shape = result.next_exploration.as_ref()
                    .map(|(_, _, s)| s.clone())
                    .unwrap_or(result.cache_key.shape.clone());

                let full_path_key = (
                    shape,
                    result.path.choices().to_vec(),
                    result.concrete_path,
                );
                if !seen_full_paths.contains(&full_path_key) {
                    seen_full_paths.insert(full_path_key);
                    stats.unique_full_paths += 1;
                }

                // Insert into full path cache
                cache.insert_full_path(result.shape, result.abstract_path, result.cached_trace);

                debug_assert_eq!(result.output_state.vector_size, 1, "Output state should be concrete");
                output_states.push(result.output_state);
                stats.output_states += 1;

                // Queue next exploration if needed
                if let Some((state, path, shape)) = result.next_exploration {
                    pending.push(PendingState { state, path, shape });
                }
            }
        }
    }

    Ok((output_states, stats))
}

/// Result of tracing with cache info for the cached parallel version.
struct TraceResultWithCache {
    output_state: State,
    forced_choices: usize,
    concrete_branches: usize,
    function_calls: usize,
    path: PathCounter,
    concrete_path: Vec<bool>,
    next_exploration: Option<(State, PathCounter, StateShape)>,
    cache_key: SimpleCacheKey,
    shape: StateShape,
    abstract_path: PathCounter,
    cached_trace: CachedTrace,
}
