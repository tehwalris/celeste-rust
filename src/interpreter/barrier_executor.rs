//! Barrier-based execution: process states through barriers in order,
//! using PathCounter for abstract path enumeration.
//!
//! Key design: PathCounter is passed through ALL levels of function calls,
//! so the path enumeration works across the entire call stack.
//!
//! Optimizations:
//! - Early path merging: States are merged incrementally as paths complete
//! - Parallel lane processing: Vector lanes are processed in parallel using rayon
//! - Path deduplication: Duplicate states are detected and merged during enumeration

use std::collections::{BTreeMap, HashMap};

use anyhow::{anyhow, Result};
use rayon::prelude::*;

use crate::ir::{BarrierId, Block, Cfg, Instruction, Label, LocalId, Terminator};

use super::{
    fixed_env::FixedEnv,
    local_env::LocalEnv,
    state::State,
    value::{HeapValue, MaybeVector, Value},
    vectorize::{vectorize_states, normalize_state_for_comparison, NormalizedState},
};

/// Build a map from BarrierId to the block that has that barrier.
/// Returns None for entry block, Some(label) for named blocks.
fn build_barrier_map(cfg: &Cfg) -> HashMap<BarrierId, Option<Label>> {
    let mut map = HashMap::new();

    // Check entry block
    if let Some(barrier_id) = &cfg.entry.barrier {
        map.insert(barrier_id.clone(), None);
    }

    // Check named blocks
    for (label, block) in &cfg.named {
        if let Some(barrier_id) = &block.barrier {
            map.insert(barrier_id.clone(), Some(label.clone()));
        }
    }

    map
}

/// PathCounter tracks which abstract path we're exploring.
/// When encountering UnknownBool, we consult the counter to pick true/false.
/// After a run completes, we increment and try again until exhausted.
#[derive(Clone, Debug)]
pub struct PathCounter {
    /// Current path: bit i = true means take "true" branch for choice i
    current_path: u64,
    /// Number of choices made so far in this run
    choices_made: usize,
    /// Maximum choices seen in any path (for knowing when we're done)
    max_choices: usize,
}

impl PathCounter {
    pub fn new() -> Self {
        Self {
            current_path: 0,
            choices_made: 0,
            max_choices: 0,
        }
    }

    /// Get the choice for the next UnknownBool branch.
    /// Returns true or false based on the current path.
    pub fn get_choice(&mut self) -> bool {
        let choice = (self.current_path >> self.choices_made) & 1 == 1;
        self.choices_made += 1;
        self.max_choices = self.max_choices.max(self.choices_made);
        choice
    }

    /// Check if the choice at index `choice_idx` is true in current path
    pub fn peek_choice(&self, choice_idx: usize) -> bool {
        (self.current_path >> choice_idx) & 1 == 1
    }

    /// Reset for a new run (called at barrier)
    pub fn reset_for_new_run(&mut self) {
        self.choices_made = 0;
    }

    /// Try to increment to the next path.
    /// Returns true if there are more paths to explore, false if exhausted.
    pub fn increment(&mut self) -> bool {
        if self.max_choices == 0 {
            // No choices were made - only one path exists
            return false;
        }

        // Increment the path counter
        self.current_path += 1;

        // Check if we've exhausted all paths (2^max_choices)
        if self.current_path >= (1u64 << self.max_choices) {
            return false;
        }

        // Reset for new run
        self.choices_made = 0;
        true
    }

    /// Check if this path is still valid (haven't made impossible choices)
    pub fn is_valid(&self) -> bool {
        // A path becomes invalid if we've made more choices than the path encoding allows
        self.choices_made <= 64
    }
}

/// Execute a call instruction using PathCounter for all branching decisions.
/// This function is called by run_to_next_barrier for function calls.
/// PathCounter is passed through to nested function calls.
fn interpret_call_with_path_counter(
    state: State,
    local_id: LocalId,
    closure_local_id: LocalId,
    arg_local_ids: &[LocalId],
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
) -> Result<State> {
    // Get the closure value from the local environment
    let closure_heap_id = match state.local_env.get(closure_local_id) {
        Value::Pointer(heap_id) => *heap_id,
        Value::NilPointer(hint) => {
            return Err(anyhow!("Attempt to call nil ({})", hint));
        }
        _ => {
            return Err(anyhow!("Expected pointer for closure"));
        }
    };

    // Get the heap value
    let heap_value = state.heap.get(closure_heap_id).clone();

    // Gather argument values
    let arg_values: Vec<Value> = arg_local_ids
        .iter()
        .map(|id| state.local_env.get(*id).clone())
        .collect();

    match heap_value {
        HeapValue::BuiltinFun(name) => {
            // Look up the builtin function
            let builtin_fn = fixed_env
                .builtin_funs
                .get(&name)
                .ok_or_else(|| anyhow!("Unknown builtin function: {}", name))?;

            // Call the builtin, which returns multiple (state, return_value) pairs
            let results = builtin_fn(state, arg_values)?;

            if results.is_empty() {
                return Err(anyhow!("Builtin {} returned no results", name));
            }

            // Use PathCounter to pick one result
            let choice = if results.len() > 1 {
                path_counter.get_choice()
            } else {
                false
            };
            let idx = if choice { 1.min(results.len() - 1) } else { 0 };
            let (mut result_state, return_value) = results.into_iter().nth(idx).unwrap();

            // Set the return value
            result_state.local_env.set(local_id, return_value);
            Ok(result_state)
        }
        HeapValue::Closure(fun_def_name, captured_values) => {
            // Look up the function definition with prepared CFG
            let (fun_def, prepared_cfg) = fixed_env
                .fun_defs
                .get(&fun_def_name)
                .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;

            // Create a new local_env for the function body
            let mut new_local_env = LocalEnv::new();

            // Set up captured values
            for (capture_id, value) in fun_def.capture_ids.iter().zip(captured_values.iter()) {
                new_local_env.set(*capture_id, value.clone());
            }

            // Set up argument values (padding with Nil if needed)
            for (i, arg_id) in fun_def.arg_ids.iter().enumerate() {
                if let Some(arg_id) = arg_id {
                    let value = arg_values
                        .get(i)
                        .cloned()
                        .unwrap_or(Value::Nil(Some("missing argument".to_string())));
                    new_local_env.set(*arg_id, value);
                }
            }

            // Create the state for executing the function body
            let mut new_outer_local_envs = vec![state.local_env.clone()];
            new_outer_local_envs.extend(state.outer_local_envs.clone());

            let function_state = State {
                heap: state.heap.clone(),
                local_env: new_local_env,
                outer_local_envs: new_outer_local_envs,
                global_env: state.global_env.clone(),
                prints: state.prints.clone(),
                vector_size: state.vector_size,
            };

            // Recursively interpret the function's CFG using PathCounter
            // This runs the entire function and returns when it completes
            let (result_state, return_value) =
                run_cfg_to_completion(&prepared_cfg.cfg, function_state, fixed_env, path_counter)?;

            // Restore caller's local_env from outer_local_envs
            let (caller_local_env, remaining_outer_envs) = {
                let mut envs = result_state.outer_local_envs.clone();
                let caller_env = envs.remove(0);
                (caller_env, envs)
            };

            let mut caller_state = State {
                heap: result_state.heap,
                local_env: caller_local_env,
                outer_local_envs: remaining_outer_envs,
                global_env: result_state.global_env,
                prints: result_state.prints,
                vector_size: result_state.vector_size,
            };

            // Set the return value
            caller_state
                .local_env
                .set(local_id, return_value.unwrap_or(Value::Nil(None)));
            Ok(caller_state)
        }
        _ => Err(anyhow!("Expected closure or builtin function")),
    }
}

/// Run a CFG to completion using PathCounter for branching decisions.
/// Used for function calls - runs until a Return is encountered.
fn run_cfg_to_completion(
    cfg: &Cfg,
    mut state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
) -> Result<(State, Option<Value>)> {
    // Use a fake label for the entry block (needed for PHI nodes in successors)
    let entry_label = Label::from("__entry".to_string());

    let mut current_block = &cfg.entry;
    let mut current_block_label: Option<&Label> = None; // None = entry block
    let mut incoming_label: Option<&Label> = None; // Label of block we came FROM (for PHI)

    loop {
        // Execute all instructions in the block
        for (local_id, instruction) in &current_block.instructions {
            // Handle phi nodes - use incoming_label to pick the right branch
            if let Instruction::Phi { branches } = instruction {
                if let Some(from_label) = incoming_label {
                    for (branch_label, value_id) in branches {
                        if branch_label == from_label {
                            let value = state.local_env.get(*value_id).clone();
                            state.local_env.set(*local_id, value);
                            break;
                        }
                    }
                }
                continue;
            }

            // Handle calls with PathCounter
            if let Instruction::Call { closure, args } = instruction {
                state = interpret_call_with_path_counter(
                    state,
                    *local_id,
                    *closure,
                    args,
                    fixed_env,
                    path_counter,
                )?;
                continue;
            }

            // Handle other instructions using CoreInterpreter
            let mut interpreter =
                super::core_interpreter::CoreInterpreter::new(state, fixed_env);
            interpreter.interpret_non_call_instruction(*local_id, instruction)?;
            state = interpreter.into_state();
        }

        // Handle terminator
        let (_, terminator) = &current_block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| state.local_env.get(id).clone());
                return Ok((state, return_value));
            }
            Terminator::UnconditionalBranch { target } => {
                // Set incoming_label to current block's label (for PHI in target)
                incoming_label = current_block_label.or(Some(&entry_label));
                current_block_label = Some(target);
                current_block = cfg
                    .named
                    .get(target)
                    .ok_or_else(|| anyhow!("Unknown block label: {:?}", target))?;
            }
            Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } => {
                let condition_value = state.local_env.get(*condition);

                let take_true = match condition_value {
                    Value::Bool(MaybeVector::Scalar(b)) => *b,
                    Value::Bool(MaybeVector::Vector(_)) => {
                        panic!("Unexpected vector bool in scalar execution");
                    }
                    Value::UnknownBool => path_counter.get_choice(),
                    Value::Nil(_) => false,
                    Value::Number(_)
                    | Value::NumberInterval(_)
                    | Value::String(_)
                    | Value::Pointer(_) => true,
                    Value::NilPointer(_) => return Err(anyhow!("Nil pointer in condition")),
                };

                let target = if take_true { true_target } else { false_target };
                // Set incoming_label to current block's label (for PHI in target)
                incoming_label = current_block_label.or(Some(&entry_label));
                current_block_label = Some(target);
                current_block = cfg
                    .named
                    .get(target)
                    .ok_or_else(|| anyhow!("Unknown block label: {:?}", target))?;
            }
        }
    }
}

/// Key for ordering barrier processing: (barrier_id, hit_count)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BarrierKey {
    pub barrier_id: BarrierId,
    pub hit_count: usize,
}

/// A state waiting at a barrier, along with the block to start executing from.
#[derive(Clone, Debug)]
pub struct WaitingState {
    pub state: State,
    pub barrier_key: BarrierKey,
    /// The block to start executing from. None means entry block.
    pub start_block: Option<Label>,
}

/// The implicit "END" barrier for states that complete execution
pub fn end_barrier_id() -> BarrierId {
    BarrierId::new(vec![i32::MAX])
}

/// The implicit "START" barrier for initial states
pub fn start_barrier_id() -> BarrierId {
    BarrierId::new(vec![i32::MIN])
}

/// Result of running a scalar state from one barrier to the next
#[derive(Clone, Debug)]
pub struct BarrierRunResult {
    /// The output state
    pub state: State,
    /// The destination barrier (or END)
    pub destination: BarrierId,
    /// Optional return value (if this was a function call that returned)
    pub return_value: Option<Value>,
}

/// Accumulator for collecting states with early deduplication.
/// Uses normalized state comparison to detect and merge duplicate states.
struct StateAccumulator {
    /// States grouped by destination barrier
    states_by_dest: HashMap<BarrierId, Vec<State>>,
    /// Normalized state signatures for deduplication (per destination)
    seen_normalized: HashMap<BarrierId, std::collections::HashSet<NormalizedState>>,
    /// Count of states that were deduplicated (for stats)
    dedup_count: usize,
}

impl StateAccumulator {
    fn new() -> Self {
        Self {
            states_by_dest: HashMap::new(),
            seen_normalized: HashMap::new(),
            dedup_count: 0,
        }
    }

    /// Add a state to the accumulator with deduplication.
    /// Returns true if the state was added (new), false if it was a duplicate.
    fn add_state(&mut self, dest: BarrierId, mut state: State) -> bool {
        // GC the state first to get deterministic heap IDs
        state.gc();

        // Compute normalized form for deduplication
        let normalized = normalize_state_for_comparison(&state);

        // Check if we've seen this state before
        let seen_set = self.seen_normalized.entry(dest.clone()).or_default();
        if seen_set.contains(&normalized) {
            self.dedup_count += 1;
            return false;
        }

        // Add to seen set and states
        seen_set.insert(normalized);
        self.states_by_dest.entry(dest).or_default().push(state);
        true
    }

    /// Merge another accumulator into this one
    fn merge(&mut self, other: StateAccumulator) {
        for (dest, states) in other.states_by_dest {
            for state in states {
                // Re-check for duplicates when merging
                let normalized = normalize_state_for_comparison(&state);
                let seen_set = self.seen_normalized.entry(dest.clone()).or_default();
                if !seen_set.contains(&normalized) {
                    seen_set.insert(normalized);
                    self.states_by_dest.entry(dest.clone()).or_default().push(state);
                } else {
                    self.dedup_count += 1;
                }
            }
        }
        self.dedup_count += other.dedup_count;
    }

    /// Convert to pending map format
    fn into_pending(self, barrier_map: &HashMap<BarrierId, Option<Label>>) -> BTreeMap<BarrierKey, Vec<(State, Option<Label>)>> {
        let mut pending = BTreeMap::new();
        for (dest, states) in self.states_by_dest {
            let dest_block = if dest == end_barrier_id() {
                None
            } else {
                barrier_map.get(&dest).cloned().flatten()
            };
            let dest_key = BarrierKey {
                barrier_id: dest,
                hit_count: 0, // Will be updated in main loop
            };
            pending.insert(
                dest_key,
                states.into_iter().map(|s| (s, dest_block.clone())).collect(),
            );
        }
        pending
    }
}

/// Process a single lane and all its paths, returning an accumulator with results.
fn process_lane(
    cfg: &Cfg,
    vec_state: &State,
    lane_idx: usize,
    fixed_env: &FixedEnv,
    start_block: Option<&Label>,
) -> Result<StateAccumulator> {
    let mut accumulator = StateAccumulator::new();

    // Extract scalar state for this lane
    let scalar_state = extract_scalar_lane(vec_state, lane_idx);

    // Run with PathCounter to enumerate all paths
    let mut path_counter = PathCounter::new();
    loop {
        // Clone the scalar state for this path
        let run_state = scalar_state.clone();

        // Run until next barrier or completion
        let run_result = run_to_next_barrier(
            cfg,
            run_state,
            fixed_env,
            &mut path_counter,
            start_block,
        )?;

        // Add to accumulator with deduplication
        accumulator.add_state(run_result.destination, run_result.state);

        // Try next path
        if !path_counter.increment() {
            break;
        }
        path_counter.reset_for_new_run();
    }

    Ok(accumulator)
}

/// Execute states through a CFG using barrier-based execution.
///
/// This replaces the flow-based fixed-point algorithm with a simpler approach:
/// 1. Process barriers in order of (barrier_id, hit_count)
/// 2. At each barrier, vectorize accumulated states
/// 3. For each scalar lane, enumerate all abstract paths using PathCounter
/// 4. Collect output states at their destination barriers
/// 5. Repeat until all states reach END barrier
///
/// Optimizations:
/// - Parallel lane processing: All lanes of a vector state are processed in parallel
/// - Early deduplication: States are deduplicated as they are generated
/// - GC before dedup: States are GC'd to normalize heap IDs before comparison
pub fn execute_with_barriers(
    cfg: &Cfg,
    initial_states: Vec<State>,
    fixed_env: &FixedEnv,
) -> Result<Vec<(State, Option<Value>)>> {
    // Build map from barrier ID to block label
    let barrier_map = build_barrier_map(cfg);

    // Map from barrier key to waiting states with their starting blocks
    let mut pending: BTreeMap<BarrierKey, Vec<(State, Option<Label>)>> = BTreeMap::new();

    // Track per-state hit counts (state hash -> hit count per barrier)
    // For now, use a simpler approach: global hit count per barrier
    let mut barrier_hit_counts: HashMap<BarrierId, usize> = HashMap::new();

    // Initialize with states at START barrier (they will start from entry block)
    let start_key = BarrierKey {
        barrier_id: start_barrier_id(),
        hit_count: 0,
    };
    pending.insert(
        start_key,
        initial_states.into_iter().map(|s| (s, None)).collect(),
    );

    // Collect results (states that reached END)
    let mut results: Vec<(State, Option<Value>)> = Vec::new();

    // Stats
    let mut total_paths_enumerated = 0usize;
    let mut total_dedup_count = 0usize;

    loop {
        // Find the lowest barrier key with pending states
        let next_key = pending
            .iter()
            .find(|(_, states)| !states.is_empty())
            .map(|(k, _)| k.clone());

        let current_key = match next_key {
            Some(k) => k,
            None => break, // No more pending states
        };

        // Take the states at this barrier
        let states_with_blocks = pending.remove(&current_key).unwrap_or_default();
        if states_with_blocks.is_empty() {
            continue;
        }

        // Check if this is the END barrier
        if current_key.barrier_id == end_barrier_id() {
            // These states are done
            for (state, _) in states_with_blocks {
                results.push((state, None));
            }
            continue;
        }

        // Determine starting block for this barrier
        let start_block = if current_key.barrier_id == start_barrier_id() {
            None // Start from entry block
        } else {
            barrier_map.get(&current_key.barrier_id).cloned().flatten()
        };

        // Extract just the states for vectorization
        let states: Vec<State> = states_with_blocks.into_iter().map(|(s, _)| s).collect();

        // Vectorize the states at this barrier
        let vectorized = vectorize_states(states);

        let expanded_count: usize = vectorized.iter().map(|s| s.vector_size).sum();
        println!(
            "  Barrier {:?} hit={}: {} vectorized states ({} expanded)",
            current_key.barrier_id.0,
            current_key.hit_count,
            vectorized.len(),
            expanded_count
        );

        // Collect all (vec_state_ref, lane_idx) pairs for parallel processing
        let lane_tasks: Vec<(&State, usize)> = vectorized
            .iter()
            .flat_map(|vec_state| {
                (0..vec_state.vector_size).map(move |lane_idx| (vec_state, lane_idx))
            })
            .collect();

        // Process lanes in batches with intermediate vectorization
        // This reduces memory and allows more effective deduplication
        const BATCH_SIZE: usize = 64;

        let start_block_ref = start_block.as_ref();
        let mut combined_accumulator = StateAccumulator::new();

        for batch in lane_tasks.chunks(BATCH_SIZE) {
            // Process this batch in parallel
            let batch_results: Vec<Result<StateAccumulator>> = batch
                .par_iter()
                .map(|(vec_state, lane_idx)| {
                    process_lane(cfg, vec_state, *lane_idx, fixed_env, start_block_ref)
                })
                .collect();

            // Merge batch results into combined accumulator
            for result in batch_results {
                let accumulator = result?;
                total_paths_enumerated += accumulator.states_by_dest.values().map(|v| v.len()).sum::<usize>() + accumulator.dedup_count;
                combined_accumulator.merge(accumulator);
            }

            // Intermediate vectorization: reduce states after each batch
            // This helps limit state explosion and improve dedup effectiveness
            if combined_accumulator.states_by_dest.values().map(|v| v.len()).sum::<usize>() > 256 {
                for (dest, states) in &mut combined_accumulator.states_by_dest {
                    if states.len() > 64 {
                        let before = states.len();
                        *states = vectorize_states(std::mem::take(states));
                        let after = states.len();
                        if after < before {
                            // Update the seen set with the newly vectorized states
                            let seen_set = combined_accumulator.seen_normalized.entry(dest.clone()).or_default();
                            seen_set.clear();
                            for state in states.iter() {
                                seen_set.insert(normalize_state_for_comparison(state));
                            }
                        }
                    }
                }
            }
        }

        total_dedup_count += combined_accumulator.dedup_count;

        // Get destination hit counts and add to pending
        for (dest, states) in combined_accumulator.states_by_dest {
            let dest_hit = barrier_hit_counts.get(&dest).copied().unwrap_or(0);
            let dest_key = BarrierKey {
                barrier_id: dest.clone(),
                hit_count: dest_hit,
            };
            let dest_block = if dest == end_barrier_id() {
                None
            } else {
                barrier_map.get(&dest).cloned().flatten()
            };
            pending
                .entry(dest_key)
                .or_default()
                .extend(states.into_iter().map(|s| (s, dest_block.clone())));
        }

        // Increment hit count for this barrier
        *barrier_hit_counts.entry(current_key.barrier_id).or_insert(0) += 1;
    }

    println!(
        "  [Stats] Paths enumerated: {}, deduplicated: {} ({:.1}% reduction)",
        total_paths_enumerated,
        total_dedup_count,
        if total_paths_enumerated > 0 {
            100.0 * total_dedup_count as f64 / total_paths_enumerated as f64
        } else {
            0.0
        }
    );

    Ok(results)
}

/// Extract a scalar state from a vectorized state at the given lane index.
/// Creates a mask with only lane_idx set to true and filters the state.
fn extract_scalar_lane(state: &State, lane_idx: usize) -> State {
    if state.vector_size == 1 {
        return state.clone();
    }

    // Create a mask with only lane_idx set to true
    let mut mask = vec![false; state.vector_size];
    mask[lane_idx] = true;

    // Use the existing filter_by_mask method
    state.filter_by_mask(&mask)
}

/// Run a scalar state from the given block until it hits a barrier or completes.
/// Uses PathCounter for ALL branching decisions, including within function calls.
///
/// `start_block`: None means start from entry block, Some(label) means start from that block.
///
/// Returns a single result (PathCounter ensures deterministic path selection).
fn run_to_next_barrier(
    cfg: &Cfg,
    mut state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    start_block: Option<&Label>,
) -> Result<BarrierRunResult> {
    // Use a fake label for the entry block (needed for PHI nodes in successors)
    let entry_label = Label::from("__entry".to_string());

    // Initialize with the starting block
    let (mut current_block, mut current_block_label): (&Block, Option<&Label>) = match start_block
    {
        None => (&cfg.entry, None),
        Some(label) => {
            let block = cfg
                .named
                .get(label)
                .ok_or_else(|| anyhow!("Unknown start block: {:?}", label))?;
            (block, Some(label))
        }
    };

    // Track the label of the block we came FROM (for PHI resolution)
    let mut incoming_label: Option<&Label> = None;

    // Track if this is the first block (skip barrier check at entry since we just passed it)
    let mut first_block = true;

    loop {
        // Execute all instructions in the block
        for (local_id, instruction) in &current_block.instructions {
            // Handle phi nodes - use incoming_label to pick the right branch
            if let Instruction::Phi { branches } = instruction {
                if let Some(from_label) = incoming_label {
                    for (branch_label, value_id) in branches {
                        if branch_label == from_label {
                            let value = state.local_env.get(*value_id).clone();
                            state.local_env.set(*local_id, value);
                            break;
                        }
                    }
                }
                continue;
            }

            // Handle calls with PathCounter (passes through to nested calls)
            if let Instruction::Call { closure, args } = instruction {
                state = interpret_call_with_path_counter(
                    state,
                    *local_id,
                    *closure,
                    args,
                    fixed_env,
                    path_counter,
                )?;
                continue;
            }

            // Handle other instructions using CoreInterpreter
            let mut interpreter =
                super::core_interpreter::CoreInterpreter::new(state, fixed_env);
            interpreter.interpret_non_call_instruction(*local_id, instruction)?;
            state = interpreter.into_state();
        }

        // Check if this block has a barrier (skip on first block)
        if !first_block {
            if let Some(barrier_id) = &current_block.barrier {
                return Ok(BarrierRunResult {
                    state,
                    destination: barrier_id.clone(),
                    return_value: None,
                });
            }
        }
        first_block = false;

        // Handle terminator
        let (_, terminator) = &current_block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| state.local_env.get(id).clone());
                return Ok(BarrierRunResult {
                    state,
                    destination: end_barrier_id(),
                    return_value,
                });
            }
            Terminator::UnconditionalBranch { target } => {
                // Set incoming_label to current block's label (for PHI in target)
                incoming_label = current_block_label.or(Some(&entry_label));
                current_block_label = Some(target);
                current_block = cfg
                    .named
                    .get(target)
                    .ok_or_else(|| anyhow!("Unknown block label: {:?}", target))?;
            }
            Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } => {
                let condition_value = state.local_env.get(*condition);

                let take_true = match condition_value {
                    Value::Bool(MaybeVector::Scalar(b)) => *b,
                    Value::Bool(MaybeVector::Vector(_)) => {
                        panic!("Unexpected vector bool in scalar execution");
                    }
                    Value::UnknownBool => path_counter.get_choice(),
                    Value::Nil(_) => false,
                    Value::Number(_)
                    | Value::NumberInterval(_)
                    | Value::String(_)
                    | Value::Pointer(_) => true,
                    Value::NilPointer(_) => return Err(anyhow!("Nil pointer in condition")),
                };

                let target = if take_true { true_target } else { false_target };
                // Set incoming_label to current block's label (for PHI in target)
                incoming_label = current_block_label.or(Some(&entry_label));
                current_block_label = Some(target);
                current_block = cfg
                    .named
                    .get(target)
                    .ok_or_else(|| anyhow!("Unknown block label: {:?}", target))?;
            }
        }

        // Check if the new block has a barrier at entry
        if let Some(barrier_id) = &current_block.barrier {
            return Ok(BarrierRunResult {
                state,
                destination: barrier_id.clone(),
                return_value: None,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_counter_single_choice() {
        let mut pc = PathCounter::new();

        // First run: choice 0 = false (bit 0 of 0)
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Second run: choice 0 = true (bit 0 of 1)
        assert!(pc.get_choice());
        assert!(!pc.increment()); // No more paths
    }

    #[test]
    fn test_path_counter_two_choices() {
        let mut pc = PathCounter::new();

        // Run 0 (path 00): false, false
        assert!(!pc.get_choice());
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Run 1 (path 01): true, false
        assert!(pc.get_choice());
        assert!(!pc.get_choice());
        assert!(pc.increment());

        // Run 2 (path 10): false, true
        assert!(!pc.get_choice());
        assert!(pc.get_choice());
        assert!(pc.increment());

        // Run 3 (path 11): true, true
        assert!(pc.get_choice());
        assert!(pc.get_choice());
        assert!(!pc.increment()); // No more paths (2^2 = 4 paths done)
    }

    #[test]
    fn test_barrier_key_ordering() {
        let k1 = BarrierKey {
            barrier_id: BarrierId::new(vec![1]),
            hit_count: 0,
        };
        let k2 = BarrierKey {
            barrier_id: BarrierId::new(vec![1]),
            hit_count: 1,
        };
        let k3 = BarrierKey {
            barrier_id: BarrierId::new(vec![2]),
            hit_count: 0,
        };

        assert!(k1 < k2); // Same barrier, lower hit count
        assert!(k2 < k3); // Lower barrier ID
        assert!(k1 < k3);
    }
}
