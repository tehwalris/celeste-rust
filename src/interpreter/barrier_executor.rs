//! Barrier-based execution: process states through barriers in order,
//! using PathCounter for abstract path enumeration.
//!
//! Key design: PathCounter is passed through ALL levels of function calls,
//! so the path enumeration works across the entire call stack.
//!
//! **Important**: Barriers inside function calls ARE respected. When a barrier
//! is hit inside a nested function call, execution yields back to the main
//! executor with a CallStack that records where to resume.
//!
//! Optimizations:
//! - Early path merging: States are merged incrementally as paths complete
//! - Parallel lane processing: Vector lanes are processed in parallel using rayon
//! - Path deduplication: Duplicate states are detected and merged during enumeration

use std::collections::BTreeMap;
use std::sync::LazyLock;

use anyhow::{anyhow, Result};
use rayon::prelude::*;
use rustc_hash::FxHashMap;

use crate::ir::{BarrierId, Block, Cfg, Instruction, Label, LocalId, Terminator, GlobalId};

/// Static entry label to avoid repeated String allocations.
/// Used for PHI node resolution when entering a block from the CFG entry point.
pub(crate) static ENTRY_LABEL: LazyLock<Label> = LazyLock::new(|| Label::from("__entry".to_string()));

use super::{
    fixed_env::FixedEnv,
    local_env::LocalEnv,
    state::State,
    value::{HeapValue, MaybeVector, Value},
    vectorize::vectorize_states,
};

/// Build a map from BarrierId to the block that has that barrier.
/// Returns None for entry block, Some(label) for named blocks.
fn build_barrier_map(cfg: &Cfg) -> FxHashMap<BarrierId, Option<Label>> {
    let mut map = FxHashMap::default();

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
    pub current_path: u64,
    /// Number of choices made so far in this run
    pub choices_made: usize,
    /// Maximum choices seen in any path (for knowing when we're done)
    pub max_choices: usize,
}

impl PathCounter {
    #[inline]
    pub fn new() -> Self {
        Self {
            current_path: 0,
            choices_made: 0,
            max_choices: 0,
        }
    }

    /// Get the choice for the next UnknownBool branch.
    /// Returns true or false based on the current path.
    #[inline]
    pub fn get_choice(&mut self) -> bool {
        let choice = (self.current_path >> self.choices_made) & 1 == 1;
        self.choices_made += 1;
        self.max_choices = self.max_choices.max(self.choices_made);
        choice
    }

    /// Check if the choice at index `choice_idx` is true in current path
    #[inline]
    pub fn peek_choice(&self, choice_idx: usize) -> bool {
        (self.current_path >> choice_idx) & 1 == 1
    }

    /// Reset for a new run (called at barrier)
    #[inline]
    pub fn reset_for_new_run(&mut self) {
        self.choices_made = 0;
    }

    /// Try to increment to the next path.
    /// Returns true if there are more paths to explore, false if exhausted.
    #[inline]
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
    #[inline]
    pub fn is_valid(&self) -> bool {
        // A path becomes invalid if we've made more choices than the path encoding allows
        self.choices_made <= 64
    }
}

/// A frame in the call stack, representing a suspended function call waiting
/// for a barrier to be processed.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallFrame {
    /// The function being called (for looking up its CFG)
    pub function_name: GlobalId,
    /// The local ID where the return value should be stored in the caller
    pub return_local_id: LocalId,
    /// The block label to resume from after the barrier (None = entry block)
    /// Note: This is the block where the barrier was hit, so we resume FROM there.
    pub resume_block: Option<Label>,
    /// Index of the next instruction to execute in the resume block
    /// (after all instructions in the block up to barrier have been executed)
    pub instruction_index: usize,
    /// The incoming label for PHI node resolution (the block we branched from)
    pub incoming_label: Option<Label>,
}

/// A stack of call frames representing suspended execution.
/// The innermost (most recently called) function is at the end.
///
/// Uses Arc<Vec> for O(1) cloning via reference counting.
/// The COW semantics ensure that mutations create new copies only when necessary.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct CallStack {
    /// None represents empty stack (avoids allocation for common case)
    /// Some(Arc<Vec>) for non-empty stacks, allowing O(1) clone
    frames: Option<std::sync::Arc<Vec<CallFrame>>>,
}

impl CallStack {
    #[inline]
    pub fn new() -> Self {
        Self { frames: None }
    }

    pub fn from_frames(frames: Vec<CallFrame>) -> Self {
        if frames.is_empty() {
            Self { frames: None }
        } else {
            Self { frames: Some(std::sync::Arc::new(frames)) }
        }
    }

    #[inline]
    pub fn push(&mut self, frame: CallFrame) {
        match &mut self.frames {
            Some(arc) => std::sync::Arc::make_mut(arc).push(frame),
            None => self.frames = Some(std::sync::Arc::new(vec![frame])),
        }
    }

    #[inline]
    pub fn pop(&mut self) -> Option<CallFrame> {
        match &mut self.frames {
            Some(arc) => {
                let frames = std::sync::Arc::make_mut(arc);
                let result = frames.pop();
                if frames.is_empty() {
                    self.frames = None;
                }
                result
            }
            None => None,
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.frames.as_ref().map_or(true, |f| f.is_empty())
    }

    /// Get the number of frames in the stack.
    #[inline]
    pub fn len(&self) -> usize {
        self.frames.as_ref().map_or(0, |f| f.len())
    }

    /// Get an iterator over the frames (from outermost to innermost).
    pub fn iter(&self) -> impl Iterator<Item = &CallFrame> {
        self.frames.as_deref().into_iter().flatten()
    }

    /// Insert a frame at the front (making it the outermost frame).
    pub fn insert_front(&mut self, frame: CallFrame) {
        match &mut self.frames {
            Some(arc) => std::sync::Arc::make_mut(arc).insert(0, frame),
            None => self.frames = Some(std::sync::Arc::new(vec![frame])),
        }
    }

    /// Extend with frames from another CallStack.
    pub fn extend(&mut self, other: CallStack) {
        if let Some(other_arc) = other.frames {
            match &mut self.frames {
                Some(arc) => {
                    let frames = std::sync::Arc::make_mut(arc);
                    // Unwrap the Arc or clone its contents
                    match std::sync::Arc::try_unwrap(other_arc) {
                        Ok(other_frames) => frames.extend(other_frames),
                        Err(arc) => frames.extend(arc.iter().cloned()),
                    }
                }
                None => self.frames = Some(other_arc),
            }
        }
    }
}

/// Result of running a CFG segment - either completion or barrier yield.
#[derive(Clone, Debug)]
pub enum CfgRunResult {
    /// The CFG completed (hit a Return terminator)
    Completed {
        state: State,
        return_value: Option<Value>,
    },
    /// A barrier was hit (could be in this CFG or a nested function call)
    BarrierYield {
        state: State,
        barrier_id: BarrierId,
        /// The block where execution should resume (in the current CFG).
        /// None means entry block.
        resume_block: Option<Label>,
        /// Index of the next instruction to execute in the resume block.
        instruction_index: usize,
        /// If the barrier was hit inside a nested function call, this contains
        /// the call stack of suspended frames.
        call_stack: CallStack,
    },
}

/// Execute a call instruction using PathCounter for all branching decisions.
/// This function is called by run_to_next_barrier for function calls.
/// PathCounter is passed through to nested function calls.
///
/// Returns a CfgRunResult which can be either:
/// - Completed: the function returned normally
/// - BarrierYield: a barrier was hit inside the function (or nested calls)
fn interpret_call_with_path_counter(
    state: State,
    local_id: LocalId,
    closure_local_id: LocalId,
    arg_local_ids: &[LocalId],
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
) -> Result<CfgRunResult> {
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

    // Get the heap value - avoid clone by matching on reference first
    let heap_value = state.heap.get(closure_heap_id);

    // Gather argument values
    let arg_values: Vec<Value> = arg_local_ids
        .iter()
        .map(|id| state.local_env.get(*id).clone())
        .collect();

    match heap_value {
        HeapValue::BuiltinFun(name) => {
            let name = name.clone(); // Clone the Arc-wrapped name so we can use it after moving state
            // Look up the builtin function
            let builtin_fn = fixed_env
                .builtin_funs
                .get(name.as_str())
                .ok_or_else(|| anyhow!("Unknown builtin function: {}", name.as_str()))?;

            // Call the builtin, which returns multiple (state, return_value) pairs
            // Builtins never hit barriers - they complete immediately
            let results = builtin_fn(state, arg_values)?;

            if results.is_empty() {
                return Err(anyhow!("Builtin {} returned no results", name.as_str()));
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
            Ok(CfgRunResult::Completed {
                state: result_state,
                return_value: None, // Return value already set in local_env
            })
        }
        HeapValue::Closure(fun_def_name, captured_values) => {
            // Look up the function definition with prepared CFG
            let (fun_def, prepared_cfg) = fixed_env
                .fun_defs
                .get(fun_def_name)
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
                    let value = arg_values.get(i).cloned().unwrap_or(Value::Nil(None));
                    new_local_env.set(*arg_id, value);
                }
            }

            // Create the state for executing the function body
            let new_outer_local_envs = state.outer_local_envs.push_caller_env(state.local_env.clone());

            let function_state = State {
                heap: state.heap.clone(),
                local_env: new_local_env,
                outer_local_envs: new_outer_local_envs,
                global_env: state.global_env.clone(),
                prints: state.prints.clone(),
                vector_size: state.vector_size,
            };

            // Recursively interpret the function's CFG using PathCounter
            // This may yield at a barrier inside the function!
            let cfg_result = run_cfg_to_barrier_or_completion(
                &prepared_cfg.cfg,
                function_state,
                fixed_env,
                path_counter,
                None, // Start from entry block
                0,    // Start from first instruction
            )?;

            match cfg_result {
                CfgRunResult::Completed { state: result_state, return_value } => {
                    // Function completed normally - restore caller state
                    let (caller_local_env, outer_local_envs) = result_state.outer_local_envs.pop_caller_env();

                    let mut caller_state = State {
                        heap: result_state.heap,
                        local_env: caller_local_env,
                        outer_local_envs,
                        global_env: result_state.global_env,
                        prints: result_state.prints,
                        vector_size: result_state.vector_size,
                    };

                    // Set the return value
                    caller_state
                        .local_env
                        .set(local_id, return_value.unwrap_or(Value::Nil(None)));
                    Ok(CfgRunResult::Completed {
                        state: caller_state,
                        return_value: None, // Return value already set in local_env
                    })
                }
                CfgRunResult::BarrierYield {
                    state: yield_state,
                    barrier_id,
                    resume_block,
                    instruction_index,
                    mut call_stack,
                } => {
                    // Barrier was hit inside the function call!
                    //
                    // The call_stack is ordered so that when we pop() (from the END), we get
                    // the INNERMOST frame first. This means:
                    // - Position 0: outermost caller
                    // - Position N-1: innermost callee (where barrier was hit)
                    //
                    // We're in `interpret_call_with_path_counter`, which was called to invoke
                    // this function (fun_def_name). The callee has hit a barrier and returned
                    // a yield with resume info for the callee.
                    //
                    // We need to add this function's (the callee's) frame to the call stack.
                    // Since the callee is the innermost function at this point, we should
                    // add it at the END so it's popped first during resume.
                    //
                    // But wait - when we propagate up, the CALLER will also add its frame.
                    // The caller is OUTER relative to us. So when the caller adds its frame,
                    // it should go at the FRONT (position 0).
                    //
                    // So we should INSERT at position 0, not push at the end!
                    call_stack.insert_front(CallFrame {
                        function_name: fun_def_name.clone(),
                        return_local_id: local_id,
                        resume_block,
                        instruction_index,
                        incoming_label: None, // Not used in old recursive path
                    });

                    // Propagate the yield up with the augmented call stack
                    // The caller will receive this and add ITS frame at the front.
                    Ok(CfgRunResult::BarrierYield {
                        state: yield_state,
                        barrier_id,
                        resume_block: None, // Caller will fill this in
                        instruction_index: 0,
                        call_stack,
                    })
                }
            }
        }
        _ => Err(anyhow!("Expected closure or builtin function")),
    }
}

/// Run a CFG until a barrier is hit or execution completes.
/// Used for function calls - runs until a Return OR a barrier is encountered.
///
/// `start_block`: None means start from entry block, Some(label) means start from that block.
/// `start_instruction_index`: Index of the first instruction to execute in the start block.
fn run_cfg_to_barrier_or_completion(
    cfg: &Cfg,
    mut state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    start_block: Option<&Label>,
    start_instruction_index: usize,
) -> Result<CfgRunResult> {
    // Initialize with the starting block
    let (mut current_block, mut current_block_label): (&Block, Option<&Label>) = match start_block {
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

    // Track if this is the first block (skip barrier check at entry since we're resuming from there)
    let mut first_block = true;
    let mut instruction_start_index = start_instruction_index;

    loop {
        // Execute instructions in the block (starting from instruction_start_index)
        for (inst_idx, (local_id, instruction)) in current_block.instructions.iter().enumerate() {
            // Skip instructions before the start index (when resuming)
            if inst_idx < instruction_start_index {
                continue;
            }

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

            // Handle calls with PathCounter - may yield at a barrier!
            if let Instruction::Call { closure, args } = instruction {
                let call_result = interpret_call_with_path_counter(
                    state,
                    *local_id,
                    *closure,
                    args,
                    fixed_env,
                    path_counter,
                )?;

                match call_result {
                    CfgRunResult::Completed { state: new_state, .. } => {
                        state = new_state;
                    }
                    CfgRunResult::BarrierYield {
                        state: yield_state,
                        barrier_id,
                        call_stack,
                        ..
                    } => {
                        // A barrier was hit inside the call!
                        // We need to record that we should resume at the NEXT instruction
                        // after this call completes.
                        return Ok(CfgRunResult::BarrierYield {
                            state: yield_state,
                            barrier_id,
                            resume_block: current_block_label.cloned(),
                            instruction_index: inst_idx + 1, // Resume at next instruction
                            call_stack,
                        });
                    }
                }
                continue;
            }

            // Handle other instructions using CoreInterpreter
            let mut interpreter =
                super::core_interpreter::CoreInterpreter::new(state, fixed_env);
            interpreter.interpret_non_call_instruction(*local_id, instruction)?;
            state = interpreter.into_state();
        }

        // After first block, reset instruction start index
        instruction_start_index = 0;

        // Check if this block has a barrier (skip on first block when resuming)
        if !first_block {
            if let Some(barrier_id) = &current_block.barrier {
                return Ok(CfgRunResult::BarrierYield {
                    state,
                    barrier_id: barrier_id.clone(),
                    resume_block: current_block_label.cloned(),
                    instruction_index: 0, // Resume from start of this block's instructions
                    call_stack: CallStack::new(), // No nested calls suspended
                });
            }
        }
        first_block = false;

        // Handle terminator
        let (_, terminator) = &current_block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| state.local_env.get(id).clone());
                return Ok(CfgRunResult::Completed { state, return_value });
            }
            Terminator::UnconditionalBranch { target } => {
                // Set incoming_label to current block's label (for PHI in target)
                incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
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
                incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
                current_block_label = Some(target);
                current_block = cfg
                    .named
                    .get(target)
                    .ok_or_else(|| anyhow!("Unknown block label: {:?}", target))?;
            }
        }

        // Check if the new block has a barrier at entry
        if let Some(barrier_id) = &current_block.barrier {
            return Ok(CfgRunResult::BarrierYield {
                state,
                barrier_id: barrier_id.clone(),
                resume_block: current_block_label.cloned(),
                instruction_index: 0,
                call_stack: CallStack::new(),
            });
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
static END_BARRIER_ID: LazyLock<BarrierId> = LazyLock::new(|| BarrierId::new(vec![i32::MAX]));

pub fn end_barrier_id() -> BarrierId {
    END_BARRIER_ID.clone()
}

/// The implicit "START" barrier for initial states
static START_BARRIER_ID: LazyLock<BarrierId> = LazyLock::new(|| BarrierId::new(vec![i32::MIN]));

pub fn start_barrier_id() -> BarrierId {
    START_BARRIER_ID.clone()
}

/// Result of running a scalar state from one barrier to the next
#[derive(Clone, Debug)]
pub struct BarrierRunResult {
    /// The output state
    pub state: State,
    /// The destination barrier (or END)
    pub destination: BarrierId,
    /// The call stack for resumption (empty if barrier was hit in top-level CFG)
    pub call_stack: CallStack,
    /// Block to resume in the top-level CFG
    pub top_level_resume_block: Option<Label>,
    /// Instruction index to resume at in the top-level CFG
    pub top_level_instruction_index: usize,
}

/// Resume context for a state waiting at a barrier.
/// This contains all the information needed to resume execution.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ResumeContext {
    /// Block to resume in the top-level CFG (None = entry block)
    pub top_level_resume_block: Option<Label>,
    /// Instruction index to resume at in the top-level CFG
    pub top_level_instruction_index: usize,
    /// Call stack for resumption (empty if barrier was hit in top-level CFG)
    pub call_stack: CallStack,
}

impl ResumeContext {
    /// Create a context for starting fresh from a block
    pub fn fresh(start_block: Option<Label>) -> Self {
        Self {
            top_level_resume_block: start_block,
            top_level_instruction_index: 0,
            call_stack: CallStack::new(),
        }
    }
}

/// Accumulator for collecting states.
///
/// States are grouped by (BarrierId, ResumeContext) since states at the same
/// barrier but with different resume contexts cannot be merged - they represent
/// different program points (e.g., same barrier called from different call sites).
///
/// Deduplication happens during vectorization, not at collection time.
/// This reduces per-state overhead (no normalize for every state).
struct StateAccumulator {
    /// States grouped by (destination barrier, resume context)
    states_by_dest: FxHashMap<(BarrierId, ResumeContext), Vec<State>>,
    /// Count of states that were deduplicated (for stats)
    dedup_count: usize,
}

impl StateAccumulator {
    fn new() -> Self {
        Self {
            states_by_dest: FxHashMap::default(),
            dedup_count: 0,
        }
    }

    /// Add a state to the accumulator with deduplication.
    /// Returns true if the state was added (new), false if it was a duplicate.
    #[inline]
    fn add_state(&mut self, dest: BarrierId, state: State, resume_ctx: ResumeContext) -> bool {
        // Don't GC here - defer to vectorize_in_place for batching
        // Key by (BarrierId, ResumeContext) - states with different resume contexts cannot be merged
        let key = (dest, resume_ctx);

        // Add directly - GC and dedup will happen during vectorization
        self.states_by_dest.entry(key).or_default().push(state);
        true
    }

    /// Merge another accumulator into this one
    fn merge(&mut self, other: StateAccumulator) {
        for (key, mut states) in other.states_by_dest {
            // Append directly - dedup will happen during vectorization
            self.states_by_dest.entry(key).or_default().append(&mut states);
        }
        self.dedup_count += other.dedup_count;
    }

    /// Convert to pending map format
    fn into_pending(self) -> BTreeMap<BarrierKey, Vec<(Vec<State>, ResumeContext)>> {
        let mut pending: BTreeMap<BarrierKey, Vec<(Vec<State>, ResumeContext)>> = BTreeMap::new();
        for ((dest, resume_ctx), states) in self.states_by_dest {
            let dest_key = BarrierKey {
                barrier_id: dest,
                hit_count: 0, // Will be updated in main loop
            };
            pending.entry(dest_key).or_default().push((states, resume_ctx));
        }
        pending
    }

    /// Vectorize accumulated states in place to reduce memory and speed up future comparisons.
    /// This merges states with identical shapes into vectorized states.
    /// Also GCs states before vectorization to get deterministic heap IDs.
    fn vectorize_in_place(&mut self) {
        use rayon::prelude::*;

        for states in self.states_by_dest.values_mut() {
            // GC all states first to get deterministic heap IDs (required for vectorization)
            // Parallelize GC when there are many states
            if states.len() > 100 {
                states.par_iter_mut().for_each(|state| state.gc());
            } else {
                for state in states.iter_mut() {
                    state.gc();
                }
            }
            if states.len() > 1 {
                // Vectorize the states (this also does deduplication)
                let vectorized = vectorize_states(std::mem::take(states));
                *states = vectorized;
            }
        }
    }
}

/// Process a single lane and all its paths, returning an accumulator with results.
///
/// Optimization: Uses checkpoint/restore pattern for path enumeration.
/// Instead of cloning State for each path, we:
/// 1. Create a ScalarRuntime once
/// 2. Checkpoint its state (including PathCounter)
/// 3. For each path: restore from checkpoint, run, extract result
///
/// This eliminates the State->ScalarRuntime->State conversion overhead per path.
fn process_lane(
    cfg: &Cfg,
    vec_state: &State,
    lane_idx: usize,
    fixed_env: &FixedEnv,
    resume_ctx: &ResumeContext,
) -> Result<StateAccumulator> {
    use super::scalar_runtime::{run_to_next_barrier_flat_inplace, RuntimeCheckpoint, ScalarRuntime};

    let mut accumulator = StateAccumulator::new();

    // Extract scalar state for this lane
    let mut scalar_state = extract_scalar_lane(vec_state, lane_idx);
    scalar_state.heap.freeze();

    let mut path_counter = PathCounter::new();
    let mut paths_since_vectorize = 0;
    const VECTORIZE_BATCH_SIZE: usize = 32;

    let mut runtime = ScalarRuntime::from_state(scalar_state, fixed_env, &mut path_counter);
    let checkpoint = RuntimeCheckpoint::from_runtime(&runtime);

    loop {
        checkpoint.restore_to(&mut runtime);

        // Use the new flat execution loop
        let run_result = run_to_next_barrier_flat_inplace(
            cfg,
            &mut runtime,
            resume_ctx.top_level_resume_block.as_ref(),
            resume_ctx.top_level_instruction_index,
            resume_ctx.call_stack.clone(),
        )?;

        let dest_resume_ctx = ResumeContext {
            top_level_resume_block: run_result.top_level_resume_block,
            top_level_instruction_index: run_result.top_level_instruction_index,
            call_stack: run_result.call_stack,
        };

        let result_state = runtime.snapshot_to_state();
        accumulator.add_state(run_result.destination, result_state, dest_resume_ctx);
        paths_since_vectorize += 1;

        if paths_since_vectorize >= VECTORIZE_BATCH_SIZE {
            accumulator.vectorize_in_place();
            paths_since_vectorize = 0;
        }

        if !runtime.path_counter.increment() {
            break;
        }
    }

    accumulator.vectorize_in_place();
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
/// **Key feature**: Barriers inside function calls ARE respected! When a barrier
/// is hit inside a nested function call, execution yields back to the main
/// executor with a CallStack that records where to resume.
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
    // Map from barrier key to list of (states, resume_context) groups
    // States with different resume contexts are kept separate even at the same barrier
    let mut pending: BTreeMap<BarrierKey, Vec<(Vec<State>, ResumeContext)>> = BTreeMap::new();

    // Track per-state hit counts (state hash -> hit count per barrier)
    // For now, use a simpler approach: global hit count per barrier
    let mut barrier_hit_counts: FxHashMap<BarrierId, usize> = FxHashMap::default();

    // Initialize with states at START barrier (they will start from entry block)
    let start_key = BarrierKey {
        barrier_id: start_barrier_id(),
        hit_count: 0,
    };
    pending.insert(
        start_key,
        vec![(initial_states, ResumeContext::fresh(None))],
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
            .find(|(_, groups)| groups.iter().any(|(states, _)| !states.is_empty()))
            .map(|(k, _)| k.clone());

        let current_key = match next_key {
            Some(k) => k,
            None => break, // No more pending states
        };

        // Take the state groups at this barrier
        let state_groups = pending.remove(&current_key).unwrap_or_default();
        if state_groups.is_empty() || state_groups.iter().all(|(s, _)| s.is_empty()) {
            continue;
        }

        // Check if this is the END barrier
        if current_key.barrier_id == end_barrier_id() {
            // These states are done
            for (states, _) in state_groups {
                for state in states {
                    results.push((state, None));
                }
            }
            continue;
        }

        let mut combined_accumulator = StateAccumulator::new();

        // Process each group of states (same barrier, different resume contexts)
        for (states, resume_ctx) in state_groups {
            if states.is_empty() {
                continue;
            }

            // Vectorize the states at this barrier
            let mut vectorized = vectorize_states(states);

            // Freeze heaps to move values from im::HashMap overlay to Arc<Vec> base.
            // This makes subsequent get_opt() calls O(1) instead of O(log n).
            for state in &mut vectorized {
                state.heap.freeze();
            }

            let expanded_count: usize = vectorized.iter().map(|s| s.vector_size).sum();
            let call_depth = resume_ctx.call_stack.len();
            let resume_info = if call_depth > 0 {
                format!(
                    " [call depth {}, resume_block={:?}, instr_idx={}]",
                    call_depth,
                    resume_ctx.top_level_resume_block,
                    resume_ctx.top_level_instruction_index
                )
            } else {
                String::new()
            };
            println!(
                "  Barrier {:?} hit={}: {} vectorized states ({} expanded){}",
                current_key.barrier_id.0,
                current_key.hit_count,
                vectorized.len(),
                expanded_count,
                resume_info
            );

            // Collect all (vec_state_ref, lane_idx) pairs for parallel processing
            let lane_tasks: Vec<(&State, usize)> = vectorized
                .iter()
                .flat_map(|vec_state| {
                    (0..vec_state.vector_size).map(move |lane_idx| (vec_state, lane_idx))
                })
                .collect();

            let resume_ctx_ref = &resume_ctx;

            // Process all lanes in parallel using rayon
            let lane_results: Vec<Result<StateAccumulator>> = lane_tasks
                .par_iter()
                .map(|(vec_state, lane_idx)| {
                    process_lane(cfg, vec_state, *lane_idx, fixed_env, resume_ctx_ref)
                })
                .collect();

            // Merge results into combined accumulator
            for result in lane_results {
                let accumulator = result?;
                total_paths_enumerated += accumulator.states_by_dest.values().map(|v| v.len()).sum::<usize>() + accumulator.dedup_count;
                combined_accumulator.merge(accumulator);
            }
        }

        total_dedup_count += combined_accumulator.dedup_count;

        // Get destination hit counts and add to pending
        // Note: States are already GC'd from vectorize_in_place in process_lane
        for ((dest, dest_resume_ctx), states) in combined_accumulator.states_by_dest {
            let dest_hit = barrier_hit_counts.get(&dest).copied().unwrap_or(0);
            let dest_key = BarrierKey {
                barrier_id: dest.clone(),
                hit_count: dest_hit,
            };
            pending
                .entry(dest_key)
                .or_default()
                .push((states, dest_resume_ctx));
        }

        // Increment hit count for this barrier
        *barrier_hit_counts.entry(current_key.barrier_id).or_insert(0) += 1;
    }

    println!(
        "  [Stats] Paths: {}, dedup: {} ({:.1}%)",
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
/// Uses the optimized extract_scalar_lane method on State.
fn extract_scalar_lane(state: &State, lane_idx: usize) -> State {
    state.extract_scalar_lane(lane_idx)
}

/// Resume execution of a call stack until the next barrier or completion.
/// This is called when we need to continue executing from inside nested function calls.
///
/// The call_stack contains frames from outermost to innermost (the last frame is
/// the currently executing function). We resume from the innermost and unwind
/// as functions return.
fn resume_call_stack(
    mut state: State,
    mut call_stack: CallStack,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
) -> Result<CfgRunResult> {
    // Pop the innermost frame and resume execution in that function
    while let Some(frame) = call_stack.pop() {
        // Look up the function's CFG
        let (_, prepared_cfg) = fixed_env
            .fun_defs
            .get(&frame.function_name)
            .ok_or_else(|| anyhow!("Unknown function: {:?}", frame.function_name))?;

        // Resume execution from where we left off
        let result = run_cfg_to_barrier_or_completion(
            &prepared_cfg.cfg,
            state,
            fixed_env,
            path_counter,
            frame.resume_block.as_ref(),
            frame.instruction_index,
        )?;

        match result {
            CfgRunResult::Completed { state: result_state, return_value } => {
                // Function completed - restore caller's state and continue with parent
                let (caller_local_env, outer_local_envs) = result_state.outer_local_envs.pop_caller_env();

                let mut caller_state = State {
                    heap: result_state.heap,
                    local_env: caller_local_env,
                    outer_local_envs,
                    global_env: result_state.global_env,
                    prints: result_state.prints,
                    vector_size: result_state.vector_size,
                };

                // Set the return value at the local_id where the call was
                caller_state
                    .local_env
                    .set(frame.return_local_id, return_value.unwrap_or(Value::Nil(None)));

                // Continue with the caller state - we'll process the next frame
                state = caller_state;
            }
            CfgRunResult::BarrierYield {
                state: yield_state,
                barrier_id,
                resume_block,
                instruction_index,
                call_stack: inner_call_stack,
            } => {
                // Hit another barrier inside this function (or a nested call)
                // We need to rebuild the call stack correctly:
                // - call_stack (the remaining parent frames) should be at the FRONT (outermost)
                // - The current frame should be after those
                // - inner_call_stack (from nested calls) should be at the END (innermost)
                //
                // Example: if we had [A, B] and B hit barrier with inner_call_stack = [C],
                // new call stack should be [A, B, C] so pop() gives C, then B, then A.

                // Build frames vector: [remaining parents, current frame, inner frames]
                let mut frames: Vec<CallFrame> = call_stack.iter().cloned().collect();

                // Add the current frame (this function)
                frames.push(CallFrame {
                    function_name: frame.function_name,
                    return_local_id: frame.return_local_id,
                    resume_block,
                    instruction_index,
                    incoming_label: None, // Not used in old recursive path
                });

                // Add inner frames from nested calls
                frames.extend(inner_call_stack.iter().cloned());

                let new_call_stack = CallStack::from_frames(frames);

                return Ok(CfgRunResult::BarrierYield {
                    state: yield_state,
                    barrier_id,
                    resume_block: None,
                    instruction_index: 0,
                    call_stack: new_call_stack,
                });
            }
        }
    }

    // All frames completed - return the final state as completed
    Ok(CfgRunResult::Completed {
        state,
        return_value: None,
    })
}

/// Run a scalar state from the given block until it hits a barrier or completes.
/// Uses PathCounter for ALL branching decisions, including within function calls.
///
/// This function now properly yields at barriers inside function calls!
///
/// `start_block`: None means start from entry block, Some(label) means start from that block.
/// `start_instruction_index`: Index of the instruction to start from in the start block.
/// `call_stack`: If non-empty, we're resuming inside nested function calls.
///
/// Returns a single result (PathCounter ensures deterministic path selection).
fn run_to_next_barrier(
    cfg: &Cfg,
    state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    start_block: Option<&Label>,
    start_instruction_index: usize,
    call_stack: CallStack,
) -> Result<BarrierRunResult> {
    // If we have a call stack, we need to resume inside nested function calls first
    let (state, start_block, start_instruction_index) = if !call_stack.is_empty() {
        // Resume the call stack
        let result = resume_call_stack(state, call_stack, fixed_env, path_counter)?;

        match result {
            CfgRunResult::Completed { state: completed_state, .. } => {
                // All nested calls completed - continue from where we left off in the top-level CFG
                // Note: We need to continue from start_block/start_instruction_index in the top-level CFG
                (completed_state, start_block, start_instruction_index)
            }
            CfgRunResult::BarrierYield {
                state: yield_state,
                barrier_id,
                call_stack: new_call_stack,
                ..
            } => {
                // Hit a barrier inside the call stack
                return Ok(BarrierRunResult {
                    state: yield_state,
                    destination: barrier_id,
                    call_stack: new_call_stack,
                    top_level_resume_block: start_block.cloned(),
                    top_level_instruction_index: start_instruction_index,
                });
            }
        }
    } else {
        (state, start_block, start_instruction_index)
    };

    // Run the top-level CFG
    let result = run_cfg_to_barrier_or_completion(
        cfg,
        state,
        fixed_env,
        path_counter,
        start_block,
        start_instruction_index,
    )?;

    match result {
        CfgRunResult::Completed { state, .. } => {
            Ok(BarrierRunResult {
                state,
                destination: end_barrier_id(),
                call_stack: CallStack::new(),
                top_level_resume_block: None,
                top_level_instruction_index: 0,
            })
        }
        CfgRunResult::BarrierYield {
            state,
            barrier_id,
            resume_block,
            instruction_index,
            call_stack,
        } => {
            Ok(BarrierRunResult {
                state,
                destination: barrier_id,
                call_stack,
                top_level_resume_block: resume_block,
                top_level_instruction_index: instruction_index,
            })
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
