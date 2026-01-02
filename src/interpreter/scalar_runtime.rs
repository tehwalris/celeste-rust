//! Lightweight scalar runtime for execution between barriers.
//!
//! This module provides a mutable runtime that eliminates State cloning at function
//! boundaries. Instead of creating new State objects for each function call, we
//! use a stack of local environments and mutate shared heap/global_env/prints in place.
//!
//! Key optimization: Function calls just push/pop on local_env_stack (O(1)) instead
//! of cloning State components.

use std::sync::Arc;

use rustc_hash::FxHashMap;

use anyhow::{anyhow, Result};

use crate::ir::{Block, Cfg, GlobalId, Instruction, Label, LocalId, Terminator, BarrierId};

use super::{
    barrier_executor::{CallFrame, CallStack, PathCounter, BarrierRunResult, end_barrier_id, ENTRY_LABEL},
    fixed_env::FixedEnv,
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    op::{interpret_binary_op, interpret_unary_op},
    state::{GlobalEnv, Prints, OuterLocalEnvs, State},
    value::{HeapValue, MaybeVector, Value},
};

/// A COW (copy-on-write) stack of local environments for O(1) cloning.
/// Uses Arc<Vec<LocalEnv>> internally so cloning is O(1) instead of O(n).
/// Mutations trigger COW via Arc::make_mut.
#[derive(Clone, Debug)]
pub struct LocalEnvStack {
    inner: Arc<Vec<LocalEnv>>,
}

impl LocalEnvStack {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Vec::new()),
        }
    }

    #[inline]
    pub fn from_vec(envs: Vec<LocalEnv>) -> Self {
        Self {
            inner: Arc::new(envs),
        }
    }

    #[inline]
    pub fn push(&mut self, env: LocalEnv) {
        Arc::make_mut(&mut self.inner).push(env);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<LocalEnv> {
        Arc::make_mut(&mut self.inner).pop()
    }

    #[inline]
    pub fn last(&self) -> Option<&LocalEnv> {
        self.inner.last()
    }

    #[inline]
    pub fn last_mut(&mut self) -> Option<&mut LocalEnv> {
        Arc::make_mut(&mut self.inner).last_mut()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Convert to (local_env, outer_local_envs) by popping and reversing.
    /// Consumes the stack.
    pub fn into_state_components(self) -> (LocalEnv, OuterLocalEnvs) {
        // Try to unwrap the Arc first to avoid cloning
        let mut envs = Arc::try_unwrap(self.inner).unwrap_or_else(|arc| (*arc).clone());
        let local_env = envs.pop().expect("stack should not be empty");
        // Reverse: we need index 0 to be most recent caller
        envs.reverse();
        (local_env, OuterLocalEnvs::from_vec(envs))
    }

    /// Create from State's outer_local_envs and local_env.
    pub fn from_state_components(outer: &OuterLocalEnvs, local_env: LocalEnv) -> Self {
        // outer_local_envs has most recent caller at index 0
        // We want oldest at index 0, so reverse
        let mut stack: Vec<LocalEnv> = outer.iter().cloned().collect();
        stack.reverse();
        stack.push(local_env);
        Self::from_vec(stack)
    }
}

/// A mutable runtime for scalar execution between barriers.
///
/// This is designed to be lightweight - function calls just push/pop on the
/// local_env_stack without creating new State objects or cloning heap/global_env/prints.
pub struct ScalarRuntime<'a> {
    /// The heap - shared across all function calls (no cloning!)
    pub heap: Heap,
    /// Global environment - shared across all function calls (no cloning!)
    pub global_env: GlobalEnv,
    /// Print output - shared across all function calls (no cloning!)
    pub prints: Prints,
    /// Stack of local environments. Last element = current function.
    /// On function call: push new local_env
    /// On return: pop
    /// Uses Arc<Vec> for O(1) cloning during checkpoint/restore.
    pub local_env_stack: LocalEnvStack,
    /// The fixed environment (function definitions, builtins)
    pub fixed_env: &'a FixedEnv,
    /// Path counter for abstract branching decisions
    pub path_counter: &'a mut PathCounter,
}

/// A checkpoint of ScalarRuntime state for efficient save/restore during path enumeration.
/// Uses clone() which is O(1) for Arc-based COW data structures (heap, local_envs).
/// Also captures PathCounter state so function calls work correctly after backtracking.
#[derive(Clone)]
pub struct RuntimeCheckpoint {
    heap: Heap,
    global_env: GlobalEnv,
    prints: Prints,
    local_env_stack: LocalEnvStack,
    /// PathCounter state at checkpoint time
    path_counter_state: PathCounterCheckpoint,
}

/// Checkpoint of PathCounter state
#[derive(Clone)]
pub struct PathCounterCheckpoint {
    current_path: u64,
    choices_made: usize,
    max_choices: usize,
}

impl RuntimeCheckpoint {
    /// Create a checkpoint from the current runtime state.
    /// This is O(1) for COW data structures - just clones Arc pointers.
    #[inline]
    pub fn from_runtime(runtime: &ScalarRuntime) -> Self {
        Self {
            heap: runtime.heap.clone(),
            global_env: runtime.global_env.clone(),
            prints: runtime.prints.clone(),
            local_env_stack: runtime.local_env_stack.clone(),
            path_counter_state: PathCounterCheckpoint {
                current_path: runtime.path_counter.current_path,
                choices_made: runtime.path_counter.choices_made,
                max_choices: runtime.path_counter.max_choices,
            },
        }
    }

    /// Restore runtime state from this checkpoint.
    /// This is O(1) - just replaces Arc pointers.
    ///
    /// NOTE: This only restores the execution state (heap, envs, etc.) and
    /// resets choices_made for the new run. It does NOT restore current_path
    /// or max_choices because those are used for path enumeration.
    #[inline]
    pub fn restore_to(&self, runtime: &mut ScalarRuntime) {
        runtime.heap = self.heap.clone();
        runtime.global_env = self.global_env.clone();
        runtime.prints = self.prints.clone();
        runtime.local_env_stack = self.local_env_stack.clone();
        // Only reset choices_made for the new run, don't touch current_path or max_choices
        runtime.path_counter.choices_made = 0;
    }
}

impl<'a> ScalarRuntime<'a> {
    /// Create a new scalar runtime from a State.
    pub fn from_state(state: State, fixed_env: &'a FixedEnv, path_counter: &'a mut PathCounter) -> Self {
        // Convert outer_local_envs + local_env into a stack
        let local_env_stack = LocalEnvStack::from_state_components(&state.outer_local_envs, state.local_env);

        Self {
            heap: state.heap,
            global_env: state.global_env,
            prints: state.prints,
            local_env_stack,
            fixed_env,
            path_counter,
        }
    }

    /// Convert the runtime back to a State.
    pub fn into_state(self) -> State {
        let (local_env, outer_local_envs) = self.local_env_stack.into_state_components();

        State {
            heap: self.heap,
            local_env,
            outer_local_envs,
            global_env: self.global_env,
            prints: self.prints,
            vector_size: 1,
        }
    }

    /// Get a reference to the current function's local environment.
    #[inline]
    pub fn local_env(&self) -> &LocalEnv {
        self.local_env_stack.last().expect("local_env_stack should not be empty")
    }

    /// Get a mutable reference to the current function's local environment.
    #[inline]
    pub fn local_env_mut(&mut self) -> &mut LocalEnv {
        self.local_env_stack.last_mut().expect("local_env_stack should not be empty")
    }

    /// Push a new local environment for a function call.
    #[inline]
    pub fn push_local_env(&mut self, local_env: LocalEnv) {
        self.local_env_stack.push(local_env);
    }

    /// Pop the current function's local environment (for return).
    #[inline]
    pub fn pop_local_env(&mut self) -> LocalEnv {
        self.local_env_stack.pop().expect("local_env_stack should not be empty")
    }

    /// Get a heap value by ID.
    #[inline]
    pub fn heap_get(&self, id: HeapId) -> &HeapValue {
        self.heap.get(id)
    }

    /// Set a heap value by ID.
    #[inline]
    pub fn heap_set(&mut self, id: HeapId, value: HeapValue) {
        self.heap.set(id, value);
    }

    /// Allocate a new heap slot.
    #[inline]
    pub fn heap_alloc(&mut self) -> HeapId {
        self.heap.alloc()
    }

    /// Get a mutable reference to a heap value by ID.
    #[inline]
    pub fn heap_get_mut(&mut self, id: HeapId) -> &mut HeapValue {
        self.heap.get_mut(id)
    }

    /// Get a value from the current local environment.
    #[inline]
    pub fn get_local(&self, id: LocalId) -> &Value {
        self.local_env().get(id)
    }

    /// Set a value in the current local environment.
    #[inline]
    pub fn set_local(&mut self, id: LocalId, value: Value) {
        self.local_env_mut().set(id, value);
    }

    /// Get a heap ID from a pointer in the local environment.
    fn heap_id_from_pointer_local(&self, local_id: LocalId) -> Result<HeapId> {
        match self.get_local(local_id) {
            Value::Pointer(heap_id) => Ok(*heap_id),
            Value::NilPointer(hint) => Err(anyhow!("Attempted to dereference nil ({})", hint)),
            value => Err(anyhow!("Value is not a pointer: {:?}", value)),
        }
    }

    /// Interpret a non-call instruction and return the result value.
    #[inline]
    pub fn interpret_non_call_instruction(&mut self, local_id: LocalId, instruction: &Instruction) -> Result<()> {
        let value = self.interpret_instruction_inner(instruction)?;
        if let Some(value) = value {
            self.set_local(local_id, value);
        }
        Ok(())
    }

    /// Inner implementation of instruction interpretation.
    #[inline]
    fn interpret_instruction_inner(&mut self, instruction: &Instruction) -> Result<Option<Value>> {
        match instruction {
            Instruction::Alloc => {
                let heap_id = self.heap_alloc();
                Ok(Some(Value::Pointer(heap_id)))
            }
            Instruction::GetGlobal { name, create_if_missing } => {
                let heap_id = self.global_env.get(name);
                if let Some(&heap_id) = heap_id {
                    Ok(Some(Value::Pointer(heap_id)))
                } else if *create_if_missing {
                    let heap_id = self.heap_alloc();
                    self.global_env.insert(name.clone(), heap_id);
                    Ok(Some(Value::Pointer(heap_id)))
                } else {
                    Ok(Some(Value::NilPointer(format!("global {}", name))))
                }
            }
            Instruction::Load { source } => {
                match self.get_local(*source) {
                    Value::Pointer(heap_id) => {
                        match self.heap_get(*heap_id) {
                            HeapValue::Value(value) => Ok(Some(value.clone())),
                            HeapValue::Closure(_, _) | HeapValue::BuiltinFun(_) => {
                                Ok(Some(Value::Pointer(*heap_id)))
                            }
                            HeapValue::ObjectTable(_)
                            | HeapValue::ArrayTable(_)
                            | HeapValue::UnknownTable => {
                                Ok(Some(Value::Pointer(*heap_id)))
                            }
                        }
                    }
                    Value::NilPointer(hint) => {
                        Ok(Some(Value::Nil(Some(format!("nil pointer to {}", hint)))))
                    }
                    value => Err(anyhow!("Load expected pointer, got {:?}", value)),
                }
            }
            Instruction::Store { target, source } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                let source_value = self.get_local(*source).clone();
                self.heap_set(heap_id, HeapValue::Value(source_value));
                Ok(None)
            }
            Instruction::StoreEmptyTable { target } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                self.heap_set(heap_id, HeapValue::UnknownTable);
                Ok(None)
            }
            Instruction::StoreClosure { target, fun_def, captures } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                let captured_values = captures
                    .iter()
                    .map(|id| self.get_local(*id).clone())
                    .collect();
                self.heap_set(heap_id, HeapValue::Closure(fun_def.clone(), captured_values));
                Ok(None)
            }
            Instruction::GetField { receiver, field, create_if_missing } => {
                let table_heap_id = self.heap_id_from_pointer_local(*receiver)?;
                let field_heap_id = match self.heap_get(table_heap_id) {
                    HeapValue::ObjectTable(old_fields) => old_fields.get(field).copied(),
                    HeapValue::UnknownTable => None,
                    _ => return Err(anyhow!("GetField on non-table")),
                };
                if let Some(field_heap_id) = field_heap_id {
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    let field_heap_id = self.heap_alloc();
                    self.heap_set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.heap_get_mut(table_heap_id) {
                        HeapValue::ObjectTable(fields) => {
                            fields.insert(field.clone(), field_heap_id);
                        }
                        HeapValue::UnknownTable => {
                            let mut fields = FxHashMap::default();
                            fields.insert(field.clone(), field_heap_id);
                            self.heap_set(table_heap_id, HeapValue::ObjectTable(fields));
                        }
                        _ => unreachable!(),
                    }
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else {
                    Ok(Some(Value::NilPointer(format!("field {}", field))))
                }
            }
            Instruction::GetIndex { receiver, index, create_if_missing } => {
                let table_heap_id = self.heap_id_from_pointer_local(*receiver)?;
                let index = match self.get_local(*index) {
                    Value::Number(MaybeVector::Scalar(index)) => {
                        let index = index
                            .as_i16()
                            .ok_or(anyhow!("Index is not an integer"))?;
                        if index < 1 {
                            return Err(anyhow!("Index is less than 1"));
                        }
                        index
                    }
                    _ => return Err(anyhow!("Index is not a scalar number")),
                };
                let field_heap_id = match self.heap_get(table_heap_id) {
                    HeapValue::ArrayTable(old_fields) => {
                        old_fields.get(index as usize - 1).copied()
                    }
                    HeapValue::UnknownTable => None,
                    _ => return Err(anyhow!("GetIndex on non-array-table")),
                };
                if let Some(field_heap_id) = field_heap_id {
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    let field_heap_id = self.heap_alloc();
                    self.heap_set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.heap_get_mut(table_heap_id) {
                        HeapValue::ArrayTable(fields) => {
                            if index as usize != fields.len() + 1 {
                                return Err(anyhow!("Index is not the next index in the array"));
                            }
                            fields.push(field_heap_id);
                        }
                        HeapValue::UnknownTable => {
                            if index as usize != 1 {
                                return Err(anyhow!("Index is not 1 for new array"));
                            }
                            let fields = vec![field_heap_id];
                            self.heap_set(table_heap_id, HeapValue::ArrayTable(fields));
                        }
                        _ => unreachable!(),
                    }
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else {
                    Ok(Some(Value::NilPointer(format!("index {}", index))))
                }
            }
            Instruction::NumberConstant { value } => {
                Ok(Some(Value::Number(MaybeVector::Scalar(*value))))
            }
            Instruction::BoolConstant { value } => {
                Ok(Some(Value::Bool(MaybeVector::Scalar(*value))))
            }
            Instruction::StringConstant { value } => {
                Ok(Some(Value::String(value.clone())))
            }
            Instruction::NilConstant => {
                Ok(Some(Value::Nil(None)))
            }
            Instruction::Call { .. } => {
                panic!("Call instruction should be handled by interpret_call")
            }
            Instruction::UnaryOp { op, arg } => {
                let arg = self.get_local(*arg);
                interpret_unary_op(&self.heap, *op, arg).map(Some)
            }
            Instruction::BinaryOp { left, op, right } => {
                let left = self.get_local(*left);
                let right = self.get_local(*right);
                interpret_binary_op(left, *op, right).map(Some)
            }
            Instruction::Phi { .. } => {
                panic!("Phi nodes should be handled separately")
            }
        }
    }

    /// Interpret a function call. Returns the return value.
    ///
    /// This is the key optimization: instead of creating a new State, we just
    /// push a new LocalEnv onto the stack and run the function's CFG.
    pub fn interpret_call(
        &mut self,
        local_id: LocalId,
        closure_local_id: LocalId,
        arg_local_ids: &[LocalId],
    ) -> Result<ScalarCfgResult> {
        let closure_heap_id = match self.get_local(closure_local_id) {
            Value::Pointer(heap_id) => *heap_id,
            Value::NilPointer(hint) => {
                return Err(anyhow!("Attempt to call nil ({})", hint));
            }
            _ => return Err(anyhow!("Expected pointer for closure")),
        };

        // Gather argument values (we need to clone these to pass to the function)
        let arg_values: Vec<Value> = arg_local_ids
            .iter()
            .map(|id| self.get_local(*id).clone())
            .collect();

        match self.heap_get(closure_heap_id) {
            HeapValue::BuiltinFun(name) => {
                let name = name.clone();
                let builtin_fn = self.fixed_env
                    .builtin_funs
                    .get(name.as_str())
                    .ok_or_else(|| anyhow!("Unknown builtin: {}", name.as_str()))?;

                // For builtins, we need to create a temporary State (they expect State)
                // This is unfortunate but builtins are relatively rare
                let temp_state = self.snapshot_to_state();
                let results = builtin_fn(temp_state, arg_values)?;

                if results.is_empty() {
                    return Err(anyhow!("Builtin {} returned no results", name.as_str()));
                }

                // Use PathCounter to pick one result
                let choice = if results.len() > 1 {
                    self.path_counter.get_choice()
                } else {
                    false
                };
                let idx = if choice { 1.min(results.len() - 1) } else { 0 };
                let (result_state, return_value) = results.into_iter().nth(idx).unwrap();

                // Update our state from the result
                self.restore_from_state(result_state);
                self.set_local(local_id, return_value);

                Ok(ScalarCfgResult::Completed)
            }
            HeapValue::Closure(fun_def_name, captured_values) => {
                let fun_def_name = fun_def_name.clone();
                let captured_values = captured_values.clone();

                let (fun_def, prepared_cfg) = self.fixed_env
                    .fun_defs
                    .get(&fun_def_name)
                    .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;

                // Build new local_env efficiently using from_iter (single allocation)
                let capture_bindings = fun_def.capture_ids.iter()
                    .zip(captured_values.iter())
                    .map(|(id, value)| (*id, value.clone()));

                let arg_bindings = fun_def.arg_ids.iter()
                    .enumerate()
                    .filter_map(|(i, arg_id)| {
                        arg_id.map(|id| {
                            let value = arg_values.get(i).cloned().unwrap_or(Value::Nil(None));
                            (id, value)
                        })
                    });

                let new_local_env = LocalEnv::from_iter(capture_bindings.chain(arg_bindings));

                // Push new local_env (this is the key optimization - no State cloning!)
                self.push_local_env(new_local_env);

                // Run the function's CFG
                let cfg_result = run_cfg_scalar(
                    &prepared_cfg.cfg,
                    self,
                    None, // Start from entry
                    0,
                )?;

                match cfg_result {
                    ScalarCfgResult::Completed => {
                        // Function completed - pop and set return value
                        self.pop_local_env();
                        // Return value should have been set by the Return terminator
                        // For now, use Nil if no explicit return
                        // Actually, the return value is returned from run_cfg_scalar
                        Ok(ScalarCfgResult::Completed)
                    }
                    ScalarCfgResult::CompletedWithReturn(return_value) => {
                        // Function returned with a value
                        self.pop_local_env();
                        self.set_local(local_id, return_value.unwrap_or(Value::Nil(None)));
                        Ok(ScalarCfgResult::Completed)
                    }
                    ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, mut call_stack } => {
                        // Barrier hit inside function call - add this frame to call stack
                        call_stack.insert_front(CallFrame {
                            function_name: fun_def_name,
                            return_local_id: local_id,
                            resume_block,
                            instruction_index,
                            incoming_label: None, // Not used in old recursive path
                        });
                        Ok(ScalarCfgResult::BarrierYield {
                            barrier_id,
                            resume_block: None,
                            instruction_index: 0,
                            call_stack,
                        })
                    }
                    ScalarCfgResult::BranchYield { .. } => {
                        // BranchYield shouldn't happen from function calls using run_cfg_scalar
                        // (which uses PathCounter, not backtracking)
                        panic!("BranchYield from run_cfg_scalar in interpret_call - this shouldn't happen");
                    }
                }
            }
            _ => Err(anyhow!("Expected closure or builtin")),
        }
    }

    /// Create a State snapshot from current runtime state.
    /// This clones the runtime state to create a new State.
    /// Uses Arc-based cloning for O(1) local_env_stack clone.
    pub fn snapshot_to_state(&self) -> State {
        let (local_env, outer_local_envs) = self.local_env_stack.clone().into_state_components();

        State {
            heap: self.heap.clone(),
            local_env,
            outer_local_envs,
            global_env: self.global_env.clone(),
            prints: self.prints.clone(),
            vector_size: 1,
        }
    }

    /// Restore runtime state from a State (used after builtin calls).
    fn restore_from_state(&mut self, state: State) {
        self.heap = state.heap;
        self.global_env = state.global_env;
        self.prints = state.prints;
        // Restore local_env_stack
        self.local_env_stack = LocalEnvStack::from_state_components(&state.outer_local_envs, state.local_env);
    }
}

/// Result of running a CFG in scalar mode.
#[derive(Clone, Debug)]
pub enum ScalarCfgResult {
    /// CFG completed normally (no return value or void)
    Completed,
    /// CFG completed with an explicit return
    CompletedWithReturn(Option<Value>),
    /// Hit a barrier - need to yield
    BarrierYield {
        barrier_id: BarrierId,
        resume_block: Option<Label>,
        instruction_index: usize,
        call_stack: CallStack,
    },
    /// Hit an UnknownBool branch - yield for backtracking
    BranchYield {
        /// The block where this branch occurred
        branch_block: Option<Label>,
        /// The target for the "true" branch
        true_target: Label,
        /// The target for the "false" branch
        false_target: Label,
    },
}

/// Run a CFG in scalar mode until completion or barrier.
///
/// This is the scalar equivalent of run_cfg_to_barrier_or_completion but
/// it operates on a mutable ScalarRuntime instead of owned States.
pub fn run_cfg_scalar(
    cfg: &Cfg,
    runtime: &mut ScalarRuntime,
    start_block: Option<&Label>,
    start_instruction_index: usize,
) -> Result<ScalarCfgResult> {
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

    let mut incoming_label: Option<&Label> = None;
    let mut first_block = true;
    let mut instruction_start_index = start_instruction_index;

    loop {
        // Execute instructions
        for (inst_idx, (local_id, instruction)) in current_block.instructions.iter().enumerate() {
            if inst_idx < instruction_start_index {
                continue;
            }

            // Handle phi nodes
            if let Instruction::Phi { branches } = instruction {
                if let Some(from_label) = incoming_label {
                    for (branch_label, value_id) in branches {
                        if branch_label == from_label {
                            let value = runtime.get_local(*value_id).clone();
                            runtime.set_local(*local_id, value);
                            break;
                        }
                    }
                }
                continue;
            }

            // Handle calls
            if let Instruction::Call { closure, args } = instruction {
                let call_result = runtime.interpret_call(*local_id, *closure, args)?;

                match call_result {
                    ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
                        // Continue with next instruction
                    }
                    ScalarCfgResult::BarrierYield { barrier_id, call_stack, .. } => {
                        return Ok(ScalarCfgResult::BarrierYield {
                            barrier_id,
                            resume_block: current_block_label.cloned(),
                            instruction_index: inst_idx + 1,
                            call_stack,
                        });
                    }
                    ScalarCfgResult::BranchYield { .. } => {
                        panic!("BranchYield from interpret_call in run_cfg_scalar - this shouldn't happen");
                    }
                }
                continue;
            }

            // Handle other instructions
            runtime.interpret_non_call_instruction(*local_id, instruction)?;
        }

        instruction_start_index = 0;

        // Check for barrier (skip on first block when resuming)
        if !first_block {
            if let Some(barrier_id) = &current_block.barrier {
                return Ok(ScalarCfgResult::BarrierYield {
                    barrier_id: barrier_id.clone(),
                    resume_block: current_block_label.cloned(),
                    instruction_index: 0,
                    call_stack: CallStack::new(),
                });
            }
        }
        first_block = false;

        // Handle terminator
        let (_, terminator) = &current_block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| runtime.get_local(id).clone());
                return Ok(ScalarCfgResult::CompletedWithReturn(return_value));
            }
            Terminator::UnconditionalBranch { target } => {
                incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
                current_block_label = Some(target);
                current_block = cfg.named.get(target)
                    .ok_or_else(|| anyhow!("Unknown block: {:?}", target))?;
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                let condition_value = runtime.get_local(*condition);

                let take_true = match condition_value {
                    Value::Bool(MaybeVector::Scalar(b)) => *b,
                    Value::Bool(MaybeVector::Vector(_)) => {
                        panic!("Unexpected vector bool in scalar execution");
                    }
                    Value::UnknownBool => runtime.path_counter.get_choice(),
                    Value::Nil(_) => false,
                    Value::Number(_)
                    | Value::NumberInterval(_)
                    | Value::String(_)
                    | Value::Pointer(_) => true,
                    Value::NilPointer(_) => return Err(anyhow!("Nil pointer in condition")),
                };

                let target = if take_true { true_target } else { false_target };
                incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
                current_block_label = Some(target);
                current_block = cfg.named.get(target)
                    .ok_or_else(|| anyhow!("Unknown block: {:?}", target))?;
            }
        }

        // Check for barrier in new block
        if let Some(barrier_id) = &current_block.barrier {
            return Ok(ScalarCfgResult::BarrierYield {
                barrier_id: barrier_id.clone(),
                resume_block: current_block_label.cloned(),
                instruction_index: 0,
                call_stack: CallStack::new(),
            });
        }
    }
}

/// Run a CFG with backtracking - yields at UnknownBool branches.
///
/// Unlike run_cfg_scalar which uses PathCounter to make branch decisions,
/// this function yields control back to the caller at UnknownBool branches,
/// allowing the caller to checkpoint and implement backtracking.
///
/// The caller should:
/// 1. Call this function
/// 2. On BranchYield: checkpoint, pick a branch (usually false first), continue from that target
/// 3. On BarrierYield/Completed: record result, backtrack to try other branches
pub fn run_cfg_backtracking(
    cfg: &Cfg,
    runtime: &mut ScalarRuntime,
    start_block: Option<&Label>,
    start_instruction_index: usize,
) -> Result<ScalarCfgResult> {
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

    let mut incoming_label: Option<&Label> = None;
    let mut first_block = true;
    let mut instruction_start_index = start_instruction_index;

    loop {
        // Execute instructions
        for (inst_idx, (local_id, instruction)) in current_block.instructions.iter().enumerate() {
            if inst_idx < instruction_start_index {
                continue;
            }

            // Handle phi nodes
            if let Instruction::Phi { branches } = instruction {
                if let Some(from_label) = incoming_label {
                    for (branch_label, value_id) in branches {
                        if branch_label == from_label {
                            let value = runtime.get_local(*value_id).clone();
                            runtime.set_local(*local_id, value);
                            break;
                        }
                    }
                }
                continue;
            }

            // Handle calls - note: function calls may also hit UnknownBool inside
            // For now, we use PathCounter for calls (backtracking only at top level)
            if let Instruction::Call { closure, args } = instruction {
                let call_result = runtime.interpret_call(*local_id, *closure, args)?;

                match call_result {
                    ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
                        // Continue with next instruction
                    }
                    ScalarCfgResult::BarrierYield { barrier_id, call_stack, .. } => {
                        return Ok(ScalarCfgResult::BarrierYield {
                            barrier_id,
                            resume_block: current_block_label.cloned(),
                            instruction_index: inst_idx + 1,
                            call_stack,
                        });
                    }
                    ScalarCfgResult::BranchYield { .. } => {
                        // Branch inside function call - propagate up
                        // For now, we don't support backtracking inside function calls
                        panic!("BranchYield from function call not supported in backtracking mode");
                    }
                }
                continue;
            }

            // Handle other instructions
            runtime.interpret_non_call_instruction(*local_id, instruction)?;
        }

        instruction_start_index = 0;

        // Check for barrier (skip on first block when resuming)
        if !first_block {
            if let Some(barrier_id) = &current_block.barrier {
                return Ok(ScalarCfgResult::BarrierYield {
                    barrier_id: barrier_id.clone(),
                    resume_block: current_block_label.cloned(),
                    instruction_index: 0,
                    call_stack: CallStack::new(),
                });
            }
        }
        first_block = false;

        // Handle terminator
        let (_, terminator) = &current_block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| runtime.get_local(id).clone());
                return Ok(ScalarCfgResult::CompletedWithReturn(return_value));
            }
            Terminator::UnconditionalBranch { target } => {
                incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
                current_block_label = Some(target);
                current_block = cfg.named.get(target)
                    .ok_or_else(|| anyhow!("Unknown block: {:?}", target))?;
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                let condition_value = runtime.get_local(*condition);

                let take_true = match condition_value {
                    Value::Bool(MaybeVector::Scalar(b)) => Some(*b),
                    Value::Bool(MaybeVector::Vector(_)) => {
                        panic!("Unexpected vector bool in scalar execution");
                    }
                    Value::UnknownBool => None, // Yield for backtracking!
                    Value::Nil(_) => Some(false),
                    Value::Number(_)
                    | Value::NumberInterval(_)
                    | Value::String(_)
                    | Value::Pointer(_) => Some(true),
                    Value::NilPointer(_) => return Err(anyhow!("Nil pointer in condition")),
                };

                match take_true {
                    Some(b) => {
                        let target = if b { true_target } else { false_target };
                        incoming_label = current_block_label.or(Some(&ENTRY_LABEL));
                        current_block_label = Some(target);
                        current_block = cfg.named.get(target)
                            .ok_or_else(|| anyhow!("Unknown block: {:?}", target))?;
                    }
                    None => {
                        // UnknownBool - yield for backtracking!
                        return Ok(ScalarCfgResult::BranchYield {
                            branch_block: current_block_label.cloned(),
                            true_target: true_target.clone(),
                            false_target: false_target.clone(),
                        });
                    }
                }
            }
        }

        // Check for barrier in new block
        if let Some(barrier_id) = &current_block.barrier {
            return Ok(ScalarCfgResult::BarrierYield {
                barrier_id: barrier_id.clone(),
                resume_block: current_block_label.cloned(),
                instruction_index: 0,
                call_stack: CallStack::new(),
            });
        }
    }
}

/// Run from a call stack (resuming after barrier in nested calls).
pub fn resume_call_stack_scalar(
    runtime: &mut ScalarRuntime,
    mut call_stack: CallStack,
) -> Result<ScalarCfgResult> {
    while let Some(frame) = call_stack.pop() {
        let (_, prepared_cfg) = runtime.fixed_env
            .fun_defs
            .get(&frame.function_name)
            .ok_or_else(|| anyhow!("Unknown function: {:?}", frame.function_name))?;

        let result = run_cfg_scalar(
            &prepared_cfg.cfg,
            runtime,
            frame.resume_block.as_ref(),
            frame.instruction_index,
        )?;

        match result {
            ScalarCfgResult::Completed => {
                // Function completed with no return value
                runtime.pop_local_env();
                runtime.set_local(frame.return_local_id, Value::Nil(None));
            }
            ScalarCfgResult::CompletedWithReturn(return_value) => {
                // Function completed with explicit return
                runtime.pop_local_env();
                runtime.set_local(frame.return_local_id, return_value.unwrap_or(Value::Nil(None)));
            }
            ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, call_stack: inner_call_stack } => {
                // Hit another barrier - rebuild call stack
                let mut frames: Vec<CallFrame> = call_stack.iter().cloned().collect();
                frames.push(CallFrame {
                    function_name: frame.function_name,
                    return_local_id: frame.return_local_id,
                    resume_block,
                    instruction_index,
                    incoming_label: None, // Not used in old recursive path
                });
                frames.extend(inner_call_stack.iter().cloned());

                return Ok(ScalarCfgResult::BarrierYield {
                    barrier_id,
                    resume_block: None,
                    instruction_index: 0,
                    call_stack: CallStack::from_frames(frames),
                });
            }
            ScalarCfgResult::BranchYield { .. } => {
                // BranchYield shouldn't happen from run_cfg_scalar (uses PathCounter)
                panic!("BranchYield in resume_call_stack_scalar - this shouldn't happen");
            }
        }
    }

    Ok(ScalarCfgResult::Completed)
}

/// Run a scalar state from a barrier to the next barrier or completion.
/// This is the main entry point used by process_lane.
pub fn run_to_next_barrier_scalar(
    cfg: &Cfg,
    state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    start_block: Option<&Label>,
    start_instruction_index: usize,
    call_stack: CallStack,
) -> Result<BarrierRunResult> {
    let mut runtime = ScalarRuntime::from_state(state, fixed_env, path_counter);

    // If we have a call stack, resume inside nested calls first
    let (start_block, start_instruction_index) = if !call_stack.is_empty() {
        let result = resume_call_stack_scalar(&mut runtime, call_stack)?;

        match result {
            ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
                // All nested calls completed - continue from original resume point
                (start_block, start_instruction_index)
            }
            ScalarCfgResult::BarrierYield { barrier_id, call_stack, .. } => {
                // Hit a barrier in the call stack
                return Ok(BarrierRunResult {
                    state: runtime.into_state(),
                    destination: barrier_id,
                    call_stack,
                    top_level_resume_block: start_block.cloned(),
                    top_level_instruction_index: start_instruction_index,
                });
            }
            ScalarCfgResult::BranchYield { .. } => {
                panic!("BranchYield from resume_call_stack_scalar in run_to_next_barrier_scalar - this shouldn't happen");
            }
        }
    } else {
        (start_block, start_instruction_index)
    };

    // Run the top-level CFG
    let result = run_cfg_scalar(cfg, &mut runtime, start_block, start_instruction_index)?;

    match result {
        ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
            Ok(BarrierRunResult {
                state: runtime.into_state(),
                destination: end_barrier_id(),
                call_stack: CallStack::new(),
                top_level_resume_block: None,
                top_level_instruction_index: 0,
            })
        }
        ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, call_stack } => {
            Ok(BarrierRunResult {
                state: runtime.into_state(),
                destination: barrier_id,
                call_stack,
                top_level_resume_block: resume_block,
                top_level_instruction_index: instruction_index,
            })
        }
        ScalarCfgResult::BranchYield { .. } => {
            // BranchYield shouldn't happen from run_cfg_scalar (uses PathCounter)
            panic!("BranchYield in run_to_next_barrier_scalar - this shouldn't happen");
        }
    }
}

/// Result of running a path in-place on a ScalarRuntime.
/// Unlike BarrierRunResult, this doesn't contain the State - caller extracts it from runtime.
pub struct InplaceRunResult {
    /// The destination barrier (or END)
    pub destination: BarrierId,
    /// The call stack for resumption (empty if barrier was hit in top-level CFG)
    pub call_stack: CallStack,
    /// Block to resume in the top-level CFG
    pub top_level_resume_block: Option<Label>,
    /// Instruction index to resume at in the top-level CFG
    pub top_level_instruction_index: usize,
}

//=============================================================================
// FLAT EXECUTION LOOP
//=============================================================================
//
// This section implements a flat execution model where function calls don't
// use recursive calls to run_cfg_scalar. Instead, we maintain an explicit
// call stack and a single execution loop.
//
// This enables proper backtracking because UnknownBool branches in nested
// function calls are handled by the same loop that handles top-level branches.

/// A frame in the explicit execution stack.
/// Each frame represents one function call level.
#[derive(Clone, Debug)]
struct ExecutionFrame {
    /// Function name for CFG lookup (None = top-level _update function)
    function_name: Option<GlobalId>,
    /// Where to store return value in caller's local env
    return_local_id: Option<LocalId>,
    /// Current block label (None = entry block)
    current_block_label: Option<Label>,
    /// Current instruction index within the block
    instruction_index: usize,
    /// Incoming label for phi resolution (the block we came from)
    incoming_label: Option<Label>,
    /// Whether we've checked the barrier on the current block
    barrier_checked: bool,
    /// True if this frame was resumed from a CallStack (not a fresh call).
    /// When such a frame's callee returns, we don't increment instruction_index
    /// because the stored index already points past the Call.
    resumed_from_barrier: bool,
}

/// Run execution with a flat loop and explicit call stack.
/// This is the new architecture that enables proper backtracking across function calls.
pub fn run_flat(
    top_level_cfg: &Cfg,
    runtime: &mut ScalarRuntime,
    start_block: Option<Label>,
    start_instruction_index: usize,
    initial_call_stack: CallStack,
) -> Result<ScalarCfgResult> {
    // Build initial execution stack
    // The execution stack grows downward: index 0 is the top-level (_update),
    // and the last element is the current (innermost) function.
    let mut exec_stack: Vec<ExecutionFrame> = Vec::new();

    // Push the top-level frame first (index 0)
    let has_call_stack = !initial_call_stack.is_empty();
    exec_stack.push(ExecutionFrame {
        function_name: None, // Top-level uses top_level_cfg
        return_local_id: None,
        current_block_label: start_block,
        instruction_index: start_instruction_index,
        incoming_label: None,
        barrier_checked: true, // When resuming, skip barrier check for all frames
        resumed_from_barrier: has_call_stack, // If we have a call stack, we're resuming
    });

    // Push frames from initial_call_stack (oldest to newest)
    // The CallStack is ordered with most recent caller first, so we reverse
    let call_frames: Vec<_> = initial_call_stack.iter().cloned().collect();
    for frame in call_frames.into_iter().rev() {
        exec_stack.push(ExecutionFrame {
            function_name: Some(frame.function_name),
            return_local_id: Some(frame.return_local_id),
            current_block_label: frame.resume_block,
            instruction_index: frame.instruction_index,
            incoming_label: frame.incoming_label, // Restore for PHI nodes
            barrier_checked: true, // Resuming after barrier, skip barrier check
            resumed_from_barrier: true, // All frames from call stack are resumed
        });
    }

    // Debug: track barrier yields
    let debug_flat = std::env::var("DEBUG_FLAT").is_ok();

    if debug_flat {
        let frame_summary: Vec<String> = exec_stack.iter().enumerate().map(|(i, f)| {
            let block_str = f.current_block_label.as_ref()
                .map(|l| l.as_str())
                .unwrap_or("ENTRY");
            let incoming_str = f.incoming_label.as_ref()
                .map(|l| l.as_str())
                .unwrap_or("NONE");
            format!("{}:{}@{}:{}:in={}:r{}", i,
                f.function_name.as_ref().map(|g| g.as_str()).unwrap_or("TOP"),
                block_str,
                f.instruction_index,
                incoming_str,
                if f.resumed_from_barrier { "Y" } else { "N" })
        }).collect();
        eprintln!("[FLAT START] frames=[{}] local_env_stack_len={}", frame_summary.join(", "), runtime.local_env_stack.len());
    }

    // Main execution loop
    'main_loop: loop {
        // Get current frame
        let frame_idx = exec_stack.len() - 1;

        // Get the CFG for this frame
        let cfg: &Cfg = if exec_stack[frame_idx].function_name.is_none() {
            top_level_cfg
        } else {
            let fn_name = exec_stack[frame_idx].function_name.as_ref().unwrap();
            let (_, prepared) = runtime.fixed_env
                .fun_defs
                .get(fn_name)
                .ok_or_else(|| anyhow!("Unknown function: {:?}", fn_name))?;
            &prepared.cfg
        };

        // Get current block
        let block_label = exec_stack[frame_idx].current_block_label.clone();
        let block: &Block = match &block_label {
            None => &cfg.entry,
            Some(label) => cfg.named.get(label)
                .ok_or_else(|| anyhow!("Unknown block: {:?}", label))?,
        };

        // Check for barrier at start of block (unless we're resuming)
        if !exec_stack[frame_idx].barrier_checked {
            if let Some(barrier_id) = &block.barrier {
                // Yield at barrier
                let result = build_barrier_yield_from_exec_stack(
                    &exec_stack,
                    barrier_id.clone(),
                );
                if debug_flat {
                    let frame_summary: Vec<String> = exec_stack.iter().enumerate().map(|(i, f)| {
                        let block_str = f.current_block_label.as_ref()
                            .map(|l| l.as_str())
                            .unwrap_or("ENTRY");
                        let incoming_str = f.incoming_label.as_ref()
                            .map(|l| l.as_str())
                            .unwrap_or("NONE");
                        format!("{}:{}@{}:{}:in={}:r{}", i,
                            f.function_name.as_ref().map(|g| g.as_str()).unwrap_or("TOP"),
                            block_str,
                            f.instruction_index,
                            incoming_str,
                            if f.resumed_from_barrier { "Y" } else { "N" })
                    }).collect();
                    eprintln!("[FLAT YIELD] barrier={:?} frames=[{}]", barrier_id, frame_summary.join(", "));
                }
                return Ok(result);
            }
        }
        exec_stack[frame_idx].barrier_checked = false; // Reset for next block

        // Execute instructions
        while exec_stack[frame_idx].instruction_index < block.instructions.len() {
            let inst_idx = exec_stack[frame_idx].instruction_index;
            let (local_id, instruction) = &block.instructions[inst_idx];

            if debug_flat {
                let fn_name = exec_stack[frame_idx].function_name.as_ref()
                    .map(|g| g.as_str())
                    .unwrap_or("TOP");
                let block_str = exec_stack[frame_idx].current_block_label.as_ref()
                    .map(|l| l.as_str())
                    .unwrap_or("ENTRY");
                if fn_name == "foreach_1" {
                    eprintln!("[FLAT EXEC] fn={} block={} instr={} local_id={:?} type={:?}",
                        fn_name, block_str, inst_idx, local_id,
                        match instruction {
                            Instruction::Phi { .. } => "Phi",
                            Instruction::Call { .. } => "Call",
                            Instruction::NumberConstant { .. } => "NumberConstant",
                            Instruction::BinaryOp { .. } => "BinaryOp",
                            _ => "Other",
                        });
                }
            }

            // Handle phi nodes
            if let Instruction::Phi { branches } = instruction {
                if let Some(from_label) = &exec_stack[frame_idx].incoming_label {
                    for (branch_label, value_id) in branches {
                        if branch_label == from_label {
                            if debug_flat {
                                let block_str = exec_stack[frame_idx].current_block_label.as_ref()
                                    .map(|l| l.as_str())
                                    .unwrap_or("ENTRY");
                                let has_value = runtime.local_env_stack.last()
                                    .map(|env| env.contains_raw_id(usize::from(*value_id)))
                                    .unwrap_or(false);
                                let fn_name = exec_stack[frame_idx].function_name.as_ref()
                                    .map(|g| g.as_str())
                                    .unwrap_or("TOP");
                                eprintln!("[FLAT PHI] fn={} block={} from={} value_id={:?} -> local_id={:?} exists={} local_env_idx={}",
                                    fn_name, block_str, from_label.as_str(), value_id, local_id, has_value,
                                    runtime.local_env_stack.len() - 1);
                            }
                            let value = runtime.get_local(*value_id).clone();
                            runtime.set_local(*local_id, value);
                            break;
                        }
                    }
                } else if debug_flat {
                    let block_str = exec_stack[frame_idx].current_block_label.as_ref()
                        .map(|l| l.as_str())
                        .unwrap_or("ENTRY");
                    let fn_name = exec_stack[frame_idx].function_name.as_ref()
                        .map(|g| g.as_str())
                        .unwrap_or("TOP");
                    eprintln!("[FLAT PHI SKIP] fn={} block={} incoming_label=None", fn_name, block_str);
                }
                exec_stack[frame_idx].instruction_index += 1;
                continue;
            }

            // Handle call instructions
            if let Instruction::Call { closure, args } = instruction {
                let call_result = handle_call_flat(
                    runtime,
                    &mut exec_stack,
                    *local_id,
                    *closure,
                    args,
                )?;

                match call_result {
                    FlatCallResult::PushedFrame => {
                        // New frame was pushed, continue from new frame's entry
                        continue 'main_loop;
                    }
                    FlatCallResult::BuiltinCompleted => {
                        // Builtin completed, move to next instruction
                        exec_stack[frame_idx].instruction_index += 1;
                        continue;
                    }
                }
            }

            // Handle other instructions
            runtime.interpret_non_call_instruction(*local_id, instruction)?;
            exec_stack[frame_idx].instruction_index += 1;
        }

        // All instructions executed, handle terminator
        let (_, terminator) = &block.terminator;
        match terminator {
            Terminator::Return { value } => {
                let return_value = value.map(|id| runtime.get_local(id).clone());

                // Pop current frame
                let finished_frame = exec_stack.pop().unwrap();

                if debug_flat {
                    let fn_name = finished_frame.function_name.as_ref()
                        .map(|g| g.as_str())
                        .unwrap_or("TOP");
                    eprintln!("[FLAT RETURN] fn={} local_env_stack_before={}", fn_name, runtime.local_env_stack.len());
                }

                if exec_stack.is_empty() {
                    // Top-level function returned
                    return Ok(ScalarCfgResult::CompletedWithReturn(return_value));
                }

                // Pop local env for the returning function
                runtime.pop_local_env();
                if debug_flat {
                    eprintln!("[FLAT RETURN] local_env_stack_after={} exec_stack_len={}", runtime.local_env_stack.len(), exec_stack.len());
                }

                // Set return value in caller
                if let Some(ret_local_id) = finished_frame.return_local_id {
                    runtime.set_local(ret_local_id, return_value.unwrap_or(Value::Nil(None)));
                }

                // Move caller past the Call instruction
                // BUT: if the finished frame was resumed from barrier, its caller's
                // instruction_index already points past the call (stored +1), so don't increment
                let caller_idx = exec_stack.len() - 1;
                if !finished_frame.resumed_from_barrier {
                    exec_stack[caller_idx].instruction_index += 1;
                }

                // IMPORTANT: The caller is still in the same block. Don't re-check the barrier
                // (we already checked it when we first entered the block).
                exec_stack[caller_idx].barrier_checked = true;

                if debug_flat && exec_stack[caller_idx].resumed_from_barrier {
                    let f = &exec_stack[caller_idx];
                    let block_str = f.current_block_label.as_ref()
                        .map(|l| l.as_str())
                        .unwrap_or("ENTRY");
                    eprintln!("[FLAT RESUME_RETURN] caller={}@{}:{} finished_resumed={}",
                        f.function_name.as_ref().map(|g| g.as_str()).unwrap_or("TOP"),
                        block_str,
                        f.instruction_index,
                        finished_frame.resumed_from_barrier);
                }

                continue 'main_loop;
            }
            Terminator::UnconditionalBranch { target } => {
                let incoming = block_label.unwrap_or_else(|| ENTRY_LABEL.clone());
                if debug_flat {
                    let fn_name = exec_stack[frame_idx].function_name.as_ref()
                        .map(|g| g.as_str())
                        .unwrap_or("TOP");
                    eprintln!("[FLAT BRANCH] fn={} from={} to={}", fn_name, incoming.as_str(), target.as_str());
                }
                exec_stack[frame_idx].incoming_label = Some(incoming);
                exec_stack[frame_idx].current_block_label = Some(target.clone());
                exec_stack[frame_idx].instruction_index = 0;
                exec_stack[frame_idx].barrier_checked = false;
                continue 'main_loop;
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                let condition_value = runtime.get_local(*condition);

                let take_true = match condition_value {
                    Value::Bool(MaybeVector::Scalar(b)) => *b,
                    Value::Bool(MaybeVector::Vector(_)) => {
                        panic!("Unexpected vector bool in scalar execution");
                    }
                    Value::UnknownBool => runtime.path_counter.get_choice(),
                    Value::Nil(_) => false,
                    Value::Number(_)
                    | Value::NumberInterval(_)
                    | Value::String(_)
                    | Value::Pointer(_) => true,
                    Value::NilPointer(_) => return Err(anyhow!("Nil pointer in condition")),
                };

                let target = if take_true { true_target } else { false_target };
                let incoming = block_label.unwrap_or_else(|| ENTRY_LABEL.clone());
                if debug_flat {
                    let fn_name = exec_stack[frame_idx].function_name.as_ref()
                        .map(|g| g.as_str())
                        .unwrap_or("TOP");
                    eprintln!("[FLAT COND] fn={} from={} to={} (took {})", fn_name, incoming.as_str(), target.as_str(), if take_true { "true" } else { "false" });
                }
                exec_stack[frame_idx].incoming_label = Some(incoming);
                exec_stack[frame_idx].current_block_label = Some(target.clone());
                exec_stack[frame_idx].instruction_index = 0;
                exec_stack[frame_idx].barrier_checked = false;
                continue 'main_loop;
            }
        }
    }
}

/// Result of handling a call instruction in flat mode
enum FlatCallResult {
    /// A new frame was pushed onto exec_stack (for closures)
    PushedFrame,
    /// Builtin completed synchronously, return value already set
    BuiltinCompleted,
}

/// Handle a call instruction in flat execution mode.
/// For closures: pushes a new frame and sets up local env.
/// For builtins: executes synchronously and returns.
fn handle_call_flat(
    runtime: &mut ScalarRuntime,
    exec_stack: &mut Vec<ExecutionFrame>,
    local_id: LocalId,
    closure_local_id: LocalId,
    arg_local_ids: &[LocalId],
) -> Result<FlatCallResult> {
    let closure_heap_id = match runtime.get_local(closure_local_id) {
        Value::Pointer(heap_id) => *heap_id,
        Value::NilPointer(hint) => {
            return Err(anyhow!("Attempt to call nil ({})", hint));
        }
        _ => return Err(anyhow!("Expected pointer for closure")),
    };

    // Gather argument values
    let arg_values: Vec<Value> = arg_local_ids
        .iter()
        .map(|id| runtime.get_local(*id).clone())
        .collect();

    match runtime.heap_get(closure_heap_id) {
        HeapValue::BuiltinFun(name) => {
            let name = name.clone();
            let builtin_fn = runtime.fixed_env
                .builtin_funs
                .get(name.as_str())
                .ok_or_else(|| anyhow!("Unknown builtin: {}", name.as_str()))?;

            // Builtins need a State (unfortunate but they're relatively rare)
            let temp_state = runtime.snapshot_to_state();
            let results = builtin_fn(temp_state, arg_values)?;

            if results.is_empty() {
                return Err(anyhow!("Builtin {} returned no results", name.as_str()));
            }

            // Use PathCounter to pick one result
            let choice = if results.len() > 1 {
                runtime.path_counter.get_choice()
            } else {
                false
            };
            let idx = if choice { 1.min(results.len() - 1) } else { 0 };
            let (result_state, return_value) = results.into_iter().nth(idx).unwrap();

            // Update runtime from result
            runtime.restore_from_state(result_state);
            runtime.set_local(local_id, return_value);

            Ok(FlatCallResult::BuiltinCompleted)
        }
        HeapValue::Closure(fun_def_name, captured_values) => {
            let fun_def_name = fun_def_name.clone();
            let captured_values = captured_values.clone();

            let (fun_def, _prepared_cfg) = runtime.fixed_env
                .fun_defs
                .get(&fun_def_name)
                .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;

            // Build new local_env efficiently
            let capture_bindings = fun_def.capture_ids.iter()
                .zip(captured_values.iter())
                .map(|(id, value)| (*id, value.clone()));

            let arg_bindings = fun_def.arg_ids.iter()
                .enumerate()
                .filter_map(|(i, arg_id)| {
                    arg_id.map(|id| {
                        let value = arg_values.get(i).cloned().unwrap_or(Value::Nil(None));
                        (id, value)
                    })
                });

            let new_local_env = LocalEnv::from_iter(capture_bindings.chain(arg_bindings));

            // Push new local_env
            runtime.push_local_env(new_local_env);

            // NOTE: We do NOT modify the caller's resumed_from_barrier flag here.
            // That flag tracks whether the caller was restored from call_stack (and thus
            // has instruction_index already past the call). Making new nested calls doesn't
            // change that. For yield building, we determine which frames are "at a Call"
            // by checking if they have callees below them in exec_stack.

            // Push new execution frame
            exec_stack.push(ExecutionFrame {
                function_name: Some(fun_def_name),
                return_local_id: Some(local_id),
                current_block_label: None, // Start at entry
                instruction_index: 0,
                incoming_label: None,
                barrier_checked: false,
                resumed_from_barrier: false, // Fresh call, not resumed
            });

            Ok(FlatCallResult::PushedFrame)
        }
        _ => Err(anyhow!("Expected closure or builtin")),
    }
}

/// Build a BarrierYield result from the execution stack.
fn build_barrier_yield_from_exec_stack(
    exec_stack: &[ExecutionFrame],
    barrier_id: BarrierId,
) -> ScalarCfgResult {
    // The exec_stack has frames from oldest (top-level) to newest (current).
    // We need to build a CallStack with the nested function calls.
    // The top-level frame doesn't go in CallStack (it's represented by resume_block/instruction_index).
    //
    // IMPORTANT: The instruction_index stored should be the instruction AFTER the call,
    // so that when we resume, we don't re-execute the Call instruction.
    // For the innermost frame (where barrier was hit), instruction_index stays as-is (0 for barrier).
    // For caller frames, instruction_index should be +1 to skip past the Call - BUT only if the
    // frame is at a Call instruction (not already resumed past it).

    let debug_flat = std::env::var("DEBUG_FLAT").is_ok();

    if exec_stack.len() <= 1 {
        // Only top-level frame - barrier in top-level CFG
        let frame = &exec_stack[0];
        return ScalarCfgResult::BarrierYield {
            barrier_id,
            resume_block: frame.current_block_label.clone(),
            instruction_index: frame.instruction_index,
            call_stack: CallStack::new(),
        };
    }

    // Build call stack from nested frames (skip the first, which is top-level)
    let mut call_frames: Vec<CallFrame> = Vec::new();
    let num_frames = exec_stack.len();
    for i in 1..num_frames {
        let frame = &exec_stack[i];
        // For the innermost frame (last in exec_stack), use instruction_index as-is.
        // For caller frames, check if their callee was restored from call_stack:
        // - If callee (frame i+1) was resumed: this caller's instruction_index is already past the call
        // - If callee was a fresh call: add +1 to skip past the call
        let resume_instruction_index = if i == num_frames - 1 {
            frame.instruction_index
        } else {
            // Check if the callee (next frame) was restored from call_stack
            let callee_was_resumed = exec_stack[i + 1].resumed_from_barrier;
            if callee_was_resumed {
                frame.instruction_index // Caller's instruction_index is already past this call
            } else {
                frame.instruction_index + 1 // Skip past the call
            }
        };
        if debug_flat {
            let fn_name = frame.function_name.as_ref().map(|g| g.as_str()).unwrap_or("NONE");
            let block_str = frame.current_block_label.as_ref().map(|l| l.as_str()).unwrap_or("ENTRY");
            let callee_resumed = if i < num_frames - 1 { exec_stack[i + 1].resumed_from_barrier } else { false };
            eprintln!("[FLAT BUILD_YIELD] i={} fn={} block={} instr={} callee_resumed={} -> resume_instr={}",
                i, fn_name, block_str, frame.instruction_index, callee_resumed, resume_instruction_index);
        }
        call_frames.push(CallFrame {
            function_name: frame.function_name.clone().expect("nested frame should have function name"),
            return_local_id: frame.return_local_id.expect("nested frame should have return local id"),
            resume_block: frame.current_block_label.clone(),
            instruction_index: resume_instruction_index,
            incoming_label: frame.incoming_label.clone(),
        });
    }

    // CallStack wants frames ordered with most recent caller first
    call_frames.reverse();

    // Top-level instruction_index: check if its callee (frame 1) was restored from call_stack
    let top_frame = &exec_stack[0];
    let callee_was_resumed = exec_stack[1].resumed_from_barrier;
    let top_instruction_index = if callee_was_resumed {
        top_frame.instruction_index // Already past the call
    } else {
        top_frame.instruction_index + 1 // Skip past the call
    };
    if debug_flat {
        eprintln!("[FLAT BUILD_YIELD] TOP instr={} callee_resumed={} -> resume_instr={}",
            top_frame.instruction_index, callee_was_resumed, top_instruction_index);
    }
    ScalarCfgResult::BarrierYield {
        barrier_id,
        resume_block: top_frame.current_block_label.clone(),
        instruction_index: top_instruction_index,
        call_stack: CallStack::from_frames(call_frames),
    }
}

/// Run using the flat execution loop to the next barrier or completion.
/// This is the new entry point that replaces run_to_next_barrier_scalar.
pub fn run_to_next_barrier_flat(
    cfg: &Cfg,
    state: State,
    fixed_env: &FixedEnv,
    path_counter: &mut PathCounter,
    start_block: Option<&Label>,
    start_instruction_index: usize,
    call_stack: CallStack,
) -> Result<BarrierRunResult> {
    let mut runtime = ScalarRuntime::from_state(state, fixed_env, path_counter);

    let result = run_flat(
        cfg,
        &mut runtime,
        start_block.cloned(),
        start_instruction_index,
        call_stack,
    )?;

    match result {
        ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
            Ok(BarrierRunResult {
                state: runtime.into_state(),
                destination: end_barrier_id(),
                call_stack: CallStack::new(),
                top_level_resume_block: None,
                top_level_instruction_index: 0,
            })
        }
        ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, call_stack } => {
            Ok(BarrierRunResult {
                state: runtime.into_state(),
                destination: barrier_id,
                call_stack,
                top_level_resume_block: resume_block,
                top_level_instruction_index: instruction_index,
            })
        }
        ScalarCfgResult::BranchYield { .. } => {
            panic!("BranchYield from run_flat in run_to_next_barrier_flat - this shouldn't happen with PathCounter");
        }
    }
}

/// Run inplace using the flat execution loop.
/// This is the new entry point for process_lane that replaces run_to_next_barrier_inplace.
pub fn run_to_next_barrier_flat_inplace(
    cfg: &Cfg,
    runtime: &mut ScalarRuntime,
    start_block: Option<&Label>,
    start_instruction_index: usize,
    call_stack: CallStack,
) -> Result<InplaceRunResult> {
    let result = run_flat(
        cfg,
        runtime,
        start_block.cloned(),
        start_instruction_index,
        call_stack,
    )?;

    match result {
        ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
            Ok(InplaceRunResult {
                destination: end_barrier_id(),
                call_stack: CallStack::new(),
                top_level_resume_block: None,
                top_level_instruction_index: 0,
            })
        }
        ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, call_stack } => {
            Ok(InplaceRunResult {
                destination: barrier_id,
                call_stack,
                top_level_resume_block: resume_block,
                top_level_instruction_index: instruction_index,
            })
        }
        ScalarCfgResult::BranchYield { .. } => {
            panic!("BranchYield from run_flat - this shouldn't happen with PathCounter");
        }
    }
}

/// A checkpoint at a branch point for backtracking path enumeration.
/// Stores the runtime state and whether we've tried the "true" branch yet.
#[derive(Clone)]
struct BranchCheckpoint {
    /// Runtime state at this branch point
    checkpoint: RuntimeCheckpoint,
    /// The block and instruction index where this branch occurred
    block_label: Option<Label>,
    instruction_index: usize,
    /// The targets for true and false branches
    true_target: Label,
    false_target: Label,
    /// Whether we've already explored the "true" branch
    tried_true: bool,
}

/// State for backtracking path enumeration.
/// Uses a stack of checkpoints at branch points for efficient exploration.
pub struct BacktrackingState {
    /// Stack of branch checkpoints (innermost/most recent at end)
    branch_stack: Vec<BranchCheckpoint>,
    /// Counter for statistics
    paths_explored: usize,
}

impl BacktrackingState {
    pub fn new() -> Self {
        Self {
            branch_stack: Vec::new(),
            paths_explored: 0,
        }
    }

    /// Get the number of paths explored so far.
    pub fn paths_explored(&self) -> usize {
        self.paths_explored
    }

    /// Check if there are more branches to explore.
    pub fn has_pending_branches(&self) -> bool {
        self.branch_stack.iter().any(|b| !b.tried_true)
    }

    /// Pop back to a pending branch and take the other path.
    /// Returns None if all paths have been explored.
    /// Returns Some((checkpoint, true_target)) to restore and continue.
    pub fn backtrack(&mut self) -> Option<(RuntimeCheckpoint, Label)> {
        // Find the most recent branch that hasn't tried "true" yet
        while let Some(mut branch) = self.branch_stack.pop() {
            if !branch.tried_true {
                // Mark this branch as having tried true
                branch.tried_true = true;
                let target = branch.true_target.clone();
                let checkpoint = branch.checkpoint.clone();
                // Push it back so we can backtrack past it if needed
                self.branch_stack.push(branch);
                return Some((checkpoint, target));
            }
            // This branch has exhausted both options, continue popping
        }
        None
    }

    /// Record a branch point with a checkpoint.
    pub fn record_branch(
        &mut self,
        checkpoint: RuntimeCheckpoint,
        block_label: Option<Label>,
        instruction_index: usize,
        true_target: Label,
        false_target: Label,
    ) {
        self.branch_stack.push(BranchCheckpoint {
            checkpoint,
            block_label,
            instruction_index,
            true_target,
            false_target,
            tried_true: false,
        });
    }

    /// Called when a path completes (reaches barrier or end).
    pub fn path_completed(&mut self) {
        self.paths_explored += 1;
    }
}

/// Run a ScalarRuntime in-place from a barrier to the next barrier or completion.
///
/// This is optimized for path enumeration: it operates directly on the runtime
/// without State->ScalarRuntime->State conversions for each path.
///
/// The caller is responsible for checkpointing/restoring the runtime state
/// between paths using RuntimeCheckpoint.
pub fn run_to_next_barrier_inplace(
    cfg: &Cfg,
    runtime: &mut ScalarRuntime,
    start_block: Option<&Label>,
    start_instruction_index: usize,
    call_stack: CallStack,
) -> Result<InplaceRunResult> {
    // If we have a call stack, resume inside nested calls first
    let (start_block, start_instruction_index) = if !call_stack.is_empty() {
        let result = resume_call_stack_scalar(runtime, call_stack.clone())?;

        match result {
            ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
                // All nested calls completed - continue from original resume point
                (start_block, start_instruction_index)
            }
            ScalarCfgResult::BarrierYield { barrier_id, call_stack: new_call_stack, .. } => {
                // Hit a barrier in the call stack
                return Ok(InplaceRunResult {
                    destination: barrier_id,
                    call_stack: new_call_stack,
                    top_level_resume_block: start_block.cloned(),
                    top_level_instruction_index: start_instruction_index,
                });
            }
            ScalarCfgResult::BranchYield { .. } => {
                panic!("BranchYield from resume_call_stack_scalar - this shouldn't happen");
            }
        }
    } else {
        (start_block, start_instruction_index)
    };

    // Run the top-level CFG
    let result = run_cfg_scalar(cfg, runtime, start_block, start_instruction_index)?;

    match result {
        ScalarCfgResult::Completed | ScalarCfgResult::CompletedWithReturn(_) => {
            Ok(InplaceRunResult {
                destination: end_barrier_id(),
                call_stack: CallStack::new(),
                top_level_resume_block: None,
                top_level_instruction_index: 0,
            })
        }
        ScalarCfgResult::BarrierYield { barrier_id, resume_block, instruction_index, call_stack } => {
            Ok(InplaceRunResult {
                destination: barrier_id,
                call_stack,
                top_level_resume_block: resume_block,
                top_level_instruction_index: instruction_index,
            })
        }
        ScalarCfgResult::BranchYield { .. } => {
            panic!("BranchYield from run_cfg_scalar - this shouldn't happen");
        }
    }
}
