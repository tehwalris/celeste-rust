//! Lightweight scalar runtime for execution between barriers.
//!
//! This module provides a mutable runtime that eliminates State cloning at function
//! boundaries. Instead of creating new State objects for each function call, we
//! use a stack of local environments and mutate shared heap/global_env/prints in place.
//!
//! Key optimization: Function calls just push/pop on local_env_stack (O(1)) instead
//! of cloning State components.

use rustc_hash::FxHashMap;

use anyhow::{anyhow, Result};

use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator, BarrierId};

use super::{
    barrier_executor::{CallFrame, CallStack, PathCounter, BarrierRunResult, end_barrier_id, ENTRY_LABEL},
    fixed_env::FixedEnv,
    heap::{Heap, HeapId},
    local_env::LocalEnv,
    op::{interpret_binary_op, interpret_unary_op},
    state::{GlobalEnv, Prints, OuterLocalEnvs, State},
    value::{HeapValue, MaybeVector, Value},
};

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
    pub local_env_stack: Vec<LocalEnv>,
    /// The fixed environment (function definitions, builtins)
    pub fixed_env: &'a FixedEnv,
    /// Path counter for abstract branching decisions
    pub path_counter: &'a mut PathCounter,
}

impl<'a> ScalarRuntime<'a> {
    /// Create a new scalar runtime from a State.
    pub fn from_state(state: State, fixed_env: &'a FixedEnv, path_counter: &'a mut PathCounter) -> Self {
        // Convert outer_local_envs + local_env into a stack
        // Stack order: [oldest outer, ..., newest outer, current]
        let mut local_env_stack: Vec<LocalEnv> = state.outer_local_envs.iter().cloned().collect();
        // outer_local_envs is stored with caller at index 0 (most recent caller)
        // We want oldest at index 0, so reverse
        local_env_stack.reverse();
        local_env_stack.push(state.local_env);

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
    pub fn into_state(mut self) -> State {
        let local_env = self.local_env_stack.pop().expect("local_env_stack should not be empty");
        // Reverse back: index 0 becomes most recent caller
        self.local_env_stack.reverse();
        let outer_local_envs = OuterLocalEnvs::from_vec(self.local_env_stack);

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
    /// This is O(1) amortized - no cloning of heap/global_env/prints!
    #[inline]
    pub fn push_local_env(&mut self, local_env: LocalEnv) {
        self.local_env_stack.push(local_env);
    }

    /// Pop the current function's local environment (for return).
    /// This is O(1) - no cloning!
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
                        });
                        Ok(ScalarCfgResult::BarrierYield {
                            barrier_id,
                            resume_block: None,
                            instruction_index: 0,
                            call_stack,
                        })
                    }
                }
            }
            _ => Err(anyhow!("Expected closure or builtin")),
        }
    }

    /// Create a State snapshot from current runtime state.
    fn snapshot_to_state(&self) -> State {
        let mut local_env_stack = self.local_env_stack.clone();
        let local_env = local_env_stack.pop().expect("stack not empty");
        local_env_stack.reverse();
        let outer_local_envs = OuterLocalEnvs::from_vec(local_env_stack);

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
        let mut new_stack: Vec<LocalEnv> = state.outer_local_envs.iter().cloned().collect();
        new_stack.reverse();
        new_stack.push(state.local_env);
        self.local_env_stack = new_stack;
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
                });
                frames.extend(inner_call_stack.iter().cloned());

                return Ok(ScalarCfgResult::BarrierYield {
                    barrier_id,
                    resume_block: None,
                    instruction_index: 0,
                    call_stack: CallStack::from_frames(frames),
                });
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
    }
}
