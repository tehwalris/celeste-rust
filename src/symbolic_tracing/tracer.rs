//! TracingInterpreter: executes a concrete state through CFGs while tracking the path.
//!
//! Key design:
//! 1. Non-recursive: uses an explicit call stack instead of Rust recursion
//! 2. Path counter persists across call boundaries
//! 3. Always works with concrete scalar states (vector_size = 1)
//! 4. Abstract conditionals use path counter to pick a branch
//! 5. Tracks symbolic expressions showing how outputs are computed from inputs

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, Result};

use crate::interpreter::{
    fixed_env::FixedEnv,
    heap::HeapId,
    local_env::LocalEnv,
    state::State,
    symbolic::{ConcreteValue, SymExpr, SymbolGenerator, SymbolId},
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::{BinaryOp, Cfg, Instruction, Label, LocalId, Terminator, UnaryOp};
use crate::pico8_num::Pico8Num;

use super::PathCounter;

/// A stack frame for the tracing interpreter.
#[derive(Clone)]
struct CallFrame {
    /// The CFG we're executing (Arc for cheap cloning)
    cfg: Arc<Cfg>,
    /// Current block label (None = entry block)
    current_block: Option<Label>,
    /// Previous block label (for Phi resolution on return)
    previous_block: Option<Label>,
    /// Index of the call instruction within the block's non-phi instructions
    /// On return, we resume from instruction_index + 1
    instruction_index: usize,
    /// Where to store the return value
    return_local_id: LocalId,
    /// The caller's local_env (restored on return)
    caller_local_env: LocalEnv,
}

/// Mapping from SymbolId to the HeapId it was created from (for input symbols).
pub type InputSymbolMap = HashMap<SymbolId, HeapId>;

/// Mapping from HeapId to its symbolic expression.
pub type HeapSymbolMap = HashMap<HeapId, SymExpr>;

/// Tracks the concrete (value-dependent) branch outcomes during execution.
/// Each element is true if the branch went true, false if it went false.
pub type ConcretePath = Vec<bool>;

/// Result of tracing a concrete state through a CFG.
#[derive(Clone, Debug)]
pub struct TracingResult {
    /// The output state after execution.
    pub output_state: State,
    /// The return value (if the function returned a value).
    pub return_value: Option<Value>,
    /// The path taken through abstract branches (UnknownBool).
    pub path: PathCounter,
    /// The outcomes of concrete (value-dependent) branches.
    pub concrete_path: ConcretePath,
    /// Number of "forced" choices where condition was truly abstract.
    pub forced_choices: usize,
    /// Number of concrete branches taken (value-dependent, not abstract).
    /// If > 0, cache reuse requires matching concrete_path.
    pub concrete_branches: usize,
    /// Number of function calls made during tracing.
    pub function_calls: usize,
    /// Symbolic expressions for output heap values (how they're computed from inputs).
    pub heap_symbols: HeapSymbolMap,
    /// Mapping from input symbols to source heap locations.
    pub input_symbols: InputSymbolMap,
    /// Path conditions that must be satisfied for this path.
    pub path_conditions: Vec<SymExpr>,
    /// Symbolic condition expressions for concrete branches (without Not wrapper).
    /// Used to build branch templates for cache lookup.
    pub concrete_branch_conditions: Vec<SymExpr>,
    /// HeapIds that were allocated during tracing (not present in input).
    /// These need to be re-allocated when applying the cached trace.
    pub allocated_heap_ids: Vec<HeapId>,
}

/// Interprets a concrete state through CFGs, tracking the path taken.
/// Uses an explicit call stack instead of recursion.
pub struct TracingInterpreter<'a> {
    fixed_env: &'a FixedEnv,
    /// The mutable state being traced
    state: State,
    /// The path counter for making/tracking choices (persists across calls)
    path: PathCounter,
    /// Current position in the path (for replaying)
    choice_position: usize,
    /// Number of forced choices made
    forced_choices: usize,
    /// Number of concrete branches taken (value-dependent decisions)
    concrete_branches: usize,
    /// Outcomes of concrete (value-dependent) branches
    concrete_path: ConcretePath,
    /// Call stack
    call_stack: Vec<CallFrame>,
    /// Current CFG (None only before interpret() is called)
    current_cfg: Option<Arc<Cfg>>,
    /// Current block label (None = entry block)
    current_block: Option<Label>,
    /// Previous block label (for Phi node resolution)
    previous_block: Option<Label>,
    /// Resume instruction index (skip this many non-phi instructions when entering a block)
    resume_instruction_index: usize,

    // === Symbolic tracking ===
    /// Generator for fresh symbol IDs
    symbol_gen: SymbolGenerator,
    /// Symbolic expressions for local variables
    local_symbols: HashMap<LocalId, SymExpr>,
    /// Symbolic expressions for heap values
    heap_symbols: HeapSymbolMap,
    /// Mapping from input symbols to their source heap locations
    input_symbols: InputSymbolMap,
    /// Path conditions for this execution path (boolean expressions that must be true)
    path_conditions: Vec<SymExpr>,
    /// Concrete branch condition expressions (raw, without Not wrapper)
    concrete_branch_conditions: Vec<SymExpr>,
    /// HeapIds that existed in the input state (before tracing)
    input_heap_ids: std::collections::HashSet<HeapId>,
    /// HeapIds that were allocated during tracing
    allocated_heap_ids: Vec<HeapId>,
    /// Number of function calls made
    function_calls: usize,
}

impl<'a> TracingInterpreter<'a> {
    /// Create a new tracing interpreter.
    pub fn new(fixed_env: &'a FixedEnv, path: Option<PathCounter>) -> Self {
        Self {
            fixed_env,
            state: State::new(), // Will be replaced in interpret()
            path: path.unwrap_or_default(),
            choice_position: 0,
            forced_choices: 0,
            concrete_branches: 0,
            concrete_path: Vec::new(),
            call_stack: Vec::new(),
            current_cfg: None, // Will be set in interpret()
            current_block: None,
            previous_block: None,
            resume_instruction_index: 0,
            // Symbolic tracking
            symbol_gen: SymbolGenerator::new(),
            local_symbols: HashMap::new(),
            heap_symbols: HashMap::new(),
            input_symbols: HashMap::new(),
            path_conditions: Vec::new(),
            concrete_branch_conditions: Vec::new(),
            input_heap_ids: std::collections::HashSet::new(),
            allocated_heap_ids: Vec::new(),
            function_calls: 0,
        }
    }

    /// Interpret a CFG with a concrete state.
    pub fn interpret(mut self, cfg: Arc<Cfg>, state: State) -> Result<TracingResult> {
        debug_assert_eq!(state.vector_size, 1, "Tracing requires scalar state");

        self.state = state;
        self.current_cfg = Some(cfg);
        self.current_block = None;
        self.previous_block = None;

        // Record all HeapIds that exist in the input state
        // Any HeapIds allocated later are "new" and need to be re-allocated during apply()
        self.input_heap_ids = (0..self.state.heap.len())
            .map(HeapId::from_raw)
            .collect();

        // Initialize input symbols for all heap values reachable from globals
        self.initialize_input_symbols();

        // Main interpretation loop
        loop {
            // Clone the Arc (cheap refcount bump) so we can borrow the block across mutations
            let current_cfg = self.current_cfg.clone().expect("current_cfg should be set");

            // Get a reference to the current block (no cloning!)
            let block = if let Some(ref label) = self.current_block {
                current_cfg.named.get(label).ok_or_else(|| {
                    anyhow!("Block not found: {:?}", label)
                })?
            } else {
                &current_cfg.entry
            };

            // Split into phi and non-phi instructions
            let (phi_instructions, non_phi_instructions) = block.split_block_phi_instructions();

            // Skip phi instructions if resuming from a call return
            // (phi instructions were already processed before the call)
            if self.resume_instruction_index == 0 {
                // Process phi instructions if we have a previous block
                for (local_id, instruction) in phi_instructions {
                    match instruction {
                        Instruction::Phi { branches } => {
                            // Find the branch matching our previous block
                            let source_local_id = if let Some(ref prev_label) = self.previous_block {
                                branches
                                    .iter()
                                    .find(|(label, _)| label == prev_label)
                                    .map(|(_, local_id)| *local_id)
                                    .ok_or_else(|| anyhow!(
                                        "No Phi branch for previous block {:?}", prev_label
                                    ))?
                            } else {
                                // Entry block - should not have phi instructions
                                return Err(anyhow!("Phi instruction in entry block"));
                            };
                            // Copy value
                            let value = self.state.local_env.get(source_local_id).clone();
                            self.state.local_env.set(*local_id, value);
                            // Copy symbol
                            if let Some(sym) = self.local_symbols.get(&source_local_id).cloned() {
                                self.local_symbols.insert(*local_id, sym);
                            }
                        }
                        _ => return Err(anyhow!("Expected Phi instruction")),
                    }
                }
            }

            // Execute non-phi instructions (skip those before resume_instruction_index)
            let mut cfg_changed = false;
            let mut call_instruction_index = 0;
            for (idx, (local_id, instruction)) in non_phi_instructions.iter().enumerate() {
                if idx < self.resume_instruction_index {
                    continue; // Skip already-executed instructions
                }
                call_instruction_index = idx;
                if self.interpret_instruction(*local_id, instruction)? {
                    cfg_changed = true;
                    break; // CFG changed (closure call), restart loop
                }
            }

            // Reset resume index - if we continue from here, we've finished this block
            self.resume_instruction_index = 0;

            if cfg_changed {
                // Save the instruction index for when we return
                // The top of call_stack was just pushed by interpret_call
                if let Some(frame) = self.call_stack.last_mut() {
                    frame.instruction_index = call_instruction_index;
                }
                continue; // Restart with new CFG
            }

            // Handle the terminator
            let (_, terminator) = &block.terminator;
            match terminator {
                Terminator::Return { value } => {
                    // Get return value if present
                    let return_value = value.map(|id| self.state.local_env.get(id).clone());

                    // Pop from call stack
                    if let Some(frame) = self.call_stack.pop() {
                        // Pop the caller's local_env from outer_local_envs
                        // (it was pushed when we entered this function)
                        if !self.state.outer_local_envs.is_empty() {
                            self.state.outer_local_envs.remove(0);
                        }

                        // Restore caller's local_env and set return value
                        self.state.local_env = frame.caller_local_env;
                        let value = return_value
                            .unwrap_or_else(|| Value::Nil(Some("no return value".to_string())));
                        self.state.local_env.set(frame.return_local_id, value);

                        // Continue in caller's CFG from after the call instruction
                        self.current_cfg = Some(frame.cfg);
                        self.current_block = frame.current_block;
                        self.previous_block = frame.previous_block;
                        // Resume from the instruction AFTER the call
                        self.resume_instruction_index = frame.instruction_index + 1;

                        continue;
                    } else {
                        // Top-level return - we're done
                        self.path.truncate(self.choice_position);

                        return Ok(TracingResult {
                            output_state: self.state,
                            return_value,
                            path: self.path,
                            concrete_path: self.concrete_path,
                            forced_choices: self.forced_choices,
                            concrete_branches: self.concrete_branches,
                            function_calls: self.function_calls,
                            heap_symbols: self.heap_symbols,
                            input_symbols: self.input_symbols,
                            path_conditions: self.path_conditions,
                            concrete_branch_conditions: self.concrete_branch_conditions,
                            allocated_heap_ids: self.allocated_heap_ids,
                        });
                    }
                }
                Terminator::UnconditionalBranch { target } => {
                    // Use synthetic "__entry" label for entry block
                    self.previous_block = Some(self.current_block.clone()
                        .unwrap_or_else(|| Label::from("__entry".to_string())));
                    self.current_block = Some(target.clone());
                }
                Terminator::ConditionalBranch {
                    condition,
                    true_target,
                    false_target,
                } => {
                    let cond_value = self.state.local_env.get(*condition).clone();
                    let (take_true, is_forced, is_concrete) = self.evaluate_condition(&cond_value)?;

                    if is_forced {
                        self.forced_choices += 1;
                        // Record path condition for abstract branches
                        if let Some(cond_sym) = self.local_symbols.get(condition).cloned() {
                            let path_cond = if take_true {
                                cond_sym
                            } else {
                                SymExpr::Not(Arc::new(cond_sym))
                            };
                            self.path_conditions.push(path_cond);
                        }
                    }

                    // A concrete branch is only "value-dependent" if the condition's
                    // symbolic expression depends on input values. Pure constants
                    // (like `if true then`) are not value-dependent.
                    if is_concrete {
                        let cond_sym = self.local_symbols.get(condition);
                        let depends_on_inputs = cond_sym
                            .map(|sym| sym.has_inputs())
                            .unwrap_or(false);
                        if depends_on_inputs {
                            self.concrete_branches += 1;
                            self.concrete_path.push(take_true);
                            // Also record path condition for concrete branches
                            // This enables cache reuse by checking conditions
                            if let Some(cond_sym) = cond_sym.cloned() {
                                // Store raw condition (for branch template)
                                self.concrete_branch_conditions.push(cond_sym.clone());
                                // Store full path condition (with Not if false branch)
                                let path_cond = if take_true {
                                    cond_sym
                                } else {
                                    SymExpr::Not(Arc::new(cond_sym))
                                };
                                self.path_conditions.push(path_cond);
                            }
                        }
                    }

                    // Use synthetic "__entry" label for entry block
                    self.previous_block = Some(self.current_block.clone()
                        .unwrap_or_else(|| Label::from("__entry".to_string())));
                    self.current_block = Some(if take_true {
                        true_target.clone()
                    } else {
                        false_target.clone()
                    });
                }
            }
        }
    }

    /// Evaluate a condition, returning (take_true_branch, is_forced_choice, is_concrete_branch).
    fn evaluate_condition(&mut self, value: &Value) -> Result<(bool, bool, bool)> {
        match value {
            // Concrete false or nil -> take false branch
            Value::Bool(MaybeVector::Scalar(false)) | Value::Nil(_) => {
                Ok((false, false, true))
            }
            // Concrete true or truthy value -> take true branch
            Value::Number(_)
            | Value::NumberInterval(_)
            | Value::Bool(MaybeVector::Scalar(true))
            | Value::String(_)
            | Value::Pointer(_) => {
                Ok((true, false, true))
            }
            // Abstract boolean -> need to make a choice
            Value::UnknownBool => {
                let choice = self.path.record_choice(2, self.choice_position);
                self.choice_position += 1;
                let take_true = choice == 0;
                Ok((take_true, true, false))
            }
            // Vector of bools - shouldn't happen in scalar tracing
            Value::Bool(MaybeVector::Vector(_)) => {
                Err(anyhow!("Vector bool in scalar tracing - state should be scalar"))
            }
            Value::NilPointer(_) => {
                Err(anyhow!("Nil pointer in condition"))
            }
        }
    }

    /// Interpret a single instruction.
    /// Returns Ok(true) if the CFG changed (we switched to a callee's CFG) and
    /// the main loop should restart immediately. Returns Ok(false) to continue normally.
    fn interpret_instruction(
        &mut self,
        local_id: LocalId,
        instruction: &Instruction,
    ) -> Result<bool> {
        match instruction {
            Instruction::Call { closure, args } => {
                return self.interpret_call(local_id, *closure, args);
            }
            Instruction::Alloc => {
                let heap_id = self.tracked_alloc();
                self.state.local_env.set(local_id, Value::Pointer(heap_id));
            }
            Instruction::GetGlobal { name, create_if_missing } => {
                let heap_id = self.state.global_env.get(name);
                if let Some(&heap_id) = heap_id {
                    self.state.local_env.set(local_id, Value::Pointer(heap_id));
                } else if *create_if_missing {
                    let heap_id = self.tracked_alloc();
                    self.state.global_env.insert(name.clone(), heap_id);
                    self.state.local_env.set(local_id, Value::Pointer(heap_id));
                } else {
                    self.state.local_env.set(local_id, Value::NilPointer(format!("global {}", name)));
                }
            }
            Instruction::Load { source } => {
                let (value, sym) = match self.state.local_env.get(*source) {
                    Value::Pointer(heap_id) => match self.state.heap.get(*heap_id) {
                        HeapValue::Value(value) => {
                            // Get symbol from heap_symbols if available
                            let sym = self.heap_symbols.get(heap_id).cloned()
                                .unwrap_or_else(|| self.value_to_const_sym(value));
                            (value.clone(), sym)
                        }
                        HeapValue::Closure(_, _) | HeapValue::BuiltinFun(_) => {
                            (Value::Pointer(*heap_id), SymExpr::pointer(*heap_id))
                        }
                        HeapValue::ObjectTable(_)
                        | HeapValue::ArrayTable(_)
                        | HeapValue::UnknownTable => {
                            (Value::Pointer(*heap_id), SymExpr::pointer(*heap_id))
                        }
                    },
                    Value::NilPointer(hint) => {
                        (Value::Nil(Some(format!("nil pointer to {}", hint))), SymExpr::nil())
                    }
                    value => return Err(anyhow!("Load from non-pointer: {:?}", value)),
                };
                self.state.local_env.set(local_id, value);
                self.local_symbols.insert(local_id, sym);
            }
            Instruction::Store { target, source } => {
                let heap_id = self.get_heap_id(*target)?;
                let source_value = self.state.local_env.get(*source).clone();
                let source_sym = self.get_local_symbol(*source);
                self.state.heap.set(heap_id, HeapValue::Value(source_value));
                self.heap_symbols.insert(heap_id, source_sym);
            }
            Instruction::StoreEmptyTable { target } => {
                let heap_id = self.get_heap_id(*target)?;
                self.state.heap.set(heap_id, HeapValue::UnknownTable);
            }
            Instruction::StoreClosure { target, fun_def, captures } => {
                let heap_id = self.get_heap_id(*target)?;
                let captured_values: Vec<Value> = captures
                    .iter()
                    .map(|id| self.state.local_env.get(*id).clone())
                    .collect();
                self.state.heap.set(heap_id, HeapValue::Closure(fun_def.clone(), captured_values));
            }
            Instruction::GetField { receiver, field, create_if_missing } => {
                let table_heap_id = self.get_heap_id(*receiver)?;
                let field_heap_id = match self.state.heap.get(table_heap_id) {
                    HeapValue::ObjectTable(fields) => fields.get(field).copied(),
                    HeapValue::UnknownTable => None,
                    _ => return Err(anyhow!("GetField on non-object")),
                };
                if let Some(field_heap_id) = field_heap_id {
                    self.state.local_env.set(local_id, Value::Pointer(field_heap_id));
                } else if *create_if_missing {
                    let field_heap_id = self.tracked_alloc();
                    self.state.heap.set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    let field_clone = field.clone();
                    let hv = self.state.heap.get_mut(table_heap_id);
                    match hv {
                        HeapValue::ObjectTable(fields) => {
                            fields.insert(field_clone, field_heap_id);
                        }
                        HeapValue::UnknownTable => {
                            let mut fields = HashMap::new();
                            fields.insert(field_clone, field_heap_id);
                            *hv = HeapValue::ObjectTable(fields);
                        }
                        _ => {}
                    }
                    self.state.local_env.set(local_id, Value::Pointer(field_heap_id));
                } else {
                    self.state.local_env.set(local_id, Value::NilPointer(format!("field {}", field)));
                }
            }
            Instruction::GetIndex { receiver, index, create_if_missing } => {
                let table_heap_id = self.get_heap_id(*receiver)?;
                let index_val = match self.state.local_env.get(*index) {
                    Value::Number(MaybeVector::Scalar(n)) => n.as_i16()
                        .ok_or_else(|| anyhow!("Index is not an integer"))?,
                    _ => return Err(anyhow!("Index is not a scalar number")),
                };
                if index_val < 1 {
                    return Err(anyhow!("Index {} is less than 1", index_val));
                }
                let field_heap_id = match self.state.heap.get(table_heap_id) {
                    HeapValue::ArrayTable(items) => items.get(index_val as usize - 1).copied(),
                    HeapValue::UnknownTable => None,
                    _ => return Err(anyhow!("GetIndex on non-array")),
                };
                if let Some(field_heap_id) = field_heap_id {
                    self.state.local_env.set(local_id, Value::Pointer(field_heap_id));
                } else if *create_if_missing {
                    let field_heap_id = self.tracked_alloc();
                    self.state.heap.set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    let hv = self.state.heap.get_mut(table_heap_id);
                    match hv {
                        HeapValue::ArrayTable(items) => {
                            if index_val as usize == items.len() + 1 {
                                items.push(field_heap_id);
                            }
                        }
                        HeapValue::UnknownTable => {
                            if index_val == 1 {
                                *hv = HeapValue::ArrayTable(vec![field_heap_id]);
                            }
                        }
                        _ => {}
                    }
                    self.state.local_env.set(local_id, Value::Pointer(field_heap_id));
                } else {
                    self.state.local_env.set(local_id, Value::NilPointer(format!("index {}", index_val)));
                }
            }
            Instruction::NumberConstant { value } => {
                self.state.local_env.set(local_id, Value::Number(MaybeVector::Scalar(*value)));
                self.local_symbols.insert(local_id, SymExpr::num(*value));
            }
            Instruction::BoolConstant { value } => {
                self.state.local_env.set(local_id, Value::Bool(MaybeVector::Scalar(*value)));
                self.local_symbols.insert(local_id, SymExpr::bool(*value));
            }
            Instruction::StringConstant { value } => {
                self.state.local_env.set(local_id, Value::String(value.clone()));
                self.local_symbols.insert(local_id, SymExpr::Const(ConcreteValue::String(Arc::new(value.clone()))));
            }
            Instruction::NilConstant => {
                self.state.local_env.set(local_id, Value::Nil(None));
                self.local_symbols.insert(local_id, SymExpr::nil());
            }
            Instruction::UnaryOp { op, arg } => {
                let arg_val = self.state.local_env.get(*arg);
                let result = crate::interpreter::op::interpret_unary_op(&self.state, *op, arg_val)?;
                let arg_sym = self.get_local_symbol(*arg);
                let result_sym = self.build_unary_sym(*op, arg_sym);
                self.state.local_env.set(local_id, result);
                self.local_symbols.insert(local_id, result_sym);
            }
            Instruction::BinaryOp { left, op, right } => {
                let left_val = self.state.local_env.get(*left);
                let right_val = self.state.local_env.get(*right);
                let result = crate::interpreter::op::interpret_binary_op(left_val, *op, right_val)?;
                let left_sym = self.get_local_symbol(*left);
                let right_sym = self.get_local_symbol(*right);
                let result_sym = self.build_binary_sym(left_sym, *op, right_sym);
                self.state.local_env.set(local_id, result);
                self.local_symbols.insert(local_id, result_sym);
            }
            Instruction::Phi { .. } => {
                return Err(anyhow!("Phi nodes should not appear in tracing"));
            }
        }
        Ok(false)
    }

    /// Get HeapId from a pointer local
    fn get_heap_id(&self, local_id: LocalId) -> Result<HeapId> {
        match self.state.local_env.get(local_id) {
            Value::Pointer(heap_id) => Ok(*heap_id),
            Value::NilPointer(hint) => Err(anyhow!("Nil pointer: {}", hint)),
            value => Err(anyhow!("Expected pointer, got {:?}", value)),
        }
    }

    /// Allocate a new HeapId and track it as an allocation.
    fn tracked_alloc(&mut self) -> HeapId {
        let heap_id = self.state.heap.alloc();
        self.allocated_heap_ids.push(heap_id);
        heap_id
    }

    /// Interpret a call instruction.
    /// Returns Ok(true) if we switched to a callee's CFG (closure call).
    /// Returns Ok(false) if we handled the call inline (builtin).
    fn interpret_call(
        &mut self,
        return_local_id: LocalId,
        closure_local_id: LocalId,
        arg_local_ids: &[LocalId],
    ) -> Result<bool> {
        self.function_calls += 1;

        // Get the closure value
        let closure_heap_id = match self.state.local_env.get(closure_local_id) {
            Value::Pointer(heap_id) => *heap_id,
            Value::NilPointer(hint) => return Err(anyhow!("Call on nil: {}", hint)),
            value => return Err(anyhow!("Call on non-pointer: {:?}", value)),
        };

        let heap_value = self.state.heap.get(closure_heap_id).clone();

        // Gather argument values
        let arg_values: Vec<Value> = arg_local_ids
            .iter()
            .map(|id| self.state.local_env.get(*id).clone())
            .collect();

        match heap_value {
            HeapValue::BuiltinFun(name) => {
                // Call builtin directly
                let builtin_fn = self.fixed_env.builtin_funs.get(&name)
                    .ok_or_else(|| anyhow!("Unknown builtin: {}", name))?;

                // Builtins can return multiple states - we need to choose one
                let results = builtin_fn(self.state.clone(), arg_values)?;

                if results.is_empty() {
                    return Err(anyhow!("Builtin {} returned no states", name));
                }

                if results.len() == 1 {
                    let (new_state, return_value) = results.into_iter().next().unwrap();
                    self.state = new_state;
                    self.state.local_env.set(return_local_id, return_value);
                } else {
                    // Multiple results - use path counter to choose
                    let choice = self.path.record_choice(results.len(), self.choice_position);
                    self.choice_position += 1;
                    self.forced_choices += 1;

                    let (new_state, return_value) = results.into_iter().nth(choice).unwrap();
                    self.state = new_state;
                    self.state.local_env.set(return_local_id, return_value);
                }
                return Ok(false); // Builtin handled inline, continue normally
            }
            HeapValue::Closure(fun_def_name, captured_values) => {
                // Look up the function definition
                let (fun_def, prepared_cfg) = self.fixed_env.fun_defs.get(&fun_def_name)
                    .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;

                // Save current frame
                let caller_frame = CallFrame {
                    cfg: self.current_cfg.clone().expect("current_cfg should be set"),
                    current_block: self.current_block.clone(),
                    previous_block: self.previous_block.clone(),
                    instruction_index: 0, // Will be updated by interpret() after this returns
                    return_local_id,
                    caller_local_env: self.state.local_env.clone(),
                };
                self.call_stack.push(caller_frame);

                // Set up callee's local_env
                let mut new_local_env = LocalEnv::new();

                // Set up captured values
                for (capture_id, value) in fun_def.capture_ids.iter().zip(captured_values.iter()) {
                    new_local_env.set(*capture_id, value.clone());
                }

                // Set up argument values
                for (i, arg_id) in fun_def.arg_ids.iter().enumerate() {
                    if let Some(arg_id) = arg_id {
                        let value = arg_values.get(i).cloned()
                            .unwrap_or(Value::Nil(Some("missing argument".to_string())));
                        new_local_env.set(*arg_id, value);
                    }
                }

                // Switch to callee's context
                // Push caller's local_env onto outer_local_envs (for GC to find roots)
                let mut new_outer_local_envs = vec![self.state.local_env.clone()];
                new_outer_local_envs.extend(self.state.outer_local_envs.clone());
                self.state.outer_local_envs = new_outer_local_envs;

                self.state.local_env = new_local_env;
                self.current_cfg = Some(prepared_cfg.cfg.clone());
                self.current_block = None; // Start at entry block
                self.previous_block = None; // Entry block has no predecessor
                return Ok(true); // CFG changed, restart main loop
            }
            _ => {
                return Err(anyhow!("Call on non-callable: {:?}", heap_value));
            }
        }
    }

    /// Initialize input symbols for all heap values reachable from globals.
    /// This creates fresh SymbolIds for each heap slot that contains a value,
    /// which we can then trace through the execution.
    fn initialize_input_symbols(&mut self) {
        use std::collections::HashSet;

        // Visit all HeapIds reachable from globals and assign input symbols
        let mut visited: HashSet<HeapId> = HashSet::new();
        let mut to_visit: Vec<HeapId> = self.state.global_env.values().copied().collect();

        while let Some(heap_id) = to_visit.pop() {
            if visited.contains(&heap_id) {
                continue;
            }
            visited.insert(heap_id);

            // Get the heap value and process it
            let heap_value = self.state.heap.get(heap_id).clone();
            match &heap_value {
                HeapValue::Value(value) => {
                    // Create input symbol for this value
                    let sym_id = self.symbol_gen.fresh();
                    self.input_symbols.insert(sym_id, heap_id);
                    self.heap_symbols.insert(heap_id, SymExpr::Input(sym_id));
                }
                HeapValue::ObjectTable(fields) => {
                    // Visit all field heap IDs
                    for field_heap_id in fields.values() {
                        to_visit.push(*field_heap_id);
                    }
                }
                HeapValue::ArrayTable(items) => {
                    // Visit all item heap IDs
                    for item_heap_id in items {
                        to_visit.push(*item_heap_id);
                    }
                }
                HeapValue::Closure(_, captures) => {
                    // Visit captured values that are pointers
                    for capture in captures {
                        if let Value::Pointer(capture_heap_id) = capture {
                            to_visit.push(*capture_heap_id);
                        }
                    }
                }
                HeapValue::BuiltinFun(_) | HeapValue::UnknownTable => {
                    // No values to trace
                }
            }
        }
    }

    /// Get the symbol for a local variable, or create a constant symbol if none exists.
    fn get_local_symbol(&self, local_id: LocalId) -> SymExpr {
        if let Some(sym) = self.local_symbols.get(&local_id) {
            sym.clone()
        } else {
            // No symbol tracked - create a constant from the concrete value
            self.value_to_const_sym(self.state.local_env.get(local_id))
        }
    }

    /// Convert a concrete Value to a constant SymExpr.
    fn value_to_const_sym(&self, value: &Value) -> SymExpr {
        match value {
            Value::Number(MaybeVector::Scalar(n)) => {
                SymExpr::Const(ConcreteValue::Number(*n))
            }
            Value::NumberInterval(MaybeVector::Scalar(interval)) => {
                SymExpr::Const(ConcreteValue::NumberInterval(*interval))
            }
            Value::Bool(MaybeVector::Scalar(b)) => {
                SymExpr::Const(ConcreteValue::Bool(*b))
            }
            Value::String(s) => {
                SymExpr::Const(ConcreteValue::String(Arc::new(s.clone())))
            }
            Value::Nil(_) => SymExpr::nil(),
            Value::Pointer(heap_id) => {
                SymExpr::Const(ConcreteValue::Pointer(*heap_id))
            }
            Value::UnknownBool => {
                // Abstract bool - preserve as UnknownBool
                SymExpr::Const(ConcreteValue::UnknownBool)
            }
            Value::NilPointer(_) => SymExpr::nil(),
            Value::Number(MaybeVector::Vector(_))
            | Value::NumberInterval(MaybeVector::Vector(_))
            | Value::Bool(MaybeVector::Vector(_)) => {
                // Vectors shouldn't appear in scalar tracing
                SymExpr::nil()
            }
        }
    }

    /// Build the symbolic expression for a unary operation.
    fn build_unary_sym(&self, op: UnaryOp, arg_sym: SymExpr) -> SymExpr {
        let arg = Arc::new(arg_sym);
        match op {
            UnaryOp::Not => SymExpr::Not(arg),
            UnaryOp::Minus => SymExpr::Neg(arg),
            UnaryOp::Hash => {
                // Hash is the length operator - length depends on runtime value
                // For now, just return a placeholder (length is typically computed concretely)
                SymExpr::Const(ConcreteValue::Number(Pico8Num::from_i16(0)))
            }
        }
    }

    /// Build the symbolic expression for a binary operation.
    fn build_binary_sym(&self, left_sym: SymExpr, op: BinaryOp, right_sym: SymExpr) -> SymExpr {
        let left = Arc::new(left_sym);
        let right = Arc::new(right_sym);
        match op {
            BinaryOp::Plus => SymExpr::Add(left, right),
            BinaryOp::Minus => SymExpr::Sub(left, right),
            BinaryOp::Star => SymExpr::Mul(left, right),
            BinaryOp::Slash => SymExpr::Div(left, right),
            BinaryOp::Percent => SymExpr::Mod(left, right),
            BinaryOp::LessThan => SymExpr::Lt(left, right),
            BinaryOp::LessThanEqual => SymExpr::Le(left, right),
            BinaryOp::GreaterThan => SymExpr::Gt(left, right),
            BinaryOp::GreaterThanEqual => SymExpr::Ge(left, right),
            BinaryOp::TwoEqual => SymExpr::Eq(left, right),
            BinaryOp::TildeEqual => SymExpr::Ne(left, right),
            BinaryOp::Caret => {
                // Power operation - we don't have a SymExpr for this, use a placeholder
                // In practice, this should be rare and could be expanded later
                SymExpr::Mul(left, right) // Placeholder - not correct for actual power
            }
            BinaryOp::TwoDots => SymExpr::Concat(left, right),
        }
    }
}

#[cfg(test)]
mod tests {
    // Integration tests via trace_test binary
}
