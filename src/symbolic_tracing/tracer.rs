//! TracingInterpreter: executes a concrete state through a CFG while tracking the path.
//!
//! Unlike the standard interpreter which uses vectorization to track multiple possibilities,
//! this interpreter:
//! 1. Always works with concrete states (vector_size = 1)
//! 2. When encountering abstract conditionals, picks one branch using PathCounter
//! 3. When functions return multiple/vectorized states, splits and picks one
//! 4. Records the path taken for caching and later exploration

use anyhow::{anyhow, Result};

use crate::interpreter::{
    core_interpreter::CoreInterpreter,
    fixed_env::FixedEnv,
    heap::HeapId,
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::{Cfg, Instruction, Label, Terminator};

use super::PathCounter;

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

/// Result of tracing a concrete state through a CFG.
#[derive(Clone, Debug)]
pub struct TracingResult {
    /// The output state after execution.
    pub output_state: State,
    /// The return value (if the function returned a value).
    pub return_value: Option<Value>,
    /// The path taken through the code.
    pub path: PathCounter,
    /// Number of "forced" choices where condition was truly abstract.
    /// If 0, this state had no ambiguous branches.
    pub forced_choices: usize,
}

/// Interprets a concrete state through a CFG, tracking the path taken.
pub struct TracingInterpreter<'a> {
    fixed_env: &'a FixedEnv,
    /// The path counter for making/tracking choices.
    path: PathCounter,
    /// Current position in the path (for replaying).
    choice_position: usize,
    /// Number of forced choices made.
    forced_choices: usize,
}

impl<'a> TracingInterpreter<'a> {
    /// Create a new tracing interpreter.
    /// If `path` is provided, we replay those choices; otherwise we explore with all-zeros.
    pub fn new(fixed_env: &'a FixedEnv, path: Option<PathCounter>) -> Self {
        Self {
            fixed_env,
            path: path.unwrap_or_default(),
            choice_position: 0,
            forced_choices: 0,
        }
    }

    /// Interpret a prepared CFG with a concrete state.
    pub fn interpret(
        mut self,
        cfg: &Cfg,
        mut state: State,
    ) -> Result<TracingResult> {
        // Start with the entry block
        let mut current_block_label: Option<Label> = None;

        loop {
            // Get the current block
            let block = if let Some(ref label) = current_block_label {
                cfg.named.get(label).ok_or_else(|| {
                    anyhow!("Block not found: {:?}", label)
                })?
            } else {
                &cfg.entry
            };

            // Execute all instructions in the block
            for (local_id, instruction) in &block.instructions {
                state = self.interpret_instruction(state, *local_id, instruction)?;
            }

            // Handle the terminator
            let (_, terminator) = &block.terminator;
            match terminator {
                Terminator::Return { value } => {
                    // Get return value if present
                    let return_value = value.map(|id| state.local_env.get(id).clone());

                    // Truncate path to actual choices made
                    self.path.truncate(self.choice_position);

                    return Ok(TracingResult {
                        output_state: state,
                        return_value,
                        path: self.path,
                        forced_choices: self.forced_choices,
                    });
                }
                Terminator::UnconditionalBranch { target } => {
                    current_block_label = Some(target.clone());
                }
                Terminator::ConditionalBranch {
                    condition,
                    true_target,
                    false_target,
                } => {
                    let cond_value = state.local_env.get(*condition);
                    let (take_true, is_forced) = self.evaluate_condition(cond_value)?;

                    if is_forced {
                        self.forced_choices += 1;
                    }

                    current_block_label = Some(if take_true {
                        true_target.clone()
                    } else {
                        false_target.clone()
                    });
                }
            }
        }
    }

    /// Evaluate a condition, returning (take_true_branch, is_forced_choice).
    fn evaluate_condition(&mut self, value: &Value) -> Result<(bool, bool)> {
        match value {
            // Concrete false or nil -> take false branch
            Value::Bool(MaybeVector::Scalar(false)) | Value::Nil(_) => {
                Ok((false, false))
            }
            // Concrete true or truthy value -> take true branch
            Value::Number(_)
            | Value::NumberInterval(_)
            | Value::Bool(MaybeVector::Scalar(true))
            | Value::String(_)
            | Value::Pointer(_) => {
                Ok((true, false))
            }
            // Abstract boolean -> need to make a choice
            Value::UnknownBool => {
                // 2 options: true (0) or false (1)
                let choice = self.path.record_choice(2, self.choice_position);
                self.choice_position += 1;
                let take_true = choice == 0;
                Ok((take_true, true))
            }
            // Vector of bools -> need to make a choice about which branch
            Value::Bool(MaybeVector::Vector(bools)) => {
                // Check if all same
                let has_true = bools.iter().any(|b| *b);
                let has_false = bools.iter().any(|b| !*b);

                if has_true && has_false {
                    // Mixed: need to choose
                    let choice = self.path.record_choice(2, self.choice_position);
                    self.choice_position += 1;
                    let take_true = choice == 0;
                    Ok((take_true, true))
                } else if has_true {
                    // All true
                    Ok((true, false))
                } else {
                    // All false
                    Ok((false, false))
                }
            }
            Value::NilPointer(_) => {
                Err(anyhow!("Nil pointer in condition"))
            }
        }
    }

    /// Interpret a single instruction.
    fn interpret_instruction(
        &mut self,
        state: State,
        local_id: crate::ir::LocalId,
        instruction: &Instruction,
    ) -> Result<State> {
        match instruction {
            Instruction::Call { .. } => {
                // For calls, use the existing interpreter but split vectorized results
                let interpreter = CoreInterpreter::new(state, self.fixed_env);
                let result_states = interpreter.interpret_call_instruction(local_id, instruction)?;

                if result_states.is_empty() {
                    return Err(anyhow!("Call returned no states"));
                }

                // Split any vectorized states into individual concrete states
                let mut all_concrete_states: Vec<State> = Vec::new();
                for result_state in result_states {
                    all_concrete_states.extend(split_vectorized_state(result_state));
                }

                // Now choose one concrete state based on path counter
                if all_concrete_states.len() == 1 {
                    Ok(all_concrete_states.into_iter().next().unwrap())
                } else {
                    // Multiple states: need to choose
                    let choice = self.path.record_choice(all_concrete_states.len(), self.choice_position);
                    self.choice_position += 1;
                    self.forced_choices += 1;
                    Ok(all_concrete_states.into_iter().nth(choice).unwrap())
                }
            }
            _ => {
                // Non-call instructions: use existing interpreter
                let mut interpreter = CoreInterpreter::new(state, self.fixed_env);
                interpreter.interpret_non_call_instruction(local_id, instruction)?;
                let result_state = interpreter.into_state();

                // Split if vectorized (shouldn't happen for non-call, but be safe)
                if result_state.vector_size > 1 {
                    let concrete_states = split_vectorized_state(result_state);
                    if concrete_states.len() == 1 {
                        Ok(concrete_states.into_iter().next().unwrap())
                    } else {
                        let choice = self.path.record_choice(concrete_states.len(), self.choice_position);
                        self.choice_position += 1;
                        self.forced_choices += 1;
                        Ok(concrete_states.into_iter().nth(choice).unwrap())
                    }
                } else {
                    Ok(result_state)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Basic tests would go here
    // For now, we'll test through integration tests
}
