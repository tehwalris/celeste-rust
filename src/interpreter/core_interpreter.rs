use std::collections::HashMap;

use super::{
    fixed_env::FixedEnv,
    heap::HeapId,
    op::{interpret_binary_op, interpret_unary_op},
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::{Instruction, LocalId};
use anyhow::{anyhow, Result};

pub struct CoreInterpreter<'a> {
    state: State,
    fixed_env: &'a FixedEnv,
}

fn make_non_pointer_error(value: &Value) -> anyhow::Error {
    match value {
        Value::Pointer(_) | Value::NilPointer(_) => {
            panic!("failwith_not_pointer called with pointer")
        }
        Value::Nil(Some(hint)) => {
            anyhow!("Value is not a pointer (value is nil; {})", hint)
        }
        Value::Nil(None) => anyhow!("Value is not a pointer (value is nil)"),
        _ => anyhow!("Value is not a pointer"),
    }
}

impl<'a> CoreInterpreter<'a> {
    pub fn new(state: State, fixed_env: &'a FixedEnv) -> Self {
        Self { state, fixed_env }
    }

    pub fn into_state(self) -> State {
        self.state
    }

    fn heap_id_from_pointer_local(&self, local_id: LocalId) -> Result<HeapId> {
        match self.state.local_env.get(local_id) {
            Value::Pointer(heap_id) => Ok(*heap_id),
            Value::NilPointer(hint) => Err(anyhow!("Attempted to dereference nil ({})", hint)),
            value => Err(make_non_pointer_error(value)),
        }
    }

    fn interpret_non_call_instruction_no_assign(
        &mut self,
        instruction: &Instruction,
    ) -> Result<Option<Value>> {
        match instruction {
            Instruction::Alloc => {
                let heap_id = self.state.heap.alloc();
                Ok(Some(Value::Pointer(heap_id)))
            }
            Instruction::GetGlobal {
                name,
                create_if_missing,
            } => {
                let heap_id = self.state.global_env.get(name);
                if let Some(&heap_id) = heap_id {
                    Ok(Some(Value::Pointer(heap_id)))
                } else if *create_if_missing {
                    let heap_id = self.state.heap.alloc();
                    self.state.global_env.insert(name.clone(), heap_id);
                    Ok(Some(Value::Pointer(heap_id)))
                } else {
                    Ok(Some(Value::NilPointer(format!("global {}", name))))
                }
            }
            Instruction::Load { source } => match self.state.local_env.get(*source) {
                Value::Pointer(heap_id) => match self.state.heap.get(*heap_id) {
                    HeapValue::Value(value) => Ok(Some(value.clone())),
                    // Closures and builtins are returned as pointers - they are callable
                    HeapValue::Closure(_, _) | HeapValue::BuiltinFun(_) => {
                        Ok(Some(Value::Pointer(*heap_id)))
                    }
                    HeapValue::ObjectTable(_)
                    | HeapValue::ArrayTable(_)
                    | HeapValue::UnknownTable => {
                        // Tables are returned as pointers
                        Ok(Some(Value::Pointer(*heap_id)))
                    }
                },
                Value::NilPointer(hint) => {
                    Ok(Some(Value::Nil(Some(format!("nil pointer to {}", hint)))))
                }
                value => Err(make_non_pointer_error(value)),
            },
            Instruction::Store { target, source } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                let source_value = self.state.local_env.get(*source);
                self.state
                    .heap
                    .set(heap_id, HeapValue::Value(source_value.clone()));
                Ok(None)
            }
            Instruction::StoreEmptyTable { target } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                self.state.heap.set(heap_id, HeapValue::UnknownTable);
                Ok(None)
            }
            Instruction::StoreClosure {
                target,
                fun_def,
                captures,
            } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                let captured_values = captures
                    .iter()
                    .map(|id| self.state.local_env.get(*id))
                    .cloned()
                    .collect();
                self.state.heap.set(
                    heap_id,
                    HeapValue::Closure(fun_def.clone(), captured_values),
                );
                Ok(None)
            }
            Instruction::GetField {
                receiver,
                field,
                create_if_missing,
            } => {
                let table_heap_id = self.heap_id_from_pointer_local(*receiver)?;
                let field_heap_id = match self.state.heap.get(table_heap_id) {
                    HeapValue::ObjectTable(old_fields) => old_fields.get(field).copied(),
                    HeapValue::UnknownTable => None,
                    _ => {
                        return Err(anyhow!(
                            "GetField called on something that's not an object-like \
                             table or unknown table"
                        ))
                    }
                };
                if let Some(field_heap_id) = field_heap_id {
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    let field_heap_id = self.state.heap.alloc();
                    self.state
                        .heap
                        .set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.state.heap.get_mut(table_heap_id) {
                        HeapValue::ObjectTable(fields) => {
                            fields.insert(field.clone(), field_heap_id);
                        }
                        HeapValue::UnknownTable => {
                            let mut fields = HashMap::new();
                            fields.insert(field.clone(), field_heap_id);
                            self.state
                                .heap
                                .set(table_heap_id, HeapValue::ObjectTable(fields));
                        }
                        _ => unreachable!(),
                    }
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else {
                    Ok(Some(Value::NilPointer(format!("field {}", field))))
                }
            }
            Instruction::GetIndex {
                receiver,
                index,
                create_if_missing,
            } => {
                let table_heap_id = self.heap_id_from_pointer_local(*receiver)?;
                let index = match self.state.local_env.get(*index) {
                    Value::Number(MaybeVector::Scalar(index)) => {
                        let index = index
                            .as_i16()
                            .ok_or(anyhow!("Index is a scalar number, but not an integer"))?;
                        if index < 1 {
                            return Err(anyhow!("Index is less than 1"));
                        }
                        index
                    }
                    _ => return Err(anyhow!("Index is not a scalar number")),
                };
                let field_heap_id = match self.state.heap.get(table_heap_id) {
                    HeapValue::ArrayTable(old_fields) => {
                        old_fields.get(index as usize - 1).copied()
                    }
                    HeapValue::UnknownTable => None,
                    _ => {
                        return Err(anyhow!(
                            "GetIndex called on something that's not an array-like \
                             table or unknown table"
                        ))
                    }
                };
                if let Some(field_heap_id) = field_heap_id {
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    let field_heap_id = self.state.heap.alloc();
                    self.state
                        .heap
                        .set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.state.heap.get_mut(table_heap_id) {
                        HeapValue::ArrayTable(fields) => {
                            if index as usize != fields.len() + 1 {
                                return Err(anyhow!("Index is not the next index in the array"));
                            }
                            fields.push(field_heap_id);
                        }
                        HeapValue::UnknownTable => {
                            if index as usize != 1 {
                                return Err(anyhow!("Index is not the next index in the array"));
                            }
                            let mut fields = Vec::new();
                            fields.push(field_heap_id);
                            self.state
                                .heap
                                .set(table_heap_id, HeapValue::ArrayTable(fields));
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
            Instruction::StringConstant { value } => Ok(Some(Value::String(value.clone()))),
            Instruction::NilConstant => Ok(Some(Value::Nil(None))),
            Instruction::Call { .. } => {
                panic!("Call instruction passed to interpret_non_call_instruction")
            }
            Instruction::UnaryOp { op, arg } => {
                let arg = self.state.local_env.get(*arg);
                interpret_unary_op(&self.state, *op, arg).map(Some)
            }
            Instruction::BinaryOp { left, op, right } => {
                let left = self.state.local_env.get(*left);
                let right = self.state.local_env.get(*right);
                interpret_binary_op(left, *op, right).map(Some)
            }
            Instruction::Phi { branches } => {
                panic!("Phi nodes should not be handled at this level")
            }
        }
    }

    pub fn interpret_non_call_instruction(
        &mut self,
        local_id: LocalId,
        instruction: &Instruction,
    ) -> Result<()> {
        if let Some(value) = self.interpret_non_call_instruction_no_assign(instruction)? {
            self.state.local_env.set(local_id, value);
        }
        Ok(())
    }

    pub fn interpret_call_instruction(
        self,
        local_id: LocalId,
        instruction: &Instruction,
    ) -> Result<Vec<State>> {
        let (closure_local_id, arg_local_ids) = match instruction {
            Instruction::Call { closure, args } => (closure, args),
            _ => panic!("Non-call instruction passed to interpret_call_instruction"),
        };

        // Get the closure value from the local environment
        let closure_heap_id = match self.state.local_env.get(*closure_local_id) {
            Value::Pointer(heap_id) => *heap_id,
            Value::NilPointer(hint) => {
                return Err(anyhow!("Attempt to call nil ({})", hint));
            }
            value => {
                return Err(make_non_pointer_error(value));
            }
        };

        // Get the heap value
        let heap_value = self.state.heap.get(closure_heap_id).clone();

        // Gather argument values
        let arg_values: Vec<Value> = arg_local_ids
            .iter()
            .map(|id| self.state.local_env.get(*id).clone())
            .collect();

        match heap_value {
            HeapValue::BuiltinFun(name) => {
                // Look up the builtin function
                let builtin_fn = self
                    .fixed_env
                    .builtin_funs
                    .get(&name)
                    .ok_or_else(|| anyhow!("Unknown builtin function: {}", name))?;

                // Call the builtin, which returns multiple (state, return_value) pairs
                let results = builtin_fn(self.state, arg_values)?;

                // Set the return value in each state's local_env at the local_id
                let states = results
                    .into_iter()
                    .map(|(mut state, return_value)| {
                        state.local_env.set(local_id, return_value);
                        state
                    })
                    .collect();

                Ok(states)
            }
            HeapValue::Closure(fun_def_name, captured_values) => {
                // Look up the function definition
                let (fun_def, _prepared_cfg) = self
                    .fixed_env
                    .fun_defs
                    .get(&fun_def_name)
                    .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;
                let fun_cfg = fun_def.cfg.clone();

                // Create a new local_env for the function body
                let mut new_local_env = super::local_env::LocalEnv::new();

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
                // Note: We don't push outer_local_envs since functions get a fresh scope
                // and captures are handled via the captured_values
                let function_state = State {
                    heap: self.state.heap.clone(),
                    local_env: new_local_env,
                    outer_local_envs: Vec::new(), // Functions start with empty outer envs
                    global_env: self.state.global_env.clone(),
                    prints: self.state.prints.clone(),
                    vector_size: self.state.vector_size,
                };

                // Recursively interpret the function's CFG
                let result_states =
                    super::glue::interpret_cfg(fun_cfg, function_state, self.fixed_env)?;

                // For each result state, create an output state that:
                // 1. Takes the heap, global_env, and prints from the function execution
                // 2. Restores the caller's local_env
                // 3. Sets the return value at local_id
                let states = result_states
                    .into_iter()
                    .map(|function_result_state| {
                        let mut caller_state = State {
                            heap: function_result_state.heap,
                            local_env: self.state.local_env.clone(),
                            outer_local_envs: self.state.outer_local_envs.clone(),
                            global_env: function_result_state.global_env,
                            prints: function_result_state.prints,
                            vector_size: self.state.vector_size,
                        };
                        // Functions that don't return a value return nil
                        caller_state
                            .local_env
                            .set(local_id, Value::Nil(Some("no return value".to_string())));
                        caller_state
                    })
                    .collect();

                Ok(states)
            }
            _ => Err(anyhow!(
                "Attempt to call something that is not a function: {:?}",
                heap_value
            )),
        }
    }
}
