use std::collections::HashMap;

use super::{
    heap::HeapId,
    op::{interpret_binary_op, interpret_unary_op},
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::{Instruction, LocalId};
use anyhow::{anyhow, Result};

pub struct CoreInterpreter {
    state: State,
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

impl CoreInterpreter {
    pub fn new(state: State) -> Self {
        Self { state }
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

    pub fn interpret_instruction(&mut self, instruction: &Instruction) -> Result<Option<Value>> {
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
                    _ => Err(anyhow!(
                        "Value is of a type that can not be stored in a local"
                    )),
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
            Instruction::Call { closure, args } => todo!(),
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
}
