use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use super::{
    fixed_env::FixedEnv,
    heap::HeapId,
    op::{interpret_binary_op, interpret_select, interpret_unary_op},
    profiling::{DagOperation, SpanGuard, with_profiler},
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::ir::{Instruction, LocalId};
use anyhow::{anyhow, Context, Result};

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

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
        Self {
            state,
            fixed_env,
        }
    }

    pub fn into_state(self) -> State {
        self.state
    }

    /// `assert_true` under lane-granular deopt collection (see
    /// `deopt_collect`): lanes that falsify the premise have their origins
    /// captured and are dropped; the surviving lanes continue. Returns `None`
    /// when no lane survives - the caller drops the fragment.
    ///
    /// `UnknownBool` captures every lane: the premise cannot be confirmed for
    /// any of them, which is exactly the situation the plain re-run is for. A
    /// non-bool value is still a hard error - that is a broken program, not a
    /// falsified premise.
    pub fn collect_assert_true(self, value: LocalId) -> Result<Option<State>> {
        // Clone is an Arc bump for the vector case; it ends the borrow of
        // `state` so the captures below can take it.
        let condition = self.state.local_env.get(value).clone();
        match condition {
            Value::Bool(MaybeVector::Scalar(true)) => Ok(Some(self.state)),
            Value::Bool(MaybeVector::Scalar(false)) | Value::UnknownBool => {
                crate::interpreter::deopt_collect::capture_all(&self.state);
                Ok(None)
            }
            Value::Bool(MaybeVector::Vector(lanes)) => {
                if lanes.iter().all(|l| *l) {
                    return Ok(Some(self.state));
                }
                let keep: Vec<bool> = lanes.iter().copied().collect();
                crate::interpreter::deopt_collect::capture_dropped(&self.state, &keep);
                if keep.iter().all(|k| !k) {
                    return Ok(None);
                }
                Ok(Some(self.state.filter_by_mask_clone(
                    &keep,
                    crate::interpreter::state::FILTER_DEOPT,
                )))
            }
            other => Err(anyhow!(
                "AssertTrue(%{}) failed: expected a bool, got {:?}",
                usize::from(value),
                other
            )),
        }
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
            Instruction::AssertClosure { value, fun_def, captures } => {
                let actual = self.state.local_env.get(*value);
                let Value::Pointer(heap_id) = actual else {
                    return Err(anyhow!(
                        "AssertClosure({}) failed: expected a closure pointer, got {:?}",
                        fun_def.as_str(),
                        actual
                    ));
                };
                let (name, actual_captures) = match self.state.heap.get(*heap_id) {
                    HeapValue::Closure(name, actual_captures) => (name, actual_captures),
                    other => {
                        return Err(anyhow!(
                            "AssertClosure({}) failed: target is {:?}",
                            fun_def.as_str(),
                            other
                        ))
                    }
                };
                if name != fun_def {
                    return Err(anyhow!(
                        "AssertClosure({}) failed: closure is {}",
                        fun_def.as_str(),
                        name.as_str()
                    ));
                }
                if actual_captures.len() != captures.len() {
                    return Err(anyhow!(
                        "AssertClosure({}) failed: closure captured {} value(s), the assertion \
                         names {}",
                        fun_def.as_str(),
                        actual_captures.len(),
                        captures.len()
                    ));
                }
                // Compared by exact `Value` equality rather than anything
                // cleverer. This is the whole guarantee behind inlining a
                // closure with captures, so it should refuse anything it is not
                // certain about - a scalar and a vector of the same number are
                // deliberately not accepted as equal.
                let expected: Vec<Value> = captures
                    .iter()
                    .map(|id| self.state.local_env.get(*id).clone())
                    .collect();
                for (index, (want, got)) in expected.iter().zip(actual_captures.iter()).enumerate()
                {
                    if want != got {
                        return Err(anyhow!(
                            "AssertClosure({}) failed: capture {} is {:?}, the assertion names \
                             %{} which holds {:?}",
                            fun_def.as_str(),
                            index,
                            got,
                            usize::from(captures[index]),
                            want
                        ));
                    }
                }
                Ok(None)
            }
            Instruction::CallBuiltin { callee, name, args } => {
                let heap_id = self.heap_id_from_pointer_local(*callee)?;
                match self.state.heap.get(heap_id) {
                    HeapValue::BuiltinFun(actual) if actual == name => {}
                    other => {
                        return Err(anyhow!(
                            "CallBuiltin({}) failed: %{} is {:?}, not the builtin a rewrite \
                             pinned it to",
                            name,
                            usize::from(*callee),
                            other
                        ))
                    }
                }
                let f = self
                    .fixed_env
                    .pure_builtin_funs
                    .get(name)
                    .ok_or_else(|| anyhow!("{} is not a pure builtin", name))?;
                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|id| self.state.local_env.get(*id).clone())
                    .collect();
                Ok(Some(f(&arg_values)?))
            }
            Instruction::AssertPointer { value } => {
                match self.state.local_env.get(*value) {
                    Value::Pointer(_) => Ok(None),
                    Value::NilPointer(hint) => Err(anyhow!(
                        "AssertPointer({}) failed: it is nil ({}). A rewrite claimed this \
                         always exists by the time it is read.",
                        usize::from(*value),
                        hint
                    )),
                    other => Err(anyhow!(
                        "AssertPointer({}) failed: expected a pointer, got {:?}",
                        usize::from(*value),
                        other
                    )),
                }
            }
            Instruction::AssertValueCell { target } => {
                let heap_id = self.heap_id_from_pointer_local(*target)?;
                match self.state.heap.get(heap_id) {
                    HeapValue::Value(_) => Ok(None),
                    other => Err(anyhow!(
                        "AssertValueCell(%{}) failed: the cell holds {}, not a plain \
                         value. A rewrite claimed loading and storing this cell back \
                         is the identity, and for this cell it is not.",
                        usize::from(*target),
                        match other {
                            HeapValue::Closure(fun_def, _) =>
                                format!("a closure of {}", fun_def.as_str()),
                            HeapValue::BuiltinFun(name) => format!("builtin {}", name),
                            _ => "a table".to_string(),
                        }
                    )),
                }
            }
            Instruction::AssertTrue { value } => match self.state.local_env.get(*value) {
                Value::Bool(MaybeVector::Scalar(true)) => Ok(None),
                Value::Bool(MaybeVector::Scalar(false)) => Err(anyhow!(
                    "AssertTrue(%{}) failed: it is false. A rewrite stated this \
                     premise and this state falsifies it. ({})",
                    usize::from(*value),
                    crate::interpreter::inspect::describe_objects(&self.state)
                )),
                Value::Bool(MaybeVector::Vector(lanes)) => {
                    let false_lanes = lanes.iter().filter(|l| !**l).count();
                    if false_lanes == 0 {
                        Ok(None)
                    } else {
                        Err(anyhow!(
                            "AssertTrue(%{}) failed: false in {} of {} lanes. A \
                             rewrite stated this premise and these lanes falsify it.",
                            usize::from(*value),
                            false_lanes,
                            lanes.len()
                        ))
                    }
                }
                // A guard whose premise cannot be confirmed must not pass.
                Value::UnknownBool => Err(anyhow!(
                    "AssertTrue(%{}) failed: the value is an unknown bool, so the \
                     premise cannot be confirmed.",
                    usize::from(*value)
                )),
                other => Err(anyhow!(
                    "AssertTrue(%{}) failed: expected a bool, got {:?}",
                    usize::from(*value),
                    other
                )),
            },
            Instruction::GetGlobal {
                name,
                create_if_missing,
            } => {
                let heap_id = self.state.global_env.get(name);
                if let Some(&heap_id) = heap_id {
                    if *create_if_missing {
                        crate::create_sites::record(crate::create_sites::Site::Global, false);
                    }
                    Ok(Some(Value::Pointer(heap_id)))
                } else if *create_if_missing {
                    crate::create_sites::record(crate::create_sites::Site::Global, true);
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
                    if *create_if_missing {
                        crate::create_sites::record(crate::create_sites::Site::Field, false);
                    }
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    crate::create_sites::record(crate::create_sites::Site::Field, true);
                    let field_heap_id = self.state.heap.alloc();
                    self.state
                        .heap
                        .set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.state.heap.get(table_heap_id) {
                        HeapValue::ObjectTable(_) => {
                            let field = field.clone();
                            self.state.heap.modify(table_heap_id, |v| {
                                if let HeapValue::ObjectTable(fields) = v {
                                    fields.insert(field, field_heap_id);
                                }
                            });
                        }
                        HeapValue::UnknownTable => {
                            let mut fields = FxHashMap::default();
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
                            .ok_or_else(|| anyhow!("Index is a scalar number, but not an integer"))?;
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
                    if *create_if_missing {
                        crate::create_sites::record(crate::create_sites::Site::Index, false);
                    }
                    Ok(Some(Value::Pointer(field_heap_id)))
                } else if *create_if_missing {
                    crate::create_sites::record(crate::create_sites::Site::Index, true);
                    let field_heap_id = self.state.heap.alloc();
                    self.state
                        .heap
                        .set(field_heap_id, HeapValue::Value(Value::Nil(None)));
                    match self.state.heap.get(table_heap_id) {
                        HeapValue::ArrayTable(fields) => {
                            if index as usize != fields.len() + 1 {
                                return Err(anyhow!("Index is not the next index in the array"));
                            }
                            self.state.heap.modify(table_heap_id, |v| {
                                if let HeapValue::ArrayTable(fields) = v {
                                    fields.push(field_heap_id);
                                }
                            });
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
            Instruction::Select { condition, if_true, if_false } => {
                let condition = self.state.local_env.get(*condition);
                let if_true = self.state.local_env.get(*if_true);
                let if_false = self.state.local_env.get(*if_false);
                interpret_select(condition, if_true, if_false).map(Some)
            }
            Instruction::Phi { .. } => {
                panic!("Phi nodes should not be handled at this level")
            }
            Instruction::Kill { values } => {
                // Dropping a local is the whole point: it stops being copied
                // by every branch filter and stops being part of
                // `StateShape`, so two states that differ only in dead
                // temporaries can merge. Killing a slot that some later
                // definition already took over is a no-op rather than an
                // error - the value is gone either way, and `LocalEnv::get`
                // is what catches a genuine use-after-kill.
                for value in values {
                    self.state.local_env.kill(*value);
                }
                Ok(None)
            }
            Instruction::Expand { value } => {
                let current = self.state.local_env.get(*value).clone();
                match current {
                    // Already concrete - a fixed-input run, or a cell some
                    // earlier expand in this frame concretized. Identity.
                    concrete @ Value::Bool(_) => Ok(Some(concrete)),
                    Value::UnknownBool => {
                        let n = self.state.vector_size;
                        self.state.expand_lanes();
                        let mut lanes = Vec::with_capacity(2 * n);
                        lanes.resize(n, true);
                        lanes.resize(2 * n, false);
                        Ok(Some(Value::Bool(MaybeVector::vector(lanes))))
                    }
                    other => Err(anyhow!(
                        "Expand(%{}) expected a bool or unknown bool, got {:?}",
                        usize::from(*value),
                        other
                    )),
                }
            }
        }
    }

    pub fn interpret_non_call_instruction(
        &mut self,
        local_id: LocalId,
        instruction: &Instruction,
    ) -> Result<()> {
        // Naming the instruction costs nothing unless it fails, and a rewrite
        // that speculates an arm onto lanes that cannot take it fails *here* -
        // so this is what turns "something in the program went wrong" into a
        // location a recipe entry can be traced back to.
        let value = self
            .interpret_non_call_instruction_no_assign(instruction)
            .with_context(|| {
                format!(
                    "at %{} = {}",
                    usize::from(local_id),
                    crate::rewrite::print::format_instruction(instruction)
                )
            })?;
        if let Some(value) = value {
            // The one place a transient tri-state becomes a stored value.
            // Resolving here - rather than at each consumer - is what lets
            // `select` and the branch machinery stay untouched: by the time
            // they read a cell, its condition is definite.
            let value = match value {
                Value::MaybeBool(tri) => resolve_maybe_bool(&mut self.state, tri),
                definite => definite,
            };
            self.state.local_env.set(local_id, value);
        }
        Ok(())
    }

    pub fn interpret_call_instruction(
        mut self,
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
                let _span = SpanGuard::new_lazy(|| format!("builtin:{}", name), "call");

                // Look up the builtin function
                let builtin_fn = self
                    .fixed_env
                    .builtin_funs
                    .get(&name)
                    .ok_or_else(|| anyhow!("Unknown builtin function: {}", name))?;

                // Call the builtin, which returns multiple (state, return_value) pairs
                let results = builtin_fn(self.state, arg_values)?;

                // Track if this builtin caused a state split
                if results.len() > 1 {
                    let expanded_count: usize = results.iter().map(|(s, _)| s.vector_size).sum();
                    with_profiler(|p| {
                        p.create_dag_node(
                            results.len(),
                            expanded_count,
                            DagOperation::BuiltinSplit { builtin_name: name.clone() },
                            p.current_dag_node(),
                        )
                    });
                }

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
                // Look up the function definition with prepared CFG
                let (fun_def, prepared_cfg) = self
                    .fixed_env
                    .fun_defs
                    .get(&fun_def_name)
                    .ok_or_else(|| anyhow!("Unknown function: {:?}", fun_def_name))?;

                let _span = SpanGuard::new_with_source_lazy(
                    || format!("closure:{}", fun_def_name.as_str()),
                    "call",
                    fun_def.source_span.as_ref(),
                );

                // Create a new local_env for the function body, under the
                // callee's slot map - arguments and captures are written
                // through it, so it has to be in place before they are set.
                let mut new_local_env = super::local_env::LocalEnv::with_slots(
                    std::sync::Arc::clone(&prepared_cfg.cfg.slots),
                );

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

                // Create the state for executing the function body.
                // We push the caller's local_env onto outer_local_envs so it can be
                // restored after the function returns.
                //
                // We use std::mem::take to move fields instead of cloning - this is safe
                // because interpret_call_instruction takes `self` by value, so we own the state.
                let mut new_outer_local_envs = vec![std::mem::take(&mut self.state.local_env)];
                new_outer_local_envs.extend(std::mem::take(&mut self.state.outer_local_envs));

                let function_state = State {
                    heap: std::mem::take(&mut self.state.heap),
                    local_env: new_local_env,
                    outer_local_envs: new_outer_local_envs,
                    global_env: std::mem::take(&mut self.state.global_env),
                    prints: std::mem::take(&mut self.state.prints),
                    vector_size: self.state.vector_size,
                };

                // Recursively interpret the function's prepared CFG (uses cached labels)
                // Pass function name and source span for profiling
                let result_states = super::glue::interpret_prepared_cfg_with_name(
                    prepared_cfg,
                    function_state,
                    self.fixed_env,
                    Some(fun_def_name.as_str().to_string()),
                    fun_def.source_span,
                )?;

                // Track if this closure call caused a state split
                if result_states.len() > 1 {
                    let expanded_count: usize = result_states.iter().map(|(s, _)| s.vector_size).sum();
                    with_profiler(|p| {
                        p.create_dag_node(
                            result_states.len(),
                            expanded_count,
                            DagOperation::ClosureSplit { function_name: fun_def_name.as_str().to_string() },
                            p.current_dag_node(),
                        )
                    });
                }

                // NOTE: We intentionally do NOT auto-renormalize here because:
                // 1. Each state is paired with its specific return value
                // 2. Vectorizing states would lose the state-to-return-value association
                // 3. Auto-renormalization happens in flow.rs and glue.rs after calls complete

                // For each result state, create an output state that:
                // 1. Takes the heap, global_env, and prints from the function execution
                // 2. Restores the caller's local_env from the outer_local_envs stack
                // 3. Sets the return value at local_id
                let states = result_states
                    .into_iter()
                    .map(|(function_result_state, return_value)| {
                        // Pop the caller's local_env from the stack.
                        // This was pushed before the function call.
                        // We own function_result_state from into_iter(), so take ownership (no clone).
                        let (caller_local_env, remaining_outer_envs) = {
                            let mut envs = function_result_state.outer_local_envs;
                            let caller_env = envs.remove(0); // Pop the first (caller's) env
                            (caller_env, envs)
                        };

                        let mut caller_state = State {
                            heap: function_result_state.heap,
                            local_env: caller_local_env,
                            outer_local_envs: remaining_outer_envs,
                            global_env: function_result_state.global_env,
                            prints: function_result_state.prints,
                            vector_size: function_result_state.vector_size,
                        };
                        // Set the return value (or nil if none)
                        let value = return_value
                            .unwrap_or_else(|| Value::Nil(Some("no return value".to_string())));
                        caller_state.local_env.set(local_id, value);
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

/// Turn a transient `MaybeBool` into a definite `Bool`, widening the state
/// so that every ambiguous lane exists twice: the original resolved TRUE
/// and an appended copy resolved FALSE.
///
/// This is the whole tri-state mechanism. It is exact rather than an
/// approximation: the lane's interval genuinely admits both outcomes, so
/// both successors are reachable and listing both is what the abstraction
/// already means. Nothing is invented - which is why hulling the two arms
/// into one interval was rejected: that admits values no execution produces.
///
/// Definite lanes are untouched, so no lane's imprecision can reach a
/// neighbour (the LANE INDEPENDENCE criterion in plans/tristate-plan.md).
pub fn resolve_maybe_bool(state: &mut State, tri: MaybeVector<Option<bool>>) -> Value {
    let n = state.vector_size;
    let per_lane: Vec<Option<bool>> = match &tri {
        MaybeVector::Scalar(t) => vec![*t; n],
        MaybeVector::Vector(v) => {
            assert_eq!(v.len(), n, "tri-state width must match the state's lanes");
            v.as_ref().clone()
        }
    };
    let ambiguous: Vec<bool> = per_lane.iter().map(|t| t.is_none()).collect();
    let extra = crate::interpreter::value::count_true(&ambiguous);
    if extra == 0 {
        // All definite: identical to what the comparison used to return.
        return Value::Bool(MaybeVector::vector(
            per_lane.into_iter().map(|t| t.unwrap()).collect(),
        ));
    }
    state.duplicate_lanes(&ambiguous);
    // Originals: definite lanes keep their answer, ambiguous ones take the
    // TRUE resolution. Appended copies (in ambiguous-lane order, matching
    // `duplicate_lanes`) take the FALSE one.
    let mut lanes: Vec<bool> = per_lane.iter().map(|t| t.unwrap_or(true)).collect();
    lanes.resize(n + extra, false);
    Value::Bool(MaybeVector::vector(lanes))
}


#[cfg(test)]
mod expand_tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn num_vec(values: &[i16]) -> Value {
        Value::Number(MaybeVector::vector(
            values.iter().map(|v| Pico8Num::from_i16(*v)).collect(),
        ))
    }

    /// A 3-lane state: an `UnknownBool` in %0, a number vector in %1, a
    /// scalar in %2, and a heap cell holding another number vector.
    fn three_lane_state() -> (State, HeapId) {
        let mut state = State::new();
        state.vector_size = 3;
        state.local_env.set(id(0), Value::UnknownBool);
        state.local_env.set(id(1), num_vec(&[1, 2, 3]));
        state
            .local_env
            .set(id(2), Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(9))));
        let cell = state.heap.alloc();
        state.heap.set(cell, HeapValue::Value(num_vec(&[4, 5, 6])));
        (state, cell)
    }

    fn run_expand(state: State, value: LocalId) -> Result<State> {
        let fixed_env = FixedEnv::new();
        let mut interpreter = CoreInterpreter::new(state, &fixed_env);
        interpreter.interpret_non_call_instruction(id(10), &Instruction::Expand { value })?;
        Ok(interpreter.into_state())
    }

    /// The point of the instruction: an unknown bool becomes a per-lane
    /// vector - true on the first copy of every lane, false on the second -
    /// and every vector in the state doubles along with it.
    #[test]
    fn unknown_bool_doubles_the_state() {
        let (state, cell) = three_lane_state();
        let state = run_expand(state, id(0)).unwrap();

        assert_eq!(state.vector_size, 6);
        assert_eq!(
            state.local_env.get(id(10)),
            &Value::Bool(MaybeVector::vector(vec![true, true, true, false, false, false]))
        );
        // Env vector doubled, first copy then second.
        assert_eq!(state.local_env.get(id(1)), &num_vec(&[1, 2, 3, 1, 2, 3]));
        // Scalars broadcast; untouched.
        assert_eq!(
            state.local_env.get(id(2)),
            &Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(9)))
        );
        // Heap vectors double too.
        assert_eq!(state.heap.get(cell), &HeapValue::Value(num_vec(&[4, 5, 6, 4, 5, 6])));
        // The operand itself stays an unknown bool - expand reads it, it does
        // not redefine it.
        assert_eq!(state.local_env.get(id(0)), &Value::UnknownBool);
    }

    /// On a concrete bool - what a fixed-input run sees, or a cell an earlier
    /// expand already concretized - this is the identity.
    #[test]
    fn concrete_bool_is_identity() {
        let (mut state, cell) = three_lane_state();
        state
            .local_env
            .set(id(0), Value::Bool(MaybeVector::vector(vec![true, false, true])));
        let state = run_expand(state, id(0)).unwrap();

        assert_eq!(state.vector_size, 3);
        assert_eq!(
            state.local_env.get(id(10)),
            &Value::Bool(MaybeVector::vector(vec![true, false, true]))
        );
        assert_eq!(state.local_env.get(id(1)), &num_vec(&[1, 2, 3]));
        assert_eq!(state.heap.get(cell), &HeapValue::Value(num_vec(&[4, 5, 6])));
    }

    #[test]
    fn scalar_bool_is_identity() {
        let (mut state, _) = three_lane_state();
        state.local_env.set(id(0), Value::Bool(MaybeVector::Scalar(false)));
        let state = run_expand(state, id(0)).unwrap();
        assert_eq!(state.vector_size, 3);
        assert_eq!(
            state.local_env.get(id(10)),
            &Value::Bool(MaybeVector::Scalar(false))
        );
    }

    /// Anything that is not a bool fails loudly rather than being coerced -
    /// Lua truthiness has no business here, the operand is a button state.
    #[test]
    fn non_bool_fails() {
        let (state, _) = three_lane_state();
        let error = format!("{:#}", run_expand(state, id(1)).unwrap_err());
        assert!(error.contains("expected a bool"), "{}", error);
    }

    /// Only the ambiguous lanes duplicate - the point of the whole design.
    /// Three lanes, one ambiguous: four lanes out, not six.
    #[test]
    fn resolve_duplicates_only_the_ambiguous_lanes() {
        let (mut state, cell) = three_lane_state();
        let tri = MaybeVector::vector(vec![Some(true), None, Some(false)]);
        let resolved = resolve_maybe_bool(&mut state, tri);

        assert_eq!(state.vector_size, 4, "one ambiguous lane adds exactly one");
        // Originals keep their answers; the ambiguous one resolves true and
        // its appended copy resolves false.
        assert_eq!(
            resolved,
            Value::Bool(MaybeVector::vector(vec![true, true, false, false]))
        );
        // The duplicated lane carries the ambiguous lane's DATA, so the
        // appended lane must equal lane 1 in every other column.
        match state.heap.get(cell) {
            HeapValue::Value(Value::Number(MaybeVector::Vector(nums))) => {
                assert_eq!(nums.len(), 4);
                assert_eq!(nums[3], nums[1], "copy must carry the source lane's data");
            }
            other => panic!("unexpected cell {:?}", other),
        }
    }

    /// An all-definite tri-state must behave exactly as the old code did:
    /// a plain Bool and no widening at all.
    #[test]
    fn resolve_is_a_no_op_when_every_lane_is_definite() {
        let (mut state, _) = three_lane_state();
        let tri = MaybeVector::vector(vec![Some(true), Some(false), Some(true)]);
        let resolved = resolve_maybe_bool(&mut state, tri);
        assert_eq!(state.vector_size, 3);
        assert_eq!(
            resolved,
            Value::Bool(MaybeVector::vector(vec![true, false, true]))
        );
    }

    /// LANE INDEPENDENCE in miniature: a definite lane's answer must not
    /// depend on whether an ambiguous lane shares its state. Resolving
    /// lane 0 alone and resolving it batched must agree.
    #[test]
    fn a_definite_lane_is_unaffected_by_an_ambiguous_neighbour() {
        let (mut batched, _) = three_lane_state();
        let with_neighbour =
            resolve_maybe_bool(&mut batched, MaybeVector::vector(vec![Some(true), None, Some(false)]));
        let batched_lane0 = match &with_neighbour {
            Value::Bool(MaybeVector::Vector(v)) => v[0],
            Value::Bool(MaybeVector::Scalar(b)) => *b,
            other => panic!("expected bools, got {:?}", other),
        };

        let (mut alone, _) = three_lane_state();
        let solo = resolve_maybe_bool(
            &mut alone,
            MaybeVector::vector(vec![Some(true), Some(true), Some(false)]),
        );
        let solo_lane0 = match &solo {
            Value::Bool(MaybeVector::Vector(v)) => v[0],
            Value::Bool(MaybeVector::Scalar(b)) => *b,
            other => panic!("expected bools, got {:?}", other),
        };
        assert_eq!(batched_lane0, solo_lane0);
    }

    /// End-to-end reproduction of the room-(0,0) strawberry failure, which
    /// is the reason this machinery exists. The archived sweep log shows:
    ///
    ///   at %157 = select %125 ? %154 : %125: select on a condition with
    ///   no per-lane value: UnknownBool ... whole-state fallback
    ///   (in fruit.update_34 -> obj.collide_49)
    ///
    /// A three-lane comparison where ONE lane straddles a widened interval:
    /// before, that collapsed the whole value and the select below failed,
    /// dropping the entire state onto the plain program. Now the definite
    /// lanes keep their answers, only the straddler duplicates, and the
    /// select succeeds.
    #[test]
    fn a_straddling_lane_no_longer_poisons_the_select() {
        use crate::interpreter::op::{interpret_binary_op, interpret_select, set_tri_state};
        use crate::ir::BinaryOp;
        use crate::pico8_num::Pico8NumInterval;
        set_tri_state(true);

        let n = |v: i16| Pico8Num::from_i16(v);
        // Lane 0: [0,1] < 10   -> definitely true
        // Lane 1: [5,15] < 10  -> STRADDLES
        // Lane 2: [20,30] < 10 -> definitely false
        let fruit_y = Value::NumberInterval(MaybeVector::vector(vec![
            Pico8NumInterval::new(n(0), n(1)),
            Pico8NumInterval::new(n(5), n(15)),
            Pico8NumInterval::new(n(20), n(30)),
        ]));
        let threshold = Value::Number(MaybeVector::Scalar(n(10)));

        let compared =
            interpret_binary_op(&fruit_y, BinaryOp::LessThan, &threshold).unwrap();
        let tri = match compared {
            Value::MaybeBool(tri) => tri,
            other => panic!("expected a mixed tri-state, got {:?}", other),
        };

        let (mut state, _) = three_lane_state();
        let condition = resolve_maybe_bool(&mut state, tri);
        assert_eq!(state.vector_size, 4, "only the straddling lane duplicates");

        // The definite lanes kept the answers they would have had alone.
        assert_eq!(
            condition,
            Value::Bool(MaybeVector::vector(vec![true, true, false, false]))
        );

        // And the select that used to fail now resolves per lane.
        let a = Value::Number(MaybeVector::Scalar(n(7)));
        let b = Value::Number(MaybeVector::Scalar(n(9)));
        let picked = interpret_select(&condition, &a, &b).unwrap();
        assert_eq!(
            picked,
            Value::Number(MaybeVector::vector(vec![n(7), n(7), n(9), n(9)])),
            "straddling lane appears once per resolution, definite lanes untouched"
        );
    }

    /// An ALL-straddling comparison keeps the old whole-value behaviour, so
    /// pre-existing paths are bit-for-bit unchanged.
    /// The gate has two states and only one of them was ever exercised.
    ///
    /// With tri-state OFF, a MIXED comparison - some lanes definite, some
    /// straddling - must collapse to `UnknownBool`, which is what this arm
    /// did before tri-state existed. It instead fell into the all-definite
    /// arm and unwrapped a `None`, which aborted the room (0,0) campaign at
    /// frame 66 (the widened strawberry produces straddling comparisons
    /// constantly; room (1,0) produces none, so every gate ran green).
    #[test]
    fn a_mixed_comparison_with_tri_state_off_collapses_instead_of_panicking() {
        use crate::interpreter::op::{interpret_binary_op, set_tri_state};
        use crate::pico8_num::{Pico8Num, Pico8NumInterval};

        let n = |v: i16| Pico8Num::from_i16(v);
        // Lane 0 is definitely less; lane 1 straddles.
        let left = Value::NumberInterval(MaybeVector::vector(vec![
            Pico8NumInterval::new(n(0), n(1)),
            Pico8NumInterval::new(n(0), n(10)),
        ]));
        let right = Value::Number(MaybeVector::Scalar(n(5)));

        set_tri_state(false);
        let off = interpret_binary_op(&left, crate::ir::BinaryOp::LessThan, &right)
            .expect("a mixed comparison must not error");
        assert_eq!(
            off,
            Value::UnknownBool,
            "with the gate off, a straddling lane must collapse the whole value"
        );

        set_tri_state(true);
        let on = interpret_binary_op(&left, crate::ir::BinaryOp::LessThan, &right)
            .expect("a mixed comparison must not error");
        assert!(
            matches!(on, Value::MaybeBool(_)),
            "with the gate on, the definite lanes keep their answers, got {:?}",
            on
        );
        set_tri_state(false);
    }

    #[test]
    fn an_all_straddling_comparison_is_still_unknown_bool() {
        use crate::interpreter::op::{interpret_binary_op, set_tri_state};
        use crate::ir::BinaryOp;
        use crate::pico8_num::Pico8NumInterval;
        set_tri_state(true);

        let n = |v: i16| Pico8Num::from_i16(v);
        let straddling = Value::NumberInterval(MaybeVector::vector(vec![
            Pico8NumInterval::new(n(5), n(15)),
            Pico8NumInterval::new(n(0), n(20)),
        ]));
        let threshold = Value::Number(MaybeVector::Scalar(n(10)));
        let compared =
            interpret_binary_op(&straddling, BinaryOp::LessThan, &threshold).unwrap();
        assert_eq!(compared, Value::UnknownBool);
    }

    /// duplicate_lanes with an empty mask must not touch the state.
    #[test]
    fn duplicate_lanes_with_no_selection_is_inert() {
        let (mut state, cell) = three_lane_state();
        let before = state.heap.get(cell).clone();
        state.duplicate_lanes(&[false, false, false]);
        assert_eq!(state.vector_size, 3);
        assert_eq!(state.heap.get(cell), &before);
    }
}
