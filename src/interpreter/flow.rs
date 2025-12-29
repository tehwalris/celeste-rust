use std::collections::HashSet;

use anyhow::{anyhow, Result};
use itertools::Itertools;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    instruction_flow::FlowSide,
    ir::{Block, Instruction, Label, LocalId, Terminator},
    liveness::LivenessAnalysisResult,
};

use super::{
    core_interpreter::CoreInterpreter,
    fixed_env::FixedEnv,
    state::State,
    value::{MaybeVector, Value},
};

pub struct InterpreterFlowAdapter<'a> {
    pub fixed_env: &'a FixedEnv,
}

#[derive(Clone)]
pub enum FlowData {
    States(Vec<State>),
    StatesAndReturns(Vec<(State, Value)>),
}

impl FlowData {
    pub fn is_empty(&self) -> bool {
        match self {
            FlowData::States(states) => states.is_empty(),
            FlowData::StatesAndReturns(states_and_returns) => states_and_returns.is_empty(),
        }
    }

    /// Returns (state_count, expanded_count)
    pub fn counts(&self) -> (usize, usize) {
        match self {
            FlowData::States(states) => {
                let state_count = states.len();
                let expanded_count = states.iter().map(|s| s.vector_size).sum();
                (state_count, expanded_count)
            }
            FlowData::StatesAndReturns(states_and_returns) => {
                let state_count = states_and_returns.len();
                let expanded_count = states_and_returns.iter().map(|(s, _)| s.vector_size).sum();
                (state_count, expanded_count)
            }
        }
    }

    pub fn join_mut(&mut self, other: Self) {
        // TODO there's probably meant to be deduplication and stuff here
        match (self, other) {
            (FlowData::States(a), FlowData::States(b)) => {
                a.extend(b);
            }
            (FlowData::StatesAndReturns(a), FlowData::StatesAndReturns(b)) => {
                a.extend(b);
            }
            _ => panic!("Cannot join States and StatesAndReturns"),
        }
    }
}

pub enum BoundInterpreterFlow<'a> {
    BlockPhi {
        phi_instructions: Vec<(LocalId, LocalId)>, // (instruction_local_id, source_local_id)
    },
    BlockBeforeJoin {
        /// None means "keep all variables" (used when liveness analysis is not available)
        live_variables: Option<HashSet<LocalId>>,
    },
    BlockPostPhi {
        fixed_env: &'a FixedEnv,
        non_phi_instructions: Vec<(LocalId, Instruction)>,
    },
    BranchUnconditional,
    BranchConditional {
        condition_local_id: LocalId,
        condition_from_flow_edge: bool,
    },
    Return {
        return_local_id: Option<LocalId>,
    },
}

impl<'a> UnboundSplitBlockFlow<FlowData, BoundInterpreterFlow<'a>> for InterpreterFlowAdapter<'a> {
    fn flow_block_phi(
        &self,
        source_block_name: &Label,
        target_block: &Block,
    ) -> Result<BoundInterpreterFlow<'a>> {
        let (phi_instructions, _) = target_block.split_block_phi_instructions();
        let phi_instructions = phi_instructions
            .iter()
            .map(|(instruction_local_id, instruction)| match &instruction {
                &Instruction::Phi { branches } => {
                    let (_, source_local_id) = branches
                        .iter()
                        .filter(|(branch_label, _)| branch_label == source_block_name)
                        .exactly_one()
                        .unwrap();
                    (*instruction_local_id, *source_local_id)
                }
                _ => panic!("Expected Phi instruction"),
            })
            .collect();
        Ok(BoundInterpreterFlow::BlockPhi { phi_instructions })
    }

    fn flow_block_before_join(
        &self,
        liveness: &LivenessAnalysisResult,
        block: &Block,
    ) -> Result<BoundInterpreterFlow<'a>> {
        let (terminator_local_id, _) = block.terminator;
        let first_non_phi_local_id = block
            .instructions
            .iter()
            .find_map(|(local_id, instruction)| match instruction {
                Instruction::Phi { .. } => None,
                _ => Some(*local_id),
            })
            .unwrap_or(terminator_local_id);
        let live_variables = liveness
            .get_live_variables(FlowSide::Before, first_non_phi_local_id)
            .cloned();
        Ok(BoundInterpreterFlow::BlockBeforeJoin { live_variables })
    }

    fn flow_block_post_phi(&self, target_block: &Block) -> Result<BoundInterpreterFlow<'a>> {
        let (_, non_phi_instructions) = target_block.split_block_phi_instructions();
        Ok(BoundInterpreterFlow::BlockPostPhi {
            fixed_env: self.fixed_env,
            non_phi_instructions: non_phi_instructions.to_vec(),
        })
    }

    fn flow_branch(
        &self,
        terminator: &Terminator,
        flow_target: &Label,
    ) -> Result<BoundInterpreterFlow<'a>> {
        match terminator {
            Terminator::UnconditionalBranch { target } if target == flow_target => {
                Ok(BoundInterpreterFlow::BranchUnconditional)
            }
            Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } if flow_target == true_target || flow_target == false_target => {
                Ok(BoundInterpreterFlow::BranchConditional {
                    condition_local_id: *condition,
                    condition_from_flow_edge: flow_target == true_target,
                })
            }
            _ => panic!("Unexpected flow"),
        }
    }

    fn flow_return(&self, terminator: &Terminator) -> Result<BoundInterpreterFlow<'a>> {
        match terminator {
            Terminator::Return { value } => Ok(BoundInterpreterFlow::Return {
                return_local_id: *value,
            }),
            _ => panic!("Unexpected flow"),
        }
    }
}

impl<'a> BoundInterpreterFlow<'a> {
    fn flow_single_state(&self, mut state: State) -> Result<FlowData> {
        match self {
            Self::BlockPhi { phi_instructions } => {
                for &(instruction_local_id, source_local_id) in phi_instructions {
                    let value = state.local_env.get(source_local_id).clone();
                    state.local_env.set(instruction_local_id, value);
                }
                Ok(FlowData::States(vec![state]))
            }
            Self::BlockBeforeJoin { live_variables } => {
                // Only prune if we have liveness information; None means keep all
                if let Some(live_variables) = live_variables {
                    state
                        .local_env
                        .retain(|local_id| live_variables.contains(&local_id));
                }
                Ok(FlowData::States(vec![state]))
            }
            Self::BlockPostPhi {
                fixed_env,
                non_phi_instructions,
            } => {
                let mut states = vec![state];
                for (local_id, instruction) in non_phi_instructions {
                    let mut new_states = vec![];
                    for old_state in states {
                        let interpreter = CoreInterpreter::new(old_state, fixed_env);
                        match instruction {
                            Instruction::Call { .. } => {
                                new_states.extend(
                                    interpreter.interpret_call_instruction(*local_id, instruction)?,
                                );
                            }
                            _ => {
                                let mut interpreter = interpreter;
                                interpreter.interpret_non_call_instruction(*local_id, instruction)?;
                                new_states.push(interpreter.into_state());
                            }
                        }
                    }

                    states = new_states;
                }
                Ok(FlowData::States(states))
            }
            Self::BranchUnconditional => Ok(FlowData::States(vec![state])),
            Self::BranchConditional {
                condition_local_id,
                condition_from_flow_edge,
            } => match &state.local_env.get(*condition_local_id) {
                Value::Bool(MaybeVector::Scalar(false)) | Value::Nil(_) => {
                    Ok(FlowData::States(if *condition_from_flow_edge {
                        vec![]
                    } else {
                        vec![state]
                    }))
                }
                Value::UnknownBool => Ok(FlowData::States(vec![state])),
                Value::Number(_)
                | Value::NumberInterval(_)
                | Value::Bool(MaybeVector::Scalar(true))
                | Value::String(_)
                | Value::Pointer(_) => Ok(FlowData::States(if *condition_from_flow_edge {
                    vec![state]
                } else {
                    vec![]
                })),
                Value::NilPointer(_) => Err(anyhow!("Nil pointer in condition")),
                Value::Bool(MaybeVector::Vector(bool_vector)) => {
                    let mask_true_count = bool_vector
                        .iter()
                        .filter(|v| **v == *condition_from_flow_edge)
                        .count();

                    if mask_true_count == 0 {
                        Ok(FlowData::States(vec![]))
                    } else if mask_true_count == bool_vector.len() {
                        Ok(FlowData::States(vec![state]))
                    } else {
                        let mask: Vec<bool> = bool_vector
                            .iter()
                            .map(|v| *v == *condition_from_flow_edge)
                            .collect();

                        state.map_values_in_place(|v| v.filter_vectors(&mask));
                        state.vector_size = mask_true_count;
                        Ok(FlowData::States(vec![state]))
                    }
                }
            },
            Self::Return { return_local_id } => match *return_local_id {
                Some(return_local_id) => {
                    state.local_env.retain(|id| id == return_local_id);
                    let value = state.local_env.get(return_local_id).clone();
                    Ok(FlowData::StatesAndReturns(vec![(state, value)]))
                }
                None => {
                    state.local_env.clear();
                    Ok(FlowData::States(vec![state]))
                }
            },
        }
    }
}

impl<'a> BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow<'a> {
    fn flow(&self, v: FlowData) -> Result<FlowData> {
        let out_parts = match &v {
            FlowData::States(states) => states
                .iter()
                .map(|state| self.flow_single_state(state.clone()))
                .collect::<Result<Vec<FlowData>>>()?,
            FlowData::StatesAndReturns(_) => {
                return Err(anyhow!("Return value in unexpected part of CFG"))
            }
        };

        let result = match out_parts.first() {
            Some(FlowData::States(_)) => FlowData::States(
                out_parts
                    .iter()
                    .map(|part| match part {
                        FlowData::States(states) => states.clone(),
                        FlowData::StatesAndReturns(_) => {
                            panic!("Mix of States and StatesAndReturns")
                        }
                    })
                    .flatten()
                    .collect(),
            ),
            Some(FlowData::StatesAndReturns(_)) => FlowData::StatesAndReturns(
                out_parts
                    .iter()
                    .map(|part| match part {
                        FlowData::States(_) => panic!("Mix of States and StatesAndReturns"),
                        FlowData::StatesAndReturns(states_and_returns) => {
                            states_and_returns.clone()
                        }
                    })
                    .flatten()
                    .collect(),
            ),
            None => FlowData::States(vec![]),
        };

        Ok(result)
    }
}
