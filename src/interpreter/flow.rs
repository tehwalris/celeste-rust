use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    instruction_flow::FlowSide,
    ir::{Block, Instruction, Label, LocalId, Terminator},
    liveness::LivenessAnalysisResult,
};

use super::{
    core_interpreter::CoreInterpreter,
    state::State,
    value::{MaybeVector, Value},
};

struct InterpreterFlowAdapter {}

pub enum FlowData {
    States(Vec<State>),
    StatesAndReturns(Vec<(State, Value)>),
}

pub enum BoundInterpreterFlow {
    BlockPhi {
        phi_instructions: Vec<(LocalId, LocalId)>, // (instruction_local_id, source_local_id)
    },
    BlockBeforeJoin {
        live_variables: HashSet<LocalId>,
    },
    BlockPostPhi {
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

impl UnboundSplitBlockFlow<FlowData, BoundInterpreterFlow> for InterpreterFlowAdapter {
    fn flow_block_phi(
        &self,
        source_block_name: &Label,
        target_block: &Block,
    ) -> Result<BoundInterpreterFlow> {
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
    ) -> Result<BoundInterpreterFlow> {
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
            .unwrap();
        Ok(BoundInterpreterFlow::BlockBeforeJoin {
            live_variables: live_variables.clone(),
        })
    }

    fn flow_block_post_phi(&self, target_block: &Block) -> Result<BoundInterpreterFlow> {
        let (_, non_phi_instructions) = target_block.split_block_phi_instructions();
        Ok(BoundInterpreterFlow::BlockPostPhi {
            non_phi_instructions: non_phi_instructions.to_vec(),
        })
    }

    fn flow_branch(
        &self,
        terminator: &Terminator,
        flow_target: &Label,
    ) -> Result<BoundInterpreterFlow> {
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

    fn flow_return(&self, terminator: &Terminator) -> Result<BoundInterpreterFlow> {
        match terminator {
            Terminator::Return { value } => Ok(BoundInterpreterFlow::Return {
                return_local_id: *value,
            }),
            _ => panic!("Unexpected flow"),
        }
    }
}

impl BoundInterpreterFlow {
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
                state
                    .local_env
                    .retain(|local_id| live_variables.contains(&local_id));
                Ok(FlowData::States(vec![state]))
            }
            Self::BlockPostPhi {
                non_phi_instructions,
            } => {
                let mut states = vec![state];
                for (local_id, instruction) in non_phi_instructions {
                    let mut new_states = vec![];
                    for old_state in states {
                        let mut interpreter = CoreInterpreter::new(old_state);
                        match instruction {
                            Instruction::Call { .. } => {
                                new_states
                                    .extend(interpreter.interpret_call_instruction(instruction)?);
                            }
                            _ => {
                                interpreter.interpret_non_call_instruction(instruction)?;
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
                | Value::Bool(MaybeVector::Scalar(true))
                | Value::String(_)
                | Value::Pointer(_) => Ok(FlowData::States(if *condition_from_flow_edge {
                    vec![state]
                } else {
                    vec![]
                })),
                Value::NilPointer(_) => panic!("NilPointer probably shouldn't be here"),
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

impl BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow {
    fn flow(&self, v: FlowData) -> Result<FlowData> {
        todo!()
    }
}
