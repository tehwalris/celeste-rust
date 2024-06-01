use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    instruction_flow::FlowSide,
    ir::{Block, Instruction, Label, LocalId},
    liveness::LivenessAnalysisResult,
};

use super::{core_interpreter::CoreInterpreter, state::State, value::Value};

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
    Branch,
    Return,
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
        block: &crate::ir::Block,
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

    fn flow_block_post_phi(&self, target_block: &crate::ir::Block) -> Result<BoundInterpreterFlow> {
        let (_, non_phi_instructions) = target_block.split_block_phi_instructions();
        Ok(BoundInterpreterFlow::BlockPostPhi {
            non_phi_instructions: non_phi_instructions.to_vec(),
        })
    }

    fn flow_branch(
        &self,
        terminator: &crate::ir::Terminator,
        label: &crate::ir::Label,
    ) -> Result<BoundInterpreterFlow> {
        todo!()
    }

    fn flow_return(&self, terminator: &crate::ir::Terminator) -> Result<BoundInterpreterFlow> {
        todo!()
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
                        let mut interpreter = CoreInterpreter::new(state);
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
                Ok(FlowData::States(vec![state]))
            }
            Self::Branch => todo!(),
            Self::Return => todo!(),
        }
    }
}

impl BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow {
    fn flow(&self, v: FlowData) -> Result<FlowData> {
        todo!()
    }
}
