use std::collections::HashSet;

use itertools::Itertools;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    instruction_flow::FlowSide,
    ir::{Block, Instruction, Label, LocalId},
    liveness::LivenessAnalysisResult,
};

use super::{state::State, value::Value};

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
    BlockPostPhi,
    Branch,
    Return,
}

impl UnboundSplitBlockFlow<FlowData, BoundInterpreterFlow> for InterpreterFlowAdapter {
    fn flow_block_phi(
        &self,
        source_block_name: &Label,
        target_block: &Block,
    ) -> BoundInterpreterFlow {
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
        BoundInterpreterFlow::BlockPhi { phi_instructions }
    }

    fn flow_block_before_join(
        &self,
        liveness: &LivenessAnalysisResult,
        block: &crate::ir::Block,
    ) -> BoundInterpreterFlow {
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
        BoundInterpreterFlow::BlockBeforeJoin {
            live_variables: live_variables.clone(),
        }
    }

    fn flow_block_post_phi(&self, block: &crate::ir::Block) -> BoundInterpreterFlow {
        todo!()
    }

    fn flow_branch(
        &self,
        terminator: &crate::ir::Terminator,
        label: &crate::ir::Label,
    ) -> BoundInterpreterFlow {
        todo!()
    }

    fn flow_return(&self, terminator: &crate::ir::Terminator) -> BoundInterpreterFlow {
        todo!()
    }
}

impl BoundInterpreterFlow {
    fn flow_single_state(&self, mut state: State) -> FlowData {
        match self {
            Self::BlockPhi { phi_instructions } => {
                for &(instruction_local_id, source_local_id) in phi_instructions {
                    let value = state.local_env.get(source_local_id).clone();
                    state.local_env.set(instruction_local_id, value);
                }
                FlowData::States(vec![state])
            }
            Self::BlockBeforeJoin { live_variables } => {
                state
                    .local_env
                    .retain(|local_id| live_variables.contains(&local_id));
                FlowData::States(vec![state])
            }
            Self::BlockPostPhi => todo!(),
            Self::Branch => todo!(),
            Self::Return => todo!(),
        }
    }
}

impl BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow {
    fn flow(&self, v: FlowData) -> FlowData {
        todo!()
    }
}
