use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    instruction_flow::FlowSide,
    ir::{Block, Instruction, Label, LocalId, Terminator},
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
    BranchUnconditional,
    BranchConditional {
        condition_local_id: LocalId,
        condition_from_flow_edge: bool,
    },
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

    /*
      and flow_branch (terminator : Ir.terminator) (flow_target : Ir.label) :
      LazyStateSet.t -> LazyStateSet.t =
    match terminator with
    | Ir.Br terminator_target when terminator_target = flow_target ->
        fun states -> states
    | Ir.Cbr (local_id, true_label, false_label)
      when flow_target = true_label || flow_target = false_label ->
        fun states ->
          LazyStateSet.filter_map
            (fun state ->
              match Ir.LocalIdMap.find local_id state.local_env with
              | Scalar (SBool false) | Scalar (SNil _) ->
                  if flow_target = false_label then Some state else None
              | Scalar SUnknownBool -> Some state
              | Scalar _ -> if flow_target = true_label then Some state else None
              | Vector (VBool vec) ->
                  Perf.count_and_time Perf.global_counters.cbr_filter @@ fun () ->
                  assert (Array.length vec = state.vector_size);
                  let filter_value = flow_target = true_label in
                  let mask = Array.map (fun v -> v = filter_value) vec in
                  let mask_true_count =
                    Array.fold_left
                      (fun acc v -> if v then acc + 1 else acc)
                      0 mask
                  in
                  if mask_true_count = 0 then None
                  else
                    let filtered_state =
                      state_map_values
                        (function
                          | Scalar s -> Scalar s
                          | Vector vec -> Option.get @@ filter_vector mask vec)
                        state
                    in
                    Some { filtered_state with vector_size = mask_true_count }
              | Vector _ -> if flow_target = true_label then Some state else None)
            states
    | _ -> failwith "Unexpected flow"
       */

    fn flow_branch(
        &self,
        terminator: &crate::ir::Terminator,
        flow_target: &crate::ir::Label,
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
            } => {
                let v = state.local_env.get(*condition_local_id);
                todo!()
            }
            Self::Return => todo!(),
        }
    }
}

impl BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow {
    fn flow(&self, v: FlowData) -> Result<FlowData> {
        todo!()
    }
}
