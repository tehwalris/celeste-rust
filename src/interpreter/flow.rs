use anyhow::{anyhow, Result};
use itertools::Itertools;
use rustc_hash::FxHashSet;

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
        live_variables: Option<FxHashSet<LocalId>>,
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
        Ok(BoundInterpreterFlow::BlockPhi {
            phi_instructions,
        })
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
        Ok(BoundInterpreterFlow::BlockBeforeJoin {
            live_variables,
        })
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
                // Use two buffers and swap between them to avoid repeated allocations
                let mut states_a = vec![state];
                let mut states_b = Vec::new();
                let mut current_is_a = true;

                for (local_id, instruction) in non_phi_instructions {
                    let (src, dst) = if current_is_a {
                        (&mut states_a, &mut states_b)
                    } else {
                        (&mut states_b, &mut states_a)
                    };
                    dst.clear();

                    for old_state in src.drain(..) {
                        let interpreter = CoreInterpreter::new(old_state, fixed_env);
                        match instruction {
                            Instruction::Call { .. } => {
                                dst.extend(
                                    interpreter.interpret_call_instruction(*local_id, instruction)?,
                                );
                            }
                            _ => {
                                let mut interpreter = interpreter;
                                interpreter.interpret_non_call_instruction(*local_id, instruction)?;
                                dst.push(interpreter.into_state());
                            }
                        }
                    }

                    current_is_a = !current_is_a;
                }

                let final_states = if current_is_a { states_a } else { states_b };
                Ok(FlowData::States(final_states))
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
                    let condition_target = *condition_from_flow_edge;

                    // Build a mask for lanes matching this condition
                    let condition_mask: Vec<bool> = bool_vector
                        .iter()
                        .map(|v| *v == condition_target)
                        .collect();

                    let matching_count = condition_mask.iter().filter(|&&b| b).count();

                    if matching_count == 0 {
                        Ok(FlowData::States(vec![]))
                    } else if matching_count == state.vector_size {
                        // ALL lanes go this direction - no filtering needed
                        Ok(FlowData::States(vec![state]))
                    } else {
                        // Mixed: filter the state immediately
                        let new_state = state.filter_by_mask(&condition_mask);
                        Ok(FlowData::States(vec![new_state]))
                    }
                }
                Value::Bool(MaybeVector::LazyVector { data, mask, len }) => {
                    let condition_target = *condition_from_flow_edge;

                    // Build a mask for lanes matching this condition
                    // We need to iterate over the masked data (active elements only)
                    let condition_mask: Vec<bool> = data
                        .iter()
                        .zip(mask.iter())
                        .filter_map(|(v, &m)| if m { Some(*v == condition_target) } else { None })
                        .collect();

                    debug_assert_eq!(condition_mask.len(), *len);

                    let matching_count = condition_mask.iter().filter(|&&b| b).count();

                    if matching_count == 0 {
                        Ok(FlowData::States(vec![]))
                    } else if matching_count == state.vector_size {
                        // ALL lanes go this direction - no filtering needed
                        Ok(FlowData::States(vec![state]))
                    } else {
                        // Mixed: filter the state immediately
                        let new_state = state.filter_by_mask(&condition_mask);
                        Ok(FlowData::States(vec![new_state]))
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
        let states = match v {
            FlowData::States(states) => states,
            FlowData::StatesAndReturns(_) => {
                return Err(anyhow!("Return value in unexpected part of CFG"))
            }
        };

        // Pre-compute total capacity to avoid reallocations
        // Most flow operations return a single state, so estimate 1 per input
        let mut result_states: Vec<State> = Vec::with_capacity(states.len());
        let mut result_returns: Vec<(State, Value)> = Vec::new();
        let mut has_returns = false;

        for state in states {
            match self.flow_single_state(state)? {
                FlowData::States(new_states) => {
                    result_states.extend(new_states);
                }
                FlowData::StatesAndReturns(new_returns) => {
                    has_returns = true;
                    result_returns.extend(new_returns);
                }
            }
        }

        if has_returns {
            if !result_states.is_empty() {
                panic!("Mix of States and StatesAndReturns");
            }
            Ok(FlowData::StatesAndReturns(result_returns))
        } else {
            Ok(FlowData::States(result_states))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;
    use std::sync::Arc;

    /// Test immediate filtering with a vectorized bool condition
    #[test]
    fn test_branch_conditional_immediate_filter() {
        // Create a state with vector_size = 4
        // local_env[0] = Bool([true, false, true, false])  <- condition
        // local_env[1] = Number([1, 2, 3, 4])              <- data
        let mut state = State::new();
        state.vector_size = 4;

        let condition_local_id = LocalId::from(0);
        let data_local_id = LocalId::from(1);

        state.local_env.set(condition_local_id, Value::Bool(MaybeVector::Vector(Arc::new(vec![
            true, false, true, false
        ]))));
        state.local_env.set(data_local_id, Value::Number(MaybeVector::Vector(Arc::new(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
            Pico8Num::from_i16(4),
        ]))));

        // Create true branch flow
        let true_branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id,
            condition_from_flow_edge: true,
        };

        // Create false branch flow
        let false_branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id,
            condition_from_flow_edge: false,
        };

        // Clone state for false branch
        let state_for_false = state.clone();

        // Execute true branch
        let true_result = true_branch.flow_single_state(state).unwrap();
        let true_states = match true_result {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };

        assert_eq!(true_states.len(), 1, "True branch should return 1 state");
        let true_state = &true_states[0];
        assert_eq!(true_state.vector_size, 2, "True branch should have 2 lanes");

        // Vectors should be immediately filtered
        // Note: May be LazyVector due to filtering optimization
        let true_data_value = true_state.local_env.get(data_local_id).materialize_if_lazy();
        match true_data_value {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(nums.len(), 2, "Vector should be filtered to 2 elements");
                assert_eq!(nums[0], Pico8Num::from_i16(1), "First element");
                assert_eq!(nums[1], Pico8Num::from_i16(3), "Second element");
            }
            _ => panic!("Expected Number vector"),
        }

        // Execute false branch
        let false_result = false_branch.flow_single_state(state_for_false).unwrap();
        let false_states = match false_result {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };

        assert_eq!(false_states.len(), 1, "False branch should return 1 state");
        let false_state = &false_states[0];
        assert_eq!(false_state.vector_size, 2, "False branch should have 2 lanes");

        // Vectors should be immediately filtered
        // Note: May be LazyVector due to filtering optimization
        let false_data_value = false_state.local_env.get(data_local_id).materialize_if_lazy();
        match false_data_value {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(nums.len(), 2, "Vector should be filtered to 2 elements");
                assert_eq!(nums[0], Pico8Num::from_i16(2), "First element");
                assert_eq!(nums[1], Pico8Num::from_i16(4), "Second element");
            }
            _ => panic!("Expected Number vector"),
        }

        // Total count should equal original
        let total = true_state.vector_size + false_state.vector_size;
        assert_eq!(total, 4, "Total lanes should equal original (2 + 2 = 4)");
    }

    /// Test that all lanes going true direction returns unchanged state
    #[test]
    fn test_branch_all_true() {
        let mut state = State::new();
        state.vector_size = 3;

        let cond_id = LocalId::from(0);
        // All true
        state.local_env.set(cond_id, Value::Bool(MaybeVector::Vector(Arc::new(vec![true, true, true]))));

        let branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id: cond_id,
            condition_from_flow_edge: true,
        };

        let result = branch.flow_single_state(state).unwrap();
        let result_state = match result {
            FlowData::States(mut s) => s.pop().unwrap(),
            _ => panic!("Expected States"),
        };

        // Should return unchanged (no filtering needed)
        assert_eq!(result_state.vector_size, 3);
    }

    /// Test that no lanes going our direction returns empty
    #[test]
    fn test_branch_all_opposite() {
        let mut state = State::new();
        state.vector_size = 3;

        let cond_id = LocalId::from(0);
        // All true, but we're taking false branch
        state.local_env.set(cond_id, Value::Bool(MaybeVector::Vector(Arc::new(vec![true, true, true]))));

        let branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id: cond_id,
            condition_from_flow_edge: false, // Taking false branch
        };

        let result = branch.flow_single_state(state).unwrap();
        let states = match result {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };

        assert!(states.is_empty(), "No lanes go to false branch, should be empty");
    }
}
