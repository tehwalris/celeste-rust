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
                    // LAZY MASKING: compose masks instead of filtering immediately
                    let condition_target = *condition_from_flow_edge;

                    // Count how many ACTIVE lanes will go this direction
                    let old_vector_size = state.vector_size;
                    let old_original_size = state.original_size;
                    let active_matching = match &state.mask {
                        None => {
                            bool_vector.iter().filter(|&&v| v == condition_target).count()
                        }
                        Some(mask) => {
                            mask.iter()
                                .zip(bool_vector.iter())
                                .filter(|(&m, &v)| m && v == condition_target)
                                .count()
                        }
                    };

                    if active_matching == 0 {
                        Ok(FlowData::States(vec![]))
                    } else if active_matching == old_vector_size {
                        // ALL active lanes go this direction - no mask change needed
                        Ok(FlowData::States(vec![state]))
                    } else {
                        // Mixed: apply lazy mask
                        let condition_mask: Vec<bool> = bool_vector
                            .iter()
                            .map(|v| *v == condition_target)
                            .collect();

                        let mut new_state = state;
                        new_state.apply_lazy_mask(&condition_mask);
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

    /// Test lazy masking with a vectorized bool condition
    #[test]
    fn test_branch_conditional_lazy_mask_basic() {
        // Create a state with vector_size = 4
        // local_env[0] = Bool([true, false, true, false])  <- condition
        // local_env[1] = Number([1, 2, 3, 4])              <- data
        let mut state = State::new();
        state.original_size = 4;
        state.vector_size = 4;
        state.mask = None;

        let condition_local_id = LocalId::from(0);
        let data_local_id = LocalId::from(1);

        state.local_env.set(condition_local_id, Value::Bool(MaybeVector::Vector(vec![
            true, false, true, false
        ])));
        state.local_env.set(data_local_id, Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
            Pico8Num::from_i16(4),
        ])));

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
        assert_eq!(true_state.vector_size, 2, "True branch should have 2 active lanes");
        assert_eq!(true_state.original_size, 4, "Original size should still be 4 (lazy masking)");
        assert!(true_state.mask.is_some(), "Should have a mask set");
        assert_eq!(
            true_state.mask.as_ref().unwrap(),
            &vec![true, false, true, false],
            "Mask should match condition"
        );

        // Execute false branch
        let false_result = false_branch.flow_single_state(state_for_false).unwrap();
        let false_states = match false_result {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };

        assert_eq!(false_states.len(), 1, "False branch should return 1 state");
        let false_state = &false_states[0];
        assert_eq!(false_state.vector_size, 2, "False branch should have 2 active lanes");
        assert_eq!(false_state.original_size, 4, "Original size should still be 4 (lazy masking)");
        assert!(false_state.mask.is_some(), "Should have a mask set");
        assert_eq!(
            false_state.mask.as_ref().unwrap(),
            &vec![false, true, false, true],
            "Mask should match inverted condition"
        );

        // Total expanded count should equal original
        let total_expanded = true_state.vector_size + false_state.vector_size;
        assert_eq!(total_expanded, 4, "Total lanes should equal original (2 + 2 = 4)");
    }

    /// Test that materialize produces the correct filtered vectors
    #[test]
    fn test_materialize_after_branch() {
        let mut state = State::new();
        state.original_size = 4;
        state.vector_size = 4;
        state.mask = None;

        let condition_local_id = LocalId::from(0);
        let data_local_id = LocalId::from(1);

        state.local_env.set(condition_local_id, Value::Bool(MaybeVector::Vector(vec![
            true, false, true, false
        ])));
        state.local_env.set(data_local_id, Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
            Pico8Num::from_i16(4),
        ])));

        // Take true branch
        let true_branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id,
            condition_from_flow_edge: true,
        };

        let result = true_branch.flow_single_state(state).unwrap();
        let mut true_state = match result {
            FlowData::States(mut s) => s.pop().unwrap(),
            _ => panic!("Expected States"),
        };

        // Before materialize: vectors still have original size
        assert_eq!(true_state.original_size, 4);
        assert_eq!(true_state.vector_size, 2);
        assert!(true_state.mask.is_some());

        // Check data vector still has original length
        match true_state.local_env.get(data_local_id) {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(nums.len(), 4, "Before materialize: vector should have original length");
            }
            _ => panic!("Expected Number vector"),
        }

        // Materialize
        true_state.materialize();

        // After materialize: vectors should be filtered
        assert_eq!(true_state.original_size, 2, "After materialize: original_size = 2");
        assert_eq!(true_state.vector_size, 2, "After materialize: vector_size = 2");
        assert!(true_state.mask.is_none(), "After materialize: mask should be None");

        // Check data vector is now filtered
        match true_state.local_env.get(data_local_id) {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(nums.len(), 2, "After materialize: vector should have filtered length");
                assert_eq!(nums[0], Pico8Num::from_i16(1), "First active lane");
                assert_eq!(nums[1], Pico8Num::from_i16(3), "Second active lane");
            }
            _ => panic!("Expected Number vector"),
        }
    }

    /// Test sequential branches compose masks correctly
    #[test]
    fn test_sequential_branches_compose_mask() {
        // Start with 4 lanes, branch twice
        let mut state = State::new();
        state.original_size = 4;
        state.vector_size = 4;
        state.mask = None;

        let cond1_id = LocalId::from(0);
        let cond2_id = LocalId::from(1);

        // First condition: [true, true, false, false]
        state.local_env.set(cond1_id, Value::Bool(MaybeVector::Vector(vec![
            true, true, false, false
        ])));
        // Second condition: [true, false, true, false]
        state.local_env.set(cond2_id, Value::Bool(MaybeVector::Vector(vec![
            true, false, true, false
        ])));

        // First branch: take true path (lanes 0, 1 active)
        let branch1 = BoundInterpreterFlow::BranchConditional {
            condition_local_id: cond1_id,
            condition_from_flow_edge: true,
        };

        let result1 = branch1.flow_single_state(state).unwrap();
        let state_after_branch1 = match result1 {
            FlowData::States(mut s) => s.pop().unwrap(),
            _ => panic!("Expected States"),
        };

        assert_eq!(state_after_branch1.vector_size, 2, "After branch1: 2 active");
        assert_eq!(state_after_branch1.mask.as_ref().unwrap(), &vec![true, true, false, false]);

        // Second branch: take true path (lane 0 active from first, lane 2 was inactive)
        let branch2 = BoundInterpreterFlow::BranchConditional {
            condition_local_id: cond2_id,
            condition_from_flow_edge: true,
        };

        let result2 = branch2.flow_single_state(state_after_branch1).unwrap();
        let state_after_branch2 = match result2 {
            FlowData::States(mut s) => s.pop().unwrap(),
            _ => panic!("Expected States"),
        };

        // After two branches: only lane 0 should be active
        // mask1 = [true, true, false, false]
        // cond2 = [true, false, true, false]
        // composed = mask1 && cond2 = [true, false, false, false]
        assert_eq!(state_after_branch2.vector_size, 1, "After branch2: 1 active");
        assert_eq!(state_after_branch2.mask.as_ref().unwrap(), &vec![true, false, false, false]);

        // Materialize and check
        let mut final_state = state_after_branch2;
        final_state.materialize();
        assert_eq!(final_state.vector_size, 1);
        assert_eq!(final_state.original_size, 1);
    }

    /// Test that all lanes going true direction returns unchanged state
    #[test]
    fn test_branch_all_true() {
        let mut state = State::new();
        state.original_size = 3;
        state.vector_size = 3;
        state.mask = None;

        let cond_id = LocalId::from(0);
        // All true
        state.local_env.set(cond_id, Value::Bool(MaybeVector::Vector(vec![true, true, true])));

        let branch = BoundInterpreterFlow::BranchConditional {
            condition_local_id: cond_id,
            condition_from_flow_edge: true,
        };

        let result = branch.flow_single_state(state).unwrap();
        let result_state = match result {
            FlowData::States(mut s) => s.pop().unwrap(),
            _ => panic!("Expected States"),
        };

        // Should return unchanged (no mask applied)
        assert_eq!(result_state.vector_size, 3);
        assert_eq!(result_state.original_size, 3);
        assert!(result_state.mask.is_none(), "All-true branch should not set mask");
    }

    /// Test that no lanes going our direction returns empty
    #[test]
    fn test_branch_all_opposite() {
        let mut state = State::new();
        state.original_size = 3;
        state.vector_size = 3;
        state.mask = None;

        let cond_id = LocalId::from(0);
        // All true, but we're taking false branch
        state.local_env.set(cond_id, Value::Bool(MaybeVector::Vector(vec![true, true, true])));

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
