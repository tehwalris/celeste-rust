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

/// Threshold below which we stop using parallel iteration.
/// This reduces crossbeam overhead for deep recursive calls.
const PARALLEL_BUDGET_THRESHOLD: f64 = 0.01;

/// Minimum number of states required to use parallel iteration.
/// Below this, sequential is faster due to parallelization overhead.
const MIN_STATES_FOR_PARALLEL: usize = 128;

pub struct InterpreterFlowAdapter<'a> {
    pub fixed_env: &'a FixedEnv,
    /// Budget for parallel execution. Starts at 1.0 at the top level and is
    /// divided as we recurse. When below PARALLEL_BUDGET_THRESHOLD, we use
    /// sequential iteration instead of parallel.
    pub parallel_budget: f64,
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
        parallel_budget: f64,
    },
    BlockBeforeJoin {
        /// None means "keep all variables" (used when liveness analysis is not available)
        live_variables: Option<FxHashSet<LocalId>>,
        parallel_budget: f64,
    },
    BlockPostPhi {
        fixed_env: &'a FixedEnv,
        non_phi_instructions: Vec<(LocalId, Instruction)>,
        parallel_budget: f64,
    },
    BranchUnconditional {
        parallel_budget: f64,
    },
    BranchConditional {
        condition_local_id: LocalId,
        condition_from_flow_edge: bool,
        parallel_budget: f64,
    },
    Return {
        return_local_id: Option<LocalId>,
        parallel_budget: f64,
    },
}

impl<'a> BoundInterpreterFlow<'a> {
    fn parallel_budget(&self) -> f64 {
        match self {
            Self::BlockPhi { parallel_budget, .. } => *parallel_budget,
            Self::BlockBeforeJoin { parallel_budget, .. } => *parallel_budget,
            Self::BlockPostPhi { parallel_budget, .. } => *parallel_budget,
            Self::BranchUnconditional { parallel_budget } => *parallel_budget,
            Self::BranchConditional { parallel_budget, .. } => *parallel_budget,
            Self::Return { parallel_budget, .. } => *parallel_budget,
        }
    }
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
            parallel_budget: self.parallel_budget,
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
            parallel_budget: self.parallel_budget,
        })
    }

    fn flow_block_post_phi(&self, target_block: &Block) -> Result<BoundInterpreterFlow<'a>> {
        let (_, non_phi_instructions) = target_block.split_block_phi_instructions();
        Ok(BoundInterpreterFlow::BlockPostPhi {
            fixed_env: self.fixed_env,
            non_phi_instructions: non_phi_instructions.to_vec(),
            parallel_budget: self.parallel_budget,
        })
    }

    fn flow_branch(
        &self,
        terminator: &Terminator,
        flow_target: &Label,
    ) -> Result<BoundInterpreterFlow<'a>> {
        match terminator {
            Terminator::UnconditionalBranch { target } if target == flow_target => {
                Ok(BoundInterpreterFlow::BranchUnconditional {
                    parallel_budget: self.parallel_budget,
                })
            }
            Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } if flow_target == true_target || flow_target == false_target => {
                Ok(BoundInterpreterFlow::BranchConditional {
                    condition_local_id: *condition,
                    condition_from_flow_edge: flow_target == true_target,
                    parallel_budget: self.parallel_budget,
                })
            }
            _ => panic!("Unexpected flow"),
        }
    }

    fn flow_return(&self, terminator: &Terminator) -> Result<BoundInterpreterFlow<'a>> {
        match terminator {
            Terminator::Return { value } => Ok(BoundInterpreterFlow::Return {
                return_local_id: *value,
                parallel_budget: self.parallel_budget,
            }),
            _ => panic!("Unexpected flow"),
        }
    }
}

impl<'a> BoundInterpreterFlow<'a> {
    fn flow_single_state(&self, mut state: State) -> Result<FlowData> {
        match self {
            Self::BlockPhi { phi_instructions, .. } => {
                for &(instruction_local_id, source_local_id) in phi_instructions {
                    let value = state.local_env.get(source_local_id).clone();
                    state.local_env.set(instruction_local_id, value);
                }
                Ok(FlowData::States(vec![state]))
            }
            Self::BlockBeforeJoin { live_variables, .. } => {
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
                parallel_budget,
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
                        let interpreter = CoreInterpreter::new_with_parallel_budget(
                            old_state,
                            fixed_env,
                            *parallel_budget,
                        );
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
            Self::BranchUnconditional { .. } => Ok(FlowData::States(vec![state])),
            Self::BranchConditional {
                condition_local_id,
                condition_from_flow_edge,
                ..
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
            Self::Return { return_local_id, .. } => match *return_local_id {
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
