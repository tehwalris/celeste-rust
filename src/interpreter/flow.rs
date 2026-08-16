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

/// The data that flows along a CFG edge: a set of states (each of which is
/// itself `vector_size` lanes in SoA layout).
///
/// Note there is no join/merge operation here. Two edges arriving at the same
/// block are *not* combined; `glue::interpret_prepared_cfg_inner` queues them
/// as separate work items and the block runs once per fragment. States are only
/// ever merged back together at `hint_normalize` blocks, via `vectorize_states`
/// + `union_diff_states`. This is the main source of intra-frame work
/// multiplication - see plans/rewrite-plan.md.
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

                let timing = crate::instr_time::enabled();
                for (local_id, instruction) in non_phi_instructions {
                    let started = if timing { Some(std::time::Instant::now()) } else { None };
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
                            // Lane-granular deopt: a falsified premise captures
                            // the violating lanes' origins and continues (or
                            // drops) the rest, instead of aborting the frame.
                            // See `deopt_collect`.
                            Instruction::AssertTrue { value }
                                if crate::interpreter::deopt_collect::is_collecting() =>
                            {
                                if let Some(state) = interpreter.collect_assert_true(*value)? {
                                    dst.push(state);
                                }
                            }
                            _ => {
                                let mut interpreter = interpreter;
                                // Two instructions can yield a second state
                                // here, and both spill through this one
                                // channel. `dst` already carries many states
                                // per instruction (see the Call arm), so
                                // neither needs new plumbing.
                                //
                                //  - a MIXED interval comparison PARTITIONS:
                                //    definite lanes stay, straddling lanes
                                //    spill. Lanes are moved, so the total is
                                //    unchanged.
                                //  - a `select` on a whole-value UnknownBool
                                //    SPLITS: the spill is the false copy and
                                //    this state becomes the true one. Both
                                //    keep every lane, so lanes DO double -
                                //    that is the price of not dropping the
                                //    whole frame onto the plain program.
                                let spill = interpreter
                                    .interpret_non_call_instruction(*local_id, instruction)?;
                                dst.push(interpreter.into_state());
                                dst.extend(spill);
                            }
                        }
                    }

                    if let Some(started) = started {
                        crate::instr_time::record(usize::from(*local_id), started.elapsed());
                    }
                    if crate::instr_time::cardinality_enabled() {
                        // After the timed window on purpose - counting distinct
                        // values costs more than many instructions do.
                        for result_state in dst.iter() {
                            if let Some(value) = result_state.local_env.try_get(*local_id) {
                                crate::instr_time::record_cardinality(
                                    usize::from(*local_id),
                                    value,
                                    result_state.vector_size,
                                );
                            }
                        }
                    }
                    current_is_a = !current_is_a;
                }

                let final_states = if current_is_a { states_a } else { states_b };
                Ok(FlowData::States(final_states))
            }
            Self::BranchUnconditional => Ok(FlowData::States(vec![state])),
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

/// Run `f` over every state, in input order. States are independent of
/// each other through a flow step - no instruction reads another state,
/// and a call's descendants replace their parent in place - so this is the
/// shared per-state driver for plain flow steps and the fused branch
/// split.
///
/// A state-parallel version of this (rayon pool, opt-in per program) was
/// measured at -22% on the unrewritten search path but +4-6% and +8-18%
/// peak RSS on the rewritten one - the fused block's per-state streaming
/// working sets evict each other. Since the rewritten path is the one the
/// search runs, the parallel arm was reverted by review; it lives in git
/// history ("state-parallel flow steps") if the tradeoff ever flips.
fn map_states_maybe_parallel<R: Send>(
    states: Vec<State>,
    f: impl Fn(State) -> Result<R> + Sync,
) -> Result<Vec<R>> {
    let mut flat = Vec::with_capacity(states.len());
    for state in states {
        flat.push(f(state)?);
    }
    Ok(flat)
}

/// A conditional branch bound for both edges at once. Replaces the two
/// per-edge `BranchConditional` flows: the condition is inspected once per
/// state, and a lane-mixed condition splits the state in a single pass
/// (`State::split_by_condition`) instead of filtering a clone per edge.
pub struct BoundBranchSplit {
    condition_local_id: LocalId,
}

impl<'a> InterpreterFlowAdapter<'a> {
    pub fn flow_branch_split(&self, terminator: &Terminator) -> Result<BoundBranchSplit> {
        match terminator {
            Terminator::ConditionalBranch { condition, .. } => Ok(BoundBranchSplit {
                condition_local_id: *condition,
            }),
            _ => panic!("Unexpected flow"),
        }
    }
}

impl BoundBranchSplit {
    /// Both edges' states, in input order per edge.
    pub fn flow_split(&self, v: FlowData) -> Result<(FlowData, FlowData)> {
        let states = match v {
            FlowData::States(states) => states,
            FlowData::StatesAndReturns(_) => {
                return Err(anyhow!("Return value in unexpected part of CFG"))
            }
        };
        let pairs = map_states_maybe_parallel(states, |state| self.split_single_state(state))?;
        let mut true_states = Vec::new();
        let mut false_states = Vec::new();
        for (t, f) in pairs {
            true_states.extend(t);
            false_states.extend(f);
        }
        Ok((FlowData::States(true_states), FlowData::States(false_states)))
    }

    fn split_single_state(&self, state: State) -> Result<(Option<State>, Option<State>)> {
        // Clone the condition value (an Arc bump for vectors) so the state
        // can be moved below.
        let condition_value = state.local_env.get(self.condition_local_id).clone();
        match condition_value {
            // Transient: comparisons resolve MaybeBool into a definite Bool
            // at the instruction that produced it, so a branch can never see
            // one. Loud rather than accommodated - see value.rs.
            Value::MaybeBool(_) => panic!("{}", crate::interpreter::value::MAYBE_BOOL_ESCAPED),
            Value::Bool(MaybeVector::Scalar(false)) | Value::Nil(_) => Ok((None, Some(state))),
            Value::UnknownBool => {
                // Both edges get the whole state - no filtering, so this
                // costs a duplication and everything downstream of it
                // rather than filter time. Counted separately for that
                // reason. (Once per branch now; the per-edge formulation
                // counted each duplication twice.)
                //
                // EDGE REFINEMENT (plans/spd-rung.md round 2): each
                // successor learns the condition's value. An UnknownBool
                // is a bool by construction (comparisons, btn, not), so
                // on the true edge it IS true and on the false edge it
                // IS false - overwriting the local is exact, not an
                // approximation. Without this, Lua's `a and b or c`
                // VALUE idiom returns the still-unknown condition out of
                // the false arm (observed: `appr` returning UnknownBool
                // into spd.x on the plain program under the spd rung).
                // Heap copies or aliases of the value stay unknown; only
                // the branched-on local is refined.
                crate::op_census::record_unknown_branch_dup(state.vector_size);
                let mut true_state = state.clone();
                true_state.local_env.set(
                    self.condition_local_id,
                    Value::Bool(MaybeVector::Scalar(true)),
                );
                let mut false_state = state;
                false_state.local_env.set(
                    self.condition_local_id,
                    Value::Bool(MaybeVector::Scalar(false)),
                );
                Ok((Some(true_state), Some(false_state)))
            }
            Value::Number(_)
            | Value::NumberInterval(_)
            | Value::Bool(MaybeVector::Scalar(true))
            | Value::String(_)
            | Value::Pointer(_) => Ok((Some(state), None)),
            Value::NilPointer(_) => Err(anyhow!("Nil pointer in condition")),
            Value::Bool(MaybeVector::Vector(bool_vector)) => {
                Ok(state.split_by_condition(&bool_vector, true))
            }
        }
    }
}

/// Concatenate per-state flow outputs in input order - shared tail of the
/// sequential and parallel paths.
fn finish_flow(per_state: Vec<FlowData>) -> Result<FlowData> {
    let mut result_states: Vec<State> = Vec::with_capacity(per_state.len());
    let mut result_returns: Vec<(State, Value)> = Vec::new();
    let mut has_returns = false;
    for data in per_state {
        match data {
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

impl<'a> BoundSplitBlockFlow<FlowData> for BoundInterpreterFlow<'a> {
    fn flow(&self, v: FlowData) -> Result<FlowData> {
        let states = match v {
            FlowData::States(states) => states,
            FlowData::StatesAndReturns(_) => {
                return Err(anyhow!("Return value in unexpected part of CFG"))
            }
        };
        let per_state = map_states_maybe_parallel(states, |state| self.flow_single_state(state))?;
        finish_flow(per_state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    /// A lane-mixed vector condition splits the state in one pass: both
    /// edges' states come back from a single flow_split, filtered exactly
    /// as the two per-edge filters used to produce.
    #[test]
    fn test_branch_split_immediate_filter() {
        // vector_size = 4; condition [t, f, t, f]; data [1, 2, 3, 4].
        let mut state = State::new();
        state.vector_size = 4;

        let condition_local_id = LocalId::from(0);
        let data_local_id = LocalId::from(1);

        state.local_env.set(
            condition_local_id,
            Value::Bool(MaybeVector::vector(vec![true, false, true, false])),
        );
        state.local_env.set(
            data_local_id,
            Value::Number(MaybeVector::vector(vec![
                Pico8Num::from_i16(1),
                Pico8Num::from_i16(2),
                Pico8Num::from_i16(3),
                Pico8Num::from_i16(4),
            ])),
        );

        let split = BoundBranchSplit {
            condition_local_id,
        };
        let (true_data, false_data) = split.flow_split(FlowData::States(vec![state])).unwrap();

        let true_states = match true_data {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };
        assert_eq!(true_states.len(), 1, "True edge should get 1 state");
        let true_state = &true_states[0];
        assert_eq!(true_state.vector_size, 2, "True edge should have 2 lanes");
        match true_state.local_env.get(data_local_id) {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(&nums[..], &[Pico8Num::from_i16(1), Pico8Num::from_i16(3)]);
            }
            _ => panic!("Expected Number vector"),
        }

        let false_states = match false_data {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };
        assert_eq!(false_states.len(), 1, "False edge should get 1 state");
        let false_state = &false_states[0];
        assert_eq!(false_state.vector_size, 2, "False edge should have 2 lanes");
        match false_state.local_env.get(data_local_id) {
            Value::Number(MaybeVector::Vector(nums)) => {
                assert_eq!(&nums[..], &[Pico8Num::from_i16(2), Pico8Num::from_i16(4)]);
            }
            _ => panic!("Expected Number vector"),
        }

        assert_eq!(
            true_state.vector_size + false_state.vector_size,
            4,
            "Total lanes should equal original"
        );
    }

    /// A uniform vector condition routes the whole state down one edge
    /// unchanged, with nothing on the other edge. (With the uniform
    /// collapse in MaybeVector::vector such a condition is normally a
    /// Scalar already; the vector form is built directly here to pin the
    /// split's own handling of it.)
    #[test]
    fn test_branch_split_all_one_side() {
        let mut state = State::new();
        state.vector_size = 3;

        let cond_id = LocalId::from(0);
        state.local_env.set(
            cond_id,
            Value::Bool(MaybeVector::Vector(std::sync::Arc::new(vec![
                true, true, true,
            ]))),
        );

        let split = BoundBranchSplit {
            condition_local_id: cond_id,
        };
        let (true_data, false_data) = split.flow_split(FlowData::States(vec![state])).unwrap();

        let true_states = match true_data {
            FlowData::States(s) => s,
            _ => panic!("Expected States"),
        };
        assert_eq!(true_states.len(), 1);
        assert_eq!(true_states[0].vector_size, 3, "unchanged on the taken edge");
        assert!(false_data.is_empty(), "nothing on the untaken edge");
    }
}
