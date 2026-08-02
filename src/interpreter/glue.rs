use std::hash::BuildHasherDefault;
use std::time::Instant;

use anyhow::{Context, Result};
use rustc_hash::FxHasher;

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

use crate::{
    block_flow::{BoundSplitBlockFlow, UnboundSplitBlockFlow},
    ir::{Cfg, Label, Terminator},
    liveness::LivenessAnalysisResult,
};

use super::{
    fixed_env::{FixedEnv, PreparedCfg},
    flow::{FlowData, InterpreterFlowAdapter},
    profiling::{with_profiler, DagOperation, FixedPointGuard, SpanGuard},
    state::State,
    tracing::TraceSpan,
    value::Value,
    vectorize::{union_diff_states, vectorize_states},
};

/// Interprets a CFG with the given initial state and fixed environment.
/// Returns the resulting states after execution, each with an optional return value.
///
/// This version takes a raw Cfg and computes labels on demand - use interpret_prepared_cfg
/// for better performance when the CFG will be interpreted multiple times.
pub fn interpret_cfg(
    cfg: Cfg,
    initial_state: State,
    fixed_env: &FixedEnv,
) -> Result<Vec<(State, Option<Value>)>> {
    let prepared = PreparedCfg::new(cfg);
    interpret_prepared_cfg(&prepared, initial_state, fixed_env)
}

/// Interprets a prepared CFG with cached labels.
/// This is faster than interpret_cfg when interpreting the same CFG multiple times.
pub fn interpret_prepared_cfg(
    prepared: &PreparedCfg,
    initial_state: State,
    fixed_env: &FixedEnv,
) -> Result<Vec<(State, Option<Value>)>> {
    interpret_prepared_cfg_inner(prepared, initial_state, fixed_env, None, None)
}

/// Internal implementation with optional function name for profiling
pub fn interpret_prepared_cfg_with_name(
    prepared: &PreparedCfg,
    initial_state: State,
    fixed_env: &FixedEnv,
    name: Option<String>,
    source_span: Option<crate::ir::SourceSpan>,
) -> Result<Vec<(State, Option<Value>)>> {
    interpret_prepared_cfg_inner(prepared, initial_state, fixed_env, name, source_span)
}

fn interpret_prepared_cfg_inner(
    prepared: &PreparedCfg,
    mut initial_state: State,
    fixed_env: &FixedEnv,
    name: Option<String>,
    source_span: Option<crate::ir::SourceSpan>,
) -> Result<Vec<(State, Option<Value>)>> {
    // Locals live in slots, and which slot is which is a property of the CFG we
    // are about to run. On a call the caller already built the environment
    // under the callee's map (see `interpret_call_instruction`), so this is a
    // no-op. On an entry chunk the state arrives from the previous chunk, whose
    // `Return` emptied the environment, so all that is left is to swap the map.
    initial_state
        .local_env
        .reslot(&prepared.cfg.slots);

    // Lightweight tracing span for CFG execution (low overhead)
    let _trace = TraceSpan::new(name.as_deref().unwrap_or("__main"), "cfg");

    // Create profiling guard for this fixed-point invocation (full profiler)
    let fp_guard = FixedPointGuard::new(name.clone());
    let _span = SpanGuard::new_with_source(
        &name.as_deref().unwrap_or("interpret_cfg"),
        "fixed_point",
        source_span.as_ref(),
    );

    let adapter = InterpreterFlowAdapter { fixed_env };
    let cfg = &prepared.cfg;

    // Register CFG for visualization and push onto CFG stack
    let cfg_name_str = name.as_deref().unwrap_or("__main");
    with_profiler(|p| {
        p.register_cfg(cfg, cfg_name_str, source_span);
        p.push_cfg(cfg_name_str.to_string());
    });
    let fake_liveness = LivenessAnalysisResult::all_live();

    // Track profiling stats
    let mut iterations = 0;
    let mut states_processed = 0;
    let mut blocks_executed = 0;

    // Create initial DAG node
    let initial_state_count = 1;
    let initial_expanded = initial_state.vector_size;
    let parent_dag_id = with_profiler(|p| {
        p.create_dag_node(
            initial_state_count,
            initial_expanded,
            DagOperation::Entry,
            None,
        )
    });

    // Start with the entry block
    // For hint_normalize blocks, we accumulate states before processing
    let mut pending_blocks: Vec<(Option<Label>, FlowData, Option<u64>)> = vec![(
        None, // None means entry block
        FlowData::States(vec![initial_state]),
        Some(parent_dag_id),
    )];
    // Accumulator for hint_normalize blocks - we collect states here before processing
    // The "accumulated" field persists across iterations and contains all states seen so far
    // The "pending" field contains states that arrived since last processing
    let mut hint_normalize_accumulators: FxHashMap<Label, (Vec<State>, Vec<State>, Vec<Option<u64>>)> = FxHashMap::default();
    // ^-- (accumulated_states, pending_states, dag_ids)
    let mut results: Vec<(State, Option<Value>)> = vec![];

    // Helper to queue states for a target block
    // For hint_normalize blocks, states are accumulated. For others, they go to pending.
    let queue_for_block = |
        target: &Label,
        flow_data: FlowData,
        dag_id: Option<u64>,
        pending: &mut Vec<(Option<Label>, FlowData, Option<u64>)>,
        accumulators: &mut FxHashMap<Label, (Vec<State>, Vec<State>, Vec<Option<u64>>)>,
        cfg: &Cfg,
    | {
        let target_block = cfg.named.get(target);
        let is_hint_normalize = target_block.map(|b| b.hint_normalize).unwrap_or(false);

        if is_hint_normalize {
            // Extract states from flow_data
            let new_states = match flow_data {
                FlowData::States(states) => states,
                FlowData::StatesAndReturns(_) => {
                    panic!("StatesAndReturns not expected at hint_normalize block")
                }
            };

            // Add to pending states in the accumulator
            match accumulators.entry(target.clone()) {
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    let (_, pending_states, dag_ids) = e.get_mut();
                    pending_states.extend(new_states);
                    dag_ids.push(dag_id);
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    // First time seeing this block: accumulated is empty, pending has the new states
                    e.insert((vec![], new_states, vec![dag_id]));
                }
            }
        } else {
            // Normal blocks go straight to pending
            pending.push((Some(target.clone()), flow_data, dag_id));
        }
    };

    loop {
        // First, try to pop from pending_blocks
        // If empty, check if there are accumulated states for hint_normalize blocks
        let (block_label, flow_data, dag_parent_id) = match pending_blocks.pop() {
            Some(item) => item,
            None => {
                // Check accumulators - find one with pending states
                let mut found_label: Option<Label> = None;
                for (label, (_, pending_states, _)) in hint_normalize_accumulators.iter() {
                    if !pending_states.is_empty() {
                        found_label = Some(label.clone());
                        break;
                    }
                }

                if let Some(label) = found_label {
                    let (accumulated_states, pending_states, dag_ids) =
                        hint_normalize_accumulators.get_mut(&label).unwrap();

                    // Take pending states
                    let pending = std::mem::take(pending_states);
                    let dag_ids_copy: Vec<_> = std::mem::take(dag_ids);

                    // Vectorize pending states first (to merge compatible shapes)
                    let (new_union, actually_new) = {
                        // Named so the profile can tell merging at a
                        // hint_normalize block apart from merging at the frame
                        // boundary; they are the same code but different
                        // problems.
                        let _trace = TraceSpan::new("merge_hint_normalize", "merge_site");
                        let vectorized_pending = vectorize_states(pending);
                        union_diff_states(
                            std::mem::take(accumulated_states),
                            vectorized_pending,
                        )
                    };

                    // Update the accumulator with the union (persists for next iteration)
                    *accumulated_states = new_union;

                    // Only process actually_new states
                    if actually_new.is_empty() {
                        // No new states - continue looking for other accumulators
                        continue;
                    }

                    let (state_count, expanded_count) = {
                        let s: usize = actually_new.len();
                        let e: usize = actually_new.iter().map(|st| st.vector_size).sum();
                        (s, e)
                    };

                    // Create a DAG node for the vectorization/diff operation
                    let vec_dag_id = with_profiler(|p| {
                        p.create_dag_node(
                            state_count,
                            expanded_count,
                            DagOperation::Vectorization,
                            dag_ids_copy.into_iter().flatten().next(), // Use first parent if any
                        )
                    });

                    (Some(label), FlowData::States(actually_new), Some(vec_dag_id))
                } else {
                    // Nothing left to process
                    break;
                }
            }
        };
        iterations += 1;
        let (state_count, expanded_count) = flow_data.counts();
        states_processed += state_count;
        blocks_executed += 1;

        let block_name = block_label.as_ref().map(|l| l.as_str().to_string());
        let block_start = Instant::now();

        // Create DAG node for this block execution
        let block_dag_id = with_profiler(|p| {
            p.create_dag_node(
                state_count,
                expanded_count,
                DagOperation::BlockExecution { block_name: block_name.clone() },
                dag_parent_id,
            )
        });
        with_profiler(|p| p.set_current_dag_node(Some(block_dag_id)));

        let block = match &block_label {
            None => &cfg.entry,
            Some(label) => cfg.named.get(label).ok_or_else(|| {
                anyhow::anyhow!("Block not found: {:?}", label)
            })?,
        };

        // Execute the block's instructions (post-phi flow)
        // Note: For hint_normalize blocks, states were already vectorized when pulled from accumulators
        let instruction_count = block.instructions.len();
        crate::block_coverage::record_block(
            name.as_deref().unwrap_or("__main"),
            block_label.as_ref().map_or("__entry", |l| l.as_str()),
            block,
        );
        let bound_post_phi = adapter.flow_block_post_phi(block)?;
        let flow_data = bound_post_phi.flow(flow_data).with_context(|| {
            format!(
                "in {} block '{}'",
                name.as_deref().unwrap_or("__main"),
                block_label.as_ref().map_or("__entry", |l| l.as_str())
            )
        })?;

        // Update DAG node with processing stats
        with_profiler(|p| {
            p.update_dag_node(
                block_dag_id,
                block_start.elapsed(),
                instruction_count,
                0, // call count tracked in flow
            );
        });

        // Handle the terminator
        let (_, terminator) = &block.terminator;
        match terminator {
            Terminator::Return { value: _ } => {
                // Collect the resulting states with their return values
                let bound_return = adapter.flow_return(terminator)?;
                match bound_return.flow(flow_data)? {
                    FlowData::States(states) => {
                        // No return value
                        results.extend(states.into_iter().map(|s| (s, None)));
                    }
                    FlowData::StatesAndReturns(states_and_returns) => {
                        // With return values
                        results.extend(
                            states_and_returns
                                .into_iter()
                                .map(|(s, v)| (s, Some(v))),
                        );
                    }
                }
            }
            Terminator::UnconditionalBranch { target } => {
                let target_block = cfg.named.get(target).ok_or_else(|| {
                    anyhow::anyhow!("Target block not found: {:?}", target)
                })?;

                // Apply branch flow
                let bound_branch = adapter.flow_branch(terminator, target)?;
                let flow_data = bound_branch.flow(flow_data)?;

                // Apply phi instructions if any
                let source_label = block_label.clone().unwrap_or_else(|| {
                    // Entry block doesn't have a label, but we need one for phi
                    // This case shouldn't happen in practice as entry block
                    // typically doesn't branch to blocks with phis
                    Label::from("__entry".to_string())
                });
                let bound_phi = adapter.flow_block_phi(&source_label, target_block)?;
                let flow_data = bound_phi.flow(flow_data)?;

                // Apply before-join normalization
                let bound_before_join = adapter.flow_block_before_join(&fake_liveness, target_block)?;
                let flow_data = bound_before_join.flow(flow_data)?;

                // Queue the target block (using accumulator for hint_normalize blocks)
                queue_for_block(
                    target,
                    flow_data,
                    Some(block_dag_id),
                    &mut pending_blocks,
                    &mut hint_normalize_accumulators,
                    cfg,
                );
            }
            Terminator::ConditionalBranch {
                condition: _,
                true_target,
                false_target,
            } => {
                // Process true branch with cloned data, false branch with original
                // This avoids one unnecessary clone
                let source_label = block_label.clone().unwrap_or_else(|| {
                    Label::from("__entry".to_string())
                });

                // Helper to process a branch
                let mut process_branch = |is_true_branch: bool, target: &Label, branch_flow_data: FlowData| -> Result<()> {
                    let target_block = cfg.named.get(target).ok_or_else(|| {
                        anyhow::anyhow!("Target block not found: {:?}", target)
                    })?;

                    if !branch_flow_data.is_empty() {
                        // Create DAG node for the branch split
                        let (branch_state_count, branch_expanded) = branch_flow_data.counts();
                        let branch_dag_id = with_profiler(|p| {
                            p.create_dag_node(
                                branch_state_count,
                                branch_expanded,
                                DagOperation::ConditionalSplit { branch: is_true_branch },
                                Some(block_dag_id),
                            )
                        });

                        // Apply phi instructions
                        let bound_phi = adapter.flow_block_phi(&source_label, target_block)?;
                        let branch_flow_data = bound_phi.flow(branch_flow_data)?;

                        // Apply before-join
                        let bound_before_join = adapter.flow_block_before_join(&fake_liveness, target_block)?;
                        let branch_flow_data = bound_before_join.flow(branch_flow_data)?;

                        // Queue the target block (using accumulator for hint_normalize blocks)
                        queue_for_block(
                            target,
                            branch_flow_data,
                            Some(branch_dag_id),
                            &mut pending_blocks,
                            &mut hint_normalize_accumulators,
                            cfg,
                        );
                    }
                    Ok(())
                };

                // True branch: clone the flow_data
                let bound_true = adapter.flow_branch(terminator, true_target)?;
                let true_flow_data = bound_true.flow(flow_data.clone())?;
                process_branch(true, true_target, true_flow_data)?;

                // False branch: consume the original flow_data
                let bound_false = adapter.flow_branch(terminator, false_target)?;
                let false_flow_data = bound_false.flow(flow_data)?;
                process_branch(false, false_target, false_flow_data)?;
            }
        }
    }

    // Update fixed-point stats
    fp_guard.update_stats(iterations, states_processed, blocks_executed);
    with_profiler(|p| {
        p.set_current_dag_node(None);
        p.pop_cfg();
    });

    Ok(results)
}
