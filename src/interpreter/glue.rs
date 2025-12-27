use std::time::Instant;

use anyhow::Result;
use indexmap::IndexSet;
use petgraph::prelude::GraphMap;

use crate::{
    block_flow::{flow_graph_of_cfg, BoundMergedBlockFlow, BoundSplitBlockFlow, FlowNode, UnboundSplitBlockFlow},
    fixed_point::Analysis,
    ir::{Cfg, Label},
    liveness::LivenessAnalysisResult,
};

use super::{
    fixed_env::FixedEnv,
    flow::{BoundInterpreterFlow, FlowData, InterpreterFlowAdapter},
    profiling::{DagOperation, FixedPointGuard, SpanGuard, with_profiler},
    vectorize::vectorize_states,
};

/// Helper to vectorize states within FlowData
fn vectorize_flow_data(data: &FlowData) -> FlowData {
    match data {
        FlowData::States(states) => {
            FlowData::States(vectorize_states(states.clone()))
        }
        FlowData::StatesAndReturns(states_and_returns) => {
            // For StatesAndReturns, we can't easily vectorize because the return values
            // may differ. For now, just return as-is.
            // TODO: Consider grouping by return value shape and vectorizing within groups
            FlowData::StatesAndReturns(states_and_returns.clone())
        }
    }
}

struct InterpreterAnalysis<'a> {
    adapter: InterpreterFlowAdapter<'a>,
    cfg: Cfg,
    graph: GraphMap<FlowNode, (), petgraph::Directed>,
    labels: IndexSet<Label>,
}

impl<'a> InterpreterAnalysis<'a> {
    pub fn new(cfg: Cfg, fixed_env: &'a FixedEnv) -> Result<Self> {
        let (graph, labels) = flow_graph_of_cfg(&cfg).unwrap();
        Ok(Self {
            adapter: InterpreterFlowAdapter { fixed_env },
            cfg,
            graph,
            labels,
        })
    }
}

impl<'a>
    Analysis<FlowNode, Option<FlowData>, BoundMergedBlockFlow<FlowData, BoundInterpreterFlow<'a>>>
    for InterpreterAnalysis<'a>
{
    fn graph(&self) -> &GraphMap<FlowNode, (), petgraph::Directed> {
        &self.graph
    }

    fn new_empty(&self) -> Option<FlowData> {
        None
    }

    fn is_empty(&self, data: &Option<FlowData>) -> bool {
        match data {
            Some(data) => data.is_empty(),
            None => true,
        }
    }

    fn is_input(&self, vertex: &FlowNode) -> bool {
        vertex == &FlowNode::BeforeEntryBlock
    }

    fn is_output(&self, vertex: &FlowNode) -> bool {
        vertex == &FlowNode::Return
    }

    fn hint_normalize(&self, vertex: &FlowNode) -> bool {
        match vertex {
            FlowNode::BeforeEntryBlock => self.cfg.entry.hint_normalize,
            FlowNode::BeforeNamedBlock(name_index) => {
                let name = self.labels.get_index(*name_index).unwrap();
                let block = self.cfg.named.get(name).unwrap();
                block.hint_normalize
            }
            _ => false,
        }
    }

    fn join_mut(&self, existing: &mut Option<FlowData>, new: &Option<FlowData>) {
        // TODO can we avoid cloning here?
        match (existing.as_mut(), new.clone()) {
            (Some(existing), Some(new)) => {
                existing.join_mut(new);
            }
            (Some(existing), None) => {}
            (None, Some(new)) => {
                *existing = Some(new);
            }
            (None, None) => {}
        }
    }

    fn accumulate(
        &self,
        accumulated: &Option<FlowData>,
        potentially_new: &Option<FlowData>,
    ) -> (Option<FlowData>, Option<FlowData>) {
        // At hint_normalize blocks, we vectorize states to merge those with the same shape.
        // This reduces state explosion during execution.
        //
        // We combine accumulated and potentially_new, vectorize them together,
        // then return the vectorized result as both the new accumulated and actually_new.
        //
        // Note: This doesn't provide exact deduplication for loop termination (which would
        // require tracking exact state equality). But it does reduce state count significantly
        // by merging states with compatible shapes.

        #[cfg(test)]
        eprintln!("[accumulate] called with accumulated={:?} states, potentially_new={:?} states",
            accumulated.as_ref().map(|d| d.counts()),
            potentially_new.as_ref().map(|d| d.counts()));

        match (accumulated, potentially_new) {
            (None, None) => (None, None),
            (None, Some(new)) => {
                let vectorized = vectorize_flow_data(new);
                #[cfg(test)]
                eprintln!("[accumulate] vectorized from {:?} to {:?}", new.counts(), vectorized.counts());
                (Some(vectorized.clone()), Some(vectorized))
            }
            (Some(acc), None) => (Some(acc.clone()), None),
            (Some(acc), Some(new)) => {
                // Combine and vectorize
                let mut combined = acc.clone();
                combined.join_mut(new.clone());
                let vectorized = vectorize_flow_data(&combined);
                #[cfg(test)]
                eprintln!("[accumulate] combined {:?} + {:?} -> vectorized {:?}",
                    acc.counts(), new.counts(), vectorized.counts());
                // Return vectorized as both accumulated and new
                // (we don't track what's "actually new" vs "already seen")
                (Some(vectorized.clone()), Some(vectorized))
            }
        }
    }

    fn bind_analyze(
        &self,
        edge: &(FlowNode, FlowNode),
    ) -> BoundMergedBlockFlow<FlowData, BoundInterpreterFlow<'a>> {
        // TODO
        let fake_liveness_result = LivenessAnalysisResult::all_live();
        // TODO don't unwrap
        BoundMergedBlockFlow::new(
            &self.adapter,
            &self.cfg,
            &self.labels,
            &fake_liveness_result,
            edge,
        )
        .unwrap()
    }
}

use super::{state::State, value::Value, fixed_env::PreparedCfg};
use crate::ir::Terminator;

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
    interpret_prepared_cfg_inner(prepared, initial_state, fixed_env, None)
}

/// Internal implementation with optional function name for profiling
pub fn interpret_prepared_cfg_with_name(
    prepared: &PreparedCfg,
    initial_state: State,
    fixed_env: &FixedEnv,
    name: Option<String>,
) -> Result<Vec<(State, Option<Value>)>> {
    interpret_prepared_cfg_inner(prepared, initial_state, fixed_env, name)
}

fn interpret_prepared_cfg_inner(
    prepared: &PreparedCfg,
    initial_state: State,
    fixed_env: &FixedEnv,
    name: Option<String>,
) -> Result<Vec<(State, Option<Value>)>> {
    use std::collections::HashMap;

    // Create profiling guard for this fixed-point invocation
    let fp_guard = FixedPointGuard::new(name.clone());
    let _span = SpanGuard::new(
        &name.as_deref().unwrap_or("interpret_cfg"),
        "fixed_point",
    );

    let adapter = InterpreterFlowAdapter { fixed_env };
    let cfg = &prepared.cfg;
    let labels = &prepared.labels;
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
    let mut hint_normalize_accumulators: HashMap<Label, (FlowData, Vec<Option<u64>>)> = HashMap::new();
    let mut results: Vec<(State, Option<Value>)> = vec![];

    // Helper to queue states for a target block
    // Returns true if states were queued to pending_blocks, false if accumulated for later
    let queue_for_block = |
        target: &Label,
        flow_data: FlowData,
        dag_id: Option<u64>,
        pending: &mut Vec<(Option<Label>, FlowData, Option<u64>)>,
        accumulators: &mut HashMap<Label, (FlowData, Vec<Option<u64>>)>,
        cfg: &Cfg,
    | {
        let target_block = cfg.named.get(target);
        let is_hint_normalize = target_block.map(|b| b.hint_normalize).unwrap_or(false);

        if is_hint_normalize {
            // Accumulate states for hint_normalize blocks
            match accumulators.entry(target.clone()) {
                std::collections::hash_map::Entry::Occupied(mut e) => {
                    let (existing_data, dag_ids) = e.get_mut();
                    existing_data.join_mut(flow_data);
                    dag_ids.push(dag_id);
                }
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert((flow_data, vec![dag_id]));
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
                // Check accumulators - drain one if available
                if let Some((label, (accumulated_data, dag_ids))) = hint_normalize_accumulators.drain().next() {
                    // Vectorize the accumulated states before processing
                    let (before_count, before_expanded) = accumulated_data.counts();
                    let vectorized = vectorize_flow_data(&accumulated_data);
                    let (after_count, after_expanded) = vectorized.counts();

                    // Create a DAG node for the vectorization if states were actually merged
                    let vec_dag_id = if after_count < before_count {
                        Some(with_profiler(|p| {
                            p.create_dag_node(
                                after_count,
                                after_expanded,
                                DagOperation::Vectorization,
                                dag_ids.into_iter().flatten().next(), // Use first parent if any
                            )
                        }))
                    } else {
                        dag_ids.into_iter().flatten().next()
                    };

                    (Some(label), vectorized, vec_dag_id)
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
        let bound_post_phi = adapter.flow_block_post_phi(block)?;
        let flow_data = bound_post_phi.flow(flow_data)?;

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
                // For each branch, apply the appropriate flow and queue
                for (is_true_branch, target) in [(true, true_target), (false, false_target)] {
                    let target_block = cfg.named.get(target).ok_or_else(|| {
                        anyhow::anyhow!("Target block not found: {:?}", target)
                    })?;

                    // Apply branch flow (this filters states based on condition)
                    let bound_branch = adapter.flow_branch(terminator, target)?;
                    let branch_flow_data = bound_branch.flow(flow_data.clone())?;

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
                        let source_label = block_label.clone().unwrap_or_else(|| {
                            Label::from("__entry".to_string())
                        });
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
                }
            }
        }
    }

    // Update fixed-point stats
    fp_guard.update_stats(iterations, states_processed, blocks_executed);
    with_profiler(|p| p.set_current_dag_node(None));

    Ok(results)
}
