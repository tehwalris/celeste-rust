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
};

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
        // For now, a simple implementation that:
        // - Joins potentially_new into accumulated
        // - Returns all potentially_new as actually_new (no deduplication)
        //
        // A more sophisticated version would deduplicate states and only return
        // truly new states. This is needed for loops to terminate.
        //
        // TODO: Implement proper state deduplication for loop termination.
        let mut new_accumulated = accumulated.clone();
        self.join_mut(&mut new_accumulated, potentially_new);
        (new_accumulated, potentially_new.clone())
    }

    fn bind_analyze(
        &self,
        edge: &(FlowNode, FlowNode),
    ) -> BoundMergedBlockFlow<FlowData, BoundInterpreterFlow<'a>> {
        // TODO
        let fake_liveness_result = LivenessAnalysisResult {};
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

use super::state::State;
use crate::ir::Terminator;

/// Interprets a CFG with the given initial state and fixed environment.
/// Returns the resulting states after execution.
///
/// This is a simplified interpreter that handles linear CFGs and simple branches.
/// It doesn't use the full fixed-point analysis machinery.
pub fn interpret_cfg(cfg: Cfg, initial_state: State, fixed_env: &FixedEnv) -> Result<Vec<State>> {
    let adapter = InterpreterFlowAdapter { fixed_env };
    let (_, labels) = flow_graph_of_cfg(&cfg)?;
    let fake_liveness = LivenessAnalysisResult {};

    // Start with the entry block
    let mut pending_blocks: Vec<(Option<Label>, FlowData)> = vec![(
        None, // None means entry block
        FlowData::States(vec![initial_state]),
    )];
    let mut results: Vec<State> = vec![];

    while let Some((block_label, flow_data)) = pending_blocks.pop() {
        let block = match &block_label {
            None => &cfg.entry,
            Some(label) => cfg.named.get(label).ok_or_else(|| {
                anyhow::anyhow!("Block not found: {:?}", label)
            })?,
        };

        // Execute the block's instructions (post-phi flow)
        let bound_post_phi = adapter.flow_block_post_phi(block)?;
        let flow_data = bound_post_phi.flow(flow_data)?;

        // Handle the terminator
        let (_, terminator) = &block.terminator;
        match terminator {
            Terminator::Return { value } => {
                // Collect the resulting states
                let bound_return = adapter.flow_return(terminator)?;
                match bound_return.flow(flow_data)? {
                    FlowData::States(states) => {
                        results.extend(states);
                    }
                    FlowData::StatesAndReturns(states_and_returns) => {
                        // For returns with values, we just take the states
                        results.extend(states_and_returns.into_iter().map(|(s, _)| s));
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

                // Queue the target block
                pending_blocks.push((Some(target.clone()), flow_data));
            }
            Terminator::ConditionalBranch {
                condition: _,
                true_target,
                false_target,
            } => {
                // For each branch, apply the appropriate flow and queue
                for target in [true_target, false_target] {
                    let target_block = cfg.named.get(target).ok_or_else(|| {
                        anyhow::anyhow!("Target block not found: {:?}", target)
                    })?;

                    // Apply branch flow (this filters states based on condition)
                    let bound_branch = adapter.flow_branch(terminator, target)?;
                    let branch_flow_data = bound_branch.flow(flow_data.clone())?;

                    if !branch_flow_data.is_empty() {
                        // Apply phi instructions
                        let source_label = block_label.clone().unwrap_or_else(|| {
                            Label::from("__entry".to_string())
                        });
                        let bound_phi = adapter.flow_block_phi(&source_label, target_block)?;
                        let branch_flow_data = bound_phi.flow(branch_flow_data)?;

                        // Apply before-join
                        let bound_before_join = adapter.flow_block_before_join(&fake_liveness, target_block)?;
                        let branch_flow_data = bound_before_join.flow(branch_flow_data)?;

                        // Queue the target block
                        pending_blocks.push((Some(target.clone()), branch_flow_data));
                    }
                }
            }
        }
    }

    Ok(results)
}
