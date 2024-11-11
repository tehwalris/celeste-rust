use anyhow::Result;
use indexmap::IndexSet;
use petgraph::prelude::GraphMap;

use crate::{
    block_flow::{flow_graph_of_cfg, BoundMergedBlockFlow, FlowNode},
    fixed_point::Analysis,
    ir::{Cfg, Label},
    liveness::LivenessAnalysisResult,
};

use super::flow::{BoundInterpreterFlow, FlowData, InterpreterFlowAdapter};

struct InterpreterAnalysis {
    adapter: InterpreterFlowAdapter,
    cfg: Cfg,
    graph: GraphMap<FlowNode, (), petgraph::Directed>,
    labels: IndexSet<Label>,
}

impl InterpreterAnalysis {
    pub fn new(cfg: Cfg) -> Result<Self> {
        let (graph, labels) = flow_graph_of_cfg(&cfg).unwrap();
        Ok(Self {
            adapter: InterpreterFlowAdapter {},
            cfg,
            graph,
            labels,
        })
    }
}

impl Analysis<FlowNode, Option<FlowData>, BoundMergedBlockFlow<FlowData, BoundInterpreterFlow>>
    for InterpreterAnalysis
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
        todo!()
    }

    fn bind_analyze(
        &self,
        edge: &(FlowNode, FlowNode),
    ) -> BoundMergedBlockFlow<FlowData, BoundInterpreterFlow> {
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

pub fn init() {}
