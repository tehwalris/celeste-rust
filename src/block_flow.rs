use anyhow::Result;
use indexmap::IndexSet;
use petgraph::{graphmap::GraphMap, Directed};

use crate::{
    fixed_point::BoundAnalyze,
    ir::{Block, Cfg, Label, Terminator},
    liveness::LivenessAnalysisResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FlowNode {
    BeforeEntryBlock,
    AfterEntryBlock,
    BeforeNamedBlock(usize),
    AfterNamedBlock(usize),
    Return,
}

fn targets_of_terminator(t: &Terminator, labels: &IndexSet<Label>) -> Result<Vec<FlowNode>> {
    let get_label_index = |label: &Label| -> Result<usize> {
        labels
            .get_index_of(label)
            .ok_or_else(|| anyhow!("label {:?} not found", label))
    };

    match t {
        Terminator::Return { .. } => Ok(vec![FlowNode::Return]),
        Terminator::UnconditionalBranch { target } => {
            Ok(vec![FlowNode::BeforeNamedBlock(get_label_index(target)?)])
        }
        Terminator::ConditionalBranch {
            true_target,
            false_target,
            ..
        } => Ok(vec![
            FlowNode::BeforeNamedBlock(get_label_index(true_target)?),
            FlowNode::BeforeNamedBlock(get_label_index(false_target)?),
        ]),
    }
}

pub fn flow_graph_of_cfg(cfg: &Cfg) -> Result<(GraphMap<FlowNode, (), Directed>, IndexSet<Label>)> {
    let mut g = GraphMap::<FlowNode, (), Directed>::new();

    g.add_edge(FlowNode::BeforeEntryBlock, FlowNode::AfterEntryBlock, ());
    g.add_node(FlowNode::Return);

    let mut labels = IndexSet::new();

    for name in cfg.named.keys() {
        let (name_index, _) = labels.insert_full(name.clone());
        g.add_edge(
            FlowNode::BeforeNamedBlock(name_index),
            FlowNode::AfterNamedBlock(name_index),
            (),
        );
    }

    let mut add_block_terminator_edges =
        |after_block_node: FlowNode, block: &crate::ir::Block| -> Result<()> {
            let (_, terminator) = &block.terminator;
            for target in targets_of_terminator(terminator, &labels)? {
                g.add_edge(after_block_node, target, ());
            }
            Ok(())
        };

    add_block_terminator_edges(FlowNode::AfterEntryBlock, &cfg.entry)?;
    for (name, block) in &cfg.named {
        add_block_terminator_edges(
            FlowNode::AfterNamedBlock(labels.get_index_of(name).unwrap()),
            block,
        )?;
    }

    Ok((g, labels))
}

pub trait UnboundSplitBlockFlow<T, B: BoundSplitBlockFlow<T>> {
    fn flow_block_phi(&self, source_block_name: &Label, target_block: &Block) -> Result<B>;
    fn flow_block_before_join(
        &self,
        liveness: &LivenessAnalysisResult,
        target_block: &Block,
    ) -> Result<B>;
    fn flow_block_post_phi(&self, target_block: &Block) -> Result<B>;
    fn flow_branch(&self, terminator: &Terminator, flow_target: &Label) -> Result<B>;
    fn flow_return(&self, terminator: &Terminator) -> Result<B>;
}

pub trait BoundSplitBlockFlow<T> {
    fn flow(&self, v: T) -> Result<T>;
}

pub struct BoundMergedBlockFlow<T, B: BoundSplitBlockFlow<T>> {
    _t: std::marker::PhantomData<T>,
    parts: Vec<B>,
}

impl<T, B: BoundSplitBlockFlow<T>> BoundMergedBlockFlow<T, B> {
    pub fn new(
        unbound_split_block_flow: &impl UnboundSplitBlockFlow<T, B>,
        cfg: &Cfg,
        labels: &IndexSet<Label>,
        liveness: &LivenessAnalysisResult,
        edge: &(FlowNode, FlowNode),
    ) -> Result<BoundMergedBlockFlow<T, B>> {
        let parts = match edge {
            &(FlowNode::BeforeEntryBlock, FlowNode::AfterEntryBlock) => {
                vec![unbound_split_block_flow.flow_block_post_phi(&cfg.entry)?]
            }
            &(FlowNode::BeforeNamedBlock(index), FlowNode::AfterNamedBlock(other_index))
                if index == other_index =>
            {
                let name = &labels[index];
                let block = cfg.named.get(name).unwrap();
                vec![unbound_split_block_flow.flow_block_post_phi(block)?]
            }
            &(FlowNode::AfterEntryBlock, FlowNode::BeforeNamedBlock(target_index)) => {
                let target_name = &labels[target_index];
                let (_, terminator) = &cfg.entry.terminator;
                let target_block = cfg.named.get(target_name).unwrap();
                vec![
                    unbound_split_block_flow.flow_branch(terminator, target_name)?,
                    unbound_split_block_flow.flow_block_before_join(liveness, target_block)?,
                ]
            }
            &(
                FlowNode::AfterNamedBlock(source_index),
                FlowNode::BeforeNamedBlock(target_index),
            ) => {
                let source_name = &labels[source_index];
                let target_name = &labels[target_index];
                let source_block = cfg.named.get(source_name).unwrap();
                let target_block = cfg.named.get(target_name).unwrap();
                let (_, terminator) = &source_block.terminator;
                vec![
                    unbound_split_block_flow.flow_branch(terminator, target_name)?,
                    unbound_split_block_flow.flow_block_phi(source_name, target_block)?,
                    unbound_split_block_flow.flow_block_before_join(liveness, target_block)?,
                ]
            }
            &(FlowNode::AfterEntryBlock, FlowNode::Return) => {
                let (_, terminator) = &cfg.entry.terminator;
                vec![unbound_split_block_flow.flow_return(terminator)?]
            }
            &(FlowNode::AfterNamedBlock(source_index), FlowNode::Return) => {
                let source_name = &labels[source_index];
                let source_block = cfg.named.get(source_name).unwrap();
                let (_, terminator) = &source_block.terminator;
                vec![unbound_split_block_flow.flow_return(terminator)?]
            }
            _ => panic!("flow has unexpected edge"),
        };

        Ok(BoundMergedBlockFlow {
            _t: std::marker::PhantomData,
            parts,
        })
    }

    fn flow_required(&self, v: T) -> Result<T> {
        self.parts
            .iter()
            .fold(Ok(v), |v, part| v.and_then(|v| part.flow(v)))
    }
}

impl<T, B: BoundSplitBlockFlow<T>> BoundAnalyze<Option<T>> for BoundMergedBlockFlow<T, B> {
    fn call(&self, v: Option<T>) -> Option<T> {
        // TODO don't unwrap
        v.map(|v| self.flow_required(v).unwrap())
    }
}
