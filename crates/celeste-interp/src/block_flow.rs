use anyhow::Result;

use crate::{
    ir::{Block, Label, Terminator},
    liveness::LivenessAnalysisResult,
};

/// The interpreter models each CFG edge as a sequence of smaller "flow" steps
/// (branch filtering, phi evaluation, join-point pruning, block body, return).
/// `UnboundSplitBlockFlow` produces a step for a given syntactic position;
/// `BoundSplitBlockFlow` runs it over a set of states.
///
/// The worklist that drives these steps lives in `interpreter::glue`.
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
