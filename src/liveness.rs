use std::collections::HashSet;

use crate::{instruction_flow::FlowSide, ir::LocalId};

pub struct LivenessAnalysisResult {}

impl LivenessAnalysisResult {
    pub fn get_live_variables(
        &self,
        side: FlowSide,
        instruction_id: LocalId,
    ) -> Option<&HashSet<LocalId>> {
        todo!()
    }
}
