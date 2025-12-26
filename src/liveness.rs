use std::collections::HashSet;

use crate::{instruction_flow::FlowSide, ir::LocalId};

/// Liveness analysis result.
/// For now, this is a stub that doesn't actually compute liveness.
/// TODO: Implement proper liveness analysis.
pub struct LivenessAnalysisResult {
    /// If set, return this set for all queries (used for "all live" stub)
    all_live: Option<HashSet<LocalId>>,
}

impl LivenessAnalysisResult {
    /// Creates a stub result where all variables are considered live.
    /// This is conservative - it won't prune any variables.
    pub fn all_live() -> Self {
        Self {
            all_live: Some(HashSet::new()),
        }
    }

    pub fn get_live_variables(
        &self,
        _side: FlowSide,
        _instruction_id: LocalId,
    ) -> Option<&HashSet<LocalId>> {
        // For the stub, return an empty set which means "keep all" since
        // the flow code retains variables that are in the live set.
        // Actually, an empty set would prune everything. We need the opposite.
        // Let's return None to indicate "all live".
        None
    }
}
