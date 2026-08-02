use rustc_hash::FxHashSet;

use crate::{instruction_flow::FlowSide, ir::LocalId};

/// Liveness information used to prune dead locals out of `LocalEnv` at join
/// points (see `BoundInterpreterFlow::BlockBeforeJoin` in `interpreter::flow`).
///
/// Pruning dead locals matters for more than memory: `local_env` is part of
/// `StateShape`, so a dead temporary left behind on one path prevents that
/// state from merging with an otherwise identical one. The OCaml
/// implementation does this; we do not yet.
///
/// This is currently a **no-op stub**. `get_live_variables` always returns
/// `None`, which the flow code interprets as "no information, keep every
/// local". That is conservative: it never prunes something that is still
/// needed, it just fails to prune anything at all.
///
/// TODO: implement a real backwards liveness analysis over the CFG.
#[derive(Default)]
pub struct LivenessAnalysisResult {}

impl LivenessAnalysisResult {
    /// Creates the conservative stub: nothing is ever pruned.
    pub fn all_live() -> Self {
        Self {}
    }

    /// Returns the set of locals live at `instruction_id`, or `None` if no
    /// liveness information is available (meaning "keep all locals").
    ///
    /// Always returns `None` today - see the type-level docs.
    pub fn get_live_variables(
        &self,
        _side: FlowSide,
        _instruction_id: LocalId,
    ) -> Option<&FxHashSet<LocalId>> {
        None
    }
}
