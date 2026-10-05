//! The frame lowering's inputs and outputs: `Emit` (the traced graph plus
//! the specialization knobs `transpile::lower` reads) and `OutFields` (one
//! frame's boundary outputs, each with the constant it holds in every row
//! when it holds one).
//!
//! Abstract-domain limits (straddling splits, unknown compares) are
//! per-lane facts in the graph (a body's `error` mask, `trace::error`),
//! never Rust errors.


use super::graph::{Graph, NodeId};

/// What `transpile::lower` specializes: the graph and the knobs.
pub(crate) struct Emit {
    /// Number of forks in the graph. Every one is resolved at compile time
    /// (`transpile::lower` enumerates each outcome over its own forks).
    pub(crate) fork_depth: usize,
    /// The member's value graph.
    pub(crate) graph: Graph,
    /// Decide the boolean layer of the specialized arena before emitting:
    /// interval fold, `bdd::simplify_local`, interval fold again.
    pub(crate) decide: bool,
    /// The map, when the caller has it: lets the interval pass decide
    /// collision tests instead of treating them as unknown.
    pub(crate) room: Option<crate::transpile::graph::Room>,
    /// Input cells (engine numbering) known to lie in a range, folded at
    /// decide time (`ival::fold_with`).
    pub(crate) ranges: std::collections::HashMap<u32, (i32, i32)>,
}

impl Emit {
    /// An `Emit` around the tracer's graph; the caller fills in `fork_depth`
    /// and `room`.
    pub(crate) fn bare(graph: Graph) -> Self {
        Emit { fork_depth: 0, graph, decide: true, room: None, ranges: Default::default() }
    }
}

/// One boundary output of a frame.
pub(crate) struct OutField {
    /// Cell id this value lands in.
    pub(crate) cell: u32,
    pub(crate) ty: &'static str,
    /// The graph node this cell ends the frame holding.
    pub(crate) node: NodeId,
    /// Set when `Rt2::boundary` widens this cell to a uniform value. The key
    /// must then take the widened value from the constant key prefix, so the
    /// field is excluded from the per-lane fold, or the folded key disagrees
    /// with `b.row_keys`.
    pub(crate) widen_uniform: Option<celeste_engine::runtime2::AV>,
    /// The `AV` this cell holds in every row, when every body of the outcome
    /// writes the same compile-time constant (`lower::lower_outcomes`). Written
    /// once as `Col::U` and skipped by the dedup fold. The boundary must key by
    /// THIS value: `structure_of`'s rt2 leaves some constant cells `Nil`.
    pub(crate) konst_av: Option<celeste_engine::runtime2::AV>,
}

/// Output cells of one frame.
pub(crate) struct OutFields {
    pub(crate) fields: Vec<OutField>,
}
