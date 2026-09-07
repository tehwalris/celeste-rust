//! The frame LOWERING's inputs and outputs: `Emit` (the traced graph plus
//! the specialization knobs `transpile::lower` reads) and `OutFields`
//! (one frame's boundary outputs, each with the constant it holds in every
//! row when it holds one). `trace::emit::lower_frame` builds an
//! `Emit::bare` around the tracer's graph; the ASM backend
//! (`compiled::asm_kernel`) consumes the specialized graph through
//! `trace::emit::asm_fused` and reads `OutFields` for its accumulators.
//!
//! Abstract-domain limits (straddling splits, unknown compares, masked
//! guard failures) are PER-LANE facts in the graph (`ok`), never errors.


use super::graph::{Graph, NodeId};

/// What `transpile::lower` specializes: the member's graph and the knobs
/// of the specialization.
pub(crate) struct Emit {
    /// Number of forks in the graph (each `__split_by_flr` on a per-lane
    /// interval is a <=2-way fork). Every one is resolved at COMPILE time
    /// - `transpile::lower` enumerates each outcome over its own forks -
    /// so this is a count, not an open loop nest.
    pub(crate) fork_depth: usize,
    /// The member's value graph (plans/multi-output-fusion.md, P1').
    pub(crate) graph: Graph,
    /// Run `bdd::simplify` on the SPECIALIZED arena before emitting. On
    /// a TRACED graph it is the difference between 10,510 and 4,714
    /// nodes.
    pub(crate) decide: bool,
    /// The map, when the caller has it. `Some` makes the interval pass
    /// decide collision tests instead of treating them as unknown.
    pub(crate) room: Option<crate::transpile::graph::Room>,
}

impl Emit {
    /// An `Emit` around the tracer's graph. The caller fills in
    /// `fork_depth` and `room`.
    pub(crate) fn bare(graph: Graph) -> Self {
        Emit { fork_depth: 0, graph, decide: true, room: None }
    }
}

/// One boundary output of a frame.
pub(crate) struct OutField {
    /// Cell id this value lands in.
    pub(crate) cell: u32,
    pub(crate) ty: &'static str,
    /// The graph node this cell ends the frame holding. Every output has
    /// one: a value the emitter could not describe structurally would have
    /// failed at its bind site, not here.
    pub(crate) node: NodeId,
    /// Set when `Rt2::boundary` WIDENS this cell to a UNIFORM value at level
    /// 0 (rem -> the [-0.5, 0.5) interval, a timer -> 0). The kernel key must
    /// then contribute the WIDENED value from the constant key prefix, not
    /// the raw per-lane value - so the field is EXCLUDED from the per-lane
    /// fold (`transpile::lower`). Without it the folded key differs from
    /// `b.row_keys` and the key gate misses.
    pub(crate) widen_uniform: Option<celeste_engine::runtime2::AV>,
    /// The `AV` this cell holds in EVERY row, when every body of the
    /// outcome writes the same compile-time constant there
    /// (`lower::lower_outcomes`). Such a column is written ONCE as
    /// `Col::U` when the accumulator is built rather than pushed per row
    /// (measured on the traced room kernels: 44 of outcome 0's 52 output
    /// fields), and the within-chunk dedup fold skips it. The acc (and
    /// thus the boundary) uses THIS value - `structure_of`'s rt2 leaves
    /// some konst cells `Nil`, so keying by rt2 there is wrong.
    pub(crate) konst_av: Option<celeste_engine::runtime2::AV>,
}

/// Output cells of one frame.
pub(crate) struct OutFields {
    pub(crate) fields: Vec<OutField>,
}
