//! The frame lowering's inputs and outputs: `Emit` (the traced graph plus
//! the specialization knobs) and `OutFields` (one frame's boundary outputs,
//! each with the constant it holds in every row when it holds one).
//! Abstract-domain limits are per-lane facts in the graph (a body's `error`
//! mask), never Rust errors.

use celeste_core::arith::Fixed;

use super::garith::GraphArith;
use super::graph::{Graph, NodeId};

pub(crate) struct Emit<A: GraphArith = Fixed> {
    /// The forks in the graph, every one resolved at compile time.
    pub(crate) fork_depth: usize,
    pub(crate) graph: Graph<A>,
    /// Decide the boolean layer of the specialized arena before emitting:
    /// interval fold, `bdd::simplify_local`, interval fold again.
    pub(crate) decide: bool,
    /// The map, when the caller has it: lets the interval pass decide
    /// collision tests.
    pub(crate) room: Option<crate::transpile::graph::Room>,
}

impl<A: GraphArith> Emit<A> {
    /// An `Emit` around the tracer's graph (`fork_depth` and `room` the
    /// caller's).
    pub(crate) fn bare(graph: Graph<A>) -> Self {
        Emit { fork_depth: 0, graph, decide: true, room: None }
    }
}

pub(crate) struct OutField<A: GraphArith = Fixed> {
    pub(crate) cell: u32,
    /// The column's type (an interval column may hold a point).
    pub(crate) ty: super::graph::Ty,
    /// The graph node this cell ends the frame holding.
    pub(crate) node: NodeId,
    /// A field stored as a uniform value no graph constant names (the unknown
    /// number): the key takes it from the constant key prefix, so it is off the
    /// per-lane fold (or the folded key disagrees with `b.row_keys`).
    pub(crate) widen_uniform: Option<celeste_engine::runtime2::AV<A::Num>>,
    /// The `AV` this cell holds in every row, when every body of the outcome
    /// writes the same compile-time constant: written once as `Col::U`, skipped
    /// by the dedup fold. The boundary must key by THIS value (the traced
    /// skeleton leaves every scalar `Nil`).
    pub(crate) konst_av: Option<celeste_engine::runtime2::AV<A::Num>>,
}

pub(crate) struct OutFields<A: GraphArith = Fixed> {
    pub(crate) fields: Vec<OutField<A>>,
}
