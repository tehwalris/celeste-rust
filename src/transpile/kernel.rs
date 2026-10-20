//! The frame lowering's inputs and outputs: `Emit` (the traced graph plus
//! the specialization knobs), `OutFields` (one frame's boundary outputs,
//! each with the constant it holds in every row when it holds one), and
//! what an outcome computes from the trace to the kernel (`Roots`, a
//! specialized `Body`).
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

/// WHAT AN OUTCOME COMPUTES, as graph roots, carried from the trace to the
/// kernel (`frame::FrameOut::roots`, `trace::lower::FrameOutcome`, a
/// specialized `Body`, the assembler's slots): its stored fields in field
/// order, the lanes it is live on (`live`, its guard), where its row is
/// undefined (`error`), and its transfer roots (`arc`: per axis
/// `frame::ARC_AXIS_ROOTS`, level-0 traces only; computed per body, stored
/// in no row).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Roots {
    pub fields: Vec<NodeId>,
    pub live: NodeId,
    pub error: NodeId,
    pub arc: Vec<NodeId>,
}

impl Roots {
    /// Every root in THE KERNEL'S ORDER, the one place they are a list:
    /// fields, `error`, `live`, transfer roots (the slots are numbered by
    /// first appearance in it).
    pub fn flat(&self) -> Vec<NodeId> {
        let mut r = self.fields.clone();
        r.extend([self.error, self.live]);
        r.extend(&self.arc);
        r
    }

    /// The same roots, each node replaced by `f`'s.
    pub fn map(&self, mut f: impl FnMut(NodeId) -> NodeId) -> Roots {
        Roots { fields: self.fields.iter().map(|n| f(*n)).collect(), live: f(self.live), error: f(self.error), arc: self.arc.iter().map(|n| f(*n)).collect() }
    }

    /// The ROW a body writes: its fields and transfer roots (two bodies of
    /// one outcome with one row are one body).
    pub fn row(&self) -> Vec<NodeId> {
        self.fields.iter().chain(&self.arc).copied().collect()
    }
}

/// One specialized body (`lower::specialize_frame`): its outcome, the fork
/// configuration it resolved (`graph::ANY_VALID` for a dead fork; 0 for a
/// fork outside the outcome's cone), and its roots in the FUSED graph.
#[derive(Clone, Debug)]
pub struct Body {
    pub outcome: usize,
    pub splits: Vec<u8>,
    pub roots: Roots,
}
