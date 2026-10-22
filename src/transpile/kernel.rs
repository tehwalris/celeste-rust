//! The frame lowering's records: what an outcome computes from the trace to
//! the kernel (`Roots`, a specialized `Body`) and what it stores
//! (`OutField`: each field's cell, type and the value every row holds where
//! one does).
//! Abstract-domain limits are per-lane facts in the graph (a body's `error`
//! mask), never Rust errors.

use celeste_core::arith::Fixed;
use celeste_engine::runtime2::AV;

use super::garith::GraphArith;
use super::graph::{NodeId, Ty};

/// One stored field of an outcome.
#[derive(Clone, Debug)]
pub struct OutField<A: GraphArith = Fixed> {
    pub cell: u32,
    /// The column's type (an interval column may hold a point).
    pub ty: Ty,
    /// The `AV` this cell holds in EVERY row, where it holds one: the
    /// UNKNOWN NUMBER (a widened field, `trace::lower::bind`: no graph
    /// constant names it, so the key takes it from the constant key prefix),
    /// or the compile-time constant every body of the outcome computes
    /// (`lower::konst_fields`). Written once as `Col::U`, off the per-lane
    /// key fold; the boundary must key by THIS value (the traced skeleton
    /// leaves every scalar `Nil`).
    pub uniform: Option<AV<A>>,
}

/// WHAT AN OUTCOME COMPUTES, as graph roots, carried from the trace to the
/// kernel (`frame::FrameOut::roots`, `trace::lower::FrameOutcome`, a
/// specialized `Body`, the assembler's slots): its stored fields in field
/// order, the lanes it is live on (`live`, its guard), where its row is
/// undefined (`error`), and its transfer roots (`arc`, level-0 traces only).
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Roots {
    pub fields: Vec<NodeId>,
    pub live: NodeId,
    pub error: NodeId,
    pub arc: Option<ArcRoots>,
}

impl Roots {
    /// Every root in THE KERNEL'S ORDER, the one place they are a list:
    /// fields, `error`, `live`, transfer roots (the slots are numbered by
    /// first appearance in it).
    pub fn flat(&self) -> Vec<NodeId> {
        let mut r = self.fields.clone();
        r.extend([self.error, self.live]);
        r.extend(self.arc.iter().flat_map(ArcRoots::nodes));
        r
    }

    /// The same roots, each node replaced by `f`'s.
    pub fn map(&self, mut f: impl FnMut(NodeId) -> NodeId) -> Roots {
        let fields = self.fields.iter().map(|n| f(*n)).collect();
        let (live, error) = (f(self.live), f(self.error));
        Roots { fields, live, error, arc: self.arc.map(|a| a.map(f)) }
    }

    /// The ROW a body writes: its fields and transfer roots (two bodies of
    /// one outcome with one row are one body).
    pub fn row(&self) -> Vec<NodeId> {
        self.fields.iter().copied().chain(self.arc.iter().flat_map(ArcRoots::nodes)).collect()
    }
}

/// One axis's transfer roots (`search::rem::Capture`, `trace::state::ArcAxis`):
/// did the frame's move split the player's remainder (`took`), the split's
/// argument (`pre`), the fragment it took, the move's `ox`, and the
/// remainder at the frame's end before any widening (`fin`; 0 without a
/// player).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct AxisRoots {
    pub took: NodeId,
    pub pre: NodeId,
    pub frag: NodeId,
    pub ox: NodeId,
    pub fin: NodeId,
}

impl AxisRoots {
    /// An axis's roots, in `nodes`' order.
    pub const N: usize = 5;
    pub const NAMES: [&str; Self::N] = ["took", "pre", "frag", "ox", "fin"];
    /// `fin`'s place among them.
    pub const FIN: usize = 4;

    pub fn nodes(&self) -> [NodeId; Self::N] {
        [self.took, self.pre, self.frag, self.ox, self.fin]
    }
}

/// THE TRANSFER ROOTS of an outcome (`search::arc_edges`): per axis, and
/// whether its `fin` roots are real (the outcome ends with a player).
/// Computed per body, stored in no row.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ArcRoots {
    pub axes: [AxisRoots; 2],
    pub fin: bool,
}

impl ArcRoots {
    /// Every root, axis by axis, each axis in `AxisRoots::nodes`' order.
    pub fn nodes(&self) -> [NodeId; 2 * AxisRoots::N] {
        let [x, y] = self.axes.map(|a| a.nodes());
        std::array::from_fn(|k| if k < AxisRoots::N { x[k] } else { y[k - AxisRoots::N] })
    }

    /// The same roots, each node replaced by `f`'s, in `nodes`' order.
    pub fn map(&self, mut f: impl FnMut(NodeId) -> NodeId) -> ArcRoots {
        let axes = self.axes.map(|a| AxisRoots { took: f(a.took), pre: f(a.pre), frag: f(a.frag), ox: f(a.ox), fin: f(a.fin) });
        ArcRoots { axes, fin: self.fin }
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
