//! The KERNEL EMITTER's state and output stream.
//!
//! `Emit` is what `transpile::lower` fills in while it turns a traced
//! graph (`transpile::graph`) into straight-line, branch-free, fully-typed
//! lane code, and `Line` is the stream it fills: one `Let` per bound node,
//! `Raw` for structure. `trace::emit::lower_frame` builds an `Emit::bare`
//! around the tracer's graph, `trace::kernel::render` assembles the lines
//! into the checked-in per-shape kernels, one crate per room
//! (`crates/celeste-kernels-room00/src/traced/` etc., aggregated by
//! `crates/celeste-kernels`).
//!
//! Every value has a static class - emit-time constant, block-uniform
//! scalar (computed once per block), or per-lane (16 rows per zmm) - and
//! abstract-domain limits (straddling splits, unknown compares, masked
//! guard failures) are PER-LANE facts in the graph (`ok`), never errors.
//!
//! This module used to be a second FRONT END as well: an abstract
//! evaluator that walked the rewritten IR against a shape witness at emit
//! time to produce the per-class "walk" kernels. That path was deleted
//! 2026-08-25 (plans/delete-the-interpreter.md Phase 1); the tracer is the
//! only producer of graphs now.

use std::collections::BTreeMap;

use super::graph::{Graph, NodeId};

/// One line of an emitted kernel body - and, for `Let`, one NODE of the
/// member's pure expression graph (plans/shape-tag-plan.md step 2b). The
/// bindings are kept structured rather than streamed as text so
/// `trace::kernel::render` can inspect them (`bd_v*` must be `false`, no
/// fork loop may be open) before it assembles the checked-in kernels
/// (`traced_kernels_are_current` is the gate on that).
#[derive(Clone, Debug)]
pub enum Line {
    /// A structural or effect statement: row-input loads, `zguard`/`*bd`
    /// guards, comments.
    Raw(String),
    /// `let name: ty = expr;` - a value node. `expr`'s arguments are
    /// whole-identifier variable names, witness field accesses (`u.cN`,
    /// `rin.cN`) or literals, so the graph edges recover by identifier scan.
    Let {
        name: String,
        ty: &'static str,
        expr: String,
    },
}

pub(crate) fn render_lines(lines: &[Line]) -> String {
    let mut out = String::new();
    for line in lines {
        match line {
            Line::Raw(s) => {
                out.push_str("    ");
                out.push_str(s);
                out.push('\n');
            }
            Line::Let { name, ty, expr } => {
                out.push_str(&format!("    let {}: {} = {};\n", name, ty, expr));
            }
        }
    }
    out
}

pub(crate) struct Emit {
    /// Uniform cells bound at runtime: cell id -> kind ("num"|"ival"|"bool").
    pub(crate) uni: BTreeMap<u32, &'static str>,
    /// Varying input cells: id -> ("num"|"bool").
    pub(crate) vary_in: BTreeMap<u32, &'static str>,
    /// The emitted body, in one piece. There is no prefix/suffix split
    /// any more: `transpile::lower` eliminates the free choices by
    /// specializing the graph on all 64 assignments into one interned
    /// arena, so what two assignments share is one node rather than
    /// something the compiler has to rediscover per monomorphization.
    pub(crate) body: Vec<Line>,
    /// One entry per DISTINCT free assignment (36 of 64 on steady).
    pub(crate) variants: Vec<super::lower::Variant>,
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
    /// Fold each successor's ROW KEY in the graph, and hand it to
    /// `append` instead of making `append` fold it per lane. On for the
    /// TRACER, whose kernels dedup as they write - that fold was 75% of
    /// all kernel time, see `plans/successors.md`.
    pub(crate) row_key: bool,
    /// The map, when the caller has it. `Some` makes the interval pass
    /// decide collision tests instead of treating them as unknown.
    pub(crate) room: Option<crate::transpile::graph::Room>,
}

impl Emit {
    /// An `Emit` around the tracer's graph. The caller fills in `uni`,
    /// `vary_in`, `fork_depth` and `room`; `transpile::lower` reads those
    /// and `graph`, and writes `body` and `variants`.
    pub(crate) fn bare(graph: Graph) -> Self {
        Emit {
            uni: BTreeMap::new(),
            vary_in: BTreeMap::new(),
            body: Vec::new(),
            variants: Vec::new(),
            fork_depth: 0,
            graph,
            decide: true,
            row_key: true,
            room: None,
        }
    }
}

/// One boundary output of a frame.
pub(crate) struct OutField {
    /// Cell id this value lands in.
    pub(crate) cell: u32,
    pub(crate) ty: &'static str,
    /// The emitted expression (what the lowered body calls this cell's
    /// value); the emitter itself binds by `node`.
    pub(crate) expr: String,
    /// Button-dependent (differs between the 64 variants).
    pub(crate) tainted: bool,
    /// The graph node this cell ends the frame holding. Every output has
    /// one: a value the emitter could not describe structurally would have
    /// failed at its bind site, not here.
    pub(crate) node: NodeId,
    /// The `AV` literal this cell holds in EVERY row, when it is a
    /// compile-time constant and every variant agrees.
    ///
    /// Such a column has one value for the whole accumulator, so it can
    /// be written ONCE as `Col::U` when the block is built rather than
    /// pushed per row. Measured on the traced room kernels: 44 of
    /// outcome 0's 52 output fields are `zn_splat` of a literal, so this
    /// removes most of the per-row column writes - and most of the
    /// output values that were living across the variant sequence.
    pub(crate) konst: Option<String>,
}

/// Output cells of one frame.
pub(crate) struct OutFields {
    pub(crate) fields: Vec<OutField>,
}
