//! Lowering a TRACED graph with the kernel emitter.
//!
//! This is the join between the two halves of the campaign. `transpile::
//! lower` turns a graph into the lines of a kernel and is the emitter the
//! generated crates already use; `trace` produces a graph without any of
//! the rewrites the old front half needed. If the second graph goes
//! through the first emitter, the rewrites have nothing left to do.
//!
//! The BOUNDARY NUMBERING is `bind` below. The cell ids the emitter sees
//! are the engine's canonical ones, resolved from the tracer's paths by
//! `trace::bind` - so the lines this produces name cells an `Rt2` has.
//! The tracer's own dense slot numbering never leaves the tracer.
//!
//! Inputs and outputs are two numbering spaces, and keeping them apart
//! is the thing to get right. An outcome that allocates or frees an
//! object shifts every canonical id after the change, so its output
//! cells are ids in ITS structure, not in the input block's. The
//! generated code reads inputs off the chunk and writes outputs onto an
//! accumulator built from the outcome's structure, which are different
//! blocks - so the two spaces coexist, and an id is only meaningful
//! against the structure it came from.

use anyhow::Result;

use crate::transpile::graph::{Graph, NodeId};
use crate::transpile::kernel::{Emit, Line, OutField, OutFields};

/// What a traced frame hands the emitter.
pub struct Lowered {
    pub body: Vec<Line>,
    /// One entry per DISTINCT button assignment, each carrying its
    /// result for every outcome.
    pub(crate) variants: Vec<crate::transpile::lower::Variant>,
    /// One per output shape, with `expr` and `tainted` filled in by
    /// `emit_body`: `tainted` cells differ between variants and live in
    /// the per-variant struct, the rest are computed once.
    pub(crate) outs: Vec<OutFields>,
}

/// A node's subtree, to a bounded depth, as text. For DIAGNOSTICS: the
/// graph is a DAG with tens of thousands of nodes, so printing one whole
/// is useless, but three levels around a node says what kind of thing it
/// is. Nodes past the depth limit print as `Op#id` so they can be looked
/// up if they matter.
pub fn show_tree(g: &Graph, root: NodeId, depth: usize) -> String {
    let nd = g.get(root);
    if depth == 0 {
        return format!("{:?}#{}", nd.op, root);
    }
    if nd.args.is_empty() {
        return format!("{:?}", nd.op);
    }
    let kids: Vec<String> = nd.args.iter().map(|a| show_tree(g, *a, depth - 1)).collect();
    format!("{:?}({})", nd.op, kids.join(", "))
}

/// One output shape of a traced frame: the cells it writes, and the two
/// booleans that say which lanes reach it and which it may keep.
pub struct FrameOutcome {
    pub outputs: Vec<(u32, NodeId, &'static str)>,
    pub live: NodeId,
    pub ok: NodeId,
}

/// A traced frame in the ENGINE's numbering: everything `lower_frame`
/// needs, with cell ids an `Rt2` has.
pub struct Bound {
    pub graph: Graph,
    pub inputs: Vec<(u32, &'static str)>,
    pub uni: Vec<(u32, &'static str)>,
    pub outcomes: Vec<FrameOutcome>,
}


/// What each root index passed to `renumber_cells` is: the roots are
/// every outcome's fields then its `live` and `ok`, flattened, and a
/// failure that says "root #58" means nothing without that key.
fn root_legend(f: &crate::trace::verify::Frame) -> String {
    let mut out = Vec::new();
    let mut k = 0usize;
    for (i, o) in f.outs.iter().enumerate() {
        for (p, _, _) in &o.fields {
            out.push(format!("  #{} outcome {} field {}", k, i, crate::trace::iface::show(p)));
            k += 1;
        }
        out.push(format!("  #{} outcome {} live", k, i));
        out.push(format!("  #{} outcome {} ok", k + 1, i));
        k += 2;
    }
    out.join("\n")
}

/// Resolve a traced frame against the engine's numbering.
///
/// `g` is passed separately because the tracer's graph lives on the
/// interpreter's domain and a frame only holds node ids into it. Several
/// frames traced through one interpreter share that graph - which is how
/// per-pm1-key bodies share subexpressions - so this renumbers the whole
/// arena and remaps the frame's roots through it.
///
/// UNIFORM vs PER-LANE is a boundary decision the tracer does not model
/// yet, and the emitter needs it: `tile_flag_at` takes its width and
/// height as block-uniform `P8`, so a per-lane hitbox is a narrowing it
/// refuses. A hitbox IS uniform - it is fixed per object type and never
/// written during a frame - so saying so here is a stand-in for the
/// classification, not a fudge.
pub fn bind(f: &crate::trace::verify::Frame, g: &Graph) -> Result<Bound> {
    let mut roots: Vec<NodeId> = Vec::new();
    for o in &f.outs {
        roots.extend(o.fields.iter().map(|(_, nd, _)| *nd));
        roots.push(o.guard);
        roots.push(o.ok);
    }
    let (graph, roots) = crate::trace::bind::renumber_cells(g, &f.in_cells, &roots)
        .map_err(|e| anyhow::anyhow!("{:#}\nwhere the roots are\n{}", e, root_legend(f)))?;

    let mut inputs: Vec<(u32, &'static str)> = Vec::new();
    let mut uni: Vec<(u32, &'static str)> = Vec::new();
    for (i, c) in f.iface.init.iter().enumerate() {
        let kind = match c {
            crate::trace::iface::Conc::Num(_) => "num",
            crate::trace::iface::Conc::Bool(_) => "bool",
        };
        let cell = f.in_cells[i];
        if crate::trace::iface::show(&f.iface.slots[i]).contains(".hitbox.") {
            uni.push((cell, kind));
        } else {
            inputs.push((cell, kind));
        }
    }

    let mut at = 0usize;
    let mut outcomes = Vec::new();
    for o in &f.outs {
        let n = o.fields.len();
        let outputs = o
            .fields
            .iter()
            .enumerate()
            .map(|(i, (_, _, is_bool))| {
                (o.cells[i], roots[at + i], if *is_bool { "ZB" } else { "ZN" })
            })
            .collect();
        outcomes.push(FrameOutcome { outputs, live: roots[at + n], ok: roots[at + n + 1] });
        at += n + 2;
    }
    Ok(Bound { graph, inputs, uni, outcomes })
}

/// Lower a traced frame - ALL of its output shapes into ONE body.
///
/// A frame that kills an object ends in a different heap shape than one
/// that does not, and both are real successors. Lowering them separately
/// emits the whole frame up to the branch once per outcome. Lowering
/// them together emits it once, because they are nodes in one graph and
/// the emitter binds a node once.
pub fn lower_frame(
    graph: &Graph,
    inputs: &[(u32, &'static str)],
    uni: &[(u32, &'static str)],
    outcomes: &[FrameOutcome],
    room: Option<crate::transpile::graph::Room>,
) -> Result<Lowered> {
    let mut e = Emit::bare(graph.clone());
    e.room = room;
    // `Emit`'s own `ok`/`live` are the walk path's; the outcomes carry
    // their own, and nothing below reads these two.
    e.live = outcomes.first().map(|o| o.live).unwrap_or(0);
    e.ok = outcomes.first().map(|o| o.ok).unwrap_or(0);
    for (cell, kind) in inputs {
        e.vary_in.insert(*cell, *kind);
    }
    // Block-uniform inputs. The emitter needs the distinction: some
    // primitives take a uniform `P8` and refuse a per-lane `ZN`, since
    // narrowing is always a derivation bug.
    for (cell, kind) in uni {
        e.uni.insert(*cell, *kind);
    }
    let mut outs: Vec<crate::transpile::lower::Outcome> = outcomes
        .iter()
        .map(|o| crate::transpile::lower::Outcome {
            of: OutFields {
                fields: o
                    .outputs
                    .iter()
                    .map(|(cell, node, ty)| OutField {
                        cell: *cell,
                        ty,
                        expr: String::new(),
                        tainted: false,
                        node: *node,
                    })
                    .collect(),
                ubool: Vec::new(),
            },
            ok: o.ok,
            live: o.live,
        })
        .collect();
    crate::transpile::lower::emit_body(&mut e, &mut outs)?;
    Ok(Lowered {
        body: e.body,
        variants: e.variants,
        outs: outs.into_iter().map(|o| o.of).collect(),
    })
}
