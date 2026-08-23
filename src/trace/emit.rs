//! Lowering a TRACED graph with the kernel emitter.
//!
//! This is the join between the two halves of the campaign. `transpile::
//! lower` turns a graph into the lines of a kernel and is the emitter the
//! generated crates already use; `trace` produces a graph without any of
//! the rewrites the old front half needed. If the second graph goes
//! through the first emitter, the rewrites have nothing left to do.
//!
//! What is deliberately NOT here yet: the boundary numbering. The cell
//! ids below are the tracer's own, not the engine's, so the lines this
//! produces do not yet plug into `Rt2`. Matching the engine's numbering
//! would mean reproducing artifacts of the IR heap - a global holding a
//! Lua function is a `Val` cell pointing at a `Clo` cell, while a global
//! holding a builtin IS the builtin's cell, which is a fact about how the
//! IR lowering built its heap and not about the program. Since that
//! pipeline is what this campaign deletes, the numbering will be the
//! tracer's; this module exists to find out whether the graph is
//! emittable at all, which is the part nobody knows.

use anyhow::Result;

use crate::transpile::graph::{Graph, NodeId};
use crate::transpile::kernel::{Emit, Line, OutField, OutFields};

/// What a traced frame hands the emitter.
pub struct Lowered {
    pub body: Vec<Line>,
    pub variants: usize,
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
    Ok(Lowered { body: e.body, variants: e.variants.len() })
}
