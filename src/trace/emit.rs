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
    /// result for every outcome. Nothing renders these to Rust any more;
    /// the `#[ignore]` diagnostics in `trace::kernel::tests` still read
    /// them to size a lowering - which is a cfg(test) read, hence the
    /// scoped allow.
    #[cfg_attr(not(test), allow(dead_code))]
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
    /// Cells `Rt2::boundary` widens to a UNIFORM value at level 0 (rem x/y ->
    /// the [-0.5, 0.5) interval; timer globals -> 0), with that value. The key
    /// emitter mirrors the boundary: these contribute the widened value from
    /// `KPART`, off the per-lane fold. See `OutField::widen_uniform`.
    pub widen: Vec<(u32, celeste_engine::runtime2::AV)>,
}

/// A traced frame in the ENGINE's numbering: everything `lower_frame`
/// needs, with cell ids an `Rt2` has.
pub struct Bound {
    pub graph: Graph,
    /// See `Frame::forks`.
    pub forks: u8,
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
pub fn bind(f: &crate::trace::verify::Frame, g: &Graph, widen_level0: bool) -> Result<Bound> {
    let mut roots: Vec<NodeId> = Vec::new();
    for o in &f.outs {
        roots.extend(o.fields.iter().map(|(_, nd, _)| *nd));
        roots.push(o.guard);
        roots.push(o.ok);
    }
    let (graph, roots) = crate::trace::bind::renumber_cells(g, &f.in_cells, &roots)
        .map_err(|e| anyhow::anyhow!("{:#}\nwhere the roots are\n{}", e, root_legend(f)))?;

    // Only the cells the graph actually READS.
    //
    // `symbolize` turns every scalar under the roots into a cell, and
    // some of them are dead by construction: `__reset_button_states`
    // runs before the frame body, so the six button cells are
    // overwritten before anything looks at them. Declaring one anyway is
    // not merely wasteful - the kernel's row gather demands a DECIDED
    // boolean, and a button cell at a frame boundary is `AV::UBool`, so
    // a dead input is a block the kernel refuses for a value it was
    // never going to read.
    //
    // Dropping it costs no check. What a kernel must agree with a block
    // about is its SHAPE, and that is the dispatcher's hash, not this
    // list.
    let mut used = vec![false; graph.len()];
    let mut stack: Vec<NodeId> = roots.clone();
    let mut reads: std::collections::BTreeSet<u32> = Default::default();
    while let Some(n) = stack.pop() {
        if used[n as usize] {
            continue;
        }
        used[n as usize] = true;
        if let crate::transpile::graph::Op::Cell(c) = graph.get(n).op {
            reads.insert(c);
        }
        stack.extend(graph.get(n).args.iter().copied());
    }

    let mut inputs: Vec<(u32, &'static str)> = Vec::new();
    let mut uni: Vec<(u32, &'static str)> = Vec::new();
    for (i, c) in f.iface.init.iter().enumerate() {
        let kind = match c {
            // An interval slot is still a number to `Conc`, which
            // records a point because a block has to be built from one.
            // What makes it an interval is `Iface::ival`, and the
            // emitter needs to know: an `ival` input is a `ZI` lane and
            // a `num` one is a `ZN`.
            _ if f.iface.ival[i] => "ival",
            crate::trace::iface::Conc::Num(_) => "num",
            crate::trace::iface::Conc::Bool(_) => "bool",
        };
        let cell = f.in_cells[i];
        if !reads.contains(&cell) {
            continue;
        }
        if crate::trace::iface::show(&f.iface.slots[i]).contains(".hitbox.") {
            uni.push((cell, kind));
        } else {
            inputs.push((cell, kind));
        }
    }

    // Level-0 boundary WIDENINGS that make a cell UNIFORM (rem -> [-0.5, 0.5)
    // interval; timer globals -> 0). The kernel key must contribute these the
    // way `Rt2::boundary` does - from the constant KPART, off the per-lane
    // fold - or `mix64(KPART+h)` != `b.row_keys` and Option 1's probe misses.
    // Identified with the boundary's OWN walk (`mark_walk`, `g_timers`), on
    // each outcome's output structure, so this is exactly what it widens.
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::boundary_ids();
    let half = celeste_core::pico8_num::Pico8Num::from_parts(0, 0x8000);
    let rem_ival = AV::Ival(-half, half.next_smallest());
    let zero = celeste_core::pico8_num::Pico8Num::from_i16(0);
    let mut at = 0usize;
    let mut outcomes = Vec::new();
    for o in &f.outs {
        let n = o.fields.len();
        let outputs = o
            .fields
            .iter()
            .enumerate()
            .map(|(i, (_, _, ty))| {
                (o.cells[i], roots[at + i], *ty)
            })
            .collect();
        let mut widen: Vec<(u32, AV)> = Vec::new();
        // LADDER/EXACT sets go through `Rt2::boundary_exact` (no widening), so
        // their key must NOT widen. Only the LEVEL0 set (widen_level0) does.
        if widen_level0 {
            let (rem_cells, _det_cells) = o.rt2.mark_walk(&ids);
            for c in rem_cells {
                widen.push((c, rem_ival));
            }
            for &tg in &ids.g_timers {
                let cell = o.rt2.globals[tg as usize];
                if (cell as usize) < o.rt2.structure.len() {
                    widen.push((cell, AV::Num(zero)));
                }
            }
        }
        outcomes.push(FrameOutcome {
            outputs,
            live: roots[at + n],
            ok: roots[at + n + 1],
            widen,
        });
        at += n + 2;
    }
    Ok(Bound { graph, forks: f.forks, inputs, uni, outcomes })
}

/// The per-cell ASM input reprs for a bound frame (`bool`/`num`/`ival` from
/// the bound cell kinds). Input cells survive specialization unchanged, so
/// this is valid for both the raw and the fused graph.
pub fn asm_input_reprs(
    bound: &Bound,
) -> Result<std::collections::HashMap<u32, crate::transpile::asm::CellRepr>> {
    use crate::transpile::asm::CellRepr;
    let mut reprs = std::collections::HashMap::new();
    for (cell, kind) in bound.inputs.iter().chain(bound.uni.iter()) {
        let repr = match *kind {
            "bool" => CellRepr::Bool,
            "num" => CellRepr::Num,
            "ival" => CellRepr::Ival,
            other => anyhow::bail!("cell {} has unexpected input kind {:?}", cell, other),
        };
        reprs.insert(*cell, repr);
    }
    Ok(reprs)
}

/// One fused (specialized) body: which outcome it belongs to, the choices
/// it resolved, and its roots in the FUSED graph - the outcome's output
/// field nodes in order, then `ok`, then `live`.
pub struct AsmBody {
    pub outcome: usize,
    pub frees: u8,
    pub splits: u64,
    /// `outputs.len() + 2` nodes: fields..., ok, live.
    pub roots: Vec<NodeId>,
}

/// The FUSED ASM graph, its bodies, the flat root list, and the input
/// reprs for a bound frame.
///
/// `lower::specialize_frame` resolves every (button, fork) configuration
/// into ONE shared, hash-consed graph - `Free` -> constant, `Split` ->
/// `Frag` - so the result holds only ordinary ops the codegen lowers, and
/// it is the SAME compute the Rust kernel emits (both call
/// `specialize_frame`). The h1/h2 row-key fold is NOT included: the ASM
/// path recomputes the key in Rust from the output cells
/// (`Rt2::row_keys_canonical`, whose seeds match the kernel's).
///
/// `flat_roots` is every body's roots concatenated (what `compile` wants);
/// `bodies` keeps the per-body structure the append step needs.
pub fn asm_fused(
    bound: &Bound,
    room: Option<&crate::transpile::graph::Room>,
    decide: bool,
) -> Result<(
    Graph,
    Vec<AsmBody>,
    Vec<NodeId>,
    std::collections::HashMap<u32, crate::transpile::asm::CellRepr>,
)> {
    let outs_spec: Vec<(Vec<NodeId>, NodeId, NodeId)> = bound
        .outcomes
        .iter()
        .map(|o| (o.outputs.iter().map(|(_, nd, _)| *nd).collect(), o.ok, o.live))
        .collect();
    let (fused, raw_bodies) = crate::transpile::lower::specialize_frame(
        &bound.graph,
        &outs_spec,
        bound.forks,
        decide,
        room,
    );
    let mut flat_roots = Vec::new();
    let mut bodies = Vec::with_capacity(raw_bodies.len());
    for (outcome, frees, splits, roots) in raw_bodies {
        flat_roots.extend(roots.iter().copied());
        bodies.push(AsmBody { outcome, frees, splits, roots });
    }
    let reprs = asm_input_reprs(bound)?;
    Ok((fused, bodies, flat_roots, reprs))
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
    forks: u8,
) -> Result<Lowered> {
    let mut e = Emit::bare(graph.clone());
    e.room = room;
    e.fork_depth = forks as usize;
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
                        konst: None,
                        konst_av: None,
                        // Boundary-widened-to-uniform cells (rem, timers): the
                        // key emitter excludes them from the per-lane fold.
                        widen_uniform: o
                            .widen
                            .iter()
                            .find(|(c, _)| *c == *cell)
                            .map(|(_, av)| *av),
                    })
                    .collect(),
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
