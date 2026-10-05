//! Binding a TRACED frame to the engine's numbering and lowering it.
//!
//! `bind` resolves the tracer's paths to the engine's canonical cell ids
//! (`trace::bind`), so everything downstream names cells an `Rt2` has.
//!
//! Inputs and outputs are two numbering spaces: an outcome that allocates
//! or frees an object shifts every canonical id after the change, so its
//! output cells are ids in ITS structure, not the input block's. An id is
//! only meaningful against the structure it came from.

use anyhow::Result;

use crate::transpile::graph::{Graph, NodeId};
use crate::transpile::kernel::{Emit, OutField, OutFields};

/// What a lowered frame hands the kernel builder.
pub struct Lowered {
    /// One per output shape, with each field's `konst_av` filled in by
    /// `lower::lower_outcomes`.
    pub(crate) outs: Vec<OutFields>,
    /// Number of distinct (outcome, roots) bodies the fused graph has - a
    /// size measure for the probes.
    pub bodies: usize,
    /// THE specialization (`lower::specialize_frame`): the fused graph and
    /// its bodies, computed once here and consumed by `asm_fused_from`.
    pub(crate) spec: (Graph, Vec<crate::transpile::lower::SpecializedBody>),
}

/// A node's subtree, to a bounded depth, as text (for diagnostics). Nodes
/// past the depth limit print as `Op#id`.
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
/// booleans that say which lanes reach it and on which its row is undefined.
pub struct FrameOutcome {
    pub outputs: Vec<(u32, NodeId, &'static str)>,
    pub live: NodeId,
    pub error: NodeId,
    /// Cells `Rt2::boundary` widens to a UNIFORM value at level 0 (rem x/y ->
    /// [-0.5, 0.5); timer globals -> 0), with that value. The key emitter
    /// mirrors the boundary (`OutField::widen_uniform`).
    pub widen: Vec<(u32, celeste_engine::runtime2::AV)>,
    /// The transfer roots (`verify::FrameOut::arc`; empty but at level 0),
    /// after `live` in a body's roots, and whether the outcome has a player
    /// at its end.
    pub arc: Vec<NodeId>,
    pub arc_fin: bool,
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


/// What each root index passed to `renumber_cells` is (every outcome's
/// fields then its `live` and `error`, flattened), for error messages.
fn root_legend(f: &crate::trace::verify::Frame) -> String {
    let mut out = Vec::new();
    let mut k = 0usize;
    for (i, o) in f.outs.iter().enumerate() {
        for (p, _, _) in &o.fields {
            out.push(format!("  #{} outcome {} field {}", k, i, crate::trace::iface::show(p)));
            k += 1;
        }
        out.push(format!("  #{} outcome {} live", k, i));
        out.push(format!("  #{} outcome {} error", k + 1, i));
        k += 2;
    }
    out.join("\n")
}

/// Resolve a traced frame against the engine's numbering.
///
/// `g` is passed separately: frames traced through one interpreter share
/// its graph (and so subexpressions), and a frame only holds node ids into
/// it. This renumbers the whole arena and remaps the frame's roots.
///
/// Hitbox cells are declared UNIFORM: `tile_flag_at` needs a block-uniform
/// width and height, and a hitbox is fixed per object type and never
/// written during a frame.
pub fn bind(f: &crate::trace::verify::Frame, g: &Graph) -> Result<Bound> {
    let mut roots: Vec<NodeId> = Vec::new();
    for o in &f.outs {
        roots.extend(o.fields.iter().map(|(_, nd, _)| *nd));
        roots.push(o.guard);
        roots.push(o.error);
        roots.extend(o.arc.iter().copied());
    }
    let (mut graph, mut roots) = crate::trace::bind::renumber_cells(g, &f.in_cells, &roots)
        .map_err(|e| anyhow::anyhow!("{:#}\nwhere the roots are\n{}", e, root_legend(f)))?;
    // The frame's own fork arities (`Frame::fork_ways`).
    graph.reset_forks();
    for d in 0..f.forks {
        if let Some(&w) = f.fork_ways.get(d as usize) {
            graph.set_fork_ways(d, w);
        }
    }

    // Only the cells the graph actually READS: a dead input (e.g. a button
    // cell, `AV::UBool` at the boundary and overwritten before use) would
    // make the kernel refuse a block for a value it never reads. Kernel and
    // block agree on the SHAPE hash, not on this list.
    //
    // THE UNKNOWN NUMBER never reaches a kernel: a field holding it is
    // stored as the uniform `AV::UNum` (below), so its root gets a literal
    // placeholder, and any other root that reads one is refused.
    let placeholder = graph.leaf(crate::transpile::graph::Op::Const(0, 0));
    let mut unknown_fields: Vec<Vec<bool>> = Vec::with_capacity(f.outs.len());
    {
        let mut at = 0usize;
        for o in &f.outs {
            let n = o.fields.len();
            let mine: Vec<bool> = (0..n).map(|i| matches!(graph.get(roots[at + i]).op, crate::transpile::graph::Op::UnknownNum)).collect();
            for (i, u) in mine.iter().enumerate() {
                if *u {
                    roots[at + i] = placeholder;
                }
            }
            unknown_fields.push(mine);
            at += n + 2 + o.arc.len();
        }
    }
    let mut used = vec![false; graph.len()];
    let mut stack: Vec<NodeId> = roots.clone();
    let mut reads: std::collections::BTreeSet<u32> = Default::default();
    while let Some(n) = stack.pop() {
        if used[n as usize] {
            continue;
        }
        used[n as usize] = true;
        match graph.get(n).op {
            crate::transpile::graph::Op::Cell(c) => {
                reads.insert(c);
            }
            crate::transpile::graph::Op::UnknownNum => {
                // Name the roots that read it: the field, the guard or the error.
                let reaches = |r: NodeId| crate::trace::verify::cone(&graph, &[r]).contains(&n);
                let which: Vec<String> = roots.iter().enumerate().filter(|(_, r)| reaches(**r)).map(|(i, r)| format!("#{i} {}", show_tree(&graph, *r, 4))).collect();
                anyhow::bail!("an unknown number reaches the kernel: node {n} is read by root(s)\n{}\nwhere the roots are\n{}", which.join("\n"), root_legend(f))
            }
            _ => {}
        }
        stack.extend(graph.get(n).args.iter().copied());
    }

    let mut inputs: Vec<(u32, &'static str)> = Vec::new();
    let mut uni: Vec<(u32, &'static str)> = Vec::new();
    for (i, c) in f.iface.init.iter().enumerate() {
        let kind = match c {
            // `Iface::ival` marks an interval slot (a `ZI` lane, not `ZN`);
            // on a boolean slot, one a lane may hold unknown.
            crate::trace::iface::Conc::Bool(_) if f.iface.ival[i] => "ubool",
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

    // Level-0 boundary WIDENINGS that make a cell UNIFORM (rem -> [-0.5, 0.5);
    // timer globals -> 0). The kernel key must take these from the constant
    // KPART, off the per-lane fold, exactly as `Rt2::boundary` does, or the
    // keys disagree. Found with the boundary's OWN walk (`mark_walk`,
    // `g_timers`) on each outcome's structure, so the two cannot drift.
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::boundary_ids();
    let half = celeste_core::pico8_num::Pico8Num::from_parts(0, 0x8000);
    let rem_ival = AV::Ival(-half, half.next_smallest());
    let zero = celeste_core::pico8_num::Pico8Num::from_i16(0);
    let mut at = 0usize;
    let mut outcomes = Vec::new();
    for (oi, o) in f.outs.iter().enumerate() {
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
        // The unknown numbers, uniform, off the per-lane key fold. (Unknown
        // BOOLEANS are not fields at all: `verify::out_fields` stores them
        // uniform.)
        for (i, u) in unknown_fields[oi].iter().enumerate() {
            if *u {
                widen.push((o.cells[i], AV::UNum));
            }
        }
        let arc_at = at + n + 2;
        outcomes.push(FrameOutcome {
            outputs,
            live: roots[at + n],
            error: roots[at + n + 1],
            widen,
            arc: roots[arc_at..arc_at + o.arc.len()].to_vec(),
            arc_fin: o.arc_fin,
        });
        at = arc_at + o.arc.len();
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
            "ubool" => CellRepr::UBool,
            "num" => CellRepr::Num,
            "ival" => CellRepr::Ival,
            other => anyhow::bail!("cell {} has unexpected input kind {:?}", cell, other),
        };
        reprs.insert(*cell, repr);
    }
    Ok(reprs)
}

/// One fused (specialized) body: its outcome, the fork configuration it
/// resolved (buttons included), and its roots in the FUSED graph.
pub struct AsmBody {
    pub outcome: usize,
    pub splits: Vec<u8>,
    /// `outputs.len() + 2 + arc` nodes: fields..., error, live, transfer
    /// roots...
    pub roots: Vec<NodeId>,
}

/// `asm_fused` on a specialization already computed (`Lowered::spec`).
pub fn asm_fused_from(
    bound: &Bound,
    spec: &(Graph, Vec<crate::transpile::lower::SpecializedBody>),
) -> Result<(
    Graph,
    Vec<AsmBody>,
    Vec<NodeId>,
    std::collections::HashMap<u32, crate::transpile::asm::CellRepr>,
)> {
    asm_fused_of(bound, spec.0.clone(), spec.1.clone())
}

fn asm_fused_of(
    bound: &Bound,
    fused: Graph,
    raw_bodies: Vec<crate::transpile::lower::SpecializedBody>,
) -> Result<(
    Graph,
    Vec<AsmBody>,
    Vec<NodeId>,
    std::collections::HashMap<u32, crate::transpile::asm::CellRepr>,
)> {
    let mut flat_roots = Vec::new();
    let mut bodies = Vec::with_capacity(raw_bodies.len());
    for (outcome, splits, roots) in raw_bodies {
        flat_roots.extend(roots.iter().copied());
        bodies.push(AsmBody { outcome, splits, roots });
    }
    let reprs = asm_input_reprs(bound)?;
    Ok((fused, bodies, flat_roots, reprs))
}

/// Lower a traced frame - ALL of its output shapes together, so the part
/// of the frame before outcomes diverge is emitted once (one graph, each
/// node bound once).
pub fn lower_frame(
    bound: &Bound,
    room: Option<crate::transpile::graph::Room>,
    ranges: std::collections::HashMap<u32, (i32, i32)>,
) -> Result<Lowered> {
    let (graph, outcomes, forks) = (&bound.graph, &bound.outcomes, bound.forks);
    let mut e = Emit::bare(graph.clone());
    e.room = room;
    e.fork_depth = forks as usize;
    e.ranges = ranges;
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
                        node: *node,
                        konst_av: None,
                        // Excluded from the per-lane key fold.
                        widen_uniform: o
                            .widen
                            .iter()
                            .find(|(c, _)| *c == *cell)
                            .map(|(_, av)| *av),
                    })
                    .collect(),
            },
            error: o.error,
            live: o.live,
            arc: o.arc.clone(),
        })
        .collect();
    let spec = crate::transpile::lower::lower_outcomes(&e, &mut outs);
    let bodies = spec.1.len();
    Ok(Lowered { outs: outs.into_iter().map(|o| o.of).collect(), bodies, spec })
}
