//! Binding a TRACED frame to the engine's numbering (`bind`: paths to the
//! canonical cell ids an `Rt2` has) and lowering it: the walk's frames as
//! the kernel registry takes them (`lattice_kernel_refs`).
//!
//! Inputs and outputs are two numbering spaces: an outcome that allocates
//! or frees an object shifts every canonical id after the change, so its
//! output cells are ids in ITS structure, not the input block's. An id is
//! only meaningful against the structure it came from.

use anyhow::Result;

use celeste_core::arith::Fixed;

use crate::transpile::garith::GraphArith;
use crate::transpile::graph::{Graph, NodeId};
use crate::transpile::asm::CellRepr;
use crate::transpile::kernel::{ColTy, Emit, OutField, OutFields};

use super::frame::Frame;
use super::walk::{room_constant_lattice, Region, Walk};

/// What a lowered frame hands the kernel builder.
pub struct Lowered<A: GraphArith = Fixed> {
    /// One per output shape, with each field's `konst_av` filled in by
    /// `lower::lower_outcomes`.
    pub(crate) outs: Vec<OutFields<A>>,
    /// Number of distinct (outcome, roots) bodies the fused graph has - a
    /// size measure for the probes.
    pub bodies: usize,
    /// THE specialization (`lower::specialize_frame`): the fused graph and
    /// its bodies, computed once here and consumed by `asm_fused_from`.
    pub(crate) spec: (Graph<A>, Vec<crate::transpile::lower::SpecializedBody>),
}

/// A node's subtree, to a bounded depth, as text (for diagnostics). Nodes
/// past the depth limit print as `Op#id`.
pub fn show_tree<A: GraphArith>(g: &Graph<A>, root: NodeId, depth: usize) -> String {
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
pub struct FrameOutcome<A: GraphArith = Fixed> {
    pub outputs: Vec<(u32, NodeId, ColTy)>,
    pub live: NodeId,
    pub error: NodeId,
    /// Fields stored as the UNKNOWN NUMBER (uniform, but no graph constant
    /// `konst_av` could find), with that value: off the key's per-lane fold
    /// (`OutField::widen_uniform`).
    pub widen: Vec<(u32, celeste_engine::runtime2::AV<A::Num>)>,
    /// The transfer roots (`frame::FrameOut::arc`; empty but at level 0),
    /// after `live` in a body's roots, and whether the outcome has a player
    /// at its end.
    pub arc: Vec<NodeId>,
    pub arc_fin: bool,
}

/// A traced frame in the ENGINE's numbering: everything `lower_frame`
/// needs, with cell ids an `Rt2` has.
pub struct Bound<A: GraphArith = Fixed> {
    pub graph: Graph<A>,
    pub forks: u8,
    pub inputs: Vec<(u32, CellRepr)>,
    pub uni: Vec<(u32, CellRepr)>,
    pub outcomes: Vec<FrameOutcome<A>>,
}


/// What each root index passed to `renumber_cells` is (every outcome's
/// fields then its `live` and `error`, flattened), for error messages.
fn root_legend<A: GraphArith>(f: &crate::trace::frame::Frame<A>) -> String {
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

/// Resolve a traced frame against the engine's numbering: the whole arena
/// `g` (frames traced through one interpreter share it) renumbered and the
/// frame's roots remapped. Hitbox cells are declared UNIFORM: `tile_flag_at`
/// needs a block-uniform width and height, and a hitbox is fixed per object
/// type and never written during a frame.
pub fn bind<A: GraphArith>(f: &crate::trace::frame::Frame<A>, g: &Graph<A>) -> Result<Bound<A>> {
    let roots: Vec<NodeId> = f.outs.iter().flat_map(|o| o.roots(true)).collect();
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
    let zero = A::lit(A::from_i16(0));
    let placeholder = graph.leaf(crate::transpile::graph::Op::Const(zero, zero));
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
                let reaches = |r: NodeId| graph.cone(&[r]).contains(&n);
                let which: Vec<String> = roots.iter().enumerate().filter(|(_, r)| reaches(**r)).map(|(i, r)| format!("#{i} {}", show_tree(&graph, *r, 4))).collect();
                anyhow::bail!("an unknown number reaches the kernel: node {n} is read by root(s)\n{}\nwhere the roots are\n{}", which.join("\n"), root_legend(f))
            }
            _ => {}
        }
        stack.extend(graph.get(n).args.iter().copied());
    }

    let mut inputs: Vec<(u32, CellRepr)> = Vec::new();
    let mut uni: Vec<(u32, CellRepr)> = Vec::new();
    let hitbox = crate::trace::iface::key("hitbox");
    for (i, c) in f.iface.init.iter().enumerate() {
        let repr = match c {
            // `Iface::ival` marks an interval slot (a `ZI` lane, not `ZN`);
            // on a boolean slot, one a lane may hold unknown.
            crate::trace::iface::Conc::Bool(_) if f.iface.ival[i] => CellRepr::UBool,
            _ if f.iface.ival[i] => CellRepr::Ival,
            crate::trace::iface::Conc::Num(_) => CellRepr::Num,
            crate::trace::iface::Conc::Bool(_) => CellRepr::Bool,
        };
        let cell = f.in_cells[i];
        if !reads.contains(&cell) {
            continue;
        }
        // A field of an object's `hitbox` (`obj.hitbox.w`).
        if f.iface.slots[i].windows(2).skip(1).any(|w| w[0] == hitbox && matches!(w[1], crate::trace::iface::Step::Key(_))) {
            uni.push((cell, repr));
        } else {
            inputs.push((cell, repr));
        }
    }

    use celeste_engine::runtime2::AV;
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
        // The unknown numbers, uniform, off the per-lane key fold. (Unknown
        // BOOLEANS are not fields at all: `frame::out_fields` stores them
        // uniform. A widening that stores a constant - the remainder's
        // interval, a dead timer's 0 - is a constant node in every body,
        // which `lower::lower_outcomes` finds as the field's `konst_av`.)
        let mut widen: Vec<(u32, AV<A::Num>)> = Vec::new();
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

/// One fused (specialized) body: its outcome, the fork configuration it
/// resolved (buttons included), and its roots in the FUSED graph.
pub struct AsmBody {
    pub outcome: usize,
    pub splits: Vec<u8>,
    /// `outputs.len() + 2 + arc` nodes: fields..., error, live, transfer
    /// roots...
    pub roots: Vec<NodeId>,
}

/// A lowered frame's specialization (`Lowered::spec`) as the assembler
/// takes it: the fused graph, its bodies, their roots in one list, and the
/// input cells' reprs.
pub fn asm_fused_from<A: GraphArith>(
    bound: &Bound<A>,
    spec: &(Graph<A>, Vec<crate::transpile::lower::SpecializedBody>),
) -> Result<(
    Graph<A>,
    Vec<AsmBody>,
    Vec<NodeId>,
    std::collections::HashMap<u32, crate::transpile::asm::CellRepr>,
)> {
    let mut flat_roots = Vec::new();
    let mut bodies = Vec::with_capacity(spec.1.len());
    for (outcome, splits, roots) in spec.1.iter().cloned() {
        flat_roots.extend(roots.iter().copied());
        bodies.push(AsmBody { outcome, splits, roots });
    }
    // Input cells survive specialization unchanged.
    let reprs = bound.inputs.iter().chain(&bound.uni).copied().collect();
    Ok((spec.0.clone(), bodies, flat_roots, reprs))
}

/// Lower a traced frame - ALL of its output shapes together, so the part
/// of the frame before outcomes diverge is emitted once (one graph, each
/// node bound once). `build_trace`: `--build-trace`.
pub fn lower_frame<A: GraphArith>(
    bound: &Bound<A>,
    room: Option<crate::transpile::graph::Room>,
    build_trace: bool,
) -> Result<Lowered<A>> {
    let (graph, outcomes, forks) = (&bound.graph, &bound.outcomes, bound.forks);
    let mut e = Emit::bare(graph.clone());
    e.room = room;
    e.fork_depth = forks as usize;
    let mut outs: Vec<crate::transpile::lower::Outcome<A>> = outcomes
        .iter()
        .map(|o| crate::transpile::lower::Outcome {
            of: OutFields {
                fields: o
                    .outputs
                    .iter()
                    .map(|&(cell, node, ty)| OutField {
                        cell,
                        ty,
                        node,
                        konst_av: None,
                        // Excluded from the per-lane key fold.
                        widen_uniform: o
                            .widen
                            .iter()
                            .find(|(c, _)| *c == cell)
                            .map(|(_, av)| *av),
                    })
                    .collect(),
            },
            error: o.error,
            live: o.live,
            arc: o.arc.clone(),
        })
        .collect();
    let spec = crate::transpile::lower::lower_outcomes(&e, &mut outs, build_trace);
    let bodies = spec.1.len();
    Ok(Lowered { outs: outs.into_iter().map(|o| o.of).collect(), bodies, spec })
}

/// One traced frame, bound and lowered, everything owned: what the kernel
/// registry assembles.
pub struct LoweredFrame<A: GraphArith = Fixed> {
    pub frame: Frame<A>,
    pub bound: Bound<A>,
    pub lowered: Lowered<A>,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Restate a lowering failure with its `Cell(n)`s named by interface path.
fn name_cells<A: GraphArith>(f: &super::frame::Frame<A>, e: anyhow::Error) -> anyhow::Error {
    let msg = format!("{:#}", e);
    let mut seen: std::collections::BTreeSet<u32> = Default::default();
    let mut at = msg.as_str();
    while let Some(i) = at.find("Cell(") {
        at = &at[i + 5..];
        let end = at.find(')').unwrap_or(0);
        if let Ok(n) = at[..end].parse::<u32>() {
            seen.insert(n);
        }
    }
    let mut lines = Vec::new();
    for c in seen {
        let name = f
            .in_cells
            .iter()
            .position(|x| *x == c)
            .map(|i| super::iface::show(&f.iface.slots[i]))
            .unwrap_or_else(|| "not an input of this frame".to_string());
        lines.push(format!("  Cell({}) = {}", c, name));
    }
    anyhow::anyhow!("{}\nwhere\n{}", msg, lines.join("\n"))
}


/// The constant-lattice walk's frames for the start room, lowered, in walk
/// order. A frame that fails to bind or lower is FATAL (a coverage gap
/// otherwise).
pub(crate) fn lattice_kernel_refs<A: GraphArith>(
    root: &std::path::Path,
    opts: crate::abstraction::Level,
    s: &crate::settings::Settings,
) -> Result<Vec<(Option<Region>, LoweredFrame<A>)>> {
    let mut lw = room_constant_lattice(root, opts, &Walk { settings: s, only: None })?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    // Each frame was bound by the walk in the worker arena it was traced in.
    let frames: Vec<(Option<Region>, super::frame::Frame<A>, Bound<A>)> = std::mem::take(&mut lw.frames)
        .into_iter()
        .map(|((shape, region), wf)| -> Result<_> {
            let bound = wf.bound.map_err(|e| anyhow::anyhow!("the walk's frame of shape {shape} region {region:?} did not bind: {e}"))?;
            Ok((region, wf.frame, bound))
        })
        .collect::<Result<Vec<_>>>()?;
    // Lower (the expensive half of a build), in parallel. Each frame is
    // taken by exactly one worker (a `Frame` is not cloned).
    let (cart, cache) = (lw.cart.clone(), lw.cache.clone());
    let slots: Vec<std::sync::Mutex<Option<(Option<Region>, super::frame::Frame<A>, Bound<A>)>>> = frames.into_iter().map(|x| std::sync::Mutex::new(Some(x))).collect();
    let refs = crate::par::Pool::per_core().stack(128 << 20).map(slots.len(), |i| {
        let (key, f, bound) = slots[i].lock().expect("a frame's slot").take().expect("each frame is lowered once");
        let lowered = super::lower::lower_frame(&bound, Some(room.clone()), s.diag.build_trace).map_err(|e| anyhow::anyhow!("lattice frame {} ({:?}) lower: {:#}", i, key, name_cells(&f, e)))?;
        Ok((key, LoweredFrame { frame: f, bound, lowered, cart: cart.clone(), cache: cache.clone() }))
    })?;
    Ok(refs)
}
