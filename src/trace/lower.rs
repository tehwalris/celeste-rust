//! Binding a TRACED frame to the engine's numbering (`bind`: paths to the
//! canonical cell ids an `Rt2` has) and lowering it: the walk's frames as
//! the kernel registry takes them (`lattice_kernel_refs`).
//!
//! Inputs and outputs are two numbering spaces: an outcome that allocates
//! or frees an object shifts every canonical id after the change, so its
//! output cells are ids in ITS structure, not the input block's.

use anyhow::Result;

use celeste_core::arith::Fixed;

use crate::transpile::garith::GraphArith;
use crate::transpile::graph::{Graph, NodeId};
use crate::transpile::graph::Ty;
use crate::transpile::kernel::{Body, Emit, OutField, OutFields, Roots};

use super::frame::Frame;
use super::walk::{room_constant_lattice, Region, Walk};

pub struct Lowered<A: GraphArith = Fixed> {
    /// One per output shape, with each field's `konst_av`.
    pub(crate) outs: Vec<OutFields<A>>,
    /// THE specialization (`lower::specialize_frame`): the fused graph and its
    /// bodies, computed once.
    pub(crate) spec: (Graph<A>, Vec<Body>),
}

/// A node's subtree, to a bounded depth, as text (for diagnostics).
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

/// One output shape of a traced frame: the cell and column type of each of
/// its fields, and what it computes (`roots`, over the engine's cells).
pub struct FrameOutcome<A: GraphArith = Fixed> {
    pub fields: Vec<(u32, Ty)>,
    pub roots: Roots,
    /// Fields stored as the UNKNOWN NUMBER, with that value: off the key's
    /// per-lane fold (`OutField::widen_uniform`).
    pub widen: Vec<(u32, celeste_engine::runtime2::AV<A>)>,
    /// Does the outcome have a player at its end (its `fin` transfer roots
    /// are real: `frame::FrameOut::arc_fin`)?
    pub arc_fin: bool,
}

/// A traced frame in the ENGINE's numbering: everything `lower_frame` needs.
pub struct Bound<A: GraphArith = Fixed> {
    /// The frame's graph over the engine's cells, each cell typed
    /// (`Graph::cell_kind`: its input layout).
    pub graph: Graph<A>,
    pub forks: u8,
    pub outcomes: Vec<FrameOutcome<A>>,
}


/// What each root of the frame's outcomes, flattened (`Roots::flat`), is:
/// for error messages.
fn root_legend<A: GraphArith>(f: &crate::trace::frame::Frame<A>) -> String {
    let mut out: Vec<String> = Vec::new();
    for (i, o) in f.outs.iter().enumerate() {
        let what = o.fields.iter().map(|(p, _, _)| format!("field {}", crate::trace::iface::show(p))).chain(["error".to_string(), "live".to_string()]).chain((0..o.arc.len()).map(|k| format!("transfer root {k}")));
        for w in what {
            out.push(format!("  #{} outcome {i} {w}", out.len()));
        }
    }
    out.join("\n")
}

/// Resolve a traced frame against the engine's numbering: the whole arena
/// `g` (frames traced through one interpreter share it) renumbered and the
/// frame's roots remapped.
pub fn bind<A: GraphArith>(f: &crate::trace::frame::Frame<A>, g: &Graph<A>) -> Result<Bound<A>> {
    let flat: Vec<NodeId> = f.outs.iter().flat_map(|o| o.roots().flat()).collect();
    let (mut graph, map) = crate::trace::bind::renumber_cells(g, &f.in_cells, &flat)
        .map_err(|e| anyhow::anyhow!("{:#}\nwhere the roots are\n{}", e, root_legend(f)))?;
    graph.reset_forks();
    for d in 0..f.forks {
        if let Some(&w) = f.fork_ways.get(d as usize) {
            graph.set_fork_ways(d, w);
        }
    }

    // THE UNKNOWN NUMBER never reaches a kernel: a field holding it is stored
    // as the uniform `AV::UNum` (its root gets a literal placeholder), and
    // any other root that reads one is refused.
    use celeste_engine::runtime2::AV;
    let zero = A::lit(A::from_i16(0));
    let placeholder = graph.leaf(crate::transpile::graph::Op::Const(zero, zero));
    let mut outcomes = Vec::with_capacity(f.outs.len());
    for o in &f.outs {
        let mut roots = o.roots().map(|n| map[n as usize]);
        let mut widen: Vec<(u32, AV<A>)> = Vec::new();
        for (i, r) in roots.fields.iter_mut().enumerate() {
            if matches!(graph.get(*r).op, crate::transpile::graph::Op::UnknownNum { .. }) {
                *r = placeholder;
                // Uniform, off the per-lane key fold (unknown BOOLEANS are stored
                // uniform by `frame::out_fields`; a widening that stores a
                // constant is the field's `konst_av`).
                widen.push((o.cells[i], AV::UNum));
            }
        }
        let fields = o.fields.iter().zip(&o.cells).map(|((_, _, ty), cell)| (*cell, *ty)).collect();
        outcomes.push(FrameOutcome { fields, roots, widen, arc_fin: o.arc_fin });
    }
    let roots: Vec<NodeId> = outcomes.iter().flat_map(|o| o.roots.flat()).collect();
    let reach = graph.reachable(&roots);
    if let Some(n) = (0..graph.len() as NodeId).find(|n| reach[*n as usize] && matches!(graph.get(*n).op, crate::transpile::graph::Op::UnknownNum { .. })) {
        // Name the roots that read it: the field, the guard or the error.
        let reaches = |r: NodeId| graph.cone(&[r]).contains(&n);
        let which: Vec<String> = roots.iter().enumerate().filter(|(_, r)| reaches(**r)).map(|(i, r)| format!("#{i} {}", show_tree(&graph, *r, 4))).collect();
        anyhow::bail!("an unknown number reaches the kernel: node {n} is read by root(s)\n{}\nwhere the roots are\n{}", which.join("\n"), root_legend(f))
    }
    Ok(Bound { graph, forks: f.forks, outcomes })
}

/// Lower a traced frame - ALL of its output shapes together, so the part of
/// the frame before outcomes diverge is emitted once.
pub fn lower_frame<A: GraphArith>(
    bound: &Bound<A>,
    room: Option<crate::transpile::graph::Room>,
    build_trace: bool,
) -> Lowered<A> {
    let (graph, outcomes, forks) = (&bound.graph, &bound.outcomes, bound.forks);
    let mut e = Emit::bare(graph.clone());
    e.room = room;
    e.fork_depth = forks as usize;
    let mut outs: Vec<crate::transpile::lower::Outcome<A>> = outcomes
        .iter()
        .map(|o| crate::transpile::lower::Outcome {
            of: OutFields {
                fields: o
                    .fields
                    .iter()
                    .map(|&(cell, ty)| OutField {
                        cell,
                        ty,
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
            roots: o.roots.clone(),
        })
        .collect();
    let spec = crate::transpile::lower::lower_outcomes(&e, &mut outs, build_trace);
    Lowered { outs: outs.into_iter().map(|o| o.of).collect(), spec }
}

/// One traced frame, bound and lowered: what the kernel registry assembles.
pub struct LoweredFrame<A: GraphArith = Fixed> {
    pub frame: Frame<A>,
    pub bound: Bound<A>,
    pub lowered: Lowered<A>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// The constant-lattice walk's frames for the start room, lowered, in walk
/// order. A frame that fails to bind or lower is FATAL.
pub(crate) fn lattice_kernel_refs<A: GraphArith>(
    opts: crate::abstraction::Level,
    s: &crate::settings::Settings,
) -> Result<Vec<(Option<Region>, LoweredFrame<A>)>> {
    let mut lw = room_constant_lattice(opts, &Walk { settings: s, only: None })?;
    let room = crate::transpile::graph::Room { cache: lw.cache.clone() };
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
    let cache = lw.cache.clone();
    let slots: Vec<std::sync::Mutex<Option<(Option<Region>, super::frame::Frame<A>, Bound<A>)>>> = frames.into_iter().map(|x| std::sync::Mutex::new(Some(x))).collect();
    let refs = crate::par::Pool::per_core().stack(128 << 20).map(slots.len(), |i| {
        let (key, f, bound) = slots[i].lock().expect("a frame's slot").take().expect("each frame is lowered once");
        let lowered = super::lower::lower_frame(&bound, Some(room.clone()), s.diag.build_trace);
        Ok((key, LoweredFrame { frame: f, bound, lowered, cache: cache.clone() }))
    })?;
    Ok(refs)
}
