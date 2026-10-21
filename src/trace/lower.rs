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
use crate::transpile::kernel::{Body, OutField, Roots};

use super::frame::Frame;
use super::walk::{room_constant_lattice, Region, Walk};

/// THE specialization of a bound frame (`lower::specialize_frame`): the
/// fused graph and its bodies, computed once.
pub struct Lowered<A: GraphArith = Fixed> {
    pub(crate) fused: Graph<A>,
    pub(crate) bodies: Vec<Body>,
}

/// One output shape of a traced frame: its stored fields, and what it
/// computes (`roots`, over the engine's cells; `roots.fields` parallel to
/// `fields`).
pub struct FrameOutcome<A: GraphArith = Fixed> {
    pub fields: Vec<OutField<A>>,
    pub roots: Roots,
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
        let mut fields: Vec<OutField<A>> = o.fields.iter().zip(&o.cells).map(|((_, _, ty), cell)| OutField { cell: *cell, ty: *ty, uniform: None }).collect();
        for (r, f) in roots.fields.iter_mut().zip(&mut fields) {
            if matches!(graph.get(*r).op, crate::transpile::graph::Op::UnknownNum { .. }) {
                *r = placeholder;
                // Uniform, off the per-lane key fold (unknown BOOLEANS are stored
                // uniform by `frame::out_fields`; a widening that stores a
                // constant is the lowering's `konst_fields`).
                f.uniform = Some(AV::UNum);
            }
        }
        outcomes.push(FrameOutcome { fields, roots, arc_fin: o.arc_fin });
    }
    let roots: Vec<NodeId> = outcomes.iter().flat_map(|o| o.roots.flat()).collect();
    let reach = graph.reachable(&roots);
    if let Some(n) = (0..graph.len() as NodeId).find(|n| reach[*n as usize] && matches!(graph.get(*n).op, crate::transpile::graph::Op::UnknownNum { .. })) {
        // Name the roots that read it: the field, the guard or the error.
        let reaches = |r: NodeId| graph.cone(&[r]).contains(&n);
        let which: Vec<String> = roots.iter().enumerate().filter(|(_, r)| reaches(**r)).map(|(i, r)| format!("#{i} {}", crate::transpile::why::Explain::of(&graph).tree(*r, 4))).collect();
        anyhow::bail!("an unknown number reaches the kernel: node {n} is read by root(s)\n{}\nwhere the roots are\n{}", which.join("\n"), root_legend(f))
    }
    Ok(Bound { graph, forks: f.forks, outcomes })
}

/// Lower a traced frame - ALL of its output shapes together, so the part of
/// the frame before outcomes diverge is emitted once - and give each field
/// the constant every body stores, where one does (`lower::konst_fields`).
pub fn lower_frame<A: GraphArith>(bound: &mut Bound<A>, room: Option<&crate::transpile::graph::Room>, build_trace: bool) -> Lowered<A> {
    let roots: Vec<Roots> = bound.outcomes.iter().map(|o| o.roots.clone()).collect();
    let (fused, bodies) = crate::transpile::lower::specialize_frame(&bound.graph, &roots, bound.forks, room, build_trace);
    for (oi, o) in bound.outcomes.iter_mut().enumerate() {
        crate::transpile::lower::konst_fields(&fused, &bodies, oi, &mut o.fields);
    }
    Lowered { fused, bodies }
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
        let (key, f, mut bound) = slots[i].lock().expect("a frame's slot").take().expect("each frame is lowered once");
        let lowered = super::lower::lower_frame(&mut bound, Some(&room), s.diag.build_trace);
        Ok((key, LoweredFrame { frame: f, bound, lowered, cache: cache.clone() }))
    })?;
    Ok(refs)
}
