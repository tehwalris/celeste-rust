//! The traced-frame KERNEL pipeline's front half: trace a room's shapes,
//! bind and lower each one, and hand the result to the ASM backend
//! (`compiled::asm_kernel`, which retraces at startup and consumes
//! `lattice_kernel_refs` through `trace::emit::asm_fused`).
//!
//! `Reference` (one traced frame, everything owned), `room_kernels_in`
//! (trace and lower the start room's shapes), `room_constant_lattice`
//! (the per-shape constant fixpoint the specialization bakes in), and the
//! analysis probes (`specialize_probe`, `room_constants`) behind
//! `bin/transpile`.

use anyhow::Result;

use super::emit::{Bound, Lowered};
use super::verify::Frame;


/// One traced frame, everything owned, ready to run or to hand on.
///
/// The tracer's `Interp` borrows the parsed ASTs, so a caller outside
/// this crate cannot easily set one up. Nothing in here borrows them -
/// `Frame`, `Graph` and `Rt2` are all owned - so the whole thing can be
/// handed across a crate boundary, which is what the out-of-tree check
/// harness needs.
pub struct Reference {
    pub frame: Frame,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// One kernel per heap SHAPE the room reaches - the CONSTANT-LATTICE
/// level-0 references (the production base set). The walk-based
/// generator this used to name was retired with the non-lattice sets
/// (plans/specialize.md "Spec: latticeify everything, all rooms, one
/// table").
///
/// No pm1 pin: a kernel covers every key of its shape (the pin is worth
/// -6.3% (T13) and costs a kernel per key). The LATTICE pins are a
/// different thing: fields constant across the room's reachable states,
/// guarded by `pin_guard` in `ok`.
///
/// REFUSES rather than returns a partial set. A shape the fixpoint could
/// not trace is a kernel that will not exist, and under the never-deopt
/// doctrine that is a run that stops. Better to fail here, where the
/// reason is in hand.
pub fn room_kernels_in(root: &std::path::Path) -> Result<Vec<Reference>> {
    Ok(lattice_kernel_refs(root, super::shapes::WalkOpts::LEVEL0)?.into_iter().map(|(_, r)| r).collect())
}

/// Restate a lowering failure with its `Cell(n)`s NAMED.
///
/// The emitter works on the bound graph, where an input is a canonical
/// engine cell number and nothing downstream of the tracer knows what a
/// cell means. The interface does, so translating the numbers back is a
/// lookup - and the difference between "arms disagree in Sel(.., Cell(272))"
/// and knowing 272 is a particular object's field is the whole diagnosis.
fn name_cells(f: &super::verify::Frame, e: anyhow::Error) -> anyhow::Error {
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


/// The constant-lattice `Reference`s for the configured start room, in
/// the (deterministic) shape-key order of the fixpoint. A shape that
/// fails to bind, lower or render is FATAL: this is the sole production
/// generator, and a missing kernel is a runtime coverage gap under
/// strict mode, not a fallback.
pub(crate) fn lattice_kernel_refs(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<Vec<(KernelKey, Reference)>> {
    let mut lw = room_constant_lattice(root, opts)?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    // The frames to lower: per (shape, region) for an exact-speed set; per
    // (shape, speed key) - the key fixpoint - for a bucketed one, whose
    // kernels are specialized on the key (`key_frame`).
    // A bucketed set's frames come bound out of the key fixpoint workers'
    // arenas; an exact-speed set's frames are bound against the walk's.
    let frames: Vec<(KernelKey, super::verify::Frame, Bounds, Option<Bound>)> = match opts.spd.width_log2() {
        Some(w) => key_fixpoint(&lw, w)?.into_iter().map(|n| (KernelKey { speed: n.key, region: None }, n.frame, n.bounds, Some(n.bound))).collect(),
        // Bound by the walk, each in the worker arena it was traced in.
        None => std::mem::take(&mut lw.frames)
            .into_iter()
            .map(|((shape, region), wf)| -> Result<_> {
                let bound = wf.bound.map_err(|e| anyhow::anyhow!("the walk's frame of shape {shape} region {region:?} did not bind: {e}"))?;
                Ok((KernelKey { speed: None, region }, wf.frame, wf.bounds, Some(bound)))
            })
            .collect::<Result<Vec<_>>>()?,
    };
    // Bind + lower (specialize + decide, the expensive half of a kernel
    // build) per frame, frames in parallel.
    let (graph, cart, cache) = (&lw.tracer.it.d.graph, lw.cart.clone(), lw.cache.clone());
    let room = &room;
    // A pool of one worker per core pulling frames off an atomic index
    // (a thread per frame lowered 600 at once and hit the memory cap).
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4).min(frames.len().max(1));
    let next = std::sync::atomic::AtomicUsize::new(0);
    // Each frame is taken by exactly one worker (a `Frame` is not cloned).
    let slots: Vec<std::sync::Mutex<Option<(KernelKey, super::verify::Frame, Bounds, Option<Bound>)>>> =
        frames.into_iter().map(|x| std::sync::Mutex::new(Some(x))).collect();
    let slots = &slots;
    let mut built: Vec<(usize, Result<(KernelKey, Reference)>)> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..n_workers)
            .map(|_| {
                let (cart, cache, next) = (cart.clone(), cache.clone(), &next);
                std::thread::Builder::new()
                    .stack_size(128 * 1024 * 1024)
                    .spawn_scoped(scope, move || {
                        let mut out = Vec::new();
                        loop {
                            let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some((key, f, bounds, pre)) = slots.get(i).and_then(|m| m.lock().unwrap().take()) else { break };
                            let r = (|| -> Result<(KernelKey, Reference)> {
                                let t_phase = std::time::Instant::now();
                                let bound = match pre {
                                    Some(b) => b,
                                    None => super::emit::bind(&f, graph, opts.widen)
                                        .map_err(|e| anyhow::anyhow!("lattice frame {} ({:?}) bind: {:#}", i, key, e))?,
                                };
                                crate::transpile::lower::build_add(2, t_phase);
                                let lowered = super::emit::lower_frame(&bound, Some(room.clone()), engine_ranges(&f, &bounds))
                                    .map_err(|e| anyhow::anyhow!("lattice frame {} ({:?}) lower: {:#}", i, key, name_cells(&f, e)))?;
                                Ok((key, Reference { frame: f, bound, lowered, cart: cart.clone(), cache: cache.clone() }))
                            })();
                            out.push((i, r));
                        }
                        out
                    })
                    .expect("spawn lattice frame worker")
            })
            .collect();
        handles.into_iter().flat_map(|h| h.join().expect("lattice frame worker panicked")).collect()
    });
    built.sort_by_key(|(i, _)| *i);
    let refs = built.into_iter().map(|(_, r)| r).collect::<Result<Vec<_>>>()?;
    Ok(refs)
}

/// What a bucket kernel is specialized on: the player's speed buckets,
/// `dash_time`, and mid-dash the dash's target and accel (raw), which
/// `appr` compares the speed against.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SpeedKey {
    pub bx: u16,
    pub by: u16,
    /// Mid-dash (`dash_time > 0`). The value itself is only decremented
    /// and tested against 0, so one kernel covers 1..4 (Philippe).
    pub dashing: bool,
    /// `[target.x, target.y, accel.x, accel.y]`, all 0 when not mid-dash.
    pub dash: [i32; 4],
}

/// THE REGION KEY (room (3,0), 2026-09-17, plans/room30.md): a kernel is
/// specialized on the player's whole-pixel position lying in one square of
/// a `px`-pixel grid and its speed in `[-speed, speed]` px per frame, both
/// guarded in `ok` (a lane outside declines loudly). Room (3,0)'s frame
/// checks its 12 fall floors in every collision loop of every move step;
/// with the position unknown the trace ran for minutes, with it bounded to a
/// square the range analysis folds the far floors away (15.6 s for the spawn
/// square's whole walk). Rows dispatch on the region of their input cell.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Region {
    pub ix: i16,
    pub iy: i16,
}

/// The grid a region key is taken on (`CELESTE_REGION="px,S"`; unset: no
/// region key, one kernel per shape as before).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RegionGrid {
    pub px: i32,
    pub speed: i32,
}

/// The process's region grid, read once. `S` is at most 7: the move loop
/// is unrolled for `abs(amount) <= 8` (`Interp::unroll_bound`), and
/// `amount` is `flr(rem + spd + 0.5)`.
pub fn region_grid() -> Option<RegionGrid> {
    static GRID: std::sync::OnceLock<Option<RegionGrid>> = std::sync::OnceLock::new();
    *GRID.get_or_init(|| {
        let s = std::env::var("CELESTE_REGION").ok()?;
        let v: Vec<i32> = s.split(',').map(|t| t.trim().parse().expect("CELESTE_REGION=\"px,S\"")).collect();
        assert!(v.len() == 2 && v[0] >= 8 && (1..=7).contains(&v[1]), "CELESTE_REGION=\"px,S\" with px >= 8 and 1 <= S <= 7, not {s:?}");
        let g = RegionGrid { px: v[0], speed: v[1] };
        eprintln!("[region] kernels keyed on a {} px grid, player speed asserted within [-{}, {}] px", g.px, g.speed, g.speed);
        Some(g)
    })
}

impl RegionGrid {
    /// The region of a whole-pixel position.
    pub fn of(&self, x: i32, y: i32) -> Region {
        Region { ix: x.div_euclid(self.px) as i16, iy: y.div_euclid(self.px) as i16 }
    }

    /// The region of a row's input cell; `None` for a row without a player.
    pub fn of_cell(&self, cell: u32) -> Option<Region> {
        crate::search::pos_graph::cell_xy(cell).map(|(x, y)| self.of(x, y))
    }

    /// The bounds a region's kernel is traced under, by path under `pl`.
    fn bounds(&self, pl: &super::iface::Path, r: Region) -> Bounds {
        const ONE: i32 = 1 << 16;
        let field = |fs: &[&str]| -> super::iface::Path {
            let mut q = pl.clone();
            for f in fs {
                q.push(super::iface::key(f));
            }
            q
        };
        let (x0, y0) = (r.ix as i32 * self.px, r.iy as i32 * self.px);
        vec![
            (field(&["x"]), (x0 * ONE, (x0 + self.px - 1) * ONE)),
            (field(&["y"]), (y0 * ONE, (y0 + self.px - 1) * ONE)),
            (field(&["spd", "x"]), (-self.speed * ONE, self.speed * ONE)),
            (field(&["spd", "y"]), (-self.speed * ONE, self.speed * ONE)),
        ]
    }
}

/// What one kernel of a shape is specialized on: the speed key (bucketed
/// levels) and the region (`CELESTE_REGION`).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct KernelKey {
    pub speed: Option<SpeedKey>,
    pub region: Option<Region>,
}

/// The input bounds a frame is traced under (`verify::trace_frame`).
pub type Bounds = Vec<(super::iface::Path, (i32, i32))>;

/// A node of the constant-lattice walk: a shape key and a region.
pub type WalkNode = (String, Option<Region>);

/// The regions a state's rows can be in: `None` without a grid or without a
/// player; otherwise every region the hull of the player's whole-pixel `x`
/// and `y` touches (`Symbolic::range_of` over the frame's seeded bounds),
/// clamped to the screen and one region beyond it on each side. An axis with
/// no static range (the player the spawn creates at its own, unbounded,
/// position) takes that whole window. Both only ever ADD kernels: a row
/// outside the window has no kernel and stops the run (`[asm] MISS`).
fn successor_regions(
    st: &super::state::State<super::domain::Symbolic>,
    d: &mut super::domain::Symbolic,
    grid: Option<RegionGrid>,
) -> Result<Vec<Option<Region>>> {
    use super::{iface, shapes};
    let (Some(g), Some(pl)) = (grid, shapes::player_path(st)) else { return Ok(vec![None]) };
    let window = (-(g.px as i64), 128 + g.px as i64 - 1);
    let mut hull = [(0i64, 0i64); 2];
    for (axis, name) in ["x", "y"].into_iter().enumerate() {
        let mut p = pl.clone();
        p.push(iface::key(name));
        let Some(super::heap::Value::Num(n)) = iface::get(st, &p) else { anyhow::bail!("{}: not a number", iface::show(&p)) };
        hull[axis] = match d.range_of(n) {
            Some(pieces) if !pieces.is_empty() => {
                let lo = pieces.iter().map(|q| q.0).min().unwrap_or(0).div_euclid(1 << 16);
                let hi = pieces.iter().map(|q| q.1).max().unwrap_or(0).div_euclid(1 << 16);
                (lo.max(window.0), hi.min(window.1))
            }
            _ => window,
        };
        anyhow::ensure!(hull[axis].0 <= hull[axis].1, "{}: its range lies outside the screen window {window:?}", iface::show(&p));
    }
    let (a, b) = (g.of(hull[0].0 as i32, hull[1].0 as i32), g.of(hull[0].1 as i32, hull[1].1 as i32));
    Ok((a.iy..=b.iy).flat_map(|iy| (a.ix..=b.ix).map(move |ix| Some(Region { ix, iy }))).collect())
}

/// The bounds `dash_time` is traced under per class: `> 0` folds either
/// way. Half the 16.16 range on each side (Philippe): a value in it can
/// neither wrap the decrement nor fall outside every kernel, and one
/// beyond it declines loudly.
pub const DASHING_RANGE: (i32, i32) = (1 << 16, i32::MAX / 2);
pub const NOT_DASHING_RANGE: (i32, i32) = (i32::MIN / 2, 0);

/// The player's dispatch fields, by path under `pl`: `spd.x`, `spd.y`,
/// `dash_time`, `dash_target.x/y`, `dash_accel.x/y`.
pub fn key_paths_of(pl: &super::iface::Path) -> [super::iface::Path; 7] {
    key_paths(pl)
}

fn key_paths(pl: &super::iface::Path) -> [super::iface::Path; 7] {
    let fld = |f: &[&str]| -> super::iface::Path {
        let mut p = pl.clone();
        for x in f {
            p.push(super::iface::key(x));
        }
        p
    };
    [
        fld(&["spd", "x"]),
        fld(&["spd", "y"]),
        fld(&["dash_time"]),
        fld(&["dash_target", "x"]),
        fld(&["dash_target", "y"]),
        fld(&["dash_accel", "x"]),
        fld(&["dash_accel", "y"]),
    ]
}

/// THE BUCKET DISPATCH (2026-09-15): trace one shape specialized on the
/// player's speed key at grid width `2^w` (`celeste_core::spd_buckets`) -
/// or, for a shape without a player, unspecialized. The speed cells are
/// bounded to the key's buckets and `rem` to its `[-0.5, 0.5)`
/// (`Symbolic::ranges`: every comparison on them folds, and the boundary
/// snap forks at exactly the edges the output can cross,
/// `widen::spd_table_node`); `dash_time` and, mid-dash, the dash target
/// and accel are pinned. Both are guarded in `ok`: a lane outside declines
/// loudly. Returns the frame and the bounds it was traced under.
pub fn key_frame(
    t: &mut Tracer,
    lw: &WalkView<'_>,
    shape_hash: u64,
    key: Option<SpeedKey>,
    w: u8,
    spd_range: [(i64, i64); 2],
) -> Result<(super::verify::Frame, Vec<(super::iface::Path, (i32, i32))>)> {
    use super::iface::{self, Conc};
    use super::{shapes, verify};
    let skey = lw.by_hash.get(&shape_hash).cloned().ok_or_else(|| anyhow::anyhow!("no traced shape with hash {shape_hash:#x}"))?;
    let st = lw.reps[&skey].clone();
    let opts = lw.opts;
    let roots = shapes::state_paths(&st)?;
    let ival = if opts.ival { with_extra(shapes::ival_paths(&st, opts.spd_ival(), opts.pos_ival()), lw.ival_extra.get(&skey)) } else { Vec::new() };
    let mut pin: Vec<(iface::Path, Conc)> = lw.lattice[&skey]
        .iter()
        .filter(|(p, _)| roots.iter().any(|r| r == *p))
        .map(|(p, c)| (p.clone(), *c))
        .collect();
    let mut bounds: Vec<(iface::Path, (i32, i32))> = Vec::new();
    match (key, shapes::player_path(&st)) {
        (Some(sk), Some(pl)) => {
            use celeste_core::pico8_num::Pico8Num as P8;
            let [px, py, pdt, ptx, pty, pax, pay] = key_paths(&pl);
            // The key's WHOLE bucket on each bucketed axis (an x-only level
            // bounds only x: y is exact and not part of the key).
            let axes: Vec<(iface::Path, u16)> = if opts.spd.buckets_y() { vec![(px, sk.bx), (py, sk.by)] } else { vec![(px, sk.bx)] };
            for (axis, (p, b)) in axes.into_iter().enumerate() {
                let (blo, bhi) = celeste_core::spd_buckets::range(b, w, axis);
                let (lo, hi) = spd_range[axis];
                anyhow::ensure!(
                    lo >= blo as i64 && hi <= bhi as i64 && lo <= hi,
                    "shape {shape_hash:#x} key {sk:?}: speed range {:?} outside bucket [{blo}, {bhi}] on axis {axis}",
                    spd_range[axis]
                );
                bounds.push((p, (lo as i32, hi as i32)));
            }
            let mut rem = pl.clone();
            rem.push(iface::key("rem"));
            for ax in ["x", "y"] {
                let mut p = rem.clone();
                p.push(iface::key(ax));
                bounds.push((p, (-0x8000, 0x7fff)));
            }
            let num = |v: i32| Conc::Num(P8::from_raw(v));
            bounds.push((pdt, if sk.dashing { DASHING_RANGE } else { NOT_DASHING_RANGE }));
            let mut pins = Vec::new();
            if sk.dashing {
                pins.push((ptx, num(sk.dash[0])));
                pins.push((pty, num(sk.dash[1])));
                pins.push((pax, num(sk.dash[2])));
                pins.push((pay, num(sk.dash[3])));
            }
            for (p, c) in pins {
                if roots.iter().any(|r| *r == p) && !pin.iter().any(|(q, _)| *q == p) {
                    pin.push((p, c));
                }
            }
            bounds.retain(|(p, _)| roots.iter().any(|r| r == p));
        }
        (None, None) => {}
        (Some(_), None) => anyhow::bail!("shape {shape_hash:#x} has no player but a speed key"),
        (None, Some(_)) => anyhow::bail!("shape {shape_hash:#x} has a player but no speed key"),
    }
    let f = verify::trace_frame(&mut t.it, t.reset, t.fr, st, &roots, &pin, &ival, opts.widen_mode(), &bounds)?;
    Ok((f, bounds))
}

/// The speed key of a state whose dispatch fields are CONSTANTS (the
/// start state), or `None` for a state without a player.
fn concrete_key(st: &super::state::State<super::domain::Symbolic>, d: &super::domain::Symbolic, w: u8, buckets_y: bool) -> Result<(Option<SpeedKey>, [(i64, i64); 2])> {
    use super::domain::Domain;
    use super::{iface, shapes};
    let Some(pl) = shapes::player_path(st) else { return Ok((None, [(0, 0), (0, 0)])) };
    let paths = key_paths(&pl);
    let mut v = [0i32; 7];
    for (i, p) in paths.iter().enumerate() {
        let Some(super::heap::Value::Num(n)) = iface::get(st, p) else { anyhow::bail!("{}: not a number", iface::show(p)) };
        let Some(c) = d.as_const(&n) else { anyhow::bail!("{}: not a constant in the start state", iface::show(p)) };
        v[i] = c.as_raw_u32() as i32;
    }
    let dashing = v[2] > 0;
    let dash = if dashing { [v[3], v[4], v[5], v[6]] } else { [0; 4] };
    Ok((
        Some(SpeedKey {
            bx: celeste_core::spd_buckets::index(v[0], w, 0),
            by: if buckets_y { celeste_core::spd_buckets::index(v[1], w, 1) } else { 0 },
            dashing,
            dash,
        }),
        [(v[0] as i64, v[0] as i64), (v[1] as i64, v[1] as i64)],
    ))
}

/// The successor keys of one traced frame: per outcome, the outcome's
/// shape and - if it has a player - every speed key its rows can carry,
/// read per BUTTON CONFIGURATION (the buttons substituted, a graph
/// rewrite, so the dash fields' values are the constants of that
/// configuration rather than a product of per-field sets) from the
/// pieces of the dispatch fields' output nodes: the buckets the speed's
/// pieces touch, the constant `dash_time`, the constant target and accel
/// where mid-dash.
/// A successor: its shape, key, and the hull of the speeds that reach it,
/// per axis (within the key's bucket). `key_fixpoint` traces every key on
/// its WHOLE buckets instead (2026-09-16), so a node is traced once: the
/// range lattice (the join of these over every predecessor) re-traced
/// 42% of room (2,0)'s nodes and never finished in minutes, while x-only
/// whole buckets converged at 430 nodes with no re-trace. (The lattice was
/// introduced when both axes bucketed with no dash-constant reader spread
/// to 15,000 keys, 2026-09-15.)
type Successor = (u64, Option<SpeedKey>, [(i64, i64); 2]);

fn successor_keys(
    d: &mut super::domain::Symbolic,
    f: &super::verify::Frame,
    w: u8,
    buckets_y: bool,
) -> Result<Vec<Successor>> {
    use super::{iface, shapes};
    use crate::transpile::graph::{Op, NodeId};
    const NONE_RANGE: [(i64, i64); 2] = [(0, 0), (0, 0)];
    let mut out: Vec<Successor> = Vec::new();
    for o in &f.outs {
        let shape = o.rt2.shape_hash_of();
        let Some(pl) = shapes::player_path(&o.st) else {
            out.push((shape, None, NONE_RANGE));
            continue;
        };
        let paths = key_paths(&pl);
        let mut nodes = [0 as NodeId; 7];
        for (i, p) in paths.iter().enumerate() {
            let Some(super::heap::Value::Num(n)) = iface::get(&o.st, p) else { anyhow::bail!("{}: not a number at the boundary", iface::show(p)) };
            nodes[i] = n;
        }
        // The cone of the seven, per button configuration.
        let mut need = vec![false; d.graph.len()];
        let mut stack: Vec<NodeId> = nodes.to_vec();
        while let Some(n) = stack.pop() {
            if need[n as usize] {
                continue;
            }
            need[n as usize] = true;
            stack.extend(d.graph.get(n).args.iter().copied());
        }
        // Copy the cone ONCE: the 64 per-button passes below then walk it,
        // not the shared arena every trace grows (reading successors went
        // from 19 to 45 ms per node over room (1,0)'s fixpoint, the arena
        // from 42k to 253k nodes, 2026-09-15).
        let mut cone = d.graph.like();
        let mut cmap: Vec<NodeId> = vec![NodeId::MAX; d.graph.len()];
        for id in 0..d.graph.len() {
            if !need[id] {
                continue;
            }
            let nd = d.graph.get(id as NodeId);
            let args: Vec<NodeId> = nd.args.iter().map(|a| cmap[*a as usize]).collect();
            cmap[id] = cone.add(nd.op.clone(), args);
        }
        let cnodes: [NodeId; 7] = std::array::from_fn(|i| cmap[nodes[i] as usize]);
        let seeds_by_node: Vec<(NodeId, (i64, i64))> =
            d.ranges.iter().filter(|(n, _)| need[**n as usize]).map(|(n, r)| (cmap[*n as usize], *r)).collect();
        let mut seen: std::collections::BTreeSet<[NodeId; 7]> = Default::default();
        // Per button configuration, and within it per assignment of the
        // conditions the fields select on (`djump > 0` decides both
        // `dash_time` and the target: read jointly, or the target's set
        // would include the unbounded old value under `dash_time = 4`).
        let mut cases: Vec<(crate::transpile::graph::Graph, [NodeId; 7], std::collections::HashMap<NodeId, (i64, i64)>)> = Vec::new();
        // Every configuration specializes into ONE graph, which hash-conses,
        // so two configurations with the same seven fields get the same ids
        // and the second is skipped (as `lower::specialize_frame` does). A
        // fresh graph per configuration compared ids ACROSS graphs:
        // symmetric configurations built different constants at the same
        // ids, and of the four diagonal dash starts only the first
        // configuration's was ever read (room (2,0) f37: a down-left dash
        // start with no kernel, 2026-09-16).
        let mut shared = cone.like();
        for m in 0u8..64 {
            let map = cone.specialize_subset_into(m, None, None, None, &mut shared);
            let ssig: [NodeId; 7] = std::array::from_fn(|i| map[cnodes[i] as usize]);
            if !seen.insert(ssig) {
                continue;
            }
            // This configuration's cone of the seven, copied out of the
            // shared graph for the case analysis below to rewrite.
            let mut sneed = vec![false; shared.len()];
            let mut stack: Vec<NodeId> = ssig.to_vec();
            while let Some(n) = stack.pop() {
                if sneed[n as usize] {
                    continue;
                }
                sneed[n as usize] = true;
                stack.extend(shared.get(n).args.iter().copied());
            }
            let mut probe = shared.like();
            let mut pmap: Vec<NodeId> = vec![NodeId::MAX; shared.len()];
            for id in 0..shared.len() {
                if !sneed[id] {
                    continue;
                }
                let nd = shared.get(id as NodeId);
                let args: Vec<NodeId> = nd.args.iter().map(|a| pmap[*a as usize]).collect();
                pmap[id] = probe.add(nd.op.clone(), args);
            }
            let sig: [NodeId; 7] = std::array::from_fn(|i| pmap[ssig[i] as usize]);
            let seeds: std::collections::HashMap<NodeId, (i64, i64)> = seeds_by_node
                .iter()
                .filter(|(n, _)| sneed[map[*n as usize] as usize])
                .map(|(n, r)| (pmap[map[*n as usize] as usize], *r))
                .collect();
            // Read the dash constants JOINTLY with `dash_time`. Phase 1:
            // the paths of `dash_time`'s select tree (dash pressed, `djump
            // > 0`, mid-dash), each fixing the conditions on its path.
            // Phase 2: within a path, the selects the target and accel
            // still depend on (`flip.x` for a no-direction dash), innermost
            // first (ids are topological), so the outer `sign(..) > 0`
            // folds once the inner speed is fixed. A comparison whose
            // operands have static pieces is decided from them rather than
            // split. The speeds' own conditions (on ground, on ice, the
            // clamp) are left to the bucket product - splitting on every
            // collision test in their cones ran to 65,536 cases
            // (2026-09-15).
            type Case = (crate::transpile::graph::Graph, [NodeId; 7], std::collections::HashMap<NodeId, (i64, i64)>);
            const MAX_CASES_PER_REP: usize = 4096;
            let cone_of = |g: &crate::transpile::graph::Graph, roots: &[NodeId]| -> Vec<bool> {
                let mut v = vec![false; g.len()];
                let mut stack: Vec<NodeId> = roots.to_vec();
                while let Some(n) = stack.pop() {
                    if v[n as usize] {
                        continue;
                    }
                    v[n as usize] = true;
                    stack.extend(g.get(n).args.iter().copied());
                }
                v
            };
            // The condition to split next for `roots`: the first undecided
            // select on their VALUE paths (outermost first), and of its
            // condition, the SOURCE - a numeric comparison (or its negation)
            // computed through a select is split on that select's condition
            // instead, recursively, so `spd.x ~= 0` over the no-direction
            // dash's `flip and -1 or 1` splits on the flip and folds, rather
            // than being split on its own and paired with a direction that
            // contradicts it (target (2, 0) with accel (1.5, 1.5)). Any other
            // condition - a boolean cell, a boolean select (the facing
            // update), a collision test - is opaque: walking through those
            // into the physics that decides them (the move's collision tests,
            // the speed clamp) ran past 4096 cases once a coarse speed table
            // left them undecided (2026-09-16).
            fn cond_source(g: &crate::transpile::graph::Graph, c: NodeId) -> NodeId {
                if !matches!(g.get(c).op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Not) {
                    return c;
                }
                let mut stack = vec![c];
                let mut seen: std::collections::HashSet<NodeId> = Default::default();
                while let Some(n) = stack.pop() {
                    if !seen.insert(n) {
                        continue;
                    }
                    let nd = g.get(n);
                    match nd.op {
                        Op::Sel if !matches!(g.get(nd.args[0]).op, Op::ConstBool(_)) => return cond_source(g, nd.args[0]),
                        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Not | Op::Sel => stack.extend(nd.args.iter().rev().copied()),
                        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Flr | Op::Min | Op::Max => {
                            stack.extend(nd.args.iter().rev().copied())
                        }
                        _ => {}
                    }
                }
                c
            }
            let next_value_cond = |g: &crate::transpile::graph::Graph, roots: &[NodeId]| -> Option<NodeId> {
                let mut seen: std::collections::HashSet<NodeId> = Default::default();
                let mut stack: Vec<NodeId> = roots.iter().rev().copied().collect();
                while let Some(n) = stack.pop() {
                    if !seen.insert(n) {
                        continue;
                    }
                    let nd = g.get(n);
                    if matches!(nd.op, Op::Sel) {
                        if !matches!(g.get(nd.args[0]).op, Op::ConstBool(_)) {
                            return Some(cond_source(g, nd.args[0]));
                        }
                        stack.push(nd.args[2]);
                        stack.push(nd.args[1]);
                    } else {
                        stack.extend(nd.args.iter().rev().copied());
                    }
                }
                None
            };
            // A condition decided by the pieces of its operands.
            let decide = |g: &crate::transpile::graph::Graph, seeds: &std::collections::HashMap<NodeId, (i64, i64)>, c: NodeId| -> Option<bool> {
                let nd = g.get(c);
                let mut memo = std::collections::HashMap::new();
                let one = |op: &Op, x: (i64, i64), y: (i64, i64)| -> Option<bool> {
                    match op {
                        Op::Lt => if x.1 < y.0 { Some(true) } else if x.0 >= y.1 { Some(false) } else { None },
                        Op::Le => if x.1 <= y.0 { Some(true) } else if x.0 > y.1 { Some(false) } else { None },
                        Op::Gt => if x.0 > y.1 { Some(true) } else if x.1 <= y.0 { Some(false) } else { None },
                        Op::Ge => if x.0 >= y.1 { Some(true) } else if x.1 < y.0 { Some(false) } else { None },
                        Op::Eq => if x.0 == x.1 && y.0 == y.1 && x.0 == y.0 { Some(true) } else if x.1 < y.0 || y.1 < x.0 { Some(false) } else { None },
                        _ => None,
                    }
                };
                match nd.op {
                    Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => {
                        let xs = crate::transpile::graph::pieces_of(g, seeds, &mut memo, nd.args[0])?;
                        let ys = crate::transpile::graph::pieces_of(g, seeds, &mut memo, nd.args[1])?;
                        let mut r: Option<Option<bool>> = None;
                        for x in &xs {
                            for y in &ys {
                                let d = one(&nd.op, *x, *y);
                                match r {
                                    None => r = Some(d),
                                    Some(p) if p == d => {}
                                    _ => return None,
                                }
                            }
                        }
                        r.flatten()
                    }
                    _ => None,
                }
            };
            let split = |case: &Case, c: NodeId, b: bool| -> Case {
                let (g2, sig2, seeds2) = case;
                let need2 = cone_of(g2, sig2);
                let mut g3 = g2.like();
                let m3 = g2.substitute_bools(&need2, &std::collections::HashMap::from([(c, b)]), &mut g3);
                let sig3: [NodeId; 7] = std::array::from_fn(|i| m3[sig2[i] as usize]);
                let seeds3: std::collections::HashMap<NodeId, (i64, i64)> =
                    seeds2.iter().filter(|(n, _)| need2[**n as usize]).map(|(n, r)| (m3[*n as usize], *r)).collect();
                (g3, sig3, seeds3)
            };
            // Each phase walks one field's select TREE - `dash_time`, then
            // `spd.x`, then `spd.y` - splitting on the conditions of the
            // selects the field is built from, each condition opaque
            // (splitting INSIDE a condition, through `on_ground`'s
            // collision tests, ran to thousands of cases per rep). Once
            // the speeds are fixed the dash target and accel, which are
            // computed from them, fold to constants.
            let mut done: Vec<Case> = Vec::new();
            for field in [2usize, 0, 1] {
                let start: Vec<Case> = if field == 2 { vec![(probe.clone(), sig, seeds.clone())] } else { std::mem::take(&mut done) };
                let mut todo = start;
                while let Some(case) = todo.pop() {
                    let nd = case.0.get(case.1[field]);
                    let cond = match nd.op {
                        Op::Sel if !matches!(case.0.get(nd.args[0]).op, Op::ConstBool(_)) => Some(nd.args[0]),
                        _ => None,
                    };
                    let phase = field;
                    match cond {
                        None => done.push(case),
                        Some(c) => match decide(&case.0, &case.2, c) {
                            Some(b) => todo.push(split(&case, c, b)),
                            None => {
                                todo.push(split(&case, c, true));
                                todo.push(split(&case, c, false));
                            }
                        },
                    }
                    anyhow::ensure!(done.len() + todo.len() <= MAX_CASES_PER_REP, "outcome {shape:#x}: over {MAX_CASES_PER_REP} select cases for the dispatch fields in one button configuration (phase {phase})");
                }
            }
            // Mid-dash, the dash target and accel are read JOINTLY, as one
            // constant each: split on the undecided selects on their value
            // paths (`next_value_cond`) until all four are single points. They are
            // arithmetic over selects (`2*sign(spd.x)` with `spd.x` the
            // no-direction dash's `flip.x and -1 or 1`), which the select
            // trees above do not reach, and their pieces taken field by
            // field multiplied into dashes no cart state makes (target
            // (2, 0) with accel (1.5, 1.5): 12 dash sets for 8 directions,
            // 2026-09-15). A case with no condition left and a field still
            // not a point is reported by the reader below.
            let mut todo = std::mem::take(&mut done);
            // The conditions split on, for the refusal below to name.
            let mut dash_conds: Vec<String> = Vec::new();
            while let Some(case) = todo.pop() {
                let mut memo = std::collections::HashMap::new();
                let dashing = crate::transpile::graph::pieces_of(&case.0, &case.2, &mut memo, case.1[2]).is_some_and(|p| p.iter().any(|q| q.1 > 0));
                let one_point = |n: NodeId, memo: &mut std::collections::HashMap<NodeId, Option<crate::transpile::graph::Pieces>>| {
                    crate::transpile::graph::pieces_of(&case.0, &case.2, memo, n).is_some_and(|p| p.len() == 1 && p[0].0 == p[0].1)
                };
                if !dashing || (3..7).all(|i| one_point(case.1[i], &mut memo)) {
                    done.push(case);
                    continue;
                }
                match next_value_cond(&case.0, &case.1[3..7]) {
                    None => done.push(case),
                    Some(c) => {
                        let nd = case.0.get(c);
                        dash_conds.push(format!("{:?}({})", nd.op, nd.args.iter().map(|a| format!("{:?}", case.0.get(*a).op)).collect::<Vec<_>>().join(", ")));
                        match decide(&case.0, &case.2, c) {
                            Some(b) => todo.push(split(&case, c, b)),
                            None => {
                                todo.push(split(&case, c, true));
                                todo.push(split(&case, c, false));
                            }
                        }
                    }
                }
                if done.len() + todo.len() > MAX_CASES_PER_REP {
                    let mut counts: std::collections::BTreeMap<&str, usize> = Default::default();
                    for s in &dash_conds {
                        *counts.entry(s.as_str()).or_default() += 1;
                    }
                    let mut top: Vec<(&str, usize)> = counts.into_iter().collect();
                    top.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
                    anyhow::bail!(
                        "outcome {shape:#x}: over {MAX_CASES_PER_REP} select cases for the dash constants in one button configuration; split on: {}",
                        top.iter().take(12).map(|(s, n)| format!("{s} x{n}")).collect::<Vec<_>>().join("; ")
                    );
                }
            }
            cases.extend(done);
        }
        if std::env::var_os("CELESTE_KEY_TRACE").is_some() {
            eprintln!("[keytrace] {shape:#x}: {} cases", cases.len());
        }
        for (probe, sig, seeds) in &cases {
            let mut memo = std::collections::HashMap::new();
            let mut pieces = |n: NodeId| crate::transpile::graph::pieces_of(probe, seeds, &mut memo, n);
            // The buckets the pieces touch, each with the hull of the
            // pieces inside it.
            let buckets = |axis: usize, ps: &[(i64, i64)]| -> Vec<(u16, (i64, i64))> {
                let e = celeste_core::spd_buckets::edges(w, axis);
                let mut bs = Vec::new();
                for j in 0..=e.len() {
                    let (lo, hi) = celeste_core::spd_buckets::range(j as u16, w, axis);
                    let (lo, hi) = (lo as i64, hi as i64);
                    let mut hull: Option<(i64, i64)> = None;
                    for p in ps {
                        if lo <= p.1 && hi >= p.0 {
                            let (a, b) = (p.0.max(lo), p.1.min(hi));
                            hull = Some(match hull {
                                Some((x, y)) => (x.min(a), y.max(b)),
                                None => (a, b),
                            });
                        }
                    }
                    if let Some(h) = hull {
                        bs.push((j as u16, h));
                    }
                }
                bs
            };
            let sx = pieces(sig[0]).ok_or_else(|| anyhow::anyhow!("outcome {shape:#x}: no static range for the output spd.x"))?;
            // On an x-only level `spd.y` is exact and not in the key: it has
            // no bucket bounds, so no static range is needed or asked for.
            let sy = if !buckets_y {
                Vec::new()
            } else {
                pieces(sig[1]).ok_or_else(|| anyhow::anyhow!("outcome {shape:#x}: no static range for the output spd.y"))?
            };
            let dts = pieces(sig[2]).ok_or_else(|| anyhow::anyhow!("outcome {shape:#x}: no static range for the output dash_time"))?;
            let points = |ps: &[(i64, i64)], what: &str| -> Result<Vec<i32>> {
                let mut v = Vec::new();
                for (lo, hi) in ps {
                    anyhow::ensure!(lo == hi, "outcome {shape:#x}: the output {what} is a range [{lo}, {hi}], not constants");
                    v.push(*lo as i32);
                }
                Ok(v)
            };
            let bxs = buckets(0, &sx);
            // On an x-only level y is exact: one key value, no range.
            let bys = if !buckets_y {
                vec![(0u16, (0i64, 0i64))]
            } else {
                buckets(1, &sy)
            };
            if std::env::var_os("CELESTE_KEY_TRACE").is_some() {
                let show = |ps: &[(i64, i64)]| ps.iter().map(|(a, b)| if a == b { format!("{:.3}", *a as f64 / 65536.0) } else { format!("[{:.3},{:.3}]", *a as f64 / 65536.0, *b as f64 / 65536.0) }).collect::<Vec<_>>().join("|");
                let mut fields = vec![show(&sx), show(&sy), show(&dts)];
                for i in 3..7 {
                    fields.push(pieces(sig[i]).map(|p| show(&p)).unwrap_or_else(|| "?".into()));
                }
                eprintln!("[keytrace] {shape:#x} case: spd ({}, {}) dash_time {} target ({}, {}) accel ({}, {})", fields[0], fields[1], fields[2], fields[3], fields[4], fields[5], fields[6]);
            }
            // The classes the output `dash_time` can take: a piece above 0
            // is mid-dash, a piece reaching 0 or below is not.
            let mut classes: Vec<bool> = Vec::new();
            if dts.iter().any(|p| p.1 > 0) {
                classes.push(true);
            }
            if dts.iter().any(|p| p.0 <= 0) {
                classes.push(false);
            }
            for dashing in classes {
                let dashes: Vec<[i32; 4]> = if dashing {
                    let mut vals: Vec<Vec<i32>> = Vec::new();
                    for (i, what) in [(3, "dash_target.x"), (4, "dash_target.y"), (5, "dash_accel.x"), (6, "dash_accel.y")] {
                        let ps = pieces(sig[i]).ok_or_else(|| anyhow::anyhow!("outcome {shape:#x}: no static range for the output {what}"))?;
                        vals.push(points(&ps, what)?);
                    }
                    let mut combos = Vec::new();
                    for &a in &vals[0] {
                        for &b in &vals[1] {
                            for &c in &vals[2] {
                                for &e in &vals[3] {
                                    combos.push([a, b, c, e]);
                                }
                            }
                        }
                    }
                    combos
                } else {
                    vec![[0; 4]]
                };
                for &(bx, rx) in &bxs {
                    for &(by, ry) in &bys {
                        for &dash in &dashes {
                            out.push((shape, Some(SpeedKey { bx, by, dashing, dash }), [rx, ry]));
                        }
                    }
                }
            }
        }
        let _ = Op::Cell;
    }
    // One entry per (shape, key): the hull of its ranges.
    out.sort();
    let mut merged: Vec<Successor> = Vec::new();
    for s in out {
        match merged.last_mut() {
            Some(last) if (last.0, last.1) == (s.0, s.1) => {
                for ax in 0..2 {
                    last.2[ax] = (last.2[ax].0.min(s.2[ax].0), last.2[ax].1.max(s.2[ax].1));
                }
            }
            _ => merged.push(s),
        }
    }
    Ok(merged)
}

/// A key frame's bounds by ENGINE cell: what the lowering's decide step
/// and fragment pruning read.
fn engine_ranges(
    f: &super::verify::Frame,
    bounds: &[(super::iface::Path, (i32, i32))],
) -> std::collections::HashMap<u32, (i32, i32)> {
    bounds
        .iter()
        .filter_map(|(p, r)| f.iface.slots.iter().position(|q| q == p).map(|i| (f.in_cells[i], *r)))
        .collect()
}

/// One node of the key fixpoint: a shape, its speed key (`None` for a
/// shape without a player), its key's whole buckets (raw, per axis), its
/// trace under them, and the trace bound out of the worker's arena it was
/// traced in (`emit::bind`: self-contained, lowered from here).
pub struct KeyNode {
    pub shape: u64,
    pub key: Option<SpeedKey>,
    pub range: [(i64, i64); 2],
    pub frame: super::verify::Frame,
    pub bounds: Vec<(super::iface::Path, (i32, i32))>,
    pub bound: Bound,
}

/// A node as one line of a `--key-census` node set: `shape bx by dashing
/// target.x target.y accel.x accel.y xlo xhi ylo yhi` (raw), or `shape none`.
fn node_line(n: &KeyNode) -> String {
    match n.key {
        None => format!("{:#x} none", n.shape),
        Some(k) => format!(
            "{:#x} {} {} {} {} {} {} {} {} {} {} {}",
            n.shape, k.bx, k.by, k.dashing as u8, k.dash[0], k.dash[1], k.dash[2], k.dash[3], n.range[0].0, n.range[0].1, n.range[1].0, n.range[1].1
        ),
    }
}

fn parse_node_line(line: &str) -> Result<(u64, Option<SpeedKey>, [(i64, i64); 2])> {
    let f: Vec<&str> = line.split_whitespace().collect();
    let hex = f.first().and_then(|s| s.strip_prefix("0x")).ok_or_else(|| anyhow::anyhow!("bad node line {line:?}"))?;
    let shape = u64::from_str_radix(hex, 16)?;
    if f.get(1) == Some(&"none") {
        return Ok((shape, None, [(0, 0), (0, 0)]));
    }
    anyhow::ensure!(f.len() == 12, "bad node line {line:?}");
    let n = |i: usize| -> Result<i64> { Ok(f[i].parse::<i64>()?) };
    let key = SpeedKey { bx: n(1)? as u16, by: n(2)? as u16, dashing: n(3)? != 0, dash: [n(4)? as i32, n(5)? as i32, n(6)? as i32, n(7)? as i32] };
    Ok((shape, Some(key), [(n(8)?, n(9)?), (n(10)?, n(11)?)]))
}

/// The key fixpoint's output, for judging it: per shape and class the
/// node count and how many ranges are single speeds; the dash constant
/// sets; per axis the buckets reached; and the (x, y) bucket grid.
fn census_report(nodes: &[KeyNode], w: u8) -> String {
    use std::collections::BTreeMap;
    use std::fmt::Write as _;
    let px = |v: i64| v as f64 / 65536.0;
    let bucket = |i: u16, axis: usize| -> String {
        let (lo, hi) = celeste_core::spd_buckets::range(i, w, axis);
        let end = |v: i32| if v == i32::MIN { "-inf".to_string() } else if v == i32::MAX { "inf".to_string() } else { format!("{:.4}", px(v as i64)) };
        format!("[{}, {}]", end(lo), end(hi))
    };
    let mut out = String::new();
    let mut per_shape: BTreeMap<u64, [usize; 3]> = BTreeMap::new();
    let mut per_class: BTreeMap<bool, [usize; 3]> = BTreeMap::new();
    let mut dashes: BTreeMap<[i32; 4], usize> = BTreeMap::new();
    let mut axes: [BTreeMap<u16, ([usize; 2], (i64, i64))>; 2] = [BTreeMap::new(), BTreeMap::new()];
    let mut grid: BTreeMap<(u16, u16), [bool; 2]> = BTreeMap::new();
    for n in nodes {
        let Some(k) = n.key else {
            per_shape.entry(n.shape).or_default()[2] += 1;
            continue;
        };
        per_shape.entry(n.shape).or_default()[k.dashing as usize] += 1;
        let c = per_class.entry(k.dashing).or_default();
        c[0] += 1;
        c[1] += (n.range[0].0 == n.range[0].1) as usize;
        c[2] += (n.range[1].0 == n.range[1].1) as usize;
        if k.dashing {
            *dashes.entry(k.dash).or_default() += 1;
        }
        for (axis, b) in [(0, k.bx), (1, k.by)] {
            let e = axes[axis].entry(b).or_insert(([0, 0], n.range[axis]));
            e.0[k.dashing as usize] += 1;
            e.1 = (e.1 .0.min(n.range[axis].0), e.1 .1.max(n.range[axis].1));
        }
        grid.entry((k.bx, k.by)).or_default()[k.dashing as usize] = true;
    }
    for (s, [nd, d, none]) in &per_shape {
        let _ = writeln!(out, "shape {s:#x}: {nd} not dashing, {d} dashing, {none} without a player");
    }
    for (dashing, [n, px_, py_]) in &per_class {
        let _ = writeln!(out, "dashing {dashing}: {n} nodes; single-speed ranges: x {px_}, y {py_}");
    }
    for (d, n) in &dashes {
        let _ = writeln!(out, "dash target ({:.4}, {:.4}) accel ({:.4}, {:.4}): {n} nodes", px(d[0] as i64), px(d[1] as i64), px(d[2] as i64), px(d[3] as i64));
    }
    for (axis, name) in [(0, "x"), (1, "y")] {
        for (b, ([nd, d], hull)) in &axes[axis] {
            let _ = writeln!(out, "{name} bucket {b:>2} {:<22} {nd:>3} not dashing {d:>3} dashing; ranges within [{:.4}, {:.4}]", bucket(*b, axis), px(hull.0), px(hull.1));
        }
    }
    // The grid: columns x buckets, rows y buckets; `o` not dashing, `d`
    // dashing, `*` both.
    let xs: Vec<u16> = axes[0].keys().copied().collect();
    let ys: Vec<u16> = axes[1].keys().copied().collect();
    let _ = writeln!(out, "grid (columns: x buckets {}, rows: y buckets):", xs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(" "));
    for y in &ys {
        let row: String = xs
            .iter()
            .map(|x| match grid.get(&(*x, *y)) {
                None => '.',
                Some([true, true]) => '*',
                Some([true, false]) => 'o',
                Some(_) => 'd',
            })
            .collect();
        let _ = writeln!(out, "  y {y:>2} {:<22} {row}", bucket(*y, 1));
    }
    out
}

/// What a lowered kernel's bodies ARE, for finding why there are many:
/// root slots against distinct root nodes and constants; per outcome the
/// button reps, bodies per rep, bodies whose `ok` folded false, the forks
/// whose fragments separate bodies (and the most fragments within one
/// rep), which roots differ between the bodies of one rep, and the
/// configurations of the largest rep.
fn body_breakdown(
    f: &super::verify::Frame,
    bound: &Bound,
    spec: &(crate::transpile::graph::Graph, Vec<crate::transpile::lower::SpecializedBody>),
) -> String {
    use crate::transpile::graph::Op;
    use std::collections::{BTreeMap, BTreeSet};
    use std::fmt::Write as _;
    let (sp, bodies) = spec;
    let mut out = String::new();
    let slots: usize = bodies.iter().map(|b| b.3.len()).sum();
    let distinct: BTreeSet<u32> = bodies.iter().flat_map(|b| b.3.iter().copied()).collect();
    let konst = |r: u32| matches!(sp.get(r).op, Op::Const(..) | Op::ConstBool(_));
    let const_slots = bodies.iter().flat_map(|b| b.3.iter()).filter(|r| konst(**r)).count();
    let _ = writeln!(
        out,
        "  {} bodies, {} fused nodes; {slots} root slots, {} distinct root nodes, {const_slots} slots hold constants",
        bodies.len(),
        sp.len(),
        distinct.len()
    );
    let named = f.outs.len() == bound.outcomes.len();
    for (oi, o) in bound.outcomes.iter().enumerate() {
        let mine: Vec<&crate::transpile::lower::SpecializedBody> = bodies.iter().filter(|b| b.0 == oi).collect();
        if mine.is_empty() {
            let _ = writeln!(out, "  outcome {oi}: no bodies");
            continue;
        }
        let nf = o.outputs.len();
        let names: Vec<String> = if named && f.outs[oi].fields.len() == nf {
            f.outs[oi].fields.iter().map(|(p, _, _)| super::iface::show(p)).collect()
        } else {
            (0..nf).map(|i| format!("field #{i}")).collect()
        };
        let root_name = |j: usize| -> String {
            if j < nf {
                names[j].clone()
            } else if j == nf {
                "ok".into()
            } else if j == nf + 1 {
                "live".into()
            } else {
                format!("key of {}", names.get(o.keys[j - nf - 2].0).cloned().unwrap_or_default())
            }
        };
        let mut by_rep: BTreeMap<u8, Vec<&crate::transpile::lower::SpecializedBody>> = BTreeMap::new();
        for b in &mine {
            by_rep.entry(b.1).or_default().push(b);
        }
        let mut sizes: Vec<usize> = by_rep.values().map(|v| v.len()).collect();
        sizes.sort_unstable();
        let ok_false = mine.iter().filter(|b| matches!(sp.get(b.3[nf]).op, Op::ConstBool(false))).count();
        let _ = writeln!(
            out,
            "  outcome {oi}: {} bodies over {} button reps (per rep min {} median {} max {}); {ok_false} with ok folded false; {} roots per body",
            mine.len(),
            by_rep.len(),
            sizes[0],
            sizes[sizes.len() / 2],
            sizes[sizes.len() - 1],
            mine[0].3.len()
        );
        for d in 0..bound.forks {
            let vals: BTreeSet<u8> = mine.iter().map(|b| b.2[d as usize]).collect();
            if vals.len() <= 1 {
                continue;
            }
            let in_rep = by_rep.values().map(|v| v.iter().map(|b| b.2[d as usize]).collect::<BTreeSet<_>>().len()).max().unwrap_or(0);
            let table = bound.graph.fork_table(d);
            let kind = if table.is_empty() {
                format!("move, {} ways", bound.graph.fork_ways(d))
            } else {
                format!(
                    "table {}",
                    table.iter().map(|(a, b)| format!("[{:.4},{:.4}]", *a as f64 / 65536.0, *b as f64 / 65536.0)).collect::<Vec<_>>().join(" ")
                )
            };
            let _ = writeln!(out, "    fork {d} ({kind}): fragments {vals:?} used, at most {in_rep} within one rep");
        }
        let nroots = mine[0].3.len();
        let mut differ = vec![0usize; nroots];
        let mut groups = 0usize;
        for v in by_rep.values() {
            if v.len() < 2 {
                continue;
            }
            groups += 1;
            for (j, dj) in differ.iter_mut().enumerate() {
                if v.iter().any(|b| b.3[j] != v[0].3[j]) {
                    *dj += 1;
                }
            }
        }
        let mut ranked: Vec<(usize, usize)> = differ.iter().copied().enumerate().filter(|(_, c)| *c > 0).collect();
        ranked.sort_by_key(|(j, c)| (std::cmp::Reverse(*c), *j));
        let _ = writeln!(
            out,
            "    roots differing between the bodies of one rep ({} of {nroots}, over {groups} reps with 2+ bodies): {}",
            ranked.len(),
            ranked.iter().take(30).map(|(j, c)| format!("{} ({c})", root_name(*j))).collect::<Vec<_>>().join(", ")
        );
        if let Some((m, v)) = by_rep.iter().max_by_key(|(_, v)| v.len()) {
            let used: Vec<u8> = (0..bound.forks).filter(|d| v.iter().map(|b| b.2[*d as usize]).collect::<BTreeSet<_>>().len() > 1).collect();
            let cfgs: Vec<String> = v.iter().take(40).map(|b| format!("{:?}", used.iter().map(|d| b.2[*d as usize]).collect::<Vec<_>>())).collect();
            let _ = writeln!(out, "    largest rep {m}: {} bodies; configurations over forks {used:?}: {}", v.len(), cfgs.join(" "));
        }
    }
    out
}

/// `bdd::census` of a lowered kernel's fused graph over all its bodies' roots.
fn census_of(spec: &(crate::transpile::graph::Graph, Vec<crate::transpile::lower::SpecializedBody>)) -> String {
    let roots: Vec<u32> = spec.1.iter().flat_map(|b| b.3.iter().copied()).collect();
    crate::transpile::bdd::census(&spec.0, &roots)
}

/// `transpile --key-probe FILE SEL`: re-trace and lower the nodes of a
/// `--key-census` node set at line indices SEL, each with its
/// `body_breakdown`, after the exact-speed kernel of the same shape. The
/// dev loop for a kernel's size: two shape lattices (~1 s) and one trace
/// + lowering (~3 s) per node, not the fixpoint.
pub fn key_probe(root: &std::path::Path, dump: &std::path::Path, sel: &[usize], spd: crate::interpreter::abstraction::SpdPrecision) -> Result<String> {
    use std::fmt::Write as _;
    let w = spd.width_log2().ok_or_else(|| anyhow::anyhow!("the key probe needs a bucketed speed precision, not {spd:?}"))?;
    let opts = super::shapes::WalkOpts::level0(spd, crate::interpreter::abstraction::PosPrecision::EXACT);
    let text = std::fs::read_to_string(dump).map_err(|e| anyhow::anyhow!("{}: {e}", dump.display()))?;
    let lines: Vec<&str> = text.lines().collect();
    let lw = room_constant_lattice(root, opts)?;
    let mut tr = lw.tracer.clone();
    let lwx = room_constant_lattice(root, super::shapes::WalkOpts::LEVEL0)?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    let mut exact_done: std::collections::BTreeSet<u64> = Default::default();
    let mut out = String::new();
    for &i in sel {
        let line = lines.get(i).ok_or_else(|| anyhow::anyhow!("node set has {} lines, no line {i}", lines.len()))?;
        let (shape, key, range) = parse_node_line(line)?;
        if exact_done.insert(shape) {
            let skey = lwx.by_hash.get(&shape).ok_or_else(|| anyhow::anyhow!("the exact-speed lattice has no shape {shape:#x}"))?;
            let wf = &lwx.frames[&(skey.clone(), None)];
            let f = &wf.frame;
            let bound = wf.bound.as_ref().map_err(|e| anyhow::anyhow!("the walk's frame of shape {shape:#x} did not bind: {e}"))?;
            let t = std::time::Instant::now();
            let lowered = super::emit::lower_frame(bound, Some(room.clone()), Default::default())?;
            writeln!(out, "=== exact-speed kernel of shape {shape:#x}: lower {:.2} s", t.elapsed().as_secs_f64())?;
            out.push_str(&body_breakdown(f, bound, &lowered.spec));
            out.push_str(&census_of(&lowered.spec));
        }
        writeln!(out, "=== node {i}: {key:?}, range x [{:.4}, {:.4}] y [{:.4}, {:.4}]", range[0].0 as f64 / 65536.0, range[0].1 as f64 / 65536.0, range[1].0 as f64 / 65536.0, range[1].1 as f64 / 65536.0)?;
        let t = std::time::Instant::now();
        let (f, bounds) = key_frame(&mut tr, &lw.view(), shape, key, w, range)?;
        let t_trace = t.elapsed();
        let bound = super::emit::bind(&f, &tr.it.d.graph, opts.widen)?;
        let t = std::time::Instant::now();
        let lowered = super::emit::lower_frame(&bound, Some(room.clone()), engine_ranges(&f, &bounds))?;
        writeln!(out, "  trace {:.2} s, lower {:.2} s", t_trace.as_secs_f64(), t.elapsed().as_secs_f64())?;
        out.push_str(&body_breakdown(&f, &bound, &lowered.spec));
        out.push_str(&census_of(&lowered.spec));
    }
    Ok(out)
}

/// `transpile --key-build`: build the start room's level-0 bucketed kernel
/// set exactly as the search does (`Registry::build_for_start_room`), with
/// nothing around it - the benchmark for the build. The phase accounting
/// prints from the build; this returns the set's size and wall time.
pub fn key_build(root: &std::path::Path) -> Result<String> {
    let opts = super::shapes::WalkOpts::level0(
        crate::interpreter::abstraction::SpdPrecision::WidthLog2(16),
        crate::interpreter::abstraction::PosPrecision::EXACT,
    );
    let t = std::time::Instant::now();
    let reg = crate::compiled::asm_kernel::Registry::build_for_start_room(root, opts, false)?;
    Ok(format!("level-0 bucketed set: {} kernels in {:.1} s\n", reg.kernel_count(), t.elapsed().as_secs_f64()))
}

/// `transpile --shape-diff A B [S]`: where two of the start room's shapes
/// differ, by shape hash (as the dispatch and `--key-census` name them),
/// under the walk at speed precision `S`: the tokens of their structures
/// between the common prefix and the common suffix. Built to name what
/// separates the shape a runtime row reached from the shape the key
/// fixpoint predicted for it (room (2,0) f37, 2026-09-16).
pub fn shape_diff(root: &std::path::Path, spd: crate::interpreter::abstraction::SpdPrecision, a: u64, b: u64) -> Result<String> {
    use std::fmt::Write as _;
    const SHOW: usize = 80;
    let opts = super::shapes::WalkOpts::level0(spd, crate::interpreter::abstraction::PosPrecision::EXACT);
    let lw = room_constant_lattice(root, opts)?;
    let key = |h: u64| -> Result<&String> {
        lw.by_hash.get(&h).ok_or_else(|| {
            let mut known: Vec<String> = lw.by_hash.keys().map(|k| format!("{k:#018x}")).collect();
            known.sort();
            anyhow::anyhow!("no shape {h:#018x} in the walk; shapes: {}", known.join(" "))
        })
    };
    let d = shape_key_diff(key(a)?, key(b)?, SHOW);
    let mut out = String::new();
    writeln!(out, "{a:#018x}: {} tokens; {b:#018x}: {} tokens; {} in common before the difference, {} after", d.a_tokens, d.b_tokens, d.prefix, d.suffix)?;
    writeln!(out, "before: {}", d.before)?;
    writeln!(out, "{a:#018x} ({} tokens): {}", d.a_len, d.a)?;
    writeln!(out, "{b:#018x} ({} tokens): {}", d.b_len, d.b)?;
    writeln!(out, "after: {}", d.after)?;
    Ok(out)
}

/// Where two shape keys (`State::shape`'s Debug text) differ, tokenized at
/// punctuation: the tokens between their common prefix and common suffix
/// (at most `show` each side) and some context around them.
struct ShapeKeyDiff {
    a_tokens: usize,
    b_tokens: usize,
    prefix: usize,
    suffix: usize,
    before: String,
    a: String,
    a_len: usize,
    b: String,
    b_len: usize,
    after: String,
}

fn shape_key_diff(a: &str, b: &str, show: usize) -> ShapeKeyDiff {
    let tokens = |s: &str| -> Vec<String> {
        let mut out = Vec::new();
        let mut cur = String::new();
        for ch in s.chars() {
            cur.push(ch);
            if matches!(ch, ',' | '{' | '}' | '[' | ']' | '(' | ')') {
                out.push(std::mem::take(&mut cur).trim().to_string());
            }
        }
        if !cur.trim().is_empty() {
            out.push(cur.trim().to_string());
        }
        out
    };
    let (ta, tb) = (tokens(a), tokens(b));
    let pre = ta.iter().zip(&tb).take_while(|(x, y)| x == y).count();
    let suf = ta[pre..].iter().rev().zip(tb[pre..].iter().rev()).take_while(|(x, y)| x == y).count();
    let (da, db) = (&ta[pre..ta.len() - suf], &tb[pre..tb.len() - suf]);
    let join = |t: &[String]| t.iter().take(show).cloned().collect::<Vec<_>>().join(" ");
    ShapeKeyDiff {
        a_tokens: ta.len(),
        b_tokens: tb.len(),
        prefix: pre,
        suffix: suf,
        before: ta[pre.saturating_sub(show / 4)..pre].join(" "),
        a: join(da),
        a_len: da.len(),
        b: join(db),
        b_len: db.len(),
        after: ta[ta.len() - suf..].iter().take(show / 4).cloned().collect::<Vec<_>>().join(" "),
    }
}

/// `transpile --key-census [N [FILE]]`: the key fixpoint of the start
/// room's level-0 bucketed set on its own - the census and timings, the
/// node set written to FILE (the input of `--key-probe`), and (with `N`)
/// the lowering of the first N nodes as a microbenchmark.
pub fn key_census(root: &std::path::Path, lower: usize, dump: Option<&std::path::Path>, spd: crate::interpreter::abstraction::SpdPrecision) -> Result<String> {
    use std::fmt::Write as _;
    let w = spd.width_log2().ok_or_else(|| anyhow::anyhow!("the key census needs a bucketed speed precision, not {spd:?}"))?;
    let opts = super::shapes::WalkOpts::level0(spd, crate::interpreter::abstraction::PosPrecision::EXACT);
    let t = std::time::Instant::now();
    let lw = room_constant_lattice(root, opts)?;
    let t_lattice = t.elapsed();
    let t = std::time::Instant::now();
    let nodes = key_fixpoint(&lw, w)?;
    let t_fix = t.elapsed();
    let mut out = String::new();
    writeln!(out, "shape lattice {:.1} s ({} shapes); key fixpoint {:.1} s ({} nodes)", t_lattice.as_secs_f64(), lw.by_hash.len(), t_fix.as_secs_f64(), nodes.len())?;
    out.push_str(&census_report(&nodes, w));
    if let Some(path) = dump {
        let text: String = nodes.iter().map(|n| node_line(n) + "\n").collect();
        std::fs::write(path, text)?;
        writeln!(out, "node set: {} ({} lines; `--key-probe` indexes them from 0)", path.display(), nodes.len())?;
    }
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    for n in nodes.iter().take(lower) {
        let (key, f, bound) = (&n.key, &n.frame, &n.bound);
        let t = std::time::Instant::now();
        let lowered = super::emit::lower_frame(bound, Some(room.clone()), engine_ranges(f, &n.bounds))?;
        let t_lower = t.elapsed();
        let t = std::time::Instant::now();
        let (fused, _, roots, reprs) = super::emit::asm_fused_from(bound, &lowered.spec)?;
        let n = roots.len();
        let (compiled, _loaded) = crate::transpile::asm::compile_and_load_reprs(&fused, &roots, &format!("kc{}", t_lower.as_nanos()), &reprs)?;
        let t_asm = t.elapsed();
        writeln!(
            out,
            "key {key:?}: lower {:.2} s ({} bodies, {} fused nodes), assemble {:.2} s ({} roots, {} slots)",
            t_lower.as_secs_f64(), lowered.bodies, lowered.spec.0.len(), t_asm.as_secs_f64(), n, compiled.n_roots
        )?;
    }
    Ok(out)
}

/// THE KEY FIXPOINT: every (shape, speed key) a run at grid width `2^w`
/// can reach, from the start state, each traced once under its key; the
/// successors of a key are read off its trace (`successor_keys`), and a
/// key not in the result is a coverage gap at runtime. The shape lattice
/// (`room_constant_lattice`) supplies the shapes, their representative
/// states and their constant pins.
pub fn key_fixpoint(lw: &LatticeWalk, w: u8) -> Result<Vec<KeyNode>> {
    anyhow::ensure!(region_grid().is_none(), "the speed-key fixpoint is not built with the region key (CELESTE_REGION)");
    let t = std::time::Instant::now();
    let (mut t_trace, mut t_succ) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
    let start_shape = lw.frames[&(lw.start_key.clone(), None)].frame.in_rt2.shape_hash_of();
    let buckets_y = lw.opts.spd.buckets_y();
    let (start_key, start_range) = concrete_key(&lw.reps[&lw.start_key], &lw.tracer.it.d, w, buckets_y)?;
    // A key's whole buckets, per bucketed axis; an unbucketed y keeps what
    // it was given (exact speeds are not bounded by the key).
    let whole = |key: Option<SpeedKey>, given: [(i64, i64); 2]| -> [(i64, i64); 2] {
        match key {
            Some(k) => {
                let (xlo, xhi) = celeste_core::spd_buckets::range(k.bx, w, 0);
                let y = if buckets_y {
                    let (ylo, yhi) = celeste_core::spd_buckets::range(k.by, w, 1);
                    (ylo as i64, yhi as i64)
                } else {
                    given[1]
                };
                [(xlo as i64, xhi as i64), y]
            }
            None => given,
        }
    };
    let start_range = whole(start_key, start_range);
    type Node = (u64, Option<SpeedKey>);
    // Per node: its key's whole buckets. A node is traced once, when it is
    // first reached: in the round after the one that reached it.
    let mut ranges: std::collections::HashMap<Node, [(i64, i64); 2]> = Default::default();
    ranges.insert((start_shape, start_key), start_range);
    let mut frontier: Vec<Node> = vec![(start_shape, start_key)];
    let mut out: Vec<KeyNode> = Vec::new();
    // THE ROUNDS IN PARALLEL (2026-09-16): each worker traces in its own copy
    // of the walk's tracer (copied once), reads its successors and binds the
    // trace out of that arena. Serially, room (2,0)'s 430 keys took 107 s.
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut tracers: Vec<Tracer> = (0..n_workers).map(|_| lw.tracer.clone()).collect();
    let view = lw.view();
    let mut round = 0usize;
    while !frontier.is_empty() {
        round += 1;
        let t_round = std::time::Instant::now();
        let next = std::sync::atomic::AtomicUsize::new(0);
        type Traced = (KeyNode, Vec<Successor>, std::time::Duration, std::time::Duration);
        let (frontier_ref, ranges_ref, view_ref) = (&frontier, &ranges, &view);
        let results: Vec<Result<Vec<Traced>>> = std::thread::scope(|scope| {
            let handles: Vec<_> = tracers
                .iter_mut()
                .map(|tr| {
                    let next = &next;
                    std::thread::Builder::new()
                        .stack_size(128 * 1024 * 1024)
                        .spawn_scoped(scope, move || -> Result<Vec<Traced>> {
                            let mut done = Vec::new();
                            loop {
                                let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some(&(shape, key)) = frontier_ref.get(i) else { break };
                                let range = ranges_ref[&(shape, key)];
                                let t1 = std::time::Instant::now();
                                let (f, bounds) = key_frame(tr, view_ref, shape, key, w, range)
                                    .map_err(|e| anyhow::anyhow!("key fixpoint: shape {shape:#x} key {key:?}: {e:#}"))?;
                                let dt_trace = t1.elapsed();
                                // Per trace, what it cost: a blow-up names the
                                // keys whose traces grow before one refuses.
                                if std::env::var_os("CELESTE_KEY_TRACE").is_some() {
                                    use super::domain::Domain;
                                    eprintln!(
                                        "[keytrace] round {round}: shape {shape:#x} key {key:?} range {range:?}: {} nodes added, {:.0} ms",
                                        tr.it.d.node_count().saturating_sub(tr.it.trace_start_nodes),
                                        dt_trace.as_secs_f64() * 1000.0
                                    );
                                }
                                let t1 = std::time::Instant::now();
                                let succs = successor_keys(&mut tr.it.d, &f, w, buckets_y)?;
                                let bound = super::emit::bind(&f, &tr.it.d.graph, view_ref.opts.widen)
                                    .map_err(|e| anyhow::anyhow!("key fixpoint: shape {shape:#x} key {key:?} bind: {e:#}"))?;
                                done.push((KeyNode { shape, key, range, frame: f, bounds, bound }, succs, dt_trace, t1.elapsed()));
                            }
                            Ok(done)
                        })
                        .expect("spawn key fixpoint worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("key fixpoint worker panicked")).collect()
        });
        let traced_now = frontier.len();
        let mut new_frontier: Vec<Node> = Vec::new();
        for r in results {
            for (node, succs, dt_trace, dt_succ) in r? {
                t_trace += dt_trace;
                t_succ += dt_succ;
                for (s_shape, s_key, s_range) in succs {
                    let known = lw.by_hash.contains_key(&s_shape);
                    // The edge, from the coordinator so the lines of different
                    // workers do not interleave: which node a key was read
                    // from (room (2,0) f37: a runtime row with a key no node
                    // produced, 2026-09-16).
                    if std::env::var_os("CELESTE_KEY_TRACE").is_some() {
                        eprintln!("[keysucc] {:#x} {:?} -> {s_shape:#x} {s_key:?}{}", node.shape, node.key, if known { "" } else { " (not in the walk)" });
                    }
                    // An outcome shape the shape lattice has no frame for is
                    // one it excluded on purpose (left the room, or `ok` folds
                    // false): there is no kernel for it in any set.
                    if !known {
                        continue;
                    }
                    let succ = (s_shape, s_key);
                    if let std::collections::hash_map::Entry::Vacant(e) = ranges.entry(succ) {
                        e.insert(whole(s_key, s_range));
                        new_frontier.push(succ);
                    }
                }
                out.push(node);
            }
        }
        eprintln!(
            "[asm build] key fixpoint round {round}: {traced_now} traced, {} new, {} nodes, {:.1} s ({n_workers} workers)",
            new_frontier.len(),
            ranges.len(),
            t_round.elapsed().as_secs_f64()
        );
        frontier = new_frontier;
    }
    // The census of the node set: per `dash_time`, how many nodes and how
    // many distinct bucket pairs (so a blow-up names its axis).
    {
        let mut per_dt: std::collections::BTreeMap<bool, (usize, std::collections::BTreeSet<(u16, u16)>, std::collections::BTreeSet<[i32; 4]>)> = Default::default();
        for n in &out {
            if let Some(k) = n.key {
                let e = per_dt.entry(k.dashing).or_default();
                e.0 += 1;
                e.1.insert((k.bx, k.by));
                e.2.insert(k.dash);
            }
        }
        for (dt, (n, pairs, dashes)) in &per_dt {
            eprintln!("[asm build]   dashing {dt}: {n} nodes, {} bucket pairs, {} dash constant sets", pairs.len(), dashes.len());
        }
        eprintln!("[asm build]   edges: x {} y {}", celeste_core::spd_buckets::edges(w, 0).len(), celeste_core::spd_buckets::edges(w, 1).len());
    }
    out.sort_by_key(|n| (n.shape, n.key));
    crate::transpile::lower::build_add_duration(0, t_trace);
    crate::transpile::lower::build_add_duration(1, t_succ);
    eprintln!(
        "[asm build] key fixpoint: {} (shape, key) nodes in {round} rounds, {:.1} s wall (summed over workers: tracing {:.1} s, successors and bind {:.1} s), largest worker arena {} nodes",
        out.len(),
        t.elapsed().as_secs_f64(),
        t_trace.as_secs_f64(),
        t_succ.as_secs_f64(),
        tracers.iter().map(|tr| tr.it.d.graph.len()).max().unwrap_or(0)
    );
    Ok(out)
}

#[cfg(test)]
mod tests {

    /// The ASM cutover's compute gate (plans/asm-and-posgraph-execution.md
    /// B): pull out the FUSED graph (`emit::asm_fused` ->
    /// `lower::specialize_frame`, the same fused, fork-free graph the Rust
    /// kernel is emitted from) and assemble it, for EVERY start-room shape.
    ///
    /// Because the graph is fused - every fork resolved to `Frag`/const,
    /// hash-consed - no `Free`/`Split` survives, so all real shapes compile
    /// and load, not just the fork-free ones. This proves the whole pipeline
    /// (trace -> specialize -> assemble) end-to-end on real kernel graphs.
    /// One assembly kernel per shape.
    #[test]
    fn every_start_room_kernel_graph_asm_compiles_the_fused_graph() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        assert!(!refs.is_empty(), "no start-room kernels traced");
        let mut total_roots = 0usize;
        for (si, r) in refs.iter().enumerate() {
            let room = crate::transpile::graph::Room {
                cart: r.cart.clone(),
                cache: r.cache.clone(),
            };
            let (fused, bodies, roots, reprs) =
                crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
                    .unwrap_or_else(|e| panic!("shape {si} fused extraction failed: {e:#}"));
            assert!(!bodies.is_empty(), "shape {si}: no bodies");
            assert!(!roots.is_empty(), "shape {si}: no roots");
            let (compiled, _loaded) = crate::transpile::asm::compile_and_load_reprs(
                &fused,
                &roots,
                &format!("fused{si}"),
                &reprs,
            )
            .unwrap_or_else(|e| {
                let msg = format!("{e:#}");
                // Pull "node N" out and report its consumers, so a domain
                // mismatch names the op that mishandled it.
                let mut consumers = String::new();
                if let Some(rest) = msg.split("node ").nth(1) {
                    if let Ok(n) = rest
                        .split(|c: char| !c.is_ascii_digit())
                        .next()
                        .unwrap_or("")
                        .parse::<u32>()
                    {
                        for id in 0..fused.len() as u32 {
                            if fused.get(id).args.contains(&n) {
                                consumers.push_str(&format!(
                                    " {}={:?}",
                                    id,
                                    fused.get(id).op
                                ));
                            }
                        }
                    }
                }
                panic!(
                    "shape {si}: fused asm compile+load failed: {msg}\n  consumers:{consumers}"
                )
            });
            assert_eq!(compiled.n_roots, roots.len(), "shape {si}: root count");
            total_roots += roots.len();
        }
        eprintln!(
            "[asm] {} start-room shapes: fused graph compiled+loaded, {} roots total",
            refs.len(),
            total_roots
        );
    }

    /// A speed's bucket key is attached to the SPEED fields, by path. At
    /// rest both speeds are the constant 0 - one hash-consed node with
    /// every other field that is 0 - and matching the override by node
    /// gave the key to whichever field came first: the row key check
    /// failed and the synthetic gate lost exact marks (2026-09-15).
    #[test]
    fn a_speed_key_names_the_speed_fields() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let opts = crate::trace::shapes::WalkOpts::level0(
            crate::interpreter::abstraction::SpdPrecision::WidthLog2(16),
            crate::interpreter::abstraction::PosPrecision::EXACT,
        );
        let lw = super::room_constant_lattice(std::path::Path::new("."), opts).expect("lattice");
        let mut tr = lw.tracer.clone();
        let skey = lw.reps.iter().find(|(_, st)| crate::trace::shapes::player_path(st).is_some()).map(|(k, _)| k.clone()).expect("a shape with a player");
        let shape = lw.frames[&(skey.clone(), None)].frame.in_rt2.shape_hash_of();
        let rest = super::SpeedKey {
            bx: celeste_core::spd_buckets::index(0, 16, 0),
            by: celeste_core::spd_buckets::index(0, 16, 1),
            dashing: false,
            dash: [0; 4],
        };
        let (f, _) = super::key_frame(&mut tr, &lw.view(), shape, Some(rest), 16, [(0, 0), (0, 0)]).expect("rest key frame");
        let mut checked = 0;
        for o in &f.outs {
            let Some(pl) = crate::trace::shapes::player_path(&o.st) else { continue };
            let want = super::key_paths(&pl);
            let mut named: Vec<String> = o.keys.iter().map(|(i, _)| crate::trace::iface::show(&o.fields[*i].0)).collect();
            named.sort();
            let mut expect = vec![crate::trace::iface::show(&want[0]), crate::trace::iface::show(&want[1])];
            expect.sort();
            assert_eq!(named, expect, "the key overrides name the speed fields");
            checked += 1;
        }
        assert!(checked > 0, "no outcome with a player");
    }

    /// A player that is not dashing starts a dash in every direction or in
    /// none: the direction is the buttons alone. The successor reader once
    /// deduplicated button configurations by node ids across separately
    /// built graphs and read only the first diagonal (room (2,0) f37: a
    /// down-left dash start had no kernel, 2026-09-16).
    #[test]
    fn a_dash_start_is_read_in_every_direction() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let w = 20;
        let opts = crate::trace::shapes::WalkOpts::level0(
            crate::interpreter::abstraction::SpdPrecision::WidthLog2X(w),
            crate::interpreter::abstraction::PosPrecision::EXACT,
        );
        let lw = super::room_constant_lattice(std::path::Path::new("."), opts).expect("lattice");
        let mut tr = lw.tracer.clone();
        let rest = super::SpeedKey { bx: celeste_core::spd_buckets::index(0, w, 0), by: 0, dashing: false, dash: [0; 4] };
        let mut read = 0usize;
        for (&shape, skey) in &lw.by_hash {
            if crate::trace::shapes::player_path(&lw.reps[skey]).is_none() {
                continue;
            }
            let (f, _) = super::key_frame(&mut tr, &lw.view(), shape, Some(rest), w, [(0, 0), (0, 0)]).expect("rest key frame");
            let succs = super::successor_keys(&mut tr.it.d, &f, w, false).expect("successor keys");
            let dirs: std::collections::BTreeSet<(i32, i32)> = succs
                .iter()
                .filter_map(|(_, k, _)| k.as_ref().filter(|k| k.dashing).map(|k| (k.dash[0].signum(), k.dash[1].signum())))
                .collect();
            assert!(dirs.is_empty() || dirs.len() == 8, "shape {shape:#x}: dash starts read in directions {dirs:?}");
            read += usize::from(!dirs.is_empty());
        }
        assert!(read > 0, "no player shape read a dash start");
    }


}

// ---------------------------------------------------------------------------
// The bucket-dispatch probe (2026-09-15). NOT production: it re-traces one
// shape with the player's speed cells bounded to one threshold bucket per
// axis (`Symbolic::ranges`), the boundary snap forking at the bucket table
// (`widen::spd_table_node`), and reports what the specialization buys:
// nodes, forks and their arities, bodies, loop bodies traced, comparisons
// folded, merge premises left. Driven by `transpile --bucket-probe SHAPE`.
// ---------------------------------------------------------------------------

/// The buckets of an edge list within `[-lim, lim]` raw: `(lo, hi)` inclusive.
fn buckets_of(edges: &[i32], lim: i32) -> Vec<(i32, i32)> {
    edges
        .windows(2)
        .map(|w| (w[0], w[1] - 1))
        .filter(|(lo, hi)| *lo >= -lim && *hi <= lim)
        .collect()
}

/// One trace + lowering of the probe, and its statistics.
struct Stat {
    nodes: usize,
    /// Per fork: arity and kind (`g` grid, `t` table).
    forks: Vec<String>,
    bodies: usize,
    fused: usize,
    /// The assembled kernel: instruction lines and output slots.
    asm_lines: usize,
    slots: usize,
    iters: u64,
    folds: u64,
    premises: usize,
    err: Option<String>,
}

#[allow(clippy::too_many_arguments)]
fn probe_trace<'a>(
    it: &mut super::interp::Interp<'a, super::domain::Symbolic>,
    reset: &'a full_moon::ast::Ast,
    fr: &'a full_moon::ast::Ast,
    st: &super::state::State<super::domain::Symbolic>,
    roots: &[super::iface::Path],
    ival: &[super::iface::Path],
    opts: super::shapes::WalkOpts,
    room: &crate::transpile::graph::Room,
    pin: &[(super::iface::Path, super::iface::Conc)],
    bounds: &[(super::iface::Path, (i32, i32))],
) -> Stat {
    use crate::transpile::graph::Op;
    let it0 = it.for_iterations;
    let f = match super::verify::trace_frame(it, reset, fr, st.clone(), roots, pin, ival, opts.widen_mode(), bounds) {
        Ok(f) => f,
        Err(e) => {
            return Stat { nodes: 0, forks: vec![], bodies: 0, fused: 0, asm_lines: 0, slots: 0, iters: it.for_iterations - it0, folds: it.d.range_folds, premises: 0, err: Some(format!("{e:#}")) }
        }
    };
    // Nodes reachable from every outcome's roots, and the merge premises
    // (`Known` of a non-constant) among them.
    let g = &it.d.graph;
    let mut seen = vec![false; g.len()];
    let mut stack: Vec<u32> = Vec::new();
    for o in &f.outs {
        stack.extend(o.fields.iter().map(|(_, n, _)| *n));
        stack.push(o.ok);
        stack.push(o.guard);
        stack.extend(o.keys.iter().map(|(_, n)| *n));
    }
    // A merge premise (`Known(cond)`) that can FAIL at runtime is one
    // whose condition reads an interval: a comparison with an interval
    // operand, or a connective / select over one.
    fn reads_interval(d: &super::domain::Symbolic, n: u32, depth: usize) -> bool {
        use super::domain::Domain;
        let nd = d.graph.get(n);
        match nd.op {
            Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq => nd.args.iter().any(|a| d.is_interval(a)),
            Op::Not | Op::And | Op::Or | Op::Sel if depth > 0 => nd.args.iter().any(|a| reads_interval(d, *a, depth - 1)),
            _ => false,
        }
    }
    let (mut nodes, mut premises) = (0usize, 0usize);
    let mut known_args: Vec<u32> = Vec::new();
    while let Some(n) = stack.pop() {
        if seen[n as usize] {
            continue;
        }
        seen[n as usize] = true;
        nodes += 1;
        let nd = g.get(n);
        if matches!(nd.op, Op::Known) && !matches!(g.get(nd.args[0]).op, Op::ConstBool(_) | Op::Flr) {
            known_args.push(nd.args[0]);
        }
        stack.extend(nd.args.iter().copied());
    }
    for a in known_args {
        if reads_interval(&it.d, a, 6) {
            premises += 1;
        }
    }
    let g = &it.d.graph;
    let forks: Vec<String> = (0..f.forks)
        .map(|d| format!("{}{}", g.fork_ways(d), if g.fork_table(d).is_empty() { "g" } else { "t" }))
        .collect();
    // The bounds, by ENGINE cell, for the decide step's folding.
    let ranges: std::collections::HashMap<u32, (i32, i32)> = bounds
        .iter()
        .filter_map(|(p, r)| f.iface.slots.iter().position(|q| q == p).map(|i| (f.in_cells[i], *r)))
        .collect();
    // Lower and ASSEMBLE, for the kernel's size: the instruction count is
    // the per-slice cost the fused node count only approximates.
    static TAGS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let build = || -> Result<(usize, usize, usize, usize)> {
        let b = super::emit::bind(&f, g, opts.widen)?;
        let l = super::emit::lower_frame(&b, Some(room.clone()), ranges)?;
        let (fused, _bodies, roots, reprs) = super::emit::asm_fused_from(&b, &l.spec)?;
        let tag = format!("bp{}", TAGS.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
        let (compiled, _loaded) = crate::transpile::asm::compile_and_load_reprs(&fused, &roots, &tag, &reprs)?;
        let lines = compiled.asm.lines().filter(|l| l.starts_with('\t') || l.starts_with("    ")).count();
        Ok((l.bodies, l.spec.0.len(), lines, compiled.n_roots))
    };
    let (bodies, fused, asm_lines, slots, err) = match build() {
        Ok((b, n, a, s)) => (b, n, a, s, None),
        Err(e) => (0, 0, 0, 0, Some(format!("lower: {e:#}"))),
    };
    Stat { nodes, forks, bodies, fused, asm_lines, slots, iters: it.for_iterations - it0, folds: it.d.range_folds, premises, err }
}

pub fn bucket_probe(root: &std::path::Path, shape_idx: usize) -> Result<String> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::iface::{self, Step};
    use super::verify::run_one;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    use celeste_core::pico8_num::Pico8Num as P8;
    use std::fmt::Write as _;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;
    // The assembled text is what the size metric counts.
    std::env::set_var("CELESTE_KEEP_ASM", "1");

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };

    let st0 = cart::fresh_state::<Symbolic>(&mut it.d);
    let st0 = run_one(&mut it, &top, st0)?;
    let mut st0 = st0;
    cart::inject_tile_flag_at(&mut st0);
    let st0 = run_one(&mut it, &init, st0)?;
    // The hull's level-0 set: uniform 1 px speed buckets, the baseline.
    let opts = shapes::WalkOpts::level0(
        crate::interpreter::abstraction::SpdPrecision::WidthLog2(16),
        crate::interpreter::abstraction::PosPrecision::EXACT,
    );
    let w = shapes::walk(&mut it, &reset, &fr, st0, 400, opts)?;
    if shape_idx >= w.shapes.len() {
        bail!("shape {} out of range ({} shapes)", shape_idx, w.shapes.len());
    }
    let st = w.shapes[shape_idx].state.clone();
    let roots = shapes::state_paths(&st)?;
    let ival = shapes::ival_paths(&st, opts.spd_ival(), opts.pos_ival());
    let Some(pl) = shapes::player_path(&st) else { bail!("shape {shape_idx} has no player") };
    let fld = |f: &[&str]| -> Vec<Step> {
        let mut p = pl.clone();
        for x in f {
            p.push(iface::key(x));
        }
        p
    };
    let (px, py) = (fld(&["spd", "x"]), fld(&["spd", "y"]));
    // The player's rem is within [-0.5, 0.5) at every rung by the
    // boundary's construction, so its cells can be seeded too: that is
    // what bounds `flr(rem + spd + 0.5)`, the move loop's trip count.
    let rem: Vec<(Vec<Step>, (i32, i32))> = vec![(fld(&["rem", "x"]), (-0x8000, 0x7fff)), (fld(&["rem", "y"]), (-0x8000, 0x7fff))];
    // The dash's target and accel are written from constants (0, ±2,
    // ±1.5; 1.5, 1.06), so their cells are bounded too - the reachable
    // constant lattice would say the same.
    let dash: Vec<(Vec<Step>, (i32, i32))> = vec![
        (fld(&["dash_target", "x"]), (-2 << 16, 2 << 16)),
        (fld(&["dash_target", "y"]), (-2 << 16, 2 << 16)),
        (fld(&["dash_accel", "x"]), (0, 3 << 15)),
        (fld(&["dash_accel", "y"]), (0, 3 << 15)),
    ];
    // Two dispatch keys on top of the speed bucket: not mid-dash, and
    // with / without a dash available - the old pm1 key's shape.
    let pins: Vec<(&str, Vec<(Vec<Step>, super::iface::Conc)>)> = vec![
        ("dash_time=0 djump=0", vec![(fld(&["dash_time"]), super::iface::Conc::Num(P8::from_i16(0))), (fld(&["djump"]), super::iface::Conc::Num(P8::from_i16(0)))]),
        ("dash_time=0 djump=1", vec![(fld(&["dash_time"]), super::iface::Conc::Num(P8::from_i16(0))), (fld(&["djump"]), super::iface::Conc::Num(P8::from_i16(1)))]),
    ];

    let line = |s: &Stat| -> String {
        match &s.err {
            Some(e) => format!("ERROR {e}"),
            None => format!(
                "nodes {:>5} forks {:?} bodies {:>4} fused {:>6} asm {:>7} slots {:>5} loops {:>3} folds {:>3} premises {:>3}",
                s.nodes, s.forks, s.bodies, s.fused, s.asm_lines, s.slots, s.iters, s.folds, s.premises
            ),
        }
    };

    let mut rep = String::new();
    let base = probe_trace(&mut it, &reset, &fr, &st, &roots, &ival, opts, &room, &[], &[]);
    writeln!(rep, "shape {shape_idx} of {} ({} objects): baseline (1 px buckets, grid fork)", w.shapes.len(), roots.iter().filter(|p| p.len() == 3 && p[0] == iface::key("objects")).map(|p| p[1].clone()).collect::<std::collections::BTreeSet<_>>().len())?;
    writeln!(rep, "  {}", line(&base))?;

    let ex: Vec<i32> = celeste_core::spd_buckets::edges(16, 0).to_vec();
    let ey: Vec<i32> = celeste_core::spd_buckets::edges(16, 1).to_vec();
    let lim = 2 << 16;
    let xs = buckets_of(&ex, lim);
    let yonly = std::env::var_os("CELESTE_BUCKET_YONLY").is_some();
    let ys: Vec<(i32, i32)> = {
        let raw = |s: &str| -> i32 { s.parse::<celeste_core::pico8_num::Pico8Num>().unwrap().as_raw_u32() as i32 };
        let want = [0i32, 1, raw("1"), raw("-2")];
        let all = buckets_of(&ey, 8 << 16);
        let pick: Vec<(i32, i32)> = want.iter().filter_map(|w| all.iter().find(|(lo, _)| lo == w).copied()).collect();
        if yonly { pick[..1].to_vec() } else { pick }
    };
    let show = |(lo, hi): (i32, i32)| -> String {
        let f = |r: i32| r as f64 / 65536.0;
        if lo == hi { format!("{{{:.4}}}", f(lo)) } else { format!("[{:.4},{:.4}]", f(lo), f(hi)) }
    };
    writeln!(rep, "x buckets {} ({} edges), y buckets {} ({} edges); tracing {} pairs", xs.len(), ex.len(), ys.len(), ey.len(), xs.len() * ys.len())?;
    let (mut sum_bodies, mut sum_fused, mut max_ways) = (0usize, 0usize, 0u8);
    let base_max = base.forks.iter().map(|f| f.trim_end_matches(['g', 't']).parse::<u8>().unwrap_or(0)).max().unwrap_or(0);
    for (pname, pin) in &pins {
        writeln!(rep, "== {pname}")?;
        for &yb in &ys {
            writeln!(rep, "y {}:", show(yb))?;
            for &xb in &xs {
                let mut bounds = rem.clone();
                bounds.extend(dash.iter().cloned());
                bounds.push((px.clone(), xb));
                bounds.push((py.clone(), yb));
                let s = probe_trace(&mut it, &reset, &fr, &st, &roots, &ival, opts, &room, pin, &bounds);
                eprintln!("[probe] {pname} x {} y {}: {}", show(xb), show(yb), line(&s));
                writeln!(rep, "  x {:<18} {}", show(xb), line(&s))?;
                sum_bodies += s.bodies;
                sum_fused += s.fused;
                max_ways = max_ways.max(s.forks.iter().map(|f| f.trim_end_matches(['g', 't']).parse::<u8>().unwrap_or(0)).max().unwrap_or(0));
            }
        }
    }
    writeln!(rep, "totals over {} (pin, pair)s: bodies {} (baseline {}), fused nodes {} (baseline {}), max fork arity {} (baseline {})",
        pins.len() * xs.len() * ys.len(), sum_bodies, base.bodies, sum_fused, base.fused, max_ways, base_max)?;
    Ok(rep)
}

// ---------------------------------------------------------------------------
// Specialization probe (plans/specialize.md). NOT production: it re-traces one
// shape with the player position, the springs, and a pm1 key pinned, and
// reports how far the graph collapses. Driven by `transpile --spec-probe`.
// ---------------------------------------------------------------------------

/// Pin the player XY, the springs' XY, and a pm1 key on one shape, re-trace,
/// and report the node/fork/body collapse against the unpinned base.
pub fn specialize_probe(
    root: &std::path::Path,
    shape_idx: usize,
    player_xy: (i16, i16),
    spring_xy: &[(i16, i16)],
) -> Result<String> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::iface::{self, Conc, Step};
    use super::verify::{run_one, trace_frame};
    use super::heap::Value;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    use celeste_core::pico8_num::Pico8Num as P8;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st0 = cart::fresh_state::<Symbolic>(&mut it.d);
    let st0 = run_one(&mut it, &top, st0)?;
    let mut st0 = st0;
    cart::inject_tile_flag_at(&mut st0);
    let st0 = run_one(&mut it, &init, st0)?;
    let w = shapes::walk(&mut it, &reset, &fr, st0, 400, shapes::WalkOpts::LEVEL0)?;

    if shape_idx >= w.shapes.len() {
        bail!("shape {} out of range ({} shapes)", shape_idx, w.shapes.len());
    }
    let st = w.shapes[shape_idx].state.clone();
    let base_forks = w.shapes[shape_idx].frame.forks;

    // Locate the player and the spring objects by type.
    let objects_of = |st: &super::state::State<Symbolic>, name: &str| -> Vec<Vec<Step>> {
        let mut out = Vec::new();
        let Some(Value::Table(want)) = iface::get(st, &[iface::key(name)]) else { return out };
        let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return out };
        let n = st.heap.tables[&objects].arr.len();
        for i in 0..n {
            let base = vec![iface::key("objects"), Step::Idx(i)];
            let mut ty = base.clone();
            ty.push(iface::key("type"));
            if iface::get(st, &ty) == Some(Value::Table(want)) { out.push(base); }
        }
        out
    };
    // Object-identity check: for each spring, does its `collide`
    // closure's captured `obj` point to the same table as the live
    // spring? (CELESTE_SPEC_OBJID)
    if std::env::var("CELESTE_SPEC_OBJID").is_ok() {
        let springs_ck = objects_of(&st, "spring");
        for (i, sp) in springs_ck.iter().enumerate() {
            let live_tid = match iface::get(&st, sp) { Some(Value::Table(t)) => t, _ => { eprintln!("spring {} not a table", i); continue } };
            // the collide closure
            let mut cp = sp.clone(); cp.push(iface::key("collide"));
            let cl = match iface::get(&st, &cp) { Some(Value::Func(c)) => c, other => { eprintln!("spring {} collide = {:?}", i, other); continue } };
            let env = st.heap.closures.get(&cl).map(|c| c.env);
            let cap_obj = env.and_then(|e| st.heap.scopes.get(&e)).and_then(|sc| sc.vars.get("obj")).cloned();
            let cap_tid = match cap_obj { Some(Value::Table(t)) => Some(t), _ => None };
            eprintln!("spring {}: live table {}, collide.env captured obj = {:?} (match: {})",
                i, live_tid, cap_tid, cap_tid == Some(live_tid));
            // also print the captured obj's x vs live x
            if let Some(ct) = cap_tid {
                let cap_x = st.heap.tables.get(&ct).and_then(|t| t.hash.get("x")).cloned();
                let live_x = st.heap.tables.get(&live_tid).and_then(|t| t.hash.get("x")).cloned();
                eprintln!("    captured obj.x node = {:?}, live obj.x node = {:?}", cap_x.map(|v| format!("{:?}", v)), live_x.map(|v| format!("{:?}", v)));
            }
        }
    }
    let players = objects_of(&st, "player");
    let springs = objects_of(&st, "spring");
    let fld = |base: &[Step], f: &[&str]| -> Vec<Step> {
        let mut p = base.to_vec();
        for x in f { p.push(iface::key(x)); }
        p
    };
    let num = |v: i16| Conc::Num(P8::from_i16(v));

    let mut pin: Vec<(Vec<Step>, Conc)> = Vec::new();
    // The player position.
    let groups: std::collections::HashSet<String> = std::env::var("CELESTE_SPEC_GROUPS")
        .unwrap_or_default().split(',').map(|s| s.trim().to_string()).collect();
    // `motionfree` leaves the player's motion (x, y, spd, rem) symbolic
    // and pins everything else, so the position pin is off too.
    let nopos = std::env::var("CELESTE_SPEC_NOPOS").is_ok() || groups.contains("motionfree");
    if let Some(pl) = players.first() {
        if !nopos { pin.push((fld(pl, &["x"]), num(player_xy.0))); pin.push((fld(pl, &["y"]), num(player_xy.1))); }
        // pm1 key on the player, canonical "steady" values.
        pin.push((fld(pl, &["dash_time"]), num(0)));
        pin.push((fld(pl, &["p_dash"]), Conc::Bool(false)));
        pin.push((fld(pl, &["p_jump"]), Conc::Bool(false)));
    }
    // Optional extra pin groups, so the graph's fork sources can be
    // isolated without recompiling. CELESTE_SPEC_GROUPS is a comma list:
    //   spd        - pin the player spd.x/spd.y to 0
    //   springall  - pin every spring scalar (freeze the springs)
    //   playerall  - pin every player scalar except rem (the fork input)
    //   motionfree - pin every player scalar except x, y, spd and rem
    for (i, sp) in springs.iter().enumerate() {
        if let Some((x, y)) = spring_xy.get(i) {
            pin.push((fld(sp, &["x"]), num(*x)));
            pin.push((fld(sp, &["y"]), num(*y)));
        }
        // CELESTE_SPEC_SFIELDS: dotted spring field paths to pin, e.g.
        // "spr,hide_for,hide_in,delay". spr defaults to 18 (active).
        for spec in std::env::var("CELESTE_SPEC_SFIELDS").unwrap_or_default().split(',') {
            let spec = spec.trim();
            if spec.is_empty() { continue; }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(sp, &parts);
            let v = if spec == "spr" { num(18) } else { num(0) };
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(false))),
                    Some(Value::Num(_)) => pin.push((path, v)),
                    _ => {}
                }
            }
        }
        if groups.contains("springall") {
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                if f.starts_with(sp) && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    if let Some(pl) = players.first() {
        if groups.contains("spd") {
            pin.push((fld(pl, &["spd", "x"]), num(0)));
            pin.push((fld(pl, &["spd", "y"]), num(0)));
        }
        // CELESTE_SPEC_PFIELDS: dotted player field paths to pin to 0,
        // e.g. "spd.x,spd.y,djump,grace,dash_effect_time".
        for spec in std::env::var("CELESTE_SPEC_PFIELDS").unwrap_or_default().split(',') {
            let mut spec = spec.trim();
            if spec.is_empty() { continue; }
            // trailing "=1" pins a bool to true.
            let want_true = spec.ends_with("=1");
            if want_true { spec = &spec[..spec.len()-2]; }
            let mut numval = 0i16;
            if let Some((f, v)) = spec.split_once(':') { spec = f; numval = v.parse().unwrap_or(0); }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(pl, &parts);
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(want_true))),
                    Some(Value::Num(_)) => pin.push((path, num(numval))),
                    _ => {}
                }
            }
        }
        if groups.contains("playerall") || groups.contains("motionfree") {
            let motion = groups.contains("motionfree");
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                let shown = iface::show(&f);
                let is_rem = f.starts_with(pl) && f.iter().any(|s| format!("{:?}", s).contains("rem"));
                let is_motion = motion
                    && f.starts_with(pl)
                    && (shown.ends_with(".x") || shown.ends_with(".y"))
                    && (f.len() == pl.len() + 1 || shown.contains("spd"));
                if f.starts_with(pl) && !is_rem && !is_motion && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    // CELESTE_SPEC_ALLGEOM: pin EVERY object's x,y and hitbox to
    // constants (spread positions, default hitbox) - tests whether
    // baking in the static object geometry folds the collision graph.
    if std::env::var("CELESTE_SPEC_ALLGEOM").is_ok() {
        let all: Vec<Vec<Step>> = {
            let mut v = Vec::new();
            if let Some(Value::Table(objs)) = iface::get(&st, &[iface::key("objects")]) {
                let n = st.heap.tables[&objs].arr.len();
                for i in 0..n { v.push(vec![iface::key("objects"), Step::Idx(i)]); }
            }
            v
        };
        let player_bases: Vec<Vec<Step>> = players.clone();
        for (i, ob) in all.iter().enumerate() {
            if player_bases.iter().any(|pb| pb == ob) { continue; }
            for (f, v) in [("x", (i as i16) * 12 + 4), ("y", 40)] {
                let path = fld(ob, &[f]);
                if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r==&path) && !pin.iter().any(|(q,_)| q==&path) {
                    pin.push((path, num(v)));
                }
            }
            // hitboxes are pinned by CELESTE_SPEC_HITBOX, not here.
        }
    }
    // CELESTE_SPEC_HITBOX: pin player hitbox (1,3,6,5) and spring
    // hitbox (0,0,8,8) - the type-fixed values - to test whether the
    // symbolic hitbox is what blocks collide from folding.
    if std::env::var("CELESTE_SPEC_HITBOX").is_ok() {
        if let Some(pl) = players.first() {
            for (f, v) in [("x", 1), ("y", 3), ("w", 6), ("h", 5)] {
                pin.push((fld(pl, &["hitbox", f]), num(v)));
            }
        }
        for sp in &springs {
            for (f, v) in [("x", 0), ("y", 0), ("w", 8), ("h", 8)] {
                pin.push((fld(sp, &["hitbox", f]), num(v)));
            }
        }
    }
    // pm1 globals.
    pin.push((vec![iface::key("freeze")], num(0)));
    pin.push((vec![iface::key("has_dashed")], Conc::Bool(false)));

    // Only keep pins whose path is actually a scalar in this state.
    let roots = shapes::state_paths(&st)?;
    let ival = shapes::ival_paths(&st, false, (false, false));
    pin.retain(|(p, _)| roots.iter().any(|r| r == p));

    let pinned_paths: Vec<String> = pin.iter().map(|(p, _)| iface::show(p)).collect();

    // Measure one traced frame: reachable node count, distinct LIVE
    // forks (Op::Split still referenced), and an op census.
    fn measure(g: &crate::transpile::graph::Graph, f: &super::verify::Frame) -> (usize, std::collections::BTreeSet<u8>, Vec<String>) {
        use crate::transpile::graph::Op;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs {
            for (_, nd, _) in &o.fields { roots_n.push(*nd); }
            roots_n.push(o.guard);
            roots_n.push(o.ok);
        }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let nodes = reach.iter().filter(|b| **b).count();
        let mut forks = std::collections::BTreeSet::new();
        let mut census: std::collections::BTreeMap<String, usize> = Default::default();
        for id in 0..g.len() {
            if reach[id] {
                let op = &g.get(id as u32).op;
                if let Op::Split(d) | Op::SplitValid(d) = op { forks.insert(*d); }
                *census.entry(format!("{:?}", op).split('(').next().unwrap().to_string()).or_default() += 1;
            }
        }
        let mut v: Vec<_> = census.into_iter().collect();
        v.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        let top = v.into_iter().take(12).map(|(k, n)| format!("{} {}", k, n)).collect();
        (nodes, forks, top)
    }

    // Base: same shape, NO pins.
    let base = trace_frame(&mut it, &reset, &fr, st.clone(), &roots, &[], &ival, Some(crate::trace::widen::WidenMode::Level0(crate::interpreter::abstraction::SpdPrecision::Exact, crate::interpreter::abstraction::PosPrecision::EXACT)), &[])
        .map_err(|e| anyhow!("base trace of shape {}: {:#}", shape_idx, e))?;
    let (bn, bf, _) = measure(&it.d.graph, &base);

    // Pinned.
    let f = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, Some(crate::trace::widen::WidenMode::Level0(crate::interpreter::abstraction::SpdPrecision::Exact, crate::interpreter::abstraction::PosPrecision::EXACT)), &[])
        .map_err(|e| anyhow!("pinned trace of shape {}: {:#}", shape_idx, e))?;
    let (pn, pf, ptop) = measure(&it.d.graph, &f);

    // Slot -> path, so Cell(i) in the dump can be read.
    if std::env::var("CELESTE_SPEC_SLOTS").is_ok() {
        for (i, sp) in f.iface.slots.iter().enumerate() {
            eprintln!("  slot {} = {}", i, iface::show(sp));
        }
    }
    // The assembled kernel's shape: bind + fuse (every fork and button
    // configuration resolved), then count the fused graph's nodes and
    // its BODIES - one body is one distinct output row an input row can
    // produce, so the body count bounds the fan-out per input row.
    let fused_of = |fr: &super::verify::Frame| -> Result<(usize, usize)> {
        let bound = super::emit::bind(fr, &it.d.graph, true)?;
        let (g, bodies, _, _) = super::emit::asm_fused(&bound, None, true)?;
        Ok((g.len(), bodies.len()))
    };
    let (b_fused, b_bodies) = fused_of(&base)?;
    let (p_fused, p_bodies) = fused_of(&f)?;
    let mut out = String::new();
    out.push_str(&format!("shape {}: {} outcomes, {} players, {} springs\n", shape_idx, f.outs.len(), players.len(), springs.len()));
    out.push_str(&format!("  pinned: {}\n", pinned_paths.join(", ")));
    out.push_str(&format!("  BASE   : {} nodes, {} live forks {:?}, counter {}; fused {} nodes, {} bodies\n", bn, bf.len(), bf, base_forks, b_fused, b_bodies));
    out.push_str(&format!("  PINNED : {} nodes, {} live forks {:?}, counter {}; fused {} nodes, {} bodies\n", pn, pf.len(), pf, f.forks, p_fused, p_bodies));
    out.push_str(&format!("  pinned op census: {}\n", ptop.join(", ")));
    // CELESTE_SPEC_CMPS: dump symbolic comparison nodes (box tests etc).
    if std::env::var("CELESTE_SPEC_CMPS").is_ok() {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let mut seen = std::collections::BTreeSet::new();
        for id in 0..g.len() {
            if reach[id] {
                if matches!(g.get(id as u32).op, Op::Gt|Op::Ge|Op::Lt|Op::Le) {
                    let t = super::emit::show_tree(g, id as u32, 4);
                    if seen.insert(t.clone()) { out.push_str(&format!("  cmp: {}\n", t)); }
                }
            }
        }
    }
    // CELESTE_SPEC_BTNFORKS: for each of the 64 button assignments,
    // resolve the buttons (specialize_config_into) and count how many
    // forks remain LIVE. If per-button the count is ~2 (rem x,y), the
    // emitter's 2^fork_depth enumeration is over-counting per config.
    if std::env::var("CELESTE_SPEC_BTNFORKS").is_ok() {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let mut hist: std::collections::BTreeMap<usize, usize> = Default::default();
        for m in 0u8..64 {
            let mut sp = g.like();
            let mapped = g.specialize_config_into(m, None, &mut sp);
            let sroots: Vec<_> = roots_n.iter().map(|r| mapped[*r as usize]).collect();
            let reach = crate::transpile::bdd::reachable(&sp, &sroots);
            let mut forks = std::collections::BTreeSet::new();
            for id in 0..sp.len() {
                if reach[id] { if let Op::Split(d) = sp.get(id as u32).op { forks.insert(d); } }
            }
            *hist.entry(forks.len()).or_default() += 1;
        }
        out.push_str(&format!("  live forks per button assignment (fork_depth {}): {:?}\n", f.forks, hist));
    }
    // What each LIVE fork forks on.
    {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        for id in 0..g.len() {
            if reach[id] {
                if let Op::Split(d) = g.get(id as u32).op {
                    let operand = g.get(id as u32).args[0];
                    out.push_str(&format!("  fork {}: {}\n", d, super::emit::show_tree(g, operand, 8)));
                }
            }
        }
    }
    Ok(out)
}

/// The per-shape constant lattice by FIXPOINT (plans/specialize.md D1).
///
/// Seeds from the concrete spawn state (all fields constant) and traces
/// forward keeping each shape's known-constant fields CONCRETE (pinned)
/// rather than abstracting them - so a field the frame never changes
/// (spring `spd`, static positions) stays a constant and folds the
/// collisions that depend on it. On reaching a shape, the outcome's
/// constant fields are INTERSECTED into that shape's lattice; a shape
/// whose lattice shrinks is re-processed. Monotone (fields only go
/// constant->abstract), so it terminates.
///
/// Returns `(shape key -> constant field map)` plus a blanked
/// representative state per shape for later re-tracing.
///
/// `opts` is the widening axis (plans/kernel-ladder.md): the fixpoint
/// and every traced frame run under the variant's own options, so the
/// shape set and the emitted frames are self-consistent per variant.
/// The baked constants come out the same either way - a field the
/// boundary widens is excluded from `field_constants` regardless - but
/// EXACT's shapes differ (rem as a plain number), so the fixpoint must
/// see the variant it generates for.
pub fn room_constant_lattice(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<LatticeWalk> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::run_one;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    type Cmap = std::collections::BTreeMap<super::iface::Path, super::iface::Conc>;

    let src = cart::sources_in(root)?;
    // The ASTs outlive the walk: the tracer is kept (`LatticeWalk::tracer`)
    // to re-trace a shape under a speed bucket on demand (the bucket
    // dispatch), and `Interp` borrows what it ran.
    let leak = |a: full_moon::ast::Ast| -> &'static full_moon::ast::Ast { Box::leak(Box::new(a)) };
    let top = leak(full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?);
    cart::check_absent_fields(top)?;
    let init = leak(full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?);
    let reset = leak(full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?);
    let fr = leak(full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?);

    let mut it: Interp<'static, Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());
    // Every frame this walk traces (and its key traces, through copies of
    // this tracer) forks the held-button trails (`widen::fork_held_inputs`).
    it.d.held_unknown = opts.held;
    // ... and replaces the fly fruit's widened fields (`widen::fork_fruit_inputs`).
    it.d.fruit_unknown = opts.fruit;

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let st = run_one(&mut it, top, st)?;
    let mut st = st;
    cart::inject_tile_flag_at(&mut st);
    let start = run_one(&mut it, init, st)?;

    let key = |st: &super::state::State<Symbolic>| -> Result<String> { Ok(format!("{:?}", st.shape()?)) };

    let mut lattice: std::collections::BTreeMap<String, Cmap> = Default::default();
    let mut reps: std::collections::BTreeMap<String, super::state::State<Symbolic>> = Default::default();
    let mut forks: std::collections::BTreeMap<String, usize> = Default::default();
    let mut frames: std::collections::BTreeMap<WalkNode, WalkFrame> = Default::default();
    let mut forkops: std::collections::BTreeMap<String, Vec<String>> = Default::default();
    let mut refused: std::collections::BTreeMap<WalkNode, String> = Default::default();
    let mut ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>> = Default::default();

    let sk = key(&start)?;
    let start_key = sk.clone();
    lattice.insert(sk.clone(), shapes::field_constants(&start, &it.d, opts.spd_ival(), opts.pos_ival(), opts.held, opts.fruit)?);
    reps.insert(sk.clone(), start.clone());
    // THE REGION KEY (`RegionGrid`): a node is (shape, region), the lattice
    // stays per shape, and a narrowed lattice re-traces every region its
    // shape has reached.
    let grid = region_grid();
    anyhow::ensure!(
        grid.is_none() || (opts.spd == crate::interpreter::abstraction::SpdPrecision::Exact && opts.pos == crate::interpreter::abstraction::PosPrecision::EXACT),
        "the region key (CELESTE_REGION) is built only for exact-speed, exact-position levels, not {opts:?}"
    );
    let start_regions = successor_regions(&start, &mut it.d, grid)?;
    let mut regions: std::collections::BTreeMap<String, std::collections::BTreeSet<Option<Region>>> = Default::default();
    regions.insert(sk.clone(), start_regions.iter().copied().collect());
    let room0 = shapes::room_of(&start, &it.d);
    let lattice_trace = std::env::var_os("CELESTE_LATTICE_TRACE").is_some();

    // THE ROUNDS IN PARALLEL (2026-09-17, plans/room30.md): each round traces
    // its whole frontier, one copy of the tracer per worker, against the
    // round's snapshot of the lattice, and the results are applied in node
    // order between rounds. A narrowed lattice re-traces a node once per
    // round, not once per narrowing (room (3,0): 471 serial traces for 114
    // nodes, 315 s). A new shape's representative crosses from a worker's
    // arena into the walk's through `shapes::rebase` (checked); each frame is
    // bound in the arena it was traced in (`WalkFrame::bound`).
    let mut rep_constants: std::collections::BTreeMap<String, std::collections::HashMap<u32, super::iface::Conc>> = Default::default();
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut tracers: Vec<Tracer> = (0..n_workers).map(|_| Tracer { it: it.clone(), reset, fr }).collect();
    let mut frontier: Vec<WalkNode> = start_regions.into_iter().map(|r| (sk.clone(), r)).collect();
    let (mut traces, mut round) = (0usize, 0usize);
    let t_walk = std::time::Instant::now();
    while !frontier.is_empty() {
        round += 1;
        traces += frontier.len();
        if traces > 20000 {
            bail!("constant-lattice fixpoint did not converge");
        }
        let t_round = std::time::Instant::now();
        let jobs: Vec<WalkJob> = frontier
            .iter()
            .map(|(k, region)| -> Result<WalkJob> {
                let st = reps[k].clone();
                let roots = shapes::state_paths(&st)?;
                let ival = if opts.ival { with_extra(shapes::ival_paths(&st, opts.spd_ival(), opts.pos_ival()), ival_extra.get(k)) } else { Vec::new() };
                // Pin the shape's known constants (only those that are real
                // scalar inputs here), everything else abstract.
                let pin: Vec<(super::iface::Path, super::iface::Conc)> =
                    lattice[k].iter().filter(|(p, _)| roots.iter().any(|r| r == *p)).map(|(p, c)| (p.clone(), *c)).collect();
                let bounds: Bounds = match (grid, *region, shapes::player_path(&st)) {
                    (Some(g), Some(r), Some(pl)) => g.bounds(&pl, r).into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect(),
                    (None, None, _) | (Some(_), None, None) => Vec::new(),
                    (g, r, pl) => bail!("walk node {r:?} of a shape {} a player under grid {g:?}", if pl.is_some() { "with" } else { "without" }),
                };
                let rebase = if *k == start_key { None } else { Some(rep_constants[k].clone()) };
                Ok(WalkJob { node: (k.clone(), *region), st, roots, pin, ival, bounds, rebase })
            })
            .collect::<Result<_>>()?;
        let next = std::sync::atomic::AtomicUsize::new(0);
        let (jobs_ref, next_ref) = (&jobs, &next);
        let results: Vec<Result<Vec<(usize, WalkTraced)>>> = std::thread::scope(|scope| {
            let handles: Vec<_> = tracers
                .iter_mut()
                .map(|tr| {
                    std::thread::Builder::new()
                        .stack_size(128 * 1024 * 1024)
                        .spawn_scoped(scope, move || -> Result<Vec<(usize, WalkTraced)>> {
                            let mut done = Vec::new();
                            loop {
                                let i = next_ref.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some(job) = jobs_ref.get(i) else { break };
                                done.push((i, walk_trace(tr, job, opts, grid, room0)?));
                            }
                            Ok(done)
                        })
                        .expect("spawn walk worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("walk worker panicked")).collect()
        });
        let mut traced: Vec<(usize, WalkTraced)> = Vec::new();
        for r in results {
            traced.extend(r?);
        }
        traced.sort_by_key(|(i, _)| *i);
        let mut next_frontier: std::collections::BTreeSet<WalkNode> = Default::default();
        for (i, t) in traced {
            let job = &jobs[i];
            let (k, region) = (&job.node.0, job.node.1);
            match t {
                WalkTraced::Refused(e) => {
                    // Remember the refusal instead of silently skipping: a
                    // shape that never traces is a MISSING KERNEL, and the
                    // post-fixpoint check below turns that into a hard
                    // error. (A refusal on an early pass that a later
                    // re-trace of the same shape survives is fine - the
                    // successful frame lands in `frames`.)
                    //
                    // And the shape's EARLIER frame goes: it was traced under
                    // pins the lattice has since dropped, so a kernel built from
                    // it bakes constants the shape no longer has. Keeping it
                    // turned a refused re-trace into a runtime premise failure
                    // (room (4,0): the spawn's `state`/`delay` baked at 0, a
                    // KERNEL COVERAGE GAP at f8, 2026-09-16).
                    refused.insert(job.node.clone(), e);
                    frames.remove(&job.node);
                }
                WalkTraced::Traced { frame, bound, forks: live_forks, forkops: ops, nodes_added, arena, outs, skipped } => {
                    refused.remove(&job.node);
                    forks.insert(k.clone(), live_forks);
                    if live_forks > 2 {
                        forkops.insert(k.clone(), ops);
                    }
                    if lattice_trace {
                        eprintln!(
                            "[lattice] trace shape #{} region {:?} ({} pinned): {} outcomes, {} nodes added ({} in the worker's arena)",
                            lattice.keys().position(|x| x == k).unwrap_or(usize::MAX),
                            region,
                            job.pin.len(),
                            frame.outs.len(),
                            nodes_added,
                            arena
                        );
                        for s in &skipped {
                            eprintln!("[lattice]   outcome skipped: {s}");
                        }
                    }
                    for o in outs {
                        let tk = o.key;
                        // The slots this outcome wrote an interval to, outside
                        // the boundary's own widenings: the next frame reads
                        // them as interval inputs.
                        let mut new_ival = false;
                        for p in o.ival {
                            new_ival |= ival_extra.entry(tk.clone()).or_default().insert(p);
                        }
                        if lattice_trace {
                            let known = lattice.get(&tk).map(|m| m.iter().filter(|(p, v)| o.constants.get(*p) != Some(*v)).map(|(p, _)| super::iface::show(p)).collect::<Vec<_>>());
                            match known {
                                None => {
                                    // What separates the new shape from the one
                                    // whose frame produced it.
                                    let d = shape_key_diff(k, &tk, 40);
                                    eprintln!(
                                        "[lattice]   outcome: NEW shape, {} constants; from shape #{}: after `{}`, `{}` ({} tokens) became `{}` ({} tokens)",
                                        o.constants.len(),
                                        lattice.keys().position(|x| x == k).unwrap_or(usize::MAX),
                                        d.before,
                                        d.a,
                                        d.a_len,
                                        d.b,
                                        d.b_len
                                    )
                                }
                                Some(gone) => eprintln!("[lattice]   outcome: shape #{}, constants removed: {:?}", lattice.keys().position(|x| *x == tk).unwrap_or(usize::MAX), gone),
                            }
                        }
                        let changed = match lattice.get_mut(&tk) {
                            None => {
                                lattice.insert(tk.clone(), o.constants);
                                let mut rep = o.st;
                                shapes::rebase(&mut rep, &mut it.d, &o.heap_constants)?;
                                rep_constants.insert(tk.clone(), shapes::heap_constants(&rep, &it.d));
                                reps.insert(tk.clone(), rep);
                                true
                            }
                            Some(m) => {
                                let before = m.len();
                                m.retain(|p, v| o.constants.get(p) == Some(v));
                                m.len() != before
                            }
                        };
                        // The outcome's regions: the new ones are traced, and a
                        // narrowed lattice re-traces every region the shape has
                        // reached.
                        let reached = regions.entry(tk.clone()).or_default();
                        let mut push: Vec<Option<Region>> = if changed || new_ival { reached.iter().copied().collect() } else { Vec::new() };
                        for r in o.regions {
                            if reached.insert(r) {
                                push.push(r);
                            }
                        }
                        for r in push {
                            next_frontier.insert((tk.clone(), r));
                        }
                    }
                    // Keep the converged frame for generation (last trace wins).
                    frames.insert(job.node.clone(), WalkFrame { frame, bounds: job.bounds.clone(), bound });
                }
            }
        }
        if lattice_trace {
            eprintln!("[walk] round {round}: {} traced, {} next, {:.1} s", jobs.len(), next_frontier.len(), t_round.elapsed().as_secs_f64());
        }
        frontier = next_frontier.into_iter().collect();
    }
    // Every reachable shape must have a frame. A shape whose every
    // trace refused would otherwise just be MISSING from the generated
    // set - a silent runtime coverage gap, fatal under strict mode and
    // invisible until then.
    let nodes: Vec<WalkNode> = regions.iter().flat_map(|(k, rs)| rs.iter().map(move |r| (k.clone(), *r))).collect();
    let missing: Vec<String> = nodes
        .iter()
        .filter(|node| !frames.contains_key(*node))
        .map(|node| format!("shape {} region {:?}: {}", node.0, node.1, refused.get(node).map(String::as_str).unwrap_or("never traced")))
        .collect();
    if !missing.is_empty() {
        bail!(
            "the constant-lattice fixpoint could not trace {} of {} (shape, region) nodes:\n{}",
            missing.len(),
            nodes.len(),
            missing.join("\n")
        );
    }
    eprintln!(
        "[walk] reached {} shapes in {} (shape, region) nodes: {traces} traces in {round} rounds, {:.1} s ({n_workers} workers)",
        lattice.len(),
        nodes.len(),
        t_walk.elapsed().as_secs_f64()
    );
    let graph = it.d.graph.clone();
    let mut by_hash = std::collections::HashMap::new();
    for ((k, _), wf) in &frames {
        by_hash.insert(wf.frame.in_rt2.shape_hash_of(), k.clone());
    }
    Ok(LatticeWalk { lattice, reps, forks, frames, graph, cart: cart_data, cache, forkops, tracer: Tracer { it, reset, fr }, by_hash, opts, start_key, ival_extra })
}

/// `ival_paths` plus a shape's discovered interval slots (`LatticeWalk::
/// ival_extra`), each once.
fn with_extra(mut ival: Vec<super::iface::Path>, extra: Option<&std::collections::BTreeSet<super::iface::Path>>) -> Vec<super::iface::Path> {
    for p in extra.into_iter().flatten() {
        if !ival.contains(p) {
            ival.push(p.clone());
        }
    }
    ival
}

/// What one walk node is traced under, from its round's snapshot of the
/// lattice.
struct WalkJob {
    node: WalkNode,
    st: super::state::State<super::domain::Symbolic>,
    roots: Vec<super::iface::Path>,
    pin: Vec<(super::iface::Path, super::iface::Conc)>,
    ival: Vec<super::iface::Path>,
    bounds: Bounds,
    /// `Some` where the representative is a rebased outcome, not the walk's
    /// start state: its heap's constants in the walk's arena, to rebase it
    /// into the worker's (`shapes::rebase`).
    rebase: Option<std::collections::HashMap<u32, super::iface::Conc>>,
}

/// A live outcome of a traced walk node, as the walk reads it: its shape
/// key, its constants, the slots it wrote an interval to beyond the
/// boundary's own widenings, its regions, and the state (a new shape's
/// representative).
struct WalkOutcome {
    key: String,
    constants: std::collections::BTreeMap<super::iface::Path, super::iface::Conc>,
    ival: Vec<super::iface::Path>,
    regions: Vec<Option<Region>>,
    st: super::state::State<super::domain::Symbolic>,
    /// `shapes::heap_constants` of `st`, in the worker's arena.
    heap_constants: std::collections::HashMap<u32, super::iface::Conc>,
}

/// One traced walk node, or why its trace refused.
enum WalkTraced {
    Refused(String),
    Traced {
        frame: super::verify::Frame,
        bound: std::result::Result<Bound, String>,
        forks: usize,
        forkops: Vec<String>,
        nodes_added: usize,
        arena: usize,
        outs: Vec<WalkOutcome>,
        skipped: Vec<&'static str>,
    },
}

/// A walk node's converged frame, the bounds it was traced under (its
/// region's; none without a region key), and the frame bound in the arena it
/// was traced in (a worker's, gone once the walk returns), or why it did not
/// bind.
pub struct WalkFrame {
    pub frame: super::verify::Frame,
    pub bounds: Bounds,
    pub bound: std::result::Result<Bound, String>,
}

/// Trace one walk node on a worker's tracer and read what the walk needs off
/// it, all in that worker's arena.
fn walk_trace(
    tr: &mut Tracer,
    job: &WalkJob,
    opts: super::shapes::WalkOpts,
    grid: Option<RegionGrid>,
    room0: (i16, i16),
) -> Result<WalkTraced> {
    use super::domain::Domain;
    use super::{shapes, verify};
    use crate::transpile::graph::Op;
    let mut st = job.st.clone();
    if let Some(constants) = &job.rebase {
        shapes::rebase(&mut st, &mut tr.it.d, constants)?;
    }
    let f = match verify::trace_frame(&mut tr.it, tr.reset, tr.fr, st, &job.roots, &job.pin, &job.ival, opts.widen_mode(), &job.bounds) {
        Ok(f) => f,
        Err(e) => return Ok(WalkTraced::Refused(format!("{:#}", e))),
    };
    let nodes_added = tr.it.d.node_count().saturating_sub(tr.it.trace_start_nodes);
    // Live forks of THIS trace.
    let (live_forks, forkops) = {
        let g = &tr.it.d.graph;
        let mut rn: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs {
            for (_, nd, _) in &o.fields {
                rn.push(*nd);
            }
            rn.push(o.guard);
            rn.push(o.ok);
        }
        let reach = crate::transpile::bdd::reachable(g, &rn);
        let splits: Vec<u32> = (0..g.len()).filter(|&id| reach[id] && matches!(g.get(id as u32).op, Op::Split(_))).map(|id| id as u32).collect();
        let mut live = std::collections::BTreeSet::new();
        for &id in &splits {
            if let Op::Split(d) = g.get(id).op {
                live.insert(d);
            }
        }
        let ops: Vec<String> = if live.len() > 2 { splits.iter().map(|&id| super::emit::show_tree(g, g.get(id).args[0], 5)).collect() } else { Vec::new() };
        (live.len(), ops)
    };
    let mut outs = Vec::new();
    let mut skipped = Vec::new();
    for o in &f.outs {
        if tr.it.d.decide(&o.ok) == Some(false) {
            skipped.push("ok folds false");
            continue;
        }
        if shapes::room_of(&o.st, &tr.it.d) != room0 {
            skipped.push("another room");
            continue;
        }
        let key = format!("{:?}", o.st.shape()?);
        let constants = shapes::field_constants(&o.st, &tr.it.d, opts.spd_ival(), opts.pos_ival(), opts.held, opts.fruit)?;
        let mut ival = Vec::new();
        if opts.ival {
            let widened = shapes::ival_paths(&o.st, opts.spd_ival(), opts.pos_ival());
            let fruit: Vec<super::iface::Path> = if opts.fruit { super::widen::fly_fruit_paths(&o.st).all().cloned().collect() } else { Vec::new() };
            for p in shapes::state_paths(&o.st)? {
                if widened.contains(&p) || fruit.contains(&p) {
                    continue;
                }
                if let Some(super::heap::Value::Num(n)) = super::iface::get(&o.st, &p) {
                    if tr.it.d.is_interval(&n) {
                        ival.push(p);
                    }
                }
            }
        }
        let regions = successor_regions(&o.st, &mut tr.it.d, grid)?;
        let heap_constants = shapes::heap_constants(&o.st, &tr.it.d);
        outs.push(WalkOutcome { key, constants, ival, regions, st: o.st.clone(), heap_constants });
    }
    let bound = super::emit::bind(&f, &tr.it.d.graph, opts.widen).map_err(|e| format!("{:#}", e));
    Ok(WalkTraced::Traced { frame: f, bound, forks: live_forks, forkops, nodes_added, arena: tr.it.d.node_count(), outs, skipped })
}

/// The tracer a walk ran in, kept so a shape can be re-traced later in
/// the same arena (the walk's states are graph nodes of it).
#[derive(Clone)]
pub struct Tracer {
    pub it: super::interp::Interp<'static, super::domain::Symbolic>,
    pub reset: &'static full_moon::ast::Ast,
    pub fr: &'static full_moon::ast::Ast,
}

// The raw pointers inside (`Interp::body_ids`' function-body keys and the
// AST references) all point into the `'static` ASTs `room_constant_lattice`
// leaks - immutable and never freed - so a copy of the tracer can move to a
// key fixpoint worker (the same argument as `RefEngine`'s).
unsafe impl Send for Tracer {}

/// The parts of a lattice walk a key trace reads (`key_frame`), shared
/// read-only by the key fixpoint's workers, each tracing in its own copy of
/// the walk's `Tracer`.
pub struct WalkView<'a> {
    pub by_hash: &'a std::collections::HashMap<u64, String>,
    pub reps: &'a std::collections::BTreeMap<String, super::state::State<super::domain::Symbolic>>,
    pub lattice: &'a std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc>>,
    pub ival_extra: &'a std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>>,
    pub opts: super::shapes::WalkOpts,
}

impl LatticeWalk {
    pub fn view(&self) -> WalkView<'_> {
        WalkView { by_hash: &self.by_hash, reps: &self.reps, lattice: &self.lattice, ival_extra: &self.ival_extra, opts: self.opts }
    }
}

/// The output of `room_constant_lattice`.
pub struct LatticeWalk {
    pub tracer: Tracer,
    /// Shape hash (of the input structure) -> the walk's shape key.
    pub by_hash: std::collections::HashMap<u64, String>,
    pub opts: super::shapes::WalkOpts,
    /// The start state's shape key.
    pub start_key: String,
    pub lattice: std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc>>,
    pub reps: std::collections::BTreeMap<String, super::state::State<super::domain::Symbolic>>,
    pub forks: std::collections::BTreeMap<String, usize>,
    /// Per (shape, region), its converged frame, bounds and binding.
    pub frames: std::collections::BTreeMap<WalkNode, WalkFrame>,
    pub graph: crate::transpile::graph::Graph,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    pub forkops: std::collections::BTreeMap<String, Vec<String>>,
    /// Per shape, the input slots a reachable frame WROTE an interval to,
    /// beyond `shapes::ival_paths` (the boundary's widenings): a value from
    /// `rnd` (the chest's shake), or computed from one. Typed as interval
    /// inputs, since a number input reading an interval panicked (room
    /// (4,0) f62, the chest's `x`). Monotone like the constants.
    pub ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>>,
}

/// Report the constant lattice for `transpile --room-consts`.
pub fn room_constants(root: &std::path::Path) -> Result<String> {
    let LatticeWalk { lattice, forks, frames, cart, cache, forkops, .. } =
        room_constant_lattice(root, super::shapes::WalkOpts::LEVEL0)?;
    let room = crate::transpile::graph::Room { cart: cart.clone(), cache: cache.clone() };
    // Lower each converged frame (bound by the walk) to get the emitted size.
    let mut bodies_by_shape: std::collections::BTreeMap<String, usize> = Default::default();
    for ((k, _), wf) in &frames {
        if let Ok(bound) = &wf.bound {
            if let Ok(low) = super::emit::lower_frame(bound, Some(room.clone()), engine_ranges(&wf.frame, &wf.bounds)) {
                *bodies_by_shape.entry(k.clone()).or_default() += low.bodies;
            }
        }
    }
    let mut out = String::new();
    out.push_str(&format!("{} shapes reached (constant-lattice fixpoint)\n", lattice.len()));
    for (i, (k, cm)) in lattice.iter().enumerate() {
        let spd: Vec<String> = cm.keys().map(super::iface::show)
            .filter(|s| s.contains("objects[") && s.contains(".spd")).collect();
        out.push_str(&format!("shape {}: {} const fields, {} forks, {} bodies\n",
            i, cm.len(), forks.get(k).copied().unwrap_or(999), bodies_by_shape.get(k).copied().unwrap_or(0)));
        if let Some(ops) = forkops.get(k) {
            for o in ops.iter().take(4) { out.push_str(&format!("    fork: {}\n", o)); }
        }
        let _ = spd;
    }
    out.push_str(&format!("total bodies (lattice): {}\n", bodies_by_shape.values().sum::<usize>()));
    Ok(out)
}
