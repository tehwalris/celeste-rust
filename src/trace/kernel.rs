//! The kernel pipeline's front half: the constant-lattice walk over a
//! room's (shape, region) nodes (`room_constant_lattice`), then binding and
//! lowering each traced frame (`lattice_kernel_refs`) for the ASM backend
//! (`compiled::asm_kernel`).

use anyhow::Result;

use super::emit::{Bound, Lowered};
use super::verify::Frame;


/// One traced frame, bound and lowered, everything owned (nothing borrows
/// the tracer's ASTs).
pub struct Reference {
    pub frame: Frame,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Restate a lowering failure with its `Cell(n)`s NAMED by their
/// interface paths.
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
/// the walk's (deterministic) node order. A frame that fails to bind or
/// lower is FATAL: a missing kernel would be a coverage gap at runtime.
pub(crate) fn lattice_kernel_refs(
    root: &std::path::Path,
    opts: crate::abstraction::Level,
) -> Result<Vec<(Option<Region>, Reference)>> {
    let mut lw = room_constant_lattice(root, opts)?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    // Each frame was bound by the walk in the worker arena it was traced in.
    let frames: Vec<(Option<Region>, super::verify::Frame, Bounds, Bound)> = std::mem::take(&mut lw.frames)
        .into_iter()
        .map(|((shape, region), wf)| -> Result<_> {
            let bound = wf.bound.map_err(|e| anyhow::anyhow!("the walk's frame of shape {shape} region {region:?} did not bind: {e}"))?;
            Ok((region, wf.frame, wf.bounds, bound))
        })
        .collect::<Result<Vec<_>>>()?;
    // Lower (specialize + decide, the expensive half of a build), in parallel.
    let (cart, cache) = (lw.cart.clone(), lw.cache.clone());
    let room = &room;
    // A pool pulling frames off an atomic index. `CELESTE_BUILD_THREADS`
    // caps the frames lowered at once: the largest take several GB each.
    let n_workers = std::env::var("CELESTE_BUILD_THREADS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4))
        .min(frames.len().max(1));
    let next = std::sync::atomic::AtomicUsize::new(0);
    // Each frame is taken by exactly one worker (a `Frame` is not cloned).
    let slots: Vec<std::sync::Mutex<Option<(Option<Region>, super::verify::Frame, Bounds, Bound)>>> =
        frames.into_iter().map(|x| std::sync::Mutex::new(Some(x))).collect();
    let slots = &slots;
    let mut built: Vec<(usize, Result<(Option<Region>, Reference)>)> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..n_workers)
            .map(|_| {
                let (cart, cache, next) = (cart.clone(), cache.clone(), &next);
                std::thread::Builder::new()
                    .stack_size(128 * 1024 * 1024)
                    .spawn_scoped(scope, move || {
                        let mut out = Vec::new();
                        loop {
                            let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some((key, f, bounds, bound)) = slots.get(i).and_then(|m| m.lock().unwrap().take()) else { break };
                            let r = (|| -> Result<(Option<Region>, Reference)> {
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

/// THE REGION KEY: a kernel is specialized on the player's whole-pixel
/// position lying in one square of a `px`-pixel grid and its speed in
/// `[-speed, speed]` px per frame; a lane outside declines loudly. Bounding
/// the position lets the range analysis fold away far-off objects'
/// collision tests. Rows dispatch on the region of their input cell.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Region {
    pub ix: i16,
    pub iy: i16,
}

/// The grid a region key is taken on (`region_grid`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RegionGrid {
    pub px: i32,
    pub speed: i32,
}

/// The region grid of every kernel set. `CELESTE_REGION="px,S"` sets it,
/// `off` leaves every kernel set unkeyed (one kernel per shape); unset it is
/// 16 px with speeds within 6 px. `S` is at most 7: the move loop is
/// unrolled for `abs(amount) <= 8` (`Interp::unroll_bound`), and `amount`
/// is `flr(rem + spd + 0.5)`.
pub fn region_grid() -> Option<RegionGrid> {
    static GRID: std::sync::OnceLock<Option<RegionGrid>> = std::sync::OnceLock::new();
    *GRID.get_or_init(|| {
        let g = match std::env::var("CELESTE_REGION").ok().as_deref() {
            Some("off") => None,
            Some(s) => {
                let v: Vec<i32> = s.split(',').map(|t| t.trim().parse().expect("CELESTE_REGION=\"px,S\" or off")).collect();
                assert!(v.len() == 2 && v[0] >= 1 && (1..=7).contains(&v[1]), "CELESTE_REGION=\"px,S\" with px >= 1 and 1 <= S <= 7, not {s:?}");
                Some(RegionGrid { px: v[0], speed: v[1] })
            }
            None => Some(RegionGrid { px: 16, speed: 6 }),
        };
        match g {
            Some(g) => eprintln!("[region] kernels keyed on a {} px grid, player speed asserted within [-{}, {}] px", g.px, g.speed, g.speed),
            None => eprintln!("[region] no region key (CELESTE_REGION=off)"),
        }
        g
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
            // `move` leaves `rem` in `[-0.5, 0.5)`; bounding it gives the
            // move amount a range (collisions reach 7 px each way, not 8).
            (field(&["rem", "x"]), (-0x8000, 0x7fff)),
            (field(&["rem", "y"]), (-0x8000, 0x7fff)),
        ]
    }
}

/// The input bounds a frame is traced under (`verify::trace_frame`).
pub type Bounds = Vec<(super::iface::Path, (i32, i32))>;

/// How many frames of platform motion the platform worlds cover
/// (`concrete::platform_worlds`): a platforms-unknown level is sound only for
/// a search no longer than this, and refuses a longer one
/// (`frame::forward_frame`).
pub const PLATFORM_WORLD_FRAMES: usize = 127;

/// A node of the constant-lattice walk: a shape key and a region.
pub type WalkNode = (String, Option<Region>);

/// The regions a state's rows can be in: `None` without a grid or without a
/// player; otherwise every region the hull of the player's whole-pixel `x`
/// and `y` touches, clamped to the screen plus one region on each side (an
/// axis with no static range takes that whole window). Over-covering only
/// adds kernels; a row outside every region stops the run.
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
    // THE WALK PROBE: `CELESTE_WALK_REGIONS="(ix,iy) .."` traces only those
    // regions. Not a search mode: every other row has no kernel.
    static ONLY: std::sync::OnceLock<Option<Vec<(i16, i16)>>> = std::sync::OnceLock::new();
    let only = ONLY.get_or_init(|| {
        std::env::var("CELESTE_WALK_REGIONS").ok().map(|v| {
            v.split_whitespace()
                .map(|t| {
                    let t = t.trim_matches(|c| c == '(' || c == ')');
                    let (x, y) = t.split_once(',').expect("CELESTE_WALK_REGIONS=\"(ix,iy) ..\"");
                    (x.parse().expect("ix"), y.parse().expect("iy"))
                })
                .collect()
        })
    });
    if let Some(o) = only {
        // Reachable or not.
        return Ok(o.iter().map(|&(ix, iy)| Some(Region { ix, iy })).collect());
    }
    Ok((a.iy..=b.iy).flat_map(|iy| (a.ix..=b.ix).map(move |ix| Some(Region { ix, iy }))).collect())
}

/// THE NO-PLAYER PHASE'S RANGES (region-keyed rooms). One trace covers
/// every frame of the spawn, so a field that varies over it (the spawn's
/// `y`) would be an unbounded input and the player it creates could be
/// anywhere. No input matters before the player exists, so the phase is
/// DETERMINISTIC: run it fully pinned while the shape stays the no-player
/// shape, and bound each numeric field to the hull of what it took.
/// Asserted like a region's bounds: a lane outside declines.
fn no_player_ranges(
    it: &mut super::interp::Interp<'static, super::domain::Symbolic>,
    reset: &'static full_moon::ast::Ast,
    fr: &'static full_moon::ast::Ast,
    start: &super::state::State<super::domain::Symbolic>,
    opts: crate::abstraction::Level,
) -> Result<Bounds> {
    use super::iface::Conc;
    use super::shapes;
    if shapes::player_path(start).is_some() {
        return Ok(Vec::new());
    }
    let shape = start.shape()?;
    let mut hull: std::collections::BTreeMap<super::iface::Path, (i32, i32)> = Default::default();
    let mut st = start.clone();
    // The values, read before `rebase` blanks them and pinned back by the
    // next trace.
    let mut consts = shapes::field_constants(start, &it.d, &opts)?;
    // The spawn takes ~20 frames; the cap stops a phase that never ends.
    // Only a phase run to its END (the player appears, or the shape
    // changes) bounds anything; one that cannot run concretely (an `rnd`
    // draw) is not deterministic and bounds nothing.
    let mut ended = false;
    for _ in 0..240 {
        for (p, c) in &consts {
            if let Conc::Num(v) = c {
                let r = v.as_raw_u32() as i32;
                let e = hull.entry(p.clone()).or_insert((r, r));
                *e = (e.0.min(r), e.1.max(r));
            }
        }
        let roots = shapes::state_paths(&st)?;
        let pin: Vec<(super::iface::Path, Conc)> = consts.iter().filter(|(p, _)| roots.contains(p)).map(|(p, c)| (p.clone(), *c)).collect();
        let Ok(f) = super::verify::trace_frame(it, reset, fr, st.clone(), &roots, &pin, &[], true, &[]) else { break };
        let [o] = f.outs.as_slice() else { break };
        if o.shape != shape || shapes::player_path(&o.st).is_some() {
            ended = true;
            break;
        }
        consts = shapes::field_constants(&o.st, &it.d, &opts)?;
        let heap = shapes::heap_constants(&o.st, &it.d);
        let mut next = o.st.clone();
        shapes::rebase(&mut next, &mut it.d, &heap)?;
        st = next;
    }
    if !ended {
        return Ok(Vec::new());
    }
    Ok(hull.into_iter().filter(|(_, (lo, hi))| lo < hi).collect())
}

/// A frame's bounds by ENGINE cell: what the lowering's decide step and
/// fragment pruning read.
fn engine_ranges(
    f: &super::verify::Frame,
    bounds: &[(super::iface::Path, (i32, i32))],
) -> std::collections::HashMap<u32, (i32, i32)> {
    bounds
        .iter()
        .filter_map(|(p, r)| f.iface.slots.iter().position(|q| q == p).map(|i| (f.in_cells[i], *r)))
        .collect()
}

#[cfg(test)]
mod tests {

    #[test]
    fn near_floors_side_by_side_split_only_what_collisions_read() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        std::env::set_var("CELESTE_START_ROOM", "6,1");
        std::env::set_var("CELESTE_WALK_REGIONS", "(3,6) (4,6)");
        let opts = crate::abstraction::Level { held: true, floors_near: true, ..crate::abstraction::Level::EXACT };
        let lw = super::room_constant_lattice(std::path::Path::new("."), opts).unwrap_or_else(|e| panic!("{e:#}"));
        let mut traced = 0;
        for ((shape, region), f) in &lw.frames {
            let Some(r) = region else { continue };
            assert!(f.frame.outs.len() <= 72, "shape {shape} region ({},{}): {} outcomes", r.ix, r.iy, f.frame.outs.len());
            traced += 1;
        }
        assert!(traced >= 2, "{traced} (shape, region) nodes traced");
    }
}

/// The per-shape constant lattice by FIXPOINT over (shape, region) nodes.
///
/// Seeds from the concrete spawn state and traces forward with each
/// shape's known-constant fields PINNED, so a field no frame changes stays
/// a constant and folds what depends on it. An outcome's constants are
/// INTERSECTED into its shape's lattice; a shape whose lattice shrinks is
/// re-traced. Monotone (fields only go constant -> abstract), so it
/// terminates.
///
/// Every frame is traced under `opts` (the level), so the shape set and the
/// frames are consistent with the level the kernels are built for.
pub fn room_constant_lattice(
    root: &std::path::Path,
    opts: crate::abstraction::Level,
) -> Result<LatticeWalk> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::run_one;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    type Cmap = std::collections::BTreeMap<super::iface::Path, super::iface::Conc>;

    let src = cart::sources_in(root)?;
    // Leaked: the tracer is kept (`LatticeWalk::tracer`) and `Interp`
    // borrows the ASTs.
    let leak = |a: full_moon::ast::Ast| -> &'static full_moon::ast::Ast { Box::leak(Box::new(a)) };
    let top = leak(full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?);
    cart::check_absent_fields(top)?;
    let init = leak(full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?);
    let reset = leak(full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?);
    let fr = leak(full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?);

    let mut it: Interp<'static, Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = crate::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());
    // The level's widenings, for every frame this walk (and every copy of
    // its tracer) traces.
    it.d.held_unknown = opts.held;
    it.d.fruit_unknown = opts.fruit;
    it.d.floors_near = opts.floors_near;
    it.d.platforms_unknown = opts.platforms;
    if opts.platforms {
        it.d.worlds = Some(std::sync::Arc::new(crate::concrete::platform_worlds(PLATFORM_WORLD_FRAMES)?));
    }

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
    // Poisoned paths over the whole walk, by reason (summed per-job drains).
    let mut illegal: std::collections::BTreeMap<String, usize> = Default::default();
    // The nodes whose `Frame::raise` did not fold to false.
    let mut raising: std::collections::BTreeSet<WalkNode> = Default::default();
    let mut ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>> = Default::default();

    let sk = key(&start)?;
    let start_key = sk.clone();
    let no_player_shape = start.shape()?;
    let no_player = match region_grid() {
        Some(_) => no_player_ranges(&mut it, reset, fr, &start, opts)?,
        None => Vec::new(),
    };
    if std::env::var_os("CELESTE_LATTICE_TRACE").is_some() {
        eprintln!("[walk] no-player ranges: {:?}", no_player.iter().map(|(p, (a, b))| format!("{} [{:.2}, {:.2}]", super::iface::show(p), *a as f64 / 65536.0, *b as f64 / 65536.0)).collect::<Vec<_>>());
    }
    lattice.insert(sk.clone(), shapes::field_constants(&start, &it.d, &opts)?);
    reps.insert(sk.clone(), start.clone());
    // The START state's own intervals (an `rnd` draw in `_init`, e.g. a
    // balloon's `offset`) are interval inputs too, at every level: no traced
    // frame wrote them, so the write discovery below would never type them.
    // Only the representative holds a blanked POINT; the lattice was read
    // from `start`, where the slot is no constant.
    let mut start_ivals: std::collections::BTreeMap<super::iface::Path, (i32, i32)> = Default::default();
    {
        use super::domain::Domain as _;
        for r in shapes::state_paths(&start)? {
            for p in super::iface::scalars(&start, &r)? {
                let Some(super::heap::Value::Num(n)) = super::iface::get(&start, &p) else { continue };
                if it.d.as_const(&n).is_some() {
                    continue;
                }
                if let crate::transpile::graph::Op::Const(lo, hi) = it.d.graph.get(n).op {
                    start_ivals.insert(p.clone(), (lo, hi));
                }
                let zero = it.d.num(celeste_core::pico8_num::Pico8Num::from_i16(0));
                super::iface::set(reps.get_mut(&sk).expect("inserted above"), &p, super::heap::Value::Num(zero))?;
                ival_extra.entry(sk.clone()).or_default().insert(p);
            }
        }
    }
    // A node is (shape, region); the lattice stays per shape.
    let grid = region_grid();
    let start_regions = successor_regions(&start, &mut it.d, grid)?;
    let mut regions: std::collections::BTreeMap<String, std::collections::BTreeSet<Option<Region>>> = Default::default();
    regions.insert(sk.clone(), start_regions.iter().copied().collect());
    let room0 = shapes::room_of(&start, &it.d);
    let lattice_trace = std::env::var_os("CELESTE_LATTICE_TRACE").is_some();

    // THE ROUNDS: each traces its whole frontier in parallel against the
    // round's lattice snapshot; results are applied in node order between
    // rounds. A new shape's representative crosses from a worker's arena
    // into the walk's through `shapes::rebase`; each frame is bound in the
    // arena it was traced in (`WalkFrame::bound`).
    let mut rep_constants: std::collections::BTreeMap<String, std::collections::HashMap<u32, super::iface::Conc>> = Default::default();
    // `CELESTE_WALK_THREADS` caps the workers: each holds its own arena,
    // and the heaviest traces take GBs each.
    let n_workers = std::env::var("CELESTE_WALK_THREADS").ok().and_then(|v| v.parse().ok()).unwrap_or_else(|| std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4));
    let mut tracers: Vec<Tracer> = (0..n_workers).map(|_| Tracer { it: it.clone(), reset, fr }).collect();
    let mut frontier: Vec<WalkNode> = start_regions.into_iter().map(|r| (sk.clone(), r)).collect();
    // Shapes whose reached regions wait for a re-trace (the deferred re-trace).
    let mut dirty: std::collections::BTreeSet<String> = Default::default();
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
                let ival = with_extra(boundary_ival(&st, opts), ival_extra.get(k));
                // Pin the shape's known constants that are inputs here.
                let pin: Vec<(super::iface::Path, super::iface::Conc)> =
                    lattice[k].iter().filter(|(p, _)| roots.iter().any(|r| r == *p)).map(|(p, c)| (p.clone(), *c)).collect();
                let bounds: Bounds = match (grid, *region, shapes::player_path(&st)) {
                    (Some(g), Some(r), Some(pl)) => {
                        g.bounds(&pl, r).into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect()
                    }
                    (None, None, _) => Vec::new(),
                    // The spawn phase's hull, on ITS shape only: bounds are
                    // by path, and in another shape `objects[k]` is another
                    // object.
                    (Some(_), None, None) if st.shape()? == no_player_shape => {
                        no_player.iter().filter(|(p, _)| roots.iter().any(|q| q == p)).cloned().collect()
                    }
                    (Some(_), None, None) => Vec::new(),
                    (g, r, pl) => bail!("walk node {r:?} of a shape {} a player under grid {g:?}", if pl.is_some() { "with" } else { "without" }),
                };
                let rebase = if *k == start_key { None } else { Some(rep_constants[k].clone()) };
                Ok(WalkJob { node: (k.clone(), *region), st, roots, pin, ival, bounds, rebase })
            })
            .collect::<Result<_>>()?;
        let next = std::sync::atomic::AtomicUsize::new(0);
        let (jobs_ref, next_ref) = (&jobs, &next);
        let results: Vec<Result<Vec<(usize, WalkTraced)>>> = std::thread::scope(|scope| {
            // Each worker OWNS its pristine copy (a `Tracer` is `Send`, not
            // `Sync`) and only ever clones from it.
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
                                // Every job in a FRESH copy of the starting
                                // arena, so a frame's graph is a function of
                                // the job, not of scheduling (`renumber_cells`
                                // orders commutative operands by arena id).
                                let mut fresh = Tracer { it: tr.it.clone(), reset: tr.reset, fr: tr.fr };
                                done.push((i, walk_trace(&mut fresh, job, opts, grid, room0)?));
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
                WalkTraced::Refused(e, poisoned) => {
                    for (why, n) in poisoned {
                        *illegal.entry(why).or_default() += n;
                    }
                    // Remembered: a node that never traces is a MISSING
                    // KERNEL, a hard error after the fixpoint (a later
                    // successful re-trace clears it). The node's EARLIER
                    // frame goes too: it was traced under pins the lattice
                    // has since dropped, so it would bake stale constants.
                    refused.insert(job.node.clone(), e);
                    frames.remove(&job.node);
                }
                WalkTraced::Traced { frame, bound, forks: live_forks, forkops: ops, nodes_added, arena, outs, skipped, illegal: poisoned, can_raise } => {
                    for (why, n) in poisoned {
                        *illegal.entry(why).or_default() += n;
                    }
                    if can_raise {
                        raising.insert(job.node.clone());
                    }
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
                        // Slots written an interval beyond the boundary's
                        // widenings: interval inputs of the next frame.
                        let mut new_ival = false;
                        for p in o.ival {
                            if lattice_trace && !ival_extra.get(&tk).is_some_and(|s| s.contains(&p)) {
                                eprintln!("[lattice]   outcome: new interval slot {} (shape #{})", super::iface::show(&p), lattice.keys().position(|x| *x == tk).unwrap_or(usize::MAX));
                            }
                            new_ival |= ival_extra.entry(tk.clone()).or_default().insert(p);
                        }
                        if lattice_trace {
                            let known = lattice.get(&tk).map(|m| m.iter().filter(|(p, v)| o.constants.get(*p) != Some(*v)).map(|(p, _)| super::iface::show(p)).collect::<Vec<_>>());
                            match known {
                                None => eprintln!(
                                    "[lattice]   outcome: NEW shape, {} constants, from shape #{}",
                                    o.constants.len(),
                                    lattice.keys().position(|x| x == k).unwrap_or(usize::MAX)
                                ),
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
                        // New regions are traced next round; a narrowed
                        // shape is re-traced later (`dirty`, below).
                        let reached = regions.entry(tk.clone()).or_default();
                        if changed || new_ival {
                            dirty.insert(tk.clone());
                        }
                        for r in o.regions {
                            if reached.insert(r) {
                                next_frontier.insert((tk.clone(), r));
                            }
                        }
                    }
                    // The last trace wins.
                    frames.insert(job.node.clone(), WalkFrame { frame, bounds: job.bounds.clone(), bound });
                }
            }
        }
        // THE DEFERRED RE-TRACE: a shape whose lattice narrowed (or gained
        // an interval slot) is re-traced in every region it reached once the
        // walk finds no new region, not at each narrowing. Sound: pins only
        // drop, so a trace under older pins sees a subset of what the final
        // ones admit, and the re-trace under the final lattice finds the
        // rest, repeating until nothing changes.
        if next_frontier.is_empty() {
            for k in std::mem::take(&mut dirty) {
                for r in regions.get(&k).into_iter().flatten() {
                    next_frontier.insert((k.clone(), *r));
                }
            }
        }
        if lattice_trace {
            eprintln!("[walk] round {round}: {} traced, {} next, {:.1} s", jobs.len(), next_frontier.len(), t_round.elapsed().as_secs_f64());
        }
        frontier = next_frontier.into_iter().collect();
    }
    // Every reachable node must have a frame, or its kernel is missing and
    // the gap would only show at runtime.
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
    // POISONED PATHS, always reported: a Lua type error the tracer could
    // not prove unreachable is a modelling gap (zero in every room so far).
    if !illegal.is_empty() {
        let total: usize = illegal.values().sum();
        eprintln!("[walk] {total} POISONED paths (a Lua raise the tracer could not rule out), by reason:");
        for (why, n) in &illegal {
            eprintln!("[walk]   {n} x {why}");
        }
    }
    // THE RAISE ROW: the poison count is how often the tracer HIT a raise
    // site; this is whether the resulting condition survived folding.
    if !raising.is_empty() {
        eprintln!(
            "[walk] {} of {} (shape, region) nodes have a LIVE raise row (`Frame::raise` did not fold to false)",
            raising.len(),
            nodes.len()
        );
    }
    let graph = it.d.graph.clone();
    let mut by_hash = std::collections::HashMap::new();
    for ((k, _), wf) in &frames {
        by_hash.insert(wf.frame.in_rt2.shape_hash_of(), k.clone());
    }
    Ok(LatticeWalk { lattice, reps, forks, frames, graph, cart: cart_data, cache, forkops, tracer: Tracer { it, reset, fr }, by_hash, opts, start_key, ival_extra, start_ivals })
}

/// The interval inputs the BOUNDARY widens (the player's `rem`, and the
/// level's widened objects). The `rnd`-derived ones (`ival_extra`) come on
/// top (`with_extra`).
fn boundary_ival(st: &super::state::State<super::domain::Symbolic>, opts: crate::abstraction::Level) -> Vec<super::iface::Path> {
    let mut ival = super::shapes::ival_paths(st);
    // The moving platforms' `x` and `last` at a platforms-unknown level.
    if opts.platforms {
        let pp = super::widen::platform_paths(st);
        ival.extend(pp.x.iter().chain(pp.last.iter()).cloned());
    }
    // A near level's floors (`state` an interval, `collideable` a boolean a
    // lane may hold unknown) and the objects' phases. Not the countdowns:
    // those are the unknown number (`widen::forget_countdown_inputs`).
    if opts.floors_near {
        ival.extend(super::widen::near_floor_paths(st).all().cloned());
        ival.extend(super::widen::phase_paths(st).into_iter().filter(|(_, r)| !matches!(r, super::widen::PhaseRange::Countdown)).map(|(p, _)| p));
    }
    ival
}

/// `ival` plus a shape's discovered interval slots, each once.
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
    /// For a representative that is not the start state: its heap's
    /// constants in the walk's arena, to rebase it into the worker's.
    rebase: Option<std::collections::HashMap<u32, super::iface::Conc>>,
}

/// A live outcome of a traced walk node, as the walk reads it.
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
    /// Why the trace did not compile, and what it poisoned before refusing.
    Refused(String, std::collections::BTreeMap<String, usize>),
    Traced {
        frame: super::verify::Frame,
        bound: std::result::Result<Bound, String>,
        forks: usize,
        forkops: Vec<String>,
        nodes_added: usize,
        arena: usize,
        outs: Vec<WalkOutcome>,
        skipped: Vec<&'static str>,
        /// Paths this trace POISONED, by reason (`Interp::illegal`, drained
        /// per job so the walk can sum them).
        illegal: std::collections::BTreeMap<String, usize>,
        /// `Frame::raise` did not fold to `false` (decided in the worker,
        /// whose arena the node lives in).
        can_raise: bool,
    },
}

/// A walk node's converged frame, the bounds it was traced under, and the
/// frame bound in the arena it was traced in (or why it did not bind).
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
    opts: crate::abstraction::Level,
    grid: Option<RegionGrid>,
    room0: (i16, i16),
) -> Result<WalkTraced> {
    use super::domain::Domain;
    use super::{shapes, verify};
    use crate::transpile::graph::Op;
    // `Interp::illegal` accumulates across a worker's jobs, so both
    // `WalkTraced` exits DRAIN it; the `?` exits need not, since an `Err`
    // fails the whole walk.
    debug_assert!(tr.it.illegal.is_empty(), "a previous job left {} poison reasons behind", tr.it.illegal.len());
    tr.it.illegal.clear();
    let mut st = job.st.clone();
    if let Some(constants) = &job.rebase {
        shapes::rebase(&mut st, &mut tr.it.d, constants)?;
    }
    let f = match verify::trace_frame(&mut tr.it, tr.reset, tr.fr, st, &job.roots, &job.pin, &job.ival, true, &job.bounds) {
        Ok(f) => f,
        // A refused trace's poisoned paths count too.
        Err(e) => return Ok(WalkTraced::Refused(format!("{:#}", e), std::mem::take(&mut tr.it.illegal))),
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
            rn.push(o.error);
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
        // A non-constant room (-1) is not "another room": it may hold this
        // room's states, so it is an error rather than skipped
        // (`state::same_room` keeps rooms apart, so it should not happen).
        let room = shapes::room_of(&o.st, &tr.it.d);
        anyhow::ensure!(room.0 >= 0 && room.1 >= 0, "an outcome whose room is not a constant: {room:?}");
        if room != room0 {
            skipped.push("another room");
            continue;
        }
        let key = format!("{:?}", o.st.shape()?);
        let constants = shapes::field_constants(&o.st, &tr.it.d, &opts)?;
        // An interval beyond the boundary's widenings came from `rnd`.
        let mut ival = Vec::new();
        {
            let widened = boundary_ival(&o.st, opts);
            let fruit = shapes::level_widened_paths(&o.st, &opts);
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
    let bound = super::emit::bind(&f, &tr.it.d.graph).map_err(|e| format!("{:#}", e));
    // DRAINED, not copied (see the top of this function).
    let illegal = std::mem::take(&mut tr.it.illegal);
    // Decided HERE: `f.raise` names a node of this worker's arena.
    let can_raise = tr.it.d.decide(&f.raise) != Some(false);
    Ok(WalkTraced::Traced { frame: f, bound, forks: live_forks, forkops, nodes_added, arena: tr.it.d.node_count(), outs, skipped, illegal, can_raise })
}

/// The tracer a walk ran in, kept so a shape can be re-traced later in
/// the same arena (the walk's states are graph nodes of it).
#[derive(Clone)]
pub struct Tracer {
    pub it: super::interp::Interp<'static, super::domain::Symbolic>,
    pub reset: &'static full_moon::ast::Ast,
    pub fr: &'static full_moon::ast::Ast,
}

// SAFETY: the raw pointers inside (`Interp::body_ids`' keys, the AST
// references) point into the leaked `'static` ASTs, immutable and never
// freed, so a copy of the tracer can move to a walk worker.
unsafe impl Send for Tracer {}

/// The output of `room_constant_lattice`.
pub struct LatticeWalk {
    pub tracer: Tracer,
    /// Shape hash (of the input structure) -> the walk's shape key.
    pub by_hash: std::collections::HashMap<u64, String>,
    pub opts: crate::abstraction::Level,
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
    /// Per shape, the input slots a reachable frame WROTE an interval to
    /// beyond the boundary's widenings (an `rnd`-derived value), typed as
    /// interval inputs. Monotone like the constants.
    pub ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>>,
    /// The START state's own intervals (an `rnd` draw), by path: the
    /// representative holds a blanked POINT there, so a reader seeding values
    /// from it (level -1) takes the real range from here.
    pub start_ivals: std::collections::BTreeMap<super::iface::Path, (i32, i32)>,
}
