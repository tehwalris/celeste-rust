//! The kernel pipeline's front half: the constant-lattice walk over a room's
//! (shape, region) nodes, then binding and lowering each traced frame.

use anyhow::Result;

use celeste_core::arith::{Arithmetic, Fixed};

use super::emit::{Bound, Lowered};
use super::verify::Frame;
use crate::transpile::garith::GraphArith;


/// One traced frame, bound and lowered, everything owned.
pub struct Reference<A: GraphArith = Fixed> {
    pub frame: Frame<A>,
    pub bound: Bound<A>,
    pub lowered: Lowered<A>,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Restate a lowering failure with its `Cell(n)`s named by interface path.
fn name_cells<A: GraphArith>(f: &super::verify::Frame<A>, e: anyhow::Error) -> anyhow::Error {
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


/// The constant-lattice `Reference`s for the start room, in walk order. A
/// frame that fails to bind or lower is FATAL (a coverage gap otherwise).
pub(crate) fn lattice_kernel_refs<A: GraphArith>(
    root: &std::path::Path,
    opts: crate::abstraction::Level,
    s: &crate::settings::Settings,
) -> Result<Vec<(Option<Region>, Reference<A>)>> {
    let mut lw = room_constant_lattice(root, opts, &Walk { settings: s, only: None })?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    // Each frame was bound by the walk in the worker arena it was traced in.
    let frames: Vec<(Option<Region>, super::verify::Frame<A>, Bound<A>)> = std::mem::take(&mut lw.frames)
        .into_iter()
        .map(|((shape, region), wf)| -> Result<_> {
            let bound = wf.bound.map_err(|e| anyhow::anyhow!("the walk's frame of shape {shape} region {region:?} did not bind: {e}"))?;
            Ok((region, wf.frame, bound))
        })
        .collect::<Result<Vec<_>>>()?;
    // Lower (the expensive half of a build), in parallel.
    let (cart, cache) = (lw.cart.clone(), lw.cache.clone());
    let room = &room;
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4).min(frames.len().max(1));
    let next = std::sync::atomic::AtomicUsize::new(0);
    // Each frame is taken by exactly one worker (a `Frame` is not cloned).
    let slots: Vec<std::sync::Mutex<Option<(Option<Region>, super::verify::Frame<A>, Bound<A>)>>> =
        frames.into_iter().map(|x| std::sync::Mutex::new(Some(x))).collect();
    let slots = &slots;
    let mut built: Vec<(usize, Result<(Option<Region>, Reference<A>)>)> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..n_workers)
            .map(|_| {
                let (cart, cache, next) = (cart.clone(), cache.clone(), &next);
                std::thread::Builder::new()
                    .stack_size(128 * 1024 * 1024)
                    .spawn_scoped(scope, move || {
                        let mut out = Vec::new();
                        loop {
                            let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some((key, f, bound)) = slots.get(i).and_then(|m| m.lock().unwrap().take()) else { break };
                            let r = (|| -> Result<(Option<Region>, Reference<A>)> {
                                let lowered = super::emit::lower_frame(&bound, Some(room.clone()), s.diag.build_trace)
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
/// position in one `px`-pixel grid square and speed in `[-speed, speed]`,
/// so the range analysis folds away far-off collision tests; a lane outside
/// declines loudly. Each bound is an `Op::Restrict` on its input cell
/// (`verify::trace_frame`): the range analyses read the range off that node,
/// and its own error, on the raw cell, is the per-lane check (`trace::error`).
/// The dispatch picks the kernel by the lane's region, so `x`/`y` hold by
/// construction; speed and remainder hold only by that check.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Region {
    pub ix: i16,
    pub iy: i16,
}

/// The grid a region key is taken on (`--region "px,S"`, `off` for none,
/// default 16 px / speed 6: `Settings::region_grid`). `S <= 7` because the
/// move loop unrolls for `abs(amount) <= 8` and `amount = flr(rem + spd +
/// 0.5)`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RegionGrid {
    pub px: i32,
    pub speed: i32,
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

    /// The bounds a region's kernel is traced under, by path under `pl`
    /// (16.16 constants, exact in every arithmetic; the remainder's range
    /// the table's, `widening::range`).
    fn bounds<A: GraphArith>(&self, pl: &super::iface::Path, r: Region) -> Bounds<A> {
        const ONE: i32 = 1 << 16;
        let lit = |raw: i32| A::lit(A::from_fixed_raw(raw));
        let (rlo, rhi) = celeste_engine::widening::range::<A::Num>(celeste_engine::widening::REM.0, celeste_engine::widening::REM.1);
        let rem = (A::lit(rlo), A::lit(rhi));
        let field = |fs: &[&str]| -> super::iface::Path {
            let mut q = pl.clone();
            for f in fs {
                q.push(super::iface::key(f));
            }
            q
        };
        let (x0, y0) = (r.ix as i32 * self.px, r.iy as i32 * self.px);
        vec![
            (field(&["x"]), (lit(x0 * ONE), lit((x0 + self.px - 1) * ONE))),
            (field(&["y"]), (lit(y0 * ONE), lit((y0 + self.px - 1) * ONE))),
            (field(&["spd", "x"]), (lit(-self.speed * ONE), lit(self.speed * ONE))),
            (field(&["spd", "y"]), (lit(-self.speed * ONE), lit(self.speed * ONE))),
            // `move` leaves `rem` in `[-0.5, 0.5)`; bounding it bounds the move.
            (field(&["rem", "x"]), rem),
            (field(&["rem", "y"]), rem),
        ]
    }
}

/// The input bounds a frame is traced under (`verify::trace_frame`).
pub type Bounds<A = Fixed> = Vec<(super::iface::Path, (<A as Arithmetic>::Lit, <A as Arithmetic>::Lit))>;

/// Frames of platform motion the platform worlds cover: a platforms-unknown
/// level is sound only up to this horizon and refuses a longer search.
pub const PLATFORM_WORLD_FRAMES: usize = 127;

/// A node of the constant-lattice walk: a shape key and a region.
pub type WalkNode = (String, Option<Region>);

/// The regions a state's rows can be in (`None` without a grid or player):
/// every region the hull of the player's `x`/`y` touches, clamped to the
/// screen plus one region. Over-covering only adds kernels.
fn successor_regions<A: GraphArith>(
    st: &super::state::State<super::domain::Symbolic<A>>,
    d: &mut super::domain::Symbolic<A>,
    grid: Option<RegionGrid>,
    only: Option<&[Region]>,
) -> Result<Vec<Option<Region>>> {
    use super::{iface, shapes};
    let (Some(g), Some(pl)) = (grid, shapes::player_path(st)) else { return Ok(vec![None]) };
    let window = (-(g.px as i64), 128 + g.px as i64 - 1);
    let mut hull = [(0i64, 0i64); 2];
    for (axis, name) in ["x", "y"].into_iter().enumerate() {
        let mut p = pl.clone();
        p.push(iface::key(name));
        let Some(super::heap::Value::Num(n)) = iface::get(st, &p) else { anyhow::bail!("{}: not a number", iface::show(&p)) };
        hull[axis] = match d.range_of(n).as_ref().and_then(A::floor_hull) {
            Some((lo, hi)) => (lo.max(window.0), hi.min(window.1)),
            None => window,
        };
        anyhow::ensure!(hull[axis].0 <= hull[axis].1, "{}: its range lies outside the screen window {window:?}", iface::show(&p));
    }
    let (a, b) = (g.of(hull[0].0 as i32, hull[1].0 as i32), g.of(hull[0].1 as i32, hull[1].1 as i32));
    // A test's walk traces just these (`room_constant_lattice`'s `only`).
    if let Some(o) = only {
        return Ok(o.iter().map(|&r| Some(r)).collect());
    }
    Ok((a.iy..=b.iy).flat_map(|iy| (a.ix..=b.ix).map(move |ix| Some(Region { ix, iy }))).collect())
}

/// THE NO-PLAYER PHASE'S RANGES. One trace covers a whole stretch of the
/// spawn, so a field varying over it would be an unbounded input. No input
/// matters before the player exists, so run the phase fully pinned and bound
/// each numeric field to the hull it took, PER SHAPE: the phase may change
/// shape before the player appears (room (3,0): the start's shape lasts one
/// frame, and a later spawn shape traced unbounded made every fall floor
/// "near", 2^12 outcomes). The chain runs the fly fruit exact; the walk and
/// the forward run the level's widenings, under which the phase reaches
/// shapes the chain never takes (a widened fly fruit gone at any frame:
/// room (6,0) at `r0sxhfp`), so the start is also EXPLORED at the level's
/// own widenings (`explore_no_player`) into the same hulls, and so is every
/// RESPAWN the walk reaches (the room reloaded after a death, every object
/// updated once). A shape no explored state has gets the spawn object's hull
/// by type (`NoPlayer::bounds`). A lane outside declines.
fn no_player_ranges<A: GraphArith>(
    it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
    reset: &'static full_moon::ast::Ast,
    fr: &'static full_moon::ast::Ast,
    start: &super::state::State<super::domain::Symbolic<A>>,
    opts: crate::abstraction::Level,
) -> Result<NoPlayer<A>> {
    use super::domain::Domain as _;
    use super::iface::Conc;
    use super::shapes;
    if shapes::player_path(start).is_some() {
        return Ok(NoPlayer::default());
    }
    let mut np = NoPlayer::default();
    // The chain runs with the fly fruit EXACT (unknown, its `fly` and `y`
    // fork every frame into outcomes, and the chain needs one), and the
    // fruit's fields, unknown at a fruit level, bound nothing there.
    let fruit = std::mem::replace(&mut it.d.level.fruit, false);
    let chain_opts = crate::abstraction::Level { fruit: false, ..opts };
    // Only a phase run to its END bounds anything, and only one that runs
    // concretely. Two things stop that, and end the chain bounding nothing
    // (said in the log): a start holding an `rnd` draw (a balloon's `offset`
    // without `--balloon-seeds`), which no pin can hold, and a frame whose
    // unpinned fields (the level's widened ones) fork it into several
    // outcomes. Every other refusal is an error.
    let run = (|| -> Result<bool> {
        let drawn = shapes::state_paths(start)?.into_iter().find(|p| match super::iface::get(start, p) {
            Some(super::heap::Value::Num(n)) => it.d.as_const(&n).is_none(),
            Some(super::heap::Value::Bool(b)) => it.d.decide(&b).is_none(),
            _ => false,
        });
        if let Some(p) = drawn {
            eprintln!("[walk] the start holds an `rnd` draw ({}): its no-player phase cannot run concretely, no hull bounds it", super::iface::show(&p));
            return Ok(false);
        }
        let mut st = start.clone();
        // Read before `rebase` blanks them; the next trace pins them back.
        let mut consts = shapes::field_constants(start, &it.d, &chain_opts)?;
        for k in 0..NO_PLAYER_FRAMES {
            np.note(&st, &consts, fruit)?;
            let roots = shapes::state_paths(&st)?;
            let pin: Vec<(super::iface::Path, Conc<A>)> = consts.iter().filter(|(p, _)| roots.contains(p)).map(|(p, c)| (p.clone(), *c)).collect();
            let f = super::verify::trace_frame(it, reset, fr, st.clone(), &roots, &pin, &[], true, &[], &[]).map_err(|e| e.context(format!("the start's no-player chain, frame {k}")))?;
            let o = match f.outs.as_slice() {
                [o] => o,
                [] => anyhow::bail!("the start's no-player chain, frame {k}: the frame has no outcome"),
                outs => {
                    eprintln!("[walk] the start's no-player chain forks into {} outcomes at frame {k}: no hull bounds its phase", outs.len());
                    return Ok(false);
                }
            };
            if shapes::player_path(&o.st).is_some() {
                return Ok(true);
            }
            consts = shapes::field_constants(&o.st, &it.d, &chain_opts)?;
            let heap = shapes::heap_constants(&o.st, &it.d);
            let mut next = o.st.clone();
            shapes::rebase(&mut next, &mut it.d, &heap)?;
            st = next;
        }
        anyhow::bail!("the start's no-player phase did not reach the player within {NO_PLAYER_FRAMES} frames")
    })();
    it.d.level.fruit = fruit;
    if !run? {
        return Ok(NoPlayer::default());
    }
    np.on = true;
    let c = shapes::field_constants(start, &it.d, &opts)?;
    let (n, followed, ended) = explore_no_player(it, reset, fr, start.clone(), c, opts, &mut np, "the start's")?;
    eprintln!("[walk] the start explored: {n} states explored, {followed} outcomes followed, {ended} reach the player");
    Ok(np)
}

/// Frames a no-player phase may last (the start's chain, an exploration):
/// one that has not reached the player by then is FATAL.
const NO_PLAYER_FRAMES: usize = 240;

/// THE NO-PLAYER EXPLORATION: from an entry of the no-player phase
/// (`entry`, its constants `consts`: the start, or a respawn entry the walk
/// reached - the room reloaded after a death), every outcome of every frame
/// up to the player's appearance, AT THE LEVEL'S OWN WIDENINGS (`opts`, the
/// fly fruit as the level has it: what the walk and the forward reach), into
/// `np`'s hulls. Fields the entry holds unknown (a respawn's: the dead
/// phase's `freeze` and `delay_restart`; the level's widened fields) stay
/// unknown inputs. No merge selects on the state's OWN unknowns (not the
/// level's widened fields): `_update`'s `if freeze>0 then ... return end`
/// would merge the frozen branch into the spawn's fields (`y = Sel(freeze >
/// 0, 124, 120)`), after which none is a constant; refused, each branch
/// keeps its constants (the frozen one repeats its parent). A state is its
/// shape and its constants (the trace pins exactly those), so a repeated one
/// is explored once. FATAL (`what`: whose phase): a state still without the
/// player after `NO_PLAYER_FRAMES` frames, a state that does not trace.
/// Returns the states explored, the outcomes followed and those where the
/// player appears.
fn explore_no_player<A: GraphArith>(
    it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
    reset: &'static full_moon::ast::Ast,
    fr: &'static full_moon::ast::Ast,
    entry: super::state::State<super::domain::Symbolic<A>>,
    consts: std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>,
    opts: crate::abstraction::Level,
    np: &mut NoPlayer<A>,
    what: &str,
) -> Result<(usize, usize, usize)> {
    use super::shapes;
    // The level's fly fruit (`note`: a widened one's fields bound nothing).
    let fruit = it.d.level.fruit;
    let mut seen: std::collections::BTreeSet<String> = Default::default();
    let mut todo = vec![(entry, consts, 0usize)];
    let (mut states, mut followed, mut ended) = (0usize, 0usize, 0usize);
    while let Some((st, consts, depth)) = todo.pop() {
        if !seen.insert(format!("{:?} {consts:?}", st.shape()?)) {
            continue;
        }
        states += 1;
        np.note(&st, &consts, fruit)?;
        anyhow::ensure!(depth < NO_PLAYER_FRAMES, "{what} no-player phase did not reach the player within {NO_PLAYER_FRAMES} frames");
        let roots = shapes::state_paths(&st)?;
        let pin: Vec<(super::iface::Path, super::iface::Conc<A>)> = consts.iter().filter(|(p, _)| roots.contains(p)).map(|(p, c)| (p.clone(), *c)).collect();
        // The state's own unknowns (not the level's widened fields, not
        // the buttons the frame resets): no merge selects on them, so
        // each branch keeps its own constants.
        let widened = shapes::boundary_widened_paths(&st, &opts);
        let buttons = super::iface::key("__button_states");
        let unknown: Vec<super::iface::Path> = roots.iter().filter(|p| !consts.contains_key(*p) && !widened.contains(*p) && p.first() != Some(&buttons)).cloned().collect();
        let f = super::verify::trace_frame(it, reset, fr, st, &roots, &pin, &[], true, &[], &unknown).map_err(|e| e.context(format!("{what} no-player state {depth} frames after its entry did not trace")))?;
        for o in &f.outs {
            followed += 1;
            if shapes::player_path(&o.st).is_some() {
                ended += 1;
                continue;
            }
            let c = shapes::field_constants(&o.st, &it.d, &opts)?;
            let heap = shapes::heap_constants(&o.st, &it.d);
            let mut next = o.st.clone();
            shapes::rebase(&mut next, &mut it.d, &heap)?;
            todo.push((next, c, depth + 1));
        }
    }
    Ok((states, followed, ended))
}

/// A numeric field's hull per path (raw 16.16, inclusive).
type Hull<A> = std::collections::BTreeMap<super::iface::Path, (<A as Arithmetic>::Num, <A as Arithmetic>::Num)>;

/// The no-player phase's hulls over every state explored: the start's chain
/// and each respawn's exploration, per shape and the spawn object's by the
/// path below it. A field bounds its shape where its hull is not one value
/// and every explored state of the shape holds it a constant (`bounds`).
#[derive(Default)]
struct NoPlayer<A: GraphArith = Fixed> {
    /// The start's chain reached the player: the hulls bound.
    on: bool,
    /// Per shape: each numeric field's hull over the states where it was a
    /// constant, and the fields some state held unknown.
    shapes: Vec<(super::heap::Shape, Hull<A>, std::collections::BTreeSet<super::iface::Path>)>,
    /// The spawn object's fields (by the path below it), likewise.
    spawn: Hull<A>,
    spawn_unknown: std::collections::BTreeSet<super::iface::Path>,
    /// The respawn entries explored, by shape and constants.
    entries: std::collections::BTreeSet<String>,
}

impl<A: GraphArith> NoPlayer<A> {
    /// An explored state `st` with its constants `consts` into the hulls
    /// (`fruit`: the level widens the fly fruit, whose fields bound nothing).
    fn note(&mut self, st: &super::state::State<super::domain::Symbolic<A>>, consts: &std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>, fruit: bool) -> Result<()> {
        let shape = st.shape()?;
        let at = match self.shapes.iter().position(|(s, ..)| *s == shape) {
            Some(i) => i,
            None => {
                self.shapes.push((shape, Hull::<A>::new(), Default::default()));
                self.shapes.len() - 1
            }
        };
        let fruit_objs = if fruit { super::widen::objects_of_type(st, "fly_fruit") } else { Vec::new() };
        let spawn_objs = super::widen::objects_of_type(st, "player_spawn");
        let join = |h: &mut Hull<A>, p: super::iface::Path, r: A::Num| {
            let e = h.entry(p).or_insert((r, r));
            *e = (if A::lt(r, e.0) { r } else { e.0 }, if A::lt(e.1, r) { r } else { e.1 });
        };
        for p in super::shapes::state_paths(st)? {
            if !matches!(super::iface::get(st, &p), Some(super::heap::Value::Num(_))) || fruit_objs.iter().any(|o| p.starts_with(o)) {
                continue;
            }
            let below = spawn_objs.iter().find(|o| p.starts_with(o)).map(|o| p[o.len()..].to_vec());
            match consts.get(&p) {
                Some(super::iface::Conc::Num(v)) => {
                    let r = *v;
                    if let Some(b) = below {
                        join(&mut self.spawn, b, r);
                    }
                    join(&mut self.shapes[at].1, p, r);
                }
                _ => {
                    if let Some(b) = below {
                        self.spawn_unknown.insert(b);
                    }
                    self.shapes[at].2.insert(p);
                }
            }
        }
        Ok(())
    }

    /// A no-player state's bounds, by path: its shape's hulls, or for a
    /// shape no explored state has, the spawn object's. A field that varies
    /// but some explored state of the shape holds unknown has no hull: it
    /// bounds nothing (an unbounded input, as every unhulled field) - FATAL
    /// for a field of the spawn, the phase the bounds are for.
    fn bounds(&self, st: &super::state::State<super::domain::Symbolic<A>>) -> Result<Bounds<A>> {
        let shape = st.shape()?;
        let spawn_objs = super::widen::objects_of_type(st, "player_spawn");
        let varying = |h: &Hull<A>, unknown: &std::collections::BTreeSet<super::iface::Path>, spawn: &dyn Fn(&super::iface::Path) -> bool, what: &str| -> Result<Bounds<A>> {
            let mut b = Bounds::<A>::new();
            for (p, &(lo, hi)) in h.iter().filter(|(_, (lo, hi))| A::lt(*lo, *hi)) {
                if !unknown.contains(p) {
                    b.push((p.clone(), (A::lit(lo), A::lit(hi))));
                } else if spawn(p) {
                    anyhow::bail!("THE NO-PLAYER BOUNDS: the spawn's {} varies over {what} but an explored state holds it unknown: no hull bounds it", super::iface::show(p));
                }
            }
            Ok(b)
        };
        if let Some((_, h, unknown)) = self.shapes.iter().find(|(s, ..)| *s == shape) {
            return varying(h, unknown, &|p| spawn_objs.iter().any(|o| p.starts_with(o)), "the explored states of its shape");
        }
        let spawn = varying(&self.spawn, &self.spawn_unknown, &|_| true, "the explored spawn objects")?;
        let mut out = Bounds::<A>::new();
        for o in spawn_objs {
            for (suffix, r) in &spawn {
                let mut p = o.clone();
                p.extend(suffix.iter().cloned());
                out.push((p, *r));
            }
        }
        Ok(out)
    }

    /// Each shape's bounds and the spawn object's, for the log (a field
    /// some explored state holds unknown, which bounds nothing, unshown).
    fn show(&self) -> Vec<String> {
        let show = |h: &Hull<A>, unknown: &std::collections::BTreeSet<super::iface::Path>| h.iter().filter(|(p, (lo, hi))| A::lt(*lo, *hi) && !unknown.contains(*p)).map(|(p, (a, b))| format!("{} [{:.2}, {:.2}]", super::iface::show(p), celeste_core::arith::EngineNum::to_f64(*a), celeste_core::arith::EngineNum::to_f64(*b))).collect::<Vec<_>>();
        let mut out: Vec<String> = self.shapes.iter().enumerate().map(|(i, (_, h, unknown))| format!("[walk] no-player shape {i} ranges: {:?}", show(h, unknown))).collect();
        out.push(format!("[walk] the spawn object's ranges: {:?}", show(&self.spawn, &self.spawn_unknown)));
        out
    }
}

#[cfg(test)]
mod tests {
    /// THE NO-PLAYER CHAIN ENDS QUIETLY ONLY WHERE IT CANNOT RUN CONCRETELY.
    /// Room (5,0)'s balloon draws its `offset` from `rnd` in `_init`: the
    /// chain bounds nothing, and says so. Any other refusal of a chain frame
    /// (here the graph budget) is an error, never "no bounds": until
    /// 2026-10-16 every trace error ended the chain silently.
    #[test]
    fn the_no_player_chain_refuses_loudly_but_for_an_rnd_draw() {
        use crate::settings::Settings;
        let level = crate::abstraction::Level::parse("r0sxh").expect("level");
        let chain = |s: Settings, max_nodes: Option<usize>| {
            let (mut tr, start) = super::boot::<celeste_core::arith::Fixed>(std::path::Path::new("."), &s, level).expect("boot");
            tr.it.max_nodes = max_nodes.unwrap_or(tr.it.max_nodes);
            super::no_player_ranges(&mut tr.it, tr.reset, tr.fr, &start, level)
        };
        let unseeded = chain(Settings { room: (5, 0), ..Default::default() }, None).expect("an `rnd` draw ends the chain quietly");
        assert!(!unseeded.on, "a start with an `rnd` draw bounds nothing");
        let e = chain(Settings::default(), Some(1)).err().expect("a refused chain frame is an error");
        let e = format!("{e:#}");
        assert!(e.contains("no-player chain, frame 0") && e.contains("the trace grew the graph"), "{e}");
    }

    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (6,1)'s lattice walk, ~15 s"]
    fn near_floors_side_by_side_split_only_what_collisions_read() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let s = crate::settings::Settings { room: (6, 1), ..Default::default() };
        let opts = crate::abstraction::Level { held: true, floors_near: true, ..crate::abstraction::Level::EXACT };
        let only = [super::Region { ix: 3, iy: 6 }, super::Region { ix: 4, iy: 6 }];
        let lw: super::LatticeWalk = super::room_constant_lattice(std::path::Path::new("."), opts, &super::Walk { settings: &s, only: Some(&only) }).unwrap_or_else(|e| panic!("{e:#}"));
        let mut traced = 0;
        for ((shape, region), f) in &lw.frames {
            let Some(r) = region else { continue };
            assert!(f.frame.outs.len() <= 72, "shape {shape} region ({},{}): {} outcomes", r.ix, r.iy, f.frame.outs.len());
            traced += 1;
        }
        assert!(traced >= 2, "{traced} (shape, region) nodes traced");
    }

    /// A RESPAWN IS INSIDE ITS SHAPE'S BOUNDS. Room (0,3) gemskip 100% under
    /// the loading jank (J 1) starts with the spawn one update in (y 124, a
    /// shape of its own: the springs gain `delay` at the first boundary), so
    /// the start's chain bounds the next shape's `y` to [98, 120]. A death
    /// reloads the room and updates every object once: the spawn at y 124 in
    /// that shape (the 2500m run's KERNEL COVERAGE GAP, 2026-10-14). Every
    /// no-player node's bound on a spawn's `y` covers 124.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (0,3)'s lattice walk, ~40 CPU-s"]
    fn a_respawn_after_the_loading_jank_is_inside_its_shapes_bounds() {
        use super::super::{iface, shapes, widen};
        let s = crate::settings::Settings { room: (0, 3), hundred: true, gemskip: true, loading_jank: Some(1), ..Default::default() };
        let opts = crate::abstraction::Level::parse("r0sxh").expect("level");
        let only: Vec<super::Region> = (0..8).flat_map(|ix| (5..8).map(move |iy| super::Region { ix, iy })).collect();
        let lw: super::LatticeWalk = super::room_constant_lattice(std::path::Path::new("."), opts, &super::Walk { settings: &s, only: Some(&only) }).unwrap_or_else(|e| panic!("{e:#}"));
        let mut bounded = 0;
        for ((shape, region), f) in &lw.frames {
            let rep = &lw.reps[shape];
            if region.is_some() || shapes::player_path(rep).is_some() {
                continue;
            }
            for o in widen::objects_of_type(rep, "player_spawn") {
                let mut y = o.clone();
                y.push(iface::key("y"));
                if let Some((_, (lo, hi))) = f.bounds.iter().find(|(p, _)| *p == y) {
                    assert!(*hi >= 124 << 16, "a no-player shape bounds {} to [{}, {}]: a respawn's y 124 declines", iface::show(&y), lo >> 16, hi >> 16);
                    bounded += 1;
                }
            }
        }
        assert!(bounded >= 2, "{bounded} no-player shapes bound the spawn's y (the chain's and a respawn's)");
    }
}

/// The walk's tracer at room `s.room`'s start: the cart's toplevel and
/// `_init` run in it, every frame it traces at `opts`. The ASTs are leaked:
/// the kept tracer's `Interp` borrows them.
fn boot<A: GraphArith>(root: &std::path::Path, s: &crate::settings::Settings, opts: crate::abstraction::Level) -> Result<(Tracer<A>, super::state::State<super::domain::Symbolic<A>>)> {
    use super::cart;
    use super::verify::run_one;
    use anyhow::anyhow;
    let src = cart::sources_in(root, s)?;
    let leak = |a: full_moon::ast::Ast| -> &'static full_moon::ast::Ast { Box::leak(Box::new(a)) };
    let top = leak(full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?);
    cart::check_absent_fields(top)?;
    let init = leak(full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?);
    let reset = leak(full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?);
    let fr = leak(full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?);

    let mut it = super::interp::Interp::new(super::domain::Symbolic::<A>::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = s.room;
    it.cache = Some(std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?));
    it.cart = Some(cart_data);
    it.d.build_trace = s.diag.build_trace;
    // The level's widenings, for every frame this walk traces.
    it.d.level = opts;
    if opts.platforms {
        it.d.worlds = Some(std::sync::Arc::new(crate::concrete::platform_worlds::<A>(PLATFORM_WORLD_FRAMES, s)?));
    }
    let st = cart::fresh_state(&mut it.d);
    let mut st = run_one(&mut it, top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let start = run_one(&mut it, init, st)?;
    Ok((Tracer { it, reset, fr }, start))
}

/// The per-shape constant lattice by FIXPOINT over (shape, region) nodes:
/// trace forward from the spawn with each shape's known constants PINNED,
/// INTERSECT each outcome's constants into its shape's lattice, re-trace a
/// shape that shrinks. Monotone (constant -> abstract only), so it
/// terminates. Every frame is traced under `opts`, the kernels' level.
pub fn room_constant_lattice<A: GraphArith>(
    root: &std::path::Path,
    opts: crate::abstraction::Level,
    walk: &Walk,
) -> Result<LatticeWalk<A>> {
    use super::domain::Symbolic;
    use super::shapes;
    use anyhow::bail;

    let (s, only) = (walk.settings, walk.only);
    let (Tracer { mut it, reset, fr }, start) = boot::<A>(root, s, opts)?;
    let (cart_data, cache) = (it.cart.clone().expect("boot loads the cart"), it.cache.clone().expect("boot loads the collision cache"));

    let key = |st: &super::state::State<Symbolic<A>>| -> Result<String> { Ok(format!("{:?}", st.shape()?)) };

    let mut lattice: std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>> = Default::default();
    let mut reps: std::collections::BTreeMap<String, super::state::State<Symbolic<A>>> = Default::default();
    let mut frames: std::collections::BTreeMap<WalkNode, WalkFrame<A>> = Default::default();
    let mut refused: std::collections::BTreeMap<WalkNode, String> = Default::default();
    // Poisoned paths by reason, summed over jobs.
    let mut illegal: std::collections::BTreeMap<String, usize> = Default::default();
    // The nodes whose `Frame::raise` did not fold to false.
    let mut raising: std::collections::BTreeSet<WalkNode> = Default::default();
    let mut ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>> = Default::default();

    let sk = key(&start)?;
    let start_key = sk.clone();
    let mut no_player = match s.region_grid() {
        Some(_) => no_player_ranges(&mut it, reset, fr, &start, opts)?,
        None => NoPlayer::default(),
    };
    if s.diag.build_trace {
        for l in no_player.show() {
            eprintln!("{l}");
        }
    }
    lattice.insert(sk.clone(), shapes::field_constants(&start, &it.d, &opts)?);
    reps.insert(sk.clone(), start.clone());
    // The START state's own intervals (an `rnd` draw in `_init`) are interval
    // inputs at every level: no traced frame wrote them, so write discovery
    // would miss them.
    let mut start_ivals: std::collections::BTreeMap<super::iface::Path, Option<(A::Lit, A::Lit)>> = Default::default();
    {
        use super::domain::Domain as _;
        for r in shapes::state_paths(&start)? {
            for p in super::iface::scalars(&start, &r)? {
                let Some(super::heap::Value::Num(n)) = super::iface::get(&start, &p) else { continue };
                if it.d.as_const(&n).is_some() {
                    continue;
                }
                // Under the loading jank (--loading-jank) the start's
                // `rnd` draw has had one update (a balloon's `offset += 0.01`,
                // its `y` from `sin(offset)`): not a literal but a function of
                // one. Its static range's hull stands for it - a superset of
                // the start's values, sound for level -1's lower bound (it only
                // widens the start; the correlation between slots is dropped).
                let lit = match it.d.graph.get(n).op {
                    crate::transpile::graph::Op::Const(lo, hi) => Some((lo, hi)),
                    _ => it.d.range_of(n).as_ref().and_then(A::lit_hull),
                };
                start_ivals.insert(p.clone(), lit);
                let zero = it.d.num(A::from_i16(0));
                super::iface::set(reps.get_mut(&sk).expect("inserted above"), &p, super::heap::Value::Num(zero))?;
                ival_extra.entry(sk.clone()).or_default().insert(p);
            }
        }
    }
    // A node is (shape, region); the lattice stays per shape.
    let grid = s.region_grid();
    let start_regions = successor_regions(&start, &mut it.d, grid, only)?;
    let mut regions: std::collections::BTreeMap<String, std::collections::BTreeSet<Option<Region>>> = Default::default();
    regions.insert(sk.clone(), start_regions.iter().copied().collect());
    let room0 = shapes::room_of(&start, &it.d);
    let lattice_trace = s.diag.build_trace;

    // THE ROUNDS: each traces its frontier in parallel against a lattice
    // snapshot; results apply in node order between rounds. Each frame is
    // bound in the worker arena it was traced in.
    let mut rep_constants: std::collections::BTreeMap<String, std::collections::HashMap<u32, super::iface::Conc<A>>> = Default::default();
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut tracers: Vec<Tracer<A>> = (0..n_workers).map(|_| Tracer { it: it.clone(), reset, fr }).collect();
    let mut frontier: Vec<WalkNode> = start_regions.into_iter().map(|r| (sk.clone(), r)).collect();
    // Shapes waiting for the deferred re-trace.
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
        let jobs: Vec<WalkJob<A>> = frontier
            .iter()
            .map(|(k, region)| -> Result<WalkJob<A>> {
                let st = reps[k].clone();
                let roots = shapes::state_paths(&st)?;
                let ival = with_extra(boundary_ival(&st, opts), ival_extra.get(k));
                // Pin the shape's known constants that are inputs here.
                let pin: Vec<(super::iface::Path, super::iface::Conc<A>)> =
                    lattice[k].iter().filter(|(p, _)| roots.iter().any(|r| r == *p)).map(|(p, c)| (p.clone(), *c)).collect();
                let bounds: Bounds<A> = match (grid, *region, shapes::player_path(&st)) {
                    (Some(g), Some(r), Some(pl)) => {
                        g.bounds::<A>(&pl, r).into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect()
                    }
                    (None, None, _) => Vec::new(),
                    // The no-player phase's hull (bounds are by path).
                    (Some(_), None, None) => no_player.bounds(&st)?.into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect(),
                    (g, r, pl) => bail!("walk node {r:?} of a shape {} a player under grid {g:?}", if pl.is_some() { "with" } else { "without" }),
                };
                let rebase = if *k == start_key { None } else { Some(rep_constants[k].clone()) };
                Ok(WalkJob { node: (k.clone(), *region), st, roots, pin, ival, bounds, rebase })
            })
            .collect::<Result<_>>()?;
        let next = std::sync::atomic::AtomicUsize::new(0);
        let (jobs_ref, next_ref) = (&jobs, &next);
        let results: Vec<Result<Vec<(usize, WalkTraced<A>)>>> = std::thread::scope(|scope| {
            // Each worker owns its copy (`Tracer` is `Send`, not `Sync`).
            let handles: Vec<_> = tracers
                .iter_mut()
                .map(|tr| {
                    std::thread::Builder::new()
                        .stack_size(128 * 1024 * 1024)
                        .spawn_scoped(scope, move || -> Result<Vec<(usize, WalkTraced<A>)>> {
                            let mut done = Vec::new();
                            loop {
                                let i = next_ref.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some(job) = jobs_ref.get(i) else { break };
                                // A FRESH arena per job, so a graph depends on
                                // the job, not scheduling (`renumber_cells`).
                                let mut fresh = Tracer { it: tr.it.clone(), reset: tr.reset, fr: tr.fr };
                                done.push((i, walk_trace(&mut fresh, job, opts, grid, only, room0)?));
                            }
                            Ok(done)
                        })
                        .expect("spawn walk worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("walk worker panicked")).collect()
        });
        let mut traced: Vec<(usize, WalkTraced<A>)> = Vec::new();
        for r in results {
            traced.extend(r?);
        }
        traced.sort_by_key(|(i, _)| *i);
        let mut next_frontier: std::collections::BTreeSet<WalkNode> = Default::default();
        let mut respawned = false;
        for (i, t) in traced {
            let job = &jobs[i];
            let (k, region) = (&job.node.0, job.node.1);
            match t {
                WalkTraced::Refused(e, poisoned) => {
                    for (why, n) in poisoned {
                        *illegal.entry(why).or_default() += n;
                    }
                    // A node that never traces is a MISSING KERNEL (an error
                    // after the fixpoint). Its earlier frame goes too: it
                    // was traced under pins since dropped.
                    refused.insert(job.node.clone(), e);
                    frames.remove(&job.node);
                }
                WalkTraced::Traced { frame, bound, nodes_added, arena, outs, skipped, illegal: poisoned, can_raise } => {
                    for (why, n) in poisoned {
                        *illegal.entry(why).or_default() += n;
                    }
                    if can_raise {
                        raising.insert(job.node.clone());
                    }
                    refused.remove(&job.node);
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
                        // A RESPAWN entry: its no-player phase explored into
                        // the hulls (once per entry); the shapes whose bounds
                        // grew are re-traced below.
                        if let (true, Some(c)) = (no_player.on, &o.respawn) {
                            if no_player.entries.insert(format!("{tk} {c:?}")) {
                                let mut entry = o.st.clone();
                                shapes::rebase(&mut entry, &mut it.d, &o.heap_constants)?;
                                let (n, followed, ended) = explore_no_player(&mut it, reset, fr, entry, c.clone(), opts, &mut no_player, "a respawn's")?;
                                let shape_no = |x: &String| lattice.keys().position(|y| y == x).map_or("new".to_string(), |i| format!("#{i}"));
                                eprintln!(
                                    "[walk] respawn entry {} (shape {}, from shape {} {:?}): {n} states explored, {followed} outcomes followed, {ended} reach the player",
                                    no_player.entries.len(),
                                    shape_no(&tk),
                                    shape_no(k),
                                    region
                                );
                                respawned = true;
                            }
                        }
                        // Intervals beyond the boundary's widenings: inputs next frame.
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
                        // New regions trace next round; narrowed shapes later (`dirty`).
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
        // A no-player node traced under bounds a respawn has since changed.
        if respawned {
            for ((k, region), wf) in &frames {
                if region.is_none() && shapes::player_path(&reps[k]).is_none() {
                    let roots = shapes::state_paths(&reps[k])?;
                    let now: Bounds<A> = no_player.bounds(&reps[k])?.into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect();
                    if now != wf.bounds {
                        dirty.insert(k.clone());
                    }
                }
            }
            if s.diag.build_trace {
                for l in no_player.show() {
                    eprintln!("{l}");
                }
            }
        }
        // THE DEFERRED RE-TRACE: once no new region appears, re-trace each
        // narrowed shape in every region it reached. Sound: pins only drop,
        // and the re-trace repeats until nothing changes.
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
    // Every reachable node needs a frame, or the gap shows only at runtime.
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
    // POISONED PATHS: a Lua type error not proven unreachable is a
    // modelling gap.
    if !illegal.is_empty() {
        let total: usize = illegal.values().sum();
        eprintln!("[walk] {total} POISONED paths (a Lua raise the tracer could not rule out), by reason:");
        for (why, n) in &illegal {
            eprintln!("[walk]   {n} x {why}");
        }
    }
    // Poison counts raise-site hits; this is whether the condition survived.
    if !raising.is_empty() {
        eprintln!(
            "[walk] {} of {} (shape, region) nodes have a LIVE raise row (`Frame::raise` did not fold to false)",
            raising.len(),
            nodes.len()
        );
    }
    let mut by_hash = std::collections::HashMap::new();
    for ((k, _), wf) in &frames {
        by_hash.insert(wf.frame.in_rt2.shape_hash_of(), k.clone());
    }
    Ok(LatticeWalk { lattice, reps, frames, cart: cart_data, cache, tracer: Tracer { it, reset, fr }, by_hash, start_key, ival_extra, start_ivals })
}

/// The interval inputs the level's widenings make (`widen::interval_input`:
/// `rem`, the level's widened objects); `rnd`-derived ones come on top
/// (`with_extra`).
fn boundary_ival<A: GraphArith>(st: &super::state::State<super::domain::Symbolic<A>>, opts: crate::abstraction::Level) -> Vec<super::iface::Path> {
    super::widen::slots_where(st, opts, super::widen::interval_input)
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

/// What one walk node is traced under.
struct WalkJob<A: GraphArith = Fixed> {
    node: WalkNode,
    st: super::state::State<super::domain::Symbolic<A>>,
    roots: Vec<super::iface::Path>,
    pin: Vec<(super::iface::Path, super::iface::Conc<A>)>,
    ival: Vec<super::iface::Path>,
    bounds: Bounds<A>,
    /// A non-start representative's heap constants, to rebase it.
    rebase: Option<std::collections::HashMap<u32, super::iface::Conc<A>>>,
}

/// A live outcome of a traced walk node, as the walk reads it.
struct WalkOutcome<A: GraphArith = Fixed> {
    key: String,
    constants: std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>,
    ival: Vec<super::iface::Path>,
    regions: Vec<Option<Region>>,
    st: super::state::State<super::domain::Symbolic<A>>,
    /// `shapes::heap_constants` of `st`, in the worker's arena.
    heap_constants: std::collections::HashMap<u32, super::iface::Conc<A>>,
    /// A RESPAWN entry (a state of the no-player phase from a node outside
    /// it: the room reloaded after a death), its constants at the level
    /// (`explore_no_player` pins them).
    respawn: Option<std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>>,
}

/// One traced walk node, or why its trace refused.
enum WalkTraced<A: GraphArith = Fixed> {
    /// Why the trace did not compile, and what it poisoned before refusing.
    Refused(String, std::collections::BTreeMap<String, usize>),
    Traced {
        frame: super::verify::Frame<A>,
        bound: std::result::Result<Bound<A>, String>,
        nodes_added: usize,
        arena: usize,
        outs: Vec<WalkOutcome<A>>,
        skipped: Vec<&'static str>,
        /// Paths this trace POISONED, by reason (drained per job).
        illegal: std::collections::BTreeMap<String, usize>,
        /// `Frame::raise` did not fold to `false`.
        can_raise: bool,
    },
}

/// A walk node's converged frame, its bounds, and its binding (or why not).
pub struct WalkFrame<A: GraphArith = Fixed> {
    pub frame: super::verify::Frame<A>,
    pub bounds: Bounds<A>,
    pub bound: std::result::Result<Bound<A>, String>,
}

/// Trace one walk node on a worker's tracer, in that worker's arena.
fn walk_trace<A: GraphArith>(
    tr: &mut Tracer<A>,
    job: &WalkJob<A>,
    opts: crate::abstraction::Level,
    grid: Option<RegionGrid>,
    only: Option<&[Region]>,
    room0: (i16, i16),
) -> Result<WalkTraced<A>> {
    use super::domain::Domain;
    use super::{shapes, verify};
    // `illegal` accumulates per worker, so both `WalkTraced` exits DRAIN it.
    debug_assert!(tr.it.illegal.is_empty(), "a previous job left {} poison reasons behind", tr.it.illegal.len());
    tr.it.illegal.clear();
    let mut st = job.st.clone();
    if let Some(constants) = &job.rebase {
        shapes::rebase(&mut st, &mut tr.it.d, constants)?;
    }
    let f = match verify::trace_frame(&mut tr.it, tr.reset, tr.fr, st, &job.roots, &job.pin, &job.ival, true, &job.bounds, &[]) {
        Ok(f) => f,
        // A refused trace's poisoned paths count too.
        Err(e) => return Ok(WalkTraced::Refused(format!("{:#}", e), std::mem::take(&mut tr.it.illegal))),
    };
    let nodes_added = tr.it.d.node_count().saturating_sub(tr.it.trace_start_nodes);
    let mut outs = Vec::new();
    let mut skipped = Vec::new();
    for o in &f.outs {
        // A non-constant room (-1) may hold this room's states: an error,
        // not skipped.
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
            let widened = super::widen::slots_where(&o.st, opts, |s| super::widen::interval_input(s) || super::widen::stores_widened(s));
            for p in shapes::state_paths(&o.st)? {
                if widened.contains(&p) {
                    continue;
                }
                if let Some(super::heap::Value::Num(n)) = super::iface::get(&o.st, &p) {
                    if tr.it.d.is_interval(&n) {
                        ival.push(p);
                    }
                }
            }
        }
        let regions = successor_regions(&o.st, &mut tr.it.d, grid, only)?;
        let heap_constants = shapes::heap_constants(&o.st, &tr.it.d);
        let spawn_phase = |st: &super::state::State<super::domain::Symbolic<A>>| shapes::player_path(st).is_none() && !super::widen::objects_of_type(st, "player_spawn").is_empty();
        let respawn = if grid.is_some() && spawn_phase(&o.st) && super::widen::objects_of_type(&job.st, "player_spawn").is_empty() {
            Some(shapes::field_constants(&o.st, &tr.it.d, &opts)?)
        } else {
            None
        };
        outs.push(WalkOutcome { key, constants, ival, regions, st: o.st.clone(), heap_constants, respawn });
    }
    let bound = super::emit::bind(&f, &tr.it.d.graph).map_err(|e| format!("{:#}", e));
    let illegal = std::mem::take(&mut tr.it.illegal);
    // Decided HERE: `f.raise` names a node of this worker's arena.
    let can_raise = tr.it.d.decide(&f.raise) != Some(false);
    Ok(WalkTraced::Traced { frame: f, bound, nodes_added, arena: tr.it.d.node_count(), outs, skipped, illegal, can_raise })
}

/// The tracer a walk ran in, kept to re-trace a shape in the same arena.
#[derive(Clone)]
pub struct Tracer<A: GraphArith = Fixed> {
    pub it: super::interp::Interp<'static, super::domain::Symbolic<A>>,
    pub reset: &'static full_moon::ast::Ast,
    pub fr: &'static full_moon::ast::Ast,
}

// SAFETY: the raw pointers inside point into the leaked `'static` ASTs,
// immutable and never freed, so a copy can move to a worker.
unsafe impl<A: GraphArith> Send for Tracer<A> {}

/// What a lattice walk runs under: the run's settings (the cart, the room,
/// the region grid, `--build-trace`) and, for a test, `only` the player
/// regions it traces (`None`: every region a state can be in).
pub struct Walk<'a> {
    pub settings: &'a crate::settings::Settings,
    pub only: Option<&'a [Region]>,
}

/// The output of `room_constant_lattice`.
pub struct LatticeWalk<A: GraphArith = Fixed> {
    pub tracer: Tracer<A>,
    /// Shape hash (of the input structure) -> the walk's shape key.
    pub by_hash: std::collections::HashMap<u64, String>,
    /// The start state's shape key.
    pub start_key: String,
    pub lattice: std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>>,
    pub reps: std::collections::BTreeMap<String, super::state::State<super::domain::Symbolic<A>>>,
    /// Per (shape, region), its converged frame, bounds and binding.
    pub frames: std::collections::BTreeMap<WalkNode, WalkFrame<A>>,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    /// Per shape, input slots a reachable frame wrote an `rnd`-derived
    /// interval to, typed as interval inputs. Monotone.
    pub ival_extra: std::collections::BTreeMap<String, std::collections::BTreeSet<super::iface::Path>>,
    /// The start state's own intervals by path, the literal or `None` (the
    /// representative holds a blanked point there; level -1 seeds from this).
    /// A path absent here holds its start value in the representative, even
    /// where a later frame writes an interval to it (`ival_extra`).
    pub start_ivals: std::collections::BTreeMap<super::iface::Path, Option<(A::Lit, A::Lit)>>,
}
