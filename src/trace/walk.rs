//! The constant-lattice WALK over a room's (shape, region) nodes: every frame
//! a kernel is built from (`lower::lattice_kernel_refs` binds and lowers them),
//! with the no-player phase's ranges and the region key.

use anyhow::Result;

use celeste_core::arith::{Arithmetic, Fixed};

use super::lower::Bound;
use crate::transpile::garith::GraphArith;

/// THE REGION KEY: a kernel is specialized on the player's whole-pixel
/// position in one `px`-pixel grid square and speed in `[-speed, speed]`, so
/// the range analysis folds away far-off collision tests. Each bound is an
/// `Op::Restrict` on its input cell, whose own error is the per-lane check:
/// `x`/`y` hold by the dispatch, speed and remainder only by that check.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, serde::Serialize, serde::Deserialize)]
pub struct Region {
    pub ix: i16,
    pub iy: i16,
}

/// The grid a region key is taken on (`--region "px,S"`, `off` for none,
/// default 16 px / speed 6). `S <= 7` because the move loop unrolls for
/// `abs(amount) <= 8` and `amount = flr(rem + spd + 0.5)`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RegionGrid {
    pub px: i32,
    pub speed: i32,
}

impl RegionGrid {
    pub fn of(&self, x: i32, y: i32) -> Region {
        Region { ix: x.div_euclid(self.px) as i16, iy: y.div_euclid(self.px) as i16 }
    }

    /// The region of a row's input cell; `None` for a row without a player.
    pub fn of_cell(&self, cell: u32) -> Option<Region> {
        crate::storage::cells::cell_xy(cell).map(|(x, y)| self.of(x, y))
    }

    /// The bounds a region's kernel is traced under, by path under `pl` (16.16
    /// constants, exact in every arithmetic).
    fn bounds<A: GraphArith>(&self, pl: &super::iface::Path, r: Region) -> InputRanges<A> {
        const ONE: i32 = 1 << 16;
        let lit = |raw: i32| A::lit(A::from_fixed_raw(raw));
        let (rlo, rhi) = celeste_engine::widening::range::<A>(celeste_engine::widening::REM.0, celeste_engine::widening::REM.1);
        let rem = (A::lit(rlo), A::lit(rhi));
        let field = |fs: &[&str]| super::widen::field(pl, fs);
        let (x0, y0) = (r.ix as i32 * self.px, r.iy as i32 * self.px);
        vec![
            (field(&["x"]), (lit(x0 * ONE), lit((x0 + self.px - 1) * ONE))),
            (field(&["y"]), (lit(y0 * ONE), lit((y0 + self.px - 1) * ONE))),
            (field(&["spd", "x"]), (lit(-self.speed * ONE), lit(self.speed * ONE))),
            (field(&["spd", "y"]), (lit(-self.speed * ONE), lit(self.speed * ONE))),
            // `move` leaves `rem` in `[-0.5, 0.5)`; bounding it bounds the move.
            (field(&[super::game::REM, "x"]), rem),
            (field(&[super::game::REM, "y"]), rem),
        ]
    }
}

pub type InputRanges<A = Fixed> = Vec<(super::iface::Path, (<A as Arithmetic>::Lit, <A as Arithmetic>::Lit))>;

/// Frames of platform motion the platform worlds cover: a platforms-unknown
/// level is sound only up to this horizon and refuses a longer search.
pub const PLATFORM_WORLD_FRAMES: usize = 127;

/// A walk SHAPE: its block's shape hash (`Rt2::shape_hash`), what the
/// kernels are keyed by.
pub type ShapeKey = u64;

pub type WalkNode = (ShapeKey, Option<Region>);

/// The shape key of a state (its skeleton's shape hash).
pub fn shape_key<A: GraphArith>(st: &super::state::State<super::domain::Symbolic<A>>, cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>) -> Result<ShapeKey> {
    Ok(super::block::skeleton::<_, A>(st, cache)?.shape_hash)
}

/// The walk's REPRESENTATIVES: a shape's state as a one-lane block
/// (`block::to_rt2`), every scalar a constant, the state's own (what a trace
/// symbolizes) blanked but at the start. A block is the same state in every
/// arena: a trace starts from its `from_block`, the heap the reference engine
/// reads.
struct Reps<A: GraphArith> {
    fn_info: super::block::FnInfo,
    /// The boot state: the builtins a block drops.
    base: super::state::State<super::domain::Symbolic<A>>,
    cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    blocks: std::collections::BTreeMap<ShapeKey, celeste_engine::runtime2::Rt2<A>>,
}

impl<A: GraphArith> Reps<A> {
    fn new(start: &super::state::State<super::domain::Symbolic<A>>, cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>) -> Result<Self> {
        Ok(Reps { fn_info: super::block::fn_info_of(start)?, base: start.clone(), cache, blocks: Default::default() })
    }

    /// `st` as a representative (`blanked`: its state paths 0 or false).
    fn block(&self, st: &super::state::State<super::domain::Symbolic<A>>, d: &mut super::domain::Symbolic<A>, blanked: bool) -> Result<celeste_engine::runtime2::Rt2<A>> {
        use super::domain::Domain as _;
        use celeste_engine::runtime2::AV;
        let mut st = st.clone();
        if blanked {
            super::shapes::blank(&mut st, d)?;
        }
        let d = &*d;
        super::block::to_rt2(
            &st,
            self.cache.clone(),
            |n| d.as_const(n).map(AV::Num).ok_or_else(|| anyhow::anyhow!("a representative holds a number that is no constant")),
            |b| d.decide(b).map(AV::Bool).ok_or_else(|| anyhow::anyhow!("a representative holds a boolean that is no constant")),
        )
    }

    /// A representative as a state in `d`'s arena.
    fn state(&self, rep: &celeste_engine::runtime2::Rt2<A>, d: &mut super::domain::Symbolic<A>) -> Result<super::state::State<super::domain::Symbolic<A>>> {
        let (st, unknown) = super::block::from_block(d, rep, 0, &self.fn_info, &self.base)?;
        anyhow::ensure!(unknown.is_empty(), "a representative holds unknown booleans: {unknown:?}");
        Ok(st)
    }
}

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
    for (axis, name) in super::game::AXES.into_iter().enumerate() {
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
/// each numeric field to the hull it took, PER SHAPE (a later spawn shape
/// traced unbounded makes every fall floor "near"). The chain runs the fly
/// fruit exact; at the level's widenings the phase reaches shapes the chain
/// never takes, so the start and every respawn the walk reaches are also
/// explored (`explore_no_player`) into the same hulls. A shape no explored
/// state has gets the spawn object's hull by type (`NoPlayer::bounds`). A
/// lane outside declines.
fn no_player_ranges<A: GraphArith>(
    it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
    chunks: &'static super::cart::Chunks,
    reps: &Reps<A>,
    start: &super::state::State<super::domain::Symbolic<A>>,
    opts: crate::abstraction::Level,
) -> Result<NoPlayer<A>> {
    use super::domain::Domain as _;
    use super::shapes;
    if shapes::player_path(start).is_some() {
        return Ok(NoPlayer::default());
    }
    let mut np = NoPlayer::default();
    // The chain runs with the fly fruit EXACT (unknown, its `fly` and `y` fork
    // every frame into outcomes); the fruit's fields bound nothing then.
    let fruit = it.d.level.fruit;
    let chain_opts = crate::abstraction::Level { fruit: false, ..opts };
    let chain_level = crate::abstraction::Level { fruit: false, ..it.d.level };
    // Only a phase run to its END, concretely, bounds anything. Two things
    // stop that and end the chain bounding nothing (said in the log): a start
    // holding an `rnd` draw (a balloon's `offset` without `--balloon-seeds`)
    // and a frame whose unpinned fields (the level's widened ones) fork it.
    // Every other refusal is an error.
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
        let mut key = shape_key(start, reps.cache.clone())?;
        // Read before the representative blanks them; the next trace pins them back.
        let mut consts = shapes::field_constants(start, &it.d, &chain_opts)?;
        for k in 0..NO_PLAYER_FRAMES {
            np.note(key, &st, &consts, fruit)?;
            let roots = shapes::state_paths(&st)?;
            let pin = pins_of(&consts, &roots);
            let spec = super::frame::TraceSpec { roots: &roots, pin: &pin, level: Some(chain_level), ..Default::default() };
            let f = super::frame::trace_frame(it, chunks, st.clone(), &spec).map_err(|e| e.context(format!("the start's no-player chain, frame {k}")))?;
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
            (key, st, consts) = next_entry(reps, &mut it.d, &o.st, &chain_opts)?;
        }
        anyhow::bail!("the start's no-player phase did not reach the player within {NO_PLAYER_FRAMES} frames")
    })();
    if !run? {
        return Ok(NoPlayer::default());
    }
    np.on = true;
    let c = shapes::field_constants(start, &it.d, &opts)?;
    let key = shape_key(start, reps.cache.clone())?;
    let (n, followed, ended) = explore_no_player(it, chunks, reps, (key, start.clone(), c), opts, &mut np, "the start's")?;
    eprintln!("[walk] the start explored: {n} states explored, {followed} outcomes followed, {ended} reach the player");
    Ok(np)
}

/// Frames a no-player phase may last; past them it is FATAL.
const NO_PLAYER_FRAMES: usize = 240;

/// THE NO-PLAYER EXPLORATION: from an entry of the no-player phase (the
/// start, or a respawn: the room reloaded after a death), every outcome of
/// every frame up to the player's appearance, AT THE LEVEL'S OWN WIDENINGS
/// (`opts`), into `np`'s hulls. Fields the entry holds unknown (a respawn's
/// `freeze` and `delay_restart`, the level's widened fields) stay unknown
/// inputs. No merge selects on the state's OWN unknowns: `if freeze>0 then
/// ... return end` would merge the frozen branch into the spawn's fields
/// (`y = Sel(freeze > 0, 124, 120)`), and none would be a constant. A state
/// is its shape and its constants (what the trace pins), so a repeated one is
/// explored once. FATAL (`what`: whose phase): a state still without the
/// player after `NO_PLAYER_FRAMES` frames, a state that does not trace.
/// Returns the states explored, the outcomes followed and those where the
/// player appears.
fn explore_no_player<A: GraphArith>(
    it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
    chunks: &'static super::cart::Chunks,
    reps: &Reps<A>,
    entry: Entry<A>,
    opts: crate::abstraction::Level,
    np: &mut NoPlayer<A>,
    what: &str,
) -> Result<(usize, usize, usize)> {
    use super::shapes;
    let fruit = it.d.level.fruit;
    let mut seen: std::collections::BTreeSet<StateId> = Default::default();
    let (key, st, consts) = entry;
    let mut todo = vec![(key, st, consts, 0usize)];
    let (mut states, mut followed, mut ended) = (0usize, 0usize, 0usize);
    while let Some((key, st, consts, depth)) = todo.pop() {
        if !seen.insert(state_id(key, &consts)) {
            continue;
        }
        states += 1;
        np.note(key, &st, &consts, fruit)?;
        anyhow::ensure!(depth < NO_PLAYER_FRAMES, "{what} no-player phase did not reach the player within {NO_PLAYER_FRAMES} frames");
        let roots = shapes::state_paths(&st)?;
        let pin = pins_of(&consts, &roots);
        // The state's own unknowns (not the level's widened fields, not the
        // buttons the frame resets): no merge selects on them.
        let widened = super::widen::slots_where(&st, opts, super::widen::stores_widened);
        let buttons = super::iface::key(super::game::BUTTONS);
        let unknown: Vec<super::iface::Path> = roots.iter().filter(|p| !consts.contains_key(*p) && !widened.contains(*p) && p.first() != Some(&buttons)).cloned().collect();
        let spec = super::frame::TraceSpec { roots: &roots, pin: &pin, merge_refused_on: &unknown, ..Default::default() };
        let f = super::frame::trace_frame(it, chunks, st, &spec).map_err(|e| e.context(format!("{what} no-player state {depth} frames after its entry did not trace")))?;
        for o in &f.outs {
            followed += 1;
            if shapes::player_path(&o.st).is_some() {
                ended += 1;
                continue;
            }
            let (k, next, c) = next_entry(reps, &mut it.d, &o.st, &opts)?;
            todo.push((k, next, c, depth + 1));
        }
    }
    Ok((states, followed, ended))
}

/// The constants of `consts` at input slots of `roots`: a trace's pins.
fn pins_of<A: GraphArith>(consts: &std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>, roots: &[super::iface::Path]) -> Vec<(super::iface::Path, super::iface::Conc<A>)> {
    consts.iter().filter(|(p, _)| roots.contains(p)).map(|(p, c)| (p.clone(), *c)).collect()
}

/// A no-player state to explore: its shape, the state, its constants.
type Entry<A> = (ShapeKey, super::state::State<super::domain::Symbolic<A>>, std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>);

/// A constant by its exact bits (`Arithmetic::bits`): two constants are
/// one exactly when their bits are.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
enum ConstBits {
    Num(u64),
    Bool(bool),
}

/// A no-player state's IDENTITY: its shape and every constant, by path and
/// exact bits (never a rendering of them: what dedups the exploration must
/// be what tells the states apart).
type StateId = (ShapeKey, Vec<(super::iface::Path, ConstBits)>);

fn state_id<A: GraphArith>(shape: ShapeKey, consts: &std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>) -> StateId {
    let bits = |c: &super::iface::Conc<A>| match *c {
        super::iface::Conc::Num(n) => ConstBits::Num(A::bits(n)),
        super::iface::Conc::Bool(b) => ConstBits::Bool(b),
    };
    (shape, consts.iter().map(|(p, c)| (p.clone(), bits(c))).collect())
}

/// The next frame's entry from outcome `st`: its constants at `opts` (read
/// before the representative blanks them) and its representative's state.
fn next_entry<A: GraphArith>(reps: &Reps<A>, d: &mut super::domain::Symbolic<A>, st: &super::state::State<super::domain::Symbolic<A>>, opts: &crate::abstraction::Level) -> Result<Entry<A>> {
    let consts = super::shapes::field_constants(st, d, opts)?;
    let rep = reps.block(st, d, true)?;
    Ok((rep.shape_hash, reps.state(&rep, d)?, consts))
}

/// A numeric field's hull per path (raw 16.16, inclusive).
type Hull<A> = std::collections::BTreeMap<super::iface::Path, (A, A)>;

/// The no-player phase's hulls over every state explored (the start's chain
/// and each respawn's exploration), per shape and the spawn object's. A
/// field bounds its shape where its hull is not one value and every
/// explored state of the shape holds it a constant.
#[derive(Default)]
struct NoPlayer<A: GraphArith = Fixed> {
    /// The start's chain reached the player: the hulls bound.
    on: bool,
    /// Per shape: each numeric field's hull over the states where it was a
    /// constant, and the fields some state held unknown.
    shapes: Vec<(ShapeKey, Hull<A>, std::collections::BTreeSet<super::iface::Path>)>,
    /// The spawn object's fields (by the path below it), likewise.
    spawn: Hull<A>,
    spawn_unknown: std::collections::BTreeSet<super::iface::Path>,
    entries: std::collections::BTreeSet<StateId>,
}

impl<A: GraphArith> NoPlayer<A> {
    /// An explored state into the hulls (`fruit`: the level widens the fly
    /// fruit, whose fields bound nothing).
    fn note(&mut self, shape: ShapeKey, st: &super::state::State<super::domain::Symbolic<A>>, consts: &std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>, fruit: bool) -> Result<()> {
        let at = match self.shapes.iter().position(|(s, ..)| *s == shape) {
            Some(i) => i,
            None => {
                self.shapes.push((shape, Hull::<A>::new(), Default::default()));
                self.shapes.len() - 1
            }
        };
        let fruit_objs = if fruit { super::widen::objects_of_type(st, "fly_fruit") } else { Vec::new() };
        let spawn_objs = super::widen::objects_of_type(st, super::game::PLAYER_SPAWN);
        let join = |h: &mut Hull<A>, p: super::iface::Path, r: A| {
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

    /// A no-player state's bounds, by path: its shape's hulls, else the spawn
    /// object's. A varying field some explored state holds unknown bounds
    /// nothing - FATAL for a field of the spawn, the phase the bounds are for.
    fn bounds(&self, shape: ShapeKey, st: &super::state::State<super::domain::Symbolic<A>>) -> Result<InputRanges<A>> {
        let spawn_objs = super::widen::objects_of_type(st, super::game::PLAYER_SPAWN);
        let varying = |h: &Hull<A>, unknown: &std::collections::BTreeSet<super::iface::Path>, spawn: &dyn Fn(&super::iface::Path) -> bool, what: &str| -> Result<InputRanges<A>> {
            let mut b = InputRanges::<A>::new();
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
        let mut out = InputRanges::<A>::new();
        for o in spawn_objs {
            for (suffix, r) in &spawn {
                let mut p = o.clone();
                p.extend(suffix.iter().cloned());
                out.push((p, *r));
            }
        }
        Ok(out)
    }

    /// Each shape's bounds and the spawn object's, for the log.
    fn show(&self) -> Vec<String> {
        let show = |h: &Hull<A>, unknown: &std::collections::BTreeSet<super::iface::Path>| h.iter().filter(|(p, (lo, hi))| A::lt(*lo, *hi) && !unknown.contains(*p)).map(|(p, (a, b))| format!("{} [{:.2}, {:.2}]", super::iface::show(p), celeste_core::arith::Arithmetic::to_f64(*a), celeste_core::arith::Arithmetic::to_f64(*b))).collect::<Vec<_>>();
        let mut out: Vec<String> = self.shapes.iter().enumerate().map(|(i, (_, h, unknown))| format!("[walk] no-player shape {i} ranges: {:?}", show(h, unknown))).collect();
        out.push(format!("[walk] the spawn object's ranges: {:?}", show(&self.spawn, &self.spawn_unknown)));
        out
    }
}

#[cfg(test)]
mod tests {
    /// A NO-PLAYER STATE IS ITS SHAPE AND ITS CONSTANTS' BITS: a double's -0
    /// and 0 (equal to the cart's `==`) are two states, and equal bits one.
    #[test]
    fn a_no_player_states_identity_is_its_constants_bits() {
        use super::super::iface::{Conc, Step};
        use celeste_core::arith::Float;
        let at = |v: f64| std::collections::BTreeMap::from([(vec![Step::Key("x".into())], Conc::Num(Float::new(v).expect("finite")))]);
        assert!(super::state_id(1, &at(0.0)) != super::state_id(1, &at(-0.0)));
        assert!(super::state_id(1, &at(0.5)) == super::state_id(1, &at(0.5)));
        assert!(super::state_id(1, &at(0.5)) != super::state_id(2, &at(0.5)));
    }

    /// THE NO-PLAYER CHAIN ENDS QUIETLY ONLY WHERE IT CANNOT RUN CONCRETELY.
    /// Room (5,0)'s balloon draws its `offset` from `rnd` in `_init`: the
    /// chain bounds nothing, and says so. Any other refusal of a chain frame
    /// (here the graph budget) is an error, never "no bounds".
    #[test]
    fn the_no_player_chain_refuses_loudly_but_for_an_rnd_draw() {
        use crate::settings::Settings;
        let level = crate::abstraction::Level::parse("r0sxh").expect("level");
        let chain = |s: Settings, max_nodes: Option<usize>| {
            let (mut tr, start) = super::boot::<celeste_core::arith::Fixed>(&s, level).expect("boot");
            tr.it.max_nodes = max_nodes.unwrap_or(tr.it.max_nodes);
            let reps = super::Reps::new(&start, tr.it.cache.clone().expect("boot loads the collision cache")).expect("the start's functions");
            super::no_player_ranges(&mut tr.it, tr.chunks, &reps, &start, level)
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
        let s = crate::settings::Settings { room: (6, 1), ..Default::default() };
        let opts = crate::abstraction::Level { held: true, floors_near: true, ..crate::abstraction::Level::EXACT };
        let only = [super::Region { ix: 3, iy: 6 }, super::Region { ix: 4, iy: 6 }];
        let lw: super::LatticeWalk = super::room_constant_lattice(opts, &super::Walk { settings: &s, only: Some(&only) }).unwrap_or_else(|e| panic!("{e:#}"));
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
    /// that shape. Every no-player node's bound on a spawn's `y` covers 124.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (0,3)'s lattice walk, ~40 CPU-s"]
    fn a_respawn_after_the_loading_jank_is_inside_its_shapes_bounds() {
        use super::super::{iface, shapes, widen};
        let s = crate::settings::Settings { room: (0, 3), category: crate::settings::Category::Gemskip100, loading_jank: Some(1), ..Default::default() };
        let opts = crate::abstraction::Level::parse("r0sxh").expect("level");
        let only: Vec<super::Region> = (0..8).flat_map(|ix| (5..8).map(move |iy| super::Region { ix, iy })).collect();
        let lw: super::LatticeWalk = super::room_constant_lattice(opts, &super::Walk { settings: &s, only: Some(&only) }).unwrap_or_else(|e| panic!("{e:#}"));
        let mut bounded = 0;
        for ((shape, region), f) in &lw.frames {
            let rep = &lw.reps[shape];
            if region.is_some() || shapes::player_path(rep).is_some() {
                continue;
            }
            for o in widen::objects_of_type(rep, super::super::game::PLAYER_SPAWN) {
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

/// The walk's tracer at room `s.room`'s start, every frame traced at `opts`.
fn boot<A: GraphArith>(s: &crate::settings::Settings, opts: crate::abstraction::Level) -> Result<(Tracer<A>, super::state::State<super::domain::Symbolic<A>>)> {
    let chunks = super::cart::Chunks::parse(s)?.leak();
    let mut it = super::interp::Interp::new(super::domain::Symbolic::<A>::default());
    it.d.build_trace = s.diag.build_trace;
    it.d.level = opts;
    if opts.platforms {
        it.d.worlds = Some(std::sync::Arc::new(crate::concrete::platform_worlds::<A>(PLATFORM_WORLD_FRAMES, s)?));
    }
    let start = super::cart::boot(&mut it, chunks, s.room)?;
    Ok((Tracer { it, chunks }, start))
}

/// The per-shape constant lattice by FIXPOINT over (shape, region) nodes:
/// trace forward from the spawn with each shape's known constants PINNED,
/// INTERSECT each outcome's constants into its shape's lattice, re-trace a
/// shape that shrinks. Monotone (constant -> abstract only), so it
/// terminates. Every frame is traced under `opts`, the kernels' level.
pub fn room_constant_lattice<A: GraphArith>(
    opts: crate::abstraction::Level,
    walk: &Walk,
) -> Result<LatticeWalk<A>> {
    let s = walk.settings;
    let (Tracer { mut it, chunks }, start) = boot::<A>(s, opts)?;
    let (mut w, mut frontier) = WalkState::start(&mut it, chunks, &start, opts, walk)?;
    // THE ROUNDS: each traces its frontier in parallel against a lattice
    // snapshot; results apply in node order between rounds. Each frame is
    // bound in the worker arena it was traced in.
    let n_workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut tracers: Vec<Tracer<A>> = (0..n_workers).map(|_| Tracer { it: it.clone(), chunks }).collect();
    let (mut traces, mut round) = (0usize, 0usize);
    let t_walk = std::time::Instant::now();
    while !frontier.is_empty() {
        round += 1;
        traces += frontier.len();
        anyhow::ensure!(traces <= 20000, "constant-lattice fixpoint did not converge");
        let t_round = std::time::Instant::now();
        let jobs = w.jobs(&frontier)?;
        // Each worker clones its own copy (a `Tracer` is `Send`, not `Sync`), and
        // per job a FRESH arena from it, so a graph depends on the job, not on
        // scheduling.
        let free = std::sync::Mutex::new(tracers.iter_mut().collect::<Vec<_>>());
        let traced: Vec<WalkTraced<A>> = crate::par::Pool::new(n_workers).stack(128 << 20).map_with(
            jobs.len(),
            || free.lock().expect("the walk's tracers").pop().ok_or_else(|| anyhow::anyhow!("a tracer per walk worker")),
            |tr, i| walk_trace(&mut tr.clone(), &w.reps, &jobs[i], opts, w.grid, walk.only, w.room0),
        )?;
        let (mut next, respawned) = w.apply(&mut it, chunks, &jobs, traced)?;
        if respawned {
            w.respawned()?;
        }
        // THE DEFERRED RE-TRACE: once no new region appears, re-trace each
        // narrowed shape in every region it reached. Sound: pins only drop,
        // and the re-trace repeats until nothing changes.
        if next.is_empty() {
            for k in std::mem::take(&mut w.dirty) {
                next.extend(w.regions.get(&k).into_iter().flatten().map(|r| (k, *r)));
            }
        }
        if w.trace {
            eprintln!("[walk] round {round}: {} traced, {} next, {:.1} s", jobs.len(), next.len(), t_round.elapsed().as_secs_f64());
        }
        frontier = next.into_iter().collect();
    }
    w.finish(Tracer { it, chunks }, format!("{traces} traces in {round} rounds, {:.1} s ({n_workers} workers)", t_walk.elapsed().as_secs_f64()))
}

/// THE WALK between its rounds (`room_constant_lattice`).
struct WalkState<A: GraphArith> {
    opts: crate::abstraction::Level,
    grid: Option<RegionGrid>,
    /// The start's room: an outcome in another is a terminal.
    room0: (i16, i16),
    /// `--build-trace`: the per-trace `[lattice]` lines.
    trace: bool,
    reps: Reps<A>,
    no_player: NoPlayer<A>,
    start_key: ShapeKey,
    start_ivals: std::collections::BTreeMap<super::iface::Path, Option<(A::Lit, A::Lit)>>,
    /// Per shape, the constants every outcome reaching it agreed on.
    lattice: std::collections::BTreeMap<ShapeKey, std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>>,
    /// Each representative's state in the walk's own arena (what a job's
    /// paths, pins and bounds are read off).
    states: std::collections::BTreeMap<ShapeKey, super::state::State<super::domain::Symbolic<A>>>,
    /// The regions each shape was reached in.
    regions: std::collections::BTreeMap<ShapeKey, std::collections::BTreeSet<Option<Region>>>,
    frames: std::collections::BTreeMap<WalkNode, WalkFrame<A>>,
    refused: std::collections::BTreeMap<WalkNode, String>,
    /// Poisoned paths by reason, summed over jobs.
    illegal: std::collections::BTreeMap<String, usize>,
    /// The nodes whose `Frame::raise` did not fold to false.
    raising: std::collections::BTreeSet<WalkNode>,
    ival_extra: std::collections::BTreeMap<ShapeKey, std::collections::BTreeSet<super::iface::Path>>,
    /// Shapes waiting for the deferred re-trace.
    dirty: std::collections::BTreeSet<ShapeKey>,
}

impl<A: GraphArith> WalkState<A> {
    /// The walk at the start state (its no-player ranges, its own interval
    /// inputs, its shape's representative, constants and regions), and the
    /// first round's frontier: the start's regions.
    fn start(
        it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
        chunks: &'static super::cart::Chunks,
        start: &super::state::State<super::domain::Symbolic<A>>,
        opts: crate::abstraction::Level,
        walk: &Walk,
    ) -> Result<(Self, Vec<WalkNode>)> {
        use super::domain::Domain as _;
        use super::shapes;
        let s = walk.settings;
        let cache = it.cache.clone().expect("boot loads the collision cache");
        let mut reps = Reps::new(start, cache)?;
        let no_player = match s.region_grid() {
            Some(_) => no_player_ranges(it, chunks, &reps, start, opts)?,
            None => NoPlayer::default(),
        };
        if s.diag.build_trace {
            for l in no_player.show() {
                eprintln!("{l}");
            }
        }
        // The START state's own intervals (an `rnd` draw in `_init`) are interval
        // inputs at every level: no traced frame wrote them. Its representative
        // holds 0 there.
        let mut start_ivals: std::collections::BTreeMap<super::iface::Path, Option<(A::Lit, A::Lit)>> = Default::default();
        let mut start_rep = start.clone();
        for r in shapes::state_paths(start)? {
            for p in super::iface::scalars(start, &r)? {
                let Some(super::heap::Value::Num(n)) = super::iface::get(start, &p) else { continue };
                if it.d.as_const(&n).is_some() {
                    continue;
                }
                // Under the loading jank the start's `rnd` draw has had one update (a
                // balloon's `offset += 0.01`, its `y` from `sin(offset)`): a function of a
                // literal. Its static range's hull stands for it, a superset of the
                // start's values, sound for level -1's lower bound.
                let lit = match it.d.graph.get(n).op {
                    crate::transpile::graph::Op::Const(lo, hi) => Some((lo, hi)),
                    _ => it.d.range_of(n).as_ref().and_then(A::lit_hull),
                };
                start_ivals.insert(p.clone(), lit);
                let zero = it.d.int(0);
                super::iface::set(&mut start_rep, &p, super::heap::Value::Num(zero))?;
            }
        }
        let start_rep = reps.block(&start_rep, &mut it.d, false)?;
        let sk = start_rep.shape_hash;
        let mut ival_extra: std::collections::BTreeMap<ShapeKey, std::collections::BTreeSet<super::iface::Path>> = Default::default();
        if !start_ivals.is_empty() {
            ival_extra.insert(sk, start_ivals.keys().cloned().collect());
        }
        let lattice = std::collections::BTreeMap::from([(sk, shapes::field_constants(start, &it.d, &opts)?)]);
        let states = std::collections::BTreeMap::from([(sk, reps.state(&start_rep, &mut it.d)?)]);
        reps.blocks.insert(sk, start_rep);
        let grid = s.region_grid();
        let start_regions = successor_regions(start, &mut it.d, grid, walk.only)?;
        let regions = std::collections::BTreeMap::from([(sk, start_regions.iter().copied().collect())]);
        let w = WalkState {
            opts,
            grid,
            room0: shapes::room_of(start, &it.d),
            trace: s.diag.build_trace,
            reps,
            no_player,
            start_key: sk,
            start_ivals,
            lattice,
            states,
            regions,
            frames: Default::default(),
            refused: Default::default(),
            illegal: Default::default(),
            raising: Default::default(),
            ival_extra,
            dirty: Default::default(),
        };
        Ok((w, start_regions.into_iter().map(|r| (sk, r)).collect()))
    }

    /// The shape number `k` has in the lattice (`--build-trace`'s lines).
    fn shape_no(&self, k: ShapeKey) -> usize {
        self.lattice.keys().position(|x| *x == k).unwrap_or(usize::MAX)
    }

    /// The round's jobs: each frontier node's paths, pins, interval inputs
    /// and bounds (its region's square, or the no-player hull).
    fn jobs(&self, frontier: &[WalkNode]) -> Result<Vec<WalkJob<A>>> {
        use super::shapes;
        frontier
            .iter()
            .map(|(k, region)| -> Result<WalkJob<A>> {
                let st = &self.states[k];
                let roots = shapes::state_paths(st)?;
                let ival = with_extra(super::widen::slots_where(st, self.opts, super::widen::interval_input), self.ival_extra.get(k));
                let pin = pins_of(&self.lattice[k], &roots);
                let bounds: InputRanges<A> = match (self.grid, *region, shapes::player_path(st)) {
                    (Some(g), Some(r), Some(pl)) => {
                        g.bounds::<A>(&pl, r).into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect()
                    }
                    (None, None, _) => Vec::new(),
                    // The no-player phase's hull (bounds are by path).
                    (Some(_), None, None) => self.no_player.bounds(*k, st)?.into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect(),
                    (g, r, pl) => anyhow::bail!("walk node {r:?} of a shape {} a player under grid {g:?}", if pl.is_some() { "with" } else { "without" }),
                };
                let spawn = !super::widen::objects_of_type(st, super::game::PLAYER_SPAWN).is_empty();
                Ok(WalkJob { node: (*k, *region), roots, pin, ival, bounds, spawn })
            })
            .collect()
    }

    /// The round's results, in node order: each node's frame, and each
    /// outcome into its shape's lattice, interval inputs and regions (a
    /// respawn explored into the no-player hulls). Returns the regions new
    /// this round, and whether a respawn was explored.
    fn apply(
        &mut self,
        it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
        chunks: &'static super::cart::Chunks,
        jobs: &[WalkJob<A>],
        traced: Vec<WalkTraced<A>>,
    ) -> Result<(std::collections::BTreeSet<WalkNode>, bool)> {
        let mut next: std::collections::BTreeSet<WalkNode> = Default::default();
        let mut respawned = false;
        for (job, t) in jobs.iter().zip(traced) {
            let (k, region) = job.node;
            match t {
                WalkTraced::Refused(e, poisoned) => {
                    for (why, n) in poisoned {
                        *self.illegal.entry(why).or_default() += n;
                    }
                    // A node that never traces is a MISSING KERNEL (an error
                    // after the fixpoint). Its earlier frame goes too: it
                    // was traced under pins since dropped.
                    self.refused.insert(job.node, e);
                    self.frames.remove(&job.node);
                }
                WalkTraced::Traced { frame, bound, nodes_added, arena, outs, skipped, illegal: poisoned, can_raise } => {
                    for (why, n) in poisoned {
                        *self.illegal.entry(why).or_default() += n;
                    }
                    if can_raise {
                        self.raising.insert(job.node);
                    }
                    self.refused.remove(&job.node);
                    if self.trace {
                        eprintln!(
                            "[lattice] trace shape #{} region {:?} ({} pinned): {} outcomes, {} nodes added ({} in the worker's arena)",
                            self.shape_no(k),
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
                        respawned |= self.outcome(it, chunks, (k, region), o, &mut next)?;
                    }
                    // The frame binds the block its representative is.
                    anyhow::ensure!(frame.in_rt2.shape_hash_of() == k, "shape {k:#x} region {region:?}: the traced frame's input block has shape {:#x}", frame.in_rt2.shape_hash_of());
                    self.frames.insert(job.node, WalkFrame { frame, bounds: job.bounds.clone(), bound });
                }
            }
        }
        Ok((next, respawned))
    }

    /// One live outcome of node `from` (`apply`): new regions into `next`,
    /// a narrowed shape `dirty`. True if it was a respawn entry, explored.
    fn outcome(
        &mut self,
        it: &mut super::interp::Interp<'static, super::domain::Symbolic<A>>,
        chunks: &'static super::cart::Chunks,
        (k, region): WalkNode,
        o: WalkOutcome<A>,
        next: &mut std::collections::BTreeSet<WalkNode>,
    ) -> Result<bool> {
        let tk = o.key;
        let mut respawned = false;
        // A RESPAWN entry: its no-player phase explored into the hulls (once
        // per entry); the shapes whose bounds grew are re-traced (`respawned`).
        if let (true, Some(c)) = (self.no_player.on, &o.respawn) {
            if self.no_player.entries.insert(state_id(tk, c)) {
                let entry = self.reps.state(&o.rep, &mut it.d)?;
                let (n, followed, ended) = explore_no_player(it, chunks, &self.reps, (tk, entry, c.clone()), self.opts, &mut self.no_player, "a respawn's")?;
                let shape_no = |x: &ShapeKey| self.lattice.keys().position(|y| y == x).map_or("new".to_string(), |i| format!("#{i}"));
                eprintln!(
                    "[walk] respawn entry {} (shape {}, from shape {} {:?}): {n} states explored, {followed} outcomes followed, {ended} reach the player",
                    self.no_player.entries.len(),
                    shape_no(&tk),
                    shape_no(&k),
                    region
                );
                respawned = true;
            }
        }
        // Intervals beyond the boundary's widenings: inputs next frame.
        let mut new_ival = false;
        for p in o.ival {
            if self.trace && !self.ival_extra.get(&tk).is_some_and(|s| s.contains(&p)) {
                eprintln!("[lattice]   outcome: new interval slot {} (shape #{})", super::iface::show(&p), self.shape_no(tk));
            }
            new_ival |= self.ival_extra.entry(tk).or_default().insert(p);
        }
        if self.trace {
            let known = self.lattice.get(&tk).map(|m| m.iter().filter(|(p, v)| o.constants.get(*p) != Some(*v)).map(|(p, _)| super::iface::show(p)).collect::<Vec<_>>());
            match known {
                None => eprintln!("[lattice]   outcome: NEW shape, {} constants, from shape #{}", o.constants.len(), self.shape_no(k)),
                Some(gone) => eprintln!("[lattice]   outcome: shape #{}, constants removed: {:?}", self.shape_no(tk), gone),
            }
        }
        let changed = match self.lattice.get_mut(&tk) {
            None => {
                self.lattice.insert(tk, o.constants);
                self.states.insert(tk, self.reps.state(&o.rep, &mut it.d)?);
                self.reps.blocks.insert(tk, o.rep);
                true
            }
            Some(m) => {
                let before = m.len();
                m.retain(|p, v| o.constants.get(p) == Some(v));
                m.len() != before
            }
        };
        // New regions trace next round; narrowed shapes later (`dirty`).
        let reached = self.regions.entry(tk).or_default();
        if changed || new_ival {
            self.dirty.insert(tk);
        }
        for r in o.regions {
            if reached.insert(r) {
                next.insert((tk, r));
            }
        }
        Ok(respawned)
    }

    /// A respawn grew the no-player hulls: a no-player node traced under
    /// bounds that since changed is re-traced (`dirty`).
    fn respawned(&mut self) -> Result<()> {
        use super::shapes;
        for ((k, region), wf) in &self.frames {
            if region.is_none() && shapes::player_path(&self.states[k]).is_none() {
                let roots = shapes::state_paths(&self.states[k])?;
                let now: InputRanges<A> = self.no_player.bounds(*k, &self.states[k])?.into_iter().filter(|(p, _)| roots.iter().any(|q| q == p)).collect();
                if now != wf.bounds {
                    self.dirty.insert(*k);
                }
            }
        }
        if self.trace {
            for l in self.no_player.show() {
                eprintln!("{l}");
            }
        }
        Ok(())
    }

    /// The fixpoint reached (`rounds`: what it took, for the log): every
    /// reachable node has its frame, or the walk fails.
    fn finish(self, tracer: Tracer<A>, rounds: String) -> Result<LatticeWalk<A>> {
        // Every reachable node needs a frame, or the gap shows only at runtime.
        let nodes: Vec<WalkNode> = self.regions.iter().flat_map(|(k, rs)| rs.iter().map(move |r| (*k, *r))).collect();
        let missing: Vec<String> = nodes
            .iter()
            .filter(|node| !self.frames.contains_key(*node))
            .map(|node| format!("shape {:#x} region {:?}: {}", node.0, node.1, self.refused.get(node).map(String::as_str).unwrap_or("never traced")))
            .collect();
        if !missing.is_empty() {
            anyhow::bail!(
                "the constant-lattice fixpoint could not trace {} of {} (shape, region) nodes:\n{}",
                missing.len(),
                nodes.len(),
                missing.join("\n")
            );
        }
        eprintln!("[walk] reached {} shapes in {} (shape, region) nodes: {rounds}", self.lattice.len(), nodes.len());
        // POISONED PATHS: a Lua type error not proven unreachable is a
        // modelling gap.
        if !self.illegal.is_empty() {
            let total: usize = self.illegal.values().sum();
            eprintln!("[walk] {total} POISONED paths (a Lua raise the tracer could not rule out), by reason:");
            for (why, n) in &self.illegal {
                eprintln!("[walk]   {n} x {why}");
            }
        }
        // Poison counts raise-site hits; this is whether the condition survived.
        if !self.raising.is_empty() {
            eprintln!(
                "[walk] {} of {} (shape, region) nodes have a LIVE raise row (`Frame::raise` did not fold to false)",
                self.raising.len(),
                nodes.len()
            );
        }
        let cache = self.reps.cache.clone();
        Ok(LatticeWalk { lattice: self.lattice, reps: self.states, frames: self.frames, cache, tracer, start_key: self.start_key, ival_extra: self.ival_extra, start_ivals: self.start_ivals })
    }
}

/// `ival` (the level's interval inputs) plus a shape's discovered
/// `rnd`-derived interval slots, each once.
fn with_extra(mut ival: Vec<super::iface::Path>, extra: Option<&std::collections::BTreeSet<super::iface::Path>>) -> Vec<super::iface::Path> {
    for p in extra.into_iter().flatten() {
        if !ival.contains(p) {
            ival.push(p.clone());
        }
    }
    ival
}

struct WalkJob<A: GraphArith = Fixed> {
    node: WalkNode,
    roots: Vec<super::iface::Path>,
    pin: Vec<(super::iface::Path, super::iface::Conc<A>)>,
    ival: Vec<super::iface::Path>,
    bounds: InputRanges<A>,
    /// The shape has a `player_spawn`.
    spawn: bool,
}

/// A live outcome of a traced walk node, as the walk reads it.
struct WalkOutcome<A: GraphArith = Fixed> {
    key: ShapeKey,
    constants: std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>,
    ival: Vec<super::iface::Path>,
    regions: Vec<Option<Region>>,
    /// The outcome as its shape's representative.
    rep: celeste_engine::runtime2::Rt2<A>,
    /// A RESPAWN entry (a state of the no-player phase from a node outside
    /// it: the room reloaded after a death), its constants at the level
    /// (`explore_no_player` pins them).
    respawn: Option<std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>>,
}

enum WalkTraced<A: GraphArith = Fixed> {
    /// The refusal, and what the trace poisoned before it.
    Refused(String, std::collections::BTreeMap<String, usize>),
    Traced {
        frame: super::frame::Frame<A>,
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

/// A walk node's converged frame, its bounds, and its binding.
pub struct WalkFrame<A: GraphArith = Fixed> {
    pub frame: super::frame::Frame<A>,
    pub bounds: InputRanges<A>,
    pub bound: std::result::Result<Bound<A>, String>,
}

fn walk_trace<A: GraphArith>(
    tr: &mut Tracer<A>,
    reps: &Reps<A>,
    job: &WalkJob<A>,
    opts: crate::abstraction::Level,
    grid: Option<RegionGrid>,
    only: Option<&[Region]>,
    room0: (i16, i16),
) -> Result<WalkTraced<A>> {
    use super::domain::Domain;
    use super::{frame, shapes};
    // `illegal` accumulates per worker, so both `WalkTraced` exits DRAIN it.
    debug_assert!(tr.it.illegal.is_empty(), "a previous job left {} poison reasons behind", tr.it.illegal.len());
    tr.it.illegal.clear();
    let st = reps.state(&reps.blocks[&job.node.0], &mut tr.it.d)?;
    let spec = frame::TraceSpec { roots: &job.roots, pin: &job.pin, ival: &job.ival, bounds: &job.bounds, ..Default::default() };
    let f = match frame::trace_frame(&mut tr.it, tr.chunks, st, &spec) {
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
        // The room exit is a TERMINAL, recorded and not stepped: the next
        // room's objects belong to its own kernel set.
        if room != room0 {
            skipped.push("another room");
            continue;
        }
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
        let spawn_phase = shapes::player_path(&o.st).is_none() && !super::widen::objects_of_type(&o.st, super::game::PLAYER_SPAWN).is_empty();
        let respawn = if grid.is_some() && spawn_phase && !job.spawn { Some(shapes::field_constants(&o.st, &tr.it.d, &opts)?) } else { None };
        let rep = reps.block(&o.st, &mut tr.it.d, true)?;
        outs.push(WalkOutcome { key: rep.shape_hash, constants, ival, regions, rep, respawn });
    }
    let bound = super::lower::bind(&f, &tr.it.d.graph).map_err(|e| format!("{:#}", e));
    let illegal = std::mem::take(&mut tr.it.illegal);
    // Decided HERE: `f.raise` names a node of this worker's arena.
    let can_raise = tr.it.d.decide(&f.raise) != Some(false);
    Ok(WalkTraced::Traced { frame: f, bound, nodes_added, arena: tr.it.d.node_count(), outs, skipped, illegal, can_raise })
}

/// The tracer a walk ran in, kept to re-trace a shape in the same arena.
#[derive(Clone)]
pub struct Tracer<A: GraphArith = Fixed> {
    pub it: super::interp::Interp<'static, super::domain::Symbolic<A>>,
    pub chunks: &'static super::cart::Chunks,
}

/// What a lattice walk runs under: the run's settings and, for a test,
/// `only` the player regions it traces (`None`: every region).
pub struct Walk<'a> {
    pub settings: &'a crate::settings::Settings,
    pub only: Option<&'a [Region]>,
}

pub struct LatticeWalk<A: GraphArith = Fixed> {
    pub tracer: Tracer<A>,
    pub start_key: ShapeKey,
    pub lattice: std::collections::BTreeMap<ShapeKey, std::collections::BTreeMap<super::iface::Path, super::iface::Conc<A>>>,
    /// Each shape's representative, a state in `tracer`'s arena.
    pub reps: std::collections::BTreeMap<ShapeKey, super::state::State<super::domain::Symbolic<A>>>,
    /// Per (shape, region), its converged frame, bounds and binding.
    pub frames: std::collections::BTreeMap<WalkNode, WalkFrame<A>>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    /// Per shape, input slots a reachable frame wrote an `rnd`-derived interval
    /// to, typed as interval inputs.
    pub ival_extra: std::collections::BTreeMap<ShapeKey, std::collections::BTreeSet<super::iface::Path>>,
    /// The start state's own intervals by path, the literal or `None` (the
    /// representative holds a blanked point there; level -1 seeds from this). A
    /// path absent here holds its start value in the representative.
    pub start_ivals: std::collections::BTreeMap<super::iface::Path, Option<(A::Lit, A::Lit)>>,
}
