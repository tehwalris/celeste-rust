//! LEVEL -1 (plans/level-minus-one.md): a POSITION-ONLY cost-to-go table for
//! the start room, computed from the traced frame graphs. The search reads it
//! as a filter (`CELESTE_LEVEL_MINUS_ONE="H,S"`, `frame::level_minus_one`:
//! a cell that provably cannot exit by H is dropped); `transpile
//! --level-minus-one` checks and reports it (`probe`).
//!
//! A node is (heap shape, cell of the located object - the player, or the
//! player spawn). Every other input of the frame is a RANGE (numbers) or
//! unknown (booleans), one per shape and the same for every cell of it: the
//! objects' `rem` its whole [-0.5, 0.5), the player's speed [-S, S], the rest
//! discovered below. One frame from a node is the shape's traced frame with
//! every `move` fork RESOLVED per configuration (`specialize_subset_into`: in
//! a configuration the move amount is one number, so the pixel steps and
//! their collisions are exact at an exact position) and evaluated over the
//! ranges with `Graph::eval_narrow_top_in`: a select whose condition the
//! ranges leave undecided JOINS its arms, the six buttons (forks over a
//! literal, left standing as that literal) unknown.
//! Every outcome whose `live` is not definitely false is a successor, at the
//! cells its position hull covers; an outcome in another room is the EXIT.
//! `d(node)` is the fewest frames from the node to an exit over that graph.
//!
//! The table over-approximates the game's transitions only under three
//! premises, and each is CHECKED, never assumed:
//!
//! * INDUCTIVE RANGES. A successor's values must lie in its shape's ranges
//!   and agree with its lattice pins. After a pass over the reachable graph
//!   every observed value is compared; a range that does not contain what
//!   flows into it widens (`widen`), a pin that does not hold is
//!   dropped (the shape re-traced without it), and the pass is redone. A pass
//!   that changes nothing is the answer.
//! * NO `error` on any live outcome (`trace::error`), read with `Known(..)`
//!   and `SplitOk(..)` as true. `Known(c)` is how a select's own error says
//!   the kernel reads a DECIDED condition (and a floor's, a single one): this
//!   evaluator joins an undecided select instead, which is sound.
//!   `SplitOk(n)` is that a lane's fork operand spans at most `n` floors:
//!   checked directly on every configuration's operands instead. What is
//!   left - pin guards, the static range claims, an unrolled loop that had
//!   not finished, the widenings' containment - must evaluate to FALSE, or
//!   the node is reported.
//! * Every position hull has whole endpoints (a cell is a whole pixel).
//!
//! A range is a HULL of the node's values; two refinements keep what the hull
//! forgets, both exact per state, so the premises above stay checked as they
//! are (2026-10-01, rooms (6,1), (7,1), (0,2)):
//!
//! * a slot every state holds as the same literal interval (the balloon's
//!   `rnd` phase, `Shape::lits`) is read as that literal, so its own ends
//!   (`Lo`/`Hi`) are exact;
//! * selects are lifted out of the ops above them (`lift`): `Lo`/`Hi`/`Sub`
//!   in every frame, every value op in the precise frame a node falls back to
//!   when its plain evaluation is in violation (`eval_node`).
//!
//! And three things it does NOT model, each counted in the report:
//!
//! * THE SPAWN is run as a chain from the start state (`probe`): as a
//!   position-only node it has no bound (`solids=false`, one `state` range per
//!   shape), so its ranges never converge.
//! * A DEATH (an outcome without a located object) is a dead end.
//! * A successor cell outside `WINDOW` is clipped.
//!
//! The recorded transitions of a level-0 tree are the check that none of the
//! three dropped a real move (`probe`'s soundness section).

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::path::{Path as FsPath, PathBuf};

use anyhow::{anyhow, bail, ensure, Result};

use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Path, Step};
use super::kernel::{LatticeWalk, Tracer};
use super::state::State;
use crate::transpile::graph::{Graph, NodeId, Op, Room, Val};

/// A report line, to the report and at once to stderr (a run is minutes).
macro_rules! say {
    ($rep:expr, $($a:tt)*) => {{
        let s = format!($($a)*);
        eprintln!("{s}");
        $rep.push_str(&s);
        $rep.push('\n');
        Ok::<(), anyhow::Error>(())
    }};
}

/// A raw 16.16 range, inclusive.
type Range = (i64, i64);

/// The rem range every object's `rem` is seeded with and must stay in.
const REM: Range = (-0x8000, 0x7fff);

/// More fork configurations than this in one shape is refused, not traced.
const MAX_CONFIGS: usize = 1 << 14;

/// A successor box larger than this (cells) is refused: an imprecision that
/// size is a finding to look at, not a table to build.
const MAX_BOX: i64 = 4096;

/// The window a successor cell is kept in: the room and 16 px around it. A
/// player in the room is at x in [-1, 121] (the draw clamp) and y in [-4,
/// 128] (the exit and the bottom death); what the ranges admit past that is
/// CLIPPED and counted, never silently: the player spawn has `solids=false`
/// and a `state` that is one range for every cell, so at level -1 it can keep
/// rising at any height (plans/level-minus-one.md).
const WINDOW: (i64, i64) = (-16, 143);

/// The "no located object" coordinate (a death's countdown).
const NONE: i16 = i16::MIN;

/// What the probe is asked.
pub struct Opts {
    /// The player's speed range per axis, whole pixels per frame.
    pub spd_px: i32,
    /// A level-0 checkpoint tree to check against and compare with.
    pub level_dir: PathBuf,
    pub ceiling: u32,
    pub from: u32,
    pub to: u32,
    /// The tree's backward marks at horizon `ceiling` (`hNNN/level00.marks.bin`):
    /// states on a level-0 winning path, none of which the table may call
    /// too late.
    pub marks: Option<PathBuf>,
    pub threads: usize,
}

/// What flowed into one input slot of a shape, joined over the edges.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Obs {
    Num(i64, i64),
    /// A LITERAL interval (`Op::Const(lo, hi)`, `lo < hi`): every lane holds
    /// exactly this interval - the balloon's canonical phase `[0, 1)`, an
    /// `rnd` draw. `Num` is a hull of lanes' values instead.
    Lit(i64, i64),
    Bool { f: bool, t: bool, u: bool },
}

impl Obs {
    fn of(v: Val) -> Obs {
        match v {
            Val::Num(i) => Obs::Num(raw(i.low), raw(i.high)),
            Val::Bool(Some(false)) => Obs::Bool { f: true, t: false, u: false },
            Val::Bool(Some(true)) => Obs::Bool { f: false, t: true, u: false },
            Val::Bool(None) => Obs::Bool { f: false, t: false, u: true },
        }
    }
    /// What flowed in as a hull of the lanes' values: a literal is one.
    fn hull(self) -> Obs {
        match self {
            Obs::Lit(a, b) => Obs::Num(a, b),
            o => o,
        }
    }
    fn join(self, o: Obs) -> Result<Obs> {
        Ok(match (self, o) {
            (Obs::Lit(a, b), Obs::Lit(c, d)) if (a, b) == (c, d) => Obs::Lit(a, b),
            (Obs::Num(a, b) | Obs::Lit(a, b), Obs::Num(c, d) | Obs::Lit(c, d)) => Obs::Num(a.min(c), b.max(d)),
            (Obs::Bool { f, t, u }, Obs::Bool { f: g, t: s, u: v }) => Obs::Bool { f: f || g, t: t || s, u: u || v },
            _ => bail!("a slot observed as both a number and a boolean"),
        })
    }
}

fn raw(p: P8) -> i64 {
    p.as_raw_u32() as i32 as i64
}

fn px(r: i64) -> f64 {
    r as f64 / 65536.0
}

/// Widen `seed` to contain `obs`: the hull, or - for a slot that has already
/// grown twice (`jump`) - the moved end straight to the 16.16 extreme. The
/// hull first because anything wider can overshoot what a premise allows (a
/// fruit's `y` past its bob band leaves `widen_fruit`'s containment premise
/// undecided); the jump because a range the evaluator cannot bound (an
/// undecided `if freeze>0 then freeze-1`, whose arm is computed over the
/// whole range) would otherwise take one pass per step down.
fn widen(seed: Range, obs: Range, jump: bool) -> Range {
    let lo = if obs.0 < seed.0 { if jump { i32::MIN as i64 } else { obs.0 } } else { seed.0 };
    let hi = if obs.1 > seed.1 { if jump { i32::MAX as i64 } else { obs.1 } } else { seed.1 };
    (lo, hi)
}

/// The object a cell is read from: the player, or during the spawn the
/// player spawn - `pos_graph::player_object`'s rule.
fn located_object(st: &State<Symbolic>) -> Option<Path> {
    if let Some(p) = super::shapes::player_path(st) {
        return Some(p);
    }
    let Some(Value::Table(want)) = iface::get(st, &[iface::key("player_spawn")]) else { return None };
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return None };
    (0..st.heap.tables[&objects].arr.len()).map(|i| vec![iface::key("objects"), Step::Idx(i)]).find(|base| {
        let mut ty = base.clone();
        ty.push(iface::key("type"));
        iface::get(st, &ty) == Some(Value::Table(want))
    })
}

fn with(base: &Path, names: &[&str]) -> Path {
    let mut p = base.clone();
    for n in names {
        p.push(iface::key(n));
    }
    p
}

/// Is `p` an object's `rem.x/y`?
fn is_rem(p: &Path) -> bool {
    p.len() == 4 && p[0] == iface::key("objects") && p[2] == iface::key("rem")
}

/// Every object's `rem.x/y` and `spd.x/y` slots.
fn motion_paths(st: &State<Symbolic>) -> Vec<Path> {
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return Vec::new() };
    let mut out = Vec::new();
    for i in 0..st.heap.tables[&objects].arr.len() {
        let base = vec![iface::key("objects"), Step::Idx(i)];
        for sub in ["rem", "spd"] {
            for ax in ["x", "y"] {
                let p = with(&base, &[sub, ax]);
                if matches!(iface::get(st, &p), Some(Value::Num(_))) {
                    out.push(p);
                }
            }
        }
    }
    out
}

/// How an input slot is given a value.
#[derive(Clone)]
enum Seed {
    Pin(Conc),
    X,
    Y,
    /// A numeric range, by path into the shape's `ranges`.
    Range(Path),
    Unknown,
}

enum Target {
    Exit,
    Shape { id: usize, located: bool },
}

struct OutSpec {
    target: Target,
    /// The observation slot of each output field (`Table::obs_keys`).
    fields: Vec<usize>,
}

struct Roots {
    live: NodeId,
    error: NodeId,
    xy: Option<(NodeId, NodeId)>,
    fields: Vec<NodeId>,
}

/// One configuration's roots: per outcome, and the fork operands with their
/// arities.
struct Config {
    outs: Vec<Roots>,
    operands: Vec<(u8, NodeId)>,
}

/// Every fork configuration of a cone, specialized into one shared graph.
struct Spec {
    graph: Graph,
    configs: Vec<Config>,
}

/// What a shape's frame is before specialization: the cone with its roots
/// and forks (in the cone's ids), from which `Spec`s are built.
struct Cone {
    graph: Graph,
    roots: Vec<Roots>,
    forks: BTreeMap<u8, NodeId>,
    /// `(fork, arity)` of every fork in `forks`.
    arities: Vec<(u8, u8)>,
    n_forks: u8,
}

/// A shape traced for level -1.
struct Traced {
    seeds: Vec<Seed>,
    outs: Vec<OutSpec>,
    /// The frame specialized as traced: what every node is evaluated with.
    plain: Spec,
    /// The same frame with its selects LIFTED (`lift`), more precise and
    /// ~25x larger: built the first time a node's plain evaluation is in
    /// violation, and then that node's answer (`eval_node`).
    precise: std::sync::OnceLock<Spec>,
    /// Which trace this is (`TRACES`): a node's frame is a function of it and
    /// the seed ranges (`Table::sig`).
    generation: u64,
    cone: Cone,
    /// The bounds it was traced under (the static ranges the arity came from).
    bounds: BTreeMap<Path, Range>,
    pins: BTreeMap<Path, Conc>,
    stats: String,
}

struct Shape {
    key: String,
    ranges: BTreeMap<Path, Range>,
    /// The `ranges` that are a LITERAL interval every lane holds (`Obs::Lit`):
    /// the cone reads the slot as that literal (`Op::Const`), so a lane's own
    /// ends (`Lo`/`Hi`) are exact - the balloon's full-period premise
    /// (`widen::canon_balloon_offset`) is decided, where over a hull of the
    /// lanes' values it is not. Only an `rnd`-derived slot (`ival_extra`).
    lits: BTreeSet<Path>,
    /// Ranges not yet observed: taken from a blanked representative (0), so
    /// the first observation REPLACES them rather than joining the 0 in.
    unseeded: BTreeSet<Path>,
    demoted: BTreeSet<Path>,
    traced: Option<Traced>,
}

/// One node's frame, as its successors.
#[derive(Default)]
struct NodeOut {
    /// `(target shape, box)`; `usize::MAX` is the exit.
    succ: Vec<(usize, Option<(i16, i16, i16, i16)>)>,
    /// A successor box was clipped to `WINDOW`.
    clipped: bool,
}

/// A worker's accumulated observations and violations.
struct Acc {
    obs: Vec<Option<Obs>>,
    /// Successors clipped to `WINDOW`, per target shape.
    clipped: Vec<u64>,
    violations: Vec<String>,
    n_violations: usize,
    /// Nodes answered by the precise frame (`Traced::precise`).
    precise: usize,
}

struct Table<'a> {
    lw: &'a LatticeWalk,
    room: Room,
    room0: (i16, i16),
    spd_px: i32,
    shapes: Vec<Shape>,
    by_key: HashMap<String, usize>,
    obs_keys: Vec<(usize, Path)>,
    obs_index: BTreeMap<(usize, Path), usize>,
}

impl<'a> Table<'a> {
    fn shape_id(&mut self, key: &str) -> Result<usize> {
        if let Some(&i) = self.by_key.get(key) {
            return Ok(i);
        }
        ensure!(self.lw.reps.contains_key(key), "an outcome shape the lattice walk never reached: {key}");
        let i = self.shapes.len();
        self.shapes.push(Shape { key: key.to_string(), ranges: BTreeMap::new(), lits: BTreeSet::new(), unseeded: BTreeSet::new(), demoted: BTreeSet::new(), traced: None });
        self.by_key.insert(key.to_string(), i);
        Ok(i)
    }

    /// May slot `p` of shape `key` be a literal (`Shape::lits`)? An
    /// `rnd`-derived slot only, never a motion slot: a range there stands for
    /// every concrete value, which is what the table bounds.
    fn lit_slot(&self, key: &str, p: &Path) -> bool {
        let motion = p.len() == 4 && p[0] == iface::key("objects") && (p[2] == iface::key("rem") || p[2] == iface::key("spd"));
        !motion && self.lw.ival_extra.get(key).is_some_and(|s| s.contains(p))
    }

    fn obs_slot(&mut self, shape: usize, p: &Path) -> usize {
        if let Some(&i) = self.obs_index.get(&(shape, p.clone())) {
            return i;
        }
        let i = self.obs_keys.len();
        self.obs_keys.push((shape, p.clone()));
        self.obs_index.insert((shape, p.clone()), i);
        i
    }

    /// Trace shape `id` under its current ranges and pins, and specialize its
    /// frame on every fork configuration into one shared graph.
    fn trace(&mut self, tr: &mut Tracer, id: usize) -> Result<()> {
        let t0 = std::time::Instant::now();
        let key = self.shapes[id].key.clone();
        let st = self.lw.reps[&key].clone();
        let roots = super::shapes::state_paths(&st)?;
        let pins: BTreeMap<Path, Conc> = self.lw.lattice[&key]
            .iter()
            .filter(|(p, _)| roots.iter().any(|r| r == *p) && !self.shapes[id].demoted.contains(*p))
            .map(|(p, c)| (p.clone(), *c))
            .collect();
        let player = super::shapes::player_path(&st);
        let located = located_object(&st);
        let (xp, yp) = match &located {
            Some(o) => (Some(with(o, &["x"])), Some(with(o, &["y"]))),
            None => (None, None),
        };
        // Seed every numeric slot not pinned: a range kept from an earlier
        // pass, else the player's rem half and speed [-S, S], or the value the
        // representative holds (the start state's own, a blanked shape's 0).
        for p in &roots {
            if pins.contains_key(p) || self.shapes[id].ranges.contains_key(p) || Some(p) == xp.as_ref() || Some(p) == yp.as_ref() {
                continue;
            }
            let Some(Value::Num(n)) = iface::get(&st, p) else { continue };
            let is_player = |f: &str| player.as_ref().is_some_and(|pl| *p == with(pl, &[f, "x"]) || *p == with(pl, &[f, "y"]));
            let start_ival = key == self.lw.start_key && self.lw.ival_extra.get(&key).is_some_and(|s| s.contains(p));
            let r = if start_ival {
                // The start state's own interval (the balloon's `offset =
                // rnd(1)`): its representative holds a blanked point, and
                // seeded with that point the balloon's phase was no full
                // period, so `canon_balloon_offset`'s premise failed on the
                // spawn's first frame (rooms (6,1) and (7,1), 2026-10-01).
                // A literal: the one start state holds exactly it.
                let (lo, hi) = *self.lw.start_ivals.get(p).ok_or_else(|| anyhow!("{key}: the start state's interval {} is not a literal range", iface::show(p)))?;
                ensure!(self.lit_slot(&key, p), "{key}: the start state's interval {} is not an `rnd` slot", iface::show(p));
                self.shapes[id].lits.insert(p.clone());
                (lo as i64, hi as i64)
            } else if is_player("rem") {
                REM
            } else if is_player("spd") {
                (-(self.spd_px as i64) << 16, (self.spd_px as i64) << 16)
            } else {
                let v = tr.it.d.as_const(&n).ok_or_else(|| anyhow!("{key}: {} is not a constant in the representative", iface::show(p)))?;
                if key != self.lw.start_key {
                    self.shapes[id].unseeded.insert(p.clone());
                }
                (raw(v), raw(v))
            };
            self.shapes[id].ranges.insert(p.clone(), r);
        }
        // Interval inputs: every object's unpinned rem and speed (so `move`
        // forks, and the fork's arity comes from their static ranges), and
        // the boundary's own (a fruit's band, the walk's discovered slots).
        let mut ival: Vec<Path> = Vec::new();
        let mut bounds: BTreeMap<Path, Range> = BTreeMap::new();
        for p in motion_paths(&st) {
            if pins.contains_key(&p) {
                continue;
            }
            bounds.insert(p.clone(), self.shapes[id].ranges[&p]);
            ival.push(p);
        }
        for p in super::shapes::ival_paths(&st).into_iter().chain(self.lw.ival_extra.get(&key).into_iter().flatten().cloned()) {
            if !pins.contains_key(&p) && !ival.contains(&p) {
                ival.push(p);
            }
        }
        let pin_list: Vec<(Path, Conc)> = pins.iter().map(|(p, c)| (p.clone(), *c)).collect();
        let bound_list: Vec<(Path, (i32, i32))> = bounds.iter().map(|(p, r)| (p.clone(), (r.0 as i32, r.1 as i32))).collect();
        // The held trails are unknown booleans read from their cells, not a
        // fork (the walk forks them for the kernels).
        tr.it.d.held_unknown = false;
        // The move forks' arity from the ranges' full width (`flr_ways`): the
        // speed is [-S, S] at one node, 2S + 1 floors. Restored below.
        tr.it.d.uncapped_ways = true;
        // And its undecided selects stay selects: this evaluator joins their
        // arms (module doc), a fork would only multiply configurations.
        tr.it.d.no_known_forks = true;
        let f = super::verify::trace_frame(
            &mut tr.it,
            tr.reset,
            tr.fr,
            st.clone(),
            &roots,
            &pin_list,
            &ival,
            Some(super::widen::WidenMode::Level0),
            &bound_list,
        )
        .map_err(|e| anyhow!("level -1 trace of shape {id}: {e:#}"));
        tr.it.d.uncapped_ways = false;
        tr.it.d.no_known_forks = false;
        let f = f?;
        let t_trace = t0.elapsed();
        let n_slots = f.iface.slots.len();
        let seeds: Vec<Seed> = f
            .iface
            .slots
            .iter()
            .zip(&f.iface.init)
            .map(|(p, c)| {
                if Some(p) == xp.as_ref() {
                    Seed::X
                } else if Some(p) == yp.as_ref() {
                    Seed::Y
                } else if let Some(c) = pins.get(p) {
                    Seed::Pin(*c)
                } else {
                    match c {
                        Conc::Num(_) => Seed::Range(p.clone()),
                        Conc::Bool(_) => Seed::Unknown,
                    }
                }
            })
            .collect();
        ensure!(located.is_none() || seeds.iter().filter(|s| matches!(s, Seed::X | Seed::Y)).count() == 2, "shape {id}: the located object's x/y are not input slots");

        // The outcomes and every root read from them.
        let d = &tr.it.d;
        let mut outs: Vec<OutSpec> = Vec::new();
        let mut roots_old: Vec<Roots> = Vec::new();
        for o in &f.outs {
            let room = super::shapes::room_of(&o.st, d);
            ensure!(room.0 >= 0 && room.1 >= 0, "shape {id}: an outcome whose room is not a constant");
            if room != self.room0 {
                outs.push(OutSpec { target: Target::Exit, fields: Vec::new() });
                roots_old.push(Roots { live: o.guard, error: o.error, xy: None, fields: Vec::new() });
                continue;
            }
            let tkey = format!("{:?}", o.st.shape()?);
            let tid = self.shape_id(&tkey)?;
            let tloc = located_object(&o.st);
            let node_of = |p: &Path| o.fields.iter().find(|(q, _, _)| q == p).map(|(_, n, _)| *n);
            let xy = match &tloc {
                Some(ob) => Some((
                    node_of(&with(ob, &["x"])).ok_or_else(|| anyhow!("shape {id}: an outcome's located object has no x"))?,
                    node_of(&with(ob, &["y"])).ok_or_else(|| anyhow!("shape {id}: an outcome's located object has no y"))?,
                )),
                None => None,
            };
            let mut fields = Vec::new();
            let mut fnodes = Vec::new();
            for (p, n, _) in &o.fields {
                fields.push(self.obs_slot(tid, p));
                fnodes.push(*n);
            }
            outs.push(OutSpec { target: Target::Shape { id: tid, located: tloc.is_some() }, fields });
            roots_old.push(Roots { live: o.guard, error: o.error, xy, fields: fnodes });
        }

        // The cone, copied out of the arena with every fork over a literal
        // (the buttons) standing for its whole literal - unknown - and the
        // kernels' premises read as true (module doc).
        let arena = &d.graph;
        let mut rs: Vec<NodeId> = Vec::new();
        for r in &roots_old {
            rs.push(r.live);
            rs.push(r.error);
            if let Some((x, y)) = r.xy {
                rs.push(x);
                rs.push(y);
            }
            rs.extend(&r.fields);
        }
        let need = crate::transpile::bdd::reachable(arena, &rs);
        let mut cone = arena.like();
        cone.reset_forks();
        for k in 0..f.forks {
            cone.set_fork_ways(k, f.fork_ways[k as usize]);
        }
        let mut cmap: Vec<NodeId> = vec![NodeId::MAX; arena.len()];
        let mut forks: BTreeMap<u8, NodeId> = BTreeMap::new();
        // The literal slots' cells (`Shape::lits`), read as their literal.
        let lit_cells: HashMap<u32, (i32, i32)> = f
            .iface
            .slots
            .iter()
            .enumerate()
            .filter(|(_, p)| self.shapes[id].lits.contains(*p))
            .map(|(i, p)| {
                let r = self.shapes[id].ranges[p];
                (i as u32, (r.0 as i32, r.1 as i32))
            })
            .collect();
        let mut lifted: HashMap<(Op, Vec<NodeId>), NodeId> = HashMap::new();
        for i in 0..arena.len() {
            if !need[i] {
                continue;
            }
            let nd = arena.get(i as NodeId);
            let args: Vec<NodeId> = nd.args.iter().map(|a| cmap[*a as usize]).collect();
            cmap[i] = match nd.op {
                _ if arena.is_literal_fork(i as NodeId) => cmap[nd.args[0] as usize],
                Op::Cell(c) if lit_cells.contains_key(&c) => {
                    let (lo, hi) = lit_cells[&c];
                    cone.leaf(Op::Const(lo, hi))
                }
                Op::Known | Op::SplitOk(_) | Op::NoWrap => cone.leaf(Op::ConstBool(true)),
                _ => {
                    if let Op::Split(k) | Op::SplitInt(k) = nd.op {
                        forks.entry(k).or_insert(args[0]);
                    }
                    lift(&mut cone, nd.op.clone(), args, Lift::Ends, &mut lifted)
                }
            };
        }
        // `SplitValid` without its `Split` in the cone still names a fork.
        for i in 0..arena.len() {
            if need[i] {
                if let Op::SplitValid(k) = arena.get(i as NodeId).op {
                    forks.entry(k).or_insert(cmap[arena.get(i as NodeId).args[0] as usize]);
                }
            }
        }
        let arities: Vec<(u8, u8)> = forks.keys().map(|k| (*k, f.fork_ways[*k as usize])).collect();
        let n_configs: usize = arities.iter().map(|(_, w)| *w as usize).product();
        ensure!(n_configs <= MAX_CONFIGS, "shape {id}: {n_configs} fork configurations ({arities:?})");
        let m = |n: &NodeId| cmap[*n as usize];
        let roots: Vec<Roots> = roots_old.iter().map(|r| Roots { live: m(&r.live), error: m(&r.error), xy: r.xy.map(|(x, y)| (m(&x), m(&y))), fields: r.fields.iter().map(m).collect() }).collect();
        let cone = Cone { graph: cone, roots, forks, arities, n_forks: f.forks };
        let plain = cone.specialize();
        let operands: Vec<String> = cone.forks.iter().map(|(k, op)| format!("fork {k}: {}", super::emit::show_tree(&cone.graph, *op, 3))).collect();
        let stats = format!(
            "{} slots ({} pinned, {} interval), {} outcomes, forks {:?}, {} configurations ({} distinct), cone {} nodes, specialized {} nodes, trace {:.1} s, specialize {:.1} s",
            n_slots,
            pins.len(),
            ival.len(),
            outs.len(),
            cone.arities.iter().map(|(_, w)| *w).collect::<Vec<_>>(),
            n_configs,
            plain.configs.len(),
            cone.graph.len(),
            plain.graph.len(),
            t_trace.as_secs_f64(),
            (t0.elapsed() - t_trace).as_secs_f64()
        ) + &operands.iter().map(|o| format!("\n      {o}")).collect::<String>();
        let generation = TRACES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.shapes[id].traced = Some(Traced { seeds, outs, plain, precise: std::sync::OnceLock::new(), generation, cone, bounds, pins, stats });
        Ok(())
    }

}

impl Cone {
    /// Every fork configuration, specialized into one shared graph; the
    /// configurations that come out identical kept once.
    fn specialize(&self) -> Spec {
        let n_configs: usize = self.arities.iter().map(|(_, w)| *w as usize).product();
        let mut shared = self.graph.like();
        let mut configs: Vec<Config> = Vec::new();
        let mut seen: HashSet<Vec<NodeId>> = HashSet::new();
        // Only what a root or a fork operand reads: the lifted cone
        // (`precise`) holds every intermediate `lift` made, ~45% of it dead.
        let mut roots: Vec<NodeId> = self.forks.values().copied().collect();
        for r in &self.roots {
            roots.extend([r.live, r.error]);
            roots.extend(r.xy.iter().flat_map(|(x, y)| [*x, *y]));
            roots.extend(&r.fields);
        }
        let need = crate::transpile::bdd::reachable(&self.graph, &roots);
        for c in 0..n_configs {
            let mut splits = vec![0u8; self.n_forks as usize];
            let mut rest = c;
            for (k, w) in &self.arities {
                splits[*k as usize] = (rest % *w as usize) as u8;
                rest /= *w as usize;
            }
            let map = self.graph.specialize_subset_into(&splits, Some(&need), &mut shared);
            let m = |n: NodeId| map[n as usize];
            let outs_c: Vec<Roots> = self
                .roots
                .iter()
                .map(|r| Roots { live: m(r.live), error: m(r.error), xy: r.xy.map(|(x, y)| (m(x), m(y))), fields: r.fields.iter().map(|n| m(*n)).collect() })
                .collect();
            let operands: Vec<(u8, NodeId)> = self.forks.iter().map(|(k, op)| (self.graph.fork_ways(*k), map[*op as usize])).collect();
            let mut sig: Vec<NodeId> = Vec::new();
            for r in &outs_c {
                sig.extend([r.live, r.error]);
                if let Some((x, y)) = r.xy {
                    sig.extend([x, y]);
                }
                sig.extend(&r.fields);
            }
            sig.extend(operands.iter().map(|o| o.1));
            if seen.insert(sig) {
                configs.push(Config { outs: outs_c, operands });
            }
        }
        Spec { graph: shared, configs }
    }

    /// The cone with its selects lifted (`lift`), specialized.
    fn precise(&self) -> Spec {
        let g = &self.graph;
        let mut out = g.like();
        let mut memo: HashMap<(Op, Vec<NodeId>), NodeId> = HashMap::new();
        let mut map: Vec<NodeId> = Vec::with_capacity(g.len());
        for i in 0..g.len() {
            let nd = g.get(i as NodeId);
            let args: Vec<NodeId> = nd.args.iter().map(|a| map[*a as usize]).collect();
            map.push(if args.is_empty() { out.leaf(nd.op.clone()) } else { lift(&mut out, nd.op.clone(), args, Lift::All, &mut memo) });
        }
        let m = |n: NodeId| map[n as usize];
        let lifted = Cone {
            graph: out,
            roots: self.roots.iter().map(|r| Roots { live: m(r.live), error: m(r.error), xy: r.xy.map(|(x, y)| (m(x), m(y))), fields: r.fields.iter().map(|n| m(*n)).collect() }).collect(),
            forks: self.forks.iter().map(|(k, n)| (*k, m(*n))).collect(),
            arities: self.arities.clone(),
            n_forks: self.n_forks,
        };
        lifted.specialize()
    }
}

/// Traces made so far (`Traced::generation`).
static TRACES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// `a + b` / `a - b` of a literal interval and a point, as the literal every
/// lane then holds; `None` for anything else or past the 16.16 range.
fn lit_shift(g: &Graph, op: &Op, args: &[NodeId]) -> Option<(i32, i32)> {
    let c = |n: NodeId| match g.get(n).op {
        Op::Const(lo, hi) => Some((lo as i64, hi as i64)),
        _ => None,
    };
    let (a, b) = (c(*args.first()?)?, c(*args.get(1)?)?);
    let (lo, hi) = match op {
        Op::Add if a.0 < a.1 && b.0 == b.1 => (a.0 + b.0, a.1 + b.0),
        Op::Add if b.0 < b.1 && a.0 == a.1 => (b.0 + a.0, b.1 + a.0),
        Op::Sub if a.0 < a.1 && b.0 == b.1 => (a.0 - b.0, a.1 - b.0),
        _ => return None,
    };
    (lo >= i32::MIN as i64 && hi <= i32::MAX as i64).then_some((lo as i32, hi as i32))
}

/// Which ops `lift` lifts selects out of.
#[derive(Clone, Copy)]
enum Lift {
    /// A lane's ends and their difference (`Lo`, `Hi`, `Sub`): cheap, and what
    /// the balloon's full-period width needs at every node of a balloon room.
    /// The frame every node is evaluated with (`Traced::plain`).
    Ends,
    /// Every pure value op: ~25x the graph (room (7,1): 92k -> 2.4M nodes
    /// specialized), so only where `Ends` is in violation (`Traced::precise`).
    All,
}

/// `op(args)` with every select among the args LIFTED out where they all
/// select on one condition: `op(.., sel(c, a, b), ..)` is `sel(c, op(.., a,
/// ..), op(.., b, ..))`, exact per lane - a lane takes one arm of `c` in every
/// operand at once. This evaluator joins a select it cannot decide, and an
/// expression of a joined value forgets that its operands moved together:
///
/// * the balloon's full-period width `Hi(v) - Lo(v)`, `v` a select between two
///   literal phases (shown or popped), is any difference of the two ends'
///   hulls, and its premise (`widen::canon_balloon_offset`) is undecided;
/// * a tile loop's "finished" obligation `start + k <= end`, both ends of the
///   player's `x` after a `move` the evaluator cannot decide was taken (its
///   speed range holds 0), puts a loop over 2 tiles at 3 (room (7,1)).
///
/// Lifted, each arm is decided on its own. Selects on DIFFERENT conditions
/// are left joined: lifting those multiplies arms. Never across a fork op (a
/// fork's operand stays one node) or a select's own condition. And a literal
/// shifted by a point is the shifted literal (`lit_shift`), which `Graph::fold`
/// leaves: it is exact with respect to `eval`'s hulls, not per lane.
fn lift(g: &mut Graph, op: Op, args: Vec<NodeId>, what: Lift, memo: &mut HashMap<(Op, Vec<NodeId>), NodeId>) -> NodeId {
    let liftable = match what {
        Lift::Ends => matches!(op, Op::Sub | Op::Lo | Op::Hi),
        Lift::All => matches!(
            op,
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem | Op::Neg | Op::Abs | Op::Flr | Op::Sin | Op::Min | Op::Max
                | Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Not | Op::And | Op::Or | Op::Lo | Op::Hi | Op::TileFlagAt
        ),
    };
    if let Some((lo, hi)) = lit_shift(g, &op, &args) {
        return g.leaf(Op::Const(lo, hi));
    }
    if !liftable {
        return g.fold(op, args);
    }
    let cond = |g: &Graph, a: NodeId| match g.get(a).op {
        Op::Sel => Some(g.get(a).args[0]),
        _ => None,
    };
    let conds: Vec<NodeId> = args.iter().filter_map(|a| cond(g, *a)).collect();
    let Some(&c) = conds.first() else {
        return g.fold(op, args);
    };
    if conds.iter().any(|d| *d != c) {
        return g.fold(op, args);
    }
    if let Some(&r) = memo.get(&(op.clone(), args.clone())) {
        return r;
    }
    let arm = |g: &Graph, k: usize| -> Vec<NodeId> { args.iter().map(|a| if cond(g, *a).is_some() { g.get(*a).args[k] } else { *a }).collect() };
    let (ta, fa) = (arm(g, 1), arm(g, 2));
    let t = lift(g, op.clone(), ta, what, memo);
    let f = lift(g, op.clone(), fa, what, memo);
    let r = g.fold(Op::Sel, vec![c, t, f]);
    memo.insert((op, args), r);
    r
}

/// One node's frame: evaluate every configuration over the shape's ranges at
/// cell `xy`. Free of the `Table`, whose tracer is not `Sync`.
fn eval_node(shapes: &[Shape], room: &Room, id: usize, xy: (i16, i16), acc: &mut Acc) -> Result<NodeOut> {
    let sh = &shapes[id];
    let t = sh.traced.as_ref().expect("a node's shape is traced before its layer");
    let exact = |v: i64| Val::Num(Pico8NumInterval::new(P8::from_raw(v as i32), P8::from_raw(v as i32)));
    let mut cells: HashMap<u32, Val> = HashMap::with_capacity(t.seeds.len());
    for (i, s) in t.seeds.iter().enumerate() {
        let v = match s {
            Seed::Pin(Conc::Num(n)) => Val::exact_num(*n),
            Seed::Pin(Conc::Bool(b)) => Val::Bool(Some(*b)),
            Seed::X => exact((xy.0 as i64) << 16),
            Seed::Y => exact((xy.1 as i64) << 16),
            Seed::Range(p) => {
                let r = sh.ranges[p];
                Val::Num(Pico8NumInterval::new(P8::from_raw(r.0 as i32), P8::from_raw(r.1 as i32)))
            }
            Seed::Unknown => Val::Bool(None),
        };
        cells.insert(i as u32, v);
    }
    // The plain frame first; where it is in violation, the precise one (both
    // over-approximate the node's frame, so either answer is sound).
    let vals = t.plain.graph.eval_narrow_top_in(&cells, room)?;
    let (sp, vals) = if in_violation(&t.plain, &vals) {
        let precise = t.precise.get_or_init(|| {
            let t0 = std::time::Instant::now();
            let p = t.cone.precise();
            eprintln!("  shape {id}: the precise frame, {} nodes, {:.1} s", p.graph.len(), t0.elapsed().as_secs_f64());
            p
        });
        acc.precise += 1;
        (precise, precise.graph.eval_narrow_top_in(&cells, room)?)
    } else {
        (&t.plain, vals)
    };
    let mut out = NodeOut::default();
    let violation = |acc: &mut Acc, msg: String| {
        acc.n_violations += 1;
        if acc.violations.len() < 12 {
            acc.violations.push(msg);
        }
    };
    // A position hull as whole pixels, or why not.
    let whole = |v: Val| -> std::result::Result<(i64, i64), String> {
        let Val::Num(i) = v else { return Err("not a number".into()) };
        let (lo, hi) = (raw(i.low), raw(i.high));
        if lo & 0xffff != 0 || hi & 0xffff != 0 {
            return Err(format!("[{}, {}] is not whole pixels", px(lo), px(hi)));
        }
        Ok((lo >> 16, hi >> 16))
    };
    for (ci, cfg) in sp.configs.iter().enumerate() {
        for &(ways, n) in &cfg.operands {
            if let Val::Num(i) = vals[n as usize] {
                let floors = (raw(i.high) >> 16) - (raw(i.low) >> 16) + 1;
                if floors > ways as i64 {
                    violation(acc, format!("shape {id} cell {xy:?} configuration {ci}: a fork operand spans {floors} floors, arity {ways}"));
                }
            }
        }
        for (oi, (spec, r)) in t.outs.iter().zip(&cfg.outs).enumerate() {
            if vals[r.live as usize] == Val::Bool(Some(false)) {
                continue;
            }
            if vals[r.error as usize] != Val::Bool(Some(false)) {
                let why = if acc.violations.len() < 12 { culprit(&sp.graph, &vals, r.error) } else { String::new() };
                violation(acc, format!("shape {id} cell {xy:?} configuration {ci} outcome {oi}: a live outcome's error is {:?}: {why}", vals[r.error as usize]));
            }
            for (slot, n) in spec.fields.iter().zip(&r.fields) {
                let o = match sp.graph.get(*n).op {
                    Op::Const(lo, hi) if lo < hi => Obs::Lit(lo as i64, hi as i64),
                    _ => Obs::of(vals[*n as usize]),
                };
                acc.obs[*slot] = Some(match acc.obs[*slot] {
                    Some(p) => p.join(o)?,
                    None => o,
                });
            }
            match spec.target {
                Target::Exit => out.succ.push((usize::MAX, None)),
                Target::Shape { id: tid, located } => {
                    let bx = match (located, r.xy) {
                        (true, Some((xn, yn))) => {
                            let what = format!("shape {id} cell {xy:?} configuration {ci} outcome {oi} (to shape {tid})");
                            let ((xl, xh), (yl, yh)) = match (whole(vals[xn as usize]), whole(vals[yn as usize])) {
                                (Ok(x), Ok(y)) => (x, y),
                                (Err(e), _) | (_, Err(e)) => {
                                    violation(acc, format!("{what}: a successor position {e}"));
                                    continue;
                                }
                            };
                            let area = (xh - xl + 1) * (yh - yl + 1);
                            if area > MAX_BOX {
                                violation(acc, format!("{what}: a successor box [{xl}, {xh}] x [{yl}, {yh}]"));
                                continue;
                            }
                            let clip = |(l, h): (i64, i64)| (l.max(WINDOW.0), h.min(WINDOW.1));
                            let ((cxl, cxh), (cyl, cyh)) = (clip((xl, xh)), clip((yl, yh)));
                            if (cxl, cxh, cyl, cyh) != (xl, xh, yl, yh) {
                                acc.clipped[tid] += 1;
                                out.clipped = true;
                            }
                            if cxl > cxh || cyl > cyh {
                                continue;
                            }
                            Some((cxl as i16, cxh as i16, cyl as i16, cyh as i16))
                        }
                        _ => None,
                    };
                    out.succ.push((tid, bx));
                }
            }
        }
    }
    Ok(out)
}

/// Would `eval_node` report a violation of this evaluation: a fork operand
/// wider than its arity, a live outcome's error not false, a successor
/// position not whole pixels or too large a box?
fn in_violation(spec: &Spec, vals: &[Val]) -> bool {
    let whole = |v: Val| matches!(v, Val::Num(i) if raw(i.low) & 0xffff == 0 && raw(i.high) & 0xffff == 0);
    let cells = |i: Pico8NumInterval| (raw(i.high) >> 16) - (raw(i.low) >> 16) + 1;
    spec.configs.iter().any(|cfg| {
        cfg.operands.iter().any(|&(ways, n)| matches!(vals[n as usize], Val::Num(i) if cells(i) > ways as i64))
            || cfg.outs.iter().any(|r| {
                vals[r.live as usize] != Val::Bool(Some(false))
                    && (vals[r.error as usize] != Val::Bool(Some(false))
                        || r.xy.is_some_and(|(x, y)| match (vals[x as usize], vals[y as usize]) {
                            (Val::Num(a), Val::Num(b)) if whole(vals[x as usize]) && whole(vals[y as usize]) => cells(a) * cells(b) > MAX_BOX,
                            _ => true,
                        }))
            })
    })
}

/// Which disjunct keeps `n` from being false: down the `Or`s and decided
/// selects to the first node that is not, with its operands' values.
fn culprit(g: &Graph, vals: &[Val], mut n: NodeId) -> String {
    loop {
        let nd = g.get(n);
        let not_false = |a: &NodeId| vals[*a as usize] != Val::Bool(Some(false));
        match nd.op {
            Op::Or => {
                if let Some(a) = nd.args.iter().find(|a| not_false(a)) {
                    n = *a;
                    continue;
                }
            }
            Op::Sel => match vals[nd.args[0] as usize] {
                Val::Bool(Some(c)) => {
                    n = nd.args[if c { 1 } else { 2 }];
                    continue;
                }
                _ => {
                    if let Some(a) = nd.args[1..].iter().find(|a| not_false(a)) {
                        n = *a;
                        continue;
                    }
                }
            },
            _ => {}
        }
        return format!("{} = {:?}, operands {:?}", super::emit::show_tree(g, n, 4), vals[n as usize], nd.args.iter().map(|a| vals[*a as usize]).collect::<Vec<_>>());
    }
}

/// The table after the passes converged.
struct Graph1 {
    nodes: Vec<(usize, i16, i16)>,
    index: HashMap<(usize, i16, i16), u32>,
    edges: Vec<Vec<u32>>,
    exits: Vec<bool>,
    /// A successor without a located object (a death).
    deaths: Vec<bool>,
    /// A successor clipped to `WINDOW`.
    clipped: Vec<bool>,
}

/// What `build` hands on: per block shape hash its table shape, the converged
/// graph, the sound d per node (`sound_d`), and the spawn chain.
struct Built {
    by_hash: HashMap<u64, usize>,
    g1: Graph1,
    sound: Vec<u32>,
    chain: Vec<(usize, (i16, i16))>,
    start_d: u32,
}

/// A LOWER BOUND on the frames from each node to an exit - what a filter may
/// refuse a row on. A multi-source shortest path backward over the edges,
/// seeded where the graph stops modelling: an exit edge (1); a death successor
/// (1 + the start state's d, `chain_frames` + d of the chain's end: the room
/// restarts and replays the spawn chain, however long its countdown). The death
/// seed reads the end's d, which it cannot lower (a path through a death is
/// longer than the start's d), so the second run is the answer.
///
/// A successor part CLIPPED to `WINDOW` is not a seed: no real player is there
/// at a frame boundary (the draw clamp keeps x in [-1, 121], skipped on a freeze
/// frame by at most one move; below y = 128 the update kills; above y = -4 the
/// room changes), so a clipped part is the evaluator's imprecision. Seeding it
/// as a possible exit (tried first) put every node near a clamped edge within a
/// frame or two of an exit and cut nothing in room (2,0). The claim is checked
/// on every row the filter sees (`CostToGo::too_late`), never assumed.
fn sound_d(g: &Graph1, rev: &[Vec<u32>], chain_frames: u32, end_node: u32) -> Vec<u32> {
    use std::cmp::Reverse;
    let n = g.nodes.len();
    let run = |death: Option<u32>| -> Vec<u32> {
        let mut dist = vec![u32::MAX; n];
        let mut heap = std::collections::BinaryHeap::new();
        for i in 0..n {
            let mut s = if g.exits[i] { 1 } else { u32::MAX };
            if let (true, Some(w)) = (g.deaths[i], death) {
                s = s.min(w.saturating_add(1));
            }
            if s != u32::MAX {
                dist[i] = s;
                heap.push(Reverse((s, i as u32)));
            }
        }
        while let Some(Reverse((dn, i))) = heap.pop() {
            if dn != dist[i as usize] {
                continue;
            }
            for &p in &rev[i as usize] {
                if dn + 1 < dist[p as usize] {
                    dist[p as usize] = dn + 1;
                    heap.push(Reverse((dn + 1, p)));
                }
            }
        }
        dist
    };
    let mut death = None;
    loop {
        let dist = run(death);
        let start = dist[end_node as usize];
        let w = (start != u32::MAX).then(|| start.saturating_add(chain_frames));
        if w == death {
            return dist;
        }
        death = w;
    }
}

/// THE LEVEL -1 FILTER's table (`frame::level_minus_one`): per (block shape
/// hash, player cell), a lower bound on the frames to an exit (`sound_d`).
pub struct CostToGo {
    d: HashMap<(u64, i16, i16), u32>,
    /// The start state's d: no exit is sooner than this.
    pub start_d: u32,
}

impl CostToGo {
    /// The number of (shape, cell) entries.
    pub fn len(&self) -> usize {
        self.d.len()
    }

    /// A hash of every entry and the start's d, in a fixed order: two builds
    /// with the same fingerprint filter identically.
    pub fn fingerprint(&self) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut es: Vec<(&(u64, i16, i16), &u32)> = self.d.iter().collect();
        es.sort_unstable();
        let mut h = std::collections::hash_map::DefaultHasher::new();
        self.start_d.hash(&mut h);
        es.hash(&mut h);
        h.finish()
    }

    /// Is a row of `shape` at `cell` at `frame` provably unable to exit by
    /// `horizon`? Only a table node is ever refused: a row without a player,
    /// one that has left the room, a shape or a cell the table never reached
    /// is kept. A row in the room OUTSIDE `WINDOW` breaks the premise the table
    /// dropped its clipped successors on (`sound_d`): it panics rather than
    /// filter on a table that does not cover it.
    pub fn too_late(&self, shape: u64, cell: u32, frame: u32, horizon: u32) -> bool {
        let Some((x, y)) = crate::search::pos_graph::cell_xy(cell) else { return false };
        if x >= 128 {
            return false;
        }
        let (x64, y64) = (x as i64, y as i64);
        assert!(
            x64 >= WINDOW.0 && y64 >= WINDOW.0 && y64 <= WINDOW.1,
            "level -1 filter: a row of shape {shape:#x} at ({x}, {y}), frame {frame}, lies outside the window {WINDOW:?} the table assumes a player never leaves"
        );
        match self.d.get(&(shape, x as i16, y as i16)) {
            None => false,
            Some(&d) => d == u32::MAX || frame.saturating_add(d) > horizon,
        }
    }
}

/// Build the level -1 table of the configured start room at player speed bound
/// `spd_px` (its report goes to stderr as it is built), or read it from the
/// table cache (`cache_file`) when this binary built it before for the same
/// inputs.
pub fn cost_to_go(root: &FsPath, spd_px: i32, threads: usize) -> Result<CostToGo> {
    let cache = cache_file(root, spd_px)?;
    if let Some((path, _)) = &cache {
        match CostToGo::load(path) {
            Ok(Some(t)) => {
                eprintln!("[level -1] table read from {} ({} entries)", path.display(), t.d.len());
                return Ok(t);
            }
            Ok(None) => {}
            Err(e) => eprintln!("[level -1] the cached table {} is unreadable ({e:#}): building it", path.display()),
        }
    }
    let mut rep = String::new();
    let b = build(root, spd_px, threads, &mut rep)?;
    let mut d = HashMap::new();
    for (&h, &id) in &b.by_hash {
        for (i, &(sid, x, y)) in b.g1.nodes.iter().enumerate() {
            if sid == id {
                d.insert((h, x, y), b.sound[i]);
            }
        }
    }
    let t = CostToGo { d, start_d: b.start_d };
    if let Some((path, key)) = &cache {
        match t.save(path, key) {
            Ok(()) => eprintln!("[level -1] table cached at {}", path.display()),
            Err(e) => eprintln!("[level -1] could not cache the table at {} ({e:#})", path.display()),
        }
    }
    Ok(t)
}

/// The table cache's format; bump it when what a file holds changes.
const CACHE_FORMAT: u32 = 1;

/// Where the table for these inputs is cached, and the key's description:
/// `CELESTE_L1_CACHE` (a directory, default `/var/tmp/celeste-l1-cache`; `off`
/// to always build). The table is a function of the code (this binary's own
/// bytes), the cart (the traced Lua as `trace::cart::sources_in` assembles it,
/// start room and `CELESTE_SPLIT_FRAME` included, and the map/flag data), the
/// speed bound, and the knobs the tracer reads - every `CELESTE_*` variable
/// but the ones that cannot change it, so an unknown knob costs a rebuild,
/// never a wrong table.
fn cache_file(root: &FsPath, spd_px: i32) -> Result<Option<(PathBuf, String)>> {
    use std::hash::{Hash, Hasher};
    let dir = match std::env::var("CELESTE_L1_CACHE") {
        Ok(s) if s == "off" => return Ok(None),
        Ok(s) => PathBuf::from(s),
        Err(_) => PathBuf::from("/var/tmp/celeste-l1-cache"),
    };
    let exe = std::env::current_exe()?;
    let exe_bytes = std::fs::read(&exe).map_err(|e| anyhow!("reading this binary {}: {e}", exe.display()))?;
    let lua = super::cart::sources_in(root)?;
    let mut cart: Vec<(String, Vec<u8>)> = Vec::new();
    for e in std::fs::read_dir(root.join("cart"))? {
        let e = e?;
        if e.file_type()?.is_file() {
            cart.push((e.file_name().to_string_lossy().into_owned(), std::fs::read(e.path())?));
        }
    }
    cart.sort();
    const IRRELEVANT: [&str; 3] = ["CELESTE_THREADS", "CELESTE_LEVEL_MINUS_ONE", "CELESTE_L1_CACHE"];
    let mut env: Vec<(String, String)> = std::env::vars().filter(|(k, _)| k.starts_with("CELESTE_") && !IRRELEVANT.contains(&k.as_str())).collect();
    env.sort();
    let room = celeste_interp::game_runner::start_room();
    // Two 64-bit SipHashes under different prefixes: a 128-bit name.
    let half = |salt: u8| {
        let mut h = std::collections::hash_map::DefaultHasher::new();
        (salt, CACHE_FORMAT, &exe_bytes, &lua, &cart, &env, room, spd_px).hash(&mut h);
        h.finish()
    };
    let name = format!("{:016x}{:016x}", half(1), half(2));
    let key = format!(
        "level -1 table cache, format {CACHE_FORMAT}\nroom {room:?}, S = {spd_px}\nbinary {} ({} bytes)\nenv {env:?}\ncart files {:?}\n",
        exe.display(),
        exe_bytes.len(),
        cart.iter().map(|c| &c.0).collect::<Vec<_>>()
    );
    Ok(Some((dir.join(format!("room{}_{}_s{spd_px}_{name}.bin", room.0, room.1)), key)))
}

impl CostToGo {
    /// The cached table at `path`, `None` when there is none.
    fn load(path: &FsPath) -> Result<Option<CostToGo>> {
        let b = match std::fs::read(path) {
            Ok(b) => b,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e.into()),
        };
        let mut at = 0usize;
        let mut take = |n: usize| -> Result<&[u8]> {
            let s = b.get(at..at + n).ok_or_else(|| anyhow!("truncated"))?;
            at += n;
            Ok(s)
        };
        ensure!(take(4)? == b"CL1T", "not a level -1 table");
        let u32_of = |s: &[u8]| u32::from_le_bytes(s.try_into().unwrap());
        let u64_of = |s: &[u8]| u64::from_le_bytes(s.try_into().unwrap());
        ensure!(u32_of(take(4)?) == CACHE_FORMAT, "another format");
        let start_d = u32_of(take(4)?);
        let n = u64_of(take(8)?) as usize;
        let mut d = HashMap::with_capacity(n);
        for _ in 0..n {
            let e = take(16)?;
            let (h, x, y, v) = (u64_of(&e[0..8]), i16::from_le_bytes([e[8], e[9]]), i16::from_le_bytes([e[10], e[11]]), u32_of(&e[12..16]));
            d.insert((h, x, y), v);
        }
        let fp = u64_of(take(8)?);
        ensure!(at == b.len(), "trailing bytes");
        let t = CostToGo { d, start_d };
        ensure!(t.fingerprint() == fp, "the fingerprint does not match the entries");
        Ok(Some(t))
    }

    /// Write the table to `path` (atomically: a reader sees all of it or
    /// none), with `key` beside it as text.
    fn save(&self, path: &FsPath, key: &str) -> Result<()> {
        if let Some(dir) = path.parent() {
            std::fs::create_dir_all(dir)?;
        }
        let mut es: Vec<(&(u64, i16, i16), &u32)> = self.d.iter().collect();
        es.sort_unstable();
        let mut b: Vec<u8> = Vec::with_capacity(28 + 16 * es.len());
        b.extend(b"CL1T");
        b.extend(CACHE_FORMAT.to_le_bytes());
        b.extend(self.start_d.to_le_bytes());
        b.extend((es.len() as u64).to_le_bytes());
        for ((h, x, y), v) in es {
            b.extend(h.to_le_bytes());
            b.extend(x.to_le_bytes());
            b.extend(y.to_le_bytes());
            b.extend(v.to_le_bytes());
        }
        b.extend(self.fingerprint().to_le_bytes());
        let tmp = path.with_extension(format!("tmp{}", std::process::id()));
        std::fs::write(&tmp, &b)?;
        std::fs::rename(&tmp, path)?;
        std::fs::write(path.with_extension("txt"), key)?;
        Ok(())
    }
}

/// A node `(shape, x, y)` as one word: the key of every map the passes keep.
fn pack(k: (usize, i16, i16)) -> u64 {
    debug_assert!(k.0 < 1 << 32);
    (k.0 as u64) << 32 | (k.1 as u16 as u64) << 16 | k.2 as u16 as u64
}

fn unpack(p: u64) -> (usize, i16, i16) {
    ((p >> 32) as usize, (p >> 16) as u16 as i16, p as u16 as i16)
}

/// What a node's frame is a function of besides its cell: the shape's trace
/// and the ranges its seeds read (`eval_node`).
#[derive(Clone, PartialEq)]
struct Sig {
    generation: u64,
    ranges: Vec<Range>,
}

impl Table<'_> {
    /// The shape's `Sig`, `None` while it is not traced.
    fn sig(&self, id: usize) -> Option<Sig> {
        let sh = &self.shapes[id];
        let t = sh.traced.as_ref()?;
        let ranges = t
            .seeds
            .iter()
            .filter_map(|s| match s {
                Seed::Range(p) => Some(sh.ranges[p]),
                _ => None,
            })
            .collect();
        Some(Sig { generation: t.generation, ranges })
    }
}

/// One node's frame as the passes read it: `eval_node`'s answer with its
/// boxes expanded, and the node's share of the counts.
struct NodeEval {
    /// The successor nodes (packed), sorted, each once.
    succ: Vec<u64>,
    exit: bool,
    /// A successor without a located object (a death).
    death: bool,
    /// Death successors (outcome x configuration), for the report.
    n_deaths: u32,
    /// A successor box was clipped to `WINDOW`.
    clipped: bool,
    /// Clipped successor boxes per target shape (the nonzero ones).
    clipped_by: Vec<(u32, u64)>,
    n_violations: u32,
    /// The first violation's message.
    violation: Option<String>,
    precise: bool,
}

/// Node answers kept across passes. A node's `NodeEval` and observations are
/// a function of its cell and its shape's `Sig`, so while the `Sig` holds the
/// node is not evaluated again. Observations are kept per shape as the join
/// over exactly the nodes `nodes` holds of it (`agg`, `count`): a pass that
/// reaches every one of them again joins `agg` as it is; one that does not
/// observes the reached ones again (`observe`).
#[derive(Default)]
struct Memo {
    nodes: rustc_hash::FxHashMap<u64, NodeEval>,
    sig: Vec<Option<Sig>>,
    agg: Vec<Vec<Option<Obs>>>,
    count: Vec<usize>,
}

impl Memo {
    /// Forget every shape whose `Sig` is not `sigs`'.
    fn keep(&mut self, sigs: &[Option<Sig>]) {
        self.grow(sigs.len(), 0);
        let stale: Vec<bool> = (0..self.sig.len()).map(|s| self.sig[s].is_none() || self.sig[s] != sigs[s]).collect();
        self.nodes.retain(|k, _| !stale[unpack(*k).0]);
        for (s, st) in stale.iter().enumerate() {
            if *st {
                self.sig[s] = None;
                self.agg[s] = Vec::new();
                self.count[s] = 0;
            }
        }
    }

    fn grow(&mut self, n_shapes: usize, n_obs: usize) {
        if self.sig.len() < n_shapes {
            self.sig.resize(n_shapes, None);
            self.agg.resize(n_shapes, Vec::new());
            self.count.resize(n_shapes, 0);
        }
        for a in &mut self.agg {
            if a.len() < n_obs {
                a.resize(n_obs, None);
            }
        }
    }
}

fn join_obs(into: &mut [Option<Obs>], from: &[Option<Obs>]) -> Result<()> {
    for (a, b) in into.iter_mut().zip(from) {
        if let Some(o) = b {
            *a = Some(match *a {
                Some(p) => p.join(*o)?,
                None => *o,
            });
        }
    }
    Ok(())
}

/// One node in the pass's graph: its successors' ids and its flags.
struct Rec {
    edges: Vec<u32>,
    exit: bool,
    death: bool,
    clipped: bool,
}

/// Evaluate node `k`: its `NodeEval`, its observations joined into `obs`.
fn eval_key(shapes: &[Shape], room: &Room, k: u64, n_obs: usize, obs: &mut [Option<Obs>]) -> Result<NodeEval> {
    let (id, x, y) = unpack(k);
    let mut acc = Acc { obs: vec![None; n_obs], clipped: vec![0; shapes.len()], violations: Vec::new(), n_violations: 0, precise: 0 };
    let out = eval_node(shapes, room, id, (x, y), &mut acc)?;
    join_obs(obs, &acc.obs)?;
    let mut succ: Vec<u64> = Vec::new();
    let (mut exit, mut n_deaths) = (false, 0u32);
    for (tid, bx) in out.succ {
        if tid == usize::MAX {
            exit = true;
            continue;
        }
        // An outcome without a located object is a death: no edge
        // (plans/level-minus-one.md); `sound_d` seeds it with the respawn.
        let Some((xl, xh, yl, yh)) = bx else {
            n_deaths += 1;
            continue;
        };
        for x in xl..=xh {
            for y in yl..=yh {
                succ.push(pack((tid, x, y)));
            }
        }
    }
    succ.sort_unstable();
    succ.dedup();
    Ok(NodeEval {
        succ,
        exit,
        death: n_deaths > 0,
        n_deaths,
        clipped: out.clipped,
        clipped_by: acc.clipped.iter().enumerate().filter(|(_, c)| **c > 0).map(|(t, c)| (t as u32, *c)).collect(),
        n_violations: acc.n_violations as u32,
        violation: acc.violations.into_iter().next(),
        precise: acc.precise > 0,
    })
}

/// The observations of `keys` (nodes of traced shapes), evaluated again.
fn observe(shapes: &[Shape], room: &Room, keys: &[u64], threads: usize, n_obs: usize) -> Result<Vec<Option<Obs>>> {
    let next = std::sync::atomic::AtomicUsize::new(0);
    let results: Vec<Result<Vec<Option<Obs>>>> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                let next = &next;
                scope.spawn(move || -> Result<Vec<Option<Obs>>> {
                    let mut obs = vec![None; n_obs];
                    while let Some(&k) = keys.get(next.fetch_add(1, std::sync::atomic::Ordering::Relaxed)) {
                        eval_key(shapes, room, k, n_obs, &mut obs)?;
                    }
                    Ok(obs)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("level -1 worker panicked")).collect()
    });
    let mut obs = vec![None; n_obs];
    for r in results {
        join_obs(&mut obs, &r?)?;
    }
    Ok(obs)
}

/// The work list the workers share.
struct Queue {
    stack: Vec<(u32, u64)>,
    /// Workers evaluating a node (and so maybe about to push more).
    active: usize,
    pending: Vec<(u32, u64)>,
    failed: bool,
}

/// One pass's walk over the reachable nodes: every worker takes a node,
/// answers it (from the memo, else `eval_key`), and registers its successors
/// - no layers, no serial merge between them. A node of a shape not traced
/// yet is set aside (`pending`) for the caller to trace and resume.
struct Explore {
    /// Node id -> key, in order of discovery.
    keys: Vec<u64>,
    index: rustc_hash::FxHashMap<u64, u32>,
    recs: Vec<Option<Rec>>,
    stack: Vec<(u32, u64)>,
    pending: Vec<(u32, u64)>,
    /// Evaluated this pass (for the memo).
    fresh: Vec<(u64, NodeEval)>,
    /// This pass's evaluations' observations, per shape.
    fresh_obs: Vec<Option<Vec<Option<Obs>>>>,
    /// Memo answers taken, per shape.
    hits: Vec<usize>,
    n_violations: usize,
    n_precise: usize,
    n_deaths: u64,
    clipped: Vec<u64>,
    violations: Vec<String>,
}

impl Explore {
    fn new(root: u64) -> Explore {
        let mut index: rustc_hash::FxHashMap<u64, u32> = HashMap::default();
        index.insert(root, 0);
        Explore {
            keys: vec![root],
            index,
            recs: Vec::new(),
            stack: vec![(0, root)],
            pending: Vec::new(),
            fresh: Vec::new(),
            fresh_obs: Vec::new(),
            hits: Vec::new(),
            n_violations: 0,
            n_precise: 0,
            n_deaths: 0,
            clipped: Vec::new(),
            violations: Vec::new(),
        }
    }

    /// Answer every node reachable from `stack` through traced shapes.
    fn run(&mut self, shapes: &[Shape], room: &Room, memo: &Memo, threads: usize, n_obs: usize) -> Result<()> {
        use std::sync::{Condvar, Mutex};
        let n_shapes = shapes.len();
        let traced = |k: u64| shapes[unpack(k).0].traced.is_some();
        let (ready, waiting): (Vec<_>, Vec<_>) = std::mem::take(&mut self.stack).into_iter().partition(|(_, k)| traced(*k));
        self.pending.extend(waiting);
        let queue = Mutex::new(Queue { stack: ready, active: 0, pending: Vec::new(), failed: false });
        let wake = Condvar::new();
        let index = Mutex::new((std::mem::take(&mut self.index), std::mem::take(&mut self.keys)));
        /// What a worker hands back.
        struct Done {
            recs: Vec<(u32, Rec)>,
            fresh: Vec<(u64, NodeEval)>,
            obs: Vec<Option<Vec<Option<Obs>>>>,
            hits: Vec<usize>,
            n_violations: usize,
            n_precise: usize,
            n_deaths: u64,
            clipped: Vec<u64>,
            violations: Vec<String>,
        }
        let results: Vec<Result<Done>> = std::thread::scope(|scope| {
            let hs: Vec<_> = (0..threads)
                .map(|_| {
                    let (queue, wake, index, traced) = (&queue, &wake, &index, &traced);
                    scope.spawn(move || -> Result<Done> {
                        // A worker that panics stops the others, which would
                        // otherwise wait for its node forever.
                        struct OnPanic<'q>(&'q Mutex<Queue>, &'q Condvar);
                        impl Drop for OnPanic<'_> {
                            fn drop(&mut self) {
                                if std::thread::panicking() {
                                    self.0.lock().unwrap_or_else(|e| e.into_inner()).failed = true;
                                    self.1.notify_all();
                                }
                            }
                        }
                        let _on_panic = OnPanic(queue, wake);
                        let mut d = Done { recs: Vec::new(), fresh: Vec::new(), obs: vec![None; n_shapes], hits: vec![0; n_shapes], n_violations: 0, n_precise: 0, n_deaths: 0, clipped: vec![0; n_shapes], violations: Vec::new() };
                        loop {
                            let (n, k) = {
                                let mut q = queue.lock().unwrap();
                                loop {
                                    if q.failed {
                                        return Ok(d);
                                    }
                                    if let Some(item) = q.stack.pop() {
                                        q.active += 1;
                                        break item;
                                    }
                                    if q.active == 0 {
                                        return Ok(d);
                                    }
                                    q = wake.wait(q).unwrap();
                                }
                            };
                            let s = unpack(k).0;
                            let mut fresh_e: Option<NodeEval> = None;
                            let e: &NodeEval = match memo.nodes.get(&k) {
                                Some(e) => {
                                    d.hits[s] += 1;
                                    e
                                }
                                None => {
                                    let obs = d.obs[s].get_or_insert_with(|| vec![None; n_obs]);
                                    match eval_key(shapes, room, k, n_obs, obs) {
                                        Ok(e) => fresh_e.insert(e),
                                        Err(err) => {
                                            let mut q = queue.lock().unwrap();
                                            q.failed = true;
                                            q.active -= 1;
                                            wake.notify_all();
                                            return Err(err);
                                        }
                                    }
                                }
                            };
                            d.n_violations += e.n_violations as usize;
                            d.n_precise += e.precise as usize;
                            d.n_deaths += e.n_deaths as u64;
                            for &(t, c) in &e.clipped_by {
                                d.clipped[t as usize] += c;
                            }
                            if let Some(v) = &e.violation {
                                if d.violations.len() < 12 {
                                    d.violations.push(v.clone());
                                }
                            }
                            let mut edges = Vec::with_capacity(e.succ.len());
                            let mut new: Vec<(u32, u64)> = Vec::new();
                            {
                                let mut ix = index.lock().unwrap();
                                let (map, keys) = &mut *ix;
                                for &t in &e.succ {
                                    let id = match map.get(&t) {
                                        Some(&id) => id,
                                        None => {
                                            let id = keys.len() as u32;
                                            map.insert(t, id);
                                            keys.push(t);
                                            new.push((id, t));
                                            id
                                        }
                                    };
                                    edges.push(id);
                                }
                            }
                            d.recs.push((n, Rec { edges, exit: e.exit, death: e.death, clipped: e.clipped }));
                            if let Some(f) = fresh_e {
                                d.fresh.push((k, f));
                            }
                            let mut q = queue.lock().unwrap();
                            for item in new {
                                if traced(item.1) {
                                    q.stack.push(item);
                                } else {
                                    q.pending.push(item);
                                }
                            }
                            q.active -= 1;
                            wake.notify_all();
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("level -1 worker panicked")).collect()
        });
        let (index, keys) = index.into_inner().unwrap();
        self.index = index;
        self.keys = keys;
        self.pending.extend(queue.into_inner().unwrap().pending);
        self.recs.resize_with(self.keys.len(), || None);
        self.hits.resize(n_shapes, 0);
        self.fresh_obs.resize_with(n_shapes, || None);
        self.clipped.resize(n_shapes, 0);
        let mut first_err = None;
        for r in results {
            let d = match r {
                Ok(d) => d,
                Err(e) => {
                    first_err.get_or_insert(e);
                    continue;
                }
            };
            for (n, rec) in d.recs {
                self.recs[n as usize] = Some(rec);
            }
            self.fresh.extend(d.fresh);
            for (s, o) in d.obs.into_iter().enumerate() {
                if let Some(o) = o {
                    match &mut self.fresh_obs[s] {
                        Some(a) => {
                            a.resize(n_obs, None);
                            join_obs(a, &o)?;
                        }
                        a @ None => *a = Some(o),
                    }
                }
            }
            for (h, x) in self.hits.iter_mut().zip(&d.hits) {
                *h += x;
            }
            self.n_violations += d.n_violations;
            self.n_precise += d.n_precise;
            self.n_deaths += d.n_deaths;
            for (c, x) in self.clipped.iter_mut().zip(&d.clipped) {
                *c += x;
            }
            for v in d.violations {
                if self.violations.len() < 12 {
                    self.violations.push(v);
                }
            }
        }
        match first_err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }
}

impl Graph1 {
    /// The pass's graph, its nodes in KEY order (the walk's discovery order
    /// depends on the scheduling; the table does not).
    fn from_explore(keys: Vec<u64>, recs: Vec<Option<Rec>>) -> Graph1 {
        let mut order: Vec<u32> = (0..keys.len() as u32).collect();
        order.sort_unstable_by_key(|&i| keys[i as usize]);
        let mut renum = vec![0u32; keys.len()];
        for (new, &old) in order.iter().enumerate() {
            renum[old as usize] = new as u32;
        }
        let nodes: Vec<(usize, i16, i16)> = order.iter().map(|&i| unpack(keys[i as usize])).collect();
        let index = nodes.iter().enumerate().map(|(i, k)| (*k, i as u32)).collect();
        let mut recs = recs;
        let mut g = Graph1 { nodes, index, edges: Vec::with_capacity(order.len()), exits: Vec::new(), deaths: Vec::new(), clipped: Vec::new() };
        for &old in &order {
            let r = recs[old as usize].take().expect("every reached node is answered");
            let mut es: Vec<u32> = r.edges.iter().map(|e| renum[*e as usize]).collect();
            es.sort_unstable();
            g.edges.push(es);
            g.exits.push(r.exit);
            g.deaths.push(r.death);
            g.clipped.push(r.clipped);
        }
        g
    }
}

/// The table: the lattice walk, the spawn chain, the passes to inductive ranges,
/// and d.
fn build(root: &FsPath, spd_px: i32, threads: usize, rep: &mut String) -> Result<Built> {
    let t_all = std::time::Instant::now();
    let room0 = celeste_interp::game_runner::start_room();
    let lw = super::kernel::room_constant_lattice(root, super::shapes::WalkOpts::LEVEL0.with_held(true))?;
    say!(rep, "room {room0:?}: lattice walk {} shapes, {:.1} s", lw.lattice.len(), t_all.elapsed().as_secs_f64())?;
    let mut tr = lw.tracer.clone();
    let mut table = Table {
        lw: &lw,
        room: Room { cart: lw.cart.clone(), cache: lw.cache.clone() },
        room0,
        spd_px,
        shapes: Vec::new(),
        by_key: HashMap::new(),
        obs_keys: Vec::new(),
        obs_index: BTreeMap::new(),
    };
    let start_id = table.shape_id(&lw.start_key)?;
    let start_xy = {
        let st = &lw.reps[&lw.start_key];
        let o = located_object(st).ok_or_else(|| anyhow!("the start state has no player or spawn"))?;
        let at = |f: &str| -> Result<i16> {
            let Some(Value::Num(n)) = iface::get(st, &with(&o, &[f])) else { bail!("start object {f} is not a number") };
            let v = lw.tracer.it.d.as_const(&n).ok_or_else(|| anyhow!("start object {f} is not a constant"))?;
            ensure!(raw(v) & 0xffff == 0, "start object {f} is not a whole pixel");
            Ok((raw(v) >> 16) as i16)
        };
        (at("x")?, at("y")?)
    };

    // THE SPAWN PREFIX. The player spawn reads no button and its `state`,
    // `delay` and speed are one range per shape at level -1, so as a
    // position-only node it can rise forever (it has `solids=false`): its
    // ranges never converge. So the prefix is run as a CHAIN instead: each
    // frame evaluated at the previous frame's own values (the same traced
    // frame, the same evaluator), which must give exactly one successor
    // cell, until the successor has a player. That player state seeds the
    // table.
    let t_chain = std::time::Instant::now();
    let mut chain: Vec<(usize, (i16, i16))> = vec![(start_id, start_xy)];
    let (end_id, end_xy) = loop {
        let (id, xy) = *chain.last().expect("the chain starts with the start state");
        ensure!(chain.len() <= 256, "the spawn prefix did not reach a player in 256 frames");
        table.shapes[id].traced = None;
        table.trace(&mut tr, id)?;
        let mut acc = Acc { obs: vec![None; table.obs_keys.len()], clipped: vec![0; table.shapes.len()], violations: Vec::new(), n_violations: 0, precise: 0 };
        let out = eval_node(&table.shapes, &table.room, id, xy, &mut acc)?;
        let f = chain.len() - 1;
        ensure!(acc.n_violations == 0 && acc.clipped.iter().all(|c| *c == 0), "the spawn prefix at frame {f} {xy:?}: {:?} (clipped {:?})", acc.violations, acc.clipped);
        let succ: BTreeSet<(usize, Option<(i16, i16, i16, i16)>)> = out.succ.into_iter().collect();
        ensure!(succ.len() == 1, "the spawn prefix at frame {f} {xy:?}: {} successors {succ:?}, not one", succ.len());
        let (tid, bx) = succ.into_iter().next().expect("one successor");
        let Some((xl, _, yl, _)) = bx.filter(|b| b.0 == b.1 && b.2 == b.3) else {
            bail!("the spawn prefix at frame {f} {xy:?}: the successor is not one cell ({tid}, {bx:?})")
        };
        // The successor's values are its seeds.
        let mut ranges: BTreeMap<Path, Range> = BTreeMap::new();
        let mut lits: BTreeSet<Path> = BTreeSet::new();
        let tkey = table.shapes[tid].key.clone();
        for (slot, o) in acc.obs.iter().enumerate() {
            let (sid, p) = &table.obs_keys[slot];
            if *sid != tid {
                continue;
            }
            match o {
                Some(Obs::Lit(lo, hi)) if table.lit_slot(&tkey, p) => {
                    ranges.insert(p.clone(), (*lo, *hi));
                    lits.insert(p.clone());
                }
                Some(Obs::Num(lo, hi) | Obs::Lit(lo, hi)) => {
                    ranges.insert(p.clone(), (*lo, *hi));
                }
                _ => {}
            }
        }
        table.shapes[tid].lits = lits;
        chain.push((tid, (xl, yl)));
        if let Some(pl) = super::shapes::player_path(&lw.reps[&table.shapes[tid].key]) {
            // The player's speed range joins the successor's own speed.
            let sp = (spd_px as i64) << 16;
            for ax in ["x", "y"] {
                let q = with(&pl, &["spd", ax]);
                let r = ranges.get(&q).copied().unwrap_or((0, 0));
                ranges.insert(q, (r.0.min(-sp), r.1.max(sp)));
            }
            table.shapes[tid].ranges = ranges;
            table.shapes[tid].traced = None;
            break (tid, (xl, yl));
        }
        table.shapes[tid].ranges = ranges;
    };
    say!(rep, "spawn prefix: {} frames, {:?} -> shape {end_id} at {end_xy:?}, {:.1} s", chain.len() - 1, chain.iter().map(|c| c.1).collect::<Vec<_>>(), t_chain.elapsed().as_secs_f64())?;

    let mut pass = 0usize;
    // How often each range has grown (`widen`).
    let mut grown: BTreeMap<(usize, Path), u32> = BTreeMap::new();
    // Node answers kept across passes: a node of a shape whose trace and seed
    // ranges did not change is not evaluated again (`Memo`).
    let mut memo = Memo::default();
    let g1 = loop {
        pass += 1;
        ensure!(pass <= 16, "the level -1 ranges did not converge in 16 passes");
        let t_pass = std::time::Instant::now();
        let mut t_trace = std::time::Duration::ZERO;
        let sigs: Vec<Option<Sig>> = (0..table.shapes.len()).map(|s| table.sig(s)).collect();
        memo.keep(&sigs);
        let mut ex = Explore::new(pack((end_id, end_xy.0, end_xy.1)));
        loop {
            ex.run(&table.shapes, &table.room, &memo, threads, table.obs_keys.len())?;
            // Nodes of shapes not traced yet wait for their trace.
            let pending = std::mem::take(&mut ex.pending);
            if pending.is_empty() {
                break;
            }
            let tt = std::time::Instant::now();
            let ids: BTreeSet<usize> = pending.iter().map(|(_, k)| unpack(*k).0).collect();
            for id in ids {
                if table.shapes[id].traced.is_none() {
                    table.trace(&mut tr, id)?;
                    say!(rep, "  pass {pass}: traced shape {id}: {}", table.shapes[id].traced.as_ref().unwrap().stats)?;
                }
            }
            t_trace += tt.elapsed();
            ex.stack = pending;
        }
        let n_obs = table.obs_keys.len();
        let n_shapes = table.shapes.len();
        memo.grow(n_shapes, n_obs);
        // The observations, per shape: the memo's join (when every node it
        // holds was reached again) and this pass's evaluations.
        let mut obs: Vec<Option<Obs>> = vec![None; n_obs];
        let mut reached: Vec<Vec<u64>> = vec![Vec::new(); n_shapes];
        for &k in &ex.keys {
            reached[unpack(k).0].push(k);
        }
        let mut reobserved = 0usize;
        for s in 0..n_shapes {
            let hits = ex.hits.get(s).copied().unwrap_or(0);
            let mut agg: Vec<Option<Obs>> = if hits == memo.count[s] {
                std::mem::take(&mut memo.agg[s])
            } else {
                // A node the memo holds was not reached: its observations
                // are in the memo's join, so the reached ones it holds are
                // observed again and the rest forgotten.
                let again: Vec<u64> = reached[s].iter().copied().filter(|k| memo.nodes.contains_key(k)).collect();
                reobserved += again.len();
                let keep: HashSet<u64> = reached[s].iter().copied().collect();
                memo.nodes.retain(|k, _| unpack(*k).0 != s || keep.contains(k));
                observe(&table.shapes, &table.room, &again, threads, n_obs)?
            };
            agg.resize(n_obs, None);
            if let Some(o) = ex.fresh_obs.get_mut(s).and_then(|o| o.take()) {
                join_obs(&mut agg, &o)?;
            }
            join_obs(&mut obs, &agg)?;
            memo.agg[s] = agg;
            memo.count[s] = reached[s].len();
            memo.sig[s] = if reached[s].is_empty() { None } else { table.sig(s) };
        }
        let n_fresh = ex.fresh.len();
        for (k, e) in ex.fresh.drain(..) {
            memo.nodes.insert(k, e);
        }
        let Explore { keys, recs, n_violations, n_precise, n_deaths, mut clipped, violations, .. } = ex;
        clipped.resize(n_shapes, 0);
        let g = Graph1::from_explore(keys, recs);
        let n_edges: usize = g.edges.iter().map(|e| e.len()).sum();
        say!(
            rep,
            "pass {pass}: {} nodes, {} edges, {} exit nodes, {n_deaths} death successors; {:.1} s ({:.1} s tracing); {n_precise} nodes evaluated precisely; {n_violations} violations; {n_fresh} nodes evaluated, {} reused, {reobserved} observed again",
            g.nodes.len(),
            n_edges,
            g.exits.iter().filter(|e| **e).count(),
            t_pass.elapsed().as_secs_f64(),
            t_trace.as_secs_f64(),
            g.nodes.len() - n_fresh
        )?;
        if clipped.iter().any(|c| *c > 0) {
            say!(rep, "  successors clipped to the window, per target shape: {:?}", clipped.iter().enumerate().filter(|(_, c)| **c > 0).collect::<Vec<_>>())?;
        }
        for v in violations.iter().take(12) {
            say!(rep, "  VIOLATION {v}")?;
        }
        // The inductive check.
        let mut changes: Vec<String> = Vec::new();
        for (slot, (id, p)) in table.obs_keys.clone().iter().enumerate() {
            let Some(o) = obs[slot] else { continue };
            let eligible = table.lit_slot(&table.shapes[*id].key, p);
            let sh = &mut table.shapes[*id];
            let Some(t) = sh.traced.as_ref() else {
                // Observed flowing into a shape no node of reached: nothing to
                // check it against.
                continue;
            };
            if let Some(c) = t.pins.get(p) {
                let o = o.hull();
                let holds = match (c, o) {
                    (Conc::Num(v), Obs::Num(lo, hi)) => lo == raw(*v) && hi == raw(*v),
                    (Conc::Bool(b), Obs::Bool { f, t, u }) => !u && (if *b { !f } else { !t }),
                    _ => false,
                };
                if !holds {
                    changes.push(format!("shape {id}: pin {} = {c:?} does not hold ({o:?}): dropped", iface::show(p)));
                    if let (Conc::Num(v), Obs::Num(lo, hi)) = (c, o) {
                        sh.ranges.insert(p.clone(), widen((raw(*v), raw(*v)), (lo, hi), false));
                    }
                    sh.demoted.insert(p.clone());
                    sh.traced = None;
                }
                continue;
            }
            // Only the slots seeded from a range (not the enumerated cell).
            if !t.seeds.iter().any(|s| matches!(s, Seed::Range(q) if q == p)) {
                continue;
            }
            // A literal holds while exactly it flows in; anything else and the
            // slot is a hull of the lanes' values from now on.
            if sh.lits.contains(p) {
                let r = sh.ranges[p];
                if o != Obs::Lit(r.0, r.1) {
                    let Obs::Num(lo, hi) = o.hull() else { bail!("shape {id}: {} observed {o:?}", iface::show(p)) };
                    let w = (r.0.min(lo), r.1.max(hi));
                    changes.push(format!("shape {id}: the literal {} = [{}, {}] observed {o:?}: the range [{}, {}]", iface::show(p), px(r.0), px(r.1), px(w.0), px(w.1)));
                    sh.lits.remove(p);
                    sh.ranges.insert(p.clone(), w);
                    sh.traced = None;
                }
                continue;
            }
            if let (true, Obs::Lit(lo, hi), true) = (eligible, o, sh.unseeded.contains(p)) {
                sh.unseeded.remove(p);
                changes.push(format!("shape {id}: {} first observed the literal [{}, {}]", iface::show(p), px(lo), px(hi)));
                sh.ranges.insert(p.clone(), (lo, hi));
                sh.lits.insert(p.clone());
                sh.traced = None;
                continue;
            }
            if let (Some(r), Obs::Num(lo, hi)) = (sh.ranges.get(p).copied(), o.hull()) {
                if sh.unseeded.remove(p) {
                    if (lo, hi) != r {
                        changes.push(format!("shape {id}: {} first observed [{}, {}]", iface::show(p), px(lo), px(hi)));
                        sh.ranges.insert(p.clone(), (lo, hi));
                        if t.bounds.contains_key(p) {
                            sh.traced = None;
                        }
                    }
                    continue;
                }
                if lo < r.0 || hi > r.1 {
                    // A rem stays in [-0.5, 0.5) by `move`'s own arithmetic:
                    // one outside is a finding, not a range to widen.
                    ensure!(!is_rem(p) || (lo >= REM.0 && hi <= REM.1), "shape {id}: {} = [{}, {}] leaves rem's [-0.5, 0.5)", iface::show(p), px(lo), px(hi));
                    let n = grown.entry((*id, p.clone())).or_insert(0);
                    *n += 1;
                    let w = widen(r, (lo, hi), *n > 2);
                    let w = if is_rem(p) { (w.0.max(REM.0), w.1.min(REM.1)) } else { w };
                    changes.push(format!("shape {id}: {} [{}, {}] observed [{}, {}]: widened to [{}, {}]", iface::show(p), px(r.0), px(r.1), px(lo), px(hi), px(w.0), px(w.1)));
                    sh.ranges.insert(p.clone(), w);
                    if t.bounds.contains_key(p) {
                        sh.traced = None;
                    }
                }
            }
        }
        for c in changes.iter().take(40) {
            say!(rep, "  {c}")?;
        }
        if changes.len() > 40 {
            say!(rep, "  ... {} changes in all", changes.len())?;
        }
        if changes.is_empty() {
            ensure!(n_violations == 0, "the converged table has {n_violations} violations (above): it is not sound");
            break g;
        }
    };

    // The final ranges.
    say!(rep, "\nconverged after {pass} passes; the ranges (px), per shape:")?;
    for (id, sh) in table.shapes.iter().enumerate() {
        let Some(t) = &sh.traced else { continue };
        let n = g1.nodes.iter().filter(|k| k.0 == id).count();
        say!(rep, "shape {id} ({:#x}): {n} nodes; {}", lw.frames.iter().find(|((k, _), _)| *k == sh.key).map(|(_, wf)| wf.frame.in_rt2.shape_hash_of()).unwrap_or(0), t.stats)?;
        let rs: Vec<String> = sh
            .ranges
            .iter()
            .filter(|(p, _)| t.seeds.iter().any(|s| matches!(s, Seed::Range(q) if q == *p)))
            .map(|(p, r)| {
                if r.0 == r.1 {
                    format!("{}={}", iface::show(p), px(r.0))
                } else if sh.lits.contains(p) {
                    format!("{}=literal[{},{}]", iface::show(p), px(r.0), px(r.1))
                } else {
                    format!("{}=[{},{}]", iface::show(p), px(r.0), px(r.1))
                }
            })
            .collect();
        say!(rep, "    {}", rs.join(" "))?;
        if !sh.demoted.is_empty() {
            say!(rep, "    pins dropped: {}", sh.demoted.iter().map(iface::show).collect::<Vec<_>>().join(" "))?;
        }
    }

    // d: fewest frames to an exit, backward from the nodes with an exit edge.
    let n = g1.nodes.len();
    let mut rev: Vec<Vec<u32>> = vec![Vec::new(); n];
    for (s, es) in g1.edges.iter().enumerate() {
        for &e in es {
            rev[e as usize].push(s as u32);
        }
    }
    let mut dist = vec![u32::MAX; n];
    let mut q: VecDeque<u32> = VecDeque::new();
    for i in 0..n {
        if g1.exits[i] {
            dist[i] = 1;
            q.push_back(i as u32);
        }
    }
    while let Some(i) = q.pop_front() {
        let dn = dist[i as usize] + 1;
        for &s in &rev[i as usize] {
            if dist[s as usize] == u32::MAX {
                dist[s as usize] = dn;
                q.push_back(s);
            }
        }
    }
    let end_node = g1.index[&(end_id, end_xy.0, end_xy.1)];
    say!(
        rep,
        "\nd: {} of {n} nodes reach an exit; the prefix's end d = {}, so the start state's d = {}; max finite d = {}",
        dist.iter().filter(|d| **d != u32::MAX).count(),
        dist[end_node as usize],
        dist[end_node as usize].saturating_add(chain.len() as u32 - 1),
        dist.iter().filter(|d| **d != u32::MAX).max().copied().unwrap_or(0)
    )?;
    let sound = sound_d(&g1, &rev, chain.len() as u32 - 1, end_node);
    let start_d = sound[end_node as usize].saturating_add(chain.len() as u32 - 1);
    say!(
        rep,
        "sound d (a death costs a respawn; clipped parts dropped, the window checked on every filtered row): {} of {n} nodes finite; the start state's d = {start_d}; {} clipped and {} death nodes",
        sound.iter().filter(|d| **d != u32::MAX).count(),
        g1.clipped.iter().filter(|c| **c).count(),
        g1.deaths.iter().filter(|c| **c).count()
    )?;
    let mut by_hash = HashMap::new();
    for (h, k) in &lw.by_hash {
        if let Some(&id) = table.by_key.get(k) {
            by_hash.insert(*h, id);
        }
    }
    Ok(Built { by_hash, g1, sound, chain, start_d })
}

pub fn probe(root: &FsPath, opts: &Opts) -> Result<String> {
    let mut rep = String::new();
    let t_all = std::time::Instant::now();
    let Built { by_hash, g1, sound, chain, .. } = build(root, opts.spd_px, opts.threads, &mut rep)?;

    // The d map: per 8 px tile, the smallest sound d of any table node in it
    // (0-9, then a-z for 10-35, '+' above, '*' no exit path, '.' no node).
    say!(rep, "\nsound d per 8 px tile, the smallest over the table's shapes:")?;
    let mut best: HashMap<(i16, i16), u32> = HashMap::new();
    for (i, &(_, x, y)) in g1.nodes.iter().enumerate() {
        let e = best.entry((x.div_euclid(8), y.div_euclid(8))).or_insert(u32::MAX);
        *e = (*e).min(sound[i]);
    }
    for ty in -2i16..=17 {
        let line: String = (-2i16..=17)
            .map(|tx| match best.get(&(tx, ty)) {
                None => '.',
                Some(&u32::MAX) => '*',
                Some(&d) if d < 10 => (b'0' + d as u8) as char,
                Some(&d) if d < 36 => (b'a' + (d - 10) as u8) as char,
                Some(_) => '+',
            })
            .collect();
        say!(rep, "{:4} {line}", ty as i32 * 8)?;
    }
    // `CELESTE_L1_PATH="x,y"`: the table's fastest path from that cell, one
    // successor of d - 1 per step, to see HOW it gets to the exit.
    if let Ok(s) = std::env::var("CELESTE_L1_PATH") {
        let (x, y) = s
            .split_once(',')
            .and_then(|(a, b)| Some((a.trim().parse::<i16>().ok()?, b.trim().parse::<i16>().ok()?)))
            .ok_or_else(|| anyhow!("CELESTE_L1_PATH=\"x,y\", not {s:?}"))?;
        let show = |i: usize| format!("({}, {}) shape {} d {}", g1.nodes[i].1, g1.nodes[i].2, g1.nodes[i].0, sound[i]);
        match g1.nodes.iter().enumerate().filter(|(_, n)| n.1 == x && n.2 == y).min_by_key(|(i, _)| sound[*i]).map(|(i, _)| i) {
            None => say!(rep, "\npath from ({x}, {y}): not a table node")?,
            Some(mut i) => {
                let mut steps = vec![show(i)];
                while sound[i] > 1 && sound[i] != u32::MAX && steps.len() < 64 {
                    let Some(&next) = g1.edges[i].iter().find(|&&e| sound[e as usize] == sound[i] - 1) else {
                        steps.push("(no successor of d - 1: a death seed)".to_string());
                        break;
                    };
                    i = next as usize;
                    steps.push(show(i));
                }
                say!(rep, "\npath from ({x}, {y}), the table's fastest:\n  {}{}", steps.join("\n  "), if g1.exits[i] { "\n  EXIT" } else { "" })?;
            }
        }
    }

    // The soundness check against the recorded transitions.
    use crate::search::pos_graph::{cell_xy, PosGraph, CELL_COUNT, NO_CELL};
    let pg = PosGraph::load(&crate::frame::pos_graph_path(&opts.level_dir))?;
    let mut pos_edges: HashSet<(i16, i16, i16, i16)> = HashSet::new();
    let mut exit_src: HashSet<(i16, i16)> = HashSet::new();
    for w in chain.windows(2) {
        pos_edges.insert((w[0].1 .0, w[0].1 .1, w[1].1 .0, w[1].1 .1));
    }
    for (s, es) in g1.edges.iter().enumerate() {
        let (_, sx, sy) = g1.nodes[s];
        for &e in es {
            let (_, dx, dy) = g1.nodes[e as usize];
            pos_edges.insert((sx, sy, dx, dy));
        }
        if g1.exits[s] {
            exit_src.insert((sx, sy));
        }
    }
    let xy_of = |c: u32| -> (i16, i16) { if c == NO_CELL { (NONE, NONE) } else { cell_xy(c).map(|(x, y)| (x as i16, y as i16)).expect("a grid cell") } };
    let (mut checked, mut crossings, mut deaths, mut bad) = (0u64, 0u64, 0u64, Vec::new());
    for d in 0..CELL_COUNT as u32 {
        let (dx, dy) = xy_of(d);
        for &s in pg.srcs_of(d) {
            let (sx, sy) = xy_of(s);
            if sx != NONE && sx >= 128 {
                continue;
            }
            // A death or a respawn: not modelled (dead ends).
            if sx == NONE || dx == NONE {
                deaths += 1;
                continue;
            }
            if dx != NONE && dx >= 128 {
                crossings += 1;
                if !exit_src.contains(&(sx, sy)) {
                    bad.push(format!("({sx}, {sy}) -> exit"));
                }
                continue;
            }
            checked += 1;
            if !pos_edges.contains(&(sx, sy, dx, dy)) {
                bad.push(format!("({sx}, {sy}) -> ({dx}, {dy})"));
            }
        }
    }
    say!(rep, "\nsoundness vs {}: {} recorded pairs, {checked} in the room checked, {crossings} crossings checked, {deaths} deaths and respawns not modelled; {} VIOLATIONS", opts.level_dir.display(), pg.pairs(), bad.len())?;
    for b in bad.iter().take(20) {
        say!(rep, "  not allowed: {b}")?;
    }

    // The comparison with the tree.
    let marks = match &opts.marks {
        Some(p) => {
            let v = crate::frame::Visited::load(p)?;
            say!(rep, "\n{} marked states in {}", v.len(), p.display())?;
            Some(v)
        }
        None => None,
    };
    say!(rep, "\nframe | states | too late (f + d > {}) | no exit path | band {},8 cut | death (kept) | left the room | other shape | not in the table | marked | MARKED TOO LATE", opts.ceiling, opts.ceiling)?;
    let mut samples: BTreeSet<String> = BTreeSet::new();
    for f in opts.from..=opts.to {
        let (mut states, mut late, mut nopath, mut band, mut dead, mut left, mut other, mut missing) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
        let (mut marked, mut marked_late) = (0u64, 0u64);
        for (_, ff) in crate::frame::frame_files(&opts.level_dir, f)? {
            let id = by_hash.get(&ff.shape_hash()).copied();
            let mut late_cells: rustc_hash::FxHashSet<u32> = Default::default();
            for (cell, rows) in ff.cell_counts() {
                let rows = rows as u64;
                states += rows;
                // What the time band (`frame::cell_too_late`, 8 px) drops.
                if crate::frame::cell_too_late(cell, f, opts.ceiling, 8) {
                    band += rows;
                }
                if cell == NO_CELL {
                    dead += rows;
                    continue;
                }
                let (x, y) = xy_of(cell);
                if x != NONE && x >= 128 {
                    left += rows;
                    continue;
                }
                let Some(id) = id else {
                    other += rows;
                    if samples.len() < 24 {
                        samples.insert(format!("f{f:03}: {rows} rows of shape {:#x} (not a table shape) at ({x}, {y})", ff.shape_hash()));
                    }
                    continue;
                };
                match g1.index.get(&(id, x, y)) {
                    None => {
                        missing += rows;
                        if samples.len() < 24 {
                            samples.insert(format!("f{f:03}: {rows} rows of shape {id} at ({x}, {y}): not a table node"));
                        }
                    }
                    Some(&i) => {
                        // The filter's d (`sound_d`), what it would drop.
                        let d = sound[i as usize];
                        if d == u32::MAX {
                            nopath += rows;
                            late += rows;
                            late_cells.insert(cell);
                        } else if f + d > opts.ceiling {
                            late += rows;
                            late_cells.insert(cell);
                        }
                    }
                }
            }
            // The level-0 backward's marks: a marked state is on a winning path
            // by the horizon, so the table calling it too late is a finding.
            if let Some(m) = &marks {
                let shape = ff.shape_hash();
                for (cell, key) in ff.cell_keys() {
                    if m.contains(shape, key, cell) {
                        marked += 1;
                        if late_cells.contains(&cell) {
                            marked_late += 1;
                            if samples.len() < 24 {
                                let (x, y) = xy_of(cell);
                                samples.insert(format!("f{f:03}: a MARKED state of shape {shape:#x} at ({x}, {y}) is too late"));
                            }
                        }
                    }
                }
            }
        }
        let pc = |x: u64| 100.0 * x as f64 / states.max(1) as f64;
        say!(rep, "f{f:03} | {states} | {late} ({:.1}%) | {nopath} | {band} ({:.1}%) | {dead} | {left} | {other} | {missing} | {marked} | {marked_late}", pc(late), pc(band))?;
    }
    for x in &samples {
        say!(rep, "  {x}")?;
    }
    say!(rep, "\ntotal {:.1} s", t_all.elapsed().as_secs_f64())?;
    Ok(rep)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn num(lo: i32, hi: i32) -> Val {
        Val::Num(Pico8NumInterval::new(P8::from_raw(lo), P8::from_raw(hi)))
    }

    /// The balloon's full-period premise `Hi(v) - Lo(v) >= 1` (the width of its
    /// phase, `widen::canon_balloon_offset`), with `v` a select on a condition
    /// the evaluator leaves undecided between the phase `[0, 1)` and the phase
    /// advanced by 0.01. Over the joined select the two ends are independent
    /// and the premise is undecided - every node of rooms (6,1) and (7,1) in
    /// violation; lifted (`lift`, `Lift::Ends`) each arm is a literal shifted
    /// by a point (`lit_shift`), and the premise holds.
    #[test]
    fn a_select_of_literal_phases_keeps_its_full_period_width() {
        let period = 0xffff;
        let build = |what: Option<Lift>| -> (Graph, NodeId) {
            let mut g = Graph::default();
            let mut memo = HashMap::new();
            let mut mk = |g: &mut Graph, op: Op, args: Vec<NodeId>| match what {
                Some(w) => lift(g, op, args, w, &mut memo),
                None => g.fold(op, args),
            };
            let shown = g.leaf(Op::Cell(0));
            let zero = g.leaf(Op::Const(0, 0));
            let c = mk(&mut g, Op::Gt, vec![shown, zero]);
            let phase = g.leaf(Op::Const(0, period));
            let step = g.leaf(Op::Const(655, 655));
            let advanced = mk(&mut g, Op::Add, vec![phase, step]);
            let v = mk(&mut g, Op::Sel, vec![c, advanced, phase]);
            let hi = mk(&mut g, Op::Hi, vec![v]);
            let lo = mk(&mut g, Op::Lo, vec![v]);
            let width = mk(&mut g, Op::Sub, vec![hi, lo]);
            let p = g.leaf(Op::Const(period, period));
            let full = mk(&mut g, Op::Ge, vec![width, p]);
            (g, full)
        };
        let cells: HashMap<u32, Val> = [(0, num(-0x10000, 0x10000))].into_iter().collect();
        let (g, full) = build(None);
        assert_eq!(g.eval_narrow_top(&cells).unwrap()[full as usize], Val::Bool(None), "joined, the width is undecided");
        for w in [Lift::Ends, Lift::All] {
            let (g, full) = build(Some(w));
            assert_eq!(g.eval_narrow_top(&cells).unwrap()[full as usize], Val::Bool(Some(true)), "lifted, each arm is a full period");
        }
    }

    /// A tile loop's "another iteration" test `start + 2 <= end` over the
    /// player's `x` after a `move` the evaluator cannot decide was taken (x 44
    /// or 52, room (7,1) at (52, 35)): both ends of the loop read the same `x`,
    /// so per lane the loop is two tiles, but over the joined `x` it may be
    /// three - the unrolled loop's "finished" obligation undecided. Lifting
    /// every value op (`Lift::All`, the precise frame) decides it; `Lift::Ends`
    /// does not touch it.
    #[test]
    fn lifting_keeps_a_tile_loop_over_one_x_two_tiles_wide() {
        let build = |what: Lift| -> (Graph, NodeId) {
            let mut g = Graph::default();
            let mut memo = HashMap::new();
            let mut mk = |g: &mut Graph, op: Op, args: Vec<NodeId>| lift(g, op, args, what, &mut memo);
            let moved = g.leaf(Op::Cell(0));
            let zero = g.leaf(Op::Const(0, 0));
            let c = mk(&mut g, Op::Gt, vec![moved, zero]);
            let (a, b) = (g.leaf(Op::Const(44 << 16, 44 << 16)), g.leaf(Op::Const(52 << 16, 52 << 16)));
            let x = mk(&mut g, Op::Sel, vec![c, a, b]);
            let k = |g: &mut Graph, v: i32| g.leaf(Op::Const(v << 16, v << 16));
            let (one, five, eight, two) = (k(&mut g, 1), k(&mut g, 5), k(&mut g, 8), k(&mut g, 2));
            let x1 = mk(&mut g, Op::Add, vec![x, one]);
            let sd = mk(&mut g, Op::Div, vec![x1, eight]);
            let start = mk(&mut g, Op::Flr, vec![sd]);
            let xe = mk(&mut g, Op::Add, vec![x1, five]);
            let ed = mk(&mut g, Op::Div, vec![xe, eight]);
            let end = mk(&mut g, Op::Flr, vec![ed]);
            let third = mk(&mut g, Op::Add, vec![start, two]);
            let more = mk(&mut g, Op::Le, vec![third, end]);
            (g, more)
        };
        let cells: HashMap<u32, Val> = [(0, num(-0x10000, 0x10000))].into_iter().collect();
        let (g, more) = build(Lift::Ends);
        assert_eq!(g.eval_narrow_top(&cells).unwrap()[more as usize], Val::Bool(None));
        let (g, more) = build(Lift::All);
        assert_eq!(g.eval_narrow_top(&cells).unwrap()[more as usize], Val::Bool(Some(false)), "per lane, no third tile");
    }
}

#[cfg(test)]
mod room_tests {
    /// Room (7,1)'s table builds and is sound by its own checks (rooms (6,1)
    /// and (7,1) failed at the spawn's first frame on the balloon's phase,
    /// then on a tile loop over a joined `x`, 2026-10-01): the start state is
    /// at least 45 frames from the exit (the known optimum is 86).
    #[test]
    #[ignore = "~30 s with 8 threads: a lattice walk and five passes"]
    fn room_71_table_builds_with_its_balloon() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        std::env::set_var("CELESTE_START_ROOM", "7,1");
        let t = std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(|| super::cost_to_go(std::path::Path::new("."), 5, 8))
            .unwrap()
            .join()
            .unwrap()
            .unwrap_or_else(|e| panic!("{e:#}"));
        assert_eq!(t.start_d, 45);
    }
}
