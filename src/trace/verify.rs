//! Tracing ONE frame as a kernel, and checking it against the oracle.
//!
//! A trace is a FUNCTION: input cells in, output cells out, plus per outcome
//! `guard` (when it applies) and `error` (where its row is undefined,
//! derived from its values).
//!
//! The check runs the frame twice from the same state: symbolically (fields
//! as `Op::Cell` leaves, buttons free) into a graph, and with real numbers
//! and buttons. Both are the SAME interpreter over the SAME domain (the
//! concrete run is the symbolic domain with every leaf a constant), so a
//! disagreement can only be the compilation: a bad merge, a wrong guard, a
//! select on the wrong condition.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;

use crate::transpile::graph::NodeId;
#[cfg(test)]
use crate::transpile::graph::Graph;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Iface, Path};
#[cfg(test)]
use super::iface::Step;
use super::interp::{Flow, Interp};
use super::state::State;

/// One traced outcome of a frame.
pub struct FrameOut {
    pub guard: NodeId,
    /// Where this outcome's row has no defined value (`trace::error`): the
    /// lanes live here on which it holds decline. Derived, never carried.
    pub error: NodeId,
    /// Every scalar reachable from the globals table, by path, with the
    /// KIND the tracer knows it to be (not re-derivable from the op: an
    /// `Op::Cell` or `Op::Sel` can be either).
    pub fields: Vec<(Path, NodeId, &'static str)>,
    /// Slots that are DEAD at the frame boundary: the six button cells
    /// (`btn(i)` stores its resolved choice back), and the shared output
    /// unknown.
    ///
    /// The production chunk ends with `__reset_button_states()`; the tracer
    /// runs the reset at the START, so recording these as dead makes the two
    /// boundaries agree and lets converged lanes dedup. Sound because the
    /// cells are dead until the next reset overwrites them - CLAIMED, not
    /// proven; the differential check would catch a `btn` read outliving it.
    pub ubool: Vec<Path>,
    /// What kept this outcome from merging with its siblings: only a shape
    /// difference can.
    pub shape: super::heap::Shape,
    /// The ENGINE's structure for the state this outcome ends in, and
    /// the canonical cell each of `fields` / `ubool` lands on in it.
    /// Per outcome (an allocation or free shifts cells): NOT comparable with
    /// `Frame::in_cells` unless the outcome kept the input shape.
    pub rt2: celeste_engine::runtime2::Rt2,
    pub cells: Vec<u32>,
    pub ubool_cells: Vec<u32>,
    /// The state itself, so a shape walk can step FORWARD from it.
    pub st: State<Symbolic>,
    /// THE TRANSFER ROOTS (`search::arc_edges`, a level-0 trace;
    /// empty otherwise): per axis x then y, `took`, `pre`, `frag`, `ox` (the
    /// player's split, `State::arc`) and `fin` (the player's remainder
    /// before the boundary widening; a placeholder 0 without a player at the
    /// end, `arc_fin`). Computed beside the row, never stored in it.
    pub arc: Vec<NodeId>,
    /// Does this outcome have a player at its end (`fin` is real)?
    pub arc_fin: bool,
}

/// Roots per axis in `FrameOut::arc`: took, pre, frag, ox, fin.
pub const ARC_AXIS_ROOTS: usize = 5;

impl Clone for FrameOut {
    /// For splitting an outcome (`split_undecided_selects`): the block is
    /// cloned through `Rt2::clone_block`, which shares the cart.
    fn clone(&self) -> Self {
        FrameOut {
            guard: self.guard,
            error: self.error,
            fields: self.fields.clone(),
            ubool: self.ubool.clone(),
            shape: self.shape.clone(),
            rt2: self.rt2.clone_block(),
            cells: self.cells.clone(),
            ubool_cells: self.ubool_cells.clone(),
            st: self.st.clone(),
            arc: self.arc.clone(),
            arc_fin: self.arc_fin,
        }
    }
}

pub struct Frame {
    pub iface: Iface,
    /// Traced with held buttons unknown (`Symbolic::held_unknown`): every
    /// outcome writes the player's `p_jump` / `p_dash` unknown (`emit::bind`).
    pub held_unknown: bool,
    /// Traced with the fly fruit unknown (`Symbolic::fruit_unknown`): every
    /// outcome writes its `fly` unknown (`emit::bind`).
    pub fruit_unknown: bool,
    /// What each fork minted as both values was minted for
    /// (`Symbolic::fork_origins`), for the kernel dump.
    pub fork_origins: Vec<(u8, String)>,
    /// How many FORK choices this frame made; the emitter enumerates them.
    pub forks: u8,
    /// The forks' arities AS TRACED. The arena outlives the frame and a
    /// later frame overwrites them, so `emit::bind` installs these.
    pub fork_ways: Vec<u8>,
    pub outs: Vec<FrameOut>,
    /// THE RAISE ROW's liveness: when this frame would have hit a Lua raise,
    /// as the OR of the path guards at every `Interp::poison` site.
    /// `ConstBool(false)` where nothing raises (all but the rooms where a
    /// spring stands on a breakable floor).
    ///
    /// NOT an element of `outs`: a raise has no field values, and every
    /// `outs` consumer indexes positionally assuming them.
    pub raise: NodeId,
    /// The canonical cell each `Iface` slot names in the state the frame
    /// STARTS in - the engine's numbering for `Op::Cell(i)`.
    pub in_cells: Vec<u32>,
    /// The engine's structure for that state: a kernel runs against a block
    /// of this shape (`celeste_engine::slots::reshape`).
    pub in_rt2: celeste_engine::runtime2::Rt2,
}

/// Run one chunk and require it to end normally in exactly one state.
/// The oracle side has to: with no unknowns there is nothing to branch on.
pub fn run_one<'a, D: Domain>(
    it: &mut Interp<'a, D>,
    ast: &'a ast::Ast,
    st: State<D>,
) -> Result<State<D>> {
    let out = it.exec_block(ast.nodes(), st)?;
    if out.len() != 1 {
        bail!("expected one state, got {}", out.len());
    }
    let (s, f) = out.into_iter().next().unwrap();
    if let Flow::Break = f {
        bail!("break at chunk toplevel");
    }
    Ok(s)
}

/// Every scalar the frame ends with, as graph nodes, split into the ones that carry
/// data and the ones that are dead at the boundary (`FrameOut::ubool`).
fn out_fields(
    st: &State<Symbolic>,
    d: &Symbolic,
) -> Result<(Vec<(Path, NodeId, &'static str)>, Vec<Path>)> {
    let mut out = Vec::new();
    let mut ubool = Vec::new();
    // A near level's floor `state`s are an interval column in every outcome,
    // an exact `[n, n]` included: the column's type is per shape.
    let ival_always: Vec<Path> = if d.floors_near { super::widen::near_floor_paths(st).state } else { Vec::new() };
    for p in iface::scalars(st, &[])? {
        if p.first() == Some(&iface::key("__button_states")) {
            ubool.push(p);
            continue;
        }
        // The canonical output unknown (`Symbolic::unknown_bool_output`: held
        // trails, the fly fruit's `fly`, the fall floors' `collideable`): the
        // same uniform `AV::UBool` whatever the body, so no root.
        if let Some(Value::Bool(b)) = iface::get(st, &p) {
            if d.is_unknown_output(b) {
                ubool.push(p);
                continue;
            }
        }
        // The engine TYPE of the column, asked of the graph (an interval is
        // a `Value::Num` too): writing an interval as a number would be an
        // unchecked narrowing.
        let (n, ty) = match iface::get(st, &p).unwrap() {
            Value::Num(n) => {
                let ty = if d.is_interval(&n) || ival_always.contains(&p) { "ZI" } else { "ZN" };
                (n, ty)
            }
            Value::Bool(n) => (n, "ZB"),
            _ => unreachable!("scalars only yields scalars"),
        };
        out.push((p, n, ty));
    }
    Ok((out, ubool))
}

/// Symbolize `root`, free the buttons, and trace one frame.
pub fn trace_frame<'a>(
    it: &mut Interp<'a, Symbolic>,
    reset: &'a ast::Ast,
    frame: &'a ast::Ast,
    st: State<Symbolic>,
    roots: &[Path],
    pin: &[(Path, Conc)],
    // Slots the boundary WIDENS to an interval (the player's `rem.x` /
    // `rem.y`): a frame that reads one forks at `__split_by_flr`.
    ival: &[Path],
    // Apply the boundary's widenings INSIDE the frame (`trace::widen`), so
    // a row is hashed on the value it stores, and capture the remainder
    // transfers (`search::arc_edges`). Off for the differential check
    // against the concrete oracle, which has no intervals.
    widen: bool,
    // Input slots known to lie in a RANGE (raw 16.16, inclusive): the body
    // is specialized on them (`Symbolic::ranges`); outside them the frame is
    // in error, like a pin.
    bounds: &[(Path, (i32, i32))],
) -> Result<Frame> {
    it.trace_start_nodes = it.d.node_count();
    let mut st = st;
    // Fork choices are per FRAME; the six buttons are among them.
    it.d.forks = 0;
    it.d.graph.reset_forks();
    it.d.clear_ranges();
    it.d.clear_fork_memo();
    it.d.unknown_atoms = 0;
    // The arc capture (`search::arc_edges`): every widening trace.
    it.arc_capture = widen;
    // What a previous trace left if it failed part-way.
    it.raised.clear();
    it.d.escaped.clear();
    it.d.fork_origins.clear();
    it.d.evaluated.clear();
    let iface = iface::symbolize(&mut it.d, &mut st, roots, pin, ival)?;
    // THE KERNEL'S ADMISSIBLE INPUTS: its pins and its region's ranges, on
    // the input cells (built before the frame runs). A lane outside them is
    // an error of the whole frame.
    let mut admissible = iface::pin_guard(&mut it.d, &iface);
    // The player's input position and its region, for the points
    // (`Points`): the bounds on its `x` and `y`.
    let player = super::shapes::player_path(&st);
    let mut position: [Option<(NodeId, (i32, i32))>; 2] = [None, None];
    for (p, (lo, hi)) in bounds {
        let i = iface.slots.iter().position(|q| q == p).ok_or_else(|| anyhow!("bounded {} is not an input slot", iface::show(p)))?;
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(i as u32));
        it.d.ranges.insert(cell, (*lo as i64, *hi as i64));
        if let Some(pl) = &player {
            for (k, f) in ["x", "y"].iter().enumerate() {
                if p.len() == pl.len() + 1 && p.starts_with(pl) && p[pl.len()] == iface::key(f) {
                    position[k] = Some((cell, (*lo, *hi)));
                }
            }
        }
        // The obligation (both ends in the range), built on the graph
        // directly so the range analysis it seeds cannot fold it away.
        use crate::transpile::graph::Op;
        let (klo, khi) = (it.d.graph.leaf(Op::Const(*lo, *lo)), it.d.graph.leaf(Op::Const(*hi, *hi)));
        let (vlo, vhi) = (it.d.graph.fold(Op::Lo, vec![cell]), it.d.graph.fold(Op::Hi, vec![cell]));
        let a = it.d.graph.fold(Op::Ge, vec![vlo, klo]);
        let b = it.d.graph.fold(Op::Le, vec![vhi, khi]);
        let both = it.d.graph.fold(Op::And, vec![a, b]);
        admissible = it.d.graph.fold(Op::And, vec![admissible, both]);
    }
    // The engine's numbering for the INPUT shape: the last moment the input
    // state exists (`symbolize` changed values, not the shape).
    let (cart, cache) = match (it.cart.clone(), it.cache.clone()) {
        (Some(a), Some(b)) => (a, b),
        _ => bail!("tracing a frame needs the cart and the room's collision cache"),
    };
    let in_rt2 = super::bind::structure_of(&st, cart.clone(), cache.clone())?;
    let in_cells = super::bind::bind_inputs(&in_rt2, &iface)?;
    let mut st = st;
    // Held buttons unknown: both trails run as a fork of both values, after
    // the input shape is taken, before anything reads them.
    if it.d.held_unknown {
        super::widen::fork_held_inputs(&mut st, &mut it.d)?;
    }
    // The fly fruit unknown: its widened fields replaced before anything reads
    // them.
    if it.d.fruit_unknown {
        super::widen::fork_fruit_inputs(&mut st, &mut it.d)?;
    }
    // A near level's floors: each `collideable` derived from `state`, except
    // mid split frame, where the row stores the first step's `collideable`:
    // read as stored, an unknown one forked.
    if it.d.floors_near {
        if super::widen::mid_frame(&st) {
            super::widen::fork_unknown_near_collideables(&mut st, &mut it.d)?;
        } else {
            super::widen::fork_near_floor_inputs(&mut st, &mut it.d)?;
        }
    }
    // The countdowns of a near level: the unknown number, as stored. After
    // the near floors' inputs, which materialize the absent fields.
    super::widen::forget_countdown_inputs(&mut st, &mut it.d)?;
    // The moving platforms unknown: their input cells, decided per world by
    // the split pass (`widen::platform_inputs`, `Points`).
    it.d.platform_cells.clear();
    if it.d.platforms_unknown {
        for ob in super::widen::platform_inputs(&mut st, &mut it.d)? {
            admissible = it.d.graph.fold(crate::transpile::graph::Op::And, vec![admissible, ob]);
        }
    }
    // A representative state carries an earlier trace's capture: per FRAME.
    st.arc = if it.arc_capture {
        let (x, y) = (super::state::ArcAxis::none(&mut it.d), super::state::ArcAxis::none(&mut it.d));
        Some(Box::new([x, y]))
    } else {
        None
    };
    let st = run_one(it, reset, st)?;
    let finished = it.exec_block(frame.nodes(), st)?;
    // The error of the whole frame, OR-ed into every outcome's: inputs this
    // kernel was not built for.
    let global = {
        use super::domain::Domain;
        it.d.not(&admissible)
    };
    let mut outs = Vec::new();
    for (s, f) in finished {
        if let Flow::Break = f {
            bail!("break at frame toplevel");
        }
        let mut s = s;
        s.gc();
        // The absent-as-zero fields (`widen::ABSENT_AS_ZERO`), at every
        // level: part of the shape, not of the precision.
        super::widen::materialize_absent_fields(&mut s, &mut it.d)?;
        // The transfer roots, the player's remainder read BEFORE the
        // widenings replace it (`FrameOut::arc`).
        let (arc, arc_fin) = match s.arc.as_deref() {
            None => (Vec::new(), false),
            Some(axes) => {
                let players = super::widen::objects_of_type(&s, "player");
                anyhow::ensure!(players.len() <= 1, "arc capture: {} player objects at the frame's end", players.len());
                let mut roots = Vec::with_capacity(2 * ARC_AXIS_ROOTS);
                for (k, a) in axes.iter().enumerate() {
                    let fin = match players.first() {
                        Some(pl) => match iface::get(&s, &super::widen::field(pl, &["rem", ["x", "y"][k]])) {
                            Some(Value::Num(n)) => n,
                            other => bail!("arc capture: the player's rem.{} is {other:?}", ["x", "y"][k]),
                        },
                        None => it.d.graph.leaf(crate::transpile::graph::Op::Const(0, 0)),
                    };
                    roots.extend([a.took, a.pre, a.frag, a.ox, fin]);
                }
                (roots, !players.is_empty())
            }
        };
        // THE WIDENINGS, in the graph so a row is hashed on the value it
        // stores. Before `out_fields` (reads the state), after `gc` (walks
        // the live objects).
        let owed = if widen { super::widen::widen(&mut s, &mut it.d)? } else { Vec::new() };
        let (fields, ubool) = out_fields(&s, &it.d)?;
        // What this outcome owes beyond its operators: the frame's, where its
        // path's model ended (`State::ended`), and the widenings' of the
        // slots it STORES.
        let error = owed
            .iter()
            .filter(|(p, _)| fields.iter().any(|(q, _, _)| q == p))
            .fold(it.d.graph.fold(crate::transpile::graph::Op::Or, vec![global, s.ended]), |e, (_, w)| it.d.graph.fold(crate::transpile::graph::Op::Or, vec![e, *w]));
        // The engine's numbering for THIS outcome's shape. Fields and dead
        // cells share one cell space, so they are resolved together.
        let rt2 = super::bind::structure_of(&s, cart.clone(), cache.clone())?;
        let paths: Vec<Path> =
            fields.iter().map(|(p, _, _)| p.clone()).chain(ubool.iter().cloned()).collect();
        let all = super::bind::resolve_all(&rt2, &paths)?;
        let (cells, ubool_cells) = all.split_at(fields.len());
        let (guard, shape) = (s.guard.clone(), s.shape()?);
        outs.push(FrameOut {
            guard,
            error,
            fields,
            ubool,
            shape,
            rt2,
            cells: cells.to_vec(),
            ubool_cells: ubool_cells.to_vec(),
            st: s,
            arc,
            arc_fin,
        });
    }
    // The selects left on a condition a lane can hold undecided are split,
    // until none is (`split_undecided_selects`).
    if !it.d.no_known_forks {
        let points = if it.d.platforms_unknown { Some(Points::new(&it.d, position)?) } else { None };
        let room = crate::transpile::graph::Room { cart: cart.clone(), cache: cache.clone() };
        outs = split_undecided_selects(&mut it.d, outs, points, Some(&room))?;
        // The conditions an operator was EVALUATED under are path guards
        // read by the error's derivation; an undecided select left in one
        // would read a garbage bit. Read three-valued like the guards: an
        // unknown condition makes `own and at` unknown, which reads as error.
        three_valued_evaluated(&mut it.d, &outs);
    }
    // ERROR, DERIVED once the graph is final: from the fields, the guard
    // and the conditions already owed (`trace::error`).
    let roots: Vec<Vec<NodeId>> = outs
        .iter()
        .map(|o| o.fields.iter().map(|(_, n, _)| *n).chain([o.guard, o.error]).collect())
        .collect();
    for (o, e) in outs.iter_mut().zip(super::error::for_outcomes(&mut it.d, &roots)) {
        o.error = it.d.graph.fold(crate::transpile::graph::Op::Or, vec![o.error, e]);
    }
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build] trace_frame (widen {widen}): {} forks at the end, {} outcomes, {} pins", it.d.forks, outs.len(), pin.len());
    }
    // DIAGNOSTIC: forks minted by origin, and how many an outcome still sees.
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        let mut by: std::collections::BTreeMap<&str, usize> = Default::default();
        for (_, o) in &it.d.fork_origins {
            *by.entry(o.as_str()).or_default() += 1;
        }
        use crate::transpile::graph::Op;
        let mut roots: Vec<NodeId> = Vec::new();
        for o in &outs {
            roots.extend(o.fields.iter().map(|(_, n, _)| *n));
            roots.push(o.guard);
            roots.push(o.error);
        }
        let reach = crate::transpile::bdd::reachable(&it.d.graph, &roots);
        let mut live: std::collections::BTreeSet<u8> = Default::default();
        for (i, r) in reach.iter().enumerate() {
            if !*r {
                continue;
            }
            if let Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) = it.d.graph.get(i as NodeId).op {
                live.insert(d);
            }
        }
        eprintln!(
            "[build] trace_frame: {} forks, {} with an origin: {by:?}; live {}",
            it.d.forks,
            it.d.fork_origins.len(),
            live.len()
        );
    }
    let fork_ways: Vec<u8> = (0..it.d.forks).map(|d| it.d.graph.fork_ways(d)).collect();
    // The raise row's liveness: the OR of the guards at the raises hit.
    let raise = {
        use super::domain::Domain;
        let raised = std::mem::take(&mut it.raised);
        let mut r = it.d.boolean(false);
        for (g, _) in &raised {
            r = it.d.or(&r, g);
        }
        r
    };
    Ok(Frame { iface, held_unknown: it.d.held_unknown, fruit_unknown: it.d.fruit_unknown, fork_origins: it.d.fork_origins.clone(), forks: it.d.forks, fork_ways, outs, raise, in_cells, in_rt2 })
}

/// An outcome's row as `Waiting` indexes it: its shape class and its fields.
type RowKey = (usize, Vec<NodeId>);

/// `split_undecided_selects`' queue: the outcomes still to split, popped
/// largest cone first, deduped as they arrive (a side equal to a waiting
/// outcome merges into it: `Lite::merge`, `PointSet::union`).
#[derive(Default)]
struct Waiting {
    slots: Vec<Option<(Lite, Option<PointSet>)>>,
    index: rustc_hash::FxHashMap<RowKey, usize>,
    heap: std::collections::BinaryHeap<(usize, std::cmp::Reverse<usize>)>,
}

/// One outcome of `split_undecided_selects` in flight: what a split changes,
/// over the outcome it came from (`t`, whose shape, state and structure it
/// keeps), so no state or block is cloned per side.
#[derive(Clone)]
struct Lite {
    t: usize,
    guard: NodeId,
    error: NodeId,
    fields: Vec<NodeId>,
    /// Conditions every lane DECIDES that a case split of this outcome may
    /// merge into outcomes already made (`absorbed`): what a split's atom
    /// guarded in the select's condition.
    rests: Vec<NodeId>,
}

impl Lite {
    /// What the row stores: the fields.
    fn row(&self) -> Vec<NodeId> {
        self.fields.clone()
    }

    fn every(&self) -> Vec<NodeId> {
        let mut r = self.row();
        r.extend([self.error, self.guard]);
        r
    }

    /// Absorb another with the same row: live wherever either is; error
    /// `(g1 and e1) or (g2 and e2)` (each counts on its own lanes), or the
    /// one node where both are the same.
    fn merge(&mut self, d: &mut Symbolic, guard: NodeId, error: NodeId, rests: &[NodeId]) {
        for r in rests {
            if !self.rests.contains(r) && self.rests.len() < MAX_RESTS {
                self.rests.push(*r);
            }
        }
        if self.error != error {
            let (a, b) = (d.and(&self.guard, &self.error), d.and(&guard, &error));
            self.error = d.or(&a, &b);
        }
        self.guard = d.or(&self.guard, &guard);
    }
}

impl Waiting {
    /// Outcomes of one shape with the same row are one: the template's
    /// SHAPE class (`class`) rather than the template itself.
    fn key(class: &[usize], o: &Lite) -> RowKey {
        (class[o.t], o.fields.clone())
    }

    fn push(&mut self, d: &mut Symbolic, class: &[usize], o: Lite, pts: Option<PointSet>, size: usize) {
        let key = Self::key(class, &o);
        if let Some(&i) = self.index.get(&key) {
            let (p, pp) = self.slots[i].as_mut().expect("an indexed outcome is waiting");
            p.merge(d, o.guard, o.error, &o.rests);
            if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                a.union(b);
            }
            return;
        }
        let i = self.slots.len();
        self.slots.push(Some((o, pts)));
        self.index.insert(key, i);
        self.heap.push((size, std::cmp::Reverse(i)));
    }

    fn pop(&mut self, class: &[usize]) -> Option<(Lite, Option<PointSet>)> {
        let (_, std::cmp::Reverse(i)) = self.heap.pop()?;
        let out = self.slots[i].take().expect("a queued outcome is waiting");
        self.index.remove(&Self::key(class, &out.0));
        Some(out)
    }

    fn len(&self) -> usize {
        self.index.len()
    }
}

/// An outcome's error once its row is resolved: WHERE A LANE MAY ERR, with no
/// select on an undecided condition left (`MayErr`). By case analysis on the
/// error alone, not the outcome (split sides with one row merge back with
/// the guards copied in, doubling each round); and not three-valued (a hull
/// loses what a select's condition says about its value, e.g. the platform
/// wrap).
fn settle_error(d: &mut Symbolic, points: Option<&mut Points>, pts: Option<&PointSet>, room: Option<&crate::transpile::graph::Room>, mut o: Lite) -> Lite {
    let mut m = MayErr { points, room, memo: Default::default(), cases: 0 };
    let here = pts.cloned();
    o.error = m.may(d, o.error, here.as_ref()).0;
    o
}

/// `MayErr::may`'s bound on the cases it opens under one error.
const MAY_CASES: usize = 4096;

/// A boolean's `(may be true, may be false)` over the states a lane stands
/// for, with no undecided select left:
/// - `Or` / `And` / `Not` through the operands (`and` over-approximates,
///   the safe side for an error);
/// - a node reading an undecided select is split on its lowest undecidable
///   comparison, each case under that answer's `may` guard
///   (`Symbolic::may_answers`) and narrowed to the points that give it;
/// - a node reading none, as it is.
///
/// Past `MAY_CASES` cases, the three-valued reading (`three_valued`).
struct MayErr<'a, 'r> {
    points: Option<&'a mut Points>,
    room: Option<&'r crate::transpile::graph::Room>,
    memo: rustc_hash::FxHashMap<(NodeId, Option<Vec<u64>>), (NodeId, NodeId)>,
    cases: usize,
}

impl MayErr<'_, '_> {
    fn may(&mut self, d: &mut Symbolic, n: NodeId, pts: Option<&PointSet>) -> (NodeId, NodeId) {
        use crate::transpile::graph::Op;
        let key = (n, pts.map(|p| p.0.clone()));
        if let Some(hit) = self.memo.get(&key) {
            return *hit;
        }
        let (op, args) = {
            let node = d.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        let reads_undecided = |d: &mut Symbolic, n: NodeId| cone(&d.graph, &[n]).into_iter().find(|m| d.graph.get(*m).op == Op::Sel && d.lane_undecidable(d.graph.get(*m).args[0]));
        let out = match op {
            Op::Or | Op::And => {
                let parts: Vec<(NodeId, NodeId)> = args.iter().map(|a| self.may(d, *a, pts)).collect();
                let (t, f): (Vec<NodeId>, Vec<NodeId>) = parts.into_iter().unzip();
                if op == Op::Or {
                    (d.graph.fold(Op::Or, t), d.graph.fold(Op::And, f))
                } else {
                    (d.graph.fold(Op::And, t), d.graph.fold(Op::Or, f))
                }
            }
            Op::Not => {
                let (t, f) = self.may(d, args[0], pts);
                (f, t)
            }
            _ => match reads_undecided(d, n) {
                None => {
                    let nn = d.graph.fold(Op::Not, vec![n]);
                    (n, nn)
                }
                Some(sel) if self.cases < MAY_CASES => {
                    self.cases += 1;
                    let c = d.graph.get(sel).args[0];
                    let atom = cone(&d.graph, &[c])
                        .into_iter()
                        .find(|m| matches!(d.graph.get(*m).op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::UnknownBool(_)) && d.lane_undecidable(*m))
                        .unwrap_or(c);
                    let sides: Vec<(bool, Option<PointSet>)> = match (self.points.as_deref_mut(), pts) {
                        (Some(p), Some(here)) => {
                            let (yes, no) = p.answers(d, self.room, atom);
                            [(true, yes.and(here)), (false, no.and(here))].into_iter().filter(|(_, s)| !s.is_empty()).map(|(a, s)| (a, Some(s))).collect()
                        }
                        _ => vec![(true, None), (false, None)],
                    };
                    let decided = sides.len() == 1;
                    let (may_true, may_false) = d.may_answers(atom);
                    let reach = cone(&d.graph, &[n]);
                    let (mut t, mut f) = (d.graph.leaf(Op::ConstBool(false)), d.graph.leaf(Op::ConstBool(false)));
                    for (answer, side) in sides {
                        let to = d.graph.leaf(Op::ConstBool(answer));
                        let map = rebuild_all(d, &reach, &rustc_hash::FxHashMap::from_iter([(atom, to)]));
                        let m = map.get(&n).copied().unwrap_or(n);
                        let (ct, cf) = self.may(d, m, side.as_ref());
                        let (ct, cf) = if decided {
                            (ct, cf)
                        } else {
                            let g = if answer { may_true } else { may_false };
                            (d.graph.fold(Op::And, vec![g, ct]), d.graph.fold(Op::And, vec![g, cf]))
                        };
                        t = d.graph.fold(Op::Or, vec![t, ct]);
                        f = d.graph.fold(Op::Or, vec![f, cf]);
                    }
                    (t, f)
                }
                Some(_) => {
                    let e = three_valued(d, n);
                    let ne = d.graph.fold(Op::Not, vec![e]);
                    (e, ne)
                }
            },
        };
        self.memo.insert(key, out);
        out
    }
}

/// `Symbolic::evaluated`'s conditions, for every node the outcomes reach
/// (through the conditions themselves too), rewritten by `three_valued`.
fn three_valued_evaluated(d: &mut Symbolic, outs: &[FrameOut]) {
    let mut roots: Vec<NodeId> = Vec::new();
    for o in outs {
        roots.extend(o.fields.iter().map(|f| f.1));
        roots.extend([o.guard, o.error]);
    }
    let mut done: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut todo = cone(&d.graph, &roots);
    while !todo.is_empty() {
        let mut next: Vec<NodeId> = Vec::new();
        for n in todo {
            if !done.insert(n) {
                continue;
            }
            if let Some(at) = d.evaluated.get(&n).copied() {
                let at2 = three_valued(d, at);
                if at2 != at {
                    d.evaluated.insert(n, at2);
                }
                next.push(at2);
            }
        }
        todo = cone(&d.graph, &next).into_iter().filter(|n| !done.contains(n)).collect();
    }
}

/// A set of `Points`' points, one bit each.
#[derive(Clone, PartialEq, Eq, Debug)]
struct PointSet(Vec<u64>);

impl PointSet {
    fn empty(n: usize) -> Self {
        PointSet(vec![0; n.div_ceil(64)])
    }
    fn full(n: usize) -> Self {
        let mut s = Self::empty(n);
        for i in 0..n {
            s.insert(i);
        }
        s
    }
    fn insert(&mut self, i: usize) {
        self.0[i / 64] |= 1 << (i % 64);
    }
    fn contains(&self, i: usize) -> bool {
        self.0[i / 64] >> (i % 64) & 1 == 1
    }
    fn union(&mut self, o: &PointSet) {
        for (a, b) in self.0.iter_mut().zip(&o.0) {
            *a |= b;
        }
    }
    fn and(&self, o: &PointSet) -> PointSet {
        PointSet(self.0.iter().zip(&o.0).map(|(a, b)| a & b).collect())
    }
    fn is_empty(&self) -> bool {
        self.0.iter().all(|w| *w == 0)
    }
}

/// THE POINTS a platforms-unknown frame's comparisons are decided over:
/// every PLATFORM WORLD (`concrete::platform_worlds`) at every whole pixel of
/// the player's region. A path carries the points consistent with its
/// answers; a comparison one way at all of them is decided, and a path left
/// with none is dropped, so one path's answers come from ONE arrangement of
/// the platforms. Compile time only: what reaches the kernel is each
/// outcome's pixels (`position_guard`).
///
/// At a point, a comparison is decided by the interval evaluator with the
/// platform cells pinned to the world and the player's to the pixel
/// (`Graph::eval_lenient_in`); a point it leaves undecided goes both ways.
pub struct Points {
    /// Per world, the value of each platform cell: `(cell, raw)`.
    worlds: Vec<Vec<(u32, i32)>>,
    platform: rustc_hash::FxHashSet<u32>,
    /// The player's input `x` and `y`: the cell, its node, the first whole
    /// pixel of the region and how many. `None` without a player.
    pos: [Option<(u32, NodeId, i32, usize)>; 2],
    memo: rustc_hash::FxHashMap<NodeId, (PointSet, PointSet)>,
}

impl Points {
    fn new(d: &Symbolic, pos: [Option<(NodeId, (i32, i32))>; 2]) -> Result<Points> {
        use crate::transpile::graph::Op;
        let table = d.worlds.clone().ok_or_else(|| anyhow!("the platforms are unknown but there is no world table"))?;
        let cell = |n: NodeId| match d.graph.get(n).op {
            Op::Cell(c) => Ok(c),
            ref o => Err(anyhow!("a pinned input is {o:?}, not a cell")),
        };
        let mut worlds = Vec::with_capacity(table.len());
        for w in table.iter() {
            anyhow::ensure!(w.len() == d.platform_cells.len(), "a world has {} platforms, the frame {}", w.len(), d.platform_cells.len());
            let mut pins = Vec::new();
            for (p, x) in w.iter().zip(&d.platform_cells) {
                pins.push((cell(*x)?, p[0]));
            }
            worlds.push(pins);
        }
        let platform = worlds.first().map(|w| w.iter().map(|(c, _)| *c).collect()).unwrap_or_default();
        let mut out = [None, None];
        for (k, p) in pos.iter().enumerate() {
            if let Some((n, (lo, hi))) = p {
                const ONE: i32 = 1 << 16;
                anyhow::ensure!(lo % ONE == 0 && hi % ONE == 0, "the player's position range is not whole pixels");
                let (a, b) = (lo.div_euclid(ONE), hi.div_euclid(ONE));
                out[k] = Some((cell(*n)?, *n, a, (b - a + 1) as usize));
            }
        }
        Ok(Points { worlds, platform, pos: out, memo: Default::default() })
    }

    fn per_world(&self) -> usize {
        self.pos.iter().map(|p| p.map_or(1, |p| p.3)).product()
    }

    fn len(&self) -> usize {
        self.worlds.len() * self.per_world()
    }

    /// The point of world `w` at pixel offsets `(i, j)` into the region.
    fn index(&self, w: usize, i: usize, j: usize) -> usize {
        let ny = self.pos[1].map_or(1, |p| p.3);
        (w * self.per_world()) + i * ny + j
    }

    /// Where `c` can come out true, and where false.
    fn answers(&mut self, d: &Symbolic, room: Option<&crate::transpile::graph::Room>, c: NodeId) -> (PointSet, PointSet) {
        use crate::transpile::graph::{Op, Val};
        use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};
        if let Some(hit) = self.memo.get(&c) {
            return hit.clone();
        }
        // The cone as a graph of its own, so an evaluation costs the cone.
        let nodes = cone(&d.graph, &[c]);
        let mut g = d.graph.like();
        let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = Default::default();
        let mut ranges: std::collections::HashMap<u32, (i32, i32)> = Default::default();
        for &n in &nodes {
            let node = d.graph.get(n);
            let args = node.args.iter().map(|a| map[a]).collect();
            map.insert(n, g.add(node.op.clone(), args));
            if let (Op::Cell(k), Some(r)) = (&node.op, d.ranges.get(&n)) {
                ranges.insert(*k, (r.0 as i32, r.1 as i32));
            }
        }
        let root = map[&c] as usize;
        let base = crate::transpile::ival::seed_cells(&g, &ranges);
        let exact = |v: i32| Val::Num(Pico8NumInterval::new(Pico8Num::from_raw(v), Pico8Num::from_raw(v)));
        let eval = |cells: &std::collections::HashMap<u32, Val>| -> Option<bool> {
            let v = match room {
                Some(r) => g.eval_lenient_in(cells, r),
                None => g.eval_lenient(cells),
            };
            match v.ok()?.get(root) {
                Some(Val::Bool(b)) => *b,
                _ => None,
            }
        };
        let reads_platform = base.keys().any(|k| self.platform.contains(k));
        let reads = |k: usize| self.pos[k].filter(|p| base.contains_key(&p.0));
        let (px, py) = (reads(0), reads(1));
        let n = self.len();
        let (mut yes, mut no) = (PointSet::empty(n), PointSet::empty(n));
        let (nx, ny) = (self.pos[0].map_or(1, |p| p.3), self.pos[1].map_or(1, |p| p.3));
        // Mark worlds `ws` at pixels `is` x `js` with answer `r`.
        let mut mark = |ws: &[usize], is: &[usize], js: &[usize], r: Option<bool>| {
            for &w in ws {
                for &i in is {
                    for &j in js {
                        let at = self.index(w, i, j);
                        if r != Some(false) {
                            yes.insert(at);
                        }
                        if r != Some(true) {
                            no.insert(at);
                        }
                    }
                }
            }
        };
        let all_i: Vec<usize> = (0..nx).collect();
        let all_j: Vec<usize> = (0..ny).collect();
        let all_w: Vec<usize> = (0..self.worlds.len()).collect();
        let groups: Vec<Vec<usize>> = if reads_platform { all_w.iter().map(|w| vec![*w]).collect() } else { vec![all_w.clone()] };
        for ws in &groups {
            let mut cells = base.clone();
            if reads_platform {
                for (k, v) in &self.worlds[ws[0]] {
                    if cells.contains_key(k) {
                        cells.insert(*k, exact(*v));
                    }
                }
            }
            let r = eval(&cells);
            if r.is_some() || (px.is_none() && py.is_none()) {
                mark(ws, &all_i, &all_j, r);
                continue;
            }
            // Per pixel, along the axes the comparison reads.
            let is: Vec<Option<usize>> = if px.is_some() { (0..nx).map(Some).collect() } else { vec![None] };
            let js: Vec<Option<usize>> = if py.is_some() { (0..ny).map(Some).collect() } else { vec![None] };
            for i in &is {
                for j in &js {
                    let mut at = cells.clone();
                    if let (Some(i), Some(p)) = (i, px) {
                        at.insert(p.0, exact((p.2 + *i as i32) << 16));
                    }
                    if let (Some(j), Some(p)) = (j, py) {
                        at.insert(p.0, exact((p.2 + *j as i32) << 16));
                    }
                    let r = eval(&at);
                    let ii = i.map_or(all_i.clone(), |i| vec![i]);
                    let jj = j.map_or(all_j.clone(), |j| vec![j]);
                    mark(ws, &ii, &jj, r);
                }
            }
        }
        self.memo.insert(c, (yes.clone(), no.clone()));
        (yes, no)
    }

    /// The lanes an outcome at `pts` is live on: the player's pixels some
    /// world of `pts` has - `true` when that is every pixel.
    fn position_guard(&self, d: &mut Symbolic, pts: &PointSet) -> NodeId {
        use crate::transpile::graph::Op;
        let (nx, ny) = (self.pos[0].map_or(1, |p| p.3), self.pos[1].map_or(1, |p| p.3));
        let at = |i: usize, j: usize| (0..self.worlds.len()).any(|w| pts.contains(self.index(w, i, j)));
        let yes = d.graph.leaf(Op::ConstBool(true));
        if (0..nx).all(|i| (0..ny).all(|j| at(i, j))) {
            return yes;
        }
        // `lo <= cell <= hi` over whole pixels of one axis.
        let within = |d: &mut Symbolic, axis: usize, lo: usize, hi: usize| -> NodeId {
            let Some((_, n, first, _)) = self.pos[axis] else { return d.graph.leaf(Op::ConstBool(true)) };
            let (a, b) = ((first + lo as i32) << 16, (first + hi as i32) << 16);
            let (ka, kb) = (d.graph.leaf(Op::Const(a, a)), d.graph.leaf(Op::Const(b, b)));
            let ge = d.graph.fold(Op::Ge, vec![n, ka]);
            let le = d.graph.fold(Op::Le, vec![n, kb]);
            d.graph.fold(Op::And, vec![ge, le])
        };
        // Runs of `j` per column, and runs of columns with the same runs.
        let runs = |i: usize| -> Vec<(usize, usize)> {
            let mut out: Vec<(usize, usize)> = Vec::new();
            for j in 0..ny {
                if at(i, j) {
                    match out.last_mut() {
                        Some(r) if r.1 + 1 == j => r.1 = j,
                        _ => out.push((j, j)),
                    }
                }
            }
            out
        };
        let mut guard = d.graph.leaf(Op::ConstBool(false));
        let mut i = 0;
        while i < nx {
            let r = runs(i);
            let mut k = i;
            while k + 1 < nx && runs(k + 1) == r {
                k += 1;
            }
            if !r.is_empty() {
                let mut col = d.graph.leaf(Op::ConstBool(false));
                for (a, b) in &r {
                    let w = within(d, 1, *a, *b);
                    col = d.graph.fold(Op::Or, vec![col, w]);
                }
                let x = within(d, 0, i, k);
                let both = d.graph.fold(Op::And, vec![x, col]);
                guard = d.graph.fold(Op::Or, vec![guard, both]);
            }
            i = k + 1;
        }
        guard
    }
}

/// MAKING THE FRAME EXECUTABLE UNDER ABSTRACT INPUTS. A SELECT whose
/// condition a lane can hold both ways cannot be evaluated: it stands for
/// concrete states answering each way.
///
/// A select the row STORES must be resolved (the row is one value), so it is
/// split on; the ERROR is then settled (`settle_error`), and the GUARD,
/// three-valued already (`asm_kernel::read_zb_may`), rewritten
/// (`three_valued`), neither split.
///
/// One split at a time: the FIRST such select in program order, the outcome
/// in two (condition true / false, readers refolded with the static ranges,
/// each guard narrowed by `Symbolic::may_answers`), until no stored value
/// has one; equal outcomes fused. At a platforms-unknown level each path
/// carries its POINTS (`Points`): a comparison with one answer at them is
/// decided without a split, and each outcome ends live only on its pixels.
fn split_undecided_selects(d: &mut Symbolic, outs: Vec<FrameOut>, mut points: Option<Points>, room: Option<&crate::transpile::graph::Room>) -> Result<Vec<FrameOut>> {
    use crate::transpile::graph::Op;
    const MAX_OUTCOMES: usize = 4096;
    let n_in = outs.len();
    // Templates of one shape share a class: their outcomes dedupe together.
    let class: Vec<usize> = (0..outs.len()).map(|i| (0..=i).find(|&j| outs[j].shape == outs[i].shape).expect("itself")).collect();
    // The outcomes waiting, LARGEST CONE FIRST. A split only shrinks a cone,
    // so every path into a state has arrived and merged before it is split
    // (else it is split again per path).
    let mut work = Waiting::default();
    let everywhere = points.as_ref().map(|p| PointSet::full(p.len()));
    for (t, o) in outs.iter().enumerate() {
        let lite = Lite {
            t,
            guard: o.guard,
            error: o.error,
            // The transfer roots (`FrameOut::arc`) ride with the fields: same
            // substitutions, and they are part of what makes two outcomes one.
            fields: o.fields.iter().map(|f| f.1).chain(o.arc.iter().copied()).collect(),
            rests: Vec::new(),
        };
        let size = cone(&d.graph, &lite.row()).len();
        work.push(d, &class, lite, everywhere.clone(), size);
    }
    let (mut n_splits, mut n_decided, mut n_rest) = (0usize, 0usize, 0usize);
    let mut done: Vec<(Lite, Option<PointSet>)> = Vec::new();
    while let Some((o, pts)) = work.pop(&class) {
        anyhow::ensure!(work.len() + done.len() < MAX_OUTCOMES, "more than {MAX_OUTCOMES} outcomes splitting undecided selects");
        // The cone of what must be resolved, in node order: the outcome's
        // own, not the arena's (that would be quadratic).
        let reach = cone(&d.graph, &o.row());
        let first: Option<NodeId> = reach
            .iter()
            .filter(|n| d.graph.get(**n).op == Op::Sel)
            .map(|n| d.graph.get(*n).args[0])
            .find(|c| d.lane_undecidable(*c));
        let Some(c) = first else {
            // The row is resolved; the error is settled, not split.
            let o = settle_error(d, points.as_mut(), pts.as_ref(), room, o);
            let same = |p: &Lite| class[p.t] == class[o.t] && p.fields == o.fields;
            match done.iter_mut().find(|(p, _)| same(p)) {
                Some((p, pp)) => {
                    p.merge(d, o.guard, o.error, &o.rests);
                    if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                        a.union(b);
                    }
                }
                None => done.push((o, pts)),
            }
            continue;
        };
        // Split on the finest undecidable part: the lowest comparison a lane
        // can hold both ways.
        let atom = cone(&d.graph, &[c])
            .into_iter()
            .find(|n| matches!(d.graph.get(*n).op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::UnknownBool(_)) && d.lane_undecidable(*n))
            .unwrap_or(c);
        // Where each answer can come from among this path's points: an
        // answer no point gives is no side, and with one side left the
        // comparison is DECIDED (no guard narrowing).
        let sides: Vec<(bool, Option<PointSet>)> = match (points.as_mut(), &pts) {
            (Some(p), Some(here)) => {
                let (yes, no) = p.answers(d, room, atom);
                [(true, yes.and(here)), (false, no.and(here))].into_iter().filter(|(_, s)| !s.is_empty()).map(|(a, s)| (a, Some(s))).collect()
            }
            _ => vec![(true, None), (false, None)],
        };
        let decided = sides.len() == 1;
        if decided {
            n_decided += 1;
        } else {
            n_splits += 1;
        }
        let (may_true, may_false) = d.may_answers(atom);
        // The equal side of `x == k` (`k` a literal point, `x` a set) reads
        // `k` wherever it read `x`: exact, and a near level's exact floor must
        // store the `state` it narrowed to. The other side learns nothing an
        // interval can hold.
        let narrowed = {
            let node = d.graph.get(atom);
            let point = |n: NodeId| matches!(d.graph.get(n).op, Op::Const(lo, hi) if lo == hi);
            match (node.op == Op::Eq, node.args.first().copied(), node.args.get(1).copied()) {
                (true, Some(a), Some(b)) if point(b) && d.abstract_beneath_lane_ops(a) => Some((a, b)),
                (true, Some(a), Some(b)) if point(a) && d.abstract_beneath_lane_ops(b) => Some((b, a)),
                _ => None,
            }
        };
        // The atom's SIBLINGS in the select's condition: the other operands
        // of an `And`/`Or` the atom (or its negation) is in (`not check(player,
        // 0, 0)` beside `delay <= 0`). With the condition itself, what a side
        // may be case split on to merge (`absorbed`).
        let sibs: Vec<NodeId> = {
            let not_atom = d.graph.fold(Op::Not, vec![atom]);
            let is_atom = |n: NodeId| n == atom || n == not_atom;
            let mut out: Vec<NodeId> = Vec::new();
            for n in cone(&d.graph, &[c]) {
                let node = d.graph.get(n);
                if matches!(node.op, Op::And | Op::Or) && node.args.iter().any(|a| is_atom(*a)) {
                    for a in node.args.iter().copied().filter(|a| !is_atom(*a)) {
                        if !out.contains(&a) {
                            out.push(a);
                        }
                    }
                }
            }
            out
        };
        let reach = cone(&d.graph, &o.every());
        let mut made: Vec<(Lite, Option<PointSet>, Vec<NodeId>)> = Vec::new();
        for (answer, side_pts) in sides {
            let to = d.graph.leaf(Op::ConstBool(answer));
            let mut subst = rustc_hash::FxHashMap::from_iter([(atom, to)]);
            if let (true, Some((x, k))) = (answer, narrowed) {
                subst.insert(x, k);
            }
            let map = rebuild_all(d, &reach, &subst);
            let m = |n: NodeId| map.get(&n).copied().unwrap_or(n);
            let mut guard = m(o.guard);
            if !decided {
                let may = if answer { may_true } else { may_false };
                guard = d.and(&guard, &may);
            }
            if d.decide(&guard) == Some(false) {
                continue;
            }
            let mut side = Lite {
                t: o.t,
                guard,
                error: m(o.error),
                fields: o.fields.iter().map(|f| m(*f)).collect(),
                rests: Vec::new(),
            };
            // This split's candidates first, then the inherited ones.
            let mut fresh: Vec<NodeId> = Vec::new();
            for r in std::iter::once(c).chain(sibs.iter().copied()).map(m) {
                if !fresh.contains(&r) {
                    fresh.push(r);
                }
            }
            for r in fresh.iter().copied().chain(o.rests.iter().map(|r| m(*r))) {
                if !side.rests.contains(&r) && side.rests.len() < MAX_RESTS {
                    side.rests.push(r);
                }
            }
            made.push((side, side_pts, fresh));
        }
        // A side that is two outcomes already made, case split on a
        // condition every lane decides, is them (`absorbed`).
        let rows: Vec<RowKey> = made.iter().map(|(l, _, _)| Waiting::key(&class, l)).collect();
        for (si, (side, side_pts, fresh)) in made.into_iter().enumerate() {
            let made_elsewhere = |k: &RowKey| {
                rows.iter().enumerate().any(|(j, r)| j != si && r == k) || work.index.contains_key(k) || done.iter().any(|(p, _)| Waiting::key(&class, p) == *k)
            };
            match absorbed(d, &class, &side, &fresh, made_elsewhere) {
                Some(halves) => {
                    n_rest += 1;
                    for h in halves {
                        let size = cone(&d.graph, &h.row()).len();
                        work.push(d, &class, h, side_pts.clone(), size);
                    }
                }
                None => {
                    let size = cone(&d.graph, &side.row()).len();
                    work.push(d, &class, side, side_pts, size);
                }
            }
        }
    }
    // Absorption again once every outcome is made (halves made after the
    // split). Absorbing makes no row, so an outcome is retried only when a
    // merge gives it new candidates; rows never change, so they are kept
    // beside `done`.
    let mut keys: Vec<_> = done.iter().map(|(p, _)| Waiting::key(&class, p)).collect();
    let mut dirty = vec![true; done.len()];
    while let Some(i) = dirty.iter().position(|x| *x) {
        dirty[i] = false;
        let rests = done[i].0.rests.clone();
        let Some(halves) = absorbed(d, &class, &done[i].0, &rests, |k| *k != keys[i] && keys.contains(k)) else {
            continue;
        };
        let (_, pts) = done.remove(i);
        keys.remove(i);
        dirty.remove(i);
        for h in halves {
            let key = Waiting::key(&class, &h);
            let at = keys.iter().position(|k| *k == key).expect("an absorbing half's row is an outcome");
            let (p, pp) = &mut done[at];
            let had = p.rests.len();
            p.merge(d, h.guard, h.error, &h.rests);
            dirty[at] |= p.rests.len() > had;
            if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                a.union(b);
            }
        }
        n_rest += 1;
    }
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build] split_undecided_selects: {n_in} outcomes, {n_splits} splits, {n_decided} decided by the points, {n_rest} absorbed by a case split, {} out", done.len());
    }
    // Each outcome whole again, over its template; of the points, only its
    // pixels reach the kernel.
    let mut whole = Vec::with_capacity(done.len());
    for (lite, pts) in done {
        // The error settled again: merges copied guards into it, whose
        // selects are only exact case by case.
        let lite = settle_error(d, points.as_mut(), pts.as_ref(), room, lite);
        let mut o = outs[lite.t].clone();
        // The guard, three-valued, with the row's splits substituted.
        o.guard = three_valued(d, lite.guard);
        o.error = lite.error;
        let nf = o.fields.len();
        for (f, n) in o.fields.iter_mut().zip(&lite.fields[..nf]) {
            f.1 = *n;
        }
        o.arc = lite.fields[nf..].to_vec();
        if let (Some(p), Some(pts)) = (points.as_ref(), pts.as_ref()) {
            let g = p.position_guard(d, pts);
            o.guard = d.and(&o.guard, &g);
        }
        whole.push(o);
    }
    Ok(whole)
}

/// `root` - a guard or an error - with no select on an undecided condition left for the
/// kernel to read by a garbage bit (`split_undecided_selects`). A boolean
/// select becomes `(c and a) or (not c and b)`, exact in Kleene logic. A
/// NUMBER selected on an undecided `c` becomes the hull of its arms where
/// the lane does not decide `c`, and the select where it does:
/// `Sel(Known(c), Sel(c, x, y), [min, max])` - a straddling comparison on
/// the hull reads as "may be live", which `live` over-approximates anyway.
/// (Pushing the readers into the arms is exact but exponential.) Except for
/// the ops the kernel computes on exact operands only (`exact_only`): those
/// are pushed into the arms, up to `ARMS` combinations (`arms`).
///
/// Also the error's, once the row is resolved (`settle_error`).
fn three_valued(d: &mut Symbolic, root: NodeId) -> NodeId {
    use crate::transpile::graph::Op;
    let nodes = cone(&d.graph, &[root]);
    // A select this already wrapped (`Sel(Known(c), Sel(c, ..), hull)`)
    // stays as it is, keeping the registration that bounds its error:
    // idempotent.
    let wrapped: rustc_hash::FxHashSet<NodeId> = nodes
        .iter()
        .filter_map(|n| {
            let node = d.graph.get(*n);
            if node.op != Op::Sel {
                return None;
            }
            let (k, inner) = (d.graph.get(node.args[0]), d.graph.get(node.args[1]));
            (k.op == Op::Known && inner.op == Op::Sel && inner.args[0] == k.args[0]).then_some(node.args[1])
        })
        .collect();
    let undecided = |d: &mut Symbolic, n: NodeId| !wrapped.contains(&n) && d.graph.get(n).op == Op::Sel && d.lane_undecidable(d.graph.get(n).args[0]);
    if !nodes.iter().any(|n| undecided(d, *n)) {
        return root;
    }
    // The booleans, typed bottom-up from what each node IS (a boolean op, a
    // boolean cell, a select of booleans), never from who reads it.
    let mut boolean: rustc_hash::FxHashSet<NodeId> = Default::default();
    for &n in &nodes {
        let node = d.graph.get(n);
        let b = match node.op {
            Op::Sel => boolean.contains(&node.args[1]) || boolean.contains(&node.args[2]),
            Op::Cell(c) => d.graph.cell_kind(c) == crate::transpile::graph::CellKind::Bool,
            ref o => is_bool_op(o),
        };
        if b {
            boolean.insert(n);
        }
    }
    let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = Default::default();
    // What an undecided select reaches (its readers, transitively), and the
    // exact values of those numbers per arm (`arms`).
    let mut affected: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut arms_memo: rustc_hash::FxHashMap<NodeId, Option<Vec<(NodeId, NodeId)>>> = Default::default();
    let yes = d.graph.leaf(Op::ConstBool(true));
    for &n in &nodes {
        let (op, args) = {
            let node = d.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        if wrapped.contains(&n) {
            map.insert(n, n);
            continue;
        }
        let a: Vec<NodeId> = args.iter().map(|x| *map.get(x).unwrap_or(x)).collect();
        let sel_undecided = op == Op::Sel && (d.lane_undecidable(args[0]) || d.lane_undecidable(a[0]));
        // Undecided as REWRITTEN: a condition that became undecidable here (a
        // tile test past `ARMS`, an unknown now) makes a select that was
        // decided before an undecided one.
        let new = if sel_undecided {
            let (c, x, y) = (a[0], a[1], a[2]);
            if boolean.contains(&n) {
                let nc = d.graph.fold(Op::Not, vec![c]);
                let tx = d.graph.fold(Op::And, vec![c, x]);
                let fy = d.graph.fold(Op::And, vec![nc, y]);
                d.graph.fold(Op::Or, vec![tx, fy])
            } else {
                let decided = d.graph.fold(Op::Known, vec![c]);
                let picked = d.graph.fold(Op::Sel, vec![c, x, y]);
                // Evaluated only where the lane decides `c`, so its own error
                // holds nowhere else. SET, not OR-ed: `picked` is the original
                // select (hash-consed), whose earlier registration would make
                // `not Known(c)` hold everywhere. This wrapper is the only
                // place a select on an undecided condition survives.
                d.evaluated.insert(picked, decided);
                let (xl, yl) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Lo, vec![y]));
                let (xh, yh) = (d.graph.fold(Op::Hi, vec![x]), d.graph.fold(Op::Hi, vec![y]));
                let lo = d.graph.fold(Op::Min, vec![xl, yl]);
                let hi = d.graph.fold(Op::Max, vec![xh, yh]);
                let hull = d.graph.fold(Op::Span, vec![lo, hi]);
                d.graph.fold(Op::Sel, vec![decided, picked, hull])
            }
        } else if exact_only(d, &op, &args) && args.iter().any(|x| affected.contains(x) && !boolean.contains(x) && !is_bool_op(&d.graph.get(*x).op)) {
            // An exact-only op reading a hull: distributed into the arms, one
            // per combination of the undecided selects it reads (a boolean
            // joined Kleene-wise, a number the hull of the arms' values).
            // Past `ARMS`, the weakest value (unknown, or the whole range).
            let mut combos: Option<Vec<(NodeId, Vec<NodeId>)>> = Some(vec![(yes, Vec::new())]);
            for (k, x) in args.iter().enumerate() {
                let alts = if affected.contains(x) && !boolean.contains(x) && !is_bool_op(&d.graph.get(*x).op) {
                    arms(d, *x, &map, &affected, &boolean, &mut arms_memo)
                } else {
                    Some(vec![(yes, a[k])])
                };
                combos = match (combos, alts) {
                    (Some(cs), Some(alts)) if cs.len() * alts.len() <= ARMS => Some(
                        cs.iter()
                            .flat_map(|(c, vs)| {
                                alts.iter().map(move |(ac, v)| {
                                    let mut vs = vs.clone();
                                    vs.push(*v);
                                    ((*c, *ac), vs)
                                })
                            })
                            .collect::<Vec<_>>()
                            .into_iter()
                            .map(|((c, ac), vs)| (d.graph.fold(Op::And, vec![c, ac]), vs))
                            .collect(),
                    ),
                    _ => None,
                };
            }
            let boolean_op = is_bool_op(&op);
            match combos {
                Some(cs) if boolean_op => {
                    let mut out = d.graph.leaf(Op::ConstBool(false));
                    for (c, vs) in cs {
                        let v = d.graph.fold(op.clone(), vs);
                        let both = d.graph.fold(Op::And, vec![c, v]);
                        out = d.graph.fold(Op::Or, vec![out, both]);
                    }
                    out
                }
                Some(cs) => {
                    let vals: Vec<NodeId> = cs.into_iter().map(|(_, vs)| d.graph.fold(op.clone(), vs)).collect();
                    hull_of(d, &vals)
                }
                None if boolean_op => {
                    let u = d.unknown_bool_atom();
                    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
                        eprintln!("[build] three_valued: {op:?} past {ARMS} arms, unknown {:?}", d.graph.get(u).op);
                    }
                    u
                }
                None => {
                    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
                        eprintln!("[build] three_valued: {op:?} past {ARMS} arms, the whole range");
                    }
                    d.graph.leaf(Op::Const(i32::MIN, i32::MAX))
                }
            }
        } else if a == args {
            n
        } else {
            d.graph.fold(op.clone(), a)
        };
        if args.iter().any(|x| affected.contains(x)) || sel_undecided {
            affected.insert(n);
        }
        map.insert(n, new);
    }
    map[&root]
}

/// `three_valued`'s bound on the arms an exact-only op is distributed over.
const ARMS: usize = 64;

/// Ops the kernel computes on exact operands only (`asm::codegen`'s
/// `as_num`): an interval must not reach them. `Mul`/`Div` take an interval
/// scaled by a positive literal (`pos_const_scalar`), and only that.
fn exact_only(d: &Symbolic, op: &crate::transpile::graph::Op, args: &[NodeId]) -> bool {
    use crate::transpile::graph::Op;
    let pos = |n: NodeId| matches!(d.graph.get(n).op, Op::Const(lo, hi) if lo == hi && lo > 0);
    match op {
        Op::TileFlagAt | Op::Mget | Op::Rem | Op::Sin => true,
        Op::Mul => !(pos(args[0]) || pos(args[1])),
        Op::Div => !pos(args[1]),
        _ => false,
    }
}

/// Ops whose value is a boolean.
fn is_bool_op(op: &crate::transpile::graph::Op) -> bool {
    use crate::transpile::graph::Op;
    matches!(
        op,
        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Not | Op::And | Op::Or | Op::Known | Op::ConstBool(_) | Op::UnknownBool(_) | Op::TileFlagAt
            | Op::SplitValid(_) | Op::SplitOk(_) | Op::FragOk(_) | Op::NoWrap
    )
}

/// The hull `[min lo, max hi]` of numbers.
fn hull_of(d: &mut Symbolic, vals: &[NodeId]) -> NodeId {
    use crate::transpile::graph::Op;
    let (mut lo, mut hi) = (d.graph.fold(Op::Lo, vec![vals[0]]), d.graph.fold(Op::Hi, vec![vals[0]]));
    for v in &vals[1..] {
        let (l, h) = (d.graph.fold(Op::Lo, vec![*v]), d.graph.fold(Op::Hi, vec![*v]));
        lo = d.graph.fold(Op::Min, vec![lo, l]);
        hi = d.graph.fold(Op::Max, vec![hi, h]);
    }
    d.graph.fold(Op::Span, vec![lo, hi])
}

/// A number `three_valued` hulled, as the exact values it takes and where:
/// `(condition, value)` over the undecided selects under it, the conditions
/// Kleene (`map`'s). `None` past `ARMS`.
fn arms(
    d: &mut Symbolic,
    n: NodeId,
    map: &rustc_hash::FxHashMap<NodeId, NodeId>,
    affected: &rustc_hash::FxHashSet<NodeId>,
    boolean: &rustc_hash::FxHashSet<NodeId>,
    memo: &mut rustc_hash::FxHashMap<NodeId, Option<Vec<(NodeId, NodeId)>>>,
) -> Option<Vec<(NodeId, NodeId)>> {
    use crate::transpile::graph::Op;
    if let Some(hit) = memo.get(&n) {
        return hit.clone();
    }
    let yes = d.graph.leaf(Op::ConstBool(true));
    let (op, args) = {
        let node = d.graph.get(n);
        (node.op.clone(), node.args.clone())
    };
    let numeric = |d: &Symbolic, x: NodeId| affected.contains(&x) && !boolean.contains(&x) && !is_bool_op(&d.graph.get(x).op);
    let out = if !affected.contains(&n) {
        Some(vec![(yes, *map.get(&n).unwrap_or(&n))])
    } else if op == Op::Sel && (d.lane_undecidable(args[0]) || d.lane_undecidable(*map.get(&args[0]).unwrap_or(&args[0]))) {
        // Undecided as rewritten too (`three_valued`'s `sel_undecided`): an
        // arm per answer, never a select left on it.
        let c = *map.get(&args[0]).unwrap_or(&args[0]);
        let nc = d.graph.fold(Op::Not, vec![c]);
        match (arms(d, args[1], map, affected, boolean, memo), arms(d, args[2], map, affected, boolean, memo)) {
            (Some(t), Some(f)) if t.len() + f.len() <= ARMS => {
                let mut v = Vec::with_capacity(t.len() + f.len());
                for (k, x) in t {
                    v.push((d.graph.fold(Op::And, vec![k, c]), x));
                }
                for (k, x) in f {
                    v.push((d.graph.fold(Op::And, vec![k, nc]), x));
                }
                Some(v)
            }
            _ => None,
        }
    } else {
        let mut combos: Option<Vec<(NodeId, Vec<NodeId>)>> = Some(vec![(yes, Vec::new())]);
        for x in &args {
            let alts = if numeric(d, *x) { arms(d, *x, map, affected, boolean, memo) } else { Some(vec![(yes, *map.get(x).unwrap_or(x))]) };
            combos = match (combos, alts) {
                (Some(cs), Some(alts)) if cs.len() * alts.len() <= ARMS => {
                    let mut next = Vec::with_capacity(cs.len() * alts.len());
                    for (c, vs) in &cs {
                        for (ac, v) in &alts {
                            let mut vs = vs.clone();
                            vs.push(*v);
                            next.push((d.graph.fold(Op::And, vec![*c, *ac]), vs));
                        }
                    }
                    Some(next)
                }
                _ => None,
            };
        }
        combos.map(|cs| cs.into_iter().map(|(c, vs)| (c, d.graph.fold(op.clone(), vs))).collect())
    };
    memo.insert(n, out.clone());
    out
}

/// The nodes `roots` reach, ascending (operands precede their node).
pub(crate) fn cone(g: &crate::transpile::graph::Graph, roots: &[NodeId]) -> Vec<NodeId> {
    let mut seen: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if seen.insert(n) {
            stack.extend(g.get(n).args.iter().copied());
        }
    }
    let mut out: Vec<NodeId> = seen.into_iter().collect();
    out.sort_unstable();
    out
}

/// How many case-split candidates an outcome carries (`Lite::rests`).
const MAX_RESTS: usize = 32;

/// `o` CASE-SPLIT on one of `cands`, a condition every lane decides, where
/// both halves are outcomes already made (`made`): its halves, to merge into
/// them, or `None`.
///
/// With a split's atom substituted, the rest of a select's condition may be
/// one every lane DECIDES (a fall floor's `delay <= 0 and not check(player,
/// 0, 0)` on its `delay <= 0` side). That select would stay in the row as a
/// third variant beside "solid" and "hidden" - on every lane one of the two,
/// as a row neither - multiplying outcomes by 3 per floor instead of 2. Split
/// on the decided rest, the halves are the two existing rows.
///
/// EXACT: a lane decides `cv`, so it takes exactly the half its own answer
/// gives, with the outcome's row, error and guard on that lane. Taken only
/// where BOTH halves merge, so it never adds an outcome.
fn absorbed(
    d: &mut Symbolic,
    class: &[usize],
    o: &Lite,
    cands: &[NodeId],
    made: impl Fn(&RowKey) -> bool,
) -> Option<Vec<Lite>> {
    use crate::transpile::graph::Op;
    // Only the ROW is rebuilt: on a half's lanes the guard and error read
    // `cv` as the lane decides it, so they are unchanged (and an error's cone
    // can be huge).
    let mut reach: Option<Vec<NodeId>> = None;
    for &cv in cands {
        if d.lane_undecidable(cv) || d.decide(&cv).is_some() {
            continue;
        }
        let reach = reach.get_or_insert_with(|| cone(&d.graph, &o.row()));
        if reach.binary_search(&cv).is_err() {
            continue;
        }
        let mut halves = Vec::new();
        for half in [true, false] {
            let to = d.graph.leaf(Op::ConstBool(half));
            let map = rebuild_all(d, reach, &rustc_hash::FxHashMap::from_iter([(cv, to)]));
            let m = |n: NodeId| map.get(&n).copied().unwrap_or(n);
            let at = if half { cv } else { d.not(&cv) };
            let guard = d.and(&o.guard, &at);
            if d.decide(&guard) == Some(false) {
                continue;
            }
            halves.push(Lite {
                t: o.t,
                guard,
                error: o.error,
                fields: o.fields.iter().map(|f| m(*f)).collect(),
                rests: o.rests.iter().map(|r| m(*r)).filter(|r| *r != cv).collect(),
            });
        }
        if !halves.is_empty() && halves.iter().all(|h| made(&Waiting::key(class, h))) {
            return Some(halves);
        }
    }
    None
}

/// Every node of `cone` rebuilt with the nodes of `subst` replaced, in node
/// order; comparisons refolded against the static ranges
/// (`Symbolic::compare`), the rest by `Graph::fold`. Only the nodes that
/// changed are in the map.
///
/// Where a node was EVALUATED (`Symbolic::evaluated`, which bounds its own
/// error) goes with it: the rebuild is registered at the rebuilt condition,
/// so the cone is widened to those conditions first. Dropped, the rebuild's
/// own error would hold on every lane.
fn rebuild_all(d: &mut Symbolic, cone: &[NodeId], subst: &rustc_hash::FxHashMap<NodeId, NodeId>) -> rustc_hash::FxHashMap<NodeId, NodeId> {
    use super::domain::{Cmp, Domain};
    use crate::transpile::graph::Op;
    let mut all: Vec<NodeId> = cone.to_vec();
    loop {
        let have: rustc_hash::FxHashSet<NodeId> = all.iter().copied().collect();
        let more: Vec<NodeId> = all.iter().filter_map(|n| d.evaluated.get(n).copied()).filter(|a| !have.contains(a)).collect();
        if more.is_empty() {
            break;
        }
        all.extend(more);
        all = self::cone(&d.graph, &all);
    }
    let cone = &all[..];
    let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = subst.clone();
    for &i in cone {
        if subst.contains_key(&i) {
            continue;
        }
        let (op, args) = {
            let node = d.graph.get(i);
            (node.op.clone(), node.args.clone())
        };
        let mapped: Vec<NodeId> = args.iter().map(|a| *map.get(a).unwrap_or(a)).collect();
        if mapped == args {
            continue;
        }
        let cmp = match op {
            Op::Lt => Some(Cmp::Lt),
            Op::Le => Some(Cmp::Le),
            Op::Gt => Some(Cmp::Gt),
            Op::Ge => Some(Cmp::Ge),
            _ => None,
        };
        let by_range = cmp.and_then(|k| d.compare(k, &mapped[0], &mapped[1]).ok()).filter(|r| matches!(d.graph.get(*r).op, Op::ConstBool(_)));
        let n = match by_range {
            Some(r) => r,
            None => d.graph.fold(op, mapped),
        };
        map.insert(i, n);
    }
    let moved: Vec<(NodeId, NodeId)> = cone.iter().filter_map(|i| Some((*map.get(i)?, *d.evaluated.get(i)?))).collect();
    for (n, at) in moved {
        let at = *map.get(&at).unwrap_or(&at);
        d.evaluated_at(&n, &at);
    }
    map
}

/// The player: the object with a `djump` field (not a position: objects
/// are deleted from the list).
#[cfg(test)]
pub fn find_player<D: Domain>(st: &State<D>) -> Option<Path> {
    let objs = vec![iface::key("objects")];
    let Some(Value::Table(t)) = iface::get(st, &objs) else { return None };
    for i in 0..st.heap.tables[&t].arr.len() {
        let mut p = objs.clone();
        p.push(Step::Idx(i));
        if let Some(Value::Table(o)) = iface::get(st, &p) {
            if st.heap.tables[&o].hash.contains_key("djump") {
                return Some(p);
            }
        }
    }
    None
}

/// The six "pm1 key" cells, as tracer paths: globals `has_dashed` and
/// `freeze`, and player fields `dash_time`, `djump`, `p_dash`, `p_jump`.
/// A test fixture for pinning a body to one key.
#[cfg(test)]
pub fn pm1_paths(player: &Path) -> Vec<Path> {
    let mut v: Vec<Path> = vec![vec![iface::key("has_dashed")], vec![iface::key("freeze")]];
    for f in ["dash_time", "djump", "p_dash", "p_jump"] {
        let mut q = player.clone();
        q.push(iface::key(f));
        v.push(q);
    }
    v
}

/// The pm1 key a state is IN: the six paths at the values it holds, as
/// `trace_frame`'s pin list.
#[cfg(test)]
pub fn pm1_key(player: &Path, st: &State<Symbolic>, d: &Symbolic) -> Result<Vec<(Path, Conc)>> {
    let mut out = Vec::new();
    for p in pm1_paths(player) {
        let c = match iface::get(st, &p) {
            Some(Value::Num(n)) => Conc::Num(
                d.as_const(&n)
                    .ok_or_else(|| anyhow!("pm1 cell {} is symbolic", iface::show(&p)))?,
            ),
            Some(Value::Bool(b)) => Conc::Bool(
                d.decide(&b)
                    .ok_or_else(|| anyhow!("pm1 cell {} is symbolic", iface::show(&p)))?,
            ),
            other => bail!("pm1 cell {} is {:?}, not a scalar", iface::show(&p), other),
        };
        out.push((p, c));
    }
    Ok(out)
}

/// Write a concrete button assignment, for the oracle side.
#[cfg(test)]
pub fn set_buttons(d: &mut Symbolic, st: &mut State<Symbolic>, bits: &[bool; 6]) -> Result<()> {
    for (i, b) in bits.iter().enumerate() {
        let v = d.boolean(*b);
        iface::set(st, &[iface::key("__button_states"), Step::Idx(i)], Value::Bool(v))?;
    }
    Ok(())
}

/// The input vector for one PERTURBATION of the traced state: the values
/// the frame was traced at, with some slots replaced.
///
/// Checking only at the traced values would pass a graph that had folded
/// every input away.
#[cfg(test)]
pub fn cells_with(iface: &Iface, over: &[(Path, Conc)]) -> Result<Vec<Conc>> {
    let mut v = iface.init.clone();
    for (p, c) in over {
        let i = iface
            .slots
            .iter()
            .position(|q| q == p)
            .ok_or_else(|| anyhow!("{} is not an input cell", iface::show(p)))?;
        v[i] = *c;
    }
    Ok(v)
}

/// A traced frame's graph with its six buttons RESOLVED to one input
/// (`bits`, in `__button_states` order), for the concrete evaluator, which
/// has no value for a fork: the cone of the frame's outcomes specialized
/// into a fresh graph, and the map to it. The buttons are the forks
/// `Symbolic::unknown_bool` minted, in `__reset_button_states` order; every
/// other fork is left standing.
#[cfg(test)]
pub fn at_buttons(g: &Graph, f: &Frame, bits: &[bool; 6]) -> Result<(Graph, Vec<NodeId>)> {
    let buttons: Vec<u8> = f.fork_origins.iter().filter(|(_, o)| o == super::domain::UNKNOWN_BOOL_ORIGIN).map(|(d, _)| *d).collect();
    if buttons.len() != 6 {
        bail!("the frame minted {} unknown booleans, not the six buttons", buttons.len());
    }
    let mut cfg = vec![crate::transpile::graph::OPEN; 256];
    for (d, b) in buttons.iter().zip(bits) {
        cfg[*d as usize] = *b as u8;
    }
    let roots: Vec<NodeId> = f.outs.iter().flat_map(|o| o.fields.iter().map(|(_, n, _)| *n).chain([o.guard, o.error])).collect();
    let need = crate::transpile::bdd::reachable(g, &roots);
    let mut out = g.like();
    let map = g.specialize_subset_into(&cfg, Some(&need), &mut out);
    Ok((out, map))
}

/// Check one traced frame against the oracle at one (inputs, buttons)
/// point, `at` being the frame at those buttons (`at_buttons`). Returns
/// `(which outcome claimed it, fields compared)`.
#[cfg(test)]
pub fn check_at(
    it: &Interp<'_, Symbolic>,
    f: &Frame,
    at: &(Graph, Vec<NodeId>),
    cells: &[Conc],
    bits: &[bool; 6],
    oracle: &[(Path, Conc)],
) -> Result<(usize, usize)> {
    let env = super::eval::Env {
        cells,
        cart: it.cart.clone(),
        cache: it.cache.clone(),
    };
    let (g, map) = (&at.0, &at.1);
    let mut live: Vec<usize> = Vec::new();
    for (i, o) in f.outs.iter().enumerate() {
        if super::eval::eval(g, map[o.guard as usize], &env)? == Conc::Bool(true) {
            live.push(i);
        }
    }
    // The guards are pairwise disjoint and cover everything (`State::guard`):
    // exactly one outcome claims this lane.
    if live.len() != 1 {
        bail!("{:?}: {} outcomes claim this assignment, not 1", bits, live.len());
    }
    let o = &f.outs[live[0]];
    if super::eval::eval(g, map[o.error as usize], &env)? != Conc::Bool(false) {
        bail!("{:?}: the trace declined this assignment (its error holds)", bits);
    }
    let mut n = 0;
    for (p, want) in oracle {
        // Dead cells (`FrameOut::ubool`) are not computed; skipped, but still
        // counted by the total below. The deadness claim itself is not
        // checked here.
        if o.ubool.contains(p) {
            continue;
        }
        let got = o
            .fields
            .iter()
            .find(|(q, _, _)| q == p)
            .ok_or_else(|| anyhow!("{:?}: traced state has no {}", bits, iface::show(p)))?;
        let got = super::eval::eval(g, map[got.1 as usize], &env)?;
        if got != *want {
            bail!("{:?}: {} is {:?}, oracle says {:?}", bits, iface::show(p), got, want);
        }
        n += 1;
    }
    if o.fields.len() + o.ubool.len() != oracle.len() {
        bail!(
            "{:?}: traced state has {} scalars + {} dead, oracle has {}",
            bits,
            o.fields.len(),
            o.ubool.len(),
            oracle.len()
        );
    }
    Ok((live[0], n))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::cart;

    use super::find_player;

    /// A body compiled for one pm1 key must REFUSE a state in another.
    ///
    /// The pin folds the key's values into the body, so nothing reads those
    /// cells; `pin_guard`'s negation is the frame's error, and this checks it
    /// bites: perturb one pinned cell and the frame declines the lane.
    #[test]
    fn a_body_pinned_to_a_pm1_key_refuses_any_other_key() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = crate::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");
        let mut player = None;
        for _ in 0..40 {
            if let Some(p) = find_player(&st) {
                player = Some(p);
                break;
            }
            st = run_one(&mut it, &frame, st).expect("warm-up");
        }
        let player = player.expect("a player");
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }
        let key = pm1_key(&player, &st, &it.d).expect("pm1 key");
        assert_eq!(key.len(), 6, "the pm1 key is six cells");
        let f = trace_frame(&mut it, &reset, &frame, st, &roots, &key, &[], false, &[]).expect("trace");
        assert_eq!(f.iface.pins.len(), 6, "all six pinned");

        // No error in whichever outcome claims this assignment.
        let (g, map) = at_buttons(&it.d.graph, &f, &[false; 6]).expect("the six buttons");
        let ok_at = |cells: &[Conc]| -> bool {
            let env = super::super::eval::Env {
                cells,
                cart: it.cart.clone(),
                cache: it.cache.clone(),
            };
            let live: Vec<&FrameOut> = f
                .outs
                .iter()
                .filter(|o| super::super::eval::eval(&g, map[o.guard as usize], &env).expect("guard") == Conc::Bool(true))
                .collect();
            assert_eq!(live.len(), 1, "exactly one outcome claims a lane");
            super::super::eval::eval(&g, map[live[0].error as usize], &env).expect("error") == Conc::Bool(false)
        };

        assert!(ok_at(&f.iface.init), "the key it was compiled for is accepted");

        for (i, c) in &f.iface.pins {
            let mut cells = f.iface.init.clone();
            cells[*i] = match c {
                Conc::Num(v) => Conc::Num(*v + crate::pico8_num::Pico8Num::from_parts(1, 0)),
                Conc::Bool(b) => Conc::Bool(!*b),
            };
            assert!(
                !ok_at(&cells),
                "{} moved off its pin and the body still accepted the lane",
                iface::show(&f.iface.slots[*i])
            );
        }
    }

    /// `ice_at` is `tile_flag_at(.., 4)`.
    /// In every room of the map, with that room's collision cache, the
    /// tracer's answer for a concrete rectangle is the tile scan's - true
    /// on some rectangle in the rooms with ice (so the check is not
    /// vacuous), false everywhere in the others (the trace-time fold).
    #[test]
    fn ice_at_answers_the_tile_scan_in_every_room() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        // Parsed before the interpreter, which borrows the ASTs it runs.
        let rects: Vec<(i16, i16, i16, i16)> = (0..16i16)
            .flat_map(|i| (0..16i16).map(move |j| (i * 8, j * 8)))
            .flat_map(|(x, y)| [(x, y, 8, 8), (x + 1, y + 3, 6, 5)])
            .collect();
        let probes: Vec<_> = rects
            .iter()
            .map(|(x, y, w, h)| full_moon::parse(&format!("ice_probe = ice_at({x},{y},{w},{h})")).expect("parse probe"))
            .collect();
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let (rx0, ry0) = crate::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx0, ry0).expect("cache"),
        ));
        it.cart = Some(cd.clone());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let st = run_one(&mut it, &init, st).expect("_init");
        let scan = |rx: i16, ry: i16, x: i16, y: i16, w: i16, h: i16| -> bool {
            let p = |v: i16| crate::pico8_num::Pico8Num::from_i16(v);
            (y.max(0) / 8..=((y + h - 1) / 8).min(15)).any(|ty| {
                (x.max(0) / 8..=((x + w - 1) / 8).min(15)).any(|tx| {
                    let t = cd.mget(p(rx * 16 + tx), p(ry * 16 + ty)).expect("mget");
                    cd.fget(p(t as i16), p(4)).expect("fget")
                })
            })
        };
        let (mut icy, mut dry) = (0, 0);
        for rx in 0..8i16 {
            for ry in 0..4i16 {
                it.cache = Some(std::sync::Arc::new(
                    celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
                ));
                let mut o = st.clone();
                for (k, v) in [("x", rx), ("y", ry)] {
                    let n = it.d.num(crate::pico8_num::Pico8Num::from_i16(v));
                    iface::set(&mut o, &[iface::key("room"), iface::key(k)], Value::Num(n))
                        .expect("set room");
                }
                let mut any = false;
                for (&(x, y, w, h), probe) in rects.iter().zip(&probes) {
                    let s = run_one(&mut it, probe, o.clone()).expect("ice_at answers");
                    let Some(Value::Bool(b)) = iface::get(&s, &[iface::key("ice_probe")]) else {
                        panic!("ice_at did not return a boolean")
                    };
                    let want = scan(rx, ry, x, y, w, h);
                    assert_eq!(it.d.decide(&b), Some(want), "room ({rx},{ry}) ice_at({x},{y},{w},{h})");
                    any |= want;
                }
                if any { icy += 1 } else { dry += 1 }
            }
        }
        eprintln!("[ice] {icy} rooms with ice, {dry} without");
        assert!(icy > 0 && dry > 0, "the map has rooms with and without ice");
    }

    /// `break` in a loop whose bound the tracer CANNOT know.
    ///
    /// The PICO-8 corpus cannot reach `run_for_symbolic` (its limits are
    /// concrete). Trace the loop once with a symbolic limit, then evaluate
    /// the graph at each concrete limit against the hand-worked answer.
    #[test]
    fn break_leaves_a_loop_whose_bound_is_symbolic() {
        // `abs(amount)`: `unroll_bound` is keyed on the limit expression's
        // SOURCE TEXT, and this is the cart's own spelling (`move_x`/`move_y`).
        let src = "
function f(amount)
  local n = 0
  for i=0,abs(amount) do
    if i >= 2 then break end
    n = n + 1
  end
  return n
end
";
        let top = full_moon::parse(src).expect("parse");
        let call = full_moon::parse("result = f(amount)").expect("parse call");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(0));
        iface::set(&mut st, &[iface::key("amount")], Value::Num(cell)).expect("set amount");
        let st = run_one(&mut it, &call, st).expect("call f");
        let Some(Value::Num(node)) = iface::get(&st, &[iface::key("result")]) else {
            panic!("f did not return a number")
        };
        // 100 is past the unroll bound of 8 on purpose: once `break` is
        // reached the bound stops mattering, so the lane must not decline.

        for a in [0i16, 1, 2, 3, 5, 8, 100] {
            let cells = [Conc::Num(crate::pico8_num::Pico8Num::from_i16(a))];
            let env = super::super::eval::Env {
                cells: &cells,
                cart: None,
                cache: None,
            };
            let got = super::super::eval::eval(&it.d.graph, node, &env).expect("eval result");
            let want = (a + 1).min(2);
            assert_eq!(
                got,
                Conc::Num(crate::pico8_num::Pico8Num::from_i16(want)),
                "amount = {}",
                a
            );
            let beyond = super::super::eval::eval(&it.d.graph, st.ended, &env).expect("eval ended");
            assert_eq!(beyond, Conc::Bool(false), "amount = {}: the trace declined", a);
        }
    }

    /// Trace ONE frame with the player's fields symbolic and the six
    /// buttons free, then check that one graph against the oracle at
    /// every point of a position/speed sweep crossed with all 64 button
    /// assignments: ONE graph must answer for all of them (checking only at
    /// the traced values would pass a graph that folded its inputs away).
    ///
    /// Every step up to the comparison must succeed, or the test passes
    /// without comparing anything. A point the trace DECLINES is a refusal,
    /// counted, not a wrong answer.
    #[test]
    fn a_traced_frame_agrees_with_the_oracle() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");

        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = crate::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);

        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");

        // Warm up with the buttons concrete, past the spawn animation (which
        // reads no buttons and would make the check vacuous).
        let mut player = None;
        for n in 0..40 {
            if let Some(p) = find_player(&st) {
                player = Some((n, p));
                break;
            }
            st = run_one(&mut it, &frame, st).unwrap_or_else(|e| panic!("[verify] warm-up frame {n} stopped at: {e:#}"));
        }
        let (warm, player) = player.expect("[verify] a player within 40 warm-up frames");
        eprintln!("[verify] player at {} after {} warm-up frames", iface::show(&player), warm);

        // The rest of the frame's INPUT state; everything else reachable is
        // static configuration or the button slots.
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in [
            "deaths",
            "delay_restart",
            "frames",
            "freeze",
            "has_dashed",
            "has_key",
            "max_djump",
            "minutes",
            "pause_player",
            "seconds",
            "will_restart",
        ] {
            roots.push(vec![iface::key(g)]);
        }

        let before = it.d.graph.len();
        let f = trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &[], &[], false, &[])
            .unwrap_or_else(|e| panic!("[verify] symbolic frame stopped at: {e:#}"));
        eprintln!(
            "[verify] {} input cells, {} outcome(s), {} nodes ({} new)",
            f.iface.slots.len(),
            f.outs.len(),
            it.d.graph.len(),
            it.d.graph.len() - before
        );

        // PERTURB THE INPUTS as well as the buttons: the same override into
        // the heap for the oracle and into the cell vector for the graph.
        // Nothing is re-traced.
        let px = |k: &str| {
            let mut p = player.clone();
            p.push(iface::key(k));
            p
        };
        let sub = |k: &str, s: &str| {
            let mut p = player.clone();
            p.push(iface::key(k));
            p.push(iface::key(s));
            p
        };
        let at = |p: &Path| match f.iface.init[f.iface.slots.iter().position(|q| q == p).unwrap()] {
            Conc::Num(n) => n,
            Conc::Bool(_) => panic!("{} is a boolean", iface::show(p)),
        };
        let n = crate::pico8_num::Pico8Num::from_i16;
        // A cross product over position and speed: it hits walls, floors
        // and the pit below the room, i.e. the non-trivial output SHAPES.
        let mut perts: Vec<(String, Vec<(Path, Conc)>)> = Vec::new();
        for dx in [-8i16, -1, 0, 1, 8] {
            for dy in [-8i16, -1, 0, 1, 8, 24, 64] {
                // A falling speed of 8 steps past the unroll bound: the
                // REFUSAL case, on one column of the sweep only.
                let sys: &[Option<i16>] =
                    if dx == 0 { &[None, Some(-2), Some(2), Some(8)] } else { &[None, Some(-2), Some(2)] };
                for sy in sys.iter().copied() {
                    let mut over = vec![
                        (px("x"), Conc::Num(at(&px("x")) + n(dx))),
                        (px("y"), Conc::Num(at(&px("y")) + n(dy))),
                    ];
                    if let Some(v) = sy {
                        over.push((sub("spd", "y"), Conc::Num(n(v))));
                    }
                    perts.push((format!("dx{} dy{} sy{:?}", dx, dy, sy), over));
                }
            }
        }
        // The control-flow inputs, one at a time: each changes which BRANCH
        // the frame takes, so crossing them adds nothing.
        let g = |k: &str| vec![iface::key(k)];
        for (name, over) in [
            ("freeze", vec![(g("freeze"), Conc::Num(n(1)))]),
            ("pause_player", vec![(g("pause_player"), Conc::Bool(true))]),
            ("will_restart", vec![(g("will_restart"), Conc::Bool(true))]),
            (
                "restarting",
                vec![
                    (g("will_restart"), Conc::Bool(true)),
                    (g("delay_restart"), Conc::Num(n(1))),
                ],
            ),
            ("max_djump", vec![(g("max_djump"), Conc::Num(n(2)))]),
            ("has_dashed", vec![(g("has_dashed"), Conc::Bool(true))]),
            ("no djump", vec![(px("djump"), Conc::Num(n(0)))]),
            ("no grace", vec![(px("grace"), Conc::Num(n(0)))]),
            ("dashing", vec![(px("dash_time"), Conc::Num(n(3)))]),
            ("dash effect", vec![(px("dash_effect_time"), Conc::Num(n(5)))]),
            ("frames", vec![(g("frames"), Conc::Num(n(29)))]),
            // Out of the TOP of the room: `next_room()`, the largest shape.
            ("top edge", vec![(px("y"), Conc::Num(at(&px("y")) - n(120)))]),
            ("right edge", vec![(px("x"), Conc::Num(at(&px("x")) + n(124)))]),
            ("left edge", vec![(px("x"), Conc::Num(at(&px("x")) - n(16)))]),
        ] {
            perts.push((name.to_string(), over));
        }

        // WHAT are the outcomes? Only shape divergence can leave more than
        // one, so two with the SAME shape are a `collapse` bug; same scalar
        // count but different shapes are printed for a closer look.
        {
            let mut by_len: std::collections::BTreeMap<usize, usize> = Default::default();
            for o in &f.outs {
                *by_len.entry(o.fields.len()).or_default() += 1;
            }
            eprintln!("[verify] outcomes by scalar count: {:?}", by_len);
            // The object list is what distinguishes them.
            for (i, o) in f.outs.iter().enumerate() {
                let mut objs: std::collections::BTreeSet<String> = Default::default();
                for (q, _, _) in &o.fields {
                    let pre = iface::show(&q[..q.len().saturating_sub(1)].to_vec());
                    if pre.starts_with("objects") {
                        objs.insert(pre);
                    }
                }
                eprintln!("[verify]   outcome {}: {} scalars, {:?}", i, o.fields.len(), objs);
            }
            let mut same = 0;
            for i in 0..f.outs.len() {
                for j in (i + 1)..f.outs.len() {
                    if f.outs[i].shape == f.outs[j].shape {
                        same += 1;
                    }
                }
            }
            assert_eq!(same, 0, "two outcomes with equal shapes did not merge");
            // Where the first same-size pair parts company.
            'pair: for i in 0..f.outs.len() {
                for j in (i + 1)..f.outs.len() {
                    let (a, b) = (&f.outs[i].shape, &f.outs[j].shape);
                    if f.outs[i].fields.len() != f.outs[j].fields.len() || a == b {
                        continue;
                    }
                    if a.tables.len() != b.tables.len() {
                        eprintln!("[verify] {} vs {}: {} tables vs {}", i, j, a.tables.len(), b.tables.len());
                        break 'pair;
                    }
                    if a.scopes.len() != b.scopes.len() {
                        eprintln!("[verify] {} vs {}: {} scopes vs {}", i, j, a.scopes.len(), b.scopes.len());
                        break 'pair;
                    }
                    for (k, (x, y)) in a.tables.iter().zip(b.tables.iter()).enumerate() {
                        if x != y {
                            eprintln!("[verify] {} vs {}: table {} differs\n   {:?}\n   {:?}", i, j, k, x, y);
                            break 'pair;
                        }
                    }
                    for (k, (x, y)) in a.scopes.iter().zip(b.scopes.iter()).enumerate() {
                        if x != y {
                            eprintln!("[verify] {} vs {}: scope {} differs\n   {:?}\n   {:?}", i, j, k, x, y);
                            break 'pair;
                        }
                    }
                }
            }
        }

        let mut checked = 0usize;
        let mut compared = 0usize;
        let mut declined = 0usize;
        let mut declined_at: std::collections::BTreeMap<String, usize> = Default::default();
        let mut used: std::collections::BTreeSet<usize> = Default::default();
        let at: Vec<(crate::transpile::graph::Graph, Vec<NodeId>)> = (0u8..64)
            .map(|m| at_buttons(&it.d.graph, &f, &std::array::from_fn(|i| m & (1 << i) != 0)).expect("the six buttons"))
            .collect();
        for (label, over) in &perts {
            let label = label.as_str();
            let cells = cells_with(&f.iface, over).expect("overrides name input cells");
            for mask in 0u8..64 {
                let mut bits = [false; 6];
                for (i, b) in bits.iter_mut().enumerate() {
                    *b = mask & (1 << i) != 0;
                }
                let mut o = st.clone();
                for (p, c) in over {
                    let v = match c {
                        Conc::Num(n) => Value::Num(it.d.num(*n)),
                        Conc::Bool(b) => Value::Bool(it.d.boolean(*b)),
                    };
                    iface::set(&mut o, p, v).expect("override a heap slot");
                }
                set_buttons(&mut it.d, &mut o, &bits).unwrap_or_else(|e| panic!("[verify] {label} {bits:?}: {e:#}"));
                let mut o = run_one(&mut it, &frame, o).unwrap_or_else(|e| panic!("[verify] oracle {label} {bits:?} stopped at: {e:#}"));
                // The absent-as-zero fields, as every frame's outcomes write them.
                crate::trace::widen::materialize_absent_fields(&mut o, &mut it.d).expect("materialize the absent fields");
                let want = iface::read_concrete(&it.d, &o, &[]).unwrap_or_else(|e| panic!("[verify] oracle {label} {bits:?} not concrete: {e:#}"));
                match check_at(&it, &f, &at[mask as usize], &cells, &bits, &want) {
                    Ok((which, n)) => {
                        checked += 1;
                        compared += n;
                        used.insert(which);
                    }
                    // A refusal, not a wrong answer: counted.
                    Err(e) if format!("{}", e).contains("declined") => {
                        declined += 1;
                        let n = declined_at.entry(label.to_string()).or_default();
                        // The first refusal per sweep label, named.
                        if *n == 0 {
                            eprintln!("[verify] {label} {bits:?} declined: {e:#}");
                        }
                        *n += 1;
                    }
                    // A MISMATCH is a WRONG ANSWER: it must fail the test.
                    Err(e) => panic!("[verify] MISMATCH {} {:#}", label, e),
                }
            }
        }
        eprintln!(
            "[verify] {} points agree ({} declined), {} field comparisons, {}/{} outcomes claimed something",
            checked,
            declined,
            compared,
            used.len(),
            f.outs.len()
        );
        eprintln!(
            "[verify] outcome scalar counts claimed: {:?}, never claimed: {:?}",
            used.iter().map(|i| f.outs[*i].fields.len()).collect::<Vec<_>>(),
            (0..f.outs.len())
                .filter(|i| !used.contains(i))
                .map(|i| f.outs[i].fields.len())
                .collect::<Vec<_>>()
        );
        if !declined_at.is_empty() {
            eprintln!(
                "[verify] declined at {} of the {} sweep points: {:?}",
                declined_at.len(),
                perts.len(),
                declined_at.keys().collect::<Vec<_>>()
            );
        }
        assert_eq!(checked + declined, perts.len() * 64, "every point should have been checked");
        assert!(compared > 0, "nothing was actually compared");
        // Every outcome must be REACHED by the sweep, or it is a successor
        // the program does not have (or the sweep needs extending).
        assert_eq!(
            used.len(),
            f.outs.len(),
            "some outcome was never reached by the sweep"
        );
    }
}
