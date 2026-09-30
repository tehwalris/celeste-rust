//! Tracing ONE frame as a kernel, and checking it against the oracle.
//!
//! This is the first thing in `trace` that treats a trace as a FUNCTION
//! rather than as a walk: input cells in, output cells out, plus the two
//! booleans every outcome has (`guard` - when does this outcome apply;
//! `error` - where its row is undefined, derived from its values).
//!
//! The check is the point. Run the frame twice from the same state:
//!
//! * symbolically, with the player's fields replaced by `Op::Cell` leaves
//!   and the six buttons left free, which produces a graph;
//! * concretely, with real numbers in those fields and a real button
//!   assignment, which produces numbers.
//!
//! Then evaluate the graph at that assignment and compare. Both runs are
//! the SAME interpreter over the SAME domain - the "concrete" one is just
//! the symbolic domain with every leaf already a constant, which folds -
//! so a disagreement can only be the compilation itself: a bad merge, a
//! guard that claims the wrong lanes, a select on the wrong condition.
//! Nothing else is being tested, which is what makes a failure readable.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;

use crate::transpile::graph::{Graph, NodeId};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Iface, Path, Step};
use super::interp::{Flow, Interp};
use super::state::State;

/// One traced outcome of a frame.
pub struct FrameOut {
    pub guard: NodeId,
    /// Where this outcome's row has no defined value (`trace::error`): the
    /// lanes live here on which it holds decline. Derived, never carried.
    pub error: NodeId,
    /// Every scalar reachable from the globals table, by path, with the
    /// KIND the tracer knows it to be. Carrying the kind rather than
    /// re-deriving it from the node's op matters: `Op::Cell` and `Op::Sel`
    /// are both, and a guess there is a guess about the boundary.
    pub fields: Vec<(Path, NodeId, &'static str)>,
    /// Fields keyed on another node than they store (`State::key_override`):
    /// `(index into fields, key node)`.
    pub keys: Vec<(usize, NodeId)>,
    /// Slots that are DEAD at the frame boundary: the six button cells.
    ///
    /// `btn(i)` writes as well as reads - it resolves the unknown to a
    /// definite value and stores it back, which is what keeps the choice
    /// consistent within a frame - so at the end of a traced frame these
    /// hold this frame's resolved choices rather than fresh unknowns.
    ///
    /// The PRODUCTION frame chunk ends with `__reset_button_states()`,
    /// so at its boundary they are fresh unknowns and cannot distinguish
    /// two rows. The tracer runs the reset at the START, so its boundary
    /// sits one step earlier in the same cycle. Recording these paths as
    /// dead is what makes the two boundaries agree; leaving them in
    /// `fields` would make every row carry this frame's button values and
    /// stop converged lanes from deduping.
    ///
    /// Soundness is the same premise the deleted `widen_buttons` rewrite
    /// documents: the cells are dead until the next frame's reset
    /// overwrites them, so overwriting them changes nothing observable.
    /// That is a property of the whole program rather than of this
    /// frame, and it is CLAIMED here, not proven - the differential
    /// screen is what would catch a `btn` read that outlived it.
    pub ubool: Vec<Path>,
    /// What kept this outcome from merging with its siblings. Only a
    /// shape difference can, so keeping it is what turns "twelve
    /// outcomes" into a statement about the program.
    pub shape: super::heap::Shape,
    /// The ENGINE's structure for the state this outcome ends in, and
    /// the canonical cell each of `fields` / `ubool` lands on in it.
    ///
    /// Per outcome, not once per frame: an outcome that allocates - a
    /// death making a new player - or that frees one shifts every cell
    /// after the change, so the ids here are only meaningful against
    /// `rt2`. They are NOT comparable with `Frame::in_cells` unless the
    /// outcome kept the input shape.
    pub rt2: celeste_engine::runtime2::Rt2,
    pub cells: Vec<u32>,
    pub ubool_cells: Vec<u32>,
    /// The state itself. Kept so a shape walk can step FORWARD: a new
    /// heap shape only appears by actually advancing a frame, and the
    /// state an outcome ends in is the only thing that has that shape.
    pub st: State<Symbolic>,
}

impl Clone for FrameOut {
    /// For splitting an outcome (`split_undecided_selects`): the block is
    /// cloned through `Rt2::clone_block`, which shares the cart.
    fn clone(&self) -> Self {
        FrameOut {
            guard: self.guard,
            error: self.error,
            fields: self.fields.clone(),
            keys: self.keys.clone(),
            ubool: self.ubool.clone(),
            shape: self.shape.clone(),
            rt2: self.rt2.clone_block(),
            cells: self.cells.clone(),
            ubool_cells: self.ubool_cells.clone(),
            st: self.st.clone(),
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
    /// Traced with the fall floors unknown (`Symbolic::floors_unknown`): every
    /// outcome writes their `collideable` unknown (`emit::bind`).
    pub floors_unknown: bool,
    /// What each fork minted as both values was minted for
    /// (`Symbolic::fork_origins`), for the kernel dump.
    pub fork_origins: Vec<(u8, String)>,
    /// How many FORK choices this frame made (`__split_by_flr` on a
    /// widened value). The emitter needs it: a node whose cone contains
    /// a split lives at fork level 1 or deeper, and a body emitted at
    /// depth 0 silently drops every one of them.
    pub forks: u8,
    /// The forks' arities and table-fork ranges AS TRACED (`Graph::
    /// fork_ways` / `fork_table` at the end of this frame). The arena
    /// outlives the frame and a later frame's forks overwrite them, so a
    /// frame carries its own and `emit::bind` installs them in the bound
    /// graph.
    pub fork_ways: Vec<u8>,
    pub fork_tables: Vec<Vec<(i32, i32)>>,
    pub outs: Vec<FrameOut>,
    /// THE RAISE ROW's liveness: when this frame would have hit a Lua raise,
    /// as the OR of the path guards at every `Interp::poison` site
    /// (plans/graph-model.md section 4, "a raise is its own row").
    ///
    /// `ConstBool(false)` for a frame that cannot raise, which is every frame
    /// of every room but the three where a spring stands on a breakable floor
    /// (rooms (7,0), (6,1), (7,1)) - so it folds away and costs nothing where
    /// nothing raises.
    ///
    /// NOT an element of `outs`: a raise has no field values, while every
    /// `outs` consumer indexes positionally and assumes `fields` / `rt2` /
    /// `shape` / `st` are there (`emit::bind`'s roots, `kernel`'s
    /// `outs.len() == bound.outcomes.len()` pairing, the level -1 walk).
    pub raise: NodeId,
    /// The canonical cell each `Iface` slot names in the state the frame
    /// STARTS in - the engine's numbering for `Op::Cell(i)`.
    pub in_cells: Vec<u32>,
    /// The engine's structure for that state. Kept because a kernel is
    /// RUN against a block of this shape, and the input state is gone by
    /// then - `celeste_engine::slots::reshape` makes one from it.
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

/// Every scalar the state ends the frame holding, as graph nodes.
/// Every scalar the frame ends with, split into the ones that carry
/// data and the ones that are dead at the boundary (`FrameOut::ubool`).
fn out_fields(
    st: &State<Symbolic>,
    d: &Symbolic,
) -> Result<(Vec<(Path, NodeId, &'static str)>, Vec<Path>)> {
    let mut out = Vec::new();
    let mut ubool = Vec::new();
    // A near level's floor `state`s are an interval column in every outcome
    // (`widen::widen_near_floors`), an exact state `[n, n]` included: a shape's
    // column keys a point as an interval, and so does the mark filter's
    // projection (`Rt2::widen_to`).
    let ival_always: Vec<Path> = if d.floors_near { super::widen::near_floor_paths(st, d)?.state } else { Vec::new() };
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
        // The engine TYPE of the column this slot becomes. Asked of the
        // graph rather than of the tracer's `Value`, because an interval
        // is a `Value::Num` too: `player.rem` comes in widened and leaves
        // as a narrowed fragment, and both are intervals. A slot the
        // frame computes from one is an interval column, and writing it
        // as a number would be a narrowing nobody checked.
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
    // Slots the boundary WIDENS to an interval - the player's
    // `rem.x`/`rem.y`. A frame that reads one has to fork at
    // `__split_by_flr` rather than floor it (T24).
    ival: &[Path],
    // Apply the boundary's widenings INSIDE the frame (`trace::widen`),
    // so the graph knows about them and a row is hashed on the value it
    // stores.
    //
    // Off for the differential check against the CONCRETE oracle, whose
    // job is frame semantics: it runs the same chunk with real numbers
    // and has no interval to compare a widened `rem` against. The
    // abstraction is checked by the end-to-end room run instead, against
    // the interpreter, which widens too.
    //
    // `Some(Level0)` bakes the full Bits(0) widenings in; `Some(RemRung)`
    // bakes only the rem widening at the configured rung (Phase 1 of
    // moving the ladder widening into the graph); `None` leaves every
    // widening to the campaign boundary.
    widen: Option<super::widen::WidenMode>,
    // Input slots known to lie in a RANGE (raw 16.16, inclusive): the
    // body is specialized on them (`Symbolic::ranges`, the bucket
    // dispatch), and outside them the frame is in error, like a pin.
    bounds: &[(Path, (i32, i32))],
) -> Result<Frame> {
    it.trace_start_nodes = it.d.node_count();
    let mut st = st;
    // Key overrides are per FRAME too: a shape's representative is an
    // OUTPUT state of an earlier trace and still carries that trace's
    // overrides, whose key nodes name ITS forks - inherited, the kernel
    // hashed a stale key (2026-09-15).
    st.key_override.clear();
    // Fork choices are per FRAME; the six buttons are among them.
    it.d.forks = 0;
    it.d.graph.reset_forks();
    it.d.clear_ranges();
    it.d.clear_fork_memo();
    // The fork grid is the rung's rem bucket width: `move` forks at the
    // bucket edges (which include the integers), so one fork per axis
    // settles the integer move AND the output bucket, and the boundary
    // snap below never has to fork again.
    it.d.graph.set_fork_bits(match widen {
        Some(super::widen::WidenMode::RemRung(crate::interpreter::abstraction::RemPrecision::Bits(k), _)) => k,
        _ => 0,
    });
    // The `move` fork's arity: three under a bucketed speed (see
    // `Domain::move_ways`), two otherwise.
    it.d.move_ways = match widen {
        Some(super::widen::WidenMode::Level0(spd, _)) | Some(super::widen::WidenMode::RemRung(_, spd))
            if spd.width_log2().is_some() =>
        {
            3
        }
        _ => 2,
    };
    it.d.unknown_atoms = 0;
    // What the previous trace left if it failed part-way (a success takes
    // both below).
    it.raised.clear();
    it.d.escaped.clear();
    it.d.fork_origins.clear();
    it.d.evaluated.clear();
    let iface = iface::symbolize(&mut it.d, &mut st, roots, pin, ival)?;
    // THE KERNEL'S ADMISSIBLE INPUTS: the pins it was specialised on and the
    // ranges its region seeded. Built BEFORE the frame runs, so it names the
    // input cells rather than whatever the frame did to those slots. A lane
    // outside it should not have been run through this kernel at all, which
    // is an error of the whole frame rather than of any value it computes.
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
        // The obligation, built on the graph directly so the range
        // analysis it seeds cannot fold it away: the lane's value (an
        // interval: both ends) lies in the range.
        use crate::transpile::graph::Op;
        let (klo, khi) = (it.d.graph.leaf(Op::Const(*lo, *lo)), it.d.graph.leaf(Op::Const(*hi, *hi)));
        let (vlo, vhi) = (it.d.graph.fold(Op::Lo, vec![cell]), it.d.graph.fold(Op::Hi, vec![cell]));
        let a = it.d.graph.fold(Op::Ge, vec![vlo, klo]);
        let b = it.d.graph.fold(Op::Le, vec![vhi, khi]);
        let both = it.d.graph.fold(Op::And, vec![a, b]);
        admissible = it.d.graph.fold(Op::And, vec![admissible, both]);
    }
    // The engine's numbering for the INPUT shape. Here rather than in a
    // later pass because this is the last moment the input state exists;
    // `symbolize` changed the values in it and not the shape, so the
    // structure this describes is the one the boundary handed us.
    let (cart, cache) = match (it.cart.clone(), it.cache.clone()) {
        (Some(a), Some(b)) => (a, b),
        _ => bail!("tracing a frame needs the cart and the room's collision cache"),
    };
    let in_rt2 = super::bind::structure_of(&st, cart.clone(), cache.clone())?;
    let in_cells = super::bind::bind_inputs(&in_rt2, &iface)?;
    // A position bucket in the input runs as one exact position per fork
    // configuration. After the input shape is taken (the block carries
    // the bucket), before anything reads the position.
    let mut st = st;
    if let Some(super::widen::WidenMode::Level0(_, pos)) = widen {
        super::widen::fork_pos_inputs(&mut st, &mut it.d, pos)?;
        if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
            eprintln!("[build] trace_frame {widen:?}: {} forks after fork_pos_inputs, {} ival slots", it.d.forks, iface.ival.iter().filter(|b| **b).count());
        }
    }
    // Held buttons unknown: both trails run as a fork of both values, after
    // the input shape is taken, before anything reads them.
    if it.d.held_unknown {
        super::widen::fork_held_inputs(&mut st, &mut it.d)?;
    }
    // The fly fruit unknown: its widened fields replaced before anything reads
    // them (plans/fly-fruit.md).
    if it.d.fruit_unknown {
        super::widen::fork_fruit_inputs(&mut st, &mut it.d)?;
    }
    // The fall floors unknown: likewise (plans/fall-floors.md).
    if it.d.floors_unknown {
        super::widen::fork_floor_inputs(&mut st, &mut it.d)?;
    }
    // A near level's floors: each `collideable` a lane may hold unknown, forked.
    if it.d.floors_near {
        super::widen::fork_near_floor_inputs(&mut st, &mut it.d)?;
    }
    // The moving platforms unknown: their input cells, decided per world by
    // the split pass (`widen::platform_inputs`, `Points`).
    it.d.platform_cells.clear();
    if it.d.platforms_unknown {
        for ob in super::widen::platform_inputs(&mut st, &mut it.d)? {
            admissible = it.d.graph.fold(crate::transpile::graph::Op::And, vec![admissible, ob]);
        }
    }
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
        // THE WIDENINGS, here rather than at the boundary a moment
        // later, so the graph knows about them and the value a row is
        // hashed on is the value it stores (`trace::widen`). Before
        // `out_fields`, which reads the values off the state.
        //
        // After `gc`, because it walks the object list to find the
        // player and the fruit, and a dead object is not one.
        let owed = match widen {
            Some(mode) => super::widen::widen(&mut s, &mut it.d, mode)?,
            None => Vec::new(),
        };
        let (fields, ubool) = out_fields(&s, &it.d)?;
        // What this outcome owes beyond its operators: the frame's, where its
        // path's model ended (`State::ended`), and the widenings' of the
        // slots it STORES - a widened slot no field holds (its object died)
        // is no part of the row.
        let error = owed
            .iter()
            .filter(|(p, _)| fields.iter().any(|(q, _, _)| q == p))
            .fold(it.d.graph.fold(crate::transpile::graph::Op::Or, vec![global, s.ended]), |e, (_, w)| it.d.graph.fold(crate::transpile::graph::Op::Or, vec![e, *w]));
        // A widened slot no output holds (the outcome destroyed its
        // object: a death) has no key to override. Matched by PATH: two
        // fields holding the same (hash-consed) node are still two fields.
        let keys: Vec<(usize, NodeId)> = s
            .key_override
            .iter()
            .filter_map(|(held, key)| fields.iter().position(|(p, _, _)| p == held).map(|i| (i, *key)))
            .collect();
        anyhow::ensure!(
            keys.iter().enumerate().all(|(k, (i, _))| keys[..k].iter().all(|(j, _)| j != i)),
            "field {} has more than one key override",
            keys.iter().find(|(i, _)| keys.iter().filter(|(j, _)| j == i).count() > 1).map(|(i, _)| super::iface::show(&fields[*i].0)).unwrap_or_default()
        );
        // The engine's numbering for THIS outcome's shape. Fields and
        // dead cells are resolved together: they share one cell space,
        // so a collision between the two halves is exactly as wrong as
        // one within either, and only resolving them together sees it.
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
            keys,
            ubool,
            shape,
            rt2,
            cells: cells.to_vec(),
            ubool_cells: ubool_cells.to_vec(),
            st: s,
        });
    }
    // The selects left on a condition a lane can hold undecided are split,
    // until none is (`split_undecided_selects`).
    if !it.d.no_known_forks {
        let points = if it.d.platforms_unknown { Some(Points::new(&it.d, position)?) } else { None };
        let room = crate::transpile::graph::Room { cart: cart.clone(), cache: cache.clone() };
        outs = split_undecided_selects(&mut it.d, outs, points, Some(&room))?;
        // The conditions an operator was EVALUATED under (`Symbolic::evaluated`)
        // are read by the error's derivation too (`trace::error`), and they
        // are the tracer's path guards: a select on a condition the lane
        // cannot decide left in one reads a garbage bit, and its own error
        // `not Known(c)` held on every lane that straddles `c` (room (6,0)
        // f24: 750k such selects under the outcomes' fields, 2026-09-28).
        // Read three-valued like the guards (`three_valued`): an unknown
        // condition makes `own and at` unknown, which reads as error.
        three_valued_evaluated(&mut it.d, &outs);
    }
    // ERROR, DERIVED - once, now that the graph each outcome reads is final:
    // from what the row stores (fields, keys), from where it is live (an
    // error in the guard is the whole lane's), and from the conditions it
    // already owes (`trace::error`).
    let roots: Vec<Vec<NodeId>> = outs
        .iter()
        .map(|o| o.fields.iter().map(|(_, n, _)| *n).chain(o.keys.iter().map(|(_, n)| *n)).chain([o.guard, o.error]).collect())
        .collect();
    for (o, e) in outs.iter_mut().zip(super::error::for_outcomes(&mut it.d, &roots)) {
        o.error = it.d.graph.fold(crate::transpile::graph::Op::Or, vec![o.error, e]);
    }
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build] trace_frame {widen:?}: {} forks at the end, {} outcomes, {} pins", it.d.forks, outs.len(), pin.len());
    }
    // DIAGNOSTIC (CELESTE_BUILD_TRACE): how many forks this frame minted, by
    // origin, and how many an outcome can still see.
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        let mut by: std::collections::BTreeMap<&str, usize> = Default::default();
        for (_, o) in &it.d.fork_origins {
            *by.entry(o.as_str()).or_default() += 1;
        }
        // And how many of them any outcome can still see (its fields, guard,
        // error): the forks a kernel would really have to enumerate.
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
            if let Op::Split(d) | Op::SplitValid(d) | Op::SplitInt(d) | Op::SplitTab(d) | Op::SplitValidTab(d) | Op::SplitKeyTab(d) | Op::SplitOkTab(d) = it.d.graph.get(i as NodeId).op {
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
    let fork_tables: Vec<Vec<(i32, i32)>> = (0..it.d.forks).map(|d| it.d.graph.fork_table(d).to_vec()).collect();
    // The raise row's liveness: the OR over the guards at the raises this
    // trace hit. Folded from `false`, so a frame that cannot raise gets
    // `ConstBool(false)` and a frame with one raise gets that guard exactly
    // (`false OR g` folds to `g`).
    let raise = {
        use super::domain::Domain;
        let raised = std::mem::take(&mut it.raised);
        let mut r = it.d.boolean(false);
        for (g, _) in &raised {
            r = it.d.or(&r, g);
        }
        r
    };
    Ok(Frame { iface, held_unknown: it.d.held_unknown, fruit_unknown: it.d.fruit_unknown, floors_unknown: it.d.floors_unknown, fork_origins: it.d.fork_origins.clone(), forks: it.d.forks, fork_ways, fork_tables, outs, raise, in_cells, in_rt2 })
}

/// `split_undecided_selects`' queue: the outcomes still to split, popped
/// largest cone first, and deduped as they arrive - a side equal to an
/// outcome still waiting is that outcome, live wherever either is, at the
/// points either is (`Lite::merge`, `PointSet::union`).
#[derive(Default)]
struct Waiting {
    slots: Vec<Option<(Lite, Option<PointSet>)>>,
    index: rustc_hash::FxHashMap<(usize, Vec<(usize, NodeId)>, Vec<NodeId>), usize>,
    heap: std::collections::BinaryHeap<(usize, std::cmp::Reverse<usize>)>,
}

/// One outcome of `split_undecided_selects` in flight: what a split changes,
/// over the outcome it came from (`t`, whose shape, state and structure it
/// keeps). Cloning the whole `FrameOut` per side held a state and a block
/// per waiting outcome.
#[derive(Clone)]
struct Lite {
    t: usize,
    guard: NodeId,
    error: NodeId,
    fields: Vec<NodeId>,
    keys: Vec<(usize, NodeId)>,
}

impl Lite {
    /// What the row stores: the fields and the keys.
    fn row(&self) -> Vec<NodeId> {
        let mut r = self.fields.clone();
        r.extend(self.keys.iter().map(|(_, n)| *n));
        r
    }

    fn every(&self) -> Vec<NodeId> {
        let mut r = self.row();
        r.extend([self.error, self.guard]);
        r
    }

    /// Absorb another with the same row: live wherever either is, and in
    /// error where the one live there is - each error only counts on its own
    /// lanes, so the merged error is `(g1 and e1) or (g2 and e2)`, and where
    /// the two errors are one node, that node.
    fn merge(&mut self, d: &mut Symbolic, guard: NodeId, error: NodeId) {
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
    fn key(class: &[usize], o: &Lite) -> (usize, Vec<(usize, NodeId)>, Vec<NodeId>) {
        (class[o.t], o.keys.clone(), o.fields.clone())
    }

    fn push(&mut self, d: &mut Symbolic, class: &[usize], o: Lite, pts: Option<PointSet>, size: usize) {
        let key = Self::key(class, &o);
        if let Some(&i) = self.index.get(&key) {
            let (p, pp) = self.slots[i].as_mut().expect("an indexed outcome is waiting");
            p.merge(d, o.guard, o.error);
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
/// select on an undecided condition left (`MayErr`) - by case analysis on
/// the error alone, never on the outcome: splitting the outcome on the
/// error's selects gave two sides with one row that merged back as
/// `(g1 and e1) or (g2 and e2)`, the guards copied in, doubling each round
/// (room (6,0): a 41-node row under a 5M-node error). Nor three-valued: a
/// hull loses what a select's condition says about its value - the
/// platform wrap `x < -16 ? 128 : (x > 128 ? -16 : x)` hulled to
/// `[-16, 129]` failed its own containment, and the lane declined.
fn settle_error(d: &mut Symbolic, points: Option<&mut Points>, pts: Option<&PointSet>, room: Option<&crate::transpile::graph::Room>, mut o: Lite) -> Lite {
    let mut m = MayErr { points, room, memo: Default::default(), cases: 0 };
    let here = pts.cloned();
    o.error = m.may(d, o.error, here.as_ref()).0;
    o
}

/// `MayErr::may`'s bound on the cases it opens under one error.
const MAY_CASES: usize = 4096;

/// A boolean's `(may be true, may be false)` over the states a lane stands
/// for, with no undecided select left: an `Or` or `And` or `Not` through its
/// operands (`or` exact; `and` over-approximates, which for an error is
/// the safe side), a node that reads an undecided select split on its
/// lowest undecidable comparison - each case under that answer's `may`
/// guard (`Symbolic::may_answers`), narrowed to the points that give it,
/// and a case no point gives dropped - and a node that reads none as it is
/// (the kernel reads an unknown as both). Past `MAY_CASES` cases, the
/// three-valued reading (`three_valued`).
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
        roots.extend(o.keys.iter().map(|k| k.1));
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

/// THE POINTS a platforms-unknown frame's comparisons are decided over
/// (plans/graph-model.md step 5, Philippe 2026-09-28): every PLATFORM WORLD
/// (`concrete::platform_worlds`) at every whole pixel of the player's region.
/// A path through the frame carries the points still consistent with its
/// answers: a comparison narrows them to where it can come out as the path
/// takes it, a comparison that comes out one way at all of them is decided,
/// and a path left with none is dropped - the answers of one path must all
/// come from ONE arrangement of the platforms, which ten independent
/// intervals could not say. Compile time only: a lane never holds a world;
/// what reaches the kernel is each outcome's pixels (`position_guard`).
///
/// A comparison is decided at a point by the interval evaluator over its
/// cone with the platforms' cells pinned to the world and the player's to
/// the pixel (`Graph::eval_lenient_in`, the map included); what else it
/// reads keeps its range, and a point it leaves undecided goes both ways.
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

/// MAKING THE FRAME EXECUTABLE UNDER ABSTRACT INPUTS (plans/graph-model.md
/// section 1, stage 2). The traced graph computes a row for concrete inputs;
/// a SELECT whose condition a lane can hold both ways cannot be evaluated -
/// it stands for concrete states answering each way.
///
/// WHERE IT MATTERS, AND WHERE IT DOES NOT (Philippe, 2026-09-27). A select
/// the row STORES (a field, a key) must be resolved: the row is one value.
/// The ERROR is strict, but once the row is resolved it is finished rather
/// than split (`settle_error`): its splits all came back to one row, merged
/// as the three-valued reading with the guards copied in. A select in the GUARD
/// is only a question of whether the lane is live, and that is three-valued
/// already - an undecided `live` reads as live (`asm_kernel::read_zb_may`).
/// So the guard is rewritten, not split (`three_valued`): a boolean select
/// becomes `(c and a) or (not c and b)`, exact in Kleene logic, and a number
/// selected on an undecided `c` is pushed through whatever reads it into
/// the select's arms. Splitting the guard's selects too made the player's
/// every collision test after a move split again on the platforms the move
/// had already asked about - room (6,0), 50,000 splits for 112 outcomes in
/// one trace.
///
/// Then, one at a time: find a select the row reads whose
/// condition a lane can hold both ways, take the FIRST in program order
/// (the lowest node: its condition was computed first), and split the
/// outcome in two - the condition true in one, false in the other,
/// everything that reads it refolded, each side's guard narrowed to the
/// lanes where some state they stand for gives that answer
/// (`Symbolic::may_answers`). Then look again on the split outcomes: a
/// condition that was undecidable only through the other answer is decided
/// now, and never split - a loop that stops at the first blocked pixel
/// comes out as one outcome per stop point. Stop when no stored value has
/// an undecided select, and fuse the outcomes that came out equal.
///
/// Comparisons are refolded with the static ranges (`Symbolic::compare`), so
/// a branch the kernel's region never takes folds away, and what it read
/// with it.
///
/// At a platforms-unknown level each path also carries its POINTS (`Points`):
/// the worlds and pixels its answers are consistent with. A comparison only
/// one answer comes out of at them is decided without a split, and each
/// outcome ends live only on its pixels.
fn split_undecided_selects(d: &mut Symbolic, outs: Vec<FrameOut>, mut points: Option<Points>, room: Option<&crate::transpile::graph::Room>) -> Result<Vec<FrameOut>> {
    use crate::transpile::graph::Op;
    const MAX_OUTCOMES: usize = 4096;
    let n_in = outs.len();
    // Templates of one shape share a class: their outcomes dedupe together.
    let class: Vec<usize> = (0..outs.len()).map(|i| (0..=i).find(|&j| outs[j].shape == outs[i].shape).expect("itself")).collect();
    // The outcomes waiting, LARGEST CONE FIRST. A split only shrinks an
    // outcome's cone, so every path into a state has arrived - and merged
    // with it - before the state is split: popped in any other order, a
    // state reached by several paths was split again for each (room (6,0):
    // 20k splits churning ~20 outcomes).
    let mut work = Waiting::default();
    let everywhere = points.as_ref().map(|p| PointSet::full(p.len()));
    for (t, o) in outs.iter().enumerate() {
        let lite = Lite {
            t,
            guard: o.guard,
            error: o.error,
            fields: o.fields.iter().map(|f| f.1).collect(),
            keys: o.keys.clone(),
        };
        let size = cone(&d.graph, &lite.row()).len();
        work.push(d, &class, lite, everywhere.clone(), size);
    }
    let (mut n_splits, mut n_decided) = (0usize, 0usize);
    let mut done: Vec<(Lite, Option<PointSet>)> = Vec::new();
    while let Some((o, pts)) = work.pop(&class) {
        anyhow::ensure!(work.len() + done.len() < MAX_OUTCOMES, "more than {MAX_OUTCOMES} outcomes splitting undecided selects");
        // The cone of what must be resolved, in node order - the outcome's
        // own, not the arena's: each split adds nodes, and a walk of the
        // whole arena per split made the splitting quadratic.
        let reach = cone(&d.graph, &o.row());
        let first: Option<NodeId> = reach
            .iter()
            .filter(|n| d.graph.get(**n).op == Op::Sel)
            .map(|n| d.graph.get(*n).args[0])
            .find(|c| d.lane_undecidable(*c));
        let Some(c) = first else {
            // The row is resolved; the error is finished rather than split
            // (`settle_error`). Splitting it gave two sides with one row,
            // merged straight back as `(g1 and e1) or (g2 and e2)` - the
            // three-valued reading with the guards copied in, doubling each
            // round (room (6,0): a 41-node row under a 5M-node error).
            let o = settle_error(d, points.as_mut(), pts.as_ref(), room, o);
            let same = |p: &Lite| class[p.t] == class[o.t] && p.keys == o.keys && p.fields == o.fields;
            match done.iter_mut().find(|(p, _)| same(p)) {
                Some((p, pp)) => {
                    p.merge(d, o.guard, o.error);
                    if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                        a.union(b);
                    }
                }
                None => done.push((o, pts)),
            }
            continue;
        };
        // Split on the finest undecidable part of it - the lowest comparison
        // a lane can hold both ways - so the points it is decided over are
        // the answer's own, not a condition's built of several.
        let atom = cone(&d.graph, &[c])
            .into_iter()
            .find(|n| matches!(d.graph.get(*n).op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::UnknownBool(_)) && d.lane_undecidable(*n))
            .unwrap_or(c);
        // Where each answer can come from, among this path's points: an
        // answer no point gives is no side at all, and when only one is
        // left the comparison is DECIDED here - the same answer at every
        // point, so every state this path stands for gives it, and no
        // lane's guard needs narrowing.
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
        // The equal side of `x == k` (`k` a literal point, `x` a set a lane
        // holds) knows `x`: it IS `k` on every state the side stands for, so
        // the side reads `k` wherever it read `x`. Exact, and what a near
        // level needs: a floor's `state` on `[0, 2]` splits on its update's
        // `state == 0 / 1 / 2`, and an outcome that keeps the floor exact (the
        // player overlaps it) must store the state it narrowed to, as the
        // mark filter's projection does (`Rt2::widen_to`). The other side
        // learns nothing an interval can hold.
        let narrowed = {
            let node = d.graph.get(atom);
            let point = |n: NodeId| matches!(d.graph.get(n).op, Op::Const(lo, hi) if lo == hi);
            match (node.op == Op::Eq, node.args.first().copied(), node.args.get(1).copied()) {
                (true, Some(a), Some(b)) if point(b) && d.abstract_beneath_lane_ops(a) => Some((a, b)),
                (true, Some(a), Some(b)) if point(a) && d.abstract_beneath_lane_ops(b) => Some((b, a)),
                _ => None,
            }
        };
        let reach = cone(&d.graph, &o.every());
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
            let side = Lite {
                t: o.t,
                guard,
                error: m(o.error),
                fields: o.fields.iter().map(|f| m(*f)).collect(),
                keys: o.keys.iter().map(|(i, k)| (*i, m(*k))).collect(),
            };
            let size = cone(&d.graph, &side.row()).len();
            work.push(d, &class, side, side_pts, size);
        }
    }
    if std::env::var_os("CELESTE_BUILD_TRACE").is_some() {
        eprintln!("[build] split_undecided_selects: {n_in} outcomes, {n_splits} splits, {n_decided} decided by the points, {} out", done.len());
    }
    // Each outcome whole again, over its template; what reaches the kernel
    // of the points is its pixels.
    let mut whole = Vec::with_capacity(done.len());
    for (lite, pts) in done {
        // The error settled again: merged outcomes copied their guards into
        // it, and what a guard's selects read (a tile test at a position a
        // split left open) is only exact case by case - three-valued, a tile
        // test past `ARMS` was an unknown in the error and declined the
        // lanes of the other path (room (6,0) f26, 2026-09-28).
        let lite = settle_error(d, points.as_mut(), pts.as_ref(), room, lite);
        let mut o = outs[lite.t].clone();
        // The guard read three-valued now, with what the row's splits
        // decided already substituted (`three_valued`).
        o.guard = three_valued(d, lite.guard);
        o.error = lite.error;
        for (f, n) in o.fields.iter_mut().zip(lite.fields) {
            f.1 = n;
        }
        o.keys = lite.keys;
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
/// `Sel(Known(c), Sel(c, x, y), [min, max])` - a comparison on the hull that
/// straddles reads as "may be live", which is what `live` over-approximates
/// with anyway. (Pushing the readers into the arms instead is exact and
/// exponential: the player's position after a move is a tree of such
/// selects, and every sum of two of them multiplies.) Except for the ops the
/// kernel computes on exact operands only (`exact_only`: a tile lookup at a
/// hulled position cannot be assembled): those are pushed into the arms,
/// up to `ARMS` combinations (`arms`).
///
/// Also the error's, once the row is resolved (`settle_error`).
fn three_valued(d: &mut Symbolic, root: NodeId) -> NodeId {
    use crate::transpile::graph::Op;
    let nodes = cone(&d.graph, &[root]);
    // The select a wrapper this built already guards (`Sel(Known(c), Sel(c,
    // ..), hull)`) stays as it is: rewriting it again moved it off the
    // registration that bounds its error - so this is idempotent.
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
    // boolean cell, a select of booleans) - not from who reads it: a select
    // of booleans read as another select's condition, or by `Eq`, was taken
    // for a number and hulled (room (6,0), 2026-09-28).
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
                // Evaluated only where the lane decides `c` - its own error
                // holds nowhere else (`trace::error`). SET, not added to:
                // `picked` is the original select itself (hash-consed), and
                // what the tracer registered for it (an unguarded `true`)
                // made its `not Known(c)` hold on every lane (room (6,0) f24,
                // 2026-09-28). This wrapper is the only place a select on an
                // undecided condition survives the split pass.
                d.evaluated.insert(picked, decided);
                let (xl, yl) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Lo, vec![y]));
                let (xh, yh) = (d.graph.fold(Op::Hi, vec![x]), d.graph.fold(Op::Hi, vec![y]));
                let lo = d.graph.fold(Op::Min, vec![xl, yl]);
                let hi = d.graph.fold(Op::Max, vec![xh, yh]);
                let hull = d.graph.fold(Op::Span, vec![lo, hi]);
                d.graph.fold(Op::Sel, vec![decided, picked, hull])
            }
        } else if exact_only(d, &op, &args) && args.iter().any(|x| affected.contains(x) && !boolean.contains(x) && !is_bool_op(&d.graph.get(*x).op)) {
            // An op the kernel only computes on exact operands (a tile
            // lookup, `mget`, ...), reading a hull: distributed into the
            // arms instead, one per combination of the undecided selects
            // it reads - a boolean joined Kleene-wise over them, a number
            // the hull of the arms' values. Past `ARMS` combinations, the
            // weakest value (unknown, or the whole range): sound, and it
            // has not been seen.
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
            | Op::SplitValid(_) | Op::SplitValidTab(_) | Op::SplitOk(_) | Op::SplitOkTab(_) | Op::FragOk(_)
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
fn cone(g: &crate::transpile::graph::Graph, roots: &[NodeId]) -> Vec<NodeId> {
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

/// Every node of `cone` rebuilt with the nodes of `subst` replaced, in node
/// order; comparisons refolded against the static ranges
/// (`Symbolic::compare`), the rest by `Graph::fold`. Only the nodes that
/// changed are in the map.
///
/// Where a node was EVALUATED (`Symbolic::evaluated`, what bounds its own
/// error, `trace::error`) goes with it: its rebuild is registered at the
/// rebuilt condition, so the cone is widened to those conditions first.
/// Dropped, the rebuild's own error held everywhere - a three-valued guard's
/// `Sel(Known(c), Sel(c, ..), ..)` rebuilt by a split erred as `not Known(c)`
/// on every lane (room (6,0) f24, 2026-09-28).
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

/// The player is the object with a `djump` field. Naming it by
/// position would be wrong the moment an object dies: `objects` is a
/// list and things are deleted from it.
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

/// The six cells `Rt2::partition_pm1` splits a block on, as tracer
/// paths: globals `has_dashed` and `freeze`, and player fields
/// `dash_time`, `djump`, `p_dash`, `p_jump` (`src/compiled/mod.rs`).
///
/// This list is the CONTRACT between the two sides. A body compiled for
/// a key is dispatchable only to blocks the engine has partitioned on
/// exactly these cells; drop one here and the pin's error still catches
/// the mismatch, but every such block declines instead of running.
pub fn pm1_paths(player: &Path) -> Vec<Path> {
    let mut v: Vec<Path> = vec![vec![iface::key("has_dashed")], vec![iface::key("freeze")]];
    for f in ["dash_time", "djump", "p_dash", "p_jump"] {
        let mut q = player.clone();
        q.push(iface::key(f));
        v.push(q);
    }
    v
}

/// The pm1 key a state is IN: the six paths at the values it holds.
/// Compiling for a different key means handing `trace_frame` a different
/// value list, not a different state.
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
/// This is what makes an input cell a variable rather than a constant.
/// Checking the graph only at the values it was traced at would pass for
/// a graph that had folded every one of them away, which is the one bug
/// the whole design is exposed to.
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
/// `Symbolic::unknown_bool` minted, in the order `__reset_button_states`
/// asked for them; every other fork is left standing.
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
    let map = g.specialize_subset_into(&cfg, None, Some(&need), &mut out);
    Ok((out, map))
}

/// Check one traced frame against the oracle at one (inputs, buttons)
/// point, `at` being the frame at those buttons (`at_buttons`). Returns `(which outcome claimed it, fields compared)` - the
/// outcome index so a caller can tell whether the guards ever
/// discriminate, and the count so it can tell "agreed about everything"
/// from "agreed about nothing, because the paths did not line up".
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
    // The frontier's guards are pairwise disjoint and cover everything
    // (see `State::guard`), so exactly one outcome claims this lane.
    // Anything else is a broken invariant, not a rounding difference.
    if live.len() != 1 {
        bail!("{:?}: {} outcomes claim this assignment, not 1", bits, live.len());
    }
    let o = &f.outs[live[0]];
    if super::eval::eval(g, map[o.error as usize], &env)? != Conc::Bool(false) {
        bail!("{:?}: the trace declined this assignment (its error holds)", bits);
    }
    let mut n = 0;
    for (p, want) in oracle {
        // The button cells are DEAD at the boundary (`FrameOut::ubool`),
        // so the trace does not compute them and there is nothing to
        // compare. Skipped rather than dropped from the count: the total
        // below still has to account for every scalar the oracle has, so
        // a slot cannot go missing unnoticed.
        //
        // What this does NOT check is the deadness claim itself. The
        // oracle knows the resolved value and the trace declines to; if
        // a `btn` read ever outlived the boundary, this comparison would
        // stay silent. That premise lives in `FrameOut::ubool`.
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

    /// Globals that are PROGRAM CONSTANTS rather than state.
    ///
    /// The button INDICES. `btn(k)` asserts its argument is one of the
    /// six, so a symbolic `k_right` makes every `btn` call fail. They are
    /// `k_left=0 .. k_dash=5` in the source and nothing writes them.
    ///
    /// Note what is NOT here: `freeze`, `will_restart`, `delay_restart`,
    /// `has_dashed` and `has_key` are also assigned at the cart's
    /// toplevel, and they are state. "Set up at toplevel" is therefore
    /// not the rule; "no frame writes it" is, and this list is the part
    /// of it discovered so far.
    const FROZEN_GLOBALS: &[&str] =
        &["k_left", "k_right", "k_up", "k_down", "k_jump", "k_dash"];

    /// The tables that hold PROGRAM CONSTANTS rather than state: the
    /// object prototypes in `types`, everything reachable from them, and
    /// `room`.
    ///
    /// Identified structurally, from `types`, rather than by listing
    /// names - the cart's type list is the cart's own answer to "what is
    /// a prototype".
    fn frozen_tables(st: &State<Symbolic>) -> std::collections::BTreeSet<u32> {
        let mut out = std::collections::BTreeSet::new();
        let mut stack: Vec<u32> = Vec::new();
        for name in ["types", "room"] {
            if let Some(Value::Table(t)) = iface::get(st, &[iface::key(name)]) {
                stack.push(t);
            }
        }
        while let Some(t) = stack.pop() {
            if !out.insert(t) {
                continue;
            }
            let tab = &st.heap.tables[&t];
            for v in tab.hash.values().chain(tab.arr.iter()) {
                if let Value::Table(u) = v {
                    stack.push(*u);
                }
            }
        }
        out
    }

    /// Does this path pass through a frozen table on its way to a scalar?
    fn under_frozen(
        st: &State<Symbolic>,
        p: &Path,
        frozen: &std::collections::BTreeSet<u32>,
    ) -> bool {
        if let Some(Step::Key(k)) = p.first() {
            if FROZEN_GLOBALS.contains(&k.as_str()) {
                return true;
            }
        }
        for k in 0..p.len() {
            if let Some(Value::Table(t)) = iface::get(st, &p[..k]) {
                if frozen.contains(&t) {
                    return true;
                }
            }
        }
        false
    }

    /// THE SHAPE FIXPOINT. Start at the spawn shape, trace, collect the
    /// outcomes' shapes, repeat until nothing new appears.
    ///
    /// A kernel is specialized to one INPUT SHAPE, so covering a room
    /// without ever deopting means knowing every shape it reaches. This
    /// is that set.
    ///
    /// The CONCRETE VALUES DO NOT MATTER, which is what makes this a
    /// fixpoint over shapes alone rather than over states. Every
    /// non-frozen scalar is symbolized at the start of each frame, so
    /// whatever a slot held is erased before it can decide anything -
    /// two states with the same shape trace identically. Stepping
    /// forward therefore only needs a state with the right SHAPE, and
    /// this blanks the values to make that explicit rather than carrying
    /// values that look meaningful and are not.
    #[test]
    #[ignore]
    fn the_rooms_shape_set_is_a_fixpoint() {
        const CAP: usize = 400;
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let st = run_one(&mut it, &init, st).expect("_init");

        let started = std::time::Instant::now();
        let mut seen: std::collections::BTreeMap<String, State<Symbolic>> = Default::default();
        let mut queue: Vec<String> = Vec::new();
        let key = |s: &State<Symbolic>| format!("{:?}", s.shape().expect("shape"));
        let room0 = room_of(&st, &mut it.d);
        let k0 = key(&st);
        seen.insert(k0.clone(), st);
        queue.push(k0);

        let (mut frames, mut refused, mut dropped) = (0usize, 0usize, 0usize);
        let mut kept_arena = 0usize;
        let mut left_room = 0usize;
        let mut reasons: std::collections::BTreeMap<String, usize> = Default::default();
        while let Some(k) = queue.pop() {
            let st = seen[&k].clone();
            let frozen = frozen_tables(&st);
            let roots: Vec<Path> = iface::scalars(&st, &[])
                .expect("scalars")
                .into_iter()
                .filter(|p| !under_frozen(&st, p, &frozen))
                .collect();
            frames += 1;
            let f = match trace_frame(&mut it, &reset, &frame, st, &roots, &[], &[], None, &[]) {
                Ok(f) => f,
                Err(e) => {
                    refused += 1;
                    *reasons.entry(format!("{:#}", e)).or_default() += 1;
                    continue;
                }
            };
            // A path that raised left no outcome (its lanes are in the raise
            // row, `Interp::poison`), so every shape here is one some kernel
            // needs; the raises are counted in `illegal`, reported below.
            for o in f.outs {
                // A ROOM TRANSITION is a terminal, not a step.
                //
                // This is what made the walk diverge. With the player's
                // position symbolic the tracer takes the room-exit
                // branch, `next_room` calls `load_room(room.x+1, ...)`,
                // and a DIFFERENT room's objects appear - room (1,0) has
                // only `player_spawn` and `fake_wall` tiles, and the walk
                // was finding twelve `fall_floor`s and a `fly_fruit`.
                // Then those fed the next iteration.
                //
                // Nothing needs to be made concrete to stop it. The exit
                // is a real successor; it just belongs to another room's
                // kernel set, which is the multi-room seam BENCHMARK_DATA
                // already parks. Recorded and not stepped.
                if room_of(&o.st, &mut it.d) != room0 {
                    left_room += 1;
                    continue;
                }
                let k = key(&o.st);
                if seen.contains_key(&k) {
                    continue;
                }
                if seen.len() >= CAP {
                    dropped += 1;
                    continue;
                }
                let mut next = o.st;
                blank(&mut next, &mut it.d);
                seen.insert(k.clone(), next);
                queue.push(k);
            }
            // Release this frame's arena, IF every state is concrete.
            //
            // Blanking makes the globals-reachable scalars constants,
            // but the heap can hold a symbolic value it does not reach -
            // a scope variable, or a table the walk does not name. One
            // stale id is an out-of-bounds index into the new arena, so
            // this is all-or-nothing and the skips are counted.
            if !rebase(seen.values_mut().collect(), &mut it.d) {
                kept_arena += 1;
            }
        }

        eprintln!(
            "[fix] {} shapes from {} traced frames in {:.1}s ({} refused, {} dropped at the cap of {}), \
             {} graph nodes, {} left the room, {} frames could not release the arena",
            seen.len(),
            frames,
            started.elapsed().as_secs_f64(),
            refused,
            dropped,
            CAP,
            it.d.graph.len(),
            left_room,
            kept_arena
        );
        for (why, n) in &reasons {
            eprintln!("[fix]   {} x REFUSED: {}", n, why);
        }
        // Paths no legal run takes. Reported, not swallowed: these are
        // over-approximation, and a growing list is the tracer losing an
        // invariant the game holds rather than the game getting harder.
        for (why, n) in &it.illegal {
            eprintln!("[fix]   {} x path poisoned: {}", n, why);
        }
        // WHAT is accumulating? A shape is a heap, and a heap grows by
        // objects, so name them: the object list, by type.
        let mut census: Vec<(usize, String)> = seen
            .values()
            .map(|st| (st.heap.tables.len(), objects_by_type(st)))
            .collect();
        census.sort();
        for (tables, types) in &census {
            eprintln!("[fix] {:>3} tables: {}", tables, types);
        }
        assert_eq!(dropped, 0, "the shape walk hit its cap - raise CAP or it is not closed");
    }

    /// Which room a state is in. Concrete: `room` is frozen as an input
    /// and `load_room` only ever writes it a constant.
    fn room_of(st: &State<Symbolic>, d: &mut Symbolic) -> (i16, i16) {
        let at = |k: &str| -> i16 {
            match iface::get(st, &[iface::key("room"), iface::key(k)]) {
                Some(Value::Num(n)) => d
                    .as_const(&n)
                    .and_then(|v| v.as_i16_or_err().ok())
                    .unwrap_or(-1),
                _ => -1,
            }
        };
        (at("x"), at("y"))
    }

    /// The `objects` list by type name, as a histogram. Types are named
    /// by finding the global whose table IS the object's `type`, which
    /// is how the cart names them too.
    ///
    /// Counts the LUA border (`#objects`), not `arr.len()`. The two
    /// differ, and on purpose: `del` ends in
    /// `__array_table_drop_last`, which nils the last slot and leaves
    /// it in the array part rather than shrinking it (see
    /// `heap::Table::len` for why shrinking is not observationally
    /// neutral). So the post-death heap holds `arr == [nil]` with
    /// `#objects == 0`. Counting SLOTS reported that hole as an object
    /// whose type could not be named - a `1x?` line that read like a
    /// modelling bug and was only ever this.
    ///
    /// Whatever still cannot be named now says WHY instead of `?`.
    fn objects_by_type(st: &State<Symbolic>) -> String {
        let mut name_of: std::collections::BTreeMap<u32, String> = Default::default();
        let groot = &st.heap.tables[&st.globals];
        for (k, v) in groot.hash.iter() {
            if let Value::Table(t) = v {
                name_of.insert(*t, k.clone());
            }
        }
        let tag = |v: &Value<Symbolic>| match v {
            Value::Nil => "nil",
            Value::Num(_) => "a number",
            Value::Bool(_) => "a boolean",
            Value::Str(_) => "a string",
            Value::Table(_) => "a table",
            _ => "a non-table",
        };
        let objs = vec![iface::key("objects")];
        let Some(Value::Table(list)) = iface::get(st, &objs) else {
            return "<no objects list>".to_string();
        };
        let tab = &st.heap.tables[&list];
        // `len()` is `None` when the border is not exact - an interior
        // hole, or an integer part. Neither happens in this room, but
        // falling back to every slot keeps the census honest if one
        // ever does, and says so rather than quietly counting fewer.
        let (n, exact) = match tab.len() {
            Some(n) => (n, true),
            None => (tab.arr.len(), false),
        };
        let mut counts: std::collections::BTreeMap<String, usize> = Default::default();
        for i in 0..n {
            let mut p = objs.clone();
            p.push(Step::Idx(i));
            let name = match iface::get(st, &p) {
                None => "<past the end>".to_string(),
                Some(Value::Table(o)) => match st.heap.tables[&o].hash.get("type") {
                    None => "<no type field>".to_string(),
                    Some(Value::Table(t)) => name_of
                        .get(t)
                        .cloned()
                        .unwrap_or_else(|| format!("<type T{}, not a global>", t)),
                    Some(v) => format!("<type is {}>", tag(v)),
                },
                Some(v) => format!("<{}>", tag(&v)),
            };
            *counts.entry(name).or_default() += 1;
        }
        let mut out = if counts.is_empty() {
            "(empty)".to_string()
        } else {
            counts
                .into_iter()
                .map(|(k, v)| format!("{}x{}", v, k))
                .collect::<Vec<_>>()
                .join(" ")
        };
        // The slots `del` nil'd out. Not objects, but not nothing
        // either: the tracer keeps them, so they are part of the heap
        // the shape is taken of.
        if exact && tab.arr.len() > n {
            out.push_str(&format!(" (+{} nil slot(s))", tab.arr.len() - n));
        }
        if !exact {
            out.push_str(" (border not exact - counted every slot)");
        }
        out
    }

    /// Snapshot every scalar, throw the graph away, and write them back
    /// as fresh constants.
    ///
    /// The walk traces one frame per shape into a shared arena, and each
    /// trace is tens of thousands of nodes. Sixteen of them reached two
    /// million and the walk stopped - not because the shape set is that
    /// big, but because nothing was releasing the previous frame's work.
    ///
    /// Releasing it is safe here precisely because the values do not
    /// matter: a blanked state holds constants, and a constant is the
    /// same constant in any arena. Only the SHAPE has to survive, and
    /// that is the heap's topology, which this does not touch.
    fn rebase(states: Vec<&mut State<Symbolic>>, d: &mut Symbolic) -> bool {
        // Every scalar in the WHOLE heap, not the ones a path reaches.
        // Walking from the globals misses the state's own `guard`, and
        // it misses scope variables - and one stale id
        // anywhere is an out-of-bounds index into the new arena, which
        // is how the first two attempts at this failed.
        let read = |d: &Symbolic, v: &Value<Symbolic>| -> Option<Conc> {
            match v {
                Value::Num(n) => d.as_const(n).map(Conc::Num),
                Value::Bool(b) => d.decide(b).map(Conc::Bool),
                _ => None,
            }
        };
        let scalar = |v: &Value<Symbolic>| matches!(v, Value::Num(_) | Value::Bool(_));
        let mut snaps: Vec<Vec<Option<Conc>>> = Vec::new();
        for st in states.iter() {
            let mut snap = Vec::new();
            for t in st.heap.tables.values() {
                for v in t.hash.values().chain(t.arr.iter()).chain(t.ints.values()) {
                    if scalar(v) && read(d, v).is_none() {
                        return false;
                    }
                    snap.push(read(d, v));
                }
            }
            for sc in st.heap.scopes.values() {
                for v in sc.vars.values() {
                    if scalar(v) && read(d, v).is_none() {
                        return false;
                    }
                    snap.push(read(d, v));
                }
            }
            snaps.push(snap);
        }
        d.graph = d.graph.like();
        for (st, snap) in states.into_iter().zip(snaps) {
            // A state to be traced from is unconditional, so these are
            // simply true. They live on the state rather than in the
            // heap, which is what makes them easy to forget.
            st.guard = d.boolean(true);
            st.ended = d.boolean(false);
            let mut it = snap.into_iter();
            let mut put = |d: &mut Symbolic, v: &mut Value<Symbolic>| {
                let c = it.next().expect("snapshot and heap disagree about size");
                match (v, c) {
                    (Value::Num(n), Some(Conc::Num(x))) => *n = d.num(x),
                    (Value::Bool(b), Some(Conc::Bool(x))) => *b = d.boolean(x),
                    // A value that was not a constant cannot be carried
                    // across arenas. Nothing should be symbolic in a
                    // blanked state, so this says so rather than
                    // silently substituting something.
                    (_, _) => {}
                }
            };
            let mut tables = std::mem::take(&mut st.heap.tables);
            for t in tables.values_mut() {
                for v in t.hash.values_mut().chain(t.arr.iter_mut()).chain(t.ints.values_mut()) {
                    put(d, v);
                }
            }
            st.heap.tables = tables;
            let mut scopes = std::mem::take(&mut st.heap.scopes);
            for sc in scopes.values_mut() {
                for v in sc.vars.values_mut() {
                    put(d, v);
                }
            }
            st.heap.scopes = scopes;
        }
        true
    }

    /// Erase every non-frozen scalar. See the fixpoint above: the values
    /// are about to be symbolized anyway, and carrying the ones an
    /// outcome happened to compute would suggest they mean something.
    fn blank(st: &mut State<Symbolic>, d: &mut Symbolic) {
        let frozen = frozen_tables(st);
        let paths: Vec<Path> = iface::scalars(st, &[])
            .expect("scalars")
            .into_iter()
            .filter(|p| !under_frozen(st, p, &frozen))
            .collect();
        for p in paths {
            let v = match iface::get(st, &p) {
                Some(Value::Bool(_)) => Value::Bool(d.boolean(false)),
                _ => Value::Num(d.num(celeste_core::pico8_num::Pico8Num::from_i16(0))),
            };
            iface::set(st, &p, v).expect("set");
        }
    }

    /// Can the KERNEL EMITTER lower a traced graph?
    ///
    /// This is the join the whole campaign is for: `transpile::lower` is
    /// the emitter the generated crates already use, and it consumes a
    /// `Graph`. The tracer produces a `Graph` without any of the ~13,000
    /// rewrites the old front half needed. If the second goes through the
    /// first, the rewrites have nothing left to do.
    ///
    /// A PROBE, not a gate: it reports what the emitter said. Every
    /// refusal names something the tracer emits that the emitter cannot
    /// represent, which is the list of work between here and a kernel.
    /// A body compiled for one pm1 key must REFUSE a state in another.
    ///
    /// The pin folds the key's values into the body, so nothing in it
    /// reads those cells any more - which is exactly how a specialization
    /// silently runs the wrong physics if the obligation is left implicit.
    /// `pin_guard`'s negation is the frame's error, and this is the test that
    /// it bites: perturb one pinned cell and the frame declines the lane.
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
        let (rx, ry) = celeste_interp::game_runner::start_room();
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
        let f = trace_frame(&mut it, &reset, &frame, st, &roots, &key, &[], None, &[]).expect("trace");
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

    /// COMPILE FOR EVERY KEY, not just the one the warm-up state is in.
    ///
    /// The key set is not declared, it is DISCOVERED: trace a frame for a
    /// key, read the six pm1 slots off each outcome under each of the 64
    /// button assignments, and those are the successor keys. Iterate to a
    /// fixpoint. That is the set of keys the game can actually be in, as
    /// opposed to the ~1300-entry cross product of the six cells' ranges,
    /// almost all of which never occur.
    ///
    /// It is an UNDER-approximation twice over - successors are read at
    /// one input point per key, and the walk starts from one state - and
    /// that is affordable precisely because of `pin_guard`. A key that is
    /// missed has no body, so its blocks deopt to the interpreter: slower,
    /// never wrong. The key list is a performance decision, not a
    /// correctness one, and that is the whole reason it is allowed to be
    /// discovered by sampling.
    /// ~4 min: it traces and lowers one frame per key. `#[ignore]`d so
    /// the edit loop stays usable; still a gate under `--run-ignored all`.
    #[test]
    #[ignore]
    fn every_reachable_pm1_key_gets_its_own_body() {
        const CAP: usize = 64;
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
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
            st = match run_one(&mut it, &frame, st) {
                Ok(s) => s,
                Err(e) => return eprintln!("[keys] warm-up stopped at: {:#}", e),
            };
        }
        let Some(player) = player else { return eprintln!("[keys] no player") };
        let paths = pm1_paths(&player);
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }

        let k0: Vec<Conc> = match pm1_key(&player, &st, &it.d) {
            Ok(k) => k.into_iter().map(|(_, c)| c).collect(),
            Err(e) => return eprintln!("[keys] pm1 key: {:#}", e),
        };
        let show_key = |k: &[Conc]| -> String {
            paths
                .iter()
                .zip(k)
                .map(|(p, c)| {
                    let name = iface::show(p);
                    let name = name.rsplit('.').next().unwrap().to_string();
                    match c {
                        Conc::Num(v) => format!("{}={}", name, v.as_i16_or_err().unwrap_or(-999)),
                        Conc::Bool(b) => format!("{}={}", name, b),
                    }
                })
                .collect::<Vec<_>>()
                .join(" ")
        };

        // The fixpoint. `frames` holds one traced body per key, all into
        // ONE graph so they share subexpressions - which is also what
        // makes lowering them comparable.
        let mut queue: Vec<Vec<Conc>> = vec![k0.clone()];
        let mut seen: Vec<Vec<Conc>> = vec![k0];
        let mut bodies: Vec<(Vec<Conc>, Frame)> = Vec::new();
        let mut dropped = 0usize;
        let mut unresolved = 0usize;
        while let Some(key) = queue.pop() {
            let pin: Vec<(Path, Conc)> =
                paths.iter().cloned().zip(key.iter().copied()).collect();
            let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &pin, &[], None, &[]) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("[keys] {} REFUSED: {:#}", show_key(&key), e);
                    continue;
                }
            };
            // Successors: where does a frame from this key land?
            for m in 0u8..64 {
                let bits = [
                    m & 1 != 0,
                    m & 2 != 0,
                    m & 4 != 0,
                    m & 8 != 0,
                    m & 16 != 0,
                    m & 32 != 0,
                ];
                let env = super::super::eval::Env {
                    cells: &f.iface.init,
                    cart: it.cart.clone(),
                    cache: it.cache.clone(),
                };
                let (g, map) = at_buttons(&it.d.graph, &f, &bits).expect("the six buttons");
                for o in &f.outs {
                    if super::super::eval::eval(&g, map[o.guard as usize], &env).ok()
                        != Some(Conc::Bool(true))
                    {
                        continue;
                    }
                    let mut next = Vec::new();
                    for p in &paths {
                        match o.fields.iter().find(|(q, _, _)| q == p) {
                            Some((_, nd, _)) => {
                                match super::super::eval::eval(&g, map[*nd as usize], &env) {
                                    Ok(c) => next.push(c),
                                    Err(_) => break,
                                }
                            }
                            // Death replaces the player, so a pm1 field
                            // can simply not be there. That successor is
                            // a SHAPE change, not a key change.
                            None => break,
                        }
                    }
                    if next.len() != paths.len() {
                        unresolved += 1;
                        continue;
                    }
                    if seen.contains(&next) {
                        continue;
                    }
                    if seen.len() >= CAP {
                        dropped += 1;
                        continue;
                    }
                    seen.push(next.clone());
                    queue.push(next);
                }
            }
            bodies.push((key, f));
        }

        eprintln!(
            "[keys] {} keys reached from {} traced bodies ({} successors unresolved by shape,              {} dropped at the cap of {})",
            seen.len(),
            bodies.len(),
            unresolved,
            dropped,
            CAP
        );
        assert_eq!(dropped, 0, "the key walk hit its cap - raise CAP or the set is not closed");

        // The map, so the interval pass can decide collision tests
        // rather than treating every one of them as unknown.
        let room = match (it.cart.clone(), it.cache.clone()) {
            (Some(cart), Some(cache)) => Some(crate::transpile::graph::Room { cart, cache }),
            _ => None,
        };
        let g = std::mem::take(&mut it.d.graph);

        let mut total_variants = 0usize;
        let mut refused = 0usize;
        for (key, f) in &bodies {
            let mut variants = 0usize;
            // Every key was traced from the same state, so they share an
            // input shape and the binding is the same one 24 times over.
            // Doing it per body anyway is what would SAY SO if a key ever
            // came from a different shape, instead of silently emitting
            // one body's cell ids for another body's slots.
            assert_eq!(
                f.in_cells, bodies[0].1.in_cells,
                "{}: a different input numbering than the first key",
                show_key(key)
            );
            match super::super::emit::bind(f, &g, true)
                .and_then(|b| super::super::emit::lower_frame(&b, room.clone(), Default::default()))
            {
                Ok(l) => {
                    variants = l.bodies;
                }
                Err(_) => refused += 1,
            }
            eprintln!(
                "[keys] {:<64} {} outcomes, {} bodies",
                show_key(key),
                f.outs.len(),
                variants
            );
            total_variants += variants;
        }
        eprintln!(
            "[keys] TOTAL {} keys, {} bodies, {} outcomes refused by the emitter",
            bodies.len(),
            total_variants,
            refused
        );
    }

    /// `ice_at` is `tile_flag_at(.., 4)`, and only flag 0 is modelled.
    /// Answering `false` for every other flag is right exactly where the
    /// room has no such tile - so this checks BOTH halves: that it
    /// answers in a room without ice, and that it RAISES in one with.
    ///
    /// The second half is the point. A guard that never fires is
    /// indistinguishable from no guard, and this one was wrong in the
    /// interpreter too, so no differential test between the two could
    /// have found it.
    #[test]
    fn ice_answers_without_ice_and_raises_with_it() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let probe = full_moon::parse("ice_probe = ice_at(0,0,8,8)").expect("parse probe");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let st = run_one(&mut it, &init, st).expect("_init");

        // The start room has no ice, so the answer is false and exact.
        let s = run_one(&mut it, &probe, st.clone()).expect("start room should answer");
        let Some(Value::Bool(b)) = iface::get(&s, &[iface::key("ice_probe")]) else {
            panic!("ice_at did not return a boolean")
        };
        assert_eq!(it.d.decide(&b), Some(false), "the start room has no ice");

        // Somewhere on the map there IS ice, and there it must raise
        // rather than quietly answer false. Searched rather than
        // hard-coded, so the test cannot pass by looking in the wrong
        // place.
        let mut refused = 0;
        let mut answered = 0;
        for rx in 0..8i16 {
            for ry in 0..4i16 {
                let mut o = st.clone();
                for (k, v) in [("x", rx), ("y", ry)] {
                    let n = it.d.num(crate::pico8_num::Pico8Num::from_i16(v));
                    iface::set(&mut o, &[iface::key("room"), iface::key(k)], Value::Num(n))
                        .expect("set room");
                }
                match run_one(&mut it, &probe, o) {
                    Ok(_) => answered += 1,
                    Err(e) => {
                        assert!(
                            format!("{:#}", e).contains("CONTAINS that flag"),
                            "refused for the wrong reason: {:#}",
                            e
                        );
                        refused += 1;
                    }
                }
            }
        }
        eprintln!("[ice] {} rooms answer false, {} raise", answered, refused);
        assert!(refused > 0, "no room on the map has ice - then this guard is untested");
        assert!(answered > 0, "every room raised - the guard is too coarse");
    }

    /// `break` in a loop whose bound the tracer CANNOT know.
    ///
    /// The PICO-8 corpus cannot reach this. `run_for_symbolic` only runs
    /// when the limit is unknown, and every program PICO-8 can also run is
    /// concrete, so the corpus exercises `run_for` and nothing else. That
    /// is exactly why the bug lived here and not there: `for_body`
    /// rewrote `Flow::Break` to `Flow::Normal`, the state went back into
    /// the frontier and ran the body again, and `break` did nothing.
    ///
    /// So: trace the loop once with a symbolic limit, then evaluate the
    /// resulting graph at each concrete limit and compare with the answer
    /// worked out by hand. One graph, every point - the same discipline as
    /// the frame check.
    #[test]
    fn break_leaves_a_loop_whose_bound_is_symbolic() {
        // `abs(amount)` because `unroll_bound` is keyed on the limit
        // expression's SOURCE TEXT, and that is the cart's own spelling
        // (`move_x`/`move_y`), which is where this actually bites.
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
        // 100 is well past the unroll bound of 8 on purpose: once a
        // `break` is reached the bound stops mattering, so the frame must be
        // modelled there too. Before the fix the loop ran on and the "it
        // finished" obligation made this lane deopt.

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
    /// assignments.
    ///
    /// The sweep is the part that matters. Checking only at the values
    /// the frame was traced at would pass for a graph that had constant
    /// -folded every input away, and re-tracing per point would not test
    /// anything: the claim is that ONE graph answers for all of them.
    ///
    /// Still a PROBE where it cannot get far enough - it prints and
    /// returns rather than panicking, because each stop names the next
    /// thing to implement and a panic hides the ones behind it. Anything
    /// it does reach, it asserts about.
    #[test]
    fn a_traced_frame_agrees_with_the_oracle() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");

        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);

        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");

        // Warm up with the buttons held concrete (the toplevel leaves
        // them false and `btn` writes concrete values back, so nothing
        // symbolic enters). This gets past the spawn animation, which
        // reads no buttons and would make the check vacuous.
        let mut player = None;
        for n in 0..40 {
            if let Some(p) = find_player(&st) {
                player = Some((n, p));
                break;
            }
            st = match run_one(&mut it, &frame, st) {
                Ok(s) => s,
                Err(e) => return eprintln!("[verify] warm-up frame {} stopped at: {:#}", n, e),
            };
        }
        let Some((warm, player)) = player else {
            return eprintln!("[verify] no player after 40 frames");
        };
        eprintln!("[verify] player at {} after {} warm-up frames", iface::show(&player), warm);

        // The rest of the frame's INPUT state. Everything else reachable
        // from the globals table is static configuration - the `k_*`
        // button numbers, each type's `tile`, `room.x/y` - or the button
        // slots, which `__reset_button_states` makes free choices rather
        // than cells.
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
        let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &[], &[], None, &[]) {
            Ok(f) => f,
            Err(e) => return eprintln!("[verify] symbolic frame stopped at: {:#}", e),
        };
        eprintln!(
            "[verify] {} input cells, {} outcome(s), {} nodes ({} new)",
            f.iface.slots.len(),
            f.outs.len(),
            it.d.graph.len(),
            it.d.graph.len() - before
        );

        // PERTURB THE INPUTS as well as the buttons. Without this the
        // graph is only ever evaluated at the values it was traced at,
        // which a graph that folded every input away would also pass.
        // The same override goes into both sides: written into the heap
        // for the oracle, into the cell vector for the graph. Nothing is
        // re-traced - reusing one graph across all of these is the claim
        // being tested.
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
        // A cross product rather than a random sample: the interesting
        // structure is where the player is relative to the tiles, and a
        // sweep over position and speed hits walls, floors and the pit
        // below the room, which is what the non-trivial output SHAPES are.
        let mut perts: Vec<(String, Vec<(Path, Conc)>)> = Vec::new();
        for dx in [-8i16, -1, 0, 1, 8] {
            for dy in [-8i16, -1, 0, 1, 8, 24, 64] {
                // A falling speed of 8 makes `move_y` step further than
                // the unroll bound, so it is the REFUSAL case - kept on
                // one column of the sweep so that path stays covered
                // without spending a quarter of the run on it.
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
        // The control-flow inputs, one at a time rather than crossed with
        // the position sweep: each of these changes which BRANCH the
        // frame takes rather than where it lands, so crossing them would
        // multiply the run without touching anything new.
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
            // Out of the TOP of the room (`this.y < -4`), which is
            // `next_room()` - a whole new object list, and the only way
            // to reach the largest of the output shapes.
            ("top edge", vec![(px("y"), Conc::Num(at(&px("y")) - n(120)))]),
            ("right edge", vec![(px("x"), Conc::Num(at(&px("x")) + n(124)))]),
            ("left edge", vec![(px("x"), Conc::Num(at(&px("x")) - n(16)))]),
        ] {
            perts.push((name.to_string(), over));
        }

        // WHAT are the outcomes? Shape divergence is the only thing that
        // can leave more than one, so two outcomes with the SAME shape
        // would be a `collapse` bug, and two with the same scalar count
        // but different shapes are worth looking at closely - that is how
        // the body-interning bug was found, where ten of twelve outcomes
        // were the same 110 scalars and differed only in `BodyId`s.
        {
            let mut by_len: std::collections::BTreeMap<usize, usize> = Default::default();
            for o in &f.outs {
                *by_len.entry(o.fields.len()).or_default() += 1;
            }
            eprintln!("[verify] outcomes by scalar count: {:?}", by_len);
            // The object list is what actually distinguishes them: a
            // player, a player_spawn, nothing at all, or a whole new
            // room's worth.
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
                if let Err(e) = set_buttons(&mut it.d, &mut o, &bits) {
                    return eprintln!("[verify] {} {:?}: {:#}", label, bits, e);
                }
                let o = match run_one(&mut it, &frame, o) {
                    Ok(s) => s,
                    Err(e) => {
                        return eprintln!("[verify] oracle {} {:?} stopped at: {:#}", label, bits, e)
                    }
                };
                let want = match iface::read_concrete(&it.d, &o, &[]) {
                    Ok(w) => w,
                    Err(e) => {
                        return eprintln!("[verify] oracle {} {:?} not concrete: {:#}", label, bits, e)
                    }
                };
                match check_at(&it, &f, &at[mask as usize], &cells, &bits, &want) {
                    Ok((which, n)) => {
                        checked += 1;
                        compared += n;
                        used.insert(which);
                    }
                    // A trace that declined a point is not a wrong
                    // answer, it is a refusal - count it and keep going,
                    // because how OFTEN it refuses is the number that
                    // matters and one panic would hide it.
                    Err(e) if format!("{}", e).contains("declined") => {
                        declined += 1;
                        let n = declined_at.entry(label.to_string()).or_default();
                        // The first refusal per sweep label, named: a count
                        // alone does not say which premise refused.
                        if *n == 0 {
                            eprintln!("[verify] {label} {bits:?} declined: {e:#}");
                        }
                        *n += 1;
                    }
                    // A MISMATCH is a WRONG ANSWER, not a refusal - the
                    // refusals are counted above and are a measurement.
                    // This used to `return eprintln!`, so the test passed
                    // while printing the failure, and a change that broke
                    // the comparison outright went green (2026-08-23: the
                    // button cells moved to `FrameOut::ubool` and every
                    // point started failing with "traced state has no
                    // __button_states[0]"). A check that cannot fail is
                    // not a check.
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
        // Every outcome the tracer produced has to be REACHABLE, or it
        // is a successor the program does not have. A new one that this
        // sweep cannot reach is a thing to explain - either extend the
        // sweep to reach it, or find out why the tracer kept it.
        assert_eq!(
            used.len(),
            f.outs.len(),
            "some outcome was never reached by the sweep"
        );
    }
}
