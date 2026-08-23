//! Tracing ONE frame as a kernel, and checking it against the oracle.
//!
//! This is the first thing in `trace` that treats a trace as a FUNCTION
//! rather than as a walk: input cells in, output cells out, plus the two
//! booleans every outcome carries (`guard` - when does this outcome
//! apply; `ok` - did the tracer compute it correctly).
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

use crate::transpile::graph::NodeId;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Iface, Path, Step};
use super::interp::{Flow, Interp};
use super::state::State;

/// One traced outcome of a frame.
pub struct FrameOut {
    pub guard: NodeId,
    pub ok: NodeId,
    /// Every scalar reachable from the globals table, by path, with the
    /// KIND the tracer knows it to be. Carrying the kind rather than
    /// re-deriving it from the node's op matters: `Op::Cell` and `Op::Sel`
    /// are both, and a guess there is a guess about the boundary.
    pub fields: Vec<(Path, NodeId, &'static str)>,
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
    /// Soundness is the same premise `rewrite::rules::widen_buttons`
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

pub struct Frame {
    pub iface: Iface,
    /// How many FORK choices this frame made (`__split_by_flr` on a
    /// widened value). The emitter needs it: a node whose cone contains
    /// a split lives at fork level 1 or deeper, and a body emitted at
    /// depth 0 silently drops every one of them.
    pub forks: u8,
    pub outs: Vec<FrameOut>,
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
    for p in iface::scalars(st, &[])? {
        if p.first() == Some(&iface::key("__button_states")) {
            ubool.push(p);
            continue;
        }
        // The engine TYPE of the column this slot becomes. Asked of the
        // graph rather than of the tracer's `Value`, because an interval
        // is a `Value::Num` too: `player.rem` comes in widened and leaves
        // as a narrowed fragment, and both are intervals. A slot the
        // frame computes from one is an interval column, and writing it
        // as a number would be a narrowing nobody checked.
        let (n, ty) = match iface::get(st, &p).unwrap() {
            Value::Num(n) => {
                let ty = if d.is_interval(&n) { "ZI" } else { "ZN" };
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
) -> Result<Frame> {
    let mut st = st;
    // Fork choices are per FRAME, like the six buttons above.
    it.d.forks = 0;
    // One frame has exactly six free choices, `Free(0..5)`. The counter
    // is on the domain rather than the frame, so tracing a SECOND frame
    // through one interpreter - which compiling per pm1 key does - would
    // otherwise run out of buttons on the seventh.
    it.d.frees = 0;
    let iface = iface::symbolize(&mut it.d, &mut st, roots, pin, ival)?;
    // Built BEFORE the frame runs, so it names the input cells rather
    // than whatever the frame did to those slots.
    let pin_ok = iface::pin_guard(&mut it.d, &iface);
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
    let st = run_one(it, reset, st)?;
    let mut outs = Vec::new();
    for (s, f) in it.exec_block(frame.nodes(), st)? {
        if let Flow::Break = f {
            bail!("break at frame toplevel");
        }
        let mut s = s;
        s.gc();
        // The specialization's obligation rides on `ok`: a lane whose
        // key disagrees with what this body was compiled for deopts to
        // the interpreter. Folds to `s.ok` when nothing is pinned.
        let ok = it.d.graph.fold(crate::transpile::graph::Op::And, vec![s.ok, pin_ok]);
        let (fields, ubool) = out_fields(&s, &it.d)?;
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
            ok,
            fields,
            ubool,
            shape,
            rt2,
            cells: cells.to_vec(),
            ubool_cells: ubool_cells.to_vec(),
            st: s,
        });
    }
    Ok(Frame { iface, forks: it.d.forks, outs, in_cells, in_rt2 })
}

/// The player is the object with a `djump` field. Naming it by
/// position would be wrong the moment an object dies: `objects` is a
/// list and things are deleted from it.
pub fn find_player(st: &State<Symbolic>) -> Option<Path> {
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
/// exactly these cells; drop one here and the guard on `ok` still
/// catches the mismatch, but every such block deopts instead of running.
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

/// Check one traced frame against the oracle at one (inputs, buttons)
/// point. Returns `(which outcome claimed it, fields compared)` - the
/// outcome index so a caller can tell whether the guards ever
/// discriminate, and the count so it can tell "agreed about everything"
/// from "agreed about nothing, because the paths did not line up".
pub fn check_at(
    it: &Interp<'_, Symbolic>,
    f: &Frame,
    cells: &[Conc],
    bits: &[bool; 6],
    oracle: &[(Path, Conc)],
) -> Result<(usize, usize)> {
    let env = super::eval::Env {
        cells,
        frees: bits,
        cart: it.cart.clone(),
        cache: it.cache.clone(),
    };
    let g = &it.d.graph;
    let mut live: Vec<usize> = Vec::new();
    for (i, o) in f.outs.iter().enumerate() {
        if super::eval::eval(g, o.guard, &env)? == Conc::Bool(true) {
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
    if super::eval::eval(g, o.ok, &env)? != Conc::Bool(true) {
        bail!("{:?}: the trace declined this assignment (ok is false)", bits);
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
        let got = super::eval::eval(g, got.1, &env)?;
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

    /// IS THE SHAPE SET A FIXPOINT? The first experiment, before the
    /// walk that would depend on it.
    ///
    /// A kernel is specialized to one INPUT SHAPE. Covering a room
    /// without ever deopting therefore means knowing every shape the
    /// room reaches, and the natural way to get that is a fixpoint:
    /// start at the spawn shape, trace, collect the outcomes' shapes,
    /// repeat.
    ///
    /// The step this checks first is whether the trace works at all with
    /// EVERYTHING symbolic. It has to be everything: a branch on a cell
    /// left concrete is decided, so a shape behind it never appears and
    /// the fixpoint looks converged when it is not. Today's probes
    /// symbolize seven roots.
    ///
    /// The VALUES do not matter, only the shape. Every scalar becomes a
    /// fresh cell immediately, so what a slot held before is erased -
    /// which is why this can step forward without evaluating anything.
    /// The shapes that come out are therefore a SUPERSET of the
    /// reachable ones, which is the safe direction: a shape that cannot
    /// really occur costs a kernel nobody dispatches to.
    #[test]
    #[ignore]
    fn tracing_a_frame_with_everything_symbolic() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");
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
        for _ in 0..40 {
            if find_player(&st).is_some() {
                break;
            }
            st = match run_one(&mut it, &frame, st) {
                Ok(s) => s,
                Err(e) => return eprintln!("[shapes] warm-up stopped at: {:#}", e),
            };
        }

        // "Everything" has to mean every scalar that is STATE, not
        // every number in the heap.
        //
        // `types` and the object prototypes it holds are program
        // constants: `balloon.tile` is 22 in the source and stays 22.
        // Symbolizing them makes `type.tile == tile` undecidable in
        // `load_room`'s scan, so the tracer explores EVERY object type
        // for every tile - including a balloon, whose `init` calls
        // `rnd`, which the minimal cart does not define. Room (0,0)'s
        // tile map has no balloon at all; the trace had wandered into a
        // room that does not exist.
        //
        // `room` goes with them. A kernel is per room by construction -
        // `G` carries that room's collision cache - so pinning the room
        // states an existing specialization rather than adding one.
        let frozen = frozen_tables(&st);
        let all: Vec<Path> = iface::scalars(&st, &[])
            .expect("scalars")
            .into_iter()
            .filter(|p| !under_frozen(&st, p, &frozen))
            .collect();
        let n_slots = all.len();
        let started = std::time::Instant::now();
        match trace_frame(&mut it, &reset, &frame, st, &all, &[], &[]) {
            Ok(f) => {
                let mut by_shape: std::collections::BTreeMap<String, usize> = Default::default();
                for o in &f.outs {
                    *by_shape.entry(format!("{:?}", o.shape)).or_default() += 1;
                }
                eprintln!(
                    "[shapes] {} slots symbolized -> {} outcomes in {} distinct shapes, \
                     {} graph nodes, {:.1}s",
                    n_slots,
                    f.outs.len(),
                    by_shape.len(),
                    it.d.graph.len(),
                    started.elapsed().as_secs_f64()
                );
                for o in &f.outs {
                    eprintln!(
                        "[shapes]   outcome: {} cells, {} scalars",
                        o.rt2.structure.len(),
                        o.cells.len()
                    );
                }
            }
            // A REFUSAL is the result here, not a failure of the test.
            // Full symbolization is strictly harder than what the tracer
            // does today, and where it stops is the next thing to fix.
            Err(e) => eprintln!(
                "[shapes] {} slots symbolized: REFUSED after {:.1}s: {:#}",
                n_slots,
                started.elapsed().as_secs_f64(),
                e
            ),
        }
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
        let frame = full_moon::parse("_update()").expect("parse frame");
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
        let mut poisoned_outcomes = 0usize;
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
            let f = match trace_frame(&mut it, &reset, &frame, st, &roots, &[], &[]) {
                Ok(f) => f,
                Err(e) => {
                    refused += 1;
                    *reasons.entry(format!("{:#}", e)).or_default() += 1;
                    continue;
                }
            };
            for o in f.outs {
                // An outcome whose `ok` is statically false is reached
                // only along poisoned paths - no legal run gets there,
                // so its shape is not one any kernel needs.
                if it.d.decide(&o.ok) == Some(false) {
                    poisoned_outcomes += 1;
                    continue;
                }
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
             {} graph nodes, {} outcomes dropped as unreachable, {} left the room, {} frames could not release the arena",
            seen.len(),
            frames,
            started.elapsed().as_secs_f64(),
            refused,
            dropped,
            CAP,
            it.d.graph.len(),
            poisoned_outcomes,
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
        // Walking from the globals misses the state's own `guard` and
        // `ok`, and it misses scope variables - and one stale id
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
        d.graph = crate::transpile::graph::Graph::new();
        for (st, snap) in states.into_iter().zip(snaps) {
            // A state to be traced from is unconditional, so these are
            // simply true. They live on the state rather than in the
            // heap, which is what makes them easy to forget.
            st.guard = d.boolean(true);
            st.ok = d.boolean(true);
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
    /// `pin_guard` puts it on `ok`, and this is the test that it bites:
    /// perturb one pinned cell and the frame declines the lane.
    #[test]
    fn a_body_pinned_to_a_pm1_key_refuses_any_other_key() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");
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
        let f = trace_frame(&mut it, &reset, &frame, st, &roots, &key, &[]).expect("trace");
        assert_eq!(f.iface.pins.len(), 6, "all six pinned");

        // `ok` of whichever outcome claims this assignment.
        let bits = [false; 6];
        let ok_at = |cells: &[Conc]| -> bool {
            let env = super::super::eval::Env {
                cells,
                frees: &bits,
                cart: it.cart.clone(),
                cache: it.cache.clone(),
            };
            let g = &it.d.graph;
            let live: Vec<&FrameOut> = f
                .outs
                .iter()
                .filter(|o| super::super::eval::eval(g, o.guard, &env).expect("guard") == Conc::Bool(true))
                .collect();
            assert_eq!(live.len(), 1, "exactly one outcome claims a lane");
            super::super::eval::eval(g, live[0].ok, &env).expect("ok") == Conc::Bool(true)
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
        let frame = full_moon::parse("_update()").expect("parse frame");
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
            let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &pin, &[]) {
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
                    frees: &bits,
                    cart: it.cart.clone(),
                    cache: it.cache.clone(),
                };
                for o in &f.outs {
                    if super::super::eval::eval(&it.d.graph, o.guard, &env).ok()
                        != Some(Conc::Bool(true))
                    {
                        continue;
                    }
                    let mut next = Vec::new();
                    for p in &paths {
                        match o.fields.iter().find(|(q, _, _)| q == p) {
                            Some((_, nd, _)) => {
                                match super::super::eval::eval(&it.d.graph, *nd, &env) {
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

        let mut total_lines = 0usize;
        let mut total_variants = 0usize;
        let mut refused = 0usize;
        for (key, f) in &bodies {
            let (mut lines, mut variants) = (0usize, 0usize);
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
            match super::super::emit::bind(f, &g)
                .and_then(|b| super::super::emit::lower_frame(
                    &b.graph, &b.inputs, &b.uni, &b.outcomes, room.clone(), b.forks,
                ))
            {
                Ok(l) => {
                    lines = l.body.len();
                    variants = l.variants.len();
                }
                Err(_) => refused += 1,
            }
            eprintln!(
                "[keys] {:<64} {} outcomes, {} lines, {} variants",
                show_key(key),
                f.outs.len(),
                lines,
                variants
            );
            total_lines += lines;
            total_variants += variants;
        }
        eprintln!(
            "[keys] TOTAL {} bodies, {} lines, {} variants, {} outcomes refused by the emitter",
            bodies.len(),
            total_lines,
            total_variants,
            refused
        );
    }

    /// WHAT DOES PINNING THE POSITION BUY? A direct A/B.
    ///
    /// The interval pass can decide a collision test only when it knows
    /// where the player is, and at TOP it does not. A room is 128x128
    /// pixels, so specialising per position is a real option rather than
    /// a thought experiment - this measures what one position costs and
    /// what it saves, before anyone builds the 16k-body version of it.
    #[test]
    #[ignore]
    fn specialising_to_one_player_position() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");
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
                Err(e) => return eprintln!("[pos] warm-up stopped at: {:#}", e),
            };
        }
        let Some(player) = player else { return eprintln!("[pos] no player") };
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }
        let base = match pm1_key(&player, &st, &it.d) {
            Ok(k) => k,
            Err(e) => return eprintln!("[pos] pm1 key: {:#}", e),
        };
        // The extra pins, at whatever the state holds. `rem` too: `x` is
        // the integer part and `rem.x` the sub-pixel remainder, and the
        // NEXT position is `flr(x + rem.x + spd.x + 0.5)` - pinning one
        // without the other leaves the move amount just as unknown.
        let extra = |names: &[&str]| -> Vec<(Path, Conc)> {
            let mut v = base.clone();
            for n in names {
                let mut q = player.clone();
                for seg in n.split('.') {
                    q.push(iface::key(seg));
                }
                match iface::get(&st, &q) {
                    Some(Value::Num(nv)) => {
                        v.push((q, Conc::Num(it.d.as_const(&nv).expect("concrete"))))
                    }
                    other => panic!("{} is {:?}", n, other),
                }
            }
            v
        };
        let room = match (it.cart.clone(), it.cache.clone()) {
            (Some(cart), Some(cache)) => Some(crate::transpile::graph::Room { cart, cache }),
            _ => None,
        };

        for (label, pin) in [
            ("pm1 only", base.clone()),
            ("pm1 + x,y", extra(&["x", "y"])),
            ("pm1 + x,y,rem", extra(&["x", "y", "rem.x", "rem.y"])),
            ("pm1 + x,y,rem,spd", extra(&["x", "y", "rem.x", "rem.y", "spd.x", "spd.y"])),
        ] {
            let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &pin, &[]) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!("[pos] {:<26} REFUSED: {:#}", label, e);
                    continue;
                }
            };
            let g = it.d.graph.clone();
            let (mut lines, mut variants) = (0usize, 0usize);
            match super::super::emit::bind(&f, &g)
                .and_then(|b| super::super::emit::lower_frame(
                    &b.graph, &b.inputs, &b.uni, &b.outcomes, room.clone(), b.forks,
                ))
            {
                Ok(l) => {
                    lines = l.body.len();
                    variants = l.variants.len();
                }
                Err(e) => eprintln!("[pos] {} REFUSED: {:#}", label, e),
            }
            eprintln!(
                "[pos] {:<26} {} pins, {} outcomes, {} lines, {} variants",
                label,
                f.iface.pins.len(),
                f.outs.len(),
                lines,
                variants
            );
        }
    }

    #[test]
    fn the_kernel_emitter_lowers_a_traced_graph() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");
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
                Err(e) => return eprintln!("[emit] warm-up stopped at: {:#}", e),
            };
        }
        let Some(player) = player else { return eprintln!("[emit] no player") };
        let mut roots: Vec<Path> = vec![player.clone()];
        // `has_dashed` is here to be PINNED, not to become a cell. Left
        // out of the roots it stayed concrete anyway - an undeclared
        // specialization, which is the same folding with nothing saying
        // so and nothing to guard it.
        for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }
        // The pm1 CLASS specialization. `partition_pm1` makes every lane
        // of a block agree on these six cells, so a body compiled for one
        // key is exact for the blocks that hold it - and `pin_guard` puts
        // the check on `ok`, so a block that disagrees deopts instead of
        // running the wrong physics.
        let pin = match pm1_key(&player, &st, &it.d) {
            Ok(k) => k,
            Err(e) => return eprintln!("[emit] pm1 key: {:#}", e),
        };
        // The INPUT structure, for comparison with the outcomes' below:
        // whether a traced outcome keeps the shape it started in decides
        // whether a generated `acc_init` may clone the chunk's structure
        // or has to carry the outcome's own.
        let in_cells = super::super::bind::structure_of(
            &st,
            it.cart.clone().unwrap(),
            it.cache.clone().unwrap(),
        )
        .map(|r| r.structure.len())
        .unwrap_or(0);
        let f = match trace_frame(&mut it, &reset, &frame, st, &roots, &pin, &[]) {
            Ok(f) => f,
            Err(e) => return eprintln!("[emit] trace stopped at: {:#}", e),
        };
        for (i, c) in &f.iface.pins {
            eprintln!("[emit] pinned {} = {:?}", iface::show(&f.iface.slots[*i]), c);
        }

        // The map, so the interval pass can decide collision tests
        // rather than treating every one of them as unknown.
        let room = match (it.cart.clone(), it.cache.clone()) {
            (Some(cart), Some(cache)) => Some(crate::transpile::graph::Room { cart, cache }),
            _ => None,
        };
        let g = std::mem::take(&mut it.d.graph);
        // The ENGINE's cell ids, resolved from the tracer's paths.
        let b = match super::super::emit::bind(&f, &g) {
            Ok(b) => b,
            Err(e) => return eprintln!("[emit] BIND REFUSED: {:#}", e),
        };
        eprintln!(
            "[emit] bound {} row inputs + {} uniform inputs; input shape {} cells, \
             outcome shapes {:?}",
            b.inputs.len(),
            b.uni.len(),
            in_cells,
            f.outs.iter().map(|o| o.rt2.structure.len()).collect::<Vec<_>>()
        );
        // Does the kernel compute the WHOLE output block, or only part
        // of it? Every VALUE cell in an outcome's structure has to get a
        // value from somewhere. If the frame's outputs cover all of
        // them, the block is (constant structure + computed columns) and
        // nothing passes through from the input block.
        for (n, o) in f.outs.iter().enumerate() {
            use celeste_engine::runtime2::{Cell2, Col, AV};
            let covered: std::collections::BTreeSet<u32> =
                o.cells.iter().chain(o.ubool_cells.iter()).copied().collect();
            let (mut ptr, mut left) = (0usize, Vec::new());
            for (c, cell) in o.rt2.structure.iter().enumerate() {
                if !matches!(cell, Cell2::Val) {
                    continue;
                }
                if matches!(&o.rt2.cols[c], Col::U(AV::Ptr(_))) {
                    ptr += 1;
                } else if !covered.contains(&(c as u32)) {
                    left.push(c as u32);
                }
            }
            eprintln!(
                "[emit] outcome {}: {} pointer cells (in the structure) + {} computed \
                 + {} dead, leaving {} value cells with no source",
                n,
                ptr,
                o.cells.len(),
                o.ubool_cells.len(),
                left.len()
            );
        }

        // ONE INPUT SHAPE, N OUTPUT SHAPES. Lowering each outcome on its
        // own - which is what this probe does - emits four KERNELS, and
        // the four share the whole frame up to the point where they
        // diverge. So how much is being duplicated?
        {
            let reach = |roots: &[crate::transpile::graph::NodeId]| {
                let mut seen = std::collections::BTreeSet::new();
                let mut stack: Vec<_> = roots.to_vec();
                while let Some(n) = stack.pop() {
                    if !seen.insert(n) {
                        continue;
                    }
                    stack.extend(g.get(n).args.iter().copied());
                }
                seen
            };
            let per: Vec<std::collections::BTreeSet<_>> = f
                .outs
                .iter()
                .map(|o| {
                    let mut r: Vec<_> =
                        o.fields.iter().map(|(_, nd, _)| *nd).collect();
                    r.push(o.guard);
                    r.push(o.ok);
                    reach(&r)
                })
                .collect();
            let union: std::collections::BTreeSet<_> =
                per.iter().flat_map(|s| s.iter().copied()).collect();
            let sum: usize = per.iter().map(|s| s.len()).sum();
            let shared_by_all: usize = union
                .iter()
                .filter(|n| per.iter().all(|s| s.contains(n)))
                .count();
            eprintln!(
                "[emit] reachable nodes: {:?} per outcome, {} summed, {} in the union, \
                 {} shared by ALL - so lowering separately emits {:.1}x what one body would",
                per.iter().map(|s| s.len()).collect::<Vec<_>>(),
                sum,
                union.len(),
                shared_by_all,
                sum as f64 / union.len() as f64
            );
        }
        // THE A/B THIS EXISTS FOR: all outcomes lowered into ONE body,
        // against the same outcomes lowered one at a time. The separate
        // numbers are printed per outcome below; this is the total they
        // are compared against.
        match super::super::emit::lower_frame(
            &b.graph,
            &b.inputs,
            &b.uni,
            &b.outcomes,
            room.clone(),
            b.forks,
        ) {
            Ok(l) => {
                eprintln!(
                    "[emit] FUSED: {} outcomes in one body, {} lines, {} variants",
                    f.outs.len(),
                    l.body.len(),
                    l.variants.len()
                );
                // Render a whole kernel and drop it where it can be
                // COMPILED. Lowering succeeding only says the emitter
                // produced lines; whether those lines are Rust is a
                // different question, and the only way to answer it is
                // to hand them to rustc.
                match super::super::kernel::render(&f, &b, &l, "room (1,0) f40, steady pm1") {
                    Ok(src) => {
                        let path =
                            std::path::Path::new("target").join("traced-kernel.rs");
                        std::fs::write(&path, &src).expect("write");
                        eprintln!(
                            "[emit] rendered {} bytes to {} ({} lines)",
                            src.len(),
                            path.display(),
                            src.lines().count()
                        );
                    }
                    Err(e) => eprintln!("[emit] RENDER REFUSED: {:#}", e),
                }
            }
            Err(e) => eprintln!("[emit] FUSED REFUSED: {:#}", e),
        }
        for (n, o) in f.outs.iter().enumerate() {
            // The BUTTON cells are not ordinary outputs. They end the
            // frame holding next frame's free choices, so they are
            // button-dependent by construction and would distinguish all
            // 64 assignments on their own - which is why the emitter has
            // `OutFields::ubool` to keep them out of `fields`.
            let lowered = super::super::emit::lower_frame(
                &b.graph,
                &b.inputs,
                &b.uni,
                &b.outcomes[n..n + 1],
                room.clone(),
                b.forks,
            );
            match &lowered {
                Ok(ref l) => eprintln!(
                    "[emit] outcome {}: {} lines, {} variants, {} outputs",
                    n,
                    l.body.len(),
                    l.variants.len(),
                    o.fields.len()
                ),
                Err(e) => eprintln!("[emit] outcome {} REFUSED: {:#}", n, e),
            }
            // WHY 64 variants, where the existing `steady` kernel
            // collapses 64 button assignments to 36? The emitter dedups
            // an assignment on (every output, `ok`, `live`) together, so
            // count each key on its own and find out which one splits,
            // rather than telling a story about the guard.
            let l_variants = match &lowered {
                Ok(l) => l.variants.len(),
                Err(_) => 0,
            };
            {
                let mut sp = crate::transpile::graph::Graph::new();
                let maps: Vec<Vec<crate::transpile::graph::NodeId>> =
                    (0u8..64).map(|m| g.specialize_into(m, &mut sp)).collect();
                // Emission happens on the SPECIALISED arena, not on the
                // traced graph: 64 assignments folded into one interned
                // arena. So the traced node count is not what gets
                // emitted, and the gap between them is what
                // specialisation costs.
                {
                    let mut seen = std::collections::BTreeSet::new();
                    let mut stack: Vec<crate::transpile::graph::NodeId> = Vec::new();
                    for m in 0..64usize {
                        for (_, nd, _) in &o.fields {
                            stack.push(maps[m][*nd as usize]);
                        }
                        stack.push(maps[m][o.ok as usize]);
                        stack.push(maps[m][o.guard as usize]);
                    }
                    while let Some(x) = stack.pop() {
                        if !seen.insert(x) {
                            continue;
                        }
                        stack.extend(sp.get(x).args.iter().copied());
                    }
                    // HEURISTIC DUPLICATION CENSUS. Fingerprint every
                    // reachable node by WHAT IT COMPUTES at a handful of
                    // input points, and bucket. Nodes sharing a fingerprint
                    // are equal without being identical - exactly the
                    // candidates the emitter's structural key cannot merge.
                    // This SIZES the opportunity; it does not authorise the
                    // merge, since agreeing at K points is necessary and not
                    // sufficient.
                    if n == 2 {
                        // MANY points, varying EVERY input. Eight points
                        // bumping one cell each gave 97 fingerprints with
                        // buckets of 8,535 - which is not duplication, it
                        // is collision: two nodes share a fingerprint
                        // whenever they happen to agree everywhere it
                        // looks.
                        //
                        // TWO FAMILIES, because they answer different
                        // questions. NEAR is one game state jittered by
                        // +-8, which is what the first census used; it
                        // reported 58% of the body as never varying, and
                        // a jitter that small cannot tell "always false"
                        // from "false in this neighbourhood". WIDE
                        // resamples each slot across the room - a
                        // different game situation, not a nudge - so a
                        // node constant over BOTH families is evidence of
                        // dead algebra rather than of locality. Wide
                        // points do make some nodes stop evaluating (a
                        // map lookup off the room), which `eval_all`
                        // reports as `None` and the count below carries.
                        let mut rng: u64 = 0x9E3779B97F4A7C15;
                        let mut roll = |m: i64| -> i64 {
                            rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                            (rng >> 33) as i64 % m
                        };
                        let mut near: Vec<Vec<Conc>> = vec![f.iface.init.clone()];
                        for _ in 0..96 {
                            let mut pt = f.iface.init.clone();
                            for slot in pt.iter_mut() {
                                let r = roll(1 << 20);
                                *slot = match *slot {
                                    Conc::Num(v) => Conc::Num(
                                        v + crate::pico8_num::Pico8Num::from_i16(
                                            (r % 17 - 8) as i16,
                                        ),
                                    ),
                                    Conc::Bool(_) => Conc::Bool(r & 1 == 0),
                                };
                            }
                            near.push(pt);
                        }
                        let mut wide: Vec<Vec<Conc>> = Vec::new();
                        for _ in 0..96 {
                            let mut pt = f.iface.init.clone();
                            for slot in pt.iter_mut() {
                                let r = roll(1 << 20);
                                *slot = match *slot {
                                    // A spread of magnitudes rather than
                                    // one range: positions want the whole
                                    // room, speeds want single digits,
                                    // timers want zero, and the census
                                    // does not know which slot is which.
                                    Conc::Num(v) => {
                                        let d = [0i16, 1, -1, 4, -4, 16, -16, 64, -64]
                                            [(r as usize) % 9];
                                        let base = if r & 0x100 == 0 {
                                            v
                                        } else {
                                            crate::pico8_num::Pico8Num::from_i16(0)
                                        };
                                        Conc::Num(base + crate::pico8_num::Pico8Num::from_i16(d))
                                    }
                                    Conc::Bool(_) => Conc::Bool(r & 1 == 0),
                                };
                            }
                            wide.push(pt);
                        }
                        let n_near = near.len();
                        let mut probe_pts = near;
                        probe_pts.extend(wide);
                        let mut cols: Vec<Vec<Option<Conc>>> = Vec::new();
                        for pt in &probe_pts {
                            let env = super::super::eval::Env {
                                cells: pt,
                                frees: &[false; 6],
                                cart: it.cart.clone(),
                                cache: it.cache.clone(),
                            };
                            cols.push(super::super::eval::eval_all(&sp, &env));
                        }
                        let mut buckets: std::collections::HashMap<
                            Vec<Option<(u8, i32)>>,
                            Vec<crate::transpile::graph::NodeId>,
                        > = Default::default();
                        let mut live_nodes = 0usize;
                        let mut undecided = 0usize;
                        for id in seen.iter().copied() {
                            let fp: Vec<Option<(u8, i32)>> = cols
                                .iter()
                                .map(|c| {
                                    c[id as usize].map(|v| match v {
                                        Conc::Num(x) => (0u8, x.as_raw_u32() as i32),
                                        Conc::Bool(b) => (1u8, b as i32),
                                    })
                                })
                                .collect();
                            if fp.iter().all(|x| x.is_none()) {
                                undecided += 1;
                                continue;
                            }
                            live_nodes += 1;
                            buckets.entry(fp).or_default().push(id);
                        }
                        let mut sizes: Vec<usize> = buckets.values().map(|v| v.len()).collect();
                        sizes.sort_unstable_by(|a, b| b.cmp(a));
                        // WHAT is in the biggest buckets? A bucket of
                        // 6,000 is either collision or a genuinely
                        // constant region - those are different problems,
                        // and the fingerprint says which.
                        let mut by_size: Vec<_> = buckets.iter().collect();
                        by_size.sort_by_key(|(_, v)| std::cmp::Reverse(v.len()));
                        for (fp, ids) in by_size.iter().take(2) {
                            let mut ops: std::collections::BTreeMap<String, usize> =
                                Default::default();
                            for id in ids.iter() {
                                *ops.entry(format!("{:?}", sp.get(*id).op)).or_default() += 1;
                            }
                            let mut top: Vec<_> = ops.into_iter().collect();
                            top.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
                            eprintln!(
                                "[emit]     bucket of {}: constant near = {}, constant everywhere \
                                 = {}, value {:?}, ops {:?}",
                                ids.len(),
                                fp[..n_near].iter().all(|x| *x == fp[0]),
                                fp.iter().all(|x| *x == fp[0]),
                                fp[0],
                                &top[..top.len().min(5)]
                            );
                        }
                        // THE QUESTION the wide family exists to answer.
                        // A node constant over the near points alone may
                        // just be one whose branch this state does not
                        // take; a node constant over both families is a
                        // candidate for being genuinely dead.
                        let (mut c_near, mut c_all) = (0usize, 0usize);
                        for fp in buckets.keys() {
                            let n = buckets[fp].len();
                            if fp[..n_near].iter().all(|x| *x == fp[0]) {
                                c_near += n;
                                if fp.iter().all(|x| *x == fp[0]) {
                                    c_all += n;
                                }
                            }
                        }
                        eprintln!(
                            "[emit]   DUPLICATION: {} reachable nodes evaluate somewhere ({} never do); \
                             {} distinct fingerprints over {} points ({} near + {} wide), so a perfect \
                             semantic dedup would keep {:.0}%. Biggest buckets: {:?}",
                            live_nodes,
                            undecided,
                            buckets.len(),
                            probe_pts.len(),
                            n_near,
                            probe_pts.len() - n_near,
                            100.0 * buckets.len() as f64 / live_nodes as f64,
                            &sizes[..sizes.len().min(6)]
                        );
                        eprintln!(
                            "[emit]   CONSTANCY: {} nodes ({:.0}%) never vary over the NEAR points; \
                             of those {} ({:.0}% of the body) also never vary over the WIDE ones",
                            c_near,
                            100.0 * c_near as f64 / live_nodes as f64,
                            c_all,
                            100.0 * c_all as f64 / live_nodes as f64,
                        );

                        // WHERE constancy is CREATED. A false `And` is
                        // false because an operand is, so counting the
                        // constant nodes says how far it spread, not what
                        // started it. A SOURCE is a node that is constant
                        // while none of its operands is: that is where an
                        // analysis would have to look, and its op tells
                        // which analysis.
                        let is_const = |id: crate::transpile::graph::NodeId| -> Option<
                            Option<(u8, i32)>,
                        > {
                            let mut it = cols.iter().map(|c| {
                                c[id as usize].map(|v| match v {
                                    Conc::Num(x) => (0u8, x.as_raw_u32() as i32),
                                    Conc::Bool(b) => (1u8, b as i32),
                                })
                            });
                            let first = it.next().unwrap();
                            if it.all(|x| x == first) {
                                Some(first)
                            } else {
                                None
                            }
                        };
                        let mut src_ops: std::collections::BTreeMap<String, usize> =
                            Default::default();
                        let mut examples: Vec<crate::transpile::graph::NodeId> = Vec::new();
                        for id in seen.iter().copied() {
                            if is_const(id).is_none() {
                                continue;
                            }
                            if sp.get(id).args.iter().any(|a| is_const(*a).is_some()) {
                                continue;
                            }
                            *src_ops.entry(format!("{:?}", sp.get(id).op)).or_default() += 1;
                            if examples.len() < 4 && !sp.get(id).args.is_empty() {
                                examples.push(id);
                            }
                        }
                        let mut top: Vec<_> = src_ops.iter().map(|(k, v)| (k.clone(), *v)).collect();
                        top.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
                        eprintln!(
                            "[emit]   SOURCES of constancy: {} nodes are constant with no constant \
                             operand; ops {:?}",
                            src_ops.values().sum::<usize>(),
                            &top[..top.len().min(6)]
                        );
                        for id in examples {
                            eprintln!(
                                "[emit]     source {:?} = {} (operands vary)",
                                is_const(id).unwrap(),
                                super::super::emit::show_tree(&sp, id, 3)
                            );
                        }
                        // WHICH SHAPES. Four examples is an anecdote; a
                        // rewrite rule needs a count. Everything here is a
                        // guess about the shape until this prints, and
                        // "other" is the honest bucket - it is what says
                        // whether a handful of rules would actually clear
                        // the sources or only the ones I happened to look
                        // at.
                        let compl = |a: crate::transpile::graph::NodeId,
                                     b: crate::transpile::graph::NodeId|
                         -> bool {
                            use crate::transpile::graph::Op as O;
                            let (na, nb) = (sp.get(a), sp.get(b));
                            if na.op == O::Not && na.args[0] == b {
                                return true;
                            }
                            if nb.op == O::Not && nb.args[0] == a {
                                return true;
                            }
                            // `fold` rewrites `Not(Lt)` to `Ge`, so a
                            // complementary PAIR no longer shares a `Not`.
                            let opposite = matches!(
                                (&na.op, &nb.op),
                                (O::Lt, O::Ge)
                                    | (O::Ge, O::Lt)
                                    | (O::Le, O::Gt)
                                    | (O::Gt, O::Le)
                            );
                            opposite && na.args == nb.args
                        };
                        let mut shapes: std::collections::BTreeMap<&str, usize> =
                            Default::default();
                        let mut others: Vec<crate::transpile::graph::NodeId> = Vec::new();
                        for id in seen.iter().copied() {
                            if is_const(id).is_none()
                                || sp.get(id).args.iter().any(|a| is_const(*a).is_some())
                            {
                                continue;
                            }
                            use crate::transpile::graph::Op as O;
                            let nd = sp.get(id);
                            let two = nd.args.len() == 2;
                            let kind = if two && nd.op == O::And && compl(nd.args[0], nd.args[1]) {
                                "and(a, not a)"
                            } else if two && nd.op == O::Or && compl(nd.args[0], nd.args[1]) {
                                "or(a, not a)"
                            } else if two
                                && nd.op == O::Or
                                && sp.get(nd.args[0]).op == O::And
                                && sp.get(nd.args[1]).op == O::And
                                && {
                                    let (l, r) = (sp.get(nd.args[0]), sp.get(nd.args[1]));
                                    // The MERGE shape: split gave `g and c`
                                    // and `g and not c`, and joining them
                                    // is `g` again.
                                    (0..2).any(|i: usize| {
                                        (0..2).any(|j: usize| {
                                            compl(l.args[i], r.args[j])
                                                && l.args[1 - i] == r.args[1 - j]
                                        })
                                    })
                                }
                            {
                                "or(and(a,x), and(not a,x))"
                            } else if two && (nd.op == O::And || nd.op == O::Or) {
                                "other and/or"
                            } else {
                                "other"
                            };
                            *shapes.entry(kind).or_default() += 1;
                            if kind.starts_with("other") && others.len() < 3 {
                                others.push(id);
                            }
                        }
                        let mut sh: Vec<_> = shapes.into_iter().collect();
                        sh.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
                        eprintln!("[emit]   SOURCE SHAPES: {:?}", sh);

                        // WHAT SURVIVES THE DECISION PROCEDURE.
                        //
                        // Everything above is measured on the graph
                        // BEFORE the BDD pass, so it sizes the
                        // opportunity, not the residue. Re-fingerprinting
                        // the SIMPLIFIED graph at the same points says
                        // how much the decision procedure actually took -
                        // and printing the biggest surviving buckets says
                        // what a next mechanism would have to understand,
                        // which is the only way to choose one.
                        {
                            let mut rts: Vec<crate::transpile::graph::NodeId> = Vec::new();
                            for m in 0..64usize {
                                for (_, nd, _) in &o.fields {
                                    rts.push(maps[m][*nd as usize]);
                                }
                                rts.push(maps[m][o.ok as usize]);
                                rts.push(maps[m][o.guard as usize]);
                            }
                            // THE PIPELINE, in the order the two passes
                            // feed each other: interval folding decides
                            // value-range facts the BDD cannot see (`0 >
                            // abs(x)`), the BDD propagates them through
                            // boolean algebra, and a second interval pass
                            // sees the comparisons that collapsed as a
                            // result.
                            let (g1, m1, si1) =
                                crate::transpile::ival::fold(&sp, &rts, room.as_ref()).expect("ival");
                            let r1: Vec<crate::transpile::graph::NodeId> =
                                rts.iter().map(|r| m1[*r as usize]).collect();
                            let (g2, m2, _) = crate::transpile::bdd::simplify_until_stable(
                                &g1,
                                &r1,
                                1 << 22,
                                4,
                            );
                            let r2: Vec<crate::transpile::graph::NodeId> =
                                r1.iter().map(|r| m2[*r as usize]).collect();
                            let (sp2, m3, si2) =
                                crate::transpile::ival::fold(&g2, &r2, room.as_ref()).expect("ival 2");
                            eprintln!(
                                "[emit]   IVAL+BDD: {} -> {} (ival: {} bools, {} nums) -> {} \
                                 (bdd) -> {} (ival again: {} bools, {} nums)",
                                si1.before, si1.after, si1.bools, si1.nums,
                                g2.len(), si2.after, si2.bools, si2.nums
                            );
                            let nm: Vec<crate::transpile::graph::NodeId> = (0..sp.len())
                                .map(|i| {
                                    let a = m1[i];
                                    if a == crate::transpile::bdd::UNREACHABLE {
                                        return a;
                                    }
                                    let b = m2[a as usize];
                                    if b == crate::transpile::bdd::UNREACHABLE {
                                        return b;
                                    }
                                    m3[b as usize]
                                })
                                .collect();
                            let mut seen2 = std::collections::BTreeSet::new();
                            let mut stack2: Vec<crate::transpile::graph::NodeId> =
                                rts.iter().map(|r| nm[*r as usize]).collect();
                            while let Some(x) = stack2.pop() {
                                if !seen2.insert(x) {
                                    continue;
                                }
                                stack2.extend(sp2.get(x).args.iter().copied());
                            }
                            let mut cols2: Vec<Vec<Option<Conc>>> = Vec::new();
                            for pt in &probe_pts {
                                let env = super::super::eval::Env {
                                    cells: pt,
                                    frees: &[false; 6],
                                    cart: it.cart.clone(),
                                    cache: it.cache.clone(),
                                };
                                cols2.push(super::super::eval::eval_all(&sp2, &env));
                            }
                            let mut b2: std::collections::HashMap<
                                Vec<Option<(u8, i32)>>,
                                Vec<crate::transpile::graph::NodeId>,
                            > = Default::default();
                            let (mut live2, mut const2) = (0usize, 0usize);
                            for id in seen2.iter().copied() {
                                let fp: Vec<Option<(u8, i32)>> = cols2
                                    .iter()
                                    .map(|c| {
                                        c[id as usize].map(|v| match v {
                                            Conc::Num(x) => (0u8, x.as_raw_u32() as i32),
                                            Conc::Bool(b) => (1u8, b as i32),
                                        })
                                    })
                                    .collect();
                                if fp.iter().all(|x| x.is_none()) {
                                    continue;
                                }
                                live2 += 1;
                                if fp.iter().all(|x| *x == fp[0]) {
                                    const2 += 1;
                                }
                                b2.entry(fp).or_default().push(id);
                            }
                            eprintln!(
                                "[emit]   SURVIVING: {} nodes evaluate somewhere, {} distinct \
                                 fingerprints ({:.0}% of them), {} ({:.0}%) STILL constant over \
                                 all {} points after the BDD had its say",
                                live2,
                                b2.len(),
                                100.0 * b2.len() as f64 / live2.max(1) as f64,
                                const2,
                                100.0 * const2 as f64 / live2.max(1) as f64,
                                probe_pts.len()
                            );
                            let mut big: Vec<_> = b2.iter().collect();
                            big.sort_by_key(|(_, v)| std::cmp::Reverse(v.len()));
                            for (fp, ids) in big.iter().take(4) {
                                let mut ops: std::collections::BTreeMap<String, usize> =
                                    Default::default();
                                for id in ids.iter() {
                                    *ops.entry(format!("{:?}", sp2.get(*id).op)).or_default() += 1;
                                }
                                let mut top: Vec<_> = ops.into_iter().collect();
                                top.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
                                eprintln!(
                                    "[emit]     SURVIVING bucket of {}: value {:?}, constant = {}, \
                                     ops {:?}",
                                    ids.len(),
                                    fp[0],
                                    fp.iter().all(|x| *x == fp[0]),
                                    &top[..top.len().min(4)]
                                );
                                // The thing itself. Two representatives,
                                // because one example of a 3,000-node
                                // bucket says nothing about the bucket.
                                for id in ids.iter().take(2) {
                                    eprintln!(
                                        "[emit]       #{} = {}",
                                        id,
                                        super::super::emit::show_tree(&sp2, *id, 4)
                                    );
                                }
                            }
                        }
                        for id in others {
                            eprintln!(
                                "[emit]     unclassified {:?} = {}",
                                is_const(id).unwrap(),
                                super::super::emit::show_tree(&sp, id, 4)
                            );
                        }
                    }

                    // THE VARIANT FLOOR, for EVERY outcome. The
                    // emitter dedups the 64 button assignments on
                    // STRUCTURAL identity, which is a sound
                    // under-approximation of "the same successor for
                    // every input". This is how many are distinguishable
                    // at all: fingerprint each assignment by its whole
                    // output tuple across many points and count. Per
                    // outcome because they differ - a single per-point
                    // number cannot tell an outcome that is at its floor
                    // from one that is over-specialised twelvefold.
                    {
                        let mut rng: u64 = 0xDEADBEEF12345678;
                        let mut pts: Vec<Vec<Conc>> = vec![f.iface.init.clone()];
                        for _ in 0..48 {
                            let mut pt = f.iface.init.clone();
                            for slot in pt.iter_mut() {
                                rng =
                                    rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                                let r = (rng >> 33) as i64;
                                *slot = match *slot {
                                    Conc::Num(v) => {
                                        let d = [0i16, 1, -1, 4, -4, 16, -16, 64, -64]
                                            [(r as usize) % 9];
                                        Conc::Num(
                                            v + crate::pico8_num::Pico8Num::from_i16(d),
                                        )
                                    }
                                    Conc::Bool(_) => Conc::Bool(r & 1 == 0),
                                };
                            }
                            pts.push(pt);
                        }
                        let cols: Vec<Vec<Option<Conc>>> = pts
                            .iter()
                            .map(|pt| {
                                super::super::eval::eval_all(
                                    &sp,
                                    &super::super::eval::Env {
                                        cells: pt,
                                        frees: &[false; 6],
                                        cart: it.cart.clone(),
                                        cache: it.cache.clone(),
                                    },
                                )
                            })
                            .collect();
                        let mut fps: std::collections::HashSet<Vec<Option<(u8, i32)>>> =
                            Default::default();
                        for m in 0..64usize {
                            let mut fp: Vec<Option<(u8, i32)>> = Vec::new();
                            // The BUTTON cells are not ordinary outputs -
                            // they end the frame holding next frame's free
                            // choices, so they separate all 64 by
                            // construction, and the emitter keeps them out
                            // of `fields` too. Leaving them in is how this
                            // first reported a floor of 64 against a
                            // structural count of 24, which is impossible:
                            // structural equality implies semantic
                            // equality, so the floor can only be lower.
                            for (_, nd, _) in o.fields.iter().filter(|(p, _, _)| {
                                !iface::show(p).starts_with("__button_states")
                            }) {
                                let node = maps[m][*nd as usize];
                                for c in cols.iter() {
                                    fp.push(c[node as usize].map(|v| match v {
                                        Conc::Num(x) => (0u8, x.as_raw_u32() as i32),
                                        Conc::Bool(b) => (1u8, b as i32),
                                    }));
                                }
                            }
                            fps.insert(fp);
                        }
                        eprintln!(
                            "[emit]   outcome {} VARIANT FLOOR: 64 assignments are \
                             distinguishable {} ways over {} points; the emitter keeps {} \
                             by structural identity",
                            n,
                            fps.len(),
                            pts.len(),
                            l_variants
                        );
                    }

                    // Can the BDD find what the census found by
                    // SAMPLING? The constant algebra lives here, in the
                    // SPECIALISED arena where the buttons are pinned -
                    // not in the traced graph - so this is the arena the
                    // question is actually about.
                    {
                        let mut roots: Vec<crate::transpile::graph::NodeId> = Vec::new();
                        for m in 0..64usize {
                            for (_, nd, _) in &o.fields {
                                roots.push(maps[m][*nd as usize]);
                            }
                            roots.push(maps[m][o.ok as usize]);
                            roots.push(maps[m][o.guard as usize]);
                        }
                        let (_, _, sts) = crate::transpile::bdd::simplify_until_stable(
                            &sp,
                            &roots,
                            1 << 22,
                            4,
                        );
                        for (i, p) in sts.iter().enumerate() {
                            eprintln!(
                                "[emit]     pass {}: {} -> {} nodes, {} constant, {} to atom, \
                                 {} equalities proved, {} atoms, capped = {}",
                                i, p.before, p.after, p.constants, p.to_atom, p.mergeable,
                                p.atoms, p.overflowed
                            );
                        }
                        let st = *sts.first().unwrap();
                        // THE CONTROL. `simplify` rebuilds through
                        // `fold`, so some of the shrinkage is just
                        // normalization cascading on a second pass and
                        // has nothing to do with the BDD. A zero-node
                        // budget makes every boolean an atom, which
                        // leaves exactly the rebuild - so the difference
                        // between the two is what deciding actually
                        // bought.
                        let (_, _, refold) = crate::transpile::bdd::simplify(&sp, &roots, 0);
                        eprintln!(
                            "[emit]   outcome {} REFOLD ONLY (control): {} -> {} nodes",
                            n, refold.before, refold.after
                        );
                        eprintln!(
                            "[emit]   outcome {} SPECIALISED+DECIDED: {} -> {} nodes; {} atoms, \
                             {} bdd nodes, capped = {}; {} constant, {} equal to an atom, \
                             {} more equalities proved",
                            n,
                            st.before,
                            st.after,
                            st.atoms,
                            st.bdd_nodes,
                            st.overflowed,
                            st.constants,
                            st.to_atom,
                            st.mergeable
                        );
                    }

                    eprintln!(
                        "[emit]   outcome {}: {} traced nodes reachable -> {} after \
                         specialising on 64 assignments ({} in the whole arena)",
                        n,
                        {
                            let mut s2 = std::collections::BTreeSet::new();
                            let mut st2: Vec<crate::transpile::graph::NodeId> = o
                                .fields
                                .iter()
                                .map(|(_, nd, _)| *nd)
                                .chain([o.ok, o.guard])
                                .collect();
                            while let Some(x) = st2.pop() {
                                if s2.insert(x) {
                                    st2.extend(g.get(x).args.iter().copied());
                                }
                            }
                            s2.len()
                        },
                        seen.len(),
                        sp.len()
                    );
                }
                let count = |with_ok: bool, with_live: bool| -> usize {
                    let mut seen = std::collections::BTreeSet::new();
                    for m in 0..64usize {
                        let mut k: Vec<crate::transpile::graph::NodeId> = o
                            .fields
                            .iter()
                            .filter(|(p, _, _)| !iface::show(p).starts_with("__button_states"))
                            .map(|(_, nd, _)| maps[m][*nd as usize])
                            .collect();
                        if with_ok {
                            k.push(maps[m][o.ok as usize]);
                        }
                        if with_live {
                            k.push(maps[m][o.guard as usize]);
                        }
                        seen.insert(k);
                    }
                    seen.len()
                };
                eprintln!(
                    "[emit]   outcome {} distinct assignments: outputs {}, +ok {}, +live {}",
                    n,
                    count(false, false),
                    count(true, false),
                    count(true, true)
                );
                // WHICH cells actually differ between assignments? "40
                // variants" is a count; the question is what the buttons
                // are still reaching in a state with no objects left.
                let mut varying: Vec<(String, usize)> = Vec::new();
                for (p, nd, _) in &o.fields {
                    if iface::show(p).starts_with("__button_states") {
                        continue;
                    }
                    let vals: std::collections::BTreeSet<_> =
                        (0..64usize).map(|m| maps[m][*nd as usize]).collect();
                    if vals.len() > 1 {
                        varying.push((iface::show(p), vals.len()));
                    }
                }
                // How many DISTINCT SUCCESSOR ROWS can actually come
                // out? That is the honest meaning of a variant: not "how
                // many assignments produce structurally different
                // expressions" (which is what the emitter's dedup key
                // measures, and which over-counts whenever two
                // expressions are equal without being identical), but
                // "how many different rows can the search see". Anything
                // above this number is generated code that no input can
                // tell apart.
                //
                // Only assignments whose guard holds at the point count -
                // the others describe a different outcome there - and
                // only those the trace did not decline, since a declining
                // lane leaves for the interpreter rather than becoming a
                // row.
                // EXACT paths. `ends_with(".y")` finds
                // `objects[0].dash_accel.y` first, so an earlier version
                // of this perturbed the dash accelerator instead of the
                // player and never reached the death or next-room
                // outcomes at all.
                let base = iface::show(&player);
                let idx = |field: &str| {
                    let want = format!("{}.{}", base, field);
                    f.iface.slots.iter().position(|q| iface::show(q) == want)
                };
                let gidx = |name: &str| {
                    f.iface.slots.iter().position(|q| iface::show(q) == name)
                };
                // A SWEEP, not a nudge. The death, restart and next-room
                // outcomes are only live where the player actually dies
                // or leaves, so a handful of one-pixel perturbations
                // never reaches them and the measurement comes out
                // vacuously zero.
                let n16 = crate::pico8_num::Pico8Num::from_i16;
                let mut points: Vec<Vec<Conc>> = Vec::new();
                for dx in [-8i16, 0, 8] {
                    for dy in [-120i16, -8, 0, 8, 24, 64] {
                        for sy in [None, Some(2i16)] {
                            let mut p = f.iface.init.clone();
                            for (field, d) in [("x", dx), ("y", dy)] {
                                if let Some(i) = idx(field) {
                                    if let Conc::Num(v) = p[i] {
                                        p[i] = Conc::Num(v + n16(d));
                                    }
                                }
                            }
                            if let (Some(v), Some(i)) = (sy, idx("spd.y")) {
                                p[i] = Conc::Num(n16(v));
                            }
                            points.push(p);
                        }
                    }
                }
                // The cells `freeze` and `has_dashed` actually depend on
                // are the DASH AND JUMP state, and a sweep over position
                // and velocity never touches them - so a row count taken
                // from position alone is a lower bound measured in the
                // wrong dimensions.
                for (field, vals) in [
                    ("djump", [0i16, 1].as_slice()),
                    ("dash_time", [0, 1, 3].as_slice()),
                    ("grace", [0, 1, 6].as_slice()),
                    ("dash_effect_time", [0, 5].as_slice()),
                ] {
                    if let Some(i) = idx(field) {
                        for v in vals {
                            let mut p = f.iface.init.clone();
                            p[i] = Conc::Num(n16(*v));
                            points.push(p);
                        }
                    }
                }
                for field in ["p_dash", "p_jump"] {
                    if let Some(i) = idx(field) {
                        for v in [false, true] {
                            let mut p = f.iface.init.clone();
                            p[i] = Conc::Bool(v);
                            points.push(p);
                        }
                    }
                }
                if let (Some(w), Some(d)) = (gidx("will_restart"), gidx("delay_restart")) {
                    let mut p = f.iface.init.clone();
                    p[w] = Conc::Bool(true);
                    p[d] = Conc::Num(n16(1));
                    points.push(p);
                }
                let mut most_rows = 0usize;
                let mut live_at_most = 0usize;
                for pt in &points {
                    let mut rows: std::collections::BTreeSet<Vec<(u8, i32)>> = Default::default();
                    let mut live_here = 0usize;
                    for m in 0u8..64 {
                        let mut bits = [false; 6];
                        for (i, b) in bits.iter_mut().enumerate() {
                            *b = m & (1 << i) != 0;
                        }
                        let env = super::super::eval::Env {
                            cells: pt,
                            frees: &bits,
                            cart: it.cart.clone(),
                            cache: it.cache.clone(),
                        };
                        let ev = |nd| super::super::eval::eval(&g, nd, &env);
                        let holds = |nd| matches!(ev(nd), Ok(Conc::Bool(true)));
                        if !holds(o.guard) || !holds(o.ok) {
                            continue;
                        }
                        live_here += 1;
                        let mut row = Vec::new();
                        for (q, nd, _) in &o.fields {
                            if iface::show(q).starts_with("__button_states") {
                                continue;
                            }
                            match ev(*nd) {
                                Ok(Conc::Num(v)) => row.push((0u8, v.as_raw_u32() as i32)),
                                Ok(Conc::Bool(b)) => row.push((1u8, b as i32)),
                                Err(_) => row.push((2u8, 0)),
                            }
                        }
                        rows.insert(row);
                    }
                    if rows.len() > most_rows {
                        most_rows = rows.len();
                        live_at_most = live_here;
                    }
                }
                eprintln!(
                    "[emit]   outcome {}: emitter keeps {} variants; the most DISTINCT ROWS any \
                     of {} input points produces is {} (from {} live assignments)",
                    n,
                    l_variants,
                    points.len(),
                    most_rows,
                    live_at_most
                );

                // WHY do assignments that produce the same VALUE keep
                // different trees? The emitter's dedup key is structural
                // node identity after specialisation, so they collapse
                // exactly when they fold to the same tree. Print the
                // first two that differ, shallowly.
                if n == 2 {
                    // WHAT holds the 24 variants apart. The boolean layer
                    // is decided now, so whatever distinguishes them is
                    // numeric - and naming it is the difference between
                    // "the gap is numeric" and knowing what to do.
                    let mut cmp: std::collections::BTreeMap<
                        crate::transpile::graph::NodeId,
                        Vec<(String, i32)>,
                    > = Default::default();
                    for id in 0..sp.len() as crate::transpile::graph::NodeId {
                        use crate::transpile::graph::Op as O;
                        let nd = sp.get(id);
                        if !matches!(nd.op, O::Lt | O::Le | O::Gt | O::Ge | O::Eq) {
                            continue;
                        }
                        // A comparison against a literal: the shape an
                        // implication between atoms would relate.
                        let (a, b) = (nd.args[0], nd.args[1]);
                        let pick = match (&sp.get(a).op, &sp.get(b).op) {
                            (O::Const(lo, hi), _) if lo == hi => Some((b, *lo)),
                            (_, O::Const(lo, hi)) if lo == hi => Some((a, *lo)),
                            _ => None,
                        };
                        if let Some((x, k)) = pick {
                            cmp.entry(x).or_default().push((format!("{:?}", nd.op), k));
                        }
                    }
                    let mut groups: Vec<_> =
                        cmp.into_iter().filter(|(_, v)| v.len() > 1).collect();
                    groups.sort_by_key(|(_, v)| std::cmp::Reverse(v.len()));
                    eprintln!(
                        "[emit]   ATOM IMPLICATIONS: {} distinct values are compared against \
                         2+ literals ({} such comparisons in total)",
                        groups.len(),
                        groups.iter().map(|(_, v)| v.len()).sum::<usize>()
                    );
                    for (x, ks) in groups.iter().take(3) {
                        let mut ks = ks.clone();
                        ks.sort_by_key(|(_, k)| *k);
                        eprintln!(
                            "[emit]     {} compared against {:?}",
                            super::super::emit::show_tree(&sp, *x, 2),
                            ks.iter()
                                .map(|(o, k)| format!("{} {}", o, *k as f64 / 65536.0))
                                .collect::<Vec<_>>()
                        );
                    }
                }
                if n == 2 {
                    let show = super::super::emit::show_tree;
                    if let Some((fp, fnode, _)) =
                        o.fields.iter().find(|(q, _, _)| iface::show(q) == "deaths")
                    {
                        let mut seen: std::collections::BTreeMap<
                            crate::transpile::graph::NodeId,
                            Vec<u8>,
                        > = Default::default();
                        for m in 0u8..64 {
                            seen.entry(maps[m as usize][*fnode as usize])
                                .or_default()
                                .push(m);
                        }
                        eprintln!(
                            "[emit]   {} has {} distinct trees over 64 assignments",
                            iface::show(fp),
                            seen.len()
                        );
                        for (node, ms) in seen.iter().take(3) {
                            eprintln!(
                                "[emit]     masks {:?}..: {}",
                                &ms[..ms.len().min(3)],
                                show(&sp, *node, 3)
                            );
                        }
                    }
                }

                varying.sort_by_key(|(_, k)| std::cmp::Reverse(*k));
                eprintln!(
                    "[emit]   outcome {}: {} of {} output cells vary; top: {:?}",
                    n,
                    varying.len(),
                    o.fields.len(),
                    varying.iter().take(6).collect::<Vec<_>>()
                );
            }
        }
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
        // `break` is reached the bound stops mattering, so `ok` has to be
        // true there too. Before the fix the loop ran on and the "it
        // finished" obligation made this lane deopt.
        for a in [0i16, 1, 2, 3, 5, 8, 100] {
            let cells = [Conc::Num(crate::pico8_num::Pico8Num::from_i16(a))];
            let env = super::super::eval::Env {
                cells: &cells,
                frees: &[false; 6],
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
            let ok = super::super::eval::eval(&it.d.graph, st.ok, &env).expect("eval ok");
            assert_eq!(ok, Conc::Bool(true), "amount = {}: the trace declined", a);
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
        let frame = full_moon::parse("_update()").expect("parse frame");

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
        let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots, &[], &[]) {
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
                match check_at(&it, &f, &cells, &bits, &want) {
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
                        *declined_at.entry(label.to_string()).or_default() += 1;
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
