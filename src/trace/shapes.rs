//! Every heap shape a room reaches.
//!
//! A kernel is specialized to one INPUT SHAPE, so covering a room
//! without ever deopting means knowing every shape the room reaches.
//! That set is a fixpoint: start at the spawn shape, trace, collect the
//! outcomes' shapes, repeat.
//!
//! ## The values do not matter
//!
//! Which is what makes this a fixpoint over SHAPES rather than over
//! states. Every non-frozen scalar is symbolized at the start of each
//! frame, so whatever a slot held is erased before it can decide
//! anything - two states with the same shape trace identically.
//! Stepping forward only needs a state with the right shape, so `blank`
//! erases the values rather than carrying ones that look meaningful and
//! are not.
//!
//! ## "Everything symbolic" is not every number in the heap
//!
//! `types` and the object prototypes it holds are program CONSTANTS.
//! `balloon.tile` is 22 in the source and stays 22. Symbolize it and
//! `type.tile == tile` in `load_room`'s scan is undecidable, so the
//! tracer explores every object type for every tile - including a
//! balloon, whose `init` calls `rnd`, which the minimal cart does not
//! define.
//!
//! `room` goes with them: `load_room` does `mget(room.x*16+tx, ...)`, so
//! a symbolic room is every room. Freezing it states an existing
//! specialization rather than adding one, since a kernel is per room by
//! construction - `G` carries that room's collision cache.
//!
//! The button INDICES too: `btn(k)` asserts its argument is one of six.
//!
//! Note what is NOT frozen. `freeze`, `will_restart`, `delay_restart`,
//! `has_dashed` and `has_key` are also assigned at the cart's toplevel,
//! and they are state. "Set up at toplevel" is not the rule. The rule is
//! "no frame writes it", and this list is the part of it discovered so
//! far - by refusal, which is worth saying plainly.
//!
//! The risk direction is the safe one. Freezing something that is really
//! state would MISS a shape, hence a kernel, hence a deopt - and a deopt
//! stops the run and names itself, so an over-freeze is loud.
//!
//! ## The room exit is a terminal
//!
//! With the player's position symbolic the tracer takes the room-exit
//! branch, `next_room` calls `load_room(room.x+1, room.y)`, and a
//! DIFFERENT room's whole object set arrives - then that state feeds the
//! next iteration and exits again. Stepping through it, the walk found
//! 16 shapes and had not closed, at 155 s and two million graph nodes,
//! with twelve `fall_floor`s in a room whose only object tile is
//! `player_spawn`. Recording the exit and not stepping it: 3 shapes,
//! closed, 0.2 s, 4,423 nodes.
//!
//! Nothing has to be made concrete for that. The exit is a real
//! successor; it just belongs to another room's kernel set.

use anyhow::Result;
use full_moon::ast;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::interp::Interp;
use super::state::State;
use super::verify::{trace_frame, Frame};

/// Tables holding PROGRAM CONSTANTS rather than state: the object
/// prototypes in `types`, everything reachable from them, and `room`.
///
/// Identified structurally, from `types`, rather than by listing names -
/// the cart's own type list is its answer to "what is a prototype".
pub fn frozen_tables(st: &State<Symbolic>) -> std::collections::BTreeSet<u32> {
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

/// Globals that are program constants. See the module docs: `btn(k)`
/// asserts its argument is one of the six, so a symbolic `k_right`
/// makes every `btn` call fail.
pub const FROZEN_GLOBALS: &[&str] =
    &["k_left", "k_right", "k_up", "k_down", "k_jump", "k_dash"];

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

/// Every scalar that is STATE: what a kernel for this shape takes as
/// input, and what the tracer symbolizes.
pub fn state_paths(st: &State<Symbolic>) -> Result<Vec<Path>> {
    let frozen = frozen_tables(st);
    Ok(iface::scalars(st, &[])?
        .into_iter()
        .filter(|p| !under_frozen(st, p, &frozen))
        .collect())
}

/// Erase every non-frozen scalar. The values are about to be symbolized
/// anyway; carrying the ones an outcome happened to compute would
/// suggest they mean something.
pub fn blank(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for p in state_paths(st)? {
        let v = match iface::get(st, &p) {
            Some(Value::Bool(_)) => Value::Bool(d.boolean(false)),
            _ => Value::Num(d.num(celeste_core::pico8_num::Pico8Num::from_i16(0))),
        };
        iface::set(st, &p, v)?;
    }
    // The path condition and the obligation belong to the frame that
    // produced this state, not to the one about to be traced from it.
    //
    // Carrying them is not a small inaccuracy. `guard` is what a merge
    // SELECTS ON, so a stale one puts the previous frame's input cells
    // inside this frame's output values - and those cells are that
    // frame's dense slot indices, which this frame's interface does not
    // name. That is how "Op::Cell(18) is not an interface slot (17
    // slots)" and a select with a numeric arm and a boolean arm both
    // arrived: neither is a mixed VALUE, both are one frame's expression
    // read in another frame's numbering.
    //
    // Dropping `ok` is not dropping the obligation. A lane only reaches
    // this state by satisfying the previous kernel's `ok`, which that
    // kernel checks. Re-checking it here would deopt a lane twice for
    // one obligation, and it would need cells this frame does not have.
    st.guard = d.boolean(true);
    st.ok = d.boolean(true);
    Ok(())
}

/// Which room a state is in. Concrete: `room` is frozen as an input and
/// `load_room` only ever writes it a constant.
pub fn room_of(st: &State<Symbolic>, d: &Symbolic) -> (i16, i16) {
    let at = |k: &str| -> i16 {
        match iface::get(st, &[iface::key("room"), iface::key(k)]) {
            Some(Value::Num(n)) => {
                d.as_const(&n).and_then(|v| v.as_i16_or_err().ok()).unwrap_or(-1)
            }
            _ => -1,
        }
    };
    (at("x"), at("y"))
}

/// One shape, and the frame traced from it.
pub struct Shape {
    pub state: State<Symbolic>,
    pub frame: Frame,
}

/// The walk's result, with everything it declined to follow. Nothing is
/// dropped silently: a bounded analysis that does not report what it
/// bounded reads as complete when it is not.
pub struct Walk {
    pub shapes: Vec<Shape>,
    /// Outcomes that left the room - real successors, belonging to
    /// another room's kernel set.
    pub left_room: usize,
    /// Outcomes reached only along poisoned paths, which no legal run
    /// takes (`Interp::poison`).
    pub unreachable: usize,
    /// Outcomes discarded because the walk hit `cap`. NOT zero means the
    /// shape set is incomplete and a kernel is missing.
    pub dropped: usize,
    /// Traces that stopped, by reason.
    pub refused: std::collections::BTreeMap<String, usize>,
}

/// Walk the room's shapes to a fixpoint, tracing one frame per shape.
///
/// `start` must be a state in the room the kernels are for; the walk
/// stays in it.
/// The scalar fields of `st` that are compile-time constants: their value
/// node is an exact `Const(v,v)` (numbers) or `ConstBool` (booleans).
/// These are the fields the per-shape constant lattice can bake in.
/// The scalar fields the runtime BOUNDARY widens to intervals, so they are
/// never compile-time constants no matter what a trace computes: the
/// player's `rem` (ival_paths) and every live fruit's `off`/`y`
/// (widen.rs). The lattice must not bake these, or a mid-game block whose
/// `off` is an interval will not bind a kernel that expects a number.
pub fn boundary_widened_paths(st: &State<Symbolic>) -> std::collections::BTreeSet<Path> {
    let mut out: std::collections::BTreeSet<Path> = ival_paths(st).into_iter().collect();
    let Some(Value::Table(fruit)) = iface::get(st, &[iface::key("fruit")]) else { return out };
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return out };
    let n = st.heap.tables[&objects].arr.len();
    for i in 0..n {
        let base = vec![iface::key("objects"), Step::Idx(i)];
        let mut ty = base.clone(); ty.push(iface::key("type"));
        if iface::get(st, &ty) != Some(Value::Table(fruit)) { continue; }
        for f in ["off", "y"] {
            let mut p = base.clone(); p.push(iface::key(f));
            if iface::get(st, &p).is_some() { out.insert(p); }
        }
    }
    out
}

pub fn field_constants(
    st: &State<Symbolic>,
    d: &Symbolic,
) -> Result<std::collections::BTreeMap<Path, super::iface::Conc>> {
    use super::domain::Domain;
    use super::iface::Conc;
    let widened = boundary_widened_paths(st);
    let mut out = std::collections::BTreeMap::new();
    for p in state_paths(st)? {
        if widened.contains(&p) { continue; }
        match iface::get(st, &p) {
            Some(Value::Num(n)) => {
                if let Some(v) = d.as_const(&n) {
                    out.insert(p, Conc::Num(v));
                }
            }
            Some(Value::Bool(b)) => {
                if let Some(v) = d.decide(&b) {
                    out.insert(p, Conc::Bool(v));
                }
            }
            _ => {}
        }
    }
    Ok(out)
}

pub fn walk<'a>(
    it: &mut Interp<'a, Symbolic>,
    reset: &'a ast::Ast,
    frame: &'a ast::Ast,
    start: State<Symbolic>,
    cap: usize,
) -> Result<Walk> {
    let room0 = room_of(&start, &it.d);
    let key = |st: &State<Symbolic>| -> Result<String> { Ok(format!("{:?}", st.shape()?)) };

    let mut seen: std::collections::BTreeMap<String, State<Symbolic>> = Default::default();
    let mut queue: Vec<String> = vec![key(&start)?];
    seen.insert(queue[0].clone(), start);

    let mut out = Walk {
        shapes: Vec::new(),
        left_room: 0,
        unreachable: 0,
        dropped: 0,
        refused: Default::default(),
    };
    while let Some(k) = queue.pop() {
        let st = seen[&k].clone();
        let roots = state_paths(&st)?;
        let ival = ival_paths(&st);
        let f = match trace_frame(it, reset, frame, st.clone(), &roots, &[], &ival, true) {
            Ok(f) => f,
            Err(e) => {
                *out.refused.entry(format!("{:#}", e)).or_default() += 1;
                continue;
            }
        };
        for o in &f.outs {
            if it.d.decide(&o.ok) == Some(false) {
                out.unreachable += 1;
                continue;
            }
            if room_of(&o.st, &it.d) != room0 {
                out.left_room += 1;
                continue;
            }
            let k = key(&o.st)?;
            if seen.contains_key(&k) {
                continue;
            }
            if seen.len() >= cap {
                out.dropped += 1;
                continue;
            }
            let mut next = o.st.clone();
            blank(&mut next, &mut it.d)?;
            seen.insert(k.clone(), next);
            queue.push(k);
        }
        out.shapes.push(Shape { state: st, frame: f });
    }
    Ok(out)
}

/// The slots the boundary WIDENS to an interval: the player's
/// `rem.x` and `rem.y`.
///
/// Found the way `Rt2::mark_walk` finds them - the objects whose `type`
/// is the `player` global - rather than by position, because which
/// object is the player changes within a room and the widening follows
/// the type, not the index.
pub fn ival_paths(st: &State<Symbolic>) -> Vec<Path> {
    let Some(Value::Table(player)) = iface::get(st, &[iface::key("player")]) else {
        return Vec::new();
    };
    let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else {
        return Vec::new();
    };
    let n = st.heap.tables[&objects].arr.len();
    let mut out = Vec::new();
    for i in 0..n {
        let base = vec![iface::key("objects"), Step::Idx(i)];
        let mut ty = base.clone();
        ty.push(iface::key("type"));
        if iface::get(st, &ty) != Some(Value::Table(player)) {
            continue;
        }
        for f in ["x", "y"] {
            let mut p = base.clone();
            p.push(iface::key("rem"));
            p.push(iface::key(f));
            if iface::get(st, &p).is_some() {
                out.push(p);
            }
        }
    }
    // A live fruit's `off` and `y` are widened to intervals by the runtime
    // boundary (widen.rs), so they arrive at a kernel as intervals, not
    // numbers. They must be ival INPUTS or a mid-game block will not bind
    // (room 1 has no fruit, so this is a no-op there).
    if let Some(Value::Table(fruit)) = iface::get(st, &[iface::key("fruit")]) {
        for i in 0..n {
            let base = vec![iface::key("objects"), Step::Idx(i)];
            let mut ty = base.clone(); ty.push(iface::key("type"));
            if iface::get(st, &ty) != Some(Value::Table(fruit)) { continue; }
            for f in ["off", "y"] {
                let mut p = base.clone(); p.push(iface::key(f));
                if iface::get(st, &p).is_some() { out.push(p); }
            }
        }
    }
    out
}
