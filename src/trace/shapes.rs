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

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;

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
    st.guard = d.boolean(true);
    st.ended = d.boolean(false);
    Ok(())
}

/// Every number and boolean in the heap - its tables' and its closure scopes'
/// - that is a constant, by node: what `rebase` re-makes in another arena.
pub fn heap_constants(st: &State<Symbolic>, d: &Symbolic) -> std::collections::HashMap<u32, iface::Conc> {
    use super::domain::Domain;
    let mut out = std::collections::HashMap::new();
    let mut note = |v: &Value<Symbolic>| match v {
        Value::Num(n) => {
            if let Some(c) = d.as_const(n) {
                out.insert(*n, iface::Conc::Num(c));
            }
        }
        Value::Bool(b) => {
            if let Some(c) = d.decide(b) {
                out.insert(*b, iface::Conc::Bool(c));
            }
        }
        _ => {}
    };
    for tab in st.heap.tables.values() {
        for v in tab.hash.values().chain(tab.arr.iter()).chain(tab.ints.values()) {
            note(v);
        }
    }
    for sc in st.heap.scopes.values() {
        for v in sc.vars.values() {
            note(v);
        }
    }
    out
}

/// Move a walk's representative state into another tracer's arena
/// (`kernel::room_constant_lattice`'s rounds): every number and boolean names
/// a node of the arena it was produced in. The slots `blank` rewrites are
/// blanked; every OTHER scalar - a frozen table's, a closure scope's (the
/// `x`/`y` an object's methods captured at `init_object`) - is re-made from
/// `constants` (`heap_constants` of the state, in the arena it came from); a
/// table scalar that is neither refuses, a closure scope's is blanked (below).
/// The path's decisions and the key overrides go with `guard`.
pub fn rebase(st: &mut State<Symbolic>, d: &mut Symbolic, constants: &std::collections::HashMap<u32, iface::Conc>) -> Result<()> {
    // Where `blank` writes: each state path's (table, last step).
    let mut blanked: std::collections::BTreeSet<(u32, Step)> = Default::default();
    for p in state_paths(st)? {
        let Some((last, parent)) = p.split_last() else { continue };
        if let Some(Value::Table(t)) = iface::get(st, &parent.to_vec()) {
            blanked.insert((t, last.clone()));
        }
    }
    let remake = |d: &mut Symbolic, at: &dyn Fn() -> String, v: &mut Value<Symbolic>| -> Result<()> {
        let (n, is_num) = match v {
            Value::Num(n) => (*n, true),
            Value::Bool(b) => (*b, false),
            _ => return Ok(()),
        };
        *v = match (constants.get(&n), is_num) {
            (Some(iface::Conc::Num(c)), true) => Value::Num(d.num(*c)),
            (Some(iface::Conc::Bool(c)), false) => Value::Bool(d.boolean(*c)),
            _ => anyhow::bail!("rebasing a walk state: {} holds node {n}, neither a constant nor a slot `blank` rewrites", at()),
        };
        Ok(())
    };
    for (t, tab) in st.heap.tables.iter_mut() {
        for (k, v) in tab.hash.iter_mut() {
            if !blanked.contains(&(*t, Step::Key(k.clone()))) {
                remake(d, &|| format!("table {t} [{k:?}]"), v)?;
            }
        }
        for (i, v) in tab.arr.iter_mut().enumerate() {
            if !blanked.contains(&(*t, Step::Idx(i))) {
                remake(d, &|| format!("table {t} [{i}]"), v)?;
            }
        }
        for (i, v) in tab.ints.iter_mut() {
            if !blanked.contains(&(*t, Step::Int(*i))) {
                remake(d, &|| format!("table {t} [int {i}]"), v)?;
            }
        }
    }
    // A closure scope can also hold a value no constant describes: the `y`
    // `init_object(player, this.x, this.y)` captured is the spawn's symbolic
    // position, a node of the previous frame. Nothing reads it again (the
    // methods read `obj`), and a trace that did would read garbage either way:
    // in the serial walk the node is the previous frame's `Cell(k)`, which is
    // hash-consed with THIS frame's slot k. So it is blanked like `guard`,
    // which is what carrying it amounted to.
    for sc in st.heap.scopes.values_mut() {
        for v in sc.vars.values_mut() {
            let (n, is_num) = match v {
                Value::Num(n) => (*n, true),
                Value::Bool(b) => (*b, false),
                _ => continue,
            };
            *v = match (constants.get(&n), is_num) {
                (Some(iface::Conc::Num(c)), true) => Value::Num(d.num(*c)),
                (Some(iface::Conc::Bool(c)), false) => Value::Bool(d.boolean(*c)),
                (_, true) => Value::Num(d.num(celeste_core::pico8_num::Pico8Num::from_i16(0))),
                (_, false) => Value::Bool(d.boolean(false)),
            };
        }
    }
    blank(st, d)?;
    st.path.clear();
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

/// The scalar fields of `st` that are compile-time constants: their value
/// node is an exact `Const(v,v)` (numbers) or `ConstBool` (booleans).
/// These are the fields the per-shape constant lattice can bake in.
/// The scalar fields the runtime BOUNDARY widens to intervals, so they are
/// never compile-time constants no matter what a trace computes: the
/// player's `rem` (ival_paths) and every live fruit's `off`/`y`
/// (widen.rs). The lattice must not bake these, or a mid-game block whose
/// `off` is an interval will not bind a kernel that expects a number.
pub fn boundary_widened_paths(st: &State<Symbolic>, opts: &crate::abstraction::Level) -> std::collections::BTreeSet<Path> {
    let mut out: std::collections::BTreeSet<Path> = ival_paths(st).into_iter().collect();
    // The fields a level widens beyond the boundary's own: the moving
    // platforms, the fly fruit, the fall floors.
    out.extend(level_widened_paths(st, opts));
    // Held buttons unknown: the boundary writes the trails unknown.
    if opts.held {
        if let Some(pl) = player_path(st) {
            for f in ["p_jump", "p_dash"] {
                let mut p = pl.clone();
                p.push(iface::key(f));
                out.insert(p);
            }
        }
    }
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

/// The object fields a level widens (the moving platforms, the fly fruit,
/// the fall floors, at the levels that widen them): never compile-time
/// constants, whatever a trace computes.
pub fn level_widened_paths(st: &State<Symbolic>, opts: &crate::abstraction::Level) -> Vec<Path> {
    let mut out = Vec::new();
    if opts.platforms {
        out.extend(super::widen::platform_paths(st).all().cloned());
    }
    if opts.fruit {
        out.extend(super::widen::fly_fruit_paths(st).all().cloned());
    }
    if opts.floors_near {
        out.extend(super::widen::floor_timer_paths(st));
        out.extend(super::widen::near_floor_paths(st).all().cloned());
        out.extend(super::widen::phase_paths(st).into_iter().map(|(p, _)| p));
    }
    out
}

pub fn field_constants(st: &State<Symbolic>, d: &Symbolic, opts: &crate::abstraction::Level) -> Result<std::collections::BTreeMap<Path, super::iface::Conc>> {
    use super::domain::Domain;
    use super::iface::Conc;
    let widened = boundary_widened_paths(st, opts);
    // The buttons are dead at the boundary: blocks hold them unknown (the
    // canonical form, `refbridge`) and every frame resets them before it
    // reads them. A concrete `false` in the post-`_init` start state is not a
    // constant of the shape: pinned, the start shape's kernel guarded on them
    // and read an unknown (room (3,0), whose start shape no frame returns to
    // once `delay` is materialized, so nothing narrowed the pin away).
    let buttons = iface::key("__button_states");
    let mut out = std::collections::BTreeMap::new();
    for p in state_paths(st)? {
        if widened.contains(&p) || p.first() == Some(&buttons) { continue; }
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

/// The path of the player INSTANCE in `objects` (the entry whose `type`
/// is the `player` global), if the state has one.
pub fn player_path(st: &State<Symbolic>) -> Option<Path> {
    let Value::Table(want) = iface::get(st, &[iface::key("player")])? else { return None };
    let Value::Table(objects) = iface::get(st, &[iface::key("objects")])? else { return None };
    let n = st.heap.tables[&objects].arr.len();
    (0..n)
        .map(|i| vec![iface::key("objects"), Step::Idx(i)])
        .find(|base| {
            let mut ty = base.clone();
            ty.push(iface::key("type"));
            iface::get(st, &ty) == Some(Value::Table(want))
        })
}

/// The slots the boundary WIDENS to an interval: the player's
/// `rem.x` and `rem.y` (and a live fruit's `off`/`y`).
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
    // boundary (widen.rs 3b), so a mid-game block carries them as
    // intervals - they must be ival INPUTS or a kernel expecting `num`
    // will not bind. `sin((1+off)/40)` over the widened `off` folds to
    // its range [-1,1] (domain.rs fun1), so this no longer breaks lower.
    // Room 1 has no fruit -> no-op there.
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
