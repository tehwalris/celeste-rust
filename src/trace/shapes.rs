//! Every heap shape a room reaches: a fixpoint over SHAPES (start at the
//! spawn shape, trace, collect the outcomes' shapes, repeat), since a
//! kernel is specialized to one input shape.
//!
//! The values do not matter: every non-frozen scalar is symbolized at the
//! start of each frame, so two states with the same shape trace
//! identically, and `blank` erases the values.
//!
//! FROZEN (program constants, not symbolized): `types` and the prototypes
//! it holds (a symbolic `type.tile` would make `load_room` try every type
//! for every tile), `room` (a kernel is per room anyway), and the button
//! indices (`btn(k)` asserts `k` is one of six). The rule is "no frame
//! writes it", not "set at toplevel" (`freeze`, `has_key` etc. are state).
//! Over-freezing would miss a shape, hence a kernel, which is a loud fatal
//! coverage gap - the safe direction.
//!
//! The room exit is a TERMINAL: recorded, not stepped, since the next
//! room's objects belong to another room's kernel set (stepping it never
//! closes the walk).

use anyhow::Result;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;

/// Tables holding PROGRAM CONSTANTS rather than state: the object
/// prototypes in `types`, everything reachable from them, and `room`.
///
/// Identified structurally, from `types`, rather than by listing names.
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

/// Globals that are program constants: `btn(k)` asserts its argument is
/// one of the six.
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

/// Erase every non-frozen scalar (they are about to be symbolized anyway).
pub fn blank(st: &mut State<Symbolic>, d: &mut Symbolic) -> Result<()> {
    for p in state_paths(st)? {
        let v = match iface::get(st, &p) {
            Some(Value::Bool(_)) => Value::Bool(d.boolean(false)),
            _ => Value::Num(d.num(celeste_core::pico8_num::Pico8Num::from_i16(0))),
        };
        iface::set(st, &p, v)?;
    }
    // The path condition and the obligation belong to the frame that
    // produced this state. A stale `guard` (what a merge selects on) would
    // put the previous frame's input cells, in its numbering, into this
    // frame's outputs.
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

/// Move a walk's representative state into another tracer's arena (every
/// number and boolean names a node of the arena it was produced in). The
/// slots `blank` rewrites are blanked; every other scalar is re-made from
/// `constants` (`heap_constants` in the source arena); a table scalar that
/// is neither refuses, a closure scope's is blanked.
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
    // A closure scope can hold a non-constant (the spawn position
    // `init_object` captured, a node of the previous frame). Nothing reads
    // it again (the methods read `obj`), so it is blanked like `guard`.
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

/// The scalar fields the BOUNDARY (or the level) widens, so never
/// compile-time constants whatever a trace computes: the player's `rem`,
/// a live fruit's `off`/`y`, the level's object fields, the held trails.
/// The lattice must not bake these, or a widened block will not bind.
pub fn boundary_widened_paths(st: &State<Symbolic>, opts: &crate::abstraction::Level) -> std::collections::BTreeSet<Path> {
    let mut out: std::collections::BTreeSet<Path> = ival_paths(st).into_iter().collect();
    // The fields a level widens beyond the boundary's own.
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
    if opts.speed {
        out.extend(super::widen::speed_paths(st));
    }
    if opts.floors_near {
        out.extend(super::widen::floor_timer_paths(st));
        out.extend(super::widen::near_floor_paths(st).all().cloned());
        out.extend(super::widen::phase_paths(st).into_iter().map(|(p, _)| p));
    }
    out
}

/// The scalar fields of `st` that are compile-time constants (exact
/// `Const(v,v)` or `ConstBool`): what the per-shape constant lattice bakes in.
pub fn field_constants(st: &State<Symbolic>, d: &Symbolic, opts: &crate::abstraction::Level) -> Result<std::collections::BTreeMap<Path, super::iface::Conc>> {
    use super::domain::Domain;
    use super::iface::Conc;
    let widened = boundary_widened_paths(st, opts);
    // The buttons are dead at the boundary (blocks hold them unknown; every
    // frame resets them before reading), so the start state's concrete
    // `false` is not a constant of the shape and must not be baked.
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
/// Found by `type` (as `Rt2::mark_walk` does), not by index: which object
/// is the player changes within a room.
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
    // A live fruit's `off` and `y` are widened by the boundary, so they
    // must be ival INPUTS or a kernel expecting `num` will not bind.
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
