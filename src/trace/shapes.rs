//! A state as the walk sees it: which scalars are STATE (`state_paths`, what
//! a trace symbolizes), its constants, and `blank` (every non-frozen scalar
//! is symbolized at a frame's start, so two states of one shape trace
//! identically: the walk's representatives hold them blanked).
//!
//! FROZEN (program constants, not symbolized): `types` and the prototypes it
//! holds (a symbolic `type.tile` would make `load_room` try every type for
//! every tile), `room`, and the button indices. The rule is "no frame writes
//! it", not "set at toplevel" (`freeze`, `has_key` are state). Over-freezing
//! would miss a shape, hence a kernel: a loud fatal coverage gap, the safe
//! direction.

use anyhow::Result;

use super::domain::{Domain, Symbolic};
use crate::transpile::garith::GraphArith;
use super::heap::Value;
use super::iface::{self, Path, Step};
use super::state::State;

/// Tables holding PROGRAM CONSTANTS rather than state: the object
/// prototypes in `types`, everything reachable from them, and `room`
/// (structurally, from `types`, not by listing names).
pub fn frozen_tables<A: GraphArith>(st: &State<Symbolic<A>>) -> std::collections::BTreeSet<u32> {
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

fn under_frozen<A: GraphArith>(
    st: &State<Symbolic<A>>,
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

/// Every scalar that is STATE: a kernel's input, what the tracer symbolizes.
pub fn state_paths<A: GraphArith>(st: &State<Symbolic<A>>) -> Result<Vec<Path>> {
    let frozen = frozen_tables(st);
    Ok(iface::scalars(st, &[])?
        .into_iter()
        .filter(|p| !under_frozen(st, p, &frozen))
        .collect())
}

/// Erase every non-frozen scalar (they are about to be symbolized anyway).
pub fn blank<A: GraphArith>(st: &mut State<Symbolic<A>>, d: &mut Symbolic<A>) -> Result<()> {
    for p in state_paths(st)? {
        let v = match iface::get(st, &p) {
            Some(Value::Bool(_)) => Value::Bool(d.boolean(false)),
            _ => Value::Num(d.int(0)),
        };
        iface::set(st, &p, v)?;
    }
    // The path condition belongs to the frame that produced this state: a
    // stale `guard` would put the previous frame's input cells into this
    // frame's outputs.
    st.guard = d.boolean(true);
    st.ended = d.boolean(false);
    Ok(())
}

/// Which room a state is in. Concrete: `room` is frozen as an input and
/// `load_room` only ever writes it a constant.
pub fn room_of<A: GraphArith>(st: &State<Symbolic<A>>, d: &Symbolic<A>) -> (i16, i16) {
    let at = |k: &str| -> i16 {
        match iface::get(st, &[iface::key("room"), iface::key(k)]) {
            Some(Value::Num(n)) => {
                d.as_const(&n).and_then(A::as_i16).unwrap_or(-1)
            }
            _ => -1,
        }
    };
    (at("x"), at("y"))
}

/// The scalar fields of `st` that are compile-time constants (exact
/// `Const(v,v)` or `ConstBool`): what the per-shape constant lattice bakes in.
pub fn field_constants<A: GraphArith>(st: &State<Symbolic<A>>, d: &Symbolic<A>, opts: &crate::abstraction::Level) -> Result<std::collections::BTreeMap<Path, super::iface::Conc<A>>> {
    use super::domain::Domain;
    use super::iface::Conc;
    // The fields the level's widenings store as other than one value per
    // state are never constants, whatever a trace computes: baked, a widened
    // block would not bind.
    let widened: std::collections::BTreeSet<Path> = super::widen::slots_where(st, *opts, super::widen::stores_widened).into_iter().collect();
    // The buttons are dead at the boundary (every frame resets them before
    // reading): the start state's `false` is no constant of the shape.
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

/// The path of the player INSTANCE in `objects`, if the state has one.
pub fn player_path<D: Domain>(st: &State<D>) -> Option<Path> {
    super::widen::objects_of_type(st, "player").into_iter().next()
}
