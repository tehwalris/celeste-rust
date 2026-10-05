//! The frame driver for the reference interpreter (`refdomain::RefDomain`).
//!
//! `Interp<RefDomain>` runs ONE scalar path of a frame (`run_frame_all`). This
//! driver enumerates the whole fork tree by re-execution: run the frame to a
//! leaf, record the output state, `cursor.advance()` to the next path, and
//! re-run - a depth-first search (plans/kernel-boundary-and-deletion.md). The
//! set of output states is the frame's successor set for one input state.
//!
//! The cursor is the ONLY state carried across reruns; everything else - a
//! fresh `Interp`, a fresh clone of the input state - is rebuilt each path, so
//! there is no snapshot/backtrack machinery.

use anyhow::{bail, Result};
use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use full_moon::ast;

use crate::trace::interp::Interp;
use crate::trace::refdomain::{Cursor, RefDomain};
use crate::trace::state::State;

/// Build an `Interp<RefDomain>` with the room's cart + collision cache and a
/// fresh cursor. The caller runs the cart toplevel + `_init` on it once to
/// register the function bodies, then reuses it for every frame.
pub fn fresh_interp<'a>(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Interp<'a, RefDomain> {
    let mut it = Interp::new(RefDomain::new());
    it.cart = Some(cart);
    it.cache = Some(cache);
    it
}

/// Run one frame from `input`, enumerating every fork path, and return the set
/// of output states (one per leaf of the decision tree). `body` is the code to
/// run per path - for a real game frame that is
/// `__reset_button_states()\n_update()\n_draw()`, so the six buttons and every
/// internal straddle/floor-split are enumerated by the cursor.
///
/// `it` must already have the function bodies registered (run the cart toplevel
/// on it first) and carry the room's cart/cache; it is REUSED across paths, so
/// its `d.cursor` is the persistent DFS state. `input` is cloned per path.
///
/// A path that REFUSES (a poisoned/illegal frame, e.g. a fork premise a lane
/// cannot satisfy) propagates as an error.
///
/// `level` is the precision the frame runs at, passed in rather than read from
/// the process-global level: a concrete step (`RefEngine::step`) is exact
/// whatever level the search has set.
///
/// `unknown` names the input's fields that hold an unknown boolean
/// (`refbridge::from_block`): each is a cursor choice of both
/// values per path, made before the frame runs - as the kernels fork a held
/// trail, and read a fall floor's unknown `collideable`.
pub fn run_frame_all<'a>(
    it: &mut Interp<'a, RefDomain>,
    body: &'a ast::Ast,
    input: &State<RefDomain>,
    unknown: &[(crate::trace::heap::TableId, String)],
    level: crate::abstraction::Level,
) -> Result<Vec<State<RefDomain>>> {
    it.d.cursor = Cursor::new();
    let mut outputs = Vec::new();
    let mut paths = 0usize;
    // Nor the fly fruit and the moving platforms unknown: their ranges and
    // worlds have no reference form yet.
    if level.fruit {
        anyhow::bail!("the reference engine does not run fruit-unknown levels (plans/fly-fruit.md)");
    }
    if level.platforms {
        anyhow::bail!("the reference engine does not run platforms-unknown levels (plans/platforms-unknown.md)");
    }
    loop {
        it.d.cursor.reset();
        it.prints.clear();
        let mut st = input.clone();
        for (t, k) in unknown {
            let b = it.d.cursor.choose(2) == 1;
            st.heap.tables.get_mut(t).expect("an unknown field's table").hash.insert(k.clone(), crate::trace::heap::Value::Bool(b));
        }
        if level.floors_near {
            concretize_near_floors(&mut st, &mut it.d)?;
        }
        // A path that RAISES has no successor (`Interp::poison`; the nodiag
        // mode's diagonal dash is one): it ends in no state, and only then.
        it.raised.clear();
        let ended = it.exec_block(body.nodes(), st)?;
        match ended.len() {
            0 => anyhow::ensure!(!it.raised.is_empty(), "a frame path ended in no state without a raise"),
            1 => {
                let (mut out, flow) = ended.into_iter().next().expect("one state");
                anyhow::ensure!(!matches!(flow, crate::trace::interp::Flow::Break), "break at chunk toplevel");
                // The absent-as-zero fields, as the tracer's `trace_frame` writes them.
                crate::trace::widen::materialize_absent_fields(&mut out, &mut it.d)?;
                outputs.push(out);
            }
            n => bail!("a frame path ended in {n} states (expected one, or none after a raise)"),
        }
        paths += 1;
        if paths > 1_000_000 {
            bail!("run_frame_all: >1M paths - fork tree did not terminate");
        }
        if !it.d.cursor.advance() {
            break;
        }
    }
    Ok(outputs)
}

/// A near level's floors, concretized per path as the kernels read them
/// (`widen::fork_near_floor_inputs`): a widened `state` (an interval) is each
/// whole number in it, by the cursor, and `collideable` is `state ~= 2` - the
/// cart keeps the two in step, so a stored unknown `collideable` is not a
/// separate choice.
fn concretize_near_floors(st: &mut State<RefDomain>, d: &mut RefDomain) -> Result<()> {
    use crate::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};
    use crate::trace::heap::Value;
    use crate::trace::iface;
    use crate::trace::widen::{field, objects_of_type};
    for obj in objects_of_type(st, "fall_floor") {
        let (ps, pc) = (field(&obj, &["state"]), field(&obj, &["collideable"]));
        let Some(Value::Num(v)) = iface::get(st, &ps) else { bail!("{}: not a number", iface::show(&ps)) };
        let (lo, hi) = (v.low.as_i16_or_err()?, v.high.as_i16_or_err()?);
        let k = lo + d.cursor.choose((hi - lo + 1) as u32) as i16;
        iface::set(st, &ps, Value::Num(Iv::from_number(P8::from_i16(k))))?;
        iface::set(st, &pc, Value::Bool(k != 2))?;
    }
    Ok(())
}
