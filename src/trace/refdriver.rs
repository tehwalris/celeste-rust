//! The frame driver for the reference interpreter (`refdomain::RefDomain`).
//!
//! `Interp<RefDomain>` runs ONE scalar path of a frame; `run_frame_all`
//! enumerates the fork tree depth-first by re-execution (run to a leaf,
//! record the output, `cursor.advance()`, re-run). The outputs are the
//! frame's successor set for one input state. The cursor is the only state
//! carried across reruns: the input is cloned per path, so nothing is
//! snapshotted or backtracked.

use anyhow::{bail, Result};
use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use full_moon::ast;

use crate::trace::interp::Interp;
use crate::trace::refdomain::{Cursor, RefDomain};
use crate::trace::state::State;

/// An `Interp<RefDomain>` with the room's cart and collision cache. The caller
/// runs the cart toplevel and `_init` on it once, then reuses it every frame.
pub fn fresh_interp<'a>(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Interp<'a, RefDomain> {
    let mut it = Interp::new(RefDomain::new());
    it.cart = Some(cart);
    it.cache = Some(cache);
    it
}

/// Run one frame of `body` from `input` along every fork path; one output state
/// per leaf. For a game frame `body` is
/// `__reset_button_states()\n_update()\n_draw()`, so the buttons are forks too.
///
/// `it` must have the cart's functions registered; its `d.cursor` is the DFS
/// state. A path that REFUSES (e.g. a fork premise a lane cannot satisfy) is
/// an error; a path that RAISES has no successor.
///
/// `level` is passed in, not read from the process-global level: a concrete
/// step (`RefEngine::step`) is exact whatever the search has set.
///
/// `unknown` names the input's unknown-boolean fields (`refbridge::from_block`):
/// each is a cursor choice made before the frame runs, as the kernels fork them.
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
    // Unknown fly fruit and platforms have no reference form yet.
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
        // A path that RAISES (`Interp::poison`) ends in no state, and only then.
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
