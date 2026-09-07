//! The frame driver for the reference interpreter (`refdomain::RefDomain`).
//!
//! `Interp<RefDomain>` runs ONE scalar path of a frame (`run_one`). This
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

use crate::cengine::domain::Domain;
use crate::cengine::interp::{Flow, Interp};
use crate::cengine::refdomain::{Cursor, RefDomain};
use crate::cengine::state::State;

/// Run `ast` as one chunk on `st`, requiring exactly one outgoing state and
/// no `break` at the top level. (Copied from `trace::verify::run_one`.)
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
/// cannot satisfy) is a real coverage answer, not a crash - but for now it
/// propagates so the gate sees it; the driver will classify refusals once the
/// row-key extraction lands.
pub fn run_frame_all<'a>(
    it: &mut Interp<'a, RefDomain>,
    body: &'a ast::Ast,
    input: &State<RefDomain>,
) -> Result<Vec<State<RefDomain>>> {
    it.d.cursor = Cursor::new();
    let mut outputs = Vec::new();
    let mut paths = 0usize;
    loop {
        it.d.cursor.reset();
        it.prints.clear();
        let out = run_one(it, body, input.clone())?;
        outputs.push(out);
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
