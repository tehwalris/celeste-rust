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

use crate::trace::interp::Interp;
use crate::trace::refdomain::{Cursor, RefDomain};
use crate::trace::state::State;
use crate::trace::verify::run_one;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::{cart, verify::find_player};

    /// Smoke: run the real cart forward through RefDomain to where the player
    /// exists, then enumerate one real frame's successor states. Exercises the
    /// whole domain - arithmetic, collision, control flow, the six buttons,
    /// and any floor-splits - on the actual game. Not a correctness gate yet
    /// (that needs the row-key extraction), but proves RefDomain runs a real
    /// frame end to end and the fork enumeration terminates.
    #[test]
    #[ignore] // needs the cart on disk; run explicitly
    fn refdomain_runs_a_real_frame_end_to_end() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse top");
        let init = full_moon::parse("_init()").expect("parse _init");
        let body = full_moon::parse("__reset_button_states()\n_update()\n_draw()")
            .expect("parse body");

        let cd = Arc::new(CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        let cache = Arc::new(CollisionCache::new(&cd, rx, ry).expect("cache"));

        let mut it = fresh_interp(cd.clone(), cache.clone());
        let st = cart::fresh_state::<RefDomain>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");

        // Warm up to where the player object exists (the intro sequence),
        // driving buttons as a single fixed path (the cursor's first leaf).
        for _ in 0..40 {
            if find_player(&st).is_some() {
                break;
            }
            let outs = run_frame_all(&mut it, &body, &st).expect("warmup frame");
            st = outs.into_iter().next().expect("at least one successor");
        }
        assert!(find_player(&st).is_some(), "player never appeared in warm-up");

        let outs = run_frame_all(&mut it, &body, &st).expect("frame");
        assert!(!outs.is_empty(), "a frame produced no successors");
        eprintln!("[refdriver] one frame -> {} successor states", outs.len());
    }
}
