//! `RefEngine`: the reference interpreter as a frame step - one multi-lane
//! boundary `State` in, its frame successors out, keyed identically to the
//! kernels (the gate proves it).
//!
//! It owns the parsed cart ASTs (leaked to `'static` so the `Interp` can
//! borrow the function bodies for its whole life) and a base state with the
//! cart's functions registered, and runs each input lane through the bridge:
//! `to_trace_state` -> `run_frame_all` (DFS over the fork tree) ->
//! `to_interp_state`. Slow by design (per-lane, no vectorization); this is the
//! REFERENCE, so callers that run it on many lanes sample.

use anyhow::Result;
use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use full_moon::ast;

use crate::interpreter::state::State as OState;
use crate::trace::interp::Interp;
use crate::trace::refbridge::{
    add_missing_builtins, fn_info_of, patch_closures_for, to_interp_state, to_trace_state, FnInfo,
};
use crate::trace::refdomain::RefDomain;
use crate::trace::refdriver::{fresh_interp, run_frame_all};
use crate::trace::state::State as TState;
use crate::trace::verify::run_one;

pub struct RefEngine {
    it: Interp<'static, RefDomain>,
    body: &'static ast::Ast,
    /// The concrete frame body: `_update();_draw()` with NO button reset, so
    /// a caller that has already set the six buttons concretely gets one
    /// deterministic leaf (the concrete replay path: `concrete_run`, the TAS
    /// walks). The forking body (`body`) resets and re-forks the buttons.
    body_concrete: &'static ast::Ast,
    base: TState<RefDomain>,
    fn_info: FnInfo,
}

impl RefEngine {
    /// Set up the reference interpreter for the configured start room: parse
    /// the cart, register the function bodies, and capture the base state the
    /// bridge patches closures against.
    pub fn new() -> Result<Self> {
        let src = crate::trace::cart::sources()?;
        let top: &'static ast::Ast = Box::leak(Box::new(full_moon::parse(&src)?));
        let init: &'static ast::Ast = Box::leak(Box::new(full_moon::parse("_init()")?));
        let body: &'static ast::Ast = Box::leak(Box::new(full_moon::parse(
            "__reset_button_states()\n_update()\n_draw()",
        )?));
        let body_concrete: &'static ast::Ast =
            Box::leak(Box::new(full_moon::parse("_update()\n_draw()")?));

        let cart = Arc::new(CartData::load("cart")?);
        let (rx, ry) = crate::game_runner::start_room();
        let cache = Arc::new(CollisionCache::new(&cart, rx, ry)?);

        let mut it = fresh_interp(cart, cache);
        let st0 = crate::trace::cart::fresh_state::<RefDomain>(&mut it.d);
        let mut st0 = run_one(&mut it, top, st0)?;
        crate::trace::cart::inject_tile_flag_at(&mut st0);
        let base = run_one(&mut it, init, st0)?;
        let fn_info = fn_info_of(&base)?;

        Ok(RefEngine { it, body, body_concrete, base, fn_info })
    }

    /// The frame-0 state (post-`_init`, with the native `tile_flag_at`
    /// injected) as a one-lane old-interpreter boundary state - the
    /// RefEngine equivalent of `interpret_cfg(init_cfg, ...)`. Used to seed
    /// the search and the frame-0 blocks now that the CFG interpreter is
    /// gone. The bridge gate proves each subsequent frame reproduces the
    /// interpreter; the init is frame 0 of that same derivation.
    pub fn initial_state(&self) -> Result<OState> {
        crate::trace::refbridge::to_interp_state(&self.base)
    }

    /// One CONCRETE frame of a single-lane state whose six buttons the caller
    /// has already set (`concrete::set_concrete_buttons`). Runs
    /// `_update();_draw()` with no reset and no forking, so a fully concrete
    /// input yields exactly one successor - the drop-in for
    /// `concrete::step_frame`.
    pub fn run_frame_concrete(&mut self, input: &OState) -> Result<OState> {
        use anyhow::bail;
        if input.vector_size != 1 {
            bail!("run_frame_concrete expects one lane, got {}", input.vector_size);
        }
        let mut d = RefDomain::new();
        let mut bridged = to_trace_state(input, 0, &mut d)?;
        patch_closures_for(&mut bridged, &self.fn_info)?;
        add_missing_builtins(&mut bridged, &self.base);
        let leaves = run_frame_all(&mut self.it, self.body_concrete, &bridged)?;
        if leaves.len() != 1 {
            bail!("concrete frame produced {} leaves (expected exactly 1)", leaves.len());
        }
        to_interp_state(&leaves[0])
    }

    /// Run one frame of every lane of `input`, returning all successor states
    /// (across every lane and every fork path). The successors carry the same
    /// canonical row key `engine_row_keys` would compute.
    pub fn run_frame(&mut self, input: &OState) -> Result<Vec<OState>> {
        let mut out = Vec::new();
        for lane in 0..input.vector_size.max(1) {
            let mut d = RefDomain::new();
            let mut bridged = to_trace_state(input, lane, &mut d)?;
            patch_closures_for(&mut bridged, &self.fn_info)?;
            add_missing_builtins(&mut bridged, &self.base);
            for leaf in run_frame_all(&mut self.it, self.body, &bridged)? {
                out.push(to_interp_state(&leaf)?);
            }
        }
        Ok(out)
    }
}

/// The interpreter as the trusted `FrameStep` implementation (interface #1).
/// `&mut self` because the `Interp` is reused across lanes and paths; the
/// kernel engine ignores the mutability. Each fork leaf is one single-lane
/// output block - correct but unvectorized, which is exactly the reference's
/// contract (callers that run it wide sample).
impl crate::frame::FrameStep for RefEngine {
    fn run(&mut self, block: crate::frame::Block) -> Result<Vec<crate::frame::Block>> {
        // The reference runs on interpreter `State`s: cross the bridge both
        // ways. `Block::from_state` keys each leaf by the one canonical rule,
        // so the loop sees the same key column the kernels would attach.
        let input = block.to_state();
        self.run_frame(&input)?
            .iter()
            .map(crate::frame::Block::from_state)
            .collect()
    }
}
