//! `RefEngine`: the reference interpreter (`Interp<RefDomain>`) as a frame
//! step on blocks. One lane in, every fork leaf out as a keyed one-row block,
//! so a reference successor keys like a kernel row. Slow by design (one lane,
//! one path at a time).

use anyhow::{ensure, Result};
use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::Rt2;
use full_moon::ast;

use crate::frame::Block;
use crate::abstraction::{current_level, Level};
use crate::trace::interp::Interp;
use crate::trace::refbridge::{fn_info_of, from_block, set_buttons, to_block, FnInfo};
use crate::trace::refdomain::RefDomain;
use crate::trace::refdriver::{fresh_interp, run_frame_all};
use crate::trace::state::State;
use crate::trace::verify::run_one;

pub struct RefEngine {
    it: Interp<'static, RefDomain>,
    /// `__reset_button_states();_update();_draw()`: the buttons are forks.
    body: &'static ast::Ast,
    /// `_update();_draw()` with no reset: the caller has set the buttons.
    body_concrete: &'static ast::Ast,
    /// The post-`_init` state: the room's start, and what a block does not carry.
    base: State<RefDomain>,
    fn_info: FnInfo,
}

// The raw pointers inside (`Interp`'s AST references) point into the
// `'static` ASTs leaked in `new`, immutable and never freed, so moving the
// engine to another thread is sound.
unsafe impl Send for RefEngine {}

impl RefEngine {
    /// Parse the cart, run its toplevel and `_init` for the start room.
    pub fn new() -> Result<Self> {
        let leak = |src: &str| -> Result<&'static ast::Ast> { Ok(Box::leak(Box::new(full_moon::parse(src)?))) };
        let top = leak(&crate::trace::cart::sources()?)?;
        crate::trace::cart::check_absent_fields(top)?;
        let init = leak("_init()")?;
        let body = leak("__reset_button_states()\n_update()\n_draw()")?;
        let body_concrete = leak("_update()\n_draw()")?;

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

    /// The room's start (post-`_init`) as a one-row block, without keys.
    pub fn initial(&self) -> Result<Rt2> {
        to_block(&self.base)
    }

    /// One CONCRETE frame of row 0 of `row` under input `byte`: every leaf,
    /// keyed. It forks only where no input decides (`rnd`); unknown fields of
    /// the row are read as their placeholder, not forked.
    pub fn step(&mut self, row: &Rt2, byte: u8) -> Result<Vec<Block>> {
        let (mut st, _) = from_block(row, 0, &self.fn_info, &self.base)?;
        set_buttons(&mut st, byte)?;
        let leaves = run_frame_all(&mut self.it, self.body_concrete, &st, &[], Level::EXACT)?;
        leaves.iter().map(|l| Block::keyed(to_block(l)?)).collect()
    }

    /// One whole GAME frame: `step` once, or under the split frame
    /// (`frame::steps_per_frame`) its two parts, both under `byte` (the
    /// buttons are the frame's; part a reads none). Every leaf of both.
    pub fn frame(&mut self, row: &Rt2, byte: u8) -> Result<Vec<Block>> {
        let mut out = self.step(row, byte)?;
        for _ in 1..crate::frame::steps_per_frame() {
            let mut next = Vec::new();
            for b in &out {
                next.extend(self.step(b.rt2(), byte)?);
            }
            out = next;
        }
        Ok(out)
    }

    /// `step` where the frame must not fork: the one successor.
    pub fn step_one(&mut self, row: &Rt2, byte: u8) -> Result<Block> {
        let mut out = self.step(row, byte)?;
        ensure!(out.len() == 1, "concrete frame produced {} leaves (expected exactly 1)", out.len());
        Ok(out.remove(0))
    }

    /// One frame of lane `lane` at the current level: every fork leaf (the
    /// buttons, and each unknown boolean of the row, are forked per path).
    pub fn run_lane(&mut self, block: &Rt2, lane: usize) -> Result<Vec<Block>> {
        let (st, unknown) = from_block(block, lane, &self.fn_info, &self.base)?;
        let leaves = run_frame_all(&mut self.it, self.body, &st, &unknown, current_level())?;
        leaves.iter().map(|l| Block::keyed(to_block(l)?)).collect()
    }
}

/// The interpreter as the trusted `FrameStep`, behind a lock (the `Interp` is
/// reused). Each fork leaf is one row, emitted with its source lane's cell.
impl crate::frame::FrameStep for std::sync::Mutex<RefEngine> {
    fn run(
        &self,
        block: &Block,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> Result<()> {
        let mut engine = self.lock().expect("reference engine lock");
        for lane in lanes {
            for b in engine.run_lane(block.rt2(), lane)? {
                let cell_out = b.positions()?[0];
                if sink.edges_on {
                    sink.edges.insert((cell_in[lane], cell_out));
                }
                sink.emit_row(b.rt2(), cell_out)?;
            }
        }
        Ok(())
    }
}
