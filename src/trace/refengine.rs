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
        Ok(self.step_reads(row, byte)?.0)
    }

    /// `step`, and the mask of the buttons it READ (`Interp::button_reads`,
    /// over every fork path). The successors are a function of the row and
    /// of the buttons read alone - the output writes every button `UBool`
    /// (`refbridge`) - so an input that agrees with `byte` on the mask has
    /// the same successors.
    fn step_reads(&mut self, row: &Rt2, byte: u8) -> Result<(Vec<Block>, u8)> {
        let (mut st, _) = from_block(row, 0, &self.fn_info, &self.base)?;
        let buttons = set_buttons(&mut st, byte)?;
        self.it.button_reads = Some((buttons, 0));
        let leaves = run_frame_all(&mut self.it, self.body_concrete, &st, &[], Level::EXACT);
        let (_, mask) = self.it.button_reads.take().expect("set above");
        let out = leaves?.iter().map(|l| Block::keyed(to_block(l)?)).collect::<Result<_>>()?;
        Ok((out, mask))
    }

    /// One whole GAME frame: `step` once, or under the split frame
    /// (`frame::steps_per_frame`) its two parts, both under `byte` (the
    /// buttons are the frame's; part a reads none). Every leaf of both.
    pub fn frame(&mut self, row: &Rt2, byte: u8) -> Result<Vec<Block>> {
        Ok(self.frame_reads(row, byte)?.0)
    }

    /// `frame`, and the buttons it read (`step_reads`; the union over its
    /// steps and their leaves): every input that agrees with `byte` there
    /// has the same successors, so the concrete search runs one of them.
    pub fn frame_reads(&mut self, row: &Rt2, byte: u8) -> Result<(Vec<Block>, u8)> {
        let (mut out, mut mask) = self.step_reads(row, byte)?;
        for _ in 1..crate::frame::steps_per_frame() {
            let mut next = Vec::new();
            for b in &out {
                let (o, m) = self.step_reads(b.rt2(), byte)?;
                next.extend(o);
                mask |= m;
            }
            out = next;
        }
        Ok((out, mask))
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The concrete search runs one input per class of `frame_reads`
    /// (`arc_dp::Covered`): every input that agrees with a run input on the
    /// buttons that run read must make the same successors, exactly. Checked
    /// for all 64 inputs at states along room (1,0)'s 99-frame exit.
    #[test]
    fn inputs_agreeing_on_the_buttons_read_make_the_same_successors() {
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        let inputs = crate::concrete::read_inputs("tas/room_1_0_exit_frame_99.txt").expect("the exit's inputs");
        let mut eng = RefEngine::new().expect("ref engine");
        let exact = |bs: Vec<Block>| -> Vec<((u64, u64), u32)> { bs.iter().map(|b| (b.rt2().clone_block().row_keys_canonical()[0], b.positions().expect("cells")[0])).collect() };
        let mut row = eng.initial().expect("initial state");
        let (mut runs, mut checked) = (0, 0);
        for (f, &byte) in inputs.iter().enumerate() {
            if f % 4 == 0 {
                let mut ran: Vec<(u8, u8, Vec<((u64, u64), u32)>)> = Vec::new();
                for b in 0..64u8 {
                    let (succ, read) = eng.frame_reads(&row, b).expect("frame");
                    let succ = exact(succ);
                    if let Some((r, _, s)) = ran.iter().find(|(r, m, _)| (r ^ b) & m == 0) {
                        assert_eq!(&succ, s, "f{f}: input {b} agrees with input {r} on the buttons read, but not in its successors");
                        checked += 1;
                    } else {
                        ran.push((b, read, succ));
                    }
                }
                runs += ran.len();
            }
            row = eng.step_one(&row, byte).expect("frame").into_rt2();
        }
        // The point of it: most inputs are repeats (the spawn and freeze
        // frames read no button; up/down only matter where a dash starts).
        eprintln!("{runs} inputs run, {checked} covered");
        assert!(checked > 2 * runs, "{checked} inputs covered, {runs} run");
    }
}
