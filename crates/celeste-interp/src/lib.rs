//! The interpreter's `State` model (heap, values, state), the ladder's
//! levels (`interpreter::abstraction`), and the game setup (`game_runner`:
//! the start room, the Lua sources).
//!
//! The reference engine itself is `trace::refengine` in celeste-rust; this
//! crate is the state representation it and the bridge to the block model
//! (`compiled::bridge`) speak. The process-global level
//! (`abstraction::set_level`) is why the test suite runs under nextest (one
//! process per test) rather than cargo test.

#[macro_use(anyhow)]
extern crate anyhow;

// Re-exported at the paths the moved code already uses, so `crate::ir::..`
// and `crate::pico8_num::..` keep resolving inside this crate. The same
// trick `celeste-rust` uses for `celeste-core`: it makes the move
// invisible to a few thousand call sites, including grouped imports like
// `use crate::{ir::GlobalId, pico8_num::Pico8Num}` that no mechanical
// rewrite handles cleanly.
//
// The IR crate is gone; the only survivors the interpreter still names are
// the two id newtypes, folded down into `celeste-core::ids`. `crate::ir`
// stays as their path so those grouped imports keep resolving.
pub use celeste_core::{cart_data, collision_cache, pico8_num};
pub use celeste_core::ids as ir;

pub mod game_runner;
pub mod interpreter;
