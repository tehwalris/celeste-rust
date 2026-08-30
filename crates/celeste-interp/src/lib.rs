//! The reference interpreter, and the game setup it needs.
//!
//! This is the ORACLE: the thing the compiled path is checked against, and
//! ultimately the definition of what the program means. It sits above
//! `celeste-ir` and below everything that transforms or compiles - the
//! rewrites, the emitters and the search all depend on it, and it depends
//! on none of them.
//!
//! That direction used to be violated by exactly one call - the
//! interpreter formatting an instruction through `program::print` - which
//! is why the printer now lives in `celeste-ir`.
//!
//! The instrumentation modules travel with it because they instrument it:
//! `op_census` alone has ~100 call sites inside the interpreter. Several
//! of them keep process-global state, which is why the test suite has to
//! run under nextest (one process per test) rather than cargo test.

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
