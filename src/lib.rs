
pub mod compiled;
pub mod frame;
pub mod concrete;
pub mod metrics;
pub mod search;
pub mod trace;
pub mod transpile;

// The leaf types moved DOWN to `celeste-core` (task #150) so the engine
// crates can have PICO-8 numbers and the cart without depending on the
// interpreter. Re-exported at the old paths on purpose: `crate::pico8_num`
// and `celeste_rust::pico8_num` still resolve, so the move is invisible to
// the ~2,000 call sites and to anything that reads a fingerprint.
pub use celeste_core::{cart_data, collision_cache, pico8_num};

// The IR crate is gone (the CFG and its Lua frontend went with the
// recipe/compile pipeline). The only survivors were the builtin-name ABI
// and the two id newtypes, folded down into `celeste-core`. Re-exported at
// the old paths: `crate::builtins::...` and `crate::ir::GlobalId` keep
// resolving.
pub use celeste_core::{builtins, ids as ir};

// The interpreter - the ORACLE - and the game setup and instrumentation
// that travel with it now live in `celeste-interp`, so the rewrites, the
// emitters and the search can depend on it without any of them being in
// the same compilation unit (plans/build-time.md). Re-exported at the old
// paths.
pub use celeste_interp::{ game_runner, interpreter };
