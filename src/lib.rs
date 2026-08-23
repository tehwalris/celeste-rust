
pub mod compiled;
pub mod concrete;
pub mod metrics;
pub mod rewrite;
pub mod trace;
pub mod transpile;

// The leaf types moved DOWN to `celeste-core` (task #150) so the engine
// crates can have PICO-8 numbers and the cart without depending on the
// interpreter. Re-exported at the old paths on purpose: `crate::pico8_num`
// and `celeste_rust::pico8_num` still resolve, so the move is invisible to
// the ~2,000 call sites and to anything that reads a fingerprint.
pub use celeste_core::{cart_data, collision_cache, pico8_num};

// The IR, the Lua frontend, the builtin table and the printer moved to
// `celeste-ir` so that the interpreter and the emitters can have them
// without the rewrite machinery (plans/build-time.md). Re-exported at the
// old paths: `crate::ir::...` keeps resolving everywhere.
pub use celeste_ir::{builtins, frontend, ir};

// The interpreter - the ORACLE - and the game setup and instrumentation
// that travel with it now live in `celeste-interp`, so the rewrites, the
// emitters and the search can depend on it without any of them being in
// the same compilation unit (plans/build-time.md). Re-exported at the old
// paths.
pub use celeste_interp::{ block_flow, game_runner, instr_time,
    instruction_flow, interpreter, liveness,
};
