
pub mod abstraction;
pub mod compiled;
pub mod frame;
pub mod game_runner;
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

pub use celeste_core::builtins;

