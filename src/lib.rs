
pub mod abstraction;
pub mod compiled;
pub mod frame;
pub mod game_runner;
pub mod concrete;
pub mod metrics;
pub mod search;
pub mod trace;
pub mod transpile;

// The leaf types live in `celeste-core` (so the engine crates can use them);
// re-exported here so `celeste_rust::pico8_num` etc. resolve.
pub use celeste_core::{cart_data, collision_cache, pico8_num};

pub use celeste_core::builtins;

