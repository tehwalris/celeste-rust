
pub mod abstraction;
pub mod check;
pub mod compiled;
pub mod frame;
pub mod game_runner;
pub mod concrete;
pub mod engines;
pub mod metrics;
pub mod par;
pub mod search;
pub mod settings;
pub mod starts;
pub mod storage;
pub mod tools;
pub mod trace;
pub mod transpile;
pub mod ui_export;

#[cfg(test)]
mod conventions;
#[cfg(test)]
mod testutil;

// The leaf types live in `celeste-core` (so the engine crates can use them);
// re-exported here so `celeste_rust::pico8_num` etc. resolve.
pub use celeste_core::{arith, pico8_num};

pub use celeste_core::builtins;

