
#[macro_use(anyhow)]
extern crate anyhow;

pub mod block_coverage;
pub mod branch_sites;
pub mod create_sites;
pub mod block_flow;
pub mod builtins;
pub mod compiled;
pub mod concrete;
pub mod frontend;
pub mod game_runner;
pub mod instr_time;
pub mod instruction_flow;
pub mod interpreter;
pub mod ir;
pub mod liveness;
pub mod merge_stats;
pub mod op_census;
pub mod metrics;
pub mod rewrite;
pub mod transpile;

// The leaf types moved DOWN to `celeste-core` (task #150) so the engine
// crates can have PICO-8 numbers and the cart without depending on the
// interpreter. Re-exported at the old paths on purpose: `crate::pico8_num`
// and `celeste_rust::pico8_num` still resolve, so the move is invisible to
// the ~2,000 call sites and to anything that reads a fingerprint.
pub use celeste_core::{cart_data, collision_cache, pico8_num};
