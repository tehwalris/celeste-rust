
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

pub mod block_coverage;
pub mod branch_sites;
pub mod create_sites;
pub mod block_flow;
pub mod cart_data;
pub mod collision_cache;
pub mod frontend;
pub mod game_runner;
pub mod input;
pub mod instr_time;
pub mod instruction_flow;
pub mod interpreter;
pub mod ir;
pub mod liveness;
pub mod merge_stats;
pub mod op_census;
pub mod pico8_num;
pub mod rewrite;
pub mod tas;
