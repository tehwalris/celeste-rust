// TODO don't allow later
#![allow(dead_code)]
#![allow(unused_variables)]

#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

pub mod block_flow;
pub mod cart_data;
pub mod collision_cache;
pub mod fixed_point;
pub mod frontend;
pub mod game_runner;
pub mod input;
pub mod instruction_flow;
pub mod interpreter;
pub mod ir;
pub mod liveness;
pub mod pico8_num;
pub mod tas;
