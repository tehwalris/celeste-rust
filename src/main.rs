// TODO don't allow later
#![allow(dead_code)]
#![allow(unused_variables)]

use anyhow::Result;
use cart_data::CartData;

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

mod cart_data;
mod frontend;
mod input;
mod ir;
mod pico8_num;
mod tas;

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;

    Ok(())
}
