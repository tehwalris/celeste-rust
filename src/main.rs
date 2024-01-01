use anyhow::Result;
use cart_data::CartData;

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
#[macro_use(lazy_static)]
extern crate lazy_static;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

mod cart_data;
mod input;
mod pico8_num;
mod tas;

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;

    Ok(())
}
