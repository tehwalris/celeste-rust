// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
#[macro_use(lazy_static)]
extern crate lazy_static;
extern crate hex;
extern crate regex;

mod cart_data;
mod game;
mod pico8_num;
mod player_flags;

fn main() {
    println!("Hello, world!");
}
