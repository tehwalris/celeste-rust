//! The leaf types every other crate needs: PICO-8 arithmetic and the cart.
//!
//! This is the bottom of the workspace (task #150). Nothing here knows about
//! the IR, the interpreter, the block engine or the kernels - it is
//! `Pico8Num`/`Pico8NumInterval` (the number semantics everything agrees on),
//! `CartData` (the map/flag tables read out of the cart) and
//! `CollisionCache` (the per-room tile lookup those two build).
//!
//! It exists because the engine crates need PICO-8 numbers and the cart but
//! must NOT depend on the interpreter; `celeste-rust` re-exports these three
//! modules at its own root, so `celeste_rust::pico8_num::...` keeps working
//! for the ~2,000 existing references.

#[macro_use(anyhow)]
extern crate anyhow;

pub mod cart_data;
pub mod collision_cache;
pub mod pico8_num;
