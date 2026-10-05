//! The leaf types every other crate needs: PICO-8 arithmetic
//! (`Pico8Num`/`Pico8NumInterval`), the cart's map and flag tables
//! (`CartData`) and the per-room tile lookup (`CollisionCache`).
//!
//! The bottom of the workspace: nothing here knows the IR, the interpreter or
//! the engine. `celeste-rust` re-exports these modules at its own root.

#[macro_use(anyhow)]
extern crate anyhow;

pub mod builtins;
pub mod cart_data;
pub mod collision_cache;
pub mod pico8_num;
