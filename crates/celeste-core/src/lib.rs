//! The leaf types every other crate needs: the game's arithmetic (`arith`:
//! PICO-8's 16.16 `Pico8Num`, the tools' Lua doubles `LuaF64`), the cart's
//! map and flag tables (`CartData`) and a room's tile lookup (`room::Room`).
//! Nothing here knows the IR, the interpreter or the engine.

#[macro_use(anyhow)]
extern crate anyhow;

pub mod arith;
pub mod builtins;
pub mod cart_data;
pub mod lua_f64;
pub mod pico8_num;
pub mod room;
