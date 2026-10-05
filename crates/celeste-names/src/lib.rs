//! The name tables, and nothing else.
//!
//! `gen.rs` is checked in and FROZEN: its generator is gone, so it is edited
//! by hand, APPEND-ONLY, and only if the Lua grows a name. It is its own crate
//! because `celeste-engine` needs `FIELD_NAMES` below it.
//!
//! The ORDER is load-bearing: `FIELD_NAMES`' order is the field ordering
//! `Cell2::Obj` interns against, so it feeds the shape hash and the row key
//! the search dedups on. A reorder is a different search.

pub mod gen;

pub use gen::{field_id, global_id, FIELD_NAMES, FN_NAMES, GLOBAL_NAMES, STRINGS};
