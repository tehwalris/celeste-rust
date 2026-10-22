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

/// The id of global `name`, which the code names as one of the frozen
/// table's: a name it lacks is a bug of the caller's.
pub fn global(name: &str) -> u32 {
    global_id(name).unwrap_or_else(|| panic!("`{name}` is not a frozen name of a global (celeste_names::gen)"))
}

/// The id of field `name` (`global`'s for the fields).
pub fn field(name: &str) -> u32 {
    field_id(name).unwrap_or_else(|| panic!("`{name}` is not a frozen name of a field (celeste_names::gen)"))
}
