//! The builtin ABI: index-to-name, folded down here when `celeste-ir` was
//! deleted.
//!
//! `BUILTIN_NAMES` is index-to-name, and the index is what `Cell2::Bi`
//! stores, what the tracer's bridge emits, and what `import`/`export`
//! translate through in both directions - so the ORDER is load-bearing in
//! exactly the way `FIELD_NAMES`' order is. The names and their semantics
//! come from the reference interpreter's builtin set.

pub const BUILTIN_NAMES: [&str; 19] = [
    "__print",
    "__new_unknown_boolean",
    "__widen_rem",
    "__new_vector",
    "__array_table_drop_last",
    "error",
    "min",
    "max",
    "abs",
    "flr",
    "__split_by_flr",
    "__split_at",
    "add",
    "print",
    "sin",
    "mget",
    "fget",
    "tile_flag_at",
    // APPENDED 2026-09-16: `rnd`, an interval draw in the tracer (room
    // (5,0)'s balloon, the chest). Appended so every index above is unchanged.
    "rnd",
];
