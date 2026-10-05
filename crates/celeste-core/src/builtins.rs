//! The builtin ABI: `BUILTIN_NAMES` is index-to-name, and the index is what
//! `Cell2::Bi` stores (the tracer's `bind` and `refbridge` both write it), so
//! the ORDER is load-bearing in exactly the way `FIELD_NAMES`' order is.

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
    // Appended, so every index above is unchanged.
    "rnd",
];
