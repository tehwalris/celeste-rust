//! The builtin ABI: the one thing that survived the scalar runtime.
//!
//! This file used to be `runtime.rs`, a concrete-lane interpreter for the
//! transpiled IR (task #124) that existed to be a hex-exact oracle for the
//! `Engine`-trait emission. Both are gone: the celeste-rust interpreter is
//! now the only reference implementation of a frame (K4 stage 2) and the
//! generated kernels are the only compiled one, so a third executor was
//! only a third thing to keep in agreement.
//!
//! What is left is an ABI. `BUILTIN_NAMES` is index-to-name, and the index
//! is what `Cell2::Bi` stores, what the transpiler emits, and what
//! `import`/`export` translate through in both directions - so the ORDER is
//! load-bearing in exactly the way `FIELD_NAMES`' order is. The names and
//! their semantics come from `game_runner.rs`
//! `create_fixed_env_with_(game_)builtins`.
//!
//! It used to be TWO tables that had to be kept in agreement by hand - one
//! here (then in the probe), one in `src/bin/transpile/main.rs` - which the
//! P1 plan flagged as a loose end. Both consumers turned out to live in
//! this crate (the transpiler emits the ids, `compiled::bridge` translates
//! them), so the fix was to keep one table and delete the other rather than
//! to assert the two agree.

pub const BUILTIN_NAMES: [&str; 18] = [
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
];
