//! The generated name tables, and nothing else.
//!
//! `gen.rs` is written by `cargo run --release --bin transpile -- --recipe
//! rewrites-compile.jsonl` and is CHECKED IN. It is not built by a build.rs
//! on purpose: generating it needs the whole rewrite machinery, which lives
//! in `celeste-rust`, which depends on this crate - so a build script here
//! would be a bootstrap cycle. Instead it is committed and a test
//! (`tests/generated_is_current.rs` in celeste-rust) regenerates it and
//! compares, which is the same shape as the recipe-replay test.
//!
//! Why it is its OWN crate, below the engine: `celeste-engine` reads
//! `FIELD_NAMES` (it is the canonical field ordering the boundary hashes),
//! while the generated kernels read `celeste_engine::{Rt2, Col, AV}`. Put
//! both generated modules in one crate and that is a cycle. Splitting them
//! by direction - name tables BELOW the engine, kernels ABOVE it - is
//! forced by the dependency graph, not a matter of taste.
//!
//! The ORDER of these tables is load-bearing. `FIELD_NAMES`' order is the
//! field ordering `Cell2::Obj` interns against, so it feeds the shape hash,
//! which feeds the row key, which is what the search dedups on: a reorder
//! is a different search, not a cosmetic change. The gate on any change to
//! the emitter is that these tables come out byte-identical.

pub mod gen;

pub use gen::{field_id, global_id, FIELD_NAMES, FN_NAMES, GLOBAL_NAMES, STRINGS};
