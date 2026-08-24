//! The generated name tables, and nothing else.
//!
//! `gen.rs` is CHECKED IN and FROZEN. It was written by the IR walk
//! `transpile::names` from the rewritten `rewrites-compile.jsonl` program
//! (last regenerated in cfe8680, 2026-08-18); that walk was deleted with
//! the walk kernels (plans/delete-the-interpreter.md Phase 1), so the file
//! is now edited by hand, APPEND-ONLY, and only if the Lua grows a name.
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
//! is a different search, not a cosmetic change.

pub mod gen;

pub use gen::{field_id, global_id, FIELD_NAMES, FN_NAMES, GLOBAL_NAMES, STRINGS};
