//! The FAST interpreter: the reference engine's semantics over a resolved
//! program (names to small integers, locals to slots, builtins to an enum)
//! and a flat heap, so a path costs nanoseconds per node instead of
//! microseconds.
//!
//! `lower` turns the cart's AST into a `program::Program` once; `exec` runs
//! one path over it with `RefDomain` doing the interval arithmetic and the
//! forking; `bridge` converts a boundary `State` lane to a `heap::Heap` and
//! back; `engine` packages the DFS over the fork tree as `FastEngine`.
//! Bit-identical leaves to `cengine::refengine::RefEngine` are the contract
//! (`fiber_fwd` is the differential).

pub mod bridge;
pub mod engine;
pub mod exec;
pub mod heap;
pub mod lower;
pub mod program;
