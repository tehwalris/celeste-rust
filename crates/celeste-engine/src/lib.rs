//! The block engine: the `(shape, rows)` data model and the lane runtime.
//! `runtime2` is the block itself (structure and columns, the boundary,
//! dedup, row keys, the shape hash); `kernel` the typed 16-lane primitives
//! (the reference semantics the ASM codegen is tested against, and its
//! call-outs). The crate does not know the interpreter: translating states
//! into blocks is `trace::block` in `celeste-rust`.

pub mod exact;
pub mod kernel;
pub mod runtime2;
pub mod slots;
pub mod widening;

pub use runtime2::{Cell2, Col, Rt2, AV, NONE};

/// The hasher the row machinery keys with. Part of this crate's ABI: maps
/// passed into `runtime2` must use the same one (rustc-hash 1 and 2 differ).
pub use rustc_hash::FxHashMap;
