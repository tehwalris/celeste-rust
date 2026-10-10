//! The block engine: the `(shape, rows)` data model and the lane runtime.
//!
//! `runtime2` is the block itself - `Rt2`'s structure and columns, the
//! boundary abstraction, dedup, merge, retain, row keys and the shape hash.
//! `kernel` holds the typed 16-lane primitives: the reference semantics the
//! ASM codegen is tested against, and the call-outs the assembled kernels make.
//!
//! This crate does not know about the interpreter: it depends only on
//! `celeste-core` (numbers, cart, collision cache) and `celeste-names`
//! (`FIELD_NAMES`). Translating reference states into blocks is
//! `trace::refbridge` in `celeste-rust`.

pub mod exact;
pub mod kernel;
pub mod runtime2;
pub mod slots;
pub mod widening;

pub use runtime2::{Cell2, Col, Rt2, AV, NONE};

/// The hasher the row machinery keys with. Part of this crate's ABI: maps
/// passed into `runtime2` must use the same one (rustc-hash 1 and 2 differ).
pub use rustc_hash::FxHashMap;
