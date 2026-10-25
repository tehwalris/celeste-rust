//! The block engine: the `(shape, rows)` data model. `runtime2` is the
//! block itself (structure and columns, the boundary, dedup, row keys, the
//! shape hash). The crate does not know the interpreter: translating states
//! into blocks is `trace::block` in `celeste-rust`.

pub mod exact;
pub mod runtime2;
pub mod shape;
pub mod slots;
pub mod widening;

pub use runtime2::{Col, ColRepr, Rt2, Template, AV, NONE};
pub use shape::{Cell, Shape};

/// The hasher the row machinery keys with. Part of this crate's ABI: maps
/// passed into `runtime2` must use the same one (rustc-hash 1 and 2 differ).
pub use rustc_hash::FxHashMap;
