//! The block engine: the `(shape, rows)` data model and the lane runtime the
//! generated kernels are emitted against.
//!
//! Two modules, and the split between them is the split between GENERIC and
//! GENERATED. `runtime2` is the block itself - `Rt2`'s structure/columns, the
//! boundary abstraction, dedup, k-way merge, retain, row keys, the shape hash
//! and the pm1 partitioning. `kernel` is the typed per-lane primitives
//! (`W = 16` rows, static types, deopt as a mask) that `celeste-kernels`'
//! emitted straight-line code calls.
//!
//! What this crate deliberately does NOT know about is the interpreter. It
//! reaches down to `celeste-core` for PICO-8 numbers, the cart and the
//! collision cache, and down to `celeste-names` for `FIELD_NAMES`, and that
//! is the whole of its dependencies - which is what makes it callable from
//! both the forward search and the backward sweep (task #150). Translating
//! an interpreter `State` into a block is the BRIDGE, and the bridge lives
//! up in `celeste-rust` where the interpreter types are.

pub mod kernel;
pub mod runtime2;
pub mod slots;

pub use runtime2::{Cell2, Col, Rt2, AV, NONE};

/// The hasher the row machinery keys with, re-exported because it is part
/// of this crate's ABI rather than an implementation detail: callers that
/// build the dedup and census maps `runtime2` consumes have to use the
/// same one, and rustc-hash 1 and 2 hash differently. celeste-rust is on
/// 1.x for its own maps, so without this the frame interface would quietly
/// key with the other hasher.
pub use rustc_hash::FxHashMap;
