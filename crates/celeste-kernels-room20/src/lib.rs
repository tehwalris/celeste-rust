//! Room (2,0)'s generated CONSTANT-LATTICE kernels, and nothing
//! hand-written.
//!
//! One crate per room exists so that touching this room's generated
//! kernels only recompiles THIS crate, not every room's - the old
//! single `celeste-kernels` crate held all three rooms' generated code
//! (hundreds of thousands of lines) as one compilation unit, so a
//! one-line change to a room (2,0) kernel forced a full relink of room
//! (0,0) and (1,0) too. `crates/celeste-kernels` is now a thin
//! aggregator over this crate and its room00/room10 siblings.
//!
//! Three variant modules (`traced`/`ladder`/`exact`, see
//! `crates/celeste-kernels/src/lib.rs` for what each means), each
//! holding this room's `room20` submodule: one kernel per heap shape
//! the room reaches, plus that room's `KERNELS` table and content-hash
//! `FINGERPRINT`. Regenerate with `./regen-generated.sh`; staleness is
//! caught by `trace::kernel::tests::{traced,ladder,exact}_kernels_are_current`
//! and the `#[ignore]`d `room20_kernels_are_current`.
//!
//! Sits ABOVE `celeste-engine` for the same reason the aggregator does:
//! the emitted code calls `celeste_engine::kernel`'s lane primitives and
//! builds `celeste_engine::runtime2::Rt2` blocks.

pub mod traced;
pub mod ladder;
pub mod exact;
