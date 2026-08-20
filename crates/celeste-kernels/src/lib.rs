//! The generated class kernels, and nothing hand-written.
//!
//! One module per player class - `steady` (shape (player), pm1 freeze=0
//! dash_time=0), `dash`, `frozen` - each emitted by `transpile --kernel
//! WITNESS OUT` from a certified trace overlay plus a class-filtered shape
//! witness. Together they cover 100% of room (1,0)'s player lanes; anything
//! they decline falls back to the celeste-rust interpreter, which is the
//! reference (K4 stage 2), so a gap is slow rather than wrong.
//!
//! CHECKED IN, like `celeste-names`' tables, and for the same reason: a
//! build.rs would have to run `transpile`, which needs the rewrite
//! machinery from a crate that sits ABOVE this one. Staleness is caught by
//! a test that regenerates and compares instead. Regenerating is:
//!
//! ```text
//! for c in steady dash frozen; do
//!   ./target/release/transpile --recipe rewrites-trace10-$c.jsonl \
//!       --kernel native-probe/$c-shape.json \
//!       crates/celeste-kernels/src/kernel_gen_$c.rs
//! done
//! ```
//!
//! This crate sits ABOVE `celeste-engine` because the emitted code calls
//! `celeste_engine::kernel`'s lane primitives and builds
//! `celeste_engine::runtime2::Rt2` blocks - while the engine reaches DOWN to
//! `celeste-names` for `FIELD_NAMES`. That is why the generated code is two
//! crates and not one: in one crate those two directions are a cycle.

pub mod kernel_gen_dash;
pub mod kernel_gen_frozen;
pub mod kernel_gen_steady;

// Room (2,0) class kernels: shape (fruit, spring, spring, player), the
// dominant witness shape hash per class. Same emitter, the trace20
// overlays; the springs add two per-lane row columns (delay, spr) and
// the fruit is fully block-uniform (the #170 boundary widening).
pub mod kernel_gen_r20_dash;
pub mod kernel_gen_r20_frozen;
pub mod kernel_gen_r20_steady;

/// The fused specialization-set artifact (plans/shape-tag-plan.md): the
/// steady member plus the dying members in one kernel, per-member deopt.
/// GENERATED PER CAMPAIGN (`transpile --fuse`), gitignored, feature-gated:
/// unlike the class kernels above there is no single canonical member
/// list to check in, and the staleness gate does not cover it.
#[cfg(feature = "fused")]
pub mod fused_gen_player;
