//! The generated TRACED kernels, and nothing hand-written.
//!
//! One module per heap SHAPE the start room reaches, emitted by
//! `transpile --room-kernels crates/celeste-kernels/src/traced` - the
//! tracer walks the room itself; there is no recipe and no witness.
//! Together they cover every lane of room (1,0) at the production horizon
//! (BENCHMARK_DATA.md 2026-08-24: f94, missed 0, plain-routed 0). A chunk
//! they decline is a coverage gap, not a degraded mode (CLAUDE.md "Never
//! deopt to the interpreter").
//!
//! CHECKED IN, like `celeste-names`' tables, and for the same reason: a
//! build.rs would have to run `transpile`, which needs the tracer from a
//! crate that sits ABOVE this one. Staleness is caught by
//! `traced_kernels_are_current`, which regenerates and compares byte for
//! byte. Regenerating is `./regen-generated.sh`.
//!
//! This crate sits ABOVE `celeste-engine` because the emitted code calls
//! `celeste_engine::kernel`'s lane primitives and builds
//! `celeste_engine::runtime2::Rt2` blocks - while the engine reaches DOWN to
//! `celeste-names` for `FIELD_NAMES`. That is why the generated code is two
//! crates and not one: in one crate those two directions are a cycle.
//!
//! The per-(room, player-class) "walk" kernels that used to sit beside
//! this set were deleted 2026-08-25 (plans/delete-the-interpreter.md
//! Phase 1) after taking 0 lanes at the production horizon.

pub mod traced;

/// The RUNG-AGNOSTIC set (plans/kernel-ladder.md): the same shapes,
/// traced with the boundary widenings left OUT of the graph, so a kernel
/// hands back the frame's EXACT rows and the campaign boundary applies
/// whichever precision rung is configured (`CELESTE_REM_BITS`). Its
/// accumulators go through `Rt2::boundary_exact`, never `boundary`.
/// Regenerate with `transpile --room-kernels-ladder
/// crates/celeste-kernels/src/ladder`; staleness is caught by
/// `ladder_kernels_are_current`.
pub mod ladder;

/// The EXACT-REM set, for the ladder's top rung (k = 16, rem `Exact`):
/// the interval slots are plain per-lane numbers, so `__split_by_flr`
/// is the identity and the set has NO rem forks - which is why it is
/// far smaller than the other two. Exact rows through
/// `Rt2::boundary_exact`. Regenerate with `transpile
/// --room-kernels-exact crates/celeste-kernels/src/exact`; staleness is
/// caught by `exact_kernels_are_current`.
pub mod exact;
