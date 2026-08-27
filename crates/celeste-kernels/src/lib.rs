//! A THIN AGGREGATOR over the generated CONSTANT-LATTICE kernels, and
//! nothing hand-written except this file and the three variant
//! `mod.rs` files below (also generated - see their own doc comments).
//!
//! Three variants x three rooms, every one of them lattice-specialized
//! (plans/specialize.md "Spec: latticeify everything, all rooms, one
//! table"): the per-room constant fixpoint bakes the fields that are
//! constant across that room's reachable states (static object
//! positions, speeds, hitboxes) into the kernel body, and `pin_guard`
//! puts the check that a lane actually holds those constants into the
//! kernel's `ok` - a lane that disagrees is DECLINED, fatal under the
//! default `CELESTE_KERNEL_STRICT`, never silently wrong. The
//! non-lattice walk sets that used to live here were retired 2026-08-26.
//!
//! The generated kernels themselves live ONE CRATE PER ROOM
//! (`celeste-kernels-room00`, `celeste-kernels-room10`,
//! `celeste-kernels-room20`) so that touching one room's kernels only
//! recompiles that room's crate - before this split the three rooms
//! were ~900k lines in ONE compilation unit, so a one-line edit to a
//! room (2,0) kernel forced a full relink of every room. Each variant
//! module here (`traced`/`ladder`/`exact`) re-exports that room crate's
//! `room00`/`room10`/`room20` submodule and assembles `SETS` - every
//! room's `KERNELS` table, which `Dispatch::new_multi` flattens into
//! the one shape-hash registry - plus a `FINGERPRINT` that re-hashes
//! every room's kernel sources (so it is unaffected by which crate a
//! room's kernels happen to live in). Rooms are generated one process
//! each (`CELESTE_START_ROOM` feeds process globals) into their own
//! crate and merged file-level by `transpile --merge-kernels`; the
//! canonical regen is `./regen-generated.sh`.
//!
//! CHECKED IN, like `celeste-names`' tables, and for the same reason: a
//! build.rs would have to run `transpile`, which needs the tracer from a
//! crate that sits ABOVE this one. Staleness is caught byte-for-byte by
//! `traced_kernels_are_current` / `ladder_kernels_are_current` /
//! `exact_kernels_are_current` (room (1,0) + every room's fingerprint)
//! and the `#[ignore]`d `room00_kernels_are_current` /
//! `room20_kernels_are_current`.
//!
//! This crate (and its room crates) sit ABOVE `celeste-engine` because
//! the emitted code calls `celeste_engine::kernel`'s lane primitives and
//! builds `celeste_engine::runtime2::Rt2` blocks - while the engine reaches
//! DOWN to `celeste-names` for `FIELD_NAMES`. That is why the generated
//! code is two crates (well, five, counting the per-room split) and not
//! one: in one crate those two directions are a cycle.

/// The BASE (level-0) set: the Bits(0) boundary widenings are traced
/// into the graph, accumulators go through `Rt2::boundary`. Serves rem
/// Bits(0) / spd Exact only. Regenerate with `transpile --room-kernels`
/// per room; staleness gate `traced_kernels_are_current`.
pub mod traced;

/// The RUNG-AGNOSTIC set (plans/kernel-ladder.md): the same shapes,
/// traced with the boundary widenings left OUT of the graph, so a kernel
/// hands back the frame's EXACT rows and the campaign boundary applies
/// whichever precision rung is configured (`CELESTE_REM_BITS`). Its
/// accumulators go through `Rt2::boundary_exact`. Regenerate with
/// `transpile --room-kernels-ladder` per room; staleness gate
/// `ladder_kernels_are_current`.
pub mod ladder;

/// The EXACT-REM set, for the ladder's top rung (k = 16, rem `Exact`):
/// the interval slots are plain per-lane numbers, so `__split_by_flr`
/// is the identity and the set has NO rem forks - which is why it is
/// far smaller than the other two. Exact rows through
/// `Rt2::boundary_exact`. Regenerate with `transpile
/// --room-kernels-exact` per room; staleness gate
/// `exact_kernels_are_current`.
pub mod exact;
