//! Kernel DISPATCH: which kernel, if any, runs a chunk.
//!
//! The registry is the active CONSTANT-LATTICE kernel set
//! (`celeste_kernels::{traced,ladder,exact}`, selected by rung): one
//! kernel per (room, heap SHAPE) pair across every generated room,
//! indexed by the chunk's shape hash. A chunk no kernel takes returns
//! `false`, which is FATAL under the default strict mode - a coverage
//! gap that only shows up as wall clock is the failure mode this
//! campaign is trying to avoid (CLAUDE.md "Never deopt to the
//! interpreter"). `CELESTE_KERNEL_STRICT=0` restores the counted,
//! named fall-through to the interpreter reference path.
//!
//! This module was the probe's; it moved into celeste-rust with the rest of
//! the frame interface (task #150) so the campaign can reach it. What is
//! left in it is dispatch and the diagnostic counters - the frame itself is
//! `super::FrameEngine::step`.

use celeste_engine::runtime2;

/// Which traced set runs, and which boundary its accumulators take
/// (plans/kernel-ladder.md).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TracedMode {
    /// The checked-in level-0 set (`celeste_kernels::traced`): the
    /// Bits(0) widenings are in the graph, accumulators go through
    /// `Rt2::boundary`. Valid ONLY at rem Bits(0) / spd Exact -
    /// `compiled_forward` refuses anything else.
    Level0,
    /// The rung-agnostic set (`celeste_kernels::ladder`): exact rows,
    /// `Rt2::boundary_exact`; the campaign boundary applies whichever
    /// precision rung is configured. Serves rem Bits(0..=15).
    Level0Agnostic,
    /// The exact-rem set (`celeste_kernels::exact`), for the top rung
    /// (k = 16): interval slots as plain numbers, no rem forks, exact
    /// rows through `Rt2::boundary_exact`. Binds only blocks whose rem
    /// is a number, which is every block of an exact-rem campaign.
    ExactRem,
}

/// The mode, re-evaluated per call (the in-process ladder changes the rem
/// rung between levels): `CELESTE_TRACED_SET=traced|ladder` overrides;
/// otherwise the rem rung picks (Bits(0) -> the level-0 set, every other
/// rung -> the rung-agnostic set). `compiled_forward` validates the
/// (mode, precision) combination before any chunk runs.
pub(crate) fn traced_mode() -> TracedMode {
    // NOT cached: the in-process ladder changes rem precision per rung (via
    // `set_rem_precision`), and the kernel set must follow it -
    // Bits(0) -> the level-0 set, Bits(1..) -> the rung-agnostic set, Exact ->
    // the exact set. A OnceLock here froze the mode at level 0's Bits(0) and
    // made every rung refuse its own set. `rem_precision_from_env` is a cheap
    // atomic/env read.
    traced_mode_for(crate::interpreter::abstraction::rem_precision_from_env())
}

/// The mode a process AT `rem` would select - the same env override,
/// else the rung picks. Public within the crate because the checkpoint
/// fingerprint of a COARSER precision level must be computed with THAT
/// level's engine, not the current process's: a banded rung validates
/// the level below it against the fingerprint that level itself wrote
/// (found by the h40 KERNELS=1 smoke, where k=1 computed level 0's
/// fingerprint with the ladder set and refused the level-0 checkpoints
/// the level-0-set process had written).
pub(crate) fn traced_mode_for(
    rem: crate::interpreter::abstraction::RemPrecision,
) -> TracedMode {
    use crate::interpreter::abstraction::RemPrecision;
    match std::env::var("CELESTE_TRACED_SET").as_deref() {
        Ok("traced") => TracedMode::Level0,
        Ok("ladder") => TracedMode::Level0Agnostic,
        Ok("exact") => TracedMode::ExactRem,
        Ok(other) => {
            panic!("CELESTE_TRACED_SET={:?}: expected traced, ladder or exact", other)
        }
        Err(_) => match rem {
            RemPrecision::Bits(0) => TracedMode::Level0,
            RemPrecision::Bits(_) => TracedMode::Level0Agnostic,
            RemPrecision::Exact => TracedMode::ExactRem,
        },
    }
}

/// Whether the ladder (`Level0Agnostic`) kernel set bakes the rem rung
/// widening into the graph (`WalkOpts::LADDER_WIDEN`, `WidenMode::RemRung`)
/// instead of emitting exact rows and leaving the rung widening to the
/// campaign boundary (plans/keying-widening-flow.md).
///
/// DEFAULT ON (2026-08-29): the exact-ladder-then-wrapper-widen path was
/// the source of the Bits(2) keying artifact - the kernel's own
/// `chunk_skip` acted on the pre-widening key while the frontier stored
/// the widened key - and moving the widening into the graph fixes it AND
/// is FASTER per frame (the within-frame dedup collapses rows on the
/// widened key before they reach the boundary; measured -34% on room
/// (1,0) f31 at Bits(2)). `CELESTE_WIDEN_IN_GRAPH=0` is the kill-switch
/// for debugging / A-B. Rung-SPECIFIC, so a process must not change rem
/// precision after the registry is built (the OnceLock caches one rung's
/// kernels); the production ladder runs one rung PER PROCESS
/// (`ladder.sh`), so this holds. Level 0 already widens in the graph
/// (`WidenMode::Level0`) and the exact-rem set has no rem intervals, so
/// this only governs the Bits(1..15) ladder set.
pub(crate) fn widen_in_graph() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_WIDEN_IN_GRAPH").map_or(true, |v| v != "0"))
}

/// The assert-noop guard (plans/keying-widening-flow.md, Phase 1 point 1):
/// when the kernels widen in the graph, re-applying the campaign's
/// `make_state_abstract` to their output must be a no-op on the row keys.
/// A failure means the graph UNDER-widened some field the boundary would
/// still move - the exact class the fruit `off`/`y` bug was in. OFF by
/// default (a per-frame re-abstraction + double keying); opt-in via
/// `CELESTE_KERNEL_WIDEN_NOOP=1`. Only meaningful with
/// `CELESTE_WIDEN_IN_GRAPH=1` (an exact-rem kernel is not a fixed point of
/// the rung widening by construction).
pub(crate) fn widen_noop_check() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_KERNEL_WIDEN_NOOP").map_or(false, |v| v != "0"))
}

/// A one-line miss summary for the strict-mode abort: how many lanes the
/// ASM kernels could not serve (a shape with no assembled kernel, or a
/// declined lane). The ASM path does not categorize by refusal step.
pub(crate) fn miss_report() -> String {
    format!("  {} lanes missed the ASM kernels", missed_lanes())
}

pub(crate) fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
) -> bool {
    // The ASM backend is THE kernel implementation: the fused compute graph
    // assembled at startup (`asm_kernel`), which replaced the generated Rust
    // kernels. A miss (no shape, or a declined lane) falls through to the
    // reference path, counted below.
    let hit = super::asm_kernel::run_chunk(chunk, ids, done);
    if hit {
        KERNEL_HITS[0].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
    false
}

/// Lanes the engine routed through the PLAIN program (kernel deopt
/// sub-chunks; see `FrameEngine::plain_block`).
pub(crate) static PLAIN_ROUTED: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

/// The kernel backend's content hash, hashed into the campaign fingerprint
/// whenever the compiled engine is on: nothing else the fingerprint reads
/// determines what the kernels compute. `mode` is ignored - the rem rung is
/// already a separate fingerprint component (`CampaignConfig::precision`),
/// so the ASM engine's identity is mode-independent (see
/// `asm_kernel::engine_fingerprint`), which is also what lets the band
/// loader recompute a previous level's fingerprint in-process and match.
pub(crate) fn set_fingerprint_for(_mode: TracedMode) -> u64 {
    super::asm_kernel::engine_fingerprint()
}

/// Lanes [0] the kernels ran, [1] missed.
static KERNEL_HITS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Lanes the traced set has run so far, WITHOUT resetting the counter.
///
/// `print_kernel_hits` swaps every counter to zero, so a test that wants
/// to assert the traced kernels actually engaged cannot use it - and an
/// engine differential that passes because nothing ran is the failure
/// this exists to prevent.
pub fn traced_lanes() -> u64 {
    KERNEL_HITS[0].load(std::sync::atomic::Ordering::Relaxed)
}

/// Lanes the traced set has MISSED so far, without resetting the counter.
/// The ladder differential tests assert this is zero: a run where chunks
/// quietly fell through to the reference path would otherwise pass while
/// checking interpreter against interpreter.
pub fn missed_lanes() -> u64 {
    KERNEL_HITS[1].load(std::sync::atomic::Ordering::Relaxed)
}

pub fn print_kernel_hits() {
    let v: Vec<u64> = KERNEL_HITS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    let plain = PLAIN_ROUTED.swap(0, std::sync::atomic::Ordering::Relaxed);
    if v.iter().any(|x| *x > 0) || plain > 0 {
        eprintln!(
            "kernel lanes: traced {} missed {} plain-routed {}",
            v[0], v[1], plain
        );
    }
}

/// The frozen-frontier + within-frame skip, for the ASM append. A row whose
/// key is already in the frozen frontier (Option 1) or was already emitted
/// this frame by a sibling chunk (Option 4, race-sound) need not be exported
/// - the frontier subtract would drop it anyway, so this only saves the
/// export/merge. Off (no guard) => both return false => byte-identical to no
/// skip. Applied POST-boundary in `asm_kernel`, since the boundary is what
/// computes the row keys.
pub(crate) fn chunk_skip(key: (u64, u64)) -> bool {
    // The skip reduces a chunk's materialized rows, which makes the
    // per-chunk `check` comparison see fewer rows than the interpreter (a
    // sibling chunk covers the within-frame dups, and under
    // CELESTE_FRONTIER_ONLY the frozen frontier drops already-visited rows
    // - neither of which the per-chunk interpreter reference does). So the
    // skip is a per-chunk comparison asymmetry BY CONSTRUCTION, on the
    // WITHIN-FRAME axis as well as the frontier one - "on for both" cannot
    // fix the within-frame half without cross-chunk deduping the reference
    // too. CELESTE_ASM_NO_SKIP turns it off so `check` compares full frame
    // outputs ("off for both"), and CHECK MODE forces that automatically
    // (plans/keying-widening-flow.md, Phase 4): the skip is a pure
    // export/merge optimization the campaign's own boundary re-does, so
    // dropping it in check changes no stored result, only what the
    // comparison sees.
    static NO_SKIP: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    if *NO_SKIP.get_or_init(|| {
        std::env::var_os("CELESTE_ASM_NO_SKIP").is_some()
            || std::env::var("CELESTE_COMPILED_FORWARD").as_deref() == Ok("check")
    }) {
        return false;
    }
    frontier_hit(key) || within_frame_dup(key)
}

// ---- Option 1: frozen-frontier skip before materialization ----
//
// The generated kernel `append` takes a `skip: &dyn Fn((u64,u64))->bool`
// and skips materializing any row it returns true for. The frontier lives
// in `Visited` (interp), which the generated kernels cannot name, and the
// worker path does not thread it down. So the worker sets it on a
// thread-local for the duration of its compiled-engine call, and
// `run_traced_kernel` reads it. Off (no guard) => `false` => byte-identical.

thread_local! {
    static FROZEN_FRONTIER: std::cell::Cell<Option<*const crate::interpreter::visited::Visited>> =
        const { std::cell::Cell::new(None) };
}

/// Clears the thread-local frozen frontier on drop.
pub struct FrontierGuard(());

/// Point this thread's kernels at `v` as the frozen frontier for Option-1
/// skip, until the guard drops. CONTRACT: `v` must be FROZEN (read-only)
/// while the guard is alive - i.e. `v.is_frozen()` (the buffered map or the
/// mmap engine). Only the worker that owns this thread's kernel call sets it,
/// around ONE `interpret_state_base`, so the pointer never outlives the borrow.
pub fn with_frozen_frontier(v: &crate::interpreter::visited::Visited) -> FrontierGuard {
    FROZEN_FRONTIER.with(|c| c.set(Some(v as *const _)));
    FrontierGuard(())
}

impl Drop for FrontierGuard {
    fn drop(&mut self) {
        FROZEN_FRONTIER.with(|c| c.set(None));
    }
}

/// Is `key` in the frozen frontier this thread was pointed at? `false` when
/// no guard is set (Option 1 off).
fn frontier_hit(key: (u64, u64)) -> bool {
    FROZEN_FRONTIER.with(|c| match c.get() {
        // SAFETY: the pointer is set only for the lifetime of a
        // `FrontierGuard`, which the worker keeps alive across the kernel
        // call, and the frontier is read-only (frozen) for that whole time.
        Some(p) => unsafe { (*p).contains_historic(key) },
        None => false,
    })
}

use std::sync::atomic::{AtomicPtr, AtomicU8, AtomicU64, Ordering as O};

/// A frame-scoped, SHARED-across-workers set the kernels probe to skip
/// materializing a successor a sibling chunk already emitted THIS frame
/// (Option 4). Lock-free and RACE-SOUND: two occurrences of a successor are
/// byte-identical, so a race that lets both through just materializes a
/// duplicate (collapsed by `partition_filter`), never loses a row. The only
/// unsound outcome - reporting a DUP for a distinct key - is ruled out by
/// comparing the FULL 128-bit key on every hit.
pub struct WithinFrameSet {
    mask: usize,
    state: Vec<AtomicU8>, // 0 = empty, 1 = writing, 2 = full
    k0: Vec<AtomicU64>,
    k1: Vec<AtomicU64>,
}

impl WithinFrameSet {
    /// `bits` slots = 2^bits. ~1.2M distinct successors/frame, so 2^22 (4M
    /// slots, load factor ~0.3) keeps probe chains short.
    pub fn with_bits(bits: u32) -> Self {
        let n = 1usize << bits;
        Self {
            mask: n - 1,
            state: (0..n).map(|_| AtomicU8::new(0)).collect(),
            k0: (0..n).map(|_| AtomicU64::new(0)).collect(),
            k1: (0..n).map(|_| AtomicU64::new(0)).collect(),
        }
    }

    /// Reset to empty for a new frame. Only `state` matters - a stale key
    /// under `state == empty` is never read.
    pub fn clear(&self) {
        for s in &self.state {
            s.store(0, O::Relaxed);
        }
    }

    /// `true` => this key is ALREADY present (skip / do not materialize).
    /// `false` => newly inserted, or "not sure" under contention (materialize).
    #[inline]
    pub fn probe_or_insert(&self, key: (u64, u64)) -> bool {
        let h = key.0 ^ key.1.rotate_left(32);
        let mut slot = (h as usize) & self.mask;
        for _ in 0..32 {
            match self.state[slot].load(O::Acquire) {
                2 => {
                    if self.k0[slot].load(O::Relaxed) == key.0
                        && self.k1[slot].load(O::Relaxed) == key.1
                    {
                        return true; // exact dup
                    }
                    slot = (slot + 1) & self.mask; // collision, linear probe
                }
                0 => {
                    // Claim the empty slot; only the CAS winner writes it.
                    if self.state[slot]
                        .compare_exchange(0, 1, O::AcqRel, O::Acquire)
                        .is_ok()
                    {
                        self.k0[slot].store(key.0, O::Relaxed);
                        self.k1[slot].store(key.1, O::Relaxed);
                        self.state[slot].store(2, O::Release);
                        return false; // newly inserted
                    }
                    // lost the claim; reload the same slot next iteration
                }
                _ => return false, // writing: treat as new (materialize) - sound
            }
        }
        false // probe budget spent: materialize - sound
    }
}

/// The current frame's within-frame set, shared across the step_parallel
/// workers. Set for the lifetime of the worker scope, null otherwise.
static WITHIN_FRAME: AtomicPtr<WithinFrameSet> = AtomicPtr::new(std::ptr::null_mut());

/// SAFETY: the caller keeps `set` alive for the whole worker scope and clears
/// the pointer (below) before dropping it.
pub fn set_within_frame(set: &WithinFrameSet) {
    WITHIN_FRAME.store(set as *const _ as *mut _, O::Release);
}

pub fn clear_within_frame() {
    WITHIN_FRAME.store(std::ptr::null_mut(), O::Release);
}

#[inline]
fn within_frame_dup(key: (u64, u64)) -> bool {
    let p = WITHIN_FRAME.load(O::Acquire);
    if p.is_null() {
        return false;
    }
    // SAFETY: non-null only within the worker scope that set it, where the set
    // outlives every kernel probe.
    unsafe { (*p).probe_or_insert(key) }
}
