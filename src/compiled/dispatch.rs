//! Kernel DISPATCH: which kernel set runs, and the hit/miss counters.
//!
//! The registry (`super::asm_kernel`) is the constant-lattice kernel set
//! for the active rem rung: one assembled kernel per start-room heap
//! SHAPE, indexed by the chunk's shape hash. A chunk no kernel takes
//! returns `false`, which is FATAL (`FrameEngine::run_frame_block`): a
//! coverage gap that only shows up as wall clock is the failure mode this
//! search refuses to have (CLAUDE.md "Never deopt to the interpreter").

use celeste_engine::runtime2;

/// Which traced set runs, and which boundary its accumulators take
/// (plans/kernel-ladder.md).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TracedMode {
    /// The level-0 set (`WalkOpts::LEVEL0`): the Bits(0) widenings are in
    /// the graph, accumulators go through `Rt2::boundary`. Valid ONLY at
    /// rem Bits(0).
    Level0,
    /// The ladder set (`WalkOpts::LADDER_WIDEN`): the rem rung's widening
    /// in the graph, rows through `Rt2::boundary_exact`. Serves rem
    /// Bits(1..=15).
    Level0Agnostic,
    /// The exact-rem set (`WalkOpts::EXACT`), for the top rung: interval
    /// slots as plain numbers, no rem forks, exact rows through
    /// `Rt2::boundary_exact`. Binds only blocks whose rem is a number,
    /// which is every block of an exact-rem level.
    ExactRem,
}

/// The mode, re-evaluated per call (the in-process ladder changes the rem
/// rung between levels, via `set_rem_precision`, and the kernel set must
/// follow it - a OnceLock here froze the mode at level 0's Bits(0) and made
/// every rung refuse its own set): `CELESTE_TRACED_SET=traced|ladder|exact`
/// overrides; otherwise the rem rung picks (Bits(0) -> the level-0 set,
/// Bits(1..15) -> the rung-agnostic set, Exact -> the exact set).
pub(crate) fn traced_mode() -> TracedMode {
    use crate::interpreter::abstraction::RemPrecision;
    let rem = crate::interpreter::abstraction::rem_precision_from_env();
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
    cell_in: &[u32],
    sink: &mut crate::frame::ForwardSink,
) -> bool {
    // The ASM backend is THE kernel implementation: the fused compute graph
    // assembled at startup (`asm_kernel`). A miss (no shape, or a declined
    // lane) is counted here and is fatal in the caller.
    let hit = super::asm_kernel::run_chunk(chunk, ids, cell_in, sink);
    if hit {
        KERNEL_HITS[0].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
    false
}

/// Lanes [0] the kernels ran, [1] missed.
static KERNEL_HITS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

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
    if v.iter().any(|x| *x > 0) {
        eprintln!("kernel lanes: traced {} missed {}", v[0], v[1]);
    }
    let [calls, rows, slice_lanes] = super::asm_kernel::take_call_stats();
    if calls > 0 {
        eprintln!(
            "kernel calls: {} calls, {} rows ({:.1} rows/call), {} slice-lanes executed \
             ({:.1}% padding)",
            calls,
            rows,
            rows as f64 / calls as f64,
            slice_lanes,
            100.0 * (slice_lanes - rows) as f64 / slice_lanes as f64
        );
    }
}
