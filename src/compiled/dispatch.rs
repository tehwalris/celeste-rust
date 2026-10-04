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
    /// The ladder set (`WalkOpts::ladder_widen`): the rem rung's widening
    /// in the graph, rows through `Rt2::boundary_exact`. Serves rem
    /// Bits(1..=15).
    Level0Agnostic,
    /// The exact-rem set (`WalkOpts::EXACT`), for the top rung: interval
    /// slots as plain numbers, no rem forks, exact rows through
    /// `Rt2::boundary_exact`. Binds only blocks whose rem is a number,
    /// which is every block of an exact-rem level.
    ExactRem,
}

/// The mode of a rem rung: Bits(0) -> the level-0 set, Bits(1..15) -> the
/// rung-specific ladder set, Exact -> the exact set.
pub(crate) fn traced_mode_for(rem: crate::interpreter::abstraction::RemPrecision) -> TracedMode {
    use crate::interpreter::abstraction::RemPrecision;
    match rem {
        RemPrecision::Bits(0) => TracedMode::Level0,
        RemPrecision::Bits(_) => TracedMode::Level0Agnostic,
        RemPrecision::Exact => TracedMode::ExactRem,
    }
}

/// A one-line miss summary for the strict-mode abort: how many lanes the
/// ASM kernels could not serve (a shape with no assembled kernel, or a
/// declined lane). The ASM path does not categorize by refusal step.
pub(crate) fn miss_report() -> String {
    format!("  {} lanes missed the ASM kernels", missed_lanes())
}

pub(crate) fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    cell_in: &[u32],
    lanes: std::ops::Range<usize>,
    sink: &mut crate::frame::ForwardSink,
) -> bool {
    // The ASM backend is THE kernel implementation: the fused compute graph
    // assembled at startup (`asm_kernel`). A miss (no shape, or a declined
    // lane) is counted here and is fatal in the caller.
    let n = lanes.len() as u64;
    let hit = super::asm_kernel::run_chunk(chunk, cell_in, lanes, sink);
    if hit {
        KERNEL_HITS[0].fetch_add(n, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[1].fetch_add(n, std::sync::atomic::Ordering::Relaxed);
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
    let [calls, rows, slice_lanes, bodies, bodies_taken, lane_emits, unique] =
        super::asm_kernel::take_call_stats();
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
        eprintln!(
            "kernel utilization: {} (body, slice) evaluations, {} ({:.1}%) took a lane; \
             {} lane emissions ({:.1} per input row), {} after the call's dedup cache ({:.1}%)",
            bodies,
            bodies_taken,
            100.0 * bodies_taken as f64 / bodies.max(1) as f64,
            lane_emits,
            lane_emits as f64 / rows.max(1) as f64,
            unique,
            100.0 * unique as f64 / lane_emits.max(1) as f64
        );
    }
}
