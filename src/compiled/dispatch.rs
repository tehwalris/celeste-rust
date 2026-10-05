//! Kernel DISPATCH: the call into the ASM kernel set and the hit/miss
//! counters. A chunk no kernel takes returns `false`, which is FATAL in
//! `FrameEngine::run_bucket` (never deopt silently).

use celeste_engine::runtime2;

/// A one-line miss summary for the abort: lanes the kernels could not serve
/// (a shape with no kernel, or a declined lane).
pub(crate) fn miss_report() -> String {
    format!("  {} lanes missed the ASM kernels", missed_lanes())
}

pub(crate) fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    cell_in: &[u32],
    lanes: std::ops::Range<usize>,
    sink: &mut crate::frame::ForwardSink,
) -> bool {
    // A miss is counted here and is fatal in the caller.
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

/// Lanes the kernels have MISSED so far, without resetting the counter.
/// Kernels-against-reference tests assert this is zero.
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
