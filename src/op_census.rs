//! A census of every vector-scale operation the interpreter performs:
//! calls, elements, bytes touched, nanoseconds. The point is a roofline
//! comparison - each category's achieved GB/s against what the machine
//! sustains for that access pattern (see plans/roofline.md) - so the
//! numbers to trust are per-category throughput, not absolute totals.
//!
//! Enabled by `CELESTE_CENSUS=1`; off, the cost is one atomic load per
//! call site. Timing is one `Instant` pair per *vector* operation (scalar
//! paths are never counted), which at census sizes is negligible.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(usize)]
pub enum Cat {
    /// `select` lane pick (mask + two arms -> fresh vector).
    Select = 0,
    /// Binary op via `map2` (two vectors -> fresh vector).
    Binop,
    /// Unary op via `map`/`map_to` (one vector -> fresh vector).
    Map,
    /// Merge concatenation of one cell across a group (fresh vector).
    Concat,
    /// Column-major row hashing in dedup (streaming read + hash rmw).
    HashRows,
    /// Dedup bucket phase: hashmap probes + row verification.
    DedupBucket,
    /// State filtering (branch/dedup): gathers of kept lanes, all cells.
    Filter,
    /// `expand_lanes`: every vector doubles in place.
    Expand,
    /// `normalize_state_for_comparison`: clone + GC + per-column sorts.
    Normalize,
    /// Heap GC walks.
    Gc,
    _Count,
}

const N: usize = Cat::_Count as usize;

static CALLS: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];
static ELEMS: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];
static BYTES: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];
static NANOS: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];

/// log2-bucketed histogram of Filter gather sizes (kept elements per call).
static FILTER_HIST_CALLS: [AtomicU64; 24] = [const { AtomicU64::new(0) }; 24];
static FILTER_HIST_ELEMS: [AtomicU64; 24] = [const { AtomicU64::new(0) }; 24];

/// Extra detail for the dedup bucket phase, which the plain
/// calls/elems/bytes shape cannot express: how many candidate rows the
/// probe actually verified, and how many column cells that verification
/// read. The two have completely different fixes (probe locality vs
/// row-major random gathers), so the split decides what to build.
static DEDUP_EQ_CALLS: AtomicU64 = AtomicU64::new(0);
static DEDUP_COL_CMPS: AtomicU64 = AtomicU64::new(0);
static DEDUP_COLS: AtomicU64 = AtomicU64::new(0);

pub fn record_dedup_detail(eq_calls: u64, col_cmps: u64, cols: u64) {
    DEDUP_EQ_CALLS.fetch_add(eq_calls, Ordering::Relaxed);
    DEDUP_COL_CMPS.fetch_add(col_cmps, Ordering::Relaxed);
    DEDUP_COLS.fetch_add(cols, Ordering::Relaxed);
}

pub fn record_filter_size(kept: usize) {
    let b = (usize::BITS - kept.max(1).leading_zeros() - 1).min(23) as usize;
    FILTER_HIST_CALLS[b].fetch_add(1, Ordering::Relaxed);
    FILTER_HIST_ELEMS[b].fetch_add(kept as u64, Ordering::Relaxed);
}

pub fn enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_CENSUS").is_some())
}

#[inline]
pub fn start() -> Option<std::time::Instant> {
    if enabled() {
        Some(std::time::Instant::now())
    } else {
        None
    }
}

#[inline]
pub fn record(cat: Cat, elems: usize, bytes: usize, started: Option<std::time::Instant>) {
    let Some(started) = started else { return };
    let i = cat as usize;
    CALLS[i].fetch_add(1, Ordering::Relaxed);
    ELEMS[i].fetch_add(elems as u64, Ordering::Relaxed);
    BYTES[i].fetch_add(bytes as u64, Ordering::Relaxed);
    NANOS[i].fetch_add(started.elapsed().as_nanos() as u64, Ordering::Relaxed);
}

/// Zero every counter. Used to census one frame at a time - the per-lane
/// cost of an op as the lane count grows is the whole question behind
/// tiling, and cumulative totals hide it.
pub fn reset() {
    for i in 0..N {
        CALLS[i].store(0, Ordering::Relaxed);
        ELEMS[i].store(0, Ordering::Relaxed);
        BYTES[i].store(0, Ordering::Relaxed);
        NANOS[i].store(0, Ordering::Relaxed);
    }
    for b in 0..24 {
        FILTER_HIST_CALLS[b].store(0, Ordering::Relaxed);
        FILTER_HIST_ELEMS[b].store(0, Ordering::Relaxed);
    }
    DEDUP_EQ_CALLS.store(0, Ordering::Relaxed);
    DEDUP_COL_CMPS.store(0, Ordering::Relaxed);
    DEDUP_COLS.store(0, Ordering::Relaxed);
}

pub fn report() {
    if !enabled() {
        return;
    }
    let cats = [
        (Cat::Select, "select"),
        (Cat::Binop, "binop(map2)"),
        (Cat::Map, "map/map_to"),
        (Cat::Concat, "merge concat"),
        (Cat::HashRows, "hash_rows"),
        (Cat::DedupBucket, "dedup bucket"),
        (Cat::Filter, "state filter"),
        (Cat::Expand, "expand_lanes"),
        (Cat::Normalize, "normalize"),
        (Cat::Gc, "gc"),
    ];
    eprintln!();
    eprintln!(
        "{:<14} {:>10} {:>10} {:>9} {:>8} {:>8} {:>9}",
        "op census", "calls", "Melems", "GB", "sec", "GB/s", "ns/elem"
    );
    let mut total_ns = 0u64;
    for (cat, name) in cats {
        let i = cat as usize;
        let calls = CALLS[i].load(Ordering::Relaxed);
        if calls == 0 {
            continue;
        }
        let elems = ELEMS[i].load(Ordering::Relaxed);
        let bytes = BYTES[i].load(Ordering::Relaxed);
        let ns = NANOS[i].load(Ordering::Relaxed);
        total_ns += ns;
        eprintln!(
            "{:<14} {:>10} {:>10.1} {:>9.2} {:>8.2} {:>8.1} {:>9.2}",
            name,
            calls,
            elems as f64 / 1e6,
            bytes as f64 / 1e9,
            ns as f64 / 1e9,
            if ns > 0 { bytes as f64 / ns as f64 } else { 0.0 },
            if elems > 0 { ns as f64 / elems as f64 } else { 0.0 },
        );
    }
    eprintln!(
        "{:<14} {:>41.2} s inside censused ops",
        "total", total_ns as f64 / 1e9
    );
    let eq_calls = DEDUP_EQ_CALLS.load(Ordering::Relaxed);
    if eq_calls > 0 {
        let col_cmps = DEDUP_COL_CMPS.load(Ordering::Relaxed);
        let cols = DEDUP_COLS.load(Ordering::Relaxed);
        eprintln!(
            "dedup detail: {:.1} M rows_equal calls, {:.1} M column cells read \
             ({:.1} of {:.1} columns per call before a mismatch)",
            eq_calls as f64 / 1e6,
            col_cmps as f64 / 1e6,
            col_cmps as f64 / eq_calls as f64,
            cols as f64 / eq_calls as f64,
        );
    }
    eprintln!("filter gather size histogram (kept elems/call):");
    for b in 0..24 {
        let c = FILTER_HIST_CALLS[b].load(Ordering::Relaxed);
        if c == 0 {
            continue;
        }
        let e = FILTER_HIST_ELEMS[b].load(Ordering::Relaxed);
        eprintln!(
            "  2^{:<2} {:>10} calls {:>10.1} Melems",
            b,
            c,
            e as f64 / 1e6
        );
    }
}
