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
}
