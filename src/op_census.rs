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

/// Where the vector columns a filter has to gather actually live. Filter
/// cost is proportional to live columns, and the two containers have
/// completely different levers: `local_env` columns are killed by liveness
/// pruning (which the interpreter currently disables - it runs
/// `all_live()`), heap columns are not.
static FILTER_COLS: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
pub const COL_ORIGINS: [&str; 3] = ["heap", "local_env", "outer_local_envs"];

pub fn record_filter_columns(heap: usize, local: usize, outer: usize) {
    FILTER_COLS[0].fetch_add(heap as u64, Ordering::Relaxed);
    FILTER_COLS[1].fetch_add(local as u64, Ordering::Relaxed);
    FILTER_COLS[2].fetch_add(outer as u64, Ordering::Relaxed);
}

/// What garbage collection actually reclaims, and how much of the heap a
/// filter has to look at. `gc` only reclaims cells nothing can *reach*;
/// a cell that is still reachable but will never be read again is invisible
/// to it, and gets copied by every filter until the frame ends.
static GC_CELLS_BEFORE: AtomicU64 = AtomicU64::new(0);
static GC_CELLS_AFTER: AtomicU64 = AtomicU64::new(0);
static FILTER_HEAP_CELLS: AtomicU64 = AtomicU64::new(0);

pub fn record_gc_cells(before: usize, after: usize) {
    if !enabled() {
        return;
    }
    GC_CELLS_BEFORE.fetch_add(before as u64, Ordering::Relaxed);
    GC_CELLS_AFTER.fetch_add(after as u64, Ordering::Relaxed);
}

pub fn record_filter_heap_cells(cells: usize) {
    FILTER_HEAP_CELLS.fetch_add(cells as u64, Ordering::Relaxed);
}

/// Locals *occupied* at filter time, against which `FILTER_COLS[1]` counts
/// only the ones holding per-lane vectors. A local holding a scalar, a
/// pointer or a nil costs a filter nothing, so the two numbers answer
/// different questions: how much is live, versus how much is lane data.
static FILTER_LOCAL_SLOTS: AtomicU64 = AtomicU64::new(0);

pub fn record_filter_local_slots(slots: usize) {
    FILTER_LOCAL_SLOTS.fetch_add(slots as u64, Ordering::Relaxed);
}

/// The virtual-concat experiment: how many columns a merge group has, how
/// many of them are uniform (so materialising would collapse them out of
/// the dedup key), and what hashing them costs without concatenating. The
/// three parked attempts lost on exactly these two numbers.
static VC_COLUMNS: AtomicU64 = AtomicU64::new(0);
static VC_UNIFORM: AtomicU64 = AtomicU64::new(0);
static VC_ROWS: AtomicU64 = AtomicU64::new(0);
static VC_NANOS: AtomicU64 = AtomicU64::new(0);

pub fn record_virtual_hash(columns: usize, uniform: usize, rows: usize, nanos: u64) {
    VC_COLUMNS.fetch_add(columns as u64, Ordering::Relaxed);
    VC_UNIFORM.fetch_add(uniform as u64, Ordering::Relaxed);
    VC_ROWS.fetch_add(rows as u64, Ordering::Relaxed);
    VC_NANOS.fetch_add(nanos, Ordering::Relaxed);
}

/// Source length of filter gathers, against which `Cat::Filter`'s element
/// count is the *kept* length. The ratio decides what the gather actually
/// costs: below about one kept element per cache line, a gather touches
/// every line of the source, so the honest traffic is the whole source -
/// and the `Cat::Filter` GB/s figure, which counts only kept elements,
/// understates it by exactly that factor.
static FILTER_SRC_ELEMS: AtomicU64 = AtomicU64::new(0);
static FILTER_SRC_BYTES: AtomicU64 = AtomicU64::new(0);

/// Per-branch-site accounting for the two different things a conditional
/// branch can cost.
///
/// A vector-bool condition with mixed lanes filters the state in two, and
/// that shows up as `filter_branch` time. An `UnknownBool` condition does
/// not filter at all - `flow.rs` sends the whole state down both edges -
/// so it costs a state duplication and everything downstream of it
/// instead, and pays nothing here. The two need completely different
/// rewrites (if-conversion to selects vs `expand` into lanes), so
/// attributing the cost per site is what says which sites are worth which
/// treatment.
#[derive(Default, Clone, Copy)]
pub struct BranchSiteCost {
    pub filter_events: u64,
    pub lanes_in: u64,
    pub lanes_kept: u64,
    pub nanos: u64,
    pub unknown_dups: u64,
    pub unknown_dup_lanes: u64,
}

type SiteKey = (String, String);

fn branch_site_stats() -> &'static std::sync::Mutex<std::collections::HashMap<SiteKey, BranchSiteCost>>
{
    static STATS: OnceLock<std::sync::Mutex<std::collections::HashMap<SiteKey, BranchSiteCost>>> =
        OnceLock::new();
    STATS.get_or_init(Default::default)
}

thread_local! {
    /// The branch currently being flowed, so a filter deep inside
    /// `filter_by_mask` can name the conditional that caused it.
    static BRANCH_SITE: std::cell::RefCell<SiteKey> =
        std::cell::RefCell::new((String::new(), String::new()));
}

/// Names the branch about to be flowed. Only called under the census.
pub fn set_branch_site(function: &str, block: &str) {
    BRANCH_SITE.with(|site| {
        let mut site = site.borrow_mut();
        site.0.clear();
        site.0.push_str(function);
        site.1.clear();
        site.1.push_str(block);
    });
}

fn with_current_site(f: impl FnOnce(&mut BranchSiteCost)) {
    BRANCH_SITE.with(|site| {
        let site = site.borrow();
        let mut stats = branch_site_stats().lock().unwrap();
        f(stats.entry(site.clone()).or_default());
    });
}

pub fn record_branch_filter(lanes_in: usize, kept: usize, started: Option<std::time::Instant>) {
    let Some(started) = started else { return };
    let nanos = started.elapsed().as_nanos() as u64;
    with_current_site(|entry| {
        entry.filter_events += 1;
        entry.lanes_in += lanes_in as u64;
        entry.lanes_kept += kept as u64;
        entry.nanos += nanos;
    });
}

pub fn record_unknown_branch_dup(lanes: usize) {
    if !enabled() {
        return;
    }
    with_current_site(|entry| {
        entry.unknown_dups += 1;
        entry.unknown_dup_lanes += lanes as u64;
    });
}

/// Whole-state filter events split by why they happened. `filter_branch` is
/// overhead the branch-removal campaign exists to delete; `filter_dedup` is
/// the merge doing useful work; `filter_split_flr` is the search genuinely
/// fanning out. Knowing which one owns the time decides whether to make
/// filtering faster or to stop doing it.
static FILTER_REASON_CALLS: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static FILTER_REASON_KEPT: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];
static FILTER_REASON_NANOS: [AtomicU64; 3] = [const { AtomicU64::new(0) }; 3];

pub const REASON_NAMES: [&str; 3] = ["filter_branch", "filter_dedup", "filter_split_flr"];

pub fn record_filter_reason(reason_index: usize, kept: usize, started: Option<std::time::Instant>) {
    let Some(started) = started else { return };
    FILTER_REASON_CALLS[reason_index].fetch_add(1, Ordering::Relaxed);
    FILTER_REASON_KEPT[reason_index].fetch_add(kept as u64, Ordering::Relaxed);
    FILTER_REASON_NANOS[reason_index].fetch_add(started.elapsed().as_nanos() as u64, Ordering::Relaxed);
}

pub fn record_filter_source(src_elems: usize, elem_size: usize) {
    FILTER_SRC_ELEMS.fetch_add(src_elems as u64, Ordering::Relaxed);
    FILTER_SRC_BYTES.fetch_add((src_elems * elem_size) as u64, Ordering::Relaxed);
}

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
    VC_COLUMNS.store(0, Ordering::Relaxed);
    VC_UNIFORM.store(0, Ordering::Relaxed);
    VC_ROWS.store(0, Ordering::Relaxed);
    VC_NANOS.store(0, Ordering::Relaxed);
    GC_CELLS_BEFORE.store(0, Ordering::Relaxed);
    GC_CELLS_AFTER.store(0, Ordering::Relaxed);
    FILTER_HEAP_CELLS.store(0, Ordering::Relaxed);
    FILTER_LOCAL_SLOTS.store(0, Ordering::Relaxed);
    branch_site_stats().lock().unwrap().clear();
    for i in 0..3 {
        FILTER_REASON_CALLS[i].store(0, Ordering::Relaxed);
        FILTER_REASON_KEPT[i].store(0, Ordering::Relaxed);
        FILTER_REASON_NANOS[i].store(0, Ordering::Relaxed);
    }
    for i in 0..3 {
        FILTER_COLS[i].store(0, Ordering::Relaxed);
    }
    FILTER_SRC_ELEMS.store(0, Ordering::Relaxed);
    FILTER_SRC_BYTES.store(0, Ordering::Relaxed);
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
    {
        let stats = branch_site_stats().lock().unwrap();
        let mut rows: Vec<_> = stats.iter().collect();
        rows.sort_by_key(|(_, c)| std::cmp::Reverse(c.nanos));
        if !rows.is_empty() {
            eprintln!(
                "branch sites by filter cost ({:<28} {:>7} {:>10} {:>8} {:>7} {:>10})",
                "function::block", "filters", "lanes in", "kept %", "sec", "unknown"
            );
            for ((function, block), cost) in rows.iter().take(12) {
                if cost.nanos == 0 && cost.unknown_dups == 0 {
                    continue;
                }
                eprintln!(
                    "  {:<44} {:>7} {:>10.1}M {:>7.1}% {:>7.2} {:>5} dups/{:.1}M lanes",
                    format!("{}::{}", function, block),
                    cost.filter_events,
                    cost.lanes_in as f64 / 1e6,
                    if cost.lanes_in > 0 {
                        100.0 * cost.lanes_kept as f64 / cost.lanes_in as f64
                    } else {
                        0.0
                    },
                    cost.nanos as f64 / 1e9,
                    cost.unknown_dups,
                    cost.unknown_dup_lanes as f64 / 1e6,
                );
            }
        }
    }
    for i in 0..3 {
        let calls = FILTER_REASON_CALLS[i].load(Ordering::Relaxed);
        if calls == 0 {
            continue;
        }
        eprintln!(
            "  {:<18} {:>7} state filters, {:>9.1} M lanes kept, {:>6.2} s",
            REASON_NAMES[i],
            calls,
            FILTER_REASON_KEPT[i].load(Ordering::Relaxed) as f64 / 1e6,
            FILTER_REASON_NANOS[i].load(Ordering::Relaxed) as f64 / 1e9,
        );
    }
    let vc_columns = VC_COLUMNS.load(Ordering::Relaxed);
    if vc_columns > 0 {
        let uniform = VC_UNIFORM.load(Ordering::Relaxed);
        let rows = VC_ROWS.load(Ordering::Relaxed);
        let ns = VC_NANOS.load(Ordering::Relaxed);
        eprintln!(
            "virtual concat: {} columns, {} uniform ({:.1}%) -> {} key columns; \
             hashing {:.1} M rows virtually took {:.2} s ({:.2} ns/cell)",
            vc_columns,
            uniform,
            100.0 * uniform as f64 / vc_columns as f64,
            vc_columns - uniform,
            rows as f64 / 1e6,
            ns as f64 / 1e9,
            ns as f64 / ((rows * (vc_columns - uniform)) as f64 / vc_columns as f64).max(1.0),
        );
    }
    let gc_before = GC_CELLS_BEFORE.load(Ordering::Relaxed);
    if gc_before > 0 {
        let gc_after = GC_CELLS_AFTER.load(Ordering::Relaxed);
        eprintln!(
            "gc: {} cells in -> {} out ({:.1}% unreachable and reclaimed)",
            gc_before,
            gc_after,
            100.0 * (gc_before - gc_after) as f64 / gc_before as f64,
        );
    }
    let heap_cells = FILTER_HEAP_CELLS.load(Ordering::Relaxed);
    if heap_cells > 0 {
        eprintln!(
            "filter heap reach: {} cells present, {} of them hold vectors ({:.1}%)",
            heap_cells,
            FILTER_COLS[0].load(Ordering::Relaxed),
            100.0 * FILTER_COLS[0].load(Ordering::Relaxed) as f64 / heap_cells as f64,
        );
    }
    let local_slots = FILTER_LOCAL_SLOTS.load(Ordering::Relaxed);
    if local_slots > 0 {
        eprintln!(
            "filter local reach: {} occupied slots, {} of them hold vectors ({:.1}%)",
            local_slots,
            FILTER_COLS[1].load(Ordering::Relaxed),
            100.0 * FILTER_COLS[1].load(Ordering::Relaxed) as f64 / local_slots as f64,
        );
    }
    let cols: Vec<u64> = (0..3).map(|i| FILTER_COLS[i].load(Ordering::Relaxed)).collect();
    if cols.iter().sum::<u64>() > 0 {
        let total = cols.iter().sum::<u64>() as f64;
        eprintln!(
            "filter columns by origin: {}",
            (0..3)
                .map(|i| format!(
                    "{} {} ({:.1}%)",
                    COL_ORIGINS[i],
                    cols[i],
                    100.0 * cols[i] as f64 / total
                ))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    let src_elems = FILTER_SRC_ELEMS.load(Ordering::Relaxed);
    if src_elems > 0 {
        let kept = ELEMS[Cat::Filter as usize].load(Ordering::Relaxed);
        let src_bytes = FILTER_SRC_BYTES.load(Ordering::Relaxed);
        let ns = NANOS[Cat::Filter as usize].load(Ordering::Relaxed);
        // Honest traffic: every source cache line a gather touches, plus the
        // index list and the output.
        let kept_bytes = BYTES[Cat::Filter as usize].load(Ordering::Relaxed);
        let traffic = src_bytes + kept_bytes;
        eprintln!(
            "filter detail: {:.1} M source elems -> {:.1} M kept ({:.1}% keep rate); \
             whole-source traffic {:.2} GB = {:.1} GB/s",
            src_elems as f64 / 1e6,
            kept as f64 / 1e6,
            100.0 * kept as f64 / src_elems as f64,
            traffic as f64 / 1e9,
            if ns > 0 { traffic as f64 / ns as f64 } else { 0.0 },
        );
    }
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
