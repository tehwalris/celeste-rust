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
static FILTER_REASON_CALLS: [AtomicU64; REASON_COUNT] = [const { AtomicU64::new(0) }; REASON_COUNT];
static FILTER_REASON_KEPT: [AtomicU64; REASON_COUNT] = [const { AtomicU64::new(0) }; REASON_COUNT];
static FILTER_REASON_NANOS: [AtomicU64; REASON_COUNT] = [const { AtomicU64::new(0) }; REASON_COUNT];
/// Contiguous runs in the kept-index list, per reason. kept/runs is the mean
/// run length: how much of a filter gather could be chunked memcpy instead of
/// per-lane gather, and - if it is low - how much a sorted lane order at the
/// merge could raise it.
static FILTER_REASON_RUNS: [AtomicU64; REASON_COUNT] = [const { AtomicU64::new(0) }; REASON_COUNT];

pub const REASON_COUNT: usize = 8;
pub const REASON_NAMES: [&str; REASON_COUNT] = [
    "filter_branch",
    "filter_dedup",
    "filter_split_flr",
    "filter_visited",
    "filter_deopt",
    "filter_band",
    "filter_chunk",
    "filter_straddle",
];

pub fn record_filter_reason(reason_index: usize, kept: usize, started: Option<std::time::Instant>) {
    let Some(started) = started else { return };
    FILTER_REASON_CALLS[reason_index].fetch_add(1, Ordering::Relaxed);
    FILTER_REASON_KEPT[reason_index].fetch_add(kept as u64, Ordering::Relaxed);
    FILTER_REASON_NANOS[reason_index].fetch_add(started.elapsed().as_nanos() as u64, Ordering::Relaxed);
}

pub fn record_filter_runs(reason_index: usize, runs: usize) {
    FILTER_REASON_RUNS[reason_index].fetch_add(runs as u64, Ordering::Relaxed);
}

/// Cross-fragment op-memo pricing: how many vector-input binary ops and
/// selects recur with *identical input Arcs* within one frame. Fragments
/// born from an UnknownBool duplication share unchanged column Arcs, so an
/// op over shared inputs recomputes the same output in every fragment - a
/// per-frame memo keyed on (op, input pointers) would return a clone
/// instead. These counters price that memo before it is built.
static MEMO_CALLS: AtomicU64 = AtomicU64::new(0);
static MEMO_HITS: AtomicU64 = AtomicU64::new(0);
static MEMO_HIT_ELEMS: AtomicU64 = AtomicU64::new(0);
static MEMO_ELEMS: AtomicU64 = AtomicU64::new(0);

lazy_static::lazy_static! {
    /// Seen memo keys plus strong refs to every probed Arc, so a freed
    /// vector's address cannot be reused and fake a hit. Diagnostic only,
    /// cleared with the rest of the census.
    static ref MEMO_SEEN: std::sync::Mutex<(
        rustc_hash::FxHashSet<u64>,
        Vec<std::sync::Arc<dyn std::any::Any + Send + Sync>>,
    )> = std::sync::Mutex::new((rustc_hash::FxHashSet::default(), Vec::new()));
}

/// Probe the pricing memo with a pre-hashed (op, input pointers) key.
pub fn memo_probe(
    key: u64,
    elems: usize,
    holders: Vec<std::sync::Arc<dyn std::any::Any + Send + Sync>>,
) {
    let mut seen = MEMO_SEEN.lock().unwrap();
    let hit = !seen.0.insert(key);
    if !hit {
        seen.1.extend(holders);
    }
    drop(seen);
    record_memo_probe(hit, elems);
}

/// Class-dead speculation: a select whose mask is uniform (scalar, or a
/// vector that collapsed to one truth value) returns one arm and discards
/// the other. A discarded *vector* arm is upstream per-lane compute that
/// this consumption point threw away - with partitioned merges making
/// masks uniform per class, this counts the vector work that per-class
/// constant folding + DCE of the speculated regions could skip entirely.
static SELECT_UNIFORM_CALLS: AtomicU64 = AtomicU64::new(0);
static SELECT_UNIFORM_DISCARD_LANES: AtomicU64 = AtomicU64::new(0);
static SELECT_MIXED_CALLS: AtomicU64 = AtomicU64::new(0);

pub fn record_select_uniform(discarded_vector_lanes: usize) {
    SELECT_UNIFORM_CALLS.fetch_add(1, Ordering::Relaxed);
    SELECT_UNIFORM_DISCARD_LANES.fetch_add(discarded_vector_lanes as u64, Ordering::Relaxed);
}

pub fn record_select_mixed() {
    SELECT_MIXED_CALLS.fetch_add(1, Ordering::Relaxed);
}

pub fn record_memo_probe(hit: bool, elems: usize) {
    MEMO_CALLS.fetch_add(1, Ordering::Relaxed);
    MEMO_ELEMS.fetch_add(elems as u64, Ordering::Relaxed);
    if hit {
        MEMO_HITS.fetch_add(1, Ordering::Relaxed);
        MEMO_HIT_ELEMS.fetch_add(elems as u64, Ordering::Relaxed);
    }
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

/// How lossy the UnknownBool collapse is: lanes that HAD a definite answer
/// at a comparison that nonetheless produced a whole-value UnknownBool,
/// against the lanes involved. If the definite share is ~0 the collapse
/// costs nothing and partitioning the state would buy nothing.
static COLLAPSE_DEFINITE: AtomicU64 = AtomicU64::new(0);
static COLLAPSE_TOTAL: AtomicU64 = AtomicU64::new(0);
static COLLAPSE_CALLS: AtomicU64 = AtomicU64::new(0);
static COLLAPSE_MIXED: AtomicU64 = AtomicU64::new(0);

pub fn record_unknown_collapse(definite: usize, total: usize) {
    COLLAPSE_DEFINITE.fetch_add(definite as u64, Ordering::Relaxed);
    COLLAPSE_TOTAL.fetch_add(total as u64, Ordering::Relaxed);
    COLLAPSE_CALLS.fetch_add(1, Ordering::Relaxed);
    if definite > 0 {
        COLLAPSE_MIXED.fetch_add(1, Ordering::Relaxed);
    }
}

/// How many times a `select` on a whole-value `UnknownBool` split the
/// state rather than dropping the frame onto the plain program. Each one
/// is a deopt avoided, but also a state created, so this is the number to
/// watch against the fragment count if the split ever starts to multiply.
static SELECT_SPLITS: AtomicU64 = AtomicU64::new(0);

pub fn record_select_split() {
    SELECT_SPLITS.fetch_add(1, Ordering::Relaxed);
}

/// Per-frame select splits, resetting the counter.
pub fn take_select_splits() -> u64 {
    SELECT_SPLITS.swap(0, Ordering::Relaxed)
}

/// Wall-clock nanoseconds spent inside `run_deopt_frame`, i.e. re-running a
/// state under the plain program after the rewritten one refused it. Summed
/// across worker threads, so on an N-thread frame this can exceed the frame's
/// elapsed time; it is a share-of-CPU figure, not a share-of-wall figure.
///
/// This exists to answer "is the deopt worth attacking?" with a number. The
/// lane count alone cannot: a deopted lane costs far more than a normal one,
/// so a 1%-of-lanes deopt is not a 1%-of-time deopt.
static DEOPT_NANOS: AtomicU64 = AtomicU64::new(0);

pub fn record_deopt_nanos(nanos: u64) {
    DEOPT_NANOS.fetch_add(nanos, Ordering::Relaxed);
}

/// Per-frame deopt CPU time, resetting the counter.
pub fn take_deopt_nanos() -> u64 {
    DEOPT_NANOS.swap(0, Ordering::Relaxed)
}

/// Per-frame collapse figures, resetting the counters.
///
/// Returns (constructions, mixed, definite_lanes, total_lanes). The
/// per-frame breakdown is what says whether partitioning a mixed
/// comparison would split a state ONCE or repeatedly: compare `mixed`
/// against the frame's fragment count. Many mixed constructions per
/// fragment would mean each partition gets re-split by the next
/// comparison, and the state count grows multiplicatively rather than by
/// one.
pub fn take_unknown_collapse() -> (u64, u64, u64, u64) {
    (
        COLLAPSE_CALLS.swap(0, Ordering::Relaxed),
        COLLAPSE_MIXED.swap(0, Ordering::Relaxed),
        COLLAPSE_DEFINITE.swap(0, Ordering::Relaxed),
        COLLAPSE_TOTAL.swap(0, Ordering::Relaxed),
    )
}

pub fn report_unknown_collapse() {
    let calls = COLLAPSE_CALLS.load(Ordering::Relaxed);
    if calls == 0 {
        return;
    }
    let total = COLLAPSE_TOTAL.load(Ordering::Relaxed).max(1);
    let definite = COLLAPSE_DEFINITE.load(Ordering::Relaxed);
    println!(
        "UNKNOWNBOOL COLLAPSE: {} constructions, {} mixed ({:.1}%); \
         {} of {} lanes had a definite answer ({:.2}%)",
        calls,
        COLLAPSE_MIXED.load(Ordering::Relaxed),
        100.0 * COLLAPSE_MIXED.load(Ordering::Relaxed) as f64 / calls as f64,
        definite,
        total,
        100.0 * definite as f64 / total as f64,
    );
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
    SELECT_UNIFORM_CALLS.store(0, Ordering::Relaxed);
    SELECT_UNIFORM_DISCARD_LANES.store(0, Ordering::Relaxed);
    SELECT_MIXED_CALLS.store(0, Ordering::Relaxed);
    {
        let mut seen = MEMO_SEEN.lock().unwrap();
        seen.0.clear();
        seen.1.clear();
    }
    MEMO_CALLS.store(0, Ordering::Relaxed);
    MEMO_HITS.store(0, Ordering::Relaxed);
    MEMO_HIT_ELEMS.store(0, Ordering::Relaxed);
    MEMO_ELEMS.store(0, Ordering::Relaxed);
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
    GC_CELLS_BEFORE.store(0, Ordering::Relaxed);
    GC_CELLS_AFTER.store(0, Ordering::Relaxed);
    FILTER_HEAP_CELLS.store(0, Ordering::Relaxed);
    FILTER_LOCAL_SLOTS.store(0, Ordering::Relaxed);
    branch_site_stats().lock().unwrap().clear();
    for i in 0..REASON_COUNT {
        FILTER_REASON_CALLS[i].store(0, Ordering::Relaxed);
        FILTER_REASON_KEPT[i].store(0, Ordering::Relaxed);
        FILTER_REASON_NANOS[i].store(0, Ordering::Relaxed);
        FILTER_REASON_RUNS[i].store(0, Ordering::Relaxed);
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
    report_unknown_collapse();
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
    for i in 0..REASON_COUNT {
        let calls = FILTER_REASON_CALLS[i].load(Ordering::Relaxed);
        if calls == 0 {
            continue;
        }
        let kept = FILTER_REASON_KEPT[i].load(Ordering::Relaxed);
        let runs = FILTER_REASON_RUNS[i].load(Ordering::Relaxed);
        eprintln!(
            "  {:<18} {:>7} state filters, {:>9.1} M lanes kept, {:>6.2} s, {:>6.1} lanes/run",
            REASON_NAMES[i],
            calls,
            kept as f64 / 1e6,
            FILTER_REASON_NANOS[i].load(Ordering::Relaxed) as f64 / 1e9,
            kept as f64 / runs.max(1) as f64,
        );
    }
    let memo_calls = MEMO_CALLS.load(Ordering::Relaxed);
    if memo_calls > 0 {
        let hits = MEMO_HITS.load(Ordering::Relaxed);
        eprintln!(
            "op memo pricing: {} vector-op calls, {} ({:.1}%) with previously seen input Arcs; {:.1} of {:.1} M output elems would be memo clones",
            memo_calls,
            hits,
            100.0 * hits as f64 / memo_calls as f64,
            MEMO_HIT_ELEMS.load(Ordering::Relaxed) as f64 / 1e6,
            MEMO_ELEMS.load(Ordering::Relaxed) as f64 / 1e6,
        );
    }
    let uniform_selects = SELECT_UNIFORM_CALLS.load(Ordering::Relaxed);
    if uniform_selects > 0 {
        eprintln!(
            "selects: {} uniform-mask (routing) vs {} mixed; {:.1} M vector-arm lanes discarded by uniform masks (class-dead speculation, foldable per class)",
            uniform_selects,
            SELECT_MIXED_CALLS.load(Ordering::Relaxed),
            SELECT_UNIFORM_DISCARD_LANES.load(Ordering::Relaxed) as f64 / 1e6,
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
