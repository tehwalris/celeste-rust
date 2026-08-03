//! Data-volume counters for the merge machinery (`vectorize_states`).
//!
//! `bench --profile` says merging costs ~half of wall clock, but not how much
//! data it actually touches. These counters record the real task size - states
//! grouped, cells concatenated, rows hashed - so the span times can be turned
//! into a throughput figure and compared against what the operation
//! fundamentally requires (hashing / copying that many elements). Counting is
//! per merge call, not per element, so the overhead is negligible and the
//! counters are always on; `bench` resets them at the start of a run.

use std::sync::atomic::{AtomicU64, Ordering};

static VECTORIZE_CALLS: AtomicU64 = AtomicU64::new(0);
static STATES_IN: AtomicU64 = AtomicU64::new(0);
static STATES_OUT: AtomicU64 = AtomicU64::new(0);
static GROUPS: AtomicU64 = AtomicU64::new(0);

static CONCAT_STATES: AtomicU64 = AtomicU64::new(0);
static CONCAT_CELLS: AtomicU64 = AtomicU64::new(0);

static DEDUP_CALLS: AtomicU64 = AtomicU64::new(0);
static DEDUP_ROWS: AtomicU64 = AtomicU64::new(0);
static DEDUP_COLS: AtomicU64 = AtomicU64::new(0);
static DEDUP_ELEMS: AtomicU64 = AtomicU64::new(0);
static DEDUP_ROWS_REMOVED: AtomicU64 = AtomicU64::new(0);
static DEDUP_HEAP_CELLS: AtomicU64 = AtomicU64::new(0);

pub fn reset() {
    for counter in [
        &VECTORIZE_CALLS,
        &STATES_IN,
        &STATES_OUT,
        &GROUPS,
        &CONCAT_STATES,
        &CONCAT_CELLS,
        &DEDUP_CALLS,
        &DEDUP_ROWS,
        &DEDUP_COLS,
        &DEDUP_ELEMS,
        &DEDUP_ROWS_REMOVED,
        &DEDUP_HEAP_CELLS,
    ] {
        counter.store(0, Ordering::Relaxed);
    }
}

pub fn record_vectorize(states_in: usize, groups: usize, states_out: usize) {
    VECTORIZE_CALLS.fetch_add(1, Ordering::Relaxed);
    STATES_IN.fetch_add(states_in as u64, Ordering::Relaxed);
    GROUPS.fetch_add(groups as u64, Ordering::Relaxed);
    STATES_OUT.fetch_add(states_out as u64, Ordering::Relaxed);
}

/// One group of same-shape states got concatenated: `states` inputs, each
/// with `cells` heap cells (scalar or vector) that were cloned into the
/// merged state.
pub fn record_concat(states: usize, cells: usize) {
    CONCAT_STATES.fetch_add(states as u64, Ordering::Relaxed);
    CONCAT_CELLS.fetch_add((states * cells) as u64, Ordering::Relaxed);
}

/// One merged state got row-deduped: `rows` lanes, `cols` vector columns
/// (each hashed per row), `heap_cells` total heap cells in the merged state
/// (the non-column remainder stayed scalar - equal across all lanes, so not
/// part of the row key), `removed` duplicate rows.
pub fn record_dedup(rows: usize, cols: usize, heap_cells: usize, removed: usize) {
    DEDUP_CALLS.fetch_add(1, Ordering::Relaxed);
    DEDUP_ROWS.fetch_add(rows as u64, Ordering::Relaxed);
    DEDUP_COLS.fetch_add(cols as u64, Ordering::Relaxed);
    DEDUP_ELEMS.fetch_add((rows * cols) as u64, Ordering::Relaxed);
    DEDUP_ROWS_REMOVED.fetch_add(removed as u64, Ordering::Relaxed);
    DEDUP_HEAP_CELLS.fetch_add(heap_cells as u64, Ordering::Relaxed);
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Snapshot {
    pub vectorize_calls: u64,
    pub states_in: u64,
    pub states_out: u64,
    pub groups: u64,
    pub concat_states: u64,
    pub concat_cells: u64,
    pub dedup_calls: u64,
    pub dedup_rows: u64,
    pub dedup_cols: u64,
    pub dedup_elems: u64,
    pub dedup_rows_removed: u64,
    pub dedup_heap_cells: u64,
}

pub fn snapshot() -> Snapshot {
    Snapshot {
        vectorize_calls: VECTORIZE_CALLS.load(Ordering::Relaxed),
        states_in: STATES_IN.load(Ordering::Relaxed),
        states_out: STATES_OUT.load(Ordering::Relaxed),
        groups: GROUPS.load(Ordering::Relaxed),
        concat_states: CONCAT_STATES.load(Ordering::Relaxed),
        concat_cells: CONCAT_CELLS.load(Ordering::Relaxed),
        dedup_calls: DEDUP_CALLS.load(Ordering::Relaxed),
        dedup_rows: DEDUP_ROWS.load(Ordering::Relaxed),
        dedup_cols: DEDUP_COLS.load(Ordering::Relaxed),
        dedup_elems: DEDUP_ELEMS.load(Ordering::Relaxed),
        dedup_rows_removed: DEDUP_ROWS_REMOVED.load(Ordering::Relaxed),
        dedup_heap_cells: DEDUP_HEAP_CELLS.load(Ordering::Relaxed),
    }
}

#[cfg(test)]
mod tests {
    // Counters are global, and other tests exercise the interpreter (and so
    // the merge machinery) concurrently, so only monotonicity is checked -
    // not absolute values, and no reset() (it would race).
    #[test]
    fn counters_accumulate() {
        let before = super::snapshot();
        super::record_vectorize(10, 3, 3);
        super::record_concat(4, 279);
        super::record_dedup(100, 17, 279, 89);
        let after = super::snapshot();
        assert!(after.vectorize_calls >= before.vectorize_calls + 1);
        assert!(after.states_in >= before.states_in + 10);
        assert!(after.concat_cells >= before.concat_cells + 4 * 279);
        assert!(after.dedup_elems >= before.dedup_elems + 1700);
        assert!(after.dedup_rows_removed >= before.dedup_rows_removed + 89);
    }
}
