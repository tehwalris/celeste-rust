//! How compressible is the state set at a merge?
//!
//! Diagnostic only, gated on `CELESTE_DUMP_MERGE`. At a merge point - after
//! `gc`, so heaps are canonical and garbage-free - every state's per-lane
//! data is written out as raw binary and squeezed into a single zstd stream.
//! One bundle for the whole merge, so cross-state redundancy counts: 97% of
//! the rows arriving at dedup are duplicates of some other row, and the
//! question is how much of the remaining volume is structure a smarter
//! representation could exploit.
//!
//! Two layouts of exactly the same bytes are measured, because they answer
//! different questions:
//!
//!   * **column-major** - each column's lanes contiguous, which is how the
//!     interpreter actually stores them. Compresses well when a column is
//!     smooth or mostly constant across lanes.
//!   * **row-major** - each lane's columns contiguous, one game state per
//!     record. Compresses well when whole rows repeat.
//!
//! Scalars are broadcast to full width so both layouts contain identical
//! content and their ratios are directly comparable.

use std::io::Write;

use super::state::State;
use super::heap::HeapId;
use super::value::HeapValue;
use super::virtual_merge::{collect_columns_labeled, Column, Origin, Piece};

/// Resolves heap cells to the field path that reaches them, so a column can
/// be named `player.spd.x` rather than `cell 49`.
pub fn cell_names(state: &State) -> std::collections::HashMap<usize, String> {
    let mut names: std::collections::HashMap<usize, String> = Default::default();
    for (global, id) in state.global_env.iter() {
        names.insert(id.raw(), global.clone());
    }
    // Several passes so nesting resolves even when a table is visited before
    // its parent is named. Names propagate through pointer cells and array
    // elements, so objects reached only via `objects[i]` still get full paths
    // (objects.1.rem.x) rather than falling back to cellN.x.
    for _ in 0..5 {
        for i in 0..state.heap.len() {
            match state.heap.get_opt(HeapId::from_raw(i)) {
                Some(HeapValue::ObjectTable(fields)) => {
                    let parent = names.get(&i).cloned();
                    for (field, child) in fields.iter() {
                        let label = match &parent {
                            Some(p) => format!("{}.{}", p, field),
                            None => format!("cell{}.{}", i, field),
                        };
                        names.entry(child.raw()).or_insert(label);
                    }
                }
                Some(HeapValue::ArrayTable(items)) => {
                    let parent = names.get(&i).cloned();
                    for (k, child) in items.iter().enumerate() {
                        let label = match &parent {
                            Some(p) => format!("{}.{}", p, k + 1),
                            None => format!("cell{}.{}", i, k + 1),
                        };
                        names.entry(child.raw()).or_insert(label);
                    }
                }
                Some(HeapValue::Value(super::value::Value::Pointer(target))) => {
                    if let Some(name) = names.get(&i).cloned() {
                        names.entry(target.raw()).or_insert(name);
                    }
                }
                _ => {}
            }
        }
    }
    names
}

/// Field-path names for a state's columns, in collection order.
fn column_labels(state: &State) -> Vec<String> {
    let names = cell_names(state);
    let Some((_, origins)) = collect_columns_labeled(std::slice::from_ref(state)) else {
        return Vec::new();
    };
    origins
        .iter()
        .map(|origin| match origin {
            Origin::Heap(cell) => names
                .get(cell)
                .cloned()
                .unwrap_or_else(|| format!("cell{}", cell)),
            Origin::HeapCapture(cell, k) => format!("cell{}.capture{}", cell, k),
            Origin::Local(slot) => format!("local{}", slot),
            Origin::Outer(d, slot) => format!("outer{}.local{}", d, slot),
        })
        .collect()
}

/// One column of one state, as raw little-endian bytes.
struct RawColumn {
    width: usize,
    bytes: Vec<u8>,
}

fn to_raw(column: &Column, rows: usize) -> RawColumn {
    fn fill<T: Copy, const W: usize>(
        pieces: &[Piece<T>],
        rows: usize,
        encode: impl Fn(T) -> [u8; W],
    ) -> RawColumn {
        let mut bytes = Vec::with_capacity(rows * W);
        for piece in pieces {
            match piece {
                Piece::Slice(s) => {
                    for v in s.iter() {
                        bytes.extend_from_slice(&encode(*v));
                    }
                }
                Piece::Scalar(v, n) => {
                    let e = encode(*v);
                    for _ in 0..*n {
                        bytes.extend_from_slice(&e);
                    }
                }
            }
        }
        RawColumn { width: W, bytes }
    }
    match column {
        Column::Numbers(p) => fill::<_, 4>(p, rows, |v| v.to_bits().to_le_bytes()),
        Column::Bools(p) => fill::<_, 1>(p, rows, |v| [v as u8]),
        Column::Intervals(p) => fill::<_, 8>(p, rows, |v| {
            let mut out = [0u8; 8];
            out[..4].copy_from_slice(&v.low.to_bits().to_le_bytes());
            out[4..].copy_from_slice(&v.high.to_bits().to_le_bytes());
            out
        }),
    }
}

fn zstd_len(data: &[u8], level: i32) -> usize {
    let mut encoder = zstd::Encoder::new(Vec::new(), level).expect("zstd encoder");
    encoder.write_all(data).expect("zstd write");
    encoder.finish().expect("zstd finish").len()
}

/// Distinct values per column, which is what a dictionary encoding would
/// have to store. Keys are the element's raw bytes widened to u64, so
/// equality here is exactly value equality (see `Pico8Num::to_bits`).
fn distinct_per_column(per_state: &[Vec<RawColumn>]) -> Vec<(usize, usize)> {
    let Some(first) = per_state.first() else {
        return Vec::new();
    };
    let mut out = Vec::with_capacity(first.len());
    for (index, col) in first.iter().enumerate() {
        let mut seen: rustc_hash::FxHashSet<u64> = Default::default();
        let mut lanes = 0usize;
        for state_cols in per_state {
            // Fragments of a different shape have a different column list;
            // counting them positionally would mix unrelated fields.
            let Some(c) = state_cols.get(index) else { continue };
            if c.width != col.width {
                continue;
            }
            lanes += c.bytes.len() / c.width;
            for chunk in c.bytes.chunks_exact(c.width) {
                let mut key = 0u64;
                for (i, b) in chunk.iter().enumerate() {
                    key |= (*b as u64) << (8 * i);
                }
                seen.insert(key);
            }
        }
        out.push((lanes, seen.len()));
    }
    out
}

/// Tests whether the surviving rows are *hierarchical*: many rows sharing a
/// prefix of field values and differing only in a suffix, nested.
///
/// Columns are visited cheapest-first (fewest distinct values), and after
/// each one we count how many distinct prefixes exist. A set of rows drawn
/// independently from the per-field domains would multiply out by each
/// column's full cardinality; a hierarchy branches by far less, and the
/// profile shows exactly where the branching happens.
fn prefix_profile(state: &State) {
    let Some((columns, _)) = collect_columns_labeled(std::slice::from_ref(state)) else {
        return;
    };
    let raw: Vec<RawColumn> = columns
        .iter()
        .map(|c| to_raw(c, state.vector_size))
        .collect();
    let labels = column_labels(state);
    let rows = state.vector_size;
    if rows == 0 {
        return;
    }

    let key_at = |c: &RawColumn, row: usize| -> u64 {
        let at = row * c.width;
        let mut key = 0u64;
        for (i, b) in c.bytes[at..at + c.width].iter().enumerate() {
            key |= (*b as u64) << (8 * i);
        }
        key
    };

    // Cheapest columns first: a compact hierarchy, if there is one, shows up
    // as long stretches with a branching factor near 1.
    let mut order: Vec<usize> = (0..raw.len()).collect();
    let cardinality: Vec<usize> = raw
        .iter()
        .map(|c| {
            let mut seen: rustc_hash::FxHashSet<u64> = Default::default();
            for row in 0..rows {
                seen.insert(key_at(c, row));
            }
            seen.len()
        })
        .collect();
    order.sort_by_key(|i| (cardinality[*i], *i));

    eprintln!(
        "               prefix profile of the merged rows ({} rows, cheapest field first):",
        rows
    );
    let mut hashes = vec![0xcbf2_9ce4_8422_2325u64; rows];
    let mut previous = 1usize;
    for index in order {
        let column = &raw[index];
        for (row, h) in hashes.iter_mut().enumerate() {
            *h = (h.rotate_left(26) ^ key_at(column, row)).wrapping_mul(0x9e37_79b9_7f4a_7c15);
        }
        let distinct: rustc_hash::FxHashSet<u64> = hashes.iter().copied().collect();
        let distinct = distinct.len();
        // Only print where the structure actually changes.
        if distinct != previous {
            eprintln!(
                "                 +{:<26} card {:>4}  ->{:>9} distinct prefixes  (x{:.2})",
                labels.get(index).cloned().unwrap_or_default(),
                cardinality[index],
                distinct,
                distinct as f64 / previous as f64,
            );
            previous = distinct;
        }
    }
}

/// What one side of the merge costs, stored and stored-compressed.
pub struct Sizes {
    pub states: usize,
    pub lanes: usize,
    pub columns: usize,
    pub raw: usize,
    pub col_major: usize,
    pub row_major: usize,
    /// (lanes, distinct values) per column.
    pub distinct: Vec<(usize, usize)>,
    pub labels: Vec<String>,
}

/// Measures, if `CELESTE_DUMP_MERGE` is set and this merge is at least that
/// many lanes (default 1,000,000 - small merges are noise). `detail` also
/// prints the per-column breakdown.
pub fn measure(states: &[State], detail: bool) -> Option<Sizes> {
    measure_with_threshold(states, detail, enabled_threshold()?)
}

/// Cached: `vectorize_states` runs ~100 times a frame and this is off in
/// every normal run.
fn enabled_threshold() -> Option<usize> {
    static THRESHOLD: std::sync::OnceLock<Option<usize>> = std::sync::OnceLock::new();
    *THRESHOLD.get_or_init(|| {
        std::env::var_os("CELESTE_DUMP_MERGE").map(|setting| {
            setting
                .to_str()
                .and_then(|s| s.parse().ok())
                .unwrap_or(1_000_000)
        })
    })
}

/// The merged side is much smaller than the threshold that selected the
/// merge, so it is always measured once the unmerged side qualified.
fn measure_with_threshold(states: &[State], detail: bool, threshold: usize) -> Option<Sizes> {
    let total_rows: usize = states.iter().map(|s| s.vector_size).sum();
    if total_rows < threshold || states.is_empty() {
        return None;
    }

    // Per-state columns, since states at one merge need not share a shape.
    let mut per_state: Vec<Vec<RawColumn>> = Vec::with_capacity(states.len());
    for state in states {
        let Some((columns, _)) = collect_columns_labeled(std::slice::from_ref(state)) else {
            eprintln!("[merge dump] skipped a state whose columns would not collect");
            return None;
        };
        per_state.push(
            columns
                .iter()
                .map(|c| to_raw(c, state.vector_size))
                .collect(),
        );
    }

    let raw_bytes: usize = per_state
        .iter()
        .flat_map(|cs| cs.iter())
        .map(|c| c.bytes.len())
        .sum();
    let columns: usize = per_state.first().map_or(0, |cs| cs.len());

    let mut col_major = Vec::with_capacity(raw_bytes);
    for cols in &per_state {
        for c in cols {
            col_major.extend_from_slice(&c.bytes);
        }
    }

    let mut row_major = Vec::with_capacity(raw_bytes);
    for (cols, state) in per_state.iter().zip(states) {
        for row in 0..state.vector_size {
            for c in cols {
                let at = row * c.width;
                row_major.extend_from_slice(&c.bytes[at..at + c.width]);
            }
        }
    }

    let mb = |n: usize| n as f64 / (1024.0 * 1024.0);
    let sizes = Sizes {
        states: states.len(),
        lanes: total_rows,
        columns,
        raw: raw_bytes,
        col_major: zstd_len(&col_major, 3),
        row_major: zstd_len(&row_major, 3),
        distinct: distinct_per_column(&per_state),
        labels: column_labels(&states[0]),
    };

    // Per-column, on the largest state, so the ratios are not diluted by
    // whichever columns happen to be constant in small fragments.
    if detail {
    if let Some((cols, state)) = per_state
        .iter()
        .zip(states)
        .max_by_key(|(_, s)| s.vector_size)
    {
        eprintln!(
            "               per-column on the largest state ({} lanes):",
            state.vector_size
        );
        let names = cell_names(state);
        let origins = collect_columns_labeled(std::slice::from_ref(state))
            .map(|(_, o)| o)
            .unwrap_or_default();
        let label = |i: usize| -> String {
            match origins.get(i) {
                Some(Origin::Heap(cell)) => names
                    .get(cell)
                    .cloned()
                    .unwrap_or_else(|| format!("cell{}", cell)),
                Some(Origin::HeapCapture(cell, k)) => format!("cell{}.capture{}", cell, k),
                Some(Origin::Local(slot)) => format!("local{}", slot),
                Some(Origin::Outer(d, slot)) => format!("outer{}.local{}", d, slot),
                None => format!("col{}", i),
            }
        };
        let mut rows: Vec<(usize, usize, usize, usize)> = cols
            .iter()
            .enumerate()
            .map(|(i, c)| (i, c.width, c.bytes.len(), zstd_len(&c.bytes, 3)))
            .collect();
        rows.sort_by_key(|(_, _, _, packed)| std::cmp::Reverse(*packed));
        for (i, width, raw, packed) in rows.iter().take(20) {
            eprintln!(
                "                 {:<26} ({}B) {:>7.2} MiB -> {:>8.3} MiB  {:>7.1}x",
                label(*i),
                width,
                mb(*raw),
                mb(*packed),
                *raw as f64 / *packed as f64,
            );
        }
    }
    }
    let _ = mb;
    Some(sizes)
}

/// Prints the 2x2 the merge is really about: the fragments as they arrive
/// versus the single deduplicated state they become, each stored raw and
/// stored compressed. Dedup and compression both attack redundancy, and the
/// interesting question is how much of one the other already covers.
pub fn report(before: Option<Sizes>, after: &[State]) {
    let Some(before) = before else { return };
    let Some(after) = measure_with_threshold(after, false, 1) else {
        eprintln!("[merge dump] merged side below threshold; before was {} lanes", before.lanes);
        return;
    };
    let mb = |n: usize| n as f64 / (1024.0 * 1024.0);
    eprintln!(
        "[merge dump] {} fragments / {} lanes  ->  {} state(s) / {} lanes  ({:.1}x fewer lanes)",
        before.states,
        before.lanes,
        after.states,
        after.lanes,
        before.lanes as f64 / after.lanes.max(1) as f64,
    );
    eprintln!(
        "               {:<22} {:>12} {:>12} {:>10}",
        "", "unmerged", "merged", "merge gain"
    );
    let row = |name: &str, b: usize, a: usize| {
        eprintln!(
            "               {:<22} {:>9.1} MiB {:>9.1} MiB {:>9.1}x",
            name,
            mb(b),
            mb(a),
            b as f64 / a.max(1) as f64,
        );
    };
    row("uncompressed", before.raw, after.raw);
    row("zstd-3 column-major", before.col_major, after.col_major);
    row("zstd-3 row-major", before.row_major, after.row_major);
    eprintln!(
        "               compression gain: unmerged {:.1}x, merged {:.1}x (column-major)",
        before.raw as f64 / before.col_major as f64,
        after.raw as f64 / after.col_major as f64,
    );

    eprintln!(
        "               {:<26} {:>22} {:>22}",
        "distinct values per field", "unmerged", "merged"
    );
    let mut rows: Vec<(String, (usize, usize), (usize, usize))> = before
        .distinct
        .iter()
        .enumerate()
        .map(|(i, b)| {
            (
                before.labels.get(i).cloned().unwrap_or_default(),
                *b,
                after.distinct.get(i).copied().unwrap_or((0, 0)),
            )
        })
        .collect();
    rows.sort_by_key(|(_, b, _)| std::cmp::Reverse(b.1));
    let _ = &rows;
    for (label, (lanes_b, uniq_b), (lanes_a, uniq_a)) in rows.iter().take(24) {
        eprintln!(
            "               {:<26} {:>9} /{:>9} ({:>5.2}%) {:>8} /{:>7} ({:>5.2}%)",
            label,
            uniq_b,
            lanes_b,
            100.0 * *uniq_b as f64 / (*lanes_b).max(1) as f64,
            uniq_a,
            lanes_a,
            100.0 * *uniq_a as f64 / (*lanes_a).max(1) as f64,
        );
    }
}

/// Runs the hierarchy test on the merged state.
pub fn report_structure(after: &[State]) {
    if enabled_threshold().is_none() {
        return;
    }
    if let Some(state) = after.iter().max_by_key(|s| s.vector_size) {
        if state.vector_size >= 100_000 {
            prefix_profile(state);
        }
    }
}
