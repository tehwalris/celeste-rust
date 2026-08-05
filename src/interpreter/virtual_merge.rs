//! Merging and deduplicating a merge group *without* concatenating it.
//!
//! A merge takes ~700 same-shape fragments, concatenates every column
//! across them into one huge table (23.6M rows at frame 37), hashes those
//! rows, and then filters the ~10% survivors out again. 90-97% of the rows
//! are duplicates, so the concatenation is mostly building data that is
//! immediately thrown away - 0.44s of frame 37's merge_groups, plus the
//! peak memory to hold a table whose survivors are a small fraction of it.
//!
//! This module computes the survivor set over a *virtual* concatenation -
//! the columns stay as per-fragment pieces - and then gathers only the
//! survivors into the merged state. It has been tried before (three
//! variants, all parked as regressions), and the overnight log names
//! exactly why each lost. Both causes are addressed here:
//!
//!   * **Uniform columns.** Materialising collapses a column whose value is
//!     identical in every fragment and lane back to a `Scalar`, and
//!     collapsed columns then drop out of the dedup key entirely (measured:
//!     17.1 key columns materialised versus 19.0 virtual). A virtual layout
//!     loses that for free, so it is done explicitly here - `uniform_scalar`
//!     scans each column with an early exit, which is cheap precisely
//!     because non-uniform columns bail after a few elements.
//!
//!   * **Segmented random access.** The earlier attempt paid 4.2 vs 3.0
//!     ns/element because verifying a duplicate meant locating which piece
//!     a row index fell in. Every access this module needs - hashing,
//!     packing representatives, verifying, and the final gather - visits
//!     rows in ascending order, so each is a sorted walk over the pieces
//!     with no search and no per-element dispatch. The one exception is the
//!     re-check of hash-contaminated classes, which needs random access and
//!     builds per-column offset tables on demand; at ~1e-5 expected
//!     collisions per merge that path is a correctness fallback, not a
//!     cost.
//!
//! The output is *identical* to the materialised pipeline
//! (`vectorize_same_shape_states` + `dedup_vectorized_state` +
//! `unvectorize_if_possible`): the hash arithmetic and seed match
//! `vectorize::hash_rows` exactly, so the survivor mask matches, and the
//! survivor gather produces the same lanes in the same order. A test in
//! `vectorize.rs` holds the two paths equal.

use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::heap::{Heap, HeapId};
use super::state::State;
use super::tracing::TraceSpan;
use super::value::{HeapValue, MaybeVector, Value};

use rustc_hash::{FxHashMap, FxHashSet};

/// One fragment's contribution to one column: either a per-lane run of
/// values, or a single value broadcast over that fragment's lanes.
#[derive(Clone, Copy)]
pub enum Piece<'a, T> {
    Slice(&'a [T]),
    Scalar(T, usize),
}

impl<'a, T: Copy + PartialEq> Piece<'a, T> {
    fn len(&self) -> usize {
        match self {
            Piece::Slice(s) => s.len(),
            Piece::Scalar(_, n) => *n,
        }
    }

    /// Whether every lane of this piece equals `value`. Early-exits, which
    /// is what makes the uniform scan affordable on columns that are not.
    fn all_equal_to(&self, value: &T) -> bool {
        match self {
            Piece::Slice(s) => s.iter().all(|v| v == value),
            Piece::Scalar(v, _) => v == value,
        }
    }

    fn first(&self) -> Option<T> {
        match self {
            Piece::Slice(s) => s.first().copied(),
            Piece::Scalar(v, n) => (*n > 0).then_some(*v),
        }
    }
}

/// Walk `rows` (ascending global indices) through `pieces`, yielding the
/// k-th requested value. The cursor only ever moves forward - this is the
/// sorted-walk access pattern the module is built around.
fn visit_rows_impl<T: Copy + PartialEq, O: Copy>(
    pieces: &[Piece<T>],
    rows: &[u32],
    to_out: impl Fn(T) -> O,
    mut f: impl FnMut(usize, O),
) {
    let mut piece_idx = 0usize;
    let mut piece_start = 0usize;
    for (k, &r) in rows.iter().enumerate() {
        let r = r as usize;
        debug_assert!(r >= piece_start, "rows must be ascending");
        while r - piece_start >= pieces[piece_idx].len() {
            piece_start += pieces[piece_idx].len();
            piece_idx += 1;
        }
        let v = match &pieces[piece_idx] {
            Piece::Slice(s) => s[r - piece_start],
            Piece::Scalar(v, _) => *v,
        };
        f(k, to_out(v));
    }
}

/// Visit every row of the virtual column in order, yielding (row, value).
fn visit_all_impl<T: Copy + PartialEq, O: Copy>(
    pieces: &[Piece<T>],
    to_out: impl Fn(T) -> O,
    mut f: impl FnMut(usize, O),
) {
    let mut i = 0usize;
    for piece in pieces {
        match piece {
            Piece::Slice(s) => {
                for v in s.iter() {
                    f(i, to_out(*v));
                    i += 1;
                }
            }
            Piece::Scalar(v, n) => {
                let out = to_out(*v);
                for _ in 0..*n {
                    f(i, out);
                    i += 1;
                }
            }
        }
    }
}

/// A column value widened to at most two u32 words, the same packing
/// `bucket_unique_mask` uses for its dense representative table.
type Words = (u32, u32);

/// A column of the virtual concatenation, one piece per fragment.
pub enum Column<'a> {
    Numbers(Vec<Piece<'a, Pico8Num>>),
    Bools(Vec<Piece<'a, bool>>),
    Intervals(Vec<Piece<'a, Pico8NumInterval>>),
}

impl<'a> Column<'a> {
    /// The single scalar this column collapses to, if no two rows can be
    /// told apart by it. Such a column leaves the dedup key and the merged
    /// state stores it as a `Scalar` - the collapse that materialising
    /// would have done for free in `merge_values`.
    pub fn uniform_scalar(&self) -> Option<Value> {
        fn check<T: Copy + PartialEq>(pieces: &[Piece<T>]) -> Option<T> {
            let first = pieces.iter().find_map(|p| p.first())?;
            pieces
                .iter()
                .all(|p| p.all_equal_to(&first))
                .then_some(first)
        }
        match self {
            Column::Numbers(p) => check(p).map(|v| Value::Number(MaybeVector::Scalar(v))),
            Column::Bools(p) => check(p).map(|v| Value::Bool(MaybeVector::Scalar(v))),
            Column::Intervals(p) => {
                check(p).map(|v| Value::NumberInterval(MaybeVector::Scalar(v)))
            }
        }
    }

    /// True when no two rows can be told apart by this column.
    pub fn is_uniform(&self) -> bool {
        self.uniform_scalar().is_some()
    }

    /// How many u32 words a packed value of this column occupies.
    fn words(&self) -> usize {
        match self {
            Column::Intervals(_) => 2,
            Column::Numbers(_) | Column::Bools(_) => 1,
        }
    }

    fn total_bytes(&self) -> usize {
        match self {
            Column::Numbers(p) => p.iter().map(|x| x.len()).sum::<usize>() * 4,
            Column::Intervals(p) => p.iter().map(|x| x.len()).sum::<usize>() * 8,
            Column::Bools(p) => p.iter().map(|x| x.len()).sum::<usize>(),
        }
    }

    /// Visit the ascending `rows`, yielding (k, packed words).
    fn visit_rows_words(&self, rows: &[u32], f: impl FnMut(usize, Words)) {
        match self {
            Column::Numbers(p) => visit_rows_impl(p, rows, |v| (v.to_bits(), 0), f),
            Column::Bools(p) => visit_rows_impl(p, rows, |v| (v as u32, 0), f),
            Column::Intervals(p) => {
                visit_rows_impl(p, rows, |v| (v.low.to_bits(), v.high.to_bits()), f)
            }
        }
    }

    /// Visit every row in order, yielding (row, packed words).
    fn visit_all_words(&self, f: impl FnMut(usize, Words)) {
        match self {
            Column::Numbers(p) => visit_all_impl(p, |v| (v.to_bits(), 0), f),
            Column::Bools(p) => visit_all_impl(p, |v| (v as u32, 0), f),
            Column::Intervals(p) => {
                visit_all_impl(p, |v| (v.low.to_bits(), v.high.to_bits()), f)
            }
        }
    }

    /// Piece start offsets, for the rare random-access fallback.
    fn offsets(&self) -> Vec<usize> {
        fn go<T: Copy + PartialEq>(pieces: &[Piece<T>]) -> Vec<usize> {
            let mut offsets = Vec::with_capacity(pieces.len() + 1);
            let mut at = 0usize;
            offsets.push(0);
            for p in pieces {
                at += p.len();
                offsets.push(at);
            }
            offsets
        }
        match self {
            Column::Numbers(p) => go(p),
            Column::Bools(p) => go(p),
            Column::Intervals(p) => go(p),
        }
    }

    /// Random access by global row index, given this column's `offsets()`.
    fn words_at(&self, offsets: &[usize], row: usize) -> Words {
        let piece_idx = offsets.partition_point(|&o| o <= row) - 1;
        let local = row - offsets[piece_idx];
        fn at<T: Copy + PartialEq>(pieces: &[Piece<T>], piece_idx: usize, local: usize) -> T {
            match &pieces[piece_idx] {
                Piece::Slice(s) => s[local],
                Piece::Scalar(v, _) => *v,
            }
        }
        match self {
            Column::Numbers(p) => (at(p, piece_idx, local).to_bits(), 0),
            Column::Bools(p) => (at(p, piece_idx, local) as u32, 0),
            Column::Intervals(p) => {
                let v = at(p, piece_idx, local);
                (v.low.to_bits(), v.high.to_bits())
            }
        }
    }

    /// Gather the ascending `kept` rows into a fresh value. Goes through
    /// `MaybeVector::vector`, which collapses a uniform gather to `Scalar` -
    /// though a non-uniform column stays non-uniform under dedup, since
    /// every distinct row survives by construction.
    fn gather(&self, kept: &[u32]) -> Value {
        fn go<T: std::fmt::Debug + Copy + PartialEq + Eq>(
            pieces: &[Piece<T>],
            kept: &[u32],
        ) -> MaybeVector<T> {
            let mut out: Vec<T> = Vec::with_capacity(kept.len());
            visit_rows_impl(pieces, kept, |v| v, |_, v| out.push(v));
            MaybeVector::vector(out)
        }
        match self {
            Column::Numbers(p) => Value::Number(go(p, kept)),
            Column::Bools(p) => Value::Bool(go(p, kept)),
            Column::Intervals(p) => Value::NumberInterval(go(p, kept)),
        }
    }
}

/// Collects every vectorizable leaf of the group as a virtual column.
///
/// The traversal must visit leaves in the same order as the merge itself
/// (heap cells ascending, then `local_env` slots, then each outer env), and
/// must include leaves that are `Scalar` in some fragments - shape equality
/// does not distinguish a scalar from a vector, so a column can be mixed.
pub fn collect_columns<'a>(states: &'a [State]) -> Option<Vec<Column<'a>>> {
    collect_columns_labeled(states).map(|(c, _)| c)
}

/// Where a column came from, so the state builder can check it is placing
/// each column at the leaf the traversal collected it from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    Heap(usize),
    HeapCapture(usize, usize),
    Local(usize),
    Outer(usize, usize),
}

pub fn collect_columns_labeled<'a>(
    states: &'a [State],
) -> Option<(Vec<Column<'a>>, Vec<Origin>)> {
    let first = &states[0];
    let mut columns: Vec<Column<'a>> = Vec::new();
    let mut origins: Vec<Origin> = Vec::new();

    let push_value = |columns: &mut Vec<Column<'a>>, values: Vec<&'a Value>| -> bool {
        match values[0] {
            Value::Number(_) => {
                let mut pieces = Vec::with_capacity(values.len());
                for (v, state) in values.iter().zip(states) {
                    match v {
                        Value::Number(MaybeVector::Vector(x)) => pieces.push(Piece::Slice(&x[..])),
                        Value::Number(MaybeVector::Scalar(x)) => {
                            pieces.push(Piece::Scalar(*x, state.vector_size))
                        }
                        _ => return false,
                    }
                }
                columns.push(Column::Numbers(pieces));
            }
            Value::Bool(_) => {
                let mut pieces = Vec::with_capacity(values.len());
                for (v, state) in values.iter().zip(states) {
                    match v {
                        Value::Bool(MaybeVector::Vector(x)) => pieces.push(Piece::Slice(&x[..])),
                        Value::Bool(MaybeVector::Scalar(x)) => {
                            pieces.push(Piece::Scalar(*x, state.vector_size))
                        }
                        _ => return false,
                    }
                }
                columns.push(Column::Bools(pieces));
            }
            Value::NumberInterval(_) => {
                let mut pieces = Vec::with_capacity(values.len());
                for (v, state) in values.iter().zip(states) {
                    match v {
                        Value::NumberInterval(MaybeVector::Vector(x)) => {
                            pieces.push(Piece::Slice(&x[..]))
                        }
                        Value::NumberInterval(MaybeVector::Scalar(x)) => {
                            pieces.push(Piece::Scalar(*x, state.vector_size))
                        }
                        _ => return false,
                    }
                }
                columns.push(Column::Intervals(pieces));
            }
            _ => {}
        }
        true
    };

    // Heap, in the same ascending order the merge rebuilds it.
    for i in 0..first.heap.len() {
        let id = HeapId::from_raw(i);
        let Some(first_hv) = first.heap.get_opt(id) else {
            continue;
        };
        match first_hv {
            HeapValue::Value(_) => {
                let mut values = Vec::with_capacity(states.len());
                for state in states {
                    match state.heap.get_opt(id) {
                        Some(HeapValue::Value(v)) => values.push(v),
                        _ => return None,
                    }
                }
                let before = columns.len();
                if !push_value(&mut columns, values) {
                    return None;
                }
                origins.extend((before..columns.len()).map(|_| Origin::Heap(i)));
            }
            HeapValue::Closure(_, caps) => {
                for capture in 0..caps.len() {
                    let mut values = Vec::with_capacity(states.len());
                    for state in states {
                        match state.heap.get_opt(id) {
                            Some(HeapValue::Closure(_, c)) if capture < c.len() => {
                                values.push(&c[capture])
                            }
                            _ => return None,
                        }
                    }
                    let before = columns.len();
                    if !push_value(&mut columns, values) {
                        return None;
                    }
                    origins.extend((before..columns.len()).map(|_| Origin::HeapCapture(i, capture)));
                }
            }
            _ => {}
        }
    }

    // Locals, positionally, exactly as `merge_local_envs` does.
    for (slot, _) in first.local_env.iter() {
        let values: Vec<&'a Value> = states
            .iter()
            .map(|s| s.local_env.get_by_raw_id(slot))
            .collect();
        let before = columns.len();
        if !push_value(&mut columns, values) {
            return None;
        }
        origins.extend((before..columns.len()).map(|_| Origin::Local(slot)));
    }
    for depth in 0..first.outer_local_envs.len() {
        for (slot, _) in first.outer_local_envs[depth].iter() {
            if states
                .iter()
                .any(|s| s.outer_local_envs.len() <= depth)
            {
                return None;
            }
            let values: Vec<&'a Value> = states
                .iter()
                .map(|s| s.outer_local_envs[depth].get_by_raw_id(slot))
                .collect();
            let before = columns.len();
            if !push_value(&mut columns, values) {
                return None;
            }
            origins.extend((before..columns.len()).map(|_| Origin::Outer(depth, slot)));
        }
    }

    Some((columns, origins))
}

/// Folds every key column into a per-row hash, piece by piece.
///
/// Identical arithmetic and seed to `vectorize::hash_rows`, so the survivor
/// mask - and therefore the merged output - matches the materialised
/// pipeline exactly. Identical access pattern too: each piece is contiguous
/// and visited once in order, so the per-piece dispatch amortises over
/// thousands of lanes rather than costing anything per element.
pub fn hash_rows(columns: &[&Column], total_rows: usize) -> Vec<u64> {
    use std::hash::{Hash, Hasher};
    #[inline(always)]
    fn elem_hash<T: Hash>(value: &T) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        value.hash(&mut hasher);
        hasher.finish()
    }
    #[inline(always)]
    fn fold(row: &mut u64, value: u64) {
        *row = (row.rotate_left(26) ^ value).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    }
    fn run<T: Hash + Copy + PartialEq>(pieces: &[Piece<T>], hashes: &mut [u64]) {
        let mut at = 0;
        for piece in pieces {
            let n = piece.len();
            match piece {
                Piece::Slice(s) => {
                    for (row, value) in hashes[at..at + n].iter_mut().zip(s.iter()) {
                        fold(row, elem_hash(value));
                    }
                }
                Piece::Scalar(v, _) => {
                    // One hash for the whole run - the value is the same in
                    // every lane of this fragment.
                    let h = elem_hash(v);
                    for row in hashes[at..at + n].iter_mut() {
                        fold(row, h);
                    }
                }
            }
            at += n;
        }
    }

    let t = crate::op_census::start();
    let mut hashes = vec![0x51_7c_c1_b7_27_22_0a_95u64; total_rows];
    for column in columns {
        match column {
            Column::Numbers(p) => run(p, &mut hashes),
            Column::Bools(p) => run(p, &mut hashes),
            Column::Intervals(p) => run(p, &mut hashes),
        }
    }
    let col_bytes: usize = columns.iter().map(|c| c.total_bytes()).sum();
    crate::op_census::record(
        crate::op_census::Cat::HashRows,
        total_rows * columns.len(),
        col_bytes + 16 * total_rows * columns.len(),
        t,
    );
    hashes
}

/// Which rows to keep: `true` for the first occurrence of each distinct
/// row, `false` for every later copy. The virtual twin of
/// `vectorize::bucket_unique_mask`, with the same four phases:
///
/// 1. Probe: representative = first row seen with this hash. No column
///    access at all.
/// 2. Pack the candidate-unique rows row-major into a dense word table, in
///    L2-sized tiles. `uniq` is ascending by construction (first
///    occurrences are discovered in row order), so this is a sorted walk
///    per column per tile.
/// 3. Verify each duplicate against its representative's packed row. Done
///    column-major here - each column is streamed once in row order, and
///    the random access goes into the dense table, which is small enough
///    to stay cached. An `ok` bitmap accumulates the comparison across
///    columns.
/// 4. Distinct rows sharing a 64-bit hash (expected ~1e-5 per merge) are
///    redone exactly, with on-demand offset tables for random access.
fn virtual_unique_mask(key: &[&Column], row_hashes: &[u64]) -> (Vec<bool>, usize) {
    let n = row_hashes.len();

    let mut first_of_hash: FxHashMap<u64, u32> =
        FxHashMap::with_capacity_and_hasher(n / 2, Default::default());
    let mut mask = vec![false; n];
    let mut dense_of_row: Vec<u32> = vec![0; n];
    let mut uniq: Vec<u32> = Vec::new();

    for (i, &row_hash) in row_hashes.iter().enumerate() {
        match first_of_hash.entry(row_hash) {
            std::collections::hash_map::Entry::Vacant(slot) => {
                let dense = uniq.len() as u32;
                slot.insert(dense);
                uniq.push(i as u32);
                dense_of_row[i] = dense;
                mask[i] = true;
            }
            std::collections::hash_map::Entry::Occupied(slot) => {
                dense_of_row[i] = *slot.get();
            }
        }
    }
    let mut unique_count = uniq.len();
    if unique_count == n {
        return (mask, unique_count);
    }

    // 2. Pack the representatives.
    let words_per_row: usize = key.iter().map(|c| c.words()).sum();
    let mut dense = vec![0u32; uniq.len() * words_per_row];
    const DENSE_TILE_BYTES: usize = 192 * 1024;
    let tile_rows = (DENSE_TILE_BYTES / (words_per_row.max(1) * 4)).max(1);
    for tile_start in (0..uniq.len()).step_by(tile_rows) {
        let tile_end = (tile_start + tile_rows).min(uniq.len());
        let tile = &uniq[tile_start..tile_end];
        let mut w = 0;
        for column in key {
            let words = column.words();
            column.visit_rows_words(tile, |k, (w0, w1)| {
                let base = (tile_start + k) * words_per_row + w;
                dense[base] = w0;
                if words == 2 {
                    dense[base + 1] = w1;
                }
            });
            w += words;
        }
    }

    // 3. Verify, column-major.
    let mut ok = vec![true; n];
    {
        let mut w = 0;
        for column in key {
            let words = column.words();
            column.visit_all_words(|i, (w0, w1)| {
                if !mask[i] {
                    let base = dense_of_row[i] as usize * words_per_row + w;
                    let mut equal = dense[base] == w0;
                    if words == 2 {
                        equal &= dense[base + 1] == w1;
                    }
                    ok[i] &= equal;
                }
            });
            w += words;
        }
    }
    let contaminated: Vec<usize> = (0..n).filter(|&i| !mask[i] && !ok[i]).collect();

    // 4. Hash collision between distinct rows: redo those classes exactly.
    if !contaminated.is_empty() {
        let offsets: Vec<Vec<usize>> = key.iter().map(|c| c.offsets()).collect();
        let rows_equal = |a: usize, b: usize| -> bool {
            key.iter()
                .zip(&offsets)
                .all(|(c, o)| c.words_at(o, a) == c.words_at(o, b))
        };
        let bad: FxHashSet<u64> = contaminated.iter().map(|&i| row_hashes[i]).collect();
        let mut per_hash: FxHashMap<u64, Vec<usize>> = FxHashMap::default();
        for i in 0..n {
            let row_hash = row_hashes[i];
            if !bad.contains(&row_hash) {
                continue;
            }
            let indices = per_hash.entry(row_hash).or_default();
            let is_duplicate = indices.iter().any(|&prev| rows_equal(prev, i));
            if is_duplicate {
                if mask[i] {
                    mask[i] = false;
                    unique_count -= 1;
                }
            } else {
                indices.push(i);
                if !mask[i] {
                    mask[i] = true;
                    unique_count += 1;
                }
            }
        }
    }

    (mask, unique_count)
}

fn vectorizable(v: &Value) -> bool {
    matches!(
        v,
        Value::Number(_) | Value::NumberInterval(_) | Value::Bool(_)
    )
}

/// Merge a same-shape group and deduplicate its rows, without ever
/// materialising the concatenated table. Returns `None` when the group has
/// a leaf the column collection cannot represent - the caller falls back
/// to the materialised pipeline, which handles (or loudly rejects)
/// everything.
pub fn merge_dedup_group(states: &[State]) -> Option<State> {
    debug_assert!(states.len() > 1);
    let _trace = TraceSpan::new("virtual_merge", "vectorize");

    let (columns, origins) = collect_columns_labeled(states)?;
    let total_rows: usize = states.iter().map(|s| s.vector_size).sum();
    let first = &states[0];
    crate::merge_stats::record_concat(states.len(), first.heap.len());

    // Uniform columns leave the key and become scalars in the output.
    let uniform: Vec<Option<Value>> = columns.iter().map(|c| c.uniform_scalar()).collect();
    let key: Vec<&Column> = columns
        .iter()
        .zip(&uniform)
        .filter_map(|(c, u)| u.is_none().then_some(c))
        .collect();

    let (kept, removed) = if key.is_empty() {
        // No column tells any two rows apart: every row is the same row.
        (vec![0u32], total_rows - 1)
    } else {
        let hashes = hash_rows(&key, total_rows);
        let t_bucket = crate::op_census::start();
        let (mask, unique_count) = virtual_unique_mask(&key, &hashes);
        crate::op_census::record(crate::op_census::Cat::DedupBucket, total_rows, 0, t_bucket);
        let kept: Vec<u32> = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &keep)| keep.then_some(i as u32))
            .collect();
        debug_assert_eq!(kept.len(), unique_count);
        (kept, total_rows - unique_count)
    };
    crate::merge_stats::record_dedup(total_rows, key.len(), first.heap.len(), removed);

    // Build the merged state, walking the same structure the collection
    // walked and consuming its columns in order. The `Origin` check makes a
    // traversal mismatch a loud panic instead of a silently misplaced
    // column.
    let mut next_col = 0usize;
    let mut take = |expected: Origin| -> Value {
        assert_eq!(
            origins[next_col], expected,
            "virtual merge column traversal diverged from collection"
        );
        let value = match &uniform[next_col] {
            Some(scalar) => scalar.clone(),
            None => columns[next_col].gather(&kept),
        };
        next_col += 1;
        value
    };

    // In a debug build, check the claim shape equality makes for the leaves
    // that are cloned from the first state: they are identical in every
    // state of the group. (`merge_values` does the same check.)
    #[cfg(debug_assertions)]
    let assert_all_equal = |get: &dyn Fn(&State) -> Option<&Value>| {
        let first_v = get(first);
        for s in states {
            assert_eq!(get(s), first_v, "non-vectorizable leaf differs in group");
        }
    };

    let mut new_heap = Heap::new();
    for _ in 0..first.heap.len() {
        new_heap.alloc();
    }
    for i in 0..first.heap.len() {
        let id = HeapId::from_raw(i);
        let Some(hv) = first.heap.get_opt(id) else {
            continue;
        };
        let merged = match hv {
            HeapValue::Value(v) if vectorizable(v) => HeapValue::Value(take(Origin::Heap(i))),
            HeapValue::Value(v) => {
                #[cfg(debug_assertions)]
                assert_all_equal(&|s: &State| match s.heap.get_opt(id) {
                    Some(HeapValue::Value(v)) => Some(v),
                    _ => None,
                });
                HeapValue::Value(v.clone())
            }
            HeapValue::Closure(gid, caps) => {
                let merged_caps: Vec<Value> = caps
                    .iter()
                    .enumerate()
                    .map(|(c, cap)| {
                        if vectorizable(cap) {
                            take(Origin::HeapCapture(i, c))
                        } else {
                            #[cfg(debug_assertions)]
                            assert_all_equal(&|s: &State| match s.heap.get_opt(id) {
                                Some(HeapValue::Closure(_, caps)) => caps.get(c),
                                _ => None,
                            });
                            cap.clone()
                        }
                    })
                    .collect();
                HeapValue::Closure(gid.clone(), merged_caps)
            }
            other => other.clone(),
        };
        new_heap.set(id, merged);
    }

    let mut merged_env = first.local_env.empty_like();
    for (slot, v) in first.local_env.iter() {
        let value = if vectorizable(v) {
            take(Origin::Local(slot))
        } else {
            #[cfg(debug_assertions)]
            assert_all_equal(&|s: &State| Some(s.local_env.get_by_raw_id(slot)));
            v.clone()
        };
        merged_env.set_slot(slot, first.local_env.occupant_of_slot(slot), value);
    }

    let merged_outer: Vec<super::local_env::LocalEnv> = (0..first.outer_local_envs.len())
        .map(|depth| {
            let env = &first.outer_local_envs[depth];
            let mut merged = env.empty_like();
            for (slot, v) in env.iter() {
                let value = if vectorizable(v) {
                    take(Origin::Outer(depth, slot))
                } else {
                    #[cfg(debug_assertions)]
                    assert_all_equal(&|s: &State| {
                        Some(s.outer_local_envs[depth].get_by_raw_id(slot))
                    });
                    v.clone()
                };
                merged.set_slot(slot, env.occupant_of_slot(slot), value);
            }
            merged
        })
        .collect();

    assert_eq!(
        next_col,
        columns.len(),
        "virtual merge left columns unconsumed"
    );

    Some(State {
        heap: new_heap,
        local_env: merged_env,
        outer_local_envs: merged_outer,
        global_env: first.global_env.clone(),
        prints: first.prints.clone(),
        vector_size: kept.len(),
    })
}
