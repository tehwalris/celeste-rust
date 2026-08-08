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

/// Walk the rows in `[start, start + out_len)` of `pieces`, calling
/// `f(global_row, value)`. Sorted-walk over the pieces, entered at `start`
/// by skipping whole pieces - this is what lets a *range* of the virtual
/// concatenation be processed independently of the rest, which is the unit
/// of parallelism here.
fn visit_range_impl<T: Copy + PartialEq, O: Copy>(
    pieces: &[Piece<T>],
    start: usize,
    end: usize,
    to_out: impl Fn(T) -> O,
    mut f: impl FnMut(usize, O),
) {
    let mut at = 0usize;
    for piece in pieces {
        let n = piece.len();
        let (p0, p1) = (at, at + n);
        at = p1;
        if p1 <= start {
            continue;
        }
        if p0 >= end {
            break;
        }
        let lo = start.max(p0);
        let hi = end.min(p1);
        match piece {
            Piece::Slice(s) => {
                for (i, v) in (lo..hi).zip(&s[lo - p0..hi - p0]) {
                    f(i, to_out(*v));
                }
            }
            Piece::Scalar(v, _) => {
                let out = to_out(*v);
                for i in lo..hi {
                    f(i, out);
                }
            }
        }
    }
}

/// Rows below this count are processed sequentially - thread setup costs
/// more than it saves on small merges. Same figure the materialised
/// parallel experiments settled on.
const PARALLEL_ROW_THRESHOLD: usize = 1 << 14;

/// How many threads the interpreter's parallel pieces use. The work is
/// memory-bound, so this saturates well below the core count.
///
/// Cached: `available_parallelism` is NOT a getter - on Linux it opens and
/// reads half a dozen procfs/cgroup files per call. Calling it per flow
/// step put 7.7M syscalls into a 30-frame run and turned minutes of wall
/// clock into kernel time.
pub fn worker_threads() -> usize {
    static THREADS: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *THREADS.get_or_init(|| {
        std::thread::available_parallelism()
            .map(|p| p.get())
            .unwrap_or(1)
            .min(16)
    })
}

thread_local! {
    /// Set on a thread that is ITSELF one of many running a frame chunk
    /// (`CELESTE_FRAME_THREADS`). The inner row-hashing then stays
    /// sequential.
    static NESTED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Mark this thread as already-parallel for the duration of the caller.
///
/// Without it the two levels multiply: a frame worker calls `hash_rows`
/// once per boundary state, and each call would `thread::scope` another 16
/// OS threads - thousands of spawns per frame, oversubscribing 16 cores
/// many times over. The outer parallelism is the coarser and better
/// balanced of the two, so the inner one yields.
pub fn set_nested_parallel(nested: bool) {
    NESTED.with(|n| n.set(nested));
}

fn merge_threads() -> usize {
    if NESTED.with(|n| n.get()) {
        1
    } else {
        worker_threads()
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

/// A column's single value, when it has one. The untyped `uniform_scalar`
/// wraps it in a `Value` (an allocation-bearing enum); the row-key hash
/// wants the raw payload to hash, once, per column.
pub enum UniformValue {
    Number(Pico8Num),
    Bool(bool),
    Interval(Pico8NumInterval),
}

impl<'a> Column<'a> {
    /// The single value every row of this column holds, if there is one.
    /// `None` also covers the empty column, which has nothing to fold.
    pub fn uniform_value(&self) -> Option<UniformValue> {
        fn check<T: Copy + PartialEq>(pieces: &[Piece<T>]) -> Option<T> {
            let first = pieces.iter().find_map(|p| p.first())?;
            pieces
                .iter()
                .all(|p| p.all_equal_to(&first))
                .then_some(first)
        }
        match self {
            Column::Numbers(p) => check(p).map(UniformValue::Number),
            Column::Bools(p) => check(p).map(UniformValue::Bool),
            Column::Intervals(p) => check(p).map(UniformValue::Interval),
        }
    }

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

    /// Visit rows `[start, end)` in order, yielding (row, packed words).
    fn visit_range_words(&self, start: usize, end: usize, f: impl FnMut(usize, Words)) {
        match self {
            Column::Numbers(p) => visit_range_impl(p, start, end, |v| (v.to_bits(), 0), f),
            Column::Bools(p) => visit_range_impl(p, start, end, |v| (v as u32, 0), f),
            Column::Intervals(p) => {
                visit_range_impl(p, start, end, |v| (v.low.to_bits(), v.high.to_bits()), f)
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
    hash_rows_seeded(columns, total_rows, 0)
}

/// `hash_rows` with the seed mixed into every element hash, giving an
/// independent hash function per seed. The frontier visited set keys rows by
/// two independently-seeded 64-bit hashes (128 bits total), which takes the
/// birthday collision risk at 10^8 rows from ~10^-4 to negligible - the
/// difference between "modulo hashing" and a claim one can argue about.
pub fn hash_rows_seeded(columns: &[&Column], total_rows: usize, seed: u64) -> Vec<u64> {
    use std::hash::{Hash, Hasher};
    #[inline(always)]
    fn elem_hash_seeded<T: Hash>(value: &T, seed: u64) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        seed.hash(&mut hasher);
        value.hash(&mut hasher);
        hasher.finish()
    }
    #[inline(always)]
    fn fold(row: &mut u64, value: u64) {
        *row = (row.rotate_left(26) ^ value).wrapping_mul(0x9e37_79b9_7f4a_7c15);
    }
    fn run<T: Hash + Copy + PartialEq>(pieces: &[Piece<T>], hashes: &mut [u64], seed: u64) {
        let mut at = 0;
        for piece in pieces {
            let n = piece.len();
            match piece {
                Piece::Slice(s) => {
                    for (row, value) in hashes[at..at + n].iter_mut().zip(s.iter()) {
                        fold(row, elem_hash_seeded(value, seed));
                    }
                }
                Piece::Scalar(v, _) => {
                    // One hash for the whole run - the value is the same in
                    // every lane of this fragment.
                    let h = elem_hash_seeded(v, seed);
                    for row in hashes[at..at + n].iter_mut() {
                        fold(row, h);
                    }
                }
            }
            at += n;
        }
    }

    fn hash_range(columns: &[&Column], hashes: &mut [u64], start: usize, seed: u64) {
        let end = start + hashes.len();
        for column in columns {
            match column {
                Column::Numbers(p) => visit_range_impl(p, start, end, |v| elem_hash_seeded(&v, seed), |i, h| {
                    fold(&mut hashes[i - start], h)
                }),
                Column::Bools(p) => visit_range_impl(p, start, end, |v| elem_hash_seeded(&v, seed), |i, h| {
                    fold(&mut hashes[i - start], h)
                }),
                Column::Intervals(p) => visit_range_impl(p, start, end, |v| elem_hash_seeded(&v, seed), |i, h| {
                    fold(&mut hashes[i - start], h)
                }),
            }
        }
    }

    let t = crate::op_census::start();
    let mut hashes = vec![0x51_7c_c1_b7_27_22_0a_95u64; total_rows];
    let threads = merge_threads();
    if total_rows < PARALLEL_ROW_THRESHOLD || threads == 1 {
        for column in columns {
            match column {
                Column::Numbers(p) => run(p, &mut hashes, seed),
                Column::Bools(p) => run(p, &mut hashes, seed),
                Column::Intervals(p) => run(p, &mut hashes, seed),
            }
        }
    } else {
        // Row-range chunks: each thread folds every column over its own
        // contiguous stretch, entering the piece walk by skipping whole
        // pieces. Identical arithmetic per row, so the result matches the
        // sequential pass exactly.
        let chunk = total_rows.div_ceil(threads);
        std::thread::scope(|scope| {
            for (i, hash_chunk) in hashes.chunks_mut(chunk).enumerate() {
                scope.spawn(move || hash_range(columns, hash_chunk, i * chunk, seed));
            }
        });
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

/// The frontier's 128-bit row key: both halves, in one pass, with the
/// state's UNIFORM columns folded once instead of once per row.
///
/// Measured on room (1,0) at depth: a boundary state has ~50.8 columns that
/// hold the same value in every lane and ~7.2 that vary. `hash_rows` folds
/// all 58 into every row, so ~87% of the arithmetic re-derives a per-state
/// constant per lane - and 64-bit multiply is one port, so those folds were
/// 23% of all cycles in the profile.
///
/// The combine is therefore ORDER-INDEPENDENT (wrapping addition of
/// well-diffused per-column contributions) rather than a sequential fold.
/// That is not only an optimisation - it is required for the hoist to be
/// CORRECT. Uniformity is a property of a state, not of a column: the same
/// logical row can arrive in a state where column 5 is uniform and column 9
/// varies, and in another where it is the other way round. A sequential
/// fold would have to visit both in canonical position to agree; an
/// order-independent one does not care which side of the hoist each column
/// fell on.
///
/// What order-independence costs is that the column's IDENTITY has to enter
/// its contribution explicitly, or two columns swapping values would
/// produce the same sum. The ordinal is hashed in with the value, and each
/// contribution goes through a splitmix finalizer before being added, so
/// low-entropy values (booleans, small integers - most of this state) still
/// spread across all 64 bits.
///
/// Two independently-seeded halves give the 128-bit key. Comparability
/// across states rests on the same premise the sequential version rested
/// on: equal-shape states produce columns in the same canonical order, and
/// the shape hash keys the table.
pub fn row_key_hashes(
    shape_hash: u64,
    columns: &[&Column],
    total_rows: usize,
    seed2: u64,
) -> Vec<(u64, u64)> {
    use std::hash::{Hash, Hasher};

    /// Per-(column, seed) salt: the column's identity, hashed once. It is
    /// what stops two columns swapping values from producing the same sum.
    #[inline(always)]
    fn salt(ord: usize, seed: u64) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        seed.hash(&mut hasher);
        (ord as u64).hash(&mut hasher);
        hasher.finish()
    }
    /// splitmix64's finalizer. Load-bearing, not decoration: contributions
    /// are ADDED, so each one has to be spread over all 64 bits first or
    /// the low bits of a column of small integers would carry the sum.
    #[inline(always)]
    fn finalize(h: u64) -> u64 {
        let h = (h ^ (h >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        let h = (h ^ (h >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        h ^ (h >> 31)
    }
    #[inline(always)]
    fn contrib<T: Hash>(value: &T, salt: u64) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        value.hash(&mut hasher);
        finalize(hasher.finish() ^ salt)
    }

    /// Add one column's contributions into both halves.
    fn add_column<T: Hash + Copy + PartialEq>(
        pieces: &[Piece<T>],
        ord: usize,
        seed2: u64,
        keys: &mut [(u64, u64)],
    ) {
        let (pa, pb) = (salt(ord, 0), salt(ord, seed2));
        let mut at = 0;
        for piece in pieces {
            let n = piece.len();
            match piece {
                Piece::Slice(s) => {
                    for (key, v) in keys[at..at + n].iter_mut().zip(s.iter()) {
                        key.0 = key.0.wrapping_add(contrib(v, pa));
                        key.1 = key.1.wrapping_add(contrib(v, pb));
                    }
                }
                Piece::Scalar(v, _) => {
                    let (ca, cb) = (contrib(v, pa), contrib(v, pb));
                    for key in keys[at..at + n].iter_mut() {
                        key.0 = key.0.wrapping_add(ca);
                        key.1 = key.1.wrapping_add(cb);
                    }
                }
            }
            at += n;
        }
    }

    let t = crate::op_census::start();
    const INIT: u64 = 0x51_7c_c1_b7_27_22_0a_95;
    // Pass 1: the shape and every column that holds one value across the
    // whole virtual concatenation contribute constants, computed once. The
    // shape used to be mixed in per lane by `RowTable::key`, which is four
    // multiplies per lane for something that cannot vary within a state.
    let (mut base_a, mut base_b) = (
        finalize(INIT ^ shape_hash),
        finalize(INIT ^ shape_hash.wrapping_add(0x9e37_79b9_7f4a_7c15)),
    );
    let mut varying: Vec<(usize, &Column)> = Vec::new();
    for (ord, column) in columns.iter().enumerate() {
        macro_rules! uniform_add {
            ($v:expr) => {{
                base_a = base_a.wrapping_add(contrib(&$v, salt(ord, 0)));
                base_b = base_b.wrapping_add(contrib(&$v, salt(ord, seed2)));
            }};
        }
        match column.uniform_value() {
            Some(UniformValue::Number(v)) => uniform_add!(v),
            Some(UniformValue::Bool(v)) => uniform_add!(v),
            Some(UniformValue::Interval(v)) => uniform_add!(v),
            // An empty column has no value to fold and no rows to
            // distinguish; it contributes nothing either way.
            None => varying.push((ord, column)),
        }
    }
    let mut keys = vec![(base_a, base_b); total_rows];
    // Pass 2: only the columns that actually tell rows apart.
    for (ord, column) in &varying {
        match column {
            Column::Numbers(p) => add_column(p, *ord, seed2, &mut keys),
            Column::Bools(p) => add_column(p, *ord, seed2, &mut keys),
            Column::Intervals(p) => add_column(p, *ord, seed2, &mut keys),
        }
    }
    let col_bytes: usize = varying.iter().map(|(_, c)| c.total_bytes()).sum();
    crate::op_census::record(
        crate::op_census::Cat::HashRows,
        2 * total_rows * varying.len().max(1),
        col_bytes + 32 * total_rows * varying.len(),
        t,
    );
    keys
}

/// The probe phase's outputs: which rows are representatives (first row
/// seen with their hash), each row's representative's slot in the dense
/// table, and the representatives themselves in ascending row order (slot
/// d belongs to row `uniq[d]`).
struct Probe {
    mask: Vec<bool>,
    dense_of_row: Vec<u32>,
    uniq: Vec<u32>,
}

fn sequential_probe(row_hashes: &[u64]) -> Probe {
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
    Probe {
        mask,
        dense_of_row,
        uniq,
    }
}

/// The probe, partitioned by hash high bits and bit-identical to
/// `sequential_probe`.
///
/// Equal rows share a hash, so a hash class lands intact in one partition;
/// partition row lists are built from contiguous per-thread ranges
/// concatenated in thread order, so they are ascending, and the first
/// occurrence within a partition is the global first occurrence. Dense
/// slots are assigned per partition and then renumbered by representative
/// row order - which is exactly the discovery order the sequential probe
/// assigns. Two wins: the partitions probe in parallel, and each
/// partition's hash map is ~PARTS times smaller, small enough to stay in
/// cache where the single big map thrashed.
///
/// Every write is either to a thread-owned contiguous chunk or single-
/// threaded; no atomics, no unsafe.
fn partitioned_probe(row_hashes: &[u64], threads: usize) -> Probe {
    const PARTS: usize = 64;
    let shift = 64 - PARTS.trailing_zeros();
    let part_of = |hash: u64| (hash >> shift) as usize;
    let n = row_hashes.len();
    let chunk = n.div_ceil(threads);

    // Phase A: scatter rows into partition lists, per-thread; record each
    // row's position within its thread-local partition list.
    let mut local_pos: Vec<u32> = vec![0; n];
    let per_thread_parts: Vec<Vec<Vec<u32>>> = std::thread::scope(|scope| {
        let handles: Vec<_> = row_hashes
            .chunks(chunk)
            .zip(local_pos.chunks_mut(chunk))
            .enumerate()
            .map(|(t, (hash_chunk, pos_chunk))| {
                scope.spawn(move || {
                    let start = t * chunk;
                    let mut parts: Vec<Vec<u32>> = vec![Vec::new(); PARTS];
                    for (k, (&h, pos)) in hash_chunk.iter().zip(pos_chunk).enumerate() {
                        let p = part_of(h);
                        *pos = parts[p].len() as u32;
                        parts[p].push((start + k) as u32);
                    }
                    parts
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    });

    // Offsets of each thread's contribution within the concatenated
    // partition lists, and the concatenation itself.
    let thread_count = per_thread_parts.len();
    let mut offset_of_thread: Vec<[u32; PARTS]> = vec![[0; PARTS]; thread_count];
    let mut part_lens = [0u32; PARTS];
    for (t, parts) in per_thread_parts.iter().enumerate() {
        for p in 0..PARTS {
            offset_of_thread[t][p] = part_lens[p];
            part_lens[p] += parts[p].len() as u32;
        }
    }
    let parts: Vec<Vec<u32>> = (0..PARTS)
        .map(|p| {
            let mut list = Vec::with_capacity(part_lens[p] as usize);
            for thread_parts in &per_thread_parts {
                list.extend_from_slice(&thread_parts[p]);
            }
            list
        })
        .collect();

    // Phase B: probe each partition independently, partitions striped
    // across threads. Outputs are per-partition: the representative rows
    // (ascending) and, for each list position, its representative's
    // partition-local slot.
    struct PartProbe {
        firsts: Vec<u32>,
        rep_of_pos: Vec<u32>,
    }
    let probes: Vec<PartProbe> = std::thread::scope(|scope| {
        let parts = &parts;
        let handles: Vec<_> = (0..threads.min(PARTS))
            .map(|t| {
                scope.spawn(move || {
                    let mut out: Vec<(usize, PartProbe)> = Vec::new();
                    let mut p = t;
                    while p < PARTS {
                        let list = &parts[p];
                        let mut map: FxHashMap<u64, u32> = FxHashMap::with_capacity_and_hasher(
                            list.len() / 2,
                            Default::default(),
                        );
                        let mut firsts: Vec<u32> = Vec::new();
                        let mut rep_of_pos: Vec<u32> = Vec::with_capacity(list.len());
                        for &row in list {
                            match map.entry(row_hashes[row as usize]) {
                                std::collections::hash_map::Entry::Vacant(slot) => {
                                    let local = firsts.len() as u32;
                                    slot.insert(local);
                                    firsts.push(row);
                                    rep_of_pos.push(local);
                                }
                                std::collections::hash_map::Entry::Occupied(slot) => {
                                    rep_of_pos.push(*slot.get());
                                }
                            }
                        }
                        out.push((p, PartProbe { firsts, rep_of_pos }));
                        p += threads;
                    }
                    out
                })
            })
            .collect();
        let mut probes: Vec<Option<PartProbe>> = (0..PARTS).map(|_| None).collect();
        for handle in handles {
            for (p, probe) in handle.join().unwrap() {
                probes[p] = Some(probe);
            }
        }
        probes.into_iter().map(|p| p.unwrap()).collect()
    });

    // Phase C: provisional global slots (partition-major), then renumber so
    // slot order equals representative row order - the sequential probe's
    // discovery order.
    let mut base = [0u32; PARTS];
    let mut total_uniq = 0u32;
    for p in 0..PARTS {
        base[p] = total_uniq;
        total_uniq += probes[p].firsts.len() as u32;
    }
    let mut pairs: Vec<(u32, u32)> = Vec::with_capacity(total_uniq as usize);
    for p in 0..PARTS {
        for (j, &row) in probes[p].firsts.iter().enumerate() {
            pairs.push((row, base[p] + j as u32));
        }
    }
    pairs.sort_unstable_by_key(|&(row, _)| row);
    let uniq: Vec<u32> = pairs.iter().map(|&(row, _)| row).collect();
    let mut remap: Vec<u32> = vec![0; total_uniq as usize];
    for (sorted_idx, &(_, provisional)) in pairs.iter().enumerate() {
        remap[provisional as usize] = sorted_idx as u32;
    }

    // Phase D: fill dense_of_row and mask, threads on contiguous row
    // ranges again.
    let mut mask = vec![false; n];
    let mut dense_of_row: Vec<u32> = vec![0; n];
    std::thread::scope(|scope| {
        let probes = &probes;
        let offset_of_thread = &offset_of_thread;
        let remap = &remap;
        for ((t, hash_chunk), (pos_chunk, (mask_chunk, dense_chunk))) in row_hashes
            .chunks(chunk)
            .enumerate()
            .zip(local_pos.chunks(chunk).zip(
                mask.chunks_mut(chunk).zip(dense_of_row.chunks_mut(chunk)),
            ))
        {
            scope.spawn(move || {
                for k in 0..hash_chunk.len() {
                    let p = part_of(hash_chunk[k]);
                    let pos = (offset_of_thread[t][p] + pos_chunk[k]) as usize;
                    let local_rep = probes[p].rep_of_pos[pos];
                    let provisional = base[p] + local_rep;
                    dense_chunk[k] = remap[provisional as usize];
                    // A row is kept iff it is its own representative.
                    mask_chunk[k] =
                        probes[p].firsts[local_rep as usize] == (t * chunk + k) as u32;
                }
            });
        }
    });

    Probe {
        mask,
        dense_of_row,
        uniq,
    }
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

    let _t_probe = TraceSpan::new("vm_probe", "vectorize");
    // The probe is worth partitioning only once its hash map outgrows the
    // cache: below ~512k rows the single map is L3-resident and the
    // partitioned version's extra O(n) passes (scatter, fill, remap) cost
    // more than they save - measured +0.3% on the runner with the plain
    // 16k threshold, against a halved probe on the deep merges.
    const PARTITIONED_PROBE_THRESHOLD: usize = 1 << 19;
    let Probe {
        mask,
        dense_of_row,
        uniq,
    } = if n < PARTITIONED_PROBE_THRESHOLD || merge_threads() == 1 {
        sequential_probe(row_hashes)
    } else {
        partitioned_probe(row_hashes, merge_threads())
    };
    let mut mask = mask;
    drop(_t_probe);
    let mut unique_count = uniq.len();
    if unique_count == n {
        return (mask, unique_count);
    }

    // 2. Pack the representatives - tiles are disjoint dense ranges, so
    // they pack in parallel, each still an L2-sized destination filled by
    // sorted piece walks.
    let _t_pack = TraceSpan::new("vm_pack", "vectorize");
    let words_per_row: usize = key.iter().map(|c| c.words()).sum();
    let mut dense = vec![0u32; uniq.len() * words_per_row];
    const DENSE_TILE_BYTES: usize = 192 * 1024;
    let tile_rows = (DENSE_TILE_BYTES / (words_per_row.max(1) * 4)).max(1);
    {
        let pack_tile = |tile_index: usize, dense_tile: &mut [u32]| {
            let tile_start = tile_index * tile_rows;
            let tile_end = (tile_start + tile_rows).min(uniq.len());
            let tile = &uniq[tile_start..tile_end];
            let mut w = 0;
            for column in key.iter() {
                let words = column.words();
                column.visit_rows_words(tile, |k, (w0, w1)| {
                    let base = k * words_per_row + w;
                    dense_tile[base] = w0;
                    if words == 2 {
                        dense_tile[base + 1] = w1;
                    }
                });
                w += words;
            }
        };
        let threads = merge_threads();
        if uniq.len() < PARALLEL_ROW_THRESHOLD || threads == 1 {
            for (tile_index, dense_tile) in
                dense.chunks_mut(tile_rows * words_per_row).enumerate()
            {
                pack_tile(tile_index, dense_tile);
            }
        } else {
            let tiles: Vec<(usize, &mut [u32])> = dense
                .chunks_mut(tile_rows * words_per_row)
                .enumerate()
                .collect();
            let per_thread = tiles.len().div_ceil(threads);
            let mut groups: Vec<Vec<(usize, &mut [u32])>> = Vec::new();
            for (i, entry) in tiles.into_iter().enumerate() {
                if i % per_thread == 0 {
                    groups.push(Vec::with_capacity(per_thread));
                }
                groups.last_mut().unwrap().push(entry);
            }
            std::thread::scope(|scope| {
                for group in groups {
                    let pack_tile = &pack_tile;
                    scope.spawn(move || {
                        for (tile_index, dense_tile) in group {
                            pack_tile(tile_index, dense_tile);
                        }
                    });
                }
            });
        }
    }

    drop(_t_pack);
    // 3. Verify, column-major over contiguous row ranges - one range per
    // thread, each a sorted piece walk, all random access confined to the
    // cache-resident dense table. Within a range, work goes through
    // L1-sized chunks: each column decodes its chunk into a stack buffer
    // with a sequential store loop, then a tight zip loop compares the
    // buffer against the representatives - equal-length slices, so the
    // compiler drops the bounds checks the per-element closure paid
    // (measured: the closure was the profile's top symbol at ~18% CPU).
    let _t_verify = TraceSpan::new("vm_verify", "vectorize");
    let mut ok = vec![true; n];
    {
        const VERIFY_CHUNK: usize = 4096;
        let verify_range = |ok_chunk: &mut [bool], start: usize| {
            let mut buf = vec![(0u32, 0u32); VERIFY_CHUNK];
            let len = ok_chunk.len();
            let mut chunk_start = 0usize;
            while chunk_start < len {
                let chunk_len = VERIFY_CHUNK.min(len - chunk_start);
                let global = start + chunk_start;
                let mask_c = &mask[global..global + chunk_len];
                let dense_of_row_c = &dense_of_row[global..global + chunk_len];
                let ok_c = &mut ok_chunk[chunk_start..chunk_start + chunk_len];
                let buf_c = &mut buf[..chunk_len];
                let mut w = 0;
                for column in key.iter() {
                    let words = column.words();
                    column.visit_range_words(global, global + chunk_len, |i, wv| {
                        buf_c[i - global] = wv;
                    });
                    for (((&(w0, w1), &masked), &dense_row), ok_slot) in buf_c
                        .iter()
                        .zip(mask_c)
                        .zip(dense_of_row_c)
                        .zip(ok_c.iter_mut())
                    {
                        if !masked {
                            let base = dense_row as usize * words_per_row + w;
                            let mut equal = dense[base] == w0;
                            if words == 2 {
                                equal &= dense[base + 1] == w1;
                            }
                            *ok_slot &= equal;
                        }
                    }
                    w += words;
                }
                chunk_start += chunk_len;
            }
        };
        let threads = merge_threads();
        if n < PARALLEL_ROW_THRESHOLD || threads == 1 {
            verify_range(&mut ok, 0);
        } else {
            let chunk = n.div_ceil(threads);
            std::thread::scope(|scope| {
                for (i, ok_chunk) in ok.chunks_mut(chunk).enumerate() {
                    let verify_range = &verify_range;
                    scope.spawn(move || verify_range(ok_chunk, i * chunk));
                }
            });
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

    let (columns, origins) = {
        let _t = TraceSpan::new("vm_collect", "vectorize");
        collect_columns_labeled(states)?
    };
    let total_rows: usize = states.iter().map(|s| s.vector_size).sum();
    let first = &states[0];
    crate::merge_stats::record_concat(states.len(), first.heap.len());

    // Uniform columns leave the key and become scalars in the output.
    let uniform: Vec<Option<Value>> = {
        let _t = TraceSpan::new("vm_uniform", "vectorize");
        columns.iter().map(|c| c.uniform_scalar()).collect()
    };
    let key: Vec<&Column> = columns
        .iter()
        .zip(&uniform)
        .filter_map(|(c, u)| u.is_none().then_some(c))
        .collect();

    let (kept, removed) = if key.is_empty() {
        // No column tells any two rows apart: every row is the same row.
        (vec![0u32], total_rows - 1)
    } else {
        let hashes = {
            let _t = TraceSpan::new("vm_hash", "vectorize");
            hash_rows(&key, total_rows)
        };
        let t_bucket = crate::op_census::start();
        let _t_mask = TraceSpan::new("vm_mask", "vectorize");
        let (mask, unique_count) = virtual_unique_mask(&key, &hashes);
        drop(_t_mask);
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

    // Gather every non-uniform column's survivors up front, columns in
    // parallel - they are independent, and each gather is a sorted piece
    // walk into a fresh allocation.
    let _t_gather = TraceSpan::new("vm_gather", "vectorize");
    let mut gathered: Vec<Option<Value>> = {
        let threads = merge_threads().min(columns.len().max(1));
        let mut gathered: Vec<Option<Value>> = (0..columns.len()).map(|_| None).collect();
        if kept.len() < PARALLEL_ROW_THRESHOLD || threads <= 1 {
            for (slot, (column, uniform)) in gathered.iter_mut().zip(columns.iter().zip(&uniform))
            {
                if uniform.is_none() {
                    *slot = Some(column.gather(&kept));
                }
            }
        } else {
            let chunk = columns.len().div_ceil(threads);
            let kept = &kept;
            let columns = &columns;
            let uniform = &uniform;
            std::thread::scope(|scope| {
                for (i, out_chunk) in gathered.chunks_mut(chunk).enumerate() {
                    scope.spawn(move || {
                        for (k, slot) in out_chunk.iter_mut().enumerate() {
                            let col_index = i * chunk + k;
                            if uniform[col_index].is_none() {
                                *slot = Some(columns[col_index].gather(kept));
                            }
                        }
                    });
                }
            });
        }
        gathered
    };

    drop(_t_gather);
    let _t_build = TraceSpan::new("vm_build", "vectorize");
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
            None => gathered[next_col]
                .take()
                .expect("non-uniform column must have been gathered"),
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The partitioned probe must be bit-identical to the sequential one -
    /// same mask, same dense slots, same uniq order - across sizes, thread
    /// counts and duplicate densities.
    #[test]
    fn partitioned_probe_matches_sequential() {
        // Deterministic pseudo-random hashes with plenty of duplicates.
        let mut seed: u64 = 0x1234_5678_9abc_def0;
        let mut next = move || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            seed
        };
        for &n in &[1usize, 5, 100, 1 << 14, (1 << 14) + 1, 100_000] {
            for &distinct in &[1usize, 2, 37, 5000] {
                let pool: Vec<u64> = (0..distinct).map(|_| next()).collect();
                let hashes: Vec<u64> =
                    (0..n).map(|_| pool[(next() as usize) % distinct]).collect();
                let sequential = sequential_probe(&hashes);
                for &threads in &[2usize, 3, 16] {
                    let partitioned = partitioned_probe(&hashes, threads);
                    assert_eq!(partitioned.mask, sequential.mask, "n={n} threads={threads}");
                    assert_eq!(
                        partitioned.dense_of_row, sequential.dense_of_row,
                        "n={n} threads={threads}"
                    );
                    assert_eq!(partitioned.uniq, sequential.uniq, "n={n} threads={threads}");
                }
            }
        }
    }
}
