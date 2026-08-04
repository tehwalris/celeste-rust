//! Deduplicating a merge group *before* it is concatenated.
//!
//! A merge takes ~700 same-shape fragments, concatenates every column
//! across them into one ~19.8 M-row table, hashes those rows, and then
//! filters the ~613 k survivors out again. 97% of the rows are duplicates,
//! so the concatenation is mostly building data that is immediately thrown
//! away - 0.47 s of frame 39, plus the peak memory to hold it.
//!
//! This module computes the survivor set over a *virtual* concatenation:
//! the columns stay as per-fragment pieces and are never joined. It has
//! been tried before (three variants, all parked as regressions), and the
//! overnight log names exactly why each lost. Both causes are addressed
//! here:
//!
//!   * **Uniform columns.** Materialising collapses a column whose value is
//!     identical in every fragment and lane back to a `Scalar`, and
//!     collapsed columns then drop out of the dedup key entirely (measured:
//!     17.1 key columns materialised versus 19.0 virtual). A virtual layout
//!     loses that for free, so it is done explicitly here - `uniform_value`
//!     scans each column with an early exit, which is cheap precisely
//!     because non-uniform columns bail after a few elements.
//!
//!   * **Segmented random access.** The earlier attempt paid 4.2 vs 3.0
//!     ns/element because verifying a duplicate meant locating which piece
//!     a row index fell in. Every access this module needs - hashing,
//!     packing representatives, verifying, and the final gather - visits
//!     rows in ascending order, so each is a sorted walk over the pieces
//!     with no search and no per-element dispatch.

use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::state::State;
use super::value::{HeapValue, MaybeVector, Value};

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

/// A column of the virtual concatenation, one piece per fragment.
pub enum Column<'a> {
    Numbers(Vec<Piece<'a, Pico8Num>>),
    Bools(Vec<Piece<'a, bool>>),
    Intervals(Vec<Piece<'a, Pico8NumInterval>>),
}

impl<'a> Column<'a> {
    /// True when no two rows can be told apart by this column, so it can be
    /// left out of the dedup key. This is the collapse that materialising
    /// would have done for free.
    pub fn is_uniform(&self) -> bool {
        fn check<T: Copy + PartialEq>(pieces: &[Piece<T>]) -> bool {
            let Some(first) = pieces.iter().find_map(|p| p.first()) else {
                return true;
            };
            pieces.iter().all(|p| p.all_equal_to(&first))
        }
        match self {
            Column::Numbers(p) => check(p),
            Column::Bools(p) => check(p),
            Column::Intervals(p) => check(p),
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

/// Where a column came from, so a diagnostic can name it.
#[derive(Clone, Copy, Debug)]
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
        let id = super::heap::HeapId::from_raw(i);
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
/// Identical arithmetic to `vectorize::hash_rows`, and identical access
/// pattern: each piece is contiguous and visited once in order, so the
/// per-piece dispatch amortises over ~28 k lanes rather than costing
/// anything per element.
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

    let mut hashes = vec![0x51_7c_c1_b7_27_22_0a_95u64; total_rows];
    for column in columns {
        match column {
            Column::Numbers(p) => run(p, &mut hashes),
            Column::Bools(p) => run(p, &mut hashes),
            Column::Intervals(p) => run(p, &mut hashes),
        }
    }
    hashes
}
