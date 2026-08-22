//! Per-instruction wall-clock attribution for the program under test.
//!
//! `bench --profile` resolves time to interpreter *phases* (`cfg:...`,
//! `vectorize:...`), which says the frame body costs X but not which of its
//! instructions cost it. At current speeds one instruction execution over a
//! vectorized state is tens of nanoseconds to microseconds - well within
//! `Instant`'s resolution - so this module simply times every instruction
//! execution and accumulates per `(function, LocalId)`. The report joins the
//! ids back to blocks and text against the program.
//!
//! Scope and caveats:
//!
//!   * One sample per instruction per state per block execution, taken in the
//!     interpreter's instruction loop (`flow.rs`). Timer overhead inflates
//!     everything roughly uniformly (~2 `Instant::now` per sample); use the
//!     shares, not the absolute numbers.
//!   * A `Call` instruction's time includes its whole callee execution -
//!     attribution is by site, not flat.
//!   * Phi moves, terminators and the state filtering they trigger are *not*
//!     covered here; they stay under the `filter:*` spans. This module covers
//!     exactly the `BlockPostPhi` instruction loop.
//!
//! Off by default; one relaxed atomic load per instruction when disabled.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use rustc_hash::{FxHashMap, FxHashSet};

static ENABLED: AtomicBool = AtomicBool::new(false);
static CARD_ENABLED: AtomicBool = AtomicBool::new(false);

thread_local! {
    /// Stack of function names, innermost last. A stack because calls nest
    /// cfg executions.
    static CURRENT_FN: RefCell<Vec<Arc<str>>> = const { RefCell::new(Vec::new()) };
}

lazy_static::lazy_static! {
    static ref TOTALS: Mutex<FxHashMap<(Arc<str>, usize), (Duration, u64)>> =
        Mutex::new(FxHashMap::default());
    static ref CARD_TOTALS: Mutex<FxHashMap<(Arc<str>, usize), CardStat>> =
        Mutex::new(FxHashMap::default());
}

/// Output-cardinality tally for one instruction: across all its executions,
/// how many lanes it produced and how many of them were *distinct within
/// their vector*. `distinct/lanes` close to zero means the instruction spends
/// nearly all its time recomputing values it has already computed in other
/// lanes of the same vector - the direct measure of what dictionary-encoded
/// columns or per-distinct-value evaluation could save.
#[derive(Default, Clone, Copy)]
pub struct CardStat {
    /// Total output lanes across every (state, execution) pair.
    pub lanes: u64,
    /// Sum over executions of the number of distinct values in the output.
    /// A scalar output counts as 1 distinct over `vector_size` lanes - the
    /// interpreter already collapses that redundancy, which is why scalars
    /// score as fully non-redundant here only if `vector_size` is 1.
    pub distinct: u64,
    /// State-level executions observed.
    pub execs: u64,
    /// How many of those executions produced a vector (not scalar) output.
    pub vector_execs: u64,
    /// Largest distinct count seen in any single execution.
    pub max_distinct: u64,
}

pub fn enable() {
    ENABLED.store(true, Ordering::Relaxed);
}

pub fn enable_cardinality() {
    CARD_ENABLED.store(true, Ordering::Relaxed);
}

pub fn reset() {
    TOTALS.lock().unwrap().clear();
    CARD_TOTALS.lock().unwrap().clear();
}

#[inline]
pub fn enabled() -> bool {
    ENABLED.load(Ordering::Relaxed)
}

#[inline]
pub fn cardinality_enabled() -> bool {
    CARD_ENABLED.load(Ordering::Relaxed)
}

/// Marks the current thread as executing `name` until the guard drops.
pub struct FnGuard;

pub fn enter_function(name: &str) -> FnGuard {
    CURRENT_FN.with(|c| c.borrow_mut().push(Arc::from(name)));
    FnGuard
}

impl Drop for FnGuard {
    fn drop(&mut self) {
        CURRENT_FN.with(|c| {
            c.borrow_mut().pop();
        });
    }
}

pub fn record(local_id: usize, elapsed: Duration) {
    let Some(function) = CURRENT_FN.with(|c| c.borrow().last().cloned()) else {
        return;
    };
    let mut totals = TOTALS.lock().unwrap();
    let entry = totals.entry((function, local_id)).or_default();
    entry.0 += elapsed;
    entry.1 += 1;
}

/// Counts distinct values in one instruction output and accumulates.
///
/// Called outside the timed window, so the (substantial) counting cost
/// inflates wall-clock but not the per-instruction time attribution. Use a
/// separate run without `CELESTE_INSTR_CARD` for time-of-record numbers.
pub fn record_cardinality(local_id: usize, value: &crate::interpreter::value::Value, vector_size: usize) {
    use crate::interpreter::value::{MaybeVector, Value};

    let (lanes, distinct, is_vector) = match value {
        Value::Number(MaybeVector::Vector(v)) => {
            let mut seen = FxHashSet::default();
            for x in v.iter() {
                seen.insert(*x);
            }
            (v.len() as u64, seen.len() as u64, true)
        }
        Value::NumberInterval(MaybeVector::Vector(v)) => {
            let mut seen = FxHashSet::default();
            for x in v.iter() {
                seen.insert(*x);
            }
            (v.len() as u64, seen.len() as u64, true)
        }
        Value::Bool(MaybeVector::Vector(v)) => {
            // Early exit once both truth values have appeared.
            let mut seen_true = false;
            let mut seen_false = false;
            for &b in v.iter() {
                if b {
                    seen_true = true;
                } else {
                    seen_false = true;
                }
                if seen_true && seen_false {
                    break;
                }
            }
            (v.len() as u64, (seen_true as u64) + (seen_false as u64), true)
        }
        // Scalars (including strings, pointers, nils) are one value broadcast
        // over the state's lanes - redundancy the interpreter already elides.
        _ => (vector_size as u64, 1, false),
    };

    let Some(function) = CURRENT_FN.with(|c| c.borrow().last().cloned()) else {
        return;
    };
    let mut totals = CARD_TOTALS.lock().unwrap();
    let entry = totals.entry((function, local_id)).or_default();
    entry.lanes += lanes;
    entry.distinct += distinct;
    entry.execs += 1;
    entry.vector_execs += is_vector as u64;
    entry.max_distinct = entry.max_distinct.max(distinct);
}

/// Everything the cardinality census recorded: `(function, local id, stat)`.
pub fn cardinality_report() -> Vec<(String, usize, CardStat)> {
    let totals = CARD_TOTALS.lock().unwrap();
    let mut rows: Vec<(String, usize, CardStat)> = totals
        .iter()
        .map(|((f, id), stat)| (f.to_string(), *id, *stat))
        .collect();
    rows.sort_by_key(|(f, id, _)| (f.clone(), *id));
    rows
}

/// Everything recorded so far: `(function, local id, total time, samples)`,
/// most expensive first.
pub fn report() -> Vec<(String, usize, Duration, u64)> {
    let totals = TOTALS.lock().unwrap();
    let mut rows: Vec<(String, usize, Duration, u64)> = totals
        .iter()
        .map(|((f, id), (d, n))| (f.to_string(), *id, *d, *n))
        .collect();
    rows.sort_by_key(|(f, id, d, _)| (std::cmp::Reverse(*d), f.clone(), *id));
    rows
}
