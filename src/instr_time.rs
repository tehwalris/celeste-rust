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

use rustc_hash::FxHashMap;

static ENABLED: AtomicBool = AtomicBool::new(false);

thread_local! {
    /// Stack of function names, innermost last. A stack because calls nest
    /// cfg executions.
    static CURRENT_FN: RefCell<Vec<Arc<str>>> = const { RefCell::new(Vec::new()) };
}

lazy_static::lazy_static! {
    static ref TOTALS: Mutex<FxHashMap<(Arc<str>, usize), (Duration, u64)>> =
        Mutex::new(FxHashMap::default());
}

pub fn enable() {
    ENABLED.store(true, Ordering::Relaxed);
}

pub fn reset() {
    TOTALS.lock().unwrap().clear();
}

#[inline]
pub fn enabled() -> bool {
    ENABLED.load(Ordering::Relaxed)
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
