//! Per-execution branch DIRECTION trace, for deriving guard_branch pin
//! sweeps from a witness run (plans/shape-tag-plan.md, the dying overlays).
//!
//! `branch_sites` answers "which sites ever split"; this answers "which edge
//! did each conditional take, in execution order" for one bracketed run -
//! exactly what a `guard_branch` sweep needs: a site that appears only as
//! `true` pins taken, only as `false` pins not-taken, and a site with both
//! outcomes (a real loop or divergence) cannot be pinned and is reported as
//! such by the consumer.
//!
//! Off, the cost is one relaxed atomic load per conditional. The buffer is
//! global, so bracket exactly one single-threaded run (membercheck's member
//! attempt); results are meaningless across concurrent frames.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

static ACTIVE: AtomicBool = AtomicBool::new(false);

lazy_static::lazy_static! {
    static ref TRACE: Mutex<Vec<(String, String, bool, bool)>> = Mutex::new(Vec::new());
}

pub fn active() -> bool {
    ACTIVE.load(Ordering::Relaxed)
}

/// Start collecting. Clears any previous trace.
pub fn start() {
    TRACE.lock().unwrap().clear();
    ACTIVE.store(true, Ordering::Relaxed);
}

/// Stop collecting and return the trace in execution order:
/// `(function, block, took_true, took_false)` per conditional execution.
pub fn stop_take() -> Vec<(String, String, bool, bool)> {
    ACTIVE.store(false, Ordering::Relaxed);
    std::mem::take(&mut *TRACE.lock().unwrap())
}

pub fn record(function: &str, block: &str, took_true: bool, took_false: bool) {
    TRACE
        .lock()
        .unwrap()
        .push((function.to_string(), block.to_string(), took_true, took_false));
}
