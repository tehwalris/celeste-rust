//! Failure collection for lane-granular deopt.
//!
//! When a specialized program's premise (`assert_true`, see `collapse_loop`
//! etc.) fails for a boundary state, re-running the *whole* state under the
//! plain program wastes the specialized work of every lane that did not
//! violate the premise - and at kill frames that is millions of alive lanes
//! paying for a few dying ones. The granular path instead retries the frame
//! with a synthetic per-lane *origin* column (a global the program never
//! reads, carried automatically by every filter/expand/merge), and runs the
//! interpreter in *collect mode*: an `assert_true` whose value is false in
//! some lanes captures those lanes' origins here and continues the surviving
//! lanes, instead of aborting the frame. The deopt driver then re-runs only
//! the captured origins under the plain program.
//!
//! The sink is process-global because `flow` may fan states out across
//! threads; captures are rare (only failing premises), so a mutex is fine.
//! Exactly one `AbstractRun` steps at a time, matching the other process-
//! global interpreter state (e.g. merge partition patterns).

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};

/// Name of the synthetic per-lane origin global. The game program never
/// references it; it exists only during a granular-deopt retry and is
/// stripped from every output before the frame boundary.
pub const ORIGIN_GLOBAL: &str = "__lane_origin";

static COLLECTING: AtomicBool = AtomicBool::new(false);
static SINK: Mutex<Vec<u32>> = Mutex::new(Vec::new());

/// Enter collect mode. The sink starts empty.
pub fn begin() {
    let mut sink = SINK.lock().unwrap();
    sink.clear();
    COLLECTING.store(true, Ordering::SeqCst);
}

/// Leave collect mode and return every captured origin.
pub fn take() -> Vec<u32> {
    COLLECTING.store(false, Ordering::SeqCst);
    std::mem::take(&mut *SINK.lock().unwrap())
}

#[inline]
pub fn is_collecting() -> bool {
    COLLECTING.load(Ordering::Relaxed)
}

/// The per-lane origin indices of `state`, decoded from the origin global.
///
/// Panics if the column is missing or malformed: collect mode without an
/// origin column is a driver bug, and continuing would silently lose lanes.
pub fn read_origins(state: &State) -> Vec<u32> {
    let cell = state
        .global_env
        .get(ORIGIN_GLOBAL)
        .copied()
        .unwrap_or_else(|| panic!("deopt_collect: no {} global in collect mode", ORIGIN_GLOBAL));
    let Some(HeapValue::Value(Value::Number(n))) = state.heap.get_opt(cell) else {
        panic!("deopt_collect: {} is not a number column", ORIGIN_GLOBAL);
    };
    match n {
        // A filtered single-lane state's column may have collapsed to a
        // scalar; every lane of the state shares that origin.
        MaybeVector::Scalar(v) => vec![v.as_raw_u32(); state.vector_size.max(1)],
        MaybeVector::Vector(vs) => {
            assert_eq!(
                vs.len(),
                state.vector_size,
                "deopt_collect: origin column length mismatch"
            );
            vs.iter().map(|v| v.as_raw_u32()).collect()
        }
    }
}

/// Capture the origins of every lane of `state`.
pub fn capture_all(state: &State) {
    SINK.lock().unwrap().extend(read_origins(state));
}

/// Capture the origins of the lanes where `keep` is false (the lanes that
/// violated the premise; the `keep` lanes continue executing).
pub fn capture_dropped(state: &State, keep: &[bool]) {
    let origins = read_origins(state);
    assert_eq!(origins.len(), keep.len(), "deopt_collect: mask length mismatch");
    let mut sink = SINK.lock().unwrap();
    sink.extend(
        origins
            .iter()
            .zip(keep)
            .filter(|(_, keep)| !**keep)
            .map(|(o, _)| *o),
    );
}
