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
//! The sink is THREAD-LOCAL. It used to be a process-global mutex, on the
//! reasoning that exactly one `AbstractRun` steps at a time; chunk-parallel
//! frame execution (`CELESTE_FRAME_THREADS`) breaks that premise - several
//! worker threads each run a whole frame in collect mode at once, and a
//! shared sink would attribute one chunk's failing origins to another
//! chunk's lane numbering. A thread-local sink is both correct under
//! parallelism and cheaper (no lock on the capture path).
//!
//! The contract this imposes on callers: `begin`, the interpretation it
//! guards, and `take` must all run on the SAME thread. Both call sites
//! (`run_deopt_frame_granular` and the sweep's replay) satisfy that - the
//! frame runs inline between them.

use std::cell::{Cell, RefCell};

use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};

/// Name of the synthetic per-lane origin global. The game program never
/// references it; it exists only during a granular-deopt retry and is
/// stripped from every output before the frame boundary.
pub const ORIGIN_GLOBAL: &str = "__lane_origin";

thread_local! {
    static COLLECTING: Cell<bool> = const { Cell::new(false) };
    static SINK: RefCell<Vec<u32>> = const { RefCell::new(Vec::new()) };
}

/// Enter collect mode on this thread. The sink starts empty.
pub fn begin() {
    SINK.with(|s| s.borrow_mut().clear());
    COLLECTING.with(|c| c.set(true));
}

/// Leave collect mode and return every origin captured on this thread.
pub fn take() -> Vec<u32> {
    COLLECTING.with(|c| c.set(false));
    SINK.with(|s| std::mem::take(&mut *s.borrow_mut()))
}

#[inline]
pub fn is_collecting() -> bool {
    COLLECTING.with(|c| c.get())
}

/// The per-lane origin indices of `state`, decoded from the origin global.
///
/// Panics if the column is missing or malformed: collect mode without an
/// origin column is a driver bug, and continuing would silently lose lanes.
pub fn read_origins(state: &State) -> Vec<u32> {
    read_origins_named(state, ORIGIN_GLOBAL)
}

/// `read_origins` for an arbitrarily-named origin column (the backward sweep
/// carries its own, so it cannot collide with the deopt machinery's).
pub fn read_origins_named(state: &State, name: &str) -> Vec<u32> {
    let cell = state
        .global_env
        .get(name)
        .copied()
        .unwrap_or_else(|| panic!("deopt_collect: no {} global in collect mode", name));
    let Some(HeapValue::Value(Value::Number(n))) = state.heap.get_opt(cell) else {
        panic!("deopt_collect: {} is not a number column", name);
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

/// Attach a per-lane origin column under `name`: a Number global the program
/// never reads, whose raw bits per lane are `values`. Every existing filter /
/// expand / merge carries it; strip it (remove the global) before any row
/// canonicalization.
pub fn inject_named(state: &mut State, name: &str, values: &[u32]) {
    use crate::pico8_num::Pico8Num;
    assert_eq!(values.len(), state.vector_size.max(1), "origin count mismatch");
    let lanes: Vec<Pico8Num> = values
        .iter()
        .map(|i| Pico8Num::from_parts((i >> 16) as i16, *i as u16))
        .collect();
    let value = if lanes.len() == 1 {
        MaybeVector::Scalar(lanes[0])
    } else {
        MaybeVector::Vector(std::sync::Arc::new(lanes))
    };
    let cell = state.heap.alloc();
    state.heap.set(cell, HeapValue::Value(Value::Number(value)));
    state.global_env.insert(name.to_string(), cell);
}

/// Capture the origins of every lane of `state`.
pub fn capture_all(state: &State) {
    let origins = read_origins(state);
    SINK.with(|s| s.borrow_mut().extend(origins));
}

/// Capture the origins of the lanes where `keep` is false (the lanes that
/// violated the premise; the `keep` lanes continue executing).
pub fn capture_dropped(state: &State, keep: &[bool]) {
    let origins = read_origins(state);
    assert_eq!(origins.len(), keep.len(), "deopt_collect: mask length mismatch");
    SINK.with(|s| {
        s.borrow_mut().extend(
            origins
                .iter()
                .zip(keep)
                .filter(|(_, keep)| !**keep)
                .map(|(o, _)| *o),
        )
    });
}
