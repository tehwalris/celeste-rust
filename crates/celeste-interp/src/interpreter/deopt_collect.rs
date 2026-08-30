//! Per-lane origin columns for lane-granular deopt / backward sweep.
//!
//! A synthetic per-lane *origin* column is a global the program never reads,
//! carried automatically by every filter/expand/merge. The compiled deopt
//! driver and the backward sweep inject one before a retry and read it back
//! out afterwards, to re-run only the lanes they care about under the plain
//! program.

use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};

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
    use celeste_core::pico8_num::Pico8Num;
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
