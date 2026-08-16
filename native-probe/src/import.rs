//! Import interpreter `State`s (saved frame snapshots) into the native
//! runtime, one lane at a time - the bridge that lets the compiled engine
//! run on real campaign states (gap census, abstract-oracle runs).
//!
//! Field names the program never mentions cannot be accessed by any
//! transpiled instruction, so heap fields outside `gen::FIELD_NAMES` are
//! dropped on import (they are unreachable dead weight for execution;
//! NOTE: they would matter for canonical row comparison, so the oracle
//! path must account for dropped fields explicitly).

use std::collections::HashMap;

use celeste_rust::interpreter::heap::HeapId;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};

use crate::gen;
use crate::runtime::{Cell, Rt, BUILTIN_NAMES, V};

/// Pick lane `lane` of a possibly-vector value. Interval values get a
/// PLACEHOLDER scalar (their low endpoint): fine for receiver/pointer
/// topology work (gap census), NOT for value-exact oracle runs - those
/// need the abstract value support (V::Ival) instead.
fn import_value(
    rt: &mut Rt,
    v: &Value,
    lane: usize,
    memo: &mut HashMap<HeapId, u32>,
    state: &State,
    placeholder_intervals: bool,
) -> V {
    match v {
        Value::Number(mv) => V::Num(match mv {
            MaybeVector::Scalar(n) => *n,
            MaybeVector::Vector(ns) => ns[lane],
        }),
        Value::NumberInterval(mv) => {
            assert!(
                placeholder_intervals,
                "interval cell reached without placeholder mode (oracle path needs V::Ival)"
            );
            let iv = match mv {
                MaybeVector::Scalar(iv) => iv.clone(),
                MaybeVector::Vector(ivs) => ivs[lane].clone(),
            };
            V::Num(iv.low)
        }
        Value::Bool(mv) => V::Bool(match mv {
            MaybeVector::Scalar(b) => *b,
            MaybeVector::Vector(bs) => bs[lane],
        }),
        Value::UnknownBool => V::UBool,
        Value::String(s) => {
            let id = rt.strings.len() as u32;
            rt.strings.push(s.clone());
            V::Str(id)
        }
        Value::Nil(_) => V::Nil,
        Value::Pointer(id) => V::Ptr(import_cell(rt, *id, lane, memo, state, placeholder_intervals)),
        Value::NilPointer(_) => V::NilPtr,
        Value::MaybeBool(_) => panic!("MaybeBool must never be stored (value.rs contract)"),
    }
}

fn import_cell(
    rt: &mut Rt,
    id: HeapId,
    lane: usize,
    memo: &mut HashMap<HeapId, u32>,
    state: &State,
    placeholder_intervals: bool,
) -> u32 {
    if let Some(&c) = memo.get(&id) {
        return c;
    }
    // Reserve the slot first: cycles (objects reference type tables which
    // reference update closures capturing nothing, but sub-tables can
    // point back) must terminate.
    let slot = rt.alloc(Cell::Val(V::Nil));
    memo.insert(id, slot);
    let cell = match state.heap.get(id) {
        HeapValue::Value(v) => {
            Cell::Val(import_value(rt, v, lane, memo, state, placeholder_intervals))
        }
        HeapValue::ObjectTable(fields) => {
            let mut sorted: Vec<(&String, &HeapId)> = fields.iter().collect();
            // Deterministic import order (FxHashMap iteration is not).
            sorted.sort();
            let mut out = Vec::with_capacity(sorted.len());
            for (name, fid) in sorted {
                let Some(f) = gen::field_id(name) else {
                    continue; // program never names this field: unreachable
                };
                let c = import_cell(rt, *fid, lane, memo, state, placeholder_intervals);
                out.push((f, c));
            }
            Cell::Obj(out)
        }
        HeapValue::ArrayTable(items) => Cell::Arr(
            items
                .iter()
                .map(|i| import_cell(rt, *i, lane, memo, state, placeholder_intervals))
                .collect(),
        ),
        HeapValue::UnknownTable => Cell::Unk,
        HeapValue::Closure(fun, caps) => {
            let f = gen::FN_NAMES
                .iter()
                .position(|n| *n == fun.as_str())
                .unwrap_or_else(|| panic!("closure of unknown fn {:?}", fun))
                as u32;
            let caps: Vec<V> = caps
                .iter()
                .map(|v| import_value(rt, v, lane, memo, state, placeholder_intervals))
                .collect();
            Cell::Clo(f, caps.into_boxed_slice())
        }
        HeapValue::BuiltinFun(name) => Cell::Bi(
            BUILTIN_NAMES
                .iter()
                .position(|n| *n == name.as_str())
                .unwrap_or_else(|| panic!("unknown builtin {:?}", name)) as u32,
        ),
    };
    rt.heap[slot as usize] = cell;
    slot
}

/// Import lane `lane` of `state` into a fresh globals table on `rt`
/// (heap is appended; call on a fresh Rt for a clean import).
pub fn import_lane(rt: &mut Rt, state: &State, lane: usize, placeholder_intervals: bool) {
    assert!(lane < state.vector_size, "lane out of range");
    let mut memo = HashMap::new();
    for (name, cell) in state.global_env.iter() {
        let Some(g) = gen::global_id(name) else {
            continue; // program never names this global
        };
        let c = import_cell(rt, *cell, lane, &mut memo, state, placeholder_intervals);
        rt.globals[g as usize] = c;
    }
}
