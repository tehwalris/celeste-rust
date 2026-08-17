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

// ---- vectorized import: State -> columnar block (Rt2) ----
//
// The one-frame dev-loop bench (`--abstract-bench`) needs REAL boundary
// states with their abstract values intact: intervals become Col::I,
// lane-varying numbers become Col::N, UnknownBool stays UBool. One
// interpreter `State` (many lanes) maps to one `Rt2` block; pointer
// topology is lane-uniform in a State by construction, which is exactly
// the columnar block's shape premise.

use crate::runtime2::{Cell2, Col, Rt2, AV, NONE};

fn import_value2(
    rt2: &mut Rt2,
    v: &Value,
    memo: &mut HashMap<HeapId, u32>,
    state: &State,
) -> Col {
    let w = state.vector_size;
    match v {
        Value::Number(MaybeVector::Scalar(n)) => Col::U(AV::Num(*n)),
        Value::Number(MaybeVector::Vector(ns)) => {
            assert_eq!(ns.len(), w, "vector length != vector_size");
            Col::N(ns.to_vec())
        }
        Value::NumberInterval(MaybeVector::Scalar(iv)) => Col::U(AV::Ival(iv.low, iv.high)),
        Value::NumberInterval(MaybeVector::Vector(ivs)) => {
            assert_eq!(ivs.len(), w);
            Col::I(ivs.iter().map(|iv| (iv.low, iv.high)).collect())
        }
        Value::Bool(MaybeVector::Scalar(b)) => Col::U(AV::Bool(*b)),
        Value::Bool(MaybeVector::Vector(bs)) => {
            assert_eq!(bs.len(), w);
            Col::V(bs.iter().map(|b| AV::Bool(*b)).collect())
        }
        Value::UnknownBool => Col::U(AV::UBool),
        Value::String(s) => {
            let id = rt2.strings.len() as u32;
            rt2.strings.push(s.clone());
            Col::U(AV::Str(id))
        }
        Value::Nil(_) => Col::U(AV::Nil),
        Value::Pointer(id) => Col::U(AV::Ptr(import_cell2(rt2, *id, memo, state))),
        Value::NilPointer(_) => Col::U(AV::NilPtr),
        Value::MaybeBool(_) => panic!("MaybeBool must never be stored (value.rs contract)"),
    }
}

fn import_cell2(rt2: &mut Rt2, id: HeapId, memo: &mut HashMap<HeapId, u32>, state: &State) -> u32 {
    if let Some(&c) = memo.get(&id) {
        return c;
    }
    // Reserve first: cycles must terminate.
    let slot = rt2.structure.len() as u32;
    rt2.structure.push(Cell2::Val);
    rt2.cols.push(Col::U(AV::Nil));
    memo.insert(id, slot);
    let (cell, col) = match state.heap.get(id) {
        HeapValue::Value(v) => {
            let col = import_value2(rt2, v, memo, state);
            (Cell2::Val, col)
        }
        HeapValue::ObjectTable(fields) => {
            let mut sorted: Vec<(&String, &HeapId)> = fields.iter().collect();
            sorted.sort();
            let mut out = Vec::with_capacity(sorted.len());
            for (name, fid) in sorted {
                let Some(f) = gen::field_id(name) else {
                    continue; // program never names this field: unreachable
                };
                let c = import_cell2(rt2, *fid, memo, state);
                out.push((f, c));
            }
            (Cell2::Obj(out), Col::U(AV::Nil))
        }
        HeapValue::ArrayTable(items) => (
            Cell2::Arr(
                items
                    .iter()
                    .map(|i| import_cell2(rt2, *i, memo, state))
                    .collect(),
            ),
            Col::U(AV::Nil),
        ),
        HeapValue::UnknownTable => (Cell2::Unk, Col::U(AV::Nil)),
        HeapValue::Closure(fun, caps) => {
            let f = gen::FN_NAMES
                .iter()
                .position(|n| *n == fun.as_str())
                .unwrap_or_else(|| panic!("closure of unknown fn {:?}", fun))
                as u32;
            let caps: Vec<Col> = caps
                .iter()
                .map(|v| import_value2(rt2, v, memo, state))
                .collect();
            (Cell2::Clo(f, caps.into_boxed_slice()), Col::U(AV::Nil))
        }
        HeapValue::BuiltinFun(name) => (
            Cell2::Bi(
                BUILTIN_NAMES
                    .iter()
                    .position(|n| *n == name.as_str())
                    .unwrap_or_else(|| panic!("unknown builtin {:?}", name)) as u32,
            ),
            Col::U(AV::Nil),
        ),
    };
    rt2.structure[slot as usize] = cell;
    rt2.cols[slot as usize] = col;
    slot
}

/// Import a whole interpreter boundary `State` as one columnar block.
pub fn import_block(
    state: &State,
    cart: std::sync::Arc<celeste_rust::cart_data::CartData>,
    cache: std::sync::Arc<celeste_rust::collision_cache::CollisionCache>,
) -> Rt2 {
    assert!(
        state.local_env.iter().count() == 0 && state.outer_local_envs.is_empty(),
        "boundary states must have empty local envs"
    );
    let mut rt2 = Rt2::empty(
        state.vector_size,
        gen::GLOBAL_NAMES.len(),
        gen::STRINGS,
        cart,
        cache,
    );
    rt2.prints = state.prints.clone();
    let mut memo = HashMap::new();
    for (name, cell) in state.global_env.iter() {
        let Some(g) = gen::global_id(name) else {
            continue; // program never names this global
        };
        let c = import_cell2(&mut rt2, *cell, &mut memo, state);
        rt2.globals[g as usize] = c;
    }
    let _ = NONE;
    rt2
}
