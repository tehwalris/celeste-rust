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
/// need the abstract value support (V::Ival) instead. Pointers RESERVE
/// their target (BFS discovery) instead of recursing.
fn conv_lane_value(
    rt: &mut Rt,
    v: &Value,
    lane: usize,
    memo: &mut HashMap<HeapId, u32>,
    queue: &mut std::collections::VecDeque<HeapId>,
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
        Value::Pointer(id) => V::Ptr(reserve_lane_cell(rt, *id, memo, queue)),
        Value::NilPointer(_) => V::NilPtr,
        Value::MaybeBool(_) => panic!("MaybeBool must never be stored (value.rs contract)"),
    }
}

/// Assign (or return) the heap slot for a cell, enqueueing it for
/// filling on first sight - the scalar twin of `reserve_cell`.
fn reserve_lane_cell(
    rt: &mut Rt,
    id: HeapId,
    memo: &mut HashMap<HeapId, u32>,
    queue: &mut std::collections::VecDeque<HeapId>,
) -> u32 {
    if let Some(&c) = memo.get(&id) {
        return c;
    }
    let slot = rt.alloc(Cell::Val(V::Nil));
    memo.insert(id, slot);
    queue.push_back(id);
    slot
}

/// Convert one cell's content, reserving (not recursing into) children.
fn fill_lane_cell(
    rt: &mut Rt,
    id: HeapId,
    lane: usize,
    memo: &mut HashMap<HeapId, u32>,
    queue: &mut std::collections::VecDeque<HeapId>,
    state: &State,
    placeholder_intervals: bool,
) {
    let slot = memo[&id];
    let cell = match state.heap.get(id) {
        HeapValue::Value(v) => {
            Cell::Val(conv_lane_value(rt, v, lane, memo, queue, placeholder_intervals))
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
                out.push((f, reserve_lane_cell(rt, *fid, memo, queue)));
            }
            Cell::Obj(out)
        }
        HeapValue::ArrayTable(items) => Cell::Arr(
            items
                .iter()
                .map(|i| reserve_lane_cell(rt, *i, memo, queue))
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
                .map(|v| conv_lane_value(rt, v, lane, memo, queue, placeholder_intervals))
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
}

/// Import lane `lane` of `state` into a fresh globals table on `rt`
/// (heap is appended; call on a fresh Rt for a clean import).
///
/// Traversal is the SAME BFS as `import_block` and the boundary's
/// canonical compaction (globals by gen index, then breadth-first), so
/// scalar heap ids == canonical cell ids and the census's result cells
/// can be dumped as slot ids directly (plans/columnar-engine.md "Slot
/// binding subtlety", option b). `assert_lane_matches_block` checks the
/// two importers agree on real data.
pub fn import_lane(rt: &mut Rt, state: &State, lane: usize, placeholder_intervals: bool) {
    assert!(lane < state.vector_size, "lane out of range");
    let mut memo = HashMap::new();
    let mut queue: std::collections::VecDeque<HeapId> = Default::default();
    for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        if let Some(cell) = state.global_env.get(*name) {
            let c = reserve_lane_cell(rt, *cell, &mut memo, &mut queue);
            rt.globals[gi] = c;
        }
    }
    while let Some(id) = queue.pop_front() {
        fill_lane_cell(rt, id, lane, &mut memo, &mut queue, state, placeholder_intervals);
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

/// Import a whole interpreter boundary `State` as one columnar block.
pub fn import_block(
    state: &State,
    cart: std::sync::Arc<celeste_rust::cart_data::CartData>,
    cache: std::sync::Arc<celeste_rust::collision_cache::CollisionCache>,
) -> Rt2 {
    import_block_mapped(state, cart, cache).0
}

/// `import_block` plus the canonical-id -> interpreter `HeapId` map
/// (index = canonical cell id), for tooling that needs to NAME columns
/// (the kernel row census). Execution paths use `import_block`.
pub fn import_block_mapped(
    state: &State,
    cart: std::sync::Arc<celeste_rust::cart_data::CartData>,
    cache: std::sync::Arc<celeste_rust::collision_cache::CollisionCache>,
) -> (Rt2, Vec<Option<HeapId>>) {
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
    // Traverse in the SAME order as the boundary's canonical BFS
    // (globals by gen index, then breadth-first), so imported cell ids
    // ARE the canonical ids - the invariant slot binding relies on
    // (plans/columnar-engine.md "Slot binding subtlety", option b).
    // import_cell2's recursion is depth-first, so drive it one cell at a
    // time from an explicit BFS frontier: enqueue a cell, convert it
    // WITHOUT recursing (children enqueue instead).
    let mut memo = HashMap::new();
    let mut queue: std::collections::VecDeque<HeapId> = Default::default();
    for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        if let Some(cell) = state.global_env.get(*name) {
            let c = reserve_cell(&mut rt2, *cell, &mut memo, &mut queue);
            rt2.globals[gi] = c;
        }
    }
    while let Some(id) = queue.pop_front() {
        fill_cell(&mut rt2, id, &mut memo, &mut queue, state);
    }
    // The BFS order IS the canonical order, so the shape key is valid
    // right away (the slot-binding shape gate reads it).
    rt2.shape_hash = rt2.shape_hash_of();
    let _ = NONE;
    let mut rev: Vec<Option<HeapId>> = vec![None; rt2.structure.len()];
    for (heap_id, canon) in memo.iter() {
        rev[*canon as usize] = Some(*heap_id);
    }
    (rt2, rev)
}

/// Assert the scalar importer (`import_lane`) and the vectorized one
/// (`import_block`) assign IDENTICAL cell numbers on the same state:
/// same heap size, same globals table, same per-cell kind and pointer
/// topology. This is what lets the scalar census's result-cell ids be
/// dumped directly as canonical slot ids.
pub fn assert_lane_matches_block(rt: &Rt, rt2: &Rt2) {
    assert_eq!(
        rt.heap.len(),
        rt2.structure.len(),
        "scalar/block importers disagree on cell count"
    );
    assert_eq!(rt.globals, rt2.globals, "scalar/block importers disagree on globals");
    for (i, (a, b)) in rt.heap.iter().zip(rt2.structure.iter()).enumerate() {
        let ok = match (a, b) {
            (Cell::Val(_), Cell2::Val) => true,
            (Cell::Obj(fa), Cell2::Obj(fb)) => fa == fb,
            (Cell::Arr(ia), Cell2::Arr(ib)) => ia == ib,
            (Cell::Unk, Cell2::Unk) => true,
            (Cell::Clo(fa, ca), Cell2::Clo(fb, cb)) => fa == fb && ca.len() == cb.len(),
            (Cell::Bi(a), Cell2::Bi(b)) => a == b,
            _ => false,
        };
        assert!(ok, "cell {} differs between importers: {:?} vs {:?}", i, a, b);
        // Pointer values must point at the same cell ids.
        if let (Cell::Val(V::Ptr(p)), Cell2::Val) = (a, b) {
            match &rt2.cols[i] {
                Col::U(AV::Ptr(q)) => assert_eq!(p, q, "cell {} pointer target differs", i),
                other => panic!("cell {} is Ptr({}) scalar but {:?} in block", i, p, other),
            }
        }
    }
}

/// Assign (or return) the slot for a heap cell, enqueueing it for
/// filling on first sight - the BFS discovery step.
fn reserve_cell(
    rt2: &mut Rt2,
    id: HeapId,
    memo: &mut HashMap<HeapId, u32>,
    queue: &mut std::collections::VecDeque<HeapId>,
) -> u32 {
    if let Some(&c) = memo.get(&id) {
        return c;
    }
    let slot = rt2.structure.len() as u32;
    rt2.structure.push(Cell2::Val);
    rt2.cols.push(Col::U(AV::Nil));
    memo.insert(id, slot);
    queue.push_back(id);
    slot
}

/// Convert one cell's content, reserving (not recursing into) children.
fn fill_cell(
    rt2: &mut Rt2,
    id: HeapId,
    memo: &mut HashMap<HeapId, u32>,
    queue: &mut std::collections::VecDeque<HeapId>,
    state: &State,
) {
    let slot = memo[&id];
    let conv_value = |rt2: &mut Rt2,
                      v: &Value,
                      memo: &mut HashMap<HeapId, u32>,
                      queue: &mut std::collections::VecDeque<HeapId>|
     -> Col {
        match v {
            Value::Pointer(p) => Col::U(AV::Ptr(reserve_cell(rt2, *p, memo, queue))),
            other => import_scalar_value(rt2, other, state),
        }
    };
    let (cell, col) = match state.heap.get(id) {
        HeapValue::Value(v) => {
            let col = conv_value(rt2, v, memo, queue);
            (Cell2::Val, col)
        }
        HeapValue::ObjectTable(fields) => {
            let mut sorted: Vec<(&String, &HeapId)> = fields.iter().collect();
            sorted.sort();
            let mut out = Vec::with_capacity(sorted.len());
            for (name, fid) in sorted {
                let Some(f) = gen::field_id(name) else {
                    continue;
                };
                out.push((f, reserve_cell(rt2, *fid, memo, queue)));
            }
            (Cell2::Obj(out), Col::U(AV::Nil))
        }
        HeapValue::ArrayTable(items) => (
            Cell2::Arr(
                items
                    .iter()
                    .map(|i| reserve_cell(rt2, *i, memo, queue))
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
                .map(|v| conv_value(rt2, v, memo, queue))
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
}

/// Non-pointer value conversion (shared by cell values and captures).
fn import_scalar_value(rt2: &mut Rt2, v: &Value, state: &State) -> Col {
    let w = state.vector_size;
    match v {
        Value::Number(MaybeVector::Scalar(n)) => Col::U(AV::Num(*n)),
        Value::Number(MaybeVector::Vector(ns)) => {
            assert_eq!(ns.len(), w);
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
        Value::Pointer(_) => unreachable!("handled by conv_value"),
        Value::NilPointer(_) => Col::U(AV::NilPtr),
        Value::MaybeBool(_) => panic!("MaybeBool must never be stored (value.rs contract)"),
    }
}
