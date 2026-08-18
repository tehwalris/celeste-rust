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

// ---- export: columnar block -> interpreter State ----

/// The inverse of `import_block`: turn a columnar block back into an
/// interpreter `State`, so the reference interpreter can run a frame the
/// compiled engine declined (plans/k4-retirement-plan.md stage 2).
///
/// Fidelity, stated precisely, because the fallback's whole job is to be
/// the REFERENCE:
///
/// * Cell ids become heap ids in the SAME order, so `import_block` of the
///   result reproduces this block's canonical numbering exactly. The
///   round trip is checked by `assert_block_round_trips`.
/// * Fields the program never names were dropped on import and cannot be
///   restored here. That is not a new loss - it is the same content the
///   boundary compares - but it means export is the inverse of import,
///   not of the original State.
/// * A block is only exportable at a FRAME BOUNDARY. Mid-frame columns can
///   hold per-lane values of mixed KIND, which no single interpreter
///   `Value` can represent, and mid-frame blocks carry a live arena and
///   COW history this does not read. Both are checked below rather than
///   assumed: a mixed column is a hard error, not a guess.
pub fn export_block(rt2: &Rt2) -> State {
    assert!(
        rt2.arena.is_empty(),
        "export_block on a mid-frame block: the local arena is live ({} slots)",
        rt2.arena.len()
    );
    assert!(rt2.width > 0, "export_block on a zero-lane block");
    let mut state = State::new();
    let ids: Vec<HeapId> = (0..rt2.structure.len()).map(|_| state.heap.alloc()).collect();
    for (i, cell) in rt2.structure.iter().enumerate() {
        let value = match cell {
            Cell2::Val => HeapValue::Value(export_col(rt2, &rt2.cols[i], &ids, i)),
            Cell2::Obj(fields) => HeapValue::ObjectTable(
                fields
                    .iter()
                    .map(|(f, c)| (gen::FIELD_NAMES[*f as usize].to_string(), ids[*c as usize]))
                    .collect(),
            ),
            Cell2::Arr(items) => {
                HeapValue::ArrayTable(items.iter().map(|c| ids[*c as usize]).collect())
            }
            Cell2::Unk => HeapValue::UnknownTable,
            Cell2::Clo(f, caps) => HeapValue::Closure(
                celeste_rust::ir::GlobalId::from(gen::FN_NAMES[*f as usize].to_string()),
                caps.iter().map(|c| export_col(rt2, c, &ids, i)).collect(),
            ),
            Cell2::Bi(b) => HeapValue::BuiltinFun(BUILTIN_NAMES[*b as usize].to_string()),
        };
        state.heap.set(ids[i], value);
    }
    for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        let slot = rt2.globals[gi];
        if slot != NONE {
            state.global_env.insert(name.to_string(), ids[slot as usize]);
        }
    }
    state.vector_size = rt2.width;
    state.prints = rt2.prints.clone();
    state
}

/// One column back to a `Value`. `cell` is only used for error messages.
fn export_col(rt2: &Rt2, col: &Col, ids: &[HeapId], cell: usize) -> Value {
    let w = rt2.width;
    let scalar = |a: &AV| -> Value {
        match a {
            AV::Num(n) => Value::Number(MaybeVector::Scalar(*n)),
            AV::Ival(l, h) => Value::NumberInterval(MaybeVector::Scalar(
                celeste_rust::pico8_num::Pico8NumInterval::new(*l, *h),
            )),
            AV::Bool(b) => Value::Bool(MaybeVector::Scalar(*b)),
            AV::UBool => Value::UnknownBool,
            AV::Str(s) => Value::String(rt2.strings[*s as usize].clone()),
            AV::Nil => Value::Nil(None),
            AV::Ptr(p) => Value::Pointer(ids[*p as usize]),
            // Import loses the NilPointer's field name (AV has no room
            // for it) and nothing reads it, so a placeholder is honest
            // about what survived rather than inventing a name.
            AV::NilPtr => Value::NilPointer(String::new()),
        }
    };
    use std::sync::Arc;
    let iv = |l: &crate::runtime2::P8, h: &crate::runtime2::P8| {
        celeste_rust::pico8_num::Pico8NumInterval::new(*l, *h)
    };
    match col {
        Col::U(a) => scalar(a),
        Col::N(v) => {
            assert_eq!(v.len(), w, "cell {}: short Num column", cell);
            Value::Number(MaybeVector::Vector(Arc::new(v.clone())))
        }
        Col::I(v) => {
            assert_eq!(v.len(), w, "cell {}: short interval column", cell);
            Value::NumberInterval(MaybeVector::Vector(Arc::new(
                v.iter().map(|(l, h)| iv(l, h)).collect(),
            )))
        }
        // A varying column has to become ONE interpreter Value, so every
        // lane must agree on the KIND. At a frame boundary they do (the
        // block came from a State, which has the same restriction);
        // mid-frame they need not, which is why a mixed column is an
        // error rather than a fallback to an encoding that does not
        // exist. Dispatching on lane 0 and then requiring every other
        // lane to match IS the check.
        Col::V(v) => {
            assert_eq!(v.len(), w, "cell {}: short varying column", cell);
            let mixed = |other: &AV, kind: &str| -> ! {
                panic!(
                    "cell {}: a varying column mixes {} with {:?} - no single \
                     interpreter Value can hold that",
                    cell, kind, other
                )
            };
            match v[0] {
                AV::Bool(_) => Value::Bool(MaybeVector::Vector(Arc::new(
                    v.iter()
                        .map(|a| match a {
                            AV::Bool(b) => *b,
                            other => mixed(other, "Bool"),
                        })
                        .collect(),
                ))),
                AV::Num(_) => Value::Number(MaybeVector::Vector(Arc::new(
                    v.iter()
                        .map(|a| match a {
                            AV::Num(n) => *n,
                            other => mixed(other, "Num"),
                        })
                        .collect(),
                ))),
                AV::Ival(_, _) => Value::NumberInterval(MaybeVector::Vector(Arc::new(
                    v.iter()
                        .map(|a| match a {
                            AV::Ival(l, h) => iv(l, h),
                            other => mixed(other, "Ival"),
                        })
                        .collect(),
                ))),
                // Everything else is uniform-only in a State: a pointer
                // column that VARIES would be lane-dependent heap
                // topology, which the block model forbids outright.
                other => panic!(
                    "cell {}: a varying column of {:?} has no interpreter Value \
                     (pointer topology is lane-uniform by the block's shape premise)",
                    cell, other
                ),
            }
        }
    }
}

/// `import_block(export_block(b))` must reproduce `b`'s structure and
/// columns exactly. This is the fallback's correctness premise stated as
/// a check: if export loses or reorders anything, every frame the
/// interpreter runs for the engine is silently wrong.
pub fn assert_block_round_trips(rt2: &Rt2) {
    let state = export_block(rt2);
    let back = import_block(&state, rt2.cart.clone(), rt2.cache.clone());
    assert_eq!(back.width, rt2.width, "round trip changed the lane count");
    assert_eq!(
        back.structure.len(),
        rt2.structure.len(),
        "round trip changed the cell count"
    );
    assert_eq!(back.globals, rt2.globals, "round trip changed the globals table");
    for (i, (a, b)) in rt2.structure.iter().zip(back.structure.iter()).enumerate() {
        assert_eq!(
            format!("{:?}", a),
            format!("{:?}", b),
            "round trip changed cell {}'s structure",
            i
        );
    }
    for (i, (a, b)) in rt2.cols.iter().zip(back.cols.iter()).enumerate() {
        // Uniform vs a materialized all-equal column are the same VALUE;
        // compare per lane so a legitimate representation change is not
        // reported as a content change.
        if matches!(rt2.structure[i], Cell2::Val) {
            for lane in 0..rt2.width {
                assert_eq!(
                    a.at(lane),
                    b.at(lane),
                    "round trip changed cell {} lane {}",
                    i,
                    lane
                );
            }
        }
    }
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
