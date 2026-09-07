//! Bridging the OLD vectorized interpreter and the NEW reference interpreter.
//!
//! Two directions and a gate:
//!
//! * `to_trace_state` turns ONE LANE of an old-interpreter boundary `State`
//!   into a `trace::state::State<RefDomain>` (a fresh heap). This is the
//!   INPUT bridge: it feeds the reference driver a mid-game state that the
//!   old interpreter (or a checkpoint) produced.
//! * `to_interp_state` is its inverse for a single-lane trace state, and
//!   `row_key_of` runs that inverse through `engine_row_keys` so a reference
//!   output gets the SAME canonical `(shape_hash, content_hash)` the compiled
//!   engine assigns. Going through `engine_row_keys` rather than re-deriving
//!   the hash is deliberate - there is exactly one keyer and both sides use
//!   it.
//! * `gate` (behind an ignored test) runs an early checkpoint frame through
//!   both interpreters and asserts the two output row-key SETS agree.
//!
//! ## The two heap models, and the box convention that separates them
//!
//! The old interpreter BOXES every slot: a global, an object field and an
//! array element are each their own `HeapValue::Value` cell, and a reference
//! to a table is a `Value::Pointer` INTO that box. So `player.spd` is
//! `player(ObjectTable){spd -> box}` -> `box(Value::Pointer(spdCell))` ->
//! `spdCell(ObjectTable)`. The tracer's heap has no boxes: a `Table.hash`
//! maps a name straight to a `Value`, and `Value::Table(id)` is the only way
//! to name a sub-table. The canonical row hash (`Rt2::shape_hash_of` +
//! `boundary_finish`) walks the WHOLE reachable heap, so the boxes are part
//! of the key: `to_interp_state` must re-materialize every one of them, and
//! it does, uniformly.
//!
//! ## `__button_states`
//!
//! `__button_states` is a hashed global, and the two interpreters leave it in
//! different states at a frame boundary. The old program resets the buttons
//! LAST (`_update();_draw();__reset_button_states()`), so its output holds
//! `UnknownBool`; the reference driver resets them FIRST (so the six free
//! choices are forkable) and never resets again, so its output holds the
//! concrete forked bool. The engine's own kernels treat the buttons as
//! dead-at-boundary and write `UBool`, so that is the canonical form:
//! `to_interp_state` rewrites every bool under `__button_states` back to
//! `UnknownBool`. `to_trace_state` maps the incoming `UnknownBool` to a
//! placeholder `false` (it is overwritten by the reset before it is read).

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, bail, Result};

use celeste_core::pico8_num::Pico8NumInterval as Iv;
use celeste_names as gen;

use crate::builtins::BUILTIN_NAMES;
use crate::interpreter::heap::HeapId;
use crate::interpreter::state::State as OState;
use crate::interpreter::value::{HeapValue, MaybeVector, Value as OValue};

use crate::ir::GlobalId;
use crate::cengine::heap::{Heap as THeap, TableId, Value as TValue};
use crate::cengine::refdomain::RefDomain;
use crate::cengine::state::State as TState;

const BUTTON_GLOBAL: &str = "__button_states";

// ------------------------------------------------------------------
// lane readers
// ------------------------------------------------------------------

fn lane_of<T: Clone + std::fmt::Debug + PartialEq + Eq>(mv: &MaybeVector<T>, lane: usize) -> T {
    match mv {
        MaybeVector::Scalar(s) => s.clone(),
        MaybeVector::Vector(v) => v[lane].clone(),
    }
}

/// The `&'static str` interning a builtin name; the trace `Value::Builtin`
/// wants a `'static`, and `BUILTIN_NAMES` is the canonical static table.
fn builtin_static(name: &str) -> Result<&'static str> {
    BUILTIN_NAMES
        .iter()
        .copied()
        .find(|n| *n == name)
        .ok_or_else(|| anyhow!("unknown builtin {:?} has no static name", name))
}

// ==================================================================
// INPUT bridge: old State (one lane) -> trace State<RefDomain>
// ==================================================================

struct ToTrace<'a> {
    old: &'a OState,
    lane: usize,
    heap: THeap<RefDomain>,
    /// old target cell (table/closure/builtin) -> trace value naming it.
    memo: HashMap<HeapId, TValue<RefDomain>>,
}

impl<'a> ToTrace<'a> {
    /// Read a SLOT (a box HeapId): a global, an object field or an array
    /// element. Almost always `HeapValue::Value`; a slot that points
    /// straight at a target is tolerated for robustness.
    fn conv_slot(&mut self, box_id: HeapId) -> Result<TValue<RefDomain>> {
        match self.old.heap.get(box_id) {
            HeapValue::Value(v) => self.conv_value(&v.clone()),
            _ => self.conv_target(box_id),
        }
    }

    fn conv_value(&mut self, v: &OValue) -> Result<TValue<RefDomain>> {
        Ok(match v {
            OValue::Number(mv) => TValue::Num(Iv::from_number(lane_of(mv, self.lane))),
            OValue::NumberInterval(mv) => {
                let iv = lane_of(mv, self.lane);
                TValue::Num(Iv::new(iv.low, iv.high))
            }
            OValue::Bool(mv) => TValue::Bool(lane_of(mv, self.lane)),
            // Overwritten by `__reset_button_states` before any read; a
            // definite placeholder keeps RefDomain's `Bool = bool` total.
            OValue::UnknownBool => TValue::Bool(false),
            OValue::String(s) => TValue::Str(Arc::from(s.as_str())),
            OValue::Nil(_) => TValue::Nil,
            OValue::NilPointer(_) => TValue::Nil,
            OValue::Pointer(id) => self.conv_target(*id)?,
            OValue::MaybeBool(_) => bail!("MaybeBool must never be stored (value.rs contract)"),
        })
    }

    fn conv_target(&mut self, id: HeapId) -> Result<TValue<RefDomain>> {
        if let Some(v) = self.memo.get(&id) {
            return Ok(v.clone());
        }
        match self.old.heap.get(id).clone() {
            HeapValue::ObjectTable(fields) => {
                let t = self.heap.new_table();
                self.memo.insert(id, TValue::Table(t));
                // Sort by name so the (concrete) heap is deterministic; the
                // trace `Table.hash` is a BTreeMap so order is by key anyway.
                let mut names: Vec<(&String, &HeapId)> = fields.iter().collect();
                names.sort();
                for (name, fid) in names {
                    let cv = self.conv_slot(*fid)?;
                    self.heap.tables.get_mut(&t).unwrap().hash.insert(name.clone(), cv);
                }
                Ok(TValue::Table(t))
            }
            HeapValue::ArrayTable(items) => {
                let t = self.heap.new_table();
                self.memo.insert(id, TValue::Table(t));
                for it in items {
                    let cv = self.conv_slot(it)?;
                    self.heap.tables.get_mut(&t).unwrap().arr.push(cv);
                }
                Ok(TValue::Table(t))
            }
            HeapValue::Closure(gid, caps) => {
                // Captured values are stored positionally in the old model;
                // the trace model names them, so synthesize names `cap0..`
                // and hang them off a fresh scope. `body` is a placeholder
                // (0): a state built only for KEYING is never executed, and
                // the gate patches the body to the interp's real one before
                // running (see `patch_closures_for`).
                let env = self.heap.new_scope(None);
                let mut capture_names = Vec::with_capacity(caps.len());
                for (i, cv) in caps.iter().enumerate() {
                    let name = format!("cap{}", i);
                    let tv = self.conv_value(cv)?;
                    self.heap.scopes.get_mut(&env).unwrap().vars.insert(name.clone(), tv);
                    capture_names.push(name);
                }
                let fn_id = gen::FN_NAMES.iter().position(|n| *n == gid.as_str()).map(|p| p as u32);
                let c = self.heap.new_closure(0, env, fn_id, capture_names);
                self.memo.insert(id, TValue::Func(c));
                Ok(TValue::Func(c))
            }
            HeapValue::BuiltinFun(name) => {
                let v = TValue::Builtin(builtin_static(&name)?);
                self.memo.insert(id, v.clone());
                Ok(v)
            }
            HeapValue::UnknownTable => {
                // A freshly-created `{}` that never had a field stored
                // (`StoreEmptyTable`). Its trace equivalent is an empty
                // table; `to_interp_state` maps an empty table back to
                // `UnknownTable`.
                let t = self.heap.new_table();
                self.memo.insert(id, TValue::Table(t));
                Ok(TValue::Table(t))
            }
            HeapValue::Value(v) => {
                // A pointer chain landing on a boxed value (not a table);
                // convert it as a plain value (no identity to preserve).
                self.conv_value(&v.clone())
            }
        }
    }
}

/// Build a `State<RefDomain>` equivalent to LANE `lane` of an old-interpreter
/// boundary state. Preserves table identity (shared pointers become a shared
/// `TableId`). Closures get a placeholder body id; see `patch_closures_for`.
pub fn to_trace_state(
    old: &OState,
    lane: usize,
    _d: &mut RefDomain,
) -> Result<TState<RefDomain>> {
    assert!(
        old.local_env.iter().count() == 0 && old.outer_local_envs.is_empty(),
        "to_trace_state expects a boundary state (empty local envs)"
    );
    if lane >= old.vector_size {
        bail!("lane {} out of range (vector_size {})", lane, old.vector_size);
    }
    let mut cx = ToTrace { old, lane, heap: THeap::default(), memo: HashMap::new() };
    let globals = cx.heap.new_table();
    let scope = cx.heap.new_scope(None);
    // Globals in sorted order (OrdMap is already sorted).
    let names: Vec<(String, HeapId)> =
        old.global_env.iter().map(|(k, v)| (k.clone(), *v)).collect();
    for (name, box_id) in names {
        let v = cx.conv_slot(box_id)?;
        cx.heap.tables.get_mut(&globals).unwrap().hash.insert(name, v);
    }
    Ok(TState {
        heap: cx.heap,
        globals,
        scope,
        stack: Vec::new(),
        guard: true,
        ok: true,
        path: Vec::new(),
    })
}

/// What a function's closures look like in a state that the interp itself
/// built: the registered body id, and the NAMES the closure captures (which
/// the old positional model does not record and `to_trace_state` cannot know).
pub type FnInfo = HashMap<u32, (crate::cengine::heap::BodyId, Vec<String>)>;

/// Read `FnInfo` off a state the interp built by running the cart toplevel +
/// `_init`. Every instance of one function agrees on body and capture names,
/// so the last-writer-wins map is well defined; a disagreement is a bug and
/// is surfaced.
pub fn fn_info_of(base: &TState<RefDomain>) -> Result<FnInfo> {
    let mut map: FnInfo = HashMap::new();
    for cl in base.heap.closures.values() {
        let Some(fn_id) = cl.fn_id else { continue };
        let entry = (cl.body, cl.captures.clone());
        if let Some(prev) = map.get(&fn_id) {
            if *prev != entry {
                bail!(
                    "fn_id {} ({}) has inconsistent closures across instances",
                    fn_id,
                    gen::FN_NAMES[fn_id as usize]
                );
            }
        }
        map.insert(fn_id, entry);
    }
    Ok(map)
}

/// Repoint every closure in a bridged state at the REAL body the interp
/// registered, and rename its synthetic `cap{i}` captures to the interp's
/// real capture names, so the reference driver can CALL the methods and their
/// captured `obj` resolves. The bridge stamps a placeholder body id and
/// positional names; this fixes both from `FnInfo`.
pub fn patch_closures_for(st: &mut TState<RefDomain>, fn_info: &FnInfo) -> Result<()> {
    let ids: Vec<_> = st.heap.closures.keys().copied().collect();
    for cid in ids {
        let (fn_id, old_caps) = {
            let cl = &st.heap.closures[&cid];
            (cl.fn_id, cl.captures.clone())
        };
        let Some(fn_id) = fn_id else {
            bail!("bridged closure #{} has no fn_id (anonymous) - cannot patch", cid);
        };
        let (body, names) = fn_info.get(&fn_id).ok_or_else(|| {
            anyhow!("no registered body for fn_id {} ({})", fn_id, gen::FN_NAMES[fn_id as usize])
        })?;
        if names.len() != old_caps.len() {
            bail!(
                "fn_id {} ({}): bridged {} captures but interp has {} - shape mismatch",
                fn_id,
                gen::FN_NAMES[fn_id as usize],
                old_caps.len(),
                names.len()
            );
        }
        // Rename the captured values in the closure's own env scope from
        // `cap{i}` to the real name, in order.
        let env = st.heap.closures[&cid].env;
        for (i, real) in names.iter().enumerate() {
            let synth = format!("cap{}", i);
            if let Some(v) = st.heap.scopes.get_mut(&env).unwrap().vars.remove(&synth) {
                st.heap.scopes.get_mut(&env).unwrap().vars.insert(real.clone(), v);
            }
        }
        let cl = st.heap.closures.get_mut(&cid).unwrap();
        cl.body = *body;
        cl.captures = names.clone();
    }
    Ok(())
}

/// Add the tracer's native BUILTIN globals that a bridged state is missing,
/// copying them from a `base` state the interp built. The old frontend
/// CONSUMES some builtin calls at compile time (`_hint_normalize` marks a
/// merge block rather than calling a function), so those names never appear in
/// an old-interpreter state - but the AST interpreter really calls them, so
/// they must be present. They sit OUTSIDE `gen::GLOBAL_NAMES`, so they are
/// dropped on import and do not affect any row key.
pub fn add_missing_builtins(st: &mut TState<RefDomain>, base: &TState<RefDomain>) {
    let base_g = &base.heap.tables[&base.globals].hash;
    let g = st.globals;
    let present: std::collections::BTreeSet<String> =
        st.heap.tables[&g].hash.keys().cloned().collect();
    let add: Vec<(String, &'static str)> = base_g
        .iter()
        .filter_map(|(k, v)| match v {
            TValue::Builtin(n) if !present.contains(k) => Some((k.clone(), *n)),
            _ => None,
        })
        .collect();
    for (k, n) in add {
        st.heap.tables.get_mut(&g).unwrap().hash.insert(k, TValue::Builtin(n));
    }
}

// ==================================================================
// OUTPUT bridge: trace State<RefDomain> (one lane) -> old State
// ==================================================================

struct ToInterp<'a> {
    ts: &'a TState<RefDomain>,
    st: OState,
    memo_t: HashMap<TableId, HeapId>,
    memo_c: HashMap<crate::cengine::heap::ClosureId, HeapId>,
    memo_b: HashMap<&'static str, HeapId>,
}

impl<'a> ToInterp<'a> {
    /// Convert a trace value to an old `Value` (a scalar, or a `Pointer` to a
    /// target cell). `ubool` forces booleans to `UnknownBool` (the
    /// `__button_states` subtree).
    fn conv_value(&mut self, v: &TValue<RefDomain>, ubool: bool) -> Result<OValue> {
        Ok(match v {
            TValue::Nil => OValue::Nil(None),
            TValue::Num(iv) => match iv.to_number() {
                Some(n) => OValue::Number(MaybeVector::Scalar(n)),
                None => OValue::NumberInterval(MaybeVector::Scalar(*iv)),
            },
            TValue::Bool(_) if ubool => OValue::UnknownBool,
            TValue::Bool(b) => OValue::Bool(MaybeVector::Scalar(*b)),
            TValue::Str(s) => OValue::String(s.to_string()),
            TValue::Table(t) => OValue::Pointer(self.conv_table(*t, ubool)?),
            TValue::Func(c) => OValue::Pointer(self.conv_closure(*c)?),
            TValue::Builtin(name) => OValue::Pointer(self.conv_builtin(name)?),
        })
    }

    /// A SLOT (global / field / array element) holding `v`, returning the slot
    /// cell's id.
    ///
    /// The old interpreter seeds native BUILTINS inline: their global cell IS
    /// the `BuiltinFun` (a read returns a self-pointer), so the first slot
    /// holding a builtin is its canonical cell and a later slot is a
    /// `Value::Pointer` to it. Everything else - CLOSURES and TABLES alike -
    /// is boxed: the constructor (`StoreClosure` / a table constructor)
    /// allocates its own cell and the slot holds a `Value::Pointer` to it (two
    /// cells). Getting the builtin case wrong adds a cell per builtin and
    /// diverges every downstream canonical id.
    fn box_slot(&mut self, v: &TValue<RefDomain>, ubool: bool) -> Result<HeapId> {
        match v {
            TValue::Builtin(name) => {
                if let Some(&h) = self.memo_b.get(name) {
                    let id = self.st.heap.alloc();
                    self.st.heap.set(id, HeapValue::Value(OValue::Pointer(h)));
                    Ok(id)
                } else {
                    self.conv_builtin(name)
                }
            }
            _ => {
                let ov = self.conv_value(v, ubool)?;
                let id = self.st.heap.alloc();
                self.st.heap.set(id, HeapValue::Value(ov));
                Ok(id)
            }
        }
    }

    fn conv_table(&mut self, t: TableId, ubool: bool) -> Result<HeapId> {
        if let Some(&c) = self.memo_t.get(&t) {
            return Ok(c);
        }
        let cell = self.st.heap.alloc();
        self.memo_t.insert(t, cell);
        let table = &self.ts.heap.tables[&t];
        // Object vs Array: string keys -> ObjectTable, else the array part
        // -> ArrayTable. The two are disjoint in this program (the IR chose
        // one at each allocation site); a table carrying both parts is a
        // modelling surprise worth surfacing.
        let has_hash = !table.hash.is_empty();
        let has_arr = !table.arr.is_empty();
        if !table.ints.is_empty() {
            bail!("to_interp_state: table #{} has a non-empty integer part - unsupported", t);
        }
        if has_hash && has_arr {
            bail!("to_interp_state: table #{} has both string and array parts", t);
        }
        let hv = if !has_hash && !has_arr {
            // An empty table is an as-yet-unwritten `{}`: `UnknownTable`,
            // the inverse of `to_trace_state`'s UnknownTable handling.
            HeapValue::UnknownTable
        } else if has_hash {
            // Object (also the default for an empty table). The map type is
            // `HeapValue::ObjectTable`'s (an FxHashMap); build it by
            // inference through the constructor below.
            let mut fields = std::collections::HashMap::with_hasher(Default::default());
            // Collect first to avoid borrowing `self.ts` across `self` mutation.
            let entries: Vec<(String, TValue<RefDomain>)> =
                table.hash.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            for (name, v) in entries {
                let child_ubool = ubool || name_is_buttons(&name);
                let bid = self.box_slot(&v, child_ubool)?;
                fields.insert(name, bid);
            }
            HeapValue::ObjectTable(fields)
        } else {
            let items: Vec<TValue<RefDomain>> = table.arr.to_vec();
            let mut out = Vec::with_capacity(items.len());
            for v in items {
                out.push(self.box_slot(&v, ubool)?);
            }
            HeapValue::ArrayTable(out)
        };
        self.st.heap.set(cell, hv);
        Ok(cell)
    }

    fn conv_closure(&mut self, c: crate::cengine::heap::ClosureId) -> Result<HeapId> {
        if let Some(&h) = self.memo_c.get(&c) {
            return Ok(h);
        }
        let cell = self.st.heap.alloc();
        self.memo_c.insert(c, cell);
        let cl = self.ts.heap.closures[&c].clone();
        let fn_id = cl
            .fn_id
            .ok_or_else(|| anyhow!("anonymous closure #{} cannot be exported", c))?;
        let gid = GlobalId::from(gen::FN_NAMES[fn_id as usize].to_string());
        // Captures, positionally, looked up by name in the closure's scope.
        let mut caps = Vec::with_capacity(cl.captures.len());
        for name in &cl.captures {
            let v = self
                .ts
                .heap
                .lookup(cl.env, name)
                .ok_or_else(|| anyhow!("capture {:?} of closure #{} not in its scope", name, c))?
                .clone();
            caps.push(self.conv_value(&v, false)?);
        }
        self.st.heap.set(cell, HeapValue::Closure(gid, caps));
        Ok(cell)
    }

    fn conv_builtin(&mut self, name: &'static str) -> Result<HeapId> {
        if let Some(&h) = self.memo_b.get(name) {
            return Ok(h);
        }
        let cell = self.st.heap.alloc();
        self.st.heap.set(cell, HeapValue::BuiltinFun(name.to_string()));
        self.memo_b.insert(name, cell);
        Ok(cell)
    }
}

fn name_is_buttons(name: &str) -> bool {
    name == BUTTON_GLOBAL
}

/// Turn a single-lane trace state into a one-lane old-interpreter boundary
/// state - the inverse of `to_trace_state`.
pub fn to_interp_state(ts: &TState<RefDomain>) -> Result<OState> {
    let mut cx = ToInterp {
        ts,
        st: OState::new(),
        memo_t: HashMap::new(),
        memo_c: HashMap::new(),
        memo_b: HashMap::new(),
    };
    cx.st.vector_size = 1;
    let globals = &ts.heap.tables[&ts.globals];
    let entries: Vec<(String, TValue<RefDomain>)> =
        globals.hash.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    for (name, v) in entries {
        // The tracer's base registers COMPILE-TIME hint builtins
        // (`_hint_normalize` etc.) that the frontend consumes, so a real
        // interpreter state never holds them and re-importing one
        // (`to_trace_state`/`builtin_static`) would fail. They sit outside
        // `GLOBAL_NAMES`, so no row key sees them; drop them on export, the
        // inverse of `add_missing_builtins` adding them on import.
        if let TValue::Builtin(n) = &v {
            if !BUILTIN_NAMES.iter().any(|b| b == n) {
                continue;
            }
        }
        let ubool = name_is_buttons(&name);
        let bid = cx.box_slot(&v, ubool)?;
        cx.st.global_env.insert(name, bid);
    }
    Ok(cx.st)
}

/// The canonical `(shape_hash, content_hash)` of a single-lane reference
/// output, exactly as `engine_row_keys` computes it for the old interpreter.
pub fn row_key_of(st: &TState<RefDomain>) -> Result<(u64, u64)> {
    let ostate = to_interp_state(st)?;
    let keys = crate::compiled::engine_row_keys(&ostate)?;
    if keys.len() != 1 {
        bail!("row_key_of: expected 1 lane, got {}", keys.len());
    }
    Ok(keys[0])
}

/// The engine keys of an interpreter output, put through the SAME campaign
/// abstraction the search applies before it keys or checkpoints a frame
/// (`split_precision_straddles` + `make_state_abstract`). Raw, un-abstracted
/// multi-lane outputs are NOT in the search's key space - `engine_row_keys`
/// is only consistent on abstracted states - so both sides of a differential
/// (the bridge gate AND the sampled kernel gate) must abstract before keying.
#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    const CKPT: &str = "/var/tmp/celeste-checkpoints/kfwd/room1";


    /// The conversion round-trips: old lane -> trace -> old must reproduce the
    /// SAME engine row key the original lane has. Isolates the two bridge
    /// functions from the reference driver.
    #[test]
    #[ignore]
    fn bridge_round_trips_engine_keys() {
        let dir = Path::new(CKPT);
        let frame = std::env::var("FRAME").ok().and_then(|s| s.parse().ok()).unwrap_or(5u32);
        let states = crate::search::checkpoint::load_frame_states(dir, frame).expect("load frame");
        let cap: usize = std::env::var("MAXLANES").ok().and_then(|s| s.parse().ok()).unwrap_or(300);
        let mut d = RefDomain::new();
        let mut ok = 0usize;
        let mut bad = 0usize;
        'outer: for (si, s) in states.iter().enumerate() {
            let want = crate::compiled::engine_row_keys(s).expect("engine keys");
            for lane in 0..s.vector_size {
                if ok + bad >= cap {
                    break 'outer;
                }
                let ts = match to_trace_state(s, lane, &mut d) {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("  state {} lane {}: to_trace_state FAILED: {:#}", si, lane, e);
                        bad += 1;
                        continue;
                    }
                };
                match row_key_of(&ts) {
                    Ok(got) if got == want[lane] => ok += 1,
                    Ok(got) => {
                        eprintln!(
                            "  state {} lane {}: KEY MISMATCH got={:?} want={:?}",
                            si, lane, got, want[lane]
                        );
                        bad += 1;
                    }
                    Err(e) => {
                        eprintln!("  state {} lane {}: row_key_of FAILED: {:#}", si, lane, e);
                        bad += 1;
                    }
                }
            }
        }
        eprintln!("round-trip: {} ok, {} bad", ok, bad);
        assert_eq!(bad, 0, "{} lanes did not round-trip", bad);
    }


}
