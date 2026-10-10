//! The reference engine's edge: one lane of a block <-> a `State<RefDomain>`.
//!
//! A block BOXES every slot (each global, field and array element is a `Val`
//! cell; tables, closures and builtins are reached through an `AV::Ptr`,
//! except a builtin's FIRST slot in traversal order, which is its `Bi` cell).
//! The row key walks the whole structure, so `to_block` must rebuild the
//! boxes exactly as the kernels do for a reference successor to key like a
//! kernel row; `from_block` drops them.
//!
//! `__button_states`: the kernels write the buttons as `UBool`, so `to_block`
//! does too; `from_block` reads a `UBool` as a placeholder `false` (buttons
//! are overwritten before they are read; any other unknown boolean is
//! returned for the driver to fork). Names outside `celeste_names` (the
//! tracer's hint builtins) are dropped by `to_block` and restored from the
//! base state by `from_block`.

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, bail, ensure, Result};
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};
use celeste_engine::runtime2::{Cell2, Col, Rt2, AV, NONE};
use celeste_names as gen;

use crate::builtins::BUILTIN_NAMES;
use crate::trace::heap::{BodyId, ClosureId, Heap, TableId, Value};
use crate::trace::refdomain::RefDomain;
use crate::trace::state::State;

type TV = Value<RefDomain>;

const BUTTON_GLOBAL: &str = "__button_states";

/// Per function (`gen::FN_NAMES` index): the body the interpreter registered
/// and the names its closures capture, which a block does not record.
pub type FnInfo = HashMap<u32, (BodyId, Vec<String>)>;

/// `FnInfo` off a state the interpreter built (the cart toplevel + `_init`).
/// Every closure of one function agrees on both; a disagreement is a bug.
pub fn fn_info_of(base: &State<RefDomain>) -> Result<FnInfo> {
    let mut map: FnInfo = HashMap::new();
    for cl in base.heap.closures.values() {
        let Some(fn_id) = cl.fn_id else { continue };
        let entry = (cl.body, cl.captures.clone());
        if let Some(prev) = map.insert(fn_id, entry.clone()) {
            ensure!(prev == entry, "fn {} has inconsistent closures", gen::FN_NAMES[fn_id as usize]);
        }
    }
    Ok(map)
}

struct FromBlock<'a> {
    rt2: &'a Rt2,
    lane: usize,
    fn_info: &'a FnInfo,
    heap: Heap<RefDomain>,
    memo: HashMap<u32, TV>,
    unknown: Vec<(TableId, String)>,
}

impl FromBlock<'_> {
    fn is_unknown(&self, cell: u32) -> bool {
        matches!(self.rt2.structure[cell as usize], Cell2::Val) && self.rt2.cols[cell as usize].at(self.lane) == AV::UBool
    }

    fn slot(&mut self, cell: u32) -> Result<TV> {
        match self.rt2.structure[cell as usize] {
            Cell2::Val => self.value(self.rt2.cols[cell as usize].at(self.lane)),
            _ => self.target(cell),
        }
    }

    fn value(&mut self, v: AV) -> Result<TV> {
        Ok(match v {
            AV::Num(n) => TV::Num(Iv::from_number(n)),
            AV::Ival(lo, hi) => TV::Num(Iv::new(lo, hi)),
            // The whole range; a straddling comparison forks.
            AV::UNum => TV::Num(Iv::new(P8::from_raw(i32::MIN), P8::from_raw(i32::MAX))),
            AV::Bool(b) => TV::Bool(b),
            AV::UBool => TV::Bool(false),
            AV::Str(s) => TV::Str(Arc::from(self.rt2.strings[s as usize].as_str())),
            AV::Nil | AV::NilPtr => TV::Nil,
            AV::Ptr(p) => self.target(p)?,
        })
    }

    fn target(&mut self, cell: u32) -> Result<TV> {
        if let Some(v) = self.memo.get(&cell) {
            return Ok(v.clone());
        }
        let rt2 = self.rt2;
        Ok(match &rt2.structure[cell as usize] {
            Cell2::Obj(fields) => {
                let t = self.heap.new_table();
                self.memo.insert(cell, TV::Table(t));
                let mut named: Vec<(&str, u32)> = fields.iter().map(|&(f, c)| (gen::FIELD_NAMES[f as usize], c)).collect();
                named.sort();
                for (name, c) in named {
                    if self.is_unknown(c) {
                        self.unknown.push((t, name.to_string()));
                    }
                    let v = self.slot(c)?;
                    self.heap.tables.get_mut(&t).expect("just made").hash.insert(name.to_string(), v);
                }
                TV::Table(t)
            }
            Cell2::Arr(items) => {
                let t = self.heap.new_table();
                self.memo.insert(cell, TV::Table(t));
                for &c in items {
                    let v = self.slot(c)?;
                    self.heap.tables.get_mut(&t).expect("just made").arr.push(v);
                }
                TV::Table(t)
            }
            // A `{}` that never had a field stored.
            Cell2::Unk => {
                let t = self.heap.new_table();
                self.memo.insert(cell, TV::Table(t));
                TV::Table(t)
            }
            Cell2::Clo(f, caps) => {
                let (body, names) = self.fn_info.get(f).ok_or_else(|| anyhow!("no registered body for {}", gen::FN_NAMES[*f as usize]))?;
                ensure!(names.len() == caps.len(), "{}: {} captures in the block, {} registered", gen::FN_NAMES[*f as usize], caps.len(), names.len());
                let env = self.heap.new_scope(None);
                for (name, cap) in names.iter().zip(caps.iter()) {
                    let v = self.value(cap.at(self.lane))?;
                    self.heap.scopes.get_mut(&env).expect("just made").vars.insert(name.clone(), v);
                }
                let c = self.heap.new_closure(*body, env, Some(*f), names.clone());
                self.memo.insert(cell, TV::Func(c));
                TV::Func(c)
            }
            Cell2::Bi(b) => {
                let v = TV::Builtin(BUILTIN_NAMES[*b as usize]);
                self.memo.insert(cell, v.clone());
                v
            }
            Cell2::Val => self.value(rt2.cols[cell as usize].at(self.lane))?,
        })
    }
}

/// Lane `lane` of a block as a reference state (plus the base state's
/// builtins), and its unknown-boolean fields except the buttons, for the
/// driver to fork.
pub fn from_block(
    rt2: &Rt2,
    lane: usize,
    fn_info: &FnInfo,
    base: &State<RefDomain>,
) -> Result<(State<RefDomain>, Vec<(TableId, String)>)> {
    ensure!(lane < rt2.width, "lane {lane} of a {}-lane block", rt2.width);
    let mut cx = FromBlock { rt2, lane, fn_info, heap: Heap::default(), memo: HashMap::new(), unknown: Vec::new() };
    let globals = cx.heap.new_table();
    let scope = cx.heap.new_scope(None);
    let mut named: Vec<(&str, u32)> =
        gen::GLOBAL_NAMES.iter().zip(&rt2.globals).filter(|(_, &c)| c != NONE).map(|(n, &c)| (*n, c)).collect();
    named.sort();
    for (name, c) in named {
        if cx.is_unknown(c) {
            cx.unknown.push((globals, name.to_string()));
        }
        let v = cx.slot(c)?;
        cx.heap.tables.get_mut(&globals).expect("just made").hash.insert(name.to_string(), v);
    }
    for (k, v) in &base.heap.tables[&base.globals].hash {
        if let TV::Builtin(n) = v {
            cx.heap.tables.get_mut(&globals).expect("just made").hash.entry(k.clone()).or_insert(TV::Builtin(n));
        }
    }
    let buttons = match cx.heap.tables[&globals].hash.get(BUTTON_GLOBAL) {
        Some(TV::Table(t)) => Some(*t),
        _ => None,
    };
    let unknown = cx.unknown.into_iter().filter(|(t, _)| Some(*t) != buttons).collect();
    let st = State {
        heap: cx.heap,
        globals,
        scope,
        stack: Vec::new(),
        guard: true,
        ended: false,
        path: Vec::new(),
        frag: Vec::new(),
        arc: None,
    };
    Ok((st, unknown))
}

/// Set the six buttons of a reference state from an input byte (bit 0 left,
/// 1 right, 2 up, 3 down, 4 jump, 5 dash). Returns the button table.
pub fn set_buttons(st: &mut State<RefDomain>, byte: u8) -> Result<TableId> {
    let Some(TV::Table(t)) = st.heap.tables[&st.globals].hash.get(BUTTON_GLOBAL).cloned() else {
        bail!("no {BUTTON_GLOBAL} table")
    };
    let arr = &mut st.heap.tables.get_mut(&t).expect("a live table").arr;
    for (i, b) in arr.iter_mut().enumerate() {
        *b = TV::Bool(byte >> i & 1 == 1);
    }
    Ok(t)
}

struct ToBlock<'a> {
    st: &'a State<RefDomain>,
    rt2: Rt2,
    memo_t: HashMap<TableId, u32>,
    memo_c: HashMap<ClosureId, u32>,
    memo_b: HashMap<&'static str, u32>,
}

impl ToBlock<'_> {
    fn cell(&mut self, cell: Cell2, col: Col) -> u32 {
        self.rt2.structure.push(cell);
        self.rt2.cols.push(col);
        (self.rt2.structure.len() - 1) as u32
    }

    /// `ubool`: under `__button_states`, where a boolean is written `UBool`.
    fn value(&mut self, v: &TV, ubool: bool) -> Result<AV> {
        Ok(match v {
            TV::Nil => AV::Nil,
            TV::Num(iv) => match iv.to_number() {
                Some(n) => AV::Num(n),
                None => AV::Ival(iv.low, iv.high),
            },
            TV::Bool(_) if ubool => AV::UBool,
            TV::Bool(b) => AV::Bool(*b),
            TV::Str(s) => {
                self.rt2.strings.push(s.to_string());
                AV::Str((self.rt2.strings.len() - 1) as u32)
            }
            TV::Table(t) => AV::Ptr(self.table(*t, ubool)?),
            TV::Func(c) => AV::Ptr(self.closure(*c)?),
            TV::Builtin(n) => AV::Ptr(self.builtin(n)?),
        })
    }

    /// A slot holding `v`: its own `Val` cell, except a builtin's first slot,
    /// which is the builtin's cell.
    fn slot(&mut self, v: &TV, ubool: bool) -> Result<u32> {
        if let TV::Builtin(n) = v {
            return match self.memo_b.get(n) {
                Some(&b) => Ok(self.cell(Cell2::Val, Col::U(AV::Ptr(b)))),
                None => self.builtin(n),
            };
        }
        let av = self.value(v, ubool)?;
        Ok(self.cell(Cell2::Val, Col::U(av)))
    }

    fn table(&mut self, t: TableId, ubool: bool) -> Result<u32> {
        if let Some(&c) = self.memo_t.get(&t) {
            return Ok(c);
        }
        let cell = self.cell(Cell2::Unk, Col::U(AV::Nil));
        self.memo_t.insert(t, cell);
        let table = &self.st.heap.tables[&t];
        // Integer keys past the array part become a dense array with explicit
        // nils, as `bind` flattens it for the kernels.
        let arr: Vec<TV> = if table.ints.is_empty() {
            table.arr.to_vec()
        } else {
            ensure!(table.hash.is_empty(), "table #{t} has integer keys and named fields");
            let lo = *table.ints.keys().next().expect("non-empty");
            let top = *table.ints.keys().next_back().expect("non-empty");
            ensure!(lo >= 1, "table #{t} has a non-positive integer key");
            let mut arr = table.arr.to_vec();
            arr.resize(top as usize, TV::Nil);
            for (k, v) in &table.ints {
                arr[*k as usize - 1] = v.clone();
            }
            arr
        };
        ensure!(table.hash.is_empty() || arr.is_empty(), "table #{t} has both named and array parts");
        let shape = if !table.hash.is_empty() {
            let mut fields = Vec::new();
            for (name, v) in &table.hash {
                let b = self.slot(v, ubool || name == BUTTON_GLOBAL)?;
                if let Some(f) = gen::field_id(name) {
                    fields.push((f, b));
                }
            }
            Cell2::Obj(fields)
        } else if !arr.is_empty() {
            let mut items = Vec::with_capacity(arr.len());
            for v in &arr {
                items.push(self.slot(v, ubool)?);
            }
            Cell2::Arr(items)
        } else {
            Cell2::Unk
        };
        self.rt2.structure[cell as usize] = shape;
        Ok(cell)
    }

    fn closure(&mut self, c: ClosureId) -> Result<u32> {
        if let Some(&h) = self.memo_c.get(&c) {
            return Ok(h);
        }
        let cell = self.cell(Cell2::Unk, Col::U(AV::Nil));
        self.memo_c.insert(c, cell);
        let cl = &self.st.heap.closures[&c];
        let f = cl.fn_id.ok_or_else(|| anyhow!("anonymous closure #{c} at a frame boundary"))?;
        let mut caps = Vec::with_capacity(cl.captures.len());
        for name in &cl.captures {
            let v = self.st.heap.lookup(cl.env, name).ok_or_else(|| anyhow!("capture {name:?} of closure #{c} not in its scope"))?;
            caps.push(Col::U(self.value(v, false)?));
        }
        self.rt2.structure[cell as usize] = Cell2::Clo(f, caps.into_boxed_slice());
        Ok(cell)
    }

    fn builtin(&mut self, name: &'static str) -> Result<u32> {
        if let Some(&b) = self.memo_b.get(name) {
            return Ok(b);
        }
        let i = BUILTIN_NAMES.iter().position(|n| *n == name).ok_or_else(|| anyhow!("unknown builtin {name:?}"))?;
        let b = self.cell(Cell2::Bi(i as u32), Col::U(AV::Nil));
        self.memo_b.insert(name, b);
        Ok(b)
    }
}

/// A reference state as a one-lane block in canonical cell order (no row
/// keys: `frame::Block::canonical` makes them canonical; a tree keys them).
pub fn to_block(st: &State<RefDomain>) -> Result<Rt2> {
    let (cart, cache) = crate::compiled::room_context()?;
    let rt2 = Rt2::empty(1, gen::GLOBAL_NAMES.len(), gen::STRINGS, cart, cache);
    let mut cx = ToBlock { st, rt2, memo_t: HashMap::new(), memo_c: HashMap::new(), memo_b: HashMap::new() };
    for (name, v) in &st.heap.tables[&st.globals].hash {
        // The tracer's compile-time hint builtins (`_hint_normalize`, ...).
        if matches!(v, TV::Builtin(n) if !BUILTIN_NAMES.contains(n)) {
            continue;
        }
        let b = cx.slot(v, name == BUTTON_GLOBAL)?;
        if let Some(g) = gen::GLOBAL_NAMES.iter().position(|n| n == name) {
            cx.rt2.globals[g] = b;
        }
    }
    let mut rt2 = cx.rt2;
    rt2.canonicalize_ids();
    // A string's id (part of the key) is its occurrence in canonical order
    // after the static ones.
    let mut strings: Vec<String> = gen::STRINGS.iter().map(|s| s.to_string()).collect();
    let old = std::mem::take(&mut rt2.strings);
    let mut renumber = |col: &mut Col| {
        if let Col::U(AV::Str(s)) = col {
            strings.push(old[*s as usize].clone());
            *s = (strings.len() - 1) as u32;
        }
    };
    for (cell, col) in rt2.structure.iter_mut().zip(rt2.cols.iter_mut()) {
        match cell {
            Cell2::Val => renumber(col),
            Cell2::Clo(_, caps) => caps.iter_mut().for_each(&mut renumber),
            _ => {}
        }
    }
    rt2.strings = strings;
    rt2.shape_hash = rt2.shape_hash_of();
    Ok(rt2)
}
