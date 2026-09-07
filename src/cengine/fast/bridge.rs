//! The fast interpreter's boundary: one lane of an old-interpreter `State`
//! in, one old-interpreter `State` per leaf out.
//!
//! This is `cengine::refbridge` (`to_trace_state` + `patch_closures_for`
//! + `add_missing_builtins`, and `to_interp_state`) with the reference
//! heap cut out of the middle. Everything that bridge documents about the
//! two heap models still holds - the box convention, builtins as a single
//! cell, `__button_states` to `UnknownBool`, an empty table to
//! `UnknownTable` - and the export walks the heap in the SAME order (globals
//! and fields by name), so the old-side heap it builds is the one the
//! reference bridge would build.

use anyhow::{anyhow, bail, Result};
use celeste_core::builtins::BUILTIN_NAMES;
use celeste_core::ids::GlobalId;
use celeste_names as gen;
use rustc_hash::FxHashMap;

use crate::interpreter::heap::HeapId;
use crate::interpreter::state::State as OState;
use crate::interpreter::value::{HeapValue, MaybeVector, Value as OValue};
use crate::pico8_num::Pico8NumInterval as Iv;

use super::heap::{ClosureId, Heap, TableId, Value};
use super::program::{Builtin, Program, Sym};

const BUTTON_GLOBAL: &str = "__button_states";

fn lane_of<T: Clone + std::fmt::Debug + PartialEq + Eq>(mv: &MaybeVector<T>, lane: usize) -> T {
    match mv {
        MaybeVector::Scalar(s) => s.clone(),
        MaybeVector::Vector(v) => v[lane].clone(),
    }
}

/// What the bridge needs to know about the program, computed once.
pub struct Bridge {
    prog: &'static Program,
    /// Every `Sym`, in byte-lexicographic order of its name: the order the
    /// reference exports globals and fields in.
    by_name: Vec<Sym>,
    /// `rank[sym]` = position of `sym` in `by_name`.
    rank: Vec<u32>,
    sym_buttons: Sym,
}

impl Bridge {
    pub fn new(prog: &'static Program) -> Result<Self> {
        let n = prog.interner.len();
        let mut by_name: Vec<Sym> = (0..n as Sym).collect();
        by_name.sort_by(|a, b| prog.interner.name(*a).cmp(prog.interner.name(*b)));
        let mut rank = vec![0u32; n];
        for (i, s) in by_name.iter().enumerate() {
            rank[*s as usize] = i as u32;
        }
        let sym_buttons = prog
            .interner
            .get(BUTTON_GLOBAL)
            .ok_or_else(|| anyhow!("program never names {}", BUTTON_GLOBAL))?;
        Ok(Bridge { prog, by_name, rank, sym_buttons })
    }

    // ------------------------------------------------------------ import

    /// One lane of a boundary state as a fast heap, with every builtin
    /// global of `base` that the state lacks added (the frontend consumes
    /// some builtin calls at compile time, so an old state never names
    /// them, but the interpreter really calls them).
    pub fn import(&self, old: &OState, lane: usize, base: &Heap) -> Result<Heap> {
        assert!(
            old.local_env.iter().count() == 0 && old.outer_local_envs.is_empty(),
            "import expects a boundary state (empty local envs)"
        );
        if lane >= old.vector_size {
            bail!("lane {} out of range (vector_size {})", lane, old.vector_size);
        }
        let mut cx = Import { br: self, old, lane, heap: Heap::default(), memo: FxHashMap::default() };
        for (name, box_id) in old.global_env.iter() {
            let sym = self
                .prog
                .interner
                .get(name)
                .ok_or_else(|| anyhow!("global {:?} is not a name the program knows", name))?;
            let v = cx.conv_slot(*box_id)?;
            cx.heap.set_global(sym, v);
        }
        let mut heap = cx.heap;
        for (i, g) in base.globals.iter().enumerate() {
            if let Some(v @ Value::Builtin(_)) = g {
                if heap.globals.get(i).copied().flatten().is_none() {
                    heap.set_global(i as Sym, *v);
                }
            }
        }
        Ok(heap)
    }

    // ------------------------------------------------------------ export

    /// A leaf heap as a one-lane old-interpreter boundary state.
    pub fn export(&self, heap: &Heap) -> Result<OState> {
        let mut cx = Export {
            br: self,
            heap,
            st: OState::new(),
            memo_t: FxHashMap::default(),
            memo_c: FxHashMap::default(),
            memo_b: FxHashMap::default(),
        };
        cx.st.vector_size = 1;
        for &sym in &self.by_name {
            let Some(Some(v)) = heap.globals.get(sym as usize) else { continue };
            // Builtins outside `BUILTIN_NAMES` are the interpreter's own
            // (compile-time hints in the old world); drop them, the inverse
            // of `import` adding them.
            if let Value::Builtin(b) = v {
                if !BUILTIN_NAMES.contains(&b.name()) {
                    continue;
                }
            }
            let ubool = sym == self.sym_buttons;
            let bid = cx.box_slot(*v, ubool)?;
            cx.st.global_env.insert(self.prog.interner.name(sym).to_string(), bid);
        }
        Ok(cx.st)
    }
}

struct Import<'a> {
    br: &'a Bridge,
    old: &'a OState,
    lane: usize,
    heap: Heap,
    memo: FxHashMap<HeapId, Value>,
}

impl Import<'_> {
    fn conv_slot(&mut self, box_id: HeapId) -> Result<Value> {
        match self.old.heap.get(box_id) {
            HeapValue::Value(v) => self.conv_value(&v.clone()),
            _ => self.conv_target(box_id),
        }
    }

    fn conv_value(&mut self, v: &OValue) -> Result<Value> {
        Ok(match v {
            OValue::Number(mv) => Value::Num(Iv::from_number(lane_of(mv, self.lane))),
            OValue::NumberInterval(mv) => {
                let iv = lane_of(mv, self.lane);
                Value::Num(Iv::new(iv.low, iv.high))
            }
            OValue::Bool(mv) => Value::Bool(lane_of(mv, self.lane)),
            // Overwritten by `__reset_button_states` before any read.
            OValue::UnknownBool => Value::Bool(false),
            OValue::String(s) => Value::Str(
                self.br
                    .prog
                    .interner
                    .get(s)
                    .ok_or_else(|| anyhow!("string {:?} is not one the program knows", s))?,
            ),
            OValue::Nil(_) | OValue::NilPointer(_) => Value::Nil,
            OValue::Pointer(id) => self.conv_target(*id)?,
            OValue::MaybeBool(_) => bail!("MaybeBool must never be stored (value.rs contract)"),
        })
    }

    fn conv_target(&mut self, id: HeapId) -> Result<Value> {
        if let Some(v) = self.memo.get(&id) {
            return Ok(*v);
        }
        match self.old.heap.get(id).clone() {
            HeapValue::ObjectTable(fields) => {
                let t = self.heap.new_table();
                self.memo.insert(id, Value::Table(t));
                let mut names: Vec<(&String, &HeapId)> = fields.iter().collect();
                names.sort();
                for (name, fid) in names {
                    let sym = self
                        .br
                        .prog
                        .interner
                        .get(name)
                        .ok_or_else(|| anyhow!("field {:?} is not a name the program knows", name))?;
                    let cv = self.conv_slot(*fid)?;
                    self.heap.tables[t as usize].hash.push((sym, cv));
                }
                Ok(Value::Table(t))
            }
            HeapValue::ArrayTable(items) => {
                let t = self.heap.new_table();
                self.memo.insert(id, Value::Table(t));
                for it in items {
                    let cv = self.conv_slot(it)?;
                    self.heap.tables[t as usize].arr.push(cv);
                }
                Ok(Value::Table(t))
            }
            HeapValue::Closure(gid, caps) => {
                let fn_id = gen::FN_NAMES
                    .iter()
                    .position(|n| *n == gid.as_str())
                    .ok_or_else(|| anyhow!("closure {:?} is not in FN_NAMES", gid.as_str()))?
                    as u32;
                let func = *self
                    .br
                    .prog
                    .by_fn_id
                    .get(&fn_id)
                    .ok_or_else(|| anyhow!("no function body for fn_id {} ({})", fn_id, gid.as_str()))?;
                let expected = self.br.prog.funcs[func as usize].captures.len();
                if caps.len() != expected {
                    bail!(
                        "{}: boundary closure has {} captures but the function takes {}",
                        gid.as_str(),
                        caps.len(),
                        expected
                    );
                }
                let mut upvals = Vec::with_capacity(caps.len());
                for cv in &caps {
                    upvals.push(self.conv_value(cv)?);
                }
                let c = self.heap.new_closure(func, upvals);
                self.memo.insert(id, Value::Func(c));
                Ok(Value::Func(c))
            }
            HeapValue::BuiltinFun(name) => {
                let b = Builtin::from_name(&name)
                    .ok_or_else(|| anyhow!("builtin {:?} is not one the interpreter implements", name))?;
                let v = Value::Builtin(b);
                self.memo.insert(id, v);
                Ok(v)
            }
            HeapValue::UnknownTable => {
                let t = self.heap.new_table();
                self.memo.insert(id, Value::Table(t));
                Ok(Value::Table(t))
            }
            HeapValue::Value(v) => self.conv_value(&v.clone()),
        }
    }
}

struct Export<'a> {
    br: &'a Bridge,
    heap: &'a Heap,
    st: OState,
    memo_t: FxHashMap<TableId, HeapId>,
    memo_c: FxHashMap<ClosureId, HeapId>,
    memo_b: FxHashMap<Builtin, HeapId>,
}

impl Export<'_> {
    fn conv_value(&mut self, v: Value, ubool: bool) -> Result<OValue> {
        Ok(match v {
            Value::Nil => OValue::Nil(None),
            Value::Num(iv) => match iv.to_number() {
                Some(n) => OValue::Number(MaybeVector::Scalar(n)),
                None => OValue::NumberInterval(MaybeVector::Scalar(iv)),
            },
            Value::Bool(_) | Value::UBool(_) if ubool => OValue::UnknownBool,
            Value::Bool(b) => OValue::Bool(MaybeVector::Scalar(b)),
            Value::UBool(_) => bail!("an undecided boolean escaped `__button_states`"),
            Value::Str(s) => OValue::String(self.br.prog.interner.name(s).to_string()),
            Value::Table(t) => OValue::Pointer(self.conv_table(t, ubool)?),
            Value::Func(c) => OValue::Pointer(self.conv_closure(c)?),
            Value::Builtin(b) => OValue::Pointer(self.conv_builtin(b)),
        })
    }

    /// See `refbridge::ToInterp::box_slot` for the builtin/other split.
    fn box_slot(&mut self, v: Value, ubool: bool) -> Result<HeapId> {
        match v {
            Value::Builtin(b) => {
                if let Some(&h) = self.memo_b.get(&b) {
                    let id = self.st.heap.alloc();
                    self.st.heap.set(id, HeapValue::Value(OValue::Pointer(h)));
                    Ok(id)
                } else {
                    Ok(self.conv_builtin(b))
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
        let table = &self.heap.tables[t as usize];
        let has_hash = !table.hash.is_empty();
        let has_arr = !table.arr.is_empty();
        if !table.ints.is_empty() {
            bail!("export: table #{} has a non-empty integer part - unsupported", t);
        }
        if has_hash && has_arr {
            bail!("export: table #{} has both string and array parts", t);
        }
        let hv = if !has_hash && !has_arr {
            HeapValue::UnknownTable
        } else if has_hash {
            let mut entries: Vec<(Sym, Value)> = table.hash.clone();
            entries.sort_by_key(|(s, _)| self.br.rank[*s as usize]);
            let mut fields = FxHashMap::default();
            for (sym, v) in entries {
                let child_ubool = ubool || sym == self.br.sym_buttons;
                let bid = self.box_slot(v, child_ubool)?;
                fields.insert(self.br.prog.interner.name(sym).to_string(), bid);
            }
            HeapValue::ObjectTable(fields)
        } else {
            let mut out = Vec::with_capacity(table.arr.len());
            for v in &table.arr {
                out.push(self.box_slot(*v, ubool)?);
            }
            HeapValue::ArrayTable(out)
        };
        self.st.heap.set(cell, hv);
        Ok(cell)
    }

    fn conv_closure(&mut self, c: ClosureId) -> Result<HeapId> {
        if let Some(&h) = self.memo_c.get(&c) {
            return Ok(h);
        }
        let cell = self.st.heap.alloc();
        self.memo_c.insert(c, cell);
        let cl = &self.heap.closures[c as usize];
        let f = &self.br.prog.funcs[cl.func as usize];
        let fn_id = f
            .fn_id
            .ok_or_else(|| anyhow!("anonymous closure #{} ({}) cannot be exported", c, f.name))?;
        let gid = GlobalId::from(gen::FN_NAMES[fn_id as usize].to_string());
        let mut caps = Vec::with_capacity(cl.upvals.len());
        for v in &cl.upvals {
            caps.push(self.conv_value(*v, false)?);
        }
        self.st.heap.set(cell, HeapValue::Closure(gid, caps));
        Ok(cell)
    }

    fn conv_builtin(&mut self, b: Builtin) -> HeapId {
        if let Some(&h) = self.memo_b.get(&b) {
            return h;
        }
        let cell = self.st.heap.alloc();
        self.st.heap.set(cell, HeapValue::BuiltinFun(b.name().to_string()));
        self.memo_b.insert(b, cell);
        cell
    }
}

impl Bridge {
    /// A canonical fingerprint of what `export` would produce: the same
    /// walk (globals and fields by name, arrays in order, tables and
    /// closures numbered by first visit) flattened into words. Two heaps
    /// with equal fingerprints export to equal old-interpreter states, so
    /// the driver can drop a leaf whose fingerprint it has already seen
    /// instead of paying the conversion again. The walk hashes STRUCTURE,
    /// so it never merges two leaves `export` would tell apart.
    pub fn fingerprint(&self, heap: &Heap, out: &mut Vec<u64>) {
        out.clear();
        let mut fp = Fingerprint { br: self, heap, out, seen_t: Vec::new(), seen_c: Vec::new(), scratch: Vec::new() };
        for &sym in &self.by_name {
            let Some(Some(v)) = heap.globals.get(sym as usize) else { continue };
            if let Value::Builtin(b) = v {
                if !BUILTIN_NAMES.contains(&b.name()) {
                    continue;
                }
            }
            fp.out.push(sym as u64);
            fp.value(*v, sym == self.sym_buttons);
        }
    }
}

struct Fingerprint<'a> {
    br: &'a Bridge,
    heap: &'a Heap,
    out: &'a mut Vec<u64>,
    /// `seen_t[i]` = the i-th distinct table visited (its canonical number
    /// is `i`). Linear scans: a leaf holds a few dozen tables.
    seen_t: Vec<TableId>,
    seen_c: Vec<ClosureId>,
    scratch: Vec<(Sym, Value)>,
}

impl Fingerprint<'_> {
    fn value(&mut self, v: Value, ubool: bool) {
        match v {
            Value::Nil => self.out.push(0),
            Value::Num(iv) => {
                self.out.push(1);
                self.out.push(((iv.low.as_raw_u32() as u64) << 32) | iv.high.as_raw_u32() as u64);
            }
            Value::Bool(_) | Value::UBool(_) if ubool => self.out.push(2),
            Value::Bool(b) => self.out.push(3 | (b as u64) << 8),
            // Refused by `export`; kept distinct here so the refusal fires.
            Value::UBool(i) => self.out.push(12 | (i as u64) << 8),
            Value::Str(s) => self.out.push(4 | (s as u64) << 8),
            Value::Table(t) => self.table(t, ubool),
            Value::Func(c) => self.closure(c),
            Value::Builtin(b) => {
                let i = BUILTIN_NAMES.iter().position(|n| *n == b.name()).unwrap_or(usize::MAX) as u64;
                self.out.push(6 | i << 8)
            }
        }
    }

    fn table(&mut self, t: TableId, ubool: bool) {
        if let Some(i) = self.seen_t.iter().position(|x| *x == t) {
            self.out.push(7 | (i as u64) << 8);
            return;
        }
        self.seen_t.push(t);
        let heap = self.heap;
        let table = &heap.tables[t as usize];
        self.out.push(8 | (table.hash.len() as u64) << 8 | (table.arr.len() as u64) << 32);
        // The integer part is exported as a bail; fingerprint it so such
        // leaves are at least kept distinct.
        for (k, v) in &table.ints {
            self.out.push(9 | (*k as u16 as u64) << 8);
            self.value(*v, ubool);
        }
        let start = self.scratch.len();
        self.scratch.extend_from_slice(&table.hash);
        self.scratch[start..].sort_by_key(|(s, _)| self.br.rank[*s as usize]);
        for i in start..self.scratch.len() {
            let (sym, v) = self.scratch[i];
            self.out.push(sym as u64);
            self.value(v, ubool || sym == self.br.sym_buttons);
        }
        self.scratch.truncate(start);
        for v in &table.arr {
            self.value(*v, ubool);
        }
    }

    fn closure(&mut self, c: ClosureId) {
        if let Some(i) = self.seen_c.iter().position(|x| *x == c) {
            self.out.push(10 | (i as u64) << 8);
            return;
        }
        self.seen_c.push(c);
        let heap = self.heap;
        let cl = &heap.closures[c as usize];
        self.out.push(11 | (cl.func as u64) << 8 | (cl.upvals.len() as u64) << 40);
        for v in &cl.upvals {
            self.value(*v, false);
        }
    }
}
