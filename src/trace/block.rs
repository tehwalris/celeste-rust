//! An interpreter state and a block: ONE converter each way, for every
//! domain. `to_rt2` makes the engine's block of a state - the tracer's output
//! skeletons (over `Symbolic`) and the reference engine's rows (over
//! `RefDomain`) - so a kernel row and a reference row of one state have one
//! structure and one key; the domain supplies only what a number and a
//! boolean become. `from_block` is the inverse, one lane as a state.
//!
//! A block BOXES every slot: each global, field and array element is a `Val`
//! cell. A table or a closure is ONE cell (every slot holding it points at
//! it, `AV::Ptr`); a builtin is a `Bi` cell AT every slot holding it (it is
//! its name, with nothing to share). The cells are numbered by
//! `Rt2::canonicalize_ids`.
//!
//! What a block keeps of a state: the `GLOBAL_NAMES` globals and the
//! `FIELD_NAMES` fields (`from_block` restores the dropped builtins from its
//! base state); integer keys past an array part as a dense array with
//! explicit nils (a frame's outcomes hold them so, `dense_ints`); a closure's captures, tables only. The buttons are
//! written `UBool`, as the kernels write them; `from_block` reads a `UBool`
//! button as a placeholder `false` (overwritten before it is read) and
//! returns any other unknown boolean for the driver to fork.

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, bail, ensure, Result};
use celeste_core::room::Room;
use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Col, Rt2, AV, NONE};
use celeste_engine::shape::Cell;
use celeste_names as gen;

use crate::builtins::BUILTIN_NAMES;
use crate::trace::domain::Domain;
use crate::trace::heap::{BodyId, ClosureId, Heap, Table, TableId, Value};
use crate::trace::state::State;


/// The block of `st`, one lane: its structure, and its values by `num` and
/// `boolean` (the domain's part). Canonical cell order, its shape hash set,
/// no row keys.
pub fn to_rt2<D: Domain, N: Arithmetic>(
    st: &State<D>,
    room: &'static Room,
    num: impl Fn(&D::Num) -> Result<AV<N>>,
    boolean: impl Fn(&D::Bool) -> Result<AV<N>>,
) -> Result<Rt2<N>> {
    let globals = &st.heap.tables[&st.globals].hash;
    let buttons = match globals.get(super::game::BUTTONS) {
        Some(Value::Table(t)) => Some(*t),
        _ => None,
    };
    let static_strings = gen::STRINGS.iter().map(|s| s.to_string()).collect();
    let mut cx = ToRt2 { st, cells: Vec::new(), cols: Vec::new(), strings: static_strings, tables: HashMap::new(), closures: HashMap::new(), buttons, num, boolean };
    let mut cell_of_global = vec![NONE; gen::GLOBAL_NAMES.len()];
    for (g, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        if let Some(v) = globals.get(*name) {
            cell_of_global[g] = cx.slot(v, false, None)?;
        }
    }
    let (old, statics) = (cx.strings, gen::STRINGS.len());
    let mut rt2 = Rt2::canonical_of(1, cx.cells, cx.cols, cell_of_global, Arc::from(&old[..statics]), room);
    // A string's id (part of the key) is its occurrence in canonical order
    // after the static ones.
    let mut strings: Vec<String> = old[..statics].to_vec();
    for (cell, col) in rt2.shape.cells().iter().zip(rt2.cols.iter_mut()) {
        if let (Cell::Val, Col::U(AV::Str(s))) = (cell, col) {
            strings.push(old[*s as usize].clone());
            *s = (strings.len() - 1) as u32;
        }
    }
    rt2.strings = strings.into();
    the_topology_is_the_shapes(&rt2);
    Ok(rt2)
}

/// A shape's POINTER TOPOLOGY in a block: each value cell's target
/// (`NONE`: no pointer), as `(cell, target)` in cell order.
type Topology = Box<[(u32, u32)]>;

/// THE POINTER TOPOLOGY IS THE SHAPE'S, process-wide: every block made
/// under one shape has one `Topology` (its captures' targets are its
/// shape's: `Shape::intern`). The signature names no value cell's target,
/// and a kernel is bound by path against one representative of its shape
/// (`trace::bind`): a block whose pointers differ under the hash would run
/// a kernel bound to the other's cells. FATAL (`exact`'s
/// `pointer_violation` is the key space's side).
fn the_topology_is_the_shapes<N: Arithmetic>(rt2: &Rt2<N>) {
    type Seen = celeste_engine::FxHashMap<u64, Arc<Topology>>;
    static SEEN: std::sync::OnceLock<std::sync::RwLock<Seen>> = std::sync::OnceLock::new();
    let topology: Vec<(u32, u32)> = rt2
        .cells()
        .iter()
        .zip(&rt2.cols)
        .enumerate()
        .filter(|(_, (cell, _))| **cell == Cell::Val)
        .map(|(c, (_, col))| (c as u32, if let Col::U(AV::Ptr(t)) = col { *t } else { NONE }))
        .collect();
    let seen = SEEN.get_or_init(Default::default);
    let hash = rt2.shape_hash();
    let had = seen.read().expect("the shapes' topologies").get(&hash).cloned();
    let had = had.unwrap_or_else(|| seen.write().expect("the shapes' topologies").entry(hash).or_insert_with(|| Arc::new(topology.clone().into_boxed_slice())).clone());
    if let Some((a, b)) = had.iter().zip(&topology).find(|(a, b)| a != b) {
        panic!(
            "block: shape {:#x}: cell {} points at {} where an earlier block of the shape's cell {} points at {} (u32::MAX: no pointer): \
             the POINTER TOPOLOGY, which a kernel binds by path against one representative, varies under one shape hash",
            hash, b.0, b.1, a.0, a.1
        );
    }
    assert!(had.len() == topology.len(), "block: shape {hash:#x}: {} pointers where an earlier block of the shape has {}", topology.len(), had.len());
}

/// The tracer's block of `st`: its shape, every number and boolean `Nil`
/// (the kernels compute them). What a traced frame's input and outcomes bind
/// against.
pub fn skeleton<D: Domain, N: Arithmetic>(st: &State<D>, room: &'static Room) -> Result<Rt2<N>> {
    to_rt2(st, room, |_| Ok(AV::Nil), |_| Ok(AV::Nil))
}

/// A table's array part as a block holds it: integer keys past it
/// (`got_fruit[1 + level_index()]` on an empty table) at their places in a
/// dense array with explicit nils.
fn dense<D: Domain>(t: TableId, table: &Table<D>) -> Result<Vec<Value<D>>> {
    if table.ints.is_empty() {
        return Ok(table.arr.to_vec());
    }
    ensure!(table.hash.is_empty(), "table #{t} has integer keys and named fields");
    let lo = *table.ints.keys().next().expect("non-empty");
    let top = *table.ints.keys().next_back().expect("non-empty");
    ensure!(lo >= 1, "table #{t} has a non-positive integer key: it has no place in a dense array part");
    let mut arr = table.arr.to_vec();
    arr.resize(top as usize, Value::Nil);
    for (k, v) in &table.ints {
        arr[*k as usize - 1] = v.clone();
    }
    Ok(arr)
}

/// Every table of `st` with integer keys past its array part made the dense
/// array a block holds (`dense`): a frame's outcome, so a state traced from
/// its block (`from_block`) names every slot as the state itself does.
pub fn dense_ints<D: Domain>(st: &mut State<D>) -> Result<()> {
    for (t, table) in st.heap.tables.iter_mut() {
        if !table.ints.is_empty() {
            table.arr = dense(*t, table)?;
            table.ints.clear();
        }
    }
    Ok(())
}

struct ToRt2<'a, D: Domain, E: Arithmetic, N, B> {
    st: &'a State<D>,
    /// The block's cells and columns as made (`Rt2::canonical_of` numbers
    /// them), the static strings then the state's.
    cells: Vec<Cell>,
    cols: Vec<Col<E>>,
    strings: Vec<String>,
    tables: HashMap<TableId, u32>,
    /// Each closure's cell and the table holding it (`closure`).
    closures: HashMap<ClosureId, (u32, Option<TableId>)>,
    /// The table `__button_states` holds: its booleans are written `UBool`.
    buttons: Option<TableId>,
    num: N,
    boolean: B,
}

impl<D: Domain, E: Arithmetic, N, B> ToRt2<'_, D, E, N, B>
where
    N: Fn(&D::Num) -> Result<AV<E>>,
    B: Fn(&D::Bool) -> Result<AV<E>>,
{
    fn cell(&mut self, cell: Cell, col: Col<E>) -> u32 {
        self.cells.push(cell);
        self.cols.push(col);
        (self.cells.len() - 1) as u32
    }

    /// A slot holding `v`: its own `Val` cell, or a builtin's `Bi` cell.
    /// `button`: a slot of the button table; `holder`: the table whose slot
    /// it is (`None`: a global).
    fn slot(&mut self, v: &Value<D>, button: bool, holder: Option<TableId>) -> Result<u32> {
        if let Value::Builtin(name) = v {
            let i = BUILTIN_NAMES.iter().position(|n| n == name).ok_or_else(|| anyhow!("unknown builtin {name:?}"))?;
            return Ok(self.cell(Cell::Bi(i as u32), Col::U(AV::Nil)));
        }
        let av = match v {
            Value::Nil => AV::Nil,
            Value::Num(n) => (self.num)(n)?,
            Value::Bool(_) if button => AV::UBool,
            Value::Bool(b) => (self.boolean)(b)?,
            Value::Str(s) => {
                self.strings.push(s.to_string());
                AV::Str((self.strings.len() - 1) as u32)
            }
            Value::Table(t) => AV::Ptr(self.table(*t)?),
            Value::Func(c) => AV::Ptr(self.closure(*c, holder)?),
            Value::Builtin(_) => unreachable!("a builtin is its own cell"),
        };
        Ok(self.cell(Cell::Val, Col::U(av)))
    }

    fn table(&mut self, t: TableId) -> Result<u32> {
        if let Some(&c) = self.tables.get(&t) {
            return Ok(c);
        }
        let cell = self.cell(Cell::Unk, Col::U(AV::Nil));
        self.tables.insert(t, cell);
        let st = self.st;
        let table = &st.heap.tables[&t];
        // A table is an object OR an array to the engine, never both.
        let arr = dense(t, table)?;
        ensure!(table.hash.is_empty() || arr.is_empty(), "table #{t} has both named and array parts");
        let button = Some(t) == self.buttons;
        // An EMPTY table has no kind yet: `Unk`, not an object with no fields
        // (a different cell to the shape hash).
        let shape = if !table.hash.is_empty() {
            let mut fields = Vec::new();
            for (name, v) in &table.hash {
                if let Some(f) = gen::field_id(name) {
                    fields.push((f, self.slot(v, button, Some(t))?));
                }
            }
            Cell::Obj(fields.into_boxed_slice())
        } else if !arr.is_empty() {
            let mut items = Vec::with_capacity(arr.len());
            for v in &arr {
                items.push(self.slot(v, button, Some(t))?);
            }
            Cell::Arr(items.into_boxed_slice())
        } else {
            Cell::Unk
        };
        self.cells[cell as usize] = shape;
        Ok(cell)
    }

    /// Closure `c`'s cell, reached through a slot of `holder`. Its captures'
    /// targets are the cell's (its shape's), but a state's EXACT identity
    /// reads none of them (`exact::exact_rows` reads `Val` cells; the
    /// signature counts them): the block is exact only because every
    /// capture IS the closure's OWNER, the one table holding it (`obj.move`
    /// captures `obj`), which the block's structure already says. Any other
    /// capture - another table, a closure held by two tables or by a global
    /// - is refused, naming the closure (plans/correctness.md C6).
    fn closure(&mut self, c: ClosureId, holder: Option<TableId>) -> Result<u32> {
        let st = self.st;
        let cl = &st.heap.closures[&c];
        // The engine names a closure by `FN_NAMES` index (`Interp::fn_id_of`);
        // the cart's anonymous functions never outlive their frame.
        let f = cl.fn_id.ok_or_else(|| anyhow!("anonymous closure #{c} at a frame boundary"))?;
        let name = gen::FN_NAMES[f as usize];
        if let Some(&(h, owner)) = self.closures.get(&c) {
            ensure!(cl.captures.is_empty() || owner == holder, "closure {name} (#{c}) captures {:?} and is held by two tables, #{owner:?} and #{holder:?}: a capture is identity no key reads (plans/correctness.md C6)", cl.captures);
            return Ok(h);
        }
        let cell = self.cell(Cell::Unk, Col::U(AV::Nil));
        self.closures.insert(c, (cell, holder));
        let mut caps = Vec::with_capacity(cl.captures.len());
        for cap in &cl.captures {
            match st.heap.lookup(cl.env, cap) {
                Some(Value::Table(t)) if Some(*t) == holder => caps.push(self.table(*t)?),
                Some(Value::Table(t)) => bail!("closure {name} (#{c}): its capture {cap:?} is table #{t}, not its owner #{holder:?} (the table holding it): a capture is identity no key reads (plans/correctness.md C6)"),
                other => bail!("closure {name} (#{c}): its capture {cap:?} holds {other:?}, not a table"),
            }
        }
        self.cells[cell as usize] = Cell::Clo { f, caps: caps.into_boxed_slice() };
        Ok(cell)
    }
}

/// Per function (`gen::FN_NAMES` index): the body the interpreter registered
/// and the names its closures capture, which a block does not record.
pub type FnInfo = HashMap<u32, (BodyId, Vec<String>)>;

/// `FnInfo` off a state the interpreter built (the cart toplevel + `_init`).
/// Every closure of one function agrees on both; a disagreement is a bug.
pub fn fn_info_of<D: Domain>(base: &State<D>) -> Result<FnInfo> {
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

/// Lane `lane` of a block as a state in domain `d` (plus the builtins of
/// `base`'s globals, which a block drops), and its unknown-boolean fields
/// except the buttons, for the driver to fork.
pub fn from_block<N: Arithmetic, D: Domain<A = N>, E: Domain>(
    d: &mut D,
    rt2: &Rt2<N>,
    lane: usize,
    fn_info: &FnInfo,
    base: &State<E>,
) -> Result<(State<D>, Vec<(TableId, String)>)> {
    ensure!(lane < rt2.width, "lane {lane} of a {}-lane block", rt2.width);
    let mut cx = FromBlock { d, rt2, lane, fn_info, heap: Heap::default(), memo: HashMap::new(), unknown: Vec::new() };
    let globals = cx.heap.new_table();
    let scope = cx.heap.new_scope(None);
    let mut named: Vec<(&str, u32)> =
        gen::GLOBAL_NAMES.iter().zip(rt2.shape.globals()).filter(|(_, &c)| c != NONE).map(|(n, &c)| (*n, c)).collect();
    named.sort();
    for (name, c) in named {
        if cx.is_unknown(c) {
            cx.unknown.push((globals, name.to_string()));
        }
        let v = cx.slot(c)?;
        cx.heap.tables.get_mut(&globals).expect("just made").hash.insert(name.to_string(), v);
    }
    for (k, v) in &base.heap.tables[&base.globals].hash {
        if let Value::Builtin(n) = v {
            cx.heap.tables.get_mut(&globals).expect("just made").hash.entry(k.clone()).or_insert(Value::Builtin(n));
        }
    }
    let buttons = match cx.heap.tables[&globals].hash.get(super::game::BUTTONS) {
        Some(Value::Table(t)) => Some(*t),
        _ => None,
    };
    let unknown = cx.unknown.into_iter().filter(|(t, _)| Some(*t) != buttons).collect();
    let (guard, ended) = (cx.d.boolean(true), cx.d.boolean(false));
    let st = State {
        heap: cx.heap,
        globals,
        scope,
        stack: Vec::new(),
        guard,
        ended,
        path: Vec::new(),
        frag: Vec::new(),
        arc: None,
    };
    Ok((st, unknown))
}

struct FromBlock<'a, N: Arithmetic, D: Domain> {
    d: &'a mut D,
    rt2: &'a Rt2<N>,
    lane: usize,
    fn_info: &'a FnInfo,
    heap: Heap<D>,
    memo: HashMap<u32, Value<D>>,
    unknown: Vec<(TableId, String)>,
}

impl<N: Arithmetic, D: Domain<A = N>> FromBlock<'_, N, D> {
    fn is_unknown(&self, cell: u32) -> bool {
        matches!(self.rt2.cells()[cell as usize], Cell::Val) && self.rt2.cols[cell as usize].at(self.lane) == AV::UBool
    }

    fn slot(&mut self, cell: u32) -> Result<Value<D>> {
        match self.rt2.cells()[cell as usize] {
            Cell::Val => self.value(self.rt2.cols[cell as usize].at(self.lane)),
            _ => self.target(cell),
        }
    }

    fn value(&mut self, v: AV<N>) -> Result<Value<D>> {
        Ok(match v {
            AV::Num(n) => Value::Num(self.d.num(n)),
            AV::Ival(lo, hi) => Value::Num(self.d.range_num(lo, hi)?),
            // The whole range; a straddling comparison forks.
            AV::UNum => {
                let (lo, hi) = N::bounds(N::unknown());
                Value::Num(self.d.range_num(lo, hi)?)
            }
            AV::Bool(b) => Value::Bool(self.d.boolean(b)),
            AV::UBool => Value::Bool(self.d.boolean(false)),
            AV::Str(s) => Value::Str(Arc::from(self.rt2.strings[s as usize].as_str())),
            AV::Nil => Value::Nil,
            AV::Ptr(p) => self.target(p)?,
        })
    }

    fn target(&mut self, cell: u32) -> Result<Value<D>> {
        if let Some(v) = self.memo.get(&cell) {
            return Ok(v.clone());
        }
        let rt2 = self.rt2;
        Ok(match &rt2.cells()[cell as usize] {
            Cell::Obj(fields) => {
                let t = self.heap.new_table();
                self.memo.insert(cell, Value::Table(t));
                let mut named: Vec<(&str, u32)> = fields.iter().map(|&(f, c)| (gen::FIELD_NAMES[f as usize], c)).collect();
                named.sort();
                for (name, c) in named {
                    if self.is_unknown(c) {
                        self.unknown.push((t, name.to_string()));
                    }
                    let v = self.slot(c)?;
                    self.heap.tables.get_mut(&t).expect("just made").hash.insert(name.to_string(), v);
                }
                Value::Table(t)
            }
            Cell::Arr(items) => {
                let t = self.heap.new_table();
                self.memo.insert(cell, Value::Table(t));
                for &c in items.iter() {
                    let v = self.slot(c)?;
                    self.heap.tables.get_mut(&t).expect("just made").arr.push(v);
                }
                Value::Table(t)
            }
            // A `{}` that never had a field stored.
            Cell::Unk => {
                let t = self.heap.new_table();
                self.memo.insert(cell, Value::Table(t));
                Value::Table(t)
            }
            Cell::Clo { f, caps } => {
                let (body, names) = self.fn_info.get(f).ok_or_else(|| anyhow!("no registered body for {}", gen::FN_NAMES[*f as usize]))?;
                ensure!(names.len() == caps.len(), "{}: {} captures in the block, {} registered", gen::FN_NAMES[*f as usize], caps.len(), names.len());
                let env = self.heap.new_scope(None);
                for (name, &cap) in names.iter().zip(caps.iter()) {
                    let v = self.target(cap)?;
                    self.heap.scopes.get_mut(&env).expect("just made").vars.insert(name.clone(), v);
                }
                let c = self.heap.new_closure(*body, env, Some(*f), names.clone());
                self.memo.insert(cell, Value::Func(c));
                Value::Func(c)
            }
            Cell::Bi(b) => {
                let v = Value::Builtin(BUILTIN_NAMES[*b as usize]);
                self.memo.insert(cell, v.clone());
                v
            }
            Cell::Val => self.value(rt2.cols[cell as usize].at(self.lane))?,
        })
    }
}

#[cfg(test)]
mod tests {
    use celeste_core::arith::Fixed;
    /// THE SIGNATURE READS BACK ITS BLOCK: on the start blocks of rooms of
    /// every kind of object (the platforms', a 100% room's), `sig::kinds`
    /// of what `sig::write` writes is the block's cells' kinds, and every
    /// kind occurs (a reader whose cases the blocks skip is unchecked).
    #[test]
    fn the_signature_reads_back_the_blocks_kinds() {
        use crate::settings::{Category, Settings};
        use celeste_engine::shape::{sig, Kind};
        let mut seen = std::collections::BTreeSet::new();
        for s in [
            Settings::default(),
            Settings { room: (6, 0), ..Default::default() },
            Settings { room: (4, 2), ..Default::default() },
            Settings { room: (6, 2), category: Category::Hundred, loading_jank: Some(2), ..Default::default() },
        ] {
            let eng = crate::engines::reference::ReferenceEngine::<Fixed>::new(&s).expect("ref engine");
            let b = eng.initial().expect("the start converts");
            let want: Vec<Kind> = b.cells().iter().map(|c| c.kind()).collect();
            assert_eq!(sig::kinds(b.shape.sig()).as_deref(), Ok(&want[..]), "room {:?}", s.room);
            seen.extend(want.iter().map(|k| format!("{k:?}")));
        }
        assert_eq!(seen.len(), 6, "the kinds the start blocks hold: {seen:?}");
    }

    /// THE POINTER TOPOLOGY IS THE SHAPE'S: room (1,0)'s start with its
    /// `player_spawn`'s `type` pointed at the `player` type table has the
    /// start's signature and hash (the signature names a value cell, not
    /// its target), so its block is FATAL once the start's is made.
    #[test]
    fn a_block_whose_pointers_differ_under_its_shape_hash_is_fatal() {
        use crate::trace::heap::Value;
        let eng = crate::engines::reference::ReferenceEngine::<Fixed>::new(&Default::default()).expect("ref engine");
        let start = eng.start_state();
        let b = crate::engines::reference::to_block(start, eng.room()).expect("the start converts");
        let mut st = start.clone();
        let global = |name: &str| match st.heap.tables[&st.globals].hash.get(name) {
            Some(Value::Table(t)) => *t,
            other => panic!("the global {name} holds {other:?}"),
        };
        let (objects, spawn, player) = (global("objects"), global("player_spawn"), global("player"));
        let items: Vec<_> = st.heap.tables[&objects].arr.iter().filter_map(|v| if let Value::Table(t) = v { Some(*t) } else { None }).collect();
        let obj = *items.iter().find(|o| st.heap.tables[o].hash.get("type") == Some(&Value::Table(spawn))).expect("the start holds a player_spawn");
        st.heap.tables.get_mut(&obj).expect("the spawn").hash.insert("type".into(), Value::Table(player));
        let Err(e) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| crate::engines::reference::to_block(&st, eng.room()))) else { panic!("a retargeted pointer converted") };
        let msg = e.downcast_ref::<String>().map_or("(not a formatted panic)", |m| m.as_str()).to_string();
        assert!(msg.contains("POINTER TOPOLOGY") && msg.contains(&format!("{:#x}", b.shape_hash())), "{msg}");
    }

    /// A CLOSURE'S CAPTURE IS ITS OWNER, or the block refuses it, naming the
    /// closure (plans/correctness.md C6): room (1,0)'s start converts; with
    /// every captured name rebound to the globals table, the conversion
    /// fails on the first closure it meets.
    #[test]
    fn a_closure_capturing_another_table_than_its_owner_is_refused() {
        use crate::trace::heap::Value;
        let s = crate::settings::Settings::default();
        let eng = crate::engines::reference::ReferenceEngine::<Fixed>::new(&s).expect("ref engine");
        let start = eng.start_state();
        crate::engines::reference::to_block(start, eng.room()).expect("the start converts");
        let mut st = start.clone();
        let rebind: Vec<(crate::trace::heap::ScopeId, String)> = st.heap.closures.values().flat_map(|cl| cl.captures.iter().map(move |c| (cl.env, c.clone()))).collect();
        assert!(!rebind.is_empty(), "the start's objects hold closures with captures");
        let globals = st.globals;
        for (env, cap) in &rebind {
            assert!(st.heap.assign(*env, cap, Value::Table(globals)), "the capture's binding");
        }
        let Err(e) = crate::engines::reference::to_block(&st, eng.room()) else { panic!("a capture that is not the owner converted") };
        let e = e.to_string();
        assert!(e.starts_with("closure ") && e.contains("not its owner"), "{e}");
    }
}
