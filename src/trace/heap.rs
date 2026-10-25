//! The CONCRETE heap the tracer keeps while the values go symbolic: tables,
//! fields, lengths, closure identity and scope structure are facts at trace
//! time; only game DATA is symbolic, and nothing indexes the heap with it
//! (where something does, the tracer REFUSES: `domain::refuse_unknown`).
//! Every lexical scope is a heap object and a closure captures it by id, so
//! writing through an upvalue needs no capture analysis.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use anyhow::Result;

use super::domain::Domain;

pub type TableId = u32;
pub type ScopeId = u32;
pub type ClosureId = u32;
/// Index into the interpreter's list of function bodies.
pub type BodyId = u32;

#[derive(Clone)]
pub enum Value<D: Domain> {
    Nil,
    Num(D::Num),
    Bool(D::Bool),
    Str(Arc<str>),
    Table(TableId),
    /// A closure: a HEAP OBJECT with identity, referred to by id. PICO-8 (Lua
    /// 5.2, measured) CACHES closures: identity is (prototype, upvalue cells),
    /// so two calls of a body with no upvalues return the SAME closure. This
    /// model approximates the cells by the enclosing scope, so `==` on two
    /// closures is refused rather than answered.
    Func(ClosureId),
    Builtin(&'static str),
}

impl<D: Domain> PartialEq for Value<D> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Table(a), Value::Table(b)) => a == b,
            // Reference equality, which is Lua's.
            (Value::Func(a), Value::Func(b)) => a == b,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl<D: Domain> std::fmt::Debug for Value<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            // Tag the kind: domains render a Num and a Bool alike.
            Value::Num(n) => write!(f, "num({:?})", n),
            Value::Bool(b) => write!(f, "bool({:?})", b),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Table(t) => write!(f, "table#{}", t),
            Value::Func(c) => write!(f, "closure#{}", c),
            Value::Builtin(n) => write!(f, "builtin:{}", n),
        }
    }
}

/// A Lua table: a string part, an ARRAY part and an integer part, the split
/// Lua itself has. The integer part must exist because `#` depends on it: on
/// a real PICO-8 (`lua/probe/tables.lua`), `t={} t[3]="c"` has `#t == 0`,
/// and `t[1]="a" t[2]="b"` then gives 3 once the array part absorbs the gap.
#[derive(Clone, Default)]
pub struct Table<D: Domain> {
    pub hash: BTreeMap<String, Value<D>>,
    /// The array part. May contain explicit `Nil`s (Lua does not shrink it).
    pub arr: Vec<Value<D>>,
    /// Integer keys OUTSIDE the array part, absorbed into `arr` when an append
    /// closes the gap.
    pub ints: BTreeMap<i16, Value<D>>,
}

impl<D: Domain> Table<D> {
    /// Every value the table holds: the ONE place that knows a table has
    /// three parts (`gc` and `shape` go through it).
    pub fn values(&self) -> impl Iterator<Item = &Value<D>> {
        self.hash.values().chain(self.arr.iter()).chain(self.ints.values())
    }

    /// `#t`, when this model can answer it exactly; `None` means raise. Lua's
    /// `luaH_getn` depends on the array part's CAPACITY (the table's history),
    /// which this model does not track; the answer is exact when the integer
    /// part is empty and the array part has no INTERIOR hole. Trailing holes are
    /// fine (`__array_table_drop_last` leaves them, and `del` takes `#` next).
    pub fn len(&self) -> Option<usize> {
        if !self.ints.is_empty() {
            return None;
        }
        let last = self
            .arr
            .iter()
            .rposition(|v| !matches!(v, Value::Nil))
            .map_or(0, |i| i + 1);
        if self.arr[..last].iter().any(|v| matches!(v, Value::Nil)) {
            return None;
        }
        Some(last)
    }

    /// `name = v` for a GLOBAL, Lua's way: assigning nil removes the key (an
    /// explicit `Nil` global is a structural slot: a cleared global would be a
    /// different SHAPE from a never-set one). Globals only: an object FIELD
    /// assigned nil keeps its slot (the cart does it to every object of a type
    /// alike); removing it would move every key for no merge.
    pub fn set_global(&mut self, name: String, v: Value<D>) {
        if matches!(v, Value::Nil) {
            self.hash.remove(&name);
        } else {
            self.hash.insert(name, v);
        }
    }

    pub fn get_index(&self, i: i16) -> Option<&Value<D>> {
        if i >= 1 && (i as usize) <= self.arr.len() {
            Some(&self.arr[i as usize - 1])
        } else {
            self.ints.get(&i)
        }
    }

    pub fn set_index(&mut self, i: i16, v: Value<D>) {
        if i >= 1 && (i as usize) <= self.arr.len() {
            self.arr[i as usize - 1] = v;
            return;
        }
        if i >= 1 && i as usize == self.arr.len() + 1 {
            self.arr.push(v);
            // The array part absorbs the integer keys right after it.
            while let Ok(k) = i16::try_from(self.arr.len() + 1) {
                match self.ints.remove(&k) {
                    Some(next) => self.arr.push(next),
                    None => break,
                }
            }
            return;
        }
        // Assigning nil to an absent key is not a key.
        if matches!(v, Value::Nil) {
            self.ints.remove(&i);
        } else {
            self.ints.insert(i, v);
        }
    }
}

#[derive(Clone)]
pub struct Scope<D: Domain> {
    pub vars: BTreeMap<String, Value<D>>,
    pub parent: Option<ScopeId>,
}

impl<D: Domain> PartialEq for Table<D> {
    fn eq(&self, o: &Self) -> bool {
        self.hash == o.hash && self.arr == o.arr && self.ints == o.ints
    }
}

impl<D: Domain> PartialEq for Scope<D> {
    fn eq(&self, o: &Self) -> bool {
        self.parent == o.parent && self.vars == o.vars
    }
}

impl<D: Domain> Heap<D> {
    /// Every object alike BY ID: two heaps of one fork that neither side has
    /// written to since (`state::same_heap`).
    pub fn same_as(&self, o: &Self) -> bool {
        self.tables == o.tables && self.scopes == o.scopes && self.closures == o.closures
    }
}

/// A closure object: its function body and captured scope, both immutable.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Closure {
    pub body: BodyId,
    pub env: ScopeId,
    /// This function's index in `celeste_names::gen::FN_NAMES` (what
    /// `shape::Cell::Clo` carries), resolved AT CREATION, where the naming declaration
    /// is in hand. `None`: an anonymous function (the cart's `foreach`
    /// callbacks), refused in a block.
    pub fn_id: Option<u32>,
    /// The names this closure CAPTURES: the free variables of its body that
    /// resolve to a local of the defining scope (names, not values: the state
    /// holds what a capture holds).
    pub captures: Vec<String>,
}

#[derive(Clone, Default)]
pub struct Heap<D: Domain> {
    pub tables: BTreeMap<TableId, Table<D>>,
    pub scopes: BTreeMap<ScopeId, Scope<D>>,
    pub closures: BTreeMap<ClosureId, Closure>,
    next: u32,
}

impl<D: Domain> Heap<D> {
    fn fresh(&mut self) -> u32 {
        let id = self.next;
        self.next += 1;
        id
    }

    pub fn new_table(&mut self) -> TableId {
        let id = self.fresh();
        self.tables.insert(id, Table::default());
        id
    }

    pub fn new_closure(
        &mut self,
        body: BodyId,
        env: ScopeId,
        fn_id: Option<u32>,
        captures: Vec<String>,
    ) -> ClosureId {
        let id = self.fresh();
        self.closures.insert(id, Closure { body, env, fn_id, captures });
        id
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = self.fresh();
        self.scopes.insert(id, Scope { vars: BTreeMap::new(), parent });
        id
    }

    pub fn lookup(&self, scope: ScopeId, name: &str) -> Option<&Value<D>> {
        let mut cur = Some(scope);
        while let Some(s) = cur {
            let sc = self.scopes.get(&s)?;
            if let Some(v) = sc.vars.get(name) {
                return Some(v);
            }
            cur = sc.parent;
        }
        None
    }

    /// Assign to an existing binding wherever the chain holds it; returns
    /// false if there is none, which the caller turns into a global.
    pub fn assign(&mut self, scope: ScopeId, name: &str, v: Value<D>) -> bool {
        let mut cur = Some(scope);
        while let Some(s) = cur {
            let Some(sc) = self.scopes.get_mut(&s) else { return false };
            if sc.vars.contains_key(name) {
                sc.vars.insert(name.to_string(), v);
                return true;
            }
            cur = sc.parent;
        }
        false
    }

    pub fn declare(&mut self, scope: ScopeId, name: &str, v: Value<D>) {
        if let Some(sc) = self.scopes.get_mut(&scope) {
            sc.vars.insert(name.to_string(), v);
        }
    }

    /// Drop everything unreachable from `roots`: two states that did the same
    /// thing by different routes leave different garbage, and merging
    /// compares SHAPES.
    pub fn gc(&mut self, roots: &[Root]) {
        let (live_t, live_s, live_c) = self.reachable(roots);
        self.tables.retain(|k, _| live_t.contains(k));
        self.scopes.retain(|k, _| live_s.contains(k));
        self.closures.retain(|k, _| live_c.contains(k));
    }

    pub fn reachable(&self, roots: &[Root]) -> (BTreeSet<TableId>, BTreeSet<ScopeId>, BTreeSet<ClosureId>) {
        let mut live: (BTreeSet<TableId>, BTreeSet<ScopeId>, BTreeSet<ClosureId>) = Default::default();
        self.visit(roots, |r| match r {
            Root::Table(t) => live.0.insert(t),
            Root::Scope(s) => live.1.insert(s),
            Root::Closure(c) => live.2.insert(c),
        });
        live
    }

    /// Every object reachable from `roots`, BREADTH-FIRST (the canonical
    /// order `shape_and_order` numbers objects by): `first` is told of each
    /// and says whether it is new (an object seen before is not entered).
    /// The ONE place that knows what an object refers to (with `push_value`).
    fn visit(&self, roots: &[Root], mut first: impl FnMut(Root) -> bool) {
        let mut queue: Vec<Root> = roots.to_vec();
        let mut i = 0;
        while i < queue.len() {
            let r = queue[i];
            i += 1;
            if !first(r) {
                continue;
            }
            match r {
                Root::Closure(c) => queue.extend(self.closures.get(&c).map(|cl| Root::Scope(cl.env))),
                Root::Table(t) => {
                    for v in self.tables.get(&t).into_iter().flat_map(|tab| tab.values()) {
                        push_value(v, &mut queue);
                    }
                }
                Root::Scope(s) => {
                    if let Some(sc) = self.scopes.get(&s) {
                        for v in sc.vars.values() {
                            push_value(v, &mut queue);
                        }
                        queue.extend(sc.parent.map(Root::Scope));
                    }
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Root {
    Closure(ClosureId),
    Table(TableId),
    Scope(ScopeId),
}

/// The objects a value refers to: the ONE place that knows which value kinds
/// are references, so `gc` and `shape_and_order` cannot disagree.
pub fn push_value<D: Domain>(v: &Value<D>, stack: &mut Vec<Root>) {
    match v {
        Value::Table(t) => stack.push(Root::Table(*t)),
        Value::Func(c) => stack.push(Root::Closure(*c)),
        _ => {}
    }
}

/// The part of a state that decides whether two states can MERGE: structure,
/// not data (which objects exist, their keys, array lengths and closure
/// bodies).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Slot {
    Nil,
    Num,
    Bool,
    Str(String),
    Table(u32),
    /// The CANONICAL closure number, as `Table`.
    Func(u32),
    Builtin(&'static str),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Shape {
    /// Canonical (BFS-numbered) tables: keys and slot kinds of all THREE
    /// parts (string, array, integer).
    pub tables: Vec<(Vec<(String, Slot)>, Vec<Slot>, Vec<(i16, Slot)>)>,
    pub scopes: Vec<(Vec<(String, Slot)>, Option<u32>)>,
    /// Canonical (BFS-numbered) closures: their body, and the canonical
    /// number of the scope they captured.
    pub closures: Vec<(BodyId, u32)>,
}

impl<D: Domain> Heap<D> {
    /// Canonicalize from `roots` in BFS order, so two heaps that allocated the
    /// same structure in a different ORDER compare equal.
    pub fn shape(&self, roots: &[Root]) -> Result<Shape> {
        Ok(self.shape_and_order(roots)?.0)
    }

    /// `shape`, and the canonical BFS order of the reachable objects it was
    /// numbered by - the order a merge pairs the two sides' objects in.
    pub fn shape_and_order(&self, roots: &[Root]) -> Result<(Shape, Vec<Root>)> {
        let mut t_num: BTreeMap<TableId, u32> = BTreeMap::new();
        let mut s_num: BTreeMap<ScopeId, u32> = BTreeMap::new();
        let mut c_num: BTreeMap<ClosureId, u32> = BTreeMap::new();
        let mut order: Vec<Root> = Vec::new();
        self.visit(roots, |r| {
            // The next number, on first sight.
            let number = |m: &mut BTreeMap<u32, u32>, id: u32| {
                let k = m.len() as u32;
                match m.entry(id) {
                    std::collections::btree_map::Entry::Vacant(e) => {
                        e.insert(k);
                        true
                    }
                    std::collections::btree_map::Entry::Occupied(_) => false,
                }
            };
            let new = match r {
                Root::Table(t) => number(&mut t_num, t),
                Root::Scope(s) => number(&mut s_num, s),
                Root::Closure(c) => number(&mut c_num, c),
            };
            if new {
                order.push(r);
            }
            new
        });
        let slot = |v: &Value<D>| -> Slot {
            match v {
                Value::Nil => Slot::Nil,
                Value::Num(_) => Slot::Num,
                Value::Bool(_) => Slot::Bool,
                Value::Str(s) => Slot::Str(s.to_string()),
                Value::Table(t) => Slot::Table(t_num[t]),
                Value::Func(c) => Slot::Func(c_num[c]),
                Value::Builtin(n) => Slot::Builtin(n),
            }
        };
        let mut shape = Shape::default();
        for r in &order {
            match r {
                Root::Table(t) => {
                    let tab = &self.tables[t];
                    shape.tables.push((
                        tab.hash.iter().map(|(k, v)| (k.clone(), slot(v))).collect(),
                        tab.arr.iter().map(slot).collect(),
                        tab.ints.iter().map(|(k, v)| (*k, slot(v))).collect(),
                    ));
                }
                Root::Scope(s) => {
                    let sc = &self.scopes[s];
                    shape.scopes.push((
                        sc.vars.iter().map(|(k, v)| (k.clone(), slot(v))).collect(),
                        sc.parent.map(|p| s_num[&p]),
                    ));
                }
                Root::Closure(c) => {
                    let cl = &self.closures[c];
                    shape.closures.push((cl.body, s_num[&cl.env]));
                }
            }
        }
        Ok((shape, order))
    }
}
