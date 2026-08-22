//! The CONCRETE heap the tracer keeps while the values go symbolic.
//!
//! This is the half of the design that makes the whole thing work. Tables,
//! their fields, their lengths, closure identity and scope structure are
//! all facts at trace time, so `count(objects)`, `#t`, `obj.hitbox.w` and
//! `objects[i]` resolve without anything symbolic entering the picture.
//! What stays symbolic is game DATA - positions, speeds, timers, button
//! state - and nothing indexes the heap with those.
//!
//! Where one does, the tracer REFUSES (`domain::refuse_unknown`) rather
//! than widening. That is the same stance as everywhere else here: a
//! transformation we cannot check is worse than no transformation.
//!
//! ## Scopes are boxed, always
//!
//! Every lexical scope is a heap object and a closure captures it by id,
//! so writing through an upvalue works without any capture analysis. That
//! is deliberately the most general thing rather than the cheapest: the
//! tracer runs at build time over one state, so an extra indirection per
//! variable read costs nothing anyone will ever measure, while getting
//! capture semantics subtly wrong would cost a silently wrong graph.

use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use anyhow::Result;

use super::domain::Domain;

pub type TableId = u32;
pub type ScopeId = u32;
pub type ClosureId = u32;
/// Index into the interpreter's list of function bodies (the AST outlives
/// the heap, so the heap stores an index rather than a reference).
pub type BodyId = u32;

pub enum Value<D: Domain> {
    Nil,
    Num(D::Num),
    Bool(D::Bool),
    Str(Rc<str>),
    Table(TableId),
    /// A closure. A HEAP OBJECT, like a table, referred to by id.
    ///
    /// Not `{ body, env }` inline, which is what this was: a closure has
    /// an identity of its own, and the old interpreter models it that way
    /// too (`HeapValue::Closure` sits at a `HeapId` behind a
    /// `Value::Pointer`, so it has content AND identity). Being in the
    /// heap also means the existing machinery does the work - GC collects
    /// one nothing refers to, and `shape`'s canonical BFS renumbers it,
    /// so two states that each built the same closure have the same
    /// SHAPE and merge.
    ///
    /// ## What PICO-8 actually does, which is not what Lua 5.4 does
    ///
    /// PICO-8 is Lua 5.2, which CACHES closures: `OP_CLOSURE` reuses an
    /// existing one with the same prototype and the same upvalue CELLS.
    /// (5.4 removed this, which is where the folklore that
    /// `function() end == function() end` is false comes from.) Measured,
    /// not assumed - `/tmp` carts run against ~/pico-8/pico8:
    ///
    /// ```text
    /// two calls, body has no upvalues              true   (cached)
    /// two calls, upvalue is a fresh local          false
    /// same local captured at two different sites   false  (two prototypes)
    /// body only reads a GLOBAL, so no upvalues     true
    /// same site, same upvalue cell                 true
    /// same site, different frames, equal values    false
    /// loop capturing i                             false
    /// loop capturing nothing                       true
    /// ```
    ///
    /// So identity is (prototype, upvalue cells). This model has the
    /// prototype exactly (`BodyId`, interned by AST node) but approximates
    /// the cells by the whole enclosing SCOPE, which is exact only when
    /// every capture comes from that scope. Two closures can therefore be
    /// distinct here and the same object in PICO-8. `==` on two closures
    /// is refused for that reason rather than answered - see
    /// `Interp::eval_binop`.
    Func(ClosureId),
    Builtin(&'static str),
}

impl<D: Domain> Clone for Value<D> {
    fn clone(&self) -> Self {
        match self {
            Value::Nil => Value::Nil,
            Value::Num(n) => Value::Num(n.clone()),
            Value::Bool(b) => Value::Bool(b.clone()),
            Value::Str(s) => Value::Str(s.clone()),
            Value::Table(t) => Value::Table(*t),
            Value::Func(c) => Value::Func(*c),
            Value::Builtin(n) => Value::Builtin(n),
        }
    }
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
            // Tag the kind: both domains render a Num and a Bool the same
            // way (a node id, or a raw number), and "arithmetic on
            // non-numbers: 201 and 184" says nothing without it.
            Value::Num(n) => write!(f, "num({:?})", n),
            Value::Bool(b) => write!(f, "bool({:?})", b),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Table(t) => write!(f, "table#{}", t),
            Value::Func(c) => write!(f, "closure#{}", c),
            Value::Builtin(n) => write!(f, "builtin:{}", n),
        }
    }
}

/// A Lua table: a string part, an ARRAY part and an integer part, which
/// is the split Lua itself has and the reason `#` behaves the way it
/// does. The IR collapses this into `ObjectTable`/`ArrayTable`; keeping
/// all three avoids having to decide what a fresh `{}` is before anything
/// has been put in it.
///
/// ## Why the integer part has to exist
///
/// `t[3] = v` on an empty table is legal Lua and puts 3 in the HASH part,
/// leaving the array part empty - so `#t` is 0, not 3. Modelling arrays
/// densely and materialising the gap as nils gets the READS right and the
/// length wrong. Measured in real PICO-8 (`lua/probe/tables.lua`, whose
/// output is checked in):
///
/// ```text
/// t={}      #t == 0
/// t[3]="c"  #t == 0     -- dense-with-nils would say 3
/// t[1]="a"  #t == 1
/// t[2]="b"  #t == 3     -- the array part absorbs 3 once the gap closes
/// {"a",nil,"c"}         #t == 3   -- a CONSTRUCTOR sizes the array part
/// {"a","b","c"} t[3]=nil #t == 2  -- a border search inside the array
/// ```
///
/// The same contents give different lengths depending on how the table
/// was BUILT, which is why this is a matter for measurement rather than
/// for reasoning about what "undefined for tables with holes" permits.
pub struct Table<D: Domain> {
    pub hash: BTreeMap<String, Value<D>>,
    /// The array part. May contain explicit `Nil`s; Lua does not shrink
    /// it when one is punched, and `len` finds the border instead.
    pub arr: Vec<Value<D>>,
    /// Integer keys OUTSIDE the array part - too large, or zero, or
    /// negative. Absorbed into `arr` when an append closes the gap.
    pub ints: BTreeMap<i16, Value<D>>,
}

impl<D: Domain> Table<D> {
    /// Every value the table holds. The ONE place that knows a table has
    /// three parts: `gc`, `shape` and `canonical_order` all went through
    /// their own hand-written `hash.values().chain(arr.iter())`, so
    /// adding the integer part meant finding all three, and a fourth
    /// would mean finding them again.
    pub fn values(&self) -> impl Iterator<Item = &Value<D>> {
        self.hash.values().chain(self.arr.iter()).chain(self.ints.values())
    }

    /// `#t`, WHEN THIS MODEL CAN ANSWER IT EXACTLY. `None` means raise.
    ///
    /// Lua's `luaH_getn` searches the array part's CAPACITY, which grows
    /// in powers of two at rehash and therefore depends on the table's
    /// history rather than its contents. Two tables holding the same
    /// keys can have different lengths:
    ///
    /// ```text
    /// t={} t[1]=1 t[2]=2 t[3]=3 t[2]=nil    #t == 1
    /// {"a",nil,"c"}                          #t == 3
    /// t={} t[1]=1 t[2]=2 t[4]=4              #t == 4  (rehash pulled 4 in)
    /// ```
    ///
    /// Line 1 and line 2 hold the same keys. Line 3's answer skips a
    /// genuine hole at 3, because `computesizes` decided a 4-slot array
    /// part was worth it. Reproducing any of this means reproducing
    /// `computesizes` and the node part's occupancy, and then the
    /// capacity becomes part of the SHAPE - a merge cost paid for
    /// something the cart cannot reach (`del` shifts left and drops the
    /// last, and `got_fruit` never has `#` taken of it).
    ///
    /// So: answer exactly, or raise. The answer is exact precisely when
    /// the integer part is empty and the array part has no INTERIOR hole,
    /// because then the border is the last non-nil index whatever the
    /// capacity is - Lua's binary search over any capacity at least that
    /// large lands in the same place, and with a full array part
    /// `unbound_search` starts past the end and stops immediately.
    /// Trailing holes are fine, which matters: that is what
    /// `__array_table_drop_last` leaves behind, and `del` then takes `#`
    /// of it on the next call.
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
            // MIGRATE. The array part absorbs whatever the integer part
            // has sitting immediately after it, which is what makes
            // `t={} t[3]=c t[1]=a t[2]=b` end at `#t == 3` while
            // `t={} t[3]=c` on its own is `#t == 0`.
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

impl<D: Domain> Clone for Table<D> {
    fn clone(&self) -> Self {
        Table { hash: self.hash.clone(), arr: self.arr.clone(), ints: self.ints.clone() }
    }
}

impl<D: Domain> Default for Table<D> {
    fn default() -> Self {
        Table { hash: BTreeMap::new(), arr: Vec::new(), ints: BTreeMap::new() }
    }
}

pub struct Scope<D: Domain> {
    pub vars: BTreeMap<String, Value<D>>,
    pub parent: Option<ScopeId>,
}

impl<D: Domain> Clone for Scope<D> {
    fn clone(&self) -> Self {
        Scope { vars: self.vars.clone(), parent: self.parent }
    }
}

/// A closure object: which function body, and the scope it captured.
/// Both are immutable, so unlike a table there is nothing here to merge -
/// it is in the heap for its IDENTITY.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Closure {
    pub body: BodyId,
    pub env: ScopeId,
}

pub struct Heap<D: Domain> {
    pub tables: BTreeMap<TableId, Table<D>>,
    pub scopes: BTreeMap<ScopeId, Scope<D>>,
    pub closures: BTreeMap<ClosureId, Closure>,
    next: u32,
}

impl<D: Domain> Clone for Heap<D> {
    fn clone(&self) -> Self {
        Heap {
            tables: self.tables.clone(),
            scopes: self.scopes.clone(),
            closures: self.closures.clone(),
            next: self.next,
        }
    }
}

impl<D: Domain> Default for Heap<D> {
    fn default() -> Self {
        Heap {
            tables: BTreeMap::new(),
            scopes: BTreeMap::new(),
            closures: BTreeMap::new(),
            next: 0,
        }
    }
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

    pub fn new_closure(&mut self, body: BodyId, env: ScopeId) -> ClosureId {
        let id = self.fresh();
        self.closures.insert(id, Closure { body, env });
        id
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = self.fresh();
        self.scopes.insert(id, Scope { vars: BTreeMap::new(), parent });
        id
    }

    /// Read a variable by walking the scope chain outward.
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

    /// Declare in the innermost scope (`local x = ..`).
    pub fn declare(&mut self, scope: ScopeId, name: &str, v: Value<D>) {
        if let Some(sc) = self.scopes.get_mut(&scope) {
            sc.vars.insert(name.to_string(), v);
        }
    }

    /// Drop everything unreachable from `roots`. Merging compares SHAPES,
    /// and two states that did the same thing by different routes leave
    /// different garbage behind, so collecting is what makes them
    /// comparable rather than merely tidy.
    pub fn gc(&mut self, roots: &[Root]) {
        let mut live_t: BTreeSet<TableId> = BTreeSet::new();
        let mut live_s: BTreeSet<ScopeId> = BTreeSet::new();
        let mut live_c: BTreeSet<ClosureId> = BTreeSet::new();
        let mut stack: Vec<Root> = roots.to_vec();
        while let Some(r) = stack.pop() {
            match r {
                Root::Closure(c) => {
                    if !live_c.insert(c) {
                        continue;
                    }
                    if let Some(cl) = self.closures.get(&c) {
                        stack.push(Root::Scope(cl.env));
                    }
                }
                Root::Table(t) => {
                    if !live_t.insert(t) {
                        continue;
                    }
                    if let Some(tab) = self.tables.get(&t) {
                        for v in tab.values() {
                            push_value(v, &mut stack);
                        }
                    }
                }
                Root::Scope(s) => {
                    if !live_s.insert(s) {
                        continue;
                    }
                    if let Some(sc) = self.scopes.get(&s) {
                        for v in sc.vars.values() {
                            push_value(v, &mut stack);
                        }
                        if let Some(p) = sc.parent {
                            stack.push(Root::Scope(p));
                        }
                    }
                }
            }
        }
        self.tables.retain(|k, _| live_t.contains(k));
        self.scopes.retain(|k, _| live_s.contains(k));
        self.closures.retain(|k, _| live_c.contains(k));
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Root {
    Closure(ClosureId),
    Table(TableId),
    Scope(ScopeId),
}

/// The objects a value refers to. The ONE place that knows which value
/// kinds are references, so `gc`, `shape` and `canonical_order` cannot
/// disagree about it - they each had their own copy, and adding closures
/// to the heap meant finding all three.
pub fn push_value<D: Domain>(v: &Value<D>, stack: &mut Vec<Root>) {
    match v {
        Value::Table(t) => stack.push(Root::Table(*t)),
        Value::Func(c) => stack.push(Root::Closure(*c)),
        _ => {}
    }
}

/// The part of a state that decides whether two states can MERGE.
///
/// Everything here is structure, not data: which objects exist, what keys
/// they have, how long the arrays are, which function each closure is. Two
/// states with the same shape differ only in symbolic values, so they
/// merge into one with a `Sel` per differing slot. Two states with
/// different shapes are different successors and both survive - which is
/// how a frame that kills the player naturally produces two output shapes
/// rather than needing a specialization set to express it.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Slot {
    Nil,
    Num,
    Bool,
    Str(String),
    Table(u32),
    /// The CANONICAL closure number, exactly as `Table` is the canonical
    /// table number. Which body it is and what it captured live in
    /// `Shape::closures`, so identity and content stay separate here too.
    Func(u32),
    Builtin(&'static str),
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Shape {
    /// Canonical (BFS-numbered) tables: their keys and their slot kinds,
    /// for all THREE parts - string, array, integer. The integer part is
    /// part of the shape for the same reason the others are: two states
    /// whose tables differ there cannot be merged with a select.
    pub tables: Vec<(Vec<(String, Slot)>, Vec<Slot>, Vec<(i16, Slot)>)>,
    pub scopes: Vec<(Vec<(String, Slot)>, Option<u32>)>,
    /// Canonical (BFS-numbered) closures: their body, and the canonical
    /// number of the scope they captured.
    pub closures: Vec<(BodyId, u32)>,
}

impl<D: Domain> Heap<D> {
    /// Canonicalize from `roots` in BFS order, so two heaps that allocated
    /// the same structure in a different ORDER compare equal.
    pub fn shape(&self, roots: &[Root]) -> Result<Shape> {
        let mut t_num: BTreeMap<TableId, u32> = BTreeMap::new();
        let mut s_num: BTreeMap<ScopeId, u32> = BTreeMap::new();
        let mut c_num: BTreeMap<ClosureId, u32> = BTreeMap::new();
        let mut queue: Vec<Root> = roots.to_vec();
        let mut order: Vec<Root> = Vec::new();
        let mut i = 0;
        while i < queue.len() {
            let r = queue[i];
            i += 1;
            match r {
                Root::Closure(c) => {
                    if c_num.contains_key(&c) {
                        continue;
                    }
                    c_num.insert(c, c_num.len() as u32);
                    order.push(r);
                    if let Some(cl) = self.closures.get(&c) {
                        queue.push(Root::Scope(cl.env));
                    }
                }
                Root::Table(t) => {
                    if t_num.contains_key(&t) {
                        continue;
                    }
                    t_num.insert(t, t_num.len() as u32);
                    order.push(r);
                    if let Some(tab) = self.tables.get(&t) {
                        for v in tab.values() {
                            push_value(v, &mut queue);
                        }
                    }
                }
                Root::Scope(s) => {
                    if s_num.contains_key(&s) {
                        continue;
                    }
                    s_num.insert(s, s_num.len() as u32);
                    order.push(r);
                    if let Some(sc) = self.scopes.get(&s) {
                        for v in sc.vars.values() {
                            push_value(v, &mut queue);
                        }
                        if let Some(p) = sc.parent {
                            queue.push(Root::Scope(p));
                        }
                    }
                }
            }
        }
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
        Ok(shape)
    }
}
