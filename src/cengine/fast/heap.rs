//! The fast interpreter's run-time state: a `Copy` value and a heap that
//! is two flat vectors, so a path's private copy of it is a handful of
//! `memcpy`s rather than a walk over a `BTreeMap` of `BTreeMap`s.
//!
//! The table model is the reference's (`cengine::heap::Table`): a string
//! part, an array part that may hold explicit nils, and an integer part
//! for indices outside the array part, with `len` and `set_index` copied
//! semantics-for-semantics - those two are what PICO-8's `#` and `del`
//! actually depend on, and they are checked against a real console there.

use crate::pico8_num::Pico8NumInterval as Iv;

use super::program::{Builtin, FuncId, Sym};

pub type TableId = u32;
pub type ClosureId = u32;

/// 12 bytes, `Copy`. A number is always an interval (a point is `[n, n]`),
/// which is the reference domain's `Num`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Value {
    Nil,
    Num(Iv),
    Bool(bool),
    /// An UNDECIDED boolean: what `__new_unknown_boolean()` returns. The
    /// reference forks it on creation; here it is forked on first use
    /// (`Exec::truthy`), which yields the same leaf SET with 2^k fewer
    /// paths for the k buttons a path never reads. The index names the
    /// path-local decision slot, so a token read twice decides once.
    UBool(u32),
    Str(Sym),
    Table(TableId),
    Func(ClosureId),
    Builtin(Builtin),
}

impl Value {
    pub fn kind(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::Num(_) => "number",
            Value::Bool(_) | Value::UBool(_) => "boolean",
            Value::Str(_) => "string",
            Value::Table(_) => "table",
            Value::Func(_) => "function",
            Value::Builtin(_) => "builtin",
        }
    }
}

#[derive(Clone, Default)]
pub struct Table {
    /// The string part. Unordered; a linear scan over a dozen `u32`s
    /// beats any map at this size.
    pub hash: Vec<(Sym, Value)>,
    pub arr: Vec<Value>,
    pub ints: Vec<(i16, Value)>,
}

impl Table {
    pub fn get_field(&self, k: Sym) -> Value {
        for (s, v) in &self.hash {
            if *s == k {
                return *v;
            }
        }
        Value::Nil
    }

    pub fn set_field(&mut self, k: Sym, v: Value) {
        for (s, slot) in self.hash.iter_mut() {
            if *s == k {
                *slot = v;
                return;
            }
        }
        self.hash.push((k, v));
    }

    /// `#t` when this model can answer it exactly, else `None` (raise).
    /// Copied from the reference `Table::len`; see the reasoning there.
    pub fn len(&self) -> Option<usize> {
        if !self.ints.is_empty() {
            return None;
        }
        let last = self.arr.iter().rposition(|v| !matches!(v, Value::Nil)).map_or(0, |i| i + 1);
        if self.arr[..last].iter().any(|v| matches!(v, Value::Nil)) {
            return None;
        }
        Some(last)
    }

    pub fn get_index(&self, i: i16) -> Value {
        if i >= 1 && (i as usize) <= self.arr.len() {
            return self.arr[i as usize - 1];
        }
        for (k, v) in &self.ints {
            if *k == i {
                return *v;
            }
        }
        Value::Nil
    }

    fn remove_int(&mut self, i: i16) -> Option<Value> {
        let pos = self.ints.iter().position(|(k, _)| *k == i)?;
        Some(self.ints.remove(pos).1)
    }

    pub fn set_index(&mut self, i: i16, v: Value) {
        if i >= 1 && (i as usize) <= self.arr.len() {
            self.arr[i as usize - 1] = v;
            return;
        }
        if i >= 1 && i as usize == self.arr.len() + 1 {
            self.arr.push(v);
            // Migrate: the array part absorbs what the integer part holds
            // immediately after it (reference `set_index`).
            while let Ok(k) = i16::try_from(self.arr.len() + 1) {
                match self.remove_int(k) {
                    Some(next) => self.arr.push(next),
                    None => break,
                }
            }
            return;
        }
        if matches!(v, Value::Nil) {
            self.remove_int(i);
        } else if let Some(slot) = self.ints.iter_mut().find(|(k, _)| *k == i) {
            slot.1 = v;
        } else {
            self.ints.push((i, v));
        }
    }
}

/// A closure: its body and the VALUES it captured. Value capture, not a
/// shared cell: the lowering refuses any function whose captured local is
/// assigned after it could have been captured, so the two are the same
/// thing for every program it accepts (`lower::check_capture_immutable`).
/// This is also exactly what the boundary stores (`HeapValue::Closure`
/// carries values), so nothing is lost crossing it.
#[derive(Clone)]
pub struct Closure {
    pub func: FuncId,
    pub upvals: Vec<Value>,
}

#[derive(Clone, Default)]
pub struct Heap {
    pub tables: Vec<Table>,
    pub closures: Vec<Closure>,
    /// Indexed by `Sym`. `None` is a name that was never assigned; a
    /// global assigned nil is `Some(Nil)` and KEEPS its key, as in the
    /// reference's globals table (and the old interpreter's, which the
    /// boundary reproduces key-for-key). Reads treat both as nil.
    pub globals: Vec<Option<Value>>,
}

impl Heap {
    pub fn new_table(&mut self) -> TableId {
        self.tables.push(Table::default());
        (self.tables.len() - 1) as TableId
    }

    pub fn new_closure(&mut self, func: FuncId, upvals: Vec<Value>) -> ClosureId {
        self.closures.push(Closure { func, upvals });
        (self.closures.len() - 1) as ClosureId
    }

    pub fn global(&self, s: Sym) -> Value {
        self.globals.get(s as usize).copied().flatten().unwrap_or(Value::Nil)
    }

    pub fn set_global(&mut self, s: Sym, v: Value) {
        let i = s as usize;
        if i >= self.globals.len() {
            self.globals.resize(i + 1, None);
        }
        self.globals[i] = Some(v);
    }
}
