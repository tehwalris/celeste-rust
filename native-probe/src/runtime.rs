//! Concrete-lane runtime for the transpiled Celeste IR (probe, task #124).
//!
//! Every operation is a port of the SCALAR path of the vectorized
//! interpreter, with a `file:line` pointer to the code it mirrors. The
//! contract is hex-exactness against `concrete_run`, so anything the
//! concrete path cannot reach (intervals, unknown bools in ops, lane
//! machinery) panics loudly instead of approximating.

use std::sync::Arc;

use celeste_rust::cart_data::CartData;
use celeste_rust::collision_cache::CollisionCache;
use celeste_rust::pico8_num::Pico8Num;

pub type P8 = Pico8Num;

/// One local/argument value. 8 bytes, `Copy`.
/// Mirrors `interpreter::value::Value` minus the abstract-only variants
/// (`NumberInterval`, `MaybeBool`) and the per-value payloads that only
/// feed error messages (`Nil(Option<String>)`, `NilPointer(String)`).
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum V {
    Num(P8),
    Bool(bool),
    /// `Value::UnknownBool`. Reached concretely only as the value
    /// `__reset_button_states` stores at the END of a frame, overwritten by
    /// `set_buttons` before the next frame ever reads it. Every op panics.
    UBool,
    Str(u32),
    Nil,
    Ptr(u32),
    NilPtr,
}

/// One heap cell. Mirrors `interpreter::value::HeapValue`.
#[derive(Clone, Debug)]
pub enum Cell {
    Val(V),
    /// ObjectTable: (interned field id, cell id), insertion-ordered.
    Obj(Vec<(u32, u32)>),
    Arr(Vec<u32>),
    /// UnknownTable - a fresh `{}` before its shape is known.
    Unk,
    Clo(u32, Box<[V]>),
    Bi(u32),
}

// Builtin ids. ORDER IS AN ABI shared with the generator
// (src/bin/transpile.rs BUILTIN_NAMES) and `call_builtin` below.
// Names/impls: game_runner.rs create_fixed_env_with_(game_)builtins.
pub const BUILTIN_NAMES: [&str; 18] = [
    "__print",
    "__new_unknown_boolean",
    "__widen_rem",
    "__new_vector",
    "__array_table_drop_last",
    "error",
    "min",
    "max",
    "abs",
    "flr",
    "__split_by_flr",
    "__split_at",
    "add",
    "print",
    "sin",
    "mget",
    "fget",
    "tile_flag_at",
];
pub const BI___PRINT: u32 = 0;
pub const BI___NEW_UNKNOWN_BOOLEAN: u32 = 1;
pub const BI___WIDEN_REM: u32 = 2;
pub const BI___NEW_VECTOR: u32 = 3;
pub const BI___ARRAY_TABLE_DROP_LAST: u32 = 4;
pub const BI_ERROR: u32 = 5;
pub const BI_MIN: u32 = 6;
pub const BI_MAX: u32 = 7;
pub const BI_ABS: u32 = 8;
pub const BI_FLR: u32 = 9;
pub const BI___SPLIT_BY_FLR: u32 = 10;
pub const BI___SPLIT_AT: u32 = 11;
pub const BI_ADD: u32 = 12;
pub const BI_PRINT: u32 = 13;
pub const BI_SIN: u32 = 14;
pub const BI_MGET: u32 = 15;
pub const BI_FGET: u32 = 16;
pub const BI_TILE_FLAG_AT: u32 = 17;

/// "absent" marker in the globals table.
pub const NONE: u32 = u32::MAX;

pub struct Rt {
    pub heap: Vec<Cell>,
    /// Interned global name id -> heap cell (NONE = absent).
    /// Mirrors `State::global_env`.
    pub globals: Vec<u32>,
    /// Static strings from the program + runtime concat results.
    pub strings: Vec<String>,
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
    pub prints: Vec<String>,
    /// Per-site receiver log for the gap census (empty = logging off).
    /// Lattice per site: 0 = unseen, cell+1 = single receiver, MAX = multi.
    pub site_log: Vec<u64>,
    /// Per-branch-site outcome flags (empty = off): bit0 = took true,
    /// bit1 = took false. 3 across a fan-out = divergent site.
    pub branch_log: Vec<u8>,
}

#[inline]
fn num(v: V) -> P8 {
    match v {
        V::Num(n) => n,
        other => panic!("expected a number, got {:?}", other),
    }
}

#[inline]
fn ptr_of(v: V) -> u32 {
    // heap_id_from_pointer_local: a Store/GetField/... receiver must be a
    // real pointer (core_interpreter.rs).
    match v {
        V::Ptr(p) => p,
        other => panic!("expected a pointer, got {:?}", other),
    }
}

impl Rt {
    pub fn new(
        cart: Arc<CartData>,
        cache: Arc<CollisionCache>,
        globals_len: usize,
        static_strings: &[&str],
    ) -> Self {
        Rt {
            heap: Vec::with_capacity(1 << 16),
            globals: vec![NONE; globals_len],
            strings: static_strings.iter().map(|s| s.to_string()).collect(),
            cart,
            cache,
            prints: Vec::new(),
            site_log: Vec::new(),
            branch_log: Vec::new(),
        }
    }

    /// Join `recv` into the site's receiver lattice (census mode only).
    #[inline]
    fn log_site(&mut self, site: u32, recv: V) {
        if self.site_log.is_empty() {
            return;
        }
        let slot = &mut self.site_log[site as usize];
        let v = match recv {
            V::Ptr(p) => p as u64 + 1,
            _ => u64::MAX, // non-pointer receiver: never columnizable
        };
        *slot = match *slot {
            0 => v,
            x if x == v => x,
            _ => u64::MAX,
        };
    }

    #[inline]
    pub fn alloc(&mut self, c: Cell) -> u32 {
        let id = self.heap.len() as u32;
        self.heap.push(c);
        id
    }

    /// `Instruction::Alloc` (core_interpreter.rs:122). The interpreter's
    /// alloc leaves the cell unwritten (write-before-read invariant); we
    /// seed `Val(Nil)` which is observationally identical (Nil is what Lua
    /// reads from a never-written cell anyway).
    #[inline]
    pub fn alloc_nil(&mut self) -> V {
        V::Ptr(self.alloc(Cell::Val(V::Nil)))
    }

    /// `Instruction::GetGlobal` (core_interpreter.rs:279).
    #[inline]
    pub fn get_global(&mut self, g: u32, create: bool) -> V {
        let cell = self.globals[g as usize];
        if cell != NONE {
            V::Ptr(cell)
        } else if create {
            let id = self.alloc(Cell::Val(V::Nil));
            self.globals[g as usize] = id;
            V::Ptr(id)
        } else {
            V::NilPtr // NilPointer("global {name}")
        }
    }

    /// `Instruction::Load` (core_interpreter.rs:298).
    #[inline]
    pub fn load(&self, v: V) -> V {
        match v {
            V::Ptr(p) => match &self.heap[p as usize] {
                Cell::Val(x) => *x,
                // Tables, closures and builtins load as self-pointers.
                _ => V::Ptr(p),
            },
            V::NilPtr => V::Nil, // Nil("nil pointer to ..")
            other => panic!("load: not a pointer: {:?}", other),
        }
    }

    /// `Instruction::Store` (core_interpreter.rs:317).
    #[inline]
    pub fn store(&mut self, t: V, s: V) {
        let p = ptr_of(t);
        self.heap[p as usize] = Cell::Val(s);
    }

    /// `Instruction::StoreEmptyTable` (core_interpreter.rs:340).
    #[inline]
    pub fn store_empty_table(&mut self, t: V) {
        let p = ptr_of(t);
        self.heap[p as usize] = Cell::Unk;
    }

    /// `Instruction::StoreClosure` (core_interpreter.rs:345).
    #[inline]
    pub fn store_closure(&mut self, t: V, fun: u32, caps: &[V]) {
        let p = ptr_of(t);
        self.heap[p as usize] = Cell::Clo(fun, caps.into());
    }

    /// `Instruction::GetField` (core_interpreter.rs:362).
    pub fn get_field(&mut self, recv: V, f: u32, create: bool, site: u32) -> V {
        self.log_site(site, recv);
        let table = ptr_of(recv);
        let existing = match &self.heap[table as usize] {
            Cell::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
            Cell::Unk => None,
            other => panic!("GetField on a non-object cell: {:?}", other),
        };
        if let Some(cell) = existing {
            V::Ptr(cell)
        } else if create {
            let cell = self.alloc(Cell::Val(V::Nil));
            match &mut self.heap[table as usize] {
                Cell::Obj(fields) => fields.push((f, cell)),
                slot @ Cell::Unk => *slot = Cell::Obj(vec![(f, cell)]),
                _ => unreachable!(),
            }
            V::Ptr(cell)
        } else {
            V::NilPtr // NilPointer("field {name}")
        }
    }

    /// `Instruction::GetIndex` (core_interpreter.rs:412), including the
    /// gap-fill create path (dense arrays; skipped indices materialise as
    /// explicit nils).
    pub fn get_index(&mut self, recv: V, idx: V, create: bool, site: u32) -> V {
        self.log_site(site, recv);
        let table = ptr_of(recv);
        let index = num(idx)
            .as_i16()
            .expect("GetIndex: index is a scalar number, but not an integer");
        assert!(index >= 1, "GetIndex: index is less than 1");
        let existing = match &self.heap[table as usize] {
            Cell::Arr(items) => items.get(index as usize - 1).copied(),
            Cell::Unk => None,
            other => panic!("GetIndex on a non-array cell: {:?}", other),
        };
        if let Some(cell) = existing {
            V::Ptr(cell)
        } else if create {
            let cell = self.alloc(Cell::Val(V::Nil));
            let old_len = match &self.heap[table as usize] {
                Cell::Arr(items) => items.len(),
                Cell::Unk => 0,
                _ => unreachable!(),
            };
            let mut tail: Vec<u32> = Vec::with_capacity(index as usize - old_len);
            for _ in old_len..(index as usize - 1) {
                let gap = self.alloc(Cell::Val(V::Nil));
                tail.push(gap);
            }
            tail.push(cell);
            match &mut self.heap[table as usize] {
                Cell::Arr(items) => items.extend_from_slice(&tail),
                slot @ Cell::Unk => *slot = Cell::Arr(tail),
                _ => unreachable!(),
            }
            V::Ptr(cell)
        } else {
            V::NilPtr // NilPointer("index {i}")
        }
    }

    // ---- unary ops (op.rs:12-49) ----

    #[inline]
    pub fn un_minus(&self, v: V) -> V {
        V::Num(-num(v))
    }

    #[inline]
    pub fn un_not(&self, v: V) -> V {
        // interpret_not (op.rs:12): bools only, NOT Lua truthiness - the
        // frontend compiles truthiness into branches.
        match v {
            V::Bool(b) => V::Bool(!b),
            other => panic!("not: unsupported value {:?}", other),
        }
    }

    pub fn un_hash(&self, v: V) -> V {
        match v {
            V::Str(s) => V::Num(P8::from_i16(self.strings[s as usize].len() as i16)),
            V::Ptr(p) => match &self.heap[p as usize] {
                Cell::Arr(items) => V::Num(P8::from_i16(items.len() as i16)),
                Cell::Obj(_) | Cell::Unk => V::Num(P8::from_i16(0)),
                other => panic!("# on non-table pointer: {:?}", other),
            },
            other => panic!("# on {:?}", other),
        }
    }

    // ---- binary ops (op.rs:397-666, scalar arms) ----

    /// `==` (op.rs:432-467). Exhaustive over what a concrete lane can hold;
    /// arms the interpreter rejects (e.g. NilPointer on the left) panic.
    pub fn eq(&self, l: V, r: V) -> V {
        let b = match (l, r) {
            (V::Num(a), V::Num(b)) => a == b,
            (V::Num(_), _) => false,
            (V::Bool(a), V::Bool(b)) => a == b,
            (V::Bool(_), V::UBool) | (V::UBool, _) => {
                panic!("== on UnknownBool on the concrete path")
            }
            (V::Bool(_), _) => false,
            (V::Str(a), V::Str(b)) => {
                a == b || self.strings[a as usize] == self.strings[b as usize]
            }
            (V::Str(_), _) => false,
            (V::Nil, V::Nil) => true,
            (V::Nil, _) => false,
            (V::Ptr(a), V::Ptr(b)) => a == b,
            (V::Ptr(_), _) => false,
            (V::NilPtr, _) => panic!("== on a nil pointer"),
        };
        V::Bool(b)
    }

    #[inline]
    pub fn ne(&self, l: V, r: V) -> V {
        // `~=` is not(==) (op.rs:429).
        match self.eq(l, r) {
            V::Bool(b) => V::Bool(!b),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn lt(&self, l: V, r: V) -> V {
        V::Bool(num(l) < num(r))
    }
    #[inline]
    pub fn le(&self, l: V, r: V) -> V {
        V::Bool(num(l) <= num(r))
    }
    #[inline]
    pub fn gt(&self, l: V, r: V) -> V {
        V::Bool(num(l) > num(r))
    }
    #[inline]
    pub fn ge(&self, l: V, r: V) -> V {
        V::Bool(num(l) >= num(r))
    }

    #[inline]
    pub fn op_add(&self, l: V, r: V) -> V {
        V::Num(num(l) + num(r))
    }
    #[inline]
    pub fn op_sub(&self, l: V, r: V) -> V {
        V::Num(num(l) - num(r))
    }
    #[inline]
    pub fn op_mul(&self, l: V, r: V) -> V {
        V::Num(num(l) * num(r))
    }
    #[inline]
    pub fn op_div(&self, l: V, r: V) -> V {
        V::Num(num(l) / num(r))
    }
    #[inline]
    pub fn op_rem(&self, l: V, r: V) -> V {
        V::Num(num(l) % num(r))
    }
    pub fn op_pow(&self, _l: V, _r: V) -> V {
        // BinaryOp::Caret has no interpreter arm either (op.rs falls through
        // to "Unsupported binary op"); the cart never uses it.
        panic!("^ reached - the interpreter has no arm for it either")
    }

    /// `..` (op.rs:469-478): string/string plus integer/string in either
    /// order; a fractional number errors (`as_i16_or_err`).
    pub fn concat(&mut self, l: V, r: V) -> V {
        let s = match (l, r) {
            (V::Str(a), V::Str(b)) => {
                format!("{}{}", self.strings[a as usize], self.strings[b as usize])
            }
            (V::Str(a), V::Num(n)) => format!(
                "{}{}",
                self.strings[a as usize],
                n.as_i16().expect(".. on a fractional number")
            ),
            (V::Num(n), V::Str(b)) => format!(
                "{}{}",
                n.as_i16().expect(".. on a fractional number"),
                self.strings[b as usize]
            ),
            other => panic!(".. on {:?}", other),
        };
        let id = self.strings.len() as u32;
        self.strings.push(s);
        V::Str(id)
    }

    // ---- control ----

    /// Branch truthiness (flow.rs:343-394).
    #[inline]
    pub fn truthy(&self, v: V) -> bool {
        match v {
            V::Bool(b) => b,
            V::Nil => false,
            V::Num(_) | V::Str(_) | V::Ptr(_) => true,
            V::NilPtr => panic!("branch on a nil pointer"),
            V::UBool => panic!("branch on UnknownBool on the concrete path"),
        }
    }

    /// `truthy` with branch-site logging (SIMD divergence census).
    #[inline]
    pub fn truthy_b(&mut self, v: V, site: u32) -> bool {
        let t = self.truthy(v);
        if !self.branch_log.is_empty() {
            self.branch_log[site as usize] |= if t { 1 } else { 2 };
        }
        t
    }

    /// `Select` condition (op.rs:158 interpret_select): the condition must
    /// BE a bool - not merely truthy.
    #[inline]
    pub fn sel_bool(&self, v: V) -> bool {
        match v {
            V::Bool(b) => b,
            other => panic!("select on a non-bool condition: {:?}", other),
        }
    }

    /// `Expand` (core_interpreter.rs:545): identity on a concrete bool.
    #[inline]
    pub fn expand(&self, v: V) -> V {
        match v {
            V::Bool(b) => V::Bool(b),
            other => panic!("expand on {:?} on the concrete path", other),
        }
    }

    // ---- guards (recipe-planted instructions; rewritten program only) ----

    #[inline]
    pub fn assert_builtin(&self, callee: V, bi: u32) {
        // CallBuiltin's callee check (core_interpreter.rs:690).
        let p = ptr_of(callee);
        match &self.heap[p as usize] {
            Cell::Bi(b) if *b == bi => {}
            other => panic!(
                "CallBuiltin({}) failed: cell holds {:?}",
                BUILTIN_NAMES[bi as usize], other
            ),
        }
    }

    pub fn assert_closure(&self, v: V, fun: u32, caps: &[V], ctx: &str) {
        let p = ptr_of(v);
        match &self.heap[p as usize] {
            Cell::Clo(f, c) if *f == fun && c.as_ref() == caps => {}
            other => panic!("AssertClosure(fn {}) failed at {}: {:?}", fun, ctx, other),
        }
    }

    #[inline]
    pub fn assert_pointer(&self, v: V, ctx: &str) {
        match v {
            V::Ptr(_) => {}
            other => panic!("AssertPointer failed at {}: {:?}", ctx, other),
        }
    }

    #[inline]
    pub fn assert_value_cell(&self, v: V, ctx: &str) {
        let p = ptr_of(v);
        match &self.heap[p as usize] {
            Cell::Val(_) => {}
            other => panic!("AssertValueCell failed at {}: {:?}", ctx, other),
        }
    }

    #[inline]
    pub fn assert_true(&self, v: V, ctx: &str) {
        match v {
            V::Bool(true) => {}
            other => panic!("AssertTrue failed at {}: {:?}", ctx, other),
        }
    }

    // ---- builtins (game_runner.rs impls, scalar arms) ----

    pub fn call_builtin(&mut self, b: u32, args: &[V]) -> V {
        match b {
            BI___PRINT | BI_PRINT => {
                let printed = match args.first() {
                    None => String::new(),
                    Some(V::Str(s)) => self.strings[*s as usize].clone(),
                    Some(V::Num(n)) => format!("{:?}", n),
                    Some(V::Bool(x)) => x.to_string(),
                    Some(V::Nil) => "nil".to_string(),
                    Some(other) => format!("{:?}", other),
                };
                self.prints.push(printed);
                V::Nil
            }
            BI___NEW_UNKNOWN_BOOLEAN => {
                assert!(args.is_empty(), "__new_unknown_boolean takes no arguments");
                V::UBool
            }
            BI___WIDEN_REM | BI___NEW_VECTOR => {
                panic!("abstract-only builtin reached on the concrete path")
            }
            BI___ARRAY_TABLE_DROP_LAST => {
                // game_runner.rs:496.
                let p = ptr_of(args[0]);
                match &mut self.heap[p as usize] {
                    Cell::Arr(items) => {
                        assert!(!items.is_empty(), "drop_last of an empty array");
                        items.pop();
                    }
                    other => panic!("__array_table_drop_last on {:?}", other),
                }
                V::Nil
            }
            BI_ERROR => panic!("error called: {:?}", args),
            BI_MIN => V::Num(num(args[0]).min(num(args[1]))),
            BI_MAX => V::Num(num(args[0]).max(num(args[1]))),
            BI_ABS => V::Num(num(args[0]).abs()),
            BI_FLR => V::Num(num(args[0]).flr()),
            // Identity on a scalar number (game_runner.rs:240-242 / :354).
            BI___SPLIT_BY_FLR | BI___SPLIT_AT => V::Num(num(args[0])),
            BI_ADD => {
                // game_runner.rs:53 builtin_add. Dead in practice: the Lua
                // `add` (builtin_level_3.lua:1) shadows it during init.
                let p = ptr_of(args[0]);
                let value = args[1];
                let cell = self.alloc(Cell::Val(value));
                match &mut self.heap[p as usize] {
                    Cell::Arr(items) => items.push(cell),
                    slot @ Cell::Unk => *slot = Cell::Arr(vec![cell]),
                    other => panic!("add on {:?}", other),
                }
                value
            }
            BI_SIN => V::Num(num(args[0]).pico8_sin()),
            BI_MGET => self.bi_mget(args[0], args[1]),
            BI_FGET => V::Bool(
                self.cart
                    .fget(num(args[0]), num(args[1]))
                    .expect("fget failed"),
            ),
            BI_TILE_FLAG_AT => self.bi_tile_flag_at(args[0], args[1], args[2], args[3], args[4]),
            other => panic!("unknown builtin id {}", other),
        }
    }

    #[inline]
    pub fn bi_min(&self, l: V, r: V) -> V {
        V::Num(num(l).min(num(r)))
    }
    #[inline]
    pub fn bi_max(&self, l: V, r: V) -> V {
        V::Num(num(l).max(num(r)))
    }
    #[inline]
    pub fn bi_abs(&self, v: V) -> V {
        V::Num(num(v).abs())
    }
    #[inline]
    pub fn bi_flr(&self, v: V) -> V {
        V::Num(num(v).flr())
    }
    #[inline]
    pub fn bi_sin(&self, v: V) -> V {
        V::Num(num(v).pico8_sin())
    }

    #[inline]
    pub fn bi_mget(&self, x: V, y: V) -> V {
        V::Num(P8::from_i16(
            self.cart.mget(num(x), num(y)).expect("mget failed") as i16,
        ))
    }

    /// tile_flag_at (game_runner.rs:564): flag 0 through the collision
    /// cache's precomputed solid map with a computed fallback; non-zero
    /// flags fall to `tile_flag_at_computed`, whose per-element core
    /// (game_runner.rs:718) returns false for any non-zero flag.
    pub fn bi_tile_flag_at(&self, x: V, y: V, w: V, h: V, flag: V) -> V {
        let f = num(flag).as_i16().expect("tile_flag_at: flag must be integer");
        if f != 0 {
            return V::Bool(false);
        }
        let wi = num(w).as_i16().expect("tile_flag_at: w must be integer");
        let hi = num(h).as_i16().expect("tile_flag_at: h must be integer");
        let xi = num(x).as_i16().expect("tile_flag_at: x must be an integer");
        let yi = num(y).as_i16().expect("tile_flag_at: y must be an integer");
        if let Some((map, dx, dy)) = self.cache.solid_map(wi, hi) {
            if let Some(v) = map.get(xi + dx, yi + dy) {
                return V::Bool(v);
            }
        }
        V::Bool(
            self.cache
                .solid_at(&self.cart, xi, yi, wi, hi)
                .unwrap_or(false),
        )
    }
}
