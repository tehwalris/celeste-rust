//! The fast interpreter: one straight-line path over the resolved
//! `Program`, with the reference domain's interval arithmetic and its
//! decision cursor doing the forking.
//!
//! This is `cengine::interp::Interp<RefDomain>` with everything that only
//! the tracer needed taken out. Under `RefDomain` `decide` is always
//! `Some`, so no expression ever fans out: `eval` returns ONE `Value` and
//! a statement ONE `Flow`. Names are slots and symbols, a call is a frame
//! on one value stack, and the heap is two vectors. What is kept, exactly:
//! the order every sub-expression is evaluated in (the cursor keys forks
//! by encounter order, so the leaf set depends on it), which operations
//! poison a path and which refuse the frame, the table model, and the
//! symbolic-loop unrolling with its finished-obligation. Every domain
//! operation goes through `RefDomain` itself rather than a reimplementation.

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use rustc_hash::FxHashMap;

use crate::cengine::domain::{Arith, Cmp, Domain, Fun1, Fun2};
use crate::cengine::refdomain::RefDomain;
use crate::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};

use super::heap::{ClosureId, Heap, Value};
use super::program::{Block, Builtin, CaptureSrc, Expr, FuncId, Last, Program, Stmt, Sym};

pub enum Flow {
    Normal,
    Break,
    Return(Value),
}

/// "No closure": the chunk and the frame body run outside any closure and
/// have no upvalues.
const NO_CLOSURE: ClosureId = u32::MAX;

pub struct Exec {
    prog: &'static Program,
    pub d: RefDomain,
    pub heap: Heap,
    stack: Vec<Value>,
    base: usize,
    cur_closure: ClosureId,
    /// The reference's `State::ok`: false once the path did something no
    /// legal run does (`poison`). A poisoned path executes no further
    /// STATEMENTS but still unwinds, and its leaf is still reported.
    pub ok: bool,
    /// Poison reasons and counts, for diagnostics.
    pub illegal: FxHashMap<String, u64>,
    pub prints: Vec<String>,
    /// The decision slots behind `Value::UBool`, reset per path.
    pub decided: Vec<Option<bool>>,
    cart: Arc<CartData>,
    cache: Arc<CollisionCache>,
    room_flags: FxHashMap<(i16, i16, i16), bool>,
    sym_room: Sym,
    sym_x: Sym,
    sym_y: Sym,
    /// SPIKE INSTRUMENTATION: expressions evaluated.
    pub n_eval: u64,
}

impl Exec {
    pub fn new(prog: &'static Program, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Result<Self> {
        let sym = |n: &str| prog.interner.get(n).ok_or_else(|| anyhow!("program never names {:?}", n));
        Ok(Exec {
            prog,
            d: RefDomain::new(),
            heap: Heap::default(),
            stack: Vec::with_capacity(1024),
            base: 0,
            cur_closure: NO_CLOSURE,
            ok: true,
            illegal: FxHashMap::default(),
            prints: Vec::new(),
            decided: Vec::new(),
            cart,
            cache,
            room_flags: FxHashMap::default(),
            sym_room: sym("room")?,
            sym_x: sym("x")?,
            sym_y: sym("y")?,
            n_eval: 0,
        })
    }

    pub fn program(&self) -> &'static Program {
        self.prog
    }

    // ------------------------------------------------------------ calls

    /// Call the global function `name` with no arguments, from outside any
    /// closure (the driver's `_update()` etc.).
    pub fn call_global(&mut self, name: Sym) -> Result<Value> {
        let f = self.heap.global(name);
        let base = self.stack.len();
        self.call(f, base, 0)
            .map_err(|e| anyhow!("calling {}: {:#}", self.prog.interner.name(name), e))
    }

    /// Run a function body (the chunk) with no closure and no arguments.
    pub fn call_func(&mut self, func: FuncId) -> Result<Value> {
        let base = self.stack.len();
        self.enter(func, NO_CLOSURE, base, 0)
    }

    /// Call `f` whose `nargs` arguments already sit at `stack[base..]`.
    /// The arguments become the callee's parameter slots in place.
    fn call(&mut self, f: Value, base: usize, nargs: usize) -> Result<Value> {
        match f {
            Value::Func(c) => {
                let func = self.heap.closures[c as usize].func;
                self.enter(func, c, base, nargs)
            }
            Value::Builtin(b) => {
                let mut args = [Value::Nil; 6];
                if nargs > args.len() {
                    bail!("{}: too many arguments ({})", b.name(), nargs);
                }
                args[..nargs].copy_from_slice(&self.stack[base..base + nargs]);
                self.stack.truncate(base);
                self.call_builtin(b, &args[..nargs])
            }
            other => {
                self.stack.truncate(base);
                bail!("calling a non-function: {:?}", other)
            }
        }
    }

    fn enter(&mut self, func: FuncId, closure: ClosureId, base: usize, nargs: usize) -> Result<Value> {
        let f = &self.prog.funcs[func as usize];
        // Extra arguments are dropped, missing ones are nil, and every
        // other slot starts nil.
        self.stack.truncate(base + nargs.min(f.nparams as usize));
        self.stack.resize(base + f.nslots as usize, Value::Nil);
        let saved = (self.base, self.cur_closure);
        self.base = base;
        self.cur_closure = closure;
        let flow = self.exec_block(&f.body);
        self.base = saved.0;
        self.cur_closure = saved.1;
        self.stack.truncate(base);
        Ok(match flow? {
            Flow::Return(v) => v,
            Flow::Normal => Value::Nil,
            Flow::Break => bail!("break outside a loop"),
        })
    }

    // ------------------------------------------------------- statements

    fn exec_block(&mut self, b: &Block) -> Result<Flow> {
        for s in &b.stmts {
            // A poisoned path executes no further statements (the
            // reference's `exec_block_flat`), but its `return` still runs.
            if !self.ok {
                break;
            }
            match self.exec_stmt(s)? {
                Flow::Normal => {}
                other => return Ok(other),
            }
        }
        Ok(match &b.last {
            None => Flow::Normal,
            Some(Last::Break) => Flow::Break,
            Some(Last::Return(None)) => Flow::Return(Value::Nil),
            Some(Last::Return(Some(e))) => Flow::Return(self.eval(e)?),
        })
    }

    fn exec_stmt(&mut self, s: &Stmt) -> Result<Flow> {
        match s {
            Stmt::Local { slot, value } => {
                let v = match value {
                    Some(e) => self.eval(e)?,
                    None => Value::Nil,
                };
                self.stack[self.base + *slot as usize] = v;
            }
            Stmt::SetLocal { slot, value } => {
                let v = self.eval(value)?;
                self.stack[self.base + *slot as usize] = v;
            }
            Stmt::SetGlobal { name, value } => {
                let v = self.eval(value)?;
                self.heap.set_global(*name, v);
            }
            Stmt::SetField { table, key, value } => {
                let t = self.eval(table)?;
                let v = self.eval(value)?;
                let Value::Table(id) = t else {
                    bail!("assigning through a non-table: {:?}", t)
                };
                self.heap.tables[id as usize].set_field(*key, v);
            }
            Stmt::SetIndex { table, index, value } => {
                let t = self.eval(table)?;
                let i = self.eval(index)?;
                let k = index_key(&i)?;
                let v = self.eval(value)?;
                let Value::Table(id) = t else {
                    bail!("assigning through a non-table: {:?}", t)
                };
                let tab = &mut self.heap.tables[id as usize];
                match k {
                    Key::Field(f) => tab.set_field(f, v),
                    Key::Index(i) => tab.set_index(i, v),
                }
            }
            Stmt::Call(e) => {
                self.eval(e)?;
            }
            Stmt::If { arms, els } => {
                for (cond, block) in arms {
                    let c = self.eval(cond)?;
                    if self.truthy(c)? {
                        return self.exec_block(block);
                    }
                }
                if let Some(b) = els {
                    return self.exec_block(b);
                }
            }
            Stmt::For { slot, start, limit, body, unroll } => {
                return self.exec_for(*slot, start, limit, body, *unroll);
            }
        }
        Ok(Flow::Normal)
    }

    fn exec_for(&mut self, slot: u16, start: &Expr, limit: &Expr, body: &Block, unroll: Option<u32>) -> Result<Flow> {
        let a = self.eval(start)?;
        let b = self.eval(limit)?;
        let (Value::Num(a), Value::Num(b)) = (a, b) else {
            bail!("numeric for bounds must be numbers");
        };
        let slot = self.base + slot as usize;
        match (a.to_number(), b.to_number()) {
            (Some(from), Some(to)) => {
                // The reference's `run_for`: `i = i + 1` in P8 (wrapping),
                // until `i <= to` fails or the body leaves the loop.
                let one = P8::from_i16(1);
                let mut i = from;
                while i <= to {
                    self.stack[slot] = Value::Num(Iv::from_number(i));
                    match self.exec_block(body)? {
                        Flow::Break => break,
                        Flow::Return(v) => return Ok(Flow::Return(v)),
                        Flow::Normal => {}
                    }
                    i = i + one;
                }
                Ok(Flow::Normal)
            }
            _ => {
                // The reference's `run_for_symbolic`: unroll `bound` times,
                // each iteration masked by `start + k <= limit` (which may
                // fork), and require a path that never left the loop to
                // have finished when the bound runs out.
                let bound = unroll.ok_or_else(|| {
                    anyhow!("numeric for with symbolic limit has no unroll bound - add one to `unroll_bound`")
                })?;
                for k in 0..bound {
                    let off = self.d.num(P8::from_i16(k as i16));
                    let iv = self.d.arith(Arith::Add, &a, &off)?;
                    let cond = self.d.compare(Cmp::Le, &iv, &b)?;
                    if !cond {
                        return Ok(Flow::Normal);
                    }
                    self.stack[slot] = Value::Num(iv);
                    match self.exec_block(body)? {
                        Flow::Break => return Ok(Flow::Normal),
                        Flow::Return(v) => return Ok(Flow::Return(v)),
                        Flow::Normal => {}
                    }
                }
                let off = self.d.num(P8::from_i16(bound as i16));
                let iv = self.d.arith(Arith::Add, &a, &off)?;
                let over = self.d.compare(Cmp::Le, &iv, &b)?;
                let finished = self.d.not(&over);
                self.ok = self.d.and(&self.ok, &finished);
                Ok(Flow::Normal)
            }
        }
    }

    // ------------------------------------------------------ expressions

    /// Lua truthiness, deciding an undecided boolean on the way (the fork
    /// the reference took at the boolean's creation).
    fn truthy(&mut self, v: Value) -> Result<bool> {
        Ok(match v {
            Value::Nil => false,
            Value::Bool(b) => b,
            Value::UBool(i) => self.decide(i)?,
            _ => true,
        })
    }

    fn decide(&mut self, i: u32) -> Result<bool> {
        if let Some(b) = self.decided[i as usize] {
            return Ok(b);
        }
        let b = self.d.unknown_bool()?;
        self.decided[i as usize] = Some(b);
        Ok(b)
    }

    /// `v` with an undecided boolean decided, for the operations that
    /// look at a boolean's VALUE rather than its truthiness.
    fn resolved(&mut self, v: Value) -> Result<Value> {
        Ok(match v {
            Value::UBool(i) => Value::Bool(self.decide(i)?),
            other => other,
        })
    }

    fn poison(&mut self, why: String) {
        self.ok = false;
        *self.illegal.entry(why).or_default() += 1;
    }

    pub fn eval(&mut self, e: &Expr) -> Result<Value> {
        self.n_eval += 1;
        Ok(match e {
            Expr::Const(v) => *v,
            Expr::Local(slot) => self.stack[self.base + *slot as usize],
            Expr::Upval(i) => self.heap.closures[self.cur_closure as usize].upvals[*i as usize],
            Expr::Global(s) => self.heap.global(*s),
            Expr::Field(t, k) => {
                let t = self.eval(t)?;
                let Value::Table(id) = t else {
                    bail!("reading .{}: indexing a non-table: {:?}", self.prog.interner.name(*k), t)
                };
                self.heap.tables[id as usize].get_field(*k)
            }
            Expr::Index(t, i) => {
                let t = self.eval(t)?;
                let i = self.eval(i)?;
                let k = index_key(&i)?;
                let Value::Table(id) = t else {
                    bail!("indexing a non-table: {:?}", t)
                };
                let tab = &self.heap.tables[id as usize];
                match k {
                    Key::Field(f) => tab.get_field(f),
                    Key::Index(i) => tab.get_index(i),
                }
            }
            Expr::Call(f, args, path) => {
                let fv = self.eval(f)?;
                let base = self.stack.len();
                for a in args {
                    let v = self.eval(a)?;
                    self.stack.push(v);
                }
                self.call(fv, base, args.len()).map_err(|e| anyhow!("calling {}: {:#}", path, e))?
            }
            Expr::Neg(x) => {
                let v = self.eval(x)?;
                let Value::Num(n) = v else { bail!("unary minus on a non-number") };
                Value::Num(self.d.fun1(Fun1::Neg, &n)?)
            }
            Expr::Len(x) => {
                let v = self.eval(x)?;
                let Value::Table(t) = v else { bail!("# of a non-table") };
                let len = self.heap.tables[t as usize].len().ok_or_else(|| {
                    anyhow!(
                        "# of a table this model cannot measure exactly: PICO-8's length \
                         searches the array part's CAPACITY, which depends on rehash history"
                    )
                })? as i16;
                Value::Num(Iv::from_number(P8::from_i16(len)))
            }
            Expr::Not(x) => {
                let v = self.eval(x)?;
                Value::Bool(!self.truthy(v)?)
            }
            Expr::Arith(op, l, r, src) => {
                let a = self.eval(l)?;
                let b = self.eval(r)?;
                match (a, b) {
                    (Value::Num(x), Value::Num(y)) => Value::Num(self.d.arith(*op, &x, &y).map_err(|e| anyhow!("in `{}`: {:#}", src, e))?),
                    _ => {
                        // Lua raises here, so this path is not a legal run.
                        let t = &src[..src.len().min(48)];
                        self.poison(format!("`{}`: arithmetic on {} and {}", t, a.kind(), b.kind()));
                        Value::Num(Iv::from_number(P8::from_i16(0)))
                    }
                }
            }
            Expr::Cmp { op, negate, lhs, rhs, src } => {
                let a = self.eval(lhs)?;
                let b = self.eval(rhs)?;
                let a = self.resolved(a)?;
                let b = self.resolved(b)?;
                let r = match (a, b) {
                    (Value::Num(x), Value::Num(y)) => self.d.compare(*op, &x, &y)?,
                    (Value::Bool(x), Value::Bool(y)) if *op == Cmp::Eq => x == y,
                    (Value::Func(x), Value::Func(y)) if *op == Cmp::Eq => {
                        if x != y {
                            bail!(
                                "comparing two closures: PICO-8 caches them on (prototype, \
                                 upvalue cells) and this model cannot tell whether these are one object"
                            );
                        }
                        true
                    }
                    _ if *op == Cmp::Eq => a == b,
                    _ => {
                        let t = &src[..src.len().min(48)];
                        self.poison(format!("`{}`: comparison of {} and {}", t, a.kind(), b.kind()));
                        false
                    }
                };
                Value::Bool(if *negate { !r } else { r })
            }
            Expr::And(l, r) => {
                let a = self.eval(l)?;
                if self.truthy(a)? {
                    self.eval(r)?
                } else {
                    a
                }
            }
            Expr::Or(l, r) => {
                let a = self.eval(l)?;
                if self.truthy(a)? {
                    a
                } else {
                    self.eval(r)?
                }
            }
            Expr::Function(id) => {
                let f = &self.prog.funcs[*id as usize];
                let mut upvals = Vec::with_capacity(f.captures.len());
                for src in &f.captures {
                    upvals.push(match src {
                        CaptureSrc::Local(slot) => self.stack[self.base + *slot as usize],
                        CaptureSrc::Upval(i) => self.heap.closures[self.cur_closure as usize].upvals[*i as usize],
                    });
                }
                Value::Func(self.heap.new_closure(*id, upvals))
            }
            Expr::Table(fields) => {
                let id = self.heap.new_table();
                for (k, e) in fields {
                    let v = self.eval(e)?;
                    let tab = &mut self.heap.tables[id as usize];
                    match k {
                        Some(s) => tab.set_field(*s, v),
                        None => tab.arr.push(v),
                    }
                }
                Value::Table(id)
            }
        })
    }

    // --------------------------------------------------------- builtins

    fn call_builtin(&mut self, b: Builtin, args: &[Value]) -> Result<Value> {
        let name = b.name();
        let num = |i: usize| -> Result<Iv> {
            let v = args.get(i).ok_or_else(|| {
                anyhow!("{}: needs at least {} arguments, got {}", name, i + 1, args.len())
            })?;
            match v {
                Value::Num(n) => Ok(*n),
                other => bail!("{}: expected a number, got {:?}", name, other),
            }
        };
        Ok(match b {
            Builtin::Abs => Value::Num(self.d.fun1(Fun1::Abs, &num(0)?)?),
            Builtin::Flr => Value::Num(self.d.fun1(Fun1::Flr, &num(0)?)?),
            Builtin::Sin => Value::Num(self.d.fun1(Fun1::Sin, &num(0)?)?),
            Builtin::Mget => {
                let (a, b) = (num(0)?, num(1)?);
                match (a.to_number(), b.to_number()) {
                    (Some(x), Some(y)) => {
                        let t = self.cart.mget(x, y)?;
                        Value::Num(Iv::from_number(P8::from_i16(t as i16)))
                    }
                    _ => Value::Num(self.d.mget(&a, &b)?),
                }
            }
            Builtin::Fget => {
                let (a, b) = (num(0)?, num(1)?);
                match (a.to_number(), b.to_number()) {
                    (Some(x), Some(y)) => Value::Bool(self.cart.fget(x, y)?),
                    _ => bail!("fget with unknown arguments is not modelled"),
                }
            }
            Builtin::TileFlagAt => {
                let (x, y, w, h, fl) = (num(0)?, num(1)?, num(2)?, num(3)?, num(4)?);
                let gi = |v: P8| v.as_i16().ok_or_else(|| anyhow!("tile_flag_at: non-integer"));
                // THE FLAG DECIDES FIRST, before the coordinates (see the
                // reference for why).
                let Some(fi) = fl.to_number().map(gi).transpose()? else {
                    bail!("tile_flag_at with an unknown flag");
                };
                if fi != 0 {
                    if self.room_has_flag(fi)? {
                        bail!(
                            "tile_flag_at with flag {} in a room that CONTAINS that flag: \
                             only flag 0 (solid) is modelled",
                            fi
                        );
                    }
                    Value::Bool(false)
                } else {
                    match (x.to_number(), y.to_number(), w.to_number(), h.to_number()) {
                        (Some(x), Some(y), Some(w), Some(h)) => {
                            Value::Bool(self.cache.solid_at(&self.cart, gi(x)?, gi(y)?, gi(w)?, gi(h)?)?)
                        }
                        _ => Value::Bool(self.d.tile_flag_at(&x, &y, &w, &h, &fl)?),
                    }
                }
            }
            Builtin::Print | Builtin::UPrint | Builtin::HintNormalize => Value::Nil,
            Builtin::Printh => {
                let first = match args.first() {
                    Some(v) => Some(self.resolved(*v)?),
                    None => None,
                };
                let text = match first.as_ref() {
                    None | Some(Value::Nil) => "[nil]".to_string(),
                    Some(Value::Str(s)) => self.prog.interner.name(*s).to_string(),
                    Some(Value::Bool(b)) => b.to_string(),
                    Some(Value::Num(n)) => match n.to_number() {
                        Some(v) => fmt_p8(v),
                        None => bail!("printh of a symbolic number"),
                    },
                    Some(other) => bail!("printh of {:?}", other),
                };
                self.prints.push(text);
                Value::Nil
            }
            Builtin::ArrayTableDropLast => {
                let Some(Value::Table(t)) = args.first() else {
                    bail!("__array_table_drop_last on a non-table: {:?}", args.first())
                };
                let tab = &mut self.heap.tables[*t as usize];
                let n = tab.len().ok_or_else(|| {
                    anyhow!("__array_table_drop_last on a table whose length is not exact")
                })?;
                if n == 0 {
                    bail!("__array_table_drop_last on an empty table");
                }
                tab.set_index(n as i16, Value::Nil);
                Value::Nil
            }
            Builtin::SplitByFlr | Builtin::SplitAt => {
                let x = num(0)?;
                if !self.d.is_interval(&x) {
                    args[0]
                } else {
                    // `fork_flr`'s validity and `span_ok` are both `true`
                    // under RefDomain (it forks on the actual span), so
                    // neither guard nor `ok` moves.
                    let (v, _valid) = self.d.fork_flr(&x);
                    let _premise = self.d.span_ok(&x);
                    Value::Num(v)
                }
            }
            Builtin::NewUnknownBoolean => {
                self.decided.push(None);
                Value::UBool((self.decided.len() - 1) as u32)
            }
            Builtin::Min => Value::Num(self.d.fun2(Fun2::Min, &num(0)?, &num(1)?)?),
            Builtin::Max => Value::Num(self.d.fun2(Fun2::Max, &num(0)?, &num(1)?)?),
            Builtin::Unsupported(_) => bail!("unsupported builtin {:?}", name),
        })
    }

    /// Does any tile in the loaded room carry this flag bit? A copy of the
    /// reference's `room_has_flag`.
    fn room_has_flag(&mut self, flag: i16) -> Result<bool> {
        let Value::Table(rt) = self.heap.global(self.sym_room) else {
            bail!("tile_flag_at: no `room` global to check the flag against");
        };
        let room = &self.heap.tables[rt as usize];
        let coord = |k: Sym, what: &str| -> Result<i16> {
            match room.get_field(k) {
                Value::Num(n) => n
                    .to_number()
                    .and_then(|v| v.as_i16())
                    .ok_or_else(|| anyhow!("tile_flag_at: room.{} is not a known integer", what)),
                other => bail!("tile_flag_at: room.{} is {:?}", what, other),
            }
        };
        let (rx, ry) = (coord(self.sym_x, "x")?, coord(self.sym_y, "y")?);
        if let Some(v) = self.room_flags.get(&(rx, ry, flag)) {
            return Ok(*v);
        }
        let mut found = false;
        'scan: for i in 0..16i16 {
            for j in 0..16i16 {
                let t = self.cart.mget(P8::from_i16(rx * 16 + i), P8::from_i16(ry * 16 + j))?;
                if self.cart.fget(P8::from_i16(t as i16), P8::from_i16(flag))? {
                    found = true;
                    break 'scan;
                }
            }
        }
        self.room_flags.insert((rx, ry, flag), found);
        Ok(found)
    }
}

enum Key {
    Field(Sym),
    Index(i16),
}

/// The key a `[..]` index denotes. Concrete or refused, as in the
/// reference.
fn index_key(v: &Value) -> Result<Key> {
    Ok(match v {
        Value::Str(s) => Key::Field(*s),
        Value::Num(n) => {
            let c = n.to_number().ok_or_else(|| anyhow!("refusing to use an unknown value as a table index"))?;
            let i = c.as_i16().ok_or_else(|| anyhow!("fractional table index"))?;
            Key::Index(i)
        }
        other => bail!("unsupported table key {:?}", other),
    })
}

/// A pico-8 number the way `printh` renders it (copy of the reference's).
fn fmt_p8(v: P8) -> String {
    if let Some(i) = v.as_i16() {
        return format!("{}", i);
    }
    let raw = v.as_raw_u32() as i32 as f64 / 65536.0;
    let s = format!("{:.4}", raw);
    s.trim_end_matches('0').trim_end_matches('.').to_string()
}
