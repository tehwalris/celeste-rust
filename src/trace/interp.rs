//! The AST interpreter. One implementation, two domains.
//!
//! Control flow is ordinary recursion because the AST says where things
//! join: the end of an `if` statement is right there in the syntax, so
//! there is no post-dominator analysis and no CFG. A call is recursion
//! too - hand the callee the state, get the state back - so nothing is
//! inlined because nothing has to be.
//!
//! ## Where it fans out
//!
//! A statement returns a LIST of outcomes, because a branch the domain
//! cannot decide runs both arms and they do not always merge back (an
//! object destroyed on one side changes the shape). An EXPRESSION threads
//! a single state, and refuses loudly if a call inside it fans out. That
//! is a real restriction and it is deliberate for now: in this program
//! shape divergence happens in statement position (`del(objects, obj)`),
//! and a loud refusal is the right way to find out if that is ever wrong.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;
use full_moon::tokenizer::{Symbol, TokenType};

use crate::pico8_num::Pico8Num as P8;

use super::domain::{refuse_unknown, Arith, Cmp, Domain, Fun1, Fun2};
use super::heap::{BodyId, Value};
use super::state::{merge, State};

pub enum Flow<D: Domain> {
    Normal,
    Break,
    Return(Value<D>),
}

impl<D: Domain> Clone for Flow<D> {
    fn clone(&self) -> Self {
        match self {
            Flow::Normal => Flow::Normal,
            Flow::Break => Flow::Break,
            Flow::Return(v) => Flow::Return(v.clone()),
        }
    }
}

impl<D: Domain> Flow<D> {
    fn is_normal(&self) -> bool {
        matches!(self, Flow::Normal)
    }
    /// Two outcomes can only merge if they are doing the same thing.
    fn same_kind(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Flow::Normal, Flow::Normal)
                | (Flow::Break, Flow::Break)
                | (Flow::Return(_), Flow::Return(_))
        )
    }
}

/// A statement's possible outcomes.
pub type Outcome<D> = Vec<(State<D>, Flow<D>)>;

pub struct Interp<'a, D: Domain> {
    pub d: D,
    /// Function bodies, referred to by id from closures - the AST outlives
    /// the heap, so the heap stores an index rather than a reference.
    bodies: Vec<&'a ast::FunctionBody>,
    /// The cart, for `mget`/`fget`. Optional so the unit tests can run
    /// programs that never touch the map.
    pub cart: Option<std::sync::Arc<celeste_core::cart_data::CartData>>,
}

impl<'a, D: Domain> Interp<'a, D> {
    pub fn new(d: D) -> Self {
        Interp { d, bodies: Vec::new(), cart: None }
    }

    fn intern_body(&mut self, b: &'a ast::FunctionBody) -> BodyId {
        self.bodies.push(b);
        (self.bodies.len() - 1) as BodyId
    }

    // ------------------------------------------------------------ blocks

    pub fn exec_block(&mut self, block: &'a ast::Block, st: State<D>) -> Result<Outcome<D>> {
        let mut live: Outcome<D> = vec![(st, Flow::Normal)];
        for stmt in block.stmts() {
            let mut next: Outcome<D> = Vec::new();
            for (s, f) in live {
                if !f.is_normal() {
                    next.push((s, f));
                    continue;
                }
                next.extend(self.exec_stmt(stmt, s)?);
            }
            live = next;
            if live.iter().all(|(_, f)| !f.is_normal()) {
                break;
            }
        }
        if let Some(last) = block.last_stmt() {
            let mut next: Outcome<D> = Vec::new();
            for (s, f) in live {
                if !f.is_normal() {
                    next.push((s, f));
                    continue;
                }
                next.extend(self.exec_last(last, s)?);
            }
            live = next;
        }
        Ok(live)
    }

    fn exec_last(&mut self, last: &'a ast::LastStmt, st: State<D>) -> Result<Outcome<D>> {
        Ok(match last {
            ast::LastStmt::Break(_) => vec![(st, Flow::Break)],
            ast::LastStmt::Return(r) => {
                let mut s = st;
                let mut v = Value::Nil;
                if let Some(e) = r.returns().iter().next() {
                    let (s2, v2) = self.eval(e, s)?;
                    s = s2;
                    v = v2;
                }
                if r.returns().len() > 1 {
                    bail!("multiple return values are not supported");
                }
                vec![(s, Flow::Return(v))]
            }
            other => bail!("unsupported last statement {:?}", other),
        })
    }

    // -------------------------------------------------------- statements

    fn exec_stmt(&mut self, stmt: &'a ast::Stmt, st: State<D>) -> Result<Outcome<D>> {
        match stmt {
            ast::Stmt::LocalAssignment(la) => {
                let mut s = st;
                let names: Vec<_> = la.names().iter().collect();
                let exprs: Vec<_> = la.expressions().iter().collect();
                for (i, name) in names.iter().enumerate() {
                    let v = match exprs.get(i) {
                        Some(e) => {
                            let (s2, v) = self.eval(e, s)?;
                            s = s2;
                            v
                        }
                        None => Value::Nil,
                    };
                    let n = ident(name)?;
                    s.heap.declare(s.scope, &n, v);
                }
                Ok(vec![(s, Flow::Normal)])
            }
            ast::Stmt::Assignment(a) => {
                let var = a
                    .variables()
                    .iter()
                    .next()
                    .ok_or_else(|| anyhow!("assignment with no variable"))?;
                if a.variables().len() > 1 {
                    bail!("multiple assignment is not supported");
                }
                let e = a
                    .expressions()
                    .iter()
                    .next()
                    .ok_or_else(|| anyhow!("assignment with no expression"))?;
                let (s, v) = self.eval(e, st)?;
                let s = self.assign_var(var, v, s)?;
                Ok(vec![(s, Flow::Normal)])
            }
            ast::Stmt::FunctionCall(call) => {
                let (s, _) = self.eval_call(call, st)?;
                Ok(vec![(s, Flow::Normal)])
            }
            ast::Stmt::FunctionDeclaration(f) => {
                let body = self.intern_body(f.body());
                let mut s = st;
                let v = Value::Func { body, env: s.scope };
                let name = f.name().to_string().trim().to_string();
                if name.contains('.') || name.contains(':') {
                    bail!("qualified function names are not supported: {:?}", name);
                }
                if !s.heap.assign(s.scope, &name, v.clone()) {
                    let g = s.globals;
                    s.heap.tables.get_mut(&g).unwrap().hash.insert(name, v);
                }
                Ok(vec![(s, Flow::Normal)])
            }
            ast::Stmt::If(iff) => self.exec_if(iff, st),
            ast::Stmt::NumericFor(f) => self.exec_for(f, st),
            other => bail!("unsupported statement {:?}", other),
        }
    }

    /// `if` is where the tracer stops being an interpreter. A condition the
    /// domain can decide just takes its arm - most of them, because the
    /// heap is concrete. One it cannot runs BOTH arms from a copy of the
    /// state and merges the results.
    fn exec_if(&mut self, iff: &'a ast::If, st: State<D>) -> Result<Outcome<D>> {
        // `elseif` chains are the same thing nested, so build the arms in
        // order and recurse over the tail.
        let mut arms: Vec<(&'a ast::Expression, &'a ast::Block)> =
            vec![(iff.condition(), iff.block())];
        if let Some(eis) = iff.else_if() {
            for ei in eis {
                arms.push((ei.condition(), ei.block()));
            }
        }
        self.exec_arms(&arms, iff.else_block(), st)
    }

    fn exec_arms(
        &mut self,
        arms: &[(&'a ast::Expression, &'a ast::Block)],
        els: Option<&'a ast::Block>,
        st: State<D>,
    ) -> Result<Outcome<D>> {
        let Some(((cond_e, blk), rest)) = arms.split_first() else {
            return Ok(match els {
                Some(b) => self.exec_block(b, st)?,
                None => vec![(st, Flow::Normal)],
            });
        };
        let (s, cv) = self.eval(cond_e, st)?;
        let cond = self.truthy(&cv);
        match self.d.decide(&cond) {
            Some(true) => self.exec_block(blk, s),
            Some(false) => self.exec_arms(rest, els, s),
            None => {
                let t = self.exec_block(blk, s.clone())?;
                let f = self.exec_arms(rest, els, s)?;
                self.merge_outcomes(&cond, t, f)
            }
        }
    }

    /// Put two arms' outcomes back together where the shapes allow it.
    ///
    /// Only the simple case merges for now: one outcome each, doing the
    /// same thing. Anything else is concatenated, which is CORRECT but
    /// grows the frontier - and if that ever grows without bound the fix
    /// belongs here, where it is visible, rather than in a heuristic
    /// somewhere upstream.
    fn merge_outcomes(
        &mut self,
        cond: &D::Bool,
        t: Outcome<D>,
        f: Outcome<D>,
    ) -> Result<Outcome<D>> {
        if t.len() == 1 && f.len() == 1 && t[0].1.same_kind(&f[0].1) {
            let mut t = t;
            let mut f = f;
            let (ts, tf) = t.pop().unwrap();
            let (fs, ff) = f.pop().unwrap();
            if let Some(m) = merge(&mut self.d, cond, ts.clone(), fs.clone())? {
                let flow = match (&tf, &ff) {
                    (Flow::Return(a), Flow::Return(b)) => match (a, b) {
                        (Value::Num(x), Value::Num(y)) => {
                            Flow::Return(Value::Num(self.d.sel_num(cond, x, y)))
                        }
                        (Value::Bool(x), Value::Bool(y)) => {
                            Flow::Return(Value::Bool(self.d.sel_bool(cond, x, y)))
                        }
                        (x, y) if x == y => Flow::Return(x.clone()),
                        _ => return Ok(vec![(ts, tf), (fs, ff)]),
                    },
                    (Flow::Break, Flow::Break) => Flow::Break,
                    _ => Flow::Normal,
                };
                return Ok(vec![(m, flow)]);
            }
            return Ok(vec![(ts, tf), (fs, ff)]);
        }
        let mut out = t;
        out.extend(f);
        Ok(out)
    }

    /// Numeric `for`. The bounds must be CONCRETE, which they are almost
    /// everywhere because the heap is: `for i=1,count(objects)` reads a
    /// real table. The exceptions are the pixel-steppers in `move_x`/
    /// `move_y`; those get a bound and a guard, which is not built yet.
    fn exec_for(&mut self, f: &'a ast::NumericFor, st: State<D>) -> Result<Outcome<D>> {
        if f.step().is_some() {
            bail!("numeric for with an explicit step is not supported");
        }
        let (s, from) = self.eval(f.start(), st)?;
        let (s, to) = self.eval(f.end(), s)?;
        let (Value::Num(from), Value::Num(to)) = (&from, &to) else {
            bail!("numeric for bounds must be numbers");
        };
        let from = self
            .d
            .as_const(from)
            .ok_or_else(|| refuse_unknown("a numeric for's start"))?;
        let to = self
            .d
            .as_const(to)
            .ok_or_else(|| refuse_unknown("a numeric for's limit"))?;
        let name = ident(f.index_variable())?;

        // A `break` ends the LOOP for that state, not the state: it moves
        // to `done` and carries on after the loop. Getting this wrong the
        // first time made `break` a no-op, so `foreach`'s bounded walk ran
        // its full 32767 iterations instead of stopping.
        let mut running: Outcome<D> = vec![(s, Flow::Normal)];
        let mut done: Outcome<D> = Vec::new();
        let name = &name;
        let one = P8::from_i16(1);
        let mut i = from;
        while i <= to && running.iter().any(|(_, f)| f.is_normal()) {
            let mut next: Outcome<D> = Vec::new();
            for (mut cur, fl) in running {
                if !fl.is_normal() {
                    done.push((cur, fl));
                    continue;
                }
                let body_scope = cur.heap.new_scope(Some(cur.scope));
                let iv = self.d.num(i);
                cur.heap.declare(body_scope, name, Value::Num(iv));
                let outer = cur.scope;
                cur.scope = body_scope;
                for (mut s2, f2) in self.exec_block(f.block(), cur)? {
                    s2.scope = outer;
                    match f2 {
                        Flow::Break => done.push((s2, Flow::Normal)),
                        Flow::Return(v) => done.push((s2, Flow::Return(v))),
                        Flow::Normal => next.push((s2, Flow::Normal)),
                    }
                }
            }
            running = next;
            i = i + one;
        }
        // Whatever was still going when the bound ran out just continues.
        done.extend(running.into_iter().map(|(s, _)| (s, Flow::Normal)));
        Ok(done)
    }

    // ------------------------------------------------------- expressions

    fn truthy(&mut self, v: &Value<D>) -> D::Bool {
        // Lua: only nil and false are falsy. So a number condition is
        // statically TRUE, which removes most would-be branches before
        // the domain is even asked.
        match v {
            Value::Nil => self.d.boolean(false),
            Value::Bool(b) => b.clone(),
            _ => self.d.boolean(true),
        }
    }

    pub fn eval(
        &mut self,
        e: &'a ast::Expression,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        match e {
            ast::Expression::Number(n) => {
                let TokenType::Number { text } = n.token_type() else {
                    bail!("expected a number literal");
                };
                let v: P8 = text.parse()?;
                let n = self.d.num(v);
                Ok((st, Value::Num(n)))
            }
            ast::Expression::String(s) => {
                let TokenType::StringLiteral { literal, .. } = s.token_type() else {
                    bail!("expected a string literal");
                };
                Ok((st, Value::Str(literal.to_string().into())))
            }
            ast::Expression::Symbol(sym) => {
                let TokenType::Symbol { symbol } = sym.token_type() else {
                    bail!("expected a symbol");
                };
                Ok(match symbol {
                    Symbol::True => {
                        let b = self.d.boolean(true);
                        (st, Value::Bool(b))
                    }
                    Symbol::False => {
                        let b = self.d.boolean(false);
                        (st, Value::Bool(b))
                    }
                    Symbol::Nil => (st, Value::Nil),
                    other => bail!("unsupported symbol {:?}", other),
                })
            }
            ast::Expression::Parentheses { expression, .. } => self.eval(expression, st),
            ast::Expression::UnaryOperator { unop, expression } => {
                let (st, v) = self.eval(expression, st)?;
                Ok(match unop {
                    ast::UnOp::Minus(_) => {
                        let Value::Num(n) = v else { bail!("unary minus on a non-number") };
                        let r = self.d.fun1(Fun1::Neg, &n)?;
                        (st, Value::Num(r))
                    }
                    ast::UnOp::Hash(_) => {
                        // Concrete, because the heap is. This is why
                        // `for i=1,#t` needs no unrolling heuristic.
                        let Value::Table(t) = v else { bail!("# of a non-table") };
                        let len = st.heap.tables[&t].arr.len() as i16;
                        let n = self.d.num(P8::from_i16(len));
                        (st, Value::Num(n))
                    }
                    ast::UnOp::Not(_) => {
                        let t = self.truthy(&v);
                        let r = self.d.not(&t);
                        (st, Value::Bool(r))
                    }
                    other => bail!("unsupported unary operator {:?}", other),
                })
            }
            ast::Expression::BinaryOperator { lhs, binop, rhs } => {
                self.eval_binop(lhs, binop, rhs, st)
            }
            ast::Expression::Var(v) => match v {
                ast::Var::Name(t) => {
                    let n = ident(t)?;
                    let val = self.read_name(&n, &st);
                    Ok((st, val))
                }
                ast::Var::Expression(ve) => {
                    let suffixes: Vec<_> = ve.suffixes().collect();
                    self.walk_suffixes(ve.prefix(), &suffixes, st)
                }
                other => bail!("unsupported var {:?}", other),
            },
            ast::Expression::FunctionCall(call) => self.eval_call(call, st),
            ast::Expression::Function((_, body)) => {
                let id = self.intern_body(body);
                let env = st.scope;
                Ok((st, Value::Func { body: id, env }))
            }
            ast::Expression::TableConstructor(t) => {
                let mut st = st;
                let id = st.heap.new_table();
                for field in t.fields() {
                    match field {
                        ast::Field::NameKey { key, value, .. } => {
                            let k = ident(key)?;
                            let (st2, v) = self.eval(value, st)?;
                            st = st2;
                            st.heap.tables.get_mut(&id).unwrap().hash.insert(k, v);
                        }
                        ast::Field::NoKey(e) => {
                            let (st2, v) = self.eval(e, st)?;
                            st = st2;
                            st.heap.tables.get_mut(&id).unwrap().arr.push(v);
                        }
                        other => bail!("unsupported table field {:?}", other),
                    }
                }
                Ok((st, Value::Table(id)))
            }
            other => bail!("unsupported expression {:?}", other),
        }
    }

    fn eval_binop(
        &mut self,
        lhs: &'a ast::Expression,
        binop: &ast::BinOp,
        rhs: &'a ast::Expression,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        // `and`/`or` short-circuit and return VALUES, not booleans.
        if matches!(binop, ast::BinOp::And(_) | ast::BinOp::Or(_)) {
            let is_and = matches!(binop, ast::BinOp::And(_));
            let (st, a) = self.eval(lhs, st)?;
            let t = self.truthy(&a);
            return match self.d.decide(&t) {
                Some(x) if x == is_and => self.eval(rhs, st),
                Some(_) => Ok((st, a)),
                None => {
                    let (st, b) = self.eval(rhs, st)?;
                    let (kept, other) = if is_and { (b, a) } else { (a, b) };
                    match (&kept, &other) {
                        (Value::Num(x), Value::Num(y)) => {
                            let r = self.d.sel_num(&t, x, y);
                            Ok((st, Value::Num(r)))
                        }
                        (Value::Bool(x), Value::Bool(y)) => {
                            let r = self.d.sel_bool(&t, x, y);
                            Ok((st, Value::Bool(r)))
                        }
                        _ => bail!(
                            "and/or on an undecided condition needs both sides to be the \
                             same kind of value ({:?} and {:?})",
                            kept, other
                        ),
                    }
                }
            };
        }
        let (st, a) = self.eval(lhs, st)?;
        let (st, b) = self.eval(rhs, st)?;
        let arith = |op| -> Option<Arith> { Some(op) };
        let (num_op, cmp_op) = match binop {
            ast::BinOp::Plus(_) => (arith(Arith::Add), None),
            ast::BinOp::Minus(_) => (arith(Arith::Sub), None),
            ast::BinOp::Star(_) => (arith(Arith::Mul), None),
            ast::BinOp::Slash(_) => (arith(Arith::Div), None),
            ast::BinOp::Percent(_) => (arith(Arith::Rem), None),
            ast::BinOp::LessThan(_) => (None, Some(Cmp::Lt)),
            ast::BinOp::LessThanEqual(_) => (None, Some(Cmp::Le)),
            ast::BinOp::GreaterThan(_) => (None, Some(Cmp::Gt)),
            ast::BinOp::GreaterThanEqual(_) => (None, Some(Cmp::Ge)),
            ast::BinOp::TwoEqual(_) => (None, Some(Cmp::Eq)),
            ast::BinOp::TildeEqual(_) => (None, Some(Cmp::Eq)),
            other => bail!("unsupported binary operator {:?}", other),
        };
        if let Some(op) = num_op {
            let (Value::Num(x), Value::Num(y)) = (&a, &b) else {
                bail!("arithmetic on non-numbers: {:?} and {:?}", a, b)
            };
            let r = self.d.arith(op, x, y)?;
            return Ok((st, Value::Num(r)));
        }
        let op = cmp_op.unwrap();
        let r = match (&a, &b) {
            (Value::Num(x), Value::Num(y)) => self.d.compare(op, x, y)?,
            // Equality on anything else is structural and decidable now,
            // because everything but numbers and booleans is concrete.
            _ if op == Cmp::Eq => {
                let same = a == b;
                self.d.boolean(same)
            }
            _ => bail!("comparison of {:?} and {:?}", a, b),
        };
        let r = if matches!(binop, ast::BinOp::TildeEqual(_)) {
            self.d.not(&r)
        } else {
            r
        };
        Ok((st, Value::Bool(r)))
    }

    /// Read a name: the scope chain first, then globals. A name that is
    /// nowhere is `nil`, as in Lua.
    fn read_name(&mut self, name: &str, st: &State<D>) -> Value<D> {
        if let Some(v) = st.heap.lookup(st.scope, name) {
            return v.clone();
        }
        st.heap.tables[&st.globals]
            .hash
            .get(name)
            .cloned()
            .unwrap_or(Value::Nil)
    }

    fn eval_prefix(
        &mut self,
        p: &'a ast::Prefix,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        match p {
            ast::Prefix::Name(t) => {
                let n = ident(t)?;
                let v = self.read_name(&n, &st);
                Ok((st, v))
            }
            ast::Prefix::Expression(e) => self.eval(e, st),
            other => bail!("unsupported prefix {:?}", other),
        }
    }

    /// The key a `[..]` index denotes. It has to be CONCRETE - the heap
    /// cannot be indexed by something symbolic, and refusing is the whole
    /// point rather than a limitation.
    fn index_key(&mut self, v: &Value<D>) -> Result<Key> {
        Ok(match v {
            Value::Str(s) => Key::Field(s.to_string()),
            Value::Num(n) => {
                let c = self
                    .d
                    .as_const(n)
                    .ok_or_else(|| refuse_unknown("a table index"))?;
                let i = c.as_i16().ok_or_else(|| anyhow!("fractional table index"))?;
                Key::Index(i)
            }
            other => bail!("unsupported table key {:?}", other),
        })
    }

    fn get_key(&mut self, tab: &Value<D>, k: &Key, st: &State<D>) -> Result<Value<D>> {
        let Value::Table(t) = tab else {
            bail!("indexing a non-table: {:?}", tab)
        };
        let table = &st.heap.tables[t];
        Ok(match k {
            Key::Field(f) => table.hash.get(f).cloned().unwrap_or(Value::Nil),
            Key::Index(i) => {
                if *i >= 1 && (*i as usize) <= table.arr.len() {
                    table.arr[*i as usize - 1].clone()
                } else {
                    Value::Nil
                }
            }
        })
    }

    fn set_key(&mut self, tab: &Value<D>, k: Key, v: Value<D>, st: &mut State<D>) -> Result<()> {
        let Value::Table(t) = tab else {
            bail!("assigning through a non-table: {:?}", tab)
        };
        let table = st.heap.tables.get_mut(t).unwrap();
        match k {
            Key::Field(f) => {
                table.hash.insert(f, v);
            }
            Key::Index(i) => {
                if i >= 1 && (i as usize) <= table.arr.len() {
                    table.arr[i as usize - 1] = v;
                } else if i as usize == table.arr.len() + 1 {
                    table.arr.push(v);
                } else {
                    bail!("array index {} is past the end of a {}-element table", i, table.arr.len());
                }
            }
        }
        Ok(())
    }

    /// Walk a prefix and its suffixes as an rvalue.
    fn walk_suffixes(
        &mut self,
        p: &'a ast::Prefix,
        suffixes: &[&'a ast::Suffix],
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        let (mut st, mut cur) = self.eval_prefix(p, st)?;
        // A readable path, so "calling a non-function: nil" says WHICH
        // nil. Costs a string per suffix at trace time and nothing at run
        // time, and it is the difference between a five-minute diagnosis
        // and an hour of bisecting Lua.
        let mut path = match p {
            ast::Prefix::Name(t) => ident(t).unwrap_or_else(|_| "?".into()),
            _ => "(expr)".to_string(),
        };
        for suffix in suffixes {
            match suffix {
                ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                    let k = Key::Field(ident(name)?);
                    path = format!("{}.{}", path, ident(name)?);
                    cur = self.get_key(&cur, &k, &st)?;
                }
                ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                    let (st2, iv) = self.eval(expression, st)?;
                    st = st2;
                    let k = self.index_key(&iv)?;
                    cur = self.get_key(&cur, &k, &st)?;
                }
                ast::Suffix::Call(ast::Call::AnonymousCall(ast::FunctionArgs::Parentheses {
                    arguments,
                    ..
                })) => {
                    let mut args = Vec::new();
                    for a in arguments {
                        let (st2, v) = self.eval(a, st)?;
                        st = st2;
                        args.push(v);
                    }
                    let (st2, v) = self
                        .call_value(cur, args, st)
                        .map_err(|e| anyhow!("calling {}: {:#}", path, e))?;
                    st = st2;
                    cur = v;
                    path = format!("{}()", path);
                }
                other => bail!("unsupported suffix {:?}", other),
            }
        }
        Ok((st, cur))
    }

    fn assign_var(&mut self, var: &'a ast::Var, v: Value<D>, st: State<D>) -> Result<State<D>> {
        match var {
            ast::Var::Name(t) => {
                let name = ident(t)?;
                let mut st = st;
                if !st.heap.assign(st.scope, &name, v.clone()) {
                    let g = st.globals;
                    st.heap.tables.get_mut(&g).unwrap().hash.insert(name, v);
                }
                Ok(st)
            }
            ast::Var::Expression(e) => {
                let suffixes: Vec<_> = e.suffixes().collect();
                let Some((last, init)) = suffixes.split_last() else {
                    bail!("assignment target with no suffixes")
                };
                let (mut st, target) = self.walk_suffixes(e.prefix(), init, st)?;
                let k = match last {
                    ast::Suffix::Index(ast::Index::Dot { name, .. }) => Key::Field(ident(name)?),
                    ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                        let (st2, iv) = self.eval(expression, st)?;
                        st = st2;
                        self.index_key(&iv)?
                    }
                    other => bail!("cannot assign through {:?}", other),
                };
                self.set_key(&target, k, v, &mut st)?;
                Ok(st)
            }
            other => bail!("unsupported assignment target {:?}", other),
        }
    }

    fn eval_call(
        &mut self,
        call: &'a ast::FunctionCall,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        let suffixes: Vec<_> = call.suffixes().collect();
        self.walk_suffixes(call.prefix(), &suffixes, st)
    }

    /// A call is RECURSION - hand the callee the state, get it back. There
    /// is no inlining because there is nothing to inline into.
    fn call_value(
        &mut self,
        f: Value<D>,
        args: Vec<Value<D>>,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        match f {
            Value::Func { body, env } => {
                let mut st = st;
                let frame = st.heap.new_scope(Some(env));
                let b = self.bodies[body as usize];
                for (i, p) in b.parameters().iter().enumerate() {
                    let name = match p {
                        ast::Parameter::Name(t) => ident(t)?,
                        other => bail!("unsupported parameter {:?}", other),
                    };
                    st.heap.declare(frame, &name, args.get(i).cloned().unwrap_or(Value::Nil));
                }
                let outer = st.scope;
                st.scope = frame;
                let out = self.exec_block(b.block(), st)?;
                if out.len() != 1 {
                    // An expression threads ONE state, so a call that
                    // ended in several is a shape divergence inside an
                    // expression. Loud, because the alternative is picking
                    // one arbitrarily.
                    bail!(
                        "a call fanned out into {} states - shape divergence inside an                          expression is not supported yet",
                        out.len()
                    );
                }
                let (mut s, flow) = out.into_iter().next().unwrap();
                s.scope = outer;
                Ok(match flow {
                    Flow::Return(v) => (s, v),
                    Flow::Normal => (s, Value::Nil),
                    Flow::Break => bail!("break outside a loop"),
                })
            }
            Value::Builtin(name) => self.call_builtin(name, args, st),
            other => bail!("calling a non-function: {:?}", other),
        }
    }

    fn call_builtin(
        &mut self,
        name: &'static str,
        args: Vec<Value<D>>,
        st: State<D>,
    ) -> Result<(State<D>, Value<D>)> {
        let num = |v: &Value<D>| -> Result<D::Num> {
            match v {
                Value::Num(n) => Ok(n.clone()),
                other => bail!("{}: expected a number, got {:?}", name, other),
            }
        };
        Ok(match name {
            "abs" | "flr" | "sin" => {
                let f = match name {
                    "abs" => Fun1::Abs,
                    "flr" => Fun1::Flr,
                    _ => Fun1::Sin,
                };
                let a = num(&args[0])?;
                let r = self.d.fun1(f, &a)?;
                (st, Value::Num(r))
            }
            // The map is DATA, not code, and it is concrete - so a
            // lookup with concrete coordinates folds to a constant here
            // exactly as it would in the interpreter. Only a symbolic
            // coordinate would need the graph, and that is `zn_mget`'s
            // job in the emitted kernel rather than the tracer's.
            "mget" | "fget" => {
                let cart = self
                    .cart
                    .clone()
                    .ok_or_else(|| anyhow!("{}: no cart loaded", name))?;
                let (a, b) = (num(&args[0])?, num(&args[1])?);
                let (x, y) = (
                    self.d.as_const(&a).ok_or_else(|| refuse_unknown("an mget/fget x"))?,
                    self.d.as_const(&b).ok_or_else(|| refuse_unknown("an mget/fget y"))?,
                );
                if name == "mget" {
                    let t = cart.mget(x, y)?;
                    let n = self.d.num(P8::from_i16(t as i16));
                    (st, Value::Num(n))
                } else {
                    let r = cart.fget(x, y)?;
                    let b = self.d.boolean(r);
                    (st, Value::Bool(b))
                }
            }
            "print" | "__print" => (st, Value::Nil),
            // A merge HINT. The frontend turns it into a block flag so the
            // interpreter's worklist accumulates states there; the tracer
            // merges at every join by construction, so there is nothing
            // for it to do. Kept rather than removed from the Lua because
            // the IR pipeline still needs it.
            "_hint_normalize" => (st, Value::Nil),
            // On an EXACT value the floor is already unique, so there is
            // one fragment and this is the identity. It only splits once
            // the value has been widened to an interval - and the tracer
            // has no widening yet, which is the next real design question
            // (see plans/tracing.md): the search depends on `rem` being
            // widened so that states merge, so an exact-semantics graph is
            // correct but would not merge anything.
            "__split_by_flr" | "__split_at" => (st, args[0].clone()),
            // Shrinking an array is a SHAPE change, which is exactly what
            // the tracer is built to let happen: two branches that
            // disagree about whether an object exists stop being
            // mergeable and become two output states.
            "__array_table_drop_last" => {
                let Value::Table(t) = &args[0] else {
                    bail!("__array_table_drop_last on a non-table: {:?}", args[0])
                };
                let mut st = st;
                let tab = st.heap.tables.get_mut(t).unwrap();
                tab.arr
                    .pop()
                    .ok_or_else(|| anyhow!("__array_table_drop_last on an empty table"))?;
                (st, Value::Nil)
            }
            "__new_unknown_boolean" => {
                let b = self.d.unknown_bool()?;
                (st, Value::Bool(b))
            }
            "min" | "max" => {
                let f = if name == "min" { Fun2::Min } else { Fun2::Max };
                let (a, b) = (num(&args[0])?, num(&args[1])?);
                let r = self.d.fun2(f, &a, &b)?;
                (st, Value::Num(r))
            }
            other => bail!("unsupported builtin {:?}", other),
        })
    }
}

/// A resolved table key. Concrete by construction - see `index_key`.
enum Key {
    Field(String),
    Index(i16),
}

fn ident(t: &full_moon::tokenizer::TokenReference) -> Result<String> {
    match t.token_type() {
        TokenType::Identifier { identifier } => Ok(identifier.to_string()),
        other => bail!("expected an identifier, got {:?}", other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::domain::{Concrete, Symbolic};
    use crate::trace::heap::Heap;
    use crate::transpile::graph::Op;

    fn parse(src: &str) -> full_moon::ast::Ast {
        full_moon::parse(src).expect("parse")
    }

    fn fresh<D: Domain>() -> State<D> {
        let mut heap: Heap<D> = Heap::default();
        let globals = heap.new_table();
        let scope = heap.new_scope(None);
        State { heap, globals, scope }
    }

    /// Run `src` and read back the global `result`.
    fn run<'a, D: Domain>(d: D, ast: &'a full_moon::ast::Ast) -> (Interp<'a, D>, Value<D>) {
        let mut it = Interp::new(d);
        let st = fresh::<D>();
        let out = it.exec_block(ast.nodes(), st).expect("exec");
        assert_eq!(out.len(), 1, "expected one outcome");
        let s = &out[0].0;
        let v = s.heap.tables[&s.globals].hash.get("result").cloned().unwrap_or(Value::Nil);
        (it, v)
    }

    /// The oracle instantiation really does interpret Lua: closures,
    /// recursion through calls, tables, and a concrete `for`.
    #[test]
    fn the_concrete_domain_runs_lua() {
        let ast = parse(
            r#"
            local t = {a = 3, b = 4}
            function hyp2(p)
              return p.a * p.a + p.b * p.b
            end
            local acc = 0
            for i = 1, 3 do
              acc = acc + i
            end
            result = hyp2(t) + acc
            "#,
        );
        let (it, v) = run(Concrete, &ast);
        let Value::Num(n) = v else { panic!("expected a number, got {:?}", v) };
        // 3*3 + 4*4 = 25, plus 1+2+3 = 6.
        assert_eq!(it.d.as_const(&n), Some(P8::from_i16(31)));
    }

    /// The same program under the tracing domain folds to the same
    /// constant, because nothing in it is unknown. Everything the heap
    /// depends on - the field names, the loop bound, the call target -
    /// stayed concrete without the interpreter special-casing any of it.
    #[test]
    fn the_symbolic_domain_agrees_when_nothing_is_unknown() {
        let ast = parse(
            r#"
            local t = {a = 3, b = 4}
            function hyp2(p)
              return p.a * p.a + p.b * p.b
            end
            local acc = 0
            for i = 1, 3 do
              acc = acc + i
            end
            result = hyp2(t) + acc
            "#,
        );
        let (it, v) = run(Symbolic::default(), &ast);
        let Value::Num(n) = v else { panic!("expected a number, got {:?}", v) };
        assert_eq!(it.d.as_const(&n), Some(P8::from_i16(31)));
    }

    /// And the thing the tracer exists for: a branch on something it
    /// cannot know runs BOTH arms and merges them into one select, with
    /// no rewrite anywhere in sight.
    #[test]
    fn an_unknown_branch_becomes_a_select() {
        let ast = parse(
            r#"
            if input > 0 then
              result = 10
            else
              result = 20
            end
            "#,
        );
        let mut d = Symbolic::default();
        // `input` stands in for game data the tracer cannot know.
        let sym = d.graph.leaf(Op::Cell(1));
        let mut it = Interp::new(d);
        let mut st = fresh::<Symbolic>();
        let g = st.globals;
        st.heap.tables.get_mut(&g).unwrap().hash.insert("input".into(), Value::Num(sym));

        let out = it.exec_block(ast.nodes(), st).expect("exec");
        assert_eq!(out.len(), 1, "the arms agreed on shape, so they merged");
        let s = &out[0].0;
        let Value::Num(r) = s.heap.tables[&s.globals].hash["result"].clone() else {
            panic!("expected a number")
        };
        let node = it.d.graph.get(r);
        assert_eq!(node.op, Op::Sel);
        // Two constants under one condition - the `if` became data.
        assert_eq!(it.d.graph.get(node.args[1]).op, Op::Const(10 << 16, 10 << 16));
        assert_eq!(it.d.graph.get(node.args[2]).op, Op::Const(20 << 16, 20 << 16));
    }
}
