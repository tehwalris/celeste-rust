//! Lowering: the full_moon AST -> the resolved `Program`.
//!
//! One pass over the syntax that (a) interns every name, (b) gives every
//! local a frame slot and every free variable that resolves to an
//! enclosing function's local an upvalue index, (c) parses every literal,
//! (d) resolves the IR frontend's function name (`fn_id`) for every
//! function expression from its assignment context, and (e) refuses the
//! Lua the reference interpreter refuses - multiple assignment, stepped
//! loops, method calls, varargs - so the two accept the same programs.
//!
//! ## What is checked here rather than trusted
//!
//! * A captured local is never assigned after its declaration
//!   (`check_capture_immutable`). The run time captures VALUES; the
//!   reference shares the defining SCOPE. Those agree exactly when no
//!   capture is ever reassigned, which this cart satisfies (`init_object`'s
//!   `obj`, `load_room`'s `tile`/`tx`/`ty`) and which is refused otherwise.
//! * The capture list computed by resolution equals the one the reference
//!   computes syntactically (`reference_captures`, a copy of
//!   `Interp::captures_of`). The boundary stores captured values
//!   POSITIONALLY in that order, so a different order is a different row.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;
use full_moon::tokenizer::{Symbol, TokenType};
use rustc_hash::FxHashMap;

use crate::cengine::domain::{Arith, Cmp};
use crate::pico8_num::{Pico8Num as P8, Pico8NumInterval as Iv};

use super::heap::Value;
use super::program::{
    Block, CaptureSrc, Expr, Func, FuncId, Interner, Last, Program, Stmt, Sym,
};

/// Per-function lowering state.
struct FnCtx {
    /// Block scopes, innermost last: `(name, slot)`.
    scopes: Vec<Vec<(Sym, u16)>>,
    nslots: u16,
    /// Upvalues in creation order: the name and where the enclosing
    /// function finds it.
    captures: Vec<(Sym, CaptureSrc)>,
    /// Slots of THIS function captured by a nested closure.
    captured_slots: Vec<u16>,
    /// Slots of this function that a `SetLocal` targets.
    assigned_slots: Vec<u16>,
}

impl FnCtx {
    fn new() -> Self {
        FnCtx {
            scopes: vec![Vec::new()],
            nslots: 0,
            captures: Vec::new(),
            captured_slots: Vec::new(),
            assigned_slots: Vec::new(),
        }
    }

    fn declare(&mut self, name: Sym) -> u16 {
        let slot = self.nslots;
        self.nslots += 1;
        self.scopes.last_mut().unwrap().push((name, slot));
        slot
    }

    fn find_local(&self, name: Sym) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            for (n, slot) in scope.iter().rev() {
                if *n == name {
                    return Some(*slot);
                }
            }
        }
        None
    }

    fn find_capture(&self, name: Sym) -> Option<u16> {
        self.captures.iter().position(|(n, _)| *n == name).map(|i| i as u16)
    }
}

enum VarRef {
    Local(u16),
    Upval(u16),
    Global(Sym),
}

pub struct Lowerer {
    interner: Interner,
    funcs: Vec<Func>,
    fns: Vec<FnCtx>,
    /// See `Interp::hint_name`: the assignment context a function
    /// expression is created under, which names it for `fn_id_of`.
    hint: Vec<String>,
}

pub fn lower(chunk: &ast::Ast) -> Result<Program> {
    let mut lw = Lowerer { interner: Interner::default(), funcs: Vec::new(), fns: Vec::new(), hint: Vec::new() };
    // The names the engine and the bridge address that the cart's text
    // need not mention: the native builtins (some are never called from
    // Lua) and the injected `tile_flag_at`.
    for name in crate::cengine::cart::NATIVE.iter().chain(["tile_flag_at"].iter()) {
        lw.interner.intern(name);
    }
    let top = lw.lower_function_parts(&[], chunk.nodes(), String::new())?;
    let mut by_fn_id = FxHashMap::default();
    for (i, f) in lw.funcs.iter().enumerate() {
        if let Some(id) = f.fn_id {
            if by_fn_id.insert(id, i as FuncId).is_some() {
                bail!("fn_id {} ({}) is claimed by two functions", id, f.name);
            }
        }
    }
    Ok(Program { interner: lw.interner, funcs: lw.funcs, top, by_fn_id })
}

impl Lowerer {
    fn sym(&mut self, s: &str) -> Sym {
        self.interner.intern(s)
    }

    fn ctx(&mut self) -> &mut FnCtx {
        self.fns.last_mut().unwrap()
    }

    // ------------------------------------------------------ resolution

    /// Resolve `name` from function `depth` (an index into `fns`),
    /// recording the captures that makes necessary on the way down.
    fn lookup(&mut self, depth: usize, name: Sym) -> Option<CaptureSrc> {
        if let Some(slot) = self.fns[depth].find_local(name) {
            return Some(CaptureSrc::Local(slot));
        }
        if let Some(idx) = self.fns[depth].find_capture(name) {
            return Some(CaptureSrc::Upval(idx));
        }
        if depth == 0 {
            return None;
        }
        let src = self.lookup(depth - 1, name)?;
        if let CaptureSrc::Local(slot) = src {
            self.fns[depth - 1].captured_slots.push(slot);
        }
        let idx = self.fns[depth].captures.len() as u16;
        self.fns[depth].captures.push((name, src));
        Some(CaptureSrc::Upval(idx))
    }

    /// `lookup` without the side effects, for the parity check.
    fn resolvable(&self, depth: usize, name: Sym) -> bool {
        self.fns[depth].find_local(name).is_some()
            || self.fns[depth].find_capture(name).is_some()
            || (depth > 0 && self.resolvable(depth - 1, name))
    }

    fn resolve(&mut self, name: &str) -> VarRef {
        let sym = self.sym(name);
        let depth = self.fns.len() - 1;
        match self.lookup(depth, sym) {
            Some(CaptureSrc::Local(slot)) => VarRef::Local(slot),
            Some(CaptureSrc::Upval(idx)) => VarRef::Upval(idx),
            None => VarRef::Global(sym),
        }
    }

    // ------------------------------------------------------- functions

    fn lower_function(&mut self, body: &ast::FunctionBody, name: String) -> Result<FuncId> {
        let params: Vec<String> = body
            .parameters()
            .iter()
            .map(|p| match p {
                ast::Parameter::Name(t) => ident(t),
                other => bail!("unsupported parameter {:?}", other),
            })
            .collect::<Result<_>>()?;
        // The reference's capture list, decided at the creation site
        // (before the body's own locals exist).
        let expected: Vec<Sym> = {
            let depth = self.fns.len() - 1;
            let free = reference_captures(body);
            let mut out = Vec::new();
            for n in free {
                let s = self.sym(&n);
                if self.resolvable(depth, s) {
                    out.push(s);
                }
            }
            out
        };
        let id = self.lower_function_parts(&params, body.block(), name)?;
        let got = &self.funcs[id as usize].capture_names;
        if *got != expected {
            let show = |v: &[Sym]| v.iter().map(|s| self.interner.name(*s).to_string()).collect::<Vec<_>>();
            bail!(
                "function {}: resolved captures {:?} differ from the reference's {:?}",
                self.funcs[id as usize].name,
                show(got),
                show(&expected)
            );
        }
        Ok(id)
    }

    fn lower_function_parts(&mut self, params: &[String], block: &ast::Block, name: String) -> Result<FuncId> {
        let fn_id = if name.is_empty() { None } else { fn_id_of(&name) };
        self.fns.push(FnCtx::new());
        for p in params {
            let s = self.sym(p);
            self.ctx().declare(s);
        }
        // A body runs under an EMPTY hint stack (it is called, not
        // evaluated in place), so nested closures are named from their
        // own assignment context only.
        let saved_hint = std::mem::take(&mut self.hint);
        let body = self.lower_block(block);
        self.hint = saved_hint;
        let body = body.map_err(|e| anyhow!("in function {}: {:#}", if name.is_empty() { "<chunk>" } else { &name }, e))?;
        let ctx = self.fns.pop().unwrap();
        check_capture_immutable(&ctx, &name)?;
        let func = Func {
            name,
            fn_id,
            nparams: params.len() as u16,
            nslots: ctx.nslots,
            captures: ctx.captures.iter().map(|(_, src)| *src).collect(),
            capture_names: ctx.captures.iter().map(|(n, _)| *n).collect(),
            body,
        };
        self.funcs.push(func);
        Ok((self.funcs.len() - 1) as FuncId)
    }

    // ---------------------------------------------------------- blocks

    fn lower_block(&mut self, block: &ast::Block) -> Result<Block> {
        self.ctx().scopes.push(Vec::new());
        let r = self.lower_block_inner(block);
        self.ctx().scopes.pop();
        r
    }

    fn lower_block_inner(&mut self, block: &ast::Block) -> Result<Block> {
        let mut stmts = Vec::new();
        for stmt in block.stmts() {
            self.lower_stmt(stmt, &mut stmts)?;
        }
        let last = match block.last_stmt() {
            None => None,
            Some(ast::LastStmt::Break(_)) => Some(Last::Break),
            Some(ast::LastStmt::Return(r)) => {
                if r.returns().len() > 1 {
                    bail!("multiple return values are not supported");
                }
                Some(Last::Return(match r.returns().iter().next() {
                    Some(e) => Some(self.lower_expr(e)?),
                    None => None,
                }))
            }
            Some(other) => bail!("unsupported last statement {:?}", other),
        };
        Ok(Block { stmts, last })
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt, out: &mut Vec<Stmt>) -> Result<()> {
        match stmt {
            ast::Stmt::LocalAssignment(la) => {
                // Sequential, as the reference does it: each name's
                // initializer is evaluated with the previous names already
                // declared, and initializers past the last name are not
                // evaluated at all.
                let names: Vec<_> = la.names().iter().collect();
                let exprs: Vec<_> = la.expressions().iter().collect();
                for (i, name) in names.iter().enumerate() {
                    let n = ident(name)?;
                    let value = match exprs.get(i) {
                        Some(e) => {
                            self.hint.push(n.clone());
                            let v = self.lower_expr(e);
                            self.hint.pop();
                            Some(v?)
                        }
                        None => None,
                    };
                    let s = self.sym(&n);
                    let slot = self.ctx().declare(s);
                    out.push(Stmt::Local { slot, value });
                }
            }
            ast::Stmt::Assignment(a) => {
                if a.variables().len() > 1 {
                    bail!("multiple assignment is not supported");
                }
                let var = a.variables().iter().next().ok_or_else(|| anyhow!("assignment with no variable"))?;
                let e = a.expressions().iter().next().ok_or_else(|| anyhow!("assignment with no expression"))?;
                // TARGET FIRST, then the value (Lua's order, and the
                // reference's).
                enum Tgt {
                    Local(u16),
                    Global(Sym),
                    Field(Expr, Sym),
                    Index(Expr, Expr),
                }
                let tgt = match var {
                    ast::Var::Name(t) => match self.resolve(&ident(t)?) {
                        VarRef::Local(slot) => Tgt::Local(slot),
                        VarRef::Global(s) => Tgt::Global(s),
                        VarRef::Upval(_) => bail!("assignment to captured variable {:?}", ident(t)?),
                    },
                    ast::Var::Expression(ve) => {
                        let suffixes: Vec<_> = ve.suffixes().collect();
                        let Some((last, init)) = suffixes.split_last() else {
                            bail!("assignment target with no suffixes")
                        };
                        let (table, _) = self.lower_suffixes(ve.prefix(), init)?;
                        match last {
                            ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                                let k = ident(name)?;
                                let k = self.sym(&k);
                                Tgt::Field(table, k)
                            }
                            ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                                Tgt::Index(table, self.lower_expr(expression)?)
                            }
                            other => bail!("cannot assign through {:?}", other),
                        }
                    }
                    other => bail!("unsupported assignment target {:?}", other),
                };
                let hint = target_hint(var);
                if let Some(h) = &hint {
                    self.hint.push(h.clone());
                }
                let value = self.lower_expr(e);
                if hint.is_some() {
                    self.hint.pop();
                }
                let value = value?;
                out.push(match tgt {
                    Tgt::Local(slot) => {
                        self.ctx().assigned_slots.push(slot);
                        Stmt::SetLocal { slot, value }
                    }
                    Tgt::Global(name) => Stmt::SetGlobal { name, value },
                    Tgt::Field(table, key) => Stmt::SetField { table, key, value },
                    Tgt::Index(table, index) => Stmt::SetIndex { table, index, value },
                });
            }
            ast::Stmt::FunctionCall(call) => {
                let suffixes: Vec<_> = call.suffixes().collect();
                let (e, _) = self.lower_suffixes(call.prefix(), &suffixes)?;
                out.push(Stmt::Call(e));
            }
            ast::Stmt::FunctionDeclaration(f) => {
                let name = f.name().to_string().trim().to_string();
                if name.contains('.') || name.contains(':') {
                    bail!("qualified function names are not supported: {:?}", name);
                }
                let id = self.lower_function(f.body(), name.clone())?;
                let value = Expr::Function(id);
                out.push(match self.resolve(&name) {
                    VarRef::Local(slot) => {
                        self.ctx().assigned_slots.push(slot);
                        Stmt::SetLocal { slot, value }
                    }
                    VarRef::Global(s) => Stmt::SetGlobal { name: s, value },
                    VarRef::Upval(_) => bail!("function declaration over a captured variable {:?}", name),
                });
            }
            ast::Stmt::If(iff) => {
                let mut arms = vec![(self.lower_expr(iff.condition())?, self.lower_block(iff.block())?)];
                if let Some(eis) = iff.else_if() {
                    for ei in eis {
                        arms.push((self.lower_expr(ei.condition())?, self.lower_block(ei.block())?));
                    }
                }
                let els = match iff.else_block() {
                    Some(b) => Some(self.lower_block(b)?),
                    None => None,
                };
                out.push(Stmt::If { arms, els });
            }
            ast::Stmt::NumericFor(f) => {
                if f.step().is_some() {
                    bail!("numeric for with an explicit step is not supported");
                }
                let start = self.lower_expr(f.start())?;
                let limit = self.lower_expr(f.end())?;
                let unroll = unroll_bound(f.end().to_string().trim());
                let name = ident(f.index_variable())?;
                let s = self.sym(&name);
                self.ctx().scopes.push(Vec::new());
                let slot = self.ctx().declare(s);
                let body = self.lower_block(f.block());
                self.ctx().scopes.pop();
                out.push(Stmt::For { slot, start, limit, body: body?, unroll });
            }
            other => bail!("unsupported statement {:?}", other),
        }
        Ok(())
    }

    // ----------------------------------------------------- expressions

    fn lower_expr(&mut self, e: &ast::Expression) -> Result<Expr> {
        Ok(match e {
            ast::Expression::Number(n) => {
                let TokenType::Number { text } = n.token_type() else {
                    bail!("expected a number literal");
                };
                let v: P8 = text.parse()?;
                Expr::Const(Value::Num(Iv::from_number(v)))
            }
            ast::Expression::String(s) => {
                let TokenType::StringLiteral { literal, .. } = s.token_type() else {
                    bail!("expected a string literal");
                };
                let sym = self.sym(literal);
                Expr::Const(Value::Str(sym))
            }
            ast::Expression::Symbol(sym) => {
                let TokenType::Symbol { symbol } = sym.token_type() else {
                    bail!("expected a symbol");
                };
                Expr::Const(match symbol {
                    Symbol::True => Value::Bool(true),
                    Symbol::False => Value::Bool(false),
                    Symbol::Nil => Value::Nil,
                    other => bail!("unsupported symbol {:?}", other),
                })
            }
            ast::Expression::Parentheses { expression, .. } => self.lower_expr(expression)?,
            ast::Expression::UnaryOperator { unop, expression } => {
                let inner = Box::new(self.lower_expr(expression)?);
                match unop {
                    ast::UnOp::Minus(_) => Expr::Neg(inner),
                    ast::UnOp::Hash(_) => Expr::Len(inner),
                    ast::UnOp::Not(_) => Expr::Not(inner),
                    other => bail!("unsupported unary operator {:?}", other),
                }
            }
            ast::Expression::BinaryOperator { lhs, binop, rhs } => {
                let l = Box::new(self.lower_expr(lhs)?);
                let r = Box::new(self.lower_expr(rhs)?);
                let src = || -> Box<str> {
                    let t = format!("{}{}{}", lhs, binop, rhs);
                    let t = t.trim();
                    t[..t.len().min(60)].into()
                };
                match binop {
                    ast::BinOp::And(_) => Expr::And(l, r),
                    ast::BinOp::Or(_) => Expr::Or(l, r),
                    ast::BinOp::Plus(_) => Expr::Arith(Arith::Add, l, r, src()),
                    ast::BinOp::Minus(_) => Expr::Arith(Arith::Sub, l, r, src()),
                    ast::BinOp::Star(_) => Expr::Arith(Arith::Mul, l, r, src()),
                    ast::BinOp::Slash(_) => Expr::Arith(Arith::Div, l, r, src()),
                    ast::BinOp::Percent(_) => Expr::Arith(Arith::Rem, l, r, src()),
                    ast::BinOp::LessThan(_) => Expr::Cmp { op: Cmp::Lt, negate: false, lhs: l, rhs: r, src: src() },
                    ast::BinOp::LessThanEqual(_) => Expr::Cmp { op: Cmp::Le, negate: false, lhs: l, rhs: r, src: src() },
                    ast::BinOp::GreaterThan(_) => Expr::Cmp { op: Cmp::Gt, negate: false, lhs: l, rhs: r, src: src() },
                    ast::BinOp::GreaterThanEqual(_) => Expr::Cmp { op: Cmp::Ge, negate: false, lhs: l, rhs: r, src: src() },
                    ast::BinOp::TwoEqual(_) => Expr::Cmp { op: Cmp::Eq, negate: false, lhs: l, rhs: r, src: src() },
                    ast::BinOp::TildeEqual(_) => Expr::Cmp { op: Cmp::Eq, negate: true, lhs: l, rhs: r, src: src() },
                    other => bail!("unsupported binary operator {:?}", other),
                }
            }
            ast::Expression::Var(v) => match v {
                ast::Var::Name(t) => self.var_expr(&ident(t)?),
                ast::Var::Expression(ve) => {
                    let suffixes: Vec<_> = ve.suffixes().collect();
                    self.lower_suffixes(ve.prefix(), &suffixes)?.0
                }
                other => bail!("unsupported var {:?}", other),
            },
            ast::Expression::FunctionCall(call) => {
                let suffixes: Vec<_> = call.suffixes().collect();
                self.lower_suffixes(call.prefix(), &suffixes)?.0
            }
            ast::Expression::Function((_, body)) => {
                let name = if self.hint.is_empty() { String::new() } else { self.hint.join(".") };
                Expr::Function(self.lower_function(body, name)?)
            }
            ast::Expression::TableConstructor(t) => {
                let mut fields = Vec::new();
                for field in t.fields() {
                    match field {
                        ast::Field::NameKey { key, value, .. } => {
                            let k = ident(key)?;
                            self.hint.push(k.clone());
                            let v = self.lower_expr(value);
                            self.hint.pop();
                            let s = self.sym(&k);
                            fields.push((Some(s), v?));
                        }
                        ast::Field::NoKey(e) => fields.push((None, self.lower_expr(e)?)),
                        other => bail!("unsupported table field {:?}", other),
                    }
                }
                Expr::Table(fields)
            }
            other => bail!("unsupported expression {:?}", other),
        })
    }

    fn var_expr(&mut self, name: &str) -> Expr {
        match self.resolve(name) {
            VarRef::Local(slot) => Expr::Local(slot),
            VarRef::Upval(idx) => Expr::Upval(idx),
            VarRef::Global(s) => Expr::Global(s),
        }
    }

    /// A prefix and its suffixes, plus the readable path the reference
    /// builds for its error messages (`player.move()`).
    fn lower_suffixes(&mut self, p: &ast::Prefix, suffixes: &[&ast::Suffix]) -> Result<(Expr, String)> {
        let (mut cur, mut path) = match p {
            ast::Prefix::Name(t) => {
                let n = ident(t)?;
                (self.var_expr(&n), n)
            }
            ast::Prefix::Expression(e) => (self.lower_expr(e)?, "(expr)".to_string()),
            other => bail!("unsupported prefix {:?}", other),
        };
        for suffix in suffixes {
            match suffix {
                ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                    let k = ident(name)?;
                    path = format!("{}.{}", path, k);
                    let s = self.sym(&k);
                    cur = Expr::Field(Box::new(cur), s);
                }
                ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                    let idx = self.lower_expr(expression)?;
                    path = format!("{}[..]", path);
                    cur = Expr::Index(Box::new(cur), Box::new(idx));
                }
                ast::Suffix::Call(ast::Call::AnonymousCall(ast::FunctionArgs::Parentheses { arguments, .. })) => {
                    let mut args = Vec::new();
                    for a in arguments {
                        args.push(self.lower_expr(a)?);
                    }
                    cur = Expr::Call(Box::new(cur), args, path.as_str().into());
                    path = format!("{}()", path);
                }
                other => bail!("unsupported suffix {:?}", other),
            }
        }
        Ok((cur, path))
    }
}

/// The engine's index for a function named `name` - a copy of the
/// reference's `Interp::fn_id_of`: match on the BASE of `base_N` and
/// require it to be unique.
pub fn fn_id_of(name: &str) -> Option<u32> {
    let mut found = None;
    for (i, n) in celeste_names::gen::FN_NAMES.iter().enumerate() {
        let base = match n.rfind('_') {
            Some(k) if n[k + 1..].chars().all(|c| c.is_ascii_digit()) => &n[..k],
            _ => *n,
        };
        if base == name {
            if found.is_some() {
                return None;
            }
            found = Some(i as u32);
        }
    }
    found
}

/// A copy of the reference's `target_hint`.
fn target_hint(var: &ast::Var) -> Option<String> {
    match var {
        ast::Var::Name(t) => Some(t.token().to_string().trim().to_string()),
        ast::Var::Expression(ve) => {
            let mut out = match ve.prefix() {
                ast::Prefix::Name(t) => t.token().to_string().trim().to_string(),
                _ => return None,
            };
            for suffix in ve.suffixes() {
                match suffix {
                    ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                        out.push('.');
                        out.push_str(name.token().to_string().trim());
                    }
                    _ => return None,
                }
            }
            Some(out)
        }
        _ => None,
    }
}

/// A copy of the reference's `unroll_bound`, keyed by the limit's source
/// text. A bound too small fails at run time (the loop must have finished
/// when it runs out), so this is a performance choice, not a correctness
/// one.
fn unroll_bound(limit_src: &str) -> Option<u32> {
    match limit_src {
        "abs(amount)" => Some(8),
        "min(15,(x+w-1)/8)" | "min(15,(y+h-1)/8)" => Some(3),
        _ => None,
    }
}

/// The reference's syntactic capture candidates (`Interp::captures_of`
/// before its scope filter): free names in visit order, minus every name
/// bound anywhere in the body.
fn reference_captures(body: &ast::FunctionBody) -> Vec<String> {
    use full_moon::visitors::{Visit, Visitor};

    #[derive(Default)]
    struct Names {
        order: Vec<String>,
        seen: std::collections::HashSet<String>,
        bound: std::collections::HashSet<String>,
    }
    impl Names {
        fn push(&mut self, n: String) {
            if self.seen.insert(n.clone()) {
                self.order.push(n);
            }
        }
    }
    impl Visitor for Names {
        fn visit_var(&mut self, v: &ast::Var) {
            if let ast::Var::Name(t) = v {
                self.push(t.token().to_string().trim().to_string());
            }
        }
        fn visit_prefix(&mut self, p: &ast::Prefix) {
            if let ast::Prefix::Name(t) = p {
                self.push(t.token().to_string().trim().to_string());
            }
        }
        fn visit_function_body(&mut self, b: &ast::FunctionBody) {
            for p in b.parameters() {
                if let ast::Parameter::Name(t) = p {
                    self.bound.insert(t.token().to_string().trim().to_string());
                }
            }
        }
        fn visit_local_assignment(&mut self, la: &ast::LocalAssignment) {
            for n in la.names() {
                self.bound.insert(n.token().to_string().trim().to_string());
            }
        }
        fn visit_local_function(&mut self, lf: &ast::LocalFunction) {
            self.bound.insert(lf.name().token().to_string().trim().to_string());
        }
        fn visit_numeric_for(&mut self, f: &ast::NumericFor) {
            self.bound.insert(f.index_variable().token().to_string().trim().to_string());
        }
        fn visit_generic_for(&mut self, f: &ast::GenericFor) {
            for n in f.names() {
                self.bound.insert(n.token().to_string().trim().to_string());
            }
        }
    }
    let mut names = Names::default();
    body.visit(&mut names);
    names.order.into_iter().filter(|n| !names.bound.contains(n)).collect()
}

fn check_capture_immutable(ctx: &FnCtx, name: &str) -> Result<()> {
    for slot in &ctx.captured_slots {
        if ctx.assigned_slots.contains(slot) {
            bail!(
                "function {}: local slot {} is captured by a closure and assigned - value \
                 capture would diverge from the reference's shared scope",
                if name.is_empty() { "<chunk>" } else { name },
                slot
            );
        }
    }
    Ok(())
}

fn ident(t: &full_moon::tokenizer::TokenReference) -> Result<String> {
    match t.token_type() {
        TokenType::Identifier { identifier } => Ok(identifier.to_string()),
        other => bail!("expected an identifier, got {:?}", other),
    }
}
