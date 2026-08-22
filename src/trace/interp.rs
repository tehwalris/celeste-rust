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
use super::heap::{BodyId, TableId, Value};
use super::state::{merge, split_path, State};

thread_local! {
    /// Is the path lookup ever load-bearing? Counts calls that reached it
    /// (the domain could not fold the condition) and calls that found the
    /// literal. See plans/tracing.md T5.
    pub static PATH_ASKED: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    pub static PATH_HIT: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

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

/// One or more `(state, thing)` outcomes.
///
/// EVERYTHING that can run code returns one of these, expressions
/// included. An expression is not pure: it can contain a call, the call
/// can branch on something unknown, and the two arms may disagree about
/// the heap's shape and so fail to merge. Threading a single state through
/// expressions was not a simplification, it was wrong.
pub type Multi<D, T> = Vec<(State<D>, T)>;
pub type Outcome<D> = Multi<D, Flow<D>>;

pub struct Interp<'a, D: Domain> {
    pub d: D,
    /// Function bodies, referred to by id from closures - the AST outlives
    /// the heap, so the heap stores an index rather than a reference.
    bodies: Vec<&'a ast::FunctionBody>,
    /// The cart and the room's collision cache, for `mget`/`fget` and
    /// `tile_flag_at`. Optional so the unit tests can run programs that
    /// never touch the map.
    pub cart: Option<std::sync::Arc<celeste_core::cart_data::CartData>>,
    pub cache: Option<std::sync::Arc<celeste_core::collision_cache::CollisionCache>>,
    /// Budgets, so a blow-up is a DIAGNOSIS rather than a hang. A tracer
    /// that runs forever tells you nothing about where it went wrong; one
    /// that stops at a limit tells you exactly which construct did it.
    pub max_states: usize,
    pub max_nodes: usize,
}

impl<'a, D: Domain> Interp<'a, D> {
    pub fn new(d: D) -> Self {
        Interp {
            d,
            bodies: Vec::new(),
            cart: None,
            cache: None,
            max_states: 256,
            max_nodes: 2_000_000,
        }
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
            live = self.collapse(next)?;
            if live.len() > self.max_states {
                let mut hist: std::collections::BTreeMap<(&str, usize), usize> =
                    Default::default();
                for (st, f) in &live {
                    let k = match f {
                        Flow::Normal => "normal",
                        Flow::Break => "break",
                        Flow::Return(_) => "return",
                    };
                    *hist.entry((k, st.path.len())).or_default() += 1;
                }
                eprintln!("[trace] frontier by (flow, path len): {:?}", hist);
                bail!(
                    "frontier grew to {} states (limit {}) - something is fanning out \
                     without merging back",
                    live.len(),
                    self.max_states
                );
            }
            if self.d.node_count() > self.max_nodes {
                bail!(
                    "graph grew to {} nodes (limit {})",
                    self.d.node_count(),
                    self.max_nodes
                );
            }
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
                if r.returns().len() > 1 {
                    bail!("multiple return values are not supported");
                }
                match r.returns().iter().next() {
                    Some(e) => self
                        .eval(e, st)?
                        .into_iter()
                        .map(|(s, v)| (s, Flow::Return(v)))
                        .collect(),
                    None => vec![(st, Flow::Return(Value::Nil))],
                }
            }
            other => bail!("unsupported last statement {:?}", other),
        })
    }

    // -------------------------------------------------------- statements

    fn exec_stmt(&mut self, stmt: &'a ast::Stmt, st: State<D>) -> Result<Outcome<D>> {
        match stmt {
            ast::Stmt::LocalAssignment(la) => {
                let names: Vec<_> = la.names().iter().collect();
                let exprs: Vec<_> = la.expressions().iter().collect();
                let mut cur: Vec<State<D>> = vec![st];
                for (i, name) in names.iter().enumerate() {
                    let n = ident(name)?;
                    let mut next = Vec::new();
                    for s in cur {
                        match exprs.get(i) {
                            Some(e) => {
                                for (mut s, v) in self.eval(e, s)? {
                                    s.heap.declare(s.scope, &n, v);
                                    next.push(s);
                                }
                            }
                            None => {
                                let mut s = s;
                                s.heap.declare(s.scope, &n, Value::Nil);
                                next.push(s);
                            }
                        }
                    }
                    cur = next;
                }
                Ok(cur.into_iter().map(|s| (s, Flow::Normal)).collect())
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
                let mut out = Vec::new();
                for (s, v) in self.eval(e, st)? {
                    out.extend(self.assign_var(var, v, s)?);
                }
                Ok(out.into_iter().map(|s| (s, Flow::Normal)).collect())
            }
            ast::Stmt::FunctionCall(call) => Ok(self
                .eval_call(call, st)?
                .into_iter()
                .map(|(s, _)| (s, Flow::Normal))
                .collect()),
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
        let mut out: Outcome<D> = Vec::new();
        for (s, cv) in self.eval(cond_e, st)? {
            let cond = self.truthy(&cv);
            out.extend(match self.decide_on_path(&cond, &s) {
                Some(true) => self.exec_block(blk, s)?,
                Some(false) => self.exec_arms(rest, els, s)?,
                None => {
                    // Each arm only happens under its own condition, so
                    // record that before running it. A merge puts the
                    // original back, because either side happening IS the
                    // original condition.
                    let path = s.path.clone();
                    let (ts, fs) = split_path(s, &cond);
                    let t = match ts {
                        Some(x) => self.exec_block(blk, x)?,
                        None => Vec::new(),
                    };
                    let f = match fs {
                        Some(x) => self.exec_arms(rest, els, x)?,
                        None => Vec::new(),
                    };
                    // One side dead means no branch really happened.
                    if t.is_empty() || f.is_empty() {
                        let mut both = t;
                        both.extend(f);
                        both
                    } else {
                        self.merge_outcomes(&cond, &path, t, f)?
                    }
                }
            });
        }
        Ok(out)
    }

    /// Merge any outcomes that differ in exactly one path literal.
    ///
    /// Merging only where the branch was TAKEN is not enough, and the
    /// reason is specific. `btn(x) and 86` fans out into a number and a
    /// boolean - different shapes, so no merge, by design. The enclosing
    /// `or` then DECIDES on each outcome separately, because the path
    /// already records whether `btn(x)` held, so it never splits and no
    /// join for that literal ever runs. The two outcomes only become
    /// compatible AFTER the whole expression, where the join-tied merge
    /// is not looking.
    ///
    /// So the collapse is a fixpoint over the whole frontier instead:
    /// repeatedly merge any two outcomes that agree on everything but one
    /// assumption. Run after every statement, it keeps the frontier at the
    /// number of genuinely different futures rather than the product of
    /// every branch taken along the way.
    pub fn collapse(&mut self, mut outs: Outcome<D>) -> Result<Outcome<D>> {
        'again: loop {
            for i in 0..outs.len() {
                for j in (i + 1)..outs.len() {
                    if !outs[i].1.same_kind(&outs[j].1) {
                        continue;
                    }
                    let Some(k) = differ_at_one(&outs[i].0.path, &outs[j].0.path) else {
                        continue;
                    };
                    // Merge on the literal, true side first.
                    let (ti, fi) = if outs[i].0.path[k].1 { (i, j) } else { (j, i) };
                    let cond = outs[ti].0.path[k].0.clone();
                    let rest = without(&outs[ti].0.path, k);
                    let flow = match (&outs[ti].1, &outs[fi].1) {
                        (Flow::Return(a), Flow::Return(b)) => match (a.clone(), b.clone()) {
                            (Value::Num(x), Value::Num(y)) => {
                                Some(Flow::Return(Value::Num(self.d.sel_num(&cond, &x, &y))))
                            }
                            (Value::Bool(x), Value::Bool(y)) => {
                                Some(Flow::Return(Value::Bool(self.d.sel_bool(&cond, &x, &y))))
                            }
                            (x, y) if x == y => Some(Flow::Return(x)),
                            _ => None,
                        },
                        (Flow::Break, Flow::Break) => Some(Flow::Break),
                        (Flow::Normal, Flow::Normal) => Some(Flow::Normal),
                        _ => None,
                    };
                    let Some(fl) = flow else { continue };
                    let (a, b) = (outs[ti].0.clone(), outs[fi].0.clone());
                    if let Some(m) = merge(&mut self.d, &cond, &rest, a, b)? {
                        let (hi, lo) = if ti > fi { (ti, fi) } else { (fi, ti) };
                        outs.remove(hi);
                        outs.remove(lo);
                        outs.push((m, fl));
                        continue 'again;
                    }
                }
            }
            break;
        }
        Ok(outs)
    }

    /// Put two arms' outcomes back together, PAIRWISE.
    ///
    /// Merging only the one-outcome-each case looked like a reasonable
    /// simplification and was not: branches nest, so by the time control
    /// reaches a join each side usually has several outcomes already, and
    /// falling through to concatenation made the state count MULTIPLY per
    /// frame. Frame 24 of the real cart came out as 12 states that all had
    /// the SAME SHAPE - 3 horizontal-input outcomes times 4 from a later
    /// branch, none of which needed to be separate.
    ///
    /// Two outcomes pair up when they agree on everything except the
    /// literal this join is about: same remaining path, same kind of flow,
    /// mergeable shape. Because each join does this, the collapse cascades
    /// bottom-up and the inner joins have already tidied up by the time an
    /// outer one runs.
    fn merge_outcomes(
        &mut self,
        cond: &D::Bool,
        path: &[(D::Bool, bool)],
        t: Outcome<D>,
        f: Outcome<D>,
    ) -> Result<Outcome<D>> {
        let idx = path.len();
        let mut fs: Vec<Option<(State<D>, Flow<D>)>> = f.into_iter().map(Some).collect();
        let mut out: Outcome<D> = Vec::new();
        for (ts, tf) in t {
            let rest = without(&ts.path, idx);
            let partner = fs.iter().position(|slot| match slot {
                Some((s, fl)) => tf.same_kind(fl) && without(&s.path, idx) == rest,
                None => false,
            });
            let Some(i) = partner else {
                out.push((ts, tf));
                continue;
            };
            let (fst, ff) = fs[i].take().unwrap();
            let flow = match (&tf, &ff) {
                (Flow::Return(a), Flow::Return(b)) => match (a, b) {
                    (Value::Num(x), Value::Num(y)) => {
                        Some(Flow::Return(Value::Num(self.d.sel_num(cond, x, y))))
                    }
                    (Value::Bool(x), Value::Bool(y)) => {
                        Some(Flow::Return(Value::Bool(self.d.sel_bool(cond, x, y))))
                    }
                    (x, y) if x == y => Some(Flow::Return(x.clone())),
                    _ => None,
                },
                (Flow::Break, Flow::Break) => Some(Flow::Break),
                (Flow::Normal, Flow::Normal) => Some(Flow::Normal),
                _ => None,
            };
            let merged = match &flow {
                Some(_) => merge(&mut self.d, cond, &rest, ts.clone(), fst.clone())?,
                None => None,
            };
            match (merged, flow) {
                (Some(m), Some(fl)) => out.push((m, fl)),
                _ => {
                    out.push((ts, tf));
                    out.push((fst, ff));
                }
            }
        }
        out.extend(fs.into_iter().flatten());
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
        let name = ident(f.index_variable())?;
        let mut all: Outcome<D> = Vec::new();
        // Bounds can themselves fan out (they are expressions), so each
        // combination is its own loop.
        let mut bounds: Multi<D, Bound<D>> = Vec::new();
        for (s, from) in self.eval(f.start(), st)? {
            for (s, to) in self.eval(f.end(), s)? {
                let (Value::Num(a), Value::Num(b)) = (&from, &to) else {
                    bail!("numeric for bounds must be numbers");
                };
                match (self.d.as_const(a), self.d.as_const(b)) {
                    (Some(a), Some(b)) => bounds.push((s, Bound::Concrete(a, b))),
                    // Either end unknown: unroll under a heuristic and
                    // require that the loop had finished. The START can be
                    // symbolic too - the tile scans begin at
                    // `max(0, flr(x/8))` - so the loop variable is
                    // `start + k`, itself a symbolic value.
                    _ => bounds.push((s, Bound::Symbolic(a.clone(), b.clone()))),
                }
            }
        }
        for (s, b) in bounds {
            all.extend(match b {
                Bound::Concrete(from, to) => self.run_for(f, &name, from, to, s)?,
                Bound::Symbolic(start, limit) => {
                    let limit_src = f.end().to_string().trim().to_string();
                    let n = unroll_bound(&limit_src).ok_or_else(|| {
                        anyhow!(
                            "numeric for with symbolic limit `{}` has no unroll bound - \
                             add one to `unroll_bound`",
                            limit_src
                        )
                    })?;
                    self.run_for_symbolic(f, &name, start, limit, s, n)?
                }
            });
        }
        Ok(all)
    }

    /// Unroll a loop whose limit is not known at trace time, masking each
    /// iteration by `i <= limit` and merging straight away - so the result
    /// is ONE state whose values are nested selects, which is what
    /// `mask_loop` produced as a rewrite.
    ///
    /// After the bound runs out, the state is REQUIRED to have finished.
    /// Too small a bound therefore fails at run time rather than silently
    /// truncating the loop, which is what makes the number above a
    /// performance choice instead of a correctness one.
    fn run_for_symbolic(
        &mut self,
        f: &'a ast::NumericFor,
        name: &str,
        start: D::Num,
        limit: D::Num,
        s: State<D>,
        bound: u32,
    ) -> Result<Outcome<D>> {
        let mut running: Outcome<D> = vec![(s, Flow::Normal)];
        for k in 0..bound {
            let mut next: Outcome<D> = Vec::new();
            for (s, fl) in running {
                if !fl.is_normal() {
                    next.push((s, fl));
                    continue;
                }
                let off = self.d.num(P8::from_i16(k as i16));
                let iv = self.d.arith(Arith::Add, &start, &off)?;
                let cond = self.d.compare(Cmp::Le, &iv, &limit)?;
                match self.decide_on_path(&cond, &s) {
                    // Past the limit on this path: nothing more to do, but
                    // the state carries on after the loop.
                    Some(false) => next.push((s, Flow::Normal)),
                    Some(true) => next.extend(self.for_body(f, name, &iv, s)?),
                    None => {
                        let path = s.path.clone();
                        let (ts, fs) = split_path(s, &cond);
                        let t = match ts {
                            Some(x) => self.for_body(f, name, &iv, x)?,
                            None => Vec::new(),
                        };
                        let fo: Outcome<D> = match fs {
                            Some(x) => vec![(x, Flow::Normal)],
                            None => Vec::new(),
                        };
                        if t.is_empty() || fo.is_empty() {
                            next.extend(t);
                            next.extend(fo);
                        } else {
                            next.extend(self.merge_outcomes(&cond, &path, t, fo)?);
                        }
                    }
                }
            }
            running = self.collapse(next)?;
        }
        // The obligation: by now the loop must be over.
        let off = self.d.num(P8::from_i16(bound as i16));
        let iv = self.d.arith(Arith::Add, &start, &off)?;
        let over = self.d.compare(Cmp::Le, &iv, &limit)?;
        let finished = self.d.not(&over);
        for (s, fl) in running.iter_mut() {
            let _ = &fl;
            if !s.ok.contains(&finished) {
                s.ok.push(finished.clone());
            }
        }
        Ok(running
            .into_iter()
            .map(|(s, fl)| (s, if matches!(fl, Flow::Break) { Flow::Normal } else { fl }))
            .collect())
    }

    /// One iteration of a numeric `for`, in its own scope.
    fn for_body(
        &mut self,
        f: &'a ast::NumericFor,
        name: &str,
        iv: &D::Num,
        mut cur: State<D>,
    ) -> Result<Outcome<D>> {
        let body_scope = cur.heap.new_scope(Some(cur.scope));
        cur.heap.declare(body_scope, name, Value::Num(iv.clone()));
        let outer = cur.scope;
        cur.scope = body_scope;
        let mut out = Vec::new();
        for (mut s2, f2) in self.exec_block(f.block(), cur)? {
            s2.scope = outer;
            match f2 {
                Flow::Break => out.push((s2, Flow::Normal)),
                other => out.push((s2, other)),
            }
        }
        Ok(out)
    }

    fn run_for(
        &mut self,
        f: &'a ast::NumericFor,
        name: &str,
        from: P8,
        to: P8,
        s: State<D>,
    ) -> Result<Outcome<D>> {

        // A `break` ends the LOOP for that state, not the state: it moves
        // to `done` and carries on after the loop. Getting this wrong the
        // first time made `break` a no-op, so `foreach`'s bounded walk ran
        // its full 32767 iterations instead of stopping.
        let mut running: Outcome<D> = vec![(s, Flow::Normal)];
        let mut done: Outcome<D> = Vec::new();
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

    /// Decide a condition, consulting what the path already assumed. A
    /// literal branched on earlier is not unknown any more, and that is
    /// what stops `a and b or c` fanning out twice on the same question.
    fn decide_on_path(&self, cond: &D::Bool, st: &State<D>) -> Option<bool> {
        if let Some(b) = self.d.decide(cond) {
            return Some(b);
        }
        if std::env::var_os("TRACE_NO_PATH_DECIDE").is_some() {
            return None;
        }
        let hit = st.path.iter().find(|(l, _)| l == cond).map(|(_, v)| *v);
        PATH_ASKED.with(|c| c.set(c.get() + 1));
        if hit.is_some() {
            PATH_HIT.with(|c| c.set(c.get() + 1));
        }
        hit
    }

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
    ) -> Result<Multi<D, Value<D>>> {
        match e {
            ast::Expression::Number(n) => {
                let TokenType::Number { text } = n.token_type() else {
                    bail!("expected a number literal");
                };
                let v: P8 = text.parse()?;
                let n = self.d.num(v);
                Ok(vec![(st, Value::Num(n))])
            }
            ast::Expression::String(s) => {
                let TokenType::StringLiteral { literal, .. } = s.token_type() else {
                    bail!("expected a string literal");
                };
                Ok(vec![(st, Value::Str(literal.to_string().into()))])
            }
            ast::Expression::Symbol(sym) => {
                let TokenType::Symbol { symbol } = sym.token_type() else {
                    bail!("expected a symbol");
                };
                let v = match symbol {
                    Symbol::True => Value::Bool(self.d.boolean(true)),
                    Symbol::False => Value::Bool(self.d.boolean(false)),
                    Symbol::Nil => Value::Nil,
                    other => bail!("unsupported symbol {:?}", other),
                };
                Ok(vec![(st, v)])
            }
            ast::Expression::Parentheses { expression, .. } => self.eval(expression, st),
            ast::Expression::UnaryOperator { unop, expression } => {
                let mut out = Vec::new();
                for (st, v) in self.eval(expression, st)? {
                    out.push(match unop {
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
                    });
                }
                Ok(out)
            }
            ast::Expression::BinaryOperator { lhs, binop, rhs } => {
                // Carry the SOURCE TEXT. A type error in an expression is
                // useless without knowing which expression.
                self.eval_binop(lhs, binop, rhs, st).map_err(|err| {
                    let t = e.to_string();
                    let t = t.trim();
                    anyhow!("in `{}`: {:#}", &t[..t.len().min(60)], err)
                })
            }
            ast::Expression::Var(v) => match v {
                ast::Var::Name(t) => {
                    let n = ident(t)?;
                    let val = self.read_name(&n, &st);
                    Ok(vec![(st, val)])
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
                Ok(vec![(st, Value::Func { body: id, env })])
            }
            ast::Expression::TableConstructor(t) => {
                let mut cur: Multi<D, TableId> = {
                    let mut st = st;
                    let id = st.heap.new_table();
                    vec![(st, id)]
                };
                for field in t.fields() {
                    let mut next: Multi<D, TableId> = Vec::new();
                    for (st, id) in cur {
                        match field {
                            ast::Field::NameKey { key, value, .. } => {
                                let k = ident(key)?;
                                for (mut st, v) in self.eval(value, st)? {
                                    st.heap.tables.get_mut(&id).unwrap().hash.insert(k.clone(), v);
                                    next.push((st, id));
                                }
                            }
                            ast::Field::NoKey(e) => {
                                for (mut st, v) in self.eval(e, st)? {
                                    st.heap.tables.get_mut(&id).unwrap().arr.push(v);
                                    next.push((st, id));
                                }
                            }
                            other => bail!("unsupported table field {:?}", other),
                        }
                    }
                    cur = next;
                }
                Ok(cur.into_iter().map(|(st, id)| (st, Value::Table(id))).collect())
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
    ) -> Result<Multi<D, Value<D>>> {
        // `and`/`or` short-circuit and return VALUES, not booleans - so
        // `btn(r) and 1` is either a boolean or a number. Those are
        // different SHAPES, so an undecided condition fans out here and
        // the two sides merge again only if they agree on kind. The
        // enclosing `or` is what collapses the idiom back to a number.
        if matches!(binop, ast::BinOp::And(_) | ast::BinOp::Or(_)) {
            let is_and = matches!(binop, ast::BinOp::And(_));
            let mut out = Vec::new();
            for (st, a) in self.eval(lhs, st)? {
                let t = self.truthy(&a);
                match self.decide_on_path(&t, &st) {
                    Some(x) if x == is_and => out.extend(self.eval(rhs, st)?),
                    Some(_) => out.push((st, a)),
                    None => {
                        let path = st.path.clone();
                        let taken_cond = if is_and { t.clone() } else { self.d.not(&t) };
                        let (ts, fs) = split_path(st, &taken_cond);
                        let taken = match ts {
                            Some(x) => self.eval(rhs, x)?,
                            None => Vec::new(),
                        };
                        let kept: Multi<D, Value<D>> = match fs {
                            // The kept operand is the one whose truthiness
                            // we JUST decided, so hand on the CONSTANT, not
                            // the symbolic node we split on. `and` keeps a
                            // falsy left operand, `or` a truthy one, so the
                            // constant is `!is_and`.
                            //
                            // Without this the idiom leaks: `btn(u) and -1`
                            // hands `Bool(U)` to the enclosing `or`, which
                            // splits on U AGAIN and produces a state where
                            // `v_input` is a bool - and four lines later
                            // `v_input*d_half` is arithmetic on a boolean.
                            // Only `Value::Bool` is rewritten; a falsy `Nil`
                            // or a truthy table is already as concrete as it
                            // gets.
                            Some(x) => {
                                let v = match a {
                                    Value::Bool(_) => Value::Bool(self.d.boolean(!is_and)),
                                    other => other,
                                };
                                vec![(x, v)]
                            }
                            None => Vec::new(),
                        };
                        if taken.is_empty() || kept.is_empty() {
                            out.extend(taken);
                            out.extend(kept);
                        } else {
                            out.extend(self.merge_values(&taken_cond, &path, taken, kept)?);
                        }
                    }
                }
            }
            return Ok(out);
        }
        let mut out = Vec::new();
        for (st, a) in self.eval(lhs, st)? {
            for (st, b) in self.eval(rhs, st)? {
                out.push((st, self.binop_values(binop, &a, &b)?));
            }
        }
        Ok(out)
    }

    fn binop_values(
        &mut self,
        binop: &ast::BinOp,
        a: &Value<D>,
        b: &Value<D>,
    ) -> Result<Value<D>> {
        let (num_op, cmp_op) = match binop {
            ast::BinOp::Plus(_) => (Some(Arith::Add), None),
            ast::BinOp::Minus(_) => (Some(Arith::Sub), None),
            ast::BinOp::Star(_) => (Some(Arith::Mul), None),
            ast::BinOp::Slash(_) => (Some(Arith::Div), None),
            ast::BinOp::Percent(_) => (Some(Arith::Rem), None),
            ast::BinOp::LessThan(_) => (None, Some(Cmp::Lt)),
            ast::BinOp::LessThanEqual(_) => (None, Some(Cmp::Le)),
            ast::BinOp::GreaterThan(_) => (None, Some(Cmp::Gt)),
            ast::BinOp::GreaterThanEqual(_) => (None, Some(Cmp::Ge)),
            ast::BinOp::TwoEqual(_) => (None, Some(Cmp::Eq)),
            ast::BinOp::TildeEqual(_) => (None, Some(Cmp::Eq)),
            other => bail!("unsupported binary operator {:?}", other),
        };
        if let Some(op) = num_op {
            let (Value::Num(x), Value::Num(y)) = (a, b) else {
                bail!("arithmetic on non-numbers: {:?} and {:?}", a, b)
            };
            return Ok(Value::Num(self.d.arith(op, x, y)?));
        }
        let op = cmp_op.unwrap();
        let r = match (a, b) {
            (Value::Num(x), Value::Num(y)) => self.d.compare(op, x, y)?,
            // Everything but numbers and booleans is concrete, so equality
            // on it is decidable right here.
            _ if op == Cmp::Eq => {
                let same = a == b;
                self.d.boolean(same)
            }
            _ => bail!("comparison of {:?} and {:?}", a, b),
        };
        Ok(Value::Bool(if matches!(binop, ast::BinOp::TildeEqual(_)) {
            self.d.not(&r)
        } else {
            r
        }))
    }

    /// Put two fanned-out value outcomes back together where they agree.
    /// A number and a boolean do NOT agree - that is the whole reason
    /// `btn(r) and 1` fans out rather than needing a special value kind.
    /// The same pairwise merge for a value in flight. A number and a
    /// boolean do NOT agree, which is the whole reason `btn(r) and 1` fans
    /// out rather than needing a special value kind.
    fn merge_values(
        &mut self,
        cond: &D::Bool,
        path: &[(D::Bool, bool)],
        t: Multi<D, Value<D>>,
        f: Multi<D, Value<D>>,
    ) -> Result<Multi<D, Value<D>>> {
        let idx = path.len();
        let mut fs: Vec<Option<(State<D>, Value<D>)>> = f.into_iter().map(Some).collect();
        let mut out: Multi<D, Value<D>> = Vec::new();
        for (ts, tv) in t {
            let rest = without(&ts.path, idx);
            let partner = fs.iter().position(|slot| match slot {
                Some((s, _)) => without(&s.path, idx) == rest,
                None => false,
            });
            let Some(i) = partner else {
                out.push((ts, tv));
                continue;
            };
            let (fst, fv) = fs[i].take().unwrap();
            let joined = match (&tv, &fv) {
                (Value::Num(x), Value::Num(y)) => Some(Value::Num(self.d.sel_num(cond, x, y))),
                (Value::Bool(x), Value::Bool(y)) => Some(Value::Bool(self.d.sel_bool(cond, x, y))),
                (x, y) if x == y => Some(x.clone()),
                _ => None,
            };
            let merged = match &joined {
                Some(_) => merge(&mut self.d, cond, &rest, ts.clone(), fst.clone())?,
                None => None,
            };
            match (merged, joined) {
                (Some(m), Some(v)) => out.push((m, v)),
                _ => {
                    out.push((ts, tv));
                    out.push((fst, fv));
                }
            }
        }
        out.extend(fs.into_iter().flatten());
        Ok(out)
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
    ) -> Result<Multi<D, Value<D>>> {
        match p {
            ast::Prefix::Name(t) => {
                let n = ident(t)?;
                let v = self.read_name(&n, &st);
                Ok(vec![(st, v)])
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
                    bail!(
                        "array index {} is past the end of a {}-element table",
                        i,
                        table.arr.len()
                    );
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
    ) -> Result<Multi<D, Value<D>>> {
        // A readable path, so "calling a non-function: nil" says WHICH
        // nil. Costs a string per suffix at trace time and nothing at run
        // time, and it is the difference between a five-minute diagnosis
        // and an hour of bisecting Lua.
        let base = match p {
            ast::Prefix::Name(t) => ident(t).unwrap_or_else(|_| "?".into()),
            _ => "(expr)".to_string(),
        };
        let mut cur: Multi<D, (Value<D>, String)> = self
            .eval_prefix(p, st)?
            .into_iter()
            .map(|(s, v)| (s, (v, base.clone())))
            .collect();
        for suffix in suffixes {
            let mut next: Multi<D, (Value<D>, String)> = Vec::new();
            for (st, (val, path)) in cur {
                match suffix {
                    ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                        let k = Key::Field(ident(name)?);
                        let p2 = format!("{}.{}", path, ident(name)?);
                        let v = self
                            .get_key(&val, &k, &st)
                            .map_err(|e| anyhow!("reading {}: {:#}", p2, e))?;
                        next.push((st, (v, p2)));
                    }
                    ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                        for (st, iv) in self.eval(expression, st)? {
                            let k = self.index_key(&iv)?;
                            let v = self.get_key(&val, &k, &st)?;
                            next.push((st, (v, format!("{}[..]", path))));
                        }
                    }
                    ast::Suffix::Call(ast::Call::AnonymousCall(
                        ast::FunctionArgs::Parentheses { arguments, .. },
                    )) => {
                        let mut argsets: Multi<D, Vec<Value<D>>> = vec![(st, Vec::new())];
                        for a in arguments {
                            let mut grown: Multi<D, Vec<Value<D>>> = Vec::new();
                            for (st, sofar) in argsets {
                                for (st, v) in self.eval(a, st)? {
                                    let mut xs = sofar.clone();
                                    xs.push(v);
                                    grown.push((st, xs));
                                }
                            }
                            argsets = grown;
                        }
                        for (st, args) in argsets {
                            let got = self
                                .call_value(val.clone(), args, st)
                                .map_err(|e| anyhow!("calling {}: {:#}", path, e))?;
                            next.extend(
                                got.into_iter().map(|(s, v)| (s, (v, format!("{}()", path)))),
                            );
                        }
                    }
                    other => bail!("unsupported suffix {:?}", other),
                }
            }
            cur = next;
        }
        Ok(cur.into_iter().map(|(s, (v, _))| (s, v)).collect())
    }

    fn assign_var(
        &mut self,
        var: &'a ast::Var,
        v: Value<D>,
        st: State<D>,
    ) -> Result<Vec<State<D>>> {
        match var {
            ast::Var::Name(t) => {
                let name = ident(t)?;
                let mut st = st;
                if !st.heap.assign(st.scope, &name, v.clone()) {
                    let g = st.globals;
                    st.heap.tables.get_mut(&g).unwrap().hash.insert(name, v);
                }
                Ok(vec![st])
            }
            ast::Var::Expression(e) => {
                let suffixes: Vec<_> = e.suffixes().collect();
                let Some((last, init)) = suffixes.split_last() else {
                    bail!("assignment target with no suffixes")
                };
                let mut out = Vec::new();
                for (st, target) in self.walk_suffixes(e.prefix(), init, st)? {
                    match last {
                        ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                            let mut st = st;
                            let k = Key::Field(ident(name)?);
                            self.set_key(&target, k, v.clone(), &mut st)?;
                            out.push(st);
                        }
                        ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                            for (mut st, iv) in self.eval(expression, st)? {
                                let k = self.index_key(&iv)?;
                                self.set_key(&target, k, v.clone(), &mut st)?;
                                out.push(st);
                            }
                        }
                        other => bail!("cannot assign through {:?}", other),
                    }
                }
                Ok(out)
            }
            other => bail!("unsupported assignment target {:?}", other),
        }
    }

    fn eval_call(
        &mut self,
        call: &'a ast::FunctionCall,
        st: State<D>,
    ) -> Result<Multi<D, Value<D>>> {
        let suffixes: Vec<_> = call.suffixes().collect();
        self.walk_suffixes(call.prefix(), &suffixes, st)
    }

    /// A call is RECURSION - hand the callee the state, get it back. There
    /// is no inlining because there is nothing to inline into, and a call
    /// that fans out is just a call that fans out.
    fn call_value(
        &mut self,
        f: Value<D>,
        args: Vec<Value<D>>,
        st: State<D>,
    ) -> Result<Multi<D, Value<D>>> {
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
                    st.heap
                        .declare(frame, &name, args.get(i).cloned().unwrap_or(Value::Nil));
                }
                let outer = st.scope;
                // The caller's scope must stay a GC ROOT while the callee
                // runs: the frame's parent is the closure's captured
                // scope, not the caller's.
                st.stack.push(outer);
                st.scope = frame;
                let mut out = Vec::new();
                for (mut s, flow) in self.exec_block(b.block(), st)? {
                    s.stack.pop();
                    s.scope = outer;
                    out.push(match flow {
                        Flow::Return(v) => (s, v),
                        Flow::Normal => (s, Value::Nil),
                        Flow::Break => bail!("break outside a loop"),
                    });
                }
                Ok(out)
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
    ) -> Result<Multi<D, Value<D>>> {
        let num = |v: &Value<D>| -> Result<D::Num> {
            match v {
                Value::Num(n) => Ok(n.clone()),
                other => bail!("{}: expected a number, got {:?}", name, other),
            }
        };
        let one = match name {
            "abs" | "flr" | "sin" => {
                let f = match name {
                    "abs" => Fun1::Abs,
                    "flr" => Fun1::Flr,
                    _ => Fun1::Sin,
                };
                let a = num(&args[0])?;
                (st, Value::Num(self.d.fun1(f, &a)?))
            }
            // The map is DATA, not code, and it is concrete - so a lookup
            // with concrete coordinates folds to a constant here exactly
            // as it would in the interpreter.
            "mget" | "fget" => {
                let cart = self
                    .cart
                    .clone()
                    .ok_or_else(|| anyhow!("{}: no cart loaded", name))?;
                let (a, b) = (num(&args[0])?, num(&args[1])?);
                match (self.d.as_const(&a), self.d.as_const(&b)) {
                    (Some(x), Some(y)) => {
                        if name == "mget" {
                            let t = cart.mget(x, y)?;
                            (st, Value::Num(self.d.num(P8::from_i16(t as i16))))
                        } else {
                            let r = cart.fget(x, y)?;
                            (st, Value::Bool(self.d.boolean(r)))
                        }
                    }
                    // Inside a tile scan whose bounds came out symbolic.
                    _ if name == "mget" => (st, Value::Num(self.d.mget(&a, &b)?)),
                    _ => bail!("fget with unknown arguments is not modelled"),
                }
            }
            // Native, exactly as the IR pipeline makes it. With concrete
            // coordinates it folds to a constant here; otherwise it is one
            // graph node, where tracing the Lua would have been a scan
            // loop over a symbolic range.
            "tile_flag_at" => {
                let (x, y, w, h, fl) = (
                    num(&args[0])?,
                    num(&args[1])?,
                    num(&args[2])?,
                    num(&args[3])?,
                    num(&args[4])?,
                );
                let known = |d: &D, v: &D::Num| d.as_const(v);
                match (
                    known(&self.d, &x),
                    known(&self.d, &y),
                    known(&self.d, &w),
                    known(&self.d, &h),
                    known(&self.d, &fl),
                ) {
                    (Some(x), Some(y), Some(w), Some(h), Some(f)) => {
                        let cache = self
                            .cache
                            .clone()
                            .ok_or_else(|| anyhow!("tile_flag_at: no collision cache"))?;
                        let cart = self
                            .cart
                            .clone()
                            .ok_or_else(|| anyhow!("tile_flag_at: no cart"))?;
                        let r = if f.as_i16() != Some(0) {
                            false
                        } else {
                            let gi = |v: P8| v.as_i16().ok_or_else(|| anyhow!("tile_flag_at: non-integer"));
                            cache.solid_at(&cart, gi(x)?, gi(y)?, gi(w)?, gi(h)?)?
                        };
                        (st, Value::Bool(self.d.boolean(r)))
                    }
                    _ => {
                        let b = self.d.tile_flag_at(&x, &y, &w, &h, &fl)?;
                        (st, Value::Bool(b))
                    }
                }
            }
            "print" | "__print" => (st, Value::Nil),
            // A merge HINT. The frontend turns it into a block flag so the
            // interpreter's worklist accumulates states there; the tracer
            // merges at every join by construction.
            "_hint_normalize" => (st, Value::Nil),
            // Shrinking an array is a SHAPE change, which is exactly what
            // the tracer is built to let happen.
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
            // On an EXACT value the floor is unique, so there is one
            // fragment and this is the identity. It only splits once the
            // value has been widened to an interval - which the boundary
            // does, not the frame (see plans/tracing.md).
            "__split_by_flr" | "__split_at" => (st, args[0].clone()),
            "__new_unknown_boolean" => (st, Value::Bool(self.d.unknown_bool()?)),
            "min" | "max" => {
                let f = if name == "min" { Fun2::Min } else { Fun2::Max };
                let (a, b) = (num(&args[0])?, num(&args[1])?);
                (st, Value::Num(self.d.fun2(f, &a, &b)?))
            }
            other => bail!("unsupported builtin {:?}", other),
        };
        Ok(vec![one])
    }
}

/// A numeric `for`'s limit: known now, or only at run time.
enum Bound<D: Domain> {
    Concrete(P8, P8),
    Symbolic(D::Num, D::Num),
}

/// A resolved table key. Concrete by construction - see `index_key`.
enum Key {
    Field(String),
    Index(i16),
}

/// How far to unroll a loop whose limit the tracer cannot know.
///
/// Keyed by the LIMIT EXPRESSION'S SOURCE TEXT, not a line number: the
/// chunk concatenates the two builtin files ahead of the cart, so absolute
/// lines move whenever those change.
///
/// This is a PERFORMANCE choice only. Every unrolled loop carries a
/// run-time predicate that it actually finished, so a bound that is too
/// small - or a key that stops matching - fails loudly instead of quietly
/// truncating the loop.
///
/// The cart has exactly two such loops, the pixel-steppers in
/// `obj.move_x` / `obj.move_y`. The other four symbolic loops were inside
/// `tile_flag_at`, which is a native builtin here.
fn unroll_bound(limit_src: &str) -> Option<u32> {
    match limit_src {
        // `for i=start,abs(amount)` / `for i=0,abs(amount)`: the player
        // moves well under 8 px in a frame.
        "abs(amount)" => Some(8),
        // The tile scans in `spikes_at` / `solid_at`: a range clamped to
        // [0, 15] but only ever a tile or two wide for these hitboxes.
        "min(15,(x+w-1)/8)" | "min(15,(y+h-1)/8)" => Some(3),
        _ => None,
    }
}

/// The one index at which two paths disagree, if there is exactly one.
/// Two outcomes like that are the two sides of a branch - whatever created
/// them - so they can be merged on that literal.
fn differ_at_one<B: PartialEq>(a: &[(B, bool)], b: &[(B, bool)]) -> Option<usize> {
    if a.len() != b.len() {
        return None;
    }
    let mut found = None;
    for (i, (x, y)) in a.iter().zip(b.iter()).enumerate() {
        if x.0 != y.0 {
            return None;
        }
        if x.1 != y.1 {
            if found.is_some() {
                return None;
            }
            found = Some(i);
        }
    }
    found
}

/// A path with the literal at `idx` removed - what two outcomes of the
/// same join must agree on to be paired. Everything after `idx` came from
/// branches INSIDE the arms, so two outcomes that differ there are
/// genuinely different and stay apart.
fn without<B: Clone>(path: &[(B, bool)], idx: usize) -> Vec<(B, bool)> {
    let mut v = path.to_vec();
    if idx < v.len() {
        v.remove(idx);
    }
    v
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

    fn fresh<D: Domain>(_d: &mut D) -> State<D> {
        let mut heap: Heap<D> = Heap::default();
        let globals = heap.new_table();
        let scope = heap.new_scope(None);
        State { heap, globals, scope, stack: Vec::new(), path: Vec::new(), ok: Vec::new() }
    }

    /// Run `src` and read back the global `result`.
    fn run<'a, D: Domain>(d: D, ast: &'a full_moon::ast::Ast) -> (Interp<'a, D>, Value<D>) {
        let mut it = Interp::new(d);
        let st = fresh::<D>(&mut it.d);
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
        let mut st = fresh::<Symbolic>(&mut it.d);
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
