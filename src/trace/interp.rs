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
use super::state::{merge, split, State};

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

}

/// Can these two values be joined into one? Only numbers and booleans
/// can differ - everything else is concrete, so it either matches
/// outright or the two states are genuinely different successors.
fn joinable<D: Domain>(a: &Value<D>, b: &Value<D>) -> bool {
    matches!(
        (a, b),
        (Value::Num(_), Value::Num(_)) | (Value::Bool(_), Value::Bool(_))
    ) || a == b
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
    /// AST node -> its id, so evaluating the same `function ... end`
    /// twice yields the SAME closure body.
    ///
    /// This is not a cache. Minting a fresh id per evaluation made two
    /// states that had built the same closure structurally different, and
    /// since a `Func` slot is part of the SHAPE, they then could not
    /// merge. It cost ten unmergeable outcomes on the first frame that
    /// was traced as a kernel, all of them the same 110 scalars and
    /// differing only in these numbers.
    body_ids: std::collections::HashMap<*const ast::FunctionBody, BodyId>,
    /// Paths poisoned by `poison`: a Lua type error means no legal run
    /// takes that path, so its lanes deopt rather than aborting the
    /// trace. Counted by reason, because turning an error into a deopt
    /// would otherwise hide a modelling gap at build time.
    pub illegal: std::collections::BTreeMap<String, usize>,
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
    /// What `printh` has printed, in order. This exists so a program can
    /// be run in the tracer AND in real PICO-8 and the two outputs
    /// compared line for line (`lua/probe/`), which is the only way to
    /// settle a question like what `#` does to a table with a hole in it.
    pub prints: Vec<String>,
    /// `(room x, room y, flag bit) -> does any tile in that room carry
    /// it`. Answering costs 256 map lookups, and `ice_at` asks every
    /// frame for every object.
    room_flags: std::collections::HashMap<(i16, i16, i16), bool>,
}

impl<'a, D: Domain> Interp<'a, D> {
    pub fn new(d: D) -> Self {
        Interp {
            d,
            bodies: Vec::new(),
            body_ids: std::collections::HashMap::new(),
            illegal: Default::default(),
            cart: None,
            cache: None,
            max_states: 256,
            max_nodes: 2_000_000,
            prints: Vec::new(),
            room_flags: std::collections::HashMap::new(),
        }
    }

    fn intern_body(&mut self, b: &'a ast::FunctionBody) -> BodyId {
        // By POINTER: the AST outlives the interpreter and never moves,
        // so the address is a stable name for the syntax. Two closures
        // over the same syntax still differ when they captured different
        // scopes - that is what `Func::env` is for.
        let k = b as *const ast::FunctionBody;
        if let Some(id) = self.body_ids.get(&k) {
            return *id;
        }
        self.bodies.push(b);
        let id = (self.bodies.len() - 1) as BodyId;
        self.body_ids.insert(k, id);
        id
    }

    // ------------------------------------------------------------ blocks

    /// A BLOCK IS A SCOPE. Without this, a `local` declared inside an
    /// `if` arm lands in the enclosing FUNCTION's scope and outlives the
    /// arm, which is not what Lua does:
    ///
    /// ```text
    /// local m=1 if true then local m=2 printh(m) end printh(m)   -- 2, then 1
    /// if true then local leaked=7 end printh(leaked)             -- [nil]
    /// ```
    ///
    /// Nothing in this cart shadows a name, so it was never a wrong
    /// VALUE. It was a merge failure: a leaked name is part of
    /// `Shape::scopes`, so the two arms of `if this.dash_time>0 then ..
    /// else <maxrun, accel, deccel, maxfall, gravity, ..> end` ended with
    /// different scope key sets and could not merge.
    ///
    /// Only when the block actually declares something. A scope object
    /// per `if` arm would otherwise be allocated, walked by GC and
    /// canonicalised for every branch in the program, to hold nothing.
    pub fn exec_block(&mut self, block: &'a ast::Block, st: State<D>) -> Result<Outcome<D>> {
        if !declares_local(block) {
            return self.exec_block_flat(block, st);
        }
        let mut st = st;
        let outer = st.scope;
        st.scope = st.heap.new_scope(Some(outer));
        let mut out = self.exec_block_flat(block, st)?;
        for (s, _) in out.iter_mut() {
            s.scope = outer;
        }
        Ok(out)
    }

    fn exec_block_flat(&mut self, block: &'a ast::Block, st: State<D>) -> Result<Outcome<D>> {
        let mut live: Outcome<D> = vec![(st, Flow::Normal)];
        for stmt in block.stmts() {
            let mut next: Outcome<D> = Vec::new();
            for (s, f) in live {
                if !f.is_normal() {
                    next.push((s, f));
                    continue;
                }
                // A POISONED path executes no further. `ok` is already
                // false, so every lane on it deopts and nothing it goes
                // on to compute can be read - but it would still intern
                // nodes, and on the shape walk that was most of a
                // two-million-node graph. Carried rather than dropped:
                // dropping is `guard`'s direction, and a successor that
                // vanishes is the one failure nothing downstream sees.
                if self.d.decide(&s.ok) == Some(false) {
                    next.push((s, f));
                    continue;
                }
                next.extend(self.exec_stmt(stmt, s)?);
            }
            live = self.collapse(next)?;
            if live.len() > self.max_states {
                let mut hist: std::collections::BTreeMap<&str, usize> = Default::default();
                for (st, f) in &live {
                    let k = match f {
                        Flow::Normal => "normal",
                        Flow::Break => "break",
                        Flow::Return(_) => "return",
                    };
                    let _ = st;
                    *hist.entry(k).or_default() += 1;
                }
                eprintln!("[trace] frontier by flow: {:?}", hist);
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
                // TARGET FIRST, then the value.
                let mut out = Vec::new();
                for (s, tgt) in self.resolve_target(var, st)? {
                    for (mut s2, v) in self.eval(e, s)? {
                        self.store(&tgt, v, &mut s2)?;
                        out.push(s2);
                    }
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
                let c = s.heap.new_closure(body, s.scope);
                let v = Value::Func(c);
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
            out.extend(match self.decide(&cond) {
                Some(true) => self.exec_block(blk, s)?,
                Some(false) => self.exec_arms(rest, els, s)?,
                None => {
                    // Each arm only happens under its own condition, so
                    // record that before running it. A merge puts the
                    // original back, because either side happening IS the
                    // original condition.
                    let (ts, fs) = split(&mut self.d, s, &cond);
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
                        {
                            let mut all = t;
                            all.extend(f);
                            self.collapse(all)?
                        }
                    }
                }
            });
        }
        Ok(out)
    }

    /// Merge every pair of outcomes that can be merged, to a fixpoint.
    ///
    /// Any two same-kind outcomes with a mergeable shape now merge, on
    /// `t`'s guard - there is no sibling relation to establish first. The
    /// rule this replaces could only pair outcomes differing in exactly
    /// ONE assumed literal, which is sound (two such outcomes cover their
    /// parent) but far too weak: an `elseif` ladder inside a loop
    /// accumulates one `return` per rung, each a literal deeper than the
    /// last, and `spikes_at` reached 276 of them - all returning `true`
    /// from a function that mutates nothing. One behaviour the rule could
    /// not see.
    ///
    /// Run after every statement, so the collapse cascades bottom-up and
    /// the frontier stays at the number of genuinely different futures
    /// rather than the product of every branch taken to get there.
    pub fn collapse(&mut self, mut outs: Outcome<D>) -> Result<Outcome<D>> {
        'again: loop {
            for i in 0..outs.len() {
                for j in (i + 1)..outs.len() {
                    if !self.can_join(&outs[i].1, &outs[j].1) {
                        continue;
                    }
                    let (a, b) = (outs[i].0.clone(), outs[j].0.clone());
                    let Some(m) = merge(&mut self.d, a, b)? else {
                        continue;
                    };
                    let cond = outs[i].0.guard.clone();
                    let fl = self.join_flow(&cond, &outs[i].1, &outs[j].1);
                    // j > i, so drop the later index first.
                    outs.remove(j);
                    outs.remove(i);
                    outs.push((m, fl));
                    continue 'again;
                }
            }
            break;
        }
        Ok(outs)
    }

    /// The same fixpoint for a fanned-out EXPRESSION, where the outcomes
    /// carry a value instead of a flow.
    pub fn collapse_values(
        &mut self,
        mut outs: Multi<D, Value<D>>,
    ) -> Result<Multi<D, Value<D>>> {
        'again: loop {
            for i in 0..outs.len() {
                for j in (i + 1)..outs.len() {
                    if !joinable(&outs[i].1, &outs[j].1) {
                        continue;
                    }
                    let (a, b) = (outs[i].0.clone(), outs[j].0.clone());
                    let Some(m) = merge(&mut self.d, a, b)? else {
                        continue;
                    };
                    let cond = outs[i].0.guard.clone();
                    let v = self.join_value(&cond, &outs[i].1.clone(), &outs[j].1.clone());
                    outs.remove(j);
                    outs.remove(i);
                    outs.push((m, v));
                    continue 'again;
                }
            }
            break;
        }
        Ok(outs)
    }

    /// Can these two flows be joined - WITHOUT building any nodes? Asked
    /// before the merge so a rejected pair costs nothing; `collapse` now
    /// tries every pair rather than only siblings, so most pairs are
    /// rejected and building a `Sel` for each would be pure garbage.
    fn can_join(&self, a: &Flow<D>, b: &Flow<D>) -> bool {
        match (a, b) {
            (Flow::Return(x), Flow::Return(y)) => joinable(x, y),
            (Flow::Break, Flow::Break) | (Flow::Normal, Flow::Normal) => true,
            _ => false,
        }
    }

    fn join_flow(&mut self, cond: &D::Bool, a: &Flow<D>, b: &Flow<D>) -> Flow<D> {
        match (a, b) {
            (Flow::Return(x), Flow::Return(y)) => {
                Flow::Return(self.join_value(cond, &x.clone(), &y.clone()))
            }
            (Flow::Break, _) => Flow::Break,
            _ => Flow::Normal,
        }
    }

    /// Only numbers and booleans actually differ; `joinable` has already
    /// established that this pair is one of those or is equal outright.
    fn join_value(&mut self, cond: &D::Bool, a: &Value<D>, b: &Value<D>) -> Value<D> {
        match (a, b) {
            (Value::Num(x), Value::Num(y)) => Value::Num(self.d.sel_num(cond, x, y)),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(self.d.sel_bool(cond, x, y)),
            (x, _) => x.clone(),
        }
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
        // Two lists, exactly as `run_for` keeps them. A state LEAVES the
        // loop when it goes past the limit, breaks, or returns, and a
        // state that has left must not re-enter.
        //
        // `for_body` used to rewrite `Flow::Break` to `Flow::Normal`, so a
        // broken-out state went straight back into `running` and ran the
        // body again - `break` was a no-op on this path. `run_for` has a
        // comment saying that exact bug was fixed there once; it was
        // still here.
        //
        // Splitting out the states that went PAST the limit matters for a
        // second reason. They used to be merged back into the frontier
        // and re-tested at every later k, and since the guard is opaque
        // the tracer cannot see that `i <= limit` is already false - so it
        // split again, and explored a body under `g and c and not c`.
        // `Graph::fold` only collapses a constant `And`, so those states
        // are built, traced and selected away rather than never existing.
        let mut running: Outcome<D> = vec![(s, Flow::Normal)];
        let mut done: Outcome<D> = Vec::new();
        for k in 0..bound {
            if running.is_empty() {
                break;
            }
            let mut next: Outcome<D> = Vec::new();
            for (s, fl) in std::mem::take(&mut running) {
                debug_assert!(fl.is_normal(), "only a running state re-enters the body");
                let off = self.d.num(P8::from_i16(k as i16));
                let iv = self.d.arith(Arith::Add, &start, &off)?;
                let cond = self.d.compare(Cmp::Le, &iv, &limit)?;
                match self.decide(&cond) {
                    // Past the limit on this path: out of the loop.
                    Some(false) => done.push((s, Flow::Normal)),
                    Some(true) => next.extend(self.for_body(f, name, &iv, s)?),
                    None => {
                        let (ts, fs) = split(&mut self.d, s, &cond);
                        if let Some(x) = ts {
                            next.extend(self.for_body(f, name, &iv, x)?);
                        }
                        if let Some(x) = fs {
                            done.push((x, Flow::Normal));
                        }
                    }
                }
            }
            for (s, fl) in self.collapse(next)? {
                match fl {
                    // A `break` ends the LOOP, not the state.
                    Flow::Break => done.push((s, Flow::Normal)),
                    Flow::Return(v) => done.push((s, Flow::Return(v))),
                    Flow::Normal => running.push((s, Flow::Normal)),
                }
            }
        }
        // The obligation, and it belongs ONLY to the states that never
        // left: by the time the bound ran out, the loop must be over.
        // Applying it to a state that broke or returned would deopt a
        // lane for a bound it never depended on.
        let off = self.d.num(P8::from_i16(bound as i16));
        let iv = self.d.arith(Arith::Add, &start, &off)?;
        let over = self.d.compare(Cmp::Le, &iv, &limit)?;
        let finished = self.d.not(&over);
        for (s, _) in running.iter_mut() {
            s.ok = self.d.and(&s.ok, &finished);
        }
        done.extend(running);
        self.collapse(done)
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
            // `Flow::Break` is passed THROUGH. The caller decides what
            // leaving the loop means; swallowing it here made `break` a
            // no-op for the symbolic-bound loop.
            out.push((s2, f2));
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
    /// The condition's value, if the domain knows it. There is nothing
    /// else to consult: a state's guard is opaque by design, and the
    /// measurement that justified looking inside it stopped firing once
    /// `and`/`or` handed on the constant they had just split on.
    fn decide(&self, cond: &D::Bool) -> Option<bool> {
        self.d.decide(cond)
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
                            // `Table::len` is Lua's `luaH_getn`, checked
                            // against real PICO-8 by `lua/probe/tables.lua`.
                            // It is NOT `arr.len()`: a hole makes the
                            // answer a border, and which border you get
                            // depends on how the table was built.
                            let len = st.heap.tables[&t].len().ok_or_else(|| {
                                anyhow!(
                                    "# of a table this model cannot measure exactly: PICO-8's \
                                     length searches the array part's CAPACITY, which depends \
                                     on rehash history, not on the keys"
                                )
                            })? as i16;
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
                let mut st = st;
                let env = st.scope;
                let c = st.heap.new_closure(id, env);
                Ok(vec![(st, Value::Func(c))])
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
                match self.decide(&t) {
                    Some(x) if x == is_and => out.extend(self.eval(rhs, st)?),
                    Some(_) => out.push((st, a)),
                    None => {
                        let taken_cond = if is_and { t.clone() } else { self.d.not(&t) };
                        let (ts, fs) = split(&mut self.d, st, &taken_cond);
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
                            let mut all = taken;
                            all.extend(kept);
                            out.extend(self.collapse_values(all)?);
                        }
                    }
                }
            }
            return Ok(out);
        }
        let mut out = Vec::new();
        for (st, a) in self.eval(lhs, st)? {
            for (mut st, b) in self.eval(rhs, st)? {
                let (v, illegal) = self.binop_values(binop, &a, &b)?;
                if let Some(why) = illegal {
                    // The source text, built only when it is needed: a
                    // type error is useless without knowing which
                    // expression, and `to_string` on every binop is not.
                    let t = format!("{}{}{}", lhs, binop, rhs);
                    let t = t.trim();
                    self.poison(&mut st, format!("`{}`: {}", &t[..t.len().min(48)], why));
                }
                out.push((st, v));
            }
        }
        Ok(out)
    }

    /// Mark this path as one no legal run takes, and keep going.
    ///
    /// PICO-8 raises on `nil > 0`, so a path that does it is not a game
    /// execution at all - it is a path the tracer reached only because
    /// it over-approximated a branch. The real game holds an invariant
    /// the tracer cannot see; `spring.init` never sets `delay`, and
    /// `spring.update` reads it only in a branch that a prior branch
    /// assigns it in first.
    ///
    /// `ok`, NOT `guard`. Those mean different things and only one is
    /// safe here. Clearing `guard` would say "no lane takes this
    /// path", and if that were ever wrong the successor would silently
    /// vanish. Clearing `ok` says "the kernel declines these lanes",
    /// which is the direction that fails loudly - and under the
    /// never-deopt doctrine a lane that really got here stops the run
    /// and names itself, so a wrong invariant is reported rather than
    /// assumed.
    ///
    /// Counted in `illegal` rather than swallowed. Turning an error into
    /// a deopt hides modelling gaps at BUILD time - the shape walk would
    /// happily include shapes only reachable through impossible paths -
    /// so the count is what keeps that visible.
    fn poison(&mut self, st: &mut State<D>, why: String) {
        st.ok = self.d.boolean(false);
        *self.illegal.entry(why).or_default() += 1;
    }

    /// The value, and - if Lua itself would have raised - why this path
    /// is not a legal run. The caller poisons, because it is the caller
    /// that knows which expression this was.
    fn binop_values(
        &mut self,
        binop: &ast::BinOp,
        a: &Value<D>,
        b: &Value<D>,
    ) -> Result<(Value<D>, Option<String>)> {
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
                // Lua raises here, so this path is not a legal run.
                let zero = Value::Num(self.d.num(P8::from_i16(0)));
                let why = format!("arithmetic on {} and {}", kind_of(a), kind_of(b));
                return Ok((zero, Some(why)));
            };
            return Ok((Value::Num(self.d.arith(op, x, y)?), None));
        }
        let op = cmp_op.unwrap();
        let r = match (a, b) {
            (Value::Num(x), Value::Num(y)) => self.d.compare(op, x, y)?,
            // Two booleans compare by VALUE in Lua, so this is
            // answerable exactly rather than by comparing node ids -
            // which is what the fallthrough below would do, making two
            // distinct symbolic booleans unconditionally unequal and
            // `b == true` unconditionally false.
            (Value::Bool(x), Value::Bool(y)) if op == Cmp::Eq => {
                let both = self.d.and(x, y);
                let (nx, ny) = (self.d.not(x), self.d.not(y));
                let neither = self.d.and(&nx, &ny);
                self.d.or(&both, &neither)
            }
            // Two closures. The SAME object is equal in any model, so
            // that much is answerable; anything else is not. PICO-8 is
            // Lua 5.2 and caches closures on (prototype, upvalue cells),
            // so two closures this model calls distinct can be one object
            // there - `function() end` evaluated twice is cached, because
            // it captures nothing. This model approximates the cells by
            // the enclosing scope and so cannot tell. Refuse rather than
            // answer; the cart never compares two functions (every `==`
            // and `~=` in the three Lua files was checked, and the only
            // function comparisons are against nil).
            (Value::Func(x), Value::Func(y)) if op == Cmp::Eq => {
                if x != y {
                    bail!(
                        "comparing two closures: PICO-8 caches them on (prototype, \
                         upvalue cells) and this model approximates the cells by the \
                         enclosing scope, so it cannot tell whether these are one object"
                    );
                }
                self.d.boolean(true)
            }
            // Everything but numbers and booleans is concrete, so equality
            // on it is decidable right here.
            _ if op == Cmp::Eq => {
                let same = a == b;
                self.d.boolean(same)
            }
            // Lua raises when an ordered comparison gets a non-number,
            // so this path is not a legal run either.
            _ => {
                let why = format!("comparison of {} and {}", kind_of(a), kind_of(b));
                return Ok((Value::Bool(self.d.boolean(false)), Some(why)));
            }
        };
        let v = Value::Bool(if matches!(binop, ast::BinOp::TildeEqual(_)) {
            self.d.not(&r)
        } else {
            r
        });
        Ok((v, None))
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
            Key::Index(i) => table.get_index(*i).cloned().unwrap_or(Value::Nil),
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
            Key::Index(i) => table.set_index(i, v),
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

    /// Work out WHERE an assignment will store, without storing. Split
    /// out from the store itself so the left-hand side can be evaluated
    /// BEFORE the right-hand side, which is the order Lua uses:
    /// `t[f()] = g()` calls `f` and then `g`. This model used to do it
    /// the other way round.
    fn resolve_target(
        &mut self,
        var: &'a ast::Var,
        st: State<D>,
    ) -> Result<Multi<D, Target<D>>> {
        match var {
            ast::Var::Name(t) => Ok(vec![(st, Target::Name(ident(t)?))]),
            ast::Var::Expression(e) => {
                let suffixes: Vec<_> = e.suffixes().collect();
                let Some((last, init)) = suffixes.split_last() else {
                    bail!("assignment target with no suffixes")
                };
                let mut out = Vec::new();
                for (st, target) in self.walk_suffixes(e.prefix(), init, st)? {
                    match last {
                        ast::Suffix::Index(ast::Index::Dot { name, .. }) => {
                            out.push((st, Target::Field(target, Key::Field(ident(name)?))));
                        }
                        ast::Suffix::Index(ast::Index::Brackets { expression, .. }) => {
                            for (st, iv) in self.eval(expression, st)? {
                                let k = self.index_key(&iv)?;
                                out.push((st, Target::Field(target.clone(), k)));
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

    /// Does any tile in the loaded room carry this flag bit? The room is
    /// in the heap (`room.x` / `room.y`), so this needs no plumbing.
    fn room_has_flag(&mut self, st: &State<D>, flag: i16) -> Result<bool> {
        let Some(Value::Table(rt)) = st.heap.tables[&st.globals].hash.get("room").cloned() else {
            bail!("tile_flag_at: no `room` global to check the flag against");
        };
        let coord = |k: &str| -> Result<i16> {
            match st.heap.tables[&rt].hash.get(k) {
                Some(Value::Num(n)) => self
                    .d
                    .as_const(n)
                    .and_then(|v| v.as_i16())
                    .ok_or_else(|| anyhow!("tile_flag_at: room.{} is not a known integer", k)),
                other => bail!("tile_flag_at: room.{} is {:?}", k, other),
            }
        };
        let (rx, ry) = (coord("x")?, coord("y")?);
        if let Some(v) = self.room_flags.get(&(rx, ry, flag)) {
            return Ok(*v);
        }
        let cart = self.cart.clone().ok_or_else(|| anyhow!("tile_flag_at: no cart"))?;
        let mut found = false;
        'scan: for i in 0..16i16 {
            for j in 0..16i16 {
                let t = cart.mget(P8::from_i16(rx * 16 + i), P8::from_i16(ry * 16 + j))?;
                if cart.fget(P8::from_i16(t as i16), P8::from_i16(flag))? {
                    found = true;
                    break 'scan;
                }
            }
        }
        self.room_flags.insert((rx, ry, flag), found);
        Ok(found)
    }

    fn store(&mut self, t: &Target<D>, v: Value<D>, st: &mut State<D>) -> Result<()> {
        match t {
            Target::Name(name) => {
                if !st.heap.assign(st.scope, name, v.clone()) {
                    let g = st.globals;
                    st.heap.tables.get_mut(&g).unwrap().hash.insert(name.clone(), v);
                }
            }
            Target::Field(tab, k) => self.set_key(tab, k.clone(), v, st)?,
        }
        Ok(())
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
            Value::Func(c) => {
                let mut st = st;
                let super::heap::Closure { body, env } = *st
                    .heap
                    .closures
                    .get(&c)
                    .ok_or_else(|| anyhow!("calling a collected closure #{}", c))?;
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
        // By INDEX, and bounds-checked. `min(5)` used to be a Rust index
        // panic on `args[1]`. PICO-8 answers it (`min(5)` is 0, `max(5)`
        // is 5, treating the absent argument as 0), but the cart always
        // passes two, and a coercion of nil to 0 is not something to
        // infer from one measurement - so this raises, which is the other
        // half of "match exactly or raise".
        let num = |i: usize| -> Result<D::Num> {
            let v = args.get(i).ok_or_else(|| {
                anyhow!("{}: needs at least {} arguments, got {}", name, i + 1, args.len())
            })?;
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
                let a = num(0)?;
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
                let (a, b) = (num(0)?, num(1)?);
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
                let (x, y, w, h, fl) = (num(0)?, num(1)?, num(2)?, num(3)?, num(4)?);
                let gi = |v: P8| v.as_i16().ok_or_else(|| anyhow!("tile_flag_at: non-integer"));
                // THE FLAG DECIDES FIRST, before the coordinates.
                //
                // A non-zero flag is ice, and only flag 0 (solid) is
                // modelled. Returning `false` for every other flag - which
                // is what this did, and what the interpreter still does
                // (`game_runner.rs`, with a "not ideal but allows testing"
                // comment) - is right only where the room contains no such
                // tile. 16 tiles carry flag 4 across map rows 2 and 3, and
                // `ice_at` gates acceleration and the wall-slide every
                // frame, so the first search in one of those rooms gets
                // silently wrong physics. In BOTH engines at once, which
                // is why the differential gate cannot see it.
                //
                // Checking the flag before the coordinates matters: with a
                // symbolic player position `ice_at` has unknown x and y,
                // so an earlier version of this guard sat in the
                // all-known branch and the call went straight past it into
                // a graph node. Whether a room contains a flag does not
                // depend on where in the room you look.
                let Some(fi) = self.d.as_const(&fl).map(gi).transpose()? else {
                    bail!("tile_flag_at with an unknown flag");
                };
                if fi != 0 {
                    if self.room_has_flag(&st, fi)? {
                        bail!(
                            "tile_flag_at with flag {} in a room that CONTAINS that flag: \
                             only flag 0 (solid) is modelled",
                            fi
                        );
                    }
                    // No tile in this room carries it, so the answer is
                    // false for every rectangle in the room. Exact, and
                    // it keeps the node out of the graph entirely.
                    let b = self.d.boolean(false);
                    (st, Value::Bool(b))
                } else {
                    match (
                        self.d.as_const(&x),
                        self.d.as_const(&y),
                        self.d.as_const(&w),
                        self.d.as_const(&h),
                    ) {
                        (Some(x), Some(y), Some(w), Some(h)) => {
                            let cache = self
                                .cache
                                .clone()
                                .ok_or_else(|| anyhow!("tile_flag_at: no collision cache"))?;
                            let cart = self
                                .cart
                                .clone()
                                .ok_or_else(|| anyhow!("tile_flag_at: no cart"))?;
                            let r = cache.solid_at(&cart, gi(x)?, gi(y)?, gi(w)?, gi(h)?)?;
                            (st, Value::Bool(self.d.boolean(r)))
                        }
                        _ => {
                            let b = self.d.tile_flag_at(&x, &y, &w, &h, &fl)?;
                            (st, Value::Bool(b))
                        }
                    }
                }
            }
            "print" | "__print" => (st, Value::Nil),
            // Rendered the way PICO-8's `printh` renders a value, because
            // the golden files in `lua/probe/` are literally its stdout.
            "printh" => {
                let text = match args.first() {
                    None | Some(Value::Nil) => "[nil]".to_string(),
                    Some(Value::Str(s)) => s.to_string(),
                    Some(Value::Bool(b)) => match self.d.decide(b) {
                        Some(v) => v.to_string(),
                        None => bail!("printh of an undecided boolean"),
                    },
                    Some(Value::Num(n)) => match self.d.as_const(n) {
                        Some(v) => fmt_p8(v),
                        None => bail!("printh of a symbolic number"),
                    },
                    Some(other) => bail!("printh of {:?}", other),
                };
                self.prints.push(text);
                (st, Value::Nil)
            }
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
                // Exactly `list[#list] = nil`, which is what the shim it
                // replaces means (`del` in builtin_level_4). NOT
                // `arr.pop()`: that removes the last SLOT, and after a
                // hole has been punched the last slot is not the last
                // element. Shrinking the array part is not observationally
                // neutral either - `arr=[a,nil,nil]` has `#==1`, but
                // shrinking it to `[a]` and then writing t[3] puts 3 in
                // the integer part and gives `#==1` where leaving the
                // array part alone gives 3.
                let n = tab.len().ok_or_else(|| {
                    anyhow!("__array_table_drop_last on a table whose length is not exact")
                })?;
                if n == 0 {
                    bail!("__array_table_drop_last on an empty table");
                }
                tab.set_index(n as i16, Value::Nil);
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
                let (a, b) = (num(0)?, num(1)?);
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
#[derive(Clone)]
enum Key {
    Field(String),
    Index(i16),
}

/// A resolved assignment TARGET: where the store will go, worked out
/// before the right-hand side runs.
enum Target<D: Domain> {
    Name(String),
    Field(Value<D>, Key),
}

/// Does this block bind a name of its own? Only its OWN statements - a
/// nested block gets its own scope when it runs.
fn declares_local(block: &ast::Block) -> bool {
    block.stmts().any(|s| matches!(s, ast::Stmt::LocalAssignment(_)))
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

    fn fresh<D: Domain>(d: &mut D) -> State<D> {
        let mut heap: Heap<D> = Heap::default();
        let globals = heap.new_table();
        let scope = heap.new_scope(None);
        let t = d.boolean(true);
        State { heap, globals, scope, stack: Vec::new(), guard: t.clone(), ok: t }
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

/// A pico-8 number the way `printh` renders it: whole numbers without a
/// point, fractions to four places with trailing zeros stripped. Four is
/// not a guess - `printh(1/3)` prints `0.3333`.
fn fmt_p8(v: P8) -> String {
    if let Some(i) = v.as_i16() {
        return format!("{}", i);
    }
    let raw = v.as_raw_u32() as i32 as f64 / 65536.0;
    let s = format!("{:.4}", raw);
    let s = s.trim_end_matches('0').trim_end_matches('.').to_string();
    s
}

/// A value's Lua type name, for the messages `poison` records. The
/// VALUE is deliberately not included: two paths that fail the same way
/// on different symbolic operands are one modelling gap, not two.
fn kind_of<D: Domain>(v: &Value<D>) -> &'static str {
    match v {
        Value::Nil => "nil",
        Value::Num(_) => "number",
        Value::Bool(_) => "boolean",
        Value::Str(_) => "string",
        Value::Table(_) => "table",
        Value::Func(_) => "function",
        Value::Builtin(_) => "builtin",
    }
}
