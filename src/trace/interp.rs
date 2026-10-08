//! The AST interpreter, generic over the domain. Control flow is recursion
//! (a call hands the callee the state and gets it back; nothing is inlined).
//! Anything that runs code returns a LIST of outcomes: an undecided branch
//! runs both arms, which do not always merge back.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;
use full_moon::tokenizer::{Symbol, TokenType};

use crate::pico8_num::Pico8Num as P8;

use super::domain::{refuse_unknown, Arith, Cmp, Domain, Fun1, Fun2};
use super::heap::{BodyId, TableId, Value};
use super::state::{merge_canon, split, Canon, Sides, State};

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

/// Can these two values be joined? Only numbers and booleans may differ.
fn joinable<D: Domain>(a: &Value<D>, b: &Value<D>) -> bool {
    matches!(
        (a, b),
        (Value::Num(_), Value::Num(_)) | (Value::Bool(_), Value::Bool(_))
    ) || a == b
}

/// One or more `(state, thing)` outcomes; expressions return these too.
pub type Multi<D, T> = Vec<(State<D>, T)>;
pub type Outcome<D> = Multi<D, Flow<D>>;

/// The interpreter. Cloneable so workers can trace from copies of one
/// walked tracer.
#[derive(Clone)]
pub struct Interp<'a, D: Domain> {
    pub d: D,
    /// Function bodies, by `BodyId`.
    bodies: Vec<&'a ast::FunctionBody>,
    /// AST node -> its id, so one `function ... end` evaluated twice is the
    /// SAME body: the body is part of the SHAPE, so states must agree on it.
    body_ids: std::collections::HashMap<*const ast::FunctionBody, BodyId>,
    /// See `hint_name`.
    hint: Vec<String>,
    /// Paths poisoned by `poison` (a Lua error), counted by reason so a
    /// modelling gap stays visible.
    pub illegal: std::collections::BTreeMap<String, usize>,
    /// Each poisoned path's guard and reason; their OR is the raise row's
    /// liveness. Reset per trace by `verify::trace_frame`.
    pub raised: Vec<(D::Bool, String)>,
    /// The cart and the room's collision cache (optional for unit tests).
    pub cart: Option<std::sync::Arc<celeste_core::cart_data::CartData>>,
    pub cache: Option<std::sync::Arc<celeste_core::collision_cache::CollisionCache>>,
    /// Budgets, so a blow-up is a DIAGNOSIS naming the construct, not a hang.
    pub max_states: usize,
    pub max_nodes: usize,
    /// Graph size when this trace began: `max_nodes` bounds ONE trace.
    pub trace_start_nodes: usize,
    /// What `printh` printed, compared against real PICO-8 (`lua/probe/`).
    pub prints: Vec<String>,
    /// Loop bodies traced (`for_body` calls); a probe statistic.
    pub for_iterations: u64,
    /// Memoized `(room x, room y, flag bit) -> any tile carries it`.
    room_flags: std::collections::HashMap<(i16, i16, i16), bool>,
    /// Merges that selected on the full guard (`state::Merged::fell_back`).
    pub merge_fallbacks: usize,
    /// Literal splits handed out: the ids in `State::frag`.
    next_split: usize,
    /// THE ARC CAPTURE (`search::arc_edges`), set per level-0 trace: every
    /// `__split_by_flr` must name its site and the player's record what they
    /// did (`State::arc`).
    pub arc_capture: bool,
    /// The site of the `__split_by_flr` call being made, taken by the builtin.
    split_site: Option<SplitSite>,
}

/// What a `__split_by_flr(obj.rem.x|y)` splits: the PLAYER's remainder on an
/// axis, or another object's.
#[derive(Clone, Copy, Debug)]
enum SplitSite {
    Player(usize),
    Other,
}

impl<'a, D: Domain> Interp<'a, D> {
    pub fn new(d: D) -> Self {
        Interp {
            d,
            bodies: Vec::new(),
            body_ids: std::collections::HashMap::new(),
            hint: Vec::new(),
            illegal: Default::default(),
            raised: Vec::new(),
            cart: None,
            cache: None,
            max_states: 256,
            // `CELESTE_MAX_TRACE_NODES`: for measuring a refused trace.
            max_nodes: std::env::var("CELESTE_MAX_TRACE_NODES")
                .ok()
                .map(|v| v.parse().unwrap_or_else(|_| panic!("CELESTE_MAX_TRACE_NODES={v:?} is not a number")))
                .unwrap_or(2_000_000),
            trace_start_nodes: 0,
            prints: Vec::new(),
            for_iterations: 0,
            room_flags: std::collections::HashMap::new(),
            merge_fallbacks: 0,
            next_split: 0,
            arc_capture: false,
            split_site: None,
        }
    }

    /// The engine's index for function `name`: `FN_NAMES` spells `base_N`;
    /// this matches a unique BASE (`anonymous` is refused).
    fn fn_id_of(name: &str) -> Option<u32> {
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

    /// The name a function expression is stored under, as `FN_NAMES` spells
    /// it (`player = { init = function ... }` is `player.init`). A syntactic
    /// stack pushed around the sub-evaluation.
    fn hint_name(&self) -> Option<String> {
        if self.hint.is_empty() {
            return None;
        }
        Some(self.hint.join("."))
    }

    /// The free variables of a body that resolve to a LOCAL of the defining
    /// scope (`Cell2::Clo`'s columns), decided syntactically over variable
    /// positions only (`obj.x` does not capture `x`); the rest are globals.
    fn captures_of(
        &self,
        body: &'a ast::FunctionBody,
        env: super::heap::ScopeId,
        st: &State<D>,
    ) -> Vec<String> {
        // `Visit::visit`, not the `visit_function_body` hook (which recurses
        // into nothing).
        use full_moon::visitors::{Visit, Visitor};

        #[derive(Default)]
        struct Names {
            order: Vec<String>,
            seen: std::collections::HashSet<String>,
            /// Names bound inside the body, nested functions' too (over-subtracts).
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
                self.bound
                    .insert(f.index_variable().token().to_string().trim().to_string());
            }
            fn visit_generic_for(&mut self, f: &ast::GenericFor) {
                for n in f.names() {
                    self.bound.insert(n.token().to_string().trim().to_string());
                }
            }
        }
        let mut names = Names::default();
        body.visit(&mut names);
        names
            .order
            .iter()
            .filter(|n| !names.bound.contains(*n))
            .filter(|n| st.heap.lookup(env, n).is_some())
            .cloned()
            .collect()
    }

    fn intern_body(&mut self, b: &'a ast::FunctionBody) -> BodyId {
        // By POINTER: the AST outlives the interpreter and never moves.
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

    /// Run a block. A BLOCK IS A SCOPE, as in Lua: a leaked `local` is part
    /// of `Shape::scopes` and would keep arms from merging. A scope is
    /// allocated only when the block declares something.
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
                // A RAISED path executes no further (its lanes are in the
                // raise row).
                if self.d.decide(&s.guard) == Some(false) {
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
            if self.d.node_count().saturating_sub(self.trace_start_nodes) > self.max_nodes {
                bail!(
                    "the trace grew the graph by {} nodes (limit {}; {} in the arena)",
                    self.d.node_count().saturating_sub(self.trace_start_nodes),
                    self.max_nodes,
                    self.d.node_count()
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
                                self.hint.push(n.clone());
                                let vs = self.eval(e, s);
                                self.hint.pop();
                                for (mut s, v) in vs? {
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
                let hint = target_hint(var);
                let mut out = Vec::new();
                for (s, tgt) in self.resolve_target(var, st)? {
                    if let Some(h) = &hint {
                        self.hint.push(h.clone());
                    }
                    let vs = self.eval(e, s);
                    if hint.is_some() {
                        self.hint.pop();
                    }
                    for (mut s2, v) in vs? {
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
                let name = f.name().to_string().trim().to_string();
                if name.contains('.') || name.contains(':') {
                    bail!("qualified function names are not supported: {:?}", name);
                }
                let caps = self.captures_of(f.body(), s.scope, &s);
                let c = s.heap.new_closure(body, s.scope, Self::fn_id_of(&name), caps);
                let v = Value::Func(c);
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

    /// `if`: a decided condition takes its arm; an undecided one runs BOTH
    /// arms and merges.
    fn exec_if(&mut self, iff: &'a ast::If, st: State<D>) -> Result<Outcome<D>> {
        // `elseif` chains are nested arms: recurse over the tail.
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
                    let (ts, fs) = split(&mut self.d, s, &cond);
                    let t = match ts {
                        Some(x) => self.exec_block(blk, x)?,
                        None => Vec::new(),
                    };
                    let f = match fs {
                        Some(x) => self.exec_arms(rest, els, x)?,
                        None => Vec::new(),
                    };
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

    /// Merge every mergeable pair of outcomes to a fixpoint, not only
    /// siblings (an `elseif` ladder in a loop leaves one `return` per rung).
    /// Run after every statement, so the frontier is the number of genuinely
    /// different futures.
    pub fn collapse(&mut self, mut outs: Outcome<D>) -> Result<Outcome<D>> {
        self.drop_raised(&mut outs);
        let mut pairing = Pairing::new(outs.len());
        'again: loop {
            // Siblings first (`merge_order`).
            let states: Vec<&State<D>> = outs.iter().map(|o| &o.0).collect();
            pairing.prepare(&states)?;
            let pairs = super::state::merge_order(&states, |i, j| {
                pairing.may_merge(i, j) && self.can_join(&outs[i].1, &outs[j].1)
            });
            for (i, j) in pairs {
                let Some(m) = merge_canon(&mut self.d, &outs[i].0, &outs[j].0, pairing.sides(i, j))? else {
                    pairing.refuse(i, j);
                    continue;
                };
                if let (Flow::Return(x), Flow::Return(y)) = (&outs[i].1, &outs[j].1) {
                    if !self.joins_independent(&m.cond, x, y) {
                        pairing.refuse(i, j);
                        continue;
                    }
                }
                self.merge_fallbacks += m.fell_back as usize;
                let fl = self.join_flow(&m.cond, &outs[i].1, &outs[j].1);
                let state = m.state;
                outs.remove(j);
                outs.remove(i);
                pairing.replace(i, j);
                outs.push((state, fl));
                continue 'again;
            }
            break;
        }
        Ok(outs)
    }

    /// The same fixpoint for a fanned-out EXPRESSION's value outcomes.
    pub fn collapse_values(
        &mut self,
        mut outs: Multi<D, Value<D>>,
    ) -> Result<Multi<D, Value<D>>> {
        self.drop_raised(&mut outs);
        let mut pairing = Pairing::new(outs.len());
        'again: loop {
            let states: Vec<&State<D>> = outs.iter().map(|o| &o.0).collect();
            pairing.prepare(&states)?;
            let pairs = super::state::merge_order(&states, |i, j| {
                pairing.may_merge(i, j) && joinable(&outs[i].1, &outs[j].1)
            });
            for (i, j) in pairs {
                let Some(m) = merge_canon(&mut self.d, &outs[i].0, &outs[j].0, pairing.sides(i, j))? else {
                    pairing.refuse(i, j);
                    continue;
                };
                if !self.joins_independent(&m.cond, &outs[i].1, &outs[j].1) {
                    pairing.refuse(i, j);
                    continue;
                }
                self.merge_fallbacks += m.fell_back as usize;
                let v = self.join_value(&m.cond, &outs[i].1.clone(), &outs[j].1.clone());
                let state = m.state;
                outs.remove(j);
                outs.remove(i);
                pairing.replace(i, j);
                outs.push((state, v));
                continue 'again;
            }
            break;
        }
        Ok(outs)
    }

    /// Drop states live nowhere (a raise cleared their guard): merged they
    /// would add selects no lane can take.
    fn drop_raised<T>(&mut self, outs: &mut Vec<(State<D>, T)>) {
        outs.retain(|(s, _)| self.d.decide(&s.guard) != Some(false));
    }

    /// Can these two flows be joined? Builds no nodes, so rejections are free.
    fn can_join(&self, a: &Flow<D>, b: &Flow<D>) -> bool {
        match (a, b) {
            (Flow::Return(x), Flow::Return(y)) => joinable(x, y),
            (Flow::Break, Flow::Break) | (Flow::Normal, Flow::Normal) => true,
            _ => false,
        }
    }

    /// Can two values be joined on `cond` without a select some lane cannot
    /// take? On a condition reading an unknown atom, only an independent join
    /// can; otherwise they stay two successors, as `state::merge` does.
    fn joins_independent(&mut self, cond: &D::Bool, a: &Value<D>, b: &Value<D>) -> bool {
        if a == b || !self.d.reads_unknown_atom(cond) {
            return true;
        }
        match (a, b) {
            (Value::Num(x), Value::Num(y)) => self.d.join_num_independent(cond, x, y).is_some(),
            _ => true,
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

    /// Join two `joinable` values.
    fn join_value(&mut self, cond: &D::Bool, a: &Value<D>, b: &Value<D>) -> Value<D> {
        match (a, b) {
            // As `state::join`.
            (Value::Num(x), Value::Num(y)) => Value::Num(match self.d.join_num_independent(cond, x, y) {
                Some(j) => j,
                None => self.d.sel_num(cond, x, y),
            }),
            (Value::Bool(x), Value::Bool(y)) => Value::Bool(match self.d.join_bool_independent(cond, x, y) {
                Some(j) => j,
                None => self.d.sel_bool(cond, x, y),
            }),
            (x, _) => x.clone(),
        }
    }


    /// Numeric `for`. Bounds are almost always concrete; a symbolic one is
    /// unrolled to `unroll_bound` by `run_for_symbolic`.
    fn exec_for(&mut self, f: &'a ast::NumericFor, st: State<D>) -> Result<Outcome<D>> {
        if f.step().is_some() {
            bail!("numeric for with an explicit step is not supported");
        }
        let name = ident(f.index_variable())?;
        let mut all: Outcome<D> = Vec::new();
        // Bounds can fan out: each combination is its own loop.
        let mut bounds: Multi<D, Bound<D>> = Vec::new();
        for (s, from) in self.eval(f.start(), st)? {
            for (s, to) in self.eval(f.end(), s)? {
                let (Value::Num(a), Value::Num(b)) = (&from, &to) else {
                    bail!("numeric for bounds must be numbers");
                };
                match (self.d.as_const(a), self.d.as_const(b)) {
                    (Some(a), Some(b)) => bounds.push((s, Bound::Concrete(a, b))),
                    // Either end unknown: the variable is `start + k`.
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

    /// Unroll a loop whose limit is unknown at trace time, masking each
    /// iteration by `i <= limit`. A lane still looping when the bound runs
    /// out is an error (`State::ended`), so the bound is performance only.
    fn run_for_symbolic(
        &mut self,
        f: &'a ast::NumericFor,
        name: &str,
        start: D::Num,
        limit: D::Num,
        s: State<D>,
        bound: u32,
    ) -> Result<Outcome<D>> {
    // A state LEAVES the loop and never re-enters: the guard is opaque, so a
    // re-tested `i <= limit` would split under `g and c and not c`.
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
        // Where the model ENDS, for states still running only.
        let off = self.d.num(P8::from_i16(bound as i16));
        let iv = self.d.arith(Arith::Add, &start, &off)?;
        let over = self.d.compare(Cmp::Le, &iv, &limit)?;
        for (s, _) in running.iter_mut() {
            s.ended = self.d.or(&s.ended, &over);
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
        self.for_iterations += 1;
        let body_scope = cur.heap.new_scope(Some(cur.scope));
        cur.heap.declare(body_scope, name, Value::Num(iv.clone()));
        let outer = cur.scope;
        cur.scope = body_scope;
        let mut out = Vec::new();
        for (mut s2, f2) in self.exec_block(f.block(), cur)? {
            s2.scope = outer;
            // `Flow::Break` passes THROUGH: the caller ends the loop.
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

        // A `break` ends the LOOP for that state, which carries on after it.
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
        done.extend(running.into_iter().map(|(s, _)| (s, Flow::Normal)));
        Ok(done)
    }

    // ------------------------------------------------------- expressions

    /// The condition's value, if the domain knows it.
    fn decide(&self, cond: &D::Bool) -> Option<bool> {
        self.d.decide(cond)
    }


    fn truthy(&mut self, v: &Value<D>) -> D::Bool {
        // Lua: only nil and false are falsy.
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
                            let Value::Table(t) = v else { bail!("# of a non-table") };
                            // `Table::len`, NOT `arr.len()` (holes).
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
                // Carry the SOURCE TEXT into the error.
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
                let fn_id = self.hint_name().and_then(|n| Self::fn_id_of(&n));
                let st = st;
                let env = st.scope;
                let caps = self.captures_of(body, env, &st);
                let mut st = st;
                let c = st.heap.new_closure(id, env, fn_id, caps);
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
                                self.hint.push(k.clone());
                                let vs = self.eval(value, st);
                                self.hint.pop();
                                for (mut st, v) in vs? {
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
        // `and`/`or` short-circuit and return VALUES (`btn(r) and 1`): an
        // undecided condition fans out; sides merge only if kinds agree.
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
                            // Hand on the CONSTANT `!is_and`, not the node just
                            // split on, or an enclosing `or` splits on it again.
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
        // A comparison reading a countdown field (`domain::COUNTDOWN_FIELDS`).
        let countdown = |e: &ast::Expression| {
            let t = e.to_string();
            let t = t.trim();
            crate::trace::domain::COUNTDOWN_FIELDS.iter().any(|f| t.strip_suffix(f).is_some_and(|h| h.ends_with('.')))
        };
        let hint = countdown(lhs) || countdown(rhs);
        for (st, a) in self.eval(lhs, st)? {
            for (mut st, b) in self.eval(rhs, st)? {
                self.d.set_countdown_hint(hint);
                let r = self.binop_values(binop, &a, &b);
                self.d.set_countdown_hint(false);
                let (v, illegal) = r?;
                if let Some(why) = illegal {
                    let t = format!("{}{}{}", lhs, binop, rhs);
                    let t = t.trim();
                    self.poison(&mut st, format!("`{}`: {}", &t[..t.len().min(48)], why));
                }
                out.push((st, v));
            }
        }
        Ok(out)
    }

    /// A LUA RAISE on this path (e.g. `nil > 0`): the game ends, so these
    /// lanes have NO successor and `guard` is cleared. Not a silent drop: the
    /// guard goes to `raised`, whose OR is the raise row's liveness, so every
    /// lane ends in exactly one row (exact even where a merge's lost
    /// correlation reaches the raise). Counted in `illegal`.
    fn poison(&mut self, st: &mut State<D>, why: String) {
        self.raised.push((st.guard.clone(), why.clone()));
        st.guard = self.d.boolean(false);
        *self.illegal.entry(why).or_default() += 1;
    }

    /// The value and, if Lua would raise, why (the caller poisons). An
    /// undecided comparison is its plain node.
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
                // Lua raises.
                let zero = Value::Num(self.d.num(P8::from_i16(0)));
                let why = format!("arithmetic on {} and {}", kind_of(a), kind_of(b));
                return Ok((zero, Some(why)));
            };
            return Ok((Value::Num(self.d.arith(op, x, y)?), None));
        }
        let op = cmp_op.unwrap();
        let r = match (a, b) {
            (Value::Num(x), Value::Num(y)) => self.d.compare(op, x, y)?,
            // Booleans compare by VALUE, not by node id.
            (Value::Bool(x), Value::Bool(y)) if op == Cmp::Eq => {
                let both = self.d.and(x, y);
                let (nx, ny) = (self.d.not(x), self.d.not(y));
                let neither = self.d.and(&nx, &ny);
                self.d.or(&both, &neither)
            }
            // Distinct closures may be one object in PICO-8, so refuse; the
            // cart compares functions only against nil.
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
            _ if op == Cmp::Eq => {
                let same = a == b;
                self.d.boolean(same)
            }
            // Lua raises on an ordered comparison of non-numbers.
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



    /// Read a name: scopes first, then globals, else `nil`.
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

    /// The key a `[..]` index denotes; it must be CONCRETE (the heap is).
    fn index_key(&mut self, v: &Value<D>) -> Result<Key> {
        Ok(match v {
            Value::Str(s) => {
                // `cart::check_absent_fields` sees only `.field`, so refuse a
                // computed key naming an absent-as-zero field here.
                if let Some((_, f)) = crate::trace::widen::ABSENT_AS_ZERO.iter().find(|(_, f)| s.to_string() == **f) {
                    bail!("`[\"{f}\"]` indexes an absent-as-zero field (widen::ABSENT_AS_ZERO) outside what cart::check_absent_fields checks");
                }
                Key::Field(s.to_string())
            }
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

    fn walk_suffixes(
        &mut self,
        p: &'a ast::Prefix,
        suffixes: &[&'a ast::Suffix],
        st: State<D>,
    ) -> Result<Multi<D, Value<D>>> {
        // A readable path, so "calling a non-function: nil" says WHICH nil.
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
                            if self.arc_capture && matches!(val, Value::Builtin("__split_by_flr")) {
                                self.split_site = Some(self.split_site_of(arguments, &st)?);
                            }
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

    /// Work out WHERE an assignment stores, so the left-hand side evaluates
    /// before the right, as in Lua.
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

    /// Does any tile in the loaded room carry this flag bit?
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
                    st.heap.tables.get_mut(&g).unwrap().set_global(name.clone(), v);
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

    /// A call is RECURSION: hand the callee the state, get it back.
    fn call_value(
        &mut self,
        f: Value<D>,
        args: Vec<Value<D>>,
        st: State<D>,
    ) -> Result<Multi<D, Value<D>>> {
        match f {
            Value::Func(c) => {
                let mut st = st;
                let super::heap::Closure { body, env, .. } = *st
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
                // The caller's scope stays a GC ROOT (`State::stack`).
                st.stack.push(outer);
                st.scope = frame;
                let atoms_before = self.d.atoms_minted();
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
                // Literal splits made inside this call rejoin as it returns.
                if out.iter().any(|(s, _)| s.frag.last().is_some_and(|f| f.0 > s.stack.len())) {
                    out = self.rejoin_fragments(out)?;
                }
                // Atoms made inside this call do not escape it.
                if self.d.atoms_minted() > atoms_before {
                    out = out.into_iter().map(|(s, v)| self.fork_escaped_atoms(s, v, atoms_before)).collect();
                }
                Ok(out)
            }
            Value::Builtin(name) => self.call_builtin(name, args, st),
            other => bail!("calling a non-function: {:?}", other),
        }
    }

    /// Heap booleans and the returned value holding an atom minted at or
    /// after `since` become that atom's fork of both values.
    fn fork_escaped_atoms(&mut self, mut s: State<D>, v: Value<D>, since: u32) -> (State<D>, Value<D>) {
        // Only what the caller can reach: fork ids are scarce.
        let mut roots = s.roots();
        super::heap::push_value(&v, &mut roots);
        let (tables, scopes, _) = s.heap.reachable(&roots);
        let d = &mut self.d;
        // The origin is named when a fork is minted (a table's key, `x`/`y`).
        let swap = |d: &mut D, x: &mut Value<D>, origin: &dyn Fn(&D) -> String| {
            if let Value::Bool(b) = x {
                if let Some(f) = d.escaped_atom(b, since, origin) {
                    *b = f;
                }
            }
        };
        for (_, t) in s.heap.tables.iter_mut().filter(|(id, _)| tables.contains(id)) {
            let num = |k: &str| match t.hash.get(k) {
                Some(Value::Num(n)) => Some(n.clone()),
                _ => None,
            };
            let at = (num("x"), num("y"));
            for (k, x) in t.hash.iter_mut() {
                swap(&mut *d, x, &|d: &D| match &at {
                    (Some(px), Some(py)) => format!("{k} at x {} y {}", d.describe_num(px), d.describe_num(py)),
                    _ => k.clone(),
                });
            }
            for x in t.arr.iter_mut() {
                swap(&mut *d, x, &|_: &D| "[array]".to_string());
            }
            for x in t.ints.values_mut() {
                swap(&mut *d, x, &|_: &D| "[int]".to_string());
            }
        }
        for (_, sc) in s.heap.scopes.iter_mut().filter(|(id, _)| scopes.contains(id)) {
            for (k, x) in sc.vars.iter_mut() {
                swap(&mut *d, x, &|_: &D| k.to_string());
            }
        }
        let mut v = v;
        swap(&mut *d, &mut v, &|_: &D| "[returned]".to_string());
        (s, v)
    }

    /// Rejoin the fragments of literal splits made inside a call that just
    /// returned. No lane picks a fragment (every lane holds the value alike),
    /// so the states of one split merge on an undecided atom and everything
    /// else must JOIN (a hull or the unknown number). A surviving select, an
    /// unjoinable return or two shapes refuse the trace.
    fn rejoin_fragments(&mut self, out: Multi<D, Value<D>>) -> Result<Multi<D, Value<D>>> {
        let mut rest: Multi<D, Value<D>> = Vec::new();
        type Tags = Vec<(usize, usize, u16)>;
        let mut groups: Vec<(Tags, usize, Multi<D, Value<D>>)> = Vec::new();
        for (mut s, v) in out {
            match s.frag.last().copied() {
                Some((depth, id, _)) if depth > s.stack.len() => {
                    s.frag.pop();
                    match groups.iter_mut().find(|g| g.0 == s.frag && g.1 == id) {
                        Some(g) => g.2.push((s, v)),
                        None => groups.push((s.frag.clone(), id, vec![(s, v)])),
                    }
                }
                _ => rest.push((s, v)),
            }
        }
        for (_, id, group) in groups {
            let mut states = group.into_iter();
            let mut acc = states.next().expect("a split has a fragment");
            for (s, v) in states {
                let cond = self.d.undecided_atom()?;
                let m = super::state::merge_on(&mut self.d, &acc.0, &s, cond.clone())?
                    .ok_or_else(|| anyhow!("literal split {id}: its fragments end in different shapes"))?;
                anyhow::ensure!(
                    !m.selects,
                    "literal split {id}: its fragments did not rejoin, {} differs by more than a literal",
                    m.first_select.as_deref().unwrap_or("a value")
                );
                let v = match (&acc.1, &v) {
                    (a, b) if a == b => a.clone(),
                    (Value::Num(x), Value::Num(y)) => Value::Num(
                        self.d.join_num_independent(&cond, x, y).ok_or_else(|| anyhow!("literal split {id}: the call's return value did not rejoin"))?,
                    ),
                    (Value::Bool(x), Value::Bool(y)) => Value::Bool(
                        self.d.join_bool_independent(&cond, x, y).ok_or_else(|| anyhow!("literal split {id}: the call's return value did not rejoin"))?,
                    ),
                    _ => bail!("literal split {id}: its fragments return different kinds"),
                };
                acc = (m.state, v);
            }
            rest.push(acc);
        }
        Ok(rest)
    }

    /// THE ARC CAPTURE: what `__split_by_flr(obj.rem.x)` splits. The argument
    /// must be spelled `<name>.rem.<x|y>` (the cart's `move`), and `<name>` is
    /// compared BY IDENTITY with `player`; any other spelling is refused.
    fn split_site_of(&mut self, arguments: &'a full_moon::ast::punctuated::Punctuated<ast::Expression>, st: &State<D>) -> Result<SplitSite> {
        let args: Vec<&ast::Expression> = arguments.iter().collect();
        let spelled = match args.as_slice() {
            [ast::Expression::Var(v)] => target_hint(v),
            _ => None,
        };
        let parts: Vec<String> = spelled.as_deref().unwrap_or("").split('.').map(str::to_string).collect();
        let (obj, axis) = match parts.as_slice() {
            [obj, rem, axis] if rem == "rem" && (axis == "x" || axis == "y") => (obj.clone(), if axis == "x" { 0 } else { 1 }),
            _ => bail!("arc capture: a `__split_by_flr` whose argument is not `<obj>.rem.<x|y>` ({spelled:?}): no way to tell whose remainder it splits"),
        };
        let Value::Table(t) = self.read_name(&obj, st) else { bail!("arc capture: `{obj}` in `__split_by_flr({obj}.rem..)` is not a table") };
        let players = super::widen::objects_of_type(st, "player");
        anyhow::ensure!(players.len() <= 1, "arc capture: {} player objects", players.len());
        let is_player = players.first().is_some_and(|p| super::iface::get(st, p) == Some(Value::Table(t)));
        Ok(if is_player { SplitSite::Player(axis) } else { SplitSite::Other })
    }

    /// THE ARC CAPTURE: the player's split on `axis` took fragment `v` of `x`;
    /// record it with the move's `ox`/`oy`. A second on one axis in one path
    /// (a platform carrying the player) is refused: an arc edge describes
    /// one rotation per axis per frame.
    fn capture_player_split(&mut self, st: &mut State<D>, axis: usize, x: &D::Num, v: &D::Num) -> Result<()> {
        let name = if axis == 0 { "ox" } else { "oy" };
        let Value::Num(ox) = self.read_name(name, st) else { bail!("arc capture: the player's split on axis {axis} has no number `{name}` in scope") };
        let Some(arc) = st.arc.as_mut() else { bail!("arc capture: a player split on a path that does not capture") };
        let a = &mut arc[axis];
        anyhow::ensure!(
            self.d.decide(&a.took) == Some(false),
            "arc capture: a second split of the player's remainder on axis {axis} in one path (a moving platform carrying the player?) - one rotation per axis per frame is all an arc edge describes"
        );
        a.took = self.d.boolean(true);
        a.pre = x.clone();
        a.frag = v.clone();
        a.ox = ox;
        Ok(())
    }

    fn call_builtin(
        &mut self,
        name: &'static str,
        args: Vec<Value<D>>,
        st: State<D>,
    ) -> Result<Multi<D, Value<D>>> {
        // A missing argument raises (PICO-8 would coerce nil to 0; the cart
        // always passes them).
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
                let v = self.d.fun1(f, &a)?;
                let at = st.decided(&mut self.d);
                self.d.evaluated_at(&v, &at);
                (st, Value::Num(v))
            }
            // The map is concrete data: concrete coordinates fold.
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
                    _ if name == "mget" => (st, Value::Num(self.d.mget(&a, &b)?)),
                    _ => bail!("fget with unknown arguments is not modelled"),
                }
            }
            // Native: folds on concrete coordinates, else one graph node.
            "tile_flag_at" => {
                let (x, y, w, h, fl) = (num(0)?, num(1)?, num(2)?, num(3)?, num(4)?);
                let gi = |v: P8| v.as_i16().ok_or_else(|| anyhow!("tile_flag_at: non-integer"));
                // THE FLAG DECIDES FIRST: a flag no tile of the room carries
                // is false everywhere, exactly; others are answered like solid.
                let Some(fi) = self.d.as_const(&fl).map(gi).transpose()? else {
                    bail!("tile_flag_at with an unknown flag");
                };
                if fi != 0 && !self.room_has_flag(&st, fi)? {
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
                            let r = cache.flag_at(&cart, gi(x)?, gi(y)?, gi(w)?, gi(h)?, fi)?;
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
            // Rendered as PICO-8's `printh` does (the `lua/probe/` goldens).
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
            // A merge hint: the tracer merges at every join anyway.
            "_hint_normalize" => (st, Value::Nil),
            // A SHAPE change.
            "__array_table_drop_last" => {
                let Value::Table(t) = &args[0] else {
                    bail!("__array_table_drop_last on a non-table: {:?}", args[0])
                };
                let mut st = st;
                let tab = st.heap.tables.get_mut(t).unwrap();
                // Exactly `list[#list] = nil`, NOT `arr.pop()`: after a hole
                // the last slot is not the last element.
                let n = tab.len().ok_or_else(|| {
                    anyhow!("__array_table_drop_last on a table whose length is not exact")
                })?;
                if n == 0 {
                    bail!("__array_table_drop_last on an empty table");
                }
                tab.set_index(n as i16, Value::Nil);
                (st, Value::Nil)
            }
            // The fork is a CHOICE, not a state fan-out: one node whose value
            // depends on the fragment (one fragment on an exact value).
            // Validity goes in the GUARD (fragments partition the lane);
            // coverage is the fork's own error (a lane spanning more floors
            // than fragments cannot run in this body).
            "__split_by_flr" | "__split_at" => {
                let x = num(0)?;
                let site = match (self.arc_capture && name == "__split_by_flr", self.split_site.take()) {
                    (false, _) => None,
                    (true, Some(site)) => Some(site),
                    (true, None) => bail!("arc capture: a `__split_by_flr` call the tracer did not see the argument of"),
                };
                let player_axis = match site {
                    Some(SplitSite::Player(axis)) => Some(axis),
                    _ => None,
                };
                // A value every lane holds alike: each fragment runs as its
                // own trace state, rejoined when the call returns.
                if let Some(frags) = self.d.literal_fragments(&x)? {
                    // Its fragments rejoin into one hull: no body keeps the piece.
                    anyhow::ensure!(player_axis.is_none(), "arc capture: the player's remainder split on axis {player_axis:?} is a literal split");
                    if frags.len() == 1 {
                        return Ok(vec![(st, Value::Num(frags[0].clone()))]);
                    }
                    let (id, depth) = (self.next_split, st.stack.len());
                    self.next_split += 1;
                    return Ok(frags
                        .into_iter()
                        .enumerate()
                        .map(|(c, v)| {
                            let mut s = st.clone();
                            s.frag.push((depth, id, c as u16));
                            (s, Value::Num(v))
                        })
                        .collect());
                }
                if !self.d.is_interval(&x) {
                    let mut st = st;
                    if let Some(axis) = player_axis {
                        self.capture_player_split(&mut st, axis, &x, &x)?;
                    }
                    (st, args[0].clone())
                } else {
                    // The coverage error is derived where evaluated.
                    let ways = self.d.flr_ways(&x);
                    let (v, valid) = self.d.fork_flr(&x, ways);
                    let at = st.decided(&mut self.d);
                    self.d.evaluated_at(&v, &at);
                    let mut st = st;
                    st.guard = self.d.and(&st.guard, &valid);
                    if let Some(axis) = player_axis {
                        self.capture_player_split(&mut st, axis, &x, &v)?;
                    }
                    (st, Value::Num(v))
                }
            }
            "__new_unknown_boolean" => (st, Value::Bool(self.d.unknown_bool()?)),
            // `rnd(x)`: the whole range [0, x), a sound over-approximation of
            // an unmodelled seed; a bound that is not a positive constant is
            // refused.
            "rnd" => {
                let x = match args.first() {
                    None => P8::from_i16(1),
                    Some(Value::Num(n)) => self
                        .d
                        .as_const(n)
                        .ok_or_else(|| anyhow!("rnd: a non-constant bound is not modelled"))?,
                    Some(other) => bail!("rnd: expected a number, got {:?}", other),
                };
                anyhow::ensure!(x > P8::from_i16(0), "rnd: a non-positive bound {x:?} is not modelled");
                let hi = P8::from_raw(x.as_raw_u32() as i32 - 1);
                (st, Value::Num(self.d.range_num(P8::from_i16(0), hi)?))
            }
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

/// A resolved table key, concrete by construction (`index_key`).
#[derive(Clone)]
enum Key {
    Field(String),
    Index(i16),
}

/// A resolved assignment TARGET, worked out before the right-hand side runs.
enum Target<D: Domain> {
    Name(String),
    Field(Value<D>, Key),
}

/// Does this block bind a name in its OWN statements?
fn declares_local(block: &ast::Block) -> bool {
    block.stmts().any(|s| matches!(s, ast::Stmt::LocalAssignment(_)))
}

/// An assignment target spelled as a function name (`obj.is_solid`).
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

/// How far to unroll a loop with an unknown limit, keyed by the limit's
/// SOURCE TEXT. Performance only: a lane looping past it is an error, and an
/// unknown key is refused.
fn unroll_bound(limit_src: &str) -> Option<u32> {
    match limit_src {
        // `move_x` / `move_y` pixel steppers.
        "abs(amount)" => Some(8),
        // `spikes_at` / `solid_at` tile scans.
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

/// Which pairs of a `collapse` frontier to try merging. Different shapes
/// never merge, and a refused pair stays refused (`merge` is a function of
/// the two states), so each pair is tried once.
struct Pairing {
    /// Per outcome: its id, and its shape hash and `Canon` once computed.
    keys: Vec<(usize, Option<(u64, Canon)>)>,
    /// Every outcome holds the first one's heap: pair by object, no `Canon`.
    same: bool,
    /// Whether `same` can still hold.
    same_possible: bool,
    next: usize,
    refused: std::collections::HashSet<(usize, usize)>,
}

impl Pairing {
    fn new(n: usize) -> Self {
        Pairing { keys: (0..n).map(|i| (i, None)).collect(), same: false, same_possible: true, next: n, refused: Default::default() }
    }

    /// Decide how `states` pair and compute the canons that needs.
    fn prepare<D: Domain>(&mut self, states: &[&State<D>]) -> Result<()> {
        use std::hash::{Hash, Hasher};
        self.same = self.same_possible && states.iter().skip(1).all(|s| super::state::same_heap(states[0], s));
        self.same_possible = self.same;
        if self.same || states.len() < 2 {
            return Ok(());
        }
        for (k, s) in self.keys.iter_mut().zip(states) {
            if k.1.is_none() {
                let canon = Canon::of(s)?;
                let mut h = rustc_hash::FxHasher::default();
                canon.shape().hash(&mut h);
                k.1 = Some((h.finish(), canon));
            }
        }
        Ok(())
    }

    fn may_merge(&self, i: usize, j: usize) -> bool {
        let alike = self.same || matches!((&self.keys[i].1, &self.keys[j].1), (Some((a, _)), Some((b, _))) if a == b);
        alike && !self.refused.contains(&(self.keys[i].0, self.keys[j].0))
    }

    /// How outcomes `i` and `j` pair, for `merge_canon`.
    fn sides(&self, i: usize, j: usize) -> Sides<'_> {
        match (&self.keys[i].1, &self.keys[j].1) {
            _ if self.same => Sides::Same,
            (Some((_, a)), Some((_, b))) => Sides::Canon(a, b),
            _ => unreachable!("`prepare` computed every canon of a differing frontier"),
        }
    }

    fn refuse(&mut self, i: usize, j: usize) {
        self.refused.insert((self.keys[i].0, self.keys[j].0));
    }

    /// Outcomes `i < j` merged into `merged`, which goes last.
    fn replace(&mut self, i: usize, j: usize) {
        self.keys.remove(j);
        self.keys.remove(i);
        self.keys.push((self.next, None));
        self.next += 1;
        // The merged heap is collected: recheck.
        self.same_possible = true;
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
        let f = d.boolean(false);
        State { heap, globals, scope, stack: Vec::new(), guard: t, ended: f, path: Vec::new(), frag: Vec::new(), arc: None }
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

    /// The oracle domain interprets Lua: closures, recursion, tables, `for`.
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
        assert_eq!(it.d.as_const(&n), Some(P8::from_i16(31)));
    }

    /// The tracing domain folds the same program to the same constant.
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

    /// An unknown branch runs BOTH arms and merges them into one select.
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
        assert_eq!(it.d.graph.get(node.args[1]).op, Op::Const(10 << 16, 10 << 16));
        assert_eq!(it.d.graph.get(node.args[2]).op, Op::Const(20 << 16, 20 << 16));
    }

    /// Branches alike in heap returning different numbers: a select whose
    /// error is its undecided condition.
    #[test]
    fn a_returned_select_owns_its_condition_being_decided() {
        let ast = parse(
            r#"
            function f()
              if input > 0 then
                return 10
              else
                return 20
              end
            end
            result = f()
            "#,
        );
        let mut d = Symbolic::default();
        // An interval input: a lane can hold `input > 0` both ways.
        d.ival_cells.insert(1);
        d.forget_intervals();
        let sym = d.graph.leaf(Op::Cell(1));
        let mut it = Interp::new(d);
        let mut st = fresh::<Symbolic>(&mut it.d);
        let g = st.globals;
        st.heap.tables.get_mut(&g).unwrap().hash.insert("input".into(), Value::Num(sym));

        let out = it.exec_block(ast.nodes(), st).expect("exec");
        assert_eq!(out.len(), 1, "the returns merged");
        let s = &out[0].0;
        let Value::Num(r) = s.heap.tables[&s.globals].hash["result"].clone() else {
            panic!("expected a number")
        };
        assert_eq!(it.d.graph.get(r).op, Op::Sel, "the returned value is a select");
        let cond = it.d.graph.get(r).args[0];
        let e = super::super::error::of(&mut it.d, &[r]);
        let known = it.d.graph.fold(Op::Known, vec![cond]);
        assert_eq!(e, it.d.graph.fold(Op::Not, vec![known]), "and its error is the condition undecided");
    }

    /// Two states of one shape in DIFFERENT rooms stay two successors.
    #[test]
    fn states_in_different_rooms_stay_apart() {
        let ast = parse(
            r#"
            room = {x = 6, y = 2}
            o = {y = 0}
            if input > 0 then
              room.x = 7
              o.y = 1
            else
              o.y = 2
            end
            "#,
        );
        let mut d = Symbolic::default();
        let sym = d.graph.leaf(Op::Cell(1));
        let mut it = Interp::new(d);
        let mut st = fresh::<Symbolic>(&mut it.d);
        let g = st.globals;
        st.heap.tables.get_mut(&g).unwrap().hash.insert("input".into(), Value::Num(sym));

        let out = it.exec_block(ast.nodes(), st).expect("exec");
        assert_eq!(out.len(), 2, "one successor per room");
        let mut rooms: Vec<i16> = out
            .iter()
            .map(|(s, _)| {
                let Some(Value::Num(x)) = crate::trace::iface::get(s, &[crate::trace::iface::key("room"), crate::trace::iface::key("x")]) else {
                    panic!("room.x is not a number")
                };
                it.d.as_const(&x).expect("room.x is a constant in every outcome").as_i16_or_err().expect("a whole room")
            })
            .collect();
        rooms.sort();
        assert_eq!(rooms, vec![6, 7]);
    }

    /// `__split_by_flr` of a literal splits on the integers and rejoins into
    /// one literal, no error, no fork.
    #[test]
    fn a_literal_split_rejoins_without_error() {
        let ast = parse(
            r#"
            function mv(r)
              r = __split_by_flr(r + 0.5)
              local amount = flr(r)
              return r - 0.5 - amount
            end
            result = mv(input)
            "#,
        );
        let mut d = Symbolic::default();
        d.fruit_unknown = true;
        // The literal [-4, 1): the fruit's `rem.y + spd.y`.
        let input = d.graph.leaf(Op::Const(-4 << 16, (1 << 16) - 1));
        let mut it = Interp::new(d);
        let mut st = fresh::<Symbolic>(&mut it.d);
        let g = st.globals;
        let globals = &mut st.heap.tables.get_mut(&g).unwrap().hash;
        globals.insert("input".into(), Value::Num(input));
        for b in ["__split_by_flr", "flr"] {
            globals.insert(b.into(), Value::Builtin(b));
        }
        let out = it.exec_block(ast.nodes(), st).expect("exec");
        assert_eq!(out.len(), 1, "the fragments rejoined");
        let s = &out[0].0;
        let Value::Num(r) = s.heap.tables[&s.globals].hash["result"].clone() else {
            panic!("expected a number")
        };
        // + 0.5 is [-3.5, 1.5): six fragments hulled back to one literal.
        assert_eq!(it.d.graph.get(r).op, Op::Const(-0x8000, 0x7fff), "the remainder is the literal [-0.5, 0.5)");
        let e = super::super::error::of(&mut it.d, &[r]);
        assert_eq!(it.d.graph.get(e).op, Op::ConstBool(false), "no lane errs");
        assert_eq!(it.d.forks, 0, "no per-lane fork");
    }

    /// A literal spanning too many integers is refused while tracing.
    #[test]
    fn a_literal_split_past_max_ways_is_refused() {
        let ast = parse("result = __split_by_flr(input)");
        let mut d = Symbolic::default();
        d.fruit_unknown = true;
        let input = d.graph.leaf(Op::Const(0, (300 << 16) - 1));
        let mut it = Interp::new(d);
        let mut st = fresh::<Symbolic>(&mut it.d);
        let g = st.globals;
        let globals = &mut st.heap.tables.get_mut(&g).unwrap().hash;
        globals.insert("input".into(), Value::Num(input));
        globals.insert("__split_by_flr".into(), Value::Builtin("__split_by_flr"));
        let err = it.exec_block(ast.nodes(), st).err().expect("refused");
        assert!(format!("{err:#}").contains("more than 255 fragments"), "{err:#}");
    }
}

/// A number as `printh` renders it (`printh(1/3)` prints `0.3333`).
fn fmt_p8(v: P8) -> String {
    if let Some(i) = v.as_i16() {
        return format!("{}", i);
    }
    let raw = v.as_raw_u32() as i32 as f64 / 65536.0;
    let s = format!("{:.4}", raw);
    let s = s.trim_end_matches('0').trim_end_matches('.').to_string();
    s
}

/// A value's Lua type name, for `poison` (one gap regardless of operands).
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
