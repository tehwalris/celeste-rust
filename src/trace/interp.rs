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

use anyhow::{anyhow, bail, ensure, Result};
use full_moon::ast;
use full_moon::tokenizer::{Symbol, TokenType};

use crate::pico8_num::Pico8Num as P8;

use super::domain::{refuse_unknown, Arith, Cmp, Domain, Fun1, Fun2};
use super::heap::{BodyId, ScopeId, TableId, Value};
use super::stage::At;
use super::state::{merge_canon, split, Canon, Sides, State};

pub enum Flow<D: Domain> {
    Normal,
    Break,
    Return(Value<D>),
    /// The stage ended at a cut (`trace::stage`): the state carries where
    /// (`State::cont`), and every enclosing level passes it up, adding
    /// where it was, instead of running on.
    Suspend,
}

impl<D: Domain> Clone for Flow<D> {
    fn clone(&self) -> Self {
        match self {
            Flow::Normal => Flow::Normal,
            Flow::Break => Flow::Break,
            Flow::Return(v) => Flow::Return(v.clone()),
            Flow::Suspend => Flow::Suspend,
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

/// Cloneable so key-set workers can each trace from a copy of one walked
/// tracer (its arena holds the shapes' representative states).
#[derive(Clone)]
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
    /// See `hint_name`.
    hint: Vec<String>,
    /// Paths poisoned by `poison`: a Lua type error means no legal run
    /// takes that path, so its lanes deopt rather than aborting the
    /// trace. Counted by reason, because turning an error into a deopt
    /// would otherwise hide a modelling gap at build time.
    pub illegal: std::collections::BTreeMap<String, usize>,
    /// WHERE each poisoned path was, as the path guard at the raise, with the
    /// reason: the raise row's liveness is the OR of these
    /// (plans/graph-model.md section 4, "a raise is its own row").
    ///
    /// `illegal` counts by reason and so cannot build that - a string is not a
    /// condition. Kept beside it rather than replacing it, because the count
    /// is the build-time diagnostic and this is the value the kernel needs.
    /// Per trace: `verify::trace_frame` resets it and takes it.
    pub raised: Vec<(D::Bool, String)>,
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
    /// The graph's size when the current frame trace began
    /// (`verify::trace_frame`): `max_nodes` bounds ONE trace's growth. A
    /// walk traces many frames into one arena, and bounding the arena's
    /// total made a trace refuse because of the traces before it (room
    /// (3,0): 12 fall floors, two shapes refused at `__reset_button_states`,
    /// 2026-09-16).
    pub trace_start_nodes: usize,
    /// What `printh` has printed, in order. This exists so a program can
    /// be run in the tracer AND in real PICO-8 and the two outputs
    /// compared line for line (`lua/probe/`), which is the only way to
    /// settle a question like what `#` does to a table with a hole in it.
    pub prints: Vec<String>,
    /// Loop bodies traced (`for_body` calls): how far the unrolled loops
    /// ran. A probe statistic.
    pub for_iterations: u64,
    /// `(room x, room y, flag bit) -> does any tile in that room carry
    /// it`. Answering costs 256 map lookups, and `ice_at` asks every
    /// frame for every object.
    room_flags: std::collections::HashMap<(i16, i16, i16), bool>,
    /// Merges whose two sides did not diverge at one shared decision, so
    /// the select condition was the full guard (`state::Merged::fell_back`).
    /// A count, because whether `collapse`'s pairing ever produces this
    /// case is a number and not an argument.
    pub merge_fallbacks: usize,
    /// Literal splits handed out: the ids in `State::frag`.
    next_split: usize,
    /// The STAGE being traced (`trace::stage`): which cuts end it. `None`
    /// outside a multi-stage kernel trace, where a `_hint_normalize()` is
    /// never a cut.
    pub stage: Option<super::stage::StageCtx<D>>,
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
            // `CELESTE_MAX_TRACE_NODES` overrides the budget for a MEASUREMENT
            // (how big a refused trace really is), never silently in production:
            // the default stays the runaway guard it was.
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
            stage: None,
        }
    }

    /// The engine's index for a function named `name`.
    ///
    /// `FN_NAMES` spells a function as `base_N`, where `N` is a counter
    /// the IR frontend assigns in compile order. Reproducing that
    /// counter here would mean reproducing the frontend's traversal and
    /// keeping the two in step forever, so this matches on the BASE and
    /// requires the match to be unique. It is, for all 74 named
    /// functions; the only ambiguous base is `anonymous`, which has four
    /// and which this refuses by construction.
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

    /// The name a function expression is being stored under, as the IR
    /// frontend would spell it: the enclosing assignment target, extended
    /// by each table-constructor field on the way in. `player = { init =
    /// function ... }` is `player.init`, which is `player.init_20`.
    ///
    /// A STACK rather than a parameter threaded through `eval`: the hint
    /// is syntactic, so pushing it around the sub-evaluation is exact,
    /// and `eval`'s signature is on every expression in the tracer.
    fn hint_name(&self) -> Option<String> {
        if self.hint.is_empty() {
            return None;
        }
        Some(self.hint.join("."))
    }

    /// The free variables of a body that resolve to a LOCAL of the
    /// defining scope - which is what the IR frontend captures, and what
    /// `Cell2::Clo` carries as columns.
    ///
    /// Free is decided syntactically, by visiting the variable and prefix
    /// positions only: a field name after a dot is not a variable, and
    /// `obj.x` must not capture a local `x` that happens to be in scope.
    /// `init_object` has exactly that shape - locals `obj`, `type`, `x`,
    /// `y`, and a body full of `obj.x` - so a text scan would capture
    /// three things the interpreter does not.
    ///
    /// Names the scope chain does not have are globals, which are not
    /// captured. Parameters and body-locals are not in the DEFINING
    /// scope, so the same filter removes them.
    fn captures_of(
        &self,
        body: &'a ast::FunctionBody,
        env: super::heap::ScopeId,
        st: &State<D>,
    ) -> Vec<String> {
        // `Visit::visit`, not the `visit_function_body` HOOK. Calling
        // the hook by name runs that one method and recurses into
        // nothing, which looks like a body with no free variables at all
        // - every closure came out with an empty capture list.
        use full_moon::visitors::{Visit, Visitor};

        #[derive(Default)]
        struct Names {
            order: Vec<String>,
            seen: std::collections::HashSet<String>,
            /// Names BOUND inside the body: its parameters, every
            /// `local`, and every loop variable - including nested
            /// functions', which over-subtracts. A capture missed that
            /// way is a difference
            /// `the_tracer_encodes_a_block_the_way_the_importer_does`
            /// reports, which is the reason to prefer this to a
            /// scope-accurate walk that would have to agree with the IR
            /// frontend's by inspection.
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

    /// Intern every function body of a program chunk, in syntactic order,
    /// before it runs: a body's id is then a function of the source, not of
    /// which tracer evaluated which `function` first. A continuation names
    /// bodies by id (`stage::At::Call`, a closure in a local), and a stage is
    /// resumed in a copy of another tracer (`kernel::room_constant_lattice`).
    pub fn intern_all(&mut self, chunk: &'a ast::Ast) {
        use full_moon::visitors::Visitor;
        #[derive(Default)]
        struct Bodies(Vec<*const ast::FunctionBody>);
        impl Visitor for Bodies {
            fn visit_function_body(&mut self, b: &ast::FunctionBody) {
                self.0.push(b as *const ast::FunctionBody);
            }
        }
        let mut v = Bodies::default();
        v.visit_ast(chunk);
        for b in v.0 {
            // SAFETY: `b` points into `chunk`, which lives for `'a`.
            self.intern_body(unsafe { &*b });
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
        self.exec_stmts(block, 0, vec![(st, Flow::Normal)])
    }

    /// Run `block`'s statements from `from` on, then its last statement, on
    /// the states of `live` that are still running: a block from the top,
    /// or the rest of one a stage resumes in (`resume_block`).
    fn exec_stmts(&mut self, block: &'a ast::Block, from: usize, mut live: Outcome<D>) -> Result<Outcome<D>> {
        for (index, stmt) in block.stmts().enumerate().skip(from) {
            if live.iter().all(|(_, f)| !f.is_normal()) {
                break;
            }
            let mut next: Outcome<D> = Vec::new();
            for (s, f) in live {
                if !f.is_normal() {
                    next.push((s, f));
                    continue;
                }
                // A RAISED path executes no further: its lanes are in the
                // raise row (`poison`), so it is live nowhere, and running
                // on would intern nodes nothing reads - on the shape walk
                // that was most of a two-million-node graph.
                if self.d.decide(&s.guard) == Some(false) {
                    continue;
                }
                let out = self.exec_stmt(stmt, s)?;
                next.extend(Self::suspended_at(out, |s| super::stage::At::Stmt { index: index as u32, scope: s.scope }));
            }
            live = self.collapse(next)?;
            if live.len() > self.max_states {
                let mut hist: std::collections::BTreeMap<&str, usize> = Default::default();
                for (st, f) in &live {
                    let k = match f {
                        Flow::Normal => "normal",
                        Flow::Break => "break",
                        Flow::Return(_) => "return",
                        Flow::Suspend => "suspend",
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

    /// Each outcome a level passes SUSPENDED gets that level pushed onto its
    /// continuation (`trace::stage::At`, innermost first): where it was, for
    /// the resume.
    fn suspended_at(out: Outcome<D>, at: impl Fn(&State<D>) -> super::stage::At) -> Outcome<D> {
        out.into_iter()
            .map(|(mut s, f)| {
                if let Flow::Suspend = f {
                    let a = at(&s);
                    s.cont.as_mut().expect("a suspended state carries its continuation").frames.push(a);
                }
                (s, f)
            })
            .collect()
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
            ast::Stmt::FunctionCall(call) => {
                if let Some(cut) = self.cut_at(call, &st) {
                    return Ok(vec![(self.suspend(st, cut)?, Flow::Suspend)]);
                }
                // A call statement is the one place a stage may stop inside
                // a call: its value is dropped, so the rest of the caller is
                // all a resume has to run (`walk_suffixes` refuses the rest).
                Ok(self
                    .walk_suffixes(call.prefix(), &call.suffixes().collect::<Vec<_>>(), st, true)?
                    .into_iter()
                    .map(|(s, _)| {
                        let f = if s.cont.is_some() { Flow::Suspend } else { Flow::Normal };
                        (s, f)
                    })
                    .collect())
            }
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

    /// The cut this call statement is, if the stage being traced stops there:
    /// a bare `_hint_normalize()` at a cut's object (`stage::cut_at`).
    fn cut_at(&self, call: &ast::FunctionCall, st: &State<D>) -> Option<u32> {
        let ctx = self.stage.as_ref()?;
        let ast::Prefix::Name(t) = call.prefix() else { return None };
        if ident(t).ok()? != "_hint_normalize" {
            return None;
        }
        let mut suffixes = call.suffixes();
        match (suffixes.next(), suffixes.next()) {
            (Some(ast::Suffix::Call(ast::Call::AnonymousCall(ast::FunctionArgs::Parentheses { arguments, .. }))), None)
                if arguments.is_empty() => {}
            _ => return None,
        }
        super::stage::cut_at(st, ctx.cuts)
    }

    /// End the stage at cut `cut`: the state leaves with a continuation that
    /// every level it unwinds through adds itself to (`Flow::Suspend`).
    ///
    /// The row it becomes has no column for the buttons: every stage starts
    /// with the reset, as every frame does (`verify::trace_frame`), so a
    /// button this stage READ would be read again, independently, by the next
    /// - two choices for one frame's input. Refused rather than assumed: the
    /// buttons must be exactly what the reset left.
    fn suspend(&mut self, st: State<D>, cut: u32) -> Result<State<D>> {
        let ctx = self.stage.as_ref().expect("a cut is only found while tracing a stage");
        ensure!(
            cut > ctx.stage,
            "cut {cut} reached in stage {} of its frame: a frame passes each cut once, in order",
            ctx.stage
        );
        ensure!(st.frag.is_empty(), "a stage cut inside a literal split");
        let buttons = match st.heap.tables[&st.globals].hash.get("__button_states") {
            Some(Value::Table(b)) => st.heap.tables[b].arr.clone(),
            _ => Vec::new(),
        };
        ensure!(buttons == ctx.buttons, "a stage cut after a button was read: the next stage would choose it again");
        let mut st = st;
        st.cont = Some(super::stage::Cont { stage: 0, cut: Some(cut), frames: Vec::new(), key: String::new() });
        Ok(st)
    }

    /// Run the rest of the frame a stage stopped at a cut (`trace::stage`):
    /// `top` is the frame chunk, `frames` the continuation (innermost first).
    /// Each level carries on where it was, then as if it had never stopped.
    pub fn resume(&mut self, top: &'a ast::Block, frames: &[At], st: State<D>) -> Result<Outcome<D>> {
        let outer = st.scope;
        let path: Vec<At> = frames.iter().rev().cloned().collect();
        self.resume_block(top, &path, st, outer)
    }

    /// Resume in `block` at the statement `path` starts with, then run the
    /// statements after it; the block's outcomes leave in scope `outer`.
    fn resume_block(&mut self, block: &'a ast::Block, path: &[At], mut st: State<D>, outer: ScopeId) -> Result<Outcome<D>> {
        let Some((At::Stmt { index, scope }, rest)) = path.split_first() else {
            bail!("a continuation level {:?} where a statement was expected", path.first());
        };
        let index = *index as usize;
        let stmt = block
            .stmts()
            .nth(index)
            .ok_or_else(|| anyhow!("a continuation at statement {index} of a block of {}", block.stmts().count()))?;
        st.scope = *scope;
        let inner = self.resume_stmt(stmt, rest, st)?;
        let inner = Self::suspended_at(inner, |s| At::Stmt { index: index as u32, scope: s.scope });
        let inner = self.collapse(inner)?;
        let mut out = self.exec_stmts(block, index + 1, inner)?;
        for (s, _) in out.iter_mut() {
            s.scope = outer;
        }
        Ok(out)
    }

    /// Resume inside `stmt` along `path`; an empty path is the cut itself,
    /// which has run.
    fn resume_stmt(&mut self, stmt: &'a ast::Stmt, path: &[At], mut st: State<D>) -> Result<Outcome<D>> {
        let Some((at, rest)) = path.split_first() else {
            return Ok(vec![(st, Flow::Normal)]);
        };
        match (stmt, at) {
            (ast::Stmt::If(iff), At::Arm(a)) => {
                let elseifs: Vec<&'a ast::ElseIf> = iff.else_if().map(|e| e.iter().collect()).unwrap_or_default();
                let blk = match *a as usize {
                    0 => iff.block(),
                    k if k <= elseifs.len() => elseifs[k - 1].block(),
                    k if k == elseifs.len() + 1 => iff.else_block().ok_or_else(|| anyhow!("a continuation in the else of an if without one"))?,
                    k => bail!("a continuation in arm {k} of an if with {} arms", elseifs.len() + 1),
                };
                let outer = st.scope;
                let out = self.resume_block(blk, rest, st, outer)?;
                Ok(Self::suspended_at(out, |_| At::Arm(*a)))
            }
            (ast::Stmt::NumericFor(f), At::Loop { next, to }) => {
                let name = ident(f.index_variable())?;
                let (next, to) = (P8::from_raw(*next), P8::from_raw(*to));
                let outer = st.scope;
                let (mut done, mut again) = (Vec::new(), Vec::new());
                for (s, fl) in self.resume_block(f.block(), rest, st, outer)? {
                    Self::after_body(s, fl, next, to, &mut done, &mut again);
                }
                done.extend(self.run_for(f, &name, next, to, again)?);
                Ok(done)
            }
            (ast::Stmt::FunctionCall(_), At::Call(body)) => {
                let b = *self.bodies.get(*body as usize).ok_or_else(|| anyhow!("a continuation in function body {body}, which this tracer does not have"))?;
                let outer = st.scope;
                st.stack.push(outer);
                let atoms_before = self.d.atoms_minted();
                let flows = self.resume_block(b.block(), rest, st, outer)?;
                Ok(self
                    .return_from(*body, outer, flows, atoms_before)?
                    .into_iter()
                    .map(|(s, _)| {
                        let f = if s.cont.is_some() { Flow::Suspend } else { Flow::Normal };
                        (s, f)
                    })
                    .collect())
            }
            (stmt, at) => bail!("a continuation level {at:?} at a statement it does not fit: {}", stmt.to_string().trim()),
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
        self.exec_arms(&arms, iff.else_block(), st, 0)
    }

    /// The arms from `arms[0]`, which is arm number `arm` of its `if` (the
    /// `else` is the last number): what a suspended stage records.
    fn exec_arms(
        &mut self,
        arms: &[(&'a ast::Expression, &'a ast::Block)],
        els: Option<&'a ast::Block>,
        st: State<D>,
        arm: u32,
    ) -> Result<Outcome<D>> {
        let Some(((cond_e, blk), rest)) = arms.split_first() else {
            return Ok(match els {
                Some(b) => self.exec_arm(b, arm, st)?,
                None => vec![(st, Flow::Normal)],
            });
        };
        let mut out: Outcome<D> = Vec::new();
        for (s, cv) in self.eval(cond_e, st)? {
            let cond = self.truthy(&cv);
            out.extend(match self.decide(&cond) {
                Some(true) => self.exec_arm(blk, arm, s)?,
                Some(false) => self.exec_arms(rest, els, s, arm + 1)?,
                None => {
                    // Each arm only happens under its own condition, so
                    // record that before running it. A merge puts the
                    // original back, because either side happening IS the
                    // original condition.
                    let (ts, fs) = split(&mut self.d, s, &cond);
                    let t = match ts {
                        Some(x) => self.exec_arm(blk, arm, x)?,
                        None => Vec::new(),
                    };
                    let f = match fs {
                        Some(x) => self.exec_arms(rest, els, x, arm + 1)?,
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

    fn exec_arm(&mut self, blk: &'a ast::Block, arm: u32, st: State<D>) -> Result<Outcome<D>> {
        Ok(Self::suspended_at(self.exec_block(blk, st)?, |_| super::stage::At::Arm(arm)))
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
        self.drop_raised(&mut outs);
        let mut pairing = Pairing::new(outs.len());
        'again: loop {
            // Siblings first (longest shared decision prefix), so that a
            // merge selects on the decision that separates its two sides
            // rather than falling back to the full guard - see
            // `State::path` and `merge_order`.
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
                // j > i, so drop the later index first.
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

    /// The same fixpoint for a fanned-out EXPRESSION, where the outcomes
    /// carry a value instead of a flow.
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

    /// Drop the states live nowhere before merging: a raise cleared their
    /// guard (`poison`) and its lanes are in the raise row. Merged instead,
    /// such a state would put a select on the separating decision into
    /// every slot it disagrees in, for lanes that cannot take it.
    fn drop_raised<T>(&mut self, outs: &mut Vec<(State<D>, T)>) {
        outs.retain(|(s, _)| self.d.decide(&s.guard) != Some(false));
    }

    /// Can these two flows be joined - WITHOUT building any nodes? Asked
    /// before the merge so a rejected pair costs nothing; `collapse` now
    /// tries every pair rather than only siblings, so most pairs are
    /// rejected and building a `Sel` for each would be pure garbage.
    fn can_join(&self, a: &Flow<D>, b: &Flow<D>) -> bool {
        match (a, b) {
            (Flow::Return(x), Flow::Return(y)) => joinable(x, y),
            // Two suspended states merge where they stopped at the same place
            // (`state::merge` compares the continuations).
            (Flow::Break, Flow::Break) | (Flow::Normal, Flow::Normal) | (Flow::Suspend, Flow::Suspend) => true,
            _ => false,
        }
    }

    /// Can two values be joined on `cond` without a select some lane cannot
    /// take? On a condition that reads an unknown atom, only an independent
    /// join (`Domain::join_num_independent`) can; otherwise the states stay two
    /// successors, as `state::merge` keeps their heaps.
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
            (Flow::Suspend, _) => Flow::Suspend,
            _ => Flow::Normal,
        }
    }

    /// Only numbers and booleans actually differ; `joinable` has already
    /// established that this pair is one of those or is equal outright.
    fn join_value(&mut self, cond: &D::Bool, a: &Value<D>, b: &Value<D>) -> Value<D> {
        match (a, b) {
            // As a heap slot's join (`state::join`): a condition no lane
            // decides over values every lane holds alike joins, not selects.
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
                Bound::Concrete(from, to) => self.run_for(f, &name, from, to, vec![(s, Flow::Normal)])?,
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
                    // Its next index is symbolic, which a continuation (a
                    // shape) cannot hold.
                    Flow::Suspend => bail!("a stage cut inside a loop with a symbolic bound"),
                }
            }
        }
        // Where the model ENDS: a state still running when the bound ran
        // out goes on as if the loop were over, and the lanes on which it
        // was not are unmodelled from here (`State::ended`). Only the states
        // that never left: one that broke or returned never depended on the
        // bound.
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
            // `Flow::Break` is passed THROUGH. The caller decides what
            // leaving the loop means; swallowing it here made `break` a
            // no-op for the symbolic-bound loop.
            out.push((s2, f2));
        }
        Ok(out)
    }

    /// A numeric `for` with concrete bounds, from index `from`, on the
    /// states of `running`: a loop from its start, or the rest of one a stage
    /// resumes in.
    fn run_for(
        &mut self,
        f: &'a ast::NumericFor,
        name: &str,
        from: P8,
        to: P8,
        mut running: Outcome<D>,
    ) -> Result<Outcome<D>> {

        // A `break` ends the LOOP for that state, not the state: it moves
        // to `done` and carries on after the loop. Getting this wrong the
        // first time made `break` a no-op, so `foreach`'s bounded walk ran
        // its full 32767 iterations instead of stopping.
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
                    Self::after_body(s2, f2, i + one, to, &mut done, &mut next);
                }
            }
            running = next;
            i = i + one;
        }
        // Whatever was still going when the bound ran out just continues.
        done.extend(running.into_iter().map(|(s, _)| (s, Flow::Normal)));
        Ok(done)
    }

    /// Where one outcome of a loop body goes: out of the loop (a `break`, a
    /// `return`, a suspended stage - which records that the loop goes on at
    /// `next`) or round again.
    fn after_body(s: State<D>, f: Flow<D>, next: P8, to: P8, done: &mut Outcome<D>, again: &mut Outcome<D>) {
        match f {
            Flow::Break => done.push((s, Flow::Normal)),
            Flow::Return(v) => done.push((s, Flow::Return(v))),
            Flow::Normal => again.push((s, Flow::Normal)),
            Flow::Suspend => done.extend(Self::suspended_at(vec![(s, Flow::Suspend)], |_| super::stage::At::Loop {
                next: next.as_raw_u32() as i32,
                to: to.as_raw_u32() as i32,
            })),
        }
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
                    self.walk_suffixes(ve.prefix(), &suffixes, st, false)
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
                                // `player = { init = function ... }` is
                                // `player.init` to the IR frontend, so the
                                // field name extends the hint for exactly
                                // this sub-evaluation.
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

    /// A LUA RAISE on this path: route its lanes to the frame's raise row.
    ///
    /// PICO-8 raises on `nil > 0`, and a raise ends the game: the lanes that
    /// get here have NO successor, which is what clearing `guard` says. It is
    /// not a silent drop, because nothing is thrown away - the guard at the
    /// raise goes to `raised`, whose OR is the raise row's liveness
    /// (`verify::Frame::raise`, plans/graph-model.md section 4), so every
    /// lane still ends in exactly one row. The lanes that raise are a set the
    /// build can name and the walk reports when it is not empty
    /// (`kernel::room_constant_lattice`).
    ///
    /// Where it fires, the real game cannot reach it: in rooms (7,0), (6,1)
    /// and (7,1) a spring standing on a breakable floor reaches `this.delay
    /// > 0` with `delay` unset only through a merge that lost the
    /// correlation between `spr` and `hide_for` (plans/graph-model.md, "The
    /// raise is NOT reachable concretely"). Routing those lanes away is exact
    /// either way: a state that raises has no successor, and one that does
    /// not takes the other arm, whose guard is emitted.
    ///
    /// Counted in `illegal` by reason as well, the build-time diagnostic.
    fn poison(&mut self, st: &mut State<D>, why: String) {
        // The lanes are ROUTED, not declined: they leave this state for the
        // raise row, whose liveness is the OR of the guards recorded here, so
        // every lane still ends in exactly one outcome.
        self.raised.push((st.guard.clone(), why.clone()));
        st.guard = self.d.boolean(false);
        *self.illegal.entry(why).or_default() += 1;
    }

    /// The value, and - if Lua itself would have raised - why this path
    /// is not a legal run. The caller poisons, because it is the caller
    /// that knows which expression this was. An undecided comparison is its
    /// plain node: a select on it becomes a fork only if one survives the
    /// traced frame (`verify::fork_undecided_selects`).
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
            Value::Str(s) => {
                // `cart::check_absent_fields` sees only `.field`: a bracketed
                // or computed key naming an absent-as-zero field escapes it.
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

    /// Walk a prefix and its suffixes as an rvalue.
    ///
    /// `stmt`: this is a call STATEMENT, whose last call may suspend the
    /// stage (`trace::stage`); a suspension anywhere else - inside an
    /// expression, where the resume would have to finish evaluating it -
    /// refuses the trace.
    fn walk_suffixes(
        &mut self,
        p: &'a ast::Prefix,
        suffixes: &[&'a ast::Suffix],
        st: State<D>,
        stmt: bool,
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
        for (k, suffix) in suffixes.iter().enumerate() {
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
                            if got.iter().any(|(s, _)| s.cont.is_some()) && !(stmt && k + 1 == suffixes.len()) {
                                bail!("calling {path}: a stage cut inside an expression (only a call statement may stop a stage)");
                            }
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
                for (st, target) in self.walk_suffixes(e.prefix(), init, st, false)? {
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
        self.walk_suffixes(call.prefix(), &suffixes, st, false)
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
                // The caller's scope must stay a GC ROOT while the callee
                // runs: the frame's parent is the closure's captured
                // scope, not the caller's.
                st.stack.push(outer);
                st.scope = frame;
                let atoms_before = self.d.atoms_minted();
                let flows = self.exec_block(b.block(), st)?;
                self.return_from(body, outer, flows, atoms_before)
            }
            Value::Builtin(name) => self.call_builtin(name, args, st),
            other => bail!("calling a non-function: {:?}", other),
        }
    }

    /// A function body's outcomes as its call's: back in the caller's scope
    /// `outer`, with the value returned. A suspended one (`trace::stage`) keeps
    /// its state marked by its continuation, which records the function it
    /// stopped in; the caller's call statement passes it on.
    fn return_from(&mut self, body: BodyId, outer: super::heap::ScopeId, flows: Outcome<D>, atoms_before: u32) -> Result<Multi<D, Value<D>>> {
        let mut out = Vec::new();
        for (mut s, flow) in Self::suspended_at(flows, |_| super::stage::At::Call(body)) {
            s.stack.pop();
            s.scope = outer;
            out.push(match flow {
                Flow::Return(v) => (s, v),
                Flow::Normal | Flow::Suspend => (s, Value::Nil),
                Flow::Break => bail!("break outside a loop"),
            });
        }
        // The literal splits made inside this call rejoin as it
        // returns (`rejoin_fragments`).
        if out.iter().any(|(s, _)| s.frag.last().is_some_and(|f| f.0 > s.stack.len())) {
            out = self.rejoin_fragments(out)?;
        }
        // The atoms made inside this call do not escape it
        // (`Domain::escaped_atom`).
        if self.d.atoms_minted() > atoms_before {
            out = out.into_iter().map(|(s, v)| self.fork_escaped_atoms(s, v, atoms_before)).collect();
        }
        Ok(out)
    }

    /// Every heap boolean and the returned value that hold an atom made at or
    /// after `since` become that atom's fork of both values
    /// (`Domain::escaped_atom`).
    fn fork_escaped_atoms(&mut self, mut s: State<D>, v: Value<D>, since: u32) -> (State<D>, Value<D>) {
        // Only what the caller can still reach: the callee's frame is garbage,
        // and a fork for an atom in it spends a fork id on nothing (room (3,0)
        // with the fruit and the floors unknown: 63 forks, when a fork mask
        // held 58).
        let mut roots = s.roots();
        super::heap::push_value(&v, &mut roots);
        let (tables, scopes, _) = s.heap.reachable(&roots);
        let d = &mut self.d;
        // The origin is named only when a fork is minted: a table's key, with
        // the table's `x`, `y` where it has them (which fall floor).
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

    /// Rejoin the fragments of the literal splits made inside a call that has
    /// just returned (`State::frag`, plans/fly-fruit.md). The states of one
    /// split, alike in every other fragment tag, merge on an undecided atom:
    /// no lane picks a fragment (the split value is the same in every lane),
    /// so everything they differ in must JOIN - a literal hull or the unknown
    /// number (`Domain::join_num_independent`). A select that survives, a
    /// return value that does not join, or two shapes refuse the trace: the
    /// fragments did not rejoin.
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
                let v = self.d.fun1(f, &a)?;
                let at = st.decided(&mut self.d);
                self.d.evaluated_at(&v, &at);
                (st, Value::Num(v))
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
            // fragment and this is the identity. On a WIDENED one the
            // lane holds points whose floors differ, and this is the
            // place the cart marks for the program to enumerate the
            // cases (T24).
            //
            // The fork is a CHOICE, not a state fan-out: one node whose
            // value depends on the fragment, which specialization
            // enumerates exactly as it does the six buttons. Its
            // validity goes in the GUARD, because the fragments
            // partition the lane and a lane in neither is not a lane at
            // all; its coverage is the fork's own error, because a lane
            // spanning more floors than there are fragments is REAL and
            // this body cannot run it.
            "__split_by_flr" | "__split_at" => {
                let x = num(0)?;
                // A value every lane holds alike (a literal interval at a
                // fruit-unknown set, plans/fly-fruit.md): each fragment runs as
                // its own trace state and they rejoin when the enclosing call
                // returns (`rejoin_fragments`), so the split multiplies no
                // configuration of the frame.
                if let Some(frags) = self.d.literal_fragments(&x) {
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
                    (st, args[0].clone())
                } else {
                    // The fork's coverage is its own error, derived from the
                    // fork node where it was evaluated (`trace::error`).
                    let ways = self.d.flr_ways(&x);
                    let (v, valid) = self.d.fork_flr(&x, ways);
                    let at = st.decided(&mut self.d);
                    self.d.evaluated_at(&v, &at);
                    let mut st = st;
                    st.guard = self.d.and(&st.guard, &valid);
                    (st, Value::Num(v))
                }
            }
            "__new_unknown_boolean" => (st, Value::Bool(self.d.unknown_bool()?)),
            // `rnd(x)`: PICO-8's generator draws in [0, x), `x` defaulting
            // to 1. The draw depends on a seed nothing in the search
            // models, so its value is the whole range - a sound
            // over-approximation (room (5,0)'s balloon phase, the chest's
            // shake). A bound that is not a positive constant is refused,
            // not guessed.
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

/// An assignment target as the IR frontend spells it: a bare name, or a
/// dotted path (`obj.is_solid`). `None` for anything else - an index
/// expression names nothing a function could be looked up by.
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

/// Which pairs of a `collapse` frontier are worth trying to merge.
///
/// Every outcome has an id, increasing with its position (so a pair's ids are
/// in the order of its indices), and its shape's hash: two different shapes
/// never merge. A pair that refused stays refused - `merge` is a function of
/// the two states, and nothing here changes a state, it only replaces a merged
/// pair - so it is tried once per collapse rather than again after every
/// merge. A frontier that keeps successors apart used to re-try all of its
/// pairs after each merge (room (3,0) at a fruit-unknown set, 2026-09-17).
struct Pairing {
    /// Per outcome: its id, and its shape's hash and `Canon` once `prepare`
    /// computed them - once per outcome rather than per attempt.
    keys: Vec<(usize, Option<(u64, Canon)>)>,
    /// Every outcome holds the first one's heap (`state::same_heap`): they
    /// pair object by object and need no `Canon`.
    same: bool,
    /// Whether `same` can still hold: once the heaps differ, canons it is.
    same_possible: bool,
    next: usize,
    refused: std::collections::HashSet<(usize, usize)>,
}

impl Pairing {
    fn new(n: usize) -> Self {
        Pairing { keys: (0..n).map(|i| (i, None)).collect(), same: false, same_possible: true, next: n, refused: Default::default() }
    }

    /// Before pairing `states` (in `keys`' order): decide how they pair, and
    /// compute the canons that needs. A lone outcome is never paired.
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
        // The merged heap is collected, the others are not: recheck.
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
        State { heap, globals, scope, stack: Vec::new(), guard: t, ended: f, path: Vec::new(), key_override: Vec::new(), frag: Vec::new(), cont: None }
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

    /// A call whose branches leave the heap alike but RETURN different
    /// numbers: the value is a select, and its own error is its condition
    /// being undecided - derived from the value (`trace::error`), where it
    /// used to be conjoined into the state by the merge. Without it a lane
    /// where the condition is undecided took one arm silently.
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
