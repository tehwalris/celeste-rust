//! Deciding boolean equality on the graph, instead of sampling it.
//!
//! `Graph::fold` normalizes: it rewrites syntax it recognizes. That is
//! cheap and it halved the emitted body, but it is bounded by the shapes
//! someone thought to write down. Measuring what was left said so
//! bluntly - of the 514 nodes where constancy is CREATED (constant at
//! every one of 193 sampled game states, with no constant operand), the
//! three patterns worth guessing at from a handful of examples
//! (`a and not a`, `a or not a`, `(a and x) or (not a and x)`) covered
//! 23. The other 491 are `And`/`Or` nodes with no shape in common.
//!
//! So the question "is this boolean node constant" has to be DECIDED
//! rather than pattern-matched. That is what an ROBDD does: build each
//! boolean node's function over its atoms, and two nodes are equal
//! exactly when their BDDs are the same reference. Constant is the
//! special case where the reference is a terminal.
//!
//! `simplify_local` does this PER NODE, on a small BDD of the node's own
//! bounded cone, once, in topological order over the rewritten graph. A
//! global analysis - one table for the whole graph, iterated four times -
//! stood here until 2026-09-15: on a fused kernel of the bucket dispatch
//! its 2^22-node cap filled on the first formulas and 5,500 to 8,300
//! `And`/`Or`/`Not` per kernel went unanalysed, at ~all of the build's
//! CPU. The local pass finds more (room (1,0)'s exact-speed player shape:
//! 11,844 fused nodes against 7,569) at ~1/150 of the time.
//!
//! ## What counts as an atom
//!
//! Everything that is not `And`, `Or`, `Not`, `ConstBool` or a `Sel`
//! between booleans: comparisons, `TileFlagAt`, `Known`, `Free`, boolean
//! input cells. Atoms are treated as INDEPENDENT free variables, which is
//! the source of this analysis's incompleteness and also of its
//! soundness:
//!
//! * `x < 3` and `x > 5` are two atoms, so their conjunction is
//!   satisfiable here and the analysis will not fold it. It MISSES real
//!   constants. That is fine.
//! * `Known(x)` and `x` are two atoms, likewise.
//! * But if a function is constant when its atoms range over ALL
//!   assignments, it is constant over the realizable ones too, because
//!   those are a subset. So everything this DOES conclude holds.
//!
//! Incomplete and sound is the right side to err on: a missed fold costs
//! emitted lines, an unsound one costs correctness.
//!
//! ## What this is not
//!
//! Not a SAT solver over pico-8 arithmetic. The numeric layer is left
//! alone - but it still benefits, because a numeric `Sel` whose condition
//! this proves constant folds through `Graph::fold` when the graph is
//! rebuilt.

use rustc_hash::FxHashMap;

use super::graph::{Graph, NodeId, Op};

/// `simplify_local`'s settings for the lowering: the most BDD nodes one
/// node's analysis may build, and the compound nodes of its cone it expands
/// before treating the rest as opaque. Measured on six kernels of room
/// (1,0) (2026-09-15): every expansion from 4 to 32 finds the same rewrites
/// (3,110 merges, 8 constants, 71,008 fused nodes against the global
/// pass's 84,923); 64 and above overflow the local table and find LESS.
pub const LOCAL_CAP: usize = 1 << 12;
pub const LOCAL_EXPAND: usize = 8;

/// A reference into the BDD's node table. 0 and 1 are the terminals.
pub type Ref = u32;
pub const FALSE: Ref = 0;
pub const TRUE: Ref = 1;

/// The image of a node `simplify_local` did not rebuild. Deliberately not 0:
/// an out-of-range id panics in `Graph::get`, where node 0 would quietly
/// be some unrelated constant.
pub const UNREACHABLE: NodeId = NodeId::MAX;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Triple {
    /// Index into the variable order. Terminals compare as `u32::MAX`, so
    /// the ordering test needs no special case.
    var: u32,
    lo: Ref,
    hi: Ref,
}

/// A reduced ordered BDD over a set of opaque atoms.
pub struct Bdd {
    nodes: Vec<Triple>,
    intern: FxHashMap<Triple, Ref>,
    memo: FxHashMap<(Ref, Ref, Ref), Ref>,
    /// The budget. An ROBDD can be exponential in its variable count, and
    /// a guard chain over hundreds of atoms is exactly the shape that
    /// blows up, so the cap is not decoration. Hitting it makes the
    /// operation return `None`, which the caller counts (`Stats::unanalysed`).
    cap: usize,
}

impl Bdd {
    pub fn new(cap: usize) -> Self {
        let dummy = Triple { var: u32::MAX, lo: 0, hi: 0 };
        Bdd {
            nodes: vec![dummy, dummy],
            intern: FxHashMap::default(),
            memo: FxHashMap::default(),
            cap,
        }
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    fn var_of(&self, r: Ref) -> u32 {
        self.nodes[r as usize].var
    }

    fn is_terminal(r: Ref) -> bool {
        r <= 1
    }

    /// The reduce step: a node whose branches agree is not a node, and an
    /// identical triple is the same node. Together these are what make
    /// equality of functions the same thing as equality of references.
    fn mk(&mut self, var: u32, lo: Ref, hi: Ref) -> Option<Ref> {
        if lo == hi {
            return Some(lo);
        }
        let t = Triple { var, lo, hi };
        if let Some(r) = self.intern.get(&t) {
            return Some(*r);
        }
        if self.nodes.len() >= self.cap {
            return None;
        }
        let r = self.nodes.len() as Ref;
        self.nodes.push(t);
        self.intern.insert(t, r);
        Some(r)
    }

    /// The one operation everything else is written in terms of.
    pub fn ite(&mut self, f: Ref, g: Ref, h: Ref) -> Option<Ref> {
        if f == TRUE {
            return Some(g);
        }
        if f == FALSE {
            return Some(h);
        }
        if g == h {
            return Some(g);
        }
        if g == TRUE && h == FALSE {
            return Some(f);
        }
        if let Some(r) = self.memo.get(&(f, g, h)) {
            return Some(*r);
        }
        let v = self
            .var_of(f)
            .min(self.var_of(g))
            .min(self.var_of(h));
        let branch = |b: &Bdd, r: Ref, hi: bool| -> Ref {
            if Bdd::is_terminal(r) || b.var_of(r) != v {
                r
            } else if hi {
                b.nodes[r as usize].hi
            } else {
                b.nodes[r as usize].lo
            }
        };
        let (fh, gh, hh) = (branch(self, f, true), branch(self, g, true), branch(self, h, true));
        let (fl, gl, hl) = (branch(self, f, false), branch(self, g, false), branch(self, h, false));
        let hi = self.ite(fh, gh, hh)?;
        let lo = self.ite(fl, gl, hl)?;
        let r = self.mk(v, lo, hi)?;
        self.memo.insert((f, g, h), r);
        Some(r)
    }

    pub fn not(&mut self, f: Ref) -> Option<Ref> {
        self.ite(f, FALSE, TRUE)
    }
    pub fn and(&mut self, f: Ref, g: Ref) -> Option<Ref> {
        self.ite(f, g, FALSE)
    }
    pub fn or(&mut self, f: Ref, g: Ref) -> Option<Ref> {
        self.ite(f, TRUE, g)
    }

    fn var(&mut self, index: u32) -> Option<Ref> {
        self.mk(index, FALSE, TRUE)
    }

    /// Empty again, keeping the tables' capacity (`simplify_local` reuses
    /// one table for every node it analyses).
    fn reset(&mut self) {
        self.nodes.truncate(2);
        self.intern.clear();
        self.memo.clear();
    }
}

/// Does this op PRODUCE a boolean, whatever its operands are? Used only
/// to decide whether a `Sel` is a boolean select; everything else is
/// driven by the op itself.
fn inherently_bool(op: &Op) -> bool {
    matches!(
        op,
        Op::ConstBool(_)
            | Op::Not
            | Op::And
            | Op::Or
            | Op::Known
            | Op::SplitValid(_)
            | Op::SplitOk(_)
            | Op::Free(_)
            | Op::Lt
            | Op::Le
            | Op::Gt
            | Op::Ge
            | Op::Eq
            | Op::TileFlagAt
    )
}

/// Nodes reachable from `roots`, walking operands - everything that can
/// affect what those roots compute, and nothing else.
pub fn reachable(g: &Graph, roots: &[NodeId]) -> Vec<bool> {
    let mut need = vec![false; g.len()];
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if need[n as usize] {
            continue;
        }
        need[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }
    need
}

/// What `simplify_local` did, for reporting. Every field exists because a
/// simplifier that reports only its output size cannot be told from one
/// that silently gave up.
#[derive(Debug, Default, Clone, Copy)]
pub struct Stats {
    pub before: usize,
    pub after: usize,
    /// Boolean nodes proved constant and replaced.
    pub constants: usize,
    /// Boolean nodes proved equal to an ATOM and replaced by it.
    pub to_atom: usize,
    /// Branch re-merges collapsed to their common factor - the
    /// `(A & p) | (A & not p) -> A` family. See the safety proof on
    /// `simplify_local`.
    pub merged: usize,
    /// Of `merged`, those of the tracer's re-merge shape with SYNTACTIC
    /// complements - two terms, one residual each, `p` and `Not(p)` or a
    /// complementary comparison over the same operands: the ones no BDD
    /// is needed to prove (2026-09-15, measuring what the BDD buys).
    pub merged_simple: usize,
    /// `And`/`Or`/`Not` nodes the analysis gave no function (the cap ran
    /// out before them, or an operand had none): not simplified at all.
    pub unanalysed: usize,
    /// Duplicate leaves dropped from `And`/`Or` trees.
    pub deduped: usize,
    /// The largest local table one node's analysis built.
    pub bdd_nodes: usize,
}

/// Flatten a same-op tree (`Or`-of-`Or`s or `And`-of-`And`s) in `g` into
/// its non-`op` leaves. `None` when the tree is bigger than `cap`, which
/// bounds the work per candidate rather than trusting the input.
fn flatten(g: &Graph, root: NodeId, want_and: bool, cap: usize) -> Option<Vec<NodeId>> {
    let mut leaves = Vec::new();
    let mut stack = vec![root];
    while let Some(x) = stack.pop() {
        let n = g.get(x);
        let same = matches!((&n.op, want_and), (Op::And, true) | (Op::Or, false));
        if same {
            stack.extend(n.args.iter().copied());
        } else {
            leaves.push(x);
            if leaves.len() > cap {
                return None;
            }
        }
    }
    Some(leaves)
}

/// `x` and `y` are complements by their spelling: one is `Not` of the
/// other, or they are complementary comparisons over the same operands.
fn syntactic_complements(g: &Graph, x: NodeId, y: NodeId) -> bool {
    let (nx, ny) = (g.get(x), g.get(y));
    match (&nx.op, &ny.op) {
        (Op::Not, _) if nx.args[0] == y => true,
        (_, Op::Not) if ny.args[0] == x => true,
        (Op::Lt, Op::Ge) | (Op::Ge, Op::Lt) | (Op::Le, Op::Gt) | (Op::Gt, Op::Le) => nx.args == ny.args,
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// THE LOCAL PASS (2026-09-15)
//
// The global analysis this replaced built every boolean node's function in
// ONE table shared by the whole graph. On a fused kernel of the bucket
// dispatch (~700 atoms, guard chains across hundreds of configurations) a few
// early formulas filled its 2^22-node cap, and every node after them - 5,500
// to 8,300 `And`/`Or`/`Not` per kernel, the `ok`/`live` region included - was
// left unanalysed; four passes of that were ~all of the level-0 bucketed
// set's build CPU (7,292 CPU-seconds of the build's ~7,200 user seconds).
// What it found was almost entirely the common-factor collapse, and a
// collapse needs the functions of a handful of residual leaves, not of the
// graph.
//
// `simplify_local` applies the same three rewrites, under the same safety
// rules, each proved on a small BDD of the node's OWN bounded cone: past the
// expansion budget a compound node is an opaque variable, which proves less
// and never anything false (a tautology over independent variables holds for
// the realizable assignments too). It runs once, in topological order over
// the REWRITTEN graph, so a collapse that exposes another is seen in the
// same pass - what the global analysis needed further passes for (a second
// local pass finds nothing on room (1,0)'s kernels).
// ---------------------------------------------------------------------------

/// A variable of a local analysis: a comparison in canonical form - `Lt`
/// over its operands in order: `Ge(a, b)` is its negation, `Gt(a, b)` is
/// `Lt(b, a)` and `Le(a, b)` that negated - or any other node as itself.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum LocalVar {
    Lt(NodeId, NodeId),
    Node(NodeId),
}

/// A `Sel` between booleans (the test `analyze` uses: an arm's own op).
fn is_bool_sel(g: &Graph, n: NodeId) -> bool {
    let node = g.get(n);
    matches!(node.op, Op::Sel) && (inherently_bool(&g.get(node.args[1]).op) || inherently_bool(&g.get(node.args[2]).op))
}

/// One node's function over its bounded cone (`simplify_local`).
struct Local {
    b: Bdd,
    vars: FxHashMap<LocalVar, u32>,
    /// Per variable, the node that IS it: a GENUINE atom (a comparison, a
    /// cell, a cart lookup - not an opaque compound) in positive polarity.
    /// The only kind of node an equal node may be replaced by.
    rep: Vec<Option<NodeId>>,
    memo: FxHashMap<NodeId, Ref>,
    /// Compound nodes left to expand; past it a compound is opaque.
    expand: usize,
}

impl Local {
    fn new(cap: usize) -> Self {
        Local { b: Bdd::new(cap), vars: FxHashMap::default(), rep: Vec::new(), memo: FxHashMap::default(), expand: 0 }
    }

    fn reset(&mut self, expand: usize) {
        self.b.reset();
        self.vars.clear();
        self.rep.clear();
        self.memo.clear();
        self.expand = expand;
    }

    fn var(&mut self, key: LocalVar, genuine: Option<NodeId>) -> Option<Ref> {
        let next = self.vars.len() as u32;
        let v = *self.vars.entry(key).or_insert(next);
        if v == next {
            self.rep.push(None);
        }
        if let Some(n) = genuine {
            self.rep[v as usize].get_or_insert(n);
        }
        self.b.var(v)
    }

    /// `n`'s function, `None` if the local table overflowed.
    fn func(&mut self, g: &Graph, n: NodeId) -> Option<Ref> {
        if let Some(r) = self.memo.get(&n) {
            return Some(*r);
        }
        let node = g.get(n);
        let compound = matches!(node.op, Op::And | Op::Or | Op::Not) || is_bool_sel(g, n);
        let r = if compound && self.expand == 0 {
            self.var(LocalVar::Node(n), None)?
        } else if compound {
            self.expand -= 1;
            let a = node.args.clone();
            match node.op {
                Op::Not => {
                    let x = self.func(g, a[0])?;
                    self.b.not(x)?
                }
                Op::And => {
                    let (x, y) = (self.func(g, a[0])?, self.func(g, a[1])?);
                    self.b.and(x, y)?
                }
                Op::Or => {
                    let (x, y) = (self.func(g, a[0])?, self.func(g, a[1])?);
                    self.b.or(x, y)?
                }
                _ => {
                    let (c, t, f) = (self.func(g, a[0])?, self.func(g, a[1])?, self.func(g, a[2])?);
                    self.b.ite(c, t, f)?
                }
            }
        } else {
            match node.op {
                Op::ConstBool(v) => {
                    if v {
                        TRUE
                    } else {
                        FALSE
                    }
                }
                Op::Lt => self.var(LocalVar::Lt(node.args[0], node.args[1]), Some(n))?,
                Op::Gt => self.var(LocalVar::Lt(node.args[1], node.args[0]), Some(n))?,
                Op::Ge => {
                    let v = self.var(LocalVar::Lt(node.args[0], node.args[1]), None)?;
                    self.b.not(v)?
                }
                Op::Le => {
                    let v = self.var(LocalVar::Lt(node.args[1], node.args[0]), None)?;
                    self.b.not(v)?
                }
                // A select not recognized as boolean is opaque, never a
                // genuine atom: its abstract value is not an input's.
                Op::Sel => self.var(LocalVar::Node(n), None)?,
                _ => self.var(LocalVar::Node(n), Some(n))?,
            }
        };
        self.memo.insert(n, r);
        Some(r)
    }

    /// The genuine atom node a function IS, where it is a positive variable.
    fn atom(&self, r: Ref) -> Option<NodeId> {
        if Bdd::is_terminal(r) {
            return None;
        }
        let t = self.b.nodes[r as usize];
        if t.lo == FALSE && t.hi == TRUE {
            self.rep.get(t.var as usize).copied().flatten()
        } else {
            None
        }
    }
}

/// The common-factor collapse (see `simplify_local`) of node `id` of the
/// REWRITTEN graph: the operator and factor set to rebuild it from, and
/// whether it was the syntactic-complement shape.
fn factor_local(g: &Graph, local: &mut Local, id: NodeId, expand: usize) -> Option<(Op, Vec<NodeId>, bool)> {
    use std::collections::BTreeSet;
    let dual = match g.get(id).op {
        Op::Or => false,
        Op::And => true,
        _ => return None,
    };
    let terms = flatten(g, id, dual, 64)?;
    if terms.len() < 2 {
        return None;
    }
    let mut parts: Vec<Vec<NodeId>> = Vec::with_capacity(terms.len());
    for t in &terms {
        parts.push(flatten(g, *t, !dual, 64)?);
    }
    let mut common: BTreeSet<NodeId> = parts[0].iter().copied().collect();
    for p in parts.iter().skip(1) {
        let here: BTreeSet<NodeId> = p.iter().copied().collect();
        common = common.intersection(&here).copied().collect();
        if common.is_empty() {
            return None;
        }
    }
    local.reset(expand);
    let mut joint = if dual { TRUE } else { FALSE };
    for p in &parts {
        let mut res = if dual { FALSE } else { TRUE };
        for l in p.iter().filter(|l| !common.contains(l)) {
            let r = local.func(g, *l)?;
            res = if dual { local.b.or(res, r)? } else { local.b.and(res, r)? };
        }
        joint = if dual { local.b.and(joint, res)? } else { local.b.or(joint, res)? };
    }
    if joint != if dual { FALSE } else { TRUE } {
        return None;
    }
    let residual = |p: &[NodeId]| -> Vec<NodeId> { p.iter().copied().filter(|l| !common.contains(l)).collect() };
    let simple = parts.len() == 2 && {
        let (x, y) = (residual(&parts[0]), residual(&parts[1]));
        x.len() == 1 && y.len() == 1 && syntactic_complements(g, x[0], y[0])
    };
    Some((if dual { Op::Or } else { Op::And }, common.into_iter().collect(), simple))
}

/// Rebuild `g` over the nodes `roots` reach with the boolean layer
/// simplified LOCALLY (see the section note): per `And`/`Or`/`Not`/boolean
/// `Sel` of the rewritten graph, its function on a BDD of at most `cap`
/// nodes over its cone expanded to `expand` compound nodes, then a
/// constant, a genuine atom, or the common-factor collapse where proved.
/// Returns the new graph, the old -> new map (`UNREACHABLE` outside the
/// reachable set) and what it did (`Stats::unanalysed`: nodes whose local
/// table overflowed).
///
/// The numeric layer is copied through `fold`, which is where it picks up
/// the benefit: a select whose condition just became `ConstBool` folds to
/// one of its arms, and nothing the other arm used is referenced any more.
///
/// ## Why only constants and atoms
///
/// The BDD proves that two nodes compute the same CONCRETE function. It
/// does not follow that they are interchangeable, because the graph is
/// also evaluated ABSTRACTLY - in Kleene, per lane - and two forms of one
/// function approximate it differently. `And(x, y)` decides `false` as
/// soon as either side is known false; a select-shaped form of the same
/// function may answer `unknown` there. Substituting the coarser form for
/// the finer one is SOUND, but it makes lanes undecided that were
/// decided, and an undecided `live` is not merely slower - it is a lane
/// that may fall out of every outcome.
///
/// Constants and atoms are the two cases where the representative is
/// provably the MOST precise form there is:
///
/// * a constant is exact by definition;
/// * an atom's abstract value is its input's, and a node equal to that
///   atom computes exactly "read this input", so no form of it can be
///   more precise. Hence a GENUINE atom only: an opaque compound standing
///   in for its cone is not one.
///
/// ## The common-factor collapse (2026-08-26)
///
/// There is a THIRD safe case, and it is a specific SHAPE, not a
/// general "merge into anything in your own cone". The dominant
/// redundancy in a traced frame is the tracer's branch re-merge
/// `(A & p) | (A & not p)`, which is `A` again - three levels deep in
/// the dash block, once per direction combo (`plans/graph-audit.md`).
///
/// **Rule.** Let `N` be an `Or` whose flattened disjuncts are
/// `And`-trees `dᵢ = ⋀ Cᵢ ∪ Pᵢ`, where every disjunct contains the same
/// non-empty common factor set `C` (compared by node identity in the
/// rewritten graph) and `Pᵢ` are the residual conjuncts. If the local BDD
/// proves `⋁ᵢ (⋀ Pᵢ) ≡ true`, rewrite `N` to `⋀ C`. (An empty `Pᵢ` is
/// `true`, which covers absorption `A ∨ (A ∧ p)`.) The `And`-of-`Or`s dual
/// with `⋀ᵢ (⋁ Pᵢ) ≡ false` rewrites to `⋁ C`.
///
/// **Safety** (never makes a lane less decided; the substitution
/// invariant is `val(map[x]) ⊒ val(x)` in the information order, with
/// concrete equality). Concretely `N = C ∧ ⋁Pᵢ = C ∧ true = C` by
/// distributivity and the tautology, so the concrete half holds. For
/// decidedness, suppose `N`'s Kleene value is decided:
///
/// * decided TRUE: some disjunct is true, so all its conjuncts are
///   true, so every member of `C` (whose image is `⊒` some true
///   conjunct) is true - `⋀C` is true.
/// * decided FALSE: every disjunct has a decided-false conjunct. If any
///   of those is a `C` member, `⋀C` is false. If all of them are
///   residual, then every `⋀Pᵢ` is decided false, and concretizing the
///   undecided atoms keeps them false - contradicting the tautology
///   `⋁ᵢ⋀Pᵢ ≡ true`, which the BDD proved over ALL assignments of its
///   variables (independent there, a superset of the realizable ones - an
///   opaque compound variable included).
///
/// So `N` decided forces `⋀C` decided and equal; `N` undecided needs
/// nothing. ∎
///
/// **Why not the audit's broader rule.** `plans/graph-audit.md`
/// proposed merging `N` into ANY node `M` of `N`'s own cone with the
/// same BDD reference, with a monotonicity proof. That proof is WRONG:
/// it writes `N = F(M, v)` and lets `M` range independently of `v`,
/// which fails when `v` shares atoms with `M`. Counterexample:
/// `A = (a ∧ ¬a) ∨ (d ∧ e)` (concretely `d ∧ e`), `N = A ∧ (d ∧ e)` -
/// same BDD reference, `A` in `N`'s cone, but at `a = ⊥, d = false`
/// Kleene gives `N = ⊥ ∧ false = false` (decided) while `A = ⊥`.
/// `false ∧ ⊥ = false` MANUFACTURES decidedness, so an ancestor is not
/// always at-least-as-decided. The general merge was implemented first
/// and the preservation gate caught it losing a decided lane; only the
/// factor shape above survives.
///
/// This is deliberately NOT a `Graph::fold` rule: `fold`'s contract
/// (`folding_is_exact_not_merely_sound`) is exactness in Kleene, and
/// this rewrite is a refinement (at `A = true`, `p = ⊥` the merged form
/// is `⊥`, `A` is `true`). It also needs the complementary-comparison
/// knowledge (`gt`/`le` on the same operands are one variable) that
/// only the BDD has. Dropping a duplicate leaf, by contrast, IS exact in
/// Kleene (idempotence, associativity, commutativity).
pub fn simplify_local(g: &Graph, roots: &[NodeId], cap: usize, expand: usize) -> (Graph, Vec<NodeId>, Stats) {
    let need = reachable(g, roots);
    let mut out = g.like();
    let mut map: Vec<NodeId> = vec![UNREACHABLE; g.len()];
    let mut st = Stats { before: need.iter().filter(|x| **x).count(), ..Default::default() };
    let mut local = Local::new(cap);
    // Per node of `out`, what it settled to: hash-consing hands back a node
    // already analysed.
    let mut settled: FxHashMap<NodeId, NodeId> = FxHashMap::default();
    for id in 0..g.len() {
        if !need[id] {
            continue;
        }
        let node = g.get(id as NodeId);
        let args: Vec<NodeId> = node.args.iter().map(|x| map[*x as usize]).collect();
        let built = out.fold(node.op.clone(), args);
        if let Some(s) = settled.get(&built) {
            map[id] = *s;
            continue;
        }
        // A leaf repeated in an `And`/`Or` tree goes: idempotence, with
        // associativity and commutativity, is exact in Kleene. The census
        // of room (1,0)'s kernels counted 17 to 410 such leaves per kernel
        // after the rewrites below (2026-09-15).
        let new = match out.get(built).op {
            Op::And | Op::Or => {
                let op = out.get(built).op.clone();
                match flatten(&out, built, matches!(op, Op::And), 64) {
                    Some(leaves) => {
                        let mut seen: rustc_hash::FxHashSet<NodeId> = Default::default();
                        let uniq: Vec<NodeId> = leaves.iter().copied().filter(|l| seen.insert(*l)).collect();
                        if uniq.len() < leaves.len() {
                            st.deduped += leaves.len() - uniq.len();
                            let mut it = uniq.into_iter();
                            let first = it.next().expect("a tree has a leaf");
                            it.fold(first, |acc, m| out.fold(op.clone(), vec![acc, m]))
                        } else {
                            built
                        }
                    }
                    None => built,
                }
            }
            _ => built,
        };
        if let Some(s) = settled.get(&new).copied() {
            settled.insert(built, s);
            map[id] = s;
            continue;
        }
        let structural = matches!(out.get(new).op, Op::And | Op::Or | Op::Not) || is_bool_sel(&out, new);
        let result = if !structural {
            new
        } else {
            local.reset(expand);
            match local.func(&out, new) {
                None => {
                    st.unanalysed += 1;
                    new
                }
                Some(r) if Bdd::is_terminal(r) => {
                    st.constants += 1;
                    out.leaf(Op::ConstBool(r == TRUE))
                }
                Some(r) => match local.atom(r).filter(|at| *at != new) {
                    Some(at) => {
                        st.to_atom += 1;
                        at
                    }
                    None => match factor_local(&out, &mut local, new, expand) {
                        Some((op, common, simple)) => {
                            st.merged += 1;
                            st.merged_simple += simple as usize;
                            let mut it = common.into_iter();
                            let first = it.next().expect("a non-empty factor");
                            it.fold(first, |acc, m| out.fold(op.clone(), vec![acc, m]))
                        }
                        None => new,
                    },
                },
            }
        };
        st.bdd_nodes = st.bdd_nodes.max(local.b.len());
        settled.insert(new, result);
        settled.insert(built, result);
        map[id] = result;
    }
    let mapped: Vec<NodeId> = roots.iter().map(|r| map[*r as usize]).collect();
    st.after = reachable(&out, &mapped).iter().filter(|x| **x).count();
    (out, map, st)
}

/// The census of what a simplified graph still holds, for finding the next
/// simplification in the data rather than by guessing (`transpile
/// --key-probe`, 2026-09-15): the op histogram over what `roots` reach; the
/// maximal `And`/`Or` trees, and in them complementary or duplicate leaves,
/// common factors (all terms, or some pair) and comparisons of one operand
/// against constants; and the select shapes that could collapse.
pub fn census(g: &Graph, roots: &[NodeId]) -> String {
    use std::collections::{BTreeMap, BTreeSet};
    use std::fmt::Write as _;
    let live = reachable(g, roots);
    let ids: Vec<NodeId> = (0..g.len() as NodeId).filter(|n| live[*n as usize]).collect();
    let mut out = String::new();
    // The histogram.
    let mut ops: BTreeMap<String, usize> = BTreeMap::new();
    for &n in &ids {
        let name = format!("{:?}", g.get(n).op);
        let name = name.split('(').next().unwrap_or("").to_string();
        *ops.entry(name).or_default() += 1;
    }
    let mut by_count: Vec<(String, usize)> = ops.into_iter().collect();
    by_count.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    let _ = writeln!(out, "    census: {} live nodes; ops {}", ids.len(), by_count.iter().map(|(o, c)| format!("{o} {c}")).collect::<Vec<_>>().join(", "));
    // A node is the root of its same-op tree when no live parent has its op.
    let mut same_parent = vec![false; g.len()];
    for &n in &ids {
        let node = g.get(n);
        if matches!(node.op, Op::And | Op::Or) {
            for a in &node.args {
                if g.get(*a).op == node.op {
                    same_parent[*a as usize] = true;
                }
            }
        }
    }
    let is_cmp_const = |x: NodeId| -> Option<NodeId> {
        let nd = g.get(x);
        if !matches!(nd.op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq) {
            return None;
        }
        match (&g.get(nd.args[0]).op, &g.get(nd.args[1]).op) {
            (Op::Const(..), Op::Const(..)) => None,
            (_, Op::Const(..)) => Some(nd.args[0]),
            (Op::Const(..), _) => Some(nd.args[1]),
            _ => None,
        }
    };
    // What a rewrite would REMOVE, not just where it applies: duplicate
    // leaves past the first, same-direction comparisons of one operand
    // against constants past the tightest, and the conjuncts a full common
    // factor repeats.
    let (mut dup_removable, mut range_removable, mut factor_removable) = (0usize, 0usize, 0usize);
    let (mut trees, mut leaves_total, mut complement, mut duplicate, mut full_factor, mut pair_factor, mut range_merge) = (0, 0, 0, 0, 0, 0, 0);
    for &n in &ids {
        let node = g.get(n);
        if !matches!(node.op, Op::And | Op::Or) || same_parent[n as usize] {
            continue;
        }
        let want_and = matches!(node.op, Op::And);
        let Some(leaves) = flatten(g, n, want_and, 256) else { continue };
        trees += 1;
        leaves_total += leaves.len();
        let set: BTreeSet<NodeId> = leaves.iter().copied().collect();
        if set.len() < leaves.len() {
            duplicate += 1;
            dup_removable += leaves.len() - set.len();
        }
        if leaves.iter().enumerate().any(|(i, x)| leaves[i + 1..].iter().any(|y| syntactic_complements(g, *x, *y))) {
            complement += 1;
        }
        // Comparisons of one operand against constants, conjoined; the same
        // direction (lower bound / upper bound) merges into one.
        if want_and {
            let mut per: BTreeMap<(NodeId, bool), BTreeSet<NodeId>> = BTreeMap::new();
            for l in set.iter() {
                if let Some(x) = is_cmp_const(*l) {
                    let nd = g.get(*l);
                    let const_right = nd.args[0] == x;
                    // A lower bound on x: `x > k`, `x >= k`, `k < x`, `k <= x`.
                    let lower = match nd.op {
                        Op::Gt | Op::Ge => const_right,
                        Op::Lt | Op::Le => !const_right,
                        _ => continue,
                    };
                    per.entry((x, lower)).or_default().insert(*l);
                }
            }
            if per.values().any(|c| c.len() >= 2) {
                range_merge += 1;
                range_removable += per.values().map(|c| c.len().saturating_sub(1)).sum::<usize>();
            }
        }
        // Common factors among the terms (the terms of an Or are And-trees).
        if leaves.len() >= 2 {
            let parts: Vec<BTreeSet<NodeId>> = leaves.iter().filter_map(|t| flatten(g, *t, !want_and, 256)).map(|v| v.into_iter().collect()).collect();
            if parts.len() == leaves.len() {
                let mut common = parts[0].clone();
                for p in &parts[1..] {
                    common = common.intersection(p).copied().collect();
                }
                if !common.is_empty() {
                    full_factor += 1;
                    factor_removable += (parts.len() - 1) * common.len();
                } else if parts.iter().enumerate().any(|(i, p)| parts[i + 1..].iter().any(|q| p.intersection(q).next().is_some())) {
                    pair_factor += 1;
                }
            }
        }
    }
    let _ = writeln!(
        out,
        "    boolean trees: {trees} maximal And/Or trees, {leaves_total} leaves; with a complementary pair {complement}, a duplicate leaf {duplicate} ({dup_removable} removable), a factor common to all terms {full_factor} ({factor_removable} repeated conjuncts), one shared by some pair {pair_factor}; And-trees with same-direction comparisons of one operand against constants {range_merge} ({range_removable} removable)"
    );
    // Select shapes.
    let (mut same_arms, mut nested_same_cond, mut threshold_equal_arm) = (0, 0, 0);
    for &n in &ids {
        let nd = g.get(n);
        if !matches!(nd.op, Op::Sel) {
            continue;
        }
        let (c, t, f) = (nd.args[0], nd.args[1], nd.args[2]);
        if t == f {
            same_arms += 1;
        }
        for arm in [t, f] {
            let an = g.get(arm);
            if matches!(an.op, Op::Sel) && an.args[0] == c {
                nested_same_cond += 1;
            }
        }
        let fnode = g.get(f);
        if matches!(fnode.op, Op::Sel) && fnode.args[1] == t && matches!(g.get(t).op, Op::Const(..)) {
            threshold_equal_arm += 1;
        }
        let _ = c;
    }
    let _ = writeln!(
        out,
        "    selects: identical arms {same_arms}, an arm selecting on the same condition {nested_same_cond}, `Sel(c1, k, Sel(c2, k, r))` with a constant k {threshold_equal_arm}"
    );
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};
    use std::collections::HashMap as Map;

    /// The local pass at the settings the lowering uses.
    fn local(g: &Graph, roots: &[NodeId]) -> (Graph, Vec<NodeId>, Stats) {
        simplify_local(g, roots, LOCAL_CAP, LOCAL_EXPAND)
    }

    use super::super::graph::Val;

    fn n(v: i16) -> Pico8Num {
        Pico8Num::from_i16(v)
    }

    #[test]
    fn the_merged_form_is_never_more_decided_than_its_ancestor() {
        // The property test the rewrite's safety rests on: for the
        // remerge shape, at EVERY tri-state assignment, wherever the
        // merged form `(A & p) | (A & not p)` is decided, `A` is
        // decided and agrees - so substituting `A` never turns a
        // decided lane undecided. Built over a pool of ancestors that
        // are themselves compound, not just atoms.
        let mut g = Graph::new();
        let cells: Vec<NodeId> = (0..3).map(|i| g.leaf(Op::Cell(i))).collect();
        let ancestors = vec![
            cells[0],
            g.fold(Op::And, vec![cells[0], cells[1]]),
            g.fold(Op::Or, vec![cells[1], cells[2]]),
        ];
        let ps = vec![cells[2], g.fold(Op::Not, vec![cells[0]])];
        let mut pairs: Vec<(NodeId, NodeId)> = Vec::new();
        for a in &ancestors {
            for p in &ps {
                let np = g.fold(Op::Not, vec![*p]);
                let l = g.fold(Op::And, vec![*a, *p]);
                let r = g.fold(Op::And, vec![*a, np]);
                let merged = g.fold(Op::Or, vec![l, r]);
                pairs.push((merged, *a));
            }
        }
        let tri = [Some(true), Some(false), None];
        for b0 in tri {
            for b1 in tri {
                for b2 in tri {
                    let cells = Map::from([
                        (0u32, Val::Bool(b0)),
                        (1u32, Val::Bool(b1)),
                        (2u32, Val::Bool(b2)),
                    ]);
                    let out = g.eval(&cells).unwrap();
                    for (merged, anc) in &pairs {
                        let (m, a) = (out[*merged as usize], out[*anc as usize]);
                        match (m, a) {
                            // Merged undecided: substituting A only
                            // ever ADDS decidedness. Fine.
                            (Val::Bool(None), _) => {}
                            // Merged decided: A must be decided the
                            // same way, or the substitution would
                            // change a decided lane.
                            (Val::Bool(Some(x)), Val::Bool(Some(y))) => {
                                assert_eq!(x, y, "decided disagreement at {:?}", cells)
                            }
                            (m, a) => panic!(
                                "merged {:?} more decided than ancestor {:?} at {:?}",
                                m, a, cells
                            ),
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn complementary_comparisons_are_one_variable_and_its_negation() {
        // `fold` rewrites `Not(Lt(x,y))` to `Ge(x,y)`, which is a good
        // rewrite and which - if the BDD treated every comparison as its
        // own variable - would hide the complementarity that the `Not`
        // made obvious. So normalization would have blinded the analysis
        // it was meant to feed. `x < y or x >= y` is the test.
        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        let lt = g.fold(Op::Lt, vec![x, y]);
        let ge = g.fold(Op::Ge, vec![x, y]);
        assert_ne!(lt, ge, "two nodes, or this proves nothing");
        let either = g.fold(Op::Or, vec![lt, ge]);
        let both = g.fold(Op::And, vec![lt, ge]);
        let (out, map, _) = local(&g, &[either, both]);
        assert_eq!(out.get(map[either as usize]).op, Op::ConstBool(true));
        assert_eq!(out.get(map[both as usize]).op, Op::ConstBool(false));
        // The same for the other pair, spelled the other way round.
        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        let gt = g.fold(Op::Gt, vec![x, y]);
        let le = g.fold(Op::Le, vec![x, y]);
        let either = g.fold(Op::Or, vec![gt, le]);
        let (out, map, _) = local(&g, &[either]);
        assert_eq!(out.get(map[either as usize]).op, Op::ConstBool(true));
    }

    /// The same gate for the local pass.
    #[test]
    fn simplifying_locally_preserves_what_the_graph_evaluates_to() {
        check_preserves(local);
    }

    /// The local pass decides what the global one did - contradiction,
    /// tautology, the re-merge - and through a SWAPPED comparison
    /// (`Gt(y, x)` is `Lt(x, y)`), which the global analysis's variables
    /// never see.
    #[test]
    fn local_decides_tautologies_remerges_and_swapped_comparisons() {
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        let na = g.fold(Op::Not, vec![a]);
        let contradiction = g.fold(Op::And, vec![a, na]);
        let tautology = g.fold(Op::Or, vec![a, na]);
        let l = g.fold(Op::And, vec![a, x]);
        let r = g.fold(Op::And, vec![na, x]);
        let rejoined = g.fold(Op::Or, vec![l, r]);
        let (u, v) = (g.leaf(Op::Cell(10)), g.leaf(Op::Cell(11)));
        let lt = g.fold(Op::Lt, vec![u, v]);
        let le_swapped = g.fold(Op::Le, vec![v, u]);
        let swapped_either = g.fold(Op::Or, vec![lt, le_swapped]);
        let gt_swapped = g.fold(Op::Gt, vec![v, u]);
        let same = g.fold(Op::And, vec![lt, gt_swapped]);
        let roots = [contradiction, tautology, rejoined, swapped_either, same];
        let (out, map, st) = local(&g, &roots);
        let m: Vec<NodeId> = roots.iter().map(|r| map[*r as usize]).collect();
        assert_eq!(st.unanalysed, 0);
        assert_eq!(out.get(m[0]).op, Op::ConstBool(false));
        assert_eq!(out.get(m[1]).op, Op::ConstBool(true));
        assert_eq!(out.get(m[2]).op, Op::Cell(2));
        assert_eq!(out.get(m[3]).op, Op::ConstBool(true), "`u < v or v <= u`");
        assert_eq!(out.get(m[4]).op, Op::Lt, "`u < v and v > u` is `u < v`");
    }

    /// Three levels of re-merge over a compound ancestor collapse to it, one
    /// merge per level, in ONE pass.
    #[test]
    fn local_collapses_a_three_level_remerge_in_one_pass() {
        let mut g = Graph::new();
        let a0 = g.leaf(Op::Cell(1));
        let a1 = g.leaf(Op::Cell(4));
        let a = g.fold(Op::And, vec![a0, a1]);
        let p = g.leaf(Op::Cell(2));
        let (x, y) = (g.leaf(Op::Cell(10)), g.leaf(Op::Cell(11)));
        let np = g.fold(Op::Not, vec![p]);
        let l1 = {
            let l = g.fold(Op::And, vec![a, p]);
            let r = g.fold(Op::And, vec![a, np]);
            // `add`, not `fold`: `Graph::fold` folds a remerge over one
            // condition itself now, and this pass is what is under test.
            g.add(Op::Or, vec![l, r])
        };
        let gt = g.fold(Op::Gt, vec![x, y]);
        let le = g.fold(Op::Le, vec![x, y]);
        let l2 = {
            let l = g.fold(Op::And, vec![l1, gt]);
            let r = g.fold(Op::And, vec![l1, le]);
            g.add(Op::Or, vec![l, r])
        };
        let q = g.leaf(Op::Cell(3));
        let nq = g.fold(Op::Not, vec![q]);
        let l3 = {
            let l = g.fold(Op::And, vec![l2, q]);
            let r = g.fold(Op::And, vec![l2, nq]);
            g.add(Op::Or, vec![l, r])
        };
        let (out, map, st) = local(&g, &[l3]);
        assert_eq!(map[l3 as usize], map[a as usize], "the whole chain is A");
        assert_eq!(out.get(map[l3 as usize]).op, Op::And);
        // The pass rebuilds through `Graph::fold`, which folds each level's
        // remerge itself now (`Graph::complements`), so nothing is left to count
        // as a BDD merge: what remains is A's three nodes, in one pass.
        assert_eq!(st.after, 3, "the chain is A's three nodes: {:?}", st);
    }

    /// What `simplify_until_stable` needed a second pass for, the local pass
    /// sees in one: the select collapses, the two comparisons become one
    /// node, and the formula over them is then a tautology.
    #[test]
    fn local_sees_a_collapse_it_created_in_the_same_pass() {
        let mut g = Graph::new();
        let (x, y, z) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)), g.leaf(Op::Cell(3)));
        let b = g.leaf(Op::Cell(4));
        let nb = g.fold(Op::Not, vec![b]);
        let c = g.fold(Op::Or, vec![b, nb]);
        let sel = g.fold(Op::Sel, vec![c, x, y]);
        let lt1 = g.fold(Op::Lt, vec![sel, z]);
        let lt2 = g.fold(Op::Lt, vec![x, z]);
        let nlt2 = g.fold(Op::Not, vec![lt2]);
        let same = g.fold(Op::Or, vec![lt1, nlt2]);
        let (out, map, _) = local(&g, &[same]);
        assert_eq!(out.get(map[same as usize]).op, Op::ConstBool(true));
    }

    /// A leaf repeated across a nested `And` tree is dropped, and the
    /// tree is rebuilt over its distinct leaves.
    #[test]
    fn local_drops_duplicate_leaves() {
        let mut g = Graph::new();
        let (a, b, c) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)), g.leaf(Op::Cell(3)));
        let ba = g.fold(Op::And, vec![b, a]);
        let cba = g.fold(Op::And, vec![c, ba]);
        let t = g.fold(Op::And, vec![a, cba]);
        let (out, map, st) = local(&g, &[t]);
        assert_eq!(st.deduped, 1, "{:?}", st);
        let leaves = flatten(&out, map[t as usize], true, 64).expect("small");
        let mut sorted = leaves.clone();
        sorted.sort_unstable();
        assert_eq!(sorted, vec![a, b, c], "each leaf once: {leaves:?}");
    }

    /// `x < 3 and x > 5`: two independent atoms, not folded - the
    /// incompleteness that makes the conclusions hold.
    #[test]
    fn local_treats_different_comparisons_as_independent() {
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(1));
        let three = g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32));
        let five = g.leaf(Op::Const(n(5).as_raw_u32() as i32, n(5).as_raw_u32() as i32));
        let lo = g.fold(Op::Lt, vec![x, three]);
        let hi = g.fold(Op::Gt, vec![x, five]);
        let both = g.fold(Op::And, vec![lo, hi]);
        let (out, map, _) = local(&g, &[both]);
        assert_eq!(out.get(map[both as usize]).op, Op::And);
    }

    /// Build a pile of boolean algebra over atoms, simplify, and check the
    /// two graphs agree at every tri-state assignment - which is a stronger
    /// demand than the concrete one, and the one that would catch a
    /// mis-ordered `ite`. The one legitimate difference is a node DECIDED
    /// where the original was unknown.
    fn check_preserves(simplifier: impl Fn(&Graph, &[NodeId]) -> (Graph, Vec<NodeId>, Stats)) {
        let mut g = Graph::new();
        let cells: Vec<NodeId> = (0..4).map(|i| g.leaf(Op::Cell(i))).collect();
        let nums: Vec<NodeId> = (10..12).map(|i| g.leaf(Op::Cell(i))).collect();
        let mut pool = cells.clone();
        pool.push(g.fold(Op::Lt, vec![nums[0], nums[1]]));
        // The COMPLEMENT of the one above, so the differential check
        // covers the one-variable-and-its-negation path - which is the
        // riskiest thing in `analyze`, since getting the polarity
        // backwards would still typecheck and still produce a graph.
        pool.push(g.fold(Op::Ge, vec![nums[0], nums[1]]));
        pool.push(g.fold(Op::Le, vec![nums[1], nums[0]]));
        // The swapped forms, which the local pass maps onto one variable.
        pool.push(g.fold(Op::Gt, vec![nums[1], nums[0]]));
        pool.push(g.fold(Op::Le, vec![nums[0], nums[1]]));
        pool.push(g.leaf(Op::ConstBool(true)));
        let mut rng: u64 = 12345;
        let mut roots = Vec::new();
        for _ in 0..300 {
            let mut pick = || {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
                (rng >> 33) as usize
            };
            let (i, j, k) = (pick(), pick(), pick());
            let (p, q) = (pool[i % pool.len()], pool[j % pool.len()]);
            let node = match k % 4 {
                0 => g.fold(Op::And, vec![p, q]),
                1 => g.fold(Op::Or, vec![p, q]),
                2 => g.fold(Op::Not, vec![p]),
                _ => g.fold(Op::Sel, vec![pool[k % pool.len()], p, q]),
            };
            pool.push(node);
            roots.push(node);
        }
        // A numeric select on top, so the numeric layer is exercised too.
        let numsel = g.fold(Op::Sel, vec![*roots.last().unwrap(), nums[0], nums[1]]);
        roots.push(numsel);

        let (out, nodemap, st) = simplifier(&g, &roots);
        let mapped: Vec<NodeId> = roots.iter().map(|r| nodemap[*r as usize]).collect();
        assert_eq!(st.unanalysed, 0, "{:?}", st);
        assert!(
            st.constants + st.to_atom > 0,
            "nothing was simplified: {:?}",
            st
        );

        let tri = [Some(true), Some(false), None];
        let ivals = [
            Pico8NumInterval::from_number(n(0)),
            Pico8NumInterval::new(n(0), n(2)),
        ];
        for a in tri {
            for b in tri {
                for c in tri {
                    for d in tri {
                        for i0 in ivals {
                            for i1 in ivals {
                                let cells = Map::from([
                                    (0u32, Val::Bool(a)),
                                    (1u32, Val::Bool(b)),
                                    (2u32, Val::Bool(c)),
                                    (3u32, Val::Bool(d)),
                                    (10u32, Val::Num(i0)),
                                    (11u32, Val::Num(i1)),
                                ]);
                                let before = g.eval(&cells).unwrap();
                                let after = out.eval(&cells).unwrap();
                                for (r, m) in roots.iter().zip(mapped.iter()) {
                                    let (x, y) =
                                        (before[*r as usize], after[*m as usize]);
                                    if x == y {
                                        continue;
                                    }
                                    // The one legitimate difference: the
                                    // BDD knows things Kleene does not, so
                                    // the simplified node may be DECIDED
                                    // where the original was unknown. The
                                    // reverse, or a decided disagreement,
                                    // is a bug.
                                    match (x, y) {
                                        (Val::Bool(None), Val::Bool(_)) => {}
                                        _ => panic!(
                                            "node {} changed from {:?} to {:?}",
                                            r, x, y
                                        ),
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn the_cap_is_reported_rather_than_silent() {
        // A truncated analysis that finds nothing reads exactly like a
        // complete one that finds nothing, so the count is the whole
        // safety property here.
        let mut g = Graph::new();
        let cells: Vec<NodeId> = (0..40).map(|i| g.leaf(Op::Cell(i))).collect();
        // Parity over 40 independent atoms - the textbook BDD blow-up
        // under a bad variable order, and a good way to hit a small cap.
        let mut acc = g.leaf(Op::ConstBool(false));
        for c in &cells {
            let na = g.fold(Op::Not, vec![acc]);
            let nc = g.fold(Op::Not, vec![*c]);
            let l = g.fold(Op::And, vec![acc, nc]);
            let r = g.fold(Op::And, vec![na, *c]);
            acc = g.fold(Op::Or, vec![l, r]);
        }
        // Parity's BDD is 2 nodes per variable, and 64 expanded compounds
        // reach ~16 variables: a 16-node table cannot hold it.
        let (_, _, st) = simplify_local(&g, &[acc], 16, 64);
        assert!(st.unanalysed > 0, "a 16-node table should not survive this: {st:?}");
        // And it still produced a graph rather than panicking.
        assert!(st.after > 0);
    }
}
