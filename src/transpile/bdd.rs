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

use std::collections::HashMap;

use super::graph::{Graph, NodeId, Op};

/// A reference into the BDD's node table. 0 and 1 are the terminals.
pub type Ref = u32;
pub const FALSE: Ref = 0;
pub const TRUE: Ref = 1;

/// The image of a node `simplify` did not rebuild. Deliberately not 0:
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
    intern: HashMap<Triple, Ref>,
    memo: HashMap<(Ref, Ref, Ref), Ref>,
    /// The budget. An ROBDD can be exponential in its variable count, and
    /// a guard chain over hundreds of atoms is exactly the shape that
    /// blows up, so the cap is not decoration.
    cap: usize,
    /// Whether the cap was ever hit. Reported, never silent: a truncated
    /// analysis that says "no simplifications" reads identically to a
    /// complete one that found nothing.
    pub overflowed: bool,
}

impl Bdd {
    pub fn new(cap: usize) -> Self {
        let dummy = Triple { var: u32::MAX, lo: 0, hi: 0 };
        Bdd {
            nodes: vec![dummy, dummy],
            intern: HashMap::new(),
            memo: HashMap::new(),
            cap,
            overflowed: false,
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
            self.overflowed = true;
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
}

/// What `analyze` learned about one graph.
pub struct Analysis {
    /// Per graph node: its boolean function, where it has one. `None` is
    /// "not a boolean node, or the cap was hit building it". An ATOM's
    /// function is its own variable, so atoms appear here too - which is
    /// what lets `(a and x) or (not a and x)` be recognized as `x`.
    pub of: Vec<Option<Ref>>,
    /// For each variable, the graph node it stands for. `simplify` will
    /// only ever substitute one of these, or a constant.
    pub atom_of: HashMap<Ref, NodeId>,
    pub atoms: usize,
    pub overflowed: bool,
    pub bdd_nodes: usize,
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
            | Op::SplitOk
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

/// Build every boolean node's function. One forward pass: a node's
/// operands always have smaller ids, so their references already exist.
///
/// `need` bounds the work to the nodes that matter. That is not just an
/// optimization: the BDD has a node budget, and spending it on parts of
/// the graph nobody asked about is how an analysis reports "capped" on a
/// fifty-node question - which is exactly what it did the first time.
pub fn analyze(g: &Graph, need: &[bool], cap: usize) -> (Bdd, Analysis) {
    let mut b = Bdd::new(cap);
    let mut of: Vec<Option<Ref>> = vec![None; g.len()];
    // Atoms are minted ON DEMAND, by the first boolean node that uses
    // one. Minting eagerly for every node would spend variable-order
    // slots on the numeric layer, which never appears in a formula.
    let mut atom_of: HashMap<Ref, NodeId> = HashMap::new();
    let mut n_atoms: u32 = 0;
    // Two comparisons over the same operands can be exact COMPLEMENTS,
    // and then they are one variable and its negation rather than two
    // independent ones. This is not an optional refinement: `fold`
    // rewrites `Not(Lt(x,y))` to `Ge(x,y)`, so normalization itself is
    // what turns a negated atom into a second atom, and without this the
    // analysis would be blinded by a rewrite meant to help it.
    let mut cmp: HashMap<(Op, Vec<NodeId>), Ref> = HashMap::new();

    for id in 0..g.len() {
        if !need[id] {
            continue;
        }
        let node = g.get(id as NodeId);
        // The operand's function, or an atom standing for it.
        macro_rules! r {
            ($i:expr) => {{
                let a = node.args[$i] as usize;
                match of[a] {
                    Some(x) => Some(x),
                    None => {
                        let an = g.get(a as NodeId);
                        // The complementary comparison, if this is one.
                        let pair = match an.op {
                            Op::Lt => Some((Op::Lt, false)),
                            Op::Ge => Some((Op::Lt, true)),
                            Op::Le => Some((Op::Le, false)),
                            Op::Gt => Some((Op::Le, true)),
                            _ => None,
                        };
                        let v = match pair {
                            Some((canon, negated)) => {
                                let key = (canon, an.args.clone());
                                match cmp.get(&key).copied() {
                                    Some(base) => {
                                        if negated {
                                            b.not(base)
                                        } else {
                                            Some(base)
                                        }
                                    }
                                    None => {
                                        // Mint the POSITIVE form's
                                        // variable, whichever way round
                                        // the graph happened to spell it.
                                        let base = b.var(n_atoms);
                                        if let Some(base) = base {
                                            n_atoms += 1;
                                            cmp.insert(key, base);
                                        }
                                        match (base, negated) {
                                            (Some(base), true) => b.not(base),
                                            (x, _) => x,
                                        }
                                    }
                                }
                            }
                            None => {
                                let v = b.var(n_atoms);
                                if v.is_some() {
                                    n_atoms += 1;
                                }
                                v
                            }
                        };
                        if let Some(v) = v {
                            of[a] = Some(v);
                            // Only a node whose function IS a bare
                            // variable can stand in for that variable. A
                            // negated comparison is `not v`, so it must
                            // not be recorded as v's representative.
                            atom_of.entry(v).or_insert(a as NodeId);
                        }
                        v
                    }
                }
            }};
        }
        let f = match node.op {
            Op::ConstBool(v) => Some(if v { TRUE } else { FALSE }),
            Op::Not => match r!(0) {
                Some(x) => b.not(x),
                None => None,
            },
            Op::And => match (r!(0), r!(1)) {
                (Some(x), Some(y)) => b.and(x, y),
                _ => None,
            },
            Op::Or => match (r!(0), r!(1)) {
                (Some(x), Some(y)) => b.or(x, y),
                _ => None,
            },
            // A `Sel` is boolean when its ARMS are, and the test has to
            // be the ARM'S OWN OP rather than "does it already have a
            // reference": an arm picks up a reference the moment anything
            // mints an atom for it, and a numeric select misread as a
            // boolean one would produce nonsense. Conservative in the
            // right direction - a select between two boolean input cells
            // is missed and becomes an atom.
            Op::Sel
                if inherently_bool(&g.get(node.args[1]).op)
                    || inherently_bool(&g.get(node.args[2]).op) =>
            {
                match (r!(0), r!(1), r!(2)) {
                    (Some(c), Some(t), Some(f)) => b.ite(c, t, f),
                    _ => None,
                }
            }
            _ => None,
        };
        // Not `of[id] = match ...` directly: `r!` writes to `of` when it
        // mints an atom, so the two would overlap.
        if f.is_some() {
            of[id] = f;
        }
    }
    let overflowed = b.overflowed;
    let bdd_nodes = b.len();
    (b, Analysis { of, atom_of, atoms: n_atoms as usize, overflowed, bdd_nodes })
}

/// What `simplify` did, for reporting. Every field exists because a
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
    /// Boolean nodes proved equal to an earlier NON-atom node - counted,
    /// and deliberately NOT applied. See the note on `simplify`.
    pub mergeable: usize,
    pub atoms: usize,
    pub bdd_nodes: usize,
    pub overflowed: bool,
}

/// Rebuild `g` with every boolean node the BDD proves to be a CONSTANT or
/// an ATOM replaced by it. Returns the new graph and the old -> new node
/// map, whose entries are `UNREACHABLE` outside `roots`' reachable set -
/// a value that panics on use rather than silently naming node 0.
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
///   more precise.
///
/// Anything else is counted as `mergeable` and left alone. That is not a
/// permanent refusal - it is a refusal to guess, since the measurement
/// that would settle it (does deopt volume actually rise?) needs the
/// traced path wired into the search, which it is not yet.
pub fn simplify(g: &Graph, roots: &[NodeId], cap: usize) -> (Graph, Vec<NodeId>, Stats) {
    let need = reachable(g, roots);
    let (_, a) = analyze(g, &need, cap);
    let mut out = Graph::new();
    let mut map: Vec<NodeId> = vec![UNREACHABLE; g.len()];
    let mut seen: HashMap<Ref, NodeId> = HashMap::new();
    let mut st = Stats {
        before: need.iter().filter(|x| **x).count(),
        atoms: a.atoms,
        bdd_nodes: a.bdd_nodes,
        overflowed: a.overflowed,
        ..Default::default()
    };

    for id in 0..g.len() {
        if !need[id] {
            continue;
        }
        let node = g.get(id as NodeId);
        let rebuild = |out: &mut Graph, map: &Vec<NodeId>| {
            let args: Vec<NodeId> = node.args.iter().map(|x| map[*x as usize]).collect();
            out.fold(node.op.clone(), args)
        };
        let new = match a.of[id] {
            Some(r) if Bdd::is_terminal(r) && !matches!(node.op, Op::ConstBool(_)) => {
                st.constants += 1;
                out.leaf(Op::ConstBool(r == TRUE))
            }
            Some(r) if !Bdd::is_terminal(r) => match a.atom_of.get(&r) {
                Some(at) if *at != id as NodeId => {
                    st.to_atom += 1;
                    map[*at as usize]
                }
                Some(_) => rebuild(&mut out, &map),
                None => {
                    if seen.insert(r, id as NodeId).is_some() {
                        st.mergeable += 1;
                    }
                    rebuild(&mut out, &map)
                }
            },
            _ => rebuild(&mut out, &map),
        };
        map[id] = new;
    }
    st.after = out.len();
    (out, map, st)
}

/// `simplify` until it stops finding anything, composing the node maps.
///
/// One pass is not obviously a fixed point, and the reason is the ATOMS.
/// An atom is an opaque node, and comparisons are atoms - so
/// `Lt(Sel(c, x, y), z)` and `Lt(x, z)` are two INDEPENDENT variables
/// even though they may be the same comparison. If a pass proves `c`
/// constant, the select collapses and the first comparison becomes the
/// second, literally: one node, one atom. The next pass then has
/// relational information the previous one could not have had.
///
/// So iterating is not "run it again in case", it is following a
/// specific mechanism. Whether that mechanism fires on any given program
/// is a measurement; `passes` returns one `Stats` per pass so the answer
/// is visible rather than assumed.
pub fn simplify_until_stable(
    g: &Graph,
    roots: &[NodeId],
    cap: usize,
    max_passes: usize,
) -> (Graph, Vec<NodeId>, Vec<Stats>) {
    let mut cur = g.clone();
    let mut cur_roots: Vec<NodeId> = roots.to_vec();
    let mut composed: Vec<NodeId> = (0..g.len() as NodeId).collect();
    let mut all = Vec::new();
    for _ in 0..max_passes {
        let (next, map, st) = simplify(&cur, &cur_roots, cap);
        let progress = st.constants + st.to_atom;
        all.push(st);
        for c in composed.iter_mut() {
            *c = if *c == UNREACHABLE { UNREACHABLE } else { map[*c as usize] };
        }
        cur_roots = cur_roots.iter().map(|r| map[*r as usize]).collect();
        cur = next;
        if progress == 0 {
            break;
        }
    }
    (cur, composed, all)
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::{Pico8Num, Pico8NumInterval};
    use std::collections::HashMap as Map;

    use super::super::graph::Val;

    const CAP: usize = 1 << 20;

    fn n(v: i16) -> Pico8Num {
        Pico8Num::from_i16(v)
    }

    #[test]
    fn it_decides_the_tautologies_that_pattern_rules_missed() {
        // `fold` deliberately does NOT do these: in Kleene, `a and not a`
        // is unknown when a is, so folding it to false is a refinement
        // rather than a rewrite of syntax. The BDD does not work in
        // Kleene - it works over the concrete function, where the two
        // occurrences of `a` are the SAME a - so it decides them.
        let mut g = Graph::new();
        let a = g.leaf(Op::Cell(1));
        let x = g.leaf(Op::Cell(2));
        let na = g.fold(Op::Not, vec![a]);
        let contradiction = g.fold(Op::And, vec![a, na]);
        let tautology = g.fold(Op::Or, vec![a, na]);
        // The MERGE shape: a split gave `g and c` and `g and not c`, and
        // joining them is `g` again.
        let l = g.fold(Op::And, vec![a, x]);
        let r = g.fold(Op::And, vec![na, x]);
        let rejoined = g.fold(Op::Or, vec![l, r]);

        let (out, map, st) = simplify(&g, &[contradiction, tautology, rejoined], CAP);
        let roots: Vec<NodeId> = [contradiction, tautology, rejoined]
            .iter()
            .map(|r| map[*r as usize])
            .collect();
        assert!(!st.overflowed);
        assert_eq!(out.get(roots[0]).op, Op::ConstBool(false));
        assert_eq!(out.get(roots[1]).op, Op::ConstBool(true));
        // `x` is an atom, so it survives as itself.
        assert_eq!(out.get(roots[2]).op, Op::Cell(2));
    }

    #[test]
    fn an_equality_it_can_prove_but_will_not_act_on_is_counted() {
        // De Morgan's law between two nodes that are neither constant nor
        // an atom. The BDD proves them equal; `simplify` COUNTS that and
        // leaves them alone, because picking one of two forms of a
        // function changes how precisely the Kleene evaluation
        // approximates it, and the measurement that would say whether
        // that matters does not exist yet.
        let mut g = Graph::new();
        let (a, b) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        let (na, nb) = (g.fold(Op::Not, vec![a]), g.fold(Op::Not, vec![b]));
        let inner = g.fold(Op::And, vec![na, nb]);
        let lhs = g.fold(Op::Not, vec![inner]);
        let rhs = g.fold(Op::Or, vec![a, b]);
        assert_ne!(lhs, rhs, "structurally different, or this proves nothing");
        let (_, map, st) = simplify(&g, &[lhs, rhs], CAP);
        assert_eq!(st.mergeable, 1, "the equality was proved");
        assert_ne!(map[lhs as usize], map[rhs as usize], "and deliberately not acted on");
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
        let (out, map, _) = simplify(&g, &[either, both], CAP);
        assert_eq!(out.get(map[either as usize]).op, Op::ConstBool(true));
        assert_eq!(out.get(map[both as usize]).op, Op::ConstBool(false));
        // The same for the other pair, spelled the other way round.
        let mut g = Graph::new();
        let (x, y) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)));
        let gt = g.fold(Op::Gt, vec![x, y]);
        let le = g.fold(Op::Le, vec![x, y]);
        let either = g.fold(Op::Or, vec![gt, le]);
        let (out, map, _) = simplify(&g, &[either], CAP);
        assert_eq!(out.get(map[either as usize]).op, Op::ConstBool(true));
    }

    #[test]
    fn independent_atoms_are_why_it_is_sound_and_why_it_misses() {
        // `x < 3 and x > 5` is unsatisfiable, and this analysis does NOT
        // fold it: the two comparisons are separate atoms. That is the
        // deliberate incompleteness - it is what buys the guarantee that
        // everything it DOES conclude holds for the realizable
        // assignments, since those are a subset of all of them.
        let mut g = Graph::new();
        let x = g.leaf(Op::Cell(1));
        let three = g.leaf(Op::Const(n(3).as_raw_u32() as i32, n(3).as_raw_u32() as i32));
        let five = g.leaf(Op::Const(n(5).as_raw_u32() as i32, n(5).as_raw_u32() as i32));
        let lo = g.fold(Op::Lt, vec![x, three]);
        let hi = g.fold(Op::Gt, vec![x, five]);
        let both = g.fold(Op::And, vec![lo, hi]);
        let (out, map, _) = simplify(&g, &[both], CAP);
        assert_eq!(out.get(map[both as usize]).op, Op::And, "not folded, and that is correct");
    }

    #[test]
    fn simplifying_preserves_what_the_graph_evaluates_to() {
        // The gate. Build a pile of boolean algebra over atoms, simplify,
        // and check the two graphs agree at every tri-state assignment -
        // which is a stronger demand than the concrete one, and the one
        // that would catch a mis-ordered `ite`.
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

        let (out, nodemap, st) = simplify(&g, &roots, CAP);
        let mapped: Vec<NodeId> = roots.iter().map(|r| nodemap[*r as usize]).collect();
        assert!(!st.overflowed);
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
    fn a_second_pass_can_see_what_the_first_could_not() {
        // The mechanism iteration exists for. `Lt(Sel(c, x, y), z)` and
        // `Lt(x, z)` are two atoms while the select stands, so a formula
        // over both looks contingent. Prove `c` true, the select
        // collapses to `x`, the two comparisons become ONE node - and
        // only then is `Lt(..) == Lt(x, z)` visible as a tautology.
        let mut g = Graph::new();
        let (x, y, z) = (g.leaf(Op::Cell(1)), g.leaf(Op::Cell(2)), g.leaf(Op::Cell(3)));
        let b = g.leaf(Op::Cell(4));
        // `c` is a tautology the FIRST pass can decide, but which is not
        // syntactically constant.
        let nb = g.fold(Op::Not, vec![b]);
        let c = g.fold(Op::Or, vec![b, nb]);
        let sel = g.fold(Op::Sel, vec![c, x, y]);
        let lt1 = g.fold(Op::Lt, vec![sel, z]);
        let lt2 = g.fold(Op::Lt, vec![x, z]);
        assert_ne!(lt1, lt2, "two atoms, or this proves nothing");
        let nlt2 = g.fold(Op::Not, vec![lt2]);
        let same = g.fold(Op::Or, vec![lt1, nlt2]);

        // One pass: the select goes, but the equality was invisible while
        // it stood.
        let (one, map1, _) = simplify(&g, &[same], CAP);
        let after_one = one.get(map1[same as usize]).op.clone();

        let (out, map, passes) = simplify_until_stable(&g, &[same], CAP, 4);
        assert!(passes.len() >= 2, "it should have taken a second look");
        assert_eq!(
            out.get(map[same as usize]).op,
            Op::ConstBool(true),
            "after one pass it was {:?}",
            after_one
        );
        assert_eq!(
            passes.last().unwrap().constants + passes.last().unwrap().to_atom,
            0,
            "the last pass is the one that found nothing"
        );
    }

    #[test]
    fn the_cap_is_reported_rather_than_silent() {
        // A truncated analysis that finds nothing reads exactly like a
        // complete one that finds nothing, so the flag is the whole
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
        let (_, _, st) = simplify(&g, &[acc], 64);
        assert!(st.overflowed, "a 64-node cap should not survive this");
        // And it still produced a graph rather than panicking.
        assert!(st.after > 0);
    }
}
