//! THE SPLIT PASS (`split_undecided_selects`): a traced frame made
//! executable under abstract inputs. Errors are settled by case analysis
//! (`MayErr`), guards rewritten three-valued (`three_valued`).

use anyhow::Result;

use super::domain::{Domain, Symbolic};
use super::frame::FrameOut;
use crate::transpile::garith::GraphArith;
use crate::transpile::graph::NodeId;

/// An outcome's row as `Waiting` indexes it: its shape class and its fields.
type RowKey = (usize, Vec<NodeId>);

/// `split_undecided_selects`' queue: largest cone first, deduped on arrival.
#[derive(Default)]
struct Waiting {
    slots: Vec<Option<Lite>>,
    index: rustc_hash::FxHashMap<RowKey, usize>,
    heap: std::collections::BinaryHeap<(usize, std::cmp::Reverse<usize>)>,
}

/// An outcome in flight: only what splits changed over its template `t`, so
/// no state or block is cloned per side.
#[derive(Clone)]
struct Lite {
    t: usize,
    guard: NodeId,
    error: NodeId,
    fields: Vec<NodeId>,
    /// Lane-decided conditions a case split may use to merge this outcome
    /// into existing ones (`absorbed`).
    rests: Vec<NodeId>,
}

impl Lite {
    fn row(&self) -> Vec<NodeId> {
        self.fields.clone()
    }

    fn every(&self) -> Vec<NodeId> {
        let mut r = self.row();
        r.extend([self.error, self.guard]);
        r
    }

    /// Absorb another with the same row: live where either is; error
    /// `(g1 and e1) or (g2 and e2)`.
    fn merge<A: GraphArith>(&mut self, d: &mut Symbolic<A>, guard: NodeId, error: NodeId, rests: &[NodeId]) {
        for r in rests {
            if !self.rests.contains(r) && self.rests.len() < MAX_RESTS {
                self.rests.push(*r);
            }
        }
        if self.error != error {
            let (a, b) = (d.and(&self.guard, &self.error), d.and(&guard, &error));
            self.error = d.or(&a, &b);
        }
        self.guard = d.or(&self.guard, &guard);
    }
}

impl Waiting {
    fn key(class: &[usize], o: &Lite) -> RowKey {
        (class[o.t], o.fields.clone())
    }

    fn push<A: GraphArith>(&mut self, d: &mut Symbolic<A>, class: &[usize], o: Lite, size: usize) {
        let key = Self::key(class, &o);
        if let Some(&i) = self.index.get(&key) {
            let p = self.slots[i].as_mut().expect("an indexed outcome is waiting");
            p.merge(d, o.guard, o.error, &o.rests);
            return;
        }
        let i = self.slots.len();
        self.slots.push(Some(o));
        self.index.insert(key, i);
        self.heap.push((size, std::cmp::Reverse(i)));
    }

    fn pop(&mut self, class: &[usize]) -> Option<Lite> {
        let (_, std::cmp::Reverse(i)) = self.heap.pop()?;
        let out = self.slots[i].take().expect("a queued outcome is waiting");
        self.index.remove(&Self::key(class, &out));
        Some(out)
    }

    fn len(&self) -> usize {
        self.index.len()
    }
}

/// An outcome's error once its row is resolved: WHERE A LANE MAY ERR, with no
/// undecided select left. By case analysis on the error alone (an outcome's
/// split sides would double per round), not three-valued (a hull loses what
/// a select's condition says).
fn settle_error<A: GraphArith>(d: &mut Symbolic<A>, mut o: Lite) -> Lite {
    let mut m = MayErr { memo: Default::default(), cases: 0 };
    o.error = m.may(d, o.error).0;
    o
}

/// Where a split of condition `c` cuts: the lowest comparison (or unknown
/// atom) under it a lane can hold both ways; `c` itself if none.
fn split_atom<A: GraphArith>(d: &Symbolic<A>, c: NodeId) -> NodeId {
    use crate::transpile::graph::Op;
    d.graph
        .cone(&[c])
        .into_iter()
        .find(|n| matches!(d.graph.get(*n).op, Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::UnknownBool(_)) && d.lane_undecidable(*n))
        .unwrap_or(c)
}

/// `MayErr::may`'s bound on the cases it opens under one error.
const MAY_CASES: usize = 4096;

/// A boolean's `(may be true, may be false)` with no undecided select left:
/// connectives through operands (`and` over-approximates, safe for an
/// error); a node reading an undecided select is split on its lowest
/// undecidable comparison, each case under that answer's `may` guard. Past
/// `MAY_CASES`, three-valued.
struct MayErr {
    memo: rustc_hash::FxHashMap<NodeId, (NodeId, NodeId)>,
    cases: usize,
}

impl MayErr {
    fn may<A: GraphArith>(&mut self, d: &mut Symbolic<A>, n: NodeId) -> (NodeId, NodeId) {
        use crate::transpile::graph::Op;
        if let Some(hit) = self.memo.get(&n) {
            return *hit;
        }
        let (op, args) = {
            let node = d.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        let reads_undecided = |d: &mut Symbolic<A>, n: NodeId| d.graph.cone(&[n]).into_iter().find(|m| d.graph.get(*m).op == Op::Sel && d.lane_undecidable(d.graph.get(*m).args[0]));
        let out = match op {
            Op::Or | Op::And => {
                let parts: Vec<(NodeId, NodeId)> = args.iter().map(|a| self.may(d, *a)).collect();
                let (t, f): (Vec<NodeId>, Vec<NodeId>) = parts.into_iter().unzip();
                if op == Op::Or {
                    (d.graph.fold(Op::Or, t), d.graph.fold(Op::And, f))
                } else {
                    (d.graph.fold(Op::And, t), d.graph.fold(Op::Or, f))
                }
            }
            Op::Not => {
                let (t, f) = self.may(d, args[0]);
                (f, t)
            }
            _ => match reads_undecided(d, n) {
                None => {
                    let nn = d.graph.fold(Op::Not, vec![n]);
                    (n, nn)
                }
                Some(sel) if self.cases < MAY_CASES => {
                    self.cases += 1;
                    let atom = split_atom(d, d.graph.get(sel).args[0]);
                    let (may_true, may_false) = d.may_answers(atom);
                    let reach = d.graph.cone(&[n]);
                    let (mut t, mut f) = (d.graph.leaf(Op::ConstBool(false)), d.graph.leaf(Op::ConstBool(false)));
                    for answer in [true, false] {
                        let to = d.graph.leaf(Op::ConstBool(answer));
                        let map = rebuild_all(d, &reach, &rustc_hash::FxHashMap::from_iter([(atom, to)]));
                        let m = map.get(&n).copied().unwrap_or(n);
                        let (ct, cf) = self.may(d, m);
                        let g = if answer { may_true } else { may_false };
                        let (ct, cf) = (d.graph.fold(Op::And, vec![g, ct]), d.graph.fold(Op::And, vec![g, cf]));
                        t = d.graph.fold(Op::Or, vec![t, ct]);
                        f = d.graph.fold(Op::Or, vec![f, cf]);
                    }
                    (t, f)
                }
                Some(_) => {
                    let e = three_valued(d, n);
                    let ne = d.graph.fold(Op::Not, vec![e]);
                    (e, ne)
                }
            },
        };
        self.memo.insert(n, out);
        out
    }
}

/// `Symbolic::evaluated`'s conditions, rewritten by `three_valued`.
pub(super) fn three_valued_evaluated<A: GraphArith>(d: &mut Symbolic<A>, outs: &[FrameOut<A>]) {
    // The stored fields, guards and errors (not the transfer roots).
    let roots: Vec<NodeId> = outs.iter().flat_map(|o| o.fields.iter().map(|f| f.1).chain([o.guard, o.error])).collect();
    let mut done: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut todo = d.graph.cone(&roots);
    while !todo.is_empty() {
        let mut next: Vec<NodeId> = Vec::new();
        for n in todo {
            if !done.insert(n) {
                continue;
            }
            if let Some(at) = d.frame.evaluated.get(&n).copied() {
                let at2 = three_valued(d, at);
                if at2 != at {
                    d.frame.evaluated.insert(n, at2);
                }
                next.push(at2);
            }
        }
        todo = d.graph.cone(&next).into_iter().filter(|n| !done.contains(n)).collect();
    }
}

/// MAKING THE FRAME EXECUTABLE UNDER ABSTRACT INPUTS: a select whose
/// condition a lane holds both ways cannot be evaluated, so a select the row
/// STORES is split, the first in program order (the outcome in two, readers
/// refolded, guards narrowed by `may_answers`), until none is left; equal
/// outcomes fuse. The error is settled and the guard rewritten three-valued,
/// neither split.
pub(super) fn split_undecided_selects<A: GraphArith>(d: &mut Symbolic<A>, outs: Vec<FrameOut<A>>) -> Result<Vec<FrameOut<A>>> {
    use crate::transpile::graph::Op;
    const MAX_OUTCOMES: usize = 4096;
    // Templates of one shape share a class: their outcomes dedupe together.
    let class: Vec<usize> = (0..outs.len()).map(|i| (0..=i).find(|&j| outs[j].shape == outs[i].shape).expect("itself")).collect();
    // LARGEST CONE FIRST: a split only shrinks a cone, so every path into a
    // state merges before it is split.
    let mut work = Waiting::default();
    for (t, o) in outs.iter().enumerate() {
        let lite = Lite {
            t,
            guard: o.guard,
            error: o.error,
            // The transfer roots ride with the fields: same substitutions, and
            // part of what makes two outcomes one.
            fields: o.fields.iter().map(|f| f.1).chain(o.arc.iter().flat_map(|a| a.nodes())).collect(),
            rests: Vec::new(),
        };
        let size = d.graph.cone(&lite.row()).len();
        work.push(d, &class, lite, size);
    }
    let mut done: Vec<Lite> = Vec::new();
    while let Some(o) = work.pop(&class) {
        anyhow::ensure!(work.len() + done.len() < MAX_OUTCOMES, "more than {MAX_OUTCOMES} outcomes splitting undecided selects");
        // The outcome's own cone, in node order (the arena's is quadratic).
        let reach = d.graph.cone(&o.row());
        let first: Option<NodeId> = reach
            .iter()
            .filter(|n| d.graph.get(**n).op == Op::Sel)
            .map(|n| d.graph.get(*n).args[0])
            .find(|c| d.lane_undecidable(*c));
        let Some(c) = first else {
            // The row is resolved; the error is settled, not split.
            let o = settle_error(d, o);
            let same = |p: &Lite| class[p.t] == class[o.t] && p.fields == o.fields;
            match done.iter_mut().find(|p| same(p)) {
                Some(p) => p.merge(d, o.guard, o.error, &o.rests),
                None => done.push(o),
            }
            continue;
        };
        let atom = split_atom(d, c);
        let (may_true, may_false) = d.may_answers(atom);
        // The equal side of `x == k` reads `k` for `x` (exact; a near floor
        // must store the `state` it narrowed to). The other side learns nothing.
        let narrowed = {
            let node = d.graph.get(atom);
            let point = |n: NodeId| d.graph.point(n).is_some();
            match (node.op == Op::Eq, node.args.first().copied(), node.args.get(1).copied()) {
                (true, Some(a), Some(b)) if point(b) && d.abstract_beneath_lane_ops(a) => Some((a, b)),
                (true, Some(a), Some(b)) if point(a) && d.abstract_beneath_lane_ops(b) => Some((b, a)),
                _ => None,
            }
        };
        // The atom's SIBLINGS in the condition (other operands of its
        // `And`/`Or`): with the condition, what a side may case split on.
        let sibs: Vec<NodeId> = {
            let not_atom = d.graph.fold(Op::Not, vec![atom]);
            let is_atom = |n: NodeId| n == atom || n == not_atom;
            let mut out: Vec<NodeId> = Vec::new();
            for n in d.graph.cone(&[c]) {
                let node = d.graph.get(n);
                if matches!(node.op, Op::And | Op::Or) && node.args.iter().any(|a| is_atom(*a)) {
                    for a in node.args.iter().copied().filter(|a| !is_atom(*a)) {
                        if !out.contains(&a) {
                            out.push(a);
                        }
                    }
                }
            }
            out
        };
        let reach = d.graph.cone(&o.every());
        let mut made: Vec<(Lite, Vec<NodeId>)> = Vec::new();
        for answer in [true, false] {
            let to = d.graph.leaf(Op::ConstBool(answer));
            let mut subst = rustc_hash::FxHashMap::from_iter([(atom, to)]);
            if let (true, Some((x, k))) = (answer, narrowed) {
                subst.insert(x, k);
            }
            let map = rebuild_all(d, &reach, &subst);
            let m = |n: NodeId| map.get(&n).copied().unwrap_or(n);
            let may = if answer { may_true } else { may_false };
            let guard = d.and(&m(o.guard), &may);
            if d.decide(&guard) == Some(false) {
                continue;
            }
            let mut side = Lite {
                t: o.t,
                guard,
                error: m(o.error),
                fields: o.fields.iter().map(|f| m(*f)).collect(),
                rests: Vec::new(),
            };
            // This split's candidates first, then the inherited ones.
            let mut fresh: Vec<NodeId> = Vec::new();
            for r in std::iter::once(c).chain(sibs.iter().copied()).map(m) {
                if !fresh.contains(&r) {
                    fresh.push(r);
                }
            }
            for r in fresh.iter().copied().chain(o.rests.iter().map(|r| m(*r))) {
                if !side.rests.contains(&r) && side.rests.len() < MAX_RESTS {
                    side.rests.push(r);
                }
            }
            made.push((side, fresh));
        }
        // A side that is two existing outcomes under a decided case split is
        // them (`absorbed`).
        let rows: Vec<RowKey> = made.iter().map(|(l, _)| Waiting::key(&class, l)).collect();
        for (si, (side, fresh)) in made.into_iter().enumerate() {
            let made_elsewhere = |k: &RowKey| rows.iter().enumerate().any(|(j, r)| j != si && r == k) || work.index.contains_key(k) || done.iter().any(|p| Waiting::key(&class, p) == *k);
            match absorbed(d, &class, &side, &fresh, made_elsewhere) {
                Some(halves) => {
                    for h in halves {
                        let size = d.graph.cone(&h.row()).len();
                        work.push(d, &class, h, size);
                    }
                }
                None => {
                    let size = d.graph.cone(&side.row()).len();
                    work.push(d, &class, side, size);
                }
            }
        }
    }
    absorb_again(d, &class, &mut done);
    Ok(reassemble(d, &outs, done))
}

/// ABSORPTION AGAIN once all outcomes exist (`split_undecided_selects`): an
/// outcome that is two others under a decided case split is them. One is
/// retried only when a merge gives it new candidates; rows never change, so
/// the keys are kept.
fn absorb_again<A: GraphArith>(d: &mut Symbolic<A>, class: &[usize], done: &mut Vec<Lite>) {
    let mut keys: Vec<_> = done.iter().map(|p| Waiting::key(class, p)).collect();
    let mut dirty = vec![true; done.len()];
    while let Some(i) = dirty.iter().position(|x| *x) {
        dirty[i] = false;
        let rests = done[i].rests.clone();
        let Some(halves) = absorbed(d, class, &done[i], &rests, |k| *k != keys[i] && keys.contains(k)) else {
            continue;
        };
        done.remove(i);
        keys.remove(i);
        dirty.remove(i);
        for h in halves {
            let key = Waiting::key(class, &h);
            let at = keys.iter().position(|k| *k == key).expect("an absorbing half's row is an outcome");
            let p = &mut done[at];
            let had = p.rests.len();
            p.merge(d, h.guard, h.error, &h.rests);
            dirty[at] |= p.rests.len() > had;
        }
    }
}

/// Each outcome WHOLE again (`split_undecided_selects`): its error settled
/// once more (merges copied guards into it), its guard three-valued, its
/// fields and transfer roots the split's.
fn reassemble<A: GraphArith>(d: &mut Symbolic<A>, outs: &[FrameOut<A>], done: Vec<Lite>) -> Vec<FrameOut<A>> {
    let mut whole = Vec::with_capacity(done.len());
    for lite in done {
        // Settle the error again: merges copied guards into it.
        let lite = settle_error(d, lite);
        let mut o = outs[lite.t].clone();
        o.guard = three_valued(d, lite.guard);
        o.error = lite.error;
        let nf = o.fields.len();
        for (f, n) in o.fields.iter_mut().zip(&lite.fields[..nf]) {
            f.1 = *n;
        }
        let mut arc = lite.fields[nf..].iter().copied();
        o.arc = o.arc.map(|a| a.map(|_| arc.next().expect("the transfer roots ride with the fields")));
        whole.push(o);
    }
    whole
}

/// `root` (a guard or error) with no select on an undecided condition left
/// for the kernel to read by a garbage bit: a boolean select becomes `(c and
/// a) or (not c and b)` (exact in Kleene), a NUMBER `Sel(Known(c), Sel(c, x,
/// y), hull)`, a sound "may be live". Ops the kernel computes on exact
/// operands only are pushed into the arms, up to `ARMS` combinations.
fn three_valued<A: GraphArith>(d: &mut Symbolic<A>, root: NodeId) -> NodeId {
    use crate::transpile::graph::Op;
    let nodes = d.graph.cone(&[root]);
    // Already-wrapped selects stay (they keep the registration bounding their
    // error): idempotent.
    let wrapped: rustc_hash::FxHashSet<NodeId> = nodes
        .iter()
        .filter_map(|n| {
            let node = d.graph.get(*n);
            if node.op != Op::Sel {
                return None;
            }
            let (k, inner) = (d.graph.get(node.args[0]), d.graph.get(node.args[1]));
            (k.op == Op::Known && inner.op == Op::Sel && inner.args[0] == k.args[0]).then_some(node.args[1])
        })
        .collect();
    let undecided = |d: &mut Symbolic<A>, n: NodeId| !wrapped.contains(&n) && d.graph.get(n).op == Op::Sel && d.lane_undecidable(d.graph.get(n).args[0]);
    if !nodes.iter().any(|n| undecided(d, *n)) {
        return root;
    }
    let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = Default::default();
    // What undecided selects reach transitively, and those numbers' exact
    // per-arm values.
    let mut affected: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut arms_memo: rustc_hash::FxHashMap<NodeId, Option<Vec<(NodeId, NodeId)>>> = Default::default();
    let yes = d.graph.leaf(Op::ConstBool(true));
    for &n in &nodes {
        let (op, args) = {
            let node = d.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        if wrapped.contains(&n) {
            map.insert(n, n);
            continue;
        }
        let a: Vec<NodeId> = args.iter().map(|x| *map.get(x).unwrap_or(x)).collect();
        let sel_undecided = op == Op::Sel && (d.lane_undecidable(args[0]) || d.lane_undecidable(a[0]));
        // Undecided as REWRITTEN: a condition made undecidable here turns a
        // decided select into an undecided one.
        let new = if sel_undecided {
            let (c, x, y) = (a[0], a[1], a[2]);
            if d.graph.ty(n).is_bool() {
                let nc = d.graph.fold(Op::Not, vec![c]);
                let tx = d.graph.fold(Op::And, vec![c, x]);
                let fy = d.graph.fold(Op::And, vec![nc, y]);
                d.graph.fold(Op::Or, vec![tx, fy])
            } else {
                let decided = d.graph.fold(Op::Known, vec![c]);
                let picked = d.graph.fold(Op::Sel, vec![c, x, y]);
                // Evaluated only where the lane decides `c`. SET, not OR-ed:
                // `picked` is the hash-consed original, whose earlier registration
                // would make `not Known(c)` hold everywhere. The only place an
                // undecided select survives.
                d.frame.evaluated.insert(picked, decided);
                let (xl, yl) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Lo, vec![y]));
                let (xh, yh) = (d.graph.fold(Op::Hi, vec![x]), d.graph.fold(Op::Hi, vec![y]));
                let lo = d.graph.fold(Op::Min, vec![xl, yl]);
                let hi = d.graph.fold(Op::Max, vec![xh, yh]);
                let hull = d.graph.fold(Op::Span, vec![lo, hi]);
                d.graph.fold(Op::Sel, vec![decided, picked, hull])
            }
        } else if d.graph.exact_operands_only(&op, &args) && args.iter().any(|x| numeric(d, &affected, *x)) {
            // An exact-only op reading a hull: distributed over the arm
            // combinations (Kleene join / hull); past `ARMS`, the weakest value.
            let mut combos: Option<Vec<(NodeId, Vec<NodeId>)>> = Some(vec![(yes, Vec::new())]);
            for (k, x) in args.iter().enumerate() {
                let alts = if numeric(d, &affected, *x) {
                    arms(d, *x, &map, &affected, &mut arms_memo)
                } else {
                    Some(vec![(yes, a[k])])
                };
                combos = arm_product(d, combos, alts);
            }
            let boolean_op = d.graph.ty(n).is_bool();
            match combos {
                Some(cs) if boolean_op => {
                    let mut out = d.graph.leaf(Op::ConstBool(false));
                    for (c, vs) in cs {
                        let v = d.graph.fold(op.clone(), vs);
                        let both = d.graph.fold(Op::And, vec![c, v]);
                        out = d.graph.fold(Op::Or, vec![out, both]);
                    }
                    out
                }
                Some(cs) => {
                    let vals: Vec<NodeId> = cs.into_iter().map(|(_, vs)| d.graph.fold(op.clone(), vs)).collect();
                    hull_of(d, &vals)
                }
                None if boolean_op => {
                    d.unknown_bool_atom()
                }
                None => {
                    let (lo, hi) = A::bounds(A::unknown());
                    d.graph.leaf(Op::Const(A::lit(lo), A::lit(hi)))
                }
            }
        } else if a == args {
            n
        } else {
            d.graph.fold(op.clone(), a)
        };
        if args.iter().any(|x| affected.contains(x)) || sel_undecided {
            affected.insert(n);
        }
        map.insert(n, new);
    }
    map[&root]
}

/// Each of `combos` (a condition and the operands so far) with each of `alts`
/// (where the next operand takes which value). `None` past `ARMS`.
fn arm_product<A: GraphArith>(d: &mut Symbolic<A>, combos: Option<Vec<(NodeId, Vec<NodeId>)>>, alts: Option<Vec<(NodeId, NodeId)>>) -> Option<Vec<(NodeId, Vec<NodeId>)>> {
    use crate::transpile::graph::Op;
    let (cs, alts) = (combos?, alts?);
    if cs.len() * alts.len() > ARMS {
        return None;
    }
    let mut next = Vec::with_capacity(cs.len() * alts.len());
    for (c, vs) in &cs {
        for (ac, v) in &alts {
            let mut vs = vs.clone();
            vs.push(*v);
            next.push((d.graph.fold(Op::And, vec![*c, *ac]), vs));
        }
    }
    Some(next)
}

const ARMS: usize = 64;

/// A number an undecided select reaches (`three_valued`'s `affected`).
fn numeric<A: GraphArith>(d: &Symbolic<A>, affected: &rustc_hash::FxHashSet<NodeId>, x: NodeId) -> bool {
    affected.contains(&x) && !d.graph.ty(x).is_bool()
}

fn hull_of<A: GraphArith>(d: &mut Symbolic<A>, vals: &[NodeId]) -> NodeId {
    use crate::transpile::graph::Op;
    let (mut lo, mut hi) = (d.graph.fold(Op::Lo, vec![vals[0]]), d.graph.fold(Op::Hi, vec![vals[0]]));
    for v in &vals[1..] {
        let (l, h) = (d.graph.fold(Op::Lo, vec![*v]), d.graph.fold(Op::Hi, vec![*v]));
        lo = d.graph.fold(Op::Min, vec![lo, l]);
        hi = d.graph.fold(Op::Max, vec![hi, h]);
    }
    d.graph.fold(Op::Span, vec![lo, hi])
}

/// A hulled number as its exact values and where: `(condition, value)`
/// over its undecided selects (Kleene conditions). `None` past `ARMS`.
fn arms<A: GraphArith>(
    d: &mut Symbolic<A>,
    n: NodeId,
    map: &rustc_hash::FxHashMap<NodeId, NodeId>,
    affected: &rustc_hash::FxHashSet<NodeId>,
    memo: &mut rustc_hash::FxHashMap<NodeId, Option<Vec<(NodeId, NodeId)>>>,
) -> Option<Vec<(NodeId, NodeId)>> {
    use crate::transpile::graph::Op;
    if let Some(hit) = memo.get(&n) {
        return hit.clone();
    }
    let yes = d.graph.leaf(Op::ConstBool(true));
    let (op, args) = {
        let node = d.graph.get(n);
        (node.op.clone(), node.args.clone())
    };
    let out = if !affected.contains(&n) {
        Some(vec![(yes, *map.get(&n).unwrap_or(&n))])
    } else if op == Op::Sel && (d.lane_undecidable(args[0]) || d.lane_undecidable(*map.get(&args[0]).unwrap_or(&args[0]))) {
        // Undecided as rewritten too: an arm per answer.
        let c = *map.get(&args[0]).unwrap_or(&args[0]);
        let nc = d.graph.fold(Op::Not, vec![c]);
        match (arms(d, args[1], map, affected, memo), arms(d, args[2], map, affected, memo)) {
            (Some(t), Some(f)) if t.len() + f.len() <= ARMS => {
                let mut v = Vec::with_capacity(t.len() + f.len());
                for (k, x) in t {
                    v.push((d.graph.fold(Op::And, vec![k, c]), x));
                }
                for (k, x) in f {
                    v.push((d.graph.fold(Op::And, vec![k, nc]), x));
                }
                Some(v)
            }
            _ => None,
        }
    } else {
        let mut combos: Option<Vec<(NodeId, Vec<NodeId>)>> = Some(vec![(yes, Vec::new())]);
        for x in &args {
            let alts = if numeric(d, affected, *x) { arms(d, *x, map, affected, memo) } else { Some(vec![(yes, *map.get(x).unwrap_or(x))]) };
            combos = arm_product(d, combos, alts);
        }
        combos.map(|cs| cs.into_iter().map(|(c, vs)| (c, d.graph.fold(op.clone(), vs))).collect())
    };
    memo.insert(n, out.clone());
    out
}

/// How many case-split candidates an outcome carries (`Lite::rests`).
const MAX_RESTS: usize = 32;

/// `o` CASE-SPLIT on a lane-decided condition in `cands` whose halves are
/// both existing outcomes: the halves to merge, or `None`. With an atom
/// substituted, the rest of a condition may be lane-decided (a fall floor's
/// `delay <= 0 and not check(player, 0, 0)`); left in the row it is a third
/// variant, 3x outcomes per floor instead of 2. EXACT: a lane takes the half
/// its own answer gives; taken only where BOTH halves merge.
fn absorbed<A: GraphArith>(
    d: &mut Symbolic<A>,
    class: &[usize],
    o: &Lite,
    cands: &[NodeId],
    made: impl Fn(&RowKey) -> bool,
) -> Option<Vec<Lite>> {
    use crate::transpile::graph::Op;
    // Only the ROW is rebuilt: on a half's lanes guard and error read `cv` as
    // decided (and an error's cone can be huge).
    let mut reach: Option<Vec<NodeId>> = None;
    for &cv in cands {
        if d.lane_undecidable(cv) || d.decide(&cv).is_some() {
            continue;
        }
        let reach = reach.get_or_insert_with(|| d.graph.cone(&o.row()));
        if reach.binary_search(&cv).is_err() {
            continue;
        }
        let mut halves = Vec::new();
        for half in [true, false] {
            let to = d.graph.leaf(Op::ConstBool(half));
            let map = rebuild_all(d, reach, &rustc_hash::FxHashMap::from_iter([(cv, to)]));
            let m = |n: NodeId| map.get(&n).copied().unwrap_or(n);
            let at = if half { cv } else { d.not(&cv) };
            let guard = d.and(&o.guard, &at);
            if d.decide(&guard) == Some(false) {
                continue;
            }
            halves.push(Lite {
                t: o.t,
                guard,
                error: o.error,
                fields: o.fields.iter().map(|f| m(*f)).collect(),
                rests: o.rests.iter().map(|r| m(*r)).filter(|r| *r != cv).collect(),
            });
        }
        if !halves.is_empty() && halves.iter().all(|h| made(&Waiting::key(class, h))) {
            return Some(halves);
        }
    }
    None
}

/// Every node of `cone` rebuilt with `subst` applied, in node order
/// (comparisons refolded against the static ranges); only changed nodes are
/// mapped. Where a node was EVALUATED moves with it, else the rebuild's own
/// error would hold on every lane.
fn rebuild_all<A: GraphArith>(d: &mut Symbolic<A>, cone: &[NodeId], subst: &rustc_hash::FxHashMap<NodeId, NodeId>) -> rustc_hash::FxHashMap<NodeId, NodeId> {
    use super::domain::{Cmp, Domain};
    use crate::transpile::graph::Op;
    let mut all: Vec<NodeId> = cone.to_vec();
    loop {
        let have: rustc_hash::FxHashSet<NodeId> = all.iter().copied().collect();
        let more: Vec<NodeId> = all.iter().filter_map(|n| d.frame.evaluated.get(n).copied()).filter(|a| !have.contains(a)).collect();
        if more.is_empty() {
            break;
        }
        all.extend(more);
        all = d.graph.cone(&all);
    }
    let cone = &all[..];
    let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = subst.clone();
    for &i in cone {
        if subst.contains_key(&i) {
            continue;
        }
        let (op, args) = {
            let node = d.graph.get(i);
            (node.op.clone(), node.args.clone())
        };
        let mapped: Vec<NodeId> = args.iter().map(|a| *map.get(a).unwrap_or(a)).collect();
        if mapped == args {
            continue;
        }
        // The orderings (`compare` folds an equality by its literals only).
        let cmp = Cmp::of(&op).filter(|c| *c != Cmp::Eq);
        let by_range = cmp.and_then(|k| d.compare(k, &mapped[0], &mapped[1]).ok()).filter(|r| matches!(d.graph.get(*r).op, Op::ConstBool(_)));
        let n = match by_range {
            Some(r) => r,
            None => d.graph.fold(op, mapped),
        };
        map.insert(i, n);
    }
    let moved: Vec<(NodeId, NodeId)> = cone.iter().filter_map(|i| Some((*map.get(i)?, *d.frame.evaluated.get(i)?))).collect();
    for (n, at) in moved {
        let at = *map.get(&at).unwrap_or(&at);
        d.evaluated_at(&n, &at);
    }
    map
}
