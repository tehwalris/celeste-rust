//! THE SPLIT PASS: making a traced frame executable under abstract inputs
//! (`split_undecided_selects`). A select a lane can hold both ways cannot be
//! evaluated, so a select the row stores is split; errors are settled by
//! case analysis (`MayErr`), guards rewritten three-valued (`three_valued`),
//! and with the platforms unknown, comparisons decided per platform world
//! (`Points`).

use anyhow::{anyhow, Result};

use super::domain::{Domain, Symbolic};
use super::frame::FrameOut;
use crate::transpile::garith::GraphArith;
use crate::transpile::graph::NodeId;
use celeste_core::arith::Fixed;

/// An outcome's row as `Waiting` indexes it: its shape class and its fields.
type RowKey = (usize, Vec<NodeId>);

/// `split_undecided_selects`' queue: largest cone first, deduped on arrival
/// (an equal outcome merges in).
#[derive(Default)]
struct Waiting {
    slots: Vec<Option<(Lite, Option<PointSet>)>>,
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
    /// What the row stores.
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
    /// Same shape class and row: one outcome.
    fn key(class: &[usize], o: &Lite) -> RowKey {
        (class[o.t], o.fields.clone())
    }

    fn push<A: GraphArith>(&mut self, d: &mut Symbolic<A>, class: &[usize], o: Lite, pts: Option<PointSet>, size: usize) {
        let key = Self::key(class, &o);
        if let Some(&i) = self.index.get(&key) {
            let (p, pp) = self.slots[i].as_mut().expect("an indexed outcome is waiting");
            p.merge(d, o.guard, o.error, &o.rests);
            if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                a.union(b);
            }
            return;
        }
        let i = self.slots.len();
        self.slots.push(Some((o, pts)));
        self.index.insert(key, i);
        self.heap.push((size, std::cmp::Reverse(i)));
    }

    fn pop(&mut self, class: &[usize]) -> Option<(Lite, Option<PointSet>)> {
        let (_, std::cmp::Reverse(i)) = self.heap.pop()?;
        let out = self.slots[i].take().expect("a queued outcome is waiting");
        self.index.remove(&Self::key(class, &out.0));
        Some(out)
    }

    fn len(&self) -> usize {
        self.index.len()
    }
}

/// An outcome's error once its row is resolved: WHERE A LANE MAY ERR, with no
/// undecided select left. By case analysis on the error alone (not the
/// outcome, whose split sides would double per round) and not three-valued
/// (a hull loses what a select's condition says, e.g. the platform wrap).
fn settle_error<A: GraphArith>(d: &mut Symbolic<A>, points: Option<&mut Points<A>>, pts: Option<&PointSet>, room: Option<&crate::transpile::graph::Room>, mut o: Lite) -> Lite {
    let mut m = MayErr { points, room, memo: Default::default(), cases: 0 };
    let here = pts.cloned();
    o.error = m.may(d, o.error, here.as_ref()).0;
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

/// The answers a split of `atom` takes at the points `pts` (`Points`), each
/// with its points: an answer no point gives is no side, and one side left
/// means the points DECIDE it. Without points, both answers.
fn sides<A: GraphArith>(d: &Symbolic<A>, points: Option<&mut Points<A>>, room: Option<&crate::transpile::graph::Room>, pts: Option<&PointSet>, atom: NodeId) -> Vec<(bool, Option<PointSet>)> {
    match (points, pts) {
        (Some(p), Some(here)) => {
            let (yes, no) = p.answers(d, room, atom);
            [(true, yes.and(here)), (false, no.and(here))].into_iter().filter(|(_, s)| !s.is_empty()).map(|(a, s)| (a, Some(s))).collect()
        }
        _ => vec![(true, None), (false, None)],
    }
}

/// `MayErr::may`'s bound on the cases it opens under one error.
const MAY_CASES: usize = 4096;

/// A boolean's `(may be true, may be false)` with no undecided select left:
/// connectives through operands (`and` over-approximates, safe for an
/// error); a node reading an undecided select is split on its lowest
/// undecidable comparison, each case under that answer's `may` guard and
/// narrowed points; others as they are. Past `MAY_CASES`, three-valued.
struct MayErr<'a, 'r, A: GraphArith> {
    points: Option<&'a mut Points<A>>,
    room: Option<&'r crate::transpile::graph::Room>,
    memo: rustc_hash::FxHashMap<(NodeId, Option<Vec<u64>>), (NodeId, NodeId)>,
    cases: usize,
}

impl<A: GraphArith> MayErr<'_, '_, A> {
    fn may(&mut self, d: &mut Symbolic<A>, n: NodeId, pts: Option<&PointSet>) -> (NodeId, NodeId) {
        use crate::transpile::graph::Op;
        let key = (n, pts.map(|p| p.0.clone()));
        if let Some(hit) = self.memo.get(&key) {
            return *hit;
        }
        let (op, args) = {
            let node = d.graph.get(n);
            (node.op.clone(), node.args.clone())
        };
        let reads_undecided = |d: &mut Symbolic<A>, n: NodeId| d.graph.cone(&[n]).into_iter().find(|m| d.graph.get(*m).op == Op::Sel && d.lane_undecidable(d.graph.get(*m).args[0]));
        let out = match op {
            Op::Or | Op::And => {
                let parts: Vec<(NodeId, NodeId)> = args.iter().map(|a| self.may(d, *a, pts)).collect();
                let (t, f): (Vec<NodeId>, Vec<NodeId>) = parts.into_iter().unzip();
                if op == Op::Or {
                    (d.graph.fold(Op::Or, t), d.graph.fold(Op::And, f))
                } else {
                    (d.graph.fold(Op::And, t), d.graph.fold(Op::Or, f))
                }
            }
            Op::Not => {
                let (t, f) = self.may(d, args[0], pts);
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
                    let sides = sides(d, self.points.as_deref_mut(), self.room, pts, atom);
                    let decided = sides.len() == 1;
                    let (may_true, may_false) = d.may_answers(atom);
                    let reach = d.graph.cone(&[n]);
                    let (mut t, mut f) = (d.graph.leaf(Op::ConstBool(false)), d.graph.leaf(Op::ConstBool(false)));
                    for (answer, side) in sides {
                        let to = d.graph.leaf(Op::ConstBool(answer));
                        let map = rebuild_all(d, &reach, &rustc_hash::FxHashMap::from_iter([(atom, to)]));
                        let m = map.get(&n).copied().unwrap_or(n);
                        let (ct, cf) = self.may(d, m, side.as_ref());
                        let (ct, cf) = if decided {
                            (ct, cf)
                        } else {
                            let g = if answer { may_true } else { may_false };
                            (d.graph.fold(Op::And, vec![g, ct]), d.graph.fold(Op::And, vec![g, cf]))
                        };
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
        self.memo.insert(key, out);
        out
    }
}

/// `Symbolic::evaluated`'s conditions for every reached node, rewritten by
/// `three_valued`.
pub(super) fn three_valued_evaluated<A: GraphArith>(d: &mut Symbolic<A>, outs: &[FrameOut<A>]) {
    let roots: Vec<NodeId> = outs.iter().flat_map(|o| o.roots(false)).collect();
    let mut done: rustc_hash::FxHashSet<NodeId> = Default::default();
    let mut todo = d.graph.cone(&roots);
    while !todo.is_empty() {
        let mut next: Vec<NodeId> = Vec::new();
        for n in todo {
            if !done.insert(n) {
                continue;
            }
            if let Some(at) = d.evaluated.get(&n).copied() {
                let at2 = three_valued(d, at);
                if at2 != at {
                    d.evaluated.insert(n, at2);
                }
                next.push(at2);
            }
        }
        todo = d.graph.cone(&next).into_iter().filter(|n| !done.contains(n)).collect();
    }
}

/// A set of `Points`' points, one bit each.
#[derive(Clone, PartialEq, Eq, Debug)]
struct PointSet(Vec<u64>);

impl PointSet {
    fn empty(n: usize) -> Self {
        PointSet(vec![0; n.div_ceil(64)])
    }
    fn full(n: usize) -> Self {
        let mut s = Self::empty(n);
        for i in 0..n {
            s.insert(i);
        }
        s
    }
    fn insert(&mut self, i: usize) {
        self.0[i / 64] |= 1 << (i % 64);
    }
    fn contains(&self, i: usize) -> bool {
        self.0[i / 64] >> (i % 64) & 1 == 1
    }
    fn union(&mut self, o: &PointSet) {
        for (a, b) in self.0.iter_mut().zip(&o.0) {
            *a |= b;
        }
    }
    fn and(&self, o: &PointSet) -> PointSet {
        PointSet(self.0.iter().zip(&o.0).map(|(a, b)| a & b).collect())
    }
    fn is_empty(&self) -> bool {
        self.0.iter().all(|w| *w == 0)
    }
}

/// THE POINTS a platforms-unknown frame's comparisons are decided over: every
/// PLATFORM WORLD at every pixel of the player's region. A path carries the
/// points consistent with its answers (dropped at none), so its answers come
/// from ONE platform arrangement. A point's comparison is decided by
/// `eval_lenient_in` with cells pinned; undecided goes both ways. Compile time
/// only: the kernel sees each outcome's pixels (`position_guard`).
pub struct Points<A: GraphArith = Fixed> {
    /// Per world, the value of each platform cell: `(cell, x)`.
    worlds: Vec<Vec<(u32, A::Num)>>,
    platform: rustc_hash::FxHashSet<u32>,
    /// The player's input `x`/`y`: cell, node, the region's first pixel and
    /// count. `None` without a player.
    pos: [Option<(u32, NodeId, i16, usize)>; 2],
    memo: rustc_hash::FxHashMap<NodeId, (PointSet, PointSet)>,
}

impl<A: GraphArith> Points<A> {
    pub(super) fn new(d: &Symbolic<A>, pos: [Option<(NodeId, (A::Lit, A::Lit))>; 2]) -> Result<Points<A>> {
        use crate::transpile::graph::Op;
        let table = d.worlds.clone().ok_or_else(|| anyhow!("the platforms are unknown but there is no world table"))?;
        let cell = |n: NodeId| match d.graph.get(n).op {
            Op::Cell(c) => Ok(c),
            ref o => Err(anyhow!("a pinned input is {o:?}, not a cell")),
        };
        let mut worlds = Vec::with_capacity(table.0.len());
        for w in table.0.iter() {
            anyhow::ensure!(w.len() == d.platform_cells.len(), "a world has {} platforms, the frame {}", w.len(), d.platform_cells.len());
            let mut pins = Vec::new();
            for (p, x) in w.iter().zip(&d.platform_cells) {
                pins.push((cell(*x)?, p[0]));
            }
            worlds.push(pins);
        }
        let platform = worlds.first().map(|w| w.iter().map(|(c, _)| *c).collect()).unwrap_or_default();
        let mut out = [None, None];
        for (k, p) in pos.iter().enumerate() {
            if let Some((n, (lo, hi))) = p {
                let pixel = |l: A::Lit| A::as_i16(A::from_lit(l)).ok_or_else(|| anyhow!("the player's position range is not whole pixels"));
                let (a, b) = (pixel(*lo)?, pixel(*hi)?);
                anyhow::ensure!(a <= b, "the player's position range is empty");
                out[k] = Some((cell(*n)?, *n, a, (b - a + 1) as usize));
            }
        }
        Ok(Points { worlds, platform, pos: out, memo: Default::default() })
    }

    fn per_world(&self) -> usize {
        self.pos.iter().map(|p| p.map_or(1, |p| p.3)).product()
    }

    fn len(&self) -> usize {
        self.worlds.len() * self.per_world()
    }

    /// The point of world `w` at pixel offsets `(i, j)` into the region.
    fn index(&self, w: usize, i: usize, j: usize) -> usize {
        let ny = self.pos[1].map_or(1, |p| p.3);
        (w * self.per_world()) + i * ny + j
    }

    /// Where `c` can come out true, and where false.
    fn answers(&mut self, d: &Symbolic<A>, room: Option<&crate::transpile::graph::Room>, c: NodeId) -> (PointSet, PointSet) {
        use crate::transpile::graph::Val;
        if let Some(hit) = self.memo.get(&c) {
            return hit.clone();
        }
        // The cone as its own graph, so an evaluation costs only the cone.
        let nodes = d.graph.cone(&[c]);
        let mut g = d.graph.like();
        let mut map: rustc_hash::FxHashMap<NodeId, NodeId> = Default::default();
        for &n in &nodes {
            let node = d.graph.get(n);
            let args = node.args.iter().map(|a| map[a]).collect();
            map.insert(n, g.add(node.op.clone(), args));
        }
        let root = map[&c] as usize;
        // Every cell at its weakest; a bounded input narrows at its `Restrict`.
        let base = crate::transpile::ival::seed_cells(&g);
        let value = |v: A::Num| Val::<A>::Num(crate::transpile::garith::value_box::<A>(v));
        let pixel = |p: i16, i: usize| value(A::from_i16(p + i as i16));
        let eval = |cells: &std::collections::HashMap<u32, Val<A>>| -> Option<bool> {
            let v = match room {
                Some(r) => g.eval_lenient_in(cells, r),
                None => g.eval_lenient(cells),
            };
            match v.ok()?.get(root) {
                Some(Val::Bool(b)) => *b,
                _ => None,
            }
        };
        let reads_platform = base.keys().any(|k| self.platform.contains(k));
        let reads = |k: usize| self.pos[k].filter(|p| base.contains_key(&p.0));
        let (px, py) = (reads(0), reads(1));
        let n = self.len();
        let (mut yes, mut no) = (PointSet::empty(n), PointSet::empty(n));
        let (nx, ny) = (self.pos[0].map_or(1, |p| p.3), self.pos[1].map_or(1, |p| p.3));
        // Mark worlds `ws` at pixels `is` x `js` with answer `r`.
        let mut mark = |ws: &[usize], is: &[usize], js: &[usize], r: Option<bool>| {
            for &w in ws {
                for &i in is {
                    for &j in js {
                        let at = self.index(w, i, j);
                        if r != Some(false) {
                            yes.insert(at);
                        }
                        if r != Some(true) {
                            no.insert(at);
                        }
                    }
                }
            }
        };
        let all_i: Vec<usize> = (0..nx).collect();
        let all_j: Vec<usize> = (0..ny).collect();
        let all_w: Vec<usize> = (0..self.worlds.len()).collect();
        let groups: Vec<Vec<usize>> = if reads_platform { all_w.iter().map(|w| vec![*w]).collect() } else { vec![all_w.clone()] };
        for ws in &groups {
            let mut cells = base.clone();
            if reads_platform {
                for (k, v) in &self.worlds[ws[0]] {
                    if cells.contains_key(k) {
                        cells.insert(*k, value(*v));
                    }
                }
            }
            let r = eval(&cells);
            if r.is_some() || (px.is_none() && py.is_none()) {
                mark(ws, &all_i, &all_j, r);
                continue;
            }
            // Per pixel, along the axes the comparison reads.
            let is: Vec<Option<usize>> = if px.is_some() { (0..nx).map(Some).collect() } else { vec![None] };
            let js: Vec<Option<usize>> = if py.is_some() { (0..ny).map(Some).collect() } else { vec![None] };
            for i in &is {
                for j in &js {
                    let mut at = cells.clone();
                    if let (Some(i), Some(p)) = (i, px) {
                        at.insert(p.0, pixel(p.2, *i));
                    }
                    if let (Some(j), Some(p)) = (j, py) {
                        at.insert(p.0, pixel(p.2, *j));
                    }
                    let r = eval(&at);
                    let ii = i.map_or(all_i.clone(), |i| vec![i]);
                    let jj = j.map_or(all_j.clone(), |j| vec![j]);
                    mark(ws, &ii, &jj, r);
                }
            }
        }
        self.memo.insert(c, (yes.clone(), no.clone()));
        (yes, no)
    }

    /// The lanes an outcome at `pts` is live on: the pixels some world of
    /// `pts` has (`true` if all).
    fn position_guard(&self, d: &mut Symbolic<A>, pts: &PointSet) -> NodeId {
        use crate::transpile::graph::Op;
        let (nx, ny) = (self.pos[0].map_or(1, |p| p.3), self.pos[1].map_or(1, |p| p.3));
        let at = |i: usize, j: usize| (0..self.worlds.len()).any(|w| pts.contains(self.index(w, i, j)));
        let yes = d.graph.leaf(Op::ConstBool(true));
        if (0..nx).all(|i| (0..ny).all(|j| at(i, j))) {
            return yes;
        }
        // `lo <= cell <= hi` over whole pixels of one axis.
        let within = |d: &mut Symbolic<A>, axis: usize, lo: usize, hi: usize| -> NodeId {
            let Some((_, n, first, _)) = self.pos[axis] else { return d.graph.leaf(Op::ConstBool(true)) };
            let (a, b) = (A::lit(A::from_i16(first + lo as i16)), A::lit(A::from_i16(first + hi as i16)));
            let (ka, kb) = (d.graph.leaf(Op::Const(a, a)), d.graph.leaf(Op::Const(b, b)));
            let ge = d.graph.fold(Op::Ge, vec![n, ka]);
            let le = d.graph.fold(Op::Le, vec![n, kb]);
            d.graph.fold(Op::And, vec![ge, le])
        };
        // Runs of `j` per column, and runs of columns with the same runs.
        let runs = |i: usize| -> Vec<(usize, usize)> {
            let mut out: Vec<(usize, usize)> = Vec::new();
            for j in 0..ny {
                if at(i, j) {
                    match out.last_mut() {
                        Some(r) if r.1 + 1 == j => r.1 = j,
                        _ => out.push((j, j)),
                    }
                }
            }
            out
        };
        let mut guard = d.graph.leaf(Op::ConstBool(false));
        let mut i = 0;
        while i < nx {
            let r = runs(i);
            let mut k = i;
            while k + 1 < nx && runs(k + 1) == r {
                k += 1;
            }
            if !r.is_empty() {
                let mut col = d.graph.leaf(Op::ConstBool(false));
                for (a, b) in &r {
                    let w = within(d, 1, *a, *b);
                    col = d.graph.fold(Op::Or, vec![col, w]);
                }
                let x = within(d, 0, i, k);
                let both = d.graph.fold(Op::And, vec![x, col]);
                guard = d.graph.fold(Op::Or, vec![guard, both]);
            }
            i = k + 1;
        }
        guard
    }
}

/// MAKING THE FRAME EXECUTABLE UNDER ABSTRACT INPUTS: a select whose
/// condition a lane holds both ways cannot be evaluated. A select the row
/// STORES is split, one at a time, the first in program order (the outcome
/// in two, readers refolded, guards narrowed by `may_answers`), until none
/// is left; equal outcomes fuse. The error is settled (`settle_error`) and
/// the guard rewritten three-valued, neither split. With `Points`, a
/// comparison with one answer at a path's points is decided without a split.
pub(super) fn split_undecided_selects<A: GraphArith>(d: &mut Symbolic<A>, outs: Vec<FrameOut<A>>, mut points: Option<Points<A>>, room: Option<&crate::transpile::graph::Room>) -> Result<Vec<FrameOut<A>>> {
    use crate::transpile::graph::Op;
    const MAX_OUTCOMES: usize = 4096;
    let n_in = outs.len();
    // Templates of one shape share a class: their outcomes dedupe together.
    let class: Vec<usize> = (0..outs.len()).map(|i| (0..=i).find(|&j| outs[j].shape == outs[i].shape).expect("itself")).collect();
    // LARGEST CONE FIRST: a split only shrinks a cone, so every path into a
    // state merges before it is split.
    let mut work = Waiting::default();
    let everywhere = points.as_ref().map(|p| PointSet::full(p.len()));
    for (t, o) in outs.iter().enumerate() {
        let lite = Lite {
            t,
            guard: o.guard,
            error: o.error,
    // The transfer roots ride with the fields: same substitutions, and part
    // of what makes two outcomes one.
            fields: o.fields.iter().map(|f| f.1).chain(o.arc.iter().copied()).collect(),
            rests: Vec::new(),
        };
        let size = d.graph.cone(&lite.row()).len();
        work.push(d, &class, lite, everywhere.clone(), size);
    }
    let (mut n_splits, mut n_decided, mut n_rest) = (0usize, 0usize, 0usize);
    let mut done: Vec<(Lite, Option<PointSet>)> = Vec::new();
    while let Some((o, pts)) = work.pop(&class) {
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
            let o = settle_error(d, points.as_mut(), pts.as_ref(), room, o);
            let same = |p: &Lite| class[p.t] == class[o.t] && p.fields == o.fields;
            match done.iter_mut().find(|(p, _)| same(p)) {
                Some((p, pp)) => {
                    p.merge(d, o.guard, o.error, &o.rests);
                    if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                        a.union(b);
                    }
                }
                None => done.push((o, pts)),
            }
            continue;
        };
        let atom = split_atom(d, c);
        let sides = sides(d, points.as_mut(), room, pts.as_ref(), atom);
        let decided = sides.len() == 1;
        if decided {
            n_decided += 1;
        } else {
            n_splits += 1;
        }
        let (may_true, may_false) = d.may_answers(atom);
        // The equal side of `x == k` reads `k` for `x` (exact; a near floor
        // must store the `state` it narrowed to). The other side learns nothing.
        let narrowed = {
            let node = d.graph.get(atom);
            let point = |n: NodeId| matches!(d.graph.get(n).op, Op::Const(lo, hi) if lo == hi);
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
        let mut made: Vec<(Lite, Option<PointSet>, Vec<NodeId>)> = Vec::new();
        for (answer, side_pts) in sides {
            let to = d.graph.leaf(Op::ConstBool(answer));
            let mut subst = rustc_hash::FxHashMap::from_iter([(atom, to)]);
            if let (true, Some((x, k))) = (answer, narrowed) {
                subst.insert(x, k);
            }
            let map = rebuild_all(d, &reach, &subst);
            let m = |n: NodeId| map.get(&n).copied().unwrap_or(n);
            let mut guard = m(o.guard);
            if !decided {
                let may = if answer { may_true } else { may_false };
                guard = d.and(&guard, &may);
            }
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
            made.push((side, side_pts, fresh));
        }
        // A side that is two existing outcomes under a decided case split is
        // them (`absorbed`).
        let rows: Vec<RowKey> = made.iter().map(|(l, _, _)| Waiting::key(&class, l)).collect();
        for (si, (side, side_pts, fresh)) in made.into_iter().enumerate() {
            let made_elsewhere = |k: &RowKey| {
                rows.iter().enumerate().any(|(j, r)| j != si && r == k) || work.index.contains_key(k) || done.iter().any(|(p, _)| Waiting::key(&class, p) == *k)
            };
            match absorbed(d, &class, &side, &fresh, made_elsewhere) {
                Some(halves) => {
                    n_rest += 1;
                    for h in halves {
                        let size = d.graph.cone(&h.row()).len();
                        work.push(d, &class, h, side_pts.clone(), size);
                    }
                }
                None => {
                    let size = d.graph.cone(&side.row()).len();
                    work.push(d, &class, side, side_pts, size);
                }
            }
        }
    }
    // Absorption again once all outcomes exist. An outcome is retried only
    // when a merge gives it new candidates; rows never change, so keep them.
    let mut keys: Vec<_> = done.iter().map(|(p, _)| Waiting::key(&class, p)).collect();
    let mut dirty = vec![true; done.len()];
    while let Some(i) = dirty.iter().position(|x| *x) {
        dirty[i] = false;
        let rests = done[i].0.rests.clone();
        let Some(halves) = absorbed(d, &class, &done[i].0, &rests, |k| *k != keys[i] && keys.contains(k)) else {
            continue;
        };
        let (_, pts) = done.remove(i);
        keys.remove(i);
        dirty.remove(i);
        for h in halves {
            let key = Waiting::key(&class, &h);
            let at = keys.iter().position(|k| *k == key).expect("an absorbing half's row is an outcome");
            let (p, pp) = &mut done[at];
            let had = p.rests.len();
            p.merge(d, h.guard, h.error, &h.rests);
            dirty[at] |= p.rests.len() > had;
            if let (Some(a), Some(b)) = (pp.as_mut(), pts.as_ref()) {
                a.union(b);
            }
        }
        n_rest += 1;
    }
    if d.build_trace {
        eprintln!("[build] split_undecided_selects: {n_in} outcomes, {n_splits} splits, {n_decided} decided by the points, {n_rest} absorbed by a case split, {} out", done.len());
    }
    // Each outcome whole again; only its pixels reach the kernel.
    let mut whole = Vec::with_capacity(done.len());
    for (lite, pts) in done {
        // Settle the error again: merges copied guards into it.
        let lite = settle_error(d, points.as_mut(), pts.as_ref(), room, lite);
        let mut o = outs[lite.t].clone();
        // The guard, three-valued.
        o.guard = three_valued(d, lite.guard);
        o.error = lite.error;
        let nf = o.fields.len();
        for (f, n) in o.fields.iter_mut().zip(&lite.fields[..nf]) {
            f.1 = *n;
        }
        o.arc = lite.fields[nf..].to_vec();
        if let (Some(p), Some(pts)) = (points.as_ref(), pts.as_ref()) {
            let g = p.position_guard(d, pts);
            o.guard = d.and(&o.guard, &g);
        }
        whole.push(o);
    }
    Ok(whole)
}

/// `root` (a guard or error) with no select on an undecided condition left
/// for the kernel to read by a garbage bit. A boolean select becomes
/// `(c and a) or (not c and b)` (exact in Kleene); a NUMBER becomes
/// `Sel(Known(c), Sel(c, x, y), hull)`, a sound "may be live". Ops the kernel
/// computes on exact operands only (`exact_only`) are pushed into the arms,
/// up to `ARMS` combinations (pushing everything is exact but exponential).
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
    // Booleans typed bottom-up from what each node IS, never from readers.
    let mut boolean: rustc_hash::FxHashSet<NodeId> = Default::default();
    for &n in &nodes {
        let node = d.graph.get(n);
        let b = match node.op {
            Op::Sel => boolean.contains(&node.args[1]) || boolean.contains(&node.args[2]),
            Op::Cell(c) => d.graph.cell_kind(c) == crate::transpile::graph::CellKind::Bool,
            ref o => is_bool_op(o),
        };
        if b {
            boolean.insert(n);
        }
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
            if boolean.contains(&n) {
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
                d.evaluated.insert(picked, decided);
                let (xl, yl) = (d.graph.fold(Op::Lo, vec![x]), d.graph.fold(Op::Lo, vec![y]));
                let (xh, yh) = (d.graph.fold(Op::Hi, vec![x]), d.graph.fold(Op::Hi, vec![y]));
                let lo = d.graph.fold(Op::Min, vec![xl, yl]);
                let hi = d.graph.fold(Op::Max, vec![xh, yh]);
                let hull = d.graph.fold(Op::Span, vec![lo, hi]);
                d.graph.fold(Op::Sel, vec![decided, picked, hull])
            }
        } else if exact_only(d, &op, &args) && args.iter().any(|x| affected.contains(x) && !boolean.contains(x) && !is_bool_op(&d.graph.get(*x).op)) {
            // An exact-only op reading a hull: distributed over the arm
            // combinations (Kleene join / hull); past `ARMS`, the weakest value.
            let mut combos: Option<Vec<(NodeId, Vec<NodeId>)>> = Some(vec![(yes, Vec::new())]);
            for (k, x) in args.iter().enumerate() {
                let alts = if affected.contains(x) && !boolean.contains(x) && !is_bool_op(&d.graph.get(*x).op) {
                    arms(d, *x, &map, &affected, &boolean, &mut arms_memo)
                } else {
                    Some(vec![(yes, a[k])])
                };
                combos = arm_product(d, combos, alts);
            }
            let boolean_op = is_bool_op(&op);
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
                    let u = d.unknown_bool_atom();
                    if d.build_trace {
                        eprintln!("[build] three_valued: {op:?} past {ARMS} arms, unknown {:?}", d.graph.get(u).op);
                    }
                    u
                }
                None => {
                    if d.build_trace {
                        eprintln!("[build] three_valued: {op:?} past {ARMS} arms, the whole range");
                    }
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

/// Each of `combos` (a condition and the operands so far) with each of
/// `alts` (where the next operand takes which value): the conditions AND-ed,
/// the value appended. `None` past `ARMS` (or where either is).
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

/// `three_valued`'s bound on the arms an exact-only op is distributed over.
const ARMS: usize = 64;

/// Ops the kernel computes on exact operands only (`asm::codegen`'s
/// `as_num`); `Mul`/`Div` also take an interval times a positive literal.
fn exact_only<A: GraphArith>(d: &Symbolic<A>, op: &crate::transpile::graph::Op<A>, args: &[NodeId]) -> bool {
    use crate::transpile::graph::Op;
    let pos = |n: NodeId| matches!(d.graph.get(n).op, Op::Const(lo, hi) if lo == hi && A::lt(A::from_i16(0), A::from_lit(lo)));
    match op {
        Op::TileFlagAt | Op::Mget | Op::Rem | Op::Sin => true,
        Op::Mul => !(pos(args[0]) || pos(args[1])),
        Op::Div => !pos(args[1]),
        _ => false,
    }
}

/// Ops whose value is a boolean.
fn is_bool_op<A: GraphArith>(op: &crate::transpile::graph::Op<A>) -> bool {
    use crate::transpile::graph::Op;
    matches!(
        op,
        Op::Lt | Op::Le | Op::Gt | Op::Ge | Op::Eq | Op::Not | Op::And | Op::Or | Op::Known | Op::ConstBool(_) | Op::UnknownBool(_) | Op::TileFlagAt
            | Op::SplitValid(_) | Op::SplitOk(_) | Op::FragOk(_) | Op::NoWrap
            | Op::RSplitValid(_) | Op::RSplitOk(_) | Op::RFragOk(_) | Op::Same
    )
}

/// The hull `[min lo, max hi]` of numbers.
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
    boolean: &rustc_hash::FxHashSet<NodeId>,
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
    let numeric = |d: &Symbolic<A>, x: NodeId| affected.contains(&x) && !boolean.contains(&x) && !is_bool_op(&d.graph.get(x).op);
    let out = if !affected.contains(&n) {
        Some(vec![(yes, *map.get(&n).unwrap_or(&n))])
    } else if op == Op::Sel && (d.lane_undecidable(args[0]) || d.lane_undecidable(*map.get(&args[0]).unwrap_or(&args[0]))) {
        // Undecided as rewritten too: an arm per answer.
        let c = *map.get(&args[0]).unwrap_or(&args[0]);
        let nc = d.graph.fold(Op::Not, vec![c]);
        match (arms(d, args[1], map, affected, boolean, memo), arms(d, args[2], map, affected, boolean, memo)) {
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
            let alts = if numeric(d, *x) { arms(d, *x, map, affected, boolean, memo) } else { Some(vec![(yes, *map.get(x).unwrap_or(x))]) };
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
/// variant beside "solid" and "hidden", 3x outcomes per floor instead of 2.
/// EXACT: a lane takes the half its own answer gives. Taken only where BOTH
/// halves merge, so it never adds an outcome.
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

/// Every node of `cone` rebuilt with `subst` applied, in node order:
/// comparisons refolded against the static ranges, the rest by `fold`; only
/// changed nodes are mapped. Where a node was EVALUATED moves with it (the
/// cone is widened to those conditions first), else the rebuild's own error
/// would hold on every lane.
fn rebuild_all<A: GraphArith>(d: &mut Symbolic<A>, cone: &[NodeId], subst: &rustc_hash::FxHashMap<NodeId, NodeId>) -> rustc_hash::FxHashMap<NodeId, NodeId> {
    use super::domain::{Cmp, Domain};
    use crate::transpile::graph::Op;
    let mut all: Vec<NodeId> = cone.to_vec();
    loop {
        let have: rustc_hash::FxHashSet<NodeId> = all.iter().copied().collect();
        let more: Vec<NodeId> = all.iter().filter_map(|n| d.evaluated.get(n).copied()).filter(|a| !have.contains(a)).collect();
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
    let moved: Vec<(NodeId, NodeId)> = cone.iter().filter_map(|i| Some((*map.get(i)?, *d.evaluated.get(i)?))).collect();
    for (n, at) in moved {
        let at = *map.get(&at).unwrap_or(&at);
        d.evaluated_at(&n, &at);
    }
    map
}
