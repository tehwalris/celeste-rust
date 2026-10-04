//! The rotation graph's dynamic programme (plans/arcs.md, steps 2 and 3):
//! over a STATIC remainder-free transition graph whose edges carry, per axis,
//! a guard (the remainders that take the edge) and an action (a rotation or a
//! collision's constant) - `search::arcs` - compute
//!
//! * BACKWARD, the winning sets `W_t(n)`: the remainder rectangles from which
//!   node `n`, occupied at frame `t`, wins by the horizon
//!   (`W_t(n) = U_e guard_e  n  action_e^-1( W_{t+1}(dst_e) )`, a win node's
//!   set the whole torus), and
//! * FORWARD, exact, inside them: the start's remainder pushed along the
//!   edges as points, kept only where `W` says they can still win. The first
//!   frame a point reaches a win node is the optimum for the graph's
//!   precision (exact in the remainder); the path is the witness.
//!
//! Exactness rests on "intersect with the guard, then act" distributing over
//! unions, so a node's set at a frame is the union over every path into it.
//! The graph is frame-independent (a node's transitions do not depend on when
//! it is occupied), so one recorded expansion per node serves every frame;
//! a node is occupied no earlier than the layer it was first reached.

use std::sync::Arc;

use super::arcs::{Action, Region, Transfer};
use rustc_hash::{FxHashMap, FxHashSet};

/// An edge of the graph: `src` -> `dst`, its x and y transfers.
#[derive(Clone, Copy, Debug)]
pub struct Edge {
    pub src: u64,
    pub dst: u64,
    pub x: Transfer,
    pub y: Transfer,
}

/// An out-edge in the dense graph: the target's index and the transfers.
#[derive(Clone, Copy, Debug)]
pub struct Out {
    pub dst: u32,
    pub x: Transfer,
    pub y: Transfer,
}

/// The graph, DENSE: nodes are indices `0..len` (in node-id order), edges
/// compressed by source (`out`) with the reverse adjacency (`preds`) for the
/// backward's change propagation.
pub struct Graph {
    ids: Vec<u64>,
    layer: Vec<u32>,
    /// One past the last frame a node can still win from (0: never; `u32::MAX`:
    /// no bound).
    until: Vec<u32>,
    win: Vec<bool>,
    out_at: Vec<u32>,
    out: Vec<Out>,
    pred_at: Vec<u32>,
    preds: Vec<u32>,
    /// Non-win nodes by deadline (`by_deadline[t]`: deadline `t`).
    by_deadline: Vec<Vec<u32>>,
}

/// Compressed adjacency by key, a counting sort (no comparison sort): `at[i]..at[i + 1]`
/// indexes the result for key `i`; within a key, the input order.
fn csr<I, T>(n: usize, parts: &[Vec<I>], key: impl Fn(&I) -> u32, val: impl Fn(&I) -> T) -> (Vec<u32>, Vec<T>) {
    let mut at = vec![0u32; n + 1];
    for p in parts.iter().flatten() {
        at[key(p) as usize + 1] += 1;
    }
    for i in 0..n {
        at[i + 1] += at[i];
    }
    let total = at[n] as usize;
    let mut fill = at.clone();
    let mut order = vec![(0u32, 0u32); total];
    for (pi, part) in parts.iter().enumerate() {
        for (k, p) in part.iter().enumerate() {
            let slot = &mut fill[key(p) as usize];
            order[*slot as usize] = (pi as u32, k as u32);
            *slot += 1;
        }
    }
    (at, order.into_iter().map(|(pi, k)| val(&parts[pi as usize][k as usize])).collect())
}

impl Graph {
    /// The graph over the nodes `ids` (sorted, unique; an edge's index is
    /// a position in it): `edges` per source index (in parts, as the loader
    /// produced them), `layer(id)` the first frame a node can be occupied,
    /// `deadline` per node the last frame it can still win from (a node
    /// absent from it never wins; `None`: no bound).
    pub fn new(ids: Vec<u64>, edges: Vec<Vec<(u32, Out)>>, layer: impl Fn(u64) -> u32, wins: impl IntoIterator<Item = u64>, deadline: Option<&FxHashMap<u64, u16>>) -> Self {
        let n = ids.len();
        assert!(n < u32::MAX as usize, "{n} nodes do not fit a u32 index");
        debug_assert!(ids.windows(2).all(|w| w[0] < w[1]), "ids sorted and unique");
        let (out_at, out) = csr(n, &edges, |p| p.0, |p| p.1);
        let (pred_at, preds) = csr(n, &edges, |p| p.1.dst, |p| p.0);
        drop(edges);
        let wins: FxHashSet<u64> = wins.into_iter().collect();
        let layer: Vec<u32> = ids.iter().map(|&id| layer(id)).collect();
        let win: Vec<bool> = ids.iter().map(|id| wins.contains(id)).collect();
        let until: Vec<u32> = match deadline {
            None => vec![u32::MAX; n],
            Some(d) => ids.iter().map(|id| d.get(id).map_or(0, |&t| t as u32 + 1)).collect(),
        };
        let mut by_deadline: Vec<Vec<u32>> = Vec::new();
        for i in 0..n {
            if !win[i] && until[i] != 0 && until[i] != u32::MAX {
                let t = until[i] as usize - 1;
                if by_deadline.len() <= t {
                    by_deadline.resize_with(t + 1, Vec::new);
                }
                by_deadline[t].push(i as u32);
            }
        }
        Graph { ids, layer, until, win, out_at, out, pred_at, preds, by_deadline }
    }
    /// From edges over node ids (small graphs: the prototype, tests).
    pub fn from_edges(edges: Vec<Edge>, layer: impl Fn(u64) -> u32, wins: impl IntoIterator<Item = u64>, deadline: Option<&FxHashMap<u64, u16>>) -> Self {
        let wins: Vec<u64> = wins.into_iter().collect();
        let mut ids: Vec<u64> = edges.iter().flat_map(|e| [e.src, e.dst]).chain(wins.iter().copied()).collect();
        ids.sort_unstable();
        ids.dedup();
        let idx = |id: u64| ids.binary_search(&id).expect("an edge's node is indexed") as u32;
        let part: Vec<(u32, Out)> = edges.iter().map(|e| (idx(e.src), Out { dst: idx(e.dst), x: e.x, y: e.y })).collect();
        Graph::new(ids, vec![part], layer, wins, deadline)
    }
    pub fn len(&self) -> usize {
        self.ids.len()
    }
    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }
    pub fn index(&self, id: u64) -> Option<u32> {
        self.ids.binary_search(&id).ok().map(|i| i as u32)
    }
    pub fn id(&self, i: u32) -> u64 {
        self.ids[i as usize]
    }
    pub fn is_win(&self, i: u32) -> bool {
        self.win[i as usize]
    }
    pub fn layer_of(&self, i: u32) -> u32 {
        self.layer[i as usize]
    }
    pub fn out_of(&self, i: u32) -> &[Out] {
        &self.out[self.out_at[i as usize] as usize..self.out_at[i as usize + 1] as usize]
    }
    fn preds_of(&self, i: u32) -> &[u32] {
        &self.preds[self.pred_at[i as usize] as usize..self.pred_at[i as usize + 1] as usize]
    }
    /// Can non-win node `i` be occupied at `t`, and still win from there?
    fn live(&self, i: u32, t: u32) -> bool {
        self.layer[i as usize] <= t && t < self.until[i as usize]
    }
    /// Does node `i` have a set at `t` at all: a win node from its layer on,
    /// any other while live.
    fn present(&self, i: u32, t: u32) -> bool {
        if self.win[i as usize] {
            self.layer[i as usize] <= t
        } else {
            self.live(i, t)
        }
    }
}

/// One frame's winning sets: the nodes (sorted) and their sets (shared
/// with the neighbouring frames where unchanged).
#[derive(Default)]
struct Frame {
    nodes: Vec<u32>,
    sets: Vec<Arc<Region>>,
}

/// The winning sets per frame, `t` in `0..=horizon`.
pub struct Winning {
    frames: Vec<Frame>,
}

impl Winning {
    /// `W_t(i)` (`None` = empty).
    pub fn at(&self, t: u32, i: u32) -> Option<&Region> {
        let f = self.frames.get(t as usize)?;
        f.nodes.binary_search(&i).ok().map(|k| &*f.sets[k])
    }
    /// Every non-empty `W_t(i)` of frame `t`.
    pub fn frame(&self, t: u32) -> impl Iterator<Item = (u32, &Region)> + '_ {
        let f = &self.frames[t as usize];
        f.nodes.iter().copied().zip(f.sets.iter().map(|r| &**r))
    }
}

/// `W_t(i)` from scratch: the union of `i`'s out-edges' preimages of the
/// next frame's sets (`next(dst)`).
fn pull<'a>(g: &Graph, i: u32, next: impl Fn(u32) -> Option<&'a Region>, pieces: &mut Vec<super::arcs::Piece>, scratch: &mut Vec<super::arcs::Seg>) -> Region {
    pieces.clear();
    for e in g.out_of(i) {
        if let Some(r) = next(e.dst) {
            r.pull(e.x, e.y, pieces);
        }
    }
    Region::from_pieces(pieces, scratch)
}

/// BACKWARD: `W_t` for `t = horizon` down to 0. A win node's set is the whole
/// torus from its layer on; every other node's is the union of its
/// out-edges' preimages of `W_{t+1}`, while it is live (occupiable, within
/// its deadline).
///
/// INCREMENTAL: `W_t(i) = W_{t+1}(i)` when `i` is live at both and none of
/// its successors' sets changed between `t + 1` and `t + 2`, so a frame
/// recomputes only the predecessors of the nodes that changed, and the nodes
/// that become live at `t` (their deadline); the rest share the next frame's
/// set. The recomputation is a PULL per node (reads only `W_{t+1}`), in
/// parallel, its contributions made canonical in one sweep.
pub fn backward(g: &Graph, horizon: u32) -> Winning {
    let n = g.len();
    let threads = crate::frame::threads();
    let full = Arc::new(Region::full());
    let mut frames: Vec<Frame> = Vec::with_capacity(horizon as usize + 1);
    let mut top = Frame::default();
    for i in 0..n as u32 {
        if g.win[i as usize] && g.layer[i as usize] <= horizon {
            top.nodes.push(i);
            top.sets.push(full.clone());
        }
    }
    let mut changed: Vec<u32> = top.nodes.clone();
    // `pos[i]`: i's position in the next frame's lists (u32::MAX: no set).
    let mut pos = vec![u32::MAX; n];
    for (k, &i) in top.nodes.iter().enumerate() {
        pos[i as usize] = k as u32;
    }
    frames.push(top);
    let (mut n_cand, mut n_same, mut n_scan) = (0u64, 0u64, 0u64);
    for t in (0..horizon).rev() {
        let next = frames.last().expect("the frame after");
        let mut cand: Vec<u32> = changed.iter().flat_map(|&c| g.preds_of(c).iter().copied()).filter(|&p| !g.win[p as usize] && g.live(p, t)).collect();
        if let Some(fresh) = g.by_deadline.get(t as usize) {
            cand.extend(fresh.iter().copied().filter(|&i| g.live(i, t)));
        }
        cand.sort_unstable();
        cand.dedup();
        n_cand += cand.len() as u64;
        n_scan += cand.iter().map(|&i| g.out_of(i).len() as u64).sum::<u64>();
        // Recompute the candidates.
        const CHUNK: usize = 256;
        let at = std::sync::atomic::AtomicUsize::new(0);
        let lookup = |d: u32| {
            let k = pos[d as usize];
            (k != u32::MAX).then(|| &*next.sets[k as usize])
        };
        let mut parts: Vec<(usize, Vec<Region>)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads)
                .map(|_| {
                    sc.spawn(|| {
                        let (mut out, mut pieces, mut scratch) = (Vec::new(), Vec::new(), Vec::new());
                        loop {
                            let i = at.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if i >= cand.len() {
                                return out;
                            }
                            let rs = cand[i..(i + CHUNK).min(cand.len())].iter().map(|&c| pull(g, c, lookup, &mut pieces, &mut scratch)).collect();
                            out.push((i, rs));
                        }
                    })
                })
                .collect();
            hs.into_iter().flat_map(|h| h.join().expect("a backward worker panicked")).collect()
        });
        parts.sort_unstable_by_key(|p| p.0);
        let mut fresh: Vec<Region> = parts.into_iter().flat_map(|p| p.1).collect();
        // Merge: the next frame's sets that persist, overridden by the
        // recomputed candidates.
        let mut cur = Frame::default();
        let mut new_changed: Vec<u32> = Vec::new();
        let (mut a, mut b) = (0usize, 0usize);
        while a < next.nodes.len() || b < cand.len() {
            let ia = next.nodes.get(a).copied().unwrap_or(u32::MAX);
            let ib = cand.get(b).copied().unwrap_or(u32::MAX);
            if ib < ia || (ib == ia && ib != u32::MAX) {
                let r = std::mem::take(&mut fresh[b]);
                let prev = (ib == ia).then(|| &next.sets[a]);
                if r.is_empty() {
                    if prev.is_some() {
                        new_changed.push(ib);
                    }
                } else if let Some(p) = prev.filter(|p| ***p == r) {
                    n_same += 1;
                    cur.nodes.push(ib);
                    cur.sets.push(p.clone());
                } else {
                    new_changed.push(ib);
                    cur.nodes.push(ib);
                    cur.sets.push(Arc::new(r));
                }
                if ib == ia {
                    a += 1;
                }
                b += 1;
            } else {
                if g.present(ia, t) {
                    cur.nodes.push(ia);
                    cur.sets.push(next.sets[a].clone());
                } else {
                    new_changed.push(ia);
                }
                a += 1;
            }
        }
        for &i in &next.nodes {
            pos[i as usize] = u32::MAX;
        }
        for (k, &i) in cur.nodes.iter().enumerate() {
            pos[i as usize] = k as u32;
        }
        changed = new_changed;
        frames.push(cur);
    }
    eprintln!("[arc] backward: {n_cand} recomputations ({n_same} unchanged), {n_scan} out-edges scanned");
    frames.reverse();
    Winning { frames }
}

/// The EXACT PARTITION of each node's remainders, irrespective of any goal:
/// the cut points pulled back from every future within `horizon` frames - two
/// remainders with no cut between them behave identically on every path
/// (a bisimulation of the remainder). Per frame `t`, per node occupiable at
/// `t`, the cut points of its x and y circles (sorted). A cut is where an
/// edge's guard begins or ends; a rotation carries a successor's cuts back
/// (shifted), a collision's constant carries none. Two axes are partitioned
/// separately (the product partition).
pub fn partition(g: &Graph, horizon: u32) -> Vec<FxHashMap<u32, (Vec<u32>, Vec<u32>)>> {
    let mut out: Vec<FxHashMap<u32, (Vec<u32>, Vec<u32>)>> = (0..=horizon).map(|_| FxHashMap::default()).collect();
    for t in (0..horizon).rev() {
        let (lo, hi) = out.split_at_mut(t as usize + 1);
        let (cur, next) = (&mut lo[t as usize], &hi[0]);
        for src in 0..g.len() as u32 {
            if g.layer_of(src) > t || g.out_of(src).is_empty() {
                continue;
            }
            let (mut xs, mut ys): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
            for e in g.out_of(src) {
                let cuts = |tr: &Transfer, succ: &[u32], acc: &mut Vec<u32>| {
                    if tr.guard.lo != 0 {
                        acc.push(tr.guard.lo);
                    }
                    if let Action::Rotate(v) = tr.action {
                        for &c in succ {
                            let back = apply(Action::Rotate(-v), c);
                            if tr.takes(back) {
                                acc.push(back);
                            }
                        }
                    }
                };
                let (sx, sy) = next.get(&e.dst).map(|(a, b)| (a.as_slice(), b.as_slice())).unwrap_or((&[], &[]));
                cuts(&e.x, sx, &mut xs);
                cuts(&e.y, sy, &mut ys);
            }
            xs.sort_unstable();
            xs.dedup();
            ys.sort_unstable();
            ys.dedup();
            cur.insert(src, (xs, ys));
        }
    }
    out
}

/// The OPTIMUM and a witness: the win frame (from frame 0) and the path to
/// it (node id, remainder point) frame by frame.
pub struct Found {
    pub frame: u32,
    pub path: Vec<(u64, (u32, u32))>,
}

/// The OPTIMUM from node `start` with the remainder point `point`, read off
/// ONE backward: `p` in `W_t(start)` says "a win within `horizon - t`
/// frames", because the graph is the same at every frame but for the
/// layers, and a layer (a node's first-reach frame) never binds on a walk
/// from the start: k steps from it reach only nodes of layer <= k. So the
/// optimum is `horizon - max{t : point in W_t(start)}`.
///
/// The witness is a greedy walk from there: a point inside `W_t(n)` (n not
/// a win) has, by the definition of `W_t`, an out-edge into `W_{t+1}`, and
/// at the horizon only win nodes have sets, so the walk never backtracks;
/// started with the fewest frames left, it wins in exactly that many.
pub fn optimum(g: &Graph, w: &Winning, start: u32, point: (u32, u32), horizon: u32) -> Option<Found> {
    let inside = |t: u32, n: u32, p: (u32, u32)| w.at(t, n).is_some_and(|r| r.contains(p.0, p.1));
    let from = (0..=horizon).rev().find(|&t| inside(t, start, point))?;
    let (mut n, mut p) = (start, point);
    let mut path = vec![(g.id(n), p)];
    for t in from..=horizon {
        if g.is_win(n) {
            assert_eq!(t, horizon, "a win before the fewest frames left: W is not monotone in t");
            return Some(Found { frame: t - from, path });
        }
        let mut step = None;
        for e in g.out_of(n) {
            if !(e.x.takes(p.0) && e.y.takes(p.1)) {
                continue;
            }
            let q = (apply(e.x.action, p.0), apply(e.y.action, p.1));
            if inside(t + 1, e.dst, q) {
                step = Some((e.dst, q));
                if g.is_win(e.dst) {
                    break;
                }
            }
        }
        (n, p) = step.expect("a point inside W_t has an edge into W_{t+1}");
        path.push((g.id(n), p));
    }
    unreachable!("a walk inside W reaches a win by the horizon")
}

/// An action on one point of the circle.
pub fn apply(a: Action, p: u32) -> u32 {
    match a {
        Action::Rotate(v) => (p as i64 + v as i64).rem_euclid(super::arcs::CIRCLE as i64) as u32,
        Action::Const(c) => c,
    }
}

/// Fragmentation of a frame's winning sets: per node its x segments over
/// all slabs (a rectangle count of the canonical form), as (nodes, median,
/// p90, max).
pub fn fragmentation<'a>(sets: impl Iterator<Item = &'a Region>) -> (usize, usize, usize, usize) {
    let mut v: Vec<usize> = sets.map(|r| r.n_segs()).collect();
    if v.is_empty() {
        return (0, 0, 0, 0);
    }
    v.sort_unstable();
    let q = |f: f64| v[((v.len() - 1) as f64 * f) as usize];
    (v.len(), q(0.5), q(0.9), q(1.0))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::arcs::{pieces, point, Seg, Set, CIRCLE};

    /// A one-axis corridor: from node 0, speed 0.6 px a frame; the remainder
    /// below the cut stays on the pixel (node 0 again), above it moves on
    /// (node 1, which wins). The start's remainder decides when.
    fn corridor() -> Graph {
        let v = 39322; // 0.6 px
        let p = pieces(v);
        let both = |g: &Set| Transfer { guard: g.0[0], action: Action::Rotate(v) };
        let still = Transfer { guard: Seg { lo: 0, hi: 1 << 16 }, action: Action::Rotate(0) };
        let edges = vec![
            Edge { src: 0, dst: 0, x: both(&p[0]), y: still },
            Edge { src: 0, dst: 1, x: both(&p[1]), y: still },
        ];
        Graph::from_edges(edges, |_| 0, [1], None)
    }

    #[test]
    fn the_start_remainder_decides_the_frame() {
        let g = corridor();
        let s = g.index(0).unwrap();
        // rem -0.1 + 0.6 crosses at once: a win at frame 1.
        let w = backward(&g, 3);
        let f = optimum(&g, &w, s, (point(-6553), point(0)), 3).expect("a win");
        assert_eq!(f.frame, 1);
        // rem -0.5: -0.5 + 0.6 = 0.1 stays, 0.7 crosses at frame 2.
        let f = optimum(&g, &w, s, (point(-32768), point(0)), 3).expect("a win");
        assert_eq!(f.frame, 2);
        assert_eq!(f.path.len(), 3);
        // Horizon 1 refutes it from -0.5, not from -0.1.
        let w1 = backward(&g, 1);
        assert!(optimum(&g, &w1, s, (point(-32768), point(0)), 1).is_none());
        assert!(optimum(&g, &w1, s, (point(-6553), point(0)), 1).is_some());
    }

    /// The incremental backward against the definition: every live node
    /// recomputed at every frame. Random layered graphs with cycles, random
    /// guards, rotations and collisions, deadlines.
    #[test]
    fn the_incremental_backward_is_the_definition() {
        let mut s = 11u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
        for _ in 0..60 {
            let n = 4 + r(20) as u64;
            let layers: Vec<u32> = (0..n).map(|i| if i == 0 { 0 } else { 1 + r(6) }).collect();
            let mut edges = Vec::new();
            for _ in 0..n * 3 {
                let (a, b) = (r(n as u32) as u64, r(n as u32) as u64);
                let tr = |r: &mut dyn FnMut(u32) -> u32| {
                    let cut = 1 + r(CIRCLE - 1);
                    let guard = match r(3) { 0 => Seg { lo: 0, hi: CIRCLE }, 1 => Seg { lo: 0, hi: cut }, _ => Seg { lo: cut, hi: CIRCLE } };
                    let action = if r(5) == 0 { Action::Const(r(CIRCLE)) } else { Action::Rotate(r(CIRCLE) as i32 - 32768) };
                    Transfer { guard, action }
                };
                let (x, y) = (tr(&mut r), tr(&mut r));
                edges.push(Edge { src: a, dst: b, x, y });
            }
            let wins: Vec<u64> = (1..n).filter(|_| r(4) == 0).collect();
            let horizon = 4 + r(6);
            let deadline: Option<FxHashMap<u64, u16>> = if r(2) == 0 {
                let mut d = FxHashMap::default();
                for i in 0..n {
                    if r(5) != 0 {
                        d.insert(i, r(horizon + 1) as u16);
                    }
                }
                Some(d)
            } else {
                None
            };
            let lay = layers.clone();
            let g = Graph::from_edges(edges, move |id| lay[id as usize], wins, deadline.as_ref());
            let w = backward(&g, horizon);
            // The definition.
            let mut want: Vec<Vec<Option<Region>>> = vec![vec![None; g.len()]; horizon as usize + 1];
            for i in 0..g.len() as u32 {
                if g.is_win(i) && g.layer_of(i) <= horizon {
                    want[horizon as usize][i as usize] = Some(Region::full());
                }
            }
            for t in (0..horizon).rev() {
                for i in 0..g.len() as u32 {
                    let v = if g.is_win(i) {
                        (g.layer_of(i) <= t).then(Region::full)
                    } else if g.live(i, t) {
                        let next = &want[t as usize + 1];
                        Some(pull(&g, i, |d| next[d as usize].as_ref(), &mut Vec::new(), &mut Vec::new())).filter(|r| !r.is_empty())
                    } else {
                        None
                    };
                    want[t as usize][i as usize] = v;
                }
            }
            for t in 0..=horizon {
                for i in 0..g.len() as u32 {
                    assert_eq!(w.at(t, i), want[t as usize][i as usize].as_ref(), "t {t} node {i}");
                }
            }
        }
    }

    /// One backward reads off the optimum: on graphs whose layers are first-
    /// reach frames from the start (as the forward's are), the optimum from
    /// `backward(H)` is the smallest `h` whose own backward holds the start,
    /// for every start remainder.
    #[test]
    fn one_backward_gives_the_optimum() {
        let mut s = 5u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
        let mut checked = 0;
        for _ in 0..80 {
            let n = 3 + r(12) as u64;
            let mut edges = Vec::new();
            for _ in 0..n * 3 {
                let (a, b) = (r(n as u32) as u64, r(n as u32) as u64);
                let cut = 1 + r(CIRCLE - 1);
                let guard = match r(3) { 0 => Seg { lo: 0, hi: CIRCLE }, 1 => Seg { lo: 0, hi: cut }, _ => Seg { lo: cut, hi: CIRCLE } };
                let action = if r(5) == 0 { Action::Const(r(CIRCLE)) } else { Action::Rotate(r(CIRCLE) as i32 - 32768) };
                let x = Transfer { guard, action };
                let y = Transfer { guard: Seg { lo: 0, hi: CIRCLE }, action: Action::Rotate(0) };
                edges.push(Edge { src: a, dst: b, x, y });
            }
            // Layers: BFS distance from node 0; unreachable nodes dropped.
            let mut dist = vec![u32::MAX; n as usize];
            dist[0] = 0;
            for _ in 0..n {
                for e in &edges {
                    if dist[e.src as usize] != u32::MAX && dist[e.src as usize] + 1 < dist[e.dst as usize] {
                        dist[e.dst as usize] = dist[e.src as usize] + 1;
                    }
                }
            }
            edges.retain(|e| dist[e.src as usize] != u32::MAX);
            let wins: Vec<u64> = (1..n).filter(|&i| dist[i as usize] != u32::MAX && r(3) == 0).collect();
            if edges.is_empty() || wins.is_empty() {
                continue;
            }
            let horizon = 8;
            let d = dist.clone();
            let g = Graph::from_edges(edges, move |id| d[id as usize], wins, None);
            let Some(start) = g.index(0) else { continue };
            let w = backward(&g, horizon);
            let ws: Vec<Winning> = (0..=horizon).map(|h| backward(&g, h)).collect();
            for _ in 0..20 {
                let p = (r(CIRCLE), point(0));
                let got = optimum(&g, &w, start, p, horizon).map(|f| f.frame);
                let want = (0..=horizon).find(|&h| ws[h as usize].at(0, start).is_some_and(|r| r.contains(p.0, p.1)));
                assert_eq!(got, want, "start remainder {p:?}");
                if let Some(f) = optimum(&g, &w, start, p, horizon) {
                    assert_eq!(f.path.len() as u32, f.frame + 1);
                    checked += 1;
                }
            }
        }
        assert!(checked > 50, "only {checked} winning starts exercised");
    }
}
