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

use super::arcs::{Action, Rect, Rects, Seg, Set};
use rustc_hash::{FxHashMap, FxHashSet};

/// One axis of an edge: the remainders that take it (one piece of the
/// circle: a guard never wraps - the pieces are cut at the wrap) and what it
/// does to them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Transfer {
    pub guard: Seg,
    pub action: Action,
}

impl Transfer {
    pub fn guard_set(&self) -> Set {
        Set::seg(self.guard.lo, self.guard.hi)
    }
    pub fn takes(&self, p: u32) -> bool {
        self.guard.lo <= p && p < self.guard.hi
    }
}

/// An edge of the graph: `src` -> `dst`, its x and y transfers.
#[derive(Clone, Copy, Debug)]
pub struct Edge {
    pub src: u64,
    pub dst: u64,
    pub x: Transfer,
    pub y: Transfer,
}

/// The graph: its edges in two sorted orders, each node's first layer, the
/// wins, and optionally each node's DEADLINE (the last frame it can still
/// win from - the remainder-free backward's; past it W is empty).
pub struct Graph {
    pub edges: Vec<Edge>,
    by_dst: Vec<u32>,
    dst_at: FxHashMap<u64, (u32, u32)>,
    by_src: Vec<u32>,
    src_at: FxHashMap<u64, (u32, u32)>,
    layer: Box<dyn Fn(u64) -> u32 + Sync>,
    pub wins: FxHashSet<u64>,
    deadline: Option<FxHashMap<u64, u16>>,
}

fn ranges(order: &[u32], key: impl Fn(u32) -> u64) -> FxHashMap<u64, (u32, u32)> {
    let mut m: FxHashMap<u64, (u32, u32)> = FxHashMap::default();
    let mut i = 0usize;
    while i < order.len() {
        let k = key(order[i]);
        let mut j = i;
        while j < order.len() && key(order[j]) == k {
            j += 1;
        }
        m.insert(k, (i as u32, j as u32));
        i = j;
    }
    m
}

impl Graph {
    pub fn new(edges: Vec<Edge>, layer: Box<dyn Fn(u64) -> u32 + Sync>, wins: impl IntoIterator<Item = u64>, deadline: Option<FxHashMap<u64, u16>>) -> Self {
        let mut by_dst: Vec<u32> = (0..edges.len() as u32).collect();
        by_dst.sort_unstable_by_key(|&i| edges[i as usize].dst);
        let mut by_src: Vec<u32> = (0..edges.len() as u32).collect();
        by_src.sort_unstable_by_key(|&i| edges[i as usize].src);
        let dst_at = ranges(&by_dst, |i| edges[i as usize].dst);
        let src_at = ranges(&by_src, |i| edges[i as usize].src);
        Graph { edges, by_dst, dst_at, by_src, src_at, layer, wins: wins.into_iter().collect(), deadline }
    }
    fn into(&self, n: u64) -> impl Iterator<Item = &Edge> {
        let (a, b) = self.dst_at.get(&n).copied().unwrap_or((0, 0));
        self.by_dst[a as usize..b as usize].iter().map(move |&i| &self.edges[i as usize])
    }
    fn out_of(&self, n: u64) -> impl Iterator<Item = &Edge> {
        let (a, b) = self.src_at.get(&n).copied().unwrap_or((0, 0));
        self.by_src[a as usize..b as usize].iter().map(move |&i| &self.edges[i as usize])
    }
    /// Can `n` be occupied at `t`, and still win from there?
    fn live(&self, n: u64, t: u32) -> bool {
        (self.layer)(n) <= t && self.deadline.as_ref().is_none_or(|d| d.get(&n).is_some_and(|&dl| t <= dl as u32))
    }
    pub fn layer_of(&self, n: u64) -> u32 {
        (self.layer)(n)
    }
    pub fn sources(&self) -> impl Iterator<Item = u64> + '_ {
        self.src_at.keys().copied()
    }
}

/// The torus.
fn whole() -> Rect {
    Rect { x: Set::full(), y: Set::full() }
}

/// The winning sets per frame: `w[t]` maps a node to `W_t(node)` (absent =
/// empty), for `t` in `0..=horizon`.
pub struct Winning {
    pub w: Vec<FxHashMap<u64, Rects>>,
}

impl Winning {
    pub fn at(&self, t: u32, n: u64) -> Option<&Rects> {
        self.w.get(t as usize).and_then(|m| m.get(&n))
    }
}

/// BACKWARD: `W_t` for `t = horizon` down to 0. At `horizon` only the win
/// nodes (occupied then, they win by it); below, a win node is the whole
/// torus again and every other node collects its out-edges' contributions.
/// Only a node live at `t` (occupiable, within its deadline) gets a set.
pub fn backward(g: &Graph, horizon: u32) -> Winning {
    let mut w: Vec<FxHashMap<u64, Rects>> = (0..=horizon).map(|_| FxHashMap::default()).collect();
    let seed = |m: &mut FxHashMap<u64, Rects>, t: u32| {
        for &n in &g.wins {
            if (g.layer)(n) <= t {
                let mut r = Rects::empty();
                r.add(whole());
                m.insert(n, r);
            }
        }
    };
    seed(&mut w[horizon as usize], horizon);
    for t in (0..horizon).rev() {
        let (lo, hi) = w.split_at_mut(t as usize + 1);
        let (cur, next) = (&mut lo[t as usize], &hi[0]);
        seed(cur, t);
        for (&dst, set) in next.iter() {
            for e in g.into(dst) {
                if g.wins.contains(&e.src) || !g.live(e.src, t) {
                    continue;
                }
                let (gx, gy) = (e.x.guard_set(), e.y.guard_set());
                let acc = cur.entry(e.src).or_default();
                for r in &set.0 {
                    acc.add(Rect { x: e.x.action.preimage(&r.x).intersect(&gx), y: e.y.action.preimage(&r.y).intersect(&gy) });
                }
            }
        }
        cur.retain(|_, r| !r.is_empty());
    }
    Winning { w }
}

/// The EXACT PARTITION of each node's remainders, irrespective of any goal:
/// the cut points pulled back from every future within `horizon` frames - two
/// remainders with no cut between them behave identically on every path
/// (a bisimulation of the remainder). Per frame `t`, per node occupiable at
/// `t`, the cut points of its x and y circles (sorted). A cut is where an
/// edge's guard begins or ends; a rotation carries a successor's cuts back
/// (shifted), a collision's constant carries none. Two axes are partitioned
/// separately (the product partition).
pub fn partition(g: &Graph, horizon: u32) -> Vec<FxHashMap<u64, (Vec<u32>, Vec<u32>)>> {
    let mut out: Vec<FxHashMap<u64, (Vec<u32>, Vec<u32>)>> = (0..=horizon).map(|_| FxHashMap::default()).collect();
    for t in (0..horizon).rev() {
        let (lo, hi) = out.split_at_mut(t as usize + 1);
        let (cur, next) = (&mut lo[t as usize], &hi[0]);
        for src in g.sources().collect::<Vec<_>>() {
            if g.layer_of(src) > t {
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

/// The FORWARD's result: the first frame a win node is occupied, and the
/// path to it (node, remainder point) frame by frame.
pub struct Found {
    pub frame: u32,
    pub path: Vec<(u64, (u32, u32))>,
}

/// FORWARD, exact, inside `W`: from `start` with the remainder point
/// `point` at frame 0. A point at a node at frame `t` is kept only if it lies
/// in `W_t(node)` - it can still win by the horizon - so the sets stay what the
/// optimum needs.
pub fn forward(g: &Graph, w: &Winning, start: u64, point: (u32, u32), horizon: u32) -> Option<Found> {
    type Points = FxHashMap<(u64, (u32, u32)), Option<(u64, (u32, u32))>>;
    let inside = |t: u32, n: u64, p: (u32, u32)| w.at(t, n).is_some_and(|r| r.contains(p.0, p.1));
    if !inside(0, start, point) {
        return None;
    }
    // Per frame: (node, point) -> its parent at the frame before.
    let mut frames: Vec<Points> = vec![Points::default()];
    frames[0].insert((start, point), None);
    for t in 0..=horizon {
        if let Some((&(n, p), _)) = frames[t as usize].iter().find(|((n, _), _)| g.wins.contains(n)) {
            let mut path = vec![(n, p)];
            let (mut cur, mut tt) = ((n, p), t as usize);
            while let Some(Some(parent)) = frames[tt].get(&cur) {
                path.push(*parent);
                cur = *parent;
                tt -= 1;
            }
            path.reverse();
            return Some(Found { frame: t, path });
        }
        if t == horizon {
            break;
        }
        let mut next = Points::default();
        for &(n, p) in frames[t as usize].keys() {
            for e in g.out_of(n) {
                if !(e.x.takes(p.0) && e.y.takes(p.1)) {
                    continue;
                }
                let q = (apply(e.x.action, p.0), apply(e.y.action, p.1));
                if inside(t + 1, e.dst, q) {
                    next.entry((e.dst, q)).or_insert(Some((n, p)));
                }
            }
        }
        frames.push(next);
    }
    None
}

/// An action on one point of the circle.
pub fn apply(a: Action, p: u32) -> u32 {
    match a {
        Action::Rotate(v) => (p as i64 + v as i64).rem_euclid(super::arcs::CIRCLE as i64) as u32,
        Action::Const(c) => c,
    }
}

/// Fragmentation of a winning-set map: per node the rectangles, as
/// (nodes, median, p90, max).
pub fn fragmentation(m: &FxHashMap<u64, Rects>) -> (usize, usize, usize, usize) {
    let mut v: Vec<usize> = m.values().map(|r| r.0.len()).collect();
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
    use crate::search::arcs::{pieces, point};

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
        Graph::new(edges, Box::new(|n| n as u32), [1], None)
    }

    #[test]
    fn the_start_remainder_decides_the_frame() {
        let g = corridor();
        // rem -0.1 + 0.6 crosses at once: a win at frame 1.
        let w = backward(&g, 3);
        let f = forward(&g, &w, 0, (point(-6553), point(0)), 3).expect("a win");
        assert_eq!(f.frame, 1);
        // rem -0.5: -0.5 + 0.6 = 0.1 stays, 0.7 crosses at frame 2.
        let f = forward(&g, &w, 0, (point(-32768), point(0)), 3).expect("a win");
        assert_eq!(f.frame, 2);
        // Horizon 1 refutes it from -0.5, not from -0.1.
        let w1 = backward(&g, 1);
        assert!(forward(&g, &w1, 0, (point(-32768), point(0)), 1).is_none());
        assert!(forward(&g, &w1, 0, (point(-6553), point(0)), 1).is_some());
    }
}
