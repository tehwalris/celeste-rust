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

use super::arcs::{Action, Rect, Rects, Set};
use rustc_hash::FxHashMap;

/// One axis of an edge: the remainders that take it and what it does to them.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Transfer {
    pub guard: Set,
    pub action: Action,
}

/// An edge of the graph: `src` -> `dst`, its x and y transfers.
#[derive(Clone, Debug)]
pub struct Edge {
    pub src: u64,
    pub dst: u64,
    pub x: Transfer,
    pub y: Transfer,
}

/// The graph, indexed both ways, with each node's first layer.
pub struct Graph {
    pub by_dst: FxHashMap<u64, Vec<usize>>,
    pub by_src: FxHashMap<u64, Vec<usize>>,
    pub edges: Vec<Edge>,
    /// The frame a node is first reached: it cannot be occupied earlier.
    pub layer: FxHashMap<u64, u32>,
    /// Nodes that win as soon as they are occupied.
    pub wins: Vec<u64>,
}

impl Graph {
    pub fn new(edges: Vec<Edge>, layer: FxHashMap<u64, u32>, wins: Vec<u64>) -> Self {
        let (mut by_dst, mut by_src): (FxHashMap<u64, Vec<usize>>, FxHashMap<u64, Vec<usize>>) = Default::default();
        for (i, e) in edges.iter().enumerate() {
            by_dst.entry(e.dst).or_default().push(i);
            by_src.entry(e.src).or_default().push(i);
        }
        Graph { by_dst, by_src, edges, layer, wins }
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
/// Only a node occupiable at `t` (`layer <= t`) is given a set.
pub fn backward(g: &Graph, horizon: u32) -> Winning {
    let occupiable = |n: u64, t: u32| g.layer.get(&n).is_some_and(|&l| l <= t);
    let mut w: Vec<FxHashMap<u64, Rects>> = (0..=horizon).map(|_| FxHashMap::default()).collect();
    let seed = |m: &mut FxHashMap<u64, Rects>, t: u32| {
        for &n in &g.wins {
            if occupiable(n, t) {
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
        // Every node with a non-empty set at t + 1 pulls its in-edges' sources.
        for (&dst, set) in next.iter() {
            let Some(ins) = g.by_dst.get(&dst) else { continue };
            for &i in ins {
                let e = &g.edges[i];
                if !occupiable(e.src, t) || g.wins.contains(&e.src) {
                    continue;
                }
                let acc = cur.entry(e.src).or_default();
                for r in &set.0 {
                    acc.add(Rect {
                        x: e.x.action.preimage(&r.x).intersect(&e.x.guard),
                        y: e.y.action.preimage(&r.y).intersect(&e.y.guard),
                    });
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
    let occupiable = |n: u64, t: u32| g.layer.get(&n).is_some_and(|&l| l <= t);
    let mut out: Vec<FxHashMap<u64, (Vec<u32>, Vec<u32>)>> = (0..=horizon).map(|_| FxHashMap::default()).collect();
    for t in (0..horizon).rev() {
        let (lo, hi) = out.split_at_mut(t as usize + 1);
        let (cur, next) = (&mut lo[t as usize], &hi[0]);
        for (&src, outs) in &g.by_src {
            if !occupiable(src, t) {
                continue;
            }
            let (mut xs, mut ys): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
            for &i in outs {
                let e = &g.edges[i];
                let cuts = |tr: &Transfer, succ: &[u32], acc: &mut Vec<u32>| {
                    for seg in &tr.guard.0 {
                        if seg.lo != 0 {
                            acc.push(seg.lo);
                        }
                    }
                    if let Action::Rotate(v) = tr.action {
                        for &c in succ {
                            let back = apply(Action::Rotate(-v), c);
                            if tr.guard.contains(back) {
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
            let Some(outs) = g.by_src.get(&n) else { continue };
            for &i in outs {
                let e = &g.edges[i];
                if !(e.x.guard.contains(p.0) && e.y.guard.contains(p.1)) {
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
        let both = |g: &Set| Transfer { guard: g.clone(), action: Action::Rotate(v) };
        let still = Transfer { guard: Set::full(), action: Action::Rotate(0) };
        let edges = vec![
            Edge { src: 0, dst: 0, x: both(&p[0]), y: still.clone() },
            Edge { src: 0, dst: 1, x: both(&p[1]), y: still },
        ];
        let layer: FxHashMap<u64, u32> = [(0, 0), (1, 1)].into_iter().collect();
        Graph::new(edges, layer, vec![1])
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
