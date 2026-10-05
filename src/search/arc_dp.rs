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

/// An out-edge in the dense graph: the target's index and its transfer pair
/// (an index into `Graph::xfers`: a graph has a few thousand distinct pairs
/// and hundreds of millions of edges).
#[derive(Clone, Copy, Debug)]
pub struct Out {
    pub dst: u32,
    pub xfer: u32,
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
    /// The distinct (x, y) transfer pairs the edges index.
    xfers: Vec<(Transfer, Transfer)>,
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
    /// produced them) over the transfer pairs `xfers`, `layer(id)` the first
    /// frame a node can be occupied, `deadline` per node the last frame it
    /// can still win from (a node absent from it never wins; `None`: no
    /// bound).
    pub fn new(
        ids: Vec<u64>,
        edges: Vec<Vec<(u32, Out)>>,
        xfers: Vec<(Transfer, Transfer)>,
        layer: impl Fn(u64) -> u32,
        wins: impl IntoIterator<Item = u64>,
        deadline: Option<&FxHashMap<u64, u16>>,
    ) -> Self {
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
        Graph { ids, layer, until, win, out_at, out, xfers, pred_at, preds, by_deadline }
    }
    /// From edges over node ids (small graphs: the prototype, tests).
    pub fn from_edges(edges: Vec<Edge>, layer: impl Fn(u64) -> u32, wins: impl IntoIterator<Item = u64>, deadline: Option<&FxHashMap<u64, u16>>) -> Self {
        let wins: Vec<u64> = wins.into_iter().collect();
        let mut ids: Vec<u64> = edges.iter().flat_map(|e| [e.src, e.dst]).chain(wins.iter().copied()).collect();
        ids.sort_unstable();
        ids.dedup();
        let idx = |id: u64| ids.binary_search(&id).expect("an edge's node is indexed") as u32;
        let xfers: Vec<(Transfer, Transfer)> = edges.iter().map(|e| (e.x, e.y)).collect();
        let part: Vec<(u32, Out)> = edges.iter().enumerate().map(|(k, e)| (idx(e.src), Out { dst: idx(e.dst), xfer: k as u32 })).collect();
        Graph::new(ids, vec![part], xfers, layer, wins, deadline)
    }
    /// The transfer pair of edge `e`.
    pub fn xfer(&self, e: &Out) -> (Transfer, Transfer) {
        self.xfers[e.xfer as usize]
    }
    /// Drop the adjacency (out-edges, predecessors, transfers): what is left
    /// answers `index`, `id`, `is_win`, `layer_of`.
    pub fn forget_edges(&mut self) {
        for v in [&mut self.out_at, &mut self.pred_at, &mut self.preds] {
            *v = Vec::new();
        }
        self.out = Vec::new();
        self.xfers = Vec::new();
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

/// A level's rotation graph up to a horizon, loaded (`load`).
pub struct Loaded {
    pub graph: Graph,
    /// The start's node index (layer 0, its frame file's row 0).
    pub start: u32,
    /// The remainder-free backward's marks: every id that wins by the
    /// horizon with SOME remainder, with its deadline, by id (the start
    /// always, at deadline 0 if the BFS did not reach it).
    pub marks: Vec<(u64, u16)>,
    /// The win rows of frames `1..=horizon`.
    pub wins: Vec<u64>,
    pub edges: usize,
}

/// The rotation graph of the tree in `dir` up to `horizon`: the
/// remainder-free BFS over the recorded edges (`edges::bfs`) gives the nodes
/// that can win by the horizon at all, and their deadlines; only the edges
/// into them are read (`EdgeGraph::preds_at`, the BFS's lookup), and only
/// those from marked nodes kept - an edge between other nodes is on no path
/// to a win by the horizon with any remainder. Every loaded edge must carry
/// its transfer (the tree was recorded at level 0).
pub fn load(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Loaded> {
    use crate::frame::{frame_files, id_layer, pack_id};
    let t0 = std::time::Instant::now();
    let mut wins: Vec<u64> = Vec::new();
    let mut start_id: Option<u64> = None;
    // Per frame, the rows it kept (its layer).
    let mut kept: Vec<u32> = Vec::with_capacity(horizon as usize + 1);
    for f in 0..=horizon {
        let mut rows = 0u32;
        for (seq, file) in frame_files(dir, f)? {
            rows += file.cell_counts().map(|(_, n)| n).sum::<u32>();
            if f == 0 {
                anyhow::ensure!(start_id.is_none(), "{}: more than one start file", dir.display());
                start_id = Some(pack_id(0, seq, 0));
            } else {
                wins.extend(file.win_rows().iter().map(|&(row, _)| pack_id(f, seq, row)));
            }
        }
        kept.push(rows);
    }
    let start_id = start_id.ok_or_else(|| anyhow::anyhow!("{}: no frame-0 row", dir.display()))?;
    let eg = super::edges::EdgeGraph::open(&dir.join("edges"), horizon)?;
    // A frame that kept rows recorded the edges into them: its own layer's
    // run must be there. A frame that kept none (the level -1 filter can
    // drop every row at the horizon) may have no run at all.
    for (f, &rows) in kept.iter().enumerate().skip(1) {
        let f = f as u32;
        anyhow::ensure!(rows == 0 || eg.has_run(f, f), "{}: no edge run for f{f}, which kept {rows} rows", dir.display());
    }
    let (bfs, _) = super::edges::bfs(&eg, horizon, wins.iter().copied());
    let t_bfs = t0.elapsed();
    // The BFS never marks layer 0 (the start is given): its deadline is 0
    // when a win is exactly tight.
    let mut deadline: FxHashMap<u64, u16> = bfs.with_deadlines().into_iter().collect();
    deadline.entry(start_id).or_insert(0);
    let mut marks: Vec<(u64, u16)> = deadline.iter().map(|(&id, &d)| (id, d)).collect();
    marks.sort_unstable();
    let mut ids: Vec<u64> = marks.iter().map(|&(id, _)| id).chain(wins.iter().copied()).collect();
    ids.sort_unstable();
    ids.dedup();
    let index: FxHashMap<u64, u32> = ids.iter().enumerate().map(|(i, &id)| (id, i as u32)).collect();
    // The frames' transfer tables into one: per frame, its ids' global ones.
    let mut xfers: Vec<(Transfer, Transfer)> = Vec::new();
    let mut global: FxHashMap<super::arc_edges::Pair, u32> = FxHashMap::default();
    let remap: Vec<Vec<u32>> = (0..=horizon)
        .map(|f| {
            eg.pairs(f)
                .iter()
                .map(|p| {
                    *global.entry(*p).or_insert_with(|| {
                        xfers.push((p.0.transfer(), p.1.transfer()));
                        xfers.len() as u32 - 1
                    })
                })
                .collect()
        })
        .collect();
    // Into every node, its edges of frames layer..=horizon from nodes; in
    // parallel over chunks of targets.
    let t1 = std::time::Instant::now();
    let threads = crate::frame::threads().max(1);
    let chunk = ids.len().div_ceil(threads * 8).max(1);
    let next = std::sync::atomic::AtomicUsize::new(0);
    let parts: Vec<anyhow::Result<Vec<(u32, Out)>>> = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                sc.spawn(|| -> anyhow::Result<Vec<(u32, Out)>> {
                    let (mut out, mut buf) = (Vec::new(), Vec::new());
                    loop {
                        let lo = next.fetch_add(chunk, std::sync::atomic::Ordering::Relaxed);
                        if lo >= ids.len() {
                            return Ok(out);
                        }
                        for (k, &target) in ids[lo..(lo + chunk).min(ids.len())].iter().enumerate() {
                            let dst = (lo + k) as u32;
                            for frame in id_layer(target).max(1)..=horizon {
                                buf.clear();
                                eg.preds_at(target, frame, &mut buf);
                                for e in &buf {
                                    let xfer = *remap[frame as usize].get(e.xfer as usize).ok_or_else(|| {
                                        anyhow::anyhow!("{}: an edge at f{frame} into {target:#x} has transfer {} past its frame's table", dir.display(), e.xfer)
                                    })?;
                                    let mut m = e.mask;
                                    while m != 0 {
                                        let lane = m.trailing_zeros() as u64;
                                        m &= m - 1;
                                        let Some(&src) = index.get(&(e.base + lane)) else { continue };
                                        out.push((src, Out { dst, xfer }));
                                    }
                                }
                            }
                        }
                    }
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("an edge loader panicked")).collect()
    });
    let parts: Vec<Vec<(u32, Out)>> = parts.into_iter().collect::<anyhow::Result<_>>()?;
    let edges: usize = parts.iter().map(|p| p.len()).sum();
    drop(index);
    let t_read = t1.elapsed();
    let t2 = std::time::Instant::now();
    let n_xfers = xfers.len();
    let graph = Graph::new(ids, parts, xfers, id_layer, wins.iter().copied(), Some(&deadline));
    eprintln!(
        "[arc] h{horizon}: {} marked nodes, {} win rows, {edges} edges, {n_xfers} transfer pairs; bfs {:.1} s, edges {:.1} s, graph {:.1} s",
        marks.len(),
        wins.len(),
        t_bfs.as_secs_f64(),
        t_read.as_secs_f64(),
        t2.elapsed().as_secs_f64()
    );
    let start = graph.index(start_id).expect("the start is a node");
    Ok(Loaded { graph, start, marks, wins, edges })
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

/// A candidate's set at `t`, against its set at `t + 1`.
enum Recomputed {
    Empty,
    Same,
    New(Arc<Region>),
}

/// `W_t(i)` from scratch: the union of `i`'s out-edges' preimages of the
/// next frame's sets (`next(dst)`).
fn pull<'a>(g: &Graph, i: u32, next: impl Fn(u32) -> Option<&'a Region>, pieces: &mut Vec<super::arcs::Piece>, scratch: &mut Vec<super::arcs::Seg>) -> Region {
    pieces.clear();
    for e in g.out_of(i) {
        if let Some(r) = next(e.dst) {
            let (x, y) = g.xfer(e);
            r.pull(x, y, pieces);
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
    // `stamp[i] == t`: i is already a candidate at t.
    let mut stamp = vec![u32::MAX; n];
    let (mut n_cand, mut n_same, mut n_scan) = (0u64, 0u64, 0u64);
    let (mut d_cand, mut d_pull, mut d_merge) = (std::time::Duration::ZERO, std::time::Duration::ZERO, std::time::Duration::ZERO);
    for t in (0..horizon).rev() {
        let t0 = std::time::Instant::now();
        let next = frames.last().expect("the frame after");
        let mut cand: Vec<u32> = Vec::new();
        let mut take = |p: u32, cand: &mut Vec<u32>| {
            if stamp[p as usize] != t && !g.win[p as usize] && g.live(p, t) {
                stamp[p as usize] = t;
                cand.push(p);
            }
        };
        for &c in &changed {
            for &p in g.preds_of(c) {
                take(p, &mut cand);
            }
        }
        if let Some(fresh) = g.by_deadline.get(t as usize) {
            for &i in fresh {
                take(i, &mut cand);
            }
        }
        cand.sort_unstable();
        n_cand += cand.len() as u64;
        d_cand += t0.elapsed();
        let t0 = std::time::Instant::now();
        // Recompute the candidates, each against its set at t + 1.
        const CHUNK: usize = 256;
        let at = std::sync::atomic::AtomicUsize::new(0);
        let pos_ref = &pos;
        let lookup = |d: u32| {
            let k = pos_ref[d as usize];
            (k != u32::MAX).then(|| &*next.sets[k as usize])
        };
        let mut parts: Vec<(usize, Vec<Recomputed>, u64)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads)
                .map(|_| {
                    sc.spawn(|| {
                        let (mut out, mut pieces, mut scratch) = (Vec::new(), Vec::new(), Vec::new());
                        loop {
                            let i = at.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if i >= cand.len() {
                                return out;
                            }
                            let mut scanned = 0u64;
                            let rs = cand[i..(i + CHUNK).min(cand.len())]
                                .iter()
                                .map(|&c| {
                                    scanned += g.out_of(c).len() as u64;
                                    let r = pull(g, c, lookup, &mut pieces, &mut scratch);
                                    if r.is_empty() {
                                        Recomputed::Empty
                                    } else if lookup(c) == Some(&r) {
                                        Recomputed::Same
                                    } else {
                                        Recomputed::New(Arc::new(r))
                                    }
                                })
                                .collect();
                            out.push((i, rs, scanned));
                        }
                    })
                })
                .collect();
            hs.into_iter().flat_map(|h| h.join().expect("a backward worker panicked")).collect()
        });
        parts.sort_unstable_by_key(|p| p.0);
        n_scan += parts.iter().map(|p| p.2).sum::<u64>();
        let fresh: Vec<Recomputed> = parts.into_iter().flat_map(|p| p.1).collect();
        d_pull += t0.elapsed();
        let t0 = std::time::Instant::now();
        // Merge: the next frame's sets that persist, overridden by the
        // recomputed candidates.
        let mut cur = Frame { nodes: Vec::with_capacity(next.nodes.len()), sets: Vec::with_capacity(next.nodes.len()) };
        let mut new_changed: Vec<u32> = Vec::new();
        let (mut a, mut b) = (0usize, 0usize);
        let mut fresh = fresh.into_iter();
        while a < next.nodes.len() || b < cand.len() {
            let ia = next.nodes.get(a).copied().unwrap_or(u32::MAX);
            let ib = cand.get(b).copied().unwrap_or(u32::MAX);
            if ib <= ia {
                let had = ib == ia;
                match fresh.next().expect("one result per candidate") {
                    Recomputed::Empty => {
                        if had {
                            new_changed.push(ib);
                        }
                    }
                    Recomputed::Same => {
                        n_same += 1;
                        cur.nodes.push(ib);
                        cur.sets.push(next.sets[a].clone());
                    }
                    Recomputed::New(r) => {
                        new_changed.push(ib);
                        cur.nodes.push(ib);
                        cur.sets.push(r);
                    }
                }
                if had {
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
        d_merge += t0.elapsed();
    }
    eprintln!(
        "[arc] backward: {n_cand} recomputations ({n_same} unchanged), {n_scan} out-edges scanned; candidates {:.2} s, pull {:.2} s, merge {:.2} s",
        d_cand.as_secs_f64(),
        d_pull.as_secs_f64(),
        d_merge.as_secs_f64()
    );
    frames.reverse();
    Winning { frames }
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
            let (x, y) = g.xfer(e);
            if !(x.takes(p.0) && y.takes(p.1)) {
                continue;
            }
            let q = (apply(x.action, p.0), apply(y.action, p.1));
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
fn apply(a: Action, p: u32) -> u32 {
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

/// A concrete witness: the inputs, and the player's cell after each (the
/// start's first).
pub struct Witness {
    pub inputs: Vec<u8>,
    pub cells: Vec<u32>,
}

impl Witness {
    /// The inputs as a comma list (`concrete_run -i`, `pico8_diff/replay.py`).
    pub fn inputs_text(&self) -> String {
        self.inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")
    }

    /// `witness.txt`: `inputs ...`, then `f x y` per frame (frame 0 the start).
    pub fn save(&self, path: &std::path::Path) -> anyhow::Result<()> {
        let mut text = format!("inputs {}\n", self.inputs_text());
        for (f, &c) in self.cells.iter().enumerate() {
            match super::pos_graph::cell_xy(c) {
                Some((x, y)) => text += &format!("{f} {x} {y}\n"),
                None => text += &format!("{f} - -\n"),
            }
        }
        Ok(std::fs::write(path, text)?)
    }
}

/// THE CONCRETE SEARCH: the concrete optimum and its inputs, inside the
/// winning sets. A BREADTH-FIRST search over concrete states from the room's
/// start through the reference engine's concrete step (every input, every
/// `rnd` leaf), layer k+1 admitting a successor only if its projection onto
/// `level` is a node of `g` (`dir`'s tree, frames `0..=horizon`) whose
/// `W_{k+1}` holds the successor's exact remainder (a state without a player
/// reads the point (0, 0): no edge cuts it), and holding each EXACT state
/// once (the first in (parent, input, leaf) order: deterministic).
///
/// W_t holds every state that wins within `horizon - t` frames, so every
/// concrete path that wins by the horizon stays inside it: the search is
/// EXHAUSTIVE inside W and prunes by nothing else, and the first layer with a
/// win is the CONCRETE optimum (over every `rnd` draw: a leaf is a
/// possibility). `None`: no concrete win by the horizon. The graph's optimum
/// `bound` is a lower bound on it (the level's widenings over-approximate);
/// layers before it are reported but cannot win.
///
/// Layers are expanded in parallel, one reference engine per worker. (It was
/// a DFS counting up from the bound, one exhaustive pass per frame: room
/// (7,0)'s pass grew 6.5x a frame, 172k steps at f80, 1.29M at f81, and the
/// steps ran on one core.)
pub fn concrete_search(
    dir: &std::path::Path,
    level: crate::interpreter::abstraction::Level,
    horizon: u32,
    g: &Graph,
    w: &Winning,
    bound: u32,
) -> anyhow::Result<Option<Witness>> {
    use crate::concrete::ConcreteEngine;
    use crate::frame::{frame_files, pack_id, widened_keys, wins_of, Block};
    use crate::interpreter::state::State;
    use anyhow::Result;
    use celeste_engine::runtime2::{Rt2, AV};
    // The engines run the cart's `_init` under the GLOBAL level: build them
    // before the level is set (room (5,3) nodiag's level-0 `_init` ended in
    // no state); their frames run at the exact level whatever is set.
    let workers = crate::frame::threads().max(1);
    let engines: Vec<std::sync::Mutex<ConcreteEngine>> = (0..workers).map(|_| ConcreteEngine::new().map(std::sync::Mutex::new)).collect::<Result<_>>()?;
    let initial = engines[0].lock().expect("an engine").initial_state()?;
    crate::interpreter::abstraction::set_level(level);
    let t0 = std::time::Instant::now();
    // (shape, key, cell) -> the node's index, for the graph's nodes.
    let mut node: FxHashMap<(u64, (u64, u64), u32), u32> = FxHashMap::default();
    for f in 0..=horizon {
        for (seq, file) in frame_files(dir, f)? {
            if let Some(rt2) = file.load_all()? {
                let b = Block::from_rt2(rt2);
                let shape = b.rt2().shape_hash;
                let cells = b.positions()?;
                for (r, (k, &c)) in b.keys().iter().zip(&cells).enumerate() {
                    if let Some(i) = g.index(pack_id(f, seq, r as u32)) {
                        node.entry((shape, *k, c)).or_insert(i);
                    }
                }
            }
        }
    }
    eprintln!("[concrete] {} nodes keyed in {:.1} s; {workers} workers; the arc bound f{bound}; rss {:.1} GB", node.len(), t0.elapsed().as_secs_f64(), crate::metrics::current_rss_gb());
    fn rem_of(rt2: &Rt2) -> Result<(u32, u32)> {
        let ids = crate::compiled::ids();
        let Some((cx, cy)) = rt2.player_xy_cells(ids, ids.f_rem) else { return Ok((super::arcs::point(0), super::arcs::point(0))) };
        let raw = |c: usize| -> Result<u32> {
            match rt2.cols[c].at(0) {
                AV::Num(n) => Ok(super::arcs::point(n.as_raw_u32() as i32)),
                other => anyhow::bail!("a concrete remainder expected, got {other:?}"),
            }
        };
        Ok((raw(cx)?, raw(cy)?))
    }
    /// A successor that stays inside W (or wins), in (parent, input, leaf)
    /// order within its worker's chunk.
    struct Succ {
        parent: u32,
        byte: u8,
        cell: u32,
        win: bool,
        exact: (u64, u64),
        st: State,
    }
    let start_cell = Block::from_state(&initial)?.positions()?[0];
    // THE FAST PATH: a depth-first search for a win AT the bound, inside
    // `W_{H - bound + k}` (the bound's frames-left alignment), on one engine,
    // with a step budget. Where the bound is the optimum (no level widening
    // made it lower) it finds the witness in a few hundred steps, where the
    // breadth-first search would expand whole layers. Either way the answer
    // is sound: a win found here is at the bound, and the bound is a lower
    // bound; an exhausted or abandoned DFS proves nothing and the BFS runs.
    {
        struct Dfs<'a> {
            eng: &'a mut ConcreteEngine,
            initial: &'a State,
            node: &'a FxHashMap<(u64, (u64, u64), u32), u32>,
            w: &'a Winning,
            level: crate::interpreter::abstraction::Level,
            from: u32,
            frames: u32,
            dead: FxHashSet<((u64, u64), u32, u32)>,
            path: Vec<(u8, u32)>,
            steps: u64,
        }
        /// `Ok(None)`: out of budget.
        fn dfs(cx: &mut Dfs, st: &State, k: u32) -> Result<Option<bool>> {
            if k >= cx.frames {
                return Ok(Some(false));
            }
            for byte in 0u8..64 {
                for succ in cx.eng.step_all(st, byte, cx.initial)? {
                    cx.steps += 1;
                    if cx.steps > DFS_BUDGET {
                        return Ok(None);
                    }
                    let b = Block::from_state(&succ)?;
                    let cell = b.positions()?[0];
                    if wins_of(b.rt2())?.iter().any(|&x| x) {
                        // The bound is a lower bound: a concrete win before it
                        // would say the arc backward lost a path.
                        anyhow::ensure!(k + 1 == cx.frames, "a concrete win at f{} before the arc bound f{}: the backward lost a path", k + 1, cx.frames);
                        cx.path.push((byte, cell));
                        return Ok(Some(true));
                    }
                    let exact = (b.rt2().clone_block().row_keys_canonical()[0], cell, k + 1);
                    if cx.dead.contains(&exact) {
                        continue;
                    }
                    let (shape, keys, cells) = widened_keys(&b, cx.level)?;
                    let Some(&i) = cx.node.get(&(shape, keys[0], cells[0])) else { continue };
                    let q = rem_of(b.rt2())?;
                    if !cx.w.at(cx.from + k + 1, i).is_some_and(|r| r.contains(q.0, q.1)) {
                        continue;
                    }
                    cx.path.push((byte, cell));
                    match dfs(cx, &succ, k + 1)? {
                        Some(true) => return Ok(Some(true)),
                        None => return Ok(None),
                        Some(false) => {}
                    }
                    cx.path.pop();
                    cx.dead.insert(exact);
                }
            }
            Ok(Some(false))
        }
        /// Concrete steps the fast path may take before it gives up.
        const DFS_BUDGET: u64 = 200_000;
        let t = std::time::Instant::now();
        let mut eng = engines[0].lock().expect("an engine");
        let mut cx = Dfs { eng: &mut eng, initial: &initial, node: &node, w, level, from: horizon - bound, frames: bound, dead: FxHashSet::default(), path: Vec::new(), steps: 0 };
        let found = dfs(&mut cx, &initial, 0)?;
        eprintln!(
            "[concrete] depth-first at the bound f{bound}: {} after {} steps, {:.1} s",
            match found {
                Some(true) => "a win",
                Some(false) => "no win (exhaustive)",
                None => "out of budget",
            },
            cx.steps,
            t.elapsed().as_secs_f64()
        );
        if found == Some(true) {
            let inputs = cx.path.iter().map(|&(b, _)| b).collect();
            let cells = std::iter::once(start_cell).chain(cx.path.iter().map(|&(_, c)| c)).collect();
            return Ok(Some(Witness { inputs, cells }));
        }
    }
    // Per layer k >= 1, each state's (parent index in layer k-1, input, cell).
    let mut back: Vec<Vec<(u32, u8, u32)>> = Vec::new();
    let mut cur: Vec<State> = vec![initial.clone()];
    let mut steps = 0u64;
    for k in 0..horizon {
        let t = std::time::Instant::now();
        let t_w = k + 1;
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        let parts: Vec<Result<(Vec<(usize, Vec<Succ>)>, u64)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, node, initial, next_chunk) = (&cur, &node, &initial, &next_chunk);
                    sc.spawn(move || -> Result<(Vec<(usize, Vec<Succ>)>, u64)> {
                        let mut eng = engine.lock().expect("an engine");
                        let (mut out, mut steps) = (Vec::new(), 0u64);
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, steps));
                            }
                            let mut got = Vec::new();
                            for (p, st) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                for byte in 0u8..64 {
                                    for succ in eng.step_all(st, byte, initial).map_err(|e| e.context(format!("the frame after layer {k} with input {byte}")))? {
                                        steps += 1;
                                        let b = Block::from_state(&succ)?;
                                        let cell = b.positions()?[0];
                                        // The memo key is the EXACT state: `b.keys()` is
                                        // the row key at the search's level (the
                                        // remainder and held buttons widened), and two
                                        // states one key stands for need not share
                                        // their fate (`522de36`).
                                        let exact = b.rt2().clone_block().row_keys_canonical()[0];
                                        let win = wins_of(b.rt2())?.iter().any(|&x| x);
                                        if !win {
                                            let (shape, keys, cells) = widened_keys(&b, level)?;
                                            let Some(&i) = node.get(&(shape, keys[0], cells[0])) else { continue };
                                            let q = rem_of(b.rt2())?;
                                            if !w.at(t_w, i).is_some_and(|r| r.contains(q.0, q.1)) {
                                                continue;
                                            }
                                        }
                                        got.push(Succ { parent: p as u32, byte, cell, win, exact, st: succ });
                                    }
                                }
                            }
                            out.push((lo, got));
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a concrete worker panicked")).collect()
        });
        let mut chunks: Vec<(usize, Vec<Succ>)> = Vec::new();
        for p in parts {
            let (c, s) = p?;
            chunks.extend(c);
            steps += s;
        }
        chunks.sort_unstable_by_key(|c| c.0);
        let succs = chunks.into_iter().flat_map(|c| c.1);
        // The layer: each exact state once, the first in (parent, input,
        // leaf) order; the first win ends the search.
        let mut seen: FxHashSet<((u64, u64), u32)> = FxHashSet::default();
        let (mut next, mut links): (Vec<State>, Vec<(u32, u8, u32)>) = (Vec::new(), Vec::new());
        let mut won: Option<(u32, u8, u32)> = None;
        for s in succs {
            if s.win {
                won = Some((s.parent, s.byte, s.cell));
                break;
            }
            if seen.insert((s.exact, s.cell)) {
                next.push(s.st);
                links.push((s.parent, s.byte, s.cell));
            }
        }
        eprintln!(
            "[concrete] layer {:3}: {} states -> {} inside W{}; {steps} steps so far, {:.1} s, rss {:.1} GB",
            k + 1,
            cur.len(),
            next.len(),
            if won.is_some() { ", A WIN" } else { "" },
            t.elapsed().as_secs_f64(),
            crate::metrics::current_rss_gb()
        );
        if let Some((parent, byte, cell)) = won {
            anyhow::ensure!(k + 1 >= bound, "a concrete win at f{} before the arc bound f{bound}: the backward lost a path", k + 1);
            // The path, back through the layers.
            let (mut inputs, mut cells) = (vec![byte], vec![cell]);
            let mut p = parent;
            for layer in back.iter().rev() {
                let (pp, b, c) = layer[p as usize];
                inputs.push(b);
                cells.push(c);
                p = pp;
            }
            inputs.reverse();
            cells.push(start_cell);
            cells.reverse();
            eprintln!("[concrete] a win at f{} after {steps} concrete steps, {:.1} s", inputs.len(), t0.elapsed().as_secs_f64());
            return Ok(Some(Witness { inputs, cells }));
        }
        if next.is_empty() {
            println!("NO CONCRETE WIN by f{horizon}: no concrete state inside W past layer {k} ({steps} steps)");
            return Ok(None);
        }
        back.push(links);
        cur = next;
    }
    println!("NO CONCRETE WIN by f{horizon} ({steps} steps)");
    Ok(None)
}

/// What the arc phase of a search found (`solve`).
pub struct Solved {
    /// The optimum over the rotation graph (exact in the remainder; a lower
    /// bound when the level's objects are coarse). `None`: no win by the
    /// horizon at all - the horizon is REFUTED.
    pub arc: Option<u32>,
    /// The concrete optimum and its witness (`concrete_search`), when
    /// asked for and found.
    pub concrete: Option<Witness>,
}

/// THE ARC PHASE over a finished level-0 tree in `dir` (frames and edges
/// through `horizon`): load the rotation graph (`load`), the winning sets
/// backward (`backward`), the optimum read off them (`optimum`), and with
/// `witness` the concrete search (`concrete_search`). Prints `[gate]` lines - the
/// marks' and each frame's winning sets' fingerprints over (shape, key,
/// cell), which do not depend on the scheduling the ids do - and with
/// `save`, writes the UI's arc pass there (`export-ui --forward-only --arc`):
/// the remainder-free marks (`level0.marks.bin`), the ARC-MARKED nodes
/// (`arc.marks.bin`: a node whose winning set is non-empty at some frame),
/// both with `dist` the horizon minus the last frame the node still wins
/// from, and `arc.txt`.
pub fn solve(
    dir: &std::path::Path,
    level: crate::interpreter::abstraction::Level,
    horizon: u32,
    witness: bool,
    save: Option<&std::path::Path>,
) -> anyhow::Result<Solved> {
    use crate::frame::{frame_files, id_layer, Visited};
    use celeste_engine::runtime2::mix64;
    let ld = load(dir, horizon)?;
    let g = &ld.graph;
    let tb = std::time::Instant::now();
    let w = backward(g, horizon);
    let tb = tb.elapsed().as_secs_f64();
    for t in (0..horizon).rev().step_by(4) {
        let (n, med, p90, max) = fragmentation(w.frame(t).filter(|&(i, _)| !g.is_win(i)).map(|(_, r)| r));
        eprintln!("[arc] W frame {t:3}: {n} non-win nodes can win; rectangles per node med {med} p90 {p90} max {max}");
    }
    // The start has no player yet (the spawn): its remainder is no input of
    // anything; any point stands for it.
    let p0 = (super::arcs::point(0), super::arcs::point(0));
    let found = optimum(g, &w, ld.start, p0, horizon);
    let arc = found.as_ref().map(|f| f.frame);
    eprintln!("[arc] backward {tb:.2} s; optimum {arc:?}");
    // The nodes as (shape, key, cell), for the fingerprints and the marks.
    let mut files = Vec::new();
    for f in 0..=horizon {
        files.extend(frame_files(dir, f)?.into_iter().map(|(seq, file)| (f, seq, file)));
    }
    let mut node_key: Vec<u64> = Vec::with_capacity(g.len());
    let all: Vec<(u64, u16)> = (0..g.len() as u32).map(|i| (g.id(i), 0)).collect();
    super::edges::resolve_ids(&files, &all, |shape, key, cell, _| node_key.push(mix64(shape ^ mix64(key.0 ^ mix64(key.1 ^ (cell as u64) << 1)))))?;
    let mut marked = Visited::new();
    super::edges::resolve_ids(&files, &ld.marks, |shape, key, cell, d| {
        marked.insert_until(shape, key, cell, d);
    })?;
    let (n, fp) = marked.fingerprint();
    println!("[gate] h{horizon} marks {n} {fp:016x}");
    for t in 0..=horizon {
        let (mut n, mut acc) = (0usize, 0u64);
        for (i, r) in w.frame(t) {
            let mut h = node_key[i as usize];
            for (y, xs) in r.slabs() {
                h = mix64(h ^ ((y.lo as u64) << 32 | y.hi as u64));
                for x in xs {
                    h = mix64(h ^ ((x.lo as u64) << 32 | x.hi as u64));
                }
            }
            acc = acc.wrapping_add(h);
            n += 1;
        }
        println!("[gate] h{horizon} W f{t:03} {n} {acc:016x}");
    }
    println!("[gate] h{horizon} arc optimum {}", arc.map_or("none".to_string(), |f| f.to_string()));
    if let Some(out) = save {
        std::fs::create_dir_all(out)?;
        marked.save(&out.join("level0.marks.bin"), horizon)?;
        // Per node the LAST frame its winning set is non-empty: the arc
        // analogue of the BFS's deadline.
        let mut last = vec![u32::MAX; g.len()];
        for t in 0..=horizon {
            for (i, _) in w.frame(t) {
                last[i as usize] = t;
            }
        }
        let ids: Vec<(u64, u16)> = (0..g.len() as u32).filter(|&i| last[i as usize] != u32::MAX).map(|i| (g.id(i), last[i as usize] as u16)).collect();
        let mut am = Visited::new();
        super::edges::resolve_ids(&files, &ids, |shape, key, cell, d| {
            am.insert_until(shape, key, cell, d);
        })?;
        am.save(&out.join("arc.marks.bin"), horizon)?;
        let first_win = ld.wins.iter().map(|&id| id_layer(id)).min();
        let show = |v: Option<u32>| v.map_or("none".to_string(), |f| f.to_string());
        std::fs::write(out.join("arc.txt"), format!("horizon {horizon}\nlevel {level}\nlevel0_first_win {}\noptimal {}\n", show(first_win), show(arc)))?;
    }
    drop(files);
    // The concrete search reads W and the node index, not the edges: they
    // go first (room (4,3) nodiag h111: 769M edges, 9.2 GB of adjacency).
    let mut graph = ld.graph;
    graph.forget_edges();
    let concrete = match (witness, arc) {
        (true, Some(f)) => concrete_search(dir, level, horizon, &graph, &w, f)?,
        _ => None,
    };
    if let Some(wt) = &concrete {
        println!("[gate] h{horizon} concrete optimum {} inputs {}", wt.inputs.len(), wt.inputs_text());
        if let Some(out) = save {
            wt.save(&out.join("witness.txt"))?;
        }
    }
    Ok(Solved { arc, concrete })
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
