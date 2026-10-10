//! The rotation graph's dynamic programme (plans/architecture.md "Arcs"):
//! over the remainder-free transition graph whose edges carry per axis a
//! guard and an action (`search::arcs`), compute
//!
//! * BACKWARD, the winning sets `W_t(n)`: the remainders from which node `n`,
//!   occupied at frame `t`, wins by the horizon
//!   (`W_t(n) = U_e guard_e  n  action_e^-1( W_{t+1}(dst_e) )`, a win node's
//!   set the whole torus), and
//! * FORWARD, exact, inside them: the start's remainder pushed along the
//!   edges as points, kept only where `W` says they can still win.
//!
//! Exact because "intersect with the guard, then act" distributes over
//! unions. The graph is frame-independent, so one recorded expansion per node
//! serves every frame; a node is occupied no earlier than its layer.

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

/// An out-edge in the dense graph: the target's index and its transfer
/// pair's index into `Graph::xfers` (few distinct pairs, many edges).
#[derive(Clone, Copy, Debug)]
pub struct Out {
    pub dst: u32,
    pub xfer: u32,
}

/// The graph, DENSE: nodes are indices `0..len` in id order, edges
/// compressed by source, plus the reverse adjacency for the backward.
pub struct Graph {
    ids: Vec<u64>,
    layer: Vec<u32>,
    /// One past the last frame a node can still win from (0: never; `u32::MAX`:
    /// no bound).
    until: Vec<u32>,
    win: Vec<bool>,
    out_at: Vec<u64>,
    out: Vec<Out>,
    /// The distinct (x, y) transfer pairs the edges index.
    xfers: Vec<(Transfer, Transfer)>,
    pred_at: Vec<u64>,
    preds: Vec<u32>,
    /// Non-win nodes by deadline (`by_deadline[t]`: deadline `t`).
    by_deadline: Vec<Vec<u32>>,
}

/// Compressed adjacency by key (a counting sort): `at[i]..at[i + 1]` indexes
/// key `i`'s values, in input order.
fn csr<I, T>(n: usize, items: &[I], key: impl Fn(&I) -> u32, val: impl Fn(&I) -> T) -> (Vec<u64>, Vec<T>) {
    let mut at = vec![0u64; n + 1];
    for p in items {
        at[key(p) as usize + 1] += 1;
    }
    for i in 0..n {
        at[i + 1] += at[i];
    }
    let mut fill = at.clone();
    let mut order = vec![0u32; items.len()];
    for (k, p) in items.iter().enumerate() {
        let slot = &mut fill[key(p) as usize];
        order[*slot as usize] = k as u32;
        *slot += 1;
    }
    (at, order.into_iter().map(|k| val(&items[k as usize])).collect())
}

/// The adjacency of a graph (`Graph::from_csr`): `out_at`/`out` by source,
/// `pred_at`/`preds` by target, the same edges.
struct Csr {
    out_at: Vec<u64>,
    out: Vec<Out>,
    pred_at: Vec<u64>,
    preds: Vec<u32>,
}

impl Graph {
    /// The graph over `ids` (sorted, unique) and its adjacency: `layer(id)`
    /// the first frame a node can be occupied, `win` the win nodes, `until`
    /// one past the last frame each can still win from (0: never;
    /// `u32::MAX`: no bound).
    fn from_csr(ids: Vec<u64>, adj: Csr, xfers: Vec<(Transfer, Transfer)>, layer: Vec<u32>, win: Vec<bool>, until: Vec<u32>) -> Self {
        let n = ids.len();
        assert!(n < u32::MAX as usize, "{n} nodes do not fit a u32 index");
        debug_assert!(ids.windows(2).all(|w| w[0] < w[1]), "ids sorted and unique");
        assert!(adj.out_at.len() == n + 1 && adj.pred_at.len() == n + 1 && win.len() == n && until.len() == n && layer.len() == n);
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
        let Csr { out_at, out, pred_at, preds } = adj;
        Graph { ids, layer, until, win, out_at, out, xfers, pred_at, preds, by_deadline }
    }
    /// From edges over node ids (small graphs: the prototype, tests).
    pub fn from_edges(edges: Vec<Edge>, layer: impl Fn(u64) -> u32, wins: impl IntoIterator<Item = u64>, deadline: Option<&FxHashMap<u64, u16>>) -> Self {
        let wins: Vec<u64> = wins.into_iter().collect();
        let mut ids: Vec<u64> = edges.iter().flat_map(|e| [e.src, e.dst]).chain(wins.iter().copied()).collect();
        ids.sort_unstable();
        ids.dedup();
        let n = ids.len();
        let idx = |id: u64| ids.binary_search(&id).expect("an edge's node is indexed") as u32;
        let xfers: Vec<(Transfer, Transfer)> = edges.iter().map(|e| (e.x, e.y)).collect();
        let part: Vec<(u32, Out)> = edges.iter().enumerate().map(|(k, e)| (idx(e.src), Out { dst: idx(e.dst), xfer: k as u32 })).collect();
        let (out_at, out) = csr(n, &part, |p| p.0, |p| p.1);
        let (pred_at, preds) = csr(n, &part, |p| p.1.dst, |p| p.0);
        let wins: FxHashSet<u64> = wins.into_iter().collect();
        let win: Vec<bool> = ids.iter().map(|id| wins.contains(id)).collect();
        let until: Vec<u32> = match deadline {
            None => vec![u32::MAX; n],
            Some(d) => ids.iter().map(|id| d.get(id).map_or(0, |&t| t as u32 + 1)).collect(),
        };
        let layer = ids.iter().map(|&id| layer(id)).collect();
        Graph::from_csr(ids, Csr { out_at, out, pred_at, preds }, xfers, layer, win, until)
    }
    /// The transfer pair of edge `e`.
    pub fn xfer(&self, e: &Out) -> (Transfer, Transfer) {
        self.xfers[e.xfer as usize]
    }
    /// Drop the adjacency; `index`, `id`, `is_win`, `layer_of` still work.
    pub fn forget_edges(&mut self) {
        self.out_at = Vec::new();
        self.pred_at = Vec::new();
        self.preds = Vec::new();
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
    /// Does node `i` have a set at `t`: a win node from its layer on, any
    /// other while live.
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
    /// The remainder-free BFS's marks, sorted by id: every id that wins by
    /// the horizon with SOME remainder, with its deadline (the start always).
    /// They are the graph's nodes, in its order: node `i` is `marks[i]`.
    pub marks: Vec<(u64, u16)>,
    /// The win rows of frames `1..=horizon`, with their layers.
    pub wins: Vec<(u64, u32)>,
    pub edges: usize,
}

/// Run `f` over the nodes `0..n` in parallel chunks (`f(lo, hi)`), collecting
/// its results in chunk order.
fn par_chunks<T: Send>(n: usize, f: impl Fn(usize, usize) -> anyhow::Result<T> + Sync) -> anyhow::Result<Vec<T>> {
    let threads = crate::frame::threads().max(1);
    let chunk = n.div_ceil(threads * 8).max(1);
    let next = std::sync::atomic::AtomicUsize::new(0);
    let mut parts: Vec<(usize, T)> = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                sc.spawn(|| -> anyhow::Result<Vec<(usize, T)>> {
                    let mut out = Vec::new();
                    loop {
                        let lo = next.fetch_add(chunk, std::sync::atomic::Ordering::Relaxed);
                        if lo >= n {
                            return Ok(out);
                        }
                        out.push((lo, f(lo, (lo + chunk).min(n))?));
                    }
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("a graph loader panicked")).collect::<anyhow::Result<Vec<_>>>()
    })?
    .into_iter()
    .flatten()
    .collect();
    parts.sort_unstable_by_key(|p| p.0);
    Ok(parts.into_iter().map(|p| p.1).collect())
}

/// Frames `0..=horizon` of the tree in `dir`, each its files `(seq, file)`.
/// A forward whose frontier died (a finer level of the objects ladder,
/// filtered down to nothing) has no frames past the empty one: they are
/// empty too. Any other missing frame is an error.
fn tree_frames(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Vec<Vec<(u32, crate::search::checkpoint::FrameFile)>>> {
    let mut out: Vec<Vec<(u32, crate::search::checkpoint::FrameFile)>> = Vec::with_capacity(horizon as usize + 1);
    for f in 0..=horizon {
        if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
            let died = out.last().is_some_and(|last| last.iter().all(|(_, file)| file.width() == 0));
            anyhow::ensure!(died, "{}: no frame f{f}, and the frontier before it was not empty", dir.display());
            out.push(Vec::new());
            continue;
        }
        out.push(crate::frame::frame_files(dir, f)?);
    }
    Ok(out)
}

/// The rotation graph of the tree in `dir` up to `horizon`, restricted to the
/// nodes the remainder-free BFS (`edges::bfs`) marks: an edge between other
/// nodes is on no path to a win with any remainder. Every loaded edge must
/// carry its transfer.
///
/// MEMORY: the nodes are the BFS's marks, numbered by their rank in its own
/// masks (`storage::marks::MarkRanks`), and the edges are read TWICE, unit
/// by unit as recorded - a pass counting each node's in- and out-degree,
/// then a pass filling the two adjacencies in place - so no edge list is
/// ever held beside the graph: 12 B an edge, the graph's own (room (2,3)
/// gemskip h137, 373M edges: 28 B an edge and a 12.1 GB peak before).
pub fn load(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Loaded> {
    use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
    let t0 = std::time::Instant::now();
    let mut wins: Vec<(u64, u32)> = Vec::new();
    let mut start_id: Option<u64> = None;
    // Per frame, the rows it kept (its layer).
    let mut kept: Vec<u32> = Vec::with_capacity(horizon as usize + 1);
    for (f, files) in tree_frames(dir, horizon)?.into_iter().enumerate() {
        let f = f as u32;
        let mut rows = 0u32;
        for (_, file) in files {
            rows += file.width();
            if f == 0 {
                anyhow::ensure!(start_id.is_none() && file.width() == 1, "{}: more than one start row", dir.display());
                start_id = Some(file.id_at(0));
            } else {
                wins.extend(file.win_rows().iter().map(|&(row, _)| (file.id_at(row), f)));
            }
        }
        kept.push(rows);
    }
    let start_id = start_id.ok_or_else(|| anyhow::anyhow!("{}: no frame-0 row", dir.display()))?;
    let eg = crate::storage::edges::EdgeStore::open(&dir.join("edges"), horizon)?;
    crate::metrics::mem_phase("arc: edge files opened");
    // A frame that kept rows must have its edges; one that kept none (the
    // level -1 filter) may have none.
    for (f, &rows) in kept.iter().enumerate().skip(1) {
        anyhow::ensure!(rows == 0 || eg.has_frame(f as u32), "{}: no edges for f{f}, which kept {rows} rows", dir.display());
    }
    let (mut bfs, _) = super::edges::bfs(&eg, horizon, wins.iter().copied());
    let t_bfs = t0.elapsed();
    // The BFS never marks layer 0: the start, deadline 0. The wins are
    // seeds, marked; so the marks are the nodes.
    bfs.insert(start_id, 0, 0);
    let (ranks, members) = bfs.into_ranked();
    let n = members.len();
    anyhow::ensure!(n < u32::MAX as usize, "{n} nodes do not fit a u32 index");
    let marks: Vec<(u64, u16)> = members.iter().map(|&(id, d, _)| (id, d)).collect();
    let layers: Vec<u32> = members.iter().map(|m| m.2).collect();
    drop(members);
    crate::metrics::mem_phase("arc: bfs, node numbers");
    // The transfers: the tree's global table.
    let xfers: Vec<(Transfer, Transfer)> = eg.pairs().iter().map(|p| (p.0.transfer(), p.1.transfer())).collect();
    // The edges between nodes, read UNIT BY UNIT (source-side, as recorded:
    // no probe per node and frame): `f(src, dst, xfer)` per edge whose source
    // and target are both nodes. A unit with no node among its sources is
    // skipped whole; a lid's region ranks are looked up once a unit.
    // A frame at a time: its lids' owners (`EdgeStore::owners`) held while
    // its units are read, in parallel.
    let each_unit = |f: &(dyn Fn(&crate::storage::edges::UnitView) -> anyhow::Result<()> + Sync)| -> anyhow::Result<()> {
        for frame in 1..=horizon {
            let owners = eg.owners(frame);
            let units = eg.units(frame);
            par_chunks(units.len(), |lo, hi| {
                for &(fi, ui) in &units[lo..hi] {
                    f(&eg.unit(frame, &owners, fi, ui))?;
                }
                Ok(())
            })?;
        }
        Ok(())
    };
    let each_edge = |u: &crate::storage::edges::UnitView, f: &mut dyn FnMut(u32, u32, u32)| -> anyhow::Result<()> {
        let src: Vec<Option<u32>> = (0..u.n_sources() as u32).map(|s| ranks.rank(u.source(s))).collect();
        if src.iter().all(Option::is_none) {
            return Ok(());
        }
        let owners: Vec<Option<(crate::storage::marks::RegionRanks, u32)>> =
            (0..u.n_lids() as u32).map(|l| u.lid_owner(l).and_then(|(r, e)| ranks.region(r).map(|rr| (rr, e)))).collect();
        let mut bad = None;
        u.edges(|lid, local, s, x| {
            let (Some(s), Some((rr, e))) = (src[s as usize], owners[lid as usize]) else { return };
            if (x as usize) >= xfers.len() {
                bad = Some(x);
                return;
            }
            if let Some(d) = rr.rank(e, local) {
                f(s, d, x);
            }
        });
        anyhow::ensure!(bad.is_none(), "{}: an edge with transfer {bad:?} past the table", dir.display());
        Ok(())
    };
    // Pass 1: the degrees.
    let t1 = std::time::Instant::now();
    let out_deg: Vec<AtomicU64> = (0..n).map(|_| AtomicU64::new(0)).collect();
    let in_deg: Vec<AtomicU64> = (0..n).map(|_| AtomicU64::new(0)).collect();
    each_unit(&|u| {
        each_edge(u, &mut |s, d, _| {
            out_deg[s as usize].fetch_add(1, Relaxed);
            in_deg[d as usize].fetch_add(1, Relaxed);
        })
    })?;
    let prefix = |deg: &mut dyn Iterator<Item = u64>| -> Vec<u64> {
        let mut at = Vec::with_capacity(n + 1);
        let mut acc = 0u64;
        at.push(0);
        for d in deg {
            acc += d;
            at.push(acc);
        }
        at
    };
    let pred_at = prefix(&mut in_deg.into_iter().map(|d| d.into_inner()));
    let out_at = prefix(&mut out_deg.into_iter().map(|d| d.into_inner()));
    let edges = pred_at[n] as usize;
    anyhow::ensure!(out_at[n] as usize == edges, "the edge count pass disagrees with itself");
    // Pass 2: the adjacencies, filled in place through a cursor per node,
    // then each node's edges sorted (deterministic whatever the scheduling).
    let mut preds: Vec<u32> = vec![0; edges];
    let mut out: Vec<Out> = vec![Out { dst: 0, xfer: 0 }; edges];
    {
        let out_cursor: Vec<AtomicU64> = out_at[..n].iter().map(|&a| AtomicU64::new(a)).collect();
        let pred_cursor: Vec<AtomicU64> = pred_at[..n].iter().map(|&a| AtomicU64::new(a)).collect();
        let (preds_ptr, out_ptr) = (preds.as_mut_ptr() as usize, out.as_mut_ptr() as usize);
        each_unit(&|u| {
            each_edge(u, &mut |s, d, xfer| {
                let slot = out_cursor[s as usize].fetch_add(1, Relaxed);
                let at = pred_cursor[d as usize].fetch_add(1, Relaxed);
                assert!(at < pred_at[d as usize + 1] && slot < out_at[s as usize + 1], "the edge fill pass disagrees with the count");
                // SAFETY: `slot` and `at` were each claimed once, by
                // their atomic cursors, from their node's range; both
                // are below `edges`.
                unsafe {
                    (preds_ptr as *mut u32).add(at as usize).write(s);
                    (out_ptr as *mut Out).add(slot as usize).write(Out { dst: d, xfer });
                }
            })
        })?;
        anyhow::ensure!(out_cursor.iter().zip(&out_at[1..]).all(|(c, &e)| c.load(Relaxed) == e), "the edge fill pass disagrees with the count");
        anyhow::ensure!(pred_cursor.iter().zip(&pred_at[1..]).all(|(c, &e)| c.load(Relaxed) == e), "the edge fill pass disagrees with the count");
    }
    {
        let (out_ref, preds_ref, out_at_ref, pred_at_ref) = (out.as_mut_ptr() as usize, preds.as_mut_ptr() as usize, &out_at, &pred_at);
        par_chunks(n, |lo, hi| {
            // SAFETY: the nodes `lo..hi` own the disjoint ranges
            // `out_at[lo]..out_at[hi]` and `pred_at[lo]..pred_at[hi]`.
            let o = unsafe { std::slice::from_raw_parts_mut((out_ref as *mut Out).add(out_at_ref[lo] as usize), (out_at_ref[hi] - out_at_ref[lo]) as usize) };
            let p = unsafe { std::slice::from_raw_parts_mut((preds_ref as *mut u32).add(pred_at_ref[lo] as usize), (pred_at_ref[hi] - pred_at_ref[lo]) as usize) };
            let (ob, pb) = (out_at_ref[lo], pred_at_ref[lo]);
            for i in lo..hi {
                o[(out_at_ref[i] - ob) as usize..(out_at_ref[i + 1] - ob) as usize].sort_unstable_by_key(|o| (o.dst, o.xfer));
                p[(pred_at_ref[i] - pb) as usize..(pred_at_ref[i + 1] - pb) as usize].sort_unstable();
            }
            Ok(())
        })?;
    }
    drop(ranks);
    drop(eg);
    let t_read = t1.elapsed();
    crate::metrics::mem_phase("arc: edges read");
    let t2 = std::time::Instant::now();
    let n_xfers = xfers.len();
    let mut win = vec![false; n];
    for &(w, _) in &wins {
        let i = marks.binary_search_by_key(&w, |m| m.0).map_err(|_| anyhow::anyhow!("a win state {} the BFS did not mark", crate::storage::show_id(w)))?;
        win[i] = true;
    }
    let until: Vec<u32> = marks.iter().map(|&(_, d)| d as u32 + 1).collect();
    let ids: Vec<u64> = marks.iter().map(|&(id, _)| id).collect();
    let graph = Graph::from_csr(ids, Csr { out_at, out, pred_at, preds }, xfers, layers, win, until);
    crate::metrics::mem_phase("arc: graph built");
    eprintln!(
        "[arc] h{horizon}: {} marked nodes, {} win rows, {edges} edges, {n_xfers} transfer pairs; bfs {:.1} s, edges {:.1} s (two passes), graph {:.1} s",
        marks.len(),
        wins.len(),
        t_bfs.as_secs_f64(),
        t_read.as_secs_f64(),
        t2.elapsed().as_secs_f64()
    );
    let start = graph.index(start_id).expect("the start is a node");
    Ok(Loaded { graph, start, marks, wins, edges })
}

/// One frame's winning sets: the nodes (sorted) and their sets (indices
/// into the backward's arena), with the last frame of each set's span so far
/// (the backward runs down in `t`).
#[derive(Default)]
struct Frame {
    nodes: Vec<u32>,
    sets: Vec<u32>,
    his: Vec<u16>,
}

/// Node `node`'s winning set (`Winning::sets[set]`) over the frames
/// `lo..=hi`, where it does not change.
struct Span {
    node: u32,
    lo: u16,
    hi: u16,
    set: u32,
}

/// The winning sets, `t` in `0..=horizon`, as SPANS: a node's set changes
/// at a few frames, so one entry per (node, unchanged run of frames) - 12 B -
/// into an arena of the distinct sets. A table per frame (12 B per node and
/// frame it is present at, each set an `Arc`) held 4.4 GB for room (2,3)
/// gemskip h137, 18.6M nodes.
pub struct Winning {
    /// By (node, lo), disjoint per node.
    spans: Vec<Span>,
    sets: Vec<Region>,
}

impl Winning {
    /// `W_t(i)` (`None` = empty).
    pub fn at(&self, t: u32, i: u32) -> Option<&Region> {
        let lo = self.spans.partition_point(|s| s.node < i);
        let hi = lo + self.spans[lo..].partition_point(|s| s.node == i);
        let k = lo + self.spans[lo..hi].partition_point(|s| (s.hi as u32) < t);
        self.spans.get(k).filter(|s| s.node == i && s.lo as u32 <= t && t <= s.hi as u32).map(|s| &self.sets[s.set as usize])
    }
    /// Every span `(node, lo, hi, set)`: `W_t(node) = set` for `t` in
    /// `lo..=hi`, empty at the frames no span covers.
    pub fn spans(&self) -> impl Iterator<Item = (u32, u32, u32, &Region)> + '_ {
        self.spans.iter().map(|s| (s.node, s.lo as u32, s.hi as u32, &self.sets[s.set as usize]))
    }
}

/// A candidate's set at `t`, against its set at `t + 1`, as a code: empty,
/// the same, NEW (the next of the pull's new sets, with its hash), or the
/// index of a set already in the arena (found by the pulling worker). Four
/// bytes: a 64-byte enum carrying the new sets made the merge read ~1 GB a
/// frame (room (2,3) level 1: merge 5 -> 50 s).
const EMPTY: u32 = u32::MAX;
const SAME: u32 = u32::MAX - 1;
const NEW: u32 = u32::MAX - 2;

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
/// torus from its layer on; any other's is the union of its out-edges'
/// preimages of `W_{t+1}`, while it is live.
///
/// INCREMENTAL: `W_t(i) = W_{t+1}(i)` when `i` is live at both and none of
/// its successors' sets changed, so a frame recomputes (a parallel PULL per
/// node) only the predecessors of changed nodes and the nodes that become
/// live at `t`; the rest share the next frame's set. Prints the sets'
/// fragmentation every 4th frame.
pub fn backward(g: &Graph, horizon: u32) -> Winning {
    let n = g.len();
    assert!(horizon < u16::MAX as u32, "a horizon past u16");
    let threads = crate::frame::threads();
    // The ARENA of sets, equal sets once (found by hash; a collision with a
    // different set is stored apart). Index 0: the whole torus.
    let mut sets: Vec<Region> = vec![Region::full()];
    let mut interned: FxHashMap<u64, u32> = FxHashMap::default();
    let hash_of = |r: &Region| {
        use std::hash::{Hash, Hasher};
        let mut hs = rustc_hash::FxHasher::default();
        r.hash(&mut hs);
        hs.finish()
    };
    interned.insert(hash_of(&sets[0]), 0);
    let mut spans: Vec<Span> = Vec::new();
    let mut next = Frame::default();
    for i in 0..n as u32 {
        if g.win[i as usize] && g.layer[i as usize] <= horizon {
            next.nodes.push(i);
            next.sets.push(0);
            next.his.push(horizon as u16);
        }
    }
    let mut changed: Vec<u32> = next.nodes.clone();
    // `pos[i]`: i's position in the next frame's lists (u32::MAX: no set).
    let mut pos = vec![u32::MAX; n];
    for (k, &i) in next.nodes.iter().enumerate() {
        pos[i as usize] = k as u32;
    }
    // `stamp[i] == t`: i is already a candidate at t.
    let mut stamp = vec![u32::MAX; n];
    let (mut n_cand, mut n_same, mut n_scan) = (0u64, 0u64, 0u64);
    let (mut d_cand, mut d_pull, mut d_merge) = (std::time::Duration::ZERO, std::time::Duration::ZERO, std::time::Duration::ZERO);
    let mut fragments: Vec<String> = Vec::new();
    for t in (0..horizon).rev() {
        let t0 = std::time::Instant::now();
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
        let (pos_ref, next_ref, sets_ref, interned_ref) = (&pos, &next, &sets, &interned);
        let lookup = |d: u32| {
            let k = pos_ref[d as usize];
            (k != u32::MAX).then(|| &sets_ref[next_ref.sets[k as usize] as usize])
        };
        type Part = (usize, Vec<u32>, Vec<(Region, u64)>, u64);
        let mut parts: Vec<Part> = std::thread::scope(|sc| {
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
                            let mut news = Vec::new();
                            let codes = cand[i..(i + CHUNK).min(cand.len())]
                                .iter()
                                .map(|&c| {
                                    scanned += g.out_of(c).len() as u64;
                                    let r = pull(g, c, lookup, &mut pieces, &mut scratch);
                                    if r.is_empty() {
                                        EMPTY
                                    } else if lookup(c) == Some(&r) {
                                        SAME
                                    } else {
                                        // Looked up (and a duplicate freed) here, in parallel.
                                        let h = hash_of(&r);
                                        match interned_ref.get(&h) {
                                            Some(&s) if sets_ref[s as usize] == r => s,
                                            _ => {
                                                news.push((r, h));
                                                NEW
                                            }
                                        }
                                    }
                                })
                                .collect();
                            out.push((i, codes, news, scanned));
                        }
                    })
                })
                .collect();
            hs.into_iter().flat_map(|h| h.join().expect("a backward worker panicked")).collect()
        });
        parts.sort_unstable_by_key(|p| p.0);
        let (mut codes, mut news): (Vec<u32>, Vec<(Region, u64)>) = (Vec::with_capacity(cand.len()), Vec::new());
        for (_, c, nw, sc) in parts {
            codes.extend(c);
            news.extend(nw);
            n_scan += sc;
        }
        d_pull += t0.elapsed();
        let t0 = std::time::Instant::now();
        // Merge: the next frame's persisting sets, overridden by the
        // candidates. A set that ends at t + 1 closes its span.
        let mut cur = Frame { nodes: Vec::with_capacity(next.nodes.len()), sets: Vec::with_capacity(next.nodes.len()), his: Vec::with_capacity(next.nodes.len()) };
        let mut new_changed: Vec<u32> = Vec::new();
        let (mut a, mut b) = (0usize, 0usize);
        let (mut codes, mut news) = (codes.into_iter(), news.into_iter());
        // New sets equal to one interned earlier in this frame: freed in
        // parallel below, not one by one here (they were allocated by the
        // pulling workers; freeing ~5M in the merge cost 3.5 s at one frame
        // of room (2,3) level 1).
        let mut garbage: Vec<Region> = Vec::new();
        let close = |a: usize, spans: &mut Vec<Span>| spans.push(Span { node: next.nodes[a], lo: t as u16 + 1, hi: next.his[a], set: next.sets[a] });
        while a < next.nodes.len() || b < cand.len() {
            let ia = next.nodes.get(a).copied().unwrap_or(u32::MAX);
            let ib = cand.get(b).copied().unwrap_or(u32::MAX);
            if ib <= ia {
                let had = ib == ia;
                let set = match codes.next().expect("one result per candidate") {
                    EMPTY => None,
                    SAME => {
                        n_same += 1;
                        cur.nodes.push(ib);
                        cur.sets.push(next.sets[a]);
                        cur.his.push(next.his[a]);
                        b += 1;
                        a += 1;
                        continue;
                    }
                    NEW => {
                        let (r, h) = news.next().expect("a new set per NEW");
                        Some(match interned.get(&h) {
                            Some(&s) if sets[s as usize] == r => {
                                garbage.push(r);
                                s
                            }
                            found => {
                                let s = sets.len() as u32;
                                assert!(s < NEW, "more distinct sets than set codes");
                                sets.push(r);
                                if found.is_none() {
                                    interned.insert(h, s);
                                }
                                s
                            }
                        })
                    }
                    known => Some(known),
                };
                if had {
                    new_changed.push(ib);
                    close(a, &mut spans);
                    a += 1;
                }
                if let Some(s) = set {
                    if !had {
                        new_changed.push(ib);
                    }
                    cur.nodes.push(ib);
                    cur.sets.push(s);
                    cur.his.push(t as u16);
                }
                b += 1;
            } else {
                if g.present(ia, t) {
                    cur.nodes.push(ia);
                    cur.sets.push(next.sets[a]);
                    cur.his.push(next.his[a]);
                } else {
                    new_changed.push(ia);
                    close(a, &mut spans);
                }
                a += 1;
            }
        }
        let per = garbage.len().div_ceil(threads).max(1);
        std::thread::scope(|sc| {
            while !garbage.is_empty() {
                let part = garbage.split_off(garbage.len().saturating_sub(per));
                sc.spawn(move || drop(part));
            }
        });
        for &i in &next.nodes {
            pos[i as usize] = u32::MAX;
        }
        for (k, &i) in cur.nodes.iter().enumerate() {
            pos[i as usize] = k as u32;
        }
        changed = new_changed;
        next = cur;
        d_merge += t0.elapsed();
        if (horizon - 1 - t) % 4 == 0 {
            let (k, med, p90, max) = fragmentation(next.nodes.iter().zip(&next.sets).filter(|&(&i, _)| !g.is_win(i)).map(|(_, &s)| &sets[s as usize]));
            fragments.push(format!("[arc] W frame {t:3}: {k} non-win nodes can win; rectangles per node med {med} p90 {p90} max {max}"));
        }
    }
    for a in 0..next.nodes.len() {
        spans.push(Span { node: next.nodes[a], lo: 0, hi: next.his[a], set: next.sets[a] });
    }
    drop(next);
    spans.sort_unstable_by_key(|s| (s.node, s.lo));
    eprintln!(
        "[arc] backward: {n_cand} recomputations ({n_same} unchanged), {n_scan} out-edges scanned; candidates {:.2} s, pull {:.2} s, merge {:.2} s; {} spans, {} distinct sets",
        d_cand.as_secs_f64(),
        d_pull.as_secs_f64(),
        d_merge.as_secs_f64(),
        spans.len(),
        sets.len()
    );
    for f in fragments {
        eprintln!("{f}");
    }
    Winning { spans, sets }
}

/// FORWARD inside W: `R_t(i)`, the remainders with which node `i` is reached
/// at frame `t` from the start (at `point`) along the graph's edges and
/// still wins - `R_0 = {point}` at the start, `R_{t+1}(m) = U push_e(R_t(p))
/// ∩ W_{t+1}(m)` over the edges `p -> m` (a win node is not left). Per node
/// the LAST frame its `R` is non-empty (`u32::MAX`: never), for the next
/// level's filter.
///
/// Sound as a filter where `W`'s deadline is: a concrete winning path's
/// remainder at frame `t` is in `W_t` of its node (W holds every winner)
/// and is the image of its remainder at `t - 1` along the recorded edge (the
/// arc model `arc-check` probes), so by induction it is in `R_t`. Tighter:
/// a node `W` lets win from some remainder no path from the start brings is
/// not marked.
///
/// A set more fragmented than `REACH_SEGS` segments is replaced by `W_t`
/// itself (a superset: the filter only loosens), which bounds the memory -
/// W's sets are shared, a node's reached set is its own.
pub fn reach(g: &Graph, w: &Winning, start: u32, point: (u32, u32), horizon: u32) -> Vec<u32> {
    use super::arcs::Piece;
    let n = g.len();
    let threads = crate::frame::threads().max(1);
    let mut last = vec![u32::MAX; n];
    const REACH_SEGS: usize = 32;
    // The current frame's sets, by node (`None`: W_t of the node); `pos[i]`
    // its index (u32::MAX: none).
    let mut cur: Vec<(u32, Option<Region>)> = Vec::new();
    let mut pos = vec![u32::MAX; n];
    if let Some(r) = w.at(0, start) {
        if r.contains(point.0, point.1) {
            let p = Piece { y: super::arcs::Seg { lo: point.1, hi: point.1 + 1 }, x: super::arcs::Seg { lo: point.0, hi: point.0 + 1 } };
            cur.push((start, Some(Region::from_pieces(&mut [p], &mut Vec::new()))));
            pos[start as usize] = 0;
            last[start as usize] = 0;
        }
    }
    let mut stamp = vec![u32::MAX; n];
    let (mut active_max, mut in_w) = (0usize, 0u64);
    for t in 0..horizon {
        if cur.is_empty() {
            break;
        }
        active_max = active_max.max(cur.len());
        if t % 16 == 0 {
            let own = cur.iter().filter(|c| c.1.is_some()).count();
            let segs: usize = cur.iter().filter_map(|c| c.1.as_ref()).map(|r| r.n_segs()).sum();
            eprintln!("[arc] reach f{t:03}: {} nodes ({own} own sets, {segs} segments; the rest W's)", cur.len());
        }
        // Candidates: the successors of the nodes with a set, W non-empty at t + 1.
        let mut cand: Vec<u32> = Vec::new();
        for (i, _) in &cur {
            if g.is_win(*i) {
                continue;
            }
            for e in g.out_of(*i) {
                if stamp[e.dst as usize] != t && w.at(t + 1, e.dst).is_some() {
                    stamp[e.dst as usize] = t;
                    cand.push(e.dst);
                }
            }
        }
        cand.sort_unstable();
        // Each candidate's set, pulled from its predecessors' (in parallel).
        const CHUNK: usize = 256;
        let at = std::sync::atomic::AtomicUsize::new(0);
        let (cur_ref, pos_ref, cand_ref) = (&cur, &pos, &cand);
        let mut parts: Vec<(usize, Vec<Option<Option<Region>>>)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads)
                .map(|_| {
                    sc.spawn(|| {
                        let (mut out, mut pieces, mut clipped, mut scratch) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
                        loop {
                            let lo = at.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cand_ref.len() {
                                return out;
                            }
                            let sets: Vec<Option<Option<Region>>> = cand_ref[lo..(lo + CHUNK).min(cand_ref.len())]
                                .iter()
                                .map(|&m| {
                                    let wm = w.at(t + 1, m).expect("a candidate has a winning set");
                                    pieces.clear();
                                    let mut preds: Vec<u32> = g.preds_of(m).to_vec();
                                    preds.sort_unstable();
                                    preds.dedup();
                                    for p in preds {
                                        let k = pos_ref[p as usize];
                                        if k == u32::MAX || g.is_win(p) {
                                            continue;
                                        }
                                        let r = match &cur_ref[k as usize].1 {
                                            Some(r) => r,
                                            None => w.at(t, p).expect("a node reached in W has a set"),
                                        };
                                        for e in g.out_of(p).iter().filter(|e| e.dst == m) {
                                            let (x, y) = g.xfer(e);
                                            r.push(x, y, &mut pieces);
                                        }
                                    }
                                    clipped.clear();
                                    for &pc in &pieces {
                                        wm.clip_into(pc, &mut clipped);
                                    }
                                    let r = Region::from_pieces(&mut clipped, &mut scratch);
                                    if r.is_empty() {
                                        None
                                    } else if r.n_segs() > REACH_SEGS {
                                        Some(None)
                                    } else {
                                        Some(Some(r))
                                    }
                                })
                                .collect();
                            out.push((lo, sets));
                        }
                    })
                })
                .collect();
            hs.into_iter().flat_map(|h| h.join().expect("a reach worker panicked")).collect()
        });
        parts.sort_unstable_by_key(|p| p.0);
        for (i, _) in &cur {
            pos[*i as usize] = u32::MAX;
        }
        let mut next: Vec<(u32, Option<Region>)> = Vec::new();
        for (m, r) in cand.iter().zip(parts.into_iter().flat_map(|p| p.1)) {
            if let Some(r) = r {
                in_w += r.is_none() as u64;
                pos[*m as usize] = next.len() as u32;
                last[*m as usize] = t + 1;
                next.push((*m, r));
            }
        }
        cur = next;
    }
    let marked = last.iter().filter(|&&l| l != u32::MAX).count();
    eprintln!("[arc] reach: {marked} of {n} nodes reached inside W from the start (at most {active_max} a frame; {in_w} sets widened to W)");
    last
}

/// The OPTIMUM and a witness: the win frame (from frame 0) and the path to
/// it (node id, remainder point) frame by frame.
pub struct Found {
    pub frame: u32,
    pub path: Vec<(u64, (u32, u32))>,
}

/// The OPTIMUM from `start` at remainder `point`, read off ONE backward:
/// `horizon - max{t : point in W_t(start)}`. Valid because the graph is the
/// same at every frame but for the layers, and k steps from the start reach
/// only layers <= k, so a layer never binds.
///
/// The witness is a greedy walk: a point in `W_t(n)` (n not a win) has an
/// out-edge into `W_{t+1}`, and at the horizon only win nodes have sets, so
/// the walk never backtracks and wins in exactly the fewest frames.
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

/// Fragmentation of winning sets (x segments per node, over all slabs) as
/// (nodes, median, p90, max).
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

/// The graph's nodes by `(shape, key, cell)`, for the concrete search:
/// sorted, 32 B a node (a hash map took ~80).
pub struct NodeKeys(Vec<(u64, (u64, u64), u32, u32)>);

impl NodeKeys {
    /// From `(shape, key, cell, node)` in any order; a key twice keeps its
    /// lowest node.
    fn new(mut v: Vec<(u64, (u64, u64), u32, u32)>) -> Self {
        v.sort_unstable();
        v.dedup_by_key(|e| (e.0, e.1, e.2));
        NodeKeys(v)
    }
    pub(super) fn get(&self, shape: u64, key: Option<(u64, u64)>, cell: u32) -> Option<u32> {
        let k = (shape, key?, cell);
        self.0.binary_search_by(|e| (e.0, e.1, e.2).cmp(&k)).ok().map(|j| self.0[j].3)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// A concrete state's node key at `level` (`lookup_view`, widened) in the
/// tree's key space (`None`: a code the tree never stored - no node).
pub(super) fn lookup_keys(b: &crate::frame::Block, level: crate::abstraction::Level, seeded: bool, keys: &celeste_engine::exact::KeySpace) -> anyhow::Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    if !seeded {
        return crate::frame::widened_keys(b, level, keys);
    }
    crate::frame::widened_keys_rt2(&lookup_view(b.rt2(), seeded)?, level, keys)
}

/// A concrete state's EXACT identity, UNWIDENED: its canonical bytes
/// (`exact::exact_rows`) and its cell. Never the level's widened key:
/// states sharing it need not share their fate (`522de36`).
pub(super) fn exact_state(b: &crate::frame::Block, cell: u32) -> celeste_engine::exact::ExactRow {
    let mut v = celeste_engine::exact::exact_rows(b.rt2(), crate::compiled::ids()).swap_remove(0).into_vec();
    v.extend_from_slice(&cell.to_le_bytes());
    v.into_boxed_slice()
}

/// A concrete state as the tree holds it before the level's widening. With
/// seeded balloons (`concrete_search`), the state holds each balloon's
/// phase as one number and its `y` as one point, where the tree - built
/// with the phase an interval - holds the canonical phase `[0, 1)` and the
/// bob `y` `[start - 2, start + 2]` (the forward recomputes `y` from the
/// interval phase every frame); the view maps them there, so its key is the
/// node that over-approximates the state.
pub(super) fn lookup_view(row: &celeste_engine::runtime2::Rt2, seeded: bool) -> anyhow::Result<celeste_engine::runtime2::Rt2> {
    use celeste_engine::runtime2::{Col, AV, BALLOON_BOB_RAW, BALLOON_PERIOD_RAW};
    use crate::pico8_num::Pico8Num as P8;
    let mut rt2 = row.clone_block();
    if !seeded {
        return Ok(rt2);
    }
    let ids = crate::compiled::ids();
    for obj in rt2.objects_of_type(ids, ids.g_balloon) {
        let (Some(oc), Some(yc), Some(sc)) = (rt2.obj_field_cell(obj, ids.f_offset), rt2.obj_field_cell(obj, ids.f_y), rt2.obj_field_cell(obj, ids.f_start)) else { continue };
        let AV::Num(start) = rt2.cols[sc as usize].at(0) else { anyhow::bail!("a balloon's `start` is not a number") };
        let s = start.as_raw_u32() as i32;
        rt2.cols[oc as usize] = Col::U(AV::Ival(P8::from_raw(0), P8::from_raw(BALLOON_PERIOD_RAW)));
        rt2.cols[yc as usize] = Col::U(AV::Ival(P8::from_raw(s - BALLOON_BOB_RAW), P8::from_raw(s + BALLOON_BOB_RAW)));
    }
    Ok(rt2)
}

/// A concrete state's remainder as a point of the torus (no player: the
/// point (0, 0)).
pub(super) fn rem_of(rt2: &celeste_engine::runtime2::Rt2) -> anyhow::Result<(u32, u32)> {
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::ids();
    let Some((cx, cy)) = rt2.player_xy_cells(ids, ids.f_rem) else { return Ok((super::arcs::point(0), super::arcs::point(0))) };
    let raw = |c: usize| -> anyhow::Result<u32> {
        match rt2.cols[c].at(0) {
            AV::Num(n) => Ok(super::arcs::point(n.as_raw_u32() as i32)),
            other => anyhow::bail!("a concrete remainder expected, got {other:?}"),
        }
    };
    Ok((raw(cx)?, raw(cy)?))
}

/// `n` reference engines for concrete steps, and whether they run seeded:
/// `CELESTE_CONCRETE_BALLOON_SEEDS` fixes the balloons' phases (a
/// tasdatabase file's seeds), so a concrete run is one real run under them -
/// the tree and W keep the phase an interval (sound for every seed), and a
/// concrete state's key maps its phase to that interval (`lookup_view`).
/// Built under the global level: `_init` runs under it.
pub(super) fn concrete_engines(n: usize) -> anyhow::Result<(Vec<crate::trace::refengine::RefEngine>, bool)> {
    let seeds = std::env::var("CELESTE_CONCRETE_BALLOON_SEEDS").ok();
    if let Some(s) = &seeds {
        std::env::set_var("CELESTE_BALLOON_SEEDS", s);
    }
    let engines: anyhow::Result<Vec<_>> = (0..n).map(|_| crate::trace::refengine::RefEngine::new()).collect();
    if seeds.is_some() {
        std::env::remove_var("CELESTE_BALLOON_SEEDS");
    }
    Ok((engines?, seeds.is_some()))
}

/// The inputs a state has been stepped under, each with the buttons its
/// frame read (`RefEngine::frame_reads`). An input that agrees with one of
/// them on every button that one read runs the same frame - the same reads,
/// the same values, the same successors (the output writes no button) - so
/// its successors are repeats of states already emitted in (parent, input)
/// order, which the search's exact dedup would drop: skipping it changes
/// nothing but the step count.
#[derive(Default)]
struct Covered(Vec<(u8, u8)>);

impl Covered {
    fn covers(&self, byte: u8) -> bool {
        self.0.iter().any(|&(b, read)| (b ^ byte) & read == 0)
    }

    fn push(&mut self, byte: u8, read: u8) {
        self.0.push((byte, read));
    }
}

/// The 64 inputs in the order the concrete search tries them at the frame
/// after `k`: `prefer[k]` first (`rewrite search --prefer`: a known TAS, so
/// the witness follows it wherever an optimal route allows), then the rest.
/// Every input is still tried: the order only picks WHICH optimal witness.
fn input_order(prefer: Option<&[u8]>, k: u32) -> impl Iterator<Item = u8> {
    let first = prefer.and_then(|p| p.get(k as usize).copied()).filter(|&b| b < 64);
    first.into_iter().chain((0u8..64).filter(move |&b| Some(b) != first))
}

/// search over concrete states through the reference engine (every input,
/// every `rnd` leaf), layer k+1 admitting a successor only if its projection
/// onto `level` is a node of `g` whose `W_{k+1}` holds its exact remainder
/// (no player: the point (0, 0)), each EXACT state once (the first in
/// (parent, input, leaf) order: deterministic).
///
/// Sound: every concrete path that wins by the horizon stays inside W, so
/// the search is EXHAUSTIVE inside W and prunes by nothing else; its first
/// win is the CONCRETE optimum. `None`: no concrete win by the horizon.
/// `bound` (the graph's optimum, in frames) is a lower bound on it. Layers
/// run in parallel, one reference engine per worker. `horizon` is in search
/// steps, W's index: under the split frame layer k is W at step `2k`, a
/// frame boundary, whose nodes key exactly as a whole frame's states.
pub fn concrete_search(
    level: crate::abstraction::Level,
    horizon: u32,
    node: &NodeKeys,
    keys: &celeste_engine::exact::KeySpace,
    w: &Winning,
    bound: u32,
    prefer: Option<&[u8]>,
) -> anyhow::Result<Option<Witness>> {
    use crate::frame::{wins_of, Block};
    use anyhow::Result;
    use crate::trace::refengine::RefEngine;
    use celeste_engine::runtime2::Rt2;
    // Build the engines BEFORE setting the level: `_init` runs under the
    // global level. Their frames run exact whatever is set.
    let workers = crate::frame::threads().max(1);
    let (engines, seeded) = concrete_engines(workers)?;
    let engines: Vec<std::sync::Mutex<RefEngine>> = engines.into_iter().map(std::sync::Mutex::new).collect();
    let initial = engines[0].lock().expect("an engine").initial()?;
    crate::abstraction::set_level(level);
    // W is indexed by search STEP; the concrete search steps whole frames
    // (`RefEngine::frame`), and frame k ends at step `spf * k`.
    let spf = crate::frame::steps_per_frame();
    anyhow::ensure!(horizon % spf == 0, "the horizon {horizon} is not a whole number of frames ({spf} steps each)");
    let frames = horizon / spf;
    let t0 = std::time::Instant::now();
    eprintln!("[concrete] {} nodes keyed; {workers} workers; the arc bound f{bound}", node.len());
    crate::metrics::mem_phase("concrete: engines up");
    /// A successor that stays inside W (or wins), in (parent, input, leaf)
    /// order within its worker's chunk.
    struct Succ {
        parent: u32,
        byte: u8,
        cell: u32,
        win: bool,
        exact: celeste_engine::exact::ExactRow,
        /// `None` for a win (its state is never expanded).
        row: Option<Rt2>,
    }
    let start = Block::canonical(initial)?;
    let start_cell = start.positions()?[0];
    // THE FAST PATH: a budgeted depth-first search for a win AT the bound,
    // inside `W_{H - bound + k}`, on one engine. Sound: a win here is at the
    // lower bound, hence optimal; an exhausted or abandoned DFS proves nothing
    // and the BFS runs.
    {
        struct Dfs<'a> {
            eng: &'a mut RefEngine,
            node: &'a NodeKeys,
            keys: &'a celeste_engine::exact::KeySpace,
            w: &'a Winning,
            level: crate::abstraction::Level,
            from: u32,
            frames: u32,
            /// Exhausted states by (exact state, frame).
            dead: FxHashSet<(celeste_engine::exact::ExactRow, u32)>,
            path: Vec<(u8, u32)>,
            steps: u64,
            prefer: Option<&'a [u8]>,
            seeded: bool,
            spf: u32,
        }
        /// `Ok(None)`: out of budget.
        fn dfs(cx: &mut Dfs, st: &Rt2, k: u32) -> Result<Option<bool>> {
            if k >= cx.frames {
                return Ok(Some(false));
            }
            let mut ran = Covered::default();
            for byte in input_order(cx.prefer, k) {
                if ran.covers(byte) {
                    continue;
                }
                let (succ, read) = cx.eng.frame_reads(st, byte)?;
                ran.push(byte, read);
                for b in succ {
                    cx.steps += 1;
                    if cx.steps > DFS_BUDGET {
                        return Ok(None);
                    }
                    let cell = b.positions()?[0];
                    if wins_of(b.rt2())?.iter().any(|&x| x) {
                        // A win before the lower bound means a lost path.
                        anyhow::ensure!(k + 1 == cx.frames, "a concrete win at f{} before the arc bound f{}: the backward lost a path", k + 1, cx.frames);
                        cx.path.push((byte, cell));
                        return Ok(Some(true));
                    }
                    let exact = (exact_state(&b, cell), k + 1);
                    if cx.dead.contains(&exact) {
                        continue;
                    }
                    let (shape, nk, cells) = lookup_keys(&b, cx.level, cx.seeded, cx.keys)?;
                    let Some(i) = cx.node.get(shape, nk[0], cells[0]) else { continue };
                    let q = rem_of(b.rt2())?;
                    if !cx.w.at(cx.spf * (cx.from + k + 1), i).is_some_and(|r| r.contains(q.0, q.1)) {
                        continue;
                    }
                    cx.path.push((byte, cell));
                    match dfs(cx, b.rt2(), k + 1)? {
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
        let mut cx = Dfs { eng: &mut eng, node, keys, w, level, from: frames - bound, frames: bound, dead: FxHashSet::default(), path: Vec::new(), steps: 0, prefer, seeded, spf };
        let found = dfs(&mut cx, start.rt2(), 0)?;
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
    let mut cur: Vec<Rt2> = vec![start.into_rt2()];
    let mut steps = 0u64;
    for k in 0..frames {
        let t = std::time::Instant::now();
        let t_w = spf * (k + 1);
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        let parts: Vec<Result<(Vec<(usize, Vec<Succ>)>, u64)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, next_chunk) = (&cur, &next_chunk);
                    sc.spawn(move || -> Result<(Vec<(usize, Vec<Succ>)>, u64)> {
                        let mut eng = engine.lock().expect("an engine");
                        let (mut out, mut steps) = (Vec::new(), 0u64);
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, steps));
                            }
                            // Dedup within the chunk already (the first in
                            // order, as the merge keeps it): most successors
                            // are repeats.
                            let mut got = Vec::new();
                            let mut chunk_seen: FxHashSet<celeste_engine::exact::ExactRow> = FxHashSet::default();
                            'chunk: for (p, row) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                let mut ran = Covered::default();
                                for byte in input_order(prefer, k) {
                                    if ran.covers(byte) {
                                        continue;
                                    }
                                    let (succ, read) = eng.frame_reads(row, byte).map_err(|e| e.context(format!("the frame after layer {k} with input {byte}")))?;
                                    ran.push(byte, read);
                                    for b in succ {
                                        steps += 1;
                                        let cell = b.positions()?[0];
                                        // Dedup on the EXACT state, never the level's
                                        // widened key (`exact_state`).
                                        let exact = exact_state(&b, cell);
                                        let win = wins_of(b.rt2())?.iter().any(|&x| x);
                                        if !win && chunk_seen.contains(&exact) {
                                            continue;
                                        }
                                        if !win {
                                            let (shape, nk, cells) = lookup_keys(&b, level, seeded, keys)?;
                                            let Some(i) = node.get(shape, nk[0], cells[0]) else { continue };
                                            let q = rem_of(b.rt2())?;
                                            if !w.at(t_w, i).is_some_and(|r| r.contains(q.0, q.1)) {
                                                continue;
                                            }
                                        }
                                        chunk_seen.insert(exact.clone());
                                        // A win keeps no state (only its link), and the chunk
                                        // ends at its first: the earliest win (chunks merge in
                                        // order) ends the search. Every successor of the last
                                        // layer can win - kept whole, room (4,3) 100% reached
                                        // 100 GB there.
                                        got.push(Succ { parent: p as u32, byte, cell, win, exact, row: (!win).then(|| b.into_rt2()) });
                                        if win {
                                            break 'chunk;
                                        }
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
        // Each exact state once, in order; the first win ends the search.
        let mut seen: FxHashSet<celeste_engine::exact::ExactRow> = FxHashSet::default();
        let (mut next, mut links): (Vec<Rt2>, Vec<(u32, u8, u32)>) = (Vec::new(), Vec::new());
        let mut won: Option<(u32, u8, u32)> = None;
        for s in succs {
            if s.win {
                won = Some((s.parent, s.byte, s.cell));
                break;
            }
            if seen.insert(s.exact) {
                next.push(s.row.expect("a non-winning successor keeps its state"));
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
            println!("NO CONCRETE WIN by f{frames}: no concrete state inside W past layer {k} ({steps} steps)");
            return Ok(None);
        }
        back.push(links);
        cur = next;
    }
    println!("NO CONCRETE WIN by f{frames} ({steps} steps)");
    Ok(None)
}

/// What the arc phase of a search found (`solve`).
pub struct Solved {
    /// The optimum over the rotation graph, a lower bound on the game's, in
    /// FRAMES (`horizon` and the marks' deadlines are in search steps).
    /// `None`: the horizon is REFUTED.
    pub arc: Option<u32>,
    /// The concrete optimum and its witness, when asked for and found.
    pub concrete: Option<Witness>,
    /// The ARC-MARKED nodes with their deadlines (the last frame their
    /// winning set is non-empty): the next level's `frame::MarkFilter`.
    pub arc_marks: Option<crate::frame::Visited>,
    /// The known route's exit frame when it was checked (`search::known`):
    /// it wins by the horizon and survives every pruning step.
    pub known: Option<u32>,
}

/// THE ARC PHASE over a finished tree in `dir`: `load`, `backward`,
/// `optimum`, and as much of `concrete_search` as `concrete` says. Prints
/// the `[gate]` fingerprints (over (shape, key, cell), independent of
/// scheduling). With `save`, writes the UI's arc pass: `level0.marks.bin`
/// (remainder-free marks), `arc.marks.bin` (arc-marked nodes), `arc.txt`
/// and the witness. With `known`, checks a known solution against every
/// pruning step before the concrete search (`search::known`): an error if
/// one drops it.
#[allow(clippy::too_many_arguments)]
pub fn solve(
    dir: &std::path::Path,
    level: crate::abstraction::Level,
    horizon: u32,
    concrete: bool,
    want_marks: bool,
    save: Option<&std::path::Path>,
    prefer: Option<&[u8]>,
    known: Option<&super::known::Route>,
) -> anyhow::Result<Solved> {
    use crate::frame::{mark_row, save_marks, MarkRow, Visited};
    use celeste_engine::runtime2::mix64;
    crate::metrics::mem_phase("arc: start");
    let Loaded { graph, start, marks, wins, .. } = load(dir, horizon)?;
    let g = &graph;
    let tb = std::time::Instant::now();
    let w = backward(g, horizon);
    crate::metrics::mem_phase("arc: backward");
    let tb = tb.elapsed().as_secs_f64();
    // The start has no player yet: any point stands for its remainder.
    let p0 = (super::arcs::point(0), super::arcs::point(0));
    let found = optimum(g, &w, start, p0, horizon);
    let arc_steps = found.as_ref().map(|f| f.frame);
    // In frames: a win at step s is during frame ceil(s / spf) (a lower
    // bound still: a frame's win is a win at its last step at the latest).
    let arc = arc_steps.map(|s| s.div_ceil(crate::frame::steps_per_frame()));
    eprintln!("[arc] backward {tb:.2} s; optimum {arc_steps:?} (steps)");
    // The nodes as (shape, key, cell), through the storage metadata (the
    // marks are the nodes, in order): the fingerprints, the marks file and
    // the concrete search's keys.
    let resolver = crate::storage::marks::Resolver::load(dir, horizon)?;
    // Fingerprints over CONTENT (`exact::ContentHash`): independent of the
    // order the tree's dictionaries grew in (a raise).
    let content = resolver.space.content_hash();
    let mut files = Vec::new();
    for (f, fs) in tree_frames(dir, horizon)?.into_iter().enumerate() {
        files.extend(fs.into_iter().map(|(seq, file)| (f as u32, seq, file)));
    }
    let keyed = (concrete && arc.is_some()) || known.is_some();
    let mut node_key: Vec<u64> = Vec::with_capacity(g.len());
    let mut keys: Vec<(u64, (u64, u64), u32, u32)> = Vec::with_capacity(if keyed { g.len() } else { 0 });
    let mut rows: Vec<MarkRow> = Vec::new();
    let mut fp = 0u64;
    for &(id, d) in &marks {
        let (shape, key, cell) = resolver.resolve(id)?;
        // `Visited::fingerprint`: the marks are distinct states.
        let h = content.state(shape, key, cell);
        node_key.push(h);
        fp = fp.wrapping_add(h);
        if keyed {
            keys.push((shape, key, cell, keys.len() as u32));
        }
        if save.is_some() {
            rows.push(mark_row(shape, key, cell, d, horizon));
        }
    }
    println!("[gate] h{horizon} marks {} {fp:016x}", marks.len());
    drop(marks);
    if let Some(out) = save {
        std::fs::create_dir_all(out)?;
        save_marks(&out.join("level0.marks.bin"), std::mem::take(&mut rows), horizon, &resolver.space)?;
    }
    // Per frame, the nodes with a set and the sum of their (node, set) hashes.
    let mut per_frame = vec![(0usize, 0u64); horizon as usize + 1];
    for (i, lo, hi, r) in w.spans() {
        let mut h = node_key[i as usize];
        for (y, xs) in r.slabs() {
            h = mix64(h ^ ((y.lo as u64) << 32 | y.hi as u64));
            for x in xs {
                h = mix64(h ^ ((x.lo as u64) << 32 | x.hi as u64));
            }
        }
        for f in &mut per_frame[lo as usize..=hi as usize] {
            f.0 += 1;
            f.1 = f.1.wrapping_add(h);
        }
    }
    for (t, (n, acc)) in per_frame.into_iter().enumerate() {
        println!("[gate] h{horizon} W f{t:03} {n} {acc:016x}");
    }
    drop(node_key);
    println!("[gate] h{horizon} arc optimum {}", arc_steps.map_or("none".to_string(), |f| f.to_string()));
    crate::metrics::mem_phase("arc: fingerprints");
    // Per node the LAST frame it is reached inside W from the start (`reach`;
    // at most its W deadline): the next level's filter, and the UI's arc marks.
    let mut arc_marks = want_marks.then(|| Visited::new(resolver.space.clone()));
    if want_marks || save.is_some() {
        let tr = std::time::Instant::now();
        let last = reach(g, &w, start, p0, horizon);
        eprintln!("[arc] reach {:.2} s", tr.elapsed().as_secs_f64());
        crate::metrics::mem_phase("arc: reach");
        let ids: Vec<(u64, u16)> = (0..g.len() as u32).filter(|&i| last[i as usize] != u32::MAX).map(|i| (g.id(i), last[i as usize] as u16)).collect();
        drop(last);
        for &(id, d) in &ids {
            let (shape, key, cell) = resolver.resolve(id)?;
            if let Some(am) = &mut arc_marks {
                am.insert_until(shape, key, cell, d);
            }
            if save.is_some() {
                rows.push(mark_row(shape, key, cell, d, horizon));
            }
        }
    }
    if let Some(out) = save {
        save_marks(&out.join("arc.marks.bin"), rows, horizon, &resolver.space)?;
        let first_win = wins.iter().map(|&(_, f)| f).min();
        let show = |v: Option<u32>| v.map_or("none".to_string(), |f| f.to_string());
        std::fs::write(out.join("arc.txt"), format!("horizon {horizon}\nlevel {level}\nlevel0_first_win {}\noptimal {}\n", show(first_win), show(arc)))?;
    }
    let node = NodeKeys::new(keys);
    let known = match known {
        Some(route) => {
            let p = super::known::Pruning { level, horizon, node: &node, keys: &resolver.space, start, w: &w, reached: arc_marks.as_ref(), files: &files };
            super::known::check(route, &p)?
        }
        None => None,
    };
    drop(files);
    // The concrete search reads W and the node keys, not the graph (GBs).
    drop(graph);
    crate::metrics::mem_phase("arc: marks for the next level, graph dropped");
    let found = match (concrete, arc) {
        (true, Some(f)) => concrete_search(level, horizon, &node, &resolver.space, &w, f, prefer)?,
        _ => None,
    };
    if let Some(wt) = &found {
        println!("[gate] h{horizon} concrete optimum {} inputs {}", wt.inputs.len(), wt.inputs_text());
        if let Some(out) = save {
            wt.save(&out.join("witness.txt"))?;
        }
    }
    Ok(Solved { arc, concrete: found, arc_marks, known })
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

    /// The incremental backward equals the definition (every live node
    /// recomputed every frame) on random layered graphs with deadlines.
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

    /// `reach` against the points themselves: from one start point every
    /// frame's reached set is finite, so walk every (node, point) through the
    /// edges whose guards take it, keeping those inside W.
    #[test]
    fn reach_is_the_forward_of_the_start_point_inside_w() {
        let mut s = 23u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
        let mut nonempty = 0;
        for _ in 0..80 {
            let n = 3 + r(14) as u64;
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
            if edges.is_empty() {
                continue;
            }
            let horizon = 7;
            let d = dist.clone();
            let g = Graph::from_edges(edges, move |id| d[id as usize], wins, None);
            let Some(start) = g.index(0) else { continue };
            let w = backward(&g, horizon);
            let p0 = (r(CIRCLE), r(CIRCLE));
            let got = reach(&g, &w, start, p0, horizon);
            let mut want = vec![u32::MAX; g.len()];
            let inside = |t: u32, i: u32, p: (u32, u32)| w.at(t, i).is_some_and(|reg| reg.contains(p.0, p.1));
            let mut cur: FxHashSet<(u32, (u32, u32))> = FxHashSet::default();
            if inside(0, start, p0) {
                cur.insert((start, p0));
                want[start as usize] = 0;
            }
            for t in 0..horizon {
                let mut next = FxHashSet::default();
                for &(i, p) in &cur {
                    if g.is_win(i) {
                        continue;
                    }
                    for e in g.out_of(i) {
                        let (x, y) = g.xfer(e);
                        if x.takes(p.0) && y.takes(p.1) {
                            let q = (apply(x.action, p.0), apply(y.action, p.1));
                            if inside(t + 1, e.dst, q) {
                                next.insert((e.dst, q));
                                want[e.dst as usize] = t + 1;
                            }
                        }
                    }
                }
                cur = next;
            }
            nonempty += want.iter().filter(|&&l| l != u32::MAX).count();
            assert_eq!(got, want);
        }
        assert!(nonempty > 50, "only {nonempty} reached nodes over the cases");
    }

    /// With first-reach layers, the optimum read off `backward(H)` is the
    /// smallest `h` whose own backward holds the start.
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
