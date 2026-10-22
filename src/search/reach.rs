//! FORWARD REACHABILITY over a finished tree's recorded edges: ONE
//! DATAFLOW (`forward` over a `Flow`): from a start, every reached node's
//! remainder set pushed along its out-edges, the pushes into a target
//! closed into its set at the next frame; out-edges are read at a node's
//! first reach and kept (decoding them every frame made a frame's cost grow
//! with the reach). Its flows: `inside_w` (a `Region` inside W, the next
//! level's filter), `exact` (a test oracle: with W = everything, what the
//! hull forward's soundness is checked against) and `hull_replay` (the
//! hull forward without its engine).

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::arcs::{Circle, Piece, Region, RegionRef, Seg};
use super::backward::Winning;
use super::rem::{Point, Rem, Xfer};
use crate::storage::edges::EdgeStore;
use crate::storage::Sk;
use celeste_core::pico8_num::Pico8Num as P8;

/// A FORWARD DATAFLOW over recorded edges (`forward`): nodes (a state, or
/// W's node), each with its out-edges `(target, transfer)` read once; a
/// node's set at a frame pushed along them, the pushes into a target closed
/// into its set at the next.
pub(crate) trait Flow: Sync {
    type Node: Copy + Ord + Send + Sync;
    /// A node's set of remainders at a frame.
    type Set: Send + Sync;
    /// What one push gives its target (pieces of an image; a hull).
    type Part: Send + Sync;
    /// A closing worker's scratch.
    type Scratch: Default;
    /// Read the out-edges of `nodes` (reached at frame `t`) not read yet.
    fn load(&mut self, t: u32, nodes: &[Self::Node]) -> Result<()>;
    /// Node `n`'s out-edges `(target, transfer)`.
    fn out(&self, n: Self::Node) -> &[(Self::Node, u32)];
    /// Node `n`'s `set` at `t` pushed along transfer `x` into target `m`:
    /// its parts into `out` (none: nothing passes).
    fn push(&self, t: u32, n: Self::Node, set: &Self::Set, m: Self::Node, x: u32, out: &mut Vec<Self::Part>);
    /// Target `m`'s set at `t + 1` from its pushes `(m, part)` (`None`:
    /// none).
    fn close(&self, t: u32, m: Self::Node, parts: &[(Self::Node, Self::Part)], sc: &mut Self::Scratch) -> Result<Option<Self::Set>>;
    /// The sets made at frame `t` (a flow that keeps its sets keeps them).
    fn made(&mut self, _t: u32, _sets: &[(Self::Node, Self::Set)]) {}
}

/// Nodes a pushing task takes; targets a closing task takes.
const PUSH_CHUNK: usize = 64;
const CLOSE_CHUNK: usize = 256;

/// THE FORWARD of `flow` from `start` (frame 0) to `horizon`: per frame
/// every node with a set pushes along its out-edges, the pushes into each
/// target closed (both in parallel; the pushes' order is no matter: a
/// close is order-free). `visit(flow, t, sets)` sees each frame's sets (by
/// node) up to the last with one, at most `horizon`.
pub(crate) fn forward<F: Flow>(flow: &mut F, start: Vec<(F::Node, F::Set)>, horizon: u32, threads: usize, mut visit: impl FnMut(&F, u32, &[(F::Node, F::Set)]) -> Result<()>) -> Result<()> {
    let pool = crate::par::Pool::new(threads);
    let mut cur = start;
    for t in 0..=horizon {
        visit(flow, t, &cur)?;
        if t == horizon || cur.is_empty() {
            break;
        }
        flow.load(t, &cur.iter().map(|c| c.0).collect::<Vec<F::Node>>())?;
        let (fl, cur_ref) = (&*flow, &cur);
        let mut pushed: Vec<(F::Node, F::Part)> = pool
            .chunks(cur.len(), PUSH_CHUNK, |lo, hi| {
                let (mut out, mut tmp) = (Vec::new(), Vec::new());
                for (n, set) in &cur_ref[lo..hi] {
                    for &(m, x) in fl.out(*n) {
                        fl.push(t, *n, set, m, x, &mut tmp);
                        out.extend(tmp.drain(..).map(|p| (m, p)));
                    }
                }
                out
            })
            .into_iter()
            .flatten()
            .collect();
        pushed.sort_unstable_by_key(|e| e.0);
        // The targets, each with its pushes.
        let mut groups: Vec<(usize, usize)> = Vec::new();
        let mut a = 0;
        while a < pushed.len() {
            let b = a + pushed[a..].partition_point(|e| e.0 == pushed[a].0);
            groups.push((a, b));
            a = b;
        }
        let (pushed_ref, groups_ref) = (&pushed, &groups);
        let next: Vec<(F::Node, F::Set)> = pool
            .map_with(groups.len().div_ceil(CLOSE_CHUNK), || Ok(F::Scratch::default()), |sc, c| {
                let mut out = Vec::new();
                for &(a, b) in &groups_ref[c * CLOSE_CHUNK..((c + 1) * CLOSE_CHUNK).min(groups_ref.len())] {
                    let m = pushed_ref[a].0;
                    if let Some(set) = fl.close(t, m, &pushed_ref[a..b], sc)? {
                        out.push((m, set));
                    }
                }
                Ok(out)
            })?
            .into_iter()
            .flatten()
            .collect();
        drop(pushed);
        flow.made(t + 1, &next);
        cur = next;
    }
    Ok(())
}

/// The out-edges `inside_w` follows (from a non-win source to a node with a
/// set at or past the edge's frame; the others carry nothing), loaded at the
/// source's first reach: 8 B an edge, not the graph between W's nodes.
pub(crate) struct ReachEdges<'a, N: Rem = P8> {
    store: &'a EdgeStore<N>,
    w: &'a Winning<N::C>,
    /// The win states, sorted: never expanded, so never left.
    wins: Vec<Sk>,
    threads: usize,
    loaded: FxHashMap<u32, Box<[(u32, u32)]>>,
    edges: usize,
}

impl<'a, N: Rem> ReachEdges<'a, N> {
    pub(crate) fn new(store: &'a EdgeStore<N>, w: &'a Winning<N::C>, wins: Vec<Sk>, threads: usize) -> Self {
        ReachEdges { store, w, wins, threads, loaded: FxHashMap::default(), edges: 0 }
    }
    /// Load the out-edges of `nodes` not loaded yet (recorded at layer + 1).
    fn load(&mut self, nodes: &[u32]) -> Result<()> {
        let mut todo: Vec<(u32, Sk, u32)> = nodes.iter().filter(|i| !self.loaded.contains_key(i)).map(|&i| {
            let n = self.w.node(i);
            (n.layer + 1, n.id, i)
        }).collect();
        todo.sort_unstable();
        todo.dedup();
        let mut a = 0;
        while a < todo.len() {
            let f = todo[a].0;
            let b = a + todo[a..].partition_point(|x| x.0 == f);
            let ids: Vec<Sk> = todo[a..b].iter().map(|x| x.1).collect();
            let mut lists: Vec<Vec<(u32, u32)>> = vec![Vec::new(); ids.len()];
            for (k, dst, x) in self.store.out_of(f, &ids, self.threads)? {
                if self.wins.binary_search(&ids[k]).is_ok() {
                    continue;
                }
                if let Some(m) = self.w.index(dst).filter(|&m| self.w.node(m).last() >= f) {
                    lists[k].push((m, x));
                }
            }
            for (&(_, _, i), mut l) in todo[a..b].iter().zip(lists) {
                l.sort_unstable();
                l.dedup();
                self.edges += l.len();
                self.loaded.insert(i, l.into_boxed_slice());
            }
            a = b;
        }
        Ok(())
    }
    /// The nodes and edges loaded.
    pub(crate) fn sizes(&self) -> (usize, usize) {
        (self.loaded.len(), self.edges)
    }
}

/// A reached set past this many segments becomes its node's W (`inside_w`).
const REACH_SEGS: usize = 32;

/// `inside_w`'s flow: a node a W node, its set its own (`Some`) or its W
/// at the frame (`None`).
struct InsideW<'g, 'a, 'p, N: Rem> {
    g: &'g mut ReachEdges<'a, N>,
    pairs: &'p [Xfer<N>],
}

impl<N: Rem> Flow for InsideW<'_, '_, '_, N> {
    type Node = u32;
    type Set = Option<Region<N::C>>;
    type Part = Piece<Point<N>>;
    type Scratch = (Vec<Piece<Point<N>>>, Vec<Seg<Point<N>>>);

    fn load(&mut self, _t: u32, nodes: &[u32]) -> Result<()> {
        self.g.load(nodes)
    }

    fn out(&self, n: u32) -> &[(u32, u32)] {
        &self.g.loaded[&n]
    }

    /// Into the targets with a set at `t + 1`.
    fn push(&self, t: u32, n: u32, set: &Self::Set, m: u32, x: u32, out: &mut Vec<Self::Part>) {
        let w = self.g.w;
        if w.at(t + 1, m).is_none() {
            return;
        }
        let r: RegionRef<N::C> = match set {
            Some(r) => r.as_ref(),
            None => w.at(t, n).expect("a node reached in W has a set"),
        };
        let (x, y) = self.pairs[x as usize];
        r.push(x, y, out);
    }

    /// Clipped into `W_{t+1}(m)`, united.
    fn close(&self, t: u32, m: u32, parts: &[(u32, Self::Part)], (clipped, scratch): &mut Self::Scratch) -> Result<Option<Self::Set>> {
        let wm = self.g.w.at(t + 1, m).expect("a target has a winning set");
        clipped.clear();
        for &(_, pc) in parts {
            wm.clip_into(pc, clipped);
        }
        let r = Region::from_pieces(clipped, scratch);
        Ok(if r.is_empty() {
            None
        } else if r.n_segs() > REACH_SEGS {
            Some(None)
        } else {
            Some(Some(r))
        })
    }
}

/// FORWARD inside W: `R_t(i)`, the remainders with which node `i` is reached
/// at frame `t` from the start (at `point`) and still wins: `R_0 = {point}`,
/// `R_{t+1}(m) = U push_e(R_t(p)) ∩ W_{t+1}(m)`. Per node the LAST frame its
/// `R` is non-empty (`u32::MAX`: never). Sound as a filter where W's deadline
/// is: a concrete winner's remainder at `t` is in `W_t` of its node and is
/// the image of its remainder at `t - 1` along the recorded edge (the arc
/// model `check arcs` probes), so by induction it is in `R_t`; tighter, as a
/// node W lets win only from remainders no path brings is not marked. A set
/// past `REACH_SEGS` segments becomes `W_t` (a superset: the filter only
/// loosens), which bounds the memory (W's sets are shared).
#[allow(clippy::too_many_arguments)]
pub(crate) fn inside_w<N: Rem>(w: &Winning<N::C>, g: &mut ReachEdges<N>, pairs: &[Xfer<N>], start: Option<u32>, point: (Point<N>, Point<N>), horizon: u32, threads: usize) -> Result<Vec<u32>> {
    let n = w.len();
    let mut last = vec![u32::MAX; n];
    let mut cur: Vec<(u32, Option<Region<N::C>>)> = Vec::new();
    if let Some(s) = start {
        if w.at(0, s).is_some_and(|r| r.contains(point.0, point.1)) {
            let p = Piece { y: Seg { lo: point.1, hi: N::C::next(point.1) }, x: Seg { lo: point.0, hi: N::C::next(point.0) } };
            cur.push((s, Some(Region::from_pieces(&mut [p], &mut Vec::new()))));
        }
    }
    let (mut active_max, mut in_w) = (0usize, 0u64);
    forward(&mut InsideW { g, pairs }, cur, horizon, threads, |_, t, cur| {
        for (m, r) in cur {
            last[*m as usize] = t;
            in_w += (t > 0 && r.is_none()) as u64;
        }
        if t < horizon && !cur.is_empty() {
            active_max = active_max.max(cur.len());
            if t % 16 == 0 {
                let own = cur.iter().filter(|c| c.1.is_some()).count();
                let segs: usize = cur.iter().filter_map(|c| c.1.as_ref()).map(|r| r.n_segs()).sum();
                eprintln!("[arc] reach f{t:03}: {} nodes ({own} own sets, {segs} segments; the rest W's)", cur.len());
            }
        }
        Ok(())
    })?;
    let marked = last.iter().filter(|&&l| l != u32::MAX).count();
    eprintln!("[arc] reach: {marked} of {n} nodes reached inside W from the start (at most {active_max} a frame; {in_w} sets widened to W)");
    Ok(last)
}

/// THE EXACT REACH, a test oracle (the hull forward is checked against
/// it): over the tree in `dir` up to `horizon` (search steps) from its start
/// at `point` (`concrete::rem_of`'s encoding), `R_{t+1}(m) = U_e
/// push_e(R_t(p))` over the edges recorded at `t + 1`; `visit(t, states)`
/// sees each frame (by state). Per state the LAST frame it is reached at.
/// Exact in the remainder the tree's rows widen: an empty `R_t` means no
/// path of the level's game reaches the state at `t`; bounded by what the
/// tree kept (a level -1 drop, a filter). A set past `MAX_SEGS` segments
/// FAILS (the oracle never coarsens).
#[cfg(test)]
pub(crate) mod exact {
    use anyhow::Result;
    use rustc_hash::FxHashMap;

    use super::super::arcs::{Piece, Region, Seg, Transfer};
    use super::Flow;
    use crate::storage::Sk;
    use celeste_core::pico8_num::Pico8Num as P8;

    /// Sources per batch of out-edges read.
    const BATCH: usize = 1 << 20;

    /// The most x segments a reached set holds.
    const MAX_SEGS: usize = 1 << 10;

    /// The exact reach's flow: a node a state, its out-edges read at its
    /// layer + 1 (the tree's), a win's none.
    struct Exact {
        seeds: super::super::backward::Seeds<P8>,
        tree: crate::storage::layer::Tree,
        layers: Vec<crate::storage::layer::Layer>,
        wins: Vec<Sk>,
        out: FxHashMap<Sk, Box<[(Sk, u32)]>>,
        threads: usize,
    }

    impl Flow for Exact {
        type Node = Sk;
        type Set = Region;
        type Part = Piece;
        type Scratch = Vec<Seg>;

        fn load(&mut self, t: u32, nodes: &[Sk]) -> Result<()> {
            // Out-edges are recorded once, at a state's LAYER + 1 (a later
            // visit has the same successors): read at its first reach, by layer.
            while self.layers.len() <= t as usize {
                self.layers.push(self.tree.layer(self.layers.len() as u32, None, self.threads)?.0);
            }
            let mut by_layer: Vec<Vec<Sk>> = vec![Vec::new(); t as usize + 1];
            for &s in nodes {
                if self.wins.binary_search(&s).is_ok() || self.out.contains_key(&s) {
                    continue;
                }
                match (0..=t as usize).rev().find(|&f| self.layers[f].contains(s)) {
                    Some(f) => by_layer[f].push(s),
                    None => anyhow::bail!("f{t}: state {} is reached with an EXACT remainder set but is in no layer of the tree: the hull forward lost it", s.show()),
                }
            }
            for (f, ids) in by_layer.iter().enumerate() {
                for batch in ids.chunks(BATCH) {
                    let mut lists: Vec<Vec<(Sk, u32)>> = vec![Vec::new(); batch.len()];
                    for (k, dst, x) in self.seeds.store.out_of(f as u32 + 1, batch, self.threads)? {
                        lists[k].push((dst, x));
                    }
                    for (s, mut l) in batch.iter().zip(lists) {
                        l.sort_unstable();
                        l.dedup();
                        self.out.insert(*s, l.into_boxed_slice());
                    }
                }
            }
            Ok(())
        }

        fn out(&self, s: Sk) -> &[(Sk, u32)] {
            self.out.get(&s).map_or(&[], |l| l)
        }

        fn push(&self, _t: u32, _s: Sk, set: &Region, _m: Sk, x: u32, out: &mut Vec<Piece>) {
            let (tx, ty): (Transfer, Transfer) = self.seeds.pairs[x as usize];
            set.as_ref().push(tx, ty, out);
        }

        /// United (which must fit `MAX_SEGS` segments).
        fn close(&self, _t: u32, _m: Sk, parts: &[(Sk, Piece)], scratch: &mut Vec<Seg>) -> Result<Option<Region>> {
            let mut v: Vec<Piece> = parts.iter().map(|p| p.1).collect();
            let u = Region::try_from_pieces(&mut v, scratch, MAX_SEGS).ok_or_else(|| anyhow::anyhow!("an exactly reached set past {MAX_SEGS} segments"))?;
            Ok((!u.is_empty()).then_some(u))
        }
    }

    pub(crate) fn reach(dir: &std::path::Path, horizon: u32, point: (u32, u32), threads: usize, mut visit: impl FnMut(u32, &[(Sk, Region)]) -> Result<()>) -> Result<FxHashMap<Sk, u32>> {
        let seeds = super::super::backward::Seeds::<P8>::load(dir, horizon)?;
        let tree = crate::storage::layer::Tree::open(dir, horizon)?;
        let wins = seeds.win_ids();
        let start = seeds.start;
        let mut flow = Exact { seeds, tree, layers: Vec::new(), wins, out: FxHashMap::default(), threads };
        let start_piece = Piece { y: Seg { lo: point.1, hi: point.1 + 1 }, x: Seg { lo: point.0, hi: point.0 + 1 } };
        let mut last: FxHashMap<Sk, u32> = FxHashMap::default();
        super::forward(&mut flow, vec![(start, Region::from_pieces(&mut [start_piece], &mut Vec::new()))], horizon, threads, |_, t, cur| {
            for (s, _) in cur {
                last.insert(*s, t);
            }
            visit(t, cur)
        })?;
        Ok(last)
    }
}

#[cfg(test)]
mod tests {
    use super::super::backward::backward;
    use super::ReachEdges;
    use super::super::backward::tests::{apply, random_graph, rng, sid, TestTree};
    use super::super::arcs::CIRCLE;
    use rustc_hash::FxHashSet;

    /// Room (1,0) to its synthetic gate win (9,101), 35 frames: the exact
    /// reach holds the start, lies inside the tree, and holds the known
    /// route's state at every frame (stepped by the reference engine).
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels and a forward, ~25 CPU-s"]
    fn the_exact_reach_holds_the_known_route_inside_the_tree() {
        use crate::engines::reference::RefEngine;
        let s = crate::settings::Settings { win_at: Some((9, 9, 101, 101)), threads: Some(4), ..Default::default() };
        let dir = crate::testutil::TestDir::new("reach");
        let level = crate::abstraction::Level::parse("r0sxh").expect("level");
        crate::frame::grow_tree(&dir, 35, None, None, level, || Ok(Box::new(crate::compiled::FrameEngine::new(&s, level)?)), || Ok(vec![RefEngine::new(&s)?.initial()?.canonicalized()]), &s).expect("forward");
        let p0 = (super::super::arcs::point(0), super::super::arcs::point(0));
        let tree = crate::storage::layer::Tree::open(&dir, 35).expect("tree");
        let mut per_frame: Vec<Vec<crate::storage::Sk>> = Vec::new();
        let mut sets: Vec<Vec<(crate::storage::Sk, super::Region)>> = Vec::new();
        let last = super::exact::reach(&dir, 35, p0, 4, |_, cur| {
            per_frame.push(cur.iter().map(|c| c.0).collect());
            sets.push(cur.to_vec());
            Ok(())
        })
        .expect("reach");
        assert_eq!(per_frame[0].len(), 1, "the start");
        // Every reached state is a tree state (of some layer up to its frame).
        for (t, states) in per_frame.iter().enumerate() {
            for st in states {
                assert!((0..=t as u32).any(|f| tree.mask_at(f, st.region, st.key).is_ok_and(|m| m >> st.local & 1 == 1)), "f{t}: a reached state no layer holds");
            }
        }
        // The known route (gates/known_room10_win9-101.txt, 33 frames): its
        // state at every frame is reached exactly.
        let known = crate::concrete::read_inputs(concat!(env!("CARGO_MANIFEST_DIR"), "/gates/known_room10_win9-101.txt")).expect("known route");
        let mut eng = RefEngine::new(&s).expect("engine");
        let mut row = eng.initial().expect("initial").canonicalized();
        for (i, &b) in known.iter().enumerate().take(33) {
            row = eng.step_one(&row, b).expect("one successor");
            let t = i + 1;
            let (shape, keys, cells) = crate::frame::widened_keys(&row, level, &tree.space).expect("keys");
            let si = tree.shapes.iter().position(|&h| h == shape).expect("a tree shape") as u32;
            let sk = crate::storage::Sk::at(si, keys[0].expect("a tree key"), cells[0]);
            let q = super::super::concrete::rem_of(&row).expect("rem");
            let hit = sets[t].iter().find(|c| c.0 == sk).map(|c| c.1.as_ref().contains(q.0, q.1));
            assert_eq!(hit, Some(true), "f{t}: the known route's state is not reached with its exact remainder");
        }
        // The start is reached at frame 0 only, and every frame up to the win
        // holds something.
        assert!(per_frame.iter().take(34).all(|f| !f.is_empty()));
        assert_eq!(last.get(&per_frame[0][0]), Some(&0));
        // Exact reach is a subset of the tree's states, and here a strict one.
        let tree_states: u64 = (0..=35).map(|f| tree.states(f)).sum();
        assert!((last.len() as u64) < tree_states, "{} states reached exactly of the tree's {tree_states}", last.len());
    }

    /// `inside_w` against the points themselves: from one start point every
    /// frame's reached set is finite, so walk every (node, point) through the
    /// edges whose guards take it, keeping those inside W.
    #[test]
    fn reach_is_the_forward_of_the_start_point_inside_w() {
        let mut r = rng(23);
        let mut nonempty = 0;
        for _ in 0..80 {
            let n = 3 + r(14);
            let (layers, edges, wins) = random_graph(&mut r, n, true);
            if edges.is_empty() {
                continue;
            }
            let horizon = 7;
            let tree = TestTree::new("reach", layers, &edges, wins.clone());
            let (store, pairs, seeds) = tree.open(horizon);
            let w = backward(&store, &pairs, &seeds, horizon, 2, None).expect("backward");
            let mut g = ReachEdges::new(&store, &w, tree.win_ids(), 2);
            let start = w.index(sid(0));
            let p0 = (r(CIRCLE), r(CIRCLE));
            let got = super::inside_w(&w, &mut g, &pairs, start, p0, horizon, 2).expect("reach");
            let mut want = vec![u32::MAX; w.len()];
            let inside = |t: u32, i: u32, p: (u32, u32)| w.index(sid(i)).and_then(|k| w.at(t, k)).is_some_and(|reg| reg.contains(p.0, p.1));
            let mut cur: FxHashSet<(u32, (u32, u32))> = FxHashSet::default();
            if inside(0, 0, p0) {
                cur.insert((0, p0));
                want[start.expect("a node") as usize] = 0;
            }
            for t in 0..horizon {
                let mut next = FxHashSet::default();
                for &(i, p) in &cur {
                    if wins.contains(&i) {
                        continue;
                    }
                    for e in edges.iter().filter(|e| e.0 == i) {
                        if e.2.takes(p.0) && e.3.takes(p.1) {
                            let q = (apply(e.2, p.0), apply(e.3, p.1));
                            if inside(t + 1, e.1, q) {
                                next.insert((e.1, q));
                                want[w.index(sid(e.1)).expect("inside W") as usize] = t + 1;
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
}
