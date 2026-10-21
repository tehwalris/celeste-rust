//! FORWARD REACHABILITY over a finished tree's recorded edges: from the
//! start at its exact remainder, every reached state's remainder set pushed
//! along its out-edges (`RegionRef::push`), the pushes into a target united;
//! a win state is not left. Out-edges are read at a state's first reach and
//! kept, 8 B an edge (decoding them every frame made a frame's cost grow
//! with the reach). `inside_w`: inside W, the next level's filter. `exact`
//! (a test oracle): with W = everything, what the hull forward's soundness
//! is checked against.

use rustc_hash::FxHashMap;

use super::arcs::{Circle, Piece, Region, RegionRef, Seg};
use super::backward::Winning;
use super::rem::{Point, Rem, Xfer};
use crate::storage::edges::EdgeStore;
use crate::storage::{Sk, GEOMETRY};
use celeste_core::pico8_num::Pico8Num as P8;

/// The out-edges recorded at `frame` of `sources` (layer `frame - 1`):
/// `(k, target, transfer)` for `sources[k]`, read from the target regions
/// the frame's index names for the sources' regions.
fn out_edges<N: Rem>(store: &EdgeStore<N>, frame: u32, sources: &[Sk], threads: usize) -> anyhow::Result<Vec<(usize, Sk, u32)>> {
    let geo = GEOMETRY;
    let mut wanted: rustc_hash2::FxHashMap<(u32, (u64, u64)), Vec<(u32, usize)>> = Default::default();
    let mut regions: Vec<u32> = Vec::new();
    for (k, s) in sources.iter().enumerate() {
        wanted.entry((s.shape(&geo), s.key)).or_default().push((s.cell(&geo), k));
        regions.push(s.region);
    }
    regions.sort_unstable();
    regions.dedup();
    let mut targets: Vec<u32> = regions.iter().flat_map(|&r| store.targets_of(frame, r)).collect();
    targets.sort_unstable();
    targets.dedup();
    let parts = crate::storage::wave::par_map(&targets, threads, |&region| -> anyhow::Result<Vec<(usize, Sk, u32)>> {
        let mut dec = crate::storage::edges::Decoder::default();
        let mut out = Vec::new();
        for seg in store.segments(frame, region) {
            dec.bundles(&seg, |b| {
                let Some(cands) = wanted.get(&(b.s_shape, b.s_key)) else { return };
                crate::storage::edges::expand(&geo, region, &b, |src, dst| {
                    let c = src.cell(&geo);
                    for &(wc, k) in cands {
                        if wc == c {
                            out.push((k, dst, b.x));
                        }
                    }
                });
            })?;
        }
        Ok(out)
    });
    let mut out = Vec::new();
    for p in parts {
        out.extend(p?);
    }
    Ok(out)
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
    fn load(&mut self, nodes: impl Iterator<Item = u32>) -> anyhow::Result<()> {
        let mut todo: Vec<(u32, Sk, u32)> = nodes.filter(|i| !self.loaded.contains_key(i)).map(|i| {
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
            for (k, dst, x) in out_edges(self.store, f, &ids, self.threads)? {
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
    fn of(&self, i: u32) -> &[(u32, u32)] {
        &self.loaded[&i]
    }
    /// The nodes and edges loaded.
    pub(crate) fn sizes(&self) -> (usize, usize) {
        (self.loaded.len(), self.edges)
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
pub(crate) fn inside_w<N: Rem>(w: &Winning<N::C>, g: &mut ReachEdges<N>, pairs: &[Xfer<N>], start: Option<u32>, point: (Point<N>, Point<N>), horizon: u32, threads: usize) -> anyhow::Result<Vec<u32>> {
    let n = w.len();
    let pool = crate::par::Pool::new(threads);
    let mut last = vec![u32::MAX; n];
    const REACH_SEGS: usize = 32;
    // The current frame's sets, by node (`None`: W_t of the node).
    let mut cur: Vec<(u32, Option<Region<N::C>>)> = Vec::new();
    if let Some(s) = start {
        if w.at(0, s).is_some_and(|r| r.contains(point.0, point.1)) {
            let p = Piece { y: Seg { lo: point.1, hi: N::C::next(point.1) }, x: Seg { lo: point.0, hi: N::C::next(point.0) } };
            cur.push((s, Some(Region::from_pieces(&mut [p], &mut Vec::new()))));
            last[s as usize] = 0;
        }
    }
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
        // Each reached node's set pushed along its out-edges into the
        // targets with a set at t + 1.
        g.load(cur.iter().map(|c| c.0))?;
        let (cur_ref, g) = (&cur, &*g);
        let mut pushed: Vec<(u32, Piece<Point<N>>)> = pool.chunks(cur.len(), 64, |lo, hi| {
            let (mut out, mut tmp) = (Vec::new(), Vec::new());
            for (p, r) in &cur_ref[lo..hi] {
                let r: RegionRef<N::C> = match r {
                    Some(r) => r.as_ref(),
                    None => w.at(t, *p).expect("a node reached in W has a set"),
                };
                for &(m, x) in g.of(*p) {
                    if w.at(t + 1, m).is_some() {
                        let (x, y) = pairs[x as usize];
                        tmp.clear();
                        r.push(x, y, &mut tmp);
                        out.extend(tmp.iter().map(|&pc| (m, pc)));
                    }
                }
            }
            out
        })
        .into_iter()
        .flatten()
        .collect();
        pushed.sort_unstable_by_key(|e| e.0);
        // The targets, each with its pushes: clipped into W_{t+1}, united.
        let mut groups: Vec<(usize, usize)> = Vec::new();
        let mut a = 0;
        while a < pushed.len() {
            let b = a + pushed[a..].partition_point(|e| e.0 == pushed[a].0);
            groups.push((a, b));
            a = b;
        }
        let pushed_ref = &pushed;
        let next: Vec<(u32, Option<Region<N::C>>)> = pool.chunks(groups.len(), 256, |lo, hi| {
            let (mut clipped, mut scratch) = (Vec::new(), Vec::new());
            groups[lo..hi]
                .iter()
                .filter_map(|&(a, b)| {
                    let m = pushed_ref[a].0;
                    let wm = w.at(t + 1, m).expect("a target has a winning set");
                    clipped.clear();
                    for &(_, pc) in &pushed_ref[a..b] {
                        wm.clip_into(pc, &mut clipped);
                    }
                    let r = Region::from_pieces(&mut clipped, &mut scratch);
                    if r.is_empty() {
                        None
                    } else if r.n_segs() > REACH_SEGS {
                        Some((m, None))
                    } else {
                        Some((m, Some(r)))
                    }
                })
                .collect::<Vec<_>>()
        })
        .into_iter()
        .flatten()
        .collect();
        for (m, r) in &next {
            in_w += r.is_none() as u64;
            last[*m as usize] = t + 1;
        }
        cur = next;
    }
    let marked = last.iter().filter(|&&l| l != u32::MAX).count();
    eprintln!("[arc] reach: {marked} of {n} nodes reached inside W from the start (at most {active_max} a frame; {in_w} sets widened to W)");
    Ok(last)
}

/// THE EXACT REACH, a test oracle (the hull forward is checked against
/// it): over the tree in `dir` up to `horizon` (search steps) from its start
/// at `point` (`concrete::rem_of`'s encoding), `R_{t+1}(m) = U_e
/// push_e(R_t(p))` over the edges recorded at `t + 1`; `visit(t, states)`
/// sees each frame. Per state the LAST frame it is reached at. Exact in the
/// remainder the tree's rows widen: an empty `R_t` means no path of the
/// level's game reaches the state at `t`; bounded by what the tree kept (a
/// level -1 drop, a filter). A set past `MAX_SEGS` segments FAILS (the
/// oracle never coarsens).
#[cfg(test)]
pub(crate) mod exact {
    use anyhow::Result;
    use rustc_hash::FxHashMap;

    use super::super::arcs::{Piece, Region, Seg, Transfer};
    use super::out_edges;
    use crate::storage::Sk;
    use celeste_core::pico8_num::Pico8Num as P8;

    /// Sources per batch of out-edges read.
    const BATCH: usize = 1 << 20;

    /// Targets per parallel task of a frame's pushes (taken costliest first).
    const TARGET_CHUNK: usize = 16;

    /// The most x segments a reached set holds.
    const MAX_SEGS: usize = 1 << 10;

    /// The union of `v`, which must fit `MAX_SEGS` segments.
    fn union(v: &mut [Piece], scratch: &mut Vec<Seg>) -> Result<Region> {
        Region::try_from_pieces(v, scratch, MAX_SEGS).ok_or_else(|| anyhow::anyhow!("an exactly reached set past {MAX_SEGS} segments"))
    }

    pub(crate) fn reach(dir: &std::path::Path, horizon: u32, point: (u32, u32), threads: usize, mut visit: impl FnMut(u32, &[(Sk, Region)]) -> Result<()>) -> Result<FxHashMap<Sk, u32>> {
        let seeds = super::super::backward::Seeds::<P8>::load(dir, horizon)?;
        let tree = crate::storage::layer::Tree::open(dir, horizon)?;
        let wins = seeds.win_ids();
        let pairs: &[(Transfer, Transfer)] = &seeds.pairs;
        let start_piece = Piece { y: Seg { lo: point.1, hi: point.1 + 1 }, x: Seg { lo: point.0, hi: point.0 + 1 } };
        let mut cur: Vec<(Sk, Region)> = vec![(seeds.start, Region::from_pieces(&mut [start_piece], &mut Vec::new()))];
        let mut last: FxHashMap<Sk, u32> = FxHashMap::default();
        // Every state seen as an edge's target, numbered; the out-edges read
        // so far, `(target, transfer)` by number.
        let mut index: FxHashMap<Sk, u32> = FxHashMap::default();
        let mut names: Vec<Sk> = Vec::new();
        let mut out: FxHashMap<Sk, Box<[(u32, u32)]>> = FxHashMap::default();
        // The tree's layers as loaded (frame `f`: the states first reached at `f`).
        let mut layers: Vec<crate::storage::layer::Layer> = Vec::new();
        let pool = crate::par::Pool::new(threads);
        for t in 0..=horizon {
            for (s, _) in &cur {
                last.insert(*s, t);
            }
            if t == horizon || cur.is_empty() {
                visit(t, &cur)?;
                break;
            }
            // Out-edges are recorded once, at a state's LAYER + 1 (a later
            // visit has the same successors): read at its first reach, by layer.
            while layers.len() <= t as usize {
                layers.push(tree.layer(layers.len() as u32, None, threads)?.0);
            }
            let mut by_layer: Vec<Vec<Sk>> = vec![Vec::new(); t as usize + 1];
            for (s, _) in &cur {
                if wins.binary_search(s).is_ok() || out.contains_key(s) {
                    continue;
                }
                match (0..=t as usize).rev().find(|&f| layers[f].contains(*s)) {
                    Some(f) => by_layer[f].push(*s),
                    None => anyhow::bail!("f{t}: state {} is reached with an EXACT remainder set but is in no layer of the tree: the hull forward lost it", s.show()),
                }
            }
            for (f, ids) in by_layer.iter().enumerate() {
                for batch in ids.chunks(BATCH) {
                    let mut lists: Vec<Vec<(u32, u32)>> = vec![Vec::new(); batch.len()];
                    for (k, dst, x) in out_edges(&seeds.store, f as u32 + 1, batch, threads)? {
                        let n = names.len() as u32;
                        let d = *index.entry(dst).or_insert_with(|| {
                            names.push(dst);
                            n
                        });
                        lists[k].push((d, x));
                    }
                    for (s, mut l) in batch.iter().zip(lists) {
                        l.sort_unstable();
                        l.dedup();
                        out.insert(*s, l.into_boxed_slice());
                    }
                }
            }
            // The frame's edges grouped by target (a counting sort):
            // `inc[start[d]..start[d + 1]]` the (source in `cur`, transfer)
            // pairs into `d`.
            let edges_of: Vec<&[(u32, u32)]> = cur.iter().map(|(s, _)| out.get(s).map_or(&[][..], |e| &e[..])).collect();
            let mut start = vec![0u32; names.len() + 1];
            for es in &edges_of {
                for &(d, _) in es.iter() {
                    start[d as usize + 1] += 1;
                }
            }
            for i in 1..start.len() {
                start[i] += start[i - 1];
            }
            let mut fill = start.clone();
            let mut inc = vec![(0u32, 0u32); start[names.len()] as usize];
            for (i, es) in edges_of.iter().enumerate() {
                for &(d, x) in es.iter() {
                    let p = &mut fill[d as usize];
                    inc[*p as usize] = (i as u32, x);
                    *p += 1;
                }
            }
            let targets: Vec<u32> = (0..names.len() as u32).filter(|&d| start[d as usize + 1] > start[d as usize]).collect();
            // Per target every push into it, united whenever the pieces
            // pass twice the last union's (not on every push).
            let (cur_ref, names_ref) = (&cur, &names);
            let parts = pool.map_with(
                targets.len().div_ceil(TARGET_CHUNK),
                || Ok((Vec::new(), Vec::new(), Vec::new())),
                |(v, tmp, scratch): &mut (Vec<Piece>, Vec<Piece>, Vec<Seg>), c| {
                    let mut res: Vec<(Sk, Region)> = Vec::new();
                    for &d in &targets[c * TARGET_CHUNK..((c + 1) * TARGET_CHUNK).min(targets.len())] {
                        v.clear();
                        let mut lim = 256usize;
                        for &(i, x) in &inc[start[d as usize] as usize..start[d as usize + 1] as usize] {
                            let (tx, ty) = pairs[x as usize];
                            tmp.clear();
                            cur_ref[i as usize].1.as_ref().push(tx, ty, tmp);
                            v.extend_from_slice(tmp);
                            if v.len() > lim {
                                let u = union(v, scratch)?;
                                v.clear();
                                u.as_ref().pieces(v);
                                lim = (2 * v.len()).max(256);
                            }
                        }
                        let u = union(v, scratch)?;
                        if !u.is_empty() {
                            res.push((names_ref[d as usize], u));
                        }
                    }
                    Ok(res)
                },
            )?;
            let mut nx: Vec<(Sk, Region)> = parts.into_iter().flatten().collect();
            nx.sort_unstable_by_key(|e| e.0);
            visit(t, &cur)?;
            cur = nx;
        }
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
        let geo = crate::storage::GEOMETRY;
        for (i, &b) in known.iter().enumerate().take(33) {
            row = eng.step_one(&row, b).expect("one successor");
            let t = i + 1;
            let (shape, keys, cells) = crate::frame::widened_keys(&row, level, &tree.space).expect("keys");
            let si = tree.shapes.iter().position(|&h| h == shape).expect("a tree shape") as u32;
            let sk = crate::storage::Sk::at(&geo, si, keys[0].expect("a tree key"), cells[0]);
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
