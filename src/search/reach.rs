//! `rewrite reach`: EXACT-REMAINDER FORWARD REACHABILITY over a finished
//! tree's recorded edges - the arc phase's `reach` with W = everything.
//! From the start state at its exact remainder, frame by frame, every
//! reached state's remainder set is pushed along its out-edges through
//! their transfers (`RegionRef::push`) and the pushes into a target united:
//! `R_0 = {start: point}`, `R_{t+1}(m) = U_e push_e(R_t(p))` over the edges
//! `p -> m` recorded at frame `t + 1`. A win state is not left.
//!
//! The forward's tree over-approximates the game (a row's remainder is
//! widened to the whole circle at the boundary); `R` is exact in the
//! remainder: a state with an empty `R_t` is reached at `t` by no path of
//! the level's game from the start (the level's other widenings stand; at
//! an exact-objects level, none). Bounded by the tree: what its forward did
//! not keep (a level -1 drop, a filter) `R` cannot hold.
//!
//! Streamed like the backward: per frame only the reached states are held,
//! with their sets. A state's out-edges are decoded once, the first frame
//! it is reached (in batches of `BATCH` sources from its layer's target
//! blocks, `arc_dp::out_edges`), and kept as `(target, transfer)` numbers
//! (8 B an edge): decoding them again every frame made a frame cost grow
//! with the reach (219 s a frame on a 13B-state tree). The pushes are merged
//! per target.
//!
//! A target's set past `MAX_SEGS` x segments (a cloud of the remainders
//! many paths leave) is COARSENED: its pieces snapped outward to the
//! coarsest-needed grid of `2^k` points per axis (`k` the smallest that
//! fits). From then on that state's set, and what it reaches, OVER-
//! approximate - still sound for "unreachable", an upper bound on the
//! rest - and are counted as inexact with the largest `k` behind them.
//!
//! A frame's pushes and unions run in parallel over its targets (the
//! frame's edges grouped by target with a counting sort).

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::arcs::{Piece, Region, Seg, Transfer, CIRCLE};
use crate::storage::{Sk, GEOMETRY};

/// Sources per batch of out-edges read.
const BATCH: usize = 1 << 20;

/// Targets per parallel task of a frame's pushes and unions (taken costliest
/// first).
const TARGET_CHUNK: usize = 16;

/// The most x segments a reached set keeps exactly (far below a `Region`'s
/// 65535: the sets are clouds of the remainders many paths leave, ~10 B a
/// segment, and a big tree's frames hold millions of states - 16384 took
/// 53 GB by f164 of a 13B-state tree).
const MAX_SEGS: usize = 1 << 10;

/// The most segments a state's first-arrival union keeps (coarsened past
/// it like a reached set): held for every state ever reached, it is most
/// of the memory (1024 segments: 15 GB by f140 of a 13B-state tree).
/// Coarser, it grows less often: the entries counted are a LOWER bound
/// (every one is a real growth), the frames a state is reached at the
/// upper.
const ACC_SEGS: usize = 32;

/// What a reach keeps besides the reached sets.
#[derive(Clone, Copy, Debug)]
pub struct ReachOpts {
    /// The most x segments a reached set keeps exactly (`MAX_SEGS`
    /// default): lower trades precision (more inexact states, supersets)
    /// for memory.
    pub max_segs: usize,
    /// Track FIRST ARRIVAL (`FrameReach::entered`, `Reach::entries`): a
    /// union per state ever reached, the run's biggest memory after the
    /// frame's sets.
    pub first_arrival: bool,
}

impl Default for ReachOpts {
    fn default() -> Self {
        ReachOpts { max_segs: MAX_SEGS, first_arrival: false }
    }
}

/// One frame's exact reach, summed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameReach {
    pub frame: u32,
    /// States with a non-empty set, those of them reached here for the
    /// first time, and their sets' points.
    pub states: u64,
    pub new: u64,
    pub points: u64,
    /// Distinct player cells among them (`None`: no player).
    pub cells: u64,
    /// The forward's layer at this frame (states first reached here, rows
    /// kept), for scale.
    pub forward_layer: u64,
    /// States whose set was coarsened arriving here; the states whose set
    /// over-approximates (coarsened here or reached from an inexact
    /// state), and the coarsest grid behind them (`2^k` points, 0 exact).
    pub coarsened: u64,
    pub inexact: u64,
    pub max_k: u8,
    /// FIRST ARRIVAL in (state, remainder): the states whose union of sets
    /// so far (`R_0..R_t`) grew here - what an exact-remainder forward that
    /// keeps each (state, remainder) at its first frame would expand at `t`
    /// - and their out-edge records (its emissions; 0 at the horizon).
    pub entered: u64,
    pub entered_edges: u64,
    /// The out-edge records of every reached state (the upper bound on the
    /// entries': a state entering at every frame it is reached).
    pub reached_edges: u64,
}

/// A frame's edge pushes: every edge of a reached state, and those whose
/// image is not empty.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Pushes {
    pub edges: u64,
    pub nonempty: u64,
}

/// What `reach` found: per frame its sums; per state the LAST frame it is
/// exactly reached at (the marks).
pub struct Reach {
    pub frames: Vec<FrameReach>,
    pub last: FxHashMap<Sk, u32>,
    /// Per frame `t` (to `t + 1`), the pushes.
    pub pushes: Vec<Pushes>,
    /// The out-edges of the states ever reached (not wins): as recorded
    /// (the forward's records), distinct (target, transfer), and of those
    /// the ones whose image was ever non-empty.
    pub edges_raw: u64,
    pub edges: u64,
    pub edges_pushed: u64,
    /// `entries[n]`: the states ever reached that entered (first arrival)
    /// at `n` frames.
    pub entries: Vec<u64>,
}

/// An edge's transfer id's bit marking an edge whose image was non-empty
/// at some frame.
const PUSHED: u32 = 1 << 31;

/// Reached states per parallel task of the first-arrival unions.
const GROW_CHUNK: usize = 1024;

/// `s` widened outward to the grid of `2^k` points.
fn snap(s: Seg, k: u32) -> Seg {
    let m = (1u32 << k) - 1;
    Seg { lo: s.lo & !m, hi: ((s.hi + m) & !m).min(CIRCLE) }
}

/// The union of `v` (deduplicated, left as the union's input), and its
/// coarsening: 0 exact, else the `k` its pieces were snapped to (outward,
/// to the grid of `2^k` points per axis) to fit `max` segments - the
/// smallest from `k0` (a target already coarsened to `k0` within the frame
/// starts there: no ladder of failed sweeps on every union).
fn union(v: &mut Vec<Piece>, scratch: &mut Vec<Seg>, k0: u8, max: usize) -> (Region, u8) {
    v.sort_unstable_by_key(|p| (p.y, p.x));
    v.dedup();
    if k0 == 0 {
        if let Some(r) = Region::try_from_pieces(v, scratch, max) {
            return (r, 0);
        }
    }
    let mut w = Vec::with_capacity(v.len());
    for k in (k0 as u32).max(1)..=16 {
        w.clear();
        w.extend(v.iter().map(|p| Piece { y: snap(p.y, k), x: snap(p.x, k) }));
        w.sort_unstable_by_key(|p| (p.y, p.x));
        w.dedup();
        if let Some(r) = Region::try_from_pieces(&mut w, scratch, max) {
            return (r, k as u8);
        }
    }
    unreachable!("at k 16 every piece is the whole torus")
}

/// The number of points of a set.
pub fn points(r: &Region) -> u64 {
    r.slabs().map(|(y, xs)| (y.hi - y.lo) as u64 * xs.map(|x| (x.hi - x.lo) as u64).sum::<u64>()).sum()
}

/// EXACT REACH over the tree in `dir` up to `horizon` (search steps) from
/// its start at `point` (`arc_dp::rem_of`'s encoding). `visit(sums,
/// pushes, states, inexact)` sees every frame's sums once they are whole
/// (its pushes to the next frame, none at the last), its reached states
/// and sets, and the inexact ones with their coarsest grid (the caller's
/// facts, written as they come).
#[allow(clippy::type_complexity)]
pub fn reach(dir: &std::path::Path, horizon: u32, point: (u32, u32), threads: usize, opts: ReachOpts, mut visit: impl FnMut(&FrameReach, Option<Pushes>, &[(Sk, Region)], &FxHashMap<Sk, u8>) -> Result<()>) -> Result<Reach> {
    anyhow::ensure!((1..=super::arcs::MAX_SEGS).contains(&opts.max_segs), "max_segs {} outside 1..={}", opts.max_segs, super::arcs::MAX_SEGS);
    let geo = GEOMETRY;
    let seeds = super::arc_dp::Seeds::load(dir, horizon)?;
    let tree = crate::storage::layer::Tree::open(dir, horizon)?;
    let wins = seeds.win_ids();
    let pairs: &[(Transfer, Transfer)] = &seeds.pairs;
    let start_piece = Piece { y: Seg { lo: point.1, hi: point.1 + 1 }, x: Seg { lo: point.0, hi: point.0 + 1 } };
    let mut cur: Vec<(Sk, Region)> = vec![(seeds.start, Region::from_pieces(&mut [start_piece], &mut Vec::new()))];
    let mut last: FxHashMap<Sk, u32> = FxHashMap::default();
    // Every state seen as an edge's target, numbered; the out-edges read so
    // far, `(target, transfer)` by number (8 B an edge).
    let mut index: FxHashMap<Sk, u32> = FxHashMap::default();
    let mut names: Vec<Sk> = Vec::new();
    let mut out: FxHashMap<Sk, (u32, Box<[(u32, u32)]>)> = FxHashMap::default();
    // First arrival: per state ever reached, the union of its sets so far
    // and the frames it entered at.
    let mut acc: FxHashMap<Sk, (Region, u32)> = FxHashMap::default();
    let mut pushes: Vec<Pushes> = Vec::new();
    let mut acc_segs = 0u64;
    // The tree's layers as loaded (frame `f`: the states first reached at `f`).
    let mut layers: Vec<crate::storage::layer::Layer> = Vec::new();
    let mut frames = Vec::new();
    let mut coarsened = 0u64;
    // The reached states whose set over-approximates, with their `k`.
    let mut inexact: FxHashMap<Sk, u8> = FxHashMap::default();
    let pool = crate::par::Pool::new(threads);
    let sum = |t: u32, cur: &[(Sk, Region)]| -> FrameReach {
        let mut cells: Vec<u32> = cur.iter().map(|(s, _)| s.cell(&geo)).collect();
        cells.sort_unstable();
        cells.dedup();
        FrameReach { frame: t, states: cur.len() as u64, points: cur.iter().map(|(_, r)| points(r)).sum(), cells: cells.len() as u64, forward_layer: tree.states(t), ..Default::default() }
    };
    for t in 0..=horizon {
        let mut new = 0;
        for (s, _) in &cur {
            new += last.insert(*s, t).is_none() as u64;
        }
        let max_k = inexact.values().copied().max().unwrap_or(0);
        frames.push(FrameReach { new, coarsened, inexact: inexact.len() as u64, max_k, ..sum(t, &cur) });
        if t == horizon || cur.is_empty() {
            visit(&frames[frames.len() - 1], None, &cur, &inexact)?;
            break;
        }
        let tt = std::time::Instant::now();
        // A state's out-edges are recorded once, at the frame after its
        // LAYER (the forward expands a state when it first reaches it; a
        // later visit has the same successors). They are read the first
        // time a state is reached (grouped by its layer) and kept: a state
        // reached again costs no decoding.
        while layers.len() <= t as usize {
            layers.push(tree.layer(layers.len() as u32, None, threads)?.0);
        }
        let mut by_layer: Vec<Vec<Sk>> = vec![Vec::new(); t as usize + 1];
        let mut outside = 0u64;
        for (s, _) in &cur {
            if wins.binary_search(s).is_ok() || out.contains_key(s) {
                continue;
            }
            match (0..=t as usize).rev().find(|&f| layers[f].contains(*s)) {
                Some(f) => by_layer[f].push(*s),
                // A tree holds every state an exact remainder reaches (a
                // hull forward's hulls hold the exact sets), so a state
                // outside it is reached only by a COARSENED set (a
                // superset): it has no edges, and nothing exact reaches it.
                None if inexact.contains_key(s) => outside += 1,
                None => anyhow::bail!("f{t}: state {} is reached with an EXACT remainder set but is in no layer of the tree: the forward lost it", s.show()),
            }
        }
        if outside > 0 {
            eprintln!("[reach] f{t:03}: {outside} states reached by coarsened sets only, outside the tree (left)");
        }
        for (f, ids) in by_layer.iter().enumerate() {
            for batch in ids.chunks(BATCH) {
                let mut lists: Vec<Vec<(u32, u32)>> = vec![Vec::new(); batch.len()];
                for (k, dst, x) in super::arc_dp::out_edges(&seeds.store, f as u32 + 1, batch, threads)? {
                    let n = names.len() as u32;
                    let d = *index.entry(dst).or_insert_with(|| {
                        names.push(dst);
                        n
                    });
                    lists[k].push((d, x));
                }
                for (s, mut l) in batch.iter().zip(lists) {
                    let raw = l.len() as u32;
                    anyhow::ensure!(l.iter().all(|e| e.1 < PUSHED), "a transfer id past 2^31");
                    l.sort_unstable();
                    l.dedup();
                    out.insert(*s, (raw, l.into_boxed_slice()));
                }
            }
        }
        let t_read = tt.elapsed().as_secs_f64();
        // First arrival: a state enters where `R_t` adds to its union so far.
        let acc_ref = &acc;
        let grow_tasks = if opts.first_arrival { cur.len().div_ceil(GROW_CHUNK) } else { 0 };
        let grown = pool.map_with(
            grow_tasks,
            || Ok((Vec::new(), Vec::new())),
            |(v, scratch): &mut (Vec<Piece>, Vec<Seg>), c| {
                let mut res: Vec<(u32, Region)> = Vec::new();
                for i in c * GROW_CHUNK..((c + 1) * GROW_CHUNK).min(cur.len()) {
                    let (s, r) = &cur[i];
                    match acc_ref.get(s) {
                        None => {
                            v.clear();
                            r.pieces(v);
                            res.push((i as u32, union(v, scratch, 0, ACC_SEGS).0));
                        }
                        Some((a, _)) => {
                            v.clear();
                            a.pieces(v);
                            r.pieces(v);
                            let (u, _) = union(v, scratch, 0, ACC_SEGS);
                            if u != *a {
                                res.push((i as u32, u));
                            }
                        }
                    }
                }
                Ok(res)
            },
        )?;
        let (mut entered, mut entered_edges) = (0u64, 0u64);
        for (i, u) in grown.into_iter().flatten() {
            let s = cur[i as usize].0;
            let e = acc.entry(s).or_insert_with(|| (Region::default(), 0));
            acc_segs = acc_segs + u.n_segs() as u64 - e.0.n_segs() as u64;
            e.0 = u;
            e.1 += 1;
            entered += 1;
            entered_edges += out.get(&s).map_or(0, |o| o.0 as u64);
        }
        let reached_edges = cur.iter().map(|(s, _)| out.get(s).map_or(0, |o| o.0 as u64)).sum();
        if let Some(f) = frames.last_mut() {
            (f.entered, f.entered_edges, f.reached_edges) = (entered, entered_edges, reached_edges);
        }
        // The frame's edges grouped by target (a counting sort): `inc[start[d]
        // ..start[d + 1]]` the (source in `cur`, transfer) pairs into `d`.
        let edges_of: Vec<&[(u32, u32)]> = cur.iter().map(|(s, _)| out.get(s).map_or(&[][..], |e| &e.1[..])).collect();
        // Each (source, edge)'s place in the frame's pushed bits.
        let mut off: Vec<u64> = Vec::with_capacity(cur.len() + 1);
        off.push(0);
        for es in &edges_of {
            off.push(off[off.len() - 1] + es.len() as u64);
        }
        let pushed: Vec<std::sync::atomic::AtomicU64> = (0..off[cur.len()].div_ceil(64)).map(|_| Default::default()).collect();
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
            for (j, &(d, _)) in es.iter().enumerate() {
                let p = &mut fill[d as usize];
                inc[*p as usize] = (i as u32, j as u32);
                *p += 1;
            }
        }
        drop(fill);
        // The targets, the costliest first (the segments pushed into each):
        // a few fragmented sets dominate a frame, and a worker that takes
        // one late would hold the frame up.
        let mut targets: Vec<(u64, u32)> = (0..names.len() as u32)
            .filter(|&d| start[d as usize + 1] > start[d as usize])
            .map(|d| (inc[start[d as usize] as usize..start[d as usize + 1] as usize].iter().map(|&(i, _)| cur[i as usize].1.n_segs() as u64).sum(), d))
            .collect();
        targets.sort_unstable_by(|a, b| b.cmp(a));
        // Per target: every push into it, united (a union whenever the
        // pieces pass twice the last union's: a set of many pieces is not
        // re-united on every push), coarsened where it must be.
        let (cur_ref, inexact_ref, names_ref, edges_ref, off_ref, pushed_ref) = (&cur, &inexact, &names, &edges_of, &off, &pushed);
        let parts = pool.map_with(
            targets.len().div_ceil(TARGET_CHUNK),
            || Ok((Vec::new(), Vec::new(), Vec::new())),
            |(v, tmp, scratch): &mut (Vec<Piece>, Vec<Piece>, Vec<Seg>), c| {
                let mut res: Vec<(Sk, Region, u8, u8)> = Vec::new();
                for &(_, d) in &targets[c * TARGET_CHUNK..((c + 1) * TARGET_CHUNK).min(targets.len())] {
                    v.clear();
                    let (mut lim, mut snapped, mut from) = (256usize, 0u8, 0u8);
                    let grid = |p: Piece, k: u8| Piece { y: snap(p.y, k as u32), x: snap(p.x, k as u32) };
                    for &(i, j) in &inc[start[d as usize] as usize..start[d as usize + 1] as usize] {
                        let (s, r) = &cur_ref[i as usize];
                        let x = edges_ref[i as usize][j as usize].1 & !PUSHED;
                        let (tx, ty) = pairs[x as usize];
                        tmp.clear();
                        r.push(tx, ty, tmp);
                        if tmp.is_empty() {
                            continue;
                        }
                        let g = off_ref[i as usize] + j as u64;
                        pushed_ref[(g / 64) as usize].fetch_or(1 << (g % 64), std::sync::atomic::Ordering::Relaxed);
                        from = from.max(inexact_ref.get(s).copied().unwrap_or(0));
                        if snapped == 0 {
                            v.extend_from_slice(tmp);
                        } else {
                            v.extend(tmp.iter().map(|&p| grid(p, snapped)));
                        }
                        if v.len() > lim {
                            let (u, k) = union(v, scratch, snapped, opts.max_segs);
                            snapped = snapped.max(k);
                            v.clear();
                            u.pieces(v);
                            lim = (2 * v.len()).max(256);
                        }
                    }
                    let (u, k) = union(v, scratch, snapped, opts.max_segs);
                    snapped = snapped.max(k);
                    if !u.is_empty() {
                        res.push((names_ref[d as usize], u, snapped, from));
                    }
                }
                Ok(res)
            },
        )?;
        drop(inc);
        drop(edges_of);
        // The non-empty pushes, marked on their edges.
        let bit = |g: u64| pushed[(g / 64) as usize].load(std::sync::atomic::Ordering::Relaxed) >> (g % 64) & 1 == 1;
        let mut nonempty = 0u64;
        for (i, (s, _)) in cur.iter().enumerate() {
            let (a, b) = (off[i], off[i + 1]);
            if !(a..b).any(bit) {
                continue;
            }
            let es = &mut out.get_mut(s).expect("a pushed state's edges").1;
            for (j, e) in es.iter_mut().enumerate() {
                if bit(a + j as u64) {
                    e.1 |= PUSHED;
                    nonempty += 1;
                }
            }
        }
        pushes.push(Pushes { edges: off[cur.len()], nonempty });
        drop(pushed);
        let mut nx: Vec<(Sk, Region)> = Vec::new();
        let mut next_inexact: FxHashMap<Sk, u8> = FxHashMap::default();
        coarsened = 0;
        for (s, r, snapped, from) in parts.into_iter().flatten() {
            coarsened += (snapped > 0) as u64;
            if snapped.max(from) > 0 {
                next_inexact.insert(s, snapped.max(from));
            }
            nx.push((s, r));
        }
        nx.sort_unstable_by_key(|e| e.0);
        eprintln!(
            "[reach] f{:03} -> f{:03}: {} states -> {} ({} coarsened, {} inexact; {} states' edges held, {} names; segments {} -> {}, first arrival {} entered, {} held, {} segments; rss {:.1} GB; read {:.1} s, total {:.1} s)",
            t,
            t + 1,
            cur.len(),
            nx.len(),
            coarsened,
            next_inexact.len(),
            out.len(),
            names.len(),
            cur.iter().map(|c| c.1.n_segs() as u64).sum::<u64>(),
            nx.iter().map(|c| c.1.n_segs() as u64).sum::<u64>(),
            entered,
            acc.len(),
            acc_segs,
            crate::metrics::current_rss_gb(),
            t_read,
            tt.elapsed().as_secs_f64()
        );
        visit(&frames[frames.len() - 1], pushes.last().copied(), &cur, &inexact)?;
        cur = nx;
        inexact = next_inexact;
    }
    let edges_raw = out.values().map(|o| o.0 as u64).sum();
    let edges = out.values().map(|o| o.1.len() as u64).sum();
    let edges_pushed = out.values().map(|o| o.1.iter().filter(|e| e.1 & PUSHED != 0).count() as u64).sum();
    let mut entries = Vec::new();
    for (_, n) in acc.values() {
        if entries.len() <= *n as usize {
            entries.resize(*n as usize + 1, 0);
        }
        entries[*n as usize] += 1;
    }
    Ok(Reach { frames, last, pushes, edges_raw, edges, edges_pushed, entries })
}

#[cfg(test)]
mod tests {
    /// A cloud of isolated points past `MAX_SEGS` segments is coarsened to
    /// a superset that fits, on the finest grid that does; a small one is
    /// united exactly.
    #[test]
    fn a_union_past_its_segments_snaps_to_a_superset() {
        use super::super::arcs::{Piece, Seg};
        let pt = |x: u32, y: u32| Piece { y: Seg { lo: y, hi: y + 1 }, x: Seg { lo: x, hi: x + 1 } };
        // 3 * MAX_SEGS points, 3 a row 5 apart in x, the rows 3 apart (never
        // adjacent).
        let cloud: Vec<Piece> = (0..3 * super::MAX_SEGS as u32).map(|i| pt(1000 + 5 * (i % 3), 3 * (i / 3))).collect();
        let (r, k) = super::union(&mut cloud.clone(), &mut Vec::new(), 0, super::MAX_SEGS);
        assert!(k > 0 && r.n_segs() <= super::MAX_SEGS, "k {k}, {} segments", r.n_segs());
        assert!(cloud.iter().all(|p| r.contains(p.x.lo, p.y.lo)), "the coarsened set holds every point");
        let (r1, k1) = super::union(&mut cloud.iter().copied().map(|p| super::Piece { y: super::snap(p.y, k as u32 - 1), x: super::snap(p.x, k as u32 - 1) }).collect(), &mut Vec::new(), 0, super::MAX_SEGS);
        assert!(k1 > 0 || r1.n_segs() > super::MAX_SEGS, "a finer grid would have fit: k {k} is not the smallest");
        let (small, k0) = super::union(&mut cloud[..100].to_vec(), &mut Vec::new(), 0, super::MAX_SEGS);
        assert_eq!((k0, super::points(&small)), (0, 100));
    }

    /// Room (1,0) to its synthetic gate win (9,101), a forward of 35 frames:
    /// the exact reach holds the start, is a subset of every frame's tree
    /// states, and holds the known route's state at every frame (the gate's
    /// witness, stepped by the reference engine and keyed in the tree).
    #[test]
    fn the_exact_reach_holds_the_known_route_inside_the_tree() {
        use crate::frame::Block;
        use crate::trace::refengine::RefEngine;
        let s = crate::settings::Settings { win_at: Some((9, 9, 101, 101)), threads: Some(4), ..Default::default() };
        let dir = std::env::temp_dir().join(format!("celeste-reach-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let level = crate::abstraction::Level::parse("r0sxh").expect("level");
        crate::frame::grow_tree(&dir, 35, None, None, level, || Ok(Box::new(crate::compiled::FrameEngine::new(&s, level)?)), || Ok(vec![Block::canonical(RefEngine::new(&s)?.initial()?)?]), &s).expect("forward");
        let p0 = (super::super::arcs::point(0), super::super::arcs::point(0));
        let tree = crate::storage::layer::Tree::open(&dir, 35).expect("tree");
        let mut per_frame: Vec<Vec<crate::storage::Sk>> = Vec::new();
        let mut sets: Vec<Vec<(crate::storage::Sk, super::Region)>> = Vec::new();
        let r = super::reach(&dir, 35, p0, 4, super::ReachOpts { first_arrival: true, ..Default::default() }, |_, _, cur, inexact| {
            assert!(inexact.is_empty(), "room (1,0) to h35 needs no coarsening");
            per_frame.push(cur.iter().map(|c| c.0).collect());
            sets.push(cur.to_vec());
            Ok(())
        })
        .expect("reach");
        assert_eq!(r.frames[0].states, 1, "the start");
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
        let mut row = Block::canonical(eng.initial().expect("initial")).expect("canonical");
        let geo = crate::storage::GEOMETRY;
        for (i, &b) in known.iter().enumerate().take(33) {
            row = eng.step_one(row.rt2(), b).expect("one successor");
            let t = i + 1;
            let (shape, keys, cells) = crate::frame::widened_keys(&row, level, &tree.space).expect("keys");
            let si = tree.shapes.iter().position(|&h| h == shape).expect("a tree shape") as u32;
            let sk = crate::storage::Sk::at(&geo, si, keys[0].expect("a tree key"), cells[0]);
            let q = super::super::arc_dp::rem_of(row.rt2()).expect("rem");
            let hit = sets[t].iter().find(|c| c.0 == sk).map(|c| c.1.contains(q.0, q.1));
            assert_eq!(hit, Some(true), "f{t}: the known route's state is not reached with its exact remainder");
        }
        // The start is reached at frame 0 only, and every frame up to the win
        // holds something.
        assert!(r.frames.iter().take(34).all(|f| f.states > 0));
        assert_eq!(r.last.get(&per_frame[0][0]), Some(&0));
        // Exact reach is a subset of the tree's states, and here a strict one.
        let tree_states: u64 = r.frames.iter().map(|f| f.forward_layer).sum();
        assert!((r.last.len() as u64) < tree_states, "{} states reached exactly of the tree's {tree_states}", r.last.len());
        assert_eq!(r.frames.iter().map(|f| f.new).sum::<u64>(), r.last.len() as u64, "every reached state is new at one frame");
        // First arrival: every state reached before the horizon enters at
        // least once, at most at every frame it is reached.
        assert_eq!(r.entries.first().copied().unwrap_or(0), 0);
        assert_eq!(r.entries.iter().sum::<u64>(), r.frames[..35].iter().map(|f| f.new).sum::<u64>());
        let entered: u64 = r.frames.iter().map(|f| f.entered).sum();
        assert_eq!(entered, r.entries.iter().enumerate().map(|(n, &c)| n as u64 * c).sum::<u64>());
        assert!(entered <= r.frames[..35].iter().map(|f| f.states).sum::<u64>());
        assert!(r.frames.iter().map(|f| f.entered_edges).sum::<u64>() <= r.frames.iter().map(|f| f.reached_edges).sum::<u64>());
        // Edges: the distinct ones of the recorded, the pushed of those.
        assert!(r.edges_pushed > 0 && r.edges_pushed <= r.edges && r.edges <= r.edges_raw);
        assert!(r.pushes.iter().map(|p| p.nonempty).sum::<u64>() >= r.edges_pushed);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
