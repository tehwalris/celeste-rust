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
//! Streamed like the backward: per frame the reached states are read in
//! batches of `BATCH` sources (their out-edges decoded from the frame's
//! target blocks, `arc_dp::out_edges`), the pushes merged per target; the
//! memory is the frame's reached states and their sets.

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::arcs::{Piece, Region, Seg, Transfer};
use crate::storage::{Sk, GEOMETRY};

/// Sources per batch of out-edges.
const BATCH: usize = 1 << 20;

/// One frame's exact reach, summed.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameReach {
    pub frame: u32,
    /// States with a non-empty set, and their sets' points.
    pub states: u64,
    pub points: u64,
    /// Distinct player cells among them (`None`: no player).
    pub cells: u64,
    /// The forward's layer at this frame (states first reached here, rows
    /// kept), for scale.
    pub forward_layer: u64,
    /// States whose set was coarsened to a bounding box (`MAX_PIECES`).
    pub coarsened: u64,
}

/// What `reach` found: per frame its sums; per state the LAST frame it is
/// exactly reached at (the marks).
pub struct Reach {
    pub frames: Vec<FrameReach>,
    pub last: FxHashMap<Sk, u32>,
}

/// Distinct pieces past which a state's set is COARSENED to its bounding
/// box before the union (a `Region` holds at most 65535 x segments): a
/// superset, so the reach stays sound as an upper bound but is no longer
/// exact at that state (counted per frame: `FrameReach::coarsened`).
const MAX_PIECES: usize = 32768;

/// The union of `v` (deduplicated; coarsened past `MAX_PIECES`), and
/// whether it was coarsened.
fn union(v: &mut Vec<Piece>, scratch: &mut Vec<Seg>) -> (Region, bool) {
    v.sort_unstable_by_key(|p| (p.y, p.x));
    v.dedup();
    if v.len() <= MAX_PIECES {
        return (Region::from_pieces(v, scratch), false);
    }
    let (ylo, yhi) = (v.iter().map(|p| p.y.lo).min().unwrap_or(0), v.iter().map(|p| p.y.hi).max().unwrap_or(0));
    let (xlo, xhi) = (v.iter().map(|p| p.x.lo).min().unwrap_or(0), v.iter().map(|p| p.x.hi).max().unwrap_or(0));
    let mut bbox = [Piece { y: Seg { lo: ylo, hi: yhi }, x: Seg { lo: xlo, hi: xhi } }];
    (Region::from_pieces(&mut bbox, scratch), true)
}

/// The number of points of a set.
pub fn points(r: &Region) -> u64 {
    r.slabs().map(|(y, xs)| (y.hi - y.lo) as u64 * xs.map(|x| (x.hi - x.lo) as u64).sum::<u64>()).sum()
}

/// EXACT REACH over the tree in `dir` up to `horizon` (search steps) from
/// its start at `point` (`arc_dp::rem_of`'s encoding). `visit(t, states)`
/// sees every frame's reached states and sets (the caller's facts).
pub fn reach(dir: &std::path::Path, horizon: u32, point: (u32, u32), threads: usize, mut visit: impl FnMut(u32, &[(Sk, Region)]) -> Result<()>) -> Result<Reach> {
    let geo = GEOMETRY;
    let seeds = super::arc_dp::Seeds::load(dir, horizon)?;
    let tree = crate::storage::layer::Tree::open(dir, horizon)?;
    let wins = seeds.win_ids();
    let pairs: &[(Transfer, Transfer)] = &seeds.pairs;
    let start_piece = Piece { y: Seg { lo: point.1, hi: point.1 + 1 }, x: Seg { lo: point.0, hi: point.0 + 1 } };
    let mut cur: Vec<(Sk, Region)> = vec![(seeds.start, Region::from_pieces(&mut [start_piece], &mut Vec::new()))];
    let mut last: FxHashMap<Sk, u32> = FxHashMap::default();
    // The tree's layers as loaded (frame `f`: the states first reached at `f`).
    let mut layers: Vec<crate::storage::layer::Layer> = Vec::new();
    let mut frames = Vec::new();
    let mut coarsened = 0u64;
    let sum = |t: u32, cur: &[(Sk, Region)]| -> FrameReach {
        let mut cells: Vec<u32> = cur.iter().map(|(s, _)| s.cell(&geo)).collect();
        cells.sort_unstable();
        cells.dedup();
        FrameReach { frame: t, states: cur.len() as u64, points: cur.iter().map(|(_, r)| points(r)).sum(), cells: cells.len() as u64, forward_layer: tree.states(t), coarsened: 0 }
    };
    for t in 0..=horizon {
        for (s, _) in &cur {
            last.insert(*s, t);
        }
        frames.push(FrameReach { coarsened, ..sum(t, &cur) });
        visit(t, &cur)?;
        if t == horizon || cur.is_empty() {
            break;
        }
        let tt = std::time::Instant::now();
        let mut coarse: rustc_hash::FxHashSet<Sk> = Default::default();
        // A state's out-edges are recorded once, at the frame after its
        // LAYER (the forward expands a state when it first reaches it; a
        // later visit has the same successors): each reached state is
        // grouped by its layer, the most recent first.
        while layers.len() <= t as usize {
            layers.push(tree.layer(layers.len() as u32, None, threads)?.0);
        }
        let mut by_layer: Vec<Vec<usize>> = vec![Vec::new(); t as usize + 1];
        for (i, (s, _)) in cur.iter().enumerate() {
            if wins.binary_search(s).is_ok() {
                continue;
            }
            let f = (0..=t as usize).rev().find(|&f| layers[f].contains(*s)).ok_or_else(|| anyhow::anyhow!("f{t}: reached state {} is in no layer", s.show()))?;
            by_layer[f].push(i);
        }
        // The pushes into each target, merged batch by batch.
        let mut next: FxHashMap<Sk, Vec<Piece>> = FxHashMap::default();
        for (f, idx) in by_layer.iter().enumerate() {
            for batch in idx.chunks(BATCH) {
                let ids: Vec<Sk> = batch.iter().map(|&i| cur[i].0).collect();
                let edges = super::arc_dp::out_edges(&seeds.store, f as u32 + 1, &ids, threads)?;
                let mut tmp = Vec::new();
                for (k, dst, x) in edges {
                    let (tx, ty) = pairs[x as usize];
                    tmp.clear();
                    cur[batch[k]].1.push(tx, ty, &mut tmp);
                    if !tmp.is_empty() {
                        next.entry(dst).or_default().extend_from_slice(&tmp);
                    }
                }
                // Keep each target's pieces compact: a union once they pile up.
                let mut scratch = Vec::new();
                for (dst, v) in next.iter_mut() {
                    if v.len() > 64 {
                        let (r, c) = union(v, &mut scratch);
                        if c {
                            coarse.insert(*dst);
                        }
                        v.clear();
                        r.pieces(v);
                    }
                }
            }
        }
        let mut scratch = Vec::new();
        let mut nx: Vec<(Sk, Region)> = next
            .into_iter()
            .filter_map(|(s, mut v)| {
                let (r, c) = union(&mut v, &mut scratch);
                if c {
                    coarse.insert(s);
                }
                (!r.is_empty()).then_some((s, r))
            })
            .collect();
        coarsened = coarse.len() as u64;
        nx.sort_unstable_by_key(|e| e.0);
        eprintln!("[reach] f{:03} -> f{:03}: {} states -> {} ({} coarsened; {:.1} s)", t, t + 1, cur.len(), nx.len(), coarsened, tt.elapsed().as_secs_f64());
        cur = nx;
    }
    Ok(Reach { frames, last })
}

#[cfg(test)]
mod tests {
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
        let r = super::reach(&dir, 35, p0, 4, |_, cur| {
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
        let _ = std::fs::remove_dir_all(&dir);
    }
}
