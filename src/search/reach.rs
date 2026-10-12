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
//! with the reach (bal200: 219 s at f143). The pushes are merged per target.
//!
//! A target's set past `Region`'s 65535 segments is replaced by its
//! bounding box and counted as COARSENED: from then on that state's set,
//! and what it reaches, over-approximate (still sound for "unreachable").

use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

use super::arcs::{Piece, Region, Seg, Transfer};
use crate::storage::{Sk, GEOMETRY};

/// Sources per batch of out-edges read.
const BATCH: usize = 1 << 20;

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
    /// States whose set was coarsened to a bounding box arriving here, and
    /// the states whose set over-approximates (coarsened here or reached
    /// from an inexact state).
    pub coarsened: u64,
    pub inexact: u64,
}

/// What `reach` found: per frame its sums; per state the LAST frame it is
/// exactly reached at (the marks).
pub struct Reach {
    pub frames: Vec<FrameReach>,
    pub last: FxHashMap<Sk, u32>,
}

/// The union of `v` (deduplicated), and whether it was COARSENED: a union
/// past a `Region`'s 65535 x segments is replaced by its bounding box - a
/// superset, so the reach stays sound as an upper bound but is no longer
/// exact at that state (counted per frame: `FrameReach::coarsened`).
fn union(v: &mut Vec<Piece>, scratch: &mut Vec<Seg>) -> (Region, bool) {
    v.sort_unstable_by_key(|p| (p.y, p.x));
    v.dedup();
    if let Some(r) = Region::try_from_pieces(v, scratch) {
        return (r, false);
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
pub fn reach(dir: &std::path::Path, horizon: u32, point: (u32, u32), threads: usize, mut visit: impl FnMut(u32, &[(Sk, Region)], &FxHashSet<Sk>) -> Result<()>) -> Result<Reach> {
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
    let mut out: FxHashMap<Sk, Box<[(u32, u32)]>> = FxHashMap::default();
    // The tree's layers as loaded (frame `f`: the states first reached at `f`).
    let mut layers: Vec<crate::storage::layer::Layer> = Vec::new();
    let mut frames = Vec::new();
    let mut coarsened = 0u64;
    // The reached states whose set over-approximates.
    let mut inexact: FxHashSet<Sk> = FxHashSet::default();
    let sum = |t: u32, cur: &[(Sk, Region)]| -> FrameReach {
        let mut cells: Vec<u32> = cur.iter().map(|(s, _)| s.cell(&geo)).collect();
        cells.sort_unstable();
        cells.dedup();
        FrameReach { frame: t, states: cur.len() as u64, new: 0, points: cur.iter().map(|(_, r)| points(r)).sum(), cells: cells.len() as u64, forward_layer: tree.states(t), coarsened: 0, inexact: 0 }
    };
    for t in 0..=horizon {
        let mut new = 0;
        for (s, _) in &cur {
            new += last.insert(*s, t).is_none() as u64;
        }
        frames.push(FrameReach { coarsened, inexact: inexact.len() as u64, new, ..sum(t, &cur) });
        visit(t, &cur, &inexact)?;
        if t == horizon || cur.is_empty() {
            break;
        }
        let tt = std::time::Instant::now();
        let mut coarse: FxHashSet<Sk> = Default::default();
        let mut tainted: FxHashSet<u32> = Default::default();
        // A state's out-edges are recorded once, at the frame after its
        // LAYER (the forward expands a state when it first reaches it; a
        // later visit has the same successors). They are read the first
        // time a state is reached (grouped by its layer) and kept: a state
        // reached again costs no decoding.
        while layers.len() <= t as usize {
            layers.push(tree.layer(layers.len() as u32, None, threads)?.0);
        }
        let mut by_layer: Vec<Vec<Sk>> = vec![Vec::new(); t as usize + 1];
        for (s, _) in &cur {
            if wins.binary_search(s).is_ok() || out.contains_key(s) {
                continue;
            }
            let f = (0..=t as usize).rev().find(|&f| layers[f].contains(*s)).ok_or_else(|| anyhow::anyhow!("f{t}: reached state {} is in no layer", s.show()))?;
            by_layer[f].push(*s);
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
                    l.sort_unstable();
                    l.dedup();
                    out.insert(*s, l.into_boxed_slice());
                }
            }
        }
        // The pushes into each target, by target.
        let mut next: FxHashMap<u32, Vec<Piece>> = FxHashMap::default();
        let mut tmp = Vec::new();
        let mut scratch = Vec::new();
        for (s, r) in &cur {
            let Some(es) = out.get(s) else { continue };
            let bad = inexact.contains(s);
            for &(d, x) in es.iter() {
                let (tx, ty) = pairs[x as usize];
                tmp.clear();
                r.push(tx, ty, &mut tmp);
                if tmp.is_empty() {
                    continue;
                }
                if bad {
                    tainted.insert(d);
                }
                let v = next.entry(d).or_default();
                v.extend_from_slice(&tmp);
                // Keep a target's pieces compact: a union once they pile up.
                if v.len() > 256 {
                    let (u, c) = union(v, &mut scratch);
                    if c {
                        coarse.insert(names[d as usize]);
                    }
                    v.clear();
                    u.pieces(v);
                }
            }
        }
        let next: FxHashMap<Sk, Vec<Piece>> = next.into_iter().map(|(d, v)| (names[d as usize], v)).collect();
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
        inexact = coarse;
        inexact.extend(tainted.into_iter().map(|d| names[d as usize]));
        nx.sort_unstable_by_key(|e| e.0);
        eprintln!("[reach] f{:03} -> f{:03}: {} states -> {} ({} coarsened, {} inexact; {} states' edges held, {} names; {:.1} s)", t, t + 1, cur.len(), nx.len(), coarsened, inexact.len(), out.len(), names.len(), tt.elapsed().as_secs_f64());
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
        let r = super::reach(&dir, 35, p0, 4, |_, cur, inexact| {
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
        let _ = std::fs::remove_dir_all(&dir);
    }
}
