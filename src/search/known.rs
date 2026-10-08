//! A KNOWN solution against the search's pruning (`rewrite check-known`, and
//! `rewrite search --prefer` when the file wins by the horizon).
//!
//! The search prunes by several things - level -1, the objects ladder's
//! filter (the coarser level's REACHED nodes), the remainder-free BFS
//! marks, the winning sets `W_t` the concrete search looks states up in -
//! and each of them over a tree the kernels and the door built. If any is
//! wrong it silently removes real winners: a wrong "optimal" or a tie, never
//! a fake improvement (witnesses are replayed). A known route that wins by
//! the horizon must survive every one of them, so it is stepped through the
//! reference engine and checked at every search step `s`:
//!
//! * level -1 does not call its cell too late (every step past the start);
//! * the ladder's filter admits it (`MarkFilter::allowed`, every step past
//!   the start: mid-frame steps pass by construction, the boundaries are
//!   filtered);
//! * at a frame boundary (`s` a multiple of `steps_per_frame`, where a
//!   concrete state keys exactly as the tree's rows; mid-frame rows carry the
//!   split frame's cut widening, which `Rt2::widen_to` does not): its
//!   projection is a node of the arc graph (in the tree AND marked by the
//!   remainder-free BFS), whose `W_s` holds its exact remainder - the concrete
//!   search's own admission test - and, at a level that filters the next one,
//!   the node is REACHED (`arc_dp::reach`) with a deadline >= s. The start
//!   is the tree's frame-0 row (stored unwidened), its W the arc optimum's.
//!
//! An `rnd` fork makes several leaves: the route survives when ONE winning
//! lineage passes everywhere (the concrete search is exhaustive over leaves).
//! Otherwise the first failing step of each winning lineage is reported with
//! the check that failed, the node and the state, and the search FAILS.

use super::arc_dp::{concrete_engines, lookup_keys, lookup_view, rem_of, NodeKeys, Winning};
use crate::frame::{steps_per_frame, wins_of, Block, MarkFilter, Visited};

/// A known solution to check, and the filter the level's forward ran under.
pub struct Route<'a> {
    pub inputs: &'a [u8],
    pub filter: Option<&'a MarkFilter<'a>>,
}

/// What the arc phase holds for the check (`arc_dp::solve`).
pub struct Pruning<'a> {
    pub level: crate::abstraction::Level,
    /// In search steps.
    pub horizon: u32,
    pub node: &'a NodeKeys,
    /// The start's node: the tree holds the start as the engine made it,
    /// unwidened, so it is not looked up by its projection.
    pub start: u32,
    pub w: &'a Winning,
    /// This level's reached nodes (the next level's filter), if it has one.
    pub reached: Option<&'a Visited>,
    /// The tree's frame files `(layer, seq, file)`, for the report.
    pub files: &'a [(u32, u32, super::checkpoint::FrameFile)],
}

/// The route's concrete states per search step, each with its parent's
/// index at the step before, up to the step before the exit; the exit step
/// and the indices of its winning leaves' parents.
struct Run {
    layers: Vec<Vec<(u32, Block)>>,
    exit: Option<(u32, Vec<u32>)>,
}

fn run(inputs: &[u8], horizon: u32) -> anyhow::Result<(Run, bool)> {
    let spf = steps_per_frame();
    let (mut engines, seeded) = concrete_engines(1)?;
    let eng = &mut engines[0];
    let mut layers: Vec<Vec<(u32, Block)>> = vec![vec![(0, Block::keyed(eng.initial()?)?)]];
    for s in 0..horizon {
        let Some(&byte) = inputs.get((s / spf) as usize) else { break };
        let (mut next, mut seen, mut won) = (Vec::new(), rustc_hash::FxHashSet::default(), Vec::new());
        for (p, (_, b)) in layers.last().expect("the start").iter().enumerate() {
            for c in eng.step(b.rt2(), byte)? {
                if wins_of(c.rt2())?.iter().any(|&x| x) {
                    won.push(p as u32);
                } else if seen.insert((c.rt2().clone_block().row_keys_canonical()[0], c.positions()?[0])) {
                    next.push((p as u32, c));
                }
            }
        }
        if !won.is_empty() {
            return Ok((Run { layers, exit: Some((s + 1, won)) }, seeded));
        }
        if next.is_empty() {
            break;
        }
        layers.push(next);
    }
    Ok((Run { layers, exit: None }, seeded))
}

/// Check the route against one level's pruning. `Ok(None)`: it does not win
/// by the horizon (nothing to check); `Ok(Some(f))`: it wins during frame
/// `f` and survives everything; an error: it is PRUNED (the report printed).
pub fn check(route: &Route, p: &Pruning) -> anyhow::Result<Option<u32>> {
    let t0 = std::time::Instant::now();
    let spf = steps_per_frame();
    let (run, seeded) = run(route.inputs, p.horizon)?;
    let Some((exit, winners)) = &run.exit else {
        eprintln!("[known] the known route does not exit by f{} ({} inputs): not checked", p.horizon / spf, route.inputs.len());
        return Ok(None);
    };
    let frame = exit.div_ceil(spf);
    let l1 = crate::frame::level_minus_one();
    // Per step and state: the first check it fails (`None`: it passes).
    let mut verdicts: Vec<Vec<Option<Miss>>> = Vec::with_capacity(run.layers.len());
    for (s, layer) in run.layers.iter().enumerate() {
        let s = s as u32;
        verdicts.push(layer.iter().map(|(_, b)| verdict(b, s, seeded, l1, route, p)).collect::<anyhow::Result<_>>()?);
    }
    // A state is good when it and every state before it on its lineage pass.
    let mut good: Vec<Vec<bool>> = Vec::with_capacity(run.layers.len());
    for (s, layer) in run.layers.iter().enumerate() {
        let row = layer.iter().enumerate().map(|(i, &(parent, _))| verdicts[s][i].is_none() && (s == 0 || good[s - 1][parent as usize])).collect();
        good.push(row);
    }
    let last = run.layers.len() - 1;
    let states: usize = run.layers.iter().map(Vec::len).sum();
    if winners.iter().any(|&w| good[last][w as usize]) {
        eprintln!(
            "[known] level {}: the known route (exit f{frame}) survives every pruning step at h{}: {states} states over {} steps, {}; {:.2} s",
            p.level,
            p.horizon,
            exit,
            [
                Some("node and W at every frame boundary"),
                l1.map(|_| "level -1"),
                route.filter.map(|_| "the ladder's filter"),
                p.reached.map(|_| "reached (the next level's filter)"),
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .join(", "),
            t0.elapsed().as_secs_f64()
        );
        return Ok(Some(frame));
    }
    // The report, per winning lineage (at most 3): where the route LEAVES the
    // tree (the forward's loss: what the arc phase misses after it is a
    // consequence), else its first failing step (the backward's loss).
    let mut lineages: Vec<Vec<usize>> = Vec::new();
    for &w in winners.iter().take(3) {
        let mut idx = vec![0usize; run.layers.len()];
        idx[last] = w as usize;
        for s in (1..=last).rev() {
            idx[s - 1] = run.layers[s][idx[s]].0 as usize;
        }
        if !lineages.contains(&idx) {
            lineages.push(idx);
        }
    }
    let mut first = u32::MAX;
    for idx in &lineages {
        let miss = |s: usize| verdicts[s][idx[s]].as_ref();
        let (s, what) = match (0..=last).find(|&s| miss(s).is_some_and(|m| m.forward)) {
            Some(s) => (s, "LEAVES THE TREE"),
            None => ((0..=last).find(|&s| miss(s).is_some()).expect("a pruned lineage fails somewhere"), "is dropped by the arc phase"),
        };
        first = first.min(s as u32);
        eprintln!("[known] PRUNED at level {}: the known route (exit f{frame}) {what} at step {s} ({}):", p.level, at_frame(s as u32, spf));
        for t in s.saturating_sub(spf as usize)..=(s + spf as usize).min(last) {
            let (_, b) = &run.layers[t][idx[t]];
            let (x, y) = rem_of(b.rt2())?;
            eprintln!(
                "[known]   step {t} ({}): {}\n[known]     state {} (remainder point ({x}, {y}))",
                at_frame(t as u32, spf),
                miss(t).map_or("passes", |m| m.what.as_str()),
                crate::search::inspect::player_summary(b.rt2(), 0)
            );
        }
    }
    anyhow::bail!(
        "the known route (exit f{frame}) is PRUNED at level {} by step {first} ({}): a real winner the search drops - the [known] report above names the check",
        p.level,
        at_frame(first, spf)
    )
}

/// A step as the frame it belongs to.
fn at_frame(s: u32, spf: u32) -> String {
    if s % spf == 0 {
        format!("frame {}", s / spf)
    } else {
        format!("mid-frame {}", s / spf + 1)
    }
}

/// A failed check: the report's line, and whether the FORWARD lost the
/// state (level -1, the ladder's filter, not in the tree) rather than the
/// arc phase (not marked, outside W, not reached).
struct Miss {
    forward: bool,
    what: String,
}

fn forward(what: String) -> Option<Miss> {
    Some(Miss { forward: true, what })
}

fn backward(what: String) -> Option<Miss> {
    Some(Miss { forward: false, what })
}

/// The first check state `b` at step `s` fails.
fn verdict(b: &Block, s: u32, seeded: bool, l1: Option<(u32, &crate::trace::level_minus_one::CostToGo)>, route: &Route, p: &Pruning) -> anyhow::Result<Option<Miss>> {
    let (shape, keys, cells) = lookup_keys(b, p.level, seeded)?;
    let (key, cell) = (keys[0], cells[0]);
    let node = format!("node shape {shape:#x} key {:#x}:{:#x} cell {cell} {:?}", key.0, key.1, super::pos_graph::cell_xy(cell));
    // The forward's filters run at every flush, so on every step but the
    // start (the initial block is never flushed).
    if s > 0 {
        if let Some((h, table)) = l1 {
            if table.too_late(shape, cell, s, h) {
                return Ok(forward(format!("LEVEL -1 drops it: d {:?} from step {s} exceeds its horizon {h} ({node})", table.d_of(shape, cell))));
            }
        }
        if let Some(f) = route.filter {
            let mut row = lookup_view(b.rt2(), seeded)?;
            crate::frame::widen_rt2_to(&mut row, p.level);
            if !f.allowed(&row, s)?[0] {
                return Ok(forward(format!(
                    "THE LADDER'S FILTER drops it: its projection onto {} has deadline {:?}, step {s} needs one >= {s} ({node})",
                    f.coarser(),
                    f.deadlines(&row)?[0]
                )));
            }
        }
    }
    if s % steps_per_frame() != 0 {
        return Ok(None);
    }
    let Some(i) = (if s == 0 { Some(p.start) } else { p.node.get(shape, key, cell) }) else {
        return Ok(match in_tree(p.files, s, shape, key, cell) {
            Some((layer, seq, row)) => backward(format!(
                "NOT MARKED: in the tree (layer {layer} s{seq} r{row}) but the remainder-free BFS finds no recorded path from it to a win by step {} - a successor on the route was lost ({node})",
                p.horizon
            )),
            None => forward(format!("NOT IN THE TREE by step {s}: the forward lost it since the step before (the kernels, the door, or a filter on a mid-frame step; `rewrite follow` shows its parent's kernel successors) ({node})")),
        });
    };
    let (x, y) = rem_of(b.rt2())?;
    match p.w.at(s, i) {
        None => return Ok(backward(format!("W_{s} of its node is EMPTY ({node})"))),
        Some(r) if !r.contains(x, y) => return Ok(backward(format!("its remainder ({x}, {y}) is OUTSIDE W_{s} of its node, {r:?} ({node})"))),
        Some(_) => {}
    }
    // The start is reached whenever its W holds it (`arc_dp::reach`).
    if let Some(m) = p.reached.filter(|_| s > 0) {
        let d = m.deadline(shape, key, cell);
        if !d.is_some_and(|d| u32::from(d) >= s) {
            return Ok(backward(format!("NOT REACHED inside W (`arc_dp::reach`): deadline {d:?}, step {s} needs one >= {s} - the next level's filter drops it ({node})")));
        }
    }
    Ok(None)
}

/// Where the tree holds a row `(shape, key, cell)` at a layer <= `s`.
fn in_tree(files: &[(u32, u32, super::checkpoint::FrameFile)], s: u32, shape: u64, key: (u64, u64), cell: u32) -> Option<(u32, u32, u32)> {
    files
        .iter()
        .filter(|(layer, _, f)| *layer <= s && f.shape_hash() == shape)
        .find_map(|(layer, seq, f)| f.rows_of_cell(cell).into_iter().flatten().find(|&r| f.key_at(r) == key).map(|r| (*layer, *seq, r)))
}
