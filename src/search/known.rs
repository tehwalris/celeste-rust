//! A KNOWN solution against the search's pruning (`rewrite check known`, and
//! `rewrite search --prefer` when the file wins by the horizon).
//!
//! The search prunes by several things - level -1, the objects ladder's
//! filter (the coarser level's REACHED nodes), the winning sets `W_t` the
//! concrete search looks states up in -
//! and each of them over a tree the kernels and the visited set built. If any is
//! wrong it silently removes real winners: a wrong "optimal" or a tie, never
//! a fake improvement (witnesses are replayed). A known route that wins by
//! the horizon must survive every one of them, so it is stepped through the
//! reference engine and checked at every search step `s`:
//!
//! * level -1 (the filter the TREE recorded, `TreeFilter::of_tree`, not the
//!   environment's) does not call its cell too late (every step past the start);
//! * the ladder's filter admits it (`MarkFilter::allowed`, every step past
//!   the start: mid-frame steps pass by construction, the boundaries are
//!   filtered);
//! * at a frame boundary (`Clock::boundary`: where a
//!   concrete state keys exactly as the tree's rows; mid-frame rows carry the
//!   split frame's cut widening, which `Rt2::widen_to` does not): its
//!   projection is a node of `W`'s support (in the tree AND with a winning
//!   set at some frame), whose `W_s` holds its exact remainder - the concrete
//!   search's own admission test - and, at a level that filters the next one,
//!   the node is REACHED (`reach::inside_w`) with a deadline >= s. The start
//!   is the tree's frame-0 row (stored unwidened), its W the arc optimum's.
//!
//! `rewrite check known` also checks THE HULL FORWARD'S CLAIM (`Hulls`;
//! whole frames only): at every step the route's state is in the tree by
//! then, its exact remainder inside its state's hull, and the step from its
//! parent a recorded edge whose transfer takes the parent's remainder to its
//! own - every concrete path the filters keep is in the tree, recorded.
//!
//! An `rnd` fork makes several leaves: the route survives when ONE winning
//! lineage passes everywhere (the concrete search is exhaustive over leaves).
//! Otherwise the first failing step of each winning lineage is reported with
//! the check that failed, the node and the state, and the search FAILS.
//! Where the tree does not hold the state, the report also steps its state
//! at the frame boundary before through the level's kernels (`explain`):
//! the route's state among their successors, or a SOUNDNESS gap.

use super::concrete::{concrete_engines, exact_state, lookup_keys, lookup_view, rem_of, Side};
use super::rem::Rem;
use crate::compiled::KernelLanes;
use crate::frame::{wins_of, Block, MarkFilter, Visited};
use crate::storage::Sk;
use celeste_core::arith::EngineNum;
use celeste_core::pico8_num::Pico8Num as P8;

/// A known solution to check, and the filter the level's forward ran under.
pub struct Route<'a, N: Rem = P8> {
    pub inputs: &'a [u8],
    pub filter: Option<&'a MarkFilter<'a, N>>,
    /// Also the hull forward's claim (`Hulls`): `rewrite check known`.
    pub hulls: bool,
}

/// What the hull forward's claim is checked against: the tree, its visited
/// set (each state's hull) and its recorded edges.
pub struct Hulls<N: Rem = P8> {
    tree: crate::storage::layer::Tree<N>,
    visited: crate::storage::visited::VisitedSet<N>,
    edges: crate::storage::edges::EdgeStore<N>,
    /// The tree's start (its one frame-0 state, stored as the engine made it).
    start: Sk,
}

impl<N: Rem> Hulls<N> {
    /// The tree in `dir` through step `horizon`.
    pub fn open(dir: &std::path::Path, horizon: u32, s: &crate::settings::Settings) -> anyhow::Result<Self> {
        let tree = crate::storage::layer::Tree::<N>::open(dir, horizon)?;
        let (visited, ..) = crate::frame::restore_visited::<N>(dir, horizon, s.room, s.threads())?;
        let edges = crate::storage::edges::EdgeStore::<N>::open(&dir.join("edges"), horizon)?;
        let starts: Vec<Sk> = tree.layer(0, None, s.threads())?.0.states_iter().collect();
        anyhow::ensure!(starts.len() == 1, "{}: {} states at frame 0", dir.display(), starts.len());
        Ok(Hulls { tree, visited, edges, start: starts[0] })
    }
}

/// What the arc phase holds for the check (`driver::solve`).
pub(super) struct Pruning<'a, N: Rem = P8> {
    /// The level's winning sets, as the concrete search admits states:
    /// its settings (the room, category, split frame), level, W, its nodes'
    /// keys and the tree's key space (concrete states keyed as its rows).
    pub side: &'a Side<'a, N>,
    /// In search steps.
    pub horizon: u32,
    /// The start's node (`None`: no winning set at any frame): the tree
    /// holds the start as the engine made it, unwidened, so it is not looked
    /// up by its projection.
    pub start: Option<u32>,
    /// This level's reached nodes (the next level's filter), if it has one.
    pub reached: Option<&'a Visited<N>>,
    /// The level -1 filter the tree was built under (`TreeFilter::of_tree`).
    pub minus_one: Option<crate::frame::MinusOne>,
    /// The tree's directory, for the report (its frame files read on a
    /// miss).
    pub dir: &'a std::path::Path,
    /// The hull forward's claim, checked at every step (`Route::hulls`).
    pub hulls: Option<&'a Hulls<N>>,
}

/// The route's concrete states per search step, each with its parent's
/// index at the step before, up to the step before the exit; the exit step
/// and the indices of its winning leaves' parents.
struct Run<N: EngineNum> {
    layers: Vec<Vec<(u32, Block<N>)>>,
    exit: Option<(u32, Vec<u32>)>,
}

fn run<N: Rem>(inputs: &[u8], horizon: u32, settings: &crate::settings::Settings) -> anyhow::Result<(Run<N>, bool)> {
    let clock = settings.clock();
    let (mut engines, seeded) = concrete_engines::<N>(1, settings)?;
    let eng = &mut engines[0];
    let mut layers: Vec<Vec<(u32, Block<N>)>> = vec![vec![(0, Block::canonical(eng.initial()?)?)]];
    for s in 0..horizon {
        // The step to `s + 1` is in frame `frame_of(s + 1)`: its input.
        let Some(&byte) = inputs.get(clock.frame_of(s + 1) as usize - 1) else { break };
        let (mut next, mut seen, mut won) = (Vec::new(), rustc_hash::FxHashSet::default(), Vec::new());
        for (p, (_, b)) in layers.last().expect("the start").iter().enumerate() {
            for c in eng.step(b.rt2(), byte)? {
                if wins_of(c.rt2(), settings)?.iter().any(|&x| x) {
                    won.push(p as u32);
                } else if seen.insert(exact_state(&c, c.positions()?[0])) {
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
pub(super) fn check<N: KernelLanes>(route: &Route<N>, p: &Pruning<N>) -> anyhow::Result<Option<u32>> {
    let t0 = std::time::Instant::now();
    let clock = p.side.s.clock();
    let (run, seeded) = run::<N>(route.inputs, p.horizon, p.side.s)?;
    let Some((exit, winners)) = &run.exit else {
        eprintln!("[known] the known route does not exit by f{} ({} inputs): not checked", clock.frame_of(p.horizon), route.inputs.len());
        return Ok(None);
    };
    let frame = clock.frame_of(*exit);
    let l1 = p.minus_one.map(|m| (m.h, m.table(p.side.s)));
    // Per step and state: the first check it fails (`None`: it passes), and
    // where the tree holds it (with `hulls`: its state and remainder).
    let mut verdicts: Vec<Vec<Option<Miss>>> = Vec::with_capacity(run.layers.len());
    let mut held: Vec<Vec<Option<Held<N>>>> = Vec::with_capacity(run.layers.len());
    for (s, layer) in run.layers.iter().enumerate() {
        let (mut v, mut h) = (Vec::with_capacity(layer.len()), Vec::with_capacity(layer.len()));
        for (parent, b) in layer {
            let from = if s == 0 { None } else { held[s - 1][*parent as usize] };
            let (miss, at) = verdict(b, s as u32, from, seeded, l1, route, p)?;
            v.push(miss);
            h.push(at);
        }
        verdicts.push(v);
        held.push(h);
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
            p.side.level,
            p.horizon,
            exit,
            [
                Some("node and W at every frame boundary"),
                p.hulls.map(|_| "in the tree, inside its hull, by a recorded edge at every step"),
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
    // The report, per winning lineage (at most 3): where the FORWARD loses
    // the route (a filter, the tree, a hull, an edge: what the arc phase
    // misses after it is a consequence), else its first failing step (the
    // backward's loss).
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
            Some(s) => (s, "is LOST BY THE FORWARD"),
            None => ((0..=last).find(|&s| miss(s).is_some()).expect("a pruned lineage fails somewhere"), "is dropped by the arc phase"),
        };
        first = first.min(s as u32);
        eprintln!("[known] PRUNED at level {}: the known route (exit f{frame}) {what} at step {s} ({}):", p.side.level, clock.show(s as u32));
        let per_frame = clock.per_frame as usize;
        for t in s.saturating_sub(per_frame)..=(s + per_frame).min(last) {
            let (_, b) = &run.layers[t][idx[t]];
            let (x, y) = rem_of::<N>(b.rt2())?;
            eprintln!(
                "[known]   step {t} ({}): {}\n[known]     state {} (remainder point ({x:?}, {y:?}))",
                clock.show(t as u32),
                miss(t).map_or("passes", |m| m.what.as_str()),
                crate::search::inspect::player_summary(b.rt2(), 0)
            );
        }
        if miss(s).is_some_and(|m| m.lost) {
            match explain(&run, idx, s, seeded, p) {
                Ok(lines) => lines.iter().for_each(|l| eprintln!("[known]   {l}")),
                Err(e) => eprintln!("[known]   (the kernels' successors could not be shown: {e:#})"),
            }
        }
    }
    anyhow::bail!(
        "the known route (exit f{frame}) is PRUNED at level {} by step {first} ({}): a real winner the search drops - the [known] report above names the check",
        p.side.level,
        clock.show(first)
    )
}

/// Why the forward lost the route's state at step `s` (a frame boundary,
/// on lineage `idx`): its state at the frame boundary before, as the tree
/// holds it, stepped to `s` by the level's kernels; the route's state
/// projected onto the level among their successors (the forward made it
/// and did not keep it: a win, level -1, a filter) or not (a SOUNDNESS gap
/// in the kernels), and the successors closest to it.
fn explain<N: KernelLanes>(run: &Run<N>, idx: &[usize], s: usize, seeded: bool, p: &Pruning<N>) -> anyhow::Result<Vec<String>> {
    use crate::search::inspect::{brief, project_onto, Proj};
    let per_frame = p.side.s.clock().per_frame as usize;
    let s0 = s.checked_sub(per_frame).ok_or_else(|| anyhow::anyhow!("the start is the tree's"))?;
    let tree = crate::storage::layer::Tree::<N>::open(p.dir, s as u32)?;
    let (sk, layer) = if s0 == 0 {
        (tree.layer(0, None, 1)?.0.states_iter().next().ok_or_else(|| anyhow::anyhow!("no frame-0 state"))?, 0)
    } else {
        let (shape, keys, cells) = lookup_keys(&run.layers[s0][idx[s0]].1, p.side.level, seeded, p.side.keys)?;
        let sk = keys[0].and_then(|k| tree.sk(shape, k, cells[0])).ok_or_else(|| anyhow::anyhow!("its state at step {s0} is not in the tree either"))?;
        (sk, tree.layer_of(sk, s0 as u32)?.ok_or_else(|| anyhow::anyhow!("its state at step {s0} is not in the tree by then"))?)
    };
    let kernels = crate::compiled::Kernels::<N>::new(p.side.s, p.side.level)?;
    let mut blocks = vec![Block::from_rt2(tree.row(layer, sk, p.side.s.room)?)];
    for k in 0..per_frame {
        blocks = crate::storage::wave::one_frame(&kernels, blocks, layer + 1 + k as u32)?.next;
    }
    let kset: std::collections::BTreeSet<Proj> = blocks.into_iter().flat_map(|b| project_onto(&mut b.into_rt2(), p.side.level)).collect();
    let want = project_onto(&mut lookup_view(run.layers[s][idx[s]].1.rt2(), seeded)?, p.side.level).remove(0);
    let made = kset.contains(&want);
    let mut out = vec![format!(
        "the kernels from its state at step {s0} (the tree's row, layer {layer}, state {}): {} successors at step {s}; the route's state is {}",
        sk.show(),
        kset.len(),
        if made { "ONE OF THEM (the forward made it and did not keep it: a win, level -1 or a filter)" } else { "NOT one of them: a SOUNDNESS gap in the kernels" }
    )];
    out.push(format!("  the route's state: {}", brief(&want)));
    if made {
        return Ok(out);
    }
    let dist = |k: &Proj| k.iter().filter(|(n, v)| want.get(*n) != Some(*v)).count() + want.keys().filter(|n| !k.contains_key(*n)).count();
    let mut near: Vec<&Proj> = kset.iter().collect();
    near.sort_by_key(|k| dist(k));
    for k in near.iter().take(5) {
        let diff: Vec<String> = k.iter().filter(|(n, v)| want.get(*n) != Some(*v)).map(|(n, v)| format!("{n}: kernel {v}, route {}", want.get(n).map_or("-", String::as_str))).collect();
        out.push(format!("  a kernel successor at distance {}: {}", dist(k), diff.join("; ")));
    }
    Ok(out)
}

/// A failed check: the report's line, and whether the FORWARD lost the
/// state (level -1, the ladder's filter, not in the tree) rather than the
/// arc phase (no winning set, outside W, not reached).
struct Miss {
    forward: bool,
    /// The tree does not hold it: the report explains (`explain`).
    lost: bool,
    what: String,
}

fn forward(what: String) -> Option<Miss> {
    Some(Miss { forward: true, lost: false, what })
}

fn lost(what: String) -> Option<Miss> {
    Some(Miss { forward: true, lost: true, what })
}

fn backward(what: String) -> Option<Miss> {
    Some(Miss { forward: false, lost: false, what })
}

/// A route state as the tree holds it: its state, and its exact remainder
/// (`None`: no player).
type Held<N> = (Sk, Option<(N, N)>);

/// The player's exact remainder in `b` (`None`: no player).
fn rem_raw<N: Rem>(b: &Block<N>) -> anyhow::Result<Option<(N, N)>> {
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::ids();
    let Some((cx, cy)) = b.rt2().player_xy_cells(ids, ids.f_rem) else { return Ok(None) };
    let num = |c: usize| match b.rt2().cols[c].at(0) {
        AV::Num(n) => Ok(n),
        other => Err(anyhow::anyhow!("a concrete remainder expected, got {other:?}")),
    };
    Ok(Some((num(cx)?, num(cy)?)))
}

/// THE HULL FORWARD'S CLAIM for state `b` at step `s` (its parent `from`):
/// in the tree by `s`, its remainder inside its state's hull, and a
/// recorded edge from its parent mapping the parent's remainder to its
/// own. Where the tree holds it, or the first part that fails.
fn hull_claim<N: Rem>(b: &Block<N>, s: u32, sk: Option<Sk>, from: Option<Held<N>>, h: &Hulls<N>, node: &str) -> anyhow::Result<Result<Held<N>, Miss>> {
    let miss = |what: String| Ok(Err(Miss { forward: true, lost: false, what }));
    let Some((sk, layer)) = (match sk {
        Some(sk) => h.tree.layer_of(sk, s)?.map(|l| (sk, l)),
        None => None,
    }) else {
        return Ok(Err(Miss { forward: true, lost: true, what: format!("NOT IN THE TREE by step {s} ({node})") }));
    };
    let r = rem_raw(b)?;
    let zero = <N::A as celeste_core::arith::Arithmetic>::from_i16(0);
    let (x, y) = r.unwrap_or((zero, zero));
    // (A full hull is not stored: it holds every remainder.)
    if let Some(info) = h.visited.state(sk.region, sk.key, sk.local as u32) {
        if !crate::search::rem::holds::<N>(&info.hull, x, y) {
            return miss(format!("its remainder ({x:?}, {y:?}) is OUTSIDE its state's hull {:?} (state {}, layer {layer}; {node})", info.hull, sk.show()));
        }
    }
    if let Some((ps, pr)) = from {
        let pl = h.tree.layer_of(ps, s - 1)?.expect("a held parent is in the tree");
        let edges: Vec<u32> = h.edges.preds_at(sk, pl + 1)?.into_iter().filter(|(src, _)| *src == ps).map(|(_, x)| x).collect();
        let pfrom = pr.unwrap_or((zero, zero));
        if !edges.iter().any(|&x| h.edges.pair(x).is_some_and(|t| crate::search::rem::maps::<N>(&t, pfrom, r))) {
            return miss(format!(
                "no recorded edge from its parent (state {}, layer {pl}) takes the parent's remainder {pfrom:?} to {r:?}: {:?} ({node})",
                ps.show(),
                edges.iter().map(|&x| h.edges.pair(x)).collect::<Vec<_>>()
            ));
        }
    }
    Ok(Ok((sk, r)))
}

/// The first check state `b` at step `s` (its parent held at `from`) fails,
/// and where the tree holds it (with `hulls`).
#[allow(clippy::too_many_arguments)]
fn verdict<N: Rem>(b: &Block<N>, s: u32, from: Option<Held<N>>, seeded: bool, l1: Option<(u32, &crate::search::level_minus_one::CostToGo)>, route: &Route<N>, p: &Pruning<N>) -> anyhow::Result<(Option<Miss>, Option<Held<N>>)> {
    let (shape, keys, cells) = lookup_keys(b, p.side.level, seeded, p.side.keys)?;
    let (key, cell) = (keys[0], cells[0]);
    let node = match key {
        Some(k) => format!("node shape {shape:#x} key {:#x}:{:#x} cell {cell} {:?}", k.0, k.1, super::pos_graph::cell_xy(cell)),
        None => format!("shape {shape:#x}, a code the tree never stored, cell {cell} {:?}", super::pos_graph::cell_xy(cell)),
    };
    // The forward's filters run at every flush, so on every step but the
    // start (the initial block is never flushed).
    if s > 0 {
        if let Some((h, table)) = l1 {
            if table.too_late(shape, cell, s, h) {
                return Ok((forward(format!("LEVEL -1 drops it: d {:?} from step {s} exceeds its horizon {h} ({node})", table.d_of(shape, cell))), None));
            }
        }
        if let Some(f) = route.filter {
            let mut row = lookup_view(b.rt2(), seeded)?;
            crate::frame::widen_rt2_to(&mut row, p.side.level);
            if !f.allowed(&row, s)?[0] {
                return Ok((
                    forward(format!(
                    "THE LADDER'S FILTER drops it: its projection onto {} has deadline {:?}, step {s} needs one >= {s} ({node})",
                    f.coarser(),
                    f.deadlines(&row)?[0]
                )),
                    None,
                ));
            }
        }
    }
    // The hull forward's claim, at every step (whole frames only).
    let mut held = None;
    if let Some(h) = p.hulls {
        let sk = if s == 0 { Some(h.start) } else { key.and_then(|k| h.tree.sk(shape, k, cell)) };
        match hull_claim(b, s, sk, from, h, &node)? {
            Ok(at) => held = Some(at),
            Err(m) => return Ok((Some(m), None)),
        }
    }
    if !p.side.s.clock().boundary(s) {
        return Ok((None, held));
    }
    let Some(i) = (if s == 0 { p.start } else { p.side.node.get(shape, key, cell) }) else {
        let tree = crate::storage::layer::Tree::<N>::open(p.dir, s)?;
        let layer = match key.and_then(|key| tree.sk(shape, key, cell)) {
            Some(sk) => tree.layer_of(sk, s)?.map(|l| (l, sk)),
            None => None,
        };
        let miss = match layer {
            Some((layer, sk)) => backward(format!(
                "NOT IN W's SUPPORT: in the tree (layer {layer}, state {}) but its winning set is empty at every step by {} - a successor on the route was lost ({node})",
                sk.show(),
                p.horizon
            )),
            None => lost(format!("NOT IN THE TREE by step {s}: the forward lost it since the step before (the kernels, the visited set, or a filter on a mid-frame step) ({node})")),
        };
        return Ok((miss, held));
    };
    let (x, y) = rem_of::<N>(b.rt2())?;
    match p.side.w.at(s, i) {
        None => return Ok((backward(format!("W_{s} of its node is EMPTY ({node})")), held)),
        Some(r) if !r.contains(x, y) => return Ok((backward(format!("its remainder ({x:?}, {y:?}) is OUTSIDE W_{s} of its node, {r:?} ({node})")), held)),
        Some(_) => {}
    }
    // The start is reached whenever its W holds it (`reach::inside_w`).
    if let Some(m) = p.reached.filter(|_| s > 0) {
        let d = key.and_then(|key| m.deadline(shape, key, cell));
        if !d.is_some_and(|d| u32::from(d) >= s) {
            return Ok((backward(format!("NOT REACHED inside W (`reach::inside_w`): deadline {d:?}, step {s} needs one >= {s} - the next level's filter drops it ({node})")), held));
        }
    }
    Ok((None, held))
}
