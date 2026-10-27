//! THE HULL FORWARD REPLAYED over a tree's recorded graph (`rewrite check
//! replay`): the start's hull pushed through the recorded transfers,
//! re-pushed where it grows - the forward without the engine (a node is
//! expanded with its remainder widened, so its hull only decides which
//! pushes land), a flow of `reach::forward` - under EVERY PRUNE the forward
//! applied:
//! - level -1 (`prune::TreeL1`, the tree's table) at every push but the
//!   start, by the target's step, as the forward's re-push drops (at an
//!   emission the forward never recorded the edge, so the replay never sees
//!   it);
//! - the cuts and the wins expand nothing, so their states have no recorded
//!   out-edges: the replay pushes nothing from them, as the forward;
//! - the objects ladder's filter is REFUSED: its marks are the coarser
//!   level's arc phase, which a tree alone does not hold.
//!
//! Per frame the replay's FRONTIER (the states whose hull grew: new and
//! grown, each with its layer and hull) must equal the tree's frame file's,
//! by count and by content (`hulls::fingerprint`, ckhash's `h` line), and
//! its new states must be the tree's (`verdict`).

use crate::search::hull::{AccOf, HullOf};
use anyhow::{ensure, Result};
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use crate::prune::TreeL1;
use crate::search::reach::Flow;
use crate::search::rem::{Rem, Xfer};
use crate::storage::edges::EdgeStore;
use crate::storage::hulls::Grown;
use crate::storage::layer::Tree;
use crate::storage::Sk;

/// One frame of a replay: the states first reached there and the tree's
/// (`held`), the edges recorded into it, the pushes into it level -1
/// dropped, and the frontier's `(count, content hash)`, the replay's and the
/// tree's (each pair must be equal).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Frame {
    pub states: u64,
    pub held: u64,
    pub edges: u64,
    pub dropped: u64,
    pub frontier: (u64, u64),
    pub tree_frontier: (u64, u64),
}

/// The replay's flow: a node a state, with its HULL so far and the frame it
/// was first reached (its layer: its out-edges are read at the next frame);
/// its set at a frame its hull where it grew.
struct Replay<'a, N: Rem> {
    store: &'a EdgeStore<N>,
    tree: &'a Tree<N>,
    xfers: Vec<Xfer<N>>,
    l1: Option<TreeL1>,
    threads: usize,
    hulls: FxHashMap<Sk, (HullOf<N>, u32)>,
    out: FxHashMap<Sk, Box<[(Sk, u32)]>>,
    /// Per frame the edges read (recorded) there, the pushes level -1
    /// dropped into it, the frontier's fingerprint.
    edges: Vec<u64>,
    dropped: Vec<AtomicU64>,
    frontier: Vec<(u64, u64)>,
    /// The first fingerprint error (a state of no shape).
    error: Option<anyhow::Error>,
}

impl<N: Rem> Flow for Replay<'_, N> {
    type Node = Sk;
    type Set = HullOf<N>;
    type Part = AccOf<N>;
    type Scratch = ();

    /// A state first reached at `t` (its layer): its edges recorded at `t + 1`.
    fn load(&mut self, t: u32, nodes: &[Sk]) -> Result<()> {
        let new: Vec<Sk> = nodes.iter().copied().filter(|s| !self.out.contains_key(s)).collect();
        let mut lists: Vec<Vec<(Sk, u32)>> = vec![Vec::new(); new.len()];
        for (k, dst, x) in self.store.out_of(t + 1, &new, self.threads)? {
            lists[k].push((dst, x));
        }
        for (s, l) in new.into_iter().zip(lists) {
            self.edges[t as usize + 1] += l.len() as u64;
            self.out.insert(s, l.into_boxed_slice());
        }
        Ok(())
    }

    fn out(&self, s: Sk) -> &[(Sk, u32)] {
        &self.out[&s]
    }

    /// Level -1 first, by the target's step (the forward's re-push drop).
    fn push(&self, t: u32, _s: Sk, hull: &HullOf<N>, m: Sk, x: u32, out: &mut Vec<AccOf<N>>) {
        if self.l1.is_some_and(|l1| l1.drops(self.tree.shapes[m.shape() as usize], m.cell(), t + 1)) {
            self.dropped[t as usize + 1].fetch_add(1, Relaxed);
            return;
        }
        let mut acc = AccOf::<N>::default();
        if hull.push(&self.xfers[x as usize], &mut acc) {
            out.push(acc);
        }
    }

    /// The pushes merged with what the target held: its hull, if it grew.
    fn close(&self, _t: u32, m: Sk, parts: &[(Sk, AccOf<N>)], _: &mut ()) -> Result<Option<HullOf<N>>> {
        let mut acc = AccOf::<N>::default();
        for (_, a) in parts {
            acc.merge(a);
        }
        let Some(pushed) = acc.hull() else { return Ok(None) };
        Ok(match self.hulls.get(&m) {
            Some((old, _)) => {
                acc.add_hull(old);
                let merged = acc.hull().expect("not empty");
                (merged != *old).then_some(merged)
            }
            None => Some(pushed),
        })
    }

    /// Frame `t`'s frontier, fingerprinted as the frame file's is.
    fn made(&mut self, t: u32, sets: &[(Sk, HullOf<N>)]) {
        let hulls = &self.hulls;
        let states = sets.iter().map(|&(sk, hull)| Grown { sk, layer: hulls.get(&sk).map_or(t, |x| x.1) as u16, hull });
        match crate::storage::hulls::fingerprint(states, &self.tree.space, &self.tree.shapes) {
            Ok(fp) => self.frontier[t as usize] = fp,
            Err(e) => {
                self.error.get_or_insert(e);
            }
        }
        for &(s, h) in sets {
            self.hulls.entry(s).or_insert((h, t)).0 = h;
        }
    }
}

/// Replay the tree at `dir` through frame `to` from the start's hull on its
/// layer-0 states; per frame its counts and the tree's (index 0 the start).
/// Refuses a tree the objects ladder filtered.
pub fn replay<N: Rem>(dir: &std::path::Path, to: u32, threads: usize) -> Result<Vec<Frame>> {
    let key = crate::prune::MarkFilter::<N>::KEY;
    if let Some(f) = crate::storage::record::Record::read(dir)?.as_ref().and_then(|r| r.get(key)) {
        anyhow::bail!("{}: the objects ladder filtered this tree (`{key} {f}`): its filter's marks are the coarser level's arc phase, which the tree does not hold, so the replay cannot apply it", dir.display());
    }
    let start = crate::search::rem::start_hull::<N>();
    let tree = Tree::<N>::open(dir, to)?;
    let store = EdgeStore::<N>::open(dir, to)?;
    for f in 1..=to {
        ensure!(store.has_frame(f), "{}: no recorded edges at frame {f}", dir.display());
    }
    let xfers: Vec<Xfer<N>> = store.pairs().iter().map(N::xfer).collect();
    let n = to as usize + 2;
    let mut flow = Replay {
        store: &store,
        tree: &tree,
        xfers,
        l1: TreeL1::read(dir)?,
        threads,
        hulls: FxHashMap::default(),
        out: FxHashMap::default(),
        edges: vec![0; n],
        dropped: (0..n).map(|_| AtomicU64::new(0)).collect(),
        frontier: vec![(0, 0); n],
        error: None,
    };
    let starts: Vec<(Sk, HullOf<N>)> = tree.layer(0, None, threads)?.0.states_iter().map(|s| (s, start)).collect();
    flow.made(0, &starts);
    crate::search::reach::forward(&mut flow, starts, to, threads, |_, _, _| Ok(()))?;
    if let Some(e) = flow.error.take() {
        return Err(e);
    }
    let mut out: Vec<Frame> = (0..=to as usize).map(|f| Frame { edges: flow.edges[f], dropped: flow.dropped[f].load(Relaxed), frontier: flow.frontier[f], ..Frame::default() }).collect();
    for &(_, f) in flow.hulls.values() {
        out[f as usize].states += 1;
    }
    for (f, o) in out.iter_mut().enumerate() {
        let (layer, _) = tree.layer(f as u32, None, threads)?;
        o.held = layer.states();
        if let Some(file) = tree.file(f as u32) {
            o.tree_frontier = crate::storage::hulls::fingerprint(file.frontier_states(&layer, threads)?.into_iter(), &tree.space, &tree.shapes)?;
        }
    }
    Ok(out)
}

/// THE VERDICT of a replay of the tree at `dir` (`replay`'s frames): at
/// every frame the replay reached exactly the tree's states first, and its
/// frontier (states, layers, hulls) is the frame file's.
pub fn verdict(dir: &std::path::Path, frames: &[Frame]) -> Result<()> {
    let bad: Vec<String> = frames
        .iter()
        .enumerate()
        .filter(|(_, x)| x.states != x.held || x.frontier != x.tree_frontier)
        .map(|(f, x)| format!("frame {f}: the replay reaches {} states first, the tree holds {}; its frontier {} states {:016x}, the tree's {} {:016x}", x.states, x.held, x.frontier.0, x.frontier.1, x.tree_frontier.0, x.tree_frontier.1))
        .collect();
    super::differential::verdict(frames.len(), bad.len(), || format!("check replay: {}: {}", dir.display(), bad.join("; ")))
}
