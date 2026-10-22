//! THE HULL FORWARD REPLAYED over a tree's recorded graph (`rewrite check
//! replay`): the start's hull pushed through the recorded transfers,
//! re-pushed where it grows - the forward without the engine (a node is
//! expanded with its remainder widened, so its hull only decides which
//! pushes land), a flow of `reach::forward`. It must reach exactly the
//! tree's states at its frames.

use crate::search::hull::{AccOf, HullOf};
use anyhow::{ensure, Result};
use rustc_hash::FxHashMap;

use super::reach::Flow;
use super::rem::{Rem, Xfer};
use crate::storage::edges::EdgeStore;
use crate::storage::layer::Tree;
use crate::storage::Sk;

/// One frame of a replay: the states first reached there (equal to the
/// tree's) and the edges recorded into it.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Frame {
    pub states: u64,
    pub edges: u64,
}

/// The replay's flow: a node a state, with its HULL so far and the frame it
/// was first reached (its layer: its out-edges are read at the next frame);
/// its set at a frame its hull where it grew.
struct Replay<'a, N: Rem> {
    store: &'a EdgeStore<N>,
    xfers: Vec<Xfer<N>>,
    threads: usize,
    hulls: FxHashMap<Sk, (HullOf<N>, u32)>,
    out: FxHashMap<Sk, Box<[(Sk, u32)]>>,
    /// Per frame the edges read (recorded) there.
    edges: Vec<u64>,
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

    fn push(&self, _t: u32, _s: Sk, hull: &HullOf<N>, _m: Sk, x: u32, out: &mut Vec<AccOf<N>>) {
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

    fn made(&mut self, t: u32, sets: &[(Sk, HullOf<N>)]) {
        for &(s, h) in sets {
            self.hulls.entry(s).or_insert((h, t)).0 = h;
        }
    }
}

/// Replay the tree at `dir` through frame `to` from the start's hull on its
/// layer-0 states; per frame its counts (index 0 the start).
pub fn replay<N: Rem>(dir: &std::path::Path, to: u32, threads: usize) -> Result<Vec<Frame>> {
    let start = crate::search::rem::start_hull::<N>();
    let tree = Tree::<N>::open(dir, to)?;
    let store = EdgeStore::<N>::open(&dir, to)?;
    for f in 1..=to {
        ensure!(store.has_frame(f), "{}: no recorded edges at frame {f}", dir.display());
    }
    let xfers: Vec<Xfer<N>> = store.pairs().iter().map(N::xfer).collect();
    let mut flow = Replay { store: &store, xfers, threads, hulls: FxHashMap::default(), out: FxHashMap::default(), edges: vec![0; to as usize + 2] };
    let starts: Vec<(Sk, HullOf<N>)> = tree.layer(0, None, threads)?.0.states_iter().map(|s| (s, start)).collect();
    flow.made(0, &starts);
    super::reach::forward(&mut flow, starts, to, threads, |_, _, _| Ok(()))?;
    let mut out: Vec<Frame> = (0..=to as usize).map(|f| Frame { states: 0, edges: flow.edges[f] }).collect();
    for &(_, f) in flow.hulls.values() {
        out[f as usize].states += 1;
    }
    for (f, o) in out.iter().enumerate() {
        let held = tree.layer(f as u32, None, threads)?.0.states();
        ensure!(o.states == held, "{}: frame {f}: the replay reaches {} states first, the tree holds {held}", dir.display(), o.states);
    }
    Ok(out)
}
