//! THE HULL FORWARD REPLAYED over a tree's recorded graph (`rewrite check
//! replay`): the start's hull pushed through the recorded transfers,
//! re-pushed where it grows - the forward without the engine (a node is
//! expanded with its remainder widened, so its hull only decides which
//! pushes land). It must reach exactly the tree's states at its frames.

use crate::search::hull::{AccOf, HullOf};
use anyhow::{ensure, Result};
use rustc_hash::FxHashMap;

use super::rem::Rem;
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

/// Replay the tree at `dir` through frame `to` from the start's hull on its
/// layer-0 states; per frame its counts (index 0 the start).
pub fn replay<N: Rem>(dir: &std::path::Path, to: u32, threads: usize) -> Result<Vec<Frame>> {
    let start = crate::search::rem::start_hull::<N>();
    // The recorded graph: out-edges by source (target, transfer).
    let tree = Tree::<N>::open(dir, to)?;
    let store = EdgeStore::<N>::open(&dir, to)?;
    let xfers: Vec<crate::search::rem::Xfer<N>> = store.pairs().iter().map(N::xfer).collect();
    let mut succ: FxHashMap<Sk, Vec<(Sk, u32)>> = FxHashMap::default();
    let mut out = vec![Frame::default()];
    for f in 1..=to {
        ensure!(store.has_frame(f), "{}: no recorded edges at frame {f}", dir.display());
        let es = store.edges_at(f, threads)?;
        out.push(Frame { states: 0, edges: es.len() as u64 });
        for e in es {
            succ.entry(e.src).or_default().push((e.dst, e.xfer));
        }
    }
    // THE PROPAGATION: every state that grew pushes along its out-edges, the
    // targets merge; per state its hull and the frame it was first reached.
    let starts: Vec<Sk> = tree.layer(0, None, threads)?.0.states_iter().collect();
    let mut hull: FxHashMap<Sk, (HullOf<N>, u32)> = starts.iter().map(|&s| (s, (start, 0))).collect();
    let mut grown = starts;
    for f in 1..=to {
        let mut acc: FxHashMap<Sk, AccOf<N>> = FxHashMap::default();
        for s in &grown {
            let h = hull[s].0;
            for &(d, x) in succ.get(s).map_or(&[][..], |v| &v[..]) {
                h.push(&xfers[x as usize], acc.entry(d).or_default());
            }
        }
        grown.clear();
        for (d, a) in acc {
            let Some(pushed) = a.hull() else { continue };
            let merged = match hull.get(&d) {
                None => Some(pushed),
                Some((old, _)) => {
                    let mut m = a;
                    m.add_hull(old);
                    let merged = m.hull().expect("not empty");
                    (merged != *old).then_some(merged)
                }
            };
            if let Some(h) = merged {
                hull.entry(d).or_insert((h, f)).0 = h;
                grown.push(d);
            }
        }
        grown.sort_unstable();
    }
    for &(_, f) in hull.values() {
        out[f as usize].states += 1;
    }
    for (f, o) in out.iter().enumerate() {
        let held = tree.layer(f as u32, None, threads)?.0.states();
        ensure!(o.states == held, "{}: frame {f}: the replay reaches {} states first, the tree holds {held}", dir.display(), o.states);
    }
    Ok(out)
}
