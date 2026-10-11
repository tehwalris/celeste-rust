//! EXPERIMENT (plans/arc-memory.md, "The region worklist"): the backward as
//! a monotone FIXPOINT over nodes, each node's value its whole time-indexed
//! winning set (spans), scheduled by target REGION: open a region, apply
//! every pending update of its nodes over all their frames at once, push
//! the changed sources to their regions' queues, take the next region (the
//! one whose pending frames reach latest). The same fixpoint as the
//! time-stepped sweep (`arc_dp::Backward`), so the same `W`; measured
//! against it by `rewrite bench-backward --worklist`.

use super::arcs::{Piece, Region, Transfer};
use crate::storage::edges::{EdgeStore, ReadStats};
use crate::storage::{id_region, StateId};
use rustc_hash::FxHashMap;

/// A node's value: its layer and its spans `(lo, hi, set)`, contiguous from
/// the layer, the sets nested (growing as `t` falls).
#[derive(Clone, Default)]
struct Val {
    layer: u16,
    spans: Vec<(u16, u16, u32)>,
}

impl Val {
    fn at(&self, t: u16) -> Option<u32> {
        self.spans.iter().find(|s| s.0 <= t && t <= s.1).map(|s| s.2)
    }
}

/// What the worklist did.
#[derive(Default)]
pub struct Stats {
    /// Per target region, the times it was opened (taken off the queue).
    pub opens: FxHashMap<u32, u32>,
    /// Passes over a region's pending nodes (an open loops while its own
    /// sources change).
    pub passes: u64,
    pub read: ReadStats,
    pub updates: u64,
    pub changes: u64,
}

/// The winning sets by the region worklist: per node (by id) its spans
/// `(lo, hi, set)`, the sets, and what it did.
pub fn backward(store: &EdgeStore, pairs: &[(Transfer, Transfer)], wins: &[(StateId, u32)], horizon: u32) -> (Vec<(StateId, Vec<(u16, u16, u32)>)>, Vec<Region>, Stats) {
    use std::collections::BinaryHeap;
    let h = horizon as u16;
    let mut sets: Vec<Region> = vec![Region::full()];
    let mut interned: FxHashMap<u64, u32> = FxHashMap::default();
    let hash = |r: &Region| {
        use std::hash::{Hash, Hasher};
        let mut hs = rustc_hash::FxHasher::default();
        r.hash(&mut hs);
        hs.finish()
    };
    interned.insert(hash(&sets[0]), 0);
    let mut vals: FxHashMap<StateId, Val> = FxHashMap::default();
    // Per region its pending nodes, each with the frames its value grew at.
    let mut dirty: FxHashMap<u32, FxHashMap<StateId, (u16, u16)>> = FxHashMap::default();
    let mut queue: BinaryHeap<(u16, std::cmp::Reverse<u32>)> = BinaryHeap::new();
    for &(id, layer) in wins.iter().filter(|w| w.1 <= horizon) {
        vals.insert(id, Val { layer: layer as u16, spans: vec![(layer as u16, h, 0)] });
        dirty.entry(id_region(id)).or_default().insert(id, (layer as u16, h));
        queue.push((h, std::cmp::Reverse(id_region(id))));
    }
    let mut st = Stats::default();
    let mut pieces: Vec<Piece> = Vec::new();
    let mut scratch = Vec::new();
    while let Some((top, std::cmp::Reverse(region))) = queue.pop() {
        // A stale entry: the region's pending frames no longer reach `top`.
        let Some(maxb) = dirty.get(&region).and_then(|d| d.values().map(|w| w.1).max()) else { continue };
        if maxb != top {
            continue;
        }
        *st.opens.entry(region).or_default() += 1;
        while let Some(d) = dirty.remove(&region).filter(|d| !d.is_empty()) {
            st.passes += 1;
            let mut d: Vec<(StateId, (u16, u16))> = d.into_iter().collect();
            d.sort_unstable_by_key(|x| x.0);
            let maxb = d.iter().map(|x| x.1 .1).max().expect("pending nodes");
            // Each in-edge (source p, layer f - 1) of a pending m: per span of
            // m within its grown frames s, an update of p at s - 1.
            let mut ups: Vec<(StateId, u16, u16, u16, u32, u32)> = Vec::new();
            let mut targets = Vec::new();
            let mut which = Vec::new();
            for f in 1..=maxb as u32 {
                targets.clear();
                which.clear();
                for (k, &(m, (_, b))) in d.iter().enumerate() {
                    if vals[&m].layer as u32 <= f && f <= b as u32 {
                        targets.push(m);
                        which.push(k);
                    }
                }
                store.preds_sorted(f, &targets, &mut st.read, |k, e| {
                    let (m, (a, b)) = d[which[k]];
                    for &(lo, hi, s) in &vals[&m].spans {
                        let (x, y) = (lo.max(a).max(f as u16), hi.min(b));
                        if x <= y {
                            ups.push((e.src, (f - 1) as u16, x - 1, y - 1, s, e.xfer));
                        }
                    }
                });
            }
            st.updates += ups.len() as u64;
            ups.sort_unstable();
            ups.dedup();
            let mut i = 0;
            while i < ups.len() {
                let p = ups[i].0;
                let j = i + ups[i..].partition_point(|u| u.0 == p);
                let layer = ups[i].1;
                let group = &ups[i..j];
                i = j;
                // The frames touched, cut where an update or a span starts or ends.
                let mut cuts: Vec<u16> = group.iter().flat_map(|u| [u.2, u.3 + 1]).collect();
                let old = vals.get(&p).cloned().unwrap_or(Val { layer, spans: Vec::new() });
                assert_eq!(old.layer, layer, "a node's layer from two frames");
                cuts.extend(old.spans.iter().flat_map(|s| [s.0, s.1 + 1]));
                cuts.sort_unstable();
                cuts.dedup();
                let (lo_all, hi_all) = (group.iter().map(|u| u.2).min().expect("one"), group.iter().map(|u| u.3).max().expect("one"));
                let mut per_t: Vec<(u16, u16, u32)> = Vec::new();
                let (mut cmin, mut cmax) = (u16::MAX, 0u16);
                for w in cuts.windows(2) {
                    let (x, y) = (w[0], w[1] - 1);
                    let before = old.at(x);
                    let set = if x < lo_all || x > hi_all {
                        before
                    } else {
                        pieces.clear();
                        if let Some(b) = before {
                            sets[b as usize].pieces(&mut pieces);
                        }
                        for u in group.iter().filter(|u| u.2 <= x && x <= u.3) {
                            let (tx, ty) = pairs[u.5 as usize];
                            sets[u.4 as usize].pull(tx, ty, &mut pieces);
                        }
                        if pieces.is_empty() {
                            None
                        } else {
                            let r = Region::from_pieces(&mut pieces, &mut scratch);
                            if before.is_some_and(|b| sets[b as usize] == r) {
                                before
                            } else {
                                (cmin, cmax) = (cmin.min(x), cmax.max(y));
                                let hr = hash(&r);
                                Some(match interned.get(&hr) {
                                    Some(&s) if sets[s as usize] == r => s,
                                    found => {
                                        let s = sets.len() as u32;
                                        if found.is_none() {
                                            interned.insert(hr, s);
                                        }
                                        sets.push(r);
                                        s
                                    }
                                })
                            }
                        }
                    };
                    if let Some(s) = set {
                        match per_t.last_mut() {
                            Some(l) if l.2 == s && l.1 + 1 == x => l.1 = y,
                            _ => per_t.push((x, y, s)),
                        }
                    }
                }
                if cmin > cmax {
                    continue;
                }
                st.changes += 1;
                vals.insert(p, Val { layer, spans: per_t });
                let r = id_region(p);
                let e = dirty.entry(r).or_default().entry(p).or_insert((cmin, cmax));
                *e = (e.0.min(cmin), e.1.max(cmax));
                if r != region {
                    let top = dirty[&r].values().map(|w| w.1).max().expect("pending");
                    queue.push((top, std::cmp::Reverse(r)));
                }
            }
        }
    }
    let mut out: Vec<(StateId, Vec<(u16, u16, u32)>)> = vals.into_iter().map(|(id, v)| (id, v.spans)).collect();
    out.sort_unstable_by_key(|x| x.0);
    (out, sets, st)
}
