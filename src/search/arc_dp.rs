//! The rotation graph's dynamic programme (plans/architecture.md "Arcs",
//! plans/arc-memory.md): over the remainder-free transition graph the
//! forward recorded (`storage::edges`), whose edges carry per axis a guard
//! and an action (`search::arcs`), the WINNING SETS `W_t(n)`: the remainders
//! from which node `n`, occupied at frame `t`, wins by the horizon -
//! `W_t(n) = U_e guard_e ∩ action_e^-1(W_{t+1}(dst_e))` for `t >= layer(n)`,
//! a win node's set the whole torus from its layer on.
//!
//! ONE BACKWARD PUSH over the recorded IN-edges (`backward`). `W_t(n)` only
//! grows as `t` falls - more time never hurts, and the layers never bind:
//! an edge recorded at frame `f` leaves a state of layer `f - 1` and enters
//! one of layer <= `f`, so `W_t(p)` reads `W_{t+1}(m)` only where `m` has
//! one. Hence `W_t(p) = W_{t+1}(p) ∪ U pull_e(W_{t+1}(m))` over the edges
//! `p -> m` whose target CHANGED at `t + 1`: frame by frame, the changed
//! nodes' in-edges are read from the edge files by target (`EdgeStore::
//! preds_sorted`) and their sets pulled into the sources. No graph is held
//! and nothing marks nodes first: a node no winning set reaches is never
//! touched. Exact because "intersect with the guard, then act" distributes
//! over unions.
//!
//! Then the optimum is read off the start's sets (`optimum`), the start's
//! remainder pushed forward inside `W` for the next level's filter
//! (`reach`, over the out-edges between `W`'s nodes, `OutGraph`), and the
//! concrete search runs inside `W` (`concrete_search`).

use super::arcs::{Region, Transfer};
use crate::storage::edges::{EdgeStore, ReadStats};
use crate::storage::{id_entry, id_local, id_region, state_id, StateId};
use rustc_hash::{FxHashMap, FxHashSet};

/// A node's winning set over the frames `lo..=hi`.
#[derive(Clone, Copy, Debug)]
struct Span {
    lo: u16,
    hi: u16,
    set: u32,
}

/// The winning sets, `t` in `0..=horizon`, of the nodes that have one (W's
/// SUPPORT, sorted by id): per node its SPANS - a set and the frames it
/// holds over, 8 B - into an arena of the distinct sets. A node's set is
/// non-empty from its layer to its last frame and grows as `t` falls, so a
/// node is one span per change.
pub struct Winning {
    ids: Vec<StateId>,
    /// Node `i`'s spans: `spans[at[i]..at[i + 1]]`, by frame.
    at: Vec<u32>,
    spans: Vec<Span>,
    sets: Vec<Region>,
}

impl Winning {
    /// From the runs of closed spans the backward made (each sorted by id,
    /// a node at most once a run, the runs by falling frame): merged by
    /// (id, frame) into the nodes' lists.
    fn from_runs(runs: Vec<Vec<(StateId, Span)>>, sets: Vec<Region>) -> Self {
        use std::cmp::Reverse;
        let total: usize = runs.iter().map(Vec::len).sum();
        assert!(total < u32::MAX as usize, "{total} spans do not fit a u32 index");
        let mut heap: std::collections::BinaryHeap<Reverse<(StateId, u16, usize)>> = std::collections::BinaryHeap::new();
        let mut pos = vec![0usize; runs.len()];
        for (k, r) in runs.iter().enumerate() {
            if let Some(&(id, s)) = r.first() {
                heap.push(Reverse((id, s.lo, k)));
            }
        }
        let (mut ids, mut at, mut spans): (Vec<StateId>, Vec<u32>, Vec<Span>) = (Vec::new(), Vec::new(), Vec::with_capacity(total));
        while let Some(Reverse((id, _, k))) = heap.pop() {
            let s = runs[k][pos[k]].1;
            pos[k] += 1;
            if let Some(&(nid, ns)) = runs[k].get(pos[k]) {
                heap.push(Reverse((nid, ns.lo, k)));
            }
            if ids.last() != Some(&id) {
                ids.push(id);
                at.push(spans.len() as u32);
            } else {
                debug_assert_eq!(spans.last().map(|p: &Span| p.hi as u32 + 1), Some(s.lo as u32), "a node's spans are contiguous");
            }
            spans.push(s);
        }
        at.push(spans.len() as u32);
        ids.shrink_to_fit();
        at.shrink_to_fit();
        Winning { ids, at, spans, sets }
    }
    pub fn len(&self) -> usize {
        self.ids.len()
    }
    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }
    /// The node of state `id` (`None`: no set at any frame).
    pub fn index(&self, id: StateId) -> Option<u32> {
        self.ids.binary_search(&id).ok().map(|i| i as u32)
    }
    pub fn id(&self, i: u32) -> StateId {
        self.ids[i as usize]
    }
    fn of(&self, i: u32) -> &[Span] {
        &self.spans[self.at[i as usize] as usize..self.at[i as usize + 1] as usize]
    }
    /// The first frame node `i` has a set: its layer.
    pub fn layer(&self, i: u32) -> u32 {
        self.of(i)[0].lo as u32
    }
    /// The last frame node `i` has a set (its deadline).
    pub fn last(&self, i: u32) -> u32 {
        self.of(i).last().expect("a node has a span").hi as u32
    }
    /// `W_t(i)` (`None` = empty).
    pub fn at(&self, t: u32, i: u32) -> Option<&Region> {
        let s = self.of(i);
        let k = s.partition_point(|s| (s.hi as u32) < t);
        s.get(k).filter(|s| s.lo as u32 <= t).map(|s| &self.sets[s.set as usize])
    }
    /// Node `i`'s spans `(lo, hi, set)`: `W_t(i) = set` for `t` in
    /// `lo..=hi`, empty at the frames no span covers.
    pub fn spans_of(&self, i: u32) -> impl Iterator<Item = (u32, u32, &Region)> + '_ {
        self.of(i).iter().map(move |s| (s.lo as u32, s.hi as u32, &self.sets[s.set as usize]))
    }
    /// The bytes held: ids, span offsets, spans, and the sets' arena.
    pub fn bytes(&self) -> u64 {
        let sets: usize = self.sets.iter().map(|r| r.bytes()).sum();
        (8 * self.ids.len() + 4 * self.at.len() + 8 * self.spans.len() + sets) as u64
    }
    /// Spans and distinct sets.
    pub fn sizes(&self) -> (usize, usize) {
        (self.spans.len(), self.sets.len())
    }
}

/// A node with a set at the frame done last: the set, the last frame of its
/// current span, its layer.
#[derive(Clone, Copy, Debug)]
struct Cur {
    id: StateId,
    set: u32,
    hi: u16,
    layer: u16,
}

/// An in-edge of a changed node, read at a step: its source and the
/// source's layer, the target's set, the edge's transfer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Emit {
    p: StateId,
    set: u32,
    xfer: u32,
    layer: u16,
}

/// A set code during a step's merge: an arena index, or (with this bit) the
/// index of a set its range made, interned after.
const NEW_SET: u32 = 1 << 31;

fn region_hash(r: &Region) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hs = rustc_hash::FxHasher::default();
    r.hash(&mut hs);
    hs.finish()
}

/// Run `f(lo, hi)` over `0..n` in chunks of `chunk` on `threads` workers;
/// the results in chunk order.
fn par_chunks<T: Send>(n: usize, chunk: usize, threads: usize, f: impl Fn(usize, usize) -> T + Sync) -> Vec<T> {
    let next = std::sync::atomic::AtomicUsize::new(0);
    let workers = threads.max(1).min(n.div_ceil(chunk.max(1))).max(1);
    let mut parts: Vec<(usize, T)> = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..workers)
            .map(|_| {
                sc.spawn(|| {
                    let mut out = Vec::new();
                    loop {
                        let lo = next.fetch_add(chunk, std::sync::atomic::Ordering::Relaxed);
                        if lo >= n {
                            return out;
                        }
                        out.push((lo, f(lo, (lo + chunk).min(n))));
                    }
                })
            })
            .collect();
        hs.into_iter().flat_map(|h| h.join().expect("a worker panicked")).collect()
    });
    parts.sort_unstable_by_key(|p| p.0);
    parts.into_iter().map(|p| p.1).collect()
}

/// BACKWARD: `W_t` for `t = horizon` down to 0 (`Backward::step`, a frame at
/// a time), from the win states `wins` (id, layer).
pub fn backward(store: &EdgeStore, pairs: &[(Transfer, Transfer)], wins: &[(StateId, u32)], horizon: u32, threads: usize) -> Winning {
    let mut b = Backward::new(store, pairs, wins, horizon, threads);
    for t in (0..horizon).rev() {
        b.step(t);
    }
    b.finish()
}

/// The backward between two frames: `W_{t+1}` (the frame done last), which
/// of its sets changed there, and the spans closed so far. `rewrite
/// bench-backward` times one `step` from a clone.
#[derive(Clone)]
pub struct Backward<'a> {
    store: &'a EdgeStore,
    pairs: &'a [(Transfer, Transfer)],
    /// The win states, sorted (the fragmentation lines count the others).
    wins: Vec<StateId>,
    horizon: u32,
    threads: usize,
    /// The ARENA of sets, equal sets once (found by hash; a collision with a
    /// different set is stored apart). Index 0: the whole torus.
    sets: Vec<Region>,
    interned: FxHashMap<u64, u32>,
    /// The nodes with a set at the frame done last, by id.
    cur: Vec<Cur>,
    /// Their indices whose set changed there (or appeared).
    changed: Vec<u32>,
    /// Per step, the spans it closed (sorted by id).
    runs: Vec<Vec<(StateId, Span)>>,
    /// Per target region, the steps that read its in-edges.
    opens: FxHashMap<u32, u32>,
    read: ReadStats,
    n_changed: u64,
    n_emits: u64,
    max_emits: usize,
    max_cur: usize,
    d_read: std::time::Duration,
    d_pull: std::time::Duration,
    d_merge: std::time::Duration,
    fragments: Vec<String>,
}

impl<'a> Backward<'a> {
    /// `W_horizon`: the win nodes present by the horizon (each step on
    /// `threads` workers).
    pub fn new(store: &'a EdgeStore, pairs: &'a [(Transfer, Transfer)], wins: &[(StateId, u32)], horizon: u32, threads: usize) -> Self {
        assert!(horizon < u16::MAX as u32, "a horizon past u16");
        let sets: Vec<Region> = vec![Region::full()];
        let mut interned: FxHashMap<u64, u32> = FxHashMap::default();
        interned.insert(region_hash(&sets[0]), 0);
        let mut cur: Vec<Cur> = wins.iter().filter(|w| w.1 <= horizon).map(|&(id, layer)| Cur { id, set: 0, hi: horizon as u16, layer: layer as u16 }).collect();
        cur.sort_unstable_by_key(|c| c.id);
        cur.dedup_by_key(|c| c.id);
        let changed: Vec<u32> = (0..cur.len() as u32).collect();
        let wins = cur.iter().map(|c| c.id).collect();
        Backward {
            store,
            pairs,
            wins,
            horizon,
            threads: threads.max(1),
            sets,
            interned,
            max_cur: cur.len(),
            cur,
            changed,
            runs: Vec::new(),
            opens: FxHashMap::default(),
            read: ReadStats::default(),
            n_changed: 0,
            n_emits: 0,
            max_emits: 0,
            d_read: std::time::Duration::ZERO,
            d_pull: std::time::Duration::ZERO,
            d_merge: std::time::Duration::ZERO,
            fragments: Vec::new(),
        }
    }

    /// The frame done last's nodes with a set, and each one's set.
    pub fn current(&self) -> impl Iterator<Item = (StateId, &Region)> + '_ {
        self.cur.iter().map(|c| (c.id, &self.sets[c.set as usize]))
    }

    /// The read, pull and merge walls so far.
    pub fn walls(&self) -> [std::time::Duration; 3] {
        [self.d_read, self.d_pull, self.d_merge]
    }

    /// `W_t` from `W_{t+1}` (the frame done last; `t` counts down from
    /// `horizon - 1`): the in-edges of the nodes whose set changed at `t + 1`,
    /// recorded at frames up to `t + 1` (their sources have layers <= `t`),
    /// read target by target in id order on `threads` workers; then, source
    /// by source in parallel id ranges, its set at `t + 1` united with the
    /// pulls; the nodes of layer `t + 1` close their spans.
    pub fn step(&mut self, t: u32) {
        assert!(t < self.horizon, "a step below the horizon");
        let (store, pairs, threads) = (self.store, self.pairs, self.threads);
        let t0 = std::time::Instant::now();
        let mut last_region = u32::MAX;
        for &c in &self.changed {
            let r = id_region(self.cur[c as usize].id);
            if r != last_region {
                *self.opens.entry(r).or_default() += 1;
                last_region = r;
            }
        }
        // READ: per chunk of changed nodes (in id order), frame by frame,
        // the in-edges of those of them with layer <= the frame.
        let (cur, changed) = (&self.cur, &self.changed);
        let parts: Vec<(Vec<Emit>, ReadStats)> = par_chunks(changed.len(), 1024, threads, |lo, hi| {
            let chunk = &changed[lo..hi];
            let (mut emits, mut stats) = (Vec::new(), ReadStats::default());
            let (mut targets, mut sets): (Vec<StateId>, Vec<u32>) = (Vec::with_capacity(chunk.len()), Vec::with_capacity(chunk.len()));
            let min_layer = chunk.iter().map(|&c| cur[c as usize].layer as u32).min().unwrap_or(0);
            for f in min_layer.max(1)..=t + 1 {
                targets.clear();
                sets.clear();
                for &c in chunk {
                    let n = &cur[c as usize];
                    if n.layer as u32 <= f {
                        targets.push(n.id);
                        sets.push(n.set);
                    }
                }
                store.preds_sorted(f, &targets, &mut stats, |k, e| emits.push(Emit { p: e.src, set: sets[k], xfer: e.xfer, layer: (f - 1) as u16 }));
            }
            emits.sort_unstable();
            emits.dedup();
            (emits, stats)
        });
        let n_emits: usize = parts.iter().map(|p| p.0.len()).sum();
        for p in &parts {
            self.read.add(&p.1);
        }
        self.n_emits += n_emits as u64;
        self.max_emits = self.max_emits.max(n_emits);
        self.n_changed += changed.len() as u64;
        let n_read = changed.len();
        let d_read = t0.elapsed();
        self.d_read += d_read;
        // PULL, by source id ranges cut at the emissions' quantiles: each
        // source's set at t + 1 united with its pulls; the untouched carried,
        // those of layer t + 1 closed.
        let t0 = std::time::Instant::now();
        let mut sample: Vec<StateId> = parts.iter().flat_map(|p| p.0.iter().step_by(64).map(|e| e.p)).collect();
        sample.sort_unstable();
        let n_ranges = if n_emits < 1 << 16 { 1 } else { threads * 4 };
        let mut bounds: Vec<StateId> = vec![0];
        bounds.extend((1..n_ranges).map(|k| sample[k * sample.len() / n_ranges]));
        bounds.push(StateId::MAX);
        bounds.dedup();
        struct RangeOut {
            cur: Vec<Cur>,
            changed: Vec<u32>,
            closed: Vec<(StateId, Span)>,
            news: Vec<Region>,
        }
        let (sets, interned) = (&self.sets, &self.interned);
        let close_at = t as u16 + 1;
        let outs: Vec<RangeOut> = par_chunks(bounds.len() - 1, 1, threads, |k, _| {
            let (lo, hi) = (bounds[k], bounds[k + 1]);
            let cur = &cur[cur.partition_point(|c| c.id < lo)..cur.partition_point(|c| c.id < hi)];
            let mut es: Vec<Emit> = Vec::new();
            for p in &parts {
                es.extend_from_slice(&p.0[p.0.partition_point(|e| e.p < lo)..p.0.partition_point(|e| e.p < hi)]);
            }
            es.sort_unstable();
            es.dedup();
            let mut o = RangeOut { cur: Vec::with_capacity(cur.len()), changed: Vec::new(), closed: Vec::new(), news: Vec::new() };
            let (mut pieces, mut scratch) = (Vec::new(), Vec::new());
            let (mut a, mut k) = (0usize, 0usize);
            while a < cur.len() || k < es.len() {
                let ia = cur.get(a).map_or(StateId::MAX, |c| c.id);
                let ik = es.get(k).map_or(StateId::MAX, |e| e.p);
                if ia < ik {
                    let c = cur[a];
                    if c.layer == close_at {
                        o.closed.push((c.id, Span { lo: close_at, hi: c.hi, set: c.set }));
                    } else {
                        o.cur.push(c);
                    }
                    a += 1;
                    continue;
                }
                let k_end = k + es[k..].partition_point(|e| e.p == ik);
                let layer = es[k].layer;
                assert!(es[k..k_end].iter().all(|e| e.layer == layer), "state {} has in-edges recorded at two frames", crate::storage::show_id(ik));
                let base = (ia == ik).then(|| cur[a]);
                if let Some(b) = base {
                    assert_eq!(b.layer, layer, "state {}: its layer from the edges disagrees", crate::storage::show_id(ik));
                    a += 1;
                }
                pieces.clear();
                if let Some(b) = base {
                    sets[b.set as usize].pieces(&mut pieces);
                }
                for e in &es[k..k_end] {
                    let (x, y) = pairs[e.xfer as usize];
                    sets[e.set as usize].pull(x, y, &mut pieces);
                }
                k = k_end;
                if pieces.is_empty() {
                    continue;
                }
                let r = Region::from_pieces(&mut pieces, &mut scratch);
                if let Some(b) = base {
                    if sets[b.set as usize] == r {
                        o.cur.push(b);
                        continue;
                    }
                    o.closed.push((b.id, Span { lo: close_at, hi: b.hi, set: b.set }));
                }
                let h = region_hash(&r);
                let set = match interned.get(&h) {
                    Some(&s) if sets[s as usize] == r => s,
                    _ => {
                        o.news.push(r);
                        NEW_SET | (o.news.len() as u32 - 1)
                    }
                };
                o.changed.push(o.cur.len() as u32);
                o.cur.push(Cur { id: ik, set, hi: t as u16, layer });
            }
            o
        });
        drop(parts);
        let d_pull = t0.elapsed();
        self.d_pull += d_pull;
        // MERGE, in range order: the new sets interned, the ranges joined.
        let t0 = std::time::Instant::now();
        let mut cur = Vec::with_capacity(outs.iter().map(|o| o.cur.len()).sum());
        let mut changed = Vec::new();
        let mut closed = Vec::with_capacity(outs.iter().map(|o| o.closed.len()).sum());
        for o in outs {
            let ids: Vec<u32> = o
                .news
                .into_iter()
                .map(|r| {
                    let h = region_hash(&r);
                    match self.interned.get(&h) {
                        Some(&s) if self.sets[s as usize] == r => s,
                        found => {
                            let s = self.sets.len() as u32;
                            assert!(s < NEW_SET, "more distinct sets than set codes");
                            let fresh = found.is_none();
                            self.sets.push(r);
                            if fresh {
                                self.interned.insert(h, s);
                            }
                            s
                        }
                    }
                })
                .collect();
            let base = cur.len() as u32;
            changed.extend(o.changed.iter().map(|&k| base + k));
            cur.extend(o.cur.into_iter().map(|mut c| {
                if c.set & NEW_SET != 0 {
                    c.set = ids[(c.set & !NEW_SET) as usize];
                }
                c
            }));
            closed.extend(o.closed);
        }
        self.runs.push(closed);
        self.cur = cur;
        self.changed = changed;
        self.max_cur = self.max_cur.max(self.cur.len());
        let d_merge = t0.elapsed();
        self.d_merge += d_merge;
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        eprintln!(
            "[arc] f{t:03}: {n_read} changed at f{} -> {n_emits} in-edges; {} nodes with a set, {} changed | read {:.0} pull {:.0} merge {:.0} ms | anon {:.2} GB",
            t + 1,
            self.cur.len(),
            self.changed.len(),
            ms(d_read),
            ms(d_pull),
            ms(d_merge),
            crate::metrics::current_rss_gb()
        );
        if (self.horizon - 1 - t) % 4 == 0 {
            let wins = &self.wins;
            let (k, med, p90, max) = fragmentation(self.cur.iter().filter(|c| wins.binary_search(&c.id).is_err()).map(|c| &self.sets[c.set as usize]));
            self.fragments.push(format!("[arc] W frame {t:3}: {k} non-win nodes can win; rectangles per node med {med} p90 {p90} max {max}"));
        }
    }

    /// The winning sets, once every frame down to 0 is done.
    pub fn finish(mut self) -> Winning {
        let last: Vec<(StateId, Span)> = std::mem::take(&mut self.cur).into_iter().map(|c| (c.id, Span { lo: 0, hi: c.hi, set: c.set })).collect();
        assert_eq!(self.runs.len(), self.horizon as usize, "finish before frame 0");
        let mut runs = std::mem::take(&mut self.runs);
        runs.push(last);
        let opens: Vec<u32> = self.opens.values().copied().collect();
        let (o_mean, o_max) = (opens.iter().map(|&o| o as f64).sum::<f64>() / opens.len().max(1) as f64, opens.iter().copied().max().unwrap_or(0));
        let w = Winning::from_runs(runs, std::mem::take(&mut self.sets));
        eprintln!(
            "[arc] backward: {} sets changed, {} in-edges read ({} emissions, at most {} a frame), {:.2} GB of the edge store read ({} owner entries, {:.2} GB of lids); {} target regions read on {o_mean:.1} steps each (at most {o_max}), {} (region, frame) opens; at most {} nodes a frame; read {:.2} s, pull {:.2} s, merge {:.2} s; {} nodes, {} spans, {} distinct sets, {:.2} GB",
            self.n_changed,
            self.read.edges,
            self.n_emits,
            self.max_emits,
            self.read.bytes() as f64 / 1e9,
            self.read.owner_probes,
            self.read.lid_bytes as f64 / 1e9,
            opens.len(),
            self.read.region_opens,
            self.max_cur,
            self.d_read.as_secs_f64(),
            self.d_pull.as_secs_f64(),
            self.d_merge.as_secs_f64(),
            w.len(),
            w.spans.len(),
            w.sets.len(),
            w.bytes() as f64 / 1e9,
        );
        for f in &self.fragments {
            eprintln!("{f}");
        }
        w
    }
}

/// The out-edges between `W`'s nodes that can carry a remainder (`reach`):
/// an edge `p -> m` recorded at frame `f` (so `p` has layer `f - 1`) from a
/// non-win `p` to an `m` with a set at `f` or later - the others are on no
/// winning path. By node, each one's `(target, transfer)`s sorted.
pub struct OutGraph {
    at: Vec<u64>,
    out: Vec<(u32, u32)>,
}

impl OutGraph {
    /// Read from the edge files unit by unit as recorded, TWICE - counting
    /// each node's out-degree, then filling in place - so no edge list is
    /// held beside the graph: 8 B an edge.
    pub fn load(store: &EdgeStore, w: &Winning, wins: &[StateId], horizon: u32, threads: usize) -> anyhow::Result<Self> {
        use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
        let n = w.len();
        let xfers = store.pairs().len();
        // The edges of one unit: `f(src, dst, xfer)` per kept one.
        let each_edge = |frame: u32, u: &crate::storage::edges::UnitView, f: &mut dyn FnMut(u32, u32, u32)| -> anyhow::Result<()> {
            let src: Vec<Option<u32>> = (0..u.n_sources() as u32).map(|s| u.source(s)).map(|id| w.index(id).filter(|_| wins.binary_search(&id).is_err())).collect();
            if src.iter().all(Option::is_none) {
                return Ok(());
            }
            // Per lid its entry's nodes: a range of W's ids.
            let owners: Vec<(usize, usize)> = (0..u.n_lids() as u32)
                .map(|l| match u.lid_owner(l) {
                    Some((r, e)) => {
                        let a = w.ids.partition_point(|&id| id < state_id(r, e, 0));
                        (a, a + w.ids[a..].partition_point(|&id| id_region(id) == r && id_entry(id) == e))
                    }
                    None => (0, 0),
                })
                .collect();
            let mut bad = None;
            u.edges(|lid, local, s, x| {
                let Some(p) = src[s as usize] else { return };
                let (a, b) = owners[lid as usize];
                if a == b {
                    return;
                }
                if x as usize >= xfers {
                    bad = Some(x);
                    return;
                }
                if let Ok(k) = w.ids[a..b].binary_search_by_key(&local, |&id| id_local(id)) {
                    let m = (a + k) as u32;
                    if w.last(m) >= frame {
                        f(p, m, x);
                    }
                }
            });
            anyhow::ensure!(bad.is_none(), "an edge with transfer {bad:?} past the table");
            Ok(())
        };
        let each_unit = |f: &(dyn Fn(u32, &crate::storage::edges::UnitView) -> anyhow::Result<()> + Sync)| -> anyhow::Result<()> {
            for frame in 1..=horizon {
                let owners = store.owners(frame, threads);
                let units = store.units(frame);
                par_chunks(units.len(), 16, threads, |lo, hi| -> anyhow::Result<()> {
                    for &(fi, ui) in &units[lo..hi] {
                        f(frame, &store.unit(frame, &owners, fi, ui))?;
                    }
                    Ok(())
                })
                .into_iter()
                .collect::<anyhow::Result<()>>()?;
            }
            Ok(())
        };
        let deg: Vec<AtomicU64> = (0..n).map(|_| AtomicU64::new(0)).collect();
        each_unit(&|frame, u| {
            each_edge(frame, u, &mut |p, _, _| {
                deg[p as usize].fetch_add(1, Relaxed);
            })
        })?;
        let mut at = Vec::with_capacity(n + 1);
        at.push(0u64);
        for d in &deg {
            at.push(at.last().expect("one") + d.load(Relaxed));
        }
        let edges = at[n] as usize;
        let mut out: Vec<(u32, u32)> = vec![(0, 0); edges];
        for (d, &a) in deg.iter().zip(&at) {
            d.store(a, Relaxed);
        }
        let out_ptr = out.as_mut_ptr() as usize;
        each_unit(&|frame, u| {
            each_edge(frame, u, &mut |p, m, x| {
                let slot = deg[p as usize].fetch_add(1, Relaxed);
                assert!(slot < at[p as usize + 1], "the edge fill pass disagrees with the count");
                // SAFETY: each slot is claimed once, by its node's atomic
                // cursor, inside the node's range, below `edges`.
                unsafe { (out_ptr as *mut (u32, u32)).add(slot as usize).write((m, x)) };
            })
        })?;
        anyhow::ensure!(deg.iter().zip(&at[1..]).all(|(c, &e)| c.load(Relaxed) == e), "the edge fill pass disagrees with the count");
        drop(deg);
        let at_ref = &at;
        let out_ref = out.as_mut_ptr() as usize;
        par_chunks(n, 4096, threads, |lo, hi| {
            // SAFETY: the nodes `lo..hi` own the disjoint range
            // `at[lo]..at[hi]`.
            let o = unsafe { std::slice::from_raw_parts_mut((out_ref as *mut (u32, u32)).add(at_ref[lo] as usize), (at_ref[hi] - at_ref[lo]) as usize) };
            let base = at_ref[lo];
            for i in lo..hi {
                o[(at_ref[i] - base) as usize..(at_ref[i + 1] - base) as usize].sort_unstable();
            }
        });
        Ok(OutGraph { at, out })
    }
    fn of(&self, i: u32) -> &[(u32, u32)] {
        &self.out[self.at[i as usize] as usize..self.at[i as usize + 1] as usize]
    }
    pub fn edges(&self) -> usize {
        self.out.len()
    }
}

/// FORWARD inside W: `R_t(i)`, the remainders with which node `i` is reached
/// at frame `t` from the start (at `point`) along the graph's edges and
/// still wins - `R_0 = {point}` at the start, `R_{t+1}(m) = U push_e(R_t(p))
/// ∩ W_{t+1}(m)` over the edges `p -> m` (a win node is not left: it has no
/// out-edges). Per node the LAST frame its `R` is non-empty (`u32::MAX`:
/// never), for the next level's filter. Each frame SCATTERS: every reached
/// node's set pushed along its out-edges, the pushes into a target united.
///
/// Sound as a filter where `W`'s deadline is: a concrete winning path's
/// remainder at frame `t` is in `W_t` of its node (W holds every winner)
/// and is the image of its remainder at `t - 1` along the recorded edge (the
/// arc model `arc-check` probes), so by induction it is in `R_t`. Tighter:
/// a node `W` lets win from some remainder no path from the start brings is
/// not marked.
///
/// A set more fragmented than `REACH_SEGS` segments is replaced by `W_t`
/// itself (a superset: the filter only loosens), which bounds the memory -
/// W's sets are shared, a node's reached set is its own.
#[allow(clippy::too_many_arguments)]
pub fn reach(w: &Winning, g: &OutGraph, pairs: &[(Transfer, Transfer)], start: Option<u32>, point: (u32, u32), horizon: u32, threads: usize) -> Vec<u32> {
    use super::arcs::Piece;
    let n = w.len();
    let threads = threads.max(1);
    let mut last = vec![u32::MAX; n];
    const REACH_SEGS: usize = 32;
    // The current frame's sets, by node (`None`: W_t of the node).
    let mut cur: Vec<(u32, Option<Region>)> = Vec::new();
    if let Some(s) = start {
        if w.at(0, s).is_some_and(|r| r.contains(point.0, point.1)) {
            let p = Piece { y: super::arcs::Seg { lo: point.1, hi: point.1 + 1 }, x: super::arcs::Seg { lo: point.0, hi: point.0 + 1 } };
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
        // targets with a set at t + 1, tagged by target.
        let cur_ref = &cur;
        let mut pushed: Vec<(u32, Piece)> = par_chunks(cur.len(), 64, threads, |lo, hi| {
            let (mut out, mut tmp) = (Vec::new(), Vec::new());
            for (p, r) in &cur_ref[lo..hi] {
                let r = r.as_ref().unwrap_or_else(|| w.at(t, *p).expect("a node reached in W has a set"));
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
        let next: Vec<(u32, Option<Region>)> = par_chunks(groups.len(), 256, threads, |lo, hi| {
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
    last
}

/// The OPTIMUM from `start` at remainder `point`, read off ONE backward:
/// `horizon - max{t : point in W_t(start)}` (`None`: the horizon is
/// refuted). Valid because the graph is the same at every frame but for
/// the layers, and k steps from the start reach only layers <= k, so a
/// layer never binds.
pub fn optimum(w: &Winning, start: Option<u32>, point: (u32, u32), horizon: u32) -> Option<u32> {
    let s = start?;
    (0..=horizon).rev().find(|&t| w.at(t, s).is_some_and(|r| r.contains(point.0, point.1))).map(|t| horizon - t)
}

/// Frames `0..=horizon` of the tree in `dir`, each its files `(seq, file)`.
/// A forward whose frontier died (a finer level of the objects ladder,
/// filtered down to nothing) has no frames past the empty one: they are
/// empty too. Any other missing frame is an error.
fn tree_frames(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Vec<Vec<(u32, crate::search::checkpoint::FrameFile)>>> {
    let mut out: Vec<Vec<(u32, crate::search::checkpoint::FrameFile)>> = Vec::with_capacity(horizon as usize + 1);
    for f in 0..=horizon {
        if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
            let died = out.last().is_some_and(|last| last.iter().all(|(_, file)| file.width() == 0));
            anyhow::ensure!(died, "{}: no frame f{f}, and the frontier before it was not empty", dir.display());
            out.push(Vec::new());
            continue;
        }
        out.push(crate::frame::frame_files(dir, f)?);
    }
    Ok(out)
}

/// What the backward starts from in the tree in `dir` up to `horizon`: the
/// start's id (frame 0's one row), the win rows of frames `1..=horizon`
/// with their layers, and the edge store - every frame that kept rows has
/// its edges (one that kept none, under level -1, may have none).
pub struct Seeds {
    pub start: StateId,
    pub wins: Vec<(StateId, u32)>,
    pub store: EdgeStore,
    /// The transfer table, decoded.
    pub pairs: Vec<(Transfer, Transfer)>,
}

impl Seeds {
    pub fn load(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Self> {
        let mut wins: Vec<(StateId, u32)> = Vec::new();
        let mut start: Option<StateId> = None;
        let mut kept: Vec<u32> = Vec::with_capacity(horizon as usize + 1);
        for (f, files) in tree_frames(dir, horizon)?.into_iter().enumerate() {
            let f = f as u32;
            let mut rows = 0u32;
            for (_, file) in files {
                rows += file.width();
                if f == 0 {
                    anyhow::ensure!(start.is_none() && file.width() == 1, "{}: more than one start row", dir.display());
                    start = Some(file.id_at(0));
                } else {
                    wins.extend(file.win_rows().iter().map(|&(row, _)| (file.id_at(row), f)));
                }
            }
            kept.push(rows);
        }
        let start = start.ok_or_else(|| anyhow::anyhow!("{}: no frame-0 row", dir.display()))?;
        let store = EdgeStore::open(&dir.join("edges"), horizon)?;
        for (f, &rows) in kept.iter().enumerate().skip(1) {
            anyhow::ensure!(rows == 0 || store.has_frame(f as u32), "{}: no edges for f{f}, which kept {rows} rows", dir.display());
        }
        let pairs = store.pairs().iter().map(|p| (p.0.transfer(), p.1.transfer())).collect();
        Ok(Seeds { start, wins, store, pairs })
    }

    /// The win states, sorted.
    pub fn win_ids(&self) -> Vec<StateId> {
        let mut v: Vec<StateId> = self.wins.iter().map(|w| w.0).collect();
        v.sort_unstable();
        v.dedup();
        v
    }
}

/// Fragmentation of winning sets (x segments per node, over all slabs) as
/// (nodes, median, p90, max).
pub fn fragmentation<'a>(sets: impl Iterator<Item = &'a Region>) -> (usize, usize, usize, usize) {
    let mut v: Vec<usize> = sets.map(|r| r.n_segs()).collect();
    if v.is_empty() {
        return (0, 0, 0, 0);
    }
    v.sort_unstable();
    let q = |f: f64| v[((v.len() - 1) as f64 * f) as usize];
    (v.len(), q(0.5), q(0.9), q(1.0))
}

/// A concrete witness: the inputs, and the player's cell after each (the
/// start's first).
pub struct Witness {
    pub inputs: Vec<u8>,
    pub cells: Vec<u32>,
}

impl Witness {
    /// The inputs as a comma list (`concrete_run -i`, `pico8_diff/replay.py`).
    pub fn inputs_text(&self) -> String {
        self.inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")
    }

    /// `witness.txt`: `inputs ...`, then `f x y` per frame (frame 0 the start).
    pub fn save(&self, path: &std::path::Path) -> anyhow::Result<()> {
        let mut text = format!("inputs {}\n", self.inputs_text());
        for (f, &c) in self.cells.iter().enumerate() {
            match super::pos_graph::cell_xy(c) {
                Some((x, y)) => text += &format!("{f} {x} {y}\n"),
                None => text += &format!("{f} - -\n"),
            }
        }
        Ok(std::fs::write(path, text)?)
    }
}

/// W's nodes by `(shape, key, cell)`, for the concrete search and the known
/// route: a state's id is its region (shape and cell), its entry (the
/// region's number for its key) and its cell, so only the ENTRIES are
/// listed - `((region, key), entry)` per distinct entry of the support,
/// sorted, 24 B - and the node is the id's index in `W`.
pub struct NodeKeys<'w> {
    w: &'w Winning,
    geo: crate::storage::Geometry,
    /// Shape hash -> shape index, sorted.
    shapes: Vec<(u64, u32)>,
    entries: Vec<((u32, (u64, u64)), u32)>,
}

impl<'w> NodeKeys<'w> {
    /// Over `w`'s support, its entries' keys from the tree's metadata.
    pub fn new(w: &'w Winning, resolver: &crate::storage::marks::Resolver) -> anyhow::Result<Self> {
        let geo = crate::storage::GEOMETRY;
        let (mut shapes, mut entries) = (Vec::new(), Vec::new());
        let mut last = None;
        for &id in &w.ids {
            let (region, entry) = (id_region(id), id_entry(id));
            if last == Some((region, entry)) {
                continue;
            }
            last = Some((region, entry));
            let (shape, key, _) = resolver.resolve(id)?;
            shapes.push((shape, region / geo.slots));
            entries.push(((region, key), entry));
        }
        shapes.sort_unstable();
        shapes.dedup();
        anyhow::ensure!(shapes.windows(2).all(|p| p[0].0 != p[1].0), "a shape with two indices");
        entries.sort_unstable();
        Ok(NodeKeys { w, geo, shapes, entries })
    }
    pub(super) fn get(&self, shape: u64, key: Option<(u64, u64)>, cell: u32) -> Option<u32> {
        let key = key?;
        let k = self.shapes.binary_search_by_key(&shape, |s| s.0).ok()?;
        let (slot, local) = self.geo.slot_local(cell);
        let region = crate::storage::region_of(&self.geo, self.shapes[k].1, slot);
        let e = self.entries.binary_search_by(|e| e.0.cmp(&(region, key))).ok()?;
        self.w.index(state_id(region, self.entries[e].1, local))
    }
    /// The support's distinct entries.
    pub fn entries(&self) -> usize {
        self.entries.len()
    }
}

/// A concrete state's node key at `level` (`lookup_view`, widened) in the
/// tree's key space (`None`: a code the tree never stored - no node).
pub(super) fn lookup_keys(b: &crate::frame::Block, level: crate::abstraction::Level, seeded: bool, keys: &celeste_engine::exact::KeySpace) -> anyhow::Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    if !seeded {
        return crate::frame::widened_keys(b, level, keys);
    }
    crate::frame::widened_keys_rt2(&lookup_view(b.rt2(), seeded)?, level, keys)
}

/// A concrete state's EXACT identity, UNWIDENED: its canonical bytes
/// (`exact::exact_rows`) and its cell. Never the level's widened key:
/// states sharing it need not share their fate (`522de36`).
pub(super) fn exact_state(b: &crate::frame::Block, cell: u32) -> celeste_engine::exact::ExactRow {
    let mut v = celeste_engine::exact::exact_rows(b.rt2(), crate::compiled::ids()).swap_remove(0).into_vec();
    v.extend_from_slice(&cell.to_le_bytes());
    v.into_boxed_slice()
}

/// A concrete state as the tree holds it before the level's widening. With
/// seeded balloons (`concrete_search`), the state holds each balloon's
/// phase as one number and its `y` as one point, where the tree - built
/// with the phase an interval - holds the canonical phase `[0, 1)` and the
/// bob `y` `[start - 2, start + 2]` (the forward recomputes `y` from the
/// interval phase every frame); the view maps them there, so its key is the
/// node that over-approximates the state.
pub(super) fn lookup_view(row: &celeste_engine::runtime2::Rt2, seeded: bool) -> anyhow::Result<celeste_engine::runtime2::Rt2> {
    use celeste_engine::runtime2::{Col, AV};
    use celeste_engine::widening::{BALLOON_BOB_RAW, BALLOON_PERIOD_RAW};
    use crate::pico8_num::Pico8Num as P8;
    let mut rt2 = row.clone_block();
    if !seeded {
        return Ok(rt2);
    }
    let ids = crate::compiled::ids();
    for obj in rt2.objects_of_type(ids, ids.g_balloon) {
        let (Some(oc), Some(yc), Some(sc)) = (rt2.obj_field_cell(obj, ids.f_offset), rt2.obj_field_cell(obj, ids.f_y), rt2.obj_field_cell(obj, ids.f_start)) else { continue };
        let AV::Num(start) = rt2.cols[sc as usize].at(0) else { anyhow::bail!("a balloon's `start` is not a number") };
        let s = start.as_raw_u32() as i32;
        rt2.cols[oc as usize] = Col::U(AV::Ival(P8::from_raw(0), P8::from_raw(BALLOON_PERIOD_RAW)));
        rt2.cols[yc as usize] = Col::U(AV::Ival(P8::from_raw(s - BALLOON_BOB_RAW), P8::from_raw(s + BALLOON_BOB_RAW)));
    }
    Ok(rt2)
}

/// A concrete state's remainder as a point of the torus (no player: the
/// point (0, 0)).
pub(super) fn rem_of(rt2: &celeste_engine::runtime2::Rt2) -> anyhow::Result<(u32, u32)> {
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::ids();
    let Some((cx, cy)) = rt2.player_xy_cells(ids, ids.f_rem) else { return Ok((super::arcs::point(0), super::arcs::point(0))) };
    let raw = |c: usize| -> anyhow::Result<u32> {
        match rt2.cols[c].at(0) {
            AV::Num(n) => Ok(super::arcs::point(n.as_raw_u32() as i32)),
            other => anyhow::bail!("a concrete remainder expected, got {other:?}"),
        }
    };
    Ok((raw(cx)?, raw(cy)?))
}

/// `n` reference engines for concrete steps, and whether they run seeded:
/// `--concrete-balloon-seeds` fixes the balloons' phases (a tasdatabase
/// file's seeds), so a concrete run is one real run under them - the tree
/// and W keep the phase an interval (sound for every seed), and a concrete
/// state's key maps its phase to that interval (`lookup_view`). Built under
/// the run's settings `run`: `_init` runs under them.
pub(super) fn concrete_engines(n: usize, run: &crate::settings::Settings) -> anyhow::Result<(Vec<crate::trace::refengine::RefEngine>, bool)> {
    let seeded = run.concrete_balloon_seeds.is_some();
    let s = crate::settings::Settings { balloon_seeds: run.concrete_balloon_seeds.clone().or_else(|| run.balloon_seeds.clone()), ..run.clone() };
    let engines = (0..n).map(|_| crate::trace::refengine::RefEngine::new(&s)).collect::<anyhow::Result<Vec<_>>>()?;
    Ok((engines, seeded))
}

/// The inputs a state has been stepped under, each with the buttons its
/// frame read (`RefEngine::frame_reads`). An input that agrees with one of
/// them on every button that one read runs the same frame - the same reads,
/// the same values, the same successors (the output writes no button) - so
/// its successors are repeats of states already emitted in (parent, input)
/// order, which the search's exact dedup would drop: skipping it changes
/// nothing but the step count.
#[derive(Default)]
struct Covered(Vec<(u8, u8)>);

impl Covered {
    fn covers(&self, byte: u8) -> bool {
        self.0.iter().any(|&(b, read)| (b ^ byte) & read == 0)
    }

    fn push(&mut self, byte: u8, read: u8) {
        self.0.push((byte, read));
    }
}

/// The 64 inputs in the order the concrete search tries them at the frame
/// after `k`: `prefer[k]` first (`rewrite search --prefer`: a known TAS, so
/// the witness follows it wherever an optimal route allows), then the rest.
/// Every input is still tried: the order only picks WHICH optimal witness.
fn input_order(prefer: Option<&[u8]>, k: u32) -> impl Iterator<Item = u8> {
    let first = prefer.and_then(|p| p.get(k as usize).copied()).filter(|&b| b < 64);
    first.into_iter().chain((0u8..64).filter(move |&b| Some(b) != first))
}

/// search over concrete states through the reference engine (every input,
/// every `rnd` leaf), layer k+1 admitting a successor only if its projection
/// onto `level` is a node of `g` whose `W_{k+1}` holds its exact remainder
/// (no player: the point (0, 0)), each EXACT state once (the first in
/// (parent, input, leaf) order: deterministic).
///
/// Sound: every concrete path that wins by the horizon stays inside W, so
/// the search is EXHAUSTIVE inside W and prunes by nothing else; its first
/// win is the CONCRETE optimum. `None`: no concrete win by the horizon.
/// `bound` (the graph's optimum, in frames) is a lower bound on it. Layers
/// run in parallel, one reference engine per worker. `horizon` is in search
/// steps, W's index: under the split frame layer k is W at step `2k`, a
/// frame boundary, whose nodes key exactly as a whole frame's states.
pub fn concrete_search(
    level: crate::abstraction::Level,
    horizon: u32,
    node: &NodeKeys<'_>,
    keys: &celeste_engine::exact::KeySpace,
    w: &Winning,
    bound: u32,
    prefer: Option<&[u8]>,
    s: &crate::settings::Settings,
) -> anyhow::Result<Option<Witness>> {
    use crate::frame::{wins_of, Block};
    use anyhow::Result;
    use crate::trace::refengine::RefEngine;
    use celeste_engine::runtime2::Rt2;
    // Their frames run exact (`RefEngine::frame`); `level` is the lookups'.
    let workers = s.threads().max(1);
    let (engines, seeded) = concrete_engines(workers, s)?;
    let engines: Vec<std::sync::Mutex<RefEngine>> = engines.into_iter().map(std::sync::Mutex::new).collect();
    let initial = engines[0].lock().expect("an engine").initial()?;
    // W is indexed by search STEP; the concrete search steps whole frames
    // (`RefEngine::frame`), and frame k ends at step `spf * k`.
    let spf = s.steps_per_frame();
    anyhow::ensure!(horizon % spf == 0, "the horizon {horizon} is not a whole number of frames ({spf} steps each)");
    let frames = horizon / spf;
    let t0 = std::time::Instant::now();
    eprintln!("[concrete] {} nodes keyed by {} entries; {workers} workers; the arc bound f{bound}", w.len(), node.entries());
    crate::metrics::mem_phase("concrete: engines up");
    /// A successor that stays inside W (or wins), in (parent, input, leaf)
    /// order within its worker's chunk.
    struct Succ {
        parent: u32,
        byte: u8,
        cell: u32,
        win: bool,
        exact: celeste_engine::exact::ExactRow,
        /// `None` for a win (its state is never expanded).
        row: Option<Rt2>,
    }
    let start = Block::canonical(initial)?;
    let start_cell = start.positions()?[0];
    // THE FAST PATH: a budgeted depth-first search for a win AT the bound,
    // inside `W_{H - bound + k}`, on one engine. Sound: a win here is at the
    // lower bound, hence optimal; an exhausted or abandoned DFS proves nothing
    // and the breadth-first search runs.
    {
        struct Dfs<'a> {
            eng: &'a mut RefEngine,
            node: &'a NodeKeys<'a>,
            keys: &'a celeste_engine::exact::KeySpace,
            w: &'a Winning,
            level: crate::abstraction::Level,
            from: u32,
            frames: u32,
            /// Exhausted states by (exact state, frame).
            dead: FxHashSet<(celeste_engine::exact::ExactRow, u32)>,
            path: Vec<(u8, u32)>,
            steps: u64,
            prefer: Option<&'a [u8]>,
            seeded: bool,
            spf: u32,
            settings: &'a crate::settings::Settings,
        }
        /// `Ok(None)`: out of budget.
        fn dfs(cx: &mut Dfs, st: &Rt2, k: u32) -> Result<Option<bool>> {
            if k >= cx.frames {
                return Ok(Some(false));
            }
            let mut ran = Covered::default();
            for byte in input_order(cx.prefer, k) {
                if ran.covers(byte) {
                    continue;
                }
                let (succ, read) = cx.eng.frame_reads(st, byte)?;
                ran.push(byte, read);
                for b in succ {
                    cx.steps += 1;
                    if cx.steps > DFS_BUDGET {
                        return Ok(None);
                    }
                    let cell = b.positions()?[0];
                    if wins_of(b.rt2(), cx.settings)?.iter().any(|&x| x) {
                        // A win before the lower bound means a lost path.
                        anyhow::ensure!(k + 1 == cx.frames, "a concrete win at f{} before the arc bound f{}: the backward lost a path", k + 1, cx.frames);
                        cx.path.push((byte, cell));
                        return Ok(Some(true));
                    }
                    let exact = (exact_state(&b, cell), k + 1);
                    if cx.dead.contains(&exact) {
                        continue;
                    }
                    let (shape, nk, cells) = lookup_keys(&b, cx.level, cx.seeded, cx.keys)?;
                    let Some(i) = cx.node.get(shape, nk[0], cells[0]) else { continue };
                    let q = rem_of(b.rt2())?;
                    if !cx.w.at(cx.spf * (cx.from + k + 1), i).is_some_and(|r| r.contains(q.0, q.1)) {
                        continue;
                    }
                    cx.path.push((byte, cell));
                    match dfs(cx, b.rt2(), k + 1)? {
                        Some(true) => return Ok(Some(true)),
                        None => return Ok(None),
                        Some(false) => {}
                    }
                    cx.path.pop();
                    cx.dead.insert(exact);
                }
            }
            Ok(Some(false))
        }
        /// Concrete steps the fast path may take before it gives up.
        const DFS_BUDGET: u64 = 200_000;
        let t = std::time::Instant::now();
        let mut eng = engines[0].lock().expect("an engine");
        let mut cx = Dfs { eng: &mut eng, node, keys, w, level, from: frames - bound, frames: bound, dead: FxHashSet::default(), path: Vec::new(), steps: 0, prefer, seeded, spf, settings: s };
        let found = dfs(&mut cx, start.rt2(), 0)?;
        eprintln!(
            "[concrete] depth-first at the bound f{bound}: {} after {} steps, {:.1} s",
            match found {
                Some(true) => "a win",
                Some(false) => "no win (exhaustive)",
                None => "out of budget",
            },
            cx.steps,
            t.elapsed().as_secs_f64()
        );
        if found == Some(true) {
            let inputs = cx.path.iter().map(|&(b, _)| b).collect();
            let cells = std::iter::once(start_cell).chain(cx.path.iter().map(|&(_, c)| c)).collect();
            return Ok(Some(Witness { inputs, cells }));
        }
    }
    // Per layer k >= 1, each state's (parent index in layer k-1, input, cell).
    let mut back: Vec<Vec<(u32, u8, u32)>> = Vec::new();
    let mut cur: Vec<Rt2> = vec![start.into_rt2()];
    let mut steps = 0u64;
    for k in 0..frames {
        let t = std::time::Instant::now();
        let t_w = spf * (k + 1);
        const CHUNK: usize = 16;
        let next_chunk = std::sync::atomic::AtomicUsize::new(0);
        let parts: Vec<Result<(Vec<(usize, Vec<Succ>)>, u64)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = engines
                .iter()
                .map(|engine| {
                    let (cur, next_chunk) = (&cur, &next_chunk);
                    sc.spawn(move || -> Result<(Vec<(usize, Vec<Succ>)>, u64)> {
                        let mut eng = engine.lock().expect("an engine");
                        let (mut out, mut steps) = (Vec::new(), 0u64);
                        loop {
                            let lo = next_chunk.fetch_add(CHUNK, std::sync::atomic::Ordering::Relaxed);
                            if lo >= cur.len() {
                                return Ok((out, steps));
                            }
                            // Dedup within the chunk already (the first in
                            // order, as the merge keeps it): most successors
                            // are repeats.
                            let mut got = Vec::new();
                            let mut chunk_seen: FxHashSet<celeste_engine::exact::ExactRow> = FxHashSet::default();
                            'chunk: for (p, row) in cur.iter().enumerate().skip(lo).take(CHUNK) {
                                let mut ran = Covered::default();
                                for byte in input_order(prefer, k) {
                                    if ran.covers(byte) {
                                        continue;
                                    }
                                    let (succ, read) = eng.frame_reads(row, byte).map_err(|e| e.context(format!("the frame after layer {k} with input {byte}")))?;
                                    ran.push(byte, read);
                                    for b in succ {
                                        steps += 1;
                                        let cell = b.positions()?[0];
                                        // Dedup on the EXACT state, never the level's
                                        // widened key (`exact_state`).
                                        let exact = exact_state(&b, cell);
                                        let win = wins_of(b.rt2(), s)?.iter().any(|&x| x);
                                        if !win && chunk_seen.contains(&exact) {
                                            continue;
                                        }
                                        if !win {
                                            let (shape, nk, cells) = lookup_keys(&b, level, seeded, keys)?;
                                            let Some(i) = node.get(shape, nk[0], cells[0]) else { continue };
                                            let q = rem_of(b.rt2())?;
                                            if !w.at(t_w, i).is_some_and(|r| r.contains(q.0, q.1)) {
                                                continue;
                                            }
                                        }
                                        chunk_seen.insert(exact.clone());
                                        // A win keeps no state (only its link), and the chunk
                                        // ends at its first: the earliest win (chunks merge in
                                        // order) ends the search. Every successor of the last
                                        // layer can win - kept whole, room (4,3) 100% reached
                                        // 100 GB there.
                                        got.push(Succ { parent: p as u32, byte, cell, win, exact, row: (!win).then(|| b.into_rt2()) });
                                        if win {
                                            break 'chunk;
                                        }
                                    }
                                }
                            }
                            out.push((lo, got));
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a concrete worker panicked")).collect()
        });
        let mut chunks: Vec<(usize, Vec<Succ>)> = Vec::new();
        for p in parts {
            let (c, s) = p?;
            chunks.extend(c);
            steps += s;
        }
        chunks.sort_unstable_by_key(|c| c.0);
        let succs = chunks.into_iter().flat_map(|c| c.1);
        // Each exact state once, in order; the first win ends the search.
        let mut seen: FxHashSet<celeste_engine::exact::ExactRow> = FxHashSet::default();
        let (mut next, mut links): (Vec<Rt2>, Vec<(u32, u8, u32)>) = (Vec::new(), Vec::new());
        let mut won: Option<(u32, u8, u32)> = None;
        for s in succs {
            if s.win {
                won = Some((s.parent, s.byte, s.cell));
                break;
            }
            if seen.insert(s.exact) {
                next.push(s.row.expect("a non-winning successor keeps its state"));
                links.push((s.parent, s.byte, s.cell));
            }
        }
        eprintln!(
            "[concrete] layer {:3}: {} states -> {} inside W{}; {steps} steps so far, {:.1} s, rss {:.1} GB",
            k + 1,
            cur.len(),
            next.len(),
            if won.is_some() { ", A WIN" } else { "" },
            t.elapsed().as_secs_f64(),
            crate::metrics::current_rss_gb()
        );
        if let Some((parent, byte, cell)) = won {
            anyhow::ensure!(k + 1 >= bound, "a concrete win at f{} before the arc bound f{bound}: the backward lost a path", k + 1);
            // The path, back through the layers.
            let (mut inputs, mut cells) = (vec![byte], vec![cell]);
            let mut p = parent;
            for layer in back.iter().rev() {
                let (pp, b, c) = layer[p as usize];
                inputs.push(b);
                cells.push(c);
                p = pp;
            }
            inputs.reverse();
            cells.push(start_cell);
            cells.reverse();
            eprintln!("[concrete] a win at f{} after {steps} concrete steps, {:.1} s", inputs.len(), t0.elapsed().as_secs_f64());
            return Ok(Some(Witness { inputs, cells }));
        }
        if next.is_empty() {
            println!("NO CONCRETE WIN by f{frames}: no concrete state inside W past layer {k} ({steps} steps)");
            return Ok(None);
        }
        back.push(links);
        cur = next;
    }
    println!("NO CONCRETE WIN by f{frames} ({steps} steps)");
    Ok(None)
}

/// What the arc phase of a search found (`solve`).
pub struct Solved {
    /// The optimum over the rotation graph, a lower bound on the game's, in
    /// FRAMES (`horizon` and the marks' deadlines are in search steps).
    /// `None`: the horizon is REFUTED.
    pub arc: Option<u32>,
    /// The concrete optimum and its witness, when asked for and found.
    pub concrete: Option<Witness>,
    /// The ARC-MARKED nodes with their deadlines (the last frame their
    /// winning set is non-empty): the next level's `frame::MarkFilter`.
    pub arc_marks: Option<crate::frame::Visited>,
    /// The known route's exit frame when it was checked (`search::known`):
    /// it wins by the horizon and survives every pruning step.
    pub known: Option<u32>,
}

/// One node's term of the gate's `W f{t}` line: the node's content hash
/// (`exact::ContentHash::state`) folded with its set's rectangles
/// (`rewrite bench-backward` prints the same line for one frame).
pub fn gate_w_hash(node: u64, r: &Region) -> u64 {
    use celeste_engine::runtime2::mix64;
    let mut h = node;
    for (y, xs) in r.slabs() {
        h = mix64(h ^ ((y.lo as u64) << 32 | y.hi as u64));
        for x in xs {
            h = mix64(h ^ ((x.lo as u64) << 32 | x.hi as u64));
        }
    }
    h
}

/// THE ARC PHASE over a finished tree in `dir`: `backward`, `optimum`, and
/// as much of `concrete_search` as `concrete` says. Prints the `[gate]`
/// fingerprints (over (shape, key, cell), independent of scheduling): W's
/// support, W at every frame, the optimum. With `save`, writes the UI's arc
/// pass: `level0.marks.bin` (W's support, each node to its last frame),
/// `arc.marks.bin` (the nodes `reach` reaches), `arc.txt` and the witness.
/// With `known`, checks a known solution against every pruning step before
/// the concrete search (`search::known`): an error if one drops it.
#[allow(clippy::too_many_arguments)]
pub fn solve(
    dir: &std::path::Path,
    level: crate::abstraction::Level,
    horizon: u32,
    concrete: bool,
    want_marks: bool,
    save: Option<&std::path::Path>,
    prefer: Option<&[u8]>,
    known: Option<&super::known::Route>,
    s: &crate::settings::Settings,
) -> anyhow::Result<Solved> {
    use crate::frame::{mark_row, save_marks, MarkRow, Visited};
    s.check(dir, level)?;
    crate::metrics::mem_phase("arc: start");
    let threads = s.threads();
    let seeds = Seeds::load(dir, horizon)?;
    crate::metrics::mem_phase("arc: edge files opened");
    let tb = std::time::Instant::now();
    // The backward's reads are scattered: no read-ahead.
    seeds.store.advise(memmap2::Advice::Random)?;
    let w = backward(&seeds.store, &seeds.pairs, &seeds.wins, horizon, threads);
    crate::metrics::mem_phase("arc: backward");
    let tb = tb.elapsed().as_secs_f64();
    let start = w.index(seeds.start);
    // The start has no player yet: any point stands for its remainder.
    let p0 = (super::arcs::point(0), super::arcs::point(0));
    let arc_steps = optimum(&w, start, p0, horizon);
    // In frames: a win at step s is during frame ceil(s / spf) (a lower
    // bound still: a frame's win is a win at its last step at the latest).
    let arc = arc_steps.map(|k| k.div_ceil(s.steps_per_frame()));
    eprintln!("[arc] backward {tb:.2} s; optimum {arc_steps:?} (steps)");
    // The nodes as (shape, key, cell), through the storage metadata: the
    // fingerprints, the marks files and the concrete search's keys.
    let resolver = crate::storage::marks::Resolver::load(dir, horizon)?;
    // Fingerprints over CONTENT (`exact::ContentHash`): independent of the
    // order the tree's dictionaries grew in (a raise).
    let content = resolver.space.content_hash();
    let mut files = Vec::new();
    for (f, fs) in tree_frames(dir, horizon)?.into_iter().enumerate() {
        files.extend(fs.into_iter().map(|(seq, file)| (f as u32, seq, file)));
    }
    let mut rows: Vec<MarkRow> = Vec::new();
    let mut fp = 0u64;
    // Per frame, the nodes with a set and the sum of their (node, set) hashes.
    let mut per_frame = vec![(0usize, 0u64); horizon as usize + 1];
    for i in 0..w.len() as u32 {
        let (shape, key, cell) = resolver.resolve(w.id(i))?;
        // `Visited::fingerprint`: the nodes are distinct states.
        let h = content.state(shape, key, cell);
        fp = fp.wrapping_add(h);
        for (lo, hi, r) in w.spans_of(i) {
            let g = gate_w_hash(h, r);
            for f in &mut per_frame[lo as usize..=hi as usize] {
                f.0 += 1;
                f.1 = f.1.wrapping_add(g);
            }
        }
        if save.is_some() {
            rows.push(mark_row(shape, key, cell, w.last(i) as u16, horizon));
        }
    }
    println!("[gate] h{horizon} support {} {fp:016x}", w.len());
    if let Some(out) = save {
        std::fs::create_dir_all(out)?;
        save_marks(&out.join("level0.marks.bin"), std::mem::take(&mut rows), horizon, &resolver.space)?;
    }
    for (t, (n, acc)) in per_frame.into_iter().enumerate() {
        println!("[gate] h{horizon} W f{t:03} {n} {acc:016x}");
    }
    println!("[gate] h{horizon} arc optimum {}", arc_steps.map_or("none".to_string(), |f| f.to_string()));
    crate::metrics::mem_phase("arc: fingerprints");
    // Per node the LAST frame it is reached inside W from the start (`reach`;
    // at most its W deadline): the next level's filter, and the UI's arc marks.
    let mut arc_marks = want_marks.then(|| Visited::new(resolver.space.clone()));
    if want_marks || save.is_some() {
        let tr = std::time::Instant::now();
        seeds.store.advise(memmap2::Advice::Normal)?;
        let g = OutGraph::load(&seeds.store, &w, &seeds.win_ids(), horizon, threads)?;
        let tl = tr.elapsed().as_secs_f64();
        crate::metrics::mem_phase("arc: W's out-edges");
        let last = reach(&w, &g, &seeds.pairs, start, p0, horizon, threads);
        eprintln!("[arc] reach {:.2} s ({} out-edges between W's nodes read in {tl:.2} s)", tr.elapsed().as_secs_f64(), g.edges());
        drop(g);
        crate::metrics::mem_phase("arc: reach");
        for i in (0..w.len() as u32).filter(|&i| last[i as usize] != u32::MAX) {
            let (shape, key, cell) = resolver.resolve(w.id(i))?;
            let d = last[i as usize] as u16;
            if let Some(am) = &mut arc_marks {
                am.insert_until(shape, key, cell, d);
            }
            if save.is_some() {
                rows.push(mark_row(shape, key, cell, d, horizon));
            }
        }
    }
    if let Some(out) = save {
        save_marks(&out.join("arc.marks.bin"), rows, horizon, &resolver.space)?;
        let first_win = seeds.wins.iter().map(|&(_, f)| f).min();
        let show = |v: Option<u32>| v.map_or("none".to_string(), |f| f.to_string());
        std::fs::write(out.join("arc.txt"), format!("horizon {horizon}\nlevel {level}\nlevel0_first_win {}\noptimal {}\n", show(first_win), show(arc)))?;
    }
    // The concrete search reads W and the node keys, not the edge files.
    drop(seeds);
    let keyed = (concrete && arc.is_some()) || known.is_some();
    let node = if keyed { Some(NodeKeys::new(&w, &resolver)?) } else { None };
    let known = match (known, &node) {
        (Some(route), Some(node)) => {
            let minus_one = crate::frame::TreeFilter::of_tree(dir, s)?;
            let p = super::known::Pruning { settings: s, level, horizon, node, keys: &resolver.space, start, w: &w, reached: arc_marks.as_ref(), minus_one, files: &files, tree: &resolver };
            super::known::check(route, &p)?
        }
        _ => None,
    };
    drop(files);
    crate::metrics::mem_phase("arc: marks for the next level, edge files closed");
    let found = match (concrete, arc, &node) {
        (true, Some(f), Some(node)) => concrete_search(level, horizon, node, &resolver.space, &w, f, prefer, s)?,
        _ => None,
    };
    if let Some(wt) = &found {
        println!("[gate] h{horizon} concrete optimum {} inputs {}", wt.inputs.len(), wt.inputs_text());
        if let Some(out) = save {
            wt.save(&out.join("witness.txt"))?;
        }
    }
    Ok(Solved { arc, concrete: found, arc_marks, known })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::arc_edges::AxisXfer;
    use crate::search::arcs::{pieces, point, Action, Seg, Set, CIRCLE};

    /// An action on one point of the circle.
    fn apply(a: Action, p: u32) -> u32 {
        match a {
            Action::Rotate(v) => (p as i64 + v as i64).rem_euclid(CIRCLE as i64) as u32,
            Action::Const(c) => c,
        }
    }

    /// Node `k`'s state id (increasing in `k`, across regions and entries).
    fn sid(k: u32) -> StateId {
        state_id(1 + (k >> 6), (k >> 3) & 7, k & 7)
    }

    fn axis(t: Transfer) -> AxisXfer {
        match t.action {
            Action::Rotate(v) => AxisXfer { lo: t.guard.lo, hi: t.guard.hi, tag: 0, val: v },
            Action::Const(c) => AxisXfer { lo: t.guard.lo, hi: t.guard.hi, tag: 1, val: c as i32 },
        }
    }

    /// A graph over nodes `0..` with `layers` (node 0 the start, layer 0)
    /// as a tree's edge files: an edge `(src, dst, x, y)` recorded at frame
    /// `layers[src] + 1` (so `layers[dst] <= layers[src] + 1`); the wins'
    /// out-edges are dropped, as the forward never expands a win.
    struct TestTree {
        dir: std::path::PathBuf,
        layers: Vec<u32>,
        wins: Vec<u32>,
    }

    impl TestTree {
        fn new(name: &str, layers: Vec<u32>, edges: &[(u32, u32, Transfer, Transfer)], wins: Vec<u32>) -> Self {
            use std::sync::atomic::{AtomicU32, Ordering};
            static SEQ: AtomicU32 = AtomicU32::new(0);
            let dir = std::env::temp_dir().join(format!("celeste-arcdp-{name}-{}-{}", std::process::id(), SEQ.fetch_add(1, Ordering::Relaxed)));
            let _ = std::fs::remove_dir_all(&dir);
            let mut pairs: Vec<crate::search::arc_edges::Pair> = edges.iter().map(|e| (axis(e.2), axis(e.3))).collect();
            pairs.sort_unstable();
            pairs.dedup();
            let mut frames: std::collections::BTreeMap<u32, Vec<(StateId, StateId, u32)>> = Default::default();
            for &(a, b, x, y) in edges {
                if wins.contains(&a) {
                    continue;
                }
                assert!(layers[b as usize] <= layers[a as usize] + 1, "an edge into a later layer");
                let k = pairs.binary_search(&(axis(x), axis(y))).expect("listed") as u32;
                frames.entry(layers[a as usize] + 1).or_default().push((sid(a), sid(b), k));
            }
            let frames: Vec<(u32, Vec<(StateId, StateId, u32)>)> = frames.into_iter().collect();
            crate::storage::edges::write_test_edges(&dir, &frames, &pairs).expect("the test tree");
            TestTree { dir, layers, wins }
        }

        /// The store up to `horizon`, its transfers, and its win seeds.
        fn open(&self, horizon: u32) -> (EdgeStore, Vec<(Transfer, Transfer)>, Vec<(StateId, u32)>) {
            let store = EdgeStore::open(&self.dir, horizon).expect("open");
            let pairs = store.pairs().iter().map(|p| (p.0.transfer(), p.1.transfer())).collect();
            let wins = self.wins.iter().map(|&k| (sid(k), self.layers[k as usize])).collect();
            (store, pairs, wins)
        }

        fn win_ids(&self) -> Vec<StateId> {
            let mut v: Vec<StateId> = self.wins.iter().map(|&k| sid(k)).collect();
            v.sort_unstable();
            v
        }
    }

    impl Drop for TestTree {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.dir);
        }
    }

    /// A random transfer on one axis.
    fn random_transfer(r: &mut dyn FnMut(u32) -> u32) -> Transfer {
        let cut = 1 + r(CIRCLE - 1);
        let guard = match r(3) {
            0 => Seg { lo: 0, hi: CIRCLE },
            1 => Seg { lo: 0, hi: cut },
            _ => Seg { lo: cut, hi: CIRCLE },
        };
        let action = if r(5) == 0 { Action::Const(r(CIRCLE)) } else { Action::Rotate(r(CIRCLE) as i32 - 32768) };
        Transfer { guard, action }
    }

    /// A random graph from node 0: `n` nodes, `3n` edges, layers the BFS
    /// distances (unreachable nodes' edges dropped), wins a third of the
    /// others reached. `y_too`: random y transfers (else the identity).
    fn random_graph(r: &mut dyn FnMut(u32) -> u32, n: u32, y_too: bool) -> (Vec<u32>, Vec<(u32, u32, Transfer, Transfer)>, Vec<u32>) {
        let still = Transfer { guard: Seg { lo: 0, hi: CIRCLE }, action: Action::Rotate(0) };
        let mut edges = Vec::new();
        for _ in 0..n * 3 {
            let (a, b) = (r(n), r(n));
            let x = random_transfer(r);
            let y = if y_too { random_transfer(r) } else { still };
            edges.push((a, b, x, y));
        }
        let mut dist = vec![u32::MAX; n as usize];
        dist[0] = 0;
        for _ in 0..n {
            for e in &edges {
                if dist[e.0 as usize] != u32::MAX && dist[e.0 as usize] + 1 < dist[e.1 as usize] {
                    dist[e.1 as usize] = dist[e.0 as usize] + 1;
                }
            }
        }
        edges.retain(|e| dist[e.0 as usize] != u32::MAX);
        let wins: Vec<u32> = (1..n).filter(|&i| dist[i as usize] != u32::MAX && r(3) == 0).collect();
        (dist, edges, wins)
    }

    /// `W` by its definition, every node at every frame: a win's the torus
    /// from its layer on, any other's the union of its out-edges' pulls of
    /// the next frame's (from its layer on), by node and frame.
    fn definition(layers: &[u32], edges: &[(u32, u32, Transfer, Transfer)], wins: &[u32], horizon: u32) -> Vec<Vec<Option<Region>>> {
        let n = layers.len();
        let mut want: Vec<Vec<Option<Region>>> = vec![vec![None; n]; horizon as usize + 1];
        for t in (0..=horizon).rev() {
            for i in 0..n as u32 {
                if layers[i as usize] == u32::MAX || layers[i as usize] > t {
                    continue;
                }
                want[t as usize][i as usize] = if wins.contains(&i) {
                    Some(Region::full())
                } else if t < horizon {
                    let mut pieces = Vec::new();
                    for e in edges.iter().filter(|e| e.0 == i) {
                        if let Some(r) = &want[t as usize + 1][e.1 as usize] {
                            r.pull(e.2, e.3, &mut pieces);
                        }
                    }
                    Some(Region::from_pieces(&mut pieces, &mut Vec::new())).filter(|r| !r.is_empty())
                } else {
                    None
                };
            }
        }
        want
    }

    /// A one-axis corridor: from node 0, speed 0.6 px a frame; the remainder
    /// below the cut stays on the pixel (node 0 again), above it moves on
    /// (node 1, which wins). The start's remainder decides when.
    #[test]
    fn the_start_remainder_decides_the_frame() {
        let v = 39322; // 0.6 px
        let p = pieces(v);
        let both = |g: &Set| Transfer { guard: g.0[0], action: Action::Rotate(v) };
        let still = Transfer { guard: Seg { lo: 0, hi: 1 << 16 }, action: Action::Rotate(0) };
        let tree = TestTree::new("corridor", vec![0, 1], &[(0, 0, both(&p[0]), still), (0, 1, both(&p[1]), still)], vec![1]);
        let opt = |h: u32, rem: i32| {
            let (store, pairs, wins) = tree.open(h);
            let w = backward(&store, &pairs, &wins, h, 2);
            optimum(&w, w.index(sid(0)), (point(rem), point(0)), h)
        };
        // rem -0.1 + 0.6 crosses at once: a win at frame 1.
        assert_eq!(opt(3, -6553), Some(1));
        // rem -0.5: -0.5 + 0.6 = 0.1 stays, 0.7 crosses at frame 2.
        assert_eq!(opt(3, -32768), Some(2));
        // Horizon 1 refutes it from -0.5, not from -0.1.
        assert_eq!(opt(1, -32768), None);
        assert_eq!(opt(1, -6553), Some(1));
    }

    /// The push backward over the recorded in-edges equals the definition
    /// (every node recomputed at every frame) on random graphs, and its
    /// support is exactly the nodes with a set somewhere.
    #[test]
    fn the_push_backward_is_the_definition() {
        let mut s = 11u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
        let mut sets = 0;
        for _ in 0..60 {
            let n = 4 + r(20);
            let (layers, edges, wins) = random_graph(&mut r, n, true);
            let horizon = 4 + r(6);
            let want = definition(&layers, &edges, &wins, horizon);
            let tree = TestTree::new("def", layers.clone(), &edges, wins.clone());
            let (store, pairs, seeds) = tree.open(horizon);
            let w = backward(&store, &pairs, &seeds, horizon, 2);
            for i in 0..n {
                let node = w.index(sid(i));
                assert_eq!(node.is_some(), (0..=horizon).any(|t| want[t as usize][i as usize].is_some()), "node {i}: in the support iff it has a set");
                for t in 0..=horizon {
                    let got = node.and_then(|k| w.at(t, k));
                    assert_eq!(got, want[t as usize][i as usize].as_ref(), "t {t} node {i}");
                    sets += got.is_some() as usize;
                }
            }
        }
        assert!(sets > 200, "only {sets} non-empty sets over the cases");
    }

    /// `reach` against the points themselves: from one start point every
    /// frame's reached set is finite, so walk every (node, point) through the
    /// edges whose guards take it, keeping those inside W.
    #[test]
    fn reach_is_the_forward_of_the_start_point_inside_w() {
        let mut s = 23u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
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
            let w = backward(&store, &pairs, &seeds, horizon, 2);
            let g = OutGraph::load(&store, &w, &tree.win_ids(), horizon, 2).expect("the out-edges");
            let start = w.index(sid(0));
            let p0 = (r(CIRCLE), r(CIRCLE));
            let got = reach(&w, &g, &pairs, start, p0, horizon, 2);
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
                            let q = (apply(e.2.action, p.0), apply(e.3.action, p.1));
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

    /// With first-reach layers, the optimum read off `backward(H)` is the
    /// smallest `h` whose own backward holds the start.
    #[test]
    fn one_backward_gives_the_optimum() {
        let mut s = 5u64;
        let mut r = move |m: u32| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        };
        let mut checked = 0;
        for _ in 0..80 {
            let n = 3 + r(12);
            let (layers, edges, wins) = random_graph(&mut r, n, false);
            if edges.is_empty() || wins.is_empty() {
                continue;
            }
            let horizon = 8;
            let tree = TestTree::new("opt", layers, &edges, wins);
            let ws: Vec<Winning> = (0..=horizon)
                .map(|h| {
                    let (store, pairs, seeds) = tree.open(h);
                    backward(&store, &pairs, &seeds, h, 2)
                })
                .collect();
            let w = &ws[horizon as usize];
            for _ in 0..20 {
                let p = (r(CIRCLE), point(0));
                let got = optimum(w, w.index(sid(0)), p, horizon);
                let want = (0..=horizon).find(|&h| ws[h as usize].index(sid(0)).and_then(|k| ws[h as usize].at(0, k)).is_some_and(|r| r.contains(p.0, p.1)));
                assert_eq!(got, want, "start remainder {p:?}");
                checked += got.is_some() as usize;
            }
        }
        assert!(checked > 50, "only {checked} winning starts exercised");
    }
}
