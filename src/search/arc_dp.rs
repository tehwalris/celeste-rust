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
//! (`reach`, over the reached nodes' out-edges, `ReachEdges`), and the
//! concrete search runs inside `W` (`concrete_search`).

use super::arcs::{Region, RegionArena, RegionRef, Transfer};
use crate::storage::edges::{get_varint, put_varint, EdgeStore, ReadStats};
use crate::storage::{id_entry, id_region, state_id, StateId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Nodes a block of `Winning` holds: a node is found by decoding at most
/// this many.
const BLOCK: usize = 16;

/// The winning sets, `t` in `0..=horizon`, of the nodes that have one (W's
/// SUPPORT, sorted by id). A node's set is non-empty from its layer to its
/// last frame and only grows as `t` falls, so it is a few SPANS - a set and
/// the frames it holds over - and is stored as varints: per node its id (a
/// delta), its layer, its spans' count, and per span its length and its set
/// (the arena's sets numbered by use, the most used first); per block of
/// `BLOCK` nodes its first id and byte offset. ~4 B a node and ~3 a span
/// (a span as fields took 8, its node's id and offset 12 more), in a scratch
/// file mapped back when the backward has a directory for it (`Spill`).
pub struct Winning {
    n: usize,
    blocks: Vec<(StateId, u64)>,
    data: Bytes,
    sets: Sets,
    spans: usize,
}

/// Bytes appended in order, then read back: in memory, or in a scratch file
/// mapped once written (`Spill::file`: page cache, which the kernel can
/// reclaim, not anonymous memory; the file is unlinked once mapped).
enum Spill {
    Mem(Vec<u8>),
    File { out: std::io::BufWriter<std::fs::File>, path: std::path::PathBuf, len: u64 },
}

impl Spill {
    fn file(path: std::path::PathBuf) -> anyhow::Result<Self> {
        let f = std::fs::File::create(&path).map_err(|e| anyhow::anyhow!("{}: {e}", path.display()))?;
        Ok(Spill::File { out: std::io::BufWriter::with_capacity(1 << 20, f), path, len: 0 })
    }
    fn len(&self) -> u64 {
        match self {
            Spill::Mem(v) => v.len() as u64,
            Spill::File { len, .. } => *len,
        }
    }
    fn put(&mut self, b: &[u8]) {
        match self {
            Spill::Mem(v) => v.extend_from_slice(b),
            Spill::File { out, path, len } => {
                use std::io::Write;
                out.write_all(b).unwrap_or_else(|e| panic!("FATAL: writing the arc phase's scratch file {}: {e}", path.display()));
                *len += b.len() as u64;
            }
        }
    }
    fn done(self) -> anyhow::Result<Bytes> {
        match self {
            Spill::Mem(v) => Ok(Bytes::Mem(v)),
            Spill::File { out, path, .. } => {
                out.into_inner().map_err(|e| anyhow::anyhow!("{}: {e}", path.display()))?.sync_data()?;
                let f = std::fs::File::open(&path)?;
                // SAFETY: the file is this process's own, complete, and
                // unlinked right after: nothing else writes it.
                let m = unsafe { memmap2::Mmap::map(&f)? };
                std::fs::remove_file(&path)?;
                Ok(Bytes::Map(m))
            }
        }
    }
}

/// `Spill::done`'s bytes.
enum Bytes {
    Mem(Vec<u8>),
    Map(memmap2::Mmap),
}

impl std::ops::Deref for Bytes {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        match self {
            Bytes::Mem(v) => v,
            Bytes::Map(m) => m,
        }
    }
}

/// W's distinct sets: the backward's arena, numbered by use (`order[k]`:
/// the arena's index of set `k`, the most used first - smaller varints)
/// without copying the arena.
struct Sets {
    arena: RegionArena,
    order: Vec<u32>,
}

impl Sets {
    fn get(&self, k: u32) -> RegionRef<'_> {
        self.arena.get(self.order[k as usize])
    }
    fn len(&self) -> usize {
        self.order.len()
    }
    fn bytes(&self) -> usize {
        self.arena.bytes() + 4 * self.order.capacity()
    }
}

/// One node of `Winning`: its id, its layer and its spans.
#[derive(Clone, Copy)]
pub struct Node<'a> {
    pub id: StateId,
    pub layer: u32,
    count: u32,
    bytes: &'a [u8],
    sets: &'a Sets,
}

impl<'a> Node<'a> {
    /// The spans `(lo, hi, set index)`, in frame order.
    fn raw(self) -> impl Iterator<Item = (u32, u32, u32)> + 'a {
        let mut pos = 0usize;
        let mut lo = self.layer;
        (0..self.count).map(move |_| {
            let hi = lo + get_varint(self.bytes, &mut pos) as u32;
            let set = get_varint(self.bytes, &mut pos) as u32;
            let s = (lo, hi, set);
            lo = hi + 1;
            s
        })
    }
    /// The spans `(lo, hi, set)`: `W_t = set` for `t` in `lo..=hi`, empty
    /// at the frames no span covers.
    pub fn spans(self) -> impl Iterator<Item = (u32, u32, RegionRef<'a>)> + 'a {
        let sets = self.sets;
        self.raw().map(move |(lo, hi, s)| (lo, hi, sets.get(s)))
    }
    /// The last frame the node has a set (its deadline).
    pub fn last(self) -> u32 {
        self.raw().last().expect("a node has a span").1
    }
    /// `W_t` (`None` = empty).
    pub fn at(self, t: u32) -> Option<RegionRef<'a>> {
        if t < self.layer {
            return None;
        }
        let sets = self.sets;
        self.raw().find(|s| t <= s.1).map(|s| sets.get(s.2))
    }
}

/// `Winning::nodes`: every node, decoded in order.
pub struct Nodes<'a> {
    w: &'a Winning,
    k: usize,
    end: usize,
    pos: usize,
    id: StateId,
}

impl<'a> Iterator for Nodes<'a> {
    type Item = Node<'a>;
    fn next(&mut self) -> Option<Node<'a>> {
        if self.k >= self.end {
            return None;
        }
        if self.k % BLOCK == 0 {
            (self.id, self.pos) = (self.w.blocks[self.k / BLOCK].0, self.w.blocks[self.k / BLOCK].1 as usize);
        }
        let n = self.w.decode_at(&mut self.pos, &mut self.id);
        self.k += 1;
        Some(n)
    }
}

impl Winning {
    /// The node at `pos` (whose previous node's id is `id`); both past it.
    fn decode_at(&self, pos: &mut usize, id: &mut StateId) -> Node<'_> {
        *id += get_varint(&self.data, pos);
        let layer = get_varint(&self.data, pos) as u32;
        let count = get_varint(&self.data, pos) as u32;
        let start = *pos;
        for _ in 0..2 * count {
            get_varint(&self.data, pos);
        }
        Node { id: *id, layer, count, bytes: &self.data[start..*pos], sets: &self.sets }
    }
    /// Node `i`.
    pub fn node(&self, i: u32) -> Node<'_> {
        let (b, k) = (i as usize / BLOCK, i as usize % BLOCK);
        let (mut id, mut pos) = (self.blocks[b].0, self.blocks[b].1 as usize);
        for _ in 0..k {
            self.decode_at(&mut pos, &mut id);
        }
        self.decode_at(&mut pos, &mut id)
    }
    /// Every node, in id order.
    pub fn nodes(&self) -> Nodes<'_> {
        self.nodes_from(0, self.n)
    }
    /// Nodes `lo..hi` in id order (`lo` a multiple of `BLOCK`: a block's
    /// first node).
    fn nodes_from(&self, lo: usize, hi: usize) -> Nodes<'_> {
        assert!(lo % BLOCK == 0, "nodes from a block's first");
        Nodes { w: self, k: lo, end: hi.min(self.n), pos: 0, id: 0 }
    }
    pub fn len(&self) -> usize {
        self.n
    }
    pub fn is_empty(&self) -> bool {
        self.n == 0
    }
    /// The node of state `id` (`None`: no set at any frame).
    pub fn index(&self, id: StateId) -> Option<u32> {
        let b = self.blocks.partition_point(|x| x.0 <= id).checked_sub(1)?;
        let (mut at, mut pos) = (self.blocks[b].0, self.blocks[b].1 as usize);
        for k in 0..BLOCK.min(self.n - b * BLOCK) {
            let n = self.decode_at(&mut pos, &mut at);
            if n.id >= id {
                return (n.id == id).then_some((b * BLOCK + k) as u32);
            }
        }
        None
    }
    pub fn id(&self, i: u32) -> StateId {
        self.node(i).id
    }
    /// `W_t(i)` (`None` = empty).
    pub fn at(&self, t: u32, i: u32) -> Option<RegionRef<'_>> {
        self.node(i).at(t)
    }
    /// The bytes held: the encoding, the block index and the sets' arena.
    pub fn bytes(&self) -> u64 {
        (self.data.len() + 16 * self.blocks.len() + self.sets.bytes()) as u64
    }
}

/// The spans one backward step closed - each node whose set changed or
/// ended below the step's frame, its span from `lo` (the step's frame + 1)
/// to its last frame - by id, as varints (the id's delta, `hi - lo`, the
/// set) at `at..at + len` of the backward's spilled runs.
#[derive(Clone, Copy)]
struct Run {
    lo: u16,
    n: usize,
    at: u64,
    len: u64,
}

impl Run {
    /// Encode `closed` (by id) into `out`.
    fn new(lo: u16, closed: impl IntoIterator<Item = (StateId, u16, u32)>, out: &mut Spill) -> Self {
        let mut bytes = Vec::new();
        let (mut last, mut n) = (0, 0);
        for (id, hi, set) in closed {
            debug_assert!(id >= last && hi >= lo);
            put_varint(&mut bytes, id - last);
            put_varint(&mut bytes, (hi - lo) as u64);
            put_varint(&mut bytes, set as u64);
            last = id;
            n += 1;
        }
        let at = out.len();
        out.put(&bytes);
        Run { lo, n, at, len: bytes.len() as u64 }
    }
    /// `(id, hi, set)` in id order, from the spilled runs `all`.
    fn iter<'b>(&self, all: &'b [u8]) -> impl Iterator<Item = (StateId, u16, u32)> + 'b {
        let b = &all[self.at as usize..(self.at + self.len) as usize];
        let lo = self.lo;
        let (mut pos, mut id) = (0usize, 0u64);
        (0..self.n).map(move |_| {
            id += get_varint(b, &mut pos);
            let hi = lo + get_varint(b, &mut pos) as u16;
            (id, hi, get_varint(b, &mut pos) as u32)
        })
    }
}

/// A node with a set at the frame done last: the set, the last frame of its
/// current span, its layer (with `WIN`: a win node).
#[derive(Clone, Copy, Debug)]
struct Cur {
    id: StateId,
    set: u32,
    hi: u16,
    layer: u16,
}

/// `Cur::layer`'s flag of a win node.
const WIN: u16 = 1 << 15;

impl Cur {
    fn layer(&self) -> u16 {
        self.layer & !WIN
    }
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

/// A source's set so far at a step (its set at the next frame united with
/// the pulls read so far): a set code (`LOCAL`: the step's own) and its
/// layer.
#[derive(Clone, Copy, Debug)]
struct Acc {
    p: StateId,
    set: u32,
    layer: u16,
}

/// Set codes during a step: below `MADE`, the arena's; with `LOCAL`, the
/// step's own sets (made this step, not in the arena); with `MADE`, the
/// sets a range of the current pass made, numbered after it.
const LOCAL: u32 = 1 << 31;
const MADE: u32 = 1 << 30;

/// The in-edges a step reads at once: a step whose changed nodes have more
/// reads them in ROUNDS, each folded into the step's sets so far (24 B an
/// in-edge: 0.8 GB a round).
const ROUND_EMITS: usize = 1 << 25;

fn region_hash(r: RegionRef) -> u64 {
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

/// Id ranges `[bounds[k], bounds[k + 1])` cutting `total` ids into about
/// `parts` pieces of equal weight, from a `sample` of them (one range when
/// they are few).
fn id_ranges(mut sample: Vec<StateId>, total: usize, parts: usize) -> Vec<StateId> {
    sample.sort_unstable();
    let n = if total < 1 << 16 || sample.is_empty() { 1 } else { parts };
    let mut bounds: Vec<StateId> = vec![0];
    bounds.extend((1..n).map(|k| sample[k * sample.len() / n]));
    bounds.push(StateId::MAX);
    bounds.dedup();
    bounds
}

/// BACKWARD: `W_t` for `t = horizon` down to 0 (`Backward::step`, a frame at
/// a time), from the win states `wins` (id, layer); with `spill`, the
/// closed spans and W's nodes go to scratch files there (`Spill`).
pub fn backward(store: &EdgeStore, pairs: &[(Transfer, Transfer)], wins: &[(StateId, u32)], horizon: u32, threads: usize, spill: Option<&std::path::Path>) -> anyhow::Result<Winning> {
    let mut b = Backward::new(store, pairs, wins, horizon, threads);
    if let Some(dir) = spill {
        b = b.spill_to(dir)?;
    }
    for t in (0..horizon).rev() {
        b.step(t);
    }
    b.finish()
}

/// The backward between two frames: `W_{t+1}` (the frame done last), which
/// of its sets changed there, and the spans closed so far. `rewrite
/// bench-backward` times one `step`.
pub struct Backward<'a> {
    store: &'a EdgeStore,
    pairs: &'a [(Transfer, Transfer)],
    horizon: u32,
    threads: usize,
    /// The ARENA of sets, equal sets once (found by hash; a collision with a
    /// different set is stored apart). Index 0: the whole torus.
    sets: RegionArena,
    interned: FxHashMap<u64, u32>,
    /// The nodes with a set at the frame done last, by id.
    cur: Vec<Cur>,
    /// Their indices whose set changed there (or appeared).
    changed: Vec<u32>,
    /// Per step, the spans it closed, in `run_bytes`; with `spill`, that
    /// and W's nodes are scratch files there.
    runs: Vec<Run>,
    run_bytes: Spill,
    spill: Option<std::path::PathBuf>,
    /// Per target region, the steps that read its in-edges.
    opens: FxHashMap<u32, u32>,
    /// A round's in-edges (`ROUND_EMITS`) and a read's targets (tests
    /// shrink both to exercise the rounds).
    round_emits: usize,
    chunk: usize,
    read: ReadStats,
    n_changed: u64,
    n_emits: u64,
    max_round: usize,
    rounds: u64,
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
        assert!(horizon < WIN as u32, "a horizon past {WIN}");
        let mut sets = RegionArena::default();
        let full = Region::full();
        sets.push(full.as_ref());
        let mut interned: FxHashMap<u64, u32> = FxHashMap::default();
        interned.insert(region_hash(full.as_ref()), 0);
        let mut cur: Vec<Cur> = wins.iter().filter(|w| w.1 <= horizon).map(|&(id, layer)| Cur { id, set: 0, hi: horizon as u16, layer: layer as u16 | WIN }).collect();
        cur.sort_unstable_by_key(|c| c.id);
        cur.dedup_by_key(|c| c.id);
        let changed: Vec<u32> = (0..cur.len() as u32).collect();
        Backward {
            store,
            pairs,
            horizon,
            threads: threads.max(1),
            sets,
            interned,
            max_cur: cur.len(),
            cur,
            changed,
            runs: Vec::new(),
            run_bytes: Spill::Mem(Vec::new()),
            spill: None,
            opens: FxHashMap::default(),
            round_emits: ROUND_EMITS,
            chunk: 1024,
            read: ReadStats::default(),
            n_changed: 0,
            n_emits: 0,
            max_round: 0,
            rounds: 0,
            d_read: std::time::Duration::ZERO,
            d_pull: std::time::Duration::ZERO,
            d_merge: std::time::Duration::ZERO,
            fragments: Vec::new(),
        }
    }

    /// The closed spans and W's nodes in scratch files in `dir` (created).
    pub fn spill_to(mut self, dir: &std::path::Path) -> anyhow::Result<Self> {
        std::fs::create_dir_all(dir)?;
        self.run_bytes = Spill::file(dir.join("runs.bin"))?;
        self.spill = Some(dir.to_path_buf());
        Ok(self)
    }

    /// Read in rounds of `emits` in-edges, `chunk` targets at a time.
    #[cfg(test)]
    fn with_rounds(mut self, emits: usize, chunk: usize) -> Self {
        (self.round_emits, self.chunk) = (emits, chunk);
        self
    }

    /// The frame done last's nodes with a set, and each one's set.
    pub fn current(&self) -> impl Iterator<Item = (StateId, RegionRef<'_>)> + '_ {
        self.cur.iter().map(|c| (c.id, self.sets.get(c.set)))
    }

    /// The read, pull and merge walls so far.
    pub fn walls(&self) -> [std::time::Duration; 3] {
        [self.d_read, self.d_pull, self.d_merge]
    }

    /// The work of a step's reads: the changed nodes (in id order) cut into
    /// chunks of `chunk` targets, each chunk read frame by frame from the
    /// least layer among its targets (at least 1) to `t + 1` - one ITEM a
    /// (chunk, frame), numbered chunk-major: per chunk its first item and
    /// first frame.
    fn read_items(&self, t: u32) -> Vec<(usize, u32)> {
        let mut items = Vec::with_capacity(self.changed.len().div_ceil(self.chunk) + 1);
        let mut at = 0usize;
        for chunk in self.changed.chunks(self.chunk) {
            let first = chunk.iter().map(|&c| self.cur[c as usize].layer() as u32).min().unwrap_or(0).max(1);
            items.push((at, first));
            at += (t + 2).saturating_sub(first) as usize;
        }
        items.push((at, 0));
        items
    }

    /// One ROUND of a step's reads: from item `start` on (`read_items`), on
    /// `threads` workers, until the round holds `round_emits` in-edges -
    /// the in-edges recorded at an item's frame into its chunk's targets of
    /// layer <= the frame (frames <= t + 1 at step t: their sources have
    /// layers <= t).
    /// Per worker its in-edges sorted and distinct; and where the round
    /// stopped. A round stops between items, so one heavy target bounds it
    /// by its in-edges at one frame.
    fn read_round(&self, items: &[(usize, u32)], start: usize) -> (Vec<(Vec<Emit>, ReadStats)>, usize) {
        use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
        let (cur, changed, store, chunk_len, budget) = (&self.cur, &self.changed, self.store, self.chunk, self.round_emits);
        let total = items.last().map_or(0, |x| x.0);
        let (next, held) = (AtomicUsize::new(start), AtomicUsize::new(0));
        let parts: Vec<(Vec<Emit>, ReadStats)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..self.threads)
                .map(|_| {
                    sc.spawn(|| {
                        let (mut emits, mut stats) = (Vec::new(), ReadStats::default());
                        let (mut targets, mut sets): (Vec<StateId>, Vec<u32>) = (Vec::new(), Vec::new());
                        while held.load(Relaxed) < budget {
                            let k = next.fetch_add(1, Relaxed);
                            if k >= total {
                                break;
                            }
                            let c = items.partition_point(|x| x.0 <= k) - 1;
                            let f = items[c].1 + (k - items[c].0) as u32;
                            let chunk = &changed[c * chunk_len..((c + 1) * chunk_len).min(changed.len())];
                            let before = emits.len();
                            targets.clear();
                            sets.clear();
                            for &n in chunk {
                                let n = &cur[n as usize];
                                if n.layer() as u32 <= f {
                                    targets.push(n.id);
                                    sets.push(n.set);
                                }
                            }
                            store.preds_sorted(f, &targets, &mut stats, |k, e| emits.push(Emit { p: e.src, set: sets[k], xfer: e.xfer, layer: (f - 1) as u16 }));
                            held.fetch_add(emits.len() - before, Relaxed);
                        }
                        emits.sort_unstable();
                        emits.dedup();
                        emits.shrink_to_fit();
                        (emits, stats)
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a backward reader panicked")).collect()
        });
        (parts, next.into_inner().min(total))
    }

    /// A set code's set (`local`: the step's own sets).
    fn set_of<'s>(&'s self, code: u32, local: &'s [Region]) -> RegionRef<'s> {
        if code & LOCAL != 0 {
            local[(code & !LOCAL) as usize].as_ref()
        } else {
            self.sets.get(code)
        }
    }

    /// The arena's code of `r`, if it holds it.
    fn arena_code(&self, r: &Region, h: u64) -> Option<u32> {
        self.interned.get(&h).copied().filter(|&s| self.sets.get(s) == *r)
    }

    /// Fold one round's in-edges into the step's sets so far (`acc`, by
    /// id): per source in parallel id ranges, its set so far (or at `t + 1`)
    /// united with the round's pulls. New sets go into `local`.
    fn fold_round(&self, acc: Vec<Acc>, parts: &[(Vec<Emit>, ReadStats)], local: &mut Vec<Region>, local_interned: &mut FxHashMap<u64, u32>) -> Vec<Acc> {
        let n_emits: usize = parts.iter().map(|p| p.0.len()).sum();
        let bounds = id_ranges(parts.iter().flat_map(|p| p.0.iter().step_by(64).map(|e| e.p)).collect(), n_emits, self.threads * 4);
        let (cur, pairs, local_ro) = (&self.cur, self.pairs, &*local);
        let outs: Vec<(Vec<Acc>, Vec<Region>)> = par_chunks(bounds.len() - 1, 1, self.threads, |k, _| {
            let (lo, hi) = (bounds[k], bounds[k + 1]);
            let a = &acc[acc.partition_point(|x| x.p < lo)..acc.partition_point(|x| x.p < hi)];
            let c = &cur[cur.partition_point(|x| x.id < lo)..cur.partition_point(|x| x.id < hi)];
            let mut es: Vec<Emit> = Vec::new();
            for p in parts {
                es.extend_from_slice(&p.0[p.0.partition_point(|e| e.p < lo)..p.0.partition_point(|e| e.p < hi)]);
            }
            es.sort_unstable();
            es.dedup();
            let (mut out, mut made) = (Vec::with_capacity(a.len() + es.len() / 4), Vec::new());
            let (mut pieces, mut scratch) = (Vec::new(), Vec::new());
            let (mut i, mut j, mut ci) = (0usize, 0usize, 0usize);
            while i < a.len() || j < es.len() {
                let ia = a.get(i).map_or(StateId::MAX, |x| x.p);
                let ie = es.get(j).map_or(StateId::MAX, |e| e.p);
                if ia < ie {
                    out.push(a[i]);
                    i += 1;
                    continue;
                }
                let j_end = j + es[j..].partition_point(|e| e.p == ie);
                let layer = es[j].layer;
                assert!(es[j..j_end].iter().all(|e| e.layer == layer), "state {} has in-edges recorded at two frames", crate::storage::show_id(ie));
                // The base: the set so far, else the set at t + 1.
                let base: Option<u32> = if ia == ie {
                    i += 1;
                    assert_eq!(a[i - 1].layer, layer, "state {}: its layer from the edges disagrees", crate::storage::show_id(ie));
                    Some(a[i - 1].set)
                } else {
                    ci += c[ci..].partition_point(|x| x.id < ie);
                    c.get(ci).filter(|x| x.id == ie).map(|x| {
                        assert_eq!(x.layer(), layer, "state {}: its layer from the edges disagrees", crate::storage::show_id(ie));
                        x.set
                    })
                };
                pieces.clear();
                if let Some(b) = base {
                    self.set_of(b, local_ro).pieces(&mut pieces);
                }
                for e in &es[j..j_end] {
                    let (x, y) = pairs[e.xfer as usize];
                    self.sets.get(e.set).pull(x, y, &mut pieces);
                }
                j = j_end;
                if pieces.is_empty() {
                    continue;
                }
                let r = Region::from_pieces(&mut pieces, &mut scratch);
                let set = match base {
                    Some(b) if self.set_of(b, local_ro) == r => b,
                    _ => {
                        let h = region_hash(r.as_ref());
                        match self.arena_code(&r, h).or_else(|| local_interned.get(&h).copied().filter(|&s| local_ro[s as usize] == r).map(|s| s | LOCAL)) {
                            Some(s) => s,
                            None => {
                                made.push(r);
                                MADE | (made.len() as u32 - 1)
                            }
                        }
                    }
                };
                out.push(Acc { p: ie, set, layer });
            }
            (out, made)
        });
        // The old sets so far are in the ranges' outputs now.
        drop(acc);
        // The ranges' new sets become the step's own, once each.
        let mut acc = Vec::with_capacity(outs.iter().map(|o| o.0.len()).sum());
        for (out, made) in outs {
            let codes: Vec<u32> = made
                .into_iter()
                .map(|r| {
                    let h = region_hash(r.as_ref());
                    match local_interned.get(&h) {
                        Some(&s) if local[s as usize] == r => s | LOCAL,
                        found => {
                            let s = local.len() as u32;
                            assert!(s < MADE, "more distinct sets in a step than set codes");
                            if found.is_none() {
                                local_interned.insert(h, s);
                            }
                            local.push(r);
                            s | LOCAL
                        }
                    }
                })
                .collect();
            acc.extend(out.into_iter().map(|mut x| {
                if x.set & MADE != 0 && x.set & LOCAL == 0 {
                    x.set = codes[(x.set & !MADE) as usize];
                }
                x
            }));
        }
        acc
    }

    /// `W_t` from `W_{t+1}` (the frame done last; `t` counts down from
    /// `horizon - 1`): the in-edges of the nodes whose set changed at `t + 1`,
    /// read target by target in id order (`read_round`, in rounds), folded
    /// source by source into their sets at `t + 1` (`fold_round`); then the
    /// sets that grew replace their spans and the nodes of layer `t + 1`
    /// close theirs.
    pub fn step(&mut self, t: u32) {
        assert!(t < self.horizon, "a step below the horizon");
        let mut last_region = u32::MAX;
        for &c in &self.changed {
            let r = id_region(self.cur[c as usize].id);
            if r != last_region {
                *self.opens.entry(r).or_default() += 1;
                last_region = r;
            }
        }
        let n_read = self.changed.len();
        self.n_changed += n_read as u64;
        let (mut acc, mut local, mut local_interned) = (Vec::new(), Vec::new(), FxHashMap::default());
        let items = self.read_items(t);
        let total = items.last().map_or(0, |x| x.0);
        let (mut start, mut n_emits, mut rounds) = (0usize, 0usize, 0u32);
        let (mut d_read, mut d_pull) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
        while start < total {
            let t0 = std::time::Instant::now();
            let (parts, end) = self.read_round(&items, start);
            start = end;
            let round: usize = parts.iter().map(|p| p.0.len()).sum();
            for p in &parts {
                self.read.add(&p.1);
            }
            n_emits += round;
            rounds += 1;
            self.max_round = self.max_round.max(round);
            d_read += t0.elapsed();
            let t0 = std::time::Instant::now();
            acc = self.fold_round(acc, &parts, &mut local, &mut local_interned);
            d_pull += t0.elapsed();
        }
        self.n_emits += n_emits as u64;
        self.rounds += rounds as u64;
        self.d_read += d_read;
        self.d_pull += d_pull;
        // MERGE: per id range in parallel, the step's sets against those at
        // t + 1 - a node whose set grew closes its span and opens one, a
        // node of layer t + 1 closes its span, the others carry on.
        let t0 = std::time::Instant::now();
        let close_at = t as u16 + 1;
        let bounds = id_ranges(acc.iter().step_by(64).map(|x| x.p).chain(self.cur.iter().step_by(64).map(|x| x.id)).collect(), acc.len() + self.cur.len(), self.threads * 4);
        struct RangeOut {
            cur: Vec<Cur>,
            changed: Vec<u32>,
            closed: Vec<(StateId, u16, u32)>,
        }
        let (cur, acc_ref, local_ref) = (&self.cur, &acc, &local);
        let outs: Vec<RangeOut> = par_chunks(bounds.len() - 1, 1, self.threads, |k, _| {
            let (lo, hi) = (bounds[k], bounds[k + 1]);
            let c = &cur[cur.partition_point(|x| x.id < lo)..cur.partition_point(|x| x.id < hi)];
            let a = &acc_ref[acc_ref.partition_point(|x| x.p < lo)..acc_ref.partition_point(|x| x.p < hi)];
            let mut o = RangeOut { cur: Vec::with_capacity(c.len() + a.len() / 8), changed: Vec::new(), closed: Vec::new() };
            let (mut i, mut j) = (0usize, 0usize);
            while i < c.len() || j < a.len() {
                let ic = c.get(i).map_or(StateId::MAX, |x| x.id);
                let ia = a.get(j).map_or(StateId::MAX, |x| x.p);
                if ic < ia {
                    let x = c[i];
                    if x.layer() == close_at {
                        o.closed.push((x.id, x.hi, x.set));
                    } else {
                        o.cur.push(x);
                    }
                    i += 1;
                    continue;
                }
                let n = a[j];
                j += 1;
                let mut win = 0;
                if ic == ia {
                    let x = c[i];
                    i += 1;
                    if n.set == x.set || self.set_of(n.set, local_ref) == self.sets.get(x.set) {
                        o.cur.push(x);
                        continue;
                    }
                    o.closed.push((x.id, x.hi, x.set));
                    win = x.layer & WIN;
                }
                o.changed.push(o.cur.len() as u32);
                o.cur.push(Cur { id: n.p, set: n.set, hi: t as u16, layer: n.layer | win });
            }
            o
        });
        drop(acc);
        // The frame done last is in the ranges' outputs now: dropped before
        // they are joined (two copies of a frame at most, not three).
        self.cur = Vec::new();
        self.changed = Vec::new();
        // In range order: the step's sets that a node keeps go into the
        // arena (once each), the ranges are joined.
        let mut to_arena: Vec<u32> = vec![u32::MAX; local.len()];
        let mut new_cur = Vec::with_capacity(outs.iter().map(|o| o.cur.len()).sum());
        let mut changed = Vec::new();
        let n_closed: usize = outs.iter().map(|o| o.closed.len()).sum();
        let mut closed = Vec::with_capacity(n_closed);
        for o in outs {
            let base = new_cur.len() as u32;
            changed.extend(o.changed.iter().map(|&k| base + k));
            for mut x in o.cur {
                if x.set & LOCAL != 0 {
                    let l = (x.set & !LOCAL) as usize;
                    if to_arena[l] == u32::MAX {
                        let r = std::mem::take(&mut local[l]);
                        let h = region_hash(r.as_ref());
                        to_arena[l] = match self.interned.get(&h) {
                            Some(&s) if self.sets.get(s) == r => s,
                            found => {
                                let s = self.sets.push(r.as_ref());
                                assert!(s < MADE, "more distinct sets than set codes");
                                if found.is_none() {
                                    self.interned.insert(h, s);
                                }
                                s
                            }
                        };
                    }
                    x.set = to_arena[l];
                }
                new_cur.push(x);
            }
            closed.extend(o.closed);
        }
        drop(local);
        let run = Run::new(close_at, closed, &mut self.run_bytes);
        self.runs.push(run);
        self.cur = new_cur;
        self.changed = changed;
        self.max_cur = self.max_cur.max(self.cur.len());
        // The step's workers are gone: their heaps' freed pages go back now
        // (mimalloc reclaims an exited thread's segments lazily otherwise -
        // ~3 GB held on the mod room).
        // SAFETY: mimalloc's documented API, callable from any thread.
        unsafe { libmimalloc_sys::mi_collect(true) };
        let d_merge = t0.elapsed();
        self.d_merge += d_merge;
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        eprintln!(
            "[arc] f{t:03}: {n_read} changed at f{} -> {n_emits} in-edges ({rounds} rounds); {} nodes with a set, {} changed | read {:.0} pull {:.0} merge {:.0} ms | anon {:.2} GB: frame {:.2}, runs {:.2}, {} sets {:.2}",
            t + 1,
            self.cur.len(),
            self.changed.len(),
            ms(d_read),
            ms(d_pull),
            ms(d_merge),
            crate::metrics::current_rss_gb(),
            (self.cur.capacity() * std::mem::size_of::<Cur>() + self.changed.capacity() * 4) as f64 / 1e9,
            self.run_bytes.len() as f64 / 1e9,
            self.sets.len(),
            (self.sets.bytes() + self.interned.capacity() * 16) as f64 / 1e9,
        );
        if (self.horizon - 1 - t) % 4 == 0 {
            let (k, med, p90, max) = fragmentation(self.cur.iter().filter(|c| c.layer & WIN == 0).map(|c| self.sets.get(c.set)));
            self.fragments.push(format!("[arc] W frame {t:3}: {k} non-win nodes can win; rectangles per node med {med} p90 {p90} max {max}"));
        }
    }

    /// The winning sets, once every frame down to 0 is done: the runs merged
    /// node by node (a heap over the runs) into `Winning`'s varints, the
    /// sets renumbered by use.
    pub fn finish(mut self) -> anyhow::Result<Winning> {
        use std::cmp::Reverse;
        assert_eq!(self.runs.len(), self.horizon as usize, "finish before frame 0");
        let last = std::mem::take(&mut self.cur);
        let mut runs = std::mem::take(&mut self.runs);
        let mut run_bytes = std::mem::replace(&mut self.run_bytes, Spill::Mem(Vec::new()));
        runs.push(Run::new(0, last.into_iter().map(|c| (c.id, c.hi, c.set)), &mut run_bytes));
        let all = run_bytes.done()?;
        // The sets by use, the most used first (smaller varints).
        let mut uses = vec![0u64; self.sets.len()];
        for r in &runs {
            for (_, _, s) in r.iter(&all) {
                uses[s as usize] += 1;
            }
        }
        let mut order: Vec<u32> = (0..self.sets.len() as u32).filter(|&s| uses[s as usize] > 0).collect();
        order.sort_by_key(|&s| (Reverse(uses[s as usize]), s));
        let mut renumber = vec![u32::MAX; self.sets.len()];
        for (k, &s) in order.iter().enumerate() {
            renumber[s as usize] = k as u32;
        }
        drop(uses);
        // (No shrink: a Vec's unused capacity is never touched, so never
        // resident; shrinking would copy the arena.)
        let sets = Sets { arena: std::mem::take(&mut self.sets), order };
        crate::metrics::mem_phase("arc: backward swept");
        // The runs merged by (id, frame).
        let spans: usize = runs.iter().map(|r| r.n).sum();
        let mut iters: Vec<_> = runs.iter().map(|r| r.iter(&all)).collect();
        let mut heap: std::collections::BinaryHeap<Reverse<(StateId, u16, u16, u32, usize)>> = std::collections::BinaryHeap::new();
        for (k, it) in iters.iter_mut().enumerate() {
            if let Some((id, hi, s)) = it.next() {
                heap.push(Reverse((id, runs[k].lo, hi, s, k)));
            }
        }
        let mut data = match &self.spill {
            Some(dir) => Spill::file(dir.join("w.bin"))?,
            None => Spill::Mem(Vec::new()),
        };
        let (mut n, mut blocks) = (0usize, Vec::new());
        let mut node: Vec<(u16, u16, u32)> = Vec::new();
        let mut bytes: Vec<u8> = Vec::new();
        let (mut cur_id, mut prev_id) = (None::<StateId>, 0u64);
        let mut flush = |id: StateId, node: &mut Vec<(u16, u16, u32)>, prev_id: &mut u64| {
            if n % BLOCK == 0 {
                blocks.push((id, data.len()));
                *prev_id = id;
            }
            bytes.clear();
            put_varint(&mut bytes, id - *prev_id);
            put_varint(&mut bytes, node[0].0 as u64);
            put_varint(&mut bytes, node.len() as u64);
            for (k, &(lo, hi, s)) in node.iter().enumerate() {
                debug_assert!(k == 0 || node[k - 1].1 + 1 == lo, "a node's spans are contiguous");
                put_varint(&mut bytes, (hi - lo) as u64);
                put_varint(&mut bytes, renumber[s as usize] as u64);
            }
            data.put(&bytes);
            *prev_id = id;
            n += 1;
            node.clear();
        };
        while let Some(Reverse((id, lo, hi, s, k))) = heap.pop() {
            if let Some((nid, nhi, ns)) = iters[k].next() {
                heap.push(Reverse((nid, runs[k].lo, nhi, ns, k)));
            }
            if cur_id != Some(id) {
                if let Some(c) = cur_id {
                    flush(c, &mut node, &mut prev_id);
                }
                cur_id = Some(id);
            }
            node.push((lo, hi, s));
        }
        if let Some(c) = cur_id {
            flush(c, &mut node, &mut prev_id);
        }
        drop(flush);
        drop(iters);
        drop(runs);
        drop(all);
        let w = Winning { n, blocks, data: data.done()?, sets, spans };
        if let Some(dir) = &self.spill {
            // Both files are mapped and unlinked.
            let _ = std::fs::remove_dir(dir);
        }
        let opens: Vec<u32> = self.opens.values().copied().collect();
        let (o_mean, o_max) = (opens.iter().map(|&o| o as f64).sum::<f64>() / opens.len().max(1) as f64, opens.iter().copied().max().unwrap_or(0));
        eprintln!(
            "[arc] backward: {} sets changed, {} in-edges read ({} emissions in {} rounds, at most {} a round), {:.2} GB of the edge store's {:.2} GB read ({} owner entries, {:.2} GB of lids); {} target regions read on {o_mean:.1} steps each (at most {o_max}), {} (region, frame) opens; at most {} nodes a frame; read {:.2} s, pull {:.2} s, merge {:.2} s; {} nodes, {} spans, {} distinct sets: W {:.2} GB (nodes and spans {:.2}, sets {:.2})",
            self.n_changed,
            self.read.edges,
            self.n_emits,
            self.rounds,
            self.max_round,
            self.read.bytes() as f64 / 1e9,
            self.store.bytes as f64 / 1e9,
            self.read.owner_probes,
            self.read.lid_bytes as f64 / 1e9,
            opens.len(),
            self.read.region_opens,
            self.max_cur,
            self.d_read.as_secs_f64(),
            self.d_pull.as_secs_f64(),
            self.d_merge.as_secs_f64(),
            w.len(),
            w.spans,
            w.sets.len(),
            w.bytes() as f64 / 1e9,
            (w.data.len() + 16 * w.blocks.len()) as f64 / 1e9,
            w.sets.bytes() as f64 / 1e9,
        );
        for f in &self.fragments {
            eprintln!("{f}");
        }
        Ok(w)
    }
}

/// The out-edges `reach` follows - between W's nodes, from a non-win
/// source to a target with a set at or past the edge's frame (the others
/// carry nothing) - loaded as `reach` first reaches their source, a frame's
/// sources at a time (`EdgeStore::out_edges`): what is held is the reached
/// nodes' edges, 8 B each, not the graph between W's nodes.
pub struct ReachEdges<'a> {
    store: &'a EdgeStore,
    w: &'a Winning,
    /// The win states, sorted: never expanded, so never left.
    wins: Vec<StateId>,
    threads: usize,
    loaded: FxHashMap<u32, Box<[(u32, u32)]>>,
    edges: usize,
}

impl<'a> ReachEdges<'a> {
    pub fn new(store: &'a EdgeStore, w: &'a Winning, wins: Vec<StateId>, threads: usize) -> Self {
        ReachEdges { store, w, wins, threads, loaded: FxHashMap::default(), edges: 0 }
    }
    /// Load the out-edges of those of `nodes` not loaded yet, frame by frame
    /// (a node's are recorded at its layer + 1).
    fn load(&mut self, nodes: impl Iterator<Item = u32>) {
        let mut todo: Vec<(u32, StateId, u32)> = nodes.filter(|i| !self.loaded.contains_key(i)).map(|i| {
            let n = self.w.node(i);
            (n.layer + 1, n.id, i)
        }).collect();
        todo.sort_unstable();
        todo.dedup();
        let mut a = 0;
        while a < todo.len() {
            let f = todo[a].0;
            let b = a + todo[a..].partition_point(|x| x.0 == f);
            let ids: Vec<StateId> = todo[a..b].iter().map(|x| x.1).collect();
            let mut lists: Vec<Vec<(u32, u32)>> = vec![Vec::new(); ids.len()];
            for (k, dst, x) in self.store.out_edges(f, &ids, self.threads) {
                if self.wins.binary_search(&ids[k]).is_ok() {
                    continue;
                }
                if let Some(m) = self.w.index(dst).filter(|&m| self.w.node(m).last() >= f) {
                    lists[k].push((m, x));
                }
            }
            for (&(_, _, i), mut l) in todo[a..b].iter().zip(lists) {
                l.sort_unstable();
                self.edges += l.len();
                self.loaded.insert(i, l.into_boxed_slice());
            }
            a = b;
        }
    }
    fn of(&self, i: u32) -> &[(u32, u32)] {
        &self.loaded[&i]
    }
    /// The nodes and edges loaded.
    pub fn sizes(&self) -> (usize, usize) {
        (self.loaded.len(), self.edges)
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
pub fn reach(w: &Winning, g: &mut ReachEdges, pairs: &[(Transfer, Transfer)], start: Option<u32>, point: (u32, u32), horizon: u32, threads: usize) -> Vec<u32> {
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
        // Each reached node's set pushed along its out-edges (loaded as the
        // node is first reached) into the targets with a set at t + 1,
        // tagged by target.
        g.load(cur.iter().map(|c| c.0));
        let (cur_ref, g) = (&cur, &*g);
        let mut pushed: Vec<(u32, Piece)> = par_chunks(cur.len(), 64, threads, |lo, hi| {
            let (mut out, mut tmp) = (Vec::new(), Vec::new());
            for (p, r) in &cur_ref[lo..hi] {
                let r: RegionRef = match r {
                    Some(r) => r.as_ref(),
                    None => w.at(t, *p).expect("a node reached in W has a set"),
                };
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
pub fn fragmentation<'a>(sets: impl Iterator<Item = RegionRef<'a>>) -> (usize, usize, usize, usize) {
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
        for id in w.nodes().map(|x| x.id) {
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
pub fn gate_w_hash(node: u64, r: RegionRef) -> u64 {
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
    // The backward's reads are scattered: no read-ahead. Its closed spans
    // and W's nodes go to scratch files beside the tree (mapped, unlinked).
    seeds.store.advise(memmap2::Advice::Random)?;
    let scratch = dir.join("arc-scratch");
    if scratch.exists() {
        std::fs::remove_dir_all(&scratch)?;
    }
    let w = backward(&seeds.store, &seeds.pairs, &seeds.wins, horizon, threads, Some(&scratch))?;
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
    let mut wanted: Vec<(u32, u32)> = Vec::new();
    for x in w.nodes() {
        let e = (id_region(x.id), id_entry(x.id));
        if wanted.last() != Some(&e) {
            wanted.push(e);
        }
    }
    let resolver = crate::storage::marks::Resolver::load_for(dir, horizon, &wanted)?;
    drop(wanted);
    // Fingerprints over CONTENT (`exact::ContentHash`): independent of the
    // order the tree's dictionaries grew in (a raise).
    let content = resolver.space.content_hash();
    let mut files = Vec::new();
    for (f, fs) in tree_frames(dir, horizon)?.into_iter().enumerate() {
        files.extend(fs.into_iter().map(|(seq, file)| (f as u32, seq, file)));
    }
    // Per frame, the nodes with a set and the sum of their (node, set)
    // hashes: each span added at its first frame and taken off past its
    // last (`delta`), then summed frame by frame; W's nodes in parallel
    // block ranges, the sums commutative.
    type Part = (u64, Vec<(i64, u64)>, Vec<MarkRow>);
    let blocks = w.len().div_ceil(BLOCK);
    let parts: Vec<anyhow::Result<Part>> = par_chunks(blocks, 4096, threads, |lo, hi| {
        let (mut fp, mut delta, mut rows) = (0u64, vec![(0i64, 0u64); horizon as usize + 2], Vec::new());
        for x in w.nodes_from(lo * BLOCK, hi * BLOCK) {
            let (shape, key, cell) = resolver.resolve(x.id)?;
            // `Visited::fingerprint`: the nodes are distinct states.
            let h = content.state(shape, key, cell);
            fp = fp.wrapping_add(h);
            for (lo, hi, r) in x.spans() {
                let g = gate_w_hash(h, r);
                delta[lo as usize] = (delta[lo as usize].0 + 1, delta[lo as usize].1.wrapping_add(g));
                delta[hi as usize + 1] = (delta[hi as usize + 1].0 - 1, delta[hi as usize + 1].1.wrapping_sub(g));
            }
            if save.is_some() {
                rows.push(mark_row(shape, key, cell, x.last() as u16, horizon));
            }
        }
        Ok((fp, delta, rows))
    });
    let (mut fp, mut delta, mut rows) = (0u64, vec![(0i64, 0u64); horizon as usize + 2], Vec::<MarkRow>::new());
    for part in parts {
        let (f, d, r) = part?;
        fp = fp.wrapping_add(f);
        for (a, b) in delta.iter_mut().zip(d) {
            *a = (a.0 + b.0, a.1.wrapping_add(b.1));
        }
        rows.extend(r);
    }
    println!("[gate] h{horizon} support {} {fp:016x}", w.len());
    if let Some(out) = save {
        std::fs::create_dir_all(out)?;
        save_marks(&out.join("level0.marks.bin"), std::mem::take(&mut rows), horizon, &resolver.space)?;
    }
    let (mut n, mut acc) = (0i64, 0u64);
    for (t, d) in delta[..=horizon as usize].iter().enumerate() {
        (n, acc) = (n + d.0, acc.wrapping_add(d.1));
        println!("[gate] h{horizon} W f{t:03} {n} {acc:016x}");
    }
    println!("[gate] h{horizon} arc optimum {}", arc_steps.map_or("none".to_string(), |f| f.to_string()));
    crate::metrics::mem_phase("arc: fingerprints");
    // Per node the LAST frame it is reached inside W from the start (`reach`;
    // at most its W deadline): the next level's filter, and the UI's arc marks.
    let mut arc_marks = want_marks.then(|| Visited::new(resolver.space.clone()));
    if want_marks || save.is_some() {
        let tr = std::time::Instant::now();
        let mut g = ReachEdges::new(&seeds.store, &w, seeds.win_ids(), threads);
        let last = reach(&w, &mut g, &seeds.pairs, start, p0, horizon, threads);
        eprintln!("[arc] reach {:.2} s ({} reached nodes' {} out-edges loaded)", tr.elapsed().as_secs_f64(), g.sizes().0, g.sizes().1);
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
            let p = super::known::Pruning { settings: s, level, horizon, node, keys: &resolver.space, start, w: &w, reached: arc_marks.as_ref(), minus_one, files: &files, dir };
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
            let w = backward(&store, &pairs, &wins, h, 2, None).expect("backward");
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
            // Rounds of a few in-edges, a target at a time: the step's
            // sets folded round by round.
            let mut b = Backward::new(&store, &pairs, &seeds, horizon, 2).with_rounds(1 + r(4) as usize, 1);
            for t in (0..horizon).rev() {
                b.step(t);
            }
            let w = b.finish().expect("backward");
            let spilled = tree.dir.join("spill");
            assert_eq!(w.len(), backward(&store, &pairs, &seeds, horizon, 2, Some(&spilled)).expect("backward").len(), "rounds or not, spilled or not, the same support");
            assert!(!spilled.exists(), "the scratch files are gone once mapped");
            for i in 0..n {
                let node = w.index(sid(i));
                assert_eq!(node.is_some(), (0..=horizon).any(|t| want[t as usize][i as usize].is_some()), "node {i}: in the support iff it has a set");
                for t in 0..=horizon {
                    let got = node.and_then(|k| w.at(t, k));
                    assert_eq!(got, want[t as usize][i as usize].as_ref().map(Region::as_ref), "t {t} node {i}");
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
            let w = backward(&store, &pairs, &seeds, horizon, 2, None).expect("backward");
            let mut g = ReachEdges::new(&store, &w, tree.win_ids(), 2);
            let start = w.index(sid(0));
            let p0 = (r(CIRCLE), r(CIRCLE));
            let got = reach(&w, &mut g, &pairs, start, p0, horizon, 2);
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
                    backward(&store, &pairs, &seeds, h, 2, None).expect("backward")
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
