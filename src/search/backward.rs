//! THE BACKWARD (plans/architecture.md "The search", step 2): over the
//! recorded edges (a guard and an action per axis, `search::arcs`), the
//! WINNING SETS `W_t(n)`, the remainders from which node `n` at frame `t`
//! wins by the horizon: `W_t(n) = U_e guard_e ∩ action_e^-1(W_{t+1}(dst_e))`
//! for `t >= layer(n)`, a win node's the whole torus from its layer on.
//! `W_t` only grows as `t` falls (more time never hurts), so ONE PUSH over
//! the in-edges of the nodes that CHANGED at `t + 1` gives `W_t(p) =
//! W_{t+1}(p) ∪ U pull_e(W_{t+1}(m))`: no graph is held, and a node no
//! winning set reaches is never touched. Exact: "intersect with the guard,
//! then act" distributes over unions.

use super::arcs::{Circle, Fixed16, Region, RegionArena, RegionRef};
use super::rem::{Rem, Xfer};
use celeste_core::pico8_num::Pico8Num as P8;
use crate::storage::codec::{get_varint, get_varint128, put_varint, put_varint128, wide};
use crate::storage::edges::EdgeStore;
use crate::storage::{Sk, GEOMETRY};
use rustc_hash::FxHashMap;

const SK_MAX: Sk = Sk { region: u32::MAX, key: (u64::MAX, u64::MAX), local: u8::MAX };

/// State `s` after `prev` in a sorted stream (`prev <= s`): its region's
/// delta, its key (a delta within the region, raw past it), its cell.
#[inline]
fn put_sk(out: &mut Vec<u8>, prev: Sk, s: Sk) {
    debug_assert!(prev <= s);
    let dr = s.region - prev.region;
    put_varint(out, dr as u64);
    put_varint128(out, if dr == 0 { wide(s.key) - wide(prev.key) } else { wide(s.key) });
    out.push(s.local);
}

#[inline]
fn get_sk(b: &[u8], pos: &mut usize, prev: Sk) -> Sk {
    let dr = get_varint(b, pos) as u32;
    let k = get_varint128(b, pos);
    let k = if dr == 0 { wide(prev.key) + k } else { k };
    let local = b[*pos];
    *pos += 1;
    Sk { region: prev.region + dr, key: ((k >> 64) as u64, k as u64), local }
}

/// A backward's reads (`stored`, `raw`: the segments' bytes).
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ReadStats {
    pub(crate) segments: u64,
    pub(crate) stored: u64,
    pub(crate) raw: u64,
    pub(crate) bundles: u64,
    pub(crate) edges: u64,
    pub(crate) region_opens: u64,
}

impl ReadStats {
    pub(crate) fn add(&mut self, o: &ReadStats) {
        self.segments += o.segments;
        self.stored += o.stored;
        self.raw += o.raw;
        self.bundles += o.bundles;
        self.edges += o.edges;
        self.region_opens += o.region_opens;
    }
}

/// Nodes per block of `Winning` (a lookup decodes at most this many).
pub(super) const BLOCK: usize = 16;

/// The winning sets, `t` in `0..=horizon`, of W's SUPPORT, by state. A
/// node's set is non-empty from its layer to its last frame and only grows
/// as `t` falls: a few SPANS, as varints - per node its state (`put_sk`),
/// layer and span count, per span its length and set (numbered by use); per
/// `BLOCK` nodes the first state and byte offset.
pub(crate) struct Winning<C: Circle = Fixed16> {
    n: usize,
    blocks: Vec<(Sk, u64)>,
    data: Bytes,
    sets: Sets<C>,
    spans: usize,
}

/// Bytes appended, then read back: in memory, or a scratch file mapped once
/// written and unlinked (page cache, reclaimable, not anonymous memory).
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

/// W's sets by use (`order[k]`: set `k`'s arena index, the most used first).
struct Sets<C: Circle> {
    arena: RegionArena<C>,
    order: Vec<u32>,
}

impl<C: Circle> Sets<C> {
    fn get(&self, k: u32) -> RegionRef<'_, C> {
        self.arena.get(self.order[k as usize])
    }
    fn len(&self) -> usize {
        self.order.len()
    }
    fn bytes(&self) -> usize {
        self.arena.bytes() + 4 * self.order.capacity()
    }
}

pub(crate) struct Node<'a, C: Circle = Fixed16> {
    pub(crate) id: Sk,
    pub(crate) layer: u32,
    count: u32,
    bytes: &'a [u8],
    sets: &'a Sets<C>,
}

impl<C: Circle> Clone for Node<'_, C> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<C: Circle> Copy for Node<'_, C> {}

impl<'a, C: Circle> Node<'a, C> {
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
    pub(crate) fn spans(self) -> impl Iterator<Item = (u32, u32, RegionRef<'a, C>)> + 'a {
        let sets = self.sets;
        self.raw().map(move |(lo, hi, s)| (lo, hi, sets.get(s)))
    }
    /// The last frame the node has a set (its deadline).
    pub(crate) fn last(self) -> u32 {
        self.raw().last().expect("a node has a span").1
    }
    /// `W_t` (`None` = empty).
    pub(crate) fn at(self, t: u32) -> Option<RegionRef<'a, C>> {
        if t < self.layer {
            return None;
        }
        let sets = self.sets;
        self.raw().find(|s| t <= s.1).map(|s| sets.get(s.2))
    }
}

pub(crate) struct Nodes<'a, C: Circle = Fixed16> {
    w: &'a Winning<C>,
    k: usize,
    end: usize,
    pos: usize,
    id: Sk,
}

impl<'a, C: Circle> Iterator for Nodes<'a, C> {
    type Item = Node<'a, C>;
    fn next(&mut self) -> Option<Node<'a, C>> {
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

impl<C: Circle> Winning<C> {
    /// The node at `pos` (whose previous node's state is `id`); both past it.
    fn decode_at(&self, pos: &mut usize, id: &mut Sk) -> Node<'_, C> {
        *id = get_sk(&self.data, pos, *id);
        let layer = get_varint(&self.data, pos) as u32;
        let count = get_varint(&self.data, pos) as u32;
        let start = *pos;
        for _ in 0..2 * count {
            get_varint(&self.data, pos);
        }
        Node { id: *id, layer, count, bytes: &self.data[start..*pos], sets: &self.sets }
    }
    pub(crate) fn node(&self, i: u32) -> Node<'_, C> {
        let (b, k) = (i as usize / BLOCK, i as usize % BLOCK);
        let (mut id, mut pos) = (self.blocks[b].0, self.blocks[b].1 as usize);
        for _ in 0..k {
            self.decode_at(&mut pos, &mut id);
        }
        self.decode_at(&mut pos, &mut id)
    }
    /// Nodes `lo..hi` in id order (`lo` a block's first).
    pub(super) fn nodes_from(&self, lo: usize, hi: usize) -> Nodes<'_, C> {
        assert!(lo % BLOCK == 0, "nodes from a block's first");
        Nodes { w: self, k: lo, end: hi.min(self.n), pos: 0, id: Sk::default() }
    }
    pub(crate) fn len(&self) -> usize {
        self.n
    }
    /// The node of state `id` (`None`: no set at any frame).
    pub(crate) fn index(&self, id: Sk) -> Option<u32> {
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
    pub(crate) fn id(&self, i: u32) -> Sk {
        self.node(i).id
    }
    /// `W_t(i)` (`None` = empty).
    pub(crate) fn at(&self, t: u32, i: u32) -> Option<RegionRef<'_, C>> {
        self.node(i).at(t)
    }
    /// The bytes held: the encoding, the block index and the sets' arena.
    pub(crate) fn bytes(&self) -> u64 {
        (self.data.len() + 16 * self.blocks.len() + self.sets.bytes()) as u64
    }
}

/// The spans a step closed (from `lo`, the step's frame + 1), by state, as
/// varints (state delta, `hi - lo`, set) at `at..at + len` of the runs.
#[derive(Clone, Copy)]
struct Run {
    lo: u16,
    n: usize,
    at: u64,
    len: u64,
}

impl Run {
    fn new(lo: u16, closed: impl IntoIterator<Item = (Sk, u16, u32)>, out: &mut Spill) -> Self {
        let mut bytes = Vec::new();
        let (mut last, mut n) = (Sk::default(), 0);
        for (id, hi, set) in closed {
            debug_assert!(id >= last && hi >= lo);
            put_sk(&mut bytes, last, id);
            put_varint(&mut bytes, (hi - lo) as u64);
            put_varint(&mut bytes, set as u64);
            last = id;
            n += 1;
        }
        let at = out.len();
        out.put(&bytes);
        Run { lo, n, at, len: bytes.len() as u64 }
    }
    /// `(state, hi, set)` in state order, from the spilled runs `all`.
    fn iter<'b>(&self, all: &'b [u8]) -> impl Iterator<Item = (Sk, u16, u32)> + 'b {
        let b = &all[self.at as usize..(self.at + self.len) as usize];
        let lo = self.lo;
        let (mut pos, mut id) = (0usize, Sk::default());
        (0..self.n).map(move |_| {
            id = get_sk(b, &mut pos, id);
            let hi = lo + get_varint(b, &mut pos) as u16;
            (id, hi, get_varint(b, &mut pos) as u32)
        })
    }
}

/// A node with a set at the frame done last: its set, its current span's
/// last frame, its layer (`| WIN`: a win node).
#[derive(Clone, Copy, Debug)]
struct Cur {
    id: Sk,
    set: u32,
    hi: u16,
    layer: u16,
}

const WIN: u16 = 1 << 15;

impl Cur {
    fn layer(&self) -> u16 {
        self.layer & !WIN
    }
}

/// An in-edge of a changed node: source and its layer, target's set, transfer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Emit {
    p: Sk,
    set: u32,
    xfer: u32,
    layer: u16,
}

/// A source's set so far at a step (at `t + 1` and the pulls), its layer.
#[derive(Clone, Copy, Debug)]
struct Acc {
    p: Sk,
    set: u32,
    layer: u16,
}

/// Set codes during a step: below `MADE`, the arena's; with `LOCAL`, the
/// step's own sets (made this step, not in the arena); with `MADE`, the
/// sets a range of the current pass made, numbered after it.
const LOCAL: u32 = 1 << 31;
const MADE: u32 = 1 << 30;

/// The in-edges a step reads at once; more are read in ROUNDS, each folded
/// into the step's sets so far (an `Emit` is 40 B: 0.8 GB a round).
const ROUND_EMITS: usize = 20 << 20;
const _: () = assert!(std::mem::size_of::<Emit>() == 40, "ROUND_EMITS is sized for 40-B emissions");

/// A step's own sets past this many bytes are compacted between rounds.
const LOCAL_COMPACT: usize = 1 << 30;

/// A step's own sets cut to those `acc` names (a round replaces its
/// sources' sets, so the old ones are named by nothing), `acc` renumbered.
fn compact_local<C: Circle>(acc: &mut [Acc], local: &RegionArena<C>) -> (RegionArena<C>, FxHashMap<u64, u32>) {
    let (mut kept, mut index, mut to) = (RegionArena::default(), FxHashMap::default(), vec![u32::MAX; local.len()]);
    for a in acc.iter_mut().filter(|a| a.set & LOCAL != 0) {
        let l = (a.set & !LOCAL) as usize;
        if to[l] == u32::MAX {
            let r = local.get(l as u32);
            to[l] = kept.push(r);
            index.entry(region_hash(r)).or_insert(to[l]);
        }
        a.set = to[l] | LOCAL;
    }
    (kept, index)
}

fn region_hash<C: Circle>(r: RegionRef<C>) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hs = rustc_hash::FxHasher::default();
    r.hash(&mut hs);
    hs.finish()
}

/// State ranges `[bounds[k], bounds[k + 1])`: about `parts` of equal
/// weight, from a `sample` of the `total` (one range when they are few).
fn id_ranges(mut sample: Vec<Sk>, total: usize, parts: usize) -> Vec<Sk> {
    sample.sort_unstable();
    let n = if total < 1 << 16 || sample.is_empty() { 1 } else { parts };
    let mut bounds: Vec<Sk> = vec![Sk::default()];
    bounds.extend((1..n).map(|k| sample[k * sample.len() / n]));
    bounds.push(SK_MAX);
    bounds.dedup();
    bounds
}

/// The first index at or past `j` whose key is not below `k` (`v` sorted,
/// every key before `j` below `k`), GALLOPING: stepping one by one costs
/// every segment read the whole group (26% of a big room's backward).
fn seek<A, B>(v: &[((u64, u64), A, B)], j: usize, k: (u64, u64)) -> usize {
    let mut lo = j;
    let mut step = 1;
    while lo < v.len() && v[lo].0 < k {
        let hi = (lo + step).min(v.len());
        if hi == v.len() || v[hi].0 >= k {
            return lo + 1 + v[lo + 1..hi].partition_point(|x| x.0 < k);
        }
        lo = hi;
        step *= 2;
    }
    lo
}

/// BACKWARD: `W_t` for `t = horizon` down to 0 from the win states `wins`
/// (id, layer); with `spill`, scratch files go there.
pub(crate) fn backward<N: Rem>(store: &EdgeStore<N>, pairs: &[Xfer<N>], wins: &[(Sk, u32)], horizon: u32, threads: usize, spill: Option<&std::path::Path>) -> anyhow::Result<Winning<N::C>> {
    let mut b = Backward::new(store, pairs, wins, horizon, threads);
    if let Some(dir) = spill {
        b = b.spill_to(dir)?;
    }
    for t in (0..horizon).rev() {
        b.step(t)?;
    }
    b.finish()
}

/// The backward between frames: `W_{t+1}`, its changed sets, closed spans.
pub struct Backward<'a, N: Rem = P8> {
    store: &'a EdgeStore<N>,
    /// Per target region the first frame with edges into it.
    first: FxHashMap<u32, u32>,
    pairs: &'a [Xfer<N>],
    horizon: u32,
    threads: usize,
    /// The ARENA of sets, equal sets once (found by hash; a collision with a
    /// different set is stored apart). Index 0: the whole torus.
    sets: RegionArena<N::C>,
    interned: FxHashMap<u64, u32>,
    /// The nodes with a set at the frame done last, by id.
    cur: Vec<Cur>,
    /// Their indices whose set changed there (or appeared).
    changed: Vec<u32>,
    /// Per step, the spans it closed, in `run_bytes`.
    runs: Vec<Run>,
    run_bytes: Spill,
    spill: Option<std::path::PathBuf>,
    /// Per target region, the steps that read its in-edges.
    opens: FxHashMap<u32, u32>,
    /// `ROUND_EMITS` and `LOCAL_COMPACT` (tests shrink both: `with_rounds`).
    round_emits: usize,
    local_compact: usize,
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

impl<'a, N: Rem> Backward<'a, N> {
    /// `W_horizon`: the win nodes present by the horizon.
    pub fn new(store: &'a EdgeStore<N>, pairs: &'a [Xfer<N>], wins: &[(Sk, u32)], horizon: u32, threads: usize) -> Self {
        assert!(horizon < WIN as u32, "a horizon past {WIN}");
        let mut sets = RegionArena::default();
        let full = Region::<N::C>::full();
        sets.push(full.as_ref());
        let mut interned: FxHashMap<u64, u32> = FxHashMap::default();
        interned.insert(region_hash(full.as_ref()), 0);
        let mut cur: Vec<Cur> = wins.iter().filter(|w| w.1 <= horizon).map(|&(id, layer)| Cur { id, set: 0, hi: horizon as u16, layer: layer as u16 | WIN }).collect();
        cur.sort_unstable_by_key(|c| c.id);
        cur.dedup_by_key(|c| c.id);
        let changed: Vec<u32> = (0..cur.len() as u32).collect();
        Backward {
            store,
            first: store.first_frames(),
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
            local_compact: LOCAL_COMPACT,
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

    /// Scratch files (closed spans, W's nodes, sealed set chunks) in `dir`.
    pub(crate) fn spill_to(mut self, dir: &std::path::Path) -> anyhow::Result<Self> {
        std::fs::create_dir_all(dir)?;
        self.run_bytes = Spill::file(dir.join("runs.bin"))?;
        self.sets.spill_to(dir);
        self.spill = Some(dir.to_path_buf());
        Ok(self)
    }

    /// Read in rounds of `emits` in-edges.
    #[cfg(test)]
    fn with_rounds(mut self, emits: usize) -> Self {
        self.round_emits = emits;
        self.local_compact = 0;
        self
    }

    /// The frame done last's nodes with a set, and each one's set.
    pub fn current(&self) -> impl Iterator<Item = (Sk, RegionRef<'_, N::C>)> + '_ {
        self.cur.iter().map(|c| (c.id, self.sets.get(c.set)))
    }

    /// The read, pull and merge walls so far.
    pub fn walls(&self) -> [std::time::Duration; 3] {
        [self.d_read, self.d_pull, self.d_merge]
    }

    /// A step's reads: the changed nodes by REGION, each read from the
    /// region's first frame with in-edges to `t + 1`, one ITEM a (region,
    /// frame); per group (first item, first frame, range of `changed`), then
    /// the total. Not from the least LAYER: an edge is recorded at its
    /// source's layer + 1 whatever its push, so its target may come LATER.
    fn read_items(&self, t: u32) -> Vec<(usize, u32, usize, usize)> {
        let mut items = Vec::new();
        let (mut at, mut a) = (0usize, 0usize);
        while a < self.changed.len() {
            let region = self.cur[self.changed[a] as usize].id.region;
            let b = a + self.changed[a..].partition_point(|&c| self.cur[c as usize].id.region == region);
            let first = self.first.get(&region).copied().unwrap_or(t + 1).max(1);
            items.push((at, first, a, b));
            at += (t + 2).saturating_sub(first) as usize;
            a = b;
        }
        items.push((at, 0, a, a));
        items
    }

    /// One ROUND of reads from item `start` until it holds `round_emits`
    /// in-edges (at step t, frames <= t + 1: sources of layer <= t). Per
    /// worker its in-edges sorted and distinct; and the item it stopped at
    /// (between items: one region bounds it by its in-edges at one frame).
    fn read_round(&self, items: &[(usize, u32, usize, usize)], start: usize) -> anyhow::Result<(Vec<(Vec<Emit>, ReadStats)>, usize)> {
        use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
        let (cur, changed, store, budget) = (&self.cur, &self.changed, self.store, self.round_emits);
        let geo = GEOMETRY;
        let total = items.last().map_or(0, |x| x.0);
        let (next, held) = (AtomicUsize::new(start), AtomicUsize::new(0));
        let parts: Vec<anyhow::Result<(Vec<Emit>, ReadStats)>> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..self.threads)
                .map(|_| {
                    sc.spawn(|| -> anyhow::Result<(Vec<Emit>, ReadStats)> {
                        let (mut emits, mut stats) = (Vec::new(), ReadStats::default());
                        let mut dec = crate::storage::edges::Decoder::default();
                        // The group's targets by (key, cell); per block
                        // target rank, its changed cells and first target.
                        let mut targets: Vec<((u64, u64), u8, u32)> = Vec::new();
                        let mut ranks: Vec<(u64, u32)> = Vec::new();
                        while held.load(Relaxed) < budget {
                            let k = next.fetch_add(1, Relaxed);
                            if k >= total {
                                break;
                            }
                            let g = items.partition_point(|x| x.0 <= k) - 1;
                            let (first_item, first_frame, a, b) = items[g];
                            let f = first_frame + (k - first_item) as u32;
                            let region = cur[changed[a] as usize].id.region;
                            targets.clear();
                            for &n in &changed[a..b] {
                                let n = &cur[n as usize];
                                targets.push((n.id.key, n.id.local, n.set));
                            }
                            let before = emits.len();
                            let slot = region % geo.slots;
                            stats.region_opens += 1;
                            for seg in store.segments(f, region) {
                                stats.segments += 1;
                                stats.stored += seg.bytes.len() as u64;
                                stats.raw += seg.raw as u64;
                                let blk = dec.open(&seg)?;
                                // The block's targets against the group's (both sorted).
                                ranks.clear();
                                let mut j = 0usize;
                                for &tk in &blk.t_keys {
                                    j = seek(&targets, j, tk);
                                    let mut m = 0u64;
                                    let mut e = j;
                                    while e < targets.len() && targets[e].0 == tk {
                                        m |= 1u64 << targets[e].1;
                                        e += 1;
                                    }
                                    ranks.push((m, j as u32));
                                }
                                stats.bundles += seg.bundles as u64;
                                let sources = &blk.sources;
                                blk.records(|r| {
                                    let (m, at) = ranks[r.t as usize];
                                    let mut hit = m & r.mask;
                                    while hit != 0 {
                                        let c = hit.trailing_zeros();
                                        hit &= hit - 1;
                                        let set = targets[at as usize..].iter().find(|x| x.1 as u32 == c).expect("a changed cell").2;
                                        let (s_shape, s_key) = sources[r.s as usize];
                                        let s_cell = crate::storage::bundle::source_cell(geo.cell(slot, c), r.o);
                                        emits.push(Emit { p: Sk::at(&geo, s_shape, s_key, s_cell), set, xfer: r.x, layer: (f - 1) as u16 });
                                    }
                                })?;
                            }
                            stats.edges += (emits.len() - before) as u64;
                            held.fetch_add(emits.len() - before, Relaxed);
                        }
                        emits.sort_unstable();
                        emits.dedup();
                        emits.shrink_to_fit();
                        Ok((emits, stats))
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a backward reader panicked")).collect()
        });
        Ok((parts.into_iter().collect::<anyhow::Result<Vec<_>>>()?, next.into_inner().min(total)))
    }

    /// A set code's set (`local`: the step's own sets).
    fn set_of<'s>(&'s self, code: u32, local: &'s RegionArena<N::C>) -> RegionRef<'s, N::C> {
        if code & LOCAL != 0 {
            local.get(code & !LOCAL)
        } else {
            self.sets.get(code)
        }
    }

    fn arena_code(&self, r: &Region<N::C>, h: u64) -> Option<u32> {
        self.interned.get(&h).copied().filter(|&s| self.sets.get(s) == *r)
    }

    /// Fold a round's in-edges into the step's sets so far (`acc`): per
    /// source its set so far (or at `t + 1`) united with the round's pulls.
    fn fold_round(&self, acc: Vec<Acc>, parts: &[(Vec<Emit>, ReadStats)], local: &mut RegionArena<N::C>, local_interned: &mut FxHashMap<u64, u32>) -> Vec<Acc> {
        let n_emits: usize = parts.iter().map(|p| p.0.len()).sum();
        let bounds = id_ranges(parts.iter().flat_map(|p| p.0.iter().step_by(64).map(|e| e.p)).collect(), n_emits, self.threads * 4);
        let (cur, pairs, local_ro) = (&self.cur, self.pairs, &*local);
        let outs: Vec<(Vec<Acc>, Vec<Region<N::C>>)> = crate::par::Pool::new(self.threads).chunks(bounds.len() - 1, 1, |k, _| {
            let (lo, hi) = (bounds[k], bounds[k + 1]);
            let a = &acc[acc.partition_point(|x| x.p < lo)..acc.partition_point(|x| x.p < hi)];
            let c = &cur[cur.partition_point(|x| x.id < lo)..cur.partition_point(|x| x.id < hi)];
            let mut es: Vec<Emit> = Vec::new();
            for p in parts {
                es.extend_from_slice(&p.0[p.0.partition_point(|e| e.p < lo)..p.0.partition_point(|e| e.p < hi)]);
            }
            // Sorted runs, one a worker: merged (the stable sort finds runs).
            es.sort();
            es.dedup();
            let (mut out, mut made) = (Vec::with_capacity(a.len() + es.len() / 4), Vec::new());
            let (mut pieces, mut scratch) = (Vec::new(), Vec::new());
            let (mut i, mut j, mut ci) = (0usize, 0usize, 0usize);
            while i < a.len() || j < es.len() {
                let ia = a.get(i).map_or(SK_MAX, |x| x.p);
                let ie = es.get(j).map_or(SK_MAX, |e| e.p);
                if ia < ie {
                    out.push(a[i]);
                    i += 1;
                    continue;
                }
                let j_end = j + es[j..].partition_point(|e| e.p == ie);
                let layer = es[j].layer;
                assert!(es[j..j_end].iter().all(|e| e.layer == layer), "state {} has in-edges recorded at two frames", ie.show());
                // The base: the set so far, else the set at t + 1.
                let base: Option<u32> = if ia == ie {
                    i += 1;
                    assert_eq!(a[i - 1].layer, layer, "state {}: its layer from the edges disagrees", ie.show());
                    Some(a[i - 1].set)
                } else {
                    ci += c[ci..].partition_point(|x| x.id < ie);
                    c.get(ci).filter(|x| x.id == ie).map(|x| {
                        assert_eq!(x.layer(), layer, "state {}: its layer from the edges disagrees", ie.show());
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
                        match self.arena_code(&r, h).or_else(|| local_interned.get(&h).copied().filter(|&s| local_ro.get(s) == r).map(|s| s | LOCAL)) {
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
        drop(acc);
        // The ranges' new sets become the step's own, once each.
        let mut acc = Vec::with_capacity(outs.iter().map(|o| o.0.len()).sum());
        for (out, made) in outs {
            let codes: Vec<u32> = made
                .into_iter()
                .map(|r| {
                    let h = region_hash(r.as_ref());
                    match local_interned.get(&h) {
                        Some(&s) if local.get(s) == r => s | LOCAL,
                        found => {
                            let s = local.len() as u32;
                            assert!(s < MADE, "more distinct sets in a step than set codes");
                            if found.is_none() {
                                local_interned.insert(h, s);
                            }
                            local.push(r.as_ref());
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

    /// `W_t` from `W_{t+1}`: fold the changed nodes' in-edges; sets that grew
    /// replace their spans, nodes of layer `t + 1` close theirs.
    pub fn step(&mut self, t: u32) -> anyhow::Result<()> {
        assert!(t < self.horizon, "a step below the horizon");
        let mut last_region = u32::MAX;
        for &c in &self.changed {
            let r = self.cur[c as usize].id.region;
            if r != last_region {
                *self.opens.entry(r).or_default() += 1;
                last_region = r;
            }
        }
        let n_read = self.changed.len();
        self.n_changed += n_read as u64;
        let (mut acc, mut local, mut local_interned) = (Vec::new(), RegionArena::default(), FxHashMap::default());
        let items = self.read_items(t);
        let total = items.last().map_or(0, |x| x.0);
        let (mut start, mut n_emits, mut rounds) = (0usize, 0usize, 0u32);
        let (mut d_read, mut d_pull) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
        while start < total {
            let t0 = std::time::Instant::now();
            let (parts, end) = self.read_round(&items, start)?;
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
            if start < total && local.bytes() > self.local_compact {
                (local, local_interned) = compact_local(&mut acc, &local);
            }
            d_pull += t0.elapsed();
        }
        self.n_emits += n_emits as u64;
        self.rounds += rounds as u64;
        self.d_read += d_read;
        self.d_pull += d_pull;
        // MERGE, the step's sets against those at t + 1: a set that grew
        // closes its span and opens one, a node of layer t + 1 closes its.
        let t0 = std::time::Instant::now();
        let close_at = t as u16 + 1;
        let bounds = id_ranges(acc.iter().step_by(64).map(|x| x.p).chain(self.cur.iter().step_by(64).map(|x| x.id)).collect(), acc.len() + self.cur.len(), self.threads * 4);
        struct RangeOut {
            cur: Vec<Cur>,
            changed: Vec<u32>,
            closed: Vec<(Sk, u16, u32)>,
        }
        let (cur, acc_ref, local_ref) = (&self.cur, &acc, &local);
        let outs: Vec<RangeOut> = crate::par::Pool::new(self.threads).chunks(bounds.len() - 1, 1, |k, _| {
            let (lo, hi) = (bounds[k], bounds[k + 1]);
            let c = &cur[cur.partition_point(|x| x.id < lo)..cur.partition_point(|x| x.id < hi)];
            let a = &acc_ref[acc_ref.partition_point(|x| x.p < lo)..acc_ref.partition_point(|x| x.p < hi)];
            let mut o = RangeOut { cur: Vec::with_capacity(c.len() + a.len() / 8), changed: Vec::new(), closed: Vec::new() };
            let (mut i, mut j) = (0usize, 0usize);
            while i < c.len() || j < a.len() {
                let ic = c.get(i).map_or(SK_MAX, |x| x.id);
                let ia = a.get(j).map_or(SK_MAX, |x| x.p);
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
        // In range order: the sets a node keeps go into the arena, once.
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
                        let r = local.get(l as u32);
                        let h = region_hash(r);
                        to_arena[l] = match self.interned.get(&h) {
                            Some(&s) if self.sets.get(s) == r => s,
                            found => {
                                let s = self.sets.push(r);
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
        let (step_sets, step_kept, step_bytes) = (local.len(), to_arena.iter().filter(|&&s| s != u32::MAX).count(), local.bytes());
        drop(local);
        let run = Run::new(close_at, closed, &mut self.run_bytes);
        self.runs.push(run);
        self.cur = new_cur;
        self.changed = changed;
        self.max_cur = self.max_cur.max(self.cur.len());
        // The step's workers are gone: return their freed pages now
        // (mimalloc reclaims them lazily: ~3 GB held on a big room).
        // SAFETY: mimalloc's documented API, callable from any thread.
        unsafe { libmimalloc_sys::mi_collect(true) };
        let d_merge = t0.elapsed();
        self.d_merge += d_merge;
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        eprintln!(
            "[arc] f{t:03}: {n_read} changed at f{} -> {n_emits} in-edges ({rounds} rounds); {} nodes with a set, {} changed | read {:.0} pull {:.0} merge {:.0} ms | anon {:.2} GB: frame {:.2}, runs {:.2}, {} sets {:.2} ({:.2} mapped), the step's {step_sets} sets {:.2} ({step_kept} kept)",
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
            self.sets.mapped_bytes() as f64 / 1e9,
            step_bytes as f64 / 1e9,
        );
        if (self.horizon - 1 - t) % 4 == 0 {
            let (k, med, p90, max) = fragmentation(self.cur.iter().filter(|c| c.layer & WIN == 0).map(|c| self.sets.get(c.set)));
            self.fragments.push(format!("[arc] W frame {t:3}: {k} non-win nodes can win; rectangles per node med {med} p90 {p90} max {max}"));
        }
        Ok(())
    }

    /// The winning sets once frame 0 is done, renumbered by use.
    pub(crate) fn finish(mut self) -> anyhow::Result<Winning<N::C>> {
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
        let spans: usize = runs.iter().map(|r| r.n).sum();
        let mut iters: Vec<_> = runs.iter().map(|r| r.iter(&all)).collect();
        let mut heap: std::collections::BinaryHeap<Reverse<(Sk, u16, u16, u32, usize)>> = std::collections::BinaryHeap::new();
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
        let (mut cur_id, mut prev_id) = (None::<Sk>, Sk::default());
        let mut flush = |id: Sk, node: &mut Vec<(u16, u16, u32)>, prev_id: &mut Sk| {
            if n % BLOCK == 0 {
                blocks.push((id, data.len()));
                *prev_id = id;
            }
            bytes.clear();
            put_sk(&mut bytes, *prev_id, id);
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
            "[arc] backward: {} sets changed, {} in-edges read ({} emissions in {} rounds, at most {} a round), {} segments decoded: {:.2} GB stored ({:.3} GB the store), {:.2} GB raw, {} bundles; {} target regions read on {o_mean:.1} steps each (at most {o_max}), {} (region, frame) opens; at most {} nodes a frame; read {:.2} s, pull {:.2} s, merge {:.2} s; {} nodes, {} spans, {} distinct sets: W {:.2} GB (nodes and spans {:.2}, sets {:.2}, {:.2} of them mapped)",
            self.n_changed,
            self.read.edges,
            self.n_emits,
            self.rounds,
            self.max_round,
            self.read.segments,
            self.read.stored as f64 / 1e9,
            self.store.bytes as f64 / 1e9,
            self.read.raw as f64 / 1e9,
            self.read.bundles,
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
            w.sets.arena.mapped_bytes() as f64 / 1e9,
        );
        for f in &self.fragments {
            eprintln!("{f}");
        }
        Ok(w)
    }
}

/// x segments per set: (sets, median, p90, max).
fn fragmentation<'a, C: Circle>(sets: impl Iterator<Item = RegionRef<'a, C>>) -> (usize, usize, usize, usize) {
    let mut v: Vec<usize> = sets.map(|r| r.n_segs()).collect();
    if v.is_empty() {
        return (0, 0, 0, 0);
    }
    v.sort_unstable();
    let q = |f: f64| v[((v.len() - 1) as f64 * f) as usize];
    (v.len(), q(0.5), q(0.9), q(1.0))
}

/// The OPTIMUM from `start` at remainder `point`, read off ONE backward:
/// `horizon - max{t : point in W_t(start)}` (`None`: the horizon is
/// refuted). A lower bound: a concrete winner at frame `h` is at each frame
/// `k` in a node of layer <= k (the hull forward's induction), so `point`
/// is in `W_{horizon - h}(start)` (its layers <= k <= horizon - h + k).
pub(crate) fn optimum<C: Circle>(w: &Winning<C>, start: Option<u32>, point: (C::P, C::P), horizon: u32) -> Option<u32> {
    let s = start?;
    (0..=horizon).rev().find(|&t| w.at(t, s).is_some_and(|r| r.contains(point.0, point.1))).map(|t| horizon - t)
}

/// What the backward starts from, the tree in `dir` up to `horizon`: the
/// start, the win states with their layers, the edge store (every frame
/// that kept rows has edges; one that kept none, under level -1, may not).
pub struct Seeds<N: Rem = P8> {
    pub(crate) start: Sk,
    pub wins: Vec<(Sk, u32)>,
    pub store: EdgeStore<N>,
    /// The transfer table, decoded.
    pub pairs: Vec<Xfer<N>>,
    /// The tree's shapes by index, and its key space.
    pub(crate) shapes: Vec<u64>,
    pub space: celeste_engine::exact::KeySpace<N>,
}

impl<N: Rem> Seeds<N> {
    pub fn load(dir: &std::path::Path, horizon: u32) -> anyhow::Result<Self> {
        let tree = crate::storage::layer::Tree::<N>::open(dir, horizon)?;
        let (first, _) = tree.layer(0, None, 1)?;
        let mut start = first.states_iter();
        let (Some(start), None) = (start.next(), start.next()) else { anyhow::bail!("{}: frame 0 is not one state", dir.display()) };
        let mut wins: Vec<(Sk, u32)> = Vec::new();
        for f in 1..=horizon {
            if let Some(file) = tree.file(f).filter(|x| x.header.wins() > 0) {
                wins.extend(file.wins()?.into_iter().map(|w| (w, f)));
            }
        }
        let store = EdgeStore::<N>::open(&dir.join("edges"), horizon)?;
        for f in 1..=horizon {
            let rows = tree.states(f);
            anyhow::ensure!(rows == 0 || store.has_frame(f), "{}: no edges for f{f}, which kept {rows} states", dir.display());
        }
        let pairs = store.pairs().iter().map(N::xfer).collect();
        Ok(Seeds { start, wins, store, pairs, shapes: tree.shapes, space: tree.space })
    }

    /// The win states, sorted.
    pub(crate) fn win_ids(&self) -> Vec<Sk> {
        let mut v: Vec<Sk> = self.wins.iter().map(|w| w.0).collect();
        v.sort_unstable();
        v.dedup();
        v
    }

    /// A state's `(shape, key, cell)`.
    pub fn state(&self, s: Sk) -> (u64, (u64, u64), u32) {
        let geo = GEOMETRY;
        (self.shapes[s.shape(&geo) as usize], s.key, s.cell(&geo))
    }
}

/// One node's term of the gate's `W f{t}` line: its content hash
/// (`exact::ContentHash::state`) folded with its set's rectangles.
pub fn gate_w_hash<C: Circle>(node: u64, r: RegionRef<C>) -> u64 {
    use celeste_engine::runtime2::mix64;
    let mut h = node;
    for (y, xs) in r.slabs() {
        h = mix64(h ^ C::seg_word(y));
        for x in xs {
            h = mix64(h ^ C::seg_word(x));
        }
    }
    h
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::search::arc_edges::AxisXfer;
    use crate::search::arcs::{pieces, point, Action, Seg, Set, Transfer, CIRCLE};

    /// `seek` is the linear walk: from every start and for every key, over
    /// groups with repeated keys, gaps and runs.
    #[test]
    fn seek_finds_what_the_linear_walk_finds() {
        let mut x = 0x9e37_79b9_7f4a_7c15u64;
        let mut rnd = |n: u64| {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            x % n
        };
        for len in [0usize, 1, 2, 3, 7, 64, 1000] {
            let mut v: Vec<((u64, u64), u8, u32)> = (0..len).map(|_| ((rnd(3), rnd(200)), 0, 0)).collect();
            v.sort_unstable();
            for _ in 0..200 {
                let k = (rnd(4), rnd(220));
                let below = v.partition_point(|t| t.0 < k);
                for j in 0..=below {
                    assert_eq!(seek(&v, j, k), below, "len {len} from {j} to {k:?}");
                }
            }
        }
    }

    /// A deterministic `r(m)` in `0..m` from `seed`.
    pub(crate) fn rng(mut s: u64) -> impl FnMut(u32) -> u32 {
        move |m| {
            s = s.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((s >> 33) as u32) % m
        }
    }

    /// An action on one point of the circle.
    pub(crate) fn apply(a: Action, p: u32) -> u32 {
        match a {
            Action::Rotate(v) => (p as i64 + v as i64).rem_euclid(CIRCLE as i64) as u32,
            Action::Const(c) => c,
        }
    }

    /// Node `k`'s state (increasing in `k`, across regions and keys).
    pub(crate) fn sid(k: u32) -> Sk {
        Sk { region: 1 + (k >> 6), key: (0, ((k >> 3) & 7) as u64), local: (k & 7) as u8 }
    }

    pub(crate) fn axis(t: Transfer) -> AxisXfer {
        match t.action {
            Action::Rotate(v) => AxisXfer { lo: t.guard.lo, hi: t.guard.hi, tag: 0, val: v },
            Action::Const(c) => AxisXfer { lo: t.guard.lo, hi: t.guard.hi, tag: 1, val: c as i32 },
        }
    }

    /// A graph over nodes `0..` with `layers` (node 0 the start, layer 0)
    /// as a tree's edge files: an edge `(src, dst, x, y)` recorded at frame
    /// `layers[src] + 1` (so `layers[dst] <= layers[src] + 1`); the wins'
    /// out-edges are dropped, as the forward never expands a win.
    pub(crate) struct TestTree {
        dir: std::path::PathBuf,
        layers: Vec<u32>,
        wins: Vec<u32>,
    }

    impl TestTree {
        pub(crate) fn new(name: &str, layers: Vec<u32>, edges: &[(u32, u32, Transfer, Transfer)], wins: Vec<u32>) -> Self {
            use std::sync::atomic::{AtomicU32, Ordering};
            static SEQ: AtomicU32 = AtomicU32::new(0);
            let dir = std::env::temp_dir().join(format!("celeste-arcdp-{name}-{}-{}", std::process::id(), SEQ.fetch_add(1, Ordering::Relaxed)));
            let _ = std::fs::remove_dir_all(&dir);
            let mut pairs: Vec<crate::search::arc_edges::Pair> = edges.iter().map(|e| (axis(e.2), axis(e.3))).collect();
            pairs.sort_unstable();
            pairs.dedup();
            let mut frames: std::collections::BTreeMap<u32, Vec<(Sk, Sk, u32)>> = Default::default();
            for &(a, b, x, y) in edges {
                if wins.contains(&a) {
                    continue;
                }
                assert!(layers[b as usize] <= layers[a as usize] + 1, "an edge into a later layer");
                let k = pairs.binary_search(&(axis(x), axis(y))).expect("listed") as u32;
                frames.entry(layers[a as usize] + 1).or_default().push((sid(a), sid(b), k));
            }
            let frames: Vec<(u32, Vec<(Sk, Sk, u32)>)> = frames.into_iter().collect();
            crate::storage::edges::write_test_edges(&dir, &frames, &pairs).expect("the test tree");
            TestTree { dir, layers, wins }
        }

        /// The store up to `horizon`, its transfers, and its win seeds.
        pub(crate) fn open(&self, horizon: u32) -> (EdgeStore, Vec<(Transfer, Transfer)>, Vec<(Sk, u32)>) {
            let store = EdgeStore::<celeste_core::pico8_num::Pico8Num>::open(&self.dir, horizon).expect("open");
            let pairs = store.pairs().iter().map(|p| (p.0.transfer(), p.1.transfer())).collect();
            let wins = self.wins.iter().map(|&k| (sid(k), self.layers[k as usize])).collect();
            (store, pairs, wins)
        }

        pub(crate) fn win_ids(&self) -> Vec<Sk> {
            let mut v: Vec<Sk> = self.wins.iter().map(|&k| sid(k)).collect();
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
    pub(crate) fn random_transfer(r: &mut dyn FnMut(u32) -> u32) -> Transfer {
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
    pub(crate) fn random_graph(r: &mut dyn FnMut(u32) -> u32, n: u32, y_too: bool) -> (Vec<u32>, Vec<(u32, u32, Transfer, Transfer)>, Vec<u32>) {
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
    pub(crate) fn definition(layers: &[u32], edges: &[(u32, u32, Transfer, Transfer)], wins: &[u32], horizon: u32) -> Vec<Vec<Option<Region>>> {
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
        let mut r = rng(11);
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
            let mut b = Backward::new(&store, &pairs, &seeds, horizon, 2).with_rounds(1 + r(4) as usize);
            for t in (0..horizon).rev() {
                b.step(t).expect("a step");
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

    /// With first-reach layers, the optimum read off `backward(H)` is the
    /// smallest `h` whose own backward holds the start.
    #[test]
    fn one_backward_gives_the_optimum() {
        let mut r = rng(5);
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
