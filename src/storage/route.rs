//! THE ROUTING of a wave's emissions to their TARGET regions, and the
//! REGION PHASE there (plans/edges-target.md "The forward").
//!
//! A unit (a run of frontier rows through the kernels) ORs each emission
//! into its BUNDLE (`bundle::Bundle`: source key, transfer, offset, target
//! key -> target cells) in a unit-local table (`UnitBundles`: one probe and
//! an OR, in cache), keyed also by the target's ROUTE - its (shape, region
//! slot): a storage region. An emission without an edge (the reference
//! engine's) is a bundle of its target alone (`NO_XFER`). At the unit's end
//! its bundles are the CHUNKS of their routes: per route the unit's
//! distinct targets, its sources, the bundles naming both by index. The
//! region phase (`run_route`) takes one route's chunks of one slice as a
//! JOB: per chunk target ONE probe of the region's table (the dedup: the
//! cells the entry lacks are new states, set at once); per bundle its
//! target named by the table's entry; the job's bundles encoded into a
//! SEGMENT of the frame's edge file. Bundles of one key from two units
//! stay two (their cells differ): merging them across units saved ~17% of
//! the bundles and cost a route-level table that was DRAM-bound. No lookup
//! happens in the units, no state is named by a number, no row is copied:
//! the new layer is the routes' new (key, cells) at the frame's end
//! (`seal`, `wave`).

use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

use super::bundle::{self, Bundle};
use super::edges::Segment;
use super::visited::{Key, RegionTable};
use celeste_engine::exact::is_provisional;

/// A unit bundle's transfer when it holds no edge (its target alone).
pub const NO_XFER: u32 = u32::MAX;

/// `Scratch::e_of`'s mark of a provisional target.
const PROV: u32 = u32::MAX;

/// The wave's routes: a target's (shape, region slot) -> a dense id, made
/// by the units as they emit (read-mostly: a worker caches its own).
#[derive(Default)]
pub struct Routes {
    map: RwLock<FxHashMap<(u64, u32), u32>>,
    list: RwLock<Vec<(u64, u32)>>,
}

impl Routes {
    /// The id of `(shape, slot)`, made on first use.
    pub fn id(&self, shape: u64, slot: u32) -> u32 {
        if let Some(&r) = self.map.read().expect("routes").get(&(shape, slot)) {
            return r;
        }
        let mut m = self.map.write().expect("routes");
        if let Some(&r) = m.get(&(shape, slot)) {
            return r;
        }
        let mut l = self.list.write().expect("routes");
        let r = l.len() as u32;
        l.push((shape, slot));
        m.insert((shape, slot), r);
        r
    }

    /// Every route's (shape, slot), by id.
    pub fn list(&self) -> Vec<(u64, u32)> {
        self.list.read().expect("routes").clone()
    }
}

/// A unit's bundles of one route: its distinct TARGETS (key, the cells
/// reached), the unit's distinct sources (shared by its chunks), the
/// bundles naming both by index (their transfers the worker's local ids),
/// and the source regions their edges leave.
pub struct Chunk {
    pub route: u32,
    pub unit: u32,
    pub worker: u32,
    pub targets: Vec<(Key, u64)>,
    pub sources: std::sync::Arc<[(u32, Key)]>,
    pub bundles: Vec<Ub>,
    pub src_regions: Vec<u32>,
}

/// A unit bundle (24 B): its target's and its source's index in its chunk,
/// its transfer (the worker's local id; `NO_XFER`: no edge), its offset,
/// its target cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ub {
    pub t: u32,
    pub s: u32,
    pub x: u32,
    pub o: u32,
    pub mask: u64,
}

/// A route through a frame: its region's table (taken out of the visited
/// set for the frame; a new one for a new shape's region), what is new in
/// it this frame, and its encoded segments.
pub struct RouteState {
    pub shape: u64,
    pub slot: u32,
    /// The visited set's region, once the shape is numbered.
    pub region: Option<u32>,
    pub table: Box<RegionTable>,
    /// Entries at the frame's start.
    pub start: u32,
    /// THE NEW CELLS, as the dedup found them: `(entry, cells it lacked)`
    /// per chunk target that found any - merged and keyed at the frame's
    /// end (`seal`). 12-16 B an event, against 8 B for every entry of the
    /// region in a per-entry array (the mod room's f69: 14.8M events
    /// against 68M entries).
    pub fresh: Vec<(u32, u64)>,
    /// A raise's cells new this frame per entry: a hit on a cell the tree
    /// held needs its layer, one the raise just made has none.
    fresh_raise: Option<FxHashMap<u32, u64>>,
    /// Bundles into provisional targets, per job (slice) a segment
    /// encoded with their provisional keys: encoded again at the frame's
    /// end with their final keys. Held as zstd blocks, not bundles: the
    /// mod room's f53 (+95) held 103M of them, 5.8 GB as structs.
    pub prov: Vec<(u32, Segment)>,
    /// Its segments with their kind (0: a job's, 1: a job's provisional
    /// bundles made final) and slice: jobs of a route may run out of slice
    /// order; the file has them in this one (`seal`).
    pub segs: Vec<((u8, u32), Segment)>,
    /// The source regions its edges leave.
    pub sources: FxHashSet<u32>,
    /// Records taken (emissions) and new states found; the dedup's
    /// events that found new cells (an entry's new cells, per chunk target).
    pub recs: u64,
    pub new_states: u64,
    pub fresh_events: u64,
}

impl RouteState {
    pub fn new(shape: u64, slot: u32, region: Option<u32>, table: Option<Box<RegionTable>>) -> Self {
        let table = table.unwrap_or_default();
        let start = table.len() as u32;
        RouteState { shape, slot, region, fresh: Vec::new(), fresh_raise: None, table, start, prov: Vec::new(), segs: Vec::new(), sources: FxHashSet::default(), recs: 0, new_states: 0, fresh_events: 0 }
    }
}

/// What the region phase reads besides its route.
pub struct RegionCtx<'a> {
    pub frame: u32,
    /// Are edges recorded?
    pub record: bool,
    /// Per worker, its local transfer ids' global ones.
    pub remaps: &'a [Vec<u32>],
    /// A raise's per-state layers: a hit on a state of a later layer than
    /// the frame is refused.
    pub layers: Option<&'a super::wave::StateLayers>,
}

/// A unit's bundles, by route: one probe and an OR an emission in a table
/// keyed by (bucket, target key, source, transfer, offset) - the last
/// bundle remembered: a body's lanes mostly share it. Each route's bundles
/// in a bucket of their own; at the unit's end each bucket's distinct
/// targets are listed and its bundles name them by index (`drain`).
#[derive(Default)]
pub struct UnitBundles {
    buckets: Vec<(u32, Vec<(Key, Ub)>)>,
    bucket_of: FxHashMap<u32, u32>,
    last_route: Option<(u32, u32)>,
    /// `bucket << 32 | position + 1`; 0 empty.
    index: Vec<u64>,
    n: usize,
    last: Option<(u32, u32)>,
    /// `drain`'s target index.
    t_idx: rustc_hash2::FxHashMap<Key, u32>,
    /// Emptied buckets, kept for the next units (their pages stay mapped),
    /// at most `SPARE_KEEP` bundles' room in all: kept without a bound,
    /// every bucket grew to the largest route's (56 MB a worker on the mod
    /// room's f53); bound to 128k, the units regrew theirs (the reference
    /// frame's emit +4%).
    spare: Vec<Vec<(Key, Ub)>>,
}

/// The bundles' room `UnitBundles` keeps in its emptied buckets (20 MB).
const SPARE_KEEP: usize = 1 << 19;

#[inline]
fn ub_hash(k: u32, t_key: Key, b: &Ub) -> u64 {
    let h = t_key.1.wrapping_mul(0x9e37_79b9_7f4a_7c15)
        ^ t_key.0.wrapping_mul(0xc2b2_ae3d_27d4_eb4f)
        ^ ((k as u64) << 32 | b.s as u64).wrapping_mul(0x1656_67b1_9e37_79f9)
        ^ ((b.x as u64) << 32 | b.o as u64).wrapping_mul(0x85eb_ca77_c2b2_ae63);
    celeste_engine::runtime2::mix64(h)
}

impl UnitBundles {
    /// Bytes its tables hold (`--phases`' `[wavemem]`).
    pub fn alloc_bytes(&self) -> usize {
        let b = 16 + std::mem::size_of::<Ub>();
        self.buckets.iter().map(|x| x.1.capacity() * b).sum::<usize>() + self.spare.iter().map(|v| v.capacity() * b).sum::<usize>() + 8 * self.index.capacity() + 24 * self.t_idx.capacity()
    }

    pub fn clear(&mut self) {
        if self.n > 0 {
            self.index.fill(0);
        }
        let mut room: usize = self.spare.iter().map(Vec::capacity).sum();
        for (_, mut v) in self.buckets.drain(..) {
            if room + v.capacity() <= SPARE_KEEP {
                room += v.capacity();
                v.clear();
                self.spare.push(v);
            }
        }
        self.bucket_of.clear();
        (self.last_route, self.last, self.n) = (None, None, 0);
    }

    /// OR `bit` into the bundle `(route, t_key; source s, x, o)`.
    #[inline]
    pub fn insert(&mut self, route: u32, t_key: Key, s: u32, x: u32, o: u32, bit: u64) {
        let probe = Ub { t: 0, s, x, o, mask: 0 };
        let same = |r: &(Key, Ub)| r.0 == t_key && r.1.s == s && r.1.x == x && r.1.o == o;
        if let Some((k, p)) = self.last {
            let (r, v) = &mut self.buckets[k as usize];
            if *r == route && same(&v[p as usize]) {
                v[p as usize].1.mask |= bit;
                return;
            }
        }
        let k = match self.last_route {
            Some((r, k)) if r == route => k,
            _ => {
                let k = match self.bucket_of.get(&route) {
                    Some(&k) => k,
                    None => {
                        let v = self.spare.pop().unwrap_or_default();
                        self.buckets.push((route, v));
                        let k = self.buckets.len() as u32 - 1;
                        self.bucket_of.insert(route, k);
                        k
                    }
                };
                self.last_route = Some((route, k));
                k
            }
        };
        if self.index.is_empty() {
            self.index = vec![0; 1 << 12];
        }
        let m = self.index.len() - 1;
        let mut i = ub_hash(k, t_key, &probe) as usize & m;
        loop {
            let e = self.index[i];
            if e == 0 {
                break;
            }
            let (bk, p) = ((e >> 32) as u32, (e as u32) - 1);
            if bk == k && same(&self.buckets[k as usize].1[p as usize]) {
                self.buckets[k as usize].1[p as usize].1.mask |= bit;
                self.last = Some((k, p));
                return;
            }
            i = (i + 1) & m;
        }
        let v = &mut self.buckets[k as usize].1;
        v.push((t_key, Ub { mask: bit, ..probe }));
        let p = v.len() as u32 - 1;
        self.index[i] = (k as u64) << 32 | (p as u64 + 1);
        self.last = Some((k, p));
        self.n += 1;
        if self.n * 2 > self.index.len() {
            let cap = self.index.len() * 2;
            self.index = vec![0; cap];
            let m = cap - 1;
            for (k, (_, v)) in self.buckets.iter().enumerate() {
                for (p, (t_key, u)) in v.iter().enumerate() {
                    let mut i = ub_hash(k as u32, *t_key, u) as usize & m;
                    while self.index[i] != 0 {
                        i = (i + 1) & m;
                    }
                    self.index[i] = (k as u64) << 32 | (p as u64 + 1);
                }
            }
        }
    }

    /// The buckets as `(route, targets, bundles)`: per bucket its distinct
    /// targets (key, cells reached) and its bundles naming them by index,
    /// the cells `drop(route, key, cell)` says cleared first (the coarser
    /// level's disallowed targets; a bundle left without cells goes); the
    /// table cleared.
    #[allow(clippy::type_complexity)]
    pub fn drain(&mut self, drop: Option<&dyn Fn(u32, Key, u32) -> bool>) -> Vec<(u32, Vec<(Key, u64)>, Vec<Ub>)> {
        let buckets = std::mem::take(&mut self.buckets);
        let mut out = Vec::with_capacity(buckets.len());
        for &(route, ref v) in &buckets {
            self.t_idx.clear();
            let mut targets: Vec<(Key, u64)> = Vec::new();
            let mut bundles: Vec<Ub> = Vec::with_capacity(v.len());
            let mut last: Option<(Key, u32)> = None;
            for &(t_key, mut u) in v {
                let mut m = u.mask;
                if let Some(drop) = drop {
                    let mut c = m;
                    while c != 0 {
                        let cell = c.trailing_zeros();
                        c &= c - 1;
                        if drop(route, t_key, cell) {
                            m &= !(1u64 << cell);
                        }
                    }
                    if m == 0 {
                        continue;
                    }
                }
                let t = match last {
                    Some((k, t)) if k == t_key => t,
                    _ => {
                        let n = targets.len() as u32;
                        let t = *self.t_idx.entry(t_key).or_insert(n);
                        if t == n {
                            targets.push((t_key, 0));
                        }
                        last = Some((t_key, t));
                        t
                    }
                };
                targets[t as usize].1 |= m;
                u.t = t;
                u.mask = m;
                bundles.push(u);
            }
            if !bundles.is_empty() {
                out.push((route, targets, bundles));
            }
        }
        self.buckets = buckets;
        self.clear();
        out
    }
}

/// A worker's scratch for the region phase: a chunk's targets' entries and
/// sources' indices, the route's source list, its edges as the encoder
/// takes them, the encoder, the raw block, zstd.
pub struct Scratch {
    /// The job's bundles into provisional targets.
    prov: Vec<Bundle>,
    e_of: Vec<u32>,
    s_of: Vec<u32>,
    s_idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    s_list: Vec<(u32, Key)>,
    tbs: Vec<bundle::Tb>,
    enc: bundle::Encoder,
    raw: Vec<u8>,
    z: zstd::bulk::Compressor<'static>,
}

/// Bundles a worker's region scratch keeps room for between jobs: a
/// heavy route's job is millions of bundles (the mod room's f69: 3.1M),
/// and every worker kept its largest job's buffers - 2.4 GB at 16
/// threads. At 256k the big jobs' buffers were made again job after job
/// (the reference frame's region phase +20%); at 1M not (~25 MB a worker).
const SCRATCH_KEEP: usize = 1 << 20;

impl Scratch {
    /// Its buffers back to `SCRATCH_KEEP` bundles' room after a job that
    /// grew them past it.
    fn bound(&mut self) {
        if self.prov.capacity() > SCRATCH_KEEP {
            self.prov = Vec::new();
        }
        if self.tbs.capacity() > SCRATCH_KEEP {
            self.tbs = Vec::with_capacity(SCRATCH_KEEP);
            self.raw = Vec::new();
            self.enc.bound(SCRATCH_KEEP);
        }
        if self.s_idx.capacity() > SCRATCH_KEEP {
            self.s_idx = Default::default();
            self.s_list = Vec::new();
        }
    }

    /// Bytes its buffers hold (`--phases`' `[wavemem]`): the job's
    /// bundles, the encoder's, the raw block, the rest.
    pub fn alloc_bytes(&self) -> [usize; 4] {
        [std::mem::size_of::<bundle::Tb>() * self.tbs.capacity() + std::mem::size_of::<Bundle>() * self.prov.capacity(), self.enc.alloc_bytes(), self.raw.capacity(), 4 * (self.e_of.capacity() + self.s_of.capacity()) + 32 * self.s_idx.capacity() + 24 * self.s_list.capacity()]
    }
}

impl Chunk {
    /// Bytes it holds (its unit's shared sources not counted).
    pub fn alloc_bytes(&self) -> usize {
        24 * self.targets.capacity() + std::mem::size_of::<Ub>() * self.bundles.capacity() + 4 * self.src_regions.capacity()
    }
}

impl RouteState {
    /// Bytes its new-cell slots, provisional bundles and segments hold.
    pub fn alloc_bytes(&self) -> [usize; 3] {
        [12 * self.fresh.capacity() + 24 * self.fresh_raise.as_ref().map_or(0, |m| m.capacity()), self.prov.iter().map(|s| s.1.bytes.capacity()).sum::<usize>(), self.segs.iter().map(|s| s.1.bytes.capacity()).sum::<usize>()]
    }
}

impl Default for Scratch {
    fn default() -> Self {
        Scratch {
            prov: Vec::new(),
            e_of: Vec::new(),
            s_of: Vec::new(),
            s_idx: Default::default(),
            s_list: Vec::new(),
            tbs: Vec::new(),
            enc: bundle::Encoder::default(),
            raw: Vec::new(),
            z: zstd::bulk::Compressor::new(1).expect("a zstd compressor"),
        }
    }
}

/// THE REGION PHASE of one route over one slice's `chunks` (module doc):
/// per chunk target the dedup (one probe of the region's table, the cells
/// it lacked new states); per bundle its edges kept with the target's entry
/// and the route's source index as names; then the slice's segment encoded
/// - its bundles as the units made them (a key's bundles of two units hold
/// other cells; they stay two).
pub fn run_route(st: &mut RouteState, slice: u32, chunks: &[&Chunk], cx: &RegionCtx, sc: &mut Scratch) -> Result<()> {
    let n: usize = chunks.iter().map(|c| c.bundles.len()).sum();
    st.recs += n as u64;
    let record = cx.record;
    sc.tbs.clear();
    sc.s_idx.clear();
    sc.s_list.clear();
    for c in chunks {
        let remap: &[u32] = cx.remaps.get(c.worker as usize).map_or(&[], |m| m.as_slice());
        st.sources.extend(c.src_regions.iter().copied());
        // THE DEDUP, per target: its entry (made for a new key); the cells
        // it lacked are new states.
        sc.e_of.clear();
        for &(key, mask) in &c.targets {
            let e = match st.table.find(key) {
                Some(e) => e,
                None => st.table.push_key(super::GEOMETRY.words, key),
            };
            let had = st.table.or_cells(e, mask);
            let new = mask & !had;
            if new != 0 {
                st.fresh.push((e, new));
                st.new_states += new.count_ones() as u64;
                st.fresh_events += 1;
            }
            if let (Some(layers), Some(region)) = (cx.layers, st.region) {
                // States of the tree: a raise refuses one of a later layer.
                let made = st.fresh_raise.get_or_insert_with(Default::default);
                let before = made.get(&e).copied().unwrap_or(0);
                if new != 0 {
                    made.insert(e, before | new);
                }
                let mut old = mask & had & !before;
                while old != 0 {
                    let cell = old.trailing_zeros();
                    old &= old - 1;
                    let layer = layers.layer(region, e, cell);
                    ensure!(
                        layer <= cx.frame,
                        "frame {}: a state the tree first reached at frame {layer} is reached at frame {}: the raised tree would move it to an earlier layer (the level -1 table is not consistent along this edge), so it cannot be extended exactly - delete the tree",
                        cx.frame,
                        cx.frame
                    );
                }
            }
            // A provisional target's bundles wait for its key (the frame's end).
            sc.e_of.push(if is_provisional(key) { PROV } else { e });
        }
        if !record {
            continue;
        }
        // THE EDGES (a provisional target's wait for its key: the frame's end).
        sc.s_of.clear();
        sc.s_of.resize(c.sources.len(), u32::MAX);
        for b in &c.bundles {
            if b.x == NO_XFER {
                continue;
            }
            let x = *remap.get(b.x as usize).ok_or_else(|| anyhow::anyhow!("frame {}: a bundle's transfer {} of worker {} has no global id", cx.frame, b.x, c.worker))?;
            let e = sc.e_of[b.t as usize];
            if e == PROV {
                let (s_shape, s_key) = c.sources[b.s as usize];
                sc.prov.push(Bundle { s_shape, s_key, x, o: b.o, t_key: c.targets[b.t as usize].0, mask: b.mask });
                continue;
            }
            let s = match sc.s_of[b.s as usize] {
                u32::MAX => {
                    let src = c.sources[b.s as usize];
                    let n = sc.s_list.len() as u32;
                    let k = *sc.s_idx.entry(src).or_insert(n);
                    if k == n {
                        sc.s_list.push(src);
                    }
                    sc.s_of[b.s as usize] = k;
                    k
                }
                k => k,
            };
            sc.tbs.push(bundle::Tb { t: e, s, x, o: b.o, mask: b.mask });
        }
    }
    if !sc.tbs.is_empty() {
        let table = &st.table;
        sc.enc.encode(&sc.tbs, table.len(), |e| table.key(e), &sc.s_list, &mut sc.raw);
        let region = st.region.unwrap_or(u32::MAX);
        st.segs.push(((0, slice), Segment::of_raw(region, &sc.raw, sc.tbs.len() as u32, sc.tbs.iter().map(|t| t.mask.count_ones() as u64).sum(), &mut sc.z)?));
    }
    if !sc.prov.is_empty() {
        st.prov.push((slice, Segment::encode(u32::MAX, &mut sc.prov, &mut sc.raw, &mut sc.z)?));
        sc.prov.clear();
    }
    sc.bound();
    Ok(())
}

/// What a route's frame end gives the wave: its region and its NEW states
/// - the entries with cells new this frame, by key, and those cells.
pub struct Sealed {
    pub region: u32,
    pub keys: Vec<Key>,
    pub masks: Vec<u64>,
}

/// THE FRAME'S END of a route (its shape numbered: `region`): provisional
/// keys made final (`finals`), its new states listed by key (canonical: a
/// layer's order), the bundles into provisional targets encoded.
pub fn seal(st: &mut RouteState, region: u32, finals: &FxHashMap<Key, Key>, sc: &mut Scratch) -> Result<Sealed> {
    st.region = Some(region);
    for seg in &mut st.segs {
        seg.1.region = region;
    }
    st.table.finish_frame(st.start, |k| if is_provisional(k) { finals.get(&k).copied() } else { Some(k) })?;
    // The new cells per entry (an entry's events merged), by key.
    let mut events = std::mem::take(&mut st.fresh);
    events.sort_unstable_by_key(|x| x.0);
    let mut new: Vec<(Key, u64)> = Vec::with_capacity(events.len());
    let mut last = u32::MAX;
    for (e, m) in events {
        if e == last {
            new.last_mut().expect("pushed").1 |= m;
        } else {
            new.push((st.table.key(e), m));
            last = e;
        }
    }
    new.sort_unstable_by_key(|x| x.0);
    // Each job's bundles into provisional targets, decoded and encoded
    // again with the final keys (a segment a job, as it was made).
    let mut dz = zstd::bulk::Decompressor::new()?;
    let mut raw: Vec<u8> = Vec::new();
    for (slice, seg) in std::mem::take(&mut st.prov) {
        bundle::decompress(&seg.bytes, seg.raw as usize, &mut dz, &mut raw)?;
        let mut v: Vec<Bundle> = Vec::with_capacity(seg.bundles as usize);
        let mut missing = false;
        bundle::Block::open(&raw, &bundle::ORDER)?.bundles(|mut b| match finals.get(&b.t_key) {
            Some(&k) => {
                b.t_key = k;
                v.push(b);
            }
            None => missing = true,
        })?;
        ensure!(!missing, "a provisional target key without its final key");
        ensure!(v.len() == seg.bundles as usize, "a provisional segment of {} bundles decoded to {}", seg.bundles, v.len());
        st.segs.push(((1, slice), Segment::encode(region, &mut v, &mut sc.raw, &mut sc.z)?));
    }
    // The file's order: the jobs' segments by slice, then the provisional
    // ones by slice (one job a (route, slice): the keys are distinct).
    st.segs.sort_unstable_by_key(|s| s.0);
    st.fresh_raise = None;
    Ok(Sealed { region, keys: new.iter().map(|x| x.0).collect(), masks: new.iter().map(|x| x.1).collect() })
}
