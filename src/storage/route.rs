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
//! distinct targets, its sources, the bundles naming both by index, and the
//! PUSHES - per target cell the hulls of its sources pushed along the
//! bundles (`unit::UnitSink::pushes`). The region phase (`run_route`) takes
//! one route's chunks of one slice as a JOB: every push merged into the
//! route's TOUCHED table (per target (key, cell) its hull so far: order-free,
//! `search::hull::HullAcc`); the job's bundles encoded into a SEGMENT of the
//! frame's edge file. Bundles of one key from two units stay two (their
//! cells differ). The re-pushes of the states expanded before (`wave`)
//! merge into the same tables. At the frame's end each route compares its
//! touched states with the visited set (`seal`): NEW states, states whose
//! hull GREW, the rest nothing. No lookup happens in the units, no state is
//! named by a number, no row is copied.

use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

use super::bundle::{self, Bundle};
use super::edges::Segment;
use super::set::RegionTable;
use super::visited::{Key, StateInfo, VisitedSet};
use super::Sk;
use crate::search::hull::{Hull, HullAcc};
use celeste_engine::exact::is_provisional;

/// A unit bundle's transfer when it holds no edge (its target alone).
pub const NO_XFER: u32 = u32::MAX;

/// `Scratch::e_of`'s mark of a provisional target.
const PROV: u32 = u32::MAX;

/// A route's TOUCHED states this frame: per key pushed into (one probe of
/// `index` a key, as the dedup's), the cells pushed and the RUN of their
/// hulls so far in `slots` (by cell; a run moves to the end when its key
/// gains a cell).
#[derive(Default)]
pub struct Touched {
    index: FxHashMap<Key, u32>,
    /// Per key: the key, its cells, its run's start.
    keys: Vec<(Key, u64, u32)>,
    slots: Vec<HullAcc>,
    states: usize,
}

impl Touched {
    /// Merge `cells` (by cell, ascending: `(cell, hull)`) into key `key`'s.
    #[inline]
    pub fn push(&mut self, key: Key, cells: &[(u8, HullAcc)]) {
        let n = self.keys.len() as u32;
        let e = *self.index.entry(key).or_insert(n);
        if e == n {
            self.keys.push((key, 0, self.slots.len() as u32));
        }
        let (_, mask, start) = self.keys[e as usize];
        let add = cells.iter().fold(0u64, |m, x| m | 1u64 << x.0);
        let rank = |m: u64, c: u8| (m & ((1u64 << c) - 1)).count_ones() as usize;
        if add & !mask == 0 {
            for &(c, ref a) in cells {
                self.slots[start as usize + rank(mask, c)].merge(a);
            }
            return;
        }
        // The key's run with the new cells, at the end.
        let next = mask | add;
        let at = self.slots.len();
        let mut m = next;
        while m != 0 {
            let c = m.trailing_zeros() as u8;
            m &= m - 1;
            self.slots.push(if mask >> c & 1 == 1 { self.slots[start as usize + rank(mask, c)] } else { HullAcc::EMPTY });
        }
        for &(c, ref a) in cells {
            self.slots[at + rank(next, c)].merge(a);
        }
        self.states += (add & !mask).count_ones() as usize;
        self.keys[e as usize] = (key, next, at as u32);
    }

    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// States pushed into.
    pub fn len(&self) -> usize {
        self.states
    }

    /// The keys pushed into.
    pub fn keys(&self) -> impl Iterator<Item = Key> + '_ {
        self.keys.iter().map(|k| k.0)
    }

    /// The keys pushed into, each `key(k)` (a provisional key made final:
    /// distinct keys stay distinct), sorted: per key its cells and its
    /// run's start in the hulls so far (by cell).
    pub fn into_sorted(self, key: impl Fn(Key) -> Result<Key>) -> Result<(Vec<(Key, u64, u32)>, Vec<HullAcc>)> {
        let mut keys = self.keys;
        for k in &mut keys {
            k.0 = key(k.0)?;
        }
        keys.sort_unstable_by_key(|k| k.0);
        ensure!(keys.windows(2).all(|w| w[0].0 != w[1].0), "two touched keys made one final key");
        Ok((keys, self.slots))
    }

    /// Bytes held.
    pub fn alloc_bytes(&self) -> usize {
        self.index.capacity() * (std::mem::size_of::<(Key, u32)>() + 1) + self.keys.capacity() * std::mem::size_of::<(Key, u64, u32)>() + self.slots.capacity() * std::mem::size_of::<HullAcc>()
    }
}

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
/// reached), its PUSHES (target, cell, the hull pushed there; the cells no
/// push reached have none), the unit's distinct sources (shared by its
/// chunks), the bundles naming both by index (their transfers the worker's
/// local ids), and the source regions their edges leave.
pub struct Chunk {
    pub route: u32,
    pub unit: u32,
    pub worker: u32,
    pub targets: Vec<(Key, u64)>,
    pub pushes: Vec<(u32, u8, HullAcc)>,
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

/// A route through a frame: its touched states, its edges' segments.
pub struct RouteState {
    pub shape: u64,
    pub slot: u32,
    /// The visited set's region, once the shape is numbered.
    pub region: Option<u32>,
    /// The states pushed into this frame, with their pushed hulls (compared
    /// with the visited set at the frame's end, `seal`; the visited set is
    /// read only).
    pub touched: Touched,
    /// The provisional keys it was pushed into or its bundles name (keys
    /// made final at the frame's end: a target pushed nothing still has
    /// its edges).
    pub prov_keys: FxHashSet<Key>,
    /// The bundles that WAIT for the frame's end: into a provisional
    /// target (a code new this frame: no final key yet) or with a transfer
    /// new this frame (no canonical id yet: `edges::XferTable::canonical`),
    /// per job (slice) a segment encoded with the provisional keys and ids,
    /// encoded again at the frame's end with the final ones (`seal`). Held
    /// as zstd blocks, not bundles: the mod room's f53 (+95) held 103M into
    /// provisional targets, 5.8 GB as structs.
    pub prov: Vec<(u32, Segment)>,
    /// Its segments with their kind (0: a job's, 1: a job's bundles that
    /// waited, made final) and slice: jobs of a route may run out of slice
    /// order; the file has them in this one (`seal`).
    pub segs: Vec<((u8, u32), Segment)>,
    /// The source regions its edges leave.
    pub sources: FxHashSet<u32>,
    /// Records taken (emissions) and edge pushes merged.
    pub recs: u64,
    pub pushes: u64,
}

impl RouteState {
    pub fn new(shape: u64, slot: u32, region: Option<u32>) -> Self {
        RouteState { shape, slot, region, touched: Touched::default(), prov_keys: FxHashSet::default(), prov: Vec::new(), segs: Vec::new(), sources: FxHashSet::default(), recs: 0, pushes: 0 }
    }

    /// Does the frame's end owe it anything: states pushed into, or edges?
    pub fn is_used(&self) -> bool {
        !self.touched.is_empty() || !self.segs.is_empty() || !self.prov.is_empty()
    }
}

/// What the region phase reads besides its route.
pub struct RegionCtx<'a> {
    pub frame: u32,
    /// Are edges recorded?
    pub record: bool,
    /// Per worker, its local transfer ids' global ones.
    pub remaps: &'a [Vec<u32>],
    /// The global ids from here on are this frame's new transfers', in the
    /// order the slices met them: provisional until the frame's end.
    pub new_xfers: u32,
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

/// A worker's scratch for the region phase: a chunk's targets' names and
/// sources' indices, the route's source list, its edges as the encoder
/// takes them, the encoder, the raw block, zstd.
pub struct Scratch {
    /// The job's bundles into provisional targets.
    prov: Vec<Bundle>,
    /// The job's targets: their names in the job (dense, as met) by key,
    /// and their keys - so the encoder's arrays are the job's size, not its
    /// region's (a region of 24M entries made each job's ranks 96 MB: 2 GB
    /// over 16 workers on the mod room).
    t_loc: rustc_hash2::FxHashMap<Key, u32>,
    t_keys: Vec<Key>,
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
        if self.t_loc.capacity() > SCRATCH_KEEP {
            self.t_loc = Default::default();
            self.t_keys = Vec::new();
        }
    }

    /// Bytes its buffers hold (`--phases`' `[wavemem]`): the job's
    /// bundles, the encoder's, the raw block, the rest.
    pub fn alloc_bytes(&self) -> [usize; 4] {
        [std::mem::size_of::<bundle::Tb>() * self.tbs.capacity() + std::mem::size_of::<Bundle>() * self.prov.capacity(), self.enc.alloc_bytes(), self.raw.capacity(), 4 * (self.e_of.capacity() + self.s_of.capacity()) + 32 * self.s_idx.capacity() + 24 * self.s_list.capacity() + 9 * self.t_loc.capacity() + 16 * self.t_keys.capacity()]
    }
}

impl Chunk {
    /// Bytes it holds (its unit's shared sources not counted).
    pub fn alloc_bytes(&self) -> usize {
        24 * self.targets.capacity() + std::mem::size_of::<(u32, u8, HullAcc)>() * self.pushes.capacity() + std::mem::size_of::<Ub>() * self.bundles.capacity() + 4 * self.src_regions.capacity()
    }
}

impl RouteState {
    /// Bytes its touched table, provisional bundles and segments hold.
    pub fn alloc_bytes(&self) -> [usize; 3] {
        [self.touched.alloc_bytes() + 24 * self.prov_keys.capacity(), self.prov.iter().map(|s| s.1.bytes.capacity()).sum::<usize>(), self.segs.iter().map(|s| s.1.bytes.capacity()).sum::<usize>()]
    }
}

impl Default for Scratch {
    fn default() -> Self {
        Scratch {
            prov: Vec::new(),
            t_loc: Default::default(),
            t_keys: Vec::new(),
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
/// every push merged into the route's touched states; per bundle its edges
/// kept with the target's name in the job and the route's source index;
/// then the slice's segment encoded - its bundles as the units made them (a
/// key's bundles of two units hold other cells; they stay two). Edges are
/// recorded only with `cx.record`.
#[inline(never)]
pub fn run_route(st: &mut RouteState, slice: u32, chunks: &[&Chunk], cx: &RegionCtx, sc: &mut Scratch) -> Result<()> {
    let n: usize = chunks.iter().map(|c| c.bundles.len()).sum();
    st.recs += n as u64;
    let record = cx.record;
    sc.tbs.clear();
    sc.s_idx.clear();
    sc.s_list.clear();
    sc.t_loc.clear();
    sc.t_keys.clear();
    for c in chunks {
        // THE PUSHES.
        let mut i = 0usize;
        let mut cells: Vec<(u8, HullAcc)> = Vec::new();
        while i < c.pushes.len() {
            let t = c.pushes[i].0;
            cells.clear();
            while i < c.pushes.len() && c.pushes[i].0 == t {
                cells.push((c.pushes[i].1, c.pushes[i].2));
                i += 1;
            }
            let key = c.targets[t as usize].0;
            if is_provisional(key) {
                st.prov_keys.insert(key);
            }
            st.touched.push(key, &cells);
        }
        st.pushes += c.pushes.len() as u64;
        if !record {
            continue;
        }
        let remap: &[u32] = cx.remaps.get(c.worker as usize).map_or(&[], |m| m.as_slice());
        st.sources.extend(c.src_regions.iter().copied());
        // The targets named in the job (a provisional one's bundles wait for
        // its key: the frame's end).
        sc.e_of.clear();
        for &(key, _) in &c.targets {
            sc.e_of.push(if is_provisional(key) {
                st.prov_keys.insert(key);
                PROV
            } else {
                let n = sc.t_keys.len() as u32;
                let l = *sc.t_loc.entry(key).or_insert(n);
                if l == n {
                    sc.t_keys.push(key);
                }
                l
            });
        }
        // THE EDGES (a provisional target's wait for its key, a new
        // transfer's for its id: the frame's end).
        sc.s_of.clear();
        sc.s_of.resize(c.sources.len(), u32::MAX);
        for b in &c.bundles {
            if b.x == NO_XFER {
                continue;
            }
            let x = *remap.get(b.x as usize).ok_or_else(|| anyhow::anyhow!("frame {}: a bundle's transfer {} of worker {} has no global id", cx.frame, b.x, c.worker))?;
            let e = sc.e_of[b.t as usize];
            if e == PROV || x >= cx.new_xfers {
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
        let keys = &sc.t_keys;
        sc.enc.encode(&sc.tbs, keys.len(), |l| keys[l as usize], &sc.s_list, &mut sc.raw);
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

/// What a route's frame end gives the wave: its region, its NEW states
/// (frozen, by key) with their hulls in the table's state order, and the
/// states whose hull GREW (with their layers and new hulls).
pub struct Sealed {
    pub region: u32,
    pub table: RegionTable,
    pub hulls: Vec<Hull>,
    pub grown: Vec<super::wave::Old>,
}

/// The frame's new transfers' canonical ids (`edges::XferTable::
/// canonical`): an id from `first` on is `first + perm[id - first]`.
pub struct Renumber<'a> {
    pub first: u32,
    pub perm: &'a [u32],
}

impl Renumber<'_> {
    #[inline]
    fn id(&self, x: u32) -> u32 {
        if x < self.first {
            x
        } else {
            self.first + self.perm[(x - self.first) as usize]
        }
    }
}

/// THE FRAME'S END of a route (its shape numbered: `region`): provisional
/// keys made final (`finals`); each touched state's hull - its pushes', and
/// the visited set's - compared with the visited set's (`visited`, read
/// only): a state it lacks is NEW (frozen, sorted by key: canonical), one
/// whose hull grew GREW; the bundles that waited encoded with their final
/// keys and transfer ids (`renumber`: `None` where the frame's new
/// transfers are in order already).
pub fn seal(st: &mut RouteState, region: u32, finals: &FxHashMap<Key, Key>, renumber: Option<&Renumber>, visited: &VisitedSet, sc: &mut Scratch) -> Result<Sealed> {
    st.region = Some(region);
    for seg in &mut st.segs {
        seg.1.region = region;
    }
    // THE STATES (final keys, sorted): new, grown, or nothing.
    let final_key = |k: Key| if is_provisional(k) { finals.get(&k).copied().ok_or_else(|| anyhow::anyhow!("a provisional key without its final key")) } else { Ok(k) };
    let (touched, accs) = std::mem::take(&mut st.touched).into_sorted(final_key)?;
    let vt = visited.table(region);
    let (mut keys, mut masks, mut hulls): (Vec<Key>, Vec<u64>, Vec<Hull>) = (Vec::new(), Vec::new(), Vec::new());
    let mut grown: Vec<super::wave::Old> = Vec::new();
    for (key, cells, start) in touched {
        let (ve, held) = vt.and_then(|t| t.find(key).map(|ve| (ve, t.mask(ve)))).unwrap_or((0, 0));
        let fresh = cells & !held;
        if fresh != 0 {
            keys.push(key);
            masks.push(fresh);
        }
        for (i, c) in super::layer::bits(cells).enumerate() {
            let acc = accs[start as usize + i];
            if held >> c & 1 == 0 {
                hulls.push(acc.hull().ok_or_else(|| anyhow::anyhow!("r{region}: a touched state pushed nothing"))?);
                continue;
            }
            // No hull stored: it is FULL, and grows no more.
            let Some(StateInfo { hull, layer }) = visited.info(region, ve, c) else { continue };
            let mut a = acc;
            a.add_hull(&hull);
            let h = a.hull().expect("a hull holds points");
            if h != hull {
                grown.push(super::wave::Old { sk: Sk { region, key, local: c as u8 }, layer, hull: h, was: hull });
            }
        }
    }
    let table = RegionTable::from_sorted(&keys, &masks)?;
    // Each job's bundles that waited, decoded and encoded again with the
    // final keys and ids (a segment a job, as it was made).
    let mut dz = zstd::bulk::Decompressor::new()?;
    let mut raw: Vec<u8> = Vec::new();
    for (slice, seg) in std::mem::take(&mut st.prov) {
        bundle::decompress(&seg.bytes, seg.raw as usize, &mut dz, &mut raw)?;
        let mut v: Vec<Bundle> = Vec::with_capacity(seg.bundles as usize);
        let mut missing = false;
        bundle::Block::open(&raw, &bundle::ORDER)?.bundles(|mut b| {
            if is_provisional(b.t_key) {
                match finals.get(&b.t_key) {
                    Some(&k) => b.t_key = k,
                    None => missing = true,
                }
            }
            if let Some(rn) = renumber {
                b.x = rn.id(b.x);
            }
            v.push(b);
        })?;
        ensure!(!missing, "a provisional target key without its final key");
        ensure!(v.len() == seg.bundles as usize, "a waiting segment of {} bundles decoded to {}", seg.bundles, v.len());
        st.segs.push(((1, slice), Segment::encode(region, &mut v, &mut sc.raw, &mut sc.z)?));
    }
    // The file's order: the jobs' segments by slice, then the ones that
    // waited by slice (one job a (route, slice): the keys are distinct).
    st.segs.sort_unstable_by_key(|s| s.0);
    Ok(Sealed { region, table, hulls, grown })
}
