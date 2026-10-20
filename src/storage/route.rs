//! THE ROUTES (a target's shape and region slot) and the REGION PHASE
//! (plans/architecture.md "The forward: hulls and storage"): a JOB
//! (`run_route`) merges one route's chunks of one slice - every push, the
//! re-pushes' too, into its TOUCHED table (order-free: `HullAcc`), its
//! bundles into a SEGMENT of the edge file (one key's from two units stay
//! two). At the frame's end the touched states are sealed: NEW, GROWN, or
//! nothing.

use crate::search::hull::{AccOf, HullOf};
use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

use super::bundle::{self, Bundle};
use super::edges::Segment;
use super::set::{PackedTable, PlainTable};
use super::visited::{Key, StateInfo, VisitedSet};
use super::Sk;
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::exact::is_provisional;

/// `JobScratch::e_of`'s mark of a provisional target.
const PROV: u32 = u32::MAX;

/// A route's TOUCHED states this frame: per key (open addressing in
/// `index`, at most 3/4 full) its cells and the RUN of their hulls in
/// `slots`, by cell (moved to the end when the key gains a cell; compacted
/// once the dead pass half the live; arrays grow by an eighth): ~30 B a state.
pub(crate) struct Touched<N: Rem = P8> {
    index: Vec<u32>,
    keys: Vec<TouchedKey>,
    slots: Vec<AccOf<N>>,
    states: usize,
    dead: usize,
}

impl<N: Rem> Default for Touched<N> {
    fn default() -> Self {
        Touched { index: Vec::new(), keys: Vec::new(), slots: Vec::new(), states: 0, dead: 0 }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TouchedKey {
    pub(crate) key: Key,
    pub(crate) cells: u64,
    pub(crate) start: u32,
}

/// Room in `v` for `more`, grown by an eighth at least (not doubled: a
/// frame's touched tables are its biggest transient).
#[inline]
fn grow<T>(v: &mut Vec<T>, more: usize) {
    if v.len() + more > v.capacity() {
        v.reserve_exact(more.max(v.len() / 8).max(64));
    }
}

#[inline]
fn key_hash(k: Key) -> u64 {
    celeste_engine::runtime2::mix64(k.0.wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ k.1)
}

impl<N: Rem> Touched<N> {
    /// Key `key`'s place in `keys`, made (no cell yet) if absent.
    #[inline]
    fn place(&mut self, key: Key) -> u32 {
        if (self.keys.len() + 1) * 4 > self.index.len() * 3 {
            let cap = ((self.keys.len() + 1) * 2).next_power_of_two().max(64);
            self.index = vec![0; cap];
            let m = cap - 1;
            for (e, k) in self.keys.iter().enumerate() {
                let mut i = key_hash(k.key) as usize & m;
                while self.index[i] != 0 {
                    i = (i + 1) & m;
                }
                self.index[i] = e as u32 + 1;
            }
        }
        let m = self.index.len() - 1;
        let mut i = key_hash(key) as usize & m;
        loop {
            match self.index[i] {
                0 => {
                    grow(&mut self.keys, 1);
                    self.keys.push(TouchedKey { key, cells: 0, start: self.slots.len() as u32 });
                    self.index[i] = self.keys.len() as u32;
                    return self.keys.len() as u32 - 1;
                }
                s if self.keys[s as usize - 1].key == key => return s - 1,
                _ => i = (i + 1) & m,
            }
        }
    }

    /// Merge `cells` (`(cell, hull)`, ascending) into key `key`'s.
    #[inline]
    pub(crate) fn push(&mut self, key: Key, cells: &[(u8, AccOf<N>)]) {
        let e = self.place(key);
        let TouchedKey { cells: mask, start, .. } = self.keys[e as usize];
        let add = cells.iter().fold(0u64, |m, x| m | 1u64 << x.0);
        let rank = |m: u64, c: u8| (m & ((1u64 << c) - 1)).count_ones() as usize;
        if add & !mask == 0 {
            for &(c, ref a) in cells {
                self.slots[start as usize + rank(mask, c)].merge(a);
            }
            return;
        }
        let next = mask | add;
        let at = self.slots.len();
        grow(&mut self.slots, next.count_ones() as usize);
        let mut m = next;
        while m != 0 {
            let c = m.trailing_zeros() as u8;
            m &= m - 1;
            self.slots.push(if mask >> c & 1 == 1 { self.slots[start as usize + rank(mask, c)] } else { AccOf::<N>::default() });
        }
        for &(c, ref a) in cells {
            self.slots[at + rank(next, c)].merge(a);
        }
        self.states += (add & !mask).count_ones() as usize;
        self.dead += mask.count_ones() as usize;
        (self.keys[e as usize].cells, self.keys[e as usize].start) = (next, at as u32);
        if self.dead > 4096 && self.dead * 2 > self.states {
            self.compact();
        }
    }

    /// The live runs, key by key, at the start of new slots.
    fn compact(&mut self) {
        let mut slots = Vec::with_capacity(self.states + self.states / 8);
        for k in &mut self.keys {
            let n = k.cells.count_ones() as usize;
            let a = slots.len() as u32;
            slots.extend_from_slice(&self.slots[k.start as usize..k.start as usize + n]);
            k.start = a;
        }
        self.slots = slots;
        self.dead = 0;
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// States pushed into.
    pub(crate) fn len(&self) -> usize {
        self.states
    }

    /// The keys, each `key(k)` (made final: distinct keys stay distinct),
    /// sorted, and the hulls (each key's run by cell).
    pub(crate) fn into_sorted(self, key: impl Fn(Key) -> Result<Key>) -> Result<(Vec<TouchedKey>, Vec<AccOf<N>>)> {
        let mut keys = self.keys;
        for k in &mut keys {
            k.key = key(k.key)?;
        }
        keys.sort_unstable_by_key(|k| k.key);
        ensure!(keys.windows(2).all(|w| w[0].key != w[1].key), "two touched keys made one final key");
        Ok((keys, self.slots))
    }

}

/// The wave's routes: (shape, region slot) -> a dense id, made as the units
/// emit (read-mostly: a worker caches its own).
#[derive(Default)]
pub(crate) struct Routes {
    map: RwLock<FxHashMap<(u64, u32), u32>>,
    list: RwLock<Vec<(u64, u32)>>,
}

impl Routes {
    pub(crate) fn id(&self, shape: u64, slot: u32) -> u32 {
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
    pub(crate) fn list(&self) -> Vec<(u64, u32)> {
        self.list.read().expect("routes").clone()
    }
}

/// A unit's bundles of one route: its TARGETS (key, cells), PUSHES (target,
/// cell, hull), the unit's sources (shared), the bundles naming both by
/// index, the source regions their edges leave.
pub(crate) struct Chunk<N: Rem = P8> {
    pub(crate) route: u32,
    pub(crate) unit: u32,
    pub(crate) worker: u32,
    pub(crate) targets: Vec<(Key, u64)>,
    pub(crate) pushes: Vec<(u32, u8, AccOf<N>)>,
    pub(crate) sources: std::sync::Arc<[(u32, Key)]>,
    pub(crate) bundles: Vec<Ub>,
    pub(crate) src_regions: Vec<u32>,
}

/// A unit bundle (24 B): its target's and source's index in its chunk, its
/// transfer (the worker's local id), offset and target cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Ub {
    pub(crate) t: u32,
    pub(crate) s: u32,
    pub(crate) x: u32,
    pub(crate) o: u32,
    pub(crate) mask: u64,
}

pub(crate) struct RouteState<N: Rem = P8> {
    pub(crate) shape: u64,
    pub(crate) slot: u32,
    /// The visited set's region, once the shape is numbered.
    pub(crate) region: Option<u32>,
    pub(crate) touched: Touched<N>,
    /// The provisional keys pushed into or named by its bundles (a target
    /// nothing was pushed into still has its edges).
    pub(crate) prov_keys: FxHashSet<Key>,
    /// The bundles that WAIT for the frame's end (a provisional target, or a
    /// new transfer: no canonical id yet), per job (slice) a segment,
    /// re-encoded with the final keys and ids (`reencode`). Compressed: a
    /// frame can hold ~100M.
    pub(crate) prov: Vec<(u32, Segment)>,
    /// Its segments by (kind (0: a job's, 1: waited, made final), slice):
    /// jobs run out of slice order; the edge file sorts them by this.
    pub(crate) segs: Vec<((u8, u32), Segment)>,
    /// The source regions its edges leave.
    pub(crate) sources: FxHashSet<u32>,
    /// Records taken (emissions) and edge pushes merged.
    pub(crate) recs: u64,
    pub(crate) pushes: u64,
}

impl<N: Rem> RouteState<N> {
    pub(crate) fn new(shape: u64, slot: u32, region: Option<u32>) -> Self {
        RouteState { shape, slot, region, touched: Touched::default(), prov_keys: FxHashSet::default(), prov: Vec::new(), segs: Vec::new(), sources: FxHashSet::default(), recs: 0, pushes: 0 }
    }

    /// Does the frame's end owe it anything: states pushed into, or edges?
    pub(crate) fn is_used(&self) -> bool {
        !self.touched.is_empty() || !self.segs.is_empty() || !self.prov.is_empty()
    }
}

pub(crate) struct RegionCtx<'a> {
    pub(crate) frame: u32,
    pub(crate) record: bool,
    /// Per worker, its local transfer ids' global ones.
    pub(crate) remaps: &'a [Vec<u32>],
    /// Ids from here on are the frame's new transfers', in slice order:
    /// provisional until the frame's end.
    pub(crate) new_xfers: u32,
    pub(crate) spool: Option<&'a super::scratch::Scratch>,
}

/// A unit's bundles, by route: an emission is APPENDED to its route's
/// bucket (ORed into the last bundle if the same); `drain` merges each
/// bucket in a table its size (cache-resident, where one table over the
/// unit missed the cache on most appends).
#[derive(Default)]
pub(crate) struct UnitBundles {
    buckets: Vec<(u32, Vec<(Key, Ub)>)>,
    bucket_of: FxHashMap<u32, u32>,
    last_route: Option<(u32, u32)>,
    /// `drain`'s merge table (`index + 1` into the bucket; 0 empty), and
    /// its target index.
    slots: Vec<u32>,
    t_idx: rustc_hash2::FxHashMap<Key, u32>,
    /// Emptied buckets kept, at most `SPARE_KEEP` bundles' room (unbounded,
    /// every bucket grows to the largest route's).
    spare: Vec<Vec<(Key, Ub)>>,
}

/// The bundles' room `UnitBundles` keeps in its emptied buckets (20 MB).
const SPARE_KEEP: usize = 1 << 19;

#[inline]
fn ub_hash(t_key: Key, b: &Ub) -> u64 {
    let h = t_key.1.wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ t_key.0.wrapping_mul(0xc2b2_ae3d_27d4_eb4f) ^ (b.s as u64).wrapping_mul(0x1656_67b1_9e37_79f9) ^ ((b.x as u64) << 32 | b.o as u64).wrapping_mul(0x85eb_ca77_c2b2_ae63);
    celeste_engine::runtime2::mix64(h)
}

impl UnitBundles {

    pub(crate) fn clear(&mut self) {
        let mut room: usize = self.spare.iter().map(Vec::capacity).sum();
        for (_, mut v) in self.buckets.drain(..) {
            if room + v.capacity() <= SPARE_KEEP {
                room += v.capacity();
                v.clear();
                self.spare.push(v);
            }
        }
        self.bucket_of.clear();
        self.last_route = None;
    }

    /// OR `bit` into bundle `(route, t_key; s, x, o)` (merged at `drain`).
    #[inline]
    pub(crate) fn insert(&mut self, route: u32, t_key: Key, s: u32, x: u32, o: u32, bit: u64) {
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
        let v = &mut self.buckets[k as usize].1;
        if let Some(l) = v.last_mut() {
            if l.0 == t_key && l.1.s == s && l.1.x == x && l.1.o == o {
                l.1.mask |= bit;
                return;
            }
        }
        v.push((t_key, Ub { t: 0, s, x, o, mask: bit }));
    }

    /// `v`'s bundles MERGED in place: each (target key, s, x, o) once, at
    /// its first place (the first `n` returned), its cells ORed.
    fn merge(slots: &mut Vec<u32>, v: &mut [(Key, Ub)]) -> usize {
        let cap = (2 * v.len()).next_power_of_two().max(16);
        slots.clear();
        slots.resize(cap, 0);
        let m = cap - 1;
        let mut n = 0usize;
        for r in 0..v.len() {
            let (k, u) = v[r];
            let mut i = ub_hash(k, &u) as usize & m;
            loop {
                let e = slots[i];
                if e == 0 {
                    v[n] = (k, u);
                    n += 1;
                    slots[i] = n as u32;
                    break;
                }
                let g = &mut v[e as usize - 1];
                if g.0 == k && g.1.s == u.s && g.1.x == u.x && g.1.o == u.o {
                    g.1.mask |= u.mask;
                    break;
                }
                i = (i + 1) & m;
            }
        }
        n
    }

    /// The buckets as `(route, targets, bundles)`, merged, targets named by
    /// index, the cells `drop(route, key, cell)` says cleared first (refused
    /// targets; a bundle left without cells goes).
    #[allow(clippy::type_complexity)]
    pub(crate) fn drain(&mut self, drop: Option<&dyn Fn(u32, Key, u32) -> bool>) -> Vec<(u32, Vec<(Key, u64)>, Vec<Ub>)> {
        let mut buckets = std::mem::take(&mut self.buckets);
        let mut out = Vec::with_capacity(buckets.len());
        for (route, v) in &mut buckets {
            let route = *route;
            let n = Self::merge(&mut self.slots, v);
            self.t_idx.clear();
            let mut targets: Vec<(Key, u64)> = Vec::new();
            let mut bundles: Vec<Ub> = Vec::with_capacity(n);
            let mut last: Option<(Key, u32)> = None;
            for &(t_key, mut u) in &v[..n] {
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
        if self.slots.capacity() > 2 * SPARE_KEEP {
            self.slots = Vec::new();
        }
        self.buckets = buckets;
        self.clear();
        out
    }
}

pub(crate) struct JobScratch {
    /// The job's bundles into provisional targets.
    prov: Vec<Bundle>,
    /// The job's targets named densely, as met (and their keys): the
    /// encoder's arrays the job's size, not its region's (up to 24M).
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

/// Bundles a worker's region scratch keeps room for between jobs (~25 MB):
/// heavy jobs are millions of bundles; with less, their buffers are remade.
const SCRATCH_KEEP: usize = 1 << 20;

impl JobScratch {
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

}

impl Default for JobScratch {
    fn default() -> Self {
        JobScratch {
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

/// THE REGION PHASE of one route over one slice's `chunks`: the pushes into
/// its touched states; with `cx.record`, the slice's segment (some bundles
/// wait: `RouteState::prov`).
#[inline(never)]
pub(crate) fn run_route<N: Rem>(st: &mut RouteState<N>, slice: u32, chunks: &[&Chunk<N>], cx: &RegionCtx, sc: &mut JobScratch) -> Result<()> {
    let n: usize = chunks.iter().map(|c| c.bundles.len()).sum();
    st.recs += n as u64;
    let record = cx.record;
    sc.tbs.clear();
    sc.s_idx.clear();
    sc.s_list.clear();
    sc.t_loc.clear();
    sc.t_keys.clear();
    for c in chunks {
        let mut i = 0usize;
        let mut cells: Vec<(u8, AccOf<N>)> = Vec::new();
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
        // The targets named in the job (`PROV`: a provisional one).
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
        sc.s_of.clear();
        sc.s_of.resize(c.sources.len(), u32::MAX);
        for b in &c.bundles {
            // A worker's local id to its global one (`UnitSink::base`).
            let x = match b.x.checked_sub(cx.new_xfers) {
                None => b.x,
                Some(l) => *remap.get(l as usize).ok_or_else(|| anyhow::anyhow!("frame {}: a bundle's transfer {} of worker {} has no global id", cx.frame, b.x, c.worker))?,
            };
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
        let mut seg = Segment::of_raw(region, &sc.raw, sc.tbs.len() as u32, sc.tbs.iter().map(|t| t.mask.count_ones() as u64).sum(), &mut sc.z)?;
        seg.bytes = seg.bytes.spill(cx.spool)?;
        st.segs.push(((0, slice), seg));
    }
    if !sc.prov.is_empty() {
        st.prov.push((slice, Segment::encode(u32::MAX, &sc.prov, &mut sc.enc, &mut sc.raw, &mut sc.z)?));
        sc.prov.clear();
    }
    sc.bound();
    Ok(())
}

/// A route's frame end: its NEW states (by key) with their hulls in the
/// table's order, and the states whose hull GREW.
pub(crate) struct Sealed<N: Rem = P8> {
    pub(crate) region: u32,
    pub(crate) table: PlainTable,
    pub(crate) hulls: Vec<HullOf<N>>,
    pub(crate) grown: Vec<super::wave::Old<N>>,
    /// Times to sort, compare (summed), make the table.
    pub(crate) t: [std::time::Duration; 3],
    /// The bundles that waited (their segments, bundles: `reencode`).
    pub(crate) waited: (usize, u64),
}

/// The frame's new transfers' canonical ids (`edges::XferTable::
/// canonical`): an id from `first` on is `first + perm[id - first]`.
pub(crate) struct Renumber<'a> {
    pub(crate) first: u32,
    pub(crate) perm: &'a [u32],
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

/// A route's FRAME END, first step (its shape numbered: `region`): keys made
/// final (`finals`), the touched keys sorted; then `seal_part` and `sealed`.
pub(crate) fn seal_sort<N: Rem>(st: &mut RouteState<N>, region: u32, finals: &FxHashMap<Key, Key>) -> Result<Touch<N>> {
    st.region = Some(region);
    for seg in &mut st.segs {
        seg.1.region = region;
    }
    let final_key = |k: Key| if is_provisional(k) { finals.get(&k).copied().ok_or_else(|| anyhow::anyhow!("a provisional key without its final key")) } else { Ok(k) };
    let t = std::time::Instant::now();
    let (touched, accs) = std::mem::take(&mut st.touched).into_sorted(final_key)?;
    let waited = (st.prov.len(), st.prov.iter().map(|x| x.1.bundles as u64).sum());
    Ok(Touch { region, touched, accs, waited, t_sort: t.elapsed() })
}

pub(crate) struct Touch<N: Rem = P8> {
    pub(crate) region: u32,
    pub(crate) touched: Vec<TouchedKey>,
    pub(crate) accs: Vec<AccOf<N>>,
    waited: (usize, u64),
    t_sort: std::time::Duration,
}

/// Touched keys a seal part takes.
pub(crate) const SEAL_PART: usize = 1 << 15;

/// A `seal_part`: its new states (keys, cells, hulls), grown.
pub(crate) struct SealPart<N: Rem = P8> {
    keys: Vec<Key>,
    masks: Vec<u64>,
    hulls: Vec<HullOf<N>>,
    grown: Vec<super::wave::Old<N>>,
    t: std::time::Duration,
}

/// The touched keys `part` COMPARED with the visited set's region (a
/// `Cursor`, each block decoded once): a state it lacks NEW, a hull grown GREW.
pub(crate) fn seal_part<N: Rem>(touch: &Touch<N>, part: std::ops::Range<usize>, visited: &VisitedSet<N>) -> Result<SealPart<N>> {
    let t = std::time::Instant::now();
    let (region, accs) = (touch.region, &touch.accs);
    let (mut keys, mut masks, mut hulls): (Vec<Key>, Vec<u64>, Vec<HullOf<N>>) = (Vec::new(), Vec::new(), Vec::new());
    let mut grown: Vec<super::wave::Old<N>> = Vec::new();
    let mut cursor = visited.table(region).map(PackedTable::cursor::<N>);
    for &TouchedKey { key, cells, start } in &touch.touched[part] {
        let entry = match cursor.as_mut() {
            Some(c) => c.seek(key)?,
            None => None,
        };
        let held = entry.as_ref().map_or(0, |e| e.mask);
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
            let Some(StateInfo { hull, layer }) = entry.as_ref().and_then(|e| e.info(c)) else { continue };
            let mut a = acc;
            a.add_hull(&hull);
            let h = a.hull().expect("a hull holds points");
            if h != hull {
                grown.push(super::wave::Old { sk: Sk { region, key, local: c as u8 }, layer, hull: h, was: hull });
            }
        }
    }
    Ok(SealPart { keys, masks, hulls, grown, t: t.elapsed() })
}

/// A route's SEALED states from its parts (in order): the new ones by key
/// (canonical).
pub(crate) fn sealed<N: Rem>(touch: Touch<N>, parts: Vec<SealPart<N>>) -> Result<Sealed<N>> {
    let t = std::time::Instant::now();
    let n = |f: fn(&SealPart<N>) -> usize| parts.iter().map(f).sum::<usize>();
    let (mut keys, mut masks, mut hulls, mut grown) = (Vec::with_capacity(n(|p| p.keys.len())), Vec::with_capacity(n(|p| p.keys.len())), Vec::with_capacity(n(|p| p.hulls.len())), Vec::with_capacity(n(|p| p.grown.len())));
    let mut t_compare = std::time::Duration::ZERO;
    for p in parts {
        keys.extend(p.keys);
        masks.extend(p.masks);
        hulls.extend(p.hulls);
        grown.extend(p.grown);
        t_compare += p.t;
    }
    let table = PlainTable::from_sorted(&keys, &masks)?;
    Ok(Sealed { region: touch.region, table, hulls, grown, t: [touch.t_sort, t_compare, t.elapsed()], waited: touch.waited })
}

/// A WAITED segment re-encoded for `region` with final keys and transfer ids
/// (`renumber`: `None` where the new transfers are in order already).
pub(crate) fn reencode(seg: &Segment, region: u32, finals: &FxHashMap<Key, Key>, renumber: Option<&Renumber>, enc: &mut bundle::Encoder, z: &mut zstd::bulk::Compressor<'static>) -> Result<Segment> {
    let (mut dz, mut raw) = (zstd::bulk::Decompressor::new()?, Vec::new());
    let super::scratch::Body::Mem(bytes) = &seg.bytes else { anyhow::bail!("a waiting segment spooled") };
    super::file::decompress(bytes, seg.raw as usize, &mut dz, &mut raw)?;
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
    let mut out = Vec::new();
    Segment::encode(region, &v, enc, &mut out, z)
}
