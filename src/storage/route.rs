//! THE ROUTING of a wave's emissions to their TARGET regions, and the
//! REGION PHASE there (plans/edges-target.md "The forward").
//!
//! A unit (a run of frontier rows through the kernels) ORs each emission
//! into its BUNDLE (`bundle::Bundle`: source key, transfer, offset, target
//! key -> target cells) in a unit-local table (`UnitBundles`: one probe and
//! an OR, in cache), keyed also by the target's ROUTE - its (shape, region
//! slot): a storage region. An emission without an edge (the reference
//! engine's) is a bundle of its target alone (`NO_XFER`). At the unit's end
//! its bundles are the CHUNKS of their routes. The region phase then takes
//! the routes, heaviest first, one worker a route: per bundle ONE probe of
//! the region's table (the dedup: the cells the entry lacks are new states,
//! set at once) and ONE probe and an OR in the route's bundle table (the
//! edges: units' bundles of one key merge); the route's bundles are encoded
//! into a SEGMENT of the frame's edge file. No lookup happens in the units,
//! no state is named by a number, no row is copied: the new layer is
//! decoded from (shape, key, cell) at the frame's end (`wave`).

use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

use super::bundle::{self, Bundle};
use super::edges::Segment;
use super::visited::{Key, RegionTable};
use celeste_engine::exact::is_provisional;

/// A unit bundle's transfer when it holds no edge (its target alone).
pub const NO_XFER: u32 = u32::MAX;

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

/// A unit's bundles of one route (their transfers the worker's local ids),
/// and the source regions their edges leave.
pub struct Chunk {
    pub route: u32,
    pub unit: u32,
    pub worker: u32,
    pub bundles: Vec<Bundle>,
    pub sources: Vec<u32>,
}

/// A frontier row as an edge names its source: its shape's index, its
/// cell, its region, its key.
#[derive(Clone, Copy, Debug)]
pub struct Src {
    pub shape: u32,
    pub cell: u32,
    pub region: u32,
    pub key: Key,
}

/// The wave's frontier rows, by their index over its blocks.
pub struct Sources {
    pub rows: Vec<Src>,
    /// Per frontier block, its first row's index.
    pub starts: Vec<u32>,
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
    /// Per entry, its cells new this frame.
    pub fresh: Vec<u64>,
    /// Bundles into provisional targets: encoded at the frame's end, their
    /// keys final.
    pub prov: Vec<Bundle>,
    pub segs: Vec<Segment>,
    /// The source regions its edges leave.
    pub sources: FxHashSet<u32>,
    /// Records taken (emissions) and new states found.
    pub recs: u64,
    pub new_states: u64,
}

impl RouteState {
    pub fn new(shape: u64, slot: u32, region: Option<u32>, table: Option<Box<RegionTable>>) -> Self {
        let table = table.unwrap_or_default();
        let start = table.len() as u32;
        RouteState { shape, slot, region, fresh: vec![0; table.len()], table, start, prov: Vec::new(), segs: Vec::new(), sources: FxHashSet::default(), recs: 0, new_states: 0 }
    }
}

/// What the region phase reads besides its route.
pub struct RegionCtx<'a> {
    pub frame: u32,
    /// The frontier as states (`None`: no edges recorded).
    pub sources: Option<&'a Sources>,
    /// Per worker, its local transfer ids' global ones.
    pub remaps: &'a [Vec<u32>],
    /// A raise's per-state layers: a hit on a state of a later layer than
    /// the frame is refused.
    pub layers: Option<&'a super::wave::StateLayers>,
}

/// A bundle's slot (its fields compared exactly): odd multipliers per
/// word, one final mix.
#[inline]
fn bundle_hash(s_shape: u32, s_key: Key, x: u32, o: u32, t_key: Key) -> u64 {
    let h = t_key.1.wrapping_mul(0x9e37_79b9_7f4a_7c15)
        ^ t_key.0.wrapping_mul(0xc2b2_ae3d_27d4_eb4f)
        ^ s_key.1.wrapping_mul(0x1656_67b1_9e37_79f9).rotate_left(23)
        ^ s_key.0.wrapping_mul(0x85eb_ca77_c2b2_ae63).rotate_left(41)
        ^ ((x as u64) << 32 | o as u64).wrapping_mul(0x27d4_eb2f_1656_67c5)
        ^ (s_shape as u64).wrapping_mul(0xff51_afd7_ed55_8ccd);
    celeste_engine::runtime2::mix64(h)
}

/// A unit's bundles, by route: one probe and an OR an emission (the last
/// bundle remembered: a body's lanes mostly share it). Each route's bundles
/// in a bucket of their own, handed over whole at the unit's end.
#[derive(Default)]
pub struct UnitBundles {
    buckets: Vec<(u32, Vec<Bundle>)>,
    bucket_of: FxHashMap<u32, u32>,
    last_route: Option<(u32, u32)>,
    /// `bucket << 32 | position + 1`; 0 empty.
    index: Vec<u64>,
    n: usize,
    last: Option<(u32, u32)>,
}

#[inline]
fn unit_hash(route: u32, b: &Bundle) -> u64 {
    bundle_hash(b.s_shape ^ route.rotate_left(16), b.s_key, b.x, b.o, b.t_key)
}

impl UnitBundles {
    pub fn clear(&mut self) {
        if self.n > 0 {
            self.index.fill(0);
        }
        self.buckets.clear();
        self.bucket_of.clear();
        (self.last_route, self.last, self.n) = (None, None, 0);
    }

    /// OR `bit` into the bundle `(route; s_shape, s_key, x, o, t_key)`.
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn insert(&mut self, route: u32, s_shape: u32, s_key: Key, x: u32, o: u32, t_key: Key, bit: u64) {
        let probe = Bundle { s_shape, s_key, x, o, t_key, mask: 0 };
        let same = |b: &Bundle| b.t_key == t_key && b.s_key == s_key && b.x == x && b.o == o && b.s_shape == s_shape;
        if let Some((k, p)) = self.last {
            let (r, v) = &mut self.buckets[k as usize];
            if *r == route && same(&v[p as usize]) {
                v[p as usize].mask |= bit;
                return;
            }
        }
        let k = match self.last_route {
            Some((r, k)) if r == route => k,
            _ => {
                let k = match self.bucket_of.get(&route) {
                    Some(&k) => k,
                    None => {
                        self.buckets.push((route, Vec::new()));
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
        let mut i = unit_hash(route, &probe) as usize & m;
        loop {
            let e = self.index[i];
            if e == 0 {
                break;
            }
            let (bk, p) = ((e >> 32) as u32, (e as u32) - 1);
            if bk == k && same(&self.buckets[k as usize].1[p as usize]) {
                self.buckets[k as usize].1[p as usize].mask |= bit;
                self.last = Some((k, p));
                return;
            }
            i = (i + 1) & m;
        }
        let v = &mut self.buckets[k as usize].1;
        v.push(Bundle { mask: bit, ..probe });
        let p = v.len() as u32 - 1;
        self.index[i] = (k as u64) << 32 | (p as u64 + 1);
        self.last = Some((k, p));
        self.n += 1;
        if self.n * 2 > self.index.len() {
            self.reindex(self.index.len() * 2);
        }
    }

    fn reindex(&mut self, cap: usize) {
        self.index = vec![0; cap];
        let m = cap - 1;
        for (k, (route, v)) in self.buckets.iter().enumerate() {
            for (p, b) in v.iter().enumerate() {
                let mut i = unit_hash(*route, b) as usize & m;
                while self.index[i] != 0 {
                    i = (i + 1) & m;
                }
                self.index[i] = (k as u64) << 32 | (p as u64 + 1);
            }
        }
    }

    /// Clear the cells `drop(route, key, cell)` says (the coarser level's
    /// disallowed targets); a bundle left without cells goes.
    pub fn retain_cells(&mut self, drop: impl Fn(u32, Key, u32) -> bool) {
        for (route, v) in &mut self.buckets {
            for b in v.iter_mut() {
                let mut m = b.mask;
                while m != 0 {
                    let c = m.trailing_zeros();
                    m &= m - 1;
                    if drop(*route, b.t_key, c) {
                        b.mask &= !(1u64 << c);
                    }
                }
            }
            v.retain(|b| b.mask != 0);
        }
        self.n = self.buckets.iter().map(|b| b.1.len()).sum();
        self.reindex(self.index.len().max(1 << 12));
        self.last = None;
    }

    /// The bundles by route; the table cleared.
    pub fn drain(&mut self) -> Vec<(u32, Vec<Bundle>)> {
        let out = std::mem::take(&mut self.buckets);
        self.clear();
        out.into_iter().filter(|b| !b.1.is_empty()).collect()
    }
}

/// A worker's scratch for the region phase: the route's edges as the
/// encoder takes them, the encoder, the raw block, zstd.
pub struct Scratch {
    tbs: Vec<bundle::Tb>,
    enc: bundle::Encoder,
    raw: Vec<u8>,
    z: zstd::bulk::Compressor<'static>,
}

impl Default for Scratch {
    fn default() -> Self {
        Scratch { tbs: Vec::new(), enc: bundle::Encoder::default(), raw: Vec::new(), z: zstd::bulk::Compressor::new(1).expect("a zstd compressor") }
    }
}

/// THE REGION PHASE of one route over one slice's `chunks` (module doc):
/// per unit bundle the dedup (one probe of the region's table) and its
/// edges kept with the target's entry as its name; then the slice's
/// segment encoded - its bundles as the units made them (a key's bundles
/// of two units hold other cells; they stay two).
pub fn run_route(st: &mut RouteState, chunks: &[&Chunk], cx: &RegionCtx, sc: &mut Scratch) -> Result<()> {
    let n: usize = chunks.iter().map(|c| c.bundles.len()).sum();
    st.recs += n as u64;
    let record = cx.sources.is_some();
    sc.tbs.clear();
    // The last target's entry: consecutive bundles often share it.
    let (mut last_key, mut last_e) = (None::<Key>, 0u32);
    for c in chunks {
        let remap: &[u32] = cx.remaps.get(c.worker as usize).map_or(&[], |m| m.as_slice());
        st.sources.extend(c.sources.iter().copied());
        for b in &c.bundles {
            // THE DEDUP: the target's entry (made for a new key); the cells
            // it lacked are new states.
            let e = match last_key {
                Some(k) if k == b.t_key => last_e,
                _ => {
                    let e = match st.table.find(b.t_key) {
                        Some(e) => e,
                        None => {
                            st.fresh.push(0);
                            st.table.push_key(super::GEOMETRY.words, b.t_key)
                        }
                    };
                    (last_key, last_e) = (Some(b.t_key), e);
                    e
                }
            };
            let had = st.table.or_cells(e, b.mask);
            let new = b.mask & !had;
            st.fresh[e as usize] |= new;
            st.new_states += new.count_ones() as u64;
            if let (Some(layers), Some(region)) = (cx.layers, st.region) {
                // States of the tree: a raise refuses one of a later layer.
                let mut old = b.mask & had & !st.fresh[e as usize];
                while old != 0 {
                    let c = old.trailing_zeros();
                    old &= old - 1;
                    let layer = layers.layer(super::state_id(region, e, c));
                    ensure!(
                        layer <= cx.frame,
                        "frame {}: a state the tree first reached at frame {layer} is reached at frame {}: the raised tree would move it to an earlier layer (the level -1 table is not consistent along this edge), so it cannot be extended exactly - delete the tree",
                        cx.frame,
                        cx.frame
                    );
                }
            }
            // THE EDGES (a provisional target's wait for its key: the frame's end).
            if record && b.x != NO_XFER {
                let x = *remap.get(b.x as usize).ok_or_else(|| anyhow::anyhow!("frame {}: a bundle's transfer {} of worker {} has no global id", cx.frame, b.x, c.worker))?;
                if is_provisional(b.t_key) {
                    st.prov.push(Bundle { x, ..*b });
                } else {
                    sc.tbs.push(bundle::Tb { t: e, x, o: b.o, s_shape: b.s_shape, s_key: b.s_key, mask: b.mask });
                }
            }
        }
    }
    if !sc.tbs.is_empty() {
        let table = &st.table;
        sc.enc.encode(&sc.tbs, table.len(), |e| table.key(e), &mut sc.raw);
        let region = st.region.unwrap_or(u32::MAX);
        st.segs.push(Segment::of_raw(region, &sc.raw, sc.tbs.len() as u32, sc.tbs.iter().map(|t| t.mask.count_ones() as u64).sum(), &mut sc.z)?);
    }
    Ok(())
}

/// What a route's frame end gives the wave: its region's new entries
/// `(region, entry, key)` and its new states by id.
pub struct Sealed {
    pub entries: Vec<(u32, u32, Key)>,
    pub states: Vec<super::StateId>,
}

/// THE FRAME'S END of a route (its shape numbered: `region`): provisional
/// keys made final (`finals`), the frame's new entries numbered in key
/// order (canonical: a function of the frame's new states), its new states
/// listed by id, the bundles into provisional targets encoded.
pub fn seal(st: &mut RouteState, region: u32, finals: &FxHashMap<Key, Key>, sc: &mut Scratch) -> Result<Sealed> {
    let words = st.table_words();
    st.region = Some(region);
    for seg in &mut st.segs {
        seg.region = region;
    }
    let order = st.table.finish_frame(words, st.start, |k| if is_provisional(k) { finals.get(&k).copied() } else { Some(k) })?;
    // `fresh` follows the entries' new numbers.
    let tail: Vec<u64> = order.iter().map(|&e| st.fresh[e as usize]).collect();
    st.fresh.truncate(st.start as usize);
    st.fresh.extend(tail);
    let entries = (st.start..st.table.len() as u32).map(|e| (region, e, st.table.key(e))).collect();
    let mut states = Vec::new();
    for (e, &m) in st.fresh.iter().enumerate() {
        let mut m = m;
        while m != 0 {
            let c = m.trailing_zeros();
            m &= m - 1;
            states.push(super::state_id(region, e as u32, c));
        }
    }
    if !st.prov.is_empty() {
        let mut v: Vec<Bundle> = std::mem::take(&mut st.prov);
        for b in &mut v {
            b.t_key = *finals.get(&b.t_key).ok_or_else(|| anyhow::anyhow!("a provisional target key without its final key"))?;
        }
        st.segs.push(Segment::encode(region, &mut v, &mut sc.raw, &mut sc.z)?);
    }
    Ok(Sealed { entries, states })
}

impl RouteState {
    fn table_words(&self) -> usize {
        super::GEOMETRY.words
    }
}
