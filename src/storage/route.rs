//! THE ROUTING of a wave's emissions to their TARGET regions, and the
//! REGION PHASE there (plans/edges-target.md "The forward").
//!
//! A unit (a run of frontier rows through the kernels) turns each emission
//! into a RECORD - the target's key and cell, the source (its index in the
//! wave's frontier), the transfer - appended to the unit's bucket of the
//! target's ROUTE (its (shape, region slot): a storage region). At the
//! unit's end each bucket is a CHUNK for its route. The region phase then
//! takes the routes, heaviest first, one worker a route: per record ONE
//! probe of the region's table (the dedup: a cell the entry lacks is a new
//! state, set at once) and ONE probe and an OR in the route's bundle table
//! (the edge, `bundle::Bundle`); the route's bundles are encoded into a
//! SEGMENT of the frame's edge file. No lookup happens in the units, no
//! state is named by a number, no row is copied: the new layer is decoded
//! from (shape, key, cell) at the frame's end (`wave`).

use anyhow::{ensure, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::RwLock;

use super::bundle::{self, Bundle};
use super::edges::Segment;
use super::visited::{Key, RegionTable};
use super::{Geometry, Sk};
use celeste_engine::exact::{is_provisional, slot_hash};

/// One emission's record (24 B): the target's key, its source's index in
/// the wave's frontier (`Sources`), and `transfer << 8 | target cell in its
/// region` - the transfer the worker's local id (`NO_XFER`: no edge, the
/// reference engine's emissions).
#[derive(Clone, Copy, Debug)]
pub struct Rec {
    pub key: Key,
    pub src: u32,
    pub xl: u32,
}

/// `Rec::xl`'s transfer of an emission without an edge.
pub const NO_XFER: u32 = (1 << 24) - 1;

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

/// A unit's records of one route.
pub struct Chunk {
    pub route: u32,
    pub unit: u32,
    pub worker: u32,
    pub recs: Vec<Rec>,
}

/// The wave's frontier rows as states, by their index over its blocks
/// (`Rec::src`).
pub struct Sources {
    pub rows: Vec<Sk>,
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
    pub geo: Geometry,
    pub frame: u32,
    /// The frontier as states (`None`: no edges recorded).
    pub sources: Option<&'a Sources>,
    /// Per worker, its local transfer ids' global ones.
    pub remaps: &'a [Vec<u32>],
    /// A raise's per-state layers: a hit on a state of a later layer than
    /// the frame is refused.
    pub layers: Option<&'a super::wave::StateLayers>,
}

/// A worker's scratch for the region phase: the bundle table, the
/// encoder's buffers.
pub struct Scratch {
    ents: Vec<Bundle>,
    index: Vec<u32>,
    raw: Vec<u8>,
    z: zstd::bulk::Compressor<'static>,
}

impl Default for Scratch {
    fn default() -> Self {
        Scratch { ents: Vec::new(), index: vec![0; 1 << 10], raw: Vec::new(), z: zstd::bulk::Compressor::new(1).expect("a zstd compressor") }
    }
}

#[inline]
fn bundle_hash(s_shape: u32, s_key: Key, x: u32, o: u32, t_key: Key) -> u64 {
    use celeste_engine::runtime2::mix64;
    mix64(slot_hash(t_key) ^ slot_hash(s_key).rotate_left(17) ^ ((s_shape as u64) << 40 | (x as u64) << 8) ^ (o as u64).rotate_left(31))
}

impl Scratch {
    fn clear(&mut self, records: usize) {
        self.ents.clear();
        let cap = (records / 2).next_power_of_two().clamp(1 << 10, 1 << 26);
        if self.index.len() != cap {
            self.index = vec![0; cap];
        } else {
            self.index.fill(0);
        }
    }

    /// OR `bit` into the bundle `(s_shape, s_key, x, o, t_key)`; its entry.
    #[inline]
    fn insert(&mut self, s_shape: u32, s_key: Key, x: u32, o: u32, t_key: Key, bit: u64) -> usize {
        let m = self.index.len() - 1;
        let mut i = bundle_hash(s_shape, s_key, x, o, t_key) as usize & m;
        loop {
            let e = self.index[i];
            if e == 0 {
                break;
            }
            let b = &mut self.ents[e as usize - 1];
            if b.t_key == t_key && b.s_key == s_key && b.x == x && b.o == o && b.s_shape == s_shape {
                b.mask |= bit;
                return e as usize - 1;
            }
            i = (i + 1) & m;
        }
        self.ents.push(Bundle { s_shape, s_key, x, o, t_key, mask: bit });
        self.index[i] = self.ents.len() as u32;
        if self.ents.len() * 2 > self.index.len() {
            self.grow();
        }
        self.ents.len() - 1
    }

    fn grow(&mut self) {
        let cap = self.index.len() * 2;
        self.index = vec![0; cap];
        let m = cap - 1;
        for (k, b) in self.ents.iter().enumerate() {
            let mut i = bundle_hash(b.s_shape, b.s_key, b.x, b.o, b.t_key) as usize & m;
            while self.index[i] != 0 {
                i = (i + 1) & m;
            }
            self.index[i] = k as u32 + 1;
        }
    }
}

/// THE REGION PHASE of one route over one slice's `chunks` (module doc).
pub fn run_route(st: &mut RouteState, chunks: &[&Chunk], cx: &RegionCtx, sc: &mut Scratch) -> Result<()> {
    let geo = cx.geo;
    let words = geo.words;
    let n: usize = chunks.iter().map(|c| c.recs.len()).sum();
    st.recs += n as u64;
    let record = cx.sources.is_some();
    if record {
        sc.clear(n);
    }
    // The last target's entry and the last bundle: consecutive records
    // mostly share both (a source's lanes, a body's slice).
    let (mut last_key, mut last_e) = (None::<Key>, 0u32);
    let mut last_src_region = u32::MAX;
    let mut last_bundle: Option<((u32, Key, u32, u32, Key), usize)> = None;
    for c in chunks {
        let remap = cx.remaps.get(c.worker as usize);
        for r in &c.recs {
            let local = r.xl & 0xff;
            // THE DEDUP: the target's entry (made for a new key), its cell.
            let e = match last_key {
                Some(k) if k == r.key => last_e,
                _ => {
                    let e = match st.table.find(r.key) {
                        Some(e) => e,
                        None => {
                            st.fresh.push(0);
                            st.table.push_key(words, r.key)
                        }
                    };
                    (last_key, last_e) = (Some(r.key), e);
                    e
                }
            };
            let bit = 1u64 << local;
            if st.table.set(words, e, local) {
                st.fresh[e as usize] |= bit;
                st.new_states += 1;
            } else if let (Some(layers), Some(region)) = (cx.layers, st.region) {
                if st.fresh[e as usize] & bit == 0 {
                    // A state of the tree: a raise refuses one of a later layer.
                    let layer = layers.layer(super::state_id(region, e, local));
                    ensure!(
                        layer <= cx.frame,
                        "frame {}: a state the tree first reached at frame {layer} is reached at frame {}: the raised tree would move it to an earlier layer (the level -1 table is not consistent along this edge), so it cannot be extended exactly - delete the tree",
                        cx.frame,
                        cx.frame
                    );
                }
            }
            // THE EDGE.
            let x = r.xl >> 8;
            if let (Some(sources), true) = (cx.sources, x != NO_XFER) {
                let s = sources.rows[r.src as usize];
                let x = remap.and_then(|m| m.get(x as usize)).copied().ok_or_else(|| anyhow::anyhow!("frame {}: a record's transfer {x} of worker {} has no global id", cx.frame, c.worker))?;
                let o = bundle::offset(s.cell(&geo), geo.cell(st.slot, local));
                let sh = s.shape(&geo);
                match last_bundle {
                    Some((k, e)) if k == (sh, s.key, x, o, r.key) => sc.ents[e].mask |= bit,
                    _ => last_bundle = Some(((sh, s.key, x, o, r.key), sc.insert(sh, s.key, x, o, r.key, bit))),
                }
                if s.region != last_src_region {
                    last_src_region = s.region;
                    st.sources.insert(s.region);
                }
            }
        }
    }
    if !record || sc.ents.is_empty() {
        return Ok(());
    }
    // Bundles into provisional targets wait for their keys (the frame's end).
    let ents = std::mem::take(&mut sc.ents);
    let (prov, mut done): (Vec<Bundle>, Vec<Bundle>) = ents.into_iter().partition(|b| is_provisional(b.t_key));
    st.prov.extend(prov);
    if !done.is_empty() {
        let region = st.region.unwrap_or(u32::MAX);
        st.segs.push(Segment::encode(region, &mut done, &mut sc.raw, &mut sc.z)?);
    }
    done.clear();
    sc.ents = done;
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
        // A bundle of two slices: its masks united.
        v.sort_unstable_by_key(|b| (b.s_shape, b.s_key, b.x, b.o, b.t_key));
        let mut merged: Vec<Bundle> = Vec::with_capacity(v.len());
        for b in v {
            match merged.last_mut() {
                Some(l) if (l.s_shape, l.s_key, l.x, l.o, l.t_key) == (b.s_shape, b.s_key, b.x, b.o, b.t_key) => l.mask |= b.mask,
                _ => merged.push(b),
            }
        }
        st.segs.push(Segment::encode(region, &mut merged, &mut sc.raw, &mut sc.z)?);
    }
    Ok(Sealed { entries, states })
}

impl RouteState {
    fn table_words(&self) -> usize {
        super::GEOMETRY.words
    }
}
