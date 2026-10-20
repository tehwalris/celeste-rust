//! THE UNIT SINK (`UnitSink`, one per worker, reset per unit): emissions
//! ORed into BUNDLES by their target's route, nothing looked up or copied;
//! at the unit's end the chunks of their routes, each with its PUSHES
//! (plans/architecture.md "The forward: hulls and storage").

use crate::search::hull::{AccOf, HullOf};
use anyhow::Result;
use celeste_core::arith::EngineNum;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Col, Rt2, AV};
use rustc_hash::{FxHashMap, FxHashSet};

use super::decode::Skeletons;
use super::route::{Chunk, Routes, UnitBundles};
use super::visited::{Key, VisitedSet};
use crate::frame::Filters;
use crate::search::rem::Rem;
use celeste_engine::exact::{is_provisional, KeySpace, Provisional, RowKey};

/// A typed varying column of buffered rows: raw words, (low, high) pairs,
/// or a byte per bool (0 false, 1 true, 2 unknown).
pub(crate) enum TCol<N: EngineNum = P8> {
    Num(Vec<N::Raw>),
    Ival(Vec<(N::Raw, N::Raw)>),
    Bool(Vec<u8>),
}

/// Rows of one shape, columnar over its skeleton, buffered only to be looked
/// at: the coarser level's verdicts, the key check, the decode guard.
pub(crate) struct RowBuf<N: EngineNum = P8> {
    pub(crate) shape: u64,
    skeleton: Rt2<N>,
    /// `(cell, column)` per varying cell, in the kernel's order.
    pub(crate) cols: Vec<(usize, TCol<N>)>,
    pub(crate) keys: Vec<Key>,
    pub(crate) cells: Vec<u32>,
}

impl<N: EngineNum> RowBuf<N> {
    pub(crate) fn new(skeleton: Rt2<N>) -> Self {
        let cols = skeleton
            .cols
            .iter()
            .enumerate()
            .filter_map(|(cell, c)| match c {
                Col::N(_) => Some((cell, TCol::Num(Vec::new()))),
                Col::I(_) => Some((cell, TCol::Ival(Vec::new()))),
                Col::V(_) => Some((cell, TCol::Bool(Vec::new()))),
                Col::U(_) => None,
            })
            .collect::<Vec<_>>();
        RowBuf { shape: skeleton.shape_hash, skeleton, cols, keys: Vec::new(), cells: Vec::new() }
    }

    pub(crate) fn rows(&self) -> usize {
        self.keys.len()
    }

    /// Drop the rows, keeping the skeleton and the columns' capacity.
    pub(crate) fn clear(&mut self) {
        for (_, c) in &mut self.cols {
            match c {
                TCol::Num(v) => v.clear(),
                TCol::Ival(v) => v.clear(),
                TCol::Bool(v) => v.clear(),
            }
        }
        self.keys.clear();
        self.cells.clear();
    }

    pub(crate) fn to_rt2(&self) -> Rt2<N> {
        let mut b = celeste_engine::slots::reshape(&self.skeleton, 0);
        b.cols = self
            .skeleton
            .cols
            .iter()
            .map(|c| match c {
                Col::N(_) => Col::N(Vec::new()),
                Col::I(_) => Col::I(Vec::new()),
                Col::V(_) => Col::V(Vec::new()),
                u => u.clone(),
            })
            .collect();
        b.shape_hash = self.shape;
        for (cell, col) in &self.cols {
            b.cols[*cell] = match col {
                TCol::Num(v) => Col::N(v.iter().map(|&x| num::<N>(x)).collect()),
                TCol::Ival(v) => Col::I(v.iter().map(|&(a, c)| (num::<N>(a), num::<N>(c))).collect()),
                TCol::Bool(v) => Col::V(
                    v.iter()
                        .map(|&x| match x {
                            0 => AV::Bool(false),
                            1 => AV::Bool(true),
                            _ => AV::UBool,
                        })
                        .collect(),
                ),
            };
        }
        b.row_keys = self.keys.clone();
        b.width = self.rows();
        b
    }

    /// Push one materialized row (the reference engine's path).
    pub(crate) fn push_row(&mut self, row: &Rt2<N>, key: Key, cell: u32) {
        debug_assert_eq!(row.width, 1);
        for (c, col) in self.cols.iter_mut() {
            let v = row.cols[*c].at(0);
            match (col, v) {
                (TCol::Num(d), AV::Num(n)) => d.push(n.raw()),
                (TCol::Ival(d), AV::Ival(a, b)) => d.push((a.raw(), b.raw())),
                (TCol::Ival(d), AV::Num(n)) => d.push((n.raw(), n.raw())),
                (TCol::Bool(d), AV::Bool(x)) => d.push(x as u8),
                (TCol::Bool(d), AV::UBool) => d.push(2),
                (_, v) => panic!("emitted row's cell {c} holds {v:?}, not its column's kind"),
            }
        }
        self.keys.push(key);
        self.cells.push(cell);
    }
}

/// The number of a buffered raw word (a buffer's own: always one).
#[inline]
fn num<N: EngineNum>(r: N::Raw) -> N {
    N::from_raw(r).expect("a buffered word is a number")
}

/// A target's verdict under the coarser level's filter (`UnitSink::decide`).
const VERDICT_PENDING: u8 = 0;
const VERDICT_YES: u8 = 1;
const VERDICT_NO: u8 = 2;
/// Rows awaiting a verdict before a batch is decided.
const VERDICT_BATCH: usize = 1024;

/// Entries of `UnitSink::xfer_id_raw`'s cache.
const XFER_CACHE: usize = 1 << 12;
/// Entries of `UnitSink::src_seen` (a power of two).
const SRC_SEEN: usize = 1 << 8;

/// The lanes of a slice mask, lowest first.
#[inline]
pub(crate) fn lanes_of(mut m: u16) -> impl Iterator<Item = usize> {
    std::iter::from_fn(move || {
        (m != 0).then(|| {
            let i = m.trailing_zeros() as usize;
            m &= m - 1;
            i
        })
    })
}

/// A slice's TARGETS in one cell column of one shape (once per column, not
/// per body): per lane cell, route, bit, source (index, region), offset.
#[derive(Clone, Copy)]
pub(crate) struct Targets {
    pub(crate) cells: [u32; 16],
    route: [u32; 16],
    local: [u8; 16],
    offset: [u32; 16],
    src: [u32; 16],
    s_region: [u32; 16],
}

impl Default for Targets {
    fn default() -> Self {
        Targets { cells: [0; 16], route: [0; 16], local: [0; 16], offset: [0; 16], src: [0; 16], s_region: [0; 16] }
    }
}

/// ONE BODY's emissions of a kernel slice: lane `i` of `take` is `(shape,
/// keys[i], targets.cells[i])`, from unit lane `lanes[i]` by `xfers[i]`.
pub(crate) struct BodyEmits<'e> {
    pub(crate) shape: u64,
    pub(crate) take: u16,
    pub(crate) keys: &'e [Key; 16],
    pub(crate) targets: &'e Targets,
    pub(crate) xfers: &'e [u32; 16],
}

/// A (route, key, cell) target as the coarser level's verdicts name it.
type Target = (u32, Key, u8);

/// A unit's SOURCES: its entries `(region, key)`, each one's index among
/// its distinct `(shape, key)`s (`unit_sources`), per lane its entry, cell
/// and hull; the lanes by source: per source its first entry, per entry its
/// next of the same source (`u32::MAX`: none), slot, cells and first lane -
/// a bundle's source cell is a lane by its rank in its entry's cells.
struct Sources<N: Rem> {
    srcs: Vec<(u32, Key)>,
    src_local: Vec<u32>,
    lanes: Vec<(u32, u32)>,
    hulls: Vec<HullOf<N>>,
    unit_sources: Vec<(u32, Key)>,
    src_idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    src_first: Vec<u32>,
    entry_next: Vec<(u32, u32, u64, u32)>,
}

impl<N: Rem> Default for Sources<N> {
    fn default() -> Self {
        Sources { srcs: Vec::new(), src_local: Vec::new(), lanes: Vec::new(), hulls: Vec::new(), unit_sources: Vec::new(), src_idx: Default::default(), src_first: Vec::new(), entry_next: Vec::new() }
    }
}

impl<N: Rem> Sources<N> {
    fn clear(&mut self) {
        self.srcs.clear();
        self.src_local.clear();
        self.lanes.clear();
        self.hulls.clear();
        self.unit_sources.clear();
        self.src_idx.clear();
    }

    #[inline]
    fn push_source(&mut self, region: u32, key: Key) -> u32 {
        let s = (region / super::GEOMETRY.slots, key);
        let n = self.unit_sources.len() as u32;
        let k = *self.src_idx.entry(s).or_insert(n);
        if k == n {
            self.unit_sources.push(s);
        }
        self.srcs.push((region, key));
        self.src_local.push(k);
        self.srcs.len() as u32 - 1
    }

    #[inline]
    fn push_lane(&mut self, entry: u32, cell: u32, hull: HullOf<N>) {
        self.lanes.push((entry, cell));
        self.hulls.push(hull);
    }

    /// The sources and lanes are in: the lanes by source and cell.
    fn done(&mut self) {
        let geo = super::GEOMETRY;
        self.src_first.clear();
        self.src_first.resize(self.unit_sources.len(), u32::MAX);
        self.entry_next.clear();
        self.entry_next.resize(self.srcs.len(), (u32::MAX, 0, 0, 0));
        // Lanes of an entry are consecutive, its cells ascending.
        for (lane, &(entry, cell)) in self.lanes.iter().enumerate() {
            let (slot, local) = geo.slot_local(cell);
            let x = &mut self.entry_next[entry as usize];
            if x.2 == 0 {
                (x.1, x.3) = (slot, lane as u32);
            }
            x.2 |= 1u64 << local;
        }
        for e in (0..self.srcs.len()).rev() {
            let s = self.src_local[e] as usize;
            self.entry_next[e].0 = self.src_first[s];
            self.src_first[s] = e as u32;
        }
    }

    /// The lane of source `s` at `cell` (of unit `unit`).
    #[inline]
    fn lane(&self, unit: u32, s: u32, cell: u32) -> usize {
        let (slot, local) = super::GEOMETRY.slot_local(cell);
        let mut e = self.src_first[s as usize];
        while e != u32::MAX {
            let (next, sl, mask, lane) = self.entry_next[e as usize];
            if sl == slot && mask >> local & 1 == 1 {
                return (lane + (mask & ((1u64 << local) - 1)).count_ones()) as usize;
            }
            e = next;
        }
        panic!("unit {unit}: a bundle's source cell {cell} of source {s} is no lane of the unit")
    }

    /// The EDGES of bundle `b` (target slot `slot`) by the unit ENTRY they
    /// leave: `f(entry, target cells, shift to their sources)`; into or out
    /// of `NO_CELL`, one cell at a time and no shift (`lane`).
    #[inline]
    fn by_entry(&self, unit: u32, slot: u32, b: &super::route::Ub, mut f: impl FnMut(u32, u64, Option<i32>)) {
        use super::bundle::{shift_into, source_cell, TO_NONE};
        let geo = super::GEOMETRY;
        let none = geo.no_cell_slot();
        if b.o < TO_NONE && slot != none {
            let mut left = b.mask;
            let mut e = self.src_first[b.s as usize];
            while e != u32::MAX && left != 0 {
                let (next, sl, _, _) = self.entry_next[e as usize];
                if let Some((inside, k)) = (sl != none).then(|| shift_into(slot, sl, b.o)).flatten() {
                    let part = left & inside;
                    if part != 0 {
                        left &= !part;
                        f(e, part, Some(k));
                    }
                }
                e = next;
            }
            assert!(left == 0, "unit {unit}: a bundle's source cells in no entry of the unit");
            return;
        }
        let mut m = b.mask;
        while m != 0 {
            let c = m.trailing_zeros();
            m &= m - 1;
            let s_slot = geo.slot_local(source_cell(geo.cell(slot, c), b.o)).0;
            let mut e = self.src_first[b.s as usize];
            while e != u32::MAX && self.entry_next[e as usize].1 != s_slot {
                e = self.entry_next[e as usize].0;
            }
            assert!(e != u32::MAX, "unit {unit}: a bundle's source cell in no entry of the unit");
            f(e, 1u64 << c, None);
        }
    }
}

/// TRANSFER IDS: a pair in the tree's table (`base`, read only) is its
/// global id; a new pair is this worker's LOCAL id `l`, named `first_new +
/// l`, made global by the gather's remaps. Only new pairs cost memory.
struct Xfers<'a, N: Rem> {
    base: &'a super::edges::XferTable<N>,
    first_new: u32,
    ids: FxHashMap<N::Pair, u32>,
    /// Per local id its decoded transfers (the pushes').
    pairs: Vec<crate::search::rem::Xfer<N>>,
    cache: Vec<(crate::compiled::asm_kernel::RawWords<N>, u32)>,
    /// The worker's table of local transfers (the gather reads it), its length.
    tab: &'a std::sync::RwLock<Vec<N::Pair>>,
    len: u32,
}

impl<'a, N: Rem> Xfers<'a, N> {
    fn new(base: &'a super::edges::XferTable<N>, tab: &'a std::sync::RwLock<Vec<N::Pair>>) -> Self {
        Xfers { base, first_new: u32::try_from(base.pairs.len()).expect("transfers past u32"), ids: Default::default(), pairs: Vec::new(), cache: Vec::new(), tab, len: 0 }
    }

    /// `id` through a direct-mapped raw -> id cache: few distinct transfers
    /// a frame, one lookup an emitted lane.
    #[inline]
    fn id_raw(&mut self, worker: u32, words: &crate::compiled::asm_kernel::RawWords<N>, decode: impl FnOnce(&crate::compiled::asm_kernel::RawWords<N>) -> N::Pair) -> u32 {
        let mut h = 0u64;
        for pair in words.chunks_exact(2) {
            // 16.16's word is `lo << 32 | hi` (`EngineNum::word`, `fold`).
            h = (h ^ N::fold(N::word(pair[0], pair[1]))).wrapping_mul(0x9e37_79b9_7f4a_7c15).rotate_left(29);
        }
        let slot = (h as usize) & (XFER_CACHE - 1);
        if self.cache.is_empty() {
            self.cache = vec![([N::Raw::default(); crate::compiled::asm_kernel::RAW_WORDS], u32::MAX); XFER_CACHE];
        }
        let (w, id) = &self.cache[slot];
        if *id != u32::MAX && w == words {
            return *id;
        }
        let id = self.id(worker, decode(words));
        self.cache[slot] = (*words, id);
        id
    }

    /// The pair `t`'s id: its global one if the tree's table holds it, else
    /// this worker's local one (interned on first use).
    fn id(&mut self, worker: u32, t: N::Pair) -> u32 {
        if let Some(g) = self.base.find(&t) {
            return g;
        }
        if let Some(&id) = self.ids.get(&t) {
            return id;
        }
        let id = self.first_new.checked_add(self.len).unwrap_or_else(|| panic!("worker {worker}: {} transfers past the table's {}", self.len, self.first_new));
        self.tab.write().expect("a transfer table").push(t);
        self.len += 1;
        self.ids.insert(t, id);
        self.pairs.push(N::xfer(&t));
        id
    }

    /// Transfer `x`'s decoded pair (the pushes').
    #[inline]
    fn get(&self, x: u32) -> crate::search::rem::Xfer<N> {
        match x.checked_sub(self.first_new) {
            Some(l) => self.pairs[l as usize],
            None => N::xfer(&self.base.pairs[x as usize]),
        }
    }
}

/// This worker's ROUTES (and their slots), and the last one asked.
struct RouteIds<'a> {
    routes: &'a Routes,
    ids: FxHashMap<(u64, u32), u32>,
    slots: FxHashMap<u32, (u64, u32)>,
    last: (u64, u32, u32),
}

impl<'a> RouteIds<'a> {
    fn new(routes: &'a Routes) -> Self {
        RouteIds { routes, ids: FxHashMap::default(), slots: FxHashMap::default(), last: (0, u32::MAX, u32::MAX) }
    }

    #[inline]
    fn id(&mut self, shape: u64, slot: u32) -> u32 {
        if (self.last.0, self.last.1) == (shape, slot) {
            return self.last.2;
        }
        let r = match self.ids.get(&(shape, slot)) {
            Some(&r) => r,
            None => {
                let r = self.routes.id(shape, slot);
                self.ids.insert((shape, slot), r);
                self.slots.insert(r, (shape, slot));
                r
            }
        };
        self.last = (shape, slot, r);
        r
    }

    /// A route this worker made: its (shape, slot).
    fn slot(&self, route: u32) -> (u64, u32) {
        *self.slots.get(&route).expect("a route this worker made")
    }
}

/// The objects ladder's VERDICTS at emission (`MarkFilter`): per target,
/// once a unit, decided in batches (a projection per block, not per row).
struct Verdicts<'a, N: Rem> {
    marks: Option<&'a crate::frame::MarkFilter<'a, N>>,
    of: rustc_hash2::FxHashMap<Target, u8>,
    /// The rows awaiting a verdict, per shape, and their targets.
    bufs: Vec<RowBuf<N>>,
    at: Vec<Vec<Target>>,
    /// A target of the unit was disallowed: `end` drops its records.
    disallowed: bool,
}

impl<N: Rem> Verdicts<'_, N> {
    fn clear(&mut self) {
        self.of.clear();
        self.disallowed = false;
    }

    /// The verdicts of the rows awaiting one (`MarkFilter::allowed` per shape).
    fn decide(&mut self, frame: u32) -> Result<()> {
        let Some(f) = self.marks else { return Ok(()) };
        for (buf, ats) in self.bufs.iter_mut().zip(self.at.iter_mut()) {
            if buf.rows() == 0 {
                continue;
            }
            for (&at, ok) in ats.iter().zip(f.allowed(&buf.to_rt2(), frame)?) {
                self.of.insert(at, if ok { VERDICT_YES } else { VERDICT_NO });
                self.disallowed |= !ok;
            }
            buf.clear();
            ats.clear();
        }
        Ok(())
    }
}

/// THE GUARDS at emission: the shapes this worker noted (`note_shape`),
/// registered (`Skeletons`) and checked this frame (the DECODE GUARD), and
/// `--key-check`'s rows, checked in batches.
struct Guards<N: Rem> {
    key_check: bool,
    check: Vec<RowBuf<N>>,
    noted: FxHashSet<u64>,
    registered: FxHashSet<u64>,
    guarded: FxHashSet<u64>,
    /// Inside `emit_row` (reference rows: no guard, no key check).
    reference: bool,
    /// The last emission's shape, once registered and guarded (0: none).
    last_shape: u64,
}

/// LEVEL -1 at emission: the table of `--level-minus-one` and its horizon,
/// resolved once, and the last (shape, cell) asked.
struct Drops {
    l1: Option<(&'static crate::search::level_minus_one::CostToGo, u32)>,
    last: Option<((u64, u32), bool)>,
}

impl Drops {
    /// Does it drop every row of (`shape`, `cell`) at frame `frame`?
    #[inline]
    fn drops(&mut self, shape: u64, cell: u32, frame: u32) -> bool {
        let Some((table, h)) = self.l1 else { return false };
        if let Some((at, r)) = self.last {
            if at == (shape, cell) {
                return r;
            }
        }
        let r = table.admitted_from(shape, cell, frame) > h;
        self.last = Some(((shape, cell), r));
        r
    }
}

/// A route's PUSHES being merged (`pushes`): per target cell reached a slot
/// (the targets' cells in order), each target's first slot; pushes by
/// (hull, this worker's transfer id).
struct Pusher<N: Rem> {
    slots: Vec<AccOf<N>>,
    offs: Vec<u32>,
    cache: crate::search::hull::PushCache<N>,
}

impl<N: Rem> Default for Pusher<N> {
    fn default() -> Self {
        Pusher { slots: Vec::new(), offs: Vec::new(), cache: Default::default() }
    }
}

impl<N: Rem> Pusher<N> {
    /// The PUSHES of a route's bundles (the route at `slot`): per target
    /// cell the merged `Hull::push`es of its source cells, `(target, cell,
    /// hull)`, the cells nothing reached left out.
    #[inline(never)]
    fn pushes(&mut self, src: &Sources<N>, xfers: &Xfers<N>, unit: u32, slot: u32, targets: &[(Key, u64)], bundles: &[super::route::Ub]) -> Vec<(u32, u8, AccOf<N>)> {
        let geo = super::GEOMETRY;
        // A slot per target cell reached (a bundle's cells are its target's).
        let (mut slots, mut offs) = (std::mem::take(&mut self.slots), std::mem::take(&mut self.offs));
        offs.clear();
        let mut n = 0u32;
        for &(_, m) in targets {
            offs.push(n);
            n += m.count_ones();
        }
        slots.clear();
        slots.resize(n as usize, AccOf::<N>::default());
        // A bundle's edges: (source lane, target cell).
        let mut edges: Vec<(u32, u8)> = Vec::with_capacity(64);
        for b in bundles {
            let (off, tm) = (offs[b.t as usize], targets[b.t as usize].1);
            debug_assert_eq!(b.mask & !tm, 0, "a bundle's cells outside its target's");
            let at = |c: u32| (off + (tm & ((1u64 << c) - 1)).count_ones()) as usize;
            edges.clear();
            src.by_entry(unit, slot, b, |e, part, k| {
                let (_, _, lanes, lane) = src.entry_next[e as usize];
                match k {
                    Some(k) => {
                        let s = super::bundle::shift(part, k);
                        assert!(s & !lanes == 0, "unit {unit}: a bundle's source cell is no lane of the unit");
                        edges.extend(super::layer::bits(s).map(|s| (lane + (lanes & ((1u64 << s) - 1)).count_ones(), (s as i32 - k) as u8)));
                    }
                    None => {
                        let c = part.trailing_zeros();
                        let s = super::bundle::source_cell(geo.cell(slot, c), b.o);
                        edges.push((src.lane(unit, b.s, s) as u32, c as u8));
                    }
                }
            });
            // The bundle's transfer, read at the first push the cache misses.
            let mut xfer = None;
            for &(lane, c) in &edges {
                let h = src.hulls[lane as usize];
                let pushed = self.cache.get(&h, &h, b.x, || {
                    let mut a = AccOf::<N>::default();
                    h.push(xfer.get_or_insert_with(|| xfers.get(b.x)), &mut a).then_some(a)
                });
                if let Some(a) = pushed {
                    slots[at(c as u32)].merge(&a);
                }
            }
        }
        let mut out: Vec<(u32, u8, AccOf<N>)> = Vec::with_capacity(n as usize);
        let mut at = 0usize;
        for (t, &(_, m)) in targets.iter().enumerate() {
            let mut c = m;
            while c != 0 {
                let k = c.trailing_zeros();
                c &= c - 1;
                if !slots[at].is_empty() {
                    out.push((t as u32, k as u8, slots[at]));
                }
                at += 1;
            }
        }
        if slots.capacity() > 1 << 20 {
            (slots, offs) = (Vec::new(), Vec::new());
        }
        (self.slots, self.offs) = (slots, offs);
        out
    }
}

pub struct UnitSink<'a, N: Rem = P8> {
    keys: &'a KeySpace<N>,
    skeletons: &'a Skeletons<N>,
    /// The wave's provisional keys (a code the key space lacks: a new state).
    prov: &'a Provisional<N>,
    frame: u32,
    /// The level the wave runs at (a reference row's key view).
    level: crate::abstraction::Level,
    pub(crate) worker: u32,
    pub(crate) unit: u32,
    src: Sources<N>,
    xfers: Xfers<'a, N>,
    routes: RouteIds<'a>,
    pusher: Pusher<N>,
    drops: Drops,
    verdicts: Verdicts<'a, N>,
    guards: Guards<N>,
    /// The unit's bundles by route, and its (route, source region) pairs
    /// (`src_seen`: the ones met, direct mapped, `pair + 1`).
    bundles: UnitBundles,
    src_regions: Vec<(u32, u32)>,
    src_seen: Vec<u64>,
    pub(crate) outbox: Vec<Chunk<N>>,
    /// The worker's statistics (merged at the frame's end).
    pub(crate) stats: super::stats::Stats,
}

impl<'a, N: Rem> UnitSink<'a, N> {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        visited: &'a VisitedSet<N>,
        routes: &'a Routes,
        skeletons: &'a Skeletons<N>,
        prov: &'a Provisional<N>,
        filters: Filters<'a, N>,
        frame: u32,
        level: crate::abstraction::Level,
        worker: u32,
        base: &'a super::edges::XferTable<N>,
        xfer_tab: &'a std::sync::RwLock<Vec<N::Pair>>,
        s: &crate::settings::Settings,
    ) -> Self {
        UnitSink {
            keys: &visited.keys,
            skeletons,
            prov,
            frame,
            level,
            worker,
            unit: 0,
            src: Sources::default(),
            xfers: Xfers::new(base, xfer_tab),
            routes: RouteIds::new(routes),
            pusher: Pusher::default(),
            drops: Drops { l1: filters.minus_one.map(|m| (m.table, m.h)), last: None },
            verdicts: Verdicts { marks: filters.marks, of: Default::default(), bufs: Vec::new(), at: Vec::new(), disallowed: false },
            guards: Guards { key_check: s.diag.key_check, check: Vec::new(), noted: Default::default(), registered: Default::default(), guarded: Default::default(), reference: false, last_shape: 0 },
            bundles: UnitBundles::default(),
            src_regions: Vec::new(),
            src_seen: Vec::new(),
            outbox: Vec::new(),
            stats: super::stats::Stats::new(s.diag.phases),
        }
    }

    /// The tree's key space (read-only during the wave).
    #[inline]
    pub(crate) fn keys(&self) -> &'a KeySpace<N> {
        self.keys
    }

    #[inline]
    pub(crate) fn provisional(&self) -> &'a Provisional<N> {
        self.prov
    }

    /// A shape the key space lacks, emitted (`Provisional::note_shape`,
    /// once a worker).
    pub(crate) fn note_shape(&mut self, r: &celeste_engine::exact::ShapeRecord) {
        if self.guards.noted.insert(r.hash) {
            self.prov.note_shape(r);
        }
    }

    /// Start unit `unit`; then `push_source`, `push_lane`, `sources_done`.
    pub(crate) fn begin(&mut self, unit: u32) {
        self.unit = unit;
        self.bundles.clear();
        self.src_regions.clear();
        self.src_seen.clear();
        self.src_seen.resize(SRC_SEEN, 0);
        self.verdicts.clear();
        self.src.clear();
    }

    /// The unit's next source entry `(region, key)`; its index.
    #[inline]
    pub(crate) fn push_source(&mut self, region: u32, key: Key) -> u32 {
        self.src.push_source(region, key)
    }

    /// The unit's next lane: source entry `entry`'s state at `cell`, its hull.
    #[inline]
    pub(crate) fn push_lane(&mut self, entry: u32, cell: u32, hull: HullOf<N>) {
        self.src.push_lane(entry, cell, hull);
    }

    /// The unit's sources and lanes are in: their hulls by source and cell.
    pub(crate) fn sources_done(&mut self) {
        self.src.done();
    }

    /// Does level -1 drop rows at emission (`minus_one_drops`)?
    #[inline]
    pub(crate) fn drops_at_emission(&self) -> bool {
        self.drops.l1.is_some()
    }

    /// The level -1 filter at EMISSION: does it drop every row of (`shape`,
    /// `cell`) at this frame? Then the row is never keyed or looked up.
    #[inline]
    pub(crate) fn minus_one_drop(&mut self, shape: u64, cell: u32) -> bool {
        self.drops.drops(shape, cell, self.frame)
    }

    /// `minus_one_drop` over a slice's lanes `need`: the lanes it drops.
    #[inline]
    pub(crate) fn minus_one_drops(&mut self, shape: u64, cells: &[u32; 16], need: u16) -> u16 {
        let mut dropped = 0u16;
        for i in lanes_of(need) {
            if self.minus_one_drop(shape, cells[i]) {
                dropped |= 1 << i;
            }
        }
        dropped
    }

    /// A transfer's id through a direct-mapped raw -> id cache (`Xfers`).
    #[inline]
    pub(crate) fn xfer_id_raw(&mut self, words: &crate::compiled::asm_kernel::RawWords<N>, decode: impl FnOnce(&crate::compiled::asm_kernel::RawWords<N>) -> N::Pair) -> u32 {
        self.xfers.id_raw(self.worker, words, decode)
    }

    /// The transfers this worker has met (its table's length).
    pub(crate) fn xfers_met(&self) -> usize {
        self.xfers.len as usize
    }

    #[inline]
    pub(crate) fn route(&mut self, shape: u64, slot: u32) -> u32 {
        self.routes.id(shape, slot)
    }

    /// The buffer of `shape` in `bufs` (made over `init()`'s skeleton).
    #[inline]
    fn buf_of(bufs: &mut Vec<RowBuf<N>>, shape: u64, init: impl FnOnce() -> Rt2<N>) -> usize {
        match bufs.iter().position(|b| b.shape == shape) {
            Some(i) => i,
            None => {
                bufs.push(RowBuf::new(init()));
                bufs.len() - 1
            }
        }
    }

    /// The TARGETS of a slice's lanes `need` (unit lanes `lanes`) at `cells`.
    pub(crate) fn targets(&mut self, shape: u64, cells: &[u32; 16], need: u16, lanes: &[usize], t: &mut Targets) {
        t.cells = *cells;
        for i in lanes_of(need) {
            let cell = cells[i];
            let (slot, local) = super::GEOMETRY.slot_local(cell);
            t.route[i] = self.routes.id(shape, slot);
            t.local[i] = local as u8;
            let (entry, s_cell) = self.src.lanes[lanes[i]];
            t.src[i] = self.src.src_local[entry as usize];
            t.s_region[i] = self.src.srcs[entry as usize].0;
            t.offset[i] = super::bundle::offset(s_cell, cell);
        }
    }

    /// ONE EMISSION: `emit_body` of one lane, the state `(shape, key, cell)`
    /// reached from unit lane `lane` with transfer `xfer`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit(&mut self, shape: u64, cell: u32, key: Key, lane: usize, xfer: u32, init: impl Fn() -> Rt2<N>, mut push: impl FnMut(&mut RowBuf<N>)) -> Result<()> {
        let mut targets = Targets::default();
        self.targets(shape, &[cell; 16], 1, &[lane], &mut targets);
        let e = BodyEmits { shape, take: 1, keys: &[key; 16], targets: &targets, xfers: &[xfer; 16] };
        self.emit_body(&e, init, |b, _| push(b))
    }

    /// A BODY'S EMISSIONS into their routes' bundles. `init`: the shape's
    /// skeleton (`Skeletons`); `push(buf, i)` appends lane `i`'s row to a
    /// buffer when one is looked at (a verdict, the key check, the guard).
    pub(crate) fn emit_body(&mut self, e: &BodyEmits, init: impl Fn() -> Rt2<N>, mut push: impl FnMut(&mut RowBuf<N>, usize)) -> Result<()> {
        let (shape, t) = (e.shape, e.targets);
        let mut take = e.take;
        if take == 0 {
            return Ok(());
        }
        self.stats.add(super::stats::S::Emissions, take.count_ones() as u64);
        // A shape's first emission (this worker, this wave): its skeleton
        // registered, and THE DECODE GUARD - its row is what its key and
        // cell decode to. Looked at only when the shape is not the last's.
        let g = &mut self.guards;
        if shape != g.last_shape {
            if !g.registered.contains(&shape) {
                self.skeletons.register(&init())?;
                g.registered.insert(shape);
            }
            g.last_shape = if g.guarded.contains(&shape) { shape } else { 0 };
        }
        if g.last_shape != shape && !g.reference {
            if let Some(i) = lanes_of(take).find(|&i| !is_provisional(e.keys[i])) {
                let mut b = RowBuf::new(init());
                push(&mut b, i);
                self.skeletons.check_decodes(self.keys, &b.to_rt2(), &[t.cells[i]])?;
                g.guarded.insert(shape);
                g.last_shape = shape;
            }
        }
        if g.key_check && !g.reference {
            let b = Self::buf_of(&mut g.check, shape, &init);
            for i in lanes_of(take) {
                push(&mut self.guards.check[b], i);
                if self.guards.check[b].rows() >= 256 {
                    self.check_rows(b)?;
                }
            }
        }
        // The objects ladder's filter, once per target, in batches: a target
        // awaiting its verdict is allowed; if refused, `end` drops its records.
        if self.verdicts.marks.is_some() {
            for i in lanes_of(take) {
                let at: Target = (t.route[i], e.keys[i], t.local[i]);
                match self.verdicts.of.get(&at) {
                    Some(&VERDICT_NO) => take &= !(1 << i),
                    Some(_) => {}
                    None => {
                        let v = &mut self.verdicts;
                        let b = Self::buf_of(&mut v.bufs, shape, &init);
                        if v.at.len() <= b {
                            v.at.resize_with(b + 1, Vec::new);
                        }
                        push(&mut v.bufs[b], i);
                        v.at[b].push(at);
                        v.of.insert(at, VERDICT_PENDING);
                        if v.bufs[b].rows() >= VERDICT_BATCH {
                            v.decide(self.frame)?;
                        }
                    }
                }
            }
        }
        // THE BUNDLES: lanes into one bundle (one source entry's cells
        // reaching one target key alike) ORed together first.
        let xfers = e.xfers;
        let mut pending: Option<(u32, Key, u32, u32, u32, u64)> = None;
        for i in lanes_of(take) {
            let route = t.route[i];
            // A (route, source region) pair once a unit, mostly: a direct
            // mapped table of the pairs met (`end` sorts and dedups the rest).
            let pair = (route as u64) << 32 | t.s_region[i] as u64;
            let slot = (pair.wrapping_mul(0x9e37_79b9_7f4a_7c15) >> (64 - SRC_SEEN.trailing_zeros())) as usize;
            if self.src_seen[slot] != pair + 1 {
                self.src_seen[slot] = pair + 1;
                self.src_regions.push((route, t.s_region[i]));
            }
            let b = (route, e.keys[i], t.src[i], xfers[i], t.offset[i]);
            match &mut pending {
                Some(p) if (p.0, p.1, p.2, p.3, p.4) == b => p.5 |= 1u64 << t.local[i],
                _ => {
                    if let Some((r, k, s, x, o, m)) = pending {
                        self.bundle(r, k, s, x, o, m);
                    }
                    pending = Some((b.0, b.1, b.2, b.3, b.4, 1u64 << t.local[i]));
                }
            }
        }
        if let Some((r, k, s, x, o, m)) = pending {
            self.bundle(r, k, s, x, o, m);
        }
        Ok(())
    }

    /// OR `m` (target cells) into bundle `(route, t_key; s, x, o)`.
    #[inline]
    fn bundle(&mut self, route: u32, t_key: Key, s: u32, x: u32, o: u32, m: u64) {
        self.bundles.insert(route, t_key, s, x, o, m);
    }

    /// `--key-check` of the rows in `check[i]`: each one's key is the
    /// boundary's, and its key and cell decode to it.
    fn check_rows(&mut self, i: usize) -> Result<()> {
        let b = &self.guards.check[i];
        crate::compiled::asm_kernel::key_check(b, self.keys, self.prov);
        let rows = b.to_rt2();
        let keep: Vec<u32> = (0..rows.width as u32).filter(|&r| !is_provisional(rows.row_keys[r as usize])).collect();
        if !keep.is_empty() {
            let mut r = rows.clone_block();
            r.retain_lanes(&keep);
            let cells: Vec<u32> = keep.iter().map(|&k| b.cells[k as usize]).collect();
            self.skeletons.check_decodes(self.keys, &r, &cells)?;
        }
        self.guards.check[i].clear();
        Ok(())
    }

    /// A reference row (CANONICAL) as the tree holds it, and its key
    /// (provisional where a code is new): widened as its kernels key it (held
    /// buttons, every level's widenings: `Rt2::widen_to`). The WIDENED row: a
    /// stored row is what its key decodes to (plans/architecture.md "Keys").
    pub(crate) fn row_key(&mut self, row: &Rt2<N>) -> (Rt2<N>, Key) {
        let level = crate::abstraction::Level { held: self.level.held, ..crate::abstraction::Level::EXACT };
        let mut w = row.clone_block();
        crate::frame::widen_rt2_to(&mut w, level);
        w.canonical();
        if self.keys.shape(w.shape_hash).is_none() {
            self.note_shape(&celeste_engine::exact::ShapeRecord::of(&w, crate::compiled::ids()));
        }
        let key = match self.keys.row_keys(&w, crate::compiled::ids()).swap_remove(0) {
            RowKey::Hit(k) => k,
            RowKey::Miss(m) => self.prov.intern(m),
        };
        (w, key)
    }

    /// The reference engine's emission: one row from unit lane `lane`
    /// (stored widened: `row_key`) by transfer `pair`.
    /// Its skeleton is `decode::reference_skeleton`: a reference row's
    /// columns follow its values, not a kernel's template.
    pub(crate) fn emit_row(&mut self, row: &Rt2<N>, lane: usize, cell: u32, pair: N::Pair) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        if self.minus_one_drop(row.shape_hash, cell) {
            return Ok(());
        }
        let (w, key) = self.row_key(row);
        anyhow::ensure!(w.shape_hash == row.shape_hash, "a reference row's widening changed its shape ({:016x} -> {:016x})", row.shape_hash, w.shape_hash);
        if !self.guards.registered.contains(&w.shape_hash) {
            self.skeletons.register(&super::decode::reference_skeleton(&w))?;
            self.guards.registered.insert(w.shape_hash);
        }
        // Its buffers (verdicts, the key check) hold the row as it is.
        let init = || {
            let mut b = celeste_engine::slots::reshape(&w, 0);
            b.cols = w
                .cols
                .iter()
                .map(|c| match c.at(0) {
                    AV::Num(_) => Col::N(Vec::new()),
                    AV::Ival(..) => Col::I(Vec::new()),
                    AV::Bool(_) | AV::UBool => Col::V(Vec::new()),
                    v => Col::U(v),
                })
                .collect();
            b.shape_hash = w.shape_hash;
            b
        };
        let xfer = self.xfers.id(self.worker, pair);
        self.guards.reference = true;
        let r = self.emit(w.shape_hash, cell, key, lane, xfer, init, |b| b.push_row(&w, key, cell));
        self.guards.reference = false;
        r
    }

    /// The worker's units are over: the shapes it checked are checked again
    /// next frame.
    pub(crate) fn finish(&mut self) {
        self.guards.guarded.clear();
        self.guards.last_shape = 0;
    }

    /// The unit is over: its rows checked, its last verdicts decided (refused
    /// targets' records go), its buckets the chunks of their routes.
    pub(crate) fn end(&mut self) -> Result<()> {
        let t = self.stats.tick_start();
        for i in 0..self.guards.check.len() {
            if self.guards.check[i].rows() > 0 {
                self.check_rows(i)?;
            }
        }
        self.verdicts.decide(self.frame)?;
        let v = &self.verdicts.of;
        let drop = |route: u32, key: Key, c: u32| v.get(&(route, key, c as u8)) == Some(&VERDICT_NO);
        let buckets = self.bundles.drain(if self.verdicts.disallowed { Some(&drop) } else { None });
        self.src_regions.sort_unstable();
        self.src_regions.dedup();
        let sources: std::sync::Arc<[(u32, Key)]> = self.src.unit_sources.as_slice().into();
        let mut buckets = buckets;
        buckets.sort_unstable_by_key(|b| b.0);
        let mut at = 0usize;
        for (route, targets, bundles) in buckets {
            let mut src_regions = Vec::new();
            while let Some(&(r, region)) = self.src_regions.get(at) {
                if r > route {
                    break;
                }
                if r == route {
                    src_regions.push(region);
                }
                at += 1;
            }
            let (_, slot) = self.routes.slot(route);
            let pushes = self.pusher.pushes(&self.src, &self.xfers, self.unit, slot, &targets, &bundles);
            self.outbox.push(Chunk { route, unit: self.unit, worker: self.worker, targets, pushes, sources: sources.clone(), bundles, src_regions });
        }
        self.stats.tick(super::stats::S::PhUnitEnd, t);
        Ok(())
    }

    /// A RE-PUSH unit's pushes as route chunks: `targets` (route, key, cells),
    /// each one's hulls a run of `slots` (`runs`); no bundles (edges recorded).
    #[inline(never)]
    pub(crate) fn repush_chunks(&mut self, targets: &[(u32, Key, u64)], runs: &[(u32, u32)], slots: &[AccOf<N>]) {
        let mut order: Vec<u32> = (0..targets.len() as u32).collect();
        order.sort_unstable_by_key(|&t| targets[t as usize].0);
        let mut chunks: Vec<Chunk<N>> = Vec::new();
        let empty: std::sync::Arc<[(u32, Key)]> = Vec::new().into();
        for &t in &order {
            let (route, key, cells) = targets[t as usize];
            if chunks.last().is_none_or(|c| c.route != route) {
                chunks.push(Chunk { route, unit: self.unit, worker: self.worker, targets: Vec::new(), pushes: Vec::new(), sources: empty.clone(), bundles: Vec::new(), src_regions: Vec::new() });
            }
            let c = chunks.last_mut().expect("pushed");
            let lt = c.targets.len() as u32;
            c.targets.push((key, cells));
            let run = &slots[runs[t as usize].0 as usize..];
            c.pushes.extend(super::layer::bits(cells).zip(run).map(|(cell, h)| (lt, cell as u8, *h)));
        }
        self.outbox.append(&mut chunks);
    }
}
