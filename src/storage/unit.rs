//! ONE UNIT of a wave (plans/edges-target.md "The forward"): a run of
//! frontier rows through the kernels. The sink (`UnitSink`, one per worker,
//! reset per unit) ORs each emission into its BUNDLE of its target's route
//! (`route::UnitBundles`: source key, transfer, offset, target key ->
//! target cells) - nothing is looked up and no row is copied here: the
//! target region's worker merges the pushes and records the edges
//! (`route::run_route`), and the new layer is decoded from the keys
//! (`wave`). At the unit's end its bundles are the chunks of their routes,
//! each with its PUSHES: per target cell the hulls of the unit's sources
//! pushed along the bundles' transfers (`search::hull`).

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

/// A typed varying column of buffered rows: raw words (16.16's by
/// default), (low, high) pairs, or a byte per bool (0 false, 1 true, 2
/// unknown).
pub enum TCol<N: EngineNum = P8> {
    Num(Vec<N::Raw>),
    Ival(Vec<(N::Raw, N::Raw)>),
    Bool(Vec<u8>),
}

/// Rows of one shape, columnar: the shape's skeleton (the union of its
/// outcome templates' varying cells), a `TCol` per varying cell, key and
/// cell per row. A unit buffers rows here only to look at them: the
/// coarser level's verdicts, the key check, the decode guard.
pub struct RowBuf<N: EngineNum = P8> {
    pub shape: u64,
    skeleton: Rt2<N>,
    /// `(cell, column)` per varying cell, in the kernel's order.
    pub cols: Vec<(usize, TCol<N>)>,
    pub keys: Vec<Key>,
    pub cells: Vec<u32>,
}

impl<N: EngineNum> RowBuf<N> {
    /// An empty buffer over `skeleton`.
    pub fn new(skeleton: Rt2<N>) -> Self {
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

    pub fn rows(&self) -> usize {
        self.keys.len()
    }

    /// The buffer's skeleton (its shape's).
    pub fn skeleton(&self) -> &Rt2<N> {
        &self.skeleton
    }

    /// Drop the rows, keeping the skeleton and the columns' capacity.
    pub fn clear(&mut self) {
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

    /// The whole buffer as a block.
    pub fn to_rt2(&self) -> Rt2<N> {
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

    /// Push one materialized row (the reference engine's path): each typed
    /// cell's value, of its column's kind.
    pub fn push_row(&mut self, row: &Rt2<N>, key: Key, cell: u32) {
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
/// Entries of `UnitSink::pos_edge`'s cache (a power of two).
const POS_RECENT: usize = 1 << 10;
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

/// A slice's TARGETS in one cell column of one output shape
/// (`UnitSink::targets`, once per column, not per body): per lane its cell,
/// route and bit in the route's region, its source (the unit's source
/// index and region) and the offset between the two cells.
#[derive(Clone, Copy)]
pub struct Targets {
    pub cells: [u32; 16],
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

/// ONE BODY's emissions of a kernel slice (`compiled::asm_kernel`'s
/// emission, column-wise): the lanes `take`, lane `i` the state `(shape,
/// keys[i], targets.cells[i])` reached from unit lane `lanes[i]` with
/// transfer `xfers[i]`.
pub struct BodyEmits<'e> {
    pub shape: u64,
    pub take: u16,
    pub keys: &'e [Key; 16],
    pub targets: &'e Targets,
    pub xfers: &'e [u32; 16],
    pub lanes: &'e [usize],
}

/// A (route, key, cell) target as the coarser level's verdicts name it.
type Target = (u32, Key, u8);

/// One worker's sink for a frame's units (module doc).
pub struct UnitSink<'a, N: Rem = P8> {
    keys: &'a KeySpace<N>,
    routes: &'a Routes,
    skeletons: &'a Skeletons<N>,
    /// The wave's provisional keys (states holding a code the key space
    /// lacks: new by construction).
    prov: &'a Provisional<N>,
    filters: Filters<'a, N>,
    frame: u32,
    /// The level the wave runs at (a reference row's key view).
    level: crate::abstraction::Level,
    pub worker: u32,
    /// The unit's index in the wave.
    pub unit: u32,
    /// The unit's sources: its entries `(region, key)`, each one's index
    /// among the unit's distinct `(shape, key)`s (`unit_sources`, the
    /// chunks' source list), and per lane its entry, cell and hull.
    srcs: Vec<(u32, Key)>,
    src_local: Vec<u32>,
    lanes: Vec<(u32, u32)>,
    hulls: Vec<N::Hull>,
    unit_sources: Vec<(u32, Key)>,
    src_idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    /// The unit's lanes' hulls by source: per unit source its first entry,
    /// per entry its next of the same source (`u32::MAX`: none), its slot,
    /// cells and first lane - a bundle's source cell (its target cell less
    /// its offset) is a lane's by its rank in its entry's cells.
    src_first: Vec<u32>,
    entry_next: Vec<(u32, u32, u64, u32)>,
    /// A route's pushes being merged (`pushes`): per target cell reached a
    /// slot (the targets' cells in order), and each target's first slot.
    push_slots: Vec<N::Acc>,
    push_offs: Vec<u32>,
    /// Pushes by (hull, local transfer id): ids are this worker's for the
    /// frame.
    push_cache: crate::search::hull::PushCache<N>,
    /// This worker's routes (and their slots), and the last one asked.
    route_ids: FxHashMap<(u64, u32), u32>,
    route_slot: FxHashMap<u32, (u64, u32)>,
    last_route: (u64, u32, u32),
    /// The unit's bundles by route, and its (route, source region) pairs
    /// (`src_seen`: the ones met, direct mapped, `pair + 1`).
    bundles: UnitBundles,
    src_regions: Vec<(u32, u32)>,
    src_seen: Vec<u64>,
    /// Per target the coarser level's verdict (`MarkFilter`), once a unit,
    /// decided in batches (a projection per block, not per row).
    verdicts: rustc_hash2::FxHashMap<Target, u8>,
    /// The rows awaiting a verdict, per shape, and their targets.
    scratch: Vec<RowBuf<N>>,
    scratch_at: Vec<Vec<Target>>,
    /// Some target of the unit was disallowed: its records go at the
    /// unit's end.
    disallowed: bool,
    /// `--key-check`: every emission's row, checked in batches (its key,
    /// and that its key and cell decode to it).
    key_check: bool,
    check: Vec<RowBuf<N>>,
    /// `--phases`: the wave's phase timers (`frame::phases`).
    pub phases: bool,
    /// The level -1 table of `filters.minus_one`, resolved once.
    l1: Option<&'static crate::trace::level_minus_one::CostToGo>,
    drop_last: Option<((u64, u32), bool)>,
    /// TRANSFER IDS: a pair the tree's table holds (`base`, read only
    /// during the wave, shared by the workers) is its global id; a pair new
    /// this frame is this worker's LOCAL id `l`, named `first_new + l` (the
    /// gather gives it its global id: the region phase's remaps). Only the
    /// new pairs cost the worker anything (each worker held every pair it
    /// met: ~170 B a pair a worker).
    base: &'a super::edges::XferTable<N>,
    first_new: u32,
    xfer_ids: FxHashMap<N::Pair, u32>,
    /// Per local transfer id its decoded transfers (the pushes').
    xfer_pairs: Vec<crate::search::rem::Xfer<N>>,
    /// Per local transfer id, the edges this worker recorded with it (the
    /// frame's new transfers are numbered by their count: `wave`).
    xfer_counts: Vec<u64>,
    xfer_cache: Vec<(crate::compiled::asm_kernel::RawWords, u32)>,
    /// The worker's table of local transfers (the wave's gather reads it),
    /// and its length.
    xfer_tab: &'a std::sync::RwLock<Vec<N::Pair>>,
    xfer_len: u32,
    /// The units' chunks.
    pub outbox: Vec<Chunk<N>>,
    /// Record the units' OUT-EDGES (`storage::out`): its last unit's blocks
    /// (raw, and whether a target key is provisional), and the parts they
    /// are built from.
    record_out: bool,
    pub out_blocks: Vec<(Vec<u8>, bool)>,
    out_parts: super::out::Parts<N>,
    /// The unit's transfers, each one's rank among them by pair
    /// (`out_parts_of`).
    x_rank: FxHashMap<u32, u32>,
    /// Record `edges` (the pos graph) at all.
    pub edges_on: bool,
    /// The distinct pos-graph edges this worker produced, and the ones met
    /// recently (direct-mapped: a slice's lanes repeat their edges).
    pos_edges: FxHashSet<(u32, u32)>,
    pos_recent: Vec<(u32, u32)>,
    /// Emissions past level -1 (each one a record).
    pub emitted: u64,
    /// `--emit-capture`: this worker's stream (`bench`).
    capture: Option<super::bench::CaptureWriter<N>>,
    /// The shapes this worker noted (`note_shape`), registered
    /// (`Skeletons`) and checked this frame (`guard`).
    noted: FxHashSet<u64>,
    registered: FxHashSet<u64>,
    guarded: FxHashSet<u64>,
    /// Inside `emit_row` (the reference engine's rows: no guard, no key
    /// check), or the emissions come without rows (`bench::Replay`).
    reference: bool,
    rowless: bool,
    /// The last emission's shape, once registered and guarded (0: none).
    last_shape: u64,
}

/// Transfer `x`'s pair: a global id's in the tree's table `base`, a local
/// one's (`first_new` on) in the worker's table `tab`.
#[inline]
fn pair_of<N: Rem>(base: &super::edges::XferTable<N>, tab: &[N::Pair], first_new: u32, x: u32) -> N::Pair {
    match x.checked_sub(first_new) {
        Some(l) => tab[l as usize],
        None => base.pairs[x as usize],
    }
}

impl<'a, N: Rem> UnitSink<'a, N> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
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
        edges_on: bool,
        record_out: bool,
        s: &crate::settings::Settings,
    ) -> Self {
        let l1 = filters.minus_one.map(|m| m.table(s));
        UnitSink {
            keys: &visited.keys,
            routes,
            skeletons,
            prov,
            filters,
            frame,
            level,
            worker,
            unit: 0,
            srcs: Vec::new(),
            src_local: Vec::new(),
            lanes: Vec::new(),
            hulls: Vec::new(),
            unit_sources: Vec::new(),
            src_idx: Default::default(),
            src_first: Vec::new(),
            entry_next: Vec::new(),
            push_slots: Vec::new(),
            push_offs: Vec::new(),
            push_cache: Default::default(),
            route_ids: FxHashMap::default(),
            route_slot: FxHashMap::default(),
            last_route: (0, u32::MAX, u32::MAX),
            bundles: UnitBundles::default(),
            src_regions: Vec::new(),
            src_seen: Vec::new(),
            verdicts: Default::default(),
            scratch: Vec::new(),
            scratch_at: Vec::new(),
            disallowed: false,
            key_check: s.diag.key_check,
            check: Vec::new(),
            phases: s.diag.phases,
            l1,
            drop_last: None,
            base,
            first_new: u32::try_from(base.pairs.len()).expect("transfers past u32"),
            xfer_ids: Default::default(),
            xfer_pairs: Vec::new(),
            xfer_counts: Vec::new(),
            xfer_cache: Vec::new(),
            xfer_tab,
            xfer_len: 0,
            outbox: Vec::new(),
            record_out,
            out_blocks: Vec::new(),
            out_parts: Default::default(),
            x_rank: Default::default(),
            edges_on,
            pos_edges: Default::default(),
            pos_recent: Vec::new(),
            emitted: 0,
            capture: super::bench::CaptureWriter::open(frame, worker, level, s),
            noted: Default::default(),
            registered: Default::default(),
            guarded: Default::default(),
            reference: false,
            rowless: false,
            last_shape: 0,
        }
    }

    /// Bytes this worker's buffers hold (`--phases`' `[wavemem]`): its
    /// bundles, its lanes' hulls, its pos-graph pairs, its transfers (by
    /// pair, decoded, counted, its table, its ranks), the rest (the unit's
    /// sources and lanes, chunks not yet handed over).
    pub fn alloc_bytes(&self) -> [usize; 5] {
        use std::mem::size_of;
        [
            self.bundles.alloc_bytes(),
            size_of::<N::Hull>() * self.hulls.capacity() + 4 * self.src_first.capacity() + 20 * self.entry_next.capacity() + size_of::<N::Acc>() * self.push_slots.capacity() + 4 * self.push_offs.capacity(),
            9 * self.pos_edges.capacity() + 8 * self.pos_recent.capacity(),
            (size_of::<N::Pair>() + 8) * self.xfer_ids.capacity()
                + size_of::<crate::search::rem::Xfer<N>>() * self.xfer_pairs.capacity()
                + 8 * self.xfer_counts.capacity()
                + size_of::<N::Pair>() * self.xfer_tab.read().expect("a transfer table").capacity()
                + 8 * self.x_rank.capacity()
                + size_of::<(crate::compiled::asm_kernel::RawWords, u32)>() * self.xfer_cache.capacity(),
            24 * (self.srcs.capacity() + self.unit_sources.capacity())
                + 4 * self.src_local.capacity()
                + 8 * self.lanes.capacity()
                + 32 * self.src_idx.capacity()
                + self.outbox.iter().map(|c| c.alloc_bytes()).sum::<usize>(),
        ]
    }

    /// The tree's key space (read-only during the wave).
    #[inline]
    pub fn keys(&self) -> &'a KeySpace<N> {
        self.keys
    }

    /// The wave's provisional keys.
    #[inline]
    pub fn provisional(&self) -> &'a Provisional<N> {
        self.prov
    }

    /// A shape the key space lacks, emitted (`Provisional::note_shape`,
    /// once a worker).
    pub fn note_shape(&mut self, r: &celeste_engine::exact::ShapeRecord) {
        if self.noted.insert(r.hash) {
            self.prov.note_shape(r);
            if let Some(c) = &mut self.capture {
                c.shape(r);
            }
        }
    }

    /// Start unit `unit` of the wave; its sources and lanes follow
    /// (`push_source`, `push_lane`).
    pub fn begin(&mut self, unit: u32) {
        self.unit = unit;
        self.bundles.clear();
        self.src_regions.clear();
        self.src_seen.clear();
        self.src_seen.resize(SRC_SEEN, 0);
        self.verdicts.clear();
        self.disallowed = false;
        self.srcs.clear();
        self.src_local.clear();
        self.lanes.clear();
        self.hulls.clear();
        self.unit_sources.clear();
        self.src_idx.clear();
    }

    /// The unit's next source entry `(region, key)`; its index.
    #[inline]
    pub fn push_source(&mut self, region: u32, key: Key) -> u32 {
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

    /// The unit's next lane: a state of source entry `entry` at `cell`,
    /// with its hull.
    #[inline]
    pub fn push_lane(&mut self, entry: u32, cell: u32, hull: N::Hull) {
        self.lanes.push((entry, cell));
        self.hulls.push(hull);
    }

    /// The unit's sources and lanes are in: their hulls by source and
    /// cell, the capture's unit record.
    pub fn sources_done(&mut self) {
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
        if let Some(c) = &mut self.capture {
            c.unit(self.unit, self.lanes.len() as u32);
        }
    }

    /// The lane of the unit's source `s` at `cell`.
    #[inline]
    fn src_lane(&self, s: u32, cell: u32) -> usize {
        let (slot, local) = super::GEOMETRY.slot_local(cell);
        let mut e = self.src_first[s as usize];
        while e != u32::MAX {
            let (next, sl, mask, lane) = self.entry_next[e as usize];
            if sl == slot && mask >> local & 1 == 1 {
                return (lane + (mask & ((1u64 << local) - 1)).count_ones()) as usize;
            }
            e = next;
        }
        panic!("unit {}: a bundle's source cell {cell} of source {s} is no lane of the unit", self.unit)
    }

    /// The EDGES of bundle `b` (target slot `slot`; not `NO_XFER`'s) by
    /// the unit's ENTRY they leave (a source key's cells in several regions
    /// are several entries): `f(entry, its target cells, the shift to
    /// their source cells)` - one shift of the grid between two cells'
    /// regions (`bundle::shift_into`), else (into or out of `NO_CELL`) one
    /// cell at a time and no shift: its source is `src_lane`'s.
    #[inline]
    fn by_entry(&self, slot: u32, b: &super::route::Ub, mut f: impl FnMut(u32, u64, Option<i32>)) {
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
            assert!(left == 0, "unit {}: a bundle's source cells in no entry of the unit", self.unit);
            return;
        }
        // Into or out of NO_CELL: cell by cell.
        let mut m = b.mask;
        while m != 0 {
            let c = m.trailing_zeros();
            m &= m - 1;
            let s_slot = geo.slot_local(source_cell(geo.cell(slot, c), b.o)).0;
            let mut e = self.src_first[b.s as usize];
            while e != u32::MAX && self.entry_next[e as usize].1 != s_slot {
                e = self.entry_next[e as usize].0;
            }
            assert!(e != u32::MAX, "unit {}: a bundle's source cell in no entry of the unit", self.unit);
            f(e, 1u64 << c, None);
        }
    }

    /// Does level -1 drop rows at emission (`minus_one_drops`)?
    #[inline]
    pub fn drops_at_emission(&self) -> bool {
        self.filters.minus_one.is_some()
    }

    /// The level -1 filter at EMISSION: does it drop every row of
    /// (`shape`, `cell`) at this frame? Then the row is never keyed or
    /// looked up. Half the emissions of room (6,2) 100% f57.
    #[inline]
    pub(crate) fn minus_one_drop(&mut self, shape: u64, cell: u32) -> bool {
        let Some(m) = self.filters.minus_one else { return false };
        if let Some((at, r)) = self.drop_last {
            if at == (shape, cell) {
                return r;
            }
        }
        let r = self.l1.expect("the level -1 table").admitted_from(shape, cell, self.frame) > m.h;
        self.drop_last = Some(((shape, cell), r));
        r
    }

    /// `minus_one_drop` of the lanes `need` of a slice, rows of `shape` at
    /// `cells`: the lanes it drops.
    #[inline]
    pub fn minus_one_drops(&mut self, shape: u64, cells: &[u32; 16], need: u16) -> u16 {
        let mut dropped = 0u16;
        for i in lanes_of(need) {
            if self.minus_one_drop(shape, cells[i]) {
                dropped |= 1 << i;
            }
        }
        dropped
    }

    /// The pos-graph edges (when edges are on) from the unit lanes `lanes`
    /// of a slice's lanes `need` into their `cells`: every lane a body
    /// takes, emitted or dropped, records its edge (the pos graph is a set:
    /// once per cell column of a slice, not per body).
    #[inline]
    pub fn pos_edges(&mut self, cells: &[u32; 16], need: u16, lanes: &[usize]) {
        if self.edges_on {
            for i in lanes_of(need) {
                self.pos_edge(lanes[i], cells[i]);
            }
        }
    }

    /// The pos-graph edge from unit lane `lane`'s cell into `cell`.
    #[inline]
    fn pos_edge(&mut self, lane: usize, cell: u32) {
        let e = (self.lanes[lane].1, cell);
        if self.pos_recent.is_empty() {
            self.pos_recent = vec![(u32::MAX, u32::MAX); POS_RECENT];
        }
        let slot = ((e.0 as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ (e.1 as u64).wrapping_mul(0xc2b2_ae3d_27d4_eb4f)) >> (64 - POS_RECENT.trailing_zeros());
        if self.pos_recent[slot as usize] != e {
            self.pos_recent[slot as usize] = e;
            self.pos_edges.insert(e);
        }
    }

    /// The distinct pos-graph edges this worker produced.
    pub fn pos_pairs(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.pos_edges.iter().copied()
    }

    /// `xfer_id` of a raw transfer, through a small direct-mapped cache of
    /// raw -> id: a frame has few distinct transfers (room (1,0) to f99:
    /// 17k) against one lookup per emitted lane, and the decode plus the
    /// hash-map intern was ~17% of a forward.
    #[inline]
    pub fn xfer_id_raw(&mut self, words: &crate::compiled::asm_kernel::RawWords, decode: impl FnOnce(&crate::compiled::asm_kernel::RawWords) -> N::Pair) -> u32 {
        let mut h = 0u64;
        for pair in words.chunks_exact(2) {
            h = (h ^ ((pair[0] as u64) << 32 | pair[1] as u64)).wrapping_mul(0x9e37_79b9_7f4a_7c15).rotate_left(29);
        }
        let slot = (h as usize) & (XFER_CACHE - 1);
        if self.xfer_cache.is_empty() {
            self.xfer_cache = vec![([0; crate::compiled::asm_kernel::RAW_WORDS], u32::MAX); XFER_CACHE];
        }
        let (w, id) = &self.xfer_cache[slot];
        if *id != u32::MAX && w == words {
            return *id;
        }
        let id = self.xfer_id(decode(words));
        self.xfer_cache[slot] = (*words, id);
        id
    }

    /// The transfer pair `t`'s id: its global one if the tree's table holds
    /// it, else this worker's local one (interned on first use).
    pub fn xfer_id(&mut self, t: N::Pair) -> u32 {
        if let Some(g) = self.base.find(&t) {
            return g;
        }
        if let Some(&id) = self.xfer_ids.get(&t) {
            return id;
        }
        let id = self.first_new.checked_add(self.xfer_len).unwrap_or_else(|| panic!("worker {}: {} transfers past the table's {}", self.worker, self.xfer_len, self.first_new));
        self.xfer_tab.write().expect("a transfer table").push(t);
        self.xfer_len += 1;
        self.xfer_ids.insert(t, id);
        self.xfer_pairs.push(N::xfer(&t));
        self.xfer_counts.push(0);
        id
    }

    /// Transfer `x`'s decoded pair (the pushes').
    #[inline]
    fn transfers(&self, x: u32) -> crate::search::rem::Xfer<N> {
        match x.checked_sub(self.first_new) {
            Some(l) => self.xfer_pairs[l as usize],
            None => N::xfer(&self.base.pairs[x as usize]),
        }
    }

    /// Per local transfer id, the edges this worker recorded with it.
    pub fn xfer_counts(&self) -> &[u64] {
        &self.xfer_counts
    }

    /// The transfers this worker has met (its table's length).
    pub fn xfers_met(&self) -> usize {
        self.xfer_len as usize
    }

    /// The route of `(shape, slot)`.
    #[inline]
    pub(crate) fn route(&mut self, shape: u64, slot: u32) -> u32 {
        if (self.last_route.0, self.last_route.1) == (shape, slot) {
            return self.last_route.2;
        }
        let r = match self.route_ids.get(&(shape, slot)) {
            Some(&r) => r,
            None => {
                let r = self.routes.id(shape, slot);
                self.route_ids.insert((shape, slot), r);
                self.route_slot.insert(r, (shape, slot));
                r
            }
        };
        self.last_route = (shape, slot, r);
        r
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

    /// The TARGETS of the lanes `need` of a slice (`lanes`: their unit
    /// lanes), rows of `shape` at `cells` (`Targets`).
    pub fn targets(&mut self, shape: u64, cells: &[u32; 16], need: u16, lanes: &[usize], t: &mut Targets) {
        t.cells = *cells;
        for i in lanes_of(need) {
            let cell = cells[i];
            let (slot, local) = super::GEOMETRY.slot_local(cell);
            t.route[i] = self.route(shape, slot);
            t.local[i] = local as u8;
            let (entry, s_cell) = self.lanes[lanes[i]];
            t.src[i] = self.src_local[entry as usize];
            t.s_region[i] = self.srcs[entry as usize].0;
            t.offset[i] = super::bundle::offset(s_cell, cell);
        }
    }

    /// ONE EMISSION: the state `(shape, key, cell)` reached from lane `lane`
    /// with transfer `xfer` (`None`: the reference engine's path, no edge):
    /// `emit_body` of one lane. `init` is the shape's skeleton (the new
    /// layer's rows are decoded with it: `Skeletons`); `push` appends the
    /// row to a buffer, when the row is looked at (a verdict, the key check,
    /// the decode guard).
    #[allow(clippy::too_many_arguments)]
    pub fn emit(&mut self, shape: u64, cell: u32, key: Key, lane: usize, xfer: u32, init: impl Fn() -> Rt2<N>, mut push: impl FnMut(&mut RowBuf<N>)) -> Result<()> {
        let mut targets = Targets::default();
        self.targets(shape, &[cell; 16], 1, &[lane], &mut targets);
        let e = BodyEmits { shape, take: 1, keys: &[key; 16], targets: &targets, xfers: &[xfer; 16], lanes: &[lane] };
        self.emit_body(&e, init, |b, _| push(b))
    }

    /// A BODY'S EMISSIONS (`BodyEmits`): per lane its state into its
    /// route's bundle (source key, transfer, offset, target key -> cells).
    /// `init` is the shape's skeleton (the new layer's rows are decoded with
    /// it: `Skeletons`); `push(buf, i)` appends lane `i`'s row to a buffer,
    /// when a row is looked at (a verdict, the key check, the decode guard).
    pub fn emit_body(&mut self, e: &BodyEmits, init: impl Fn() -> Rt2<N>, mut push: impl FnMut(&mut RowBuf<N>, usize)) -> Result<()> {
        let (shape, t) = (e.shape, e.targets);
        let mut take = e.take;
        if take == 0 {
            return Ok(());
        }
        self.emitted += take.count_ones() as u64;
        if let Some(c) = &mut self.capture {
            let tab = self.xfer_tab.read().expect("a transfer table");
            for i in lanes_of(take) {
                let key = e.keys[i];
                let x = c.xfer(e.xfers[i], || pair_of(self.base, &tab, self.first_new, e.xfers[i]));
                c.emit(shape, t.cells[i], key, e.lanes[i] as u32, x, is_provisional(key).then(|| self.prov.content(key)));
            }
        }
        // A shape's first emission (this worker, this wave): its skeleton
        // registered, and THE DECODE GUARD - its row is what its key and
        // cell decode to (the new layer is decoded). Looked at only when the
        // shape is not the last emission's.
        if shape != self.last_shape {
            if !self.registered.contains(&shape) {
                self.skeletons.register(&init())?;
                self.registered.insert(shape);
            }
            self.last_shape = if self.guarded.contains(&shape) { shape } else { 0 };
        }
        if self.last_shape != shape && !self.reference && !self.rowless {
            if let Some(i) = lanes_of(take).find(|&i| !is_provisional(e.keys[i])) {
                let mut b = RowBuf::new(init());
                push(&mut b, i);
                self.skeletons.check_decodes(self.keys, &b.to_rt2(), &[t.cells[i]])?;
                self.guarded.insert(shape);
                self.last_shape = shape;
            }
        }
        if self.key_check && !self.reference && !self.rowless {
            let b = Self::buf_of(&mut self.check, shape, &init);
            for i in lanes_of(take) {
                push(&mut self.check[b], i);
                if self.check[b].rows() >= 256 {
                    self.check_rows(b)?;
                }
            }
        }
        // The coarser level's filter (the objects ladder): once per target,
        // in batches - a target awaiting its verdict is taken as allowed, and
        // its records go at the unit's end if it is not.
        if self.filters.marks.is_some() {
            for i in lanes_of(take) {
                let at: Target = (t.route[i], e.keys[i], t.local[i]);
                match self.verdicts.get(&at) {
                    Some(&VERDICT_NO) => take &= !(1 << i),
                    Some(_) => {}
                    None => {
                        let b = Self::buf_of(&mut self.scratch, shape, &init);
                        if self.scratch_at.len() <= b {
                            self.scratch_at.resize_with(b + 1, Vec::new);
                        }
                        push(&mut self.scratch[b], i);
                        self.scratch_at[b].push(at);
                        self.verdicts.insert(at, VERDICT_PENDING);
                        if self.scratch[b].rows() >= VERDICT_BATCH {
                            self.decide()?;
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

    /// The edges `m` (target cells) of bundle `(route, t_key; source s,
    /// transfer x, offset o)` into the unit's bundles, counted for their
    /// transfer.
    #[inline]
    fn bundle(&mut self, route: u32, t_key: Key, s: u32, x: u32, o: u32, m: u64) {
        if let Some(l) = x.checked_sub(self.first_new) {
            self.xfer_counts[l as usize] += m.count_ones() as u64;
        }
        self.bundles.insert(route, t_key, s, x, o, m);
    }

    /// `--key-check` of the rows in `check[i]`: each one's key is the
    /// boundary's, and its key and cell decode to it.
    fn check_rows(&mut self, i: usize) -> Result<()> {
        let b = &self.check[i];
        crate::compiled::asm_kernel::key_check(b, self.keys, self.prov);
        let rows = b.to_rt2();
        let keep: Vec<u32> = (0..rows.width as u32).filter(|&r| !is_provisional(rows.row_keys[r as usize])).collect();
        if !keep.is_empty() {
            let mut r = rows.clone_block();
            r.retain_lanes(&keep);
            let cells: Vec<u32> = keep.iter().map(|&k| b.cells[k as usize]).collect();
            self.skeletons.check_decodes(self.keys, &r, &cells)?;
        }
        self.check[i].clear();
        Ok(())
    }

    /// The verdicts of the rows awaiting one (`MarkFilter::allowed` over
    /// each shape's batch).
    fn decide(&mut self) -> Result<()> {
        let Some(f) = self.filters.marks else { return Ok(()) };
        for (buf, ats) in self.scratch.iter_mut().zip(self.scratch_at.iter_mut()) {
            if buf.rows() == 0 {
                continue;
            }
            for (&at, ok) in ats.iter().zip(f.allowed(&buf.to_rt2(), self.frame)?) {
                self.verdicts.insert(at, if ok { VERDICT_YES } else { VERDICT_NO });
                self.disallowed |= !ok;
            }
            buf.clear();
            ats.clear();
        }
        Ok(())
    }

    /// A reference row (a CANONICAL row) as the tree holds it, and its key:
    /// the level's held buttons widened, as its kernels key it (with every
    /// level's widenings: `Rt2::widen_to`), in the tree's dictionaries -
    /// provisional where a code is new. The row is the WIDENED one: a
    /// tree's state is `(shape, key, cell)`, and its stored row is what the
    /// key decodes to (plans/rows-from-keys.md), not whichever concrete row
    /// of the class came first.
    pub fn row_key(&mut self, row: &Rt2<N>) -> (Rt2<N>, Key) {
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

    /// The reference engine's emission: one materialized row from unit lane
    /// `lane` (stored widened: `row_key`) with its transfer `pair`, and its
    /// pos-graph edge.
    /// Its shape's skeleton is the row's structure with every value cell
    /// left to the decode (`decode::Repr::Other`): a reference row's
    /// columns follow its values, not a kernel's template.
    pub fn emit_row(&mut self, row: &Rt2<N>, lane: usize, cell: u32, pair: N::Pair) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        if self.edges_on {
            self.pos_edge(lane, cell);
        }
        if self.minus_one_drop(row.shape_hash, cell) {
            return Ok(());
        }
        let (w, key) = self.row_key(row);
        anyhow::ensure!(w.shape_hash == row.shape_hash, "a reference row's widening changed its shape ({:016x} -> {:016x})", row.shape_hash, w.shape_hash);
        if !self.registered.contains(&w.shape_hash) {
            self.skeletons.register(&super::decode::reference_skeleton(&w))?;
            self.registered.insert(w.shape_hash);
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
        let xfer = self.xfer_id(pair);
        self.reference = true;
        let r = self.emit(w.shape_hash, cell, key, lane, xfer, init, |b| b.push_row(&w, key, cell));
        self.reference = false;
        r
    }

    /// The emissions come without rows (`bench::Replay`, a capture): no
    /// decode guard, no key check.
    pub fn replaying(&mut self) {
        self.rowless = true;
    }

    /// The worker's units are over: its capture stream and transfer table
    /// written (`--emit-capture`); the shapes it checked are checked again
    /// next frame.
    pub fn finish(&mut self) -> Result<()> {
        self.guarded.clear();
        self.last_shape = 0;
        match self.capture.take() {
            Some(c) => c.finish(),
            None => Ok(()),
        }
    }

    /// The PUSHES of a route's bundles (`bundles` naming `targets` by
    /// index, the route at `slot`): per target cell, the hulls of the
    /// bundles' source cells pushed along their transfers (`Hull::push`;
    /// a bundle without a transfer pushes the whole circle), merged -
    /// `(target, cell, hull so far)`, the cells nothing reached left out.
    #[inline(never)]
    fn pushes(&mut self, slot: u32, targets: &[(Key, u64)], bundles: &[super::route::Ub]) -> Vec<(u32, u8, N::Acc)> {
        let geo = super::GEOMETRY;
        // A slot per target cell reached (a bundle's cells are its target's).
        let (mut slots, mut offs) = (std::mem::take(&mut self.push_slots), std::mem::take(&mut self.push_offs));
        offs.clear();
        let mut n = 0u32;
        for &(_, m) in targets {
            offs.push(n);
            n += m.count_ones();
        }
        slots.clear();
        slots.resize(n as usize, N::Acc::default());
        // A bundle's edges: (source lane, target cell).
        let mut edges: Vec<(u32, u8)> = Vec::with_capacity(64);
        for b in bundles {
            let (off, tm) = (offs[b.t as usize], targets[b.t as usize].1);
            debug_assert_eq!(b.mask & !tm, 0, "a bundle's cells outside its target's");
            let at = |c: u32| (off + (tm & ((1u64 << c) - 1)).count_ones()) as usize;
            edges.clear();
            self.by_entry(slot, b, |e, part, k| {
                let (_, _, lanes, lane) = self.entry_next[e as usize];
                match k {
                    Some(k) => {
                        let src = super::bundle::shift(part, k);
                        assert!(src & !lanes == 0, "unit {}: a bundle's source cell is no lane of the unit", self.unit);
                        edges.extend(super::layer::bits(src).map(|s| (lane + (lanes & ((1u64 << s) - 1)).count_ones(), (s as i32 - k) as u8)));
                    }
                    None => {
                        let c = part.trailing_zeros();
                        let src = super::bundle::source_cell(geo.cell(slot, c), b.o);
                        edges.push((self.src_lane(b.s, src) as u32, c as u8));
                    }
                }
            });
            let xfer = self.transfers(b.x);
            for &(lane, c) in &edges {
                let h = self.hulls[lane as usize];
                let pushed = self.push_cache.get(&h, &h, b.x, || {
                    let mut a = N::Acc::default();
                    N::push(&h, &xfer, &mut a).then_some(a)
                });
                if let Some(a) = pushed {
                    N::acc_merge(&mut slots[at(c as u32)], &a);
                }
            }
        }
        let mut out: Vec<(u32, u8, N::Acc)> = Vec::with_capacity(n as usize);
        let mut at = 0usize;
        for (t, &(_, m)) in targets.iter().enumerate() {
            let mut c = m;
            while c != 0 {
                let k = c.trailing_zeros();
                c &= c - 1;
                if !N::acc_is_empty(&slots[at]) {
                    out.push((t as u32, k as u8, slots[at]));
                }
                at += 1;
            }
        }
        if slots.capacity() > 1 << 20 {
            (slots, offs) = (Vec::new(), Vec::new());
        }
        (self.push_slots, self.push_offs) = (slots, offs);
        out
    }


    /// The unit is over: its rows checked, its last verdicts decided (a
    /// disallowed target's records go), its buckets the chunks of their
    /// routes, each with its pushes.
    pub fn end(&mut self) -> Result<()> {
        let t = crate::frame::phases::start(self.phases);
        for i in 0..self.check.len() {
            if self.check[i].rows() > 0 {
                self.check_rows(i)?;
            }
        }
        self.decide()?;
        let v = &self.verdicts;
        let drop = |route: u32, key: Key, c: u32| v.get(&(route, key, c as u8)) == Some(&VERDICT_NO);
        let buckets = self.bundles.drain(if self.disallowed { Some(&drop) } else { None });
        self.src_regions.sort_unstable();
        self.src_regions.dedup();
        let sources: std::sync::Arc<[(u32, Key)]> = self.unit_sources.as_slice().into();
        let mut buckets = buckets;
        buckets.sort_unstable_by_key(|b| b.0);
        if self.record_out {
            self.out_parts_of(&buckets);
        }
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
                let (_, slot) = *self.route_slot.get(&route).expect("a route this worker made");
            let pushes = self.pushes(slot, &targets, &bundles);
            self.outbox.push(Chunk { route, unit: self.unit, worker: self.worker, targets, pushes, sources: sources.clone(), bundles, src_regions });
        }
        if self.record_out && !self.out_parts.rows.is_empty() {
            self.out_blocks = super::out::encode_blocks(&mut self.out_parts);
            if self.out_parts.rows.capacity() > 1 << 20 {
                self.out_parts = Default::default();
            }
        }
        crate::frame::phases::add(crate::frame::phases::END_UNIT, t);
        Ok(())
    }

    /// A RE-PUSH unit's pushes (`wave::run_repush`) as the chunks of their
    /// routes: `targets` (route, key, cells), each one's cells' hulls a run
    /// of `slots` (`runs`: its start) - no bundle: the edges are recorded.
    #[inline(never)]
    pub(crate) fn repush_chunks(&mut self, targets: &[(u32, Key, u64)], runs: &[(u32, u32)], slots: &[N::Acc]) {
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

    /// The unit's bundles (`buckets`: route, targets, bundles) as its
    /// OUT-EDGE block's parts (`storage::out`): each bundle's cells split
    /// by the source ENTRY they leave (a source key's cells in two regions
    /// are two entries, `by_entry`).
    #[inline(never)]
    fn out_parts_of(&mut self, buckets: &[(u32, Vec<(Key, u64)>, Vec<super::route::Ub>)]) {
        let mut p = std::mem::take(&mut self.out_parts);
        p.clear();
        // Routes, pairs, targets: sorted, and each one's rank.
        let keys: Vec<(u64, u32)> = buckets.iter().map(|b| *self.route_slot.get(&b.0).expect("a route this worker made")).collect();
        p.routes = keys.clone();
        p.routes.sort_unstable();
        p.routes.dedup();
        // The distinct transfers (in `x_rank`), by pair: each one's rank.
        let tab = self.xfer_tab.read().expect("a transfer table");
        self.x_rank.clear();
        let mut by_pair: Vec<(N::Pair, u32)> = Vec::new();
        for u in buckets.iter().flat_map(|b| &b.2) {
            if let std::collections::hash_map::Entry::Vacant(v) = self.x_rank.entry(u.x) {
                v.insert(0);
                by_pair.push((pair_of(self.base, &tab, self.first_new, u.x), u.x));
            }
        }
        drop(tab);
        by_pair.sort_unstable();
        for (rank, &(_, x)) in by_pair.iter().enumerate() {
            self.x_rank.insert(x, rank as u32);
        }
        p.pairs = by_pair.into_iter().map(|x| x.0).collect();
        let mut ts: Vec<((u32, Key), u32, u32)> = Vec::new();
        for (bi, (b, k)) in buckets.iter().zip(&keys).enumerate() {
            let r = p.routes.binary_search(k).expect("a route of the unit") as u32;
            ts.extend(b.1.iter().enumerate().map(|(t, x)| ((r, x.0), bi as u32, t as u32)));
        }
        ts.sort_unstable();
        let mut t_rank: Vec<Vec<u32>> = buckets.iter().map(|b| vec![0; b.1.len()]).collect();
        for (rank, &(target, bi, t)) in ts.iter().enumerate() {
            p.targets.push(target);
            t_rank[bi as usize][t as usize] = rank as u32;
        }
        // Entries: the unit's, in order (its frontier's: by region and key).
        p.entries = self.srcs.clone();
        for (bi, b) in buckets.iter().enumerate() {
            let slot = keys[bi].1;
            for u in &b.2 {
                let (t, x) = (t_rank[bi][u.t as usize], self.x_rank[&u.x]);
                // The cells by source entry (one, mostly).
                let mut parts: [(u32, u64); 4] = [(u32::MAX, 0); 4];
                self.by_entry(slot, u, |e, part, _| {
                    let k = parts.iter().position(|q| q.0 == e || q.0 == u32::MAX).expect("a target region's cells leave at most four source regions");
                    parts[k] = (e, parts[k].1 | part);
                });
                for &(e, mask) in parts.iter().take_while(|q| q.0 != u32::MAX) {
                    p.rows.push((super::out::row_key(e, t, x, u.o), mask));
                }
            }
        }
        self.out_parts = p;
    }
}
