//! ONE UNIT of a wave (plans/edges-target.md "The forward"): a run of
//! frontier rows through the kernels. The sink (`UnitSink`, one per worker,
//! reset per unit) ORs each emission into its BUNDLE of its target's route
//! (`route::UnitBundles`: source key, transfer, offset, target key ->
//! target cells) - nothing is looked up and no row is copied here: the
//! target region's worker dedups the states and merges the edges
//! (`route::run_route`), and the new layer is decoded from the keys
//! (`wave`). At the unit's end its bundles are the chunks of their routes.

use anyhow::Result;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Col, Rt2, AV};
use rustc_hash::{FxHashMap, FxHashSet};

use super::decode::Skeletons;
use super::route::{Chunk, Routes, Sources, UnitBundles, NO_XFER};
use super::visited::{Key, VisitedSet};
use crate::frame::{Filters, Raised};
use celeste_engine::exact::{is_provisional, KeySpace, Provisional, RowKey};

/// A typed varying column of buffered rows: raw 16.16 words, (low, high)
/// pairs, or a byte per bool (0 false, 1 true, 2 unknown).
pub enum TCol {
    Num(Vec<u32>),
    Ival(Vec<(u32, u32)>),
    Bool(Vec<u8>),
}

/// Rows of one shape, columnar: the shape's skeleton (the union of its
/// outcome templates' varying cells), a `TCol` per varying cell, key and
/// cell per row. A unit buffers rows here only to look at them: the
/// coarser level's verdicts, the key check, the decode guard.
pub struct RowBuf {
    pub shape: u64,
    skeleton: Rt2,
    /// `(cell, column)` per varying cell, in the kernel's order.
    pub cols: Vec<(usize, TCol)>,
    pub keys: Vec<Key>,
    pub cells: Vec<u32>,
}

impl RowBuf {
    /// An empty buffer over `skeleton`.
    pub fn new(skeleton: Rt2) -> Self {
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
    pub fn skeleton(&self) -> &Rt2 {
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
    pub fn to_rt2(&self) -> Rt2 {
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
                TCol::Num(v) => Col::N(v.iter().map(|&x| P8::from_raw(x as i32)).collect()),
                TCol::Ival(v) => Col::I(v.iter().map(|&(a, c)| (P8::from_raw(a as i32), P8::from_raw(c as i32))).collect()),
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
    pub fn push_row(&mut self, row: &Rt2, key: Key, cell: u32) {
        debug_assert_eq!(row.width, 1);
        for (c, col) in self.cols.iter_mut() {
            let v = row.cols[*c].at(0);
            match (col, v) {
                (TCol::Num(d), AV::Num(n)) => d.push(n.as_raw_u32()),
                (TCol::Ival(d), AV::Ival(a, b)) => d.push((a.as_raw_u32(), b.as_raw_u32())),
                (TCol::Ival(d), AV::Num(n)) => d.push((n.as_raw_u32(), n.as_raw_u32())),
                (TCol::Bool(d), AV::Bool(x)) => d.push(x as u8),
                (TCol::Bool(d), AV::UBool) => d.push(2),
                (_, v) => panic!("emitted row's cell {c} holds {v:?}, not its column's kind"),
            }
        }
        self.keys.push(key);
        self.cells.push(cell);
    }
}

/// A target's verdict under the coarser level's filter (`UnitSink::decide`).
const VERDICT_PENDING: u8 = 0;
const VERDICT_YES: u8 = 1;
const VERDICT_NO: u8 = 2;
/// Rows awaiting a verdict before a batch is decided.
const VERDICT_BATCH: usize = 1024;

/// Entries of `UnitSink::xfer_id_raw`'s cache.
const XFER_CACHE: usize = 1 << 12;

/// What a raise's units need beyond a wave's: which sources are the tree's.
#[derive(Clone, Copy)]
pub struct RaiseCtx {
    pub raised: Raised,
}

/// A (route, key, cell) target as the coarser level's verdicts name it.
type Target = (u32, Key, u8);

/// One worker's sink for a frame's units (module doc).
pub struct UnitSink<'a> {
    keys: &'a KeySpace,
    routes: &'a Routes,
    skeletons: &'a Skeletons,
    /// The wave's provisional keys (states holding a code the key space
    /// lacks: new by construction).
    prov: &'a Provisional,
    filters: Filters<'a>,
    frame: u32,
    /// The level the wave runs at (a reference row's key view).
    level: crate::abstraction::Level,
    pub worker: u32,
    note_drops: bool,
    drops: Option<&'a super::wave::DropNotes>,
    raise: Option<RaiseCtx>,
    record_edges: bool,
    /// The unit's: its index, its block's index in the frontier, first
    /// lane, and the block's first row's index in the wave's frontier.
    unit: u32,
    block: u32,
    lo: usize,
    src_base: u32,
    /// Are the unit's sources the tree's (a raise; not the raise's own)?
    pub sources_old: bool,
    /// Lanes of the block being run that must not be expanded.
    pub skip_in: Option<&'a [bool]>,
    /// This worker's routes, and the last one asked.
    route_ids: FxHashMap<(u64, u32), u32>,
    last_route: (u64, u32, u32),
    /// The wave's frontier as sources (`None`: no edges recorded).
    sources: Option<&'a Sources>,
    /// The unit's bundles by route, and its (route, source region) pairs.
    bundles: UnitBundles,
    src_regions: Vec<(u32, u32)>,
    /// The unit's distinct sources (shape, key), and per lane from its
    /// first its index there.
    unit_sources: Vec<(u32, Key)>,
    src_local: Vec<u32>,
    src_idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    /// Per target the coarser level's verdict (`MarkFilter`), once a unit,
    /// decided in batches (a projection per block, not per row).
    verdicts: rustc_hash2::FxHashMap<Target, u8>,
    /// The rows awaiting a verdict, per shape, and their targets.
    scratch: Vec<RowBuf>,
    scratch_at: Vec<Vec<Target>>,
    /// Some target of the unit was disallowed: its records go at the
    /// unit's end.
    disallowed: bool,
    /// `--key-check`: every emission's row, checked in batches (its key,
    /// and that its key and cell decode to it).
    key_check: bool,
    check: Vec<RowBuf>,
    /// `--phases`: the wave's phase timers (`frame::phases`).
    pub phases: bool,
    /// The level -1 table of `filters.minus_one` (or a raise's old filter:
    /// the same table), resolved once.
    l1: Option<&'static crate::trace::level_minus_one::CostToGo>,
    drop_last: Option<((u64, u32), Option<u32>)>,
    xfer_ids: FxHashMap<crate::search::arc_edges::Pair, u32>,
    xfer_cache: Vec<(crate::compiled::asm_kernel::RawWords, u32)>,
    /// The worker's transfer table (the wave's gather reads it), and its
    /// length.
    xfer_tab: &'a std::sync::RwLock<Vec<crate::search::arc_edges::Pair>>,
    xfer_len: u32,
    /// The units' chunks.
    pub outbox: Vec<Chunk>,
    /// Record `edges` (the pos graph) at all.
    pub edges_on: bool,
    /// The distinct pos-graph edges this worker produced.
    pub pos_edges: FxHashSet<(u32, u32)>,
    /// Emissions past level -1 (each one a record).
    pub emitted: u64,
    /// `--emit-capture`: this worker's stream (`bench`).
    capture: Option<super::bench::CaptureWriter>,
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

impl<'a> UnitSink<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        visited: &'a VisitedSet,
        routes: &'a Routes,
        skeletons: &'a Skeletons,
        prov: &'a Provisional,
        filters: Filters<'a>,
        frame: u32,
        level: crate::abstraction::Level,
        worker: u32,
        xfer_tab: &'a std::sync::RwLock<Vec<crate::search::arc_edges::Pair>>,
        sources: Option<&'a Sources>,
        edges_on: bool,
        drops: Option<&'a super::wave::DropNotes>,
        raise: Option<RaiseCtx>,
        s: &crate::settings::Settings,
    ) -> Self {
        let record_edges = sources.is_some();
        let l1 = filters.minus_one.or(raise.as_ref().map(|r| r.raised.old)).map(|m| m.table(s));
        UnitSink {
            keys: &visited.keys,
            routes,
            skeletons,
            prov,
            filters,
            frame,
            level,
            worker,
            note_drops: filters.notes_drops() && record_edges && drops.is_some(),
            drops,
            raise,
            record_edges,
            unit: 0,
            block: 0,
            lo: 0,
            src_base: 0,
            sources_old: false,
            skip_in: None,
            route_ids: FxHashMap::default(),
            last_route: (0, u32::MAX, u32::MAX),
            sources,
            bundles: UnitBundles::default(),
            src_regions: Vec::new(),
            unit_sources: Vec::new(),
            src_local: Vec::new(),
            src_idx: Default::default(),
            verdicts: Default::default(),
            scratch: Vec::new(),
            scratch_at: Vec::new(),
            disallowed: false,
            key_check: s.diag.key_check,
            check: Vec::new(),
            phases: s.diag.phases,
            l1,
            drop_last: None,
            xfer_ids: Default::default(),
            xfer_cache: Vec::new(),
            xfer_tab,
            xfer_len: 0,
            outbox: Vec::new(),
            edges_on,
            pos_edges: Default::default(),
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

    /// The tree's key space (read-only during the wave).
    #[inline]
    pub fn keys(&self) -> &'a KeySpace {
        self.keys
    }

    /// The wave's provisional keys.
    #[inline]
    pub fn provisional(&self) -> &'a Provisional {
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

    /// Are edges recorded (an edges dir, the frontier's sources)?
    #[inline]
    pub fn records_edges(&self) -> bool {
        self.record_edges
    }

    /// Start unit `unit` of the wave: lanes `lo..lo + n` of frontier block
    /// `block`, whose row 0 is row `src_base` of the wave's frontier
    /// (`route::Sources`; `ids`: the lanes' ids, for the capture).
    #[allow(clippy::too_many_arguments)]
    pub fn begin(&mut self, unit: u32, block: u32, lo: usize, src_base: u32, ids: &[super::StateId], skip: Option<&'a [bool]>, sources_old: bool) {
        if let Some(c) = &mut self.capture {
            c.unit(unit, block, lo, ids, sources_old);
        }
        self.unit = unit;
        self.block = block;
        self.lo = lo;
        self.src_base = src_base;
        self.skip_in = skip;
        self.sources_old = sources_old;
        self.bundles.clear();
        self.src_regions.clear();
        self.verdicts.clear();
        self.disallowed = false;
        // The unit's sources: its lanes' (shape, key)s, distinct.
        self.unit_sources.clear();
        self.src_local.clear();
        self.src_idx.clear();
        if let Some(sources) = self.sources {
            let rows = &sources.rows[src_base as usize + lo..src_base as usize + lo + ids.len()];
            let mut last: Option<((u32, Key), u32)> = None;
            for r in rows {
                let s = (r.shape, r.key);
                let k = match last {
                    Some((l, k)) if l == s => k,
                    _ => {
                        let n = self.unit_sources.len() as u32;
                        let k = *self.src_idx.entry(s).or_insert(n);
                        if k == n {
                            self.unit_sources.push(s);
                        }
                        last = Some((s, k));
                        k
                    }
                };
                self.src_local.push(k);
            }
        }
    }

    /// The level -1 filter at EMISSION: `Some(from)` where it drops every
    /// row of (`shape`, `cell`) at this frame, so the row is never keyed or
    /// looked up. Half the emissions of room (6,2) 100% f57.
    #[inline]
    pub fn minus_one_drop(&mut self, shape: u64, cell: u32) -> Option<u32> {
        let m = self.filters.minus_one?;
        if let Some((at, r)) = self.drop_last {
            if at == (shape, cell) {
                return r;
            }
        }
        let from = self.l1.expect("the level -1 table").admitted_from(shape, cell, self.frame);
        let r = (from > m.h).then_some(from);
        self.drop_last = Some(((shape, cell), r));
        r
    }

    /// A row level -1 dropped, from `lane`: its source is noted (`from`:
    /// the smallest horizon admitting it), in a level-0 tree a raise extends.
    #[inline]
    pub fn dropped(&mut self, lane: usize, from: u32) {
        if let Some(c) = &mut self.capture {
            c.dropped((lane - self.lo) as u32, from);
        }
        if let (true, Some(d)) = (self.note_drops, self.drops) {
            d.note(self.block, lane, from);
        }
    }

    /// `xfer_id` of a raw transfer, through a small direct-mapped cache of
    /// raw -> id: a frame has few distinct transfers (room (1,0) to f99:
    /// 17k) against one lookup per emitted lane, and the decode plus the
    /// hash-map intern was ~17% of a forward.
    #[inline]
    pub fn xfer_id_raw(&mut self, words: &crate::compiled::asm_kernel::RawWords, decode: impl FnOnce(&crate::compiled::asm_kernel::RawWords) -> crate::search::arc_edges::Pair) -> u32 {
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

    /// The transfer pair `t` as this worker's id (interned on first use).
    pub fn xfer_id(&mut self, t: crate::search::arc_edges::Pair) -> u32 {
        if let Some(&id) = self.xfer_ids.get(&t) {
            return id;
        }
        let id = self.xfer_len;
        assert!(id < NO_XFER, "worker {}: {id} transfers in one frame", self.worker);
        self.xfer_tab.write().expect("a transfer table").push(t);
        self.xfer_len += 1;
        self.xfer_ids.insert(t, id);
        id
    }

    /// The transfers this worker has met (its table's length).
    pub fn xfers_met(&self) -> usize {
        self.xfer_len as usize
    }

    /// The route of `(shape, slot)`.
    #[inline]
    fn route(&mut self, shape: u64, slot: u32) -> u32 {
        if (self.last_route.0, self.last_route.1) == (shape, slot) {
            return self.last_route.2;
        }
        let r = match self.route_ids.get(&(shape, slot)) {
            Some(&r) => r,
            None => {
                let r = self.routes.id(shape, slot);
                self.route_ids.insert((shape, slot), r);
                r
            }
        };
        self.last_route = (shape, slot, r);
        r
    }

    /// The buffer of `shape` in `bufs` (made over `init()`'s skeleton).
    #[inline]
    fn buf_of(bufs: &mut Vec<RowBuf>, shape: u64, init: impl FnOnce() -> Rt2) -> usize {
        match bufs.iter().position(|b| b.shape == shape) {
            Some(i) => i,
            None => {
                bufs.push(RowBuf::new(init()));
                bufs.len() - 1
            }
        }
    }

    /// ONE EMISSION: the state `(shape, key, cell)` reached from lane `lane`
    /// with transfer `xfer` (`None`: the reference engine's path, no edge).
    /// `init` is the shape's skeleton (the new layer's rows are decoded
    /// with it: `Skeletons`); `push` appends the row to a buffer, when the
    /// row is looked at (a verdict, the key check, the decode guard).
    #[inline]
    #[allow(clippy::too_many_arguments)]
    pub fn emit(
        &mut self,
        shape: u64,
        cell: u32,
        key: Key,
        lane: usize,
        xfer: Option<u32>,
        init: impl Fn() -> Rt2,
        mut push: impl FnMut(&mut RowBuf),
    ) -> Result<()> {
        // A raise re-expands sources of the tree: where the tree's own
        // filter admitted this cell, the edges are recorded already and the
        // state is in the tree.
        if let Some(r) = &self.raise {
            if self.sources_old && self.l1.expect("the level -1 table").admitted_from(shape, cell, self.frame) <= r.raised.old.h {
                return Ok(());
            }
        }
        self.emitted += 1;
        if let Some(c) = &mut self.capture {
            c.emit(shape, cell, key, (lane - self.lo) as u32, xfer, is_provisional(key).then(|| self.prov.content(key)));
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
        if self.last_shape != shape && !self.reference && !self.rowless && !is_provisional(key) {
            let mut b = RowBuf::new(init());
            push(&mut b);
            self.skeletons.check_decodes(self.keys, &b.to_rt2(), &[cell])?;
            self.guarded.insert(shape);
            self.last_shape = shape;
        }
        if self.key_check && !self.reference && !self.rowless {
            let i = Self::buf_of(&mut self.check, shape, &init);
            push(&mut self.check[i]);
            if self.check[i].rows() >= 256 {
                self.check_rows(i)?;
            }
        }
        let (slot, local) = super::GEOMETRY.slot_local(cell);
        let route = self.route(shape, slot);
        // The coarser level's filter (the objects ladder): once per target,
        // in batches - a target awaiting its verdict is taken as allowed, and
        // its records go at the unit's end if it is not.
        if self.filters.marks.is_some() {
            let at: Target = (route, key, local as u8);
            match self.verdicts.get(&at) {
                Some(&VERDICT_NO) => return Ok(()),
                Some(_) => {}
                None => {
                    let i = Self::buf_of(&mut self.scratch, shape, &init);
                    if self.scratch_at.len() <= i {
                        self.scratch_at.resize_with(i + 1, Vec::new);
                    }
                    push(&mut self.scratch[i]);
                    self.scratch_at[i].push(at);
                    self.verdicts.insert(at, VERDICT_PENDING);
                    if self.scratch[i].rows() >= VERDICT_BATCH {
                        self.decide()?;
                    }
                }
            }
        }
        let bit = 1u64 << local;
        match (xfer, self.sources) {
            (Some(x), Some(sources)) => {
                let s = &sources.rows[self.src_base as usize + lane];
                if self.src_regions.last() != Some(&(route, s.region)) {
                    self.src_regions.push((route, s.region));
                }
                let sl = self.src_local[lane - self.lo];
                self.bundles.insert(route, key, sl, x, super::bundle::offset(s.cell, cell), bit);
            }
            _ => self.bundles.insert(route, key, 0, NO_XFER, 0, bit),
        }
        Ok(())
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
    pub fn row_key(&mut self, row: &Rt2) -> (Rt2, Key) {
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

    /// The reference engine's emission: one materialized row (stored
    /// widened: `row_key`), no edge. Its shape's skeleton is the row's
    /// structure with every value cell left to the decode (`decode::Repr::
    /// Other`): a reference row's columns follow its values, not a kernel's
    /// template.
    pub fn emit_row(&mut self, row: &Rt2, cell: u32) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        if self.minus_one_drop(row.shape_hash, cell).is_some() {
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
        self.reference = true;
        let r = self.emit(w.shape_hash, cell, key, self.lo, None, init, |b| b.push_row(&w, key, cell));
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
            Some(c) => c.finish(&self.xfer_tab.read().expect("a transfer table")),
            None => Ok(()),
        }
    }

    /// The unit is over: its rows checked, its last verdicts decided (a
    /// disallowed target's records go), its buckets the chunks of their
    /// routes.
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
        let mut srcs = self.src_regions.iter().peekable();
        for (route, targets, bundles) in buckets {
            let mut src_regions = Vec::new();
            while let Some(&&(r, region)) = srcs.peek() {
                if r > route {
                    break;
                }
                if r == route {
                    src_regions.push(region);
                }
                srcs.next();
            }
            self.outbox.push(Chunk { route, unit: self.unit, worker: self.worker, targets, sources: sources.clone(), bundles, src_regions });
        }
        crate::frame::phases::add(crate::frame::phases::END_UNIT, t);
        Ok(())
    }
}
