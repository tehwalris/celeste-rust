//! ONE UNIT of a wave (plans/storage-v2.md "The forward wave"): a run of
//! frontier rows through the kernels, every emission against the visited
//! set. The sink (`UnitSink`, one per worker, reset per unit) names each
//! distinct target entry by a LID - (target shape, region slot, key) - and
//! looks it up in the visited set ONCE (read-only during the wave), keeping
//! the owner's entry number and cell mask: an emission into a cell the owner
//! holds is an old state (an edge only); any other is a REQUEST, its row
//! copied (the only copy), unless the unit requested that cell already. The
//! edges `(lid, cell, source, transfer)` are sorted and encoded into the
//! unit's block at its end (`edges::encode_block`); the translation
//! (`wave`) gives the lids their owners.

use anyhow::Result;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Col, Rt2, AV};

use super::visited::{Key, VisitedSet};
use super::{Geometry, StateId};
use crate::frame::{Filters, Raised};

/// A typed varying column of buffered rows: raw 16.16 words, (low, high)
/// pairs, or a byte per bool (0 false, 1 true, 2 unknown).
pub enum TCol {
    Num(Vec<u32>),
    Ival(Vec<(u32, u32)>),
    Bool(Vec<u8>),
}

/// Rows of one shape, columnar: the shape's skeleton (the union of its
/// outcome templates' varying cells), a `TCol` per varying cell, key and
/// cell per row. A unit buffers its requested rows here.
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

    /// Bytes allocated for the rows (capacities).
    pub fn alloc_bytes(&self) -> usize {
        self.cols
            .iter()
            .map(|(_, c)| match c {
                TCol::Num(v) => v.capacity() * 4,
                TCol::Ival(v) => v.capacity() * 8,
                TCol::Bool(v) => v.capacity(),
            })
            .sum::<usize>()
            + self.keys.capacity() * 16
            + self.cells.capacity() * 4
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

    /// An empty block of this shape for the rows to land in.
    fn empty_piece(&self) -> Rt2 {
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
        b
    }

    /// Append `rows` to `piece` (same shape); cells it holds differently go
    /// through `col_push`.
    fn gather_into(&self, piece: &mut Rt2, rows: &[u32]) {
        use celeste_engine::runtime2::col_push;
        let w = piece.width;
        let mut ti = 0;
        for (cell, c) in self.skeleton.cols.iter().enumerate() {
            if !matches!(piece.structure[cell], celeste_engine::runtime2::Cell2::Val) {
                continue;
            }
            let typed = if ti < self.cols.len() && self.cols[ti].0 == cell {
                ti += 1;
                Some(&self.cols[ti - 1].1)
            } else {
                None
            };
            let dst = &mut piece.cols[cell];
            match (typed, c) {
                (None, Col::U(v)) => {
                    if !matches!(dst, Col::U(d) if d == v) {
                        for (n, _) in rows.iter().enumerate() {
                            col_push(dst, w + n, *v);
                        }
                    }
                }
                (None, _) => unreachable!("a non-uniform skeleton column without a typed column"),
                (Some(TCol::Num(v)), _) => match dst {
                    Col::N(d) => d.extend(rows.iter().map(|&r| P8::from_raw(v[r as usize] as i32))),
                    _ => {
                        for (n, &r) in rows.iter().enumerate() {
                            col_push(dst, w + n, AV::Num(P8::from_raw(v[r as usize] as i32)));
                        }
                    }
                },
                (Some(TCol::Ival(v)), _) => match dst {
                    Col::I(d) => d.extend(rows.iter().map(|&r| {
                        let (a, b) = v[r as usize];
                        (P8::from_raw(a as i32), P8::from_raw(b as i32))
                    })),
                    _ => {
                        for (n, &r) in rows.iter().enumerate() {
                            let (a, b) = v[r as usize];
                            col_push(dst, w + n, AV::Ival(P8::from_raw(a as i32), P8::from_raw(b as i32)));
                        }
                    }
                },
                (Some(TCol::Bool(v)), _) => {
                    let av = |r: u32| match v[r as usize] {
                        0 => AV::Bool(false),
                        1 => AV::Bool(true),
                        _ => AV::UBool,
                    };
                    match dst {
                        Col::V(d) => d.extend(rows.iter().map(|&r| av(r))),
                        _ => {
                            for (n, &r) in rows.iter().enumerate() {
                                col_push(dst, w + n, av(r));
                            }
                        }
                    }
                }
            }
        }
        piece.row_keys.extend(rows.iter().map(|&r| self.keys[r as usize]));
        piece.width = w + rows.len();
    }

    /// The whole buffer as a block.
    pub fn to_rt2(&self) -> Rt2 {
        self.rows_rt2(&(0..self.rows() as u32).collect::<Vec<_>>())
    }

    /// `rows` as a block.
    pub fn rows_rt2(&self, rows: &[u32]) -> Rt2 {
        let mut b = self.empty_piece();
        self.gather_into(&mut b, rows);
        b
    }

    /// Do `self` and `other` lay rows out alike: the same skeleton values
    /// and the same typed cells (a shape's buffers from one engine do)?
    pub fn same_layout(&self, other: &RowBuf) -> bool {
        self.skeleton.cols == other.skeleton.cols && self.cols.len() == other.cols.len() && self.cols.iter().zip(&other.cols).all(|(a, b)| a.0 == b.0)
    }

    /// The rows `rows` (`(buffer, row)` into `bufs`, one shape, one layout:
    /// `same_layout`) as one block, every column canonical
    /// (`collapse_uniform`): a function of the rows' values.
    pub fn gather_rows(bufs: &[&RowBuf], rows: &[(u32, u32)]) -> Rt2 {
        use celeste_engine::runtime2::collapse_uniform;
        let t = bufs[0];
        debug_assert!(bufs.iter().all(|b| b.same_layout(t)));
        let mut piece = t.empty_piece();
        for (k, (cell, tcol)) in t.cols.iter().enumerate() {
            let col = match tcol {
                TCol::Num(_) => Col::N(
                    rows.iter()
                        .map(|&(b, r)| match &bufs[b as usize].cols[k].1 {
                            TCol::Num(v) => P8::from_raw(v[r as usize] as i32),
                            _ => unreachable!("one layout"),
                        })
                        .collect(),
                ),
                TCol::Ival(_) => Col::I(
                    rows.iter()
                        .map(|&(b, r)| match &bufs[b as usize].cols[k].1 {
                            TCol::Ival(v) => {
                                let (a, c) = v[r as usize];
                                (P8::from_raw(a as i32), P8::from_raw(c as i32))
                            }
                            _ => unreachable!("one layout"),
                        })
                        .collect(),
                ),
                TCol::Bool(_) => Col::V(
                    rows.iter()
                        .map(|&(b, r)| match &bufs[b as usize].cols[k].1 {
                            TCol::Bool(v) => match v[r as usize] {
                                0 => AV::Bool(false),
                                1 => AV::Bool(true),
                                _ => AV::UBool,
                            },
                            _ => unreachable!("one layout"),
                        })
                        .collect(),
                ),
            };
            piece.cols[*cell] = collapse_uniform(col);
        }
        piece.row_keys = rows.iter().map(|&(b, r)| bufs[b as usize].keys[r as usize]).collect();
        piece.width = rows.len();
        piece
    }

    /// Do rows `a` and `b` hold the same values (key and cell included)?
    pub fn same_row(&self, a: u32, other: &RowBuf, b: u32) -> bool {
        let (a, b) = (a as usize, b as usize);
        self.keys[a] == other.keys[b]
            && self.cells[a] == other.cells[b]
            && self.cols.len() == other.cols.len()
            && self.cols.iter().zip(&other.cols).all(|((ca, x), (cb, y))| {
                ca == cb
                    && match (x, y) {
                        (TCol::Num(x), TCol::Num(y)) => x[a] == y[b],
                        (TCol::Ival(x), TCol::Ival(y)) => x[a] == y[b],
                        (TCol::Bool(x), TCol::Bool(y)) => x[a] == y[b],
                        _ => false,
                    }
            })
    }

    /// Push one materialized row (the reference engine's path).
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

/// A unit's name for one target entry. Its OWNER (`UnitOut::owners`) is
/// found in the visited set when the lid is made, or given by the
/// translation.
#[derive(Clone, Copy, Debug)]
pub struct Lid {
    pub shape: u64,
    pub slot: u32,
    pub key: Key,
}

/// No owner (yet).
pub const NONE: u32 = u32::MAX;

/// A lid's owner `(region, entry)` packed in a u64 (`NO_OWNER`: none).
#[inline]
pub fn pack_owner(region: u32, entry: u32) -> u64 {
    (region as u64) << 32 | entry as u64
}

pub const NO_OWNER: u64 = u64::MAX;

/// A unit's lid owner while the unit runs: no owner, and another unit
/// claimed one of its states (`Claims`).
const PENDING: u64 = u64::MAX - 1;

/// A cell of a lid the unit asked the translation to add: its row is row
/// `row` of the unit's buffer `buf`.
#[derive(Clone, Copy, Debug)]
pub struct Request {
    pub lid: u32,
    pub local: u32,
    pub buf: u32,
    pub row: u32,
}

/// A finished unit: its sources, lids, requests, rows and edge block.
pub struct UnitOut {
    pub worker: u32,
    /// The frontier rows the unit ran, by lane from its first (edges name a
    /// source by its index here).
    pub sources: Vec<StateId>,
    pub lids: Vec<Lid>,
    /// Per lid its owner (`pack_owner`, `NO_OWNER`): set once, by the
    /// translation's worker of the lid's region where the visited set had
    /// none.
    pub owners: Vec<std::sync::atomic::AtomicU64>,
    pub requests: Vec<Request>,
    /// Lids without an owner whose state another unit claimed (`Claims`).
    pub pending: Vec<u32>,
    pub bufs: Vec<RowBuf>,
    /// The encoded edges (`edges::encode_block`): in the worker's blocks
    /// file at `block_at` (the wave's), or here (`block`); its length, per
    /// lid its edges' start, its edge count.
    pub block_at: Option<u64>,
    pub block_len: u64,
    pub block: Vec<u8>,
    /// The unit's transfers by use: an edge's transfer field is its rank
    /// here, which holds the worker's id.
    pub xfers: Vec<u32>,
    pub starts: Vec<u32>,
    pub edges: u64,
}

/// The states requested in a wave, across its units: the first unit to
/// request one copies its row and hands it to the translation; another
/// unit's lid of it finds it after the translation (`wave::resolve_lids`).
/// Under `CELESTE_KERNEL_KEY_CHECK=1` every request goes through, so the
/// translation compares their rows.
pub struct Claims {
    shards: Vec<std::sync::Mutex<rustc_hash::FxHashSet<(u64, u32, Key, u32)>>>,
}

impl Default for Claims {
    fn default() -> Self {
        Claims { shards: (0..CLAIM_SHARDS).map(|_| Default::default()).collect() }
    }
}

const CLAIM_SHARDS: usize = 1 << 12;

impl Claims {
    /// Is this the first request of `(shape, slot, key, cell)` this wave?
    #[inline]
    pub fn claim(&self, shape: u64, slot: u32, key: Key, local: u32) -> bool {
        let h = key.0 ^ celeste_engine::runtime2::mix64(shape ^ (slot as u64) << 8 ^ (local as u64) << 40);
        self.shards[(h >> 52) as usize & (CLAIM_SHARDS - 1)].lock().expect("a claims shard").insert((shape, slot, key, local))
    }
}

/// Bits of a packed edge (`lid | cell | source | transfer`, a u64 that sorts
/// by target): see `pack_edge`.
const LID_BITS: u32 = 20;
const LOCAL_BITS: u32 = 8;
const SRC_BITS: u32 = 13;
const XFER_BITS: u32 = 23;
/// Lanes a unit may span (its sources' indices fit `SRC_BITS`).
pub const MAX_UNIT_LANES: usize = 1 << SRC_BITS;

#[inline]
pub fn pack_edge(lid: u32, local: u32, src: u32, xfer: u32) -> u64 {
    (lid as u64) << (LOCAL_BITS + SRC_BITS + XFER_BITS) | (local as u64) << (SRC_BITS + XFER_BITS) | (src as u64) << XFER_BITS | xfer as u64
}

#[inline]
pub fn unpack_edge(e: u64) -> (u32, u32, u32, u32) {
    (
        (e >> (LOCAL_BITS + SRC_BITS + XFER_BITS)) as u32,
        (e >> (SRC_BITS + XFER_BITS)) as u32 & ((1 << LOCAL_BITS) - 1),
        (e >> XFER_BITS) as u32 & ((1 << SRC_BITS) - 1),
        e as u32 & ((1 << XFER_BITS) - 1),
    )
}

/// Entries of `UnitSink::xfer_id_raw`'s cache.
const XFER_CACHE: usize = 1 << 12;

/// What a raise's units need beyond a wave's: the layer of every state, to
/// refuse a hit from a later layer (`StateLayers`).
pub struct RaiseCtx<'a> {
    pub raised: Raised,
    pub layers: &'a super::wave::StateLayers,
}

/// One worker's sink for a frame's units (module doc).
pub struct UnitSink<'a> {
    visited: &'a VisitedSet,
    claims: &'a Claims,
    geo: Geometry,
    filters: Filters<'a>,
    frame: u32,
    pub worker: u32,
    note_drops: bool,
    drops: Option<&'a super::wave::DropNotes>,
    raise: Option<RaiseCtx<'a>>,
    record_edges: bool,
    /// The unit's: its block's index in the frontier, first lane, sources.
    block: u32,
    lo: usize,
    sources: Vec<StateId>,
    /// Are the unit's sources the tree's (a raise; not the raise's own)?
    pub sources_old: bool,
    /// Lanes of the block being run that must not be expanded.
    pub skip_in: Option<&'a [bool]>,
    lids: Vec<Lid>,
    /// Per lid its owner where the visited set has it (`pack_owner`).
    lid_owners: Vec<u64>,
    /// Per lid, `2 * words`: the owner's mask at the wave's start, then the
    /// cells this unit requested.
    lid_masks: Vec<u64>,
    lid_index: Vec<u32>,
    edges: Vec<u64>,
    /// The end's counting sort: per lid its edges' start, and the sorted edges.
    sort_at: Vec<u32>,
    sorted: Vec<u64>,
    /// The end's transfer ranks: per worker id its count, then its rank;
    /// the last unit's distinct transfers (a capacity).
    xfer_use: Vec<u32>,
    last_used: usize,
    requests: Vec<Request>,
    bufs: Vec<RowBuf>,
    /// Per (lid, cell) the coarser level's verdict (`MarkFilter`), once a unit.
    verdicts: rustc_hash::FxHashMap<u64, bool>,
    /// A scratch row for a verdict, per shape.
    scratch: Vec<RowBuf>,
    /// `CELESTE_KERNEL_KEY_CHECK=1`: every emission's row, checked in batches.
    check: Vec<RowBuf>,
    drop_last: Option<((u64, u32), Option<u32>)>,
    xfer_ids: rustc_hash::FxHashMap<crate::search::arc_edges::Pair, u32>,
    xfer_cache: Vec<(crate::compiled::asm_kernel::RawWords, u32)>,
    pub xfer_tab: Vec<crate::search::arc_edges::Pair>,
    pub outs: Vec<UnitOut>,
    /// Record `edges` (the pos graph) at all.
    pub edges_on: bool,
    /// The distinct pos-graph edges this worker produced.
    pub pos_edges: rustc_hash::FxHashSet<(u32, u32)>,
    /// Emissions past level -1 (each one a lookup and an edge).
    pub emitted: u64,
    pub n_requests: u64,
    pub n_lids: u64,
    /// Requests another unit had claimed (`Claims`).
    pub claimed_elsewhere: u64,
    /// The unit's lids without an owner whose state another unit claimed:
    /// resolved after the translation (`wave::resolve_lids`).
    pending: Vec<u32>,
    /// TSC ticks of the units' ends (sort and encode), `phases`.
    pub end_ticks: u64,
    /// `CELESTE_EMIT_CAPTURE`: this worker's stream (`bench`).
    capture: Option<super::bench::CaptureWriter>,
    /// The worker's blocks file (`edges::blocks_path`): units' blocks go
    /// there as they end; `None`: kept in memory.
    blocks: Option<super::edges::BlockWriter>,
}

impl<'a> UnitSink<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        visited: &'a VisitedSet,
        claims: &'a Claims,
        filters: Filters<'a>,
        frame: u32,
        worker: u32,
        record_edges: bool,
        edges_on: bool,
        drops: Option<&'a super::wave::DropNotes>,
        raise: Option<RaiseCtx<'a>>,
    ) -> Self {
        UnitSink {
            visited,
            claims,
            geo: visited.geo,
            filters,
            frame,
            worker,
            note_drops: filters.notes_drops() && record_edges && drops.is_some(),
            drops,
            raise,
            record_edges,
            block: 0,
            lo: 0,
            sources: Vec::new(),
            sources_old: false,
            skip_in: None,
            lids: Vec::new(),
            lid_owners: Vec::new(),
            lid_masks: Vec::new(),
            lid_index: Vec::new(),
            edges: Vec::new(),
            sort_at: Vec::new(),
            sorted: Vec::new(),
            xfer_use: Vec::new(),
            last_used: 0,
            requests: Vec::new(),
            bufs: Vec::new(),
            verdicts: Default::default(),
            scratch: Vec::new(),
            check: Vec::new(),
            drop_last: None,
            xfer_ids: Default::default(),
            xfer_cache: Vec::new(),
            xfer_tab: Vec::new(),
            outs: Vec::new(),
            edges_on,
            pos_edges: Default::default(),
            emitted: 0,
            n_requests: 0,
            n_lids: 0,
            claimed_elsewhere: 0,
            pending: Vec::new(),
            end_ticks: 0,
            capture: super::bench::CaptureWriter::open(frame, worker),
            blocks: None,
        }
    }

    /// Stream the units' blocks into `blocks` (the frame's index path's
    /// worker file) instead of keeping them.
    pub fn stream_blocks(&mut self, index: &std::path::Path) -> Result<()> {
        self.blocks = Some(super::edges::BlockWriter::create(&super::edges::blocks_path(index, self.worker))?);
        Ok(())
    }

    /// Are edges recorded (an edges dir, ids on the frontier)?
    #[inline]
    pub fn records_edges(&self) -> bool {
        self.record_edges
    }

    /// Start unit `unit` of the wave: lanes `lo..lo + sources.len()` of
    /// frontier block `block`.
    pub fn begin(&mut self, unit: u32, block: u32, lo: usize, sources: &[StateId], skip: Option<&'a [bool]>, sources_old: bool) {
        assert!(sources.len() <= MAX_UNIT_LANES, "a unit of {} lanes: at most {MAX_UNIT_LANES}", sources.len());
        if let Some(c) = &mut self.capture {
            c.unit(unit, block, lo, sources, sources_old);
        }
        self.block = block;
        self.lo = lo;
        self.sources.clear();
        self.sources.extend_from_slice(sources);
        self.skip_in = skip;
        self.sources_old = sources_old;
        self.lids.clear();
        self.lid_owners.clear();
        self.lid_masks.clear();
        let cap = (sources.len() * 16).next_power_of_two().max(1024);
        if self.lid_index.len() != cap {
            self.lid_index = vec![0; cap];
        } else {
            self.lid_index.fill(0);
        }
        self.edges.clear();
        self.pending.clear();
        self.requests.clear();
        self.bufs = Vec::new();
        self.verdicts.clear();
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
        let from = m.table().admitted_from(shape, cell, self.frame);
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
        let id = u32::try_from(self.xfer_tab.len()).expect("transfer table past u32");
        assert!(id < 1 << XFER_BITS, "worker {}: {id} transfers in one frame, past the {XFER_BITS} bits an edge holds", self.worker);
        self.xfer_tab.push(t);
        self.xfer_ids.insert(t, id);
        id
    }

    /// The lid of `(shape, slot, key)`, made (and looked up in the visited
    /// set) on first use.
    #[inline]
    fn lid(&mut self, shape: u64, slot: u32, key: Key) -> u32 {
        let words = self.geo.words;
        let m = self.lid_index.len() - 1;
        let h = key.0 ^ celeste_engine::runtime2::mix64(shape ^ (slot as u64) << 7);
        let mut i = (h as usize) & m;
        loop {
            let l = self.lid_index[i];
            if l == 0 {
                break;
            }
            let x = &self.lids[l as usize - 1];
            if x.key == key && x.shape == shape && x.slot == slot {
                return l - 1;
            }
            i = (i + 1) & m;
        }
        let l = self.lids.len() as u32;
        assert!(l < 1 << LID_BITS, "a unit with {l} distinct target entries, past the {LID_BITS} bits an edge holds");
        let (region, entry) = self.visited.find(shape, slot, key).unwrap_or((NONE, NONE));
        self.lids.push(Lid { shape, slot, key });
        self.lid_owners.push(if entry == NONE { NO_OWNER } else { pack_owner(region, entry) });
        match entry {
            NONE => self.lid_masks.extend(std::iter::repeat_n(0, 2 * words)),
            e => {
                let t = self.visited.table(region).expect("a found entry's table");
                self.lid_masks.extend_from_slice(t.mask(words, e));
                self.lid_masks.extend(std::iter::repeat_n(0, words));
            }
        }
        self.lid_index[i] = l + 1;
        if (self.lids.len() + 1) * 2 > self.lid_index.len() {
            self.grow();
        }
        l
    }

    fn grow(&mut self) {
        let cap = self.lid_index.len() * 2;
        self.lid_index = vec![0; cap];
        let m = cap - 1;
        for (l, x) in self.lids.iter().enumerate() {
            let h = x.key.0 ^ celeste_engine::runtime2::mix64(x.shape ^ (x.slot as u64) << 7);
            let mut i = (h as usize) & m;
            while self.lid_index[i] != 0 {
                i = (i + 1) & m;
            }
            self.lid_index[i] = l as u32 + 1;
        }
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
    /// `push` appends its row to a buffer when the row is needed (a
    /// request, a verdict, the key check); `init` is the shape's skeleton.
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
            if self.sources_old && r.raised.old.table().admitted_from(shape, cell, self.frame) <= r.raised.old.h {
                return Ok(());
            }
        }
        self.emitted += 1;
        if let Some(c) = &mut self.capture {
            c.emit(shape, cell, key, (lane - self.lo) as u32, xfer);
        }
        if crate::compiled::asm_kernel::key_check_on() {
            let i = Self::buf_of(&mut self.check, shape, &init);
            push(&mut self.check[i]);
            if self.check[i].rows() >= 256 {
                crate::compiled::asm_kernel::key_check(&self.check[i]);
                self.check[i].clear();
            }
        }
        let words = self.geo.words;
        let (slot, local) = self.geo.slot_local(cell);
        let lid = self.lid(shape, slot, key);
        // The coarser level's filter (the objects ladder): once per target.
        if let Some(f) = self.filters.marks {
            let at = (lid as u64) << 8 | local as u64;
            let ok = match self.verdicts.get(&at) {
                Some(&ok) => ok,
                None => {
                    let i = Self::buf_of(&mut self.scratch, shape, &init);
                    self.scratch[i].clear();
                    push(&mut self.scratch[i]);
                    let ok = f.allowed(&self.scratch[i].to_rt2(), self.frame)?[0];
                    self.verdicts.insert(at, ok);
                    ok
                }
            };
            if !ok {
                return Ok(());
            }
        }
        let (w, b) = ((local / 64) as usize, 1u64 << (local % 64));
        let base = lid as usize * 2 * words;
        if self.lid_masks[base + w] & b != 0 {
            // An old state; a raise refuses one from a later layer.
            if let Some(r) = &self.raise {
                let o = self.lid_owners[lid as usize];
                let layer = r.layers.layer(super::state_id((o >> 32) as u32, o as u32, local));
                anyhow::ensure!(
                    layer <= self.frame,
                    "frame {}: a state the tree first reached at frame {layer} is reached at frame {}: the raised tree would move it to an earlier layer (the level -1 table is not consistent along this edge), so it cannot be extended exactly - delete the tree",
                    self.frame,
                    self.frame
                );
            }
        } else if self.lid_masks[base + words + w] & b == 0 {
            self.lid_masks[base + words + w] |= b;
            // Another unit may hold the state's row already: only the
            // first copies it (`Claims`).
            if !crate::compiled::asm_kernel::key_check_on() && !self.claims.claim(shape, slot, key, local) {
                self.claimed_elsewhere += 1;
                if self.lid_owners[lid as usize] == NO_OWNER {
                    // Marked pending once: its owner slot says so.
                    self.lid_owners[lid as usize] = PENDING;
                    self.pending.push(lid);
                }
                if let (Some(x), true) = (xfer, self.record_edges) {
                    self.edges.push(pack_edge(lid, local, (lane - self.lo) as u32, x));
                }
                return Ok(());
            }
            let i = Self::buf_of(&mut self.bufs, shape, &init);
            push(&mut self.bufs[i]);
            debug_assert_eq!(self.bufs[i].cells.last(), Some(&cell));
            self.requests.push(Request { lid, local, buf: i as u32, row: self.bufs[i].rows() as u32 - 1 });
        }
        if let (Some(x), true) = (xfer, self.record_edges) {
            self.edges.push(pack_edge(lid, local, (lane - self.lo) as u32, x));
        }
        Ok(())
    }

    /// The reference engine's emission: one materialized row, no edge.
    pub fn emit_row(&mut self, row: &Rt2, cell: u32) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        debug_assert_eq!(row.row_keys.len(), 1, "an emitted row carries its key");
        if self.minus_one_drop(row.shape_hash, cell).is_some() {
            return Ok(());
        }
        let init = || {
            // Numbers and bools vary; the rest is fixed by the shape.
            let mut b = celeste_engine::slots::reshape(row, 0);
            b.cols = row
                .cols
                .iter()
                .map(|c| match c.at(0) {
                    AV::Num(_) => Col::N(Vec::new()),
                    AV::Ival(..) => Col::I(Vec::new()),
                    AV::Bool(_) | AV::UBool => Col::V(Vec::new()),
                    v => Col::U(v),
                })
                .collect();
            b.shape_hash = row.shape_hash;
            b
        };
        let key = row.row_keys[0];
        self.emit(row.shape_hash, cell, key, self.lo, None, init, |b| b.push_row(row, key, cell))
    }

    /// The worker's units are over: its blocks file complete, its capture
    /// stream and transfer table written (`CELESTE_EMIT_CAPTURE`).
    pub fn finish(&mut self) -> Result<()> {
        if let Some(b) = self.blocks.take() {
            b.finish()?;
        }
        match self.capture.take() {
            Some(c) => c.finish(&self.xfer_tab),
            None => Ok(()),
        }
    }

    /// The unit is over: its edges sorted, deduplicated and encoded; its
    /// lids, requests and rows kept for the translation.
    pub fn end(&mut self) -> Result<()> {
        let t = crate::frame::phases::start();
        for b in &self.check {
            crate::compiled::asm_kernel::key_check(b);
        }
        self.check.iter_mut().for_each(RowBuf::clear);
        // One pass: per (lid, cell) its edges' count, per transfer its use.
        let cells = (self.geo.side * self.geo.side) as usize;
        let n = self.lids.len();
        if self.xfer_use.len() < self.xfer_tab.len() {
            self.xfer_use.resize(self.xfer_tab.len(), 0);
        }
        self.sort_at.clear();
        self.sort_at.resize(n * cells + 1, 0);
        let mut used: Vec<u32> = Vec::with_capacity(self.last_used);
        for &e in &self.edges {
            let (lid, local, _, x) = unpack_edge(e);
            self.sort_at[lid as usize * cells + local as usize + 1] += 1;
            if self.xfer_use[x as usize] == 0 {
                used.push(x);
            }
            self.xfer_use[x as usize] += 1;
        }
        // The unit's transfers by use (common ones code in a byte): each
        // edge's transfer field becomes its rank.
        used.sort_unstable_by_key(|&x| (std::cmp::Reverse(self.xfer_use[x as usize]), x));
        for (r, &x) in used.iter().enumerate() {
            self.xfer_use[x as usize] = r as u32;
        }
        for k in 0..n * cells {
            self.sort_at[k + 1] += self.sort_at[k];
        }
        // By target (a counting sort on (lid, cell)), the rank in place of
        // the transfer; each target's few edges sorted (source, transfer),
        // duplicates dropped.
        self.sorted.clear();
        self.sorted.resize(self.edges.len(), 0);
        {
            let xmask = (1u64 << XFER_BITS) - 1;
            let mut cur = self.sort_at.clone();
            for &e in &self.edges {
                let (lid, local, _, x) = unpack_edge(e);
                let k = lid as usize * cells + local as usize;
                self.sorted[cur[k] as usize] = e & !xmask | self.xfer_use[x as usize] as u64;
                cur[k] += 1;
            }
        }
        for &x in &used {
            self.xfer_use[x as usize] = 0;
        }
        self.last_used = used.len();
        for k in 0..n * cells {
            let (a, b) = (self.sort_at[k] as usize, self.sort_at[k + 1] as usize);
            if b - a > 1 {
                self.sorted[a..b].sort_unstable();
            }
        }
        self.sorted.dedup();
        let (block, starts) = super::edges::encode_block(&self.sorted, n);
        let block_len = block.len() as u64;
        let (block_at, block) = match &mut self.blocks {
            Some(w) => (Some(w.append(&block)?), Vec::new()),
            None => (None, block),
        };
        let n_requests = self.requests.len();
        self.n_requests += n_requests as u64;
        self.n_lids += self.lids.len() as u64;
        self.outs.push(UnitOut {
            worker: self.worker,
            sources: std::mem::take(&mut self.sources),
            lids: std::mem::replace(&mut self.lids, Vec::with_capacity(n)),
            owners: self.lid_owners.drain(..).map(|o| std::sync::atomic::AtomicU64::new(if o == PENDING { NO_OWNER } else { o })).collect(),
            requests: std::mem::replace(&mut self.requests, Vec::with_capacity(n_requests)),
            pending: std::mem::take(&mut self.pending),
            bufs: std::mem::take(&mut self.bufs),
            block_at,
            block_len,
            block,
            xfers: used,
            starts,
            edges: self.sorted.len() as u64,
        });
        let t1 = crate::frame::phases::add(crate::frame::phases::END_UNIT, t);
        self.end_ticks += t1.saturating_sub(t);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packed_edges_sort_by_target_and_round_trip() {
        let a = pack_edge(3, 63, 8191, (1 << XFER_BITS) - 1);
        assert_eq!(unpack_edge(a), (3, 63, 8191, (1 << XFER_BITS) - 1));
        let b = pack_edge(4, 0, 0, 0);
        assert!(a < b, "a later lid sorts after");
        assert!(pack_edge(3, 62, 8191, 9) < pack_edge(3, 63, 0, 0), "then the cell");
        assert_eq!(unpack_edge(pack_edge((1 << LID_BITS) - 1, 255, 5, 7)), ((1 << LID_BITS) - 1, 255, 5, 7));
    }
}
