//! `vprod`: a FAITHFUL replica of production's per-frame dedup path (the
//! emission loop after the kernels' dup mask, the `ForwardSink` queues and
//! flushes, the unit `RowCache`, the direct edge cache, `DropNotes`, the door
//! and its `end_frame`), driven by an emission capture (`CELESTE_EMIT_CAPTURE`,
//! see bench/dedup/DESIGNS.md). Production: /var/tmp/fgwt (fg-2300, d2eda1c)
//! src/compiled/asm_kernel.rs `run_slice`, src/frame.rs `ForwardSink`,
//! `DropNotes`, `forward_frame`, crates/celeste-engine/src/kernel.rs
//! `RowCache`, src/search/door.rs (copied verbatim: door.rs here).
//!
//! What the capture cannot provide, and what stands in for it:
//! - the row's columns: a fixed 64-B payload per row (the record's 48 B + the
//!   key), pushed into the queue and gathered into the piece;
//! - the key: given (production hashes ~15 fields per emission);
//! - the transfer: the captured worker-local id (production's
//!   `xfer_id_raw`: a raw-word hash + direct-mapped cache, not replayed);
//!   the per-worker transfer table file is not written;
//! - level -1: the table is an FxHashMap<(shape, x, y), d> like production's,
//!   rebuilt from the capture's drop flags (a dropped cell gets d = 1000, a
//!   kept one 0; frame 57, h 100), probed through `minus_one_drop`'s run
//!   cache exactly as production does; so the `from` a drop note carries is a
//!   stand-in (1057);
//! - slices: the capture has no slice boundaries, so the pos-graph edge's
//!   `last_edge` run cache resets per unit, not per 16-lane slice (only the
//!   pos-graph `edges` set's insert count can differ, never its content);
//!   the EMIT phase is timed per unit, not per slice;
//! - `any_win` and `key_check` are not run (no columns; the check is off in
//!   production unless asked for);
//! - `Renumber` for `end_frame`: the identity over the wave's pieces (same
//!   lookups, same cost; canonical ids would be a permutation).
//!
//! Usage: vprod CAPTURE_DIR [vprod | vprod1 | vread | vread1] [EDGES_DIR]

mod door;

use door::{Admit, Door, Entry, Key};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::time::Instant;

const REC: usize = 48;
const PAYLOAD: usize = 64;
/// The frame the wave builds (its new ids' layer); the input is layer 56.
const FRAME: u32 = 57;
/// Level -1's horizon and the d a dropped cell gets (from = FRAME + d > H).
const H: u32 = 100;
const DROPPED_D: u32 = 1000;
const WORKERS: usize = 16;

// ---------------------------------------------------------------- phases
/// Production's `frame::phases`, verbatim but for the switch
/// (`VPROD_PHASES=0` turns it off; on by default).
pub mod phases {
    use std::sync::atomic::{AtomicU64, Ordering};
    pub const NAMES: [&str; 14] = ["pack", "kernel", "emit (net of flush)", "flush", "flush.sort", "flush.admit", "flush.edges", "flush.gather", "end_call", "flush.pre", "flush.post", "slice.setup", "finish", "unit (all of engine.run)"];
    pub const EMIT: usize = 2;
    pub const FLUSH: usize = 3;
    pub const SORT: usize = 4;
    pub const ADMIT: usize = 5;
    pub const EDGES: usize = 6;
    pub const GATHER: usize = 7;
    pub const END_CALL: usize = 8;
    pub const PRE: usize = 9;
    pub const POST: usize = 10;
    pub const FINISH: usize = 12;
    pub const UNIT: usize = 13;
    static TICKS: [AtomicU64; 14] = [const { AtomicU64::new(0) }; 14];
    fn on() -> bool {
        static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *ON.get_or_init(|| std::env::var("VPROD_PHASES").map_or(true, |v| v != "0"))
    }
    #[inline]
    pub fn start() -> u64 {
        if on() {
            unsafe { core::arch::x86_64::_rdtsc() }
        } else {
            0
        }
    }
    #[inline]
    pub fn add(p: usize, t0: u64) -> u64 {
        if t0 == 0 {
            return 0;
        }
        let t = unsafe { core::arch::x86_64::_rdtsc() };
        TICKS[p].fetch_add(t - t0, Ordering::Relaxed);
        t
    }
    pub fn sub(p: usize, ticks: u64) {
        TICKS[p].fetch_sub(ticks, Ordering::Relaxed);
    }
    pub fn print_phases(wall: std::time::Duration, workers: usize) {
        if !on() {
            return;
        }
        let v: Vec<u64> = TICKS.iter().map(|a| a.swap(0, Ordering::Relaxed)).collect();
        let t0 = std::time::Instant::now();
        let c0 = unsafe { core::arch::x86_64::_rdtsc() };
        std::thread::sleep(std::time::Duration::from_millis(50));
        let hz = (unsafe { core::arch::x86_64::_rdtsc() } - c0) as f64 / t0.elapsed().as_secs_f64();
        let budget = wall.as_secs_f64() * workers as f64;
        let line: Vec<String> = NAMES.iter().zip(&v).filter(|(_, &t)| t > 0).map(|(n, &t)| format!("{n} {:.2}s ({:.0}%)", t as f64 / hz, 100.0 * t as f64 / hz / budget)).collect();
        eprintln!("[phases] worker-seconds of {:.2} ({} x {:.3} s): {}", budget, workers, wall.as_secs_f64(), line.join(", "));
    }
}

// ---------------------------------------------------------------- ids, hashes
pub fn pack_id(layer: u32, seq: u32, row: u32) -> u64 {
    ((layer as u64) << 48) | ((seq as u64) << 32) | row as u64
}
pub fn id_layer(id: u64) -> u32 {
    (id >> 48) as u32
}
pub fn id_seq(id: u64) -> u32 {
    ((id >> 32) & 0xffff) as u32
}
pub fn id_row(id: u64) -> u32 {
    id as u32
}
const MIX_C1: u64 = 0xbf58_476d_1ce4_e5b9;
const MIX_C2: u64 = 0x94d0_49bb_1331_11eb;
#[inline]
fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(MIX_C1);
    x = (x ^ (x >> 27)).wrapping_mul(MIX_C2);
    x ^ (x >> 31)
}
const GRID: i32 = 512;
const ORIGIN: i32 = -64;
const NO_CELL: u32 = (GRID * GRID) as u32;
fn cell_xy(cell: u32) -> Option<(i32, i32)> {
    (cell != NO_CELL).then(|| (cell as i32 % GRID + ORIGIN, cell as i32 / GRID + ORIGIN))
}

// ---------------------------------------------------------------- canon::Renumber (copy)
pub struct Renumber {
    layer: u32,
    first_seq: u32,
    starts: Vec<u64>,
    ids: Vec<u64>,
}

impl Renumber {
    #[inline]
    pub fn map(&self, id: u64) -> u64 {
        if id_layer(id) != self.layer || id_seq(id) < self.first_seq {
            return id;
        }
        let k = (id_seq(id) - self.first_seq) as usize;
        assert!(k + 1 < self.starts.len(), "id {id:#x}: a piece this wave did not flush");
        let at = self.starts[k] + id_row(id) as u64;
        assert!(at < self.starts[k + 1], "id {id:#x}: a row this wave did not flush");
        self.ids[at as usize]
    }
}

// ---------------------------------------------------------------- RowCache (verbatim)
pub struct RowCache {
    slots: Vec<(u64, u64, u32, u32)>,
    mask: usize,
    gen: u32,
}

impl RowCache {
    pub const CAPACITY: usize = 1 << 15;
    const PROBES: usize = 4;
    pub fn new() -> Self {
        RowCache { slots: vec![(0, 0, 0, 0); Self::CAPACITY], mask: Self::CAPACITY - 1, gen: 1 }
    }
    pub const ID_FLAG: u64 = 1 << 63;
    pub const DROP_FLAG: u64 = 1 << 62;
    pub fn clear(&mut self) {
        self.gen = self.gen.wrapping_add(1);
        if self.gen == 0 {
            self.slots.iter_mut().for_each(|s| s.1 = 0);
            self.gen = 1;
        }
    }
    #[inline(always)]
    pub fn set_ref(&mut self, k: (u64, u64), r: u64) {
        let base = k.0 as usize;
        for p in 0..Self::PROBES {
            let i = (base + p) & self.mask;
            let s = &mut self.slots[i];
            if s.2 == self.gen && s.0 == k.1 {
                s.1 = r;
                return;
            }
        }
    }
    #[inline(always)]
    pub fn insert_ref(&mut self, k: (u64, u64), tag: u32, r: u64) -> Option<(u32, u64)> {
        let base = k.0 as usize;
        let mut victim = base & self.mask;
        for p in 0..Self::PROBES {
            let i = (base + p) & self.mask;
            let s = self.slots[i];
            if s.2 != self.gen {
                victim = i;
                break;
            }
            if s.0 == k.1 {
                return Some((s.3, s.1));
            }
        }
        self.slots[victim] = (k.1, r, self.gen, tag);
        None
    }
}

// ---------------------------------------------------------------- DropNotes (verbatim; built from src.bin)
pub struct DropNotes {
    runs: Vec<(u64, u32, u32)>,
    mins: Vec<AtomicU32>,
}

impl DropNotes {
    /// `ids`: the frontier's ids in block order (src.bin).
    fn new(ids: &[u64]) -> Self {
        let mut runs = Vec::new();
        let mut off = 0u32;
        let mut i = 0;
        while i < ids.len() {
            let mut j = i + 1;
            while j < ids.len() && ids[j] == ids[j - 1] + 1 {
                j += 1;
            }
            runs.push((ids[i], (j - i) as u32, off));
            off += (j - i) as u32;
            i = j;
        }
        runs.sort_unstable();
        let mins = (0..off).map(|_| AtomicU32::new(u32::MAX)).collect();
        DropNotes { runs, mins }
    }
    pub fn note(&self, base: u64, mask: u64, from: u32) {
        let k = self.runs.partition_point(|r| r.0 <= base);
        let (start, len, off) = self.runs[k.checked_sub(1).expect("a dropped row's source is an input row")];
        let mut m = mask;
        while m != 0 {
            let id = base + m.trailing_zeros() as u64;
            m &= m - 1;
            let at = id - start;
            assert!(at < len as u64, "source {id:#x} outside its input run");
            self.mins[(off as u64 + at) as usize].fetch_min(from, Ordering::Relaxed);
        }
    }
    fn noted(&self) -> u64 {
        self.mins.iter().filter(|m| m.load(Ordering::Relaxed) != u32::MAX).count() as u64
    }
}

// ---------------------------------------------------------------- level -1 table
/// `CostToGo` reduced to what `admitted_from` reads: production's FxHashMap.
struct CostToGo {
    d: FxHashMap<(u64, i16, i16), u32>,
}

impl CostToGo {
    fn admitted_from(&self, shape: u64, cell: u32, frame: u32) -> u32 {
        let Some((x, _y)) = cell_xy(cell) else { return 0 };
        if x >= 128 {
            return 0;
        }
        match self.d_of(shape, cell) {
            None => 0,
            Some(d) => frame.saturating_add(d),
        }
    }
    fn d_of(&self, shape: u64, cell: u32) -> Option<u32> {
        let (x, y) = cell_xy(cell)?;
        if x >= 128 {
            return None;
        }
        self.d.get(&(shape, x as i16, y as i16)).copied()
    }
}

// ---------------------------------------------------------------- edges (copies of search::edges)
#[inline]
fn push_records(out: &mut Vec<u128>, target: u64, base: u64, xfer: u32, mut mask: u64) {
    let (tseq, trow) = (id_seq(target) as u16 as u128, id_row(target) as u128);
    let head = tseq | trow << 16 | (xfer as u128) << 96;
    while mask != 0 {
        let src = base + mask.trailing_zeros() as u64;
        mask &= mask - 1;
        out.push(head | (id_seq(src) as u16 as u128) << 48 | (id_row(src) as u128) << 64);
    }
}
#[inline]
fn word_fields(w: u128) -> (u64, u64, u32) {
    let t = (w as u64 & 0xffff) << 32 | (w >> 16) as u64 & 0xffff_ffff;
    let s = ((w >> 48) as u64 & 0xffff) << 32 | (w >> 64) as u64 & 0xffff_ffff;
    (t, s, (w >> 96) as u32)
}
const CHUNK_WORDS: usize = 1 << 16;
const PREDICT_BITS: u32 = 10;
#[inline]
fn predict_slot(s: u64) -> usize {
    (s.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> (64 - PREDICT_BITS)) as usize
}
#[inline]
fn zigzag(d: i64) -> u64 {
    ((d << 1) ^ (d >> 63)) as u64
}
fn put_varint(out: &mut Vec<u8>, mut v: u64) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
}
fn write_chunk(out: &mut Vec<u8>, words: &[u128]) {
    if words.is_empty() {
        return;
    }
    const ROOM: usize = 20;
    let start = out.len();
    out.resize(start + ROOM, 0);
    let mut predict = [u32::MAX; 1 << PREDICT_BITS];
    let (mut pt, mut ps) = (0u64, 0u64);
    for &w in words {
        let (t, s, x) = word_fields(w);
        let slot = predict_slot(s);
        let hit = predict[slot] == x;
        put_varint(out, zigzag(t.wrapping_sub(pt) as i64));
        put_varint(out, zigzag(s.wrapping_sub(ps) as i64) << 1 | hit as u64);
        if !hit {
            put_varint(out, x as u64);
            predict[slot] = x;
        }
        (pt, ps) = (t, s);
    }
    let len = out.len() - start - ROOM;
    let mut head = Vec::with_capacity(ROOM);
    put_varint(&mut head, words.len() as u64);
    put_varint(&mut head, len as u64);
    out[start..start + head.len()].copy_from_slice(&head);
    out.copy_within(start + ROOM.., start + head.len());
    out.truncate(start + head.len() + len);
}
fn raw_path(dir: &std::path::Path, frame: u32, layer: u32, worker: u32) -> std::path::PathBuf {
    dir.join(format!("raw_f{frame:03}")).join(format!("l{:03}_w{:03}.bin", layer, worker))
}

/// Validation only (`VPROD_VERIFY=1`): every word written, fingerprinted
/// with its layer, to count the distinct (source, target, transfer).
static VERIFY: std::sync::Mutex<Vec<u64>> = std::sync::Mutex::new(Vec::new());
fn verify_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("VPROD_VERIFY").is_ok_and(|v| v == "1"))
}

thread_local! {
    static BYTES: std::cell::RefCell<Vec<u8>> = const { std::cell::RefCell::new(Vec::new()) };
}

fn write_edges(dir: &std::path::Path, frame: u32, layer: usize, worker: u32, buf: &mut Vec<u128>) {
    use std::io::Write;
    if verify_on() {
        let mut v = VERIFY.lock().unwrap();
        v.extend(buf.iter().map(|&w| mix64(w as u64 ^ mix64((w >> 64) as u64 ^ (layer as u64) << 56))));
    }
    let path = raw_path(dir, frame, layer as u32, worker);
    std::fs::create_dir_all(path.parent().expect("a raw dir")).unwrap();
    BYTES.with_borrow_mut(|bytes| {
        bytes.clear();
        write_chunk(bytes, buf);
        let mut f = std::fs::OpenOptions::new().create(true).append(true).open(&path).unwrap();
        f.write_all(bytes).unwrap();
    });
    buf.clear();
}

#[inline]
#[allow(clippy::too_many_arguments)]
fn append_record(edge_bufs: &mut Vec<Vec<u128>>, edge_records: &mut u64, edge_words: &mut u64, dir: &std::path::Path, frame: u32, worker: u32, target: u64, base: u64, xfer: u32, mask: u64) {
    let layer = id_layer(target) as usize;
    if edge_bufs.len() <= layer {
        edge_bufs.resize_with(layer + 1, Vec::new);
    }
    let buf = &mut edge_bufs[layer];
    push_records(buf, target, base, xfer, mask);
    *edge_records += 1;
    *edge_words += mask.count_ones() as u64;
    if buf.len() >= CHUNK_WORDS {
        write_edges(dir, frame, layer, worker, buf);
    }
}

// ---------------------------------------------------------------- the queue slot
pub const QUEUE_ROWS: usize = 256;
pub const POOL_QUEUES: usize = 256;
const DIRECT_SLOTS: usize = 1 << 12;

/// Production's `Slot` with its typed columns replaced by `PAYLOAD` bytes a row.
pub struct Slot {
    pub shape: u64,
    pub outcome: u64,
    pub cell: u32,
    pub live: bool,
    pub touched: bool,
    pub rows_data: Vec<u8>,
    pub keys: Vec<(u64, u64)>,
    pub cells: Vec<u32>,
    pub pred_base: Vec<u64>,
    pub pred_mask: Vec<u64>,
    pub pred_xfer: Vec<u32>,
    pub extra: Vec<(u32, u64, u32, u64)>,
    pub last_extra: Vec<u32>,
    pub gen: u16,
}

impl Slot {
    fn new(shape: u64) -> Self {
        Slot {
            shape,
            outcome: 0,
            cell: 0,
            live: false,
            touched: false,
            rows_data: Vec::new(),
            keys: Vec::new(),
            cells: Vec::new(),
            pred_base: Vec::new(),
            pred_mask: Vec::new(),
            pred_xfer: Vec::new(),
            extra: Vec::new(),
            last_extra: Vec::new(),
            gen: 0,
        }
    }
    pub fn rows(&self) -> usize {
        self.keys.len()
    }
    pub fn clear(&mut self) {
        self.rows_data.clear();
        self.keys.clear();
        self.cells.clear();
        self.pred_base.clear();
        self.pred_mask.clear();
        self.pred_xfer.clear();
        self.extra.clear();
        self.last_extra.clear();
        self.gen = self.gen.wrapping_add(1);
    }
    /// `BodyCols::push_row` + the key and cell.
    #[inline]
    fn push_row(&mut self, payload: &[u8; PAYLOAD], key: (u64, u64), cell: u32) {
        self.rows_data.extend_from_slice(payload);
        self.keys.push(key);
        self.cells.push(cell);
    }
    fn gather_into(&self, piece: &mut Piece, rows: &[u32]) {
        for &r in rows {
            piece.data.extend_from_slice(&self.rows_data[r as usize * PAYLOAD..(r as usize + 1) * PAYLOAD]);
        }
        piece.width += rows.len();
    }
}

pub struct Piece {
    data: Vec<u8>,
    width: usize,
}

const NO_QUEUE: ((u64, u32), u32) = ((u64::MAX, u32::MAX), u32::MAX);

pub struct ForwardSink<'a> {
    pub slots: Vec<Slot>,
    index: FxHashMap<(u64, u32), u32>,
    spare: FxHashMap<u64, Vec<u32>>,
    clock: usize,
    last: ((u64, u32), u32),
    drop_last: Option<((u64, u32), Option<u32>)>,
    door: &'a Door,
    minus_one: &'a CostToGo,
    note_drops: bool,
    drops: &'a DropNotes,
    seqs: &'a AtomicU32,
    edges_dir: std::path::PathBuf,
    edge_bufs: Vec<Vec<u128>>,
    pub edge_records: u64,
    pub edge_words: u64,
    pub seen: RowCache,
    direct: Vec<(u64, u64, u32, u64)>,
    pub t_edges: std::time::Duration,
    pieces: FxHashMap<u64, (Piece, u32, Vec<u32>)>,
    frame: u32,
    worker: u32,
    pub kept: usize,
    pub flushes: u64,
    pub flushed_rows: u64,
    sort_buf: Vec<((u64, u64), u32)>,
    keys_buf: Vec<(u64, u64)>,
    uniq_buf: Vec<u32>,
    row_uniq: Vec<u32>,
    new_buf: Vec<u32>,
    ids_buf: Vec<u64>,
    rows_buf: Vec<u32>,
    pub edges_on: bool,
    pub edges: FxHashSet<(u32, u32)>,
    pub emitted: u64,
    pub flush_ticks: u64,
}

impl<'a> ForwardSink<'a> {
    fn forward(door: &'a Door, minus_one: &'a CostToGo, drops: &'a DropNotes, seqs: &'a AtomicU32, edges_dir: &std::path::Path, worker: u32) -> Self {
        ForwardSink {
            slots: Vec::new(),
            index: Default::default(),
            spare: Default::default(),
            clock: 0,
            last: NO_QUEUE,
            drop_last: None,
            door,
            minus_one,
            note_drops: true,
            drops,
            seqs,
            edges_dir: edges_dir.to_path_buf(),
            edge_bufs: Vec::new(),
            edge_records: 0,
            edge_words: 0,
            seen: RowCache::new(),
            direct: Vec::new(),
            t_edges: std::time::Duration::ZERO,
            pieces: Default::default(),
            frame: FRAME,
            worker,
            kept: 0,
            flushes: 0,
            flushed_rows: 0,
            sort_buf: Vec::with_capacity(QUEUE_ROWS),
            keys_buf: Vec::with_capacity(QUEUE_ROWS),
            uniq_buf: Vec::with_capacity(QUEUE_ROWS),
            row_uniq: Vec::with_capacity(QUEUE_ROWS),
            new_buf: Vec::with_capacity(QUEUE_ROWS),
            ids_buf: Vec::with_capacity(QUEUE_ROWS),
            rows_buf: Vec::with_capacity(QUEUE_ROWS),
            edges_on: true,
            edges: Default::default(),
            emitted: 0,
            flush_ticks: 0,
        }
    }

    #[inline]
    pub fn minus_one_drop(&mut self, shape: u64, cell: u32) -> Option<u32> {
        if let Some((at, r)) = self.drop_last {
            if at == (shape, cell) {
                return r;
            }
        }
        let from = self.minus_one.admitted_from(shape, cell, self.frame);
        let r = (from > H).then_some(from);
        self.drop_last = Some(((shape, cell), r));
        r
    }

    #[inline]
    pub fn dropped_again(&mut self, base: u64, lane: usize, r: u64) {
        if let (true, true) = (self.note_drops, r != 0) {
            self.drops.note(base, 1u64 << lane, r as u32);
        }
    }

    #[inline]
    fn record(&mut self, target: u64, base: u64, xfer: u32, mask: u64) {
        append_record(&mut self.edge_bufs, &mut self.edge_records, &mut self.edge_words, &self.edges_dir, self.frame, self.worker, target, base, xfer, mask)
    }

    #[inline]
    pub fn direct_edge(&mut self, target: u64, base: u64, xfer: u32, lane: usize) {
        if self.direct.is_empty() {
            self.direct = vec![(u64::MAX, 0, 0, 0); DIRECT_SLOTS];
        }
        let i = (mix64(target ^ base.rotate_left(17) ^ (xfer as u64).rotate_left(41)) as usize) & (DIRECT_SLOTS - 1);
        let e = self.direct[i];
        if e.0 == target && e.1 == base && e.2 == xfer {
            self.direct[i].3 |= 1u64 << lane;
            return;
        }
        if e.0 != u64::MAX {
            self.record(e.0, e.1, e.2, e.3);
        }
        self.direct[i] = (target, base, xfer, 1u64 << lane);
    }

    pub fn end_call(&mut self) {
        let t = phases::start();
        for i in 0..self.direct.len() {
            let e = self.direct[i];
            if e.0 != u64::MAX {
                self.record(e.0, e.1, e.2, e.3);
                self.direct[i] = (u64::MAX, 0, 0, 0);
            }
        }
        phases::add(phases::END_CALL, t);
    }

    #[inline]
    pub fn row_ref(&self, q: usize) -> u64 {
        assert!(q < 1 << 24, "queue pool past 2^24 slots");
        let s = &self.slots[q];
        ((s.gen as u64) << 32) | ((q as u64) << 8) | (s.rows() as u64 - 1)
    }

    #[inline]
    pub fn mark_pred(&mut self, row_ref: u64, base: u64, xfer: u32, lane: usize) -> bool {
        let (gen, q, r) = ((row_ref >> 32) as u16, ((row_ref >> 8) & 0xff_ffff) as usize, (row_ref & 0xff) as usize);
        let s = &mut self.slots[q];
        if !s.live || s.gen != gen || r >= s.pred_mask.len() {
            return false;
        }
        if s.pred_base[r] == base && s.pred_xfer[r] == xfer {
            s.pred_mask[r] |= 1u64 << lane;
            return true;
        }
        let last = s.last_extra[r];
        if last != u32::MAX && s.extra[last as usize].1 == base && s.extra[last as usize].2 == xfer {
            s.extra[last as usize].3 |= 1u64 << lane;
        } else {
            s.last_extra[r] = s.extra.len() as u32;
            s.extra.push((r as u32, base, xfer, 1u64 << lane));
        }
        true
    }

    pub fn queue(&mut self, outcome: u64, cell: u32) -> usize {
        if self.last.0 == (outcome, cell) {
            return self.last.1 as usize;
        }
        let q = match self.index.get(&(outcome, cell)) {
            Some(&q) => q,
            None => {
                if self.index.len() >= POOL_QUEUES {
                    self.evict_one();
                }
                let q = match self.spare.get_mut(&outcome).and_then(Vec::pop) {
                    Some(q) => q,
                    None => {
                        self.slots.push(Slot::new(outcome));
                        (self.slots.len() - 1) as u32
                    }
                };
                let s = &mut self.slots[q as usize];
                s.outcome = outcome;
                s.cell = cell;
                s.live = true;
                s.touched = false;
                self.index.insert((outcome, cell), q);
                q
            }
        };
        self.last = ((outcome, cell), q);
        q as usize
    }

    fn evict_one(&mut self) {
        loop {
            let i = self.clock % self.slots.len();
            self.clock += 1;
            let s = &mut self.slots[i];
            if !s.live {
                continue;
            }
            if s.touched {
                s.touched = false;
                continue;
            }
            self.flush(i);
            return;
        }
    }

    #[inline]
    pub fn pushed(&mut self, q: usize) {
        let s = &mut self.slots[q];
        s.touched = true;
        if s.rows() >= QUEUE_ROWS {
            self.flush(q);
        }
    }

    fn flush(&mut self, q: usize) {
        let t_flush = phases::start();
        self.flush_inner(q);
        let t_end = phases::add(phases::FLUSH, t_flush);
        self.flush_ticks += t_end.saturating_sub(t_flush);
    }

    fn flush_inner(&mut self, q: usize) {
        let door = self.door;
        let t_pre = phases::start();
        let slot = &mut self.slots[q];
        let n = slot.rows();
        if n > 0 {
            self.flushes += 1;
            self.flushed_rows += n as u64;
            // (No marks filter.) The level -1 filter at the flush.
            let dropped = {
                let from = self.minus_one.admitted_from(slot.shape, slot.cell, self.frame);
                (from > H).then_some(from)
            };
            let allow: Option<Vec<bool>> = if dropped.is_some() { Some(vec![false; n]) } else { None };
            if let (Some(from), true) = (dropped, self.note_drops && slot.pred_base.len() == n) {
                let rows = slot.pred_base.iter().zip(&slot.pred_mask).map(|(&b, &m)| (b, m));
                for (b, m) in rows.chain(slot.extra.iter().map(|&(_, b, _, m)| (b, m))) {
                    self.drops.note(b, m, from);
                }
            }
            self.sort_buf.clear();
            self.sort_buf.extend(slot.keys.iter().enumerate().filter(|(r, _)| allow.as_ref().is_none_or(|a| a[*r])).map(|(r, k)| (*k, r as u32)));
            let t_ph = phases::add(phases::PRE, t_pre);
            self.sort_buf.sort_unstable();
            self.keys_buf.clear();
            self.uniq_buf.clear();
            for e in &self.sort_buf {
                if self.keys_buf.last() != Some(&e.0) {
                    self.keys_buf.push(e.0);
                }
                self.uniq_buf.push(self.keys_buf.len() as u32 - 1);
            }
            self.row_uniq.clear();
            self.row_uniq.resize(n, u32::MAX);
            for (e, &u) in self.sort_buf.iter().zip(&self.uniq_buf) {
                self.row_uniq[e.1 as usize] = u;
            }
            self.new_buf.clear();
            self.ids_buf.clear();
            let frame = self.frame;
            let seqs = self.seqs;
            let (piece, seq, piece_cells) = self.pieces.entry(slot.shape).or_insert_with(|| {
                let seq = seqs.fetch_add(1, Ordering::Relaxed);
                assert!(seq <= u16::MAX as u32, "frame {frame}: piece seq {seq} past the 16 bits an id holds");
                (Piece { data: Vec::new(), width: 0 }, seq, Vec::new())
            });
            let first_new = pack_id(frame, *seq, piece.width as u32);
            let t_ph = phases::add(phases::SORT, t_ph);
            door.admit(slot.shape, slot.cell, &self.keys_buf, first_new, &mut self.ids_buf, &mut self.new_buf);
            let t_ph = phases::add(phases::ADMIT, t_ph);
            if slot.pred_base.len() == n {
                let t_e = std::time::Instant::now();
                let (row_uniq, ids_buf) = (&self.row_uniq, &self.ids_buf);
                let (edge_bufs, edge_records, edge_words, frame, worker) = (&mut self.edge_bufs, &mut self.edge_records, &mut self.edge_words, self.frame, self.worker);
                let rows = slot.pred_base.iter().zip(&slot.pred_xfer).zip(&slot.pred_mask).enumerate().map(|(r, ((&b, &x), &m))| (r as u32, b, x, m));
                for (r, b, x, m) in rows.chain(slot.extra.iter().copied()) {
                    let u = row_uniq[r as usize];
                    if u != u32::MAX {
                        append_record(edge_bufs, edge_records, edge_words, &self.edges_dir, frame, worker, ids_buf[u as usize], b, x, m);
                    }
                }
                for (r, key) in slot.keys.iter().enumerate() {
                    let u = row_uniq[r];
                    let v = if u == u32::MAX { RowCache::DROP_FLAG | dropped.unwrap_or(u32::MAX) as u64 } else { RowCache::ID_FLAG | ids_buf[u as usize] };
                    self.seen.set_ref(*key, v);
                }
                self.t_edges += t_e.elapsed();
            }
            let t_ph = phases::add(phases::EDGES, t_ph);
            if !self.new_buf.is_empty() {
                self.rows_buf.clear();
                self.rows_buf.extend(self.new_buf.iter().map(|&u| {
                    let first = self.uniq_buf.partition_point(|&x| x < u);
                    self.sort_buf[first].1
                }));
                self.kept += self.rows_buf.len();
                slot.gather_into(piece, &self.rows_buf);
                piece_cells.extend(std::iter::repeat_n(slot.cell, self.rows_buf.len()));
            }
            let t_ph = phases::add(phases::GATHER, t_ph);
            slot.clear();
            phases::add(phases::POST, t_ph);
        }
        slot.live = false;
        slot.touched = false;
        self.index.remove(&(slot.outcome, slot.cell));
        self.spare.entry(slot.outcome).or_default().push(q as u32);
        if self.last.1 == q as u32 {
            self.last = NO_QUEUE;
        }
    }

    pub fn finish(&mut self) -> Vec<(u32, usize)> {
        for q in 0..self.slots.len() {
            if self.slots[q].live {
                self.flush(q);
            }
        }
        let dir = self.edges_dir.clone();
        for (layer, buf) in self.edge_bufs.iter_mut().enumerate() {
            if !buf.is_empty() {
                write_edges(&dir, self.frame, layer, self.worker, buf);
            }
        }
        std::mem::take(&mut self.pieces).into_values().filter(|(p, _, _)| p.width > 0).map(|(p, seq, _)| (seq, p.width)).collect()
    }
}

// ---------------------------------------------------------------- the replay
#[derive(Clone, Copy)]
struct Rec {
    src: u64,
    key: (u64, u64),
    shape: u64,
    cell: u32,
    xfer: u32,
    flags: u32,
}

#[inline(always)]
fn rec(b: &[u8]) -> Rec {
    let u64_ = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
    let u32_ = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    Rec { src: u64_(0), key: (u64_(8), u64_(16)), shape: u64_(24), cell: u32_(32), xfer: u32_(36), flags: u32_(40) }
}

/// The input's cells by id (production: `cell_in[lanes[i]]`).
struct CellsIn {
    /// Per input seq, its first index into `cells`.
    starts: Vec<u32>,
    cells: Vec<u32>,
}

impl CellsIn {
    #[inline(always)]
    fn of(&self, id: u64) -> u32 {
        self.cells[(self.starts[id_seq(id) as usize] + id_row(id)) as usize]
    }
}

/// Per worker: replay `units` (byte ranges of its capture file, each a unit
/// marker followed by its emissions) into one sink, as `forward_frame`'s
/// worker loop + `run_chunk` + `run_slice` do.
struct Shared<'a> {
    door: &'a Door,
    table: &'a CostToGo,
    notes: &'a DropNotes,
    seqs: &'a AtomicU32,
    cells_in: &'a CellsIn,
    edges_dir: &'a std::path::Path,
    drop_mismatch: &'a AtomicU64,
}

struct Done {
    pieces: Vec<(u32, usize)>,
    kept: usize,
    flushes: u64,
    flushed_rows: u64,
    emitted: u64,
    edges: usize,
    edge_records: u64,
    edge_words: u64,
    t_edges: std::time::Duration,
    busy: std::time::Duration,
}

fn worker(sh: &Shared, worker: u32, units: &[&[u8]]) -> Done {
    let t = Instant::now();
    let mut sink = ForwardSink::forward(sh.door, sh.table, sh.notes, sh.seqs, sh.edges_dir, worker);
    let mut mismatch = 0u64;
    for unit in units {
        // `forward_frame`: the unit's dedup cache starts empty.
        sink.seen.clear();
        let t_unit = phases::start();
        let t_emit = t_unit;
        let flush_before = sink.flush_ticks;
        let mut last_edge = (u32::MAX, u32::MAX);
        let mut payload = [0u8; PAYLOAD];
        for c in unit.chunks_exact(REC).skip(1) {
            let r = rec(c);
            debug_assert_ne!(r.flags, 2);
            // A lane's predecessor group: ids are `pack_id(56, seq, row)` with
            // row = lane, so its group base is the id with the low 6 bits clear.
            let (b, glane) = (r.src & !63, (r.src & 63) as usize);
            let cin = sh.cells_in.of(r.src);
            let cout = r.cell;
            if let Some(from) = sink.minus_one_drop(r.shape, cout) {
                if r.flags != 1 {
                    mismatch += 1;
                }
                if sink.edges_on && last_edge != (cin, cout) {
                    last_edge = (cin, cout);
                    sink.edges.insert((cin, cout));
                }
                sink.dropped_again(b, glane, from as u64);
                continue;
            }
            if r.flags == 1 {
                mismatch += 1;
                continue;
            }
            let key = r.key;
            let xfer = r.xfer;
            if let Some((first_cin, rf)) = sink.seen.insert_ref(key, cin, 0) {
                if sink.edges_on && first_cin != cin {
                    sink.edges.insert((cin, cout));
                }
                if rf & RowCache::ID_FLAG != 0 {
                    sink.direct_edge(rf & !RowCache::ID_FLAG, b, xfer, glane);
                    continue;
                }
                if rf & RowCache::DROP_FLAG != 0 {
                    sink.dropped_again(b, glane, rf & !RowCache::DROP_FLAG);
                    continue;
                }
                if sink.mark_pred(rf, b, xfer, glane) {
                    continue;
                }
                // A stale ref: push again; the flush merges by key.
            }
            sink.emitted += 1;
            if sink.edges_on && last_edge != (cin, cout) {
                last_edge = (cin, cout);
                sink.edges.insert((cin, cout));
            }
            let q = sink.queue(r.shape, cout);
            assert_eq!(sink.slots[q].shape, r.shape, "a queue's shape is its template union's");
            payload[..REC].copy_from_slice(c);
            payload[REC..REC + 8].copy_from_slice(&key.0.to_le_bytes());
            payload[REC + 8..].copy_from_slice(&key.1.to_le_bytes());
            sink.slots[q].push_row(&payload, key, cout);
            sink.slots[q].pred_base.push(b);
            sink.slots[q].pred_mask.push(1u64 << glane);
            sink.slots[q].pred_xfer.push(xfer);
            sink.slots[q].last_extra.push(u32::MAX);
            let rr = sink.row_ref(q);
            sink.seen.set_ref(key, rr);
            sink.pushed(q);
        }
        if t_emit != 0 {
            phases::add(phases::EMIT, t_emit);
            phases::sub(phases::EMIT, sink.flush_ticks - flush_before);
        }
        sink.end_call();
        phases::add(phases::UNIT, t_unit);
    }
    let t_fin = phases::start();
    let pieces = sink.finish();
    phases::add(phases::FINISH, t_fin);
    sh.drop_mismatch.fetch_add(mismatch, Ordering::Relaxed);
    Done {
        pieces,
        kept: sink.kept,
        flushes: sink.flushes,
        flushed_rows: sink.flushed_rows,
        emitted: sink.emitted,
        edges: sink.edges.len(),
        edge_records: sink.edge_records,
        edge_words: sink.edge_words,
        t_edges: sink.t_edges,
        busy: t.elapsed(),
    }
}

fn read_u64(b: &[u8], o: usize) -> u64 {
    u64::from_le_bytes(b[o..o + 8].try_into().unwrap())
}
fn read_u32(b: &[u8], o: usize) -> u32 {
    u32::from_le_bytes(b[o..o + 4].try_into().unwrap())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = &args[1];
    let variant = args.get(2).map(|s| s.as_str()).unwrap_or("vprod");
    let edges_dir = std::path::PathBuf::from(args.get(3).map(|s| s.as_str()).unwrap_or("/var/tmp/vprod-edges"));
    let t0 = Instant::now();
    let mut files: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().map(|e| e.path()).filter(|p| p.file_name().unwrap().to_str().unwrap().starts_with('w')).collect();
    files.sort();
    let maps: Vec<memmap2::Mmap> = files.iter().map(|p| unsafe { memmap2::Mmap::map(&std::fs::File::open(p).unwrap()).unwrap() }).collect();

    if variant == "vread" || variant == "vread1" {
        // The harness's own floor: read and parse every record, nothing else.
        let threads = if variant == "vread" { maps.len() } else { 1 };
        let t = Instant::now();
        let sum = AtomicU64::new(0);
        std::thread::scope(|s| {
            for w in 0..threads {
                let (maps, sum) = (&maps, &sum);
                s.spawn(move || {
                    let mut acc = 0u64;
                    for (i, m) in maps.iter().enumerate() {
                        if threads > 1 && i != w {
                            continue;
                        }
                        for c in m.chunks_exact(REC) {
                            let r = rec(c);
                            acc = acc.wrapping_add(r.key.0 ^ r.src ^ r.shape ^ (r.cell as u64) ^ (r.xfer as u64) ^ r.flags as u64);
                        }
                    }
                    sum.fetch_add(acc, Ordering::Relaxed);
                });
            }
        });
        eprintln!("[{variant}] {threads} threads: read + parse every record {:.3} s (sum {:x})", t.elapsed().as_secs_f64(), sum.load(Ordering::Relaxed));
        return;
    }
    let single = match variant {
        "vprod" => false,
        "vprod1" => true,
        other => panic!("unknown variant {other}"),
    };

    // ---- setup (untimed)
    let door_b = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{dir}/door.bin")).unwrap()).unwrap() };
    let srcf = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{dir}/src.bin")).unwrap()).unwrap() };
    // Per file: its units (unit index, byte range) and its (shape, cell) drop verdicts.
    struct Scan {
        units: Vec<(u64, usize, usize)>,
        cells: FxHashMap<(u64, u32), bool>,
        recs: u64,
        dropped: u64,
        lookups: u64,
    }
    let scans: Vec<Scan> = std::thread::scope(|s| {
        let hs: Vec<_> = maps
            .iter()
            .map(|m| {
                s.spawn(move || {
                    let mut sc = Scan { units: Vec::new(), cells: Default::default(), recs: 0, dropped: 0, lookups: 0 };
                    let n = m.len() / REC;
                    for i in 0..n {
                        let c = &m[i * REC..(i + 1) * REC];
                        let r = rec(c);
                        if r.flags == 2 {
                            if let Some(u) = sc.units.last_mut() {
                                u.2 = i * REC;
                            }
                            sc.units.push((r.src, i * REC, n * REC));
                            continue;
                        }
                        sc.recs += 1;
                        let d = r.flags == 1;
                        if d {
                            sc.dropped += 1;
                        } else {
                            sc.lookups += 1;
                        }
                        let e = sc.cells.entry((r.shape, r.cell)).or_insert(d);
                        assert_eq!(*e, d, "a (shape, cell) both dropped and kept");
                    }
                    assert_eq!(sc.units.first().map(|u| u.1), Some(0), "a file starts with a unit");
                    sc
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    let mut d: FxHashMap<(u64, i16, i16), u32> = Default::default();
    for sc in &scans {
        for (&(shape, cell), &dropped) in &sc.cells {
            if let Some((x, y)) = cell_xy(cell) {
                let v = if dropped { DROPPED_D } else { 0 };
                let prev = d.insert((shape, x as i16, y as i16), v);
                assert!(prev.is_none_or(|p| p == v), "inconsistent drop verdicts");
                assert!(!dropped || x < 128, "a dropped cell past x 128");
            } else {
                assert!(!dropped, "a dropped row without a cell");
            }
        }
    }
    let table = CostToGo { d };
    let (recs, dropped, lookups): (u64, u64, u64) = scans.iter().fold((0, 0, 0), |a, s| (a.0 + s.recs, a.1 + s.dropped, a.2 + s.lookups));
    // The input: ids (DropNotes' runs), cells (per seq).
    let n_src = srcf.len() / 20;
    let ids: Vec<u64> = (0..n_src).map(|i| read_u64(&srcf, i * 20)).collect();
    let notes = DropNotes::new(&ids);
    for r in &notes.runs {
        assert_eq!(id_row(r.0) % 64, 0, "an input run not starting at a 64-row group");
        assert_eq!(id_layer(r.0), FRAME - 1);
    }
    let max_seq = ids.iter().map(|&id| id_seq(id)).max().unwrap() as usize;
    let mut seq_len = vec![0u32; max_seq + 1];
    for &id in &ids {
        seq_len[id_seq(id) as usize] = seq_len[id_seq(id) as usize].max(id_row(id) + 1);
    }
    let mut starts = vec![0u32; max_seq + 1];
    let mut acc = 0u32;
    for s in 0..=max_seq {
        starts[s] = acc;
        acc += seq_len[s];
    }
    let mut cells = vec![NO_CELL; acc as usize];
    for i in 0..n_src {
        let id = ids[i];
        cells[(starts[id_seq(id) as usize] + id_row(id)) as usize] = read_u32(&srcf, i * 20 + 8);
    }
    let cells_in = CellsIn { starts, cells };
    // The door at the frame's start (production's `Door::from_shards`).
    let n_door = door_b.len() / 40;
    let mut by_shard: FxHashMap<(u64, u32), Vec<Entry>> = Default::default();
    for i in 0..n_door {
        let b = &door_b[i * 40..i * 40 + 40];
        let k: Key = (read_u64(b, 16), read_u64(b, 24));
        by_shard.entry((read_u64(b, 0), read_u32(b, 8))).or_default().push((k, read_u64(b, 32)));
    }
    let n_shards = by_shard.len();
    let door = Door::from_shards(by_shard);
    let _ = std::fs::remove_dir_all(&edges_dir);
    let n_units: usize = scans.iter().map(|s| s.units.len()).sum();
    eprintln!(
        "[setup] {:.1} s (untimed): {recs} records ({dropped} dropped, {lookups} looked up), {n_units} units, door {n_door} in {n_shards} shards, input {n_src} rows in {} runs, level -1 table {} cells",
        t0.elapsed().as_secs_f64(),
        notes.runs.len(),
        table.d.len()
    );

    // ---- the wave (timed)
    let seqs = AtomicU32::new(0);
    let drop_mismatch = AtomicU64::new(0);
    let sh = Shared { door: &door, table: &table, notes: &notes, seqs: &seqs, cells_in: &cells_in, edges_dir: &edges_dir, drop_mismatch: &drop_mismatch };
    let workers = if single { 1 } else { maps.len() };
    assert!(single || workers == WORKERS, "16 worker files");
    let t = Instant::now();
    let done: Vec<Done> = if single {
        // One worker, the units in WAVE order (their indices), as a
        // one-worker production run pulls them.
        let mut all: Vec<(u64, &[u8])> = scans.iter().zip(&maps).flat_map(|(sc, m)| sc.units.iter().map(move |&(u, lo, hi)| (u, &m[lo..hi]))).collect();
        all.sort_by_key(|u| u.0);
        let units: Vec<&[u8]> = all.into_iter().map(|u| u.1).collect();
        vec![worker(&sh, 0, &units)]
    } else {
        std::thread::scope(|s| {
            let hs: Vec<_> = scans
                .iter()
                .zip(&maps)
                .enumerate()
                .map(|(w, (sc, m))| {
                    let sh = &sh;
                    let units: Vec<&[u8]> = sc.units.iter().map(|&(_, lo, hi)| &m[lo..hi]).collect();
                    s.spawn(move || worker(sh, w as u32, &units))
                })
                .collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        })
    };
    let t_wave = t.elapsed();
    let busy: f64 = done.iter().map(|d| d.busy.as_secs_f64()).sum();
    // The renumbering `end_frame` maps the new ids through (identity).
    // (A seq whose piece kept no row has width 0.)
    let mut widths = vec![0usize; seqs.load(Ordering::Relaxed) as usize];
    for d in &done {
        for &(seq, width) in &d.pieces {
            widths[seq as usize] = width;
        }
    }
    let mut starts = vec![0u64];
    let mut rn_ids = Vec::new();
    for (seq, &width) in widths.iter().enumerate() {
        rn_ids.extend((0..width as u32).map(|r| pack_id(FRAME, seq as u32, r)));
        starts.push(rn_ids.len() as u64);
    }
    let renumber = Renumber { layer: FRAME, first_seq: 0, starts, ids: rn_ids };
    let t = Instant::now();
    door.end_frame(workers, Some(&renumber));
    let t_door = t.elapsed();
    let (kept, flushes, flushed_rows, emitted, edge_records): (usize, u64, u64, u64, u64) =
        done.iter().fold((0, 0, 0, 0, 0), |a, d| (a.0 + d.kept, a.1 + d.flushes, a.2 + d.flushed_rows, a.3 + d.emitted, a.4 + d.edge_records));
    let pos_edges: usize = done.iter().map(|d| d.edges).sum();
    let t_edges: f64 = done.iter().map(|d| d.t_edges.as_secs_f64()).sum();
    let edge_words: u64 = done.iter().map(|d| d.edge_words).sum();
    let edge_bytes: u64 = std::fs::read_dir(edges_dir.join(format!("raw_f{FRAME:03}"))).map_or(0, |rd| rd.flatten().map(|e| e.metadata().unwrap().len()).sum());
    eprintln!(
        "[{variant}] wave {:.3} s ({workers} workers, idle {:.0}%), end_frame {:.3} s; raw {emitted} (want 31685344 at 16 workers), flushes {flushes} ({:.0} rows avg; want 184432 at 16), kept {kept} (want 6735699), door now {}, edge records {edge_records}, words {edge_words} (want 257724013), edge files {:.2} GB, drop notes {}, pos edges {pos_edges} (summed per worker), drop-verdict mismatches {}; t_edges {t_edges:.2} worker-s",
        t_wave.as_secs_f64(),
        100.0 * (1.0 - busy / (workers as f64 * t_wave.as_secs_f64())),
        t_door.as_secs_f64(),
        flushed_rows as f64 / flushes.max(1) as f64,
        door.len(),
        edge_bytes as f64 / 1e9,
        notes.noted(),
        drop_mismatch.load(Ordering::Relaxed)
    );
    phases::print_phases(t_wave, workers);
    if verify_on() {
        let mut v = std::mem::take(&mut *VERIFY.lock().unwrap());
        let n = v.len();
        v.sort_unstable();
        v.dedup();
        eprintln!("[verify] edge words {n} (want 257724013 = the capture's non-dropped emissions), distinct {} ", v.len());
    }
}
