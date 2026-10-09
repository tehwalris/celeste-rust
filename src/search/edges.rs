//! The recorded backward graph.
//!
//! At every flush the forward (`frame::ForwardSink`) records one 16-byte
//! record per EDGE (emitted state, source lane, transfer): the state's id,
//! the source's id and the remainder transfer (`search::arc_edges`, a
//! worker-local id). Records land in
//! `<level>/edges/raw/f{frame}/l{target layer}_w{worker}.bin` beside each
//! worker's transfer table (`x_w{worker}.bin`), and stay there through the
//! forward: `edges/done.txt` names the last frame whose records are complete.
//! The graph is INVERTED once, when it is first read (`EdgeGraph::open` ->
//! `invert`): per frame `compact_frame` merges the tables into the frame's
//! (`xfer/f{frame}.bin`, sorted by value) and sorts each layer's records by
//! target into the RUN `l{layer}/f{frame}.bin` (v5, `encode_sorted`: ~4 B an
//! edge): every edge recorded at that frame into that layer, sources in layer
//! `frame - 1`. A frame is inverted once its raw dir is gone. A RAISE inverts
//! the tree first, then adds a frame's new edges beside its runs, in RAISED
//! runs `l{layer}/f{frame}.raised.bin` (`compact_raised`, transfers appended
//! to the frame's table). The backward (`bfs`) reads only runs, raised ones
//! included.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use crate::frame::{id_layer, pack_id};

/// One edge record in a worker's buffer, ONE LANE, a u128 (`push_records`):
/// the target's (seq, row) (layer: the buffer's), the source's (seq, row)
/// (layer: the frame's input) and the transfer (worker-local id). The 64-lane
/// mask records of run v4 (24 B) averaged 1.006 lanes in room (6,2) gemskip
/// nodiag f70. A full buffer is written to the worker's raw file of its layer
/// as a CHUNK (`write_chunk`, ~3.6 B a record): a tree keeps its frames' raw
/// files until they are inverted, ~13.6G records in room (6,2) 100%.
type Word = u128;

/// Edges into `target` from the lanes `base + i` for the bits `i` of `mask`,
/// one transfer (a run returns one edge each: `mask` 1).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    pub target: u64,
    pub base: u64,
    /// The transfer: an index into the frame's table (`EdgeGraph::pair`).
    pub xfer: u32,
    pub mask: u64,
}

/// An edge in the compaction: `(target, source, transfer)`.
type Rec = (u64, u64, u32);

/// The edges of `mask`'s lanes from `base` into `target`, a record each.
#[inline]
pub fn push_records(out: &mut Vec<Word>, target: u64, base: u64, xfer: u32, mut mask: u64) {
    // The target's half once; one word per source.
    let (tseq, trow) = (crate::frame::id_seq(target) as u16 as u128, crate::frame::id_row(target) as u128);
    let head = tseq | trow << 16 | (xfer as u128) << 96;
    while mask != 0 {
        let src = base + mask.trailing_zeros() as u64;
        mask &= mask - 1;
        out.push(head | (crate::frame::id_seq(src) as u16 as u128) << 48 | (crate::frame::id_row(src) as u128) << 64);
    }
}

/// A record's target and source as (seq << 32 | row), and its transfer.
#[inline]
fn word_fields(w: Word) -> (u64, u64, u32) {
    let t = (w as u64 & 0xffff) << 32 | (w >> 16) as u64 & 0xffff_ffff;
    let s = ((w >> 48) as u64 & 0xffff) << 32 | (w >> 64) as u64 & 0xffff_ffff;
    (t, s, (w >> 96) as u32)
}

/// Records a worker buffers per layer before it writes them as a chunk.
pub const CHUNK_WORDS: usize = 1 << 16;

/// Slots (log2) of a chunk's transfer predictor: per source hash, the last
/// transfer recorded from it.
const PREDICT_BITS: u32 = 10;

#[inline]
fn predict_slot(s: u64) -> usize {
    (s.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> (64 - PREDICT_BITS)) as usize
}

#[inline]
fn zigzag(d: i64) -> u64 {
    ((d << 1) ^ (d >> 63)) as u64
}

#[inline]
fn unzigzag(v: u64) -> i64 {
    (v >> 1) as i64 ^ -((v & 1) as i64)
}

/// Append `words` to `out` as one CHUNK: varints of the record count and
/// the payload's bytes, then per record, varints: the target's (seq << 32 |
/// row) as a zigzag delta from the previous record's; the source's likewise,
/// shifted left by one over a HIT bit (the transfer is the predictor's for
/// the source's slot, `PREDICT_BITS`); the transfer unless hit. Deltas and
/// predictor restart at each chunk. Room (6,2) 100% f57: 3.57 B a record
/// (target 1.66, source 1.1, the transfer 0.8 at a 66% hit rate), against 16.
pub fn write_chunk(out: &mut Vec<u8>, words: &[Word]) {
    if words.is_empty() {
        return;
    }
    // The payload is written after room for the header, which is then
    // written and the payload moved up to it (no second buffer).
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

/// Decode a raw file's chunks (`write_chunk`): `f(target, source, transfer)`
/// per record, in the order written. Returns the records.
fn read_chunks(b: &[u8], name: &Path, mut f: impl FnMut(u64, u64, u32)) -> Result<u64> {
    let (mut pos, mut records) = (0usize, 0u64);
    while pos < b.len() {
        let n = get_varint_checked(b, &mut pos).with_context(|| format!("{}: a truncated chunk header", name.display()))?;
        let len = get_varint_checked(b, &mut pos).with_context(|| format!("{}: a truncated chunk header", name.display()))? as usize;
        ensure!(b.len() - pos >= len, "{}: a truncated chunk ({len} bytes, {} left)", name.display(), b.len() - pos);
        let p = &b[pos..pos + len];
        let mut predict = [u32::MAX; 1 << PREDICT_BITS];
        let (mut q, mut t, mut s) = (0usize, 0u64, 0u64);
        for _ in 0..n {
            t = t.wrapping_add(unzigzag(get_varint(p, &mut q)) as u64);
            let head = get_varint(p, &mut q);
            s = s.wrapping_add(unzigzag(head >> 1) as u64);
            let slot = predict_slot(s);
            if head & 1 == 0 {
                predict[slot] = get_varint(p, &mut q) as u32;
            }
            f(t, s, predict[slot]);
        }
        ensure!(q == len, "{}: a chunk of {n} records decoded {q} of its {len} bytes", name.display());
        pos += len;
        records += n;
    }
    Ok(records)
}

/// The records of a raw file, from its chunk headers alone.
fn count_chunks(b: &[u8], name: &Path) -> Result<u64> {
    let (mut pos, mut records) = (0usize, 0u64);
    while pos < b.len() {
        let n = get_varint_checked(b, &mut pos).with_context(|| format!("{}: a truncated chunk header", name.display()))?;
        let len = get_varint_checked(b, &mut pos).with_context(|| format!("{}: a truncated chunk header", name.display()))? as usize;
        ensure!(b.len() - pos >= len, "{}: a truncated chunk ({len} bytes, {} left)", name.display(), b.len() - pos);
        pos += len;
        records += n;
    }
    Ok(records)
}

/// A record of layer `layer` recorded at `frame` (source in layer
/// `frame - 1`), its transfer id mapped through `remap` into the frame's.
#[inline]
fn to_rec(t: u64, s: u64, x: u32, layer: u32, frame: u32, remap: &[u32]) -> Rec {
    (pack_id(layer, (t >> 32) as u32, t as u32), pack_id(frame - 1, (s >> 32) as u32, s as u32), remap[x as usize])
}

fn layer_dir(dir: &Path, layer: u32) -> PathBuf {
    dir.join(format!("l{:03}", layer))
}

fn run_path(dir: &Path, layer: u32, frame: u32) -> PathBuf {
    layer_dir(dir, layer).join(format!("f{:03}.bin", frame))
}

/// Where frame `frame`'s worker files go before compaction.
pub fn raw_dir(dir: &Path, frame: u32) -> PathBuf {
    dir.join("raw").join(format!("f{:03}", frame))
}

/// A worker's raw file for one target layer of one frame.
pub fn raw_path(dir: &Path, frame: u32, layer: u32, worker: u32) -> PathBuf {
    raw_dir(dir, frame).join(format!("l{:03}_w{:03}.bin", layer, worker))
}

/// A worker's transfer table of one frame: the pairs its records' ids index.
pub fn raw_xfer_path(dir: &Path, frame: u32, worker: u32) -> PathBuf {
    raw_dir(dir, frame).join(format!("x_w{:03}.bin", worker))
}

/// The frame's transfer table (`EdgeGraph::pair`).
fn xfer_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("xfer").join(format!("f{:03}.bin", frame))
}

/// A raw record file: its path and the worker that wrote it.
type RawFile = (PathBuf, u32);

/// The raw files of frame `frame` grouped by target layer, with their
/// total bytes.
fn raw_files(dir: &Path, frame: u32) -> Vec<(u32, Vec<RawFile>, u64)> {
    let mut by_layer: std::collections::BTreeMap<u32, (Vec<RawFile>, u64)> = Default::default();
    for e in std::fs::read_dir(raw_dir(dir, frame)).into_iter().flatten().flatten() {
        let p = e.path();
        let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
        let Some(layer) = n.strip_prefix('l').and_then(|s| s.get(..3)).and_then(|s| s.parse::<u32>().ok()) else { continue };
        let Some(worker) = n.strip_suffix(".bin").and_then(|s| s.rsplit_once("_w")).and_then(|(_, w)| w.parse::<u32>().ok()) else { continue };
        let bytes = std::fs::metadata(&p).map(|m| m.len()).unwrap_or(0);
        let e = by_layer.entry(layer).or_default();
        e.0.push((p, worker));
        e.1 += bytes;
    }
    by_layer
        .into_iter()
        .map(|(layer, (mut files, bytes))| {
            files.sort();
            (layer, files, bytes)
        })
        .collect()
}

/// The last frame whose runs are complete (`<dir>/done.txt`); `None`: no
/// marker, every frame trusted.
pub fn done_frame(dir: &Path) -> Option<u32> {
    std::fs::read_to_string(dir.join("done.txt")).ok().and_then(|s| s.trim().parse().ok())
}

pub fn set_done_frame(dir: &Path, frame: u32) -> Result<()> {
    std::fs::create_dir_all(dir)?;
    let tmp = dir.join("done.tmp");
    std::fs::write(&tmp, format!("{frame}\n"))?;
    std::fs::rename(&tmp, dir.join("done.txt"))?;
    Ok(())
}

/// The layers with an `l{layer}` dir under `dir` (the runs).
fn layers(dir: &Path) -> Vec<u32> {
    let mut v: Vec<u32> = std::fs::read_dir(dir)
        .into_iter()
        .flatten()
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter_map(|p| {
            let n = p.file_name()?.to_str()?;
            n.strip_prefix('l')?.parse().ok()
        })
        .collect();
    v.sort_unstable();
    v
}

pub struct CompactStats {
    pub records: u64,
    pub edges: u64,
    pub bytes: u64,
    pub layers: usize,
    /// Wall time in the three phases: read + bucket, sort + encode, write.
    pub t_read: std::time::Duration,
    pub t_sort: std::time::Duration,
    pub t_write: std::time::Duration,
}

/// Edges per index block of a run.
const STRIDE: usize = 256;
const RUN_MAGIC: &[u8; 4] = b"CERN";
const RUN_VERSION: u32 = 5;
/// A run's header: magic, version, edges, blocks, source pieces, transfers.
const RUN_HEADER: usize = 40;

#[inline]
fn put_varint(out: &mut Vec<u8>, mut v: u64) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
}

/// `get_varint` over bytes that may end early (`None`).
fn get_varint_checked(b: &[u8], pos: &mut usize) -> Option<u64> {
    let mut v = 0u64;
    let mut shift = 0;
    loop {
        let x = *b.get(*pos)?;
        *pos += 1;
        v |= ((x & 0x7f) as u64).checked_shl(shift)?;
        if x & 0x80 == 0 {
            return Some(v);
        }
        shift += 7;
        if shift >= 64 {
            return None;
        }
    }
}

#[inline]
fn get_varint(b: &[u8], pos: &mut usize) -> u64 {
    let mut v = 0u64;
    let mut shift = 0;
    loop {
        let x = b[*pos];
        *pos += 1;
        v |= ((x & 0x7f) as u64) << shift;
        if x & 0x80 == 0 {
            return v;
        }
        shift += 7;
    }
}

/// What a run's edges are coded against, from its records: the SOURCE
/// pieces (the previous layer's checkpoint files, by `seq`, each as many
/// rows as the records reach) laid end to end, so a source is one dense
/// number below the layer's row count; and the run's TRANSFERS by
/// descending use, so the common ones code in a byte. (Room (6,2) gemskip
/// nodiag f70: 8.8 B an edge in run v4, ~5 B so.)
struct RunTables {
    /// Per source `seq`: its first dense number (`u32::MAX`: absent).
    seq_start: Vec<u32>,
    /// `(seq, first dense number)`, by dense number.
    seqs: Vec<(u32, u32)>,
    /// Per frame transfer id: its rank in the run (`u32::MAX`: unused).
    rank: Vec<u32>,
    /// The frame transfer id of each rank.
    xfers: Vec<u32>,
}

/// Counts toward a run's tables (`RunTables::new`).
#[derive(Default)]
struct TableCounts {
    /// Per source `seq`, one past the highest row an edge leaves from.
    rows: Vec<u32>,
    /// Per frame transfer id, the records that carry it.
    uses: Vec<u64>,
}

impl TableCounts {
    fn add(&mut self, &(_, src, xfer): &Rec) {
        let seq = crate::frame::id_seq(src) as usize;
        if self.rows.len() <= seq {
            self.rows.resize(seq + 1, 0);
        }
        self.rows[seq] = self.rows[seq].max(crate::frame::id_row(src) + 1);
        let x = xfer as usize;
        if self.uses.len() <= x {
            self.uses.resize(x + 1, 0);
        }
        self.uses[x] += 1;
    }

    fn merge(mut self, o: TableCounts) -> TableCounts {
        if self.rows.len() < o.rows.len() {
            self.rows.resize(o.rows.len(), 0);
        }
        for (a, b) in self.rows.iter_mut().zip(o.rows) {
            *a = (*a).max(b);
        }
        if self.uses.len() < o.uses.len() {
            self.uses.resize(o.uses.len(), 0);
        }
        for (a, b) in self.uses.iter_mut().zip(o.uses) {
            *a += b;
        }
        self
    }
}

impl RunTables {
    fn new(c: TableCounts) -> Result<Self> {
        let mut seq_start = vec![u32::MAX; c.rows.len()];
        let mut seqs = Vec::new();
        let mut acc = 0u64;
        for (seq, &rows) in c.rows.iter().enumerate() {
            if rows > 0 {
                seq_start[seq] = acc as u32;
                seqs.push((seq as u32, acc as u32));
                acc += rows as u64;
            }
        }
        ensure!(acc < u32::MAX as u64, "a run's sources span {acc} rows, past a u32");
        let mut xfers: Vec<u32> = (0..c.uses.len() as u32).filter(|&x| c.uses[x as usize] > 0).collect();
        xfers.sort_unstable_by_key(|&x| (std::cmp::Reverse(c.uses[x as usize]), x));
        let mut rank = vec![u32::MAX; c.uses.len()];
        for (r, &x) in xfers.iter().enumerate() {
            rank[x as usize] = r as u32;
        }
        Ok(RunTables { seq_start, seqs, rank, xfers })
    }
}

/// Encode records sorted by target into blocks of `STRIDE` edges, a
/// target's edges sorted by (source, transfer rank) and deduplicated. Per
/// edge, varints: a HEAD, `delta << 1 | 1` at a block's or a target's first
/// edge (the target's delta, then the source, dense (`RunTables`), in
/// full), else `delta << 1` (the source's delta from the previous edge's,
/// same target); then the transfer's rank. Returns `(index (first target,
/// offset), stream, edges)`. (Room (6,2) gemskip nodiag f70: 4.3 B an edge,
/// 8.8 B a record in run v4; room (1,0) f44, 2.3 lanes a record: 3.1 B an
/// edge, 9.5 B a record.)
fn encode_sorted(recs: &[Rec], tabs: &RunTables) -> (Vec<(u64, u64)>, Vec<u8>, u64) {
    let mut index: Vec<(u64, u64)> = Vec::with_capacity(recs.len() / STRIDE + 1);
    let mut out: Vec<u8> = Vec::with_capacity(recs.len() * 5);
    let (mut edges, mut in_block) = (0u64, 0usize);
    let (mut prev_t, mut prev_s) = (0u64, 0u32);
    let mut group: Vec<(u32, u32)> = Vec::new();
    let mut i = 0;
    while i < recs.len() {
        let t = recs[i].0;
        group.clear();
        while i < recs.len() && recs[i].0 == t {
            let (_, src, x) = recs[i];
            group.push((tabs.seq_start[crate::frame::id_seq(src) as usize] + crate::frame::id_row(src), tabs.rank[x as usize]));
            i += 1;
        }
        group.sort_unstable();
        group.dedup();
        for (k, &(s, r)) in group.iter().enumerate() {
            let fresh = in_block == 0;
            if fresh {
                index.push((t, out.len() as u64));
                prev_t = t;
            }
            if fresh || k == 0 {
                put_varint(&mut out, (t - prev_t) << 1 | 1);
                put_varint(&mut out, s as u64);
            } else {
                put_varint(&mut out, ((s - prev_s) as u64) << 1);
            }
            put_varint(&mut out, r as u64);
            prev_t = t;
            prev_s = s;
            edges += 1;
            in_block += 1;
            if in_block == STRIDE {
                in_block = 0;
            }
        }
    }
    (index, out, edges)
}

/// A run's header, index and tables (`Run::open` reads them); the stream
/// follows, `index` offsets relative to its start.
fn write_head(w: &mut impl std::io::Write, edges: u64, index: &[(u64, u64)], tabs: &RunTables) -> Result<()> {
    w.write_all(RUN_MAGIC)?;
    w.write_all(&RUN_VERSION.to_le_bytes())?;
    for v in [edges, index.len() as u64, tabs.seqs.len() as u64, tabs.xfers.len() as u64] {
        w.write_all(&v.to_le_bytes())?;
    }
    for &(t, o) in index {
        w.write_all(&t.to_le_bytes())?;
        w.write_all(&o.to_le_bytes())?;
    }
    for &(seq, start) in &tabs.seqs {
        w.write_all(&seq.to_le_bytes())?;
        w.write_all(&start.to_le_bytes())?;
    }
    for &x in &tabs.xfers {
        w.write_all(&x.to_le_bytes())?;
    }
    Ok(())
}

/// The bytes `write_head` writes.
fn head_bytes(index: &[(u64, u64)], tabs: &RunTables) -> u64 {
    (RUN_HEADER + index.len() * 16 + tabs.seqs.len() * 8 + tabs.xfers.len() * 4) as u64
}
/// Records per sort chunk of a big layer: bounds the sort buffer (~1 GB)
/// whatever the layer's size.
const CHUNK_RECORDS: usize = 32 << 20;

/// A layer's sort by target, one contiguous range of rank BUCKETS (at most
/// `chunk_records` records) at a time: each range scattered by a pass over
/// every file, its buckets sorted, appended to a `RunWriter` streaming to
/// `stream_path`. Buckets are target order, so the run is sorted as a
/// whole. Returns (the writer, sort time, write time).
fn sort_layer_chunked(
    files: &[RawView],
    layer: u32,
    frame: u32,
    chunk_records: usize,
    stream_path: &Path,
) -> Result<(RunWriter, std::time::Duration, std::time::Duration)> {
    let t_sort0 = std::time::Instant::now();
    let mut t_write = std::time::Duration::ZERO;
    let n_total: usize = files.iter().map(|v| v.records as usize).sum();
    // Per file the targets' extent, and the counts toward the run's tables.
    let scanned: Vec<(Vec<u32>, TableCounts)> = std::thread::scope(|scope| {
        let hs: Vec<_> = files
            .iter()
            .map(|v| {
                scope.spawn(move || -> Result<(Vec<u32>, TableCounts)> {
                    let mut max_row: Vec<u32> = Vec::new();
                    let mut counts = TableCounts::default();
                    read_chunks(v.bytes, v.path, |t, s, x| {
                        let (seq, row) = ((t >> 32) as usize, t as u32);
                        if max_row.len() <= seq {
                            max_row.resize(seq + 1, 0);
                        }
                        max_row[seq] = max_row[seq].max(row + 1);
                        counts.add(&to_rec(t, s, x, layer, frame, v.remap));
                    })?;
                    Ok((max_row, counts))
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("scan")).collect::<Result<Vec<_>>>()
    })?;
    let mut counts = TableCounts::default();
    let mut per_file_max: Vec<Vec<u32>> = Vec::with_capacity(scanned.len());
    for (m, c) in scanned {
        per_file_max.push(m);
        counts = counts.merge(c);
    }
    let mut writer = RunWriter::new(stream_path, RunTables::new(counts)?)?;
    let n_seq = per_file_max.iter().map(|m| m.len()).max().unwrap_or(0);
    let mut piece_off: Vec<usize> = vec![0; n_seq + 1];
    for seq in 0..n_seq {
        let w = per_file_max.iter().map(|m| m.get(seq).copied().unwrap_or(0)).max().unwrap_or(0) as usize;
        piece_off[seq + 1] = piece_off[seq] + w;
    }
    let n_ranks = piece_off[n_seq];
    let rank_of = |t: u64| piece_off[(t >> 32) as usize] + t as u32 as usize;
    // BUCKETS of 2^shift consecutive ranks, ~BUCKET_RECORDS records each on
    // average: per file a histogram over buckets (its own, no atomics),
    // then per chunk of buckets each file scatters into its own slots of
    // every bucket, and each bucket is sorted by target in cache. (Counting
    // by RANK with shared atomic cursors, a random atomic on a tens-of-MB
    // array per record twice, was ~40% of a compaction.)
    const BUCKET_RECORDS: usize = 1 << 14;
    let per_rank = (n_total / n_ranks.max(1)).max(1);
    let shift = (BUCKET_RECORDS / per_rank).max(1).ilog2();
    let n_buckets = (n_ranks >> shift) + 1;
    let bucket_of = |t: u64| rank_of(t) >> shift;
    let per_file: Vec<Vec<u32>> = std::thread::scope(|scope| {
        let hs: Vec<_> = files
            .iter()
            .map(|v| {
                let bucket_of = &bucket_of;
                scope.spawn(move || -> Result<Vec<u32>> {
                    let mut h = vec![0u32; n_buckets];
                    read_chunks(v.bytes, v.path, |t, _, _| h[bucket_of(t)] += 1)?;
                    Ok(h)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("bucket histogram")).collect::<Result<Vec<_>>>()
    })?;
    let totals: Vec<usize> = (0..n_buckets).map(|k| per_file.iter().map(|h| h[k] as usize).sum()).collect();
    debug_assert_eq!(totals.iter().sum::<usize>(), n_total);
    // Chunk cuts at bucket boundaries: a chunk closes once it holds
    // `chunk_records` (a bucket is never split).
    let mut cuts: Vec<(usize, usize)> = vec![(0, 0)]; // (bucket, first position)
    let mut acc = 0usize;
    for (k, &c) in totals.iter().enumerate() {
        if acc - cuts.last().unwrap().1 >= chunk_records && c > 0 {
            cuts.push((k, acc));
        }
        acc += c;
    }
    cuts.push((n_buckets, n_total));
    // One buffer for every chunk: a fresh one faulted in each time.
    let max_chunk = cuts.windows(2).map(|w| w[1].1 - w[0].1).max().unwrap_or(0);
    let mut out: Vec<Rec> = Vec::with_capacity(max_chunk);
    let threads = crate::frame::threads().max(1);
    for w in cuts.windows(2) {
        let ((k_lo, base), (k_hi, end)) = (w[0], w[1]);
        let n_chunk = end - base;
        if n_chunk == 0 {
            continue;
        }
        let nb = k_hi - k_lo;
        let mut starts = vec![0usize; nb + 1];
        for k in 0..nb {
            starts[k + 1] = starts[k] + totals[k_lo + k];
        }
        // File f's slots in bucket k start after the earlier files'.
        let mut cursors: Vec<Vec<usize>> = Vec::with_capacity(files.len());
        let mut next = starts[..nb].to_vec();
        for h in &per_file {
            cursors.push(next.clone());
            for k in 0..nb {
                next[k] += h[k_lo + k] as usize;
            }
        }
        out.clear();
        // SAFETY: every position 0..n_chunk is written exactly once below
        // (the files' slots partition each bucket, the buckets the chunk);
        // the element type has no drop glue.
        #[allow(clippy::uninit_vec)]
        unsafe {
            out.set_len(n_chunk);
        }
        {
            let out_ptr = out.as_mut_ptr() as usize;
            std::thread::scope(|scope| -> Result<()> {
                let hs: Vec<_> = files
                    .iter()
                    .zip(cursors)
                    .map(|(v, mut cur)| {
                        let bucket_of = &bucket_of;
                        scope.spawn(move || -> Result<()> {
                            let out_ptr = out_ptr as *mut Rec;
                            read_chunks(v.bytes, v.path, |t, s, x| {
                                let k = bucket_of(t);
                                if k < k_lo || k >= k_hi {
                                    return;
                                }
                                let pos = cur[k - k_lo];
                                cur[k - k_lo] += 1;
                                // SAFETY: this file's own slot, unique and < n_chunk.
                                unsafe { out_ptr.add(pos).write(to_rec(t, s, x, layer, frame, v.remap)) };
                            })?;
                            Ok(())
                        })
                    })
                    .collect();
                for h in hs {
                    h.join().expect("edge scatter panicked")?;
                }
                Ok(())
            })?;
        }
        // Each bucket by target (a target's records are one run; their
        // order within it is `encode_sorted`'s).
        {
            let mut slices: Vec<Vec<&mut [Rec]>> = (0..threads).map(|_| Vec::new()).collect();
            let mut rest: &mut [Rec] = &mut out;
            for k in 0..nb {
                let (head, tail) = rest.split_at_mut(starts[k + 1] - starts[k]);
                slices[k % threads].push(head);
                rest = tail;
            }
            std::thread::scope(|scope| {
                for mine in slices {
                    scope.spawn(move || {
                        for sl in mine {
                            sl.sort_unstable_by_key(|r| r.0);
                        }
                    });
                }
            });
        }
        let t0 = std::time::Instant::now();
        writer.append(&out, crate::frame::threads() * 2)?;
        t_write += t0.elapsed();
    }
    Ok((writer, t_sort0.elapsed() - t_write, t_write))
}

/// A raw file of one layer, mapped or read: its bytes, its worker's map into
/// the frame's transfer ids, its records (`count_chunks`).
struct RawView<'a> {
    bytes: &'a [u8],
    remap: &'a [u32],
    records: u64,
    path: &'a Path,
}

/// A run file written in sorted slices (index in memory, stream to a temp
/// file); `finish` writes the run.
struct RunWriter {
    tabs: RunTables,
    index: Vec<(u64, u64)>,
    stream: std::io::BufWriter<std::fs::File>,
    stream_path: PathBuf,
    off: u64,
    edges: u64,
}

impl RunWriter {
    fn new(stream_path: &Path, tabs: RunTables) -> Result<Self> {
        Ok(RunWriter {
            tabs,
            index: Vec::new(),
            stream: std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(stream_path)?),
            stream_path: stream_path.to_path_buf(),
            off: 0,
            edges: 0,
        })
    }

    /// Append a slice sorted by target, whose targets all follow the
    /// previous slice's; `encode_chunks` parallel ranges.
    fn append(&mut self, sorted: &[Rec], encode_chunks: usize) -> Result<()> {
        use std::io::Write;
        debug_assert!(self.index.last().is_none_or(|&(t, _)| sorted.first().is_none_or(|r| r.0 > t)));
        let tabs = &self.tabs;
        let encoded: Vec<(Vec<(u64, u64)>, Vec<u8>, u64)> = if encode_chunks <= 1 {
            vec![encode_sorted(sorted, tabs)]
        } else {
            let cuts = cuts_at_target_changes(sorted, encode_chunks);
            std::thread::scope(|scope| {
                let hs: Vec<_> = cuts
                    .windows(2)
                    .map(|w| {
                        let sl = &sorted[w[0]..w[1]];
                        scope.spawn(move || encode_sorted(sl, tabs))
                    })
                    .collect();
                hs.into_iter().map(|h| h.join().expect("edge encoder panicked")).collect()
            })
        };
        for (ix, s, e) in &encoded {
            for &(t, o) in ix {
                self.index.push((t, o + self.off));
            }
            self.stream.write_all(s)?;
            self.off += s.len() as u64;
            self.edges += e;
        }
        Ok(())
    }

    /// Write the run to `tmp`, rename it to `path`. Returns (edges, bytes).
    fn finish(mut self, path: &Path, tmp: &Path) -> Result<(u64, u64)> {
        use std::io::Write;
        self.stream.flush()?;
        drop(self.stream);
        {
            let mut w = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(tmp)?);
            write_head(&mut w, self.edges, &self.index, &self.tabs)?;
            let mut s = std::fs::File::open(&self.stream_path)?;
            std::io::copy(&mut s, &mut w)?;
            w.flush()?;
        }
        std::fs::remove_file(&self.stream_path)?;
        std::fs::rename(tmp, path)?;
        Ok((self.edges, head_bytes(&self.index, &self.tabs) + self.off))
    }
}

/// Boundaries of `chunks` ranges over `recs` (sorted by target), moved
/// forward so no target's group straddles one.
fn cuts_at_target_changes(recs: &[Rec], chunks: usize) -> Vec<usize> {
    let n = recs.len();
    let mut cuts: Vec<usize> = vec![0];
    for k in 1..chunks {
        let mut i = (k * n) / chunks;
        while i < n && i > 0 && recs[i].0 == recs[i - 1].0 {
            i += 1;
        }
        cuts.push(i);
    }
    cuts.push(n);
    cuts.dedup();
    cuts
}

/// Encode a layer's records into a run file (`tmp` then renamed to `path`),
/// in one piece. Returns (edges, bytes).
fn write_run(mut recs: Vec<Rec>, path: &Path, tmp: &Path) -> Result<(u64, u64)> {
    let tabs = RunTables::new(recs.iter().fold(TableCounts::default(), |mut c, r| {
        c.add(r);
        c
    }))?;
    recs.sort_unstable_by_key(|r| r.0);
    let (index, stream, edges) = encode_sorted(&recs, &tabs);
    {
        use std::io::Write;
        let mut w = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(tmp)?);
        write_head(&mut w, edges, &index, &tabs)?;
        w.write_all(&stream)?;
        w.flush()?;
    }
    std::fs::rename(tmp, path)?;
    Ok((edges, head_bytes(&index, &tabs) + stream.len() as u64))
}
/// A raw record's bytes as `compact` estimates a layer's size from its files.
const BYTES_PER_RECORD: usize = 4;
/// Records above which a layer gets the parallel counting sort; smaller
/// layers are comparison-sorted whole, several at a time (~40 ms at this
/// size, so no single small layer becomes the critical path).
const BIG_LAYER: usize = 1 << 19;

/// After frame `frame`: turn every layer's worker files into the run
/// `l{layer}/f{frame}.bin` and delete them. Big layers one at a time with
/// every thread; small layers in parallel, one thread each.
pub fn compact_frame(dir: &Path, frame: u32) -> Result<CompactStats> {
    compact(dir, frame, false)
}

/// A RAISE's records at frame `frame` (`frame::ForwardState::raise`: only
/// edges the frame's runs do not hold, and the frame's earlier raised run
/// reopened, `reopen_raised`) into the RAISED runs `l{layer}/f{frame}.raised.bin`
/// beside its runs; their new transfers are appended to the frame's table,
/// so the runs' ids keep their meaning. A frame's edges are its runs' and
/// its raised runs' (`EdgeGraph`).
pub fn compact_raised(dir: &Path, frame: u32) -> Result<CompactStats> {
    compact(dir, frame, true)
}

fn compact(dir: &Path, frame: u32, raised: bool) -> Result<CompactStats> {
    let out_path = |layer: u32| if raised { raised_path(dir, layer, frame) } else { run_path(dir, layer, frame) };
    let remaps = merge_xfer_tables(dir, frame, raised)?;
    let remap_of = |worker: u32| -> &[u32] { remaps.get(&worker).map_or(&[], |v| v.as_slice()) };
    let mut layers: Vec<(u32, Vec<RawFile>, u64)> = raw_files(dir, frame);
    // An interrupted compaction: a layer whose run is in place (renamed
    // only once complete) lost some raw files to it; the rest are stale.
    if !raised {
        let mut pending = Vec::with_capacity(layers.len());
        for (layer, files, bytes) in layers {
            if run_path(dir, layer, frame).exists() {
                for (f, _) in &files {
                    std::fs::remove_file(f)?;
                }
            } else {
                pending.push((layer, files, bytes));
            }
        }
        layers = pending;
    }
    layers.sort_by_key(|&(_, _, bytes)| std::cmp::Reverse(bytes));
    let mut st = CompactStats {
        records: 0,
        edges: 0,
        bytes: 0,
        layers: layers.len(),
        t_read: std::time::Duration::ZERO,
        t_sort: std::time::Duration::ZERO,
        t_write: std::time::Duration::ZERO,
    };
    let (big, small): (Vec<_>, Vec<_>) = layers.into_iter().partition(|(_, _, bytes)| *bytes as usize >= BIG_LAYER * BYTES_PER_RECORD);
    for (layer, files, _) in &big {
        let t0 = std::time::Instant::now();
        // MAPPED, not read: a big layer is GBs, and a heap copy would sit in
        // RSS through the compaction; mapped, it is page cache.
        let maps: Vec<memmap2::Mmap> = files
            .iter()
            .map(|(f, _)| -> Result<memmap2::Mmap> {
                let file = std::fs::File::open(f).with_context(|| f.display().to_string())?;
                // SAFETY: the worker files are complete and never modified
                // once the frame's wave is over; they are deleted below.
                Ok(unsafe { memmap2::Mmap::map(&file)? })
            })
            .collect::<Result<_>>()?;
        let views: Vec<RawView> = maps
            .iter()
            .zip(files)
            .map(|(m, (f, w))| Ok(RawView { bytes: &m[..], remap: remap_of(*w), records: count_chunks(m, f)?, path: f }))
            .collect::<Result<_>>()?;
        st.records += views.iter().map(|v| v.records).sum::<u64>();
        st.t_read += t0.elapsed();
        // Sorted in CHUNKS streamed into the run: a bounded sort buffer.
        let ldir = layer_dir(dir, *layer);
        std::fs::create_dir_all(&ldir)?;
        let (writer, t_sort, t_write) = sort_layer_chunked(&views, *layer, frame, CHUNK_RECORDS, &ldir.join(format!("tmp-f{:03}.stream", frame)))?;
        drop(views);
        drop(maps);
        st.t_sort += t_sort;
        let t0 = std::time::Instant::now();
        let (edges, bytes) = writer.finish(&out_path(*layer), &ldir.join(format!("tmp-f{:03}.bin", frame)))?;
        for (f, _) in files {
            std::fs::remove_file(f)?;
        }
        st.edges += edges;
        st.bytes += bytes;
        st.t_write += t_write + t0.elapsed();
    }
    // The small layers: a pool of threads, a layer each.
    let t0 = std::time::Instant::now();
    let next = std::sync::atomic::AtomicUsize::new(0);
    let totals: Vec<(u64, u64, u64)> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..crate::frame::threads().min(small.len().max(1)))
            .map(|_| {
                let (small, next, remap_of, out_path) = (&small, &next, &remap_of, &out_path);
                scope.spawn(move || -> Result<(u64, u64, u64)> {
                    let (mut records, mut edges, mut bytes) = (0u64, 0u64, 0u64);
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some((layer, files, fbytes)) = small.get(i) else { break };
                        let mut recs: Vec<Rec> = Vec::with_capacity(*fbytes as usize / 2);
                        for (f, w) in files {
                            let b = std::fs::read(f).with_context(|| f.display().to_string())?;
                            let remap = remap_of(*w);
                            read_chunks(&b, f, |t, s, x| recs.push(to_rec(t, s, x, *layer, frame, remap)))?;
                        }
                        let ldir = layer_dir(dir, *layer);
                        std::fs::create_dir_all(&ldir)?;
                        let n = recs.len() as u64;
                        let (p, b) = write_run(recs, &out_path(*layer), &ldir.join(format!("tmp-f{:03}.bin", frame)))?;
                        for (f, _) in files {
                            std::fs::remove_file(f)?;
                        }
                        records += n;
                        edges += p;
                        bytes += b;
                    }
                    Ok((records, edges, bytes))
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("edge compaction worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    for (r, p, b) in totals {
        st.records += r;
        st.edges += p;
        st.bytes += b;
    }
    st.t_sort += t0.elapsed();
    for w in remaps.keys() {
        std::fs::remove_file(raw_xfer_path(dir, frame, *w))?;
    }
    // Last: a frame is inverted once its raw dir is gone (`invert`).
    let raw = raw_dir(dir, frame);
    if raw.exists() {
        std::fs::remove_dir(&raw).with_context(|| format!("{}: left over after the compaction", raw.display()))?;
    }
    Ok(st)
}

/// Merge the workers' transfer tables into the frame's (`xfer/f{frame}.bin`,
/// distinct pairs sorted by value: independent of scheduling; `append`: the
/// existing table first, as it is, then the new pairs sorted). Returns per
/// worker the map from its ids to the frame's.
fn merge_xfer_tables(dir: &Path, frame: u32, append: bool) -> Result<rustc_hash::FxHashMap<u32, Vec<u32>>> {
    use super::arc_edges::{decode_pair, encode_pair, Pair, PAIR_BYTES};
    let read = |p: &Path| -> Result<Vec<Pair>> {
        let b = std::fs::read(p).with_context(|| p.display().to_string())?;
        ensure!(b.len() % PAIR_BYTES == 0, "{}: truncated transfer table", p.display());
        Ok(b.chunks_exact(PAIR_BYTES).map(decode_pair).collect())
    };
    let mut tables: Vec<(u32, Vec<Pair>)> = Vec::new();
    for e in std::fs::read_dir(raw_dir(dir, frame)).into_iter().flatten().flatten() {
        let p = e.path();
        let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
        let Some(w) = n.strip_prefix("x_w").and_then(|s| s.strip_suffix(".bin")).and_then(|s| s.parse::<u32>().ok()) else { continue };
        tables.push((w, read(&p)?));
    }
    let path = xfer_path(dir, frame);
    let mut all: Vec<Pair> = if append && path.exists() { read(&path)? } else { Vec::new() };
    let mut index: rustc_hash::FxHashMap<Pair, u32> = all.iter().enumerate().map(|(i, p)| (*p, i as u32)).collect();
    let mut new: Vec<Pair> = tables.iter().flat_map(|(_, t)| t.iter().copied()).filter(|p| !index.contains_key(p)).collect();
    new.sort_unstable();
    new.dedup();
    for p in new {
        index.insert(p, all.len() as u32);
        all.push(p);
    }
    let mut buf = Vec::with_capacity(all.len() * PAIR_BYTES);
    for p in &all {
        encode_pair(&mut buf, p);
    }
    std::fs::create_dir_all(path.parent().expect("an xfer dir"))?;
    let tmp = path.with_extension("tmp");
    std::fs::write(&tmp, buf)?;
    std::fs::rename(&tmp, &path)?;
    Ok(tables.into_iter().map(|(w, t)| (w, t.iter().map(|p| index[p]).collect())).collect())
}

/// The worker number a reopened raised run's edges are written back under
/// (`reopen_raised`); a wave's workers are numbered below it.
const REOPENED: u32 = 999;

/// A frame's RAISED run into `layer`: the edges raises added after its run
/// was written (`compact_raised`).
fn raised_path(dir: &Path, layer: u32, frame: u32) -> PathBuf {
    layer_dir(dir, layer).join(format!("f{:03}.raised.bin", frame))
}

/// Turn frame `frame`'s raised runs back into raw files (worker `REOPENED`,
/// with the frame's table, which `compact_raised` only appends to), so that
/// a frame keeps ONE raised run per layer however often it is raised.
pub fn reopen_raised(dir: &Path, frame: u32) -> Result<()> {
    use std::io::Write;
    ensure!((crate::frame::threads() as u32) < REOPENED, "{} workers: the reopened frame's worker number {REOPENED} is taken", crate::frame::threads());
    std::fs::create_dir_all(raw_dir(dir, frame))?;
    let table = xfer_path(dir, frame);
    if table.exists() {
        std::fs::copy(&table, raw_xfer_path(dir, frame, REOPENED)).with_context(|| table.display().to_string())?;
    }
    for layer in layers(dir) {
        let p = raised_path(dir, layer, frame);
        if !p.exists() {
            continue;
        }
        let run = Run::open(&p, frame)?;
        let mut w = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(raw_path(dir, frame, layer, REOPENED))?);
        let (mut words, mut buf): (Vec<Word>, Vec<u8>) = (Vec::with_capacity(CHUNK_WORDS), Vec::new());
        let mut err: Option<std::io::Error> = None;
        let mut flush = |words: &mut Vec<Word>, buf: &mut Vec<u8>| -> std::io::Result<()> {
            buf.clear();
            write_chunk(buf, words);
            words.clear();
            w.write_all(buf)
        };
        run.decode_from(0, |t, s, r| {
            let e = run.edge(t, s, r);
            push_records(&mut words, e.target, e.base, e.xfer, e.mask);
            if words.len() < CHUNK_WORDS {
                return true;
            }
            match flush(&mut words, &mut buf) {
                Ok(()) => true,
                Err(e) => {
                    err = Some(e);
                    false
                }
            }
        });
        if let Some(e) = err {
            return Err(e).with_context(|| format!("reopening {}", p.display()));
        }
        flush(&mut words, &mut buf).with_context(|| format!("reopening {}", p.display()))?;
        w.flush()?;
    }
    Ok(())
}

/// Drop what a run past `last` (the last frame whose records are complete)
/// left behind: every raw file, run and table of a later frame, and the
/// temporary files of an interrupted compaction. The raw records of frames
/// up to `last` stay: they are the graph until `invert`.
pub fn discard_after(dir: &Path, last: u32) -> Result<()> {
    for e in std::fs::read_dir(dir.join("raw")).into_iter().flatten().flatten() {
        let p = e.path();
        let frame: Option<u32> = p.file_name().and_then(|s| s.to_str()).and_then(|n| n.strip_prefix('f')).and_then(|s| s.parse().ok());
        if frame.is_none_or(|f| f > last) {
            std::fs::remove_dir_all(&p).with_context(|| p.display().to_string())?;
        }
    }
    for layer in layers(dir) {
        let ldir = layer_dir(dir, layer);
        for e in std::fs::read_dir(&ldir)?.flatten() {
            let p = e.path();
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            // `f{frame}.bin` or `f{frame}.raised.bin`.
            let run: Option<u32> = n.strip_prefix('f').and_then(|s| s.split('.').next()).and_then(|s| s.parse().ok());
            if n.starts_with("tmp-") || run.is_some_and(|f| f > last) {
                std::fs::remove_file(&p)?;
            }
        }
    }
    for e in std::fs::read_dir(dir.join("xfer")).into_iter().flatten().flatten() {
        let p = e.path();
        let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
        let table: Option<u32> = n.strip_prefix('f').and_then(|s| s.strip_suffix(".bin")).and_then(|s| s.parse().ok());
        if table.is_none_or(|f| f > last) {
            std::fs::remove_file(&p)?;
        }
    }
    Ok(())
}

/// Invert the raw records of every frame up to `horizon` into runs (sorted
/// by target), one frame at a time (each compaction uses every thread; its
/// sort buffer is bounded, `CHUNK_RECORDS`). Only complete frames (up to
/// `done.txt`) are inverted: a later frame's records are a killed wave's,
/// which the forward's resume discards. Resumable: a frame is inverted when
/// its raw dir is gone, and an interrupted compaction of it keeps the runs
/// it completed (`compact`).
pub fn invert(dir: &Path, horizon: u32) -> Result<()> {
    let last = done_frame(dir).map_or(horizon, |d| d.min(horizon));
    let frames: Vec<u32> = (1..=last).filter(|&f| raw_dir(dir, f).is_dir()).collect();
    if frames.is_empty() {
        return Ok(());
    }
    let t0 = std::time::Instant::now();
    let n = frames.len();
    let (mut records, mut edges, mut bytes) = (0u64, 0u64, 0u64);
    for f in frames {
        let c = compact_frame(dir, f).with_context(|| format!("inverting frame {f}'s edges in {}", dir.display()))?;
        records += c.records;
        edges += c.edges;
        bytes += c.bytes;
    }
    eprintln!(
        "[invert] {}: {n} frames, {records} records -> {edges} edges, {:.2} GB of runs, {:.1} s",
        dir.display(),
        bytes as f64 / 1e9,
        t0.elapsed().as_secs_f64()
    );
    Ok(())
}

/// A run, mapped: header, index, tables and stream all read in place (a
/// big tree has thousands of runs; their indexes stay page cache).
struct Run {
    map: memmap2::Mmap,
    /// Edges recorded at `frame` leave layer `frame - 1`.
    src_layer: u32,
    edges: u64,
    n_blocks: usize,
    seqs_at: usize,
    n_seqs: usize,
    xfers_at: usize,
    stream: usize,
}

impl Run {
    /// The run of the edges recorded at `frame` (`path`).
    fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        // SAFETY: runs are written once (renamed into place) and never
        // modified afterwards.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= RUN_HEADER && &map[0..4] == RUN_MAGIC, "{}: not a run", path.display());
        let version = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(version == RUN_VERSION, "{}: run version {version}, expected {RUN_VERSION}", path.display());
        ensure!(frame > 0, "{}: a run at frame 0", path.display());
        let word = |k: usize| u64::from_le_bytes(map[8 + 8 * k..16 + 8 * k].try_into().unwrap()) as usize;
        let (edges, n_blocks, n_seqs, n_xfers) = (word(0) as u64, word(1), word(2), word(3));
        let seqs_at = RUN_HEADER + n_blocks * 16;
        let xfers_at = seqs_at + n_seqs * 8;
        let stream = xfers_at + n_xfers * 4;
        ensure!(stream <= map.len(), "{}: truncated run", path.display());
        Ok(Run { map, src_layer: frame - 1, edges, n_blocks, seqs_at, n_seqs, xfers_at, stream })
    }

    fn u32_at(&self, o: usize) -> u32 {
        u32::from_le_bytes(self.map[o..o + 4].try_into().unwrap())
    }

    fn u64_at(&self, o: usize) -> u64 {
        u64::from_le_bytes(self.map[o..o + 8].try_into().unwrap())
    }

    /// Block `k`: its first target and its offset into the stream.
    fn block(&self, k: usize) -> (u64, usize) {
        let o = RUN_HEADER + 16 * k;
        (self.u64_at(o), self.u64_at(o + 8) as usize)
    }

    /// The source id of dense number `s` (`RunTables`).
    fn source(&self, s: u32) -> u64 {
        // The last piece starting at or before `s`.
        let (mut lo, mut hi) = (0usize, self.n_seqs);
        while lo < hi {
            let mid = (lo + hi) / 2;
            if self.u32_at(self.seqs_at + 8 * mid + 4) <= s {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        let o = self.seqs_at + 8 * (lo - 1);
        pack_id(self.src_layer, self.u32_at(o), s - self.u32_at(o + 4))
    }

    /// Decode from block `blk` on, `f(target, dense source, transfer rank)`
    /// per edge, until `f` returns false.
    fn decode_from(&self, mut blk: usize, mut f: impl FnMut(u64, u32, u32) -> bool) {
        if blk >= self.n_blocks {
            return;
        }
        let b = &self.map[self.stream..];
        let (mut prev_t, mut pos) = self.block(blk);
        let mut next_at = if blk + 1 < self.n_blocks { self.block(blk + 1).1 } else { usize::MAX };
        let (mut prev_s, mut fresh) = (0u32, true);
        while pos < b.len() {
            // A block boundary (blocks are `STRIDE` edges, except the last
            // of each encoded piece): the deltas restart.
            if pos >= next_at {
                blk += 1;
                prev_t = self.block(blk).0;
                next_at = if blk + 1 < self.n_blocks { self.block(blk + 1).1 } else { usize::MAX };
                fresh = true;
            }
            let head = get_varint(b, &mut pos);
            debug_assert!(!fresh || head & 1 == 1, "a block starts with a target");
            let (t, s) = if head & 1 == 1 { (prev_t + (head >> 1), get_varint(b, &mut pos) as u32) } else { (prev_t, prev_s + (head >> 1) as u32) };
            let r = get_varint(b, &mut pos) as u32;
            prev_t = t;
            prev_s = s;
            fresh = false;
            if !f(t, s, r) {
                return;
            }
        }
    }

    /// The edge (one lane) from dense source `s` with transfer rank `r`.
    fn edge(&self, target: u64, s: u32, r: u32) -> Edge {
        Edge { target, base: self.source(s), xfer: self.u32_at(self.xfers_at + 4 * r as usize), mask: 1 }
    }

    /// The edges into `target`, in (source, transfer rank) order.
    fn edges_of(&self, target: u64, out: &mut Vec<Edge>) {
        // They begin in the last block whose first target is < target (or
        // the next) and may span blocks.
        let (mut lo, mut hi) = (0usize, self.n_blocks);
        while lo < hi {
            let mid = (lo + hi) / 2;
            if self.block(mid).0 < target {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        self.decode_from(lo.saturating_sub(1), |t, s, r| {
            if t == target {
                out.push(self.edge(t, s, r));
            }
            t <= target
        });
    }
}

/// A level's graph up to a horizon: `runs[layer][frame]`, and per frame its
/// transfer table.
pub struct EdgeGraph {
    /// Per layer and frame: the run, then the raised run (`compact_raised`).
    runs: Vec<Vec<Vec<Run>>>,
    xfers: Vec<Vec<super::arc_edges::Pair>>,
    pub records: u64,
    pub bytes: u64,
}

impl EdgeGraph {
    /// Map every run `l{layer}/f{frame}.bin` (and `.raised.bin`) with `frame
    /// <= horizon`, the frames still raw inverted first (`invert`).
    pub fn open(dir: &Path, horizon: u32) -> Result<Self> {
        invert(dir, horizon)?;
        let mut runs: Vec<Vec<Vec<Run>>> = Vec::new();
        let (mut records, mut bytes) = (0u64, 0u64);
        for layer in layers(dir) {
            if layer > horizon {
                continue;
            }
            while runs.len() <= layer as usize {
                runs.push(Vec::new());
            }
            let v = &mut runs[layer as usize];
            v.resize_with(horizon as usize + 1, Vec::new);
            for frame in layer..=horizon {
                for p in [run_path(dir, layer, frame), raised_path(dir, layer, frame)] {
                    if p.exists() {
                        let r = Run::open(&p, frame)?;
                        records += r.edges;
                        bytes += r.map.len() as u64;
                        v[frame as usize].push(r);
                    }
                }
            }
        }
        let mut xfers = Vec::with_capacity(horizon as usize + 1);
        for frame in 0..=horizon {
            let p = xfer_path(dir, frame);
            let b = if p.exists() { std::fs::read(&p).with_context(|| p.display().to_string())? } else { Vec::new() };
            ensure!(b.len() % super::arc_edges::PAIR_BYTES == 0, "{}: truncated transfer table", p.display());
            xfers.push(b.chunks_exact(super::arc_edges::PAIR_BYTES).map(super::arc_edges::decode_pair).collect());
        }
        Ok(EdgeGraph { runs, xfers, records, bytes })
    }

    /// The transfer `xfer` of an edge recorded at frame `frame` (`None`: an
    /// id the frame's table does not have).
    pub fn pair(&self, frame: u32, xfer: u32) -> Option<super::arc_edges::Pair> {
        self.pairs(frame).get(xfer as usize).copied()
    }

    /// Frame `frame`'s transfer table (empty past the horizon).
    pub fn pairs(&self, frame: u32) -> &[super::arc_edges::Pair] {
        self.xfers.get(frame as usize).map_or(&[], |v| v.as_slice())
    }

    /// Every record of frame `frame`, a full scan (`rewrite arc-check`).
    pub fn records_at(&self, frame: u32) -> Vec<Edge> {
        let mut out = Vec::new();
        self.scan(frame, |e| out.push(e));
        out
    }

    /// Every edge of frame `frame`'s runs.
    fn scan(&self, frame: u32, mut f: impl FnMut(Edge)) {
        for runs in &self.runs {
            for run in runs.get(frame as usize).into_iter().flatten() {
                run.decode_from(0, |t, s, r| {
                    f(run.edge(t, s, r));
                    true
                });
            }
        }
    }

    /// Is there a run of the edges recorded at `frame` into layer `layer`?
    pub fn has_run(&self, layer: u32, frame: u32) -> bool {
        self.runs.get(layer as usize).and_then(|v| v.get(frame as usize)).is_some_and(|r| !r.is_empty())
    }

    /// The predecessors of `target` recorded at frame `frame`.
    pub fn preds_at(&self, target: u64, frame: u32, buf: &mut Vec<Edge>) {
        for run in self.runs.get(id_layer(target) as usize).and_then(|v| v.get(frame as usize)).into_iter().flatten() {
            run.edges_of(target, buf);
        }
    }
}

/// The marked set as bitmaps per `(layer, seq)` piece.
#[derive(Default)]
pub struct Marks {
    bits: rustc_hash::FxHashMap<(u32, u32), Vec<u64>>,
    /// Each marked id's DEADLINE: the last frame from which it still reaches
    /// a win by the horizon (the BFS iteration that marked it).
    deadlines: Vec<(u64, u16)>,
    pub count: usize,
}

impl Marks {
    /// True if newly marked; the first marking's `deadline` is the id's.
    fn insert(&mut self, id: u64, deadline: u32) -> bool {
        let (layer, seq, row) = (id_layer(id), crate::frame::id_seq(id), crate::frame::id_row(id));
        let v = self.bits.entry((layer, seq)).or_default();
        let (w, b) = ((row / 64) as usize, row % 64);
        if v.len() <= w {
            v.resize(w + 1, 0);
        }
        if v[w] & (1 << b) != 0 {
            return false;
        }
        v[w] |= 1 << b;
        self.count += 1;
        self.deadlines.push((id, u16::try_from(deadline).expect("a deadline past u16")));
        true
    }

    pub fn contains(&self, id: u64) -> bool {
        let (layer, seq, row) = (id_layer(id), crate::frame::id_seq(id), crate::frame::id_row(id));
        self.bits
            .get(&(layer, seq))
            .and_then(|v| v.get((row / 64) as usize))
            .is_some_and(|w| w & (1 << (row % 64)) != 0)
    }

    /// Mark `id` with `deadline` unless it is marked (the start, which the
    /// BFS never marks).
    pub fn insert_absent(&mut self, id: u64, deadline: u16) {
        self.insert(id, deadline as u32);
    }

    /// The marks as DENSE numbers - an id's rank among the marked ids in id
    /// order (`MarkRanks`) - and every marked id with its deadline, in that
    /// order. Consumes the marks: nothing is copied.
    pub fn into_ranked(self) -> (MarkRanks, Vec<(u64, u16)>) {
        let mut deadlines = self.deadlines;
        deadlines.sort_unstable();
        let mut bits = self.bits;
        let mut keys: Vec<(u32, u32)> = bits.keys().copied().collect();
        keys.sort_unstable();
        let mut pieces = rustc_hash::FxHashMap::default();
        let mut base = 0u64;
        for k in keys {
            let words = bits.remove(&k).expect("a key of the map");
            let mut before = Vec::with_capacity(words.len());
            for w in &words {
                before.push(u32::try_from(base).expect("more than u32::MAX marks"));
                base += w.count_ones() as u64;
            }
            pieces.insert(k, (words, before));
        }
        debug_assert_eq!(base as usize, deadlines.len());
        (MarkRanks { pieces }, deadlines)
    }
}

/// The marked ids' dense numbers (`Marks::into_ranked`): per `(layer, seq)`
/// piece its bitmap and, per word, the marks before it. ~1.5 bits a visited
/// row, against ~40 B a mark for a hash map.
pub struct MarkRanks {
    pieces: rustc_hash::FxHashMap<(u32, u32), (Vec<u64>, Vec<u32>)>,
}

impl MarkRanks {
    /// `id`'s rank among the marked ids (`None`: not marked).
    pub fn rank(&self, id: u64) -> Option<u32> {
        let (layer, seq, row) = (id_layer(id), crate::frame::id_seq(id), crate::frame::id_row(id));
        let (words, before) = self.pieces.get(&(layer, seq))?;
        let (w, b) = ((row / 64) as usize, row % 64);
        let word = *words.get(w)?;
        (word >> b & 1 == 1).then(|| before[w] + (word & ((1u64 << b) - 1)).count_ones())
    }
}

pub struct BfsStats {
    pub edges_read: u64,
    pub lookups: u64,
    pub t_open: std::time::Duration,
    pub t_bfs: std::time::Duration,
}

/// The remainder-free BFS backward from `seeds` (win rows): iteration i
/// (horizon-1 down to 1) marks a predecessor of an i+1-frontier state iff
/// its layer is <= i, reading the state's runs of frames layer..=i+1 once.
/// So iteration i marks exactly the states that win by the horizon from
/// frame i but not from i+1: i is the state's DEADLINE.
pub fn bfs(graph: &EdgeGraph, horizon: u32, seeds: impl IntoIterator<Item = u64>) -> (Marks, BfsStats) {
    let t = std::time::Instant::now();
    let mut marks = Marks::default();
    let mut frontier: Vec<u64> = Vec::new();
    for s in seeds {
        if id_layer(s) <= horizon && marks.insert(s, horizon) {
            frontier.push(s);
        }
    }
    let (mut edges_read, mut lookups) = (0u64, 0u64);
    let workers = crate::frame::threads().max(1);
    for i in (1..horizon).rev() {
        let t_it = std::time::Instant::now();
        frontier.sort_unstable();
        let last_frame = (i + 1).min(horizon);
        // Lookups in parallel (marks read-only), inserts sequential in
        // frontier order (deduplicating predecessors seen twice).
        let chunk = frontier.len().div_ceil(workers).max(1);
        let found: Vec<(Vec<u64>, u64, u64)> = std::thread::scope(|scope| {
            let hs: Vec<_> = frontier
                .chunks(chunk)
                .map(|part| {
                    let marks = &marks;
                    scope.spawn(move || {
                        let mut out: Vec<u64> = Vec::new();
                        let mut buf: Vec<Edge> = Vec::new();
                        let (mut lk, mut ed) = (0u64, 0u64);
                        for &tgt in part {
                            for frame in id_layer(tgt)..=last_frame {
                                if frame == 0 {
                                    continue;
                                }
                                buf.clear();
                                graph.preds_at(tgt, frame, &mut buf);
                                lk += 1;
                                ed += buf.len() as u64;
                                for e in &buf {
                                    let mut m = e.mask;
                                    while m != 0 {
                                        let lane = m.trailing_zeros();
                                        m &= m - 1;
                                        let p = e.base + lane as u64;
                                        debug_assert_eq!(id_layer(p), frame - 1);
                                        if !marks.contains(p) {
                                            out.push(p);
                                        }
                                    }
                                }
                            }
                        }
                        (out, lk, ed)
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("bfs worker panicked")).collect()
        });
        let mut next: Vec<u64> = Vec::new();
        for (out, lk, ed) in found {
            lookups += lk;
            edges_read += ed;
            for p in out {
                if marks.insert(p, i) {
                    next.push(p);
                }
            }
        }
        eprintln!(
            "[bfs] f{i:03} targets {} marked {} | {:.0} ms",
            frontier.len(),
            next.len(),
            t_it.elapsed().as_secs_f64() * 1e3
        );
        frontier = next;
    }
    let t_bfs = t.elapsed();
    (marks, BfsStats { edges_read, lookups, t_open: std::time::Duration::ZERO, t_bfs })
}

/// Resolve each `(id, deadline)` of `ids` (sorted) to its row's `(shape,
/// key, cell)` through the frame files `(layer, seq, file)`, calling `f(shape,
/// key, cell, deadline)` IN THE ORDER OF `ids`. Every id must resolve.
///
/// (`frame_paths` sorts a frame's files by name, shape hash first, not by
/// seq: walking them in that order called `f` out of id order wherever a
/// frame has two shapes, and the arc gate's W fingerprints, which index by
/// call order, depended on which seq the scheduling gave each shape.)
pub fn resolve_ids(
    files: &[(u32, u32, crate::search::checkpoint::FrameFile)],
    ids: &[(u64, u16)],
    mut f: impl FnMut(u64, (u64, u64), u32, u16),
) -> Result<()> {
    let mut order: Vec<&(u32, u32, crate::search::checkpoint::FrameFile)> = files.iter().collect();
    order.sort_by_key(|&&(layer, seq, _)| (layer, seq));
    let mut i = 0usize;
    for (layer, seq, file) in order {
        let lo = ids.partition_point(|&(id, _)| (id_layer(id), crate::frame::id_seq(id)) < (*layer, *seq));
        let hi = ids.partition_point(|&(id, _)| (id_layer(id), crate::frame::id_seq(id)) <= (*layer, *seq));
        if lo == hi {
            continue;
        }
        let cells = file.row_cells();
        for &(id, deadline) in &ids[lo..hi] {
            let row = crate::frame::id_row(id);
            ensure!(
                row < file.width(),
                "edges: marked id l{layer} s{seq} r{row} is past its file's {} rows",
                file.width()
            );
            f(file.shape_hash(), file.key_at(row), cells[row as usize], deadline);
            i += 1;
        }
    }
    ensure!(i == ids.len(), "edges: {} marked ids, {} resolved through the checkpoint files", ids.len(), i);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_worker(dir: &Path, frame: u32, layer: u32, worker: u32, edges: &[Edge]) {
        std::fs::create_dir_all(raw_dir(dir, frame)).unwrap();
        // Two chunks (the deltas and the predictor restart).
        let mut words = Vec::new();
        for e in edges {
            push_records(&mut words, e.target, e.base, e.xfer, e.mask);
        }
        let mut buf = Vec::new();
        let (a, b) = words.split_at(words.len() / 2);
        write_chunk(&mut buf, a);
        write_chunk(&mut buf, b);
        std::fs::write(raw_path(dir, frame, layer, worker), buf).unwrap();
        // The worker's transfer table: one pair, every record's id 0 (a test
        // with transfers of its own writes its table after this).
        let mut t = Vec::new();
        let a = crate::search::arc_edges::AxisXfer { lo: 0, hi: crate::search::arcs::CIRCLE, tag: 0, val: 0 };
        crate::search::arc_edges::encode_pair(&mut t, &(a, a));
        std::fs::write(raw_xfer_path(dir, frame, worker), t).unwrap();
    }

    /// A mark's deadline is the last frame it still reaches a win by the
    /// horizon from, through revisits of earlier-layer states too.
    #[test]
    fn a_marks_deadline_is_the_last_frame_it_still_wins_from() {
        let dir = std::env::temp_dir().join(format!("celeste-edges-deadline-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let (a, b, c, e, w, f, g) =
            (pack_id(0, 0, 0), pack_id(1, 0, 0), pack_id(2, 0, 0), pack_id(2, 0, 1), pack_id(3, 0, 0), pack_id(3, 0, 1), pack_id(3, 0, 2));
        let edge = |from: u64, to: u64| Edge { target: to, base: from, xfer: 0, mask: 1 };
        // a -> b -> c -> w (the win, layer 3); b -> e -> f; and two REVISITS
        // at frame 4: g -> w (in time: a win at 4) and f -> c (too late: c at
        // 4 wins at 5).
        let frames: [Vec<Edge>; 4] =
            [vec![edge(a, b)], vec![edge(b, c), edge(b, e)], vec![edge(c, w), edge(e, f)], vec![edge(f, c), edge(g, w)]];
        // A frame's records are filed by their TARGET's layer (`l{layer}`):
        // frame 4's revisits land in layers 2 and 3.
        for (i, es) in frames.iter().enumerate() {
            let frame = i as u32 + 1;
            for layer in 0..=frame {
                let part: Vec<Edge> = es.iter().filter(|e| id_layer(e.target) == layer).copied().collect();
                if !part.is_empty() {
                    write_worker(&dir, frame, layer, 0, &part);
                }
            }
            compact_frame(&dir, frame).unwrap();
        }
        let g4 = EdgeGraph::open(&dir, 4).unwrap();
        let (marks, _) = bfs(&g4, 4, [w]);
        let mut want = vec![(a, 1u16), (b, 2), (c, 3), (w, 4), (g, 3)];
        want.sort_unstable();
        let (ranks, got) = marks.into_ranked();
        assert_eq!(got, want, "e and f reach a win only after frame 4");
        for (k, &(id, _)) in got.iter().enumerate() {
            assert_eq!(ranks.rank(id), Some(k as u32), "a mark's rank is its place in id order");
        }
        assert_eq!(ranks.rank(e), None, "e is not marked");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// A target whose records straddle an index block boundary (and one
    /// spanning several blocks) is found whole.
    #[test]
    fn a_run_lookup_finds_records_across_index_blocks() {
        let dir = std::env::temp_dir().join(format!("celeste-edges-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let layer = 5;
        let mut edges: Vec<Edge> = Vec::new();
        // Targets 0..STRIDE-10 once each, then target STRIDE-10 (the last
        // one) 30 more times: its records occupy the end of block 0 and
        // the start of block 1. Then target 9000 for 3 * STRIDE records.
        for t in 0..(STRIDE as u64 - 10) {
            edges.push(Edge { target: pack_id(layer, 0, t as u32), base: pack_id(layer - 1, 0, 16 * t as u32), xfer: 0, mask: 1 });
        }
        let last = pack_id(layer, 0, STRIDE as u32 - 11);
        for k in 0..30u32 {
            edges.push(Edge { target: last, base: pack_id(layer - 1, 0, 100_000 + 16 * k), xfer: 0, mask: 3 });
        }
        let wide = pack_id(layer, 1, 9000);
        for k in 0..(3 * STRIDE as u32) {
            edges.push(Edge { target: wide, base: pack_id(layer - 1, 2, 16 * k), xfer: 0, mask: 0xffff });
        }
        // Written unsorted, across two workers.
        edges.reverse();
        let (a, b) = edges.split_at(edges.len() / 2);
        write_worker(&dir, layer, layer, 0, a);
        write_worker(&dir, layer, layer, 1, b);
        let st = compact_frame(&dir, layer).unwrap();
        let lanes = |es: &[Edge]| es.iter().map(|e| e.mask.count_ones() as usize).sum::<usize>();
        assert_eq!(st.records as usize, lanes(&edges), "a raw record per lane");
        assert_eq!(st.edges as usize, lanes(&edges), "an edge per lane");
        assert!(!raw_dir(&dir, layer).exists(), "the raw files are gone");
        let g = EdgeGraph::open(&dir, layer).unwrap();
        let mut buf = Vec::new();
        g.preds_at(last, layer, &mut buf);
        assert_eq!(buf.len(), 1 + 30 * 2, "the straddling target");
        buf.clear();
        g.preds_at(wide, layer, &mut buf);
        assert_eq!(buf.len(), 3 * STRIDE * 16, "the multi-block target");
        assert!(buf.windows(2).all(|w| w[0].base < w[1].base), "one edge per lane, by source");
        buf.clear();
        g.preds_at(pack_id(layer, 0, 0), layer, &mut buf);
        assert_eq!(buf.len(), 1);
        buf.clear();
        g.preds_at(pack_id(layer, 0, 7), layer, &mut buf);
        assert_eq!(buf, vec![Edge { target: pack_id(layer, 0, 7), base: pack_id(layer - 1, 0, 112), xfer: 0, mask: 1 }]);
        buf.clear();
        g.preds_at(pack_id(layer, 3, 0), layer, &mut buf);
        assert!(buf.is_empty(), "an absent target");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// A chunk decodes to the records written, in order: deltas both ways,
    /// seq changes, transfers the predictor has and has not, and two chunks
    /// back to back; a truncated file is refused.
    #[test]
    fn raw_chunks_round_trip() {
        let mut words: Vec<Word> = Vec::new();
        let mut v = 0x1234_5678_9abc_def0u64;
        for i in 0..5000u64 {
            v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            let (tseq, sseq) = ((v >> 60) as u32 * 300, (v >> 56 & 3) as u32);
            let target = pack_id(7, tseq, (v >> 8) as u32 & 0xff_ffff);
            let base = pack_id(6, sseq, (i as u32 * 7) ^ (v >> 40) as u32 & 0xfff);
            push_records(&mut words, target, base, (v >> 33) as u32 % 5 * 100_000, 1 << (v & 3));
        }
        let mut b = Vec::new();
        write_chunk(&mut b, &words[..1234]);
        write_chunk(&mut b, &words[1234..]);
        let mut got: Vec<(u64, u64, u32)> = Vec::new();
        let n = read_chunks(&b, Path::new("t"), |t, s, x| got.push((t, s, x))).unwrap();
        assert_eq!(n, words.len() as u64);
        assert_eq!(count_chunks(&b, Path::new("t")).unwrap(), n);
        assert_eq!(got, words.iter().map(|&w| word_fields(w)).collect::<Vec<_>>());
        assert!(read_chunks(&b[..b.len() - 1], Path::new("t"), |_, _, _| ()).is_err(), "a truncated file");
    }

    /// The graph is inverted when it is opened, only up to the last complete
    /// frame, and an inversion killed between a layer's run and the deletion
    /// of its raw files resumes into the same runs.
    #[test]
    fn an_interrupted_inversion_resumes_into_the_same_runs() {
        let root = std::env::temp_dir().join(format!("celeste-edges-invert-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        let (whole, cut) = (root.join("whole"), root.join("cut"));
        let e = |t: u64, s: u64| Edge { target: t, base: s, xfer: 0, mask: 1 };
        // Frame 2 into layers 1 and 2, two workers; frame 3 is not complete.
        let write = |dir: &Path| {
            write_worker(dir, 2, 1, 0, &[e(pack_id(1, 0, 0), pack_id(1, 0, 1)), e(pack_id(1, 0, 1), pack_id(1, 0, 0))]);
            write_worker(dir, 2, 1, 1, &[e(pack_id(1, 0, 1), pack_id(1, 0, 1))]);
            write_worker(dir, 2, 2, 0, &[e(pack_id(2, 0, 0), pack_id(1, 0, 0)), e(pack_id(2, 0, 3), pack_id(1, 0, 1))]);
            write_worker(dir, 3, 3, 0, &[e(pack_id(3, 0, 0), pack_id(2, 0, 0))]);
            set_done_frame(dir, 2).unwrap();
        };
        write(&whole);
        let g = EdgeGraph::open(&whole, 3).unwrap();
        assert!(!raw_dir(&whole, 2).exists(), "frame 2 inverted when opened");
        assert!(raw_dir(&whole, 3).exists() && !g.has_run(3, 3), "frame 3 is past done.txt: left alone");
        // The cut: layer 1's run is in place, one of its raw files gone.
        write(&cut);
        std::fs::create_dir_all(layer_dir(&cut, 1)).unwrap();
        std::fs::copy(run_path(&whole, 1, 2), run_path(&cut, 1, 2)).unwrap();
        std::fs::remove_file(raw_path(&cut, 2, 1, 0)).unwrap();
        invert(&cut, 3).unwrap();
        assert!(!raw_dir(&cut, 2).exists(), "the stale raw file of layer 1 is gone");
        for layer in [1, 2] {
            assert_eq!(std::fs::read(run_path(&cut, layer, 2)).unwrap(), std::fs::read(run_path(&whole, layer, 2)).unwrap(), "layer {layer}'s run");
        }
        assert_eq!(std::fs::read(xfer_path(&cut, 2)).unwrap(), std::fs::read(xfer_path(&whole, 2)).unwrap(), "the frame's transfer table");
        // A resume past frame 2 drops frame 3's raw records, keeps the rest.
        discard_after(&cut, 2).unwrap();
        assert!(!raw_dir(&cut, 3).exists());
        std::fs::remove_dir_all(&root).unwrap();
    }

    /// Two workers' tables intern the same transfers under different ids:
    /// the frame's table merges them, and a (target, source) pair keeps one
    /// edge per distinct transfer.
    #[test]
    fn transfers_merge_across_workers_and_split_masks() {
        use crate::search::arc_edges::{encode_pair, AxisXfer, PAIR_BYTES};
        let dir = std::env::temp_dir().join(format!("celeste-edges-xfer-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let a = AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: 0 };
        let b = AxisXfer { lo: 0, hi: 100, tag: 0, val: 7 };
        let (pa, pb) = ((a, a), (b, a));
        let table = |w: u32, pairs: &[(AxisXfer, AxisXfer)]| {
            let mut buf = Vec::new();
            for p in pairs {
                encode_pair(&mut buf, p);
            }
            assert_eq!(buf.len(), pairs.len() * PAIR_BYTES);
            std::fs::write(raw_xfer_path(&dir, 1, w), buf).unwrap();
        };
        let (t, base) = (pack_id(1, 0, 5), pack_id(0, 0, 0));
        // Worker 0: pa is its id 0, pb its id 1. Worker 1: the other way round.
        write_worker(&dir, 1, 1, 0, &[Edge { target: t, base, xfer: 0, mask: 1 }, Edge { target: t, base, xfer: 1, mask: 2 }]);
        table(0, &[pa, pb]);
        // Its last record repeats worker 0's first edge (pa, lane 0).
        write_worker(&dir, 1, 1, 1, &[Edge { target: t, base, xfer: 1, mask: 4 }, Edge { target: t, base, xfer: 0, mask: 8 }, Edge { target: t, base, xfer: 1, mask: 1 }]);
        table(1, &[pb, pa]);
        let st = compact_frame(&dir, 1).unwrap();
        assert_eq!((st.records, st.edges), (5, 4));
        assert!(!raw_dir(&dir, 1).exists(), "the raw files and tables are gone");
        let g = EdgeGraph::open(&dir, 1).unwrap();
        let mut buf = Vec::new();
        g.preds_at(t, 1, &mut buf);
        let mut got: Vec<_> = buf.iter().map(|e| (g.pair(1, e.xfer).unwrap(), e.base + e.mask.trailing_zeros() as u64)).collect();
        got.sort_unstable();
        let mut want = vec![(pa, base), (pa, base + 2), (pb, base + 1), (pb, base + 3)];
        want.sort_unstable();
        assert_eq!(got, want);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
