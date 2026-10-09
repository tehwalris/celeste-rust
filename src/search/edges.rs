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
//! target into the RUN `l{layer}/f{frame}.bin` (v5, `encode_groups`: ~4 B an
//! edge): every edge recorded at that frame into that layer, sources in layer
//! `frame - 1`. A frame is inverted once its raw dir is gone. A RAISE inverts
//! the tree first, then adds a frame's new edges beside its runs, in RAISED
//! runs `l{layer}/f{frame}.raised.bin` (`compact_raised`, transfers appended
//! to the frame's table). The backward (`bfs`) reads only runs, raised ones
//! included.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use crate::canon::Renumber;
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

/// The frame's renumbering of its own layer's ids (`canon::Renumber`),
/// which the targets of its records into that layer were written under.
pub fn renumber_path(dir: &Path, frame: u32) -> PathBuf {
    raw_dir(dir, frame).join("renumber.bin")
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

#[derive(Default)]
pub struct CompactStats {
    pub records: u64,
    pub edges: u64,
    pub bytes: u64,
    /// The layers compacted.
    pub layers: usize,
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
    /// An edge from `src` (seq << 32 | row) with transfer `xfer`.
    #[inline]
    fn add(&mut self, src: u64, xfer: u32) {
        let seq = (src >> 32) as usize;
        if self.rows.len() <= seq {
            self.rows.resize(seq + 1, 0);
        }
        self.rows[seq] = self.rows[seq].max(src as u32 + 1);
        let x = xfer as usize;
        if self.uses.len() <= x {
            self.uses.resize(x + 1, 0);
        }
        self.uses[x] += 1;
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

/// Encode a layer's edges, target by target in id order (`groups`: each
/// target and its edges as `dense source << 32 | transfer rank`, which are
/// sorted and deduplicated here), into blocks of `STRIDE` edges. Per edge,
/// varints: a HEAD, `delta << 1 | 1` at a block's or a target's first edge
/// (the target's delta, then the source, dense (`RunTables`), in full),
/// else `delta << 1` (the source's delta from the previous edge's, same
/// target); then the transfer's rank. Returns `(index (first target,
/// offset), stream, edges)`. (Room (6,2) gemskip nodiag f70: 4.3 B an edge,
/// 8.8 B a record in run v4; room (1,0) f44, 2.3 lanes a record: 3.1 B an
/// edge, 9.5 B a record.)
fn encode_groups<'a>(groups: impl Iterator<Item = (u64, &'a mut [u64])>, records: usize) -> (Vec<(u64, u64)>, Vec<u8>, u64) {
    let mut index: Vec<(u64, u64)> = Vec::with_capacity(records / STRIDE + 1);
    let mut out: Vec<u8> = Vec::with_capacity(records * 4);
    let (mut edges, mut in_block) = (0u64, 0usize);
    let (mut prev_t, mut prev_s) = (0u64, 0u32);
    for (t, group) in groups {
        group.sort_unstable();
        let mut last = u64::MAX;
        let mut k = 0usize;
        for &e in group.iter() {
            if e == last {
                continue;
            }
            last = e;
            let (s, r) = ((e >> 32) as u32, e as u32);
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
            k += 1;
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
/// The records of one layer of one frame, decoded from its raw files:
/// `f(target (seq << 32 | row), source (seq << 32 | row), the frame's
/// transfer id)`, the target through `ren` (the frame's renumbering, when
/// `files` are its own layer's). The one place a record's fields are
/// interpreted.
fn layer_records(files: &[(memmap2::Mmap, &Path, &[u32])], ren: Option<&Renumber>, mut f: impl FnMut(u64, u64, u32)) -> Result<u64> {
    let mut n = 0;
    for (b, path, remap) in files {
        n += match ren {
            Some(r) => read_chunks(b, path, |t, s, x| f(r.map_local(t), s, remap[x as usize]))?,
            None => read_chunks(b, path, |t, s, x| f(t, s, remap[x as usize]))?,
        };
    }
    Ok(n)
}

/// Per worker of an inversion: the buffers its layers reuse (a fresh one
/// faulted in per layer was a third of an inversion's time, in the kernel).
#[derive(Default)]
struct LayerBufs {
    /// Per target seq, the records into each row (then their slots).
    counts: Vec<Vec<u32>>,
    items: Vec<u64>,
}

/// One layer's raw files at `frame` into the run `out` (written to `tmp`,
/// renamed): two passes over the records - target counts per (seq, row)
/// and the run's tables, then a counting sort of the edges as encoded
/// (dense source, transfer rank) into place - and the encoder
/// over the whole layer (`remaps`: per worker, its transfer ids' in the
/// frame's table; `renumber`: the frame's, applied to its own layer). Single-threaded: an inversion runs many layers at
/// once (`compact_frames`). Returns (records, edges, run bytes).
fn invert_layer(files: &[RawFile], (remaps, renumber): &(rustc_hash::FxHashMap<u32, Vec<u32>>, Renumber), layer: u32, frame: u32, out: &Path, tmp: &Path, bufs: &mut LayerBufs) -> Result<(u64, u64, u64)> {
    let maps: Vec<(memmap2::Mmap, &Path, &[u32])> = files
        .iter()
        .map(|(f, w)| -> Result<_> {
            let file = std::fs::File::open(f).with_context(|| f.display().to_string())?;
            // SAFETY: the worker files are complete and never modified once
            // the frame's wave is over; they are deleted below.
            Ok((unsafe { memmap2::Mmap::map(&file)? }, f.as_path(), remaps.get(w).map_or(&[][..], |v| v.as_slice())))
        })
        .collect::<Result<_>>()?;
    let ren = (renumber.layer() == layer).then_some(renumber);
    let counts = &mut bufs.counts;
    for c in counts.iter_mut() {
        c.clear();
    }
    let mut tc = TableCounts::default();
    let n = layer_records(&maps, ren, |t, s, x| {
        let (seq, row) = ((t >> 32) as usize, t as u32 as usize);
        if counts.len() <= seq {
            counts.resize_with(seq + 1, Vec::new);
        }
        let c = &mut counts[seq];
        if c.len() <= row {
            c.resize(row + 1, 0);
        }
        c[row] += 1;
        tc.add(s, x);
    })? as usize;
    let tabs = RunTables::new(tc)?;
    // Each (seq, row)'s first slot, targets in id order.
    let mut acc = 0u32;
    for c in counts.iter_mut() {
        for v in c.iter_mut() {
            let k = *v;
            *v = acc;
            acc += k;
        }
    }
    debug_assert_eq!(acc as usize, n);
    // The edges as they are encoded, `dense source << 32 | transfer rank`
    // (8 B: the scatter's random writes are the inversion's main cost).
    let items = &mut bufs.items;
    items.clear();
    items.reserve(n);
    {
        let ptr = items.as_mut_ptr();
        let mut placed = 0usize;
        layer_records(&maps, ren, |t, s, x| {
            let slot = &mut counts[(t >> 32) as usize][t as u32 as usize];
            let e = ((tabs.seq_start[(s >> 32) as usize] + s as u32) as u64) << 32 | tabs.rank[x as usize] as u64;
            // SAFETY: the slots of the first pass's counts partition 0..n,
            // and this pass decodes the same records.
            unsafe { ptr.add(*slot as usize).write(e) };
            *slot += 1;
            placed += 1;
        })?;
        ensure!(placed == n, "layer {layer} of frame {frame}: {placed} records on the second pass, {n} on the first");
        // SAFETY: every slot below n was written once above.
        unsafe { items.set_len(n) };
    }
    drop(maps);
    // Each (seq, row)'s slot is now its group's end.
    let groups = counts.iter().enumerate().flat_map(|(seq, c)| c.iter().enumerate().map(move |(row, &end)| (pack_id(layer, seq as u32, row as u32), end as usize)));
    let mut rest: &mut [u64] = &mut items[..];
    let mut start = 0usize;
    let groups = groups.filter_map(|(t, end)| {
        if end == start {
            return None;
        }
        let (g, r) = std::mem::take(&mut rest).split_at_mut(end - start);
        rest = r;
        start = end;
        Some((t, g))
    });
    let (index, stream, edges) = encode_groups(groups, n);
    {
        use std::io::Write;
        let mut w = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(tmp)?);
        write_head(&mut w, edges, &index, &tabs)?;
        w.write_all(&stream)?;
        w.flush()?;
        // Its writeback started now: 49 GB of runs left dirty stalled a
        // whole inversion under the search's memory cap (page cache counts).
        use std::os::fd::AsRawFd;
        // SAFETY: a valid fd; SYNC_FILE_RANGE_WRITE only starts writeback.
        unsafe { libc::sync_file_range(w.get_ref().as_raw_fd(), 0, 0, libc::SYNC_FILE_RANGE_WRITE) };
    }
    std::fs::rename(tmp, out)?;
    for (f, _) in files {
        std::fs::remove_file(f)?;
    }
    Ok((n as u64, edges, head_bytes(&index, &tabs) + stream.len() as u64))
}

/// After frame `frame`: turn every layer's worker files into the run
/// `l{layer}/f{frame}.bin` and delete them (`compact_frames`).
pub fn compact_frame(dir: &Path, frame: u32) -> Result<CompactStats> {
    compact_frames(dir, &[frame], false)
}

/// A RAISE's records at frame `frame` (`frame::ForwardState::raise`: only
/// edges the frame's runs do not hold, and the frame's earlier raised run
/// reopened, `reopen_raised`) into the RAISED runs `l{layer}/f{frame}.raised.bin`
/// beside its runs; their new transfers are appended to the frame's table,
/// so the runs' ids keep their meaning. A frame's edges are its runs' and
/// its raised runs' (`EdgeGraph`).
pub fn compact_raised(dir: &Path, frame: u32) -> Result<CompactStats> {
    compact_frames(dir, &[frame], true)
}

/// Bytes of memory a layer's inversion holds per byte of its raw files
/// (~3.6 B a record: the sorted edges at 8 B, the run's stream, counts).
const MEM_PER_RAW_BYTE: u64 = 4;

/// The memory the layers in flight may hold together (`MEM_PER_RAW_BYTE`);
/// a layer alone may exceed it. Room (6,2) 100%'s largest layer is 454 MB
/// of raw records (~4 GB inverted).
const INVERT_MEM: u64 = 24 << 30;

/// Compact `frames`' raw records into their runs (`raised`: raised runs):
/// every (frame, layer) is one job (`invert_layer`), largest first, on
/// every hardware thread, the layers in flight bounded by `INVERT_MEM`.
/// A frame's raw dir goes after its last layer: a frame is inverted once
/// its raw dir is gone. Crash-safe: a run is renamed into place complete,
/// and a layer whose (non-raised) run exists is done - its leftover raw
/// files are stale and go.
fn compact_frames(dir: &Path, frames: &[u32], raised: bool) -> Result<CompactStats> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let mut st = CompactStats::default();
    // Every layer still to do as a job; a layer whose (non-raised) run is
    // in place is done, its leftover raw files stale.
    let mut jobs: Vec<(usize, u32, Vec<RawFile>, u64)> = Vec::new();
    for (i, &frame) in frames.iter().enumerate() {
        for (layer, files, bytes) in raw_files(dir, frame) {
            if !raised && run_path(dir, layer, frame).exists() {
                for (f, _) in &files {
                    std::fs::remove_file(f)?;
                }
                continue;
            }
            std::fs::create_dir_all(layer_dir(dir, layer))?;
            jobs.push((i, layer, files, bytes));
        }
    }
    jobs.sort_by_key(|j| std::cmp::Reverse(j.3));
    st.layers = jobs.len();
    let left: Vec<AtomicUsize> = (0..frames.len()).map(|i| AtomicUsize::new(jobs.iter().filter(|j| j.0 == i).count())).collect();
    // Per frame with layers to do: its transfer remaps (the workers' tables
    // merged, in parallel: a tree's are seconds of work) and its renumbering
    // (`canon::Renumber`: the targets in its own layer were written as flush
    // ids; a frame's raw records are refused without it). A frame with none
    // left (an inversion killed after its last layer) only loses its leftovers:
    // merging its remaining tables again would overwrite the frame's.
    type FrameMaps = Option<(rustc_hash::FxHashMap<u32, Vec<u32>>, Renumber)>;
    let todo: Vec<usize> = (0..frames.len()).collect();
    let maps: Vec<FrameMaps> = std::thread::scope(|scope| {
        let chunk = todo.len().div_ceil(std::thread::available_parallelism().map_or(2, |n| n.get())).max(1);
        let hs: Vec<_> = todo
            .chunks(chunk)
            .map(|part| {
                let left = &left;
                scope.spawn(move || -> Result<Vec<FrameMaps>> {
                    part.iter()
                        .map(|&i| {
                            if left[i].load(Ordering::Relaxed) == 0 {
                                return Ok(None);
                            }
                            let remaps = merge_xfer_tables(dir, frames[i], raised)?;
                            Ok(Some((remaps, Renumber::load(&renumber_path(dir, frames[i]))?)))
                        })
                        .collect()
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("transfer table merge panicked")).collect::<Result<Vec<_>>>()
    })?
    .into_iter()
    .flatten()
    .collect();
    // A frame's tables and renumbering go after its last layer, then (last)
    // its raw dir: a frame is inverted once its raw dir is gone (`invert`).
    let finish_frame = |i: usize| -> Result<()> {
        let raw = raw_dir(dir, frames[i]);
        for e in std::fs::read_dir(&raw).into_iter().flatten().flatten() {
            let p = e.path();
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if n.starts_with("x_w") || n == "renumber.bin" {
                std::fs::remove_file(&p)?;
            }
        }
        if raw.exists() {
            std::fs::remove_dir(&raw).with_context(|| format!("{}: left over after the compaction", raw.display()))?;
        }
        Ok(())
    };
    for (i, l) in left.iter().enumerate() {
        if l.load(Ordering::Relaxed) == 0 {
            finish_frame(i)?;
        }
    }
    let next = AtomicUsize::new(0);
    let budget = (std::sync::Mutex::new(0u64), std::sync::Condvar::new());
    let workers = std::thread::available_parallelism().map_or(2, |n| n.get()).min(jobs.len());
    let totals: Vec<(u64, u64, u64)> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..workers)
            .map(|_| {
                let (jobs, next, budget, left, maps, finish_frame) = (&jobs, &next, &budget, &left, &maps, &finish_frame);
                scope.spawn(move || -> Result<(u64, u64, u64)> {
                    let mut bufs = LayerBufs::default();
                    let mut tot = (0u64, 0u64, 0u64);
                    while let Some((i, layer, files, bytes)) = jobs.get(next.fetch_add(1, Ordering::Relaxed)) {
                        let (i, layer, frame) = (*i, *layer, frames[*i]);
                        let need = bytes * MEM_PER_RAW_BYTE;
                        {
                            let mut held = budget.0.lock().expect("budget");
                            while *held > 0 && *held + need > INVERT_MEM {
                                held = budget.1.wait(held).expect("budget");
                            }
                            *held += need;
                        }
                        let out = if raised { raised_path(dir, layer, frame) } else { run_path(dir, layer, frame) };
                        let tmp = layer_dir(dir, layer).join(format!("tmp-f{:03}.bin", frame));
                        let r = invert_layer(files, maps[i].as_ref().expect("a frame with layers to do has its maps"), layer, frame, &out, &tmp, &mut bufs)
                            .with_context(|| format!("inverting layer {layer} of frame {frame} in {}", dir.display()));
                        // Big buffers go back after a big layer.
                        if need > INVERT_MEM / 8 {
                            bufs = LayerBufs::default();
                        }
                        *budget.0.lock().expect("budget") -= need;
                        budget.1.notify_all();
                        let (records, edges, b) = r?;
                        tot = (tot.0 + records, tot.1 + edges, tot.2 + b);
                        if left[i].fetch_sub(1, Ordering::AcqRel) == 1 {
                            finish_frame(i)?;
                        }
                    }
                    Ok(tot)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("inversion worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    for (r, e, b) in totals {
        st.records += r;
        st.edges += e;
        st.bytes += b;
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
/// by target), all at once (`compact_frames`). Only complete frames (up to
/// `done.txt`) are inverted: a later frame's records are a killed wave's,
/// which the forward's resume discards. Resumable: a frame is inverted when
/// its raw dir is gone, and an interrupted inversion keeps the runs it
/// completed. Returns the totals (`layers`: the layers inverted).
pub fn invert(dir: &Path, horizon: u32) -> Result<CompactStats> {
    let last = done_frame(dir).map_or(horizon, |d| d.min(horizon));
    let frames: Vec<u32> = (1..=last).filter(|&f| raw_dir(dir, f).is_dir()).collect();
    if frames.is_empty() {
        return Ok(CompactStats::default());
    }
    let t0 = std::time::Instant::now();
    let st = compact_frames(dir, &frames, false)?;
    eprintln!(
        "[invert] {}: {} frames, {} layers, {} records -> {} edges, {:.2} GB of runs, {:.1} s",
        dir.display(),
        frames.len(),
        st.layers,
        st.records,
        st.edges,
        st.bytes as f64 / 1e9,
        t0.elapsed().as_secs_f64()
    );
    Ok(st)
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
        // Targets already canonical (a test renumbering writes its own).
        if !renumber_path(dir, frame).exists() {
            Renumber::identity(frame).save(&renumber_path(dir, frame)).unwrap();
        }
    }

    /// The frame's records into its own layer carry flush ids: the run holds
    /// them renumbered (`canon::Renumber`), sorted by the canonical target;
    /// records into older layers keep theirs.
    #[test]
    fn a_frames_own_targets_are_renumbered_at_compaction() {
        let dir = std::env::temp_dir().join(format!("celeste-edges-renumber-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let frame = 3;
        // Flush piece 2 of layer 3, rows 0..4, are canonically (5, 3 - r).
        let to: Vec<u64> = (0..4).map(|r| pack_id(frame, 5, 3 - r)).collect();
        std::fs::create_dir_all(raw_dir(&dir, frame)).unwrap();
        Renumber::of_piece(frame, 2, to.clone()).save(&renumber_path(&dir, frame)).unwrap();
        let src = |r: u32| pack_id(frame - 1, 0, r);
        let own: Vec<Edge> = (0..4).map(|r| Edge { target: pack_id(frame, 2, r), base: src(r), xfer: 0, mask: 1 }).collect();
        write_worker(&dir, frame, frame, 0, &own);
        let old = Edge { target: pack_id(1, 2, 1), base: src(9), xfer: 0, mask: 1 };
        write_worker(&dir, frame, 1, 0, &[old]);
        compact_frame(&dir, frame).unwrap();
        assert!(!raw_dir(&dir, frame).exists(), "the renumbering goes with the raw files");
        let g = EdgeGraph::open(&dir, frame).unwrap();
        let mut buf = Vec::new();
        for r in 0..4u32 {
            buf.clear();
            g.preds_at(to[r as usize], frame, &mut buf);
            assert_eq!(buf, vec![Edge { target: to[r as usize], base: src(r), xfer: 0, mask: 1 }], "flush row {r}");
        }
        buf.clear();
        g.preds_at(pack_id(frame, 2, 0), frame, &mut buf);
        assert!(buf.is_empty(), "no edge under a flush id");
        buf.clear();
        g.preds_at(old.target, frame, &mut buf);
        assert_eq!(buf, vec![old], "an older layer's target is not renumbered");
        std::fs::remove_dir_all(&dir).unwrap();
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
