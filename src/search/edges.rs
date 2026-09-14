//! The explicit backward graph (plans/waves.md, "the explicit graph").
//!
//! The forward records, at every flush, one record per (emitted state,
//! slice that produced it): the state's id (`frame::pack_id`), the id of
//! the slice's first input row and a 16-bit mask of the lanes that
//! produced the state - `crate::frame::ForwardSink`. The records land in
//! `<level>/edges/l{target layer}/w{worker}.bin`, and at the end of the
//! frame `compact_frame` sorts each layer's records by target into the RUN
//! `l{layer}/f{frame}.bin`. A run holds, for the frame it was recorded at,
//! every edge into that layer's states; the edges' sources are rows of
//! layer `frame - 1` (the frame's input frontier).
//!
//! The backward is then a BFS over these files (`backward`): no kernel is
//! re-run. Its marked set reproduces the kernel backward's exactly (the
//! eligibility rule is the same: at iteration i, a predecessor is marked
//! iff its layer is <= i), which is what `gates/marks_*` pins.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use crate::frame::{id_layer, pack_id, Visited};

/// One edge record in a worker file: the target's (seq, row) - its layer
/// is the file's - the base's (seq, row) - its layer is the frame's input
/// layer - and the 64-lane mask.
pub const RECORD_BYTES: usize = 20;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    pub target: u64,
    pub base: u64,
    pub mask: u64,
}

/// Append one record to a worker buffer.
#[inline]
pub fn encode_record(out: &mut Vec<u8>, target: u64, base: u64, mask: u64) {
    out.extend_from_slice(&(crate::frame::id_seq(target) as u16).to_le_bytes());
    out.extend_from_slice(&crate::frame::id_row(target).to_le_bytes());
    out.extend_from_slice(&(crate::frame::id_seq(base) as u16).to_le_bytes());
    out.extend_from_slice(&crate::frame::id_row(base).to_le_bytes());
    out.extend_from_slice(&mask.to_le_bytes());
}

/// Decode a record of a file for layer `layer`, recorded at frame
/// `frame` (so the base is in layer `frame - 1`).
#[inline]
fn decode(b: &[u8], layer: u32, frame: u32) -> Edge {
    let tseq = u16::from_le_bytes(b[0..2].try_into().unwrap()) as u32;
    let trow = u32::from_le_bytes(b[2..6].try_into().unwrap());
    let bseq = u16::from_le_bytes(b[6..8].try_into().unwrap()) as u32;
    let brow = u32::from_le_bytes(b[8..12].try_into().unwrap());
    Edge {
        target: pack_id(layer, tseq, trow),
        base: pack_id(frame - 1, bseq, brow),
        mask: u64::from_le_bytes(b[12..20].try_into().unwrap()),
    }
}

/// The target (seq, row) of record `i` as a sortable 48-bit value.
#[inline]
fn target_local(b: &[u8], i: usize) -> u64 {
    let o = i * RECORD_BYTES;
    ((u16::from_le_bytes(b[o..o + 2].try_into().unwrap()) as u64) << 32)
        | u32::from_le_bytes(b[o + 2..o + 6].try_into().unwrap()) as u64
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

/// The raw files of frame `frame` grouped by target layer, with their
/// total bytes.
fn raw_files(dir: &Path, frame: u32) -> Vec<(u32, Vec<PathBuf>, u64)> {
    let mut by_layer: std::collections::BTreeMap<u32, (Vec<PathBuf>, u64)> = Default::default();
    for e in std::fs::read_dir(raw_dir(dir, frame)).into_iter().flatten().flatten() {
        let p = e.path();
        let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
        let Some(layer) = n.strip_prefix('l').and_then(|s| s.get(..3)).and_then(|s| s.parse::<u32>().ok()) else { continue };
        if !n.ends_with(".bin") {
            continue;
        }
        let bytes = std::fs::metadata(&p).map(|m| m.len()).unwrap_or(0);
        let e = by_layer.entry(layer).or_default();
        e.0.push(p);
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

/// The last frame whose runs are complete (`<dir>/done.txt`, written as
/// 0 when a forward starts). `None` if there is no marker at all: a tree
/// written before the marker existed, whose frames are all trusted.
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
    pub pairs: u64,
    pub bytes: u64,
    pub layers: usize,
    /// Wall time in the three phases: read + bucket, sort + encode, write.
    pub t_read: std::time::Duration,
    pub t_sort: std::time::Duration,
    pub t_write: std::time::Duration,
}

/// Records per index block of a run.
const STRIDE: usize = 256;
const RUN_MAGIC: &[u8; 4] = b"CERN";
const RUN_VERSION: u32 = 2;

#[inline]
fn put_varint(out: &mut Vec<u8>, mut v: u64) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
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

/// A 64-lane mask: its popcount then the lane indices when there are at
/// most 4, else 0xff and the raw mask.
#[inline]
fn put_mask(out: &mut Vec<u8>, m: u64) {
    let n = m.count_ones();
    if n <= 4 {
        out.push(n as u8);
        let mut x = m;
        while x != 0 {
            out.push(x.trailing_zeros() as u8);
            x &= x - 1;
        }
    } else {
        out.push(0xff);
        out.extend_from_slice(&m.to_le_bytes());
    }
}

#[inline]
fn get_mask(b: &[u8], pos: &mut usize) -> u64 {
    let n = b[*pos];
    *pos += 1;
    if n == 0xff {
        let m = u64::from_le_bytes(b[*pos..*pos + 8].try_into().unwrap());
        *pos += 8;
        return m;
    }
    let mut m = 0u64;
    for _ in 0..n {
        m |= 1u64 << b[*pos];
        *pos += 1;
    }
    m
}

#[inline]
fn zigzag(v: i64) -> u64 {
    ((v << 1) ^ (v >> 63)) as u64
}

#[inline]
fn unzigzag(v: u64) -> i64 {
    ((v >> 1) as i64) ^ -((v & 1) as i64)
}

/// Encode a slice of records already sorted by (target, base): equal
/// pairs merged, delta-varint blocks of `STRIDE` pairs. Returns `(index
/// entries (first target, offset within this stream), stream, pairs)`.
fn encode_sorted(recs: &[(u64, u64, u64)]) -> (Vec<(u64, u64)>, Vec<u8>, u64) {
    let mut index: Vec<(u64, u64)> = Vec::with_capacity(recs.len() / STRIDE + 1);
    let mut out: Vec<u8> = Vec::with_capacity(recs.len() * 5);
    let (mut pairs, mut in_block) = (0u64, 0usize);
    let (mut prev_t, mut prev_b) = (0u64, 0u64);
    let mut i = 0;
    while i < recs.len() {
        let (t, b, mut m) = recs[i];
        i += 1;
        while i < recs.len() && recs[i].0 == t && recs[i].1 == b {
            m |= recs[i].2;
            i += 1;
        }
        if in_block == 0 {
            index.push((t, out.len() as u64));
            prev_t = t;
            prev_b = 0;
        }
        put_varint(&mut out, t - prev_t);
        put_varint(&mut out, zigzag(b as i64 - prev_b as i64));
        put_mask(&mut out, m);
        prev_t = t;
        prev_b = b;
        pairs += 1;
        in_block += 1;
        if in_block == STRIDE {
            in_block = 0;
        }
    }
    (index, out, pairs)
}

/// One layer's records (all with targets in that layer) sorted by
/// (target, base): a parallel counting sort by the target's dense rank
/// (piece offset + row; the ranks come from the records themselves),
/// then each target's group sorted by base. O(n) passes, no comparison
/// sort over the whole layer. `files` are the raw worker files' contents.
fn sort_layer(files: &[Vec<u8>], layer: u32, frame: u32) -> Vec<(u64, u64, u64)> {
    use std::sync::atomic::{AtomicU32, Ordering};
    let n_total: usize = files.iter().map(|b| b.len() / RECORD_BYTES).sum();
    // The layer's pieces: max row per seq, from a parallel scan.
    let per_file_max: Vec<Vec<u32>> = std::thread::scope(|scope| {
        let hs: Vec<_> = files
            .iter()
            .map(|b| {
                scope.spawn(move || {
                    let mut max_row: Vec<u32> = Vec::new();
                    for i in 0..b.len() / RECORD_BYTES {
                        let t = target_local(b, i);
                        let (seq, row) = ((t >> 32) as usize, t as u32);
                        if max_row.len() <= seq {
                            max_row.resize(seq + 1, 0);
                        }
                        max_row[seq] = max_row[seq].max(row + 1);
                    }
                    max_row
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("scan")).collect()
    });
    let n_seq = per_file_max.iter().map(|m| m.len()).max().unwrap_or(0);
    let mut piece_off: Vec<usize> = vec![0; n_seq + 1];
    for seq in 0..n_seq {
        let w = per_file_max.iter().map(|m| m.get(seq).copied().unwrap_or(0)).max().unwrap_or(0) as usize;
        piece_off[seq + 1] = piece_off[seq] + w;
    }
    let n_ranks = piece_off[n_seq];
    let rank_of = |t: u64| piece_off[(t >> 32) as usize] + t as u32 as usize;
    // One shared histogram over ranks (atomic adds from every file), its
    // prefix sums, then the scatter claims positions with an atomic
    // increment per record - files in parallel, disjoint positions.
    let hist: Vec<AtomicU32> = (0..n_ranks).map(|_| AtomicU32::new(0)).collect();
    std::thread::scope(|scope| {
        for b in files {
            let (hist, rank_of) = (&hist, &rank_of);
            scope.spawn(move || {
                for i in 0..b.len() / RECORD_BYTES {
                    hist[rank_of(target_local(b, i))].fetch_add(1, Ordering::Relaxed);
                }
            });
        }
    });
    let mut acc = 0u32;
    for h in &hist {
        let c = h.load(Ordering::Relaxed);
        h.store(acc, Ordering::Relaxed);
        acc += c;
    }
    debug_assert_eq!(acc as usize, n_total);
    let next = hist;
    let mut out: Vec<(u64, u64, u64)> = Vec::with_capacity(n_total);
    // SAFETY: every position 0..n_total is written exactly once below
    // (the claims partition 0..n_total); the element type has no drop
    // glue.
    #[allow(clippy::uninit_vec)]
    unsafe {
        out.set_len(n_total);
    }
    {
        let out_ptr = out.as_mut_ptr() as usize;
        std::thread::scope(|scope| {
            for b in files {
                let (next, rank_of) = (&next, &rank_of);
                scope.spawn(move || {
                    let out_ptr = out_ptr as *mut (u64, u64, u64);
                    for i in 0..b.len() / RECORD_BYTES {
                        let e = decode(&b[i * RECORD_BYTES..(i + 1) * RECORD_BYTES], layer, frame);
                        let pos = next[rank_of(target_local(b, i))].fetch_add(1, Ordering::Relaxed) as usize;
                        // SAFETY: a claimed position, unique and < n_total.
                        unsafe { out_ptr.add(pos).write((e.target, e.base, e.mask)) };
                    }
                });
            }
        });
    }
    // Each target's group by base, ranges of the output in parallel
    // (groups never straddle a chunk boundary: chunks are cut at target
    // changes).
    let cuts = cuts_at_target_changes(&out, crate::frame::threads().max(1));
    {
        let mut rest: &mut [(u64, u64, u64)] = &mut out;
        let mut pieces: Vec<&mut [(u64, u64, u64)]> = Vec::new();
        for w in cuts.windows(2) {
            let (a, b) = rest.split_at_mut(w[1] - w[0]);
            pieces.push(a);
            rest = b;
        }
        std::thread::scope(|scope| {
            for p in pieces {
                scope.spawn(move || sort_groups_by_base(p));
            }
        });
    }
    out
}

/// Boundaries of `chunks` ranges over `recs` (sorted by target), moved
/// forward so no target's group straddles one.
fn cuts_at_target_changes(recs: &[(u64, u64, u64)], chunks: usize) -> Vec<usize> {
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

/// Within each run of equal targets, sort by base.
fn sort_groups_by_base(p: &mut [(u64, u64, u64)]) {
    let mut i = 0;
    while i < p.len() {
        let mut j = i + 1;
        while j < p.len() && p[j].0 == p[i].0 {
            j += 1;
        }
        p[i..j].sort_unstable_by_key(|r| r.1);
        i = j;
    }
}

/// Encode a layer's sorted records into a run file (`tmp` then renamed
/// to `path`); `encode_chunks` parallel ranges. Returns (pairs, bytes).
fn write_run(sorted: &[(u64, u64, u64)], encode_chunks: usize, path: &Path, tmp: &Path) -> Result<(u64, u64)> {
    let cuts = cuts_at_target_changes(sorted, encode_chunks);
    let encoded: Vec<(Vec<(u64, u64)>, Vec<u8>, u64)> = if encode_chunks <= 1 {
        vec![encode_sorted(sorted)]
    } else {
        std::thread::scope(|scope| {
            let hs: Vec<_> = cuts
                .windows(2)
                .map(|w| {
                    let sl = &sorted[w[0]..w[1]];
                    scope.spawn(move || encode_sorted(sl))
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("edge encoder panicked")).collect()
        })
    };
    let n_blocks: usize = encoded.iter().map(|(ix, _, _)| ix.len()).sum();
    let pairs: u64 = encoded.iter().map(|(_, _, p)| *p).sum();
    let bytes;
    {
        use std::io::Write;
        let mut w = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(tmp)?);
        w.write_all(RUN_MAGIC)?;
        w.write_all(&RUN_VERSION.to_le_bytes())?;
        w.write_all(&pairs.to_le_bytes())?;
        w.write_all(&(n_blocks as u64).to_le_bytes())?;
        let mut off = 0u64;
        for (ix, s, _) in &encoded {
            for &(t, o) in ix {
                w.write_all(&t.to_le_bytes())?;
                w.write_all(&(o + off).to_le_bytes())?;
            }
            off += s.len() as u64;
        }
        for (_, s, _) in &encoded {
            w.write_all(s)?;
        }
        w.flush()?;
        bytes = 24 + n_blocks as u64 * 16 + off;
    }
    std::fs::rename(tmp, path)?;
    Ok((pairs, bytes))
}

/// Records above which a layer gets the parallel counting sort; smaller
/// layers are comparison-sorted whole, several layers at a time (a
/// single-threaded sort of 4M records was the compaction's critical path
/// at 4M; at 512k it is ~40 ms).
const BIG_LAYER: usize = 1 << 19;

/// After frame `frame`: turn every layer's worker files into the run
/// `l{layer}/f{frame}.bin` - the records sorted by (target, base), equal
/// pairs merged, delta-varint encoded in blocks of `STRIDE` with an index
/// of (first target, offset) per block - and delete them. Big layers one
/// at a time, each with every thread (`sort_layer`); small layers in
/// parallel, one thread each.
pub fn compact_frame(dir: &Path, frame: u32) -> Result<CompactStats> {
    let mut layers: Vec<(u32, Vec<PathBuf>, u64)> = raw_files(dir, frame);
    layers.sort_by_key(|&(_, _, bytes)| std::cmp::Reverse(bytes));
    let mut st = CompactStats {
        records: 0,
        pairs: 0,
        bytes: 0,
        layers: layers.len(),
        t_read: std::time::Duration::ZERO,
        t_sort: std::time::Duration::ZERO,
        t_write: std::time::Duration::ZERO,
    };
    let read_all = |files: &[PathBuf]| -> Result<Vec<Vec<u8>>> {
        files
            .iter()
            .map(|f| {
                let b = std::fs::read(f).with_context(|| f.display().to_string())?;
                ensure!(b.len() % RECORD_BYTES == 0, "{}: truncated edge file", f.display());
                Ok(b)
            })
            .collect()
    };
    let (big, small): (Vec<_>, Vec<_>) = layers.into_iter().partition(|(_, _, bytes)| *bytes as usize / RECORD_BYTES >= BIG_LAYER);
    for (layer, files, bytes) in &big {
        let t0 = std::time::Instant::now();
        let contents: Vec<Vec<u8>> = std::thread::scope(|scope| {
            let hs: Vec<_> = files
                .iter()
                .map(|f| {
                    scope.spawn(move || -> Result<Vec<u8>> {
                        let b = std::fs::read(f).with_context(|| f.display().to_string())?;
                        ensure!(b.len() % RECORD_BYTES == 0, "{}: truncated edge file", f.display());
                        Ok(b)
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("edge reader panicked")).collect::<Result<Vec<_>>>()
        })?;
        st.records += *bytes / RECORD_BYTES as u64;
        st.t_read += t0.elapsed();
        let t0 = std::time::Instant::now();
        let sorted = sort_layer(&contents, *layer, frame);
        drop(contents);
        st.t_sort += t0.elapsed();
        let t0 = std::time::Instant::now();
        let ldir = layer_dir(dir, *layer);
        std::fs::create_dir_all(&ldir)?;
        let (pairs, bytes) = write_run(&sorted, crate::frame::threads() * 2, &run_path(dir, *layer, frame), &ldir.join(format!("tmp-f{:03}.bin", frame)))?;
        drop(sorted);
        for f in files {
            std::fs::remove_file(f)?;
        }
        st.pairs += pairs;
        st.bytes += bytes;
        st.t_write += t0.elapsed();
    }
    // The small layers: a pool of threads, a layer each.
    let t0 = std::time::Instant::now();
    let next = std::sync::atomic::AtomicUsize::new(0);
    let totals: Vec<(u64, u64, u64)> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..crate::frame::threads().min(small.len().max(1)))
            .map(|_| {
                let (small, next, read_all) = (&small, &next, &read_all);
                scope.spawn(move || -> Result<(u64, u64, u64)> {
                    let (mut records, mut pairs, mut bytes) = (0u64, 0u64, 0u64);
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some((layer, files, fbytes)) = small.get(i) else { break };
                        let contents = read_all(files)?;
                        let mut recs: Vec<(u64, u64, u64)> = Vec::with_capacity(*fbytes as usize / RECORD_BYTES);
                        for b in &contents {
                            for i in 0..b.len() / RECORD_BYTES {
                                let e = decode(&b[i * RECORD_BYTES..(i + 1) * RECORD_BYTES], *layer, frame);
                                recs.push((e.target, e.base, e.mask));
                            }
                        }
                        drop(contents);
                        recs.sort_unstable_by_key(|r| (r.0, r.1));
                        let ldir = layer_dir(dir, *layer);
                        std::fs::create_dir_all(&ldir)?;
                        let (p, b) = write_run(&recs, 1, &run_path(dir, *layer, frame), &ldir.join(format!("tmp-f{:03}.bin", frame)))?;
                        for f in files {
                            std::fs::remove_file(f)?;
                        }
                        records += recs.len() as u64;
                        pairs += p;
                        bytes += b;
                    }
                    Ok((records, pairs, bytes))
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("edge compaction worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    for (r, p, b) in totals {
        st.records += r;
        st.pairs += p;
        st.bytes += b;
    }
    st.t_sort += t0.elapsed();
    let _ = std::fs::remove_dir(raw_dir(dir, frame));
    Ok(st)
}

/// Drop what a run past `last` (the last frame whose runs are complete)
/// left behind: every raw file and every run of a later frame.
pub fn discard_after(dir: &Path, last: u32) -> Result<()> {
    if dir.join("raw").is_dir() {
        std::fs::remove_dir_all(dir.join("raw"))?;
    }
    for layer in layers(dir) {
        let ldir = layer_dir(dir, layer);
        for e in std::fs::read_dir(&ldir)?.flatten() {
            let p = e.path();
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            let run: Option<u32> = n.strip_prefix('f').and_then(|s| s.strip_suffix(".bin")).and_then(|s| s.parse().ok());
            if n.starts_with("tmp-") || run.is_some_and(|f| f > last) {
                std::fs::remove_file(&p)?;
            }
        }
    }
    Ok(())
}

/// A run, mapped: the index (first target, stream offset per block) in
/// memory, the delta-varint stream on disk.
struct Run {
    map: memmap2::Mmap,
    index: Vec<(u64, u64)>,
    stream: usize,
    pairs: u64,
}

impl Run {
    fn open(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        // SAFETY: runs are written once (renamed into place) and never
        // modified afterwards.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 24 && &map[0..4] == RUN_MAGIC, "{}: not a run", path.display());
        let version = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(version == RUN_VERSION, "{}: run version {version}, expected {RUN_VERSION}", path.display());
        let pairs = u64::from_le_bytes(map[8..16].try_into().unwrap());
        let n_blocks = u64::from_le_bytes(map[16..24].try_into().unwrap()) as usize;
        let mut index = Vec::with_capacity(n_blocks);
        for k in 0..n_blocks {
            let o = 24 + k * 16;
            index.push((
                u64::from_le_bytes(map[o..o + 8].try_into().unwrap()),
                u64::from_le_bytes(map[o + 8..o + 16].try_into().unwrap()),
            ));
        }
        Ok(Run { map, index, stream: 24 + n_blocks * 16, pairs })
    }

    /// The pairs with `target`, in (base) order.
    fn edges_of(&self, target: u64, out: &mut Vec<Edge>) {
        // The pairs with `target` begin in the last block whose first
        // target is < target (or the one after it) and may run on across
        // block boundaries.
        let mut blk = self.index.partition_point(|&(t, _)| t < target).saturating_sub(1);
        if blk >= self.index.len() {
            return;
        }
        let b = &self.map[self.stream..];
        let mut pos = self.index[blk].1 as usize;
        let (mut prev_t, mut prev_b) = (self.index[blk].0, 0u64);
        while pos < b.len() {
            // A block boundary (blocks are `STRIDE` pairs, except the last
            // of each compaction bucket): the deltas restart.
            if blk + 1 < self.index.len() && pos >= self.index[blk + 1].1 as usize {
                blk += 1;
                prev_t = self.index[blk].0;
                prev_b = 0;
            }
            let t = prev_t + get_varint(b, &mut pos);
            let base = (prev_b as i64 + unzigzag(get_varint(b, &mut pos))) as u64;
            let mask = get_mask(b, &mut pos);
            prev_t = t;
            prev_b = base;
            if t > target {
                break;
            }
            if t == target {
                out.push(Edge { target, base, mask });
            }
        }
    }
}

/// Per-row group membership, `(layer, seq) -> mask per row` (see
/// `EdgeGraph::push_groups`).
pub type GroupMasks = rustc_hash::FxHashMap<(u32, u32), Vec<u64>>;

/// A level's graph up to a horizon: `runs[layer][frame]`.
pub struct EdgeGraph {
    runs: Vec<Vec<Option<Run>>>,
    pub records: u64,
    pub bytes: u64,
}

impl EdgeGraph {
    /// Map every run `l{layer}/f{frame}.bin` with `frame <= horizon`.
    pub fn open(dir: &Path, horizon: u32) -> Result<Self> {
        let mut runs: Vec<Vec<Option<Run>>> = Vec::new();
        let (mut records, mut bytes) = (0u64, 0u64);
        for layer in layers(dir) {
            if layer > horizon {
                continue;
            }
            while runs.len() <= layer as usize {
                runs.push(Vec::new());
            }
            for frame in layer..=horizon {
                let p = run_path(dir, layer, frame);
                let run = if p.exists() { Some(Run::open(&p)?) } else { None };
                if let Some(r) = &run {
                    records += r.pairs;
                    bytes += r.map.len() as u64;
                }
                let v = &mut runs[layer as usize];
                while v.len() <= frame as usize {
                    v.push(None);
                }
                v[frame as usize] = run;
            }
        }
        Ok(EdgeGraph { runs, records, bytes })
    }

    /// DIAGNOSTIC: every `(pred, target)` edge recorded at frame `frame`
    /// whose pred is in `preds` - a full scan of that frame's runs.
    pub fn edges_from(&self, frame: u32, preds: &[u64]) -> Vec<(u64, u64)> {
        let mut out = Vec::new();
        for runs in &self.runs {
            let Some(Some(run)) = runs.get(frame as usize) else { continue };
            let b = &run.map[run.stream..];
            let mut pos = 0usize;
            let mut blk = 0usize;
            if run.index.is_empty() {
                continue;
            }
            let (mut prev_t, mut prev_b) = (run.index[0].0, 0u64);
            while pos < b.len() {
                if blk + 1 < run.index.len() && pos >= run.index[blk + 1].1 as usize {
                    blk += 1;
                    prev_t = run.index[blk].0;
                    prev_b = 0;
                }
                let t = prev_t + get_varint(b, &mut pos);
                let base = (prev_b as i64 + unzigzag(get_varint(b, &mut pos))) as u64;
                let mask = get_mask(b, &mut pos);
                prev_t = t;
                prev_b = base;
                for &p in preds {
                    if p >= base && p < base + 64 && mask & (1u64 << (p - base)) != 0 {
                        out.push((p, t));
                    }
                }
            }
        }
        out
    }

    /// DIAGNOSTIC (`rewrite partition-probe`): push per-row GROUP masks
    /// one frame forward. `preds[(layer, seq)][row]` is the set of groups
    /// (a bit each) a frame-`frame - 1` state belongs to; the result is
    /// the same for the targets of every edge recorded at `frame` - the
    /// OR of its preds' masks. Also returns how many targets have a layer
    /// below `frame` (states the full run had already seen, whose own
    /// successors were recorded at THEIR layer and are not followed here).
    pub fn push_groups(&self, frame: u32, preds: &GroupMasks) -> (GroupMasks, u64) {
        let mut out: GroupMasks = Default::default();
        let mut old = 0u64;
        for runs in &self.runs {
            let Some(Some(run)) = runs.get(frame as usize) else { continue };
            let b = &run.map[run.stream..];
            let mut pos = 0usize;
            let mut blk = 0usize;
            if run.index.is_empty() {
                continue;
            }
            let (mut prev_t, mut prev_b) = (run.index[0].0, 0u64);
            while pos < b.len() {
                if blk + 1 < run.index.len() && pos >= run.index[blk + 1].1 as usize {
                    blk += 1;
                    prev_t = run.index[blk].0;
                    prev_b = 0;
                }
                let t = prev_t + get_varint(b, &mut pos);
                let base = (prev_b as i64 + unzigzag(get_varint(b, &mut pos))) as u64;
                let mask = get_mask(b, &mut pos);
                prev_t = t;
                prev_b = base;
                let Some(rows) = preds.get(&(id_layer(base), crate::frame::id_seq(base))) else { continue };
                let row0 = crate::frame::id_row(base) as usize;
                let mut m = 0u64;
                let mut bits = mask;
                while bits != 0 {
                    let i = bits.trailing_zeros() as usize;
                    bits &= bits - 1;
                    if let Some(&g) = rows.get(row0 + i) {
                        m |= g;
                    }
                }
                if m == 0 {
                    continue;
                }
                let v = out.entry((id_layer(t), crate::frame::id_seq(t))).or_default();
                let r = crate::frame::id_row(t) as usize;
                if v.len() <= r {
                    v.resize(r + 1, 0);
                }
                if v[r] == 0 && id_layer(t) < frame {
                    old += 1;
                }
                v[r] |= m;
            }
        }
        (out, old)
    }

    /// The predecessors of `target` recorded at frame `frame`.
    pub fn preds_at(&self, target: u64, frame: u32, buf: &mut Vec<Edge>) {
        if let Some(Some(run)) = self.runs.get(id_layer(target) as usize).and_then(|v| v.get(frame as usize)) {
            run.edges_of(target, buf);
        }
    }
}

/// The marked set as bitmaps per `(layer, seq)` piece.
#[derive(Default)]
pub struct Marks {
    bits: rustc_hash::FxHashMap<(u32, u32), Vec<u64>>,
    pub count: usize,
}

impl Marks {
    /// True if newly marked.
    fn insert(&mut self, id: u64) -> bool {
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
        true
    }

    pub fn contains(&self, id: u64) -> bool {
        let (layer, seq, row) = (id_layer(id), crate::frame::id_seq(id), crate::frame::id_row(id));
        self.bits
            .get(&(layer, seq))
            .and_then(|v| v.get((row / 64) as usize))
            .is_some_and(|w| w & (1 << (row % 64)) != 0)
    }

    /// Every marked id, by (layer, seq, row).
    pub fn ids(&self) -> Vec<u64> {
        let mut keys: Vec<&(u32, u32)> = self.bits.keys().collect();
        keys.sort();
        let mut out = Vec::with_capacity(self.count);
        for k in keys {
            for (w, &word) in self.bits[k].iter().enumerate() {
                let mut m = word;
                while m != 0 {
                    let b = m.trailing_zeros();
                    out.push(pack_id(k.0, k.1, (w as u32) * 64 + b));
                    m &= m - 1;
                }
            }
        }
        out
    }
}

pub struct BfsStats {
    pub edges_read: u64,
    pub lookups: u64,
    pub t_open: std::time::Duration,
    pub t_bfs: std::time::Duration,
}

/// The BFS backward: `seeds` (win rows, layer <= horizon) and their
/// transitive predecessors under the eligibility rule - iteration i
/// (i = horizon-1 down to 1) marks a predecessor of an i+1-frontier state
/// iff the predecessor's layer is <= i. A state's edges are consulted
/// once, when it enters the frontier: its runs of frames layer..=i+1.
pub fn bfs(graph: &EdgeGraph, horizon: u32, seeds: impl IntoIterator<Item = u64>) -> (Marks, BfsStats) {
    let t = std::time::Instant::now();
    let mut marks = Marks::default();
    let mut frontier: Vec<u64> = Vec::new();
    for s in seeds {
        if id_layer(s) <= horizon && marks.insert(s) {
            frontier.push(s);
        }
    }
    let (mut edges_read, mut lookups) = (0u64, 0u64);
    let workers = crate::frame::threads().max(1);
    for i in (1..horizon).rev() {
        let t_it = std::time::Instant::now();
        frontier.sort_unstable();
        let last_frame = (i + 1).min(horizon);
        // The lookups in parallel over the frontier (the marks read-only:
        // a predecessor seen by two workers is deduplicated at the insert
        // below), the inserts sequential and in frontier order.
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
                if marks.insert(p) {
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

pub struct BackwardResult {
    pub marked: Visited,
    pub marks: Marks,
    pub stats: BfsStats,
}

/// The backward over a level dir: seeds from the checkpoint headers' win
/// rows (layers 1..=horizon), the BFS over `<dir>/edges`, the marked ids
/// resolved to `(shape, key, cell)` through the checkpoint files.
pub fn backward(dir: &Path, horizon: u32) -> Result<BackwardResult> {
    // A tree can END before the horizon (every lane won or died - the
    // synthetic targets do this): the wins by `horizon` are then the
    // wins the tree has, and there are no frames past its last to read.
    let horizon = {
        let mut last = 0u32;
        while dir.join("frames").join(format!("f{:03}", last + 1)).is_dir() {
            last += 1;
        }
        horizon.min(last)
    };
    let t = std::time::Instant::now();
    let graph = EdgeGraph::open(&dir.join("edges"), horizon)?;
    let t_open = t.elapsed();
    let mut seeds: Vec<u64> = Vec::new();
    let mut files: Vec<(u32, u32, crate::search::checkpoint::FrameFile)> = Vec::new();
    for layer in 0..=horizon {
        for (seq, file) in crate::frame::frame_files_seq(dir, layer)? {
            if layer >= 1 {
                seeds.extend(file.win_rows().iter().map(|&(row, _cell)| pack_id(layer, seq, row)));
            }
            files.push((layer, seq, file));
        }
    }
    let (marks, mut stats) = bfs(&graph, horizon, seeds);
    stats.t_open = t_open;
    // The marked ids as the ladder's (shape, key, cell) set.
    let t = std::time::Instant::now();
    let mut marked = Visited::new();
    let ids = marks.ids();
    let mut i = 0usize;
    for (layer, seq, file) in &files {
        let lo = ids.partition_point(|&id| (id_layer(id), crate::frame::id_seq(id)) < (*layer, *seq));
        let hi = ids.partition_point(|&id| (id_layer(id), crate::frame::id_seq(id)) <= (*layer, *seq));
        if lo == hi {
            continue;
        }
        let cells = file.row_cells();
        for &id in &ids[lo..hi] {
            let row = crate::frame::id_row(id);
            ensure!(
                row < file.width(),
                "edges: marked id l{layer} s{seq} r{row} is past its file's {} rows",
                file.width()
            );
            marked.insert(file.shape_hash(), file.key_at(row), cells[row as usize]);
            i += 1;
        }
    }
    ensure!(i == ids.len(), "edges: {} marked ids, {} resolved through the checkpoint files", ids.len(), i);
    eprintln!(
        "[bfs] h{horizon}: {} runs-bytes {:.2} GB, open {:.0} ms, bfs {:.0} ms ({} lookups, {} edges), resolve {:.0} ms",
        graph.records,
        graph.bytes as f64 / 1e9,
        stats.t_open.as_secs_f64() * 1e3,
        stats.t_bfs.as_secs_f64() * 1e3,
        stats.lookups,
        stats.edges_read,
        t.elapsed().as_secs_f64() * 1e3
    );
    Ok(BackwardResult { marked, marks, stats })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_worker(dir: &Path, frame: u32, layer: u32, worker: u32, edges: &[Edge]) {
        std::fs::create_dir_all(raw_dir(dir, frame)).unwrap();
        let mut buf = Vec::new();
        for e in edges {
            encode_record(&mut buf, e.target, e.base, e.mask);
        }
        std::fs::write(raw_path(dir, frame, layer, worker), buf).unwrap();
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
            edges.push(Edge { target: pack_id(layer, 0, t as u32), base: pack_id(layer - 1, 0, 16 * t as u32), mask: 1 });
        }
        let last = pack_id(layer, 0, STRIDE as u32 - 11);
        for k in 0..30u32 {
            edges.push(Edge { target: last, base: pack_id(layer - 1, 0, 100_000 + 16 * k), mask: 3 });
        }
        let wide = pack_id(layer, 1, 9000);
        for k in 0..(3 * STRIDE as u32) {
            edges.push(Edge { target: wide, base: pack_id(layer - 1, 2, 16 * k), mask: 0xffff });
        }
        // Written unsorted, across two workers.
        edges.reverse();
        let (a, b) = edges.split_at(edges.len() / 2);
        write_worker(&dir, layer, layer, 0, a);
        write_worker(&dir, layer, layer, 1, b);
        let st = compact_frame(&dir, layer).unwrap();
        assert_eq!(st.records as usize, edges.len());
        assert_eq!(st.pairs as usize, edges.len(), "no two records share (target, base)");
        assert!(!raw_dir(&dir, layer).exists(), "the raw files are gone");
        let g = EdgeGraph::open(&dir, layer).unwrap();
        let mut buf = Vec::new();
        g.preds_at(last, layer, &mut buf);
        assert_eq!(buf.len(), 31, "the straddling target");
        buf.clear();
        g.preds_at(wide, layer, &mut buf);
        assert_eq!(buf.len(), 3 * STRIDE, "the multi-block target");
        buf.clear();
        g.preds_at(pack_id(layer, 0, 0), layer, &mut buf);
        assert_eq!(buf.len(), 1);
        buf.clear();
        g.preds_at(pack_id(layer, 0, 7), layer, &mut buf);
        assert_eq!(buf, vec![Edge { target: pack_id(layer, 0, 7), base: pack_id(layer - 1, 0, 112), mask: 1 }]);
        buf.clear();
        g.preds_at(pack_id(layer, 3, 0), layer, &mut buf);
        assert!(buf.is_empty(), "an absent target");
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
