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

/// One edge record on disk: target id, slice base id, lane mask.
pub const RECORD_BYTES: usize = 18;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    pub target: u64,
    pub base: u64,
    pub mask: u16,
}

#[inline]
fn decode(b: &[u8]) -> Edge {
    Edge {
        target: u64::from_le_bytes(b[0..8].try_into().unwrap()),
        base: u64::from_le_bytes(b[8..16].try_into().unwrap()),
        mask: u16::from_le_bytes(b[16..18].try_into().unwrap()),
    }
}

fn layer_dir(dir: &Path, layer: u32) -> PathBuf {
    dir.join(format!("l{:03}", layer))
}

fn run_path(dir: &Path, layer: u32, frame: u32) -> PathBuf {
    layer_dir(dir, layer).join(format!("f{:03}.bin", frame))
}

/// The worker files under `<dir>/l{layer}/` (`w*.bin`).
fn worker_files(ldir: &Path) -> Vec<PathBuf> {
    let mut v: Vec<PathBuf> = std::fs::read_dir(ldir)
        .into_iter()
        .flatten()
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.file_name().and_then(|s| s.to_str()).is_some_and(|n| n.starts_with('w') && n.ends_with(".bin")))
        .collect();
    v.sort();
    v
}

/// The layers with an `l{layer}` dir under `dir`.
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
const RUN_VERSION: u32 = 1;

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

#[inline]
fn zigzag(v: i64) -> u64 {
    ((v << 1) ^ (v >> 63)) as u64
}

#[inline]
fn unzigzag(v: u64) -> i64 {
    ((v >> 1) as i64) ^ -((v & 1) as i64)
}

/// Sort one bucket of records by (target, base), merge equal pairs' masks
/// and encode it as index blocks: `(index entries (first target, offset
/// within this bucket's stream), stream, pairs)`.
fn encode_bucket(recs: &mut [(u64, u64, u16)]) -> (Vec<(u64, u64)>, Vec<u8>, u64) {
    recs.sort_unstable_by_key(|r| (r.0, r.1));
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
        put_varint(&mut out, m as u64);
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

/// After frame `frame`: turn every layer's worker files into the run
/// `l{layer}/f{frame}.bin` - the records sorted by (target, base), equal
/// pairs merged, delta-varint encoded in blocks of `STRIDE` with an index
/// of (first target, offset) per block - and delete them. One layer at a
/// time (the deepest, largest first), its records range-partitioned by
/// target into buckets sorted and encoded in parallel.
pub fn compact_frame(dir: &Path, frame: u32) -> Result<CompactStats> {
    let mut layers: Vec<(u32, Vec<PathBuf>, u64)> = layers(dir)
        .into_iter()
        .map(|layer| {
            let files = worker_files(&layer_dir(dir, layer));
            let bytes: u64 = files.iter().map(|f| std::fs::metadata(f).map(|m| m.len()).unwrap_or(0)).sum();
            (layer, files, bytes)
        })
        .filter(|(_, files, _)| !files.is_empty())
        .collect();
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
    let n_buckets = (crate::frame::threads() * 2).clamp(1, 64);
    for (layer, files, bytes) in &layers {
        let t0 = std::time::Instant::now();
        // Splitters from a sample of one file's targets (every worker
        // file is a slice of the same frame): balanced buckets.
        let splitters: Vec<u64> = {
            let f = &files[0];
            let len = std::fs::metadata(f)?.len() as usize / RECORD_BYTES;
            let take = len.min(1 << 16);
            let mut sample: Vec<u64> = Vec::with_capacity(take);
            let b = std::fs::read(f).with_context(|| f.display().to_string())?;
            for k in 0..take {
                let i = (k * len) / take;
                sample.push(u64::from_le_bytes(b[i * RECORD_BYTES..i * RECORD_BYTES + 8].try_into().unwrap()));
            }
            sample.sort_unstable();
            (1..n_buckets).map(|k| sample[(k * sample.len()) / n_buckets]).collect()
        };
        let bucket_of = |t: u64| splitters.partition_point(|&s| s <= t);
        // Each worker file decoded and scattered into its buckets, files
        // in parallel.
        let parts: Vec<Vec<Vec<(u64, u64, u16)>>> = std::thread::scope(|scope| {
            let handles: Vec<_> = files
                .iter()
                .map(|f| {
                    let bucket_of = &bucket_of;
                    scope.spawn(move || -> Result<Vec<Vec<(u64, u64, u16)>>> {
                        let b = std::fs::read(f).with_context(|| f.display().to_string())?;
                        ensure!(b.len() % RECORD_BYTES == 0, "{}: truncated edge file", f.display());
                        let mut out: Vec<Vec<(u64, u64, u16)>> = vec![Vec::new(); n_buckets];
                        for i in 0..b.len() / RECORD_BYTES {
                            let e = decode(&b[i * RECORD_BYTES..(i + 1) * RECORD_BYTES]);
                            out[bucket_of(e.target)].push((e.target, e.base, e.mask));
                        }
                        Ok(out)
                    })
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("edge compaction reader panicked")).collect::<Result<Vec<_>>>()
        })?;
        st.records += *bytes / RECORD_BYTES as u64;
        st.t_read += t0.elapsed();
        let t0 = std::time::Instant::now();
        // The buckets, each gathered from every file, sorted and encoded
        // in parallel.
        let encoded: Vec<(Vec<(u64, u64)>, Vec<u8>, u64)> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..n_buckets)
                .map(|k| {
                    let parts = &parts;
                    scope.spawn(move || {
                        let n: usize = parts.iter().map(|p| p[k].len()).sum();
                        let mut recs: Vec<(u64, u64, u16)> = Vec::with_capacity(n);
                        for p in parts {
                            recs.extend_from_slice(&p[k]);
                        }
                        encode_bucket(&mut recs)
                    })
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("edge compaction worker panicked")).collect()
        });
        drop(parts);
        st.t_sort += t0.elapsed();
        let t0 = std::time::Instant::now();
        // Concatenate: the index with the buckets' offsets shifted.
        let n_blocks: usize = encoded.iter().map(|(ix, _, _)| ix.len()).sum();
        let stream_bytes: usize = encoded.iter().map(|(_, s, _)| s.len()).sum();
        let mut out: Vec<u8> = Vec::with_capacity(24 + n_blocks * 16 + stream_bytes);
        out.extend_from_slice(RUN_MAGIC);
        out.extend_from_slice(&RUN_VERSION.to_le_bytes());
        let pairs: u64 = encoded.iter().map(|(_, _, p)| *p).sum();
        out.extend_from_slice(&pairs.to_le_bytes());
        out.extend_from_slice(&(n_blocks as u64).to_le_bytes());
        let mut off = 0u64;
        for (ix, s, _) in &encoded {
            for &(t, o) in ix {
                out.extend_from_slice(&t.to_le_bytes());
                out.extend_from_slice(&(o + off).to_le_bytes());
            }
            off += s.len() as u64;
        }
        for (_, s, _) in &encoded {
            out.extend_from_slice(s);
        }
        let ldir = layer_dir(dir, *layer);
        let path = run_path(dir, *layer, frame);
        let tmp = ldir.join(format!("tmp-f{:03}.bin", frame));
        std::fs::write(&tmp, &out)?;
        std::fs::rename(&tmp, &path)?;
        for f in files {
            std::fs::remove_file(f)?;
        }
        st.pairs += pairs;
        st.bytes += out.len() as u64;
        st.t_write += t0.elapsed();
    }
    Ok(st)
}

/// Drop what a run past `last` (the last checkpointed frame) left behind:
/// every layer's worker files and every run of a later frame.
pub fn discard_after(dir: &Path, last: u32) -> Result<()> {
    for layer in layers(dir) {
        let ldir = layer_dir(dir, layer);
        for f in worker_files(&ldir) {
            std::fs::remove_file(f)?;
        }
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
            let mask = get_varint(b, &mut pos) as u16;
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
    let mut buf: Vec<Edge> = Vec::new();
    for i in (1..horizon).rev() {
        let t_it = std::time::Instant::now();
        frontier.sort_unstable();
        let mut next: Vec<u64> = Vec::new();
        let last_frame = (i + 1).min(horizon);
        for &tgt in &frontier {
            for frame in id_layer(tgt)..=last_frame {
                if frame == 0 {
                    continue;
                }
                buf.clear();
                graph.preds_at(tgt, frame, &mut buf);
                lookups += 1;
                edges_read += buf.len() as u64;
                for e in &buf {
                    let mut m = e.mask;
                    while m != 0 {
                        let lane = m.trailing_zeros();
                        m &= m - 1;
                        let p = e.base + lane as u64;
                        debug_assert_eq!(id_layer(p), frame - 1);
                        if marks.insert(p) {
                            next.push(p);
                        }
                    }
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

    fn write_worker(dir: &Path, layer: u32, worker: u32, edges: &[Edge]) {
        let ldir = layer_dir(dir, layer);
        std::fs::create_dir_all(&ldir).unwrap();
        let mut buf = Vec::new();
        for e in edges {
            buf.extend_from_slice(&e.target.to_le_bytes());
            buf.extend_from_slice(&e.base.to_le_bytes());
            buf.extend_from_slice(&e.mask.to_le_bytes());
        }
        std::fs::write(ldir.join(format!("w{:03}.bin", worker)), buf).unwrap();
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
        write_worker(&dir, layer, 0, a);
        write_worker(&dir, layer, 1, b);
        let st = compact_frame(&dir, layer).unwrap();
        assert_eq!(st.records as usize, edges.len());
        assert_eq!(st.pairs as usize, edges.len(), "no two records share (target, base)");
        assert!(worker_files(&layer_dir(&dir, layer)).is_empty());
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
