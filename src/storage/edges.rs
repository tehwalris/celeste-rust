//! THE RECORDED GRAPH, target-side and ID-free (plans/edges-target.md).
//!
//! Every edge of frame `f` leaves a state of layer `f - 1` and is stored in
//! its TARGET's region: per (target region, frame) its bundles
//! (`storage::bundle`: source key, transfer, offset, target key -> target
//! cells) in one or more SEGMENTS - a wave's slices; a raise adds a file -
//! each a zstd block. Per frame ONE file `edges/f{frame}.bin` (a raise adds
//! `f{frame}.r{seq}.bin`): a header - the segments, by region, and per
//! target region the SOURCE regions its edges leave (the out-edge lookup) -
//! and the blocks. Transfers are global ids into `edges/xfer.bin`,
//! content-canonical (each wave appends its new pairs sorted). Nothing
//! names a state by a number: a state is its region, key and cell (`Sk`).

use anyhow::{ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::bundle::{self, Bundle};
use super::{Geometry, Sk, GEOMETRY};
use crate::search::arc_edges::{decode_pair, encode_pair, Pair, PAIR_BYTES};

/// The global transfer table: every distinct pair the tree's edges carry,
/// a pair's id its position. `edges/xfer.bin` (`encode_pair` each), its
/// trusted length in the done marker (`done.txt`: pairs past it are a
/// killed wave's).
#[derive(Default, Clone)]
pub struct XferTable {
    pub pairs: Vec<Pair>,
    index: FxHashMap<Pair, u32>,
    /// Pairs written to `xfer.bin` (`flush`).
    flushed: usize,
}

fn xfer_path(dir: &Path) -> PathBuf {
    dir.join("xfer.bin")
}

impl XferTable {
    /// The tree's table (`dir` = `<level>/edges`) as its done marker trusts
    /// it; empty without one.
    pub fn load(dir: &Path) -> Result<Self> {
        let pairs = Self::load_pairs(dir)?;
        let index = pairs.iter().enumerate().map(|(i, p)| (*p, i as u32)).collect();
        Ok(XferTable { flushed: pairs.len(), pairs, index })
    }

    /// The table's pairs alone, as `load` reads them (the readers need no
    /// index: the mod room at h155 has 4.0M pairs).
    pub fn load_pairs(dir: &Path) -> Result<Vec<Pair>> {
        let n = match read_done(dir)? {
            Some(d) => d.xfers,
            None => 0,
        };
        let b = if n > 0 { std::fs::read(xfer_path(dir)).with_context(|| xfer_path(dir).display().to_string())? } else { Vec::new() };
        ensure!(b.len() >= n * PAIR_BYTES, "{}: {} pairs, {} trusted", xfer_path(dir).display(), b.len() / PAIR_BYTES, n);
        Ok(b[..n * PAIR_BYTES].chunks_exact(PAIR_BYTES).map(decode_pair).collect())
    }

    /// The workers' NEW pairs (`tails`: what each worker's table gained
    /// since its last merge) into the table: the ones it lacks appended in
    /// value order (content-canonical: a function of the pairs, whatever
    /// the workers); per tail its pairs' global ids.
    pub fn merge(&mut self, tails: &[&[Pair]]) -> Vec<Vec<u32>> {
        let mut new: Vec<Pair> = tails.iter().flat_map(|t| t.iter()).filter(|p| !self.index.contains_key(p)).copied().collect();
        new.sort_unstable();
        new.dedup();
        for p in new {
            self.index.insert(p, self.pairs.len() as u32);
            self.pairs.push(p);
        }
        tails.iter().map(|t| t.iter().map(|p| self.index[p]).collect()).collect()
    }

    /// Write the pairs merged since the last flush past the trusted length
    /// (the frame's done marker trusts them).
    pub fn flush(&mut self, dir: &Path) -> Result<()> {
        use std::io::{Seek, Write};
        let first = self.flushed;
        if self.pairs.len() > first {
            std::fs::create_dir_all(dir)?;
            let mut f = std::fs::OpenOptions::new().create(true).truncate(false).write(true).open(xfer_path(dir))?;
            f.set_len((first * PAIR_BYTES) as u64)?;
            f.seek(std::io::SeekFrom::Start((first * PAIR_BYTES) as u64))?;
            let mut buf = Vec::with_capacity((self.pairs.len() - first) * PAIR_BYTES);
            for p in &self.pairs[first..] {
                encode_pair(&mut buf, p);
            }
            f.write_all(&buf)?;
            f.sync_data()?;
        }
        self.flushed = self.pairs.len();
        Ok(())
    }
}

const MAGIC: &[u8; 4] = b"CSE2";
/// 6: target-region bundle blocks (5: source-side unit blocks with lids
/// and an owner index; trees before are rebuilt, not converted).
const VERSION: u32 = 6;

/// One segment's place in an edge file.
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
struct SegHead {
    region: u32,
    /// Offset of its zstd block in the file's data region, its length and
    /// its raw length.
    at: u64,
    len: u32,
    raw: u32,
    bundles: u32,
    edges: u64,
}

#[derive(Serialize, Deserialize)]
struct FileHead {
    frame: u32,
    /// By region (a region's segments in the order written).
    segs: Vec<SegHead>,
    /// Per target region (sorted) the source regions (sorted) its edges leave.
    sources: Vec<(u32, Vec<u32>)>,
}

/// One target region's encoded segment, as a wave hands it to the file.
pub struct Segment {
    pub region: u32,
    /// The zstd block and its raw length.
    pub bytes: Vec<u8>,
    pub raw: u32,
    pub bundles: u32,
    pub edges: u64,
}

impl Segment {
    /// A raw block of `bundles` bundles and `edges` edges as a segment of
    /// `region`.
    pub fn of_raw(region: u32, raw: &[u8], bundles: u32, edges: u64, z: &mut zstd::bulk::Compressor<'static>) -> Result<Segment> {
        ensure!(raw.len() < u32::MAX as usize, "an edge segment of {} bytes", raw.len());
        Ok(Segment { region, bytes: bundle::compress(raw, z)?, raw: raw.len() as u32, bundles, edges })
    }

    /// `bundles` (distinct) as a segment of `region`.
    pub fn encode(region: u32, bundles: &mut [Bundle], raw: &mut Vec<u8>, z: &mut zstd::bulk::Compressor<'static>) -> Result<Segment> {
        bundle::encode(bundles, &bundle::ORDER, raw);
        ensure!(raw.len() < u32::MAX as usize, "an edge segment of {} bytes", raw.len());
        Ok(Segment {
            region,
            bytes: bundle::compress(raw, z)?,
            raw: raw.len() as u32,
            bundles: bundles.len() as u32,
            edges: bundles.iter().map(|b| b.mask.count_ones() as u64).sum(),
        })
    }
}

/// The edge file of frame `frame` (`raised`: a raise's, by its first seq).
pub fn file_path(dir: &Path, frame: u32, raised: Option<u32>) -> PathBuf {
    match raised {
        None => dir.join(format!("f{frame:03}.bin")),
        Some(seq) => dir.join(format!("f{frame:03}.r{seq:04}.bin")),
    }
}

/// Write frame `frame`'s edge file from its segments (any order) and its
/// source-region index (`sources`: per target region the source regions,
/// any order). Atomic (renamed into place). Returns its bytes.
pub fn write_file(path: &Path, frame: u32, mut segs: Vec<Segment>, mut sources: Vec<(u32, Vec<u32>)>) -> Result<u64> {
    use std::io::Write;
    segs.sort_by_key(|s| s.region);
    sources.sort_unstable_by_key(|s| s.0);
    for s in &mut sources {
        s.1.sort_unstable();
        s.1.dedup();
    }
    let mut heads = Vec::with_capacity(segs.len());
    let mut at = 0u64;
    for s in &segs {
        heads.push(SegHead { region: s.region, at, len: s.bytes.len() as u32, raw: s.raw, bundles: s.bundles, edges: s.edges });
        at += s.bytes.len() as u64;
    }
    let head = bincode::serialize(&FileHead { frame, segs: heads, sources }).context("serializing an edge file header")?;
    std::fs::create_dir_all(path.parent().expect("an edges dir"))?;
    let tmp = path.with_extension("tmp");
    {
        let mut out = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(&tmp).with_context(|| tmp.display().to_string())?);
        out.write_all(MAGIC)?;
        out.write_all(&VERSION.to_le_bytes())?;
        out.write_all(&(head.len() as u64).to_le_bytes())?;
        out.write_all(&head)?;
        for s in &segs {
            out.write_all(&s.bytes)?;
        }
        out.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(16 + head.len() as u64 + at)
}

/// One edge file, mapped.
struct EdgeFile {
    map: memmap2::Mmap,
    head: FileHead,
    data: usize,
    /// Per source region the target regions its edges reach (`head.sources`
    /// inverted on first use).
    targets: std::sync::OnceLock<FxHashMap<u32, Vec<u32>>>,
}

impl EdgeFile {
    fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = std::fs::File::open(path).with_context(|| path.display().to_string())?;
        // SAFETY: edge files are written once (renamed into place) and
        // never modified afterwards.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 16 && &map[0..4] == MAGIC, "{}: not an edge file of this format (a tree from before target-region edges: rebuild it)", path.display());
        let v = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(v == VERSION, "{}: edge file version {v}, expected {VERSION}", path.display());
        let n = u64::from_le_bytes(map[8..16].try_into().unwrap()) as usize;
        ensure!(map.len() >= 16 + n, "{}: truncated header", path.display());
        let head: FileHead = bincode::deserialize(&map[16..16 + n]).with_context(|| path.display().to_string())?;
        ensure!(head.frame == frame, "{}: the edges of frame {}, not {frame}", path.display(), head.frame);
        ensure!(head.segs.windows(2).all(|w| w[0].region <= w[1].region), "{}: segments not by region", path.display());
        let data = 16 + n;
        for s in &head.segs {
            ensure!(data as u64 + s.at + s.len as u64 <= map.len() as u64, "{}: a segment past the file", path.display());
        }
        Ok(EdgeFile { map, head, data, targets: std::sync::OnceLock::new() })
    }

    /// The segments of `region`.
    fn segs_of(&self, region: u32) -> &[SegHead] {
        let s = &self.head.segs;
        let lo = s.partition_point(|x| x.region < region);
        let hi = lo + s[lo..].partition_point(|x| x.region == region);
        &s[lo..hi]
    }

    fn seg<'a>(&'a self, h: &'a SegHead) -> SegRef<'a> {
        let a = self.data + h.at as usize;
        SegRef { region: h.region, bytes: &self.map[a..a + h.len as usize], raw: h.raw as usize, bundles: h.bundles, edges: h.edges }
    }

    fn targets(&self) -> &FxHashMap<u32, Vec<u32>> {
        self.targets.get_or_init(|| {
            let mut m: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
            for (t, srcs) in &self.head.sources {
                for &s in srcs {
                    m.entry(s).or_default().push(*t);
                }
            }
            m
        })
    }
}

/// One stored segment: its target region, zstd block and raw length.
#[derive(Clone, Copy)]
pub struct SegRef<'a> {
    pub region: u32,
    pub bytes: &'a [u8],
    pub raw: usize,
    pub bundles: u32,
    pub edges: u64,
}

/// One edge: source, target, global transfer id.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Edge {
    pub src: Sk,
    pub dst: Sk,
    pub xfer: u32,
}

/// The edges of bundle `b` of target region `region`: `f(source, target)`
/// per target cell.
#[inline]
pub fn expand(geo: &Geometry, region: u32, b: &Bundle, mut f: impl FnMut(Sk, Sk)) {
    let slot = region % geo.slots;
    let mut m = b.mask;
    while m != 0 {
        let c = m.trailing_zeros();
        m &= m - 1;
        let s_cell = bundle::source_cell(geo.cell(slot, c), b.o);
        f(Sk::at(geo, b.s_shape, b.s_key, s_cell), Sk { region, key: b.t_key, local: c as u8 });
    }
}

/// A worker's decoder: a zstd context and the raw block's buffer.
pub struct Decoder {
    z: zstd::bulk::Decompressor<'static>,
    raw: Vec<u8>,
}

impl Default for Decoder {
    fn default() -> Self {
        Decoder { z: zstd::bulk::Decompressor::new().expect("a zstd decompressor"), raw: Vec::new() }
    }
}

impl Decoder {
    /// Segment `s`'s raw block, opened (its dictionaries read).
    pub fn open<'a>(&'a mut self, s: &SegRef) -> Result<bundle::Block<'a>> {
        bundle::decompress(s.bytes, s.raw, &mut self.z, &mut self.raw)?;
        bundle::Block::open(&self.raw, &bundle::ORDER)
    }

    /// Every bundle of segment `s`.
    pub fn bundles(&mut self, s: &SegRef, f: impl FnMut(Bundle)) -> Result<()> {
        self.open(s)?.bundles(f)
    }
}

/// A level's recorded graph up to a horizon, mapped: per frame its edge
/// files, and the global transfer table.
pub struct EdgeStore {
    frames: Vec<Vec<EdgeFile>>,
    xfers: Vec<Pair>,
    pub edges: u64,
    pub bytes: u64,
}

impl EdgeStore {
    /// Map frames `1..=horizon`'s edge files under `dir` (`<level>/edges`).
    pub fn open(dir: &Path, horizon: u32) -> Result<Self> {
        let mut frames: Vec<Vec<EdgeFile>> = (0..=horizon).map(|_| Vec::new()).collect();
        let (mut edges, mut bytes) = (0u64, 0u64);
        let mut paths: Vec<(u32, PathBuf)> = Vec::new();
        for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
            let p = e.path();
            let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
            let Some(f) = frame_of_name(n) else { continue };
            if !n.ends_with(".bin") || f == 0 || f > horizon {
                continue;
            }
            paths.push((f, p));
        }
        // A frame's own file first, then its raises' (by seq).
        paths.sort();
        for (f, p) in paths {
            let file = EdgeFile::open(&p, f)?;
            edges += file.head.segs.iter().map(|s| s.edges).sum::<u64>();
            bytes += file.map.len() as u64;
            frames[f as usize].push(file);
        }
        let xfers = XferTable::load_pairs(dir)?;
        Ok(EdgeStore { frames, xfers, edges, bytes })
    }

    /// The transfer of global id `xfer`.
    pub fn pair(&self, xfer: u32) -> Option<Pair> {
        self.xfers.get(xfer as usize).copied()
    }

    /// The whole transfer table.
    pub fn pairs(&self) -> &[Pair] {
        &self.xfers
    }

    /// Does frame `frame` have recorded edges?
    pub fn has_frame(&self, frame: u32) -> bool {
        self.frames.get(frame as usize).is_some_and(|v| !v.is_empty())
    }

    /// The last frame of the store.
    pub fn horizon(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    /// The target regions with edges at frame `frame`, sorted, distinct.
    pub fn regions(&self, frame: u32) -> Vec<u32> {
        let mut v: Vec<u32> = self.frames.get(frame as usize).into_iter().flatten().flat_map(|f| f.head.segs.iter().map(|s| s.region)).collect();
        v.dedup();
        v.sort_unstable();
        v.dedup();
        v
    }

    /// The segments of target region `region` at frame `frame`.
    pub fn segments(&self, frame: u32, region: u32) -> Vec<SegRef<'_>> {
        self.frames.get(frame as usize).into_iter().flatten().flat_map(|f| f.segs_of(region).iter().map(move |h| f.seg(h))).collect()
    }

    /// Every segment of frame `frame`, by region.
    pub fn all_segments(&self, frame: u32) -> Vec<SegRef<'_>> {
        let mut v: Vec<SegRef> = self.frames.get(frame as usize).into_iter().flatten().flat_map(|f| f.head.segs.iter().map(move |h| f.seg(h))).collect();
        v.sort_by_key(|s| s.region);
        v
    }

    /// The target regions holding edges that leave source region `source`
    /// at frame `frame` (the out-edge lookup), sorted, distinct.
    pub fn targets_of(&self, frame: u32, source: u32) -> Vec<u32> {
        let mut v: Vec<u32> = self.frames.get(frame as usize).into_iter().flatten().flat_map(|f| f.targets().get(&source).into_iter().flatten().copied()).collect();
        v.sort_unstable();
        v.dedup();
        v
    }

    /// Every edge of frame `frame` on `threads` workers: each folds the
    /// edges of the segments it takes into its own accumulator (`init()`);
    /// the accumulators, in no particular order (fold them commutatively).
    pub fn par_scan<A: Send>(&self, frame: u32, threads: usize, init: impl Fn() -> A + Sync, f: impl Fn(&mut A, Edge) + Sync) -> Result<Vec<A>> {
        let segs = self.all_segments(frame);
        let geo = GEOMETRY;
        let next = std::sync::atomic::AtomicUsize::new(0);
        std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads.max(1))
                .map(|_| {
                    sc.spawn(|| -> Result<A> {
                        let (mut acc, mut dec) = (init(), Decoder::default());
                        loop {
                            let k = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some(s) = segs.get(k) else { return Ok(acc) };
                            dec.bundles(s, |b| expand(&geo, s.region, &b, |src, dst| f(&mut acc, Edge { src, dst, xfer: b.x })))?;
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("an edge scan worker panicked")).collect()
        })
    }

    /// Every segment of frame `frame` opened, on `threads` workers: each
    /// folds the blocks it takes (`f(acc, target region, block)`) into its
    /// own accumulator; the accumulators, in no particular order.
    pub fn par_blocks<A: Send>(&self, frame: u32, threads: usize, init: impl Fn() -> A + Sync, f: impl Fn(&mut A, u32, &bundle::Block) -> Result<()> + Sync) -> Result<Vec<A>> {
        let segs = self.all_segments(frame);
        let next = std::sync::atomic::AtomicUsize::new(0);
        std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads.max(1))
                .map(|_| {
                    sc.spawn(|| -> Result<A> {
                        let (mut acc, mut dec) = (init(), Decoder::default());
                        loop {
                            let k = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some(s) = segs.get(k) else { return Ok(acc) };
                            let blk = dec.open(s)?;
                            f(&mut acc, s.region, &blk)?;
                        }
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("an edge scan worker panicked")).collect()
        })
    }

    /// Every edge of frame `frame`, as a list, sorted.
    pub fn edges_at(&self, frame: u32, threads: usize) -> Result<Vec<Edge>> {
        let mut v: Vec<Edge> = self.par_scan(frame, threads, Vec::new, |v, e| v.push(e))?.into_iter().flatten().collect();
        v.sort_unstable();
        Ok(v)
    }

    /// The in-edges recorded at frame `frame` of `target` (sources of layer
    /// `frame - 1`): its region's segments decoded.
    pub fn preds_at(&self, target: Sk, frame: u32) -> Result<Vec<(Sk, u32)>> {
        let geo = GEOMETRY;
        let mut out = Vec::new();
        let mut dec = Decoder::default();
        for s in self.segments(frame, target.region) {
            dec.bundles(&s, |b| {
                if b.t_key == target.key && b.mask >> target.local & 1 == 1 {
                    let s_cell = bundle::source_cell(geo.cell(target.region % geo.slots, target.local as u32), b.o);
                    out.push((Sk::at(&geo, b.s_shape, b.s_key, s_cell), b.x));
                }
            })?;
        }
        Ok(out)
    }
}

/// THE TREE'S ONE TRUST MARKER (`<level>/edges/done.txt`): the last frame
/// whose files are all complete - its frame files, metadata, drop notes,
/// edge files and the pos graph, all written before it - and the transfer
/// table's trusted length. A frame past it is a killed run's (`resume`
/// discards it).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Done {
    pub frame: u32,
    pub xfers: usize,
}

fn read_done(dir: &Path) -> Result<Option<Done>> {
    let p = dir.join("done.txt");
    let s = match std::fs::read_to_string(&p) {
        Ok(s) => s,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(e) => return Err(e).with_context(|| p.display().to_string()),
    };
    let w: Vec<&str> = s.split_whitespace().collect();
    let bad = || anyhow::anyhow!("{}: not a done marker: {s:?}", p.display());
    match w[..] {
        [frame, "xfers", n] => Ok(Some(Done { frame: frame.parse().map_err(|_| bad())?, xfers: n.parse().map_err(|_| bad())? })),
        _ => Err(bad()),
    }
}

/// The last frame whose files are complete; `None`: no marker.
pub fn done_frame(dir: &Path) -> Option<u32> {
    read_done(dir).ok().flatten().map(|d| d.frame)
}

/// Trust frame `frame` and the first `xfers` transfers: the frame's LAST
/// write.
pub fn set_done(dir: &Path, frame: u32, xfers: usize) -> Result<()> {
    std::fs::create_dir_all(dir)?;
    let tmp = dir.join("done.tmp");
    std::fs::write(&tmp, format!("{frame} xfers {xfers}\n"))?;
    std::fs::rename(&tmp, dir.join("done.txt"))?;
    Ok(())
}

/// The frame an edge file's name is of: `f{frame}.bin`, `f{frame}.r{seq}.bin`
/// - the digits after the `f`, however many (the names pad to 3:
/// `f1000.bin` is frame 1000).
pub fn frame_of_name(name: &str) -> Option<u32> {
    let digits = name.strip_prefix('f')?;
    let end = digits.find(|c: char| !c.is_ascii_digit()).unwrap_or(digits.len());
    if end == 0 || !digits[end..].starts_with('.') {
        return None;
    }
    digits[..end].parse().ok()
}

/// Drop what a run past `last` (the last complete frame) left: every edge
/// file of a later frame and temporary files. Transfers past the trusted
/// length are overwritten by the next merge.
pub fn discard_after(dir: &Path, last: u32) -> Result<()> {
    for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
        let p = e.path();
        let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
        if n.ends_with(".tmp") || frame_of_name(n).is_some_and(|f| f > last) {
            std::fs::remove_file(&p).with_context(|| p.display().to_string())?;
        }
    }
    Ok(())
}

/// Edges `(source, target, transfer)` as bundles by target region: the
/// segments of one frame and its source index.
pub fn segments_of(edges: &[(Sk, Sk, u32)]) -> Result<(Vec<Segment>, Vec<(u32, Vec<u32>)>)> {
    let geo = GEOMETRY;
    let mut by: std::collections::BTreeMap<u32, FxHashMap<(u32, super::visited::Key, u32, u32, super::visited::Key), u64>> = Default::default();
    let mut sources: std::collections::BTreeMap<u32, Vec<u32>> = Default::default();
    for &(s, t, x) in edges {
        let o = bundle::offset(s.cell(&geo), t.cell(&geo));
        *by.entry(t.region).or_default().entry((s.shape(&geo), s.key, x, o, t.key)).or_default() |= 1u64 << t.local;
        sources.entry(t.region).or_default().push(s.region);
    }
    let mut z = zstd::bulk::Compressor::new(1)?;
    let mut raw = Vec::new();
    let mut segs = Vec::new();
    for (region, m) in by {
        let mut bundles: Vec<Bundle> = m.into_iter().map(|((s_shape, s_key, x, o, t_key), mask)| Bundle { s_shape, s_key, x, o, t_key, mask }).collect();
        segs.push(Segment::encode(region, &mut bundles, &mut raw, &mut z)?);
    }
    Ok((segs, sources.into_iter().collect()))
}

/// A tree's edge files from edges `(source, target, global transfer)` per
/// frame and its transfer table `pairs`: the backward's tests write their
/// graphs with it.
#[cfg(test)]
pub(crate) fn write_test_edges(dir: &Path, frames: &[(u32, Vec<(Sk, Sk, u32)>)], pairs: &[Pair]) -> Result<()> {
    let mut xt = XferTable::default();
    xt.merge(&[pairs]);
    xt.flush(dir)?;
    ensure!(xt.pairs.len() == pairs.len(), "the test's transfer pairs are not distinct");
    let mut last = 0;
    for (frame, edges) in frames {
        let (segs, sources) = segments_of(edges)?;
        write_file(&file_path(dir, *frame, None), *frame, segs, sources)?;
        last = last.max(*frame);
    }
    set_done(dir, last, pairs.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Edge file names past frame 999 (split frame: step 1000 is frame 500).
    #[test]
    fn edge_file_names_give_their_frame_at_any_width() {
        for (frame, raised) in [(7, None), (999, Some(3)), (1000, None), (1234, Some(12))] {
            let p = file_path(Path::new("e"), frame, raised);
            let name = p.file_name().and_then(|s| s.to_str()).expect("a name");
            assert_eq!(frame_of_name(name), Some(frame), "{name}");
        }
        for name in ["done.txt", "xfer.bin", "f.bin", "fx12.bin", "f12x.bin"] {
            assert_eq!(frame_of_name(name), None, "{name}");
        }
        let dir = std::env::temp_dir().join(format!("celeste-edge-names-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("tmp");
        for f in ["f999.bin", "f1000.bin", "f1000.r0001.bin", "f0998.bin", "done.txt"] {
            std::fs::write(dir.join(f), b"").expect("write");
        }
        discard_after(&dir, 999).expect("discard");
        let mut left: Vec<String> = std::fs::read_dir(&dir).expect("ls").map(|e| e.expect("e").file_name().to_string_lossy().into_owned()).collect();
        left.sort();
        std::fs::remove_dir_all(&dir).expect("cleanup");
        assert_eq!(left, ["done.txt", "f0998.bin", "f999.bin"]);
    }

    /// A frame's edges come back from its file: every edge by a scan, a
    /// target's in-edges by `preds_at`, the out-edge index by source region;
    /// a raise's file beside the frame's adds its edges.
    #[test]
    fn a_frames_edges_come_back() {
        let dir = std::env::temp_dir().join(format!("celeste-storage-edges-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let geo = GEOMETRY;
        let cell = |x: i32, y: i32| crate::search::pos_graph::cell_of(x, y).unwrap();
        let st = |shape: u32, k: u64, c: u32| Sk::at(&geo, shape, (0, k), c);
        let a = st(0, 5, cell(3, 4));
        let b = st(0, 5, cell(4, 4));
        let t1 = st(1, 9, cell(5, 4));
        let t2 = st(1, 9, cell(6, 4));
        let far = st(1, 2, cell(100, 40));
        let none = st(0, 7, crate::search::pos_graph::NO_CELL);
        let mut edges = vec![(a, t1, 3), (b, t2, 3), (a, t2, 4), (a, far, 3), (none, t1, 0), (t1, none, 1)];
        let pairs: Vec<Pair> = (0..5)
            .map(|i| {
                let ax = crate::search::arc_edges::AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: i };
                (ax, ax)
            })
            .collect();
        write_test_edges(&dir, &[(4, edges.clone())], &pairs).unwrap();
        // A raise's file: one more edge into t1's region.
        let extra = (st(0, 6, cell(2, 4)), t1, 2);
        let (segs, src) = segments_of(&[extra]).unwrap();
        write_file(&file_path(&dir, 4, Some(1)), 4, segs, src).unwrap();
        edges.push(extra);
        let store = EdgeStore::open(&dir, 4).unwrap();
        assert_eq!(store.edges, edges.len() as u64);
        let mut got = store.edges_at(4, 2).unwrap();
        got.sort_unstable();
        let mut want: Vec<Edge> = edges.iter().map(|&(src, dst, xfer)| Edge { src, dst, xfer }).collect();
        want.sort_unstable();
        assert_eq!(got, want);
        let mut p = store.preds_at(t1, 4).unwrap();
        p.sort_unstable();
        let mut pw = vec![(a, 3), (none, 0), (extra.0, 2)];
        pw.sort_unstable();
        assert_eq!(p, pw);
        assert_eq!(store.targets_of(4, a.region), {
            let mut v = vec![t1.region, far.region];
            v.sort_unstable();
            v.dedup();
            v
        });
        assert_eq!(store.targets_of(4, t1.region), vec![none.region]);
        assert_eq!(store.regions(4).len(), 3);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
