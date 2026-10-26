//! THE RECORDED GRAPH, target-side and ID-free (plans/architecture.md,
//! "The recorded graph"): per frame ONE file `edges/f{frame}.bin`
//! (`storage::file`) of its edges (each leaving a state of layer `f - 1`):
//! a header (the segments by region; per target region the SOURCE regions
//! its edges leave), then per TARGET region its bundles (`storage::bundle`)
//! in SEGMENTS, a wave's slices, each a zstd block. Transfers are global
//! ids into `edges/xfer.bin`, final at each slice's gather
//! (`XferTable::merge`): a function of the search, not of the threads.

use anyhow::{ensure, Context, Result};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::bundle::{self, Bundle};
use super::file::{self, At, Mapped};
use super::Sk;
use crate::search::rem::{Pair, Rem};
#[cfg(test)]
use celeste_core::pico8_num::Pico8Num as P8;

/// The global transfer table: every distinct pair the tree's edges carry,
/// its id its position; `edges/xfer.bin` (`Rem::encode_pair` each), its
/// trusted length the tree record's trust marker's (`record::done`: pairs
/// past it are a killed wave's).
#[derive(Clone)]
pub struct XferTable<N: Rem> {
    pub(crate) pairs: Vec<Pair<N>>,
    index: PairIndex,
    /// Pairs written to `xfer.bin`.
    flushed: usize,
}

impl<N: Rem> Default for XferTable<N> {
    fn default() -> Self {
        XferTable { pairs: Vec::new(), index: PairIndex::default(), flushed: 0 }
    }
}

/// A pair's id by content: open addressing over `u32` slots (`id + 1`; 0
/// empty) of ids into the table's pairs, linear probing, at most 3/4 full:
/// ~6 B a pair (a hash map of pairs: ~37).
#[derive(Default, Clone)]
struct PairIndex {
    slots: Vec<u32>,
    n: usize,
}

impl PairIndex {
    #[inline]
    fn find<N: Rem>(&self, pairs: &[Pair<N>], p: &Pair<N>) -> Option<u32> {
        if self.slots.is_empty() {
            return None;
        }
        let m = self.slots.len() - 1;
        let mut i = N::pair_hash(p) as usize & m;
        loop {
            match self.slots[i] {
                0 => return None,
                s if pairs[s as usize - 1] == *p => return Some(s - 1),
                _ => i = (i + 1) & m,
            }
        }
    }

    /// Pair `id` of `pairs` (absent) indexed.
    fn insert<N: Rem>(&mut self, pairs: &[Pair<N>], id: u32) {
        if (self.n + 1) * 4 > self.slots.len() * 3 {
            let cap = ((self.n + 1) * 2).next_power_of_two().max(16);
            let old = std::mem::replace(&mut self.slots, vec![0; cap]);
            self.n = 0;
            for s in old.into_iter().filter(|&s| s != 0) {
                self.place::<N>(pairs, s - 1);
            }
        }
        self.place::<N>(pairs, id);
    }

    #[inline]
    fn place<N: Rem>(&mut self, pairs: &[Pair<N>], id: u32) {
        let m = self.slots.len() - 1;
        let mut i = N::pair_hash(&pairs[id as usize]) as usize & m;
        while self.slots[i] != 0 {
            i = (i + 1) & m;
        }
        self.slots[i] = id + 1;
        self.n += 1;
    }
}

fn xfer_path(dir: &Path) -> PathBuf {
    dir.join("xfer.bin")
}

impl<N: Rem> XferTable<N> {
    /// The tree's table (`dir`: the level's tree) as its trust marker
    /// trusts it; empty without one.
    pub(crate) fn load(dir: &Path) -> Result<Self> {
        let pairs = Self::load_pairs(dir)?;
        let mut index = PairIndex::default();
        for id in 0..pairs.len() as u32 {
            index.insert::<N>(&pairs, id);
        }
        Ok(XferTable { flushed: pairs.len(), pairs, index })
    }

    /// The pairs alone, as `load` reads them (no index: a big tree has ~4M).
    pub(crate) fn load_pairs(dir: &Path) -> Result<Vec<Pair<N>>> {
        let n = match super::record::done(dir)? {
            Some(d) => d.xfers,
            None => 0,
        };
        let p = xfer_path(&dir.join("edges"));
        let b = if n > 0 { std::fs::read(&p).with_context(|| p.display().to_string())? } else { Vec::new() };
        ensure!(b.len() >= n * N::PAIR_BYTES, "{}: {} pairs, {} trusted", p.display(), b.len() / N::PAIR_BYTES, n);
        Ok(b[..n * N::PAIR_BYTES].chunks_exact(N::PAIR_BYTES).map(N::decode_pair).collect())
    }

    #[inline]
    pub(crate) fn find(&self, p: &Pair<N>) -> Option<u32> {
        self.index.find::<N>(&self.pairs, p)
    }

    /// A wave's new pairs (`fresh`: a table of them alone, its ids less
    /// this one's length) appended, in their order.
    pub(crate) fn append(&mut self, fresh: XferTable<N>) {
        if self.pairs.len() + fresh.pairs.len() > self.pairs.capacity() {
            self.pairs.reserve_exact(fresh.pairs.len().max(self.pairs.len() / 8));
        }
        for p in fresh.pairs {
            debug_assert!(self.find(&p).is_none(), "a fresh pair the table holds");
            self.pairs.push(p);
            self.index.insert::<N>(&self.pairs, self.pairs.len() as u32 - 1);
        }
    }

    /// The workers' NEW pairs (`tails`: what each worker's table gained
    /// since its last merge: a slice's) merged in, in value order; per tail
    /// its pairs' ids. Final: the pairs first met in a slice are a function
    /// of the frame, so their ids are.
    pub(crate) fn merge(&mut self, tails: &[&[Pair<N>]]) -> Vec<Vec<u32>> {
        let mut new: Vec<Pair<N>> = tails.iter().flat_map(|t| t.iter()).filter(|p| self.index.find::<N>(&self.pairs, p).is_none()).copied().collect();
        new.sort_unstable();
        new.dedup();
        // Room by an eighth at least: doubling can hold a whole table spare.
        if self.pairs.len() + new.len() > self.pairs.capacity() {
            self.pairs.reserve_exact(new.len().max(self.pairs.len() / 8));
        }
        for p in new {
            self.pairs.push(p);
            self.index.insert::<N>(&self.pairs, self.pairs.len() as u32 - 1);
        }
        tails.iter().map(|t| t.iter().map(|p| self.index.find::<N>(&self.pairs, p).expect("a merged pair")).collect()).collect()
    }

    /// Write the pairs merged since the last flush past the trusted length
    /// (the frame's done marker trusts them).
    #[cfg(test)]
    pub(crate) fn flush(&mut self, dir: &Path) -> Result<()> {
        self.take_flush().write(dir)
    }

    /// `flush` in two: the pairs not written yet, encoded (taken as
    /// written), and their write (`Flush::write`, on any thread).
    pub(crate) fn take_flush(&mut self) -> Flush {
        let first = self.flushed;
        let mut buf = Vec::with_capacity((self.pairs.len() - first) * N::PAIR_BYTES);
        for p in &self.pairs[first..] {
            N::encode_pair(&mut buf, p);
        }
        self.flushed = self.pairs.len();
        Flush { first, pair_bytes: N::PAIR_BYTES, buf }
    }
}

/// A transfer table's pairs to append (`XferTable::take_flush`).
pub(crate) struct Flush {
    first: usize,
    pair_bytes: usize,
    buf: Vec<u8>,
}

impl Flush {
    /// Appended to `dir`'s table (synced), after the `first` pairs before.
    pub(crate) fn write(self, dir: &Path) -> Result<()> {
        use std::io::{Seek, Write};
        if !self.buf.is_empty() {
            std::fs::create_dir_all(dir)?;
            let mut f = std::fs::OpenOptions::new().create(true).truncate(false).write(true).open(xfer_path(dir))?;
            f.set_len((self.first * self.pair_bytes) as u64)?;
            f.seek(std::io::SeekFrom::Start((self.first * self.pair_bytes) as u64))?;
            f.write_all(&self.buf)?;
            f.sync_data()?;
        }
        Ok(())
    }
}

const MAGIC: &[u8; 4] = b"CSE2";
/// The edge files' own format version (older trees are rebuilt).
pub(crate) const VERSION: u32 = 6;

/// One segment's place in an edge file.
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
struct SegHead {
    region: u32,
    at: At,
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
pub(crate) struct Segment {
    pub(crate) region: u32,
    /// The zstd block, in memory or in the frame's spool.
    pub(crate) bytes: super::scratch::Body,
    pub(crate) raw: u32,
    pub(crate) bundles: u32,
    pub(crate) edges: u64,
}

impl Segment {
    pub(crate) fn of_raw(region: u32, raw: &[u8], bundles: u32, edges: u64, z: &mut zstd::bulk::Compressor<'static>) -> Result<Segment> {
        Ok(Segment { region, bytes: super::scratch::Body::Mem(file::compress(raw, z)?), raw: raw.len() as u32, bundles, edges })
    }

    /// `bundles` (distinct) as a segment of `region` (`enc`: a scratch encoder).
    pub(crate) fn encode(region: u32, bundles: &[Bundle], enc: &mut bundle::Encoder, raw: &mut Vec<u8>, z: &mut zstd::bulk::Compressor<'static>) -> Result<Segment> {
        enc.encode_bundles(bundles, raw);
        Self::of_raw(region, raw, bundles.len() as u32, bundles.iter().map(|b| b.mask.count_ones() as u64).sum(), z)
    }
}

pub(crate) fn file_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join(format!("f{frame:03}.bin"))
}

/// Write frame `frame`'s edge file from its segments and its source-region
/// index (`sources`, per target region; both any order). Atomic. Returns
/// its bytes.
pub(crate) fn write_file(path: &Path, frame: u32, mut segs: Vec<Segment>, mut sources: Vec<(u32, Vec<u32>)>, spool: Option<&super::scratch::Scratch>) -> Result<u64> {
    segs.sort_by_key(|s| s.region);
    sources.sort_unstable_by_key(|s| s.0);
    for s in &mut sources {
        s.1.sort_unstable();
        s.1.dedup();
    }
    let ats = At::place(segs.iter().map(|s| (s.bytes.len(), s.raw as usize)));
    let heads = segs.iter().zip(ats).map(|(s, at)| SegHead { region: s.region, at, bundles: s.bundles, edges: s.edges }).collect();
    file::write(path, *MAGIC, VERSION, &FileHead { frame, segs: heads, sources }, |out| {
        let mut buf = Vec::new();
        for s in &segs {
            s.bytes.with(spool, &mut buf, |b| out.write_all(b))??;
        }
        Ok(())
    })
}

struct EdgeFile {
    file: Mapped,
    head: FileHead,
    /// Per source region the target regions its edges reach (`head.sources`
    /// inverted on first use).
    targets: std::sync::OnceLock<FxHashMap<u32, Vec<u32>>>,
}

impl EdgeFile {
    fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = Mapped::open(path, *MAGIC, VERSION, "an edge file of this format (a tree from before target-region edges: rebuild it)")?;
        let head: FileHead = file.header()?;
        ensure!(head.frame == frame, "{}: the edges of frame {}, not {frame}", path.display(), head.frame);
        ensure!(head.segs.windows(2).all(|w| w[0].region <= w[1].region), "{}: segments not by region", path.display());
        file.ends_at(head.segs.last().map_or(0, |s| s.at.end()))?;
        Ok(EdgeFile { file, head, targets: std::sync::OnceLock::new() })
    }

    fn segs_of(&self, region: u32) -> &[SegHead] {
        let s = &self.head.segs;
        let lo = s.partition_point(|x| x.region < region);
        let hi = lo + s[lo..].partition_point(|x| x.region == region);
        &s[lo..hi]
    }

    fn seg<'a>(&'a self, h: &'a SegHead) -> SegRef<'a> {
        SegRef { region: h.region, bytes: self.file.stored(h.at), raw: h.at.raw as usize, bundles: h.bundles }
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

#[derive(Clone, Copy)]
pub(crate) struct SegRef<'a> {
    pub(crate) region: u32,
    pub(crate) bytes: &'a [u8],
    pub(crate) raw: usize,
    pub(crate) bundles: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Edge {
    pub src: Sk,
    pub dst: Sk,
    pub xfer: u32,
}

/// The edges of bundle `b` of target region `region`: `f(source, target)` each.
#[inline]
pub(crate) fn expand(region: u32, b: &Bundle, mut f: impl FnMut(Sk, Sk)) {
    bundle::edges(b.mask, region % crate::storage::cells::SLOTS, b.o, |c, _, s_cell| f(Sk::at(b.s_shape, b.s_key, s_cell), Sk { region, key: b.t_key, local: c as u8 }));
}

pub(crate) struct Decoder {
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
    pub(crate) fn open<'a>(&'a mut self, s: &SegRef) -> Result<bundle::Block<'a>> {
        file::decompress(s.bytes, s.raw, &mut self.z, &mut self.raw)?;
        bundle::Block::open(&self.raw)
    }

    pub(crate) fn bundles(&mut self, s: &SegRef, f: impl FnMut(Bundle)) -> Result<()> {
        self.open(s)?.bundles(f)
    }
}

/// A level's edge files, mapped: per frame its file (none past a dead
/// frontier, and none at frame 0: nothing leads into it). The forward
/// holds one, a frame added as it is trusted (the re-pushes read it).
#[derive(Default)]
pub(crate) struct EdgeFiles {
    frames: Vec<Option<EdgeFile>>,
}

impl EdgeFiles {
    /// Frames `1..=last`'s files under `dir` (`<level>/edges`).
    pub(crate) fn open(dir: &Path, last: u32) -> Result<Self> {
        let mut files = EdgeFiles { frames: vec![None] };
        for f in 1..=last {
            files.add_frame(dir, f)?;
        }
        Ok(files)
    }

    /// Frame `frame`'s file (the next), if it exists.
    pub(crate) fn add_frame(&mut self, dir: &Path, frame: u32) -> Result<()> {
        if self.frames.is_empty() {
            self.frames.push(None);
        }
        ensure!(self.frames.len() as u32 == frame, "the edge file of frame {frame} added after {} frames", self.frames.len());
        let p = file_path(dir, frame);
        self.frames.push(if p.exists() { Some(EdgeFile::open(&p, frame)?) } else { None });
        Ok(())
    }

    fn file(&self, frame: u32) -> Option<&EdgeFile> {
        self.frames.get(frame as usize).and_then(|x| x.as_ref())
    }

    /// Target region `region`'s segments at frame `frame`.
    pub(crate) fn segments(&self, frame: u32, region: u32) -> Vec<SegRef<'_>> {
        self.file(frame).into_iter().flat_map(|f| f.segs_of(region).iter().map(move |h| f.seg(h))).collect()
    }

    /// The target regions holding edges that leave source region `source`
    /// at frame `frame` (the out-edge lookup), sorted, distinct.
    pub(crate) fn targets_of(&self, frame: u32, source: u32) -> Vec<u32> {
        let mut v: Vec<u32> = self.file(frame).into_iter().flat_map(|f| f.targets().get(&source).into_iter().flatten().copied()).collect();
        v.sort_unstable();
        v.dedup();
        v
    }
}

/// A level's recorded graph up to a horizon, mapped: its edge files and the
/// global transfer table.
pub struct EdgeStore<N: Rem> {
    files: EdgeFiles,
    xfers: Vec<Pair<N>>,
    pub edges: u64,
    pub(crate) bytes: u64,
}

impl<N: Rem> EdgeStore<N> {
    /// Map frames `1..=horizon`'s edge files of the tree in `dir` (a
    /// level's), its transfers as its record trusts them.
    pub fn open(dir: &Path, horizon: u32) -> Result<Self> {
        let files = EdgeFiles::open(&dir.join("edges"), horizon)?;
        let (mut edges, mut bytes) = (0u64, 0u64);
        for file in files.frames.iter().flatten() {
            edges += file.head.segs.iter().map(|s| s.edges).sum::<u64>();
            bytes += file.file.len();
        }
        let xfers = XferTable::<N>::load_pairs(dir)?;
        Ok(EdgeStore { files, xfers, edges, bytes })
    }

    fn file(&self, frame: u32) -> Option<&EdgeFile> {
        self.files.file(frame)
    }

    pub fn pair(&self, xfer: u32) -> Option<Pair<N>> {
        self.xfers.get(xfer as usize).copied()
    }

    pub(crate) fn pairs(&self) -> &[Pair<N>] {
        &self.xfers
    }

    pub(crate) fn has_frame(&self, frame: u32) -> bool {
        self.file(frame).is_some()
    }

    /// Per target region the first frame with edges into it.
    pub(crate) fn first_frames(&self) -> FxHashMap<u32, u32> {
        let mut m: FxHashMap<u32, u32> = FxHashMap::default();
        for (f, file) in self.files.frames.iter().enumerate().rev() {
            for h in file.iter().flat_map(|x| &x.head.segs) {
                m.insert(h.region, f as u32);
            }
        }
        m
    }

    /// The target regions with edges at frame `frame`, sorted, distinct.
    #[cfg(test)]
    pub(crate) fn regions(&self, frame: u32) -> Vec<u32> {
        let mut v: Vec<u32> = self.file(frame).into_iter().flat_map(|f| f.head.segs.iter().map(|s| s.region)).collect();
        v.sort_unstable();
        v.dedup();
        v
    }

    pub(crate) fn segments(&self, frame: u32, region: u32) -> Vec<SegRef<'_>> {
        self.files.segments(frame, region)
    }

    /// Every segment of frame `frame`, by region.
    pub(crate) fn all_segments(&self, frame: u32) -> Vec<SegRef<'_>> {
        let mut v: Vec<SegRef> = self.file(frame).into_iter().flat_map(|f| f.head.segs.iter().map(move |h| f.seg(h))).collect();
        v.sort_by_key(|s| s.region);
        v
    }

    /// The target regions holding edges that leave source region `source`
    /// at frame `frame` (the out-edge lookup), sorted, distinct.
    pub(crate) fn targets_of(&self, frame: u32, source: u32) -> Vec<u32> {
        self.files.targets_of(frame, source)
    }

    /// Every edge of frame `frame`, each segment's folded into an
    /// accumulator of its own (`init()`); the accumulators, by segment.
    pub(crate) fn par_scan<A: Send>(&self, frame: u32, threads: usize, init: impl Fn() -> A + Sync, f: impl Fn(&mut A, Edge) + Sync) -> Result<Vec<A>> {
        self.par_blocks(frame, threads, init, |acc, region, blk| blk.bundles(|b| expand(region, &b, |src, dst| f(acc, Edge { src, dst, xfer: b.x }))))
    }

    /// Every segment of frame `frame` opened and folded (`f(acc, target
    /// region, block)`) into an accumulator of its own; by segment.
    pub(crate) fn par_blocks<A: Send>(&self, frame: u32, threads: usize, init: impl Fn() -> A + Sync, f: impl Fn(&mut A, u32, &bundle::Block) -> Result<()> + Sync) -> Result<Vec<A>> {
        let segs = self.all_segments(frame);
        crate::par::Pool::new(threads).map_with(
            segs.len(),
            || Ok(Decoder::default()),
            |dec, k| {
                let mut acc = init();
                let blk = dec.open(&segs[k])?;
                f(&mut acc, segs[k].region, &blk)?;
                Ok(acc)
            },
        )
    }

    /// The distinct `(source cell, target cell)` pairs of frame `frame`'s
    /// edges (`Sk::cell`: the player's position cell, `NO_CELL` without one).
    pub(crate) fn cell_pairs(&self, frame: u32, threads: usize) -> Result<FxHashSet<(u32, u32)>> {
        let mut all = FxHashSet::default();
        for part in self.par_scan(frame, threads, FxHashSet::default, |acc, e| {
            acc.insert((e.src.cell(), e.dst.cell()));
        })? {
            all.extend(part);
        }
        Ok(all)
    }

    /// Every edge of frame `frame`, as a list, sorted.
    pub fn edges_at(&self, frame: u32, threads: usize) -> Result<Vec<Edge>> {
        let mut v: Vec<Edge> = self.par_scan(frame, threads, Vec::new, |v, e| v.push(e))?.into_iter().flatten().collect();
        v.sort_unstable();
        Ok(v)
    }

    /// THE BUNDLES recorded at frame `frame` into target region `region`
    /// (its segments decoded with `dec`): `f` each (`expand`: its edges).
    pub(crate) fn bundles_into(&self, frame: u32, region: u32, dec: &mut Decoder, mut f: impl FnMut(Bundle)) -> Result<()> {
        for s in self.segments(frame, region) {
            dec.bundles(&s, &mut f)?;
        }
        Ok(())
    }

    /// The in-edges recorded at frame `frame` of `target` (sources of layer
    /// `frame - 1`).
    pub(crate) fn preds_at(&self, target: Sk, frame: u32) -> Result<Vec<(Sk, u32)>> {
        let mut out = Vec::new();
        self.bundles_into(frame, target.region, &mut Decoder::default(), |b| {
            if b.t_key == target.key {
                bundle::edges(b.mask & 1u64 << target.local, target.region % crate::storage::cells::SLOTS, b.o, |_, _, s_cell| out.push((Sk::at(b.s_shape, b.s_key, s_cell), b.x)));
            }
        })?;
        Ok(out)
    }

    /// The OUT-EDGES recorded at frame `frame` of `sources` (layer `frame -
    /// 1`): `(k, target, transfer)` for `sources[k]`, read from the target
    /// regions the frame's source index names for the sources' regions.
    pub(crate) fn out_of(&self, frame: u32, sources: &[Sk], threads: usize) -> Result<Vec<(usize, Sk, u32)>> {
        let mut wanted: rustc_hash2::FxHashMap<(u32, super::visited::Key), Vec<(u32, usize)>> = Default::default();
        let mut regions: Vec<u32> = Vec::new();
        for (k, s) in sources.iter().enumerate() {
            wanted.entry((s.shape(), s.key)).or_default().push((s.cell(), k));
            regions.push(s.region);
        }
        regions.sort_unstable();
        regions.dedup();
        let mut targets: Vec<u32> = regions.iter().flat_map(|&r| self.targets_of(frame, r)).collect();
        targets.sort_unstable();
        targets.dedup();
        let parts = crate::par::map(&targets, threads, |&region| -> Result<Vec<(usize, Sk, u32)>> {
            let mut out = Vec::new();
            self.bundles_into(frame, region, &mut Decoder::default(), |b| {
                let Some(cands) = wanted.get(&(b.s_shape, b.s_key)) else { return };
                expand(region, &b, |src, dst| {
                    let c = src.cell();
                    for &(wc, k) in cands {
                        if wc == c {
                            out.push((k, dst, b.x));
                        }
                    }
                });
            })?;
            Ok(out)
        });
        let mut out = Vec::new();
        for p in parts {
            out.extend(p?);
        }
        Ok(out)
    }
}

/// The frame an edge file's name is of (`f{frame}.bin`): the digits after
/// the `f`, however many (the names pad to 3: `f1000.bin` is frame 1000).
pub(crate) fn frame_of_name(name: &str) -> Option<u32> {
    let digits = name.strip_prefix('f')?;
    let end = digits.find(|c: char| !c.is_ascii_digit()).unwrap_or(digits.len());
    if end == 0 || !digits[end..].starts_with('.') {
        return None;
    }
    digits[..end].parse().ok()
}

/// Drop what a run past `last` (the last complete frame) left: every edge
/// file of a later frame and temporary files. Transfers past the trusted
/// length are overwritten by the next flush.
pub(crate) fn discard_after(dir: &Path, last: u32) -> Result<()> {
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
#[cfg(test)]
pub(crate) fn segments_of(edges: &[(Sk, Sk, u32)]) -> Result<(Vec<Segment>, Vec<(u32, Vec<u32>)>)> {
    let mut by: std::collections::BTreeMap<u32, rustc_hash2::FxHashMap<(u32, super::visited::Key, u32, u32, super::visited::Key), u64>> = Default::default();
    let mut sources: std::collections::BTreeMap<u32, Vec<u32>> = Default::default();
    for &(s, t, x) in edges {
        let o = bundle::offset(s.cell(), t.cell());
        *by.entry(t.region).or_default().entry((s.shape(), s.key, x, o, t.key)).or_default() |= 1u64 << t.local;
        sources.entry(t.region).or_default().push(s.region);
    }
    let mut z = super::file::compressor()?;
    let mut raw = Vec::new();
    let mut enc = bundle::Encoder::default();
    let mut segs = Vec::new();
    for (region, m) in by {
        let bundles: Vec<Bundle> = m.into_iter().map(|((s_shape, s_key, x, o, t_key), mask)| Bundle { s_shape, s_key, x, o, t_key, mask }).collect();
        segs.push(Segment::encode(region, &bundles, &mut enc, &mut raw, &mut z)?);
    }
    Ok((segs, sources.into_iter().collect()))
}

/// A tree's edge files from edges `(source, target, global transfer)` per
/// frame and its transfer table `pairs`, under the level's tree `dir`, its
/// record trusting them (the backward's tests).
#[cfg(test)]
pub(crate) fn write_test_edges(dir: &Path, frames: &[(u32, Vec<(Sk, Sk, u32)>)], pairs: &[Pair<P8>]) -> Result<()> {
    let edges = dir.join("edges");
    let mut xt = XferTable::<P8>::default();
    xt.merge(&[pairs]);
    xt.flush(&edges)?;
    ensure!(xt.pairs.len() == pairs.len(), "the test's transfer pairs are not distinct");
    let mut last = 0;
    for (frame, es) in frames {
        let (segs, sources) = segments_of(es)?;
        write_file(&file_path(&edges, *frame), *frame, segs, sources, None)?;
        last = last.max(*frame);
    }
    super::record::set_done(dir, last, pairs.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A slice's new transfers get their final ids at its merge, in value
    /// order after every pair before; a pair held keeps its id; the ids
    /// returned name the pairs, and the index finds them.
    #[test]
    fn a_slices_new_transfers_get_final_ids_in_value_order() {
        let pair = |v: i32| {
            let ax = crate::search::rem::AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: v };
            (ax, ax)
        };
        let mut t = XferTable::<P8>::default();
        t.merge(&[&[pair(5), pair(1)]]);
        // Two slices: the second meets smaller pairs (and one already held).
        let ids_a = t.merge(&[&[pair(9), pair(7)], &[pair(8)]]);
        let ids_b = t.merge(&[&[pair(3), pair(5)]]);
        assert_eq!(t.pairs, [1, 5, 7, 8, 9, 3].map(pair));
        for (p, x) in [pair(9), pair(7), pair(8), pair(3), pair(5)].iter().zip(ids_a.concat().into_iter().chain(ids_b.concat())) {
            assert_eq!(t.pairs[x as usize], *p);
            assert_eq!(t.index.find::<P8>(&t.pairs, p), Some(x));
        }
    }

    /// Edge file names past frame 999 (split frame: step 1000 is frame 500).
    #[test]
    fn edge_file_names_give_their_frame_at_any_width() {
        for frame in [7, 999, 1000, 1234] {
            let p = file_path(Path::new("e"), frame);
            let name = p.file_name().and_then(|s| s.to_str()).expect("a name");
            assert_eq!(frame_of_name(name), Some(frame), "{name}");
        }
        for name in ["xfer.bin", "f.bin", "fx12.bin", "f12x.bin"] {
            assert_eq!(frame_of_name(name), None, "{name}");
        }
        let dir = crate::testutil::TestDir::new("edge-names");
        std::fs::create_dir_all(&dir).expect("tmp");
        for f in ["f999.bin", "f1000.bin", "f1000.tmp", "f0998.bin", "xfer.bin"] {
            std::fs::write(dir.join(f), b"").expect("write");
        }
        discard_after(&dir, 999).expect("discard");
        let mut left: Vec<String> = std::fs::read_dir(&dir).expect("ls").map(|e| e.expect("e").file_name().to_string_lossy().into_owned()).collect();
        left.sort();
        assert_eq!(left, ["f0998.bin", "f999.bin", "xfer.bin"]);
    }

    /// A frame's edges come back from its file: every edge by a scan, a
    /// target's in-edges by `preds_at`, the out-edge index by source region.
    #[test]
    fn a_frames_edges_come_back() {
        let dir = crate::testutil::TestDir::new("storage-edges");
        let cell = |x: i32, y: i32| crate::storage::cells::cell_of(x, y).unwrap();
        let st = |shape: u32, k: u64, c: u32| Sk::at(shape, (0, k), c);
        let a = st(0, 5, cell(3, 4));
        let b = st(0, 5, cell(4, 4));
        let t1 = st(1, 9, cell(5, 4));
        let t2 = st(1, 9, cell(6, 4));
        let far = st(1, 2, cell(100, 40));
        let none = st(0, 7, crate::storage::cells::NO_CELL);
        let extra = (st(0, 6, cell(2, 4)), t1, 2);
        let edges = vec![(a, t1, 3), (b, t2, 3), (a, t2, 4), (a, far, 3), (none, t1, 0), (t1, none, 1), extra];
        let pairs: Vec<Pair<P8>> = (0..5)
            .map(|i| {
                let ax = crate::search::rem::AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: i };
                (ax, ax)
            })
            .collect();
        write_test_edges(&dir, &[(4, edges.clone())], &pairs).unwrap();
        let store = EdgeStore::<P8>::open(&dir, 4).unwrap();
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
    }
}
