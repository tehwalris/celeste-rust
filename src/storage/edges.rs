//! THE RECORDED GRAPH, source-side (plans/storage-v2.md "The edge file").
//!
//! Every edge of frame `f` leaves a state of layer `f - 1` (a frontier row)
//! and is recorded by the unit that ran it, in the unit's BLOCK: its edges
//! `(target lid, target cell, source, transfer)` sorted by target and
//! encoded (`encode_block`), the lids the unit's own names for target
//! entries. The unit's TRANSLATION TABLE gives each lid its owner (region,
//! entry), by lid and sorted by owner - the reverse walk. Per frame ONE
//! file `edges/f{frame}.bin` (a raise adds `f{frame}.r{seq}.bin`); the
//! transfers are global ids into `edges/xfer.bin`, content-canonical (each
//! wave appends its new pairs sorted), through a per-worker remap in the
//! file. Nothing is ever inverted: `EdgeStore::preds_at` finds a target's
//! in-edges through the units naming its region and their owner tables.

use anyhow::{ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::unit::UnitOut;
use super::{id_entry, id_local, id_region, state_id, StateId};
use crate::search::arc_edges::{decode_pair, encode_pair, Pair, PAIR_BYTES};

/// Edges per index entry of a block.
const STRIDE: usize = 256;

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

/// A unit's edges (`unit::pack_edge`, sorted, distinct) as its block: per
/// edge varints - a HEAD, `delta << 1 | 1` at a new target (the target code
/// `lid << 8 | cell` as a delta, then the source in full) or `delta << 1`
/// (the source's delta, same target) - then the transfer (the worker's id).
/// An index entry `(target code, offset)` every `STRIDE` edges, where the
/// deltas restart.
pub fn encode_block(edges: &[u64]) -> (Vec<u8>, Vec<(u32, u32)>) {
    let mut out = Vec::with_capacity(edges.len() * 3);
    let mut index = Vec::with_capacity(edges.len() / STRIDE + 1);
    let (mut prev_t, mut prev_s) = (0u32, 0u32);
    for (k, &e) in edges.iter().enumerate() {
        let (lid, local, src, x) = super::unit::unpack_edge(e);
        let t = lid << 8 | local;
        let fresh = k % STRIDE == 0;
        if fresh {
            assert!(out.len() < u32::MAX as usize, "a unit block past 4 GB");
            index.push((t, out.len() as u32));
            prev_t = t;
        }
        if fresh || t != prev_t {
            put_varint(&mut out, ((t - prev_t) as u64) << 1 | 1);
            put_varint(&mut out, src as u64);
        } else {
            put_varint(&mut out, ((src - prev_s) as u64) << 1);
        }
        put_varint(&mut out, x as u64);
        prev_t = t;
        prev_s = src;
    }
    (out, index)
}

/// A block's index: `(target code, offset)` entries, in memory or mapped.
pub trait BlockIndex {
    fn len(&self) -> usize;
    fn get(&self, k: usize) -> (u32, u32);
}

impl BlockIndex for [(u32, u32)] {
    fn len(&self) -> usize {
        <[(u32, u32)]>::len(self)
    }
    fn get(&self, k: usize) -> (u32, u32) {
        self[k]
    }
}

/// Decode a block from index entry `blk` on: `f(target code, source,
/// transfer)` per edge until `f` returns false.
fn decode_block<I: BlockIndex + ?Sized>(b: &[u8], index: &I, mut blk: usize, mut f: impl FnMut(u32, u32, u32) -> bool) {
    if blk >= index.len() {
        return;
    }
    let (mut prev_t, pos) = index.get(blk);
    let mut pos = pos as usize;
    let next = |k: usize| if k < index.len() { index.get(k).1 as usize } else { usize::MAX };
    let mut next_at = next(blk + 1);
    let mut prev_s = 0u32;
    while pos < b.len() {
        if pos >= next_at {
            blk += 1;
            prev_t = index.get(blk).0;
            next_at = next(blk + 1);
        }
        let head = get_varint(b, &mut pos);
        let (t, s) = if head & 1 == 1 { (prev_t + (head >> 1) as u32, get_varint(b, &mut pos) as u32) } else { (prev_t, prev_s + (head >> 1) as u32) };
        let x = get_varint(b, &mut pos) as u32;
        prev_t = t;
        prev_s = s;
        if !f(t, s, x) {
            return;
        }
    }
}

/// The global transfer table: every distinct pair the tree's edges carry,
/// a pair's id its position. `edges/xfer.bin` (`encode_pair` each), its
/// trusted length in `edges/xfer.len` (pairs past it are a killed wave's).
#[derive(Default, Clone)]
pub struct XferTable {
    pub pairs: Vec<Pair>,
    index: FxHashMap<Pair, u32>,
}

fn xfer_path(dir: &Path) -> PathBuf {
    dir.join("xfer.bin")
}

fn xfer_len_path(dir: &Path) -> PathBuf {
    dir.join("xfer.len")
}

impl XferTable {
    /// The tree's table (`dir` = `<level>/edges`); empty without one.
    pub fn load(dir: &Path) -> Result<Self> {
        let n: usize = match std::fs::read_to_string(xfer_len_path(dir)) {
            Ok(s) => s.trim().parse().with_context(|| xfer_len_path(dir).display().to_string())?,
            Err(_) => 0,
        };
        let b = if n > 0 { std::fs::read(xfer_path(dir)).with_context(|| xfer_path(dir).display().to_string())? } else { Vec::new() };
        ensure!(b.len() >= n * PAIR_BYTES, "{}: {} pairs, {} trusted", xfer_path(dir).display(), b.len() / PAIR_BYTES, n);
        let pairs: Vec<Pair> = b[..n * PAIR_BYTES].chunks_exact(PAIR_BYTES).map(decode_pair).collect();
        let index = pairs.iter().enumerate().map(|(i, p)| (*p, i as u32)).collect();
        Ok(XferTable { pairs, index })
    }

    /// The workers' tables into the global one: new pairs appended in value
    /// order (content-canonical), and written (`dir`); per worker, its ids'
    /// global ones.
    pub fn merge(&mut self, dir: Option<&Path>, tables: &[Vec<Pair>]) -> Result<Vec<Vec<u32>>> {
        let mut new: Vec<Pair> = tables.iter().flatten().filter(|p| !self.index.contains_key(p)).copied().collect();
        new.sort_unstable();
        new.dedup();
        let first = self.pairs.len();
        for p in new {
            self.index.insert(p, self.pairs.len() as u32);
            self.pairs.push(p);
        }
        if let (Some(dir), true) = (dir, self.pairs.len() > first) {
            use std::io::{Seek, Write};
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
            let tmp = dir.join("xfer.len.tmp");
            std::fs::write(&tmp, format!("{}\n", self.pairs.len()))?;
            std::fs::rename(&tmp, xfer_len_path(dir))?;
        }
        Ok(tables.iter().map(|t| t.iter().map(|p| self.index[p]).collect()).collect())
    }
}

const MAGIC: &[u8; 4] = b"CSE1";
const VERSION: u32 = 2;

/// The blocks file a wave's worker streams its units' blocks into, beside
/// the frame's index file `index` (`f{frame}.bin` -> `f{frame}.w{worker}.blk`).
pub fn blocks_path(index: &Path, worker: u32) -> PathBuf {
    let name = index.file_name().and_then(|s| s.to_str()).expect("an edge file name");
    index.with_file_name(format!("{}.w{worker:03}.blk", name.strip_suffix(".bin").expect("an edge index is a .bin")))
}

/// A worker's blocks file, appended unit by unit during the wave.
pub struct BlockWriter {
    out: std::io::BufWriter<std::fs::File>,
    at: u64,
}

impl BlockWriter {
    pub fn create(path: &Path) -> Result<Self> {
        std::fs::create_dir_all(path.parent().expect("an edges dir"))?;
        Ok(BlockWriter { out: std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(path).with_context(|| path.display().to_string())?), at: 0 })
    }

    /// Append a block; its offset.
    pub fn append(&mut self, b: &[u8]) -> Result<u64> {
        use std::io::Write;
        self.out.write_all(b)?;
        let at = self.at;
        self.at += b.len() as u64;
        Ok(at)
    }

    pub fn finish(mut self) -> Result<()> {
        use std::io::Write;
        self.out.flush()?;
        Ok(())
    }
}

/// One unit's place in an edge file (offsets into its data region).
#[derive(Serialize, Deserialize, Clone, Debug)]
struct UnitHead {
    worker: u32,
    /// `n_sources` source ids, u64 each.
    sources: u64,
    n_sources: u32,
    /// The block: in the worker's blocks file (`blocks_path`), or with
    /// `inline`, in this file's data region.
    inline: bool,
    block: u64,
    block_len: u64,
    edges: u64,
    /// `(target code, offset)` pairs, u32 each.
    index: u64,
    n_index: u32,
    /// Per lid its owner `(region, entry)` (u32 each; `unit::NONE`: unused).
    lids: u64,
    n_lids: u32,
    /// `(region, entry, lid)` (u32 each) sorted: the owners named.
    owners: u64,
    n_owners: u32,
}

#[derive(Serialize, Deserialize)]
struct FileHead {
    frame: u32,
    units: Vec<UnitHead>,
    /// `(region, unit)`: the units naming an entry of the region, sorted.
    region_units: Vec<(u32, u32)>,
    /// Per worker, its transfer ids' global ones.
    remaps: Vec<Vec<u32>>,
}

/// The edge file of frame `frame` (`raised`: a raise's, by its first seq).
pub fn file_path(dir: &Path, frame: u32, raised: Option<u32>) -> PathBuf {
    match raised {
        None => dir.join(format!("f{frame:03}.bin")),
        Some(seq) => dir.join(format!("f{frame:03}.r{seq:04}.bin")),
    }
}

/// One unit's sections of an edge file, built apart (in parallel).
struct UnitBytes {
    sources: Vec<u8>,
    index: Vec<u8>,
    lids: Vec<u8>,
    owners: Vec<u8>,
    n_owners: u32,
    regions: Vec<u32>,
}

fn le32(v: &mut Vec<u8>, x: u32) {
    v.extend_from_slice(&x.to_le_bytes());
}

/// Unit `u`'s sections: its sources, index, lid owners by lid and sorted by
/// owner, the regions it names.
fn unit_bytes(u: &UnitOut, frame: u32) -> Result<UnitBytes> {
    let mut sources = Vec::with_capacity(8 * u.sources.len());
    for s in &u.sources {
        sources.extend_from_slice(&s.to_le_bytes());
    }
    let mut index = Vec::with_capacity(8 * u.index.len());
    for &(t, o) in &u.index {
        le32(&mut index, t);
        le32(&mut index, o);
    }
    let mut lids = Vec::with_capacity(8 * u.lids.len());
    let mut owned: Vec<(u32, u32, u32)> = Vec::new();
    for (l, o) in u.owners.iter().enumerate() {
        let (r, e) = match o.load(std::sync::atomic::Ordering::Relaxed) {
            super::unit::NO_OWNER => (super::unit::NONE, super::unit::NONE),
            o => {
                owned.push(((o >> 32) as u32, o as u32, l as u32));
                ((o >> 32) as u32, o as u32)
            }
        };
        le32(&mut lids, r);
        le32(&mut lids, e);
    }
    owned.sort_unstable();
    ensure!(owned.windows(2).all(|w| (w[0].0, w[0].1) != (w[1].0, w[1].1)), "frame {frame}: a unit names one entry by two lids");
    let mut owners = Vec::with_capacity(12 * owned.len());
    let mut regions: Vec<u32> = Vec::new();
    for &(r, e, l) in &owned {
        le32(&mut owners, r);
        le32(&mut owners, e);
        le32(&mut owners, l);
        if regions.last() != Some(&r) {
            regions.push(r);
        }
    }
    Ok(UnitBytes { sources, index, lids, owners, n_owners: owned.len() as u32, regions })
}

/// Write frame `frame`'s edge file from its units (owners resolved) and
/// the workers' transfer remaps: the sections built per unit and written at
/// their offsets, in parallel. Atomic (renamed into place). Returns its bytes.
pub fn write_file(path: &Path, frame: u32, outs: &[UnitOut], remaps: Vec<Vec<u32>>) -> Result<u64> {
    use std::os::unix::fs::FileExt;
    let threads = crate::frame::threads();
    let bytes: Vec<UnitBytes> = super::wave::par_map(outs, threads, |u| unit_bytes(u, frame)).into_iter().collect::<Result<_>>()?;
    // The data region's layout: per unit its sections, in unit order.
    let mut units = Vec::with_capacity(outs.len());
    let mut region_units: Vec<(u32, u32)> = Vec::new();
    let mut at = 0u64;
    let mut place = |n: usize| -> u64 {
        let o = at;
        at += n as u64;
        o
    };
    for (ui, (u, b)) in outs.iter().zip(&bytes).enumerate() {
        units.push(UnitHead {
            worker: u.worker,
            sources: place(b.sources.len()),
            n_sources: u.sources.len() as u32,
            inline: u.block_at.is_none(),
            block: match u.block_at {
                Some(at) => at,
                None => place(u.block.len()),
            },
            block_len: u.block_len,
            edges: u.edges,
            index: place(b.index.len()),
            n_index: u.index.len() as u32,
            lids: place(b.lids.len()),
            n_lids: u.lids.len() as u32,
            owners: place(b.owners.len()),
            n_owners: b.n_owners,
        });
        region_units.extend(b.regions.iter().map(|&r| (r, ui as u32)));
    }
    region_units.sort_unstable();
    let head = bincode::serialize(&FileHead { frame, units, region_units, remaps }).context("serializing an edge file header")?;
    let base = 16 + head.len() as u64;
    std::fs::create_dir_all(path.parent().expect("an edges dir"))?;
    let tmp = path.with_extension("tmp");
    let file = std::fs::File::create(&tmp)?;
    let mut prefix = Vec::with_capacity(16 + head.len());
    prefix.extend_from_slice(MAGIC);
    prefix.extend_from_slice(&VERSION.to_le_bytes());
    prefix.extend_from_slice(&(head.len() as u64).to_le_bytes());
    prefix.extend_from_slice(&head);
    file.write_all_at(&prefix, 0)?;
    let head: FileHead = bincode::deserialize(&head).context("the header just written")?;
    let order: Vec<usize> = (0..outs.len()).collect();
    super::wave::par_map(&order, threads, |&k| -> Result<()> {
        let (h, b, u) = (&head.units[k], &bytes[k], &outs[k]);
        file.write_all_at(&b.sources, base + h.sources)?;
        if h.inline {
            file.write_all_at(&u.block, base + h.block)?;
        }
        file.write_all_at(&b.index, base + h.index)?;
        file.write_all_at(&b.lids, base + h.lids)?;
        file.write_all_at(&b.owners, base + h.owners)?;
        Ok(())
    })
    .into_iter()
    .collect::<Result<()>>()?;
    drop(file);
    std::fs::rename(&tmp, path)?;
    Ok(base + at)
}

/// One edge file, mapped, with its workers' blocks files.
struct EdgeFile {
    map: memmap2::Mmap,
    head: FileHead,
    data: usize,
    /// Per worker its blocks file, where a unit's block is there.
    blocks: Vec<Option<memmap2::Mmap>>,
}

impl EdgeFile {
    fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = std::fs::File::open(path).with_context(|| path.display().to_string())?;
        // SAFETY: edge files are written once (renamed into place) and
        // never modified afterwards.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 16 && &map[0..4] == MAGIC, "{}: not an edge file", path.display());
        let v = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(v == VERSION, "{}: edge file version {v}, expected {VERSION}", path.display());
        let n = u64::from_le_bytes(map[8..16].try_into().unwrap()) as usize;
        ensure!(map.len() >= 16 + n, "{}: truncated header", path.display());
        let head: FileHead = bincode::deserialize(&map[16..16 + n]).with_context(|| path.display().to_string())?;
        ensure!(head.frame == frame, "{}: the edges of frame {}, not {frame}", path.display(), head.frame);
        let mut blocks: Vec<Option<memmap2::Mmap>> = Vec::new();
        for u in head.units.iter().filter(|u| !u.inline) {
            let w = u.worker as usize;
            if blocks.len() <= w {
                blocks.resize_with(w + 1, || None);
            }
            if blocks[w].is_none() {
                let p = blocks_path(path, u.worker);
                let f = std::fs::File::open(&p).with_context(|| p.display().to_string())?;
                // SAFETY: a blocks file is complete once its frame's index
                // is in place, and never modified afterwards.
                blocks[w] = Some(unsafe { memmap2::Mmap::map(&f)? });
            }
            let len = blocks[w].as_ref().expect("mapped").len() as u64;
            ensure!(u.block + u.block_len <= len, "{}: a unit's block past its blocks file", path.display());
        }
        Ok(EdgeFile { map, head, data: 16 + n, blocks })
    }

    #[inline]
    fn u32_at(&self, off: u64, k: usize) -> u32 {
        let o = self.data + off as usize + 4 * k;
        u32::from_le_bytes(self.map[o..o + 4].try_into().unwrap())
    }

    #[inline]
    fn source(&self, u: &UnitHead, s: u32) -> StateId {
        let o = self.data + u.sources as usize + 8 * s as usize;
        u64::from_le_bytes(self.map[o..o + 8].try_into().unwrap())
    }

    fn block<'b>(&'b self, u: &'b UnitHead) -> (&'b [u8], MappedIndex<'b>) {
        let b = if u.inline {
            &self.map[self.data + u.block as usize..self.data + (u.block + u.block_len) as usize]
        } else {
            &self.blocks[u.worker as usize].as_ref().expect("mapped at open")[u.block as usize..(u.block + u.block_len) as usize]
        };
        (b, MappedIndex { file: self, u })
    }

    /// Unit `u`'s lid naming `(region, entry)`.
    fn lid_of(&self, u: &UnitHead, region: u32, entry: u32) -> Option<u32> {
        let (mut lo, mut hi) = (0usize, u.n_owners as usize);
        while lo < hi {
            let mid = (lo + hi) / 2;
            let k = (self.u32_at(u.owners, 3 * mid), self.u32_at(u.owners, 3 * mid + 1));
            match k.cmp(&(region, entry)) {
                std::cmp::Ordering::Less => lo = mid + 1,
                std::cmp::Ordering::Greater => hi = mid,
                std::cmp::Ordering::Equal => return Some(self.u32_at(u.owners, 3 * mid + 2)),
            }
        }
        None
    }

    /// The units naming an entry of `region`.
    fn units_of(&self, region: u32) -> &[(u32, u32)] {
        let ru = &self.head.region_units;
        let lo = ru.partition_point(|e| e.0 < region);
        let hi = ru.partition_point(|e| e.0 <= region);
        &ru[lo..hi]
    }
}

/// A unit's block index, read in place.
struct MappedIndex<'a> {
    file: &'a EdgeFile,
    u: &'a UnitHead,
}

impl BlockIndex for MappedIndex<'_> {
    fn len(&self) -> usize {
        self.u.n_index as usize
    }
    fn get(&self, k: usize) -> (u32, u32) {
        (self.file.u32_at(self.u.index, 2 * k), self.file.u32_at(self.u.index, 2 * k + 1))
    }
}

/// One in-edge: the source and the global transfer id.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InEdge {
    pub src: StateId,
    pub xfer: u32,
}

/// One edge of a frame: source, target, global transfer id.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Edge {
    pub src: StateId,
    pub dst: StateId,
    pub xfer: u32,
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
        for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
            let p = e.path();
            let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
            let Some(f) = n.strip_prefix('f').and_then(|s| s.get(..3)).and_then(|s| s.parse::<u32>().ok()) else { continue };
            if !n.ends_with(".bin") || f == 0 || f > horizon {
                continue;
            }
            let file = EdgeFile::open(&p, f)?;
            edges += file.head.units.iter().map(|u| u.edges).sum::<u64>();
            bytes += file.map.len() as u64 + file.blocks.iter().flatten().map(|m| m.len() as u64).sum::<u64>();
            frames[f as usize].push(file);
        }
        let xfers = XferTable::load(dir)?.pairs;
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

    /// The in-edges of `target` recorded at frame `frame` (sources of layer
    /// `frame - 1`), appended to `out`: through the units naming the
    /// target's region, each one's lid of its entry, the block's edges into
    /// (lid, cell).
    pub fn preds_at(&self, target: StateId, frame: u32, out: &mut Vec<InEdge>) {
        let (region, entry, local) = (id_region(target), id_entry(target), id_local(target));
        for file in self.frames.get(frame as usize).into_iter().flatten() {
            for &(_, ui) in file.units_of(region) {
                let u = &file.head.units[ui as usize];
                let Some(lid) = file.lid_of(u, region, entry) else { continue };
                let code = lid << 8 | local;
                let (b, index) = file.block(u);
                // The last index entry starting below the code (or the next).
                let (mut lo, mut hi) = (0usize, index.len());
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    if index.get(mid).0 < code {
                        lo = mid + 1;
                    } else {
                        hi = mid;
                    }
                }
                let k = lo;
                let remap = &file.head.remaps[u.worker as usize];
                decode_block(b, &index, k.saturating_sub(1), |t, s, x| {
                    if t == code {
                        out.push(InEdge { src: file.source(u, s), xfer: remap[x as usize] });
                    }
                    t <= code
                });
            }
        }
    }

    /// Every edge recorded at frame `frame`, unit by unit.
    pub fn scan(&self, frame: u32, mut f: impl FnMut(Edge)) {
        for file in self.frames.get(frame as usize).into_iter().flatten() {
            for u in &file.head.units {
                let remap = &file.head.remaps[u.worker as usize];
                let (b, index) = file.block(u);
                decode_block(b, &index, 0, |t, s, x| {
                    let lid = (t >> 8) as usize;
                    let (region, entry) = (file.u32_at(u.lids, 2 * lid), file.u32_at(u.lids, 2 * lid + 1));
                    f(Edge { src: file.source(u, s), dst: state_id(region, entry, t & 0xff), xfer: remap[x as usize] });
                    true
                });
            }
        }
    }

    /// Every unit of frames `1..=horizon`: `(frame, file, unit)`, for
    /// `unit` (a pass over the graph unit by unit, in parallel).
    pub fn units(&self) -> Vec<(u32, u32, u32)> {
        let mut v = Vec::new();
        for (f, files) in self.frames.iter().enumerate() {
            for (fi, file) in files.iter().enumerate() {
                v.extend((0..file.head.units.len() as u32).map(|u| (f as u32, fi as u32, u)));
            }
        }
        v
    }

    /// One unit (`units`), read in place.
    pub fn unit(&self, (frame, file, unit): (u32, u32, u32)) -> UnitView<'_> {
        let file = &self.frames[frame as usize][file as usize];
        let u = &file.head.units[unit as usize];
        UnitView { file, u, remap: &file.head.remaps[u.worker as usize] }
    }

    /// `scan` as a list.
    pub fn edges_at(&self, frame: u32) -> Vec<Edge> {
        let mut v = Vec::new();
        self.scan(frame, |e| v.push(e));
        v
    }
}

/// One recorded unit: its sources, its lids' owners, its edges.
pub struct UnitView<'a> {
    file: &'a EdgeFile,
    u: &'a UnitHead,
    remap: &'a [u32],
}

impl UnitView<'_> {
    pub fn n_sources(&self) -> usize {
        self.u.n_sources as usize
    }

    /// Source `s` (by its lane from the unit's first).
    pub fn source(&self, s: u32) -> StateId {
        self.file.source(self.u, s)
    }

    pub fn n_lids(&self) -> usize {
        self.u.n_lids as usize
    }

    /// Lid `l`'s owner `(region, entry)` (`None`: a lid no edge names).
    pub fn lid_owner(&self, l: u32) -> Option<(u32, u32)> {
        let e = self.file.u32_at(self.u.lids, 2 * l as usize + 1);
        (e != super::unit::NONE).then(|| (self.file.u32_at(self.u.lids, 2 * l as usize), e))
    }

    pub fn n_edges(&self) -> u64 {
        self.u.edges
    }

    /// Every edge, by target: `f(lid, cell, source, global transfer)`.
    pub fn edges(&self, mut f: impl FnMut(u32, u32, u32, u32)) {
        let (b, index) = self.file.block(self.u);
        decode_block(b, &index, 0, |t, s, x| {
            f(t >> 8, t & 0xff, s, self.remap[x as usize]);
            true
        });
    }
}

/// The last frame whose files are complete (`<dir>/done.txt`); `None`: no
/// marker.
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

/// Drop what a run past `last` (the last complete frame) left: every edge
/// file of a later frame and temporary files. Transfers past the trusted
/// length are overwritten by the next merge.
pub fn discard_after(dir: &Path, last: u32) -> Result<()> {
    for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
        let p = e.path();
        let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
        let frame: Option<u32> = n.strip_prefix('f').and_then(|s| s.get(..3)).and_then(|s| s.parse().ok());
        if n.ends_with(".tmp") || frame.is_some_and(|f| f > last) {
            std::fs::remove_file(&p).with_context(|| p.display().to_string())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::unit::{pack_edge, Lid, UnitOut};
    use super::*;

    fn unit(worker: u32, sources: Vec<StateId>, lids: Vec<(u32, u32)>, mut edges: Vec<u64>) -> UnitOut {
        edges.sort_unstable();
        edges.dedup();
        let (block, index) = encode_block(&edges);
        UnitOut {
            block_at: None,
            block_len: block.len() as u64,
            worker,
            sources,
            lids: lids.iter().map(|_| Lid { shape: 0, slot: 0, key: (0, 0) }).collect(),
            owners: lids.iter().map(|&(r, e)| std::sync::atomic::AtomicU64::new(super::super::unit::pack_owner(r, e))).collect(),
            requests: Vec::new(),
            pending: Vec::new(),
            bufs: Vec::new(),
            block,
            index,
            edges: edges.len() as u64,
        }
    }

    /// A target's in-edges come back from every unit naming it, across
    /// index blocks; a scan yields every edge; transfers go through the
    /// workers' remaps.
    #[test]
    fn preds_and_scans_find_every_edge() {
        let dir = std::env::temp_dir().join(format!("celeste-storage-edges-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let (r1, r2) = (5u32, 9u32);
        // Unit 0 (worker 0): 600 edges into (r1, entry 7, cell 3) and one
        // into (r2, 2, 0); unit 1 (worker 1): (r1, 7, 3) again, lid 1.
        let mut e0: Vec<u64> = (0..600).map(|s| pack_edge(0, 3, s % 300, (s / 300) as u32)).collect();
        e0.push(pack_edge(1, 0, 4, 1));
        let u0 = unit(0, (0..300).map(|s| state_id(1, s, 0)).collect(), vec![(r1, 7), (r2, 2)], e0);
        let u1 = unit(1, vec![state_id(2, 0, 1), state_id(2, 1, 1)], vec![(r2, 3), (r1, 7)], vec![pack_edge(1, 3, 1, 0), pack_edge(0, 63, 0, 0)]);
        let path = file_path(&dir, 4, None);
        write_file(&path, 4, &[u0, u1], vec![vec![10, 11], vec![20]]).unwrap();
        let mut xt = XferTable::default();
        let mut pairs = Vec::new();
        for i in 0..21u32 {
            let a = crate::search::arc_edges::AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: i as i32 };
            pairs.push((a, a));
        }
        xt.merge(Some(&dir), &[pairs]).unwrap();
        let store = EdgeStore::open(&dir, 4).unwrap();
        assert_eq!(store.edges, 603);
        let mut got = Vec::new();
        store.preds_at(state_id(r1, 7, 3), 4, &mut got);
        assert_eq!(got.len(), 601);
        assert!(got.contains(&InEdge { src: state_id(2, 1, 1), xfer: 20 }), "unit 1's edge, worker 1's remap");
        assert!(got.contains(&InEdge { src: state_id(1, 299, 0), xfer: 11 }));
        got.clear();
        store.preds_at(state_id(r1, 7, 4), 4, &mut got);
        assert!(got.is_empty(), "another cell of the entry");
        store.preds_at(state_id(r2, 3, 63), 4, &mut got);
        assert_eq!(got, vec![InEdge { src: state_id(2, 0, 1), xfer: 20 }]);
        let all = store.edges_at(4);
        assert_eq!(all.len(), 603);
        assert!(all.contains(&Edge { src: state_id(1, 4, 0), dst: state_id(r2, 2, 0), xfer: 11 }));
        assert_eq!(XferTable::load(&dir).unwrap().pairs.len(), 21);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
