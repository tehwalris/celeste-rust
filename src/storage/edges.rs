//! THE RECORDED GRAPH, source-side (plans/storage-v2.md "The edge file").
//!
//! Every edge of frame `f` leaves a state of layer `f - 1` (a frontier row)
//! and is recorded by the unit that ran it, in the unit's BLOCK: its edges
//! `(target lid, target cell, source, transfer)` sorted by target and
//! encoded (`encode_block`), the lids the unit's own names for target
//! entries. The file's OWNER INDEX, `(region, entry, unit, lid)` sorted,
//! is every unit's translation table at once - the reverse walk; a unit's
//! owners by lid are derived from it at read time (`EdgeStore::owners`).
//! A unit's sources are a row range of the previous layer's frame file
//! when they are one (a whole block), explicit ids otherwise. Per frame ONE
//! file `edges/f{frame}.bin` (a raise adds `f{frame}.r{seq}.bin`); the
//! transfers are global ids into `edges/xfer.bin`, content-canonical (each
//! wave appends its new pairs sorted). Nothing is ever inverted:
//! `EdgeStore::preds_at` finds a target's in-edges through the owner index.

use anyhow::{ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

use super::unit::UnitOut;
use super::{id_entry, id_local, id_region, state_id, StateId};
use crate::search::arc_edges::{decode_pair, encode_pair, Pair, PAIR_BYTES};

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

/// A unit's edges (`unit::pack_edge`, sorted, distinct; the transfer field
/// the unit's rank of it, `unit::UnitOut::xfers`) as its block, LID BY LID:
/// per lid its edges from `starts[lid]` (`n_lids + 1` offsets), each edge a
/// HEAD then the transfer's rank (varints). A lid's first edge: its cell (a
/// byte) and source; then `src_delta << 1` for the same cell, or
/// `cell_delta << 1 | 1` and the source for its next cell. A probe for a
/// target (lid, cell) decodes only its lid's edges. (Room (6,2) 100% f57:
/// 4.5 B an edge at first, ~3 B with the unit's tables.)
pub fn encode_block(edges: &[u64], n_lids: usize) -> (Vec<u8>, Vec<u32>) {
    let mut out = Vec::with_capacity(edges.len() * 3);
    let mut starts = Vec::with_capacity(n_lids + 1);
    let mut k = 0;
    for lid in 0..n_lids as u32 {
        assert!(out.len() < u32::MAX as usize, "a unit block past 4 GB");
        starts.push(out.len() as u32);
        let (mut prev_c, mut prev_s) = (u32::MAX, 0u32);
        while k < edges.len() {
            let (l, local, src, x) = super::unit::unpack_edge(edges[k]);
            if l != lid {
                debug_assert!(l > lid, "edges sorted by lid");
                break;
            }
            if prev_c == u32::MAX {
                out.push(local as u8);
                put_varint(&mut out, src as u64);
            } else if local == prev_c {
                put_varint(&mut out, ((src - prev_s) as u64) << 1);
            } else {
                put_varint(&mut out, ((local - prev_c) as u64) << 1 | 1);
                put_varint(&mut out, src as u64);
            }
            put_varint(&mut out, x as u64);
            (prev_c, prev_s) = (local, src);
            k += 1;
        }
    }
    assert_eq!(k, edges.len(), "an edge past the unit's lids");
    starts.push(out.len() as u32);
    (out, starts)
}

/// Decode one lid's edges `b` (its slice of the block): `f(cell, source,
/// transfer rank)` until `f` returns false.
fn decode_lid(b: &[u8], mut f: impl FnMut(u32, u32, u32) -> bool) {
    if b.is_empty() {
        return;
    }
    let mut pos = 1usize;
    let mut cell = b[0] as u32;
    let mut src = get_varint(b, &mut pos) as u32;
    loop {
        let x = get_varint(b, &mut pos) as u32;
        if !f(cell, src, x) || pos >= b.len() {
            return;
        }
        let head = get_varint(b, &mut pos);
        if head & 1 == 0 {
            src += (head >> 1) as u32;
        } else {
            cell += (head >> 1) as u32;
            src = get_varint(b, &mut pos) as u32;
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
const VERSION: u32 = 5;

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
    /// `n_sources` source ids, u64 each - or with `src_seq` (not `u32::MAX`)
    /// rows `src_row0..` of the previous layer's file `src_seq` (its id
    /// column names them).
    sources: u64,
    n_sources: u32,
    src_seq: u32,
    src_row0: u32,
    /// The block: in the worker's blocks file (`blocks_path`), or with
    /// `inline`, in this file's data region.
    inline: bool,
    block: u64,
    block_len: u64,
    edges: u64,
    /// Per lid its edges' first offset in the block, and one past the last
    /// lid's: `n_lids + 1` u32s.
    starts: u64,
    /// Per lid its owner `(region, entry)` (u32 each; `unit::NONE`: unused).
    n_lids: u32,
    /// Per transfer rank (the edges' field) its global id, u32 each.
    xfers: u64,
    n_xfers: u32,
}

#[derive(Serialize, Deserialize)]
struct FileHead {
    frame: u32,
    units: Vec<UnitHead>,
    /// The OWNER INDEX: `(region, entry, unit, lid)` (u32 each) per lid with
    /// an owner, sorted - the translation tables inverted, the reverse walk.
    owner_index: u64,
    n_owner_index: u64,
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
    xfers: Vec<u8>,
    starts: Vec<u8>,
    /// `(region, entry, lid)` per lid with an owner, sorted.
    owned: Vec<(u32, u32, u32)>,
}

fn le32(v: &mut Vec<u8>, x: u32) {
    v.extend_from_slice(&x.to_le_bytes());
}

/// Unit `u`'s sections: its sources, transfers (global ids, through its
/// worker's `remap`), per-lid starts, and the owners it names (sorted, for
/// the owner index).
fn unit_bytes(u: &UnitOut, remap: &[u32], frame: u32) -> Result<UnitBytes> {
    let mut sources = Vec::new();
    if u.source_rows.is_none() {
        sources.reserve(8 * u.sources.len());
        for s in &u.sources {
            sources.extend_from_slice(&s.to_le_bytes());
        }
    }
    let mut xfers = Vec::with_capacity(4 * u.xfers.len());
    for &x in &u.xfers {
        le32(&mut xfers, remap[x as usize]);
    }
    let mut starts = Vec::with_capacity(4 * u.starts.len());
    for &o in &u.starts {
        le32(&mut starts, o);
    }
    let mut owned: Vec<(u32, u32, u32)> = Vec::new();
    for (l, o) in u.owners.iter().enumerate() {
        match o.load(std::sync::atomic::Ordering::Relaxed) {
            super::unit::NO_OWNER => {}
            o => owned.push(((o >> 32) as u32, o as u32, l as u32)),
        }
    }
    owned.sort_unstable();
    ensure!(owned.windows(2).all(|w| (w[0].0, w[0].1) != (w[1].0, w[1].1)), "frame {frame}: a unit names one entry by two lids");
    Ok(UnitBytes { sources, xfers, starts, owned })
}

/// Write frame `frame`'s edge file from its units (owners resolved) and
/// the workers' transfer remaps: the sections built per unit and written at
/// their offsets, in parallel. Atomic (renamed into place). Returns its bytes.
pub fn write_file(path: &Path, frame: u32, outs: &[UnitOut], remaps: Vec<Vec<u32>>) -> Result<u64> {
    use std::os::unix::fs::FileExt;
    let threads = crate::frame::threads();
    let bytes: Vec<UnitBytes> = super::wave::par_map(outs, threads, |u| unit_bytes(u, &remaps[u.worker as usize], frame)).into_iter().collect::<Result<_>>()?;
    // The data region's layout: per unit its sections, in unit order.
    let mut units = Vec::with_capacity(outs.len());
    let mut at = 0u64;
    let mut place = |n: usize| -> u64 {
        let o = at;
        at += n as u64;
        o
    };
    for (u, b) in outs.iter().zip(&bytes) {
        units.push(UnitHead {
            worker: u.worker,
            sources: place(b.sources.len()),
            n_sources: u.sources.len() as u32,
            src_seq: u.source_rows.map_or(u32::MAX, |r| r.0),
            src_row0: u.source_rows.map_or(0, |r| r.1),
            inline: u.block_at.is_none(),
            block: match u.block_at {
                Some(at) => at,
                None => place(u.block.len()),
            },
            block_len: u.block_len,
            edges: u.edges,
            starts: place(b.starts.len()),
            n_lids: u.lids.len() as u32,
            xfers: place(b.xfers.len()),
            n_xfers: u.xfers.len() as u32,
        });
    }
    // The owner index: the units' sorted owners merged, a region range a
    // thread (the ranges cut at the units' region quantiles).
    let index = owner_index(&bytes, threads);
    let n_owner_index = (index.len() / 16) as u64;
    let owner_index_at = place(index.len());
    let head = bincode::serialize(&FileHead { frame, units, owner_index: owner_index_at, n_owner_index }).context("serializing an edge file header")?;
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
    file.write_all_at(&index, base + owner_index_at)?;
    let head: FileHead = bincode::deserialize(&head).context("the header just written")?;
    let order: Vec<usize> = (0..outs.len()).collect();
    super::wave::par_map(&order, threads, |&k| -> Result<()> {
        let (h, b, u) = (&head.units[k], &bytes[k], &outs[k]);
        file.write_all_at(&b.sources, base + h.sources)?;
        if h.inline {
            file.write_all_at(&u.block, base + h.block)?;
        }
        file.write_all_at(&b.starts, base + h.starts)?;
        file.write_all_at(&b.xfers, base + h.xfers)?;
        Ok(())
    })
    .into_iter()
    .collect::<Result<()>>()?;
    drop(file);
    std::fs::rename(&tmp, path)?;
    Ok(base + at)
}

/// The owner index's bytes: every unit's `(region, entry, lid)` with the
/// unit, sorted by `(region, entry, unit)`, 16 B each. Built in parallel:
/// the region space cut into ranges, each gathered from every unit's sorted
/// owners and sorted.
fn owner_index(bytes: &[UnitBytes], threads: usize) -> Vec<u8> {
    let total: usize = bytes.iter().map(|b| b.owned.len()).sum();
    if total == 0 {
        return Vec::new();
    }
    // Cuts: regions at evenly spaced positions of a sample.
    let mut sample: Vec<u32> = bytes.iter().flat_map(|b| b.owned.iter().step_by(64).map(|o| o.0)).collect();
    sample.sort_unstable();
    let parts = (threads * 4).max(1);
    let mut cuts: Vec<u32> = (1..parts).map(|k| sample[k * sample.len() / parts]).collect();
    cuts.dedup();
    let mut bounds = vec![0u32];
    bounds.extend(cuts);
    bounds.push(u32::MAX);
    bounds.dedup();
    let ranges: Vec<(u32, u32)> = bounds.windows(2).map(|w| (w[0], w[1])).collect();
    let pieces: Vec<Vec<u8>> = super::wave::par_map(&ranges, threads, |&(lo, hi)| {
        let mut v: Vec<(u32, u32, u32, u32)> = Vec::new();
        for (ui, b) in bytes.iter().enumerate() {
            let a = b.owned.partition_point(|o| o.0 < lo);
            let z = b.owned.partition_point(|o| o.0 < hi);
            v.extend(b.owned[a..z].iter().map(|&(r, e, l)| (r, e, ui as u32, l)));
        }
        v.sort_unstable();
        let mut out = Vec::with_capacity(16 * v.len());
        for (r, e, u, l) in v {
            le32(&mut out, r);
            le32(&mut out, e);
            le32(&mut out, u);
            le32(&mut out, l);
        }
        out
    });
    pieces.concat()
}

/// One edge file, mapped, with its workers' blocks files.
struct EdgeFile {
    map: memmap2::Mmap,
    head: FileHead,
    data: usize,
    /// Per worker its blocks file, where a unit's block is there.
    blocks: Vec<Option<memmap2::Mmap>>,
    /// The previous layer's files by seq, where units name their sources
    /// as rows of them.
    prev: rustc_hash::FxHashMap<u32, crate::search::checkpoint::FrameFile>,
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
        // The previous layer's files, where a unit's sources are its rows:
        // `<level>/frames/f{frame - 1}` beside `<level>/edges`.
        let mut prev = rustc_hash::FxHashMap::default();
        if head.units.iter().any(|u| u.src_seq != u32::MAX) {
            let level = path.parent().and_then(|d| d.parent()).with_context(|| format!("{}: no level dir", path.display()))?;
            for (seq, file) in crate::frame::frame_files(level, frame - 1)? {
                prev.insert(seq, file);
            }
            for u in head.units.iter().filter(|u| u.src_seq != u32::MAX) {
                let f = prev.get(&u.src_seq).with_context(|| format!("{}: a unit's sources in file seq {} of frame {}, which is missing", path.display(), u.src_seq, frame - 1))?;
                ensure!(u.src_row0 as u64 + u.n_sources as u64 <= f.width() as u64, "{}: a unit's sources past its file's rows", path.display());
            }
        }
        Ok(EdgeFile { map, head, data: 16 + n, blocks, prev })
    }

    #[inline]
    fn u32_at(&self, off: u64, k: usize) -> u32 {
        let o = self.data + off as usize + 4 * k;
        u32::from_le_bytes(self.map[o..o + 4].try_into().unwrap())
    }

    #[inline]
    fn source(&self, u: &UnitHead, s: u32) -> StateId {
        if u.src_seq != u32::MAX {
            return self.prev[&u.src_seq].id_at(u.src_row0 + s);
        }
        let o = self.data + u.sources as usize + 8 * s as usize;
        u64::from_le_bytes(self.map[o..o + 8].try_into().unwrap())
    }

    fn block<'b>(&'b self, u: &'b UnitHead) -> &'b [u8] {
        if u.inline {
            &self.map[self.data + u.block as usize..self.data + (u.block + u.block_len) as usize]
        } else {
            &self.blocks[u.worker as usize].as_ref().expect("mapped at open")[u.block as usize..(u.block + u.block_len) as usize]
        }
    }

    /// Lid `lid`'s edges in unit `u`'s block.
    #[inline]
    fn lid_edges<'b>(&'b self, u: &'b UnitHead, block: &'b [u8], lid: u32) -> &'b [u8] {
        let (a, b) = (self.u32_at(u.starts, lid as usize), self.u32_at(u.starts, lid as usize + 1));
        &block[a as usize..b as usize]
    }

    /// The owner index's entry `k`: `(region, entry, unit, lid)`.
    #[inline]
    fn owner_at(&self, k: usize) -> (u32, u32, u32, u32) {
        let o = self.head.owner_index;
        (self.u32_at(o, 4 * k), self.u32_at(o, 4 * k + 1), self.u32_at(o, 4 * k + 2), self.u32_at(o, 4 * k + 3))
    }

    /// The owner index's entries naming `(region, entry)`: their first.
    fn owners_of(&self, region: u32, entry: u32) -> usize {
        let (mut lo, mut hi) = (0usize, self.head.n_owner_index as usize);
        while lo < hi {
            let mid = (lo + hi) / 2;
            let (r, e, ..) = self.owner_at(mid);
            if (r, e) < (region, entry) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        lo
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
            let mut k = file.owners_of(region, entry);
            while k < file.head.n_owner_index as usize {
                let (r, e, ui, lid) = file.owner_at(k);
                if (r, e) != (region, entry) {
                    break;
                }
                k += 1;
                let u = &file.head.units[ui as usize];
                let block = file.block(u);
                decode_lid(file.lid_edges(u, block, lid), |c, s, x| {
                    if c == local {
                        out.push(InEdge { src: file.source(u, s), xfer: file.u32_at(u.xfers, x as usize) });
                    }
                    c <= local
                });
            }
        }
    }

    /// Every edge recorded at frame `frame`, unit by unit.
    pub fn scan(&self, frame: u32, mut f: impl FnMut(Edge)) {
        let owners = self.owners(frame);
        for (fi, unit) in self.units(frame) {
            let u = self.unit(frame, &owners, fi, unit);
            u.edges(|lid, c, s, x| {
                let (region, entry) = u.lid_owner(lid).expect("an edge names an owned lid");
                f(Edge { src: u.source(s), dst: state_id(region, entry, c), xfer: x });
            });
        }
    }

    /// Frame `frame`'s files (`storage-census`): per file its index file's
    /// bytes, its owner index's entries and its units.
    pub fn file_sizes(&self, frame: u32) -> Vec<(u64, u64, usize)> {
        self.frames.get(frame as usize).into_iter().flatten().map(|f| (f.map.len() as u64, f.head.n_owner_index, f.head.units.len())).collect()
    }

    /// The last frame with edges.
    pub fn horizon(&self) -> u32 {
        self.frames.len() as u32 - 1
    }

    /// Frame `frame`'s units: `(file, unit)`.
    pub fn units(&self, frame: u32) -> Vec<(u32, u32)> {
        let mut v = Vec::new();
        for (fi, file) in self.frames.get(frame as usize).into_iter().flatten().enumerate() {
            v.extend((0..file.head.units.len() as u32).map(|u| (fi as u32, u)));
        }
        v
    }

    /// Frame `frame`'s lids' owners, by unit and lid, from the files' owner
    /// indexes (`pack_owner`, `NO_OWNER`): held while the frame is read unit
    /// by unit, then dropped (8 B a lid; on disk only the index).
    pub fn owners(&self, frame: u32) -> FrameOwners {
        let mut files = Vec::new();
        for file in self.frames.get(frame as usize).into_iter().flatten() {
            let mut starts = Vec::with_capacity(file.head.units.len() + 1);
            let mut n = 0usize;
            for u in &file.head.units {
                starts.push(n);
                n += u.n_lids as usize;
            }
            starts.push(n);
            // The owner index scattered by (unit, lid), in parallel: it is
            // sorted by owner, so the writes land anywhere.
            use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
            let owners: Vec<AtomicU64> = (0..n).map(|_| AtomicU64::new(super::unit::NO_OWNER)).collect();
            let m = file.head.n_owner_index as usize;
            let threads = crate::frame::threads().max(1);
            let chunk = m.div_ceil(threads).max(1 << 16);
            std::thread::scope(|sc| {
                for lo in (0..m).step_by(chunk) {
                    let (owners, starts) = (&owners, &starts);
                    sc.spawn(move || {
                        for k in lo..(lo + chunk).min(m) {
                            let (r, e, ui, lid) = file.owner_at(k);
                            owners[starts[ui as usize] + lid as usize].store(super::unit::pack_owner(r, e), Relaxed);
                        }
                    });
                }
            });
            files.push((starts, owners.into_iter().map(AtomicU64::into_inner).collect()));
        }
        FrameOwners { files }
    }

    /// Unit `unit` of file `file` of frame `frame` (`units`), read in place
    /// with the frame's `owners`.
    pub fn unit<'a>(&'a self, frame: u32, owners: &'a FrameOwners, file: u32, unit: u32) -> UnitView<'a> {
        let (starts, own) = &owners.files[file as usize];
        let f = &self.frames[frame as usize][file as usize];
        let u = &f.head.units[unit as usize];
        let prev = (u.src_seq != u32::MAX).then(|| &f.prev[&u.src_seq]);
        UnitView { file: f, u, owners: &own[starts[unit as usize]..starts[unit as usize + 1]], prev }
    }

    /// `scan` as a list.
    pub fn edges_at(&self, frame: u32) -> Vec<Edge> {
        let mut v = Vec::new();
        self.scan(frame, |e| v.push(e));
        v
    }
}

/// A frame's lids' owners (`EdgeStore::owners`): per file, per unit its
/// first index, and the owners.
pub struct FrameOwners {
    files: Vec<(Vec<usize>, Vec<u64>)>,
}

/// One recorded unit: its sources, its lids' owners, its edges.
pub struct UnitView<'a> {
    file: &'a EdgeFile,
    u: &'a UnitHead,
    owners: &'a [u64],
    /// The previous layer's file holding the sources, when a row range.
    prev: Option<&'a crate::search::checkpoint::FrameFile>,
}

impl UnitView<'_> {
    pub fn n_sources(&self) -> usize {
        self.u.n_sources as usize
    }

    /// Source `s` (by its lane from the unit's first).
    pub fn source(&self, s: u32) -> StateId {
        match self.prev {
            Some(p) => p.id_at(self.u.src_row0 + s),
            None => self.file.source(self.u, s),
        }
    }

    pub fn n_lids(&self) -> usize {
        self.u.n_lids as usize
    }

    /// The unit's recorded layout (`storage-census`): its block's bytes, its
    /// transfer ranks, and whether its sources are explicit ids (8 B each)
    /// rather than a row range.
    pub fn layout(&self) -> (u64, u32, bool) {
        (self.u.block_len, self.u.n_xfers, self.u.src_seq == u32::MAX)
    }

    /// Lid `l`'s bytes in the block (`storage-census`).
    pub fn lid_bytes(&self, l: u32) -> u32 {
        self.file.u32_at(self.u.starts, l as usize + 1) - self.file.u32_at(self.u.starts, l as usize)
    }

    /// Lid `l`'s owner `(region, entry)` (`None`: a lid no edge names).
    pub fn lid_owner(&self, l: u32) -> Option<(u32, u32)> {
        match self.owners[l as usize] {
            super::unit::NO_OWNER => None,
            o => Some(((o >> 32) as u32, o as u32)),
        }
    }

    /// Every edge, by target: `f(lid, cell, source (its index, `source`),
    /// global transfer)`.
    pub fn edges(&self, mut f: impl FnMut(u32, u32, u32, u32)) {
        let block = self.file.block(self.u);
        for lid in 0..self.u.n_lids {
            decode_lid(self.file.lid_edges(self.u, block, lid), |c, s, x| {
                f(lid, c, s, self.file.u32_at(self.u.xfers, x as usize));
                true
            });
        }
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
        let (block, starts) = encode_block(&edges, lids.len());
        UnitOut {
            source_rows: None,
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
            // The ranks are the worker's ids here.
            xfers: (0..=edges.iter().map(|&e| super::super::unit::unpack_edge(e).3).max().unwrap_or(0)).collect(),
            starts,
            edges: edges.len() as u64,
        }
    }

    /// A block decodes, lid by lid, to the edges encoded, in order: same
    /// cells (source deltas), next cells, lids without edges.
    #[test]
    fn blocks_round_trip_lid_by_lid() {
        let mut v = 0x9e37_79b9_7f4a_7c15u64;
        let mut edges: Vec<u64> = (0..5000)
            .map(|_| {
                v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                let lid = (v >> 40) as u32 % 700;
                let local = (v >> 20) as u32 % 64;
                pack_edge(lid, local, (v >> 8) as u32 % 4096, (v >> 4) as u32 % 300)
            })
            .collect();
        edges.sort_unstable();
        edges.dedup();
        let (b, starts) = encode_block(&edges, 703);
        assert_eq!(starts.len(), 704);
        let want: Vec<(u32, u32, u32, u32)> = edges.iter().map(|&e| super::super::unit::unpack_edge(e)).collect();
        let mut got = Vec::new();
        for lid in 0..703u32 {
            decode_lid(&b[starts[lid as usize] as usize..starts[lid as usize + 1] as usize], |c, s, x| {
                got.push((lid, c, s, x));
                true
            });
        }
        assert_eq!(got, want);
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
