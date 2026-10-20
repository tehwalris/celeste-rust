//! LAYERS (plans/architecture.md, "The forward"): a plain set of states
//! with the skeletons its rows decode with (the frontier's new states, the
//! next layer), and the FRAME FILES (`frames/f{frame}.bin`, `storage::file`),
//! one a frame: per region a block of the frame's new entries
//! (`encode_region`), then the frame's FRONTIER (`hulls`: the new states'
//! hulls, the grown states), and a header (`FrameHeader`) with what the
//! frame added to the tree's codebooks and its skeletons. `Tree` reads a
//! tree's frame files.

use anyhow::{anyhow, ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use celeste_core::arith::Arithmetic;
use celeste_engine::exact::{Key, KeyAdditions, KeySpace};
use celeste_engine::runtime2::{mix64, Cell2, Col, Rt2, P8};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::file::{self, At, Mapped, FORMAT_VERSION};
use super::decode::Repr;
use super::set::{PlainTable, RegionSet};
use super::{Sk, GEOMETRY};

#[derive(Clone)]
pub struct Layer<N: Arithmetic = P8> {
    pub(crate) set: RegionSet,
    /// Per shape (hash) its skeleton (`SkelRecord::rt2`).
    pub(crate) skeletons: FxHashMap<u64, Arc<Rt2<N>>>,
}

impl<N: Arithmetic> Default for Layer<N> {
    fn default() -> Self {
        Layer { set: RegionSet::default(), skeletons: FxHashMap::default() }
    }
}

impl<N: Arithmetic> Layer<N> {
    /// Append region `region`'s entries (after every region held; keys
    /// ascending, masks non-empty: checked).
    pub(crate) fn push_region(&mut self, region: u32, keys: &[Key], masks: &[u64]) -> Result<()> {
        ensure!(masks.iter().all(|&m| m != 0), "region {region}: an entry without cells");
        ensure!(region % GEOMETRY.slots != GEOMETRY.no_cell_slot() || masks.iter().all(|&m| m == 1), "region {region} (no player): a cell past 0");
        self.set.push(region, PlainTable::from_sorted(keys, masks)?)
    }

    pub fn entries(&self) -> usize {
        self.set.entries()
    }

    pub fn states(&self) -> u64 {
        self.set.states()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    /// Every entry `(region, key, mask)`, in order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (u32, Key, u64)> + '_ {
        self.set.iter()
    }

    /// Every state, in order (region, key, cell).
    pub fn states_iter(&self) -> impl Iterator<Item = Sk> + '_ {
        self.set.states_iter()
    }

    #[cfg(test)]
    pub(crate) fn contains(&self, s: Sk) -> bool {
        self.set.contains(s)
    }

    /// `rewrite ckhash`'s `f` line: the states' count and the sum of their
    /// CONTENT hashes (any dictionary order). `shapes`: the tree's by index.
    pub fn content_fingerprint(&self, space: &KeySpace<N>, shapes: &[u64]) -> Result<(u64, u64)> {
        let content = space.content_hash();
        let geo = GEOMETRY;
        let (mut n, mut acc) = (0u64, 0u64);
        for (region, key, mask) in self.iter() {
            let shape = *shapes.get((region / geo.slots) as usize).ok_or_else(|| anyhow!("region {region} of no shape"))?;
            let kh = content.key(shape, key);
            for local in bits(mask) {
                acc = acc.wrapping_add(mix64(kh ^ (geo.cell(region % geo.slots, local) as u64) << 1));
                n += 1;
            }
        }
        Ok((n, acc))
    }

    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes()
    }
}

/// The set bits of `m`, ascending.
#[inline]
pub(crate) fn bits(mut m: u64) -> impl Iterator<Item = u32> {
    std::iter::from_fn(move || {
        (m != 0).then(|| {
            let b = m.trailing_zeros();
            m &= m - 1;
            b
        })
    })
}

/// A SHAPE'S SKELETON as the storage keeps it: everything of a shape's rows
/// their keys do not say - structure with closure captures, globals,
/// strings, the non-value cells' uniform columns, and per value
/// cell how its column holds numbers (`Repr`: the code merges `Num(n)` and
/// `Ival(n, n)`). Equal records decode every key alike.
#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub(crate) struct SkelRecord<N = P8> {
    pub(crate) shape_hash: u64,
    pub(crate) structure: Vec<Cell2<N>>,
    pub(crate) globals: Vec<u32>,
    pub(crate) strings: Vec<String>,
    /// The cells that are not values, with their (uniform) columns.
    pub(crate) others: Vec<(u32, Col<N>)>,
    /// The value cells whose columns hold numbers (0) or intervals (1); the
    /// others any value (`Repr::Other`).
    pub(crate) numeric: Vec<(u32, u8)>,
}

impl<N: Arithmetic> SkelRecord<N> {
    /// The skeleton of `rt2`'s shape (its values are not read).
    pub(crate) fn of(rt2: &Rt2<N>) -> Result<SkelRecord<N>> {
        for cell in &rt2.structure {
            if let Cell2::Clo(_, caps) = cell {
                ensure!(caps.iter().all(|c| matches!(c, Col::U(_))), "skeleton of {:016x}: a closure capture is not uniform", rt2.shape_hash);
            }
        }
        let (mut others, mut numeric) = (Vec::new(), Vec::new());
        for (c, (cell, col)) in rt2.structure.iter().zip(&rt2.cols).enumerate() {
            if !matches!(cell, Cell2::Val) {
                ensure!(matches!(col, Col::U(_)), "skeleton of {:016x}: the non-value cell {c} has a varying column", rt2.shape_hash);
                others.push((c as u32, col.clone()));
                continue;
            }
            match Repr::of(col) {
                Repr::Num => numeric.push((c as u32, 0)),
                Repr::Ival => numeric.push((c as u32, 1)),
                Repr::Other => {}
            }
        }
        Ok(SkelRecord { shape_hash: rt2.shape_hash, structure: rt2.structure.clone(), globals: rt2.globals.clone(), strings: rt2.strings.clone(), others, numeric })
    }

    /// The skeleton as an empty block over `cache` (a room's).
    pub(crate) fn rt2(&self, cache: Arc<celeste_core::collision_cache::CollisionCache>) -> Rt2<N> {
        let mut rt2 = Rt2::empty(0, self.globals.len(), &[], cache);
        rt2.structure = self.structure.clone();
        rt2.globals = self.globals.clone();
        rt2.strings = self.strings.clone();
        rt2.shape_hash = self.shape_hash;
        rt2.cols = vec![Repr::Other.empty(); self.structure.len()];
        for (c, col) in &self.others {
            rt2.cols[*c as usize] = col.clone();
        }
        for &(c, r) in &self.numeric {
            rt2.cols[c as usize] = if r == 0 { Repr::Num.empty() } else { Repr::Ival.empty() };
        }
        rt2
    }

    /// The skeleton in a search from `room` (its collision cache).
    pub(crate) fn rt2_in(&self, room: (i16, i16)) -> Rt2<N> {
        self.rt2(crate::compiled::collision_cache(room))
    }
}

const MAGIC: &[u8; 4] = b"CSL1";

pub(crate) fn frame_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("frames").join(format!("f{frame:03}.bin"))
}

pub(crate) fn frame_exists(dir: &Path, frame: u32) -> bool {
    frame_path(dir, frame).is_file()
}

/// The tree's last frame with a file (frames from 0 on, consecutive).
pub(crate) fn last_frame(dir: &Path) -> Option<u32> {
    let mut last = None;
    while frame_exists(dir, last.map_or(0, |f: u32| f + 1)) {
        last = Some(last.map_or(0, |f| f + 1));
    }
    last
}

/// One region's place in a frame file.
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub(crate) struct RegionIndex {
    pub(crate) region: u32,
    pub(crate) entries: u32,
    pub(crate) states: u32,
    pub(crate) wins: u32,
    pub(crate) at: At,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(bound = "")]
pub(crate) struct FrameHeader<N: Arithmetic = P8> {
    pub(crate) frame: u32,
    /// The storage region side the tree is built with.
    pub(crate) side: u32,
    /// `(index, hash)` of the shapes numbered here.
    pub(crate) shapes: Vec<(u32, u64)>,
    pub(crate) keys: KeyAdditions<N>,
    /// The skeletons of the shapes the file holds states of.
    pub(crate) skeletons: Vec<SkelRecord<N>>,
    /// The skeletons first EMITTED at the frame of shapes it holds no state
    /// of: a re-push reaches such a shape's state later without emitting
    /// it - a resume registers every file's (`frame::restore_visited`).
    pub(crate) emitted: Vec<SkelRecord<N>>,
    /// Per region held (ascending) its block.
    pub(crate) regions: Vec<RegionIndex>,
    /// The frontier's blocks (`hulls`), after the regions'.
    pub(crate) frontier: Vec<super::hulls::Block>,
}

impl<N: Arithmetic> FrameHeader<N> {
    pub(crate) fn states(&self) -> u64 {
        self.regions.iter().map(|r| r.states as u64).sum()
    }

    pub(crate) fn wins(&self) -> u64 {
        self.regions.iter().map(|r| r.wins as u64).sum()
    }
}

/// What a frame file holds besides its states.
pub(crate) struct FrameMeta<'a, N: Arithmetic = P8> {
    pub(crate) frame: u32,
    pub(crate) shapes: &'a [(u32, u64)],
    pub(crate) keys: &'a KeyAdditions<N>,
    /// `FrameHeader::emitted`.
    pub(crate) emitted: &'a [SkelRecord<N>],
}

/// One region's block: its entries' keys (delta-coded, ascending), masks
/// and win masks (sparse: entry delta, mask).
fn encode_region(keys: &[Key], masks: &[u64], wins: &[(u32, u64)], out: &mut Vec<u8>) {
    out.clear();
    put_varint(out, keys.len() as u64);
    let mut prev = 0u128;
    for (i, k) in keys.iter().enumerate() {
        let w = wide(*k);
        put_varint128(out, if i == 0 { w } else { w - prev });
        prev = w;
    }
    for m in masks {
        out.extend_from_slice(&m.to_le_bytes());
    }
    put_varint(out, wins.len() as u64);
    let mut last = 0u32;
    for (i, &(e, m)) in wins.iter().enumerate() {
        put_varint(out, (if i == 0 { e } else { e - last }) as u64);
        out.extend_from_slice(&m.to_le_bytes());
        last = e;
    }
}

/// A region's entries and win masks (by entry index), decoded.
pub(crate) struct RegionEntries {
    pub(crate) keys: Vec<Key>,
    pub(crate) masks: Vec<u64>,
    pub(crate) wins: Vec<(u32, u64)>,
}

fn decode_region(raw: &[u8]) -> Result<RegionEntries> {
    let mut pos = 0usize;
    let n = get_varint(raw, &mut pos) as usize;
    ensure!(n <= raw.len(), "a region block of {n} entries in {} bytes", raw.len());
    let mut keys = Vec::with_capacity(n);
    let mut prev = 0u128;
    for i in 0..n {
        let d = get_varint128(raw, &mut pos);
        let w = if i == 0 { d } else { prev + d };
        keys.push(narrow(w));
        prev = w;
    }
    ensure!(pos + 8 * n <= raw.len(), "a region block cut short");
    let masks = raw[pos..pos + 8 * n].chunks_exact(8).map(|b| u64::from_le_bytes(b.try_into().unwrap())).collect();
    pos += 8 * n;
    let nw = get_varint(raw, &mut pos) as usize;
    let mut wins = Vec::with_capacity(nw);
    let mut last = 0u32;
    for i in 0..nw {
        let d = get_varint(raw, &mut pos) as u32;
        let e = if i == 0 { d } else { last + d };
        ensure!(pos + 8 <= raw.len() && (e as usize) < n, "a region block's wins cut short");
        wins.push((e, u64::from_le_bytes(raw[pos..pos + 8].try_into().unwrap())));
        pos += 8;
        last = e;
    }
    ensure!(pos == raw.len(), "a region block with {} bytes past its entries", raw.len() - pos);
    Ok(RegionEntries { keys, masks, wins })
}

/// WRITE a frame file: `layer`'s states with their win masks (`wins`:
/// sorted entry indices of `layer` with a win, and the win cells), its
/// skeletons, what the frame added to the codebooks, and its frontier (the
/// new states' `hulls` in their order, the grown states in `grown`:
/// `hulls::encode`). Returns its bytes.
pub(crate) fn write_frame<N: crate::search::rem::Rem, R: super::hulls::Runs<N> + ?Sized>(path: &Path, meta: &FrameMeta<N>, layer: &Layer<N>, wins: &[(u32, u64)], hulls: &[crate::search::hull::HullOf<N>], grown: &R, threads: usize) -> Result<u64> {
    ensure!(layer.states() == hulls.len() as u64, "write_frame: {} new states, {} hulls", layer.states(), hulls.len());
    ensure!(wins.windows(2).all(|w| w[0].0 < w[1].0), "write_frame: wins not by entry");
    let tables = layer.set.tables();
    let starts = layer.set.entry_starts();
    // Per region its wins (entry numbers made region-relative).
    let mut win_at = Vec::with_capacity(tables.len());
    let mut wi = 0usize;
    for (i, (_, t)) in tables.iter().enumerate() {
        let (a, end) = (wi, starts[i] + t.len());
        while wi < wins.len() && (wins[wi].0 as usize) < end {
            let e = wins[wi].0 as usize;
            ensure!(e >= starts[i] && wins[wi].1 & !t.mask((e - starts[i]) as u32) == 0, "write_frame: a win outside its entry");
            wi += 1;
        }
        win_at.push(a..wi);
    }
    ensure!(wi == wins.len(), "write_frame: a win past the layer");
    let blocks: Vec<(Vec<u8>, usize)> = crate::par::Pool::new(threads).map_with(tables.len(), || Ok(zstd::bulk::Compressor::new(1)?), |z, i| {
        let t = &tables[i].1;
        let w: Vec<(u32, u64)> = wins[win_at[i].clone()].iter().map(|&(e, m)| (e - starts[i] as u32, m)).collect();
        let keys: Vec<Key> = t.iter().map(|x| x.0).collect();
        let mut raw = Vec::new();
        let masks: Vec<u64> = t.masks().collect();
        encode_region(&keys, &masks, &w, &mut raw);
        Ok((file::compress(&raw, z)?, raw.len()))
    })?;
    let mut frontier = super::hulls::encode(hulls, grown, threads).with_context(|| path.display().to_string())?;
    let mut ats = At::place(blocks.iter().map(|b| (b.0.len(), b.1)).chain(frontier.iter().map(|b| (b.1.len(), b.2)))).into_iter();
    let index: Vec<RegionIndex> = tables
        .iter()
        .zip(&win_at)
        .zip(ats.by_ref())
        .map(|(((region, t), w), at)| RegionIndex { region: *region, entries: t.len() as u32, states: t.states() as u32, wins: wins[w.clone()].iter().map(|x| x.1.count_ones()).sum(), at })
        .collect();
    let mut skeletons: Vec<SkelRecord<N>> = layer.skeletons.values().map(|s| SkelRecord::of(s)).collect::<Result<_>>()?;
    skeletons.sort_by_key(|s| s.shape_hash);
    ensure!(meta.emitted.iter().all(|e| !layer.skeletons.contains_key(&e.shape_hash)), "write_frame: an emitted skeleton of a shape the layer holds");
    for (b, at) in frontier.iter_mut().zip(ats) {
        b.0.at = at;
    }
    let header = FrameHeader { frame: meta.frame, side: GEOMETRY.side, shapes: meta.shapes.to_vec(), keys: meta.keys.clone(), skeletons, emitted: meta.emitted.to_vec(), regions: index, frontier: frontier.iter().map(|b| b.0).collect() };
    file::write(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &header, |f| {
        for (z, _) in &blocks {
            f.write_all(z)?;
        }
        for (_, z, _) in &frontier {
            f.write_all(z)?;
        }
        Ok(())
    })
}

pub(crate) struct FrameFile<N: Arithmetic = P8> {
    file: Mapped,
    pub(crate) header: FrameHeader<N>,
}

impl<N: Arithmetic> FrameFile<N> {
    pub(crate) fn open(path: &Path) -> Result<Self> {
        let file = Mapped::open(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &format!("a frame file of the {} arithmetic", N::NAME))?;
        let header: FrameHeader<N> = file.header()?;
        ensure!(header.side == GEOMETRY.side, "{}: built with storage regions of {} cells, this process uses {}", path.display(), header.side, GEOMETRY.side);
        file.ends_at(header.frontier.last().map(|b| b.at).or(header.regions.last().map(|r| r.at)).map_or(0, |at| at.end()))?;
        Ok(FrameFile { file, header })
    }

    pub(crate) fn path(&self) -> &Path {
        self.file.path()
    }

    /// Region `i` (of `header.regions`) decoded.
    pub(crate) fn region(&self, i: usize) -> Result<RegionEntries> {
        let r = &self.header.regions[i];
        let mut raw = Vec::new();
        self.file.block(r.at, &mut zstd::bulk::Decompressor::new()?, &mut raw).with_context(|| format!("region {}", r.region))?;
        let e = decode_region(&raw).with_context(|| format!("{}: region {}", self.path().display(), r.region))?;
        ensure!(e.keys.len() == r.entries as usize, "{}: region {}: {} entries, its index says {}", self.path().display(), r.region, e.keys.len(), r.entries);
        Ok(e)
    }

    /// Drop the resident pages of region `i`'s block (`Mapped::release`).
    pub(crate) fn release_region(&self, i: usize) -> Result<()> {
        self.file.release(self.file.stored(self.header.regions[i].at))
    }

    /// The file's states as a layer and its wins, its skeletons in a search
    /// from `room` (`None`: none - a reader of keys and cells only).
    pub(crate) fn layer(&self, room: Option<(i16, i16)>, threads: usize) -> Result<(Layer<N>, Vec<(u32, u64)>)> {
        let n = self.header.regions.len();
        let parts: Vec<Result<RegionEntries>> = super::wave::par_map(&(0..n).collect::<Vec<_>>(), threads, |&i| self.region(i));
        let mut layer = Layer::<N>::default();
        let mut wins = Vec::new();
        for (i, p) in parts.into_iter().enumerate() {
            let p = p?;
            let base = layer.entries() as u32;
            layer.push_region(self.header.regions[i].region, &p.keys, &p.masks).with_context(|| self.path().display().to_string())?;
            wins.extend(p.wins.into_iter().map(|(e, m)| (base + e, m)));
        }
        ensure!(layer.set.tables().len() == n, "{}: an empty region", self.path().display());
        if let Some(room) = room {
            for s in &self.header.skeletons {
                layer.skeletons.insert(s.shape_hash, Arc::new(s.rt2_in(room)));
            }
        }
        Ok((layer, wins))
    }

    pub(crate) fn wins(&self) -> Result<Vec<Sk>> {
        let mut out = Vec::new();
        for (i, r) in self.header.regions.iter().enumerate() {
            if r.wins == 0 {
                continue;
            }
            let e = self.region(i)?;
            for (k, m) in e.wins {
                out.extend(bits(m).map(|local| Sk { region: r.region, key: e.keys[k as usize], local: local as u8 }));
            }
        }
        Ok(out)
    }
}

impl<N: crate::search::rem::Rem> FrameFile<N> {
    /// The frame's FRONTIER: its new states' hulls (in its state order) and
    /// its grown states (sorted by layer and state).
    pub(crate) fn frontier(&self, threads: usize) -> Result<(Vec<crate::search::hull::HullOf<N>>, Vec<super::hulls::Grown<N>>)> {
        let (hulls, grown) = super::hulls::read::<N>(&self.file, &self.header.frontier, threads)?;
        ensure!(hulls.len() as u64 == self.header.states(), "{}: {} new states, {} hulls", self.path().display(), self.header.states(), hulls.len());
        Ok((hulls, grown))
    }

    /// The frontier's states with their layers and hulls, the new ones
    /// (`new`: this file's layer) first.
    pub(crate) fn frontier_states(&self, new: &Layer<N>, threads: usize) -> Result<Vec<super::hulls::Grown<N>>> {
        let (hulls, grown) = self.frontier(threads)?;
        let layer = u16::try_from(self.header.frame).context("a frame past u16")?;
        let mut out: Vec<super::hulls::Grown<N>> = new.states_iter().zip(&hulls).map(|(s, &h)| (s, layer, h)).collect();
        out.extend(grown);
        Ok(out)
    }
}

/// THE COMPLETE STORAGE CHECK of a frame file just written: read back,
/// region by region, its states are exactly `layer`'s (its regions, every
/// entry's key and cells) and its wins `wins`, its frontier `hulls` and
/// `grown` (`hulls::check`). FATAL otherwise: the frame would resume as
/// another set than the forward expands.
pub(crate) fn check_frame_file<N: crate::search::rem::Rem, R: super::hulls::Runs<N> + ?Sized>(path: &Path, layer: &Layer<N>, wins: &[(u32, u64)], hulls: &[crate::search::hull::HullOf<N>], grown: &R, threads: usize) -> Result<()> {
    let f = FrameFile::<N>::open(path)?;
    let tables = layer.set.tables();
    let starts = layer.set.entry_starts();
    let differ = |what: String| anyhow!("{}: STORED STATES DIFFER from the forward's ({what})", path.display());
    ensure!(f.header.regions.len() == tables.len(), differ(format!("{} regions in memory, {} read back", tables.len(), f.header.regions.len())));
    let parts: Vec<Result<()>> = super::wave::par_map(&(0..tables.len()).collect::<Vec<_>>(), threads, |&i| {
        let (region, t) = &tables[i];
        ensure!(f.header.regions[i].region == *region, differ(format!("region {} read back as {}", region, f.header.regions[i].region)));
        let e = f.region(i)?;
        ensure!(e.keys.len() == t.len() && t.iter().zip(e.keys.iter().zip(&e.masks)).all(|((k, m), (k2, m2))| k == *k2 && m == *m2), differ(format!("region {region}'s entries")));
        let (a, end) = (starts[i], starts[i] + t.len());
        let lo = wins.partition_point(|w| (w.0 as usize) < a);
        let hi = wins.partition_point(|w| (w.0 as usize) < end);
        let want: Vec<(u32, u64)> = wins[lo..hi].iter().map(|&(x, m)| (x - a as u32, m)).collect();
        ensure!(e.wins == want, differ(format!("region {region}'s wins")));
        Ok(())
    });
    parts.into_iter().collect::<Result<Vec<()>>>()?;
    ensure!(f.header.states() == layer.states(), differ(format!("{} states in memory, {} in the header", layer.states(), f.header.states())));
    super::hulls::check(&f.file, &f.header.frontier, hulls, grown, threads)
}

/// A TREE's frame files through a frame: its codebooks (the shapes by
/// index, the key space: every file's additions, any order) and each
/// frame's file (none past a dead frontier: those frames are empty).
pub struct Tree<N: Arithmetic = P8> {
    pub(crate) dir: PathBuf,
    pub(crate) last: u32,
    pub(crate) frames: Vec<Option<FrameFile<N>>>,
    pub shapes: Vec<u64>,
    pub space: KeySpace<N>,
}

impl<N: Arithmetic> Tree<N> {
    /// The tree in `dir` through frame `last`.
    pub fn open(dir: &Path, last: u32) -> Result<Tree<N>> {
        let mut frames: Vec<Option<FrameFile<N>>> = Vec::with_capacity(last as usize + 1);
        let mut adds = KeyAdditions::default();
        let mut shapes: Vec<(u32, u64)> = Vec::new();
        for f in 0..=last {
            if !frame_exists(dir, f) {
                let died = frames.last().is_some_and(|x| x.as_ref().is_none_or(|x| x.header.states() == 0));
                ensure!(died, "{}: no frame f{f}, and the frontier before it was not empty", dir.display());
                frames.push(None);
                continue;
            }
            let x = FrameFile::open(&frame_path(dir, f))?;
            ensure!(x.header.frame == f, "{}: of frame {}, not {f}", x.path().display(), x.header.frame);
            shapes.extend(x.header.shapes.iter().copied());
            adds.extend(x.header.keys.clone());
            frames.push(Some(x));
        }
        shapes.sort_unstable();
        ensure!(shapes.iter().enumerate().all(|(i, s)| s.0 as usize == i), "{}: the shapes are not numbered 0..{}", dir.display(), shapes.len());
        let space = KeySpace::from_additions(adds).with_context(|| format!("{}: the key space", dir.display()))?;
        Ok(Tree { dir: dir.to_path_buf(), last, frames, shapes: shapes.into_iter().map(|s| s.1).collect(), space })
    }

    /// Frame `f`'s states and wins, skeletons in a search from `room` if given.
    pub fn layer(&self, f: u32, room: Option<(i16, i16)>, threads: usize) -> Result<(Layer<N>, Vec<Sk>)> {
        let file = self.frames.get(f as usize).ok_or_else(|| anyhow!("{}: no frame {f} (the tree is open to {})", self.dir.display(), self.last))?;
        let Some(file) = file else { return Ok((Layer::default(), Vec::new())) };
        let (l, w) = file.layer(room, threads)?;
        let starts = l.set.entry_starts();
        let mut wins = Vec::new();
        for &(e, m) in &w {
            let (ti, local) = l.set.locate(&starts, e as usize)?;
            let (region, t) = &l.set.tables()[ti];
            let key = t.key(local);
            wins.extend(bits(m).map(|c| Sk { region: *region, key, local: c as u8 }));
        }
        Ok((l, wins))
    }

    /// Frame `f`'s file (`None`: past a dead frontier, or past `last`).
    pub(crate) fn file(&self, f: u32) -> Option<&FrameFile<N>> {
        self.frames.get(f as usize).and_then(|x| x.as_ref())
    }

    /// Frame `f`'s state count.
    pub(crate) fn states(&self, f: u32) -> u64 {
        self.file(f).map_or(0, |x| x.header.states())
    }

    /// The first frame with a win (`None`: none through `last`).
    pub(crate) fn first_win(&self) -> Option<u32> {
        self.frames.iter().position(|x| x.as_ref().is_some_and(|x| x.header.wins() > 0)).map(|f| f as u32)
    }

    pub(crate) fn shape_of(&self, region: u32) -> Result<u64> {
        self.shapes.get((region / GEOMETRY.slots) as usize).copied().ok_or_else(|| anyhow!("{}: region {region} of no shape", self.dir.display()))
    }

    /// A state as `(shape, key, cell)`.
    #[cfg(test)]
    pub(crate) fn state(&self, s: Sk) -> Result<(u64, Key, u32)> {
        Ok((self.shape_of(s.region)?, s.key, s.cell(&GEOMETRY)))
    }

    /// A state of `shape` with `key` at `cell`, if the tree numbers the shape.
    pub(crate) fn sk(&self, shape: u64, key: Key, cell: u32) -> Option<Sk> {
        let i = self.shapes.iter().position(|&h| h == shape)?;
        Some(Sk::at(&GEOMETRY, i as u32, key, cell))
    }

    /// The cells of `(region, key)` frame `f` holds, reading that region only.
    pub(crate) fn mask_at(&self, f: u32, region: u32, key: Key) -> Result<u64> {
        let Some(file) = self.file(f) else { return Ok(0) };
        let Ok(i) = file.header.regions.binary_search_by_key(&region, |r| r.region) else { return Ok(0) };
        let e = file.region(i)?;
        Ok(e.keys.binary_search(&key).map_or(0, |k| e.masks[k]))
    }

    /// The frame holding state `s` among `0..=upto`, by searching their files.
    pub(crate) fn layer_of(&self, s: Sk, upto: u32) -> Result<Option<u32>> {
        for f in 0..=upto.min(self.last) {
            if self.mask_at(f, s.region, s.key)? >> s.local & 1 == 1 {
                return Ok(Some(f));
            }
        }
        Ok(None)
    }

    /// The skeleton of `shape` in frame `f`'s files, in a search from `room`.
    pub(crate) fn skeleton(&self, f: u32, shape: u64, room: (i16, i16)) -> Result<Rt2<N>> {
        let r = self.file(f).into_iter().flat_map(|x| x.header.skeletons.iter()).find(|r| r.shape_hash == shape).ok_or_else(|| anyhow!("{}: frame {f} has no skeleton of shape {shape:016x}", self.dir.display()))?;
        Ok(r.rt2_in(room))
    }

    /// Frame `f`'s ROWS in a search from `room` (`decode_states`; keys set).
    pub fn rows(&self, f: u32, room: (i16, i16), chunk: usize, keep: &dyn Fn(Sk) -> bool, threads: usize) -> Result<Vec<(Vec<Sk>, Rt2<N>)>> {
        let (layer, _) = self.layer(f, Some(room), threads)?;
        decode_states(&layer, &self.space, &self.shapes, chunk, keep)
    }

    /// One state's row, decoded from its frame `f` (its region's block
    /// only) in a search from `room`.
    pub fn row(&self, f: u32, s: Sk, room: (i16, i16)) -> Result<Rt2<N>> {
        ensure!(self.mask_at(f, s.region, s.key)? >> s.local & 1 == 1, "{}: frame {f} does not hold state {}", self.dir.display(), s.show());
        let shape = self.shape_of(s.region)?;
        let skel = self.skeleton(f, shape, room)?;
        super::decode::decode_rows(&self.space, &skel, &[(s.key, s.cell(&GEOMETRY))])
    }
}

/// `layer`'s states (those `keep` admits) DECODED: per shape, chunks of at
/// most `chunk` states and their rows, in the layer's order.
pub(crate) fn decode_states<N: Arithmetic>(layer: &Layer<N>, space: &KeySpace<N>, shapes: &[u64], chunk: usize, keep: &dyn Fn(Sk) -> bool) -> Result<Vec<(Vec<Sk>, Rt2<N>)>> {
    let geo = GEOMETRY;
    let mut out = Vec::new();
    let mut cur: Vec<Sk> = Vec::new();
    let mut cur_shape: Option<u32> = None;
    let flush = |cur: &mut Vec<Sk>, shape: Option<u32>, out: &mut Vec<(Vec<Sk>, Rt2<N>)>| -> Result<()> {
        let Some(si) = shape.filter(|_| !cur.is_empty()) else { return Ok(()) };
        let h = *shapes.get(si as usize).ok_or_else(|| anyhow!("shape index {si} of no shape"))?;
        let skel = layer.skeletons.get(&h).ok_or_else(|| anyhow!("no skeleton of shape {h:016x}"))?;
        let rows: Vec<(Key, u32)> = cur.iter().map(|s| (s.key, s.cell(&geo))).collect();
        out.push((std::mem::take(cur), super::decode::decode_rows(space, skel, &rows)?));
        Ok(())
    };
    for s in layer.states_iter() {
        if !keep(s) {
            continue;
        }
        let si = s.shape(&geo);
        if cur_shape != Some(si) || cur.len() >= chunk {
            flush(&mut cur, cur_shape, &mut out)?;
            cur_shape = Some(si);
        }
        cur.push(s);
    }
    flush(&mut cur, cur_shape, &mut out)?;
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layer_of(entries: &[(u32, Key, u64)]) -> Layer {
        let mut l = Layer::default();
        let mut i = 0;
        while i < entries.len() {
            let r = entries[i].0;
            let j = i + entries[i..].iter().take_while(|e| e.0 == r).count();
            let keys: Vec<Key> = entries[i..j].iter().map(|e| e.1).collect();
            let masks: Vec<u64> = entries[i..j].iter().map(|e| e.2).collect();
            l.push_region(r, &keys, &masks).unwrap();
            i = j;
        }
        l
    }

    /// A frame file round-trips its states and wins and its frontier: wide
    /// keys, several regions and layers, a key with several cells, frontier
    /// blocks of both kinds past their sizes (the storage check passes);
    /// unsorted grown states are refused, another hull fails the check.
    #[test]
    fn frame_files_round_trip() {
        use crate::search::hull::{Arc as HArc, Hull};
        use super::super::hulls::{Grown, GROWN_BLOCK, NEW_BLOCK};
        let dir = std::env::temp_dir().join(format!("celeste-layer-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let a = layer_of(&[(3, (0, 5), 0b1011), (3, (0, 9), 1 << 63), (3, (1 << 40, 2), 7), (70, (0, 1), 1), (4095, (5, 5), 0xffff_0000)]);
        let wins = vec![(1u32, 1u64 << 63), (4, 0x0001_0000)];
        let adds = KeyAdditions::default();
        let meta = FrameMeta { frame: 7, shapes: &[], keys: &adds, emitted: &[] };
        let p = frame_path(&dir, 7);
        let st = |region: u32, key: (u64, u64), local: u8| Sk { region, key, local };
        let h = |a: u16, b: u16| Hull { x: HArc { start: a, last: b }, y: HArc { start: b, last: a } };
        let hulls: Vec<Hull> = (0..a.states() as u32).map(|i| h(i as u16, (i * 7) as u16)).collect();
        let mut grown = vec![(st(3, (0, 5), 0), 0, h(1, 2)), (st(3, (0, 5), 9), 0, h(65535, 0)), (st(3, (1 << 40, 1), 1), 0, h(3, 65535)), (st(70, (0, 0), 63), 2, h(0, 0)), (st(4096, (u64::MAX, u64::MAX), 5), 2, h(9, 9)), (st(5, (0, 1), 5), 6, h(9, 9))];
        let mut v = 1u64;
        let mut big: Vec<Grown> = (0..GROWN_BLOCK as u64 * 2 + 77)
            .map(|i| {
                v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                (st((v >> 50) as u32, (v >> 7, v), (v % 64) as u8), (i / 5000) as u16 + 7, h(v as u16, (v >> 16) as u16))
            })
            .collect();
        big.sort_unstable_by_key(|g| (g.1, g.0));
        big.dedup_by_key(|g| (g.1, g.0));
        grown.extend(big);
        // In runs, cut anywhere.
        let runs: Vec<&[Grown]> = vec![&grown[..2], &[], &grown[2..9], &grown[9..]];
        write_frame(&p, &meta, &a, &wins, &hulls, &runs[..], 3).unwrap();
        check_frame_file(&p, &a, &wins, &hulls, &runs[..], 2).unwrap();
        let f = FrameFile::<P8>::open(&p).unwrap();
        assert_eq!(f.header.states(), a.states());
        assert_eq!(f.header.wins(), 2);
        let (back, w) = f.layer(None, 1).unwrap();
        assert_eq!((back.iter().collect::<Vec<_>>(), w), (a.iter().collect::<Vec<_>>(), wins.clone()));
        assert_eq!(f.frontier(2).unwrap(), (hulls.clone(), grown.clone()));
        let mut moved = grown.clone();
        moved[5].2 = h(1, 1);
        assert!(check_frame_file(&p, &a, &wins, &hulls, &[&moved[..2], &[], &moved[2..9], &moved[9..]][..], 2).is_err(), "another hull");
        assert!(write_frame(&p, &meta, &a, &wins, &hulls, &[&[grown[1], grown[0]][..]][..], 2).is_err(), "unsorted");
        assert!(write_frame(&p, &meta, &a, &wins, &hulls, &[&grown[1..2], &grown[0..1]][..], 2).is_err(), "runs unsorted");
        // The new states' hulls past a block.
        let many = layer_of(&(0..(NEW_BLOCK as u64 + 5) / 64 + 1).map(|k| (9, (0, k), u64::MAX)).collect::<Vec<_>>());
        let mh: Vec<Hull> = (0..many.states() as u32).map(|i| h(i as u16, 3)).collect();
        write_frame(&p, &meta, &many, &[], &mh, &[] as &[&[Grown]], 2).unwrap();
        check_frame_file(&p, &many, &[], &mh, &[] as &[&[Grown]], 2).unwrap();
        assert_eq!(FrameFile::<P8>::open(&p).unwrap().frontier(3).unwrap(), (mh, Vec::new()));
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
