//! LAYERS (plans/architecture.md, "The forward"): a plain set of states
//! with the skeletons its rows decode with (the frontier's new states, the
//! next layer), and the FRAME FILES (`frames/f{frame}.bin`, `storage::file`):
//! per region a block of the frame's new entries (`encode_region`), and a
//! header (`FrameHeader`) with what the frame added to the tree's codebooks
//! and its skeletons. `Tree` reads a tree's frame files.

use anyhow::{anyhow, ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use celeste_core::arith::EngineNum;
use celeste_engine::exact::{Key, KeyAdditions, KeySpace};
use celeste_engine::runtime2::{mix64, Cell2, Col, Rt2, P8};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::file::{self, At, Mapped, FORMAT_VERSION};
use super::decode::Repr;
use super::set::{RegionSet, RegionTable};
use super::{Sk, GEOMETRY};

#[derive(Clone)]
pub struct Layer<N: EngineNum = P8> {
    pub set: RegionSet,
    /// Per shape (hash) its skeleton (`SkelRecord::rt2`).
    pub skeletons: FxHashMap<u64, Arc<Rt2<N>>>,
}

impl<N: EngineNum> Default for Layer<N> {
    fn default() -> Self {
        Layer { set: RegionSet::default(), skeletons: FxHashMap::default() }
    }
}

impl<N: EngineNum> Layer<N> {
    /// Append region `region`'s entries (after every region held; keys
    /// ascending, masks non-empty: checked).
    pub fn push_region(&mut self, region: u32, keys: &[Key], masks: &[u64]) -> Result<()> {
        ensure!(masks.iter().all(|&m| m != 0), "region {region}: an entry without cells");
        ensure!(region % GEOMETRY.slots != GEOMETRY.no_cell_slot() || masks.iter().all(|&m| m == 1), "region {region} (no player): a cell past 0");
        self.set.push(region, RegionTable::from_sorted(keys, masks)?)
    }

    pub fn entries(&self) -> usize {
        self.set.entries()
    }

    pub fn states(&self) -> u64 {
        self.set.states()
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    /// Every entry `(region, key, mask)`, in order.
    pub fn iter(&self) -> impl Iterator<Item = (u32, Key, u64)> + '_ {
        self.set.iter()
    }

    /// Every state, in order (region, key, cell).
    pub fn states_iter(&self) -> impl Iterator<Item = Sk> + '_ {
        self.set.states_iter()
    }

    pub fn mask_of(&self, region: u32, key: Key) -> u64 {
        self.set.mask_of(region, key)
    }

    pub fn contains(&self, s: Sk) -> bool {
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
pub fn bits(mut m: u64) -> impl Iterator<Item = u32> {
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
/// strings, prints, the non-value cells' uniform columns, and per value
/// cell how its column holds numbers (`Repr`: the code merges `Num(n)` and
/// `Ival(n, n)`). Equal records decode every key alike.
#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub struct SkelRecord<N = P8> {
    pub shape_hash: u64,
    pub structure: Vec<Cell2<N>>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub prints: Vec<String>,
    /// The cells that are not values, with their (uniform) columns.
    pub others: Vec<(u32, Col<N>)>,
    /// The value cells whose columns hold numbers (0) or intervals (1); the
    /// others any value (`Repr::Other`).
    pub numeric: Vec<(u32, u8)>,
}

impl<N: EngineNum> SkelRecord<N> {
    /// The skeleton of `rt2`'s shape (its values are not read).
    pub fn of(rt2: &Rt2<N>) -> Result<SkelRecord<N>> {
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
        Ok(SkelRecord { shape_hash: rt2.shape_hash, structure: rt2.structure.clone(), globals: rt2.globals.clone(), strings: rt2.strings.clone(), prints: rt2.prints.clone(), others, numeric })
    }

    /// The skeleton as an empty block over `cache` (a room's).
    pub fn rt2(&self, cache: Arc<celeste_core::collision_cache::CollisionCache>) -> Rt2<N> {
        let mut rt2 = Rt2::empty(0, self.globals.len(), &[], cache);
        rt2.structure = self.structure.clone();
        rt2.globals = self.globals.clone();
        rt2.strings = self.strings.clone();
        rt2.prints = self.prints.clone();
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
    pub fn rt2_in(&self, room: (i16, i16)) -> Rt2<N> {
        self.rt2(crate::compiled::collision_cache(room))
    }
}

const MAGIC: &[u8; 4] = b"CSL1";

pub fn frame_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("frames").join(format!("f{frame:03}.bin"))
}

pub fn frame_exists(dir: &Path, frame: u32) -> bool {
    frame_path(dir, frame).is_file()
}

/// The tree's last frame with a file (frames from 0 on, consecutive).
pub fn last_frame(dir: &Path) -> Option<u32> {
    let mut last = None;
    while frame_exists(dir, last.map_or(0, |f: u32| f + 1)) {
        last = Some(last.map_or(0, |f| f + 1));
    }
    last
}

/// One region's place in a frame file.
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub struct RegionIndex {
    pub region: u32,
    pub entries: u32,
    pub states: u32,
    pub wins: u32,
    pub at: At,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(bound = "")]
pub struct FrameHeader<N: EngineNum = P8> {
    pub frame: u32,
    /// The storage region side the tree is built with.
    pub side: u32,
    /// `(index, hash)` of the shapes numbered here.
    pub shapes: Vec<(u32, u64)>,
    pub keys: KeyAdditions<N>,
    /// The skeletons of the shapes the file holds states of.
    pub skeletons: Vec<SkelRecord<N>>,
    /// The skeletons first EMITTED at the frame of shapes it holds no state
    /// of: a re-push reaches such a shape's state later without emitting
    /// it - a resume registers every file's (`frame::restore_visited`).
    pub emitted: Vec<SkelRecord<N>>,
    /// Per region held (ascending) its block.
    pub regions: Vec<RegionIndex>,
}

impl<N: EngineNum> FrameHeader<N> {
    pub fn states(&self) -> u64 {
        self.regions.iter().map(|r| r.states as u64).sum()
    }

    pub fn wins(&self) -> u64 {
        self.regions.iter().map(|r| r.wins as u64).sum()
    }
}

/// What a frame file holds besides its states.
pub struct FrameMeta<'a, N: EngineNum = P8> {
    pub frame: u32,
    pub shapes: &'a [(u32, u64)],
    pub keys: &'a KeyAdditions<N>,
    /// `FrameHeader::emitted`.
    pub emitted: &'a [SkelRecord<N>],
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
pub struct RegionEntries {
    pub keys: Vec<Key>,
    pub masks: Vec<u64>,
    pub wins: Vec<(u32, u64)>,
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
/// skeletons, what the frame added to the codebooks. Returns its bytes.
pub fn write_frame<N: EngineNum>(path: &Path, meta: &FrameMeta<N>, layer: &Layer<N>, wins: &[(u32, u64)], threads: usize) -> Result<u64> {
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
    let ats = At::place(blocks.iter().map(|b| (b.0.len(), b.1)));
    let index: Vec<RegionIndex> = tables
        .iter()
        .zip(&win_at)
        .zip(ats)
        .map(|(((region, t), w), at)| RegionIndex { region: *region, entries: t.len() as u32, states: t.states() as u32, wins: wins[w.clone()].iter().map(|x| x.1.count_ones()).sum(), at })
        .collect();
    let mut skeletons: Vec<SkelRecord<N>> = layer.skeletons.values().map(|s| SkelRecord::of(s)).collect::<Result<_>>()?;
    skeletons.sort_by_key(|s| s.shape_hash);
    ensure!(meta.emitted.iter().all(|e| !layer.skeletons.contains_key(&e.shape_hash)), "write_frame: an emitted skeleton of a shape the layer holds");
    let header = FrameHeader { frame: meta.frame, side: GEOMETRY.side, shapes: meta.shapes.to_vec(), keys: meta.keys.clone(), skeletons, emitted: meta.emitted.to_vec(), regions: index };
    file::write(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &header, |f| {
        for (z, _) in &blocks {
            f.write_all(z)?;
        }
        Ok(())
    })
}

pub struct FrameFile<N: EngineNum = P8> {
    file: Mapped,
    pub header: FrameHeader<N>,
}

impl<N: EngineNum> FrameFile<N> {
    pub fn open(path: &Path) -> Result<Self> {
        let file = Mapped::open(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &format!("a frame file of the {} arithmetic", <N::A as celeste_core::arith::Arithmetic>::NAME))?;
        let header: FrameHeader<N> = file.header()?;
        ensure!(header.side == GEOMETRY.side, "{}: built with storage regions of {} cells, this process uses {}", path.display(), header.side, GEOMETRY.side);
        file.ends_at(header.regions.last().map_or(0, |r| r.at.end()))?;
        Ok(FrameFile { file, header })
    }

    pub fn path(&self) -> &Path {
        self.file.path()
    }

    /// Region `i` (of `header.regions`) decoded.
    pub fn region(&self, i: usize) -> Result<RegionEntries> {
        let r = &self.header.regions[i];
        let mut raw = Vec::new();
        self.file.block(r.at, &mut zstd::bulk::Decompressor::new()?, &mut raw).with_context(|| format!("region {}", r.region))?;
        let e = decode_region(&raw).with_context(|| format!("{}: region {}", self.path().display(), r.region))?;
        ensure!(e.keys.len() == r.entries as usize, "{}: region {}: {} entries, its index says {}", self.path().display(), r.region, e.keys.len(), r.entries);
        Ok(e)
    }

    /// Drop the resident pages of region `i`'s block (`Mapped::release`).
    pub fn release_region(&self, i: usize) -> Result<()> {
        self.file.release(self.file.stored(self.header.regions[i].at))
    }

    /// The file's states as a layer and its wins, its skeletons in a search
    /// from `room` (`None`: none - a reader of keys and cells only).
    pub fn layer(&self, room: Option<(i16, i16)>, threads: usize) -> Result<(Layer<N>, Vec<(u32, u64)>)> {
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

    pub fn wins(&self) -> Result<Vec<Sk>> {
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

/// THE COMPLETE STORAGE CHECK of a frame file just written: read back,
/// region by region, its states are exactly `layer`'s (its regions, every
/// entry's key and cells) and its wins `wins`. FATAL otherwise: the frame
/// would resume as another set than the forward expands.
pub fn check_frame_file<N: EngineNum>(path: &Path, layer: &Layer<N>, wins: &[(u32, u64)], threads: usize) -> Result<()> {
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
    Ok(())
}

/// A TREE's frame files through a frame: its codebooks (the shapes by
/// index, the key space: every file's additions, any order) and each
/// frame's file (none past a dead frontier: those frames are empty).
pub struct Tree<N: EngineNum = P8> {
    pub dir: PathBuf,
    pub last: u32,
    pub frames: Vec<Option<FrameFile<N>>>,
    pub shapes: Vec<u64>,
    pub space: KeySpace<N>,
}

impl<N: EngineNum> Tree<N> {
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
    pub fn file(&self, f: u32) -> Option<&FrameFile<N>> {
        self.frames.get(f as usize).and_then(|x| x.as_ref())
    }

    /// Frame `f`'s state count.
    pub fn states(&self, f: u32) -> u64 {
        self.file(f).map_or(0, |x| x.header.states())
    }

    /// The first frame with a win (`None`: none through `last`).
    pub fn first_win(&self) -> Option<u32> {
        self.frames.iter().position(|x| x.as_ref().is_some_and(|x| x.header.wins() > 0)).map(|f| f as u32)
    }

    pub fn shape_of(&self, region: u32) -> Result<u64> {
        self.shapes.get((region / GEOMETRY.slots) as usize).copied().ok_or_else(|| anyhow!("{}: region {region} of no shape", self.dir.display()))
    }

    /// A state as `(shape, key, cell)`.
    pub fn state(&self, s: Sk) -> Result<(u64, Key, u32)> {
        Ok((self.shape_of(s.region)?, s.key, s.cell(&GEOMETRY)))
    }

    /// A state of `shape` with `key` at `cell`, if the tree numbers the shape.
    pub fn sk(&self, shape: u64, key: Key, cell: u32) -> Option<Sk> {
        let i = self.shapes.iter().position(|&h| h == shape)?;
        Some(Sk::at(&GEOMETRY, i as u32, key, cell))
    }

    /// The cells of `(region, key)` frame `f` holds, reading that region only.
    pub fn mask_at(&self, f: u32, region: u32, key: Key) -> Result<u64> {
        let Some(file) = self.file(f) else { return Ok(0) };
        let Ok(i) = file.header.regions.binary_search_by_key(&region, |r| r.region) else { return Ok(0) };
        let e = file.region(i)?;
        Ok(e.keys.binary_search(&key).map_or(0, |k| e.masks[k]))
    }

    /// The frame holding state `s` among `0..=upto`, by searching their files.
    pub fn layer_of(&self, s: Sk, upto: u32) -> Result<Option<u32>> {
        for f in 0..=upto.min(self.last) {
            if self.mask_at(f, s.region, s.key)? >> s.local & 1 == 1 {
                return Ok(Some(f));
            }
        }
        Ok(None)
    }

    /// The skeleton of `shape` in frame `f`'s files, in a search from `room`.
    pub fn skeleton(&self, f: u32, shape: u64, room: (i16, i16)) -> Result<Rt2<N>> {
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
pub fn decode_states<N: EngineNum>(layer: &Layer<N>, space: &KeySpace<N>, shapes: &[u64], chunk: usize, keep: &dyn Fn(Sk) -> bool) -> Result<Vec<(Vec<Sk>, Rt2<N>)>> {
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

    /// A frame file round-trips its states and wins (the storage check passes).
    #[test]
    fn frame_files_round_trip() {
        let dir = std::env::temp_dir().join(format!("celeste-layer-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let a = layer_of(&[(3, (0, 5), 0b1011), (3, (0, 9), 1 << 63), (3, (1 << 40, 2), 7), (70, (0, 1), 1), (4095, (5, 5), 0xffff_0000)]);
        let wins = vec![(1u32, 1u64 << 63), (4, 0x0001_0000)];
        let adds = KeyAdditions::default();
        let meta = FrameMeta { frame: 7, shapes: &[], keys: &adds, emitted: &[] };
        let p = frame_path(&dir, 7);
        write_frame(&p, &meta, &a, &wins, 3).unwrap();
        check_frame_file(&p, &a, &wins, 2).unwrap();
        let f = FrameFile::<P8>::open(&p).unwrap();
        assert_eq!(f.header.states(), a.states());
        assert_eq!(f.header.wins(), 2);
        let (back, w) = f.layer(None, 1).unwrap();
        assert_eq!((back.iter().collect::<Vec<_>>(), w), (a.iter().collect::<Vec<_>>(), wins.clone()));
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
