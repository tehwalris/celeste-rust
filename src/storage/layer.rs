//! LAYERS (plans/keys-only.md): a set of states as the storage holds them -
//! per storage region (ascending) its ENTRIES, each a position-free exact
//! key (ascending in the region) with the mask of its cells. A state is
//! `(shape, key, cell)` and nothing else: a layer names no state by a
//! number. It is the one form a frame's states take
//!
//!   * in memory: the frontier a wave expands (its units decoded to rows
//!     right before the kernels: `wave`), the next layer a wave builds;
//!   * on disk: the FRAME FILES (`frames/f{frame}.bin`, a raise's additions
//!     `frames/f{frame}.r{seq}.bin`): per region a zstd block of its new
//!     entries' keys, masks and win masks, and a header with what the
//!     frame added to the tree's codebooks - the shapes it numbered, the
//!     key space's new codes and bit runs - and the skeletons its shapes'
//!     rows decode with (`SkelRecord`);
//!   * as level -1's drop notes (`dropped/f{frame}.bin`): per noted source
//!     entry and horizon, the cells.
//!
//! `Tree` reads a tree's frame files: the key space, the shapes, a frame's
//! layer, its rows decoded (the diagnostics), a state's layer.

use anyhow::{anyhow, ensure, Context, Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use celeste_engine::exact::{Key, KeyAdditions, KeySpace};
use celeste_engine::runtime2::{mix64, Cell2, Col, Rt2};

use super::bundle::{get_varint, get_varint128, put_varint, put_varint128};
use super::decode::Repr;
use super::set::{RegionSet, RegionTable};
use super::{Sk, GEOMETRY};

/// A LAYER: a frozen set of states (`set::RegionSet`: per region
/// (ascending) its entries, key (ascending) and cells) and the skeletons its
/// shapes' rows decode with.
#[derive(Default, Clone)]
pub struct Layer {
    pub set: RegionSet,
    /// Per shape (hash) its skeleton (`SkelRecord::rt2`).
    pub skeletons: FxHashMap<u64, Arc<Rt2>>,
}

impl Layer {
    /// Append region `region`'s entries (after every region held; keys
    /// ascending, masks non-empty: checked).
    pub fn push_region(&mut self, region: u32, keys: &[Key], masks: &[u64]) -> Result<()> {
        ensure!(masks.iter().all(|&m| m != 0), "region {region}: an entry without cells");
        ensure!(region % GEOMETRY.slots != GEOMETRY.no_cell_slot() || masks.iter().all(|&m| m == 1), "region {region} (no player): a cell past 0");
        self.set.push(region, RegionTable::from_sorted(keys, masks)?)
    }

    /// Entries held.
    pub fn entries(&self) -> usize {
        self.set.entries()
    }

    /// States held.
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

    /// The cells of `(region, key)` held (0: none).
    pub fn mask_of(&self, region: u32, key: Key) -> u64 {
        self.set.mask_of(region, key)
    }

    /// Does the layer hold state `s`?
    pub fn contains(&self, s: Sk) -> bool {
        self.set.contains(s)
    }

    /// `rewrite ckhash`'s `f` line over the layer: its states' count and the
    /// sum of their CONTENT hashes (`ContentHash::state` of (shape, key,
    /// cell)), each key hashed once - independent of the order the tree's
    /// dictionaries grew in (a raise). `shapes`: the tree's by index.
    pub fn content_fingerprint(&self, space: &KeySpace, shapes: &[u64]) -> Result<(u64, u64)> {
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

    /// The DISJOINT union with `other` (a frame and its raises: a state is
    /// in one of them; held twice is an error). Skeletons: both (equal
    /// where both have a shape: checked).
    pub fn union(self, other: Layer) -> Result<Layer> {
        if other.is_empty() && other.skeletons.is_empty() {
            return Ok(self);
        }
        let mut skeletons = self.skeletons.clone();
        for (h, s) in &other.skeletons {
            match skeletons.get(h) {
                Some(a) => ensure!(SkelRecord::of(a)? == SkelRecord::of(s)?, "two skeletons of shape {h:016x} in one frame"),
                None => {
                    skeletons.insert(*h, s.clone());
                }
            }
        }
        Ok(Layer { set: self.set.union(&other.set)?, skeletons })
    }

    /// Bytes allocated.
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
/// their keys do not say (plans/rows-from-keys.md) - structure with closure
/// captures, globals, strings, prints, the non-value cells' uniform
/// columns, and per value cell how its column holds numbers (`Repr`: the
/// code merges `Num(n)` and `Ival(n, n)`). Two skeletons of one shape are
/// one iff their records are equal: they decode every key alike.
#[derive(Serialize, Deserialize, Clone, PartialEq, Debug)]
pub struct SkelRecord {
    pub shape_hash: u64,
    pub structure: Vec<Cell2>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub prints: Vec<String>,
    /// The cells that are not values, with their (uniform) columns.
    pub others: Vec<(u32, Col)>,
    /// The value cells whose columns hold numbers (0) or intervals (1); the
    /// others any value (`Repr::Other`).
    pub numeric: Vec<(u32, u8)>,
}

impl SkelRecord {
    /// The skeleton of `rt2`'s shape (its values are not read).
    pub fn of(rt2: &Rt2) -> Result<SkelRecord> {
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

    /// The skeleton as an empty block over `cart` and `cache` (a room's).
    pub fn rt2(&self, cart: Arc<celeste_core::cart_data::CartData>, cache: Arc<celeste_core::collision_cache::CollisionCache>) -> Rt2 {
        let mut rt2 = Rt2::empty(0, self.globals.len(), &[], cart, cache);
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

    /// The skeleton in a search from `room` (its cart and collision cache).
    pub fn rt2_in(&self, room: (i16, i16)) -> Result<Rt2> {
        let (cart, cache) = crate::compiled::room_context(room)?;
        Ok(self.rt2(cart, cache))
    }
}

/// The frame files' and drop notes' magic.
const MAGIC: &[u8; 4] = b"CSL1";

/// A frame file's path: the wave's (`raise` None) or a raise's additions.
pub fn frame_path(dir: &Path, frame: u32, raise: Option<u32>) -> PathBuf {
    let f = dir.join("frames");
    match raise {
        None => f.join(format!("f{frame:03}.bin")),
        Some(seq) => f.join(format!("f{frame:03}.r{seq:04}.bin")),
    }
}

/// Is frame `frame` in the tree (its wave's file)?
pub fn frame_exists(dir: &Path, frame: u32) -> bool {
    frame_path(dir, frame, None).is_file()
}

/// The raise seqs of frame `frame`'s files, ascending.
pub fn raise_seqs(dir: &Path, frame: u32) -> Result<Vec<u32>> {
    let prefix = format!("f{frame:03}.r");
    let mut out = Vec::new();
    let fdir = dir.join("frames");
    for e in std::fs::read_dir(&fdir).with_context(|| fdir.display().to_string())? {
        let n = e?.file_name();
        let n = n.to_string_lossy();
        if let Some(rest) = n.strip_prefix(&prefix).and_then(|r| r.strip_suffix(".bin")) {
            out.push(rest.parse::<u32>().with_context(|| format!("{}: a frame file name", n))?);
        }
    }
    out.sort_unstable();
    Ok(out)
}

/// The last frame of the tree in `dir` with a file (frames from 0 on,
/// consecutive); `None`: none.
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
    /// The block's bytes from the data's start, stored and raw.
    pub offset: u64,
    pub len: u32,
    pub raw: u32,
}

/// A frame file's header.
#[derive(Serialize, Deserialize, Debug)]
pub struct FrameHeader {
    pub frame: u32,
    /// The storage region side the tree is built with.
    pub side: u32,
    /// `(index, hash)` of the shapes numbered here.
    pub shapes: Vec<(u32, u64)>,
    /// What the key space gained here.
    pub keys: KeyAdditions,
    /// The skeletons of the shapes the file holds states of.
    pub skeletons: Vec<SkelRecord>,
    /// Per region held (ascending) its block.
    pub regions: Vec<RegionIndex>,
}

impl FrameHeader {
    pub fn states(&self) -> u64 {
        self.regions.iter().map(|r| r.states as u64).sum()
    }

    pub fn wins(&self) -> u64 {
        self.regions.iter().map(|r| r.wins as u64).sum()
    }
}

/// What a frame file holds besides its states.
pub struct FrameMeta<'a> {
    pub frame: u32,
    pub shapes: &'a [(u32, u64)],
    pub keys: &'a KeyAdditions,
}

/// One region's block: its entries' keys (delta-coded, ascending), masks
/// and win masks (sparse: entry delta, mask).
fn encode_region(keys: &[Key], masks: &[u64], wins: &[(u32, u64)], out: &mut Vec<u8>) {
    out.clear();
    put_varint(out, keys.len() as u64);
    let mut prev = 0u128;
    for (i, k) in keys.iter().enumerate() {
        let w = (k.0 as u128) << 64 | k.1 as u128;
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
        keys.push(((w >> 64) as u64, w as u64));
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

/// WRITE a frame file (`frame_path`): `layer`'s states with their win
/// masks (`wins`: sorted entry indices of `layer` with a win, and the win
/// cells), its skeletons, what the frame added to the codebooks. Regions
/// compressed in parallel. Atomic (a `tmp-` sibling renamed into place).
/// Returns its bytes.
pub fn write_frame(path: &Path, meta: &FrameMeta, layer: &Layer, wins: &[(u32, u64)], threads: usize) -> Result<u64> {
    use std::io::Write;
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
    let blocks: Vec<Result<(Vec<u8>, u32)>> = super::wave::par_map(&(0..tables.len()).collect::<Vec<_>>(), threads, |&i| {
        let t = &tables[i].1;
        let w: Vec<(u32, u64)> = wins[win_at[i].clone()].iter().map(|&(e, m)| (e - starts[i] as u32, m)).collect();
        let keys: Vec<Key> = t.iter().map(|x| x.0).collect();
        let mut raw = Vec::new();
        let masks: Vec<u64> = t.masks().collect();
        encode_region(&keys, &masks, &w, &mut raw);
        Ok((zstd::bulk::compress(&raw, 1)?, raw.len() as u32))
    });
    let mut index = Vec::with_capacity(tables.len());
    let mut offset = 0u64;
    let mut data = Vec::new();
    for (i, b) in blocks.into_iter().enumerate() {
        let (z, raw) = b?;
        let (region, t) = &tables[i];
        let w = &wins[win_at[i].clone()];
        index.push(RegionIndex {
            region: *region,
            entries: t.len() as u32,
            states: t.states() as u32,
            wins: w.iter().map(|x| x.1.count_ones()).sum(),
            offset,
            len: z.len() as u32,
            raw,
        });
        offset += z.len() as u64;
        data.push(z);
    }
    let mut skeletons: Vec<SkelRecord> = layer.skeletons.values().map(|s| SkelRecord::of(s)).collect::<Result<_>>()?;
    skeletons.sort_by_key(|s| s.shape_hash);
    let header = FrameHeader { frame: meta.frame, side: GEOMETRY.side, shapes: meta.shapes.to_vec(), keys: meta.keys.clone(), skeletons, regions: index };
    let hb = bincode::serialize(&header).context("a frame file's header")?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!("tmp-{}", path.file_name().and_then(|s| s.to_str()).unwrap_or("frame.bin")));
    let mut bytes = 16 + hb.len() as u64;
    {
        let mut f = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        f.write_all(MAGIC)?;
        f.write_all(&crate::search::checkpoint::FORMAT_VERSION.to_le_bytes())?;
        f.write_all(&(hb.len() as u64).to_le_bytes())?;
        f.write_all(&hb)?;
        for z in &data {
            f.write_all(z)?;
            bytes += z.len() as u64;
        }
        f.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(bytes)
}

/// A frame file, mapped, its header decoded.
pub struct FrameFile {
    map: memmap2::Mmap,
    pub header: FrameHeader,
    data: usize,
    path: PathBuf,
}

impl FrameFile {
    pub fn open(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path).with_context(|| path.display().to_string())?;
        // SAFETY: a frame file is written once (renamed into place) and
        // never modified.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 16 && &map[0..4] == MAGIC, "{}: not a frame file", path.display());
        let version = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(version == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {version}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
        let hl = u64::from_le_bytes(map[8..16].try_into().unwrap()) as usize;
        ensure!(map.len() >= 16 + hl, "{}: truncated header", path.display());
        let header: FrameHeader = bincode::deserialize(&map[16..16 + hl]).with_context(|| format!("{}: its header", path.display()))?;
        ensure!(header.side == GEOMETRY.side, "{}: built with storage regions of {} cells, this process uses {}", path.display(), header.side, GEOMETRY.side);
        let data = 16 + hl;
        let end = header.regions.last().map_or(0, |r| r.offset + r.len as u64);
        ensure!(map.len() as u64 == data as u64 + end, "{}: {} bytes, its index says {}", path.display(), map.len(), data as u64 + end);
        Ok(FrameFile { map, header, data, path: path.to_path_buf() })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Region `i` (of `header.regions`) decoded.
    pub fn region(&self, i: usize) -> Result<RegionEntries> {
        let r = &self.header.regions[i];
        let a = self.data + r.offset as usize;
        let raw = zstd::bulk::decompress(&self.map[a..a + r.len as usize], r.raw as usize).with_context(|| format!("{}: region {}", self.path.display(), r.region))?;
        ensure!(raw.len() == r.raw as usize, "{}: region {} decompressed to {} bytes, not {}", self.path.display(), r.region, raw.len(), r.raw);
        let e = decode_region(&raw).with_context(|| format!("{}: region {}", self.path.display(), r.region))?;
        ensure!(e.keys.len() == r.entries as usize, "{}: region {}: {} entries, its index says {}", self.path.display(), r.region, e.keys.len(), r.entries);
        Ok(e)
    }

    /// The file's states as a layer, its skeletons in a search from `room`
    /// (`None`: none - a reader of keys and cells only). Regions decoded on
    /// `threads` threads.
    pub fn layer(&self, room: Option<(i16, i16)>, threads: usize) -> Result<(Layer, Vec<(u32, u64)>)> {
        let n = self.header.regions.len();
        let parts: Vec<Result<RegionEntries>> = super::wave::par_map(&(0..n).collect::<Vec<_>>(), threads, |&i| self.region(i));
        let mut layer = Layer::default();
        let mut wins = Vec::new();
        for (i, p) in parts.into_iter().enumerate() {
            let p = p?;
            let base = layer.entries() as u32;
            layer.push_region(self.header.regions[i].region, &p.keys, &p.masks).with_context(|| self.path.display().to_string())?;
            wins.extend(p.wins.into_iter().map(|(e, m)| (base + e, m)));
        }
        ensure!(layer.set.tables().len() == n, "{}: an empty region", self.path.display());
        if let Some(room) = room {
            for s in &self.header.skeletons {
                layer.skeletons.insert(s.shape_hash, Arc::new(s.rt2_in(room)?));
            }
        }
        Ok((layer, wins))
    }

    /// The win states.
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

/// THE COMPLETE STORAGE CHECK of a frame file just written: read back from
/// disk, region by region (in parallel, no copy of the whole frame), its
/// states are exactly `layer`'s - its regions, every entry's key and cells
/// - and its wins `wins`. FATAL otherwise: the frame would resume as
/// another set than the forward expands.
pub fn check_frame_file(path: &Path, layer: &Layer, wins: &[(u32, u64)], threads: usize) -> Result<()> {
    let f = FrameFile::open(path)?;
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

/// A frame's files: its wave's, then its raises' (by seq).
pub fn frame_files(dir: &Path, frame: u32) -> Result<Vec<FrameFile>> {
    let mut out = vec![FrameFile::open(&frame_path(dir, frame, None))?];
    for seq in raise_seqs(dir, frame)? {
        out.push(FrameFile::open(&frame_path(dir, frame, Some(seq)))?);
    }
    Ok(out)
}

/// A level -1 DROP NOTE: a noted source entry `(region, key)`, the
/// smallest horizon admitting one of its dropped successors, and the cells
/// it holds for (`dropped/f{frame}.bin`, sorted by (region, key, horizon)).
pub type Note = (u32, Key, u32, u64);

/// Frame `frame`'s drop notes' file.
pub fn notes_path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("dropped").join(format!("f{frame:03}.bin"))
}

/// Write drop notes (sorted, each (region, key, horizon) once; a cell in
/// one note per (region, key): checked), zstd. Atomic.
pub fn write_notes(path: &Path, notes: &[Note]) -> Result<()> {
    use std::io::Write;
    ensure!(notes.windows(2).all(|w| (w[0].0, w[0].1, w[0].2) < (w[1].0, w[1].1, w[1].2)), "drop notes not sorted");
    let mut raw = Vec::new();
    put_varint(&mut raw, notes.len() as u64);
    let mut prev: Option<(u32, Key)> = None;
    let mut cells = 0u64;
    for &(region, key, h, mask) in notes {
        ensure!(mask != 0, "a drop note without cells");
        if prev.is_some_and(|p| p.0 == region && p.1 == key) {
            ensure!(cells & mask == 0, "a source noted twice in region {region}");
        } else {
            cells = 0;
        }
        cells |= mask;
        put_varint(&mut raw, region as u64);
        raw.extend_from_slice(&key.0.to_le_bytes());
        raw.extend_from_slice(&key.1.to_le_bytes());
        put_varint(&mut raw, h as u64);
        raw.extend_from_slice(&mask.to_le_bytes());
        prev = Some((region, key));
    }
    let z = zstd::bulk::compress(&raw, 1)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_file_name(format!("tmp-{}", path.file_name().and_then(|s| s.to_str()).unwrap_or("notes.bin")));
    {
        let mut f = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        f.write_all(MAGIC)?;
        f.write_all(&crate::search::checkpoint::FORMAT_VERSION.to_le_bytes())?;
        f.write_all(&(raw.len() as u64).to_le_bytes())?;
        f.write_all(&z)?;
        f.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// Read drop notes (`write_notes`).
pub fn read_notes(path: &Path) -> Result<Vec<Note>> {
    let b = std::fs::read(path).with_context(|| path.display().to_string())?;
    ensure!(b.len() >= 16 && &b[0..4] == MAGIC, "{}: not a drop notes file", path.display());
    let version = u32::from_le_bytes(b[4..8].try_into().unwrap());
    ensure!(version == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {version}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
    let n_raw = u64::from_le_bytes(b[8..16].try_into().unwrap()) as usize;
    let raw = zstd::bulk::decompress(&b[16..], n_raw).with_context(|| path.display().to_string())?;
    let mut pos = 0usize;
    let n = get_varint(&raw, &mut pos) as usize;
    let mut out = Vec::with_capacity(n);
    for _ in 0..n {
        let region = get_varint(&raw, &mut pos) as u32;
        ensure!(pos + 16 <= raw.len(), "{}: cut short", path.display());
        let key = (u64::from_le_bytes(raw[pos..pos + 8].try_into().unwrap()), u64::from_le_bytes(raw[pos + 8..pos + 16].try_into().unwrap()));
        pos += 16;
        let h = get_varint(&raw, &mut pos) as u32;
        ensure!(pos + 8 <= raw.len(), "{}: cut short", path.display());
        let mask = u64::from_le_bytes(raw[pos..pos + 8].try_into().unwrap());
        pos += 8;
        out.push((region, key, h, mask));
    }
    ensure!(pos == raw.len(), "{}: bytes past its notes", path.display());
    Ok(out)
}

/// Notes (any order; each source cell at most once) sorted, the notes of
/// one (region, key, horizon) merged.
pub fn notes_of(mut notes: Vec<Note>) -> Result<Vec<Note>> {
    notes.sort_unstable_by_key(|n| (n.0, n.1, n.2));
    let mut out: Vec<Note> = Vec::with_capacity(notes.len());
    for n in notes {
        match out.last_mut() {
            Some(m) if (m.0, m.1, m.2) == (n.0, n.1, n.2) => {
                ensure!(m.3 & n.3 == 0, "a source of region {} noted twice", n.0);
                m.3 |= n.3;
            }
            _ => out.push(n),
        }
    }
    Ok(out)
}

/// A TREE's frame files through a frame: its codebooks (the shapes by
/// index, the key space: every file's additions, any order) and each
/// frame's files. A forward whose frontier died has no frames past the
/// empty one: they are empty.
pub struct Tree {
    pub dir: PathBuf,
    pub last: u32,
    /// Per frame `0..=last` its files (none past a dead frontier).
    pub frames: Vec<Vec<FrameFile>>,
    pub shapes: Vec<u64>,
    pub space: KeySpace,
}

impl Tree {
    /// The tree in `dir` through frame `last`.
    pub fn open(dir: &Path, last: u32) -> Result<Tree> {
        let mut frames: Vec<Vec<FrameFile>> = Vec::with_capacity(last as usize + 1);
        let mut adds = KeyAdditions::default();
        let mut shapes: Vec<(u32, u64)> = Vec::new();
        for f in 0..=last {
            if !frame_exists(dir, f) {
                let died = frames.last().is_some_and(|fs| fs.iter().all(|x| x.header.states() == 0));
                ensure!(died, "{}: no frame f{f}, and the frontier before it was not empty", dir.display());
                frames.push(Vec::new());
                continue;
            }
            let fs = frame_files(dir, f)?;
            for x in &fs {
                ensure!(x.header.frame == f, "{}: of frame {}, not {f}", x.path.display(), x.header.frame);
                shapes.extend(x.header.shapes.iter().copied());
                adds.extend(x.header.keys.clone());
            }
            frames.push(fs);
        }
        shapes.sort_unstable();
        ensure!(shapes.iter().enumerate().all(|(i, s)| s.0 as usize == i), "{}: the shapes are not numbered 0..{}", dir.display(), shapes.len());
        let space = KeySpace::from_additions(adds).with_context(|| format!("{}: the key space", dir.display()))?;
        Ok(Tree { dir: dir.to_path_buf(), last, frames, shapes: shapes.into_iter().map(|s| s.1).collect(), space })
    }

    /// The tree in `dir` through its last frame (`last_frame`).
    pub fn open_all(dir: &Path) -> Result<Tree> {
        let last = last_frame(dir).ok_or_else(|| anyhow!("{}: no frames", dir.display()))?;
        Tree::open(dir, last)
    }

    /// Frame `f`'s states (its files' union), skeletons in a search from
    /// `room` if given, and its wins by entry.
    pub fn layer(&self, f: u32, room: Option<(i16, i16)>, threads: usize) -> Result<(Layer, Vec<Sk>)> {
        let files = self.frames.get(f as usize).ok_or_else(|| anyhow!("{}: no frame {f} (the tree is open to {})", self.dir.display(), self.last))?;
        let mut out = Layer::default();
        let mut wins = Vec::new();
        for file in files {
            let (l, w) = file.layer(room, threads)?;
            let starts = l.set.entry_starts();
            for &(e, m) in &w {
                let (ti, local) = l.set.locate(&starts, e as usize)?;
                let (region, t) = &l.set.tables()[ti];
                let key = t.key(local);
                wins.extend(bits(m).map(|c| Sk { region: *region, key, local: c as u8 }));
            }
            out = out.union(l)?;
        }
        Ok((out, wins))
    }

    /// Frame `f`'s states.
    pub fn states(&self, f: u32) -> u64 {
        self.frames.get(f as usize).map_or(0, |fs| fs.iter().map(|x| x.header.states()).sum())
    }

    /// The first frame with a win (`None`: none through `last`).
    pub fn first_win(&self) -> Option<u32> {
        self.frames.iter().position(|fs| fs.iter().any(|x| x.header.wins() > 0)).map(|f| f as u32)
    }

    /// The shape of region `region`.
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

    /// The cells of `(region, key)` frame `f` holds (0: none), reading
    /// that region's blocks only.
    pub fn mask_at(&self, f: u32, region: u32, key: Key) -> Result<u64> {
        let mut m = 0u64;
        for file in self.frames.get(f as usize).map_or(&[][..], |v| v.as_slice()) {
            if let Ok(i) = file.header.regions.binary_search_by_key(&region, |r| r.region) {
                let e = file.region(i)?;
                if let Ok(k) = e.keys.binary_search(&key) {
                    m |= e.masks[k];
                }
            }
        }
        Ok(m)
    }

    /// The frame (layer) holding state `s` among frames `0..=upto`, by a
    /// search of their files (diagnostics).
    pub fn layer_of(&self, s: Sk, upto: u32) -> Result<Option<u32>> {
        for f in 0..=upto.min(self.last) {
            if self.mask_at(f, s.region, s.key)? >> s.local & 1 == 1 {
                return Ok(Some(f));
            }
        }
        Ok(None)
    }

    /// The skeleton of `shape` in frame `f`'s files, in a search from `room`.
    pub fn skeleton(&self, f: u32, shape: u64, room: (i16, i16)) -> Result<Rt2> {
        let files = self.frames.get(f as usize).map_or(&[][..], |v| v.as_slice());
        let r = files.iter().flat_map(|x| x.header.skeletons.iter()).find(|r| r.shape_hash == shape).ok_or_else(|| anyhow!("{}: frame {f} has no skeleton of shape {shape:016x}", self.dir.display()))?;
        r.rt2_in(room)
    }

    /// Frame `f`'s ROWS decoded in a search from `room`: per shape, chunks
    /// of at most `chunk` states, each the states (in order) and their rows
    /// (row keys set); only the states `keep` admits.
    pub fn rows(&self, f: u32, room: (i16, i16), chunk: usize, keep: &dyn Fn(Sk) -> bool, threads: usize) -> Result<Vec<(Vec<Sk>, Rt2)>> {
        let (layer, _) = self.layer(f, Some(room), threads)?;
        decode_states(&layer, &self.space, &self.shapes, chunk, keep)
    }

    /// One state's row, decoded from its frame `f` (its region's block
    /// only) in a search from `room`.
    pub fn row(&self, f: u32, s: Sk, room: (i16, i16)) -> Result<Rt2> {
        ensure!(self.mask_at(f, s.region, s.key)? >> s.local & 1 == 1, "{}: frame {f} does not hold state {}", self.dir.display(), s.show());
        let shape = self.shape_of(s.region)?;
        let skel = self.skeleton(f, shape, room)?;
        super::decode::decode_rows(&self.space, &skel, &[(s.key, s.cell(&GEOMETRY))])
    }
}

/// `layer`'s states (those `keep` admits) DECODED: per shape, chunks of at
/// most `chunk` states and their rows, in the layer's order.
pub fn decode_states(layer: &Layer, space: &KeySpace, shapes: &[u64], chunk: usize, keep: &dyn Fn(Sk) -> bool) -> Result<Vec<(Vec<Sk>, Rt2)>> {
    let geo = GEOMETRY;
    let mut out = Vec::new();
    let mut cur: Vec<Sk> = Vec::new();
    let mut cur_shape: Option<u32> = None;
    let flush = |cur: &mut Vec<Sk>, shape: Option<u32>, out: &mut Vec<(Vec<Sk>, Rt2)>| -> Result<()> {
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

    /// A frame file round-trips its states and wins exactly (the storage
    /// check passes), and a raise's file joins its frame's as a disjoint
    /// union; a state in both is refused.
    #[test]
    fn frame_files_round_trip_and_union() {
        let dir = std::env::temp_dir().join(format!("celeste-layer-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let a = layer_of(&[(3, (0, 5), 0b1011), (3, (0, 9), 1 << 63), (3, (1 << 40, 2), 7), (70, (0, 1), 1), (4095, (5, 5), 0xffff_0000)]);
        let wins = vec![(1u32, 1u64 << 63), (4, 0x0001_0000)];
        let adds = KeyAdditions::default();
        let meta = FrameMeta { frame: 7, shapes: &[], keys: &adds };
        let p = frame_path(&dir, 7, None);
        write_frame(&p, &meta, &a, &wins, 3).unwrap();
        check_frame_file(&p, &a, &wins, 2).unwrap();
        let f = FrameFile::open(&p).unwrap();
        assert_eq!(f.header.states(), a.states());
        assert_eq!(f.header.wins(), 2);
        let (back, w) = f.layer(None, 1).unwrap();
        assert_eq!((back.iter().collect::<Vec<_>>(), w), (a.iter().collect::<Vec<_>>(), wins.clone()));
        let b = layer_of(&[(3, (0, 5), 0b0100), (5, (0, 0), 1)]);
        let u = a.clone().union(b).unwrap();
        assert_eq!(u.states(), a.states() + 2);
        assert_eq!(u.mask_of(3, (0, 5)), 0b1111);
        assert!(u.contains(Sk { region: 5, key: (0, 0), local: 0 }));
        assert!(a.clone().union(layer_of(&[(3, (0, 5), 0b0001)])).is_err());
        let notes = notes_of(vec![(3, (0, 5), 90, 0b10), (70, (0, 1), 91, 1), (3, (0, 5), 90, 0b01), (3, (0, 5), 95, 0b1000)]).unwrap();
        assert_eq!(notes, vec![(3, (0, 5), 90, 0b11), (3, (0, 5), 95, 0b1000), (70, (0, 1), 91, 1)]);
        assert!(notes_of(vec![(3, (0, 5), 90, 0b10), (3, (0, 5), 90, 0b11)]).is_err(), "a cell noted twice");
        let np = notes_path(&dir, 7);
        write_notes(&np, &notes).unwrap();
        assert_eq!(read_notes(&np).unwrap(), notes);
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
