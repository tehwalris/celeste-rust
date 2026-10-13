//! THE OUT-EDGES (`edges/out/f{F}.bin`): frame F's recorded edges once more,
//! by SOURCE - what the hull forward's RE-PUSH reads (`wave`): a state
//! expanded at its layer, whose hull grows later, pushes its new hull along
//! its edges again without a kernel run. The edge files proper (`edges`)
//! hold every edge in its TARGET's region (the backward's read); read by
//! source they cost a target region's whole blocks per lookup (room (1,0)
//! f80: 438M bundles decoded for 133M edges pushed). Here a source's edges
//! are one contiguous run, found by key.
//!
//! A file is per kernel UNIT of the frame (a run of entries of one shape,
//! in (region, key) order: the units are, so the file is too) a zstd BLOCK,
//! and a header listing each block's first and last entry. A block, raw:
//!
//!   * its ROUTES (target shape hash, slot) and its PAIRS (the transfers,
//!     `arc_edges::encode_pair`): a block names no global id, so it is final
//!     when its unit ends;
//!   * its TARGETS: (route, key), sorted, keys delta-coded within a route;
//!   * its ENTRIES (region, key), each with its bundles' byte length (a
//!     reader skips the entries it does not want) and its bundles: (target,
//!     transfer, offset, target cells), sorted - the edges from the entry's
//!     cells (a bundle's cells less its offset) only.
//!
//! A unit's targets may hold PROVISIONAL keys (codes new this frame): its
//! block is encoded again at the frame's end with the final keys (`fixup`).

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::bundle::{get_varint, get_varint128, put_varint, put_varint128};
use super::visited::Key;
use crate::search::arc_edges::{decode_pair, encode_pair, Pair, PAIR_BYTES};
use celeste_engine::exact::is_provisional;

const MAGIC: &[u8; 4] = b"CSO1";

#[inline]
fn wide(k: Key) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

#[inline]
fn narrow(w: u128) -> Key {
    ((w >> 64) as u64, w as u64)
}

/// One out-edge row as a unit makes it: the source entry `(region, key)`,
/// the target's route `(shape, slot)` and key, the transfer, the offset
/// (`bundle::offset`), the target cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Row {
    pub s_region: u32,
    pub s_key: Key,
    pub route: (u64, u32),
    pub t_key: Key,
    pub pair: Pair,
    pub o: u32,
    pub mask: u64,
}

/// One bundle of an entry, as a block holds it: its target and transfer by
/// their index in the block.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OutBundle {
    pub t: u32,
    pub x: u32,
    pub o: u32,
    pub mask: u64,
}

/// A block's content by index (what `encode_parts` writes): its routes,
/// pairs, targets `(route, key)` and entries `(region, key)`, each sorted
/// and distinct, and its rows `(entry, target, pair, offset) -> cells`
/// (any order; equal ones merged).
#[derive(Default)]
pub struct Parts {
    pub routes: Vec<(u64, u32)>,
    pub pairs: Vec<Pair>,
    pub targets: Vec<(u32, Key)>,
    pub entries: Vec<(u32, Key)>,
    pub rows: Vec<(u128, u64)>,
}

/// A row's sort key: entry, target, pair, offset.
#[inline]
pub fn row_key(entry: u32, t: u32, x: u32, o: u32) -> u128 {
    (entry as u128) << 96 | (t as u128) << 64 | (x as u128) << 32 | o as u128
}

impl Parts {
    pub fn clear(&mut self) {
        self.routes.clear();
        self.pairs.clear();
        self.targets.clear();
        self.entries.clear();
        self.rows.clear();
    }
}

/// ENCODE `p` as a raw block into `out` (cleared). Returns whether a
/// target key is provisional (the block waits for the frame's end:
/// `fixup`).
pub fn encode_parts(p: &mut Parts, out: &mut Vec<u8>) -> bool {
    out.clear();
    debug_assert!(p.routes.windows(2).all(|w| w[0] < w[1]) && p.pairs.windows(2).all(|w| w[0] < w[1]) && p.targets.windows(2).all(|w| w[0] < w[1]) && p.entries.windows(2).all(|w| w[0] < w[1]));
    p.rows.sort_unstable_by_key(|r| r.0);
    put_varint(out, p.routes.len() as u64);
    for &(shape, slot) in &p.routes {
        out.extend_from_slice(&shape.to_le_bytes());
        put_varint(out, slot as u64);
    }
    put_varint(out, p.pairs.len() as u64);
    for x in &p.pairs {
        encode_pair(out, x);
    }
    put_varint(out, p.targets.len() as u64);
    let mut provisional = false;
    let mut prev: Option<(u32, u128)> = None;
    for &(r, k) in &p.targets {
        provisional |= is_provisional(k);
        let w = wide(k);
        match prev {
            Some((pr, pk)) if pr == r => {
                out.push(0);
                put_varint128(out, w - pk);
            }
            _ => {
                put_varint(out, r as u64 + 1);
                put_varint128(out, w);
            }
        }
        prev = Some((r, w));
    }
    // The entries with rows, each with its bundles (merged, in order).
    let mut n_entries = 0u64;
    let mut body: Vec<u8> = Vec::new();
    let mut eb: Vec<u8> = Vec::new();
    let mut i = 0usize;
    let mut prev_e: Option<(u32, u128)> = None;
    while i < p.rows.len() {
        let e = (p.rows[i].0 >> 96) as u32;
        let j = i + p.rows[i..].partition_point(|r| (r.0 >> 96) as u32 == e);
        eb.clear();
        let mut n = 0u64;
        let mut pt = 0u32;
        let mut k = i;
        while k < j {
            let key = p.rows[k].0;
            let mut mask = 0u64;
            while k < j && p.rows[k].0 == key {
                mask |= p.rows[k].1;
                k += 1;
            }
            let (t, x, o) = ((key >> 64) as u32, (key >> 32) as u32, key as u32);
            put_varint(&mut eb, (t - pt) as u64);
            pt = t;
            put_varint(&mut eb, x as u64);
            super::bundle::put_off(&mut eb, o);
            eb.extend_from_slice(&mask.to_le_bytes());
            n += 1;
        }
        let (region, key) = p.entries[e as usize];
        let w = wide(key);
        match prev_e {
            Some((pr, pk)) if pr == region => {
                put_varint(&mut body, 0);
                put_varint128(&mut body, w - pk);
            }
            _ => {
                put_varint(&mut body, region as u64 - prev_e.map_or(0, |p| p.0 as u64) + 1);
                put_varint128(&mut body, w);
            }
        }
        prev_e = Some((region, w));
        let mut nb = Vec::with_capacity(10);
        put_varint(&mut nb, n);
        put_varint(&mut body, (nb.len() + eb.len()) as u64);
        body.extend_from_slice(&nb);
        body.extend_from_slice(&eb);
        n_entries += 1;
        i = j;
    }
    put_varint(out, n_entries);
    out.extend_from_slice(&body);
    provisional
}

/// Rows per block (`encode_blocks`): a re-push decompresses whole blocks
/// for the entries it wants, so blocks are small (room (1,0) f80 with one
/// block a kernel unit decompressed 5.3 GB for 2.6M re-pushed states).
pub const BLOCK_ROWS: usize = 1 << 11;

/// `p` as raw blocks of whole entries, about `BLOCK_ROWS` rows each, each
/// with the routes, pairs and targets it uses (`encode_parts` each);
/// whether a target key is provisional, per block.
pub fn encode_blocks(p: &mut Parts) -> Vec<(Vec<u8>, bool)> {
    p.rows.sort_unstable_by_key(|r| r.0);
    let mut out = Vec::new();
    let mut i = 0usize;
    let (mut t_new, mut x_new) = (vec![u32::MAX; p.targets.len()], vec![u32::MAX; p.pairs.len()]);
    while i < p.rows.len() {
        // Whole entries, up to the block's rows.
        let mut j = i;
        while j < p.rows.len() && (j - i < BLOCK_ROWS || (p.rows[j].0 >> 96) == (p.rows[j - 1].0 >> 96)) {
            j += 1;
        }
        let rows = &p.rows[i..j];
        let mut ts: Vec<u32> = rows.iter().map(|r| (r.0 >> 64) as u32).collect();
        ts.sort_unstable();
        ts.dedup();
        let mut xs: Vec<u32> = rows.iter().map(|r| (r.0 >> 32) as u32).collect();
        xs.sort_unstable();
        xs.dedup();
        let mut rs: Vec<u32> = ts.iter().map(|&t| p.targets[t as usize].0).collect();
        rs.sort_unstable();
        rs.dedup();
        let mut b = Parts { routes: rs.iter().map(|&r| p.routes[r as usize]).collect(), pairs: xs.iter().map(|&x| p.pairs[x as usize]).collect(), ..Default::default() };
        for (k, &t) in ts.iter().enumerate() {
            t_new[t as usize] = k as u32;
            let (r, key) = p.targets[t as usize];
            b.targets.push((rs.binary_search(&r).expect("a route of the block") as u32, key));
        }
        for (k, &x) in xs.iter().enumerate() {
            x_new[x as usize] = k as u32;
        }
        let e0 = (rows[0].0 >> 96) as u32;
        let e1 = (rows[rows.len() - 1].0 >> 96) as u32;
        b.entries = p.entries[e0 as usize..=e1 as usize].to_vec();
        b.rows = rows
            .iter()
            .map(|&(k, m)| {
                let (e, t, x, o) = ((k >> 96) as u32, (k >> 64) as u32, (k >> 32) as u32, k as u32);
                (row_key(e - e0, t_new[t as usize], x_new[x as usize], o), m)
            })
            .collect();
        let mut raw = Vec::new();
        let prov = encode_parts(&mut b, &mut raw);
        out.push((raw, prov));
        i = j;
    }
    out
}

/// ENCODE `rows` (any order; equal (entry, target, transfer, offset) rows
/// merged) as a raw block into `out` (cleared): `encode_parts` of their
/// indices. Returns whether a target key is provisional.
pub fn encode(rows: &[Row], out: &mut Vec<u8>) -> bool {
    let mut p = Parts::default();
    p.routes = rows.iter().map(|r| r.route).collect();
    p.routes.sort_unstable();
    p.routes.dedup();
    p.pairs = rows.iter().map(|r| r.pair).collect();
    p.pairs.sort_unstable();
    p.pairs.dedup();
    let route_of = |r: (u64, u32)| p.routes.binary_search(&r).expect("a route of the block") as u32;
    p.targets = rows.iter().map(|r| (route_of(r.route), r.t_key)).collect();
    p.targets.sort_unstable();
    p.targets.dedup();
    p.entries = rows.iter().map(|r| (r.s_region, r.s_key)).collect();
    p.entries.sort_unstable();
    p.entries.dedup();
    p.rows = rows
        .iter()
        .map(|r| {
            let e = p.entries.binary_search(&(r.s_region, r.s_key)).expect("an entry of the block") as u32;
            let t = p.targets.binary_search(&(route_of(r.route), r.t_key)).expect("a target of the block") as u32;
            let x = p.pairs.binary_search(&r.pair).expect("a pair of the block") as u32;
            (row_key(e, t, x, r.o), r.mask)
        })
        .collect();
    encode_parts(&mut p, out)
}

/// A raw block, its header read: routes, pairs, targets; `entries` walks
/// its entries.
pub struct Block<'a> {
    pub routes: Vec<(u64, u32)>,
    pub pairs: Vec<Pair>,
    /// (route, key) per target.
    pub targets: Vec<(u32, Key)>,
    body: &'a [u8],
    n: usize,
}

impl<'a> Block<'a> {
    pub fn open(raw: &'a [u8]) -> Result<Self> {
        let mut pos = 0usize;
        let nr = get_varint(raw, &mut pos) as usize;
        ensure!(nr <= raw.len(), "an out block of {nr} routes in {} bytes", raw.len());
        let mut routes = Vec::with_capacity(nr);
        for _ in 0..nr {
            ensure!(pos + 8 <= raw.len(), "an out block cut short");
            let shape = u64::from_le_bytes(raw[pos..pos + 8].try_into().unwrap());
            pos += 8;
            routes.push((shape, get_varint(raw, &mut pos) as u32));
        }
        let np = get_varint(raw, &mut pos) as usize;
        ensure!(pos + np * PAIR_BYTES <= raw.len(), "an out block cut short");
        let pairs: Vec<Pair> = raw[pos..pos + np * PAIR_BYTES].chunks_exact(PAIR_BYTES).map(decode_pair).collect();
        pos += np * PAIR_BYTES;
        let nt = get_varint(raw, &mut pos) as usize;
        ensure!(nt <= raw.len(), "an out block of {nt} targets in {} bytes", raw.len());
        let mut targets = Vec::with_capacity(nt);
        let (mut pr, mut pk) = (0u32, 0u128);
        for _ in 0..nt {
            let tag = get_varint(raw, &mut pos);
            let d = get_varint128(raw, &mut pos);
            if tag == 0 {
                pk += d;
            } else {
                (pr, pk) = (tag as u32 - 1, d);
            }
            ensure!((pr as usize) < routes.len(), "an out block's target of route {pr} past its {}", routes.len());
            targets.push((pr, narrow(pk)));
        }
        let n = get_varint(raw, &mut pos) as usize;
        ensure!(pos <= raw.len(), "an out block's header past its end");
        Ok(Block { routes, pairs, targets, body: &raw[pos..], n })
    }

    /// Entries in the block.
    pub fn len(&self) -> usize {
        self.n
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }

    /// Every entry `(region, key)` in order: `want(region, key)` says
    /// whether its bundles are read (`f(region, key, bundles)`) or skipped.
    pub fn entries(&self, mut want: impl FnMut(u32, Key) -> bool, mut f: impl FnMut(u32, Key, &mut dyn Iterator<Item = OutBundle>)) -> Result<()> {
        let b = self.body;
        let mut pos = 0usize;
        let (mut region, mut key) = (0u32, 0u128);
        for k in 0..self.n {
            ensure!(pos < b.len(), "an out block ends at its entry {k} of {}", self.n);
            let r = get_varint(b, &mut pos);
            let d = get_varint128(b, &mut pos);
            if r == 0 {
                ensure!(k > 0, "an out block's first entry continues no region");
                key += d;
            } else {
                region += r as u32 - 1;
                key = d;
            }
            let len = get_varint(b, &mut pos) as usize;
            ensure!(pos + len <= b.len(), "an out block's entry past its end");
            let (start, end) = (pos, pos + len);
            pos = end;
            if !want(region, narrow(key)) {
                continue;
            }
            let eb = &b[start..end];
            let mut q = 0usize;
            let n = get_varint(eb, &mut q) as usize;
            let mut t = 0u32;
            let mut it = (0..n).map(|_| {
                t += get_varint(eb, &mut q) as u32;
                let x = get_varint(eb, &mut q) as u32;
                let o = super::bundle::get_off(eb, &mut q);
                let mask = u64::from_le_bytes(eb[q..q + 8].try_into().unwrap());
                q += 8;
                OutBundle { t, x, o, mask }
            });
            f(region, narrow(key), &mut it);
            // The bundles not read are skipped with the entry.
        }
        ensure!(pos == b.len(), "an out block did not decode to its end ({pos} of {} bytes)", b.len());
        Ok(())
    }

    /// The block's rows (`Row`s, merged bundles), every entry read.
    pub fn rows(&self) -> Result<Vec<Row>> {
        let mut out = Vec::new();
        let mut bad = false;
        self.entries(
            |_, _| true,
            |region, key, it| {
                for b in it {
                    let (Some(&(r, t_key)), Some(&pair)) = (self.targets.get(b.t as usize), self.pairs.get(b.x as usize)) else {
                        bad = true;
                        return;
                    };
                    out.push(Row { s_region: region, s_key: key, route: self.routes[r as usize], t_key, pair, o: b.o, mask: b.mask });
                }
            },
        )?;
        ensure!(!bad, "an out block's bundle names a target or transfer past its tables");
        Ok(out)
    }
}

/// A raw block with PROVISIONAL target keys encoded again with their final
/// keys (`finals`, the frame's end's).
pub fn fixup(raw: &[u8], finals: &rustc_hash::FxHashMap<Key, Key>, out: &mut Vec<u8>) -> Result<()> {
    let mut rows = Block::open(raw)?.rows()?;
    for r in &mut rows {
        if is_provisional(r.t_key) {
            r.t_key = *finals.get(&r.t_key).ok_or_else(|| anyhow::anyhow!("an out block's provisional target without its final key"))?;
        }
    }
    let prov = encode(&rows, out);
    ensure!(!prov, "an out block still provisional after its fixup");
    Ok(())
}

/// One block's place in a file: its first and last entry, where it is.
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug)]
struct BlockHead {
    first: (u32, Key),
    last: (u32, Key),
    offset: u64,
    len: u32,
    raw: u32,
}

/// A block as the frame's end writes it: zstd, its raw length, its first
/// and last entry.
pub struct Sealed {
    pub z: Vec<u8>,
    pub raw: u32,
    pub first: (u32, Key),
    pub last: (u32, Key),
}

impl Sealed {
    /// `raw` compressed, its entry range read off it (`None`: no entry).
    pub fn of(raw: &[u8], z: &mut zstd::bulk::Compressor<'static>) -> Result<Option<Sealed>> {
        let b = Block::open(raw)?;
        let (mut first, mut last) = (None, None);
        b.entries(
            |r, k| {
                if first.is_none() {
                    first = Some((r, k));
                }
                last = Some((r, k));
                false
            },
            |_, _, _| {},
        )?;
        let (Some(first), Some(last)) = (first, last) else { return Ok(None) };
        ensure!(raw.len() < u32::MAX as usize, "an out block of {} bytes", raw.len());
        Ok(Some(Sealed { z: super::bundle::compress(raw, z)?, raw: raw.len() as u32, first, last }))
    }
}

/// Frame `frame`'s out file.
pub fn path(edges_dir: &Path, frame: u32) -> PathBuf {
    edges_dir.join("out").join(format!("f{frame:03}.bin"))
}

/// WRITE frame `frame`'s out file from its units' blocks in unit order
/// (their entries ascending across blocks: checked). Atomic. Returns its
/// bytes.
pub fn write(path: &Path, frame: u32, blocks: &[Sealed]) -> Result<u64> {
    use std::io::Write;
    ensure!(blocks.windows(2).all(|w| w[0].last < w[1].first), "{}: the out blocks' entries are not ascending", path.display());
    let mut heads = Vec::with_capacity(blocks.len());
    let mut offset = 0u64;
    for b in blocks {
        heads.push(BlockHead { first: b.first, last: b.last, offset, len: b.z.len() as u32, raw: b.raw });
        offset += b.z.len() as u64;
    }
    let hb = bincode::serialize(&(frame, heads)).context("an out file's header")?;
    std::fs::create_dir_all(path.parent().expect("an out dir"))?;
    let tmp = path.with_extension("tmp");
    {
        let mut f = std::io::BufWriter::with_capacity(1 << 20, std::fs::File::create(&tmp).with_context(|| tmp.display().to_string())?);
        f.write_all(MAGIC)?;
        f.write_all(&crate::search::checkpoint::FORMAT_VERSION.to_le_bytes())?;
        f.write_all(&(hb.len() as u64).to_le_bytes())?;
        f.write_all(&hb)?;
        for b in blocks {
            f.write_all(&b.z)?;
        }
        f.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(16 + hb.len() as u64 + offset)
}

/// One out file, mapped.
pub struct OutFile {
    map: memmap2::Mmap,
    heads: Vec<BlockHead>,
    data: usize,
}

impl OutFile {
    pub fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = std::fs::File::open(path).with_context(|| path.display().to_string())?;
        // SAFETY: an out file is written once (renamed into place) and never
        // modified.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        ensure!(map.len() >= 16 && &map[0..4] == MAGIC, "{}: not an out file", path.display());
        let v = u32::from_le_bytes(map[4..8].try_into().unwrap());
        ensure!(v == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {v}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
        let n = u64::from_le_bytes(map[8..16].try_into().unwrap()) as usize;
        ensure!(map.len() >= 16 + n, "{}: truncated header", path.display());
        let (f, heads): (u32, Vec<BlockHead>) = bincode::deserialize(&map[16..16 + n]).with_context(|| path.display().to_string())?;
        ensure!(f == frame, "{}: the out-edges of frame {f}, not {frame}", path.display());
        let data = 16 + n;
        let end = heads.last().map_or(0, |h| h.offset + h.len as u64);
        ensure!(map.len() as u64 == data as u64 + end, "{}: {} bytes, its header says {}", path.display(), map.len(), data as u64 + end);
        Ok(OutFile { map, heads, data })
    }

    /// The blocks that may hold entries in `lo..=hi`.
    pub fn blocks(&self, lo: (u32, Key), hi: (u32, Key)) -> std::ops::Range<usize> {
        let a = self.heads.partition_point(|h| h.last < lo);
        let b = self.heads.partition_point(|h| h.first <= hi);
        a..b.max(a)
    }

    /// Block `i`'s raw bytes into `out`.
    pub fn block(&self, i: usize, z: &mut zstd::bulk::Decompressor<'static>, out: &mut Vec<u8>) -> Result<()> {
        let h = &self.heads[i];
        let a = self.data + h.offset as usize;
        super::bundle::decompress(&self.map[a..a + h.len as usize], h.raw as usize, z, out)
    }

    /// Its blocks.
    pub fn len(&self) -> usize {
        self.heads.len()
    }

    pub fn is_empty(&self) -> bool {
        self.heads.is_empty()
    }
}

/// A tree's out files, by frame (none at frame 0: nothing leads into it).
#[derive(Default)]
pub struct OutStore {
    frames: Vec<Option<OutFile>>,
    pub bytes: u64,
}

impl OutStore {
    /// Frames `1..=horizon`'s out files under `edges_dir`.
    pub fn open(edges_dir: &Path, horizon: u32) -> Result<Self> {
        let mut s = OutStore { frames: vec![None], bytes: 0 };
        for f in 1..=horizon {
            s.add_frame(edges_dir, f)?;
        }
        Ok(s)
    }

    /// Frame `frame`'s out file (the next past the store's) mapped in.
    pub fn add_frame(&mut self, edges_dir: &Path, frame: u32) -> Result<()> {
        if self.frames.is_empty() {
            self.frames.push(None);
        }
        ensure!(frame as usize == self.frames.len(), "out store: frame {frame} after {}", self.frames.len() - 1);
        let f = OutFile::open(&path(edges_dir, frame), frame)?;
        self.bytes += f.map.len() as u64;
        self.frames.push(Some(f));
        Ok(())
    }

    /// Frame `frame`'s out file.
    pub fn file(&self, frame: u32) -> Option<&OutFile> {
        self.frames.get(frame as usize).and_then(|f| f.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A block round-trips its rows (merged), skips what is not wanted, and
    /// a fixup replaces provisional keys; a file finds its blocks by entry.
    #[test]
    fn out_blocks_round_trip() {
        let ax = |v: i32| crate::search::arc_edges::AxisXfer { lo: 0, hi: 1 << 16, tag: 0, val: v };
        let p = |v: i32| (ax(v), ax(-v));
        let prov = (celeste_engine::exact::PROVISIONAL | 3, 7);
        let row = |r: u32, k: u64, sh: u64, slot: u32, t: Key, v: i32, o: u32, mask: u64| Row { s_region: r, s_key: (0, k), route: (sh, slot), t_key: t, pair: p(v), o, mask };
        let mut rows = vec![
            row(5, 9, 77, 3, (0, 1), 4, 2049 << 11 | 1025, 0b11),
            row(5, 9, 77, 3, (0, 1), 4, 2049 << 11 | 1025, 0b100),
            row(5, 2, 77, 4, (1 << 40, 5), 1, 1024 << 11 | 1024, 1 << 63),
            row(7, 1, 12, 3, prov, 4, 1024 << 11 | 1030, 1),
            row(5, 2, 77, 3, (0, 1), 1, 1024 << 11 | 1024, 1),
        ];
        let mut raw = Vec::new();
        assert!(encode(&rows, &mut raw), "a provisional target");
        let b = Block::open(&raw).unwrap();
        assert_eq!(b.len(), 3);
        let mut got = b.rows().unwrap();
        got.sort_unstable();
        rows[0].mask = 0b111;
        rows.remove(1);
        rows.sort_unstable();
        assert_eq!(got, rows);
        let mut seen = Vec::new();
        b.entries(|r, k| r == 5 && k == (0, 9), |r, k, it| seen.push((r, k, it.count()))).unwrap();
        assert_eq!(seen, vec![(5, (0, 9), 1)]);
        let finals: rustc_hash::FxHashMap<Key, Key> = [(prov, (0, 3))].into_iter().collect();
        let mut fixed = Vec::new();
        fixup(&raw, &finals, &mut fixed).unwrap();
        assert!(Block::open(&fixed).unwrap().rows().unwrap().iter().any(|r| r.t_key == (0, 3)));
        // A file of two blocks.
        let dir = std::env::temp_dir().join(format!("celeste-out-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let mut z = zstd::bulk::Compressor::new(1).unwrap();
        let mut second = Vec::new();
        encode(&[row(9, 4, 77, 3, (0, 1), 4, 1024 << 11 | 1024, 1)], &mut second);
        let blocks = vec![Sealed::of(&fixed, &mut z).unwrap().unwrap(), Sealed::of(&second, &mut z).unwrap().unwrap()];
        let path = path(&dir, 3);
        write(&path, 3, &blocks).unwrap();
        let f = OutFile::open(&path, 3).unwrap();
        assert_eq!(f.blocks((5, (0, 0)), (5, (0, 9))), 0..1);
        assert_eq!(f.blocks((8, (0, 0)), (9, (0, 4))), 1..2);
        assert_eq!(f.blocks((7, (0, 2)), (8, (0, 0))), 1..1);
        let mut d = zstd::bulk::Decompressor::new().unwrap();
        let mut back = Vec::new();
        f.block(1, &mut d, &mut back).unwrap();
        assert_eq!(back, second);
        assert!(write(&path, 3, &[blocks[1].clone_for_test(), blocks[0].clone_for_test()]).is_err(), "unordered blocks");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    impl Sealed {
        fn clone_for_test(&self) -> Sealed {
            Sealed { z: self.z.clone(), raw: self.raw, first: self.first, last: self.last }
        }
    }
}
