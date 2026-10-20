//! THE OUT-EDGES (`edges/out/f{F}.bin`): frame F's edges once more, by
//! SOURCE, for the hull forward's RE-PUSH (`wave`: a grown hull pushed along
//! a state's edges without a kernel run). By target (`edges`) a lookup
//! decodes a region's whole blocks (~3 bundles an edge pushed); here a
//! source's edges are one run, found by key.
//!
//! A file (`storage::file`): the frame's kernel units' BLOCKS (whole
//! entries, ~`BLOCK_ROWS` rows each; in (region, key) order, as the units
//! are) and a header of each block's first and last entry. A block, raw:
//!
//!   * its ROUTES (target shape hash, slot) and PAIRS (`Rem::encode_pair`):
//!     no global id, so a block is final when its unit ends;
//!   * its TARGETS (route, key), sorted, keys delta-coded within a route;
//!   * its ENTRIES (region, key), each with its bundles' byte length (to skip
//!     it) and its bundles (target, transfer, offset, target cells), sorted:
//!     only edges from the entry's cells (a bundle's cells less its offset).
//!
//! A unit's targets may hold PROVISIONAL keys (codes new this frame): its
//! blocks are encoded again at the frame's end (`fixup`).

use anyhow::{ensure, Result};
use std::path::{Path, PathBuf};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::file::{self, At, Mapped, FORMAT_VERSION};
use super::visited::Key;
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::exact::is_provisional;

const MAGIC: &[u8; 4] = b"CSO1";

/// One out-edge row as a unit makes it (`o`: `bundle::offset`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Row<N: Rem = P8> {
    pub(crate) s_region: u32,
    pub(crate) s_key: Key,
    pub(crate) route: (u64, u32),
    pub(crate) t_key: Key,
    pub(crate) pair: N::Pair,
    pub(crate) o: u32,
    pub(crate) mask: u64,
}

/// One bundle of an entry as a block holds it: target, transfer by index.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct OutBundle {
    pub(crate) t: u32,
    pub(crate) x: u32,
    pub(crate) o: u32,
    pub(crate) mask: u64,
}

/// An entry's bundles, decoded as read (`Block::entries`). A named type,
/// not a `dyn Iterator`, so the re-push's loop is statically dispatched (a
/// trait object cost ~0.7% of a frame's instructions undevirtualized).
pub(crate) struct OutBundles<'a> {
    eb: &'a [u8],
    q: usize,
    t: u32,
    left: usize,
}

impl Iterator for OutBundles<'_> {
    type Item = OutBundle;

    #[inline]
    fn next(&mut self) -> Option<OutBundle> {
        if self.left == 0 {
            return None;
        }
        self.left -= 1;
        self.t += get_varint(self.eb, &mut self.q) as u32;
        let x = get_varint(self.eb, &mut self.q) as u32;
        let o = super::bundle::get_off(self.eb, &mut self.q);
        let mask = u64::from_le_bytes(self.eb[self.q..self.q + 8].try_into().unwrap());
        self.q += 8;
        Some(OutBundle { t: self.t, x, o, mask })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

/// A block's content by index (`encode_parts`): its routes, pairs, targets
/// `(route, key)` and entries `(region, key)`, each sorted and distinct;
/// its rows `(entry, target, pair, offset) -> cells`, any order, merged.
pub(crate) struct Parts<N: Rem = P8> {
    pub(crate) routes: Vec<(u64, u32)>,
    pub(crate) pairs: Vec<N::Pair>,
    pub(crate) targets: Vec<(u32, Key)>,
    pub(crate) entries: Vec<(u32, Key)>,
    pub(crate) rows: Vec<(u128, u64)>,
}

/// A row's sort key: entry, target, pair, offset.
#[inline]
pub(crate) fn row_key(entry: u32, t: u32, x: u32, o: u32) -> u128 {
    (entry as u128) << 96 | (t as u128) << 64 | (x as u128) << 32 | o as u128
}

impl<N: Rem> Default for Parts<N> {
    fn default() -> Self {
        Parts { routes: Vec::new(), pairs: Vec::new(), targets: Vec::new(), entries: Vec::new(), rows: Vec::new() }
    }
}

impl<N: Rem> Parts<N> {
    pub(crate) fn clear(&mut self) {
        self.routes.clear();
        self.pairs.clear();
        self.targets.clear();
        self.entries.clear();
        self.rows.clear();
    }
}

/// ENCODE `p` as a raw block into `out` (cleared). Returns whether a
/// target key is provisional (the block waits for `fixup`).
pub(crate) fn encode_parts<N: Rem>(p: &mut Parts<N>, out: &mut Vec<u8>) -> bool {
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
        N::encode_pair(out, x);
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
/// for the entries it wants, so blocks are small (one block a unit
/// decompressed ~2 KB a re-pushed state).
pub(crate) const BLOCK_ROWS: usize = 1 << 11;

/// `p` as raw blocks of whole entries, about `BLOCK_ROWS` rows each, each
/// with the routes, pairs and targets it uses (`encode_parts` each);
/// whether a target key is provisional, per block.
pub(crate) fn encode_blocks<N: Rem>(p: &mut Parts<N>) -> Vec<(Vec<u8>, bool)> {
    // By entry (a counting sort: a unit's entries are few), then each
    // entry's rows by key.
    let mut at = vec![0usize; p.entries.len() + 1];
    for r in &p.rows {
        at[(r.0 >> 96) as usize + 1] += 1;
    }
    for e in 1..at.len() {
        at[e] += at[e - 1];
    }
    let mut fill = at.clone();
    let mut by = vec![(0u128, 0u64); p.rows.len()];
    for &r in &p.rows {
        let e = (r.0 >> 96) as usize;
        by[fill[e]] = r;
        fill[e] += 1;
    }
    for e in 0..p.entries.len() {
        by[at[e]..at[e + 1]].sort_unstable_by_key(|r| r.0);
    }
    p.rows = by;
    let mut out = Vec::new();
    let mut i = 0usize;
    let (mut t_new, mut x_new) = (vec![u32::MAX; p.targets.len()], vec![u32::MAX; p.pairs.len()]);
    // Per target and pair, the last block holding it (each listed once a
    // block, then sorted: far fewer than its ~2000 rows).
    let (mut t_in, mut x_in) = (vec![u32::MAX; p.targets.len()], vec![u32::MAX; p.pairs.len()]);
    let mut block = 0u32;
    while i < p.rows.len() {
        let mut j = i;
        while j < p.rows.len() && (j - i < BLOCK_ROWS || (p.rows[j].0 >> 96) == (p.rows[j - 1].0 >> 96)) {
            j += 1;
        }
        let rows = &p.rows[i..j];
        let (mut ts, mut xs): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
        for r in rows {
            let (t, x) = ((r.0 >> 64) as u32, (r.0 >> 32) as u32);
            if t_in[t as usize] != block {
                t_in[t as usize] = block;
                ts.push(t);
            }
            if x_in[x as usize] != block {
                x_in[x as usize] = block;
                xs.push(x);
            }
        }
        block += 1;
        ts.sort_unstable();
        xs.sort_unstable();
        let mut rs: Vec<u32> = ts.iter().map(|&t| p.targets[t as usize].0).collect();
        rs.sort_unstable();
        rs.dedup();
        let mut b = Parts::<N> { routes: rs.iter().map(|&r| p.routes[r as usize]).collect(), pairs: xs.iter().map(|&x| p.pairs[x as usize]).collect(), ..Default::default() };
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

/// ENCODE `rows` (any order; equal but for their cells: merged) as a raw
/// block into `out` (cleared). Returns whether a target key is provisional.
pub(crate) fn encode<N: Rem>(rows: &[Row<N>], out: &mut Vec<u8>) -> bool {
    let mut p = Parts::<N>::default();
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

/// A raw block, its header read; `entries` walks its entries.
pub(crate) struct Block<'a, N: Rem = P8> {
    pub(crate) routes: Vec<(u64, u32)>,
    pub(crate) pairs: Vec<N::Pair>,
    /// (route, key) per target.
    pub(crate) targets: Vec<(u32, Key)>,
    body: &'a [u8],
    n: usize,
}

impl<'a, N: Rem> Block<'a, N> {
    pub(crate) fn open(raw: &'a [u8]) -> Result<Self> {
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
        ensure!(pos + np * N::PAIR_BYTES <= raw.len(), "an out block cut short");
        let pairs: Vec<N::Pair> = raw[pos..pos + np * N::PAIR_BYTES].chunks_exact(N::PAIR_BYTES).map(N::decode_pair).collect();
        pos += np * N::PAIR_BYTES;
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

    pub(crate) fn len(&self) -> usize {
        self.n
    }

    /// Every entry `(region, key)` in order: `want(region, key)` says
    /// whether its bundles are read (`f(region, key, bundles)`) or skipped.
    pub(crate) fn entries(&self, mut want: impl FnMut(u32, Key) -> bool, mut f: impl FnMut(u32, Key, &mut OutBundles<'_>)) -> Result<()> {
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
            let left = get_varint(eb, &mut q) as usize;
            f(region, narrow(key), &mut OutBundles { eb, q, t: 0, left });
            // The bundles not read are skipped with the entry.
        }
        ensure!(pos == b.len(), "an out block did not decode to its end ({pos} of {} bytes)", b.len());
        Ok(())
    }

    /// The block's rows (`Row`s, merged bundles), every entry read.
    pub(crate) fn rows(&self) -> Result<Vec<Row<N>>> {
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

/// A raw block with PROVISIONAL target keys encoded again with `finals`.
pub(crate) fn fixup<N: Rem>(raw: &[u8], finals: &rustc_hash::FxHashMap<Key, Key>, out: &mut Vec<u8>) -> Result<()> {
    let mut rows = Block::<N>::open(raw)?.rows()?;
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
    at: At,
}

/// A block as the frame's end writes it (zstd) with its entry range.
pub(crate) struct Sealed {
    /// In memory, or in the frame's spool.
    pub(crate) z: super::scratch::Body,
    pub(crate) raw: u32,
    pub(crate) first: (u32, Key),
    pub(crate) last: (u32, Key),
}

impl Sealed {
    /// `raw` compressed, its entry range read off it (`None`: no entry).
    pub(crate) fn of<N: Rem>(raw: &[u8], z: &mut zstd::bulk::Compressor<'static>) -> Result<Option<Sealed>> {
        let b = Block::<N>::open(raw)?;
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
        Ok(Some(Sealed { z: super::scratch::Body::Mem(file::compress(raw, z)?), raw: raw.len() as u32, first, last }))
    }
}

impl Sealed {
    /// Its bytes into `spool`, if there is one.
    pub(crate) fn spill(self, spool: Option<&super::scratch::Scratch>) -> Result<Sealed> {
        Ok(Sealed { z: self.z.spill(spool)?, ..self })
    }
}

pub(crate) fn path(edges_dir: &Path, frame: u32) -> PathBuf {
    edges_dir.join("out").join(format!("f{frame:03}.bin"))
}

/// WRITE frame `frame`'s out file from its units' blocks in order (entries
/// ascending across blocks: checked). Atomic. Returns its bytes.
pub(crate) fn write(path: &Path, frame: u32, blocks: &[Sealed], spool: Option<&super::scratch::Scratch>) -> Result<u64> {
    ensure!(blocks.windows(2).all(|w| w[0].last < w[1].first), "{}: the out blocks' entries are not ascending", path.display());
    let ats = At::place(blocks.iter().map(|b| (b.z.len(), b.raw as usize)));
    let heads: Vec<BlockHead> = blocks.iter().zip(ats).map(|(b, at)| BlockHead { first: b.first, last: b.last, at }).collect();
    let n = bincode::serialized_size(&(frame, &heads))?;
    ensure!(n == (12 + heads.len() * HEAD_BYTES) as u64, "an out file's header of {n} bytes for {} blocks: not the layout `OutFile` reads", heads.len());
    file::write(path, *MAGIC, FORMAT_VERSION, &(frame, &heads), |f| {
        let mut buf = Vec::new();
        for b in blocks {
            b.z.with(spool, &mut buf, |z| f.write_all(z))??;
        }
        Ok(())
    })
}

pub(crate) struct OutFile {
    file: Mapped,
    /// Its blocks (their heads read in place in the header: an index of
    /// every frame's blocks in memory takes 64 B a block).
    blocks: usize,
}

/// Bytes a `BlockHead` takes in a header (bincode 1's fixed-width little-
/// endian encoding, `write`'s): first (u32, u64, u64), last (the same),
/// offset u64, len u32, raw u32.
const HEAD_BYTES: usize = 56;

impl OutFile {
    pub(crate) fn open(path: &Path, frame: u32) -> Result<Self> {
        let file = Mapped::open(path, *MAGIC, FORMAT_VERSION, "an out file")?;
        let h = file.header_bytes();
        let n = h.len();
        ensure!(n >= 12, "{}: truncated header", path.display());
        let f = u32::from_le_bytes(h[0..4].try_into().unwrap());
        let blocks = u64::from_le_bytes(h[4..12].try_into().unwrap()) as usize;
        ensure!(n == 12 + blocks * HEAD_BYTES, "{}: a header of {n} bytes for {blocks} blocks", path.display());
        ensure!(f == frame, "{}: the out-edges of frame {f}, not {frame}", path.display());
        let out = OutFile { file, blocks };
        out.file.ends_at(if blocks == 0 { 0 } else { out.head(blocks - 1).at.end() })?;
        Ok(out)
    }

    #[inline]
    fn head(&self, i: usize) -> BlockHead {
        let b = &self.file.header_bytes()[12 + i * HEAD_BYTES..12 + (i + 1) * HEAD_BYTES];
        let (u32_at, u64_at) = (|a: usize| u32::from_le_bytes(b[a..a + 4].try_into().unwrap()), |a: usize| u64::from_le_bytes(b[a..a + 8].try_into().unwrap()));
        BlockHead { first: (u32_at(0), (u64_at(4), u64_at(12))), last: (u32_at(20), (u64_at(24), u64_at(32))), at: At { offset: u64_at(40), len: u32_at(48), raw: u32_at(52) } }
    }

    /// The blocks that may hold entries in `lo..=hi`.
    pub(crate) fn blocks(&self, lo: (u32, Key), hi: (u32, Key)) -> std::ops::Range<usize> {
        let point = |f: &dyn Fn(&BlockHead) -> bool| {
            let (mut a, mut b) = (0usize, self.blocks);
            while a < b {
                let m = (a + b) / 2;
                if f(&self.head(m)) {
                    a = m + 1;
                } else {
                    b = m;
                }
            }
            a
        };
        let a = point(&|h| h.last < lo);
        let b = point(&|h| h.first <= hi);
        a..b.max(a)
    }

    /// Block `i`'s first and last entry.
    pub(crate) fn range(&self, i: usize) -> ((u32, Key), (u32, Key)) {
        let h = self.head(i);
        (h.first, h.last)
    }

    pub(crate) fn block(&self, i: usize, z: &mut zstd::bulk::Decompressor<'static>, out: &mut Vec<u8>) -> Result<()> {
        self.file.block(self.head(i).at, z, out)
    }

}

/// A tree's out files, by frame (none at frame 0: nothing leads into it).
#[derive(Default)]
pub(crate) struct OutStore {
    frames: Vec<Option<OutFile>>,
    pub(crate) bytes: u64,
}

impl OutStore {

    /// Frames `1..=horizon`'s out files under `edges_dir`.
    pub(crate) fn open(edges_dir: &Path, horizon: u32) -> Result<Self> {
        let mut s = OutStore { frames: vec![None], bytes: 0 };
        for f in 1..=horizon {
            s.add_frame(edges_dir, f)?;
        }
        Ok(s)
    }

    /// Frame `frame`'s out file (the next past the store's) mapped in.
    pub(crate) fn add_frame(&mut self, edges_dir: &Path, frame: u32) -> Result<()> {
        if self.frames.is_empty() {
            self.frames.push(None);
        }
        ensure!(frame as usize == self.frames.len(), "out store: frame {frame} after {}", self.frames.len() - 1);
        let f = OutFile::open(&path(edges_dir, frame), frame)?;
        self.bytes += f.file.len();
        self.frames.push(Some(f));
        Ok(())
    }

    pub(crate) fn file(&self, frame: u32) -> Option<&OutFile> {
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
        let row = |r: u32, k: u64, sh: u64, slot: u32, t: Key, v: i32, o: u32, mask: u64| Row::<P8> { s_region: r, s_key: (0, k), route: (sh, slot), t_key: t, pair: p(v), o, mask };
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
        fixup::<P8>(&raw, &finals, &mut fixed).unwrap();
        assert!(Block::<P8>::open(&fixed).unwrap().rows().unwrap().iter().any(|r| r.t_key == (0, 3)));
        // A file of two blocks.
        let dir = std::env::temp_dir().join(format!("celeste-out-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let mut z = zstd::bulk::Compressor::new(1).unwrap();
        let mut second = Vec::new();
        encode(&[row(9, 4, 77, 3, (0, 1), 4, 1024 << 11 | 1024, 1)], &mut second);
        let blocks = vec![Sealed::of::<P8>(&fixed, &mut z).unwrap().unwrap(), Sealed::of::<P8>(&second, &mut z).unwrap().unwrap()];
        let path = path(&dir, 3);
        write(&path, 3, &blocks, None).unwrap();
        let f = OutFile::open(&path, 3).unwrap();
        assert_eq!(f.blocks((5, (0, 0)), (5, (0, 9))), 0..1);
        assert_eq!(f.blocks((8, (0, 0)), (9, (0, 4))), 1..2);
        assert_eq!(f.blocks((7, (0, 2)), (8, (0, 0))), 1..1);
        let mut d = zstd::bulk::Decompressor::new().unwrap();
        let mut back = Vec::new();
        f.block(1, &mut d, &mut back).unwrap();
        assert_eq!(back, second);
        assert!(write(&path, 3, &[blocks[1].clone_for_test(), blocks[0].clone_for_test()], None).is_err(), "unordered blocks");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    impl Sealed {
        fn clone_for_test(&self) -> Sealed {
            let z = match &self.z {
                crate::storage::scratch::Body::Mem(v) => crate::storage::scratch::Body::Mem(v.clone()),
                crate::storage::scratch::Body::Scratch(s) => crate::storage::scratch::Body::Scratch(*s),
            };
            Sealed { z, raw: self.raw, first: self.first, last: self.last }
        }
    }
}
