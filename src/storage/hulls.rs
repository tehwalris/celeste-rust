//! THE HULL FILES (`hulls/f{F}.bin`): frame F's FRONTIER as the hull
//! forward left it - every state whose remainder hull GREW at F: the NEW
//! states (first reached at F: frame F's file's states, in its order) by
//! their hulls alone, and the older states the frame pushed further
//! (GROWN, sorted by layer and state) with their layers and hulls. A resume
//! replays them in order (`frame::restore_visited`): a state's hull is its
//! last file's, and frame F's file is the frontier the next wave expands. A
//! file is written once (renamed into place) and read back before the
//! frame is trusted (`check`).
//!
//! Format: `CSH2`, the checkpoint format's version, the header's length and
//! the header (bincode: the frame, and per block its part, offset, lengths
//! and states), then the zstd blocks - the new states' hulls in runs of
//! `NEW_BLOCK` (8 B each), then the grown states in runs of `GROWN_BLOCK`,
//! each its layer, region, key (deltas from the state before it in the
//! block where they share the fields before) and cell, and its hull.
//! Blocks are written and read in parallel.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::bundle::{get_varint, get_varint128, put_varint, put_varint128};
use super::Sk;
use crate::search::hull::{Hull, HULL_BYTES};

const MAGIC: &[u8; 4] = b"CSH2";

/// New states' hulls a block.
const NEW_BLOCK: usize = 1 << 18;

/// Grown states a block.
const GROWN_BLOCK: usize = 1 << 16;

/// A frontier state: the state, its layer, its hull.
pub type Grown = (Sk, u16, Hull);

/// GROWN STATES in runs, sorted by layer and state across them (`write`,
/// `check`): a frontier's old states by layer, or states as they are.
pub trait Runs: Sync {
    /// The runs.
    fn count(&self) -> usize;
    /// Run `i`'s states.
    fn len(&self, i: usize) -> usize;
    /// Run `i`'s states `r`, in order.
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown>;
}

impl Runs for [&[Grown]] {
    fn count(&self) -> usize {
        <[&[Grown]]>::len(self)
    }
    fn len(&self, i: usize) -> usize {
        self[i].len()
    }
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown> {
        self[i][r].to_vec()
    }
}

impl Runs for [super::wave::OldLayer] {
    fn count(&self) -> usize {
        <[super::wave::OldLayer]>::len(self)
    }
    fn len(&self, i: usize) -> usize {
        self[i].len()
    }
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown> {
        self[i].range(r).map(|o| (o.sk, o.layer, o.hull)).collect()
    }
}

/// Frame `frame`'s hull file.
pub fn path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("hulls").join(format!("f{frame:03}.bin"))
}

fn wide(k: (u64, u64)) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

/// One block of a hull file: of the new states (`grown` false) or of the
/// grown ones.
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug)]
struct Block {
    grown: bool,
    offset: u64,
    len: u32,
    raw: u32,
    states: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct Header {
    frame: u32,
    blocks: Vec<Block>,
}

/// A HULL FILE's content: frame `frame`'s new states' hulls (in the
/// frame's state order) and its grown states (sorted by layer and state).
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Frontier {
    pub frame: u32,
    pub hulls: Vec<Hull>,
    pub grown: Vec<Grown>,
}

impl Frontier {
    /// Every state of the frontier with its layer and hull, the new ones
    /// (frame `frame`'s layer `new`, its states in order) first.
    pub fn states(&self, new: &super::layer::Layer) -> Result<Vec<Grown>> {
        ensure!(new.states() == self.hulls.len() as u64, "frame {}: {} new states, its hull file holds {} hulls", self.frame, new.states(), self.hulls.len());
        let layer = u16::try_from(self.frame).context("a frame past u16")?;
        let mut out: Vec<Grown> = new.states_iter().zip(&self.hulls).map(|(s, &h)| (s, layer, h)).collect();
        out.extend_from_slice(&self.grown);
        Ok(out)
    }
}

/// A grown block's bytes.
fn encode_grown(states: impl Iterator<Item = Grown>, n: usize) -> Vec<u8> {
    let mut raw = Vec::with_capacity(n * 14 + 8);
    put_varint(&mut raw, n as u64);
    let mut prev: Option<(u16, u32, u128)> = None;
    for (s, layer, h) in states {
        let k = wide(s.key);
        match prev {
            Some((pl, pr, pk)) if pl == layer => {
                put_varint(&mut raw, 0);
                if pr == s.region {
                    put_varint(&mut raw, 0);
                    put_varint128(&mut raw, k - pk);
                } else {
                    put_varint(&mut raw, (s.region - pr) as u64);
                    put_varint128(&mut raw, k);
                }
            }
            _ => {
                put_varint(&mut raw, layer as u64 - prev.map_or(0, |p| p.0 as u64) + prev.is_none() as u64);
                put_varint(&mut raw, s.region as u64);
                put_varint128(&mut raw, k);
            }
        }
        raw.push(s.local);
        raw.extend_from_slice(&h.to_bytes());
        prev = Some((layer, s.region, k));
    }
    raw
}

/// A grown block decoded.
fn decode_grown(raw: &[u8], want: usize) -> Result<Vec<Grown>> {
    let mut pos = 0usize;
    let n = get_varint(raw, &mut pos) as usize;
    ensure!(n == want, "{n} grown states, the index says {want}");
    let mut out: Vec<Grown> = Vec::with_capacity(n);
    let mut prev: Option<(u16, u32, u128)> = None;
    for _ in 0..n {
        ensure!(pos < raw.len(), "cut short");
        let dl = get_varint(raw, &mut pos);
        let (layer, region, key) = match prev {
            Some((pl, pr, pk)) if dl == 0 => {
                let dr = get_varint(raw, &mut pos);
                if dr == 0 {
                    (pl, pr, pk + get_varint128(raw, &mut pos))
                } else {
                    (pl, pr + dr as u32, get_varint128(raw, &mut pos))
                }
            }
            _ => {
                let layer = prev.map_or(0, |p| p.0 as u64) + dl - prev.is_none() as u64;
                ensure!(layer <= u16::MAX as u64, "a layer past u16");
                (layer as u16, get_varint(raw, &mut pos) as u32, get_varint128(raw, &mut pos))
            }
        };
        ensure!(pos + 1 + HULL_BYTES <= raw.len(), "cut short");
        let local = raw[pos];
        let h = Hull::from_bytes(&raw[pos + 1..pos + 1 + HULL_BYTES]);
        pos += 1 + HULL_BYTES;
        out.push((Sk { region, key: ((key >> 64) as u64, key as u64), local }, layer, h));
        prev = Some((layer, region, key));
    }
    ensure!(pos == raw.len(), "bytes past its states");
    Ok(out)
}

/// A hull file's blocks: per block whether it holds grown states, its run
/// and its range there (`write`'s runs; the new states' hulls are run 0 of
/// their own).
fn cuts<R: Runs + ?Sized>(hulls: &[Hull], runs: &R) -> Vec<(bool, usize, std::ops::Range<usize>)> {
    let mut cuts: Vec<(bool, usize, std::ops::Range<usize>)> = (0..hulls.len()).step_by(NEW_BLOCK).map(|a| (false, 0, a..(a + NEW_BLOCK).min(hulls.len()))).collect();
    for i in 0..runs.count() {
        let n = runs.len(i);
        cuts.extend((0..n).step_by(GROWN_BLOCK).map(|a| (true, i, a..(a + GROWN_BLOCK).min(n))));
    }
    cuts
}

/// Write frame `frame`'s frontier: its new states' `hulls` (in its state
/// order) and its grown states in `runs` (all of them sorted by layer and
/// state, distinct: checked), blocks on `threads` threads. Atomic. Returns
/// the file's bytes.
pub fn write<R: Runs + ?Sized>(path: &Path, frame: u32, hulls: &[Hull], runs: &R, threads: usize) -> Result<u64> {
    use std::io::Write;
    let key = |g: &Grown| (g.1, g.0);
    let cuts = cuts(hulls, runs);
    // Per block its bytes, and a grown block's sorted states' first and last
    // (`None`: unsorted).
    type Made = (Vec<u8>, u32, Option<Option<((u16, Sk), (u16, Sk))>>);
    let blocks: Vec<Result<Made>> = super::wave::par_map(&cuts, threads, |(g, i, r)| {
        let (raw, ends) = if *g {
            let v = runs.get(*i, r.clone());
            let ends = v.windows(2).all(|w| key(&w[0]) < key(&w[1])).then(|| v.first().map(|f| (key(f), key(v.last().expect("a first")))));
            (encode_grown(v.into_iter(), r.len()), ends)
        } else {
            (hulls[r.clone()].iter().flat_map(|h| h.to_bytes()).collect(), Some(None))
        };
        Ok((zstd::bulk::compress(&raw, 1)?, raw.len() as u32, ends))
    });
    let blocks: Vec<Made> = blocks.into_iter().collect::<Result<_>>()?;
    let mut last: Option<(u16, Sk)> = None;
    let mut sorted = true;
    for b in &blocks {
        match b.2 {
            None => sorted = false,
            Some(Some((first, end))) => {
                sorted &= last.is_none_or(|l| l < first);
                last = Some(end);
            }
            Some(None) => {}
        }
    }
    ensure!(sorted, "{}: the grown states not sorted by layer and state, and distinct", path.display());
    let mut header = Header { frame, blocks: Vec::with_capacity(cuts.len()) };
    let mut data = Vec::with_capacity(cuts.len());
    let mut offset = 0u64;
    for ((g, _, r), b) in cuts.iter().zip(blocks) {
        let (z, raw, _) = b;
        header.blocks.push(Block { grown: *g, offset, len: z.len() as u32, raw, states: r.len() as u32 });
        offset += z.len() as u64;
        data.push(z);
    }
    let hb = bincode::serialize(&header).context("a hull file's header")?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_extension("tmp");
    {
        let mut f = std::io::BufWriter::new(std::fs::File::create(&tmp).with_context(|| tmp.display().to_string())?);
        f.write_all(MAGIC)?;
        f.write_all(&crate::search::checkpoint::FORMAT_VERSION.to_le_bytes())?;
        f.write_all(&(hb.len() as u64).to_le_bytes())?;
        f.write_all(&hb)?;
        for z in &data {
            f.write_all(z)?;
        }
        f.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(16 + hb.len() as u64 + offset)
}

/// A block of a hull file (`data`: the bytes past its header) decoded: its
/// hulls (a new states' block) or its grown states.
fn decode(path: &Path, data: &[u8], x: &Block) -> Result<(Vec<Hull>, Vec<Grown>)> {
    let z = &data[x.offset as usize..(x.offset + x.len as u64) as usize];
    let raw = zstd::bulk::decompress(z, x.raw as usize).with_context(|| format!("{}: a block", path.display()))?;
    ensure!(raw.len() == x.raw as usize, "{}: a block of {} raw bytes, its index says {}", path.display(), raw.len(), x.raw);
    if x.grown {
        Ok((Vec::new(), decode_grown(&raw, x.states as usize).with_context(|| format!("{}: a grown block", path.display()))?))
    } else {
        ensure!(raw.len() == x.states as usize * HULL_BYTES, "{}: a hulls block of {} bytes for {} states", path.display(), raw.len(), x.states);
        Ok((raw.chunks_exact(HULL_BYTES).map(Hull::from_bytes).collect(), Vec::new()))
    }
}

/// A hull file's bytes, its header checked: the header, and where its
/// blocks start.
fn open(path: &Path, b: &[u8]) -> Result<(Header, usize)> {
    ensure!(b.len() >= 16 && &b[0..4] == MAGIC, "{}: not a hull file", path.display());
    let version = u32::from_le_bytes(b[4..8].try_into().unwrap());
    ensure!(version == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {version}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
    let hl = u64::from_le_bytes(b[8..16].try_into().unwrap()) as usize;
    ensure!(b.len() >= 16 + hl, "{}: truncated header", path.display());
    let header: Header = bincode::deserialize(&b[16..16 + hl]).with_context(|| format!("{}: its header", path.display()))?;
    let data = 16 + hl;
    let end = header.blocks.last().map_or(0, |x| x.offset + x.len as u64);
    ensure!(b.len() as u64 == data as u64 + end, "{}: {} bytes, its index says {}", path.display(), b.len(), data as u64 + end);
    ensure!(header.blocks.windows(2).all(|w| w[0].grown <= w[1].grown), "{}: a new states' block after a grown one", path.display());
    Ok((header, data))
}

/// Read a hull file (`write`), its blocks on `threads` threads.
pub fn read(path: &Path, threads: usize) -> Result<Frontier> {
    let b = std::fs::read(path).with_context(|| path.display().to_string())?;
    let (header, data) = open(path, &b)?;
    let parts = super::wave::par_map(&header.blocks, threads, |x| decode(path, &b[data..], x));
    let mut out = Frontier { frame: header.frame, ..Default::default() };
    for p in parts {
        let (h, g) = p?;
        out.hulls.extend(h);
        out.grown.extend(g);
    }
    ensure!(out.grown.windows(2).all(|w| (w[0].1, w[0].0) < (w[1].1, w[1].0)), "{}: its grown states are not sorted", path.display());
    Ok(out)
}

/// `rewrite ckhash --hulls`'s `h{frame}` line over a frontier: its states'
/// count and the sum of each one's term - its CONTENT hash
/// (`ContentHash::state` of (shape, key, cell)) mixed with its layer and
/// hull; `shapes`: the tree's by index. Any order.
pub fn fingerprint(states: impl Iterator<Item = Grown>, space: &celeste_engine::exact::KeySpace, shapes: &[u64]) -> Result<(u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let content = space.content_hash();
    let geo = super::GEOMETRY;
    let (mut n, mut acc) = (0u64, 0u64);
    for (s, layer, h) in states {
        let shape = *shapes.get(s.shape(&geo) as usize).ok_or_else(|| anyhow::anyhow!("a frontier state of no shape"))?;
        let node = content.state(shape, s.key, s.cell(&geo));
        let tag = mix64(u64::from_le_bytes(h.to_bytes()) ^ (layer as u64).rotate_left(48));
        acc = acc.wrapping_add(mix64(node ^ tag.rotate_left(17)));
        n += 1;
    }
    Ok((n, acc))
}

/// The hull file just written, READ BACK and compared with what was
/// written (`write`'s arguments), block by block on `threads` threads:
/// FATAL otherwise (a resume would expand another frontier than the
/// forward's).
pub fn check<R: Runs + ?Sized>(path: &Path, frame: u32, hulls: &[Hull], runs: &R, threads: usize) -> Result<()> {
    let b = std::fs::read(path).with_context(|| path.display().to_string())?;
    let (header, data) = open(path, &b)?;
    let cuts = cuts(hulls, runs);
    ensure!(header.frame == frame && header.blocks.len() == cuts.len(), "{}: STORED HULLS DIFFER from the forward's (frame {} in {} blocks read back, frame {frame} in {} blocks written)", path.display(), header.frame, header.blocks.len(), cuts.len());
    let same = super::wave::par_map(&header.blocks.iter().zip(&cuts).collect::<Vec<_>>(), threads, |(x, (g, i, r))| -> Result<bool> {
        let (h, gr) = decode(path, &b[data..], x)?;
        Ok(x.grown == *g && if *g { gr == runs.get(*i, r.clone()) } else { h == hulls[r.clone()] })
    });
    for ok in same {
        ensure!(ok?, "{}: STORED HULLS DIFFER from the forward's", path.display());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A hull file round-trips its frontier: wide keys, several regions
    /// and layers, a key with several cells, blocks of both parts past
    /// their sizes; unsorted grown states are refused.
    #[test]
    fn hull_files_round_trip() {
        let dir = std::env::temp_dir().join(format!("celeste-hulls-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let st = |region: u32, key: (u64, u64), local: u8| Sk { region, key, local };
        let h = |a: u16, b: u16| Hull { x: crate::search::hull::Arc { start: a, last: b }, y: crate::search::hull::Arc { start: b, last: a } };
        let mut grown = vec![(st(3, (0, 5), 0), 0, h(1, 2)), (st(3, (0, 5), 9), 0, h(65535, 0)), (st(3, (1 << 40, 1), 1), 0, h(3, 65535)), (st(70, (0, 0), 63), 2, h(0, 0)), (st(4096, (u64::MAX, u64::MAX), 5), 2, h(9, 9)), (st(5, (0, 1), 5), 400, h(9, 9))];
        let mut v = 1u64;
        let mut big: Vec<Grown> = (0..GROWN_BLOCK as u64 * 2 + 77)
            .map(|i| {
                v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                (st((v >> 50) as u32, (v >> 7, v), (v % 64) as u8), (i / 5000) as u16 + 401, h(v as u16, (v >> 16) as u16))
            })
            .collect();
        big.sort_unstable_by_key(|g| (g.1, g.0));
        big.dedup_by_key(|g| (g.1, g.0));
        grown.extend(big);
        let hulls: Vec<Hull> = (0..NEW_BLOCK as u32 + 5).map(|i| h(i as u16, (i * 7) as u16)).collect();
        let p = path(&dir, 12);
        // In runs, cut anywhere.
        let runs: Vec<&[Grown]> = vec![&grown[..2], &[], &grown[2..9], &grown[9..]];
        write(&p, 12, &hulls, &runs[..], 3).unwrap();
        check(&p, 12, &hulls, &runs[..], 2).unwrap();
        assert_eq!(read(&p, 1).unwrap(), Frontier { frame: 12, hulls: hulls.clone(), grown: grown.clone() });
        assert!(check(&p, 13, &hulls, &runs[..], 2).is_err(), "another frame");
        let mut moved = grown.clone();
        moved[5].2 = h(1, 1);
        assert!(check(&p, 12, &hulls, &[&moved[..2], &[], &moved[2..9], &moved[9..]][..], 2).is_err(), "another hull");
        assert!(write(&p, 12, &hulls, &[&[grown[1], grown[0]][..]][..], 2).is_err(), "unsorted");
        assert!(write(&p, 12, &hulls, &[&grown[1..2], &grown[0..1]][..], 2).is_err(), "runs unsorted");
        write(&p, 12, &[], &[] as &[&[Grown]], 2).unwrap();
        assert_eq!(read(&p, 2).unwrap(), Frontier { frame: 12, ..Default::default() });
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
