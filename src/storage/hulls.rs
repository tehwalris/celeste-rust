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
//! Format (`storage::file`): `CSH2`, the header (the frame, and per block
//! its part, place and states), then the blocks - the new states' hulls in runs of
//! `NEW_BLOCK` (8 B each), then the grown states in runs of `GROWN_BLOCK`,
//! each its layer, region, key (deltas from the state before it in the
//! block where they share the fields before) and cell, and its hull.
//! Blocks are written and read in parallel.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::bundle::{get_varint, get_varint128, put_varint, put_varint128};
use super::file::{self, At, Mapped, FORMAT_VERSION};
use super::Sk;
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;

const MAGIC: &[u8; 4] = b"CSH2";

/// New states' hulls a block.
const NEW_BLOCK: usize = 1 << 18;

/// Grown states a block.
const GROWN_BLOCK: usize = 1 << 16;

/// A frontier state: the state, its layer, its hull.
pub type Grown<N = P8> = (Sk, u16, <N as Rem>::Hull);

/// GROWN STATES in runs, sorted by layer and state across them (`write`,
/// `check`): a frontier's old states by layer, or states as they are.
pub trait Runs<N: Rem>: Sync {
    /// The runs.
    fn count(&self) -> usize;
    /// Run `i`'s states.
    fn len(&self, i: usize) -> usize;
    /// Run `i`'s states `r`, in order.
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown<N>>;
}

impl<N: Rem> Runs<N> for [&[Grown<N>]] {
    fn count(&self) -> usize {
        <[&[Grown<N>]]>::len(self)
    }
    fn len(&self, i: usize) -> usize {
        self[i].len()
    }
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown<N>> {
        self[i][r].to_vec()
    }
}

impl<N: Rem> Runs<N> for [super::wave::OldLayer<N>] {
    fn count(&self) -> usize {
        <[super::wave::OldLayer<N>]>::len(self)
    }
    fn len(&self, i: usize) -> usize {
        self[i].len()
    }
    fn get(&self, i: usize, r: std::ops::Range<usize>) -> Vec<Grown<N>> {
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
    at: At,
    states: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct Header {
    frame: u32,
    blocks: Vec<Block>,
}

/// A HULL FILE's content: frame `frame`'s new states' hulls (in the
/// frame's state order) and its grown states (sorted by layer and state).
#[derive(Debug, PartialEq, Eq)]
pub struct Frontier<N: Rem = P8> {
    pub frame: u32,
    pub hulls: Vec<N::Hull>,
    pub grown: Vec<Grown<N>>,
}

impl<N: Rem> Default for Frontier<N> {
    fn default() -> Self {
        Frontier { frame: 0, hulls: Vec::new(), grown: Vec::new() }
    }
}

impl<N: Rem> Frontier<N> {
    /// Every state of the frontier with its layer and hull, the new ones
    /// (frame `frame`'s layer `new`, its states in order) first.
    pub fn states(&self, new: &super::layer::Layer<N>) -> Result<Vec<Grown<N>>> {
        ensure!(new.states() == self.hulls.len() as u64, "frame {}: {} new states, its hull file holds {} hulls", self.frame, new.states(), self.hulls.len());
        let layer = u16::try_from(self.frame).context("a frame past u16")?;
        let mut out: Vec<Grown<N>> = new.states_iter().zip(&self.hulls).map(|(s, &h)| (s, layer, h)).collect();
        out.extend_from_slice(&self.grown);
        Ok(out)
    }
}

/// A grown block's bytes.
fn encode_grown<N: Rem>(states: impl Iterator<Item = Grown<N>>, n: usize) -> Vec<u8> {
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
        N::hull_to_bytes(&h, &mut raw);
        prev = Some((layer, s.region, k));
    }
    raw
}

/// A grown block decoded.
fn decode_grown<N: Rem>(raw: &[u8], want: usize) -> Result<Vec<Grown<N>>> {
    let mut pos = 0usize;
    let n = get_varint(raw, &mut pos) as usize;
    ensure!(n == want, "{n} grown states, the index says {want}");
    let mut out: Vec<Grown<N>> = Vec::with_capacity(n);
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
        ensure!(pos + 1 + N::HULL_BYTES <= raw.len(), "cut short");
        let local = raw[pos];
        let h = N::hull_from_bytes(&raw[pos + 1..pos + 1 + N::HULL_BYTES]);
        pos += 1 + N::HULL_BYTES;
        out.push((Sk { region, key: ((key >> 64) as u64, key as u64), local }, layer, h));
        prev = Some((layer, region, key));
    }
    ensure!(pos == raw.len(), "bytes past its states");
    Ok(out)
}

/// A hull file's blocks: per block whether it holds grown states, its run
/// and its range there (`write`'s runs; the new states' hulls are run 0 of
/// their own).
fn cuts<N: Rem, R: Runs<N> + ?Sized>(hulls: &[N::Hull], runs: &R) -> Vec<(bool, usize, std::ops::Range<usize>)> {
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
pub fn write<N: Rem, R: Runs<N> + ?Sized>(path: &Path, frame: u32, hulls: &[N::Hull], runs: &R, threads: usize) -> Result<u64> {
    let key = |g: &Grown<N>| (g.1, g.0);
    let cuts = cuts(hulls, runs);
    // Per block its bytes, and a grown block's sorted states' first and last
    // (`None`: unsorted).
    type Made = (Vec<u8>, usize, Option<Option<((u16, Sk), (u16, Sk))>>);
    let blocks: Vec<Made> = crate::par::Pool::new(threads).map_with(cuts.len(), || Ok(zstd::bulk::Compressor::new(1)?), |z, c| {
        let (g, i, r) = &cuts[c];
        let (raw, ends) = if *g {
            let v = runs.get(*i, r.clone());
            let ends = v.windows(2).all(|w| key(&w[0]) < key(&w[1])).then(|| v.first().map(|f| (key(f), key(v.last().expect("a first")))));
            (encode_grown::<N>(v.into_iter(), r.len()), ends)
        } else {
            let mut raw = Vec::with_capacity(r.len() * N::HULL_BYTES);
            for h in &hulls[r.clone()] {
                N::hull_to_bytes(h, &mut raw);
            }
            (raw, Some(None))
        };
        Ok((file::compress(&raw, z)?, raw.len(), ends))
    })?;
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
    let ats = At::place(blocks.iter().map(|b| (b.0.len(), b.1)));
    let header = Header { frame, blocks: cuts.iter().zip(ats).map(|((g, _, r), at)| Block { grown: *g, at, states: r.len() as u32 }).collect() };
    file::write(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &header, |f| {
        for (z, _, _) in &blocks {
            f.write_all(z)?;
        }
        Ok(())
    })
}

/// A block of a hull file decoded: its hulls (a new states' block) or its
/// grown states.
fn decode<N: Rem>(file: &Mapped, x: &Block) -> Result<(Vec<N::Hull>, Vec<Grown<N>>)> {
    let path = file.path();
    let mut raw = Vec::new();
    file.block(x.at, &mut zstd::bulk::Decompressor::new()?, &mut raw)?;
    if x.grown {
        Ok((Vec::new(), decode_grown::<N>(&raw, x.states as usize).with_context(|| format!("{}: a grown block", path.display()))?))
    } else {
        ensure!(raw.len() == x.states as usize * N::HULL_BYTES, "{}: a hulls block of {} bytes for {} states", path.display(), raw.len(), x.states);
        Ok((raw.chunks_exact(N::HULL_BYTES).map(N::hull_from_bytes).collect(), Vec::new()))
    }
}

/// A hull file mapped, its header checked.
fn open<N: Rem>(path: &Path) -> Result<(Mapped, Header)> {
    let file = Mapped::open(path, file::magic::<N>(MAGIC), FORMAT_VERSION, &format!("a hull file of the {} arithmetic", <N::A as celeste_core::arith::Arithmetic>::NAME))?;
    let header: Header = file.header()?;
    file.ends_at(header.blocks.last().map_or(0, |x| x.at.end()))?;
    ensure!(header.blocks.windows(2).all(|w| w[0].grown <= w[1].grown), "{}: a new states' block after a grown one", path.display());
    Ok((file, header))
}

/// Read a hull file (`write`), its blocks on `threads` threads.
pub fn read<N: Rem>(path: &Path, threads: usize) -> Result<Frontier<N>> {
    let (file, header) = open::<N>(path)?;
    let parts = super::wave::par_map(&header.blocks, threads, |x| decode::<N>(&file, x));
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
pub fn fingerprint<N: Rem>(states: impl Iterator<Item = Grown<N>>, space: &celeste_engine::exact::KeySpace<N>, shapes: &[u64]) -> Result<(u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let content = space.content_hash();
    let geo = super::GEOMETRY;
    let (mut n, mut acc) = (0u64, 0u64);
    for (s, layer, h) in states {
        let shape = *shapes.get(s.shape(&geo) as usize).ok_or_else(|| anyhow::anyhow!("a frontier state of no shape"))?;
        let node = content.state(shape, s.key, s.cell(&geo));
        let tag = mix64(N::hull_hash(&h) ^ (layer as u64).rotate_left(48));
        acc = acc.wrapping_add(mix64(node ^ tag.rotate_left(17)));
        n += 1;
    }
    Ok((n, acc))
}

/// The hull file just written, READ BACK and compared with what was
/// written (`write`'s arguments), block by block on `threads` threads:
/// FATAL otherwise (a resume would expand another frontier than the
/// forward's).
pub fn check<N: Rem, R: Runs<N> + ?Sized>(path: &Path, frame: u32, hulls: &[N::Hull], runs: &R, threads: usize) -> Result<()> {
    let (file, header) = open::<N>(path)?;
    let cuts = cuts::<N, R>(hulls, runs);
    ensure!(header.frame == frame && header.blocks.len() == cuts.len(), "{}: STORED HULLS DIFFER from the forward's (frame {} in {} blocks read back, frame {frame} in {} blocks written)", path.display(), header.frame, header.blocks.len(), cuts.len());
    let same = super::wave::par_map(&header.blocks.iter().zip(&cuts).collect::<Vec<_>>(), threads, |(x, (g, i, r))| -> Result<bool> {
        let (h, gr) = decode::<N>(&file, x)?;
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
    use crate::search::hull::Hull;

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
        write::<P8, _>(&p, 12, &hulls, &runs[..], 3).unwrap();
        check::<P8, _>(&p, 12, &hulls, &runs[..], 2).unwrap();
        assert_eq!(read::<P8>(&p, 1).unwrap(), Frontier { frame: 12, hulls: hulls.clone(), grown: grown.clone() });
        assert!(check::<P8, _>(&p, 13, &hulls, &runs[..], 2).is_err(), "another frame");
        let mut moved = grown.clone();
        moved[5].2 = h(1, 1);
        assert!(check::<P8, _>(&p, 12, &hulls, &[&moved[..2], &[], &moved[2..9], &moved[9..]][..], 2).is_err(), "another hull");
        assert!(write::<P8, _>(&p, 12, &hulls, &[&[grown[1], grown[0]][..]][..], 2).is_err(), "unsorted");
        assert!(write::<P8, _>(&p, 12, &hulls, &[&grown[1..2], &grown[0..1]][..], 2).is_err(), "runs unsorted");
        write::<P8, _>(&p, 12, &[], &[] as &[&[Grown]], 2).unwrap();
        assert_eq!(read::<P8>(&p, 2).unwrap(), Frontier { frame: 12, ..Default::default() });
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
