//! A FRAME FILE's FRONTIER blocks (`layer::write_frame`): frame F's
//! frontier - every state whose hull GREW at F: the NEW ones (the file's
//! states, in its order) by their hulls alone, the older ones (GROWN,
//! sorted by layer and state) with their layers and hulls. A resume replays
//! them in order (`frame::restore_visited`): a state's hull is its last
//! frame's; the last frame's is the frontier the next wave expands. Read
//! back before the frame is trusted (`check`).
//!
//! The blocks (each `Block` in the frame file's header): the new states'
//! hulls in runs of `NEW_BLOCK` (`HullOf::BYTES` each: 8 in 16.16), then
//! the grown states in runs of `GROWN_BLOCK`, each its layer, region, key
//! (deltas from the previous state where they share the fields before),
//! cell and hull.

use crate::search::hull::HullOf;
use anyhow::{ensure, Context, Result};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::file::{self, At, Mapped};
use super::Sk;
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;

/// New states' hulls a block.
pub(crate) const NEW_BLOCK: usize = 1 << 18;

/// Grown states a block.
pub(crate) const GROWN_BLOCK: usize = 1 << 16;

/// A frontier state: the state, its layer, its hull.
pub(crate) type Grown<N = P8> = (Sk, u16, HullOf<N>);

/// GROWN STATES in runs, sorted by layer and state across them (`write`,
/// `check`): a frontier's old states by layer, or states as they are.
pub(crate) trait Runs<N: Rem>: Sync {
    fn count(&self) -> usize;
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

/// One frontier block of a frame file: of the new states (`grown` false)
/// or grown ones.
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug)]
pub(crate) struct Block {
    pub(crate) grown: bool,
    pub(crate) at: At,
    pub(crate) states: u32,
}

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
        h.to_bytes(&mut raw);
        prev = Some((layer, s.region, k));
    }
    raw
}

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
        ensure!(pos + 1 + HullOf::<N>::BYTES <= raw.len(), "cut short");
        let local = raw[pos];
        let h = HullOf::<N>::from_bytes(&raw[pos + 1..pos + 1 + HullOf::<N>::BYTES]);
        pos += 1 + HullOf::<N>::BYTES;
        out.push((Sk { region, key: narrow(key), local }, layer, h));
        prev = Some((layer, region, key));
    }
    ensure!(pos == raw.len(), "bytes past its states");
    Ok(out)
}

/// The frontier's blocks: per block whether it holds grown states, its run
/// (the new hulls: run 0 of their own) and its range there.
fn cuts<N: Rem, R: Runs<N> + ?Sized>(hulls: &[HullOf<N>], runs: &R) -> Vec<(bool, usize, std::ops::Range<usize>)> {
    let mut cuts: Vec<(bool, usize, std::ops::Range<usize>)> = (0..hulls.len()).step_by(NEW_BLOCK).map(|a| (false, 0, a..(a + NEW_BLOCK).min(hulls.len()))).collect();
    for i in 0..runs.count() {
        let n = runs.len(i);
        cuts.extend((0..n).step_by(GROWN_BLOCK).map(|a| (true, i, a..(a + GROWN_BLOCK).min(n))));
    }
    cuts
}

/// The frontier's blocks ENCODED: its new states' `hulls` (in the frame's
/// state order) and its grown states in `runs` (all of them sorted by layer
/// and state, distinct: checked); per block its kind and states, its zstd
/// bytes and raw length (`layer::write_frame` places them).
pub(crate) fn encode<N: Rem, R: Runs<N> + ?Sized>(hulls: &[HullOf<N>], runs: &R, threads: usize) -> Result<Vec<(Block, Vec<u8>, usize)>> {
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
            let mut raw = Vec::with_capacity(r.len() * HullOf::<N>::BYTES);
            for h in &hulls[r.clone()] {
                h.to_bytes(&mut raw);
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
    ensure!(sorted, "the grown states not sorted by layer and state, and distinct");
    Ok(cuts.iter().zip(blocks).map(|((g, _, r), (z, raw, _))| (Block { grown: *g, at: At::default(), states: r.len() as u32 }, z, raw)).collect())
}

/// A frontier block decoded: its hulls (new states) or its grown states.
fn decode<N: Rem>(file: &Mapped, x: &Block) -> Result<(Vec<HullOf<N>>, Vec<Grown<N>>)> {
    let path = file.path();
    let mut raw = Vec::new();
    file.block(x.at, &mut zstd::bulk::Decompressor::new()?, &mut raw)?;
    if x.grown {
        Ok((Vec::new(), decode_grown::<N>(&raw, x.states as usize).with_context(|| format!("{}: a grown block", path.display()))?))
    } else {
        ensure!(raw.len() == x.states as usize * HullOf::<N>::BYTES, "{}: a hulls block of {} bytes for {} states", path.display(), raw.len(), x.states);
        Ok((raw.chunks_exact(HullOf::<N>::BYTES).map(HullOf::<N>::from_bytes).collect(), Vec::new()))
    }
}

/// A frame file's frontier read: its new states' hulls and its grown
/// states (sorted: checked).
pub(crate) fn read<N: Rem>(file: &Mapped, blocks: &[Block], threads: usize) -> Result<(Vec<HullOf<N>>, Vec<Grown<N>>)> {
    ensure!(blocks.windows(2).all(|w| w[0].grown <= w[1].grown), "{}: a new states' block after a grown one", file.path().display());
    let parts = super::wave::par_map(blocks, threads, |x| decode::<N>(file, x));
    let (mut hulls, mut grown) = (Vec::new(), Vec::new());
    for p in parts {
        let (h, g) = p?;
        hulls.extend(h);
        grown.extend(g);
    }
    ensure!(grown.windows(2).all(|w: &[Grown<N>]| (w[0].1, w[0].0) < (w[1].1, w[1].0)), "{}: its grown states are not sorted", file.path().display());
    Ok((hulls, grown))
}

/// `rewrite ckhash --hulls`'s `h{frame}` line: the states' count and the
/// sum of their CONTENT hashes mixed with layer and hull; any order.
pub fn fingerprint<N: Rem>(states: impl Iterator<Item = Grown<N>>, space: &celeste_engine::exact::KeySpace<N>, shapes: &[u64]) -> Result<(u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let content = space.content_hash();
    let geo = super::GEOMETRY;
    let (mut n, mut acc) = (0u64, 0u64);
    for (s, layer, h) in states {
        let shape = *shapes.get(s.shape(&geo) as usize).ok_or_else(|| anyhow::anyhow!("a frontier state of no shape"))?;
        let node = content.state(shape, s.key, s.cell(&geo));
        let tag = mix64(h.hash() ^ (layer as u64).rotate_left(48));
        acc = acc.wrapping_add(mix64(node ^ tag.rotate_left(17)));
        n += 1;
    }
    Ok((n, acc))
}

/// A frame file's frontier just written, READ BACK and compared with
/// what was written (`encode`'s arguments): FATAL otherwise (a resume
/// would expand another frontier than the forward's).
pub(crate) fn check<N: Rem, R: Runs<N> + ?Sized>(file: &Mapped, blocks: &[Block], hulls: &[HullOf<N>], runs: &R, threads: usize) -> Result<()> {
    let path = file.path();
    let cuts = cuts::<N, R>(hulls, runs);
    ensure!(blocks.len() == cuts.len(), "{}: STORED HULLS DIFFER from the forward's ({} blocks read back, {} written)", path.display(), blocks.len(), cuts.len());
    let same = super::wave::par_map(&blocks.iter().zip(&cuts).collect::<Vec<_>>(), threads, |(x, (g, i, r))| -> Result<bool> {
        let (h, gr) = decode::<N>(file, x)?;
        Ok(x.grown == *g && if *g { gr == runs.get(*i, r.clone()) } else { h == hulls[r.clone()] })
    });
    for ok in same {
        ensure!(ok?, "{}: STORED HULLS DIFFER from the forward's", path.display());
    }
    Ok(())
}
