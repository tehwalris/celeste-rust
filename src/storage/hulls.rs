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
//! the grown states in runs of `GROWN_BLOCK`, each its layer (a delta) and
//! state (`codec::put_sk` after the previous state of its layer), and its
//! hull.

use crate::search::hull::HullOf;
use anyhow::{ensure, Context, Result};

use super::codec::{get_sk, get_varint, put_sk, put_varint};
use super::file::{self, At, Mapped};
use super::wave::Frontier;
use super::Sk;
use crate::search::rem::Rem;

/// New states' hulls a block.
pub(crate) const NEW_BLOCK: usize = 1 << 18;

/// Grown states a block.
pub(crate) const GROWN_BLOCK: usize = 1 << 16;

/// A frontier state: the state, its layer, its hull.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Grown<N: Rem> {
    pub(crate) sk: Sk,
    pub(crate) layer: u16,
    pub(crate) hull: HullOf<N>,
}

impl<N: Rem> Grown<N> {
    /// The frontier's order: by layer, then state.
    fn order(&self) -> (u16, Sk) {
        (self.layer, self.sk)
    }
}

/// A frontier's states, layers and hulls: its new states (`new`, made at
/// `frame`; `hulls` in its state order), then its grown ones.
pub(crate) fn frontier_states<'a, N: Rem>(new: &'a super::layer::Layer<N>, hulls: &'a [HullOf<N>], frame: u32, grown: impl Iterator<Item = Grown<N>> + 'a) -> impl Iterator<Item = Grown<N>> + 'a {
    let layer = u16::try_from(frame).expect("a frame past u16");
    new.states_iter().zip(hulls).map(move |(sk, &hull)| Grown { sk, layer, hull }).chain(grown)
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
    let mut prev: Option<(u16, Sk)> = None;
    for g in states {
        match prev {
            Some((pl, ps)) if pl == g.layer => {
                put_varint(&mut raw, 0);
                put_sk(&mut raw, ps, g.sk);
            }
            _ => {
                put_varint(&mut raw, g.layer as u64 - prev.map_or(0, |p| p.0 as u64) + prev.is_none() as u64);
                put_sk(&mut raw, Sk::default(), g.sk);
            }
        }
        g.hull.to_bytes(&mut raw);
        prev = Some((g.layer, g.sk));
    }
    raw
}

fn decode_grown<N: Rem>(raw: &[u8], want: usize) -> Result<Vec<Grown<N>>> {
    let mut pos = 0usize;
    let n = get_varint(raw, &mut pos) as usize;
    ensure!(n == want, "{n} grown states, the index says {want}");
    let mut out: Vec<Grown<N>> = Vec::with_capacity(n);
    let mut prev: Option<(u16, Sk)> = None;
    for _ in 0..n {
        ensure!(pos < raw.len(), "cut short");
        let dl = get_varint(raw, &mut pos);
        let (layer, sk) = match prev {
            Some((pl, ps)) if dl == 0 => (pl, get_sk(raw, &mut pos, ps)),
            _ => {
                let layer = prev.map_or(0, |p| p.0 as u64) + dl - prev.is_none() as u64;
                ensure!(layer <= u16::MAX as u64, "a layer past u16");
                (layer as u16, get_sk(raw, &mut pos, Sk::default()))
            }
        };
        ensure!(pos + HullOf::<N>::BYTES <= raw.len(), "cut short");
        let hull = HullOf::<N>::from_bytes(&raw[pos..pos + HullOf::<N>::BYTES]);
        pos += HullOf::<N>::BYTES;
        out.push(Grown { sk, layer, hull });
        prev = Some((layer, sk));
    }
    ensure!(pos == raw.len(), "bytes past its states");
    Ok(out)
}

/// The frontier's blocks: per block whether it holds grown states, its old
/// layer (the new hulls: 0) and its range there.
fn cuts<N: Rem>(front: &Frontier<N>) -> Vec<(bool, usize, std::ops::Range<usize>)> {
    let n = front.hulls.len();
    let mut cuts: Vec<(bool, usize, std::ops::Range<usize>)> = (0..n).step_by(NEW_BLOCK).map(|a| (false, 0, a..(a + NEW_BLOCK).min(n))).collect();
    for (i, old) in front.old.iter().enumerate() {
        let n = old.len();
        cuts.extend((0..n).step_by(GROWN_BLOCK).map(|a| (true, i, a..(a + GROWN_BLOCK).min(n))));
    }
    cuts
}

/// Old layer `i`'s states `r` as frontier states.
fn grown_of<N: Rem>(front: &Frontier<N>, i: usize, r: std::ops::Range<usize>) -> Vec<Grown<N>> {
    front.old[i].range(r).map(Grown::from).collect()
}

/// `front`'s blocks ENCODED: its new states' hulls (in the frame's state
/// order) and its old states by layer (all of them sorted by layer and
/// state, distinct: checked); per block its kind and states, its zstd bytes
/// and raw length (`layer::write_frame` places them).
pub(crate) fn encode<N: Rem>(front: &Frontier<N>, threads: usize) -> Result<Vec<(Block, Vec<u8>, usize)>> {
    let cuts = cuts(front);
    // Per block its bytes, and a grown block's sorted states' first and last
    // (`None`: unsorted).
    type Made = (Vec<u8>, usize, Option<Option<((u16, Sk), (u16, Sk))>>);
    let blocks: Vec<Made> = crate::par::Pool::new(threads).map_with(cuts.len(), || Ok(super::file::compressor()?), |z, c| {
        let (g, i, r) = &cuts[c];
        let (raw, ends) = if *g {
            let v = grown_of(front, *i, r.clone());
            let ends = v.windows(2).all(|w| w[0].order() < w[1].order()).then(|| v.first().map(|f| (f.order(), v.last().expect("a first").order())));
            (encode_grown::<N>(v.into_iter(), r.len()), ends)
        } else {
            let mut raw = Vec::with_capacity(r.len() * HullOf::<N>::BYTES);
            for h in &front.hulls[r.clone()] {
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
    ensure!(grown.windows(2).all(|w: &[Grown<N>]| w[0].order() < w[1].order()), "{}: its grown states are not sorted", file.path().display());
    Ok((hulls, grown))
}

/// `rewrite ckhash --hulls`'s `h{frame}` line: the states' count and the
/// sum of their CONTENT hashes mixed with layer and hull; any order.
pub fn fingerprint<N: Rem>(states: impl Iterator<Item = Grown<N>>, space: &celeste_engine::exact::KeySpace<N>, shapes: &[u64]) -> Result<(u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let content = space.content_hash();
    let (mut n, mut acc) = (0u64, 0u64);
    for Grown { sk: s, layer, hull: h } in states {
        let shape = *shapes.get(s.shape() as usize).ok_or_else(|| anyhow::anyhow!("a frontier state of no shape"))?;
        let node = content.state(shape, s.key, s.cell());
        let tag = mix64(h.hash() ^ (layer as u64).rotate_left(48));
        acc = acc.wrapping_add(mix64(node ^ tag.rotate_left(17)));
        n += 1;
    }
    Ok((n, acc))
}

/// A frame file's frontier just written, READ BACK and compared with
/// `front`, what was written (`encode`): FATAL otherwise (a resume would
/// expand another frontier than the forward's).
pub(crate) fn check<N: Rem>(file: &Mapped, blocks: &[Block], front: &Frontier<N>, threads: usize) -> Result<()> {
    let path = file.path();
    let cuts = cuts(front);
    ensure!(blocks.len() == cuts.len(), "{}: STORED HULLS DIFFER from the forward's ({} blocks read back, {} written)", path.display(), blocks.len(), cuts.len());
    let same = super::wave::par_map(&blocks.iter().zip(&cuts).collect::<Vec<_>>(), threads, |(x, (g, i, r))| -> Result<bool> {
        let (h, gr) = decode::<N>(file, x)?;
        Ok(x.grown == *g && if *g { gr == grown_of(front, *i, r.clone()) } else { h == front.hulls[r.clone()] })
    });
    for ok in same {
        ensure!(ok?, "{}: STORED HULLS DIFFER from the forward's", path.display());
    }
    Ok(())
}
