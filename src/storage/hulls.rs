//! THE HULL FILES (`hulls/f{F}.bin`): frame F's FRONTIER as the hull
//! forward left it - every state whose remainder hull GREW at F (those first
//! reached at F, and older ones the frame pushed further) with its layer
//! and its hull. A resume replays them in order (`frame::restore_visited`):
//! a state's hull is its last file's, and frame F's file is the frontier
//! the next wave expands. A file is written once (renamed into place) and
//! read back before the frame is trusted (`check`).
//!
//! Format: `CSH1`, the checkpoint format's version, the header's length and
//! the header (bincode: per region its block's offset, lengths and
//! states), then per region a zstd block: the count, and per state (sorted
//! by key and cell) its key (a delta), cell, layer (varint) and hull (8 B).
//! Regions are written and read in parallel.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

use super::bundle::{get_varint, get_varint128, put_varint, put_varint128};
use super::Sk;
use crate::search::hull::{Hull, HULL_BYTES};

const MAGIC: &[u8; 4] = b"CSH1";

/// One frontier state: the state, its layer, its hull.
pub type Grown = (Sk, u16, Hull);

/// Frame `frame`'s hull file.
pub fn path(dir: &Path, frame: u32) -> PathBuf {
    dir.join("hulls").join(format!("f{frame:03}.bin"))
}

fn wide(k: (u64, u64)) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

/// One region's block in a hull file.
#[derive(serde::Serialize, serde::Deserialize, Clone, Copy, Debug)]
struct Block {
    region: u32,
    offset: u64,
    len: u32,
    raw: u32,
    states: u32,
}

/// `states` (sorted by state) cut by region.
fn regions(states: &[Grown]) -> Vec<std::ops::Range<usize>> {
    let mut out = Vec::new();
    let mut a = 0;
    while a < states.len() {
        let r = states[a].0.region;
        let b = a + states[a..].partition_point(|x| x.0.region == r);
        out.push(a..b);
        a = b;
    }
    out
}

/// Write `states` (sorted by state, distinct: checked), its regions on
/// `threads` threads. Atomic. Returns the file's bytes.
pub fn write(path: &Path, states: &[Grown], threads: usize) -> Result<u64> {
    use std::io::Write;
    ensure!(states.windows(2).all(|w| w[0].0 < w[1].0), "{}: the frontier's states not sorted and distinct", path.display());
    let groups = regions(states);
    let blocks: Vec<Result<(Vec<u8>, u32)>> = super::wave::par_map(&groups, threads, |g| {
        let mut raw = Vec::with_capacity(g.len() * 14 + 8);
        put_varint(&mut raw, g.len() as u64);
        let mut prev = 0u128;
        for &(s, layer, h) in &states[g.clone()] {
            let k = wide(s.key);
            put_varint128(&mut raw, k - prev);
            prev = k;
            raw.push(s.local);
            put_varint(&mut raw, layer as u64);
            raw.extend_from_slice(&h.to_bytes());
        }
        Ok((zstd::bulk::compress(&raw, 1)?, raw.len() as u32))
    });
    let mut index = Vec::with_capacity(groups.len());
    let mut data = Vec::with_capacity(groups.len());
    let mut offset = 0u64;
    for (g, b) in groups.iter().zip(blocks) {
        let (z, raw) = b?;
        index.push(Block { region: states[g.start].0.region, offset, len: z.len() as u32, raw, states: g.len() as u32 });
        offset += z.len() as u64;
        data.push(z);
    }
    let hb = bincode::serialize(&index).context("a hull file's header")?;
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

/// A region's block decoded.
fn decode(path: &Path, b: &Block, z: &[u8]) -> Result<Vec<Grown>> {
    let raw = zstd::bulk::decompress(z, b.raw as usize).with_context(|| format!("{}: region {}", path.display(), b.region))?;
    ensure!(raw.len() == b.raw as usize, "{}: region {}: {} raw bytes, its index says {}", path.display(), b.region, raw.len(), b.raw);
    let mut pos = 0usize;
    let n = get_varint(&raw, &mut pos) as usize;
    ensure!(n == b.states as usize, "{}: region {}: {n} states, its index says {}", path.display(), b.region, b.states);
    let mut out = Vec::with_capacity(n);
    let mut prev = 0u128;
    for _ in 0..n {
        let k = prev + get_varint128(&raw, &mut pos);
        prev = k;
        ensure!(pos < raw.len(), "{}: region {}: cut short", path.display(), b.region);
        let local = raw[pos];
        pos += 1;
        let layer = get_varint(&raw, &mut pos);
        ensure!(layer <= u16::MAX as u64 && pos + HULL_BYTES <= raw.len(), "{}: region {}: cut short", path.display(), b.region);
        let h = Hull::from_bytes(&raw[pos..pos + HULL_BYTES]);
        pos += HULL_BYTES;
        out.push((Sk { region: b.region, key: ((k >> 64) as u64, k as u64), local }, layer as u16, h));
    }
    ensure!(pos == raw.len(), "{}: region {}: bytes past its states", path.display(), b.region);
    Ok(out)
}

/// Read a hull file (`write`), its regions on `threads` threads.
pub fn read(path: &Path, threads: usize) -> Result<Vec<Grown>> {
    let b = std::fs::read(path).with_context(|| path.display().to_string())?;
    ensure!(b.len() >= 16 && &b[0..4] == MAGIC, "{}: not a hull file", path.display());
    let version = u32::from_le_bytes(b[4..8].try_into().unwrap());
    ensure!(version == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {version}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
    let hl = u64::from_le_bytes(b[8..16].try_into().unwrap()) as usize;
    ensure!(b.len() >= 16 + hl, "{}: truncated header", path.display());
    let index: Vec<Block> = bincode::deserialize(&b[16..16 + hl]).with_context(|| format!("{}: its header", path.display()))?;
    let data = 16 + hl;
    let end = index.last().map_or(0, |x| x.offset + x.len as u64);
    ensure!(b.len() as u64 == data as u64 + end, "{}: {} bytes, its index says {}", path.display(), b.len(), data as u64 + end);
    ensure!(index.windows(2).all(|w| w[0].region < w[1].region), "{}: regions not ascending", path.display());
    let parts = super::wave::par_map(&index, threads, |x| decode(path, x, &b[data + x.offset as usize..data + (x.offset + x.len as u64) as usize]));
    let mut out = Vec::with_capacity(index.iter().map(|x| x.states as usize).sum());
    for p in parts {
        out.extend(p?);
    }
    ensure!(out.windows(2).all(|w| w[0].0 < w[1].0), "{}: its states are not sorted", path.display());
    Ok(out)
}

/// `rewrite ckhash --hulls`'s `h{frame}` line over a frontier: its states'
/// count and the sum of each one's term - its CONTENT hash
/// (`ContentHash::state` of (shape, key, cell)) mixed with its layer and
/// hull; `shapes`: the tree's by index.
pub fn fingerprint(states: &[Grown], space: &celeste_engine::exact::KeySpace, shapes: &[u64]) -> Result<(u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let content = space.content_hash();
    let geo = super::GEOMETRY;
    let mut acc = 0u64;
    for &(s, layer, h) in states {
        let shape = *shapes.get(s.shape(&geo) as usize).ok_or_else(|| anyhow::anyhow!("a frontier state of no shape"))?;
        let node = content.state(shape, s.key, s.cell(&geo));
        let tag = mix64(u64::from_le_bytes(h.to_bytes()) ^ (layer as u64).rotate_left(48));
        acc = acc.wrapping_add(mix64(node ^ tag.rotate_left(17)));
    }
    Ok((states.len() as u64, acc))
}

/// The hull file just written, READ BACK and compared with `states`: FATAL
/// otherwise (a resume would expand another frontier than the forward's).
pub fn check(path: &Path, states: &[Grown], threads: usize) -> Result<()> {
    let back = read(path, threads)?;
    ensure!(back.len() == states.len() && back.iter().zip(states).all(|(a, b)| a == b), "{}: STORED HULLS DIFFER from the forward's ({} states read back, {} in memory)", path.display(), back.len(), states.len());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A hull file round-trips its states: wide keys, several regions, a
    /// key with several cells.
    #[test]
    fn hull_files_round_trip() {
        let dir = std::env::temp_dir().join(format!("celeste-hulls-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let st = |region: u32, key: (u64, u64), local: u8| Sk { region, key, local };
        let h = |a: u16, b: u16| Hull { x: crate::search::hull::Arc { start: a, last: b }, y: crate::search::hull::Arc { start: b, last: a } };
        let states = vec![(st(3, (0, 5), 0), 2, h(1, 2)), (st(3, (0, 5), 9), 7, h(65535, 0)), (st(3, (1 << 40, 1), 1), 0, h(3, 65535)), (st(70, (0, 0), 63), 400, h(0, 0)), (st(4096, (u64::MAX, u64::MAX), 5), 1, h(9, 9))];
        let p = path(&dir, 12);
        write(&p, &states, 3).unwrap();
        check(&p, &states, 2).unwrap();
        assert_eq!(read(&p, 1).unwrap(), states);
        assert!(write(&p, &[states[1], states[0]], 2).is_err(), "unsorted");
        write(&p, &[], 2).unwrap();
        assert!(read(&p, 2).unwrap().is_empty());
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
