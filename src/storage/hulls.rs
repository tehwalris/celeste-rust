//! THE HULL FILES (`hulls/f{F}.bin`): frame F's FRONTIER as the hull
//! forward left it - every state whose remainder hull GREW at F (those first
//! reached at F, and older ones the frame pushed further) with its layer
//! and its hull. A resume replays them in order (`frame::restore_visited`):
//! a state's hull is its last file's, and frame F's file is the frontier
//! the next wave expands. A file is written once (renamed into place) and
//! read back before the frame is trusted (`check`).
//!
//! Format: `CSH1`, the checkpoint format's version, the raw length, then
//! zstd of: the count, and per state (sorted by `Sk`) its region (delta),
//! key (a delta within a region), cell, layer (varints) and hull (8 B).

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

/// Write `states` (sorted by state, distinct: checked). Atomic. Returns
/// the file's bytes.
pub fn write(path: &Path, states: &[Grown]) -> Result<u64> {
    use std::io::Write;
    ensure!(states.windows(2).all(|w| w[0].0 < w[1].0), "{}: the frontier's states not sorted and distinct", path.display());
    let mut raw = Vec::with_capacity(states.len() * 14 + 8);
    put_varint(&mut raw, states.len() as u64);
    let mut prev: Option<Sk> = None;
    for &(s, layer, h) in states {
        match prev {
            Some(p) if p.region == s.region => {
                put_varint(&mut raw, 0);
                put_varint128(&mut raw, wide(s.key) - wide(p.key));
            }
            _ => {
                put_varint(&mut raw, s.region as u64 - prev.map_or(0, |p| p.region as u64) + 1);
                put_varint128(&mut raw, wide(s.key));
            }
        }
        raw.push(s.local);
        put_varint(&mut raw, layer as u64);
        raw.extend_from_slice(&h.to_bytes());
        prev = Some(s);
    }
    let z = zstd::bulk::compress(&raw, 1)?;
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let tmp = path.with_extension("tmp");
    {
        let mut f = std::io::BufWriter::new(std::fs::File::create(&tmp).with_context(|| tmp.display().to_string())?);
        f.write_all(MAGIC)?;
        f.write_all(&crate::search::checkpoint::FORMAT_VERSION.to_le_bytes())?;
        f.write_all(&(raw.len() as u64).to_le_bytes())?;
        f.write_all(&z)?;
        f.flush()?;
    }
    std::fs::rename(&tmp, path)?;
    Ok(16 + z.len() as u64)
}

/// Read a hull file (`write`).
pub fn read(path: &Path) -> Result<Vec<Grown>> {
    let b = std::fs::read(path).with_context(|| path.display().to_string())?;
    ensure!(b.len() >= 16 && &b[0..4] == MAGIC, "{}: not a hull file", path.display());
    let version = u32::from_le_bytes(b[4..8].try_into().unwrap());
    ensure!(version == crate::search::checkpoint::FORMAT_VERSION, "{}: format version {version}, this binary's is {}", path.display(), crate::search::checkpoint::FORMAT_VERSION);
    let n_raw = u64::from_le_bytes(b[8..16].try_into().unwrap()) as usize;
    let raw = zstd::bulk::decompress(&b[16..], n_raw).with_context(|| path.display().to_string())?;
    ensure!(raw.len() == n_raw, "{}: {} raw bytes, its header says {n_raw}", path.display(), raw.len());
    let mut pos = 0usize;
    let n = get_varint(&raw, &mut pos) as usize;
    ensure!(n <= raw.len(), "{}: {n} states in {} bytes", path.display(), raw.len());
    let mut out = Vec::with_capacity(n);
    let mut prev: Option<Sk> = None;
    for _ in 0..n {
        let r = get_varint(&raw, &mut pos);
        let (region, key) = match (r, prev) {
            (0, Some(p)) => (p.region, wide(p.key) + get_varint128(&raw, &mut pos)),
            (0, None) => anyhow::bail!("{}: the first state continues no region", path.display()),
            (d, p) => (p.map_or(0, |p| p.region) + d as u32 - 1, get_varint128(&raw, &mut pos)),
        };
        ensure!(pos + 1 <= raw.len(), "{}: cut short", path.display());
        let local = raw[pos];
        pos += 1;
        let layer = get_varint(&raw, &mut pos);
        ensure!(layer <= u16::MAX as u64 && pos + HULL_BYTES <= raw.len(), "{}: cut short", path.display());
        let h = Hull::from_bytes(&raw[pos..pos + HULL_BYTES]);
        pos += HULL_BYTES;
        let s = Sk { region, key: ((key >> 64) as u64, key as u64), local };
        ensure!(prev.is_none_or(|p| p < s), "{}: its states are not sorted", path.display());
        out.push((s, layer as u16, h));
        prev = Some(s);
    }
    ensure!(pos == raw.len(), "{}: bytes past its states", path.display());
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
pub fn check(path: &Path, states: &[Grown]) -> Result<()> {
    let back = read(path)?;
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
        write(&p, &states).unwrap();
        check(&p, &states).unwrap();
        assert_eq!(read(&p).unwrap(), states);
        assert!(write(&p, &[states[1], states[0]]).is_err(), "unsorted");
        write(&p, &[]).unwrap();
        assert!(read(&p).unwrap().is_empty());
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
