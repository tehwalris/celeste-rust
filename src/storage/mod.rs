//! STORAGE (plans/architecture.md, "The forward: hulls and storage" and "The
//! recorded graph"): the player-position cells (`cells`), the state set,
//! the wave, and the tree's files in one container (`file`): frame
//! (`layer`, with its frontier: `hulls`) and edge (`bundle`, `edges`)
//! files; and its one text record (`record`).
//!
//! A state is `(shape, key, cell)` (`Sk`; the key holds no position:
//! `exact::Code::of_pos`) everywhere: the edges, the frame files, the
//! backward. No state has a number.
#![warn(unreachable_pub)]

pub mod bench;
pub(crate) mod bundle;
pub mod cells;
pub(crate) mod codec;
pub(crate) mod decode;
pub mod edges;
pub(crate) mod file;
pub mod hulls;
pub mod layer;
pub(crate) mod record;
pub(crate) mod route;
pub(crate) mod set;
pub(crate) mod scratch;
pub mod stats;
pub(crate) mod unit;
pub(crate) mod visited;
pub mod wave;


/// THE TREE'S FINGERPRINT (`rewrite ckhash`), over CONTENT
/// (`exact::ContentHash`: any dictionary order): per frame its states'
/// count and an order-free hash (`fNNN n hash`); with `edges` its edges by
/// what they join and their transfers, not ids (`eNNN`); with `hulls` its
/// states' layers and hulls (`hNNN`). Equal lines: the same states, edges
/// and hulls.
pub fn fingerprint<N: crate::search::rem::Rem>(dir: &std::path::Path, to: u32, edges: bool, hulls: bool, threads: usize) -> anyhow::Result<Vec<String>> {
    let tree = layer::Tree::<N>::open(dir, to)?;
    let content = tree.space.content_hash();
    let mut out = Vec::new();
    for frame in 0..=to {
        // Past a dead frontier: empty layers.
        let (layer, _) = tree.layer(frame, None, threads)?;
        let (n, acc) = layer.content_fingerprint(&tree.space, &tree.shapes)?;
        out.push(format!("f{frame:03} {n} {acc:016x}"));
    }
    if edges {
        let eg = edges::EdgeStore::<N>::open(&dir, to)?;
        let hashes = bench::EdgeHashes::new::<N>(eg.pairs());
        for frame in 1..=to {
            let (n, acc) = hashes.frame(&eg, frame, &content, &tree.shapes, threads)?;
            out.push(format!("e{frame:03} {n} {acc:016x}"));
        }
    }
    if hulls {
        for frame in 0..=to {
            // Past a dead frontier: no files.
            let Some(file) = tree.file(frame) else { break };
            let (new, _) = tree.layer(frame, None, threads)?;
            let (n, acc) = hulls::fingerprint(file.frontier_states(&new, threads)?.into_iter(), &tree.space, &tree.shapes)?;
            out.push(format!("h{frame:03} {n} {acc:016x}"));
        }
    }
    Ok(out)
}

/// A region's index from its shape's index and its slot.
#[inline]
pub(crate) fn region_of(shape_idx: u32, slot: u32) -> u32 {
    let r = shape_idx as u64 * cells::SLOTS as u64 + slot as u64;
    assert!(r <= u32::MAX as u64, "region {r} past u32 ({shape_idx} shapes)");
    r as u32
}

/// A STATE by what it is: region (shape index and slot), exact key, cell's
/// index in the region - what the edge files name and the backward keys its
/// winning sets by. Sorts region-major, as the edge files are read.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Sk {
    pub(crate) region: u32,
    pub key: visited::Key,
    pub(crate) local: u8,
}

impl Sk {
    #[inline]
    pub(crate) fn at(shape: u32, key: visited::Key, cell: u32) -> Sk {
        let (slot, local) = cells::slot_local(cell);
        Sk { region: region_of(shape, slot), key, local: local as u8 }
    }

    /// Its shape's index.
    #[inline]
    pub fn shape(&self) -> u32 {
        self.region / cells::SLOTS
    }

    /// Its player-position cell (`cells::NO_CELL`: none).
    #[inline]
    pub fn cell(&self) -> u32 {
        cells::cell(self.region % cells::SLOTS, self.local as u32)
    }

    /// As text (`r{region}.{key hi}:{key lo}.c{local}`; `parse` reads it).
    pub fn show(&self) -> String {
        format!("r{}.{:x}:{:x}.c{}", self.region, self.key.0, self.key.1, self.local)
    }

    /// A state as `show` writes it.
    pub fn parse(s: &str) -> std::result::Result<Sk, String> {
        let bad = || format!("{s:?}: a state is r{{region}}.{{key hi}}:{{key lo}}.c{{cell}} (as the logs show it)");
        let (region, rest) = s.strip_prefix('r').and_then(|r| r.split_once('.')).ok_or_else(bad)?;
        let (key, local) = rest.split_once(".c").ok_or_else(bad)?;
        let (hi, lo) = key.split_once(':').ok_or_else(bad)?;
        let hex = |x: &str| u64::from_str_radix(x, 16).map_err(|_| bad());
        let local: u8 = local.parse().ok().filter(|&l| u32::from(l) < cells::SIDE * cells::SIDE).ok_or_else(bad)?;
        Ok(Sk { region: region.parse().map_err(|_| bad())?, key: (hex(hi)?, hex(lo)?), local })
    }
}

#[cfg(test)]
mod tests {
    /// A state reads back as the logs show it; other text is refused.
    #[test]
    fn a_state_reads_back_as_shown() {
        let s = super::Sk::at(7, (0xdead_beef, 3), super::cells::cell_of(40, 64).expect("a cell"));
        assert_eq!(super::Sk::parse(&s.show()), Ok(s));
        for bad in ["", "r1.2:3", "r1.2.c3", "x1.2:3.c4", "r1.2:3.c64", "r1.g:3.c4"] {
            assert!(super::Sk::parse(bad).is_err(), "{bad:?}");
        }
    }
}
