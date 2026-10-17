//! STORAGE (plans/architecture.md, "The forward: hulls and storage" and "The
//! recorded graph"): the state set (`set`, `visited`, `layer`), the wave
//! (`unit`, `route`, `wave`, `spool`), the tree's files in one container
//! (`file`: the frame files `layer`, the hull files `hulls`, the edge files
//! `bundle` and `edges`, the out-edge files `out`), rows from keys
//! (`decode`), and `rewrite bench frame` (`bench`).
//!
//! A state is `(shape, key, cell)` (`Sk`; the key holds no position:
//! `exact::Code::pos`) everywhere: the edges, the frame and hull files, the
//! backward. No state has a number.

pub mod bench;
pub mod bundle;
pub mod decode;
pub mod edges;
pub mod file;
pub mod hulls;
pub mod layer;
pub mod out;
pub mod route;
pub mod set;
pub mod spool;
pub mod unit;
pub mod visited;
pub mod wave;

use crate::search::pos_graph::{GRID, NO_CELL};

/// THE TREE'S FINGERPRINT (`rewrite ckhash`), over CONTENT
/// (`exact::ContentHash`: whatever order its dictionaries grew in): per
/// frame its states' count and an order-free hash (`fNNN n hash`); with
/// `edges` each frame's edges by what they join and their transfers, not
/// their ids (`eNNN`); with `hulls` each frame's states with their layers
/// and hulls (`hNNN`). Two trees with the same lines hold the same states,
/// edges and hulls.
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
        let eg = edges::EdgeStore::<N>::open(&dir.join("edges"), to)?;
        // Streamed: one frame at a time, its blocks on every thread.
        let hashes = bench::EdgeHashes::new::<N>(eg.pairs());
        for frame in 1..=to {
            let (n, acc) = hashes.frame(&eg, frame, &content, &tree.shapes, threads)?;
            out.push(format!("e{frame:03} {n} {acc:016x}"));
        }
    }
    if hulls {
        for frame in 0..=to {
            // Past a dead frontier: no files.
            if !layer::frame_exists(dir, frame) {
                break;
            }
            let hf = hulls::read::<N>(&hulls::path(dir, frame), threads)?;
            let (new, _) = tree.layer(frame, None, threads)?;
            let (n, acc) = hulls::fingerprint(hf.states(&new)?.into_iter(), &tree.space, &tree.shapes)?;
            out.push(format!("h{frame:03} {n} {acc:016x}"));
        }
    }
    Ok(out)
}

/// The storage regions: `side` x `side` cells (`GEOMETRY`: 8; the tests
/// also check 16), aligned at 0 as the kernels' `RegionGrid::of`
/// (`div_euclid`; the grid's origin -64 is a multiple of both), each nested
/// in one kernel region. A region's SLOT is its place on the grid,
/// row-major, `NO_CELL` the last (one cell).
#[derive(Clone, Copy, Debug)]
pub struct Geometry {
    pub side: u32,
    /// Regions per grid row.
    pub per_row: u32,
    /// Slots per shape: the grid's regions and `NO_CELL`'s.
    pub slots: u32,
    /// Mask words per entry: `side * side / 64`.
    pub words: usize,
}

impl Geometry {
    pub const fn new(side: u32) -> Geometry {
        assert!(side == 8 || side == 16, "a storage region is 8 or 16 cells wide");
        let per_row = GRID as u32 / side;
        Geometry { side, per_row, slots: per_row * per_row + 1, words: (side * side / 64) as usize }
    }

    #[inline]
    pub fn no_cell_slot(&self) -> u32 {
        self.slots - 1
    }

    /// A cell's region slot and its index in the region.
    #[inline]
    pub fn slot_local(&self, cell: u32) -> (u32, u32) {
        if cell == NO_CELL {
            return (self.no_cell_slot(), 0);
        }
        let (gx, gy) = (cell % GRID as u32, cell / GRID as u32);
        ((gy / self.side) * self.per_row + gx / self.side, (gy % self.side) * self.side + gx % self.side)
    }

    /// `slot_local` inverted.
    #[inline]
    pub fn cell(&self, slot: u32, local: u32) -> u32 {
        if slot == self.no_cell_slot() {
            debug_assert_eq!(local, 0);
            return NO_CELL;
        }
        let (rx, ry) = (slot % self.per_row, slot / self.per_row);
        let (gx, gy) = (rx * self.side + local % self.side, ry * self.side + local / self.side);
        gy * GRID as u32 + gx
    }
}

/// THE storage geometry: 8-cell regions. The kernels' regions (`--region`,
/// a multiple of 8 px) each hold whole storage regions.
pub const GEOMETRY: Geometry = Geometry::new(8);

/// A region's index from its shape's index and its slot.
#[inline]
pub fn region_of(geo: &Geometry, shape_idx: u32, slot: u32) -> u32 {
    let r = shape_idx as u64 * geo.slots as u64 + slot as u64;
    assert!(r <= u32::MAX as u64, "region {r} past u32 ({shape_idx} shapes)");
    r as u32
}

/// A STATE by what it is: its region (shape index and slot), its exact key
/// and its cell's index in the region - what the edge files name
/// (`bundle`) and the backward keys its winning sets by. Sorts by region,
/// key, cell: region-major, as the edge files are read.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Sk {
    pub region: u32,
    pub key: visited::Key,
    pub local: u8,
}

impl Sk {
    #[inline]
    pub fn at(geo: &Geometry, shape: u32, key: visited::Key, cell: u32) -> Sk {
        let (slot, local) = geo.slot_local(cell);
        Sk { region: region_of(geo, shape, slot), key, local: local as u8 }
    }

    #[inline]
    pub fn shape(&self, geo: &Geometry) -> u32 {
        self.region / geo.slots
    }

    #[inline]
    pub fn cell(&self, geo: &Geometry) -> u32 {
        geo.cell(self.region % geo.slots, self.local as u32)
    }

    /// As text (`r{region}.{key hi}:{key lo}.c{local}`).
    pub fn show(&self) -> String {
        format!("r{}.{:x}:{:x}.c{}", self.region, self.key.0, self.key.1, self.local)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Slots and cells round-trip at both sides; regions nest in 16 px
    /// kernel regions.
    #[test]
    fn cells_round_trip_through_regions() {
        for side in [8, 16] {
            let g = Geometry::new(side);
            for (x, y) in [(-64, -64), (-1, 0), (0, -1), (5, 7), (127, 127), (200, 40), (447, 447)] {
                let cell = crate::search::pos_graph::cell_of(x, y).unwrap();
                let (slot, local) = g.slot_local(cell);
                assert!(slot < g.no_cell_slot() && local < side * side);
                assert_eq!(g.cell(slot, local), cell);
                let (rx, ry) = (slot % g.per_row, slot / g.per_row);
                assert_eq!((rx as i32 * side as i32 - 64, ry as i32 * side as i32 - 64), (x.div_euclid(side as i32) * side as i32, y.div_euclid(side as i32) * side as i32));
                let k = |v: i32| v.div_euclid(16);
                let (x0, y0) = (rx as i32 * side as i32 - 64, ry as i32 * side as i32 - 64);
                assert_eq!((k(x0), k(y0)), (k(x0 + side as i32 - 1), k(y0 + side as i32 - 1)));
            }
            assert_eq!(g.slot_local(NO_CELL), (g.no_cell_slot(), 0));
            assert_eq!(g.cell(g.no_cell_slot(), 0), NO_CELL);
        }
    }
}
