//! STORAGE (plans/storage-v2.md): the forward's visited set, its state ids,
//! and the recorded edges, in one system.
//!
//!   * `visited` - per (shape, region) a table of ENTRIES: a state's
//!                 non-position key and a mask over the region's cells
//!   * `unit`    - one unit of the wave: the kernels' emissions against the
//!                 visited set (read-only), its edges and its requests
//!   * `wave`    - the frame: the units, the TRANSLATION (requests into the
//!                 visited set, canonical entry numbers), the new layer
//!   * `edges`   - the edge files (source-side unit blocks with their
//!                 translation tables), the global transfer table, the reader
//!   * `marks`   - the backward's marks over state ids, and the resolver of
//!                 an id to its (shape, key, cell)
//!   * `meta`    - per frame, the shapes and entries it created
//!
//! A state is `(shape, key, cell)` (the key holds no position:
//! `runtime2::pos_code`); its id is `(region, entry, cell in region)`.

pub mod edges;
pub mod marks;
pub mod meta;
pub mod unit;
pub mod visited;
pub mod wave;

use crate::search::pos_graph::{GRID, NO_CELL};

/// The storage regions: `side` x `side` cells (`CELESTE_STORAGE_REGION`, 8
/// or 16), aligned at 0 as the kernels' `RegionGrid::of` (`div_euclid`; the
/// grid's origin -64 is a multiple of both), each nested in one kernel
/// region. A region's SLOT is its place on the grid, row-major, `NO_CELL`
/// the last (one cell).
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
    pub fn new(side: u32) -> Geometry {
        assert!(side == 8 || side == 16, "a storage region is 8 or 16 cells wide, not {side}");
        let per_row = GRID as u32 / side;
        Geometry { side, per_row, slots: per_row * per_row + 1, words: (side * side / 64) as usize }
    }

    /// The slot of `NO_CELL`'s region.
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

/// The process's storage geometry: `CELESTE_STORAGE_REGION` (8 default, or
/// 16). A storage region must nest in one kernel region: the kernels' px
/// (`CELESTE_REGION`) a multiple of the side - else refused.
pub fn geometry() -> &'static Geometry {
    static G: std::sync::OnceLock<Geometry> = std::sync::OnceLock::new();
    G.get_or_init(|| {
        let side: u32 = match std::env::var("CELESTE_STORAGE_REGION") {
            Ok(v) => v.trim().parse().unwrap_or_else(|_| panic!("CELESTE_STORAGE_REGION={v:?}: 8 or 16")),
            Err(_) => 8,
        };
        let g = Geometry::new(side);
        if let Some(k) = crate::trace::kernel::region_grid() {
            assert!(
                k.px % side as i32 == 0,
                "CELESTE_STORAGE_REGION={side}: a storage region must nest in one kernel region, and the kernels' regions are {} px (CELESTE_REGION)",
                k.px
            );
        }
        g
    })
}

/// A state's id: its region (shape index * slots + slot), its entry number
/// in the region, its cell's index in the region - `region << 40 | entry <<
/// 8 | local`. Ids sort by shape, then position (regions row-major), then
/// entry: a layer is stored in id order.
pub type StateId = u64;

pub const REGION_BITS: u32 = 24;

#[inline]
pub fn state_id(region: u32, entry: u32, local: u32) -> StateId {
    debug_assert!(region < 1 << REGION_BITS && local < 256);
    (region as u64) << 40 | (entry as u64) << 8 | local as u64
}

#[inline]
pub fn id_region(id: StateId) -> u32 {
    (id >> 40) as u32
}

#[inline]
pub fn id_entry(id: StateId) -> u32 {
    (id >> 8) as u32
}

#[inline]
pub fn id_local(id: StateId) -> u32 {
    (id & 0xff) as u32
}

/// A region's index from its shape's index and its slot.
#[inline]
pub fn region_of(geo: &Geometry, shape_idx: u32, slot: u32) -> u32 {
    let r = shape_idx as u64 * geo.slots as u64 + slot as u64;
    assert!(r < 1 << REGION_BITS, "region {r} past the {REGION_BITS} bits an id holds ({shape_idx} shapes)");
    r as u32
}

/// An id's shape index and slot.
#[inline]
pub fn region_parts(geo: &Geometry, region: u32) -> (u32, u32) {
    (region / geo.slots, region % geo.slots)
}

/// An id's cell.
#[inline]
pub fn id_cell(geo: &Geometry, id: StateId) -> u32 {
    geo.cell(id_region(id) % geo.slots, id_local(id))
}

/// An id as text (`r{region}.e{entry}.c{local}`).
pub fn show_id(id: StateId) -> String {
    format!("r{}.e{}.c{}", id_region(id), id_entry(id), id_local(id))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Slots and cells round-trip at both sides; regions nest in 16 px
    /// kernel regions (`div_euclid` on the room's pixels).
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
                // The 16 px kernel region holding the cell holds the whole storage region.
                let k = |v: i32| v.div_euclid(16);
                let (x0, y0) = (rx as i32 * side as i32 - 64, ry as i32 * side as i32 - 64);
                assert_eq!((k(x0), k(y0)), (k(x0 + side as i32 - 1), k(y0 + side as i32 - 1)));
            }
            assert_eq!(g.slot_local(NO_CELL), (g.no_cell_slot(), 0));
            assert_eq!(g.cell(g.no_cell_slot(), 0), NO_CELL);
        }
        let id = state_id(77, 123_456, 63);
        assert_eq!((id_region(id), id_entry(id), id_local(id)), (77, 123_456, 63));
    }
}
