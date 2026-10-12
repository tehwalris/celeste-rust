//! THE VISITED SET: every state the forward has reached - a `set::RegionSet`
//! (per (shape, region) a table of entries: a position-free key and a mask
//! over the region's cells), INDEXED, with the shapes it numbered and the key
//! space its keys are in. A pure dedup structure, and READ-ONLY during a
//! wave: the wave's new states go to the next layer (another `RegionSet`),
//! which the frame's end merges in (`merge`).

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::set::{RegionSet, RegionTable};
use super::{region_of, Geometry};

/// A state's EXACT packed key within its shape (`celeste_engine::exact`).
pub use celeste_engine::exact::Key;
use celeste_engine::exact::KeySpace;

/// The visited set: the shapes (index -> hash, in the order the tree
/// numbered them), the states (`RegionSet`, by `storage::region_of`), and
/// the key space their keys are in (read-only during a wave).
#[derive(Clone)]
pub struct VisitedSet {
    pub geo: Geometry,
    pub keys: KeySpace,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    set: RegionSet,
}

impl VisitedSet {
    pub fn new(geo: Geometry) -> Self {
        VisitedSet { geo, keys: KeySpace::new(), shapes: Vec::new(), shape_idx: FxHashMap::default(), set: RegionSet::default() }
    }

    pub fn shape_index(&self, shape: u64) -> Option<u32> {
        self.shape_idx.get(&shape).copied()
    }

    pub fn shape_hash(&self, idx: u32) -> u64 {
        self.shapes[idx as usize]
    }

    pub fn shapes(&self) -> &[u64] {
        &self.shapes
    }

    /// Number `shape` (the next index) unless it has one; its index.
    pub fn add_shape(&mut self, shape: u64) -> u32 {
        if let Some(&i) = self.shape_idx.get(&shape) {
            return i;
        }
        let i = self.shapes.len() as u32;
        self.shapes.push(shape);
        self.shape_idx.insert(shape, i);
        i
    }

    /// A shape's index from the tree's record (a resume): it must be the next.
    pub fn restore_shape(&mut self, idx: u32, shape: u64) -> Result<()> {
        match self.shape_idx.get(&shape) {
            Some(&i) => anyhow::ensure!(i == idx, "shape {shape:016x} numbered {i} and {idx}"),
            None => {
                anyhow::ensure!(idx as usize == self.shapes.len(), "shape {shape:016x} numbered {idx}, the next is {}", self.shapes.len());
                self.add_shape(shape);
            }
        }
        Ok(())
    }

    /// The region of `(shape, slot)`, if the shape is numbered.
    #[inline]
    pub fn region(&self, shape: u64, slot: u32) -> Option<u32> {
        self.shape_index(shape).map(|s| region_of(&self.geo, s, slot))
    }

    #[inline]
    pub fn table(&self, region: u32) -> Option<&RegionTable> {
        self.set.table(region)
    }

    /// The state's entry, if `(shape, slot, key)` has one.
    #[inline]
    pub fn find(&self, shape: u64, slot: u32, k: Key) -> Option<(u32, u32)> {
        let r = self.region(shape, slot)?;
        Some((r, self.table(r)?.find(k)?))
    }

    /// MERGE the frozen set `new` (a frame's states, of numbered shapes) in:
    /// per region its entries ORed in, made where absent; the regions on
    /// `threads` threads. Returns the cells that were held already (0: the
    /// set and `new` disjoint, as a frame's new states are).
    pub fn merge(&mut self, new: &RegionSet, threads: usize) -> Result<u64> {
        for (region, _) in new.tables() {
            anyhow::ensure!(((region / self.geo.slots) as usize) < self.shapes.len(), "a merged region {region} of no numbered shape");
            self.set.table_mut(*region, RegionTable::new);
        }
        let mut work: Vec<std::sync::Mutex<(&mut RegionTable, &RegionTable)>> = Vec::with_capacity(new.tables().len());
        let mut theirs = new.tables().iter().peekable();
        for (r, t) in self.set.tables_mut() {
            if let Some((_, n)) = theirs.next_if(|x| x.0 == *r) {
                work.push(std::sync::Mutex::new((t, n)));
            }
        }
        let held = super::wave::par_map(&work, threads, |w| {
            let mut g = w.lock().expect("a merge");
            let (t, n) = &mut *g;
            let before = t.states();
            t.merge(n);
            before + n.states() - t.states()
        });
        Ok(held.into_iter().sum())
    }

    /// States held.
    pub fn len(&self) -> usize {
        self.set.states() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Entries held.
    pub fn entries(&self) -> usize {
        self.set.entries()
    }

    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes()
    }
}
