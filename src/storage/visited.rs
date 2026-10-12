//! THE VISITED SET: every state the forward has reached - a `set::RegionSet`
//! (per (shape, region) a table of entries: a position-free key and a mask
//! over the region's cells), INDEXED, with the shapes it numbered and the key
//! space its keys are in, and per state its LAYER and remainder HULL
//! (`StateInfo`). READ-ONLY during a wave: the wave's new states go to the
//! next layer (another `RegionSet`), which the frame's end merges in
//! (`merge`), and the hulls that grew are set then (`set_info`).

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::set::{RegionSet, RegionTable};
use super::{region_of, Geometry};
use crate::search::hull::Hull;

/// A state's EXACT packed key within its shape (`celeste_engine::exact`).
pub use celeste_engine::exact::Key;
use celeste_engine::exact::KeySpace;

/// A state's LAYER (the frame the forward first reached it, whose wave
/// expanded it) and its remainder HULL so far.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StateInfo {
    pub hull: Hull,
    pub layer: u16,
}

/// The visited set: the shapes (index -> hash, in the order the tree
/// numbered them), the states (`RegionSet`, by `storage::region_of`), the
/// key space their keys are in (read-only during a wave), and per state its
/// layer and hull (per region, by `entry << 6 | cell`: entries keep their
/// places).
#[derive(Clone)]
pub struct VisitedSet {
    pub geo: Geometry,
    pub keys: KeySpace,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    set: RegionSet,
    info: FxHashMap<u32, FxHashMap<u32, StateInfo>>,
}

impl VisitedSet {
    pub fn new(geo: Geometry) -> Self {
        VisitedSet { geo, keys: KeySpace::new(), shapes: Vec::new(), shape_idx: FxHashMap::default(), set: RegionSet::default(), info: FxHashMap::default() }
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

    /// The layer and hull of the state at cell `local` of entry `e` of
    /// region `region`'s table.
    #[inline]
    pub fn info(&self, region: u32, e: u32, local: u32) -> Option<StateInfo> {
        self.info.get(&region)?.get(&(e << 6 | local)).copied()
    }

    /// The layer and hull of state `(region, key, local)`, if held.
    pub fn state(&self, region: u32, key: Key, local: u32) -> Option<StateInfo> {
        let e = self.table(region)?.find(key)?;
        self.info(region, e, local)
    }

    /// SET the layer and hull of states `(key, local)` of region `region`
    /// (held by the set: checked).
    pub fn set_info(&mut self, region: u32, states: impl Iterator<Item = (Key, u32, StateInfo)>) -> Result<()> {
        let t = self.set.table(region).ok_or_else(|| anyhow::anyhow!("region {region}: no table for its states' hulls"))?;
        let m = self.info.entry(region).or_default();
        for (key, local, i) in states {
            let e = t.find(key).filter(|&e| t.mask(e) >> local & 1 == 1).ok_or_else(|| anyhow::anyhow!("region {region}: a hull for a state the set lacks"))?;
            m.insert(e << 6 | local, i);
        }
        Ok(())
    }

    /// The states with a layer and hull.
    pub fn infos(&self) -> usize {
        self.info.values().map(|m| m.len()).sum()
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

    /// Bytes allocated: the set's, and the hulls' (their maps' slots).
    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes() + self.info.values().map(|m| m.capacity() * (std::mem::size_of::<(u32, StateInfo)>() + 1)).sum::<usize>()
    }
}
