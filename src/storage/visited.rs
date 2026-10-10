//! THE VISITED SET: every state the forward has reached, per (shape,
//! region) a table of ENTRIES - a non-position key and a mask over the
//! region's cells - numbered densely in the region (a state's id is its
//! entry and cell, `storage::StateId`). Read-only while a wave's units run;
//! written only by the translation, one worker per region (`wave`).

use rustc_hash::FxHashMap;

use super::{region_of, Geometry};

pub type Key = (u64, u64);

/// One region's entries: key and mask by entry number, an open-addressing
/// index on the key (entry + 1, 0 empty).
#[derive(Default, Clone)]
pub struct RegionTable {
    keys: Vec<Key>,
    /// `words` u64 per entry.
    masks: Vec<u64>,
    index: Vec<u32>,
}

impl RegionTable {
    pub fn len(&self) -> usize {
        self.keys.len()
    }

    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// The entry holding `k`.
    #[inline]
    pub fn find(&self, k: Key) -> Option<u32> {
        if self.index.is_empty() {
            return None;
        }
        let m = self.index.len() - 1;
        let mut i = (k.0 as usize) & m;
        loop {
            let e = self.index[i];
            if e == 0 {
                return None;
            }
            if self.keys[e as usize - 1] == k {
                return Some(e - 1);
            }
            i = (i + 1) & m;
        }
    }

    /// Entry `e`'s mask.
    #[inline]
    pub fn mask(&self, words: usize, e: u32) -> &[u64] {
        &self.masks[e as usize * words..(e as usize + 1) * words]
    }

    #[inline]
    pub fn key(&self, e: u32) -> Key {
        self.keys[e as usize]
    }

    /// Does entry `e` hold cell `local`?
    #[inline]
    pub fn has(&self, words: usize, e: u32, local: u32) -> bool {
        self.masks[e as usize * words + (local / 64) as usize] >> (local % 64) & 1 == 1
    }

    /// Append the entry `k` (absent: checked); its number.
    pub fn push_key(&mut self, words: usize, k: Key) -> u32 {
        debug_assert!(self.find(k).is_none());
        let e = self.keys.len() as u32;
        self.keys.push(k);
        self.masks.extend(std::iter::repeat_n(0, words));
        if (self.keys.len() + 1) * 2 > self.index.len() {
            self.reindex();
        } else {
            self.place(e);
        }
        e
    }

    /// Append entry `e` with key `k` (a resume, entries in number order):
    /// `e` must be the next number. The index is rebuilt by `reindex`.
    pub fn push_numbered(&mut self, words: usize, e: u32, k: Key) -> anyhow::Result<()> {
        anyhow::ensure!(e as usize == self.keys.len(), "entry {e} restored after {} entries", self.keys.len());
        self.keys.push(k);
        self.masks.extend(std::iter::repeat_n(0, words));
        Ok(())
    }

    /// Rebuild the index over every entry (after `push_numbered`s); a key
    /// held by two entries is an error.
    pub fn reindex(&mut self) {
        let cap = ((self.keys.len() + 1) * 2).next_power_of_two().max(16);
        self.index = vec![0; cap];
        for e in 0..self.keys.len() as u32 {
            assert!(self.find(self.keys[e as usize]).is_none(), "two entries of one region hold the key {:?}", self.keys[e as usize]);
            self.place(e);
        }
    }

    fn place(&mut self, e: u32) {
        let m = self.index.len() - 1;
        let mut i = (self.keys[e as usize].0 as usize) & m;
        while self.index[i] != 0 {
            i = (i + 1) & m;
        }
        self.index[i] = e + 1;
    }

    /// Set cell `local` of entry `e`; true if it was not set.
    #[inline]
    pub fn set(&mut self, words: usize, e: u32, local: u32) -> bool {
        let w = &mut self.masks[e as usize * words + (local / 64) as usize];
        let b = 1u64 << (local % 64);
        let new = *w & b == 0;
        *w |= b;
        new
    }

    /// States held (bits set).
    pub fn states(&self) -> u64 {
        self.masks.iter().map(|m| m.count_ones() as u64).sum()
    }

    pub fn alloc_bytes(&self) -> usize {
        self.keys.capacity() * 16 + self.masks.capacity() * 8 + self.index.capacity() * 4
    }
}

/// The visited set: the shapes (index -> hash, in the order the tree
/// numbered them) and a table per region (`StateId`'s region index).
#[derive(Clone)]
pub struct VisitedSet {
    pub geo: Geometry,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    regions: Vec<Option<Box<RegionTable>>>,
}

impl VisitedSet {
    pub fn new(geo: Geometry) -> Self {
        VisitedSet { geo, shapes: Vec::new(), shape_idx: FxHashMap::default(), regions: Vec::new() }
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
        self.regions.resize_with(self.shapes.len() * self.geo.slots as usize, || None);
        i
    }

    /// A shape's index from the tree's record (a resume): it must be the next.
    pub fn restore_shape(&mut self, idx: u32, shape: u64) -> anyhow::Result<()> {
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
        self.regions.get(region as usize).and_then(|t| t.as_deref())
    }

    /// Region `region`'s table, created empty if absent (its shape numbered).
    pub fn table_mut(&mut self, region: u32) -> &mut RegionTable {
        self.regions[region as usize].get_or_insert_with(Default::default)
    }

    /// Every region's table, by index (the translation takes them apart).
    pub fn tables_mut(&mut self) -> &mut [Option<Box<RegionTable>>] {
        &mut self.regions
    }

    /// The state's entry, if `(shape, slot, key)` has one.
    #[inline]
    pub fn find(&self, shape: u64, slot: u32, k: Key) -> Option<(u32, u32)> {
        let r = self.region(shape, slot)?;
        Some((r, self.table(r)?.find(k)?))
    }

    /// States held.
    pub fn len(&self) -> usize {
        self.regions.iter().flatten().map(|t| t.states() as usize).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Entries held.
    pub fn entries(&self) -> usize {
        self.regions.iter().flatten().map(|t| t.len()).sum()
    }

    pub fn alloc_bytes(&self) -> usize {
        self.regions.capacity() * 8 + self.regions.iter().flatten().map(|t| t.alloc_bytes()).sum::<usize>()
    }
}
