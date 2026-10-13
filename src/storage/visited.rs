//! THE VISITED SET: every state the forward has reached - a `set::RegionSet`
//! (per (shape, region) a table of entries: a position-free key and a mask
//! over the region's cells), INDEXED, with the shapes it numbered and the key
//! space its keys are in, and per state its LAYER and remainder HULL
//! (`StateInfo`, `RegionInfos`: none for a state whose hull is the whole
//! torus - it never grows, nor is it pushed again). READ-ONLY during a wave:
//! the wave's new states go to the next layer (another `RegionSet`), which
//! the frame's end merges in with the hulls that grew (`absorb`).

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

impl StateInfo {
    /// Is its hull the whole torus (then it is not stored)?
    #[inline]
    pub fn is_full(&self) -> bool {
        self.hull.x.is_full() && self.hull.y.is_full()
    }
}

/// One region's LAYERS AND HULLS: per entry (its place in the region's
/// table: entries keep their places) the cells with one and where their RUN
/// starts in `slots` (the cells in order). A state whose hull is FULL has no
/// slot. A run moves to the end when its entry gains a cell; dead slots are
/// reclaimed once they are a third of the slots, and the arrays grow by a
/// quarter: 10 B a state stored and 12 B an entry, at most ~1.9x that held
/// (room (1,0) f99: 0.78 GB for 31M stored states and 4.5M entries when
/// dead slots waited to be half and the arrays doubled).
#[derive(Clone, Default)]
pub struct RegionInfos {
    starts: Vec<u32>,
    masks: Vec<u64>,
    slots: Vec<StateInfo>,
    dead: usize,
}

/// Room in `v` for `more` (grown by a quarter at least: not doubled).
fn grow<T>(v: &mut Vec<T>, more: usize) {
    if v.len() + more > v.capacity() {
        v.reserve_exact(more.max(v.len() / 4));
    }
}

impl RegionInfos {
    /// Entry `e`'s cell `local`'s, if stored (`None`: full, or no state).
    #[inline]
    pub fn get(&self, e: u32, local: u32) -> Option<StateInfo> {
        let mask = *self.masks.get(e as usize)?;
        (mask >> local & 1 == 1).then(|| self.slots[self.starts[e as usize] as usize + (mask & ((1u64 << local) - 1)).count_ones() as usize])
    }

    /// Entry `e`'s cells set (`updates`: by cell, ascending, distinct; a
    /// full hull's slot goes).
    pub fn set(&mut self, e: u32, updates: &[(u32, StateInfo)]) {
        debug_assert!(updates.windows(2).all(|w| w[0].0 < w[1].0));
        let e = e as usize;
        if self.masks.len() <= e {
            let more = e + 1 - self.masks.len();
            grow(&mut self.starts, more);
            grow(&mut self.masks, more);
            self.starts.resize(e + 1, 0);
            self.masks.resize(e + 1, 0);
        }
        let (start, mask) = (self.starts[e], self.masks[e]);
        let mut next = mask;
        for &(c, i) in updates {
            if i.is_full() {
                next &= !(1u64 << c);
            } else {
                next |= 1u64 << c;
            }
        }
        if next == mask {
            for &(c, i) in updates {
                if mask >> c & 1 == 1 {
                    self.slots[start as usize + (mask & ((1u64 << c) - 1)).count_ones() as usize] = i;
                }
            }
            return;
        }
        // A new run at the end.
        let at = self.slots.len();
        assert!(at + 64 < u32::MAX as usize, "a region of 2^32 hulls");
        grow(&mut self.slots, next.count_ones() as usize);
        let mut u = updates.iter().peekable();
        let mut m = next;
        while m != 0 {
            let c = m.trailing_zeros();
            m &= m - 1;
            while u.peek().is_some_and(|x| x.0 < c) {
                u.next();
            }
            let v = match u.peek() {
                Some(&&(uc, i)) if uc == c => i,
                _ => self.slots[start as usize + (mask & ((1u64 << c) - 1)).count_ones() as usize],
            };
            self.slots.push(v);
        }
        self.dead += mask.count_ones() as usize;
        (self.starts[e], self.masks[e]) = (at as u32, next);
        if self.dead > 1024 && self.dead * 3 > self.slots.len() {
            self.compact();
        }
    }

    /// The live runs, entry by entry, at the start of a new slot array.
    fn compact(&mut self) {
        let mut slots = Vec::with_capacity(self.slots.len() - self.dead);
        for (start, mask) in self.starts.iter_mut().zip(&self.masks) {
            let n = mask.count_ones() as usize;
            let a = slots.len() as u32;
            slots.extend_from_slice(&self.slots[*start as usize..*start as usize + n]);
            *start = a;
        }
        self.slots = slots;
        self.dead = 0;
    }

    /// States with a slot.
    pub fn stored(&self) -> usize {
        self.slots.len() - self.dead
    }

    pub fn alloc_bytes(&self) -> usize {
        self.starts.capacity() * 4 + self.masks.capacity() * 8 + self.slots.capacity() * std::mem::size_of::<StateInfo>()
    }
}

/// The visited set: the shapes (index -> hash, in the order the tree
/// numbered them), the states (`RegionSet`, by `storage::region_of`), the
/// key space their keys are in (read-only during a wave), and per state its
/// layer and hull (per region, `RegionInfos`).
#[derive(Clone)]
pub struct VisitedSet {
    pub geo: Geometry,
    pub keys: KeySpace,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    set: RegionSet,
    info: FxHashMap<u32, RegionInfos>,
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
            t.merge(n, |_, _| {});
            before + n.states() - t.states()
        });
        Ok(held.into_iter().sum())
    }

    /// The layer and hull of the state at cell `local` of entry `e` of
    /// region `region`'s table (`None`: its hull is FULL, or no state).
    #[inline]
    pub fn info(&self, region: u32, e: u32, local: u32) -> Option<StateInfo> {
        self.info.get(&region)?.get(e, local)
    }

    /// The layer and hull of state `(region, key, local)` (`None`: full, or
    /// no state).
    pub fn state(&self, region: u32, key: Key, local: u32) -> Option<StateInfo> {
        let e = self.table(region)?.find(key)?;
        self.info(region, e, local)
    }

    /// `states` (sorted by key and cell, held by the set: checked) put in
    /// `m`, an entry at a time.
    fn put_all(t: &RegionTable, m: &mut RegionInfos, region: u32, states: &mut dyn Iterator<Item = (Key, u32, StateInfo)>) -> Result<()> {
        let mut batch: Vec<(u32, StateInfo)> = Vec::new();
        let mut cur: Option<(Key, u32)> = None;
        for (key, local, i) in states {
            if cur.is_none_or(|c| c.0 != key) {
                if let Some((_, e)) = cur {
                    m.set(e, &batch);
                }
                batch.clear();
                let e = t.find(key).ok_or_else(|| anyhow::anyhow!("region {region}: a hull for a state the set lacks"))?;
                cur = Some((key, e));
            }
            let (_, e) = cur.expect("set above");
            anyhow::ensure!(t.mask(e) >> local & 1 == 1, "region {region}: a hull for a state the set lacks");
            anyhow::ensure!(batch.last().is_none_or(|b| b.0 < local), "region {region}: hulls not by key and cell");
            batch.push((local, i));
        }
        if let Some((_, e)) = cur {
            m.set(e, &batch);
        }
        Ok(())
    }

    /// SET layers and hulls, many regions at once, on `threads` threads:
    /// per group `(region, g)` (distinct regions) `each(g)` its states,
    /// sorted by key and cell.
    #[allow(clippy::type_complexity)]
    pub fn set_infos<G: Sync>(&mut self, groups: &[(u32, G)], each: impl Fn(&G) -> Vec<(Key, u32, StateInfo)> + Sync, threads: usize) -> Result<()> {
        for (region, _) in groups {
            self.info.entry(*region).or_default();
        }
        let want: FxHashMap<u32, usize> = groups.iter().enumerate().map(|(i, g)| (g.0, i)).collect();
        let set = &self.set;
        let work: Vec<std::sync::Mutex<(u32, &mut RegionInfos, usize)>> = self.info.iter_mut().filter_map(|(r, m)| want.get(r).map(|&i| std::sync::Mutex::new((*r, m, i)))).collect();
        let done = super::wave::par_map(&work, threads, |w| -> Result<()> {
            let mut g = w.lock().expect("a region's hulls");
            let (region, m, i) = &mut *g;
            let t = set.table(*region).ok_or_else(|| anyhow::anyhow!("region {region}: no table for its states' hulls"))?;
            Self::put_all(t, m, *region, &mut each(&groups[*i].1).into_iter())
        });
        done.into_iter().collect()
    }

    /// The states with a layer and hull stored (not full).
    pub fn infos(&self) -> usize {
        self.info.values().map(|m| m.stored()).sum()
    }

    /// A FRAME's states in, regions on `threads` threads: per region (each
    /// once) its NEW entries (frozen, of a numbered shape) with their hulls
    /// in its state order - merged in at layer `layer` - and the states
    /// whose hull grew (held already; sorted by state) with their new
    /// hulls. Returns the new cells held already (0: the frame's new states
    /// are new).
    #[allow(clippy::type_complexity)]
    pub fn absorb(&mut self, parts: &[(u32, &RegionTable, &[Hull], &[super::wave::Old])], layer: u16, threads: usize) -> Result<u64> {
        for &(region, ..) in parts {
            anyhow::ensure!(((region / self.geo.slots) as usize) < self.shapes.len(), "an absorbed region {region} of no numbered shape");
            self.set.table_mut(region, RegionTable::new);
            self.info.entry(region).or_default();
        }
        let want: FxHashMap<u32, usize> = parts.iter().enumerate().map(|(i, p)| (p.0, i)).collect();
        let mut tables: FxHashMap<u32, &mut RegionTable> = self.set.tables_mut().iter_mut().filter(|(r, _)| want.contains_key(r)).map(|(r, t)| (*r, t)).collect();
        let work: Vec<std::sync::Mutex<(&mut RegionTable, &mut RegionInfos, usize)>> = self
            .info
            .iter_mut()
            .filter_map(|(r, m)| want.get(r).map(|&i| std::sync::Mutex::new((tables.remove(r).expect("a table made above"), m, i))))
            .collect();
        let held = super::wave::par_map(&work, threads, |w| -> Result<u64> {
            let mut g = w.lock().expect("a region");
            let (t, m, i) = &mut *g;
            let (region, new, hulls, grown) = parts[*i];
            let before = t.states();
            let mut entries: Vec<u32> = Vec::with_capacity(new.len());
            t.merge(new, |_, e| entries.push(e));
            let held = before + new.states() - t.states();
            let mut h = hulls.iter();
            let mut batch: Vec<(u32, StateInfo)> = Vec::new();
            for ((_, mask), &e) in new.iter().zip(&entries) {
                batch.clear();
                let mut c = mask;
                while c != 0 {
                    let local = c.trailing_zeros();
                    c &= c - 1;
                    batch.push((local, StateInfo { hull: *h.next().ok_or_else(|| anyhow::anyhow!("region {region}: fewer hulls than new states"))?, layer }));
                }
                m.set(e, &batch);
            }
            anyhow::ensure!(h.next().is_none(), "region {region}: more hulls than new states");
            Self::put_all(t, m, region, &mut grown.iter().map(|o| (o.sk.key, o.sk.local as u32, StateInfo { hull: o.hull, layer: o.layer })))?;
            Ok(held)
        });
        held.into_iter().sum()
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

    /// Bytes allocated: the set's, and the hulls'.
    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes() + self.info.values().map(|m| m.alloc_bytes()).sum::<usize>()
    }
}
