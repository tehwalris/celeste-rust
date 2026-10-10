//! The backward's view of the storage: MARKS over state ids (a cell mask per
//! entry, as the visited set holds states) with dense RANKS, and the
//! RESOLVER of an id to its `(shape, key, cell)` from the storage metadata
//! (no frame file read).

use anyhow::{Context, Result};
use rustc_hash::FxHashMap;

use super::visited::Key;
use super::{id_entry, id_local, id_region, Geometry, StateId};

/// A set of states as masks per (region, entry), with each member's
/// DEADLINE (the remainder-free BFS's) and LAYER.
pub struct Marks {
    words: usize,
    masks: FxHashMap<u32, Vec<u64>>,
    /// Every member: id, deadline, layer.
    members: Vec<(StateId, u16, u32)>,
}

impl Marks {
    pub fn new(geo: &Geometry) -> Self {
        Marks { words: geo.words, masks: Default::default(), members: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.members.len()
    }

    pub fn is_empty(&self) -> bool {
        self.members.is_empty()
    }

    #[inline]
    fn at(&self, id: StateId) -> (usize, u64) {
        (id_entry(id) as usize * self.words + (id_local(id) / 64) as usize, 1u64 << (id_local(id) % 64))
    }

    #[inline]
    pub fn contains(&self, id: StateId) -> bool {
        let (w, b) = self.at(id);
        self.masks.get(&id_region(id)).and_then(|v| v.get(w)).is_some_and(|x| x & b != 0)
    }

    /// Add `id` (first reached at `layer`) with `deadline`; true if new.
    pub fn insert(&mut self, id: StateId, deadline: u32, layer: u32) -> bool {
        let (w, b) = self.at(id);
        let words = self.words;
        let v = self.masks.entry(id_region(id)).or_default();
        if v.len() <= w {
            v.resize((w / words + 1) * words, 0);
        }
        if v[w] & b != 0 {
            return false;
        }
        v[w] |= b;
        self.members.push((id, u16::try_from(deadline).expect("a deadline past u16"), layer));
        true
    }

    /// The marks as DENSE numbers - a member's rank in id order - and the
    /// members (id, deadline, layer) in that order. Consumes the marks.
    pub fn into_ranked(self) -> (MarkRanks, Vec<(StateId, u16, u32)>) {
        let mut members = self.members;
        members.sort_unstable();
        let mut regions: Vec<u32> = self.masks.keys().copied().collect();
        regions.sort_unstable();
        let mut masks = self.masks;
        let mut by_region = FxHashMap::default();
        let mut base = 0u64;
        for r in regions {
            let words = masks.remove(&r).expect("a key of the map");
            let mut before = Vec::with_capacity(words.len());
            for w in &words {
                before.push(u32::try_from(base).expect("more than u32::MAX marks"));
                base += w.count_ones() as u64;
            }
            by_region.insert(r, (words, before));
        }
        debug_assert_eq!(base as usize, members.len());
        (MarkRanks { words: self.words, by_region }, members)
    }
}

/// The members' dense numbers (`Marks::into_ranked`): per region its masks
/// and, per word, the members before it.
pub struct MarkRanks {
    words: usize,
    by_region: FxHashMap<u32, (Vec<u64>, Vec<u32>)>,
}

impl MarkRanks {
    /// Region `region`'s members, for ranks by (entry, cell) without a
    /// map lookup each.
    pub fn region(&self, region: u32) -> Option<RegionRanks<'_>> {
        self.by_region.get(&region).map(|(masks, before)| RegionRanks { words: self.words, masks, before })
    }

    /// `id`'s rank among the members (`None`: not one).
    #[inline]
    pub fn rank(&self, id: StateId) -> Option<u32> {
        let (masks, before) = self.by_region.get(&id_region(id))?;
        let w = id_entry(id) as usize * self.words + (id_local(id) / 64) as usize;
        let b = id_local(id) % 64;
        let word = *masks.get(w)?;
        (word >> b & 1 == 1).then(|| before[w] + (word & ((1u64 << b) - 1)).count_ones())
    }
}

/// One region's ranks (`MarkRanks::region`).
#[derive(Clone, Copy)]
pub struct RegionRanks<'a> {
    words: usize,
    masks: &'a [u64],
    before: &'a [u32],
}

impl RegionRanks<'_> {
    /// The rank of the region's state `(entry, local)` (`None`: not a member).
    #[inline]
    pub fn rank(&self, entry: u32, local: u32) -> Option<u32> {
        let w = entry as usize * self.words + (local / 64) as usize;
        let b = local % 64;
        let word = *self.masks.get(w)?;
        (word >> b & 1 == 1).then(|| self.before[w] + (word & ((1u64 << b) - 1)).count_ones())
    }
}

/// An id's `(shape, key, cell)`: the shapes by index and every entry's key,
/// from a tree's storage metadata (`storage::meta`), and the tree's key
/// space (to key other rows as the tree's: the concrete search, `known`).
pub struct Resolver {
    geo: Geometry,
    shapes: Vec<u64>,
    keys: FxHashMap<u32, Vec<Key>>,
    pub space: celeste_engine::exact::KeySpace,
}

impl Resolver {
    /// The metadata of the tree in `dir` through frame `last` (frames past
    /// a dead frontier have none).
    pub fn load(dir: &std::path::Path, last: u32) -> Result<Self> {
        let geo = *super::geometry();
        let mut shapes: Vec<(u32, u64)> = Vec::new();
        let mut entries: Vec<(u32, u32, Key)> = Vec::new();
        let mut adds = celeste_engine::exact::KeyAdditions::default();
        for f in 0..=last {
            if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
                continue;
            }
            for m in super::meta::load_frame(dir, f).with_context(|| format!("{}: frame {f}'s storage metadata", dir.display()))? {
                shapes.extend(m.shapes);
                entries.extend(m.entries);
                adds.extend(m.keys);
            }
        }
        let space = celeste_engine::exact::KeySpace::from_additions(adds).with_context(|| format!("{}: the key space", dir.display()))?;
        shapes.sort_unstable();
        anyhow::ensure!(shapes.iter().enumerate().all(|(i, s)| s.0 as usize == i), "{}: the shapes are not numbered 0..{}", dir.display(), shapes.len());
        entries.sort_unstable();
        let mut keys: FxHashMap<u32, Vec<Key>> = FxHashMap::default();
        for (r, e, k) in entries {
            let v = keys.entry(r).or_default();
            anyhow::ensure!(e as usize == v.len(), "{}: region {r}'s entries are not numbered densely", dir.display());
            v.push(k);
        }
        Ok(Resolver { geo, shapes: shapes.into_iter().map(|s| s.1).collect(), keys, space })
    }

    /// `id`'s `(shape, key, cell)`.
    pub fn resolve(&self, id: StateId) -> Result<(u64, Key, u32)> {
        let r = id_region(id);
        let shape = *self.shapes.get((r / self.geo.slots) as usize).with_context(|| format!("state {}: no shape", super::show_id(id)))?;
        let key = *self.keys.get(&r).and_then(|v| v.get(id_entry(id) as usize)).with_context(|| format!("state {}: no entry", super::show_id(id)))?;
        Ok((shape, key, super::id_cell(&self.geo, id)))
    }
}
