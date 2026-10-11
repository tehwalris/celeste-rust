//! The RESOLVER of a state id to its `(shape, key, cell)` from the storage
//! metadata (no frame file read): the arc phase's fingerprints, marks files
//! and node keys.

use anyhow::{Context, Result};
use rustc_hash::FxHashMap;

use super::visited::Key;
use super::{id_entry, id_region, Geometry, StateId};

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
        let geo = super::GEOMETRY;
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

impl Resolver {
    /// `f(shape, key)` of every entry, looked up by id like `resolve`: a
    /// per-entry value computed once, not per state or edge (`ckhash`'s
    /// content hashes).
    pub fn per_entry(&self, f: impl Fn(u64, Key) -> u64) -> PerEntry {
        let vals = self
            .keys
            .iter()
            .map(|(&r, keys)| {
                let shape = self.shapes.get((r / self.geo.slots) as usize).copied();
                (r, keys.iter().map(|&k| shape.map_or(0, |s| f(s, k))).collect())
            })
            .collect();
        PerEntry { geo: self.geo, vals, shapes: self.shapes.len() }
    }
}

/// `Resolver::per_entry`'s values.
pub struct PerEntry {
    geo: Geometry,
    vals: FxHashMap<u32, Vec<u64>>,
    shapes: usize,
}

impl PerEntry {
    /// `id`'s entry's value and `id`'s cell.
    #[inline]
    pub fn get(&self, id: StateId) -> Result<(u64, u32)> {
        let r = id_region(id);
        anyhow::ensure!(((r / self.geo.slots) as usize) < self.shapes, "state {}: no shape", super::show_id(id));
        let v = *self.vals.get(&r).and_then(|v| v.get(id_entry(id) as usize)).with_context(|| format!("state {}: no entry", super::show_id(id)))?;
        Ok((v, super::id_cell(&self.geo, id)))
    }
}
