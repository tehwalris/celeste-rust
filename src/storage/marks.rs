//! The RESOLVER of a state id to its `(shape, key, cell)` from the storage
//! metadata (no frame file read): the arc phase's fingerprints, marks files
//! and node keys.

use anyhow::{Context, Result};
use rustc_hash::FxHashMap;

use super::visited::Key;
use super::{id_entry, id_region, Geometry, StateId};

/// An id's `(shape, key, cell)`: the shapes by index and the entries' keys,
/// from a tree's storage metadata (`storage::meta`), and the tree's key
/// space (to key other rows as the tree's: the concrete search, `known`).
pub struct Resolver {
    geo: Geometry,
    shapes: Vec<u64>,
    keys: Keys,
    pub space: celeste_engine::exact::KeySpace,
}

/// The entries' keys: every entry's, by region (`load`); or some entries'
/// alone, sorted by (region, entry) (`load_for`).
enum Keys {
    Dense(FxHashMap<u32, Vec<Key>>),
    Sparse(Vec<((u32, u32), Key)>),
}

impl Resolver {
    /// The metadata of the tree in `dir` through frame `last` (frames past
    /// a dead frontier have none).
    pub fn load(dir: &std::path::Path, last: u32) -> Result<Self> {
        let (shapes, mut entries, space) = Self::read(dir, last, |_, _| true)?;
        entries.sort_unstable();
        let mut keys: FxHashMap<u32, Vec<Key>> = FxHashMap::default();
        for ((r, e), k) in entries {
            let v = keys.entry(r).or_default();
            anyhow::ensure!(e as usize == v.len(), "{}: region {r}'s entries are not numbered densely", dir.display());
            v.push(k);
        }
        Ok(Resolver { geo: super::GEOMETRY, shapes, keys: Keys::Dense(keys), space })
    }

    /// The same, keeping the keys of the entries `wanted` alone (sorted
    /// `(region, entry)`): the arc phase resolves W's nodes, a small part
    /// of a big tree's entries. Every wanted entry must be in the metadata.
    pub fn load_for(dir: &std::path::Path, last: u32, wanted: &[(u32, u32)]) -> Result<Self> {
        let (shapes, mut entries, space) = Self::read(dir, last, |r, e| wanted.binary_search(&(r, e)).is_ok())?;
        entries.sort_unstable();
        entries.dedup_by_key(|x| x.0);
        anyhow::ensure!(entries.len() == wanted.len(), "{}: {} of {} wanted entries in the metadata", dir.display(), entries.len(), wanted.len());
        Ok(Resolver { geo: super::GEOMETRY, shapes, keys: Keys::Sparse(entries), space })
    }

    /// The shapes, the entries `keep` keeps and the key space, frame by frame.
    #[allow(clippy::type_complexity)]
    fn read(dir: &std::path::Path, last: u32, keep: impl Fn(u32, u32) -> bool) -> Result<(Vec<u64>, Vec<((u32, u32), Key)>, celeste_engine::exact::KeySpace)> {
        let mut shapes: Vec<(u32, u64)> = Vec::new();
        let mut entries: Vec<((u32, u32), Key)> = Vec::new();
        let mut adds = celeste_engine::exact::KeyAdditions::default();
        for f in 0..=last {
            if !dir.join("frames").join(format!("f{f:03}")).is_dir() {
                continue;
            }
            for m in super::meta::load_frame(dir, f).with_context(|| format!("{}: frame {f}'s storage metadata", dir.display()))? {
                shapes.extend(m.shapes);
                entries.extend(m.entries.into_iter().filter(|&(r, e, _)| keep(r, e)).map(|(r, e, k)| ((r, e), k)));
                adds.extend(m.keys);
            }
        }
        let space = celeste_engine::exact::KeySpace::from_additions(adds).with_context(|| format!("{}: the key space", dir.display()))?;
        shapes.sort_unstable();
        anyhow::ensure!(shapes.iter().enumerate().all(|(i, s)| s.0 as usize == i), "{}: the shapes are not numbered 0..{}", dir.display(), shapes.len());
        Ok((shapes.into_iter().map(|s| s.1).collect(), entries, space))
    }

    /// The tree's shapes by index.
    pub fn shapes(&self) -> &[u64] {
        &self.shapes
    }

    /// `id` as a state (`Sk`: region, key, cell).
    pub fn sk(&self, id: StateId) -> Result<super::Sk> {
        Ok(super::Sk { region: id_region(id), key: self.resolve(id)?.1, local: super::id_local(id) as u8 })
    }

    /// A state's id (its region's entry holding its key; a search of the
    /// region's entries: diagnostics).
    pub fn id_of(&self, s: super::Sk) -> Result<StateId> {
        let Keys::Dense(keys) = &self.keys else { anyhow::bail!("id_of needs every entry's key (Resolver::load)") };
        let e = keys.get(&s.region).and_then(|v| v.iter().position(|&k| k == s.key)).with_context(|| format!("state {}: no entry", s.show()))?;
        Ok(super::state_id(s.region, e as u32, s.local as u32))
    }

    /// `id`'s `(shape, key, cell)`.
    pub fn resolve(&self, id: StateId) -> Result<(u64, Key, u32)> {
        let (r, e) = (id_region(id), id_entry(id));
        let shape = *self.shapes.get((r / self.geo.slots) as usize).with_context(|| format!("state {}: no shape", super::show_id(id)))?;
        let key = match &self.keys {
            Keys::Dense(keys) => keys.get(&r).and_then(|v| v.get(e as usize)).copied(),
            Keys::Sparse(keys) => keys.binary_search_by_key(&(r, e), |x| x.0).ok().map(|i| keys[i].1),
        };
        Ok((shape, key.with_context(|| format!("state {}: no entry", super::show_id(id)))?, super::id_cell(&self.geo, id)))
    }
}

impl Resolver {
    /// `f(shape, key)` of every entry, looked up by id like `resolve`: a
    /// per-entry value computed once, not per state or edge (`ckhash`'s
    /// content hashes).
    pub fn per_entry(&self, f: impl Fn(u64, Key) -> u64) -> PerEntry {
        let Keys::Dense(keys) = &self.keys else { panic!("per_entry needs every entry's key (Resolver::load)") };
        let vals = keys
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
