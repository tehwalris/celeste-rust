//! THE VISITED SET: every state the forward has reached - a `set::RegionSet`
//! of PACKED tables (per (shape, region) its entries - a position-free key
//! and a mask over the region's cells - sorted, in compressed blocks, each
//! state with its LAYER and remainder HULL: `set::StateInfo`, none for a
//! state whose hull is the whole torus - it never grows, nor is it pushed
//! again), with the shapes it numbered and the key space its keys are in.
//! READ-ONLY during a wave: a frame's seal walks a region with the sorted
//! keys it touched (`set::Cursor`), and the frame's end merges its new
//! states and the hulls that grew in (`absorb`: `set::RegionTable::upsert`,
//! a big region's runs of blocks apart).

use anyhow::Result;
use rustc_hash::FxHashMap;

use super::set::{Block, Change, RegionSet, RegionTable, Upserted};
use super::{region_of, Geometry};
use crate::search::hull::Hull;

pub use super::set::StateInfo;
/// A state's EXACT packed key within its shape (`celeste_engine::exact`).
pub use celeste_engine::exact::Key;
use celeste_engine::exact::KeySpace;

/// Blocks a region's run of an `upsert_all` takes (a big region's runs are
/// upserted apart, on every thread: a big room's biggest region holds
/// ~2000 blocks).
const RUN_BLOCKS: usize = 64;

/// The visited set: the shapes (index -> hash, in the order the tree
/// numbered them), the states (`RegionSet` of packed tables, by
/// `storage::region_of`), the key space their keys are in (read-only during
/// a wave).
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

    /// Region `region`'s packed table, if any state is held there.
    #[inline]
    pub fn table(&self, region: u32) -> Option<&RegionTable> {
        self.set.table(region)
    }

    /// The layer and hull of state `(region, key, local)` (`None`: full,
    /// or no state; a reader's single lookup).
    pub fn state(&self, region: u32, key: Key, local: u32) -> Option<StateInfo> {
        self.table(region)?.lookup_info(key, local)?.1
    }

    /// UPSERT per region its changes (`set::Change`, sorted by key and
    /// cell, distinct): regions made where absent (of numbered shapes); a
    /// big region's runs of `RUN_BLOCKS` blocks apart, with the changes
    /// between their first keys; every run on `threads` threads, the
    /// biggest first.
    pub fn upsert_all(&mut self, changes: Vec<(u32, Vec<Change>)>, threads: usize) -> Result<Upserted> {
        // A run: its region's index in `changes`, its place among the
        // region's runs, its changes, its blocks.
        type Run = (usize, usize, std::ops::Range<usize>, Vec<Block>);
        let mut runs: Vec<Run> = Vec::new();
        let mut regions: Vec<u32> = Vec::with_capacity(changes.len());
        for (i, (region, ch)) in changes.iter().enumerate() {
            anyhow::ensure!(((region / self.geo.slots) as usize) < self.shapes.len(), "an upserted region {region} of no numbered shape");
            regions.push(*region);
            let blocks = self.set.table_mut(*region, RegionTable::packed).take_blocks();
            // Runs of blocks, each with the changes from its first key to
            // the next run's (the first from the start, the last to the end).
            let groups: Vec<Vec<Block>> = {
                let mut g: Vec<Vec<Block>> = Vec::new();
                for b in blocks {
                    if g.last().is_none_or(|x| x.len() == RUN_BLOCKS) {
                        g.push(Vec::with_capacity(RUN_BLOCKS));
                    }
                    g.last_mut().expect("pushed").push(b);
                }
                g
            };
            if groups.is_empty() {
                runs.push((i, 0, 0..ch.len(), Vec::new()));
                continue;
            }
            let starts: Vec<usize> = std::iter::once(0)
                .chain(groups.iter().skip(1).map(|g| ch.partition_point(|c| c.key < g[0].first())))
                .chain(std::iter::once(ch.len()))
                .collect();
            for (k, g) in groups.into_iter().enumerate() {
                runs.push((i, k, starts[k]..starts[k + 1], g));
            }
        }
        runs.sort_by_cached_key(|r| std::cmp::Reverse(r.2.len() + r.3.len() * super::set::PACK_ENTRIES));
        let work: Vec<std::sync::Mutex<Option<Run>>> = runs.into_iter().map(|r| std::sync::Mutex::new(Some(r))).collect();
        let done = super::wave::par_map(&work, threads, |w| -> Result<(usize, usize, Vec<Block>, Upserted)> {
            let (i, k, rg, blocks) = w.lock().expect("a run").take().expect("a run taken once");
            let (out, n) = super::set::upsert_blocks(blocks, &changes[i].1[rg])?;
            Ok((i, k, out, n))
        });
        // Each region's runs back in order.
        let mut parts: Vec<Vec<(usize, Vec<Block>)>> = (0..changes.len()).map(|_| Vec::new()).collect();
        let mut total = Upserted::default();
        for d in done {
            let (i, k, out, n) = d?;
            parts[i].push((k, out));
            total += n;
        }
        for (region, mut p) in regions.into_iter().zip(parts) {
            p.sort_by_key(|x| x.0);
            *self.set.table_mut(region, RegionTable::packed) = RegionTable::of_blocks(p.into_iter().flat_map(|x| x.1).collect())?;
        }
        Ok(total)
    }

    /// The states with a layer and hull stored (not full).
    pub fn infos(&self) -> u64 {
        self.set.tables().iter().map(|t| t.1.infos()).sum()
    }

    /// A FRAME's states in, on `threads` threads: per region (each once)
    /// its NEW entries (a plain table, of a numbered shape) with their
    /// hulls in its state order - in at layer `layer` - and the states
    /// whose hull grew (held already; sorted by state) with their new
    /// hulls. Returns the new states held already (0: the frame's new
    /// states are new).
    #[allow(clippy::type_complexity)]
    pub fn absorb(&mut self, parts: &[(u32, &RegionTable, &[Hull], &[super::wave::Old])], layer: u16, threads: usize) -> Result<u64> {
        let changes: Vec<(u32, Vec<Change>)> = super::wave::par_map(parts, threads, |&(region, new, hulls, grown)| -> Result<(u32, Vec<Change>)> {
            anyhow::ensure!(new.states() == hulls.len() as u64, "region {region}: {} new states, {} hulls", new.states(), hulls.len());
            // The new states and the grown ones, merged by key and cell.
            let mut fresh: Vec<Change> = Vec::with_capacity(hulls.len());
            let mut h = hulls.iter();
            for (key, mask) in new.iter() {
                for cell in super::layer::bits(mask) {
                    let hull = *h.next().expect("counted above");
                    fresh.push(Change { key, cell: cell as u8, info: Some(StateInfo { hull, layer }), new: true });
                }
            }
            let mut out: Vec<Change> = Vec::with_capacity(fresh.len() + grown.len());
            let mut g = grown.iter().map(|o| Change { key: o.sk.key, cell: o.sk.local, info: Some(StateInfo { hull: o.hull, layer: o.layer }), new: false }).peekable();
            for c in fresh {
                while g.peek().is_some_and(|x| (x.key, x.cell) < (c.key, c.cell)) {
                    out.push(g.next().expect("peeked"));
                }
                anyhow::ensure!(g.peek().is_none_or(|x| (x.key, x.cell) != (c.key, c.cell)), "region {region}: a state new and grown at once");
                out.push(c);
            }
            out.extend(g);
            anyhow::ensure!(out.windows(2).all(|w| (w[0].key, w[0].cell) < (w[1].key, w[1].cell)), "region {region}: grown states not by key and cell");
            Ok((region, out))
        })
        .into_iter()
        .collect::<Result<_>>()?;
        Ok(self.upsert_all(changes, threads)?.held_new)
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

    /// Bytes allocated.
    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes()
    }

    /// The stored hulls' and layers' statistics (`[memory]`, a diagnostic):
    /// states with a stored hull, distinct hulls (counted up to `cap`), the
    /// states the most common 256, 4096 and 65536 hulls cover, distinct
    /// layers, and the entries whose stored states share one layer (of the
    /// entries with any).
    pub fn hull_stats(&self, cap: usize) -> Result<String> {
        let mut by: FxHashMap<u64, u64> = FxHashMap::default();
        let (mut states, mut over, mut one_layer, mut entries) = (0u64, false, 0u64, 0u64);
        let mut layers = vec![0u64; 1 << 16];
        for (_, t) in self.set.tables() {
            t.for_each_packed(|_, _, _, run| {
                if run.is_empty() {
                    return;
                }
                entries += 1;
                one_layer += run.iter().all(|i| i.layer == run[0].layer) as u64;
                for i in run {
                    states += 1;
                    layers[i.layer as usize] += 1;
                    // Mixed: FxHash leaves a word's high half out of the low
                    // bits a table indexes by (the y arc).
                    let w = celeste_engine::runtime2::mix64(i.hull.word());
                    if by.len() < cap || by.contains_key(&w) {
                        *by.entry(w).or_default() += 1;
                    } else {
                        over = true;
                    }
                }
            })?;
        }
        let mut counts: Vec<u64> = by.values().copied().collect();
        counts.sort_unstable_by(|a, b| b.cmp(a));
        let top = |k: usize| counts.iter().take(k).sum::<u64>() as f64 / states.max(1) as f64 * 100.0;
        Ok(format!(
            "{states} stored hulls: {}{} distinct, the top 256 cover {:.1}%, 4096 {:.1}%, 65536 {:.1}%; {} distinct layers; {one_layer} of {entries} entries hold one layer",
            if over { ">" } else { "" },
            by.len(),
            top(256),
            top(4096),
            top(65536),
            layers.iter().filter(|&&c| c > 0).count()
        ))
    }

    /// Its bytes by part (`[memory]`): the packed blocks (keys, masks,
    /// layers and hulls, zstd), the block and region lists, the key space.
    pub fn census(&self) -> Vec<(&'static str, usize)> {
        let s = self.set.bytes();
        vec![("visited: packed blocks (keys, cells, layers, hulls)", s[0]), ("visited: block and region lists", s[1] + s[2]), ("key space", self.keys.alloc_bytes())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::hull::Hull;

    /// A region big enough for several runs of blocks, upserted twice with
    /// changes in some runs only: every state found again with its info,
    /// the blocks in key order (`RegionTable::of_blocks` checks it).
    #[test]
    fn a_big_region_upserts_run_by_run() {
        let mut v = VisitedSet::new(super::super::GEOMETRY);
        v.add_shape(7);
        let region = 3;
        let info = |x: u64| Some(StateInfo { hull: Hull::from_bytes(&(x & 0x7fff_7fff_7fff_7fff).to_le_bytes()), layer: (x % 100) as u16 });
        // 300 blocks' worth of entries, one cell each.
        let n = 300 * crate::storage::set::PACK_ENTRIES as u64;
        let first: Vec<Change> = (0..n).map(|k| Change { key: (0, k * 2), cell: (k % 64) as u8, info: info(k), new: true }).collect();
        assert_eq!(v.upsert_all(vec![(region, first)], 8).unwrap(), Upserted { added: n, held_new: 0 });
        assert!(v.table(region).unwrap().blocks().len() > 2 * RUN_BLOCKS);
        // Changes in the first and the last run only: an update, a new
        // cell, a new key between two held ones, a key past the end.
        let mut second = vec![
            Change { key: (0, 2), cell: 1, info: info(999), new: false },
            Change { key: (0, 3), cell: 5, info: None, new: true },
            Change { key: (0, 2 * (n - 1)), cell: 62, info: info(5), new: true },
            Change { key: (0, 2 * n + 10), cell: 0, info: info(6), new: true },
        ];
        second.sort_by_key(|c| (c.key, c.cell));
        let got = v.upsert_all(vec![(region, second)], 8).unwrap();
        assert_eq!(got, Upserted { added: 3, held_new: 0 }, "(0, 2) cell 1 held: an update");
        assert_eq!(v.len() as u64, n + 3);
        assert_eq!(v.state(region, (0, 2 * (n - 1)), 62), info(5));
        assert_eq!(v.state(region, (0, 2), 1), info(999));
        assert_eq!(v.state(region, (0, 3), 5), None, "a full hull: no info");
        assert_eq!(v.table(region).unwrap().mask_of((0, 3)), 1 << 5);
        assert_eq!(v.state(region, (0, 2 * n + 10), 0), info(6));
        for k in (0..n).step_by(4999) {
            assert_eq!(v.state(region, (0, k * 2), (k % 64) as u32), info(k));
        }
    }
}
