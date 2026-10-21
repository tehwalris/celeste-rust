//! THE VISITED SET: every state the forward has reached, PACKED tables
//! (`set::RegionSet`; a state's layer and hull, none for a full hull: it
//! never grows nor is pushed again) with the shapes it numbered and its key
//! space. READ-ONLY during a wave (the seal walks it: `set::Cursor`); the
//! frame's end merges in its new states and grown hulls (`absorb`).

use crate::search::hull::HullOf;
use anyhow::Result;
use rustc_hash::FxHashMap;

use super::set::{Block, Change, PackedTable, PlainTable, RegionSet, Upserted};
use super::region_of;
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;

pub(crate) use super::set::StateInfo;
/// A state's EXACT packed key within its shape (`celeste_engine::exact`).
pub(crate) use celeste_engine::exact::Key;
use celeste_engine::exact::KeySpace;

/// Blocks a run of an `upsert_all` takes (a big region's runs go apart, on
/// every thread: a big room's biggest region holds ~2000 blocks).
const RUN_BLOCKS: usize = 64;

/// The shapes (index -> hash, in the order the tree numbered them), the
/// states (by `storage::region_of`), the key space.
#[derive(Clone)]
pub struct VisitedSet<N: Rem = P8> {
    pub keys: KeySpace<N>,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    set: RegionSet<PackedTable>,
}

impl<N: Rem> VisitedSet<N> {
    pub(crate) fn new() -> Self {
        VisitedSet { keys: KeySpace::new(), shapes: Vec::new(), shape_idx: FxHashMap::default(), set: RegionSet::default() }
    }

    /// Forget every state, keeping the key space and the shapes
    /// (`wave::one_frame`: a seed's successor equal to the seed is new).
    pub(crate) fn forget_states(&mut self) {
        self.set = RegionSet::default();
    }

    pub(crate) fn shape_index(&self, shape: u64) -> Option<u32> {
        self.shape_idx.get(&shape).copied()
    }

    pub(crate) fn shape_hash(&self, idx: u32) -> u64 {
        self.shapes[idx as usize]
    }

    pub fn shapes(&self) -> &[u64] {
        &self.shapes
    }

    /// Number `shape` (the next index) unless it has one; its index.
    pub(crate) fn add_shape(&mut self, shape: u64) -> u32 {
        if let Some(&i) = self.shape_idx.get(&shape) {
            return i;
        }
        let i = self.shapes.len() as u32;
        self.shapes.push(shape);
        self.shape_idx.insert(shape, i);
        i
    }

    /// A shape's index from the tree's record (a resume): it must be the next.
    pub(crate) fn restore_shape(&mut self, idx: u32, shape: u64) -> Result<()> {
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
    pub(crate) fn region(&self, shape: u64, slot: u32) -> Option<u32> {
        self.shape_index(shape).map(|s| region_of(s, slot))
    }

    /// Region `region`'s packed table, if any state is held there.
    #[inline]
    pub(crate) fn table(&self, region: u32) -> Option<&PackedTable> {
        self.set.table(region)
    }

    /// State `(region, key, local)`'s layer and hull (`None`: full, or absent).
    pub(crate) fn state(&self, region: u32, key: Key, local: u32) -> Option<StateInfo<N>> {
        self.table(region)?.lookup_info(key, local)?.1
    }

    /// UPSERT per region its changes (`set::Change`, sorted by key and cell,
    /// distinct; regions of numbered shapes made where absent), a big
    /// region's runs of `RUN_BLOCKS` blocks apart, the biggest first.
    pub(crate) fn upsert_all(&mut self, changes: Vec<(u32, Vec<Change<N>>)>, threads: usize) -> Result<Upserted> {
        let regions: Vec<u32> = changes.iter().map(|c| c.0).collect();
        let range = |i: usize, lo: Option<Key>, hi: Option<Key>| {
            let ch = &changes[i].1;
            lo.map_or(0, |k| ch.partition_point(|c| c.key < k))..hi.map_or(ch.len(), |k| ch.partition_point(|c| c.key < k))
        };
        self.upsert_with(&regions, threads, |i, lo, hi| range(i, lo, hi).len(), |i, lo, hi| Ok(std::borrow::Cow::Borrowed(&changes[i].1[range(i, lo, hi)])))
    }

    /// `upsert_all` with each run's changes MADE by the run, not all held at
    /// once: `make(i, lo, hi)` is region `regions[i]`'s with keys in `[lo,
    /// hi)` (`None`: unbounded), `count` their number (the runs' order).
    pub(crate) fn upsert_with<'c>(
        &mut self,
        regions: &[u32],
        threads: usize,
        count: impl Fn(usize, Option<Key>, Option<Key>) -> usize,
        make: impl Fn(usize, Option<Key>, Option<Key>) -> Result<std::borrow::Cow<'c, [Change<N>]>> + Sync,
    ) -> Result<Upserted> {
        // A run: its region's index in `regions`, its place among the
        // region's runs, its keys' range, its blocks.
        type Run = (usize, usize, (Option<Key>, Option<Key>), Vec<Block>);
        let mut runs: Vec<(usize, Run)> = Vec::new();
        for (i, region) in regions.iter().enumerate() {
            anyhow::ensure!(((region / crate::storage::cells::SLOTS) as usize) < self.shapes.len(), "an upserted region {region} of no numbered shape");
            let blocks = self.set.table_mut(*region).take_blocks();
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
                runs.push((count(i, None, None), (i, 0, (None, None), Vec::new())));
                continue;
            }
            let firsts: Vec<Option<Key>> = std::iter::once(None).chain(groups.iter().skip(1).map(|g| Some(g[0].first()))).chain(std::iter::once(None)).collect();
            for (k, g) in groups.into_iter().enumerate() {
                let (lo, hi) = (firsts[k], firsts[k + 1]);
                runs.push((count(i, lo, hi) + g.len() * super::set::PACK_ENTRIES, (i, k, (lo, hi), g)));
            }
        }
        runs.sort_by_key(|r| std::cmp::Reverse(r.0));
        let work: Vec<std::sync::Mutex<Option<Run>>> = runs.into_iter().map(|r| std::sync::Mutex::new(Some(r.1))).collect();
        let done = super::wave::par_map(&work, threads, |w| -> Result<(usize, usize, Vec<Block>, Upserted)> {
            let (i, k, (lo, hi), blocks) = w.lock().expect("a run").take().expect("a run taken once");
            let changes = make(i, lo, hi)?;
            let (out, n) = super::set::upsert_blocks(blocks, &changes)?;
            Ok((i, k, out, n))
        });
        // Each region's runs back in order.
        let mut parts: Vec<Vec<(usize, Vec<Block>)>> = (0..regions.len()).map(|_| Vec::new()).collect();
        let mut total = Upserted::default();
        for d in done {
            let (i, k, out, n) = d?;
            parts[i].push((k, out));
            total += n;
        }
        for (&region, mut p) in regions.iter().zip(parts) {
            p.sort_by_key(|x| x.0);
            *self.set.table_mut(region) = PackedTable::of_blocks(p.into_iter().flat_map(|x| x.1).collect())?;
        }
        Ok(total)
    }

    /// The states with a layer and hull stored (not full).
    pub(crate) fn infos(&self) -> u64 {
        self.set.tables().iter().map(|t| t.1.infos()).sum()
    }

    /// A FRAME's states in: per region (each once) its NEW entries (a plain
    /// table, of a numbered shape) with their hulls in its state order, at
    /// layer `layer`, and its grown states (held already; sorted) with their
    /// new hulls. Returns what it did (`held_new`: 0, the new are new).
    #[allow(clippy::type_complexity)]
    pub(crate) fn absorb(&mut self, parts: &[(u32, &PlainTable, &[HullOf<N>], &[super::wave::Old<N>])], layer: u16, threads: usize) -> Result<Upserted> {
        // Per region its new entries' first states (their hulls' places).
        let starts: Vec<Vec<u32>> = super::wave::par_map(parts, threads, |&(region, new, hulls, _)| -> Result<Vec<u32>> {
            let mut at = Vec::with_capacity(new.len() + 1);
            let mut n = 0u32;
            for m in new.masks() {
                at.push(n);
                n += m.count_ones();
            }
            at.push(n);
            anyhow::ensure!(n as usize == hulls.len(), "region {region}: {n} new states, {} hulls", hulls.len());
            Ok(at)
        })
        .into_iter()
        .collect::<Result<_>>()?;
        // A run's entries and grown states: those with keys in [lo, hi).
        let at = |i: usize, lo: Option<Key>, hi: Option<Key>| {
            let (_, new, _, grown) = parts[i];
            let e = |k: Option<Key>, end: usize| k.map_or(end, |k| new.lower_bound(k));
            let g = |k: Option<Key>, end: usize| k.map_or(end, |k| grown.partition_point(|o| o.sk.key < k));
            (e(lo, 0)..e(hi, new.len()), g(lo, 0)..g(hi, grown.len()))
        };
        let regions: Vec<u32> = parts.iter().map(|p| p.0).collect();
        // The new states and the grown ones, merged by key and cell (each
        // run makes its own: all held at once they take 32 B a state).
        self.upsert_with(
            &regions,
            threads,
            |i, lo, hi| {
                let (e, g) = at(i, lo, hi);
                (starts[i][e.end] - starts[i][e.start]) as usize + g.len()
            },
            |i, lo, hi| -> Result<std::borrow::Cow<'_, [Change<N>]>> {
                let (region, new, hulls, grown) = parts[i];
                let (e, g) = at(i, lo, hi);
                let mut fresh: Vec<Change<N>> = Vec::with_capacity((starts[i][e.end] - starts[i][e.start]) as usize);
                let mut h = hulls[starts[i][e.start] as usize..starts[i][e.end] as usize].iter();
                for x in e {
                    let (key, mask) = (new.key(x as u32), new.mask(x as u32));
                    for cell in super::layer::bits(mask) {
                        let hull = *h.next().expect("counted above");
                        fresh.push(Change { key, cell: cell as u8, info: Some(StateInfo { hull, layer }), new: true });
                    }
                }
                let mut out: Vec<Change<N>> = Vec::with_capacity(fresh.len() + g.len());
                let mut g = grown[g].iter().map(|o| Change { key: o.sk.key, cell: o.sk.local, info: Some(StateInfo { hull: o.hull, layer: o.layer }), new: false }).peekable();
                for c in fresh {
                    while g.peek().is_some_and(|x| (x.key, x.cell) < (c.key, c.cell)) {
                        out.push(g.next().expect("peeked"));
                    }
                    anyhow::ensure!(g.peek().is_none_or(|x| (x.key, x.cell) != (c.key, c.cell)), "region {region}: a state new and grown at once");
                    out.push(c);
                }
                out.extend(g);
                anyhow::ensure!(out.windows(2).all(|w| (w[0].key, w[0].cell) < (w[1].key, w[1].cell)), "region {region}: grown states not by key and cell");
                Ok(std::borrow::Cow::Owned(out))
            },
        )
    }

    pub(crate) fn len(&self) -> usize {
        self.set.states() as usize
    }

    pub fn entries(&self) -> usize {
        self.set.entries()
    }

    pub fn alloc_bytes(&self) -> usize {
        self.set.alloc_bytes()
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::hull::Hull;

    /// A region big enough for several runs of blocks, upserted twice with
    /// changes in some runs only: every state found again with its info,
    /// the blocks in key order (`PackedTable::of_blocks` checks it).
    #[test]
    fn a_big_region_upserts_run_by_run() {
        let mut v = VisitedSet::new();
        v.add_shape(7);
        let region = 3;
        let info = |x: u64| Some(StateInfo { hull: Hull::from_bytes(&(x & 0x7fff_7fff_7fff_7fff).to_le_bytes()), layer: (x % 100) as u16 });
        // 300 blocks' worth of entries, one cell each.
        let n = 300 * crate::storage::set::PACK_ENTRIES as u64;
        let first: Vec<Change> = (0..n).map(|k| Change { key: (0, k * 2), cell: (k % 64) as u8, info: info(k), new: true }).collect();
        let got = v.upsert_all(vec![(region, first)], 8).unwrap();
        assert_eq!((got.added, got.held_new), (n, 0));
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
        assert_eq!((got.added, got.held_new), (3, 0), "(0, 2) cell 1 held: an update");
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

    /// `absorb` (each run making its own changes) leaves the visited set as
    /// the changes made at once and upserted do: several runs, new keys and
    /// grown states in every run, before the first key and past the last.
    #[test]
    fn absorb_is_the_upsert_of_the_frames_changes() {
        let region = 3;
        let hull = |x: u64| Hull::from_bytes(&(x.wrapping_mul(0x9e37_79b9_7f4a_7c15) & 0x7fff_7fff_7fff_7fff).to_le_bytes());
        let n = 300 * crate::storage::set::PACK_ENTRIES as u64;
        let held: Vec<Change> = (0..n).map(|k| Change { key: (0, 4 + k * 4), cell: (k % 64) as u8, info: Some(StateInfo { hull: hull(k), layer: (k % 7) as u16 }), new: true }).collect();
        let fresh = || {
            let mut v = VisitedSet::new();
            v.add_shape(7);
            v.upsert_all(vec![(region, held.clone())], 8).unwrap();
            v
        };
        // The frame's new entries (keys between the held ones, before the
        // first and past the last; 1-3 cells), and grown held states.
        let new_keys: Vec<(u64, u64)> = std::iter::once((0, 1)).chain((0..n).step_by(97).map(|k| (0, 6 + k * 4))).chain(std::iter::once((0, 8 + n * 4))).collect();
        let masks: Vec<u64> = new_keys.iter().map(|k| 0b101u64 << (k.1 % 60) | (k.1 % 3 == 0) as u64).collect();
        let table = PlainTable::from_sorted(&new_keys, &masks).unwrap();
        let hulls: Vec<Hull> = (0..table.states()).map(|i| hull(1000 + i)).collect();
        let grown: Vec<super::super::wave::Old> = (0..n)
            .step_by(53)
            .map(|k| super::super::wave::Old { sk: super::super::Sk { region, key: (0, 4 + k * 4), local: (k % 64) as u8 }, layer: (k % 7) as u16, hull: hull(k + 7), was: hull(k) })
            .collect();
        let layer = 40;
        let mut a = fresh();
        let got = a.absorb(&[(region, &table, &hulls, &grown)], layer, 8).unwrap();
        // The same changes, made at once.
        let mut changes: Vec<Change> = Vec::new();
        let mut h = hulls.iter();
        for (key, mask) in table.iter() {
            for cell in crate::storage::layer::bits(mask) {
                changes.push(Change { key, cell: cell as u8, info: Some(StateInfo { hull: *h.next().unwrap(), layer }), new: true });
            }
        }
        changes.extend(grown.iter().map(|o| Change { key: o.sk.key, cell: o.sk.local, info: Some(StateInfo { hull: o.hull, layer: o.layer }), new: false }));
        changes.sort_by_key(|c| (c.key, c.cell));
        let mut b = fresh();
        let want = b.upsert_all(vec![(region, changes.clone())], 8).unwrap();
        assert_eq!((got.added, got.held_new), (want.added, want.held_new));
        assert_eq!((a.len(), a.entries()), (b.len(), b.entries()));
        for c in changes.iter().chain(held.iter().step_by(1013)) {
            assert_eq!(a.state(region, c.key, c.cell as u32), b.state(region, c.key, c.cell as u32), "state {:?} cell {}", c.key, c.cell);
        }
        assert!(a.table(region).unwrap().blocks().len() > 2 * RUN_BLOCKS, "several runs");
    }
}
