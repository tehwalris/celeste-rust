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
pub struct VisitedSet<N: Rem> {
    pub keys: KeySpace<N>,
    shapes: Vec<u64>,
    shape_idx: FxHashMap<u64, u32>,
    set: RegionSet<PackedTable>,
}

/// One part of what `VisitedSet::absorb` takes in: a region's new entries
/// (their hulls in the table's state order) at a layer, and its grown
/// states (`Old`s or `Grown`s, by key and cell).
pub(crate) struct Part<'a, N: Rem, G> {
    pub(crate) region: u32,
    pub(crate) new: &'a PlainTable,
    pub(crate) hulls: &'a [HullOf<N>],
    pub(crate) grown: &'a [G],
    pub(crate) layer: u16,
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
    /// region's runs of `RUN_BLOCKS` blocks apart, the biggest first:
    /// `absorb`'s oracle.
    #[cfg(test)]
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

    /// FRAMES' states in (the forward's frame end; a resume's batch of frame
    /// files): `parts`, by region, a region's in frame order - each its NEW
    /// entries (a plain table, of a numbered shape) with their hulls in its
    /// state order, at its `layer`, and its GROWN states (held already, or
    /// new in an earlier part; sorted) with their new hulls. A state's last
    /// part decides its layer and hull. Returns what it did (`held_new`: 0,
    /// the new are new).
    pub(crate) fn absorb<G: Copy + Into<super::hulls::Grown<N>> + Sync>(&mut self, parts: &[Part<'_, N, G>], threads: usize) -> Result<Upserted> {
        // Per part its new entries' first states (their hulls' places).
        let starts: Vec<Vec<u32>> = super::wave::par_map(parts, threads, |p| -> Result<Vec<u32>> {
            let mut at = Vec::with_capacity(p.new.len() + 1);
            let mut n = 0u32;
            for m in p.new.masks() {
                at.push(n);
                n += m.count_ones();
            }
            at.push(n);
            anyhow::ensure!(n as usize == p.hulls.len(), "region {}: {n} new states, {} hulls", p.region, p.hulls.len());
            Ok(at)
        })
        .into_iter()
        .collect::<Result<_>>()?;
        // The regions, each its parts.
        let mut regions: Vec<(u32, std::ops::Range<usize>)> = Vec::new();
        for (k, p) in parts.iter().enumerate() {
            match regions.last_mut() {
                Some((r, ks)) if *r == p.region => ks.end = k + 1,
                _ => regions.push((p.region, k..k + 1)),
            }
        }
        anyhow::ensure!(regions.windows(2).all(|w| w[0].0 < w[1].0), "absorbed parts not by region");
        // A part's entries and grown states in a run: keys in [lo, hi).
        let at = |k: usize, lo: Option<Key>, hi: Option<Key>| {
            let p = &parts[k];
            let e = |key: Option<Key>, end: usize| key.map_or(end, |key| p.new.lower_bound(key));
            let g = |key: Option<Key>, end: usize| key.map_or(end, |key| p.grown.partition_point(|&o| o.into().sk.key < key));
            (e(lo, 0)..e(hi, p.new.len()), g(lo, 0)..g(hi, p.grown.len()))
        };
        // A part's new states in a run, as changes (by key and cell), into
        // `out`; and its grown ones.
        let fresh = |k: usize, e: std::ops::Range<usize>, out: &mut Vec<Change<N>>| {
            let p = &parts[k];
            let mut h = p.hulls[starts[k][e.start] as usize..starts[k][e.end] as usize].iter();
            for x in e {
                let (key, mask) = (p.new.key(x as u32), p.new.mask(x as u32));
                for cell in super::layer::bits(mask) {
                    let hull = *h.next().expect("counted above");
                    out.push(Change { key, cell: cell as u8, info: Some(StateInfo { hull, layer: p.layer }), new: true });
                }
            }
        };
        let grown = |k: usize, g: std::ops::Range<usize>| {
            parts[k].grown[g].iter().map(|&o| {
                let o: super::hulls::Grown<N> = o.into();
                Change { key: o.sk.key, cell: o.sk.local, info: Some(StateInfo { hull: o.hull, layer: o.layer }), new: false }
            })
        };
        // A part's changes in a run: its new states and its grown ones,
        // merged by key and cell.
        let changes = |k: usize, lo: Option<Key>, hi: Option<Key>| -> Result<Vec<Change<N>>> {
            let p = &parts[k];
            let (e, g) = at(k, lo, hi);
            let mut new: Vec<Change<N>> = Vec::with_capacity((starts[k][e.end] - starts[k][e.start]) as usize);
            fresh(k, e, &mut new);
            let mut out: Vec<Change<N>> = Vec::with_capacity(new.len() + g.len());
            let mut g = grown(k, g).peekable();
            for c in new {
                while g.peek().is_some_and(|x| (x.key, x.cell) < (c.key, c.cell)) {
                    out.push(g.next().expect("peeked"));
                }
                anyhow::ensure!(g.peek().is_none_or(|x| (x.key, x.cell) != (c.key, c.cell)), "region {}: a state new and grown at once", p.region);
                out.push(c);
            }
            out.extend(g);
            anyhow::ensure!(out.windows(2).all(|w| (w[0].key, w[0].cell) < (w[1].key, w[1].cell)), "region {}: grown states not by key and cell", p.region);
            Ok(out)
        };
        let ids: Vec<u32> = regions.iter().map(|r| r.0).collect();
        // Each run makes its own changes (all held at once they take 32 B a
        // state); a region's parts' merged by state, the last part's kept
        // (NEW where any was: new twice, the state stored twice).
        self.upsert_with(
            &ids,
            threads,
            |i, lo, hi| {
                regions[i].1.clone().map(|k| {
                    let (e, g) = at(k, lo, hi);
                    (starts[k][e.end] - starts[k][e.start]) as usize + g.len()
                }).sum()
            },
            |i, lo, hi| -> Result<std::borrow::Cow<'_, [Change<N>]>> {
                let ks = regions[i].1.clone();
                if ks.len() == 1 {
                    return Ok(std::borrow::Cow::Owned(changes(ks.start, lo, hi)?));
                }
                // Every part's, then by state (stable: in part order).
                let mut all: Vec<Change<N>> = Vec::new();
                for k in ks {
                    let (e, g) = at(k, lo, hi);
                    fresh(k, e, &mut all);
                    all.extend(grown(k, g));
                }
                all.sort_by_key(|c| (c.key, c.cell));
                let mut out: Vec<Change<N>> = Vec::with_capacity(all.len());
                for c in all {
                    match out.last_mut() {
                        Some(l) if (l.key, l.cell) == (c.key, c.cell) => {
                            anyhow::ensure!(!(l.new && c.new), "region {}: a state stored twice", regions[i].0);
                            *l = Change { new: l.new || c.new, ..c };
                        }
                        _ => out.push(c),
                    }
                }
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
    use crate::search::arcs::Fixed16;
    use celeste_core::arith::Fixed;
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
        let first: Vec<Change<Fixed>> = (0..n).map(|k| Change { key: (0, k * 2), cell: (k % 64) as u8, info: info(k), new: true }).collect();
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
        let held: Vec<Change<Fixed>> = (0..n).map(|k| Change { key: (0, 4 + k * 4), cell: (k % 64) as u8, info: Some(StateInfo { hull: hull(k), layer: (k % 7) as u16 }), new: true }).collect();
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
        let hulls: Vec<Hull<Fixed16>> = (0..table.states()).map(|i| hull(1000 + i)).collect();
        let grown: Vec<super::super::wave::Old<Fixed>> = (0..n)
            .step_by(53)
            .map(|k| super::super::wave::Old { sk: super::super::Sk { region, key: (0, 4 + k * 4), local: (k % 64) as u8 }, layer: (k % 7) as u16, hull: hull(k + 7), was: hull(k) })
            .collect();
        let layer = 40;
        let mut a = fresh();
        let got = a.absorb(&[Part { region, new: &table, hulls: &hulls, grown: &grown, layer }], 8).unwrap();
        // The same changes, made at once.
        let mut changes: Vec<Change<Fixed>> = Vec::new();
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

    /// FRAMES AT ONCE (a resume's batch) as one after the other: a second
    /// frame with new entries of its own, growing held states and the first
    /// frame's new ones, in one `absorb` leaves the set as two do.
    #[test]
    fn absorbing_frames_at_once_is_absorbing_them_in_turn() {
        use crate::search::hull::Hull;
        use crate::storage::hulls::Grown;
        let region = 5;
        let hull = |x: u64| Hull::from_bytes(&(x.wrapping_mul(0x9e37_79b9_7f4a_7c15) & 0x7fff_7fff_7fff_7fff).to_le_bytes());
        let n = 200 * crate::storage::set::PACK_ENTRIES as u64;
        let held: Vec<Change<Fixed>> = (0..n).map(|k| Change { key: (0, 6 + k * 6), cell: (k % 64) as u8, info: Some(StateInfo { hull: hull(k), layer: (k % 5) as u16 }), new: true }).collect();
        let fresh = || {
            let mut v = VisitedSet::new();
            v.add_shape(9);
            v.upsert_all(vec![(region, held.clone())], 8).unwrap();
            v
        };
        let table = |off: u64, step: usize| {
            let keys: Vec<(u64, u64)> = (0..n).step_by(step).map(|k| (0, off + k * 6)).collect();
            let masks: Vec<u64> = keys.iter().map(|k| 0b11u64 << (k.1 % 50)).collect();
            PlainTable::from_sorted(&keys, &masks).unwrap()
        };
        let (t1, t2) = (table(1, 71), table(3, 89));
        let (h1, h2): (Vec<Hull<Fixed16>>, Vec<Hull<Fixed16>>) = ((0..t1.states()).map(|i| hull(500 + i)).collect(), (0..t2.states()).map(|i| hull(900 + i)).collect());
        // Frame 41 grows held states (every 41st) and some of frame 40's new
        // ones (the first cell of every 3rd entry).
        let mut g2: Vec<Grown<Fixed>> = (0..n).step_by(41).map(|k| Grown { sk: super::super::Sk { region, key: (0, 6 + k * 6), local: (k % 64) as u8 }, layer: (k % 5) as u16, hull: hull(k + 3) }).collect();
        g2.extend(t1.iter().step_by(3).map(|(key, mask)| Grown { sk: super::super::Sk { region, key, local: mask.trailing_zeros() as u8 }, layer: 40, hull: hull(key.1 + 11) }));
        g2.sort_unstable_by_key(|g| g.sk);
        let p1 = Part { region, new: &t1, hulls: &h1, grown: &[] as &[Grown<Fixed>], layer: 40 };
        let p2 = Part { region, new: &t2, hulls: &h2, grown: &g2, layer: 41 };
        let mut a = fresh();
        let got = a.absorb(&[p1, p2], 8).unwrap();
        let mut b = fresh();
        let p1 = Part { region, new: &t1, hulls: &h1, grown: &[] as &[Grown<Fixed>], layer: 40 };
        let p2 = Part { region, new: &t2, hulls: &h2, grown: &g2, layer: 41 };
        let first = b.absorb(&[p1], 8).unwrap();
        let second = b.absorb(&[p2], 8).unwrap();
        assert_eq!((got.added, got.held_new), (first.added + second.added, 0));
        assert_eq!((a.len(), a.entries()), (b.len(), b.entries()));
        for (key, mask) in t1.iter().chain(t2.iter()) {
            for c in crate::storage::layer::bits(mask) {
                assert_eq!(a.state(region, key, c), b.state(region, key, c), "state {key:?} cell {c}");
            }
        }
        for c in held.iter().step_by(7) {
            assert_eq!(a.state(region, c.key, c.cell as u32), b.state(region, c.key, c.cell as u32), "state {:?} cell {}", c.key, c.cell);
        }
        // A state new in both: stored twice.
        let p1 = Part { region, new: &t1, hulls: &h1, grown: &[] as &[Grown<Fixed>], layer: 40 };
        let again = Part { region, new: &t1, hulls: &h1, grown: &[] as &[Grown<Fixed>], layer: 41 };
        assert!(fresh().absorb(&[p1, again], 8).is_err(), "a state new twice");
    }
}
