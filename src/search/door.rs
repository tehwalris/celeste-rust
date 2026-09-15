//! The door: the set of every `(shape, cell, key)` the forward has reached,
//! against which emitted rows are deduplicated (plans/waves.md).
//!
//! Sharded by `(shape, cell)` - two rows in different shards can never be
//! the same state - and, within a shard, a SORTED array of 128-bit keys
//! (`base`, everything up to the previous frame, immutable during a
//! frame) plus a small sorted `delta` (this frame's admissions). A flush
//! `admit`s a sorted batch of keys with one merge-join sweep over both;
//! `end_frame` merges every delta into its base, once per frame. So the
//! big set is only READ while a frame runs. Each entry is the key and the
//! state's id (`(layer, file seq, row)`): what an edge to an old state
//! is written as (plans/waves.md, the explicit graph).
//!
//! `HashDoor` is the same contract over a hash set per shard (the
//! pre-2026-09-13 representation), kept as the oracle for `Door`'s tests.

use rustc_hash::FxHashMap;
use std::sync::{Arc, Mutex, RwLock};

pub type Key = (u64, u64);

/// THE SPEED HULL of a key: the union of the player's `spd.x`/`spd.y`
/// intervals (raw 16.16, `[x_lo, x_hi, y_lo, y_hi]`) of every row merged
/// into it. A key is the state with its speed BUCKETED; the rows store
/// the tight fragment. A new row whose fragment lies inside the hull is
/// the old state (its id); one that widens the hull is a NEW row: the
/// hull grows to the union, the key points at the row's fresh id, and
/// the row is expanded with the union as its speed - so everything
/// inside the hull has been expanded by some row containing it (the
/// union, not the row's own fragment: a hull with a gap between two
/// merged fragments would otherwise cover speeds nobody expanded).
/// `NO_HULL` when speeds are exact: never widened, never grows.
pub type Hull = [i32; 4];
pub const NO_HULL: Hull = [i32::MAX, i32::MIN, i32::MAX, i32::MIN];

pub fn hull_contains(outer: &Hull, inner: &Hull) -> bool {
    outer[0] <= inner[0] && inner[1] <= outer[1] && outer[2] <= inner[2] && inner[3] <= outer[3]
}

pub fn hull_union(a: &Hull, b: &Hull) -> Hull {
    [a[0].min(b[0]), a[1].max(b[1]), a[2].min(b[2]), a[3].max(b[3])]
}

/// A shard entry: the key, the state's id `(layer, file seq, row)`
/// packed by `crate::frame::pack_id`, and its speed hull.
pub type Entry = (Key, u64, Hull);

/// The door's contract: admit sorted, deduplicated key batches; merge at
/// the end of a frame.
pub trait Admit: Sync {
    /// `keys` sorted ascending with no duplicates. For each key, its id:
    /// an existing entry's, or - for a key NOT in the shard - a fresh one,
    /// `first_new + k` for the k-th new key in order, which the shard
    /// records. `ids` receives one id per key (in `keys`' order), `new`
    /// the indices of the new keys. With `hulls` (one per key, the rows'
    /// speed hull), an existing key whose hull does not contain the new
    /// one is NEW too (`Hull`): its hull becomes the union, its id the
    /// fresh one; `new_hulls` receives, per new key, the hull the emitted
    /// row is to carry (its own, or the union).
    fn admit(
        &self,
        shape: u64,
        cell: u32,
        keys: &[Key],
        first_new: u64,
        ids: &mut Vec<u64>,
        new: &mut Vec<u32>,
        hulls: Option<&[Hull]>,
        new_hulls: &mut Vec<Hull>,
    );
    /// The frame is over: fold this frame's admissions into the base.
    fn end_frame(&self, workers: usize);
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Bytes allocated for the entries.
    fn alloc_bytes(&self) -> usize;
}

#[derive(Default)]
struct Shard {
    base: Vec<Entry>,
    /// Bucket starts into `base` by the top bits of `key.0` (the keys are
    /// uniform hashes): `index.len() - 1` buckets of ~8 entries, so a
    /// lookup touches one index word and one or two lines of `base`
    /// instead of galloping through it. 0.5 B/entry. Rebuilt with `base`.
    index: Vec<u32>,
    delta: Vec<Entry>,
}

/// log2 of the bucket count for `n` entries: ~8 entries per bucket.
fn index_bits(n: usize) -> u32 {
    (n / 8).max(1).next_power_of_two().trailing_zeros()
}

fn build_index(base: &[Entry]) -> Vec<u32> {
    let bits = index_bits(base.len());
    let nb = 1usize << bits;
    let mut index = vec![0u32; nb + 1];
    let mut b = 0usize;
    for (i, (k, _, _)) in base.iter().enumerate() {
        let kb = if bits == 0 { 0 } else { (k.0 >> (64 - bits)) as usize };
        while b < kb {
            b += 1;
            index[b] = i as u32;
        }
    }
    while b < nb {
        b += 1;
        index[b] = base.len() as u32;
    }
    index
}

impl Shard {
    /// Merge-join `keys` against `base` and `delta`; the misses get fresh
    /// ids and go to `delta`, kept sorted by key.
    fn admit(
        &mut self,
        keys: &[Key],
        first_new: u64,
        ids: &mut Vec<u64>,
        new: &mut Vec<u32>,
        hulls: Option<&[Hull]>,
        new_hulls: &mut Vec<Hull>,
        scratch: &mut Vec<Entry>,
    ) {
        let mut d = 0usize;
        scratch.clear();
        let bits = index_bits(self.base.len());
        const AHEAD: usize = 8;
        for k in keys.iter().take(AHEAD) {
            self.prefetch(bits, k);
        }
        let mut n_new = 0u64;
        // An existing entry: the old id, unless the row's hull escapes
        // the entry's - then the entry takes the union and a fresh id,
        // and the row is new (carrying the union).
        let hit = |e: &mut Entry, i: usize, ids: &mut Vec<u64>, new: &mut Vec<u32>, new_hulls: &mut Vec<Hull>, n_new: &mut u64| {
            if let Some(h) = hulls {
                if !hull_contains(&e.2, &h[i]) {
                    e.2 = hull_union(&e.2, &h[i]);
                    let id = first_new + *n_new;
                    *n_new += 1;
                    e.1 = id;
                    ids.push(id);
                    new.push(i as u32);
                    new_hulls.push(e.2);
                    return;
                }
            }
            ids.push(e.1);
        };
        for (i, k) in keys.iter().enumerate() {
            if let Some(k2) = keys.get(i + AHEAD) {
                self.prefetch(bits, k2);
            }
            if !self.base.is_empty() {
                let kb = if bits == 0 { 0 } else { (k.0 >> (64 - bits)) as usize };
                let (lo, hi) = (self.index[kb] as usize, self.index[kb + 1] as usize);
                let b = lo + self.base[lo..hi].partition_point(|x| x.0 < *k);
                if b < hi && self.base[b].0 == *k {
                    hit(&mut self.base[b], i, ids, new, new_hulls, &mut n_new);
                    continue;
                }
            }
            d = gallop(&self.delta, d, k);
            if d < self.delta.len() && self.delta[d].0 == *k {
                hit(&mut self.delta[d], i, ids, new, new_hulls, &mut n_new);
                continue;
            }
            let id = first_new + n_new;
            n_new += 1;
            ids.push(id);
            new.push(i as u32);
            let h = hulls.map_or(NO_HULL, |h| h[i]);
            new_hulls.push(h);
            scratch.push((*k, id, h));
        }
        if scratch.is_empty() {
            return;
        }
        if self.delta.last().is_some_and(|last| last.0 < scratch[0].0) || self.delta.is_empty() {
            self.delta.extend_from_slice(scratch);
        } else {
            let merged = merge_sorted(&self.delta, scratch);
            self.delta = merged;
        }
    }

    /// Prefetch the index word and the bucket's first line for `k`, so a
    /// sorted batch's lookups overlap their cache misses.
    #[inline]
    fn prefetch(&self, bits: u32, k: &Key) {
        #[cfg(target_arch = "x86_64")]
        unsafe {
            use std::arch::x86_64::{_mm_prefetch, _MM_HINT_T0};
            let kb = if bits == 0 { 0 } else { (k.0 >> (64 - bits)) as usize };
            if kb + 1 < self.index.len() {
                _mm_prefetch(self.index.as_ptr().add(kb) as *const i8, _MM_HINT_T0);
                let lo = *self.index.get_unchecked(kb) as usize;
                if lo < self.base.len() {
                    _mm_prefetch(self.base.as_ptr().add(lo) as *const i8, _MM_HINT_T0);
                }
            }
        }
    }

    fn end_frame(&mut self) {
        if self.delta.is_empty() {
            return;
        }
        if self.base.last().is_some_and(|last| last.0 < self.delta[0].0) || self.base.is_empty() {
            self.base.append(&mut self.delta);
        } else {
            self.base = merge_sorted(&self.base, &self.delta);
            self.delta.clear();
        }
        self.delta.shrink_to_fit();
        self.index = build_index(&self.base);
    }
}

/// The first index `>= from` whose key is `>= k` in the sorted `a`, by
/// exponential then binary search from `from`.
#[inline]
fn gallop(a: &[Entry], from: usize, k: &Key) -> usize {
    let n = a.len();
    if from >= n || a[from].0 >= *k {
        return from;
    }
    let mut step = 1;
    let mut lo = from;
    let mut hi = from + 1;
    while hi < n && a[hi].0 < *k {
        lo = hi;
        step *= 2;
        hi = (hi + step).min(n);
        if hi == n {
            break;
        }
    }
    lo + 1 + a[lo + 1..hi.min(n)].partition_point(|x| x.0 < *k)
}

/// Two sorted, individually duplicate-free, mutually disjoint slices
/// merged into one.
fn merge_sorted(a: &[Entry], b: &[Entry]) -> Vec<Entry> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        if a[i].0 < b[j].0 {
            out.push(a[i]);
            i += 1;
        } else {
            out.push(b[j]);
            j += 1;
        }
    }
    out.extend_from_slice(&a[i..]);
    out.extend_from_slice(&b[j..]);
    out
}

/// The sorted door.
#[derive(Default)]
pub struct Door {
    shards: RwLock<FxHashMap<(u64, u32), Arc<Mutex<Shard>>>>,
}

impl Door {
    pub fn new() -> Self {
        Self::default()
    }

    /// A door already holding `entries` (each shard's `(key, id)`s, in any
    /// order) - the union of a level's layers so far.
    pub fn from_shards(entries: impl IntoIterator<Item = ((u64, u32), Vec<Entry>)>) -> Self {
        let mut shards = FxHashMap::default();
        for ((shape, cell), mut keys) in entries {
            keys.sort_unstable();
            // A key present twice (a grown speed hull re-emitted a row for
            // it): one entry, the newest id, the union of the hulls.
            keys.dedup_by(|b, a| {
                if a.0 == b.0 {
                    a.1 = a.1.max(b.1);
                    a.2 = hull_union(&a.2, &b.2);
                    true
                } else {
                    false
                }
            });
            keys.shrink_to_fit();
            let index = build_index(&keys);
            shards.insert((shape, cell), Arc::new(Mutex::new(Shard { base: keys, index, delta: Vec::new() })));
        }
        Door { shards: RwLock::new(shards) }
    }

    fn shard(&self, shape: u64, cell: u32) -> Arc<Mutex<Shard>> {
        if let Some(s) = self.shards.read().expect("door").get(&(shape, cell)) {
            return s.clone();
        }
        self.shards.write().expect("door").entry((shape, cell)).or_default().clone()
    }
}

thread_local! {
    static SCRATCH: std::cell::RefCell<Vec<Entry>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Admit for Door {
    fn admit(
        &self,
        shape: u64,
        cell: u32,
        keys: &[Key],
        first_new: u64,
        ids: &mut Vec<u64>,
        new: &mut Vec<u32>,
        hulls: Option<&[Hull]>,
        new_hulls: &mut Vec<Hull>,
    ) {
        debug_assert!(keys.windows(2).all(|w| w[0] < w[1]), "admit: keys sorted and deduplicated");
        let shard = self.shard(shape, cell);
        let mut shard = shard.lock().expect("door shard");
        SCRATCH.with(|s| shard.admit(keys, first_new, ids, new, hulls, new_hulls, &mut s.borrow_mut()));
    }

    fn end_frame(&self, workers: usize) {
        let shards: Vec<Arc<Mutex<Shard>>> = self.shards.read().expect("door").values().cloned().collect();
        let next = std::sync::atomic::AtomicUsize::new(0);
        std::thread::scope(|scope| {
            for _ in 0..workers.max(1) {
                let (shards, next) = (&shards, &next);
                scope.spawn(move || loop {
                    let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let Some(s) = shards.get(i) else { break };
                    s.lock().expect("door shard").end_frame();
                });
            }
        });
    }

    fn len(&self) -> usize {
        self.shards
            .read()
            .expect("door")
            .values()
            .map(|s| {
                let s = s.lock().expect("door shard");
                s.base.len() + s.delta.len()
            })
            .sum()
    }

    fn alloc_bytes(&self) -> usize {
        self.shards
            .read()
            .expect("door")
            .values()
            .map(|s| {
                let s = s.lock().expect("door shard");
                (s.base.capacity() + s.delta.capacity()) * std::mem::size_of::<Entry>() + s.index.capacity() * 4
            })
            .sum()
    }
}

/// The hash-set door: the same contract over `FxHashMap` shards.
#[cfg(test)]
#[derive(Default)]
pub struct HashDoor {
    shards: RwLock<FxHashMap<(u64, u32), Arc<Mutex<FxHashMap<Key, (u64, Hull)>>>>>,
}

#[cfg(test)]
impl HashDoor {
    pub fn new() -> Self {
        Self::default()
    }

    fn shard(&self, shape: u64, cell: u32) -> Arc<Mutex<FxHashMap<Key, (u64, Hull)>>> {
        if let Some(s) = self.shards.read().expect("door").get(&(shape, cell)) {
            return s.clone();
        }
        self.shards.write().expect("door").entry((shape, cell)).or_default().clone()
    }
}

#[cfg(test)]
impl Admit for HashDoor {
    fn admit(
        &self,
        shape: u64,
        cell: u32,
        keys: &[Key],
        first_new: u64,
        ids: &mut Vec<u64>,
        new: &mut Vec<u32>,
        hulls: Option<&[Hull]>,
        new_hulls: &mut Vec<Hull>,
    ) {
        let shard = self.shard(shape, cell);
        let mut set = shard.lock().expect("door shard");
        let mut n_new = 0u64;
        for (i, k) in keys.iter().enumerate() {
            match set.get_mut(k) {
                Some(e) => {
                    if let Some(h) = hulls.filter(|h| !hull_contains(&e.1, &h[i])) {
                        e.1 = hull_union(&e.1, &h[i]);
                        let id = first_new + n_new;
                        n_new += 1;
                        e.0 = id;
                        ids.push(id);
                        new.push(i as u32);
                        new_hulls.push(e.1);
                    } else {
                        ids.push(e.0);
                    }
                }
                None => {
                    let id = first_new + n_new;
                    n_new += 1;
                    let h = hulls.map_or(NO_HULL, |h| h[i]);
                    set.insert(*k, (id, h));
                    ids.push(id);
                    new.push(i as u32);
                    new_hulls.push(h);
                }
            }
        }
    }

    fn end_frame(&self, _workers: usize) {}

    fn len(&self) -> usize {
        self.shards.read().expect("door").values().map(|s| s.lock().expect("door shard").len()).sum()
    }

    fn alloc_bytes(&self) -> usize {
        self.shards
            .read()
            .expect("door")
            .values()
            .map(|s| s.lock().expect("door shard").capacity() * 25 + 16)
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A deterministic stream of (shape, cell, key) batches over a few
    /// frames, with heavy repetition within a batch, across batches of a
    /// frame, and across frames.
    fn stream(seed: u64) -> Vec<Vec<(u64, u32, Vec<Key>)>> {
        let mut x = seed;
        let mut rnd = move || {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            x
        };
        (0..4)
            .map(|_| {
                (0..60)
                    .map(|_| {
                        let shape = rnd() % 3;
                        let cell = (rnd() % 7) as u32;
                        let n = (rnd() % 40) as usize;
                        let keys: Vec<Key> = (0..n).map(|_| (rnd() % 50, rnd() % 3)).collect();
                        (shape, cell, keys)
                    })
                    .collect()
            })
            .collect()
    }

    fn run(door: &dyn Admit, frames: &[Vec<(u64, u32, Vec<Key>)>]) -> Vec<Vec<(u64, u32, Key)>> {
        let mut out = Vec::new();
        let mut next_id = 0u64;
        for frame in frames {
            let mut admitted = Vec::new();
            for (shape, cell, keys) in frame {
                let mut sorted = keys.clone();
                sorted.sort_unstable();
                sorted.dedup();
                let (mut new, mut ids) = (Vec::new(), Vec::new());
                door.admit(*shape, *cell, &sorted, next_id, &mut ids, &mut new, None, &mut Vec::new());
                next_id += new.len() as u64;
                // Every key gets an id; an old key's id is the one it got when admitted.
                assert_eq!(ids.len(), sorted.len());
                admitted.extend(new.iter().map(|&i| (*shape, *cell, sorted[i as usize])));
            }
            admitted.sort_unstable();
            out.push(admitted);
            door.end_frame(3);
        }
        out
    }

    #[test]
    fn sorted_door_admits_exactly_what_the_hash_door_admits() {
        for seed in 1..6u64 {
            let frames = stream(seed);
            let a = run(&Door::new(), &frames);
            let b = run(&HashDoor::new(), &frames);
            assert_eq!(a, b, "seed {seed}");
            let total: usize = a.iter().map(Vec::len).sum();
            assert!(total > 0);
            // Every admitted (shape, cell, key) is admitted exactly once over the run.
            let mut all: Vec<_> = a.concat();
            let n = all.len();
            all.sort_unstable();
            all.dedup();
            assert_eq!(all.len(), n, "seed {seed}: a key admitted twice");
        }
    }

    #[test]
    fn from_shards_preloads_the_base() {
        let door = Door::from_shards([((1, 2), vec![((5, 0), 50, NO_HULL), ((1, 0), 10, NO_HULL), ((5, 0), 50, NO_HULL)])]);
        assert_eq!(door.len(), 2);
        let (mut new, mut ids) = (Vec::new(), Vec::new());
        door.admit(1, 2, &[(0, 0), (1, 0), (5, 0), (6, 0)], 100, &mut ids, &mut new, None, &mut Vec::new());
        assert_eq!(new, vec![0, 3]);
        assert_eq!(ids, vec![100, 10, 50, 101], "old keys keep their ids, new ones get first_new + k");
        new.clear();
        ids.clear();
        door.admit(1, 2, &[(0, 0), (6, 0), (7, 0)], 200, &mut ids, &mut new, None, &mut Vec::new());
        assert_eq!(new, vec![2]);
        assert_eq!(ids, vec![100, 101, 200]);
        door.end_frame(2);
        assert_eq!(door.len(), 5);
        new.clear();
        ids.clear();
        door.admit(1, 2, &[(7, 0), (8, 0)], 300, &mut ids, &mut new, None, &mut Vec::new());
        assert_eq!(new, vec![1]);
        assert_eq!(ids, vec![200, 300]);
    }

    /// The speed hull: a key seen again with a speed inside its hull is
    /// the old state; outside it, the hull grows to the union, the key
    /// takes a fresh id and the row is new - carrying the union.
    #[test]
    fn a_grown_hull_is_a_new_row_carrying_the_union() {
        let door = Door::new();
        let (mut ids, mut new, mut nh) = (Vec::new(), Vec::new(), Vec::new());
        door.admit(1, 2, &[(5, 0)], 100, &mut ids, &mut new, Some(&[[10, 20, 0, 0]]), &mut nh);
        assert_eq!((ids.clone(), new.clone(), nh.clone()), (vec![100], vec![0], vec![[10, 20, 0, 0]]));
        door.end_frame(1);
        // inside: the old id
        let (mut ids, mut new, mut nh) = (Vec::new(), Vec::new(), Vec::new());
        door.admit(1, 2, &[(5, 0)], 200, &mut ids, &mut new, Some(&[[12, 18, 0, 0]]), &mut nh);
        assert_eq!((ids, new, nh), (vec![100], vec![], vec![]));
        // outside: grown, fresh id, the union
        let (mut ids, mut new, mut nh) = (Vec::new(), Vec::new(), Vec::new());
        door.admit(1, 2, &[(5, 0)], 300, &mut ids, &mut new, Some(&[[15, 30, -1, 1]]), &mut nh);
        assert_eq!((ids, new, nh), (vec![300], vec![0], vec![[10, 30, -1, 1]]));
        // and the union is what later rows are measured against
        let (mut ids, mut new, mut nh) = (Vec::new(), Vec::new(), Vec::new());
        door.admit(1, 2, &[(5, 0)], 400, &mut ids, &mut new, Some(&[[25, 30, 0, 0]]), &mut nh);
        assert_eq!((ids, new, nh), (vec![300], vec![], vec![]));
        assert_eq!(door.len(), 1);
    }

    #[test]
    fn admit_interleaving_with_the_delta_keeps_it_sorted() {
        let door = Door::new();
        let (mut new, mut ids) = (Vec::new(), Vec::new());
        door.admit(0, 0, &[(10, 0), (30, 0)], 0, &mut ids, &mut new, None, &mut Vec::new());
        door.admit(0, 0, &[(20, 0), (40, 0)], 2, &mut ids, &mut new, None, &mut Vec::new());
        door.admit(0, 0, &[(5, 0), (20, 0), (35, 0)], 4, &mut ids, &mut new, None, &mut Vec::new());
        assert_eq!(new, vec![0, 1, 0, 1, 0, 2]);
        assert_eq!(ids, vec![0, 1, 2, 3, 4, 2, 5]);
        assert_eq!(door.len(), 6);
    }
}
