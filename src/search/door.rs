//! The door: the set of every `(shape, cell, key)` the forward has reached,
//! against which emitted rows are deduplicated (plans/waves.md).
//!
//! Sharded by `(shape, cell)` - two rows in different shards can never be
//! the same state - and, within a shard, a SORTED array of 128-bit keys
//! (`base`, everything up to the previous frame, immutable during a
//! frame) plus a small sorted `delta` (this frame's admissions). A flush
//! `admit`s a sorted batch of keys with one merge-join sweep over both;
//! `end_frame` merges every delta into its base, once per frame. So the
//! big set is only READ while a frame runs, and each entry costs 16 bytes.
//!
//! `HashDoor` is the same contract over a hash set per shard (the
//! pre-2026-09-13 representation), kept as the oracle for `Door`'s tests
//! and the benchmark's comparison point.

use rustc_hash::FxHashMap;
use std::sync::{Arc, Mutex, RwLock};

pub type Key = (u64, u64);

/// The door's contract: admit sorted, deduplicated key batches; merge at
/// the end of a frame.
pub trait Admit: Sync {
    /// `keys` sorted ascending with no duplicates. Pushes into `new` the
    /// indices of the keys NOT already in the shard, and records them.
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], new: &mut Vec<u32>);
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
    base: Vec<Key>,
    /// Bucket starts into `base` by the top bits of `key.0` (the keys are
    /// uniform hashes): `index.len() - 1` buckets of ~8 entries, so a
    /// lookup touches one index word and one or two lines of `base`
    /// instead of galloping through it. 0.5 B/entry. Rebuilt with `base`.
    index: Vec<u32>,
    delta: Vec<Key>,
}

/// log2 of the bucket count for `n` entries: ~8 entries per bucket.
fn index_bits(n: usize) -> u32 {
    (n / 8).max(1).next_power_of_two().trailing_zeros()
}

fn build_index(base: &[Key]) -> Vec<u32> {
    let bits = index_bits(base.len());
    let nb = 1usize << bits;
    let mut index = vec![0u32; nb + 1];
    let mut b = 0usize;
    for (i, k) in base.iter().enumerate() {
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
    /// Merge-join `keys` against `base` and `delta`; the misses go to
    /// `new` (by index) and are appended to `delta`, which stays sorted
    /// because the appended keys are sorted and... not necessarily above
    /// the existing delta - so the delta is re-sorted by a merge when the
    /// batch interleaves with it (`merge_into`).
    fn admit(&mut self, keys: &[Key], new: &mut Vec<u32>, scratch: &mut Vec<Key>) {
        let mut d = 0usize;
        let (base, delta) = (&self.base, &self.delta);
        scratch.clear();
        let bits = index_bits(base.len());
        const AHEAD: usize = 8;
        for k in keys.iter().take(AHEAD) {
            self.prefetch(bits, k);
        }
        for (i, k) in keys.iter().enumerate() {
            if let Some(k2) = keys.get(i + AHEAD) {
                self.prefetch(bits, k2);
            }
            if !base.is_empty() {
                let kb = if bits == 0 { 0 } else { (k.0 >> (64 - bits)) as usize };
                let (lo, hi) = (self.index[kb] as usize, self.index[kb + 1] as usize);
                let bucket = &base[lo..hi];
                let b = lo + bucket.partition_point(|x| x < k);
                if b < hi && base[b] == *k {
                    continue;
                }
            }
            d = gallop(delta, d, k);
            if d < delta.len() && delta[d] == *k {
                continue;
            }
            new.push(i as u32);
            scratch.push(*k);
        }
        if scratch.is_empty() {
            return;
        }
        if self.delta.last().is_some_and(|last| *last < scratch[0]) || self.delta.is_empty() {
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
        if self.base.last().is_some_and(|last| *last < self.delta[0]) || self.base.is_empty() {
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
/// exponential then binary search from `from`: the batch's keys are
/// sorted too, so the answer is usually a few entries ahead and the cost
/// is ~2 log2(gap) probes rather than a linear sweep of the shard.
#[inline]
fn gallop(a: &[Key], from: usize, k: &Key) -> usize {
    let n = a.len();
    if from >= n || a[from] >= *k {
        return from;
    }
    let mut step = 1;
    let mut lo = from;
    let mut hi = from + 1;
    while hi < n && a[hi] < *k {
        lo = hi;
        step *= 2;
        hi = (hi + step).min(n);
        if hi == n {
            break;
        }
    }
    // a[lo] < k <= a[hi] (or hi == n)
    lo + 1 + a[lo + 1..hi.min(n)].partition_point(|x| x < k)
}

/// Two sorted, individually duplicate-free, mutually disjoint slices
/// merged into one.
fn merge_sorted(a: &[Key], b: &[Key]) -> Vec<Key> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        if a[i] < b[j] {
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

    /// A door already holding `entries` (each shard's keys, in any order,
    /// duplicates allowed) - the union of a level's layers so far.
    pub fn from_shards(entries: impl IntoIterator<Item = ((u64, u32), Vec<Key>)>) -> Self {
        let mut shards = FxHashMap::default();
        for ((shape, cell), mut keys) in entries {
            keys.sort_unstable();
            keys.dedup();
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

    pub fn shards(&self) -> usize {
        self.shards.read().expect("door").len()
    }
}

thread_local! {
    static SCRATCH: std::cell::RefCell<Vec<Key>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Admit for Door {
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], new: &mut Vec<u32>) {
        debug_assert!(keys.windows(2).all(|w| w[0] < w[1]), "admit: keys sorted and deduplicated");
        let shard = self.shard(shape, cell);
        let mut shard = shard.lock().expect("door shard");
        SCRATCH.with(|s| shard.admit(keys, new, &mut s.borrow_mut()));
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
                (s.base.capacity() + s.delta.capacity()) * 16 + s.index.capacity() * 4
            })
            .sum()
    }
}

/// The hash-set door: the same contract over `FxHashSet` shards.
#[derive(Default)]
pub struct HashDoor {
    shards: RwLock<FxHashMap<(u64, u32), Arc<Mutex<rustc_hash::FxHashSet<Key>>>>>,
}

impl HashDoor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_shards(entries: impl IntoIterator<Item = ((u64, u32), Vec<Key>)>) -> Self {
        let mut shards = FxHashMap::default();
        for (k, keys) in entries {
            let set: rustc_hash::FxHashSet<Key> = keys.into_iter().collect();
            shards.insert(k, Arc::new(Mutex::new(set)));
        }
        HashDoor { shards: RwLock::new(shards) }
    }

    fn shard(&self, shape: u64, cell: u32) -> Arc<Mutex<rustc_hash::FxHashSet<Key>>> {
        if let Some(s) = self.shards.read().expect("door").get(&(shape, cell)) {
            return s.clone();
        }
        self.shards.write().expect("door").entry((shape, cell)).or_default().clone()
    }
}

impl Admit for HashDoor {
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], new: &mut Vec<u32>) {
        let shard = self.shard(shape, cell);
        let mut set = shard.lock().expect("door shard");
        for (i, k) in keys.iter().enumerate() {
            if set.insert(*k) {
                new.push(i as u32);
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
            .map(|s| s.lock().expect("door shard").capacity() * 17 + 16)
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
        for frame in frames {
            let mut admitted = Vec::new();
            for (shape, cell, keys) in frame {
                let mut sorted = keys.clone();
                sorted.sort_unstable();
                sorted.dedup();
                let mut new = Vec::new();
                door.admit(*shape, *cell, &sorted, &mut new);
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
        let door = Door::from_shards([((1, 2), vec![(5, 0), (1, 0), (5, 0)])]);
        assert_eq!(door.len(), 2);
        let mut new = Vec::new();
        door.admit(1, 2, &[(0, 0), (1, 0), (5, 0), (6, 0)], &mut new);
        assert_eq!(new, vec![0, 3]);
        new.clear();
        door.admit(1, 2, &[(0, 0), (6, 0), (7, 0)], &mut new);
        assert_eq!(new, vec![2]);
        door.end_frame(2);
        assert_eq!(door.len(), 5);
        new.clear();
        door.admit(1, 2, &[(7, 0), (8, 0)], &mut new);
        assert_eq!(new, vec![1]);
    }

    #[test]
    fn admit_interleaving_with_the_delta_keeps_it_sorted() {
        let door = Door::new();
        let mut new = Vec::new();
        door.admit(0, 0, &[(10, 0), (30, 0)], &mut new);
        door.admit(0, 0, &[(20, 0), (40, 0)], &mut new);
        door.admit(0, 0, &[(5, 0), (20, 0), (35, 0)], &mut new);
        assert_eq!(new, vec![0, 1, 0, 1, 0, 2]);
        assert_eq!(door.len(), 6);
    }
}
