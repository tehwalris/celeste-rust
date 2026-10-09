//! The door: every `(shape, cell, key)` the forward has reached, against
//! which emitted rows are deduplicated.
//!
//! Sharded by `(shape, cell)` (which the key determines); within a shard a
//! SORTED `base` (up to the previous frame, read-only during a frame) plus a
//! `delta` of this frame's admissions (a hash map: a sorted vector merged on
//! every admit was quadratic in a shard's frame, 7% of room (6,2) 100% f57).
//! `admit` looks a sorted batch up in both; `end_frame` sorts each delta
//! once and folds it into the base.
//! Each entry carries the state's id, which edges to old states are written
//! as. `HashDoor` (tests only) is the oracle for the same contract.

use rustc_hash::FxHashMap;
use std::sync::{Arc, Mutex, RwLock};

pub type Key = (u64, u64);

/// A door entry: the key and the state's id `(layer, file seq, row)` packed
/// by `crate::frame::pack_id`. 24 B.
pub type Entry = (Key, u64);

/// The door's contract: admit sorted, deduplicated key batches; merge at
/// the end of a frame.
pub trait Admit: Sync {
    /// `keys` sorted, no duplicates. `ids` gets each key's id: the existing
    /// entry's, or `first_new + k` for the k-th new key (recorded); `new`
    /// gets the new keys' indices.
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], first_new: u64, ids: &mut Vec<u64>, new: &mut Vec<u32>);
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
    /// Bucket starts into `base` by the top bits of `key.0` (uniform
    /// hashes), ~8 entries a bucket: a lookup touches one index word and a
    /// line or two of `base`. Rebuilt with `base`.
    index: Vec<u32>,
    delta: FxHashMap<Key, u64>,
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
    for (i, e) in base.iter().enumerate() {
        let kb = if bits == 0 { 0 } else { (e.0 .0 >> (64 - bits)) as usize };
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
    /// Look `keys` up in `base` and `delta`; the misses get fresh ids and go
    /// to `delta`.
    fn admit(&mut self, keys: &[Key], first_new: u64, ids: &mut Vec<u64>, new: &mut Vec<u32>) {
        let bits = index_bits(self.base.len());
        const AHEAD: usize = 8;
        for k in keys.iter().take(AHEAD) {
            self.prefetch(bits, k);
        }
        let mut fresh = 0u64;
        for (i, k) in keys.iter().enumerate() {
            if let Some(k2) = keys.get(i + AHEAD) {
                self.prefetch(bits, k2);
            }
            if !self.base.is_empty() {
                let kb = if bits == 0 { 0 } else { (k.0 >> (64 - bits)) as usize };
                let (lo, hi) = (self.index[kb] as usize, self.index[kb + 1] as usize);
                let b = lo + self.base[lo..hi].partition_point(|x| x.0 < *k);
                if b < hi && self.base[b].0 == *k {
                    ids.push(self.base[b].1);
                    continue;
                }
            }
            if let Some(&id) = self.delta.get(k) {
                ids.push(id);
                continue;
            }
            let id = first_new + fresh;
            fresh += 1;
            ids.push(id);
            new.push(i as u32);
            self.delta.insert(*k, id);
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
        let mut delta: Vec<Entry> = self.delta.drain().collect();
        self.delta.shrink_to_fit();
        delta.sort_unstable_by_key(|e| e.0);
        if self.base.last().is_some_and(|last| last.0 < delta[0].0) || self.base.is_empty() {
            self.base.append(&mut delta);
        } else {
            self.base = merge_sorted(&self.base, &delta);
        }
        self.index = build_index(&self.base);
    }

    fn len(&self) -> usize {
        self.base.len() + self.delta.len()
    }

    fn alloc_bytes(&self) -> usize {
        (self.base.capacity() + self.delta.capacity()) * std::mem::size_of::<Entry>() + self.index.capacity() * 4
    }
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

    /// A door already holding `entries` (per shard, any order).
    pub fn from_shards(entries: impl IntoIterator<Item = ((u64, u32), Vec<Entry>)>) -> Self {
        let mut shards = FxHashMap::default();
        for ((shape, cell), mut keys) in entries {
            keys.sort_unstable();
            // A key present twice: one entry, the newest id.
            keys.dedup_by(|b, a| {
                if a.0 == b.0 {
                    a.1 = a.1.max(b.1);
                    true
                } else {
                    false
                }
            });
            keys.shrink_to_fit();
            let index = build_index(&keys);
            shards.insert((shape, cell), Arc::new(Mutex::new(Shard { base: keys, index, delta: FxHashMap::default() })));
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

impl Admit for Door {
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], first_new: u64, ids: &mut Vec<u64>, new: &mut Vec<u32>) {
        debug_assert!(keys.windows(2).all(|w| w[0] < w[1]), "admit: keys sorted and deduplicated");
        let shard = self.shard(shape, cell);
        let mut shard = shard.lock().expect("door shard");
        shard.admit(keys, first_new, ids, new)
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
        self.shards.read().expect("door").values().map(|s| s.lock().expect("door shard").len()).sum()
    }

    fn alloc_bytes(&self) -> usize {
        self.shards.read().expect("door").values().map(|s| s.lock().expect("door shard").alloc_bytes()).sum()
    }
}

/// The hash-set door: the same contract over `FxHashMap` shards.
#[cfg(test)]
#[derive(Default)]
pub struct HashDoor {
    shards: RwLock<FxHashMap<(u64, u32), Arc<Mutex<FxHashMap<Key, u64>>>>>,
}

#[cfg(test)]
impl HashDoor {
    pub fn new() -> Self {
        Self::default()
    }

    fn shard(&self, shape: u64, cell: u32) -> Arc<Mutex<FxHashMap<Key, u64>>> {
        if let Some(s) = self.shards.read().expect("door").get(&(shape, cell)) {
            return s.clone();
        }
        self.shards.write().expect("door").entry((shape, cell)).or_default().clone()
    }
}

#[cfg(test)]
impl Admit for HashDoor {
    fn admit(&self, shape: u64, cell: u32, keys: &[Key], first_new: u64, ids: &mut Vec<u64>, new: &mut Vec<u32>) {
        let shard = self.shard(shape, cell);
        let mut set = shard.lock().expect("door shard");
        let mut n_new = 0u64;
        for (i, k) in keys.iter().enumerate() {
            match set.get(k) {
                Some(&id) => ids.push(id),
                None => {
                    let id = first_new + n_new;
                    n_new += 1;
                    set.insert(*k, id);
                    ids.push(id);
                    new.push(i as u32);
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

    /// Deterministic (shape, cell, key) batches over a few frames, with
    /// heavy repetition within and across batches and frames.
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
                door.admit(*shape, *cell, &sorted, next_id, &mut ids, &mut new);
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

    /// The door's diet: the key and the id, nothing else.
    #[test]
    fn entries_are_the_key_and_the_id() {
        assert_eq!(std::mem::size_of::<Entry>(), 24);
    }

    #[test]
    fn from_shards_preloads_the_base() {
        let door = Door::from_shards([((1, 2), vec![((5, 0), 50), ((1, 0), 10), ((5, 0), 50)])]);
        assert_eq!(door.len(), 2);
        let (mut new, mut ids) = (Vec::new(), Vec::new());
        door.admit(1, 2, &[(0, 0), (1, 0), (5, 0), (6, 0)], 100, &mut ids, &mut new);
        assert_eq!(new, vec![0, 3]);
        assert_eq!(ids, vec![100, 10, 50, 101], "old keys keep their ids, new ones get first_new + k");
        new.clear();
        ids.clear();
        door.admit(1, 2, &[(0, 0), (6, 0), (7, 0)], 200, &mut ids, &mut new);
        assert_eq!(new, vec![2]);
        assert_eq!(ids, vec![100, 101, 200]);
        door.end_frame(2);
        assert_eq!(door.len(), 5);
        new.clear();
        ids.clear();
        door.admit(1, 2, &[(7, 0), (8, 0)], 300, &mut ids, &mut new);
        assert_eq!(new, vec![1]);
        assert_eq!(ids, vec![200, 300]);
    }

    #[test]
    fn admit_interleaving_with_the_delta_keeps_it_sorted() {
        let door = Door::new();
        let (mut new, mut ids) = (Vec::new(), Vec::new());
        door.admit(0, 0, &[(10, 0), (30, 0)], 0, &mut ids, &mut new);
        door.admit(0, 0, &[(20, 0), (40, 0)], 2, &mut ids, &mut new);
        door.admit(0, 0, &[(5, 0), (20, 0), (35, 0)], 4, &mut ids, &mut new);
        assert_eq!(new, vec![0, 1, 0, 1, 0, 2]);
        assert_eq!(ids, vec![0, 1, 2, 3, 4, 2, 5]);
        assert_eq!(door.len(), 6);
    }
}
