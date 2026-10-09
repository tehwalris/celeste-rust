//! The visited set's probe step, behind a small trait so another probe (a
//! prefetching / interleaved `resolve`, branch dedup-mlp) drops in.
//!
//! Concurrency (shared by both tables): per-(shape, cell) open addressing,
//! linear probing, slots only ever go empty -> full (no deletes, no moves).
//! - LOOKUPS ARE LOCK-FREE: a slot is PUBLISHED by one Release store of its
//!   first word after the rest of it is written; a reader Acquire-loads that
//!   word, so a match sees the whole slot, and an empty word ends the probe.
//! - INSERTS take the shard's spinlock (its own 64-B line), RE-PROBE (another
//!   thread may have inserted the key since the lock-free miss), then publish.
//!   Inserts are 2.6% of the lookups (6.7M of 257.7M), so the lock is off the
//!   hot path; a CAS-only insert would need a 16-B CAS for an exact key, or a
//!   claim-then-fill protocol readers must wait on, for nothing measurable.
//!   An owner-only scheme (each shard written by one thread) would need the
//!   lookups routed to the owner (design D's partition pass, ~6 GB more
//!   traffic) because the sweep's chunks are claimed dynamically.
//! Ids: the inserter's `fresh()`; an old state's id is its door index.

use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};

/// One lookup of a chunk that missed the thread's cache.
#[derive(Clone, Copy, Default)]
pub struct Pending {
    pub key: u128,
    pub shard: u32,
    /// The query's offset in its chunk.
    pub at: u32,
    pub id: u32,
    pub inserted: bool,
}

#[derive(Default, Clone, Copy)]
pub struct ProbeStats {
    pub lookups: u64,
    pub found: u64,
    pub inserts: u64,
    /// The lock-free probe missed, the locked re-probe found it (another
    /// thread inserted it in between).
    pub raced: u64,
    pub lock_spins: u64,
}

pub trait Probe: Sync {
    /// Lock-free: the key's id, or None.
    fn lookup(&self, shard: u32, key: u128) -> Option<u32>;
    /// Under the shard's lock: the key's id if another thread inserted it
    /// meanwhile, else inserted with `fresh()`. Returns (id, inserted).
    fn insert(&self, shard: u32, key: u128, fresh: &mut dyn FnMut() -> u32, st: &mut ProbeStats) -> (u32, bool);
    /// A chunk's cache misses, in order; the hook for a prefetching probe.
    /// `fresh` must be called once per insert, in any order (the caller
    /// places each new row by its id, not by call order).
    fn resolve(&self, batch: &mut [Pending], fresh: &mut dyn FnMut() -> u32, st: &mut ProbeStats) {
        for p in batch.iter_mut() {
            st.lookups += 1;
            match self.lookup(p.shard, p.key) {
                Some(id) => {
                    st.found += 1;
                    p.id = id;
                    p.inserted = false;
                }
                None => {
                    let (id, ins) = self.insert(p.shard, p.key, fresh, st);
                    p.id = id;
                    p.inserted = ins;
                }
            }
        }
    }
    /// The frame's end, the shard's single owner: overwrite a present key's id.
    fn set_id(&self, shard: u32, key: u128, id: u32);
    fn bytes(&self) -> usize;
    fn name(&self) -> &'static str;
}

#[repr(C, align(64))]
pub struct Lock(AtomicBool);

impl Lock {
    #[inline]
    fn lock(&self, st: &mut ProbeStats) {
        while self.0.compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed).is_err() {
            while self.0.load(Ordering::Relaxed) {
                st.lock_spins += 1;
                std::hint::spin_loop();
            }
        }
    }
    #[inline]
    fn unlock(&self) {
        self.0.store(false, Ordering::Release);
    }
}

/// An anonymous, huge-page-advised, zeroed array (the zero page is "empty").
pub struct Arena<T> {
    ptr: *mut T,
    len: usize,
}
unsafe impl<T: Sync> Sync for Arena<T> {}
unsafe impl<T: Send> Send for Arena<T> {}

impl<T> Arena<T> {
    pub fn zeroed(len: usize) -> Self {
        let layout = std::alloc::Layout::from_size_align(len.max(1) * std::mem::size_of::<T>(), 1 << 21).unwrap();
        let ptr = unsafe { std::alloc::alloc_zeroed(layout) } as *mut T;
        assert!(!ptr.is_null(), "arena allocation");
        crate::canon::advise_huge(ptr as *const u8, layout.size());
        Arena { ptr, len }
    }
    #[inline(always)]
    pub fn at(&self, i: usize) -> &T {
        debug_assert!(i < self.len);
        unsafe { &*self.ptr.add(i) }
    }
    pub fn ptr(&self) -> *mut T {
        self.ptr
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn bytes(&self) -> usize {
        self.len * std::mem::size_of::<T>()
    }
}

impl<T> Drop for Arena<T> {
    fn drop(&mut self) {
        let layout = std::alloc::Layout::from_size_align(self.len.max(1) * std::mem::size_of::<T>(), 1 << 21).unwrap();
        unsafe { std::alloc::dealloc(self.ptr as *mut u8, layout) }
    }
}

/// Per shard (arena base, mask) from its final size at `load`.
fn layout(cnt: &[u64], load: f64) -> (Vec<(u64, u64)>, u64) {
    let mut off = Vec::with_capacity(cnt.len());
    let mut total = 0u64;
    for &c in cnt {
        let cap = ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two();
        off.push((total, cap - 1));
        total += cap;
    }
    (off, total)
}

// ------------------------------------------------------------------ v4-style
/// v4's table (an EXPLORATION, as in v4: the 64-bit fingerprint is the key's
/// high half, the start slot its low bits), slot padded to 16 B so the
/// fingerprint is an aligned atomic word.
#[repr(C, align(16))]
pub struct S16 {
    fp: AtomicU64,
    id: AtomicU32,
    _p: u32,
}

pub struct V4 {
    off: Vec<(u64, u64)>,
    slots: Arena<S16>,
    locks: Vec<Lock>,
}

impl V4 {
    pub fn new(cnt: &[u64], load: f64) -> Self {
        let (off, total) = layout(cnt, load);
        V4 { off, slots: Arena::zeroed(total as usize), locks: (0..cnt.len()).map(|_| Lock(AtomicBool::new(false))).collect() }
    }
    #[inline(always)]
    fn fp(key: u128) -> u64 {
        let f = (key >> 64) as u64;
        if f == 0 {
            1
        } else {
            f
        }
    }
}

impl Probe for V4 {
    #[inline]
    fn lookup(&self, shard: u32, key: u128) -> Option<u32> {
        let f = Self::fp(key);
        let (b, m) = self.off[shard as usize];
        let mut i = key as u64 & m;
        loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.fp.load(Ordering::Acquire);
            if g == f {
                return Some(s.id.load(Ordering::Relaxed));
            }
            if g == 0 {
                return None;
            }
            i = (i + 1) & m;
        }
    }
    fn insert(&self, shard: u32, key: u128, fresh: &mut dyn FnMut() -> u32, st: &mut ProbeStats) -> (u32, bool) {
        let f = Self::fp(key);
        let (b, m) = self.off[shard as usize];
        let lock = &self.locks[shard as usize];
        lock.lock(st);
        let mut i = key as u64 & m;
        let r = loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.fp.load(Ordering::Relaxed);
            if g == f {
                st.raced += 1;
                break (s.id.load(Ordering::Relaxed), false);
            }
            if g == 0 {
                let id = fresh();
                s.id.store(id, Ordering::Relaxed);
                s.fp.store(f, Ordering::Release);
                st.inserts += 1;
                break (id, true);
            }
            i = (i + 1) & m;
        };
        lock.unlock();
        r
    }
    fn set_id(&self, shard: u32, key: u128, id: u32) {
        let f = Self::fp(key);
        let (b, m) = self.off[shard as usize];
        let mut i = key as u64 & m;
        loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.fp.load(Ordering::Relaxed);
            assert_ne!(g, 0, "set_id: key absent");
            if g == f {
                s.id.store(id, Ordering::Relaxed);
                return;
            }
            i = (i + 1) & m;
        }
    }
    fn bytes(&self) -> usize {
        self.slots.bytes() + self.locks.len() * 64
    }
    fn name(&self) -> &'static str {
        "v4"
    }
}

// ------------------------------------------------------------------ exact
/// Exact 16-B keys (v3c's contract), one 24-B slot: `lo` (never 0; checked
/// at setup) is the publish word.
#[repr(C)]
pub struct S24 {
    lo: AtomicU64,
    hi: AtomicU64,
    id: AtomicU32,
    _p: u32,
}

pub struct Exact {
    off: Vec<(u64, u64)>,
    slots: Arena<S24>,
    locks: Vec<Lock>,
}

impl Exact {
    pub fn new(cnt: &[u64], load: f64) -> Self {
        let (off, total) = layout(cnt, load);
        Exact { off, slots: Arena::zeroed(total as usize), locks: (0..cnt.len()).map(|_| Lock(AtomicBool::new(false))).collect() }
    }
}

impl Probe for Exact {
    #[inline]
    fn lookup(&self, shard: u32, key: u128) -> Option<u32> {
        let (lo, hi) = (key as u64, (key >> 64) as u64);
        let (b, m) = self.off[shard as usize];
        let mut i = lo & m;
        loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.lo.load(Ordering::Acquire);
            if g == lo && s.hi.load(Ordering::Relaxed) == hi {
                return Some(s.id.load(Ordering::Relaxed));
            }
            if g == 0 {
                return None;
            }
            i = (i + 1) & m;
        }
    }
    fn insert(&self, shard: u32, key: u128, fresh: &mut dyn FnMut() -> u32, st: &mut ProbeStats) -> (u32, bool) {
        let (lo, hi) = (key as u64, (key >> 64) as u64);
        let (b, m) = self.off[shard as usize];
        let lock = &self.locks[shard as usize];
        lock.lock(st);
        let mut i = lo & m;
        let r = loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.lo.load(Ordering::Relaxed);
            if g == lo && s.hi.load(Ordering::Relaxed) == hi {
                st.raced += 1;
                break (s.id.load(Ordering::Relaxed), false);
            }
            if g == 0 {
                let id = fresh();
                s.hi.store(hi, Ordering::Relaxed);
                s.id.store(id, Ordering::Relaxed);
                s.lo.store(lo, Ordering::Release);
                st.inserts += 1;
                break (id, true);
            }
            i = (i + 1) & m;
        };
        lock.unlock();
        r
    }
    fn set_id(&self, shard: u32, key: u128, id: u32) {
        let (lo, hi) = (key as u64, (key >> 64) as u64);
        let (b, m) = self.off[shard as usize];
        let mut i = lo & m;
        loop {
            let s = self.slots.at((b + i) as usize);
            let g = s.lo.load(Ordering::Relaxed);
            assert_ne!(g, 0, "set_id: key absent");
            if g == lo && s.hi.load(Ordering::Relaxed) == hi {
                s.id.store(id, Ordering::Relaxed);
                return;
            }
            i = (i + 1) & m;
        }
    }
    fn bytes(&self) -> usize {
        self.slots.bytes() + self.locks.len() * 64
    }
    fn name(&self) -> &'static str {
        "exact"
    }
}
