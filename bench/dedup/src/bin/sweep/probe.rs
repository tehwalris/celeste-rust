//! The visited set's probe step, behind a small trait so another probe (a
//! prefetching / interleaved `resolve`, branch dedup-mlp) drops in.
//!
//! `Pm8` (posmask8, the default) is fully lock-free; see its insert.
//! Concurrency of the keyed tables (`V4`, `Exact`): per-(shape, cell) open addressing,
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

/// A new state in CANONICAL order (by (cell shard, key)): its canonical id
/// is `n_door +` its index. `prov`: the id the wave gave it; `qi`: one
/// lookup that reached it.
#[derive(Clone, Copy, Default)]
pub struct NewState {
    pub key: u128,
    pub shard: u32,
    pub prov: u32,
    pub qi: u32,
}

pub trait Probe: Sync {
    /// What the probe is keyed on for lookup `qi` (cell shard, row key):
    /// (its shard, its key). The default: as given.
    #[inline(always)]
    fn query(&self, _qi: usize, shard: u32, key: u128) -> (u32, u128) {
        (shard, key)
    }
    /// Some(n): ids are POSITIONAL (`n_door + p`, p < n), `fresh` unused.
    fn positional_ids(&self) -> Option<usize> {
        None
    }
    /// The frame's end, before renumbering: Some(n) if provisional ids get
    /// a dense index in [0, n) (`renum_index`); None: `prov - n_door`.
    fn renum_prepare(&self, _threads: usize) -> Option<usize> {
        None
    }
    #[inline(always)]
    fn renum_index(&self, prov: u32, n_door: u32) -> usize {
        (prov - n_door) as usize
    }
    /// The frame's end, after the canonical order is known (no lookups run):
    /// make every new state's id its canonical one. The default: `set_id`.
    fn end_frame(&self, new: &[NewState], _renum: &(dyn Fn(u32) -> u32 + Sync), n_door: u32, threads: usize) {
        let per = new.len().div_ceil(threads).max(1);
        std::thread::scope(|s| {
            for (c, part) in new.chunks(per).enumerate() {
                s.spawn(move || {
                    for (k, x) in part.iter().enumerate() {
                        self.set_id(x.shard, x.key, n_door + (c * per + k) as u32);
                    }
                });
            }
        });
    }
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

// ------------------------------------------------------------------ posmask8
/// `posmask8` (DESIGNS I2): a directory per (shape, 8x8 region) on a u32 of
/// the high digits + spd.y, one 64-bit mask over the region's cells. The
/// keys come from `dedup-bench posmask8` with PM_DUMP (pm8{door,q,cnt}.bin).
/// An entry: `old` (the frame-start states, fixed during the wave), `new`
/// (this frame's, atomic fetch_or), `base` = the rank of its first old state
/// in (group, slot) order. The structure stores no ids: an old state's id is
/// `idmap[base + popcount(old below)]` (a u32 a state, rebuilt at the frame's
/// end, as the door's merge rebuilds the base); a new one's is positional,
/// `n_door + slot << 6 | bit`, renumbered at the frame's end.
#[repr(C)]
pub struct E24 {
    hk: AtomicU32,
    base: AtomicU32,
    old: AtomicU64,
    new: AtomicU64,
}

pub struct Pm8 {
    off: Vec<(u32, u32)>,
    slots: Arena<E24>,
    qpk: memmap2::Mmap,
    idmap: std::cell::UnsafeCell<Vec<u32>>,
    /// The frame's end only: per slot, its first new state's rank.
    newoff: std::cell::UnsafeCell<Vec<u32>>,
    n_door: u32,
}
// `idmap` and `newoff` are replaced only at the frame's end, when no lookup runs.
unsafe impl Sync for Pm8 {}

#[inline(always)]
fn pm_slot(hk: u32, (b, m): (u32, u32)) -> u32 {
    b + (hk.wrapping_mul(0x9E37_79B1).rotate_left(16) & m)
}

impl Pm8 {
    pub fn new(pd: &str, n_door: usize, load: f64) -> Self {
        let rd = |f: &str| std::fs::read(format!("{pd}/{f}")).unwrap_or_else(|e| panic!("{pd}/{f}: {e} (run dedup-bench posmask8 with PM_DUMP={pd})"));
        let cnt: Vec<u64> = rd("posmask8cnt.bin").chunks_exact(8).map(|c| u64::from_le_bytes(c.try_into().unwrap())).collect();
        let door_pk: Vec<u64> = rd("posmask8door.bin").chunks_exact(8).map(|c| u64::from_le_bytes(c.try_into().unwrap())).collect();
        assert_eq!(door_pk.len(), n_door);
        let mut off = Vec::with_capacity(cnt.len());
        let mut total = 0u64;
        for &c in &cnt {
            let cap = ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two();
            off.push((total as u32, (cap - 1) as u32));
            total += cap;
        }
        assert!(n_door as u64 + total * 64 < 1 << 31, "positional ids must fit 31 bits");
        let qf = std::fs::File::open(format!("{pd}/posmask8q.bin")).unwrap();
        let qpk = unsafe { memmap2::MmapOptions::new().populate().map(&qf).unwrap() };
        let p = Pm8 { off, slots: Arena::zeroed(total as usize), qpk, idmap: std::cell::UnsafeCell::new(Vec::new()), newoff: std::cell::UnsafeCell::new(Vec::new()), n_door: n_door as u32 };
        // The door's states as `old` bits; then ranks and the id map.
        for &pk in &door_pk {
            let (g, hk, b) = Self::split(pk);
            let s = p.find_or_make(g, hk);
            let e = p.slots.at(s as usize);
            let o = e.old.load(Ordering::Relaxed);
            assert_eq!(o & 1 << b, 0, "a door state twice");
            e.old.store(o | 1 << b, Ordering::Relaxed);
        }
        let mut acc = 0u32;
        for i in 0..p.slots.len() {
            let e = p.slots.at(i);
            e.base.store(acc, Ordering::Relaxed);
            acc += e.old.load(Ordering::Relaxed).count_ones();
        }
        assert_eq!(acc as usize, n_door);
        let mut idmap = vec![u32::MAX; n_door];
        for (i, &pk) in door_pk.iter().enumerate() {
            let (g, hk, b) = Self::split(pk);
            let e = p.slots.at(p.find(g, hk).unwrap() as usize);
            idmap[(e.base.load(Ordering::Relaxed) + (e.old.load(Ordering::Relaxed) & ((1u64 << b) - 1)).count_ones()) as usize] = i as u32;
        }
        unsafe { *p.idmap.get() = idmap };
        p
    }
    #[inline(always)]
    fn split(pk: u64) -> (u32, u32, u32) {
        ((pk >> 38) as u32, (pk >> 6) as u32, (pk & 63) as u32)
    }
    #[inline(always)]
    fn find(&self, g: u32, hk: u32) -> Option<u32> {
        let (b, m) = self.off[g as usize];
        let mut s = pm_slot(hk, (b, m));
        loop {
            let k = self.slots.at(s as usize).hk.load(Ordering::Acquire);
            if k == hk {
                return Some(s);
            }
            if k == 0 {
                return None;
            }
            s = b + ((s - b + 1) & m);
        }
    }
    /// Setup only (single thread).
    fn find_or_make(&self, g: u32, hk: u32) -> u32 {
        let (b, m) = self.off[g as usize];
        let mut s = pm_slot(hk, (b, m));
        loop {
            let e = self.slots.at(s as usize);
            let k = e.hk.load(Ordering::Relaxed);
            if k == hk {
                return s;
            }
            if k == 0 {
                e.hk.store(hk, Ordering::Relaxed);
                return s;
            }
            s = b + ((s - b + 1) & m);
        }
    }
    #[inline(always)]
    fn idmap(&self) -> &[u32] {
        unsafe { &*self.idmap.get() }
    }
}

impl Probe for Pm8 {
    #[inline(always)]
    fn query(&self, qi: usize, _shard: u32, _key: u128) -> (u32, u128) {
        let pk = u64::from_le_bytes(self.qpk[qi * 8..qi * 8 + 8].try_into().unwrap());
        ((pk >> 38) as u32, pk as u128)
    }
    fn positional_ids(&self) -> Option<usize> {
        Some(self.slots.len() * 64)
    }
    #[inline]
    fn lookup(&self, _g: u32, key: u128) -> Option<u32> {
        let (g, hk, bit) = Self::split(key as u64);
        let s = self.find(g, hk)?;
        let e = self.slots.at(s as usize);
        let b = 1u64 << bit;
        let o = e.old.load(Ordering::Relaxed);
        if o & b != 0 {
            return Some(self.idmap()[(e.base.load(Ordering::Relaxed) + (o & (b - 1)).count_ones()) as usize]);
        }
        (e.new.load(Ordering::Acquire) & b != 0).then(|| self.n_door + (s << 6 | bit))
    }
    /// LOCK-FREE (unlike the keyed tables): 397 groups are too few locks
    /// for the front's threads (416M spins at 16 threads with a group
    /// lock). An entry is created by a CAS of its u32 key 0 -> hk (its
    /// masks are zero until then); a state is inserted by `fetch_or` on
    /// `new`, whose old value says who was first.
    fn insert(&self, _g: u32, key: u128, _fresh: &mut dyn FnMut() -> u32, st: &mut ProbeStats) -> (u32, bool) {
        let (g, hk, bit) = Self::split(key as u64);
        let b = 1u64 << bit;
        let (bs, m) = self.off[g as usize];
        let mut s = pm_slot(hk, (bs, m));
        loop {
            let e = self.slots.at(s as usize);
            let mut k = e.hk.load(Ordering::Acquire);
            if k == 0 {
                k = match e.hk.compare_exchange(0, hk, Ordering::AcqRel, Ordering::Acquire) {
                    Ok(_) => hk,
                    Err(other) => other,
                };
            }
            if k == hk {
                let o = e.old.load(Ordering::Relaxed);
                if o & b != 0 {
                    st.raced += 1;
                    return (self.idmap()[(e.base.load(Ordering::Relaxed) + (o & (b - 1)).count_ones()) as usize], false);
                }
                let prev = e.new.fetch_or(b, Ordering::AcqRel);
                if prev & b != 0 {
                    st.raced += 1;
                } else {
                    st.inserts += 1;
                }
                return (self.n_door + (s << 6 | bit), prev & b == 0);
            }
            s = bs + ((s - bs + 1) & m);
        }
    }
    /// Per slot, the rank of its first new state (a prefix over `new`).
    fn renum_prepare(&self, threads: usize) -> Option<usize> {
        let n = self.slots.len();
        let per = n.div_ceil(threads).max(1);
        let counts: Vec<u32> = std::thread::scope(|s| {
            let hs: Vec<_> = (0..threads).map(|t| s.spawn(move || (t * per..((t + 1) * per).min(n)).map(|i| self.slots.at(i).new.load(Ordering::Relaxed).count_ones()).sum::<u32>())).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let mut off = vec![0u32; n];
        let op = off.as_mut_ptr() as usize;
        std::thread::scope(|s| {
            let mut start = 0u32;
            for t in 0..threads {
                let a = start;
                start += counts[t];
                s.spawn(move || {
                    let mut r = a;
                    for i in t * per..((t + 1) * per).min(n) {
                        unsafe { (op as *mut u32).add(i).write(r) };
                        r += self.slots.at(i).new.load(Ordering::Relaxed).count_ones();
                    }
                });
            }
        });
        let total = counts.iter().sum::<u32>() as usize;
        unsafe { *self.newoff.get() = off };
        Some(total)
    }
    #[inline(always)]
    fn renum_index(&self, prov: u32, n_door: u32) -> usize {
        let p = prov - n_door;
        let (s, bit) = ((p >> 6) as usize, p & 63);
        let nw = self.slots.at(s).new.load(Ordering::Relaxed);
        debug_assert!(nw & 1 << bit != 0);
        unsafe { (&*self.newoff.get())[s] as usize + (nw & ((1u64 << bit) - 1)).count_ones() as usize }
    }
    fn set_id(&self, _shard: u32, _key: u128, _id: u32) {
        unreachable!("posmask8 ids are positional; see end_frame")
    }
    /// Fold `new` into `old`, re-rank in (group, slot) order, rebuild the id
    /// map: an old state keeps its id, a new one gets its canonical id.
    fn end_frame(&self, new: &[NewState], renum: &(dyn Fn(u32) -> u32 + Sync), _n_door: u32, threads: usize) {
        let n = self.slots.len();
        let per = n.div_ceil(threads).max(1);
        // Ranks: per part, its states; then each part's start.
        let counts: Vec<u32> = std::thread::scope(|s| {
            let hs: Vec<_> = (0..threads).map(|t| s.spawn(move || (t * per..((t + 1) * per).min(n)).map(|i| { let e = self.slots.at(i); (e.old.load(Ordering::Relaxed) | e.new.load(Ordering::Relaxed)).count_ones() }).sum::<u32>())).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let total: u32 = counts.iter().sum();
        assert_eq!(total as usize, self.idmap().len() + new.len());
        let mut out = vec![0u32; total as usize];
        let op = out.as_mut_ptr() as usize;
        let old_map = self.idmap();
        std::thread::scope(|s| {
            let mut start = 0u32;
            for t in 0..threads {
                let a = start;
                start += counts[t];
                s.spawn(move || {
                    let mut r = a;
                    for i in t * per..((t + 1) * per).min(n) {
                        let e = self.slots.at(i);
                        let (o, nw) = (e.old.load(Ordering::Relaxed), e.new.load(Ordering::Relaxed));
                        let base = e.base.load(Ordering::Relaxed);
                        e.base.store(r, Ordering::Relaxed);
                        let mut all = o | nw;
                        while all != 0 {
                            let bit = all.trailing_zeros();
                            all &= all - 1;
                            let b = 1u64 << bit;
                            let id = if o & b != 0 { old_map[(base + (o & (b - 1)).count_ones()) as usize] } else { renum(self.n_door + ((i as u32) << 6 | bit)) };
                            unsafe { (op as *mut u32).add(r as usize).write(id) };
                            r += 1;
                        }
                        e.old.store(o | nw, Ordering::Relaxed);
                        e.new.store(0, Ordering::Relaxed);
                    }
                });
            }
        });
        unsafe { *self.idmap.get() = out };
    }
    fn bytes(&self) -> usize {
        self.slots.bytes() + self.idmap().len() * 4
    }
    fn name(&self) -> &'static str {
        "pm8"
    }
}
