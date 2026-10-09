//! Approach #1: many misses in flight per thread (DESIGNS.md "MLP").
//!
//! The single-threaded probe loops (`v4`, `bitcell`) wait for one DRAM miss
//! at a time. Here the SAME tables are driven three ways, all with exactly the
//! sequential order's decisions and ids:
//! - `seq`: the plain loop (sanity: must time as `v4` / `bitcell`);
//! - `gG` group prefetching (Chen et al. 2004): for G lookups compute the home
//!   line(s) and prefetch, then run the ordinary sequential probe on each;
//! - `aK` AMAC-style rolling interleave (Kocberber et al. 2015): a ring of K
//!   in-flight lookups, each a small state machine (prefetched -> scanned ->
//!   resolved), committed strictly in query order.
//!
//! Tables (`TABLE` below): `v4`, `bitcell`, `posmask4` / `posmask8` and
//! `bitintern` as in main.rs / bits.rs (linear probing over 12-B packed / 8-B
//! slots; posmask is bitcell's code over (shape, region) directories), and
//! `v4b` / `bitcellb` / `posmask{4,8}b`: the same keys in 64-B buckets of 5
//! entries probed with one AVX-512 masked compare (keys are 8-B fingerprints /
//! 4-B entry keys, so they are their own tags: a separate Swiss-table
//! control-byte array would add a second line per lookup).
//!
//! Exactness. A scan is READ-ONLY and may run before earlier lookups commit.
//! It stops at a matching key (Hit) or at the first empty slot (Empty). Slots
//! are never deleted or moved and a key, once written, never changes, so:
//! - Hit(s) stays valid: the slots before s on the chain stay non-matching;
//! - Empty(s) is re-checked at commit: s still empty -> insert there (the
//!   sequential probe would stop there too); s now holds OUR key (an earlier
//!   in-flight lookup inserted the same new state: an in-group duplicate) ->
//!   a hit; s holds another key -> continue the sequential probe after s.
//! Mutable payload (v4's id is written once with the key; bitcell's mask
//! word) is only read and written at commit, in order. Chains longer than the
//! prefetched line(s): the scan stops at the first slot outside them and the
//! lookup re-prefetches and waits for its next turn (AMAC), or takes the
//! demand miss in the sequential probe (group prefetching).

use std::arch::x86_64::*;
use std::time::Instant;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::bits::{slot_of, QB};
use crate::Q;

#[derive(Clone, Copy)]
pub enum Found { Hit(u32), Empty(u32) }

/// A probe position: the shard's (base, mask), the relative slot / bucket, and
/// the cache lines [lo, hi] already prefetched for it.
#[derive(Clone, Copy, Default)]
pub struct Pos { base: u32, mask: u32, i: u32, lo: usize, hi: usize }

pub(crate) trait Table {
    type Q: Copy;
    fn edge(q: &Self::Q) -> (u32, u32);
    /// The home position, its lines recorded in `lo..=hi`.
    fn home(&self, q: &Self::Q) -> Pos;
    /// Scan read-only within the prefetched lines; `None`: the chain left them
    /// (`p` moved to the next slot / bucket, its lines in `lo..=hi`).
    fn scan(&self, q: &Self::Q, p: &mut Pos) -> Option<Found>;
    /// In query order: the decision and id for a scan's result.
    fn commit(&mut self, q: &Self::Q, f: Found, next: &mut u32) -> (u32, bool);
    /// The sequential probe (the baseline loop's).
    fn probe(&mut self, q: &Self::Q, next: &mut u32) -> (u32, bool);
    fn bytes(&self) -> usize;
    /// "table on huge pages X of Y GB" for the timed structure.
    fn hp(&self) -> String;
    /// A copy on fresh (huge-advised) pages, for the next mode of one run.
    fn dup(&self) -> Self;
    /// The frame-start set, in door order; returns the door states' ids.
    fn load_door(&mut self, door: &[Self::Q]) -> Vec<u32> {
        let mut next = 0u32;
        door.iter().map(|q| { let (id, new) = self.probe(q, &mut next); assert!(new, "a door state twice"); id }).collect()
    }
}

fn huge_clone<T: Copy>(v: &[T]) -> Vec<T> { let mut c = crate::huge_cap(v.len()); c.extend_from_slice(v); c }

#[inline(always)]
fn prefetch(p: &Pos) {
    let mut l = p.lo;
    while l <= p.hi { unsafe { _mm_prefetch::<_MM_HINT_T0>((l << 6) as *const i8) }; l += 1; }
}

#[inline(always)]
fn lines_of<T>(e: *const T) -> (usize, usize) {
    let a = e as usize;
    (a >> 6, (a + std::mem::size_of::<T>() - 1) >> 6)
}

#[inline(always)]
fn fp_of(k: u128) -> u64 { let f = (k >> 64) as u64; if f == 0 { 1 } else { f } }

// ---------------------------------------------------------------------------
// v4: (fingerprint u64, id u32) in 12-B packed slots, linear probing, load 0.8.

#[repr(C, packed)]
#[derive(Clone, Copy)]
struct S { fp: u64, id: u32 }

pub(crate) struct V4 { tab: Vec<S>, off: Vec<(u32, u32)> }

impl V4 {
    pub fn new(cnt: &[u64], load: f64) -> V4 {
        let mut off = Vec::with_capacity(cnt.len());
        let mut total = 0u64;
        for &c in cnt { let cap = ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
        assert!(total < u32::MAX as u64);
        V4 { tab: crate::huge_vec(total as usize, S { fp: 0, id: 0 }), off }
    }
    #[inline(always)]
    fn probe_from(&mut self, f: u64, base: u32, mask: u32, mut i: u32, next: &mut u32) -> (u32, bool) {
        loop {
            let sl = &mut self.tab[(base + i) as usize];
            let g = sl.fp;
            if g == f { return (sl.id, false); }
            if g == 0 { *sl = S { fp: f, id: *next }; *next += 1; return (*next - 1, true); }
            i = (i + 1) & mask;
        }
    }
}

impl Table for V4 {
    type Q = Q;
    #[inline(always)] fn edge(q: &Q) -> (u32, u32) { (q.src, q.xfer) }
    #[inline(always)]
    fn home(&self, q: &Q) -> Pos {
        let (base, mask) = self.off[q.shard as usize];
        let i = (q.key as u64 as u32) & mask;
        let (lo, hi) = lines_of(unsafe { self.tab.as_ptr().add((base + i) as usize) });
        Pos { base, mask, i, lo, hi }
    }
    #[inline(always)]
    fn scan(&self, q: &Q, p: &mut Pos) -> Option<Found> {
        let f = fp_of(q.key);
        loop {
            let s = p.base + p.i;
            let e = unsafe { self.tab.as_ptr().add(s as usize) };
            let (lo, hi) = lines_of(e);
            if lo < p.lo || hi > p.hi { p.lo = lo; p.hi = hi; return None; }
            let g = unsafe { (*e).fp };
            if g == f { return Some(Found::Hit(s)); }
            if g == 0 { return Some(Found::Empty(s)); }
            p.i = (p.i + 1) & p.mask;
        }
    }
    #[inline(always)]
    fn commit(&mut self, q: &Q, fd: Found, next: &mut u32) -> (u32, bool) {
        match fd {
            Found::Hit(s) => (self.tab[s as usize].id, false),
            Found::Empty(s) => {
                let f = fp_of(q.key);
                let (base, mask) = self.off[q.shard as usize];
                self.probe_from(f, base, mask, s - base, next)
            }
        }
    }
    #[inline(always)]
    fn probe(&mut self, q: &Q, next: &mut u32) -> (u32, bool) {
        let (base, mask) = self.off[q.shard as usize];
        self.probe_from(fp_of(q.key), base, mask, (q.key as u64 as u32) & mask, next)
    }
    fn bytes(&self) -> usize { self.tab.len() * 12 }
    fn hp(&self) -> String { crate::hp_at(&self.tab) }
    fn dup(&self) -> Self { V4 { tab: huge_clone(&self.tab), off: self.off.clone() } }
}

// ---------------------------------------------------------------------------
// v4b: v4's keys in 64-B buckets: 5 fingerprints, 5 ids; AVX-512 compare.

#[repr(C, align(64))]
#[derive(Clone, Copy)]
struct B5 { fp: [u64; 5], id: [u32; 5], _p: u32 }

pub(crate) struct V4B { tab: Vec<B5>, off: Vec<(u32, u32)> }

#[inline(always)]
unsafe fn match5_u64(b: *const B5, f: u64) -> (u32, u32) {
    let v = _mm512_load_si512(b as *const __m512i);
    (_mm512_mask_cmpeq_epu64_mask(0x1f, v, _mm512_set1_epi64(f as i64)) as u32,
     _mm512_mask_cmpeq_epu64_mask(0x1f, v, _mm512_setzero_si512()) as u32)
}

impl V4B {
    pub fn new(cnt: &[u64], load: f64) -> V4B {
        let mut off = Vec::with_capacity(cnt.len());
        let mut total = 0u64;
        for &c in cnt { let cap = ((c.max(1) as f64 / (5.0 * load)).ceil() as u64).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
        assert!(total * 5 < u32::MAX as u64);
        V4B { tab: crate::huge_vec(total as usize, B5 { fp: [0; 5], id: [0; 5], _p: 0 }), off }
    }
    #[inline(always)]
    fn probe_from(&mut self, f: u64, base: u32, mask: u32, mut i: u32, next: &mut u32) -> (u32, bool) {
        loop {
            let b = &mut self.tab[(base + i) as usize];
            let (hit, emp) = unsafe { match5_u64(b, f) };
            if hit != 0 { return (b.id[hit.trailing_zeros() as usize], false); }
            if emp != 0 { let j = emp.trailing_zeros() as usize; b.fp[j] = f; b.id[j] = *next; *next += 1; return (*next - 1, true); }
            i = (i + 1) & mask;
        }
    }
}

impl Table for V4B {
    type Q = Q;
    #[inline(always)] fn edge(q: &Q) -> (u32, u32) { (q.src, q.xfer) }
    #[inline(always)]
    fn home(&self, q: &Q) -> Pos {
        let (base, mask) = self.off[q.shard as usize];
        let i = (q.key as u64 as u32) & mask;
        let (lo, hi) = lines_of(unsafe { self.tab.as_ptr().add((base + i) as usize) });
        Pos { base, mask, i, lo, hi }
    }
    #[inline(always)]
    fn scan(&self, q: &Q, p: &mut Pos) -> Option<Found> {
        let f = fp_of(q.key);
        loop {
            let b = unsafe { self.tab.as_ptr().add((p.base + p.i) as usize) };
            let l = b as usize >> 6;
            if l < p.lo || l > p.hi { p.lo = l; p.hi = l; return None; }
            let (hit, emp) = unsafe { match5_u64(b, f) };
            let s = (p.base + p.i) * 5;
            if hit != 0 { return Some(Found::Hit(s + hit.trailing_zeros())); }
            if emp != 0 { return Some(Found::Empty(s + emp.trailing_zeros())); }
            p.i = (p.i + 1) & p.mask;
        }
    }
    #[inline(always)]
    fn commit(&mut self, q: &Q, fd: Found, next: &mut u32) -> (u32, bool) {
        match fd {
            Found::Hit(s) => (self.tab[(s / 5) as usize].id[(s % 5) as usize], false),
            Found::Empty(s) => {
                let (base, mask) = self.off[q.shard as usize];
                self.probe_from(fp_of(q.key), base, mask, s / 5 - base, next)
            }
        }
    }
    #[inline(always)]
    fn probe(&mut self, q: &Q, next: &mut u32) -> (u32, bool) {
        let (base, mask) = self.off[q.shard as usize];
        self.probe_from(fp_of(q.key), base, mask, (q.key as u64 as u32) & mask, next)
    }
    fn bytes(&self) -> usize { self.tab.len() * 64 }
    fn hp(&self) -> String { crate::hp_at(&self.tab) }
    fn dup(&self) -> Self { V4B { tab: huge_clone(&self.tab), off: self.off.clone() } }
}

// ---------------------------------------------------------------------------
// bitcell: (entry key u32, mask word u64) in 12-B packed slots, linear
// probing; the id is slot << 6 | bit.

#[repr(C, packed)]
#[derive(Clone, Copy)]
struct W { hk: u32, bits: u64 }

pub struct BitCell { tab: Vec<W>, off: Vec<(u32, u32)> }

impl BitCell {
    pub fn new(pcnt: &[u64]) -> BitCell {
        let cap_of = |c: u64, load: f64| ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two();
        let load = if pcnt.iter().map(|&c| cap_of(c, 0.5)).sum::<u64>() * 64 >= 1 << 32 { 0.8 } else { 0.5 };
        let mut off = Vec::with_capacity(pcnt.len());
        let mut total = 0u64;
        for &c in pcnt { let cap = cap_of(c, load); off.push((total as u32, (cap - 1) as u32)); total += cap; }
        assert!(total * 64 < 1 << 32, "ids slot << 6 | bit must fit 32 bits");
        BitCell { tab: crate::huge_vec(total as usize, W { hk: 0, bits: 0 }), off }
    }
    #[inline(always)]
    fn set(&mut self, s: u32, bit: u32) -> (u32, bool) {
        let e = &mut self.tab[s as usize];
        let (w, b) = (e.bits, 1u64 << bit);
        let new = w & b == 0;
        if new { e.bits = w | b; }
        (s << 6 | bit, new)
    }
    #[inline(always)]
    fn find_from(&mut self, hk: u32, base: u32, mask: u32, mut s: u32) -> u32 {
        loop { let e = &mut self.tab[s as usize]; let k = e.hk; if k == hk { return s; } if k == 0 { e.hk = hk; return s; } s = base + ((s - base + 1) & mask); }
    }
}

impl Table for BitCell {
    type Q = QB;
    #[inline(always)] fn edge(q: &QB) -> (u32, u32) { (q.src, q.xfer) }
    #[inline(always)]
    fn home(&self, q: &QB) -> Pos {
        let (base, mask) = self.off[q.shard as usize];
        let s = slot_of(q.high, (base, mask));
        let (lo, hi) = lines_of(unsafe { self.tab.as_ptr().add(s as usize) });
        Pos { base, mask, i: s - base, lo, hi }
    }
    #[inline(always)]
    fn scan(&self, q: &QB, p: &mut Pos) -> Option<Found> {
        loop {
            let s = p.base + p.i;
            let e = unsafe { self.tab.as_ptr().add(s as usize) };
            let (lo, hi) = lines_of(e);
            if lo < p.lo || hi > p.hi { p.lo = lo; p.hi = hi; return None; }
            let k = unsafe { (*e).hk };
            if k == q.high { return Some(Found::Hit(s)); }
            if k == 0 { return Some(Found::Empty(s)); }
            p.i = (p.i + 1) & p.mask;
        }
    }
    #[inline(always)]
    fn commit(&mut self, q: &QB, fd: Found, _next: &mut u32) -> (u32, bool) {
        let s = match fd {
            Found::Hit(s) => s,
            Found::Empty(s) => { let (base, mask) = self.off[q.shard as usize]; self.find_from(q.high, base, mask, s) }
        };
        self.set(s, q.low)
    }
    #[inline(always)]
    fn probe(&mut self, q: &QB, _next: &mut u32) -> (u32, bool) {
        let (base, mask) = self.off[q.shard as usize];
        let s = self.find_from(q.high, base, mask, slot_of(q.high, (base, mask)));
        self.set(s, q.low)
    }
    fn bytes(&self) -> usize { self.tab.len() * 12 }
    fn hp(&self) -> String { crate::hp_at(&self.tab) }
    fn dup(&self) -> Self { BitCell { tab: huge_clone(&self.tab), off: self.off.clone() } }
}

// ---------------------------------------------------------------------------
// bitcellb: bitcell's entries in 64-B buckets: 5 keys, 5 mask words.

#[repr(C, align(64))]
#[derive(Clone, Copy)]
struct WB { hk: [u32; 5], _p: u32, bits: [u64; 5] }

pub struct BitCellB { tab: Vec<WB>, off: Vec<(u32, u32)> }

#[inline(always)]
unsafe fn match5_u32(b: *const WB, k: u32) -> (u32, u32) {
    let v = _mm512_load_si512(b as *const __m512i);
    (_mm512_mask_cmpeq_epi32_mask(0x1f, v, _mm512_set1_epi32(k as i32)) as u32,
     _mm512_mask_cmpeq_epi32_mask(0x1f, v, _mm512_setzero_si512()) as u32)
}

impl BitCellB {
    pub fn new(pcnt: &[u64], load: f64) -> BitCellB {
        let mut off = Vec::with_capacity(pcnt.len());
        let mut total = 0u64;
        for &c in pcnt { let cap = ((c.max(1) as f64 / (5.0 * load)).ceil() as u64).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
        assert!(total * 5 * 64 < 1 << 32, "ids slot << 6 | bit must fit 32 bits");
        BitCellB { tab: crate::huge_vec(total as usize, WB { hk: [0; 5], _p: 0, bits: [0; 5] }), off }
    }
    #[inline(always)]
    fn set(&mut self, s: u32, bit: u32) -> (u32, bool) {
        let w = &mut self.tab[(s / 5) as usize].bits[(s % 5) as usize];
        let b = 1u64 << bit;
        let new = *w & b == 0;
        if new { *w |= b; }
        (s << 6 | bit, new)
    }
    #[inline(always)]
    fn find_from(&mut self, hk: u32, base: u32, mask: u32, mut i: u32) -> u32 {
        loop {
            let b = &mut self.tab[(base + i) as usize];
            let (hit, emp) = unsafe { match5_u32(b, hk) };
            if hit != 0 { return (base + i) * 5 + hit.trailing_zeros(); }
            if emp != 0 { let j = emp.trailing_zeros(); b.hk[j as usize] = hk; return (base + i) * 5 + j; }
            i = (i + 1) & mask;
        }
    }
}

impl Table for BitCellB {
    type Q = QB;
    #[inline(always)] fn edge(q: &QB) -> (u32, u32) { (q.src, q.xfer) }
    #[inline(always)]
    fn home(&self, q: &QB) -> Pos {
        let (base, mask) = self.off[q.shard as usize];
        let i = slot_of(q.high, (0, mask));
        let (lo, hi) = lines_of(unsafe { self.tab.as_ptr().add((base + i) as usize) });
        Pos { base, mask, i, lo, hi }
    }
    #[inline(always)]
    fn scan(&self, q: &QB, p: &mut Pos) -> Option<Found> {
        loop {
            let b = unsafe { self.tab.as_ptr().add((p.base + p.i) as usize) };
            let l = b as usize >> 6;
            if l < p.lo || l > p.hi { p.lo = l; p.hi = l; return None; }
            let (hit, emp) = unsafe { match5_u32(b, q.high) };
            let s = (p.base + p.i) * 5;
            if hit != 0 { return Some(Found::Hit(s + hit.trailing_zeros())); }
            if emp != 0 { return Some(Found::Empty(s + emp.trailing_zeros())); }
            p.i = (p.i + 1) & p.mask;
        }
    }
    #[inline(always)]
    fn commit(&mut self, q: &QB, fd: Found, _next: &mut u32) -> (u32, bool) {
        let s = match fd {
            Found::Hit(s) => s,
            Found::Empty(s) => {
                let (bi, j) = (s / 5, s % 5);
                let k = self.tab[bi as usize].hk[j as usize];
                if k == 0 { self.tab[bi as usize].hk[j as usize] = q.high; s }
                else if k == q.high { s }
                else { let (base, mask) = self.off[q.shard as usize]; self.find_from(q.high, base, mask, bi - base) }
            }
        };
        self.set(s, q.low)
    }
    #[inline(always)]
    fn probe(&mut self, q: &QB, _next: &mut u32) -> (u32, bool) {
        let (base, mask) = self.off[q.shard as usize];
        let s = self.find_from(q.high, base, mask, slot_of(q.high, (0, mask)));
        self.set(s, q.low)
    }
    fn bytes(&self) -> usize { self.tab.len() * 64 }
    fn hp(&self) -> String { crate::hp_at(&self.tab) }
    fn dup(&self) -> Self { BitCellB { tab: huge_clone(&self.tab), off: self.off.clone() } }
}

// ---------------------------------------------------------------------------
// bitintern: `bits`' directory (high digits + 1, 8-B entries, linear probing)
// with the 128-bit spd.y mask interned: entry = (key, mask id); the masks in
// one small hash-consed table. The id is slot << 7 | low.

#[repr(C)]
#[derive(Clone, Copy)]
struct EI { hk: u32, set: u32 }

pub struct BitIntern { tab: Vec<EI>, off: Vec<(u32, u32)>, masks: Vec<u128>, intern: FxHashMap<u128, u32> }

impl BitIntern {
    pub fn new(pcnt: &[u64]) -> BitIntern {
        let mut off = Vec::with_capacity(pcnt.len());
        let mut total = 0u64;
        for &c in pcnt { let cap = (2 * c.max(1)).next_power_of_two(); off.push((total as u32, (cap - 1) as u32)); total += cap; }
        assert!(total * 128 < 1 << 32);
        let mut intern = FxHashMap::default();
        intern.insert(0u128, 0u32);
        BitIntern { tab: crate::huge_vec(total as usize, EI { hk: 0, set: 0 }), off, masks: vec![0], intern }
    }
    #[inline(always)]
    fn find_from(&mut self, hk: u32, base: u32, mask: u32, mut s: u32) -> u32 {
        loop { let e = &mut self.tab[s as usize]; if e.hk == hk { return s; } if e.hk == 0 { e.hk = hk; return s; } s = base + ((s - base + 1) & mask); }
    }
    #[inline(always)]
    fn set(&mut self, s: u32, low: u32) -> (u32, bool) {
        let e = &mut self.tab[s as usize];
        let m = self.masks[e.set as usize];
        let b = 1u128 << low;
        let new = m & b == 0;
        if new {
            let nm = m | b;
            let n = self.masks.len() as u32;
            let masks = &mut self.masks;
            e.set = *self.intern.entry(nm).or_insert_with(|| { masks.push(nm); n });
        }
        (s << 7 | low, new)
    }
}

impl Table for BitIntern {
    type Q = QB;
    #[inline(always)] fn edge(q: &QB) -> (u32, u32) { (q.src, q.xfer) }
    #[inline(always)]
    fn home(&self, q: &QB) -> Pos {
        let (base, mask) = self.off[q.shard as usize];
        let s = slot_of(q.high + 1, (base, mask));
        let (lo, hi) = lines_of(unsafe { self.tab.as_ptr().add(s as usize) });
        Pos { base, mask, i: s - base, lo, hi }
    }
    #[inline(always)]
    fn scan(&self, q: &QB, p: &mut Pos) -> Option<Found> {
        let hk = q.high + 1;
        loop {
            let s = p.base + p.i;
            let e = unsafe { self.tab.as_ptr().add(s as usize) };
            let (lo, hi) = lines_of(e);
            if lo < p.lo || hi > p.hi { p.lo = lo; p.hi = hi; return None; }
            let k = unsafe { (*e).hk };
            if k == hk { return Some(Found::Hit(s)); }
            if k == 0 { return Some(Found::Empty(s)); }
            p.i = (p.i + 1) & p.mask;
        }
    }
    #[inline(always)]
    fn commit(&mut self, q: &QB, fd: Found, _next: &mut u32) -> (u32, bool) {
        let s = match fd {
            Found::Hit(s) => s,
            Found::Empty(s) => { let (base, mask) = self.off[q.shard as usize]; self.find_from(q.high + 1, base, mask, s) }
        };
        self.set(s, q.low)
    }
    #[inline(always)]
    fn probe(&mut self, q: &QB, _next: &mut u32) -> (u32, bool) {
        let (base, mask) = self.off[q.shard as usize];
        let s = self.find_from(q.high + 1, base, mask, slot_of(q.high + 1, (base, mask)));
        self.set(s, q.low)
    }
    fn bytes(&self) -> usize { self.tab.len() * 8 }
    fn hp(&self) -> String { crate::hp_at(&self.tab) }
    fn dup(&self) -> Self { BitIntern { tab: huge_clone(&self.tab), off: self.off.clone(), masks: self.masks.clone(), intern: self.intern.clone() } }
    /// As `run_intern`: the door's masks accumulated per entry, then interned
    /// in arena order (no garbage masks at the frame start).
    fn load_door(&mut self, door: &[QB]) -> Vec<u32> {
        let mut acc: Vec<u128> = vec![0; self.tab.len()];
        let ids = door.iter().map(|q| {
            let (base, mask) = self.off[q.shard as usize];
            let s = self.find_from(q.high + 1, base, mask, slot_of(q.high + 1, (base, mask)));
            let b = 1u128 << q.low;
            assert!(acc[s as usize] & b == 0, "a door state twice");
            acc[s as usize] |= b;
            s << 7 | q.low
        }).collect();
        for (s, m) in acc.iter().enumerate() {
            if *m != 0 { let n = self.masks.len() as u32; let masks = &mut self.masks; self.tab[s].set = *self.intern.entry(*m).or_insert_with(|| { masks.push(*m); n }); }
        }
        ids
    }
}

// ---------------------------------------------------------------------------
// The drivers.

#[derive(Clone, Copy, Debug)]
pub enum Mode { Seq, Group(usize), Amac(usize), Pipe(usize) }

fn parse_mode(m: &str) -> Mode {
    if m == "seq" { return Mode::Seq; }
    let n: usize = m[1..].parse().unwrap_or_else(|_| panic!("mode {m}: seq, gN, aN or pN"));
    match &m[..1] {
        "g" => Mode::Group(n),
        "a" => { assert!(n.is_power_of_two(), "AMAC ring size must be a power of two"); Mode::Amac(n) }
        "p" => Mode::Pipe(n),
        _ => panic!("mode {m}: seq, gN, aN or pN"),
    }
}

pub struct Out { pub edges: Vec<(u32, u32, u32)>, frontier: Vec<u8>, pub new: u64, pub next: u32, pub relines: u64 }

impl Out {
    #[inline(always)]
    fn emit(&mut self, (src, xfer): (u32, u32), (id, new): (u32, bool)) {
        if new { self.new += 1; self.frontier.extend_from_slice(&[0u8; crate::PAYLOAD]); }
        self.edges.push((src, id, xfer));
    }
}

#[inline(never)]
fn run_seq<T: Table>(t: &mut T, qs: &[T::Q], o: &mut Out) {
    for q in qs { let r = t.probe(q, &mut o.next); o.emit(T::edge(q), r); }
}

#[inline(never)]
fn run_group<T: Table>(t: &mut T, qs: &[T::Q], g: usize, o: &mut Out) {
    for chunk in qs.chunks(g) {
        for q in chunk { prefetch(&t.home(q)); }
        for q in chunk { let r = t.probe(q, &mut o.next); o.emit(T::edge(q), r); }
    }
}

/// Rolling prefetch at distance D (software pipelining): before probing
/// lookup i, prefetch lookup i + D's home. What AMAC with in-order commit
/// reduces to when nearly every lookup resolves on its first line.
#[inline(never)]
fn run_pipe<T: Table>(t: &mut T, qs: &[T::Q], d: usize, o: &mut Out) {
    for q in &qs[..d.min(qs.len())] { prefetch(&t.home(q)); }
    for i in 0..qs.len() {
        if let Some(a) = qs.get(i + d) { prefetch(&t.home(a)); }
        let q = &qs[i];
        let r = t.probe(q, &mut o.next);
        o.emit(T::edge(q), r);
    }
}

const PEND: u8 = 1;
const DONE: u8 = 2;

#[derive(Clone, Copy)]
struct Slot { qi: usize, st: u8, p: Pos, f: Found }

#[inline(never)]
fn run_amac<T: Table>(t: &mut T, qs: &[T::Q], k: usize, o: &mut Out) {
    let n = qs.len();
    let km = k - 1;
    let mut ring = vec![Slot { qi: 0, st: 0, p: Pos::default(), f: Found::Hit(0) }; k];
    let mut nq = 0;
    for r in ring.iter_mut() {
        if nq < n { r.qi = nq; r.p = t.home(&qs[nq]); prefetch(&r.p); r.st = PEND; nq += 1; }
    }
    let (mut head, mut cur, mut done) = (0usize, 0usize, 0usize);
    let mut relines = 0u64;
    while done < n {
        let r = &mut ring[cur];
        if r.st == PEND {
            match t.scan(&qs[r.qi], &mut r.p) {
                Some(f) => { r.f = f; r.st = DONE; }
                None if cur == head => {
                    // The head blocks every commit: finish its chain now
                    // (demand misses) rather than after a whole rotation.
                    relines += 1;
                    r.p.lo = 0; r.p.hi = usize::MAX;
                    r.f = t.scan(&qs[r.qi], &mut r.p).unwrap();
                    r.st = DONE;
                }
                None => { prefetch(&r.p); relines += 1; }
            }
        }
        while ring[head].st == DONE {
            let r = &mut ring[head];
            let q = &qs[r.qi];
            let res = t.commit(q, r.f, &mut o.next);
            o.emit(T::edge(q), res);
            done += 1;
            if nq < n { r.qi = nq; r.p = t.home(&qs[nq]); prefetch(&r.p); r.st = PEND; nq += 1; } else { r.st = 0; }
            head = (head + 1) & km;
        }
        cur = (cur + 1) & km;
    }
    o.relines = relines;
}

/// One table, every requested mode. The door is inserted once (untimed) and
/// its input list freed; each mode then runs on a fresh copy (none for the
/// last), so a one-mode run (bench.sh) holds no untimed precompute but `qs`.
/// Timed: the drops + the lookups. Checked: v4's ids against v3c's exactly
/// (`exact`), every table's decisions and id bijection (bits::check).
#[allow(clippy::too_many_arguments)]
fn run_modes<T: Table>(tag: &str, modes: &[Mode], mut t: T, door: Vec<T::Q>, qs: &[T::Q], ds: &[u32], n_src: usize, n_door: usize, pd: &str, exact: bool) {
    let t0 = Instant::now();
    let door_ids = t.load_door(&door);
    drop(door);
    let old: FxHashSet<u32> = if exact { assert!(door_ids.iter().enumerate().all(|(i, &id)| id as usize == i)); Default::default() } else { door_ids.iter().copied().collect() };
    drop(door_ids);
    eprintln!("[mlp {tag}] table {:.2} GB, door inserted in {:.1} s", t.bytes() as f64 / 1e9, t0.elapsed().as_secs_f64());
    let mut pristine = Some(t);
    for (mi, &m) in modes.iter().enumerate() {
        let mut t = if mi + 1 == modes.len() { pristine.take().unwrap() } else { pristine.as_ref().unwrap().dup() };
        let mut o = Out { edges: crate::huge_cap(qs.len()), frontier: crate::huge_cap(7_000_000 * crate::PAYLOAD), new: 0, next: n_door as u32, relines: 0 };
        let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
        let before = crate::hp();
        crate::perf_on(true);
        let tt = Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        let t_drops = tt.elapsed().as_secs_f64();
        match m { Mode::Seq => run_seq(&mut t, qs, &mut o), Mode::Group(g) => run_group(&mut t, qs, g, &mut o), Mode::Amac(k) => run_amac(&mut t, qs, k, &mut o), Mode::Pipe(d) => run_pipe(&mut t, qs, d, &mut o) }
        let dt = tt.elapsed().as_secs_f64();
        crate::perf_on(false);
        std::hint::black_box((&o.frontier, &dropmin));
        eprintln!("[mlp {tag} {m:?}] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {} ({}), {:.1} ns per query; re-prefetches (chain left its lines) {} ({:.4} per lookup); {}; before: {before}; after: {}",
            o.new, if o.new == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qs.len() as f64, o.relines, o.relines as f64 / qs.len() as f64, t.hp(), crate::hp());
        if std::env::var_os("BENCH_NOCHECK").is_some() { continue; }
        drop(t);
        if exact {
            let rm = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/refid.bin")).unwrap()).unwrap() };
            let refid: &[u32] = crate::from_bytes(&rm);
            let diff = o.edges.iter().zip(refid).filter(|(e, &r)| e.1 != r).count();
            eprintln!("[mlp {tag} {m:?}] ids against v3c's (refid.bin), exactly: {diff} differ of {}", o.edges.len());
        }
        let is_old = |id: u32| if exact { (id as usize) < n_door } else { old.contains(&id) };
        crate::bits::check(&format!("mlp {tag} {m:?}"), pd, &o.edges, n_door, &is_old, false);
    }
}

fn read_vec<T: Copy>(path: &str) -> Vec<T> {
    use std::io::Read;
    let mut f = std::fs::File::open(path).unwrap();
    let n = f.metadata().unwrap().len() as usize / std::mem::size_of::<T>();
    let mut v: Vec<T> = crate::huge_cap(n);
    unsafe {
        f.read_exact(std::slice::from_raw_parts_mut(v.as_mut_ptr() as *mut u8, n * std::mem::size_of::<T>())).unwrap();
        v.set_len(n);
    }
    v
}

/// `bits::words_prep` (~25 s) cached under DIR/mlp/cache/MODE: the queries,
/// the door as QB records, the entries per directory group. Valid while the
/// stamp (size + mtime of every input) matches; the shared prep files are
/// regenerated by other runs, so a changed input recomputes.
fn words_cached(dir: &str, n_door: usize, mode: &str) -> (Vec<QB>, Vec<QB>, Vec<u64>) {
    let cd = format!("{dir}/mlp/cache/{mode}");
    let stamp: String = ["prep/qb.bin", "prep/db.bin", "prep/dshard.bin", "prep/sshape.bin", "prep/scell.bin", "fields/rows.bin", "fields/dicts.txt", "fields/census.txt"].iter().map(|f| {
        let m = std::fs::metadata(format!("{dir}/{f}")).unwrap();
        format!("{f} {} {:?}\n", m.len(), m.modified().unwrap())
    }).collect();
    if std::fs::read_to_string(format!("{cd}/stamp.txt")).ok().as_deref() == Some(stamp.as_str()) {
        eprintln!("[mlp {mode}] words from the cache {cd}");
        return (read_vec(&format!("{cd}/qw.bin")), read_vec(&format!("{cd}/door.bin")), read_vec(&format!("{cd}/pcnt.bin")));
    }
    let p = crate::bits::words_prep(dir, n_door, mode);
    let dq: Vec<QB> = (0..n_door).map(|i| QB { shard: p.dsh[i], src: 0, xfer: 0, low: p.dq[i].1, high: p.dq[i].0, _p: [0; 3] }).collect();
    std::fs::create_dir_all(&cd).unwrap();
    let _ = std::fs::remove_file(format!("{cd}/stamp.txt"));
    std::fs::write(format!("{cd}/qw.bin"), crate::as_bytes(&p.qw)).unwrap();
    std::fs::write(format!("{cd}/door.bin"), crate::as_bytes(&dq)).unwrap();
    std::fs::write(format!("{cd}/pcnt.bin"), crate::as_bytes(&p.pcnt)).unwrap();
    std::fs::write(format!("{cd}/stamp.txt"), &stamp).unwrap();
    eprintln!("[mlp {mode}] words computed and cached in {cd}");
    let crate::bits::WordsPrep { qw, pcnt, .. } = p;
    (qw, dq, pcnt)
}

/// `dedup-bench DIR mlp TABLE MODES`: TABLE v4 | v4b | bitcell | bitcellb |
/// posmask4 | posmask4b | posmask8 | posmask8b | bitintern (`...b`: the
/// bucketized SIMD layout of the same keys); MODES comma-separated seq | gG | aK (e.g.
/// `seq,g8,g16,g32,a8,a16,a32`).
pub fn main(dir: &str, door: &[u8], table: &str, modes: &str) {
    let t = Instant::now();
    let modes: Vec<Mode> = modes.split(',').map(parse_mode).collect();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let dm = map("d.bin");
    let ds: &[u32] = crate::from_bytes(&dm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let n_door = door.len() / 40;
    match table {
        "v4" | "v4b" => {
            let (qm, cm, sm) = (map("q.bin"), map("cnt.bin"), map("dshard.bin"));
            let qs: &[Q] = crate::from_bytes(&qm); let cnt: &[u64] = crate::from_bytes(&cm); let dsh: &[u32] = crate::from_bytes(&sm);
            let key_of = |i: usize| (u64::from_le_bytes(door[i * 40 + 16..i * 40 + 24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(door[i * 40 + 24..i * 40 + 32].try_into().unwrap()) as u128) << 64);
            let dq: Vec<Q> = (0..n_door).map(|i| Q { shard: dsh[i], src: 0, xfer: 0, _p: 0, key: key_of(i) }).collect();
            eprintln!("[mlp {table}] inputs {:.1} s", t.elapsed().as_secs_f64());
            if table == "v4" { run_modes(table, &modes, V4::new(cnt, 0.8), dq, qs, ds, n_src, n_door, &pd, true); }
            else { run_modes(table, &modes, V4B::new(cnt, 0.8), dq, qs, ds, n_src, n_door, &pd, true); }
        }
        "bitcell" | "bitcellb" | "posmask4" | "posmask4b" | "posmask8" | "posmask8b" => {
            let (qw, dq, pcnt) = words_cached(dir, n_door, table.trim_end_matches('b'));
            eprintln!("[mlp {table}] inputs ({} words, {} directory groups) {:.1} s", table.trim_end_matches('b'), pcnt.len(), t.elapsed().as_secs_f64());
            if !table.ends_with('b') { run_modes(table, &modes, BitCell::new(&pcnt), dq, &qw, ds, n_src, n_door, &pd, false); }
            else { run_modes(table, &modes, BitCellB::new(&pcnt, 0.5), dq, &qw, ds, n_src, n_door, &pd, false); }
        }
        "bitintern" => {
            let (qm, pm, sm, dbm) = (map("qb.bin"), map("pcnt.bin"), map("dshard.bin"), map("db.bin"));
            let qs: &[QB] = crate::from_bytes(&qm); let pcnt: &[u64] = crate::from_bytes(&pm);
            let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dbm);
            assert!(qs.iter().all(|q| q.low < 128));
            let dq: Vec<QB> = (0..n_door).map(|i| QB { shard: dsh[i], src: 0, xfer: 0, low: db[i].1, high: db[i].0, _p: [0; 3] }).collect();
            eprintln!("[mlp {table}] inputs {:.1} s", t.elapsed().as_secs_f64());
            run_modes(table, &modes, BitIntern::new(pcnt), dq, qs, ds, n_src, n_door, &pd, false);
        }
        other => panic!("mlp: unknown table {other}"),
    }
}
