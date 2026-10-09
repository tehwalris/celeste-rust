//! Compact per-position storage (DESIGNS.md "Compact"): fewer bytes a state,
//! so the sweep's live window fits deep in cache.
//!
//! - `QTable`: a QUOTIENTED Robin Hood table per (shape, cell) shard (Cleary's
//!   compact hashing): the key `k < 2^b` is scrambled by a bijection on b bits,
//!   `h = q * cap + home`; a slot stores only `q` and the entry's displacement
//!   (+1, 0 = empty), in 1, 2 or 4 bytes chosen per shard; optional payload
//!   bytes follow the tag. EXACT: `(home, q)` is `h`, and `h` is `k`. Built
//!   once per frame from the frame-start set, so ids (global slot index) are
//!   stable during the frame and the maximal displacement is known.
//! - `NTable`: the frame's NEW entries, non-moving linear probing on the full
//!   key (u32) + payload; id = its global slot.
//! - `prep` (`compactprep`): per-(shape, cell) and room dictionaries, keys
//!   per configuration, the size table, the live windows, the append-only
//!   dictionary census; writes the query streams the timed variants replay.
//! - `run` (`cqa`, `cqb`, `cqc`): the timed loops (the same 257.7M sweep-ordered
//!   lookups as v3c/bits), each validated with `bits::check`.

use rustc_hash::FxHashMap;
use std::io::Write;

pub const OUT: &str = "/var/tmp/emitcap/compact";

// --------------------------------------------------------------- huge pages

extern "C" {
    fn mmap(addr: *mut u8, len: usize, prot: i32, flags: i32, fd: i32, off: i64) -> *mut u8;
    fn munmap(addr: *mut u8, len: usize) -> i32;
    fn madvise(addr: *mut u8, len: usize, advice: i32) -> i32;
}

/// A zeroed anonymous mapping, `madvise(MADV_HUGEPAGE)` before its first
/// touch (THP defrag is `madvise` here: only advised ranges compact on fault).
/// `T` must be plain data for which all-zero bytes are a valid value.
pub struct HugeVec<T: Copy> { ptr: *mut T, len: usize, cap: usize, bytes: usize }

impl<T: Copy> HugeVec<T> {
    pub fn with_capacity(cap: usize) -> Self {
        let bytes = (cap.max(1) * std::mem::size_of::<T>()).div_ceil(2 << 20) * (2 << 20);
        let p = unsafe { mmap(std::ptr::null_mut(), bytes, 3, 0x22, -1, 0) };
        assert!(p as isize != -1, "mmap of {bytes} B failed");
        assert_eq!(unsafe { madvise(p, bytes, 14) }, 0, "madvise(MADV_HUGEPAGE)");
        HugeVec { ptr: p as *mut T, len: 0, cap, bytes }
    }
    pub fn zeroed(n: usize) -> Self { let mut v = Self::with_capacity(n); v.len = n; v }
    #[inline(always)]
    pub fn push(&mut self, x: T) {
        assert!(self.len < self.cap, "HugeVec full");
        unsafe { self.ptr.add(self.len).write(x) };
        self.len += 1;
    }
    pub fn region(&self) -> (usize, usize) { (self.ptr as usize, self.bytes) }
    pub fn bytes_used(&self) -> usize { self.len * std::mem::size_of::<T>() }
}
impl<T: Copy> std::ops::Deref for HugeVec<T> { type Target = [T]; fn deref(&self) -> &[T] { unsafe { std::slice::from_raw_parts(self.ptr, self.len) } } }
impl<T: Copy> std::ops::DerefMut for HugeVec<T> { fn deref_mut(&mut self) -> &mut [T] { unsafe { std::slice::from_raw_parts_mut(self.ptr, self.len) } } }
impl<T: Copy> Drop for HugeVec<T> { fn drop(&mut self) { unsafe { munmap(self.ptr as *mut u8, self.bytes) }; } }

/// How much of the given address ranges (the timed structures) is resident,
/// and on huge pages; plus the process's anonymous totals.
pub fn huge_report(tag: &str, regions: &[(usize, usize)]) {
    let smaps = std::fs::read_to_string("/proc/self/smaps").unwrap_or_default();
    let (mut rss, mut huge, mut cur) = (0u64, 0u64, false);
    for l in smaps.lines() {
        let first = l.split_whitespace().next().unwrap_or("");
        if let Some((a, b)) = first.split_once('-') {
            if let (Ok(a), Ok(b)) = (usize::from_str_radix(a, 16), usize::from_str_radix(b, 16)) {
                cur = regions.iter().any(|&(p, n)| a < p + n && p < b);
                continue;
            }
        }
        if !cur { continue; }
        let kb = |l: &str| l.split_whitespace().nth(1).and_then(|x| x.parse::<u64>().ok()).unwrap_or(0);
        if l.starts_with("Rss:") { rss += kb(l); }
        if l.starts_with("AnonHugePages:") { huge += kb(l); }
    }
    let roll = std::fs::read_to_string("/proc/self/smaps_rollup").unwrap_or_default();
    let get = |k: &str| roll.lines().find(|l| l.starts_with(k)).map(|l| l.split_whitespace().nth(1).unwrap().parse::<u64>().unwrap()).unwrap_or(0);
    eprintln!("[{tag}] hugepages: timed structure {:.0} MB resident, {:.0} MB on huge pages ({:.0}%); process Anonymous {:.0} MB, AnonHugePages {:.0} MB",
        rss as f64 / 1e3, huge as f64 / 1e3, 100.0 * huge as f64 / rss.max(1) as f64, get("Anonymous:") as f64 / 1e3, get("AnonHugePages:") as f64 / 1e3);
}

// ------------------------------------------------------------ the probe

/// A bijection of [0, 2^b) (odd multiplies mod 2^b and right xorshifts).
#[inline(always)]
pub fn scr(k: u32, b: u32) -> u32 {
    let m = ((1u64 << b) - 1) as u32;
    let mut x = k.wrapping_mul(0x9E37_79B1) & m;
    x ^= x >> (b / 2 + 1);
    x = x.wrapping_mul(0x85EB_CA6B) & m;
    x ^ (x >> (b / 3 + 1))
}

/// Lemire's exact division for 32-bit numerators: `M = ceil(2^64 / d)`, d >= 2.
#[inline(always)]
fn fastdiv_m(d: u32) -> u64 { assert!(d >= 2); u64::MAX / d as u64 + 1 }
#[inline(always)]
fn divmod(h: u32, m: u64, d: u32) -> (u32, u32) { let q = ((h as u128 * m as u128) >> 64) as u32; (q, h - q * d) }

fn bitlen(x: u64) -> u32 { 64 - x.leading_zeros() }

#[derive(Clone, Copy, Default, Debug)]
pub struct QShard { pub off: u64, pub poff: u64, pub base: u32, pub cap: u32, pub m: u64, pub b: u8, pub dbits: u8, pub tagw: u8, pub pay: u8 }

/// One shard's layout, chosen at build time: `cap` is the home range, `len`
/// the slots (cap + the overflow past the end; no wrap-around, the last slot
/// is always empty so every scan stops inside the shard).
pub struct Plan { pub cap: u32, pub len: u32, pub tagw: u32, pub dbits: u32, pub maxd: u32, pub sumd: u64, pub tags: Vec<u32> }

pub const LOADS: [f64; 8] = [0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.6, 0.5];

/// The static Robin Hood layout of one shard's DISTINCT keys: among the
/// loads <= `max_load`, the one with the fewest bytes (len x (tag + payload)).
pub fn plan(keys: &[u32], b: u32, pay: u32, max_load: f64) -> Plan {
    let n = keys.len();
    if n == 0 { return Plan { cap: 0, len: 0, tagw: 2, dbits: 0, maxd: 0, sumd: 0, tags: Vec::new() }; }
    let hs: Vec<u32> = keys.iter().map(|&k| { debug_assert!(b == 32 || (k as u64) < 1u64 << b); scr(k, b) }).collect();
    let mut best: Option<(u64, Plan)> = None;
    let mut seen_cap = 0u32;
    for &load in LOADS.iter().filter(|&&l| l <= max_load + 1e-9) {
        let cap = ((n as f64 / load).ceil() as u32).max(2);
        if cap == seen_cap { continue; }
        seen_cap = cap;
        let m = fastdiv_m(cap);
        let mut sl: Vec<(u32, u32)> = vec![(0, 0); cap as usize + 1]; // (h, dist + 1); 0 = empty
        let (mut maxd, mut sumd) = (0u32, 0u64);
        for &h in &hs {
            let (_, home) = divmod(h, m, cap);
            let (mut ch, mut cd, mut i) = (h, 1u32, home as usize);
            loop {
                if i == sl.len() { sl.push((0, 0)); }
                let e = sl[i];
                if e.1 == 0 { sl[i] = (ch, cd); break; }
                if e.1 < cd { sl[i] = (ch, cd); ch = e.0; cd = e.1; }
                i += 1;
                cd += 1;
            }
        }
        while sl.last().unwrap().1 != 0 { sl.push((0, 0)); }
        for e in &sl { if e.1 > 0 { maxd = maxd.max(e.1 - 1); sumd += (e.1 - 1) as u64; } }
        let rbits = bitlen((((1u64 << b) - 1) / cap as u64) as u64);
        let dbits = bitlen(maxd as u64 + 1);
        let need = rbits + dbits;
        let tagw = if need <= 8 { 1 } else if need <= 16 { 2 } else if need <= 32 { 4 } else { continue };
        let len = sl.len() as u32;
        let bytes = len as u64 * (tagw + pay) as u64;
        if best.as_ref().map_or(true, |(bb, p)| bytes < *bb || (bytes == *bb && cap > p.cap)) {
            let tags = sl.iter().map(|&(h, d)| if d == 0 { 0 } else { (divmod(h, m, cap).0 << dbits) | d }).collect();
            best = Some((bytes, Plan { cap, len, tagw, dbits, maxd, sumd, tags }));
        }
    }
    best.expect("no layout fits a 32-bit tag").1
}

/// Plans for many shards on `threads` threads.
pub fn plan_all(keys: &[Vec<u32>], b: &[u32], pay: u32, max_load: f64, keep_tags: bool) -> Vec<Plan> {
    let threads = std::env::var("COMPACT_THREADS").ok().and_then(|s| s.parse().ok()).unwrap_or(8usize);
    let next = std::sync::atomic::AtomicUsize::new(0);
    let out: Vec<std::sync::Mutex<Option<Plan>>> = (0..keys.len()).map(|_| std::sync::Mutex::new(None)).collect();
    std::thread::scope(|s| {
        for _ in 0..threads {
            s.spawn(|| loop {
                let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if i >= keys.len() { break; }
                let mut p = plan(&keys[i], b[i], pay, max_load);
                if !keep_tags { p.tags = Vec::new(); }
                *out[i].lock().unwrap() = Some(p);
            });
        }
    });
    out.into_iter().map(|m| m.into_inner().unwrap().unwrap()).collect()
}

/// Eight u16 tags at once (SSE2): the first lane whose displacement field is
/// below the probe's distance (an empty slot, or a richer entry) ends the
/// run; a hit is a lane before it with distance AND quotient equal.
/// `dist0` = the distance of lane 0 (1 at the home slot).
#[inline(always)]
unsafe fn scan_u16(p: *const u8, q: u32, dbits: u32) -> Option<u32> {
    use std::arch::x86_64::*;
    let dm = _mm_set1_epi16(((1u32 << dbits) - 1) as i16);
    let qv = _mm_set1_epi16(q as u16 as i16);
    let sh = _mm_cvtsi32_si128(dbits as i32);
    let eight = _mm_set1_epi16(8);
    let mut dist = _mm_setr_epi16(1, 2, 3, 4, 5, 6, 7, 8);
    let mut j = 0u32;
    loop {
        let v = _mm_loadu_si128(p.add(2 * j as usize) as *const __m128i);
        let d = _mm_and_si128(v, dm);
        let stop = _mm_movemask_epi8(_mm_cmplt_epi16(d, dist)) as u32;
        let eq = _mm_movemask_epi8(_mm_and_si128(_mm_cmpeq_epi16(d, dist), _mm_cmpeq_epi16(_mm_srl_epi16(v, sh), qv))) as u32;
        let before = if stop == 0 { u32::MAX } else { (1u32 << stop.trailing_zeros()) - 1 };
        let hit = eq & before;
        if hit != 0 { return Some(j + hit.trailing_zeros() / 2); }
        if stop != 0 { return None; }
        j += 8;
        dist = _mm_add_epi16(dist, eight);
    }
}

/// The frame-start set, quotiented: per shard a static Robin Hood table.
/// Tags and payloads in two arenas (the scan reads tags only).
pub struct QTable { pub sh: Vec<QShard>, pub tags: HugeVec<u8>, pub pays: HugeVec<u8>, pub slots: u64 }

impl QTable {
    pub fn build(keys: &[Vec<u32>], b: &[u32], pay: u32, max_load: f64) -> (QTable, Vec<Plan>) {
        let mut plans = plan_all(keys, b, pay, max_load, true);
        let tbytes: u64 = plans.iter().map(|p| p.len as u64 * p.tagw as u64).sum();
        let slots: u64 = plans.iter().map(|p| p.len as u64).sum();
        // +64 B: a 16-B scan may read past the last shard's trailing empty slot.
        let mut tags: HugeVec<u8> = HugeVec::zeroed(tbytes as usize + 64);
        let pays: HugeVec<u8> = HugeVec::zeroed((slots * pay as u64) as usize + 8);
        let (mut off, mut base) = (0u64, 0u64);
        let mut sh = Vec::with_capacity(plans.len());
        for (i, p) in plans.iter_mut().enumerate() {
            sh.push(QShard { off, poff: base * pay as u64, base: base as u32, cap: p.cap, m: if p.cap >= 2 { fastdiv_m(p.cap) } else { 0 }, b: b[i] as u8, dbits: p.dbits as u8, tagw: p.tagw as u8, pay: pay as u8 });
            for (j, &t) in p.tags.iter().enumerate() {
                let a = (off + j as u64 * p.tagw as u64) as usize;
                match p.tagw { 1 => tags[a] = t as u8, 2 => tags[a..a + 2].copy_from_slice(&(t as u16).to_le_bytes()), _ => tags[a..a + 4].copy_from_slice(&t.to_le_bytes()) }
            }
            p.tags = Vec::new();
            off += p.len as u64 * p.tagw as u64;
            base += p.len as u64;
        }
        assert!(base < 1 << 32);
        (QTable { sh, tags, pays, slots: base }, plans)
    }

    #[inline(always)]
    fn tag(&self, a: usize, w: u8) -> u32 {
        unsafe {
            let p = self.tags.as_ptr().add(a);
            match w { 1 => *p as u32, 2 => (p as *const u16).read_unaligned() as u32, _ => (p as *const u32).read_unaligned() }
        }
    }

    /// THE PROBE: `Some((global slot, byte offset of its payload))` if `k` is in
    /// shard `sh`. Robin Hood early exit: an entry closer to its home than we
    /// are to ours (or an empty slot, displacement field 0) ends the run.
    /// u16 tags (most of the states) are scanned eight at a time.
    #[inline(always)]
    pub fn find(&self, sh: u32, k: u32) -> Option<(u32, usize)> {
        let s = unsafe { self.sh.get_unchecked(sh as usize) };
        if s.cap == 0 { return None; }
        let (q, home) = divmod(scr(k, s.b as u32), s.m, s.cap);
        let found = if s.tagw == 2 {
            unsafe { scan_u16(self.tags.as_ptr().add(s.off as usize + 2 * home as usize), q, s.dbits as u32) }.map(|j| home + j)
        } else {
            let dmask = (1u32 << s.dbits) - 1;
            let (mut i, mut dist) = (home, 1u32);
            loop {
                let v = self.tag(s.off as usize + i as usize * s.tagw as usize, s.tagw);
                let d = v & dmask;
                if d < dist { break None; }
                if d == dist && v >> s.dbits == q { break Some(i); }
                dist += 1;
                i += 1;
            }
        };
        found.map(|i| (s.base + i, (s.poff + i as u64 * s.pay as u64) as usize))
    }

    /// The home slot's tag address (for software prefetching in batched
    /// probes: the MLP branch; unused by the single-probe loops here).
    #[allow(dead_code)]
    #[inline(always)]
    pub fn home_addr(&self, sh: u32, k: u32) -> *const u8 {
        let s = unsafe { self.sh.get_unchecked(sh as usize) };
        if s.cap == 0 { return self.tags.as_ptr(); }
        let (_, home) = divmod(scr(k, s.b as u32), s.m, s.cap);
        unsafe { self.tags.as_ptr().add(s.off as usize + home as usize * s.tagw as usize) }
    }

    #[inline(always)]
    pub fn pay_u64(&self, a: usize) -> u64 { unsafe { (self.pays.as_ptr().add(a) as *const u64).read_unaligned() } }
    #[inline(always)]
    pub fn set_u64(&mut self, a: usize, x: u64) { unsafe { (self.pays.as_mut_ptr().add(a) as *mut u64).write_unaligned(x) } }
    #[inline(always)]
    pub fn pay_u32(&self, a: usize) -> u32 { unsafe { (self.pays.as_ptr().add(a) as *const u32).read_unaligned() } }
    #[inline(always)]
    pub fn set_u32(&mut self, a: usize, x: u32) { unsafe { (self.pays.as_mut_ptr().add(a) as *mut u32).write_unaligned(x) } }
    pub fn bytes(&self) -> usize { self.tags.bytes_used() + self.pays.bytes_used() }
    pub fn regions(&self) -> [(usize, usize); 2] { [self.tags.region(), self.pays.region()] }

    /// UNTIMED exactness check: every occupied slot decodes (home from its
    /// displacement, q from its tag) to `h = q cap + home`; per shard the
    /// decoded h are exactly scr of the shard's keys (scr is a bijection, so
    /// the keys themselves), the last slot is empty, and every key is found.
    pub fn verify(&self, keys: &[Vec<u32>], lens: &[u32]) -> u64 {
        let mut bad = 0u64;
        for (si, s) in self.sh.iter().enumerate() {
            let mut want: Vec<u32> = keys[si].iter().map(|&k| scr(k, s.b as u32)).collect();
            want.sort_unstable();
            let mut got: Vec<u32> = Vec::with_capacity(want.len());
            let dmask = (1u32 << s.dbits) - 1;
            for i in 0..lens[si] {
                let v = self.tag(s.off as usize + i as usize * s.tagw as usize, s.tagw);
                let d = v & dmask;
                if d == 0 { continue; }
                if i + 1 == lens[si] { bad += 1; }
                let home = i as i64 - (d - 1) as i64;
                if home < 0 || home >= s.cap as i64 { bad += 1; continue; }
                let h = (v >> s.dbits) as u64 * s.cap as u64 + home as u64;
                if s.b < 32 && h >= 1u64 << s.b { bad += 1; }
                got.push(h as u32);
            }
            got.sort_unstable();
            if got != want { bad += 1; }
            for &k in &keys[si] { if self.find(si as u32, k).is_none() { bad += 1; } }
        }
        bad
    }
}

/// The frame's new entries: per shard non-moving linear probing on the full
/// key (+1; 0 = empty) and a payload; the id is the global slot, stable.
#[repr(C, packed)]
#[derive(Clone, Copy)]
pub struct NE<P: Copy> { pub k: u32, pub p: P }

pub struct NTable<P: Copy> { sh: Vec<(u32, u32, u64)>, pub e: HugeVec<NE<P>>, fill: Vec<u32>, pub slots: u64 }

impl<P: Copy> NTable<P> {
    /// `counts`: the entries each shard will receive (the bench sizes from the
    /// frame's known count; production would grow - see DESIGNS "Compact").
    pub fn new(counts: &[u64], load: f64) -> Self {
        let mut sh = Vec::with_capacity(counts.len());
        let mut base = 0u64;
        for &c in counts {
            let cap = if c == 0 { 0 } else { ((c as f64 / load).ceil() as u32).max(c as u32 + 1).max(2) };
            sh.push((base as u32, cap, if cap >= 2 { fastdiv_m(cap) } else { 0 }));
            base += cap as u64;
        }
        assert!(base < 1 << 32);
        NTable { sh, e: HugeVec::zeroed(base as usize), fill: vec![0; counts.len()], slots: base }
    }
    /// (global slot, inserted). FATAL when a shard would fill up.
    #[inline(always)]
    pub fn find_or_insert(&mut self, sh: u32, k: u32, p0: P) -> (u32, bool) {
        let (base, cap, m) = unsafe { *self.sh.get_unchecked(sh as usize) };
        assert!(cap > 0, "NTable: shard {sh} has no room for new entries");
        let (_, mut i) = divmod(k.wrapping_mul(0x9E37_79B1) ^ (k >> 13), m, cap);
        let kk = k + 1;
        loop {
            let e = unsafe { self.e.get_unchecked_mut((base + i) as usize) };
            let ek = e.k;
            if ek == kk { return (base + i, false); }
            if ek == 0 {
                let f = &mut self.fill[sh as usize];
                *f += 1;
                assert!(*f < cap, "NTable: shard {sh} full (sized for {} entries)", cap);
                *e = NE { k: kk, p: p0 };
                return (base + i, true);
            }
            i += 1; if i == cap { i = 0; }
        }
    }
    #[inline(always)]
    pub fn get(&self, slot: u32) -> P { unsafe { self.e.get_unchecked(slot as usize).p } }
    #[inline(always)]
    pub fn set(&mut self, slot: u32, p: P) { unsafe { self.e.get_unchecked_mut(slot as usize).p = p } }
}

// ------------------------------------------------------------ prep

/// Per state: its digits (room and per-shard local dictionary indices).
#[derive(Clone, Copy, Default)]
struct Dig { shard: u32, fdr: u16, sxr: u16, fdl: u16, sxl: u16, spl: u16, syr: u8, syl: u8 }

/// Per shard: shape and the local dictionaries' sizes.
#[derive(Clone, Copy, Default)]
struct ShardInfo { shape: u32, nfd: u32, nsx: u32, nsy: u32, nsp: u32 }

/// The configurations: how a state becomes (shard, key, bit).
/// `a*`: one entry a state; `b*`: a directory entry with a u64 mask over the
/// local (or room) spd.y index, the word in the key; `c*`: a directory entry
/// with an interned u128 mask over the ROOM spd.y digit.
const CFGS: [&str; 8] = ["acell", "acellp2", "aroom", "acollapse", "bcell", "broom", "ccell", "croom"];

struct Room { nfd: Vec<u32>, nsx: Vec<u32>, nsy: Vec<u32> }

fn pow2(n: u32) -> u32 { (2 * n.max(1)).next_power_of_two() }

/// (b bits of the key space, key, bit) of a state under `cfg`.
fn keyof(cfg: &str, d: &Dig, si: &ShardInfo, r: &Room) -> (u32, u64, u32) {
    let s = si.shape as usize;
    let (fl, xl, yl, pl) = (d.fdl as u64, d.sxl as u64, d.syl as u64, d.spl as u64);
    let (fr, xr, yr) = (d.fdr as u64, d.sxr as u64, d.syr as u64);
    let (nf, nx, ny, np) = (si.nfd as u64, si.nsx as u64, si.nsy as u64, si.nsp as u64);
    let (rf, rx, ry) = (r.nfd[s] as u64, r.nsx[s] as u64, r.nsy[s] as u64);
    let blen = |p: u64| bitlen(p.max(1) - 1);
    match cfg {
        "acell" => (blen(nf * nx * ny), (fl * nx + xl) * ny + yl, 0),
        "acellp2" => {
            let (bx, by) = (bitlen(pow2(si.nsx) as u64 - 1), bitlen(pow2(si.nsy) as u64 - 1));
            (bitlen(pow2(si.nfd) as u64 - 1) + bx + by, fl << (bx + by) | xl << by | yl, 0)
        }
        "aroom" => (blen(rf * rx * ry), (fr * rx + xr) * ry + yr, 0),
        "acollapse" => (blen(nf * np), fl * np + pl, 0),
        "bcell" => { let nw = ny.div_ceil(64); (blen(nf * nx * nw), (fl * nx + xl) * nw + yl / 64, (yl % 64) as u32) }
        "broom" => { let nw = ry.div_ceil(64); (blen(rf * rx * nw), (fr * rx + xr) * nw + yr / 64, (yr % 64) as u32) }
        "ccell" => (blen(nf * nx), fl * nx + xl, yr as u32),
        "croom" => (blen(rf * rx), fr * rx + xr, yr as u32),
        _ => unreachable!(),
    }
}

/// 32-byte query record (as QB: the stream is the same size as v3c's / bits').
#[repr(C)]
#[derive(Clone, Copy)]
pub struct QC { pub shard: u32, pub src: u32, pub xfer: u32, pub k: u32, pub bit: u32, _p: [u32; 3] }

/// Peak of the sum of `w[i]` over intervals [first[i], last[i]] (inclusive).
fn window(spans: &[(u32, u32)], w: &dyn Fn(usize) -> f64) -> (f64, f64) {
    let mut ev: Vec<(u32, bool, usize)> = Vec::with_capacity(2 * spans.len());
    for (i, &(f, l)) in spans.iter().enumerate() { if f != u32::MAX { ev.push((f, false, i)); ev.push((l, true, i)); } }
    ev.sort_unstable_by_key(|e| (e.0, e.1));
    let (mut cur, mut peak, mut area, mut last_t) = (0f64, 0f64, 0f64, 0u32);
    for &(t, end, i) in &ev {
        area += cur * (t - last_t) as f64; last_t = t;
        if end { cur -= w(i); } else { cur += w(i); peak = peak.max(cur); }
    }
    (peak, area / last_t.max(1) as f64)
}

/// `compactprep`: dictionaries, keys, the size table and the windows; writes
/// OUT/{cfg}_st.bin, {cfg}_b.bin and q_{cfg}.bin for the timed configurations.
pub fn prep(dir: &str, door: &[u8]) {
    let t0 = std::time::Instant::now();
    std::fs::create_dir_all(OUT).unwrap();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, sm, dsm, shm, rm) = (map("qb.bin"), map("dshard.bin"), map("db.bin"), map("sshape.bin"), map("refid.bin"));
    let qs: &[crate::bits::QB] = crate::from_bytes(&qm);
    let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm);
    let sshape: &[u32] = crate::from_bytes(&shm); let refid: &[u32] = crate::from_bytes(&rm);
    let n_door = door.len() / 40;
    let n_new = 6_735_699usize;
    let n_st = n_door + n_new;
    let n_shards = sshape.len();
    // The packing's digits per shape, as bits::run_words reads them.
    let f = crate::bits::Fields::open(dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<crate::bits::Packing> = (0..f.shapes.len()).map(|s| crate::bits::packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    let roles: Vec<Vec<(u64, u8)>> = packs.iter().map(|p| p.high.iter().map(|&d| (p.digits[d].radix, if p.digits[d].name == "spd.x" { 1 } else if p.digits[d].table.is_some() { 2 } else { 0 })).collect()).collect();
    let low_r: Vec<u64> = packs.iter().map(|p| p.low.map_or(1, |d| p.digits[d].radix)).collect();
    let n_shapes = roles.len();
    // high -> (flags+dash joint value, spd.x digit)
    let split = |s: usize, high: u32| -> (u64, u32) {
        let mut h = high as u64;
        let mut ds: Vec<(u64, u64, u8)> = Vec::with_capacity(8);
        for &(r, role) in roles[s].iter().rev() { ds.push((h % r, r, role)); h /= r; }
        assert_eq!(h, 0);
        ds.reverse();
        let (mut fl, mut sx, mut da, mut dar) = (0u64, 0u64, 0u64, 1u64);
        for &(d, r, role) in &ds { match role { 0 => fl = fl * r + d, 1 => sx = sx * r + d, _ => { da = da * r + d; dar *= r } } }
        (fl * dar + da, sx as u32)
    };
    // 1. The states: (shard, high, low) per reference id; every query agrees
    //    with its reference id's state; distinct ids are distinct states.
    let mut st: Vec<(u32, u32, u32)> = vec![(u32::MAX, 0, 0); n_st];
    for i in 0..n_door { st[i] = (dsh[i], db[i].0, db[i].1); }
    let mut disagree = 0u64;
    for (q, &r) in qs.iter().zip(refid) {
        let e = &mut st[r as usize];
        if e.0 == u32::MAX { *e = (q.shard, q.high, q.low); } else if *e != (q.shard, q.high, q.low) { disagree += 1; }
    }
    assert_eq!(disagree, 0, "a query's state is not its reference id's");
    assert!(st.iter().all(|e| e.0 != u32::MAX));
    {
        let mut p: Vec<u64> = st.iter().map(|&(s, h, l)| { assert!(s < 1 << 16 && l < 1 << 8); (s as u64) << 40 | (h as u64) << 8 | l as u64 }).collect();
        p.sort_unstable();
        assert!(p.windows(2).all(|w| w[0] != w[1]), "two reference ids, one packed state");
    }
    eprintln!("[compactprep] {n_st} states ({n_door} old + {n_new} new), {} queries consistent and injective; {:.1} s", qs.len(), t0.elapsed().as_secs_f64());
    // 2. Digits: room dictionaries (flags+dash joint per shape; spd.x and spd.y
    //    are the packing's digits) and per-shard local dictionaries.
    let mut fdv: Vec<u64> = Vec::with_capacity(n_st);
    let mut sxr: Vec<u32> = Vec::with_capacity(n_st);
    for &(sh, h, _) in &st { let (fd, sx) = split(sshape[sh as usize] as usize, h); fdv.push(fd); sxr.push(sx); }
    let mut room_fd: Vec<Vec<u64>> = vec![Vec::new(); n_shapes];
    for (i, &(sh, _, _)) in st.iter().enumerate() { room_fd[sshape[sh as usize] as usize].push(fdv[i]); }
    for v in room_fd.iter_mut() { v.sort_unstable(); v.dedup(); }
    let room = Room { nfd: room_fd.iter().map(|v| v.len() as u32).collect(), nsx: roles.iter().map(|r| r.iter().filter(|x| x.1 == 1).map(|x| x.0 as u32).product()).collect(), nsy: low_r.iter().map(|&x| x as u32).collect() };
    eprintln!("[compactprep] room alphabets per shape: flags+dash {:?}, spd.x {:?}, spd.y {:?}", room.nfd, room.nsx, room.nsy);
    // local dictionary: per shard the sorted distinct values; index by binary search.
    let local = |vals: &dyn Fn(usize) -> u64, only_old: bool| -> (Vec<u32>, Vec<u64>) {
        let n = if only_old { n_door } else { n_st };
        let mut kv: Vec<(u32, u64)> = (0..n).map(|i| (st[i].0, vals(i))).collect();
        kv.sort_unstable(); kv.dedup();
        let mut start = vec![0u32; n_shards + 1];
        for &(s, _) in &kv { start[s as usize + 1] += 1; }
        for s in 0..n_shards { start[s + 1] += start[s]; }
        (start, kv.into_iter().map(|x| x.1).collect())
    };
    let idx = |d: &(Vec<u32>, Vec<u64>), sh: u32, v: u64| -> Option<u32> {
        let (a, b) = (d.0[sh as usize] as usize, d.0[sh as usize + 1] as usize);
        d.1[a..b].binary_search(&v).ok().map(|x| x as u32)
    };
    let vfd = |i: usize| fdv[i];
    let vsx = |i: usize| sxr[i] as u64;
    let vsy = |i: usize| st[i].2 as u64;
    let vsp = |i: usize| (sxr[i] as u64) << 8 | st[i].2 as u64;
    let (lfd, lsx, lsy, lsp) = (local(&vfd, false), local(&vsx, false), local(&vsy, false), local(&vsp, false));
    let size = |d: &(Vec<u32>, Vec<u64>), s: usize| d.0[s + 1] - d.0[s];
    let info: Vec<ShardInfo> = (0..n_shards).map(|s| ShardInfo { shape: sshape[s], nfd: size(&lfd, s), nsx: size(&lsx, s), nsy: size(&lsy, s), nsp: size(&lsp, s) }).collect();
    let mut dig: Vec<Dig> = Vec::with_capacity(n_st);
    for i in 0..n_st {
        let sh = st[i].0;
        let s = sshape[sh as usize] as usize;
        let fdr = room_fd[s].binary_search(&fdv[i]).unwrap();
        let g = |d: &(Vec<u32>, Vec<u64>), v: u64| idx(d, sh, v).expect("a value missing from its own dictionary: FATAL");
        dig.push(Dig { shard: sh, fdr: fdr as u16, sxr: sxr[i] as u16, fdl: g(&lfd, fdv[i]) as u16, sxl: g(&lsx, vsx(i)) as u16, spl: g(&lsp, vsp(i)) as u16, syr: st[i].2 as u8, syl: g(&lsy, vsy(i)) as u8 });
    }
    let dsum = |f: &dyn Fn(&ShardInfo) -> u32| info.iter().map(|x| f(x) as u64).sum::<u64>();
    eprintln!("[compactprep] local dictionaries (entries over {n_shards} shards): flags+dash {}, spd.x {}, spd.y {}, (spd.x, spd.y) {}; max {} / {} / {} / {}; {:.1} s",
        dsum(&|x| x.nfd), dsum(&|x| x.nsx), dsum(&|x| x.nsy), dsum(&|x| x.nsp),
        info.iter().map(|x| x.nfd).max().unwrap(), info.iter().map(|x| x.nsx).max().unwrap(), info.iter().map(|x| x.nsy).max().unwrap(), info.iter().map(|x| x.nsp).max().unwrap(), t0.elapsed().as_secs_f64());
    // 3. Append-only: dictionaries from the frame-start set only; what f57 adds.
    {
        let (ofd, osx, osy) = (local(&vfd, true), local(&vsx, true), local(&vsy, true));
        let mut out = String::new();
        for (name, full, old) in [("flags+dash", &lfd, &ofd), ("spd.x", &lsx, &osx), ("spd.y", &lsy, &osy)] {
            let (mut appended, mut over_p2, mut over_p2x2, mut states_hit) = (0u64, 0u64, 0u64, 0u64);
            for s in 0..n_shards {
                let (nf, no) = (size(full, s), size(old, s));
                appended += (nf - no) as u64;
                if no > 0 && nf > no.next_power_of_two() { over_p2 += 1; }
                if no > 0 && nf > pow2(no) { over_p2x2 += 1; }
            }
            for i in n_door..n_st { if idx(old, st[i].0, match name { "flags+dash" => vfd(i), "spd.x" => vsx(i), _ => vsy(i) }).is_none() { states_hit += 1; } }
            out += &format!("  {name:<10}: {} values at the frame start, {appended} appended by f57 ({states_hit} of its {n_new} new states carry one); shards outgrowing radix next_pow2(n): {over_p2}, next_pow2(2n): {over_p2x2}\n", old.1.len());
        }
        let mut room_new = 0;
        for (s, v) in room_fd.iter().enumerate() {
            let mut o: Vec<u64> = (0..n_door).filter(|&i| sshape[st[i].0 as usize] as usize == s).map(|i| fdv[i]).collect();
            o.sort_unstable(); o.dedup();
            room_new += v.len() - o.len();
        }
        let new_shards = (0..n_shards).filter(|&s| lfd.0[s + 1] > lfd.0[s] && ofd.0[s + 1] == ofd.0[s]).count();
        out += &format!("  room flags+dash dictionary: {room_new} values appended by f57; shards first occupied in f57: {new_shards}\n");
        eprint!("[compactprep] append-only dictionaries (frame start -> f57):\n{out}");
    }
    // 4. The windows: per state and per shard, over the sweep (time = query index).
    let mut sfirst = vec![u32::MAX; n_st]; let mut slast = vec![0u32; n_st];
    let mut hfirst = vec![u32::MAX; n_shards]; let mut hlast = vec![0u32; n_shards];
    for (t, (q, &r)) in qs.iter().zip(refid).enumerate() {
        let t = t as u32;
        if sfirst[r as usize] == u32::MAX { sfirst[r as usize] = t; } slast[r as usize] = t;
        if hfirst[q.shard as usize] == u32::MAX { hfirst[q.shard as usize] = t; } hlast[q.shard as usize] = t;
    }
    let sspans: Vec<(u32, u32)> = sfirst.iter().zip(&slast).map(|(&a, &b)| (a, b)).collect();
    let hspans: Vec<(u32, u32)> = hfirst.iter().zip(&hlast).map(|(&a, &b)| (a, b)).collect();
    drop(sfirst); drop(slast);
    let (st_peak, st_mean) = window(&sspans, &|_| 1.0);
    drop(sspans);
    let mut cnt_end = vec![0u64; n_shards]; let mut cnt_old = vec![0u64; n_shards];
    for (i, d) in dig.iter().enumerate() { cnt_end[d.shard as usize] += 1; if i < n_door { cnt_old[d.shard as usize] += 1; } }
    let (sh_peak, _) = window(&hspans, &|s| cnt_end[s] as f64);
    eprintln!("[compactprep] live window over the sweep: per STATE peak {st_peak:.0} (mean {st_mean:.0}); per SHARD (a shard live from its first to its last lookup) peak {sh_peak:.0} states; {:.1} s", t0.elapsed().as_secs_f64());
    // 5. The size table.
    let max_load: f64 = std::env::var("COMPACT_LOAD").ok().and_then(|s| s.parse().ok()).unwrap_or(0.9);
    let new_load = 0.75;
    let mut rep = String::new();
    rep += &format!("states {n_st} (old {n_door}, new {n_new}); per-state live peak {st_peak:.0}; static tables at max load {max_load}, new tables at load {new_load}\n");
    rep += &format!("{:<10} {:>10} {:>10} {:>9} {:>9} {:>9} {:>9} {:>8} {:>9} {:>10} {:>10} {:>10} {:>16}\n", "config", "old entr", "new entr", "old MB", "new MB", "frame MB", "B/state", "dict MB", "end MB", "end B/st", "win shard", "win state", "tag 1/2/4 B (%)");
    // References, from the same counts.
    let refrow = |name: &str, per_shard: &dyn Fn(usize) -> f64| -> String {
        let tot: f64 = (0..n_shards).map(per_shard).sum();
        let (wp, _) = window(&hspans, per_shard);
        format!("{name:<10} {:>10} {:>10} {:>9} {:>9} {:>9.1} {:>9.2} {:>8} {:>9} {:>10} {:>10.1} {:>10.1}\n", "-", "-", "-", "-", tot / 1e6, tot / n_st as f64, "-", "-", "-", wp / 1e6, st_peak * tot / n_st as f64 / 1e6)
    };
    let p2 = |c: u64, load: f64| ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two() as f64;
    rep += &refrow("v3c", &|s| p2(cnt_end[s], 0.5) * 20.0);
    rep += &refrow("v4", &|s| p2(cnt_end[s], 0.8) * 12.0);
    let mut entries_of: FxHashMap<&str, Vec<u64>> = FxHashMap::default();
    for cfg in CFGS {
        let t = std::time::Instant::now();
        let mut ks: Vec<(u32, u64, u32)> = Vec::with_capacity(n_st);
        let mut bsh = vec![0u32; n_shards];
        for d in &dig { let (b, k, bit) = keyof(cfg, d, &info[d.shard as usize], &room); assert!(b <= 32 && k < 1u64 << b.max(1) || b == 0 && k == 0, "{cfg}: key {k} beyond {b} bits"); bsh[d.shard as usize] = b; ks.push((d.shard, k, bit)); }
        let dir = !cfg.starts_with('a');
        // Old and end entries per shard (distinct keys).
        let entries = |range: std::ops::Range<usize>| -> Vec<Vec<u32>> {
            let mut v: Vec<Vec<u32>> = vec![Vec::new(); n_shards];
            for i in range { v[ks[i].0 as usize].push(ks[i].1 as u32); }
            for x in v.iter_mut() { x.sort_unstable(); x.dedup(); }
            v
        };
        let (old, end) = (entries(0..n_door), entries(0..n_st));
        // Injectivity of the key: (shard, key, bit) distinct over distinct states.
        if !dir { assert!(end.iter().map(|v| v.len()).sum::<usize>() == n_st, "{cfg}: two states, one key"); }
        else { let mut p: Vec<(u32, u32, u32)> = ks.iter().map(|&(s, k, b)| (s, k as u32, b)).collect(); p.sort_unstable(); assert!(p.windows(2).all(|w| w[0] != w[1]), "{cfg}: two states, one (key, bit)"); }
        let n_old_e: u64 = old.iter().map(|v| v.len() as u64).sum();
        let n_end_e: u64 = end.iter().map(|v| v.len() as u64).sum();
        let pay = match &cfg[..1] { "a" => 0, "b" => 8, _ => 4 };
        let plans = plan_all(&old, &bsh, pay, max_load, false);
        let plans_end = plan_all(&end, &bsh, pay, max_load, false);
        let new_e: Vec<u64> = (0..n_shards).map(|s| (end[s].len() - old[s].len()) as u64).collect();
        let shard_old = |s: usize| plans[s].len as f64 * (plans[s].tagw + pay) as f64;
        let shard_new = |s: usize| if new_e[s] == 0 { 0.0 } else { ((new_e[s] as f64 / new_load).ceil().max(new_e[s] as f64 + 1.0)) * (4 + pay) as f64 };
        let dict_entries = |s: usize| -> f64 { let x = &info[s]; (match cfg { "acell" | "acellp2" | "bcell" => x.nfd + x.nsx + x.nsy, "ccell" => x.nfd + x.nsx, "acollapse" => x.nfd + x.nsp, _ => 0 }) as f64 };
        let shard_bytes = |s: usize| shard_old(s) + shard_new(s) + 8.0 * dict_entries(s);
        let old_b: f64 = (0..n_shards).map(shard_old).sum();
        let new_b: f64 = (0..n_shards).map(shard_new).sum();
        let dict_b: f64 = (0..n_shards).map(|s| 8.0 * dict_entries(s)).sum();
        let end_b: f64 = plans_end.iter().map(|p| p.len as f64 * (p.tagw + pay) as f64).sum();
        let frame = old_b + new_b;
        let (wp, _) = window(&hspans, &shard_bytes);
        let mut tw = [0f64; 3];
        for (s, p) in plans.iter().enumerate() { tw[match p.tagw { 1 => 0, 2 => 1, _ => 2 }] += old[s].len() as f64; }
        let te: f64 = tw.iter().sum();
        let maxd = plans.iter().map(|p| p.maxd).max().unwrap();
        let meand = plans.iter().map(|p| p.sumd).sum::<u64>() as f64 / n_old_e as f64;
        rep += &format!("{cfg:<10} {n_old_e:>10} {:>10} {:>9.1} {:>9.1} {:>9.1} {:>9.2} {:>8.1} {:>9.1} {:>10.2} {:>10.1} {:>10.1} {:>5.0}/{:>4.0}/{:>4.0}   disp mean {meand:.2} max {maxd}{}\n",
            n_end_e - n_old_e, old_b / 1e6, new_b / 1e6, frame / 1e6, frame / n_st as f64, dict_b / 1e6, end_b / 1e6, end_b / n_st as f64, wp / 1e6, st_peak * (frame + dict_b) / n_st as f64 / 1e6,
            100.0 * tw[0] / te, 100.0 * tw[1] / te, 100.0 * tw[2] / te, if dir { format!("; {:.2} states an entry", n_st as f64 / n_end_e as f64) } else { String::new() });
        eprintln!("[compactprep] {cfg}: {:.1} s", t.elapsed().as_secs_f64());
        entries_of.insert(cfg, end.iter().map(|v| v.len() as u64).collect());
        // The timed configurations' streams.
        let emit = std::env::var("COMPACT_EMIT").unwrap_or("acell,bcell,croom".into());
        if emit.split(',').any(|c| c == cfg) {
            let stb: Vec<u32> = ks.iter().flat_map(|&(s, k, b)| [s, k as u32, b]).collect();
            std::fs::write(format!("{OUT}/{cfg}_st.bin"), crate::as_bytes(&stb)).unwrap();
            std::fs::write(format!("{OUT}/{cfg}_b.bin"), crate::as_bytes(&bsh)).unwrap();
            let mut w = std::io::BufWriter::with_capacity(1 << 24, std::fs::File::create(format!("{OUT}/q_{cfg}.bin")).unwrap());
            for (q, &r) in qs.iter().zip(refid) {
                let (s, k, b) = ks[r as usize];
                assert_eq!(s, q.shard);
                let rec = QC { shard: q.shard, src: q.src, xfer: q.xfer, k: k as u32, bit: b, _p: [0; 3] };
                w.write_all(crate::as_bytes(std::slice::from_ref(&rec))).unwrap();
            }
            w.flush().unwrap();
            eprintln!("[compactprep] wrote {OUT}/q_{cfg}.bin; {:.1} s", t.elapsed().as_secs_f64());
        }
    }
    // bitcell / bits / bitintern from the same directory counts (bitcell's key
    // is bcell's; bits' and bitintern's is ccell's (room spd.y mask)).
    let be = &entries_of["bcell"]; let ce = &entries_of["ccell"];
    rep += &refrow("bitcell", &|s| p2(be[s], 0.5) * 12.0);
    rep += &refrow("bits", &|s| p2(ce[s], 0.5) * 24.0);
    rep += &refrow("bitintern", &|s| p2(ce[s], 0.5) * 8.0);
    rep += "columns: old/new entr = entries in the frame-start static table / the frame's new table; frame MB = both (the structure during the frame); dict MB = per-cell encode dictionaries at 8 B an entry (not in frame MB); end = one static table rebuilt over the end-of-frame set; win shard = peak bytes of the shards live in the sweep (first to last lookup; frame + dict); win state = per-state live peak x (frame + dict) bytes a state\n";
    print!("{rep}");
    std::fs::write(format!("{OUT}/sizes.txt"), &rep).unwrap();
    eprintln!("[compactprep] done; {:.1} s", t0.elapsed().as_secs_f64());
}

// ------------------------------------------------------------ the timed variants

/// `cqa` (one quotiented entry a state), `cqb` (quotiented bitcell directory:
/// tag + u64 spd.y mask), `cqc` (quotiented directory + interned u128 masks).
/// The configuration: `COMPACT_CFG` (default acell / bcell / croom).
pub fn run(dir: &str, n_door: usize, variant: &str) {
    let t = std::time::Instant::now();
    let pd = format!("{dir}/prep");
    let cfg = std::env::var("COMPACT_CFG").unwrap_or(match variant { "cqa" => "acell", "cqb" => "bcell", _ => "croom" }.into());
    let max_load: f64 = std::env::var("COMPACT_LOAD").ok().and_then(|s| s.parse().ok()).unwrap_or(0.9);
    let tag = format!("{variant}:{cfg}");
    let n_st = n_door + 6_735_699;
    let rd = |f: String| std::fs::read(&f).unwrap_or_else(|e| panic!("{f}: {e} (run compactprep)"));
    let bsh: Vec<u32> = crate::from_bytes::<u32>(&rd(format!("{OUT}/{cfg}_b.bin"))).to_vec();
    let n_shards = bsh.len();
    let (keys_old, new_counts, old_states): (Vec<Vec<u32>>, Vec<u64>, Vec<(u32, u32, u32)>) = {
        let stb = rd(format!("{OUT}/{cfg}_st.bin"));
        let s: &[u32] = crate::from_bytes(&stb);
        let st = |i: usize| (s[3 * i], s[3 * i + 1], s[3 * i + 2]);
        let mut old: Vec<Vec<u32>> = vec![Vec::new(); n_shards];
        for i in 0..n_door { let (sh, k, _) = st(i); old[sh as usize].push(k); }
        for v in old.iter_mut() { v.sort_unstable(); v.dedup(); }
        let mut newk: Vec<Vec<u32>> = vec![Vec::new(); n_shards];
        for i in n_door..n_st { let (sh, k, _) = st(i); if old[sh as usize].binary_search(&k).is_err() { newk[sh as usize].push(k); } }
        let nc = newk.iter_mut().map(|v| { v.sort_unstable(); v.dedup(); v.len() as u64 }).collect();
        (old, nc, (0..n_door).map(st).collect())
    };
    let pay = match variant { "cqa" => 0, "cqb" => 8, _ => 4 };
    let (mut tab, plans) = QTable::build(&keys_old, &bsh, pay, max_load);
    let lens: Vec<u32> = plans.iter().map(|p| p.len).collect();
    let bad = tab.verify(&keys_old, &lens);
    assert_eq!(bad, 0, "quotiented table does not decode to its keys");
    let n_old_e: usize = keys_old.iter().map(|v| v.len()).sum();
    drop(keys_old);
    let meand = plans.iter().map(|p| p.sumd).sum::<u64>() as f64 / n_old_e as f64;
    let old_bytes = tab.bytes() as f64;
    let qm = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{OUT}/q_{cfg}.bin")).unwrap()).unwrap() };
    let qs: &[QC] = crate::from_bytes(&qm);
    let dm = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/d.bin")).unwrap()).unwrap() };
    let ds: &[u32] = crate::from_bytes(&dm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let mut frontier: HugeVec<u8> = HugeVec::with_capacity(7_000_000 * crate::PAYLOAD);
    let mut edges: HugeVec<(u32, u32, u32)> = HugeVec::with_capacity(qs.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    let shift = match variant { "cqa" => 0, "cqb" => 6, _ => 7 };
    let (dt, t_drops, nw, new_bytes, extra, old_ids): (f64, f64, u64, f64, String, Vec<u32>);
    macro_rules! timed { ($body:expr) => {{
        crate::perf_on(true);
        let t = std::time::Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        let td = t.elapsed().as_secs_f64();
        let n = $body;
        let dt = t.elapsed().as_secs_f64();
        crate::perf_on(false);
        (dt, td, n)
    }}; }
    match variant {
        "cqa" => {
            drop(old_states);
            old_ids = Vec::new();
            let mut nt: NTable<()> = NTable::new(&new_counts, 0.75);
            new_bytes = nt.e.bytes_used() as f64;
            let old_slots = tab.slots as u32;
            eprintln!("[{tag}] setup {:.1} s: old {n_old_e} entries in {} slots ({:.1} MB, disp mean {meand:.2}), new table {} slots ({:.1} MB)", t.elapsed().as_secs_f64(), tab.slots, old_bytes / 1e6, nt.slots, new_bytes / 1e6);
            huge_report(&tag, &[tab.regions()[0], tab.regions()[1], nt.e.region(), edges.region(), frontier.region()]);
            (dt, t_drops, nw) = timed!({
                let mut nw = 0u64;
                for q in qs {
                    let id = match tab.find(q.shard, q.k) {
                        Some((s, _)) => s,
                        None => { let (s, ins) = nt.find_or_insert(q.shard, q.k, ()); if ins { nw += 1; frontier.extend_zero(crate::PAYLOAD); } old_slots + s }
                    };
                    edges.push((q.src, id, q.xfer));
                }
                nw
            });
            extra = String::new();
            std::hint::black_box(&nt.slots);
        }
        "cqb" => {
            for &(sh, k, b) in &old_states { let (_, a) = tab.find(sh, k).unwrap(); let w = tab.pay_u64(a); tab.set_u64(a, w | 1 << b); }
            let mut ids: Vec<u32> = old_states.iter().map(|&(sh, k, b)| tab.find(sh, k).unwrap().0 << 6 | b).collect();
            drop(old_states);
            ids.sort_unstable();
            old_ids = ids;
            let mut nt: NTable<u64> = NTable::new(&new_counts, 0.75);
            new_bytes = nt.e.bytes_used() as f64;
            let old_slots = tab.slots as u32;
            assert!((tab.slots + nt.slots) << 6 < 1 << 32);
            eprintln!("[{tag}] setup {:.1} s: old directory {n_old_e} entries in {} slots ({:.1} MB, disp mean {meand:.2}), new directory {} slots ({:.1} MB)", t.elapsed().as_secs_f64(), tab.slots, old_bytes / 1e6, nt.slots, new_bytes / 1e6);
            huge_report(&tag, &[tab.regions()[0], tab.regions()[1], nt.e.region(), edges.region(), frontier.region()]);
            (dt, t_drops, nw) = timed!({
                let mut nw = 0u64;
                for q in qs {
                    let b = 1u64 << q.bit;
                    let id = match tab.find(q.shard, q.k) {
                        Some((s, a)) => { let w = tab.pay_u64(a); if w & b == 0 { tab.set_u64(a, w | b); nw += 1; frontier.extend_zero(crate::PAYLOAD); } s << 6 | q.bit }
                        None => {
                            let (s, _) = nt.find_or_insert(q.shard, q.k, 0);
                            let w = nt.get(s);
                            if w & b == 0 { nt.set(s, w | b); nw += 1; frontier.extend_zero(crate::PAYLOAD); }
                            (old_slots + s) << 6 | q.bit
                        }
                    };
                    edges.push((q.src, id, q.xfer));
                }
                nw
            });
            extra = String::new();
        }
        _ => {
            // Masks: the OR of the old states' bits per entry, interned.
            let mut acc: FxHashMap<u32, u128> = FxHashMap::default();
            for &(sh, k, b) in &old_states { *acc.entry(tab.find(sh, k).unwrap().0).or_default() |= 1u128 << b; }
            let mut masks: HugeVec<u128> = HugeVec::with_capacity(1 << 22);
            masks.push(0);
            let mut intern: FxHashMap<u128, u32> = FxHashMap::default();
            intern.insert(0, 0);
            let mut slot_addr: FxHashMap<u32, usize> = FxHashMap::default();
            for &(sh, k, _) in &old_states { let (s, a) = tab.find(sh, k).unwrap(); slot_addr.insert(s, a); }
            for (&s, &m) in &acc { let n = masks.len() as u32; let id = *intern.entry(m).or_insert_with(|| { masks.push(m); n }); tab.set_u32(slot_addr[&s], id); }
            let mut ids: Vec<u32> = old_states.iter().map(|&(sh, k, b)| tab.find(sh, k).unwrap().0 << 7 | b).collect();
            drop((acc, slot_addr, old_states));
            ids.sort_unstable();
            old_ids = ids;
            let n_old_masks = masks.len();
            let mut nt: NTable<u32> = NTable::new(&new_counts, 0.75);
            new_bytes = nt.e.bytes_used() as f64;
            let old_slots = tab.slots as u32;
            assert!((tab.slots + nt.slots) << 7 < 1 << 32);
            eprintln!("[{tag}] setup {:.1} s: old directory {n_old_e} entries in {} slots ({:.1} MB, disp mean {meand:.2}), new directory {} slots ({:.1} MB); {n_old_masks} distinct masks", t.elapsed().as_secs_f64(), tab.slots, old_bytes / 1e6, nt.slots, new_bytes / 1e6);
            huge_report(&tag, &[tab.regions()[0], tab.regions()[1], nt.e.region(), masks.region(), edges.region(), frontier.region()]);
            (dt, t_drops, nw) = timed!({
                let mut nw = 0u64;
                for q in qs {
                    let b = 1u128 << q.bit;
                    let (s, a) = match tab.find(q.shard, q.k) { Some((s, a)) => (s, a), None => { let (s, _) = nt.find_or_insert(q.shard, q.k, 0); (old_slots + s, usize::MAX) } };
                    let mid = if a != usize::MAX { tab.pay_u32(a) } else { nt.get(s - old_slots) };
                    let m = masks[mid as usize];
                    if m & b == 0 {
                        let nm = m | b;
                        let n = masks.len() as u32;
                        let id = *intern.entry(nm).or_insert_with(|| { masks.push(nm); n });
                        if a != usize::MAX { tab.set_u32(a, id); } else { nt.set(s - old_slots, id); }
                        nw += 1; frontier.extend_zero(crate::PAYLOAD);
                    }
                    edges.push((q.src, s << 7 | q.bit, q.xfer));
                }
                nw
            });
            extra = format!("; masks {} ({:.1} MB) + intern map at the frame end", masks.len(), masks.len() as f64 * 16.0 / 1e6);
        }
    }
    std::hint::black_box((&frontier, &dropmin));
    let frame = old_bytes + new_bytes;
    eprintln!("[{tag}] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {nw} ({}), {:.1} ns per query; structure {:.1} MB = {:.2} B a state (old {:.1} MB + new {:.1} MB){extra}",
        if nw == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qs.len() as f64, frame / 1e6, frame / n_st as f64, old_bytes / 1e6, new_bytes / 1e6);
    if std::env::var_os("BENCH_NOCHECK").is_some() { return; }
    let old_slots = tab.slots as u32;
    let e: &[(u32, u32, u32)] = &edges;
    if shift == 0 { crate::bits::check(&tag, &pd, e, n_door, &|m| m < old_slots, false) }
    else { crate::bits::check(&tag, &pd, e, n_door, &|m| old_ids.binary_search(&m).is_ok(), false) }
}

impl HugeVec<u8> {
    #[inline(always)]
    pub fn extend_zero(&mut self, n: usize) {
        assert!(self.len + n <= self.cap, "HugeVec full");
        unsafe { std::ptr::write_bytes(self.ptr.add(self.len), 0, n) }; // written, as Vec::extend_from_slice does
        self.len += n;
    }
}
