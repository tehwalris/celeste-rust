//! `edgebundle CAP TREE idfree ...` (DESIGNS O, "ID-free"): edges named by
//! KEYS only, no entry numbers, lids or owner index. An edge is (source state,
//! target state, transfer), a state its shape + exact position-free key + cell.
//! Stored in the TARGET region as a table (target key, source key, transfer,
//! offset) -> u64 mask over TARGET cells - the same probe and OR as a state -
//! and at rest as a sorted trie in one of three field orders:
//!   v1 (target key, transfer, source key, offset)
//!   v2 (target key, source key, transfer, offset)
//!   v3 (source key, transfer, target key, offset)
//! and, for the placement, v4: (source key, target shape + key, transfer,
//! offset) -> mask over SOURCE cells, in the SOURCE region (no partition in a
//! forward: its units are source runs; here the batches are built by the same
//! partition, reported apart).
//! each in two encodings per unit block: E1 the trie with raw key values (a
//! level byte - the first field that changed - its delta, the later fields
//! raw), E2 the same with each key field a rank into the block's sorted key
//! dictionaries (delta-coded; block-local, not ids). Offset a byte, mask u64.
//!
//! Timed per run: the PARTITION (the emissions in capture order - kernel
//! order, 24 B each as the kernel has them: target key, source key from the
//! input row, transfer and both cells - shipped to their target unit, a
//! count, prefix and scatter pass), then per unit the dedup and the edge
//! table insert, then the block encode of one order. Runs: `dedup:T:split`
//! (partition + dedup), `v1|v2|v3:T:split`, then `access:T` (the backward's
//! read of every block, after the runs) - e.g. `idfree dedup:16:100000
//! v2:16:100000 ... access:16 access:1`.

use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

use crate::edgebundle::{back, cell_of, d_region_id, fwd, get_off, hpart, offset, put_off, slot_local, Data, H, EMPTY, SLOTS};

const SK: u32 = 50;

#[inline] fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u64 { let mut v = 0u64; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u64) << s; if x < 0x80 { return v; } s += 7; } }

/// An emission (24 B): target key; the source's `shape << 50 | key`;
/// transfer 17 | target cell 19 | source cell 19 | target shape 9.
#[derive(Clone, Copy, Default)] #[repr(C)] struct Em { tkey: u64, skey: u64, w: u64 }
/// A partitioned edge (24 B): target key, source key, transfer 17 << 31 |
/// offset code 25 << 6 | target cell in region 6.
#[derive(Clone, Copy, Default)] #[repr(C)] struct IE { tkey: u64, skey: u64, w: u64 }

/// The edge table's entry (40 B): the key (target, source, transfer << 32 |
/// offset), the mask, the generation owning it.
#[derive(Clone, Copy, Default)] #[repr(C)] struct KE { t: u64, s: u64, xo: u64, mask: u64, gen: u32 }

struct Plan { by_source: bool, dense: Vec<u32>, base: Vec<u32>, parts: Vec<u32>, unit_region: Vec<u32>, rest: Vec<Vec<u8>> }

/// An emission's home region (the target's; with `by_source` the source's).
#[inline(always)]
fn home_region(by_source: bool, e: &Em) -> u32 {
    if by_source { (e.skey >> SK) as u32 * SLOTS + slot_local((e.w >> 36) as u32 & ((1 << 19) - 1)).0 }
    else { (e.w >> 55) as u32 * SLOTS + slot_local((e.w >> 17) as u32 & ((1 << 19) - 1)).0 }
}

#[inline(always)]
fn dest(p: &Plan, e: &Em) -> usize {
    let r = p.dense[home_region(p.by_source, e) as usize] as usize;
    let k = p.parts[r];
    (p.base[r] + if k > 1 { hpart(if p.by_source { e.skey } else { e.tkey }, k) } else { 0 }) as usize
}

/// The partitioned record; by source: the target as `shape << 56 | key`, the
/// source side's offset code, the source cell in its region.
#[inline(always)]
fn to_ie(by_source: bool, e: &Em) -> IE {
    let (x, tcell, scell) = (e.w & ((1 << 17) - 1), (e.w >> 17) as u32 & ((1 << 19) - 1), (e.w >> 36) as u32 & ((1 << 19) - 1));
    if by_source {
        let off = offset(scell, tcell, false) as u64;
        IE { tkey: (e.w >> 55) << 56 | e.tkey, skey: e.skey, w: x << 31 | off << 6 | slot_local(scell).1 as u64 }
    } else {
        let off = offset(scell, tcell, true) as u64;
        IE { tkey: e.tkey, skey: e.skey, w: x << 31 | off << 6 | slot_local(tcell).1 as u64 }
    }
}

/// THE PARTITION (timed by the caller): per thread a count per unit over its
/// contiguous chunk of the emissions, the prefix, the scatter into `out`.
/// Returns the units' starts (`n_units + 1`).
fn partition(ems: &[Em], p: &Plan, threads: usize, out: &mut [IE]) -> Vec<usize> {
    let nu = p.unit_region.len();
    let chunk = ems.len().div_ceil(threads);
    let counts: Vec<Vec<u32>> = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..threads).map(|t| sc.spawn(move || {
            let mut c = vec![0u32; nu];
            for e in &ems[(t * chunk).min(ems.len())..((t + 1) * chunk).min(ems.len())] { c[dest(p, e)] += 1; }
            c
        })).collect();
        hs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    let mut starts = vec![0usize; nu + 1];
    let mut cur: Vec<Vec<usize>> = vec![vec![0; nu]; threads];
    let mut at = 0usize;
    for u in 0..nu {
        starts[u] = at;
        for t in 0..threads { cur[t][u] = at; at += counts[t][u] as usize; }
    }
    starts[nu] = at;
    let ptr = out.as_mut_ptr() as usize;
    std::thread::scope(|sc| {
        for (t, mut c) in cur.into_iter().enumerate() {
            sc.spawn(move || {
                for e in &ems[(t * chunk).min(ems.len())..((t + 1) * chunk).min(ems.len())] {
                    let u = dest(p, e);
                    // SAFETY: every slot of `out` is claimed once, by the
                    // thread and unit whose prefix range holds it.
                    unsafe { (ptr as *mut IE).add(c[u]).write(to_ie(p.by_source, e)); }
                    c[u] += 1;
                }
            });
        }
    });
    starts
}

struct Scratch { tab: Vec<H>, et: Vec<KE>, gen: u32, tup: Vec<([u64; 4], u64)>, enc1: Vec<u8>, enc2: Vec<u8>, td: Vec<u64>, sd: Vec<u64> }

/// The fields of a variant's order: (target, source, transfer << 32 | off).
#[inline] fn fields(v: usize, t: u64, s: u64, xo: u64) -> [u64; 4] {
    let (x, off) = (xo >> 32, xo & 0xffff_ffff);
    match v { 1 => [t, x, s, off], 2 => [t, s, x, off], 3 => [s, x, t, off], _ => [s, t, x, off] }
}
/// Which fields are the target and the source key.
fn key_fields(v: usize) -> (usize, usize) { match v { 1 => (0, 2), 2 => (0, 1), 3 => (2, 0), _ => (1, 0) } }
/// The transfer's field.
fn x_field(v: usize) -> usize { match v { 2 | 4 => 2, _ => 1 } }

/// One trie record: the level (first changed field), its delta, the later
/// fields raw (the offset as its byte); then the mask.
#[inline]
fn put_rec(o: &mut Vec<u8>, prev: &mut Option<[u64; 4]>, f: &[u64; 4], mask: u64) {
    let l = match prev { None => 0, Some(p) => (0..4).find(|&i| f[i] != p[i]).unwrap() };
    o.push(l as u8);
    for i in l..4 {
        if i == 3 { put_off(o, f[3] as u32); }
        else if i == l && prev.is_some() { varint(o, f[i] - prev.unwrap()[i]); }
        else { varint(o, f[i]); }
    }
    o.extend_from_slice(&mask.to_le_bytes());
    *prev = Some(*f);
}

fn get_rec(b: &[u8], i: &mut usize, prev: &mut [u64; 4], first: bool) -> u64 {
    let l = b[*i] as usize; *i += 1;
    for k in l..4 {
        if k == 3 { prev[3] = get_off(b, i) as u64; }
        else if k == l && !first { prev[k] += get_varint(b, i); }
        else { prev[k] = get_varint(b, i); }
    }
    let m = u64::from_le_bytes(b[*i..*i + 8].try_into().unwrap()); *i += 8;
    m
}

/// The table's entries as a sorted trie in order `v`: E1 into `enc1`, E2
/// into `enc2`; the nanoseconds of (collect + sort + E1, E2).
fn encode(s: &mut Scratch, cap: usize, v: usize) -> (u64, u64, u64) {
    let t0 = Instant::now();
    s.tup.clear();
    for e in &s.et[..cap] { if e.gen == s.gen { s.tup.push((fields(v, e.t, e.s, e.xo), e.mask)); } }
    s.tup.sort_unstable_by_key(|x| x.0);
    s.enc1.clear();
    varint(&mut s.enc1, s.tup.len() as u64);
    let mut prev = None;
    for (f, m) in &s.tup { put_rec(&mut s.enc1, &mut prev, f, *m); }
    let t1 = t0.elapsed().as_nanos() as u64;
    let t0 = Instant::now();
    let (ti, si) = key_fields(v);
    s.td.clear(); s.td.extend(s.tup.iter().map(|x| x.0[ti])); s.td.sort_unstable(); s.td.dedup();
    s.sd.clear(); s.sd.extend(s.tup.iter().map(|x| x.0[si])); s.sd.sort_unstable(); s.sd.dedup();
    s.enc2.clear();
    for d in [&s.td, &s.sd] { varint(&mut s.enc2, d.len() as u64); let mut p = 0u64; for &k in d.iter() { varint(&mut s.enc2, k - p); p = k; } }
    varint(&mut s.enc2, s.tup.len() as u64);
    let mut prev = None;
    for (f, m) in &s.tup {
        let mut g = *f;
        g[ti] = s.td.binary_search(&f[ti]).unwrap() as u64;
        g[si] = s.sd.binary_search(&f[si]).unwrap() as u64;
        put_rec(&mut s.enc2, &mut prev, &g, *m);
    }
    (t1, t0.elapsed().as_nanos() as u64, s.tup.len() as u64)
}

/// Decode a block of order `v` (`e2`: with its dictionaries) to
/// `f(target key, source key, transfer, offset, mask)`.
fn decode(b: &[u8], v: usize, e2: bool, mut f: impl FnMut(u64, u64, u64, u32, u64)) {
    let mut i = 0;
    let (ti, si) = key_fields(v);
    let mut dicts: [Vec<u64>; 2] = [Vec::new(), Vec::new()];
    if e2 { for d in dicts.iter_mut() { let n = get_varint(b, &mut i); let mut p = 0u64; for _ in 0..n { p += get_varint(b, &mut i); d.push(p); } } }
    let n = get_varint(b, &mut i);
    let mut prev = [0u64; 4];
    for k in 0..n {
        let m = get_rec(b, &mut i, &mut prev, k == 0);
        let (mut t, mut s) = (prev[ti], prev[si]);
        if e2 { t = dicts[0][t as usize]; s = dicts[1][s as usize]; }
        let x = prev[x_field(v)];
        f(t, s, x, prev[3] as u32, m);
    }
    assert_eq!(i, b.len(), "a block did not decode to its end");
}

/// A bump arena shared by the workers (one per order and encoding).
struct Arena { buf: Vec<u8>, at: AtomicUsize }
impl Arena {
    fn put(&self, b: &[u8]) -> (usize, usize) {
        let o = self.at.fetch_add(b.len(), Ordering::Relaxed);
        assert!(o + b.len() <= self.buf.len(), "arena too small");
        // SAFETY: the range was reserved by the fetch_add, for this writer only.
        unsafe { std::ptr::copy_nonoverlapping(b.as_ptr(), (self.buf.as_ptr() as *mut u8).add(o), b.len()); }
        (o, b.len())
    }
}

#[derive(Default, Clone, Copy)] struct UOut { new: u64, bundles: u64, cap: u64, ins: u64, e1: u64, e2: u64, b1: (usize, usize), b2: (usize, usize) }

/// One unit: the dedup of its batch against its old entries, the edge table
/// insert (unless `v` is 0), the encode of order `v`.
fn run_unit(rest: &[u8], batch: &[IE], v: usize, s: &mut Scratch, ar: Option<(&Arena, &Arena)>) -> UOut {
    if v == 4 { return run_source(batch, s, ar); }
    let t0 = Instant::now();
    let mut i = 0;
    let n = get_varint(rest, &mut i) as usize;
    let cap = ((n + batch.len().min(4 * n + 64)) * 2).next_power_of_two().max(16);
    s.tab.clear(); s.tab.resize(cap, H { key: EMPTY, m: 0, nm: 0, lid: u32::MAX });
    let mut mask = cap - 1;
    let mut k = 0u64;
    for _ in 0..n { k += get_varint(rest, &mut i); let m = u64::from_le_bytes(rest[i..i + 8].try_into().unwrap()); i += 8; let t = crate::edgebundle::hslot(&s.tab, mask, k); s.tab[t] = H { key: k, m, nm: 0, lid: u32::MAX }; }
    let mut used = n;
    let (mut ecap, mut eused) = ((batch.len() / 4).next_power_of_two().max(64), 0usize);
    if v > 0 {
        if s.et.len() < ecap { s.et.resize(ecap, KE::default()); }
        s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for e in s.et.iter_mut() { e.gen = 0; } s.gen = 1; }
    }
    let mut new = 0u64;
    for e in batch {
        let mut t = crate::edgebundle::hslot(&s.tab, mask, e.tkey);
        if s.tab[t].key == EMPTY {
            if (used + 1) * 10 > s.tab.len() * 7 {
                let old: Vec<H> = s.tab.drain(..).filter(|h| h.key != EMPTY).collect();
                let nc = (mask + 1) * 2; mask = nc - 1;
                s.tab.resize(nc, H { key: EMPTY, m: 0, nm: 0, lid: u32::MAX });
                for h in old { let q = crate::edgebundle::hslot(&s.tab, mask, h.key); s.tab[q] = h; }
                t = crate::edgebundle::hslot(&s.tab, mask, e.tkey);
            }
            s.tab[t].key = e.tkey; used += 1;
        }
        let bit = 1u64 << (e.w & 63);
        let h = &mut s.tab[t];
        if (h.m | h.nm) & bit == 0 { h.nm |= bit; new += 1; }
        if v > 0 {
            // THE EDGE: one probe, one OR.
            let xo = (e.w >> 31) << 32 | (e.w >> 6 & ((1 << 25) - 1));
            let mut m = ecap - 1;
            let mut q = (crate::bits::mix64(e.tkey ^ e.skey.rotate_left(21) ^ xo.rotate_left(43)) >> 24) as usize & m;
            loop {
                let x = &mut s.et[q];
                if x.gen != s.gen { *x = KE { t: e.tkey, s: e.skey, xo, mask: bit, gen: s.gen }; eused += 1; break; }
                if x.t == e.tkey && x.s == e.skey && x.xo == xo { x.mask |= bit; break; }
                q = (q + 1) & m;
            }
            if eused * 10 > ecap * 7 {
                let old: Vec<KE> = s.et[..ecap].iter().filter(|x| x.gen == s.gen).copied().collect();
                ecap *= 2; m = ecap - 1;
                if s.et.len() < ecap { s.et.resize(ecap, KE::default()); }
                s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for x in s.et.iter_mut() { x.gen = 0; } s.gen = 1; }
                for mut x in old { x.gen = s.gen; let mut q = (crate::bits::mix64(x.t ^ x.s.rotate_left(21) ^ x.xo.rotate_left(43)) >> 24) as usize & m; while s.et[q].gen == s.gen { q = (q + 1) & m; } s.et[q] = x; }
            }
        }
    }
    // Re-encode the entries in key order (the at-rest form of the states).
    let mut keys: Vec<(u64, u64)> = s.tab.iter().filter(|h| h.key != EMPTY).map(|h| (h.key, h.m | h.nm)).collect();
    keys.sort_unstable();
    let mut enc = Vec::with_capacity(keys.len() * 10);
    varint(&mut enc, keys.len() as u64);
    let mut pk = 0u64; for (k, m) in keys { varint(&mut enc, k - pk); enc.extend_from_slice(&m.to_le_bytes()); pk = k; }
    std::hint::black_box(enc.len());
    let mut o = UOut { new, ins: t0.elapsed().as_nanos() as u64, ..Default::default() };
    if v > 0 {
        let (e1, e2, nb) = encode(s, ecap, v);
        (o.e1, o.e2, o.bundles, o.cap) = (e1, e2, nb, ecap as u64);
        if let Some((a1, a2)) = ar { o.b1 = a1.put(&s.enc1); o.b2 = a2.put(&s.enc2); }
    }
    o
}

/// v4: the source region's table, no dedup (the targets' regions do it).
fn run_source(batch: &[IE], s: &mut Scratch, ar: Option<(&Arena, &Arena)>) -> UOut {
    let t0 = Instant::now();
    let (mut ecap, mut eused) = ((batch.len() / 4).next_power_of_two().max(64), 0usize);
    if s.et.len() < ecap { s.et.resize(ecap, KE::default()); }
    s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for e in s.et.iter_mut() { e.gen = 0; } s.gen = 1; }
    for e in batch {
        let bit = 1u64 << (e.w & 63);
        let xo = (e.w >> 31) << 32 | (e.w >> 6 & ((1 << 25) - 1));
        let mut m = ecap - 1;
        let mut q = (crate::bits::mix64(e.tkey ^ e.skey.rotate_left(21) ^ xo.rotate_left(43)) >> 24) as usize & m;
        loop {
            let x = &mut s.et[q];
            if x.gen != s.gen { *x = KE { t: e.tkey, s: e.skey, xo, mask: bit, gen: s.gen }; eused += 1; break; }
            if x.t == e.tkey && x.s == e.skey && x.xo == xo { x.mask |= bit; break; }
            q = (q + 1) & m;
        }
        if eused * 10 > ecap * 7 {
            let old: Vec<KE> = s.et[..ecap].iter().filter(|x| x.gen == s.gen).copied().collect();
            ecap *= 2; m = ecap - 1;
            if s.et.len() < ecap { s.et.resize(ecap, KE::default()); }
            s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for x in s.et.iter_mut() { x.gen = 0; } s.gen = 1; }
            for mut x in old { x.gen = s.gen; let mut q = (crate::bits::mix64(x.t ^ x.s.rotate_left(21) ^ x.xo.rotate_left(43)) >> 24) as usize & m; while s.et[q].gen == s.gen { q = (q + 1) & m; } s.et[q] = x; }
        }
    }
    let mut o = UOut { ins: t0.elapsed().as_nanos() as u64, ..Default::default() };
    let (e1, e2, nb) = encode(s, ecap, 4);
    (o.e1, o.e2, o.bundles, o.cap) = (e1, e2, nb, ecap as u64);
    if let Some((a1, a2)) = ar { o.b1 = a1.put(&s.enc1); o.b2 = a2.put(&s.enc2); }
    o
}

pub fn idfree(mut d: Data, args: &[String]) {
    let t0 = Instant::now();
    // The emissions in capture order, as the kernel has them (untimed).
    let n_shapes = d.regions.iter().map(|r| r.shape).max().unwrap() as usize + 1;
    assert!(n_shapes < 512);
    let mut ems: Vec<Em> = Vec::with_capacity(d.n_edges as usize);
    for v in std::mem::take(&mut d.edges) {
        for e in v {
            let (sr, tr) = (&d.regions[e.sreg as usize], &d.regions[e.treg as usize]);
            let k = sr.keys[e.se as usize];
            assert!(k < 1 << SK && e.tkey < 1 << 56 && e.x < 1 << 17);
            let (tcell, scell) = (cell_of(tr.slot, e.tpos as u32), cell_of(sr.slot, e.spos as u32));
            ems.push(Em { tkey: e.tkey, skey: (sr.shape as u64) << SK | k, w: e.x as u64 | (tcell as u64) << 17 | (scell as u64) << 36 | (tr.shape as u64) << 55 });
        }
    }
    let mut dense = vec![u32::MAX; n_shapes * SLOTS as usize];
    for (i, r) in d.regions.iter().enumerate() { dense[d_region_id(r) as usize] = i as u32; }
    let key_index: Vec<FxHashMap<u64, u32>> = d.regions.iter().map(|r| r.keys.iter().enumerate().map(|(e, &k)| (k, e as u32)).collect()).collect();
    println!("[idfree] {} emissions (24 B each) in capture order; {:.1} s (untimed)", ems.len(), t0.elapsed().as_secs_f64());
    let mut out: Vec<IE> = vec![IE::default(); ems.len()];
    let arena = |n: usize| Arena { buf: vec![0u8; n], at: AtomicUsize::new(0) };
    let mut plans: FxHashMap<(bool, usize), Plan> = FxHashMap::default();
    // The last run of each order: its blocks (for the check and the access) and units.
    let mut kept: FxHashMap<usize, Kept> = FxHashMap::default();
    let mut last_split = 0usize;
    for run in args {
        let w: Vec<&str> = run.split(':').collect();
        let threads: usize = w[1].parse().unwrap();
        if w[0] == "access" { access(&d, &plans[&(false, last_split)], &kept, threads); continue; }
        let v: usize = match w[0] { "dedup" => 0, "v1" => 1, "v2" => 2, "v3" => 3, "v4" => 4, m => panic!("mode {m}") };
        let split: usize = w[2].parse().unwrap();
        let by_source = v == 4;
        if !by_source { last_split = split; }
        if !plans.contains_key(&(by_source, split)) {
            let tp = Instant::now();
            let mut cnt = vec![0usize; d.regions.len()];
            for e in &ems { cnt[dense[home_region(by_source, e) as usize] as usize] += 1; }
            let (mut base, mut parts, mut ur, mut rest) = (vec![0u32; d.regions.len()], vec![0u32; d.regions.len()], Vec::new(), Vec::new());
            for (r, &c) in cnt.iter().enumerate() {
                let k = if c == 0 { 0 } else if split == 0 { 1 } else { c.div_ceil(split).max(1) as u32 };
                base[r] = ur.len() as u32; parts[r] = k;
                let reg = &d.regions[r];
                for p in 0..k {
                    ur.push(r as u32);
                    if by_source { rest.push(Vec::new()); continue; }
                    let mut old: Vec<(u64, u64)> = reg.keys.iter().zip(&reg.masks).filter(|(key, _)| k == 1 || hpart(**key, k) == p).map(|(a, b)| (*a, *b)).collect();
                    old.sort_unstable();
                    let mut b = Vec::new(); varint(&mut b, old.len() as u64);
                    let mut pk = 0u64; for (key, m) in old { varint(&mut b, key - pk); b.extend_from_slice(&m.to_le_bytes()); pk = key; }
                    rest.push(b);
                }
            }
            println!("[idfree] units by {} region, split {split}: {}; {:.1} s (untimed)", if by_source { "source" } else { "target" }, ur.len(), tp.elapsed().as_secs_f64());
            plans.insert((by_source, split), Plan { by_source, dense: dense.clone(), base, parts, unit_region: ur, rest });
        }
        let p = &plans[&(by_source, split)];
        let nu = p.unit_region.len();
        let (a1, a2) = if v > 0 { (arena(d.n_edges as usize * 6), arena(d.n_edges as usize * 3)) } else { (arena(0), arena(0)) };
        for a in [&a1, &a2] { let ptr = a.buf.as_ptr() as *mut u8; for o in (0..a.buf.len()).step_by(4096) { unsafe { ptr.add(o).write(1) }; } }
        let mut scr: Vec<Scratch> = (0..threads).map(|_| Scratch { tab: Vec::with_capacity(1 << 22), et: vec![KE::default(); 1 << 18], gen: 0, tup: Vec::with_capacity(1 << 20), enc1: Vec::with_capacity(1 << 24), enc2: Vec::with_capacity(1 << 24), td: Vec::new(), sd: Vec::new() }).collect();
        crate::perf_on(true);
        let tw = Instant::now();
        let starts = partition(&ems, p, threads, &mut out);
        let t_part = tw.elapsed().as_secs_f64();
        let next = AtomicUsize::new(0);
        let outs: Vec<std::sync::Mutex<UOut>> = (0..nu).map(|_| std::sync::Mutex::new(UOut::default())).collect();
        let mut order: Vec<usize> = (0..nu).collect();
        order.sort_by_key(|&u| std::cmp::Reverse(starts[u + 1] - starts[u]));
        std::thread::scope(|sc| {
            for s in scr.iter_mut() {
                let (next, outs, order, starts, out, a1, a2) = (&next, &outs, &order, &starts, &out, &a1, &a2);
                sc.spawn(move || loop {
                    let k = next.fetch_add(1, Ordering::Relaxed); if k >= nu { break; }
                    let u = order[k];
                    *outs[u].lock().unwrap() = run_unit(&p.rest[u], &out[starts[u]..starts[u + 1]], v, s, (v > 0).then_some((a1, a2)));
                });
            }
        });
        let wall = tw.elapsed().as_secs_f64();
        crate::perf_on(false);
        let outs: Vec<UOut> = outs.into_iter().map(|m| m.into_inner().unwrap()).collect();
        let e = d.n_edges as f64;
        let sum = |f: &dyn Fn(&UOut) -> u64| outs.iter().map(f).sum::<u64>();
        let (new, nb, ins, e1, e2) = (sum(&|o| o.new), sum(&|o| o.bundles), sum(&|o| o.ins), sum(&|o| o.e1), sum(&|o| o.e2));
        let (b1, b2, tcap) = (sum(&|o| o.b1.1 as u64), sum(&|o| o.b2.1 as u64), sum(&|o| 40 * o.cap));
        let load = std::fs::read_to_string("/proc/loadavg").unwrap_or_default();
        println!("  RUN {} threads {threads:>2} split {split}: WALL {wall:.3} s = partition {t_part:.3} s ({:.2} ns an edge) + units {:.3} s; summed over units: dedup{} {:.2} ns an edge{}; new {new} ({}){}; load {}",
            w[0], t_part * 1e9 / e, wall - t_part, if v > 0 { " + edge insert" } else { "" }, ins as f64 / e,
            if v > 0 { format!(", encode v{v}: sort + E1 {:.2}, E2 {:.2} ns an edge", e1 as f64 / e, e2 as f64 / e) } else { String::new() },
            if v == 4 { "no dedup here" } else if new == 6_735_699 { "OK" } else { "MISMATCH" },
            if v > 0 { format!("; {nb} bundles, in memory 40 B an entry: {:.2} B an edge dense, {:.2} at the tables' final sizes; blocks E1 {:.3} E2 {:.3} B an edge", 40.0 * nb as f64 / e, tcap as f64 / e, b1 as f64 / e, b2 as f64 / e) } else { String::new() },
            load.split_whitespace().next().unwrap_or("?"));
        if v > 0 {
            if !kept.contains_key(&v) { check(&d, p, v, &a1, &a2, &outs, &key_index, &dense); }
            let _ = starts;
            kept.insert(v, Kept { a1, a2, outs, unit_region: p.unit_region.clone() });
        }
    }
}

/// Untimed: every block of order `v` decoded (E1 and E2) back to edges -
/// count and fingerprint against the capture's - and zstd -1 per block.
#[allow(clippy::too_many_arguments)]
fn check(d: &Data, p: &Plan, v: usize, a1: &Arena, a2: &Arena, outs: &[UOut], key_index: &[FxHashMap<u64, u32>], dense: &[u32]) {
    let tv = Instant::now();
    let next = AtomicUsize::new(0);
    let res: Vec<[u64; 6]> = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..16).map(|_| { let next = &next; sc.spawn(move || {
            let mut r = [0u64; 6];
            loop {
                let u = next.fetch_add(1, Ordering::Relaxed); if u >= outs.len() { break; }
                let reg = &d.regions[p.unit_region[u] as usize];
                for (k, (a, (o, l))) in [(a1, outs[u].b1), (a2, outs[u].b2)].into_iter().enumerate() {
                    let b = &a.buf[o..o + l];
                    r[4 + k] += zstd::bulk::compress(b, 1).unwrap().len() as u64;
                    decode(b, v, k == 1, |t, s, x, off, mask| {
                        let mut m = mask;
                        while m != 0 {
                            let bit = m.trailing_zeros(); m &= m - 1;
                            // Home cell; the other side's by the offset.
                            let hcell = cell_of(reg.slot, bit);
                            let (scell, tcell, tshape, tkey) = if v == 4 { (hcell, fwd(hcell, off), t >> 56, t & ((1 << 56) - 1)) } else { (back(hcell, off), hcell, reg.shape as u64, t) };
                            let (sslot, sl) = slot_local(scell);
                            let srid = (s >> SK) as u32 * SLOTS + sslot;
                            let sd = dense[srid as usize] as usize;
                            let se = key_index[sd][&(s & ((1 << SK) - 1))];
                            r[2 * k] = r[2 * k].wrapping_add(d.term((srid as u64) << 38 | (se as u64) << 8 | sl as u64, tshape, tcell, tkey, x));
                            r[2 * k + 1] += 1;
                        }
                    });
                }
            }
            r
        }) }).collect();
        hs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    let mut t = [0u64; 6];
    for r in res { for k in 0..6 { t[k] = if k % 2 == 0 && k < 4 { t[k].wrapping_add(r[k]) } else { t[k] + r[k] }; } }
    let e = d.n_edges as f64;
    let ok = |fp: u64, n: u64| if fp == d.fp && n == d.n_edges { "EQUAL" } else { "MISMATCH" };
    println!("    CHECK v{v}: E1 {} edges, fingerprint {:016x} ({}); E2 {} edges, {:016x} ({}); at rest per unit block: E1 {:.3} B an edge, zstd -1 {:.3}; E2 {:.3}, zstd -1 {:.3}; check {:.1} s",
        t[1], t[0], ok(t[0], t[1]), t[3], t[2], ok(t[2], t[3]), a1.at.load(Ordering::Relaxed) as f64 / e, t[4] as f64 / e, a2.at.load(Ordering::Relaxed) as f64 / e, t[5] as f64 / e, tv.elapsed().as_secs_f64());
}

/// A run's blocks kept for the access pass.
struct Kept { a1: Arena, a2: Arena, outs: Vec<UOut>, unit_region: Vec<u32> }

/// The backward's read, a batched region pass: every block of each kept
/// order decoded per in-edge to (source key + cell, transfer) (v4: per
/// out-edge to (target key + cell, transfer)); then, for the target-side
/// orders, the same with W KEYED BY STATE - a probe of the unit's in-core
/// table (target key -> its W slots) per bundle whose target key changed.
fn access(d: &Data, p: &Plan, kept: &FxHashMap<usize, Kept>, threads: usize) {
    // Per target unit its in-core table of target keys (untimed): its entries.
    let tv = kept.keys().copied().filter(|&v| v < 4).min();
    let tabs: Vec<(Vec<H>, usize)> = match tv { None => Vec::new(), Some(tv) => (0..p.unit_region.len()).map(|u| {
        let rest = &p.rest[u];
        let mut i = 0; let n = get_varint(rest, &mut i) as usize;
        let mut keys: Vec<u64> = Vec::with_capacity(n);
        let mut k = 0u64; for _ in 0..n { k += get_varint(rest, &mut i); i += 8; keys.push(k); }
        let kp = &kept[&tv];
        let (o, l) = kp.outs[u].b1;
        decode(&kp.a1.buf[o..o + l], tv, false, |t, _, _, _, _| keys.push(t));
        keys.sort_unstable(); keys.dedup();
        let cap = (keys.len() * 2).next_power_of_two().max(16);
        let mut tab = vec![H { key: EMPTY, m: 0, nm: 0, lid: 0 }; cap];
        for (j, &k) in keys.iter().enumerate() { let q = crate::edgebundle::hslot(&tab, cap - 1, k); tab[q] = H { key: k, m: 0, nm: 0, lid: j as u32 }; }
        (tab, cap - 1)
    }).collect() };
    let mut vs: Vec<usize> = kept.keys().copied().collect(); vs.sort();
    for v in vs {
        let kp = &kept[&v];
        for (enc, a) in [("E1", &kp.a1), ("E2", &kp.a2)] {
            for probe in if v == 4 { vec![false] } else { vec![false, true] } {
                let next = AtomicUsize::new(0);
                let t = Instant::now();
                let res: Vec<(u64, u64, u64)> = std::thread::scope(|sc| {
                    let hs: Vec<_> = (0..threads).map(|_| { let (next, tabs) = (&next, &tabs); sc.spawn(move || {
                        let (mut n, mut acc, mut probes) = (0u64, 0u64, 0u64);
                        loop {
                            let u = next.fetch_add(1, Ordering::Relaxed); if u >= kp.outs.len() { break; }
                            let reg = &d.regions[kp.unit_region[u] as usize];
                            let (o, l) = if enc == "E1" { kp.outs[u].b1 } else { kp.outs[u].b2 };
                            let (mut last_t, mut slot) = (u64::MAX, 0u32);
                            decode(&a.buf[o..o + l], v, enc == "E2", |tk, s, x, off, m| {
                                if probe && tk != last_t { let q = crate::edgebundle::hslot(&tabs[u].0, tabs[u].1, tk); slot = tabs[u].0[q].lid; last_t = tk; probes += 1; }
                                let mut mm = m;
                                while mm != 0 {
                                    let bit = mm.trailing_zeros(); mm &= mm - 1;
                                    let (other, oc) = if v == 4 { (tk, fwd(cell_of(reg.slot, bit), off)) } else { (s, back(cell_of(reg.slot, bit), off)) };
                                    acc = acc.wrapping_add(other ^ (oc as u64) << 7 ^ x ^ (slot as u64) << 40 ^ bit as u64);
                                    n += 1;
                                }
                            });
                        }
                        (n, acc, probes)
                    }) }).collect();
                    hs.into_iter().map(|h| h.join().unwrap()).collect()
                });
                let wall = t.elapsed().as_secs_f64();
                let n: u64 = res.iter().map(|r| r.0).sum();
                let probes: u64 = res.iter().map(|r| r.2).sum();
                std::hint::black_box(res.iter().fold(0u64, |a, r| a ^ r.1));
                println!("    ACCESS v{v} {enc} {threads:>2} threads{}: {n} {} in {wall:.3} s ({:.2} ns an edge)",
                    if probe { format!(" + W keyed by state ({probes} target-key probes)") } else { String::new() },
                    if v == 4 { "out-edges to (target key + cell, transfer)" } else { "in-edges to (source key + cell, transfer)" }, wall * 1e9 / n.max(1) as f64);
            }
        }
    }
}
