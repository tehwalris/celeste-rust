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
//! partition, reported apart), and v5 (source key, offset, transfer, target
//! key) in the target region.
//!
//! Then `sweep:16`: EVERY order of (s, t, x, o), and each after the mask
//! (`M`: bundles grouped by identical mask), on each placement's last run;
//! the best three target-side orders also with masks RELATIVE to the
//! source entry's frontier mask (shifted by the offset and clipped to the
//! region; `ref_mask`); every block decoded back. `push:T`: the backward's
//! reads from each placement's best blocks - every edge, and the in-edges
//! of a sample of changed targets.
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
    match v { 1 => [t, x, s, off], 2 => [t, s, x, off], 3 => [s, x, t, off], 4 => [s, t, x, off], _ => [s, off, x, t] }
}
/// Which fields are the target and the source key.
fn key_fields(v: usize) -> (usize, usize) { match v { 1 => (0, 2), 2 => (0, 1), 3 => (2, 0), 4 => (1, 0), _ => (3, 0) } }
/// The transfer's field.
fn x_field(v: usize) -> usize { match v { 2 | 4 | 5 => 2, _ => 1 } }
/// The offset's field.
fn off_field(v: usize) -> usize { if v == 5 { 1 } else { 3 } }

/// Level-byte flags of a relative-mask block: the mask is the reference;
/// the mask is an explicit u64 (no reference).
const EQ: u8 = 0x80;
const RAW: u8 = 0x40;

/// One trie record: the level (first changed field) with `flags`, its delta,
/// the later fields raw (the offset as its byte). The mask follows (the caller's).
#[inline]
fn put_rec(o: &mut Vec<u8>, prev: &mut Option<[u64; 4]>, f: &[u64; 4], offi: usize, flags: u8) {
    let l = match prev { None => 0, Some(p) => (0..4).find(|&i| f[i] != p[i]).unwrap() };
    o.push(l as u8 | flags);
    for i in l..4 {
        if i == offi { put_off(o, f[i] as u32); }
        else if i == l && prev.is_some() { varint(o, f[i] - prev.unwrap()[i]); }
        else { varint(o, f[i]); }
    }
    *prev = Some(*f);
}

/// A record's fields into `prev`; its flags.
fn get_rec(b: &[u8], i: &mut usize, prev: &mut [u64; 4], first: bool, offi: usize) -> u8 {
    let lb = b[*i]; *i += 1;
    let l = (lb & 3) as usize;
    for k in l..4 {
        if k == offi { prev[k] = get_off(b, i) as u64; }
        else if k == l && !first { prev[k] += get_varint(b, i); }
        else { prev[k] = get_varint(b, i); }
    }
    lb & 0xc0
}

#[inline] fn get_u64(b: &[u8], i: &mut usize) -> u64 { let m = u64::from_le_bytes(b[*i..*i + 8].try_into().unwrap()); *i += 8; m }

/// The table's entries as a sorted trie in order `v`: E1 into `enc1`, E2
/// into `enc2` (u64 masks); the nanoseconds of (collect + sort + E1, E2).
fn encode(s: &mut Scratch, cap: usize, v: usize) -> (u64, u64, u64) {
    let t0 = Instant::now();
    let offi = off_field(v);
    s.tup.clear();
    for e in &s.et[..cap] { if e.gen == s.gen { s.tup.push((fields(v, e.t, e.s, e.xo), e.mask)); } }
    s.tup.sort_unstable_by_key(|x| x.0);
    s.enc1.clear();
    varint(&mut s.enc1, s.tup.len() as u64);
    let mut prev = None;
    for (f, m) in &s.tup { put_rec(&mut s.enc1, &mut prev, f, offi, 0); s.enc1.extend_from_slice(&m.to_le_bytes()); }
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
        put_rec(&mut s.enc2, &mut prev, &g, offi, 0);
        s.enc2.extend_from_slice(&m.to_le_bytes());
    }
    (t1, t0.elapsed().as_nanos() as u64, s.tup.len() as u64)
}

/// The frontier: per (source region, key) the entry's cells in the frame's
/// sources (the layer before).
type Front = FxHashMap<(u32, u64), u64>;

/// A bundle's REFERENCE mask: its source entry's frontier cells - in the
/// target region (`!source_side`), shifted by the offset into this region's
/// cells and clipped to them (the source cells may lie in up to four source
/// regions: one lookup each); in the source region, the entry's own.
/// `None` where an offset code names no shift (`NO_CELL`).
fn ref_mask(front: &Front, source_side: bool, home_slot: u32, s: u64, off: u32) -> Option<u64> {
    let (sh, key) = ((s >> SK) as u32, s & ((1 << SK) - 1));
    if source_side { return front.get(&(sh * SLOTS + home_slot, key)).copied(); }
    if off >= 1 << 22 || home_slot == SLOTS - 1 { return None; }
    let (dx, dy) = ((off >> 11) as i32 - 1024, (off & 2047) as i32 - 1024);
    let (tx0, ty0) = ((home_slot % 64) as i32 * 8, (home_slot / 64) as i32 * 8);
    let (sx0, sy0) = (tx0 - dx, ty0 - dy);
    let mut r = 0u64;
    for sry in sy0.div_euclid(8)..=(sy0 + 7).div_euclid(8) {
        for srx in sx0.div_euclid(8)..=(sx0 + 7).div_euclid(8) {
            if !(0..64).contains(&srx) || !(0..64).contains(&sry) { continue; }
            let Some(&m) = front.get(&(sh * SLOTS + (sry * 64 + srx) as u32, key)) else { continue };
            let shc = sx0 - srx * 8;
            for ly in 0..8 {
                let gy = sy0 + ly;
                if gy.div_euclid(8) != sry { continue; }
                let row = (m >> ((gy - sry * 8) * 8)) & 0xff;
                let tb = if shc >= 0 { row >> shc } else { (row << -shc) & 0xff };
                r |= tb << (ly * 8);
            }
        }
    }
    Some(r)
}

/// `m` over `r`'s set bits, as `ceil(popcount(r) / 8)` bytes.
fn put_rel(o: &mut Vec<u8>, r: u64, m: u64) {
    let k = r.count_ones() as usize;
    let (mut bits, mut j, mut rr) = (0u64, 0, r);
    while rr != 0 { let c = rr.trailing_zeros(); rr &= rr - 1; if m >> c & 1 == 1 { bits |= 1 << j; } j += 1; }
    o.extend_from_slice(&bits.to_le_bytes()[..k.div_ceil(8)]);
}
fn get_rel(b: &[u8], i: &mut usize, r: u64) -> u64 {
    let k = r.count_ones() as usize;
    let n = k.div_ceil(8);
    let mut w = [0u8; 8]; w[..n].copy_from_slice(&b[*i..*i + n]); *i += n;
    let bits = u64::from_le_bytes(w);
    let (mut m, mut j, mut rr) = (0u64, 0, r);
    while rr != 0 { let c = rr.trailing_zeros(); rr &= rr - 1; if bits >> j & 1 == 1 { m |= 1 << c; } j += 1; }
    m
}

/// Decode a block of order `v` (`e2`: with its dictionaries) to
/// `f(target key, source key, transfer, offset, mask)`.
fn decode(b: &[u8], v: usize, e2: bool, mut f: impl FnMut(u64, u64, u64, u32, u64)) {
    let mut i = 0;
    let (ti, si, xi, offi) = { let (a, c) = key_fields(v); (a, c, x_field(v), off_field(v)) };
    let mut dicts: [Vec<u64>; 2] = [Vec::new(), Vec::new()];
    if e2 { for d in dicts.iter_mut() { let n = get_varint(b, &mut i); let mut p = 0u64; for _ in 0..n { p += get_varint(b, &mut i); d.push(p); } } }
    let n = get_varint(b, &mut i);
    let mut prev = [0u64; 4];
    for k in 0..n {
        get_rec(b, &mut i, &mut prev, k == 0, offi);
        let (mut t, mut s) = (prev[ti], prev[si]);
        if e2 { t = dicts[0][t as usize]; s = dicts[1][s as usize]; }
        let m = get_u64(b, &mut i);
        f(t, s, prev[xi], prev[offi] as u32, m);
    }
    assert_eq!(i, b.len(), "a block did not decode to its end");
}

/// A field of a block's sort ORDER: source key, target key, transfer,
/// offset, mask.
#[derive(Clone, Copy, PartialEq, Debug)] enum F { S, T, X, O, M }
/// A bundle: target key, source key, transfer, offset, mask.
type Tup = [u64; 5];
fn fidx(f: F) -> usize { match f { F::T => 0, F::S => 1, F::X => 2, F::O => 3, F::M => 4 } }
fn order_name(o: &[F]) -> String { o.iter().map(|f| match f { F::S => "s", F::T => "t", F::X => "x", F::O => "o", F::M => "M" }).collect::<Vec<_>>().join(" ") }

/// Every order: the 24 permutations of (s, t, x, o) with the mask last, and
/// each after the mask (bundles grouped by identical mask first).
fn all_orders() -> Vec<Vec<F>> {
    let base = [F::S, F::T, F::X, F::O];
    let mut perms = Vec::new();
    for a in 0..4 { for b in 0..4 { for c in 0..4 { for d in 0..4 {
        let i = [a, b, c, d];
        if (0..4).all(|k| (0..4).filter(|&j| i[j] == k).count() == 1) { perms.push(i.iter().map(|&k| base[k]).collect::<Vec<F>>()); }
    } } } }
    let mut out = perms.clone();
    out.extend(perms.into_iter().map(|p| { let mut v = vec![F::M]; v.extend(p); v }));
    out
}

/// A block of `tups` in `order` (the state study's form: sorted, the keys as
/// ranks into the block's sorted dictionaries, a trie record per bundle - a
/// level byte, the first changed field's delta, the later fields raw, the
/// offset a byte, the mask a u64 - the mask last unless the order has it).
/// `rel` (the mask last): masks RELATIVE to their reference (`ref_mask`):
/// `EQ` where equal, the bits over its cells, `RAW` + u64 without one.
/// Returns the block and (equal, relative, raw, violations).
fn encode_order(tups: &[Tup], order: &[F], rel: Option<(&Front, bool, u32)>) -> (Vec<u8>, [u64; 4]) {
    let mut td: Vec<u64> = tups.iter().map(|x| x[0]).collect(); td.sort_unstable(); td.dedup();
    let mut sd: Vec<u64> = tups.iter().map(|x| x[1]).collect(); sd.sort_unstable(); sd.dedup();
    let mut r: Vec<(Vec<u64>, usize)> = tups.iter().enumerate().map(|(k, x)| {
        let ranked = [td.binary_search(&x[0]).unwrap() as u64, sd.binary_search(&x[1]).unwrap() as u64, x[2], x[3], x[4]];
        (order.iter().map(|&f| ranked[fidx(f)]).collect(), k)
    }).collect();
    r.sort_unstable();
    let mut o = Vec::with_capacity(tups.len() * 12);
    for d in [&td, &sd] { varint(&mut o, d.len() as u64); let mut p = 0u64; for &k in d.iter() { varint(&mut o, k - p); p = k; } }
    varint(&mut o, r.len() as u64);
    let has_m = order.contains(&F::M);
    let mut st = [0u64; 4];
    let mut prev: Option<&Vec<u64>> = None;
    for (vals, k) in &r {
        let l = match prev { None => 0, Some(p) => (0..vals.len()).find(|&i| vals[i] != p[i]).unwrap() };
        let m = tups[*k][4];
        // The mask's form, decided before the level byte that flags it.
        let (flags, payload) = match (has_m, rel) {
            (true, _) | (false, None) => (0u8, None),
            (false, Some((front, side, slot))) => match ref_mask(front, side, slot, tups[*k][1], tups[*k][3] as u32) {
                Some(rm) if m & !rm == 0 => if m == rm { st[0] += 1; (EQ, Some(None)) } else { st[1] += 1; (0, Some(Some(rm))) },
                rm => { st[2] += 1; if rm.is_some() { st[3] += 1; } (RAW, None) }
            },
        };
        o.push(l as u8 | flags);
        for i in l..vals.len() {
            match order[i] {
                F::O => put_off(&mut o, vals[i] as u32),
                F::M => o.extend_from_slice(&vals[i].to_le_bytes()),
                _ => if i == l && prev.is_some() { varint(&mut o, vals[i] - prev.unwrap()[i]) } else { varint(&mut o, vals[i]) },
            }
        }
        if !has_m {
            match payload { Some(None) => {}, Some(Some(rm)) => put_rel(&mut o, rm, m), None => o.extend_from_slice(&m.to_le_bytes()) }
        }
        prev = Some(vals);
    }
    (o, st)
}

/// `encode_order` inverted: `f` per bundle (raw keys).
fn decode_order(b: &[u8], order: &[F], rel: Option<(&Front, bool, u32)>, mut f: impl FnMut(&Tup)) {
    let mut i = 0;
    let mut dicts: [Vec<u64>; 2] = [Vec::new(), Vec::new()];
    for d in dicts.iter_mut() { let n = get_varint(b, &mut i); let mut p = 0u64; for _ in 0..n { p += get_varint(b, &mut i); d.push(p); } }
    let n = get_varint(b, &mut i);
    let has_m = order.contains(&F::M);
    let mut vals = [0u64; 5];
    for k in 0..n {
        let lb = b[i]; i += 1;
        let l = (lb & 7) as usize;
        for j in l..order.len() {
            vals[j] = match order[j] {
                F::O => get_off(b, &mut i) as u64,
                F::M => get_u64(b, &mut i),
                _ => if j == l && k > 0 { vals[j] + get_varint(b, &mut i) } else { get_varint(b, &mut i) },
            };
        }
        let mut t: Tup = [0; 5];
        for (j, &fl) in order.iter().enumerate() { t[fidx(fl)] = vals[j]; }
        t[0] = dicts[0][t[0] as usize]; t[1] = dicts[1][t[1] as usize];
        if !has_m {
            t[4] = match rel {
                None => get_u64(b, &mut i),
                Some((front, side, slot)) => if lb & RAW != 0 { get_u64(b, &mut i) } else {
                    let rm = ref_mask(front, side, slot, t[1], t[3] as u32).expect("a relative mask without a reference");
                    if lb & EQ != 0 { rm } else { get_rel(b, &mut i, rm) }
                },
            };
        }
        f(&t);
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
    let front: Front = d.regions.iter().flat_map(|r| { let rid = d_region_id(r); r.keys.iter().zip(&r.front).filter(|(_, &m)| m != 0).map(move |(&k, &m)| ((rid, k), m)) }).collect();
    // Per placement (source side?), its last run's bundles.
    let mut placed: FxHashMap<bool, Placed> = FxHashMap::default();
    let mut checked: Vec<usize> = Vec::new();
    for run in args {
        let w: Vec<&str> = run.split(':').collect();
        let threads: usize = w[1].parse().unwrap();
        if w[0] == "sweep" { for pl in placed.values_mut() { sweep(pl, &front); } continue; }
        if w[0] == "push" { push(&d, &placed, &front, threads); continue; }
        let v: usize = match w[0] { "dedup" => 0, "v1" => 1, "v2" => 2, "v3" => 3, "v4" => 4, "v5" => 5, m => panic!("mode {m}") };
        let split: usize = w[2].parse().unwrap();
        let by_source = v == 4;
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
        let (a1, a2) = if v > 0 { (arena(d.n_edges as usize * 4), arena(d.n_edges as usize * 5 / 2)) } else { (arena(0), arena(0)) };
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
            if !checked.contains(&v) { check(&d, p, v, &a1, &a2, &outs, &key_index, &dense); checked.push(v); }
            // The placement's bundles, for the sweep and the push (untimed).
            let tups: Vec<Vec<Tup>> = outs.iter().map(|o| { let mut t = Vec::new(); decode(&a2.buf[o.b2.0..o.b2.0 + o.b2.1], v, true, |tk, sk, x, off, m| t.push([tk, sk, x, off as u64, m])); t }).collect();
            let slots = p.unit_region.iter().map(|&r| d.regions[r as usize].slot).collect();
            let shapes = p.unit_region.iter().map(|&r| d.regions[r as usize].shape).collect();
            let _ = starts;
            placed.insert(by_source, Placed { source_side: by_source, slots, shapes, tups, edges: d.n_edges, best: None });
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

/// A placement's bundles after its last run (decoded from its blocks,
/// untimed), per unit with its home region's slot and shape.
struct Placed { source_side: bool, slots: Vec<u32>, shapes: Vec<u32>, tups: Vec<Vec<Tup>>, edges: u64, best: Option<(Vec<F>, bool, Vec<Vec<u8>>)> }

fn tup_hash(t: &Tup) -> u64 { t.iter().fold(0x51ed_270b_27a4_c7b1u64, |a, &x| crate::bits::mix64(a ^ x).wrapping_add(x)) }

/// Every order (and the relative masks in the best three of the target side)
/// on a placement's bundles: B an edge raw and with zstd -1 per unit block,
/// each block decoded back to its bundles (an order-free hash); the best
/// encoding's blocks kept.
fn sweep(pl: &mut Placed, front: &Front) {
    let ts = Instant::now();
    let e = pl.edges as f64;
    let want: Vec<u64> = pl.tups.iter().map(|v| v.iter().fold(0u64, |a, t| a.wrapping_add(tup_hash(t)))).collect();
    let run = |order: &[F], rel: bool, keep: bool| -> (u64, u64, [u64; 4], Vec<Vec<u8>>) {
        let next = AtomicUsize::new(0);
        let res: Vec<(u64, u64, [u64; 4], Vec<(usize, Vec<u8>)>)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..16).map(|_| { let (next, want, pl) = (&next, &want, &*pl); sc.spawn(move || {
                let (mut raw, mut z, mut st, mut kept) = (0u64, 0u64, [0u64; 4], Vec::new());
                loop {
                    let u = next.fetch_add(1, Ordering::Relaxed); if u >= pl.tups.len() { break; }
                    let rl = rel.then_some((front, pl.source_side, pl.slots[u]));
                    let (b, s4) = encode_order(&pl.tups[u], order, rl);
                    for k in 0..4 { st[k] += s4[k]; }
                    raw += b.len() as u64; z += zstd::bulk::compress(&b, 1).unwrap().len() as u64;
                    let mut h = 0u64;
                    decode_order(&b, order, rl, |t| h = h.wrapping_add(tup_hash(t)));
                    assert_eq!(h, want[u], "order {}: unit {u} decodes to other bundles", order_name(order));
                    if keep { kept.push((u, b)); }
                }
                (raw, z, st, kept)
            }) }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let (mut raw, mut z, mut st) = (0u64, 0u64, [0u64; 4]);
        let mut blocks: Vec<Vec<u8>> = vec![Vec::new(); pl.tups.len()];
        for (r, zz, s4, kept) in res { raw += r; z += zz; for k in 0..4 { st[k] += s4[k]; } for (u, b) in kept { blocks[u] = b; } }
        (raw, z, st, blocks)
    };
    let side = if pl.source_side { "source" } else { "target" };
    let mut rows: Vec<(f64, f64, String, Vec<F>)> = Vec::new();
    for o in all_orders() { let (raw, z, _, _) = run(&o, false, false); rows.push((z as f64 / e, raw as f64 / e, order_name(&o), o)); }
    rows.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    println!("    SWEEP {side} region ({} orders, every block decoded back: OK; {:.0} s): B an edge raw / zstd -1, best first:", rows.len(), ts.elapsed().as_secs_f64());
    for ch in rows.chunks(4) { println!("      {}", ch.iter().map(|r| format!("[{}] {:.3} / {:.3}", r.2, r.1, r.0)).collect::<Vec<_>>().join("   ")); }
    // The relative masks in the best three orders with the mask last.
    let mut best = (rows[0].0, rows[0].3.clone(), false);
    for r in rows.iter().filter(|r| !r.3.contains(&F::M)).take(3) {
        let (raw, z, st, _) = run(&r.3, true, false);
        let n: u64 = st[..3].iter().sum();
        println!("    REL {side} [{}]: {:.3} / {:.3} B an edge (u64 masks {:.3} / {:.3}); masks equal to the reference {:.1}%, relative {:.1}%, raw (no shift) {:.1}%, outside it {}",
            r.2, raw as f64 / e, z as f64 / e, r.1, r.0, 100.0 * st[0] as f64 / n as f64, 100.0 * st[1] as f64 / n as f64, 100.0 * st[2] as f64 / n as f64, st[3]);
        if (z as f64 / e) < best.0 { best = (z as f64 / e, r.3.clone(), true); }
    }
    let (_, _, _, blocks) = run(&best.1, best.2, true);
    println!("    BEST {side} region: [{}]{} {:.3} B an edge with zstd -1", order_name(&best.1), if best.2 { " relative masks" } else { "" }, best.0);
    pl.best = Some((best.1, best.2, blocks));
}

/// The backward's reads: (1) every in-edge (target side; the source side's
/// every out-edge) decoded from the best blocks, a region pass; (2) the PUSH
/// backward's read - the in-edges of a sample of CHANGED targets: the target
/// side decodes only the units holding one and filters each bundle's mask;
/// the source side reads every block and filters each bundle by its target.
fn push(d: &Data, placed: &FxHashMap<bool, Placed>, front: &Front, threads: usize) {
    let pass = |pl: &Placed, f: &(dyn Fn(usize, &Tup, &mut u64) + Sync), units: &(dyn Fn(usize) -> bool + Sync)| -> (f64, u64) {
        let (order, rel, blocks) = pl.best.as_ref().expect("a sweep before the push");
        let next = AtomicUsize::new(0);
        let t = Instant::now();
        let n: u64 = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads).map(|_| { let next = &next; sc.spawn(move || {
                let mut n = 0u64;
                loop {
                    let u = next.fetch_add(1, Ordering::Relaxed); if u >= blocks.len() { break; }
                    if !units(u) { continue; }
                    decode_order(&blocks[u], order, rel.then_some((front, pl.source_side, pl.slots[u])), |tp| f(u, tp, &mut n));
                }
                n
            }) }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).sum()
        });
        (t.elapsed().as_secs_f64(), n)
    };
    // (1) every edge, the other side's cell and key per edge.
    for (&side, pl) in placed.iter() {
        let (w, n) = pass(pl, &|u, tp, n| {
            let mut m = tp[4];
            while m != 0 { let bit = m.trailing_zeros(); m &= m - 1; let c = cell_of(pl.slots[u], bit); let oc = if side { fwd(c, tp[3] as u32) } else { back(c, tp[3] as u32) }; *n += 1 + (oc as u64 & 0); }
        }, &|_| true);
        let (o, rel, _) = pl.best.as_ref().unwrap();
        println!("    READ {} region [{}]{}, {threads} threads: every {} decoded ({n}) in {w:.3} s ({:.2} ns an edge)", if side { "source" } else { "target" }, order_name(o), if *rel { " relative masks" } else { "" }, if side { "out-edge" } else { "in-edge" }, w * 1e9 / n.max(1) as f64);
    }
    let (Some(tp), Some(sp)) = (placed.get(&false), placed.get(&true)) else { return };
    for every in [30u64, 1000] {
        // The changed targets (untimed): 1 in `every` of the targets with in-edges.
        let mut rng = 0x9E37_79B9_7F4A_7C15u64;
        let mut per_unit: Vec<FxHashMap<u64, u64>> = vec![FxHashMap::default(); tp.tups.len()];
        let mut global: FxHashMap<(u64, u32), ()> = FxHashMap::default();
        let mut keys: rustc_hash::FxHashSet<u64> = Default::default();
        for (u, v) in tp.tups.iter().enumerate() {
            let mut targets: Vec<(u64, u32)> = Vec::new();
            for t in v { let mut m = t[4]; while m != 0 { let b = m.trailing_zeros(); m &= m - 1; targets.push((t[0], b)); } }
            targets.sort_unstable(); targets.dedup();
            for (k, b) in targets {
                rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                if (rng >> 33) % every != 0 { continue; }
                *per_unit[u].entry(k).or_default() |= 1 << b;
                let gk = (tp.shapes[u] as u64) << 56 | k;
                global.insert((gk, cell_of(tp.slots[u], b)), ());
                keys.insert(gk);
            }
        }
        let n_changed = global.len();
        let (wt, nt) = pass(tp, &|u, t, n| { if let Some(&cm) = per_unit[u].get(&t[0]) { *n += (cm & t[4]).count_ones() as u64; } }, &|u| !per_unit[u].is_empty());
        let units_read = per_unit.iter().filter(|m| !m.is_empty()).count();
        let (ws, ns) = pass(sp, &|u, t, n| {
            if !keys.contains(&t[0]) { return; }
            let mut m = t[4];
            while m != 0 { let b = m.trailing_zeros(); m &= m - 1; if global.contains_key(&(t[0], fwd(cell_of(sp.slots[u], b), t[3] as u32))) { *n += 1; } }
        }, &|_| true);
        println!("    PUSH {threads} threads, {n_changed} changed targets (1 in {every}): target side {nt} in-edges from {units_read} of {} units in {wt:.4} s; source side {ns} in-edges, every unit read, in {ws:.4} s{}",
            tp.tups.len(), if nt == ns { "" } else { " (COUNTS DIFFER)" });
    }
    let _ = d;
}
