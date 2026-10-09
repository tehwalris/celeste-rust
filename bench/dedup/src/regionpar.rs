//! `regionpar`: regionbatch's per-region pipeline (decode, hash in-core
//! batch, re-encode) on ALL cores. Units = regions (optionally a heavy
//! region's batch split by a hash of the key into disjoint sub-batches, each
//! with the matching subset of the region's stored entries), scheduled
//! heaviest first by an atomic index (`steal`) or split up front (`static`,
//! greedy longest-processing-time over the threads). Dedup only.
//!
//! Args: R (8 | 16) fmt (varint | zstd1) then a list of runs "T:sched:split"
//! (split = max lookups a unit, 0 = none); all runs in one process.

use crate::bits::{packing, Fields, QB};
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

fn varint(o: &mut Vec<u8>, mut v: u32) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u32 { let mut v = 0u32; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u32) << s; if x < 0x80 { return v; } s += 7; } }

fn encode<const W: usize>(e: &[(u32, [u64; W])], o: &mut Vec<u8>) {
    varint(o, e.len() as u32);
    let mut prev = 0u32;
    for (k, m) in e { varint(o, k - prev); prev = *k; for x in m { o.extend_from_slice(&x.to_le_bytes()); } }
}
fn decode<const W: usize>(b: &[u8], e: &mut Vec<(u32, [u64; W])>) {
    e.clear();
    let mut i = 0;
    let n = get_varint(b, &mut i);
    let mut k = 0u32;
    for _ in 0..n {
        k += get_varint(b, &mut i);
        let mut m = [0u64; W];
        for x in m.iter_mut() { *x = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8; }
        e.push((k, m));
    }
}

#[derive(Clone, Copy)] struct H<const W: usize> { key: u32, m: [u64; W] }

/// One unit: its at-rest bytes, its batch (key, pos), and its query indices
/// (for the untimed check).
struct Unit { rest: Vec<u8>, batch: Vec<(u32, u32)>, qi: Vec<u32> }

struct Scratch<const W: usize> { dec: Vec<(u32, [u64; W])>, tab: Vec<H<W>>, plain: Vec<u8>, zc: zstd::bulk::Compressor<'static>, zd: zstd::bulk::Decompressor<'static>, out: Vec<u8>, dec_out: Vec<u8> }

/// The pipeline on one unit; returns (new states, the decision bits in batch
/// order written into `newbits`).
fn run_unit<const W: usize>(u: &Unit, zstd_: bool, s: &mut Scratch<W>, newbits: &mut [u64]) -> u64 {
    // Decode.
    if zstd_ { s.plain.clear(); let n = s.zd.decompress_to_buffer(&u.rest, &mut s.dec_out[..]).unwrap(); decode(&s.dec_out[..n], &mut s.dec); }
    else { decode(&u.rest, &mut s.dec); }
    // In-core table, sized for the stored entries + the batch's possible new keys (bounded).
    let cap = ((s.dec.len() + u.batch.len().min(4 * s.dec.len() + 64)) * 2).next_power_of_two().max(16);
    s.tab.clear(); s.tab.resize(cap, H { key: u32::MAX, m: [0; W] });
    let mut mask = cap - 1;
    #[inline(always)] fn slot<const W: usize>(tab: &[H<W>], mask: usize, k: u32) -> usize { let mut i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & mask; loop { let x = tab[i].key; if x == k || x == u32::MAX { return i; } i = (i + 1) & mask; } }
    for (k, m) in &s.dec { let i = slot(&s.tab, mask, *k); s.tab[i] = H { key: *k, m: *m }; }
    let mut used = s.dec.len();
    let mut new = 0u64;
    for (bi, &(k, p)) in u.batch.iter().enumerate() {
        let mut i = slot(&s.tab, mask, k);
        if s.tab[i].key == u32::MAX {
            if (used + 1) * 10 > s.tab.len() * 7 {
                let old: Vec<H<W>> = s.tab.drain(..).filter(|h| h.key != u32::MAX).collect();
                let nc = (mask + 1) * 2; mask = nc - 1;
                s.tab.resize(nc, H { key: u32::MAX, m: [0; W] });
                for h in old { let t = slot(&s.tab, mask, h.key); s.tab[t] = h; }
                i = slot(&s.tab, mask, k);
            }
            s.tab[i].key = k; used += 1;
        }
        let w = &mut s.tab[i].m[(p / 64) as usize];
        let b = 1u64 << (p % 64);
        if *w & b == 0 { *w |= b; new += 1; newbits[bi / 64] |= 1 << (bi % 64); }
    }
    // Re-encode in key order.
    s.dec.clear();
    s.dec.extend(s.tab.iter().filter(|h| h.key != u32::MAX).map(|h| (h.key, h.m)));
    s.dec.sort_unstable_by_key(|x| x.0);
    s.plain.clear(); encode(&s.dec, &mut s.plain);
    if zstd_ { let n = s.zc.compress_to_buffer(&s.plain, &mut s.out[..]).unwrap(); std::hint::black_box(n); }
    else { s.out.clear(); s.out.extend_from_slice(&s.plain); }
    new
}

fn pin(cpu: usize) {
    unsafe { let mut set: libc::cpu_set_t = std::mem::zeroed(); libc::CPU_SET(cpu, &mut set); libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set); }
}

pub fn regionpar(dir: &str, n_door: usize, args: &[String]) {
    let r: i32 = args[0].parse().unwrap();
    match r { 8 => go::<1>(dir, n_door, r, args), 16 => go::<4>(dir, n_door, r, args), _ => panic!("R = 8 or 16") }
}

fn go<const W: usize>(dir: &str, n_door: usize, r: i32, args: &[String]) {
    let zstd_ = args[1] == "zstd1";
    let t0 = Instant::now();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, sm, dsm, shm, scm) = (map("qb.bin"), map("dshard.bin"), map("db.bin"), map("sshape.bin"), map("scell.bin"));
    let qs: &[QB] = crate::from_bytes(&qm); let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm);
    let (sshape, scell): (&[u32], &[u32]) = (crate::from_bytes(&shm), crate::from_bytes(&scm));
    let f = Fields::open(dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<_> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    let roles: Vec<Vec<(u64, u8)>> = packs.iter().map(|p| p.high.iter().map(|&d| (p.digits[d].radix, if p.digits[d].name == "spd.x" { 1 } else if p.digits[d].table.is_some() { 2 } else { 0 })).collect()).collect();
    let low_r: Vec<u64> = packs.iter().map(|p| p.low.map_or(1, |d| p.digits[d].radix)).collect();
    let keyof = |sh: u32, high: u32, low: u32| -> u32 {
        let s = sshape[sh as usize] as usize;
        let mut h = high as u64; let mut d = Vec::with_capacity(8);
        for &(rad, role) in roles[s].iter().rev() { d.push((h % rad, rad, role)); h /= rad; }
        d.reverse();
        let mut k = 0u64;
        for want in [0u8, 2, 1] { for &(v, rad, role) in &d { if role == want { k = k * rad + v; } } }
        u32::try_from(k * low_r[s] + low as u64).unwrap()
    };
    let mut gid: FxHashMap<(u32, i32, i32), u32> = FxHashMap::default();
    let (grp, ppos): (Vec<u32>, Vec<u32>) = (0..sshape.len()).map(|s| {
        let (x, y) = crate::structure::cell_xy(scell[s]);
        let n = gid.len() as u32;
        (*gid.entry((sshape[s], x.div_euclid(r), y.div_euclid(r))).or_insert(n), (x.rem_euclid(r) + r * y.rem_euclid(r)) as u32)
    }).unzip();
    let ng = gid.len();
    let mut ents: Vec<FxHashMap<u32, [u64; W]>> = vec![Default::default(); ng];
    for i in 0..n_door { let s = dsh[i]; let p = ppos[s as usize] as usize; let m = ents[grp[s as usize] as usize].entry(keyof(s, db[i].0, db[i].1)).or_insert([0; W]); m[p / 64] |= 1 << (p % 64); }
    let mut batches: Vec<Vec<(u32, u32, u32)>> = vec![Vec::new(); ng];
    for (qi, q) in qs.iter().enumerate() { let g = grp[q.shard as usize] as usize; batches[g].push((keyof(q.shard, q.high, q.low), ppos[q.shard as usize], qi as u32)); }
    let olds: Vec<Vec<(u32, [u64; W])>> = ents.into_iter().map(|e| { let mut v: Vec<_> = e.into_iter().collect(); v.sort_unstable_by_key(|x| x.0); v }).collect();
    let t_group = t0.elapsed().as_secs_f64();
    let rm = map("refid.bin"); let refid: &[u32] = crate::from_bytes(&rm);
    let mut seen = vec![false; n_door + 6_735_699];
    let ref_new: Vec<bool> = refid.iter().map(|&rf| { let n = rf as usize >= n_door && !seen[rf as usize]; seen[rf as usize] = true; n }).collect();
    drop(seen);
    println!("regionpar R = {r}x{r}, {}: {ng} regions, {} lookups; grouping + loading (untimed) {t_group:.1} s", args[1], qs.len());
    let hkey = |k: u32, parts: usize| (k.wrapping_mul(0x85EB_CA6B).rotate_left(13) as usize) % parts;
    for run in &args[2..] {
        let v: Vec<&str> = run.split(':').collect();
        let (threads, sched, split): (usize, &str, usize) = (v[0].parse().unwrap(), v[1], v[2].parse().unwrap());
        // Units (untimed): regions, heavy ones split by a hash of the key.
        let t = Instant::now();
        let mut units: Vec<Unit> = Vec::new();
        for g in 0..ng {
            let parts = if split == 0 { 1 } else { batches[g].len().div_ceil(split).max(1) };
            for part in 0..parts {
                let old: Vec<(u32, [u64; W])> = olds[g].iter().filter(|x| parts == 1 || hkey(x.0, parts) == part).copied().collect();
                let b: Vec<&(u32, u32, u32)> = batches[g].iter().filter(|x| parts == 1 || hkey(x.0, parts) == part).collect();
                if b.is_empty() && old.is_empty() { continue; }
                let mut rest = Vec::new(); encode(&old, &mut rest);
                if zstd_ { rest = zstd::bulk::compress(&rest, 1).unwrap(); }
                units.push(Unit { rest, batch: b.iter().map(|x| (x.0, x.1)).collect(), qi: b.iter().map(|x| x.2).collect() });
            }
        }
        units.sort_by_key(|u| std::cmp::Reverse(u.batch.len()));
        let max_cap = units.iter().map(|u| u.batch.len()).max().unwrap_or(0);
        // The largest in-core table and decoded set any unit needs (prefault exactly that).
        let mut dec = Vec::new();
        let (mut tab_cap, mut dec_max, mut plain_max) = (16usize, 0usize, 0usize);
        for u in &units {
            let raw = if zstd_ { zstd::bulk::decompress(&u.rest, 1 << 26).unwrap() } else { u.rest.clone() };
            decode::<W>(&raw, &mut dec);
            dec_max = dec_max.max(dec.len()); plain_max = plain_max.max(raw.len());
            tab_cap = tab_cap.max(((dec.len() + u.batch.len().min(4 * dec.len() + 64)) * 2).next_power_of_two());
        }
        drop(dec);
        // Decision outputs, pre-touched; per-thread scratch pre-sized and touched.
        let mut newbits: Vec<Vec<u64>> = units.iter().map(|u| vec![0u64; u.batch.len().div_ceil(64)]).collect();
        let cpus: Vec<usize> = if threads == 1 { vec![4] } else { (0..threads).collect() };
        let t_units = t.elapsed().as_secs_f64();
        let tp = Instant::now();
        let mut scr: Vec<Scratch<W>> = (0..threads).map(|_| {
            // Room for the largest unit's table and its re-encode (2x: the set grows); all touched.
            let (dn, pn) = (2 * dec_max + (1 << 16), 2 * plain_max + (1 << 20));
            let mut s = Scratch { dec: Vec::with_capacity(dn), tab: Vec::with_capacity(tab_cap), plain: Vec::with_capacity(pn), zc: zstd::bulk::Compressor::new(1).unwrap(), zd: zstd::bulk::Decompressor::new().unwrap(), out: vec![0u8; pn], dec_out: vec![0u8; pn] };
            s.tab.resize(tab_cap, H { key: 0, m: [0; W] }); s.dec.resize(dn, (0, [0; W])); s.plain.resize(pn, 0);
            s
        }).collect();
        let t_prefault = tp.elapsed().as_secs_f64();
        let scratch_mb = (tab_cap * std::mem::size_of::<H<W>>() + (2 * dec_max + (1 << 16)) * std::mem::size_of::<(u32, [u64; W])>() + 3 * (2 * plain_max + (1 << 20))) as f64 / 1e6;
        // Static split: longest-processing-time on batch length.
        let assign: Vec<usize> = { let mut load = vec![0usize; threads]; units.iter().map(|u| { let t = (0..threads).min_by_key(|&t| load[t]).unwrap(); load[t] += u.batch.len() + 1; t }).collect() };
        let next = AtomicUsize::new(0);
        let unit_t: Vec<std::sync::Mutex<f64>> = units.iter().map(|_| std::sync::Mutex::new(0.0)).collect();
        let nb_ptr: Vec<usize> = newbits.iter_mut().map(|v| v.as_mut_ptr() as usize).collect();
        crate::perf_on(true);
        let t_wall = Instant::now();
        let res: Vec<(f64, f64, u64)> = std::thread::scope(|sc| {
            let hs: Vec<_> = scr.iter_mut().enumerate().map(|(ti, s)| {
                let (units, next, assign, unit_t, nb_ptr, cpu) = (&units, &next, &assign, &unit_t, &nb_ptr, cpus[ti]);
                sc.spawn(move || {
                    pin(cpu);
                    let (mut busy, mut new) = (0f64, 0u64);
                    let mut mine = (0..units.len()).filter(|&i| assign[i] == ti);
                    loop {
                        let i = if sched == "steal" { let i = next.fetch_add(1, Ordering::Relaxed); if i >= units.len() { break; } i } else { match mine.next() { Some(i) => i, None => break } };
                        let t = Instant::now();
                        let nb = unsafe { std::slice::from_raw_parts_mut(nb_ptr[i] as *mut u64, units[i].batch.len().div_ceil(64)) };
                        new += run_unit(&units[i], zstd_, s, nb);
                        let d = t.elapsed().as_secs_f64();
                        *unit_t[i].lock().unwrap() = d; busy += d;
                    }
                    (busy, t_wall.elapsed().as_secs_f64(), new)
                })
            }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let wall = t_wall.elapsed().as_secs_f64();
        crate::perf_on(false);
        let new: u64 = res.iter().map(|x| x.2).sum();
        let busy: Vec<f64> = res.iter().map(|x| x.0).collect();
        let crit = unit_t.iter().map(|m| *m.lock().unwrap()).fold(0f64, f64::max);
        let sum_busy: f64 = busy.iter().sum();
        // Check (untimed): decisions in query order.
        let mut mine = vec![false; qs.len()];
        for (u, nb) in units.iter().zip(&newbits) { for (bi, &q) in u.qi.iter().enumerate() { mine[q as usize] = nb[bi / 64] >> (bi % 64) & 1 == 1; } }
        let bad = mine.iter().zip(&ref_new).filter(|(a, b)| a != b).count();
        let fp = mine.iter().fold(0u64, |a, &x| a.wrapping_mul(0x100_0000_01b3) ^ x as u64);
        println!("  RUN threads {threads:>2} {sched:<6} split {split:>9}: {} units (largest batch {max_cap}); WALL {wall:.3} s ({:.2} ns a lookup); busy sum {sum_busy:.2} s, per thread min {:.3} max {:.3} s, idle {:.1}%; critical path (largest unit) {crit:.3} s; new {new} ({}), decision mismatches {bad}, fingerprint {fp:016x}; units built {t_units:.1} s, per-thread scratch prefault {t_prefault:.2} s ({:.0} MB a thread)",
            units.len(), wall * 1e9 / qs.len() as f64, busy.iter().cloned().fold(f64::MAX, f64::min), busy.iter().cloned().fold(0f64, f64::max),
            100.0 * (1.0 - sum_busy / (wall * threads as f64)), if new == 6_735_699 { "OK" } else { "MISMATCH" }, scratch_mb);
        drop(scr);
    }
}
