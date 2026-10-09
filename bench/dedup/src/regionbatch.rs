//! `regionbatch R`: the visited set stored compressed per (shape, R x R
//! region) (R = 8 or 16), the frame's lookups batched per region. Per region,
//! single core: DECODE its at-rest bytes into an in-core structure, run its
//! batch (lookups + inserts), RE-ENCODE. Every region of f57 runs.
//!
//! A state inside a region: key = ((flags * dash + dash combo) * spd.x +
//! spd.x) * 96 + spd.y (flags+dash > spd.x > spd.y, mixed radix, < 2^32) and
//! pos = its cell in the region; an entry = (key, mask over the R*R cells).
//!
//! IDs: CANONICAL RANKS, final when the region finishes. An old state's id =
//! old_base[region] + its rank in the region's frame-start order (key, pos);
//! a new state's = n_door + new_base[region] + its rank among the region's
//! new states in (key, pos) order. Regions run in a fixed order, so the
//! bases are prefix sums (old: known at frame start; new: running).

use crate::bits::{check, packing, Fields, QB};
use rustc_hash::FxHashMap;
use std::time::Instant;

/// Region data: the frame-start entries (sorted by key) and the batch, in
/// sweep order: (key, pos, query index).
struct Region { old: Vec<(u32, [u64; 4])>, batch: Vec<(u32, u8, u32)>, n_old: u32 }

fn varint(o: &mut Vec<u8>, mut v: u32) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u32 { let mut v = 0u32; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u32) << s; if x < 0x80 { return v; } s += 7; } }

/// At rest, (b): per entry varint(key delta) + the mask's W words raw.
fn encode(e: &[(u32, [u64; 4])], w: usize, o: &mut Vec<u8>) {
    o.clear();
    varint(o, e.len() as u32);
    let mut prev = 0u32;
    for (k, m) in e { varint(o, k - prev); prev = *k; for x in &m[..w] { o.extend_from_slice(&x.to_le_bytes()); } }
}
fn decode(b: &[u8], w: usize, e: &mut Vec<(u32, [u64; 4])>) {
    e.clear();
    let mut i = 0;
    let n = get_varint(b, &mut i);
    let mut k = 0u32;
    for _ in 0..n {
        k += get_varint(b, &mut i);
        let mut m = [0u64; 4];
        for x in m.iter_mut().take(w) { *x = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8; }
        e.push((k, m));
    }
}

#[inline] fn below(m: &[u64; 4], p: usize) -> u32 { // popcount of the bits below p
    let (wi, b) = (p / 64, p % 64);
    m[..wi].iter().map(|x| x.count_ones()).sum::<u32>() + (m[wi] & ((1u64 << b) - 1)).count_ones()
}
#[inline] fn pc(m: &[u64; 4]) -> u32 { m.iter().map(|x| x.count_ones()).sum() }

/// The in-core pass, MERGE: sort the batch by (key, pos, qi), merge with the
/// decoded sorted entries. Returns the merged entries and writes ids.
fn run_merge(old: &[(u32, [u64; 4])], batch: &mut Vec<(u32, u8, u32)>, old_base: u32, new_base: u32, want_ids: bool, ids: &mut [u32], newflag: &mut [bool], out: &mut Vec<(u32, [u64; 4])>, scratch: &mut Vec<(u64, u32)>) -> u32 {
    // Sort by (key, pos, batch index): (key << 8 | pos, index).
    scratch.clear();
    scratch.extend(batch.iter().enumerate().map(|(i, b)| ((b.0 as u64) << 8 | b.1 as u64, i as u32)));
    scratch.sort_unstable();
    out.clear();
    let (mut oi, mut orank, mut nrank) = (0usize, 0u32, 0u32);
    let mut j = 0;
    while j < scratch.len() {
        let key = (scratch[j].0 >> 8) as u32;
        while oi < old.len() && old[oi].0 < key { orank += pc(&old[oi].1); out.push(old[oi]); oi += 1; }
        let om = if oi < old.len() && old[oi].0 == key { let m = old[oi].1; oi += 1; m } else { [0; 4] };
        // This entry's lookups: first the new mask, then the ids.
        let mut e = j; let mut nm = [0u64; 4];
        while e < scratch.len() && (scratch[e].0 >> 8) as u32 == key { let p = (scratch[e].0 & 0xff) as usize; if om[p / 64] >> (p % 64) & 1 == 0 { nm[p / 64] |= 1 << (p % 64); } e += 1; }
        let mut last_p = usize::MAX;
        for x in &scratch[j..e] {
            let p = (x.0 & 0xff) as usize; let b = &batch[x.1 as usize];
            let qi = b.2 as usize;
            if om[p / 64] >> (p % 64) & 1 == 1 { if want_ids { ids[qi] = old_base + orank + below(&om, p); } }
            else { if want_ids { ids[qi] = new_base + nrank + below(&nm, p); } newflag[qi] = p != last_p; }
            last_p = p;
        }
        orank += pc(&om); nrank += pc(&nm);
        let mut m = om; for w in 0..4 { m[w] |= nm[w]; }
        out.push((key, m));
        j = e;
    }
    out.extend_from_slice(&old[oi..]);
    nrank
}

/// The in-core pass, HASH: an open-addressing table of the decoded entries
/// (with their old rank base), the batch in sweep order, then the new
/// states ranked (entries sorted by key) and the batch's new ids patched.
#[derive(Clone, Copy)] struct H { key: u32, base: u32, om: [u64; 4], nm: [u64; 4], nbase: u32 }
fn run_hash(old: &[(u32, [u64; 4])], batch: &[(u32, u8, u32)], old_base: u32, new_base: u32, want_ids: bool, ids: &mut [u32], newflag: &mut [bool], out: &mut Vec<(u32, [u64; 4])>, tab: &mut Vec<H>, prov: &mut Vec<u32>) -> u32 {
    let cap = ((old.len() + batch.len().min(4 * old.len() + 64)) * 2).next_power_of_two().max(16);
    tab.clear(); tab.resize(cap, H { key: u32::MAX, base: 0, om: [0; 4], nm: [0; 4], nbase: 0 });
    let mut mask = cap - 1;
    let slot = |tab: &[H], mask: usize, k: u32| -> usize { let mut s = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & mask; loop { if tab[s].key == k || tab[s].key == u32::MAX { return s; } s = (s + 1) & mask; } };
    let mut acc = 0u32;
    for (k, m) in old { let s = slot(tab, mask, *k); tab[s] = H { key: *k, base: acc, om: *m, nm: [0; 4], nbase: 0 }; acc += pc(m); }
    let mut used = old.len();
    prov.clear();
    for b in batch {
        let (k, p) = (b.0, b.1 as usize);
        let mut s = slot(tab, mask, k);
        if tab[s].key == u32::MAX {
            if (used + 1) * 10 > tab.len() * 7 {
                let old_t = std::mem::take(tab); let nc = old_t.len() * 2; mask = nc - 1;
                tab.resize(nc, H { key: u32::MAX, base: 0, om: [0; 4], nm: [0; 4], nbase: 0 });
                for h in old_t.into_iter().filter(|h| h.key != u32::MAX) { let t = slot(tab, mask, h.key); tab[t] = h; }
                s = slot(tab, mask, k);
            }
            tab[s].key = k; used += 1;
        }
        let h = &mut tab[s];
        let bit = 1u64 << (p % 64);
        if h.om[p / 64] & bit != 0 { if want_ids { ids[b.2 as usize] = old_base + h.base + below(&h.om, p); prov.push(u32::MAX); } }
        else { newflag[b.2 as usize] = h.nm[p / 64] & bit == 0; h.nm[p / 64] |= bit; if want_ids { prov.push(s as u32); } }
    }
    // Rank the new states: the entries in key order.
    out.clear();
    let mut order: Vec<u32> = (0..tab.len() as u32).filter(|&s| tab[s as usize].key != u32::MAX).collect();
    order.sort_unstable_by_key(|&s| tab[s as usize].key);
    let mut nacc = 0u32;
    for &s in &order { let h = &mut tab[s as usize]; h.nbase = nacc; nacc += pc(&h.nm); let mut m = h.om; for w in 0..4 { m[w] |= h.nm[w]; } out.push((h.key, m)); }
    if want_ids { for (b, &s) in batch.iter().zip(prov.iter()) { if s != u32::MAX { let h = &tab[s as usize]; ids[b.2 as usize] = new_base + h.nbase + below(&h.nm, b.1 as usize); } } }
    nacc
}

pub fn regionbatch(dir: &str, n_door: usize, r: i32, ws: &str, want_ids: bool) {
    let t0 = Instant::now();
    let w = ((r * r) as usize).div_ceil(64);
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, sm, dsm, shm, scm) = (map("qb.bin"), map("dshard.bin"), map("db.bin"), map("sshape.bin"), map("scell.bin"));
    let qs: &[QB] = crate::from_bytes(&qm); let dsh: &[u32] = crate::from_bytes(&sm); let db: &[(u32, u32)] = crate::from_bytes(&dsm);
    let (sshape, scell): (&[u32], &[u32]) = (crate::from_bytes(&shm), crate::from_bytes(&scm));
    // The digits: (flags, dash, spd.x) out of the packed high digits.
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
        let k = k * low_r[s] + low as u64;
        u32::try_from(k).expect("key past 32 bits")
    };
    // Regions.
    let mut gid: FxHashMap<(u32, i32, i32), u32> = FxHashMap::default();
    let (grp, ppos): (Vec<u32>, Vec<u8>) = (0..sshape.len()).map(|s| {
        let (x, y) = crate::structure::cell_xy(scell[s]);
        let n = gid.len() as u32;
        (*gid.entry((sshape[s], x.div_euclid(r), y.div_euclid(r))).or_insert(n), (x.rem_euclid(r) + r * y.rem_euclid(r)) as u8)
    }).unzip();
    let ng = gid.len();
    let mut regions: Vec<Region> = (0..ng).map(|_| Region { old: Vec::new(), batch: Vec::new(), n_old: 0 }).collect();
    {
        let mut ents: Vec<FxHashMap<u32, [u64; 4]>> = vec![Default::default(); ng];
        for i in 0..n_door { let s = dsh[i]; let p = ppos[s as usize] as usize; let m = ents[grp[s as usize] as usize].entry(keyof(s, db[i].0, db[i].1)).or_insert([0; 4]); m[p / 64] |= 1 << (p % 64); }
        for (g, e) in ents.into_iter().enumerate() { let mut v: Vec<(u32, [u64; 4])> = e.into_iter().collect(); v.sort_unstable_by_key(|x| x.0); regions[g].n_old = v.iter().map(|x| pc(&x.1)).sum(); regions[g].old = v; }
    }
    for (qi, q) in qs.iter().enumerate() { let g = grp[q.shard as usize] as usize; regions[g].batch.push((keyof(q.shard, q.high, q.low), ppos[q.shard as usize], qi as u32)); }
    // At rest: the varint form, and its sizes under the encodings.
    let mut rest: Vec<Vec<u8>> = Vec::with_capacity(ng);
    let (mut sz_b, mut sz_z1, mut sz_z3, mut sz_tab, mut sz_arr, mut sz_arr_z1) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    let mut ent_total = 0u64;
    for g in &regions {
        let mut o = Vec::new(); encode(&g.old, w, &mut o);
        sz_b += o.len() as u64;
        sz_z1 += zstd::bulk::compress(&o, 1).unwrap().len() as u64;
        sz_z3 += zstd::bulk::compress(&o, 3).unwrap().len() as u64;
        sz_tab += ((2 * g.old.len()).next_power_of_two() * (4 + 8 * w)) as u64;
        let arr: Vec<u8> = g.old.iter().flat_map(|(k, m)| { let mut v = k.to_le_bytes().to_vec(); for x in &m[..w] { v.extend_from_slice(&x.to_le_bytes()); } v }).collect();
        sz_arr += arr.len() as u64; sz_arr_z1 += zstd::bulk::compress(&arr, 1).unwrap().len() as u64;
        ent_total += g.old.len() as u64;
        rest.push(o);
    }
    let nd = n_door as f64;
    println!("regionbatch R = {r}x{r}: {ng} regions, {n_door} states at the frame start in {ent_total} entries ({:.1} a entry), {} lookups; setup {:.1} s (untimed)", nd / ent_total as f64, qs.len(), t0.elapsed().as_secs_f64());
    println!("  at rest, B a state: (a) posmask table at load 0.5 {:.2}; (d) sorted (u32, mask) array {:.2}, zstd -1 {:.2}; (b) varint key delta + raw mask {:.2}; (c) (b) + zstd -1 {:.2}, zstd -3 {:.2}",
        sz_tab as f64 / nd, sz_arr as f64 / nd, sz_arr_z1 as f64 / nd, sz_b as f64 / nd, sz_z1 as f64 / nd, sz_z3 as f64 / nd);
    // Samples (by lookups): tiny, median, weighted-median, heaviest, two more.
    let mut by_q: Vec<usize> = (0..ng).filter(|&g| !regions[g].batch.is_empty()).collect();
    by_q.sort_by_key(|&g| regions[g].batch.len());
    let wmed = { let mut a = 0; *by_q.iter().find(|&&g| { a += regions[g].batch.len(); a >= qs.len() / 2 }).unwrap() };
    let samples = [("tiny", by_q[0]), ("p25", by_q[by_q.len() / 4]), ("median", by_q[by_q.len() / 2]), ("p75", by_q[3 * by_q.len() / 4]), ("weighted-median", wmed), ("heaviest", *by_q.last().unwrap())];
    // Run every region, per format x working structure.
    let old_base: Vec<u32> = regions.iter().scan(0u32, |a, g| { let b = *a; *a += g.n_old; Some(b) }).collect();
    let fmt_list: Vec<&str> = vec!["raw", "varint", "varint+zstd1"];
    for fmt in &fmt_list {
        // Per format, the at-rest bytes.
        let store: Vec<Vec<u8>> = rest.iter().map(|o| match *fmt { "varint+zstd1" => zstd::bulk::compress(o, 1).unwrap(), _ => o.clone() }).collect();
        let raw_store: Vec<Vec<(u32, [u64; 4])>> = if *fmt == "raw" { regions.iter().map(|g| g.old.clone()).collect() } else { Vec::new() };
        for w_s in ws.split(',') {
            let mut ids = vec![u32::MAX; qs.len()];
            let mut newflag = vec![false; qs.len()];
            let (mut td, mut tp, mut te) = (0f64, 0f64, 0f64);
            let mut per: Vec<(f64, f64, f64)> = vec![(0.0, 0.0, 0.0); ng];
            let (mut dec, mut out, mut scratch, mut tab, mut prov, mut enc) = (Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new());
            let mut new_base = n_door as u32;
            let mut stored = 0u64;
            crate::perf_on(true);
            let t_all = Instant::now();
            for g in 0..ng {
                let mut batch = std::mem::take(&mut regions[g].batch);
                let t = Instant::now();
                let old: &[(u32, [u64; 4])] = match *fmt {
                    "raw" => &raw_store[g],
                    "varint" => { decode(&store[g], w, &mut dec); &dec }
                    _ => { let b = zstd::bulk::decompress(&store[g], 1 << 26).unwrap(); decode(&b, w, &mut dec); &dec }
                };
                let t1 = Instant::now();
                let nn = if w_s == "merge" { run_merge(old, &mut batch, old_base[g], new_base, want_ids, &mut ids, &mut newflag, &mut out, &mut scratch) }
                         else { run_hash(old, &batch, old_base[g], new_base, want_ids, &mut ids, &mut newflag, &mut out, &mut tab, &mut prov) };
                let t2 = Instant::now();
                match *fmt { "raw" => { enc.clear(); enc.extend(out.iter().map(|x| x.0 as u8)); std::hint::black_box(&out); }
                             "varint" => encode(&out, w, &mut enc),
                             _ => { encode(&out, w, &mut enc); enc = zstd::bulk::compress(&enc, 1).unwrap(); } }
                let t3 = Instant::now();
                stored += if *fmt == "raw" { (out.len() * (4 + 8 * w)) as u64 } else { enc.len() as u64 };
                new_base += nn;
                let p = ((t1 - t).as_secs_f64(), (t2 - t1).as_secs_f64(), (t3 - t2).as_secs_f64());
                per[g] = p; td += p.0; tp += p.1; te += p.2;
                regions[g].batch = batch;
            }
            let tall = t_all.elapsed().as_secs_f64();
            crate::perf_on(false);
            let new = newflag.iter().filter(|&&x| x).count();
            println!("  [{fmt} + {w_s}] all {ng} regions, ONE core (noisy): decode {td:.2} s, lookups+inserts {tp:.2} s, re-encode {te:.2} s, total {tall:.2} s = {:.1} ns a lookup; new {new} ({}); stored after {:.2} B a state",
                tall * 1e9 / qs.len() as f64, if new == 6_735_699 { "OK" } else { "MISMATCH" }, stored as f64 / (n_door + new) as f64);
            if *fmt == "varint" {
                for (label, g) in samples {
                    let (d, p, e) = per[g]; let (nq, ns) = (regions[g].batch.len(), regions[g].n_old);
                    println!("    {label:<16} region {g}: {ns} states in {} entries, {nq} lookups: decode {:.1} us, run {:.1} us, encode {:.1} us = {:.1} ns a lookup, {:.1} ns a stored state",
                        regions[g].old.len(), d * 1e6, p * 1e6, e * 1e6, (d + p + e) * 1e9 / nq as f64, (d + p + e) * 1e9 / ns.max(1) as f64);
                }
            }
            // Decisions against v3c's (first sight of a new reference id), in query order.
            {
                let rm = map("refid.bin"); let refid: &[u32] = crate::from_bytes(&rm);
                let mut seen = vec![false; n_door + 6_735_699];
                let (mut bad, mut fp) = (0u64, 0u64);
                for (qi, &rf) in refid.iter().enumerate() {
                    let ref_new = rf as usize >= n_door && !seen[rf as usize]; seen[rf as usize] = true;
                    if ref_new != newflag[qi] { bad += 1; }
                    fp = fp.wrapping_mul(0x100_0000_01b3) ^ (newflag[qi] as u64);
                }
                println!("    CHECK decisions: {bad} mismatches against v3c; fingerprint {fp:016x}");
            }
            if want_ids && fmt == &"varint" && w_s == ws.split(',').next().unwrap() {
                let edges: Vec<(u32, u32, u32)> = qs.iter().zip(&ids).map(|(q, &id)| (q.src, id, q.xfer)).collect();
                check(&format!("regionbatch{r}"), &pd, &edges, n_door, &|m| (m as usize) < n_door, true);
            }
        }
    }
}
