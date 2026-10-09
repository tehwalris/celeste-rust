//! `posregion`: the nesting inverted - POSITION LAST, as a bitmask over a
//! region's cells (regions aligned like the kernels': `x.div_euclid(R)`).
//! Tree: (shape, region) -> flags+dash -> spd.x -> spd.y -> position mask.

use crate::structure::{cell_xy, lg_choose, load, NAMES, NF};
use rustc_hash::{FxHashMap, FxHashSet};

const M: usize = NF + 1; // field 13 = the position within the region
const POS: usize = NF;
type R14 = (u32, [u16; M]);

fn sort_by(rows: &mut [R14], order: &[usize]) {
    rows.sort_unstable_by(|a, b| a.0.cmp(&b.0).then_with(|| order.iter().map(|&k| a.1[k].cmp(&b.1[k])).find(|o| o.is_ne()).unwrap_or(std::cmp::Ordering::Equal)));
}

/// Per level of `order` (rows sorted by it): nodes; the subset bound and the
/// succinct (min LOUDS / bitmap) bits a state.
fn trie(rows: &[R14], order: &[usize], alpha: &[usize]) -> (Vec<u64>, f64, f64) {
    let n = rows.len();
    let lvl: Vec<u8> = (0..n).map(|i| if i == 0 || rows[i].0 != rows[i - 1].0 { 0 } else { order.iter().position(|&k| rows[i].1[k] != rows[i - 1].1[k]).map_or(99, |p| p + 1) as u8 }).collect();
    assert!(lvl.iter().all(|&l| l != 99), "duplicate state");
    let mut prev = lvl.iter().filter(|&&l| l == 0).count() as u64;
    let (mut nodes_v, mut bound, mut succ) = (Vec::new(), 0f64, 0f64);
    for (d, &k) in order.iter().enumerate() {
        let lv = (d + 1) as u8;
        let (mut nodes, mut b) = (0u64, 0f64);
        let mut i = 0;
        while i < n {
            let mut j = i + 1; while j < n && lvl[j] > d as u8 { j += 1; }
            let c = 1 + (i + 1..j).filter(|&e| lvl[e] <= lv).count() as u64;
            nodes += c; b += lg_choose(alpha[k] as u64, c);
            i = j;
        }
        let louds = nodes as f64 * (2.0 + (alpha[k] as f64).log2().ceil());
        succ += louds.min(prev as f64 * alpha[k] as f64);
        bound += b; nodes_v.push(nodes); prev = nodes;
    }
    (nodes_v, bound / n as f64, succ / n as f64)
}

/// First-changed-field encoding of one region's rows (sorted by `order`).
fn first_changed(rows: &[R14], order: &[usize]) -> Vec<u8> {
    let mut o = Vec::new();
    let put = |o: &mut Vec<u8>, k: usize, v: u16| if k == 11 { o.extend_from_slice(&v.to_le_bytes()) } else { o.push(v as u8) };
    for (j, r) in rows.iter().enumerate() {
        if j == 0 { o.push(255); for &k in order { put(&mut o, k, r.1[k]); } continue; }
        let p = &rows[j - 1].1;
        let f = order.iter().position(|&k| r.1[k] != p[k]).unwrap();
        o.push(f as u8); put(&mut o, order[f], r.1[order[f]].wrapping_sub(p[order[f]]));
        for &k in &order[f + 1..] { put(&mut o, k, r.1[k]); }
    }
    o
}

/// zstd -19 of each region alone, summed (8 threads).
fn zstd_per_region(rows: &[R14], order: &[usize]) -> (u64, u64) {
    let mut bounds = Vec::new();
    let mut i = 0; while i < rows.len() { let mut j = i; while j < rows.len() && rows[j].0 == rows[i].0 { j += 1; } bounds.push((i, j)); i = j; }
    let next = std::sync::atomic::AtomicUsize::new(0);
    let tot = std::sync::atomic::AtomicU64::new(0);
    let raw = std::sync::atomic::AtomicU64::new(0);
    std::thread::scope(|s| for _ in 0..8 { s.spawn(|| loop {
        let b = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if b >= bounds.len() { break; }
        let (i, j) = bounds[b];
        let e = first_changed(&rows[i..j], order);
        raw.fetch_add(e.len() as u64, std::sync::atomic::Ordering::Relaxed);
        tot.fetch_add(zstd::bulk::compress(&e, 19).unwrap().len() as u64, std::sync::atomic::Ordering::Relaxed);
    }); });
    (raw.into_inner(), tot.into_inner())
}

fn gamma(v: u64) -> u64 { debug_assert!(v >= 1); 2 * (63 - v.leading_zeros() as u64) + 1 }

/// Explicit dictionary-index coding, no general compressor. Rows sorted by
/// (group, order). Every field is its index in a sorted dictionary: the
/// room's (`local` false) or the group's (`local` true; a one-value field
/// then costs nothing). A row: the first changed level as gamma(levels -
/// level) (the last level changes most), gamma of that field's index delta,
/// then each following field as gamma(index + 1). A group's first row: every
/// field as gamma(index + 1). Returns bits a state (dictionaries not counted;
/// per group they are <= a few hundred values).
fn explicit_bits(rows: &[R14], order: &[usize], local: bool) -> f64 {
    let l = order.len() as u64;
    let mut bits = 0u64;
    let mut i = 0;
    while i < rows.len() {
        let mut j = i; while j < rows.len() && rows[j].0 == rows[i].0 { j += 1; }
        let g = &rows[i..j];
        let dict: Vec<Vec<u16>> = order.iter().map(|&k| { let mut v: Vec<u16> = g.iter().map(|r| r.1[k]).collect(); v.sort_unstable(); v.dedup(); v }).collect();
        let ix = |d: usize, v: u16| -> u64 { if local { dict[d].binary_search(&v).unwrap() as u64 } else { v as u64 } };
        let one = |d: usize| local && dict[d].len() == 1;
        for (t, r) in g.iter().enumerate() {
            let f = if t == 0 { 0 } else { order.iter().position(|&k| r.1[k] != g[t - 1].1[k]).unwrap() };
            if t > 0 {
                bits += gamma(l - f as u64);
                bits += gamma(ix(f, r.1[order[f]]) - ix(f, g[t - 1].1[order[f]]));
            }
            for d in if t == 0 { 0 } else { f + 1 }..order.len() { if !one(d) { bits += gamma(ix(d, r.1[order[d]]) + 1); } }
        }
        i = j;
    }
    bits as f64 / rows.len() as f64
}

pub fn posregion(dir: &str) {
    let set = load(dir);
    let n = set.rows.len() as f64;
    // The sweep's queries (prep/qb.bin order): shard, high, low -> the state.
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, shm, scm) = (map("qb.bin"), map("sshape.bin"), map("scell.bin"));
    let qs: &[crate::bits::QB] = crate::from_bytes(&qm);
    let (sshape, scell): (&[u32], &[u32]) = (crate::from_bytes(&shm), crate::from_bytes(&scm));
    println!("posregion: {} states (end of f57, shapes 2, 3, 7); {} lookups for the live window", set.rows.len(), qs.len());
    let fd: Vec<usize> = (0..11).collect();
    let ord_a: Vec<usize> = fd.iter().copied().chain([11, 12, POS]).collect();
    let ord_c: Vec<usize> = fd.iter().copied().chain([POS, 11, 12]).collect();
    for r in [1i32, 4, 8, 16] {
        let rr = (r * r) as usize;
        let mut reg: FxHashMap<(usize, i32, i32), u32> = FxHashMap::default();
        let mut rows: Vec<R14> = set.rows.iter().map(|row| {
            let (sh, c) = set.shards[row.0 as usize];
            let (x, y) = cell_xy(c);
            let n_ = reg.len() as u32;
            let id = *reg.entry((sh, x.div_euclid(r), y.div_euclid(r))).or_insert(n_);
            let mut v = [0u16; M];
            v[..NF].copy_from_slice(&row.1);
            v[POS] = (x.rem_euclid(r) + r * y.rem_euclid(r)) as u16;
            (id, v)
        }).collect();
        let mut alpha: Vec<usize> = set.alpha.to_vec(); alpha.push(rr);
        sort_by(&mut rows, &ord_a);
        let (na, ba, sa) = trie(&rows, &ord_a, &alpha);
        let leaves = na[12]; // distinct (region, fd, spd.x, spd.y)
        let leaves_b = na[11]; // distinct (region, fd, spd.x)
        // Masks at the leaves: distinct, for interning.
        let words = rr.div_ceil(64);
        let mut masks: FxHashSet<[u64; 4]> = Default::default();
        let mut i = 0;
        while i < rows.len() {
            let mut j = i; let mut m = [0u64; 4];
            while j < rows.len() && rows[j].0 == rows[i].0 && rows[j].1[..POS] == rows[i].1[..POS] { let p = rows[j].1[POS] as usize; m[p / 64] |= 1 << (p % 64); j += 1; }
            masks.insert(m); i = j;
        }
        // B: mask over (spd.y, pos) per (region, fd, spd.x): room spd.y (96) and per-region local spd.y.
        let mut ny: FxHashMap<u32, FxHashSet<u16>> = FxHashMap::default();
        for row in &rows { ny.entry(row.0).or_default().insert(row.1[12]); }
        let mut bits_b_local = 0f64; let mut prev_key = None;
        for row in &rows { let k = (row.0, &row.1[..12]); if prev_key != Some(k) { bits_b_local += (ny[&row.0].len() * rr) as f64; prev_key = Some(k); } }
        let mask_b = (leaves as f64 * 0.0, leaves_b as f64 * 96.0 * rr as f64);
        let mb = (rr as f64 / 8.0).max(1.0);
        println!("R = {r}x{r}: {} regions; A (pos last): {leaves} leaves, {:.2} states/leaf, mask occupancy {:.2}% ({rr} bits); plain key u32 + mask = {:.2} B/state ({:.2} at load 0.5); {} distinct masks, interned (u32 key + u32 id) {:.2} B/state + {:.1} MB of masks",
            reg.len(), n / leaves as f64, 100.0 * n / (leaves as f64 * rr as f64), leaves as f64 * (4.0 + mb) / n, 2.0 * leaves as f64 * (4.0 + mb) / n, masks.len(), 8.0 * leaves as f64 / n, masks.len() as f64 * (words * 8) as f64 / 1e6);
        let _ = mask_b.0;
        println!("        B (mask over (spd.y, pos) under spd.x): {leaves_b} leaves, {:.2} states/leaf; room spd.y: occupancy {:.3}%, {:.1} B/state; per-region spd.y dictionary: occupancy {:.2}%, {:.2} B/state (+4 B key a leaf)",
            n / leaves_b as f64, 100.0 * n / mask_b.1, mask_b.1 / 8.0 / n, 100.0 * n / bits_b_local, (bits_b_local / 8.0 + 4.0 * leaves_b as f64) / n);
        println!("        explicit dictionary-index coding (gamma), order A: room dictionaries {:.2} bits/state, per-group dictionaries {:.2} bits/state",
            explicit_bits(&rows, &ord_a, false), explicit_bits(&rows, &ord_a, true));
        let (za_raw, za) = zstd_per_region(&rows, &ord_a);
        println!("        A trie: nodes per level {:?}; subset bound {ba:.2} bits, succinct {sa:.2} bits; per-region first-changed {:.2} B raw, zstd -19 {:.3} B/state", na, za_raw as f64 / n, za as f64 / n);
        let mut rc = rows.clone();
        sort_by(&mut rc, &ord_c);
        let (nc, bc, sc) = trie(&rc, &ord_c, &alpha);
        println!("        explicit dictionary-index coding (gamma), order C: room dictionaries {:.2} bits/state, per-group dictionaries {:.2} bits/state",
            explicit_bits(&rc, &ord_c, false), explicit_bits(&rc, &ord_c, true));
        let (zc_raw, zc) = zstd_per_region(&rc, &ord_c);
        println!("        C (pos after flags+dash): nodes {:?}; subset bound {bc:.2} bits, succinct {sc:.2} bits; per-region first-changed {:.2} B raw, zstd -19 {:.3} B/state", nc, zc_raw as f64 / n, zc as f64 / n);
        drop(rc); drop(rows);
        // Live window of the sweep over A's leaves (and bitcell-like (region, high) entries).
        for (label, with_low) in [("A leaves (region, high, spd.y)", true), ("(region, high) entries", false)] {
            let mut first: FxHashMap<(u32, u32, u32), (u32, u32)> = FxHashMap::default();
            for (qi, q) in qs.iter().enumerate() {
                let (x, y) = cell_xy(scell[q.shard as usize]);
                let rg = *reg.get(&(sshape[q.shard as usize] as usize, x.div_euclid(r), y.div_euclid(r))).unwrap_or(&u32::MAX);
                let e = first.entry((rg, q.high, if with_low { q.low } else { 0 })).or_insert((qi as u32, qi as u32));
                e.1 = qi as u32;
            }
            let mut diff = vec![0i64; qs.len() + 1];
            for &(a, b) in first.values() { diff[a as usize] += 1; diff[b as usize + 1] -= 1; }
            let (mut cur, mut peak, mut sum) = (0i64, 0i64, 0f64);
            for d in &diff[..qs.len()] { cur += d; peak = peak.max(cur); sum += cur as f64; }
            let eb = 4.0 + if with_low { mb } else { 12.0 };
            println!("        sweep live window, {label}: {} touched, peak {peak} ({:.1} MB at {eb} B), mean {:.0}", first.len(), peak as f64 * eb / 1e6, sum / qs.len() as f64);
        }
    }
    let _ = NAMES;
}
