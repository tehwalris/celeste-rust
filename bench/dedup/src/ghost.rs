//! `ghost`: the SOURCE-side ghost layout built in the parallel region pass
//! (DESIGNS N2). Units = SOURCE 8x8 regions (heavy ones split by a hash of
//! the TARGET key: parts own disjoint target keys). Each unit holds R's
//! unified set: an entry per key with its own core mask (64 cells) and a
//! window mask (16x16: the core plus a 4-px halo). Per emission ONE lookup in
//! that set gives the target's LOCAL index (entry lid, window cell): an own
//! target is deduplicated right there (own-new counted once, by its owner);
//! a halo target becomes a ghost bit; a target beyond the halo or of another
//! shape an overflow ghost. The edge is written flat, in emission order:
//! varint(source local, 0 = same as before), varint(lid << 9 | overflow << 8 |
//! window cell), varint(global transfer id). No grouping pass.
//!
//! Then the TRANSLATION pass, parallel per OWNER region: every ghost (R, key,
//! cell) is looked up in its owner's set after the owners' own passes; one
//! not there is NEW, inserted once (the requests of an owner are processed in
//! order, so several regions reaching it count it once).
//!
//! Stable ids: (region, entry number, cell). Old entries numbered by key; a
//! unit's own new entries `NEW | part << 20 | rank` (first appearance); the
//! translation's new entries `NEW | 1023 << 20 | rank` (requests sorted).
//!
//! Args: runs "T:split" (split = max emissions a unit, 0 = none).

use crate::bits::{packing, Fields};
use crate::structure::cell_xy;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

const NEW: u32 = 1 << 31;
const HALO: i32 = 4;
const W: i32 = 8 + 2 * HALO; // window side

#[inline] fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u64 { let mut v = 0u64; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u64) << s; if x < 0x80 { return v; } s += 7; } }
fn hpart(k: u32, parts: u32) -> u32 { (k.wrapping_mul(0x85EB_CA6B).rotate_left(13)) % parts }
fn mixf(t: &[u64; 7]) -> u64 { t.iter().fold(0x1234_5678u64, |a, &x| crate::bits::mix64(a ^ x).wrapping_add(x)) }

/// One emission of a source unit: target key; source local (old entry << 6 | cell);
/// transfer (bit 31: overflow target); window cell, or for an overflow target
/// owner region << 6 | its cell there.
#[derive(Clone, Copy)] #[repr(C)] struct Em { key: u32, src: u32, xfer: u32, w: u32 }

struct Unit { region: u32, part: u32, rest: Vec<u8>, batch: Vec<Em> }

#[derive(Clone, Copy)] struct E { key: u32, num: u32, lid: u32, core: u64, win: [u64; 4] }

/// A ghost request: owner region, key, cell in the owner, and where it came from (unit, lid, window cell).
#[derive(Clone, Copy, Default)] struct Req { owner: u32, key: u32, opos: u8 }

#[derive(Default)] struct Out { own: Vec<(u32, u32, u64)>, lid_key: Vec<u32>, over: Vec<(u32, u32)>, reqs: Vec<Req>, new: u64, thread: usize, off: usize, len: usize }

struct Scratch { tab: Vec<E>, arena: Vec<u8>, used: usize, enc: Vec<u8>, lid_slot: Vec<u32>, over: FxHashMap<(u32, u32), u32> }

#[inline(always)]
fn slot(tab: &[E], mask: usize, k: u32) -> usize { let mut i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & mask; loop { let x = tab[i].key; if x == k || x == u32::MAX { return i; } i = (i + 1) & mask; } }

fn run_unit(ui: usize, u: &Unit, geo: &[(i32, i32)], owner_of: &dyn Fn(u32, i32, i32) -> Option<(u32, u8)>, s: &mut Scratch, ti: usize, write: bool) -> Out {
    // Decode R's (part's) old own entries.
    let b = &u.rest; let mut i = 0;
    let n = get_varint(b, &mut i) as usize;
    let cap = ((n + u.batch.len().min(4 * n + 64)) * 2).next_power_of_two().max(16);
    s.tab.clear(); s.tab.resize(cap, E { key: u32::MAX, num: u32::MAX, lid: u32::MAX, core: 0, win: [0; 4] });
    let mut mask = cap - 1;
    let (mut k, mut num) = (0u32, 0u32);
    for _ in 0..n {
        k += get_varint(b, &mut i) as u32; num += get_varint(b, &mut i) as u32;
        let m = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8;
        let t = slot(&s.tab, mask, k); s.tab[t] = E { key: k, num, lid: u32::MAX, core: m, win: [0; 4] };
    }
    let mut used = n;
    s.lid_slot.clear(); s.over.clear();
    let mut over_keys: Vec<(u32, u32)> = Vec::new();
    let (mut new, mut own_new_entries) = (0u64, 0u32);
    let o = &mut s.enc; o.clear();
    varint(o, ui as u64); varint(o, u.batch.len() as u64);
    let mut prev_src = u32::MAX;
    for e in &u.batch {
        // source: 0 = same as the previous edge's
        if write { if e.src == prev_src { o.push(0); } else { varint(o, e.src as u64 + 1); prev_src = e.src; } }
        let tl: u64 = if e.xfer >> 31 == 1 {
            let nl = over_keys.len() as u32;
            let lid = *s.over.entry((e.w, e.key)).or_insert_with(|| { over_keys.push((e.w, e.key)); nl });
            (lid as u64) << 9 | 1 << 8
        } else {
            let mut t = slot(&s.tab, mask, e.key);
            if s.tab[t].key == u32::MAX {
                if (used + 1) * 10 > s.tab.len() * 7 {
                    let old: Vec<E> = s.tab.drain(..).filter(|h| h.key != u32::MAX).collect();
                    let nc = (mask + 1) * 2; mask = nc - 1;
                    s.tab.resize(nc, E { key: u32::MAX, num: u32::MAX, lid: u32::MAX, core: 0, win: [0; 4] });
                    for h in old { let q = slot(&s.tab, mask, h.key); s.tab[q] = h; }
                    // re-map lid -> slot
                    for (q, h) in s.tab.iter().enumerate() { if h.lid != u32::MAX && h.key != u32::MAX { s.lid_slot[h.lid as usize] = q as u32; } }
                    t = slot(&s.tab, mask, e.key);
                }
                s.tab[t].key = e.key; used += 1;
            }
            let h = &mut s.tab[t];
            if h.lid == u32::MAX { h.lid = s.lid_slot.len() as u32; s.lid_slot.push(t as u32); }
            let (wx, wy) = (e.w as i32 % W, e.w as i32 / W);
            if (HALO..HALO + 8).contains(&wx) && (HALO..HALO + 8).contains(&wy) {
                let cb = 1u64 << ((wx - HALO) + 8 * (wy - HALO));
                if h.core & cb == 0 { h.core |= cb; new += 1; if h.num == u32::MAX { h.num = NEW | u.part << 20 | own_new_entries; own_new_entries += 1; } }
            } else { h.win[e.w as usize / 64] |= 1 << (e.w % 64); }
            (h.lid as u64) << 9 | e.w as u64
        };
        if write { varint(o, tl); varint(o, (e.xfer & !(1 << 31)) as u64); }
    }
    // The unit's lid -> key table (the edges' local targets), its ghost requests, its own entries.
    let mut lid_key = vec![0u32; s.lid_slot.len()];
    let mut reqs = Vec::new();
    let (cx, cy) = geo[u.region as usize];
    for &t in &s.lid_slot { let h = &s.tab[t as usize]; lid_key[h.lid as usize] = h.key;
        for wq in 0..4 { let mut m = h.win[wq]; while m != 0 { let b = m.trailing_zeros(); m &= m - 1; let wp = wq as u32 * 64 + b;
            let (gx, gy) = (cx - HALO + (wp as i32 % W), cy - HALO + (wp as i32 / W));
            let (ow, opos) = owner_of(u.region, gx, gy).expect("a halo cell without an owner region");
            reqs.push(Req { owner: ow, key: h.key, opos }); } } }
    for &(w, key) in over_keys.iter() { reqs.push(Req { owner: w >> 6, key, opos: (w & 63) as u8 }); }
    let own: Vec<(u32, u32, u64)> = s.tab.iter().filter(|h| h.key != u32::MAX && h.core != 0).map(|h| (h.key, h.num, h.core)).collect();
    // edges into the arena
    let len = o.len();
    assert!(s.used + len <= s.arena.len(), "edge arena too small");
    s.arena[s.used..s.used + len].copy_from_slice(o);
    let off = s.used; s.used += len;
    Out { own, lid_key, over: over_keys, reqs, new, thread: ti, off, len }
}

fn pin(cpu: usize) { unsafe { let mut set: libc::cpu_set_t = std::mem::zeroed(); libc::CPU_SET(cpu, &mut set); libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set); } }

pub fn ghost(fields_dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap], args: &[String]) {
    let t0 = Instant::now();
    let f = Fields::open(fields_dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<_> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    let roles: Vec<Vec<(u64, u8)>> = packs.iter().map(|p| p.high.iter().map(|&d| (p.digits[d].radix, if p.digits[d].name == "spd.x" { 1 } else if p.digits[d].table.is_some() { 2 } else { 0 })).collect()).collect();
    let low_r: Vec<u64> = packs.iter().map(|p| p.low.map_or(1, |d| p.digits[d].radix)).collect();
    let n = f.n;
    let (mut rshape, mut rcell, mut rkey, mut rreg, mut rpos, mut rold) = (vec![0u8; n], vec![0u32; n], vec![0u32; n], vec![0u32; n], vec![0u8; n], vec![false; n]);
    let mut rid: FxHashMap<(u8, i32, i32), u32> = FxHashMap::default();
    let mut geo: Vec<(i32, i32)> = Vec::new();
    let mut gshape: Vec<u8> = Vec::new();
    let mut row_of: FxHashMap<u128, u32> = FxHashMap::default(); row_of.reserve(n);
    for i in 0..n {
        let (s, c, fr) = f.hdr(i);
        let (h, l) = packs[s].pack(&f, i);
        let mut hh = h as u64; let mut d = Vec::with_capacity(8);
        for &(rad, role) in roles[s].iter().rev() { d.push((hh % rad, rad, role)); hh /= rad; }
        d.reverse();
        let mut k = 0u64;
        for want in [0u8, 2, 1] { for &(v, rad, role) in &d { if role == want { k = k * rad + v; } } }
        let (x, y) = cell_xy(c);
        let nr = rid.len() as u32;
        let r = *rid.entry((s as u8, x.div_euclid(8), y.div_euclid(8))).or_insert_with(|| { geo.push((x.div_euclid(8) * 8, y.div_euclid(8) * 8)); gshape.push(s as u8); nr });
        rshape[i] = s as u8; rcell[i] = c; rkey[i] = u32::try_from(k * low_r[s] + l as u64).unwrap(); rreg[i] = r;
        rpos[i] = (x.rem_euclid(8) + 8 * y.rem_euclid(8)) as u8; rold[i] = fr <= 56;
        row_of.insert(f.key(i), i as u32);
    }
    let ng = rid.len();
    assert!(ng < 1 << 10);
    let mut old_ents: Vec<Vec<(u32, u64)>> = vec![Vec::new(); ng];
    {
        let mut m: Vec<FxHashMap<u32, u64>> = vec![Default::default(); ng];
        for i in 0..n { if rold[i] { *m[rreg[i] as usize].entry(rkey[i]).or_insert(0) |= 1 << rpos[i]; } }
        for (r, e) in m.into_iter().enumerate() { let mut v: Vec<(u32, u64)> = e.into_iter().collect(); v.sort_unstable(); old_ents[r] = v; }
    }
    let old_num = |r: u32, k: u32| old_ents[r as usize].binary_search_by_key(&k, |e| e.0).ok().map(|x| x as u32);
    // The owner region of a global cell of a given region's shape: (region, cell in it). A region absent from the
    // visited set gets an id on demand here (setup); none is needed for this capture (asserted).
    let (rid_ref, gshape_ref) = (&rid, &gshape);
    let owner_of = move |r: u32, gx: i32, gy: i32| -> Option<(u32, u8)> {
        let sh = gshape_ref[r as usize];
        rid_ref.get(&(sh, gx.div_euclid(8), gy.div_euclid(8))).map(|&o| (o, (gx.rem_euclid(8) + 8 * gy.rem_euclid(8)) as u8))
    };
    let mut src_row: FxHashMap<u64, u32> = FxHashMap::default();
    for i in 0..door.len() / 40 {
        let b = &door[i * 40..];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        src_row.insert(u64::from_le_bytes(b[32..40].try_into().unwrap()), row_of[&k]);
    }
    let mut pairs: Vec<[u8; 26]> = Vec::new();
    let mut intern: FxHashMap<[u8; 26], u32> = FxHashMap::default();
    // Emissions per SOURCE region, in capture order.
    let mut batches: Vec<Vec<Em>> = vec![Vec::new(); ng];
    let (mut fp_cap, mut n_edges, mut n_over) = (0u64, 0u64, 0u64);
    for (wn, m) in maps.iter().enumerate() {
        let b = std::fs::read(format!("{cap}/x{wn:03}.bin")).expect("per-worker transfer table");
        let local: Vec<u32> = b.chunks_exact(26).map(|c| { let a: [u8; 26] = c.try_into().unwrap(); let k = pairs.len() as u32; *intern.entry(a).or_insert_with(|| { pairs.push(a); k }) }).collect();
        for c in m.chunks_exact(48) {
            let r = crate::rec(c);
            if r.flags != 0 { continue; }
            let s = src_row[&r.src]; let t = row_of[&r.key];
            let x = local[r.xfer as usize];
            let sr = rreg[s as usize];
            let se = old_num(sr, rkey[s as usize]).expect("a source not in the frame-start set");
            let (cx, cy) = geo[sr as usize]; let (tx, ty) = cell_xy(rcell[t as usize]);
            let (wx, wy) = (tx - (cx - HALO), ty - (cy - HALO));
            let inwin = rshape[t as usize] == rshape[s as usize] && (0..W).contains(&wx) && (0..W).contains(&wy);
            let em = if inwin { Em { key: rkey[t as usize], src: se << 6 | rpos[s as usize] as u32, xfer: x, w: (wx + W * wy) as u32 } }
                     else { n_over += 1; Em { key: rkey[t as usize], src: se << 6 | rpos[s as usize] as u32, xfer: x | 1 << 31, w: rreg[t as usize] << 6 | rpos[t as usize] as u32 } };
            batches[sr as usize].push(em);
            fp_cap = fp_cap.wrapping_add(mixf(&[rshape[s as usize] as u64, rcell[s as usize] as u64, rkey[s as usize] as u64, rshape[t as usize] as u64, rcell[t as usize] as u64, rkey[t as usize] as u64, x as u64]));
            n_edges += 1;
        }
    }
    drop(src_row);
    println!("ghost: {n} states, {ng} regions, {n_edges} edges ({n_over} to overflow targets), {} transfers; capture edge fingerprint {fp_cap:016x}; setup (untimed) {:.1} s", pairs.len(), t0.elapsed().as_secs_f64());
    let write = std::env::var_os("GHOST_NOEDGES").is_none();
    let mut built: Option<(usize, Vec<Unit>)> = None;
    for run in args {
        let v: Vec<&str> = run.split(':').collect();
        let (threads, split): (usize, usize) = (v[0].parse().unwrap(), v[1].parse().unwrap());
        let tu = Instant::now();
        if !built.as_ref().is_some_and(|b| b.0 == split) {
            let mut units = Vec::new();
            for r in 0..ng {
                let parts = if split == 0 { 1 } else { batches[r].len().div_ceil(split).max(1) as u32 };
                for p in 0..parts {
                    let sel: Vec<(u32, (u32, u64))> = old_ents[r].iter().enumerate().filter(|(_, e)| parts == 1 || hpart(e.0, parts) == p).map(|(i, e)| (i as u32, *e)).collect();
                    let mut rest = Vec::new(); varint(&mut rest, sel.len() as u64);
                    let (mut pk, mut pn) = (0u32, 0u32);
                    for (num, (k, m)) in sel { varint(&mut rest, (k - pk) as u64); varint(&mut rest, (num - pn) as u64); rest.extend_from_slice(&m.to_le_bytes()); pk = k; pn = num; }
                    let batch: Vec<Em> = batches[r].iter().filter(|e| parts == 1 || hpart(e.key, parts) == p).copied().collect();
                    units.push(Unit { region: r as u32, part: p, rest, batch });
                }
            }
            units.sort_by_key(|u| std::cmp::Reverse(u.batch.len()));
            built = Some((split, units));
        }
        let units = &built.as_ref().unwrap().1;
        let t_units = tu.elapsed().as_secs_f64();
        let max_b = units.iter().map(|u| u.batch.len()).max().unwrap();
        let max_cap = units.iter().map(|u| { let mut i = 0; let n = get_varint(&u.rest, &mut i) as usize; ((n + u.batch.len().min(4 * n + 64)) * 2).next_power_of_two() }).max().unwrap();
        let arena = (n_edges as usize * 12 / threads).max(256 << 20) + 16 * max_b;
        let tp = Instant::now();
        let mut scr: Vec<Scratch> = (0..threads).map(|_| {
            let mut s = Scratch { tab: Vec::with_capacity(max_cap), arena: vec![0u8; arena], used: 0, enc: Vec::with_capacity(16 * max_b + 1024), lid_slot: Vec::with_capacity(max_b), over: FxHashMap::default() };
            s.tab.resize(max_cap, E { key: 0, num: 0, lid: 0, core: 0, win: [0; 4] }); s.enc.resize(16 * max_b + 1024, 0); s.lid_slot.resize(max_b, 0);
            for b in s.arena.iter_mut().step_by(4096) { *b = 1; }
            s
        }).collect();
        let t_pre = tp.elapsed().as_secs_f64();
        let cpus: Vec<usize> = if threads == 1 { vec![4] } else { (0..threads).collect() };
        // OWN PASS: per source unit.
        let next = AtomicUsize::new(0);
        let outs: Vec<std::sync::Mutex<Out>> = units.iter().map(|_| std::sync::Mutex::new(Out::default())).collect();
        crate::perf_on(true);
        let tw = Instant::now();
        std::thread::scope(|sc| {
            for (ti, s) in scr.iter_mut().enumerate() {
                let (units, next, outs, geo, cpu, owner_of, write) = (units, &next, &outs, &geo, cpus[ti], &owner_of, write);
                sc.spawn(move || {
                    pin(cpu);
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= units.len() { break; }
                        *outs[i].lock().unwrap() = run_unit(i, &units[i], geo, owner_of, s, ti, write);
                    }
                });
            }
        });
        let t_own = tw.elapsed().as_secs_f64();
        let outs: Vec<Out> = outs.into_iter().map(|m| m.into_inner().unwrap()).collect();
        // TRANSLATION PASS: requests by owner; per owner (parallel), its own entries after the own passes, then the requests in order.
        let tt = Instant::now();
        let mut by_owner: Vec<Vec<(usize, usize)>> = vec![Vec::new(); ng]; // (unit, req index)
        for (ui, o) in outs.iter().enumerate() { for (k, r) in o.reqs.iter().enumerate() { by_owner[r.owner as usize].push((ui, k)); } }
        let mut own_parts: Vec<Vec<usize>> = vec![Vec::new(); ng];
        for (ui, u) in units.iter().enumerate() { own_parts[u.region as usize].push(ui); }
        let mut owner_order: Vec<usize> = (0..ng).collect();
        owner_order.sort_by_key(|&o| std::cmp::Reverse(by_owner[o].len()));
        let next = AtomicUsize::new(0);
        // result per owner: final entries (key -> (num, mask)) and per request its (owner num).
        type TOut = (Vec<(u32, u32, u64)>, Vec<u32>, u64);
        let tres: Vec<std::sync::Mutex<TOut>> = (0..ng).map(|_| std::sync::Mutex::new((Vec::new(), Vec::new(), 0))).collect();
        std::thread::scope(|sc| {
            for ti in 0..threads {
                let (next, owner_order, by_owner, own_parts, outs, tres, old_ents, cpu) = (&next, &owner_order, &by_owner, &own_parts, &outs, &tres, &old_ents, cpus[ti]);
                sc.spawn(move || {
                    pin(cpu);
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= owner_order.len() { break; }
                        let o = owner_order[i];
                        // the owner's set: old entries (untouched parts included) + its units' own entries
                        let mut set: FxHashMap<u32, (u32, u64)> = old_ents[o].iter().enumerate().map(|(i, &(k, m))| (k, (i as u32, m))).collect();
                        for &ui in &own_parts[o] { for &(k, num, m) in &outs[ui].own { let e = set.entry(k).or_insert((num, 0)); e.0 = num; e.1 |= m; } }
                        let mut rq: Vec<(u32, u8, usize)> = by_owner[o].iter().enumerate().map(|(j, &(ui, k))| { let r = outs[ui].reqs[k]; (r.key, r.opos, j) }).collect();
                        rq.sort_unstable();
                        let mut nums = vec![0u32; rq.len()];
                        let (mut new, mut rank) = (0u64, 0u32);
                        for &(k, p, j) in &rq {
                            let e = set.entry(k).or_insert_with(|| { let num = NEW | 1023 << 20 | rank; rank += 1; (num, 0) });
                            if e.1 >> p & 1 == 0 { e.1 |= 1 << p; new += 1; }
                            nums[j] = e.0;
                        }
                        let mut fin: Vec<(u32, u32, u64)> = set.into_iter().map(|(k, (num, m))| (k, num, m)).collect();
                        fin.sort_unstable();
                        *tres[o].lock().unwrap() = (fin, nums, new);
                    }
                });
            }
        });
        let t_tr = tt.elapsed().as_secs_f64();
        crate::perf_on(false);
        let tres: Vec<TOut> = tres.into_iter().map(|m| m.into_inner().unwrap()).collect();
        let own_new: u64 = outs.iter().map(|o| o.new).sum();
        let tr_new: u64 = tres.iter().map(|t| t.2).sum();
        let nreq: usize = outs.iter().map(|o| o.reqs.len()).sum();
        let edge_bytes: usize = outs.iter().map(|o| o.len).sum();
        let lid_bytes: usize = outs.iter().map(|o| o.lid_key.len() * 4).sum();
        let new = own_new + tr_new;
        println!("  RUN ghost{} threads {threads:>2} split {split:>7}: {} units; OWN pass {t_own:.3} s + TRANSLATION {t_tr:.3} s = {:.3} s ({:.2} ns an edge); new {new} = own {own_new} + via ghosts {tr_new} ({}); ghost requests {nreq}; edges {:.3} B an edge (+ lid->key tables {:.3} B); units built {t_units:.1} s, prefault {t_pre:.2} s",
            if write { "" } else { "-noedges" }, units.len(), t_own + t_tr, (t_own + t_tr) * 1e9 / n_edges as f64, if new == 6_735_699 { "OK" } else { "MISMATCH" }, edge_bytes as f64 / n_edges as f64, lid_bytes as f64 / n_edges as f64);
        if !write || std::env::var_os("EDGE_NOCHECK").is_some() { continue; }
        // UNTIMED CHECK: decode every edge to (src shape, cell, key, tgt shape, cell, key, xfer) and compare with the capture;
        // the translation maps every ghost to its owner's (num, cell) holding that key; the id bijection over all states.
        let tv = Instant::now();
        let cell_of = |x: i32, y: i32| ((y + 64) * 512 + (x + 64)) as u64;
        let (mut fp, mut cnt, mut bad_tr, mut runs) = (0u64, 0u64, 0u64, 0u64);
        let mut fin_of: Vec<FxHashMap<u32, (u32, u64)>> = tres.iter().map(|t| t.0.iter().map(|&(k, num, m)| (k, (num, m))).collect()).collect();
        for (ui, o) in outs.iter().enumerate() {
            let u = &units[ui];
            let r = u.region as usize; let (cx, cy) = geo[r]; let sh = gshape[r] as u64;
            let b = &scr[o.thread].arena[o.off..o.off + o.len];
            let mut i = 0;
            assert_eq!(get_varint(b, &mut i) as usize, ui);
            let total = get_varint(b, &mut i);
            let mut src = 0u64;
            let mut prev_run: Option<(usize, u64, u64, i64)> = None;
            for _ in 0..total {
                let s = get_varint(b, &mut i); if s != 0 { src = s - 1; }
                let tl = get_varint(b, &mut i); let x = get_varint(b, &mut i);
                let (se, sp) = ((src >> 6) as usize, (src & 63) as i32);
                let skey = old_ents[r][se].0 as u64;
                let scell = cell_of(cx + sp % 8, cy + sp / 8);
                let lid = (tl >> 9) as usize;
                let (tsh, tcell, tkey) = if tl >> 8 & 1 == 1 {
                    let (w, key) = o.over[lid]; let (ow, op) = ((w >> 6) as usize, (w & 63) as i32);
                    let (ox, oy) = geo[ow]; (gshape[ow] as u64, cell_of(ox + op % 8, oy + op / 8), key as u64)
                } else { let w = (tl & 255) as i32; (sh, cell_of(cx - HALO + w % W, cy - HALO + w / W), o.lid_key[lid] as u64) };
                let run_key = (se, tl >> 8 << 8, x, (tl & 255) as i64 - (sp % 8 + HALO + W * (sp / 8 + HALO)) as i64);
                if Some(run_key) != prev_run { runs += 1; prev_run = Some(run_key); }
                fp = fp.wrapping_add(mixf(&[sh, scell, skey, tsh, tcell, tkey, x])); cnt += 1;
            }
            assert_eq!(i, b.len());
        }
        // translation check
        for o in 0..ng { for (j, &(ui, k)) in by_owner[o].iter().enumerate() { let rq = outs[ui].reqs[k]; let num = tres[o].1[j]; match fin_of[o].get(&rq.key) { Some(&(nn, m)) if nn == num && m >> rq.opos & 1 == 1 => {} _ => bad_tr += 1 } } }
        // bijection: every state -> (owner region, num, cell) from the final sets
        let mut ids: FxHashSet<(u32, u32, u8)> = FxHashSet::default(); ids.reserve(n);
        let mut bad = 0u64;
        for i in 0..n {
            let r = rreg[i] as usize;
            match fin_of[r].get(&rkey[i]) { Some(&(num, m)) if m >> rpos[i] & 1 == 1 => { if !ids.insert((r as u32, num, rpos[i])) { bad += 1; } } _ => bad += 1 }
        }
        let total_states: usize = fin_of.iter_mut().map(|m| m.values().map(|v| v.1.count_ones() as usize).sum::<usize>()).sum();
        println!("    CHECK: {cnt} edges decoded, fingerprint {fp:016x} ({}); translations {nreq}, {bad_tr} wrong; id bijection: {} ids over {n} states, {bad} violations; final sets hold {total_states} states; check {:.1} s",
            if fp == fp_cap && cnt == n_edges { "EQUAL to the capture's" } else { "MISMATCH" }, ids.len(), tv.elapsed().as_secs_f64());
        println!("    bundling of CONSECUTIVE edges in emission order with the same (src entry, tgt entry, transfer, shift): {runs} runs ({:.3} edges a run)", n_edges as f64 / runs as f64);
    }
}
