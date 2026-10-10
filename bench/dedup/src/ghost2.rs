//! `ghost2`: N2's ghost layout with (1) a TIGHT bit-packed edge stream,
//! (2) an 8-byte emission record plus per-unit SOURCE RUNS (the source is the
//! kernel call's context, not a per-lane field), (3) a translation pass that
//! reuses the owners' sorted part lists from the own pass (binary search, no
//! per-owner hash rebuild). DESIGNS N3.
//!
//! Emission (u64): target key 32 | transfer 17 | overflow 1 | 14: the target's
//! cell in the source region's 16x16 window, or for an overflow target an
//! index into the unit's side list (owner region << 6 | cell). A unit's
//! emissions come in source runs (src local = old entry << 6 | cell, count).
//!
//! Edge bit stream, per unit: [per source run: src 24 bits, gamma(count)];
//! per edge: overflow 1 bit; target: gamma(zigzag(lid - previous lid) + 1)
//! (a new lid is the next number, so small), and for a window target the shift
//! from the source cell, gamma(zz(dx) + 1) gamma(zz(dy) + 1); transfer through
//! a per-unit dictionary: gamma(index + 2), or gamma(1) + 17 raw bits when new.
//!
//! Args: runs "T:split"; env GHOST_NOEDGES (dedup only), GHOST_STREAM (read
//! the emission stream only: the harness's stream cost).

use crate::bits::{packing, Fields};
use crate::structure::cell_xy;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

const NEW: u32 = 1 << 31;
const HALO: i32 = 4;
const W: i32 = 8 + 2 * HALO;

#[inline] fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u64 { let mut v = 0u64; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u64) << s; if x < 0x80 { return v; } s += 7; } }
fn hpart(k: u32, parts: u32) -> u32 { (k.wrapping_mul(0x85EB_CA6B).rotate_left(13)) % parts }
fn mixf(t: &[u64; 7]) -> u64 { t.iter().fold(0x1234_5678u64, |a, &x| crate::bits::mix64(a ^ x).wrapping_add(x)) }
#[inline] fn zz(v: i64) -> u64 { ((v << 1) ^ (v >> 63)) as u64 }
#[inline] fn unzz(v: u64) -> i64 { (v >> 1) as i64 ^ -((v & 1) as i64) }

/// Bit writer: LSB-first into a byte vector (the vector is pre-sized; `pos` in bits).
struct Bw<'a> { buf: &'a mut [u8], acc: u64, n: u32, at: usize }
impl Bw<'_> {
    #[inline(always)] fn put(&mut self, v: u64, len: u32) {
        debug_assert!(len <= 32);
        self.acc |= v << self.n; self.n += len;
        if self.n >= 32 { self.buf[self.at..self.at + 4].copy_from_slice(&(self.acc as u32).to_le_bytes()); self.at += 4; self.acc >>= 32; self.n -= 32; }
    }
    /// Elias gamma, LSB-first: nb zeros, a one, then the low nb bits of v.
    #[inline(always)] fn gamma(&mut self, v: u64) {
        debug_assert!(v >= 1); let nb = 63 - v.leading_zeros();
        if nb < 16 { self.put((1 << nb) | (v & ((1 << nb) - 1)) << (nb + 1), 2 * nb + 1); }
        else { self.put(1 << nb, nb + 1); let mut k = 0; while k < nb { let w = (nb - k).min(16); self.put(v >> k & ((1 << w) - 1), w); k += w; } }
    }
    fn finish(mut self) -> usize { while self.n > 0 { self.buf[self.at] = self.acc as u8; self.at += 1; self.acc >>= 8; self.n = self.n.saturating_sub(8); } self.at }
}
struct Br<'a> { b: &'a [u8], pos: usize }
impl Br<'_> {
    fn bit(&mut self) -> u64 { let v = (self.b[self.pos / 8] >> (self.pos % 8)) & 1; self.pos += 1; v as u64 }
    fn get(&mut self, len: u32) -> u64 { let mut v = 0; for i in 0..len { v |= self.bit() << i; } v }
    fn gamma(&mut self) -> u64 { let mut nb = 0; while self.bit() == 0 { nb += 1; } (1 << nb) | self.get(nb) }
}

struct Unit { region: u32, part: u32, rest: Vec<u8>, em: Vec<u64>, side: Vec<u32>, runs: Vec<(u32, u32)> }

#[derive(Clone, Copy)] struct E { key: u32, num: u32, lid: u32, core: u64, win: [u64; 4] }
#[derive(Clone, Copy, Default)] struct Req { owner: u32, key: u32, opos: u8 }

#[derive(Default)] struct Out { own: Vec<(u32, u32, u64)>, lid_key: Vec<u32>, over: Vec<(u32, u32)>, xdict: Vec<u32>, reqs: Vec<Req>, new: u64, thread: usize, off: usize, len: usize }

#[derive(Clone, Copy)] struct Px { k: u64, gen: u32, set: [u32; 4] }
struct Scratch { px: Vec<Px>, pgen: u32, tab: Vec<E>, arena: Vec<u8>, used: usize, lid_slot: Vec<u32>, over: FxHashMap<(u32, u32), u32>, xmap: Vec<u64>, ep: u64 }

#[inline(always)]
fn slot(tab: &[E], mask: usize, k: u32) -> usize { let mut i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & mask; loop { let x = tab[i].key; if x == k || x == u32::MAX { return i; } i = (i + 1) & mask; } }

#[derive(Clone, Copy, PartialEq)] enum Mode { Edges, NoEdges, Stream }

fn run_unit(u: &Unit, geo: &[(i32, i32)], owner_of: &dyn Fn(u32, i32, i32) -> Option<(u32, u8)>, s: &mut Scratch, ti: usize, mode: Mode) -> Out {
    if mode == Mode::Stream {
        let mut acc = 0u64;
        for r in &u.runs { acc = acc.wrapping_add(r.0 as u64 * r.1 as u64); }
        for &e in &u.em { acc = acc.wrapping_add(e); }
        std::hint::black_box(acc);
        return Out { thread: ti, ..Default::default() };
    }
    let b = &u.rest; let mut i = 0;
    let n = get_varint(b, &mut i) as usize;
    let cap = ((n + u.em.len().min(4 * n + 64)) * 2).next_power_of_two().max(16);
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
    let mut pcap = u.em.len().next_power_of_two().max(1024); if s.px.len() < pcap { s.px.resize(pcap, Px { k: 0, gen: 0, set: [0; 4] }); }
    s.pgen += 1; let mut pgen = s.pgen; let mut pm = pcap - 1; let mut pused = 0usize;
    let mut over_keys: Vec<(u32, u32)> = Vec::new();
    let mut over_lx: Vec<u32> = Vec::new();
    let (mut new, mut own_new_entries) = (0u64, 0u32);
    s.ep += 1; let ep = s.ep;
    let mut xdict: Vec<u32> = Vec::new();
    let off = s.used;
    let write = mode == Mode::Edges;
    let mut bw = Bw { buf: &mut s.arena[off..], acc: 0, n: 0, at: 0 };
    let (mut plid, mut polid) = (0i64, 0i64);
    let mut ei = 0usize;
    let mut mru = [u32::MAX; 64]; let mut last_src = u32::MAX;
    for &(src, cnt) in &u.runs {
        let (spx, spy) = ((src & 63) as i32 % 8 + HALO, (src & 63) as i32 / 8 + HALO);
        for &e in &u.em[ei..ei + cnt as usize] {
            if write {
                // the source: '1' the previous edge's; '01' + its slot in a 64-entry direct-mapped cache (a kernel call's lanes); '00' + 24 raw bits
                if src == last_src { bw.put(1, 1); } else {
                    let q = (src.wrapping_mul(0x9E37_79B1) >> 26) as usize;
                    if mru[q] == src { bw.put(0b10 | (q as u64) << 2, 8); } else { bw.put(0, 2); bw.put(src as u64, 24); mru[q] = src; }
                    last_src = src;
                }
            }
            let key = (e >> 32) as u32; let x = (e >> 15 & 0x1ffff) as u32; let ovf = e >> 14 & 1 == 1; let w = (e & 0x3fff) as u32;
            let lastx: u64; let mut sh: Option<(i64, i64)> = None;
            if ovf {
                let ow = u.side[w as usize];
                let nl = over_keys.len() as u32;
                let lid = *s.over.entry((ow, key)).or_insert_with(|| { over_keys.push((ow, key)); nl });
                if write { bw.put(1, 1); bw.gamma(zz(lid as i64 - polid) + 1); }
                polid = lid as i64;
                if over_lx.len() <= lid as usize { over_lx.push(u32::MAX); }
                let _ = &mut over_lx; let kx = (src >> 6) as u64 | (lid as u64) << 24 | 1 << 63; lastx = kx;
            } else {
                let mut t = slot(&s.tab, mask, key);
                if s.tab[t].key == u32::MAX {
                    if (used + 1) * 10 > s.tab.len() * 7 {
                        let old: Vec<E> = s.tab.drain(..).filter(|h| h.key != u32::MAX).collect();
                        let nc = (mask + 1) * 2; mask = nc - 1;
                        s.tab.resize(nc, E { key: u32::MAX, num: u32::MAX, lid: u32::MAX, core: 0, win: [0; 4] });
                        for h in old { let q = slot(&s.tab, mask, h.key); s.tab[q] = h; }
                        for (q, h) in s.tab.iter().enumerate() { if h.lid != u32::MAX && h.key != u32::MAX { s.lid_slot[h.lid as usize] = q as u32; } }
                        t = slot(&s.tab, mask, key);
                    }
                    s.tab[t].key = key; used += 1;
                }
                let h = &mut s.tab[t];
                if h.lid == u32::MAX { h.lid = s.lid_slot.len() as u32; s.lid_slot.push(t as u32); }
                let (wx, wy) = (w as i32 % W, w as i32 / W);
                if (HALO..HALO + 8).contains(&wx) && (HALO..HALO + 8).contains(&wy) {
                    let cb = 1u64 << ((wx - HALO) + 8 * (wy - HALO));
                    if h.core & cb == 0 { h.core |= cb; new += 1; if h.num == u32::MAX { h.num = NEW | u.part << 20 | own_new_entries; own_new_entries += 1; } }
                } else { h.win[w as usize / 64] |= 1 << (w % 64); }
                if write { bw.put(0, 1); bw.gamma(zz(h.lid as i64 - plid) + 1); }
                sh = Some(((wx - spx) as i64, (wy - spy) as i64));
                plid = h.lid as i64;
                let kx = (src >> 6) as u64 | (h.lid as u64) << 24; lastx = kx;
            }
            if write {
                // the transfer: its slot among the (up to 4) transfers this (source entry, target) pair used before, else the unit dictionary
                let combo = x | sh.map_or(0, |(dx, dy)| { debug_assert!(dx.abs() < 16 && dy.abs() < 16); ((dx + 16) as u32) << 22 | ((dy + 16) as u32) << 17 | 1 << 27 });
                let mut pi = (lastx.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 40) as usize & pm;
                loop { let e = &s.px[pi]; if e.gen != pgen { s.px[pi] = Px { k: lastx, gen: pgen, set: [u32::MAX; 4] }; pused += 1; break; } if e.k == lastx { break; } pi = (pi + 1) & pm; }
                if pused * 10 > pcap * 7 {
                    // grow (rare): re-insert this generation's pairs into a doubled table
                    let old: Vec<Px> = s.px[..pcap].iter().filter(|e| e.gen == pgen).copied().collect();
                    pcap *= 2; pm = pcap - 1; if s.px.len() < pcap { s.px.resize(pcap, Px { k: 0, gen: 0, set: [0; 4] }); }
                    s.pgen += 1; pgen = s.pgen;
                    for mut e in old { e.gen = pgen; let mut q = (e.k.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 40) as usize & pm; while s.px[q].gen == pgen { q = (q + 1) & pm; } s.px[q] = e; }
                    pi = (lastx.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 40) as usize & pm; while s.px[pi].k != lastx || s.px[pi].gen != pgen { pi = (pi + 1) & pm; }
                }
                let set = &mut s.px[pi].set;
                if let Some(q) = set.iter().position(|&v| v == combo) { bw.put(1 | (q as u64) << 1, 3); } else {
                    bw.put(0, 1);
                    set.copy_within(0..3, 1); set[0] = combo;
                    if let Some((dx, dy)) = sh { bw.gamma(zz(dx) + 1); bw.gamma(zz(dy) + 1); }
                    let xm = s.xmap[x as usize];
                    let wbits = 64 - (xdict.len() as u64).leading_zeros();
                    if xm >> 32 == ep { bw.put((xm as u32) as u64, wbits); } else { s.xmap[x as usize] = ep << 32 | xdict.len() as u64; bw.put(xdict.len() as u64, wbits); xdict.push(x); bw.put(x as u64, 17); }
                }
            }
        }
        ei += cnt as usize;
    }
    let len = if write { bw.finish() } else { 0 };
    s.used += len;
    // lid -> key, ghost requests, own entries (sorted by key: the re-encode order; the translation binary-searches it)
    let mut lid_key = vec![0u32; s.lid_slot.len()];
    let mut reqs = Vec::new();
    let (cx, cy) = geo[u.region as usize];
    for &t in &s.lid_slot { let h = &s.tab[t as usize]; lid_key[h.lid as usize] = h.key;
        for wq in 0..4 { let mut m = h.win[wq]; while m != 0 { let b = m.trailing_zeros(); m &= m - 1; let wp = wq as u32 * 64 + b;
            let (gx, gy) = (cx - HALO + (wp as i32 % W), cy - HALO + (wp as i32 / W));
            let (ow, opos) = owner_of(u.region, gx, gy).expect("a halo cell without an owner region");
            reqs.push(Req { owner: ow, key: h.key, opos }); } } }
    for &(w, key) in &over_keys { reqs.push(Req { owner: w >> 6, key, opos: (w & 63) as u8 }); }
    let mut own: Vec<(u32, u32, u64)> = s.tab.iter().filter(|h| h.key != u32::MAX && h.core != 0).map(|h| (h.key, h.num, h.core)).collect();
    own.sort_unstable_by_key(|x| x.0);
    Out { own, lid_key, over: over_keys, xdict, reqs, new, thread: ti, off, len }
}

fn pin(cpu: usize) { unsafe { let mut set: libc::cpu_set_t = std::mem::zeroed(); libc::CPU_SET(cpu, &mut set); libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set); } }

pub fn ghost2(fields_dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap], args: &[String]) {
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
    // Per SOURCE region, capture order: (src local, target key, xfer, window cell | overflow (owner << 6 | cell)).
    let mut raw: Vec<Vec<(u32, u32, u32, u32, bool)>> = vec![Vec::new(); ng];
    let (mut fp_cap, mut n_edges) = (0u64, 0u64);
    for (wn, m) in maps.iter().enumerate() {
        let b = std::fs::read(format!("{cap}/x{wn:03}.bin")).expect("per-worker transfer table");
        let local: Vec<u32> = b.chunks_exact(26).map(|c| { let a: [u8; 26] = c.try_into().unwrap(); let k = pairs.len() as u32; *intern.entry(a).or_insert_with(|| { pairs.push(a); k }) }).collect();
        for c in m.chunks_exact(48) {
            let r = crate::rec(c);
            if r.flags != 0 { continue; }
            let s = src_row[&r.src]; let t = row_of[&r.key];
            let x = local[r.xfer as usize];
            assert!(x < 1 << 17);
            let sr = rreg[s as usize];
            let se = old_num(sr, rkey[s as usize]).expect("a source not in the frame-start set");
            let (cx, cy) = geo[sr as usize]; let (tx, ty) = cell_xy(rcell[t as usize]);
            let (wx, wy) = (tx - (cx - HALO), ty - (cy - HALO));
            let inwin = rshape[t as usize] == rshape[s as usize] && (0..W).contains(&wx) && (0..W).contains(&wy);
            raw[sr as usize].push((se << 6 | rpos[s as usize] as u32, rkey[t as usize], x, if inwin { (wx + W * wy) as u32 } else { rreg[t as usize] << 6 | rpos[t as usize] as u32 }, !inwin));
            fp_cap = fp_cap.wrapping_add(mixf(&[rshape[s as usize] as u64, rcell[s as usize] as u64, rkey[s as usize] as u64, rshape[t as usize] as u64, rcell[t as usize] as u64, rkey[t as usize] as u64, x as u64]));
            n_edges += 1;
        }
    }
    drop(src_row);
    println!("ghost2: {n} states, {ng} regions, {n_edges} edges, {} transfers; capture edge fingerprint {fp_cap:016x}; setup (untimed) {:.1} s", pairs.len(), t0.elapsed().as_secs_f64());
    let mode = if std::env::var_os("GHOST_STREAM").is_some() { Mode::Stream } else if std::env::var_os("GHOST_NOEDGES").is_some() { Mode::NoEdges } else { Mode::Edges };
    let mname = match mode { Mode::Edges => "edges", Mode::NoEdges => "noedges", Mode::Stream => "stream-only" };
    let mut built: Option<(usize, Vec<Unit>)> = None;
    for run in args {
        let v: Vec<&str> = run.split(':').collect();
        let (threads, split): (usize, usize) = (v[0].parse().unwrap(), v[1].parse().unwrap());
        let tu = Instant::now();
        if !built.as_ref().is_some_and(|b| b.0 == split) {
            let mut units = Vec::new();
            for r in 0..ng {
                let parts = if split == 0 { 1 } else { raw[r].len().div_ceil(split).max(1) as u32 };
                for p in 0..parts {
                    let sel: Vec<(u32, (u32, u64))> = old_ents[r].iter().enumerate().filter(|(_, e)| parts == 1 || hpart(e.0, parts) == p).map(|(i, e)| (i as u32, *e)).collect();
                    let mut rest = Vec::new(); varint(&mut rest, sel.len() as u64);
                    let (mut pk, mut pn) = (0u32, 0u32);
                    for (num, (k, m)) in sel { varint(&mut rest, (k - pk) as u64); varint(&mut rest, (num - pn) as u64); rest.extend_from_slice(&m.to_le_bytes()); pk = k; pn = num; }
                    let (mut em, mut side, mut runs) = (Vec::new(), Vec::new(), Vec::<(u32, u32)>::new());
                    let mut side_ix: FxHashMap<u32, u32> = FxHashMap::default();
                    for &(src, key, x, w, ovf) in raw[r].iter().filter(|e| parts == 1 || hpart(e.1, parts) == p) {
                        let low = if ovf { let ix = *side_ix.entry(w).or_insert_with(|| { side.push(w); side.len() as u32 - 1 }); assert!(ix < 1 << 14, "overflow cells of a unit past 14 bits"); ix as u64 | 1 << 14 } else { w as u64 };
                        em.push((key as u64) << 32 | (x as u64) << 15 | low);
                        match runs.last_mut() { Some(rn) if rn.0 == src => rn.1 += 1, _ => runs.push((src, 1)) }
                    }
                    units.push(Unit { region: r as u32, part: p, rest, em, side, runs });
                }
            }
            units.sort_by_key(|u| std::cmp::Reverse(u.em.len()));
            built = Some((split, units));
        }
        let units = &built.as_ref().unwrap().1;
        let t_units = tu.elapsed().as_secs_f64();
        let max_b = units.iter().map(|u| u.em.len()).max().unwrap();
        let max_cap = units.iter().map(|u| { let mut i = 0; let n = get_varint(&u.rest, &mut i) as usize; ((n + u.em.len().min(4 * n + 64)) * 2).next_power_of_two() }).max().unwrap();
        let arena = (n_edges as usize * 8 / threads).max(256 << 20) + 16 * max_b;
        let tp = Instant::now();
        let mut scr: Vec<Scratch> = (0..threads).map(|_| {
            let mut s = Scratch { px: vec![Px { k: 0, gen: 0, set: [0; 4] }; max_b.next_power_of_two().max(1024)], pgen: 0, tab: Vec::with_capacity(max_cap), arena: vec![0u8; arena], used: 0, lid_slot: Vec::with_capacity(max_b), over: FxHashMap::default(), xmap: vec![0; 1 << 17], ep: 0 };
            s.tab.resize(max_cap, E { key: 0, num: 0, lid: 0, core: 0, win: [0; 4] }); s.lid_slot.resize(max_b, 0);
            for b in s.arena.iter_mut().step_by(4096) { *b = 1; }
            s
        }).collect();
        let t_pre = tp.elapsed().as_secs_f64();
        let cpus: Vec<usize> = if threads == 1 { vec![4] } else { (0..threads).collect() };
        let next = AtomicUsize::new(0);
        let outs: Vec<std::sync::Mutex<Out>> = units.iter().map(|_| std::sync::Mutex::new(Out::default())).collect();
        crate::perf_on(true);
        let tw = Instant::now();
        std::thread::scope(|sc| {
            for (ti, s) in scr.iter_mut().enumerate() {
                let (units, next, outs, geo, cpu, owner_of) = (units, &next, &outs, &geo, cpus[ti], &owner_of);
                sc.spawn(move || {
                    pin(cpu);
                    loop { let i = next.fetch_add(1, Ordering::Relaxed); if i >= units.len() { break; } *outs[i].lock().unwrap() = run_unit(&units[i], geo, owner_of, s, ti, mode); }
                });
            }
        });
        let t_own = tw.elapsed().as_secs_f64();
        if mode == Mode::Stream { crate::perf_on(false); println!("  RUN ghost2 {mname} threads {threads:>2} split {split:>7}: {} units; reading the emission stream alone {t_own:.3} s ({:.2} GB)", units.len(), units.iter().map(|u| u.em.len() * 8 + u.runs.len() * 8).sum::<usize>() as f64 / 1e9); continue; }
        let mut outs: Vec<Out> = outs.into_iter().map(|m| m.into_inner().unwrap()).collect();
        // TRANSLATION: per owner, its parts' SORTED own lists (from the own pass) binary-searched; misses are new.
        let tt = Instant::now();
        let mut by_owner: Vec<Vec<(u32, u32)>> = vec![Vec::new(); ng];
        for (ui, o) in outs.iter().enumerate() { for (k, r) in o.reqs.iter().enumerate() { by_owner[r.owner as usize].push((ui as u32, k as u32)); } }
        let mut own_parts: Vec<Vec<usize>> = vec![Vec::new(); ng];
        for (ui, u) in units.iter().enumerate() { own_parts[u.region as usize].push(ui); }
        for v in own_parts.iter_mut() { v.sort_by_key(|&ui| units[ui].part); }
        let mut order: Vec<usize> = (0..ng).collect();
        order.sort_by_key(|&o| std::cmp::Reverse(by_owner[o].len()));
        let next = AtomicUsize::new(0);
        type TOut = (Vec<(u32, u32, u64)>, Vec<u32>, u64); // ghost-new entries, nums per request, new
        let tres: Vec<std::sync::Mutex<TOut>> = (0..ng).map(|_| std::sync::Mutex::new((Vec::new(), Vec::new(), 0))).collect();
        let mut owns: Vec<Vec<(u32, u32, u64)>> = outs.iter_mut().map(|o| std::mem::take(&mut o.own)).collect();
        let owns_ptr = owns.as_mut_ptr() as usize;
        let outs_ref = &outs;
        std::thread::scope(|sc| {
            for ti in 0..threads {
                let (next, order, by_owner, own_parts, tres, cpu) = (&next, &order, &by_owner, &own_parts, &tres, cpus[ti]);
                sc.spawn(move || {
                    pin(cpu);
                    let owns_p = owns_ptr as *mut Vec<(u32, u32, u64)>;
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= order.len() { break; }
                        let o = order[i];
                        let parts = own_parts[o].len() as u32;
                        // requests read through the owner's own units only mutated below (disjoint across owners)
                        let mut rq: Vec<(u32, u8, u32)> = by_owner[o].iter().enumerate().map(|(j, &(ui, k))| { let r = outs_ref[ui as usize].reqs[k as usize]; (r.key, r.opos, j as u32) }).collect();
                        rq.sort_unstable();
                        let mut nums = vec![0u32; rq.len()];
                        let (mut extra, mut new, mut rank) = (Vec::<(u32, u32, u64)>::new(), 0u64, 0u32);
                        for &(k, p, j) in &rq {
                            let ui = own_parts[o][if parts == 1 { 0 } else { hpart(k, parts) as usize }];
                            let own = unsafe { &mut *owns_p.add(ui) };
                            match own.binary_search_by_key(&k, |x| x.0) {
                                Ok(q) => { let e = &mut own[q]; if e.2 >> p & 1 == 0 { e.2 |= 1 << p; new += 1; } nums[j as usize] = e.1; }
                                Err(_) => {
                                    // a key the owner does not hold: the ghost-new entries (requests sorted, so adjacent)
                                    match extra.last_mut() { Some(e) if e.0 == k => { if e.2 >> p & 1 == 0 { e.2 |= 1 << p; new += 1; } nums[j as usize] = e.1; }
                                        _ => { let num = NEW | 1023 << 20 | rank; rank += 1; extra.push((k, num, 1 << p)); new += 1; nums[j as usize] = num; } }
                                }
                            }
                        }
                        *tres[o].lock().unwrap() = (extra, nums, new);
                    }
                });
            }
        });
        let t_tr = tt.elapsed().as_secs_f64();
        crate::perf_on(false);
        let tres: Vec<TOut> = tres.into_iter().map(|m| m.into_inner().unwrap()).collect();
        for (o, w) in outs.iter_mut().zip(owns) { o.own = w; }
        let own_new: u64 = outs.iter().map(|o| o.new).sum();
        let tr_new: u64 = tres.iter().map(|t| t.2).sum();
        let edge_bytes: usize = outs.iter().map(|o| o.len).sum();
        let side_bytes: usize = outs.iter().map(|o| o.lid_key.len() * 4 + o.over.len() * 8 + o.xdict.len() * 3).sum();
        let new = own_new + tr_new;
        println!("  RUN ghost2 {mname} threads {threads:>2} split {split:>7}: {} units; OWN pass {t_own:.3} s + TRANSLATION {t_tr:.3} s = {:.3} s ({:.2} ns an edge); new {new} = {own_new} + {tr_new} ({}); edges {:.3} B an edge + unit tables (lid keys, overflow, transfer dictionary) {:.3} B; units built {t_units:.1} s, prefault {t_pre:.2} s",
            units.len(), t_own + t_tr, (t_own + t_tr) * 1e9 / n_edges as f64, if new == 6_735_699 { "OK" } else { "MISMATCH" }, edge_bytes as f64 / n_edges as f64, side_bytes as f64 / n_edges as f64);
        if mode != Mode::Edges || std::env::var_os("EDGE_NOCHECK").is_some() { continue; }
        // UNTIMED CHECK: decode every unit's bit stream; translations; bijection.
        let tv = Instant::now();
        let cell_of = |x: i32, y: i32| ((y + 64) * 512 + (x + 64)) as u64;
        let (mut fp, mut cnt) = (0u64, 0u64);
        let mut bits = [0u64; 5]; // run headers, flag, target lid, shift, transfer
        for (ui, o) in outs.iter().enumerate() {
            let u = &units[ui];
            let r = u.region as usize; let (cx, cy) = geo[r]; let sh = gshape[r] as u64;
            let mut br = Br { b: &scr[o.thread].arena[o.off..o.off + o.len], pos: 0 };
            let (mut plid, mut polid) = (0i64, 0i64);
            let mut dict: Vec<u32> = Vec::new();
            let mut mru = [u32::MAX; 64]; let mut last_src = u32::MAX;
            let (mut dlx, mut dlx_o): (Vec<u32>, Vec<u32>) = (Vec::new(), Vec::new());
            let mut dmap: FxHashMap<u64, [u32; 4]> = FxHashMap::default();
            for _ in 0..u.em.len() {
                {
                let p0 = br.pos;
                let src = if br.bit() == 1 { last_src } else if br.bit() == 1 { mru[br.get(6) as usize] } else { let v = br.get(24) as u32; mru[(v.wrapping_mul(0x9E37_79B1) >> 26) as usize] = v; v };
                last_src = src;
                bits[0] += (br.pos - p0) as u64;
                let (se, sp) = ((src >> 6) as usize, (src & 63) as i32);
                let skey = old_ents[r][se].0 as u64; let scell = cell_of(cx + sp % 8, cy + sp / 8);
                let (spx, spy) = (sp % 8 + HALO, sp / 8 + HALO);
                    let (tsh, tcell, tkey);
                    bits[1] += 1;
                    let ovf = br.bit() == 1; let tlid;
                    if ovf {
                        let p0 = br.pos; let lid = (polid + unzz(br.gamma() - 1)) as usize; polid = lid as i64; bits[2] += (br.pos - p0) as u64; tlid = lid;
                        let (w, key) = o.over[lid]; let (ow, op) = ((w >> 6) as usize, (w & 63) as i32); let (ox, oy) = geo[ow];
                        tsh = gshape[ow] as u64; tcell = cell_of(ox + op % 8, oy + op / 8); tkey = key as u64;
                    } else {
                        let p0 = br.pos; let lid = (plid + unzz(br.gamma() - 1)) as usize; plid = lid as i64; bits[2] += (br.pos - p0) as u64; tlid = lid;
                        tsh = sh; tcell = 0; tkey = o.lid_key[lid] as u64;
                    }
                    let p0 = br.pos;
                    let _ = (&mut dlx, &mut dlx_o);
                    let kx = (src >> 6) as u64 | (tlid as u64) << 24 | if ovf { 1 << 63 } else { 0 };
                    let set: &mut [u32; 4] = dmap.entry(kx).or_insert([u32::MAX; 4]);
                    let combo = if br.bit() == 1 { set[br.get(2) as usize] } else {
                        let shp = if ovf { 0 } else { let dx = unzz(br.gamma() - 1); let dy = unzz(br.gamma() - 1); ((dx + 16) as u32) << 22 | ((dy + 16) as u32) << 17 | 1 << 27 };
                        let wbits = 64 - (dict.len() as u64).leading_zeros(); let g = br.get(wbits) as usize;
                        let x = if g == dict.len() { let x = br.get(17) as u32; dict.push(x); x } else { dict[g] };
                        let c = x | shp; set.copy_within(0..3, 1); set[0] = c; c };
                    let x = combo & 0x1ffff;
                    let tcell = if ovf { tcell } else { let (dx, dy) = ((combo >> 22 & 31) as i32 - 16, (combo >> 17 & 31) as i32 - 16); cell_of(cx - HALO + spx + dx, cy - HALO + spy + dy) };
                    bits[4] += (br.pos - p0) as u64;
                    fp = fp.wrapping_add(mixf(&[sh, scell, skey, tsh, tcell, tkey, x as u64])); cnt += 1;
                }
            }
            assert!(br.pos <= o.len * 8 && dict == o.xdict);
        }
        println!("    bits an edge: source {:.2}, overflow flag {:.2}, target lid {:.2}, shift {:.2}, transfer {:.2}", bits[0] as f64 / cnt as f64, bits[1] as f64 / cnt as f64, bits[2] as f64 / cnt as f64, bits[3] as f64 / cnt as f64, bits[4] as f64 / cnt as f64);
        // final sets: owners' part lists + ghost-new entries; translations; bijection
        let mut fin: Vec<FxHashMap<u32, (u32, u64)>> = vec![Default::default(); ng];
        for (ui, o) in outs.iter().enumerate() { for &(k, num, m) in &o.own { fin[units[ui].region as usize].insert(k, (num, m)); } }
        for (o, t) in tres.iter().enumerate() { for &(k, num, m) in &t.0 { assert!(fin[o].insert(k, (num, m)).is_none()); } }
        let mut bad_tr = 0u64; let mut nreq = 0u64;
        for o in 0..ng { let mut rq: Vec<(u32, u8, u32)> = by_owner[o].iter().enumerate().map(|(j, &(ui, k))| { let r = outs[ui as usize].reqs[k as usize]; (r.key, r.opos, j as u32) }).collect(); rq.sort_unstable();
            for &(k, p, j) in &rq { nreq += 1; match fin[o].get(&k) { Some(&(num, m)) if num == tres[o].1[j as usize] && m >> p & 1 == 1 => {} _ => bad_tr += 1 } } }
        let mut ids: FxHashSet<(u32, u32, u8)> = FxHashSet::default(); ids.reserve(n);
        let mut bad = 0u64;
        for i in 0..n { let r = rreg[i] as usize; match fin[r].get(&rkey[i]) { Some(&(num, m)) if m >> rpos[i] & 1 == 1 => { if !ids.insert((r as u32, num, rpos[i])) { bad += 1; } } _ => bad += 1 } }
        let held: usize = fin.iter().map(|m| m.values().map(|v| v.1.count_ones() as usize).sum::<usize>()).sum();
        println!("    CHECK: {cnt} edges decoded, fingerprint {fp:016x} ({}); {nreq} translations, {bad_tr} wrong; id bijection {} ids / {n} states, {bad} violations; final sets hold {held}; {:.1} s",
            if fp == fp_cap && cnt == n_edges { "EQUAL to the capture's" } else { "MISMATCH" }, ids.len(), tv.elapsed().as_secs_f64());
    }
}
