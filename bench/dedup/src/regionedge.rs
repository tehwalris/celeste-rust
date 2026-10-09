//! `regionedge`: regionpar (J2) plus the frame's EDGES, written straight out
//! of the parallel region pass (roadmap stage 1, DESIGNS M).
//!
//! Data: a capture with per-worker transfer tables (/var/tmp/emitcap2) and
//! the field dump of its tree. Units = TARGET regions (shape, 8x8), heavy
//! ones split by a hash of the key into k parts.
//!
//! Stable ids: (region, entry number, cell). Old entries (frame start) are
//! numbered by key within the region. A frame's NEW entries are numbered at
//! the unit's end, canonically: `NEW | part << 20 | rank` (rank by key among
//! the part's new entries); dense renumbering is left to the frame boundary.
//!
//! Edge encodings (per unit, into a per-thread pre-touched arena):
//! - `rel`: per (source region, target region) pair, the relation (src entry,
//!   tgt entry, transfer) -> the (src cell, tgt cell) pairs: a uniform shift
//!   + a u64 source-cell mask (>= 8 members) or a list of source cells, else
//!   an explicit list of cell pairs;
//! - `flat`: per target region, sorted (tgt entry, tgt cell, src id),
//!   varint deltas + the transfer id.
//! Transfers are global ids canonical by CONTENT.
//!
//! Args: fmt (none | rel | flat) then runs "T:split", e.g. rel 1:0 16:1000000.

use crate::bits::{packing, Fields};
use crate::structure::cell_xy;
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

const NEW: u32 = 1 << 31;

/// Per-phase seconds (summed over units), printed for the 1-thread runs.
static PH: [std::sync::atomic::AtomicU64; 8] = [const { std::sync::atomic::AtomicU64::new(0) }; 8];
const PH_NAMES: [&str; 8] = ["decode", "dedup", "number", "tgt ranks", "src table", "counting", "emit", "re-encode"];
#[inline] fn ph(i: usize, t: &mut Instant) { let n = Instant::now(); PH[i].fetch_add((n - *t).as_nanos() as u64, std::sync::atomic::Ordering::Relaxed); *t = n; }

#[inline] fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u64 { let mut v = 0u64; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u64) << s; if x < 0x80 { return v; } s += 7; } }

/// One lookup of a unit's batch: the target (key, cell) and the edge payload.
#[derive(Clone, Copy)]
#[repr(C)]
struct L { key: u32, src_entry: u32, xfer: u32, src_region: u16, pos: u8, src_pos: u8 }

/// One unit: its target region and part, the at-rest bytes of its old
/// entries (varint key delta, varint number delta, u64 mask), its batch.
struct Unit { region: u32, part: u32, rest: Vec<u8>, batch: Vec<L> }

#[derive(Clone, Copy)] struct H { key: u32, num: u32, m: u64, nm: u64 }

#[derive(Clone, Copy)] struct G { gk: u128, mask: u64, n: u32, dx: i16, dy: i16, uni: bool }
#[derive(Clone, Copy)] #[repr(C)] struct Gp { key: u64, mask: u64, dx: i16, dy: i16, n: u16, uni: bool }
#[derive(Clone, Copy)] #[repr(C)] struct Ac { hi: u64, mask: u64, lo: u32, gen: u32 }
struct Scratch { ac: Vec<Ac>, acg: u32, acl: Vec<(u128, u32)>, ep: u64, loc2: Vec<u64>, gp: Vec<Gp>, gs: Vec<(u64, u32)>, e1: Vec<u64>, e2: Vec<u64>, loc: Vec<u32>, tl: Vec<u32>, sk: Vec<u32>, cnt: Vec<u32>, tsl: Vec<u32>, trank: Vec<u32>, stab: Vec<(u32, u32)>, slist: Vec<(u32, u32)>, srank: Vec<u32>, tab: Vec<H>, slot: Vec<u32>, tup: Vec<u128>, newk: Vec<(u32, u32)>, arena: Vec<u8>, used: usize, enc: Vec<u8>, gt: Vec<G>, gord: Vec<(u128, u32)>, odd: Vec<(u32, u8, u8)> }

/// What a unit leaves: its new entries' keys in number order, its edge bytes (thread, offset, len).
#[derive(Default, Clone)] struct Out { new_keys: Vec<u32>, thread: usize, off: usize, len: usize, new: u64 }

fn hpart(k: u32, parts: u32) -> u32 { (k.wrapping_mul(0x85EB_CA6B).rotate_left(13)) % parts }

#[inline(always)]
fn slot(tab: &[H], mask: usize, k: u32) -> usize { let mut i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & mask; loop { let x = tab[i].key; if x == k || x == u32::MAX { return i; } i = (i + 1) & mask; } }

/// One relation group: header (first-changed (src region, src entry, transfer)), then its members.
#[inline]
fn emit_group(o: &mut Vec<u8>, prev: &mut Option<[u32; 3]>, k: u64, run: &[u64], geo: &[(i32, i32)], tx0: i32, ty0: i32) {
    let f = [(k >> 35) as u32, (k >> 17 & 0x3ffff) as u32, (k & 0x1ffff) as u32];
    match *prev {
        None => { o.push(255); for v in f { varint(o, v as u64); } }
        Some(p) => { let q = (0..3).find(|&q| f[q] != p[q]).unwrap(); o.push(q as u8); varint(o, f[q].wrapping_sub(p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } }
    }
    *prev = Some(f);
    let (sx0, sy0) = geo[f[0] as usize];
    let shift = |v: u64| { let (sp, tp) = ((v >> 6 & 63) as i32, (v & 63) as i32); ((tx0 + tp % 8) - (sx0 + sp % 8), (ty0 + tp / 8) - (sy0 + sp / 8)) };
    let sh = shift(run[0]);
    let uni = run.iter().all(|&v| shift(v) == sh);
    let n = run.len();
    varint(o, (n as u64) << 1 | uni as u64);
    if uni {
        varint(o, ((sh.0 as i64) << 1 ^ (sh.0 as i64) >> 63) as u64); varint(o, ((sh.1 as i64) << 1 ^ (sh.1 as i64) >> 63) as u64);
        if n >= 8 { let mut m = 0u64; for &v in run { m |= 1 << (v >> 6 & 63); } o.extend_from_slice(&m.to_le_bytes()); }
        else { for &v in run { o.push((v >> 6 & 63) as u8); } }
    } else { for &v in run { o.push((v >> 6 & 63) as u8); o.push((v & 63) as u8); } }
}

/// The pipeline on one unit: decode, dedup, number the new entries, write edges, re-encode.
fn run_unit(u: &Unit, fmt: &str, geo: &[(i32, i32)], s: &mut Scratch, ti: usize) -> Out {
    let mut tph = Instant::now();
    // Decode the old entries into the table.
    let b = &u.rest;
    let mut i = 0;
    let n = get_varint(b, &mut i) as usize;
    let cap = ((n + u.batch.len().min(4 * n + 64)) * 2).next_power_of_two().max(16);
    s.tab.clear(); s.tab.resize(cap, H { key: u32::MAX, num: 0, m: 0, nm: 0 });
    let mut mask = cap - 1;
    let (mut k, mut num) = (0u32, 0u32);
    for _ in 0..n {
        k += get_varint(b, &mut i) as u32; num += get_varint(b, &mut i) as u32;
        let m = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8;
        let t = slot(&s.tab, mask, k); s.tab[t] = H { key: k, num, m, nm: 0 };
    }
    let mut used = n;
    ph(0, &mut tph);
    // Dedup; remember each lookup's slot.
    s.slot.clear();
    let mut new = 0u64;
    for l in &u.batch {
        let mut t = slot(&s.tab, mask, l.key);
        if s.tab[t].key == u32::MAX {
            if (used + 1) * 10 > s.tab.len() * 7 {
                // Grow: slots move, so re-map the recorded ones by key afterwards.
                let old: Vec<H> = s.tab.drain(..).filter(|h| h.key != u32::MAX).collect();
                let nc = (mask + 1) * 2; mask = nc - 1;
                s.tab.resize(nc, H { key: u32::MAX, num: 0, m: 0, nm: 0 });
                for h in old { let q = slot(&s.tab, mask, h.key); s.tab[q] = h; }
                for (j, sl) in s.slot.iter_mut().enumerate() { *sl = slot(&s.tab, mask, u.batch[j].key) as u32; }
                t = slot(&s.tab, mask, l.key);
            }
            s.tab[t] = H { key: l.key, num: u32::MAX, m: 0, nm: 0 }; used += 1;
        }
        let h = &mut s.tab[t];
        let bit = 1u64 << l.pos;
        if (h.m | h.nm) & bit == 0 { h.nm |= bit; new += 1; }
        s.slot.push(t as u32);
    }
    ph(1, &mut tph);
    // Number the new entries canonically: by key, NEW | part << 20 | rank.
    s.newk.clear();
    for (t, h) in s.tab.iter().enumerate() { if h.key != u32::MAX && h.num == u32::MAX { s.newk.push((h.key, t as u32)); } }
    s.newk.sort_unstable();
    assert!(s.newk.len() < 1 << 20 && u.part < 1 << 10);
    for (r, &(_, t)) in s.newk.iter().enumerate() { s.tab[t as usize].num = NEW | u.part << 20 | r as u32; }
    let out_new_keys: Vec<u32> = s.newk.iter().map(|x| x.0).collect();
    ph(2, &mut tph);
    // Edges.
    let off = s.used;
    if fmt == "relf" {
        // A flat open-addressing ACCUMULATOR per unit, cleared by generation:
        // key (target slot 19 | src region 9 | src entry 18 | transfer 17, shift dx dy),
        // value the u64 source-cell mask. One probe + one OR an edge. Then
        // only the bundles (~1/5 of the edges) are ranked and sorted.
        let nb = u.batch.len();
        let (tx0, ty0) = geo[u.region as usize];
        assert!(s.tab.len() <= 1 << 19);
        let need = (nb / 2).next_power_of_two().max(64);
        if s.ac.len() < need { s.ac.resize(need, Ac { hi: 0, mask: 0, lo: 0, gen: 0 }); }
        s.acg = s.acg.wrapping_add(1); if s.acg == 0 { for e in s.ac.iter_mut() { e.gen = 0; } s.acg = 1; }
        let mut gen_cell = s.acg;
        let mut cap = need; let mut m = cap - 1;
        let mut used = 0usize;
        for (j, l) in u.batch.iter().enumerate() {
            let hi = (s.slot[j] as u64) << 45 | (l.src_region as u64) << 36 | (l.src_entry as u64) << 18 | l.xfer as u64;
            let (sx0, sy0) = geo[l.src_region as usize];
            let (sp, tp) = (l.src_pos as i32, l.pos as i32);
            let lo = (((tx0 + (tp & 7)) - (sx0 + (sp & 7)) + 32768) as u32) << 16 | (((ty0 + (tp >> 3)) - (sy0 + (sp >> 3)) + 32768) as u32 & 0xffff);
            let h = |hi: u64, lo: u32| ((hi ^ (lo as u64).rotate_left(29)).wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 40) as usize;
            let mut i = h(hi, lo) & m;
            loop {
                let e = &mut s.ac[i];
                if e.gen != gen_cell { *e = Ac { hi, mask: 1 << sp, lo, gen: gen_cell }; used += 1; break; }
                if e.hi == hi && e.lo == lo { e.mask |= 1 << sp; break; }
                i = (i + 1) & m;
            }
            if used * 10 > cap * 7 {
                // grow (rare): re-insert this generation's entries into a doubled table
                let old: Vec<Ac> = s.ac[..cap].iter().filter(|e| e.gen == gen_cell).copied().collect();
                cap *= 2; m = cap - 1;
                if s.ac.len() < cap { s.ac.resize(cap, Ac { hi: 0, mask: 0, lo: 0, gen: 0 }); }
                s.acg = s.acg.wrapping_add(1); if s.acg == 0 { for e in s.ac.iter_mut() { e.gen = 0; } s.acg = 1; }
                let g2 = s.acg;
                for mut e in old { e.gen = g2; let mut q = h(e.hi, e.lo) & m; while s.ac[q].gen == g2 { q = (q + 1) & m; } s.ac[q] = e; }
                gen_cell = g2;
            }
        }
        let gen = gen_cell;
        let ac = &s.ac[..cap];
        ph(3, &mut tph);
        // Collect the bundles; target ranks by number; sort by (rank, src region, src entry, transfer, shift).
        s.tsl.clear();
        s.acl.clear();
        s.ep += 1; let ep = s.ep;
        if s.loc2.len() < s.tab.len() { s.loc2.resize(s.tab.len(), 0); }
        for (i, e) in ac.iter().enumerate() {
            if e.gen != gen { continue; }
            let t = (e.hi >> 45) as usize;
            if s.loc2[t] >> 32 != ep { s.loc2[t] = ep << 32 | s.tsl.len() as u64; s.tsl.push(t as u32); }
            s.acl.push(((e.hi & ((1 << 45) - 1)) as u128 | (s.loc2[t] as u32 as u128) << 77 | (e.lo as u128) << 45, i as u32));
        }
        s.newk.clear(); for (li, &t) in s.tsl.iter().enumerate() { s.newk.push((s.tab[t as usize].num, li as u32)); }
        s.newk.sort_unstable();
        s.trank.clear(); s.trank.resize(s.tsl.len(), 0); for (r, &(_, li)) in s.newk.iter().enumerate() { s.trank[li as usize] = r as u32; }
        // re-key: rank in place of the local id, (src region, src entry, xfer) above the shift
        for x in s.acl.iter_mut() { let li = (x.0 >> 77) as usize; let hi = x.0 as u64 & ((1 << 45) - 1); let lo = (x.0 >> 45) as u32; x.0 = (s.trank[li] as u128) << 77 | (hi as u128) << 32 | lo as u128; }
        s.acl.sort_unstable_by_key(|x| x.0);
        ph(5, &mut tph);
        let o = &mut s.enc; o.clear();
        varint(o, u.region as u64); varint(o, nb as u64);
        let (mut a, mut ptn) = (0usize, u32::MAX);
        while a < s.acl.len() {
            let r = (s.acl[a].0 >> 77) as usize;
            let mut e = a; while e < s.acl.len() && (s.acl[e].0 >> 77) as usize == r { e += 1; }
            let tnum = s.newk[r].0;
            varint(o, tnum.wrapping_sub(ptn) as u64); ptn = tnum;
            varint(o, (e - a) as u64);
            let mut prev: Option<[u32; 3]> = None;
            for &(k, i) in &s.acl[a..e] {
                let hi = (k >> 32) as u64; let lo = k as u32;
                let f = [(hi >> 36 & 0x1ff) as u32, (hi >> 18 & 0x3ffff) as u32, (hi & 0x1ffff) as u32];
                match prev {
                    None => { o.push(255); for v in f { varint(o, v as u64); } }
                    Some(p) => match (0..3).find(|&q| f[q] != p[q]) { Some(q) => { o.push(q as u8); varint(o, f[q].wrapping_sub(p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } } None => o.push(3) },
                }
                prev = Some(f);
                let mask = ac[i as usize].mask;
                let n = mask.count_ones() as u64;
                varint(o, n << 1 | 1);
                let (dx, dy) = ((lo >> 16) as i64 - 32768, (lo & 0xffff) as i64 - 32768);
                varint(o, (dx << 1 ^ dx >> 63) as u64); varint(o, (dy << 1 ^ dy >> 63) as u64);
                if n >= 8 { o.extend_from_slice(&mask.to_le_bytes()); } else { let mut mm = mask; while mm != 0 { o.push(mm.trailing_zeros() as u8); mm &= mm - 1; } }
            }
            a = e;
        }
    } else if fmt == "relt" {
        // TARGET-major, no hash and no global sort: dense target ranks (by
        // number), ONE counting pass scattering packed u64 edges
        // [src region 9 | src entry 18 | transfer 17 | src cell 6 | tgt cell 6]
        // by target, then each target's small segment sorted in place: its
        // groups (src region, src entry, transfer) are runs.
        let nb = u.batch.len();
        let (tx0, ty0) = geo[u.region as usize];
        s.ep += 1; let ep = s.ep;
        if s.loc2.len() < s.tab.len() { s.loc2.resize(s.tab.len(), 0); }
        s.tsl.clear(); s.tl.clear();
        for j in 0..nb {
            let t = s.slot[j] as usize;
            let e = s.loc2[t];
            let id = if e >> 32 == ep { e as u32 } else { let id = s.tsl.len() as u32; s.loc2[t] = ep << 32 | id as u64; s.tsl.push(t as u32); id };
            s.tl.push(id);
        }
        let nt = s.tsl.len();
        s.newk.clear(); for (li, &t) in s.tsl.iter().enumerate() { s.newk.push((s.tab[t as usize].num, li as u32)); }
        s.newk.sort_unstable();
        s.trank.clear(); s.trank.resize(nt, 0); for (r, &(_, li)) in s.newk.iter().enumerate() { s.trank[li as usize] = r as u32; }
        ph(3, &mut tph);
        s.cnt.clear(); s.cnt.resize(nt + 1, 0);
        for j in 0..nb { let r = s.trank[s.tl[j] as usize]; s.tl[j] = r; s.cnt[r as usize + 1] += 1; }
        for i in 1..=nt { s.cnt[i] += s.cnt[i - 1]; }
        s.e1.clear(); s.e1.resize(nb, 0);
        for (j, l) in u.batch.iter().enumerate() {
            let r = s.tl[j] as usize;
            s.e1[s.cnt[r] as usize] = (l.src_region as u64) << 47 | (l.src_entry as u64) << 29 | (l.xfer as u64) << 12 | (l.src_pos as u64) << 6 | l.pos as u64;
            s.cnt[r] += 1;
        }
        ph(5, &mut tph);
        let o = &mut s.enc; o.clear();
        o.reserve(8 * nb + 64);
        varint(o, u.region as u64); varint(o, nb as u64);
        let mut ptn = u32::MAX;
        let mut start = 0usize;
        for r in 0..nt {
            let end = s.cnt[r] as usize;
            let tnum = s.newk[r].0;
            varint(o, tnum.wrapping_sub(ptn) as u64); ptn = tnum;
            let seg = &mut s.e1[start..end];
            if seg.len() <= 32 {
                // small: insertion sort; groups are runs
                for i in 1..seg.len() { let v = seg[i]; let mut k = i; while k > 0 && seg[k - 1] > v { seg[k] = seg[k - 1]; k -= 1; } seg[k] = v; }
                let mut ng = 0u64; let mut pk = u64::MAX; for &v in seg.iter() { if v >> 12 != pk { ng += 1; pk = v >> 12; } }
                varint(o, ng);
                let mut prev: Option<[u32; 3]> = None;
                let mut c = 0;
                while c < seg.len() {
                    let v0 = seg[c];
                    let mut d = c + 1; while d < seg.len() && seg[d] >> 12 == v0 >> 12 { d += 1; }
                    emit_group(o, &mut prev, v0 >> 12, &seg[c..d], geo, tx0, ty0);
                    c = d;
                }
            } else {
                // large: sort the segment; groups are runs
                seg.sort_unstable();
                let mut ng = 0u64; let mut pk = u64::MAX; for &v in seg.iter() { if v >> 12 != pk { ng += 1; pk = v >> 12; } }
                varint(o, ng);
                let mut prev: Option<[u32; 3]> = None;
                let mut c = 0;
                while c < seg.len() {
                    let v0 = seg[c];
                    let mut d = c + 1; while d < seg.len() && seg[d] >> 12 == v0 >> 12 { d += 1; }
                    emit_group(o, &mut prev, v0 >> 12, &seg[c..d], geo, tx0, ty0);
                    c = d;
                }
            }
            start = end;
        }
    } else if fmt == "relg" {
        // ONE hash pass groups the edges by (src region, src entry, target
        // slot, transfer) -> shift + source-cell mask; then only the GROUPS
        // (~1/5 of the edges) are ranked and sorted.
        let nb = u.batch.len();
        let (tx0, ty0) = geo[u.region as usize];
        assert!(s.tab.len() <= 1 << 19);
        let gcap = (nb / 2).next_power_of_two().max(64);
        s.gp.clear(); s.gp.resize(gcap, Gp { key: 0, mask: 0, dx: 0, dy: 0, n: 0, uni: true });
        let mut gm = gcap - 1; let mut gu = 0usize;
        let mut any_odd = false;
        #[inline(always)] fn gslot(gp: &[Gp], m: usize, k: u64) -> usize { let mut i = (k.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 40) as usize & m; loop { let x = gp[i].key; if x == k || x == 0 { return i; } i = (i + 1) & m; } }
        for (j, l) in u.batch.iter().enumerate() {
            let k = 1 + ((l.src_region as u64) << 54 | (l.src_entry as u64) << 36 | (s.slot[j] as u64) << 17 | l.xfer as u64);
            let (sx0, sy0) = geo[l.src_region as usize];
            let (sp, tp) = (l.src_pos as i32, l.pos as i32);
            let (dx, dy) = ((tx0 + tp % 8) - (sx0 + sp % 8), (ty0 + tp / 8) - (sy0 + sp / 8));
            let mut i = gslot(&s.gp, gm, k);
            if s.gp[i].key == 0 {
                if (gu + 1) * 10 > s.gp.len() * 7 {
                    let old: Vec<Gp> = s.gp.drain(..).filter(|g| g.key != 0).collect();
                    let nc = (gm + 1) * 2; gm = nc - 1;
                    s.gp.resize(nc, Gp { key: 0, mask: 0, dx: 0, dy: 0, n: 0, uni: true });
                    for g in old { let q = gslot(&s.gp, gm, g.key); s.gp[q] = g; }
                    i = gslot(&s.gp, gm, k);
                }
                s.gp[i] = Gp { key: k, mask: 0, dx: dx as i16, dy: dy as i16, n: 0, uni: true }; gu += 1;
            }
            let g = &mut s.gp[i];
            if g.uni && (g.dx as i32 != dx || g.dy as i32 != dy) { g.uni = false; any_odd = true; }
            g.mask |= 1 << sp; g.n += 1;
        }
        ph(3, &mut tph);
        // Target ranks by number, over the targets the groups touch.
        s.tsl.clear();
        for g in s.gp.iter() { if g.key != 0 { s.tsl.push(((g.key - 1) >> 17 & 0x7ffff) as u32); } }
        s.tsl.sort_unstable(); s.tsl.dedup();
        s.newk.clear(); for &t in &s.tsl { s.newk.push((s.tab[t as usize].num, t)); }
        s.newk.sort_unstable();
        if s.loc.len() < s.tab.len() { s.loc.resize(s.tab.len(), 0); }
        for (r, &(_, t)) in s.newk.iter().enumerate() { s.loc[t as usize] = r as u32; }
        // Sort the groups by (src region, src entry, target rank, transfer).
        s.gs.clear();
        for (i, g) in s.gp.iter().enumerate() { if g.key != 0 { let k = g.key - 1; s.gs.push(((k >> 36) << 35 | (s.loc[(k >> 17 & 0x7ffff) as usize] as u64) << 17 | (k & 0x1ffff), i as u32)); } }
        s.gs.sort_unstable_by_key(|x| x.0);
        // The rare non-uniform groups' (src cell, tgt cell) lists.
        s.odd.clear();
        if any_odd {
            for (j, l) in u.batch.iter().enumerate() {
                let k = 1 + ((l.src_region as u64) << 54 | (l.src_entry as u64) << 36 | (s.slot[j] as u64) << 17 | l.xfer as u64);
                let i = gslot(&s.gp, gm, k);
                if !s.gp[i].uni { s.odd.push((i as u32, l.src_pos, l.pos)); }
            }
            s.odd.sort_unstable();
        }
        ph(5, &mut tph);
        let o = &mut s.enc; o.clear();
        varint(o, u.region as u64); varint(o, nb as u64);
        let mut a = 0;
        while a < s.gs.len() {
            let sr = (s.gs[a].0 >> 53) as u32;
            let mut e = a; while e < s.gs.len() && (s.gs[e].0 >> 53) as u32 == sr { e += 1; }
            varint(o, sr as u64); varint(o, (e - a) as u64);
            let mut prev: Option<[u32; 3]> = None;
            for &(sk, gi) in &s.gs[a..e] {
                let g = s.gp[gi as usize];
                let f = [(sk >> 35 & 0x3ffff) as u32, s.newk[(sk >> 17 & 0x3ffff) as usize].0, (sk & 0x1ffff) as u32];
                match prev {
                    None => { o.push(255); for v in f { varint(o, v as u64); } }
                    Some(p) => { let q = (0..3).find(|&q| f[q] != p[q]).unwrap(); o.push(q as u8); varint(o, f[q].wrapping_sub(p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } }
                }
                prev = Some(f);
                varint(o, (g.n as u64) << 1 | g.uni as u64);
                if g.uni {
                    varint(o, ((g.dx as i64) << 1 ^ (g.dx as i64) >> 63) as u64); varint(o, ((g.dy as i64) << 1 ^ (g.dy as i64) >> 63) as u64);
                    if g.n >= 8 { o.extend_from_slice(&g.mask.to_le_bytes()); } else { let mut m = g.mask; while m != 0 { o.push(m.trailing_zeros() as u8); m &= m - 1; } }
                } else {
                    let lo = s.odd.partition_point(|x| x.0 < gi);
                    for x in &s.odd[lo..lo + g.n as usize] { o.push(x.1); o.push(x.2); }
                }
            }
            a = e;
        }
    } else if fmt == "relc" {
        // Group WITHOUT a comparison sort: dense per-unit ranks for the targets
        // (by number) and the sources (by (region, entry)), two stable counting
        // passes (by target, then by source), then per (source, target) run a
        // tiny scan over its transfers.
        let nb = u.batch.len();
        let (tx0, ty0) = geo[u.region as usize];
        // Targets touched: local ids by first touch, then ranked by number.
        s.loc.clear(); s.loc.resize(s.tab.len(), u32::MAX);
        s.tsl.clear(); s.tl.clear();
        for j in 0..nb { let t = s.slot[j] as usize; if s.loc[t] == u32::MAX { s.loc[t] = s.tsl.len() as u32; s.tsl.push(t as u32); } s.tl.push(s.loc[t]); }
        let nt = s.tsl.len();
        s.newk.clear(); for (li, &t) in s.tsl.iter().enumerate() { s.newk.push((s.tab[t as usize].num, li as u32)); }
        s.newk.sort_unstable();
        s.trank.clear(); s.trank.resize(nt, 0); for (r, &(_, li)) in s.newk.iter().enumerate() { s.trank[li as usize] = r as u32; }
        ph(3, &mut tph);
        // Sources: (region << 20 | entry) -> local id via a small open-addressing table, ranked by key.
        let scap = (nb / 4).next_power_of_two().max(64);
        s.stab.clear(); s.stab.resize(scap, (u32::MAX, 0));
        let mut smask = scap - 1;
        s.slist.clear(); s.sk.clear();
        let mut last = (u32::MAX, 0u32);
        for l in &u.batch {
            let k = (l.src_region as u32) << 20 | l.src_entry;
            if k == last.0 { s.sk.push(last.1); continue; }
            let mut i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & smask;
            loop {
                let e = s.stab[i];
                if e.0 == k { s.sk.push(e.1); last = (k, e.1); break; }
                if e.0 == u32::MAX {
                    if (s.slist.len() + 1) * 2 > s.stab.len() {
                        let nc = s.stab.len() * 2; smask = nc - 1;
                        s.stab.clear(); s.stab.resize(nc, (u32::MAX, 0));
                        for (li, &(kk, _)) in s.slist.iter().enumerate() { let mut q = (kk.wrapping_mul(0x9E37_79B1) >> 7) as usize & smask; while s.stab[q].0 != u32::MAX { q = (q + 1) & smask; } s.stab[q] = (kk, li as u32); }
                        i = (k.wrapping_mul(0x9E37_79B1) >> 7) as usize & smask; continue;
                    }
                    let li = s.slist.len() as u32; s.stab[i] = (k, li); s.slist.push((k, li)); s.sk.push(li); last = (k, li); break;
                }
                i = (i + 1) & smask;
            }
        }
        let ns = s.slist.len();
        s.slist.sort_unstable();
        s.srank.clear(); s.srank.resize(ns, 0); for (r, &(_, li)) in s.slist.iter().enumerate() { s.srank[li as usize] = r as u32; }
        ph(4, &mut tph);
        // Two stable counting passes over packed u64 edges
        // [src rank 17 | tgt rank 18 | transfer 17 | src cell 6 | tgt cell 6]: by target, then by source.
        assert!(ns < 1 << 17 && nt < 1 << 18);
        s.e1.clear(); s.e1.resize(nb, 0); s.e2.clear(); s.e2.resize(nb, 0);
        s.cnt.clear(); s.cnt.resize(nt + 1, 0);
        for j in 0..nb { s.tl[j] = s.trank[s.tl[j] as usize]; s.cnt[s.tl[j] as usize + 1] += 1; }
        for i in 1..=nt { s.cnt[i] += s.cnt[i - 1]; }
        for (j, l) in u.batch.iter().enumerate() {
            let tr = s.tl[j] as usize;
            let v = (s.srank[s.sk[j] as usize] as u64) << 47 | (tr as u64) << 29 | (l.xfer as u64) << 12 | (l.src_pos as u64) << 6 | l.pos as u64;
            s.e1[s.cnt[tr] as usize] = v; s.cnt[tr] += 1;
        }
        s.cnt.clear(); s.cnt.resize(ns + 1, 0);
        for &v in &s.e1 { s.cnt[(v >> 47) as usize + 1] += 1; }
        for i in 1..=ns { s.cnt[i] += s.cnt[i - 1]; }
        for &v in &s.e1 { let r = (v >> 47) as usize; s.e2[s.cnt[r] as usize] = v; s.cnt[r] += 1; }
        ph(5, &mut tph);
        // Emit: per source region block; per (source, target) run sorted in place by (transfer, cells).
                let o = &mut s.enc; o.clear();
        varint(o, u.region as u64); varint(o, nb as u64);
        let src_key = |r: u64| s.slist[r as usize].0; // region << 20 | entry, in rank order
        let mut a = 0;
        while a < nb {
            let sr = src_key(s.e2[a] >> 47) >> 20;
            let mut e = a; while e < nb && src_key(s.e2[e] >> 47) >> 20 == sr { e += 1; }
            let (sx0, sy0) = geo[sr as usize];
            // sort each (source, target) run by the low bits, count the groups
            let mut ng = 0u64;
            let mut c = a;
            while c < e {
                let hi = s.e2[c] >> 29;
                let mut d = c; while d < e && s.e2[d] >> 29 == hi { d += 1; }
                let run = &mut s.e2[c..d];
                if run.len() > 1 { if run.len() <= 16 { for i in 1..run.len() { let mut k = i; while k > 0 && run[k - 1] > run[k] { run.swap(k - 1, k); k -= 1; } } } else { run.sort_unstable(); } }
                let mut px = u64::MAX; for &v in run.iter() { let x = v >> 12 & 0x1ffff; if x != px { ng += 1; px = x; } }
                c = d;
            }
            varint(o, sr as u64); varint(o, ng);
            let mut prev: Option<[u32; 3]> = None;
            let mut c = a;
            while c < e {
                let v0 = s.e2[c];
                let x = (v0 >> 12 & 0x1ffff) as u32;
                let mut d = c; while d < e && s.e2[d] >> 12 == v0 >> 12 { d += 1; }
                let se = src_key(v0 >> 47) & 0xfffff;
                let tnum = s.tab[s.tsl[s.newk[(v0 >> 29 & 0x3ffff) as usize].1 as usize] as usize].num;
                let f = [se, tnum, x];
                match prev {
                    None => { o.push(255); for v in f { varint(o, v as u64); } }
                    Some(p) => { let q = (0..3).find(|&q| f[q] != p[q]).unwrap(); o.push(q as u8); varint(o, f[q].wrapping_sub(p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } }
                }
                prev = Some(f);
                let shift = |v: u64| { let (sp, tp) = ((v >> 6 & 63) as i32, (v & 63) as i32); ((tx0 + tp % 8) - (sx0 + sp % 8), (ty0 + tp / 8) - (sy0 + sp / 8)) };
                let run = &s.e2[c..d];
                let sh = shift(run[0]);
                let uni = run.iter().all(|&v| shift(v) == sh);
                let n = run.len();
                varint(o, (n as u64) << 1 | uni as u64);
                if uni {
                    varint(o, ((sh.0 as i64) << 1 ^ (sh.0 as i64) >> 63) as u64); varint(o, ((sh.1 as i64) << 1 ^ (sh.1 as i64) >> 63) as u64);
                    if n >= 8 { let mut m = 0u64; for &v in run { m |= 1 << (v >> 6 & 63); } o.extend_from_slice(&m.to_le_bytes()); }
                    else { for &v in run { o.push((v >> 6 & 63) as u8); } }
                } else { for &v in run { o.push((v >> 6 & 63) as u8); o.push((v & 63) as u8); } }
                c = d;
            }
            a = e;
        }
    } else if fmt == "relh" {
        // Group by HASH (no sort of the edges): (src region, src entry, tgt entry, transfer) -> shift, source-cell mask.
        let (tx0, ty0) = geo[u.region as usize];
        let gcap = (u.batch.len() / 2).next_power_of_two().max(64);
        s.gt.clear(); s.gt.resize(gcap, G { gk: u128::MAX, mask: 0, n: 0, dx: 0, dy: 0, uni: true });
        let mut gmask = gcap - 1;
        let mut gused = 0usize;
        let hk = |k: u128| (k as u64 ^ (k >> 64) as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 20;
        let gslot = |gt: &[G], m: usize, k: u128| { let mut i = hk(k) as usize & m; loop { let x = gt[i].gk; if x == k || x == u128::MAX { return i; } i = (i + 1) & m; } };
        let mut any_odd = false;
        for (j, l) in u.batch.iter().enumerate() {
            let tnum = s.tab[s.slot[j] as usize].num as u128;
            let gk = (l.src_region as u128) << 96 | (l.src_entry as u128) << 64 | tnum << 32 | l.xfer as u128;
            let (sx0, sy0) = geo[l.src_region as usize];
            let (sp, tp) = (l.src_pos as i32, l.pos as i32);
            let (dx, dy) = ((tx0 + tp % 8) - (sx0 + sp % 8), (ty0 + tp / 8) - (sy0 + sp / 8));
            let mut i = gslot(&s.gt, gmask, gk);
            if s.gt[i].gk == u128::MAX {
                if (gused + 1) * 10 > s.gt.len() * 7 {
                    let old: Vec<G> = s.gt.drain(..).filter(|g| g.gk != u128::MAX).collect();
                    let nc = (gmask + 1) * 2; gmask = nc - 1;
                    s.gt.resize(nc, G { gk: u128::MAX, mask: 0, n: 0, dx: 0, dy: 0, uni: true });
                    for g in old { let q = gslot(&s.gt, gmask, g.gk); s.gt[q] = g; }
                    i = gslot(&s.gt, gmask, gk);
                }
                s.gt[i] = G { gk, mask: 0, n: 0, dx: dx as i16, dy: dy as i16, uni: true }; gused += 1;
            }
            let g = &mut s.gt[i];
            if (g.dx as i32, g.dy as i32) != (dx, dy) && g.uni { g.uni = false; any_odd = true; }
            g.mask |= 1 << sp; g.n += 1;
        }
        // The rare non-uniform groups: their (src cell, tgt cell) lists.
        s.odd.clear();
        if any_odd {
            for (j, l) in u.batch.iter().enumerate() {
                let tnum = s.tab[s.slot[j] as usize].num as u128;
                let gk = (l.src_region as u128) << 96 | (l.src_entry as u128) << 64 | tnum << 32 | l.xfer as u128;
                let i = gslot(&s.gt, gmask, gk);
                if !s.gt[i].uni { s.odd.push((i as u32, l.src_pos, l.pos)); }
            }
            s.odd.sort_unstable();
        }
        s.gord.clear();
        for (i, g) in s.gt.iter().enumerate() { if g.gk != u128::MAX { s.gord.push((g.gk, i as u32)); } }
        s.gord.sort_unstable_by_key(|x| x.0);
        let o = &mut s.enc; o.clear();
        varint(o, u.region as u64); varint(o, u.batch.len() as u64);
        let mut a = 0;
        while a < s.gord.len() {
            let sr = (s.gord[a].0 >> 96) as u32;
            let mut e = a; while e < s.gord.len() && (s.gord[e].0 >> 96) as u32 == sr { e += 1; }
            varint(o, sr as u64); varint(o, (e - a) as u64);
            let mut prev: Option<[u32; 3]> = None;
            for &(gk, gi) in &s.gord[a..e] {
                let f = [(gk >> 64) as u32, (gk >> 32) as u32, gk as u32];
                match prev {
                    None => { o.push(255); for v in f { varint(o, v as u64); } }
                    Some(p) => { let q = (0..3).find(|&q| f[q] != p[q]).unwrap(); o.push(q as u8); varint(o, (f[q] - p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } }
                }
                prev = Some(f);
                let g = s.gt[gi as usize];
                varint(o, (g.n as u64) << 1 | g.uni as u64);
                if g.uni {
                    varint(o, ((g.dx as i64) << 1 ^ (g.dx as i64) >> 63) as u64); varint(o, ((g.dy as i64) << 1 ^ (g.dy as i64) >> 63) as u64);
                    if g.n >= 8 { o.extend_from_slice(&g.mask.to_le_bytes()); } else { for p in 0..64 { if g.mask >> p & 1 == 1 { o.push(p as u8); } } }
                } else {
                    let lo = s.odd.partition_point(|x| x.0 < gi);
                    for x in &s.odd[lo..lo + g.n as usize] { o.push(x.1); o.push(x.2); }
                }
            }
            a = e;
        }
    } else if fmt != "none" {
        s.tup.clear();
        let (tx0, ty0) = geo[u.region as usize];
        for (j, l) in u.batch.iter().enumerate() {
            let tnum = s.tab[s.slot[j] as usize].num as u128;
            s.tup.push(if fmt == "rel" {
                (l.src_region as u128) << 118 | (l.src_entry as u128) << 86 | tnum << 54 | (l.xfer as u128) << 22 | (l.src_pos as u128) << 6 | l.pos as u128
            } else {
                tnum << 96 | (l.pos as u128) << 88 | (l.src_region as u128) << 72 | (l.src_entry as u128) << 40 | (l.src_pos as u128) << 32 | l.xfer as u128
            });
        }
        s.tup.sort_unstable();
        let o = &mut s.enc; o.clear();
        varint(o, u.region as u64); varint(o, s.tup.len() as u64);
        if fmt == "rel" {
            let mut a = 0;
            while a < s.tup.len() {
                let sr = (s.tup[a] >> 118) as u32;
                let mut e = a; while e < s.tup.len() && (s.tup[e] >> 118) as u32 == sr { e += 1; }
                let (sx0, sy0) = geo[sr as usize];
                varint(o, sr as u64);
                // groups (src entry, tgt entry, xfer)
                let gkey = |x: u128| (x >> 22) & ((1u128 << 96) - 1);
                let mut ng = 0u64; let mut c = a; while c < e { let g = gkey(s.tup[c]); while c < e && gkey(s.tup[c]) == g { c += 1; } ng += 1; }
                varint(o, ng);
                let mut prev: Option<[u32; 3]> = None;
                let mut c = a;
                while c < e {
                    let g = gkey(s.tup[c]);
                    let mut d = c; while d < e && gkey(s.tup[d]) == g { d += 1; }
                    let f = [(s.tup[c] >> 86) as u32, (s.tup[c] >> 54) as u32, (s.tup[c] >> 22) as u32];
                    match prev {
                        None => { o.push(255); for v in f { varint(o, v as u64); } }
                        Some(p) => { let q = (0..3).find(|&q| f[q] != p[q]).unwrap(); o.push(q as u8); varint(o, (f[q] - p[q]) as u64); for v in &f[q + 1..] { varint(o, *v as u64); } }
                    }
                    prev = Some(f);
                    let cell = |x: u128| { let (sp, tp) = ((x >> 6) as i32 & 63, x as i32 & 63); (sx0 + sp % 8, sy0 + sp / 8, tx0 + tp % 8, ty0 + tp / 8, sp) };
                    let c0 = cell(s.tup[c]);
                    let sh = (c0.2 - c0.0, c0.3 - c0.1);
                    let uni = s.tup[c..d].iter().all(|&x| { let q = cell(x); (q.2 - q.0, q.3 - q.1) == sh });
                    varint(o, ((d - c) as u64) << 1 | uni as u64);
                    if uni {
                        varint(o, ((sh.0 as i64) << 1 ^ (sh.0 as i64) >> 63) as u64); varint(o, ((sh.1 as i64) << 1 ^ (sh.1 as i64) >> 63) as u64);
                        if d - c >= 8 { let mut m = 0u64; for &x in &s.tup[c..d] { m |= 1 << cell(x).4; } o.extend_from_slice(&m.to_le_bytes()); }
                        else { for &x in &s.tup[c..d] { o.push(cell(x).4 as u8); } }
                    } else { for &x in &s.tup[c..d] { o.push((x >> 6) as u8 & 63); o.push(x as u8 & 63); } }
                    c = d;
                }
                a = e;
            }
        } else {
            // Per edge: varint(tgt number delta; 0 = same target entry), tgt cell, varint(src id delta within the entry), varint(transfer).
            let mut p: (u32, u64) = (u32::MAX, 0);
            for &x in &s.tup {
                let tn = (x >> 96) as u32; let tp = (x >> 88) as u8; let src = (x >> 32) as u64 & ((1 << 56) - 1); let xf = x as u32;
                varint(o, tn.wrapping_sub(p.0) as u64);
                if tn != p.0 { p = (tn, 0); }
                o.push(tp); varint(o, src.wrapping_sub(p.1)); p.1 = src; varint(o, xf as u64);
            }
        }
    }
    if fmt != "none" {
        let o = &s.enc;
        let len = o.len();
        assert!(s.used + len <= s.arena.len(), "edge arena too small");
        s.arena[s.used..s.used + len].copy_from_slice(o);
        s.used += len;
    }
    ph(6, &mut tph);
    // Re-encode the unit's entries (key order) - the at-rest format.
    s.newk.clear();
    for (t, h) in s.tab.iter().enumerate() { if h.key != u32::MAX { s.newk.push((h.key, t as u32)); } }
    s.newk.sort_unstable();
    s.enc.clear(); varint(&mut s.enc, s.newk.len() as u64);
    let mut pk = 0u32;
    for &(k, t) in &s.newk { let h = &s.tab[t as usize]; varint(&mut s.enc, (k - pk) as u64); varint(&mut s.enc, h.num as u64); s.enc.extend_from_slice(&(h.m | h.nm).to_le_bytes()); pk = k; }
    std::hint::black_box(s.enc.len());
    ph(7, &mut tph);
    Out { new_keys: out_new_keys, thread: ti, off, len: s.used - off, new }
}

fn pin(cpu: usize) { unsafe { let mut set: libc::cpu_set_t = std::mem::zeroed(); libc::CPU_SET(cpu, &mut set); libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set); } }

fn mixf(t: &[u64; 7]) -> u64 { t.iter().fold(0x1234_5678u64, |a, &x| crate::bits::mix64(a ^ x).wrapping_add(x)) }

pub fn regionedge(fields_dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap], args: &[String]) {
    let fmt = args[0].as_str();
    let t0 = Instant::now();
    let f = Fields::open(fields_dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<_> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    // Key order flags+dash > spd.x > spd.y, as regionpar.
    let roles: Vec<Vec<(u64, u8)>> = packs.iter().map(|p| p.high.iter().map(|&d| (p.digits[d].radix, if p.digits[d].name == "spd.x" { 1 } else if p.digits[d].table.is_some() { 2 } else { 0 })).collect()).collect();
    let low_r: Vec<u64> = packs.iter().map(|p| p.low.map_or(1, |d| p.digits[d].radix)).collect();
    let n = f.n;
    let (mut rshape, mut rcell, mut rkey, mut rreg, mut rpos, mut rold) = (vec![0u8; n], vec![0u32; n], vec![0u32; n], vec![0u32; n], vec![0u8; n], vec![false; n]);
    let mut rid: FxHashMap<(u8, i32, i32), u32> = FxHashMap::default();
    let mut geo: Vec<(i32, i32)> = Vec::new();
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
        let r = *rid.entry((s as u8, x.div_euclid(8), y.div_euclid(8))).or_insert_with(|| { geo.push((x.div_euclid(8) * 8, y.div_euclid(8) * 8)); nr });
        rshape[i] = s as u8; rcell[i] = c; rkey[i] = u32::try_from(k * low_r[s] + l as u64).unwrap(); rreg[i] = r;
        rpos[i] = (x.rem_euclid(8) + 8 * y.rem_euclid(8)) as u8; rold[i] = fr <= 56;
        row_of.insert(f.key(i), i as u32);
    }
    let ng = rid.len();
    assert!(ng < 1 << 10);
    // Old entries per region, numbered by key.
    let mut old_ents: Vec<Vec<(u32, u64)>> = vec![Vec::new(); ng];
    {
        let mut m: Vec<FxHashMap<u32, u64>> = vec![Default::default(); ng];
        for i in 0..n { if rold[i] { *m[rreg[i] as usize].entry(rkey[i]).or_insert(0) |= 1 << rpos[i]; } }
        for (r, e) in m.into_iter().enumerate() { let mut v: Vec<(u32, u64)> = e.into_iter().collect(); v.sort_unstable(); old_ents[r] = v; }
    }
    let old_num = |r: u32, k: u32| old_ents[r as usize].binary_search_by_key(&k, |e| e.0).ok().map(|x| x as u32);
    // Sources: door id -> row. Transfers: canonical by content.
    let mut src_row: FxHashMap<u64, u32> = FxHashMap::default();
    for i in 0..door.len() / 40 {
        let b = &door[i * 40..];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        src_row.insert(u64::from_le_bytes(b[32..40].try_into().unwrap()), row_of[&k]);
    }
    let mut pairs: Vec<[u8; 26]> = Vec::new();
    let mut intern: FxHashMap<[u8; 26], u32> = FxHashMap::default();
    // Batches per target region, in capture (kernel / source) order; the capture's edge fingerprint.
    let mut batches: Vec<Vec<L>> = vec![Vec::new(); ng];
    let (mut fp_cap, mut n_edges) = (0u64, 0u64);
    let tuple = |s: u32, t: u32, x: u32| -> [u64; 7] { [rshape[s as usize] as u64, rcell[s as usize] as u64, rkey[s as usize] as u64, rshape[t as usize] as u64, rcell[t as usize] as u64, rkey[t as usize] as u64, x as u64] };
    let exhaustive = std::env::var_os("EDGE_EXHAUSTIVE").is_some();
    let mut cap_list: Vec<[u64; 7]> = Vec::new();
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
            batches[rreg[t as usize] as usize].push(L { key: rkey[t as usize], src_entry: se, xfer: x, src_region: sr as u16, pos: rpos[t as usize], src_pos: rpos[s as usize] });
            let tu = tuple(s, t, x);
            fp_cap = fp_cap.wrapping_add(mixf(&tu)); n_edges += 1;
            if exhaustive { cap_list.push(tu); }
        }
    }
    drop(src_row);
    let t_setup = t0.elapsed().as_secs_f64();
    println!("regionedge {fmt}: {n} states, {ng} regions, {n_edges} edges, {} transfers; capture edge fingerprint {fp_cap:016x}; setup (untimed) {t_setup:.1} s", pairs.len());
    let mut built: Option<(usize, Vec<Unit>)> = None;
    for run in &args[1..] {
        let v: Vec<&str> = run.split(':').collect();
        let (threads, split): (usize, usize) = (v[0].parse().unwrap(), v[1].parse().unwrap());
        let tu = Instant::now();
        if built.as_ref().is_some_and(|b| b.0 == split) { } else {
        let mut units: Vec<Unit> = Vec::new();
        for r in 0..ng {
            let parts = if split == 0 { 1 } else { batches[r].len().div_ceil(split).max(1) as u32 };
            for p in 0..parts {
                let mut rest = Vec::new();
                let sel: Vec<(u32, (u32, u64))> = old_ents[r].iter().enumerate().filter(|(_, e)| parts == 1 || hpart(e.0, parts) == p).map(|(i, e)| (i as u32, *e)).collect();
                varint(&mut rest, sel.len() as u64);
                let (mut pk, mut pn) = (0u32, 0u32);
                for (num, (k, m)) in sel { varint(&mut rest, (k - pk) as u64); varint(&mut rest, (num - pn) as u64); rest.extend_from_slice(&m.to_le_bytes()); pk = k; pn = num; }
                let batch: Vec<L> = batches[r].iter().filter(|l| parts == 1 || hpart(l.key, parts) == p).copied().collect();
                units.push(Unit { region: r as u32, part: p, rest, batch });
            }
        }
        units.sort_by_key(|u| std::cmp::Reverse(u.batch.len()));
        built = Some((split, units));
        }
        let units = &built.as_ref().unwrap().1;
        let t_units = tu.elapsed().as_secs_f64();
        // Scratch: the largest unit's needs, prefaulted; arenas sized for the edges (~5 B an edge, x2, split over threads).
        let tp = Instant::now();
        let max_b = units.iter().map(|u| u.batch.len()).max().unwrap();
        let max_cap = units.iter().map(|u| { let mut i = 0; let n = get_varint(&u.rest, &mut i) as usize; ((n + u.batch.len().min(4 * n + 64)) * 2).next_power_of_two() }).max().unwrap();
        let arena = if fmt == "none" { 0 } else { (n_edges as usize * 12 / threads).max(256 << 20) + 16 * max_b };
        let mut scr: Vec<Scratch> = (0..threads).map(|_| {
            let gcap = (max_b / 2).next_power_of_two().max(64) * 2;
            let mut s = Scratch { ac: vec![Ac { hi: 0, mask: 0, lo: 0, gen: 0 }; (max_b / 2).next_power_of_two().max(64)], acg: 0, acl: vec![(0, 0); max_b], ep: 0, loc2: vec![0; max_cap], gp: vec![Gp { key: 0, mask: 0, dx: 0, dy: 0, n: 0, uni: true }; (max_b / 2).next_power_of_two().max(64) * 2], gs: vec![(0, 0); max_b], e1: vec![0; max_b], e2: vec![0; max_b], loc: vec![0; max_cap], tl: vec![0; max_b], sk: vec![0; max_b], cnt: vec![0; max_b + 1], tsl: vec![0; max_cap], trank: vec![0; max_cap], stab: vec![(0, 0); (2 * max_b.min(1 << 16)).next_power_of_two().max(64) * 4], slist: vec![(0, 0); max_b], srank: vec![0; max_b], tab: Vec::with_capacity(max_cap), slot: Vec::with_capacity(max_b), tup: Vec::with_capacity(if fmt == "relh" { 0 } else { max_b }), newk: Vec::with_capacity(max_cap), arena: vec![0u8; arena], used: 0, enc: Vec::with_capacity(8 * max_b + 1024), gt: Vec::with_capacity(if fmt == "relh" { gcap } else { 0 }), gord: Vec::with_capacity(if fmt == "relh" { max_b } else { 0 }), odd: Vec::new() };
            s.tab.resize(max_cap, H { key: 0, num: 0, m: 0, nm: 0 }); s.slot.resize(max_b, 0); if fmt == "relh" { s.gt.resize(gcap, G { gk: 0, mask: 0, n: 0, dx: 0, dy: 0, uni: true }); s.gord.resize(max_b, (0, 0)); } else { s.tup.resize(max_b, 0); } s.newk.resize(max_cap, (0, 0)); s.enc.resize(8 * max_b + 1024, 0);
            for b in s.arena.iter_mut().step_by(4096) { *b = 1; }
            s
        }).collect();
        let t_pre = tp.elapsed().as_secs_f64();
        let cpus: Vec<usize> = if threads == 1 { vec![4] } else { (0..threads).collect() };
        let next = AtomicUsize::new(0);
        let outs: Vec<std::sync::Mutex<Out>> = units.iter().map(|_| std::sync::Mutex::new(Out::default())).collect();
        crate::perf_on(true);
        let tw = Instant::now();
        let busy: Vec<f64> = std::thread::scope(|sc| {
            let hs: Vec<_> = scr.iter_mut().enumerate().map(|(ti, s)| {
                let (units, next, outs, geo, cpu) = (units, &next, &outs, &geo, cpus[ti]);
                sc.spawn(move || {
                    pin(cpu);
                    let mut busy = 0f64;
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= units.len() { break; }
                        let t = Instant::now();
                        let o = run_unit(&units[i], fmt, geo, s, ti);
                        busy += t.elapsed().as_secs_f64();
                        *outs[i].lock().unwrap() = o;
                    }
                    busy
                })
            }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let wall = tw.elapsed().as_secs_f64();
        crate::perf_on(false);
        let outs: Vec<Out> = outs.into_iter().map(|m| m.into_inner().unwrap()).collect();
        let new: u64 = outs.iter().map(|o| o.new).sum();
        let bytes: usize = outs.iter().map(|o| o.len).sum();
        let sum_busy: f64 = busy.iter().sum();
        println!("  RUN {fmt} threads {threads:>2} split {split:>8}: {} units; WALL {wall:.3} s ({:.2} ns an edge); busy {sum_busy:.2} s, idle {:.1}%; new {new} ({}); edge bytes {:.3} B an edge; units built {t_units:.1} s, prefault {t_pre:.2} s",
            units.len(), wall * 1e9 / n_edges as f64, 100.0 * (1.0 - sum_busy / (wall * threads as f64)), if new == 6_735_699 { "OK" } else { "MISMATCH" }, bytes as f64 / n_edges as f64);
        if threads == 1 { println!("    phases (1 thread): {}", (0..8).map(|i| format!("{} {:.2} s", PH_NAMES[i], PH[i].swap(0, Ordering::Relaxed) as f64 / 1e9)).collect::<Vec<_>>().join(", ")); } else { for p in &PH { p.store(0, Ordering::Relaxed); } }
        if fmt == "none" || std::env::var_os("EDGE_NOCHECK").is_some() { continue; }
        // UNTIMED: decode every unit's edges back to (src shape, cell, key, tgt shape, cell, key, transfer); compare the set.
        let tv = Instant::now();
        let new_keys: FxHashMap<(u32, u32), &Vec<u32>> = units.iter().zip(&outs).map(|(u, o)| ((u.region, u.part), &o.new_keys)).collect();
        let shape_of: Vec<u8> = { let mut s = vec![0u8; ng]; for (&(sh, _, _), &r) in &rid { s[r as usize] = sh; } s };
        let key_of = |r: u32, num: u32| -> u32 { if num & NEW == 0 { old_ents[r as usize][num as usize].0 } else { new_keys[&(r, (num >> 20) & 0x3ff)][(num & 0xfffff) as usize] } };
        let cell_of = |x: i32, y: i32| ((y + 64) * 512 + (x + 64)) as u64;
        let (mut fp, mut cnt) = (0u64, 0u64);
        let mut dec_list: Vec<[u64; 7]> = Vec::new();
        let zst = std::env::var_os("EDGE_ZSTD").is_some();
        let mut zbytes = 0u64;
        for o in &outs {
            if o.len == 0 { continue; }
            let b = &scr[o.thread].arena[o.off..o.off + o.len];
            if zst { zbytes += zstd::bulk::compress(b, 1).unwrap().len() as u64; }
            let mut i = 0;
            let tr = get_varint(b, &mut i) as u32; let total = get_varint(b, &mut i);
            let (tx0, ty0) = geo[tr as usize];
            let mut seen = 0u64;
            let mut emit = |t: [u64; 7]| { fp = fp.wrapping_add(mixf(&t)); cnt += 1; if exhaustive { dec_list.push(t); } };
            if fmt == "relt" || fmt == "relf" {
                let mut ptn = u32::MAX;
                while seen < total {
                    let tn = ptn.wrapping_add(get_varint(b, &mut i) as u32); ptn = tn;
                    let tk = key_of(tr, tn);
                    let ngr = get_varint(b, &mut i);
                    let mut f = [0u32; 3];
                    for _ in 0..ngr {
                        let q = b[i]; i += 1;
                        if q == 255 { for v in f.iter_mut() { *v = get_varint(b, &mut i) as u32; } }
                        else if q < 3 { let q = q as usize; f[q] = f[q].wrapping_add(get_varint(b, &mut i) as u32); for v in f.iter_mut().skip(q + 1) { *v = get_varint(b, &mut i) as u32; } }
                        let sr = f[0]; let (sx0, sy0) = geo[sr as usize];
                        let sk = key_of(sr, f[1]);
                        let hd = get_varint(b, &mut i); let (cntm, uni) = (hd >> 1, hd & 1 == 1);
                        let mut push = |sp: i32, tx: i32, ty: i32| { let (sx, sy) = (sx0 + sp % 8, sy0 + sp / 8); emit([shape_of[sr as usize] as u64, cell_of(sx, sy), sk as u64, shape_of[tr as usize] as u64, cell_of(tx, ty), tk as u64, f[2] as u64]); };
                        if uni {
                            let zz = |v: u64| ((v >> 1) as i64 ^ -((v & 1) as i64)) as i32;
                            let (dx, dy) = (zz(get_varint(b, &mut i)), zz(get_varint(b, &mut i)));
                            let sps: Vec<i32> = if cntm >= 8 { let m = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8; (0..64).filter(|p| m >> p & 1 == 1).collect() } else { let v = b[i..i + cntm as usize].iter().map(|&p| p as i32).collect(); i += cntm as usize; v };
                            assert_eq!(sps.len() as u64, cntm);
                            for sp in sps { push(sp, sx0 + sp % 8 + dx, sy0 + sp / 8 + dy); }
                        } else { for _ in 0..cntm { let (sp, tp) = (b[i] as i32, b[i + 1] as i32); i += 2; push(sp, tx0 + tp % 8, ty0 + tp / 8); } }
                        seen += cntm;
                    }
                }
            } else if fmt == "rel" || fmt == "relh" || fmt == "relc" || fmt == "relg" {
                while seen < total {
                    let sr = get_varint(b, &mut i) as u32; let ngr = get_varint(b, &mut i);
                    let (sx0, sy0) = geo[sr as usize];
                    let mut f = [0u32; 3];
                    for _ in 0..ngr {
                        let q = b[i]; i += 1;
                        if q == 255 { for v in f.iter_mut() { *v = get_varint(b, &mut i) as u32; } }
                        else { let q = q as usize; f[q] = f[q].wrapping_add(get_varint(b, &mut i) as u32); for v in f.iter_mut().skip(q + 1) { *v = get_varint(b, &mut i) as u32; } }
                        let (sk, tk) = (key_of(sr, f[0]), key_of(tr, f[1]));
                        let hd = get_varint(b, &mut i); let (cntm, uni) = (hd >> 1, hd & 1 == 1);
                        let mut push = |sp: i32, tx: i32, ty: i32| { let (sx, sy) = (sx0 + sp % 8, sy0 + sp / 8); emit([shape_of[sr as usize] as u64, cell_of(sx, sy), sk as u64, shape_of[tr as usize] as u64, cell_of(tx, ty), tk as u64, f[2] as u64]); };
                        if uni {
                            let zz = |v: u64| ((v >> 1) as i64 ^ -((v & 1) as i64)) as i32;
                            let (dx, dy) = (zz(get_varint(b, &mut i)), zz(get_varint(b, &mut i)));
                            let sps: Vec<i32> = if cntm >= 8 { let m = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8; (0..64).filter(|p| m >> p & 1 == 1).collect() } else { let v = b[i..i + cntm as usize].iter().map(|&p| p as i32).collect(); i += cntm as usize; v };
                            assert_eq!(sps.len() as u64, cntm);
                            for sp in sps { push(sp, sx0 + sp % 8 + dx, sy0 + sp / 8 + dy); }
                        } else { for _ in 0..cntm { let (sp, tp) = (b[i] as i32, b[i + 1] as i32); i += 2; push(sp, tx0 + tp % 8, ty0 + tp / 8); } }
                        seen += cntm;
                    }
                }
            } else {
                let mut p: (u32, u64) = (u32::MAX, 0);
                for _ in 0..total {
                    let d = get_varint(b, &mut i) as u32; if d != 0 { p = (p.0.wrapping_add(d), 0); }
                    let tp = b[i] as i32; i += 1;
                    let src = p.1.wrapping_add(get_varint(b, &mut i)); p.1 = src;
                    let xf = get_varint(b, &mut i);
                    let (sr, se, sp) = ((src >> 40) as u32, (src >> 8) as u32, src as i32 & 0xff);
                    let (sx0, sy0) = geo[sr as usize];
                    emit([shape_of[sr as usize] as u64, cell_of(sx0 + sp % 8, sy0 + sp / 8), key_of(sr, se) as u64, shape_of[tr as usize] as u64, cell_of(tx0 + tp % 8, ty0 + tp / 8), key_of(tr, p.0) as u64, xf]);
                }
            }
            assert_eq!(i, b.len(), "a unit's edge block did not decode to its end");
        }
        print!("    CHECK edges: {cnt} decoded (want {n_edges}), fingerprint {fp:016x} ({}){}", if fp == fp_cap && cnt == n_edges { "EQUAL to the capture's" } else { "MISMATCH" },
            if zst { format!("; zstd -1 per unit block (untimed) {:.3} B an edge", zbytes as f64 / n_edges as f64) } else { String::new() });
        if exhaustive {
            let mut a = cap_list.clone(); a.sort_unstable(); dec_list.sort_unstable();
            let dup = a.windows(2).filter(|w| w[0] == w[1]).count();
            print!("; EXHAUSTIVE: sets {} (capture {} distinct)", if a == dec_list { "IDENTICAL" } else { "DIFFER" }, a.len() - dup);
        }
        // Stable-id bijection over every state: (region, number, pos) <-> (shape, cell, key).
        let mut ids: FxHashSet<(u32, u32, u8)> = FxHashSet::default(); ids.reserve(n);
        let mut newnum: FxHashMap<(u32, u32), u32> = FxHashMap::default();
        for u in units.iter() { for (r, &k) in new_keys[&(u.region, u.part)].iter().enumerate() { newnum.insert((u.region, k), NEW | u.part << 20 | r as u32); } }
        let mut bad = 0u64;
        for i in 0..n {
            let r = rreg[i];
            let num = match old_num(r, rkey[i]) { Some(x) => x, None => *newnum.get(&(r, rkey[i])).unwrap_or(&u32::MAX) };
            if num == u32::MAX || key_of(r, num) != rkey[i] || shape_of[r as usize] != rshape[i] { bad += 1; continue; }
            let (gx, gy) = geo[r as usize]; let p = rpos[i] as i32;
            if cell_of(gx + p % 8, gy + p / 8) != rcell[i] as u64 { bad += 1; }
            if !ids.insert((r, num, rpos[i])) { bad += 1; }
        }
        println!("; id bijection over {n} states: {} distinct ids, {bad} violations; check {:.1} s", ids.len(), tv.elapsed().as_secs_f64());
    }
}
