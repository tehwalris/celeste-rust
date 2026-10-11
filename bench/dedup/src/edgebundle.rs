//! `edgebundle` (DESIGNS O): EDGES STORED THE WAY STATES ARE. Per region a
//! table keyed by (home entry, the other side's position-free key, transfer,
//! offset dx/dy), its value a u64 cell mask; an edge is inserted by one probe
//! and one OR, exactly like a visited-set state - a duplicate is free, nothing
//! is sorted until the block is encoded.
//!   * S: in the SOURCE region; home = the source entry, other = the target's
//!     (shape, key); the mask over SOURCE cells. Its own pass over the
//!     emissions grouped by source region (the forward's units are source
//!     runs).
//!   * T: in the TARGET region; home = the target entry (found by the dedup
//!     in the same probe sequence), other = the source's (shape, key); the
//!     mask over TARGET cells - inverted already, for a backward that follows
//!     in-edges. Inserted in the dedup pass itself (J2's units: target
//!     regions, heavy ones split by a hash of the key).
//!
//! DATA. The capture `/var/tmp/emitcap2` of DESIGNS K-N needs the field dump
//! of its tree (`Fields`), which is gone with `/var/tmp/emitcap`; the same
//! frame was captured again on storage v2 with EXACT position-free keys:
//! `/var/tmp/ia-fixtures/cap-r62h57/f057` (room (6,2) 100% f56 -> f57,
//! `--emit-capture`, src/storage/bench.rs of the main crate), and its tree
//! `/var/tmp/ia-fixtures/r62h57` (format 14) gives the frame-start visited set
//! (frame files' id columns, `meta.bin` entries and shapes). A source is its
//! id (region, entry, cell); a target is (shape, cell, exact key), a new
//! state's provisional key interned by its MISS content; transfers are
//! interned by content over the workers' tables. Validation: the decoded
//! edge sets' order-free fingerprints equal the capture's (computed in the
//! setup over (source id, target shape, cell, key, transfer)), 257,724,013
//! edges, 6,735,699 new states (the tree's own `e057`/`f057` counts).
//!
//! Args: CAP TREE then runs `mode:T:split` (mode `dedup` | `t` | `s`), and
//! `access:T` (T's backward access, after a `t` run), e.g.
//! `edgebundle CAP TREE dedup:1:0 t:1:0 s:1:0 dedup:16:1000000 ...`.
//! The (untimed) decode check runs after the first run of each mode (every
//! run with env `EB_CHECK_ALL`). `edgebundle CAP TREE idfree ...`: the
//! ID-free variants (`edgekeys`).

use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
use std::time::Instant;

/// Regions per shape: 8x8 regions over the 512-cell grid, `NO_CELL`'s last.
pub(crate) const SLOTS: u32 = 64 * 64 + 1;
const NO_CELL: u32 = 512 * 512;
/// A provisional (new, MISS-content-interned) key: below `1 << 56` with this bit.
const PROV: u64 = 1 << 55;
pub(crate) const EMPTY: u64 = u64::MAX;

#[inline] fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }
#[inline] fn get_varint(b: &[u8], i: &mut usize) -> u64 { let mut v = 0u64; let mut s = 0; loop { let x = b[*i]; *i += 1; v |= ((x & 0x7f) as u64) << s; if x < 0x80 { return v; } s += 7; } }
fn mix(t: &[u64]) -> u64 { t.iter().fold(0x1234_5678u64, |a, &x| crate::bits::mix64(a ^ x).wrapping_add(x)) }
pub(crate) fn hpart(k: u64, parts: u32) -> u32 { (crate::bits::mix64(k) >> 32) as u32 % parts }

pub(crate) fn slot_local(cell: u32) -> (u32, u32) {
    if cell == NO_CELL { return (SLOTS - 1, 0); }
    let (gx, gy) = (cell % 512, cell / 512);
    ((gy / 8) * 64 + gx / 8, (gy % 8) * 8 + gx % 8)
}
pub(crate) fn cell_of(slot: u32, local: u32) -> u32 {
    if slot == SLOTS - 1 { return NO_CELL; }
    let (rx, ry) = (slot % 64, slot / 64);
    (ry * 8 + local / 8) * 512 + rx * 8 + local % 8
}
/// Target cell minus source cell as a code: `(dx + 1024) << 11 | (dy + 1024)`
/// between two cells (below `TO_NONE`); into `NO_CELL` `TO_NONE` (T: `| the
/// source cell`, the only way back to it), out of it `FROM_NONE` (S: `| the
/// target cell`), neither `NONE_NONE`. Each side's code names what its
/// mask's cell does not give.
const TO_NONE: u32 = 1 << 22;
const FROM_NONE: u32 = 1 << 23;
const NONE_NONE: u32 = 1 << 24;
pub(crate) fn offset(a: u32, b: u32, t_side: bool) -> u32 {
    match (a == NO_CELL, b == NO_CELL) {
        (false, false) => { let dx = (b % 512) as i32 - (a % 512) as i32; let dy = (b / 512) as i32 - (a / 512) as i32; ((dx + 1024) as u32) << 11 | (dy + 1024) as u32 }
        (false, true) => if t_side { TO_NONE | a } else { TO_NONE },
        (true, false) => if t_side { FROM_NONE } else { FROM_NONE | b },
        (true, true) => NONE_NONE,
    }
}
/// T: the source cell of an edge into `b` at offset code `off`.
pub(crate) fn back(b: u32, off: u32) -> u32 {
    if off < TO_NONE { let (dx, dy) = ((off >> 11) as i32 - 1024, (off & 2047) as i32 - 1024); ((b / 512) as i32 - dy) as u32 * 512 + ((b % 512) as i32 - dx) as u32 }
    else if off & !(TO_NONE - 1) == TO_NONE { off & (TO_NONE - 1) } else { NO_CELL }
}
/// S: the target cell of an edge out of `a` at offset code `off`.
pub(crate) fn fwd(a: u32, off: u32) -> u32 {
    if off < TO_NONE { let (dx, dy) = ((off >> 11) as i32 - 1024, (off & 2047) as i32 - 1024); ((a / 512) as i32 + dy) as u32 * 512 + ((a % 512) as i32 + dx) as u32 }
    else if off & !(TO_NONE - 1) == FROM_NONE { off & (TO_NONE - 1) } else { NO_CELL }
}
/// An offset in a block: `(dx + 8) << 4 | (dy + 8)` for |d| <= 7 (never 0), else 0 and its varint.
#[inline] pub(crate) fn put_off(o: &mut Vec<u8>, off: u32) {
    if off < TO_NONE { let (dx, dy) = ((off >> 11) as i32 - 1024, (off & 2047) as i32 - 1024); if dx.abs() <= 7 && dy.abs() <= 7 { o.push(((dx + 8) << 4 | (dy + 8)) as u8); return; } }
    o.push(0); varint(o, off as u64);
}
#[inline] pub(crate) fn get_off(b: &[u8], i: &mut usize) -> u32 {
    let x = b[*i]; *i += 1;
    if x != 0 { let (dx, dy) = ((x >> 4) as i32 - 8, (x & 15) as i32 - 8); ((dx + 1024) as u32) << 11 | (dy + 1024) as u32 } else { get_varint(b, i) as u32 }
}

/// One edge, as the setup lays it out (24 B): target key (shape-free; the
/// region gives the shape), source entry, transfer (content-canonical),
/// dense target and source regions, target and source cells in their regions.
#[derive(Clone, Copy)] #[repr(C)]
pub(crate) struct Edge { pub(crate) tkey: u64, pub(crate) se: u32, pub(crate) x: u32, pub(crate) treg: u16, pub(crate) sreg: u16, pub(crate) tpos: u8, pub(crate) spos: u8 }

/// Per dense region: its shape index and slot; its frame-start entries.
/// `front`: per entry its cells in the last frame's layer (the frontier: this frame's sources).
pub(crate) struct Region { pub(crate) shape: u32, pub(crate) slot: u32, pub(crate) keys: Vec<u64>, pub(crate) masks: Vec<u64>, pub(crate) front: Vec<u64> }

pub(crate) struct Data { pub(crate) regions: Vec<Region>, pub(crate) edges: Vec<Vec<Edge>>, pub(crate) n_edges: u64, pub(crate) fp: u64, shape_hash: Vec<u64>, prov_hash: Vec<u64> }

impl Data {
    /// An edge's fingerprint term over CANONICAL values (the shape's hash, a
    /// provisional key's content hash: the setup numbers both by arrival).
    pub(crate) fn term(&self, src: u64, shape: u64, cell: u32, key: u64, x: u64) -> u64 {
        let k = if key & PROV != 0 { 1 << 63 | self.prov_hash[(key & !PROV) as usize] >> 1 } else { key };
        mix(&[src, self.shape_hash[shape as usize], cell as u64, k, x])
    }
}

fn read_meta(path: &str) -> (Vec<(u32, u64)>, Vec<(u32, u32, u64)>) {
    let b = std::fs::read(path).unwrap();
    let u32_ = |i: &mut usize| { let v = u32::from_le_bytes(b[*i..*i + 4].try_into().unwrap()); *i += 4; v };
    let u64_ = |i: &mut usize| { let v = u64::from_le_bytes(b[*i..*i + 8].try_into().unwrap()); *i += 8; v };
    let mut i = 4; assert_eq!(u32_(&mut i), 14, "{path}: format 14");
    let len = u64_(&mut i) as usize; assert!(b.len() >= 16 + len);
    assert_eq!(u32_(&mut i), 8, "{path}: storage regions of 8");
    let ns = u64_(&mut i);
    let shapes = (0..ns).map(|_| { let s = u32_(&mut i); (s, u64_(&mut i)) }).collect();
    let ne = u64_(&mut i);
    let ents = (0..ne).map(|_| { let (r, e) = (u32_(&mut i), u32_(&mut i)); let (hi, lo) = (u64_(&mut i), u64_(&mut i)); assert!(hi == 0 && lo < PROV, "{path}: a key past 55 bits"); (r, e, lo) }).collect();
    (shapes, ents)
}

/// The tree's visited set through frame `last` and the capture's edges (module doc).
fn load(cap: &str, tree: &str, last: u32) -> Data {
    let t0 = Instant::now();
    let mut shape_idx: FxHashMap<u64, u32> = FxHashMap::default();
    let mut ent_keys: FxHashMap<u32, Vec<u64>> = FxHashMap::default();
    let mut ent_masks: FxHashMap<u32, Vec<u64>> = FxHashMap::default();
    let mut front_masks: FxHashMap<u32, Vec<u64>> = FxHashMap::default();
    let mut ents_all: Vec<(u32, u32, u64)> = Vec::new();
    let mut n_states = 0u64;
    for f in 0..=last {
        let fd = format!("{tree}/frames/f{f:03}");
        let (shapes, ents) = read_meta(&format!("{fd}/meta.bin"));
        for (s, h) in shapes { assert!(shape_idx.insert(h, s).is_none()); }
        ents_all.extend(ents);
        let mut files: Vec<_> = std::fs::read_dir(&fd).unwrap().flatten().map(|e| e.path()).filter(|p| p.file_name().unwrap().to_str().unwrap().starts_with('s')).collect();
        files.sort();
        for p in files {
            let b = std::fs::read(&p).unwrap();
            let hl = u64::from_le_bytes(b[8..16].try_into().unwrap()) as usize;
            for c in b[16 + hl..].chunks_exact(8) {
                let id = u64::from_le_bytes(c.try_into().unwrap());
                let (r, e, l) = ((id >> 38) as u32, (id >> 8) as u32 & ((1 << 30) - 1), (id & 0xff) as u32);
                let v = ent_masks.entry(r).or_default();
                if v.len() <= e as usize { v.resize(e as usize + 1, 0); }
                assert!(v[e as usize] >> l & 1 == 0, "a state twice");
                v[e as usize] |= 1 << l;
                if f == last { let w = front_masks.entry(r).or_default(); if w.len() <= e as usize { w.resize(e as usize + 1, 0); } w[e as usize] |= 1 << l; }
                n_states += 1;
            }
        }
    }
    ents_all.sort_unstable();
    for (r, e, k) in ents_all { let v = ent_keys.entry(r).or_default(); assert_eq!(e as usize, v.len(), "region {r}: entries not dense"); v.push(k); }
    let n_entries: usize = ent_keys.values().map(Vec::len).sum();
    println!("[load] tree f0-f{last}: {n_states} states in {n_entries} entries, {} shapes; {:.1} s", shape_idx.len(), t0.elapsed().as_secs_f64());
    // Dense regions: the tree's, then the frame's new ones.
    let mut dense: FxHashMap<u32, u16> = FxHashMap::default();
    let mut regions: Vec<Region> = Vec::new();
    let mut rids: Vec<u32> = ent_keys.keys().copied().collect(); rids.sort_unstable();
    for r in rids {
        let keys = ent_keys.remove(&r).unwrap();
        let mut masks = ent_masks.remove(&r).unwrap_or_default(); masks.resize(keys.len(), 0);
        let mut front = front_masks.remove(&r).unwrap_or_default(); front.resize(keys.len(), 0);
        dense.insert(r, regions.len() as u16);
        regions.push(Region { shape: r / SLOTS, slot: r % SLOTS, keys, masks, front });
    }
    // Transfers by content; MISS contents by bytes.
    let t1 = Instant::now();
    let mut ws: Vec<String> = std::fs::read_dir(cap).unwrap().flatten().map(|e| e.file_name().to_str().unwrap().to_string()).filter(|n| n.starts_with('w')).collect();
    ws.sort();
    let mut intern: FxHashMap<[u8; 26], u32> = FxHashMap::default();
    let xlocal: Vec<Vec<u32>> = ws.iter().map(|w| {
        let b = std::fs::read(format!("{cap}/x{}", &w[1..])).unwrap();
        b.chunks_exact(26).map(|c| { let n = intern.len() as u32; *intern.entry(c.try_into().unwrap()).or_insert(n) }).collect()
    }).collect();
    let n_x = intern.len();
    let shared = Mutex::new((shape_idx, dense, regions.len(), Vec::<(u32, u16)>::new(), FxHashMap::<Vec<u8>, u64>::default()));
    let parts: Vec<(Vec<Edge>, u64)> = std::thread::scope(|sc| {
        let hs: Vec<_> = ws.iter().enumerate().map(|(wi, w)| {
            let (shared, xl) = (&shared, &xlocal[wi]);
            sc.spawn(move || {
                let m = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{cap}/{w}")).unwrap()).unwrap() };
                let (mut out, mut n) = (Vec::new(), 0u64);
                let mut srcs: Vec<u64> = Vec::new();
                let mut miss: Option<Vec<u8>> = None;
                let mut local_dense: FxHashMap<u32, u16> = FxHashMap::default();
                let mut local_shape: FxHashMap<u64, u32> = FxHashMap::default();
                let mut i = 0;
                let rd = |i: &mut usize, k: usize| { let s = &m[*i..*i + k]; *i += k; s };
                while i < m.len() {
                    let tag = m[i]; i += 1;
                    match tag {
                        1 => {
                            rd(&mut i, 4 + 4 + 8); let ns = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap()) as usize; rd(&mut i, 1);
                            srcs.clear(); for _ in 0..ns { srcs.push(u64::from_le_bytes(rd(&mut i, 8).try_into().unwrap())); }
                        }
                        2 => {
                            let shape = u64::from_le_bytes(rd(&mut i, 8).try_into().unwrap());
                            let (hi, lo) = (u64::from_le_bytes(rd(&mut i, 8).try_into().unwrap()), u64::from_le_bytes(rd(&mut i, 8).try_into().unwrap()));
                            let cell = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap());
                            let lane = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap());
                            let xw = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap());
                            let mb = miss.take();
                            if xw == u32::MAX { continue; }
                            let tkey = if hi >> 63 == 1 {
                                let c = mb.expect("a provisional key without its MISS content");
                                let mut g = shared.lock().unwrap(); let n = g.4.len() as u64; PROV | *g.4.entry(c).or_insert(n)
                            } else { assert!(hi == 0 && lo < PROV, "a key past 55 bits"); lo };
                            let sh = match local_shape.get(&shape) { Some(&s) => s, None => { let mut g = shared.lock().unwrap(); let n = g.0.len() as u32; let s = *g.0.entry(shape).or_insert(n); local_shape.insert(shape, s); s } };
                            let (slot, tpos) = slot_local(cell);
                            let rid = sh * SLOTS + slot;
                            let treg = match local_dense.get(&rid) { Some(&d) => d, None => { let mut g = shared.lock().unwrap(); let d = match g.1.get(&rid) { Some(&d) => d, None => { let d = g.2 as u16; g.2 += 1; g.1.insert(rid, d); g.3.push((rid, d)); d } }; local_dense.insert(rid, d); d } };
                            let src = srcs[lane as usize];
                            let (sr, se, sl) = ((src >> 38) as u32, (src >> 8) as u32 & ((1 << 30) - 1), (src & 0xff) as u32);
                            let sreg = match local_dense.get(&sr) { Some(&d) => d, None => { let g = shared.lock().unwrap(); let d = *g.1.get(&sr).expect("a source region the tree lacks"); local_dense.insert(sr, d); d } };
                            let x = xl[xw as usize];
                            out.push(Edge { tkey, se, x, treg, sreg, tpos: tpos as u8, spos: sl as u8 });
                            n += 1;
                        }
                        3 => { rd(&mut i, 8); }
                        4 => { let l = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap()) as usize; miss = Some(rd(&mut i, l).to_vec()); }
                        5 => { let l = u32::from_le_bytes(rd(&mut i, 4).try_into().unwrap()) as usize; rd(&mut i, l); }
                        t => panic!("{w}: tag {t} at {i}"),
                    }
                }
                (out, n)
            })
        }).collect();
        hs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    let g = shared.into_inner().unwrap();
    for &(rid, d) in &g.3 { assert_eq!(d as usize, regions.len()); regions.push(Region { shape: rid / SLOTS, slot: rid % SLOTS, keys: Vec::new(), masks: Vec::new(), front: Vec::new() }); }
    assert!(regions.len() < 1 << 16);
    let mut n_edges = 0u64;
    let mut edges = Vec::with_capacity(parts.len());
    for (e, n) in parts { n_edges += n; edges.push(e); }
    let mut shape_hash = vec![0u64; g.0.len()];
    for (&h, &i) in &g.0 { shape_hash[i as usize] = h; }
    let mut prov_hash = vec![0u64; g.4.len()];
    for (c, &i) in &g.4 { prov_hash[i as usize] = c.chunks(8).fold(c.len() as u64, |a, w| { let mut b = [0u8; 8]; b[..w.len()].copy_from_slice(w); crate::bits::mix64(a ^ u64::from_le_bytes(b)) }); }
    let mut d = Data { regions, edges, n_edges, fp: 0, shape_hash, prov_hash };
    let fp = d.edges.iter().flatten().fold(0u64, |a, e| {
        let (sr, tr) = (&d.regions[e.sreg as usize], &d.regions[e.treg as usize]);
        a.wrapping_add(d.term((d_region_id(sr) as u64) << 38 | (e.se as u64) << 8 | e.spos as u64, tr.shape as u64, cell_of(tr.slot, e.tpos as u32), e.tkey, e.x as u64))
    });
    d.fp = fp;
    println!("[load] capture: {n_edges} edges, {n_x} transfers, {} provisional keys, {} regions ({} new); fingerprint {fp:016x}; {:.1} s", d.prov_hash.len(), d.regions.len(), g.3.len(), t1.elapsed().as_secs_f64());
    d
}

/// A unit: its region and part of it, its home entries at rest (T and dedup:
/// the frame-start entries `varint key delta, u64 mask`; S: none), its batch.
struct Unit<E> { region: u16, part: u32, rest: Vec<u8>, batch: Vec<E> }

/// A target unit's edge (32 B): the target key and cell, and T's payload as
/// the forward has it at the emission - the source's (shape << 56 | key) (the
/// kernel's input row), the transfer, the offset code.
#[derive(Clone, Copy)] #[repr(C)]
struct TEdge { tkey: u64, src: u64, x: u32, off: u32, tpos: u8 }

/// The units of a grouping: by target region (`units_t`, T's payload
/// resolved: the source's key and the offset) or by source region
/// (`units_s`), heavy ones split into parts of at most `split` edges by a
/// hash of the home key (target key; source entry), heaviest first.
fn group(d: &Data, by_target: bool) -> Vec<Vec<Edge>> {
    let mut per: Vec<Vec<Edge>> = vec![Vec::new(); d.regions.len()];
    for v in &d.edges { for e in v { per[if by_target { e.treg } else { e.sreg } as usize].push(*e); } }
    per
}

fn parts_of(n: usize, split: usize) -> u32 { if split == 0 { 1 } else { n.div_ceil(split).max(1) as u32 } }

fn units_t(d: &Data, split: usize) -> Vec<Unit<TEdge>> {
    let mut out = Vec::new();
    for (r, b) in group(d, true).into_iter().enumerate() {
        if b.is_empty() { continue; }
        let parts = parts_of(b.len(), split);
        let reg = &d.regions[r];
        for p in 0..parts {
            let batch: Vec<TEdge> = b.iter().filter(|e| parts == 1 || hpart(e.tkey, parts) == p).map(|e| {
                let sr = &d.regions[e.sreg as usize];
                TEdge { tkey: e.tkey, src: (sr.shape as u64) << 56 | sr.keys[e.se as usize], x: e.x, off: offset(cell_of(sr.slot, e.spos as u32), cell_of(reg.slot, e.tpos as u32), true), tpos: e.tpos }
            }).collect();
            let mut old: Vec<(u64, u64)> = reg.keys.iter().zip(&reg.masks).filter(|(k, _)| parts == 1 || hpart(**k, parts) == p).map(|(k, m)| (*k, *m)).collect();
            old.sort_unstable();
            let mut rest = Vec::new();
            varint(&mut rest, old.len() as u64);
            let mut pk = 0u64;
            for (k, m) in old { varint(&mut rest, k - pk); rest.extend_from_slice(&m.to_le_bytes()); pk = k; }
            out.push(Unit { region: r as u16, part: p, rest, batch });
        }
    }
    out.sort_by_key(|u| std::cmp::Reverse(u.batch.len()));
    out
}

fn units_s(d: &Data, split: usize) -> Vec<Unit<Edge>> {
    let mut out = Vec::new();
    for (r, b) in group(d, false).into_iter().enumerate() {
        if b.is_empty() { continue; }
        let parts = parts_of(b.len(), split);
        if parts == 1 { out.push(Unit { region: r as u16, part: 0, rest: Vec::new(), batch: b }); continue; }
        for p in 0..parts {
            out.push(Unit { region: r as u16, part: p, rest: Vec::new(), batch: b.iter().filter(|e| hpart(e.se as u64, parts) == p).copied().collect() });
        }
    }
    out.sort_by_key(|u| std::cmp::Reverse(u.batch.len()));
    out
}

/// A visited-set entry in the unit's table: key, old and new masks, its
/// local number (insertion order: stable across a regrow).
#[derive(Clone, Copy)] pub(crate) struct H { pub(crate) key: u64, pub(crate) m: u64, pub(crate) nm: u64, pub(crate) lid: u32 }
/// An edge-table entry: `a` the other side's (shape << 56 | key), `b` home << 32
/// | transfer, `c` the offset code; the mask; the generation that owns it.
#[derive(Clone, Copy)] #[repr(C)] struct BE { a: u64, b: u64, mask: u64, c: u32, gen: u32 }

struct Scratch { tab: Vec<H>, lids: Vec<u32>, et: Vec<BE>, gen: u32, coll: Vec<(u64, u64, u32, u64)>, coll2: Vec<(u64, u64, u32, u64)>, cnt: Vec<u32>, dict: Vec<u64>, sorted: Vec<(u64, u32)>, enc: Vec<u8>, arena: Vec<u8>, used: usize }

#[derive(Default, Clone)] struct Out { new: u64, bundles: u64, table: u64, thread: usize, off: usize, len: usize, t_ins: u64, t_enc: u64 }

#[inline(always)] pub(crate) fn hslot(tab: &[H], mask: usize, k: u64) -> usize { let mut i = (crate::bits::mix64(k) >> 20) as usize & mask; loop { let x = tab[i].key; if x == k || x == EMPTY { return i; } i = (i + 1) & mask; } }
#[inline(always)] fn ehash(a: u64, b: u64, c: u32) -> u64 { crate::bits::mix64(a ^ b.rotate_left(23) ^ (c as u64).rotate_left(47)) }

/// One probe and one OR (module doc); grows the table (rare) by re-inserting
/// this generation's entries.
#[inline(always)]
fn edge_insert(s: &mut Scratch, cap: &mut usize, used: &mut usize, a: u64, b: u64, c: u32, bit: u64) {
    let mut m = *cap - 1;
    let mut i = (ehash(a, b, c) >> 24) as usize & m;
    loop {
        let e = &mut s.et[i];
        if e.gen != s.gen { *e = BE { a, b, mask: bit, c, gen: s.gen }; *used += 1; break; }
        if e.a == a && e.b == b && e.c == c { e.mask |= bit; return; }
        i = (i + 1) & m;
    }
    if *used * 10 > *cap * 7 {
        let old: Vec<BE> = s.et[..*cap].iter().filter(|e| e.gen == s.gen).copied().collect();
        *cap *= 2; m = *cap - 1;
        if s.et.len() < *cap { s.et.resize(*cap, BE { a: 0, b: 0, mask: 0, c: 0, gen: 0 }); }
        s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for e in s.et.iter_mut() { e.gen = 0; } s.gen = 1; }
        for mut e in old { e.gen = s.gen; let mut q = (ehash(e.a, e.b, e.c) >> 24) as usize & m; while s.et[q].gen == s.gen { q = (q + 1) & m; } s.et[q] = e; }
    }
}

/// The unit's edge table as its at-rest block: entries sorted (home, other); a dictionary of the other sides (sorted, delta-coded);
/// per bundle: home delta (0: the same), dictionary rank (a delta within the
/// home), transfer (global id), offset byte, u64 mask.
fn encode_edges(s: &mut Scratch, cap: usize, n_homes: usize, home_rank: impl Fn(u32) -> u32) -> u64 {
    // By home: a counting sort (homes are dense: the unit's entry ranks, the
    // region's source entries); the dictionary of the other sides sorted;
    // each home's few bundles sorted by (other side's rank, transfer, offset).
    s.cnt.clear(); s.cnt.resize(n_homes + 1, 0);
    s.coll.clear();
    for e in s.et[..cap].iter() {
        if e.gen == s.gen { let h = home_rank((e.b >> 32) as u32); s.cnt[h as usize + 1] += 1; s.coll.push(((h as u64) << 32 | (e.b & 0xffff_ffff), e.a, e.c, e.mask)); }
    }
    for k in 0..n_homes { s.cnt[k + 1] += s.cnt[k]; }
    s.dict.clear(); s.dict.extend(s.coll.iter().map(|x| x.1)); s.dict.sort_unstable(); s.dict.dedup();
    s.coll2.clear(); s.coll2.resize(s.coll.len(), (0, 0, 0, 0));
    for &(hx, a, c, mask) in &s.coll {
        let h = (hx >> 32) as usize;
        let r = s.dict.binary_search(&a).unwrap() as u64;
        s.coll2[s.cnt[h] as usize] = (hx, r, c, mask); s.cnt[h] += 1;
    }
    let o = &mut s.enc; o.clear();
    varint(o, s.dict.len() as u64);
    let mut p = 0u64; for &k in &s.dict { varint(o, k - p); p = k; }
    varint(o, s.coll2.len() as u64);
    let (mut ph, mut pr) = (u64::MAX, 0u64);
    let mut i = 0;
    while i < s.coll2.len() {
        let h = s.coll2[i].0 >> 32;
        let mut j = i + 1; while j < s.coll2.len() && s.coll2[j].0 >> 32 == h { j += 1; }
        let seg = &mut s.coll2[i..j];
        // (other, transfer, offset): equal fields next to each other compress.
        let key = |x: &(u64, u64, u32, u64)| (x.1 as u128) << 64 | ((x.0 & 0xffff_ffff) as u128) << 32 | x.2 as u128;
        if seg.len() <= 16 { for a in 1..seg.len() { let v = seg[a]; let mut k = a; while k > 0 && key(&seg[k - 1]) > key(&v) { seg[k] = seg[k - 1]; k -= 1; } seg[k] = v; } } else { seg.sort_unstable_by_key(key); }
        for &(hx, r, c, mask) in seg.iter() {
            let x = hx & 0xffff_ffff;
            if h != ph { varint(o, if ph == u64::MAX { h + 1 } else { h - ph }); varint(o, r); ph = h; } else { varint(o, 0); varint(o, r - pr); }
            pr = r;
            varint(o, x); put_off(o, c); o.extend_from_slice(&mask.to_le_bytes());
        }
        i = j;
    }
    s.coll2.len() as u64
}

/// The dedup (and T's edges) on one target unit: decode the old entries,
/// run the batch, re-encode (J2), and with `edges` insert each edge into the
/// unit's T table and encode it.
fn run_target(u: &Unit<TEdge>, edges: bool, s: &mut Scratch, ti: usize) -> Out {
    let b = &u.rest; let mut i = 0;
    let n = get_varint(b, &mut i) as usize;
    let cap = ((n + u.batch.len().min(4 * n + 64)) * 2).next_power_of_two().max(16);
    s.tab.clear(); s.tab.resize(cap, H { key: EMPTY, m: 0, nm: 0, lid: u32::MAX });
    let mut mask = cap - 1;
    let mut k = 0u64;
    for _ in 0..n { k += get_varint(b, &mut i); let m = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8; let t = hslot(&s.tab, mask, k); s.tab[t] = H { key: k, m, nm: 0, lid: u32::MAX }; }
    let mut used = n;
    s.lids.clear();
    let (mut ecap, mut eused) = ((u.batch.len() / 4).next_power_of_two().max(64), 0usize);
    if edges { if s.et.len() < ecap { s.et.resize(ecap, BE { a: 0, b: 0, mask: 0, c: 0, gen: 0 }); } s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for e in s.et.iter_mut() { e.gen = 0; } s.gen = 1; } }
    let mut new = 0u64;
    let t_ins = Instant::now();
    for e in &u.batch {
        let mut t = hslot(&s.tab, mask, e.tkey);
        if s.tab[t].key == EMPTY {
            if (used + 1) * 10 > s.tab.len() * 7 {
                let old: Vec<H> = s.tab.drain(..).filter(|h| h.key != EMPTY).collect();
                let nc = (mask + 1) * 2; mask = nc - 1;
                s.tab.resize(nc, H { key: EMPTY, m: 0, nm: 0, lid: u32::MAX });
                for h in old { let q = hslot(&s.tab, mask, h.key); s.tab[q] = h; }
                t = hslot(&s.tab, mask, e.tkey);
            }
            s.tab[t].key = e.tkey; used += 1;
        }
        let h = &mut s.tab[t];
        let bit = 1u64 << e.tpos;
        if (h.m | h.nm) & bit == 0 { h.nm |= bit; new += 1; }
        if edges {
            if h.lid == u32::MAX { h.lid = s.lids.len() as u32; s.lids.push(0); }
            let lid = h.lid;
            edge_insert(s, &mut ecap, &mut eused, e.src, (lid as u64) << 32 | e.x as u64, e.off, bit);
        }
    }
    let t_ins = t_ins.elapsed().as_nanos() as u64;
    let t_enc = Instant::now();
    // Re-encode the entries in key order (the at-rest form), and the edges.
    s.sorted.clear();
    for (q, h) in s.tab.iter().enumerate() { if h.key != EMPTY { s.sorted.push((h.key, q as u32)); } }
    s.sorted.sort_unstable();
    s.enc.clear(); varint(&mut s.enc, s.sorted.len() as u64);
    let mut pk = 0u64;
    for &(k, q) in &s.sorted { let h = &s.tab[q as usize]; varint(&mut s.enc, k - pk); s.enc.extend_from_slice(&(h.m | h.nm).to_le_bytes()); pk = k; }
    std::hint::black_box(s.enc.len());
    let mut out = Out { new, thread: ti, table: 0, t_ins, ..Default::default() };
    if edges {
        // lid -> rank of its key among the unit's entries.
        for (r, &(_, q)) in s.sorted.iter().enumerate() { let l = s.tab[q as usize].lid; if l != u32::MAX { s.lids[l as usize] = r as u32; } }
        let lids = std::mem::take(&mut s.lids);
        let nh = s.sorted.len();
        out.bundles = encode_edges(s, ecap, nh, |l| lids[l as usize]);
        s.lids = lids;
        let len = s.enc.len();
        assert!(s.used + len <= s.arena.len(), "edge arena too small");
        s.arena[s.used..s.used + len].copy_from_slice(&s.enc);
        (out.off, out.len) = (s.used, len);
        s.used += len;
        out.table = 32 * ecap as u64;
    }
    out.t_enc = t_enc.elapsed().as_nanos() as u64;
    out
}

/// S's edges on one source unit: home = the source entry, other = the
/// target's (shape, key), the mask over source cells.
fn run_source(u: &Unit<Edge>, d: &Data, s: &mut Scratch, ti: usize) -> Out {
    let (mut ecap, mut eused) = ((u.batch.len() / 4).next_power_of_two().max(64), 0usize);
    if s.et.len() < ecap { s.et.resize(ecap, BE { a: 0, b: 0, mask: 0, c: 0, gen: 0 }); }
    s.gen = s.gen.wrapping_add(1); if s.gen == 0 { for e in s.et.iter_mut() { e.gen = 0; } s.gen = 1; }
    let reg = &d.regions[u.region as usize];
    let t_ins = Instant::now();
    for e in &u.batch {
        let tr = &d.regions[e.treg as usize];
        let off = offset(cell_of(reg.slot, e.spos as u32), cell_of(tr.slot, e.tpos as u32), false);
        edge_insert(s, &mut ecap, &mut eused, (tr.shape as u64) << 56 | e.tkey, (e.se as u64) << 32 | e.x as u64, off, 1 << e.spos);
    }
    let t_ins = t_ins.elapsed().as_nanos() as u64;
    let t_enc = Instant::now();
    let mut out = Out { thread: ti, table: 32 * ecap as u64, t_ins, ..Default::default() };
    out.bundles = encode_edges(s, ecap, d.regions[u.region as usize].keys.len(), |se| se);
    let len = s.enc.len();
    assert!(s.used + len <= s.arena.len(), "edge arena too small");
    s.arena[s.used..s.used + len].copy_from_slice(&s.enc);
    (out.off, out.len) = (s.used, len);
    s.used += len;
    out.t_enc = t_enc.elapsed().as_nanos() as u64;
    out
}

/// A decoded bundle: home, other, transfer, offset, mask.
type Bundle = (u64, u64, u64, u32, u64);

fn decode_block(b: &[u8], mut f: impl FnMut(Bundle)) {
    let mut i = 0;
    let nd = get_varint(b, &mut i) as usize;
    let mut dict = Vec::with_capacity(nd); let mut p = 0u64; for _ in 0..nd { p += get_varint(b, &mut i); dict.push(p); }
    let nb = get_varint(b, &mut i);
    let (mut h, mut r) = (u64::MAX, 0u64);
    for _ in 0..nb {
        let dh = get_varint(b, &mut i);
        if dh != 0 { h = if h == u64::MAX { dh - 1 } else { h + dh }; r = get_varint(b, &mut i); } else { r += get_varint(b, &mut i); }
        let x = get_varint(b, &mut i); let off = get_off(b, &mut i);
        let mask = u64::from_le_bytes(b[i..i + 8].try_into().unwrap()); i += 8;
        f((h, dict[r as usize], x, off, mask));
    }
    assert_eq!(i, b.len(), "a block did not decode to its end");
}

/// A target unit's homes in rank order: its entries after the frame (the
/// frame-start ones of its part and its batch's), sorted by key; and per
/// home its cells after the frame.
fn homes(d: &Data, u: &Unit<TEdge>, parts: u32) -> (Vec<u64>, Vec<u64>) {
    let reg = &d.regions[u.region as usize];
    let mut m: FxHashMap<u64, u64> = reg.keys.iter().zip(&reg.masks).filter(|(k, _)| parts == 1 || hpart(**k, parts) == u.part).map(|(k, m)| (*k, *m)).collect();
    for e in &u.batch { *m.entry(e.tkey).or_default() |= 1 << e.tpos; }
    let mut v: Vec<(u64, u64)> = m.into_iter().collect(); v.sort_unstable();
    v.into_iter().unzip()
}

/// Untimed: every unit's block decoded back to (source id, target shape,
/// cell, key, transfer) - the fingerprint and count against the capture's -
/// and the sizes of the alternatives: masks relative to the home entry's
/// cells (`ceil(k / 8)` bytes, none for one cell) instead of the u64,
/// transfers as block ranks (+ 4 B a block's transfer) instead of global
/// ids, and the transfer factored per (home, offset) where it is unique;
/// zstd -1 per block of each.
fn check(mode: &str, d: &Data, us: &[(u16, u32)], tus: &[Unit<TEdge>], outs: &[Out], scr: &[Scratch]) {
    let tv = Instant::now();
    let dense: FxHashMap<u32, usize> = d.regions.iter().enumerate().map(|(i, r)| (d_region_id(r), i)).collect();
    let key_index: Vec<FxHashMap<u64, u32>> = d.regions.iter().map(|r| r.keys.iter().enumerate().map(|(e, &k)| (k, e as u32)).collect()).collect();
    let mut parts: FxHashMap<u16, u32> = FxHashMap::default();
    for u in us { *parts.entry(u.0).or_default() += 1; }
    let (mut fp, mut cnt, mut bundles) = (0u64, 0u64, 0u64);
    let (mut raw, mut z, mut alt, mut z_alt, mut fact, mut z_fact, mut uni_groups, mut groups) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    let mut hist = [0u64; 65];
    let (mut dict_bytes, mut dict_n) = (0u64, 0u64);
    for (ui, (&(region, _), o)) in us.iter().zip(outs).enumerate() {
        let b = &scr[o.thread].arena[o.off..o.off + o.len];
        raw += b.len() as u64;
        z += zstd::bulk::compress(b, 1).unwrap().len() as u64;
        { let mut i = 0; let nd = get_varint(b, &mut i); for _ in 0..nd { get_varint(b, &mut i); } dict_bytes += i as u64; dict_n += nd; }
        let reg = &d.regions[region as usize];
        let rid = d_region_id(reg) as u64;
        let (hk, hm) = if mode == "t" { homes(d, &tus[ui], parts[&region]) } else { (Vec::new(), Vec::new()) };
        let home_cells = |h: u64| if mode == "t" { hm[h as usize].count_ones() } else { reg.masks[h as usize].count_ones() };
        let mut all: Vec<Bundle> = Vec::new();
        decode_block(b, |bd| all.push(bd));
        bundles += all.len() as u64;
        for &(h, a, x, off, mask) in &all {
            hist[mask.count_ones() as usize] += 1;
            let mut m = mask;
            while m != 0 {
                let p = m.trailing_zeros(); m &= m - 1;
                let (src, tsh, tcell, tkey) = if mode == "s" {
                    (rid << 38 | h << 8 | p as u64, a >> 56, fwd(cell_of(reg.slot, p), off), a & ((1 << 56) - 1))
                } else {
                    let tcell = cell_of(reg.slot, p);
                    let (sslot, sl) = slot_local(back(tcell, off));
                    let srid = (a >> 56) as u32 * SLOTS + sslot;
                    let se = key_index[dense[&srid]][&(a & ((1 << 56) - 1))];
                    ((srid as u64) << 38 | (se as u64) << 8 | sl as u64, reg.shape as u64, tcell, hk[h as usize])
                };
                fp = fp.wrapping_add(d.term(src, tsh, tcell, tkey, x)); cnt += 1;
            }
        }
        // The alternatives, as blocks: the same dictionary; relative masks; transfer ranks.
        let mut xu: FxHashMap<u64, u64> = FxHashMap::default();
        for bd in &all { *xu.entry(bd.2).or_default() += 1; }
        let mut xs: Vec<(u64, u64)> = xu.into_iter().map(|(x, n)| (n, x)).collect();
        xs.sort_unstable_by(|p, q| q.cmp(p));
        let xr: FxHashMap<u64, u64> = xs.iter().enumerate().map(|(r, &(_, x))| (x, r as u64)).collect();
        let mut dict: Vec<u64> = all.iter().map(|x| x.1).collect(); dict.sort_unstable(); dict.dedup();
        let rank = |a: u64| dict.binary_search(&a).unwrap() as u64;
        let rel = |h: u64, mask: u64, o: &mut Vec<u8>| {
            let k = home_cells(h);
            assert_eq!(mask & !(if mode == "t" { hm[h as usize] } else { reg.masks[h as usize] }), 0, "a bundle cell outside its home");
            if k > 1 {
                let full = if mode == "t" { hm[h as usize] } else { reg.masks[h as usize] };
                let (mut r, mut j, mut f) = (0u64, 0, full);
                while f != 0 { let c = f.trailing_zeros(); f &= f - 1; if mask >> c & 1 == 1 { r |= 1 << j; } j += 1; }
                o.extend_from_slice(&r.to_le_bytes()[..(k as usize).div_ceil(8)]);
            }
        };
        let mut head = Vec::new();
        varint(&mut head, dict.len() as u64); let mut p = 0u64; for &k in &dict { varint(&mut head, k - p); p = k; }
        varint(&mut head, xs.len() as u64); for &(_, x) in &xs { head.extend_from_slice(&(x as u32).to_le_bytes()); }
        let mut ab = head.clone();
        let (mut ph, mut pr) = (u64::MAX, 0u64);
        for &(h, a, x, off, mask) in &all {
            let r = rank(a);
            if h != ph { varint(&mut ab, if ph == u64::MAX { h + 1 } else { h - ph }); varint(&mut ab, r); ph = h; } else { varint(&mut ab, 0); varint(&mut ab, r - pr); }
            pr = r; varint(&mut ab, xr[&x]); put_off(&mut ab, off); rel(h, mask, &mut ab);
        }
        alt += ab.len() as u64; z_alt += zstd::bulk::compress(&ab, 1).unwrap().len() as u64;
        // The transfer factored per (home, offset): groups in (home, offset) order, the transfer once when unique.
        let mut v: Vec<Bundle> = all.clone();
        v.sort_unstable_by_key(|q| (q.0, q.3, q.1, q.2));
        let mut fb = head;
        let (mut i, mut ph) = (0, u64::MAX);
        while i < v.len() {
            let mut j = i + 1; while j < v.len() && (v[j].0, v[j].3) == (v[i].0, v[i].3) { j += 1; }
            let uni = v[i..j].iter().all(|q| q.2 == v[i].2);
            groups += 1; uni_groups += uni as u64;
            let h = v[i].0;
            varint(&mut fb, if ph == u64::MAX { h + 1 } else { h - ph }); ph = h;
            put_off(&mut fb, v[i].3); varint(&mut fb, ((j - i) as u64) << 1 | uni as u64);
            if uni { varint(&mut fb, xr[&v[i].2]); }
            let mut pr = 0u64;
            for q in &v[i..j] { let r = rank(q.1); varint(&mut fb, r.wrapping_sub(pr) & 0xffff_ffff); pr = r; if !uni { varint(&mut fb, xr[&q.2]); } rel(h, q.4, &mut fb); }
            i = j;
        }
        fact += fb.len() as u64; z_fact += zstd::bulk::compress(&fb, 1).unwrap().len() as u64;
    }
    let e = d.n_edges as f64;
    let wq = |q: f64| { let tot: u64 = (0..65).map(|k| k as u64 * hist[k]).sum(); let mut a = 0; for k in 0..65 { a += k as u64 * hist[k]; if a as f64 >= q * tot as f64 { return k; } } 64 };
    println!("    CHECK {mode}: {cnt} edges decoded (want {}), fingerprint {fp:016x} ({}); {bundles} bundles ({:.2} edges each, edge-weighted median {}, single-cell {:.1}% of bundles); B an edge at rest: block (global transfer ids, u64 masks) {:.3} (of it the dictionaries, {dict_n} other sides: {:.3}), zstd -1 {:.3}; block ranks + relative masks {:.3}, zstd -1 {:.3}; the transfer factored per (home, offset) ({:.1}% of {groups} groups unique) {:.3}, zstd -1 {:.3}; check {:.0} s",
        d.n_edges, if fp == d.fp && cnt == d.n_edges { "EQUAL to the capture's" } else { "MISMATCH" }, cnt as f64 / bundles.max(1) as f64, wq(0.5), 100.0 * hist[1] as f64 / bundles.max(1) as f64,
        raw as f64 / e, dict_bytes as f64 / e, z as f64 / e, alt as f64 / e, z_alt as f64 / e, 100.0 * uni_groups as f64 / groups.max(1) as f64, fact as f64 / e, z_fact as f64 / e, tv.elapsed().as_secs_f64());
}

pub(crate) fn d_region_id(r: &Region) -> u32 { r.shape * SLOTS + r.slot }

/// T's backward access, from the blocks at rest: (1) every in-edge, block by
/// block (decode, expand the masks: source cell = target cell - offset);
/// (2) a sample of target states, batched by unit: decode the unit's block,
/// find each target's home by a binary search over its bundles' homes and
/// enumerate its bundles holding the target's cell.
fn access(d: &Data, us: &[Unit<TEdge>], outs: &[Out], scr: &[Scratch], threads: usize) {
    // The sample (untimed): 1M targets with in-edges, by unit.
    // Samples (untimed): the distinct target states with in-edges, each kept
    // with probability 1 / `every`, by unit.
    let mut parts: FxHashMap<u16, u32> = FxHashMap::default();
    for u in us { *parts.entry(u.region).or_default() += 1; }
    let homes_of: Vec<Vec<u64>> = us.iter().map(|u| homes(d, u, parts[&u.region]).0).collect();
    let targets: Vec<Vec<(u32, u8)>> = us.iter().enumerate().map(|(ui, u)| {
        let mut v: Vec<(u32, u8)> = u.batch.iter().map(|e| (homes_of[ui].binary_search(&e.tkey).unwrap() as u32, e.tpos)).collect();
        v.sort_unstable(); v.dedup(); v
    }).collect();
    let n_targets: u64 = targets.iter().map(|v| v.len() as u64).sum();
    let sample = |every: u64| -> Vec<Vec<(u32, u8)>> {
        let mut rng = 0x9E37_79B9_7F4A_7C15u64;
        targets.iter().map(|v| v.iter().filter(|_| { rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407); (rng >> 33) % every == 0 }).copied().collect()).collect()
    };
    for (pass, every) in [("all", 0u64), ("sample", 30), ("sample", 1000)] {
        let per_unit = if every == 0 { Vec::new() } else { sample(every) };
        let n_sample: u64 = per_unit.iter().map(|v| v.len() as u64).sum();
        let blocks = per_unit.iter().filter(|v| !v.is_empty()).count();
        let next = AtomicUsize::new(0);
        let t = Instant::now();
        let res: Vec<(u64, u64)> = std::thread::scope(|sc| {
            let hs: Vec<_> = (0..threads).map(|_| { let (next, per_unit) = (&next, &per_unit); sc.spawn(move || {
                let (mut n, mut acc) = (0u64, 0u64);
                let mut bun: Vec<Bundle> = Vec::new();
                loop {
                    let i = next.fetch_add(1, Ordering::Relaxed); if i >= us.len() { break; }
                    let o = &outs[i]; let b = &scr[o.thread].arena[o.off..o.off + o.len];
                    let reg = &d.regions[us[i].region as usize];
                    if pass == "all" {
                        decode_block(b, |(h, a, x, off, mask)| { let mut m = mask; while m != 0 { let p = m.trailing_zeros(); m &= m - 1; let sc = back(cell_of(reg.slot, p), off); acc = acc.wrapping_add(sc as u64 ^ a ^ x ^ h); n += 1; } });
                    } else {
                        if per_unit[i].is_empty() { continue; }
                        bun.clear(); decode_block(b, |bd| bun.push(bd));
                        for &(h, p) in &per_unit[i] {
                            let lo = bun.partition_point(|q| q.0 < h as u64);
                            for q in &bun[lo..] { if q.0 != h as u64 { break; } if q.4 >> p & 1 == 1 { let sc = back(cell_of(reg.slot, p as u32), q.3); acc = acc.wrapping_add(sc as u64 ^ q.1 ^ q.2); n += 1; } }
                        }
                    }
                }
                (n, acc)
            }) }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let wall = t.elapsed().as_secs_f64();
        let n: u64 = res.iter().map(|x| x.0).sum();
        std::hint::black_box(res.iter().fold(0u64, |a, x| a ^ x.1));
        if pass == "all" { println!("    ACCESS T, {threads} threads: every in-edge, block by block: {n} in-edges in {wall:.3} s ({:.2} ns an in-edge)", wall * 1e9 / n.max(1) as f64); }
        else { println!("    ACCESS T, {threads} threads: {n_sample} sampled targets (1 in {every} of the {n_targets} with in-edges; {blocks} of {} unit blocks decoded, once each): {n} in-edges ({:.1} a target) in {wall:.4} s ({:.0} ns a target, {:.2} ns an in-edge)", us.len(), n as f64 / n_sample.max(1) as f64, wall * 1e9 / n_sample.max(1) as f64, wall * 1e9 / n.max(1) as f64); }
    }
}

pub fn edgebundle(args: &[String]) {
    let (cap, tree) = (&args[0], &args[1]);
    let d = load(cap, tree, 56);
    if args.get(2).is_some_and(|a| a == "idfree") { return crate::edgekeys::idfree(d, &args[3..]); }
    let mut tb: Option<(usize, Vec<Unit<TEdge>>)> = None;
    let mut sb: Option<(usize, Vec<Unit<Edge>>)> = None;
    let mut last_t: Option<(Vec<Out>, Vec<Scratch>)> = None;
    let mut checked: Vec<String> = Vec::new();
    for run in &args[2..] {
        let v: Vec<&str> = run.split(':').collect();
        let (mode, threads) = (v[0], v[1].parse::<usize>().unwrap());
        if mode == "access" {
            let (outs, scr) = last_t.as_ref().expect("access after a t run");
            access(&d, &tb.as_ref().expect("t units").1, outs, scr, threads);
            continue;
        }
        let split: usize = v[2].parse().unwrap();
        let tu = Instant::now();
        // One grouping alive at a time (each holds every edge).
        if mode == "s" {
            if !sb.as_ref().is_some_and(|b| b.0 == split) { drop(tb.take()); drop(last_t.take()); drop(sb.take()); sb = Some((split, units_s(&d, split))); println!("[units] by source, split {split}: {} units, largest {}; {:.1} s (untimed)", sb.as_ref().unwrap().1.len(), sb.as_ref().unwrap().1[0].batch.len(), tu.elapsed().as_secs_f64()); }
        } else if !tb.as_ref().is_some_and(|b| b.0 == split) {
            drop(sb.take()); drop(tb.take()); drop(last_t.take()); tb = Some((split, units_t(&d, split)));
            println!("[units] by target, split {split}: {} units, largest {}; {:.1} s (untimed)", tb.as_ref().unwrap().1.len(), tb.as_ref().unwrap().1[0].batch.len(), tu.elapsed().as_secs_f64());
        }
        let (n_units, max_b) = if mode == "s" { let u = &sb.as_ref().unwrap().1; (u.len(), u.iter().map(|u| u.batch.len()).max().unwrap()) } else { let u = &tb.as_ref().unwrap().1; (u.len(), u.iter().map(|u| u.batch.len()).max().unwrap()) };
        let arena = if mode == "dedup" { 0 } else { (d.n_edges as usize * 3 / threads).max(64 << 20) + 32 * max_b };
        let mut scr: Vec<Scratch> = (0..threads).map(|_| {
            let mut s = Scratch { tab: Vec::new(), lids: Vec::new(), et: vec![BE { a: 0, b: 0, mask: 0, c: 0, gen: 0 }; if mode == "dedup" { 0 } else { (max_b / 4).next_power_of_two().max(64) }], gen: 0, coll: Vec::with_capacity(max_b), coll2: Vec::with_capacity(max_b), cnt: Vec::new(), dict: Vec::with_capacity(max_b), sorted: Vec::with_capacity(2 * max_b), enc: Vec::with_capacity(32 * max_b), arena: vec![0u8; arena], used: 0 };
            s.tab.reserve(4 * max_b); for b in s.arena.iter_mut().step_by(4096) { *b = 1; }
            s
        }).collect();
        let next = AtomicUsize::new(0);
        let outs: Vec<Mutex<Out>> = (0..n_units).map(|_| Mutex::new(Out::default())).collect();
        let (tus, sus) = (tb.as_ref().map(|b| &b.1), sb.as_ref().map(|b| &b.1));
        crate::perf_on(true);
        let tw = Instant::now();
        let busy: Vec<f64> = std::thread::scope(|sc| {
            let hs: Vec<_> = scr.iter_mut().enumerate().map(|(ti, s)| { let (next, outs, d) = (&next, &outs, &d); sc.spawn(move || {
                let mut busy = 0f64;
                loop {
                    let i = next.fetch_add(1, Ordering::Relaxed); if i >= n_units { break; }
                    let t = Instant::now();
                    let o = match mode { "dedup" => run_target(&tus.unwrap()[i], false, s, ti), "t" => run_target(&tus.unwrap()[i], true, s, ti), "s" => run_source(&sus.unwrap()[i], d, s, ti), m => panic!("mode {m}") };
                    busy += t.elapsed().as_secs_f64();
                    *outs[i].lock().unwrap() = o;
                }
                busy
            }) }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).collect()
        });
        let wall = tw.elapsed().as_secs_f64();
        crate::perf_on(false);
        let outs: Vec<Out> = outs.into_iter().map(|m| m.into_inner().unwrap()).collect();
        let new: u64 = outs.iter().map(|o| o.new).sum();
        let bundles: u64 = outs.iter().map(|o| o.bundles).sum();
        let bytes: usize = outs.iter().map(|o| o.len).sum();
        let table: u64 = outs.iter().map(|o| o.table).sum();
        let (t_ins, t_enc): (u64, u64) = (outs.iter().map(|o| o.t_ins).sum(), outs.iter().map(|o| o.t_enc).sum());
        let sum_busy: f64 = busy.iter().sum();
        let load = std::fs::read_to_string("/proc/loadavg").unwrap_or_default();
        let e = d.n_edges as f64;
        println!("  RUN {mode} threads {threads:>2} split {split:>8}: {n_units} units; WALL {wall:.3} s ({:.2} ns an edge); busy {sum_busy:.2} s, idle {:.1}%; summed over units: insert {:.2} s ({:.2} ns an edge), encode {:.2} s ({:.2} ns an edge){}{}; load {}",
            wall * 1e9 / e, 100.0 * (1.0 - sum_busy / (wall * threads as f64)), t_ins as f64 / 1e9, t_ins as f64 / e, t_enc as f64 / 1e9, t_enc as f64 / e,
            if mode == "s" { String::new() } else if new == 6_735_699 { format!("; new {new} (OK)") } else { format!("; new {new} (MISMATCH)") },
            if mode == "dedup" { String::new() } else { format!("; {bundles} bundles ({:.2} edges each; in memory at 32 B an entry {:.2} B an edge dense, {:.2} at the tables' final sizes); block {:.3} B an edge", e / bundles.max(1) as f64, 32.0 * bundles as f64 / e, table as f64 / e, bytes as f64 / e) },
            load.split_whitespace().next().unwrap_or("?"));
        // The decode check, once per mode (every run with `EB_CHECK_ALL`).
        if mode != "dedup" && (!checked.iter().any(|m| m == mode) || std::env::var_os("EB_CHECK_ALL").is_some()) {
            let rp: Vec<(u16, u32)> = if mode == "s" { sus.unwrap().iter().map(|u| (u.region, u.part)).collect() } else { tus.unwrap().iter().map(|u| (u.region, u.part)).collect() };
            check(mode, &d, &rp, tus.map_or(&[][..], |v| &v[..]), &outs, &scr);
            checked.push(mode.to_string());
        }
        if mode == "t" { last_t = Some((outs, scr)); }
    }
}
