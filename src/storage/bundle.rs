//! EDGE BUNDLES (plans/edges-target.md): the recorded edges as they are
//! stored, ID-free. An edge is (source state, target state, transfer), a
//! state its shape, exact key and cell (`storage::Sk`). A BUNDLE is the
//! edges of one source STATE KEY (shape and key: every cell of it) into one
//! TARGET KEY at one transfer and one cell offset, inside one target
//! region: `(source shape and key, transfer, offset, target key) -> mask`,
//! the mask over the TARGET region's cells (an edge into target cell `c`
//! leaves source cell `c - offset`).
//!
//! A BLOCK holds a target region's bundles (a frame's, or a segment of
//! it): the block's distinct target keys and source (shape, key)s sorted
//! and delta-coded (its dictionaries; ranks, not ids), then the bundles as
//! a sorted trie in `ORDER` - per bundle a level byte (the first field that
//! changed), that field's delta, the later fields raw - and zstd -1 over
//! it at rest. `ORDER` is mask first (bundles with equal masks together),
//! then offset, transfer, source, target: measured smallest of the 48
//! orders with absolute masks (bench/dedup DESIGNS O3: 0.079 B an edge on
//! room (6,2) 100% f57, against 3.15 for the storage-v2 unit blocks).

use anyhow::{anyhow, ensure, Result};

use super::visited::Key;
use crate::search::pos_graph::{GRID, NO_CELL};

/// One bundle (module doc). `mask`: the target cells, over the region's
/// 64 (storage regions are 8 x 8 cells: `GEOMETRY.words == 1`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bundle {
    pub s_shape: u32,
    pub s_key: Key,
    /// The global transfer id (`edges::XferTable`).
    pub x: u32,
    /// The cell offset (`offset`).
    pub o: u32,
    pub t_key: Key,
    pub mask: u64,
}

const _: () = assert!(super::GEOMETRY.words == 1, "bundles hold a u64 mask: 8 x 8 storage regions");

/// The offset codes: target cell minus source cell, `(dx + 1024) << 11 |
/// (dy + 1024)`, between two cells (below `TO_NONE`); into `NO_CELL`,
/// `TO_NONE | source cell` (the target's cell cannot give it back); out of
/// `NO_CELL` `FROM_NONE`; `NONE_NONE` for neither.
pub const TO_NONE: u32 = 1 << 22;
pub const FROM_NONE: u32 = 1 << 23;
pub const NONE_NONE: u32 = 1 << 24;

/// The offset code of an edge from cell `s` into cell `t`.
#[inline]
pub fn offset(s: u32, t: u32) -> u32 {
    match (s == NO_CELL, t == NO_CELL) {
        (false, false) => {
            let g = GRID as u32;
            let dx = (t % g) as i32 - (s % g) as i32;
            let dy = (t / g) as i32 - (s / g) as i32;
            ((dx + 1024) as u32) << 11 | (dy + 1024) as u32
        }
        (false, true) => TO_NONE | s,
        (true, false) => FROM_NONE,
        (true, true) => NONE_NONE,
    }
}

/// The source cell of an edge into cell `t` at offset code `o`.
#[inline]
pub fn source_cell(t: u32, o: u32) -> u32 {
    if o < TO_NONE {
        let g = GRID as i32;
        let (dx, dy) = ((o >> 11) as i32 - 1024, (o & 2047) as i32 - 1024);
        ((t as i32 / g - dy) * g + (t as i32 % g - dx)) as u32
    } else if o & !(TO_NONE - 1) == TO_NONE {
        o & (TO_NONE - 1)
    } else {
        NO_CELL
    }
}

/// An offset in a block: `(dx + 8) << 4 | (dy + 8)` for |d| <= 7 (never 0),
/// else 0 and the code as a varint.
#[inline]
pub(crate) fn put_off(out: &mut Vec<u8>, o: u32) {
    if o < TO_NONE {
        let (dx, dy) = ((o >> 11) as i32 - 1024, (o & 2047) as i32 - 1024);
        if dx.abs() <= 7 && dy.abs() <= 7 {
            out.push(((dx + 8) << 4 | (dy + 8)) as u8);
            return;
        }
    }
    out.push(0);
    put_varint(out, o as u64);
}

#[inline]
fn get_off(b: &[u8], pos: &mut usize) -> u32 {
    let x = b[*pos];
    *pos += 1;
    if x != 0 {
        let (dx, dy) = ((x >> 4) as i32 - 8, (x & 15) as i32 - 8);
        ((dx + 1024) as u32) << 11 | (dy + 1024) as u32
    } else {
        get_varint(b, pos) as u32
    }
}

#[inline]
pub(crate) fn put_varint(out: &mut Vec<u8>, mut v: u64) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
}

#[inline]
pub(crate) fn get_varint(b: &[u8], pos: &mut usize) -> u64 {
    let mut v = 0u64;
    let mut shift = 0;
    loop {
        let x = b[*pos];
        *pos += 1;
        v |= ((x & 0x7f) as u64) << shift;
        if x & 0x80 == 0 {
            return v;
        }
        shift += 7;
    }
}

#[inline]
pub(crate) fn put_varint128(out: &mut Vec<u8>, mut v: u128) {
    while v >= 0x80 {
        out.push((v as u8) | 0x80);
        v >>= 7;
    }
    out.push(v as u8);
}

#[inline]
pub(crate) fn get_varint128(b: &[u8], pos: &mut usize) -> u128 {
    let mut v = 0u128;
    let mut shift = 0;
    loop {
        let x = b[*pos];
        *pos += 1;
        v |= ((x & 0x7f) as u128) << shift;
        if x & 0x80 == 0 {
            return v;
        }
        shift += 7;
    }
}

#[inline]
fn wide(k: Key) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

#[inline]
fn narrow(v: u128) -> Key {
    ((v >> 64) as u64, v as u64)
}

/// A field of a block's sort order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum F {
    /// The source (its rank in the block's source dictionary).
    S,
    /// The target key (its rank in the target dictionary).
    T,
    /// The transfer.
    X,
    /// The offset.
    O,
    /// The mask.
    M,
}

/// THE ORDER of a stored block (module doc).
pub const ORDER: [F; 5] = [F::M, F::O, F::X, F::S, F::T];

/// A bundle as a block's record holds it: source and target as ranks into
/// the block's dictionaries.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ranked {
    pub s: u32,
    pub t: u32,
    pub x: u32,
    pub o: u32,
    pub mask: u64,
}

impl Ranked {
    #[inline]
    fn field(&self, f: F) -> u64 {
        match f {
            F::S => self.s as u64,
            F::T => self.t as u64,
            F::X => self.x as u64,
            F::O => self.o as u64,
            F::M => self.mask,
        }
    }

    #[inline]
    fn set(&mut self, f: F, v: u64) {
        match f {
            F::S => self.s = v as u32,
            F::T => self.t = v as u32,
            F::X => self.x = v as u32,
            F::O => self.o = v as u32,
            F::M => self.mask = v,
        }
    }
}

/// A block's dictionaries: the target keys, then the sources, each sorted
/// and delta-coded (a source's shape as `shape + 1`, or 0 for the previous
/// one's, its key raw or as a delta).
pub(crate) fn write_dicts(t: &[Key], s: &[(u32, Key)], out: &mut Vec<u8>) {
    put_varint(out, t.len() as u64);
    let mut prev = 0u128;
    for (i, &k) in t.iter().enumerate() {
        let w = wide(k);
        put_varint128(out, if i == 0 { w } else { w - prev });
        prev = w;
    }
    put_varint(out, s.len() as u64);
    let (mut ps, mut pk) = (0u32, 0u128);
    for (i, &(shape, k)) in s.iter().enumerate() {
        let w = wide(k);
        if i > 0 && shape == ps {
            out.push(0);
            put_varint128(out, w - pk);
        } else {
            put_varint(out, shape as u64 + 1);
            put_varint128(out, w);
        }
        (ps, pk) = (shape, w);
    }
}

/// Encode `bundles` (distinct, any order) as a raw block in `order` into
/// `out` (cleared).
pub fn encode(bundles: &[Bundle], order: &[F; 5], out: &mut Vec<u8>) {
    out.clear();
    let mut t: Vec<Key> = bundles.iter().map(|b| b.t_key).collect();
    t.sort_unstable();
    t.dedup();
    let mut s: Vec<(u32, Key)> = bundles.iter().map(|b| (b.s_shape, b.s_key)).collect();
    s.sort_unstable();
    s.dedup();
    write_dicts(&t, &s, out);
    // The records sorted by the order's fields (ranks through hash maps:
    // a dictionary's order is its keys').
    let t_rank: rustc_hash2::FxHashMap<Key, u32> = t.iter().enumerate().map(|(i, &k)| (k, i as u32)).collect();
    let s_rank: rustc_hash2::FxHashMap<(u32, Key), u32> = s.iter().enumerate().map(|(i, &k)| (k, i as u32)).collect();
    let mut recs: Vec<[u64; 5]> = bundles
        .iter()
        .map(|b| {
            let r = Ranked { s: s_rank[&(b.s_shape, b.s_key)], t: t_rank[&b.t_key], x: b.x, o: b.o, mask: b.mask };
            order.map(|f| r.field(f))
        })
        .collect();
    recs.sort_unstable();
    debug_assert!(recs.windows(2).all(|w| w[0] != w[1]), "bundles distinct");
    put_varint(out, recs.len() as u64);
    let mut prev: Option<[u64; 5]> = None;
    for vals in &recs {
        let l = match prev {
            None => 0,
            Some(p) => (0..5).find(|&i| vals[i] != p[i]).expect("distinct records"),
        };
        out.push(l as u8);
        for i in l..5 {
            match order[i] {
                F::O => put_off(out, vals[i] as u32),
                F::M => out.extend_from_slice(&vals[i].to_le_bytes()),
                _ => match prev {
                    Some(p) if i == l => put_varint(out, vals[i] - p[i]),
                    _ => put_varint(out, vals[i]),
                },
            }
        }
        prev = Some(*vals);
    }
}

/// A bundle as the region phase holds it: its target named by an id (the
/// region table's entry, whose key the encoder looks up), its source by
/// its index in the route's source list.
#[derive(Clone, Copy, Debug)]
pub struct Tb {
    pub t: u32,
    pub s: u32,
    pub x: u32,
    pub o: u32,
    pub mask: u64,
}

/// The block encoder of the region phase (`ORDER`, the format `encode`
/// writes), its scratch reused block to block: targets and sources ranked
/// through dense arrays over their ids; the records GROUPED by (mask,
/// offset, transfer) - a block has few (room (6,2) f57: 21k-135k for 1-2.6M
/// bundles) - the groups sorted, each group's (source, target) pairs
/// sorted apart: no sort of the whole block.
#[derive(Default)]
pub struct Encoder {
    t_rank: Vec<u32>,
    tids: Vec<u32>,
    s_perm: Vec<u32>,
    gid: rustc_hash2::FxHashMap<(u64, u64), u32>,
    groups: Vec<(u64, u64, u32)>,
    item_g: Vec<u32>,
    at: Vec<u32>,
    st: Vec<u64>,
}

impl Encoder {
    /// Its buffers back to their default size if any holds room for more
    /// than `keep` items (`route::Scratch::bound`).
    pub fn bound(&mut self, keep: usize) {
        let big = [self.t_rank.capacity(), self.tids.capacity(), self.s_perm.capacity(), self.item_g.capacity(), self.at.capacity(), self.st.capacity(), self.groups.capacity(), self.gid.capacity()].into_iter().any(|c| c > keep);
        if big {
            *self = Encoder::default();
        }
    }

    /// Bytes its buffers hold (`--phases`' `[wavemem]`).
    pub fn alloc_bytes(&self) -> usize {
        4 * (self.t_rank.capacity() + self.tids.capacity() + self.s_perm.capacity() + self.item_g.capacity() + self.at.capacity()) + 8 * self.st.capacity() + 20 * self.groups.capacity() + 24 * self.gid.capacity()
    }
}

const _: () = assert!(matches!(ORDER, [F::M, F::O, F::X, F::S, F::T]), "Encoder groups ORDER's fields");

impl Encoder {
    /// `items` (distinct) as a raw block in `ORDER` into `out` (cleared):
    /// target ids below `n_ids`, their keys `t_key(id)`; source indices into
    /// `sources` (distinct; each one some item's).
    pub fn encode(&mut self, items: &[Tb], n_ids: usize, t_key: impl Fn(u32) -> Key, sources: &[(u32, Key)], out: &mut Vec<u8>) {
        out.clear();
        if self.t_rank.len() < n_ids {
            self.t_rank.resize(n_ids, u32::MAX);
        }
        // The targets: distinct ids, sorted by key.
        self.tids.clear();
        for it in items {
            if self.t_rank[it.t as usize] == u32::MAX {
                self.t_rank[it.t as usize] = 0;
                self.tids.push(it.t);
            }
        }
        let mut tk: Vec<(Key, u32)> = self.tids.iter().map(|&t| (t_key(t), t)).collect();
        tk.sort_unstable();
        for (r, &(_, t)) in tk.iter().enumerate() {
            self.t_rank[t as usize] = r as u32;
        }
        // The sources, by key.
        let mut order: Vec<u32> = (0..sources.len() as u32).collect();
        order.sort_unstable_by_key(|&i| sources[i as usize]);
        self.s_perm.clear();
        self.s_perm.resize(order.len(), 0);
        for (r, &i) in order.iter().enumerate() {
            self.s_perm[i as usize] = r as u32;
        }
        let t: Vec<Key> = tk.iter().map(|x| x.0).collect();
        let s: Vec<(u32, Key)> = order.iter().map(|&i| sources[i as usize]).collect();
        write_dicts(&t, &s, out);
        // The groups: (mask, o << 32 | x), numbered as met.
        self.gid.clear();
        self.groups.clear();
        self.item_g.clear();
        let mut last: Option<((u64, u64), u32)> = None;
        for it in items {
            let k = (it.mask, (it.o as u64) << 32 | it.x as u64);
            let g = match last {
                Some((l, g)) if l == k => g,
                _ => {
                    let n = self.groups.len() as u32;
                    let g = *self.gid.entry(k).or_insert(n);
                    if g == n {
                        self.groups.push((k.0, k.1, 0));
                    }
                    last = Some((k, g));
                    g
                }
            };
            self.groups[g as usize].2 += 1;
            self.item_g.push(g);
        }
        // The groups in order, each one's place; its (s << 32 | t)s there.
        let mut by: Vec<u32> = (0..self.groups.len() as u32).collect();
        by.sort_unstable_by_key(|&g| (self.groups[g as usize].0, self.groups[g as usize].1));
        self.at.clear();
        self.at.resize(self.groups.len(), 0);
        let mut n = 0u32;
        for &g in &by {
            self.at[g as usize] = n;
            n += self.groups[g as usize].2;
        }
        self.st.clear();
        self.st.resize(items.len(), 0);
        for (it, &g) in items.iter().zip(&self.item_g) {
            let p = &mut self.at[g as usize];
            self.st[*p as usize] = (self.s_perm[it.s as usize] as u64) << 32 | self.t_rank[it.t as usize] as u64;
            *p += 1;
        }
        for &t in &self.tids {
            self.t_rank[t as usize] = u32::MAX;
        }
        put_varint(out, items.len() as u64);
        let mut prev: Option<[u64; 5]> = None;
        let mut a = 0usize;
        for &g in &by {
            let (mask, ox, len) = self.groups[g as usize];
            let run = &mut self.st[a..a + len as usize];
            run.sort_unstable();
            debug_assert!(run.windows(2).all(|w| w[0] != w[1]), "bundles distinct");
            for &st in run.iter() {
                let vals = [mask, ox >> 32, ox & 0xffff_ffff, st >> 32, st & 0xffff_ffff];
                let l = match prev {
                    None => 0,
                    Some(p) => (0..5).find(|&i| vals[i] != p[i]).expect("distinct records"),
                };
                out.push(l as u8);
                for i in l..5 {
                    match ORDER[i] {
                        F::O => put_off(out, vals[i] as u32),
                        F::M => out.extend_from_slice(&vals[i].to_le_bytes()),
                        _ => match prev {
                            Some(p) if i == l => put_varint(out, vals[i] - p[i]),
                            _ => put_varint(out, vals[i]),
                        },
                    }
                }
                prev = Some(vals);
            }
            a += len as usize;
        }
    }
}

/// A raw block, its dictionaries read; `records` walks its bundles.
pub struct Block<'a> {
    pub t_keys: Vec<Key>,
    pub sources: Vec<(u32, Key)>,
    body: &'a [u8],
    n: usize,
    order: [F; 5],
}

impl<'a> Block<'a> {
    pub fn open(raw: &'a [u8], order: &[F; 5]) -> Result<Self> {
        let mut pos = 0usize;
        let nt = get_varint(raw, &mut pos) as usize;
        let mut t_keys = Vec::with_capacity(nt);
        let mut prev = 0u128;
        for i in 0..nt {
            let d = get_varint128(raw, &mut pos);
            prev = if i == 0 { d } else { prev + d };
            t_keys.push(narrow(prev));
        }
        let ns = get_varint(raw, &mut pos) as usize;
        let mut sources = Vec::with_capacity(ns);
        let (mut ps, mut pk) = (0u32, 0u128);
        for _ in 0..ns {
            let tag = get_varint(raw, &mut pos);
            let d = get_varint128(raw, &mut pos);
            if tag == 0 {
                pk += d;
            } else {
                (ps, pk) = (tag as u32 - 1, d);
            }
            sources.push((ps, narrow(pk)));
        }
        let n = get_varint(raw, &mut pos) as usize;
        ensure!(pos <= raw.len(), "an edge block's dictionaries past its end");
        Ok(Block { t_keys, sources, body: &raw[pos..], n, order: *order })
    }

    /// Bundles in the block.
    pub fn len(&self) -> usize {
        self.n
    }

    pub fn is_empty(&self) -> bool {
        self.n == 0
    }

    /// Every record, in the block's order.
    pub fn records(&self, mut f: impl FnMut(&Ranked)) -> Result<()> {
        let b = self.body;
        let mut pos = 0usize;
        let mut r = Ranked { s: 0, t: 0, x: 0, o: 0, mask: 0 };
        for k in 0..self.n {
            ensure!(pos < b.len(), "an edge block ends at its record {k} of {}", self.n);
            let l = b[pos] as usize;
            pos += 1;
            ensure!(l < 5 && (k > 0 || l == 0), "an edge block's record {k} at level {l}");
            for i in l..5 {
                let fl = self.order[i];
                let v = match fl {
                    F::O => get_off(b, &mut pos) as u64,
                    F::M => {
                        let v = u64::from_le_bytes(b[pos..pos + 8].try_into().unwrap());
                        pos += 8;
                        v
                    }
                    _ if i == l && k > 0 => r.field(fl) + get_varint(b, &mut pos),
                    _ => get_varint(b, &mut pos),
                };
                r.set(fl, v);
            }
            f(&r);
        }
        ensure!(pos == b.len(), "an edge block did not decode to its end ({pos} of {} bytes)", b.len());
        Ok(())
    }

    /// Every bundle, keys resolved.
    pub fn bundles(&self, mut f: impl FnMut(Bundle)) -> Result<()> {
        let (t, s) = (&self.t_keys, &self.sources);
        let mut bad = None;
        self.records(|r| match (t.get(r.t as usize), s.get(r.s as usize)) {
            (Some(&t_key), Some(&(s_shape, s_key))) => f(Bundle { s_shape, s_key, x: r.x, o: r.o, t_key, mask: r.mask }),
            _ => bad = Some(*r),
        })?;
        match bad {
            Some(r) => Err(anyhow!("an edge block's record names rank s {} t {} past its dictionaries ({}, {})", r.s, r.t, s.len(), t.len())),
            None => Ok(()),
        }
    }
}

/// zstd -1 of a raw block, at its size (the compressor's buffer is the
/// raw block's bound: a frame's segments, held to its end, took 5x their
/// bytes - 1.1 GB at the mod room's f69).
pub fn compress(raw: &[u8], z: &mut zstd::bulk::Compressor<'static>) -> Result<Vec<u8>> {
    let mut v = z.compress(raw)?;
    v.shrink_to_fit();
    Ok(v)
}

/// A stored block's raw bytes (`raw_len` from its header) into `out`.
pub fn decompress(stored: &[u8], raw_len: usize, z: &mut zstd::bulk::Decompressor<'static>, out: &mut Vec<u8>) -> Result<()> {
    out.clear();
    out.reserve(raw_len);
    let n = z.decompress_to_buffer(stored, out)?;
    ensure!(n == raw_len, "an edge block decompressed to {n} bytes, its header says {raw_len}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lcg(v: &mut u64) -> u64 {
        *v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        *v >> 11
    }

    /// Offsets round-trip between cells, into and out of `NO_CELL`, and in
    /// a block at any distance.
    #[test]
    fn offsets_give_the_source_back() {
        let g = GRID as u32;
        for (s, t) in [(0, 0), (5 + 7 * g, 6 + 7 * g), (100 * g + 3, 90 * g + 510), (g * g - 1, 0), (3, NO_CELL), (NO_CELL, 77), (NO_CELL, NO_CELL)] {
            let o = offset(s, t);
            assert_eq!(source_cell(t, o), s, "{s} -> {t}");
            let mut b = Vec::new();
            put_off(&mut b, o);
            let mut pos = 0;
            assert_eq!(get_off(&b, &mut pos), o);
            assert_eq!(pos, b.len());
        }
        let mut b = Vec::new();
        put_off(&mut b, offset(5, 6));
        assert_eq!(b.len(), 1, "a near offset is one byte");
    }

    /// A block decodes to its bundles in every order, wide keys and shapes
    /// included; compression round-trips.
    #[test]
    fn blocks_round_trip_in_every_order() {
        let mut v = 7u64;
        let mut bundles: Vec<Bundle> = (0..3000)
            .map(|i| {
                let s_shape = (lcg(&mut v) % 3) as u32;
                let s_key = if i % 97 == 0 { (lcg(&mut v), lcg(&mut v)) } else { (0, lcg(&mut v) % 50) };
                let t_key = if i % 89 == 0 { (1 << 40, lcg(&mut v)) } else { (0, lcg(&mut v) % 40) };
                let (sc, tc) = ((lcg(&mut v) % (GRID as u64 * GRID as u64 + 1)) as u32, (lcg(&mut v) % 64) as u32);
                Bundle { s_shape, s_key, x: (lcg(&mut v) % 300) as u32, o: offset(sc, tc), t_key, mask: lcg(&mut v) | 1 }
            })
            .collect();
        bundles.sort_unstable();
        bundles.dedup_by_key(|b| (b.s_shape, b.s_key, b.x, b.o, b.t_key));
        let orders = [ORDER, [F::S, F::X, F::O, F::T, F::M], [F::T, F::S, F::X, F::O, F::M]];
        let mut z = zstd::bulk::Compressor::new(1).unwrap();
        let mut d = zstd::bulk::Decompressor::new().unwrap();
        for order in orders {
            let mut raw = Vec::new();
            encode(&bundles, &order, &mut raw);
            let stored = compress(&raw, &mut z).unwrap();
            let mut back = Vec::new();
            decompress(&stored, raw.len(), &mut d, &mut back).unwrap();
            assert_eq!(back, raw);
            let mut got = Vec::new();
            Block::open(&back, &order).unwrap().bundles(|b| got.push(b)).unwrap();
            got.sort_unstable();
            assert_eq!(got, bundles, "{order:?}");
        }
        // The region phase's encoder writes what `encode` does.
        let mut ids: Vec<Key> = bundles.iter().map(|b| b.t_key).collect();
        ids.sort_unstable();
        ids.dedup();
        let mut srcs: Vec<(u32, Key)> = bundles.iter().map(|b| (b.s_shape, b.s_key)).collect();
        srcs.sort_unstable();
        srcs.dedup();
        srcs.reverse();
        let items: Vec<Tb> = bundles
            .iter()
            .map(|b| Tb { t: ids.binary_search(&b.t_key).unwrap() as u32 * 3 + 1, s: srcs.iter().position(|&x| x == (b.s_shape, b.s_key)).unwrap() as u32, x: b.x, o: b.o, mask: b.mask })
            .collect();
        let (mut a, mut b2) = (Vec::new(), Vec::new());
        encode(&bundles, &ORDER, &mut a);
        let mut enc = Encoder::default();
        for _ in 0..2 {
            enc.encode(&items, ids.len() * 3 + 1, |t| ids[(t as usize - 1) / 3], &srcs, &mut b2);
            assert_eq!(a, b2, "Encoder and encode agree");
        }
        let mut raw = Vec::new();
        encode(&[], &ORDER, &mut raw);
        let b = Block::open(&raw, &ORDER).unwrap();
        assert!(b.is_empty());
        b.records(|_| panic!("no records")).unwrap();
    }
}
