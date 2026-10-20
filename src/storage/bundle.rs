//! EDGE BUNDLES (plans/architecture.md, "The recorded graph"): the
//! recorded edges as stored, ID-free. A BUNDLE is the edges of one source
//! STATE KEY (shape and key: every cell of it) into one TARGET KEY at one
//! transfer and one cell offset, inside one target region: `(source shape
//! and key, transfer, offset, target key) -> mask` over the TARGET region's
//! cells (an edge into target cell `c` leaves source cell `c - offset`).
//!
//! A BLOCK holds a target region's bundles (a frame's, or a segment): its
//! distinct target keys and source (shape, key)s sorted and delta-coded
//! (its dictionaries; ranks, not ids), then the bundles as a sorted trie in
//! `ORDER` (`put_record`), zstd -1 at rest. `ORDER` is mask first (equal
//! masks together), then offset, transfer, source, target: measured
//! smallest of the 48 orders with absolute masks (~0.08 B an edge).

use anyhow::{anyhow, ensure, Result};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::visited::Key;
use crate::search::pos_graph::{GRID, NO_CELL};

/// One bundle; `mask` over the target region's 64 cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Bundle {
    pub(crate) s_shape: u32,
    pub(crate) s_key: Key,
    /// The global transfer id (`edges::XferTable`).
    pub(crate) x: u32,
    /// The cell offset (`offset`).
    pub(crate) o: u32,
    pub(crate) t_key: Key,
    pub(crate) mask: u64,
}

const _: () = assert!(super::GEOMETRY.words == 1, "bundles hold a u64 mask: 8 x 8 storage regions");

/// The offset codes: target cell minus source cell, `(dx + 1024) << 11 |
/// (dy + 1024)`, between two cells (below `TO_NONE`); into `NO_CELL`,
/// `TO_NONE | source cell` (the target's cell cannot give it back); out of
/// `NO_CELL` `FROM_NONE`; `NONE_NONE` for neither.
pub(crate) const TO_NONE: u32 = 1 << 22;
pub(crate) const FROM_NONE: u32 = 1 << 23;
pub(crate) const NONE_NONE: u32 = 1 << 24;

/// The offset code of an edge from cell `s` into cell `t`.
#[inline]
pub(crate) fn offset(s: u32, t: u32) -> u32 {
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
pub(crate) fn source_cell(t: u32, o: u32) -> u32 {
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

/// A cell-to-cell offset code `o` (below `TO_NONE`) from target slot
/// `t_slot` back to source slot `s_slot` (neither `NO_CELL`'s) as ONE SHIFT
/// `k` of the regions' 8 x 8 grids (source index = target index + `k`): the
/// target cells whose source is in the source region, and `k`; `None`: none.
#[inline]
pub(crate) fn shift_into(t_slot: u32, s_slot: u32, o: u32) -> Option<(u64, i32)> {
    let geo = super::GEOMETRY;
    debug_assert!(o < TO_NONE && t_slot != geo.no_cell_slot() && s_slot != geo.no_cell_slot());
    let (side, pr) = (geo.side as i32, geo.per_row);
    let (dx, dy) = ((o >> 11) as i32 - 1024, (o & 2047) as i32 - 1024);
    // A source cell is its target cell plus (ddx, ddy) in the regions' grids.
    let ddx = ((t_slot % pr) as i32 - (s_slot % pr) as i32) * side - dx;
    let ddy = ((t_slot / pr) as i32 - (s_slot / pr) as i32) * side - dy;
    if ddx.abs() >= side || ddy.abs() >= side {
        return None;
    }
    // The columns and rows the shift moves off the grid.
    const COL: u64 = 0x0101_0101_0101_0101;
    let cols = if ddx >= 0 { (0xffu64 << (8 - ddx)) & 0xff } else { (1u64 << -ddx) - 1 } * COL;
    let rows = match ddy {
        0 => 0,
        d if d > 0 => !0u64 << (8 * (8 - d)),
        d => (1u64 << (8 * -d)) - 1,
    };
    Some((!(cols | rows), 8 * ddy + ddx))
}

/// Cells `m` moved by `k` (`shift_into`; `m` inside its cells).
#[inline]
pub(crate) fn shift(m: u64, k: i32) -> u64 {
    if k >= 0 {
        m << k
    } else {
        m >> -k
    }
}

/// The SOURCE cells of target cells `mask` (not empty) of slot `t_slot` at
/// offset code `o`, all in slot `s_slot`'s region: the shifted mask and the
/// shift. `None` when one leaves it: no bundle of a source there holds it.
#[inline]
pub(crate) fn source_cells(t_slot: u32, s_slot: u32, o: u32, mask: u64) -> Option<(u64, i32)> {
    let (inside, k) = shift_into(t_slot, s_slot, o)?;
    (mask & !inside == 0).then(|| (shift(mask, k), k))
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
pub(crate) fn get_off(b: &[u8], pos: &mut usize) -> u32 {
    let x = b[*pos];
    *pos += 1;
    if x != 0 {
        let (dx, dy) = ((x >> 4) as i32 - 8, (x & 15) as i32 - 8);
        ((dx + 1024) as u32) << 11 | (dy + 1024) as u32
    } else {
        get_varint(b, pos) as u32
    }
}

/// A field of a block's sort order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum F {
    /// The source (its rank in the block's source dictionary).
    S,
    /// The target key (its rank in the target dictionary).
    T,
    X,
    O,
    M,
}

pub(crate) const ORDER: [F; 5] = [F::M, F::O, F::X, F::S, F::T];

/// A bundle as a block's record holds it: source and target as ranks into
/// the block's dictionaries.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Ranked {
    pub(crate) s: u32,
    pub(crate) t: u32,
    pub(crate) x: u32,
    pub(crate) o: u32,
    pub(crate) mask: u64,
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

/// One record of a block in `order` (`vals`, the fields in that order),
/// after `prev`: its level byte (the first field that changed), that
/// field's delta, the later fields raw (an offset short-coded, a mask its
/// 8 bytes).
#[inline]
fn put_record(out: &mut Vec<u8>, order: &[F; 5], prev: Option<&[u64; 5]>, vals: &[u64; 5]) {
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
}

/// Encode `bundles` (distinct, any order) as a raw block in `order` into
/// `out` (cleared): the reference the grouped `Encoder` is checked against.
#[cfg(test)]
pub(crate) fn encode(bundles: &[Bundle], order: &[F; 5], out: &mut Vec<u8>) {
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
    for (k, vals) in recs.iter().enumerate() {
        put_record(out, order, k.checked_sub(1).map(|j| &recs[j]), vals);
    }
}

/// A bundle as the region phase holds it: its target named by an id (the
/// region table's entry, whose key the encoder looks up), its source by
/// its index in the route's source list.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Tb {
    pub(crate) t: u32,
    pub(crate) s: u32,
    pub(crate) x: u32,
    pub(crate) o: u32,
    pub(crate) mask: u64,
}

/// The block encoder of the region phase (writes `encode`'s format), its
/// scratch reused block to block: targets and sources ranked through dense
/// arrays over their ids, the records GROUPED by (mask, offset, transfer) -
/// a block has few, ~1 per 20-50 bundles - so no sort of the whole block.
#[derive(Default)]
pub(crate) struct Encoder {
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
    /// Buffers back to default if any has room for more than `keep` items.
    pub(crate) fn bound(&mut self, keep: usize) {
        let big = [self.t_rank.capacity(), self.tids.capacity(), self.s_perm.capacity(), self.item_g.capacity(), self.at.capacity(), self.st.capacity(), self.groups.capacity(), self.gid.capacity()].into_iter().any(|c| c > keep);
        if big {
            *self = Encoder::default();
        }
    }

}

const _: () = assert!(matches!(ORDER, [F::M, F::O, F::X, F::S, F::T]), "Encoder groups ORDER's fields");

impl Encoder {
    /// `bundles` (distinct, any order) as a raw block in `ORDER` into `out`
    /// (cleared), targets and sources numbered as met: the waiting bundles,
    /// in the region phase and again at the frame's end (`route::reencode`).
    pub(crate) fn encode_bundles(&mut self, bundles: &[Bundle], out: &mut Vec<u8>) {
        let mut t_id: rustc_hash2::FxHashMap<Key, u32> = Default::default();
        let mut s_id: rustc_hash2::FxHashMap<(u32, Key), u32> = Default::default();
        let (mut t_keys, mut s_list): (Vec<Key>, Vec<(u32, Key)>) = (Vec::new(), Vec::new());
        let items: Vec<Tb> = bundles
            .iter()
            .map(|b| {
                let n = t_keys.len() as u32;
                let t = *t_id.entry(b.t_key).or_insert(n);
                if t == n {
                    t_keys.push(b.t_key);
                }
                let n = s_list.len() as u32;
                let s = *s_id.entry((b.s_shape, b.s_key)).or_insert(n);
                if s == n {
                    s_list.push((b.s_shape, b.s_key));
                }
                Tb { t, s, x: b.x, o: b.o, mask: b.mask }
            })
            .collect();
        self.encode(&items, t_keys.len(), |l| t_keys[l as usize], &s_list, out);
    }

    /// `items` (distinct) as a raw block in `ORDER` into `out` (cleared):
    /// target ids below `n_ids`, their keys `t_key(id)`; source indices into
    /// `sources` (distinct; each one some item's).
    pub(crate) fn encode(&mut self, items: &[Tb], n_ids: usize, t_key: impl Fn(u32) -> Key, sources: &[(u32, Key)], out: &mut Vec<u8>) {
        out.clear();
        if self.t_rank.len() < n_ids {
            self.t_rank.resize(n_ids, u32::MAX);
        }
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
            // The group's first record after the last group's; the rest
            // share its mask, offset and transfer: `put_record` at level 3
            // (the source's delta, the target) or 4 (the target's delta).
            let vals = |st: u64| [mask, ox >> 32, ox & 0xffff_ffff, st >> 32, st & 0xffff_ffff];
            put_record(out, &ORDER, prev.as_ref(), &vals(run[0]));
            for w in run.windows(2) {
                let ((ps, pt), (s, t)) = ((w[0] >> 32, w[0] & 0xffff_ffff), (w[1] >> 32, w[1] & 0xffff_ffff));
                if s == ps {
                    out.push(4);
                    put_varint(out, t - pt);
                } else {
                    out.push(3);
                    put_varint(out, s - ps);
                    put_varint(out, t);
                }
            }
            prev = Some(vals(run[run.len() - 1]));
            a += len as usize;
        }
    }
}

/// A raw block, its dictionaries read; `records` walks its bundles.
pub(crate) struct Block<'a> {
    pub(crate) t_keys: Vec<Key>,
    pub(crate) sources: Vec<(u32, Key)>,
    body: &'a [u8],
    n: usize,
    order: [F; 5],
}

impl<'a> Block<'a> {
    pub(crate) fn open(raw: &'a [u8], order: &[F; 5]) -> Result<Self> {
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

    #[cfg(test)]
    pub(crate) fn is_empty(&self) -> bool {
        self.n == 0
    }

    /// Every record, in the block's order.
    pub(crate) fn records(&self, mut f: impl FnMut(&Ranked)) -> Result<()> {
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
    pub(crate) fn bundles(&self, mut f: impl FnMut(Bundle)) -> Result<()> {
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

    /// `source_cells` is `source_cell` cell by cell: every cell of a mask
    /// shifted to its source when all of them stay in the source region,
    /// `None` otherwise.
    #[test]
    fn source_cells_shift_the_grid() {
        let geo = super::super::GEOMETRY;
        let mut v = 11u64;
        let (mut some, mut none) = (0, 0);
        for _ in 0..20000 {
            let t_slot = (lcg(&mut v) % (geo.slots as u64 - 1)) as u32;
            let (rx, ry) = ((t_slot % geo.per_row) as i32, (t_slot / geo.per_row) as i32);
            let s_slot = ((ry + (lcg(&mut v) % 3) as i32 - 1).clamp(0, geo.per_row as i32 - 1) * geo.per_row as i32 + (rx + (lcg(&mut v) % 3) as i32 - 1).clamp(0, geo.per_row as i32 - 1)) as u32;
            let (dx, dy) = ((lcg(&mut v) % 17) as i32 - 8, (lcg(&mut v) % 17) as i32 - 8);
            let o = ((dx + 1024) as u32) << 11 | (dy + 1024) as u32;
            let mut mask = lcg(&mut v) & lcg(&mut v) & lcg(&mut v);
            if lcg(&mut v) % 2 == 0 {
                // Only the cells whose source is in the region.
                mask &= (0..64u32).filter(|&c| geo.slot_local(source_cell(geo.cell(t_slot, c), o)).0 == s_slot).fold(0, |m, c| m | 1u64 << c);
            }
            if mask == 0 {
                continue;
            }
            let each: Option<u64> = (0..64u32).filter(|&c| mask >> c & 1 != 0).try_fold(0u64, |m, c| {
                let (slot, local) = geo.slot_local(source_cell(geo.cell(t_slot, c), o));
                (slot == s_slot).then_some(m | 1u64 << local)
            });
            match source_cells(t_slot, s_slot, o, mask) {
                Some((src, k)) => {
                    assert_eq!(Some(src), each, "slots {t_slot} {s_slot} offset ({dx}, {dy}) mask {mask:x}");
                    for c in (0..64u32).filter(|&c| mask >> c & 1 != 0) {
                        assert_eq!(geo.slot_local(source_cell(geo.cell(t_slot, c), o)).1 as i32, c as i32 + k);
                    }
                    some += 1;
                }
                None => {
                    assert!(each.is_none(), "slots {t_slot} {s_slot} offset ({dx}, {dy}) mask {mask:x}: refused");
                    none += 1;
                }
            }
        }
        assert!(some > 1000 && none > 1000, "{some} shifted, {none} refused");
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
            let stored = super::super::file::compress(&raw, &mut z).unwrap();
            let mut back = Vec::new();
            super::super::file::decompress(&stored, raw.len(), &mut d, &mut back).unwrap();
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
            let mut shuffled = bundles.clone();
            shuffled.reverse();
            enc.encode_bundles(&shuffled, &mut b2);
            assert_eq!(a, b2, "encode_bundles and encode agree");
        }
        let mut raw = Vec::new();
        encode(&[], &ORDER, &mut raw);
        let b = Block::open(&raw, &ORDER).unwrap();
        assert!(b.is_empty());
        b.records(|_| panic!("no records")).unwrap();
    }

}
