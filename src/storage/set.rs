//! THE ONE STATE SET (plans/keys-only.md): per storage region a TABLE of
//! entries - an exact key (the state without its position) and the mask of
//! its cells in the region. It is the visited set, the frontier and the
//! next layer, three copies of one type:
//!
//!   * INDEXED (a hash index over the keys): insert or OR, membership - the
//!     visited set, and the next layer while a wave builds it;
//!   * FROZEN (the entries sorted by key, no index): iteration in (region,
//!     key) order, membership by binary search - the frontier, a frame's
//!     layer read from its file, the next layer once its wave ends.
//!
//! Lean by construction, so every use gains alike:
//!
//!   * a key is stored in the bytes its table's largest key needs (`kw`, 1
//!     to 16: the mod room's 67-bit keys take 9 B, a 40-bit shape's 5 B,
//!     a table holding provisional keys 16); a wider key repacks the table;
//!   * the index is open addressing over `u32` slots - the entry and, in the
//!     bits the table's size leaves, a tag of the key's hash, so a probe
//!     reads a key only on a tag match - at a load between 0.5 and 0.85
//!     (non-power-of-two capacity), ~5.6 B an entry;
//!   * the entries' arrays are chunks of 32768 entries: a table grows
//!     without a copy, and holds part of one chunk unused at most.

use anyhow::{anyhow, ensure, Result};

use super::visited::Key;
use super::{Sk, GEOMETRY};
use celeste_engine::exact::slot_hash;

const _: () = assert!(GEOMETRY.words == 1, "a set holds a u64 mask an entry: 8 x 8 storage regions");

#[inline]
fn wide(k: Key) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

#[inline]
fn narrow(w: u128) -> Key {
    ((w >> 64) as u64, w as u64)
}

/// The bytes key `w` needs (at least 1).
#[inline]
fn bytes_of(w: u128) -> usize {
    (16 - (w.leading_zeros() / 8) as usize).max(1)
}

/// The most of an index's capacity its entries fill before it grows, and
/// the fill it grows to.
const MAX_LOAD: f64 = 0.85;
const GROW_LOAD: f64 = 0.5;

/// Room a table's last chunk grows by when full (an eighth of it, at least
/// 64 entries; at most the chunk).
#[inline]
fn grown(len: usize, need: usize) -> usize {
    need.max(len + (len / 8).max(64))
}

/// A table's hash index: `u32` slots, 0 empty, else `tag << bits | entry + 1`
/// (`bits` the entry's width at this capacity, the tag the low bits of the
/// key's hash); the slot of a hash at `h * cap >> 64`, linear probing.
#[derive(Clone, Default)]
struct Index {
    slots: Vec<u32>,
    bits: u32,
}

impl Index {
    fn with_cap(cap: usize) -> Index {
        assert!(cap < 1 << 31, "a region table of {cap} index slots");
        Index { slots: vec![0; cap], bits: 32 - (cap as u32).leading_zeros() }
    }

    #[inline]
    fn home(&self, h: u64) -> usize {
        ((h as u128 * self.slots.len() as u128) >> 64) as usize
    }

    #[inline]
    fn tag(&self, h: u64) -> u32 {
        (h as u32) & (u32::MAX >> self.bits)
    }

    #[inline]
    fn next(&self, i: usize) -> usize {
        if i + 1 == self.slots.len() {
            0
        } else {
            i + 1
        }
    }
}

/// Entries a chunk of a table's arrays holds (`RegionTable`): a table grows
/// by chunks, never copied.
const CH_BITS: u32 = 15;
const CH: usize = 1 << CH_BITS;

/// ONE REGION'S ENTRIES (module doc). The keys and masks live in CHUNKS of
/// `CH` entries (the last one growing up to `CH`): a table that grows a few
/// percent a frame - the visited set's - is never copied, and holds at
/// most part of one chunk unused (a whole-array growth copied the visited
/// set every frame, both copies alive: 3.4 GB at the mod room's f77).
#[derive(Clone)]
pub struct RegionTable {
    /// Bytes a key takes.
    kw: usize,
    /// Per chunk its keys, `kw` bytes each, then 16 zero bytes (a key is
    /// read as 16).
    keys: Vec<Vec<u8>>,
    masks: Vec<Vec<u64>>,
    len: usize,
    /// `None`: frozen (sorted by key).
    index: Option<Index>,
}

impl Default for RegionTable {
    fn default() -> Self {
        RegionTable::new()
    }
}

impl RegionTable {
    /// An empty indexed table.
    pub fn new() -> Self {
        RegionTable { kw: 1, keys: Vec::new(), masks: Vec::new(), len: 0, index: Some(Index::default()) }
    }

    /// An empty frozen table.
    pub fn frozen() -> Self {
        RegionTable { kw: 1, keys: Vec::new(), masks: Vec::new(), len: 0, index: None }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn is_frozen(&self) -> bool {
        self.index.is_none()
    }

    /// Entry `e`'s key.
    #[inline]
    pub fn key(&self, e: u32) -> Key {
        narrow(self.wkey(e as usize))
    }

    #[inline]
    fn wkey(&self, e: usize) -> u128 {
        let c = &self.keys[e >> CH_BITS];
        let o = (e & (CH - 1)) * self.kw;
        let w = u128::from_le_bytes(c[o..o + 16].try_into().expect("16 bytes"));
        if self.kw == 16 {
            w
        } else {
            w & ((1u128 << (8 * self.kw)) - 1)
        }
    }

    /// Entry `e`'s cells.
    #[inline]
    pub fn mask(&self, e: u32) -> u64 {
        self.masks[e as usize >> CH_BITS][e as usize & (CH - 1)]
    }

    /// Every entry's cells, in order.
    pub fn masks(&self) -> impl Iterator<Item = u64> + '_ {
        self.masks.iter().flatten().copied()
    }

    /// Every entry `(key, cells)`, in order (a frozen table's: by key).
    pub fn iter(&self) -> impl Iterator<Item = (Key, u64)> + '_ {
        (0..self.len).map(move |e| (narrow(self.wkey(e)), self.mask(e as u32)))
    }

    /// States held.
    pub fn states(&self) -> u64 {
        self.masks().map(|m| m.count_ones() as u64).sum()
    }

    /// The entry holding `k`.
    #[inline]
    pub fn find(&self, k: Key) -> Option<u32> {
        let w = wide(k);
        match &self.index {
            Some(ix) => {
                if ix.slots.is_empty() {
                    return None;
                }
                let h = slot_hash(k);
                let (tag, mut i) = (ix.tag(h), ix.home(h));
                let low = (1u32 << ix.bits) - 1;
                loop {
                    let s = ix.slots[i];
                    if s == 0 {
                        return None;
                    }
                    if s >> ix.bits == tag {
                        let e = (s & low) - 1;
                        if self.wkey(e as usize) == w {
                            return Some(e);
                        }
                    }
                    i = ix.next(i);
                }
            }
            None => {
                let (mut lo, mut hi) = (0usize, self.len);
                while lo < hi {
                    let mid = (lo + hi) / 2;
                    match self.wkey(mid).cmp(&w) {
                        std::cmp::Ordering::Less => lo = mid + 1,
                        std::cmp::Ordering::Greater => hi = mid,
                        std::cmp::Ordering::Equal => return Some(mid as u32),
                    }
                }
                None
            }
        }
    }

    /// The cells of `k` held (0: none).
    pub fn mask_of(&self, k: Key) -> u64 {
        self.find(k).map_or(0, |e| self.mask(e))
    }

    /// OR `mask` into entry `e`; the cells it held before.
    #[inline]
    pub fn or_cells(&mut self, e: u32, mask: u64) -> u64 {
        let m = &mut self.masks[e as usize >> CH_BITS][e as usize & (CH - 1)];
        let had = *m;
        *m |= mask;
        had
    }

    /// Room in the index for `n` more entries (a merge: at once).
    fn reserve(&mut self, n: usize) {
        let need = self.len + n;
        if let Some(ix) = &self.index {
            if need as f64 > MAX_LOAD * ix.slots.len() as f64 {
                self.reindex(need);
            }
        }
    }

    /// The keys, chunk by chunk, in `kw` bytes each.
    fn chunk_keys(ws: impl Iterator<Item = u128>, kw: usize) -> Vec<Vec<u8>> {
        let mut out: Vec<Vec<u8>> = Vec::new();
        let mut cur: Vec<u8> = Vec::new();
        let mut n = 0usize;
        for w in ws {
            if n == CH {
                cur.extend_from_slice(&[0; 16]);
                out.push(std::mem::take(&mut cur));
                n = 0;
            }
            if cur.is_empty() {
                cur.reserve_exact(CH * kw + 16);
            }
            cur.extend_from_slice(&w.to_le_bytes()[..kw]);
            n += 1;
        }
        if n > 0 {
            cur.extend_from_slice(&[0; 16]);
            cur.shrink_to_fit();
            out.push(cur);
        }
        out
    }

    /// The masks in chunks.
    fn chunk_masks(ms: impl Iterator<Item = u64>) -> Vec<Vec<u64>> {
        let mut out: Vec<Vec<u64>> = Vec::new();
        for m in ms {
            if out.last().is_none_or(|c| c.len() == CH) {
                out.push(Vec::with_capacity(CH));
            }
            out.last_mut().expect("pushed").push(m);
        }
        if let Some(c) = out.last_mut() {
            c.shrink_to_fit();
        }
        out
    }

    /// The keys repacked in `kw` bytes each.
    fn repack(&mut self, kw: usize) {
        debug_assert!(kw >= self.kw && kw <= 16);
        self.keys = Self::chunk_keys((0..self.len).map(|e| self.wkey(e)), kw);
        self.kw = kw;
    }

    /// The index rebuilt for `n` entries at the growth load.
    fn reindex(&mut self, n: usize) {
        let cap = ((n.max(4) as f64 / GROW_LOAD).ceil() as usize).max(8);
        let mut ix = Index::with_cap(cap);
        for e in 0..self.len {
            Self::place(&mut ix, slot_hash(narrow(self.wkey(e))), e as u32);
        }
        self.index = Some(ix);
    }

    #[inline]
    fn place(ix: &mut Index, h: u64, e: u32) {
        let mut i = ix.home(h);
        while ix.slots[i] != 0 {
            i = ix.next(i);
        }
        ix.slots[i] = ix.tag(h) << ix.bits | (e + 1);
    }

    /// Append the entry `(k, mask)` (absent: an indexed table checks it
    /// where it debugs; a frozen one needs `k` past its last key: checked);
    /// its number.
    pub fn push(&mut self, k: Key, mask: u64) -> u32 {
        let w = wide(k);
        let e = self.len;
        assert!(e < u32::MAX as usize, "a region table of 2^32 entries");
        if self.index.is_none() {
            assert!(e == 0 || self.wkey(e - 1) < w, "a frozen table's keys out of order");
        }
        let kw = bytes_of(w);
        if kw > self.kw {
            self.repack(kw);
        }
        let i = e & (CH - 1);
        if i == 0 {
            self.keys.push(Vec::with_capacity(64 * self.kw + 16));
            self.masks.push(Vec::with_capacity(64));
        }
        let (c, m) = (self.keys.last_mut().expect("a chunk"), self.masks.last_mut().expect("a chunk"));
        if m.len() == m.capacity() {
            let n = grown(m.len(), m.len() + 1).min(CH);
            m.reserve_exact(n - m.len());
            c.reserve_exact(n * self.kw + 16 - c.len());
        }
        c.truncate(i * self.kw);
        c.extend_from_slice(&w.to_le_bytes()[..self.kw]);
        c.extend_from_slice(&[0; 16]);
        m.push(mask);
        self.len += 1;
        if let Some(ix) = &mut self.index {
            if (e + 1) as f64 > MAX_LOAD * ix.slots.len() as f64 {
                self.reindex(e + 1);
            } else {
                Self::place(ix, slot_hash(k), e as u32);
            }
        }
        e as u32
    }

    /// OR `mask` into `k`'s entry, made if absent (an indexed table): its
    /// number and the cells it held before.
    #[inline]
    pub fn insert_or(&mut self, k: Key, mask: u64) -> (u32, u64) {
        debug_assert!(!self.is_frozen());
        match self.find(k) {
            Some(e) => (e, self.or_cells(e, mask)),
            None => (self.push(k, mask), 0),
        }
    }

    /// Every entry `(k, cells)` of the frozen `other` ORed into this indexed
    /// table (made where absent), the index sized first: a merge. Calls
    /// `at(i, e)` with each of `other`'s entries' place `e` here.
    pub fn merge(&mut self, other: &RegionTable, mut at: impl FnMut(usize, u32)) {
        debug_assert!(!self.is_frozen());
        let add = other.iter().filter(|(k, _)| self.find(*k).is_none()).count();
        let widest = (0..other.len()).map(|e| bytes_of(other.wkey(e))).max().unwrap_or(1);
        if widest > self.kw {
            self.repack(widest);
        }
        self.reserve(add);
        for (i, (k, m)) in other.iter().enumerate() {
            at(i, self.insert_or(k, m).0);
        }
    }

    /// Every key `k` made `f(k)` (an indexed table: the index rebuilt if
    /// any key changed): the frame's provisional keys made final.
    pub fn rekey(&mut self, f: impl Fn(Key) -> Result<Key>) -> Result<()> {
        let keys: Vec<u128> = (0..self.len).map(|e| f(narrow(self.wkey(e))).map(wide)).collect::<Result<_>>()?;
        if keys.iter().enumerate().all(|(e, &k)| k == self.wkey(e)) {
            return Ok(());
        }
        let kw = keys.iter().map(|&w| bytes_of(w)).max().unwrap_or(1);
        self.keys = Self::chunk_keys(keys.into_iter(), kw);
        self.kw = kw;
        if self.index.is_some() {
            self.reindex(self.len);
        }
        Ok(())
    }

    /// FROZEN: the entries sorted by key, the keys in the bytes their
    /// largest needs, the index dropped, the chunks at their size.
    pub fn freeze(&mut self) {
        let mut v: Vec<(u128, u64)> = (0..self.len).map(|e| (self.wkey(e), self.mask(e as u32))).collect();
        v.sort_unstable_by_key(|x| x.0);
        debug_assert!(v.windows(2).all(|w| w[0].0 != w[1].0), "a key twice in a table");
        let kw = v.last().map_or(1, |x| bytes_of(x.0));
        self.keys = Self::chunk_keys(v.iter().map(|x| x.0), kw);
        self.masks = Self::chunk_masks(v.iter().map(|x| x.1));
        self.kw = kw;
        self.index = None;
    }

    /// A frozen table from `(key, cells)` sorted by key (a frame file's
    /// region; checked).
    pub fn from_sorted(keys: &[Key], masks: &[u64]) -> Result<RegionTable> {
        ensure!(keys.len() == masks.len(), "a region with {} keys and {} masks", keys.len(), masks.len());
        ensure!(keys.windows(2).all(|w| w[0] < w[1]), "a region's keys not ascending");
        let kw = keys.last().map_or(1, |&k| bytes_of(wide(k)));
        Ok(RegionTable { kw, keys: Self::chunk_keys(keys.iter().map(|&k| wide(k)), kw), masks: Self::chunk_masks(masks.iter().copied()), len: keys.len(), index: None })
    }

    pub fn alloc_bytes(&self) -> usize {
        self.keys.iter().map(|c| c.capacity()).sum::<usize>() + self.masks.iter().map(|c| c.capacity() * 8).sum::<usize>() + (self.keys.capacity() + self.masks.capacity()) * 24 + self.index.as_ref().map_or(0, |ix| ix.slots.capacity() * 4)
    }
}

/// A SET OF STATES: per region (ascending) its table (module doc).
#[derive(Clone, Default)]
pub struct RegionSet {
    tables: Vec<(u32, RegionTable)>,
}

impl RegionSet {
    /// Region `region`'s table, if held.
    #[inline]
    pub fn table(&self, region: u32) -> Option<&RegionTable> {
        self.tables.binary_search_by_key(&region, |t| t.0).ok().map(|i| &self.tables[i].1)
    }

    /// Region `region`'s table, made by `new` if absent.
    pub fn table_mut(&mut self, region: u32, new: impl FnOnce() -> RegionTable) -> &mut RegionTable {
        let i = match self.tables.binary_search_by_key(&region, |t| t.0) {
            Ok(i) => i,
            Err(i) => {
                self.tables.insert(i, (region, new()));
                i
            }
        };
        &mut self.tables[i].1
    }

    /// Append region `region`'s table (past every region held: checked;
    /// an empty table is not held).
    pub fn push(&mut self, region: u32, table: RegionTable) -> Result<()> {
        ensure!(self.tables.last().is_none_or(|t| t.0 < region), "regions out of order ({region} after {:?})", self.tables.last().map(|t| t.0));
        if !table.is_empty() {
            self.tables.push((region, table));
        }
        Ok(())
    }

    /// The regions held and their tables, ascending.
    pub fn tables(&self) -> &[(u32, RegionTable)] {
        &self.tables
    }

    pub fn tables_mut(&mut self) -> &mut [(u32, RegionTable)] {
        &mut self.tables
    }

    pub fn entries(&self) -> usize {
        self.tables.iter().map(|t| t.1.len()).sum()
    }

    pub fn states(&self) -> u64 {
        self.tables.iter().map(|t| t.1.states()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.tables.iter().all(|t| t.1.is_empty())
    }

    /// Every entry `(region, key, cells)`, in order.
    pub fn iter(&self) -> impl Iterator<Item = (u32, Key, u64)> + '_ {
        self.tables.iter().flat_map(|(r, t)| t.iter().map(move |(k, m)| (*r, k, m)))
    }

    /// Every state, in order.
    pub fn states_iter(&self) -> impl Iterator<Item = Sk> + '_ {
        self.iter().flat_map(|(region, key, mask)| super::layer::bits(mask).map(move |local| Sk { region, key, local: local as u8 }))
    }

    /// The cells of `(region, key)` held (0: none).
    pub fn mask_of(&self, region: u32, key: Key) -> u64 {
        self.table(region).map_or(0, |t| t.mask_of(key))
    }

    /// Does the set hold state `s`?
    pub fn contains(&self, s: Sk) -> bool {
        self.mask_of(s.region, s.key) >> s.local & 1 == 1
    }

    /// Bytes allocated.
    pub fn alloc_bytes(&self) -> usize {
        self.tables.capacity() * std::mem::size_of::<(u32, RegionTable)>() + self.tables.iter().map(|t| t.1.alloc_bytes()).sum::<usize>()
    }

    /// The `(region, table)` holding entry `e` of the whole set (its
    /// entries numbered in order), with the entry's place in the table:
    /// `starts` from `entry_starts`.
    pub fn locate(&self, starts: &[usize], e: usize) -> Result<(usize, u32)> {
        let i = starts.partition_point(|&s| s <= e).checked_sub(1).ok_or_else(|| anyhow!("entry {e} before the set"))?;
        ensure!(i < self.tables.len() && e - starts[i] < self.tables[i].1.len(), "entry {e} past the set");
        Ok((i, (e - starts[i]) as u32))
    }

    /// Each table's first entry's number in the whole set.
    pub fn entry_starts(&self) -> Vec<usize> {
        let mut s = 0;
        self.tables
            .iter()
            .map(|t| {
                let a = s;
                s += t.1.len();
                a
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A table indexed and frozen holds what was put in: keys of every
    /// width (1 to 16 bytes, provisional ones included) and a growing
    /// count, found again after every repack, reindex, rekey and freeze;
    /// absent keys are not found; a merge ORs and adds.
    #[test]
    fn a_region_table_keeps_its_entries_through_every_change() {
        let mut t = RegionTable::new();
        let mut want: std::collections::BTreeMap<u128, u64> = Default::default();
        let mut x = 0x9e37_79b9_7f4a_7c15u64;
        let mut next = || {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            x
        };
        for i in 0..20_000u64 {
            let width = 1 + (i / 1500) as u32 % 16;
            let w = (next() as u128) << 64 | next() as u128;
            let w = if width == 16 { w | 1 << 127 } else { w & ((1u128 << (8 * width)) - 1) };
            let m = 1u64 << (next() % 64);
            let (e, had) = t.insert_or(narrow(w), m);
            assert_eq!(had, want.get(&w).copied().unwrap_or(0));
            *want.entry(w).or_default() |= m;
            assert_eq!(t.mask(e), want[&w]);
        }
        for (&w, &m) in &want {
            assert_eq!(t.mask_of(narrow(w)), m);
        }
        assert_eq!(t.find((0, 0x1234_5678_9abc)), want.get(&0x1234_5678_9abc).map(|_| t.find((0, 0x1234_5678_9abc)).unwrap()));
        // Rekey: every key's top bit cleared where set (a provisional key
        // made final).
        t.rekey(|k| Ok((k.0 & !(1 << 63), k.1))).unwrap();
        let rk: std::collections::BTreeMap<u128, u64> = want.iter().fold(Default::default(), |mut a, (&w, &m)| {
            *a.entry(w & !(1 << 127)).or_default() |= m;
            a
        });
        assert_eq!(t.len(), rk.len(), "no two keys merged by the rekey in this test");
        for (&w, &m) in &rk {
            assert_eq!(t.mask_of(narrow(w)), m);
        }
        let mut f = t.clone();
        f.freeze();
        assert!(f.is_frozen());
        assert_eq!(f.iter().map(|(k, m)| (wide(k), m)).collect::<Vec<_>>(), rk.iter().map(|(&w, &m)| (w, m)).collect::<Vec<_>>());
        for (&w, &m) in rk.iter().step_by(7) {
            assert_eq!(f.mask_of(narrow(w)), m);
            assert_eq!(f.mask_of(narrow(w ^ 1 << 100)), rk.get(&(w ^ 1 << 100)).copied().unwrap_or(0));
        }
        // Merge a frozen table into a fresh indexed one, twice: the second
        // adds nothing new.
        let mut g = RegionTable::new();
        g.merge(&f, |_, _| {});
        g.merge(&f, |_, _| {});
        assert_eq!(g.len(), f.len());
        assert_eq!(g.states(), f.states());
        // Across chunks: 100k entries (three chunks and part of a fourth),
        // a repack at entry 70000, found, frozen, merged.
        let mut big = RegionTable::new();
        for i in 0..100_000u64 {
            let k = if i < 70_000 { (0, i * 7919 % 1_000_003) } else { (1, i) };
            assert_eq!(big.insert_or(k, 1 << (i % 64)).1, 0);
        }
        assert_eq!((big.len(), big.kw), (100_000, 9));
        for i in (0..100_000u64).step_by(13) {
            let k = if i < 70_000 { (0, i * 7919 % 1_000_003) } else { (1, i) };
            assert_eq!(big.mask_of(k), 1 << (i % 64));
        }
        let mut fb = big.clone();
        fb.freeze();
        assert!(fb.iter().map(|x| wide(x.0)).collect::<Vec<_>>().windows(2).all(|w| w[0] < w[1]));
        assert_eq!(fb.states(), 100_000);
        let mut gb = RegionTable::new();
        gb.merge(&fb, |_, _| {});
        assert_eq!((gb.len(), gb.states()), (100_000, 100_000));
        assert_eq!(gb.mask_of((1, 99_999)), 1 << (99_999 % 64));
        // Narrow keys stay narrow.
        let mut n = RegionTable::new();
        for k in 0..1000u64 {
            n.push((0, k * 3), 1);
        }
        assert_eq!(n.kw, 2);
        assert!(n.keys.iter().map(|c| c.capacity()).sum::<usize>() < 1000 * 3, "more than 3 B of keys a two-byte key");
        let slots = n.index.as_ref().expect("indexed").slots.len();
        assert!((1000..=2000).contains(&slots), "{slots} index slots for 1000 entries");
    }

    /// Sets: frozen tables pushed in region order, membership, entries
    /// by index.
    #[test]
    fn a_region_set_finds() {
        let mut a = RegionSet::default();
        a.push(3, RegionTable::from_sorted(&[(0, 5), (0, 9)], &[0b11, 1]).unwrap()).unwrap();
        a.push(70, RegionTable::from_sorted(&[(0, 1)], &[1]).unwrap()).unwrap();
        assert!(a.push(5, RegionTable::frozen()).is_err(), "regions out of order");
        a.push(90, RegionTable::from_sorted(&[(0, 0)], &[1]).unwrap()).unwrap();
        assert_eq!(a.iter().collect::<Vec<_>>(), vec![(3, (0, 5), 0b11), (3, (0, 9), 1), (70, (0, 1), 1), (90, (0, 0), 1)]);
        assert!(a.contains(Sk { region: 3, key: (0, 5), local: 1 }));
        assert!(!a.contains(Sk { region: 3, key: (0, 5), local: 2 }));
        let starts = a.entry_starts();
        assert_eq!(a.locate(&starts, 2).unwrap(), (1, 0));
        assert_eq!(a.locate(&starts, 3).unwrap(), (2, 0));
        assert!(a.locate(&starts, 4).is_err());
    }
}
