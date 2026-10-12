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
//!   * the entries' arrays grow by an eighth, a merge by what it adds and a
//!     sixteenth: little slack.

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

/// Room an array grows by when full (an eighth of its length, at least 64):
/// a table growing by one entry at a time (the next layer in a wave).
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

/// ONE REGION'S ENTRIES (module doc).
#[derive(Clone)]
pub struct RegionTable {
    /// Bytes a key takes.
    kw: usize,
    /// The keys, `kw` bytes each, then 16 zero bytes (a key is read as 16).
    keys: Vec<u8>,
    masks: Vec<u64>,
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
        RegionTable { kw: 1, keys: vec![0; 16], masks: Vec::new(), index: Some(Index::default()) }
    }

    /// An empty frozen table.
    pub fn frozen() -> Self {
        RegionTable { kw: 1, keys: vec![0; 16], masks: Vec::new(), index: None }
    }

    pub fn len(&self) -> usize {
        self.masks.len()
    }

    pub fn is_empty(&self) -> bool {
        self.masks.is_empty()
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
        let o = e * self.kw;
        let w = u128::from_le_bytes(self.keys[o..o + 16].try_into().expect("16 bytes"));
        if self.kw == 16 {
            w
        } else {
            w & ((1u128 << (8 * self.kw)) - 1)
        }
    }

    /// Entry `e`'s cells.
    #[inline]
    pub fn mask(&self, e: u32) -> u64 {
        self.masks[e as usize]
    }

    pub fn masks(&self) -> &[u64] {
        &self.masks
    }

    /// Every entry `(key, cells)`, in order (a frozen table's: by key).
    pub fn iter(&self) -> impl Iterator<Item = (Key, u64)> + '_ {
        (0..self.len()).map(move |e| (narrow(self.wkey(e)), self.masks[e]))
    }

    /// States held.
    pub fn states(&self) -> u64 {
        self.masks.iter().map(|m| m.count_ones() as u64).sum()
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
                let (mut lo, mut hi) = (0usize, self.len());
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
        self.find(k).map_or(0, |e| self.masks[e as usize])
    }

    /// OR `mask` into entry `e`; the cells it held before.
    #[inline]
    pub fn or_cells(&mut self, e: u32, mask: u64) -> u64 {
        let m = &mut self.masks[e as usize];
        let had = *m;
        *m |= mask;
        had
    }

    /// Room for `n` more entries (a merge: exactly).
    fn reserve(&mut self, n: usize) {
        let need = self.len() + n;
        if self.masks.capacity() < need {
            // A sixteenth past the need: a table that grows a few percent a
            // frame is copied every few frames, not every frame.
            let c = need.max(self.len() + self.len() / 16);
            self.masks.reserve_exact(c - self.masks.len());
        }
        let bytes = self.masks.capacity() * self.kw + 16;
        if self.keys.capacity() < bytes {
            self.keys.reserve_exact(bytes - self.keys.len());
        }
        if let Some(ix) = &self.index {
            if need as f64 > MAX_LOAD * ix.slots.len() as f64 {
                self.reindex(need);
            }
        }
    }

    /// The keys repacked in `kw` bytes each.
    fn repack(&mut self, kw: usize) {
        debug_assert!(kw >= self.kw && kw <= 16);
        let n = self.len();
        let mut keys = Vec::with_capacity(self.masks.capacity() * kw + 16);
        for e in 0..n {
            keys.extend_from_slice(&self.wkey(e).to_le_bytes()[..kw]);
        }
        keys.extend_from_slice(&[0; 16]);
        self.keys = keys;
        self.kw = kw;
    }

    /// The index rebuilt for `n` entries at the growth load.
    fn reindex(&mut self, n: usize) {
        let cap = ((n.max(4) as f64 / GROW_LOAD).ceil() as usize).max(8);
        let mut ix = Index::with_cap(cap);
        for e in 0..self.len() {
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
        let e = self.len();
        assert!(e < u32::MAX as usize, "a region table of 2^32 entries");
        if self.index.is_none() {
            assert!(e == 0 || self.wkey(e - 1) < w, "a frozen table's keys out of order");
        }
        let kw = bytes_of(w);
        if kw > self.kw {
            self.repack(kw);
        }
        if self.masks.len() == self.masks.capacity() {
            let c = grown(self.masks.len(), e + 1);
            self.masks.reserve_exact(c - self.masks.len());
            self.keys.reserve_exact(c * self.kw + 16 - self.keys.len());
        }
        self.keys.truncate(e * self.kw);
        self.keys.extend_from_slice(&w.to_le_bytes()[..self.kw]);
        self.keys.extend_from_slice(&[0; 16]);
        self.masks.push(mask);
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
    /// table (made where absent), room reserved first: a merge.
    pub fn merge(&mut self, other: &RegionTable) {
        debug_assert!(!self.is_frozen());
        let add = other.iter().filter(|(k, _)| self.find(*k).is_none()).count();
        let widest = (0..other.len()).map(|e| bytes_of(other.wkey(e))).max().unwrap_or(1);
        if widest > self.kw {
            self.repack(widest);
        }
        self.reserve(add);
        for (k, m) in other.iter() {
            self.insert_or(k, m);
        }
    }

    /// Every key `k` made `f(k)` (an indexed table: the index rebuilt if
    /// any key changed): the frame's provisional keys made final.
    pub fn rekey(&mut self, f: impl Fn(Key) -> Result<Key>) -> Result<()> {
        let n = self.len();
        let keys: Vec<Key> = (0..n).map(|e| f(self.key(e as u32))).collect::<Result<_>>()?;
        if keys.iter().enumerate().all(|(e, &k)| k == self.key(e as u32)) {
            return Ok(());
        }
        let kw = keys.iter().map(|&k| bytes_of(wide(k))).max().unwrap_or(1);
        let mut bytes = Vec::with_capacity(n * kw + 16);
        for &k in &keys {
            bytes.extend_from_slice(&wide(k).to_le_bytes()[..kw]);
        }
        bytes.extend_from_slice(&[0; 16]);
        (self.keys, self.kw) = (bytes, kw);
        if self.index.is_some() {
            self.reindex(n);
        }
        Ok(())
    }

    /// FROZEN: the entries sorted by key, the keys in the bytes their
    /// largest needs, the index dropped, the arrays at their size.
    pub fn freeze(&mut self) {
        let n = self.len();
        let mut v: Vec<(u128, u64)> = (0..n).map(|e| (self.wkey(e), self.masks[e])).collect();
        v.sort_unstable_by_key(|x| x.0);
        debug_assert!(v.windows(2).all(|w| w[0].0 != w[1].0), "a key twice in a table");
        let kw = v.last().map_or(1, |x| bytes_of(x.0));
        let mut keys = Vec::with_capacity(n * kw + 16);
        for x in &v {
            keys.extend_from_slice(&x.0.to_le_bytes()[..kw]);
        }
        keys.extend_from_slice(&[0; 16]);
        self.keys = keys;
        self.kw = kw;
        self.masks = v.into_iter().map(|x| x.1).collect();
        self.index = None;
    }

    /// A frozen table from `(key, cells)` sorted by key (a frame file's
    /// region; checked).
    pub fn from_sorted(keys: &[Key], masks: &[u64]) -> Result<RegionTable> {
        ensure!(keys.len() == masks.len(), "a region with {} keys and {} masks", keys.len(), masks.len());
        ensure!(keys.windows(2).all(|w| w[0] < w[1]), "a region's keys not ascending");
        let kw = keys.last().map_or(1, |&k| bytes_of(wide(k)));
        let mut b = Vec::with_capacity(keys.len() * kw + 16);
        for &k in keys {
            b.extend_from_slice(&wide(k).to_le_bytes()[..kw]);
        }
        b.extend_from_slice(&[0; 16]);
        Ok(RegionTable { kw, keys: b, masks: masks.to_vec(), index: None })
    }

    /// The disjoint union of two frozen tables (a frame and its raise: a
    /// cell in both is an error).
    pub fn union(&self, other: &RegionTable) -> Result<RegionTable> {
        let (mut keys, mut masks) = (Vec::with_capacity(self.len() + other.len()), Vec::with_capacity(self.len() + other.len()));
        let (mut i, mut j) = (0, 0);
        while i < self.len() || j < other.len() {
            let a = (i < self.len()).then(|| self.wkey(i));
            let b = (j < other.len()).then(|| other.wkey(j));
            match (a, b) {
                (Some(x), Some(y)) if x == y => {
                    ensure!(self.masks[i] & other.masks[j] == 0, "a state held by a frame and its raise");
                    keys.push(narrow(x));
                    masks.push(self.masks[i] | other.masks[j]);
                    i += 1;
                    j += 1;
                }
                (Some(x), Some(y)) if x < y => {
                    keys.push(narrow(x));
                    masks.push(self.masks[i]);
                    i += 1;
                }
                (Some(x), None) => {
                    keys.push(narrow(x));
                    masks.push(self.masks[i]);
                    i += 1;
                }
                (_, Some(y)) => {
                    keys.push(narrow(y));
                    masks.push(other.masks[j]);
                    j += 1;
                }
                (None, None) => unreachable!(),
            }
        }
        RegionTable::from_sorted(&keys, &masks)
    }

    pub fn alloc_bytes(&self) -> usize {
        self.keys.capacity() + self.masks.capacity() * 8 + self.index.as_ref().map_or(0, |ix| ix.slots.capacity() * 4)
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

    /// The disjoint union with `other` (frozen sets).
    pub fn union(&self, other: &RegionSet) -> Result<RegionSet> {
        let mut out = RegionSet::default();
        let (mut i, mut j) = (0, 0);
        while i < self.tables.len() || j < other.tables.len() {
            let a = self.tables.get(i).map(|t| t.0);
            let b = other.tables.get(j).map(|t| t.0);
            match (a, b) {
                (Some(x), Some(y)) if x == y => {
                    out.push(x, self.tables[i].1.union(&other.tables[j].1)?)?;
                    i += 1;
                    j += 1;
                }
                (Some(x), Some(y)) if x < y => {
                    out.push(x, self.tables[i].1.clone())?;
                    i += 1;
                }
                (Some(x), None) => {
                    out.push(x, self.tables[i].1.clone())?;
                    i += 1;
                }
                (_, Some(y)) => {
                    out.push(y, other.tables[j].1.clone())?;
                    j += 1;
                }
                (None, None) => unreachable!(),
            }
        }
        Ok(out)
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
        g.merge(&f);
        g.merge(&f);
        assert_eq!(g.len(), f.len());
        assert_eq!(g.states(), f.states());
        // Narrow keys stay narrow.
        let mut n = RegionTable::new();
        for k in 0..1000u64 {
            n.push((0, k * 3), 1);
        }
        assert_eq!(n.kw, 2);
        assert!(n.keys.capacity() < 1000 * 3, "{} B of keys for 1000 two-byte keys", n.keys.capacity());
        let slots = n.index.as_ref().expect("indexed").slots.len();
        assert!((1000..=2000).contains(&slots), "{slots} index slots for 1000 entries");
    }

    /// Sets: frozen tables pushed in region order, union, membership.
    #[test]
    fn a_region_set_unions_and_finds() {
        let mut a = RegionSet::default();
        a.push(3, RegionTable::from_sorted(&[(0, 5), (0, 9)], &[0b11, 1]).unwrap()).unwrap();
        a.push(70, RegionTable::from_sorted(&[(0, 1)], &[1]).unwrap()).unwrap();
        assert!(a.push(5, RegionTable::frozen()).is_err(), "regions out of order");
        let mut b = RegionSet::default();
        b.push(3, RegionTable::from_sorted(&[(0, 5), (1, 0)], &[0b100, 1]).unwrap()).unwrap();
        b.push(9, RegionTable::from_sorted(&[(0, 0)], &[1]).unwrap()).unwrap();
        let u = a.union(&b).unwrap();
        assert_eq!(u.iter().collect::<Vec<_>>(), vec![(3, (0, 5), 0b111), (3, (0, 9), 1), (3, (1, 0), 1), (9, (0, 0), 1), (70, (0, 1), 1)]);
        assert!(u.contains(Sk { region: 3, key: (0, 5), local: 2 }));
        assert!(!u.contains(Sk { region: 3, key: (0, 5), local: 3 }));
        assert!(a.union(&a).is_err(), "a state twice");
        let starts = u.entry_starts();
        assert_eq!(u.locate(&starts, 3).unwrap(), (1, 0));
        assert_eq!(u.locate(&starts, 4).unwrap(), (2, 0));
        assert!(u.locate(&starts, 5).is_err());
    }
}
