//! THE ONE STATE SET (plans/architecture.md, "The forward"): per storage
//! region a TABLE of entries (exact key, cell mask) SORTED by key, in one
//! of two forms:
//!
//!   * PLAIN: keys (in the bytes the largest needs, `kw`) and masks in
//!     arrays: the frontier, a frame read back, the next layer;
//!   * PACKED: zstd BLOCKS of about `PACK_ENTRIES` entries, column by
//!     column, each state with its INFO (`StateInfo`; none for a full hull:
//!     its entry's BARE cells): the visited set, ~3 B a state.

use crate::search::hull::HullOf;
use anyhow::{anyhow, ensure, Result};

use super::codec::{get_varint, get_varint128, narrow, put_varint, put_varint128, wide};
use super::visited::Key;
use super::{Sk, GEOMETRY};
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;

const _: () = assert!(GEOMETRY.words == 1, "a set holds a u64 mask an entry: 8 x 8 storage regions");

/// The bytes key `w` needs (at least 1).
#[inline]
fn bytes_of(w: u128) -> usize {
    (16 - (w.leading_zeros() / 8) as usize).max(1)
}

/// A state's LAYER (the frame that first reached and expanded it), HULL so far.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StateInfo<N: Rem = P8> {
    pub hull: HullOf<N>,
    pub layer: u16,
}

impl<N: Rem> StateInfo<N> {
    /// Is its hull the whole torus (then it is not stored: a bare cell)?
    #[inline]
    pub fn is_full(&self) -> bool {
        self.hull.is_full()
    }
}

/// Entries a plain table's chunk holds.
const CH_BITS: u32 = 15;
const CH: usize = 1 << CH_BITS;

/// Entries a packed block holds, at most.
pub const PACK_ENTRIES: usize = 1024;

#[derive(Clone)]
pub struct RegionTable {
    len: usize,
    form: Form,
}

#[derive(Clone)]
enum Form {
    /// Per chunk its keys, `kw` bytes each, then 16 zero bytes (a key is
    /// read as 16), and its masks.
    Plain { kw: usize, keys: Vec<Vec<u8>>, masks: Vec<Vec<u64>> },
    /// The blocks, by key; the states held and those with an info.
    Packed { blocks: Vec<Block>, states: u64, infos: u64 },
}

/// A packed BLOCK: key range and counts (what a lookup searches), raw, zstd.
#[derive(Clone)]
pub struct Block {
    first: u128,
    last: u128,
    entries: u32,
    states: u32,
    infos: u32,
    raw: u32,
    bytes: Box<[u8]>,
}

impl Block {
    pub fn first(&self) -> Key {
        narrow(self.first)
    }
}

/// A packed block DECODED: per entry its key, cells, bare cells and its
/// first info's place (`at`, one more at the end); the infos by entry, cell.
pub struct Rows<N: Rem = P8> {
    keys: Vec<u128>,
    masks: Vec<u64>,
    bare: Vec<u64>,
    at: Vec<u32>,
    infos: Vec<StateInfo<N>>,
}

impl<N: Rem> Default for Rows<N> {
    fn default() -> Self {
        Rows { keys: Vec::new(), masks: Vec::new(), bare: Vec::new(), at: Vec::new(), infos: Vec::new() }
    }
}

impl<N: Rem> Rows<N> {
    fn clear(&mut self) {
        self.keys.clear();
        self.masks.clear();
        self.bare.clear();
        self.at.clear();
        self.at.push(0);
        self.infos.clear();
    }

    fn len(&self) -> usize {
        self.keys.len()
    }

    /// Append an entry with its infos (one per cell not bare, by cell).
    #[inline]
    fn push(&mut self, key: u128, mask: u64, bare: u64, infos: &[StateInfo<N>]) {
        debug_assert_eq!((mask & !bare).count_ones() as usize, infos.len());
        debug_assert!(self.keys.last().is_none_or(|&k| k < key), "a packed table's keys out of order");
        self.keys.push(key);
        self.masks.push(mask);
        self.bare.push(bare);
        self.infos.extend_from_slice(infos);
        self.at.push(self.infos.len() as u32);
    }

    #[inline]
    fn infos_of(&self, i: usize) -> &[StateInfo<N>] {
        &self.infos[self.at[i] as usize..self.at[i + 1] as usize]
    }

    fn encode(&self, a: usize, b: usize, raw: &mut Vec<u8>) -> Result<Block> {
        raw.clear();
        let (ia, ib) = (self.at[a] as usize, self.at[b] as usize);
        put_varint(raw, (b - a) as u64);
        put_varint(raw, (ib - ia) as u64);
        let mut prev = 0u128;
        for (k, &key) in self.keys[a..b].iter().enumerate() {
            put_varint128(raw, if k == 0 { key } else { key - prev });
            prev = key;
        }
        for &m in &self.masks[a..b] {
            raw.extend_from_slice(&m.to_le_bytes());
        }
        for &m in &self.bare[a..b] {
            put_varint(raw, m);
        }
        for i in &self.infos[ia..ib] {
            raw.extend_from_slice(&i.layer.to_le_bytes());
        }
        for i in &self.infos[ia..ib] {
            i.hull.to_bytes(raw);
        }
        ensure!(raw.len() < u32::MAX as usize, "a packed block of {} bytes", raw.len());
        let states: u32 = self.masks[a..b].iter().map(|m| m.count_ones()).sum();
        Ok(Block { first: self.keys[a], last: self.keys[b - 1], entries: (b - a) as u32, states, infos: (ib - ia) as u32, raw: raw.len() as u32, bytes: zstd_pack(raw)? })
    }

    fn decode(&mut self, block: &Block, raw: &mut Vec<u8>) -> Result<()> {
        let pos = self.decode_cells(block, raw)?;
        let ni = block.infos as usize;
        ensure!(pos + (2 + HullOf::<N>::BYTES) * ni == raw.len(), "a packed block's infos do not match its cells");
        let (layers, hulls) = raw[pos..].split_at(2 * ni);
        self.infos.extend(layers.chunks_exact(2).zip(hulls.chunks_exact(HullOf::<N>::BYTES)).map(|(l, h)| StateInfo { layer: u16::from_le_bytes([l[0], l[1]]), hull: HullOf::<N>::from_bytes(h) }));
        Ok(())
    }

    /// `block`'s keys, cells and bare cells alone (the infos are the
    /// model's) into these rows (cleared first); where the infos start.
    fn decode_cells(&mut self, block: &Block, raw: &mut Vec<u8>) -> Result<usize> {
        self.clear();
        zstd_unpack(&block.bytes, block.raw as usize, raw)?;
        let b = &raw[..];
        let mut pos = 0usize;
        let n = get_varint(b, &mut pos) as usize;
        let ni = get_varint(b, &mut pos) as usize;
        ensure!(n == block.entries as usize && ni == block.infos as usize, "a packed block of {n} entries and {ni} infos, its index says {} and {}", block.entries, block.infos);
        let mut prev = 0u128;
        for k in 0..n {
            let d = get_varint128(b, &mut pos);
            prev = if k == 0 { d } else { prev + d };
            self.keys.push(prev);
        }
        ensure!(pos + 8 * n <= b.len(), "a packed block cut short");
        self.masks.extend(b[pos..pos + 8 * n].chunks_exact(8).map(|x| u64::from_le_bytes(x.try_into().unwrap())));
        pos += 8 * n;
        let mut at = 0u32;
        for k in 0..n {
            let bare = get_varint(b, &mut pos);
            self.bare.push(bare);
            at += (self.masks[k] & !bare).count_ones();
            self.at.push(at);
        }
        ensure!(at as usize == ni, "a packed block's infos do not match its cells");
        ensure!(self.keys.first() == Some(&block.first) && self.keys.last() == Some(&block.last), "a packed block's keys are not its index's");
        Ok(pos)
    }
}

thread_local! {
    static ZSTD: std::cell::RefCell<Option<(zstd::bulk::Compressor<'static>, zstd::bulk::Decompressor<'static>)>> = const { std::cell::RefCell::new(None) };
}

fn zstd_pack(raw: &[u8]) -> Result<Box<[u8]>> {
    ZSTD.with(|z| {
        let mut z = z.borrow_mut();
        let (c, _) = match &mut *z {
            Some(x) => x,
            None => z.insert((zstd::bulk::Compressor::new(1)?, zstd::bulk::Decompressor::new()?)),
        };
        Ok(c.compress(raw)?.into_boxed_slice())
    })
}

/// `bytes` decompressed into `out` (`raw` bytes; the thread's context).
fn zstd_unpack(bytes: &[u8], raw: usize, out: &mut Vec<u8>) -> Result<()> {
    ZSTD.with(|z| {
        let mut z = z.borrow_mut();
        let (_, d) = match &mut *z {
            Some(x) => x,
            None => z.insert((zstd::bulk::Compressor::new(1)?, zstd::bulk::Decompressor::new()?)),
        };
        out.clear();
        out.reserve(raw);
        let n = d.decompress_to_buffer(bytes, out)?;
        ensure!(n == raw, "a packed block decompressed to {n} bytes, not {raw}");
        Ok(())
    })
}

/// One change of a packed table (`upsert`): state `(key, cell)`, its info
/// (`None`: a full hull, bare), and whether it is NEW (held already: an
/// error the caller counts, `Upserted::held_new`).
#[derive(Clone, Copy, Debug)]
pub struct Change<N: Rem = P8> {
    pub key: Key,
    pub cell: u8,
    pub info: Option<StateInfo<N>>,
    pub new: bool,
}

/// What an `upsert` did: the states it added, the NEW changes held already.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Upserted {
    pub added: u64,
    pub held_new: u64,
    /// Blocks kept as they were, decoded, and made (`--phases`' `[absorb]`).
    pub kept: u64,
    pub decoded: u64,
    pub made: u64,
}

impl std::ops::AddAssign for Upserted {
    fn add_assign(&mut self, o: Upserted) {
        self.added += o.added;
        self.held_new += o.held_new;
        self.kept += o.kept;
        self.decoded += o.decoded;
        self.made += o.made;
    }
}

/// An entry as a `Cursor` finds it: cells, bare cells, the others' infos.
pub struct Entry<'r, N: Rem = P8> {
    pub mask: u64,
    pub bare: u64,
    pub infos: &'r [StateInfo<N>],
}

impl<N: Rem> Entry<'_, N> {
    /// Cell `c`'s info (`None`: not held, or bare).
    #[inline]
    pub fn info(&self, c: u32) -> Option<StateInfo<N>> {
        let with = self.mask & !self.bare;
        (with >> c & 1 == 1).then(|| self.infos[(with & ((1u64 << c) - 1)).count_ones() as usize])
    }
}

/// A packed table WALKED by ascending keys: each block decoded once, if hit.
pub struct Cursor<'a, N: Rem = P8> {
    blocks: &'a [Block],
    b: usize,
    loaded: usize,
    i: usize,
    rows: Rows<N>,
    raw: Vec<u8>,
}

impl<'a, N: Rem> Cursor<'a, N> {
    /// The entry of key `k`, if held (keys asked in ascending order).
    pub fn seek(&mut self, k: Key) -> Result<Option<Entry<'_, N>>> {
        let w = wide(k);
        self.b += self.blocks[self.b..].partition_point(|x| x.last < w);
        let Some(block) = self.blocks.get(self.b) else { return Ok(None) };
        if w < block.first {
            return Ok(None);
        }
        if self.loaded != self.b {
            self.rows.decode(block, &mut self.raw)?;
            (self.loaded, self.i) = (self.b, 0);
        }
        self.i += self.rows.keys[self.i..].partition_point(|&x| x < w);
        let i = self.i;
        Ok((self.rows.keys.get(i) == Some(&w)).then(|| Entry { mask: self.rows.masks[i], bare: self.rows.bare[i], infos: self.rows.infos_of(i) }))
    }
}

impl Default for RegionTable {
    fn default() -> Self {
        RegionTable::frozen()
    }
}

impl RegionTable {
    /// An empty plain table.
    pub fn frozen() -> Self {
        RegionTable { len: 0, form: Form::Plain { kw: 1, keys: Vec::new(), masks: Vec::new() } }
    }

    pub fn packed() -> Self {
        RegionTable { len: 0, form: Form::Packed { blocks: Vec::new(), states: 0, infos: 0 } }
    }

    /// A packed table of `blocks` (by key, distinct: checked).
    pub fn of_blocks(blocks: Vec<Block>) -> Result<Self> {
        ensure!(blocks.windows(2).all(|w| w[0].last < w[1].first), "packed blocks out of order");
        let (len, states, infos) = blocks.iter().fold((0usize, 0u64, 0u64), |a, b| (a.0 + b.entries as usize, a.1 + b.states as u64, a.2 + b.infos as u64));
        Ok(RegionTable { len, form: Form::Packed { blocks, states, infos } })
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn is_packed(&self) -> bool {
        matches!(self.form, Form::Packed { .. })
    }

    /// A plain table's arrays (a packed one's are blocks: FATAL).
    #[inline]
    fn plain(&self) -> (usize, &[Vec<u8>], &[Vec<u64>]) {
        match &self.form {
            Form::Plain { kw, keys, masks } => (*kw, keys, masks),
            Form::Packed { .. } => panic!("a packed table read by entry number"),
        }
    }

    /// A packed table's blocks (a plain one has none).
    pub fn blocks(&self) -> &[Block] {
        match &self.form {
            Form::Packed { blocks, .. } => blocks,
            Form::Plain { .. } => &[],
        }
    }

    /// Its blocks taken out (a packed table: left empty).
    pub fn take_blocks(&mut self) -> Vec<Block> {
        match &mut self.form {
            Form::Packed { blocks, states, infos } => {
                (*states, *infos, self.len) = (0, 0, 0);
                std::mem::take(blocks)
            }
            Form::Plain { .. } => Vec::new(),
        }
    }

    /// Entry `e`'s key (a plain table).
    #[inline]
    pub fn key(&self, e: u32) -> Key {
        narrow(self.wkey(e as usize))
    }

    #[inline]
    fn wkey(&self, e: usize) -> u128 {
        let (kw, keys, _) = self.plain();
        let c = &keys[e >> CH_BITS];
        let o = (e & (CH - 1)) * kw;
        let w = u128::from_le_bytes(c[o..o + 16].try_into().expect("16 bytes"));
        if kw == 16 {
            w
        } else {
            w & ((1u128 << (8 * kw)) - 1)
        }
    }

    /// Entry `e`'s cells (a plain table).
    #[inline]
    pub fn mask(&self, e: u32) -> u64 {
        self.plain().2[e as usize >> CH_BITS][e as usize & (CH - 1)]
    }

    /// Every entry's cells, in order (a plain table).
    pub fn masks(&self) -> impl Iterator<Item = u64> + '_ {
        self.plain().2.iter().flatten().copied()
    }

    /// Every entry `(key, cells)`, by key (a plain table).
    pub fn iter(&self) -> impl Iterator<Item = (Key, u64)> + '_ {
        (0..self.len).map(move |e| (narrow(self.wkey(e)), self.mask(e as u32)))
    }

    pub fn states(&self) -> u64 {
        match &self.form {
            Form::Plain { .. } => self.masks().map(|m| m.count_ones() as u64).sum(),
            Form::Packed { states, .. } => *states,
        }
    }

    /// States held with an info (not full: a packed table's).
    pub fn infos(&self) -> u64 {
        match &self.form {
            Form::Plain { .. } => 0,
            Form::Packed { infos, .. } => *infos,
        }
    }

    /// The first entry whose key is `k` or past it (a plain table).
    pub fn lower_bound(&self, k: Key) -> usize {
        let w = wide(k);
        let (mut lo, mut hi) = (0usize, self.len);
        while lo < hi {
            let mid = (lo + hi) / 2;
            if self.wkey(mid) < w {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        lo
    }

    /// The entry holding `k` (a plain table: binary search).
    pub fn find(&self, k: Key) -> Option<u32> {
        let w = wide(k);
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

    /// The cells of `k` held (0: none; either form).
    pub fn mask_of(&self, k: Key) -> u64 {
        match &self.form {
            Form::Plain { .. } => self.find(k).map_or(0, |e| self.mask(e)),
            Form::Packed { .. } => self.lookup(k).map_or(0, |(m, _)| m),
        }
    }

    /// A packed table's entry of `k`: its cells and cell `local`'s info
    /// (`None`: not held, or full); one block decoded (a frame: `Cursor`).
    pub fn lookup_info<N: Rem>(&self, k: Key, local: u32) -> Option<(u64, Option<StateInfo<N>>)> {
        let mut c = self.cursor::<N>();
        let e = c.seek(k).expect("a packed block decodes")?;
        Some((e.mask, e.info(local)))
    }

    fn lookup(&self, k: Key) -> Option<(u64, u64)> {
        let (blocks, w) = (self.blocks(), wide(k));
        let block = blocks.get(blocks.partition_point(|x| x.last < w))?;
        if w < block.first {
            return None;
        }
        // No info is read: any model's rows do.
        let (mut rows, mut raw) = (Rows::<P8>::default(), Vec::new());
        rows.decode_cells(block, &mut raw).expect("a packed block decodes");
        let i = rows.keys.binary_search(&w).ok()?;
        Some((rows.masks[i], rows.bare[i]))
    }

    /// A cursor over a packed table (keys asked in ascending order).
    pub fn cursor<N: Rem>(&self) -> Cursor<'_, N> {
        Cursor { blocks: self.blocks(), b: 0, loaded: usize::MAX, i: 0, rows: Rows::default(), raw: Vec::new() }
    }

    /// Every entry of a packed table: `f(key, cells, bare cells, infos)`.
    pub fn for_each_packed<N: Rem>(&self, mut f: impl FnMut(Key, u64, u64, &[StateInfo<N>])) -> Result<()> {
        let (mut rows, mut raw) = (Rows::<N>::default(), Vec::new());
        for b in self.blocks() {
            rows.decode(b, &mut raw)?;
            for i in 0..rows.len() {
                f(narrow(rows.keys[i]), rows.masks[i], rows.bare[i], rows.infos_of(i));
            }
        }
        Ok(())
    }

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

    /// A plain table from `(key, cells)` sorted by key (checked).
    pub fn from_sorted(keys: &[Key], masks: &[u64]) -> Result<RegionTable> {
        ensure!(keys.len() == masks.len(), "a region with {} keys and {} masks", keys.len(), masks.len());
        ensure!(keys.windows(2).all(|w| w[0] < w[1]), "a region's keys not ascending");
        let kw = keys.last().map_or(1, |&k| bytes_of(wide(k)));
        Ok(RegionTable { len: keys.len(), form: Form::Plain { kw, keys: Self::chunk_keys(keys.iter().map(|&k| wide(k)), kw), masks: Self::chunk_masks(masks.iter().copied()) } })
    }

    /// UPSERT `changes` (sorted by key and cell, distinct) into a packed
    /// table: each state set with its info (`None` or a full hull: bare),
    /// made where absent (`upsert_blocks`).
    pub fn upsert<N: Rem>(&mut self, changes: &[Change<N>]) -> Result<Upserted> {
        ensure!(self.is_packed(), "an upsert into a plain table");
        let (blocks, n) = upsert_blocks(self.take_blocks(), changes)?;
        *self = RegionTable::of_blocks(blocks)?;
        Ok(n)
    }

    pub fn alloc_bytes(&self) -> usize {
        self.bytes().iter().sum()
    }

    /// Bytes allocated (`[memory]`): a plain table's keys (with their
    /// chunks' room) and masks, a packed table's blocks (zstd), the lists.
    pub fn bytes(&self) -> [usize; 3] {
        match &self.form {
            Form::Plain { keys, masks, .. } => [keys.iter().map(|c| c.capacity()).sum::<usize>(), masks.iter().map(|c| c.capacity() * 8).sum::<usize>(), (keys.capacity() + masks.capacity()) * 24],
            Form::Packed { blocks, .. } => [blocks.iter().map(|b| b.bytes.len()).sum(), 0, blocks.capacity() * std::mem::size_of::<Block>()],
        }
    }
}

/// UPSERT `changes` (sorted by key and cell, distinct) into the packed
/// `blocks` (by key). A block no change falls in is kept, unless fewer than
/// half a block's entries were made before it (then it joins them); the
/// others are decoded, merged and cut again at `PACK_ENTRIES`. Blocks are
/// independent: a big table's runs are upserted apart, put back in order.
pub fn upsert_blocks<N: Rem>(blocks: Vec<Block>, changes: &[Change<N>]) -> Result<(Vec<Block>, Upserted)> {
    debug_assert!(changes.windows(2).all(|w| (wide(w[0].key), w[0].cell) < (wide(w[1].key), w[1].cell)), "changes not by key and cell");
    let mut w = Writer::<N>::default();
    let (mut rows, mut raw) = (Rows::<N>::default(), Vec::new());
    let mut ci = 0usize;
    let mut n = Upserted::default();
    for block in blocks {
        let hi = ci + changes[ci..].partition_point(|c| wide(c.key) <= block.last);
        if ci == hi {
            if w.rows.len() == 0 || w.rows.len() >= PACK_ENTRIES / 2 {
                w.flush(true)?;
                w.out.push(block);
                n.kept += 1;
            } else {
                rows.decode(&block, &mut raw)?;
                n.decoded += 1;
                n += merge_rows(&rows, &[], &mut w)?;
            }
            continue;
        }
        rows.decode(&block, &mut raw)?;
        n.decoded += 1;
        n += merge_rows(&rows, &changes[ci..hi], &mut w)?;
        ci = hi;
    }
    rows.clear();
    n += merge_rows(&rows, &changes[ci..], &mut w)?;
    w.flush(true)?;
    n.made = (w.out.len() as u64).saturating_sub(n.kept);
    Ok((w.out, n))
}

/// An upsert's output: the entries not yet in a block, the blocks made.
struct Writer<N: Rem> {
    rows: Rows<N>,
    out: Vec<Block>,
    raw: Vec<u8>,
}

impl<N: Rem> Default for Writer<N> {
    fn default() -> Self {
        Writer { rows: Rows::default(), out: Vec::new(), raw: Vec::new() }
    }
}

impl<N: Rem> Writer<N> {
    /// Blocks of `PACK_ENTRIES` from the entries made (`all`: the rest too).
    fn flush(&mut self, all: bool) -> Result<()> {
        if self.rows.at.is_empty() {
            self.rows.clear();
        }
        let n = self.rows.len();
        let mut a = 0usize;
        while n - a >= PACK_ENTRIES || (all && a < n) {
            let b = (a + PACK_ENTRIES).min(n);
            self.out.push(self.rows.encode(a, b, &mut self.raw)?);
            a = b;
        }
        if a > 0 {
            let ia = self.rows.at[a] as usize;
            self.rows.keys.drain(..a);
            self.rows.masks.drain(..a);
            self.rows.bare.drain(..a);
            self.rows.infos.drain(..ia);
            self.rows.at.drain(..a);
            for x in &mut self.rows.at {
                *x -= ia as u32;
            }
        }
        Ok(())
    }
}

/// Entries `rows` merged with `changes` (by key and cell; keys at most the
/// rows' last unless `rows` is empty) into the writer, in key order.
fn merge_rows<N: Rem>(rows: &Rows<N>, changes: &[Change<N>], w: &mut Writer<N>) -> Result<Upserted> {
    if w.rows.at.is_empty() {
        w.rows.clear();
    }
    let mut n = Upserted::default();
    let (mut r, mut c) = (0usize, 0usize);
    let mut infos: Vec<StateInfo<N>> = Vec::with_capacity(64);
    while r < rows.len() || c < changes.len() {
        let rk = rows.keys.get(r).copied();
        let ck = changes.get(c).map(|x| wide(x.key));
        match (rk, ck) {
            (Some(rk), ck) if ck.is_none_or(|ck| rk < ck) => {
                w.rows.push(rk, rows.masks[r], rows.bare[r], rows.infos_of(r));
                r += 1;
            }
            (rk, Some(ck)) => {
                let d = c + changes[c..].iter().take_while(|x| wide(x.key) == ck).count();
                let (mask, bare, old) = if rk == Some(ck) {
                    r += 1;
                    (rows.masks[r - 1], rows.bare[r - 1], rows.infos_of(r - 1))
                } else {
                    (0, 0, &[][..])
                };
                let (mut mask2, mut bare2) = (mask, bare);
                for x in &changes[c..d] {
                    let bit = 1u64 << x.cell;
                    if mask & bit == 0 {
                        n.added += 1;
                    } else if x.new {
                        n.held_new += 1;
                    }
                    mask2 |= bit;
                    match x.info {
                        Some(i) if !i.is_full() => bare2 &= !bit,
                        _ => bare2 |= bit,
                    }
                }
                // The infos by cell: a change's, else the entry's.
                infos.clear();
                let with_old = mask & !bare;
                let mut k = c;
                let mut cells = mask2 & !bare2;
                while cells != 0 {
                    let cell = cells.trailing_zeros();
                    cells &= cells - 1;
                    while k < d && (changes[k].cell as u32) < cell {
                        k += 1;
                    }
                    let i = match changes.get(k) {
                        Some(x) if k < d && x.cell as u32 == cell => x.info.expect("a cell with an info"),
                        _ => {
                            ensure!(with_old >> cell & 1 == 1, "a packed entry's cell {cell} without an info");
                            old[(with_old & ((1u64 << cell) - 1)).count_ones() as usize]
                        }
                    };
                    infos.push(i);
                }
                w.rows.push(ck, mask2, bare2, &infos);
                c = d;
            }
            (None, None) => unreachable!("the loop's condition"),
            (Some(_), None) => unreachable!("matched above"),
        }
    }
    w.flush(false)?;
    Ok(n)
}

/// A SET OF STATES: per region (ascending) its table.
#[derive(Clone, Default)]
pub struct RegionSet {
    tables: Vec<(u32, RegionTable)>,
}

impl RegionSet {
    #[inline]
    pub fn table(&self, region: u32) -> Option<&RegionTable> {
        self.tables.binary_search_by_key(&region, |t| t.0).ok().map(|i| &self.tables[i].1)
    }

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

    pub fn states_iter(&self) -> impl Iterator<Item = Sk> + '_ {
        self.iter().flat_map(|(region, key, mask)| super::layer::bits(mask).map(move |local| Sk { region, key, local: local as u8 }))
    }

    pub fn mask_of(&self, region: u32, key: Key) -> u64 {
        self.table(region).map_or(0, |t| t.mask_of(key))
    }

    pub fn contains(&self, s: Sk) -> bool {
        self.mask_of(s.region, s.key) >> s.local & 1 == 1
    }

    pub fn alloc_bytes(&self) -> usize {
        self.bytes().iter().sum()
    }

    /// `RegionTable::bytes` summed, the region list in the last.
    pub fn bytes(&self) -> [usize; 3] {
        let mut b = [0, 0, self.tables.capacity() * std::mem::size_of::<(u32, RegionTable)>()];
        for (_, t) in &self.tables {
            for (a, x) in b.iter_mut().zip(t.bytes()) {
                *a += x;
            }
        }
        b
    }

    /// Entry `e` of the whole set (numbered in order) as its table's index
    /// and its place there; `starts` from `entry_starts`.
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
    use crate::search::hull::Hull;

    fn rng(seed: u64) -> impl FnMut() -> u64 {
        let mut x = seed | 1;
        move || {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            x
        }
    }

    /// A plain table holds what it was made of, keys of every width (1 to
    /// 16 bytes) across chunks; found by binary search, absent keys not.
    #[test]
    fn a_plain_table_finds_its_entries() {
        let mut next = rng(0x9e37_79b9_7f4a_7c15);
        let mut want: std::collections::BTreeMap<u128, u64> = Default::default();
        for i in 0..80_000u64 {
            let width = 1 + (i / 5000) as u32 % 16;
            let w = (next() as u128) << 64 | next() as u128;
            let w = if width == 16 { w | 1 << 127 } else { w & ((1u128 << (8 * width)) - 1) };
            *want.entry(w).or_default() |= 1u64 << (next() % 64);
        }
        let (keys, masks): (Vec<Key>, Vec<u64>) = want.iter().map(|(&w, &m)| (narrow(w), m)).unzip();
        let t = RegionTable::from_sorted(&keys, &masks).unwrap();
        assert_eq!(t.len(), want.len());
        assert_eq!(t.iter().map(|(k, m)| (wide(k), m)).collect::<Vec<_>>(), want.iter().map(|(&w, &m)| (w, m)).collect::<Vec<_>>());
        for (&w, &m) in want.iter().step_by(7) {
            assert_eq!(t.mask_of(narrow(w)), m);
            assert_eq!(t.mask_of(narrow(w ^ 1 << 100)), want.get(&(w ^ 1 << 100)).copied().unwrap_or(0));
        }
        assert!(RegionTable::from_sorted(&[(0, 2), (0, 1)], &[1, 1]).is_err(), "keys out of order");
        let n = RegionTable::from_sorted(&(0..1000u64).map(|k| (0, k * 3)).collect::<Vec<_>>(), &[1; 1000]).unwrap();
        assert!(n.bytes()[0] < 1000 * 3 + 64, "more than 3 B of keys a two-byte key");
    }

    /// A packed table upserted again and again holds what a model holds,
    /// by a cursor and by a single lookup; the counts are the model's;
    /// untouched blocks are kept.
    #[test]
    fn a_packed_table_upserts_as_a_model() {
        let mut next = rng(0x2545_f491_4f6c_dd1d);
        let mut model: std::collections::BTreeMap<(u128, u8), Option<StateInfo>> = Default::default();
        let mut t = RegionTable::packed();
        let hull = |x: u64| Hull::from_bytes(&x.to_le_bytes());
        let full = Hull::full();
        for round in 0..12u64 {
            // Changes: new keys, new cells of held keys, updates (some to full).
            let held: Vec<u128> = model.keys().map(|k| k.0).collect();
            let mut ch: std::collections::BTreeMap<(u128, u8), (Option<StateInfo>, bool)> = Default::default();
            for _ in 0..(if round == 0 { 5000 } else { 1500 }) {
                let key = if held.is_empty() || next() % 3 == 0 { ((next() % 4) as u128) << 64 | (next() % 200_000) as u128 } else { held[(next() % held.len() as u64) as usize] };
                let cell = (next() % 64) as u8;
                let info = match next() % 10 {
                    0 => Some(StateInfo { hull: full, layer: round as u16 }),
                    1 => None,
                    _ => Some(StateInfo { hull: hull(next() & 0x7fff_7fff_7fff_7fff), layer: (next() % 300) as u16 }),
                };
                let new = !model.contains_key(&(key, cell)) && next() % 2 == 0;
                ch.insert((key, cell), (info, new));
            }
            let changes: Vec<Change> = ch.iter().map(|(&(k, c), &(info, new))| Change { key: narrow(k), cell: c, info, new }).collect();
            let want = (ch.keys().filter(|k| !model.contains_key(k)).count() as u64, ch.iter().filter(|(k, v)| v.1 && model.contains_key(k)).count() as u64);
            let got = t.upsert::<P8>(&changes).unwrap();
            assert_eq!((got.added, got.held_new), want, "round {round}");
            for ((k, c), (info, _)) in ch {
                model.insert((k, c), info.filter(|i| !i.is_full()));
            }
            assert_eq!(t.states(), model.len() as u64);
            assert_eq!(t.infos(), model.values().filter(|i| i.is_some()).count() as u64);
            assert!(t.blocks().iter().all(|b| b.entries as usize <= PACK_ENTRIES));
            let mut by_key: std::collections::BTreeMap<u128, Vec<(u8, Option<StateInfo>)>> = Default::default();
            for (&(k, c), &i) in &model {
                by_key.entry(k).or_default().push((c, i));
            }
            let mut cur = t.cursor::<P8>();
            for (k, cells) in by_key.iter().step_by(3) {
                let e = cur.seek(narrow(*k)).unwrap().expect("a held key");
                assert_eq!(e.mask, cells.iter().fold(0, |m, x| m | 1u64 << x.0));
                for &(c, i) in cells {
                    assert_eq!(e.info(c as u32), i);
                }
            }
            let mut cur = t.cursor::<P8>();
            for k in (0..300_000u128).step_by(997) {
                assert_eq!(cur.seek(narrow(k)).unwrap().map(|e| e.mask), by_key.get(&k).map(|c| c.iter().fold(0, |m, x| m | 1u64 << x.0)));
            }
            let (&(k, c), &i) = model.iter().nth(model.len() / 2).unwrap();
            assert_eq!(t.lookup_info::<P8>(narrow(k), c as u32).map(|x| x.1), Some(i));
            let mut walked = 0u64;
            t.for_each_packed::<P8>(|_, m, _, _| walked += m.count_ones() as u64).unwrap();
            assert_eq!(walked, model.len() as u64);
        }
        // Changes in the first block alone: every other block kept as it
        // was (the same allocation).
        assert!(t.blocks().len() > 3);
        let kept: Vec<*const u8> = t.blocks().iter().map(|b| b.bytes.as_ptr()).collect();
        let first = t.blocks()[0].first;
        let k = *model.keys().find(|k| k.0 == first).expect("the first key");
        t.upsert::<P8>(&[Change { key: narrow(k.0), cell: k.1, info: Some(StateInfo { hull: hull(1), layer: 7 }), new: false }]).unwrap();
        assert_eq!(t.lookup_info::<P8>(narrow(k.0), k.1 as u32).and_then(|x| x.1), Some(StateInfo { hull: hull(1), layer: 7 }));
        assert!(t.blocks()[1..].iter().all(|b| kept.contains(&b.bytes.as_ptr())), "a block no change fell in made again");
        assert!(!kept.contains(&t.blocks()[0].bytes.as_ptr()), "the changed block kept");
    }

    /// Sets: plain tables pushed in region order, membership, entries by index.
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
