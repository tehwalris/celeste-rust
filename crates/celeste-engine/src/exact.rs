//! EXACT KEYS (plans/exact-keys.md): a state's identity within its shape is
//! the PACKED tuple of its fields' dictionary indices, not a hash.
//!
//!   * `Code`       - a value's exact code (a number and `[n, n]` alike; a
//!                    position coordinate less its low end's whole pixels)
//!   * `FieldDict`  - per (shape, cell): code <-> index, append-only, and the
//!                    field's BIT RUNS in the shape's 127-bit key
//!   * `KeySpace`   - a tree's dictionaries: keys rows, grows only through
//!                    recorded `KeyAdditions` (sorted per frame: canonical)
//!   * `Provisional`- a wave's interned keys of states holding a code no
//!                    dictionary has yet (they are new by construction)
//!   * `ExactRow`   - a whole state's canonical bytes (the concrete search's
//!                    dedup, independent of any tree)
//!
//! Bits once given to a field never move and an old index is zero in a
//! field's later bits, so a key never changes as the dictionaries grow.
//! Hashing remains only as a slot function (`slot_hash`), equality exact.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::runtime2::{mix64, BoundaryIds, Cell2, Rt2, AV, POS_WHOLE};

/// A packed key: (bits 64..127, bits 0..63). Bit 127 tags a PROVISIONAL key.
pub type Key = (u64, u64);

/// Bits a shape's packed key may use (bit 127 is the provisional tag).
pub const KEY_BITS: u32 = 127;

/// The provisional tag, in `Key::0`.
pub const PROVISIONAL: u64 = 1 << 63;

/// No index (a code not in the dictionary).
pub const NONE: u32 = u32::MAX;

#[inline]
pub fn is_provisional(k: Key) -> bool {
    k.0 & PROVISIONAL != 0
}

/// A key's slot in a hash table (the key itself is compared exactly). A
/// packed key is mostly zero in its high word: no table may use a word as
/// its hash.
#[inline]
pub fn slot_hash(k: Key) -> u64 {
    mix64(k.1 ^ mix64(k.0 ^ 0x9e37_79b9_7f4a_7c15))
}

#[inline]
fn to_key(x: u128) -> Key {
    ((x >> 64) as u64, x as u64)
}

#[inline]
fn of_key(k: Key) -> u128 {
    (k.0 as u128) << 64 | k.1 as u128
}

/// A value's exact code, ordered (kind, a, b). Numbers and intervals are
/// kind 0 with their raw bounds (a point is `a == b`: `Num(n)` and
/// `Ival(n, n)` are one code, as producers disagree on column types).
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct Code {
    pub kind: u8,
    pub a: u32,
    pub b: u32,
}

const K_NUM: u8 = 0;
const K_BOOL: u8 = 1;
const K_UBOOL: u8 = 2;
const K_UNUM: u8 = 3;
const K_STR: u8 = 4;
const K_NIL: u8 = 5;
const K_PTR: u8 = 6;
const K_NILPTR: u8 = 7;

impl Code {
    /// The number or interval with raw bounds `lo`, `hi`.
    #[inline]
    pub const fn num(lo: u32, hi: u32) -> Code {
        Code { kind: K_NUM, a: lo, b: hi }
    }

    /// `Bool(b)`, or `UBool` for `None`.
    #[inline]
    pub const fn boolean(b: Option<bool>) -> Code {
        match b {
            Some(b) => Code { kind: K_BOOL, a: b as u32, b: 0 },
            None => Code { kind: K_UBOOL, a: 0, b: 0 },
        }
    }

    /// A value's code.
    pub fn of(v: AV) -> Code {
        match v {
            AV::Num(n) => Code::num(n.as_raw_u32(), n.as_raw_u32()),
            AV::Ival(a, b) => Code::num(a.as_raw_u32(), b.as_raw_u32()),
            AV::Bool(b) => Code::boolean(Some(b)),
            AV::UBool => Code::boolean(None),
            AV::UNum => Code { kind: K_UNUM, a: 0, b: 0 },
            AV::Str(x) => Code { kind: K_STR, a: x, b: 0 },
            AV::Nil => Code { kind: K_NIL, a: 0, b: 0 },
            AV::Ptr(p) => Code { kind: K_PTR, a: p, b: 0 },
            AV::NilPtr => Code { kind: K_NILPTR, a: 0, b: 0 },
        }
    }

    /// A POSITION coordinate's code: its value less its low end's whole
    /// pixels (the cell holds those). Not a number: fatal (its cell would be
    /// `NO_CELL` and the position lost).
    pub fn of_pos(v: AV) -> Code {
        match v {
            AV::Num(n) => Code::pos(n.as_raw_u32(), n.as_raw_u32()),
            AV::Ival(a, b) => Code::pos(a.as_raw_u32(), b.as_raw_u32()),
            other => panic!("a position coordinate holds {other:?}, not a number: its cell and key would lose it"),
        }
    }

    /// `of_pos` of the raw bounds `lo`, `hi`.
    #[inline]
    pub fn pos(lo: u32, hi: u32) -> Code {
        let whole = lo & POS_WHOLE;
        Code::num(lo.wrapping_sub(whole), hi.wrapping_sub(whole))
    }

    /// A number's word for `FieldDict::find_num` (`kind == 0` only).
    #[inline]
    pub fn word(self) -> u64 {
        (self.a as u64) << 32 | self.b as u64
    }

    /// The value (position codes decode to their fraction).
    pub fn value(self) -> AV {
        use celeste_core::pico8_num::Pico8Num as P8;
        match self.kind {
            K_NUM if self.a == self.b => AV::Num(P8::from_raw(self.a as i32)),
            K_NUM => AV::Ival(P8::from_raw(self.a as i32), P8::from_raw(self.b as i32)),
            K_BOOL => AV::Bool(self.a != 0),
            K_UBOOL => AV::UBool,
            K_UNUM => AV::UNum,
            K_STR => AV::Str(self.a),
            K_NIL => AV::Nil,
            K_PTR => AV::Ptr(self.a),
            _ => AV::NilPtr,
        }
    }
}

/// Bits an index below `n` needs (0 for `n <= 1`).
#[inline]
fn bits_for(n: usize) -> u32 {
    if n <= 1 {
        0
    } else {
        usize::BITS - (n - 1).leading_zeros()
    }
}

/// One field's dictionary and its bits in the shape's key.
#[derive(Clone, Default, Debug)]
pub struct FieldDict {
    /// Index -> code.
    codes: Vec<Code>,
    /// Numbers: an open-addressing table, each slot holding its index's
    /// PLACED bits (`place`, kept current as runs are added).
    num_slots: Vec<NumSlot>,
    /// Numbers held.
    nums: usize,
    /// Every other code, by search.
    others: Vec<(Code, u32)>,
    /// `Bool(false)`, `Bool(true)`, `UBool`'s indices (`NONE`: absent) and
    /// placed bits.
    bools: [u32; 3],
    bool_placed: [u128; 3],
    /// The field's bit runs `(first bit, bits)`, low index bits first;
    /// adjacent runs merged.
    runs: Vec<(u8, u8)>,
    bits: u32,
}

/// A number's slot: its word, index + 1 (0: empty) and placed bits.
#[derive(Clone, Copy, Default, Debug)]
struct NumSlot {
    word: u64,
    idx1: u32,
    placed: u128,
}

impl FieldDict {
    fn new() -> Self {
        FieldDict { bools: [NONE; 3], ..Default::default() }
    }

    pub fn len(&self) -> usize {
        self.codes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.codes.is_empty()
    }

    pub fn code(&self, idx: u32) -> Code {
        self.codes[idx as usize]
    }

    #[inline]
    fn num_slot(&self, word: u64) -> Option<&NumSlot> {
        if self.num_slots.is_empty() {
            return None;
        }
        let m = self.num_slots.len() - 1;
        let mut i = (word.wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize & m;
        loop {
            let s = &self.num_slots[i];
            if s.idx1 == 0 {
                return None;
            }
            if s.word == word {
                return Some(s);
            }
            i = (i + 1) & m;
        }
    }

    /// A number's index (`NONE`: absent).
    #[inline]
    pub fn find_num(&self, word: u64) -> u32 {
        self.num_slot(word).map_or(NONE, |s| s.idx1 - 1)
    }

    /// A number's PLACED bits (`place` of its index; `None`: absent).
    #[inline]
    pub fn find_num_placed(&self, word: u64) -> Option<u128> {
        self.num_slot(word).map(|s| s.placed)
    }

    /// A boolean's placed bits: `0` false, `1` true, `2` unknown (`None`: absent).
    #[inline]
    pub fn find_bool_placed(&self, tri: usize) -> Option<u128> {
        (self.bools[tri] != NONE).then_some(self.bool_placed[tri])
    }

    /// Any code's index (`NONE`: absent).
    pub fn find(&self, c: Code) -> u32 {
        match c.kind {
            K_NUM => self.find_num(c.word()),
            K_BOOL => self.bools[c.a as usize],
            K_UBOOL => self.bools[2],
            _ => self.others.iter().find(|o| o.0 == c).map_or(NONE, |o| o.1),
        }
    }

    /// Index `idx` placed in the key's bits.
    #[inline]
    pub fn place(&self, idx: u32) -> u128 {
        match self.runs.len() {
            0 => 0,
            1 => (idx as u128) << self.runs[0].0,
            _ => {
                let (mut out, mut idx) = (0u128, idx as u128);
                for &(pos, len) in &self.runs {
                    out |= (idx & ((1u128 << len) - 1)) << pos;
                    idx >>= len;
                }
                out
            }
        }
    }

    /// Append `c` as the next index (absent: checked by the caller).
    fn push(&mut self, c: Code) {
        let idx = self.codes.len() as u32;
        self.codes.push(c);
        match c.kind {
            K_NUM => {
                self.nums += 1;
                let n = self.nums;
                if n * 2 > self.num_slots.len() {
                    let cap = (n * 2).next_power_of_two().max(4);
                    self.num_slots = vec![NumSlot::default(); cap];
                    for (i, c) in self.codes.iter().enumerate() {
                        if c.kind == K_NUM {
                            let placed = self.place(i as u32);
                            Self::put(&mut self.num_slots, c.word(), i as u32, placed);
                        }
                    }
                } else {
                    let placed = self.place(idx);
                    Self::put(&mut self.num_slots, c.word(), idx, placed);
                }
            }
            K_BOOL => {
                self.bools[c.a as usize] = idx;
                self.bool_placed[c.a as usize] = self.place(idx);
            }
            K_UBOOL => {
                self.bools[2] = idx;
                self.bool_placed[2] = self.place(idx);
            }
            _ => self.others.push((c, idx)),
        }
    }

    fn put(slots: &mut [NumSlot], word: u64, idx: u32, placed: u128) {
        let m = slots.len() - 1;
        let mut i = (word.wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize & m;
        while slots[i].idx1 != 0 {
            i = (i + 1) & m;
        }
        slots[i] = NumSlot { word, idx1: idx + 1, placed };
    }

    /// A new run: every code's placed bits recomputed.
    fn add_run(&mut self, pos: u8, len: u8) {
        match self.runs.last_mut() {
            Some(last) if last.0 + last.1 == pos => last.1 += len,
            _ => self.runs.push((pos, len)),
        }
        self.bits += len as u32;
        for i in 0..self.num_slots.len() {
            let s = self.num_slots[i];
            if s.idx1 != 0 {
                self.num_slots[i].placed = self.place(s.idx1 - 1);
            }
        }
        for t in 0..3 {
            if self.bools[t] != NONE {
                self.bool_placed[t] = self.place(self.bools[t]);
            }
        }
    }
}

/// A shape's SIGNATURE: exactly what `Rt2::shape_hash_of` hashes, so two
/// structures under one hash are told apart (a collision is fatal).
pub fn shape_sig(rt2: &Rt2) -> Vec<u32> {
    let mut s = Vec::with_capacity(rt2.structure.len() * 2 + rt2.globals.len() + 1);
    for cell in &rt2.structure {
        match cell {
            Cell2::Val => s.push(1),
            Cell2::Obj(fields) => {
                s.extend([2, fields.len() as u32]);
                for (k, t) in fields {
                    s.extend([*k, *t]);
                }
            }
            Cell2::Arr(items) => {
                s.extend([3, items.len() as u32]);
                s.extend(items.iter().copied());
            }
            Cell2::Unk => s.push(4),
            Cell2::Clo(f, caps) => s.extend([5, *f, caps.len() as u32]),
            Cell2::Bi(b) => s.extend([6, *b]),
        }
    }
    s.push(u32::MAX);
    s.extend(rt2.globals.iter().copied());
    s
}

/// A shape as the key space records it: its name (hash), signature and
/// position cells.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ShapeRecord {
    pub hash: u64,
    pub sig: Vec<u32>,
    pub pos: Option<(u32, u32)>,
}

impl ShapeRecord {
    /// The record of a CANONICAL block's shape.
    pub fn of(rt2: &Rt2, ids: &BoundaryIds) -> ShapeRecord {
        ShapeRecord { hash: rt2.shape_hash, sig: shape_sig(rt2), pos: rt2.position_cells(ids) }
    }
}

/// One shape's dictionaries (by cell; `None` for a cell that is not a value).
#[derive(Clone, Debug)]
pub struct ShapeKeys {
    pub record: ShapeRecord,
    fields: Vec<Option<FieldDict>>,
    next_bit: u32,
}

impl ShapeKeys {
    fn new(record: ShapeRecord) -> ShapeKeys {
        // The signature names each cell's kind: a `Val` is a lone 1.
        let mut fields = Vec::new();
        let sig = &record.sig;
        let mut i = 0;
        while sig[i] != u32::MAX {
            let (val, step) = match sig[i] {
                1 => (true, 1),
                2 => (false, 2 + 2 * sig[i + 1] as usize),
                3 => (false, 2 + sig[i + 1] as usize),
                4 => (false, 1),
                5 => (false, 3),
                _ => (false, 2),
            };
            fields.push(val.then(FieldDict::new));
            i += step;
        }
        ShapeKeys { record, fields, next_bit: 0 }
    }

    /// Cell `cell`'s dictionary (a value cell).
    #[inline]
    pub fn field(&self, cell: usize) -> Option<&FieldDict> {
        self.fields.get(cell).and_then(|f| f.as_ref())
    }

    /// Is `cell` a position coordinate?
    #[inline]
    pub fn is_pos(&self, cell: u32) -> bool {
        self.record.pos.is_some_and(|(x, y)| cell == x || cell == y)
    }

    /// The key bits used.
    pub fn bits(&self) -> u32 {
        self.next_bit
    }

    /// The value cells, in order.
    pub fn value_cells(&self) -> impl Iterator<Item = u32> + '_ {
        self.fields.iter().enumerate().filter(|(_, f)| f.is_some()).map(|(c, _)| c as u32)
    }

    /// The code of cell `cell` at `lane` of a block of this shape.
    #[inline]
    fn code_at(&self, rt2: &Rt2, cell: u32, lane: usize) -> Code {
        let v = rt2.cols[cell as usize].at(lane);
        if self.is_pos(cell) {
            Code::of_pos(v)
        } else {
            Code::of(v)
        }
    }

    /// The key of `codes` (by value cell, in order), all present.
    pub fn key_of_codes(&self, codes: &[(u32, Code)]) -> Option<Key> {
        let mut k = 0u128;
        for &(c, code) in codes {
            let f = self.field(c as usize)?;
            let i = f.find(code);
            if i == NONE {
                return None;
            }
            k |= f.place(i);
        }
        Some(to_key(k))
    }

    /// The fields' codes of a key (the inverse of the packing).
    pub fn decode(&self, k: Key) -> Vec<(u32, Code)> {
        let k = of_key(k);
        let mut out = Vec::new();
        for (c, f) in self.fields.iter().enumerate() {
            let Some(f) = f else { continue };
            let (mut idx, mut at) = (0u128, 0u32);
            for &(pos, len) in &f.runs {
                idx |= (k >> pos & ((1u128 << len) - 1)) << at;
                at += len as u32;
            }
            out.push((c as u32, f.codes.get(idx as usize).copied().unwrap_or(Code { kind: u8::MAX, a: 0, b: 0 })));
        }
        out
    }
}

/// FINGERPRINTS over exact CONTENT: a key's decoded codes, hashed - so
/// two trees whose dictionaries grew in different orders (a raise appends
/// its codes after later frames') fingerprint the same states alike. Per
/// shape the fields without bits hashed once, and per field with bits a
/// hash per index.
pub struct ContentHash {
    shapes: FxHashMap<u64, (u64, Vec<(Vec<(u8, u8)>, Vec<u64>)>)>,
}

impl ContentHash {
    #[inline]
    fn code_hash(cell: u32, c: Code) -> u64 {
        mix64(mix64((cell as u64) << 8 | c.kind as u64) ^ ((c.a as u64) << 32 | c.b as u64))
    }

    /// The content hash of `shape`'s key `k` (a key the space holds).
    pub fn key(&self, shape: u64, k: Key) -> u64 {
        let (fixed, fields) = self.shapes.get(&shape).unwrap_or_else(|| panic!("exact keys: fingerprinting a key of the unknown shape {shape:#x}"));
        let k = of_key(k);
        let mut acc = *fixed;
        for (runs, hashes) in fields {
            let (mut idx, mut at) = (0u128, 0u32);
            for &(pos, len) in runs {
                idx |= (k >> pos & ((1u128 << len) - 1)) << at;
                at += len as u32;
            }
            acc = acc.wrapping_add(*hashes.get(idx as usize).unwrap_or_else(|| panic!("exact keys: a key of shape {shape:#x} indexes past its dictionary")));
        }
        mix64(shape ^ acc)
    }

    /// The content hash of a state `(shape, key, cell)`.
    #[inline]
    pub fn state(&self, shape: u64, k: Key, cell: u32) -> u64 {
        mix64(self.key(shape, k) ^ (cell as u64) << 1)
    }
}

/// A row's key against a key space: packed, or (a code is missing) its
/// exact content.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum RowKey {
    Hit(Key),
    Miss(MissKey),
}

/// The exact content of a state some dictionary lacks a code of: the packed
/// fields that hit and the (cell, code) of those that missed, by cell.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct MissKey {
    pub shape: u64,
    pub partial: Key,
    pub missing: Vec<(u32, Code)>,
}

/// What a key space gained: shapes, codes (with their indices) and bit runs
/// (with their places). Recorded per frame; a replay is order-free.
#[derive(Clone, Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct KeyAdditions {
    pub shapes: Vec<ShapeRecord>,
    /// `(shape, cell, index, code)`.
    pub codes: Vec<(u64, u32, u32, Code)>,
    /// `(shape, cell, run number of the field, first bit, bits)`.
    pub runs: Vec<(u64, u32, u32, u8, u8)>,
}

impl KeyAdditions {
    pub fn is_empty(&self) -> bool {
        self.shapes.is_empty() && self.codes.is_empty() && self.runs.is_empty()
    }

    pub fn extend(&mut self, other: KeyAdditions) {
        self.shapes.extend(other.shapes);
        self.codes.extend(other.codes);
        self.runs.extend(other.runs);
    }
}

/// Unique generations across every key space of the process.
static GEN: AtomicU64 = AtomicU64::new(1);

/// A tree's dictionaries (module doc).
#[derive(Clone, Debug)]
pub struct KeySpace {
    shapes: FxHashMap<u64, ShapeKeys>,
    /// Per field its run count (the next run's number), for the records.
    run_counts: FxHashMap<(u64, u32), u32>,
    gen: u64,
}

impl Default for KeySpace {
    fn default() -> Self {
        KeySpace { shapes: Default::default(), run_counts: Default::default(), gen: GEN.fetch_add(1, Ordering::Relaxed) }
    }
}

impl KeySpace {
    pub fn new() -> Self {
        Self::default()
    }

    /// Changes whenever the dictionaries do (unique in the process).
    #[inline]
    pub fn generation(&self) -> u64 {
        self.gen
    }

    fn bump(&mut self) {
        self.gen = GEN.fetch_add(1, Ordering::Relaxed);
    }

    #[inline]
    pub fn shape(&self, hash: u64) -> Option<&ShapeKeys> {
        self.shapes.get(&hash)
    }

    pub fn shapes(&self) -> impl Iterator<Item = &ShapeKeys> {
        self.shapes.values()
    }

    /// The widest shape's key bits.
    pub fn max_bits(&self) -> u32 {
        self.shapes.values().map(|s| s.next_bit).max().unwrap_or(0)
    }

    /// Codes held, over every field.
    pub fn codes(&self) -> usize {
        self.shapes.values().flat_map(|s| s.fields.iter().flatten()).map(|f| f.len()).sum()
    }

    /// Register a shape (or check a known one's signature: two structures
    /// under one hash are FATAL); true if new.
    pub fn add_shape(&mut self, record: &ShapeRecord, adds: &mut KeyAdditions) -> bool {
        if let Some(s) = self.shapes.get(&record.hash) {
            assert!(s.record == *record, "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", record.hash);
            return false;
        }
        self.shapes.insert(record.hash, ShapeKeys::new(record.clone()));
        adds.shapes.push(record.clone());
        self.bump();
        true
    }

    /// Append `codes` (sorted, distinct, none present) to `shape`'s field
    /// `cell`, giving it bits if it needs more: a new run at the shape's
    /// next free bit. Recorded in `adds`. Past `KEY_BITS`: FATAL.
    pub fn add_codes(&mut self, shape: u64, cell: u32, codes: &[Code], adds: &mut KeyAdditions) {
        if codes.is_empty() {
            return;
        }
        let s = self.shapes.get_mut(&shape).unwrap_or_else(|| panic!("exact keys: codes for the unknown shape {shape:#x}"));
        let next_bit = s.next_bit;
        let f = s.fields.get_mut(cell as usize).and_then(|f| f.as_mut()).unwrap_or_else(|| panic!("exact keys: shape {shape:#x} cell {cell} is not a value"));
        for w in codes.windows(2) {
            assert!(w[0] < w[1], "exact keys: codes added unsorted");
        }
        for &c in codes {
            assert!(f.find(c) == NONE, "exact keys: shape {shape:#x} cell {cell}: code {c:?} added twice");
            adds.codes.push((shape, cell, f.codes.len() as u32, c));
            f.push(c);
        }
        let need = bits_for(f.len());
        if need > f.bits {
            let len = need - f.bits;
            assert!(
                next_bit + len <= KEY_BITS,
                "exact keys: shape {shape:#x} needs {} key bits, past the {KEY_BITS} a key holds (cell {cell}, {} codes)",
                next_bit + len,
                f.len()
            );
            f.add_run(next_bit as u8, len as u8);
            let n = self.run_counts.entry((shape, cell)).or_default();
            adds.runs.push((shape, cell, *n, next_bit as u8, len as u8));
            *n += 1;
            s.next_bit = next_bit + len;
        }
        self.bump();
    }

    /// Replay recorded additions (in any order; from several frames at
    /// once): indices dense per field, runs disjoint per shape.
    pub fn apply(&mut self, mut adds: KeyAdditions) -> anyhow::Result<()> {
        let mut scratch = KeyAdditions::default();
        for r in &adds.shapes {
            self.add_shape(r, &mut scratch);
        }
        adds.codes.sort_unstable_by_key(|&(s, c, i, _)| (s, c, i));
        for &(shape, cell, idx, code) in &adds.codes {
            let s = self.shapes.get_mut(&shape).ok_or_else(|| anyhow::anyhow!("exact keys: a code of the unknown shape {shape:#x}"))?;
            let f = s.fields.get_mut(cell as usize).and_then(|f| f.as_mut()).ok_or_else(|| anyhow::anyhow!("exact keys: shape {shape:#x} cell {cell} is not a value"))?;
            anyhow::ensure!(idx as usize == f.len(), "exact keys: shape {shape:#x} cell {cell}: index {idx} recorded after {} codes", f.len());
            anyhow::ensure!(f.find(code) == NONE, "exact keys: shape {shape:#x} cell {cell}: code {code:?} recorded twice");
            f.push(code);
        }
        adds.runs.sort_unstable_by_key(|&(s, c, n, _, _)| (s, c, n));
        let mut used: FxHashMap<u64, u128> = FxHashMap::default();
        for &(shape, cell, n, pos, len) in &adds.runs {
            let s = self.shapes.get_mut(&shape).ok_or_else(|| anyhow::anyhow!("exact keys: a run of the unknown shape {shape:#x}"))?;
            let f = s.fields.get_mut(cell as usize).and_then(|f| f.as_mut()).ok_or_else(|| anyhow::anyhow!("exact keys: shape {shape:#x} cell {cell} is not a value"))?;
            let count = self.run_counts.entry((shape, cell)).or_default();
            anyhow::ensure!(n == *count, "exact keys: shape {shape:#x} cell {cell}: run {n} recorded after {count}");
            *count += 1;
            anyhow::ensure!(len > 0 && pos as u32 + len as u32 <= KEY_BITS, "exact keys: shape {shape:#x}: a run of bits {pos}+{len}");
            let bits = ((1u128 << len) - 1) << pos;
            let u = used.entry(shape).or_default();
            anyhow::ensure!(*u & bits == 0, "exact keys: shape {shape:#x}: two runs share bits");
            *u |= bits;
            f.add_run(pos, len);
            s.next_bit = s.next_bit.max(pos as u32 + len as u32);
        }
        for s in self.shapes.values() {
            for (c, f) in s.fields.iter().enumerate() {
                if let Some(f) = f {
                    anyhow::ensure!(f.bits >= bits_for(f.len()), "exact keys: shape {:#x} cell {c}: {} codes in {} bits", s.record.hash, f.len(), f.bits);
                }
            }
        }
        self.bump();
        Ok(())
    }

    /// The content hasher of the space as it is.
    pub fn content_hash(&self) -> ContentHash {
        let shapes = self
            .shapes
            .iter()
            .map(|(&h, s)| {
                let mut fixed = 0u64;
                let mut fields = Vec::new();
                for (c, f) in s.fields.iter().enumerate() {
                    let Some(f) = f else { continue };
                    let hashes: Vec<u64> = f.codes.iter().map(|&code| ContentHash::code_hash(c as u32, code)).collect();
                    if f.runs.is_empty() {
                        // No bits: index 0 (a field never stored holds no code).
                        fixed = fixed.wrapping_add(hashes.first().copied().unwrap_or(0));
                    } else {
                        fields.push((f.runs.clone(), hashes));
                    }
                }
                (h, (fixed, fields))
            })
            .collect();
        ContentHash { shapes }
    }

    /// Everything as additions (a marks file carries its tree's key space).
    pub fn to_additions(&self) -> KeyAdditions {
        let mut hashes: Vec<u64> = self.shapes.keys().copied().collect();
        hashes.sort_unstable();
        let mut out = KeyAdditions::default();
        for h in hashes {
            let s = &self.shapes[&h];
            out.shapes.push(s.record.clone());
            for (c, f) in s.fields.iter().enumerate() {
                let Some(f) = f else { continue };
                for (i, &code) in f.codes.iter().enumerate() {
                    out.codes.push((h, c as u32, i as u32, code));
                }
                // Merged runs replay as one run each.
                for (n, &(pos, len)) in f.runs.iter().enumerate() {
                    out.runs.push((h, c as u32, n as u32, pos, len));
                }
            }
        }
        out
    }

    /// A key space from additions.
    pub fn from_additions(adds: KeyAdditions) -> anyhow::Result<KeySpace> {
        let mut k = KeySpace::new();
        k.apply(adds)?;
        Ok(k)
    }

    /// Every row's key of a CANONICAL block (ids canonical, `shape_hash`
    /// set): packed where every code is in the dictionaries, else its exact
    /// content. A block whose shape is known must have its signature.
    pub fn row_keys(&self, rt2: &Rt2, ids: &BoundaryIds) -> Vec<RowKey> {
        let Some(s) = self.shapes.get(&rt2.shape_hash) else {
            // An unknown shape: every field is missing.
            let rec = ShapeRecord::of(rt2, ids);
            let s = ShapeKeys::new(rec);
            return (0..rt2.width)
                .map(|lane| RowKey::Miss(MissKey { shape: rt2.shape_hash, partial: (0, 0), missing: s.value_cells().map(|c| (c, s.code_at(rt2, c, lane))).collect() }))
                .collect();
        };
        assert!(s.record.sig == shape_sig(rt2), "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", rt2.shape_hash);
        let cells: Vec<u32> = s.value_cells().collect();
        (0..rt2.width)
            .map(|lane| {
                let (mut k, mut missing) = (0u128, Vec::new());
                for &c in &cells {
                    let code = s.code_at(rt2, c, lane);
                    let f = s.field(c as usize).expect("a value cell");
                    match f.find(code) {
                        NONE => missing.push((c, code)),
                        i => k |= f.place(i),
                    }
                }
                if missing.is_empty() {
                    RowKey::Hit(to_key(k))
                } else {
                    RowKey::Miss(MissKey { shape: rt2.shape_hash, partial: to_key(k), missing })
                }
            })
            .collect()
    }

    /// Every row's key of a CANONICAL block, `None` where a code is missing
    /// (no state of the tree is that row).
    pub fn lookup_keys(&self, rt2: &Rt2, ids: &BoundaryIds) -> Vec<Option<Key>> {
        self.row_keys(rt2, ids)
            .into_iter()
            .map(|r| match r {
                RowKey::Hit(k) => Some(k),
                RowKey::Miss(_) => None,
            })
            .collect()
    }

    /// Add every code of the rows of `blocks` (CANONICAL) the dictionaries
    /// lack - per field SORTED - and their shapes: frame 0's seeding.
    pub fn absorb(&mut self, blocks: &[&Rt2], ids: &BoundaryIds) -> KeyAdditions {
        let mut adds = KeyAdditions::default();
        let mut shapes: Vec<&Rt2> = blocks.to_vec();
        shapes.sort_by_key(|b| b.shape_hash);
        for b in &shapes {
            self.add_shape(&ShapeRecord::of(b, ids), &mut adds);
        }
        let mut new: std::collections::BTreeMap<(u64, u32), std::collections::BTreeSet<Code>> = Default::default();
        for b in blocks {
            for r in self.row_keys(b, ids) {
                if let RowKey::Miss(m) = r {
                    for (c, code) in m.missing {
                        new.entry((m.shape, c)).or_default().insert(code);
                    }
                }
            }
        }
        for ((shape, cell), codes) in new {
            self.add_codes(shape, cell, &codes.into_iter().collect::<Vec<_>>(), &mut adds);
        }
        adds
    }

    /// The final key of a miss, once its codes are in.
    pub fn complete(&self, m: &MissKey) -> Key {
        let s = self.shapes.get(&m.shape).unwrap_or_else(|| panic!("exact keys: completing a key of the unknown shape {:#x}", m.shape));
        let rest = s.key_of_codes(&m.missing).unwrap_or_else(|| panic!("exact keys: completing a key whose codes are not all in"));
        to_key(of_key(m.partial) | of_key(rest))
    }
}

/// Shards of `Provisional`.
const PROV_SHARDS: usize = 256;

/// A wave's PROVISIONAL keys: each distinct miss content interned to
/// `PROVISIONAL | shard << 32` (high word), local number (low word). Ids
/// depend on scheduling; the translation turns them into final keys.
pub struct Provisional {
    shards: Vec<Mutex<(FxHashMap<MissKey, u32>, Vec<MissKey>)>>,
    /// The shapes the key space lacks that misses were made of.
    shapes: Mutex<FxHashMap<u64, ShapeRecord>>,
}

impl Default for Provisional {
    fn default() -> Self {
        Provisional { shards: (0..PROV_SHARDS).map(|_| Default::default()).collect(), shapes: Default::default() }
    }
}

impl Provisional {
    /// A shape the key space lacks, which misses are made of.
    pub fn note_shape(&self, r: &ShapeRecord) {
        let mut g = self.shapes.lock().expect("the provisional shapes");
        match g.get(&r.hash) {
            Some(x) => assert!(x == r, "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", r.hash),
            None => {
                g.insert(r.hash, r.clone());
            }
        }
    }

    /// A noted shape's record.
    pub fn shape_record(&self, hash: u64) -> Option<ShapeRecord> {
        self.shapes.lock().expect("the provisional shapes").get(&hash).cloned()
    }

    /// The provisional key of `m`.
    pub fn intern(&self, m: MissKey) -> Key {
        use std::hash::{BuildHasher, Hash, Hasher};
        let mut h = rustc_hash::FxBuildHasher.build_hasher();
        m.hash(&mut h);
        let shard = (mix64(h.finish()) >> 56) as usize & (PROV_SHARDS - 1);
        let mut g = self.shards[shard].lock().expect("a provisional shard");
        let (map, list) = &mut *g;
        let n = match map.get(&m) {
            Some(&n) => n,
            None => {
                let n = list.len() as u32;
                list.push(m.clone());
                map.insert(m, n);
                n
            }
        };
        (PROVISIONAL | shard as u64, n as u64)
    }

    /// A provisional key's content.
    pub fn content(&self, k: Key) -> MissKey {
        debug_assert!(is_provisional(k));
        let g = self.shards[(k.0 & !PROVISIONAL) as usize].lock().expect("a provisional shard");
        g.1[k.1 as usize].clone()
    }
}

/// A whole state's canonical bytes: its shape's name and every value
/// cell's code (a position coordinate less its whole pixels: the cell is
/// beside it). Equal bytes = equal states; no tree needed.
pub type ExactRow = Box<[u8]>;

/// The process's shapes by name, for `exact_rows` (a second structure under
/// one name is FATAL).
static SIGS: std::sync::OnceLock<Mutex<FxHashMap<u64, std::sync::Arc<Vec<u32>>>>> = std::sync::OnceLock::new();

/// Every row's `ExactRow` of a CANONICAL block.
pub fn exact_rows(rt2: &Rt2, ids: &BoundaryIds) -> Vec<ExactRow> {
    let sig = shape_sig(rt2);
    {
        let mut g = SIGS.get_or_init(Default::default).lock().expect("the shape registry");
        match g.get(&rt2.shape_hash) {
            Some(s) => assert!(**s == sig, "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", rt2.shape_hash),
            None => {
                g.insert(rt2.shape_hash, std::sync::Arc::new(sig));
            }
        }
    }
    let pos = rt2.position_cells(ids);
    let cells: Vec<u32> = (0..rt2.structure.len() as u32).filter(|&c| matches!(rt2.structure[c as usize], Cell2::Val)).collect();
    (0..rt2.width)
        .map(|lane| {
            let mut out = Vec::with_capacity(8 + cells.len() * 5);
            out.extend_from_slice(&rt2.shape_hash.to_le_bytes());
            for &c in &cells {
                let v = rt2.cols[c as usize].at(lane);
                let code = if pos.is_some_and(|(x, y)| c == x || c == y) { Code::of_pos(v) } else { Code::of(v) };
                match code.kind {
                    K_NUM if code.a == code.b => {
                        out.push(0);
                        out.extend_from_slice(&code.a.to_le_bytes());
                    }
                    K_NUM => {
                        out.push(1);
                        out.extend_from_slice(&code.a.to_le_bytes());
                        out.extend_from_slice(&code.b.to_le_bytes());
                    }
                    K_STR | K_PTR => {
                        out.push(2 + code.kind);
                        out.extend_from_slice(&code.a.to_le_bytes());
                    }
                    k => out.push(16 + k * 2 + code.a as u8),
                }
            }
            out.into_boxed_slice()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn n(x: i32) -> Code {
        Code::num(x as u32, x as u32)
    }

    /// Keys never move as dictionaries grow, stay distinct, and a replay of
    /// the records (in any order) rebuilds the same keys.
    #[test]
    fn keys_are_stable_exact_and_replayable() {
        let rec = ShapeRecord { hash: 7, sig: vec![1, 1, 4, 1, u32::MAX], pos: None };
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        assert!(ks.add_shape(&rec, &mut adds));
        ks.add_codes(7, 0, &[n(5)], &mut adds);
        ks.add_codes(7, 1, &[Code::boolean(Some(false)), Code::boolean(Some(true))], &mut adds);
        ks.add_codes(7, 3, &[n(1), n(2), n(3)], &mut adds);
        let s = ks.shape(7).unwrap();
        let key = |a: Code, b: Code, c: Code| s.key_of_codes(&[(0, a), (1, b), (3, c)]).unwrap();
        let k1 = key(n(5), Code::boolean(Some(true)), n(3));
        let first = adds.clone();
        // Growth: a constant field becomes two codes, another past 4 codes.
        let mut more = KeyAdditions::default();
        ks.add_codes(7, 0, &[n(9)], &mut more);
        ks.add_codes(7, 3, &[n(4), n(7)], &mut more);
        ks.add_codes(7, 1, &[Code::boolean(None)], &mut more);
        let s = ks.shape(7).unwrap();
        assert_eq!(s.key_of_codes(&[(0, n(5)), (1, Code::boolean(Some(true))), (3, n(3))]), Some(k1), "an old key never moves");
        let mut all = std::collections::HashSet::new();
        for a in [n(5), n(9)] {
            for b in [Code::boolean(Some(false)), Code::boolean(Some(true)), Code::boolean(None)] {
                for c in [n(1), n(2), n(3), n(4), n(7)] {
                    let k = s.key_of_codes(&[(0, a), (1, b), (3, c)]).unwrap();
                    assert!(all.insert(k), "distinct tuples, distinct keys");
                    assert_eq!(s.decode(k), vec![(0, a), (1, b), (3, c)]);
                }
            }
        }
        assert_eq!(s.key_of_codes(&[(0, n(6)), (1, Code::boolean(None)), (3, n(1))]), None);
        // The later frame's records first: the replay is order-free.
        let mut replay = more.clone();
        replay.extend(first);
        let r = KeySpace::from_additions(replay).unwrap();
        let rs = r.shape(7).unwrap();
        for a in [n(5), n(9)] {
            for c in [n(1), n(4), n(7)] {
                let codes = [(0, a), (1, Code::boolean(None)), (3, c)];
                assert_eq!(rs.key_of_codes(&codes), s.key_of_codes(&codes));
            }
        }
        assert_eq!(KeySpace::from_additions(ks.to_additions()).unwrap().shape(7).unwrap().key_of_codes(&[(0, n(9)), (1, Code::boolean(None)), (3, n(7))]), s.key_of_codes(&[(0, n(9)), (1, Code::boolean(None)), (3, n(7))]));
    }

    /// A number and its point interval are one code; a position keeps only
    /// its fraction.
    #[test]
    fn codes_are_exact() {
        use celeste_core::pico8_num::Pico8Num as P8;
        let x = P8::from_raw(0x0003_8000);
        assert_eq!(Code::of(AV::Num(x)), Code::of(AV::Ival(x, x)));
        assert_ne!(Code::of(AV::Ival(x, P8::from_raw(0x0003_8001))), Code::of(AV::Ival(x, P8::from_raw(0x0003_8002))));
        assert_eq!(Code::of_pos(AV::Num(x)), Code::num(0x8000, 0x8000));
        assert_eq!(Code::of_pos(AV::Ival(x, P8::from_raw(0x0005_0000))), Code::num(0x8000, 0x0002_0000));
        assert_ne!(Code::of(AV::Bool(false)), Code::of(AV::UBool));
    }

    /// A number and its point interval are one code; a wider interval is
    /// neither end.
    #[test]
    fn a_point_interval_codes_as_its_number() {
        use celeste_core::pico8_num::Pico8Num as P8;
        for raw in [0i32, 1, -1, 64 << 16, -(5 << 15), i32::MAX, i32::MIN] {
            let x = P8::from_raw(raw);
            assert_eq!(Code::of(AV::Ival(x, x)), Code::of(AV::Num(x)), "raw {raw:#x}");
        }
        let (a, b) = (P8::from_raw(0), P8::from_raw(1));
        assert_ne!(Code::of(AV::Ival(a, b)), Code::of(AV::Num(a)));
        assert_ne!(Code::of(AV::Ival(a, b)), Code::of(AV::Num(b)));
        assert_ne!(Code::of(AV::Ival(a, b)), Code::of(AV::Ival(b, b)));
    }

    /// A position coordinate codes without its low end's whole pixels: every
    /// integer alike, an interval by its offsets from its low pixel, and a
    /// fraction is still the key's (the cell holds only `flr`).
    #[test]
    fn a_position_codes_without_its_whole_pixels() {
        use celeste_core::pico8_num::Pico8Num as P8;
        let px = |raw: i32| P8::from_raw(raw);
        for whole in [-64i32, -1, 0, 5, 300] {
            assert_eq!(Code::of_pos(AV::Num(px(whole << 16))), Code::of_pos(AV::Num(px(0))), "x = {whole}");
            assert_eq!(Code::of_pos(AV::Num(px((whole << 16) + 0x4000))), Code::of_pos(AV::Num(px(0x4000))), "x = {whole}.25");
            let iv = AV::Ival(px((whole << 16) + 0x8000), px(((whole + 2) << 16) + 0x4000));
            assert_eq!(Code::of_pos(iv), Code::of_pos(AV::Ival(px(0x8000), px((2 << 16) + 0x4000))), "[{whole}.5, {}.25]", whole + 2);
            assert_eq!(Code::of_pos(AV::Ival(px(whole << 16), px(whole << 16))), Code::of_pos(AV::Num(px(0))));
        }
        assert_ne!(Code::of_pos(AV::Num(px(0x4000))), Code::of_pos(AV::Num(px(0))), "a fraction is the key's");
        assert_ne!(Code::of_pos(AV::Ival(px(0), px(1 << 16))), Code::of_pos(AV::Ival(px(0), px(2 << 16))), "an interval's width is the key's");
    }

    /// Past 127 bits is fatal, never truncated.
    #[test]
    #[should_panic(expected = "past the 127 a key holds")]
    fn a_key_past_its_bits_is_fatal() {
        let sig: Vec<u32> = std::iter::repeat_n(1, 9).chain([u32::MAX]).collect();
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        ks.add_shape(&ShapeRecord { hash: 1, sig, pos: None }, &mut adds);
        let codes: Vec<Code> = (0..1 << 15).map(n).collect();
        for c in 0..9 {
            ks.add_codes(1, c, &codes, &mut adds);
        }
    }
}
