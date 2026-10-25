//! EXACT KEYS (plans/architecture.md, "Keys"): a state's identity within its
//! shape is the PACKED tuple of its fields' dictionary indices, not a hash.
//!
//!   * `Code`       - a value's exact code (a number and `[n, n]` alike; a
//!                    position coordinate less its low end's whole pixels)
//!   * `FieldDict`  - per (shape, cell): code <-> index, append-only, and the
//!                    field's BIT RUNS in the shape's 127-bit key
//!   * `KeySpace`   - a tree's dictionaries, grown only through recorded
//!                    `KeyAdditions` (sorted per frame: canonical)
//!   * `Provisional`- a wave's interned keys of states holding a code no
//!                    dictionary has yet (new by construction)
//!   * `ExactRow`   - a whole state's canonical bytes (the concrete search's
//!                    dedup, independent of any tree)
//!   * `Deps`       - a FUNCTIONAL DEPENDENCY a shape declares:
//!                    FOLLOWER cells whose codes are a function of a LEADER
//!                    cell's; they carry no key bits (`Follow`)
//!
//! Bits once given to a field never move and an old index is zero in a
//! field's later bits, so a key never changes as the dictionaries grow.
//!
//! A follower is EXACT, not widened: its code is recorded per leader index
//! (`Follow`) when the leader's code is added, and every row whose leader
//! hits has its followers CHECKED against that record wherever a key is
//! formed (`KeySpace::row_keys`, `complete`, `add_misses`; the kernels'
//! emission). A row that differs from the record is FATAL: two states that
//! differ only in a follower would share a key, a merge nothing refutes.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use celeste_core::arith::Arithmetic;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::runtime2::{mix64, Rt2, AV, P8};
use crate::shape::{Anchors, Cell, KeyRole, Layout};

/// A packed key: (bits 64..127, bits 0..63). Bit 127 tags a PROVISIONAL key.
pub type Key = (u64, u64);

pub const KEY_BITS: u32 = 127;

pub const PROVISIONAL: u64 = 1 << 63;

/// No index (a code not in the dictionary).
pub const NONE: u32 = u32::MAX;

#[inline]
pub fn is_provisional(k: Key) -> bool {
    k.0 & PROVISIONAL != 0
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
/// `Ival(n, n)` are one code, as producers disagree on column types); the
/// raws are the arithmetic's (`Arithmetic::Raw`).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
#[serde(bound = "")]
pub struct Code<N: Arithmetic = P8> {
    pub kind: u8,
    pub a: N::Raw,
    pub b: N::Raw,
}

impl<N: Arithmetic> PartialOrd for Code<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<N: Arithmetic> Ord for Code<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.kind, self.a, self.b).cmp(&(other.kind, other.a, other.b))
    }
}

const K_NUM: u8 = 0;
const K_BOOL: u8 = 1;
const K_UBOOL: u8 = 2;
const K_UNUM: u8 = 3;
const K_STR: u8 = 4;
const K_NIL: u8 = 5;
const K_PTR: u8 = 6;

/// A non-number code's `u32` (a string, a pointer, a boolean).
#[inline]
fn small<N: Arithmetic>(r: N::Raw) -> u32 {
    r.try_into().ok().expect("a non-number code's word is a u32")
}

impl<N: Arithmetic> Code<N> {
    #[inline]
    pub const fn num(lo: N::Raw, hi: N::Raw) -> Code<N> {
        Code { kind: K_NUM, a: lo, b: hi }
    }

    #[inline]
    fn other(kind: u8, a: u32) -> Code<N> {
        Code { kind, a: a.into(), b: 0.into() }
    }

    /// `Bool(b)`, or `UBool` for `None`.
    #[inline]
    pub fn boolean(b: Option<bool>) -> Code<N> {
        match b {
            Some(b) => Code::other(K_BOOL, b as u32),
            None => Code::other(K_UBOOL, 0),
        }
    }

    pub fn of(v: AV<N>) -> Code<N> {
        match v {
            AV::Num(n) => Code::num(n.raw(), n.raw()),
            AV::Ival(a, b) => Code::num(a.raw(), b.raw()),
            AV::Bool(b) => Code::boolean(Some(b)),
            AV::UBool => Code::boolean(None),
            AV::UNum => Code::other(K_UNUM, 0),
            AV::Str(x) => Code::other(K_STR, x),
            AV::Nil => Code::other(K_NIL, 0),
            AV::Ptr(p) => Code::other(K_PTR, p),
        }
    }

    /// A POSITION coordinate's code: its value less its low end's whole
    /// pixels (the cell holds those). Not a number: fatal (its cell would be
    /// `NO_CELL` and the position lost).
    pub fn of_pos(v: AV<N>) -> Code<N> {
        let (lo, hi) = match v {
            AV::Num(n) => (n, n),
            AV::Ival(a, b) => (a, b),
            other => panic!("a position coordinate holds {other:?}, not a number: its cell and key would lose it"),
        };
        let whole = lo.whole_i16().unwrap_or_else(|| panic!("a position coordinate {lo:?} past the i16 pixels"));
        Code::num(lo.less_whole(whole), hi.less_whole(whole))
    }

    /// A number (or interval) code?
    #[inline]
    pub fn is_num(self) -> bool {
        self.kind == K_NUM
    }

    /// A number's word for `FieldDict::find_num` (`kind == 0` only).
    #[inline]
    pub fn word(self) -> N::Word {
        N::word(self.a, self.b)
    }

    /// The value (a position code decodes to its fraction).
    pub fn value(self) -> AV<N> {
        let num = |r: N::Raw| N::from_raw(r).expect("a number code's raw is a number");
        match self.kind {
            K_NUM if self.a == self.b => AV::Num(num(self.a)),
            K_NUM => AV::Ival(num(self.a), num(self.b)),
            K_BOOL => AV::Bool(small::<N>(self.a) != 0),
            K_UBOOL => AV::UBool,
            K_UNUM => AV::UNum,
            K_STR => AV::Str(small::<N>(self.a)),
            K_NIL => AV::Nil,
            K_PTR => AV::Ptr(small::<N>(self.a)),
            k => panic!("exact keys: a code of no kind ({k})"),
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
#[derive(Clone, Debug)]
pub struct FieldDict<N: Arithmetic = P8> {
    codes: Vec<Code<N>>,
    /// Numbers: an open-addressing table, each slot holding its index's
    /// PLACED bits (`place`, kept current as runs are added).
    num_slots: Vec<NumSlot<N>>,
    nums: usize,
    /// While `nums <= SMALL`: the numbers' words and placed bits, searched
    /// linearly (most fields hold a handful of codes).
    small: [(N::Word, u128); SMALL],
    others: Vec<(Code<N>, u32)>,
    /// `Bool(false)`, `Bool(true)`, `UBool`'s indices (`NONE`: absent) and
    /// placed bits.
    bools: [u32; 3],
    bool_placed: [u128; 3],
    /// The field's bit runs `(first bit, bits)`, low index bits first;
    /// adjacent runs merged.
    runs: Vec<(u8, u8)>,
    bits: u32,
}

/// Numbers a field searches linearly (`FieldDict::small`).
const SMALL: usize = 4;

/// A number's slot: its word, index + 1 (0: empty) and placed bits.
#[derive(Clone, Copy, Debug)]
struct NumSlot<N: Arithmetic> {
    word: N::Word,
    idx1: u32,
    placed: u128,
}

impl<N: Arithmetic> Default for NumSlot<N> {
    fn default() -> Self {
        NumSlot { word: Default::default(), idx1: 0, placed: 0 }
    }
}

impl<N: Arithmetic> FieldDict<N> {
    fn new() -> Self {
        FieldDict {
            codes: Vec::new(),
            num_slots: Vec::new(),
            nums: 0,
            small: [Default::default(); SMALL],
            others: Vec::new(),
            bools: [NONE; 3],
            bool_placed: [0; 3],
            runs: Vec::new(),
            bits: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.codes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.codes.is_empty()
    }

    pub fn code(&self, idx: u32) -> Code<N> {
        self.codes[idx as usize]
    }

    #[inline]
    fn num_slot(&self, word: N::Word) -> Option<&NumSlot<N>> {
        if self.num_slots.is_empty() {
            return None;
        }
        let m = self.num_slots.len() - 1;
        let mut i = (N::fold(word).wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize & m;
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

    #[inline]
    pub fn find_num(&self, word: N::Word) -> u32 {
        self.num_slot(word).map_or(NONE, |s| s.idx1 - 1)
    }

    /// The numbers as `(word, placed bits)` while there are at most
    /// `SMALL` of them (the emission matches them lane-parallel).
    #[inline]
    pub fn small_nums(&self) -> Option<&[(N::Word, u128)]> {
        (self.nums <= SMALL).then(|| &self.small[..self.nums])
    }

    /// A number's PLACED bits (`place` of its index; `None`: absent).
    #[inline]
    pub fn find_num_placed(&self, word: N::Word) -> Option<u128> {
        if self.nums <= SMALL {
            return self.small[..self.nums].iter().find(|s| s.0 == word).map(|s| s.1);
        }
        self.num_slot(word).map(|s| s.placed)
    }

    /// A boolean's placed bits: `0` false, `1` true, `2` unknown (`None`: absent).
    #[inline]
    pub fn find_bool_placed(&self, tri: usize) -> Option<u128> {
        (self.bools[tri] != NONE).then_some(self.bool_placed[tri])
    }

    pub fn find(&self, c: Code<N>) -> u32 {
        match c.kind {
            K_NUM => self.find_num(c.word()),
            K_BOOL => self.bools[small::<N>(c.a) as usize],
            K_UBOOL => self.bools[2],
            _ => self.others.iter().find(|o| o.0 == c).map_or(NONE, |o| o.1),
        }
    }

    /// The key bits the field holds (0: one code, the same in every key).
    #[inline]
    pub fn bits(&self) -> u32 {
        self.bits
    }

    #[inline]
    pub fn index_in(&self, k: Key) -> u32 {
        if let [(pos, len)] = self.runs[..] {
            return ((of_key(k) >> pos) as u64 & ((1u64 << len) - 1)) as u32;
        }
        let k = of_key(k);
        let (mut idx, mut at) = (0u128, 0u32);
        for &(pos, len) in &self.runs {
            idx |= (k >> pos & ((1u128 << len) - 1)) << at;
            at += len as u32;
        }
        idx as u32
    }

    /// The key bits the field's runs cover.
    #[inline]
    pub fn mask(&self) -> u128 {
        self.runs.iter().fold(0u128, |m, &(pos, len)| m | ((1u128 << len) - 1) << pos)
    }

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

    fn push(&mut self, c: Code<N>) {
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
                if n <= SMALL {
                    self.small[n - 1] = (c.word(), self.place(idx));
                }
            }
            K_BOOL => {
                let b = small::<N>(c.a) as usize;
                self.bools[b] = idx;
                self.bool_placed[b] = self.place(idx);
            }
            K_UBOOL => {
                self.bools[2] = idx;
                self.bool_placed[2] = self.place(idx);
            }
            _ => self.others.push((c, idx)),
        }
    }

    fn put(slots: &mut [NumSlot<N>], word: N::Word, idx: u32, placed: u128) {
        let m = slots.len() - 1;
        let mut i = (N::fold(word).wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize & m;
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
        if self.nums <= SMALL {
            for j in 0..self.nums {
                self.small[j].1 = self.num_slot(self.small[j].0).expect("a small number is in the table").placed;
            }
        }
        for t in 0..3 {
            if self.bools[t] != NONE {
                self.bool_placed[t] = self.place(self.bools[t]);
            }
        }
    }
}

/// A FUNCTIONAL DEPENDENCY within a shape (its declaration,
/// `widening::key_deps`): every FOLLOWER cell's code is a function of
/// the LEADER cell's. Followers carry no key bits; their codes are recorded
/// per leader index (`Follow`) and checked on every row (module doc). A
/// shape may hold several, their cells disjoint.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Deps {
    pub leader: u32,
    /// Sorted, distinct, the leader not among them.
    pub followers: Vec<u32>,
}

/// A shape as the key space records it (its `shape::Layout`, written):
/// its name (hash), signature, position cells and its key's dependencies.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ShapeRecord {
    pub hash: u64,
    pub sig: Vec<u32>,
    pub pos: Option<(u32, u32)>,
    pub deps: Vec<Deps>,
}

/// One dependency's FOLLOWERS (`Deps`): per follower, its code at each
/// index of the leader's dictionary (as long as the leader's dictionary,
/// always).
#[derive(Clone, Debug)]
pub struct Follow<N: Arithmetic = P8> {
    leader: u32,
    cells: Vec<u32>,
    cols: Vec<Vec<Code<N>>>,
}

impl<N: Arithmetic> Follow<N> {
    #[inline]
    pub fn leader(&self) -> u32 {
        self.leader
    }

    /// The follower cells, sorted; a follower's SLOT is its place here.
    #[inline]
    pub fn cells(&self) -> &[u32] {
        &self.cells
    }

    /// The code follower `slot` holds wherever the leader's index is `idx`.
    #[inline]
    pub fn code(&self, slot: usize, idx: u32) -> Code<N> {
        self.cols[slot][idx as usize]
    }

    /// Leader indices recorded.
    fn len(&self) -> usize {
        self.cols.first().map_or(0, |c| c.len())
    }
}

/// THE VIOLATION of a declared dependency: FATAL (module doc).
#[cold]
#[allow(clippy::too_many_arguments)]
pub fn follower_violation<N: Arithmetic>(shape: u64, leader: u32, lcode: Code<N>, idx: u32, cell: u32, got: Code<N>, want: Code<N>, at: &str) -> ! {
    panic!(
        "exact keys: shape {shape:#x}: follower cell {cell} holds {:?} where the leader cell {leader} holds {:?} (index {idx}), \
         recorded with {:?} ({at}): the declared key dependency is VIOLATED (`widening::CLOCKS`), and the two states would share a key",
        got.value(),
        lcode.value(),
        want.value()
    );
}

/// THE POINTER TOPOLOGY IS THE SHAPE'S: a field holding a pointer holds
/// no other code. A shape's signature names a value cell, never what it
/// points at (`shape::sig`), and a kernel is bound by path against
/// one representative of its shape (`trace::bind`): two states of one
/// shape whose pointers differ would run a kernel bound to the other's
/// cells. FATAL (`check_pointer`).
#[cold]
#[inline(never)]
fn pointer_violation<N: Arithmetic>(shape: u64, cell: u32, had: Code<N>, got: Code<N>, at: &str) -> ! {
    panic!(
        "exact keys: shape {shape:#x}: cell {cell} holds {:?} beside {:?} ({at}): a field holding a pointer holds exactly one code, \
         but the POINTER TOPOLOGY, which a kernel binds by path against one representative, varies under one shape hash",
        got.value(),
        had.value()
    );
}

/// `code` joins a field (or a follower's column) whose first code is
/// `first`: fatal where either is a pointer and they differ
/// (`pointer_violation`).
#[inline]
fn check_pointer<N: Arithmetic>(shape: u64, cell: u32, first: Option<Code<N>>, code: Code<N>, at: &str) {
    if let Some(first) = first {
        if (first.kind == K_PTR || code.kind == K_PTR) && first != code {
            pointer_violation(shape, cell, first, code, at);
        }
    }
}

/// One shape's dictionaries (by cell; `None` for a cell that is not keyed,
/// its `KeyRole`'s: a structure cell, or a follower: `Follow`).
#[derive(Clone, Debug)]
pub struct ShapeKeys<N: Arithmetic = P8> {
    layout: std::sync::Arc<Layout>,
    fields: Vec<Option<FieldDict<N>>>,
    next_bit: u32,
    follows: Vec<Follow<N>>,
}

impl<N: Arithmetic> ShapeKeys<N> {
    /// A layout's empty dictionaries.
    fn new(layout: std::sync::Arc<Layout>) -> ShapeKeys<N> {
        let fields = (0..layout.cells() as u32).map(|c| layout.role(c).keyed().then(FieldDict::new)).collect();
        let follows = layout.deps().iter().map(|d| Follow { leader: d.leader, cells: d.followers.clone(), cols: vec![Vec::new(); d.followers.len()] }).collect();
        ShapeKeys { layout, fields, next_bit: 0, follows }
    }

    /// The shape's key layout.
    #[inline]
    pub fn layout(&self) -> &std::sync::Arc<Layout> {
        &self.layout
    }

    /// A KEYED field's dictionary (`None`: not a value cell, or a follower).
    #[inline]
    pub fn field(&self, cell: usize) -> Option<&FieldDict<N>> {
        self.fields.get(cell).and_then(|f| f.as_ref())
    }

    /// The dependencies (empty: none), in the record's order.
    #[inline]
    pub fn follows(&self) -> &[Follow<N>] {
        &self.follows
    }

    /// A follower's dependency and slot (`KeyRole::Follower`).
    #[inline]
    pub fn follower(&self, cell: u32) -> Option<(usize, usize)> {
        match self.layout.get(cell)? {
            KeyRole::Follower { dep, slot } => Some((dep as usize, slot as usize)),
            _ => None,
        }
    }

    /// The dependency `cell` leads (`KeyRole::Leader`).
    #[inline]
    fn leads(&self, cell: u32) -> Option<usize> {
        match self.layout.get(cell)? {
            KeyRole::Leader(g) => Some(g as usize),
            _ => None,
        }
    }

    pub fn bits(&self) -> u32 {
        self.next_bit
    }

    /// Every value cell, keyed or follower, in order.
    pub fn value_cells(&self) -> impl Iterator<Item = u32> + '_ {
        self.layout.value_cells()
    }

    #[inline]
    fn code_at(&self, rt2: &Rt2<N>, cell: u32, lane: usize) -> Code<N> {
        self.layout.code(cell, rt2.cols[cell as usize].at(lane))
    }

    /// The key of `codes` (by value cell, in order), all present (a
    /// follower's code the one recorded for its leader's among them).
    pub fn key_of_codes(&self, codes: &[(u32, Code<N>)]) -> Option<Key> {
        let mut k = 0u128;
        let mut lead = vec![NONE; self.follows.len()];
        for &(c, code) in codes {
            let Some(f) = self.field(c as usize) else { continue };
            let i = f.find(code);
            if i == NONE {
                return None;
            }
            if let Some(g) = self.leads(c) {
                lead[g] = i;
            }
            k |= f.place(i);
        }
        for &(c, code) in codes {
            if self.field(c as usize).is_some() {
                continue;
            }
            let (g, slot) = self.follower(c)?;
            if lead[g] == NONE || self.follows[g].code(slot, lead[g]) != code {
                return None;
            }
        }
        Some(to_key(k))
    }

    /// Value cell `c`'s code in key `k` (a follower's by its leader's
    /// index; `None`: an index past a dictionary).
    pub fn code_in(&self, c: u32, k: Key) -> Option<Code<N>> {
        if let Some(f) = self.field(c as usize) {
            return f.codes.get(f.index_in(k) as usize).copied();
        }
        let (g, slot) = self.follower(c)?;
        let fl = &self.follows[g];
        let d = self.field(fl.leader as usize).expect("a leader is keyed");
        let i = d.index_in(k);
        ((i as usize) < fl.len()).then(|| fl.code(slot, i))
    }

    /// The fields' codes of a key (the packing inverted), followers too.
    pub fn decode(&self, k: Key) -> Vec<(u32, Code<N>)> {
        self.value_cells().map(|c| (c, self.code_in(c, k).unwrap_or(Code::other(u8::MAX, 0)))).collect()
    }
}

/// FINGERPRINTS over exact CONTENT: a key's decoded codes, hashed - so two
/// trees whose dictionaries grew in different orders fingerprint the same
/// states alike.
pub struct ContentHash {
    shapes: FxHashMap<u64, (u64, Vec<(Vec<(u8, u8)>, Vec<u64>)>)>,
}

impl ContentHash {
    #[inline]
    fn code_hash<N: Arithmetic>(cell: u32, c: Code<N>) -> u64 {
        mix64(mix64((cell as u64) << 8 | c.kind as u64) ^ N::fold(c.word()))
    }

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

    #[inline]
    pub fn state(&self, shape: u64, k: Key, cell: u32) -> u64 {
        mix64(self.key(shape, k) ^ (cell as u64) << 1)
    }
}

/// A row's key against a key space: packed, or (a code is missing) its
/// exact content.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum RowKey<N: Arithmetic = P8> {
    Hit(Key),
    Miss(MissKey<N>),
}

/// The exact content of a state some dictionary lacks a code of: the packed
/// fields that hit and the (cell, code) of those that missed, by cell.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
#[serde(bound = "")]
pub struct MissKey<N: Arithmetic = P8> {
    pub shape: u64,
    pub partial: Key,
    pub missing: Vec<(u32, Code<N>)>,
}

/// What a key space gained: shapes, codes (with their indices) and bit runs
/// (with their places). Recorded per frame; a replay is order-free.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(bound = "")]
pub struct KeyAdditions<N: Arithmetic = P8> {
    pub shapes: Vec<ShapeRecord>,
    /// `(shape, cell, index, code)`; a follower's index is its leader's.
    pub codes: Vec<(u64, u32, u32, Code<N>)>,
    /// `(shape, cell, run number of the field, first bit, bits)`.
    pub runs: Vec<(u64, u32, u32, u8, u8)>,
}

impl<N: Arithmetic> Default for KeyAdditions<N> {
    fn default() -> Self {
        KeyAdditions { shapes: Vec::new(), codes: Vec::new(), runs: Vec::new() }
    }
}

impl<N: Arithmetic> KeyAdditions<N> {
    pub fn is_empty(&self) -> bool {
        self.shapes.is_empty() && self.codes.is_empty() && self.runs.is_empty()
    }

    pub fn extend(&mut self, other: KeyAdditions<N>) {
        self.shapes.extend(other.shapes);
        self.codes.extend(other.codes);
        self.runs.extend(other.runs);
    }
}

/// Unique generations across every key space of the process.
static GEN: AtomicU64 = AtomicU64::new(1);

#[derive(Clone, Debug)]
pub struct KeySpace<N: Arithmetic = P8> {
    shapes: FxHashMap<u64, ShapeKeys<N>>,
    run_counts: FxHashMap<(u64, u32), u32>,
    gen: u64,
}

impl<N: Arithmetic> Default for KeySpace<N> {
    fn default() -> Self {
        KeySpace { shapes: Default::default(), run_counts: Default::default(), gen: GEN.fetch_add(1, Ordering::Relaxed) }
    }
}

impl<N: Arithmetic> KeySpace<N> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Changes whenever the dictionaries do (unique in the process).
    #[inline]
    pub fn generation(&self) -> u64 {
        self.gen
    }

    /// Bytes its dictionaries hold (approximately: their vectors' room).
    pub fn alloc_bytes(&self) -> usize {
        let field = |f: &FieldDict<N>| std::mem::size_of::<FieldDict<N>>() + f.codes.capacity() * std::mem::size_of::<Code<N>>() + f.num_slots.capacity() * std::mem::size_of::<NumSlot<N>>() + f.others.capacity() * std::mem::size_of::<(Code<N>, u32)>() + f.runs.capacity() * 2;
        let follow = |s: &ShapeKeys<N>| s.follows.iter().flat_map(|fl| &fl.cols).map(|c| c.capacity() * std::mem::size_of::<Code<N>>()).sum::<usize>();
        self.shapes.values().map(|s| s.fields.iter().flatten().map(field).sum::<usize>() + s.fields.capacity() * std::mem::size_of::<Option<FieldDict<N>>>() + follow(s)).sum::<usize>() + self.run_counts.capacity() * 20
    }

    fn bump(&mut self) {
        self.gen = GEN.fetch_add(1, Ordering::Relaxed);
    }

    #[inline]
    pub fn shape(&self, hash: u64) -> Option<&ShapeKeys<N>> {
        self.shapes.get(&hash)
    }

    pub fn shapes(&self) -> impl Iterator<Item = &ShapeKeys<N>> {
        self.shapes.values()
    }

    pub fn max_bits(&self) -> u32 {
        self.shapes.values().map(|s| s.next_bit).max().unwrap_or(0)
    }

    pub fn codes(&self) -> usize {
        self.shapes.values().flat_map(|s| s.fields.iter().flatten()).map(|f| f.len()).sum()
    }

    /// Register a shape (or check a known one's layout: two structures
    /// under one hash, or one under two layouts, are FATAL); true if new.
    pub fn add_shape(&mut self, layout: &std::sync::Arc<Layout>, adds: &mut KeyAdditions<N>) -> bool {
        let hash = layout.hash();
        if let Some(s) = self.shapes.get(&hash) {
            assert!(s.layout.deps() == layout.deps() || s.layout.sig() != layout.sig(), "exact keys: shape {hash:#x} under two key layouts ({:?} and {:?})", s.layout.deps(), layout.deps());
            assert!(*s.layout == **layout, "exact keys: two shapes under the hash {hash:#x} (a shape-hash collision)");
            return false;
        }
        self.shapes.insert(hash, ShapeKeys::new(layout.clone()));
        adds.shapes.push(layout.record());
        self.bump();
        true
    }

    /// Append `codes` (sorted, distinct, none present) to `shape`'s field
    /// `cell`, giving it bits if it needs more: a new run at the shape's
    /// next free bit. Recorded in `adds`. Past `KEY_BITS`: FATAL. Not a
    /// leader's (its codes come with their followers': `add_misses`).
    pub fn add_codes(&mut self, shape: u64, cell: u32, codes: &[Code<N>], adds: &mut KeyAdditions<N>) {
        let s = self.shapes.get(&shape).unwrap_or_else(|| panic!("exact keys: codes for the unknown shape {shape:#x}"));
        assert!(s.leads(cell).is_none(), "exact keys: shape {shape:#x}: the leader cell {cell}'s codes added without their followers'");
        self.push_codes(shape, cell, codes, adds);
    }

    /// `rows`' leader codes (sorted, distinct, none present) appended to
    /// the leader of `shape`'s dependency `g`, each with its followers'
    /// codes (in `Follow::cells` order) recorded at its index.
    fn add_leader_codes(&mut self, shape: u64, g: usize, rows: &[(Code<N>, Vec<Code<N>>)], adds: &mut KeyAdditions<N>) {
        let s = self.shapes.get(&shape).unwrap_or_else(|| panic!("exact keys: codes for the unknown shape {shape:#x}"));
        let fl = &s.follows[g];
        let (leader, first) = (fl.leader, fl.len() as u32);
        let codes: Vec<Code<N>> = rows.iter().map(|r| r.0).collect();
        self.push_codes(shape, leader, &codes, adds);
        let fl = &mut self.shapes.get_mut(&shape).expect("the shape").follows[g];
        for (i, (_, fol)) in rows.iter().enumerate() {
            assert_eq!(fol.len(), fl.cells.len(), "exact keys: shape {shape:#x}: a leader code with {} of its {} followers' codes", fol.len(), fl.cells.len());
            for (slot, &code) in fol.iter().enumerate() {
                check_pointer(shape, fl.cells[slot], fl.cols[slot].first().copied(), code, "a follower's code added");
                adds.codes.push((shape, fl.cells[slot], first + i as u32, code));
                fl.cols[slot].push(code);
            }
        }
    }

    /// Every code of `misses` the dictionaries lack, per field SORTED and
    /// appended (fields in (shape, cell) order: the dictionaries are a
    /// function of the set of misses). A new leader code comes with its
    /// followers' codes, which every miss holding it must agree on (else
    /// FATAL: the dependency is violated); a miss holds its followers'
    /// codes exactly when its leader's is new (`row_keys`). Their shapes
    /// must be in.
    pub fn add_misses<'a>(&mut self, misses: impl IntoIterator<Item = &'a MissKey<N>>, adds: &mut KeyAdditions<N>)
    where
        N: 'a,
    {
        use std::collections::btree_map::Entry;
        enum New<N: Arithmetic> {
            Codes(std::collections::BTreeSet<Code<N>>),
            Leader(usize, std::collections::BTreeMap<Code<N>, Vec<Code<N>>>),
        }
        let mut new: std::collections::BTreeMap<(u64, u32), New<N>> = Default::default();
        for m in misses {
            let s = self.shapes.get(&m.shape).unwrap_or_else(|| panic!("exact keys: codes for the unknown shape {:#x}", m.shape));
            let code_of = |fc: u32| m.missing.iter().find(|x| x.0 == fc).map(|x| x.1);
            for &(c, code) in &m.missing {
                if let Some(g) = s.leads(c) {
                    let fl = &s.follows[g];
                    let fol: Vec<Code<N>> = fl.cells.iter().map(|&fc| code_of(fc).unwrap_or_else(|| panic!("exact keys: shape {:#x}: a new leader code without follower cell {fc}'s", m.shape))).collect();
                    let New::Leader(_, rows) = new.entry((m.shape, c)).or_insert_with(|| New::Leader(g, Default::default())) else { unreachable!("the leader's entry") };
                    match rows.entry(code) {
                        Entry::Vacant(e) => {
                            e.insert(fol);
                        }
                        Entry::Occupied(e) => {
                            if let Some(slot) = (0..fol.len()).find(|&k| e.get()[k] != fol[k]) {
                                follower_violation(m.shape, c, code, NONE, fl.cells[slot], fol[slot], e.get()[slot], "two new states, one new leader code");
                            }
                        }
                    }
                } else if let Some((g, _)) = s.follower(c) {
                    assert!(code_of(s.follows[g].leader).is_some(), "exact keys: shape {:#x}: follower cell {c} missing while its leader hit", m.shape);
                } else {
                    let New::Codes(set) = new.entry((m.shape, c)).or_insert_with(|| New::Codes(Default::default())) else { unreachable!("a field's entry") };
                    set.insert(code);
                }
            }
        }
        for ((shape, cell), n) in new {
            match n {
                New::Codes(set) => self.add_codes(shape, cell, &set.into_iter().collect::<Vec<_>>(), adds),
                New::Leader(g, rows) => self.add_leader_codes(shape, g, &rows.into_iter().collect::<Vec<_>>(), adds),
            }
        }
    }

    fn push_codes(&mut self, shape: u64, cell: u32, codes: &[Code<N>], adds: &mut KeyAdditions<N>) {
        if codes.is_empty() {
            return;
        }
        let s = self.shapes.get_mut(&shape).unwrap_or_else(|| panic!("exact keys: codes for the unknown shape {shape:#x}"));
        let next_bit = s.next_bit;
        let f = s.fields.get_mut(cell as usize).and_then(|f| f.as_mut()).unwrap_or_else(|| panic!("exact keys: shape {shape:#x} cell {cell} is not a keyed value"));
        for w in codes.windows(2) {
            assert!(w[0] < w[1], "exact keys: codes added unsorted");
        }
        for &c in codes {
            assert!(f.find(c) == NONE, "exact keys: shape {shape:#x} cell {cell}: code {c:?} added twice");
            check_pointer(shape, cell, f.codes.first().copied(), c, "added");
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
    pub fn apply(&mut self, mut adds: KeyAdditions<N>) -> anyhow::Result<()> {
        let mut scratch = KeyAdditions::default();
        for r in &adds.shapes {
            let layout = Layout::from_record(r).map_err(|e| anyhow::anyhow!(e))?;
            self.add_shape(&std::sync::Arc::new(layout), &mut scratch);
        }
        adds.codes.sort_unstable_by_key(|&(s, c, i, _)| (s, c, i));
        for &(shape, cell, idx, code) in &adds.codes {
            let s = self.shapes.get_mut(&shape).ok_or_else(|| anyhow::anyhow!("exact keys: a code of the unknown shape {shape:#x}"))?;
            if let Some((g, slot)) = s.follower(cell) {
                let col = &mut s.follows[g].cols[slot];
                anyhow::ensure!(idx as usize == col.len(), "exact keys: shape {shape:#x} follower cell {cell}: leader index {idx} recorded after {}", col.len());
                check_pointer(shape, cell, col.first().copied(), code, "a follower's code replayed");
                col.push(code);
                continue;
            }
            let f = s.fields.get_mut(cell as usize).and_then(|f| f.as_mut()).ok_or_else(|| anyhow::anyhow!("exact keys: shape {shape:#x} cell {cell} is not a value"))?;
            anyhow::ensure!(idx as usize == f.len(), "exact keys: shape {shape:#x} cell {cell}: index {idx} recorded after {} codes", f.len());
            anyhow::ensure!(f.find(code) == NONE, "exact keys: shape {shape:#x} cell {cell}: code {code:?} recorded twice");
            check_pointer(shape, cell, f.codes.first().copied(), code, "replayed");
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
                    anyhow::ensure!(f.bits >= bits_for(f.len()), "exact keys: shape {:#x} cell {c}: {} codes in {} bits", s.layout.hash(), f.len(), f.bits);
                }
            }
            for fl in &s.follows {
                let n = s.field(fl.leader as usize).expect("the leader is keyed").len();
                for (slot, col) in fl.cols.iter().enumerate() {
                    anyhow::ensure!(col.len() == n, "exact keys: shape {:#x} follower cell {}: {} codes for the leader's {n}", s.layout.hash(), fl.cells[slot], col.len());
                }
            }
        }
        self.bump();
        Ok(())
    }

    pub fn content_hash(&self) -> ContentHash {
        let shapes = self
            .shapes
            .iter()
            .map(|(&h, s)| {
                let mut fixed = 0u64;
                let mut fields = Vec::new();
                for (c, f) in s.fields.iter().enumerate() {
                    let Some(f) = f else { continue };
                    let mut hashes: Vec<u64> = f.codes.iter().map(|&code| ContentHash::code_hash(c as u32, code)).collect();
                    // The leader's index stands for its followers' codes
                    // too: a state hashes as it would keyed field by field.
                    if let Some(fl) = s.leads(c as u32).map(|g| &s.follows[g]) {
                        for (i, h) in hashes.iter_mut().enumerate() {
                            for (slot, &fc) in fl.cells.iter().enumerate() {
                                *h = h.wrapping_add(ContentHash::code_hash(fc, fl.code(slot, i as u32)));
                            }
                        }
                    }
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
    pub fn to_additions(&self) -> KeyAdditions<N> {
        let mut hashes: Vec<u64> = self.shapes.keys().copied().collect();
        hashes.sort_unstable();
        let mut out = KeyAdditions::default();
        for h in hashes {
            let s = &self.shapes[&h];
            out.shapes.push(s.layout.record());
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
            // A follower's codes by the leader's index.
            for fl in &s.follows {
                for (slot, &c) in fl.cells.iter().enumerate() {
                    for (i, &code) in fl.cols[slot].iter().enumerate() {
                        out.codes.push((h, c, i as u32, code));
                    }
                }
            }
        }
        out
    }

    pub fn from_additions(adds: KeyAdditions<N>) -> anyhow::Result<KeySpace<N>> {
        let mut k = KeySpace::new();
        k.apply(adds)?;
        Ok(k)
    }

    /// Every row's key of a CANONICAL block: packed where every code is in the
    /// dictionaries, else its exact content. A block whose shape is known must
    /// have its signature. A row whose leader hits has its followers CHECKED
    /// (a difference is FATAL); a row whose leader misses holds its
    /// followers' codes in its content (`add_misses` records them).
    pub fn row_keys(&self, rt2: &Rt2<N>) -> Vec<RowKey<N>> {
        let Some(s) = self.shapes.get(&rt2.shape_hash()) else {
            // An unknown shape: every field is missing (its followers too,
            // whatever its layout).
            let s = ShapeKeys::<N>::new(std::sync::Arc::new(Layout::of_deps(rt2, Vec::new())));
            return (0..rt2.width)
                .map(|lane| RowKey::Miss(MissKey { shape: rt2.shape_hash(), partial: (0, 0), missing: s.value_cells().map(|c| (c, s.code_at(rt2, c, lane))).collect() }))
                .collect();
        };
        assert!(s.layout.sig() == rt2.shape.sig(), "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", rt2.shape_hash());
        let cells = s.layout.keyed();
        (0..rt2.width)
            .map(|lane| {
                let (mut k, mut missing) = (0u128, Vec::new());
                for &c in cells {
                    let code = s.code_at(rt2, c, lane);
                    let f = s.field(c as usize).expect("a value cell");
                    match f.find(code) {
                        NONE => missing.push((c, code)),
                        i => k |= f.place(i),
                    }
                }
                for fl in &s.follows {
                    let lead = s.field(fl.leader as usize).expect("the leader is keyed").find(s.code_at(rt2, fl.leader, lane));
                    if lead == NONE {
                        missing.extend(fl.cells.iter().map(|&c| (c, s.code_at(rt2, c, lane))));
                    } else {
                        for (slot, &c) in fl.cells.iter().enumerate() {
                            let (got, want) = (s.code_at(rt2, c, lane), fl.code(slot, lead));
                            if got != want {
                                follower_violation(rt2.shape_hash(), fl.leader, s.code_at(rt2, fl.leader, lane), lead, c, got, want, &format!("the boundary, lane {lane}"));
                            }
                        }
                    }
                }
                if missing.is_empty() {
                    RowKey::Hit(to_key(k))
                } else {
                    if !s.follows.is_empty() {
                        missing.sort_unstable_by_key(|m| m.0);
                    }
                    RowKey::Miss(MissKey { shape: rt2.shape_hash(), partial: to_key(k), missing })
                }
            })
            .collect()
    }

    /// Every row's key of a CANONICAL block, `None` where a code is missing
    /// (no state of the tree is that row).
    pub fn lookup_keys(&self, rt2: &Rt2<N>) -> Vec<Option<Key>> {
        self.row_keys(rt2)
            .into_iter()
            .map(|r| match r {
                RowKey::Hit(k) => Some(k),
                RowKey::Miss(_) => None,
            })
            .collect()
    }

    /// Add every code of the rows of `blocks` (CANONICAL) the dictionaries
    /// lack - per field SORTED - and their shapes: frame 0's seeding.
    pub fn absorb(&mut self, blocks: &[&Rt2<N>]) -> KeyAdditions<N> {
        let mut adds = KeyAdditions::default();
        let mut shapes: Vec<&Rt2<N>> = blocks.to_vec();
        shapes.sort_by_key(|b| b.shape_hash());
        for b in &shapes {
            self.add_shape(&std::sync::Arc::new(Layout::of(b)), &mut adds);
        }
        let misses: Vec<MissKey<N>> = blocks
            .iter()
            .flat_map(|b| self.row_keys(b))
            .filter_map(|r| match r {
                RowKey::Miss(m) => Some(m),
                RowKey::Hit(_) => None,
            })
            .collect();
        self.add_misses(&misses, &mut adds);
        adds
    }

    /// A miss's final key, its codes all in (`add_misses`); its followers,
    /// present exactly where their leader is, CHECKED against the record.
    pub fn complete(&self, m: &MissKey<N>) -> Key {
        let s = self.shapes.get(&m.shape).unwrap_or_else(|| panic!("exact keys: completing a key of the unknown shape {:#x}", m.shape));
        let mut k = of_key(m.partial);
        let mut lead: Vec<Option<(u32, Code<N>)>> = vec![None; s.follows.len()];
        for &(c, code) in &m.missing {
            let Some(f) = s.field(c as usize) else { continue };
            let i = f.find(code);
            assert!(i != NONE, "exact keys: completing a key whose codes are not all in (shape {:#x} cell {c})", m.shape);
            if let Some(g) = s.leads(c) {
                lead[g] = Some((i, code));
            }
            k |= f.place(i);
        }
        for &(c, code) in &m.missing {
            if s.field(c as usize).is_some() {
                continue;
            }
            let (g, slot) = s.follower(c).unwrap_or_else(|| panic!("exact keys: shape {:#x} cell {c} is not a value", m.shape));
            let fl = &s.follows[g];
            let (i, lcode) = lead[g].unwrap_or_else(|| panic!("exact keys: shape {:#x}: follower cell {c} missing while its leader hit", m.shape));
            if fl.code(slot, i) != code {
                follower_violation(m.shape, fl.leader, lcode, i, c, code, fl.code(slot, i), "a provisional key made final");
            }
        }
        to_key(k)
    }
}

const PROV_SHARDS: usize = 256;

/// A wave's PROVISIONAL keys: each distinct miss content interned to
/// `PROVISIONAL | shard << 32` (high word), local number (low word). Ids
/// depend on scheduling; the translation turns them into final keys.
pub struct Provisional<N: Arithmetic = P8> {
    shards: Vec<Mutex<(FxHashMap<MissKey<N>, u32>, Vec<MissKey<N>>)>>,
    shapes: Mutex<FxHashMap<u64, std::sync::Arc<Layout>>>,
}

impl<N: Arithmetic> Default for Provisional<N> {
    fn default() -> Self {
        Provisional { shards: (0..PROV_SHARDS).map(|_| Default::default()).collect(), shapes: Default::default() }
    }
}

impl<N: Arithmetic> Provisional<N> {
    /// A shape the key space lacks, which misses are made of.
    pub fn note_shape(&self, l: &std::sync::Arc<Layout>) {
        let mut g = self.shapes.lock().expect("the provisional shapes");
        match g.get(&l.hash()) {
            Some(x) => assert!(**x == **l, "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", l.hash()),
            None => {
                g.insert(l.hash(), l.clone());
            }
        }
    }

    pub fn shape_layout(&self, hash: u64) -> Option<std::sync::Arc<Layout>> {
        self.shapes.lock().expect("the provisional shapes").get(&hash).cloned()
    }

    pub fn intern(&self, m: MissKey<N>) -> Key {
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

    pub fn content(&self, k: Key) -> MissKey<N> {
        debug_assert!(is_provisional(k));
        let g = self.shards[(k.0 & !PROVISIONAL) as usize].lock().expect("a provisional shard");
        g.1[k.1 as usize].clone()
    }
}

/// A whole state's canonical bytes: its shape's name and every value
/// cell's code (a position coordinate less its whole pixels: the cell is
/// beside it). Equal bytes = equal states; no tree needed.
pub type ExactRow = Box<[u8]>;

/// Block `rt2`'s rows' canonical bytes.
pub fn exact_rows<N: Arithmetic>(rt2: &Rt2<N>) -> Vec<ExactRow> {
    let pos = Anchors::of(rt2).pos();
    let cells: Vec<(u32, KeyRole)> = (0..rt2.cells().len() as u32)
        .filter(|&c| rt2.cells()[c as usize] == Cell::Val)
        .map(|c| (c, if pos.is_some_and(|(x, y)| c == x || c == y) { KeyRole::Position } else { KeyRole::Value }))
        .collect();
    (0..rt2.width)
        .map(|lane| {
            let mut out = Vec::with_capacity(8 + cells.len() * 5);
            out.extend_from_slice(&rt2.shape_hash().to_le_bytes());
            for &(c, role) in &cells {
                let code = role.code(rt2.cols[c as usize].at(lane));
                match code.kind {
                    K_NUM if code.a == code.b => {
                        out.push(0);
                        N::push_raw(code.a, &mut out);
                    }
                    K_NUM => {
                        out.push(1);
                        N::push_raw(code.a, &mut out);
                        N::push_raw(code.b, &mut out);
                    }
                    K_STR | K_PTR => {
                        out.push(2 + code.kind);
                        out.extend_from_slice(&small::<N>(code.a).to_le_bytes());
                    }
                    k => out.push(16 + k * 2 + small::<N>(code.a) as u8),
                }
            }
            out.into_boxed_slice()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A record's layout (the key space takes layouts).
    fn lay(r: &ShapeRecord) -> std::sync::Arc<Layout> {
        std::sync::Arc::new(Layout::from_record(r).expect("a layout"))
    }

    /// A shape's identity is the names it holds: an absent global (a name
    /// appended to `GLOBAL_NAMES`, which no block holds yet) is not in its
    /// signature, so it moves no hash.
    #[test]
    fn an_absent_global_is_not_in_a_shapes_identity() {
        use crate::runtime2::{Col, NONE};
        use crate::shape::sig;
        let room = celeste_core::room::Room::of((1, 0));
        let x = celeste_names::field("x");
        let mut globals = vec![NONE; celeste_names::GLOBAL_NAMES.len()];
        globals[celeste_names::global("objects") as usize] = 0;
        let cols = vec![Col::U(AV::Ptr(1)), Col::U(AV::Nil), Col::U(AV::Num(P8::from_i16(3)))];
        let b = Rt2::<P8>::canonical_of(1, vec![Cell::Val, Cell::Obj(Box::new([(x, 2)])), Cell::Val], cols, globals, crate::runtime2::static_strings(), room);
        let (hash, words) = (b.shape_hash(), b.shape.sig().to_vec());
        let mut more = b.shape.globals().to_vec();
        more.push(NONE);
        assert_eq!((sig::hash(b.cells(), &more), sig::of(b.cells(), &more)), (hash, words.clone()), "an appended, absent global moves the shape");
        assert!(words.len() < celeste_names::GLOBAL_NAMES.len(), "the signature lists the globals present, not every slot");
    }

    fn n(x: i32) -> Code {
        Code::num(x as u32, x as u32)
    }

    /// Keys never move as dictionaries grow, stay distinct, and a replay of
    /// the records (in any order) rebuilds the same keys.
    #[test]
    fn keys_are_stable_exact_and_replayable() {
        let rec = ShapeRecord { hash: 7, sig: vec![1, 1, 4, 1, u32::MAX], pos: None, deps: Vec::new() };
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        assert!(ks.add_shape(&lay(&rec), &mut adds));
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
        assert_ne!(Code::<P8>::of(AV::Bool(false)), Code::of(AV::UBool));
    }

    /// A wider interval is neither end's code.
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

    /// A three-value-cell shape (cells 0, 1, 2): cell 0 the leader of 1 and 2.
    fn dep_space() -> (KeySpace, KeyAdditions) {
        let rec = ShapeRecord { hash: 5, sig: vec![1, 1, 1, u32::MAX], pos: None, deps: vec![Deps { leader: 0, followers: vec![1, 2] }] };
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        ks.add_shape(&lay(&rec), &mut adds);
        (ks, adds)
    }

    /// A miss of the dependent shape: the leader's code and its followers'.
    fn dep_miss(l: i32, a: i32, b: i32) -> MissKey {
        MissKey { shape: 5, partial: (0, 0), missing: vec![(0, n(l)), (1, n(a)), (2, n(b))] }
    }

    /// FOLLOWERS carry no key bits, decode from the leader's index, hash as
    /// keyed fields would, and round-trip through the records.
    #[test]
    fn followers_have_no_bits_and_round_trip() {
        let (mut ks, mut adds) = dep_space();
        // Two misses share a new leader code with the same followers: one row.
        ks.add_misses(&[dep_miss(7, 1, 1), dep_miss(3, 1, 2), dep_miss(7, 1, 1), dep_miss(9, 4, 2)], &mut adds);
        let s = ks.shape(5).unwrap();
        assert_eq!(s.bits(), 2, "three leader codes, no follower bits");
        assert!(s.field(1).is_none() && s.field(2).is_none(), "followers have no dictionary");
        let k = ks.complete(&dep_miss(9, 4, 2));
        assert_eq!(s.decode(k), vec![(0, n(9)), (1, n(4)), (2, n(2))]);
        assert_eq!(s.key_of_codes(&[(0, n(3)), (1, n(1)), (2, n(2))]), Some(ks.complete(&dep_miss(3, 1, 2))));
        assert_eq!(s.key_of_codes(&[(0, n(3)), (1, n(1)), (2, n(1))]), None, "a follower other than the record's is no key");
        let r = KeySpace::from_additions(adds.clone()).unwrap();
        assert_eq!(r.shape(5).unwrap().decode(k), s.decode(k), "the records replay the followers");
        let again = KeySpace::from_additions(ks.to_additions()).unwrap();
        assert_eq!(again.shape(5).unwrap().decode(k), s.decode(k));
        // The content hash is the layout's free: as if every cell were keyed.
        let flat_rec = ShapeRecord { hash: 5, sig: vec![1, 1, 1, u32::MAX], pos: None, deps: Vec::new() };
        let mut flat = KeySpace::new();
        let mut fa = KeyAdditions::default();
        flat.add_shape(&lay(&flat_rec), &mut fa);
        flat.add_misses(&[dep_miss(7, 1, 1), dep_miss(3, 1, 2), dep_miss(9, 4, 2)], &mut fa);
        let fk = flat.complete(&dep_miss(9, 4, 2));
        assert_eq!(ks.content_hash().key(5, k), flat.content_hash().key(5, fk), "a state hashes alike in both layouts");
        // A replay missing a follower's record is refused.
        let mut cut = adds;
        cut.codes.retain(|&(_, c, i, _)| !(c == 2 && i == 2));
        assert!(KeySpace::from_additions(cut).is_err(), "a leader index without its follower's code");
    }

    /// Two new states holding one new leader code with different followers:
    /// the dependency is violated, FATAL.
    #[test]
    #[should_panic(expected = "declared key dependency is VIOLATED")]
    fn a_new_leader_with_two_followers_is_fatal() {
        let (mut ks, _) = dep_space();
        ks.add_misses(&[dep_miss(7, 1, 1), dep_miss(7, 1, 2)], &mut KeyAdditions::default());
    }

    /// A row whose leader hits but whose follower differs from the record:
    /// FATAL at the boundary (a block keyed against the space).
    #[test]
    #[should_panic(expected = "declared key dependency is VIOLATED")]
    fn a_follower_other_than_the_record_is_fatal_at_the_boundary() {
        use crate::runtime2::{Col, NONE};
        let room = celeste_core::room::Room::of((1, 0));
        let shape = crate::shape::Shape::intern(vec![Cell::Val, Cell::Val, Cell::Val], vec![NONE; celeste_names::GLOBAL_NAMES.len()]);
        let num = |x: i16| AV::Num(P8::from_i16(x));
        let b = Rt2::<P8>::new(2, shape, vec![Col::U(num(7)), Col::U(num(1)), Col::V(vec![num(1), num(2)])], crate::runtime2::static_strings(), room);
        let deps = Deps { leader: 0, followers: vec![1, 2] };
        let rec = ShapeRecord { hash: b.shape_hash(), sig: b.shape.sig().to_vec(), pos: None, deps: vec![deps] };
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        ks.add_shape(&lay(&rec), &mut adds);
        let raw = |x: i32| Code::num((x << 16) as u32, (x << 16) as u32);
        let first = MissKey { shape: rec.hash, partial: (0, 0), missing: vec![(0, raw(7)), (1, raw(1)), (2, raw(1))] };
        ks.add_misses(&[first], &mut adds);
        // Lane 0 is the record's; lane 1's cell 2 is not.
        ks.row_keys(&b);
    }

    fn ptr(t: u32) -> Code {
        Code::of(AV::Ptr(t))
    }

    /// `f` panics, on the pointer topology (`pointer_violation`).
    fn breaks_the_topology(what: &str, f: impl FnOnce()) {
        let e = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).expect_err(what);
        let msg = e.downcast_ref::<String>().map_or("(not a formatted panic)", |m| m.as_str());
        assert!(msg.contains("the POINTER TOPOLOGY"), "{what}: {msg}");
    }

    /// THE POINTER TOPOLOGY IS THE SHAPE'S: a pointer field holds one code
    /// (no bits), and a second one beside it is FATAL, whichever comes
    /// first; replayed from records too.
    #[test]
    fn a_pointer_field_holds_exactly_one_code() {
        let rec = ShapeRecord { hash: 9, sig: vec![1, 1, u32::MAX], pos: None, deps: Vec::new() };
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        ks.add_shape(&lay(&rec), &mut adds);
        ks.add_codes(9, 0, &[ptr(3)], &mut adds);
        assert_eq!(ks.shape(9).unwrap().field(0).unwrap().bits(), 0, "one pointer: no key bits");
        for (cell, first, second) in [(0, None, ptr(4)), (0, None, Code::of(AV::Nil)), (1, Some(n(1)), ptr(4))] {
            let (mut ks, mut adds) = (ks.clone(), adds.clone());
            if let Some(c) = first {
                ks.add_codes(9, cell, &[c], &mut adds);
            }
            breaks_the_topology("a pointer field took a second code", || ks.add_codes(9, cell, &[second], &mut adds));
        }
        let replay = KeyAdditions { shapes: vec![rec], codes: vec![(9, 0, 0, ptr(3)), (9, 0, 1, ptr(4))], runs: vec![(9, 0, 0, 0, 1)] };
        breaks_the_topology("a replay gave a pointer field two codes", || drop(KeySpace::<P8>::from_additions(replay)));
    }

    /// A follower holding one pointer at every leader index is fine; two
    /// are FATAL.
    #[test]
    fn a_followers_pointer_is_one_code() {
        let (mut ks, mut adds) = dep_space();
        let miss = |l: i32, p: u32| MissKey { shape: 5, partial: (0, 0), missing: vec![(0, n(l)), (1, ptr(p)), (2, n(0))] };
        ks.add_misses(&[miss(1, 7), miss(2, 7)], &mut adds);
        let replay = adds.clone();
        breaks_the_topology("a follower's pointer differed between leader codes", || ks.add_misses(&[miss(3, 8)], &mut adds));
        let mut bad = replay;
        bad.codes.iter_mut().filter(|c| c.1 == 1 && c.2 == 1).for_each(|c| c.3 = ptr(8));
        breaks_the_topology("a replayed follower's pointer differed", || drop(KeySpace::<P8>::from_additions(bad)));
    }

    /// Past 127 bits is fatal, never truncated.
    #[test]
    #[should_panic(expected = "past the 127 a key holds")]
    fn a_key_past_its_bits_is_fatal() {
        let sig: Vec<u32> = std::iter::repeat_n(1, 9).chain([u32::MAX]).collect();
        let mut ks = KeySpace::new();
        let mut adds = KeyAdditions::default();
        ks.add_shape(&lay(&ShapeRecord { hash: 1, sig, pos: None, deps: Vec::new() }), &mut adds);
        let codes: Vec<Code> = (0..1 << 15).map(n).collect();
        for c in 0..9 {
            ks.add_codes(1, c, &codes, &mut adds);
        }
    }
}
