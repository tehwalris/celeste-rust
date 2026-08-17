//! Rt3: the TILE runtime (plans/columnar-engine.md "Rt3" section).
//!
//! Third `Engine` impl over the same generated program. A tile is a fixed
//! small set of input rows (TILE lanes); the driver loops the 64 input
//! variants OUTSIDE the kernel and writes concrete button bools before
//! `f___frame`, so the tile width never changes mid-frame - no widen, no
//! COW, no lane appends. Values live in a bump arena of fixed-size
//! entries, reset per run: zero allocation inside the frame.
//!
//! Straddle splits (`__split_by_flr`, `__split_at`) use the counter-replay
//! tape: pass P takes alternative `tape[j]` at split site j; a lane emits
//! rows on pass P iff `tape[j] < k_j(lane)` for every site (its `valid`
//! bit), and the tape advances like an odometer (DFS over the choice tree,
//! robust to data-dependent alternative counts). Masked-out lanes compute
//! `min(tape[j], k-1)` - a real value from an earlier pass - so no lane
//! ever computes garbage.
//!
//! Anything the tile cannot handle (non-uniform branch, failed guard,
//! unknown-bool branch) throws `TileBail`; the driver reruns that
//! tile x variant on the reference engine (Rt2), which carries the full
//! diagnostics and the deopt semantics. No error messages on this path.
//!
//! Per-lane value semantics are the SAME `av_*` ports runtime2 uses.

use crate::runtime::{Callee, Engine};
use crate::runtime::{
    BI___ARRAY_TABLE_DROP_LAST, BI___NEW_UNKNOWN_BOOLEAN, BI___NEW_VECTOR, BI___PRINT,
    BI___SPLIT_AT, BI___SPLIT_BY_FLR, BI___WIDEN_REM, BI_ABS, BI_ADD, BI_ERROR, BI_FGET, BI_FLR,
    BI_MAX, BI_MGET, BI_MIN, BI_PRINT, BI_SIN, BI_TILE_FLAG_AT,
};
use crate::runtime2::{
    av_abs, av_addsub, av_cmp, av_div, av_eq, av_flr, av_max, av_min, av_mul, av_neg, av_not,
    av_rem, av_sin, av_truthy, split_iv_by_floor, Cell2, CmpOp, Col, Rt2, AV, NONE,
};
use celeste_rust::pico8_num::{Pico8Num, Pico8NumInterval};

pub type P8 = Pico8Num;

/// Tile width. 64 lanes x 16 B AV = 1 KB per varying value. 64 so the
/// dynamic-expand mode (one boundary row fanned out over all 2^6 input
/// variants IN-TILE, sharing the pre-input trunk) fits; the concrete-
/// button mode simply runs wider row tiles.
pub const TILE: usize = 64;

/// Thrown when the tile cannot proceed; the driver falls back to Rt2.
/// Carries the bail site (file:line) - cheap, static, and the only
/// diagnostics this path has.
pub struct TileBail(pub &'static std::panic::Location<'static>);

#[inline]
#[track_caller]
fn bail() -> ! {
    std::panic::panic_any(TileBail(std::panic::Location::caller()))
}

/// One tile value: uniform, or an index into the tile-data pool. 16 B,
/// so cloning a whole column table is a small flat memcpy (the inline
/// [AV; TILE] variant made the enum 260 B and cloning dominated v1).
#[derive(Clone, Copy, Debug)]
pub enum TCol {
    U(AV),
    T(u32),
}

#[derive(Clone)]
pub struct Rt3<const BTN: u8 = 0> {
    pub width: usize, // <= TILE (last tile of a chunk may be partial)
    pub structure: Vec<Cell2>,
    pub cols: Vec<TCol>,
    pub globals: Vec<u32>,
    pub strings: std::sync::Arc<Vec<String>>,
    pub cart: std::sync::Arc<celeste_rust::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_rust::collision_cache::CollisionCache>,
    pub prints: Vec<String>,
    /// Lanes that emit rows on THIS pass (counter-replay masking).
    pub valid: [bool; TILE],
    /// Choice tape: alternative index per split site, in execution order.
    pub tape: Vec<u8>,
    cursor: usize,
    /// Max alternative count seen at each site on this pass.
    pub ks: Vec<u8>,
    /// Payload pool for TCol::T (per-lane tiles).
    pub tiles: Vec<[AV; TILE]>,
    /// log2(width) at each tile's creation, parallel to `tiles`. When
    /// `expand` doubles the lane axis (dynamic-expand mode), OLD tiles
    /// are never rewritten: lane j of an old tile is read as
    /// j >> (log_w - tile_log[ix]) - each new lane descends from parent
    /// lane j>>1, so projection is a shift. In concrete-button mode the
    /// width never changes and every shift is 0.
    tile_log: Vec<u8>,
    /// log2(current width) while widths are powers of two (dynamic-
    /// expand mode); 0 and unused otherwise.
    log_w: u8,
    /// Slot compilation (plans/columnar-engine.md): the values of the
    /// gen::SLOT_CELLS boundary cells, bound at pass start
    /// (`bind_slots`) and written back before rows are read off
    /// (`writeback_slots`). Constant indices make the store/load pairs
    /// on these cells compiler-visible.
    pub slots: [TCol; crate::gen::N_SLOTS],
    /// CELESTE_SLOT_GUARD=1: slot_set mirrors into cols and slot_get
    /// compares against them - an aliased generic access to a slot cell
    /// desyncs the two and fires the assert (the runtime guard for what
    /// the escape analysis cannot prove).
    pub guard: bool,
}

/// CELESTE_SLOT_GUARD, read once.
fn slot_guard() -> bool {
    static GUARD: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *GUARD.get_or_init(|| std::env::var("CELESTE_SLOT_GUARD").is_ok_and(|v| v == "1"))
}

impl<const BTN: u8> Rt3<BTN> {
    /// Build a tile from lanes `[lo, hi)` of an Rt2 block. The block must
    /// be at a frame boundary (its columns fully materialized).
    pub fn from_rt2(src: &Rt2, lo: usize, hi: usize) -> Rt3<BTN> {
        let w = hi - lo;
        assert!(w <= TILE);
        let mut tiles: Vec<[AV; TILE]> = Vec::new();
        let mut conv = |c: &Col| -> TCol {
            // A one-row tile is uniform in EVERY column - the dynamic-
            // expand mode's trunk sharing rests on this.
            if w == 1 {
                return TCol::U(match c {
                    Col::U(a) => *a,
                    Col::V(vs) => vs[lo],
                    Col::N(vs) => AV::Num(vs[lo]),
                    Col::I(vs) => AV::Ival(vs[lo].0, vs[lo].1),
                });
            }
            match c {
                Col::U(a) => TCol::U(*a),
                Col::V(vs) => {
                    let mut t = [AV::Nil; TILE];
                    t[..w].copy_from_slice(&vs[lo..hi]);
                    tiles.push(t);
                    TCol::T(tiles.len() as u32 - 1)
                }
                Col::N(vs) => {
                    let mut t = [AV::Nil; TILE];
                    for (i, n) in vs[lo..hi].iter().enumerate() {
                        t[i] = AV::Num(*n);
                    }
                    tiles.push(t);
                    TCol::T(tiles.len() as u32 - 1)
                }
                Col::I(vs) => {
                    let mut t = [AV::Nil; TILE];
                    for (i, (a, b)) in vs[lo..hi].iter().enumerate() {
                        t[i] = AV::Ival(*a, *b);
                    }
                    tiles.push(t);
                    TCol::T(tiles.len() as u32 - 1)
                }
            }
        };
        // Closure captures must be uniform (asserted by tcol_to_col).
        let structure: Vec<Cell2> = src
            .structure
            .iter()
            .map(|cell| match cell {
                Cell2::Clo(f, caps) => Cell2::Clo(
                    *f,
                    caps.iter()
                        .map(|c| match c {
                            Col::U(a) => Col::U(*a),
                            _ => panic!("closure captures are uniform"),
                        })
                        .collect(),
                ),
                other => other.clone(),
            })
            .collect();
        let cols: Vec<TCol> = src.cols.iter().map(&mut conv).collect();
        Rt3 {
            width: w,
            structure,
            cols,
            globals: src.globals.clone(),
            strings: std::sync::Arc::new(src.strings.clone()),
            cart: src.cart.clone(),
            cache: src.cache.clone(),
            prints: src.prints.clone(),
            valid: [true; TILE],
            tape: Vec::new(),
            cursor: 0,
            ks: Vec::new(),
            tile_log: vec![0; tiles.len()],
            tiles,
            log_w: 0,
            slots: [TCol::U(AV::Nil); crate::gen::N_SLOTS],
            guard: slot_guard(),
        }
    }

    /// Copy the slot cells' current values into the slot array. Call
    /// AFTER any direct col mutation of the pass setup (set_buttons).
    pub fn bind_slots(&mut self) {
        for (k, &cell) in crate::gen::SLOT_CELLS.iter().enumerate() {
            debug_assert!(matches!(self.structure[cell as usize], Cell2::Val));
            self.slots[k] = self.cols[cell as usize];
        }
    }

    /// Write the slot array back to the cells. Call before anything
    /// reads cols off the tile (structure_fp / seed_rt2 / append_into).
    pub fn writeback_slots(&mut self) {
        for (k, &cell) in crate::gen::SLOT_CELLS.iter().enumerate() {
            self.cols[cell as usize] = self.slots[k];
        }
    }

    /// Write concrete button bools for input `byte` (set_buttons logic on
    /// the tile: __button_states array items point at value cells).
    pub fn set_buttons(&mut self, g_button_states: u32, byte: u8) {
        let cell = self.globals[g_button_states as usize];
        assert!(cell != NONE, "no __button_states global");
        let arr = match &self.structure[cell as usize] {
            Cell2::Val => match self.cols[cell as usize] {
                TCol::U(AV::Ptr(id)) => id,
                _ => panic!("__button_states shape"),
            },
            Cell2::Arr(_) => cell,
            other => panic!("__button_states shape: {:?}", other),
        };
        let items = match &self.structure[arr as usize] {
            Cell2::Arr(items) => items.clone(),
            other => panic!("button array shape: {:?}", other),
        };
        for (i, item) in items.iter().enumerate() {
            let pressed = byte >> i & 1 == 1;
            let target = match &self.structure[*item as usize] {
                Cell2::Val => match self.cols[*item as usize] {
                    TCol::U(AV::Ptr(id)) => id,
                    _ => *item,
                },
                _ => *item,
            };
            self.structure[target as usize] = Cell2::Val;
            self.cols[target as usize] = TCol::U(AV::Bool(pressed));
        }
    }

    /// Rebind the tile to another input-variant type (fields are
    /// BTN-independent; the parameter only drives const folding).
    pub fn into_variant<const B: u8>(self) -> Rt3<B> {
        Rt3::<B> {
            width: self.width,
            structure: self.structure,
            cols: self.cols,
            globals: self.globals,
            strings: self.strings,
            cart: self.cart,
            cache: self.cache,
            prints: self.prints,
            valid: self.valid,
            tape: self.tape,
            cursor: self.cursor,
            ks: self.ks,
            tiles: self.tiles,
            tile_log: self.tile_log,
            log_w: self.log_w,
            slots: self.slots,
            guard: self.guard,
        }
    }

    /// Prepare for a (re)run pass: valid mask reset, tape cursor rewound
    /// (the tape itself carries the pass's choices).
    pub fn begin_pass(&mut self) {
        self.valid = [true; TILE];
        self.cursor = 0;
        self.ks.clear();
    }

    /// Advance the choice tape to the next pass (odometer over the
    /// per-site alternative counts recorded this pass). False = done.
    pub fn advance_tape(&mut self) -> bool {
        // ks[j] is the max alternatives seen at site j this pass; the tape
        // may be shorter (sites first reached this pass) - extend with 0s.
        while self.tape.len() < self.ks.len() {
            self.tape.push(0);
        }
        for j in (0..self.tape.len()).rev() {
            if (self.tape[j] as usize + 1) < self.ks[j] as usize {
                self.tape[j] += 1;
                self.tape.truncate(j + 1);
                return true;
            }
        }
        false
    }

    #[inline]
    pub fn tat(&self, c: TCol, lane: usize) -> AV {
        match c {
            TCol::U(a) => a,
            TCol::T(ix) => {
                let shift = self.log_w - self.tile_log[ix as usize];
                self.tiles[ix as usize][lane >> shift]
            }
        }
    }

    /// Materialized copy of a value's first `width` lanes.
    #[inline]
    fn tdat(&self, c: TCol) -> [AV; TILE] {
        let mut o = [AV::Nil; TILE];
        for i in 0..self.width {
            o[i] = self.tat(c, i);
        }
        o
    }

    #[inline]
    fn put_tile(&mut self, t: [AV; TILE]) -> TCol {
        let ix = self.tiles.len() as u32;
        self.tiles.push(t);
        self.tile_log.push(self.log_w);
        TCol::T(ix)
    }

    fn uptr(&self, id: TCol) -> u32 {
        match id {
            TCol::U(AV::Ptr(p)) => p,
            TCol::U(_) => bail(),
            c @ TCol::T(_) => {
                let AV::Ptr(p) = self.tat(c, 0) else { bail() };
                if !(0..self.width).all(|i| matches!(self.tat(c, i), AV::Ptr(q) if q == p)) {
                    bail();
                }
                p
            }
        }
    }

    #[inline]
    fn map1(&mut self, a: TCol, f: impl Fn(AV) -> AV) -> TCol {
        match a {
            TCol::U(x) => {
                let out = TCol::U(f(x));
                out
            }
            c @ TCol::T(_) => {
                let mut o = [AV::Nil; TILE];
                for i in 0..self.width {
                    o[i] = f(self.tat(c, i));
                }
                let out = self.put_tile(o);
                out
            }
        }
    }

    #[inline]
    fn map2(&mut self, a: TCol, b: TCol, f: impl Fn(AV, AV) -> AV) -> TCol {
        let (x, y) = (a, b);
        if let (TCol::U(x), TCol::U(y)) = (x, y) {
            let out = TCol::U(f(x, y));
            return out;
        }
        let mut o = [AV::Nil; TILE];
        for i in 0..self.width {
            o[i] = f(self.tat(x, i), self.tat(y, i));
        }
        let out = self.put_tile(o);
        out
    }

    /// A lane-multiplying op under the counter tape: `n_alts(lane)` and
    /// `alt(lane, k)` describe the alternatives; the site consumes one
    /// tape slot. Lanes with fewer alternatives than the chosen index
    /// clamp (their value is a repeat of an earlier pass) and lose their
    /// `valid` bit for this pass.
    fn split_site(
        &mut self,
        n_alts: impl Fn(usize) -> usize,
        alt: impl Fn(usize, usize) -> AV,
    ) -> TCol {
        let site = self.cursor;
        self.cursor += 1;
        let choice = *self.tape.get(site).unwrap_or(&0) as usize;
        let mut max_k = 1usize;
        let mut o = [AV::Nil; TILE];
        for i in 0..self.width {
            let k = n_alts(i);
            max_k = max_k.max(k);
            if choice >= k {
                self.valid[i] = false;
                o[i] = alt(i, k - 1);
            } else {
                o[i] = alt(i, choice);
            }
        }
        if self.ks.len() <= site {
            self.ks.resize(site + 1, 1);
        }
        self.ks[site] = self.ks[site].max(max_k as u8);
        // Uniform result stays uniform.
        if o[..self.width].iter().all(|v| *v == o[0]) {
            return TCol::U(o[0]);
        }
        let out = self.put_tile(o);
        out
    }
}

fn tcol_to_col(t: TCol) -> Col {
    match t {
        TCol::U(a) => Col::U(a),
        TCol::T(_) => panic!("closure captures are uniform"),
    }
}

impl<const BTN: u8> Rt3<BTN> {
    /// Fingerprint of the post-frame structure + globals: variants of one
    /// chunk must evolve identically (uniform ops) for their lanes to be
    /// accumulated into one block; checked via this hash.
    pub fn structure_fp(&self) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        for cell in &self.structure {
            match cell {
                Cell2::Val => h.write_u8(1),
                Cell2::Obj(fields) => {
                    h.write_u8(2);
                    for (k, t) in fields {
                        h.write_u32(*k);
                        h.write_u32(*t);
                    }
                }
                Cell2::Arr(items) => {
                    h.write_u8(3);
                    for t in items {
                        h.write_u32(*t);
                    }
                }
                Cell2::Unk => h.write_u8(4),
                Cell2::Clo(f, caps) => {
                    h.write_u8(5);
                    h.write_u32(*f);
                    for c in caps.iter() {
                        match c {
                            Col::U(AV::Ptr(p)) => h.write_u32(*p),
                            Col::U(_) => h.write_u8(9),
                            _ => h.write_u8(10),
                        }
                    }
                }
                Cell2::Bi(b) => {
                    h.write_u8(6);
                    h.write_u32(*b);
                }
            }
            h.write_u8(0);
        }
        for g in &self.globals {
            h.write_u32(*g);
        }
        h.finish()
    }

    /// Seed a chunk accumulator from this tile's post-frame state (the
    /// one-time structure/strings materialization for the whole chunk).
    pub fn seed_rt2(&self) -> Rt2 {
        let mut out = Rt2::empty(
            0,
            self.globals.len(),
            &[],
            self.cart.clone(),
            self.cache.clone(),
        );
        out.strings = (*self.strings).clone();
        out.prints = self.prints.clone();
        out.globals = self.globals.clone();
        out.structure = self.structure.clone();
        out.cols = self.cols.iter().map(|_| Col::U(AV::Nil)).collect();
        out
    }

    /// Append this tile's valid lanes into the chunk accumulator
    /// (structures already fingerprint-checked by the caller). Uniform
    /// columns stay uniform while every appended value agrees.
    pub fn append_into(&self, acc: &mut Rt2) {
        let lanes: Vec<usize> = (0..self.width).filter(|&i| self.valid[i]).collect();
        let w = acc.width;
        for (c, col) in self.cols.iter().enumerate() {
            let acol = &mut acc.cols[c];
            if let (TCol::U(a), Col::U(b)) = (col, &*acol) {
                if w == 0 || *a == *b {
                    *acol = Col::U(*a);
                    continue;
                }
            }
            // Materialize the accumulator column and extend.
            let mut vs: Vec<AV> = match acol {
                Col::U(a) => vec![*a; w],
                Col::V(v) => std::mem::take(v),
                Col::N(v) => v.iter().map(|n| AV::Num(*n)).collect(),
                Col::I(v) => v.iter().map(|(a, b)| AV::Ival(*a, *b)).collect(),
            };
            match col {
                TCol::U(a) => vs.extend(std::iter::repeat(*a).take(lanes.len())),
                c @ TCol::T(_) => {
                    vs.extend(lanes.iter().map(|&i| self.tat(*c, i)));
                }
            }
            *acol = Col::V(vs);
        }
        acc.width += lanes.len();
    }
}

impl<const BTN: u8> Engine for Rt3<BTN> {
    type V = TCol;

    const HAS_SLOTS: bool = true;

    #[inline(always)]
    fn slot_get(&mut self, k: u32) -> TCol {
        let v = self.slots[k as usize];
        if self.guard {
            let cell = crate::gen::SLOT_CELLS[k as usize] as usize;
            let c = self.cols[cell];
            for lane in 0..self.width {
                assert!(
                    self.tat(v, lane) == self.tat(c, lane),
                    "slot {} desynced from cell {} at lane {}: {:?} vs {:?} \
                     (an access path outside the slot binding wrote this cell)",
                    k,
                    cell,
                    lane,
                    self.tat(v, lane),
                    self.tat(c, lane)
                );
            }
        }
        v
    }

    #[inline(always)]
    fn slot_set(&mut self, k: u32, x: TCol) {
        self.slots[k as usize] = x;
        if self.guard {
            let cell = crate::gen::SLOT_CELLS[k as usize] as usize;
            self.structure[cell] = Cell2::Val;
            self.cols[cell] = x;
        }
    }

    fn c_num(&mut self, hi: i16, lo: u16) -> TCol {
        TCol::U(AV::Num(P8::from_parts(hi, lo)))
    }
    fn c_bool(&mut self, b: bool) -> TCol {
        TCol::U(AV::Bool(b))
    }
    fn c_str(&mut self, s: u32) -> TCol {
        TCol::U(AV::Str(s))
    }
    fn c_nil(&mut self) -> TCol {
        TCol::U(AV::Nil)
    }

    fn alloc_nil(&mut self) -> TCol {
        let id = self.structure.len() as u32;
        self.structure.push(Cell2::Val);
        self.cols.push(TCol::U(AV::Nil));
        TCol::U(AV::Ptr(id))
    }

    fn get_global(&mut self, g: u32, create: bool) -> TCol {
        let cell = self.globals[g as usize];
        if cell != NONE {
            TCol::U(AV::Ptr(cell))
        } else if create {
            let id = self.structure.len() as u32;
            self.structure.push(Cell2::Val);
            self.cols.push(TCol::U(AV::Nil));
            self.globals[g as usize] = id;
            TCol::U(AV::Ptr(id))
        } else {
            TCol::U(AV::NilPtr)
        }
    }

    fn load(&mut self, v: TCol) -> TCol {
        let p = match v {
            TCol::U(AV::Ptr(p)) => p,
            TCol::U(AV::NilPtr) => return TCol::U(AV::Nil),
            _ => self.uptr(v),
        };
        match &self.structure[p as usize] {
            Cell2::Val => {
                let c = self.cols[p as usize];
                c
            }
            _ => TCol::U(AV::Ptr(p)),
        }
    }

    fn store(&mut self, t: TCol, s: TCol) {
        let p = self.uptr(t) as usize;
        self.structure[p] = Cell2::Val;
        self.cols[p] = s;
    }

    fn store_empty_table(&mut self, t: TCol) {
        let p = self.uptr(t) as usize;
        self.structure[p] = Cell2::Unk;
        self.cols[p] = TCol::U(AV::Nil);
    }

    fn store_closure(&mut self, t: TCol, f: u32, caps: &[TCol]) {
        let p = self.uptr(t) as usize;
        let caps: Box<[Col]> = caps.iter().map(|c| tcol_to_col(*c)).collect();
        self.structure[p] = Cell2::Clo(f, caps);
        self.cols[p] = TCol::U(AV::Nil);
    }

    fn get_field(&mut self, recv: TCol, f: u32, create: bool, _site: u32) -> TCol {
        let table = self.uptr(recv) as usize;
        let existing = match &self.structure[table] {
            Cell2::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
            Cell2::Unk => None,
            _ => bail(),
        };
        if let Some(cell) = existing {
            TCol::U(AV::Ptr(cell))
        } else if create {
            let cell = self.structure.len() as u32;
            self.structure.push(Cell2::Val);
            self.cols.push(TCol::U(AV::Nil));
            match &mut self.structure[table] {
                Cell2::Obj(fields) => fields.push((f, cell)),
                slot @ Cell2::Unk => *slot = Cell2::Obj(vec![(f, cell)]),
                _ => unreachable!(),
            }
            TCol::U(AV::Ptr(cell))
        } else {
            TCol::U(AV::NilPtr)
        }
    }

    fn get_index(&mut self, recv: TCol, idx: TCol, create: bool, _site: u32) -> TCol {
        let table = self.uptr(recv) as usize;
        let index = match idx {
            TCol::U(AV::Num(n)) => match n.as_i16() {
                Some(i) => i,
                None => bail(),
            },
            _ => bail(),
        };
        if index < 1 {
            bail();
        }
        let existing = match &self.structure[table] {
            Cell2::Arr(items) => items.get(index as usize - 1).copied(),
            Cell2::Unk => None,
            _ => bail(),
        };
        if let Some(cell) = existing {
            TCol::U(AV::Ptr(cell))
        } else if create {
            let old_len = match &self.structure[table] {
                Cell2::Arr(items) => items.len(),
                Cell2::Unk => 0,
                _ => unreachable!(),
            };
            let mut tail: Vec<u32> = Vec::new();
            for _ in old_len..(index as usize) {
                let gap = self.structure.len() as u32;
                self.structure.push(Cell2::Val);
                self.cols.push(TCol::U(AV::Nil));
                tail.push(gap);
            }
            let cell = *tail.last().unwrap();
            match &mut self.structure[table] {
                Cell2::Arr(items) => items.extend_from_slice(&tail),
                slot @ Cell2::Unk => *slot = Cell2::Arr(tail),
                _ => unreachable!(),
            }
            TCol::U(AV::Ptr(cell))
        } else {
            TCol::U(AV::NilPtr)
        }
    }

    fn op_add(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_addsub(a, b, false))
    }
    fn op_sub(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_addsub(a, b, true))
    }
    fn op_mul(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, av_mul)
    }
    fn op_div(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, av_div)
    }
    fn op_rem(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, av_rem)
    }
    fn op_pow(&mut self, _l: TCol, _r: TCol) -> TCol {
        bail()
    }
    fn eq(&mut self, l: TCol, r: TCol) -> TCol {
        let (x, y) = ((l), (r));
        if let (TCol::U(x), TCol::U(y)) = (x, y) {
            let out = TCol::U(av_eq(x, y, &self.strings));
            return out;
        }
        let mut o = [AV::Nil; TILE];
        for i in 0..self.width {
            o[i] = av_eq(self.tat(x, i), self.tat(y, i), &self.strings);
        }
        let out = self.put_tile(o);
        out
    }
    fn ne(&mut self, l: TCol, r: TCol) -> TCol {
        let e = self.eq(l, r);
        self.map1(e, av_not)
    }
    fn lt(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_cmp(CmpOp::Lt, a, b))
    }
    fn le(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_cmp(CmpOp::Le, a, b))
    }
    fn gt(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_cmp(CmpOp::Gt, a, b))
    }
    fn ge(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, |a, b| av_cmp(CmpOp::Ge, a, b))
    }
    fn concat(&mut self, _l: TCol, _r: TCol) -> TCol {
        bail()
    }
    fn un_minus(&mut self, v: TCol) -> TCol {
        self.map1(v, av_neg)
    }
    fn un_not(&mut self, v: TCol) -> TCol {
        self.map1(v, av_not)
    }
    fn un_hash(&mut self, v: TCol) -> TCol {
        let out = match v {
            TCol::U(AV::Str(s)) => AV::Num(P8::from_i16(self.strings[s as usize].len() as i16)),
            _ => {
                let p = self.uptr(v);
                match &self.structure[p as usize] {
                    Cell2::Arr(items) => AV::Num(P8::from_i16(items.len() as i16)),
                    Cell2::Obj(_) | Cell2::Unk => AV::Num(P8::from_i16(0)),
                    _ => bail(),
                }
            }
        };
        TCol::U(out)
    }

    fn select(&mut self, c: TCol, t: TCol, f: TCol) -> TCol {
        // Uniform condition: the chosen arm passes through UNTOUCHED
        // (no per-lane pick, no tile copy) - in branch-free code most
        // select conditions are tile-uniform.
        match c {
            TCol::U(AV::Bool(true)) => return t,
            TCol::U(AV::Bool(false)) => return f,
            TCol::U(_) => bail(),
            TCol::T(_) => {}
        }
        let pick = |cv: AV, tv: AV, fv: AV| -> AV {
            match cv {
                AV::Bool(true) => tv,
                AV::Bool(false) => fv,
                _ => bail(),
            }
        };
        let mut o = [AV::Nil; TILE];
        for i in 0..self.width {
            o[i] = pick(self.tat(c, i), self.tat(t, i), self.tat(f, i));
        }
        self.put_tile(o)
    }

    /// Concrete-button mode: bools pass through (a UBool means the
    /// driver forgot set_buttons). Dynamic-expand mode: a uniform UBool
    /// DOUBLES the lane axis in-tile - new lane j descends from parent
    /// lane j>>1 and gets button value j&1; existing tiles are read
    /// through the shift projection (tat), never rewritten. This is the
    /// trunk-sharing fan-out: everything before the first expand ran
    /// once, uniformly, for all input variants.
    fn expand(&mut self, v: TCol) -> TCol {
        match v {
            TCol::U(AV::Bool(_)) => v,
            TCol::U(AV::UBool) => {
                let nw = self.width * 2;
                if nw > TILE || !self.width.is_power_of_two() {
                    bail();
                }
                let old_valid = self.valid;
                for j in 0..nw {
                    self.valid[j] = old_valid[j >> 1];
                }
                self.width = nw;
                self.log_w += 1;
                let mut t = [AV::Nil; TILE];
                for (j, slot) in t.iter_mut().enumerate().take(nw) {
                    *slot = AV::Bool(j & 1 == 1);
                }
                self.put_tile(t)
            }
            c @ TCol::T(_)
                if (0..self.width).all(|i| matches!(self.tat(c, i), AV::Bool(_))) =>
            {
                c
            }
            _ => bail(),
        }
    }

    /// The transpiler-resolved button expand: a compile-time constant in
    /// this variant. The loaded cell value must agree (set_buttons wrote
    /// it) - disagreement means a mis-resolved site: bail loudly to Rt2.
    #[inline(always)]
    fn expand_btn<const K: u32>(&mut self, v: TCol) -> TCol {
        // BTN 0xFF = the dynamic-expand mode sentinel: no variant
        // specialization, every button fans out in-tile.
        if BTN == 0xFF {
            return self.expand(v);
        }
        // Only the bits this variant type is specialized on fold; other
        // buttons pass their (concrete) cell value through.
        if K == 4 || K == 5 {
            let want = (BTN >> K) & 1 == 1;
            match v {
                TCol::U(AV::Bool(b)) if b == want => TCol::U(AV::Bool(want)),
                _ => bail(),
            }
        } else {
            self.expand(v)
        }
    }

    fn truthy_b(&mut self, v: TCol, _site: u32) -> bool {
        match v {
            TCol::U(a) => av_truthy(a),
            c @ TCol::T(_) => {
                let t = av_truthy(self.tat(c, 0));
                if !(0..self.width).all(|i| av_truthy(self.tat(c, i)) == t) {
                    bail();
                }
                t
            }
        }
    }

    fn kill(&mut self, _vs: &[TCol]) {}

    fn assert_closure(&mut self, v: TCol, f: u32, caps: &[TCol], _ctx: &str) {
        let p = self.uptr(v);
        match &self.structure[p as usize] {
            Cell2::Clo(cf, cc) if *cf == f && cc.len() == caps.len() => {
                for (stored, want) in cc.iter().zip(caps) {
                    let ok = match (stored, (*want)) {
                        (Col::U(a), TCol::U(b)) => *a == b,
                        _ => false,
                    };
                    if !ok {
                        bail();
                    }
                }
            }
            _ => bail(),
        }
    }
    fn assert_pointer(&mut self, v: TCol, _ctx: &str) {
        match v {
            TCol::U(AV::Ptr(_)) => {}
            _ => bail(),
        }
    }
    fn assert_value_cell(&mut self, v: TCol, _ctx: &str) {
        let p = self.uptr(v);
        if !matches!(self.structure[p as usize], Cell2::Val) {
            bail();
        }
    }
    fn assert_true(&mut self, v: TCol, _ctx: &str) {
        match v {
            TCol::U(AV::Bool(true)) => {}
            c @ TCol::T(_)
                if (0..self.width).all(|i| matches!(self.tat(c, i), AV::Bool(true))) => {}
            _ => bail(),
        }
    }
    fn assert_builtin(&mut self, v: TCol, b: u32) {
        let p = self.uptr(v);
        if !matches!(&self.structure[p as usize], Cell2::Bi(x) if *x == b) {
            bail();
        }
    }

    fn bi_min(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, av_min)
    }
    fn bi_max(&mut self, l: TCol, r: TCol) -> TCol {
        self.map2(l, r, av_max)
    }
    fn bi_abs(&mut self, v: TCol) -> TCol {
        self.map1(v, av_abs)
    }
    fn bi_flr(&mut self, v: TCol) -> TCol {
        self.map1(v, av_flr)
    }
    fn bi_sin(&mut self, v: TCol) -> TCol {
        self.map1(v, av_sin)
    }
    fn bi_mget(&mut self, x: TCol, y: TCol) -> TCol {
        let cart = self.cart.clone();
        self.map2(x, y, move |a, b| match (a, b) {
            (AV::Num(x), AV::Num(y)) => {
                AV::Num(P8::from_i16(cart.mget(x, y).expect("mget failed") as i16))
            }
            _ => bail(),
        })
    }
    fn bi_tile_flag_at(&mut self, x: TCol, y: TCol, w: TCol, h: TCol, f: TCol) -> TCol {
        let flag = match f {
            TCol::U(AV::Num(n)) => match n.as_i16() {
                Some(i) => i,
                None => bail(),
            },
            _ => bail(),
        };
        if flag != 0 {
            return TCol::U(AV::Bool(false));
        }
        let wi = match w {
            TCol::U(AV::Num(n)) => n.as_i16().unwrap_or_else(|| bail()),
            _ => bail(),
        };
        let hi = match h {
            TCol::U(AV::Num(n)) => n.as_i16().unwrap_or_else(|| bail()),
            _ => bail(),
        };
        let cache = self.cache.clone();
        let cart = self.cart.clone();
        self.map2(x, y, move |a, b| {
            let (AV::Num(xn), AV::Num(yn)) = (a, b) else { bail() };
            let (Some(xi), Some(yi)) = (xn.as_i16(), yn.as_i16()) else {
                bail()
            };
            if let Some((map, dx, dy)) = cache.solid_map(wi, hi) {
                if let Some(v) = map.get(xi + dx, yi + dy) {
                    return AV::Bool(v);
                }
            }
            AV::Bool(cache.solid_at(&cart, xi, yi, wi, hi).unwrap_or(false))
        })
    }

    fn call_builtin(&mut self, b: u32, args: &[TCol]) -> TCol {
        match b {
            BI___PRINT | BI_PRINT => bail(),
            BI___NEW_UNKNOWN_BOOLEAN => TCol::U(AV::UBool),
            BI___WIDEN_REM | BI___NEW_VECTOR | BI_ERROR => bail(),
            BI___ARRAY_TABLE_DROP_LAST => {
                let p = self.uptr(args[0]) as usize;
                match &mut self.structure[p] {
                    Cell2::Arr(items) => {
                        if items.is_empty() {
                            bail();
                        }
                        items.pop();
                    }
                    _ => bail(),
                }
                TCol::U(AV::Nil)
            }
            BI_MIN => self.bi_min(args[0], args[1]),
            BI_MAX => self.bi_max(args[0], args[1]),
            BI_ABS => self.bi_abs(args[0]),
            BI_FLR => self.bi_flr(args[0]),
            BI_SIN => self.bi_sin(args[0]),
            BI_MGET => self.bi_mget(args[0], args[1]),
            BI_FGET => {
                let cart = self.cart.clone();
                self.map2(args[0], args[1], move |a, b| match (a, b) {
                    (AV::Num(x), AV::Num(y)) => {
                        AV::Bool(cart.fget(x, y).expect("fget failed"))
                    }
                    _ => bail(),
                })
            }
            BI_TILE_FLAG_AT => self.bi_tile_flag_at(args[0], args[1], args[2], args[3], args[4]),
            BI___SPLIT_BY_FLR => {
                let d = args[0];
                let needs = match d {
                    TCol::U(AV::Num(_)) => false,
                    TCol::U(AV::Ival(a, b)) => a.flr() != b.flr(),
                    c @ TCol::T(_) => (0..self.width).any(|i| match self.tat(c, i) {
                        AV::Ival(a, b) => a.flr() != b.flr(),
                        AV::Num(_) => false,
                        _ => bail(),
                    }),
                    _ => bail(),
                };
                if !needs {
                    return d;
                }
                let subs = |v: AV| -> Vec<Pico8NumInterval> {
                    match v {
                        AV::Ival(a, b) => {
                            split_iv_by_floor(Pico8NumInterval::new(a, b))
                        }
                        _ => Vec::new(),
                    }
                };
                let dat: [AV; TILE] = self.tdat(d);
                self.split_site(
                    |i| match dat[i] {
                        AV::Num(_) => 1,
                        v @ AV::Ival(..) => subs(v).len(),
                        _ => bail(),
                    },
                    |i, k| match dat[i] {
                        AV::Num(n) => AV::Num(n),
                        v @ AV::Ival(..) => {
                            let s = subs(v)[k];
                            if s.low == s.high {
                                AV::Num(s.low)
                            } else {
                                AV::Ival(s.low, s.high)
                            }
                        }
                        _ => unreachable!(),
                    },
                )
            }
            BI___SPLIT_AT => {
                let c = match args[1] {
                    TCol::U(AV::Num(n)) => n,
                    _ => bail(),
                };
                let full_low = P8::from_parts(i16::MIN, 0);
                let full_high = P8::from_parts(i16::MAX, 0xffff);
                let mut sides: Vec<Pico8NumInterval> = Vec::with_capacity(3);
                if c > full_low {
                    sides.push(Pico8NumInterval::new(full_low, c.next_smallest()));
                }
                sides.push(Pico8NumInterval::from_number(c));
                if c < full_high {
                    sides.push(Pico8NumInterval::new(c.next_largest(), full_high));
                }
                let d = args[0];
                let lane_alts = |v: AV| -> Vec<AV> {
                    match v {
                        AV::Num(n) => vec![AV::Num(n)],
                        AV::Ival(a, b) => {
                            let iv = Pico8NumInterval::new(a, b);
                            sides
                                .iter()
                                .filter_map(|s| iv.intersect(s))
                                .map(|clipped| {
                                    if clipped.low == clipped.high {
                                        AV::Num(clipped.low)
                                    } else {
                                        AV::Ival(clipped.low, clipped.high)
                                    }
                                })
                                .collect()
                        }
                        _ => bail(),
                    }
                };
                let needs = match d {
                    TCol::U(v) => lane_alts(v).len() > 1,
                    c @ TCol::T(_) => {
                        (0..self.width).any(|i| lane_alts(self.tat(c, i)).len() > 1)
                    }
                };
                if !needs {
                    return self.map1(args[0], |v| lane_alts(v)[0]);
                }
                let dat: [AV; TILE] = self.tdat(d);
                self.split_site(|i| lane_alts(dat[i]).len(), |i, k| lane_alts(dat[i])[k])
            }
            BI_ADD => {
                let p = self.uptr(args[0]) as usize;
                let value = args[1];
                let cell = self.structure.len() as u32;
                self.structure.push(Cell2::Val);
                self.cols.push(value);
                match &mut self.structure[p] {
                    Cell2::Arr(items) => items.push(cell),
                    slot @ Cell2::Unk => *slot = Cell2::Arr(vec![cell]),
                    _ => bail(),
                }
                value
            }
            _ => bail(),
        }
    }

    fn callee_of(&mut self, c: TCol, _ctx: &str) -> Callee<TCol> {
        let p = self.uptr(c);
        match &self.structure[p as usize] {
            Cell2::Clo(f, caps) => {
                let f = *f;
                let caps: Vec<TCol> = caps
                    .iter()
                    .map(|c| match c {
                        Col::U(a) => TCol::U(*a),
                        _ => bail(),
                    })
                    .collect();
                Callee::Fn(f, caps.into_iter().map(|c| c).collect())
            }
            Cell2::Bi(b) => Callee::Bi(*b),
            _ => bail(),
        }
    }
}
