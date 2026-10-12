//! THE EMISSION of a kernel slice (plans/emit-batch.md): what a body's
//! `live & !error` lanes become - per lane its state's cell, key and
//! transfer - computed COLUMN-WISE, then handed to the sink a body at a time
//! (`unit::BodyEmits`).
//!
//! Bodies share output slots (one per distinct root node, `build_one_shape`),
//! so the bodies of a slice repeat each other's work: the reference frame
//! averaged 4.5-5.6 key-field lookups per distinct (field, value) and 8-12
//! transfer decodes per distinct transfer (plans/fwd-perf.md, `[keydiag]`).
//! A kernel's EMISSION PLAN (`Plan`, built with it) lists the distinct
//! READS - a key field of an output shape (`KeyField`), a transfer's root
//! set (`ArcSlots`), a cell's position sources (`PosSrc`), level -1's
//! (shape, cell column) - once, each body naming its own by index. A slice
//! evaluates each read ONCE over the union of the lanes the bodies that
//! name it take (`Cols`), lanes of equal values in one probe; each body
//! then ORs its fields' placed bits into its lanes' keys.
//!
//! The order of the sink's work is a body's, lane by lane, as before; what
//! moved is only where the cells, keys and transfers are computed. The
//! results are the same values, so the frame's states, edges and transfer
//! ids are byte-identical (the worker-local ids' first-use order is not,
//! but a slice's new transfers get their global ids by content).

use std::collections::HashMap;

use anyhow::Result;

use celeste_engine::exact::{Code, FieldDict, MissKey, ShapeKeys};
use celeste_engine::runtime2::{Rt2, POS_WHOLE};

use super::{AsmKernel, RootKind};
use crate::storage::unit::lanes_of;
use crate::search::arc_edges::{decode_axis, RawAxis};

/// One per-row field of a key: its cell (the shape's dictionary), output
/// slot and how the slot reads as a code.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct KeyField {
    pub cell: u32,
    root: usize,
    read: KeyRead,
}

/// How a key field's slot becomes its code (a position coordinate's:
/// `exact::Code::pos`).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum KeyRead {
    Num,
    Ival,
    Bool,
    PosNum,
    PosIval,
}

/// 16 lanes of 128-bit keys (or a field's placed bits in them) as two
/// planes of 64-bit words, low then high: ORed plane by plane, a body's key
/// assembly is vector code (as `[u128; 16]` it was scalar, 13% of the
/// reference frame's wave).
#[derive(Clone, Copy, Default)]
pub(super) struct Placed([[u64; 16]; 2]);

impl Placed {
    #[inline]
    fn splat(v: u128) -> Placed {
        Placed([[v as u64; 16], [(v >> 64) as u64; 16]])
    }

    /// `v` in the lanes of `m` (a masked blend).
    #[inline]
    fn fill(&mut self, m: u16, v: u128) {
        if m == 0 {
            return;
        }
        let w = [v as u64, (v >> 64) as u64];
        for p in 0..2 {
            for (j, x) in self.0[p].iter_mut().enumerate() {
                *x = if m >> j & 1 == 1 { w[p] } else { *x };
            }
        }
    }

    #[inline]
    fn get(&self, i: usize) -> u128 {
        (self.0[1][i] as u128) << 64 | self.0[0][i] as u128
    }

    /// OR `o`'s plane `p` in.
    #[inline]
    fn or(&mut self, o: &Placed, p: usize) {
        for (a, b) in self.0[p].iter_mut().zip(&o.0[p]) {
            *a |= b;
        }
    }

    /// Does any lane hold bits past the low word?
    #[inline]
    fn high(&self) -> bool {
        self.0[1].iter().fold(0, |a, &x| a | x) != 0
    }
}

/// The 16 lanes of the 32-bit column at byte `o`.
#[inline]
fn col(buf: &[u8], o: usize) -> [u32; 16] {
    let b: &[u8; 64] = buf[o..o + 64].try_into().unwrap();
    std::array::from_fn(|i| u32::from_le_bytes([b[4 * i], b[4 * i + 1], b[4 * i + 2], b[4 * i + 3]]))
}

/// The lanes where `c` holds `v`.
#[inline]
fn eq_mask(c: &[u32; 16], v: u32) -> u16 {
    let mut m = 0u16;
    for (i, &x) in c.iter().enumerate() {
        m |= ((x == v) as u16) << i;
    }
    m
}

/// A boolean root's `(value, known)` lane masks.
#[inline]
fn bool_masks(buf: &[u8], o: usize) -> (u16, u16) {
    (u16::from_le_bytes([buf[o], buf[o + 1]]), u16::from_le_bytes([buf[o + 2], buf[o + 3]]))
}

/// The lanes of `of` whose boolean `(v, k)` equals lane `r`'s.
#[inline]
fn bool_same(v: u16, k: u16, r: usize, of: u16) -> u16 {
    let splat = |m: u16| if m >> r & 1 == 1 { 0xffff } else { 0 };
    of & !((v ^ splat(v)) | (k ^ splat(k)))
}

impl KeyField {
    /// `pos`: the cell is a position coordinate (`Rt2::position_cells`),
    /// which keys without its whole pixels.
    pub(super) fn new(cell: u32, root: usize, kind: RootKind, pos: bool) -> Result<KeyField> {
        let read = match (kind, pos) {
            (RootKind::Num, false) => KeyRead::Num,
            (RootKind::Ival, false) => KeyRead::Ival,
            (RootKind::Bool, false) => KeyRead::Bool,
            (RootKind::Num, true) => KeyRead::PosNum,
            (RootKind::Ival, true) => KeyRead::PosIval,
            (RootKind::Bool, true) => anyhow::bail!("position cell {cell} is a boolean output: its row would have no cell"),
        };
        Ok(KeyField { cell, root, read })
    }

    /// The `(lo, hi)` code words of every lane of a number field (a
    /// position less its low end's whole pixels).
    #[inline]
    fn num_words(&self, buf: &[u8]) -> ([u32; 16], [u32; 16]) {
        let base = self.root;
        match self.read {
            KeyRead::Num => {
                let w = col(buf, base);
                (w, w)
            }
            KeyRead::Ival => (col(buf, base), col(buf, base + 64)),
            KeyRead::PosNum => {
                let w = col(buf, base).map(|x| x & !POS_WHOLE);
                (w, w)
            }
            KeyRead::PosIval => {
                let (lo, hi) = (col(buf, base), col(buf, base + 64));
                (std::array::from_fn(|i| lo[i] & !POS_WHOLE), std::array::from_fn(|i| hi[i].wrapping_sub(lo[i] & POS_WHOLE)))
            }
            KeyRead::Bool => unreachable!("a boolean field has no number words"),
        }
    }

    /// THE KEY READ of the lanes `need`: each one's code's placed bits in
    /// `d` into `placed`; returns the lanes whose code `d` lacks (their
    /// `placed` 0: a provisional key's partial key holds the fields that
    /// hit, `MissKey`). Lanes of equal codes are one lookup: a
    /// small dictionary is matched lane-parallel code by code, a big one
    /// probed once per distinct value among the lanes.
    #[inline]
    fn eval(&self, buf: &[u8], d: &FieldDict, need: u16, placed: &mut Placed) -> u16 {
        if self.read == KeyRead::Bool {
            let (v, k) = bool_masks(buf, self.root);
            let mut miss = 0u16;
            for (tri, lanes) in [(0, need & k & !v), (1, need & k & v), (2, need & !k)] {
                if lanes != 0 {
                    let p = d.find_bool_placed(tri);
                    placed.fill(lanes, p.unwrap_or(0));
                    if p.is_none() {
                        miss |= lanes;
                    }
                }
            }
            return miss;
        }
        let (lo, hi) = self.num_words(buf);
        if let Some(small) = d.small_nums() {
            let mut hit = 0u16;
            for &(word, p) in small {
                let eq = eq_mask(&lo, (word >> 32) as u32) & eq_mask(&hi, word as u32) & need;
                hit |= eq;
                placed.fill(eq, p);
            }
            placed.fill(need & !hit, 0);
            return need & !hit;
        }
        let mut miss = 0u16;
        let mut left = need;
        while left != 0 {
            let r = left.trailing_zeros() as usize;
            let same = eq_mask(&lo, lo[r]) & eq_mask(&hi, hi[r]) & left;
            left &= !same;
            let p = d.find_num_placed(Code::num(lo[r], hi[r]).word());
            placed.fill(same, p.unwrap_or(0));
            if p.is_none() {
                miss |= same;
            }
        }
        miss
    }

    /// Lane `i`'s code.
    fn code(&self, buf: &[u8], i: usize) -> Code {
        let w = |o: usize| u32::from_le_bytes(buf[o + 4 * i..o + 4 * i + 4].try_into().unwrap());
        let base = self.root;
        match self.read {
            KeyRead::Num => Code::num(w(base), w(base)),
            KeyRead::Ival => Code::num(w(base), w(base + 64)),
            KeyRead::Bool => {
                let (v, k) = bool_masks(buf, base);
                Code::boolean((k >> i & 1 == 1).then_some(v >> i & 1 == 1))
            }
            KeyRead::PosNum => Code::pos(w(base), w(base)),
            KeyRead::PosIval => Code::pos(w(base), w(base + 64)),
        }
    }
}

/// A body's transfer roots (`FrameOut::arc`): per axis took, pre, frag, ox,
/// fin as (offset, kind); `fin` whether the outcome ends with a player.
/// `ArcSlots::words`: two words per transfer root, then the outcome's
/// `fin` flag - the words alone do not decode: without a fin root its words
/// are 0 and the axis has no `fin`, with one they are `fin = Some((0, 0))`.
/// Keyed on the words alone, `UnitSink::xfer_id_raw`'s cache gave a lane
/// whichever body's transfer was interned first (a scheduling-dependent edge,
/// room (1,0) f61, 2026-10-09).
pub(crate) const RAW_WORDS: usize = 4 * crate::trace::verify::ARC_AXIS_ROOTS + 1;
pub(crate) type RawWords = [u32; RAW_WORDS];

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct ArcSlots {
    pub roots: [(usize, RootKind); 2 * crate::trace::verify::ARC_AXIS_ROOTS],
    pub fin: bool,
}

impl ArcSlots {
    /// The roots the transfer reads (without a `fin`, not the fin roots).
    #[inline]
    fn read_roots(&self) -> impl Iterator<Item = (usize, (usize, RootKind))> + '_ {
        let fin = self.fin;
        self.roots.iter().copied().enumerate().filter(move |(k, _)| fin || k % crate::trace::verify::ARC_AXIS_ROOTS != 4)
    }

    /// Lane `i`'s transfer as plain words off the output slots: per root
    /// its (low, high) 16.16 words, a boolean root's (value, known) bits,
    /// the fin roots 0 where the outcome has none, and the `fin` flag last.
    fn words(&self, buf: &[u8], i: usize) -> RawWords {
        let w = |o: usize| u32::from_le_bytes(buf[o..o + 4].try_into().unwrap());
        let mut out = [0u32; RAW_WORDS];
        for (k, (off, kind)) in self.read_roots() {
            let (lo, hi) = match kind {
                RootKind::Num => (w(off + i * 4), w(off + i * 4)),
                RootKind::Ival => (w(off + i * 4), w(off + 64 + i * 4)),
                RootKind::Bool => {
                    let (v, known) = bool_masks(buf, off);
                    ((v >> i & 1) as u32, (known >> i & 1) as u32)
                }
            };
            out[2 * k] = lo;
            out[2 * k + 1] = hi;
        }
        out[RAW_WORDS - 1] = self.fin as u32;
        out
    }

    /// The lanes of `of` whose transfer is lane `r`'s: every root it reads
    /// equal (what `words` reads of it).
    #[inline]
    fn same_as(&self, buf: &[u8], r: usize, of: u16) -> u16 {
        let mut m = of;
        for (_, (off, kind)) in self.read_roots() {
            m = match kind {
                RootKind::Num => {
                    let c = col(buf, off);
                    m & eq_mask(&c, c[r])
                }
                RootKind::Ival => {
                    let (lo, hi) = (col(buf, off), col(buf, off + 64));
                    m & eq_mask(&lo, lo[r]) & eq_mask(&hi, hi[r])
                }
                RootKind::Bool => {
                    let (v, k) = bool_masks(buf, off);
                    bool_same(v, k, r, m)
                }
            };
            if m == 1 << r {
                break;
            }
        }
        m
    }

    /// The raw transfer of `words`. An undecided `took` is FATAL: the
    /// record would be an unchecked claim about the remainder.
    fn raw(&self, words: &RawWords, chunk: &Rt2, lane: usize, outcome: usize) -> (RawAxis, RawAxis) {
        let axis = |a: usize| -> RawAxis {
            let base = a * crate::trace::verify::ARC_AXIS_ROOTS;
            let pair = |k: usize| (words[2 * (base + k)] as i32, words[2 * (base + k) + 1] as i32);
            assert!(
                self.roots[base].1 == RootKind::Bool && words[2 * base + 1] == 1,
                "arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}'s `took` is not a decided boolean",
                chunk.shape_hash
            );
            RawAxis { took: words[2 * base] == 1, pre: pair(1), frag: pair(2), ox: pair(3), fin: self.fin.then(|| pair(4)) }
        };
        (axis(0), axis(1))
    }

    /// The record's pair of a raw transfer (`raw`).
    fn decode(raw: &(RawAxis, RawAxis), chunk: &Rt2, lane: usize, outcome: usize) -> crate::search::arc_edges::Pair {
        let axis = |a: usize, r: &RawAxis| {
            decode_axis(r).unwrap_or_else(|e| panic!("arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}: {e} (raw {r:?})", chunk.shape_hash))
        };
        (axis(0, &raw.0), axis(1, &raw.1))
    }
}

/// One whole-pixel coordinate of an emitted row: an output root or a constant.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum PosSrc {
    Root(usize),
    Konst(i16),
}

/// The sources of a row's cell: player x/y and room x/y (`None`: no
/// numeric player position, `NO_CELL`, as `pos_graph::block_cells`).
pub(super) type PosSet = Option<[PosSrc; 4]>;

/// The cells of the lanes `need` of a position set (`start`: the kernel's
/// room), as `pos_graph::block_cells` labels them (`room_offset`).
fn cells_of(set: &PosSet, buf: &[u8], need: u16, start: (i16, i16), out: &mut [u32; 16]) {
    use crate::search::pos_graph::{cell_of, room_offset, NO_CELL};
    let Some(srcs) = set else {
        *out = [NO_CELL; 16];
        return;
    };
    let read = |s: PosSrc| -> [i16; 16] {
        match s {
            PosSrc::Root(base) => col(buf, base).map(|w| (w as i32 >> 16) as i16),
            PosSrc::Konst(v) => [v; 16],
        }
    };
    let (px, py, rx, ry) = (read(srcs[0]), read(srcs[1]), read(srcs[2]), read(srcs[3]));
    let mut room: Option<((i16, i16), (i32, i32))> = None;
    for i in lanes_of(need) {
        let (ox, oy) = match room {
            Some((r, o)) if r == (rx[i], ry[i]) => o,
            _ => {
                let o = room_offset(start, rx[i], ry[i]).unwrap_or_else(|e| panic!("emitted row: {e:#}"));
                room = Some(((rx[i], ry[i]), o));
                o
            }
        };
        out[i] = cell_of(px[i] as i32 + ox, py[i] as i32 + oy).unwrap_or_else(|e| panic!("emitted row: {e}"));
    }
}

/// An outcome's uniform fields in a key space: the packed codes that hit,
/// the `(cell, code)` of those that miss.
#[derive(Default)]
pub(super) struct PartKey {
    pub key: u128,
    pub missing: Vec<(u32, Code)>,
}

impl PartKey {
    /// `t`'s uniform fields in `sk` (`None`: a shape the key space lacks),
    /// the template's signature checked against the key space's.
    pub(super) fn of(t: &super::AccTemplate, sk: Option<&ShapeKeys>) -> PartKey {
        use celeste_engine::exact::NONE;
        let Some(sk) = sk else { return PartKey { key: 0, missing: t.uniform.clone() } };
        assert!(sk.record == t.record, "exact keys: two shapes under the hash {:#x} (a shape-hash collision)", t.shape_hash);
        let mut p = PartKey::default();
        for &(c, code) in &t.uniform {
            let d = sk.field(c as usize).unwrap_or_else(|| panic!("uniform cell {c} is not a value cell of shape {:#x}", t.shape_hash));
            match d.find(code) {
                NONE => p.missing.push((c, code)),
                i => p.key |= d.place(i),
            }
        }
        p
    }
}

/// What a body reads at emission, as indices into its kernel's `Plan`.
pub(super) struct BodyPlan {
    /// Its output shape (`Plan::shapes`).
    shape: u32,
    /// Its key fields (`Plan::reads`), as `Plan::body_reads[reads]`.
    reads: std::ops::Range<u32>,
    arc: u32,
    pos: u32,
    /// Its (output shape, position set) column (`Plan::columns`).
    column: u32,
}

/// A kernel's EMISSION PLAN (module doc): its distinct reads, each body's
/// by index.
pub(super) struct Plan {
    /// The output shapes (`AccTemplate::shape_hash`).
    shapes: Vec<u64>,
    /// Key fields: (output shape, field) - a field's dictionary is its
    /// shape's.
    reads: Vec<(u32, KeyField)>,
    body_reads: Vec<u32>,
    /// Transfer root sets, each with the outcome of its first body (named
    /// by a decode failure).
    arcs: Vec<(ArcSlots, usize)>,
    pos: Vec<PosSet>,
    /// The COLUMNS, (output shape, position set) pairs: level -1's
    /// verdicts and the sink's targets (`unit::Targets`) are per column.
    columns: Vec<(u32, u32)>,
    pub bodies: Vec<BodyPlan>,
}

/// What a body reads at emission, before the plan interns it.
pub(super) struct BodyReads {
    pub outcome: usize,
    pub shape: u64,
    pub key_fields: Vec<KeyField>,
    pub arc: ArcSlots,
    pub pos: PosSet,
}

/// Index of `x` in `v`, appended if new.
fn intern<T: Clone + Eq + std::hash::Hash>(v: &mut Vec<T>, at: &mut HashMap<T, u32>, x: T) -> u32 {
    *at.entry(x.clone()).or_insert_with(|| {
        v.push(x);
        v.len() as u32 - 1
    })
}

impl Plan {
    /// The plan of a kernel's bodies (in body order).
    pub(super) fn of(bodies: &[BodyReads]) -> Plan {
        let mut p = Plan { shapes: Vec::new(), reads: Vec::new(), body_reads: Vec::new(), arcs: Vec::new(), pos: Vec::new(), columns: Vec::new(), bodies: Vec::with_capacity(bodies.len()) };
        let (mut shapes, mut reads, mut arcs, mut pos, mut columns) = (HashMap::new(), HashMap::new(), HashMap::new(), HashMap::new(), HashMap::new());
        let mut arc_sets: Vec<ArcSlots> = Vec::new();
        for b in bodies {
            let shape = intern(&mut p.shapes, &mut shapes, b.shape);
            let first = p.body_reads.len() as u32;
            for f in &b.key_fields {
                let r = intern(&mut p.reads, &mut reads, (shape, *f));
                p.body_reads.push(r);
            }
            let n = arc_sets.len();
            let arc = intern(&mut arc_sets, &mut arcs, b.arc);
            if arc as usize == n {
                p.arcs.push((b.arc, b.outcome));
            }
            let ps = intern(&mut p.pos, &mut pos, b.pos);
            let column = intern(&mut p.columns, &mut columns, (shape, ps));
            p.bodies.push(BodyPlan { shape, reads: first..p.body_reads.len() as u32, arc, pos: ps, column });
        }
        p
    }

    /// Body `bi`'s key fields.
    fn fields(&self, bi: usize) -> impl Iterator<Item = &KeyField> {
        let r = &self.bodies[bi].reads;
        self.body_reads[r.start as usize..r.end as usize].iter().map(|&i| &self.reads[i as usize].1)
    }
}

/// A slice's columns (module doc), one per entry of the kernel's plan: the
/// lanes it is needed for and its value per lane. Per thread, shared by
/// every kernel (sized to the largest plan run so far).
#[derive(Default)]
struct Cols {
    /// Per body: the lanes it emits (after the dup masks, then level -1).
    take: Vec<u16>,
    read_need: Vec<u16>,
    read_miss: Vec<u16>,
    read_placed: Vec<Placed>,
    /// Does a read's `Placed` hold high-word bits (a key past 64 bits)?
    read_high: Vec<bool>,
    arc_need: Vec<u16>,
    arc_ids: Vec<[u32; 16]>,
    pos_need: Vec<u16>,
    cells: Vec<[u32; 16]>,
    col_need: Vec<u16>,
    drop_mask: Vec<u16>,
    drop_from: Vec<[u32; 16]>,
    targets: Vec<crate::storage::unit::Targets>,
}

/// `v` at least `n` long, its first `n` zeroed.
fn zeroed<T: Clone + Default>(v: &mut Vec<T>, n: usize) {
    if v.len() < n {
        v.resize(n, T::default());
    }
    v[..n].fill(T::default());
}

/// `v` at least `n` long.
fn sized<T: Clone + Default>(v: &mut Vec<T>, n: usize) {
    if v.len() < n {
        v.resize(n, T::default());
    }
}

impl Cols {
    fn reset(&mut self, plan: &Plan) {
        zeroed(&mut self.take, plan.bodies.len());
        zeroed(&mut self.read_need, plan.reads.len());
        sized(&mut self.read_miss, plan.reads.len());
        sized(&mut self.read_placed, plan.reads.len());
        sized(&mut self.read_high, plan.reads.len());
        zeroed(&mut self.arc_need, plan.arcs.len());
        sized(&mut self.arc_ids, plan.arcs.len());
        zeroed(&mut self.pos_need, plan.pos.len());
        sized(&mut self.cells, plan.pos.len());
        zeroed(&mut self.col_need, plan.columns.len());
        zeroed(&mut self.drop_mask, plan.columns.len());
        sized(&mut self.drop_from, plan.columns.len());
        if self.targets.len() < plan.columns.len() {
            self.targets.resize(plan.columns.len(), Default::default());
        }
    }
}

thread_local! {
    static COLS: std::cell::RefCell<Cols> = std::cell::RefCell::new(Cols::default());
}

/// What a slice's emission reads besides the kernel's outputs.
pub(super) struct Slice<'a> {
    pub chunk: &'a Rt2,
    pub lanes: &'a [usize],
    pub buf: &'a [u8],
    /// Per body, the lanes it emits (`live & !error`, the dup masks applied).
    pub takes: &'a [u16],
    /// Per outcome, its uniform fields keyed (`PartKey`, by key space
    /// generation).
    pub parts: &'a mut [(u64, PartKey)],
}

impl AsmKernel {
    /// THE EMISSION of one slice (module doc): the cells, level -1, the
    /// keys and transfers, column-wise; then each body's lanes into the
    /// sink. Returns the lanes emitted (past level -1).
    pub(super) fn emit(&self, s: Slice, sink: &mut crate::storage::unit::UnitSink) -> u64 {
        let mut cols = COLS.with(|c| std::mem::take(&mut *c.borrow_mut()));
        let n = self.emit_with(&mut cols, s, sink);
        COLS.with(|c| *c.borrow_mut() = cols);
        n
    }

    fn emit_with(&self, c: &mut Cols, s: Slice, sink: &mut crate::storage::unit::UnitSink) -> u64 {
        let plan = &self.plan;
        let Slice { chunk, lanes, buf, takes, parts } = s;
        c.reset(plan);
        c.take[..takes.len()].copy_from_slice(takes);
        let bodies = || (0..plan.bodies.len()).filter(|&bi| takes[bi] != 0);
        // THE CELLS, per position set, and their pos-graph edges.
        for bi in bodies() {
            c.pos_need[plan.bodies[bi].pos as usize] |= takes[bi];
        }
        let start = (self.room.cache.room_x, self.room.cache.room_y);
        for (k, set) in plan.pos.iter().enumerate() {
            if c.pos_need[k] != 0 {
                cells_of(set, buf, c.pos_need[k], start, &mut c.cells[k]);
                sink.pos_edges(&c.cells[k], c.pos_need[k], lanes);
            }
        }
        // LEVEL -1 first, per column: a dropped cell's row needs no key,
        // transfer or lookup, only its source's note (and its pos-graph
        // edge, above).
        if sink.drops_at_emission() {
            for bi in bodies() {
                c.col_need[plan.bodies[bi].column as usize] |= takes[bi];
            }
            for (k, &(shape, pos)) in plan.columns.iter().enumerate() {
                if c.col_need[k] != 0 {
                    c.drop_mask[k] = sink.minus_one_drops(plan.shapes[shape as usize], &c.cells[pos as usize], c.col_need[k], &mut c.drop_from[k]);
                    sink.dropped_lanes(c.col_need[k] & c.drop_mask[k], &c.drop_from[k], lanes);
                    c.col_need[k] = 0;
                }
            }
            for bi in bodies() {
                c.take[bi] &= !c.drop_mask[plan.bodies[bi].column as usize];
            }
        }
        // `--prune-rect` (HEURISTIC): its cells dropped, no note.
        for bi in bodies() {
            if c.take[bi] != 0 {
                c.take[bi] &= !sink.pruned_lanes(&c.cells[plan.bodies[bi].pos as usize], c.take[bi]);
            }
        }
        // The emitted lanes' TARGETS, per column.
        for (bi, b) in plan.bodies.iter().enumerate() {
            c.col_need[b.column as usize] |= c.take[bi];
        }
        for (k, &(shape, pos)) in plan.columns.iter().enumerate() {
            if c.col_need[k] != 0 {
                sink.targets(plan.shapes[shape as usize], &c.cells[pos as usize], c.col_need[k], lanes, &mut c.targets[k]);
            }
        }
        // THE KEYS' READS, each over the lanes of every body naming it.
        let keys = sink.keys();
        let sks: Vec<Option<&ShapeKeys>> = plan.shapes.iter().map(|&h| keys.shape(h)).collect();
        let mut any = false;
        for (bi, b) in plan.bodies.iter().enumerate() {
            let t = c.take[bi];
            if t == 0 {
                continue;
            }
            any = true;
            for &r in &plan.body_reads[b.reads.start as usize..b.reads.end as usize] {
                c.read_need[r as usize] |= t;
            }
            c.arc_need[b.arc as usize] |= t;
        }
        if !any {
            return 0;
        }
        for (r, &(shape, f)) in plan.reads.iter().enumerate() {
            let need = c.read_need[r];
            if need == 0 {
                continue;
            }
            c.read_miss[r] = match sks[shape as usize] {
                Some(sk) => {
                    let d = sk.field(f.cell as usize).unwrap_or_else(|| panic!("key field cell {} is not a value cell of shape {:#x}", f.cell, sk.record.hash));
                    f.eval(buf, d, need, &mut c.read_placed[r])
                }
                // A shape the key space lacks: every lane new, its partial
                // key empty.
                None => {
                    c.read_placed[r] = Placed::default();
                    need
                }
            };
            c.read_high[r] = c.read_placed[r].high();
        }
        // THE TRANSFERS, per root set: lanes of one transfer one lookup.
        if sink.records_edges() {
            for (k, (set, outcome)) in plan.arcs.iter().enumerate() {
                let mut left = c.arc_need[k];
                while left != 0 {
                    let r = left.trailing_zeros() as usize;
                    let same = set.same_as(buf, r, left);
                    left &= !same;
                    let words = set.words(buf, r);
                    let id = sink.xfer_id_raw(&words, |w| ArcSlots::decode(&set.raw(w, chunk, lanes[r], *outcome), chunk, lanes[r], *outcome));
                    for i in lanes_of(same) {
                        c.arc_ids[k][i] = id;
                    }
                }
            }
        }
        // Each body's emissions: its keys assembled, into the sink.
        let generation = keys.generation();
        let mut emitted = 0u64;
        for (bi, (b, body)) in plan.bodies.iter().zip(&self.bodies).enumerate() {
            let take = c.take[bi];
            if take == 0 {
                continue;
            }
            let template = &self.acc_templates[body.outcome];
            let (shape, sk) = (plan.shapes[b.shape as usize], sks[b.shape as usize]);
            if sk.is_none() {
                sink.note_shape(&template.record);
            }
            if parts[body.outcome].0 != generation {
                parts[body.outcome] = (generation, PartKey::of(template, sk));
            }
            let part = &parts[body.outcome].1;
            let mut packed = Placed::splat(part.key);
            let mut miss = 0u16;
            for &r in &plan.body_reads[b.reads.start as usize..b.reads.end as usize] {
                let p = &c.read_placed[r as usize];
                packed.or(p, 0);
                if c.read_high[r as usize] {
                    packed.or(p, 1);
                }
                miss |= c.read_miss[r as usize];
            }
            miss &= take;
            let mut out_keys = [(0u64, 0u64); 16];
            for i in lanes_of(take) {
                out_keys[i] = if miss >> i & 1 == 0 && part.missing.is_empty() {
                    (packed.0[1][i], packed.0[0][i])
                } else {
                    sink.provisional().intern(self.miss_key(bi, buf, i, sk, shape, packed.get(i), &part.missing))
                };
            }
            emitted += take.count_ones() as u64;
            let targets = &c.targets[b.column as usize];
            let e = crate::storage::unit::BodyEmits { shape, take, keys: &out_keys, targets, xfers: sink.records_edges().then_some(&c.arc_ids[b.arc as usize]), lanes };
            let cols = &self.body_cols[bi];
            if let Err(e) = sink.emit_body(&e, || (*template.union).clone_block(), |row, i| cols.push_row(row, buf, i, out_keys[i], targets.cells[i])) {
                panic!("frame step: {e:#}");
            }
        }
        emitted
    }

    /// Body `bi`'s lane `i`'s exact content where a code is missing: its
    /// fields that hit packed (`partial`), the outcome's and the body's
    /// missing codes.
    #[allow(clippy::too_many_arguments)]
    fn miss_key(&self, bi: usize, buf: &[u8], i: usize, sk: Option<&ShapeKeys>, shape: u64, partial: u128, part_missing: &[(u32, Code)]) -> MissKey {
        use celeste_engine::exact::NONE;
        let mut missing: Vec<(u32, Code)> = part_missing.to_vec();
        for f in self.plan.fields(bi) {
            let c = f.code(buf, i);
            if sk.is_none_or(|sk| sk.field(f.cell as usize).is_none_or(|d| d.find(c) == NONE)) {
                missing.push((f.cell, c));
            }
        }
        missing.sort_unstable_by_key(|m| m.0);
        MissKey { shape, partial: ((partial >> 64) as u64, partial as u64), missing }
    }
}
