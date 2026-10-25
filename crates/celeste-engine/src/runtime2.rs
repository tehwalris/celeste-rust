//! The columnar block, the kernels' input/output: heap structure shared by
//! every lane (the shape premise: one `shape::Shape`, interned), values in
//! per-lane columns. Owns the lane primitives and the BOUNDARY: a level's
//! widenings (`widen_to`, the `widening` table on blocks) and the canonical
//! renumbering into the block's shape. Row keys are the storage's
//! (`exact::KeySpace`).

use std::sync::Arc;

use celeste_core::arith::Arithmetic;
use celeste_core::room::Room;
use celeste_core::pico8_num::Pico8Num;
use serde::{Deserialize, Serialize};

use crate::shape::{Cell, Shape};

pub type P8 = Pico8Num;

/// The hash mixer: slot functions and fingerprints only (a state's
/// identity is its exact key, `exact`).
#[inline]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(MIX_C1);
    x = (x ^ (x >> 27)).wrapping_mul(MIX_C2);
    x ^ (x >> 31)
}

const MIX_C1: u64 = 0xbf58_476d_1ce4_e5b9;
const MIX_C2: u64 = 0x94d0_49bb_1331_11eb;

/// The whole pixels of a position coordinate (raw 16.16): the bits the
/// CELL holds (`flr` of the low end), so the key takes the rest only
/// (`exact::Code::of_pos`).
pub const POS_WHOLE: u32 = 0xffff_0000;

/// One lane's abstract value, its numbers of type `N` (the arithmetic's).
/// `Copy`, 12 bytes + tag in 16.16.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub enum AV<N = P8> {
    Num(N),
    Ival(N, N),
    Bool(bool),
    UBool,
    /// An UNKNOWN number (a fully widened field); no kernel reads it.
    UNum,
    Str(u32),
    Nil,
    Ptr(u32),
}

/// A column: one value per lane, or one value for every lane (`U`, which
/// costs nothing per lane).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Col<N = P8> {
    U(AV<N>),
    V(Vec<AV<N>>),
    /// An all-Num column stored raw: 4 B/lane, branchless op loops.
    N(Vec<N>),
    /// An all-interval column stored raw: (low, high) pairs, 8 B/lane.
    I(Vec<(N, N)>),
}

impl<N: Copy + PartialEq> Col<N> {
    #[inline]
    pub fn at(&self, lane: usize) -> AV<N> {
        match self {
            Col::U(a) => *a,
            Col::V(v) => v[lane],
            Col::N(v) => AV::Num(v[lane]),
            Col::I(v) => AV::Ival(v[lane].0, v[lane].1),
        }
    }

    /// The one number every lane of a `width`-lane column holds, if any.
    pub fn uniform_num(&self, width: usize) -> Option<N> {
        let AV::Num(n) = self.at(0) else { return None };
        (0..width).all(|lane| self.at(lane) == AV::Num(n)).then_some(n)
    }
}

/// How a column holds its rows' values, in a block of no rows (a
/// `Template`): one value in every row, or per row a number, an interval,
/// a boolean (known or not), any value.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum ColRepr<N = P8> {
    Uniform(AV<N>),
    Num,
    Ival,
    Bool,
    Other,
}

impl<N: Arithmetic> ColRepr<N> {
    /// A column's representation: uniform, numbers, intervals, or per row
    /// any values (`V`).
    pub fn of(col: &Col<N>) -> ColRepr<N> {
        match col {
            Col::U(v) => ColRepr::Uniform(*v),
            Col::N(_) => ColRepr::Num,
            Col::I(_) => ColRepr::Ival,
            Col::V(_) => ColRepr::Other,
        }
    }

    /// The column of a template's row of value `v`: a number, an interval
    /// or a boolean per row, any other value uniform.
    pub fn of_value(v: AV<N>) -> ColRepr<N> {
        match v {
            AV::Num(_) => ColRepr::Num,
            AV::Ival(..) => ColRepr::Ival,
            AV::Bool(_) | AV::UBool => ColRepr::Bool,
            v => ColRepr::Uniform(v),
        }
    }

    /// As the decode builds a value column from codes (the code merges
    /// `Num(n)` and `Ival(n, n)`): numbers, intervals, or any value.
    pub fn decoded(self) -> ColRepr<N> {
        match self {
            ColRepr::Num | ColRepr::Uniform(AV::Num(_)) => ColRepr::Num,
            ColRepr::Ival | ColRepr::Uniform(AV::Ival(..)) => ColRepr::Ival,
            _ => ColRepr::Other,
        }
    }

    /// The per-row kind the column's rows have, or a uniform value could
    /// (`None`: neither a number, an interval nor a boolean).
    fn kind(self) -> Option<ColRepr<N>> {
        match self {
            ColRepr::Num | ColRepr::Uniform(AV::Num(_)) => Some(ColRepr::Num),
            ColRepr::Ival | ColRepr::Uniform(AV::Ival(..)) => Some(ColRepr::Ival),
            ColRepr::Bool | ColRepr::Uniform(AV::Bool(_)) | ColRepr::Uniform(AV::UBool) => Some(ColRepr::Bool),
            _ => None,
        }
    }

    /// One column for the rows of both: the same uniform value, else their
    /// kind (a number and an interval: an interval); `None` where they
    /// cannot share a typed column.
    pub fn join(self, other: ColRepr<N>) -> Option<ColRepr<N>> {
        if let (ColRepr::Uniform(x), ColRepr::Uniform(y)) = (self, other) {
            if x == y {
                return Some(self);
            }
        }
        match (self.kind()?, other.kind()?) {
            (a, b) if a == b => Some(a),
            (ColRepr::Num, ColRepr::Ival) | (ColRepr::Ival, ColRepr::Num) => Some(ColRepr::Ival),
            _ => None,
        }
    }

    /// An empty column of the representation.
    pub fn empty(self) -> Col<N> {
        match self {
            ColRepr::Uniform(v) => Col::U(v),
            ColRepr::Num => Col::N(Vec::new()),
            ColRepr::Ival => Col::I(Vec::new()),
            ColRepr::Bool | ColRepr::Other => Col::V(Vec::new()),
        }
    }
}

/// A TEMPLATE: a block of no rows - its shape, strings and room, and per
/// cell how its rows hold their values (`ColRepr`; a non-value cell's
/// uniform). A kernel outcome's rows, a reference row's, a stored shape's.
#[derive(Clone)]
pub struct Template<N = P8> {
    pub shape: Arc<Shape>,
    pub strings: Arc<[String]>,
    pub room: &'static Room,
    pub cols: Box<[ColRepr<N>]>,
}

impl<N: Arithmetic> Template<N> {
    /// The template of block `b` (its columns as they are: `ColRepr::of`).
    pub fn of_block(b: &Rt2<N>) -> Template<N> {
        Template { shape: b.shape.clone(), strings: b.strings.clone(), room: b.room, cols: b.cols.iter().map(ColRepr::of).collect() }
    }

    /// The template of one-row block `row`'s rows: by its values
    /// (`ColRepr::of_value`), the static strings.
    pub fn of_row(row: &Rt2<N>) -> Template<N> {
        Template { shape: row.shape.clone(), strings: static_strings(), room: row.room, cols: row.cols.iter().map(|c| ColRepr::of_value(c.at(0))).collect() }
    }

    #[inline]
    pub fn shape_hash(&self) -> u64 {
        self.shape.hash()
    }

    /// As a block of no rows (`ColRepr::empty`).
    pub fn empty(&self) -> Rt2<N> {
        Rt2::new(0, self.shape.clone(), self.cols.iter().map(|c| c.empty()).collect(), self.strings.clone(), self.room)
    }
}

/// The names the engine reads itself, as their ids (`celeste_names`): the
/// anchors' (`shape::Anchors`: the position object, the player, else the
/// player spawn, in `objects` by its `type`; the `room`) and their fields.
/// The game's other names are its caller's.
pub struct Names {
    pub objects: u32,
    pub player: u32,
    pub player_spawn: u32,
    pub room: u32,
    pub type_: u32,
    pub x: u32,
    pub y: u32,
}

/// The engine's `Names`, resolved once.
pub fn names() -> &'static Names {
    static N: std::sync::OnceLock<Names> = std::sync::OnceLock::new();
    N.get_or_init(|| {
        let (g, f) = (celeste_names::global, celeste_names::field);
        Names { objects: g("objects"), player: g("player"), player_spawn: g("player_spawn"), room: g("room"), type_: f("type"), x: f("x"), y: f("y") }
    })
}

pub struct Rt2<N = P8> {
    pub width: usize,
    /// The structure every lane shares (its hash the block's shape key).
    pub shape: Arc<Shape>,
    /// Per-cell value columns, parallel to the shape's cells (a non-value
    /// cell's uniform and unread).
    pub cols: Vec<Col<N>>,
    /// The strings a `Str` value names: `celeste_names::STRINGS`, then the
    /// block's own.
    pub strings: Arc<[String]>,
    /// The search's start room (the kernels read its tiles; a row's cell
    /// is relative to it).
    pub room: &'static Room,
    /// A stored piece's per-lane EXACT keys (its tree's `exact::KeySpace`);
    /// empty elsewhere (a boundary clears them).
    pub row_keys: Vec<(u64, u64)>,
}

pub const NONE: u32 = u32::MAX;

/// V -> N / I when every lane is a number / interval.
pub(crate) fn compress_num_v<N: Copy>(vs: Vec<AV<N>>) -> Col<N> {
    if vs.iter().all(|v| matches!(v, AV::Num(_))) {
        Col::N(
            vs.iter()
                .map(|v| match v {
                    AV::Num(n) => *n,
                    _ => unreachable!(),
                })
                .collect(),
        )
    } else if vs.iter().all(|v| matches!(v, AV::Ival(..))) {
        Col::I(
            vs.iter()
                .map(|v| match v {
                    AV::Ival(a, b) => (*a, *b),
                    _ => unreachable!(),
                })
                .collect(),
        )
    } else {
        Col::V(vs)
    }
}

fn compress_num<N: Copy>(c: Col<N>) -> Col<N> {
    match c {
        Col::V(vs) => compress_num_v(vs),
        other => other,
    }
}

/// A column whose lanes all agree becomes `Col::U`. Kernel binding requires
/// `Col::U` for uniform inputs, so every producer must apply it.
pub fn collapse_uniform<N: Copy + PartialEq>(c: Col<N>) -> Col<N> {
    let all_same = match &c {
        Col::U(_) => return c,
        Col::N(vs) => vs.first().map_or(false, |f| vs.iter().all(|x| x == f)),
        Col::I(vs) => vs.first().map_or(false, |f| vs.iter().all(|x| x == f)),
        Col::V(vs) => vs.first().map_or(false, |f| vs.iter().all(|x| x == f)),
    };
    if !all_same {
        return c;
    }
    match c {
        Col::N(vs) => Col::U(AV::Num(vs[0])),
        Col::I(vs) => Col::U(AV::Ival(vs[0].0, vs[0].1)),
        Col::V(vs) => Col::U(vs[0]),
        Col::U(_) => unreachable!(),
    }
}

/// The static strings (`celeste_names::STRINGS`), shared.
pub fn static_strings() -> Arc<[String]> {
    static S: std::sync::OnceLock<Arc<[String]>> = std::sync::OnceLock::new();
    S.get_or_init(|| celeste_names::STRINGS.iter().map(|s| s.to_string()).collect()).clone()
}

impl<N: Arithmetic> Rt2<N> {
    /// A block of `shape` with `cols`, no row keys.
    pub fn new(width: usize, shape: Arc<Shape>, cols: Vec<Col<N>>, strings: Arc<[String]>, room: &'static Room) -> Rt2<N> {
        debug_assert_eq!(cols.len(), shape.cells().len(), "a block's columns are its shape's cells");
        Rt2 { width, shape, cols, strings, room, row_keys: Vec::new() }
    }

    /// The block of `cells`, `cols` and `globals` in any numbering, made
    /// CANONICAL (`canonicalize_ids`) and its shape interned.
    pub fn canonical_of(width: usize, cells: Vec<Cell>, cols: Vec<Col<N>>, globals: Vec<u32>, strings: Arc<[String]>, room: &'static Room) -> Rt2<N> {
        let (cells, cols, globals) = canonical_ids(&cells, &cols, &globals);
        Rt2::new(width, Shape::intern(cells, globals), cols, strings, room)
    }

    /// The shape's cells.
    #[inline]
    pub fn cells(&self) -> &[Cell] {
        self.shape.cells()
    }

    /// The shape's key, its hash.
    #[inline]
    pub fn shape_hash(&self) -> u64 {
        self.shape.hash()
    }
}

/// CANONICAL ORDER: every cell renumbered by BFS discovery from the globals
/// (children in stored order, an object's fields by name) and the
/// unreachable dropped, every pointer remapped - so isomorphic heaps are
/// identical and row keys block-independent; a value column compressed
/// (`N`/`I`) and collapsed where uniform (`collapse_uniform`), a non-value
/// cell's column `Nil`.
fn canonical_ids<N: Arithmetic>(cells: &[Cell], cols: &[Col<N>], globals: &[u32]) -> (Vec<Cell>, Vec<Col<N>>, Vec<u32>) {
    // Fields sorted by name, or creation order changes the shape (most
    // objects are already).
    let by_name = |f: &(u32, u32)| celeste_names::FIELD_NAMES[f.0 as usize];
    let resorted: Vec<(usize, Vec<(u32, u32)>)> = cells
        .iter()
        .enumerate()
        .filter_map(|(i, c)| match c {
            Cell::Obj(fields) if !fields.is_sorted_by_key(by_name) => {
                let mut v = fields.to_vec();
                v.sort_by_key(by_name);
                Some((i, v))
            }
            _ => None,
        })
        .collect();
    fn pick<'a>(resorted: &'a [(usize, Vec<(u32, u32)>)], i: usize, fields: &'a [(u32, u32)]) -> &'a [(u32, u32)] {
        resorted.iter().find(|r| r.0 == i).map_or(fields, |r| &r.1)
    }
    let fields_of = |i: usize, fields| pick(&resorted, i, fields);
    let mut order: Vec<u32> = Vec::new();
    let mut new_id = vec![u32::MAX; cells.len()];
    let mut queue: std::collections::VecDeque<u32> = Default::default();
    let mut enqueue = |t: u32, new_id: &mut [u32], queue: &mut std::collections::VecDeque<u32>| {
        if new_id[t as usize] == u32::MAX {
            new_id[t as usize] = order.len() as u32;
            order.push(t);
            queue.push_back(t);
        }
    };
    for &cell in globals {
        if cell != NONE {
            enqueue(cell, &mut new_id, &mut queue);
        }
    }
    while let Some(c) = queue.pop_front() {
        match &cells[c as usize] {
            Cell::Val => match &cols[c as usize] {
                Col::U(AV::Ptr(t)) => enqueue(*t, &mut new_id, &mut queue),
                Col::U(_) | Col::N(_) | Col::I(_) => {}
                Col::V(vs) => {
                    for v in vs {
                        if let AV::Ptr(t) = v {
                            enqueue(*t, &mut new_id, &mut queue);
                        }
                    }
                }
            },
            Cell::Obj(fields) => {
                for &(_, t) in fields_of(c as usize, fields) {
                    enqueue(t, &mut new_id, &mut queue);
                }
            }
            Cell::Arr(items) => {
                for &t in items.iter() {
                    enqueue(t, &mut new_id, &mut queue);
                }
            }
            Cell::Clo { caps, .. } => {
                for &t in caps.iter() {
                    enqueue(t, &mut new_id, &mut queue);
                }
            }
            Cell::Unk | Cell::Bi(_) => {}
        }
    }
    // Compact over the live cells, remapping every pointer.
    let remap_av = |v: AV<N>| -> AV<N> {
        match v {
            AV::Ptr(t) => AV::Ptr(new_id[t as usize]),
            other => other,
        }
    };
    let remap_col = |c: &Col<N>| -> Col<N> {
        match c {
            Col::U(v) => Col::U(remap_av(*v)),
            Col::V(vs) => Col::V(vs.iter().map(|v| remap_av(*v)).collect()),
            Col::N(vs) => Col::N(vs.clone()),
            Col::I(vs) => Col::I(vs.clone()),
        }
    };
    let ids = |ts: &[u32]| -> Box<[u32]> { ts.iter().map(|t| new_id[*t as usize]).collect() };
    let mut new_cells: Vec<Cell> = Vec::with_capacity(order.len());
    let mut new_cols: Vec<Col<N>> = Vec::with_capacity(order.len());
    for &old in &order {
        let cell = &cells[old as usize];
        new_cells.push(match cell {
            Cell::Val => Cell::Val,
            Cell::Obj(fields) => Cell::Obj(fields_of(old as usize, fields).iter().map(|&(k, t)| (k, new_id[t as usize])).collect()),
            Cell::Arr(items) => Cell::Arr(ids(items)),
            Cell::Unk => Cell::Unk,
            Cell::Clo { f, caps } => Cell::Clo { f: *f, caps: ids(caps) },
            Cell::Bi(b) => Cell::Bi(*b),
        });
        new_cols.push(match cell {
            Cell::Val => collapse_uniform(compress_num(remap_col(&cols[old as usize]))),
            _ => Col::U(AV::Nil),
        });
    }
    let new_globals = globals.iter().map(|&g| if g == NONE { NONE } else { new_id[g as usize] }).collect();
    (new_cells, new_cols, new_globals)
}

impl<N: Arithmetic> Rt2<N> {
    /// The cell a table-valued global points at, if any.
    pub fn global_target(&self, g: u32) -> Option<u32> {
        let cell = self.shape.globals()[g as usize];
        if cell == NONE {
            return None;
        }
        // Globals hold Val(Ptr(target)) for tables.
        match &self.cells()[cell as usize] {
            Cell::Val => match self.cols[cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t),
                _ => None,
            },
            _ => Some(cell),
        }
    }

    /// The cell holding field `f` of object `obj`, if the object has it.
    pub fn obj_field_cell(&self, obj: u32, f: u32) -> Option<u32> {
        match &self.cells()[obj as usize] {
            Cell::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
            _ => None,
        }
    }

    /// The player INSTANCES in `objects` (the global `player` is the type
    /// table).
    pub fn player_objects(&self) -> Vec<u32> {
        self.objects_of_type(names().player)
    }

    /// Instances in `objects` whose `type` is global `type_global`'s table.
    pub fn objects_of_type(&self, type_global: u32) -> Vec<u32> {
        let mut found = Vec::new();
        let Some(arr) = self.global_target(names().objects) else {
            return found;
        };
        let Cell::Arr(items) = &self.cells()[arr as usize] else {
            return found;
        };
        let type_table = self.global_target(type_global);
        for item in items {
            // Array items are cells holding Ptr(obj).
            let obj = match &self.cells()[*item as usize] {
                Cell::Val => match self.cols[*item as usize] {
                    Col::U(AV::Ptr(o)) => o,
                    _ => continue,
                },
                _ => *item,
            };
            let Some(type_cell) = self.obj_field_cell(obj, names().type_) else {
                continue;
            };
            let matches = match self.cols[type_cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t) == type_table,
                _ => false,
            };
            if matches {
                found.push(obj);
            }
        }
        found
    }

    /// The `x`/`y` cells of the table at `obj`'s field `f` (`rem`, `spd`).
    pub fn xy_cells(&self, obj: u32, f: u32) -> Option<(u32, u32)> {
        let Col::U(AV::Ptr(sub)) = self.cols[self.obj_field_cell(obj, f)? as usize] else { return None };
        Some((self.obj_field_cell(sub, names().x)?, self.obj_field_cell(sub, names().y)?))
    }

    /// Renumber every cell into CANONICAL order (`canonical_ids`), the
    /// block's shape the canonical one's.
    pub fn canonicalize_ids(&mut self) {
        let (cells, cols, globals) = canonical_ids(self.cells(), &self.cols, self.shape.globals());
        self.shape = Shape::intern(cells, globals);
        self.cols = cols;
    }

    /// The block as it is, CANONICAL (ids, shape hash; no widening): what a
    /// key space keys (`exact::KeySpace::row_keys`, `exact::exact_rows`).
    pub fn canonical(&mut self) {
        self.boundary_prepare();
        self.boundary_finish();
    }

    /// `canonical`, by value.
    pub fn canonicalized(mut self) -> Self {
        self.canonical();
        self
    }

    /// Shared boundary head: every column at block width.
    fn boundary_prepare(&mut self) {
        let full = |c: &Col<N>| match c {
            Col::U(_) => true,
            Col::V(v) => v.len() == self.width,
            Col::N(v) => v.len() == self.width,
            Col::I(v) => v.len() == self.width,
        };
        debug_assert!(self.cols.iter().all(full), "boundary: a column is not at block width");
    }

    /// Shared boundary tail: canonical ids (the canonical shape); row keys
    /// cleared.
    fn boundary_finish(&mut self) {
        self.canonicalize_ids();
        self.row_keys.clear();
    }

    /// Keep only the given lanes (ascending) in every column and row key.
    pub fn retain_lanes(&mut self, keep: &[u32]) {
        if keep.len() == self.width && keep.iter().enumerate().all(|(i, &k)| k as usize == i) {
            return;
        }
        self.gather_lanes(keep);
    }

    /// Gather `keep`'s lanes in `keep`'s order (reorder and/or filter).
    pub fn gather_lanes(&mut self, keep: &[u32]) {
        let retain_col = |col: &mut Col<N>| match col {
            Col::U(_) => {}
            Col::V(vs) => {
                let nv: Vec<AV<N>> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::V(nv);
            }
            Col::N(vs) => {
                let nv: Vec<N> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::N(nv);
            }
            Col::I(vs) => {
                let nv: Vec<(N, N)> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::I(nv);
            }
        };
        for col in self.cols.iter_mut() {
            retain_col(col);
        }
        if !self.row_keys.is_empty() {
            self.row_keys = keep.iter().map(|&i| self.row_keys[i as usize]).collect();
        }
        self.width = keep.len();
    }

    /// A clone (its shape, strings and room shared).
    pub fn clone_block(&self) -> Rt2<N> {
        Rt2 { width: self.width, shape: self.shape.clone(), cols: self.cols.clone(), strings: self.strings.clone(), room: self.room, row_keys: self.row_keys.clone() }
    }
}

impl<N: Arithmetic> Rt2<N> {
    /// The BOUNDARY: level-0 widenings and canonical renumbering (the GC)
    /// into the canonical shape, lanes kept.
    pub fn boundary_canonicalize(&mut self) {
        self.boundary_prepare();
        self.widen_to(crate::widening::Level::EXACT);
        self.boundary_finish();
    }
}
