//! The columnar block, the kernels' input/output: heap structure shared by
//! every lane (the shape premise), values in per-lane columns. Owns the lane
//! primitives and the BOUNDARY: a level's widenings (`widen_to`, the
//! `widening` table on blocks), canonical renumbering and the shape hash.
//! Row keys are the storage's (`exact::KeySpace`).

use std::sync::Arc;

use celeste_core::arith::Arithmetic;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::Pico8Num;
use serde::{Deserialize, Serialize};

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
    NilPtr,
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

/// Shared heap structure; a `Val` cell's contents are `Rt2::cols` at the same
/// index, other kinds are uniform. Closure captures are value snapshots.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Cell2<N = P8> {
    Val,
    Obj(Vec<(u32, u32)>),
    Arr(Vec<u32>),
    Unk,
    Clo(u32, Box<[Col<N>]>),
    Bi(u32),
}

/// The names the engine reads itself, as their ids (`celeste_names`): the
/// POSITION OBJECT (the player, else the player spawn, in `objects` by its
/// `type`) and its fields. The game's other names are its caller's.
pub struct Names {
    pub objects: u32,
    pub player: u32,
    pub player_spawn: u32,
    pub type_: u32,
    pub x: u32,
    pub y: u32,
}

/// The engine's `Names`, resolved once.
pub fn names() -> &'static Names {
    static N: std::sync::OnceLock<Names> = std::sync::OnceLock::new();
    N.get_or_init(|| {
        let g = |n: &str| celeste_names::global_id(n).unwrap_or_else(|| panic!("no global {n}"));
        let f = |n: &str| celeste_names::field_id(n).unwrap_or_else(|| panic!("no field {n}"));
        Names { objects: g("objects"), player: g("player"), player_spawn: g("player_spawn"), type_: f("type"), x: f("x"), y: f("y") }
    })
}

pub struct Rt2<N = P8> {
    pub width: usize,
    pub structure: Vec<Cell2<N>>,
    /// Per-cell value columns, parallel to `structure`.
    pub cols: Vec<Col<N>>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub cache: Arc<CollisionCache>,
    /// Set by `boundary`: the canonical structure hash (the shape key).
    pub shape_hash: u64,
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

/// Each name table's names as a shape's signature spells them: a name's
/// FNV-1a hash, distinct within its table (checked once), so a signature
/// is as exact as the names themselves and independent of where a name
/// sits in its table.
struct NameCodes {
    fields: Vec<u32>,
    globals: Vec<u32>,
    fns: Vec<u32>,
    builtins: Vec<u32>,
}

fn name_codes() -> &'static NameCodes {
    static C: std::sync::OnceLock<NameCodes> = std::sync::OnceLock::new();
    C.get_or_init(|| {
        let codes = |what: &str, names: &[&str]| -> Vec<u32> {
            let fnv = |n: &str| n.bytes().fold(0x811c_9dc5u32, |h, b| (h ^ b as u32).wrapping_mul(0x0100_0193));
            let out: Vec<u32> = names.iter().map(|n| fnv(n)).collect();
            let distinct: std::collections::HashSet<u32> = out.iter().copied().collect();
            assert!(distinct.len() == out.len(), "two {what} names share a signature code");
            out
        };
        NameCodes {
            fields: codes("field", celeste_names::FIELD_NAMES),
            globals: codes("global", celeste_names::GLOBAL_NAMES),
            fns: codes("function", celeste_names::gen::FN_NAMES),
            builtins: codes("builtin", &celeste_core::builtins::BUILTIN_NAMES),
        }
    })
}

impl<N: Arithmetic> Rt2<N> {
    pub fn empty(
        width: usize,
        globals_len: usize,
        static_strings: &[&str],
        cache: Arc<CollisionCache>,
    ) -> Rt2<N> {
        Rt2 {
            width,
            structure: Vec::new(),
            cols: Vec::new(),
            globals: vec![NONE; globals_len],
            strings: static_strings.iter().map(|s| s.to_string()).collect(),
            cache,
            shape_hash: 0,
            row_keys: Vec::new(),
        }
    }



}


impl<N: Arithmetic> Rt2<N> {
    /// The cell a table-valued global points at, if any.
    pub fn global_target(&self, g: u32) -> Option<u32> {
        let cell = self.globals[g as usize];
        if cell == NONE {
            return None;
        }
        // Globals hold Val(Ptr(target)) for tables.
        match &self.structure[cell as usize] {
            Cell2::Val => match self.cols[cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t),
                _ => None,
            },
            _ => Some(cell),
        }
    }

    /// The cell holding field `f` of object `obj`, if the object has it.
    pub fn obj_field_cell(&self, obj: u32, f: u32) -> Option<u32> {
        match &self.structure[obj as usize] {
            Cell2::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
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
        let Cell2::Arr(items) = &self.structure[arr as usize] else {
            return found;
        };
        let type_table = self.global_target(type_global);
        for item in items {
            // Array items are cells holding Ptr(obj).
            let obj = match &self.structure[*item as usize] {
                Cell2::Val => match self.cols[*item as usize] {
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

    /// The `x`/`y` cells of the table at `obj`'s field `f`.
    fn xy_cells_of(&self, obj: u32, f: u32) -> Vec<u32> {
        let mut out = Vec::new();
        if let Some(ptr_cell) = self.obj_field_cell(obj, f) {
            if let Col::U(AV::Ptr(sub)) = self.cols[ptr_cell as usize] {
                for g in [names().x, names().y] {
                    if let Some(c) = self.obj_field_cell(sub, g) {
                        out.push(c);
                    }
                }
            }
        }
        out
    }

    /// The POSITION object: the first player instance, else the first
    /// `player_spawn` (`search::pos_graph::player_object`'s rule: its
    /// whole-pixel `x`/`y` make the row's cell).
    pub fn position_object(&self) -> Option<u32> {
        self.player_objects().first().copied().or_else(|| self.objects_of_type(names().player_spawn).first().copied())
    }

    pub fn position_cells(&self) -> Option<(u32, u32)> {
        let obj = self.position_object()?;
        Some((self.obj_field_cell(obj, names().x)?, self.obj_field_cell(obj, names().y)?))
    }

    /// The first player INSTANCE's `x`/`y` cells of its table field `f`
    /// (`rem`, `spd`) - not `player_spawn`'s, whose remainder is real state.
    pub fn player_xy_cells(&self, f: u32) -> Option<(usize, usize)> {
        let obj = *self.player_objects().first()?;
        match self.xy_cells_of(obj, f)[..] {
            [x, y] => Some((x as usize, y as usize)),
            _ => None,
        }
    }

    /// THE SHAPE'S SIGNATURE, word by word (`exact::shape_sig` collects it,
    /// `shape_hash_of` hashes it): every cell in canonical order by kind, a
    /// field, function or builtin by its NAME's code (`NameCodes`); then the
    /// globals PRESENT, each by its name's code and its cell. A shape's
    /// identity is the names it holds, so appending a name to a table moves
    /// no shape (an absent global is not in it).
    pub fn shape_words(&self, mut w: impl FnMut(u32)) {
        let c = name_codes();
        for cell in &self.structure {
            match cell {
                Cell2::Val => w(1),
                Cell2::Obj(fields) => {
                    w(2);
                    w(fields.len() as u32);
                    for (k, t) in fields {
                        w(c.fields[*k as usize]);
                        w(*t);
                    }
                }
                Cell2::Arr(items) => {
                    w(3);
                    w(items.len() as u32);
                    items.iter().for_each(|t| w(*t));
                }
                Cell2::Unk => w(4),
                Cell2::Clo(f, caps) => {
                    w(5);
                    w(c.fns[*f as usize]);
                    w(caps.len() as u32);
                }
                Cell2::Bi(b) => {
                    w(6);
                    w(c.builtins[*b as usize]);
                }
            }
        }
        w(u32::MAX);
        for (g, &cell) in self.globals.iter().enumerate() {
            if cell != NONE {
                w(c.globals[g]);
                w(cell);
            }
        }
    }

    /// The shape key: a hash of the signature (`shape_words`).
    pub fn shape_hash_of(&self) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        self.shape_words(|x| h.write_u32(x));
        h.finish()
    }

    /// Renumber every cell into CANONICAL order (BFS discovery from the globals,
    /// children in stored order) and compact, so isomorphic heaps are identical
    /// and row keys block-independent.
    pub fn canonicalize_ids(&mut self) {
        // Fields sorted by name, or creation order changes the shape.
        for cell in self.structure.iter_mut() {
            if let Cell2::Obj(fields) = cell {
                fields.sort_by_key(|(k, _)| celeste_names::FIELD_NAMES[*k as usize]);
            }
        }
        let mut order: Vec<u32> = Vec::new();
        let mut new_id = vec![u32::MAX; self.structure.len()];
        {
            let mut queue: std::collections::VecDeque<u32> = Default::default();
            fn enqueue(
                t: u32,
                new_id: &mut [u32],
                queue: &mut std::collections::VecDeque<u32>,
                order: &mut Vec<u32>,
            ) {
                if new_id[t as usize] == u32::MAX {
                    new_id[t as usize] = order.len() as u32;
                    order.push(t);
                    queue.push_back(t);
                }
            }
            for &cell in self.globals.iter() {
                if cell != NONE {
                    enqueue(cell, &mut new_id, &mut queue, &mut order);
                }
            }
            while let Some(c) = queue.pop_front() {
                match &self.structure[c as usize] {
                    Cell2::Val => match &self.cols[c as usize] {
                        Col::U(AV::Ptr(t)) => enqueue(*t, &mut new_id, &mut queue, &mut order),
                        Col::U(_) | Col::N(_) | Col::I(_) => {}
                        Col::V(vs) => {
                            for v in vs {
                                if let AV::Ptr(t) = v {
                                    enqueue(*t, &mut new_id, &mut queue, &mut order);
                                }
                            }
                        }
                    },
                    Cell2::Obj(fields) => {
                        for (_, t) in fields {
                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                        }
                    }
                    Cell2::Arr(items) => {
                        for t in items {
                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                        }
                    }
                    Cell2::Clo(_, caps) => {
                        for cap in caps.iter() {
                            match cap {
                                Col::U(AV::Ptr(t)) => {
                                    enqueue(*t, &mut new_id, &mut queue, &mut order)
                                }
                                Col::V(vs) => {
                                    for v in vs {
                                        if let AV::Ptr(t) = v {
                                            enqueue(*t, &mut new_id, &mut queue, &mut order);
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    Cell2::Unk | Cell2::Bi(_) => {}
                }
            }
        }

        // Compact over the live cells, remapping every pointer.
        let remap_av = |v: AV<N>, new_id: &[u32]| -> AV<N> {
            match v {
                AV::Ptr(t) => AV::Ptr(new_id[t as usize]),
                other => other,
            }
        };
        let remap_col = |c: &Col<N>, new_id: &[u32]| -> Col<N> {
            match c {
                Col::U(v) => Col::U(remap_av(*v, new_id)),
                Col::V(vs) => Col::V(vs.iter().map(|v| remap_av(*v, new_id)).collect()),
                Col::N(vs) => Col::N(vs.clone()),
                Col::I(vs) => Col::I(vs.clone()),
            }
        };
        let mut new_structure: Vec<Cell2<N>> = Vec::with_capacity(order.len());
        let mut new_cols: Vec<Col<N>> = Vec::with_capacity(order.len());
        for &old in &order {
            let cell = match &self.structure[old as usize] {
                Cell2::Val => Cell2::Val,
                Cell2::Obj(fields) => Cell2::Obj(
                    fields.iter().map(|(k, t)| (*k, new_id[*t as usize])).collect(),
                ),
                Cell2::Arr(items) => {
                    Cell2::Arr(items.iter().map(|t| new_id[*t as usize]).collect())
                }
                Cell2::Unk => Cell2::Unk,
                Cell2::Clo(f, caps) => Cell2::Clo(
                    *f,
                    caps.iter().map(|c| remap_col(c, &new_id)).collect(),
                ),
                Cell2::Bi(b) => Cell2::Bi(*b),
            };
            new_structure.push(cell);
            new_cols.push(match &self.structure[old as usize] {
                Cell2::Val => {
                    collapse_uniform(compress_num(remap_col(&self.cols[old as usize], &new_id)))
                }
                _ => Col::U(AV::Nil),
            });
        }
        for g in self.globals.iter_mut() {
            if *g != NONE {
                *g = new_id[*g as usize];
            }
        }
        self.structure = new_structure;
        self.cols = new_cols;
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
        debug_assert!(
            self.structure.iter().all(|c| match c {
                Cell2::Clo(_, caps) => caps.iter().all(full),
                _ => true,
            }),
            "boundary: a closure capture is not at block width"
        );
    }

    /// Shared boundary tail: canonical ids and the shape hash; row keys
    /// cleared.
    fn boundary_finish(&mut self) {
        self.canonicalize_ids();
        self.shape_hash = self.shape_hash_of();
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
        for cell in self.structure.iter_mut() {
            if let Cell2::Clo(_, caps) = cell {
                for cap in caps.iter_mut() {
                    retain_col(cap);
                }
            }
        }
        if !self.row_keys.is_empty() {
            self.row_keys = keep.iter().map(|&i| self.row_keys[i as usize]).collect();
        }
        self.width = keep.len();
    }

    /// A clone that shares the immutable context (the collision cache's `Arc`).
    pub fn clone_block(&self) -> Rt2<N> {
        Rt2 {
            width: self.width,
            structure: self.structure.clone(),
            cols: self.cols.clone(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cache: self.cache.clone(),
            shape_hash: self.shape_hash,
            row_keys: self.row_keys.clone(),
        }
    }

}

impl<N: Arithmetic> Rt2<N> {
    /// The BOUNDARY: level-0 widenings, canonical renumbering (the GC) and
    /// the shape hash, lanes kept.
    pub fn boundary_canonicalize(&mut self) {
        self.boundary_prepare();
        self.widen_to(crate::widening::Level::EXACT);
        self.boundary_finish();
    }
}
