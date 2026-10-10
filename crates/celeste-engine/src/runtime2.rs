//! The columnar block, the kernels' input/output: heap structure shared by
//! every lane (the shape premise), values in per-lane columns. Owns the lane
//! primitives and the BOUNDARY: a level's widenings (`widen_to`, the
//! `widening` table on blocks), canonical renumbering and the shape hash. Row keys are the storage's
//! (`exact::KeySpace`): a stored piece carries them.

use std::sync::Arc;

use celeste_core::cart_data::CartData;
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

/// `mix64`'s two multipliers.
const MIX_C1: u64 = 0xbf58_476d_1ce4_e5b9;
const MIX_C2: u64 = 0x94d0_49bb_1331_11eb;

/// The whole pixels of a position coordinate (raw 16.16): the bits the
/// CELL holds (`flr` of the low end), so the key takes the rest only
/// (`exact::Code::pos`).
pub const POS_WHOLE: u32 = 0xffff_0000;

/// One lane's abstract value. `Copy`, 12 bytes + tag.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub enum AV {
    Num(P8),
    /// Closed interval [low, high].
    Ival(P8, P8),
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
pub enum Col {
    U(AV),
    V(Vec<AV>),
    /// An all-Num column stored raw: 4 B/lane, branchless op loops.
    N(Vec<P8>),
    /// An all-interval column stored raw: (low, high) pairs, 8 B/lane.
    I(Vec<(P8, P8)>),
}

impl Col {
    /// The lane's value, independent of the column's representation.
    #[inline]
    pub fn at(&self, lane: usize) -> AV {
        match self {
            Col::U(a) => *a,
            Col::V(v) => v[lane],
            Col::N(v) => AV::Num(v[lane]),
            Col::I(v) => AV::Ival(v[lane].0, v[lane].1),
        }
    }

    /// The one number every lane of a `width`-lane column holds, if any.
    pub fn uniform_num(&self, width: usize) -> Option<P8> {
        let AV::Num(n) = self.at(0) else { return None };
        (0..width).all(|lane| self.at(lane) == AV::Num(n)).then_some(n)
    }
}

/// Shared heap structure; a `Val` cell's contents are `Rt2::cols` at the same
/// index, other kinds are uniform. Closure captures are value snapshots.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Cell2 {
    Val,
    Obj(Vec<(u32, u32)>),
    Arr(Vec<u32>),
    Unk,
    Clo(u32, Box<[Col]>),
    Bi(u32),
}

/// Global and field ids the boundary walk needs, resolved by the caller.
pub struct BoundaryIds {
    pub g_objects: u32,
    pub g_player: u32,
    /// The position column reads `player_spawn` before the player exists.
    pub g_player_spawn: u32,
    /// `room` (`x`/`y`): positions are start-room-relative; wins test `room.x`.
    pub g_room: u32,
    /// `max_djump`: 2 once the orb is taken (`frame::wins_of`).
    pub g_max_djump: u32,
    pub g_timers: Vec<u32>, // frames, seconds, minutes, deaths
    pub f_type: u32,
    pub f_rem: u32,
    pub f_spd: u32,
    pub f_x: u32,
    pub f_y: u32,
    pub f_dash_effect_time: u32,
    /// Fruit-off widening ids (`widen::widen_fruit`).
    pub g_fruit: u32,
    pub f_off: u32,
    pub f_start: u32,
    /// The key and its two `frames`-derived fields, pinned with the timers.
    pub g_key: u32,
    pub f_spr: u32,
    pub f_flip: u32,
    /// The player's held-button trails (`abstraction::HeldPrecision`).
    pub f_p_jump: u32,
    pub f_p_dash: u32,
    /// The fly fruit at a fruit-unknown level (`abstraction::FruitPrecision`).
    pub g_fly_fruit: u32,
    pub f_step: u32,
    pub f_fly: u32,
    /// Moving platforms at a platforms-unknown level (`PlatformsPrecision`).
    pub g_platform: u32,
    pub f_last: u32,
    /// The fall floors at a floors-unknown level (`abstraction::FloorsPrecision`).
    pub g_fall_floor: u32,
    /// The orb room's big chest (`frame::orb_deadline_skip`).
    pub g_big_chest: u32,
    /// The berry's other sources (`frame::berry_lost`): the chest the key
    /// opens, and the fake wall that drops one when broken.
    pub g_chest: u32,
    pub g_fake_wall: u32,
    pub f_state: u32,
    pub f_delay: u32,
    pub f_collideable: u32,
    /// The balloon, whose `timer` a floors-unknown level widens.
    pub g_balloon: u32,
    pub f_timer: u32,
    /// Its phase, stored canonical at every level (`widen::canon_balloon_offset`).
    pub f_offset: u32,
    /// The spring, whose phase a floors-unknown level widens.
    pub g_spring: u32,
    pub f_hide_in: u32,
    pub f_hide_for: u32,
}

pub struct Rt2 {
    pub width: usize,
    pub structure: Vec<Cell2>,
    /// Per-cell value columns, parallel to `structure` (for `Cell2::Val`).
    pub cols: Vec<Col>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
    pub prints: Vec<String>,
    /// Set by `boundary`: the canonical structure hash (the shape key).
    pub shape_hash: u64,
    /// A stored piece's per-lane EXACT keys (its tree's `exact::KeySpace`);
    /// empty elsewhere (a boundary clears them).
    pub row_keys: Vec<(u64, u64)>,
}

pub const NONE: u32 = u32::MAX;

pub fn col_push(col: &mut Col, width: usize, v: AV) {
    match col {
        Col::U(a) if *a == v => {} // still uniform
        Col::U(a) => {
            let a = *a;
            *col = match (a, v) {
                (AV::Num(x), AV::Num(y)) => {
                    let mut vs = vec![x; width];
                    vs.push(y);
                    Col::N(vs)
                }
                (AV::Ival(x0, x1), AV::Ival(y0, y1)) => {
                    let mut vs = vec![(x0, x1); width];
                    vs.push((y0, y1));
                    Col::I(vs)
                }
                _ => {
                    let mut vs = vec![a; width];
                    vs.push(v);
                    Col::V(vs)
                }
            };
        }
        Col::N(vs) => match v {
            AV::Num(n) => vs.push(n),
            other => {
                let mut nv: Vec<AV> = vs.iter().map(|n| AV::Num(*n)).collect();
                nv.push(other);
                *col = Col::V(nv);
            }
        },
        Col::I(vs) => match v {
            AV::Ival(a, b) => vs.push((a, b)),
            other => {
                let mut nv: Vec<AV> = vs.iter().map(|(a, b)| AV::Ival(*a, *b)).collect();
                nv.push(other);
                *col = Col::V(nv);
            }
        },
        Col::V(vs) => vs.push(v),
    }
}

/// V -> N / I when every lane is a number / interval.
pub(crate) fn compress_num_v(vs: Vec<AV>) -> Col {
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

fn compress_num(c: Col) -> Col {
    match c {
        Col::V(vs) => compress_num_v(vs),
        other => other,
    }
}

/// A column whose lanes all agree becomes `Col::U`. Kernel binding requires
/// `Col::U` for uniform inputs, so every producer must apply it.
pub fn collapse_uniform(c: Col) -> Col {
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

impl Rt2 {
    /// An empty block shell.
    pub fn empty(
        width: usize,
        globals_len: usize,
        static_strings: &[&str],
        cart: Arc<CartData>,
        cache: Arc<CollisionCache>,
    ) -> Rt2 {
        Rt2 {
            width,
            structure: Vec::new(),
            cols: Vec::new(),
            globals: vec![NONE; globals_len],
            strings: static_strings.iter().map(|s| s.to_string()).collect(),
            cart,
            cache,
            prints: Vec::new(),
            shape_hash: 0,
            row_keys: Vec::new(),
        }
    }



}


impl Rt2 {
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

    /// The player INSTANCES in `objects` (`g_player` is the type table).
    pub fn player_objects(&self, ids: &BoundaryIds) -> Vec<u32> {
        self.objects_of_type(ids, ids.g_player)
    }

    /// Instances in `objects` whose `type` is global `type_global`'s table.
    pub fn objects_of_type(&self, ids: &BoundaryIds, type_global: u32) -> Vec<u32> {
        let mut found = Vec::new();
        let Some(arr) = self.global_target(ids.g_objects) else {
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
            let Some(type_cell) = self.obj_field_cell(obj, ids.f_type) else {
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

    pub fn mark_walk(&self, ids: &BoundaryIds) -> (Vec<u32>, Vec<u32>) {
        let mut rem_cells = Vec::new();
        let mut det_cells = Vec::new();
        for obj in self.player_objects(ids) {
            rem_cells.extend(self.xy_cells_of(obj, ids.f_rem, ids));
            if let Some(c) = self.obj_field_cell(obj, ids.f_dash_effect_time) {
                det_cells.push(c);
            }
        }
        (rem_cells, det_cells)
    }

    /// The `x`/`y` cells of the table at `obj`'s field `f` (rem, spd).
    fn xy_cells_of(&self, obj: u32, f: u32, ids: &BoundaryIds) -> Vec<u32> {
        let mut out = Vec::new();
        if let Some(ptr_cell) = self.obj_field_cell(obj, f) {
            if let Col::U(AV::Ptr(sub)) = self.cols[ptr_cell as usize] {
                for g in [ids.f_x, ids.f_y] {
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
    pub fn position_object(&self, ids: &BoundaryIds) -> Option<u32> {
        self.player_objects(ids).first().copied().or_else(|| self.objects_of_type(ids, ids.g_player_spawn).first().copied())
    }

    /// The position object's `x` and `y` cells (`position_object`).
    pub fn position_cells(&self, ids: &BoundaryIds) -> Option<(u32, u32)> {
        let obj = self.position_object(ids)?;
        Some((self.obj_field_cell(obj, ids.f_x)?, self.obj_field_cell(obj, ids.f_y)?))
    }

    /// The first player INSTANCE's `x`/`y` cells of its table field `f`
    /// (`rem`, `spd`) - not `player_spawn`'s, whose remainder is real state.
    pub fn player_xy_cells(&self, ids: &BoundaryIds, f: u32) -> Option<(usize, usize)> {
        let obj = *self.player_objects(ids).first()?;
        match self.xy_cells_of(obj, f, ids)[..] {
            [x, y] => Some((x as usize, y as usize)),
            _ => None,
        }
    }

    /// The shape key: a hash of the (canonical) structure and globals.
    pub fn shape_hash_of(&self) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        for cell in &self.structure {
            match cell {
                Cell2::Val => h.write_u8(1),
                Cell2::Obj(fields) => {
                    h.write_u8(2);
                    h.write_usize(fields.len());
                    for (k, t) in fields {
                        h.write_u32(*k);
                        h.write_u32(*t);
                    }
                }
                Cell2::Arr(items) => {
                    h.write_u8(3);
                    h.write_usize(items.len());
                    for t in items {
                        h.write_u32(*t);
                    }
                }
                Cell2::Unk => h.write_u8(4),
                Cell2::Clo(f, caps) => {
                    h.write_u8(5);
                    h.write_u32(*f);
                    h.write_usize(caps.len());
                }
                Cell2::Bi(b) => {
                    h.write_u8(6);
                    h.write_u32(*b);
                }
            }
        }
        for g in self.globals.iter() {
            h.write_u32(*g);
        }
        h.finish()
    }

    /// Renumber every cell into CANONICAL order (BFS discovery from the
    /// globals, children in stored order) and compact, so isomorphic heaps
    /// are identical and row keys block-independent. Public so producers
    /// that claim canonical ids (`trace::bind::structure_of`) are checked.
    pub fn canonicalize_ids(&mut self) {
        // Fields sorted by name, or creation order changes the shape.
        for cell in self.structure.iter_mut() {
            if let Cell2::Obj(fields) = cell {
                fields.sort_by_key(|(k, _)| celeste_names::FIELD_NAMES[*k as usize]);
            }
        }
        // The canonical reachability BFS.
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
        let remap_av = |v: AV, new_id: &[u32]| -> AV {
            match v {
                AV::Ptr(t) => AV::Ptr(new_id[t as usize]),
                other => other,
            }
        };
        let remap_col = |c: &Col, new_id: &[u32]| -> Col {
            match c {
                Col::U(v) => Col::U(remap_av(*v, new_id)),
                Col::V(vs) => Col::V(vs.iter().map(|v| remap_av(*v, new_id)).collect()),
                Col::N(vs) => Col::N(vs.clone()),
                Col::I(vs) => Col::I(vs.clone()),
            }
        };
        let mut new_structure: Vec<Cell2> = Vec::with_capacity(order.len());
        let mut new_cols: Vec<Col> = Vec::with_capacity(order.len());
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


    /// The BOUNDARY: level-0 widenings, canonical renumbering (the GC) and
    /// the shape hash, lanes kept.
    pub fn boundary_canonicalize(&mut self, ids: &BoundaryIds) {
        self.boundary_prepare();
        self.boundary_widen(ids);
        self.boundary_finish();
    }

    /// The block as it is, CANONICAL (ids, shape hash; no widening): what a
    /// key space keys (`exact::KeySpace::row_keys`, `exact::exact_rows`).
    pub fn canonical(&mut self) {
        self.boundary_prepare();
        self.boundary_finish();
    }

    /// Shared boundary head: check every column is at block width.
    fn boundary_prepare(&mut self) {
        let full = |c: &Col| match c {
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

    /// The boundary's widenings: `widen_to` with every level flag off.
    fn boundary_widen(&mut self, ids: &BoundaryIds) {
        self.widen_to(ids, crate::widening::Level::EXACT);
    }

    /// Shared boundary tail: canonical ids and the shape hash. Row keys are
    /// the storage's (`exact::KeySpace`): a boundary clears them.
    fn boundary_finish(&mut self) {
        assert!(self.prints.is_empty(), "prints at a frame boundary: {:?}", self.prints);
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
        let retain_col = |col: &mut Col| match col {
            Col::U(_) => {}
            Col::V(vs) => {
                let nv: Vec<AV> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::V(nv);
            }
            Col::N(vs) => {
                let nv: Vec<P8> = keep.iter().map(|&i| vs[i as usize]).collect();
                *col = Col::N(nv);
            }
            Col::I(vs) => {
                let nv: Vec<(P8, P8)> = keep.iter().map(|&i| vs[i as usize]).collect();
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

    /// A clone that shares the immutable context (cart/cache Arcs).
    pub fn clone_block(&self) -> Rt2 {
        Rt2 {
            width: self.width,
            structure: self.structure.clone(),
            cols: self.cols.clone(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            shape_hash: self.shape_hash,
            row_keys: self.row_keys.clone(),
        }
    }

}

