//! The columnar block, the kernels' input/output: heap structure shared by
//! every lane (the shape premise), values in per-lane columns. Owns the lane
//! primitives and the BOUNDARY: a level's widenings (`widen_to`, twins of
//! `trace::widen`), canonical renumbering, shape hash, row keys and dedup.

use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::Pico8Num;
use serde::{Deserialize, Serialize};

pub type P8 = Pico8Num;

/// Row-key primitive. Shared with the ASM kernels (`compiled::asm_kernel`),
/// whose keys must equal `boundary`'s exactly.
#[inline]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(MIX_C1);
    x = (x ^ (x >> 27)).wrapping_mul(MIX_C2);
    x ^ (x >> 31)
}

/// A value's contribution to the row key. A number and `[n, n]` code ALIKE:
/// producers disagree on column types, and one state must have one key.
#[inline]
pub fn av_code(v: AV) -> u64 {
    match v {
        AV::Num(n) => num_code(n.to_bits()),
        AV::Ival(a, b) => ival_code(a.to_bits(), b.to_bits()),
        AV::Bool(b) => 3u64 << 56 | b as u64,
        AV::UBool => 4u64 << 56,
        AV::UNum => 9u64 << 56,
        AV::Str(x) => 5u64 << 56 | x as u64,
        AV::Nil => 6u64 << 56,
        AV::Ptr(p) => 7u64 << 56 | p as u64,
        AV::NilPtr => 8u64 << 56,
    }
}

/// `av_code` of the number with raw bits `n`.
#[inline]
pub fn num_code(n: u32) -> u64 {
    1u64 << 56 | n as u64
}

/// `av_code` of the interval `[lo, hi]` (raw); a point codes as its number.
#[inline]
pub fn ival_code(lo: u32, hi: u32) -> u64 {
    if lo == hi {
        num_code(lo)
    } else {
        2u64 << 56 | (lo as u64) << 24 ^ mix64((hi as u64) << 1)
    }
}

/// Row-key seeds (one per half) and cell-id multiplier; shared with `Op::CellMix`.
pub const KEY_SEED1: u64 = 0x5bf0_3635;
pub const KEY_SEED2: u64 = 0x27d4_eb2f;
pub const CELL_K: u64 = 0x9e37_79b9_7f4a_7c15;
/// `mix64`'s two multipliers.
pub const MIX_C1: u64 = 0xbf58_476d_1ce4_e5b9;
pub const MIX_C2: u64 = 0x94d0_49bb_1331_11eb;

#[inline]
pub fn cell_mix(c: u64, v: AV, seed: u64) -> u64 {
    mix64(seed ^ c.wrapping_mul(CELL_K) ^ av_code(v))
}

/// The whole pixels of a position coordinate (raw 16.16): the bits the
/// CELL holds (`flr` of the low end), so the row key takes the rest only.
pub const POS_WHOLE: u32 = 0xffff_0000;

/// What a POSITION coordinate (the position object's `x`/`y`) contributes
/// to the row key: the value less its low end's whole pixels. The cell
/// (`search::pos_graph`) holds those, and the room (its offset) is in the
/// key, so `(shape, key, cell)` still names one state: the key is the
/// state WITHOUT its position (plans/storage-v2.md). An integer position,
/// the normal case, codes as 0. Not a number: fatal (the cell would be
/// `NO_CELL` and the position lost).
#[inline]
pub fn pos_code(v: AV) -> u64 {
    match v {
        AV::Num(n) => num_code(n.to_bits() & !POS_WHOLE),
        AV::Ival(a, b) => pos_ival_code(a.to_bits(), b.to_bits()),
        other => panic!("a position coordinate holds {other:?}, not a number: its cell and key would lose it"),
    }
}

/// `pos_code` of the interval `[lo, hi]` (raw).
#[inline]
pub fn pos_ival_code(lo: u32, hi: u32) -> u64 {
    let whole = lo & POS_WHOLE;
    ival_code(lo.wrapping_sub(whole), hi.wrapping_sub(whole))
}

/// `cell_mix` of a position coordinate (`pos_code`).
#[inline]
pub fn pos_mix(c: u64, v: AV, seed: u64) -> u64 {
    mix64(seed ^ c.wrapping_mul(CELL_K) ^ pos_code(v))
}

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

/// A full `sin` period as an inclusive raw interval width (`rnd(1)`'s range);
/// the balloon's canonical phase is `[0, BALLOON_PERIOD_RAW]`. ONE
/// definition, shared with `widen::canon_balloon_offset`, or lookups miss.
pub const BALLOON_PERIOD_RAW: i32 = 0xffff;

/// A fall floor's widened `state` (0 idle, 1 shaking, 2 hidden), an interval
/// so a lane may hold it or an exact state. Raw. ONE definition, shared with
/// `widen::widen_near_floors`, or lookups miss.
pub const FLOOR_STATE_RANGE: (i32, i32) = (0, 2 << 16);

/// A spring's widened `spr` (0 hidden, 18 ready, 19 compressed); `widen::phase_paths`.
pub const SPRING_SPR_RANGE: (i32, i32) = (0, 19 << 16);

/// A balloon's widened `spr` (0 popped, 22 present); `widen::phase_paths`.
pub const BALLOON_SPR_RANGE: (i32, i32) = (0, 22 << 16);

/// A balloon's bob radius (`y = start + sin(offset) * 2`); `widen::phase_paths`.
pub const BALLOON_BOB_RAW: i32 = 2 << 16;

/// Hitboxes `(x, y, w, h)`, checked by the tracer (`widen::widen_near_floors`).
pub const PLAYER_HITBOX: [i16; 4] = [1, 3, 6, 5];
pub const FLOOR_HITBOX: [i16; 4] = [0, 0, 8, 8];

/// The cart's `floor.collide(player, 0, 0)` solved for the player: OPEN
/// windows `(lo, hi)` for `x` and `y`. ONE definition, shared with
/// `widen::widen_near_floors`, or lookups miss.
pub fn floor_player_window(floor: (P8, P8)) -> [(P8, P8); 2] {
    let [px, py, pw, ph] = PLAYER_HITBOX;
    let [fx, fy, fw, fh] = FLOOR_HITBOX;
    let at = |base: P8, d: i16| base + P8::from_i16(d);
    [(at(floor.0, fx - px - pw), at(floor.0, fx + fw - px)), (at(floor.1, fy - py - ph), at(floor.1, fy + fh - py))]
}

/// Does a player in ranges `x`, `y` CERTAINLY overlap? A straddling range
/// does not (the floor stays widened, the safe side). Shared with the tracer.
pub fn player_overlaps_floor(window: [(P8, P8); 2], x: (P8, P8), y: (P8, P8)) -> bool {
    let [(xlo, xhi), (ylo, yhi)] = window;
    x.0 > xlo && x.1 < xhi && y.0 > ylo && y.1 < yhi
}


/// A moving platform's `x` (and `last`) range: it wraps between -16 and 128.
/// ONE definition, shared with `widen::widen_platforms`.
pub const PLATFORM_PATH: (i16, i16) = (-16, 128);

/// A platform's widened `rem.x`: the whole `[-0.5, 0.5)`, raw.
pub const PLATFORM_REM: (i32, i32) = (-0x8000, 0x7fff);

/// The fly fruit's widened `spd.y` (raw, inclusive), checked by
/// `widen::widen_fly_fruit`.
pub const FLY_FRUIT_SPD_Y: (i32, i32) = (-0x3_8000, 0x8000);
/// The fly fruit's `rem.y` range, [-0.5, 0.5): `move`'s own arithmetic.
pub const FLY_FRUIT_REM_Y: (i32, i32) = (-0x8000, 0x7fff);

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
    /// Set by `boundary`: per-lane 128-bit canonical row keys.
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
        self.widen_to(ids, false, false, false, false);
    }

    /// A level's widenings, as its kernels bake them (`trace::widen`), so a
    /// finer row can be looked up as the level's node. Each asserts it only grows.
    pub fn widen_to(&mut self, ids: &BoundaryIds, held: bool, fruit: bool, floors_near: bool, platforms: bool) {
        let (rem_cells, det_cells) = self.mark_walk(ids);

        // 1. The player's rem: the full [-0.5, 0.5) interval.
        let half = P8::from_parts(0, 0x8000);
        let neg_half = -half;
        let half_below = half.next_smallest();
        let wide = AV::Ival(neg_half, half_below);
        for c in rem_cells {
            let widen = |v: AV| -> AV {
                match v {
                    AV::Num(n) => {
                        assert!(n >= neg_half && n <= half_below, "player_rem value {:?} not in expected interval", n);
                        wide
                    }
                    AV::Ival(a, b) => {
                        assert!(a >= neg_half && b <= half_below, "player_rem interval [{:?}, {:?}] not in expected interval", a, b);
                        wide
                    }
                    other => panic!("Unexpected value type for player_rem: {:?}", other),
                }
            };
            self.cols[c as usize] = match &self.cols[c as usize] {
                Col::U(v) => Col::U(widen(*v)),
                Col::V(vs) => Col::V(vs.iter().map(|v| widen(*v)).collect()),
                Col::N(vs) => Col::V(vs.iter().map(|n| widen(AV::Num(*n))).collect()),
                Col::I(vs) => Col::V(vs.iter().map(|(a, b)| widen(AV::Ival(*a, *b))).collect()),
            };
        }

        // 3. dash_effect_time clamped at 0 from below.
        let zero = P8::from_i16(0);
        for c in det_cells {
            let clamp = |v: AV| match v {
                AV::Num(n) => AV::Num(if n < zero { zero } else { n }),
                other => panic!("player dash_effect_time is not a number: {:?}", other),
            };
            self.cols[c as usize] = match &self.cols[c as usize] {
                Col::U(v) => Col::U(clamp(*v)),
                Col::V(vs) => Col::V(vs.iter().map(|v| clamp(*v)).collect()),
                Col::N(vs) => Col::N(
                    vs.iter().map(|n| if *n < zero { zero } else { *n }).collect(),
                ),
                Col::I(_) => panic!("player dash_effect_time is not a number"),
            };
        }

        // 3b. Fruit: `off` [0, 39] and `y` start +/- 2.5, TOGETHER, as the
        // kernels write them.
        for obj in self.objects_of_type(ids, ids.g_fruit) {
            let field = |rt: &Self, name: &str, f: u32| {
                rt.obj_field_cell(obj, f).unwrap_or_else(|| {
                    panic!("fruit-off widening: fruit has no `{}` field", name)
                })
            };
            let off_cell = field(self, "off", ids.f_off);
            let y_cell = field(self, "y", ids.f_y);
            let start_cell = field(self, "start", ids.f_start);
            let numeric = |v: &AV| matches!(v, AV::Num(_) | AV::Ival(..));
            match &self.cols[off_cell as usize] {
                Col::U(v) if numeric(v) => {}
                Col::V(vs) if vs.iter().all(numeric) => {}
                Col::N(_) | Col::I(_) => {}
                other => panic!("fruit-off widening: off is not numeric: {:?}", other),
            }
            self.cols[off_cell as usize] =
                Col::U(AV::Ival(P8::from_i16(0), P8::from_i16(39)));

            // sin(off/40) * 2.5, for an unknown phase: the whole bob band.
            let amplitude = P8::from_parts(2, 0x8000);
            let band_at = |lane: usize| -> (P8, P8) {
                match self.cols[start_cell as usize].at(lane) {
                    AV::Num(s) => (s - amplitude, s + amplitude),
                    other => {
                        panic!("fruit-off widening: start is not a number: {:?}", other)
                    }
                }
            };
            for lane in 0..self.width {
                let (lo, hi) = band_at(lane);
                let contained = match self.cols[y_cell as usize].at(lane) {
                    AV::Num(n) => lo <= n && n <= hi,
                    AV::Ival(a, b) => lo <= a && b <= hi,
                    other => panic!("fruit-off widening: y is not numeric: {:?}", other),
                };
                assert!(
                    contained,
                    "fruit-off widening: lane {} of y {:?} is outside the bob band [{:?}, {:?}]",
                    lane, self.cols[y_cell as usize].at(lane), lo, hi
                );
            }
            let new_y = match &self.cols[start_cell as usize] {
                Col::U(_) => {
                    let (lo, hi) = band_at(0);
                    Col::U(AV::Ival(lo, hi))
                }
                _ => Col::I((0..self.width).map(band_at).collect()),
            };
            self.cols[y_cell as usize] = new_y;
        }

        // 4. The timer globals pinned to 0.
        for &g in &ids.g_timers {
            let cell = self.globals[g as usize];
            assert!(cell != NONE, "timer global missing - pin would silently not apply");
            match &self.cols[cell as usize] {
                Col::U(AV::Num(_)) | Col::V(_) | Col::N(_) => {
                    self.cols[cell as usize] = Col::U(AV::Num(zero));
                }
                other => panic!("timer global is not a number: {:?}", other),
            }
        }

        // 5. The key's `frames`-derived `spr`/`flip.x`, pinned WITH the timers
        // (`widen::widen_timers`), or exact rows have no widened counterpart.
        for obj in self.objects_of_type(ids, ids.g_key) {
            let spr = self.obj_field_cell(obj, ids.f_spr).unwrap_or_else(|| panic!("key pin: key has no `spr` field"));
            self.cols[spr as usize] = Col::U(AV::Num(P8::from_i16(8)));
            let flip = self.obj_field_cell(obj, ids.f_flip).unwrap_or_else(|| panic!("key pin: key has no `flip` field"));
            let Col::U(AV::Ptr(sub)) = self.cols[flip as usize] else {
                panic!("key pin: key `flip` is not a table: {:?}", self.cols[flip as usize])
            };
            let fx = self.obj_field_cell(sub, ids.f_x).unwrap_or_else(|| panic!("key pin: key `flip` has no `x`"));
            self.cols[fx as usize] = Col::U(AV::Bool(false));
        }

        // 6. The held-button trails at a held-unknown level: unknown.
        if held {
            for obj in self.player_objects(ids) {
                for f in [ids.f_p_jump, ids.f_p_dash] {
                    let c = self.obj_field_cell(obj, f).unwrap_or_else(|| panic!("held widening: the player has no p_jump / p_dash field"));
                    self.cols[c as usize] = Col::U(AV::UBool);
                }
            }
        }

        // 7. The fly fruit at a fruit-unknown level (`widen::widen_fly_fruit`).
        let check = |rt: &Self, c: u32, name: &str, ok: &dyn Fn(AV) -> bool| {
            for lane in 0..rt.width {
                let v = rt.cols[c as usize].at(lane);
                assert!(ok(v), "object widening: lane {lane} of `{name}` is {v:?}, which the widening does not contain");
            }
        };
        if fruit {
            for obj in self.objects_of_type(ids, ids.g_fly_fruit) {
                let field = |rt: &Self, name: &str, f: u32| {
                    rt.obj_field_cell(obj, f).unwrap_or_else(|| panic!("fly fruit widening: the fly fruit has no `{name}` field"))
                };
                for (name, f) in [("step", ids.f_step), ("y", ids.f_y)] {
                    let c = field(self, name, f);
                    check(self, c, name, &|v| matches!(v, AV::Num(_) | AV::Ival(..) | AV::UNum));
                    self.cols[c as usize] = Col::U(AV::UNum);
                }
                let fly = field(self, "fly", ids.f_fly);
                check(self, fly, "fly", &|v| matches!(v, AV::Bool(_) | AV::UBool));
                self.cols[fly as usize] = Col::U(AV::UBool);
                for (name, f, (lo, hi)) in [("spd.y", ids.f_spd, FLY_FRUIT_SPD_Y), ("rem.y", ids.f_rem, FLY_FRUIT_REM_Y)] {
                    let cells = self.xy_cells_of(obj, f, ids);
                    let [_, c] = cells[..] else { panic!("fly fruit widening: the fly fruit has no `{name}`") };
                    let (lo, hi) = (P8::from_raw(lo), P8::from_raw(hi));
                    check(self, c, name, &|v| match v {
                        AV::Num(n) => lo <= n && n <= hi,
                        AV::Ival(a, b) => lo <= a && b <= hi,
                        _ => false,
                    });
                    self.cols[c as usize] = Col::U(AV::Ival(lo, hi));
                }
            }
        }

        // An object's `(x, y)`, the same in every lane (floors never move).
        let at = |rt: &Self, obj: u32, what: &str| -> (P8, P8) {
            let get = |f: u32| {
                let col = rt.obj_field_cell(obj, f).map(|c| &rt.cols[c as usize]);
                match col.and_then(|c| c.uniform_num(rt.width)) {
                    Some(n) => n,
                    None => panic!("fall floor widening: a {what}'s position is {col:?}, not one number"),
                }
            };
            (get(ids.f_x), get(ids.f_y))
        };
        // 8. Spring and balloon phases at a near level (`widen::phase_paths`).
        // A spring that never bounced has no `delay`.
        if floors_near {
            let phases = self
                .objects_of_type(ids, ids.g_spring)
                .into_iter()
                .flat_map(|o| [(o, "spr", ids.f_spr, Some(SPRING_SPR_RANGE)), (o, "delay", ids.f_delay, None), (o, "hide_in", ids.f_hide_in, None), (o, "hide_for", ids.f_hide_for, None)])
                .chain(self.objects_of_type(ids, ids.g_balloon).into_iter().flat_map(|o| {
                    // Its `y`: the bob band around its constant `start`.
                    let sc = self.obj_field_cell(o, ids.f_start).unwrap_or_else(|| panic!("phase widening: the balloon has no `start` field"));
                    let col = &self.cols[sc as usize];
                    let start = col.uniform_num(self.width).unwrap_or_else(|| panic!("phase widening: the balloon's `start` is not one number: {:?}", col));
                    let start = start.to_bits() as i32;
                    [(o, "spr", ids.f_spr, Some(BALLOON_SPR_RANGE)), (o, "y", ids.f_y, Some((start - BALLOON_BOB_RAW, start + BALLOON_BOB_RAW)))]
                }))
                .collect::<Vec<_>>();
            {
                for (obj, name, f, range) in phases {
                    let Some(c) = self.obj_field_cell(obj, f) else {
                        assert!(f == ids.f_delay, "phase widening: no `{name}` field");
                        continue;
                    };
                    // Phases become their ranges, countdowns (`None`) unknown.
                    if let Some((lo, hi)) = range {
                        let (lo, hi) = (P8::from_raw(lo), P8::from_raw(hi));
                        check(self, c, name, &|v| match v {
                            AV::Num(n) => lo <= n && n <= hi,
                            AV::Ival(a, b) => lo <= a && b <= hi,
                            _ => false,
                        });
                        self.cols[c as usize] = Col::U(AV::Ival(lo, hi));
                    } else {
                        check(self, c, name, &|v| matches!(v, AV::Num(_) | AV::Ival(..) | AV::UNum));
                        self.cols[c as usize] = Col::U(AV::UNum);
                    }
                }
            }
        }

        // 8a. Floor `delay` and balloon `timer` unknown at a near level.
        if floors_near {
            for (ty, f, name) in [(ids.g_fall_floor, ids.f_delay, "delay"), (ids.g_balloon, ids.f_timer, "timer")] {
                for obj in self.objects_of_type(ids, ty) {
                    let Some(c) = self.obj_field_cell(obj, f) else { continue };
                    check(self, c, name, &|v| matches!(v, AV::Num(_) | AV::Ival(..) | AV::UNum));
                    self.cols[c as usize] = Col::U(AV::UNum);
                }
            }
        }

        // 8b. Fall floors at a near level (`widen::widen_near_floors`): `state`
        // and `collideable` widened except where a player overlaps the floor;
        // `state` is an interval column, as the kernels store it.
        if floors_near {
            let (slo, shi) = (P8::from_raw(FLOOR_STATE_RANGE.0), P8::from_raw(FLOOR_STATE_RANGE.1));
            let span = |v: AV, what: &str| match v {
                AV::Num(n) => (n, n),
                AV::Ival(a, b) => (a, b),
                other => panic!("near floor widening: {what} is {other:?}"),
            };
            let players: Vec<(u32, u32)> = self
                .player_objects(ids)
                .into_iter()
                .map(|o| {
                    let cell = |f: u32, what: &str| self.obj_field_cell(o, f).unwrap_or_else(|| panic!("near floor widening: the player has no `{what}`"));
                    (cell(ids.f_x, "x"), cell(ids.f_y, "y"))
                })
                .collect();
            for obj in self.objects_of_type(ids, ids.g_fall_floor) {
                let floor = at(self, obj, "fall floor");
                let window = floor_player_window(floor);
                let overlap: Vec<bool> = (0..self.width)
                    .map(|lane| {
                        players.iter().any(|&(cx, cy)| {
                            player_overlaps_floor(window, span(self.cols[cx as usize].at(lane), "the player's x"), span(self.cols[cy as usize].at(lane), "the player's y"))
                        })
                    })
                    .collect();
                let field = |f: u32, name: &str| self.obj_field_cell(obj, f).unwrap_or_else(|| panic!("near floor widening: the fall floor has no `{name}` field"));
                let (cs, cc) = (field(ids.f_state, "state"), field(ids.f_collideable, "collideable"));
                let state: Vec<(P8, P8)> = (0..self.width)
                    .map(|lane| {
                        let (a, b) = span(self.cols[cs as usize].at(lane), "a fall floor's `state`");
                        if overlap[lane] {
                            (a, b)
                        } else {
                            assert!(slo <= a && b <= shi, "near floor widening: lane {lane} of `state` is [{a:?}, {b:?}], which the widening does not contain");
                            (slo, shi)
                        }
                    })
                    .collect();
                let coll: Vec<AV> = (0..self.width)
                    .map(|lane| {
                        let v = self.cols[cc as usize].at(lane);
                        assert!(matches!(v, AV::Bool(_) | AV::UBool), "near floor widening: lane {lane} of `collideable` is {v:?}");
                        if overlap[lane] {
                            v
                        } else {
                            AV::UBool
                        }
                    })
                    .collect();
                self.cols[cs as usize] = collapse_uniform(Col::I(state));
                self.cols[cc as usize] = collapse_uniform(Col::V(coll));
            }
        }

        // 8c. Platforms (`widen::widen_platforms`): the kernels read `last` as
        // `x` (equal at every frame end), so a row where they differ asserts.
        if platforms {
            let (plo, phi) = (P8::from_i16(PLATFORM_PATH.0), P8::from_i16(PLATFORM_PATH.1));
            let (rlo, rhi) = (P8::from_raw(PLATFORM_REM.0), P8::from_raw(PLATFORM_REM.1));
            for obj in self.objects_of_type(ids, ids.g_platform) {
                let x = self.obj_field_cell(obj, ids.f_x).unwrap_or_else(|| panic!("platform widening: a platform has no `x`"));
                let last = self.obj_field_cell(obj, ids.f_last).unwrap_or_else(|| panic!("platform widening: a platform has no `last`"));
                assert!(
                    (0..self.width).all(|lane| self.cols[x as usize].at(lane) == self.cols[last as usize].at(lane)),
                    "platform widening: a row with `last != x`"
                );
                for (name, c) in [("x", x), ("last", last)] {
                    check(self, c, name, &|v| match v {
                        AV::Num(n) => plo <= n && n <= phi,
                        AV::Ival(a, b) => plo <= a && b <= phi,
                        _ => false,
                    });
                    self.cols[c as usize] = Col::U(AV::Ival(plo, phi));
                }
                let cells = self.xy_cells_of(obj, ids.f_rem, ids);
                let [c, _] = cells[..] else { panic!("platform widening: a platform has no `rem`") };
                check(self, c, "rem.x", &|v| match v {
                    AV::Num(n) => rlo <= n && n <= rhi,
                    AV::Ival(a, b) => rlo <= a && b <= rhi,
                    _ => false,
                });
                self.cols[c as usize] = Col::U(AV::Ival(rlo, rhi));
            }
        }

        // 9. At EVERY level, a full-period balloon phase becomes the canonical
        // [0, 1) (exact: `sin` is [-1, 1] either way); a narrower one asserts.
        for obj in self.objects_of_type(ids, ids.g_balloon) {
            let Some(c) = self.obj_field_cell(obj, ids.f_offset) else { continue };
            if (0..self.width).all(|lane| matches!(self.cols[c as usize].at(lane), AV::Num(_))) {
                continue;
            }
            let full = |v: AV| matches!(v, AV::Ival(lo, hi) if hi.as_raw_u32() as i32 - lo.as_raw_u32() as i32 >= BALLOON_PERIOD_RAW);
            check(self, c, "offset", &full);
            self.cols[c as usize] = Col::U(AV::Ival(P8::from_raw(0), P8::from_raw(BALLOON_PERIOD_RAW)));
        }
    }

    /// Shared boundary tail: canonical ids and the shape hash. Row keys are
    /// the storage's (`exact::KeySpace`): a boundary clears them.
    fn boundary_finish(&mut self) {
        assert!(self.prints.is_empty(), "prints at a frame boundary: {:?}", self.prints);
        self.canonicalize_ids();
        self.shape_hash = self.shape_hash_of();
        self.row_keys.clear();
    }

    /// TEMPORARY (the exact-keys bijection check): the develop hash keys.
    pub fn legacy_hash_keys(&mut self, ids: &BoundaryIds) -> Vec<(u64, u64)> {
        self.boundary_prepare();
        self.canonicalize_ids();
        let shape_hash = self.shape_hash_of();
        self.shape_hash = shape_hash;
        // 128-bit row key: per-cell mixes SUMMED, so uniform cells fold once
        // and keys agree whichever cells are uniform. The kernels match this.
        // The position's whole pixels are the cell's, not the key's (`pos_code`).
        let w = self.width;
        let pos = self.position_cells(ids);
        let mut part1: u64 = shape_hash;
        let mut part2: u64 = 0xa076_1d64_78bd_642f ^ shape_hash;
        let mut h1: Vec<u64> = vec![0; w];
        let mut h2: Vec<u64> = vec![0; w];
        for (c, cell) in self.structure.iter().enumerate() {
            if !matches!(cell, Cell2::Val) {
                continue;
            }
            let ci = c as u64;
            if pos.is_some_and(|(x, y)| c as u32 == x || c as u32 == y) {
                match &self.cols[c] {
                    Col::U(v) => {
                        part1 = part1.wrapping_add(pos_mix(ci, *v, KEY_SEED1));
                        part2 = part2.wrapping_add(pos_mix(ci, *v, KEY_SEED2));
                    }
                    col => {
                        for i in 0..w {
                            let v = col.at(i);
                            h1[i] = h1[i].wrapping_add(pos_mix(ci, v, KEY_SEED1));
                            h2[i] = h2[i].wrapping_add(pos_mix(ci, v, KEY_SEED2));
                        }
                    }
                }
                continue;
            }
            match &self.cols[c] {
                Col::U(v) => {
                    part1 = part1.wrapping_add(cell_mix(ci, *v, KEY_SEED1));
                    part2 = part2.wrapping_add(cell_mix(ci, *v, KEY_SEED2));
                }
                Col::V(vs) => {
                    for i in 0..w {
                        h1[i] = h1[i].wrapping_add(cell_mix(ci, vs[i], KEY_SEED1));
                        h2[i] = h2[i].wrapping_add(cell_mix(ci, vs[i], KEY_SEED2));
                    }
                }
                Col::N(vs) => {
                    // `cell_mix` with the per-cell constants hoisted.
                    let c1 = KEY_SEED1 ^ ci.wrapping_mul(CELL_K);
                    let c2 = KEY_SEED2 ^ ci.wrapping_mul(CELL_K);
                    for i in 0..w {
                        let code = num_code(vs[i].to_bits());
                        h1[i] = h1[i].wrapping_add(mix64(c1 ^ code));
                        h2[i] = h2[i].wrapping_add(mix64(c2 ^ code));
                    }
                }
                Col::I(vs) => {
                    for i in 0..w {
                        let v = AV::Ival(vs[i].0, vs[i].1);
                        h1[i] = h1[i].wrapping_add(cell_mix(ci, v, KEY_SEED1));
                        h2[i] = h2[i].wrapping_add(cell_mix(ci, v, KEY_SEED2));
                    }
                }
            }
        }
        (0..w)
            .map(|i| (
                mix64(part1.wrapping_add(h1[i])),
                mix64(part2.wrapping_add(h2[i])),
            ))
            .collect()
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A number and its point interval are one value to the row key.
    #[test]
    fn a_point_interval_keys_as_its_number() {
        for raw in [0i32, 1, -1, 64 << 16, -(5 << 15), i32::MAX, i32::MIN] {
            let n = P8::from_raw(raw);
            assert_eq!(av_code(AV::Ival(n, n)), av_code(AV::Num(n)), "raw {raw:#x}");
            assert_eq!(cell_mix(7, AV::Ival(n, n), KEY_SEED1), cell_mix(7, AV::Num(n), KEY_SEED1));
            assert_eq!(ival_code(n.to_bits(), n.to_bits()), num_code(n.to_bits()));
        }
        // A wider interval is not its low end, nor its high end.
        let (a, b) = (P8::from_raw(0), P8::from_raw(1));
        assert_ne!(av_code(AV::Ival(a, b)), av_code(AV::Num(a)));
        assert_ne!(av_code(AV::Ival(a, b)), av_code(AV::Num(b)));
        assert_ne!(av_code(AV::Ival(a, b)), av_code(AV::Ival(b, b)));
    }

    /// A position coordinate keys without its low end's whole pixels: every
    /// integer alike, an interval by its offsets from its low pixel, and a
    /// fraction is still the key's (the cell holds only `flr`).
    #[test]
    fn a_position_keys_without_its_whole_pixels() {
        let px = |raw: i32| P8::from_raw(raw);
        for whole in [-64i32, -1, 0, 5, 300] {
            assert_eq!(pos_code(AV::Num(px(whole << 16))), pos_code(AV::Num(px(0))), "x = {whole}");
            assert_eq!(pos_code(AV::Num(px((whole << 16) + 0x4000))), pos_code(AV::Num(px(0x4000))), "x = {whole}.25");
            let iv = AV::Ival(px((whole << 16) + 0x8000), px(((whole + 2) << 16) + 0x4000));
            assert_eq!(pos_code(iv), pos_code(AV::Ival(px(0x8000), px((2 << 16) + 0x4000))), "[{whole}.5, {}.25]", whole + 2);
            // A point interval keys as its number, as `av_code` has it.
            assert_eq!(pos_code(AV::Ival(px(whole << 16), px(whole << 16))), pos_code(AV::Num(px(0))));
        }
        assert_ne!(pos_code(AV::Num(px(0x4000))), pos_code(AV::Num(px(0))), "a fraction is the key's");
        assert_ne!(pos_code(AV::Ival(px(0), px(1 << 16))), pos_code(AV::Ival(px(0), px(2 << 16))), "an interval's width is the key's");
        assert_ne!(pos_code(AV::Num(px(0))), av_code(AV::Num(px(1 << 16))), "the code is not the value's");
    }

    /// `floor_player_window` is the cart's `floor.collide(player, 0, 0)` at
    /// every whole-pixel position around a floor.
    #[test]
    fn the_overlap_window_is_the_carts_collide() {
        let (fx, fy) = (48i16, 112i16);
        let window = floor_player_window((P8::from_i16(fx), P8::from_i16(fy)));
        let [px_, py_, pw, ph] = PLAYER_HITBOX;
        let [fx_, fy_, fw, fh] = FLOOR_HITBOX;
        for x in fx - 20..fx + 20 {
            for y in fy - 20..fy + 20 {
                let collide = x + px_ + pw > fx + fx_ && y + py_ + ph > fy + fy_ && x + px_ < fx + fx_ + fw && y + py_ < fy + fy_ + fh;
                let (px, py) = (P8::from_i16(x), P8::from_i16(y));
                assert_eq!(player_overlaps_floor(window, (px, px), (py, py)), collide, "player at ({x}, {y})");
            }
        }
        // A bucket straddling the window's edge is no overlap.
        let (a, b) = (P8::from_i16(fx - 7), P8::from_i16(fx - 6));
        assert!(!player_overlaps_floor(window, (a, b), (P8::from_i16(fy), P8::from_i16(fy))));
    }
}

