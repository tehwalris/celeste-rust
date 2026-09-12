//! The columnar block (plans/columnar-engine.md): the search's data
//! currency and the ASM kernels' input/output.
//!
//! A BLOCK of abstract lanes: heap structure is shared (uniform across
//! lanes by the shape premise), values are per-lane columns. The kernels
//! append output lanes to accumulator blocks; this module owns the block's
//! partition/retain/merge primitives and the BOUNDARY - the Bits(0)
//! widenings ported from abstraction.rs (rem widening, dash_effect_time
//! clamp, fruit off/y), canonical renumbering, the row keys and the
//! within-block dedup.

use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::Pico8Num;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

pub type P8 = Pico8Num;

/// Row-key primitives. Shared with the ASM kernels' per-chunk pre-dedup
/// (`compiled::asm_kernel`), which must collapse exactly what `boundary`'s
/// keys collapse - so they mix the same way rather than approximating it.
#[inline]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(MIX_C1);
    x = (x ^ (x >> 27)).wrapping_mul(MIX_C2);
    x ^ (x >> 31)
}

#[inline]
pub fn av_code(v: AV) -> u64 {
    match v {
        AV::Num(n) => 1u64 << 56 | n.to_bits() as u64,
        AV::Ival(a, b) => 2u64 << 56 | (a.to_bits() as u64) << 24 ^ mix64((b.to_bits() as u64) << 1),
        AV::Bool(b) => 3u64 << 56 | b as u64,
        AV::UBool => 4u64 << 56,
        AV::Str(x) => 5u64 << 56 | x as u64,
        AV::Nil => 6u64 << 56,
        AV::Ptr(p) => 7u64 << 56 | p as u64,
        AV::NilPtr => 8u64 << 56,
    }
}

/// The two seeds of the 128-bit row key (one per half) and the cell-id
/// multiplier. Shared with the ASM kernels, which compute the key in the
/// graph (`transpile::graph::Op::CellMix`).
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

/// One lane's abstract value. `Copy`, 12 bytes + tag.
/// Mirrors `interpreter::value::Value` scalar variants plus intervals.
#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize)]
pub enum AV {
    Num(P8),
    /// Closed interval [low, high] (`Value::NumberInterval`).
    Ival(P8, P8),
    Bool(bool),
    UBool,
    Str(u32),
    Nil,
    Ptr(u32),
    NilPtr,
}

/// A column: one value per lane, or one value for EVERY lane. Uniform is
/// the load-bearing case - appending lanes to it is free, and ops on two
/// uniforms cost one scalar op regardless of width.
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
    /// The lane's value regardless of how the column is stored - the one
    /// way to compare two columns by CONTENT rather than representation
    /// (uniform vs a materialized all-equal vector are the same value).
    #[inline]
    pub fn at(&self, lane: usize) -> AV {
        match self {
            Col::U(a) => *a,
            Col::V(v) => v[lane],
            Col::N(v) => AV::Num(v[lane]),
            Col::I(v) => AV::Ival(v[lane].0, v[lane].1),
        }
    }
}

/// Shared heap structure. Value cells' per-lane contents live in
/// `Rt2::cols` at the same index; the other kinds are uniform by the
/// shape premise. Closure captures are SNAPSHOTS of the capture columns
/// (the interpreter copies capture VALUES into the closure).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Cell2 {
    Val,
    Obj(Vec<(u32, u32)>),
    Arr(Vec<u32>),
    Unk,
    Clo(u32, Box<[Col]>),
    Bi(u32),
}

/// Global ids the boundary walk needs, resolved from gen.rs by the driver
/// so this module does not depend on the generated code.
pub struct BoundaryIds {
    pub g_objects: u32,
    pub g_player: u32,
    /// The spawn-animation type table: the search's position column reads
    /// the `player` instance, or the `player_spawn` one before it exists.
    pub g_player_spawn: u32,
    /// The `room` table (`x`/`y` fields via `f_x`/`f_y`): the position
    /// column is start-room-relative, and the win test is `room.x`.
    pub g_room: u32,
    pub g_timers: Vec<u32>, // frames, seconds, minutes, deaths
    pub f_type: u32,
    pub f_rem: u32,
    pub f_spd: u32,
    pub f_x: u32,
    pub f_y: u32,
    pub f_dash_effect_time: u32,
    /// Fruit-off widening ids (abstraction.rs:720): the `fruit` type
    /// global plus the `off`/`start` fields. `f_y` above doubles as the
    /// bob-band target.
    pub g_fruit: u32,
    pub f_off: u32,
    pub f_start: u32,
    /// The recipe's partition_merge (pm1) key: globals (has_dashed,
    /// freeze) and player fields (dash_time, djump, p_dash, p_jump).
    pub g_pm1: Vec<u32>,
    pub f_pm1: Vec<u32>,
}

pub struct Rt2 {
    pub width: usize,
    pub structure: Vec<Cell2>,
    /// Per-cell value columns, parallel to `structure` (meaningful for
    /// `Cell2::Val`).
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

/// Push one value onto a column of `width` lanes, materializing a uniform
/// column only when the value differs from it, and keeping the raw
/// `N`/`I` forms while the kinds allow.
fn col_push(col: &mut Col, width: usize, v: AV) {
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

/// V -> N / I when every lane is a plain number / interval.
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

/// A column whose lanes all agree IS a uniform column; store it as one.
///
/// This is representation-only - `boundary`'s row key folds uniform cells
/// into the block partial precisely so that keys do not depend on which
/// cells happen to be uniform - but it is load-bearing twice over. The
/// interpreter keeps such cells as `MaybeVector::Scalar`, so without this
/// an imported block and an engine-produced block of the SAME content
/// have different column kinds, and every kernel `bind` (which requires
/// `Col::U` for its block-uniform inputs) refuses engine output: at f35
/// the engine's own next frame ran 269,059 lanes with zero kernel
/// coverage purely for this reason.
fn collapse_uniform(c: Col) -> Col {
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
    /// An empty block shell for the vectorized importer (import.rs).
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
    /// The cell a table-valued global points at (`None` if unset or not a
    /// table pointer).
    pub fn global_target(&self, g: u32) -> Option<u32> {
        let cell = self.globals[g as usize];
        if cell == NONE {
            return None;
        }
        // Globals hold Val(Ptr(target)) for tables (inspect.rs convention).
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

    /// The player objects' relevant cells (mark_heap, abstraction.rs:134).
    /// The player INSTANCES in `objects`: array entries whose `type`
    /// field points at the `player` type table. (`g_player` itself is
    /// that table, not an instance.)
    pub fn player_objects(&self, ids: &BoundaryIds) -> Vec<u32> {
        self.objects_of_type(ids, ids.g_player)
    }

    /// Instances in `objects` whose `type` field points at the type
    /// table held by global `type_global` (find_objects_by_type,
    /// interpreter state_helper).
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
            if let Some(rem_ptr_cell) = self.obj_field_cell(obj, ids.f_rem) {
                if let Col::U(AV::Ptr(rem_obj)) = self.cols[rem_ptr_cell as usize] {
                    for f in [ids.f_x, ids.f_y] {
                        if let Some(c) = self.obj_field_cell(rem_obj, f) {
                            rem_cells.push(c);
                        }
                    }
                }
            }
            if let Some(c) = self.obj_field_cell(obj, ids.f_dash_effect_time) {
                det_cells.push(c);
            }
        }
        (rem_cells, det_cells)
    }

    /// Boundary abstraction + canonical row dedup + compaction. Returns
    /// the surviving lane count (= next frame's width).
    ///
    /// Ports, in order (make_state_abstract, abstraction.rs:286):
    ///   1. rem widening at Bits(0): full closed interval [-0.5, 0.5-eps]
    ///      (abstraction.rs:521; other precisions are follow-up work).
    ///   2. spd: Exact (the default) - nothing.
    ///   3. dash_effect_time clamp at 0 from below (abstraction.rs:713).
    ///   3b. fruit off/y widening (abstraction.rs:720): off := [0, 39],
    ///       y := start +/- 2.5, together, growth asserted per lane. A
    ///       no-op on fruit-free shapes (rooms without a live fruit).
    ///   4. timer globals pinned to 0 (abstraction.rs:837).
    /// Then: reachability BFS from globals over the shared structure (the
    /// per-frame GC + canonical cell order), per-lane 128-bit row hash
    /// over live value cells, dedup, compact columns to survivors.
    /// The block's shape key: a hash of the (canonical) structure and
    /// globals. Meaningful for comparison only when the structure is in
    /// canonical order (post-boundary, or a BFS import). It is what
    /// scopes a kernel to the shape it was generated for.
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

    pub fn boundary(&mut self, ids: &BoundaryIds) -> usize {
        self.boundary_canonicalize(ids);
        self.boundary_dedup()
    }

    /// Renumber every cell into CANONICAL order, and compact.
    ///
    /// Breadth-first from the globals in global-index order, children in
    /// stored order - the discovery order IS the numbering. Two blocks
    /// with isomorphic heaps therefore compact to identical structures,
    /// which is what makes them concatenable and the row hash
    /// block-independent, so cross-block dedup is exact.
    ///
    /// Separate from `boundary_canonicalize` so that a producer which
    /// claims to emit canonical ids already can be CHECKED against the
    /// one implementation of the rule, rather than against a second copy
    /// of it (`trace::bind::structure_of` is such a producer).
    pub fn canonicalize_ids(&mut self) {
        // Canonical field order. Objects created by the compiled program
        // carry fields in STORE order; the importer sorts them by name.
        // Sort here so the canonical BFS (child discovery order) and the
        // shape hash see ONE order regardless of an object's lineage -
        // without this an engine-spawned player and an imported one hash
        // to different shapes and every downstream cell id diverges
        // (found by gate 2 on the f020 -> f025 spawn transition).
        for cell in self.structure.iter_mut() {
            if let Cell2::Obj(fields) = cell {
                fields.sort_by_key(|(k, _)| celeste_names::FIELD_NAMES[*k as usize]);
            }
        }
        // Canonical reachability BFS from globals in GLOBAL-INDEX order,
        // fields/items in stored order - the discovery order IS the
        // canonical cell numbering, so two blocks with isomorphic heaps
        // compact to IDENTICAL structures and can be concatenated, and the
        // row hash is block-independent (cross-block dedup is exact).
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

        // Compact: rebuild structure and columns over the live cells in
        // canonical order, remapping every pointer (structural and
        // per-lane value pointers).
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

    /// The boundary WITHOUT the final within-block dedup: abstraction,
    /// canonical BFS compaction, and the per-lane row keys for ALL `width`
    /// lanes, in lane order. Split out for the D1 key gate
    /// (plans/dedup-roofline-plan.md): the gate pairs each lane's engine
    /// row key with the interpreter's `visited_row_keys` for the SAME
    /// lane, which requires the keys of lanes the dedup would drop.
    pub fn boundary_canonicalize(&mut self, ids: &BoundaryIds) {
        self.boundary_prepare();
        self.boundary_widen(ids);
        self.boundary_finish();
    }

    /// The boundary WITHOUT the widenings: materialize, canonicalize,
    /// hash, dedup - and nothing else. For the rung-agnostic kernel path
    /// (plans/kernel-ladder.md): kernels generated with `widen = false`
    /// hand the campaign EXACT rows, and the campaign applies the
    /// precision rung's own abstraction downstream. Widening here would
    /// pre-empt it with Bits(0)'s, which is exactly the refusal
    /// `compiled_forward` used to make.
    ///
    /// Dedup on exact rows is sound at every rung: two byte-identical
    /// rows are the same state under any widening.
    pub fn boundary_exact(&mut self) -> usize {
        self.boundary_prepare();
        self.boundary_finish();
        self.boundary_dedup()
    }

    /// Per-lane canonical row keys, NO widening and NO dedup: materialize,
    /// canonicalize, hash - then hand back `row_keys` verbatim, lane i of the
    /// input as key i of the output.
    ///
    /// This is the ONE row key of the search (the kernel/engine key), used to
    /// RECOMPUTE the keys of an already-abstracted state - the backward
    /// sweep's index build and the band filter. The state handed in is
    /// already at its final rung abstraction (the campaign widened it before
    /// the forward's boundary hashed it), so the widening is baked into the
    /// content and re-applying `boundary_widen` here would be idempotent at
    /// Bits(0) and WRONG at any finer rung. Skipping it makes this key a pure
    /// function of the state's content, so it reproduces whatever the
    /// forward's `boundary` / `boundary_exact` stored, at every level.
    pub fn row_keys_canonical(&mut self) -> Vec<(u64, u64)> {
        self.boundary_prepare();
        self.boundary_finish();
        self.row_keys.clone()
    }

    /// Shared boundary head: materialize every stale column (the
    /// boundary walks whole columns - BFS pointer scan, hashing,
    /// compaction) and clear the widen history.
    fn boundary_prepare(&mut self) {
        // Every producer keeps its columns physically at the block width
        // (the kernels push one lane at a time; retain/slice/merge preserve
        // it). The boundary walks whole columns, so check rather than
        // assume.
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

    /// The Bits(0) boundary widenings (see `boundary`'s doc for the list
    /// and the abstraction.rs line references).
    fn boundary_widen(&mut self, ids: &BoundaryIds) {
        self.widen_to(ids, Some(0));
    }

    /// The boundary widenings at a rem precision - `rem_bits` = `Some(k)` for
    /// Bits(k), `None` for Exact - on this block's columns, per lane. This
    /// is `make_state_abstract_rem` + `apply_conservative_widenings`
    /// (abstraction.rs) on a block: it is what the ladder's filter applies to
    /// a finer level's rows to look them up in the coarser level's marks.
    ///
    ///   1. rem: Bits(0) -> the full [-0.5, 0.5) interval; Bits(k) -> the
    ///      floor-aligned bucket of width 2^-k containing the value (an
    ///      interval spans its endpoints' buckets); Exact -> untouched.
    ///   3. dash_effect_time clamped at 0 from below.
    ///   3b. fruit: at a non-exact level, off := [0, 39] and y := its bob
    ///       band, together; at Exact, off := off mod 40 (the pin).
    ///   4. timer globals pinned to 0.
    pub fn widen_to(&mut self, ids: &BoundaryIds, rem_bits: Option<u8>) {
        let (rem_cells, det_cells) = self.mark_walk(ids);

        // 1. rem widening.
        let half = P8::from_parts(0, 0x8000);
        let neg_half = -half;
        let half_below = half.next_smallest();
        let wide = AV::Ival(neg_half, half_below);
        // The floor-aligned bucket of width 2^-bits containing `n`
        // (abstraction.rs `rem_bucket`).
        let bucket = |n: P8, bits: u8| -> (P8, P8) {
            let width: i32 = 0x1_0000 >> bits;
            let low = n.to_bits().cast_signed().div_euclid(width) * width;
            (P8::from_raw(low), P8::from_raw(low + width - 1))
        };
        if let Some(bits) = rem_bits {
            for c in rem_cells {
                let widen = |v: AV| -> AV {
                    match v {
                        AV::Num(n) => {
                            assert!(
                                n >= neg_half && n <= half_below,
                                "player_rem value {:?} not in expected interval",
                                n
                            );
                            if bits == 0 {
                                wide
                            } else {
                                let (lo, hi) = bucket(n, bits);
                                AV::Ival(lo, hi)
                            }
                        }
                        AV::Ival(a, b) => {
                            assert!(
                                a >= neg_half && b <= half_below,
                                "player_rem interval [{:?}, {:?}] not in expected interval",
                                a,
                                b
                            );
                            if bits == 0 {
                                wide
                            } else {
                                AV::Ival(bucket(a, bits).0, bucket(b, bits).1)
                            }
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
                if bits > 0 {
                    // A per-lane bucket column: raw intervals where every
                    // lane is one, uniform where all agree.
                    let col = std::mem::replace(&mut self.cols[c as usize], Col::U(AV::Nil));
                    self.cols[c as usize] = collapse_uniform(match col {
                        Col::V(vs) => compress_num_v(vs),
                        other => other,
                    });
                }
            }
        }

        // 3. dash_effect_time clamp.
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

        // 3b. Fruit off/y widening (abstraction.rs:720): each live fruit's
        // bob counter becomes the full period [0, 39] and its y the whole
        // bob band start +/- 2.5 (sin is in [-1, 1]) - bit for bit the
        // interpreter's behavior at every NON-EXACT level. `off` and `y`
        // widen TOGETHER (one without the other produces a row no
        // interpreter level has - room20-plan.md "only half a widening"),
        // and the widening must only ever grow the value it replaces
        // (asserted per lane, like the interpreter). At Exact, `off` is
        // pinned modulo its period instead (`apply_conservative_widenings`).
        if rem_bits.is_none() {
            for obj in self.objects_of_type(ids, ids.g_fruit) {
                let off_cell = self
                    .obj_field_cell(obj, ids.f_off)
                    .unwrap_or_else(|| panic!("fruit-off pin: fruit has no `off` field"));
                let reduce = |v: AV| -> AV {
                    match v {
                        AV::Num(n) => {
                            let i = n
                                .as_i16()
                                .unwrap_or_else(|| panic!("fruit-off pin: off {:?} is not an integer", n));
                            assert!(i >= 0, "fruit-off pin: off {} is negative", i);
                            AV::Num(P8::from_i16(i % 40))
                        }
                        AV::Ival(a, b) => {
                            assert!(
                                a >= P8::from_i16(0) && b <= P8::from_i16(40),
                                "fruit-off pin: widened off outside [0, 40]: [{:?}, {:?}]",
                                a,
                                b
                            );
                            v
                        }
                        other => panic!("fruit-off pin: off is not a number: {:?}", other),
                    }
                };
                self.cols[off_cell as usize] = match &self.cols[off_cell as usize] {
                    Col::U(v) => Col::U(reduce(*v)),
                    Col::V(vs) => Col::V(vs.iter().map(|v| reduce(*v)).collect()),
                    Col::N(vs) => Col::V(vs.iter().map(|n| reduce(AV::Num(*n))).collect()),
                    Col::I(vs) => Col::V(vs.iter().map(|(a, b)| reduce(AV::Ival(*a, *b))).collect()),
                };
            }
        }
        let widened_fruit = if rem_bits.is_some() {
            self.objects_of_type(ids, ids.g_fruit)
        } else {
            Vec::new()
        };
        for obj in widened_fruit {
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

        // 4. timer pins.
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

    }

    /// Shared boundary tail: canonical ids, the shape hash, and the
    /// per-lane row keys.
    fn boundary_finish(&mut self) {
        assert!(self.prints.is_empty(), "prints at a frame boundary: {:?}", self.prints);

        self.canonicalize_ids();

        // Structure hash (uniform across lanes) - the block's shape key.
        let shape_hash = self.shape_hash_of();
        self.shape_hash = shape_hash;

        // Per-lane 128-bit row key over the compacted value cells:
        // ORDER-INDEPENDENT per-cell mixes summed per lane (the
        // interpreter's row key is order-independent for the same reason).
        // Uniform cells fold ONCE into a block partial - sound because the
        // sum is independent of which cells happen to be uniform in this
        // block, so keys agree across blocks with different splits.
        // (mix64/av_code/cell_mix live at module scope: the kernels' own
        // pre-dedup keys are built from the SAME primitives.)
        let w = self.width;
        let mut part1: u64 = shape_hash;
        let mut part2: u64 = 0xa076_1d64_78bd_642f ^ shape_hash;
        let mut h1: Vec<u64> = vec![0; w];
        let mut h2: Vec<u64> = vec![0; w];
        for (c, cell) in self.structure.iter().enumerate() {
            if !matches!(cell, Cell2::Val) {
                continue;
            }
            let ci = c as u64;
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
                    // Same key as cell_mix(ci, AV::Num(v), seed), with
                    // the per-cell constants hoisted so the loop is a
                    // flat elementwise xor/mix chain (vectorizable).
                    let c1 = KEY_SEED1 ^ ci.wrapping_mul(CELL_K);
                    let c2 = KEY_SEED2 ^ ci.wrapping_mul(CELL_K);
                    for i in 0..w {
                        let code = 1u64 << 56 | vs[i].to_bits() as u64;
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
        self.row_keys = (0..w)
            .map(|i| (
                mix64(part1.wrapping_add(h1[i])),
                mix64(part2.wrapping_add(h2[i])),
            ))
            .collect();
    }

    /// The boundary's tail: dedup within the block (keeping the first lane
    /// of each row key). `boundary` = `boundary_canonicalize` + this.
    fn boundary_dedup(&mut self) -> usize {
        let w = self.width;
        // Dedup within the block, keeping the first lane of each row.
        let mut keep: Vec<u32> = Vec::new();
        let mut seen_rows: FxHashMap<(u64, u64), ()> = FxHashMap::default();
        for i in 0..w {
            if let std::collections::hash_map::Entry::Vacant(e) =
                seen_rows.entry(self.row_keys[i])
            {
                e.insert(());
                keep.push(i as u32);
            }
        }
        self.retain_lanes(&keep);
        self.width
    }

    /// Per-lane key of the "is the object moving" gates (anonymous_61
    /// __entry: `spd.x ~= 0 or spd.y ~= 0`, evaluated on FRAME-START spd -
    /// obj.move runs before update touches spd). One bit per object in
    /// walk order. Pre-partitioning by this removes the SplitReq rerun
    /// for the known divergent gates; novel gates still throw.
    pub fn moving_key(&self, ids: &BoundaryIds) -> Option<Vec<u8>> {
        let arr = self.global_target(ids.g_objects)?;
        let Cell2::Arr(items) = &self.structure[arr as usize] else {
            return None;
        };
        let mut key = vec![0u8; self.width];
        let zero = P8::from_i16(0);
        let nonzero = |v: AV| -> bool {
            match v {
                AV::Num(n) => n != zero,
                // An interval counts as nonzero exactly when `~= 0` is true,
                // which for `Ival == _ -> false` is always.
                AV::Ival(..) => true,
                other => panic!("moving_key: spd is not numeric: {:?}", other),
            }
        };
        for (bit, item) in items.iter().enumerate() {
            let obj = match &self.structure[*item as usize] {
                Cell2::Val => match self.cols[*item as usize] {
                    Col::U(AV::Ptr(o)) => o,
                    _ => continue,
                },
                _ => *item,
            };
            let Some(spd_cell) = self.obj_field_cell(obj, ids.f_spd) else {
                continue;
            };
            let Col::U(AV::Ptr(spd_obj)) = self.cols[spd_cell as usize] else {
                continue;
            };
            for f in [ids.f_x, ids.f_y] {
                let Some(c) = self.obj_field_cell(spd_obj, f) else { continue };
                match &self.cols[c as usize] {
                    Col::U(v) => {
                        if nonzero(*v) {
                            for k in key.iter_mut() {
                                *k |= 1 << (bit % 8);
                            }
                        }
                    }
                    Col::V(vs) => {
                        for (i, v) in vs.iter().enumerate() {
                            if nonzero(*v) {
                                key[i] |= 1 << (bit % 8);
                            }
                        }
                    }
                    Col::N(vs) => {
                        for (i, n) in vs.iter().enumerate() {
                            if *n != zero {
                                key[i] |= 1 << (bit % 8);
                            }
                        }
                    }
                    // Interval spd: `~= 0` is always true (Ival == _ -> false).
                    Col::I(_) => {
                        for k in key.iter_mut() {
                            *k |= 1 << (bit % 8);
                        }
                    }
                }
            }
        }
        Some(key)
    }

    /// Keep only the given lanes (ascending indices), in every value
    /// column, closure capture and row key.
    pub fn retain_lanes(&mut self, keep: &[u32]) {
        if keep.len() == self.width {
            return;
        }
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

    /// Resolve the pm1 key cells on this block (canonical ids, so the
    /// same for every block of a shape). Missing pieces (no player at
    /// spawn, absent global) just drop out of the key.
    pub fn pm1_cells(&self, ids: &BoundaryIds) -> Vec<u32> {
        let mut cells: Vec<u32> = Vec::new();
        for &g in &ids.g_pm1 {
            let c = self.globals[g as usize];
            if c != NONE {
                cells.push(c);
            }
        }
        // `g_player` holds the player TYPE table, not the instance - the
        // instance is the object in `objects` whose `type` points at it,
        // exactly as mark_walk finds it. Resolving fields off the type
        // table silently found nothing, so pm1 partitioning only ever
        // split on the two globals: engine-produced blocks kept mixed
        // dash_time / p_jump / p_dash and no class kernel could bind them
        // (at f35 that cost the whole next frame - 269,059 lanes - its
        // kernel coverage).
        for obj in self.player_objects(ids) {
            for &f in &ids.f_pm1 {
                if let Some(c) = self.obj_field_cell(obj, f) {
                    cells.push(c);
                }
            }
        }
        cells
    }

    /// Per-row CLASS key: a hash of the values the search buckets a row
    /// by - the freeze global, the moving key (which objects have a
    /// nonzero spd) and the pm1 cells. Two rows with equal class keys can
    /// share a bucket and a kernel call; the kernel's pre-partition
    /// premises (freeze, moving key) and the pm1 grouping are exactly
    /// this, so a bucket is class-uniform by construction.
    pub fn class_keys(&self, ids: &BoundaryIds, g_freeze: u32) -> Vec<u64> {
        let mut cells: Vec<u32> = self.pm1_cells(ids);
        let freeze = self.globals[g_freeze as usize];
        if freeze != NONE && !cells.contains(&freeze) {
            cells.push(freeze);
        }
        let moving = self.moving_key(ids).unwrap_or_else(|| vec![0; self.width]);
        (0..self.width)
            .map(|i| {
                let mut h: u64 = 0x9e37_79b9_7f4a_7c15 ^ moving[i] as u64;
                for &c in &cells {
                    h = mix64(h ^ av_code(self.cols[c as usize].at(i)) ^ (c as u64) << 48);
                }
                h
            })
            .collect()
    }

    /// Append `rows` of `src` (a same-shape block) to this block, column
    /// by column, keeping a column uniform for as long as every appended
    /// value equals it. This is the bucket write: no clone, no merge, no
    /// re-partition - rows land in the block they belong to.
    pub fn append_rows(&mut self, src: &Rt2, rows: &[u32]) {
        assert_eq!(self.shape_hash, src.shape_hash, "append_rows: different shapes");
        assert_eq!(self.structure.len(), src.structure.len());
        let w = self.width;
        for c in 0..self.cols.len() {
            if !matches!(self.structure[c], Cell2::Val) {
                continue;
            }
            let dst = &mut self.cols[c];
            let sc = &src.cols[c];
            // Fast path: both uniform and equal.
            if let (Col::U(a), Col::U(b)) = (&*dst, sc) {
                if a == b {
                    continue;
                }
            }
            if w == 0 {
                // An empty block takes the first append's column as-is:
                // uniform if the rows agree, else the raw typed form.
                let vs: Vec<AV> = rows.iter().map(|&r| sc.at(r as usize)).collect();
                *dst = collapse_uniform(compress_num_v(vs));
            } else {
                // The running width: a uniform column that first diverges
                // at the n-th appended row materializes `w + n` copies.
                for (n, &r) in rows.iter().enumerate() {
                    col_push(dst, w + n, sc.at(r as usize));
                }
            }
        }
        for cell in self.structure.iter_mut() {
            if let Cell2::Clo(_, caps) = cell {
                for cap in caps.iter_mut() {
                    if let Col::U(_) = cap {
                        continue;
                    }
                    panic!("append_rows: a closure capture is not uniform");
                }
            }
        }
        self.row_keys.extend(rows.iter().map(|&r| src.row_keys[r as usize]));
        self.width = w + rows.len();
    }

    /// A copy of just the lanes in `[lo, hi)` - the chunking fast path
    /// (clone_block + retain_lanes copies the whole block first).
    pub fn slice_lanes(&self, lo: usize, hi: usize) -> Rt2 {
        let slice_col = |c: &Col| -> Col {
            match c {
                Col::U(a) => Col::U(*a),
                Col::V(vs) => Col::V(vs[lo..hi].to_vec()),
                Col::N(vs) => Col::N(vs[lo..hi].to_vec()),
                Col::I(vs) => Col::I(vs[lo..hi].to_vec()),
            }
        };
        Rt2 {
            width: hi - lo,
            structure: self
                .structure
                .iter()
                .map(|cell| match cell {
                    Cell2::Clo(f, caps) => {
                        Cell2::Clo(*f, caps.iter().map(&slice_col).collect())
                    }
                    other => other.clone(),
                })
                .collect(),
            cols: self.cols.iter().map(&slice_col).collect(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            shape_hash: self.shape_hash,
            row_keys: Vec::new(),
        }
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

