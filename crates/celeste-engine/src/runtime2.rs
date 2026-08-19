//! Columnar abstract runtime (plans/columnar-engine.md).
//!
//! The SAME generated program (gen.rs, via the `Engine` trait) runs over a
//! BLOCK of abstract lanes: heap structure is shared (uniform across lanes
//! by the shape premise), values are per-lane columns, and the
//! lane-multiplying instructions (`expand`, `__split_by_flr`, `__split_at`)
//! append lanes to the block mid-flight instead of forking control flow -
//! which the zero-divergence census makes legal.
//!
//! Per-lane value semantics are ports of the interpreter's scalar-state
//! arms, with file:line pointers:
//!   op.rs:397-666  (binary ops, interval compares -> tri-state)
//!   op.rs:12-49    (unary ops)
//!   flow.rs:343    (branch truthiness; UnknownBool branch = fork, which
//!                   this engine treats as a loud error - the compiled
//!                   shape program has no such branch)
//!   game_runner.rs (builtin interval arms: min/max/abs/flr/sin, splits)
//!
//! Boundary abstraction ports abstraction.rs (rem widening, dash_effect_time
//! clamp, timer pins); the mark walk runs over the shared structure heap.

use std::sync::Arc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::Pico8Num;
use rustc_hash::FxHashMap;

pub type P8 = Pico8Num;

/// Row-key primitives. Shared with the generated kernels, whose per-chunk
/// pre-dedup keys must collapse exactly what `boundary`'s keys collapse -
/// so they mix the same way rather than approximating it.
#[inline]
pub fn mix64(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
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

#[inline]
pub fn cell_mix(c: u64, v: AV, seed: u64) -> u64 {
    mix64(seed ^ c.wrapping_mul(0x9e37_79b9_7f4a_7c15) ^ av_code(v))
}

/// One lane's abstract value. `Copy`, 12 bytes + tag.
/// Mirrors `interpreter::value::Value` scalar variants plus intervals.
#[derive(Clone, Copy, PartialEq, Debug)]
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
    pub g_timers: Vec<u32>, // frames, seconds, minutes, deaths
    pub f_type: u32,
    pub f_rem: u32,
    pub f_spd: u32,
    pub f_x: u32,
    pub f_y: u32,
    pub f_dash_effect_time: u32,
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
    /// Local-value arena. `None` = killed (use-after-kill panics loudly).
    /// Cleared at every frame boundary - all locals are dead there.
    pub arena: Vec<Option<Col>>,
    pub globals: Vec<u32>,
    pub strings: Vec<String>,
    pub cart: Arc<CartData>,
    pub cache: Arc<CollisionCache>,
    pub prints: Vec<String>,
    // stats
    pub stat_splits: u64,
    pub stat_appended: u64,
    pub stat_arena_peak: usize,
    /// Frame-start lane each current lane derives from (splits append).
    pub origin: Vec<u32>,
    /// COW lane maps (plans/columnar-engine.md "COW/lane-indirection"):
    /// one entry per widen this frame, (width_before, map current-lane ->
    /// that width's lane space; identity on 0..width_before). A column
    /// whose len is smaller than the block width reads appended lanes
    /// through the matching map instead of being physically widened.
    history: Vec<(usize, Vec<u32>)>,
    /// Recycled column buffers (killed/overwritten varying columns).
    pool: Vec<Vec<AV>>,
    /// Per-category time census (CELESTE_OP_CENSUS=1): name -> (ns, calls,
    /// lanes). The data that decides the next optimization, op_census
    /// doctrine.
    pub census: Option<FxHashMap<&'static str, (u64, u64, u64)>>,
    /// Set by `boundary`: the canonical structure hash (the shape key).
    pub shape_hash: u64,
    /// Set by `boundary`: per-lane 128-bit canonical row keys.
    pub row_keys: Vec<(u64, u64)>,
}

pub const NONE: u32 = u32::MAX;

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
            arena: Vec::new(),
            globals: vec![NONE; globals_len],
            strings: static_strings.iter().map(|s| s.to_string()).collect(),
            cart,
            cache,
            prints: Vec::new(),
            stat_splits: 0,
            stat_appended: 0,
            stat_arena_peak: 0,
            pool: Vec::new(),
            census: std::env::var_os("CELESTE_OP_CENSUS").map(|_| FxHashMap::default()),
            origin: Vec::new(),
            history: Vec::new(),
            shape_hash: 0,
            row_keys: Vec::new(),
        }
    }

    /// Append lanes: `srcs[k]` is the source lane the k-th appended lane
    /// copies. Uniform columns are untouched (appending a copy preserves
    /// uniformity); varying columns extend. Covers every live local column
    /// (the whole call stack - the arena is shared) and every heap value
    /// column.
    fn widen(&mut self, srcs: &[usize]) {
        if srcs.is_empty() {
            return;
        }
        let t = self.t0();
        self.stat_splits += 1;
        self.stat_appended += srcs.len() as u64;
        let old_w = self.width;
        // COW: no column is touched. Extend every existing map, then
        // record this width's map (identity below old_w, srcs above).
        for (_, map) in self.history.iter_mut() {
            map.reserve(srcs.len());
            for &s in srcs {
                let m = map[s];
                map.push(m);
            }
        }
        let mut map: Vec<u32> = Vec::with_capacity(old_w + srcs.len());
        map.extend(0..old_w as u32);
        map.extend(srcs.iter().map(|&s| s as u32));
        self.history.push((old_w, map));
        for &s in srcs {
            let o = self.origin[s];
            self.origin.push(o);
        }
        self.width += srcs.len();
        self.rec("widen", t);
    }

    #[inline]
    fn rec(&mut self, name: &'static str, t0: Option<std::time::Instant>) {
        if let (Some(census), Some(t0)) = (self.census.as_mut(), t0) {
            let e = census.entry(name).or_insert((0, 0, 0));
            e.0 += t0.elapsed().as_nanos() as u64;
            e.1 += 1;
        }
    }

    #[inline]
    fn t0(&self) -> Option<std::time::Instant> {
        if self.census.is_some() {
            Some(std::time::Instant::now())
        } else {
            None
        }
    }

    /// Merge a sub-block's census into an accumulator map.
    pub fn drain_census(&mut self, into: &mut FxHashMap<&'static str, (u64, u64, u64)>) {
        if let Some(census) = self.census.take() {
            for (k, v) in census {
                let e = into.entry(k).or_insert((0, 0, 0));
                e.0 += v.0;
                e.1 += v.1;
                e.2 += v.2;
            }
            self.census = Some(FxHashMap::default());
        }
    }

    /// Length of a column's physical data (width means "not stale").
    #[inline]
    fn col_len(c: &Col) -> usize {
        match c {
            Col::U(_) => usize::MAX,
            Col::V(v) => v.len(),
            Col::N(v) => v.len(),
            Col::I(v) => v.len(),
        }
    }

    /// The physical index a (possibly stale) column uses for `lane`:
    /// direct below its length, through the COW map above it.
    #[inline]
    fn phys(&self, len: usize, lane: usize) -> usize {
        if lane < len {
            return lane;
        }
        let (_, map) = self
            .history
            .iter()
            .find(|(w, _)| *w == len)
            .unwrap_or_else(|| panic!("stale column of len {} has no COW map", len));
        map[lane] as usize
    }

    /// Physically widen a stale column to the block width.
    fn materialize_col(&self, c: &Col) -> Option<Col> {
        let len = Self::col_len(c);
        if len >= self.width {
            return None;
        }
        Some(match c {
            Col::U(_) => unreachable!(),
            Col::V(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::V(nv)
            }
            Col::N(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::N(nv)
            }
            Col::I(v) => {
                let mut nv = Vec::with_capacity(self.width);
                nv.extend_from_slice(v);
                nv.extend((len..self.width).map(|i| v[self.phys(len, i)]));
                Col::I(nv)
            }
        })
    }

    /// Materialize a heap column in place.
    fn resolve_cell(&mut self, p: usize) {
        if let Some(m) = self.materialize_col(&self.cols[p]) {
            self.cols[p] = m;
        }
    }



}


impl Rt2 {
    fn global_target(&self, g: u32) -> Option<u32> {
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

    fn obj_field_cell(&self, obj: u32, f: u32) -> Option<u32> {
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
        let mut found = Vec::new();
        let Some(arr) = self.global_target(ids.g_objects) else {
            return found;
        };
        let Cell2::Arr(items) = &self.structure[arr as usize] else {
            return found;
        };
        let player_type = self.global_target(ids.g_player);
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
            let is_player = match self.cols[type_cell as usize] {
                Col::U(AV::Ptr(t)) => Some(t) == player_type,
                _ => false,
            };
            if is_player {
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

    /// The canonical BFS order (the boundary's numbering) without
    /// mutating anything - identity iff the block is already canonical
    /// (what the BFS-ordered importer guarantees; slot binding relies
    /// on it, so the bench asserts it per imported block).
    pub fn is_canonical_order(&self) -> bool {
        let mut order: Vec<u32> = Vec::new();
        let mut seen = vec![false; self.structure.len()];
        let mut queue: std::collections::VecDeque<u32> = Default::default();
        let enqueue = |t: u32, seen: &mut Vec<bool>, queue: &mut std::collections::VecDeque<u32>, order: &mut Vec<u32>| {
            if !seen[t as usize] {
                seen[t as usize] = true;
                order.push(t);
                queue.push_back(t);
            }
        };
        for &cell in self.globals.iter() {
            if cell != NONE {
                enqueue(cell, &mut seen, &mut queue, &mut order);
            }
        }
        while let Some(c) = queue.pop_front() {
            match &self.structure[c as usize] {
                Cell2::Val => match &self.cols[c as usize] {
                    Col::U(AV::Ptr(t)) => enqueue(*t, &mut seen, &mut queue, &mut order),
                    Col::V(vs) => {
                        for v in vs {
                            if let AV::Ptr(t) = v {
                                enqueue(*t, &mut seen, &mut queue, &mut order);
                            }
                        }
                    }
                    _ => {}
                },
                Cell2::Obj(fields) => {
                    for (_, t) in fields {
                        enqueue(*t, &mut seen, &mut queue, &mut order);
                    }
                }
                Cell2::Arr(items) => {
                    for t in items {
                        enqueue(*t, &mut seen, &mut queue, &mut order);
                    }
                }
                Cell2::Clo(_, caps) => {
                    for cap in caps.iter() {
                        if let Col::U(AV::Ptr(t)) = cap {
                            enqueue(*t, &mut seen, &mut queue, &mut order);
                        }
                    }
                }
                Cell2::Unk | Cell2::Bi(_) => {}
            }
        }
        order.len() == self.structure.len()
            && order.iter().enumerate().all(|(i, &o)| o == i as u32)
    }

    /// Boundary abstraction + canonical row dedup + compaction. Returns
    /// the surviving lane count (= next frame's width).
    ///
    /// Ports, in order (make_state_abstract, abstraction.rs:286):
    ///   1. rem widening at Bits(0): full closed interval [-0.5, 0.5-eps]
    ///      (abstraction.rs:521; other precisions are follow-up work).
    ///   2. spd: Exact (the default) - nothing.
    ///   3. dash_effect_time clamp at 0 from below (abstraction.rs:713).
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

    /// The boundary WITHOUT the final within-block dedup: abstraction,
    /// canonical BFS compaction, and the per-lane row keys for ALL `width`
    /// lanes, in lane order. Split out for the D1 key gate
    /// (plans/dedup-roofline-plan.md): the gate pairs each lane's engine
    /// row key with the interpreter's `visited_row_keys` for the SAME
    /// lane, which requires the keys of lanes the dedup would drop.
    pub fn boundary_canonicalize(&mut self, ids: &BoundaryIds) {
        // Materialize every stale column - the boundary walks whole
        // columns (BFS pointer scan, hashing, compaction).
        for p in 0..self.cols.len() {
            self.resolve_cell(p);
        }
        for ci in 0..self.structure.len() {
            let stale: Vec<(usize, Col)> = match &self.structure[ci] {
                Cell2::Clo(_, caps) => caps
                    .iter()
                    .enumerate()
                    .filter_map(|(i, c)| self.materialize_col(c).map(|m| (i, m)))
                    .collect(),
                _ => continue,
            };
            if let Cell2::Clo(_, caps) = &mut self.structure[ci] {
                for (i, m) in stale {
                    caps[i] = m;
                }
            }
        }
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
        self.history.clear();
        let (rem_cells, det_cells) = self.mark_walk(ids);

        // 1. rem widening, Bits(0).
        let half = P8::from_parts(0, 0x8000);
        let neg_half = -half;
        let half_below = half.next_smallest();
        let wide = AV::Ival(neg_half, half_below);
        for c in rem_cells {
            let col = &self.cols[c as usize];
            let check = |v: AV| match v {
                AV::Num(n) => assert!(
                    n >= neg_half && n <= half_below,
                    "player_rem value {:?} not in expected interval",
                    n
                ),
                AV::Ival(a, b) => assert!(
                    a >= neg_half && b <= half_below,
                    "player_rem interval [{:?}, {:?}] not in expected interval",
                    a,
                    b
                ),
                other => panic!("Unexpected value type for player_rem: {:?}", other),
            };
            match col {
                Col::U(v) => check(*v),
                Col::V(vs) => vs.iter().for_each(|v| check(*v)),
                Col::N(vs) => vs.iter().for_each(|n| check(AV::Num(*n))),
                Col::I(vs) => vs.iter().for_each(|(a, b)| check(AV::Ival(*a, *b))),
            }
            self.cols[c as usize] = Col::U(wide);
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

        // 4. timer pins.
        for &g in &ids.g_timers {
            let cell = self.globals[g as usize];
            assert!(cell != NONE, "timer global missing - pin would silently not apply");
            match &self.cols[cell as usize] {
                Col::U(AV::Num(_)) | Col::V(_) => {
                    self.cols[cell as usize] = Col::U(AV::Num(zero));
                }
                other => panic!("timer global is not a number: {:?}", other),
            }
        }

        assert!(self.prints.is_empty(), "prints at a frame boundary: {:?}", self.prints);

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
                    part1 = part1.wrapping_add(cell_mix(ci, *v, 0x5bf0_3635));
                    part2 = part2.wrapping_add(cell_mix(ci, *v, 0x27d4_eb2f));
                }
                Col::V(vs) => {
                    for i in 0..w {
                        h1[i] = h1[i].wrapping_add(cell_mix(ci, vs[i], 0x5bf0_3635));
                        h2[i] = h2[i].wrapping_add(cell_mix(ci, vs[i], 0x27d4_eb2f));
                    }
                }
                Col::N(vs) => {
                    // Same key as cell_mix(ci, AV::Num(v), seed), with
                    // the per-cell constants hoisted so the loop is a
                    // flat elementwise xor/mix chain (vectorizable).
                    let c1 = 0x5bf0_3635u64 ^ ci.wrapping_mul(0x9e37_79b9_7f4a_7c15);
                    let c2 = 0x27d4_eb2fu64 ^ ci.wrapping_mul(0x9e37_79b9_7f4a_7c15);
                    for i in 0..w {
                        let code = 1u64 << 56 | vs[i].to_bits() as u64;
                        h1[i] = h1[i].wrapping_add(mix64(c1 ^ code));
                        h2[i] = h2[i].wrapping_add(mix64(c2 ^ code));
                    }
                }
                Col::I(vs) => {
                    for i in 0..w {
                        let v = AV::Ival(vs[i].0, vs[i].1);
                        h1[i] = h1[i].wrapping_add(cell_mix(ci, v, 0x5bf0_3635));
                        h2[i] = h2[i].wrapping_add(cell_mix(ci, v, 0x27d4_eb2f));
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
    /// of each row key) and recycle the local arena. `boundary` =
    /// `boundary_canonicalize` + this.
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

        // All locals are dead at the boundary; recycle their buffers.
        self.stat_arena_peak = self.stat_arena_peak.max(self.arena.len());
        for slot in self.arena.drain(..) {
            if let Some(Col::V(vec)) = slot {
                if self.pool.len() < 48 {
                    self.pool.push(vec);
                }
            }
        }

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

    /// Split into sub-blocks of lanes sharing a key value.
    pub fn partition_by_key(self, key: &[u8]) -> Vec<Rt2> {
        let mut order: Vec<u8> = Vec::new();
        let mut groups: Vec<Vec<u32>> = Vec::new();
        for (i, k) in key.iter().enumerate() {
            match order.iter().position(|o| o == k) {
                Some(g) => groups[g].push(i as u32),
                None => {
                    order.push(*k);
                    groups.push(vec![i as u32]);
                }
            }
        }
        if groups.len() == 1 {
            return vec![self];
        }
        groups
            .into_iter()
            .map(|keep| {
                let mut b = self.clone_block();
                b.retain_lanes(&keep);
                b
            })
            .collect()
    }

    /// Frame-start button expansion (#47). MEASURED OUT (2026-08-18): it
    /// makes each widen nearly free (empty arena) but the whole frame then
    /// runs 64x wide from instruction 0 - f30 serial went 2.2s -> 7.0s.
    /// The interpreter's lazy expansion wins for the same reason. Kept
    /// (unused) as the measurement's artifact; delete on next cleanup.
    #[allow(dead_code)]
    pub fn expand_buttons(&mut self, g_button_states: u32) {
        let arr = match self.global_target(g_button_states) {
            Some(a) => a,
            None => return,
        };
        let items = match &self.structure[arr as usize] {
            Cell2::Arr(items) => items.clone(),
            _ => return,
        };
        for item in items {
            let target = match &self.structure[item as usize] {
                Cell2::Val => match self.cols[item as usize] {
                    Col::U(AV::Ptr(t)) => t,
                    _ => item,
                },
                _ => item,
            };
            if !matches!(self.cols[target as usize], Col::U(AV::UBool)) {
                continue;
            }
            let w = self.width;
            let srcs: Vec<usize> = (0..w).collect();
            self.widen(&srcs);
            let mut col: Vec<AV> = Vec::with_capacity(2 * w);
            col.extend(std::iter::repeat(AV::Bool(true)).take(w));
            col.extend(std::iter::repeat(AV::Bool(false)).take(w));
            self.cols[target as usize] = Col::V(col);
        }
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

    /// The pm1 partition (the recipe's partition_merge entry, ported to
    /// the engine's frame boundary): split a post-boundary block so the
    /// fork-condition cells are per-block UNIFORM. This is the
    /// interpreter's fragment representation - the measured reason it
    /// beats one-wide-block-per-shape: key-correlated columns stay
    /// Col::U through storage, merge and row hashing.
    pub fn partition_pm1(self, ids: &BoundaryIds) -> Vec<Rt2> {
        let cells = self.pm1_cells(ids);
        let mut parts = vec![self];
        for c in cells {
            parts = parts.into_iter().flat_map(|b| b.partition_by_cell(c)).collect();
        }
        parts
    }

    /// Group key for same-shape merging under pm1: a hash of the key
    /// cells' (uniform) values. Call on pm1-partitioned blocks.
    pub fn pm1_key_hash(&self, ids: &BoundaryIds) -> u64 {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        for c in self.pm1_cells(ids) {
            let v = match &self.cols[c as usize] {
                Col::U(v) => *v,
                // Non-uniform key cell: only possible when a caller skips
                // partition_pm1; fold lane 0 so grouping stays legal
                // (concat requires identical shapes, not key uniformity).
                Col::V(vs) => vs[0],
                Col::N(vs) => AV::Num(vs[0]),
                Col::I(vs) => AV::Ival(vs[0].0, vs[0].1),
            };
            h.write_u32(c);
            h.write_u64(match v {
                AV::Num(n) => 1u64 << 56 | n.to_bits() as u64,
                AV::Ival(a, b) => {
                    2u64 << 56 | (a.to_bits() as u64) << 24 ^ (b.to_bits() as u64)
                }
                AV::Bool(b) => 3u64 << 56 | b as u64,
                AV::UBool => 4u64 << 56,
                AV::Str(x) => 5u64 << 56 | x as u64,
                AV::Nil => 6u64 << 56,
                AV::Ptr(p) => 7u64 << 56 | p as u64,
                AV::NilPtr => 8u64 << 56,
            });
        }
        h.finish()
    }

    /// Split the block into sub-blocks whose lanes agree on the value in
    /// `cell` (the frame-start uniform-branch pre-partition; the freeze
    /// gate is the pm1 precedent). Groups in first-occurrence order.
    /// Store every all-equal column as uniform. Representation-only (see
    /// `collapse_uniform`) and idempotent.
    pub fn collapse_uniform_cols(&mut self) {
        for p in 0..self.cols.len() {
            let col = std::mem::replace(&mut self.cols[p], Col::U(AV::Nil));
            self.cols[p] = collapse_uniform(col);
        }
    }

    pub fn partition_by_cell(self, cell: u32) -> Vec<Rt2> {
        let by_vals = |vals: Vec<AV>| -> Vec<Vec<u32>> {
            let mut order: Vec<AV> = Vec::new();
            let mut groups: Vec<Vec<u32>> = Vec::new();
            for (i, v) in vals.iter().enumerate() {
                match order.iter().position(|o| o == v) {
                    Some(g) => groups[g].push(i as u32),
                    None => {
                        order.push(*v);
                        groups.push(vec![i as u32]);
                    }
                }
            }
            groups
        };
        let groups: Vec<Vec<u32>> = match &self.cols[cell as usize] {
            Col::U(_) => return vec![self],
            Col::N(vs) => by_vals(vs.iter().map(|n| AV::Num(*n)).collect()),
            Col::I(vs) => by_vals(vs.iter().map(|(a, b)| AV::Ival(*a, *b)).collect()),
            Col::V(vs) => {
                let mut order: Vec<AV> = Vec::new();
                let mut groups: Vec<Vec<u32>> = Vec::new();
                for (i, v) in vs.iter().enumerate() {
                    match order.iter().position(|o| o == v) {
                        Some(g) => groups[g].push(i as u32),
                        None => {
                            order.push(*v);
                            groups.push(vec![i as u32]);
                        }
                    }
                }
                groups
            }
        };
        if groups.len() == 1 {
            // Already pure for this cell - but "pure" means UNIFORM, and
            // saying so in the representation is the whole point (a
            // constant Col::N does not bind). This early path is the
            // COMMON one, so skipping the collapse here left nearly every
            // block unbindable even after it had been partitioned.
            let mut b = self;
            let col = std::mem::replace(&mut b.cols[cell as usize], Col::U(AV::Nil));
            b.cols[cell as usize] = collapse_uniform(col);
            return vec![b];
        }
        groups
            .into_iter()
            .map(|keep| {
                let mut b = self.clone_block();
                b.retain_lanes(&keep);
                // A partition exists to make some cell single-valued;
                // say so in the representation. Without this the split
                // column stays a constant Col::N, and every kernel bind
                // (which wants Col::U for its block-uniform inputs)
                // refuses the very blocks partitioning just made
                // bindable.
                b.collapse_uniform_cols();
                b
            })
            .collect()
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
            arena: Vec::new(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            stat_splits: self.stat_splits,
            stat_appended: self.stat_appended,
            stat_arena_peak: self.stat_arena_peak,
            pool: Vec::new(),
            census: self.census.as_ref().map(|_| FxHashMap::default()),
            origin: Vec::new(),
            history: Vec::new(),
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
            arena: Vec::new(),
            globals: self.globals.clone(),
            strings: self.strings.clone(),
            cart: self.cart.clone(),
            cache: self.cache.clone(),
            prints: self.prints.clone(),
            stat_splits: self.stat_splits,
            stat_appended: self.stat_appended,
            stat_arena_peak: self.stat_arena_peak,
            pool: Vec::new(),
            census: self.census.as_ref().map(|_| FxHashMap::default()),
            origin: self.origin.clone(),
            history: Vec::new(),
            shape_hash: self.shape_hash,
            row_keys: self.row_keys.clone(),
        }
    }

    /// K-way merge of same-shape blocks (all after `boundary`): one pass
    /// per column, no repeated re-materialization of uniform columns.
    pub fn merge_many(mut blocks: Vec<Rt2>) -> Rt2 {
        if blocks.len() == 1 {
            return blocks.pop().unwrap();
        }
        let total: usize = blocks.iter().map(|b| b.width).sum();
        let mut host = blocks.swap_remove(0);
        for b in &blocks {
            assert_eq!(host.shape_hash, b.shape_hash, "merge of different shapes");
            assert_eq!(host.structure.len(), b.structure.len());
        }
        for (c, col) in host.cols.iter_mut().enumerate() {
            let all_same_uniform = match col {
                Col::U(a) => blocks
                    .iter()
                    .all(|b| matches!(&b.cols[c], Col::U(x) if x == a)),
                Col::V(_) | Col::N(_) | Col::I(_) => false,
            };
            if all_same_uniform {
                continue;
            }
            let mut vs: Vec<AV> = Vec::with_capacity(total);
            let push_col = |vs: &mut Vec<AV>, c: &Col, w: usize| match c {
                Col::U(a) => vs.extend(std::iter::repeat(*a).take(w)),
                Col::V(v) => vs.extend_from_slice(v),
                Col::N(v) => vs.extend(v.iter().map(|n| AV::Num(*n))),
                Col::I(v) => vs.extend(v.iter().map(|(a, b)| AV::Ival(*a, *b))),
            };
            let hw = host.width;
            push_col(&mut vs, col, hw);
            for b in &blocks {
                push_col(&mut vs, &b.cols[c], b.width);
            }
            *col = compress_num_v(vs);
        }
        for b in &blocks {
            host.row_keys.extend_from_slice(&b.row_keys);
        }
        host.width = total;
        host
    }

}

