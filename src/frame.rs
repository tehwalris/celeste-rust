//! The frame-step interface, the block, and the minimal outer loop
//! (plans/architecture.md).
//!
//! Rebuilt core, written fresh against the agreed interfaces rather than copied
//! from `search/run.rs`. A block is OPAQUE to the loop except for two exposed
//! columns - its KEYS and its POSITIONS; the only other thing that crosses is
//! the frame-step call.
//!
//! Impls (kernel runner + `trace::refengine`) and the loop's checkpoint /
//! position-graph growth come next; this is the interface + the frame spine.

use anyhow::{Context, Result};
use crate::search::door::Admit;
use std::ops::Range;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::interpreter::state::State;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Col, Rt2, AV};

/// The data currency (interface #2). A columnar batch of lanes - the engine's
/// own block (`Rt2`: one shape, one column per value cell, the key column the
/// boundary computed), carried as-is. Two columns are EXPOSED - the per-lane
/// key and the per-lane position - and nothing else; the field data stays
/// inside. A whole block serializes compactly (the lanes share structure), so
/// the loop always moves blocks, never individual lanes.
///
/// It IS the kernel's block, so there is no bridge in the loop: the frame
/// step consumes and produces it directly, `keep` is a column filter, the
/// regroup is a column append, and the checkpoint is the columns. The
/// interpreter `State` appears only at the edges - the initial state, the
/// reference engine, and the ladder filter's coarsening - through
/// `from_state` / `to_state`.
pub struct Block {
    rt2: Rt2,
    /// The rows' stable ids `pack_id(layer, seq, row)` - set when the block
    /// is a checkpointed layer's piece (or a piece being built for one);
    /// empty for a block that is not part of a tree yet.
    ids: Vec<u64>,
    /// The piece's file seq within its layer (`s{shape}_{seq}.bin`).
    seq: u32,
    /// Lanes the frame step must not expand (won rows: checkpointed as
    /// backward seeds, never expanded); empty when none. Kept as a mask
    /// rather than dropped so the rows' ids stay consecutive.
    skip: Vec<bool>,
}

/// A state's stable id: its layer (the frame it was first reached), the
/// piece file within that layer, and its row in the file. Assigned when
/// the row is admitted (a piece is appended to in flush order, so the row's
/// position in the file is known then), never renumbered.
pub fn pack_id(layer: u32, seq: u32, row: u32) -> u64 {
    ((layer as u64) << 48) | ((seq as u64) << 32) | row as u64
}

pub fn id_layer(id: u64) -> u32 {
    (id >> 48) as u32
}

pub fn id_seq(id: u64) -> u32 {
    ((id >> 32) & 0xffff) as u32
}

pub fn id_row(id: u64) -> u32 {
    id as u32
}

impl Block {
    /// Wrap an engine block. Its key column must be present: every producer
    /// (the kernel boundary, `from_state`, the checkpoint loader) attaches
    /// it, and nothing downstream re-hashes.
    pub fn from_rt2(rt2: Rt2) -> Self {
        assert_eq!(
            rt2.row_keys.len(),
            rt2.width,
            "block without its key column ({} keys for {} lanes)",
            rt2.row_keys.len(),
            rt2.width
        );
        Block { rt2, ids: Vec::new(), seq: 0, skip: Vec::new() }
    }

    /// A block from a reference-interpreter `State`, keyed by the one
    /// canonical rule (`Rt2::row_keys_canonical`: what `engine_row_keys`
    /// computed). The state must already be at its rung's abstraction.
    pub fn from_state(state: &State) -> Result<Self> {
        let (cart, cache) = crate::compiled::room_context()?;
        let mut rt2 = crate::compiled::bridge::import_block(state, cart, cache);
        // The key is the level's WIDENED row (the speed hull: a row stores
        // its tight speed and is keyed on the bucket), the same rule the
        // kernels' key layer applies (`asm_kernel`: the key node override).
        // A fruit-unknown level keys the state's fly fruit as it is: storing
        // the decided fruit is exact, the kernel replaces it at the frame's
        // start (`widen::fork_fruit_inputs`), and the post-`_init` state is a
        // shape no frame returns to, so no key has to agree with it
        // (plans/fly-fruit.md).
        let level = crate::interpreter::abstraction::Level {
            fruit: crate::interpreter::abstraction::FruitPrecision::Exact,
            // Likewise the fall floors (`widen::fork_floor_inputs`).
            floors: crate::interpreter::abstraction::FloorsPrecision::Exact,
            ..crate::interpreter::abstraction::current_level()
        };
        let (_, keys, _) = widened_keys_rt2(&rt2, level)?;
        rt2.row_keys_canonical();
        rt2.row_keys = keys;
        Ok(Block { rt2, ids: Vec::new(), seq: 0, skip: Vec::new() })
    }

    /// The block as an interpreter `State` (`bridge::export_block`), for the
    /// reference engine and the ladder filter's coarsening.
    pub fn to_state(&self) -> State {
        crate::compiled::bridge::export_block(&self.rt2)
    }

    pub fn into_rt2(self) -> Rt2 {
        self.rt2
    }

    pub fn rt2(&self) -> &Rt2 {
        &self.rt2
    }

    pub fn rt2_mut(&mut self) -> &mut Rt2 {
        &mut self.rt2
    }

    /// The key column: the 128-bit canonical row key per lane (shape + content).
    /// Identity for dedup, the visited set, and checkpoints. One entry per lane.
    pub fn keys(&self) -> &[(u64, u64)] {
        &self.rt2.row_keys
    }

    /// The position column: the player-position cell per lane, for grouping and
    /// the position graph. Same length as `keys`.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::block_cells(&self.rt2)
    }

    /// Per lane: does it sit on the room's win target (`wins_of`)?
    pub fn wins(&self) -> Result<Vec<bool>> {
        wins_of(&self.rt2)
    }

    /// Keep only the lanes whose mask entry is true, as a new block; `None` if
    /// none survive. The one splitting primitive the loop needs (dedup at the
    /// door repacks with this).
    pub fn keep(mut self, mask: &[bool]) -> Option<Block> {
        let keep: Vec<u32> = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &m)| m.then_some(i as u32))
            .collect();
        if keep.is_empty() {
            return None;
        }
        self.rt2.retain_lanes(&keep);
        if !self.ids.is_empty() {
            self.ids = keep.iter().map(|&i| self.ids[i as usize]).collect();
        }
        if !self.skip.is_empty() {
            self.skip = keep.iter().map(|&i| self.skip[i as usize]).collect();
        }
        Some(self)
    }

    /// Mark lanes the frame step must not expand.
    pub fn set_skip(&mut self, skip: Vec<bool>) {
        assert_eq!(skip.len(), self.lanes());
        self.skip = if skip.iter().any(|&s| s) { skip } else { Vec::new() };
    }

    pub fn skip(&self) -> &[bool] {
        &self.skip
    }

    /// Lanes that will be expanded (not skipped).
    pub fn active_lanes(&self) -> usize {
        self.lanes() - self.skip.iter().filter(|&&s| s).count()
    }

    /// A piece of a layer: its rows' ids and its file seq.
    pub fn with_ids(rt2: Rt2, ids: Vec<u64>, seq: u32) -> Self {
        assert_eq!(ids.len(), rt2.width, "one id per row");
        let mut b = Block::from_rt2(rt2);
        b.ids = ids;
        b.seq = seq;
        b
    }

    /// The rows' ids (empty if the block is not a layer's piece).
    pub fn ids(&self) -> &[u64] {
        &self.ids
    }

    pub fn seq(&self) -> u32 {
        self.seq
    }

    /// The block's shard shape. A block is one shape.
    pub fn shard_shape(&self) -> u64 {
        self.rt2.shape_hash
    }

    /// Number of lanes in this block.
    pub fn lanes(&self) -> usize {
        self.rt2.width
    }

    /// The block's row storage in bytes: its varying columns and keys.
    pub fn bytes(&self) -> usize {
        let w = self.rt2.width;
        self.rt2
            .cols
            .iter()
            .map(|c| match c {
                Col::U(_) => 0,
                Col::N(_) => 4 * w,
                Col::I(_) => 8 * w,
                Col::V(_) => 16 * w,
            })
            .sum::<usize>()
            + 16 * w
    }
}

/// Per lane: does it sit on the room's win target? The room-exit test
/// (`room.x` past the start room) or, under `CELESTE_WIN_AT_XY`, the
/// synthetic player-position target. Pure position; no peeking inside.
pub fn wins_of(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    if let Some(target) = crate::interpreter::abstraction::synthetic_win_xy() {
        let Some(obj) = crate::search::pos_graph::player_object(rt2) else {
            return Ok(vec![false; lanes]);
        };
        // A position bucket (the position rung) wins where the target
        // lies inside it - the same rule as the queue's `any_win`.
        let axis = |f: u32| {
            rt2.obj_field_cell(obj, f)
                .and_then(|c| crate::search::pos_graph::whole_range_col(rt2, c))
        };
        return Ok(match (axis(ids.f_x), axis(ids.f_y)) {
            (Some(xs), Some(ys)) => xs
                .iter()
                .zip(&ys)
                .map(|(&(xl, xh), &(yl, yh))| xl <= target.0 && target.0 <= xh && yl <= target.1 && target.1 <= yh)
                .collect(),
            _ => vec![false; lanes],
        });
    }
    // The room `next_room` loads (`game_runner::win_room`): both coordinates,
    // since the exit from a row's last room wraps to the next row.
    let (wx, wy) = crate::game_runner::win_room();
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow::anyhow!("wins: no `room` global"))?;
    let is = |f: u32, name: &str, want: i16| -> Result<Vec<bool>> {
        let want = crate::pico8_num::Pico8Num::from_i16(want);
        let c = rt2.obj_field_cell(room, f).ok_or_else(|| anyhow::anyhow!("wins: room table has no {name} field"))?;
        Ok(match &rt2.cols[c as usize] {
            Col::U(AV::Num(n)) => vec![*n == want; lanes],
            Col::N(vs) => vs.iter().map(|n| *n == want).collect(),
            Col::V(vs) => vs.iter().map(|v| *v == AV::Num(want)).collect(),
            other => anyhow::bail!("wins: room.{name} is not a number column: {:?}", other),
        })
    };
    let (xs, ys) = (is(ids.f_x, "x", wx)?, is(ids.f_y, "y", wy)?);
    Ok(xs.iter().zip(&ys).map(|(a, b)| *a && *b).collect())
}

/// Worker (and owner) count: `CELESTE_THREADS`, else half the logical
/// CPUs - one per physical core; the kernels are AVX-512 bound and SMT
/// siblings share those units - and at least 1.
pub fn threads() -> usize {
    static N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *N.get_or_init(|| {
        std::env::var("CELESTE_THREADS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&n| n >= 1)
            .unwrap_or_else(|| {
                (std::thread::available_parallelism().map(|n| n.get()).unwrap_or(2) / 2).max(1)
            })
    })
}

/// A typed varying column of emitted rows, as the kernel writes it: raw
/// 16.16 words, (low, high) pairs, or a tri-state byte per bool (0 false,
/// 1 true, 2 unknown).
pub enum TCol {
    Num(Vec<u32>),
    Ival(Vec<(u32, u32)>),
    Bool(Vec<u8>),
}

/// Rows emitted for one owner at one outcome, written ONCE, typed column by
/// typed column: the outcome's skeleton (canonical structure, the uniform
/// columns), one `TCol` per varying cell, the key and cell per row. The
/// owner gathers the rows it keeps straight from here into its pieces.
pub struct Slot {
    pub shape: u64,
    /// The queue's key while live (`ForwardSink::queue`).
    pub outcome: u64,
    pub cell: u32,
    /// The rows' speed key (the bucket dispatch): a queue holds rows of
    /// one key, so a piece is runs of one key.
    pub dkey: Option<crate::trace::kernel::SpeedKey>,
    pub live: bool,
    pub touched: bool,
    skeleton: Rt2,
    /// `(cell, column)` per varying cell, in the kernel's order.
    pub cols: Vec<(usize, TCol)>,
    /// `speed_typed_cols`, computed once: the skeleton and the column layout
    /// never change after `new`, and the flush asked for it per queue flush,
    /// walking the object list each time (4% of a room (3,0) frame,
    /// 2026-09-18).
    speed_cols: Option<(usize, usize)>,
    pub keys: Vec<(u64, u64)>,
    pub cells: Vec<u32>,
    /// The row's predecessors in the slice that emitted it: the slice's
    /// first input id and a bit per lane (`ForwardSink::ids_in`).
    pub pred_base: Vec<u64>,
    pub pred_mask: Vec<u64>,
    /// Predecessors from OTHER slices of the same kernel call (the call
    /// dedups its emissions, so a row is pushed once per call and its
    /// later producers land here): `(row, slice base, lane mask)`.
    pub extra: Vec<(u32, u64, u64)>,
    /// Per row, the index of its latest `extra` entry (`u32::MAX`: none).
    /// The kernel emits slice by slice, so a row's producers from the
    /// current slice always merge into that entry.
    pub last_extra: Vec<u32>,
    /// Bumped at every flush; a row ref (`ForwardSink::row_ref`) carries
    /// the generation it was made under, so a ref into a flushed queue is
    /// recognised as stale rather than reaching another cell's rows.
    pub gen: u16,
}

/// The typed-column indices of the player's `spd.x` / `spd.y` in a slot over
/// `skeleton` with columns `cols`, when they are interval columns
/// (`Slot::speed_typed_cols`).
fn speed_cols_of(skeleton: &Rt2, cols: &[(usize, TCol)]) -> Option<(usize, usize)> {
    let ids = crate::compiled::ids();
    let obj = crate::search::pos_graph::player_object(skeleton)?;
    let pc = skeleton.obj_field_cell(obj, ids.f_spd)?;
    let Col::U(AV::Ptr(sub)) = skeleton.cols[pc as usize] else { return None };
    let cx = skeleton.obj_field_cell(sub, ids.f_x)?;
    let cy = skeleton.obj_field_cell(sub, ids.f_y)?;
    let tx = cols.iter().position(|(c, t)| *c == cx as usize && matches!(t, TCol::Ival(_)))?;
    let ty = cols.iter().position(|(c, t)| *c == cy as usize && matches!(t, TCol::Ival(_)))?;
    Some((tx, ty))
}

impl Slot {
    /// A slot over `skeleton`: a width-0 block whose varying cells hold
    /// EMPTY typed columns (`Col::N`/`Col::I` for numbers / intervals,
    /// `Col::V` for bools) and whose other cells are uniform.
    pub fn new(skeleton: Rt2) -> Self {
        let cols = skeleton
            .cols
            .iter()
            .enumerate()
            .filter_map(|(cell, c)| match c {
                Col::N(_) => Some((cell, TCol::Num(Vec::new()))),
                Col::I(_) => Some((cell, TCol::Ival(Vec::new()))),
                Col::V(_) => Some((cell, TCol::Bool(Vec::new()))),
                Col::U(_) => None,
            })
            .collect::<Vec<_>>();
        let speed_cols = speed_cols_of(&skeleton, &cols);
        Slot {
            shape: skeleton.shape_hash,
            outcome: 0,
            cell: 0,
            dkey: None,
            live: false,
            touched: false,
            skeleton,
            cols,
            speed_cols,
            keys: Vec::new(),
            cells: Vec::new(),
            pred_base: Vec::new(),
            pred_mask: Vec::new(),
            extra: Vec::new(),
            last_extra: Vec::new(),
            gen: 0,
        }
    }

    pub fn rows(&self) -> usize {
        self.keys.len()
    }

    /// Bytes allocated for this slot's rows (capacities, not lengths).
    pub fn alloc_bytes(&self) -> usize {
        self.cols
            .iter()
            .map(|(_, c)| match c {
                TCol::Num(v) => v.capacity() * 4,
                TCol::Ival(v) => v.capacity() * 8,
                TCol::Bool(v) => v.capacity(),
            })
            .sum::<usize>()
            + self.keys.capacity() * 16
            + self.cells.capacity() * 4
    }

    /// The typed-column indices (into `cols`) of the player's `spd.x` and
    /// `spd.y` when the level buckets the speed - they are interval
    /// columns then. `None` at exact speed (numbers) or without a player.
    pub fn speed_typed_cols(&self) -> Option<(usize, usize)> {
        self.speed_cols
    }

    /// Row `r`'s speed hull from the typed columns `speed_typed_cols` found.
    pub fn speed_hull(&self, tx: usize, ty: usize, r: usize) -> crate::search::door::Hull {
        let (TCol::Ival(x), TCol::Ival(y)) = (&self.cols[tx].1, &self.cols[ty].1) else { unreachable!("speed columns are intervals") };
        [x[r].0 as i32, x[r].1 as i32, y[r].0 as i32, y[r].1 as i32]
    }

    /// Drop the rows, keeping the skeleton and the columns' capacity.
    pub fn clear(&mut self) {
        for (_, c) in &mut self.cols {
            match c {
                TCol::Num(v) => v.clear(),
                TCol::Ival(v) => v.clear(),
                TCol::Bool(v) => v.clear(),
            }
        }
        self.keys.clear();
        self.cells.clear();
        self.pred_base.clear();
        self.pred_mask.clear();
        self.extra.clear();
        self.last_extra.clear();
        self.gen = self.gen.wrapping_add(1);
    }

    /// An empty block of this slot's shape for the rows to land in.
    pub fn empty_piece(&self) -> Rt2 {
        let mut b = celeste_engine::slots::reshape(&self.skeleton, 0);
        b.cols = self
            .skeleton
            .cols
            .iter()
            .map(|c| match c {
                Col::N(_) => Col::N(Vec::new()),
                Col::I(_) => Col::I(Vec::new()),
                Col::V(_) => Col::V(Vec::new()),
                u => u.clone(),
            })
            .collect();
        b.shape_hash = self.shape;
        b
    }

    /// Append `rows` of this slot to `piece` (a block of this shape).
    /// Typed columns extend the piece's typed columns directly; a cell
    /// that the piece holds differently (uniform here, varying there, or
    /// two outcomes' uniform values disagreeing) goes through `col_push`.
    pub fn gather_into(&self, piece: &mut Rt2, rows: &[u32]) {
        use celeste_engine::runtime2::col_push;
        let w = piece.width;
        let mut ti = 0;
        for (cell, c) in self.skeleton.cols.iter().enumerate() {
            if !matches!(piece.structure[cell], celeste_engine::runtime2::Cell2::Val) {
                continue;
            }
            let typed = if ti < self.cols.len() && self.cols[ti].0 == cell {
                ti += 1;
                Some(&self.cols[ti - 1].1)
            } else {
                None
            };
            let dst = &mut piece.cols[cell];
            match (typed, c) {
                (None, Col::U(v)) => {
                    if !matches!(dst, Col::U(d) if d == v) {
                        for (n, _) in rows.iter().enumerate() {
                            col_push(dst, w + n, *v);
                        }
                    }
                }
                (None, _) => unreachable!("a non-uniform skeleton column without a typed column"),
                (Some(TCol::Num(v)), _) => match dst {
                    Col::N(d) => d.extend(rows.iter().map(|&r| P8::from_raw(v[r as usize] as i32))),
                    _ => {
                        for (n, &r) in rows.iter().enumerate() {
                            col_push(dst, w + n, AV::Num(P8::from_raw(v[r as usize] as i32)));
                        }
                    }
                },
                (Some(TCol::Ival(v)), _) => match dst {
                    Col::I(d) => d.extend(rows.iter().map(|&r| {
                        let (a, b) = v[r as usize];
                        (P8::from_raw(a as i32), P8::from_raw(b as i32))
                    })),
                    _ => {
                        for (n, &r) in rows.iter().enumerate() {
                            let (a, b) = v[r as usize];
                            col_push(dst, w + n, AV::Ival(P8::from_raw(a as i32), P8::from_raw(b as i32)));
                        }
                    }
                },
                (Some(TCol::Bool(v)), _) => {
                    let av = |r: u32| match v[r as usize] {
                        0 => AV::Bool(false),
                        1 => AV::Bool(true),
                        _ => AV::UBool,
                    };
                    match dst {
                        Col::V(d) => d.extend(rows.iter().map(|&r| av(r))),
                        _ => {
                            for (n, &r) in rows.iter().enumerate() {
                                col_push(dst, w + n, av(r));
                            }
                        }
                    }
                }
            }
        }
        piece.row_keys.extend(rows.iter().map(|&r| self.keys[r as usize]));
        piece.width = w + rows.len();
    }

    /// The whole slot as a block (the ladder filter and the checks read
    /// blocks).
    pub fn to_rt2(&self) -> Rt2 {
        let mut b = self.empty_piece();
        let all: Vec<u32> = (0..self.rows() as u32).collect();
        self.gather_into(&mut b, &all);
        b
    }

    /// Does any of `rows` sit on the win target? `wins_of`'s test on the
    /// slot's columns: the target cells are uniform in the skeleton or
    /// typed numbers here.
    pub fn any_win(&self, rows: &[u32]) -> Result<bool> {
        let ids = crate::compiled::ids();
        let sk = &self.skeleton;
        let num_at = |cell: u32| -> Result<NumView<'_>> {
            match &sk.cols[cell as usize] {
                Col::U(AV::Num(n)) => Ok(NumView::Uniform(*n)),
                Col::N(_) => match self.cols.iter().find(|(c, _)| *c == cell as usize) {
                    Some((_, TCol::Num(v))) => Ok(NumView::Rows(v)),
                    _ => anyhow::bail!("any_win: cell {cell} is not a typed number column"),
                },
                other => anyhow::bail!("any_win: cell {cell} is not a number: {:?}", other),
            }
        };
        if let Some((tx, ty)) = crate::interpreter::abstraction::synthetic_win_xy() {
            let Some(obj) = crate::search::pos_graph::player_object(sk) else {
                return Ok(false);
            };
            let (Some(cx), Some(cy)) = (sk.obj_field_cell(obj, ids.f_x), sk.obj_field_cell(obj, ids.f_y)) else {
                return Ok(false);
            };
            // A position is a number, or a BUCKET under the position rung
            // (`PosPrecision`): the lane wins if the target lies in it -
            // the over-approximation a coarser level is entitled to, and
            // what the finer levels refute.
            let range_at = |cell: u32| -> Result<Box<dyn Fn(u32) -> (i16, i16) + '_>> {
                Ok(match &sk.cols[cell as usize] {
                    Col::U(AV::Num(n)) => {
                        let w = n.whole_part_as_i16();
                        Box::new(move |_| (w, w))
                    }
                    Col::U(AV::Ival(a, b)) => {
                        let (lo, hi) = (a.whole_part_as_i16(), b.whole_part_as_i16());
                        Box::new(move |_| (lo, hi))
                    }
                    Col::N(_) | Col::I(_) => match self.cols.iter().find(|(c, _)| *c == cell as usize) {
                        Some((_, TCol::Num(v))) => Box::new(move |r| {
                            let w = P8::from_raw(v[r as usize] as i32).whole_part_as_i16();
                            (w, w)
                        }),
                        Some((_, TCol::Ival(v))) => Box::new(move |r| {
                            let (a, b) = v[r as usize];
                            (P8::from_raw(a as i32).whole_part_as_i16(), P8::from_raw(b as i32).whole_part_as_i16())
                        }),
                        _ => anyhow::bail!("any_win: cell {cell} is not a typed position column"),
                    },
                    other => anyhow::bail!("any_win: cell {cell} is not a position: {:?}", other),
                })
            };
            let (xs, ys) = (range_at(cx)?, range_at(cy)?);
            return Ok(rows.iter().any(|&r| {
                let (xl, xh) = xs(r);
                let (yl, yh) = ys(r);
                xl <= tx && tx <= xh && yl <= ty && ty <= yh
            }));
        }
        let (wx, wy) = crate::game_runner::win_room();
        let (wx, wy) = (P8::from_i16(wx), P8::from_i16(wy));
        let room = sk.global_target(ids.g_room).ok_or_else(|| anyhow::anyhow!("any_win: no `room` global"))?;
        let x = sk.obj_field_cell(room, ids.f_x).ok_or_else(|| anyhow::anyhow!("any_win: room has no x"))?;
        let y = sk.obj_field_cell(room, ids.f_y).ok_or_else(|| anyhow::anyhow!("any_win: room has no y"))?;
        let (xs, ys) = (num_at(x)?, num_at(y)?);
        Ok(rows.iter().any(|&r| xs.at(r) == wx && ys.at(r) == wy))
    }

    /// Push one materialized row (the reference engine's path): its
    /// varying cells' values into the typed columns.
    pub fn push_row(&mut self, row: &Rt2, key: (u64, u64), cell: u32) {
        debug_assert_eq!(row.width, 1);
        for (c, col) in self.cols.iter_mut() {
            let v = row.cols[*c].at(0);
            match (col, v) {
                (TCol::Num(d), AV::Num(n)) => d.push(n.as_raw_u32()),
                (TCol::Ival(d), AV::Ival(a, b)) => d.push((a.as_raw_u32(), b.as_raw_u32())),
                (TCol::Ival(d), AV::Num(n)) => d.push((n.as_raw_u32(), n.as_raw_u32())),
                (TCol::Bool(d), AV::Bool(x)) => d.push(x as u8),
                (TCol::Bool(d), AV::UBool) => d.push(2),
                (_, v) => panic!("emitted row's cell {c} holds {v:?}, not its column's kind"),
            }
        }
        self.keys.push(key);
        self.cells.push(cell);
    }
}

/// A worker's per-layer edge buffer is appended to its file at this size.
const EDGE_BUF_BYTES: usize = 1 << 20;
/// Slots of the direct-mapped edge merge cache (`ForwardSink::direct_edge`).
const DIRECT_SLOTS: usize = 1 << 12;

/// Append one record to the target layer's buffer (`ForwardSink::edge_bufs`),
/// writing the buffer out when full.
#[inline]
fn append_record(
    edge_bufs: &mut Vec<Vec<u8>>,
    edge_records: &mut u64,
    dir: &std::path::Path,
    frame: u32,
    worker: u32,
    target: u64,
    base: u64,
    mask: u64,
) -> Result<()> {
    let layer = id_layer(target) as usize;
    if edge_bufs.len() <= layer {
        edge_bufs.resize_with(layer + 1, Vec::new);
    }
    let buf = &mut edge_bufs[layer];
    crate::search::edges::encode_record(buf, target, base, mask);
    *edge_records += 1;
    if buf.len() >= EDGE_BUF_BYTES {
        write_edges(dir, frame, layer, worker, buf)?;
    }
    Ok(())
}

/// Append `buf` to the worker's raw file for `layer` at `frame`
/// (`edges::raw_path`) and clear it.
fn write_edges(dir: &std::path::Path, frame: u32, layer: usize, worker: u32, buf: &mut Vec<u8>) -> Result<()> {
    use std::io::Write;
    let path = crate::search::edges::raw_path(dir, frame, layer as u32, worker);
    std::fs::create_dir_all(path.parent().expect("a raw dir"))?;
    let mut f = std::fs::OpenOptions::new().create(true).append(true).open(path)?;
    f.write_all(buf)?;
    buf.clear();
    Ok(())
}

/// The backward's target set - the marks of the previous iteration as
/// `(key, cell)` - with a bitmap in front of it: one bit per low-22-bit
/// slice of `key.0` (512 KB: L2-resident, read-only), so the
/// ~90% of emissions that are not targets cost one bit test instead of a
/// probe into a set that lives in L3.
pub struct TargetSet {
    set: rustc_hash::FxHashSet<(u64, u64, u32)>,
    bits: Vec<u64>,
}

impl TargetSet {
    const BITS: u32 = 22;

    pub fn new(targets: impl Iterator<Item = ((u64, u64), u32)>) -> Self {
        let mut bits = vec![0u64; 1 << (Self::BITS - 6)];
        let mut set = rustc_hash::FxHashSet::default();
        for (k, c) in targets {
            let b = (k.0 & ((1 << Self::BITS) - 1)) as usize;
            bits[b >> 6] |= 1 << (b & 63);
            set.insert((k.0, k.1, c));
        }
        TargetSet { set, bits }
    }

    #[inline(always)]
    pub fn contains(&self, key: (u64, u64), cell: u32) -> bool {
        let b = (key.0 & ((1 << Self::BITS) - 1)) as usize;
        self.bits[b >> 6] >> (b & 63) & 1 == 1 && self.set.contains(&(key.0, key.1, cell))
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }
}

/// A number column of a slot as `any_win` reads it.
enum NumView<'a> {
    Uniform(P8),
    Rows(&'a [u32]),
}

impl NumView<'_> {
    fn at(&self, row: u32) -> P8 {
        match self {
            NumView::Uniform(n) => *n,
            NumView::Rows(v) => P8::from_raw(v[row as usize] as i32),
        }
    }
}

/// Rows per queue (plans/waves.md): a queue that fills is flushed. Small
/// on purpose - a worker's whole pool stays cache-resident, which the
/// prototype measured as the difference between ~1000 and ~350 thread-ms
/// of push time per frame.
pub const QUEUE_ROWS: usize = 256;
/// Live queues per worker; a new (outcome, cell) beyond this evicts the
/// least recently touched queue (second chance). Eviction only decides
/// how many flushes happen: a flush is idempotent.
pub const POOL_QUEUES: usize = 256;

/// One worker's context for a frame: what the frame step emits into. The
/// step consumes provenance at emission - it knows which input row each
/// output row came from - and reports through this: the pos-graph edges
/// `(input cell, output cell)` of every raw output, and the output rows
/// themselves, keyed, into a QUEUE per (outcome, cell). A queue that
/// fills (or is evicted from the pool) is FLUSHED right here, by this
/// worker: rows the ladder filter rejects dropped, the rest sorted,
/// admitted at the door (`search::door`, the one shared structure, locked
/// per shard for the admission only), and the survivors appended into
/// this worker's piece of the shape. In backward mode (`targets`) nothing
/// is materialized: an emitted row that is a target marks its input row.
pub struct ForwardSink<'a> {
    /// The pool: every queue ever created by this sink, live or spare.
    pub slots: Vec<Slot>,
    /// (outcome id, cell) -> live queue. The outcome id is the emitter's:
    /// a kernel's template address, the reference engine's shape hash.
    index: rustc_hash::FxHashMap<(u64, u32, Option<crate::trace::kernel::SpeedKey>), u32>,
    /// Flushed, empty queues per outcome id, keeping their columns.
    spare: rustc_hash::FxHashMap<u64, Vec<u32>>,
    /// The second-chance hand over `slots`.
    clock: usize,
    /// The run cache: rows arrive in runs of one (outcome, cell).
    last: ((u64, u32, Option<crate::trace::kernel::SpeedKey>), u32),
    door: Option<&'a crate::search::door::Door>,
    filter: Option<&'a MarkFilter<'a>>,
    /// The ids of the block being run, per lane (set by the worker before
    /// each unit); the step reads `ids_in[lo]` as the slice's base and
    /// records predecessors as masks over the slice.
    pub ids_in: Option<&'a [u64]>,
    /// Lanes of the block being run that must not be expanded.
    pub skip_in: Option<&'a [bool]>,
    /// Where the edges go: `<dir>/l{target layer}/w{worker}.bin`, records
    /// of `(target id u64, slice base id u64, lane mask u16)`, appended as
    /// the per-layer buffers fill. `None`: no recording.
    edges_dir: Option<std::path::PathBuf>,
    edge_bufs: Vec<Vec<u8>>,
    pub edge_records: u64,
    /// The within-call dedup cache (`RowCache`), written back by the flush.
    pub seen: celeste_engine::kernel::RowCache,
    /// Direct-mapped merge of the edges to already-flushed states:
    /// `(target, base, mask)` per slot, an entry evicted or drained at the
    /// call's end becomes a record.
    direct: Vec<(u64, u64, u64)>,
    /// Time spent encoding and writing edge records (this worker).
    pub t_edges: std::time::Duration,
    /// Time spent in the ladder filter (this worker).
    pub t_filter: std::time::Duration,

    /// This worker's next-frame rows, one piece per shape: the block, its
    /// file seq in the layer (`worker * 256 + k`), and its rows' ids.
    pieces: rustc_hash::FxHashMap<u64, (Rt2, u32, Vec<u64>)>,
    /// The frame being computed (the layer the new rows belong to) and
    /// this worker's index: what a new row's id is made of.
    frame: u32,
    worker: u32,
    pub won: bool,
    pub kept: usize,
    pub flushes: u64,
    pub flushed_rows: u64,
    sort_buf: Vec<((u64, u64), u32)>,
    keys_buf: Vec<(u64, u64)>,
    /// Per distinct key of a flush, its rows' speed hull; and per new key,
    /// the hull the emitted row carries (`door::Admit::admit`).
    hull_buf: Vec<crate::search::door::Hull>,
    new_hulls: Vec<crate::search::door::Hull>,
    uniq_buf: Vec<u32>,
    row_uniq: Vec<u32>,
    new_buf: Vec<u32>,
    ids_buf: Vec<u64>,
    rows_buf: Vec<u32>,
    /// Record `edges` at all (off for the backward, which has the graph).
    pub edges_on: bool,
    /// The distinct edges this worker produced (a set: a block's rows fan
    /// out into a few hundred distinct cell pairs, not one per row).
    pub edges: rustc_hash::FxHashSet<(u32, u32)>,
    /// Rows emitted after the step's within-call dedup (the raw fan-out).
    pub emitted: u64,
    /// BACKWARD MODE: the marked `(shape, content, cell)` set at frame i+1.
    /// When set, the step materializes NOTHING; an emitted row that is in
    /// the set marks its INPUT row (`hit`). Provenance consumed at
    /// emission, as in the forward - the step's within-call dedup carries
    /// "hit" as its tag so a re-emission from another input row marks that
    /// row too.
    pub targets: Option<&'a TargetSet>,
    /// Backward mode: one flag per input row of the lane range being run.
    hits: Vec<bool>,
    hit_base: usize,
}

const NO_QUEUE: ((u64, u32, Option<crate::trace::kernel::SpeedKey>), u32) = ((u64::MAX, u32::MAX, None), u32::MAX);

impl<'a> ForwardSink<'a> {
    fn empty(edges_on: bool) -> Self {
        ForwardSink {
            slots: Vec::new(),
            index: Default::default(),
            spare: Default::default(),
            clock: 0,
            last: NO_QUEUE,
            door: None,
            filter: None,
            ids_in: None,
            skip_in: None,
            edges_dir: None,
            edge_bufs: Vec::new(),
            edge_records: 0,
            seen: celeste_engine::kernel::RowCache::new(),
            direct: Vec::new(),
            t_edges: std::time::Duration::ZERO,
            t_filter: std::time::Duration::ZERO,
            pieces: Default::default(),
            frame: 0,
            worker: 0,
            won: false,
            kept: 0,
            flushes: 0,
            flushed_rows: 0,
            sort_buf: Vec::with_capacity(QUEUE_ROWS),
            keys_buf: Vec::with_capacity(QUEUE_ROWS),
            hull_buf: Vec::with_capacity(QUEUE_ROWS),
            new_hulls: Vec::with_capacity(QUEUE_ROWS),
            uniq_buf: Vec::with_capacity(QUEUE_ROWS),
            row_uniq: Vec::with_capacity(QUEUE_ROWS),
            new_buf: Vec::with_capacity(QUEUE_ROWS),
            ids_buf: Vec::with_capacity(QUEUE_ROWS),
            rows_buf: Vec::with_capacity(QUEUE_ROWS),
            edges_on,
            edges: Default::default(),
            emitted: 0,
            targets: None,
            hits: Vec::new(),
            hit_base: 0,
        }
    }

    /// Worker `worker`'s sink for `frame`: flushes through `door` (and
    /// `filter`); its pieces' rows get ids in layer `frame`.
    pub fn forward(
        door: &'a crate::search::door::Door,
        filter: Option<&'a MarkFilter<'a>>,
        edges_on: bool,
        frame: u32,
        worker: u32,
        edges_dir: Option<&std::path::Path>,
    ) -> Self {
        let mut s = Self::empty(edges_on);
        s.door = Some(door);
        s.filter = filter;
        s.frame = frame;
        s.worker = worker;
        s.edges_dir = edges_dir.map(|p| p.to_path_buf());
        s
    }

    /// One edge record: `target` (a state id) has the lanes of `mask` in
    /// the slice based at `base` as predecessors.
    #[inline]
    fn record(&mut self, target: u64, base: u64, mask: u64) -> Result<()> {
        let dir = self.edges_dir.as_deref().expect("recording without an edges dir");
        append_record(&mut self.edge_bufs, &mut self.edge_records, dir, self.frame, self.worker, target, base, mask)
    }

    /// Lane `lane` of the slice based at `base` produced the (already
    /// flushed) state `target`: merged with the slice's other lanes in
    /// the direct-mapped cache, recorded on eviction.
    #[inline]
    pub fn direct_edge(&mut self, target: u64, base: u64, lane: usize) {
        if self.edges_dir.is_none() {
            return;
        }
        if self.direct.is_empty() {
            self.direct = vec![(u64::MAX, 0, 0); DIRECT_SLOTS];
        }
        let i = (celeste_engine::runtime2::mix64(target ^ base.rotate_left(17)) as usize) & (DIRECT_SLOTS - 1);
        let e = self.direct[i];
        if e.0 == target && e.1 == base {
            self.direct[i].2 |= 1u64 << lane;
            return;
        }
        if e.0 != u64::MAX {
            self.record(e.0, e.1, e.2).expect("recording an edge");
        }
        self.direct[i] = (target, base, 1u64 << lane);
    }

    /// The step's end of a kernel call: the merge cache drains.
    pub fn end_call(&mut self) {
        for i in 0..self.direct.len() {
            let e = self.direct[i];
            if e.0 != u64::MAX {
                self.record(e.0, e.1, e.2).expect("recording an edge");
                self.direct[i] = (u64::MAX, 0, 0);
            }
        }
    }

    /// The step's handle on the row it just pushed into queue `q` (its
    /// last row): the row (8 bits), the queue (24 bits: the pool is
    /// `POOL_QUEUES` LIVE queues, but spares are kept per outcome so the
    /// slot vector grows past that - room (1,0) f65 had a queue index >=
    /// 256 alias another queue's row, 2026-09-14) and the queue's
    /// generation (16 bits), below the cache's flag bits.
    #[inline]
    pub fn row_ref(&self, q: usize) -> u64 {
        const _: () = assert!(QUEUE_ROWS <= 256);
        assert!(q < 1 << 24, "queue pool past 2^24 slots");
        let s = &self.slots[q];
        ((s.gen as u64) << 32) | ((q as u64) << 8) | (s.rows() as u64 - 1)
    }

    /// Lane `lane` of the slice based at `base` also produced the row
    /// `row_ref` points at. False if that queue was flushed since (the row
    /// is gone; the caller pushes the row again).
    #[inline]
    pub fn mark_pred(&mut self, row_ref: u64, base: u64, lane: usize) -> bool {
        let (gen, q, r) = ((row_ref >> 32) as u16, ((row_ref >> 8) & 0xff_ffff) as usize, (row_ref & 0xff) as usize);
        let s = &mut self.slots[q];
        if !s.live || s.gen != gen || r >= s.pred_mask.len() {
            return false;
        }
        if s.pred_base[r] == base {
            s.pred_mask[r] |= 1u64 << lane;
            return true;
        }
        let last = s.last_extra[r];
        if last != u32::MAX && s.extra[last as usize].1 == base {
            s.extra[last as usize].2 |= 1u64 << lane;
        } else {
            s.last_extra[r] = s.extra.len() as u32;
            s.extra.push((r as u32, base, 1u64 << lane));
        }
        true
    }

    /// THE SPEED HULL at the call's dedup cache: a lane re-emitting a
    /// queued row's key with another speed fragment widens that row's
    /// hull in place (columns `tx`/`ty`, `Slot::speed_typed_cols`). False
    /// if the queue was flushed since (the ref is stale).
    pub fn hull_union_at(&mut self, row_ref: u64, tx: usize, ty: usize, h: crate::search::door::Hull) -> bool {
        let (gen, q, r) = ((row_ref >> 32) as u16, ((row_ref >> 8) & 0xff_ffff) as usize, (row_ref & 0xff) as usize);
        let s = &mut self.slots[q];
        if !s.live || s.gen != gen || r >= s.pred_mask.len() {
            return false;
        }
        if let TCol::Ival(x) = &mut s.cols[tx].1 {
            x[r] = ((x[r].0 as i32).min(h[0]) as u32, (x[r].1 as i32).max(h[1]) as u32);
        }
        if let TCol::Ival(y) = &mut s.cols[ty].1 {
            y[r] = ((y[r].0 as i32).min(h[2]) as u32, (y[r].1 as i32).max(h[3]) as u32);
        }
        true
    }

    /// The backward's sink for the candidate rows `lanes` of a block.
    pub fn backward(targets: &'a TargetSet, lanes: Range<usize>) -> Self {
        let mut s = Self::empty(false);
        s.targets = Some(targets);
        s.hits = vec![false; lanes.len()];
        s.hit_base = lanes.start;
        s
    }

    /// Backward mode: input row `lane` (a block lane index) reaches a target.
    pub fn hit(&mut self, lane: usize) {
        self.hits[lane - self.hit_base] = true;
    }

    /// Backward mode: the hit flags, one per lane of the range run.
    pub fn hits(&self) -> &[bool] {
        &self.hits
    }

    /// The live queue for `(outcome, cell)`, created over the skeleton
    /// `init` returns (or a spare of the outcome) on first use, evicting
    /// the least recently touched queue when the pool is full.
    pub fn queue(&mut self, outcome: u64, cell: u32, dkey: Option<crate::trace::kernel::SpeedKey>, init: impl FnOnce() -> Rt2) -> usize {
        if self.last.0 == (outcome, cell, dkey) {
            return self.last.1 as usize;
        }
        let q = match self.index.get(&(outcome, cell, dkey)) {
            Some(&q) => q,
            None => {
                if self.index.len() >= POOL_QUEUES {
                    self.evict_one();
                }
                let q = match self.spare.get_mut(&outcome).and_then(Vec::pop) {
                    Some(q) => q,
                    None => {
                        self.slots.push(Slot::new(init()));
                        (self.slots.len() - 1) as u32
                    }
                };
                let s = &mut self.slots[q as usize];
                s.outcome = outcome;
                s.cell = cell;
                s.dkey = dkey;
                s.live = true;
                s.touched = false;
                self.index.insert((outcome, cell, dkey), q);
                q
            }
        };
        self.last = ((outcome, cell, dkey), q);
        q as usize
    }

    /// Second chance over the pool: skip (and clear) the touched queues,
    /// flush the first untouched live one.
    fn evict_one(&mut self) {
        loop {
            let i = self.clock % self.slots.len();
            self.clock += 1;
            let s = &mut self.slots[i];
            if !s.live {
                continue;
            }
            if s.touched {
                s.touched = false;
                continue;
            }
            self.flush(i).expect("flushing an evicted queue");
            return;
        }
    }

    /// After a push into queue `q`: flush it when full.
    #[inline]
    pub fn pushed(&mut self, q: usize) -> Result<()> {
        let s = &mut self.slots[q];
        s.touched = true;
        if s.rows() >= QUEUE_ROWS {
            self.flush(q)?;
        }
        Ok(())
    }

    /// Flush queue `q`: filter, sort (collapsing in-queue duplicates),
    /// admit at the door, gather the admitted rows into this worker's
    /// piece of the shape; the queue goes back to its outcome's spares.
    fn flush(&mut self, q: usize) -> Result<()> {
        let door = self.door.expect("flush without a door");
        let slot = &mut self.slots[q];
        let n = slot.rows();
        if n > 0 {
            crate::compiled::asm_kernel::key_check(slot);
            self.flushes += 1;
            self.flushed_rows += n as u64;
            // Ladder filter (coarser level's marked set) first: a
            // filtered-out row is never admitted.
            let allow = match self.filter {
                Some(f) => {
                    let t_f = std::time::Instant::now();
                    let a = f.allowed(&slot.to_rt2(), self.frame)?;
                    self.t_filter += t_f.elapsed();
                    Some(a)
                }
                None => None,
            };
            // EXPERIMENT (not a default, not sound as a proof): the time band.
            // A queue is one player cell; drop it when even the fastest recorded
            // climb cannot reach the exit by the horizon.
            let allow = match band() {
                Some((h, px)) if cell_too_late(slot.cell, self.frame, h, px) => Some(vec![false; n]),
                _ => allow,
            };
            // The level -1 filter: the queue's cell provably cannot exit by H.
            let allow = match level_minus_one() {
                Some((h, table)) if table.too_late(slot.shape, slot.cell, self.frame, h) => Some(vec![false; n]),
                _ => allow,
            };
            self.sort_buf.clear();
            self.sort_buf.extend(
                slot.keys
                    .iter()
                    .enumerate()
                    .filter(|(r, _)| allow.as_ref().is_none_or(|a| a[*r]))
                    .map(|(r, k)| (*k, r as u32)),
            );
            self.sort_buf.sort_unstable();
            // The distinct keys, and each row's index among them: rows
            // sharing a key are one state with several predecessor masks.
            self.keys_buf.clear();
            self.uniq_buf.clear();
            for e in &self.sort_buf {
                if self.keys_buf.last() != Some(&e.0) {
                    self.keys_buf.push(e.0);
                }
                self.uniq_buf.push(self.keys_buf.len() as u32 - 1);
            }
            // Per ROW, its index among the distinct keys (`u32::MAX` if
            // filtered out): what the extra edges are resolved through.
            self.row_uniq.clear();
            self.row_uniq.resize(n, u32::MAX);
            for (e, &u) in self.sort_buf.iter().zip(&self.uniq_buf) {
                self.row_uniq[e.1 as usize] = u;
            }
            self.new_buf.clear();
            self.ids_buf.clear();
            self.new_hulls.clear();
            // THE SPEED HULL: per distinct key, the union of its rows'
            // speed intervals (`door::Hull`), when the level buckets the
            // speed (the slot's speed columns are intervals).
            let spd_cols = slot.speed_typed_cols();
            self.hull_buf.clear();
            if let Some((cx, cy)) = spd_cols {
                self.hull_buf.resize(self.keys_buf.len(), crate::search::door::NO_HULL);
                for (e, &u) in self.sort_buf.iter().zip(&self.uniq_buf) {
                    let h = slot.speed_hull(cx, cy, e.1 as usize);
                    self.hull_buf[u as usize] = crate::search::door::hull_union(&self.hull_buf[u as usize], &h);
                }
            }
            // The piece the new rows land in, and the id the first of them
            // gets: the door hands the k-th new key `first_new + k`, and the
            // rows are appended in that same order.
            let n_pieces = self.pieces.len() as u32;
            let (frame, worker) = (self.frame, self.worker);
            let (piece, seq, ids) = self
                .pieces
                .entry(slot.shape)
                .or_insert_with(|| (slot.empty_piece(), worker * 256 + n_pieces, Vec::new()));
            let first_new = pack_id(frame, *seq, piece.width as u32);
            door.admit(
                slot.shape,
                slot.cell,
                &self.keys_buf,
                first_new,
                &mut self.ids_buf,
                &mut self.new_buf,
                spd_cols.map(|_| &self.hull_buf[..]),
                &mut self.new_hulls,
            );
            // The edges: every row (each a predecessor mask) to its state,
            // plus the extra masks. Rows without ids (a step run on
            // id-less blocks, e.g. the tests' reference engine) have no
            // predecessor columns and record nothing.
            if let Some(edges_dir) = self.edges_dir.as_deref().filter(|_| slot.pred_base.len() == n) {
                let t_e = std::time::Instant::now();
                let (row_uniq, ids_buf) = (&self.row_uniq, &self.ids_buf);
                let (edge_bufs, edge_records, frame, worker) = (&mut self.edge_bufs, &mut self.edge_records, self.frame, self.worker);
                let rows = slot.pred_base.iter().zip(&slot.pred_mask).enumerate().map(|(r, (&b, &m))| (r as u32, b, m));
                for (r, b, m) in rows.chain(slot.extra.iter().copied()) {
                    let u = row_uniq[r as usize];
                    if u != u32::MAX {
                        append_record(edge_bufs, edge_records, edges_dir, frame, worker, ids_buf[u as usize], b, m)?;
                    }
                }
                // Write each row's fate back into the dedup cache: a later
                // re-emission in this call records an edge to the id
                // directly (or nothing, for a filtered-out row).
                for (r, key) in slot.keys.iter().enumerate() {
                    let u = row_uniq[r];
                    let v = if u == u32::MAX {
                        celeste_engine::kernel::RowCache::DROP_FLAG
                    } else {
                        celeste_engine::kernel::RowCache::ID_FLAG | ids_buf[u as usize]
                    };
                    self.seen.set_ref(*key, v);
                }
                self.t_edges += t_e.elapsed();
            }
            if !self.new_buf.is_empty() {
                self.rows_buf.clear();
                // The first row of a new key's group is the one kept.
                self.rows_buf.extend(self.new_buf.iter().map(|&u| {
                    let first = self.uniq_buf.partition_point(|&x| x < u);
                    self.sort_buf[first].1
                }));
                self.kept += self.rows_buf.len();
                self.won |= slot.any_win(&self.rows_buf)?;
                let w0 = piece.width;
                slot.gather_into(piece, &self.rows_buf);
                // An emitted row carries the hull the door handed back:
                // its own for a new key, the union for a grown one.
                if let Some((cx, cy)) = spd_cols {
                    let (cx, cy) = (slot.cols[cx].0, slot.cols[cy].0);
                    for (k, h) in self.new_hulls.iter().enumerate() {
                        celeste_engine::runtime2::col_set(&mut piece.cols[cx], piece.width, w0 + k, AV::Ival(P8::from_raw(h[0]), P8::from_raw(h[1])));
                        celeste_engine::runtime2::col_set(&mut piece.cols[cy], piece.width, w0 + k, AV::Ival(P8::from_raw(h[2]), P8::from_raw(h[3])));
                    }
                }
                ids.extend((0..self.rows_buf.len() as u32).map(|k| first_new + k as u64));
                debug_assert_eq!(ids.len(), piece.width);
            }
            slot.clear();
        }
        slot.live = false;
        slot.touched = false;
        self.index.remove(&(slot.outcome, slot.cell, slot.dkey));
        self.spare.entry(slot.outcome).or_default().push(q as u32);
        if self.last.1 == q as u32 {
            self.last = NO_QUEUE;
        }
        Ok(())
    }

    /// End of the worker's frame: flush every live queue, hand back the
    /// pieces. The counters (`kept`, `won`, `flushes`, ...) are final only
    /// after this.
    pub fn finish(&mut self) -> Result<Vec<Block>> {
        for q in 0..self.slots.len() {
            if self.slots[q].live {
                self.flush(q)?;
            }
        }
        if let Some(dir) = self.edges_dir.clone() {
            for (layer, buf) in self.edge_bufs.iter_mut().enumerate() {
                if !buf.is_empty() {
                    write_edges(&dir, self.frame, layer, self.worker, buf)?;
                }
            }
        }
        let mut out = Vec::new();
        for (mut p, seq, ids) in std::mem::take(&mut self.pieces).into_values() {
            // A piece a flush opened but never appended to (every key it
            // admitted was old) is nothing: an empty block would be
            // id-less and get renumbered at the checkpoint, colliding
            // with a real seq of this layer (room (1,0) f89, 2026-09-13).
            if p.width == 0 {
                continue;
            }
            debug_assert_eq!(ids.len(), p.width);
            // A queue's columns are typed by its skeleton, not by its rows: a
            // column every row agrees on goes back to the uniform it is (the
            // old append step's rule; the State bridge, which the reference
            // engine's rows cross, has no per-lane form for an unknown bool).
            for c in p.cols.iter_mut() {
                if !matches!(c, Col::U(_)) {
                    let taken = std::mem::replace(c, Col::U(AV::Nil));
                    *c = celeste_engine::runtime2::collapse_uniform(taken);
                }
            }
            out.push(Block::with_ids(p, ids, seq));
        }
        Ok(out)
    }

    /// Bytes allocated across the pool (capacities).
    pub fn alloc_bytes(&self) -> usize {
        self.slots.iter().map(Slot::alloc_bytes).sum()
    }

    /// Emit one materialized single-row block at `cell` (the reference
    /// engine's path; the kernels push into the queues directly).
    pub fn emit_row(&mut self, row: &Rt2, cell: u32) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        debug_assert_eq!(row.row_keys.len(), 1, "an emitted row carries its key");
        let dkey = crate::compiled::asm_kernel::speed_key_of_row(row, 0);
        let q = self.queue(row.shape_hash, cell, dkey, || {
            // The skeleton from the row itself: numeric and boolean cells
            // vary (typed, empty), the rest is uniform - which every row
            // of a shape agrees on, the shape hash being the structure.
            let mut b = celeste_engine::slots::reshape(row, 0);
            b.cols = row
                .cols
                .iter()
                .map(|c| match c.at(0) {
                    AV::Num(_) => Col::N(Vec::new()),
                    AV::Ival(..) => Col::I(Vec::new()),
                    AV::Bool(_) | AV::UBool => Col::V(Vec::new()),
                    v => Col::U(v),
                })
                .collect();
            b.shape_hash = row.shape_hash;
            b
        });
        self.slots[q].push_row(row, row.row_keys[0], cell);
        self.emitted += 1;
        self.pushed(q)
    }
}


/// EXPERIMENT: `CELESTE_BAND="H,px"`, the time band. `px` is the largest upward
/// movement per frame (8 on room (2,0)'s recorded transitions: a measurement,
/// not a proven bound), `H` the horizon. Unset: no band.
fn band() -> Option<(u32, i32)> {
    static BAND: std::sync::OnceLock<Option<(u32, i32)>> = std::sync::OnceLock::new();
    *BAND.get_or_init(|| {
        let s = std::env::var("CELESTE_BAND").ok()?;
        let (h, px) = s.split_once(',').expect("CELESTE_BAND=\"H,px\"");
        let band = (h.trim().parse().expect("CELESTE_BAND horizon"), px.trim().parse().expect("CELESTE_BAND px per frame"));
        eprintln!("[band] EXPERIMENT: dropping cells that cannot climb to the exit by f{} at {} px per frame", band.0, band.1);
        Some(band)
    })
}

/// THE LEVEL -1 FILTER (`CELESTE_LEVEL_MINUS_ONE="H,S"`, plans/level-minus-one.md):
/// drop a queue (one player cell) when the level -1 table proves its rows
/// cannot exit by horizon H (`trace::level_minus_one::CostToGo::too_late`).
/// Unlike the band it is derived from the traced frames: the table's ranges
/// are inductive, a clipped successor counts as a possible exit and a death as
/// a respawn. H must be the LARGEST horizon the run tests - level 0 persists
/// across horizons - so this is for a `--ceiling` search with H = the ceiling.
/// Built once, at the first flush, on a thread with the tracer's stack.
fn level_minus_one() -> Option<(u32, &'static crate::trace::level_minus_one::CostToGo)> {
    static TABLE: std::sync::OnceLock<Option<(u32, crate::trace::level_minus_one::CostToGo)>> = std::sync::OnceLock::new();
    TABLE
        .get_or_init(|| {
            let s = std::env::var("CELESTE_LEVEL_MINUS_ONE").ok()?;
            let (h, sp) = s.split_once(',').expect("CELESTE_LEVEL_MINUS_ONE=\"H,S\"");
            let h: u32 = h.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE horizon");
            let sp: i32 = sp.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE speed bound (px per frame)");
            let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
            let threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
            let t = std::time::Instant::now();
            let table = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || crate::trace::level_minus_one::cost_to_go(std::path::Path::new(&root), sp, threads, h))
                .expect("spawn the level -1 builder")
                .join()
                .expect("the level -1 builder panicked")
                .unwrap_or_else(|e| panic!("building the level -1 table: {e:#}"));
            eprintln!(
                "[level -1] table built in {:.1} s (S = {sp}): the start state's d = {}; dropping cells that cannot exit by f{h}",
                t.elapsed().as_secs_f64(),
                table.start_d
            );
            Some((h, table))
        })
        .as_ref()
        .map(|(h, t)| (*h, t))
}

/// Can a player at `cell` at frame `frame` not reach the exit (y < -4) by
/// horizon `h`, at `px` pixels per frame up? One frame of slack for when the
/// exit test runs. A cell with no player (a death) is kept, and so is a cell
/// past the start room: the position grid places the next room one room to
/// the right (`pos_graph`), so x >= 128 is a row that has LEFT the room - a
/// win, which a band reading its new-room y dropped (room (1,0) refuted 99 at
/// level 0, room (2,0) 95 at level 1, 2026-09-16).
pub(crate) fn cell_too_late(cell: u32, frame: u32, h: u32, px: i32) -> bool {
    let Some((x, y)) = crate::search::pos_graph::cell_xy(cell) else { return false };
    if x >= 128 {
        return false;
    }
    let frames = ((y + 5).max(0) + px - 1) / px;
    frame + (frames.max(1) - 1) as u32 > h
}

/// The ladder's forward discard-filter. A state generated at precision r+1 is
/// KEPT only if its widened-to-precision-r form was marked by the previous
/// level's backward pass - "immediately, during the forward, the whole time".
/// The marked set is coarse, so this narrows every finer level to the coarse
/// winning envelope. This is what replaces the e/g/band numbering as the
/// cross-precision link: a set membership, not a distance threshold.
pub struct MarkFilter<'a> {
    /// The marked set from the previous, COARSER precision level.
    marked: &'a Visited,
    /// The coarser LEVEL to widen down to before the membership test.
    coarser: crate::interpreter::abstraction::Level,
}

impl<'a> MarkFilter<'a> {
    pub fn new(
        marked: &'a Visited,
        coarser: crate::interpreter::abstraction::Level,
    ) -> Self {
        Self { marked, coarser }
    }

    /// Per-lane: keep lane `i` of a block at `frame` iff its widened-to-coarser
    /// form was marked with a deadline of `frame` or later. The time bound is
    /// sound because the coarse level over-approximates: a fine state that
    /// still wins by the horizon from `frame` widens to a coarse state that
    /// does too - whose deadline is then >= `frame`. The membership test
    /// alone admitted a fine state at f60 onto a coarse state marked from an
    /// earlier frame with no time left (room (3,0) level 1: 14.6M kept at
    /// f51 onto 456k layer-51 marks, 2026-09-19).
    /// Rem widening never moves the integer cell and (coarsening) never splits
    /// a lane, so the widened block is lane-aligned with the input.
    ///
    /// Crosses to the interpreter `State` for the coarsening: the widenings
    /// live there (`abstraction.rs`) and this is the one place the loop
    /// still needs them. Paid only at levels >= 1, whose frontiers the
    /// filter itself keeps small.
    pub fn allowed(&self, rt2: &Rt2, frame: u32) -> Result<Vec<bool>> {
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser)?;
        Ok(keys
            .iter()
            .zip(&cells)
            .map(|(k, &c)| self.marked.deadline(shape, *k, c).is_some_and(|d| u32::from(d) >= frame))
            .collect())
    }
}

/// Each lane's `(key, cell)` as the `coarser` level keys it - what
/// `MarkFilter` looks up. At `Bits(k)` that is the COMPLETE coarsening the
/// level's kernels bake into their rows (`Rt2::widen_to`: rem bucketing,
/// the dash clamp, the fruit widening, the timer pins), then the canonical
/// key - on the block's columns, lane for lane. At `Exact` it is the plain
/// canonical key: the Exact kernel set (`WalkOpts::EXACT`) widens NOTHING,
/// its rows are the concrete state, timers included. Public for the ladder
/// diagnostics and the witness extraction.
pub fn widened_keys(
    block: &Block,
    coarser: crate::interpreter::abstraction::Level,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    widened_keys_rt2(&block.rt2, coarser)
}

/// The speed bucket (log2 raw units) a level widens to, `None` for exact.
/// The speed table width a level buckets at, and whether it buckets y as
/// well as x (`Rt2::widen_to`'s speed step).
pub fn spd_width_log2(spd: crate::interpreter::abstraction::SpdPrecision) -> Option<(u8, bool)> {
    match spd.width_log2() {
        Some(w) => Some((w, spd.buckets_y())),
        None => None,
    }
}

/// `(shape, keys, cells)` of the widened rows - the shape is the widened
/// block's, which is what the coarser level's marks are sharded by.
pub fn widened_keys_rt2(
    rt2: &Rt2,
    coarser: crate::interpreter::abstraction::Level,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    use crate::interpreter::abstraction::RemPrecision;
    let mut w = rt2.clone_block();
    if let RemPrecision::Bits(b) = coarser.rem {
        w.widen_to(crate::compiled::ids(), b, spd_width_log2(coarser.spd), (coarser.pos.x, coarser.pos.y), coarser.held.is_unknown(), coarser.fruit.is_unknown(), coarser.floors.is_unknown());
    }
    let keys = w.row_keys_canonical();
    let cells = crate::search::pos_graph::block_cells(&w)?;
    anyhow::ensure!(
        keys.len() == rt2.width && cells.len() == rt2.width,
        "the widening changed the lane count"
    );
    Ok((w.shape_hash, keys, cells))
}

/// The one interface between the outer loop and the engines (interface #1).
/// `run` takes a block of input lanes and returns every output block - all
/// internal branching enumerated, already widened for the engine's precision,
/// each output block's key/position columns already computable. Branching,
/// widening, and keying happen INSIDE.
///
/// Two implementations behind this one trait: the compiled kernels (fast) and
/// the scalar interpreter (`trace::refengine`, the trusted reference). The loop
/// holds a `&dyn FrameStep` and never knows which.
pub trait FrameStep: Sync {
    /// One frame of lanes `lanes` of `block` (whose per-lane cells are
    /// `cell_in`), emitted into `sink`. Called from many workers at once on
    /// disjoint lane ranges: an implementation keeps its scratch in the
    /// sink or behind a lock.
    fn run(&self, block: &Block, cell_in: &[u32], lanes: Range<usize>, sink: &mut ForwardSink) -> Result<()>;
}

/// The emit phase's unit: one kernel call covers at most this many lanes of
/// one block, and a worker pulls one unit at a time, so the step's within-call
/// dedup (`seen`) spans a unit. `CELESTE_UNIT_LANES` overrides it for a
/// measurement (a multiple of 64: slices and predecessor groups start at
/// multiples of 64).
///
/// 1024: room (3,0) f44 at `r0sxhfb` (2026-09-18, `bench-frame`, 16 threads)
/// took 2.23 s with 45% of the workers' time idle at 16384 lanes (a few units
/// of the heavy kernels outlasted the rest), 1.81 s / 28% at 4096, 1.47 s / 9%
/// at 1024, 1.53 s / 4% at 256, where the smaller dedup window let more
/// duplicates through (6.10M raw rows against 6.02M). Same kept set at all.
fn unit_lanes() -> usize {
    static N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *N.get_or_init(|| match std::env::var("CELESTE_UNIT_LANES") {
        Ok(v) => {
            let n: usize = v.parse().unwrap_or_else(|_| panic!("CELESTE_UNIT_LANES={v:?} is not a number"));
            assert!(n >= 64 && n % 64 == 0, "CELESTE_UNIT_LANES={n}: must be a positive multiple of 64");
            n
        }
        Err(_) => 1024,
    })
}

/// Stack per frame worker: the ASM kernels keep their spill frames on the
/// stack (~0.5 MB at level 0, more at the finer rungs, 83 MB for room (3,0)'s
/// largest kernel with the fruit and the floors unknown, which overflowed the
/// 64 MB this was: a segfault at f59, 2026-09-18). Virtual: only the pages a
/// kernel touches are resident. Every call checks its frame fits
/// (`asm_kernel::set_thread_stack`).
const WORKER_STACK: usize = 512 << 20;

/// Cut `blocks` (by lane count) into `(block, lo, hi)` units.
fn units_of(lanes: impl Iterator<Item = usize>, unit: usize) -> Vec<(usize, usize, usize)> {
    let mut units = Vec::new();
    for (bi, n) in lanes.enumerate() {
        let mut lo = 0;
        while lo < n {
            let hi = (lo + unit).min(n);
            units.push((bi, lo, hi));
            lo = hi;
        }
    }
    units
}

/// One forward frame, one pass (plans/waves.md): units of `unit_lanes()`
/// lanes of the frontier's chunks, in CELL order, pulled by `threads()`
/// workers. Each worker runs the frame step on its units into its own
/// `ForwardSink`, whose queues flush as they fill: filter, door
/// (`search::door::Door`, the one shared structure), append into the
/// worker's pieces. One barrier, at the end: the door folds this frame's
/// admissions into its base, and the pieces become the canonical next
/// frontier. Rows emitted twice by different units are both admitted
/// against the door, which keeps the first - the same set as one call
/// would produce.
pub fn forward_frame(
    engine: &dyn FrameStep,
    frontier: Vec<Block>,
    door: &crate::search::door::Door,
    pos: Option<&crate::search::pos_graph::PosObserver>,
    filter: Option<&MarkFilter>,
    frame: u32,
    edges_dir: Option<&std::path::Path>,
) -> Result<(Vec<Block>, bool, FrameStats)> {
    use std::time::Instant;
    let mut st = FrameStats::default();
    let workers = threads();
    st.blocks_in = frontier.len();
    st.lanes_in = frontier.iter().map(Block::lanes).sum();
    st.bytes_in = frontier.iter().map(Block::bytes).sum();
    st.rss_start = crate::metrics::current_rss_gb();

    let cells: Vec<Vec<u32>> = frontier.iter().map(Block::positions).collect::<Result<_>>()?;
    // Units in WAVE order: by first cell across the (cell-sorted) pieces,
    // so consecutive units are neighbours in the room.
    let mut units = units_of(frontier.iter().map(Block::lanes), unit_lanes());
    // A unit of nothing but skipped lanes (won rows: checkpointed, never
    // expanded - their shape has no kernel) is not run.
    units.retain(|&(bi, lo, hi)| {
        let sk = &frontier[bi].skip;
        sk.is_empty() || sk[lo..hi].iter().any(|&s| !s)
    });
    units.sort_by_key(|&(bi, lo, _)| (cells[bi][lo], bi, lo));
    let next_unit = AtomicUsize::new(0);

    let t = Instant::now();
    struct Done {
        pieces: Vec<Block>,
        won: bool,
        kept: usize,
        flushes: u64,
        flushed_rows: u64,
        emitted: u64,
        edges: rustc_hash::FxHashSet<(u32, u32)>,
        queue_bytes: usize,
        edge_records: u64,
        t_edges: std::time::Duration,
        t_filter: std::time::Duration,
        busy: std::time::Duration,
    }
    let done: Vec<Done> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers)
            .map(|w| {
                let (frontier, cells, units, next_unit) = (&frontier, &cells, &units, &next_unit);
                // The kernels' spill frames are large (hundreds of KB at
                // level 0, more at the finer rungs, whose graphs are bigger);
                // the default 2 MB thread stack overflowed on room (0,0)'s
                // level 1 (2026-09-13).
                std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<Done> {
                    crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
                    let t = Instant::now();
                    let mut sink = ForwardSink::forward(door, filter, pos.is_some(), frame, w as u32, edges_dir);
                    loop {
                        let u = next_unit.fetch_add(1, Ordering::Relaxed);
                        let Some(&(bi, lo, hi)) = units.get(u) else { break };
                        let b = &frontier[bi];
                        sink.ids_in = (!b.ids.is_empty()).then_some(b.ids.as_slice());
                        sink.skip_in = (!b.skip.is_empty()).then_some(b.skip.as_slice());
                        engine.run(b, &cells[bi], lo..hi, &mut sink)?;
                    }
                    let pieces = sink.finish()?;
                    Ok(Done {
                        pieces,
                        won: sink.won,
                        kept: sink.kept,
                        flushes: sink.flushes,
                        flushed_rows: sink.flushed_rows,
                        emitted: sink.emitted,
                        edges: std::mem::take(&mut sink.edges),
                        queue_bytes: sink.alloc_bytes(),
                        edge_records: sink.edge_records,
                        t_edges: sink.t_edges,
                        t_filter: sink.t_filter,
                        busy: t.elapsed(),
                    })
                }).expect("spawn wave worker")
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("wave worker panicked"))
            .collect::<Result<Vec<_>>>()
    })?;
    st.t_wave = t.elapsed();
    st.wave_idle = idle_fraction(st.t_wave, workers, done.iter().map(|d| d.busy).sum());
    st.rss_wave = crate::metrics::current_rss_gb();
    drop(frontier);

    let t = Instant::now();
    door.end_frame(workers);
    st.t_door = t.elapsed();
    st.hull_grown = crate::search::door::take_hull_growths();
    st.door_bytes = door.alloc_bytes();

    let mut won = false;
    let mut pieces: Vec<Block> = Vec::new();
    for d in done {
        st.lanes_raw += d.emitted as usize;
        st.lanes_kept += d.kept;
        st.flushes += d.flushes;
        st.flushed_rows += d.flushed_rows;
        st.queue_bytes += d.queue_bytes;
        st.edge_records += d.edge_records;
        st.t_edges += d.t_edges;
        st.t_filter += d.t_filter;
        won |= d.won;
        if let Some(p) = pos {
            p.record_pairs(d.edges.iter().copied());
        }
        pieces.extend(d.pieces);
    }
    // The next frontier: the workers' pieces. The checkpoint sorts each
    // by (cell, key) in place, which is the order the next frame's units
    // follow; a cell's rows may sit in several pieces (one per worker
    // that flushed it), which only the door's delta ever sees.
    let next: Vec<Block> = pieces;
    st.rss_end = crate::metrics::current_rss_gb();
    st.rss_file = crate::metrics::current_file_rss_gb();
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    Ok((next, won, st))
}


/// The share of `workers x wall` a phase's workers spent waiting at its
/// barrier: 1 - busy / (workers * wall), `busy` summed over the workers.
fn idle_fraction(wall: std::time::Duration, workers: usize, busy: std::time::Duration) -> f64 {
    if workers == 0 || wall.is_zero() {
        return 0.0;
    }
    (1.0 - busy.as_secs_f64() / (workers as f64 * wall.as_secs_f64())).max(0.0)
}

/// Where one forward frame's time went and what it moved, for the
/// per-frame log line and the phase totals.
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    pub blocks_in: usize,
    pub lanes_in: usize,
    /// Output rows as the workers emitted them (after the step's
    /// within-call dedup), before the filter/door.
    pub lanes_raw: usize,
    /// Rows that passed the filter and were new at the door.
    pub lanes_kept: usize,
    pub blocks_out: usize,
    pub lanes_out: usize,
    /// The wave (kernel calls and inline flushes), wall, and its barrier
    /// idle fraction (`idle_fraction`).
    pub t_wave: std::time::Duration,
    pub wave_idle: f64,
    /// The door's end-of-frame merge, wall.
    pub t_door: std::time::Duration,
    pub flushes: u64,
    pub flushed_rows: u64,
    /// Admissions whose speed hull grew: existing states re-emitted as new
    /// rows (a bucketed level; `door::take_hull_growths`).
    pub hull_grown: u64,
    /// Edge records written (`EDGE_RECORD_BYTES` each), and the workers'
    /// summed time encoding and writing them (thread-time).
    pub edge_records: u64,
    pub t_edges: std::time::Duration,
    /// The workers' summed time in the ladder filter (thread-time).
    pub t_filter: std::time::Duration,
    /// The input frontier's row storage, bytes.
    pub bytes_in: usize,
    /// Bytes allocated in the workers' queue pools, and in the door.
    pub queue_bytes: usize,
    pub door_bytes: usize,
    /// Resident set at the frame's start, after the wave, at the end.
    pub rss_start: f64,
    pub rss_wave: f64,
    pub rss_end: f64,
    /// File-backed resident pages at the end of the frame (page cache).
    pub rss_file: f64,
}

/// The result of a backward pass: the marked set plus how many row re-runs
/// it cost - the number to compare against the forward pass (backward must
/// not exceed it; if it does, the narrowing is broken).
pub struct BackwardResult {
    pub marked: Visited,
    pub reruns: u64,
}

/// A level's checkpoint tree, every layer's files mapped once for the
/// duration of a backward: a cell's rows in layer f are a cell-index lookup
/// and a range copy per file.
struct Tree {
    layers: Vec<Vec<crate::search::checkpoint::FrameFile>>,
}

impl Tree {
    fn open(dir: &std::path::Path, horizon: u32) -> Result<Self> {
        Ok(Tree { layers: (0..=horizon).map(|f| frame_files(dir, f)).collect::<Result<_>>()? })
    }

    /// Every row at `cell` in layers `0..=upto`, one block per shape, in
    /// layer order. Returns `(blocks, rows)`.
    fn cell_rows(&self, cell: u32, upto: u32) -> Result<(Vec<Rt2>, usize)> {
        let mut by_shape: Vec<Rt2> = Vec::new();
        let mut loaded = 0usize;
        for layer in &self.layers[..=upto as usize] {
            for file in layer {
                let rs = file.rows_of_cell(cell);
                if rs.is_empty() {
                    continue;
                }
                let Some(rows) = file.load_rows(&rs)? else { continue };
                loaded += rows.width;
                match by_shape.iter_mut().find(|b| b.shape_hash == rows.shape_hash) {
                    Some(acc) => {
                        let all: Vec<u32> = (0..rows.width as u32).collect();
                        acc.append_rows(&rows, &all);
                    }
                    None => by_shape.push(rows),
                }
            }
        }
        Ok((by_shape, loaded))
    }
}

/// Backward marking - SINGLE PASS, position-narrowed (plans/architecture.md
/// sentence 8; the horizon-anchored minimal form, no distance DP). The
/// marked set is "the states that can reach a win by `horizon`": the
/// filter the next precision level needs at that horizon.
///
/// It is a reverse BFS from ALL the win states, `horizon - 1` steps deep,
/// with the frame as the anchor. The forward dedups across frames, so
/// checkpoint f holds the states FIRST reached at f - a BFS distance
/// layer, not "the states at frame f" - and a state reached at or before
/// frame i may be at frame i. That is why the seeds are the wins of EVERY
/// layer <= horizon (a win state at frame H may be filed under an earlier
/// layer) and why at iteration i the candidates are the unmarked rows of
/// every layer <= i, narrowed to the cells that can step into a target,
/// re-run against the marks added at iteration i+1. A state marked at
/// iteration i lies in a layer <= i and is `horizon - i` steps from a win:
/// a path of length <= horizon. Complete for the same reason: a state that
/// wins by the horizon from frame f >= its layer has a marked successor at
/// the right iteration. (Two earlier forms of this walk were wrong:
/// candidates from layer i only, and wins injected at their own layer's
/// iteration - each missed every path link whose other end was first
/// reached earlier, and refuted achievable horizons.)
///
/// One iteration is one parallel pass over the candidate CELLS, one cell
/// per unit, pulled dynamically: a unit gathers the cell's rows from every
/// layer <= i, drops the ones already marked (the marked set is read-only
/// during the iteration), runs them through the frame step in backward
/// mode (`ForwardSink::backward` - nothing materialized; per input row,
/// does any output hit a target) and hands back the hits. The one barrier
/// per iteration applies the hits to the marked set; they are the next
/// iteration's targets. Marks are matched by key.
pub fn backward_run(
    engine: &dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
) -> Result<BackwardResult> {
    let tree = Tree::open(dir, horizon)?;
    // Seeds: the win states of every layer 1..=horizon (listed in each
    // checkpoint file's header; nothing is decoded).
    let seeds: Vec<(u64, (u64, u64), u32)> =
        tree.layers[1..].iter().flatten().flat_map(|f| f.wins()).collect();
    backward_walk(engine, &tree, horizon, graph, seeds)
}

/// The backward walk itself, given the seeds (the states that count as
/// marked on their own, i.e. the wins, from any layer). Factored out so a
/// test can seed it directly; `backward_run` seeds from the win lanes.
pub fn backward_walk_in(
    engine: &dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
    seeds: Vec<(u64, (u64, u64), u32)>,
) -> Result<BackwardResult> {
    backward_walk(engine, &Tree::open(dir, horizon)?, horizon, graph, seeds)
}

fn backward_walk(
    engine: &dyn FrameStep,
    tree: &Tree,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
    seeds: Vec<(u64, (u64, u64), u32)>,
) -> Result<BackwardResult> {
    use rustc_hash::FxHashSet;

    let mut marked = Visited::new();
    let mut reruns: u64 = 0;
    // `frontier` is the marks added in the previous iteration - the only
    // targets a candidate's successor can newly hit (single pass).
    let mut frontier: Vec<(u64, (u64, u64), u32)> = Vec::new();
    for (shape, k, c) in seeds {
        if marked.insert(shape, k, c) {
            frontier.push((shape, k, c));
        }
    }
    for i in (1..horizon).rev() {
        let t_frame = std::time::Instant::now();
        let targets = TargetSet::new(frontier.iter().map(|&(_, k, c)| (k, c)));
        // Candidate cells = pos-graph predecessors of the targets' cells,
        // sorted so the units (and the marks' insertion order) are a
        // function of the frame, not of scheduling.
        let mut cells: Vec<u32> = frontier
            .iter()
            .flat_map(|&(_, _, c)| graph.srcs_of(c).iter().copied())
            .collect::<FxHashSet<u32>>()
            .into_iter()
            .collect();
        cells.sort_unstable();

        // One unit per cell: gather, drop the marked, run, report hits.
        struct UnitOut {
            cell_idx: usize,
            hits: Vec<(u64, (u64, u64), u32)>,
            loaded: usize,
            reruns: u64,
            t_load: std::time::Duration,
        }
        let next_cell = AtomicUsize::new(0);
        let per_worker: Vec<(Vec<UnitOut>, std::time::Duration)> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..threads())
                .map(|_| {
                    let (cells, next_cell, targets, marked) = (&cells, &next_cell, &targets, &marked);
                    std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<(Vec<UnitOut>, std::time::Duration)> {
                        crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
                        let t_busy = std::time::Instant::now();
                        let mut out = Vec::new();
                        loop {
                            let ci = next_cell.fetch_add(1, Ordering::Relaxed);
                            let Some(&cell) = cells.get(ci) else { break };
                            let t = std::time::Instant::now();
                            let (blocks, loaded) = tree.cell_rows(cell, i)?;
                            let t_load = t.elapsed();
                            let mut u = UnitOut { cell_idx: ci, hits: Vec::new(), loaded, reruns: 0, t_load };
                            for block in blocks {
                                let shape = block.shape_hash;
                                let block = Block::from_rt2(block);
                                let mask: Vec<bool> =
                                    block.keys().iter().map(|k| !marked.contains(shape, *k, cell)).collect();
                                let Some(cand) = block.keep(&mask) else { continue };
                                let cell_in = vec![cell; cand.lanes()];
                                let mut sink = ForwardSink::backward(targets, 0..cand.lanes());
                                engine.run(&cand, &cell_in, 0..cand.lanes(), &mut sink)?;
                                u.reruns += cand.lanes() as u64;
                                for (lane, hit) in sink.hits().iter().enumerate() {
                                    if *hit {
                                        u.hits.push((shape, cand.keys()[lane], cell));
                                    }
                                }
                            }
                            out.push(u);
                        }
                        Ok((out, t_busy.elapsed()))
                    }).expect("spawn backward worker")
                })
                .collect();
            handles
                .into_iter()
                .map(|h| h.join().expect("backward worker panicked"))
                .collect::<Result<Vec<_>>>()
        })?;
        let t_par = t_frame.elapsed();
        let idle = idle_fraction(t_par, per_worker.len(), per_worker.iter().map(|(_, b)| *b).sum());

        // The barrier: apply the hits, in cell order.
        let mut units: Vec<UnitOut> = per_worker.into_iter().flat_map(|(u, _)| u).collect();
        units.sort_by_key(|u| u.cell_idx);
        let (mut loaded, mut frame_reruns, mut t_load) = (0usize, 0u64, std::time::Duration::ZERO);
        let mut new_frontier: Vec<(u64, (u64, u64), u32)> = Vec::new();
        for u in units {
            loaded += u.loaded;
            frame_reruns += u.reruns;
            t_load += u.t_load;
            for (shape, k, c) in u.hits {
                if marked.insert(shape, k, c) {
                    new_frontier.push((shape, k, c));
                }
            }
        }
        reruns += frame_reruns;
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        eprintln!(
            "[bwd] f{i:03} targets {} cand-cells {} loaded {} rerun {} marked {} | \
             load {:.0} (thread-ms) par {:.0} (idle {:.0}%) total {:.0} ms",
            frontier.len(),
            cells.len(),
            loaded,
            frame_reruns,
            new_frontier.len(),
            ms(t_load),
            ms(t_par),
            idle * 100.0,
            ms(t_frame.elapsed()),
        );
        crate::metrics::record("bwd.frame", t_frame.elapsed());
        frontier = new_frontier;
        tree.layers.iter().flatten().for_each(|f| f.release());
    }
    Ok(BackwardResult { marked, reruns })
}

/// The forward search driver's state: the frontier, the visited set and
/// the position-graph observer, held in memory so a forward can be
/// EXTENDED frame by frame (the outer loop's "one more frame at rem zero")
/// rather than rerun. Every frame is checkpointed as it is produced.
pub struct ForwardState {
    frontier: Vec<Block>,
    /// The door: every (shape, cell, key) reached so far (`search::door`).
    door: crate::search::door::Door,
    observer: Option<crate::search::pos_graph::PosObserver>,
    /// The last frame computed (and checkpointed).
    pub frames: u32,
    /// The first frame a lane won, if any so far.
    pub win_frame: Option<u32>,
    /// The compaction of the last frame's edge records, running behind
    /// the next frame's wave (`join_compaction`).
    compaction: Option<(u32, std::thread::JoinHandle<Result<crate::search::edges::CompactStats>>)>,
}

/// What a forward run reports.
pub struct ForwardResult {
    /// The frame a lane first won, if any.
    pub win_frame: Option<u32>,
    /// The number of frames actually run (the last checkpointed frame).
    pub frames: u32,
    /// The position graph, when recording was on - backward's input.
    pub pos_graph: Option<crate::search::pos_graph::PosGraph>,
}

impl ForwardState {
    /// Frame 0: seed the visited set from `initial`, checkpoint it, start
    /// recording the position graph if `record`.
    pub fn start(mut initial: Vec<Block>, dir: &std::path::Path, record: bool) -> Result<Self> {
        // The checkpoint assigns the initial rows their ids (layer 0); the
        // door then takes each row under that id.
        checkpoint_frontier(dir, 0, &mut initial)?;
        crate::search::edges::set_done_frame(&dir.join("edges"), 0)?;
        let door = crate::search::door::Door::for_current_level();
        for b in &initial {
            let cells = b.positions()?;
            let shape = b.shard_shape();
            let (mut new, mut ids) = (Vec::new(), Vec::new());
            let hulls = b.rt2().speed_hulls(crate::compiled::ids());
            for (r, ((&cell, &key), &id)) in cells.iter().zip(b.keys()).zip(b.ids()).enumerate() {
                let h = hulls.as_ref().map(|h| vec![h[r]]);
                door.admit(shape, cell, &[key], id, &mut ids, &mut new, h.as_deref(), &mut Vec::new());
            }
        }
        door.end_frame(1);
        let observer = record.then(crate::search::pos_graph::PosObserver::default);
        // A graph (empty) beside f0 from the start: every tree that has
        // frames has a graph covering at least the trusted ones.
        if let Some(o) = observer.as_ref() {
            save_pos_graph(dir, &o.snapshot(), 0)?;
        }
        Ok(ForwardState {
            frontier: initial,
            door,
            observer,
            frames: 0,
            win_frame: None,
            compaction: None,
        })
    }

    /// The forward as its checkpoint tree left it, if `dir` has one: the
    /// last frame's rows as the frontier (minus the won rows, as `extend`
    /// leaves them), the door rebuilt from every layer's keys, the first
    /// win frame from the layers' win lists, the pos graph reloaded. `None`
    /// when there is no tree. Extending a resumed forward is byte-identical
    /// to extending the original (`forward_resume_matches_fresh`).
    pub fn resume(dir: &std::path::Path, record: bool) -> Result<Option<Self>> {
        use crate::search::door::Admit;
        let mut last: Option<u32> = None;
        while dir.join("frames").join(format!("f{:03}", last.map_or(0, |f| f + 1))).is_dir() {
            last = Some(last.map_or(0, |f| f + 1));
        }
        let Some(mut last) = last else { return Ok(None) };
        let t = std::time::Instant::now();
        // The frames a resume can trust are those whose edge runs are
        // complete (`edges/done.txt`, written when a frame's compaction
        // joined - before the NEXT frame's checkpoint, so at most the last
        // frame is discarded).
        let edges_dir = dir.join("edges");
        let done = crate::search::edges::done_frame(&edges_dir).unwrap_or(last);
        if done < last {
            for f in done + 1..=last {
                std::fs::remove_dir_all(dir.join("frames").join(format!("f{:03}", f)))?;
            }
            eprintln!("[resume] frames f{} to f{last} discarded: their edge runs were not complete", done + 1);
            last = done;
        }
        crate::search::edges::discard_after(&edges_dir, last)?;
        // Every layer's keys into the door, one file per unit of work.
        let seq_of = |p: &std::path::Path| -> u32 {
            p.file_name()
                .and_then(|s| s.to_str())
                .and_then(|n| n.trim_end_matches(".bin").rsplit('_').next())
                .and_then(|s| s.parse().ok())
                .unwrap_or(0)
        };
        let files: Vec<(u32, std::path::PathBuf)> = (0..=last)
            .flat_map(|f| {
                let fdir = dir.join("frames").join(format!("f{:03}", f));
                std::fs::read_dir(&fdir)
                    .into_iter()
                    .flatten()
                    .filter_map(|e| e.ok().map(|e| e.path()))
                    .filter(|p| p.file_name().and_then(|s| s.to_str()).is_some_and(|n| n.starts_with('s') && n.ends_with(".bin")))
                    .map(move |p| (f, p))
            })
            .collect();
        let next = AtomicUsize::new(0);
        let hulled = crate::interpreter::abstraction::spd_precision().width_log2().is_some();
        let partial: Vec<(rustc_hash::FxHashMap<(u64, u32), Vec<crate::search::door::Entry>>, Option<u32>)> =
            std::thread::scope(|scope| {
                let handles: Vec<_> = (0..threads())
                    .map(|_| {
                        let (files, next, seq_of, hulled) = (&files, &next, &seq_of, hulled);
                        scope.spawn(move || -> Result<_> {
                            let mut m: rustc_hash::FxHashMap<(u64, u32), Vec<crate::search::door::Entry>> = Default::default();
                            let mut win: Option<u32> = None;
                            loop {
                                let i = next.fetch_add(1, Ordering::Relaxed);
                                let Some((f, path)) = files.get(i) else { break };
                                let file = crate::search::checkpoint::FrameFile::open(path)?;
                                let shape = file.shape_hash();
                                let seq = seq_of(path);
                                // The door's speed hulls come back from the
                                // rows (a bucketed level; decoding every layer,
                                // which a resume can afford).
                                let hulls: Option<Vec<crate::search::door::Hull>> = if hulled {
                                    file.load_all()?.and_then(|rt2| rt2.speed_hulls(crate::compiled::ids()))
                                } else {
                                    None
                                };
                                for (row, (cell, key)) in file.cell_keys_rows() {
                                    let h = hulls.as_ref().map_or(crate::search::door::NO_HULL, |h| h[row as usize]);
                                    m.entry((shape, cell)).or_default().push((key, pack_id(*f, seq, row), h));
                                }
                                if !file.wins().is_empty() {
                                    win = Some(win.map_or(*f, |w| w.min(*f)));
                                }
                            }
                            Ok((m, win))
                        })
                    })
                    .collect();
                handles.into_iter().map(|h| h.join().expect("resume loader panicked")).collect::<Result<Vec<_>>>()
            })?;
        let mut shards: rustc_hash::FxHashMap<(u64, u32), Vec<crate::search::door::Entry>> = Default::default();
        let mut win_frame: Option<u32> = None;
        for (m, w) in partial {
            for (k, mut v) in m {
                shards.entry(k).or_default().append(&mut v);
            }
            win_frame = match (win_frame, w) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (a, b) => a.or(b),
            };
        }
        let door = crate::search::door::Door::from_shards(shards, hulled);
        // The frontier: the last layer minus its won rows.
        let mut frontier: Vec<Block> = load_frame(dir, last)?;
        for b in &mut frontier {
            let wins = b.wins()?;
            b.set_skip(wins);
        }
        let observer = if record {
            let path = pos_graph_path(dir);
            let graph = crate::search::pos_graph::PosGraph::load(&path)
                .with_context(|| format!("resuming {}: no pos graph at {}", dir.display(), path.display()))?;
            // The graph must cover every trusted frame. A tree without the
            // marker predates per-frame saves; its graph was saved when its
            // forward ended, covering the whole tree.
            if let Some(covered) = pos_graph_frame(dir) {
                anyhow::ensure!(
                    covered >= last,
                    "resuming {}: the pos graph covers f0-f{covered}, but the trusted frames run to f{last}",
                    dir.display()
                );
            }
            Some(crate::search::pos_graph::PosObserver::from_graph(&graph))
        } else {
            None
        };
        eprintln!(
            "[resume] {}: f{last} ({} lanes), door {} entries from {} files, first win {:?}, {:.1} s",
            dir.display(),
            frontier.iter().map(Block::lanes).sum::<usize>(),
            door.len(),
            files.len(),
            win_frame,
            t.elapsed().as_secs_f64()
        );
        Ok(Some(ForwardState { frontier, door, observer, frames: last, win_frame, compaction: None }))
    }

    /// Compute and checkpoint frames `frames+1 ..= to`. A win does NOT stop
    /// the run: a horizon is a bound on the win frame, and the backward
    /// needs every frame up to it (its seeds are the wins at each). Only an
    /// empty frontier stops it.
    /// The door's size: every distinct state reached so far.
    pub fn visited_len(&self) -> usize {
        self.door.len()
    }

    pub fn door(&self) -> &crate::search::door::Door {
        &self.door
    }

    /// Wait for the compaction running behind the current frame (the
    /// previous frame's), then mark its frame done for a resume. Returns
    /// its stats and the time waited.
    fn join_compaction(
        &mut self,
        edges_dir: &std::path::Path,
    ) -> Result<Option<(crate::search::edges::CompactStats, std::time::Duration)>> {
        let Some((frame, handle)) = self.compaction.take() else { return Ok(None) };
        let t = std::time::Instant::now();
        let st = handle.join().expect("compaction thread panicked")?;
        crate::search::edges::set_done_frame(edges_dir, frame)?;
        Ok(Some((st, t.elapsed())))
    }

    pub fn extend(
        &mut self,
        engine: &dyn FrameStep,
        dir: &std::path::Path,
        to: u32,
        filter: Option<&MarkFilter>,
    ) -> Result<()> {
        while self.frames < to && !self.frontier.is_empty() {
            let frame = self.frames + 1;
            let t_frame = std::time::Instant::now();
            let frontier = std::mem::take(&mut self.frontier);
            let edges_dir = dir.join("edges");
            let (mut next, won, st) =
                forward_frame(engine, frontier, &self.door, self.observer.as_ref(), filter, frame, Some(&edges_dir))?;
            // The PREVIOUS frame's compaction ran behind this wave; it is
            // joined and marked done before this frame is checkpointed, so
            // a resume (which trusts frames up to `done.txt`) never sees a
            // frame whose runs are incomplete.
            let joined = self.join_compaction(&edges_dir)?;
            let (t_edges, compact_records) = joined.as_ref().map_or((std::time::Duration::ZERO, 0), |(c, w)| (*w, c.records));
            let t = std::time::Instant::now();
            checkpoint_frontier(dir, frame, &mut next)?;
            let t_ckpt = t.elapsed();
            // This frame's raw records into runs, in the background.
            {
                let edges_dir = edges_dir.clone();
                let handle = std::thread::Builder::new()
                    .name(format!("compact-f{frame}"))
                    .spawn(move || crate::search::edges::compact_frame(&edges_dir, frame))
                    .expect("spawn compaction");
                self.compaction = Some((frame, handle));
            }
            let t = std::time::Instant::now();
            if let Some(o) = self.observer.as_ref() {
                save_pos_graph(dir, &o.snapshot(), frame)?;
            }
            let t_pos = t.elapsed();
            log_frame(frame, &st, t_edges, compact_records, t_ckpt, t_pos, t_frame.elapsed(), self.visited_len());
            self.frames = frame;
            if won && self.win_frame.is_none() {
                self.win_frame = Some(frame);
                eprintln!("[fwd] first win at f{frame}");
            }
            // A won row is checkpointed (it is a backward seed) but never
            // expanded: its successors have left the room, and the search
            // is about reaching the exit, not what lies past it (and the
            // start room's kernel set does not cover the next room's
            // shapes).
            self.frontier = if won {
                std::thread::scope(|scope| {
                    let handles: Vec<_> = next
                        .into_iter()
                        .map(|mut b| {
                            scope.spawn(move || -> Result<Block> {
                                let wins = b.wins()?;
                                b.set_skip(wins);
                                Ok(b)
                            })
                        })
                        .collect();
                    handles
                        .into_iter()
                        .map(|h| h.join().expect("win-skip worker panicked"))
                        .collect::<Result<Vec<_>>>()
                })?
            } else {
                next
            };
        }
        // The last frame's runs, before anything reads them (the backward).
        self.join_compaction(&dir.join("edges"))?;
        // The level's graph so far, beside its frames: a backward can then
        // run on the tree alone (`rewrite bench-backward`).
        if let Some(o) = self.observer.as_ref() {
            save_pos_graph(dir, &o.snapshot(), self.frames)?;
        }
        Ok(())
    }

    /// The position graph recorded so far (None when not recording).
    pub fn pos_graph(&self) -> Option<crate::search::pos_graph::PosGraph> {
        self.observer.as_ref().map(|o| o.snapshot())
    }
}

/// Where a level's position graph is saved alongside its frames.
/// Persist a level's position graph as it stands after `frame`, beside its
/// frames: the graph to a temp file renamed over `posgraph.bin`, then the
/// frame it covers to `posgraph.frame` the same way. Saved every frame, so a
/// crash between frames leaves a tree `ForwardState::resume` can take up.
/// Saving it only when a forward ended made every crash unresumable: room
/// (4,0)'s search died at f62 with f0-f60 on disk and no graph (2026-09-16).
/// The graph is a union of per-frame edge sets, so a graph saved at a frame
/// the resume later discards only holds edges its re-run records again.
pub fn save_pos_graph(dir: &std::path::Path, graph: &crate::search::pos_graph::PosGraph, frame: u32) -> Result<()> {
    let tmp = dir.join("posgraph.tmp");
    graph.save(&tmp)?;
    std::fs::rename(&tmp, pos_graph_path(dir))?;
    let ftmp = dir.join("posgraph.frame.tmp");
    std::fs::write(&ftmp, format!("{frame}\n"))?;
    std::fs::rename(&ftmp, dir.join("posgraph.frame"))?;
    Ok(())
}

/// The last frame the saved position graph covers (`save_pos_graph`); `None`
/// for a tree written before per-frame saves, whose graph was saved when its
/// forward ended.
pub fn pos_graph_frame(dir: &std::path::Path) -> Option<u32> {
    std::fs::read_to_string(dir.join("posgraph.frame")).ok().and_then(|s| s.trim().parse().ok())
}

pub fn pos_graph_path(dir: &std::path::Path) -> std::path::PathBuf {
    dir.join("posgraph.bin")
}

/// A whole forward run in one call: frame 0 from `initial`, then frames
/// `1..=max_frames`.
pub fn forward_run(
    engine: &dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
    filter: Option<&MarkFilter>,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, record)?;
    st.extend(engine, dir, max_frames, filter)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// One line per forward frame on stderr, plus the phase totals under
/// `metrics` (`fwd.*`), so a run's wall time is attributable without
/// re-running it under a profiler. Times in ms.
fn log_frame(
    frame: u32,
    st: &FrameStats,
    t_edges: std::time::Duration,
    edge_records: u64,
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    eprintln!(
        "[fwd] f{frame:03} in {}/{} raw {} kept {} out {}/{} visited {} | \
         wave {:.0} (idle {:.0}%) door {:.0} edges {:.0} ckpt {:.0} pos {:.0} total {:.0} ms | \
         flushes {} ({:.0} rows avg) edges {} hull growths {} | \
         in {:.2} queues {:.2} door {:.2} GB rss start {:.2} wave {:.2} end {:.2} peak {:.2} GB (anon; file {:.2})",
        st.blocks_in,
        st.lanes_in,
        st.lanes_raw,
        st.lanes_kept,
        st.blocks_out,
        st.lanes_out,
        visited,
        ms(st.t_wave),
        st.wave_idle * 100.0,
        ms(st.t_door),
        ms(t_edges),
        ms(t_ckpt),
        ms(t_pos),
        ms(t_total),
        st.flushes,
        st.flushed_rows as f64 / st.flushes.max(1) as f64,
        edge_records,
        st.hull_grown,
        st.bytes_in as f64 / 1e9,
        st.queue_bytes as f64 / 1e9,
        st.door_bytes as f64 / 1e9,
        st.rss_start,
        st.rss_wave,
        st.rss_end,
        crate::metrics::peak_rss_gb(),
        st.rss_file,
    );
    crate::metrics::record("fwd.wave", st.t_wave);
    crate::metrics::record("fwd.door", st.t_door);
    crate::metrics::record("fwd.checkpoint", t_ckpt);
    crate::metrics::record("fwd.posgraph", t_pos);
    crate::metrics::record("fwd.frame", t_total);
}

/// Checkpoint a frontier: one file per BUCKET under `frames/fNNN/`, named
/// `b{seq}_s{shape}.bin`.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &mut [Block]) -> Result<()> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    // Fresh: a re-run / resumed frame must not leave stale files behind.
    let _ = std::fs::remove_dir_all(&fdir);
    std::fs::create_dir_all(&fdir)?;
    // One file per piece, written in parallel, in the piece's own row
    // order: a row's position is its id (`pack_id`), assigned when it was
    // admitted. A block without ids (the initial frontier) gets them here.
    // Ids are (layer, seq, row): the seqs of a frame must be distinct.
    {
        let mut seqs: Vec<u32> = frontier.iter().filter(|b| !b.ids.is_empty()).map(|b| b.seq).collect();
        seqs.sort_unstable();
        anyhow::ensure!(seqs.windows(2).all(|w| w[0] != w[1]), "checkpoint f{frame}: two pieces share a seq");
    }
    std::thread::scope(|scope| {
        let handles: Vec<_> = frontier
            .iter_mut()
            .enumerate()
            .map(|(i, block)| {
                let fdir = &fdir;
                scope.spawn(move || -> Result<()> {
                    if block.ids.is_empty() {
                        anyhow::ensure!(frame == 0, "checkpoint f{frame}: an id-less block past the initial frontier");
                        block.seq = i as u32;
                        block.ids = (0..block.lanes() as u32).map(|r| pack_id(frame, i as u32, r)).collect();
                    }
                    let cells = block.positions()?;
                    let wins = block.wins()?;
                    let path = fdir.join(format!("s{:016x}_{:04}.bin", block.shard_shape(), block.seq));
                    crate::search::checkpoint::save_block(&path, &block.rt2, &cells, &wins)
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("checkpoint worker panicked"))
            .collect::<Result<()>>()
    })
}

/// The checkpoint files of a frame (one per block written), mapped.
pub fn frame_files(
    dir: &std::path::Path,
    frame: u32,
) -> Result<Vec<crate::search::checkpoint::FrameFile>> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    let mut names: Vec<std::path::PathBuf> = std::fs::read_dir(&fdir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            n.starts_with('s') && n.ends_with(".bin")
        })
        .collect();
    names.sort();
    names.iter().map(|p| crate::search::checkpoint::FrameFile::open(p)).collect()
}

/// Load every block of a checkpointed frame.
pub fn load_frame(dir: &std::path::Path, frame: u32) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    for (seq, file) in frame_files_seq(dir, frame)? {
        if let Some(rt2) = file.load_all()? {
            let ids: Vec<u64> = (0..rt2.width as u32).map(|r| pack_id(frame, seq, r)).collect();
            out.push(Block::with_ids(rt2, ids, seq));
        }
    }
    Ok(out)
}

/// A frame's files with their seq (from the name `s{shape}_{seq}.bin`).
pub fn frame_files_seq(
    dir: &std::path::Path,
    frame: u32,
) -> Result<Vec<(u32, crate::search::checkpoint::FrameFile)>> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    let mut names: Vec<std::path::PathBuf> = std::fs::read_dir(&fdir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            n.starts_with('s') && n.ends_with(".bin")
        })
        .collect();
    names.sort();
    names
        .iter()
        .map(|p| {
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            let seq: u32 = n
                .trim_end_matches(".bin")
                .rsplit('_')
                .next()
                .and_then(|s| s.parse().ok())
                .ok_or_else(|| anyhow::anyhow!("{}: no seq in the file name", p.display()))?;
            Ok((seq, crate::search::checkpoint::FrameFile::open(p)?))
        })
        .collect()
}

/// Load only the ROWS whose cell is in `cells` - the backward's per-cell
/// load, a range copy per (file, cell) out of the cell index.
pub fn load_frame_cells(
    dir: &std::path::Path,
    frame: u32,
    cells: &rustc_hash::FxHashSet<u32>,
) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    for file in frame_files(dir, frame)? {
        if let Some(rt2) = file.load_cells(cells)? {
            out.push(Block::from_rt2(rt2));
        }
    }
    Ok(out)
}

/// The visited set: the keys of every lane ever kept into a frontier. A lane is
/// kept iff its key was not already here (this is both the within-frame and the
/// across-frame dedup - one structure, one lookup at the door).
///
/// Placeholder over an in-RAM set; the production one is tiered/mmap-backed
/// (`interpreter::visited`) but that is an implementation swap behind
/// `insert`/`contains`, not part of this interface.
#[derive(Default)]
pub struct Visited {
    /// Sharded by (shape hash, cell): a shard holds the 128-bit content
    /// keys seen at that shape and cell. Two states in different shards can
    /// never be duplicates (different shape or different content ->
    /// different cell), so the sharding is a free refinement: small sets,
    /// per-cell locality, and a run of rows at one cell is one outer
    /// lookup.
    ///
    /// Each key maps to its DEADLINE: the last frame a marked state still
    /// reaches a win by the horizon from (`edges::Marks`), what `MarkFilter`
    /// bounds time with. `u16::MAX` where there is none - a door, a loaded
    /// marks file (the file stores the set only), the kernel re-run backward:
    /// the membership test alone, as before deadlines existed.
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashMap<(u64, u64), u16>>,
}

impl Visited {
    pub fn new() -> Self {
        Self::default()
    }
    /// `(entries, order-independent hash of the (content, cell) set)` - the
    /// gate that two backward passes marked the same states. A function of
    /// the keys and cells only, not of how they are sharded.
    pub fn fingerprint(&self) -> (usize, u64) {
        use celeste_engine::runtime2::mix64;
        let mut acc = 0u64;
        for ((_shape, cell), keys) in &self.shards {
            for &(k0, k1) in keys.keys() {
                acc = acc.wrapping_add(mix64(k0 ^ mix64(k1 ^ (*cell as u64) << 1)));
            }
        }
        (self.len(), acc)
    }
    /// True if `key` (at `shape`, `cell`) was NOT already present - this
    /// lane is new, keep it. No deadline.
    pub fn insert(&mut self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.insert_until(shape, key, cell, u16::MAX)
    }
    /// `insert` with a deadline; a key inserted twice keeps the later one.
    pub fn insert_until(&mut self, shape: u64, key: (u64, u64), cell: u32, deadline: u16) -> bool {
        use std::collections::hash_map::Entry;
        match self.shards.entry((shape, cell)).or_default().entry(key) {
            Entry::Occupied(mut e) => {
                let d = e.get_mut();
                *d = (*d).max(deadline);
                false
            }
            Entry::Vacant(e) => {
                e.insert(deadline);
                true
            }
        }
    }
    pub fn contains(&self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.shards.get(&(shape, cell)).is_some_and(|s| s.contains_key(&key))
    }
    /// The key's deadline, `None` if absent.
    pub fn deadline(&self, shape: u64, key: (u64, u64), cell: u32) -> Option<u16> {
        self.shards.get(&(shape, cell)).and_then(|s| s.get(&key)).copied()
    }
    pub fn len(&self) -> usize {
        self.shards.values().map(|s| s.len()).sum()
    }

    /// Bytes the shards' tables occupy (hashbrown: 16-byte keys plus a
    /// control byte per bucket, at each set's capacity).
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        let mut v: Vec<(u64, u32, u64, u64)> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            v.extend(keys.keys().map(|&(k0, k1)| (*shape, *cell, k0, k1)));
        }
        crate::search::checkpoint::save_value_to(path, &v)
    }

    pub fn load(path: &std::path::Path) -> Result<Self> {
        let v: Vec<(u64, u32, u64, u64)> = crate::search::checkpoint::load_value_from(path)?;
        let mut out = Self::new();
        for (shape, cell, k0, k1) in v {
            out.insert(shape, (k0, k1), cell);
        }
        Ok(out)
    }
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
    /// Every entry as `(shape, cell, key)`, sorted.
    pub fn entries(&self) -> Vec<(u64, u32, (u64, u64))> {
        let mut v: Vec<(u64, u32, (u64, u64))> = self
            .shards
            .iter()
            .flat_map(|((s, c), keys)| keys.keys().map(move |&k| (*s, *c, k)))
            .collect();
        v.sort_unstable();
        v
    }
}

/// A fixed-horizon ladder result (`ladder_at_horizon`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HorizonOutcome {
    /// Every precision level - through fully concrete - won by the horizon. The
    /// horizon is achievable; the concrete level yields a real winning trace.
    Confirmed,
    /// A precision level, filtered by the coarser level's marks, found no win by
    /// the horizon. A coarser level over-approximates concrete reachability, so
    /// this SOUNDLY excludes the horizon: no concrete play wins by it. `level`
    /// is the index into `precisions` that refuted.
    Refuted { level: usize },
}

impl HorizonOutcome {
    /// Persist beside the horizon's levels, so a rerun of the search skips
    /// horizons it already settled.
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let text = match self {
            HorizonOutcome::Confirmed => "confirmed\n".to_string(),
            HorizonOutcome::Refuted { level } => format!("refuted {level}\n"),
        };
        Ok(std::fs::write(path, text)?)
    }

    pub fn load(path: &std::path::Path) -> Result<Option<Self>> {
        let Ok(text) = std::fs::read_to_string(path) else { return Ok(None) };
        let mut it = text.split_whitespace();
        Ok(Some(match (it.next(), it.next()) {
            (Some("confirmed"), None) => HorizonOutcome::Confirmed,
            (Some("refuted"), Some(l)) => HorizonOutcome::Refuted { level: l.parse()? },
            _ => anyhow::bail!("{}: unreadable outcome {text:?}", path.display()),
        }))
    }
}

/// The precision ladder over rising horizons (semantics pinned with Philippe
/// 2026-08-30; incremental level 0 2026-09-12).
///
/// At a horizon H, every level in `precisions` (coarsest first, ending at
/// `RemPrecision::Exact` = fully concrete) runs its forward TO H, filtered
/// by the previous level's marks, and its backward marks every state that
/// can reach a win by H. The instant a level has no win by H the horizon is
/// `Refuted` - a coarser level over-approximates concrete reachability, so
/// that soundly excludes it. Only if EVERY level wins by H is it `Confirmed`.
///
/// Level 0 is PERSISTENT across horizons: its forward is extended by the
/// frames the new horizon adds (`ForwardState::extend`), never rerun, and
/// its backward is recomputed from the extended checkpoints. The finer
/// levels are filtered by marks that change with H, so they rerun.
pub struct Ladder<'a, E, I>
where
    E: FnMut(crate::interpreter::abstraction::Level) -> Result<Box<dyn FrameStep>>,
    I: FnMut() -> Result<Vec<Block>>,
{
    make_engine: E,
    make_initial: I,
    base_dir: &'a std::path::Path,
    precisions: &'a [crate::interpreter::abstraction::Level],
    level0: Option<(Box<dyn FrameStep>, ForwardState)>,
    /// Drop level 0's in-memory state (door, frontier, engine) once its
    /// backward at a horizon is done, and resume it from disk when a later
    /// horizon needs it. Counting down from a ceiling it never extends again,
    /// and at room (3,0) f89 it held ~10 GB idle under every finer level;
    /// the resume is 42 s (a 550M-entry door, 2026-09-19).
    drop_level0: bool,
}

impl<'a, E, I> Ladder<'a, E, I>
where
    E: FnMut(crate::interpreter::abstraction::Level) -> Result<Box<dyn FrameStep>>,
    I: FnMut() -> Result<Vec<Block>>,
{
    pub fn new(
        make_engine: E,
        make_initial: I,
        base_dir: &'a std::path::Path,
        precisions: &'a [crate::interpreter::abstraction::Level],
    ) -> Self {
        Ladder { make_engine, make_initial, base_dir, precisions, level0: None, drop_level0: false }
    }

    fn level_dir(&self, horizon: u32, level: usize) -> std::path::PathBuf {
        if level == 0 {
            self.base_dir.join("level00")
        } else {
            self.base_dir.join(format!("h{:03}", horizon)).join(format!("level{:02}", level))
        }
    }

    /// Extend level 0 to `horizon` (starting it if needed) and report its
    /// first win frame, if it has one by then.
    pub fn extend_level0(&mut self, horizon: u32) -> Result<Option<u32>> {
        anyhow::ensure!(!self.precisions.is_empty(), "an empty ladder");
        // The kernel set an engine dispatches to follows the PROCESS-GLOBAL
        // rem precision (`asm_kernel::registry`), and a finer level ran
        // since level 0 last did: say which level is running before it runs.
        crate::interpreter::abstraction::set_level(self.precisions[0]);
        let dir = self.level_dir(horizon, 0);
        if self.level0.is_none() {
            let engine = (self.make_engine)(self.precisions[0])?;
            // A tree left by an earlier run resumes; the ladder's finer
            // levels are recomputed per horizon (their outcome is on disk,
            // `outcome_path`, so completed horizons are skipped).
            let state = match ForwardState::resume(&dir, true)? {
                Some(s) => s,
                None => ForwardState::start((self.make_initial)()?, &dir, true)?,
            };
            self.level0 = Some((engine, state));
        }
        let (engine, state) = self.level0.as_mut().expect("just started");
        state.extend(engine.as_ref(), &dir, horizon, None)?;
        Ok(state.win_frame.filter(|&h| h <= horizon))
    }

    pub fn at_horizon(&mut self, horizon: u32) -> Result<HorizonOutcome> {
        // A horizon an earlier run finished: its outcome is on disk. Level 0
        // still has to be extended to it (a no-op when the resumed tree
        // already reaches it) so later horizons find their frames.
        if let Some(outcome) = HorizonOutcome::load(&self.outcome_path(horizon))? {
            self.extend_level0(horizon)?;
            eprintln!("[ladder] h{horizon}: {outcome:?} (from an earlier run)");
            return Ok(outcome);
        }
        let outcome = self.at_horizon_fresh(horizon)?;
        outcome.save(&self.outcome_path(horizon))?;
        Ok(outcome)
    }

    fn outcome_path(&self, horizon: u32) -> std::path::PathBuf {
        self.base_dir.join(format!("h{:03}", horizon)).join("outcome.txt")
    }

    fn at_horizon_fresh(&mut self, horizon: u32) -> Result<HorizonOutcome> {
        let mut prev: Option<(Visited, crate::interpreter::abstraction::Level)> = None;
        for level in 0..self.precisions.len() {
            let precision = self.precisions[level];
            let dir = self.level_dir(horizon, level);
            // The level's forward to `horizon`: level 0 extended in place,
            // a finer level fresh, under the previous level's marks.
            let mut fresh: Option<Box<dyn FrameStep>> = None;
            let (win, graph) = if level == 0 {
                let win = self.extend_level0(horizon)?;
                let state = &self.level0.as_ref().expect("started by extend_level0").1;
                (win, state.pos_graph().expect("level 0 records"))
            } else {
                crate::interpreter::abstraction::set_level(precision);
                let engine = (self.make_engine)(precision)?;
                let filter = prev.as_ref().map(|(m, p)| MarkFilter::new(m, *p));
                let fwd = forward_run(
                    engine.as_ref(),
                    (self.make_initial)()?,
                    &dir,
                    horizon,
                    true,
                    filter.as_ref(),
                )?;
                fresh = Some(engine);
                (fwd.win_frame, fwd.pos_graph.expect("record mode always builds the pos graph"))
            };
            let Some(h) = win else {
                eprintln!("[ladder] h{horizon} level {level} ({precision}): NO WIN -> Refuted");
                return Ok(HorizonOutcome::Refuted { level });
            };
            let engine: &dyn FrameStep = match fresh.as_ref() {
                Some(e) => e.as_ref(),
                None => self.level0.as_ref().expect("level 0").0.as_ref(),
            };
            let (marked, work) = if bfs_backward() {
                let bwd = crate::search::edges::backward(&dir, horizon)?;
                (bwd.marked, format!("{} edges read", bwd.stats.edges_read))
            } else {
                let bwd = backward_run(engine, &dir, horizon, &graph)?;
                (bwd.marked, format!("{} re-runs", bwd.reruns))
            };
            marked.save(&marks_path(self.base_dir, horizon, level))?;
            let (n, fp) = marked.fingerprint();
            eprintln!(
                "[ladder] h{horizon} level {level} ({precision}): first win f{h}, marked {n} states \
                 (fingerprint {fp:016x}), {work}"
            );
            prev = Some((marked, precision));
            if level == 0 && self.drop_level0 {
                self.level0 = None;
            }
        }
        Ok(HorizonOutcome::Confirmed)
    }
}

/// The backward is the BFS over the recorded edges (`search::edges`);
/// `CELESTE_BACKWARD=kernel` selects the kernel re-run walk instead (the
/// oracle the BFS is gated against, `bench-backward --diff`).
pub fn bfs_backward() -> bool {
    !std::env::var("CELESTE_BACKWARD").is_ok_and(|v| v == "kernel")
}

/// Where a level's marked set at a horizon is saved (level 0's checkpoints
/// are shared across horizons; its marks are not).
pub fn marks_path(base_dir: &std::path::Path, horizon: u32, level: usize) -> std::path::PathBuf {
    base_dir.join(format!("h{:03}", horizon)).join(format!("level{:02}.marks.bin", level))
}

/// The inner ladder at ONE horizon, from scratch - the tests' entry point;
/// the search uses `Ladder` so level 0 persists across horizons.
pub fn ladder_at_horizon(
    make_engine: impl FnMut(
        crate::interpreter::abstraction::Level,
    ) -> Result<Box<dyn FrameStep>>,
    make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    horizon: u32,
    precisions: &[crate::interpreter::abstraction::Level],
) -> Result<HorizonOutcome> {
    Ladder::new(make_engine, make_initial, base_dir, precisions).at_horizon(horizon)
}

/// The OUTER loop: the minimal winning frame. Level 0 is extended until it
/// first wins (the abstract lower bound, or `first_win` if that is later);
/// from there the horizon steps up by one until the ladder Confirms it -
/// that horizon is the optimum, and (once trace extraction lands) the
/// concrete level's winning trace is the witness. `None` if nothing confirms
/// by `max_horizon`.
pub fn find_optimum(
    make_engine: impl FnMut(
        crate::interpreter::abstraction::Level,
    ) -> Result<Box<dyn FrameStep>>,
    make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    first_win: u32,
    max_horizon: u32,
    precisions: &[crate::interpreter::abstraction::Level],
) -> Result<Option<u32>> {
    let mut ladder = Ladder::new(make_engine, make_initial, base_dir, precisions);
    let mut horizon = first_win.max(1);
    // Level 0 first: extend until it wins, one frame at a time past
    // `first_win`, without touching the finer levels.
    while horizon <= max_horizon && ladder.extend_level0(horizon)?.is_none() {
        eprintln!("[search] level 0 has no win by f{horizon}");
        horizon += 1;
    }
    while horizon <= max_horizon {
        match ladder.at_horizon(horizon)? {
            HorizonOutcome::Confirmed => return Ok(Some(horizon)),
            HorizonOutcome::Refuted { level } => {
                eprintln!("[search] horizon {horizon} refuted at level {level}");
                horizon += 1;
            }
        }
    }
    Ok(None)
}

/// `find_optimum` COUNTING DOWN from a known concrete solution (the
/// replayed community TAS, `--ceiling`). The ceiling must confirm - a
/// refutation there means the model cannot reproduce a real run and is
/// an error - and then each horizon below it is tested until one is
/// refuted: the optimum is the last confirmed. With an optimal ceiling
/// that is two ladder runs. Counting down is also the direction the
/// marks are monotone in (marked at H-1 implies marked at H), which is
/// what a cheap narrowing of the finer levels' trees can build on.
pub fn find_optimum_from_ceiling(
    make_engine: impl FnMut(
        crate::interpreter::abstraction::Level,
    ) -> Result<Box<dyn FrameStep>>,
    make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    ceiling: u32,
    precisions: &[crate::interpreter::abstraction::Level],
) -> Result<u32> {
    let mut ladder = Ladder::new(make_engine, make_initial, base_dir, precisions);
    // Horizons only go down from here: level 0 is complete at the ceiling.
    ladder.drop_level0 = true;
    anyhow::ensure!(ceiling >= 1, "a ceiling of 0 frames");
    match ladder.at_horizon(ceiling)? {
        HorizonOutcome::Confirmed => eprintln!("[search] ceiling: horizon {ceiling} confirmed"),
        HorizonOutcome::Refuted { level } => anyhow::bail!(
            "ceiling {ceiling} REFUTED at level {level}: a known concrete solution the model cannot reproduce"
        ),
    }
    let mut best = ceiling;
    while best > 1 {
        match ladder.at_horizon(best - 1)? {
            HorizonOutcome::Confirmed => {
                eprintln!("[search] horizon {} confirmed, counting down", best - 1);
                best -= 1;
            }
            HorizonOutcome::Refuted { level } => {
                eprintln!("[search] horizon {} refuted at level {level}", best - 1);
                break;
            }
        }
    }
    Ok(best)
}

#[cfg(test)]
mod tests {

    /// A row ref names its queue in full: with spares kept per outcome the
    /// slot vector grows past `POOL_QUEUES`, and a queue index above 255
    /// used to alias another queue's row (room (1,0) f65, 2026-09-14).
    #[test]
    fn row_refs_survive_more_than_256_queue_slots() {
        let mut sink = ForwardSink::empty(false);
        let engine = RefEngine::new().expect("ref engine");
        let skeleton = Block::from_state(&engine.initial_state().expect("initial state")).expect("block").into_rt2();
        for q in 0..300 {
            sink.slots.push(Slot::new(skeleton.clone_block()));
            let s = &mut sink.slots[q];
            s.live = true;
            s.gen = (q * 7) as u16;
            s.keys.push((q as u64, 1));
            s.cells.push(0);
            s.pred_base.push(1000 + q as u64 * 16);
            s.pred_mask.push(1);
            s.last_extra.push(u32::MAX);
        }
        let r = sink.row_ref(299);
        assert!(r & celeste_engine::kernel::RowCache::ID_FLAG == 0);
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 3));
        assert_eq!(sink.slots[299].pred_mask[0], 0b1001);
        assert_eq!(sink.slots[43].pred_mask[0], 1, "no other queue's row was touched");
        // Another slice of the same call: an extra entry on the same row.
        assert!(sink.mark_pred(r, 5000, 2));
        assert_eq!(sink.slots[299].extra, vec![(0, 5000, 0b100u64)]);
        // Lanes up to 63 (a 64-lane group).
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 63));
        assert_eq!(sink.slots[299].pred_mask[0], 0b1001 | (1 << 63));
        // A flushed queue's ref is stale.
        sink.slots[299].clear();
        assert!(!sink.mark_pred(r, 1000 + 299 * 16, 0));
    }
    use super::*;
    use crate::trace::refengine::RefEngine;
    use std::sync::Mutex;

    /// End-to-end proof that the rebuilt outer loop runs: drive `forward_run`
    /// over the trusted reference engine for a few frames from the initial
    /// block, checkpointing each frontier, and prove the checkpoint round-trips
    /// (reload a frame's states and match the count). Uses the interpreter (the
    /// oracle), so it is slow and needs the cart on disk; run explicitly.
    #[test]
    #[ignore]
    fn forward_run_drives_the_reference_engine() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];

        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let result = forward_run(&Mutex::new(engine), init, dir, 4, true, None).expect("forward_run");
        // No win in the 4-frame intro; the run reaches the horizon.
        assert_eq!(result.win_frame, None, "unexpected early win in the intro");
        assert_eq!(result.frames, 4, "expected 4 frames run");
        // Recording was on: a position graph was built.
        let pg = result.pos_graph.expect("pos graph built in record mode");
        eprintln!("[forward_run] pos-graph: {} live cells", pg.live_cells());

        // Every frame 0..=4 was checkpointed sharded and reloads block-by-block.
        for frame in 0..=4 {
            let blocks = load_frame(dir, frame)
                .unwrap_or_else(|e| panic!("reload frame {frame}: {e}"));
            assert!(!blocks.is_empty(), "frame {frame} checkpoint is empty");
            // Per-cell load of every cell returns every row.
            let cells: rustc_hash::FxHashSet<u32> = blocks
                .iter()
                .flat_map(|b| b.positions().unwrap())
                .collect();
            let by_cell = load_frame_cells(dir, frame, &cells).expect("per-cell load");
            let lanes: usize = blocks.iter().map(Block::lanes).sum();
            let lanes_by_cell: usize = by_cell.iter().map(Block::lanes).sum();
            assert_eq!(lanes_by_cell, lanes, "per-cell load lost rows at frame {frame}");
            eprintln!("[forward_run] f{frame}: {} buckets across {} cells", blocks.len(), cells.len());
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the ladder forward filter: one frame, then re-run it
    /// with (a) a marked set containing exactly the widened frame-1 states -
    /// nothing is discarded - and (b) an empty marked set - everything is
    /// discarded and the frontier empties. Uses Bits(0) as the coarser level.
    #[test]
    fn forward_filter_keeps_marked_and_discards_unmarked() {
        use crate::interpreter::abstraction::RemPrecision;
        let bits0 = crate::interpreter::abstraction::Level::for_rem(RemPrecision::Bits(0));
        let engine = RefEngine::new().expect("ref engine");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-filter-test");
        let seed = || vec![Block::from_state(&engine.initial_state().expect("init")).expect("block")];

        // Baseline one frame, no filter.
        {
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, None).expect("baseline");
        }
        let frame1 = load_frame(dir, 1).expect("load f1");
        assert!(!frame1.is_empty(), "baseline produced no frame 1");

        // marked_full = the widened-to-Bits(0) keys of every frame-1 state.
        let mut marked_full = Visited::new();
        for b in &frame1 {
            let (shape, keys, cells) = widened_keys(b, bits0).expect("widened keys");
            for (k, &c) in keys.iter().zip(&cells) {
                marked_full.insert(shape, *k, c);
            }
        }

        // With the full marked set, frame 1 survives.
        {
            let f = MarkFilter::new(&marked_full, bits0);
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, Some(&f)).expect("full-filter run");
            let kept = load_frame(dir, 1).expect("load f1 full");
            assert!(!kept.is_empty(), "full marked set wrongly discarded frame 1");
        }

        // With an empty marked set, everything is discarded.
        {
            let empty = Visited::new();
            let f = MarkFilter::new(&empty, bits0);
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, Some(&f)).expect("empty-filter run");
            let kept = load_frame(dir, 1).expect("load f1 empty");
            assert!(kept.is_empty(), "empty marked set failed to discard frame 1");
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A fresh reference engine behind the lock the `FrameStep` impl needs
    /// (RefEngine isn't Clone). Cheap enough for a test.
    fn engine_clone(_e: &RefEngine) -> Mutex<RefEngine> {
        Mutex::new(RefEngine::new().expect("ref engine"))
    }

    /// The FULL ladder to the concrete level: run every precision - Bits 0..=16
    /// then Exact - on the compiled engine with the synthetic early win, and
    /// require Confirmed (every level, including fully concrete, reaches the win
    /// when filtered by the coarser level's backward marks). This proves the new
    /// search works end to end through refinement, the gate for deleting the old
    /// search. Slow (many compiled forward+backward passes); run explicitly.
    #[test]
    #[ignore]
    fn new_ladder_confirms_to_concrete() {
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-full");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");

        // The real ladder maps k>=16 to Exact (rewrite.rs: `if prev_bits >= 16
        // { Exact }`), so the distinct precisions are Bits(0..=15) then Exact.
        // CELESTE_LADDER_MAXBITS bisects: Bits(0..=maxbits) then Exact.
        let maxbits: u8 = std::env::var("CELESTE_LADDER_MAXBITS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(15);
        let precisions: Vec<crate::interpreter::abstraction::Level> =
            crate::interpreter::abstraction::Level::default_ladder(maxbits);

        let make_engine = |precision: crate::interpreter::abstraction::Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome =
            ladder_at_horizon(make_engine, make_initial, dir, 14, &precisions).expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder-full] all 17 levels through Exact reached the win");
            }
            HorizonOutcome::Refuted { level } => {
                panic!("level {level} (of 17) refuted - the new ladder breaks before concrete");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// The held-button trails unknown at every non-exact level
    /// (plans/held-buttons.md): the ladder still reaches the synthetic win
    /// through Exact. A widened level's spurious retriggers only add states and
    /// marks; if its backward under-marked, the exact rung's filtered forward
    /// would lose the win and refute. Slow; run explicitly.
    #[test]
    #[ignore]
    fn new_ladder_with_held_levels_confirms_to_concrete() {
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-held");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");

        let spec: String = (0..=15).map(|k| format!("r{k}sxh,")).collect::<String>() + "rxsx";
        let precisions = crate::interpreter::abstraction::Level::parse_ladder(&spec).expect("ladder spec");

        let make_engine = |precision: crate::interpreter::abstraction::Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        match ladder_at_horizon(make_engine, make_initial, dir, 14, &precisions).expect("ladder") {
            HorizonOutcome::Confirmed => {}
            HorizonOutcome::Refuted { level } => {
                panic!("level {level} refuted the synthetic win with held-unknown levels")
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// THE LADDER VALIDATION (Philippe's "validate the per-rem-level forwards,
    /// backward equality is implied"). Run the new ladder over rem 0 then rem 1
    /// on the compiled engine, with a synthetic early win at (8,107) in room
    /// (1,0) - reached ~frame 8 on the fall path, with many states per frame.
    /// rem-1's forward is filtered by rem-0's backward marks; if it still finds
    /// the win, `Confirmed`, the backward did NOT drop the winning path. A
    /// backward that under-marks would over-filter rem 1 and it would refute.
    /// This exercises forward + backward + MarkFilter end to end on the kernels.
    #[test]
    #[ignore]
    fn new_ladder_backward_preserves_win_across_rem_levels() {
        use crate::interpreter::abstraction::RemPrecision;
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");

        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");

        let make_engine = |precision: crate::interpreter::abstraction::Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome = ladder_at_horizon(
            make_engine,
            make_initial,
            dir,
            14,
            &[
                crate::interpreter::abstraction::Level::for_rem(RemPrecision::Bits(0)),
                crate::interpreter::abstraction::Level::for_rem(RemPrecision::Bits(1)),
            ],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder] rem0 and rem1 both reached the synthetic win - backward preserved it");
            }
            HorizonOutcome::Refuted { level } => {
                panic!(
                    "level {level} lost the win that a coarser level found - \
                     the backward under-marked (dropped a winning-path state)"
                );
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// The position rung below level 0 (`PosPrecision`): a ladder that starts
    /// at 2 px y-buckets, then level 0, then rem Bits(1), must still confirm
    /// the synthetic win - the coarser level's marks (its keys widened to
    /// the bucket) filter level 0 without losing the winning path, and the
    /// bucket kernels (`IntFrag`: one exact position per fork configuration,
    /// snapped back at the output) reach it at all.
    #[test]
    #[ignore]
    fn new_ladder_position_rung_preserves_win() {
        use crate::interpreter::abstraction::Level;
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-pos-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");
        let make_engine = |precision: Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?) as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(&crate::trace::refengine::RefEngine::new()?.initial_state()?)?])
        };
        // `find_optimum` runs the level-0 forward first (the tree persists
        // across horizons since 2026-09-12; `at_horizon` alone has no tree).
        let levels = Level::parse_ladder("x2y2r0sx,y2r0sx,r0sx,r1sx,rxsx").expect("ladder");
        let with_pos = find_optimum(make_engine, make_initial, dir, 1, 14, &levels[..4]).expect("ladder");
        let _ = std::fs::remove_dir_all(dir);
        let dir2 = std::path::Path::new("/var/tmp/celeste-frame-ladder-pos-test-ref");
        let _ = std::fs::remove_dir_all(dir2);
        std::fs::create_dir_all(dir2).expect("checkpoint dir");
        let make_engine2 = |precision: Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?) as Box<dyn FrameStep>)
        };
        let make_initial2 = || {
            Ok(vec![Block::from_state(&crate::trace::refengine::RefEngine::new()?.initial_state()?)?])
        };
        let plain = Level::parse_ladder("r0sx,r1sx,rxsx").expect("ladder");
        let without = find_optimum(make_engine2, make_initial2, dir2, 1, 14, &plain[..2]).expect("ladder");
        let _ = std::fs::remove_dir_all(dir2);
        eprintln!("[ladder] optimum through the position rung {with_pos:?}, without {without:?}");
        assert!(with_pos.is_some(), "the position rung lost the synthetic win");
        assert_eq!(with_pos, without, "the position rung changed the answer");
        // Counting down from a ceiling finds the same optimum.
        let dir3 = std::path::Path::new("/var/tmp/celeste-frame-ladder-pos-test-down");
        let _ = std::fs::remove_dir_all(dir3);
        std::fs::create_dir_all(dir3).expect("checkpoint dir");
        let make_engine3 = |precision: Level| {
            crate::interpreter::abstraction::set_level(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?) as Box<dyn FrameStep>)
        };
        let make_initial3 = || {
            Ok(vec![Block::from_state(&crate::trace::refengine::RefEngine::new()?.initial_state()?)?])
        };
        let down = find_optimum_from_ceiling(make_engine3, make_initial3, dir3, 14, &levels[..4]).expect("ladder");
        let _ = std::fs::remove_dir_all(dir3);
        assert_eq!(Some(down), with_pos, "counting down from the ceiling changed the answer");
    }

    /// forward_resume, extending a checkpointed forward, reproduces a fresh run:
    /// run fresh to frame 4, capture frame 4's key set, then resume from frame 2
    /// out to 4 (rebuilding visited from the checkpoints) and check frame 4 is
    /// byte-for-byte the same key set.
    #[test]
    #[ignore]
    fn forward_extended_frame_by_frame_matches_fresh() {
        use rustc_hash::FxHashSet;
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-resume-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        let keyset = |frame: u32| -> FxHashSet<(u64, u64)> {
            load_frame(dir, frame)
                .expect("load")
                .iter()
                .flat_map(|b| b.keys().to_vec())
                .collect()
        };

        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            forward_run(&Mutex::new(e), init, dir, 4, true, None).expect("fresh");
        }
        let fresh4 = keyset(4);
        assert!(!fresh4.is_empty(), "fresh frame 4 empty");

        // The same run extended one frame at a time - the outer loop's
        // "one more frame at rem zero" - lands on the same key set.
        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            let e = Mutex::new(e);
            let mut st = ForwardState::start(init, dir, true).expect("start");
            assert_eq!(pos_graph_frame(dir), Some(0), "start must leave a graph beside f0");
            for to in 1..=4 {
                st.extend(&e, dir, to, None).expect("extend");
                assert_eq!(st.frames, to);
                // Every frame boundary leaves a graph covering the frame: a
                // crash here resumes (the graph was saved only when a
                // forward ended, so a crash lost it, 2026-09-16).
                assert_eq!(pos_graph_frame(dir), Some(to), "the pos graph must be saved with frame {to}");
            }
            assert!(st.pos_graph().is_some());
        }
        let extended4 = keyset(4);
        assert_eq!(fresh4, extended4, "extension diverged from fresh at frame 4");
        eprintln!("[extend] frame 4 key set identical: {} keys", fresh4.len());

        // RESUME: a fresh run to 6 versus the tree above resumed at 4 and
        // extended to 6 (door and pos graph rebuilt from the layers).
        let fresh_dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-resume-test-fresh6");
        let _ = std::fs::remove_dir_all(fresh_dir);
        std::fs::create_dir_all(fresh_dir).expect("mkdir");
        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            forward_run(&Mutex::new(e), init, fresh_dir, 6, true, None).expect("fresh 6");
        }
        {
            let e = Mutex::new(RefEngine::new().expect("engine"));
            let mut st = ForwardState::resume(dir, true).expect("resume").expect("a tree to resume");
            assert_eq!(st.frames, 4);
            let door_before = st.visited_len();
            st.extend(&e, dir, 6, None).expect("extend resumed");
            assert!(st.visited_len() >= door_before);
        }
        let keyset_in = |d: &std::path::Path, frame: u32| -> FxHashSet<(u64, u64)> {
            load_frame(d, frame).expect("load").iter().flat_map(|b| b.keys().to_vec()).collect()
        };
        assert_eq!(keyset_in(fresh_dir, 6), keyset_in(dir, 6), "resumed run diverged from fresh at frame 6");
        assert_eq!(keyset_in(fresh_dir, 5), keyset_in(dir, 5), "resumed run diverged from fresh at frame 5");
        eprintln!("[resume] frames 5 and 6 identical after resuming at 4");
        let _ = std::fs::remove_dir_all(dir);
        let _ = std::fs::remove_dir_all(fresh_dir);
    }

    /// End-to-end wiring of the ladder's forward+refute path: run one level with
    /// the reference over the intro (which has no win in 4 frames), and check
    /// the ladder reports Refuted. Exercises make_engine/make_initial, the
    /// filtered forward, and the no-win -> Refuted branch. (The Optimal/backward
    /// branch needs a real winning room - the "together" validation.)
    #[test]
    #[ignore]
    fn ladder_refutes_when_no_win_by_horizon() {
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-ladder-test");
        let _ = std::fs::remove_dir_all(dir);
        use crate::interpreter::abstraction::RemPrecision;
        let outcome = ladder_at_horizon(
            |_precision| Ok(Box::new(Mutex::new(RefEngine::new()?)) as Box<dyn FrameStep>),
            || Ok(vec![Block::from_state(&RefEngine::new()?.initial_state()?)?]),
            dir,
            4,
            &[crate::interpreter::abstraction::Level::for_rem(RemPrecision::Bits(0))],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Refuted { level } => {
                assert_eq!(level, 0, "refuted at the wrong level");
                eprintln!("[ladder] refuted at level={level} (no win in the 4-frame intro)");
            }
            HorizonOutcome::Confirmed => {
                panic!("ladder wrongly reported Confirmed for the win-less intro");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the backward walk (the intro has no real win, so we
    /// seed artificially). Run 4 forward frames recording the pos-graph, then
    /// seed the backward from ALL of frame 4's states and walk back. The intro
    /// is a deterministic chain (one state per frame), so backward must mark a
    /// state at every earlier frame, and - the invariant Philippe named - it
    /// must re-run no more states than forward produced.
    #[test]
    #[ignore]
    fn backward_walk_propagates_along_the_intro_chain() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-bwd-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let horizon = 4;
        let engine = Mutex::new(engine);
        let fwd = forward_run(&engine, init, dir, horizon, true, None).expect("forward");
        let graph = fwd.pos_graph.expect("pos graph");

        // Seed from every state at the horizon frame.
        let seed: Vec<(u64, (u64, u64), u32)> = load_frame(dir, horizon)
            .expect("load horizon")
            .iter()
            .flat_map(|b| {
                let shape = b.shard_shape();
                let keys = b.keys().to_vec();
                let cells = b.positions().expect("cells");
                keys.into_iter().zip(cells).map(move |(k, c)| (shape, k, c)).collect::<Vec<_>>()
            })
            .collect();
        assert!(!seed.is_empty(), "no states at the horizon to seed from");

        let bwd = backward_walk_in(&engine, dir, horizon, &graph, seed).expect("backward");
        eprintln!(
            "[backward] marked {} states, {} re-runs",
            bwd.marked.len(),
            bwd.reruns
        );
        // The deterministic chain: a state marked at every frame 1..=horizon.
        assert!(
            bwd.marked.len() >= horizon as usize,
            "backward marked {} states, expected >= {}",
            bwd.marked.len(),
            horizon
        );
        // Philippe's cost invariant: backward re-runs a position-filtered subset,
        // so it must not exceed the forward frame count (states produced).
        let fwd_states: usize = (1..=horizon)
            .map(|f| load_frame(dir, f).expect("load").iter().map(|b| b.lanes()).sum::<usize>())
            .sum();
        assert!(
            (bwd.reruns as usize) <= fwd_states,
            "backward re-ran {} > forward {} states - narrowing is broken",
            bwd.reruns,
            fwd_states
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}
