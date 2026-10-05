//! The block, the frame-step interface and the forward: `FrameStep` (the
//! kernels or the reference engine), the `ForwardSink` a step emits into
//! (queues, the door, edge records with transfers), `forward_frame` (one
//! wave) and `ForwardState` (a forward extended frame by frame, checkpointed,
//! resumable). The search over a finished forward is `search::arc_dp`.
//!
//! A block is OPAQUE to the loop except for its KEYS and POSITIONS.

use anyhow::{Context, Result};
use crate::search::door::Admit;
use std::ops::Range;
use std::sync::atomic::{AtomicUsize, Ordering};

use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Col, Rt2, AV};

/// A columnar batch of lanes of one shape: the kernels' own `Rt2` with its
/// key column. Only the per-lane key and position are exposed.
pub struct Block {
    rt2: Rt2,
    /// The rows' stable ids `pack_id(layer, seq, row)` when the block is a
    /// layer's piece; empty otherwise.
    ids: Vec<u64>,
    /// The piece's file seq within its layer (`s{shape}_{seq}.bin`).
    seq: u32,
    /// Lanes the frame step must not expand (e.g. won rows); empty when none.
    /// A mask rather than a drop so the ids stay consecutive.
    skip: Vec<bool>,
}

/// A state's stable id: its layer (the frame it was first reached), its
/// piece file in that layer, and its row in the file. Assigned at admission,
/// never renumbered.
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
    /// Wrap an engine block. Its key column must be present: nothing
    /// downstream re-hashes.
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

    /// A reference-engine block keyed as the level's kernels key it (held
    /// buttons widened; objects stay decided, which is exact).
    pub fn keyed(mut rt2: Rt2) -> Result<Self> {
        let level = crate::abstraction::Level { held: crate::abstraction::current_level().held, ..crate::abstraction::Level::EXACT };
        let (_, keys, _) = widened_keys_rt2(&rt2, level)?;
        rt2.row_keys_canonical();
        rt2.row_keys = keys;
        Ok(Block { rt2, ids: Vec::new(), seq: 0, skip: Vec::new() })
    }

    pub fn into_rt2(self) -> Rt2 {
        self.rt2
    }

    pub fn rt2(&self) -> &Rt2 {
        &self.rt2
    }

    /// The 128-bit canonical row key per lane (shape + content): the identity
    /// for dedup, the door and checkpoints.
    pub fn keys(&self) -> &[(u64, u64)] {
        &self.rt2.row_keys
    }

    /// The player-position cell per lane.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::block_cells(&self.rt2)
    }

    /// Per lane: does it sit on the room's win target (`wins_of`)?
    pub fn wins(&self) -> Result<Vec<bool>> {
        wins_of(&self.rt2)
    }

    /// Per lane: has it left the start room (`exits_of`, a superset of wins)?
    pub fn exits(&self) -> Result<Vec<bool>> {
        exits_of(&self.rt2)
    }

    /// Keep only the lanes whose mask entry is true; `None` if none survive.
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

    /// The block's shape.
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

/// The summit (room (6,3)) has no exit; it finishes when the player touches
/// the original cart's FLAG, 5 px right of its tile (118) at (fx, fy): with
/// both hitboxes, fx-6 <= x <= fx+6 and fy-7 <= y <= fy+4.
pub const SUMMIT_LEVEL: i16 = 30;

/// The winning player positions as an inclusive whole-pixel rect (x_lo,
/// x_hi, y_lo, y_hi) - `CELESTE_WIN_AT_XY`'s point or the summit's flag; a
/// lane wins where its position MEETS it. None: the win is the room exit.
pub fn win_rect() -> Option<(i16, i16, i16, i16)> {
    static RECT: std::sync::OnceLock<Option<(i16, i16, i16, i16)>> = std::sync::OnceLock::new();
    *RECT.get_or_init(|| {
        if let Some((x, y)) = crate::abstraction::synthetic_win_xy() {
            return Some((x, x, y, y));
        }
        let (rx, ry) = crate::game_runner::start_room();
        if crate::game_runner::level_index(rx, ry) != SUMMIT_LEVEL {
            return None;
        }
        let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
        let cart = celeste_core::cart_data::CartData::load(std::path::Path::new(&root).join("cart"))
            .expect("summit win: load the cart");
        let flags: Vec<(i16, i16)> = (0..16i16)
            .flat_map(|ty| (0..16i16).map(move |tx| (tx, ty)))
            .filter(|&(tx, ty)| cart.mget_whole(rx * 16 + tx, ry * 16 + ty) == 118)
            .collect();
        let [(tx, ty)] = flags[..] else { panic!("summit win: want one flag tile (118) in room ({rx},{ry}), found {flags:?}") };
        let (fx, fy) = (tx * 8 + 5, ty * 8);
        let rect = (fx - 6, fx + 6, fy - 7, fy + 4);
        eprintln!("[win] summit: the flag at ({fx}, {fy}); the player wins at x {}..={}, y {}..={}", rect.0, rect.1, rect.2, rect.3);
        Some(rect)
    })
}

/// Per lane: has it left the start room (`room` is the win room), or, where
/// there is a `win_rect`, does its position meet it?
pub fn exits_of(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    if let Some((txl, txh, tyl, tyh)) = win_rect() {
        let Some(obj) = crate::search::pos_graph::player_object(rt2) else {
            return Ok(vec![false; lanes]);
        };
        // An interval position wins where it meets the target (as `any_win`).
        let axis = |f: u32| {
            rt2.obj_field_cell(obj, f)
                .and_then(|c| crate::search::pos_graph::whole_range_col(rt2, c))
        };
        return Ok(match (axis(ids.f_x), axis(ids.f_y)) {
            (Some(xs), Some(ys)) => xs
                .iter()
                .zip(&ys)
                .map(|(&(xl, xh), &(yl, yh))| xl <= txh && txl <= xh && yl <= tyh && tyl <= yh)
                .collect(),
            _ => vec![false; lanes],
        });
    }
    // Both coordinates: the exit from a row's last room wraps to the next row.
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

/// Per lane: does it sit on the room's win target? The room exit
/// (`exits_of`), and in the orb room the orb taken too (`orb_required`).
pub fn wins_of(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::AV;
    let exits = exits_of(rt2)?;
    if win_rect().is_some() || !orb_required() {
        return Ok(exits);
    }
    let (ids, lanes) = (crate::compiled::ids(), rt2.width);
    let has_orb: Vec<bool> = {
        let c = rt2.globals[ids.g_max_djump as usize];
        anyhow::ensure!(c != celeste_engine::runtime2::NONE, "wins: no `max_djump` global");
        let two = crate::pico8_num::Pico8Num::from_i16(2);
        (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(two)).collect()
    };
    Ok(exits.iter().zip(&has_orb).map(|(e, o)| *e && *o).collect())
}

/// A SOUND LOWER BOUND on the frames from the orb room's big chest opening
/// to a win, read off the cart: the paused player waits 61 frames for the
/// orb, the orb is collectable after 8 more (`spd.y` -4 -> 0 by 0.5),
/// collecting it freezes 10 frames, then the exit: at least 79. Taken as 70,
/// a margin of 9.
pub const ORB_MIN_FRAMES_AFTER_CHEST: u32 = 70;

/// Per lane, in the orb room: is its big chest still CLOSED at `frame`, too
/// late to open it and still win by `horizon`? Such a row cannot win, so it
/// is not expanded. `horizon` is the run's ceiling.
pub fn orb_deadline_skip(rt2: &Rt2, frame: u32, horizon: u32) -> Vec<bool> {
    use celeste_engine::runtime2::AV;
    let lanes = rt2.width;
    if frame + 1 + ORB_MIN_FRAMES_AFTER_CHEST <= horizon {
        return vec![false; lanes];
    }
    let ids = crate::compiled::ids();
    let Some(&chest) = rt2.objects_of_type(ids, ids.g_big_chest).first() else {
        return vec![false; lanes];
    };
    let Some(c) = rt2.obj_field_cell(chest, ids.f_state) else { return vec![false; lanes] };
    let zero = crate::pico8_num::Pico8Num::from_i16(0);
    (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(zero)).collect()
}

/// Is the start room the orb room (5,2)? Its orb gives every later level the
/// second dash, so a win there also needs `max_djump == 2`; leaving without
/// it is not the level's exit as the game is played.
pub fn orb_required() -> bool {
    let (x, y) = crate::game_runner::start_room();
    crate::game_runner::level_index(x, y) == crate::game_runner::ORB_LEVEL
}

/// Worker count: `CELESTE_THREADS`, else one per physical core (the kernels
/// are AVX-512 bound and SMT siblings share those units), at least 1.
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

/// A typed varying column of emitted rows: raw 16.16 words, (low, high)
/// pairs, or a byte per bool (0 false, 1 true, 2 unknown).
pub enum TCol {
    Num(Vec<u32>),
    Ival(Vec<(u32, u32)>),
    Bool(Vec<u8>),
}

/// A queue of emitted rows of one (shape, cell): the skeleton (canonical
/// structure, uniform columns), one `TCol` per varying cell, the key and
/// cell per row, and each row's predecessors for the edge records.
pub struct Slot {
    pub shape: u64,
    /// The queue's key while live (`ForwardSink::queue`).
    pub outcome: u64,
    pub cell: u32,
    pub live: bool,
    pub touched: bool,
    skeleton: Rt2,
    /// `(cell, column)` per varying cell, in the kernel's order.
    pub cols: Vec<(usize, TCol)>,
    pub keys: Vec<(u64, u64)>,
    pub cells: Vec<u32>,
    /// The row's predecessors in the slice that emitted it: the slice's
    /// first input id and a bit per lane (`ForwardSink::ids_in`).
    pub pred_base: Vec<u64>,
    pub pred_mask: Vec<u64>,
    /// The remainder transfer of the lanes in `pred_mask`
    /// (`ForwardSink::xfer_id`): lanes with another transfer go to `extra`.
    pub pred_xfer: Vec<u32>,
    /// Predecessors from OTHER slices of the same kernel call, or with
    /// another transfer (a row is pushed once per call; its later producers
    /// land here): `(row, slice base, transfer, lane mask)`.
    pub extra: Vec<(u32, u64, u32, u64)>,
    /// Per row, the index of its latest `extra` entry (`u32::MAX`: none);
    /// the kernel emits slice by slice, so the current slice merges into it.
    pub last_extra: Vec<u32>,
    /// Bumped at every flush, so a row ref (`ForwardSink::row_ref`) into a
    /// flushed queue is recognised as stale.
    pub gen: u16,
}

impl Slot {
    /// A slot over `skeleton`: a width-0 block, empty typed columns for its
    /// varying cells.
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
        Slot {
            shape: skeleton.shape_hash,
            outcome: 0,
            cell: 0,
            live: false,
            touched: false,
            skeleton,
            cols,
            keys: Vec::new(),
            cells: Vec::new(),
            pred_base: Vec::new(),
            pred_mask: Vec::new(),
            pred_xfer: Vec::new(),
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
        self.pred_xfer.clear();
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

    /// Append `rows` of this slot to `piece` (a block of this shape). A cell
    /// the piece holds differently (uniform vs varying, or another uniform
    /// value) goes through `col_push`.
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

    /// The whole slot as a block (the checks read blocks).
    pub fn to_rt2(&self) -> Rt2 {
        let mut b = self.empty_piece();
        let all: Vec<u32> = (0..self.rows() as u32).collect();
        self.gather_into(&mut b, &all);
        b
    }

    /// Does any of `rows` sit on the win target? `wins_of`'s test on the
    /// slot's columns.
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
        if let Some((txl, txh, tyl, tyh)) = win_rect() {
            let Some(obj) = crate::search::pos_graph::player_object(sk) else {
                return Ok(false);
            };
            let (Some(cx), Some(cy)) = (sk.obj_field_cell(obj, ids.f_x), sk.obj_field_cell(obj, ids.f_y)) else {
                return Ok(false);
            };
            // An interval position wins if it meets the target: an
            // over-approximation the concrete search refutes.
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
                xl <= txh && txl <= xh && yl <= tyh && tyl <= yh
            }));
        }
        let (wx, wy) = crate::game_runner::win_room();
        let (wx, wy) = (P8::from_i16(wx), P8::from_i16(wy));
        let room = sk.global_target(ids.g_room).ok_or_else(|| anyhow::anyhow!("any_win: no `room` global"))?;
        let x = sk.obj_field_cell(room, ids.f_x).ok_or_else(|| anyhow::anyhow!("any_win: room has no x"))?;
        let y = sk.obj_field_cell(room, ids.f_y).ok_or_else(|| anyhow::anyhow!("any_win: room has no y"))?;
        let (xs, ys) = (num_at(x)?, num_at(y)?);
        if orb_required() {
            let c = sk.globals[ids.g_max_djump as usize];
            anyhow::ensure!(c != celeste_engine::runtime2::NONE, "any_win: no `max_djump` global");
            let md = num_at(c)?;
            let two = P8::from_i16(2);
            return Ok(rows.iter().any(|&r| xs.at(r) == wx && ys.at(r) == wy && md.at(r) == two));
        }
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
    xfer: u32,
    mask: u64,
) -> Result<()> {
    let layer = id_layer(target) as usize;
    if edge_bufs.len() <= layer {
        edge_bufs.resize_with(layer + 1, Vec::new);
    }
    let buf = &mut edge_bufs[layer];
    crate::search::edges::encode_record(buf, target, base, xfer, mask);
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

/// Rows per queue; a full queue is flushed. Small so a worker's whole pool
/// stays cache-resident.
pub const QUEUE_ROWS: usize = 256;
/// Live queues per worker; beyond this a new (outcome, cell) evicts one
/// (second chance). Eviction only changes how many flushes happen.
pub const POOL_QUEUES: usize = 256;

/// One worker's sink for a frame: keyed rows into a QUEUE per (outcome,
/// cell), with predecessors and transfers, plus pos-graph edges. A full or
/// evicted queue is FLUSHED by this worker: filtered, admitted at the door
/// (locked per shard), appended to its piece, edges recorded.
pub struct ForwardSink<'a> {
    /// The pool: every queue ever created by this sink, live or spare.
    pub slots: Vec<Slot>,
    /// (outcome id, cell) -> live queue. The outcome id is the emitter's:
    /// a kernel's template address, the reference engine's shape hash.
    index: rustc_hash::FxHashMap<(u64, u32), u32>,
    /// Flushed, empty queues per outcome id, keeping their columns.
    spare: rustc_hash::FxHashMap<u64, Vec<u32>>,
    /// The second-chance hand over `slots`.
    clock: usize,
    /// The run cache: rows arrive in runs of one (outcome, cell).
    last: ((u64, u32), u32),
    door: Option<&'a crate::search::door::Door>,
    filter: Option<&'a MarkFilter<'a>>,
    /// The ids of the block being run, per lane; the step reads `ids_in[lo]`
    /// as the slice's base and records predecessors as masks over the slice.
    pub ids_in: Option<&'a [u64]>,
    /// Lanes of the block being run that must not be expanded.
    pub skip_in: Option<&'a [bool]>,
    /// Where the edge records go (`edges::raw_path`); `None`: no recording.
    edges_dir: Option<std::path::PathBuf>,
    edge_bufs: Vec<Vec<u8>>,
    pub edge_records: u64,
    /// The within-call dedup cache (`RowCache`), written back by the flush.
    pub seen: celeste_engine::kernel::RowCache,
    /// Direct-mapped merge of edges to already-flushed states, `(target,
    /// base, transfer, mask)`; an evicted or drained entry becomes a record.
    direct: Vec<(u64, u64, u32, u64)>,
    /// This worker's interned transfer pairs (`search::arc_edges`); records
    /// carry the index, the table goes beside them (`edges::raw_xfer_path`).
    xfer_ids: rustc_hash::FxHashMap<crate::search::arc_edges::Pair, u32>,
    xfer_tab: Vec<crate::search::arc_edges::Pair>,
    /// Time spent encoding and writing edge records (this worker).
    pub t_edges: std::time::Duration,

    /// This worker's next-frame rows, one piece per shape: the block, its
    /// file seq in the layer (`worker * 256 + k`), and its rows' ids.
    pieces: rustc_hash::FxHashMap<u64, (Rt2, u32, Vec<u64>)>,
    /// The new rows' layer and this worker's index (part of a new row's id).
    frame: u32,
    worker: u32,
    pub won: bool,
    pub kept: usize,
    pub flushes: u64,
    pub flushed_rows: u64,
    sort_buf: Vec<((u64, u64), u32)>,
    keys_buf: Vec<(u64, u64)>,
    uniq_buf: Vec<u32>,
    row_uniq: Vec<u32>,
    new_buf: Vec<u32>,
    ids_buf: Vec<u64>,
    rows_buf: Vec<u32>,
    /// Record `edges` at all (off for the backward, which has the graph).
    pub edges_on: bool,
    /// The distinct pos-graph edges this worker produced.
    pub edges: rustc_hash::FxHashSet<(u32, u32)>,
    /// Rows emitted after the step's within-call dedup (the raw fan-out).
    pub emitted: u64,
}

const NO_QUEUE: ((u64, u32), u32) = ((u64::MAX, u32::MAX), u32::MAX);

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
            xfer_ids: Default::default(),
            xfer_tab: Vec::new(),
            t_edges: std::time::Duration::ZERO,
            pieces: Default::default(),
            frame: 0,
            worker: 0,
            won: false,
            kept: 0,
            flushes: 0,
            flushed_rows: 0,
            sort_buf: Vec::with_capacity(QUEUE_ROWS),
            keys_buf: Vec::with_capacity(QUEUE_ROWS),
            uniq_buf: Vec::with_capacity(QUEUE_ROWS),
            row_uniq: Vec::with_capacity(QUEUE_ROWS),
            new_buf: Vec::with_capacity(QUEUE_ROWS),
            ids_buf: Vec::with_capacity(QUEUE_ROWS),
            rows_buf: Vec::with_capacity(QUEUE_ROWS),
            edges_on,
            edges: Default::default(),
            emitted: 0,

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

    /// The transfer pair `t` as this worker's id (interned on first use).
    #[inline]
    pub fn xfer_id(&mut self, t: crate::search::arc_edges::Pair) -> u32 {
        if let Some(&id) = self.xfer_ids.get(&t) {
            return id;
        }
        let id = u32::try_from(self.xfer_tab.len()).expect("transfer table past u32");
        self.xfer_tab.push(t);
        self.xfer_ids.insert(t, id);
        id
    }

    /// One edge record: `target` (a state id) has the lanes of `mask` in
    /// the slice based at `base` as predecessors, with the transfer `xfer`.
    #[inline]
    fn record(&mut self, target: u64, base: u64, xfer: u32, mask: u64) -> Result<()> {
        let dir = self.edges_dir.as_deref().expect("recording without an edges dir");
        append_record(&mut self.edge_bufs, &mut self.edge_records, dir, self.frame, self.worker, target, base, xfer, mask)
    }

    /// Lane `lane` of the slice based at `base` produced the already flushed
    /// state `target` with transfer `xfer`: merged in the direct-mapped
    /// cache, recorded on eviction.
    #[inline]
    pub fn direct_edge(&mut self, target: u64, base: u64, xfer: u32, lane: usize) {
        if self.edges_dir.is_none() {
            return;
        }
        if self.direct.is_empty() {
            self.direct = vec![(u64::MAX, 0, 0, 0); DIRECT_SLOTS];
        }
        let i = (celeste_engine::runtime2::mix64(target ^ base.rotate_left(17) ^ (xfer as u64).rotate_left(41)) as usize) & (DIRECT_SLOTS - 1);
        let e = self.direct[i];
        if e.0 == target && e.1 == base && e.2 == xfer {
            self.direct[i].3 |= 1u64 << lane;
            return;
        }
        if e.0 != u64::MAX {
            self.record(e.0, e.1, e.2, e.3).expect("recording an edge");
        }
        self.direct[i] = (target, base, xfer, 1u64 << lane);
    }

    /// The step's end of a kernel call: the merge cache drains.
    pub fn end_call(&mut self) {
        for i in 0..self.direct.len() {
            let e = self.direct[i];
            if e.0 != u64::MAX {
                self.record(e.0, e.1, e.2, e.3).expect("recording an edge");
                self.direct[i] = (u64::MAX, 0, 0, 0);
            }
        }
    }

    /// A handle on the row just pushed into queue `q`: the row (8 bits), the
    /// queue (24 bits: spares are kept per outcome, so the slot vector grows
    /// past `POOL_QUEUES`) and the queue's generation (16 bits), below the
    /// cache's flag bits.
    #[inline]
    pub fn row_ref(&self, q: usize) -> u64 {
        const _: () = assert!(QUEUE_ROWS <= 256);
        assert!(q < 1 << 24, "queue pool past 2^24 slots");
        let s = &self.slots[q];
        ((s.gen as u64) << 32) | ((q as u64) << 8) | (s.rows() as u64 - 1)
    }

    /// Lane `lane` of the slice based at `base` also produced the row
    /// `row_ref` points at, with the transfer `xfer`. False if that queue
    /// was flushed since (the row is gone; the caller pushes the row again).
    #[inline]
    pub fn mark_pred(&mut self, row_ref: u64, base: u64, xfer: u32, lane: usize) -> bool {
        let (gen, q, r) = ((row_ref >> 32) as u16, ((row_ref >> 8) & 0xff_ffff) as usize, (row_ref & 0xff) as usize);
        let s = &mut self.slots[q];
        if !s.live || s.gen != gen || r >= s.pred_mask.len() {
            return false;
        }
        if s.pred_base[r] == base && s.pred_xfer[r] == xfer {
            s.pred_mask[r] |= 1u64 << lane;
            return true;
        }
        let last = s.last_extra[r];
        if last != u32::MAX && s.extra[last as usize].1 == base && s.extra[last as usize].2 == xfer {
            s.extra[last as usize].3 |= 1u64 << lane;
        } else {
            s.last_extra[r] = s.extra.len() as u32;
            s.extra.push((r as u32, base, xfer, 1u64 << lane));
        }
        true
    }

    /// The live queue for `(outcome, cell)`, created on first use (a spare of
    /// the outcome, or over `init`'s skeleton), evicting when the pool is full.
    pub fn queue(&mut self, outcome: u64, cell: u32, init: impl FnOnce() -> Rt2) -> usize {
        if self.last.0 == (outcome, cell) {
            return self.last.1 as usize;
        }
        let q = match self.index.get(&(outcome, cell)) {
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
                s.live = true;
                s.touched = false;
                self.index.insert((outcome, cell), q);
                q
            }
        };
        self.last = ((outcome, cell), q);
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
            // The coarser level's filter (`MarkFilter`).
            let allow = match self.filter {
                Some(f) => Some(f.allowed(&slot.to_rt2(), self.frame)?),
                None => None,
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
            // Per ROW, its index among the distinct keys (`u32::MAX`: filtered).
            self.row_uniq.clear();
            self.row_uniq.resize(n, u32::MAX);
            for (e, &u) in self.sort_buf.iter().zip(&self.uniq_buf) {
                self.row_uniq[e.1 as usize] = u;
            }
            self.new_buf.clear();
            self.ids_buf.clear();
            // The door hands the k-th new key `first_new + k`; the rows are
            // appended to the piece in that same order.
            let n_pieces = self.pieces.len() as u32;
            let (frame, worker) = (self.frame, self.worker);
            let (piece, seq, ids) = self
                .pieces
                .entry(slot.shape)
                .or_insert_with(|| (slot.empty_piece(), worker * 256 + n_pieces, Vec::new()));
            let first_new = pack_id(frame, *seq, piece.width as u32);
            door.admit(slot.shape, slot.cell, &self.keys_buf, first_new, &mut self.ids_buf, &mut self.new_buf);
            // The edges: each row's predecessor mask to its state, plus the
            // extras. Rows from id-less blocks have no predecessors.
            if let Some(edges_dir) = self.edges_dir.as_deref().filter(|_| slot.pred_base.len() == n) {
                let t_e = std::time::Instant::now();
                let (row_uniq, ids_buf) = (&self.row_uniq, &self.ids_buf);
                let (edge_bufs, edge_records, frame, worker) = (&mut self.edge_bufs, &mut self.edge_records, self.frame, self.worker);
                let rows = slot.pred_base.iter().zip(&slot.pred_xfer).zip(&slot.pred_mask).enumerate().map(|(r, ((&b, &x), &m))| (r as u32, b, x, m));
                for (r, b, x, m) in rows.chain(slot.extra.iter().copied()) {
                    let u = row_uniq[r as usize];
                    if u != u32::MAX {
                        append_record(edge_bufs, edge_records, edges_dir, frame, worker, ids_buf[u as usize], b, x, m)?;
                    }
                }
                // Each row's fate back into the dedup cache, so a later
                // re-emission in this call records straight to the id.
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
                slot.gather_into(piece, &self.rows_buf);
                ids.extend((0..self.rows_buf.len() as u32).map(|k| first_new + k as u64));
                debug_assert_eq!(ids.len(), piece.width);
            }
            slot.clear();
        }
        slot.live = false;
        slot.touched = false;
        self.index.remove(&(slot.outcome, slot.cell));
        self.spare.entry(slot.outcome).or_default().push(q as u32);
        if self.last.1 == q as u32 {
            self.last = NO_QUEUE;
        }
        Ok(())
    }

    /// End of the worker's frame: flush every live queue, write the edge
    /// buffers and transfers, hand back the pieces. Counters are final after.
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
            if !self.xfer_tab.is_empty() {
                let mut buf = Vec::with_capacity(self.xfer_tab.len() * crate::search::arc_edges::PAIR_BYTES);
                for p in &self.xfer_tab {
                    crate::search::arc_edges::encode_pair(&mut buf, p);
                }
                let path = crate::search::edges::raw_xfer_path(&dir, self.frame, self.worker);
                std::fs::create_dir_all(path.parent().expect("a raw dir"))?;
                std::fs::write(path, buf)?;
            }
        }
        let mut out = Vec::new();
        for (mut p, seq, ids) in std::mem::take(&mut self.pieces).into_values() {
            // An empty piece is dropped: id-less, it would be renumbered at
            // the checkpoint and collide with a real seq of this layer.
            if p.width == 0 {
                continue;
            }
            debug_assert_eq!(ids.len(), p.width);
            // A column every row agrees on becomes uniform (the checkpoint
            // gates are pinned with this rule).
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
        let q = self.queue(row.shape_hash, cell, || {
            // The skeleton from the row: numbers and bools vary, the rest is
            // uniform (fixed by the shape).
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


/// THE LEVEL -1 FILTER (`CELESTE_LEVEL_MINUS_ONE="H,S"`,
/// plans/level-minus-one.md): drop a queue (one player cell) when the table
/// proves its rows cannot exit by H. Sound because the table's ranges are
/// inductive, a clipped successor counts as an exit and a death as a respawn.
/// H must be the LARGEST horizon the run tests (the ceiling). Built once, at
/// the first flush, on a thread with the tracer's stack.
fn level_minus_one() -> Option<(u32, &'static crate::trace::level_minus_one::CostToGo)> {
    static TABLE: std::sync::OnceLock<Option<(u32, crate::trace::level_minus_one::CostToGo)>> = std::sync::OnceLock::new();
    TABLE
        .get_or_init(|| {
            let s = std::env::var("CELESTE_LEVEL_MINUS_ONE").ok()?;
            // The table measures the room EXIT; pruning a position win by it
            // is unsound.
            assert!(win_rect().is_none(), "CELESTE_LEVEL_MINUS_ONE: the table measures the room exit; this room's win is a position");
            let (h, sp) = s.split_once(',').expect("CELESTE_LEVEL_MINUS_ONE=\"H,S\"");
            let h: u32 = h.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE horizon");
            let sp: i32 = sp.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE speed bound (px per frame)");
            let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
            let threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
            let t = std::time::Instant::now();
            let table = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || crate::trace::level_minus_one::cost_to_go(std::path::Path::new(&root), sp, threads))
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

/// THE TIME BAND (an estimate, not a bound; for the level -1 probe): can a
/// player at `cell` not reach y < -4 by `h` at `px` pixels per frame up?
/// Never true without a player or at x >= 128 (already left the room).
pub(crate) fn cell_too_late(cell: u32, frame: u32, h: u32, px: i32) -> bool {
    let Some((x, y)) = crate::search::pos_graph::cell_xy(cell) else { return false };
    if x >= 128 {
        return false;
    }
    let frames = ((y + 5).max(0) + px - 1) / px;
    frame + (frames.max(1) - 1) as u32 > h
}

/// THE OBJECTS LADDER's link: a finer level's forward keeps a row at frame t
/// only if its projection onto the coarser level (`Rt2::widen_to`) is a node
/// the coarser level ARC-MARKED with a deadline >= t (`arc_dp::arc_marks`).
/// Sound: a fine state that wins from t with remainder r projects to a coarse
/// node that wins from t with r (the coarse level over-approximates, the
/// remainder is exact in both), so the node is marked at t. Membership
/// without the deadline would admit nodes marked only with no time left.
pub struct MarkFilter<'a> {
    marked: &'a Visited,
    coarser: crate::abstraction::Level,
}

impl<'a> MarkFilter<'a> {
    pub fn new(marked: &'a Visited, coarser: crate::abstraction::Level) -> Self {
        MarkFilter { marked, coarser }
    }

    /// Per lane of `rt2` (rows of frame `frame`): admitted?
    pub fn allowed(&self, rt2: &Rt2, frame: u32) -> Result<Vec<bool>> {
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser)?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| self.marked.deadline(shape, *k, c).is_some_and(|d| u32::from(d) >= frame)).collect())
    }
}

/// Each lane's `(shape, key, cell)` at the `coarser` level: the COMPLETE
/// coarsening its kernels bake into their rows (`Rt2::widen_to`), then the
/// canonical key. A concrete state's node at the level.
pub fn widened_keys(
    block: &Block,
    coarser: crate::abstraction::Level,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    widened_keys_rt2(&block.rt2, coarser)
}

/// `rt2` projected IN PLACE onto `level`: the row the level's kernels would
/// store for it.
pub fn widen_rt2_to(rt2: &mut Rt2, level: crate::abstraction::Level) {
    rt2.widen_to(crate::compiled::ids(), level.held, level.fruit, level.floors_near, level.platforms);
}

/// `(shape, keys, cells)` of the widened rows; the shape is the widened
/// block's, which a level's nodes are keyed by.
pub fn widened_keys_rt2(
    rt2: &Rt2,
    coarser: crate::abstraction::Level,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    let mut w = rt2.clone_block();
    widen_rt2_to(&mut w, coarser);
    let keys = w.row_keys_canonical();
    let cells = crate::search::pos_graph::block_cells(&w)?;
    anyhow::ensure!(
        keys.len() == rt2.width && cells.len() == rt2.width,
        "the widening changed the lane count"
    );
    Ok((w.shape_hash, keys, cells))
}

/// The one interface between the loop and the engines: input lanes in, every
/// successor row out, already branched, widened for the level and keyed.
/// Implemented by the compiled kernels and the reference engine
/// (`trace::refengine`); the loop never knows which.
pub trait FrameStep: Sync {
    /// One frame of lanes `lanes` of `block` (cells `cell_in`) into `sink`.
    /// Called from many workers at once on disjoint ranges: scratch lives in
    /// the sink or behind a lock.
    fn run(&self, block: &Block, cell_in: &[u32], lanes: Range<usize>, sink: &mut ForwardSink) -> Result<()>;
}

/// Lanes per unit of work (one kernel call; the within-call dedup spans it).
/// 1024 balances load (bigger units leave workers idle behind heavy kernels)
/// against the dedup window. `CELESTE_UNIT_LANES` overrides it (a multiple
/// of 64: slices and predecessor masks start at multiples of 64).
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
/// stack (up to ~83 MB seen). Virtual: only touched pages are resident.
/// Every call checks its frame fits (`asm_kernel::set_thread_stack`).
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

/// One forward frame (the wave): units in CELL order pulled by `threads()`
/// workers, each into its own `ForwardSink`; one barrier at the end, where
/// the door folds in the frame. The kept set does not depend on scheduling.
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
    // A platforms-unknown level only knows the worlds of the first
    // `PLATFORM_WORLD_FRAMES` frames.
    let level = crate::abstraction::current_level();
    // Under the split-frame prototype a game frame is two steps.
    let game_frame = if std::env::var_os("CELESTE_SPLIT_FRAME").is_some() { frame.div_ceil(2) } else { frame };
    anyhow::ensure!(
        !level.platforms || game_frame as usize <= crate::trace::kernel::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::kernel::PLATFORM_WORLD_FRAMES
    );
    let mut st = FrameStats::default();
    let workers = threads();
    st.blocks_in = frontier.len();
    st.lanes_in = frontier.iter().map(Block::lanes).sum();
    st.bytes_in = frontier.iter().map(Block::bytes).sum();
    st.rss_start = crate::metrics::current_rss_gb();

    let cells: Vec<Vec<u32>> = frontier.iter().map(Block::positions).collect::<Result<_>>()?;
    // Units in WAVE order: by first cell across the cell-sorted pieces.
    let mut units = units_of(frontier.iter().map(Block::lanes), unit_lanes());
    // A unit of only skipped lanes is not run (a won row's shape may have
    // no kernel).
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
        busy: std::time::Duration,
    }
    let done: Vec<Done> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers)
            .map(|w| {
                let (frontier, cells, units, next_unit) = (&frontier, &cells, &units, &next_unit);
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
        won |= d.won;
        if let Some(p) = pos {
            p.record_pairs(d.edges.iter().copied());
        }
        pieces.extend(d.pieces);
    }
    // The next frontier: the workers' pieces (the checkpoint sorts each by
    // (cell, key)); a cell's rows may sit in several pieces.
    let next: Vec<Block> = pieces;
    st.rss_end = crate::metrics::current_rss_gb();
    st.rss_file = crate::metrics::current_file_rss_gb();
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    Ok((next, won, st))
}


/// The share of `workers x wall` spent waiting at the barrier.
fn idle_fraction(wall: std::time::Duration, workers: usize, busy: std::time::Duration) -> f64 {
    if workers == 0 || wall.is_zero() {
        return 0.0;
    }
    (1.0 - busy.as_secs_f64() / (workers as f64 * wall.as_secs_f64())).max(0.0)
}

/// One forward frame's timings and sizes, for the `[fwd]` log line.
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    pub blocks_in: usize,
    pub lanes_in: usize,
    /// Rows emitted (after the within-call dedup), before filter and door.
    pub lanes_raw: usize,
    /// Rows that passed the filter and were new at the door.
    pub lanes_kept: usize,
    pub blocks_out: usize,
    pub lanes_out: usize,
    /// The wave's wall time and its barrier idle fraction.
    pub t_wave: std::time::Duration,
    pub wave_idle: f64,
    /// The door's end-of-frame merge, wall.
    pub t_door: std::time::Duration,
    pub flushes: u64,
    pub flushed_rows: u64,
    /// Edge records written, and the workers' summed time writing them.
    pub edge_records: u64,
    pub t_edges: std::time::Duration,
    /// The input frontier's row storage, bytes.
    pub bytes_in: usize,
    /// Bytes allocated in the workers' queue pools, and in the door.
    pub queue_bytes: usize,
    pub door_bytes: usize,
    /// Anonymous resident set at the frame's start, after the wave, at the end.
    pub rss_start: f64,
    pub rss_wave: f64,
    pub rss_end: f64,
    /// File-backed resident pages at the end of the frame.
    pub rss_file: f64,
}

/// The forward's in-memory state (frontier, door, pos-graph observer), so a
/// forward can be EXTENDED frame by frame. Every frame is checkpointed.
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
    /// Frame 0: seed the door from `initial`, checkpoint it, start recording
    /// the position graph if `record`.
    pub fn start(mut initial: Vec<Block>, dir: &std::path::Path, record: bool) -> Result<Self> {
        // A fresh tree: nothing a killed run left may survive (a stale edge
        // run for a (layer, frame) the new forward never writes would be read
        // against the new rows).
        for sub in ["frames", "edges"] {
            let p = dir.join(sub);
            if p.exists() {
                std::fs::remove_dir_all(&p).with_context(|| p.display().to_string())?;
            }
        }
        // The checkpoint assigns the ids (layer 0) the door takes.
        checkpoint_frontier(dir, 0, &mut initial)?;
        crate::search::edges::set_done_frame(&dir.join("edges"), 0)?;
        let door = crate::search::door::Door::new();
        for b in &initial {
            let cells = b.positions()?;
            let shape = b.shard_shape();
            let (mut new, mut ids) = (Vec::new(), Vec::new());
            for ((&cell, &key), &id) in cells.iter().zip(b.keys()).zip(b.ids()) {
                door.admit(shape, cell, &[key], id, &mut ids, &mut new);
            }
        }
        door.end_frame(1);
        let observer = record.then(crate::search::pos_graph::PosObserver::default);
        // An (empty) graph from the start: every tree with frames has one.
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

    /// The forward as its checkpoint tree left it (`None`: no tree), door
    /// rebuilt from every layer; extending it matches extending the original
    /// (`forward_extended_frame_by_frame_matches_fresh`).
    pub fn resume(dir: &std::path::Path, record: bool) -> Result<Option<Self>> {
        use crate::search::door::Admit;
        let mut last: Option<u32> = None;
        while dir.join("frames").join(format!("f{:03}", last.map_or(0, |f| f + 1))).is_dir() {
            last = Some(last.map_or(0, |f| f + 1));
        }
        let Some(mut last) = last else { return Ok(None) };
        let t = std::time::Instant::now();
        // Trust only frames whose edge runs are complete (`edges/done.txt`,
        // written before the next frame's checkpoint: at most one is lost).
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
        let mut files: Vec<(u32, u32, std::path::PathBuf)> = Vec::new();
        for f in 0..=last {
            files.extend(frame_paths(dir, f)?.into_iter().map(|(seq, p)| (f, seq, p)));
        }
        let next = AtomicUsize::new(0);
        let partial: Vec<(rustc_hash::FxHashMap<(u64, u32), Vec<crate::search::door::Entry>>, Option<u32>)> =
            std::thread::scope(|scope| {
                let handles: Vec<_> = (0..threads())
                    .map(|_| {
                        let (files, next) = (&files, &next);
                        scope.spawn(move || -> Result<_> {
                            let mut m: rustc_hash::FxHashMap<(u64, u32), Vec<crate::search::door::Entry>> = Default::default();
                            let mut win: Option<u32> = None;
                            loop {
                                let i = next.fetch_add(1, Ordering::Relaxed);
                                let Some((f, seq, path)) = files.get(i) else { break };
                                let file = crate::search::checkpoint::FrameFile::open(path)?;
                                let shape = file.shape_hash();
                                for (row, (cell, key)) in file.cell_keys_rows() {
                                    m.entry((shape, cell)).or_default().push((key, pack_id(*f, *seq, row)));
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
        let door = crate::search::door::Door::from_shards(shards);
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
            // The graph must cover every trusted frame (no marker: a graph
            // saved at the forward's end, covering the whole tree).
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

    /// The door's size: every distinct state reached so far.
    pub fn visited_len(&self) -> usize {
        self.door.len()
    }

    pub fn door(&self) -> &crate::search::door::Door {
        &self.door
    }

    /// Wait for the previous frame's compaction, then mark its frame done for
    /// a resume. Returns its stats and the time waited.
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

    /// Compute and checkpoint frames `frames+1 ..= to`. A win does NOT stop
    /// the run (the backward seeds from the wins at every frame); only an
    /// empty frontier does.
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
            // The previous frame's compaction is joined and marked done
            // BEFORE this frame is checkpointed, so a resume never trusts a
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
            // An exited row is checkpointed (a won one is a backward seed)
            // but never expanded: the start room's kernels do not cover the
            // next room. Nor is an orb-room row whose chest opens too late
            // (`orb_deadline_skip`).
            let orb = orb_required();
            let ceiling = level_minus_one().map(|(h, _)| h);
            self.frontier = if won || orb {
                std::thread::scope(|scope| {
                    let handles: Vec<_> = next
                        .into_iter()
                        .map(|mut b| {
                            scope.spawn(move || -> Result<Block> {
                                let mut wins = b.exits()?;
                                if let (true, Some(h)) = (orb, ceiling) {
                                    for (w, late) in wins.iter_mut().zip(orb_deadline_skip(b.rt2(), frame, h)) {
                                        *w |= late;
                                    }
                                }
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
        // The last frame's runs, before the backward reads them.
        self.join_compaction(&dir.join("edges"))?;
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

/// Persist the position graph as it stands after `frame`: `posgraph.bin`,
/// then `posgraph.frame`, each by atomic rename. Saved every frame so a
/// crash leaves a resumable tree; a graph saved at a frame the resume later
/// discards only holds edges the re-run records again.
pub fn save_pos_graph(dir: &std::path::Path, graph: &crate::search::pos_graph::PosGraph, frame: u32) -> Result<()> {
    let tmp = dir.join("posgraph.tmp");
    graph.save(&tmp)?;
    std::fs::rename(&tmp, pos_graph_path(dir))?;
    let ftmp = dir.join("posgraph.frame.tmp");
    std::fs::write(&ftmp, format!("{frame}\n"))?;
    std::fs::rename(&ftmp, dir.join("posgraph.frame"))?;
    Ok(())
}

/// The last frame the saved position graph covers; `None` if unmarked (a
/// graph saved at the forward's end).
pub fn pos_graph_frame(dir: &std::path::Path) -> Option<u32> {
    std::fs::read_to_string(dir.join("posgraph.frame")).ok().and_then(|s| s.trim().parse().ok())
}

pub fn pos_graph_path(dir: &std::path::Path) -> std::path::PathBuf {
    dir.join("posgraph.bin")
}

/// A whole forward to `max_frames`, resuming the tree in `dir` or starting
/// from `initial` (`rewrite forward`).
pub fn forward_resume_or_run(
    engine: &dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
) -> Result<ForwardResult> {
    let mut st = match ForwardState::resume(dir, true)? {
        Some(st) => st,
        None => ForwardState::start(initial, dir, true)?,
    };
    st.extend(engine, dir, max_frames, None)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// A fresh forward from `initial` to `max_frames` (the tests' driver).
#[cfg(test)]
pub fn forward_run(
    engine: &dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, record)?;
    st.extend(engine, dir, max_frames, None)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// The per-frame `[fwd]` line on stderr (times in ms), plus the phase
/// totals under `metrics` (`fwd.*`).
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
         flushes {} ({:.0} rows avg) edges {} | \
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

/// Checkpoint a frontier: one file per piece under `frames/fNNN/`, named
/// `s{shape}_{seq}.bin`, in the piece's row order (a row's position is its
/// id). An id-less block (only the initial frontier) gets its ids here.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &mut [Block]) -> Result<()> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    // A re-run frame must not leave stale files behind.
    let _ = std::fs::remove_dir_all(&fdir);
    std::fs::create_dir_all(&fdir)?;
    // Ids are (layer, seq, row): a frame's seqs must be distinct.
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

/// A frame's checkpoint files with their seq, sorted by name.
pub fn frame_paths(dir: &std::path::Path, frame: u32) -> Result<Vec<(u32, std::path::PathBuf)>> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    let mut out = Vec::new();
    for e in std::fs::read_dir(&fdir)? {
        let p = e?.path();
        let Some(n) = p.file_name().and_then(|s| s.to_str()) else { continue };
        if !(n.starts_with('s') && n.ends_with(".bin")) {
            continue;
        }
        let seq: u32 = n
            .trim_end_matches(".bin")
            .rsplit('_')
            .next()
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| anyhow::anyhow!("{}: no seq in the file name", p.display()))?;
        out.push((seq, p));
    }
    out.sort_by(|a, b| a.1.cmp(&b.1));
    Ok(out)
}

/// A frame's files (`frame_paths`), mapped, with their seq.
pub fn frame_files(dir: &std::path::Path, frame: u32) -> Result<Vec<(u32, crate::search::checkpoint::FrameFile)>> {
    frame_paths(dir, frame)?.into_iter().map(|(seq, p)| Ok((seq, crate::search::checkpoint::FrameFile::open(&p)?))).collect()
}

/// Load every block of a checkpointed frame.
pub fn load_frame(dir: &std::path::Path, frame: u32) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    for (seq, file) in frame_files(dir, frame)? {
        if let Some(rt2) = file.load_all()? {
            let ids: Vec<u64> = (0..rt2.width as u32).map(|r| pack_id(frame, seq, r)).collect();
            out.push(Block::with_ids(rt2, ids, seq));
        }
    }
    Ok(out)
}

/// One stored row by its id (`pack_id`): the row, its shape and its cell.
pub fn load_row(dir: &std::path::Path, id: u64) -> Result<(Rt2, u64, u32)> {
    let (layer, seq, row) = (id_layer(id), id_seq(id), id_row(id));
    let (_, f) = frame_files(dir, layer)?
        .into_iter()
        .find(|(s, _)| *s == seq)
        .ok_or_else(|| anyhow::anyhow!("no file l{layer} s{seq} for id {id:#x}"))?;
    anyhow::ensure!(row < f.width(), "id {id:#x}: row {row} past its file's {}", f.width());
    let rt2 = f.load_rows(&[row..row + 1])?.ok_or_else(|| anyhow::anyhow!("no row for id {id:#x}"))?;
    Ok((rt2, f.shape_hash(), f.row_cells()[row as usize]))
}

/// One row of a marks file (`Visited::save`): `(shape, cell, key.0, key.1,
/// dist)`.
type MarkRow = (u64, u32, u64, u64, u32);

/// An in-memory set of states `(shape, cell, key)`: a level's MARKED states
/// (`search --save-marks`, the UI, `MarkFilter`) or a diagnostic's reached
/// states. Saved as a marks file.
#[derive(Default)]
pub struct Visited {
    /// Sharded by (shape, cell), which the key determines, so sharding never
    /// splits a duplicate. Each key maps to its DEADLINE: the last frame from
    /// which the state still wins by the horizon; `u16::MAX`: none.
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashMap<(u64, u64), u16>>,
}

impl Visited {
    pub fn new() -> Self {
        Self::default()
    }
    /// `(entries, order-independent hash of the (key, cell) set)`.
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
    /// True if `key` (at `shape`, `cell`) was new. No deadline.
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

    /// THE MARKS FILE (read by `load` and the UI export's `MarksMap`): one
    /// bincode value `(rows, horizon)`, rows `(shape, cell, key.0, key.1,
    /// dist)` with `dist` = horizon - deadline. No deadline saves as dist 0,
    /// which filters alike (no level runs past its horizon).
    pub fn save(&self, path: &std::path::Path, horizon: u32) -> Result<()> {
        let mut rows: Vec<MarkRow> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            rows.extend(keys.iter().map(|(&(k0, k1), &d)| (*shape, *cell, k0, k1, horizon - (d as u32).min(horizon))));
        }
        rows.sort_unstable();
        crate::search::checkpoint::save_value_to(path, &(rows, horizon))
    }

    /// A marks file (`save`).
    pub fn load(path: &std::path::Path) -> Result<Self> {
        let bytes = std::fs::read(path)?;
        let payload = crate::search::checkpoint::value_payload(&bytes, path)?;
        let (rows, horizon): (Vec<MarkRow>, u32) = bincode::deserialize(payload).with_context(|| format!("deserializing {}", path.display()))?;
        let mut out = Self::new();
        for (shape, cell, k0, k1, dist) in rows {
            anyhow::ensure!(dist <= horizon, "{}: a mark {dist} frames before h{horizon}", path.display());
            out.insert_until(shape, (k0, k1), cell, u16::try_from(horizon - dist).unwrap_or(u16::MAX));
        }
        Ok(out)
    }
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
}

/// The first win of a level tree complete (frames and edge runs) through
/// `horizon`; `None` when the tree does not reach it.
pub fn tree_first_win_through(dir: &std::path::Path, horizon: u32) -> Result<Option<Option<u32>>> {
    let done = crate::search::edges::done_frame(&dir.join("edges"));
    if done.is_none_or(|d| d < horizon) || !dir.join("frames").join(format!("f{horizon:03}")).is_dir() {
        return Ok(None);
    }
    for f in 0..=horizon {
        for e in std::fs::read_dir(dir.join("frames").join(format!("f{f:03}")))? {
            let p = e?.path();
            let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            if name.starts_with('s') && name.ends_with(".bin") && !crate::search::checkpoint::FrameFile::open(&p)?.wins().is_empty() {
                return Ok(Some(Some(f)));
            }
        }
    }
    Ok(Some(None))
}

#[cfg(test)]
mod tests {
    /// The objects ladder's filter admits a row onto a marked coarse node
    /// up to the node's deadline, and nothing else.
    #[test]
    fn the_mark_filter_admits_up_to_the_deadline() {
        use crate::abstraction::Level;
        let engine = RefEngine::new().expect("ref engine");
        let block = Block::keyed(engine.initial().expect("initial state")).expect("block");
        let coarse = Level::parse("r0sxh").expect("level");
        let (shape, keys, cells) = widened_keys(&block, coarse).expect("keys");
        let mut marked = Visited::new();
        marked.insert_until(shape, keys[0], cells[0], 5);
        let f = MarkFilter::new(&marked, coarse);
        assert_eq!(f.allowed(block.rt2(), 5).expect("filter"), vec![true], "at the deadline");
        assert_eq!(f.allowed(block.rt2(), 6).expect("filter"), vec![false], "past it");
        let empty = Visited::new();
        assert_eq!(MarkFilter::new(&empty, coarse).allowed(block.rt2(), 0).expect("filter"), vec![false], "not marked");
    }


    /// A row ref names its queue in full: the slot vector grows past
    /// `POOL_QUEUES`, and an index above 255 must not alias another queue.
    #[test]
    fn row_refs_survive_more_than_256_queue_slots() {
        let mut sink = ForwardSink::empty(false);
        let engine = RefEngine::new().expect("ref engine");
        let skeleton = Block::keyed(engine.initial().expect("initial state")).expect("block").into_rt2();
        for q in 0..300 {
            sink.slots.push(Slot::new(skeleton.clone_block()));
            let s = &mut sink.slots[q];
            s.live = true;
            s.gen = (q * 7) as u16;
            s.keys.push((q as u64, 1));
            s.cells.push(0);
            s.pred_base.push(1000 + q as u64 * 16);
            s.pred_mask.push(1);
            s.pred_xfer.push(7);
            s.last_extra.push(u32::MAX);
        }
        let r = sink.row_ref(299);
        assert!(r & celeste_engine::kernel::RowCache::ID_FLAG == 0);
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 7, 3));
        assert_eq!(sink.slots[299].pred_mask[0], 0b1001);
        assert_eq!(sink.slots[43].pred_mask[0], 1, "no other queue's row was touched");
        // Another slice of the same call: an extra entry on the same row.
        assert!(sink.mark_pred(r, 5000, 7, 2));
        assert_eq!(sink.slots[299].extra, vec![(0, 5000, 7, 0b100u64)]);
        // Lanes up to 63 (a 64-lane group).
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 7, 63));
        assert_eq!(sink.slots[299].pred_mask[0], 0b1001 | (1 << 63));
        // The same slice with another transfer: its own entry, not the mask.
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 8, 5));
        assert!(sink.mark_pred(r, 1000 + 299 * 16, 8, 6));
        assert_eq!(sink.slots[299].pred_mask[0], 0b1001 | (1 << 63));
        assert_eq!(sink.slots[299].extra[1], (0, 1000 + 299 * 16, 8, 0b110_0000u64));
        // A flushed queue's ref is stale.
        sink.slots[299].clear();
        assert!(!sink.mark_pred(r, 1000 + 299 * 16, 7, 0));
    }
    use super::*;
    use crate::trace::refengine::RefEngine;
    use std::sync::Mutex;

    /// A fresh forward in a directory a killed run left files in keeps none
    /// of them (a stale edge run would be read against the new rows).
    #[test]
    fn a_fresh_forward_clears_what_a_killed_run_left() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::keyed(engine.initial().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-fresh-start-test");
        let _ = std::fs::remove_dir_all(dir);
        let stale = [dir.join("edges/l9/f009.bin"), dir.join("edges/raw/f009/w0.bin"), dir.join("frames/f009/b0_s0.bin")];
        for f in &stale {
            std::fs::create_dir_all(f.parent().unwrap()).expect("mkdir");
            std::fs::write(f, b"stale").expect("write");
        }
        ForwardState::start(init, dir, true).expect("start");
        for f in &stale {
            assert!(!f.exists(), "{} survived a fresh start", f.display());
        }
        assert!(dir.join("frames/f000").is_dir(), "frame 0 checkpointed");
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A marks file round-trips every state's deadline; one without a deadline
    /// comes back at the horizon, which filters alike (no level runs past it).
    #[test]
    fn marks_files_keep_their_deadlines() {
        let path = std::env::temp_dir().join(format!("celeste-marks-roundtrip-{}.bin", std::process::id()));
        let mut v = Visited::new();
        v.insert_until(1, (2, 3), 4, 7);
        v.insert_until(1, (5, 6), 4, 12);
        v.insert(9, (2, 3), 8);
        v.save(&path, 12).expect("save");
        let w = Visited::load(&path).expect("load");
        std::fs::remove_file(&path).expect("rm");
        assert_eq!(w.fingerprint(), v.fingerprint());
        assert_eq!(w.deadline(1, (2, 3), 4), Some(7));
        assert_eq!(w.deadline(1, (5, 6), 4), Some(12));
        assert_eq!(w.deadline(9, (2, 3), 8), Some(12), "no deadline: the horizon");
    }

    /// Extending a forward frame by frame, and resuming one from its
    /// checkpoints, reproduce a fresh run's key sets.
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
            let init = vec![Block::keyed(e.initial().expect("init")).expect("block")];
            forward_run(&Mutex::new(e), init, dir, 4, true).expect("fresh");
        }
        let fresh4 = keyset(4);
        assert!(!fresh4.is_empty(), "fresh frame 4 empty");

        // The same run extended one frame at a time.
        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::keyed(e.initial().expect("init")).expect("block")];
            let e = Mutex::new(e);
            let mut st = ForwardState::start(init, dir, true).expect("start");
            assert_eq!(pos_graph_frame(dir), Some(0), "start must leave a graph beside f0");
            for to in 1..=4 {
                st.extend(&e, dir, to, None).expect("extend");
                assert_eq!(st.frames, to);
                // Every frame leaves a graph covering it, so a crash resumes.
                assert_eq!(pos_graph_frame(dir), Some(to), "the pos graph must be saved with frame {to}");
            }
            assert!(st.pos_graph().is_some());
        }
        let extended4 = keyset(4);
        assert_eq!(fresh4, extended4, "extension diverged from fresh at frame 4");
        eprintln!("[extend] frame 4 key set identical: {} keys", fresh4.len());

        // RESUME: a fresh run to 6 against the tree above resumed at 4.
        let fresh_dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-resume-test-fresh6");
        let _ = std::fs::remove_dir_all(fresh_dir);
        std::fs::create_dir_all(fresh_dir).expect("mkdir");
        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::keyed(e.initial().expect("init")).expect("block")];
            forward_run(&Mutex::new(e), init, fresh_dir, 6, true).expect("fresh 6");
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

    /// THE SPAWN FRAME of room (0,2), where `player_spawn` becomes the player
    /// standing at (16, 104): kernels and reference make the same successors.
    /// Guards `Graph::tile_flag_over` reading every row a span covers (once
    /// only row 0, so the kernels' player spawned in the air).
    #[test]
    fn room_02_spawn_frame_kernels_make_the_reference_successors() {
        use crate::abstraction::{set_level, Level};
        std::env::set_var("CELESTE_START_ROOM", "0,2");
        set_level(Level::parse("r0sx").expect("level"));
        let kernels = crate::compiled::FrameEngine::new_for_start_room().expect("kernels");
        let reference = Mutex::new(RefEngine::new().expect("ref engine"));
        // 25 frames of no input: the spawn's last frame is the 26th.
        let mut concrete = RefEngine::new().expect("ref engine");
        let mut row = concrete.initial().expect("initial state");
        for _ in 0..25 {
            row = concrete.step_one(&row, 0).expect("frame").into_rt2();
        }
        // The state as a row of the level: its boundary widenings applied.
        widen_rt2_to(&mut row, Level::EXACT);
        let successors = |engine: &dyn FrameStep| -> std::collections::BTreeSet<((u64, u64), u32)> {
            let door = crate::search::door::Door::new();
            let (next, _, _) = forward_frame(engine, vec![Block::from_rt2(row.clone_block())], &door, None, None, 26, None).expect("frame");
            next.iter().flat_map(|b| b.keys().iter().copied().zip(b.positions().expect("cells"))).collect()
        };
        let (k, r) = (successors(&kernels), successors(&reference));
        let at: std::collections::BTreeSet<_> = r.iter().map(|(_, c)| crate::search::pos_graph::cell_xy(*c)).collect();
        assert_eq!(at, [Some((16, 104))].into_iter().collect(), "the player stands at its spawn");
        assert!(r.len() > 1, "the buttons make more than one successor");
        assert_eq!(k.difference(&r).count() + r.difference(&k).count(), 0, "kernels {} successors, reference {}: {} only in the reference", k.len(), r.len(), r.difference(&k).count());
    }

    /// THE EXIT FRAME of room (1,3)'s known 127-frame solution: from the
    /// state before it at `r0sxh`, the kernels make the concrete exit's
    /// state, KEY included, and every emitted row carries its columns' key.
    /// Guards a number and its point interval keying alike
    /// (`runtime2::av_code`), or the mark filter misses the win rows.
    #[test]
    fn room_13_exit_frame_keys_the_concrete_exit() {
        use crate::abstraction::{set_level, Level};
        std::env::set_var("CELESTE_START_ROOM", "1,3");
        let level = Level::parse("r0sxh").expect("level");
        set_level(level);
        let kernels = crate::compiled::FrameEngine::new_for_start_room().expect("kernels");
        let text = std::fs::read_to_string("tas/room_1_3_reference_frame_127.txt").expect("the reference solution");
        let inputs: Vec<u8> = text
            .lines()
            .filter(|l| !l.trim_start().starts_with('#'))
            .flat_map(|l| l.split(',').filter(|t| !t.trim().is_empty()).map(|t| t.trim().parse::<u8>().expect("an input byte")).collect::<Vec<_>>())
            .collect();
        assert_eq!(inputs.len(), 127);
        let mut reference = RefEngine::new().expect("ref engine");
        let mut st = reference.initial().expect("initial state");
        for &b in &inputs[..126] {
            st = reference.step_one(&st, b).expect("one concrete successor").into_rt2();
        }
        let exit = reference.step_one(&st, inputs[126]).expect("one concrete successor");
        assert!(wins_of(exit.rt2()).expect("wins")[0], "the reference solution exits at frame 127");
        let mut parent = st;
        widen_rt2_to(&mut parent, level);
        let door = crate::search::door::Door::new();
        let (next, _, _) = forward_frame(&kernels, vec![Block::from_rt2(parent)], &door, None, None, 127, None).expect("frame");
        let mut made: std::collections::BTreeSet<(u64, (u64, u64), u32)> = Default::default();
        for b in &next {
            let stored = b.keys().to_vec();
            let canonical = b.rt2().clone_block().row_keys_canonical();
            assert_eq!(stored, canonical, "an emitted row's key is not its columns' key (shape {:#x})", b.rt2().shape_hash);
            let (shape, keys, cells) = widened_keys(b, level).expect("keys");
            made.extend(keys.into_iter().zip(cells).map(|(k, c)| (shape, k, c)));
        }
        let (shape, keys, cells) = widened_keys(&exit, level).expect("keys");
        assert!(made.contains(&(shape, keys[0], cells[0])), "the kernels do not make the concrete exit's state ({} successors)", made.len());
    }

    /// The summit's win is touching the flag (`win_rect`), not a room exit:
    /// the TAS31 reference (fitted to the original cart's flag touch at frame
    /// 55) wins at its last frame and at no frame before it.
    #[test]
    fn the_summit_reference_wins_at_the_flag() {
        std::env::set_var("CELESTE_START_ROOM", "6,3");
        assert_eq!(win_rect(), Some((55, 67, 41, 52)), "the flag at (61, 48)");
        let text = std::fs::read_to_string("tas/room_6_3_reference_frame_55.txt").expect("the reference");
        let inputs: Vec<u8> = text
            .lines()
            .filter(|l| !l.trim_start().starts_with('#'))
            .flat_map(|l| l.split(',').filter(|t| !t.trim().is_empty()).map(|t| t.trim().parse::<u8>().expect("an input byte")).collect::<Vec<_>>())
            .collect();
        assert_eq!(inputs.len(), 55);
        let mut reference = RefEngine::new().expect("ref engine");
        let mut st = reference.initial().expect("initial state");
        for (i, &b) in inputs.iter().enumerate() {
            let next = reference.step_one(&st, b).expect("one concrete successor");
            let won = wins_of(next.rt2()).expect("wins")[0];
            assert_eq!(won, i + 1 == 55, "frame {}: won {won}", i + 1);
            st = next.into_rt2();
        }
    }

    /// THE SPLIT FRAME (`CELESTE_SPLIT_FRAME`) at a near level reaches the
    /// unsplit frame's state counts at every frame boundary (room (2,1)
    /// `r0sxhn`: two fall floors under the spawn, dashes from frame 24). The
    /// unsplit counts are pinned (`rewrite forward --level r0sxhn --room
    /// 2,1`): the kernels are built per process for one level. Guards a
    /// global set to nil being removed (`heap::Table::set_global`) and the
    /// mid-frame cut not widening floors the player then reads.
    #[test]
    fn split_frame_reaches_the_unsplit_frontier_at_a_near_level() {
        use crate::abstraction::{set_level, Level};
        std::env::set_var("CELESTE_START_ROOM", "2,1");
        std::env::set_var("CELESTE_SPLIT_FRAME", "1");
        set_level(Level::parse("r0sxhn").expect("level"));
        let dir = std::path::Path::new("/var/tmp/celeste-frame-split-near-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");
        let engine = crate::compiled::FrameEngine::new_for_start_room().expect("engine");
        let init = vec![Block::keyed(RefEngine::new().expect("ref").initial().expect("init")).expect("block")];
        // Unsplit, frame by frame: 1 state a frame through the spawn, then these.
        let unsplit: Vec<(u32, usize)> = (0..24).map(|f| (f, 1)).chain([(24, 27), (25, 308), (26, 1576), (27, 5559), (28, 15348)]).collect();
        let last = unsplit.last().unwrap().0;
        forward_run(&engine, init, dir, 2 * last, false).expect("split forward");
        for (f, want) in unsplit {
            let got: usize = load_frame(dir, 2 * f).expect("load").iter().map(Block::lanes).sum();
            assert_eq!(got, want, "frame {f} (step {}): the split frame reached {got} states, the unsplit {want}", 2 * f);
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
