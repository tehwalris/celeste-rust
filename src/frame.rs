//! The block, the frame-step interface (`FrameStep`, `ForwardSink`: queues,
//! door, edge records with transfers) and the forward (`forward_frame`, one
//! wave; `ForwardState`, extended frame by frame, checkpointed, resumable).
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
    /// Lanes the frame step must not expand (a mask, so ids stay consecutive).
    skip: Vec<bool>,
}

/// A state's stable id (layer = first frame reached, piece seq, row),
/// assigned at admission and never renumbered.
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
    /// Wrap an engine block; its key column must be present (nothing re-hashes).
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

    /// The 128-bit canonical row key per lane: the identity everywhere.
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

/// The summit (6,3) has no exit: it ends when the player touches the
/// original cart's FLAG (tile 118 at (fx, fy)): fx-6 <= x <= fx+6, fy-7 <= y <= fy+4.
pub const SUMMIT_LEVEL: i16 = 30;

/// The winning positions (x_lo, x_hi, y_lo, y_hi, inclusive): `--win-at` or
/// the summit's flag; a lane wins where it MEETS it. None: the room exit.
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

/// Per lane: has it left the start room, or met the `win_rect`?
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

/// Per lane: on the win target? The exit, plus the orb in the orb room.
pub fn wins_of(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::AV;
    let mut exits = exits_of(rt2)?;
    if crate::game_runner::hundred() {
        // 100%: the exit counts only with this room's berry taken.
        for (e, b) in exits.iter_mut().zip(got_fruit(rt2)?) {
            *e &= b;
        }
    }
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

/// Per lane: has the start room's berry been taken (`got_fruit[1 +
/// level_index()]`, an array the cart pads with nils up to that index)? An
/// unknown value (a coarse level) counts as taken: a win there is only an
/// over-approximation, and the exact level decides.
pub fn got_fruit(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let lanes = rt2.width;
    let g = celeste_names::gen::global_id("got_fruit").expect("`got_fruit` is a global name");
    let (x, y) = crate::game_runner::start_room();
    let i = crate::game_runner::level_index(x, y) as usize;
    let Some(arr) = rt2.global_target(g) else { return Ok(vec![false; lanes]) };
    let Cell2::Arr(items) = &rt2.structure[arr as usize] else { return Ok(vec![false; lanes]) };
    let Some(&c) = items.get(i) else { return Ok(vec![false; lanes]) };
    Ok((0..lanes)
        .map(|l| match &rt2.cols[c as usize] {
            Col::U(v) => matches!(v, AV::Bool(true) | AV::UBool),
            col => matches!(col.at(l), AV::Bool(true) | AV::UBool),
        })
        .collect())
}

/// A SOUND LOWER BOUND on frames from the big chest opening to a win: 61
/// paused + 8 until the orb is collectable + 10 frozen = 79, taken as 70.
pub const ORB_MIN_FRAMES_AFTER_CHEST: u32 = 70;

/// Per lane in the orb room: is the chest still CLOSED too late to win by
/// `horizon` (the ceiling)? Such rows are not expanded.
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

/// Per lane of a frame's rows: NOT expanded further. An exited row (no kernels
/// for the next room; a win or a `--win-at` exit), and in the orb room a row
/// whose chest is still closed too late for the ceiling (`orb_deadline_skip`).
/// The forward and a resume both take the frontier through this, so a resumed
/// run expands exactly the rows an uninterrupted one does.
pub fn not_expanded(b: &Block, frame: u32) -> Result<Vec<bool>> {
    let mut skip = b.exits()?;
    if let (true, Some((h, _))) = (orb_required(), level_minus_one()) {
        for (w, late) in skip.iter_mut().zip(orb_deadline_skip(b.rt2(), frame, h)) {
            *w |= late;
        }
    }
    Ok(skip)
}

/// Is the start room the orb room (5,2)? A win there also needs the orb
/// (`max_djump == 2`): later levels are played with the second dash.
pub fn orb_required() -> bool {
    let (x, y) = crate::game_runner::start_room();
    crate::game_runner::level_index(x, y) == crate::game_runner::ORB_LEVEL && !crate::game_runner::gemskip()
}

/// `CELESTE_TRIM_ROWS=1`: a frame's checkpoint files keep only keys, cells
/// and wins once a later frame's runs are complete (`checkpoint::trim`) -
/// what the search reads of them, and what a resume and `export-ui` read.
/// The rows' values are most of a frame's files: room (2,3) gemskip h137,
/// level 0's frames 3.03 -> 0.78 GB (room (5,1) nodiag's format-9 rows: 148
/// B, of which the key is 16). The diagnostics that load old rows
/// (`rerun-row`, `follow`, `arc-check`, ...) refuse a trimmed tree.
pub fn trim_rows() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_TRIM_ROWS").is_ok_and(|v| v == "1"))
}

/// Search steps per game frame: 2 under the split frame
/// (`CELESTE_SPLIT_FRAME`: part a up to and including the player's move,
/// part b the rest; step `2k` is the end of frame `k`), else 1.
pub fn steps_per_frame() -> u32 {
    if std::env::var_os("CELESTE_SPLIT_FRAME").is_some() {
        2
    } else {
        1
    }
}

/// Worker count: `CELESTE_THREADS`, else one per physical core (AVX-512 units
/// are shared by SMT siblings).
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

/// A queue of emitted rows of one (shape, cell): skeleton, a `TCol` per
/// varying cell, key and cell per row, and predecessors for the edges.
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
    /// Predecessors in the emitting slice: its first input id, a bit per lane.
    pub pred_base: Vec<u64>,
    pub pred_mask: Vec<u64>,
    /// The transfer of `pred_mask`'s lanes (others go to `extra`).
    pub pred_xfer: Vec<u32>,
    /// Later producers (other slices, other transfers) of a row pushed once
    /// per call: `(row, slice base, transfer, lane mask)`.
    pub extra: Vec<(u32, u64, u32, u64)>,
    /// Per row, its latest `extra` entry (`u32::MAX`: none), merged into.
    pub last_extra: Vec<u32>,
    /// Bumped at every flush, so a stale row ref is recognised.
    pub gen: u16,
}

impl Slot {
    /// An empty slot over `skeleton`.
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

    /// Append `rows` to `piece` (same shape); cells it holds differently go
    /// through `col_push`.
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

    /// Does any of `rows` sit on the win target (`wins_of` on the columns)?
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
            // Meeting the target wins (over-approximate; the concrete search refutes).
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

    /// Push one materialized row (the reference engine's path).
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

/// Append one record to the target layer's buffer, written out when full.
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

/// Append `buf` to the worker's raw file for `layer` at `frame`, and clear it.
/// Its equal records go once: an edge re-recorded by another kernel call of
/// the same unit lands in the same buffer (the compaction would merge it).
fn write_edges(dir: &std::path::Path, frame: u32, layer: usize, worker: u32, buf: &mut Vec<u8>) -> Result<()> {
    use crate::search::edges::RECORD_BYTES;
    use std::io::Write;
    let mut recs: Vec<[u8; RECORD_BYTES]> = buf.chunks_exact(RECORD_BYTES).map(|c| c.try_into().expect("a record")).collect();
    recs.sort_unstable();
    recs.dedup();
    buf.clear();
    buf.extend(recs.iter().flatten());
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

/// Rows per queue; small so a worker's pool stays cache-resident.
pub const QUEUE_ROWS: usize = 256;
/// Live queues per worker; beyond it one is evicted (only flush count changes).
pub const POOL_QUEUES: usize = 256;

/// One worker's sink for a frame: keyed rows into a QUEUE per (outcome,
/// cell); a full or evicted queue is FLUSHED (filter, door, piece, edges).
pub struct ForwardSink<'a> {
    /// The pool: every queue ever created by this sink, live or spare.
    pub slots: Vec<Slot>,
    /// (outcome id, cell) -> live queue; the id is the emitter's own.
    index: rustc_hash::FxHashMap<(u64, u32), u32>,
    /// Flushed, empty queues per outcome id, keeping their columns.
    spare: rustc_hash::FxHashMap<u64, Vec<u32>>,
    /// The second-chance hand over `slots`.
    clock: usize,
    /// The run cache: rows arrive in runs of one (outcome, cell).
    last: ((u64, u32), u32),
    door: Option<&'a crate::search::door::Door>,
    filter: Option<&'a MarkFilter<'a>>,
    /// The ids of the block being run (slice bases for predecessor masks).
    pub ids_in: Option<&'a [u64]>,
    /// Lanes of the block being run that must not be expanded.
    pub skip_in: Option<&'a [bool]>,
    /// Where the edge records go (`edges::raw_path`); `None`: no recording.
    edges_dir: Option<std::path::PathBuf>,
    edge_bufs: Vec<Vec<u8>>,
    pub edge_records: u64,
    /// The within-call dedup cache (`RowCache`), written back by the flush.
    pub seen: celeste_engine::kernel::RowCache,
    /// Direct-mapped merge of edges to flushed states; evicted -> a record.
    direct: Vec<(u64, u64, u32, u64)>,
    /// This worker's interned transfers (records carry the index).
    xfer_ids: rustc_hash::FxHashMap<crate::search::arc_edges::Pair, u32>,
    xfer_tab: Vec<crate::search::arc_edges::Pair>,
    /// Time spent encoding and writing edge records (this worker).
    pub t_edges: std::time::Duration,

    /// Next-frame pieces per shape: block, seq (`worker * 256 + k`), ids.
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

    /// Worker `worker`'s sink for `frame` (new ids in layer `frame`).
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

    /// One edge record: `mask`'s lanes at `base` -> state `target`, via `xfer`.
    #[inline]
    fn record(&mut self, target: u64, base: u64, xfer: u32, mask: u64) -> Result<()> {
        let dir = self.edges_dir.as_deref().expect("recording without an edges dir");
        append_record(&mut self.edge_bufs, &mut self.edge_records, dir, self.frame, self.worker, target, base, xfer, mask)
    }

    /// An edge to the flushed state `target`, merged in the direct-mapped cache.
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

    /// A handle on the row just pushed into `q`: row (8 bits), queue (24: the
    /// pool grows past `POOL_QUEUES`), generation (16), below the flag bits.
    #[inline]
    pub fn row_ref(&self, q: usize) -> u64 {
        const _: () = assert!(QUEUE_ROWS <= 256);
        assert!(q < 1 << 24, "queue pool past 2^24 slots");
        let s = &self.slots[q];
        ((s.gen as u64) << 32) | ((q as u64) << 8) | (s.rows() as u64 - 1)
    }

    /// Add a predecessor to the row at `row_ref`; false if its queue was
    /// flushed since (the caller pushes the row again).
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

    /// The live queue for `(outcome, cell)`, created on first use.
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

    /// Second chance: flush the first untouched live queue.
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

    /// Flush queue `q`: filter, sort and collapse duplicates, admit at the
    /// door, gather into this worker's piece; the queue becomes a spare.
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
            // Rows sharing a key are one state with several predecessor masks.
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
            // The k-th new key gets `first_new + k`, appended in that order.
            let n_pieces = self.pieces.len() as u32;
            let (frame, worker) = (self.frame, self.worker);
            let (piece, seq, ids) = self
                .pieces
                .entry(slot.shape)
                .or_insert_with(|| (slot.empty_piece(), worker * 256 + n_pieces, Vec::new()));
            let first_new = pack_id(frame, *seq, piece.width as u32);
            door.admit(slot.shape, slot.cell, &self.keys_buf, first_new, &mut self.ids_buf, &mut self.new_buf);
            // Each row's predecessors and extras (none from id-less blocks).
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
                // Each row's fate into the dedup cache (later re-emissions).
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

    /// End of the worker's frame: flush everything, hand back the pieces.
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
            // Drop empty pieces (renumbered, they would collide with a seq).
            if p.width == 0 {
                continue;
            }
            debug_assert_eq!(ids.len(), p.width);
            // Agreeing columns become uniform (the gates are pinned with it).
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

    /// Emit one single-row block at `cell` (the reference engine's path).
    pub fn emit_row(&mut self, row: &Rt2, cell: u32) -> Result<()> {
        debug_assert_eq!(row.width, 1);
        debug_assert_eq!(row.row_keys.len(), 1, "an emitted row carries its key");
        let q = self.queue(row.shape_hash, cell, || {
            // Numbers and bools vary; the rest is fixed by the shape.
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


/// THE LEVEL -1 FILTER (`CELESTE_LEVEL_MINUS_ONE="H,S"`): drop a queue when
/// the table proves its cell cannot exit by H (sound: inductive ranges,
/// clipped successors and deaths accounted). H must be the ceiling. Built
/// once, at the first flush, on a thread with the tracer's stack.
fn level_minus_one() -> Option<(u32, &'static crate::trace::level_minus_one::CostToGo)> {
    static TABLE: std::sync::OnceLock<Option<(u32, crate::trace::level_minus_one::CostToGo)>> = std::sync::OnceLock::new();
    TABLE
        .get_or_init(|| {
            let (h, sp) = level_minus_one_env()?;
            // The table measures the EXIT: unsound for a position win.
            assert!(win_rect().is_none(), "CELESTE_LEVEL_MINUS_ONE: the table measures the room exit; this room's win is a position");
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

/// `CELESTE_LEVEL_MINUS_ONE="H,S"`: the horizon (in search steps) and the speed bound.
fn level_minus_one_env() -> Option<(u32, i32)> {
    let s = std::env::var("CELESTE_LEVEL_MINUS_ONE").ok()?;
    let (h, sp) = s.split_once(',').expect("CELESTE_LEVEL_MINUS_ONE=\"H,S\"");
    let h: u32 = h.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE horizon");
    let sp: i32 = sp.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE speed bound (px per frame)");
    Some((h, sp))
}

/// A tree whose forward ran under level -1 at H holds no row that cannot
/// exit by H, so it serves horizons up to H only: reused (or resumed) for a
/// larger one it would miss every win the filter dropped. `<dir>/
/// level_minus_one.txt` records the smallest H any forward that ADDED rows to
/// the tree filtered at (`record_level_minus_one`); this refuses a horizon
/// past it. Called before a tree is read or extended.
pub fn check_level_minus_one(dir: &std::path::Path, horizon: u32) -> Result<()> {
    if let Some(h) = recorded_level_minus_one(dir)? {
        anyhow::ensure!(horizon <= h, "{}: the tree was filtered by level -1 at step {h}; it cannot serve step {horizon} (delete it)", dir.display());
    }
    Ok(())
}

fn recorded_level_minus_one(dir: &std::path::Path) -> Result<Option<u32>> {
    let path = dir.join("level_minus_one.txt");
    match std::fs::read_to_string(&path) {
        Ok(s) => Ok(Some(s.trim().parse().with_context(|| path.display().to_string())?)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e).with_context(|| path.display().to_string()),
    }
}

/// Before a forward adds rows to the tree for `horizon`: this run's level -1
/// filter (if any) must not be below the horizon, and it is recorded (the
/// smaller of it and the tree's record).
pub fn record_level_minus_one(dir: &std::path::Path, horizon: u32) -> Result<()> {
    if let Some((h, _)) = level_minus_one_env() {
        anyhow::ensure!(horizon <= h, "CELESTE_LEVEL_MINUS_ONE at step {h} is below the horizon, step {horizon}");
        let recorded = recorded_level_minus_one(dir)?;
        std::fs::create_dir_all(dir)?;
        std::fs::write(dir.join("level_minus_one.txt"), format!("{}\n", recorded.map_or(h, |r| r.min(h))))?;
    }
    Ok(())
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

/// THE OBJECTS LADDER's link: a finer forward keeps a row at frame t only if
/// its projection (`Rt2::widen_to`) is a coarse node ARC-MARKED with deadline
/// >= t. Sound: a fine state winning from t with remainder r projects to a
/// coarse node winning from t with r (coarse over-approximates, r exact).
pub struct MarkFilter<'a> {
    marked: &'a Visited,
    coarser: crate::abstraction::Level,
}

impl<'a> MarkFilter<'a> {
    pub fn new(marked: &'a Visited, coarser: crate::abstraction::Level) -> Self {
        MarkFilter { marked, coarser }
    }

    /// Per lane of `rt2` (rows of frame `frame`): admitted? Under the split
    /// frame only at frame boundaries (even steps): a mid-frame row's
    /// projection does not key as the coarser tree's mid-frame rows
    /// (`widen::widen_near_floors` keeps a near floor's computed
    /// `collideable` in the player's probe window there, `Rt2::widen_to`
    /// does not), so filtering it would drop marked paths - room (6,1)
    /// nodiag `r0sxhn,r0sxh` REFUTED 93, which the community TAS reaches.
    /// The next boundary filters its successors.
    pub fn allowed(&self, rt2: &Rt2, frame: u32) -> Result<Vec<bool>> {
        if frame % steps_per_frame() != 0 {
            return Ok(vec![true; rt2.width]);
        }
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser)?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| self.marked.deadline(shape, *k, c).is_some_and(|d| u32::from(d) >= frame)).collect())
    }
}

/// Each lane's `(shape, key, cell)` at the `coarser` level (its node there).
pub fn widened_keys(
    block: &Block,
    coarser: crate::abstraction::Level,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    widened_keys_rt2(&block.rt2, coarser)
}

/// `rt2` projected IN PLACE onto `level`, as its kernels would store it.
pub fn widen_rt2_to(rt2: &mut Rt2, level: crate::abstraction::Level) {
    rt2.widen_to(crate::compiled::ids(), level.held, level.fruit, level.floors_near, level.platforms);
}

/// `(shape, keys, cells)` of the widened rows (the widened block's shape).
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

/// The interface between the loop and the engines (kernels or reference):
/// lanes in, every successor row out, widened for the level and keyed.
pub trait FrameStep: Sync {
    /// One frame of `lanes` of `block` into `sink`; called concurrently on
    /// disjoint ranges (scratch in the sink or behind a lock).
    fn run(&self, block: &Block, cell_in: &[u32], lanes: Range<usize>, sink: &mut ForwardSink) -> Result<()>;
}

/// Lanes per unit (one kernel call): load balance against the dedup window.
/// `CELESTE_UNIT_LANES` overrides it (a multiple of 64, the id groups).
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

/// Stack per frame worker: kernel spill frames reach ~83 MB (virtual; each
/// call checks it fits, `asm_kernel::set_thread_stack`).
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

/// One forward frame (the wave): units in CELL order pulled by workers into
/// their own sinks; one barrier. The kept set does not depend on scheduling.
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
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let level = crate::abstraction::current_level();
    // Under the split frame a game frame is two steps.
    let game_frame = frame.div_ceil(steps_per_frame());
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
    // A unit of only skipped lanes is not run (it may have no kernel).
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
    // The next frontier: the workers' pieces (a cell may span several).
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

/// The forward's in-memory state, EXTENDED frame by frame (each checkpointed).
pub struct ForwardState {
    frontier: Vec<Block>,
    /// The door: every (shape, cell, key) reached so far (`search::door`).
    door: crate::search::door::Door,
    observer: Option<crate::search::pos_graph::PosObserver>,
    /// The last frame computed (and checkpointed).
    pub frames: u32,
    /// The first frame a lane won, if any so far.
    pub win_frame: Option<u32>,
    /// The last frame's edge compaction, running behind the next wave.
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
    /// Frame 0: seed the door from `initial` and checkpoint it.
    pub fn start(mut initial: Vec<Block>, dir: &std::path::Path, record: bool) -> Result<Self> {
        // A fresh tree keeps nothing a killed run left (stale edge runs).
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

    /// The forward as its checkpoint tree left it (`None`: no tree); extending
    /// it matches extending the original.
    pub fn resume(dir: &std::path::Path, record: bool) -> Result<Option<Self>> {
        use crate::search::door::Admit;
        let mut last: Option<u32> = None;
        while dir.join("frames").join(format!("f{:03}", last.map_or(0, |f| f + 1))).is_dir() {
            last = Some(last.map_or(0, |f| f + 1));
        }
        let Some(mut last) = last else { return Ok(None) };
        let t = std::time::Instant::now();
        // Trust only frames whose edge runs are complete (at most one lost).
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
        // The frontier: the last layer minus the rows the forward does not
        // expand (`not_expanded`, as `extend` decides it).
        let mut frontier: Vec<Block> = load_frame(dir, last)?;
        for b in &mut frontier {
            let skip = not_expanded(b, last)?;
            b.set_skip(skip);
        }
        let observer = if record {
            let path = pos_graph_path(dir);
            let graph = crate::search::pos_graph::PosGraph::load(&path)
                .with_context(|| format!("resuming {}: no pos graph at {}", dir.display(), path.display()))?;
            // It must cover every trusted frame (no marker: the whole tree).
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

    /// Join the previous compaction and mark its frame done; stats and wait.
    fn join_compaction(
        &mut self,
        edges_dir: &std::path::Path,
    ) -> Result<Option<(crate::search::edges::CompactStats, std::time::Duration)>> {
        let Some((frame, handle)) = self.compaction.take() else { return Ok(None) };
        let t = std::time::Instant::now();
        let st = handle.join().expect("compaction thread panicked")?;
        crate::search::edges::set_done_frame(edges_dir, frame)?;
        // A resume loads frame `frame` as its frontier and never an earlier
        // one's rows: those may go.
        if trim_rows() && frame >= 2 {
            let dir = edges_dir.parent().expect("a level dir");
            for (_, p) in frame_paths(dir, frame - 1)? {
                crate::search::checkpoint::trim(&p)?;
            }
        }
        Ok(Some((st, t.elapsed())))
    }

    /// Compute and checkpoint frames `frames+1 ..= to`; only an empty
    /// frontier stops it (the backward seeds from wins at every frame).
    pub fn extend(
        &mut self,
        engine: &dyn FrameStep,
        dir: &std::path::Path,
        to: u32,
        filter: Option<&MarkFilter>,
    ) -> Result<()> {
        let mut last_free = None;
        while self.frames < to && !self.frontier.is_empty() {
            let frame = self.frames + 1;
            let t_frame = std::time::Instant::now();
            let frontier = std::mem::take(&mut self.frontier);
            let edges_dir = dir.join("edges");
            let (mut next, won, st) =
                forward_frame(engine, frontier, &self.door, self.observer.as_ref(), filter, frame, Some(&edges_dir))?;
            // Joined BEFORE this checkpoint: a resume never trusts incomplete runs.
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
            ensure_disk_space(dir, &mut last_free)?;
            self.frames = frame;
            if won && self.win_frame.is_none() {
                self.win_frame = Some(frame);
                eprintln!("[fwd] first win at f{frame}");
            }
            // Exited rows are checkpointed but never expanded; nor are orb
            // rows past their deadline (`not_expanded`).
            let orb = orb_required();
            self.frontier = if won || orb {
                std::thread::scope(|scope| {
                    let handles: Vec<_> = next
                        .into_iter()
                        .map(|mut b| {
                            scope.spawn(move || -> Result<Block> {
                                let skip = not_expanded(&b, frame)?;
                                b.set_skip(skip);
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

/// Persist the position graph after `frame` (atomic renames), every frame so
/// a crash stays resumable; extra edges from discarded frames recur anyway.
pub fn save_pos_graph(dir: &std::path::Path, graph: &crate::search::pos_graph::PosGraph, frame: u32) -> Result<()> {
    let tmp = dir.join("posgraph.tmp");
    graph.save(&tmp)?;
    std::fs::rename(&tmp, pos_graph_path(dir))?;
    let ftmp = dir.join("posgraph.frame.tmp");
    std::fs::write(&ftmp, format!("{frame}\n"))?;
    std::fs::rename(&ftmp, dir.join("posgraph.frame"))?;
    Ok(())
}

/// The last frame the saved position graph covers; `None`: the whole tree.
pub fn pos_graph_frame(dir: &std::path::Path) -> Option<u32> {
    std::fs::read_to_string(dir.join("posgraph.frame")).ok().and_then(|s| s.trim().parse().ok())
}

pub fn pos_graph_path(dir: &std::path::Path) -> std::path::PathBuf {
    dir.join("posgraph.bin")
}

/// A whole forward to `max_frames`, resuming `dir` or from `initial`.
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

/// Stop the forward before the checkpoint disk fills: below
/// `CELESTE_MIN_FREE_GB` (default 20) plus three times what the last frame
/// wrote, free on `dir`'s filesystem, an error -
/// a full disk otherwise fails a worker mid-write (room (5,1) nodiag,
/// 2026-10-06) and starves everything else on the machine. `last_free`: the
/// free bytes after the previous frame of THIS forward (`None` at its first:
/// a delta across the arc phase between two levels counted everything the
/// other processes wrote meanwhile as the frame's, and stopped room (2,3)'s
/// second level at its first frame, 2026-10-07).
fn ensure_disk_space(dir: &std::path::Path, last_free: &mut Option<u64>) -> Result<()> {
    use std::os::unix::ffi::OsStrExt;
    let min_gb: u64 = match std::env::var("CELESTE_MIN_FREE_GB") {
        Ok(v) => v.parse().context("CELESTE_MIN_FREE_GB")?,
        Err(_) => 20,
    };
    let path = std::ffi::CString::new(dir.as_os_str().as_bytes())?;
    // SAFETY: `path` is NUL-terminated; `st` is a valid out-parameter that
    // statvfs fills on success.
    let mut st: libc::statvfs = unsafe { std::mem::zeroed() };
    anyhow::ensure!(unsafe { libc::statvfs(path.as_ptr(), &mut st) } == 0, "statvfs {}: {}", dir.display(), std::io::Error::last_os_error());
    let free = st.f_bavail as u64 * st.f_frsize as u64;
    // A frame can write tens of GB (a room whose frontier explodes): keep room
    // for three more frames like the last one, not just the fixed floor.
    let frame_bytes = last_free.replace(free).map_or(0, |last| last.saturating_sub(free));
    let need = (min_gb << 30) + 3 * frame_bytes;
    anyhow::ensure!(
        free >= need,
        "only {} GB free on {}'s filesystem, the last frame used {} GB (CELESTE_MIN_FREE_GB = {min_gb}, plus 3 frames): stopping the forward before the disk fills",
        free >> 30,
        dir.display(),
        frame_bytes >> 30
    );
    Ok(())
}

/// The per-frame `[fwd]` line (ms) and the `fwd.*` metrics totals.
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

/// Checkpoint a frontier: `frames/fNNN/s{shape}_{seq}.bin` per piece, in row
/// order (position = id); the id-less initial frontier gets its ids here.
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

/// One row of a marks file: `(shape, cell, key.0, key.1, dist)`.
pub type MarkRow = (u64, u32, u64, u64, u32);

/// The marks-file row of a state with `deadline` (`u16::MAX`: none, saved
/// as 0).
pub fn mark_row(shape: u64, key: (u64, u64), cell: u32, deadline: u16, horizon: u32) -> MarkRow {
    (shape, cell, key.0, key.1, horizon - (deadline as u32).min(horizon))
}

/// A marks file (`Visited::save`) from its rows, in any order, each state
/// once.
pub fn save_marks(path: &std::path::Path, mut rows: Vec<MarkRow>, horizon: u32) -> Result<()> {
    rows.sort_unstable();
    crate::search::checkpoint::save_value_to(path, &(rows, horizon))
}

/// An in-memory set of states `(shape, cell, key)` (a level's MARKED states
/// or a diagnostic's), saved as a marks file.
#[derive(Default)]
pub struct Visited {
    /// Sharded by (shape, cell); each key maps to its DEADLINE, the last frame
    /// from which it still wins by the horizon (`u16::MAX`: none).
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

    /// THE MARKS FILE (also read by the UI export): bincode `(rows, horizon)`,
    /// `dist` = horizon - deadline; no deadline saves as 0 (filters alike).
    pub fn save(&self, path: &std::path::Path, horizon: u32) -> Result<()> {
        let mut rows: Vec<MarkRow> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            rows.extend(keys.iter().map(|(&key, &d)| mark_row(*shape, key, *cell, d, horizon)));
        }
        save_marks(path, rows, horizon)
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

/// The first win of a tree complete through `horizon`; `None` if it is not.
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
    /// The mark filter admits a marked node's rows up to its deadline only.
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


    /// A row ref names queues past index 255 without aliasing.
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

    /// A fresh forward keeps none of a killed run's files.
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

    /// A trimmed checkpoint keeps everything the search reads of an old
    /// frame - width, shape, keys, cells, the cell index, the wins - and
    /// refuses to load its rows.
    #[test]
    fn a_trimmed_frame_keeps_its_keys_cells_and_wins() {
        let engine = std::sync::Mutex::new(RefEngine::new().expect("ref engine"));
        let init = vec![Block::keyed(engine.lock().unwrap().initial().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-trim-test");
        let _ = std::fs::remove_dir_all(dir);
        let mut st = ForwardState::start(init, dir, true).expect("start");
        st.extend(&engine, dir, 2, None).expect("two frames");
        type Seen = (u32, u64, Vec<(u32, (u64, u64))>, Vec<u32>, Vec<(u64, (u64, u64), u32)>);
        let seen = |f: &crate::search::checkpoint::FrameFile| -> Seen {
            (f.width(), f.shape_hash(), f.cell_keys().collect(), f.row_cells(), f.wins())
        };
        for (_, p) in frame_paths(dir, 1).expect("frame 1") {
            let before = seen(&crate::search::checkpoint::FrameFile::open(&p).expect("open"));
            assert!(crate::search::checkpoint::trim(&p).expect("trim") > 0, "the rows' values are gone");
            let f = crate::search::checkpoint::FrameFile::open(&p).expect("reopen");
            assert_eq!(seen(&f), before);
            assert!(f.load_all().is_err(), "a trimmed frame's rows do not load");
            assert_eq!(crate::search::checkpoint::trim(&p).expect("trim again"), 0);
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A marks file round-trips deadlines (none comes back as the horizon).
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

    /// Extending frame by frame and resuming reproduce a fresh run's key sets.
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

    /// Room (0,2)'s SPAWN FRAME: kernels and reference make the same
    /// successors. Guards `Graph::tile_flag_over` reading every row a span covers.
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

    /// Room (1,3)'s 127-frame EXIT FRAME: the kernels make the concrete exit's
    /// state, KEY included. Guards a number and its point interval keying alike.
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

    /// The summit wins at the flag: the TAS31 reference wins at frame 55, not before.
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

    /// THE SPLIT FRAME at a near level reaches the unsplit (pinned) state
    /// counts at every boundary, room (2,1) `r0sxhn`. Guards nil-global
    /// removal and the mid-frame cut not widening floors the player reads.
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
