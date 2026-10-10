//! The block, the frame-step interface (`FrameStep`, emitting into the
//! storage's `UnitSink`) and the forward (`ForwardState`, extended frame by
//! frame by `storage::wave::run_wave`, checkpointed, resumable, raisable).
//! A block is OPAQUE to the loop except for its KEYS and POSITIONS.

use anyhow::{Context, Result};
use std::ops::Range;

use celeste_engine::runtime2::{Col, Rt2};

use crate::storage::StateId;

/// Wave phase timers (`CELESTE_PHASES=1`): TSC ticks summed over workers,
/// printed by `print_phases`. Off, `phase_start` is one load and a branch.
pub mod phases {
    use std::sync::atomic::{AtomicU64, Ordering};
    pub const NAMES: [&str; 8] = ["pack", "kernel", "emit", "unit end (sort, encode)", "translate", "layer (gather, edge file)", "slice.setup", "unit (all of engine.run)"];
    pub const PACK: usize = 0;
    pub const KERNEL: usize = 1;
    pub const EMIT: usize = 2;
    pub const END_UNIT: usize = 3;
    pub const TRANSLATE: usize = 4;
    pub const LAYER: usize = 5;
    pub const SETUP: usize = 6;
    pub const UNIT: usize = 7;
    static TICKS: [AtomicU64; 8] = [const { AtomicU64::new(0) }; 8];
    fn on() -> bool {
        static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *ON.get_or_init(|| std::env::var("CELESTE_PHASES").is_ok_and(|v| v == "1"))
    }
    #[inline]
    pub fn start() -> u64 {
        if on() {
            unsafe { core::arch::x86_64::_rdtsc() }
        } else {
            0
        }
    }
    /// Add the ticks since `t0` (a `start`) to phase `p`; returns now.
    #[inline]
    pub fn add(p: usize, t0: u64) -> u64 {
        if t0 == 0 {
            return 0;
        }
        let t = unsafe { core::arch::x86_64::_rdtsc() };
        TICKS[p].fetch_add(t - t0, Ordering::Relaxed);
        t
    }
    pub fn print_phases(wall: std::time::Duration, workers: usize) {
        if !on() {
            return;
        }
        let v: Vec<u64> = TICKS.iter().map(|a| a.swap(0, Ordering::Relaxed)).collect();
        // TSC rate: ticks per second, measured against the wall clock.
        let t0 = std::time::Instant::now();
        let c0 = unsafe { core::arch::x86_64::_rdtsc() };
        std::thread::sleep(std::time::Duration::from_millis(50));
        let hz = (unsafe { core::arch::x86_64::_rdtsc() } - c0) as f64 / t0.elapsed().as_secs_f64();
        let budget = wall.as_secs_f64() * workers as f64;
        let line: Vec<String> = NAMES.iter().zip(&v).map(|(n, &t)| format!("{n} {:.1}s ({:.0}%)", t as f64 / hz, 100.0 * t as f64 / hz / budget)).collect();
        eprintln!("[phases] worker-seconds of {:.1} ({} x {:.2} s): {}", budget, workers, wall.as_secs_f64(), line.join(", "));
    }
}


/// A columnar batch of lanes of one shape: the kernels' own `Rt2` with its
/// key column. Only the per-lane key and position are exposed.
pub struct Block {
    rt2: Rt2,
    /// The rows' ids (`storage::StateId`) when the block is a layer's
    /// piece; empty otherwise.
    ids: Vec<StateId>,
    /// The piece's file seq within its layer (`s{shape}_{seq}.bin`).
    seq: u32,
    /// Lanes the frame step must not expand (a mask, so ids stay consecutive).
    skip: Vec<bool>,
    /// Is it its layer's file `seq` row for row (a unit's sources are then
    /// stored as a row range of it, `storage::edges`)?
    whole: bool,
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
        Block { rt2, ids: Vec::new(), seq: 0, skip: Vec::new(), whole: false }
    }

    /// A reference-engine block keyed as the level's kernels key it (held
    /// buttons widened; objects stay decided, which is exact).
    pub fn keyed(mut rt2: Rt2) -> Result<Self> {
        let level = crate::abstraction::Level { held: crate::abstraction::current_level().held, ..crate::abstraction::Level::EXACT };
        let (_, keys, _) = widened_keys_rt2(&rt2, level)?;
        rt2.row_keys_canonical(crate::compiled::ids());
        rt2.row_keys = keys;
        Ok(Block { rt2, ids: Vec::new(), seq: 0, skip: Vec::new(), whole: false })
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
        self.whole = false;
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

    /// A layer's file `seq`, row for row (`whole`).
    pub fn layer_piece(rt2: Rt2, ids: Vec<u64>, seq: u32) -> Self {
        let mut b = Block::with_ids(rt2, ids, seq);
        b.whole = true;
        b
    }

    /// Is it its layer's file `seq` row for row?
    pub fn is_whole(&self) -> bool {
        self.whole
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
        if let Some(rect) = crate::abstraction::synthetic_win_rect() {
            return Some(rect);
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
    room_is(rt2, crate::game_runner::win_room())
}

/// Per lane: is the `room` global (`room.x`, `room.y`) = `(wx, wy)`?
fn room_is(rt2: &Rt2, (wx, wy): (i16, i16)) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
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

/// Per lane: on the win target? The exit, plus the orb in the orb room,
/// and in a 100% run the room's berry taken.
pub fn wins_of(rt2: &Rt2) -> Result<Vec<bool>> {
    let mut wins = reaches_win(rt2)?;
    if crate::game_runner::hundred() {
        // 100%: the exit counts only with this room's berry taken.
        for (e, b) in wins.iter_mut().zip(got_fruit(rt2)?) {
            *e &= b;
        }
    }
    Ok(wins)
}

/// Per lane: the win target met, the berry aside (`wins_of` without its
/// 100% condition): the forward's "first win", whose frame turns on the
/// not-expanded filter (`not_expanded`) - a 100% exit without the berry is
/// no win, and is not expanded either.
pub fn reaches_win(rt2: &Rt2) -> Result<Vec<bool>> {
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

/// 100%, per lane: can no successor take the room's berry any more? Not
/// taken (`got_fruit` false, not unknown) and nothing left that holds or
/// drops it - the fruit, the fly fruit (gone once it flew off), the key and
/// its chest, a fake wall. Exact: every state the row stands for is lost.
pub fn berry_lost(rt2: &Rt2) -> Result<Vec<bool>> {
    let lanes = rt2.width;
    let ids = crate::compiled::ids();
    let sources = [ids.g_fruit, ids.g_fly_fruit, ids.g_key, ids.g_chest, ids.g_fake_wall];
    if !crate::game_runner::hundred() || sources.iter().any(|&g| !rt2.objects_of_type(ids, g).is_empty()) {
        return Ok(vec![false; lanes]);
    }
    Ok(got_fruit(rt2)?.into_iter().map(|taken| !taken).collect())
}

/// Per lane of a frame's rows: NOT expanded further. An exited row (no kernels
/// for the next room; a win or a `--win-at` exit), a 100% row whose berry is
/// lost (`berry_lost`), and in the orb room a row whose chest is still closed
/// too late for the ceiling (`orb_deadline_skip`).
/// The forward and a resume both take the frontier through this, so a resumed
/// run expands exactly the rows an uninterrupted one does. `minus_one`: the
/// tree's level -1 horizon, the orb deadline's ceiling.
pub fn not_expanded(b: &Block, frame: u32, ceiling: Option<u32>) -> Result<Vec<bool>> {
    let mut skip = b.exits()?;
    // Under a win rectangle (`--win-at`, the summit's flag) a row that LEFT
    // the room is no win, but it is not this room's search either (its
    // shape has no kernel: a coverage gap at room (0,3) f93).
    if win_rect().is_some() {
        for (w, here) in skip.iter_mut().zip(room_is(b.rt2(), crate::game_runner::start_room())?) {
            *w |= !here;
        }
    }
    for (w, lost) in skip.iter_mut().zip(berry_lost(b.rt2())?) {
        *w |= lost;
    }
    if let (true, Some(h)) = (orb_required(), ceiling) {
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

/// THE LEVEL -1 FILTER at a horizon (`CELESTE_LEVEL_MINUS_ONE="H,S"`, H in
/// search steps): drop a queue when the table proves its cell cannot exit
/// by `h` (sound: inductive ranges, clipped successors and deaths
/// accounted). A tree records the filter its frames were built under
/// (`TreeFilter`); a forward runs under its tree's, not the environment's.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MinusOne {
    pub h: u32,
    /// The table's speed bound S, px per frame.
    pub speed: i32,
}

impl MinusOne {
    /// `CELESTE_LEVEL_MINUS_ONE="H,S"`, if set.
    pub fn from_env() -> Option<MinusOne> {
        let s = std::env::var("CELESTE_LEVEL_MINUS_ONE").ok()?;
        let (h, sp) = s.split_once(',').expect("CELESTE_LEVEL_MINUS_ONE=\"H,S\"");
        let h: u32 = h.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE horizon");
        let speed: i32 = sp.trim().parse().expect("CELESTE_LEVEL_MINUS_ONE speed bound (px per frame)");
        assert!(h < u32::MAX, "CELESTE_LEVEL_MINUS_ONE horizon {h}");
        Some(MinusOne { h, speed })
    }

    /// The start room's table at this speed bound (one per process).
    pub fn table(&self) -> &'static crate::trace::level_minus_one::CostToGo {
        minus_one_table(self.speed)
    }
}

/// The level -1 table at speed bound `speed`, built (or read from its cache)
/// once, on a thread with the tracer's stack.
fn minus_one_table(speed: i32) -> &'static crate::trace::level_minus_one::CostToGo {
    static TABLE: std::sync::OnceLock<(i32, crate::trace::level_minus_one::CostToGo)> = std::sync::OnceLock::new();
    let (sp, table) = TABLE.get_or_init(|| {
        // The table measures the EXIT: unsound for a position win.
        assert!(win_rect().is_none(), "CELESTE_LEVEL_MINUS_ONE: the table measures the room exit; this room's win is a position");
        let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
        let threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
        let t = std::time::Instant::now();
        let table = std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(move || crate::trace::level_minus_one::cost_to_go(std::path::Path::new(&root), speed, threads))
            .expect("spawn the level -1 builder")
            .join()
            .expect("the level -1 builder panicked")
            .unwrap_or_else(|e| panic!("building the level -1 table: {e:#}"));
        eprintln!("[level -1] table ready in {:.1} s (S = {speed}): the start state's d = {}", t.elapsed().as_secs_f64(), table.start_d);
        (speed, table)
    });
    assert_eq!(*sp, speed, "one level -1 table per process: S = {sp}, then S = {speed}");
    table
}

/// What a forward's flush filters by.
#[derive(Clone, Copy, Default)]
pub struct Filters<'a> {
    /// The coarser level's arc marks (the objects ladder).
    pub marks: Option<&'a MarkFilter<'a>>,
    /// Level -1.
    pub minus_one: Option<MinusOne>,
}

impl Filters<'_> {
    /// Are level -1's drops noted? In a tree filtered by nothing else - one a
    /// raise extends (a marks-filtered tree is rebuilt for another horizon).
    pub fn notes_drops(&self) -> bool {
        self.marks.is_none() && self.minus_one.is_some()
    }
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
        Ok(self.deadlines(rt2)?.into_iter().map(|d| d.is_some_and(|d| u32::from(d) >= frame)).collect())
    }

    /// Per lane of `rt2`, its projection's deadline at the coarser level
    /// (`None`: not marked there).
    pub fn deadlines(&self, rt2: &Rt2) -> Result<Vec<Option<u16>>> {
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser)?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| self.marked.deadline(shape, *k, c)).collect())
    }

    /// The coarser level.
    pub fn coarser(&self) -> crate::abstraction::Level {
        self.coarser
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
    let keys = w.row_keys_canonical(crate::compiled::ids());
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
    fn run(&self, block: &Block, cell_in: &[u32], lanes: &[usize], sink: &mut crate::storage::unit::UnitSink) -> Result<()>;

    /// Build what the engine builds on first use (the kernels) before a
    /// wave's clock starts: built inside the wave, the first frame's workers
    /// all waited on the build's lock (room (6,2) 100% f57: 61 of 152
    /// worker-s, so a one-frame bench timed the build).
    fn warm(&self) {}
}

/// Where a wave's new states go.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Layer {
    /// A new frame: pieces numbered from 0.
    New,
    /// A frame the tree already has (a RAISE).
    Raised(Raised),
}

/// What a raise's wave knows of the tree it adds to.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Raised {
    /// The new pieces' first seq (after the frame's own).
    pub first_seq: u32,
    /// The filter the tree was built under: where it admitted a cell, the
    /// tree's sources' edges into it are recorded already.
    pub old: MinusOne,
    /// The sources (layer `frame - 1`) below this seq are the tree's; from
    /// it on, the raise added them.
    pub old_seqs: u32,
}

/// One forward frame's timings and sizes, for the `[fwd]` log line.
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    pub blocks_in: usize,
    pub lanes_in: usize,
    /// Emissions past level -1: each a lookup and an edge.
    pub lanes_raw: usize,
    /// New states (requests the translation found new).
    pub lanes_kept: usize,
    pub blocks_out: usize,
    pub lanes_out: usize,
    /// The units' wall time and its barrier idle fraction.
    pub t_wave: std::time::Duration,
    pub wave_idle: f64,
    /// The translation, wall.
    pub t_translate: std::time::Duration,
    /// The layer's gather and the edge file, wall.
    pub t_layer: std::time::Duration,
    pub units: usize,
    /// The units' requests and lids (distinct target entries per unit).
    pub requests: u64,
    pub lids: u64,
    /// Edges recorded, and the frame's edge file bytes.
    pub edge_records: u64,
    pub edge_bytes: u64,
    /// The input frontier's row storage, bytes.
    pub bytes_in: usize,
    /// Bytes allocated in the visited set.
    pub visited_bytes: usize,
    /// Anonymous resident set at the frame's start, after the wave, at the end.
    pub rss_start: f64,
    pub rss_wave: f64,
    pub rss_end: f64,
    /// File-backed resident pages at the end of the frame.
    pub rss_file: f64,
}

/// The level -1 filter a tree's frames were built under
/// (`<dir>/level_minus_one.txt`). A forward extends a tree under ITS
/// filter, never the environment's, so every frame of a tree is filtered
/// alike.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TreeFilter {
    /// None (`off`): the frames do not depend on the horizon.
    Off,
    /// Level -1 at `m`, with its table's fingerprint: the tree serves
    /// horizons up to `m.h`. A level-0 tree notes every frame's drops
    /// (`dropped/`), so a RAISE extends it exactly to a larger one.
    MinusOne { m: MinusOne, table: u64 },
    /// Filtered at this horizon by a binary that noted no drops (a bare
    /// number, the `gemskip-campaign` record): serves complete horizons up
    /// to it, cannot be extended.
    Legacy(u32),
}

impl TreeFilter {
    fn of(m: Option<MinusOne>) -> TreeFilter {
        match m {
            None => TreeFilter::Off,
            Some(m) => TreeFilter::MinusOne { m, table: m.table().fingerprint() },
        }
    }

    fn path(dir: &std::path::Path) -> std::path::PathBuf {
        dir.join("level_minus_one.txt")
    }

    /// The tree's record; `None` without one (a tree from before records,
    /// filtered or not: unknown).
    pub fn read(dir: &std::path::Path) -> Result<Option<TreeFilter>> {
        let path = Self::path(dir);
        let s = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e).with_context(|| path.display().to_string()),
        };
        let s = s.trim();
        if s == "off" {
            return Ok(Some(TreeFilter::Off));
        }
        if let Ok(h) = s.parse::<u32>() {
            return Ok(Some(TreeFilter::Legacy(h)));
        }
        let w: Vec<&str> = s.split_whitespace().collect();
        let parsed = match w[..] {
            ["h", h, "speed", sp, "table", fp] => (|| Some((h.parse().ok()?, sp.parse().ok()?, u64::from_str_radix(fp, 16).ok()?)))(),
            _ => None,
        };
        let (h, speed, table) = parsed.ok_or_else(|| anyhow::anyhow!("{}: not a level -1 record: {s:?}", path.display()))?;
        Ok(Some(TreeFilter::MinusOne { m: MinusOne { h, speed }, table }))
    }

    fn write(&self, dir: &std::path::Path) -> Result<()> {
        let s = match self {
            TreeFilter::Off => "off".to_string(),
            TreeFilter::MinusOne { m, table } => format!("h {} speed {} table {table:016x}", m.h, m.speed),
            TreeFilter::Legacy(h) => h.to_string(),
        };
        std::fs::create_dir_all(dir)?;
        let tmp = dir.join("level_minus_one.tmp");
        std::fs::write(&tmp, format!("{s}\n"))?;
        std::fs::rename(&tmp, Self::path(dir))?;
        Ok(())
    }

    /// The filter a forward extending the tree runs under.
    fn minus_one(&self, dir: &std::path::Path) -> Result<Option<MinusOne>> {
        match *self {
            TreeFilter::Off => Ok(None),
            TreeFilter::MinusOne { m, .. } => Ok(Some(m)),
            TreeFilter::Legacy(h) => {
                anyhow::bail!("{}: filtered by level -1 at step {h} before drops were noted: it cannot be extended (delete it)", dir.display())
            }
        }
    }

    /// The horizon the tree's frames were filtered for (`None`: none).
    fn ceiling(&self) -> Option<u32> {
        match *self {
            TreeFilter::Off => None,
            TreeFilter::MinusOne { m, .. } => Some(m.h),
            TreeFilter::Legacy(h) => Some(h),
        }
    }
}

/// Frame `frame`'s level -1 drops, as their SOURCES (rows of layer `frame -
/// 1`): `(id, the smallest horizon that admits one of its dropped
/// successors)`, sorted by id.
pub fn dropped_path(dir: &std::path::Path, frame: u32) -> std::path::PathBuf {
    dir.join("dropped").join(format!("f{frame:03}.bin"))
}

/// Present while a raise rewrites the tree's frames: a tree with it is
/// refused (half raised, it is neither the old tree nor the new one).
fn raise_marker(dir: &std::path::Path) -> std::path::PathBuf {
    dir.join("raising.txt")
}

/// The rows `ids` (sorted) of layer `layer`, as one block per file holding
/// some, exactly those rows (a raise's sources).
fn load_sources(dir: &std::path::Path, layer: u32, ids: &[StateId]) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    let mut found = 0usize;
    for (seq, file) in frame_files(dir, layer)? {
        let mut rows: Vec<u32> = ids.iter().filter_map(|&id| file.find_row(id)).collect();
        if rows.is_empty() {
            continue;
        }
        rows.sort_unstable();
        found += rows.len();
        let mut ranges: Vec<Range<u32>> = Vec::new();
        for &r in &rows {
            match ranges.last_mut() {
                Some(x) if x.end == r => x.end = r + 1,
                _ => ranges.push(r..r + 1),
            }
        }
        let rt2 = file.load_rows(&ranges)?.expect("rows of a non-empty range");
        out.push(Block::with_ids(rt2, rows.iter().map(|&r| file.id_at(r)).collect(), seq));
    }
    anyhow::ensure!(found == ids.len(), "{}: {} of {} sources found in layer {layer}", dir.display(), found, ids.len());
    Ok(out)
}

/// Every state's layer (a raise's refusal of a hit from a later layer).
fn state_layers(dir: &std::path::Path, last: u32) -> Result<crate::storage::wave::StateLayers> {
    let geo = crate::storage::geometry();
    let mut layers = crate::storage::wave::StateLayers::new((geo.side * geo.side) as usize);
    for f in 0..=last {
        for (_, file) in frame_files(dir, f)? {
            for id in file.ids() {
                layers.set(id, f);
            }
        }
    }
    Ok(layers)
}

/// The forward's in-memory state, EXTENDED frame by frame (each checkpointed).
pub struct ForwardState {
    frontier: Vec<Block>,
    /// Every state reached so far (`storage::visited`).
    visited: crate::storage::visited::VisitedSet,
    /// The tree's transfers (`storage::edges::XferTable`).
    xfers: crate::storage::edges::XferTable,
    observer: Option<crate::search::pos_graph::PosObserver>,
    /// The last frame computed (and checkpointed).
    pub frames: u32,
    /// The first frame a lane won, if any so far.
    pub win_frame: Option<u32>,
    /// The filter the tree's frames were built under (`None`: a tree from
    /// before records, which is not extended).
    pub filter: Option<TreeFilter>,
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
    /// Frame 0: `initial` into an empty visited set (its ids) and
    /// checkpointed; the tree is filtered by `minus_one` (recorded).
    pub fn start(mut initial: Vec<Block>, dir: &std::path::Path, record: bool, minus_one: Option<MinusOne>) -> Result<Self> {
        // A fresh tree keeps nothing a killed run left (stale edge files).
        for sub in ["frames", "edges", "dropped"] {
            let p = dir.join(sub);
            if p.exists() {
                std::fs::remove_dir_all(&p).with_context(|| p.display().to_string())?;
            }
        }
        let filter = TreeFilter::of(minus_one);
        filter.write(dir)?;
        let mut visited = crate::storage::visited::VisitedSet::new(*crate::storage::geometry());
        let meta = crate::storage::wave::seed(&mut visited, &mut initial)?;
        checkpoint_frontier(dir, 0, &mut initial, true)?;
        crate::storage::meta::save(dir, 0, None, &meta)?;
        crate::storage::edges::set_done_frame(&dir.join("edges"), 0)?;
        let observer = record.then(crate::search::pos_graph::PosObserver::default);
        // An (empty) graph from the start: every tree with frames has one.
        if let Some(o) = observer.as_ref() {
            save_pos_graph(dir, &o.snapshot(), 0)?;
        }
        Ok(ForwardState {
            frontier: initial,
            visited,
            xfers: Default::default(),
            observer,
            frames: 0,
            win_frame: None,
            filter: Some(filter),
        })
    }

    /// The forward as its checkpoint tree left it (`None`: no tree); extending
    /// it matches extending the original.
    pub fn resume(dir: &std::path::Path, record: bool) -> Result<Option<Self>> {
        anyhow::ensure!(
            !raise_marker(dir).exists(),
            "{}: a raise was interrupted ({}); its frames are half raised - delete the tree",
            dir.display(),
            raise_marker(dir).display()
        );
        let mut last: Option<u32> = None;
        while dir.join("frames").join(format!("f{:03}", last.map_or(0, |f| f + 1))).is_dir() {
            last = Some(last.map_or(0, |f| f + 1));
        }
        let Some(mut last) = last else { return Ok(None) };
        let t = std::time::Instant::now();
        // Trust only frames whose files are complete (at most one lost).
        let edges_dir = dir.join("edges");
        let done = crate::storage::edges::done_frame(&edges_dir).unwrap_or(last);
        if done < last {
            for f in done + 1..=last {
                std::fs::remove_dir_all(dir.join("frames").join(format!("f{:03}", f)))?;
                let _ = std::fs::remove_file(dropped_path(dir, f));
            }
            eprintln!("[resume] frames f{} to f{last} discarded: their files were not complete", done + 1);
            last = done;
        }
        crate::storage::edges::discard_after(&edges_dir, last)?;
        let filter = TreeFilter::read(dir)?;
        let (visited, win_frame) = restore_visited(dir, last)?;
        let xfers = crate::storage::edges::XferTable::load(&edges_dir)?;
        // The frontier: the last layer minus the rows the forward does not
        // expand (`not_expanded`, as `extend` decides it; a tree without a
        // record by the environment's ceiling, as it was built).
        let ceiling = match filter {
            Some(f) => f.ceiling(),
            None => MinusOne::from_env().map(|m| m.h),
        };
        let mut frontier: Vec<Block> = load_frame(dir, last)?;
        for b in &mut frontier {
            let skip = not_expanded(b, last, ceiling)?;
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
            "[resume] {}: f{last} ({} lanes), visited {} states in {} entries, first win {:?}, level -1 {:?}, {:.1} s",
            dir.display(),
            frontier.iter().map(Block::lanes).sum::<usize>(),
            visited.len(),
            visited.entries(),
            win_frame,
            filter,
            t.elapsed().as_secs_f64()
        );
        Ok(Some(ForwardState { frontier, visited, xfers, observer, frames: last, win_frame, filter }))
    }

    /// The visited set's size: every distinct state reached so far.
    pub fn visited_len(&self) -> usize {
        self.visited.len()
    }

    /// The visited set (`bench-frame`, `bench-storage`).
    pub fn visited(&self) -> &crate::storage::visited::VisitedSet {
        &self.visited
    }

    /// Compute and checkpoint frames `frames+1 ..= to` under the tree's
    /// level -1 filter and the coarser level's `marks`; only an empty
    /// frontier stops it (the backward seeds from wins at every frame).
    pub fn extend(
        &mut self,
        engine: &dyn FrameStep,
        dir: &std::path::Path,
        to: u32,
        marks: Option<&MarkFilter>,
    ) -> Result<()> {
        let minus_one = match self.filter {
            Some(f) => f.minus_one(dir)?,
            None => anyhow::bail!("{}: no level -1 record (a tree from before records): it cannot be extended (delete it)", dir.display()),
        };
        let filters = Filters { marks, minus_one };
        let mut last_free = None;
        while self.frames < to && !self.frontier.is_empty() {
            let frame = self.frames + 1;
            let t_frame = std::time::Instant::now();
            let frontier = std::mem::take(&mut self.frontier);
            let edges_dir = dir.join("edges");
            let cx = crate::storage::wave::WaveCtx {
                visited: &mut self.visited,
                xfers: &mut self.xfers,
                pos: self.observer.as_ref(),
                filters,
                frame,
                edges_dir: Some(&edges_dir),
                layer: Layer::New,
                layers: None,
            };
            let wave = crate::storage::wave::run_wave(engine, frontier, cx)?;
            let (mut next, won, st) = (wave.next, wave.won, wave.stats);
            // Before the frame is trusted (its checkpoint, then done.txt).
            if filters.notes_drops() {
                crate::search::checkpoint::save_value_to(&dropped_path(dir, frame), &wave.dropped)?;
            }
            let t = std::time::Instant::now();
            checkpoint_frontier(dir, frame, &mut next, true)?;
            crate::storage::meta::save(dir, frame, None, &wave.meta)?;
            let t_ckpt = t.elapsed();
            crate::storage::edges::set_done_frame(&edges_dir, frame)?;
            // A resume loads frame `frame` as its frontier and never an
            // earlier one's rows: those may go.
            if trim_rows() && frame >= 2 {
                for (_, p) in frame_paths(dir, frame - 1)? {
                    crate::search::checkpoint::trim(&p)?;
                }
            }
            let t = std::time::Instant::now();
            if let Some(o) = self.observer.as_ref() {
                save_pos_graph(dir, &o.snapshot(), frame)?;
            }
            let t_pos = t.elapsed();
            log_frame(frame, &st, t_ckpt, t_pos, t_frame.elapsed(), self.visited_len());
            ensure_disk_space(dir, &mut last_free)?;
            self.frames = frame;
            if won && self.win_frame.is_none() {
                self.win_frame = Some(frame);
                eprintln!("[fwd] first win at f{frame}");
            }
            // Exited rows are checkpointed but never expanded; nor are orb
            // rows past their deadline (`not_expanded`).
            let orb = orb_required();
            let ceiling = minus_one.map(|m| m.h);
            self.frontier = if won || orb {
                std::thread::scope(|scope| {
                    let handles: Vec<_> = next
                        .into_iter()
                        .map(|mut b| {
                            scope.spawn(move || -> Result<Block> {
                                let skip = not_expanded(&b, frame, ceiling)?;
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
        if let Some(o) = self.observer.as_ref() {
            save_pos_graph(dir, &o.snapshot(), self.frames)?;
        }
        Ok(())
    }

    /// RAISE the tree's level -1 filter to `to` (`None`: off) over the frames
    /// it has. A larger horizon keeps a superset at every frame: the rows
    /// the old filter dropped that `to` admits, and everything they reach.
    /// Each frame noted the SOURCES of its drops with the smallest horizon
    /// admitting one (`dropped/`), so frame by frame this re-expands those
    /// sources and the states the raise added one frame earlier, under `to`,
    /// against the whole tree's visited set: a new state joins the frame's
    /// layer (new pieces, numbered after its own; new entries numbered after
    /// every entry), an old one gets its edge; a re-expanded source's edges
    /// into cells the old filter admitted are recorded already and skipped
    /// (`Raised`). The new edges go to the frame's RAISED edge file
    /// (`f{frame}.r{seq}.bin`). The result is the tree a fresh forward under
    /// `to` makes - the same states per frame, the same edges, the same
    /// notes - unless the larger horizon reaches a state SOONER than the
    /// tree does (the table inconsistent along an edge): refused
    /// (`UnitSink::emit`). Then `extend` continues under `to`.
    pub fn raise(&mut self, engine: &dyn FrameStep, dir: &std::path::Path, to: Option<MinusOne>) -> Result<()> {
        let Some(TreeFilter::MinusOne { m: from, .. }) = self.filter else {
            anyhow::bail!("{}: only a tree built under level -1 can be raised (its record: {:?})", dir.display(), self.filter);
        };
        let h_new = to.map_or(u32::MAX, |m| m.h);
        anyhow::ensure!(h_new > from.h, "{}: a raise from step {} to {to:?} is not a raise", dir.display(), from.h);
        anyhow::ensure!(to.is_none_or(|m| m.speed == from.speed), "{}: the tree's level -1 table has S = {}, the raise asks for {to:?}", dir.display(), from.speed);
        anyhow::ensure!(
            !orb_required(),
            "{}: in the orb room the rows not expanded past the chest's deadline depend on the horizon: a raise is not supported (delete the tree)",
            dir.display()
        );
        let t0 = std::time::Instant::now();
        let last = self.frames;
        let edges_dir = dir.join("edges");
        let notes: Vec<Vec<(StateId, u32)>> = (1..=last)
            .map(|t| {
                let p = dropped_path(dir, t);
                crate::search::checkpoint::load_value_from(&p).with_context(|| format!("{}: frame {t}'s level -1 drops were not noted: the tree cannot be raised (delete it)", p.display()))
            })
            .collect::<Result<_>>()?;
        // Before touching the tree: the rows to re-expand hold their values.
        for t in 1..=last {
            let redo: Vec<StateId> = notes[t as usize - 1].iter().filter(|e| e.1 <= h_new).map(|e| e.0).collect();
            for (_, file) in frame_files(dir, t - 1)? {
                anyhow::ensure!(
                    !file.trimmed() || redo.iter().all(|&id| file.find_row(id).is_none()),
                    "{}: frame {}'s rows were trimmed (CELESTE_TRIM_ROWS) and the raise must re-expand some: delete the tree",
                    dir.display(),
                    t - 1
                );
            }
        }
        let mut layers = state_layers(dir, last)?;
        std::fs::write(raise_marker(dir), format!("raising level -1 from step {} to {to:?}\n", from.h))?;
        eprintln!("[raise] {}: level -1 from step {} to {}, frames 1-{last}", dir.display(), from.h, to.map_or("off".to_string(), |m| format!("step {}", m.h)));
        let filters = Filters { marks: None, minus_one: to };
        let mut new_prev: Vec<Block> = Vec::new();
        let (mut redone, mut added) = (0usize, 0usize);
        // The first seq the raise gave the previous layer (`u32::MAX`: none).
        let mut old_seqs = u32::MAX;
        for t in 1..=last {
            let notes_t = &notes[t as usize - 1];
            let redo: Vec<StateId> = notes_t.iter().filter(|e| e.1 <= h_new).map(|e| e.0).collect();
            if redo.is_empty() && new_prev.is_empty() {
                old_seqs = u32::MAX;
                continue;
            }
            let t_frame = std::time::Instant::now();
            let mut frontier = load_sources(dir, t - 1, &redo)?;
            let n_new = new_prev.iter().map(Block::lanes).sum::<usize>();
            frontier.append(&mut new_prev);
            let first_seq = frame_paths(dir, t)?.iter().map(|(s, _)| s + 1).max().unwrap_or(0);
            let layer = Layer::Raised(Raised { first_seq, old: from, old_seqs });
            let cx = crate::storage::wave::WaveCtx {
                visited: &mut self.visited,
                xfers: &mut self.xfers,
                pos: self.observer.as_ref(),
                filters,
                frame: t,
                edges_dir: Some(&edges_dir),
                layer,
                layers: Some(&layers),
            };
            let wave = crate::storage::wave::run_wave(engine, frontier, cx)?;
            old_seqs = first_seq;
            let mut next = wave.next;
            checkpoint_frontier(dir, t, &mut next, false)?;
            crate::storage::meta::save(dir, t, Some(first_seq), &wave.meta)?;
            for b in &next {
                for &id in b.ids() {
                    layers.set(id, t);
                }
            }
            // The frame's notes: the sources not re-expanded keep theirs.
            if to.is_some() {
                let mut kept: Vec<(StateId, u32)> = notes_t.iter().copied().filter(|e| e.1 > h_new).collect();
                kept.extend(wave.dropped);
                kept.sort_unstable();
                kept.dedup_by_key(|e| e.0);
                crate::search::checkpoint::save_value_to(&dropped_path(dir, t), &kept)?;
            }
            if wave.won {
                self.win_frame = Some(self.win_frame.map_or(t, |w| w.min(t)));
            }
            let kept = next.iter().map(Block::lanes).sum::<usize>();
            eprintln!(
                "[raise] f{t:03} re-expanded {} sources + {n_new} new, raw {} kept {kept} new, {} edges, {:.0} ms",
                redo.len(),
                wave.stats.lanes_raw,
                wave.stats.edge_records,
                t_frame.elapsed().as_secs_f64() * 1e3
            );
            redone += redo.len();
            added += kept;
            for b in &mut next {
                let skip = not_expanded(b, t, to.map(|m| m.h))?;
                b.set_skip(skip);
            }
            new_prev = next;
        }
        // The last layer's new states join the frontier `extend` expands.
        self.frontier.append(&mut new_prev);
        let filter = TreeFilter::of(to);
        filter.write(dir)?;
        self.filter = Some(filter);
        if to.is_none() && dir.join("dropped").exists() {
            std::fs::remove_dir_all(dir.join("dropped"))?;
        }
        if let Some(o) = self.observer.as_ref() {
            save_pos_graph(dir, &o.snapshot(), self.frames)?;
        }
        std::fs::remove_file(raise_marker(dir))?;
        eprintln!(
            "[raise] {}: {redone} sources re-expanded, {added} states added over f1-f{last} in {:.1} s",
            dir.display(),
            t0.elapsed().as_secs_f64()
        );
        Ok(())
    }
    /// The position graph recorded so far (None when not recording).
    pub fn pos_graph(&self) -> Option<crate::search::pos_graph::PosGraph> {
        self.observer.as_ref().map(|o| o.snapshot())
    }
}

/// A level's tree through step `to` under the run's level -1 filter `want`
/// (`CELESTE_LEVEL_MINUS_ONE`): used as it is when it reaches `to` under a
/// filter that serves it (none, or one at a horizon >= `to`); else resumed -
/// RAISED to `want` first when its filter is below `to` - and extended under
/// its filter, or started under `want`. A tree is never filtered lower than
/// it was built: frames of one tree are filtered alike. `marks`: the coarser
/// level's filter (the caller rebuilds such a tree for another horizon).
/// `engine` is built only when frames are computed.
pub fn grow_tree(
    dir: &std::path::Path,
    to: u32,
    want: Option<MinusOne>,
    marks: Option<&MarkFilter>,
    engine: impl FnOnce() -> Result<Box<dyn FrameStep>>,
    initial: impl FnOnce() -> Result<Vec<Block>>,
) -> Result<ForwardResult> {
    if let Some(w) = want {
        anyhow::ensure!(to <= w.h, "CELESTE_LEVEL_MINUS_ONE at step {} is below the horizon, step {to}", w.h);
    }
    let complete = tree_first_win_through(dir, to)?;
    let raise_to: Option<Option<MinusOne>> = if !dir.join("frames").join("f000").is_dir() {
        None
    } else {
        match TreeFilter::read(dir)? {
            None => {
                anyhow::ensure!(
                    complete.is_some(),
                    "{}: the tree has no level -1 record (built before records), so it is used only where it is complete: delete it to extend it",
                    dir.display()
                );
                None
            }
            Some(TreeFilter::Legacy(h)) => {
                anyhow::ensure!(
                    complete.is_some() && to <= h,
                    "{}: filtered by level -1 at step {h} before drops were noted: it serves complete horizons up to step {h} only (delete it)",
                    dir.display()
                );
                None
            }
            Some(TreeFilter::Off) => {
                if want.is_some() {
                    eprintln!("[forward] {}: the tree is not filtered by level -1; it stays unfiltered", dir.display());
                }
                None
            }
            Some(TreeFilter::MinusOne { m, table }) => {
                if let Some(w) = want {
                    anyhow::ensure!(w.speed == m.speed, "{}: the tree's level -1 table has S = {}, the run asks for S = {}", dir.display(), m.speed, w.speed);
                }
                if to <= m.h {
                    None
                } else {
                    if let Some(w) = want {
                        anyhow::ensure!(
                            w.table().fingerprint() == table,
                            "{}: the level -1 table changed since the tree was built, so its notes do not raise it exactly (delete it)",
                            dir.display()
                        );
                    }
                    Some(want)
                }
            }
        }
    };
    if let (None, Some(first_win)) = (raise_to, complete) {
        eprintln!("[forward] {}: the tree reaches step {to}", dir.display());
        let pos_graph = crate::search::pos_graph::PosGraph::load(&pos_graph_path(dir)).ok();
        return Ok(ForwardResult { win_frame: first_win, frames: to, pos_graph });
    }
    let engine = engine()?;
    let mut st = match ForwardState::resume(dir, true)? {
        Some(s) => s,
        None => ForwardState::start(initial()?, dir, true, want)?,
    };
    if let Some(target) = raise_to {
        st.raise(engine.as_ref(), dir, target)?;
    }
    st.extend(engine.as_ref(), dir, to, marks)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
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

/// A fresh forward from `initial` to `max_frames` (the tests' driver).
#[cfg(test)]
pub fn forward_run(
    engine: &dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, record, None)?;
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
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    eprintln!(
        "[fwd] f{frame:03} in {}/{} raw {} kept {} out {}/{} visited {} | \
         wave {:.0} (idle {:.0}%) translate {:.0} layer {:.0} ckpt {:.0} pos {:.0} total {:.0} ms | \
         units {} requests {} lids {} edges {} ({:.2} B) | \
         in {:.2} visited {:.2} GB rss start {:.2} wave {:.2} end {:.2} peak {:.2} GB (anon; file {:.2})",
        st.blocks_in,
        st.lanes_in,
        st.lanes_raw,
        st.lanes_kept,
        st.blocks_out,
        st.lanes_out,
        visited,
        ms(st.t_wave),
        st.wave_idle * 100.0,
        ms(st.t_translate),
        ms(st.t_layer),
        ms(t_ckpt),
        ms(t_pos),
        ms(t_total),
        st.units,
        st.requests,
        st.lids,
        st.edge_records,
        st.edge_bytes as f64 / st.edge_records.max(1) as f64,
        st.bytes_in as f64 / 1e9,
        st.visited_bytes as f64 / 1e9,
        st.rss_start,
        st.rss_wave,
        st.rss_end,
        crate::metrics::peak_rss_gb(),
        st.rss_file,
    );
    crate::metrics::record("fwd.wave", st.t_wave);
    crate::metrics::record("fwd.translate", st.t_translate);
    crate::metrics::record("fwd.layer", st.t_layer);
    crate::metrics::record("fwd.checkpoint", t_ckpt);
    crate::metrics::record("fwd.posgraph", t_pos);
    crate::metrics::record("fwd.frame", t_total);
}

/// Checkpoint a frontier: `frames/fNNN/s{shape}_{seq}.bin` per piece, rows
/// in id order. `fresh`: a new frame (any old files go); else pieces a raise
/// adds to the frame, beside its own.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &mut [Block], fresh: bool) -> Result<()> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    // A re-run frame must not leave stale files behind.
    if fresh {
        let _ = std::fs::remove_dir_all(&fdir);
    }
    std::fs::create_dir_all(&fdir)?;
    // A frame's seqs name its files: distinct.
    {
        let mut seqs: Vec<u32> = frontier.iter().map(|b| b.seq).collect();
        seqs.sort_unstable();
        anyhow::ensure!(seqs.windows(2).all(|w| w[0] != w[1]), "checkpoint f{frame}: two pieces share a seq");
    }
    std::thread::scope(|scope| {
        let handles: Vec<_> = frontier
            .iter_mut()
            .map(|block| {
                let fdir = &fdir;
                scope.spawn(move || -> Result<()> {
                    anyhow::ensure!(block.ids.len() == block.lanes(), "checkpoint f{frame}: a block without its ids");
                    let cells = block.positions()?;
                    let wins = block.wins()?;
                    let path = fdir.join(format!("s{:016x}_{:04}.bin", block.shard_shape(), block.seq));
                    anyhow::ensure!(fresh || !path.exists(), "checkpoint f{frame}: {} exists", path.display());
                    crate::search::checkpoint::save_block(&path, &block.rt2, &block.ids, &cells, &wins)
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
            out.push(Block::layer_piece(rt2, file.ids(), seq));
        }
    }
    Ok(out)
}

/// One stored row by its id: the row, its shape, its cell and its layer
/// (found by a binary search in each frame's files: a diagnostic's lookup).
pub fn load_row(dir: &std::path::Path, id: StateId) -> Result<(Rt2, u64, u32, u32)> {
    let mut frame = 0;
    while dir.join("frames").join(format!("f{frame:03}")).is_dir() {
        for (_, f) in frame_files(dir, frame)? {
            if let Some(row) = f.find_row(id) {
                let rt2 = f.load_rows(&[row..row + 1])?.ok_or_else(|| anyhow::anyhow!("no row for id {}", crate::storage::show_id(id)))?;
                return Ok((rt2, f.shape_hash(), f.cell_at(row), frame));
            }
        }
        frame += 1;
    }
    anyhow::bail!("{}: no frame holds state {}", dir.display(), crate::storage::show_id(id))
}

/// The visited set of a tree's frames `0..=last` (the storage metadata's
/// shapes and entries, the frame files' ids for the cells), and its first
/// win. A row whose key is not its entry's is a collision, fatal.
pub fn restore_visited(dir: &std::path::Path, last: u32) -> Result<(crate::storage::visited::VisitedSet, Option<u32>)> {
    let geo = *crate::storage::geometry();
    let mut visited = crate::storage::visited::VisitedSet::new(geo);
    let metas = crate::storage::meta::load_tree(dir, last)?;
    let mut shapes: Vec<(u32, u64)> = metas.iter().flat_map(|m| m.shapes.iter().copied()).collect();
    shapes.sort_unstable();
    for (i, s) in shapes {
        visited.restore_shape(i, s)?;
    }
    let mut entries: Vec<(u32, u32, crate::storage::visited::Key)> = metas.into_iter().flat_map(|m| m.entries).collect();
    entries.sort_unstable();
    let mut touched: Vec<u32> = Vec::new();
    for (r, e, k) in entries {
        anyhow::ensure!((r / geo.slots) < visited.shapes().len() as u32, "{}: entry of region {r}, whose shape is not numbered", dir.display());
        visited.table_mut(r).push_numbered(geo.words, e, k)?;
        if touched.last() != Some(&r) {
            touched.push(r);
        }
    }
    for r in touched {
        visited.table_mut(r).reindex();
    }
    let mut win_frame = None;
    for f in 0..=last {
        for (_, file) in frame_files(dir, f)? {
            for row in 0..file.width() {
                let id = file.id_at(row);
                let (r, e, l) = (crate::storage::id_region(id), crate::storage::id_entry(id), crate::storage::id_local(id));
                let t = visited.table_mut(r);
                anyhow::ensure!((e as usize) < t.len() && t.key(e) == file.key_at(row), "{}: f{f} row {row} (state {}) is not its entry's key", dir.display(), crate::storage::show_id(id));
                anyhow::ensure!(t.set(geo.words, e, l), "{}: state {} stored twice", dir.display(), crate::storage::show_id(id));
            }
            if !file.wins().is_empty() {
                win_frame = Some(win_frame.map_or(f, |w: u32| w.min(f)));
            }
        }
    }
    Ok((visited, win_frame))
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
    /// An order-independent hash of every `(shape, cell, key, deadline)`:
    /// equal values filter alike (`MarkFilter`).
    pub fn filter_fingerprint(&self) -> u64 {
        use celeste_engine::runtime2::mix64;
        let mut acc = 0u64;
        for ((shape, cell), keys) in &self.shards {
            for (&(k0, k1), &d) in keys {
                acc = acc.wrapping_add(mix64(k0 ^ mix64(k1 ^ mix64(*shape ^ ((*cell as u64) << 16 | d as u64)))));
            }
        }
        acc
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
    let done = crate::storage::edges::done_frame(&dir.join("edges"));
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


    use super::*;
    use crate::trace::refengine::RefEngine;
    use celeste_core::pico8_num::Pico8Num as P8;
    use celeste_engine::runtime2::AV;
    use std::sync::Mutex;

    /// A fresh forward keeps none of a killed run's files.
    #[test]
    fn a_fresh_forward_clears_what_a_killed_run_left() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::keyed(engine.initial().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-fresh-start-test");
        let _ = std::fs::remove_dir_all(dir);
        let stale = [dir.join("edges/f009.bin"), dir.join("edges/xfer.bin"), dir.join("frames/f009/b0_s0.bin")];
        for f in &stale {
            std::fs::create_dir_all(f.parent().unwrap()).expect("mkdir");
            std::fs::write(f, b"stale").expect("write");
        }
        ForwardState::start(init, dir, true, None).expect("start");
        for f in &stale {
            assert!(!f.exists(), "{} survived a fresh start", f.display());
        }
        assert!(dir.join("frames/f000").is_dir(), "frame 0 checkpointed");
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A trimmed checkpoint keeps everything the search reads of an old
    /// frame - width, shape, keys, ids, cells, the wins - and refuses to
    /// load its rows.
    #[test]
    fn a_trimmed_frame_keeps_its_keys_cells_and_wins() {
        let engine = std::sync::Mutex::new(RefEngine::new().expect("ref engine"));
        let init = vec![Block::keyed(engine.lock().unwrap().initial().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-trim-test");
        let _ = std::fs::remove_dir_all(dir);
        let mut st = ForwardState::start(init, dir, true, None).expect("start");
        st.extend(&engine, dir, 2, None).expect("two frames");
        type Seen = (u32, u64, Vec<(u32, (u64, u64))>, Vec<u64>, Vec<(u64, (u64, u64), u32)>);
        let seen = |f: &crate::search::checkpoint::FrameFile| -> Seen {
            (f.width(), f.shape_hash(), f.cell_keys().collect(), f.ids(), f.wins())
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

    /// A tree's level -1 record round-trips, reads the campaign's bare
    /// number as a legacy filter, and is absent (unknown) on an old tree.
    #[test]
    fn a_trees_level_minus_one_record_round_trips() {
        let dir = std::env::temp_dir().join(format!("celeste-tree-filter-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(TreeFilter::read(&dir).expect("read"), None, "no record: unknown");
        for f in [TreeFilter::Off, TreeFilter::MinusOne { m: MinusOne { h: 186, speed: 5 }, table: 0x0123_4567_89ab_cdef }, TreeFilter::Legacy(111)] {
            f.write(&dir).expect("write");
            assert_eq!(TreeFilter::read(&dir).expect("read"), Some(f));
        }
        std::fs::write(dir.join("level_minus_one.txt"), "116\n").expect("write");
        assert_eq!(TreeFilter::read(&dir).expect("read"), Some(TreeFilter::Legacy(116)));
        std::fs::write(dir.join("level_minus_one.txt"), "h 3 speed\n").expect("write");
        assert!(TreeFilter::read(&dir).is_err(), "a garbled record is an error");
        let _ = std::fs::remove_dir_all(&dir);
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
            let mut st = ForwardState::start(init, dir, true, None).expect("start");
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
            let visited_before = st.visited_len();
            st.extend(&e, dir, 6, None).expect("extend resumed");
            assert!(st.visited_len() >= visited_before);
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
            let next = crate::storage::wave::one_frame(engine, vec![Block::from_rt2(row.clone_block())], 26).expect("frame").next;
            next.iter().flat_map(|b| b.keys().iter().copied().zip(b.positions().expect("cells"))).collect()
        };
        let (k, r) = (successors(&kernels), successors(&reference));
        let at: std::collections::BTreeSet<_> = r.iter().map(|(_, c)| crate::search::pos_graph::cell_xy(*c)).collect();
        assert_eq!(at, [Some((16, 104))].into_iter().collect(), "the player stands at its spawn");
        assert!(r.len() > 1, "the buttons make more than one successor");
        assert_eq!(k.difference(&r).count() + r.difference(&k).count(), 0, "kernels {} successors, reference {}: {} only in the reference", k.len(), r.len(), r.difference(&k).count());
    }

    /// A REGION KERNEL CHECKS ITS BOUNDS: a lane whose speed or remainder lies
    /// outside the ranges its kernel was specialized on DECLINES (the
    /// restrictions' own error, `Op::Restrict`), and the same lane inside them
    /// runs. Before the restriction, the lowering's interval fold seeded the
    /// input cells with those ranges and folded the check away: such a lane
    /// was computed silently wrong.
    #[test]
    fn a_lane_outside_its_kernels_bounds_declines() {
        use crate::abstraction::{set_level, Level};
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        let level = Level::parse("r0sxh").expect("level");
        set_level(level);
        let grid = crate::trace::kernel::region_grid().expect("the default region grid");
        let _kernels = crate::compiled::FrameEngine::new_for_start_room().expect("kernels");
        // The first frame with a player, a few frames in: standing, at rest.
        let mut reference = RefEngine::new().expect("ref engine");
        let mut row = reference.initial().expect("initial state");
        let ids = crate::compiled::ids();
        let mut after = 0;
        while after < 3 {
            row = reference.step_one(&row, 0).expect("frame").into_rt2();
            if !row.player_objects(ids).is_empty() {
                after += 1;
            }
        }
        widen_rt2_to(&mut row, level);
        let player = row.player_objects(ids)[0];
        let cell_of = |rt2: &Rt2, f: u32, axis: u32| -> usize {
            let pc = rt2.obj_field_cell(player, f).expect("the player's table field");
            let Col::U(AV::Ptr(t)) = rt2.cols[pc as usize] else { panic!("the player's field is not a table") };
            rt2.obj_field_cell(t, axis).expect("its axis") as usize
        };
        // `row` with one field set; a number column or an interval one, as it was.
        let with = |f: u32, axis: u32, v: P8| -> Rt2 {
            let mut r = row.clone_block();
            let c = cell_of(&r, f, axis);
            r.cols[c] = match r.cols[c] {
                Col::U(AV::Ival(..)) | Col::I(_) => Col::U(AV::Ival(v, v)),
                _ => Col::U(AV::Num(v)),
            };
            r
        };
        let runs = |r: &Rt2| -> bool {
            let cells = crate::search::pos_graph::block_cells(r).expect("cells");
            assert!(grid.of_cell(cells[0]).is_some(), "the lane has a region");
            let visited = crate::storage::visited::VisitedSet::new(*crate::storage::geometry());
            let claims = crate::storage::unit::Claims::default();
            let mut sink = crate::storage::unit::UnitSink::new(&visited, &claims, Filters::default(), 1, 0, false, false, None, None);
            sink.begin(0, 0, 0, &[0], None, None, false);
            crate::compiled::dispatch::run_chunk_kernel(r, &cells, &[0], &mut sink)
        };
        let px = |n: i16| P8::from_i16(n);
        let half = P8::from_parts(0, 0x8000);
        assert!(runs(&row), "the lane as played runs");
        assert!(runs(&with(ids.f_spd, ids.f_x, px(grid.speed as i16))), "speed at the bound runs");
        assert!(runs(&with(ids.f_rem, ids.f_x, half - P8::from_raw(1))), "the remainder at its upper bound runs");
        assert!(!runs(&with(ids.f_spd, ids.f_x, px(grid.speed as i16) + half)), "speed.x past the bound declines");
        assert!(!runs(&with(ids.f_spd, ids.f_y, -(px(grid.speed as i16) + half))), "speed.y past the bound declines");
        assert!(!runs(&with(ids.f_rem, ids.f_x, half)), "a remainder of 0.5 declines");
    }

    /// A PLATFORMS-UNKNOWN KERNEL CHECKS ITS PLATFORMS: a lane whose platform
    /// `x` reaches past the path, whose `rem.x` lies outside the literal the
    /// kernel reads in its place, or whose `spd.x` lies outside its range over
    /// the worlds DECLINES. Before `x` was a restriction it was a seeded range
    /// (`Symbolic::ranges`, gone), checked nowhere, and `rem.x` was not read:
    /// such a lane was computed silently wrong.
    #[test]
    fn a_lane_outside_its_platform_bounds_declines() {
        use crate::abstraction::{set_level, Level};
        std::env::set_var("CELESTE_START_ROOM", "2,1");
        let level = Level::parse("r0sxhnp").expect("level");
        set_level(level);
        let _kernels = crate::compiled::FrameEngine::new_for_start_room().expect("kernels");
        let mut reference = RefEngine::new().expect("ref engine");
        let mut row = reference.initial().expect("initial state");
        let ids = crate::compiled::ids();
        let mut after = 0;
        while after < 3 {
            row = reference.step_one(&row, 0).expect("frame").into_rt2();
            if !row.player_objects(ids).is_empty() {
                after += 1;
            }
        }
        widen_rt2_to(&mut row, level);
        let platform = *row.objects_of_type(ids, ids.g_platform).first().expect("a moving platform");
        let sub = |rt2: &Rt2, f: u32, axis: u32| -> usize {
            let pc = rt2.obj_field_cell(platform, f).expect("the platform's table field");
            let Col::U(AV::Ptr(t)) = rt2.cols[pc as usize] else { panic!("the platform's field is not a table") };
            rt2.obj_field_cell(t, axis).expect("its axis") as usize
        };
        let x = row.obj_field_cell(platform, ids.f_x).expect("the platform's x") as usize;
        let (rem, spd) = (sub(&row, ids.f_rem, ids.f_x), sub(&row, ids.f_spd, ids.f_x));
        // `row` with one cell set; an interval column stays one.
        let with = |c: usize, lo: P8, hi: P8| -> Rt2 {
            let mut r = row.clone_block();
            r.cols[c] = match r.cols[c] {
                Col::U(AV::Ival(..)) | Col::I(_) => Col::U(AV::Ival(lo, hi)),
                _ => {
                    assert_eq!(lo, hi, "a number column holds a point");
                    Col::U(AV::Num(lo))
                }
            };
            r
        };
        let runs = |r: &Rt2| -> bool {
            let cells = crate::search::pos_graph::block_cells(r).expect("cells");
            let visited = crate::storage::visited::VisitedSet::new(*crate::storage::geometry());
            let claims = crate::storage::unit::Claims::default();
            let mut sink = crate::storage::unit::UnitSink::new(&visited, &claims, Filters::default(), 1, 0, false, false, None, None);
            sink.begin(0, 0, 0, &[0], None, None, false);
            crate::compiled::dispatch::run_chunk_kernel(r, &cells, &[0], &mut sink)
        };
        let px = |n: i16| P8::from_i16(n);
        let (path_lo, path_hi) = (px(celeste_engine::runtime2::PLATFORM_PATH.0), px(celeste_engine::runtime2::PLATFORM_PATH.1));
        let half = P8::from_parts(0, 0x8000);
        let Col::U(AV::Num(speed)) = row.cols[spd] else { panic!("the platform's spd.x is not a number") };
        assert!(runs(&row), "the lane as played runs");
        assert!(runs(&with(x, path_lo, path_hi)), "x over the whole path runs");
        assert!(!runs(&with(x, path_lo, path_hi + P8::from_raw(1))), "x past the path declines");
        assert!(!runs(&with(x, path_lo - px(1), path_hi)), "x before the path declines");
        assert!(!runs(&with(rem, -half, half)), "a remainder reaching 0.5 declines");
        assert!(!runs(&with(spd, speed + px(1), speed + px(1))), "spd.x past its worlds' range declines");
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
        let next = crate::storage::wave::one_frame(&kernels, vec![Block::from_rt2(parent)], 127).expect("frame").next;
        let mut made: std::collections::BTreeSet<(u64, (u64, u64), u32)> = Default::default();
        for b in &next {
            let stored = b.keys().to_vec();
            let canonical = b.rt2().clone_block().row_keys_canonical(crate::compiled::ids());
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
