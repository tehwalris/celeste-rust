//! The block, the frame-step interface (`FrameStep`, emitting into the
//! storage's `UnitSink`) and the HULL FORWARD (`ForwardState`, extended
//! frame by frame by `storage::wave::run_wave`, checkpointed, resumable):
//! per state a remainder HULL (`search::hull`), pushed along the recorded
//! edges; a state is expanded by the kernels once, when first reached, and
//! its edges pushed again whenever its hull grows. The forward holds its
//! frontier as KEYS (`storage::wave::Frontier`); a block - rows - exists
//! only inside a unit of a wave (decoded right before the kernels) and in
//! the readers. A block is OPAQUE to the loop except for its KEYS and
//! POSITIONS.

use anyhow::{Context, Result};

use celeste_core::arith::{Arithmetic, EngineNum};
use celeste_engine::runtime2::{Rt2, P8};

use crate::search::rem::Rem;
use crate::storage::wave::Frontier;

/// Wave phase timers (`--phases`, which the wave's sinks carry): TSC ticks
/// summed over workers, taken per frame (`take`) into `FrameStats::phase_s`
/// and printed (`print`). Off, `start` is a branch.
pub mod phases {
    use std::sync::atomic::{AtomicU64, Ordering};
    /// The phases. `UNIT` (a kernel unit's `engine.run`) holds `SETUP`,
    /// `PACK`, `KERNEL` and `EMIT`; the rest are disjoint.
    pub const NAMES: [&str; N] = [
        "pack",
        "kernel",
        "emit",
        "unit end (chunks)",
        "regions (pushes merged, segments)",
        "frame end (keys, next layer, wins decode, edge files)",
        "slice.setup",
        "unit (all of engine.run)",
        "unit decode (rows from keys, not expanded)",
        "re-push units",
        "unit out blocks (zstd)",
        "slice gather (transfer ids, jobs)",
    ];
    pub const N: usize = 12;
    pub const PACK: usize = 0;
    pub const KERNEL: usize = 1;
    pub const EMIT: usize = 2;
    pub const END_UNIT: usize = 3;
    pub const REGIONS: usize = 4;
    pub const LAYER: usize = 5;
    pub const SETUP: usize = 6;
    pub const UNIT: usize = 7;
    pub const DECODE: usize = 8;
    pub const REPUSH: usize = 9;
    pub const OUT_BLOCKS: usize = 10;
    pub const GATHER: usize = 11;
    static TICKS: [AtomicU64; N] = [const { AtomicU64::new(0) }; N];
    /// A phase's start (`on`: `--phases`; off, 0 and every `add` a no-op).
    #[inline]
    pub fn start(on: bool) -> u64 {
        if on {
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
    /// The TSC's ticks per second, measured once against the wall clock.
    fn hz() -> f64 {
        static HZ: std::sync::OnceLock<f64> = std::sync::OnceLock::new();
        *HZ.get_or_init(|| {
            let t0 = std::time::Instant::now();
            let c0 = unsafe { core::arch::x86_64::_rdtsc() };
            std::thread::sleep(std::time::Duration::from_millis(50));
            (unsafe { core::arch::x86_64::_rdtsc() } - c0) as f64 / t0.elapsed().as_secs_f64()
        })
    }
    /// The phases' seconds since the last `take` (summed over workers),
    /// the counters reset.
    pub fn take() -> [f64; N] {
        let hz = hz();
        std::array::from_fn(|p| TICKS[p].swap(0, Ordering::Relaxed) as f64 / hz)
    }
    /// `secs` (a `take`) against the budget `workers` x `wall`.
    pub fn print(secs: &[f64; N], wall: std::time::Duration, workers: usize) {
        let budget = wall.as_secs_f64() * workers as f64;
        let line: Vec<String> = NAMES.iter().zip(secs).map(|(n, &t)| format!("{n} {t:.1}s ({:.0}%)", 100.0 * t / budget)).collect();
        eprintln!("[phases] worker-seconds of {:.1} ({} x {:.2} s): {}", budget, workers, wall.as_secs_f64(), line.join(", "));
    }
}

/// A columnar batch of lanes of one shape: the kernels' own `Rt2`, with its
/// key column where it was decoded from keys. Only the per-lane key and
/// position are exposed.
pub struct Block<N: EngineNum = P8> {
    rt2: Rt2<N>,
}

impl<N: EngineNum> Block<N> {
    /// Wrap an engine block: decoded rows with their key column, or rows
    /// with none (keys are a key space's: `exact::KeySpace`).
    pub fn from_rt2(rt2: Rt2<N>) -> Self {
        assert!(
            rt2.row_keys.is_empty() || rt2.row_keys.len() == rt2.width,
            "block with a partial key column ({} keys for {} lanes)",
            rt2.row_keys.len(),
            rt2.width
        );
        Block { rt2 }
    }

    /// A reference-engine block, CANONICAL (ids, shape hash), without keys:
    /// a tree keys its rows (`UnitSink::row_key`), the concrete search
    /// compares them whole (`exact::exact_rows`).
    pub fn canonical(mut rt2: Rt2<N>) -> Result<Self> {
        rt2.canonical();
        Ok(Block { rt2 })
    }

    pub fn into_rt2(self) -> Rt2<N> {
        self.rt2
    }

    pub fn rt2(&self) -> &Rt2<N> {
        &self.rt2
    }

    /// Decoded rows' EXACT keys per lane (their tree's key space).
    pub fn keys(&self) -> &[(u64, u64)] {
        assert_eq!(self.rt2.row_keys.len(), self.rt2.width, "a block without its key column");
        &self.rt2.row_keys
    }

    /// The player-position cell per lane.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::block_cells(&self.rt2)
    }

    /// Per lane: does it sit on the room's win target (`wins_of`)?
    pub fn wins(&self, s: &crate::settings::Settings) -> Result<Vec<bool>> {
        wins_of(&self.rt2, s)
    }

    /// Per lane: has it left the start room (`exits_of`, a superset of wins)?
    pub fn exits(&self, s: &crate::settings::Settings) -> Result<Vec<bool>> {
        exits_of(&self.rt2, s)
    }

    /// Number of lanes in this block.
    pub fn lanes(&self) -> usize {
        self.rt2.width
    }
}

/// The summit (6,3) has no exit: it ends when the player touches the
/// original cart's FLAG (tile 118 at (fx, fy)): fx-6 <= x <= fx+6, fy-7 <= y <= fy+4.
pub const SUMMIT_LEVEL: i16 = 30;

/// The winning positions (x_lo, x_hi, y_lo, y_hi, inclusive): `--win-at` or
/// the summit's flag; a lane wins where it MEETS it. None: the room exit.
pub fn win_rect(s: &crate::settings::Settings) -> Option<(i16, i16, i16, i16)> {
    if let Some((xl, xh, yl, yh)) = s.win_at {
        // Said once per process, where a win is first judged.
        static SAID: std::sync::Once = std::sync::Once::new();
        SAID.call_once(|| {
            println!(
                "SYNTHETIC WIN: a lane counts as won at player x {xl}..={xh}, y {yl}..={yh}, NOT at the \
                 room exit - this is a test configuration and is in the fingerprint"
            )
        });
        return s.win_at;
    }
    summit_flag(s.room)
}

/// The summit's win rectangle around its flag, if `room` is the summit (read
/// off the cart once per room: a cache keyed on the room).
fn summit_flag((rx, ry): (i16, i16)) -> Option<(i16, i16, i16, i16)> {
    type Cache = std::sync::Mutex<std::collections::HashMap<(i16, i16), Option<(i16, i16, i16, i16)>>>;
    static RECTS: std::sync::OnceLock<Cache> = std::sync::OnceLock::new();
    if crate::game_runner::level_index(rx, ry) != SUMMIT_LEVEL {
        return None;
    }
    *RECTS.get_or_init(Default::default).lock().unwrap().entry((rx, ry)).or_insert_with(|| {
        let cart = celeste_core::cart_data::CartData::load(std::path::Path::new("cart"))
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
pub fn exits_of<N: EngineNum>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    if let Some((txl, txh, tyl, tyh)) = win_rect(s) {
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
    room_is(rt2, s.win_room())
}

/// Per lane: is the `room` global (`room.x`, `room.y`) = `(wx, wy)`?
fn room_is<N: EngineNum>(rt2: &Rt2<N>, (wx, wy): (i16, i16)) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow::anyhow!("wins: no `room` global"))?;
    let is = |f: u32, name: &str, want: i16| -> Result<Vec<bool>> {
        let want = <N::A as Arithmetic>::from_i16(want);
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
pub fn wins_of<N: EngineNum>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let mut wins = reaches_win(rt2, s)?;
    if s.hundred {
        // 100%: the exit counts only with this room's berry taken.
        for (e, b) in wins.iter_mut().zip(got_fruit(rt2, s.room)?) {
            *e &= b;
        }
    }
    Ok(wins)
}

/// Per lane: the win target met, the berry aside (`wins_of` without its
/// 100% condition).
fn reaches_win<N: EngineNum>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::AV;
    let exits = exits_of(rt2, s)?;
    if win_rect(s).is_some() || !orb_required(s) {
        return Ok(exits);
    }
    let (ids, lanes) = (crate::compiled::ids(), rt2.width);
    let has_orb: Vec<bool> = {
        let c = rt2.globals[ids.g_max_djump as usize];
        anyhow::ensure!(c != celeste_engine::runtime2::NONE, "wins: no `max_djump` global");
        let two = <N::A as Arithmetic>::from_i16(2);
        (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(two)).collect()
    };
    Ok(exits.iter().zip(&has_orb).map(|(e, o)| *e && *o).collect())
}

/// Per lane: has start room `(x, y)`'s berry been taken (`got_fruit[1 +
/// level_index()]`, an array the cart pads with nils up to that index)? An
/// unknown value (a coarse level) counts as taken: a win there is only an
/// over-approximation, and the exact level decides.
pub fn got_fruit<N: EngineNum>(rt2: &Rt2<N>, (x, y): (i16, i16)) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let lanes = rt2.width;
    let g = celeste_names::gen::global_id("got_fruit").expect("`got_fruit` is a global name");
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
pub fn orb_deadline_skip<N: EngineNum>(rt2: &Rt2<N>, frame: u32, horizon: u32) -> Vec<bool> {
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
    let zero = <N::A as Arithmetic>::from_i16(0);
    (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(zero)).collect()
}

/// 100%, per lane: can no successor take the room's berry any more? Not
/// taken (`got_fruit` false, not unknown) and nothing left that holds or
/// drops it - the fruit, the fly fruit (gone once it flew off), the key and
/// its chest, a fake wall. Exact: every state the row stands for is lost.
pub fn berry_lost<N: EngineNum>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let lanes = rt2.width;
    let ids = crate::compiled::ids();
    let sources = [ids.g_fruit, ids.g_fly_fruit, ids.g_key, ids.g_chest, ids.g_fake_wall];
    if !s.hundred || sources.iter().any(|&g| !rt2.objects_of_type(ids, g).is_empty()) {
        return Ok(vec![false; lanes]);
    }
    Ok(got_fruit(rt2, s.room)?.into_iter().map(|taken| !taken).collect())
}

/// Per lane of a frame's rows: NOT expanded further. An exited row (no kernels
/// for the next room; a win or a `--win-at` exit), a 100% row whose berry is
/// lost (`berry_lost`), and in the orb room a row whose chest is still closed
/// too late for the ceiling (`orb_deadline_skip`). THE one place the decision
/// is made: every wave's unit decides it on its decoded rows
/// (`wave::run_unit`), whether its frontier is the forward's own or a
/// resume's - so a resumed run expands exactly the rows an uninterrupted
/// one does. `frame`: the rows' layer; `ceiling`: the tree's level -1
/// horizon, the orb deadline's. (A state not expanded at its layer is never
/// re-pushed: it has no edges. One expanded is re-pushed whatever the orb's
/// deadline says at the later frame: sound, only less pruned.)
pub fn not_expanded<N: EngineNum>(b: &Block<N>, frame: u32, ceiling: Option<u32>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let mut skip = b.exits(s)?;
    // Under a win rectangle (`--win-at`, the summit's flag) a row that LEFT
    // the room is no win, but it is not this room's search either (its
    // shape has no kernel: a coverage gap at room (0,3) f93).
    if win_rect(s).is_some() {
        for (w, here) in skip.iter_mut().zip(room_is(b.rt2(), s.room)?) {
            *w |= !here;
        }
    }
    for (w, lost) in skip.iter_mut().zip(berry_lost(b.rt2(), s)?) {
        *w |= lost;
    }
    if let (true, Some(h)) = (orb_required(s), ceiling) {
        for (w, late) in skip.iter_mut().zip(orb_deadline_skip(b.rt2(), frame, h)) {
            *w |= late;
        }
    }
    Ok(skip)
}

/// Is the start room the orb room (5,2)? A win there also needs the orb
/// (`max_djump == 2`): later levels are played with the second dash.
pub fn orb_required(s: &crate::settings::Settings) -> bool {
    let (x, y) = s.room;
    crate::game_runner::level_index(x, y) == crate::game_runner::ORB_LEVEL && !s.gemskip
}

/// THE LEVEL -1 FILTER at a horizon (`--level-minus-one H,S`, H in search
/// steps): drop a queue when the table proves its cell cannot exit
/// by `h` (sound: inductive ranges, clipped successors and deaths
/// accounted). A tree records the filter its frames were built under
/// (`TreeFilter`); a forward runs under its tree's, not the run's flag.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MinusOne {
    pub h: u32,
    /// The table's speed bound S, px per frame.
    pub speed: i32,
}

impl MinusOne {
    /// The start room's table at this speed bound, under settings `s`
    /// (`minus_one_table`).
    pub fn table(&self, s: &crate::settings::Settings) -> &'static crate::trace::level_minus_one::CostToGo {
        minus_one_table(self.speed, s).unwrap_or_else(|e| panic!("building the level -1 table: {e:#}"))
    }

    /// The same, an error where the table cannot be built (the search
    /// records the refusal before anything else needs the table).
    pub fn try_table(&self, s: &crate::settings::Settings) -> Result<&'static crate::trace::level_minus_one::CostToGo> {
        minus_one_table(self.speed, s)
    }
}

/// The level -1 table at speed bound `speed` under settings `s`, built (or
/// read from its cache file) once per process and KEY (what the table is a
/// function of: `Settings::cart_key`, the pinned file, the speed), on a
/// thread with the tracer's stack. Leaked: a process holds one or two.
fn minus_one_table(speed: i32, s: &crate::settings::Settings) -> Result<&'static crate::trace::level_minus_one::CostToGo> {
    type Key = (String, Option<std::path::PathBuf>, i32);
    static TABLES: std::sync::Mutex<Vec<(Key, &'static crate::trace::level_minus_one::CostToGo)>> = std::sync::Mutex::new(Vec::new());
    let key: Key = (s.cart_key(), s.l1_table.clone(), speed);
    let mut tables = TABLES.lock().unwrap();
    if let Some((_, t)) = tables.iter().find(|(k, _)| *k == key) {
        return Ok(t);
    }
    // The table measures the EXIT: unsound for a position win.
    anyhow::ensure!(win_rect(s).is_none(), "--level-minus-one: the table measures the room exit; this room's win is a position");
    let threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let t = std::time::Instant::now();
    let owned = s.clone();
    let table = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || crate::trace::level_minus_one::cost_to_go(std::path::Path::new("."), speed, threads, &owned))
        .expect("spawn the level -1 builder")
        .join()
        .map_err(|p| anyhow::anyhow!("the level -1 builder panicked: {}", p.downcast_ref::<String>().map(String::as_str).or_else(|| p.downcast_ref::<&str>().copied()).unwrap_or("?")))??;
    eprintln!("[level -1] table ready in {:.1} s (S = {speed}): the start state's d = {}", t.elapsed().as_secs_f64(), table.start_d);
    let table: &'static _ = Box::leak(Box::new(table));
    tables.push((key, table));
    Ok(table)
}

/// What a forward's flush filters by.
pub struct Filters<'a, N: EngineNum = P8> {
    /// The coarser level's arc marks (the objects ladder).
    pub marks: Option<&'a MarkFilter<'a, N>>,
    /// Level -1.
    pub minus_one: Option<MinusOne>,
}

impl<N: EngineNum> Clone for Filters<'_, N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: EngineNum> Copy for Filters<'_, N> {}

impl<N: EngineNum> Default for Filters<'_, N> {
    fn default() -> Self {
        Filters { marks: None, minus_one: None }
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
/// The projection is keyed in the COARSE tree's key space (the marks'): a
/// code it lacks is a state the coarse tree never reached.
pub struct MarkFilter<'a, N: EngineNum = P8> {
    marked: &'a Visited<N>,
    coarser: crate::abstraction::Level,
    /// Search steps per frame (`Settings::steps_per_frame`).
    spf: u32,
}

impl<'a, N: EngineNum> MarkFilter<'a, N> {
    pub fn new(marked: &'a Visited<N>, coarser: crate::abstraction::Level, spf: u32) -> Self {
        MarkFilter { marked, coarser, spf }
    }

    /// Per lane of `rt2` (rows of frame `frame`): admitted? Under the split
    /// frame only at frame boundaries (even steps): a mid-frame row's
    /// projection does not key as the coarser tree's mid-frame rows
    /// (`widen::widen_near_floors` keeps a near floor's computed
    /// `collideable` in the player's probe window there, `Rt2::widen_to`
    /// does not), so filtering it would drop marked paths - room (6,1)
    /// nodiag `r0sxhn,r0sxh` REFUTED 93, which the community TAS reaches.
    /// The next boundary filters its successors.
    pub fn allowed(&self, rt2: &Rt2<N>, frame: u32) -> Result<Vec<bool>> {
        if frame % self.spf != 0 {
            return Ok(vec![true; rt2.width]);
        }
        Ok(self.deadlines(rt2)?.into_iter().map(|d| d.is_some_and(|d| u32::from(d) >= frame)).collect())
    }

    /// Per lane of `rt2`, its projection's deadline at the coarser level
    /// (`None`: not marked there).
    pub fn deadlines(&self, rt2: &Rt2<N>) -> Result<Vec<Option<u16>>> {
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser, self.marked.keys())?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| k.and_then(|k| self.marked.deadline(shape, k, c))).collect())
    }

    /// The coarser level.
    pub fn coarser(&self) -> crate::abstraction::Level {
        self.coarser
    }
}

/// Each lane's `(shape, key, cell)` at the `coarser` level (its node there)
/// in the key space `keys` (`None`: a code the space lacks - no node).
pub fn widened_keys<N: EngineNum>(
    block: &Block<N>,
    coarser: crate::abstraction::Level,
    keys: &celeste_engine::exact::KeySpace<N>,
) -> Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    widened_keys_rt2(&block.rt2, coarser, keys)
}

/// `rt2` projected IN PLACE onto `level`, as its kernels would store it.
pub fn widen_rt2_to<N: EngineNum>(rt2: &mut Rt2<N>, level: crate::abstraction::Level) {
    rt2.widen_to(crate::compiled::ids(), level);
}

/// `(shape, keys, cells)` of the widened rows (the widened block's shape),
/// keyed in `keys`.
pub fn widened_keys_rt2<N: EngineNum>(
    rt2: &Rt2<N>,
    coarser: crate::abstraction::Level,
    keys: &celeste_engine::exact::KeySpace<N>,
) -> Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    let mut w = rt2.clone_block();
    widen_rt2_to(&mut w, coarser);
    w.canonical();
    let keys = keys.lookup_keys(&w, crate::compiled::ids());
    let cells = crate::search::pos_graph::block_cells(&w)?;
    anyhow::ensure!(
        keys.len() == rt2.width && cells.len() == rt2.width,
        "the widening changed the lane count"
    );
    Ok((w.shape_hash, keys, cells))
}

/// The interface between the loop and the engines (kernels or reference):
/// lanes in, every successor row out, widened for the level and keyed.
pub trait FrameStep<N: Rem = P8>: Sync {
    /// One frame of `lanes` of `block` into `sink`; called concurrently on
    /// disjoint ranges (scratch in the sink or behind a lock).
    fn run(&self, block: &Block<N>, cell_in: &[u32], lanes: &[usize], sink: &mut crate::storage::unit::UnitSink<N>) -> Result<()>;

    /// Build what the engine builds on first use (the kernels) before a
    /// wave's clock starts: built inside the wave, the first frame's workers
    /// all waited on the build's lock (room (6,2) 100% f57: 61 of 152
    /// worker-s, so a one-frame bench timed the build).
    fn warm(&self) {}

    /// The level its frames widen to (its rows' key view, the platform
    /// worlds' range).
    fn level(&self) -> crate::abstraction::Level;

    /// The settings it runs under: the wave's (threads, the diagnostics,
    /// the win and the category its rows are judged by).
    fn settings(&self) -> std::sync::Arc<crate::settings::Settings>;
}

/// The frame end's steps (`FrameStats::t_end`): provisional keys made
/// final, the routes sealed, the ladder's filter, the visited set's merge,
/// the next layer and the grown states built, their wins decoded, the edge
/// file and the out file written.
pub const END_STEPS: [&str; 8] = ["keys", "seal", "filter", "absorb", "layer", "wins", "edge file", "out file"];

/// One forward frame's timings and sizes, for the `[fwd]` log line.
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    /// The frontier's new entries and states (the kernels'), and its old
    /// states (re-pushed).
    pub entries_in: usize,
    pub lanes_in: usize,
    pub old_in: usize,
    /// Emissions past level -1: each a record (a lookup and an edge).
    pub lanes_raw: usize,
    /// The old states' recorded edges read, and every push merged (the
    /// units' and the re-push's); the states pushed into.
    pub repush_edges: u64,
    pub pushes: u64,
    pub touched: u64,
    /// New states, and old states whose hull grew.
    pub lanes_kept: usize,
    pub grown: usize,
    /// The next layer's entries and states.
    pub entries_out: usize,
    pub lanes_out: usize,
    /// The units' wall time (every slice's) and its barrier idle fraction.
    pub t_wave: std::time::Duration,
    pub wave_idle: f64,
    /// The region phase: its jobs' time summed over the workers (they run
    /// inside the wave, overlapping the units).
    pub t_regions: std::time::Duration,
    /// The frame's end (keys, the next layer, its wins decoded, the edge
    /// file), wall, and its steps' (`END_STEPS`).
    pub t_layer: std::time::Duration,
    pub t_end: [std::time::Duration; END_STEPS.len()],
    /// Per step its counts (user instructions and cycles, kernel cycles,
    /// page faults: `storage::bench::step_counts`; zeros outside a bench).
    pub end_counts: [[u64; 4]; END_STEPS.len()],
    /// `--phases`: the wave's phases, seconds summed over the workers
    /// (`phases::take`; zeros without).
    pub phase_s: [f64; phases::N],
    pub units: usize,
    /// The slices, and the routes (target regions) with new states or edges.
    pub slices: usize,
    pub routes: u64,
    /// Edges recorded, their bundles, and the frame's edge file bytes and
    /// out file bytes.
    pub edge_records: u64,
    pub edge_bundles: u64,
    pub edge_bytes: u64,
    pub out_bytes: u64,
    /// The input frontier's bytes (its keys and masks).
    pub frontier_bytes: usize,
    /// Bytes allocated in the visited set.
    pub visited_bytes: usize,
    /// Anonymous resident set at the frame's start, after the wave, at the end.
    pub rss_start: f64,
    pub rss_wave: f64,
    pub rss_end: f64,
    /// File-backed resident pages at the end of the frame.
    pub rss_file: f64,
    /// The key space after the frame: its widest shape's key bits, its codes,
    /// and the codes the frame added (`exact::KeySpace`).
    pub key_bits: u32,
    pub key_codes: usize,
    pub new_codes: usize,
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
    /// horizons up to `m.h` (and cannot be raised past it: `grow_tree`).
    MinusOne { m: MinusOne, table: u64 },
}

impl TreeFilter {
    fn of(m: Option<MinusOne>, s: &crate::settings::Settings) -> TreeFilter {
        match m {
            None => TreeFilter::Off,
            Some(m) => TreeFilter::MinusOne { m, table: m.table(s).fingerprint() },
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
        };
        std::fs::create_dir_all(dir)?;
        let tmp = dir.join("level_minus_one.tmp");
        std::fs::write(&tmp, format!("{s}\n"))?;
        std::fs::rename(&tmp, Self::path(dir))?;
        Ok(())
    }

    /// The filter a forward extending the tree runs under.
    fn minus_one(&self) -> Option<MinusOne> {
        match *self {
            TreeFilter::Off => None,
            TreeFilter::MinusOne { m, .. } => Some(m),
        }
    }

    /// The filter a forward extending the tree runs under, its table checked
    /// against the one the tree recorded: a table built under other
    /// settings (`--override-settings`) or by another binary would filter
    /// the new frames otherwise than the tree's.
    fn checked_minus_one(&self, dir: &std::path::Path, s: &crate::settings::Settings) -> Result<Option<MinusOne>> {
        let m = self.minus_one();
        if let (TreeFilter::MinusOne { table, .. }, Some(m)) = (self, m) {
            let got = m.table(s).fingerprint();
            anyhow::ensure!(
                got == *table,
                "{}: the level -1 table is not the one the tree was built under (fingerprint {got:016x}, the tree's {table:016x}; a fixture pins its table: --l1-table)",
                dir.display()
            );
        }
        Ok(m)
    }

    /// The level -1 filter the tree in `dir` was built under, its table
    /// checked against the recorded fingerprint: what a check of the tree
    /// (`search::known`) must apply, whatever the environment says. Refused
    /// without a record (its table is unknown).
    pub fn of_tree(dir: &std::path::Path, s: &crate::settings::Settings) -> Result<Option<MinusOne>> {
        match TreeFilter::read(dir)? {
            None => anyhow::bail!("{}: no level -1 record (a tree from before records): its filter is unknown", dir.display()),
            Some(TreeFilter::Off) => Ok(None),
            Some(TreeFilter::MinusOne { m, table }) => {
                anyhow::ensure!(
                    m.table(s).fingerprint() == table,
                    "{}: the level -1 table (S = {}) is not the one the tree was built under (fingerprint {:016x}, the tree's {table:016x})",
                    dir.display(),
                    m.speed,
                    m.table(s).fingerprint()
                );
                Ok(Some(m))
            }
        }
    }

    /// The horizon the tree's frames were filtered for (`None`: none).
    fn ceiling(&self) -> Option<u32> {
        match *self {
            TreeFilter::Off => None,
            TreeFilter::MinusOne { m, .. } => Some(m.h),
        }
    }
}

/// The forward's in-memory state, EXTENDED frame by frame (each checkpointed).
pub struct ForwardState<N: Rem = P8> {
    /// The last frame's frontier: its new states (as keys, with their
    /// hulls) and the older states whose hull grew there.
    frontier: Frontier<N>,
    /// Every state reached so far, with its layer and hull (`storage::visited`).
    visited: crate::storage::visited::VisitedSet<N>,
    /// The tree's transfers (`storage::edges::XferTable`).
    xfers: crate::storage::edges::XferTable<N>,
    /// The tree's out-edge files (the re-push reads an old state's edges).
    outs: crate::storage::out::OutStore,
    /// The shapes' skeletons the emissions register (the next layers'),
    /// and the shapes whose skeleton a frame file holds.
    skeletons: crate::storage::decode::Skeletons<N>,
    persisted: rustc_hash::FxHashSet<u64>,
    observer: Option<crate::search::pos_graph::PosObserver>,
    /// The last frame computed (and checkpointed).
    pub frames: u32,
    /// The first frame a lane won, if any so far.
    pub win_frame: Option<u32>,
    /// The filter the tree's frames were built under (`None`: a tree from
    /// before records, which is not extended).
    pub filter: Option<TreeFilter>,
    /// A bench wave took the visited set (`bench_wave` without `copy`).
    spent: bool,
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

/// What a checkpoint wrote, and its steps' walls (`CKPT_STEPS`).
#[derive(Clone, Copy, Default)]
pub struct Checkpointed {
    /// Does a new state win?
    pub won: bool,
    pub frame_bytes: u64,
    pub hull_bytes: u64,
    pub t: [std::time::Duration; CKPT_STEPS.len()],
    /// Per step its counts (`FrameStats::end_counts`).
    pub counts: [[u64; 4]; CKPT_STEPS.len()],
}

/// A checkpoint's steps (`Checkpointed::t`).
pub const CKPT_STEPS: [&str; 4] = ["frame file", "frame check", "hull file", "hull check"];

/// CHECKPOINT a wave: frame `frame`'s file - its new states and wins - and
/// its hull file - its frontier, every state whose hull grew, with its
/// layer and hull - each read back and checked to hold exactly the wave's
/// (`layer::check_frame_file`, `hulls::check`: the complete storage
/// check).
fn checkpoint<N: Rem>(dir: &std::path::Path, frame: u32, wave: &crate::storage::wave::Wave<N>, emitted: &[crate::storage::layer::SkelRecord<N>], s: &crate::settings::Settings) -> Result<Checkpointed> {
    use crate::storage::{hulls, layer};
    let mut c = Checkpointed { won: !wave.wins.is_empty(), ..Default::default() };
    let mut lap = std::time::Instant::now();
    let mut at = crate::storage::bench::step_counts();
    let mut mark = |k: usize| {
        c.t[k] = lap.elapsed();
        (c.counts[k], at) = crate::storage::bench::counts_since(at);
        lap = std::time::Instant::now();
    };
    let path = layer::frame_path(dir, frame);
    let meta = layer::FrameMeta { frame, shapes: &wave.shapes, keys: &wave.keys, emitted };
    let frame_bytes = layer::write_frame(&path, &meta, &wave.next, &wave.wins, s.threads())?;
    mark(0);
    layer::check_frame_file(&path, &wave.next, &wave.wins, s.threads())?;
    mark(1);
    anyhow::ensure!(u16::try_from(frame).is_ok(), "frame {frame}: past a u16 layer");
    let hp = hulls::path(dir, frame);
    let hull_bytes = hulls::write::<N, _>(&hp, frame, &wave.hulls, &wave.grown[..], s.threads())?;
    mark(2);
    hulls::check::<N, _>(&hp, frame, &wave.hulls, &wave.grown[..], s.threads())?;
    mark(3);
    (c.frame_bytes, c.hull_bytes) = (frame_bytes, hull_bytes);
    Ok(c)
}

impl<N: Rem> ForwardState<N> {
    /// Frame 0: `initial` into an empty visited set and checkpointed, each
    /// with the start's hull; the tree is filtered by `minus_one` and
    /// widened at `level` (both recorded, with the settings `s`).
    pub fn start(initial: Vec<Block<N>>, dir: &std::path::Path, record: bool, minus_one: Option<MinusOne>, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Self> {
        // A fresh tree keeps nothing a killed run left (stale edge files).
        for sub in ["frames", "edges", "hulls"] {
            let p = dir.join(sub);
            if p.exists() {
                std::fs::remove_dir_all(&p).with_context(|| p.display().to_string())?;
            }
        }
        let filter = TreeFilter::of(minus_one, s);
        filter.write(dir)?;
        s.write(dir, level)?;
        // The start has no player yet (`arc_dp::start_point`'s premise):
        // `seed` keys it as it is, so a player's remainder would be stored
        // exact where every other state holds it widened, beside its hull.
        let ids = crate::compiled::ids();
        anyhow::ensure!(initial.iter().all(|b| b.rt2().player_objects(ids).is_empty()), "frame 0: an initial row holds a player, whose remainder the start's hull does not describe");
        let mut visited = crate::storage::visited::VisitedSet::<N>::new(crate::storage::GEOMETRY);
        let seeded = crate::storage::wave::seed(&mut visited, &initial, N::start_hull(s)?, s)?;
        let won = checkpoint(dir, 0, &seeded, &[], s)?.won;
        let observer = record.then(crate::search::pos_graph::PosObserver::default);
        // An (empty) graph from the start: every tree with frames has one.
        if let Some(o) = observer.as_ref() {
            save_pos_graph(dir, &o.snapshot())?;
        }
        let edges_dir = dir.join("edges");
        crate::storage::edges::set_done(&edges_dir, 0, 0)?;
        let outs = crate::storage::out::OutStore::open(&edges_dir, 0)?;
        Ok(ForwardState { frontier: seeded.frontier(), visited, xfers: Default::default(), outs, skeletons: Default::default(), persisted: Default::default(), observer, frames: 0, win_frame: won.then_some(0), filter: Some(filter), spent: false })
    }

    /// The forward as its checkpoint tree left it (`None`: no tree); extending
    /// it matches extending the original. Refused at another `level` than
    /// the tree's, or under other settings `s` than its.
    pub fn resume(dir: &std::path::Path, record: bool, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Option<Self>> {
        let Some(mut last) = crate::storage::layer::last_frame(dir) else { return Ok(None) };
        let t = std::time::Instant::now();
        // Trust only frames whose files are complete (at most one lost).
        let edges_dir = dir.join("edges");
        let done = crate::storage::edges::done_frame(&edges_dir).unwrap_or(last);
        if done < last {
            for f in done + 1..=last {
                std::fs::remove_file(crate::storage::layer::frame_path(dir, f))?;
                let _ = std::fs::remove_file(crate::storage::hulls::path(dir, f));
            }
            eprintln!("[resume] frames f{} to f{last} discarded: their files were not complete", done + 1);
            last = done;
        }
        crate::storage::edges::discard_after(&edges_dir, last)?;
        let st = Self::at(dir, last, record, level, s)?;
        eprintln!(
            "[resume] {}: f{last} ({} new states in {} entries, {} old), visited {} states in {} entries, first win {:?}, level -1 {:?}, {:.1} s",
            dir.display(),
            st.frontier.new.states(),
            st.frontier.new.entries(),
            st.frontier.old_states(),
            st.visited.len(),
            st.visited.entries(),
            st.win_frame,
            st.filter,
            t.elapsed().as_secs_f64()
        );
        Ok(Some(st))
    }

    /// The forward as it stood after frame `last` of the tree in `dir`,
    /// READ-ONLY: frames after `last` are ignored, not discarded (`resume`
    /// discards them first). What `extend` runs its next wave from, so
    /// `bench_wave` from here is frame `last + 1` of the real forward.
    /// Refused under other settings or at another `level` than the tree's
    /// (`settings::Settings::check`).
    pub fn at(dir: &std::path::Path, last: u32, record: bool, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Self> {
        s.check(dir, level)?;
        let edges_dir = dir.join("edges");
        let filter = TreeFilter::read(dir)?;
        let (visited, win_frame, frontier, skeletons) = restore_visited::<N>(dir, last, s.room, s.threads())?;
        let persisted = skeletons.shapes().into_iter().collect();
        let xfers = crate::storage::edges::XferTable::<N>::load(&edges_dir)?;
        let outs = crate::storage::out::OutStore::open(&edges_dir, last)?;
        let observer = if record {
            let path = pos_graph_path(dir);
            let graph = crate::search::pos_graph::PosGraph::load(&path)
                .with_context(|| format!("resuming {}: no pos graph at {}", dir.display(), path.display()))?;
            Some(crate::search::pos_graph::PosObserver::from_graph(&graph))
        } else {
            None
        };
        Ok(ForwardState { frontier, visited, xfers, outs, skeletons, persisted, observer, frames: last, win_frame, filter, spent: false })
    }

    /// The visited set's size: every distinct state reached so far.
    pub fn visited_len(&self) -> usize {
        self.visited.len()
    }

    /// The visited set (`bench-frame`, `bench-storage`).
    pub fn visited(&self) -> &crate::storage::visited::VisitedSet<N> {
        &self.visited
    }

    /// The frontier: the last frame's states whose hull grew (`bench-frame`,
    /// `bench-storage`).
    pub fn frontier(&self) -> &Frontier<N> {
        &self.frontier
    }

    /// The out store and the transfer table (`bench-frame`'s `[memory]`).
    pub fn outs_and_xfers(&self) -> (&crate::storage::out::OutStore, &crate::storage::edges::XferTable<N>) {
        (&self.outs, &self.xfers)
    }

    /// The tree's ceiling for the frontier's `not_expanded` (its level -1
    /// horizon; a tree without a record by the run's `s`, as it was built).
    fn ceiling(&self, s: &crate::settings::Settings) -> Option<u32> {
        match self.filter {
            Some(f) => f.ceiling(),
            None => s.minus_one().map(|m| m.h),
        }
    }

    /// ONE WAVE of `extend` - frame `frames + 1`, under the tree's level -1
    /// filter (a capture's replayed emissions are past it already,
    /// `bench-storage`; its re-pushes are not), its pos graph fed when
    /// recording - without the checkpoint (`bench_checkpoint`): the
    /// frontier is read; with `copy` the state is left as it was (the
    /// visited set and the transfer table are copied: another wave may
    /// follow), else they are MOVED in (no copy held beside the wave's: the
    /// state is spent, and refuses another wave). The edges go to
    /// `edges_dir` (a scratch dir; `None`: not recorded). Returns the wave
    /// with the visited set and transfer table it ran against, which hold
    /// its new states and pairs (`bench-frame`, run per rep).
    pub fn bench_wave(
        &mut self,
        engine: &dyn FrameStep<N>,
        dir: &std::path::Path,
        edges_dir: Option<&std::path::Path>,
        copy: bool,
    ) -> Result<(crate::storage::wave::Wave<N>, crate::storage::visited::VisitedSet<N>, crate::storage::edges::XferTable<N>)> {
        anyhow::ensure!(!self.spent, "{}: the forward state at f{} ran its last wave (its visited set was moved)", dir.display(), self.frames);
        // The frame the tree would make: under the table it was built with.
        let minus_one = match self.filter {
            Some(f) => f.checked_minus_one(dir, &engine.settings())?,
            None => anyhow::bail!("{}: no level -1 record (a tree from before records)", dir.display()),
        };
        let (mut visited, mut xfers) = if copy {
            (self.visited.clone(), self.xfers.clone())
        } else {
            self.spent = true;
            (std::mem::replace(&mut self.visited, crate::storage::visited::VisitedSet::new(crate::storage::GEOMETRY)), std::mem::take(&mut self.xfers))
        };
        let cx = crate::storage::wave::WaveCtx {
            visited: &mut visited,
            xfers: &mut xfers,
            pos: self.observer.as_ref(),
            filters: Filters { marks: None, minus_one },
            frame: self.frames + 1,
            edges_dir,
            outs: Some(&self.outs),
            skeletons: &self.skeletons,
            ceiling: self.ceiling(&engine.settings()),
        };
        let front = if copy { self.frontier.clone() } else { std::mem::take(&mut self.frontier) };
        let wave = crate::storage::wave::run_wave(engine, front, cx)?;
        Ok((wave, visited, xfers))
    }

    /// `extend`'s CHECKPOINT of a `bench_wave`'s `wave` (frame `frames +
    /// 1`) into `dir` (a scratch dir): the frame file and the hull file,
    /// each read back and checked, then the pos graph saved. Returns the
    /// checkpoint's steps and the pos graph's wall.
    pub fn bench_checkpoint(&self, wave: &crate::storage::wave::Wave<N>, dir: &std::path::Path, s: &crate::settings::Settings) -> Result<(Checkpointed, std::time::Duration)> {
        let emitted = self.skeletons.records(|h| self.persisted.contains(&h) || wave.next.skeletons.contains_key(&h));
        let c = checkpoint(dir, self.frames + 1, wave, &emitted, s)?;
        let t = std::time::Instant::now();
        if let Some(o) = self.observer.as_ref() {
            save_pos_graph(dir, &o.snapshot())?;
        }
        Ok((c, t.elapsed()))
    }

    /// Compute and checkpoint frames `frames+1 ..= to` under the tree's
    /// level -1 filter and the coarser level's `marks`; only an empty
    /// frontier stops it (the backward seeds from wins at every frame).
    pub fn extend(
        &mut self,
        engine: &dyn FrameStep<N>,
        dir: &std::path::Path,
        to: u32,
        marks: Option<&MarkFilter<N>>,
    ) -> Result<()> {
        let s = &engine.settings();
        let minus_one = match self.filter {
            Some(f) => f.checked_minus_one(dir, s)?,
            None => anyhow::bail!("{}: no level -1 record (a tree from before records): it cannot be extended (delete it)", dir.display()),
        };
        anyhow::ensure!(!self.spent, "{}: the forward state at f{} ran a bench wave without a copy", dir.display(), self.frames);
        let filters = Filters { marks, minus_one };
        let ceiling = self.ceiling(s);
        let mut last_free = None;
        while self.frames < to && !self.frontier.is_empty() {
            let frame = self.frames + 1;
            let t_frame = std::time::Instant::now();
            let edges_dir = dir.join("edges");
            let cx = crate::storage::wave::WaveCtx {
                visited: &mut self.visited,
                xfers: &mut self.xfers,
                pos: self.observer.as_ref(),
                filters,
                frame,
                edges_dir: Some(&edges_dir),
                outs: Some(&self.outs),
                skeletons: &self.skeletons,
                ceiling,
            };
            // The frontier is spent by the wave (dropped before its end).
            let wave = crate::storage::wave::run_wave(engine, std::mem::take(&mut self.frontier), cx)?;
            // Before the frame is trusted (every file, then done.txt).
            let t = std::time::Instant::now();
            // The skeletons first emitted here, of shapes the frame holds
            // no state of: a later re-push may reach one (`FrameHeader::emitted`).
            let emitted = self.skeletons.records(|h| self.persisted.contains(&h) || wave.next.skeletons.contains_key(&h));
            let won = checkpoint(dir, frame, &wave, &emitted, s)?.won;
            self.persisted.extend(self.skeletons.shapes());
            let t_ckpt = t.elapsed();
            let t = std::time::Instant::now();
            if let Some(o) = self.observer.as_ref() {
                save_pos_graph(dir, &o.snapshot())?;
            }
            let t_pos = t.elapsed();
            // The frame's LAST write: every file above is trusted from here.
            crate::storage::edges::set_done(&edges_dir, frame, self.xfers.pairs.len())?;
            self.outs.add_frame(&edges_dir, frame)?;
            log_frame(dir, frame, &wave.stats, t_ckpt, t_pos, t_frame.elapsed(), self.visited_len())?;
            ensure_disk_space(dir, &mut last_free)?;
            self.frames = frame;
            if won && self.win_frame.is_none() {
                self.win_frame = Some(frame);
                eprintln!("[fwd] first win at f{frame}");
            }
            self.frontier = wave.frontier();
        }
        Ok(())
    }

    /// The position graph recorded so far (None when not recording).
    pub fn pos_graph(&self) -> Option<crate::search::pos_graph::PosGraph> {
        self.observer.as_ref().map(|o| o.snapshot())
    }
}

/// A level's tree through step `to` under the run's level -1 filter `want`
/// (`--level-minus-one`): used as it is when it reaches `to` under a
/// filter that serves it (none, or one at a horizon >= `to`); else resumed
/// and extended under its filter, or started under `want`. A tree filtered
/// below `to` is REFUSED: a hull tree's frames depend on its horizon through
/// level -1's dropped pushes (a push not made leaves a hull smaller and a
/// state later or unreached), so it cannot be raised exactly - delete it.
/// `marks`: the coarser level's filter (the caller rebuilds such a tree for
/// another horizon). `engine` is built only when frames are computed, at
/// `level`, the tree's, under the run's settings `s` (the tree's, checked).
#[allow(clippy::too_many_arguments)]
pub fn grow_tree<N: Rem>(
    dir: &std::path::Path,
    to: u32,
    want: Option<MinusOne>,
    marks: Option<&MarkFilter<N>>,
    level: crate::abstraction::Level,
    engine: impl FnOnce() -> Result<Box<dyn FrameStep<N>>>,
    initial: impl FnOnce() -> Result<Vec<Block<N>>>,
    s: &crate::settings::Settings,
) -> Result<ForwardResult> {
    if crate::storage::layer::frame_exists(dir, 0) {
        s.check(dir, level)?;
    }
    if let Some(w) = want {
        anyhow::ensure!(to <= w.h, "--level-minus-one at step {} is below the horizon, step {to}", w.h);
    }
    let complete = tree_first_win_through::<N>(dir, to)?;
    if crate::storage::layer::frame_exists(dir, 0) {
        match TreeFilter::read(dir)? {
            None => anyhow::bail!("{}: the tree has no level -1 record (built before records): delete it", dir.display()),
            Some(TreeFilter::Off) => {
                if want.is_some() {
                    eprintln!("[forward] {}: the tree is not filtered by level -1; it stays unfiltered", dir.display());
                }
            }
            Some(TreeFilter::MinusOne { m, .. }) => {
                if let Some(w) = want {
                    anyhow::ensure!(w.speed == m.speed, "{}: the tree's level -1 table has S = {}, the run asks for S = {}", dir.display(), m.speed, w.speed);
                }
                anyhow::ensure!(
                    to <= m.h,
                    "{}: the tree was built under level -1 at step {}, below this run's step {to}: a hull tree cannot be RAISED (its frames depend on the horizon through the pushes level -1 dropped: hulls smaller, states later or never reached) - delete it, or run at most to step {}",
                    dir.display(),
                    m.h,
                    m.h
                );
            }
        }
    }
    if let Some(first_win) = complete {
        eprintln!("[forward] {}: the tree reaches step {to}", dir.display());
        let pos_graph = crate::search::pos_graph::PosGraph::load(&pos_graph_path(dir)).ok();
        return Ok(ForwardResult { win_frame: first_win, frames: to, pos_graph });
    }
    let engine = engine()?;
    anyhow::ensure!(engine.level() == level, "the forward's engine is at {}, the tree at {level}", engine.level());
    anyhow::ensure!(*engine.settings() == *s, "the forward's engine runs under other settings than the tree's");
    let mut st = match ForwardState::<N>::resume(dir, true, level, s)? {
        Some(st) => st,
        None => ForwardState::start(initial()?, dir, true, want, level, s)?,
    };
    st.extend(engine.as_ref(), dir, to, marks)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// Persist the position graph (an atomic rename), every frame BEFORE its
/// done marker, so it covers every trusted frame; a frame discarded after a
/// crash adds its edges again when it is rerun.
fn save_pos_graph(dir: &std::path::Path, graph: &crate::search::pos_graph::PosGraph) -> Result<()> {
    let tmp = dir.join("posgraph.tmp");
    graph.save(&tmp)?;
    std::fs::rename(&tmp, pos_graph_path(dir))?;
    Ok(())
}

pub fn pos_graph_path(dir: &std::path::Path) -> std::path::PathBuf {
    dir.join("posgraph.bin")
}

/// A fresh forward from `initial` to `max_frames` (the tests' driver).
#[cfg(test)]
pub fn forward_run<N: Rem>(
    engine: &dyn FrameStep<N>,
    initial: Vec<Block<N>>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, record, None, engine.level(), &engine.settings())?;
    st.extend(engine, dir, max_frames, None)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// The free disk the forward keeps, GB (`ensure_disk_space`).
const MIN_FREE_GB: u64 = 20;

/// Stop the forward before the checkpoint disk fills: below
/// `MIN_FREE_GB` plus three times what the last frame
/// wrote, free on `dir`'s filesystem, an error -
/// a full disk otherwise fails a worker mid-write (room (5,1) nodiag,
/// 2026-10-06) and starves everything else on the machine. `last_free`: the
/// free bytes after the previous frame of THIS forward (`None` at its first:
/// a delta across the arc phase between two levels counted everything the
/// other processes wrote meanwhile as the frame's, and stopped room (2,3)'s
/// second level at its first frame, 2026-10-07).
fn ensure_disk_space(dir: &std::path::Path, last_free: &mut Option<u64>) -> Result<()> {
    use std::os::unix::ffi::OsStrExt;
    let min_gb = MIN_FREE_GB;
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
        "only {} GB free on {}'s filesystem, the last frame used {} GB (the floor {min_gb} GB, plus 3 frames): stopping the forward before the disk fills",
        free >> 30,
        dir.display(),
        frame_bytes >> 30
    );
    Ok(())
}

/// The per-frame `[fwd]` line (ms), its record in the tree
/// (`FORWARD_STATS`) and the `fwd.*` metrics totals.
fn log_frame(
    dir: &std::path::Path,
    frame: u32,
    st: &FrameStats,
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) -> Result<()> {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    eprintln!(
        "[fwd] f{frame:03} in {}/{} old {} raw {} re-pushed {} pushes {} touched {} kept {} grown {} out {}/{} visited {} | \
         wave {:.0} (idle {:.0}%, regions {:.0} worker-ms) layer {:.0} ckpt {:.0} pos {:.0} total {:.0} ms | \
         units {} slices {} routes {} edges {} bundles {} ({:.3} B; out {:.3} B) | \
         in {:.2} visited {:.2} GB rss start {:.2} wave {:.2} end {:.2} peak {:.2} GB (anon; file {:.2}) | \
         keys {} bits, {} codes (+{})",
        st.entries_in,
        st.lanes_in,
        st.old_in,
        st.lanes_raw,
        st.repush_edges,
        st.pushes,
        st.touched,
        st.lanes_kept,
        st.grown,
        st.entries_out,
        st.lanes_out,
        visited,
        ms(st.t_wave),
        st.wave_idle * 100.0,
        ms(st.t_regions),
        ms(st.t_layer),
        ms(t_ckpt),
        ms(t_pos),
        ms(t_total),
        st.units,
        st.slices,
        st.routes,
        st.edge_records,
        st.edge_bundles,
        st.edge_bytes as f64 / st.edge_records.max(1) as f64,
        st.out_bytes as f64 / st.edge_records.max(1) as f64,
        st.frontier_bytes as f64 / 1e9,
        st.visited_bytes as f64 / 1e9,
        st.rss_start,
        st.rss_wave,
        st.rss_end,
        crate::metrics::peak_rss_gb(),
        st.rss_file,
        st.key_bits,
        st.key_codes,
        st.new_codes,
    );
    crate::metrics::record("fwd.wave", st.t_wave);
    crate::metrics::record("fwd.regions", st.t_regions);
    crate::metrics::record("fwd.layer", st.t_layer);
    crate::metrics::record("fwd.checkpoint", t_ckpt);
    crate::metrics::record("fwd.posgraph", t_pos);
    crate::metrics::record("fwd.frame", t_total);
    append_forward_stats(dir, &forward_stats_row(frame, st, t_ckpt, t_pos, t_total, visited))
}

/// One frame's row of the tree's forward record (`FORWARD_STATS`): (column,
/// value), times in ms, memory in GB.
pub(crate) fn forward_stats_row(
    frame: u32,
    st: &FrameStats,
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) -> Vec<(&'static str, String)> {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    vec![
        ("frame", frame.to_string()),
        ("entries_in", st.entries_in.to_string()),
        ("lanes_in", st.lanes_in.to_string()),
        ("lanes_raw", st.lanes_raw.to_string()),
        ("lanes_kept", st.lanes_kept.to_string()),
        ("old_in", st.old_in.to_string()),
        ("repush_edges", st.repush_edges.to_string()),
        ("pushes", st.pushes.to_string()),
        ("touched", st.touched.to_string()),
        ("grown", st.grown.to_string()),
        ("entries_out", st.entries_out.to_string()),
        ("lanes_out", st.lanes_out.to_string()),
        ("visited", visited.to_string()),
        ("wave_ms", format!("{:.0}", ms(st.t_wave))),
        ("wave_idle_pct", format!("{:.0}", st.wave_idle * 100.0)),
        ("regions_worker_ms", format!("{:.0}", ms(st.t_regions))),
        ("layer_ms", format!("{:.0}", ms(st.t_layer))),
        ("ckpt_ms", format!("{:.0}", ms(t_ckpt))),
        ("pos_ms", format!("{:.0}", ms(t_pos))),
        ("total_ms", format!("{:.0}", ms(t_total))),
        ("edges", st.edge_records.to_string()),
        ("rss_start_gb", format!("{:.2}", st.rss_start)),
        ("rss_wave_gb", format!("{:.2}", st.rss_wave)),
        ("rss_end_gb", format!("{:.2}", st.rss_end)),
        ("rss_file_gb", format!("{:.2}", st.rss_file)),
        ("peak_gb", format!("{:.2}", crate::metrics::peak_rss_gb())),
    ]
}

/// A tree's per-frame forward record (`<dir>/forward.tsv`): a header row
/// naming the columns, then one row per frame the forward ran, appended as
/// it runs (a resumed or extended tree appends; a frame rerun after a crash
/// appears again, and its LAST row is the frame's). `export-ui` reads it.
pub const FORWARD_STATS: &str = "forward.tsv";

pub(crate) fn append_forward_stats(dir: &std::path::Path, row: &[(&str, String)]) -> Result<()> {
    use std::io::Write;
    let path = dir.join(FORWARD_STATS);
    let header: Vec<&str> = row.iter().map(|(k, _)| *k).collect();
    let fresh = !path.exists();
    if !fresh {
        // A record written with other columns is not appended to silently.
        let mut have = String::new();
        std::io::BufRead::read_line(&mut std::io::BufReader::new(std::fs::File::open(&path)?), &mut have)?;
        anyhow::ensure!(have.trim_end() == header.join("\t"), "{}: its columns ({have:?}) are not this binary's: move it aside", path.display());
    }
    let mut f = std::fs::OpenOptions::new().create(true).append(true).open(&path)?;
    if fresh {
        writeln!(f, "{}", header.join("\t"))?;
    }
    writeln!(f, "{}", row.iter().map(|(_, v)| v.as_str()).collect::<Vec<_>>().join("\t"))?;
    Ok(())
}

/// Changes a resume holds before it upserts them (`restore_visited`): ~1 GB.
const RESUME_CHANGES: usize = 1 << 25;

/// The VISITED SET of a tree's frames `0..=last`, rebuilt from its frame
/// files (each frame's states, its codebooks' additions applied; a state
/// stored twice is an error) and its hull files (replayed in order: a
/// state's layer and hull are its last file's), its first win, and frame
/// `last`'s FRONTIER - the states whose hull grew there, its new states with
/// their skeletons - and the skeletons registry as the emissions left it
/// (every frame file's skeletons, in a search from `room`). The frames'
/// states and hulls are gathered per region as CHANGES and upserted into
/// the packed tables a batch of frames at a time (`RESUME_CHANGES`), every
/// region on every thread; the last frame's grown states read what they
/// held before it (their `was`) between its new states and its growth.
#[allow(clippy::type_complexity)]
pub fn restore_visited<N: Rem>(dir: &std::path::Path, last: u32, room: (i16, i16), threads: usize) -> Result<(crate::storage::visited::VisitedSet<N>, Option<u32>, Frontier<N>, crate::storage::decode::Skeletons<N>)> {
    use crate::storage::hulls;
    use crate::storage::set::Change;
    let geo = crate::storage::GEOMETRY;
    let tree = crate::storage::layer::Tree::<N>::open(dir, last)?;
    let mut visited = crate::storage::visited::VisitedSet::<N>::new(geo);
    for (i, &h) in tree.shapes.iter().enumerate() {
        visited.restore_shape(i as u32, h)?;
    }
    visited.keys = tree.space.clone();
    use crate::storage::visited::StateInfo;
    let mut frontier = Frontier::<N>::default();
    // The emissions' skeletons: every frame file's from frame 1 on (a
    // layer's are the registry's; frame 0's are the start's reference rows').
    let skeletons = crate::storage::decode::Skeletons::<N>::default();
    // Per region its changes not yet upserted, in frame order.
    let mut pending: rustc_hash::FxHashMap<u32, Vec<Change<N>>> = Default::default();
    let mut held = 0usize;
    // The pending changes upserted: per region by state, a state's last
    // change kept (its last file's layer and hull), NEW where any was (two
    // NEW: the state stored twice).
    let flush = |pending: &mut rustc_hash::FxHashMap<u32, Vec<Change<N>>>, visited: &mut crate::storage::visited::VisitedSet<N>, held: &mut usize| -> Result<()> {
        let mut per: Vec<(u32, Vec<Change<N>>)> = std::mem::take(pending).into_iter().collect();
        per.sort_unstable_by_key(|x| x.0);
        let per: Vec<std::sync::Mutex<(u32, Vec<Change<N>>)>> = per.into_iter().map(std::sync::Mutex::new).collect();
        let sorted = crate::storage::wave::par_map(&per, threads, |m| -> Result<(u32, Vec<Change<N>>)> {
            let (region, mut v) = std::mem::take(&mut *m.lock().expect("a region's changes"));
            v.sort_by_key(|c| (c.key, c.cell));
            let mut out: Vec<Change<N>> = Vec::with_capacity(v.len());
            for c in v {
                match out.last_mut() {
                    Some(l) if (l.key, l.cell) == (c.key, c.cell) => {
                        anyhow::ensure!(!(l.new && c.new), "{}: a state of region {region} stored twice", dir.display());
                        *l = Change { new: l.new || c.new, ..c };
                    }
                    _ => out.push(c),
                }
            }
            Ok((region, out))
        });
        let per: Vec<(u32, Vec<Change<N>>)> = sorted.into_iter().collect::<Result<_>>()?;
        *held += visited.upsert_all(per, threads)?.held_new as usize;
        Ok(())
    };
    for f in 0..=last {
        for r in tree.file(f).filter(|_| f > 0).iter().flat_map(|x| x.header.skeletons.iter().chain(&x.header.emitted)) {
            skeletons.register(&r.rt2_in(room)?)?;
        }
        let (layer, _) = tree.layer(f, (f == last).then_some(room), threads)?;
        let p = hulls::path(dir, f);
        if f > 0 && !p.exists() && tree.file(f).is_none() {
            // Past a frontier that died: no frame, no hulls.
            anyhow::ensure!(layer.is_empty(), "{}: frame {f} holds states and no hull file", dir.display());
            continue;
        }
        let hf = hulls::read::<N>(&p, threads).with_context(|| format!("{}: frame {f}'s hulls", dir.display()))?;
        anyhow::ensure!(hf.frame == f && hf.hulls.len() as u64 == layer.states(), "{}: frame {f}'s hull file is of frame {} with {} new states, the frame has {}", dir.display(), hf.frame, hf.hulls.len(), layer.states());
        let f16 = u16::try_from(f).map_err(|_| anyhow::anyhow!("frame {f}: past a u16 layer"))?;
        // The new states, in the layer's order (its hulls').
        let mut h = hf.hulls.iter();
        for (region, t) in layer.set.tables() {
            let v = pending.entry(*region).or_default();
            for (key, mask) in t.iter() {
                for c in crate::storage::layer::bits(mask) {
                    let hull = *h.next().expect("counted above");
                    v.push(Change { key, cell: c as u8, info: Some(StateInfo { hull, layer: f16 }), new: true });
                }
            }
        }
        if f == last {
            // What the last frame's old states pushed before it: the visited
            // set before its growth.
            flush(&mut pending, &mut visited, &mut held)?;
            // Read by region, each region's walked with its states sorted.
            let mut by: Vec<(u32, crate::storage::visited::Key, u8, usize)> = hf.grown.iter().enumerate().map(|(i, g)| (g.0.region, g.0.key, g.0.local, i)).collect();
            by.sort_unstable();
            let mut was: Vec<Option<StateInfo<N>>> = vec![None; hf.grown.len()];
            for run in by.chunk_by(|a, b| a.0 == b.0) {
                let Some(t) = visited.table(run[0].0) else { continue };
                let mut c = t.cursor::<N>();
                for &(_, key, local, i) in run {
                    was[i] = c.seek(key)?.and_then(|e| e.info(local as u32));
                }
            }
            let mut old: crate::storage::wave::ByLayer<N> = Vec::new();
            for (&(sk, layer, hull), was) in hf.grown.iter().zip(was) {
                anyhow::ensure!(layer < f16, "{}: a grown state of layer {layer} in frame {f}'s hulls", dir.display());
                let i = was.ok_or_else(|| anyhow::anyhow!("{}: an old state of frame {f}'s frontier without a hull before", dir.display()))?;
                anyhow::ensure!(i.layer == layer, "{}: an old state's layer {layer} in frame {f}'s hulls, {} before", dir.display(), i.layer);
                if old.last().is_none_or(|x| x.layer != layer) {
                    // Each layer's room exactly (the grown states are by layer).
                    let n = hf.grown.partition_point(|g| g.1 <= layer) - hf.grown.partition_point(|g| g.1 < layer);
                    old.push(crate::storage::wave::OldLayer::new(layer, n));
                }
                old.last_mut().expect("pushed").push(&crate::storage::wave::Old { sk, layer, hull, was: i.hull });
            }
            frontier = Frontier { new: layer, hulls: hf.hulls.clone(), old };
        }
        // The grown ones: older states.
        for &(sk, layer, hull) in &hf.grown {
            pending.entry(sk.region).or_default().push(Change { key: sk.key, cell: sk.local, info: Some(StateInfo { hull, layer }), new: false });
        }
        if f == last || pending.values().map(Vec::len).sum::<usize>() >= RESUME_CHANGES {
            flush(&mut pending, &mut visited, &mut held)?;
        }
    }
    anyhow::ensure!(held == 0, "{}: {held} states stored twice", dir.display());
    anyhow::ensure!(visited.infos() <= visited.len() as u64, "{}: {} states, {} with a hull stored", dir.display(), visited.len(), visited.infos());
    Ok((visited, tree.first_win(), frontier, skeletons))
}

/// One row of a marks file: `(shape, cell, key.0, key.1, dist)`.
pub type MarkRow = (u64, u32, u64, u64, u32);

/// The marks-file row of a state with `deadline` (`u16::MAX`: none, saved
/// as 0).
pub fn mark_row(shape: u64, key: (u64, u64), cell: u32, deadline: u16, horizon: u32) -> MarkRow {
    (shape, cell, key.0, key.1, horizon - (deadline as u32).min(horizon))
}

/// A marks file (`Visited::save`) from its rows, in any order, each state
/// once, with the key space its keys are in.
pub fn save_marks<N: EngineNum>(path: &std::path::Path, mut rows: Vec<MarkRow>, horizon: u32, keys: &celeste_engine::exact::KeySpace<N>) -> Result<()> {
    rows.sort_unstable();
    crate::search::checkpoint::save_value_to(path, &(rows, horizon, keys.to_additions()))
}

/// A marks file's rows, horizon and key space.
pub fn load_marks(path: &std::path::Path) -> Result<(Vec<MarkRow>, u32, celeste_engine::exact::KeySpace)> {
    let bytes = std::fs::read(path)?;
    let payload = crate::search::checkpoint::value_payload(&bytes, path)?;
    let (rows, horizon, adds): (Vec<MarkRow>, u32, celeste_engine::exact::KeyAdditions) = bincode::deserialize(payload).with_context(|| format!("deserializing {}", path.display()))?;
    Ok((rows, horizon, celeste_engine::exact::KeySpace::from_additions(adds).with_context(|| format!("{}: its key space", path.display()))?))
}

/// An in-memory set of states `(shape, cell, key)` (a level's MARKED states
/// or a diagnostic's), saved as a marks file; its keys are in the key space
/// of the tree it names.
pub struct Visited<N: EngineNum = P8> {
    /// Sharded by (shape, cell); each key maps to its DEADLINE, the last frame
    /// from which it still wins by the horizon (`u16::MAX`: none).
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashMap<(u64, u64), u16>>,
    keys: celeste_engine::exact::KeySpace<N>,
}

impl<N: EngineNum> Default for Visited<N> {
    fn default() -> Self {
        Visited { shards: Default::default(), keys: Default::default() }
    }
}

impl<N: EngineNum> Visited<N> {
    /// An empty set over the key space `keys`.
    pub fn new(keys: celeste_engine::exact::KeySpace<N>) -> Self {
        Visited { shards: Default::default(), keys }
    }
    /// The key space the keys are in.
    pub fn keys(&self) -> &celeste_engine::exact::KeySpace<N> {
        &self.keys
    }
    /// `(entries, order-independent CONTENT hash of the (shape, key, cell)
    /// set)` (`exact::ContentHash`).
    pub fn fingerprint(&self) -> (usize, u64) {
        let h = self.keys.content_hash();
        let mut acc = 0u64;
        for ((shape, cell), keys) in &self.shards {
            for &k in keys.keys() {
                acc = acc.wrapping_add(h.state(*shape, k, *cell));
            }
        }
        (self.len(), acc)
    }
    /// An order-independent content hash of every `(shape, cell, key,
    /// deadline)`: equal values filter alike (`MarkFilter`).
    pub fn filter_fingerprint(&self) -> u64 {
        use celeste_engine::runtime2::mix64;
        let h = self.keys.content_hash();
        let mut acc = 0u64;
        for ((shape, cell), keys) in &self.shards {
            for (&k, &d) in keys {
                acc = acc.wrapping_add(mix64(h.state(*shape, k, *cell) ^ (d as u64) << 3));
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
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
}

impl Visited {
    /// THE MARKS FILE (also read by the UI export): bincode `(rows, horizon)`,
    /// `dist` = horizon - deadline; no deadline saves as 0 (filters alike).
    pub fn save(&self, path: &std::path::Path, horizon: u32) -> Result<()> {
        let mut rows: Vec<MarkRow> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            rows.extend(keys.iter().map(|(&key, &d)| mark_row(*shape, key, *cell, d, horizon)));
        }
        save_marks(path, rows, horizon, &self.keys)
    }

    /// A marks file (`save`).
    pub fn load(path: &std::path::Path) -> Result<Self> {
        let (rows, horizon, keys) = load_marks(path)?;
        let mut out = Self::new(keys);
        for (shape, cell, k0, k1, dist) in rows {
            anyhow::ensure!(dist <= horizon, "{}: a mark {dist} frames before h{horizon}", path.display());
            out.insert_until(shape, (k0, k1), cell, u16::try_from(horizon - dist).unwrap_or(u16::MAX));
        }
        Ok(out)
    }
}

/// The first win of a tree complete through `horizon`; `None` if it is not.
pub fn tree_first_win_through<N: EngineNum>(dir: &std::path::Path, horizon: u32) -> Result<Option<Option<u32>>> {
    let done = crate::storage::edges::done_frame(&dir.join("edges"));
    if done.is_none_or(|d| d < horizon) || !crate::storage::layer::frame_exists(dir, horizon) {
        return Ok(None);
    }
    Ok(Some(crate::storage::layer::Tree::<N>::open(dir, horizon)?.first_win()))
}

#[cfg(test)]
mod tests {
    /// The mark filter admits a marked node's rows up to its deadline only.
    #[test]
    fn the_mark_filter_admits_up_to_the_deadline() {
        let s = crate::settings::Settings::default();
        use crate::abstraction::Level;
        let engine = RefEngine::new(&s).expect("ref engine");
        let block = Block::canonical(engine.initial().expect("initial state")).expect("block");
        let coarse = Level::parse("r0sxh").expect("level");
        // The coarse key space holds the projection's codes.
        let mut w = block.rt2().clone_block();
        widen_rt2_to(&mut w, coarse);
        w.canonical();
        let mut space = celeste_engine::exact::KeySpace::new();
        space.absorb(&[&w], crate::compiled::ids());
        let (shape, keys, cells) = widened_keys(&block, coarse, &space).expect("keys");
        let mut marked = Visited::new(space.clone());
        marked.insert_until(shape, keys[0].expect("keyed"), cells[0], 5);
        let f = MarkFilter::new(&marked, coarse, 1);
        assert_eq!(f.allowed(block.rt2(), 5).expect("filter"), vec![true], "at the deadline");
        assert_eq!(f.allowed(block.rt2(), 6).expect("filter"), vec![false], "past it");
        assert_eq!(MarkFilter::new(&Visited::new(Default::default()), coarse, 1).allowed(block.rt2(), 0).expect("filter"), vec![false], "a key space without its codes");
        let empty = Visited::new(space);
        assert_eq!(MarkFilter::new(&empty, coarse, 1).allowed(block.rt2(), 0).expect("filter"), vec![false], "not marked");
    }


    use super::*;
    use crate::storage::layer::Layer;
    use crate::trace::refengine::RefEngine;
    use celeste_core::pico8_num::Pico8Num as P8;
    use celeste_engine::runtime2::{Col, AV};
    use crate::trace::refengine::RefFrameStep;
    use crate::search::hull::Hull;

    /// A fresh forward keeps none of a killed run's files.
    #[test]
    fn a_fresh_forward_clears_what_a_killed_run_left() {
        let s = crate::settings::Settings::default();
        let engine = RefEngine::new(&s).expect("ref engine");
        let init = vec![Block::canonical(engine.initial().expect("initial state")).expect("block")];
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-fresh-start-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        let stale = [dir.join("edges/f009.bin"), dir.join("edges/xfer.bin"), dir.join("frames/f009.bin")];
        for f in &stale {
            std::fs::create_dir_all(f.parent().unwrap()).expect("mkdir");
            std::fs::write(f, b"stale").expect("write");
        }
        ForwardState::start(init, dir, true, None, crate::abstraction::Level::EXACT, &s).expect("start");
        for f in &stale {
            assert!(!f.exists(), "{} survived a fresh start", f.display());
        }
        assert!(crate::storage::layer::frame_exists(dir, 0), "frame 0 checkpointed");
        let _ = std::fs::remove_dir_all(dir);
    }

    /// THE HULL FORWARD LIES BETWEEN THE EXACT REACH AND THE WIDENED FORWARD,
    /// room (1,0) to f34 (the gate's forward, its first 35 frames: to f44
    /// it took 160 CPU-s): with every hull widened to
    /// the whole circle (`hull::WIDEN`: every state's remainder widened, the
    /// forward before hulls) the frames are exactly the forward gate's
    /// before hulls (gates/ckhash_room10_f000-044_widened.txt); the hull
    /// forward keeps a SUBSET of those states, each in a layer no earlier
    /// than the widened one's; and
    /// it holds every state the exact-remainder reach (`search::reach`)
    /// reaches, in a layer no later than the frame it is reached at, with
    /// every remainder the reach brings it in its hull.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels and two forwards, ~30 CPU-s"]
    fn the_hull_forward_lies_between_the_exact_reach_and_the_widened_forward() {
        use crate::search::hull::WIDEN;
        use std::sync::atomic::Ordering;
        let s = crate::settings::Settings { threads: Some(8), ..Default::default() };
        let base = std::env::temp_dir().join(format!("celeste-hull-between-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let level = crate::abstraction::Level::parse("r0sx").expect("level");
        let e = crate::compiled::FrameEngine::new(&s, level).expect("kernels");
        let init = || vec![Block::canonical(RefEngine::new(&s).expect("engine").initial().expect("init")).expect("block")];
        let to = 34;
        // Per frame, its states by content.
        let states = |d: &std::path::Path| -> Vec<rustc_hash::FxHashSet<u64>> {
            let t = crate::storage::layer::Tree::<P8>::open(d, to).expect("tree");
            let content = t.space.content_hash();
            (0..=to).map(|f| t.layer(f, None, 4).expect("layer").0.states_iter().map(|sk| { let (shape, key, cell) = t.state(sk).expect("state"); content.state(shape, key, cell) }).collect()).collect()
        };
        let (wide, hull) = (base.join("widened"), base.join("hull"));
        WIDEN.store(true, Ordering::Relaxed);
        ForwardState::start(init(), &wide, true, None, level, &s).expect("start").extend(&e, &wide, to, None).expect("widened");
        WIDEN.store(false, Ordering::Relaxed);
        ForwardState::start(init(), &hull, true, None, level, &s).expect("start").extend(&e, &hull, to, None).expect("hull");
        let t = crate::storage::layer::Tree::<P8>::open(&wide, to).expect("tree");
        let lines: Vec<String> = (0..=to).map(|f| { let (n, acc) = t.layer(f, None, 4).expect("layer").0.content_fingerprint(&t.space, &t.shapes).expect("fp"); format!("f{f:03} {n} {acc:016x}") }).collect();
        let pinned = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/gates/ckhash_room10_f000-044_widened.txt")).expect("the gate");
        assert_eq!(lines, pinned.lines().take(to as usize + 1).collect::<Vec<_>>(), "the widened forward is not the gate's");
        let (ws, hs) = (states(&wide), states(&hull));
        let first_in = |sets: &[rustc_hash::FxHashSet<u64>], h: u64| sets.iter().position(|x| x.contains(&h));
        let (mut n_hull, mut n_wide) = (0usize, 0usize);
        for (f, layer) in hs.iter().enumerate() {
            n_hull += layer.len();
            n_wide += ws[f].len();
            for &h in layer {
                let w = first_in(&ws, h).unwrap_or_else(|| panic!("f{f}: a hull state the widened forward never reaches"));
                assert!(w <= f, "f{f}: a hull state the widened forward reaches only at f{w}");
            }
        }
        assert!(n_hull < n_wide, "{n_hull} hull states, {n_wide} widened: the hulls cut nothing");
        // The exact reach, inside the hull tree's states and hulls.
        let st = ForwardState::<P8>::at(&hull, to, false, level, &s).expect("at");
        let tree = crate::storage::layer::Tree::<P8>::open(&hull, to).expect("tree");
        let layers: Vec<Layer> = (0..=to).map(|f| tree.layer(f, None, 4).expect("layer").0).collect();
        let p0 = crate::search::arc_dp::start_point::<P8>(&s).expect("start");
        let mut checked = 0u64;
        crate::search::reach::reach(&hull, to, p0, 4, |t, cur, inexact| {
            assert!(inexact.is_empty(), "room (1,0) to f{to} needs no coarsening");
            for (sk, r) in cur {
                let layer = (0..=t).find(|&f| layers[f as usize].contains(*sk)).unwrap_or_else(|| panic!("f{t}: an exactly reached state no layer holds"));
                // (A full hull is not stored: it holds everything.)
                if let Some(info) = st.visited().state(sk.region, sk.key, sk.local as u32) {
                    assert_eq!(info.layer as u32, layer, "f{t}: the state's recorded layer");
                    for (y, xs) in r.slabs() {
                        assert!(info.hull.y.contains_seg(y), "f{t}: a reached y segment outside the hull");
                        for x in xs {
                            assert!(info.hull.x.contains_seg(x), "f{t}: a reached x segment outside the hull");
                        }
                    }
                }
                checked += 1;
            }
            Ok(())
        })
        .expect("reach");
        assert!(checked > 1000, "{checked} reached states checked");
        eprintln!("[hull] f0-f{to}: {n_hull} hull states, {n_wide} widened, {checked} exactly reached (state, frame)s inside the hulls");
        let _ = std::fs::remove_dir_all(&base);
    }

    /// A tree's level -1 record round-trips, and is absent (unknown) on an
    /// old tree; a garbled one is an error.
    #[test]
    fn a_trees_level_minus_one_record_round_trips() {
        let dir = std::env::temp_dir().join(format!("celeste-tree-filter-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        assert_eq!(TreeFilter::read(&dir).expect("read"), None, "no record: unknown");
        assert!(TreeFilter::of_tree(&dir, &Default::default()).is_err(), "a check refuses an unknown filter");
        TreeFilter::Off.write(&dir).expect("write");
        assert_eq!(TreeFilter::of_tree(&dir, &Default::default()).expect("off"), None, "a check of an unfiltered tree applies no level -1");
        for f in [TreeFilter::Off, TreeFilter::MinusOne { m: MinusOne { h: 186, speed: 5 }, table: 0x0123_4567_89ab_cdef }] {
            f.write(&dir).expect("write");
            assert_eq!(TreeFilter::read(&dir).expect("read"), Some(f));
        }
        std::fs::write(dir.join("level_minus_one.txt"), "116\n").expect("write");
        assert!(TreeFilter::read(&dir).is_err(), "a bare horizon (an old binary's record) is an error");
        std::fs::write(dir.join("level_minus_one.txt"), "h 3 speed\n").expect("write");
        assert!(TreeFilter::read(&dir).is_err(), "a garbled record is an error");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A marks file round-trips deadlines (none comes back as the horizon).
    #[test]
    fn marks_files_keep_their_deadlines() {
        let path = std::env::temp_dir().join(format!("celeste-marks-roundtrip-{}.bin", std::process::id()));
        // A key space with shapes 1 and 9, two codes in one field each.
        let mut space = celeste_engine::exact::KeySpace::new();
        let mut adds = Default::default();
        for shape in [1, 9] {
            space.add_shape(&celeste_engine::exact::ShapeRecord { hash: shape, sig: vec![1, u32::MAX], pos: None }, &mut adds);
            space.add_codes(shape, 0, &[celeste_engine::exact::Code::num(0, 0), celeste_engine::exact::Code::num(1, 1)], &mut adds);
        }
        let mut v = Visited::new(space);
        v.insert_until(1, (0, 0), 4, 7);
        v.insert_until(1, (0, 1), 4, 12);
        v.insert(9, (0, 0), 8);
        v.save(&path, 12).expect("save");
        let w = Visited::load(&path).expect("load");
        std::fs::remove_file(&path).expect("rm");
        assert_eq!(w.fingerprint(), v.fingerprint());
        assert_eq!(w.deadline(1, (0, 0), 4), Some(7));
        assert_eq!(w.deadline(1, (0, 1), 4), Some(12));
        assert_eq!(w.deadline(9, (0, 0), 8), Some(12), "no deadline: the horizon");
    }

    /// A resume (`ForwardState::at`) takes the frontier the uninterrupted
    /// forward expands, at every frame. 100% in room (1,0), which has no
    /// berry: every row's berry is lost from frame 0 on. The forward used to
    /// skip `not_expanded` rows only on a frame with a win (and never at frame
    /// 0), a resume always: a resumed 100% run expanded fewer rows.
    #[test]
    fn a_resume_expands_what_the_uninterrupted_forward_expands() {
        let s = crate::settings::Settings { hundred: true, ..Default::default() };
        let dir = std::env::temp_dir().join(format!("celeste-resume-frontier-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let e = RefEngine::new(&s).expect("engine");
        let init = vec![Block::canonical(e.initial().expect("init")).expect("block")];
        let e = RefFrameStep::<P8>::new(&s, crate::abstraction::Level::EXACT).expect("engines");
        let frontier = |st: &ForwardState| -> (Vec<(u32, (u64, u64), u64)>, Vec<Hull>, crate::storage::wave::ByLayer) { (st.frontier.new.iter().collect(), st.frontier.hulls.clone(), st.frontier.old.clone()) };
        let mut st = ForwardState::start(init, &dir, true, None, crate::abstraction::Level::EXACT, &s).expect("start");
        for b in crate::storage::layer::decode_states(&st.frontier.new, &st.visited.keys, st.visited.shapes(), usize::MAX, &|_| true).expect("decode") {
            assert!(not_expanded(&Block::from_rt2(b.1), 0, None, &s).expect("not expanded").iter().all(|&x| x), "frame 0: the berry is lost");
        }
        for to in 0..=3 {
            st.extend(&e, &dir, to, None).expect("extend");
            let resumed = ForwardState::at(&dir, st.frames, true, crate::abstraction::Level::EXACT, &s).expect("at");
            assert_eq!(frontier(&st), frontier(&resumed), "f{}: the resume's frontier is not the forward's", st.frames);
            assert_eq!(st.win_frame, resumed.win_frame, "f{}: the first win", st.frames);
        }
        assert_eq!(st.frames, 1, "nothing expanded past frame 0");
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A run killed after a frame's files but before its done marker
    /// resumes to the uninterrupted tree: the frame is discarded and rerun,
    /// the pos graph (saved before the marker) covers it, the transfers past
    /// the trusted length are overwritten. The one marker holds the frame
    /// and the transfer count; nothing else claims trust.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels and three forwards, ~25 CPU-s"]
    fn a_frame_killed_before_its_done_marker_is_rerun() {
        let s = crate::settings::Settings::default();
        let base = std::env::temp_dir().join(format!("celeste-done-marker-{}", std::process::id()));
        let (a, b) = (base.join("uninterrupted"), base.join("killed"));
        let _ = std::fs::remove_dir_all(&base);
        // The kernels (they record transfers), past the spawn.
        let level = crate::abstraction::Level::parse("r0sxh").expect("level");
        let e = crate::compiled::FrameEngine::new(&s, level).expect("kernels");
        let start = RefEngine::new(&s).expect("engine").initial().expect("init");
        let init = || vec![Block::canonical(start.clone_block()).expect("block")];
        let ends = 30;
        ForwardState::start(init(), &a, true, None, level, &s).expect("start").extend(&e, &a, ends, None).expect("extend");
        let mut st = ForwardState::start(init(), &b, true, None, level, &s).expect("start");
        st.extend(&e, &b, ends - 1, None).expect("extend");
        let marker = std::fs::read_to_string(b.join("edges/done.txt")).expect("done");
        assert_eq!(marker, format!("{} xfers {}\n", ends - 1, st.xfers.pairs.len()));
        st.extend(&e, &b, ends, None).expect("extend");
        // Killed just before frame `ends`' marker.
        std::fs::write(b.join("edges/done.txt"), &marker).expect("rewind the marker");
        for d in [&a, &b] {
            for stale in ["edges/xfer.len", "posgraph.frame"] {
                assert!(!d.join(stale).exists(), "{stale}: a second marker");
            }
        }
        let mut st = ForwardState::resume(&b, true, level, &s).expect("resume").expect("a tree");
        assert_eq!(st.frames, ends - 1, "the untrusted frame is discarded");
        st.extend(&e, &b, ends, None).expect("rerun");
        for f in 0..=ends {
            let keys = |d: &std::path::Path| -> Vec<(u32, (u64, u64), u64)> { crate::storage::layer::Tree::<P8>::open(d, ends).expect("tree").layer(f, None, 2).expect("layer").0.iter().collect() };
            assert_eq!(keys(&a), keys(&b), "f{f}");
        }
        assert!(st.xfers.pairs.len() > 1, "transfers recorded");
        for file in ["edges/done.txt", "edges/xfer.bin", "posgraph.bin", "settings.txt"] {
            assert_eq!(std::fs::read(a.join(file)).expect("a"), std::fs::read(b.join(file)).expect("b"), "{file}");
        }
        let _ = std::fs::remove_dir_all(&base);
    }

    /// Extending frame by frame and resuming reproduce a fresh run's key sets.
    #[test]
    fn forward_extended_frame_by_frame_matches_fresh() {
        let s = crate::settings::Settings::default();
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-rebuild-resume-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        // A frame's states by content (their keys are their tree's).
        let content = |d: &std::path::Path, frame: u32| -> (u64, u64) {
            let t = crate::storage::layer::Tree::<P8>::open(d, frame).expect("tree");
            t.layer(frame, None, 2).expect("layer").0.content_fingerprint(&t.space, &t.shapes).expect("content")
        };
        let keyset = |frame: u32| content(dir, frame);

        {
            let e = RefEngine::new(&s).expect("engine");
            let init = vec![Block::canonical(e.initial().expect("init")).expect("block")];
            forward_run(&RefFrameStep::<P8>::new(&s, crate::abstraction::Level::EXACT).expect("engines"), init, dir, 4, true).expect("fresh");
        }
        let fresh4 = keyset(4);
        assert!(fresh4.0 > 0, "fresh frame 4 empty");

        // The same run extended one frame at a time.
        {
            let e = RefEngine::new(&s).expect("engine");
            let init = vec![Block::canonical(e.initial().expect("init")).expect("block")];
            let e = RefFrameStep::<P8>::new(&s, crate::abstraction::Level::EXACT).expect("engines");
            let mut st = ForwardState::start(init, dir, true, None, crate::abstraction::Level::EXACT, &s).expect("start");
            assert!(pos_graph_path(dir).exists(), "start must leave a graph beside f0");
            for to in 1..=4 {
                st.extend(&e, dir, to, None).expect("extend");
                assert_eq!(st.frames, to);
                assert_eq!(crate::storage::edges::done_frame(&dir.join("edges")), Some(to), "frame {to} trusted");
            }
            assert!(st.pos_graph().is_some());
        }
        let extended4 = keyset(4);
        assert_eq!(fresh4, extended4, "extension diverged from fresh at frame 4");
        eprintln!("[extend] frame 4 states identical: {}", fresh4.0);

        // RESUME: a fresh run to 6 against the tree above resumed at 4.
        let fresh_dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-rebuild-resume-test-fresh6-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(fresh_dir);
        std::fs::create_dir_all(fresh_dir).expect("mkdir");
        {
            let e = RefEngine::new(&s).expect("engine");
            let init = vec![Block::canonical(e.initial().expect("init")).expect("block")];
            forward_run(&RefFrameStep::<P8>::new(&s, crate::abstraction::Level::EXACT).expect("engines"), init, fresh_dir, 6, true).expect("fresh 6");
        }
        {
            let e = RefFrameStep::<P8>::new(&s, crate::abstraction::Level::EXACT).expect("engines");
            let mut st = ForwardState::resume(dir, true, crate::abstraction::Level::EXACT, &s).expect("resume").expect("a tree to resume");
            assert_eq!(st.frames, 4);
            let visited_before = st.visited_len();
            st.extend(&e, dir, 6, None).expect("extend resumed");
            assert!(st.visited_len() >= visited_before);
        }
        assert_eq!(content(fresh_dir, 6), content(dir, 6), "resumed run diverged from fresh at frame 6");
        assert_eq!(content(fresh_dir, 5), content(dir, 5), "resumed run diverged from fresh at frame 5");
        eprintln!("[resume] frames 5 and 6 identical after resuming at 4");
        let _ = std::fs::remove_dir_all(dir);
        let _ = std::fs::remove_dir_all(fresh_dir);
    }

    /// A REGION KERNEL CHECKS ITS BOUNDS: a lane whose speed or remainder lies
    /// outside the ranges its kernel was specialized on DECLINES (the
    /// restrictions' own error, `Op::Restrict`), and the same lane inside them
    /// runs. Before the restriction, the lowering's interval fold seeded the
    /// input cells with those ranges and folded the check away: such a lane
    /// was computed silently wrong.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels, ~20 CPU-s"]
    fn a_lane_outside_its_kernels_bounds_declines() {
        use crate::abstraction::Level;
        let s = crate::settings::Settings::default();
        let level = Level::parse("r0sxh").expect("level");
        let grid = s.region_grid().expect("the default region grid");
        let registry = crate::compiled::asm_kernel::registry(level, &s);
        // The first frame with a player, a few frames in: standing, at rest.
        let mut reference = RefEngine::new(&s).expect("ref engine");
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
            let visited = crate::storage::visited::VisitedSet::new(crate::storage::GEOMETRY);
            let (routes, skeletons) = (crate::storage::route::Routes::default(), crate::storage::decode::Skeletons::default());
            let prov = celeste_engine::exact::Provisional::default();
            let (base, xtab) = (crate::storage::edges::XferTable::default(), Default::default());
            let mut sink = crate::storage::unit::UnitSink::new(&visited, &routes, &skeletons, &prov, Filters::default(), 1, level, 0, &base, &xtab, false, false, &s);
            sink.begin(0);
            let si = sink.push_source(0, (0, 0));
            sink.push_lane(si, cells[0], Hull::full());
            sink.sources_done();
            crate::compiled::dispatch::run_chunk_kernel(&registry, r, &cells, &[0], &mut sink)
        };
        let px = |n: i16| P8::from_i16(n);
        let half = P8::from_parts(0, 0x8000);
        let past = px(grid.speed as i16).checked_add(half).expect("small");
        assert!(runs(&row), "the lane as played runs");
        assert!(runs(&with(ids.f_spd, ids.f_x, px(grid.speed as i16))), "speed at the bound runs");
        assert!(runs(&with(ids.f_rem, ids.f_x, P8::from_raw(0x7fff))), "the remainder at its upper bound runs");
        assert!(!runs(&with(ids.f_spd, ids.f_x, past)), "speed.x past the bound declines");
        assert!(!runs(&with(ids.f_spd, ids.f_y, past.wrapping_neg())), "speed.y past the bound declines");
        assert!(!runs(&with(ids.f_rem, ids.f_x, half)), "a remainder of 0.5 declines");
    }

    /// A PLATFORMS-UNKNOWN KERNEL CHECKS ITS PLATFORMS: a lane whose platform
    /// `x` reaches past the path, whose `rem.x` lies outside the literal the
    /// kernel reads in its place, or whose `spd.x` lies outside its range over
    /// the worlds DECLINES. Before `x` was a restriction it was a seeded range
    /// (`Symbolic::ranges`, gone), checked nowhere, and `rem.x` was not read:
    /// such a lane was computed silently wrong.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (2,1)'s platform kernels, ~100 CPU-s"]
    fn a_lane_outside_its_platform_bounds_declines() {
        use crate::abstraction::Level;
        let s = crate::settings::Settings { room: (2, 1), ..Default::default() };
        let level = Level::parse("r0sxhnp").expect("level");
        let registry = crate::compiled::asm_kernel::registry(level, &s);
        let mut reference = RefEngine::new(&s).expect("ref engine");
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
            let visited = crate::storage::visited::VisitedSet::new(crate::storage::GEOMETRY);
            let (routes, skeletons) = (crate::storage::route::Routes::default(), crate::storage::decode::Skeletons::default());
            let prov = celeste_engine::exact::Provisional::default();
            let (base, xtab) = (crate::storage::edges::XferTable::default(), Default::default());
            let mut sink = crate::storage::unit::UnitSink::new(&visited, &routes, &skeletons, &prov, Filters::default(), 1, level, 0, &base, &xtab, false, false, &s);
            sink.begin(0);
            let si = sink.push_source(0, (0, 0));
            sink.push_lane(si, cells[0], Hull::full());
            sink.sources_done();
            crate::compiled::dispatch::run_chunk_kernel(&registry, r, &cells, &[0], &mut sink)
        };
        let px = |n: i16| P8::from_i16(n);
        let (path_lo, path_hi) = (px(celeste_engine::widening::PLATFORM_PATH.0), px(celeste_engine::widening::PLATFORM_PATH.1));
        let half = P8::from_parts(0, 0x8000);
        let Col::U(AV::Num(speed)) = row.cols[spd] else { panic!("the platform's spd.x is not a number") };
        assert!(runs(&row), "the lane as played runs");
        assert!(runs(&with(x, path_lo, path_hi)), "x over the whole path runs");
        let add = |a: P8, b: P8| a.checked_add(b).expect("small");
        assert!(!runs(&with(x, path_lo, add(path_hi, P8::from_raw(1)))), "x past the path declines");
        assert!(!runs(&with(x, add(path_lo, px(-1)), path_hi)), "x before the path declines");
        assert!(!runs(&with(rem, half.wrapping_neg(), half)), "a remainder reaching 0.5 declines");
        assert!(!runs(&with(spd, add(speed, px(1)), add(speed, px(1)))), "spd.x past its worlds' range declines");
    }

    /// The summit wins at the flag: the TAS31 reference wins at frame 55, not before.
    #[test]
    fn the_summit_reference_wins_at_the_flag() {
        let s = crate::settings::Settings { room: (6, 3), ..Default::default() };
        assert_eq!(win_rect(&s), Some((55, 67, 41, 52)), "the flag at (61, 48)");
        let text = std::fs::read_to_string("tas/room_6_3_reference_frame_55.txt").expect("the reference");
        let inputs: Vec<u8> = text
            .lines()
            .filter(|l| !l.trim_start().starts_with('#'))
            .flat_map(|l| l.split(',').filter(|t| !t.trim().is_empty()).map(|t| t.trim().parse::<u8>().expect("an input byte")).collect::<Vec<_>>())
            .collect();
        assert_eq!(inputs.len(), 55);
        let mut reference = RefEngine::new(&s).expect("ref engine");
        let mut st = reference.initial().expect("initial state");
        for (i, &b) in inputs.iter().enumerate() {
            let next = reference.step_one(&st, b).expect("one concrete successor");
            let won = wins_of(next.rt2(), &s).expect("wins")[0];
            assert_eq!(won, i + 1 == 55, "frame {}: won {won}", i + 1);
            st = next.into_rt2();
        }
    }

    /// THE SPLIT FRAME at a near level reaches the unsplit (pinned) state
    /// counts at every boundary, room (2,1) `r0sxhn`. Guards nil-global
    /// removal and the mid-frame cut not widening floors the player reads.
    /// (The hull forward's counts: the widened forward reached 308, 1576,
    /// 5559 and 15348 at f25-f28, split and unsplit alike; a mid-frame
    /// state's hull merges its sources' and part b moves no remainder, so
    /// the boundaries' hulls are the unsplit frame's.)
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (2,1)'s split kernels and a forward, ~55 CPU-s"]
    fn split_frame_reaches_the_unsplit_frontier_at_a_near_level() {
        use crate::abstraction::Level;
        let s = crate::settings::Settings { room: (2, 1), split_frame: true, ..Default::default() };
        let level = Level::parse("r0sxhn").expect("level");
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-split-near-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");
        let engine = crate::compiled::FrameEngine::new(&s, level).expect("engine");
        let init = vec![Block::canonical(RefEngine::new(&s).expect("ref").initial().expect("init")).expect("block")];
        // Unsplit, frame by frame: 1 state a frame through the spawn, then these.
        let unsplit: Vec<(u32, usize)> = (0..24).map(|f| (f, 1)).chain([(24, 27), (25, 191), (26, 676), (27, 1883), (28, 4514)]).collect();
        let last = unsplit.last().unwrap().0;
        forward_run(&engine, init, dir, 2 * last, false).expect("split forward");
        let tree = crate::storage::layer::Tree::<P8>::open(dir, 2 * last).expect("the tree");
        for (f, want) in unsplit {
            let got = tree.states(2 * f) as usize;
            assert_eq!(got, want, "frame {f} (step {}): the split frame reached {got} states, the unsplit {want}", 2 * f);
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
