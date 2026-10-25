//! The block, the frame-step interface (`FrameStep`) and the HULL FORWARD
//! (`ForwardState`: started, resumed, extended a frame at a time by
//! `storage::wave::run_wave`, each frame checkpointed); trees (`grow_tree`,
//! `TreeL1`), wins, `MarkFilter`. plans/architecture.md "The forward:
//! hulls and storage".
#![warn(unreachable_pub)]

use anyhow::{Context, Result};

use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Rt2, P8};

use crate::search::rem::Rem;
use crate::storage::stats::{Lap, Stats, S};
use crate::storage::wave::Frontier;

/// The summit's level index: no exit, it ends at the flag (`summit_flag`).
pub(crate) const SUMMIT_LEVEL: i16 = 30;

/// The winning positions (x_lo, x_hi, y_lo, y_hi, inclusive): `--win-at` or
/// the summit's flag; a lane wins where it MEETS it. None: the room exit.
pub(crate) fn win_rect(s: &crate::settings::Settings) -> Option<(i16, i16, i16, i16)> {
    if let Some((xl, xh, yl, yh)) = s.win_at {
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

/// The summit's win rectangle around the original cart's flag (tile 118),
/// if `room` is the summit; read off the cart once.
fn summit_flag((rx, ry): (i16, i16)) -> Option<(i16, i16, i16, i16)> {
    type Cache = std::sync::Mutex<std::collections::HashMap<(i16, i16), Option<(i16, i16, i16, i16)>>>;
    static RECTS: std::sync::OnceLock<Cache> = std::sync::OnceLock::new();
    if crate::game_runner::level_index(rx, ry) != SUMMIT_LEVEL {
        return None;
    }
    *RECTS.get_or_init(Default::default).lock().unwrap().entry((rx, ry)).or_insert_with(|| {
        let cart = &celeste_core::cart_data::CART;
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
pub(crate) fn exits_of<N: Arithmetic>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    if let Some((txl, txh, tyl, tyh)) = win_rect(s) {
        let Some(obj) = rt2.position_object() else {
            return Ok(vec![false; lanes]);
        };
        // An interval position wins where it meets the target.
        let axis = |f: u32| {
            rt2.obj_field_cell(obj, f)
                .and_then(|c| crate::storage::cells::whole_range_col(rt2, c))
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
fn room_is<N: Arithmetic>(rt2: &Rt2<N>, (wx, wy): (i16, i16)) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow::anyhow!("wins: no `room` global"))?;
    let is = |f: u32, name: &str, want: i16| -> Result<Vec<bool>> {
        let want = N::from_i16(want);
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
/// and in a 100% run the room's berry taken; never where the player DIED in
/// the update (`died`).
pub fn wins_of<N: Arithmetic>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let mut wins = reaches_win(rt2, s)?;
    if s.category.hundred() {
        for (e, b) in wins.iter_mut().zip(got_fruit(rt2, s.room)?) {
            *e &= b;
        }
    }
    for (w, d) in wins.iter_mut().zip(died(rt2)?) {
        *w &= !d;
    }
    Ok(wins)
}

/// Per lane: is a room restart PENDING (`will_restart`)? Only `kill_player`
/// sets it (`restart_room`), and it holds until the reload 15 updates later,
/// during which no player exists to leave the room: so a row that left the
/// room with it set left it in the update its player died. The cart runs
/// the rest of the dead player's update on its table, and its `next_room()`
/// loads the next room - which then restarts 15 updates in. A DYING EXIT is
/// no win: by the category rules it is a death (`rewrite pico8 chain`
/// reports one), and the per-room optima must chain in real play
/// (plans/correctness.md C8). An unknown counts as alive (an abstract win
/// over-approximates; the concrete search runs it exact).
pub(crate) fn died<N: Arithmetic>(rt2: &Rt2<N>) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{NONE, AV};
    let c = rt2.globals[celeste_names::global("will_restart") as usize];
    anyhow::ensure!(c != NONE, "wins: no `will_restart` global");
    Ok((0..rt2.width).map(|l| rt2.cols[c as usize].at(l) == AV::Bool(true)).collect())
}

/// `wins_of` without its 100% condition.
fn reaches_win<N: Arithmetic>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::AV;
    let exits = exits_of(rt2, s)?;
    if win_rect(s).is_some() || !orb_required(s) {
        return Ok(exits);
    }
    let (ids, lanes) = (crate::compiled::ids(), rt2.width);
    let has_orb: Vec<bool> = {
        let c = rt2.globals[ids.g_max_djump as usize];
        anyhow::ensure!(c != celeste_engine::runtime2::NONE, "wins: no `max_djump` global");
        let two = N::from_i16(2);
        (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(two)).collect()
    };
    Ok(exits.iter().zip(&has_orb).map(|(e, o)| *e && *o).collect())
}

/// Per lane: is start room `(x, y)`'s berry taken (`got_fruit[1 +
/// level_index()]`, nil-padded)? An unknown (a coarse level) counts as
/// taken: a win there over-approximates, and the exact level decides.
pub(crate) fn got_fruit<N: Arithmetic>(rt2: &Rt2<N>, (x, y): (i16, i16)) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let lanes = rt2.width;
    let g = celeste_names::global("got_fruit");
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

/// A SOUND LOWER BOUND on the frames from the big chest's opening to a win
/// (frames, split or not): the orb appears in the chest's 61st draw after
/// it opens and is collectable in its own 8th (68 frames), then its 10
/// frozen frames and the update that can exit make 79, measured on the cart
/// and asserted against it
/// (`tests::the_orb_deadline_is_below_the_carts_chest_to_orb_time`); 70
/// keeps a margin.
pub(crate) const ORB_MIN_FRAMES_AFTER_CHEST: u32 = 70;

/// Per lane in the orb room: is the chest still CLOSED too late to win by
/// the horizon, step `h`? Closed after step `step`, the chest opens at the
/// earliest in the draw of the frame the next step ends or is inside
/// (`Clock::frame_of(step + 1)`), and a win comes at least
/// `ORB_MIN_FRAMES_AFTER_CHEST` frames after: both counted in frames. Such
/// rows are not expanded.
pub(crate) fn orb_deadline_skip<N: Arithmetic>(rt2: &Rt2<N>, step: u32, h: u32, clock: crate::settings::Clock) -> Vec<bool> {
    use celeste_engine::runtime2::AV;
    let lanes = rt2.width;
    if clock.frame_of(step + 1) + ORB_MIN_FRAMES_AFTER_CHEST <= clock.frame_of(h) {
        return vec![false; lanes];
    }
    let ids = crate::compiled::ids();
    let Some(&chest) = rt2.objects_of_type(ids.g_big_chest).first() else {
        return vec![false; lanes];
    };
    let Some(c) = rt2.obj_field_cell(chest, ids.f_state) else { return vec![false; lanes] };
    let zero = N::from_i16(0);
    (0..lanes).map(|l| rt2.cols[c as usize].at(l) == AV::Num(zero)).collect()
}

/// 100%, per lane: can no successor take the room's berry any more? Not
/// taken (`got_fruit` false, not unknown) and nothing left that holds or
/// drops it - the fruit, the fly fruit (gone once it flew off), the key and
/// its chest, a fake wall. Exact: every state the row stands for is lost.
pub(crate) fn berry_lost<N: Arithmetic>(rt2: &Rt2<N>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let lanes = rt2.width;
    let ids = crate::compiled::ids();
    let sources = [ids.g_fruit, ids.g_fly_fruit, ids.g_key, ids.g_chest, ids.g_fake_wall];
    if !s.category.hundred() || sources.iter().any(|&g| !rt2.objects_of_type(g).is_empty()) {
        return Ok(vec![false; lanes]);
    }
    Ok(got_fruit(rt2, s.room)?.into_iter().map(|taken| !taken).collect())
}

/// Per lane of a frame's rows: NOT expanded further (an exited row: no
/// kernels for the next room; a 100% row whose berry is lost; an orb-room
/// row whose chest is still closed too late for the ceiling). The one place
/// it is decided, on every unit's decoded rows (`wave::run_unit`), so a
/// resume expands exactly what an uninterrupted run does. `frame`: the
/// rows' layer; `orb`: the tree's orb cut (`OrbCut`). A state not expanded
/// has no edges, so it is never re-pushed; one expanded is re-pushed
/// whatever the orb's deadline says later: sound, less pruned.
pub(crate) fn not_expanded<N: Arithmetic>(b: &Rt2<N>, frame: u32, orb: Option<OrbCut>, s: &crate::settings::Settings) -> Result<Vec<bool>> {
    let mut skip = exits_of(b, s)?;
    // Under a win rectangle a row that LEFT the room is no win, and its
    // shape has no kernel.
    if win_rect(s).is_some() {
        for (w, here) in skip.iter_mut().zip(room_is(b, s.room)?) {
            *w |= !here;
        }
    }
    for (w, lost) in skip.iter_mut().zip(berry_lost(b, s)?) {
        *w |= lost;
    }
    if let Some(cut) = orb {
        for (w, late) in skip.iter_mut().zip(orb_deadline_skip(b, frame, cut.h, s.clock())) {
            *w |= late;
        }
    }
    Ok(skip)
}

/// THE ORB CUT (`not_expanded`): in the orb room, where a win needs the orb,
/// a row whose big chest is still closed too late for the tree's horizon is
/// not expanded (`orb_deadline_skip`). The horizon is the step the tree was
/// started for (the search's, the forward's `--steps`), its record's
/// `orb_cut` line (`off` elsewhere, or `h H`). The tree's frames depend on
/// it, so it serves horizons up to H and is never raised past it
/// (`grow_tree`), as under level -1.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct OrbCut {
    pub(crate) h: u32,
}

impl OrbCut {
    /// The record's key.
    const KEY: &'static str = "orb_cut";

    /// A new tree's cut for horizon `to` (steps) under `s`, recorded.
    fn start(to: u32, dir: &std::path::Path, s: &crate::settings::Settings) -> Result<Option<OrbCut>> {
        let cut = orb_required(s).then_some(OrbCut { h: to });
        crate::storage::record::update(dir, Self::KEY, cut.map_or("off".to_string(), |c| format!("h {}", c.h)))?;
        Ok(cut)
    }

    /// The cut the tree `dir` records (`None`: off); an error without the
    /// record's line (the cut is unknown) or with a garbled one.
    pub(crate) fn recorded(dir: &std::path::Path) -> Result<Option<OrbCut>> {
        let rec = crate::storage::record::Record::read(dir)?;
        let v = rec.as_ref().and_then(|r| r.get(Self::KEY)).ok_or_else(|| anyhow::anyhow!("{}: no orb cut record: its cut is unknown", dir.display()))?;
        if v == "off" {
            return Ok(None);
        }
        match v.split_whitespace().collect::<Vec<_>>()[..] {
            ["h", h] => Ok(Some(OrbCut { h: h.parse().with_context(|| format!("{}: not an orb cut line: {v:?}", dir.display()))? })),
            _ => anyhow::bail!("{}: not an orb cut line: {v:?}", crate::storage::record::path(dir).display()),
        }
    }
}

/// Is the start room the orb room (5,2)? A win there also needs the orb
/// (`max_djump == 2`): later levels are played with the second dash.
pub(crate) fn orb_required(s: &crate::settings::Settings) -> bool {
    let (x, y) = s.room;
    crate::game_runner::level_index(x, y) == crate::game_runner::ORB_LEVEL && !s.category.gemskip()
}

/// THE LEVEL -1 FILTER a run asks for (`--level-minus-one H,S`, `h` in
/// steps): a row is dropped at emission where the table proves its cell
/// cannot exit by `h` (sound: inductive ranges, clipped successors and
/// deaths accounted). A tree records it with its table (`TreeL1`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MinusOne {
    pub(crate) h: u32,
    /// The table's speed bound S, px per frame.
    pub(crate) speed: i32,
}

impl MinusOne {
    /// Its table as this binary builds it under `s` (a new tree's; a tree's
    /// own is `TreeL1::read`): an error where the table cannot be built
    /// (the search records the refusal before anything else needs it).
    pub(crate) fn table(&self, s: &crate::settings::Settings) -> Result<&'static crate::search::level_minus_one::CostToGo> {
        crate::search::level_minus_one::table(self.speed, s)
    }

    /// As a record names it (the ladder's `filtered_for`): `off`, or `h H
    /// speed S`.
    pub(crate) fn text(this: Option<&MinusOne>) -> String {
        this.map_or("off".to_string(), |m| format!("h {} speed {}", m.h, m.speed))
    }
}

/// A TREE'S LEVEL -1 FILTER: its record's `level_minus_one` line (`off`, or
/// `h H speed S table FP`) and its table (beside the record,
/// `level_minus_one::TABLE_FILE`, its fingerprint FP). A forward extends a
/// tree under ITS filter and table, and a check of the tree applies them,
/// never the environment's: every frame of a tree is filtered alike,
/// whatever binary reads it. The tree serves horizons up to `h` and cannot
/// be raised past it (`grow_tree`).
#[derive(Clone, Copy)]
pub(crate) struct TreeL1 {
    pub(crate) h: u32,
    pub(crate) speed: i32,
    pub(crate) table: &'static crate::search::level_minus_one::CostToGo,
}

impl TreeL1 {
    /// The record's key.
    const KEY: &'static str = "level_minus_one";

    /// A new tree's filter: `want`'s table built under `s` and saved in the
    /// tree `dir`; the record's line written.
    fn start(want: Option<MinusOne>, dir: &std::path::Path, s: &crate::settings::Settings) -> Result<Option<TreeL1>> {
        let l1 = match want {
            None => None,
            Some(m) => {
                let table = m.table(s)?;
                table.save_in(dir)?;
                Some(TreeL1 { h: m.h, speed: m.speed, table })
            }
        };
        crate::storage::record::update(dir, Self::KEY, Self::text(l1.as_ref()))?;
        Ok(l1)
    }

    /// The filter the tree `dir` records and its table's fingerprint
    /// (`None`: off); an error without the record's line (the filter is
    /// unknown) or with a garbled one.
    pub(crate) fn recorded(dir: &std::path::Path) -> Result<Option<(MinusOne, u64)>> {
        let rec = crate::storage::record::Record::read(dir)?;
        let s = rec.as_ref().and_then(|r| r.get(Self::KEY)).ok_or_else(|| anyhow::anyhow!("{}: no level -1 record: its filter is unknown", dir.display()))?;
        if s == "off" {
            return Ok(None);
        }
        let w: Vec<&str> = s.split_whitespace().collect();
        let parsed = match w[..] {
            ["h", h, "speed", sp, "table", fp] => (|| Some((h.parse().ok()?, sp.parse().ok()?, u64::from_str_radix(fp, 16).ok()?)))(),
            _ => None,
        };
        let (h, speed, table) = parsed.ok_or_else(|| anyhow::anyhow!("{}: not a level -1 line: {s:?}", crate::storage::record::path(dir).display()))?;
        Ok(Some((MinusOne { h, speed }, table)))
    }

    /// The tree `dir`'s filter (`recorded`) with the TREE's table, checked
    /// against the record's fingerprint.
    pub(crate) fn read(dir: &std::path::Path) -> Result<Option<TreeL1>> {
        Self::recorded(dir)?.map(|(m, fp)| Ok(TreeL1 { h: m.h, speed: m.speed, table: crate::search::level_minus_one::tree_table(dir, fp)? })).transpose()
    }

    /// The record line of a filter (`None`: off) with its table's fingerprint.
    fn line(filter: Option<(MinusOne, u64)>) -> String {
        match filter {
            None => "off".to_string(),
            Some((m, fp)) => format!("{} table {fp:016x}", MinusOne::text(Some(&m))),
        }
    }

    /// The record line's value (`None`: off).
    pub(crate) fn text(this: Option<&TreeL1>) -> String {
        Self::line(this.map(|l| (MinusOne { h: l.h, speed: l.speed }, l.table.fingerprint())))
    }

    /// Does it drop every row of `shape` at `cell` at step `step` (the
    /// table proves no exit by `h`)?
    #[inline]
    pub(crate) fn drops(&self, shape: u64, cell: u32, step: u32) -> bool {
        self.table.too_late(shape, cell, step, self.h)
    }

}

/// AN OBSERVER of the rows a forward step makes: the one seam a tool that
/// checks them asks of the wave (`check::keys`); none in production
/// (`ForwardState::step_with`). The wave calls it where the rows are:
/// `emitted`, a batch of one shape's rows as the kernels emitted them, keyed
/// (provisional keys `prov`'s), before level -1 and the filters drop any;
/// `stored`, a piece of the new layer decoded at the frame's end, its rows'
/// cells as the states hold them. An error fails the step.
pub(crate) trait RowCheck<N: Rem>: Sync {
    fn emitted(&self, rows: &crate::storage::unit::RowBuf<N>, keys: &celeste_engine::exact::KeySpace<N>, prov: &celeste_engine::exact::Provisional<N>, skeletons: &crate::storage::decode::Skeletons<N>) -> Result<()>;
    fn stored(&self, rows: &Rt2<N>, cells: &[u32], keys: &celeste_engine::exact::KeySpace<N>) -> Result<()>;
}

/// What a forward filters by.
pub(crate) struct Filters<'a, N: Arithmetic = P8> {
    /// The coarser level's arc marks (the objects ladder).
    pub(crate) marks: Option<&'a MarkFilter<'a, N>>,
    /// The tree's level -1 filter.
    pub(crate) l1: Option<TreeL1>,
    /// The tree's orb cut.
    pub(crate) orb: Option<OrbCut>,
}

impl<N: Arithmetic> Clone for Filters<'_, N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: Arithmetic> Copy for Filters<'_, N> {}

impl<N: Arithmetic> Default for Filters<'_, N> {
    fn default() -> Self {
        Filters { marks: None, l1: None, orb: None }
    }
}

/// THE OBJECTS LADDER's link: a finer forward keeps a row at frame t only if
/// its projection (`Rt2::widen_to`) is a coarse node ARC-MARKED with deadline
/// >= t. Sound: a fine state winning from t with remainder r projects to a
/// coarse node winning from t with r (coarse over-approximates, r exact).
/// The projection is keyed in the COARSE tree's key space (the marks'): a
/// code it lacks is a state the coarse tree never reached.
pub struct MarkFilter<'a, N: Arithmetic = P8> {
    marked: &'a Visited<N>,
    coarser: crate::abstraction::Level,
    clock: crate::settings::Clock,
}

impl<'a, N: Arithmetic> MarkFilter<'a, N> {
    pub(crate) fn new(marked: &'a Visited<N>, coarser: crate::abstraction::Level, clock: crate::settings::Clock) -> Self {
        MarkFilter { marked, coarser, clock }
    }

    /// Per lane of `rt2` (rows of step `frame`): admitted? Under the split
    /// frame only at frame boundaries: a mid-frame row's projection does not
    /// key as the coarser tree's mid-frame rows (`widen::widen_near_floors`
    /// keeps a near floor's computed `collideable` in the player's probe
    /// window there, `Rt2::widen_to` does not), so filtering it would drop
    /// marked paths. The next boundary filters its successors.
    pub(crate) fn allowed(&self, rt2: &Rt2<N>, frame: u32) -> Result<Vec<bool>> {
        if !self.clock.boundary(frame) {
            return Ok(vec![true; rt2.width]);
        }
        Ok(self.deadlines(rt2)?.into_iter().map(|d| d.is_some_and(|d| u32::from(d) >= frame)).collect())
    }

    /// Per lane of `rt2`, its projection's deadline at the coarser level
    /// (`None`: not marked there).
    pub(crate) fn deadlines(&self, rt2: &Rt2<N>) -> Result<Vec<Option<u16>>> {
        let (shape, keys, cells) = widened_keys(rt2, self.coarser, self.marked.keys())?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| k.and_then(|k| self.marked.deadline(shape, k, c))).collect())
    }

    pub(crate) fn coarser(&self) -> crate::abstraction::Level {
        self.coarser
    }

    /// The coarser level's arc-marked nodes.
    pub(crate) fn marked(&self) -> &Visited<N> {
        self.marked
    }
}


/// Each lane's `(shape, key, cell)` at the `coarser` level (its node there)
/// in the key space `keys` (`None`: a code the space lacks - no node).
pub(crate) fn widened_keys<N: Arithmetic>(
    rt2: &Rt2<N>,
    coarser: crate::abstraction::Level,
    keys: &celeste_engine::exact::KeySpace<N>,
) -> Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    let mut w = rt2.clone_block();
    w.widen_to(coarser);
    w.canonical();
    let keys = keys.lookup_keys(&w);
    let cells = crate::storage::cells::block_cells(&w)?;
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
    fn run(&self, block: &Rt2<N>, cell_in: &[u32], lanes: &[usize], sink: &mut crate::storage::unit::UnitSink<N>) -> Result<()>;

    /// Build what the engine builds on first use (the kernels) before a
    /// wave's clock starts: inside the wave every worker would wait on the
    /// build's lock, and a one-frame bench would time the build.
    fn warm(&self) {}

    /// The level its rows widen and key to.
    fn level(&self) -> crate::abstraction::Level;

    /// The settings it runs under (the wave's: threads, the win, the category).
    fn settings(&self) -> std::sync::Arc<crate::settings::Settings>;
}

/// The forward's in-memory state, EXTENDED frame by frame (each checkpointed).
#[derive(Clone)]
pub struct ForwardState<N: Rem = P8> {
    /// The last frame's frontier: its new states, and the old ones whose
    /// hull grew there.
    frontier: Frontier<N>,
    /// Every state reached so far, with its layer and hull.
    visited: crate::storage::visited::VisitedSet<N>,
    xfers: crate::storage::edges::XferTable<N>,
    /// The edge files (the re-pushes read their in-edges; shared by a copy).
    edges: std::sync::Arc<crate::storage::edges::EdgeFiles>,
    /// The shapes' skeletons the emissions register (the next layers';
    /// shared by a copy), and the shapes whose skeleton a frame file holds.
    skeletons: std::sync::Arc<crate::storage::decode::Skeletons<N>>,
    persisted: rustc_hash::FxHashSet<u64>,
    /// The last frame computed (and checkpointed).
    pub(crate) frames: u32,
    /// The first frame a lane won, if any so far.
    pub(crate) win_frame: Option<u32>,
    /// The level -1 filter the tree's frames were built under (the tree's
    /// table).
    l1: Option<TreeL1>,
    /// The orb cut the tree's frames were built under.
    orb: Option<OrbCut>,
}

pub struct ForwardResult {
    pub win_frame: Option<u32>,
    /// The last checkpointed frame.
    pub frames: u32,
}

/// CHECKPOINT a wave: frame `frame`'s file (its new states and wins, and
/// its frontier: every state whose hull grew, with its layer and hull),
/// read back and checked to hold exactly the wave's; its steps and bytes
/// into `stats`.
fn checkpoint<N: Rem>(dir: &std::path::Path, frame: u32, wave: &crate::storage::wave::Wave<N>, emitted: &[crate::storage::layer::SkelRecord<N>], s: &crate::settings::Settings, stats: &mut Stats) -> Result<()> {
    use crate::storage::layer;
    anyhow::ensure!(u16::try_from(frame).is_ok(), "frame {frame}: past a u16 layer");
    let mut lap = Lap::start();
    let path = layer::frame_path(dir, frame);
    let meta = layer::FrameMeta { frame, shapes: &wave.shapes, keys: &wave.keys, emitted };
    stats.set(S::FrameBytes, layer::write_frame(&path, &meta, &wave.frontier, &wave.wins, s.threads())?);
    stats.lap(S::CkptFile, &mut lap);
    layer::check_frame_file(&path, &wave.frontier, &wave.wins, s.threads())?;
    stats.lap(S::CkptCheck, &mut lap);
    stats.set(S::Ckpt, stats.get(S::CkptFile) + stats.get(S::CkptCheck));
    Ok(())
}

impl<N: Rem> ForwardState<N> {
    /// Frame 0: `initial` into an empty visited set and checkpointed, each
    /// with the start's hull; the tree is filtered by `minus_one`, cut for
    /// the horizon `to` (`OrbCut`) and widened at `level` (each recorded,
    /// with the settings `s`).
    pub(crate) fn start(initial: Vec<Rt2<N>>, dir: &std::path::Path, to: u32, minus_one: Option<MinusOne>, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Self> {
        // A fresh tree keeps nothing a killed run left (stale edge files, a
        // table, a trust marker); of its record, only a ladder level's
        // `filtered_for` (the driver's, written before).
        for sub in ["frames", "edges"] {
            let p = dir.join(sub);
            if p.exists() {
                std::fs::remove_dir_all(&p).with_context(|| p.display().to_string())?;
            }
        }
        let table = dir.join(crate::search::level_minus_one::TABLE_FILE);
        if table.exists() {
            std::fs::remove_file(&table).with_context(|| table.display().to_string())?;
        }
        let mut rec = crate::storage::record::Record::read(dir)?.unwrap_or_default();
        rec.retain(|k| k == "filtered_for");
        rec.write(dir)?;
        s.write(dir, level)?;
        let l1 = TreeL1::start(minus_one, dir, s)?;
        let orb = OrbCut::start(to, dir, s)?;
        // The start has no player yet (`rem::start_point`'s premise):
        // `seed` keys it as it is, so a player's remainder would be stored
        // exact where every other state holds it widened, beside its hull.
        anyhow::ensure!(initial.iter().all(|b| b.player_objects().is_empty()), "frame 0: an initial row holds a player, whose remainder the start's hull does not describe");
        let mut visited = crate::storage::visited::VisitedSet::<N>::new();
        let seeded = crate::storage::wave::seed(&mut visited, &initial, crate::search::rem::start_hull::<N>(), s)?;
        checkpoint(dir, 0, &seeded, &[], s, &mut Stats::default())?;
        let won = !seeded.wins.is_empty();
        crate::storage::record::set_done(dir, 0, 0)?;
        Ok(ForwardState { frontier: seeded.frontier, visited, xfers: Default::default(), edges: Default::default(), skeletons: Default::default(), persisted: Default::default(), frames: 0, win_frame: won.then_some(0), l1, orb })
    }

    /// The forward as its checkpoint tree left it (`None`: no tree); extending
    /// it matches extending the original. Refused at another `level` than
    /// the tree's, or under other settings `s` than its.
    pub(crate) fn resume(dir: &std::path::Path, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Option<Self>> {
        let Some(mut last) = crate::storage::layer::last_frame(dir) else { return Ok(None) };
        let t = std::time::Instant::now();
        // Trust only frames whose files are complete (at most one lost).
        let edges_dir = dir.join("edges");
        let done = crate::storage::record::done_frame(dir).unwrap_or(last);
        if done < last {
            for f in done + 1..=last {
                std::fs::remove_file(crate::storage::layer::frame_path(dir, f))?;
            }
            eprintln!("[resume] frames f{} to f{last} discarded: their files were not complete", done + 1);
            last = done;
        }
        crate::storage::edges::discard_after(&edges_dir, last)?;
        let st = Self::at(dir, last, level, s)?;
        eprintln!(
            "[resume] {}: f{last} ({} new states in {} entries, {} old), visited {} states in {} entries, first win {:?}, level -1 {}, {:.1} s",
            dir.display(),
            st.frontier.new.states(),
            st.frontier.new.entries(),
            st.frontier.old_states(),
            st.visited.len(),
            st.visited.entries(),
            st.win_frame,
            TreeL1::text(st.l1.as_ref()),
            t.elapsed().as_secs_f64()
        );
        Ok(Some(st))
    }

    /// The forward as it stood after frame `last` of the tree in `dir`,
    /// READ-ONLY: later frames are ignored, not discarded (`resume` discards
    /// them first), so `step` from here is the real forward's frame
    /// `last + 1`. Refused under other settings or at another `level`.
    pub fn at(dir: &std::path::Path, last: u32, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Self> {
        s.check(dir, level)?;
        let edges_dir = dir.join("edges");
        let l1 = TreeL1::read(dir)?;
        let orb = OrbCut::recorded(dir)?;
        let (visited, win_frame, frontier, skeletons) = restore_visited::<N>(dir, last, s.room, s.threads())?;
        let persisted = skeletons.shapes().into_iter().collect();
        let xfers = crate::storage::edges::XferTable::<N>::load(dir)?;
        let edges = std::sync::Arc::new(crate::storage::edges::EdgeFiles::open(&edges_dir, last)?);
        Ok(ForwardState { frontier, visited, xfers, edges, skeletons: std::sync::Arc::new(skeletons), persisted, frames: last, win_frame, l1, orb })
    }

    pub fn visited_len(&self) -> usize {
        self.visited.len()
    }

    pub fn visited(&self) -> &crate::storage::visited::VisitedSet<N> {
        &self.visited
    }

    pub fn frontier(&self) -> &Frontier<N> {
        &self.frontier
    }

    pub fn xfers(&self) -> &crate::storage::edges::XferTable<N> {
        &self.xfers
    }

    /// ONE FRAME, `frames + 1`, under the tree's level -1 filter and the
    /// coarser level's `marks`: the wave (its edges into `edges_dir`;
    /// `None`: not recorded), then with `ckpt` its checkpoint into that
    /// tree directory. The state advances to the frame (a bench's rep runs
    /// on a copy); the frame's stats.
    pub fn step(&mut self, engine: &dyn FrameStep<N>, edges_dir: Option<&std::path::Path>, ckpt: Option<&std::path::Path>, marks: Option<&MarkFilter<N>>) -> Result<Stats> {
        self.step_with(engine, edges_dir, ckpt, marks, None)
    }

    /// THE STEP'S LINES, as `ckhash` prints them, of the frame it last made
    /// (`frames`): `f` (its new states), `h` (the frontier's hulls), with
    /// `edges` (a directory whose `edges/` holds the frame's edge file
    /// alone) `e`, with `files` `b` over the bytes of every file under it.
    pub fn lines(&self, edges: Option<&std::path::Path>, files: Option<&std::path::Path>, threads: usize) -> Result<String> {
        let (frame, visited, front) = (self.frames, &self.visited, &self.frontier);
        let (n, acc) = front.new.content_fingerprint(&visited.keys, visited.shapes())?;
        let mut out = format!("f{frame:03} {n} {acc:016x}\n");
        let (nh, ah) = crate::storage::hulls::fingerprint(front.states(frame), &visited.keys, visited.shapes())?;
        out += &format!("h{frame:03} {nh} {ah:016x}\n");
        if let Some(dir) = edges {
            let (n, acc) = crate::storage::bench::edge_fingerprint(dir, frame, &self.xfers, visited, threads)?;
            out += &format!("e{frame:03} {n} {acc:016x}\n");
        }
        if let Some(dir) = files {
            let (files, bytes, acc) = crate::storage::bench::dir_fingerprint(dir)?;
            out += &format!("b{frame:03} {files} {bytes} {acc:016x}\n");
        }
        Ok(out)
    }

    /// `step` with its rows shown to `check` (a tool's, `RowCheck`).
    pub(crate) fn step_with(&mut self, engine: &dyn FrameStep<N>, edges_dir: Option<&std::path::Path>, ckpt: Option<&std::path::Path>, marks: Option<&MarkFilter<N>>, check: Option<&dyn RowCheck<N>>) -> Result<Stats> {
        let (s, t) = (&engine.settings(), std::time::Instant::now());
        let frame = self.frames + 1;
        let cx = crate::storage::wave::WaveCtx {
            visited: &mut self.visited,
            xfers: &mut self.xfers,
            filters: Filters { marks, l1: self.l1, orb: self.orb },
            frame,
            edges_dir,
            edges: Some(&self.edges),
            skeletons: &self.skeletons,
            check,
        };
        // The frontier is spent by the wave (dropped before its end).
        let wave = crate::storage::wave::run_wave(engine, std::mem::take(&mut self.frontier), cx)?;
        let mut stats = wave.stats;
        if let Some(dir) = ckpt {
            // The skeletons first emitted here, of shapes the frame holds
            // no state of: a later re-push may reach one (`FrameHeader::emitted`).
            let emitted = self.skeletons.records(|h| self.persisted.contains(&h) || wave.frontier.new.skeletons.contains_key(&h));
            checkpoint(dir, frame, &wave, &emitted, s, &mut stats)?;
            self.persisted.extend(self.skeletons.shapes());
        }
        if !wave.wins.is_empty() && self.win_frame.is_none() {
            self.win_frame = Some(frame);
        }
        self.frames = frame;
        self.frontier = wave.frontier;
        stats.set(S::Visited, self.visited.len() as u64);
        stats.add_time(S::Total, t.elapsed());
        stats.set_gb(S::Peak, crate::metrics::peak_rss_gb());
        Ok(stats)
    }

    /// Compute and checkpoint frames `frames+1 ..= to` under the tree's
    /// level -1 filter and the coarser level's `marks`; only an empty
    /// frontier stops it (the backward seeds from wins at every frame).
    /// Ends with the kernels' lines over the frames it computed.
    pub(crate) fn extend(&mut self, engine: &dyn FrameStep<N>, dir: &std::path::Path, to: u32, marks: Option<&MarkFilter<N>>) -> Result<()> {
        let edges_dir = dir.join("edges");
        let mut last_free = None;
        let mut total = Stats::default();
        while self.frames < to && !self.frontier.is_empty() {
            let won_before = self.win_frame.is_some();
            let stats = self.step(engine, Some(&edges_dir), Some(dir), marks)?;
            total.merge(&stats);
            let frame = self.frames;
            // The frame's LAST write: every file above is trusted from here.
            crate::storage::record::set_done(dir, frame, self.xfers.pairs.len())?;
            std::sync::Arc::get_mut(&mut self.edges).ok_or_else(|| anyhow::anyhow!("the forward's edge files are shared"))?.add_frame(&edges_dir, frame)?;
            log_frame(dir, frame, &stats)?;
            ensure_disk_space(dir, &mut last_free)?;
            if !won_before && self.win_frame.is_some() {
                eprintln!("[fwd] first win at f{frame}");
            }
        }
        crate::compiled::print_kernel_stats(&total)
    }
}

/// A level's tree through step `to` under the run's level -1 filter `want`:
/// used as it is when it reaches `to` under a filter that serves it (none,
/// or one at a horizon >= `to`); else resumed and extended under its own
/// filter, or started under `want`. A tree filtered below `to` is REFUSED:
/// its frames depend on the horizon through level -1's dropped pushes, so
/// it cannot be raised exactly. `marks`: the coarser level's filter.
/// `engine` is built only when frames are computed, at `level` under `s`
/// (the tree's, checked).
#[allow(clippy::too_many_arguments)]
pub fn grow_tree<N: Rem>(
    dir: &std::path::Path,
    to: u32,
    want: Option<MinusOne>,
    marks: Option<&MarkFilter<N>>,
    level: crate::abstraction::Level,
    engine: impl FnOnce() -> Result<Box<dyn FrameStep<N>>>,
    initial: impl FnOnce() -> Result<Vec<Rt2<N>>>,
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
        match TreeL1::recorded(dir)? {
            None => {
                if want.is_some() {
                    eprintln!("[forward] {}: the tree is not filtered by level -1; it stays unfiltered", dir.display());
                }
            }
            Some((m, _)) => {
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
        if let Some(cut) = OrbCut::recorded(dir)? {
            anyhow::ensure!(
                to <= cut.h,
                "{}: the tree's orb cut is for step {}, below this run's step {to}: its frames left unexpanded the rows whose big chest was closed too late for step {} - delete it, or run at most to step {}",
                dir.display(),
                cut.h,
                cut.h,
                cut.h
            );
        }
    }
    if let Some(first_win) = complete {
        eprintln!("[forward] {}: the tree reaches step {to}", dir.display());
        return Ok(ForwardResult { win_frame: first_win, frames: to });
    }
    let engine = engine()?;
    anyhow::ensure!(engine.level() == level, "the forward's engine is at {}, the tree at {level}", engine.level());
    anyhow::ensure!(*engine.settings() == *s, "the forward's engine runs under other settings than the tree's");
    let mut st = match ForwardState::<N>::resume(dir, level, s)? {
        Some(st) => st,
        None => ForwardState::start(initial()?, dir, to, want, level, s)?,
    };
    st.extend(engine.as_ref(), dir, to, marks)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames })
}

/// A fresh forward from `initial` to `max_frames` (the tests' driver).
#[cfg(test)]
pub(crate) fn forward_run<N: Rem>(
    engine: &dyn FrameStep<N>,
    initial: Vec<Rt2<N>>,
    dir: &std::path::Path,
    max_frames: u32,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, max_frames, None, engine.level(), &engine.settings())?;
    st.extend(engine, dir, max_frames, None)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames })
}

/// The free disk the forward keeps, GB (`ensure_disk_space`).
const MIN_FREE_GB: u64 = 20;

/// Stop the forward before the checkpoint disk fills (a full disk fails a
/// worker mid-write and starves the machine): an error below `MIN_FREE_GB`
/// plus three times what the last frame wrote. `last_free`: the free bytes
/// after the previous frame of THIS forward (`None` at its first: a delta
/// across another level's phases would count other writers' bytes).
fn ensure_disk_space(dir: &std::path::Path, last_free: &mut Option<u64>) -> Result<()> {
    use std::os::unix::ffi::OsStrExt;
    let path = std::ffi::CString::new(dir.as_os_str().as_bytes())?;
    // SAFETY: `path` is NUL-terminated; `st` is a valid out-parameter that
    // statvfs fills on success.
    let mut st: libc::statvfs = unsafe { std::mem::zeroed() };
    anyhow::ensure!(unsafe { libc::statvfs(path.as_ptr(), &mut st) } == 0, "statvfs {}: {}", dir.display(), std::io::Error::last_os_error());
    let free = st.f_bavail as u64 * st.f_frsize as u64;
    let frame_bytes = last_free.replace(free).map_or(0, |last| last.saturating_sub(free));
    let need = (MIN_FREE_GB << 30) + 3 * frame_bytes;
    anyhow::ensure!(
        free >= need,
        "only {} GB free on {}'s filesystem, the last frame used {} GB (the floor {MIN_FREE_GB} GB, plus 3 frames): stopping the forward before the disk fills",
        free >> 30,
        dir.display(),
        frame_bytes >> 30
    );
    Ok(())
}

/// The per-frame `[fwd]` line and its record in the tree (`FORWARD_STATS`).
fn log_frame(dir: &std::path::Path, frame: u32, st: &Stats) -> Result<()> {
    eprintln!("[fwd] f{frame:03} {}", st.line());
    append_forward_stats(dir, &forward_stats_row(frame, st))
}

/// One frame's row of the tree's forward record (`FORWARD_STATS`): (column,
/// value), every stat.
pub(crate) fn forward_stats_row(frame: u32, st: &Stats) -> Vec<(&'static str, String)> {
    let mut row = vec![("frame", frame.to_string())];
    row.extend(st.row());
    row
}

/// A tree's per-frame forward record (`<dir>/forward.tsv`): a header row
/// naming the columns, then one row per frame the forward ran, appended as
/// it runs (a resumed or extended tree appends; a frame rerun after a crash
/// appears again, and its LAST row is the frame's). `export-ui` reads it.
pub(crate) const FORWARD_STATS: &str = "forward.tsv";

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

/// Frame-file states a resume merges at once (`restore_visited`): ~1 GB of
/// changes.
const RESUME_STATES: usize = 1 << 25;

/// A frame file as a resume merges it: its layer, new states (with their
/// hulls, in its state order) and grown states (by state).
struct FrameIn<N: Rem> {
    layer: u16,
    new: crate::storage::layer::Layer<N>,
    hulls: Vec<crate::search::hull::HullOf<N>>,
    grown: Vec<crate::storage::hulls::Grown<N>>,
}

/// `frames` merged into `visited` at once (`VisitedSet::absorb`: by region,
/// a region's in frame order; a state stored twice an error).
fn absorb_frames<N: Rem>(visited: &mut crate::storage::visited::VisitedSet<N>, frames: &[FrameIn<N>], threads: usize) -> Result<()> {
    use crate::storage::visited::Part;
    let empty = crate::storage::set::PlainTable::default();
    let mut parts: Vec<Part<N, crate::storage::hulls::Grown<N>>> = Vec::new();
    for fr in frames {
        let (tables, mut regions) = (fr.new.set.tables(), Vec::new());
        regions.extend(tables.iter().map(|t| t.0).chain(fr.grown.iter().map(|g| g.sk.region)));
        regions.sort_unstable();
        regions.dedup();
        let (mut t, mut at, mut g) = (0usize, 0usize, 0usize);
        for region in regions {
            let new = match tables.get(t) {
                Some((r, table)) if *r == region => {
                    t += 1;
                    table
                }
                _ => &empty,
            };
            let n = new.states() as usize;
            let k = g + fr.grown[g..].partition_point(|x| x.sk.region == region);
            parts.push(Part { region, new, hulls: &fr.hulls[at..at + n], grown: &fr.grown[g..k], layer: fr.layer });
            (at, g) = (at + n, k);
        }
    }
    // By region, a region's parts in frame order.
    parts.sort_by_key(|p| p.region);
    let held = visited.absorb(&parts, threads)?.held_new;
    anyhow::ensure!(held == 0, "{held} states stored twice");
    Ok(())
}

/// The VISITED SET of a tree's frames `0..=last`, rebuilt from its frame
/// files, merged in as the forward merged them (`VisitedSet::absorb`, a
/// batch of frames at a time: `RESUME_STATES`); its first win; frame
/// `last`'s FRONTIER (its new states, and the states whose hull grew there
/// with the hull they held before: their `was`); and the skeletons registry
/// as the emissions left it.
#[allow(clippy::type_complexity)]
pub(crate) fn restore_visited<N: Rem>(dir: &std::path::Path, last: u32, room: (i16, i16), threads: usize) -> Result<(crate::storage::visited::VisitedSet<N>, Option<u32>, Frontier<N>, crate::storage::decode::Skeletons<N>)> {
    let tree = crate::storage::layer::Tree::<N>::open(dir, last)?;
    let mut visited = crate::storage::visited::VisitedSet::<N>::new();
    for (i, &h) in tree.shapes.iter().enumerate() {
        visited.restore_shape(i as u32, h)?;
    }
    visited.keys = tree.space.clone();
    let mut frontier = Frontier::<N>::default();
    // The emissions' skeletons: every frame file's from frame 1 on (a
    // layer's are the registry's; frame 0's are the start's reference rows').
    let skeletons = crate::storage::decode::Skeletons::<N>::default();
    let mut batch: Vec<FrameIn<N>> = Vec::new();
    for f in 0..=last {
        for r in tree.file(f).filter(|_| f > 0).iter().flat_map(|x| x.header.skeletons.iter().chain(&x.header.emitted)) {
            skeletons.register(&r.rt2_in(room))?;
        }
        // Past a frontier that died: no frame.
        let Some(file) = tree.file(f) else { continue };
        let (new, _) = tree.layer(f, (f == last).then_some(room), threads)?;
        let (hulls, mut grown) = file.frontier(threads).with_context(|| format!("{}: frame {f}'s frontier", dir.display()))?;
        let layer = u16::try_from(f).map_err(|_| anyhow::anyhow!("frame {f}: past a u16 layer"))?;
        anyhow::ensure!(grown.iter().all(|g| g.layer < layer), "{}: a grown state of a layer past frame {f} in its frontier", dir.display());
        if f == last {
            // Its new states with the batch; then its old states, with what
            // they held before it, merged on their own.
            let mut frames = std::mem::take(&mut batch);
            frames.push(FrameIn { layer, new, hulls, grown: Vec::new() });
            absorb_frames(&mut visited, &frames, threads).with_context(|| dir.display().to_string())?;
            let fr = frames.pop().expect("pushed");
            let old = old_layers(&visited, &grown).with_context(|| format!("{}: frame {f}'s frontier", dir.display()))?;
            grown.sort_by_key(|g| g.sk);
            let grown_in = FrameIn { layer, new: crate::storage::layer::Layer::default(), hulls: Vec::new(), grown };
            absorb_frames(&mut visited, std::slice::from_ref(&grown_in), threads).with_context(|| dir.display().to_string())?;
            frontier = Frontier { new: fr.new, hulls: fr.hulls, old };
            break;
        }
        // By state: a stable sort, which takes the file's runs (by layer,
        // then state) as they are.
        grown.sort_by_key(|g| g.sk);
        batch.push(FrameIn { layer, new, hulls, grown });
        if batch.iter().map(|b| b.hulls.len() + b.grown.len()).sum::<usize>() >= RESUME_STATES {
            absorb_frames(&mut visited, &std::mem::take(&mut batch), threads).with_context(|| dir.display().to_string())?;
        }
    }
    // Frames before a frontier that died before `last`.
    absorb_frames(&mut visited, &batch, threads).with_context(|| dir.display().to_string())?;
    anyhow::ensure!(visited.infos() <= visited.len() as u64, "{}: {} states, {} with a hull stored", dir.display(), visited.len(), visited.infos());
    Ok((visited, tree.first_win(), frontier, skeletons))
}

/// A frame's grown states (`grown`, by layer and state) as a frontier's old
/// states, each with the hull it held before the frame (`visited`'s, which
/// holds every frame before it): `was`.
fn old_layers<N: Rem>(visited: &crate::storage::visited::VisitedSet<N>, grown: &[crate::storage::hulls::Grown<N>]) -> Result<crate::storage::wave::ByLayer<N>> {
    use crate::storage::wave::{Old, OldLayer};
    // Read by region, each region's walked with its states sorted.
    let mut by: Vec<(u32, crate::storage::visited::Key, u8, usize)> = grown.iter().enumerate().map(|(i, g)| (g.sk.region, g.sk.key, g.sk.local, i)).collect();
    by.sort_unstable();
    let mut was: Vec<Option<crate::storage::visited::StateInfo<N>>> = vec![None; grown.len()];
    for run in by.chunk_by(|a, b| a.0 == b.0) {
        let Some(t) = visited.table(run[0].0) else { continue };
        let mut c = t.cursor::<N>();
        for &(_, key, local, i) in run {
            was[i] = c.seek(key)?.and_then(|e| e.info(local as u32));
        }
    }
    let mut old: crate::storage::wave::ByLayer<N> = Vec::new();
    for (g, was) in grown.iter().zip(was) {
        let i = was.ok_or_else(|| anyhow::anyhow!("an old state without a hull before"))?;
        anyhow::ensure!(i.layer == g.layer, "an old state's layer {} in the hulls, {} before", g.layer, i.layer);
        if old.last().is_none_or(|x| x.layer != g.layer) {
            // Each layer's room exactly (the grown states are by layer).
            let n = grown.partition_point(|x| x.layer <= g.layer) - grown.partition_point(|x| x.layer < g.layer);
            old.push(OldLayer::new(g.layer, n));
        }
        old.last_mut().expect("pushed").push(&Old { sk: g.sk, layer: g.layer, hull: g.hull, was: i.hull });
    }
    Ok(old)
}

/// One row of a marks file: `(shape, cell, key.0, key.1, dist)`.
pub(crate) type MarkRow = (u64, u32, u64, u64, u32);

/// The marks-file row of a state with `deadline` (`u16::MAX`: none, saved
/// as 0).
pub(crate) fn mark_row(shape: u64, key: (u64, u64), cell: u32, deadline: u16, horizon: u32) -> MarkRow {
    (shape, cell, key.0, key.1, horizon - (deadline as u32).min(horizon))
}

/// A marks file (`Visited::save`) from its rows, in any order, each state
/// once, with the key space its keys are in.
pub(crate) fn save_marks<N: Arithmetic>(path: &std::path::Path, mut rows: Vec<MarkRow>, horizon: u32, keys: &celeste_engine::exact::KeySpace<N>) -> Result<()> {
    rows.sort_unstable();
    crate::storage::file::save_value::<N, _>(path, &(rows, horizon, keys.to_additions()))
}

/// A marks file's rows, horizon and key space, in arithmetic `N` (a file of
/// another is refused: its magic is its arithmetic's).
pub(crate) fn load_marks<N: Arithmetic>(path: &std::path::Path) -> Result<(Vec<MarkRow>, u32, celeste_engine::exact::KeySpace<N>)> {
    let (rows, horizon, adds): (Vec<MarkRow>, u32, celeste_engine::exact::KeyAdditions<N>) = crate::storage::file::load_value::<N, _>(path)?;
    Ok((rows, horizon, celeste_engine::exact::KeySpace::from_additions(adds).with_context(|| format!("{}: its key space", path.display()))?))
}

/// An in-memory set of states `(shape, cell, key)` (a level's MARKED states
/// or a diagnostic's), saved as a marks file; its keys are in the key space
/// of the tree it names.
pub(crate) struct Visited<N: Arithmetic = P8> {
    /// Sharded by (shape, cell); each key maps to its DEADLINE, the last frame
    /// from which it still wins by the horizon (`u16::MAX`: none).
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashMap<(u64, u64), u16>>,
    keys: celeste_engine::exact::KeySpace<N>,
}

impl<N: Arithmetic> Default for Visited<N> {
    fn default() -> Self {
        Visited { shards: Default::default(), keys: Default::default() }
    }
}

impl<N: Arithmetic> Visited<N> {
    /// An empty set over the key space `keys`.
    pub(crate) fn new(keys: celeste_engine::exact::KeySpace<N>) -> Self {
        Visited { shards: Default::default(), keys }
    }
    pub(crate) fn keys(&self) -> &celeste_engine::exact::KeySpace<N> {
        &self.keys
    }
    /// `(entries, order-independent CONTENT hash of the (shape, key, cell)
    /// set)` (`exact::ContentHash`).
    #[cfg(test)]
    pub(crate) fn fingerprint(&self) -> (usize, u64) {
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
    pub(crate) fn filter_fingerprint(&self) -> u64 {
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
    #[cfg(test)]
    pub(crate) fn insert(&mut self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.insert_until(shape, key, cell, u16::MAX)
    }
    /// `insert` with a deadline; a key inserted twice keeps the later one.
    pub(crate) fn insert_until(&mut self, shape: u64, key: (u64, u64), cell: u32, deadline: u16) -> bool {
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
    pub(crate) fn contains(&self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.shards.get(&(shape, cell)).is_some_and(|s| s.contains_key(&key))
    }
    /// The key's deadline, `None` if absent.
    pub(crate) fn deadline(&self, shape: u64, key: (u64, u64), cell: u32) -> Option<u16> {
        self.shards.get(&(shape, cell)).and_then(|s| s.get(&key)).copied()
    }
    pub(crate) fn len(&self) -> usize {
        self.shards.values().map(|s| s.len()).sum()
    }
}

impl<N: Arithmetic> Visited<N> {
    /// THE MARKS FILE (also read by the UI export): `(rows, horizon, key
    /// space)`, `dist` = horizon - deadline; no deadline saves as 0 (filters
    /// alike), under its arithmetic's magic.
    #[cfg(test)]
    pub(crate) fn save(&self, path: &std::path::Path, horizon: u32) -> Result<()> {
        let mut rows: Vec<MarkRow> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            rows.extend(keys.iter().map(|(&key, &d)| mark_row(*shape, key, *cell, d, horizon)));
        }
        save_marks(path, rows, horizon, &self.keys)
    }

    /// A marks file (`save`) of arithmetic `N`.
    pub(crate) fn load(path: &std::path::Path) -> Result<Self> {
        let (rows, horizon, keys) = load_marks::<N>(path)?;
        let mut out = Self::new(keys);
        for (shape, cell, k0, k1, dist) in rows {
            anyhow::ensure!(dist <= horizon, "{}: a mark {dist} frames before h{horizon}", path.display());
            out.insert_until(shape, (k0, k1), cell, u16::try_from(horizon - dist).unwrap_or(u16::MAX));
        }
        Ok(out)
    }
}

/// The first win of a tree complete through `horizon`; `None` if it is not.
pub(crate) fn tree_first_win_through<N: Arithmetic>(dir: &std::path::Path, horizon: u32) -> Result<Option<Option<u32>>> {
    let done = crate::storage::record::done_frame(dir);
    if done.is_none_or(|d| d < horizon) || !crate::storage::layer::frame_exists(dir, horizon) {
        return Ok(None);
    }
    Ok(Some(crate::storage::layer::Tree::<N>::open(dir, horizon)?.first_win()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abstraction::Level;
    use crate::compiled::asm_kernel::Registry;
    use crate::engines::reference::{RefEngine, RefFrameStep};
    use crate::search::hull::Hull;
    use crate::settings::Settings;
    use crate::storage::layer::Layer;
    use celeste_core::pico8_num::Pico8Num as P8;
    use celeste_engine::runtime2::{Col, AV};

    /// The start room's initial block.
    fn start_blocks(s: &Settings) -> Vec<Rt2> {
        vec![RefEngine::new(s).expect("ref engine").initial().expect("initial state").canonicalized()]
    }

    /// The start's row a few frames after the player appears (standing, at
    /// rest), widened to `level`.
    fn standing_row(s: &Settings, level: Level) -> Rt2 {
        let mut reference = RefEngine::new(s).expect("ref engine");
        let mut row = reference.initial().expect("initial state");
        let mut after = 0;
        while after < 3 {
            row = reference.step_one(&row, 0).expect("frame");
            if !row.player_objects().is_empty() {
                after += 1;
            }
        }
        row.widen_to(level);
        row
    }

    /// Does `registry` run `r`'s one lane (else its kernel declines), keyed
    /// against `visited`'s key space?
    fn kernel_runs(registry: &Registry<P8>, r: &Rt2, s: &Settings, visited: &crate::storage::visited::VisitedSet<P8>) -> bool {
        let cells = crate::storage::cells::block_cells(r).expect("cells");
        let (routes, skeletons) = (crate::storage::route::Routes::default(), crate::storage::decode::Skeletons::default());
        let prov = celeste_engine::exact::Provisional::default();
        let (base, xtab) = (crate::storage::edges::XferTable::default(), Default::default());
        let mut sink = crate::storage::unit::UnitSink::new(visited, &routes, &skeletons, &prov, Filters::default(), 1, 0, &base, &xtab, s);
        sink.begin(0);
        let si = sink.push_source(0, (0, 0));
        sink.push_lane(si, cells[0], Hull::full());
        sink.sources_done();
        registry.run_chunk(r, &cells, &[0], &mut sink)
    }

    /// The mark filter admits a marked node's rows up to its deadline only.
    #[test]
    fn the_mark_filter_admits_up_to_the_deadline() {
        let s = Settings::default();
        let block = start_blocks(&s).pop().expect("a block");
        let coarse = Level::parse("r0sx").expect("level");
        // The coarse key space holds the projection's codes.
        let mut w = block.clone_block();
        w.widen_to(coarse);
        w.canonical();
        let mut space = celeste_engine::exact::KeySpace::new();
        space.absorb(&[&w]);
        let (shape, keys, cells) = widened_keys(&block, coarse, &space).expect("keys");
        let mut marked = Visited::new(space.clone());
        marked.insert_until(shape, keys[0].expect("keyed"), cells[0], 5);
        let f = MarkFilter::new(&marked, coarse, crate::settings::Clock { per_frame: 1 });
        assert_eq!(f.allowed(&block, 5).expect("filter"), vec![true], "at the deadline");
        assert_eq!(f.allowed(&block, 6).expect("filter"), vec![false], "past it");
        assert_eq!(MarkFilter::new(&Visited::new(Default::default()), coarse, crate::settings::Clock { per_frame: 1 }).allowed(&block, 0).expect("filter"), vec![false], "a key space without its codes");
        let empty = Visited::new(space);
        assert_eq!(MarkFilter::new(&empty, coarse, crate::settings::Clock { per_frame: 1 }).allowed(&block, 0).expect("filter"), vec![false], "not marked");
    }

    /// A fresh forward keeps none of a killed run's files.
    #[test]
    fn a_fresh_forward_clears_what_a_killed_run_left() {
        let s = Settings::default();
        let init = start_blocks(&s);
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-fresh-start-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        let stale = [dir.join("edges/f009.bin"), dir.join("edges/xfer.bin"), dir.join("frames/f009.bin")];
        for f in &stale {
            std::fs::create_dir_all(f.parent().unwrap()).expect("mkdir");
            std::fs::write(f, b"stale").expect("write");
        }
        ForwardState::start(init, dir, 0, None, Level::EXACT, &s).expect("start");
        for f in &stale {
            assert!(!f.exists(), "{} survived a fresh start", f.display());
        }
        assert!(crate::storage::layer::frame_exists(dir, 0), "frame 0 checkpointed");
        let _ = std::fs::remove_dir_all(dir);
    }

    /// THE HULL FORWARD LIES BETWEEN THE EXACT REACH AND THE WIDENED FORWARD,
    /// room (1,0) to f34: with every hull widened to the whole circle
    /// (`hull::WIDEN`) the frames are the pinned widened ones
    /// (gates/ckhash_room10_f000-034_widened.txt); the hull forward keeps a
    /// SUBSET of those states, each in a layer no earlier than the widened
    /// one's; and it holds every state the exact-remainder reach
    /// (`search::reach`) reaches, in a layer no later than the frame it is
    /// reached at, with every remainder the reach brings it in its hull.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels and two forwards, ~30 CPU-s"]
    fn the_hull_forward_lies_between_the_exact_reach_and_the_widened_forward() {
        use crate::search::hull::WIDEN;
        use std::sync::atomic::Ordering;
        let s = Settings { threads: Some(8), ..Default::default() };
        let base = crate::testutil::TestDir::new("hull-between");
        let level = Level::parse("r0sx").expect("level");
        let e = crate::compiled::FrameEngine::new(&s, level).expect("kernels");
        let init = || start_blocks(&s);
        let to = 34;
        // Per frame, its states by content.
        let states = |d: &std::path::Path| -> Vec<rustc_hash::FxHashSet<u64>> {
            let t = crate::storage::layer::Tree::<P8>::open(d, to).expect("tree");
            let content = t.space.content_hash();
            (0..=to).map(|f| t.layer(f, None, 4).expect("layer").0.states_iter().map(|sk| { let (shape, key, cell) = t.state(sk).expect("state"); content.state(shape, key, cell) }).collect()).collect()
        };
        let (wide, hull) = (base.join("widened"), base.join("hull"));
        WIDEN.store(true, Ordering::Relaxed);
        ForwardState::start(init(), &wide, to, None, level, &s).expect("start").extend(&e, &wide, to, None).expect("widened");
        WIDEN.store(false, Ordering::Relaxed);
        ForwardState::start(init(), &hull, to, None, level, &s).expect("start").extend(&e, &hull, to, None).expect("hull");
        let t = crate::storage::layer::Tree::<P8>::open(&wide, to).expect("tree");
        let lines: Vec<String> = (0..=to).map(|f| { let (n, acc) = t.layer(f, None, 4).expect("layer").0.content_fingerprint(&t.space, &t.shapes).expect("fp"); format!("f{f:03} {n} {acc:016x}") }).collect();
        let pin = concat!(env!("CARGO_MANIFEST_DIR"), "/gates/ckhash_room10_f000-034_widened.txt");
        if std::env::var_os("CELESTE_REGENERATE").is_some() {
            std::fs::write(pin, lines.join("\n") + "\n").expect("write the pin");
        }
        let pinned = std::fs::read_to_string(pin).expect("the gate (CELESTE_REGENERATE=1 writes it)");
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
        let st = ForwardState::<P8>::at(&hull, to, level, &s).expect("at");
        let tree = crate::storage::layer::Tree::<P8>::open(&hull, to).expect("tree");
        let layers: Vec<Layer> = (0..=to).map(|f| tree.layer(f, None, 4).expect("layer").0).collect();
        let p0 = crate::search::rem::start_point::<P8>();
        let mut checked = 0u64;
        crate::search::reach::exact::reach(&hull, to, p0, 4, |t, cur| {
            for (sk, r) in cur {
                let layer = (0..=t).find(|&f| layers[f as usize].contains(*sk)).unwrap_or_else(|| panic!("f{t}: an exactly reached state no layer holds"));
                // (A full hull is not stored: it holds everything.)
                if let Some(info) = st.visited().state(sk.region, sk.key, sk.local as u32) {
                    assert_eq!(info.layer as u32, layer, "f{t}: the state's recorded layer");
                    for (y, xs) in r.as_ref().slabs() {
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
    }

    /// A tree's level -1 line round-trips; without one, or garbled, it is an
    /// error (the filter unknown); a filtered tree without its table is
    /// refused.
    #[test]
    fn a_trees_level_minus_one_record_round_trips() {
        let dir = crate::testutil::TestDir::new("tree-filter");
        let set = |v: &str| crate::storage::record::update(&dir, TreeL1::KEY, v).expect("write");
        assert!(TreeL1::recorded(&dir).is_err(), "no record: unknown");
        assert!(TreeL1::read(&dir).is_err(), "a check refuses an unknown filter");
        set(&TreeL1::text(None));
        assert!(TreeL1::read(&dir).expect("off").is_none(), "a check of an unfiltered tree applies no level -1");
        let filtered = Some((MinusOne { h: 186, speed: 5 }, 0x0123_4567_89ab_cdef));
        for f in [None, filtered] {
            set(&TreeL1::line(f));
            assert_eq!(TreeL1::recorded(&dir).expect("read"), f);
        }
        assert_eq!(TreeL1::line(filtered), "h 186 speed 5 table 0123456789abcdef");
        assert!(TreeL1::read(&dir).is_err(), "a filtered tree without its table is refused");
        set("116");
        assert!(TreeL1::recorded(&dir).is_err(), "a bare horizon is an error");
        set("h 3 speed");
        assert!(TreeL1::recorded(&dir).is_err(), "a garbled line is an error");
    }

    /// A marks file round-trips deadlines (none comes back as the horizon).
    #[test]
    fn marks_files_keep_their_deadlines() {
        let path = std::env::temp_dir().join(format!("celeste-marks-roundtrip-{}.bin", std::process::id()));
        // A key space with shapes 1 and 9, two codes in one field each.
        let mut space = celeste_engine::exact::KeySpace::<P8>::new();
        let mut adds = Default::default();
        for shape in [1, 9] {
            space.add_shape(&celeste_engine::exact::ShapeRecord { hash: shape, sig: vec![1, u32::MAX], pos: None, deps: Vec::new() }, &mut adds);
            space.add_codes(shape, 0, &[celeste_engine::exact::Code::num(0, 0), celeste_engine::exact::Code::num(1, 1)], &mut adds);
        }
        let mut v = Visited::new(space);
        v.insert_until(1, (0, 0), 4, 7);
        v.insert_until(1, (0, 1), 4, 12);
        v.insert(9, (0, 0), 8);
        v.save(&path, 12).expect("save");
        let w = Visited::<P8>::load(&path).expect("load");
        std::fs::remove_file(&path).expect("rm");
        assert_eq!(w.fingerprint(), v.fingerprint());
        assert_eq!(w.deadline(1, (0, 0), 4), Some(7));
        assert_eq!(w.deadline(1, (0, 1), 4), Some(12));
        assert_eq!(w.deadline(9, (0, 0), 8), Some(12), "no deadline: the horizon");
    }

    /// A MARKS FILE IS IN ITS TREE'S ARITHMETIC: a float tree's loads as
    /// float, its key space intact, and is REFUSED as 16.16 (its codes are
    /// doubles' raws: read as 16.16 they would key other states); a 16.16
    /// file is refused as float.
    #[test]
    fn a_marks_file_loads_in_its_own_arithmetic_alone() {
        use celeste_core::arith::{Fixed, Float};
        use celeste_engine::exact::{Code, KeySpace, ShapeRecord};
        let path = std::env::temp_dir().join(format!("celeste-marks-arith-{}.bin", std::process::id()));
        let mut space = KeySpace::<Float>::new();
        let mut adds = Default::default();
        space.add_shape(&ShapeRecord { hash: 1, sig: vec![1, u32::MAX], pos: None, deps: Vec::new() }, &mut adds);
        space.add_codes(1, 0, &[Code::<Float>::num(0, 0), Code::<Float>::num(1.5f64.to_bits(), 2.5f64.to_bits())], &mut adds);
        let mut v = Visited::<Float>::new(space);
        v.insert_until(1, (0, 1), 4, 7);
        v.save(&path, 12).expect("save");
        let w = Visited::<Float>::load(&path).expect("a float marks file loads as float");
        assert_eq!((w.fingerprint(), w.deadline(1, (0, 1), 4)), (v.fingerprint(), Some(7)));
        let e = Visited::<Fixed>::load(&path).err().expect("a float marks file loaded as 16.16");
        assert!(e.to_string().contains("not a value file in fixed"), "{e:#}");
        let fixed = Visited::<Fixed>::new(KeySpace::new());
        fixed.save(&path, 12).expect("save");
        assert!(Visited::<Float>::load(&path).is_err(), "a 16.16 marks file loaded as float");
        std::fs::remove_file(&path).expect("rm");
    }

    /// A resume (`ForwardState::at`) takes the frontier the uninterrupted
    /// forward expands, at every frame. 100% in room (1,0), which has no
    /// berry: every row's berry is lost from frame 0 on.
    #[test]
    fn a_resume_expands_what_the_uninterrupted_forward_expands() {
        let s = Settings { category: crate::settings::Category::Hundred, ..Default::default() };
        let dir = crate::testutil::TestDir::new("resume-frontier");
        let init = start_blocks(&s);
        let e = RefFrameStep::<P8>::new(&s, Level::EXACT).expect("engines");
        let frontier = |st: &ForwardState| -> (Vec<(u32, (u64, u64), u64)>, Vec<Hull>, crate::storage::wave::ByLayer) { (st.frontier.new.iter().collect(), st.frontier.hulls.clone(), st.frontier.old.clone()) };
        let mut st = ForwardState::start(init, &dir, 3, None, Level::EXACT, &s).expect("start");
        for b in crate::storage::layer::decode_states(&st.frontier.new, &st.visited.keys, st.visited.shapes(), usize::MAX, &|_| true).expect("decode") {
            assert!(not_expanded(&b.1, 0, None, &s).expect("not expanded").iter().all(|&x| x), "frame 0: the berry is lost");
        }
        for to in 0..=3 {
            st.extend(&e, &dir, to, None).expect("extend");
            let resumed = ForwardState::at(&dir, st.frames, Level::EXACT, &s).expect("at");
            assert_eq!(frontier(&st), frontier(&resumed), "f{}: the resume's frontier is not the forward's", st.frames);
            assert_eq!(st.win_frame, resumed.win_frame, "f{}: the first win", st.frames);
        }
        assert_eq!(st.frames, 1, "nothing expanded past frame 0");
    }

    /// A run killed after a frame's files but before its done marker
    /// resumes to the uninterrupted tree: the frame is discarded and rerun,
    /// the transfers past the trusted length are overwritten. The one marker holds the frame
    /// and the transfer count; nothing else claims trust.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels and three forwards, ~25 CPU-s"]
    fn a_frame_killed_before_its_done_marker_is_rerun() {
        let s = Settings::default();
        let base = crate::testutil::TestDir::new("done-marker");
        let (a, b) = (base.join("uninterrupted"), base.join("killed"));
        // The kernels (they record transfers), past the spawn.
        let level = Level::parse("r0sx").expect("level");
        let e = crate::compiled::FrameEngine::new(&s, level).expect("kernels");
        let start = RefEngine::new(&s).expect("engine").initial().expect("init");
        let init = || vec![start.clone_block().canonicalized()];
        let ends = 30;
        ForwardState::start(init(), &a, ends, None, level, &s).expect("start").extend(&e, &a, ends, None).expect("extend");
        let mut st = ForwardState::start(init(), &b, ends, None, level, &s).expect("start");
        st.extend(&e, &b, ends - 1, None).expect("extend");
        let record = crate::storage::record::path(&b);
        let marker = std::fs::read_to_string(&record).expect("the record");
        assert_eq!(crate::storage::record::done(&b).expect("done"), Some(crate::storage::record::Done { frame: ends - 1, xfers: st.xfers.pairs.len() }));
        st.extend(&e, &b, ends, None).expect("extend");
        // Killed just before frame `ends`' marker.
        std::fs::write(&record, &marker).expect("rewind the marker");
        for d in [&a, &b] {
            assert!(!d.join("edges/xfer.len").exists(), "a second marker");
        }
        let mut st = ForwardState::resume(&b, level, &s).expect("resume").expect("a tree");
        assert_eq!(st.frames, ends - 1, "the untrusted frame is discarded");
        st.extend(&e, &b, ends, None).expect("rerun");
        for f in 0..=ends {
            let keys = |d: &std::path::Path| -> Vec<(u32, (u64, u64), u64)> { crate::storage::layer::Tree::<P8>::open(d, ends).expect("tree").layer(f, None, 2).expect("layer").0.iter().collect() };
            assert_eq!(keys(&a), keys(&b), "f{f}");
        }
        assert!(st.xfers.pairs.len() > 1, "transfers recorded");
        for file in ["edges/xfer.bin", crate::storage::record::FILE] {
            assert_eq!(std::fs::read(a.join(file)).expect("a"), std::fs::read(b.join(file)).expect("b"), "{file}");
        }
    }

    /// Extending frame by frame and resuming reproduce a fresh run's key sets.
    #[test]
    fn forward_extended_frame_by_frame_matches_fresh() {
        let s = Settings::default();
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-rebuild-resume-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        // A frame's states by content (their keys are their tree's).
        let content = |d: &std::path::Path, frame: u32| -> (u64, u64) {
            let t = crate::storage::layer::Tree::<P8>::open(d, frame).expect("tree");
            t.layer(frame, None, 2).expect("layer").0.content_fingerprint(&t.space, &t.shapes).expect("content")
        };
        let keyset = |frame: u32| content(dir, frame);

        forward_run(&RefFrameStep::<P8>::new(&s, Level::EXACT).expect("engines"), start_blocks(&s), dir, 4).expect("fresh");
        let fresh4 = keyset(4);
        assert!(fresh4.0 > 0, "fresh frame 4 empty");

        // The same run extended one frame at a time.
        {
            let e = RefFrameStep::<P8>::new(&s, Level::EXACT).expect("engines");
            let mut st = ForwardState::start(start_blocks(&s), dir, 4, None, Level::EXACT, &s).expect("start");
            for to in 1..=4 {
                st.extend(&e, dir, to, None).expect("extend");
                assert_eq!(st.frames, to);
                assert_eq!(crate::storage::record::done_frame(dir), Some(to), "frame {to} trusted");
            }
        }
        let extended4 = keyset(4);
        assert_eq!(fresh4, extended4, "extension diverged from fresh at frame 4");
        eprintln!("[extend] frame 4 states identical: {}", fresh4.0);

        // RESUME: a fresh run to 6 against the tree above resumed at 4.
        let fresh_dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-rebuild-resume-test-fresh6-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(fresh_dir);
        std::fs::create_dir_all(fresh_dir).expect("mkdir");
        forward_run(&RefFrameStep::<P8>::new(&s, Level::EXACT).expect("engines"), start_blocks(&s), fresh_dir, 6).expect("fresh 6");
        {
            let e = RefFrameStep::<P8>::new(&s, Level::EXACT).expect("engines");
            let mut st = ForwardState::resume(dir, Level::EXACT, &s).expect("resume").expect("a tree to resume");
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
    /// runs.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels, ~20 CPU-s"]
    fn a_lane_outside_its_kernels_bounds_declines() {
        let s = Settings::default();
        let level = Level::parse("r0sx").expect("level");
        let grid = s.region_grid().expect("the default region grid");
        let registry = crate::compiled::asm_kernel::registry(level, &s, false);
        let row = standing_row(&s, level);
        let ids = crate::compiled::ids();
        let with = |f: u32, axis: u32, v: P8| with_player_field(&row, f, axis, v);
        let runs = |r: &Rt2| -> bool {
            let cells = crate::storage::cells::block_cells(r).expect("cells");
            assert!(grid.of_cell(cells[0]).is_some(), "the lane has a region");
            kernel_runs(&registry, r, &s, &crate::storage::visited::VisitedSet::new())
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

    /// `row` with its player's table field `f`'s `axis` set to `v`; a number
    /// column or an interval one, as it was.
    fn with_player_field(row: &Rt2, f: u32, axis: u32, v: P8) -> Rt2 {
        let mut r = row.clone_block();
        let pc = r.obj_field_cell(r.player_objects()[0], f).expect("the player's table field");
        let Col::U(AV::Ptr(t)) = r.cols[pc as usize] else { panic!("the player's field is not a table") };
        let c = r.obj_field_cell(t, axis).expect("its axis") as usize;
        r.cols[c] = match r.cols[c] {
            Col::U(AV::Ival(..)) | Col::I(_) => Col::U(AV::Ival(v, v)),
            _ => Col::U(AV::Num(v)),
        };
        r
    }

    /// A DECLINE EXPLAINED (`check::explain`, `rewrite explain`): the row
    /// whose `spd.x` lies past its region kernel's bound
    /// (`a_lane_outside_its_kernels_bounds_declines`), its kernel rebuilt and
    /// evaluated on it, is declined by a body on a failing premise that is
    /// the bound's check, named by its `Restrict`; the row at the bound is
    /// declined by none.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels rebuilt twice, ~30 CPU-s"]
    fn a_decline_is_explained_by_the_bound_it_fails() {
        let s = Settings::default();
        let level = Level::parse("r0sx").expect("level");
        let grid = s.region_grid().expect("the default region grid");
        let (row, ids) = (standing_row(&s, level), crate::compiled::ids());
        let at = P8::from_i16(grid.speed as i16);
        let past = at.checked_add(P8::from_parts(0, 0x8000)).expect("small");
        let text = crate::check::explain::explain_row::<P8>(level, &s, &with_player_field(&row, ids.f_spd, ids.f_x, past)).expect("explained");
        assert!(text.contains("declines the row") && text.contains("failing premise"), "{text}");
        assert!(text.lines().any(|l| l.contains("the bound it checks") && l.contains("Restrict")), "no Restrict premise named:\n{text}");
        let fine = crate::check::explain::explain_row::<P8>(level, &s, &with_player_field(&row, ids.f_spd, ids.f_x, at)).expect("explained");
        assert!(fine.contains("no body declines the row"), "{fine}");
    }

    /// THE KERNELS' EMISSION CHECKS THE FOLLOWERS (`exact::Deps`): room
    /// (6,0)'s first three steps keyed (its ten platforms keyed by the first
    /// one's), the second step's row runs against its record; with the second
    /// platform's `x` (a follower of the first one's `rem.x`) recorded as
    /// another number at every leader index, the same step is FATAL at the
    /// emission.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (6,0)'s split kernels, ~40 CPU-s"]
    fn the_kernels_emission_checks_the_followers() {
        let s = Settings { room: (6, 0), split_frame: true, ..Default::default() };
        let level = Level::parse("r0sxf").expect("level");
        let registry = crate::compiled::asm_kernel::registry(level, &s, false);
        let mut reference = RefEngine::new(&s).expect("ref engine");
        let start = reference.initial().expect("start");
        let one = reference.step_one(&start, 0).expect("step 1");
        let two = reference.step_one(&one, 0).expect("step 2");
        let rows: Vec<Rt2> = [start, one, two]
            .into_iter()
            .map(|mut r| {
                r.widen_to(level);
                r.canonicalized()
            })
            .collect();
        let mut visited = crate::storage::visited::VisitedSet::new();
        crate::storage::wave::seed(&mut visited, &rows, Hull::full(), &s).expect("the steps keyed");
        assert!(kernel_runs(&registry, &rows[1], &s, &visited), "the second step runs against its own record");
        let ty = celeste_names::global("platform");
        let mut adds = visited.keys.to_additions();
        let mut corrupted = 0;
        for r in &rows {
            let platforms = r.objects_of_type(ty);
            let x = r.obj_field_cell(platforms[1], celeste_names::field("x")).expect("the second platform's x");
            let deps = celeste_engine::widening::key_deps(r);
            assert!(deps.len() == 2 && deps[1].followers.contains(&x), "the second platform's x follows the first one's rem.x: {deps:?}");
            for code in adds.codes.iter_mut().filter(|c| c.0 == r.shape_hash && c.1 == x) {
                code.3 = celeste_engine::exact::Code::of(AV::Num(P8::from_i16(1000)));
                corrupted += 1;
            }
        }
        assert!(corrupted > 0, "the follower's records");
        visited.keys = celeste_engine::exact::KeySpace::from_additions(adds).expect("the records replay");
        let fatal = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| kernel_runs(&registry, &rows[1], &s, &visited))).expect_err("a follower other than its record is FATAL");
        let msg = fatal.downcast_ref::<String>().cloned().unwrap_or_default();
        assert!(msg.contains("declared key dependency is VIOLATED") && msg.contains("the kernels' emission"), "{msg}");
    }

    /// A DYING EXIT IS NO WIN (plans/correctness.md C8), nor a witness's
    /// exit (`concrete::leaving`). Room (2,2)'s top row holds a right-facing
    /// spike (tile 59) at tile 8, beside the exit. A player at x 64 rising
    /// from y -3 at 2 px a frame is in its kill zone after its move (not
    /// moving left) and above y -4: the one update kills it and, on its
    /// table, loads room (3,2) with the restart pending. The same rise at x
    /// 40, clear of the spike, is the room's clean exit.
    #[test]
    fn a_dying_exit_is_no_win() {
        let s = Settings { room: (2, 2), ..Default::default() };
        let mut reference = RefEngine::new(&s).expect("ref engine");
        let mut st = reference.initial().expect("initial state");
        while st.player_objects().is_empty() {
            st = reference.step_one(&st, 0).expect("the spawn");
        }
        let ids = crate::compiled::ids();
        let num = |v: i16| Col::U(AV::Num(P8::from_i16(v)));
        let mut rise = |x: i16| {
            let mut b = st.clone_block();
            let p = b.player_objects()[0];
            for (f, v) in [(ids.f_x, x), (ids.f_y, -3)] {
                let c = b.obj_field_cell(p, f).expect("a position field");
                b.cols[c as usize] = num(v);
            }
            for (f, (vx, vy)) in [(ids.f_spd, (0, -2)), (ids.f_rem, (0, 0))] {
                let (cx, cy) = b.player_xy_cells(f).expect("a vector field");
                (b.cols[cx], b.cols[cy]) = (num(vx), num(vy));
            }
            reference.step_one(&b, 0).expect("one concrete successor")
        };
        let dying = rise(64);
        assert_eq!(exits_of(&dying, &s).expect("exits"), [true], "it leaves the room");
        assert_eq!(died(&dying).expect("died"), [true], "it died in that update");
        assert_eq!(wins_of(&dying, &s).expect("wins"), [false], "a dying exit is no win");
        assert_eq!(not_expanded(&dying, 1, None, &s).expect("not expanded"), [true], "nor is it expanded");
        assert_eq!(crate::concrete::leaving(&dying, &s).expect("leaving"), Some(false), "nor is it a witness's exit");
        let clean = rise(40);
        assert_eq!(died(&clean).expect("died"), [false]);
        assert_eq!(wins_of(&clean, &s).expect("wins"), [true], "the clean exit wins");
        assert_eq!(crate::concrete::leaving(&clean, &s).expect("leaving"), Some(true), "and is a witness's exit");
        assert_eq!(crate::concrete::leaving(&st, &s).expect("leaving"), None, "the player in the room has not left it");
    }

    /// THE ORB DEADLINE AGAINST THE CART (plans/correctness.md C2), in
    /// frames, unsplit and split: from the big chest's opening to the first
    /// update a player with the orb runs is at least
    /// `ORB_MIN_FRAMES_AFTER_CHEST`. Room (5,2)'s any% witness is replayed to
    /// the frame its chest opens; from there, with no input (nothing
    /// freezes, so every draw ticks the chest's timer and the orb's rise:
    /// the shortest), to the frame the orb is collectable (`spd.y` 0: its
    /// draw takes it from then on). The witness's own collection gives the
    /// freeze: its frames to the first update that runs. A win (the exit, in
    /// the player's update) comes no sooner than the two together.
    #[test]
    fn the_orb_deadline_is_below_the_carts_chest_to_orb_time() {
        let ids = crate::compiled::ids();
        let num = |b: &Rt2, c: u32| match b.cols[c as usize].at(0) {
            AV::Num(n) => n,
            other => panic!("not a number: {other:?}"),
        };
        let global = |b: &Rt2, name: &str| num(b, b.globals[celeste_names::global(name) as usize]);
        let chest_state = |b: &Rt2| b.objects_of_type(ids.g_big_chest).first().map(|&o| num(b, b.obj_field_cell(o, ids.f_state).expect("a state")));
        let orb_still = |b: &Rt2| {
            b.objects_of_type(celeste_names::global("orb")).iter().any(|&o| {
                let AV::Ptr(spd) = b.cols[b.obj_field_cell(o, ids.f_spd).expect("spd") as usize].at(0) else { panic!("the orb's spd is no table") };
                num(b, b.obj_field_cell(spd, ids.f_y).expect("spd.y")) == P8::from_i16(0)
            })
        };
        let inputs = crate::concrete::read_inputs("tas/room_5_2_exit_frame_150.txt").expect("the witness");
        let mut bounds = Vec::new();
        for split_frame in [false, true] {
            let s = Settings { room: (5, 2), split_frame, ..Default::default() };
            let mut eng = RefEngine::new(&s).expect("ref engine");
            let frame = |eng: &mut RefEngine, b: &Rt2, input: u8| {
                let mut leaves = eng.frame(b, input).expect("a frame");
                assert_eq!(leaves.len(), 1, "one leaf");
                leaves.remove(0)
            };
            let (mut st, mut opened, mut taken, mut exit) = (eng.initial().expect("the start"), None, None, None);
            let mut frozen_to = None;
            for (i, &b) in inputs.iter().enumerate() {
                let f = i as u32 + 1;
                st = frame(&mut eng, &st, b);
                if opened.is_none() && chest_state(&st) == Some(P8::from_i16(1)) {
                    opened = Some((f, st.clone_block()));
                }
                if taken.is_none() && global(&st, "max_djump") == P8::from_i16(2) {
                    taken = Some(f);
                }
                // The last frame the collection's freeze holds.
                if taken.is_some() && frozen_to.is_none() && global(&st, "freeze") == P8::from_i16(0) {
                    frozen_to = Some(f);
                }
                if exits_of(&st, &s).expect("exits")[0] {
                    exit = Some(f);
                    break;
                }
            }
            let (open, mut st) = opened.expect("the chest opens");
            let (taken, frozen_to, exit) = (taken.expect("the orb is taken"), frozen_to.expect("the freeze ends"), exit.expect("the witness exits"));
            let mut still = 0;
            while !orb_still(&st) {
                st = frame(&mut eng, &st, 0);
                still += 1;
                assert!(still < 200, "no orb comes to rest");
            }
            // The first update after the freeze is frame `frozen_to + 1`.
            let bound = still + (frozen_to + 1 - taken);
            assert!(exit - open >= bound, "split {split_frame}: the witness exits {} frames after the opening, under the cart's {bound}", exit - open);
            assert!(ORB_MIN_FRAMES_AFTER_CHEST <= bound, "split {split_frame}: the deadline's {ORB_MIN_FRAMES_AFTER_CHEST} frames exceed the cart's {bound}: UNSOUND");
            bounds.push((still, frozen_to + 1 - taken));
        }
        // The orb appears in the chest's 61st draw after its opening and
        // rests in its own 8th draw, the first in that same frame (68); then
        // 10 frozen frames and the update after them (11): 79, the same
        // split and unsplit.
        assert_eq!(bounds, [(68, 11), (68, 11)]);
    }

    /// THE ORB CUT COUNTS FRAMES (plans/correctness.md C2): a closed chest
    /// after step k opens at the earliest in frame `frame_of(k + 1)`, so at
    /// horizon f100 the last closed state expanded is f29's unsplit (opening
    /// in f30, 30 + 70 = 100); split, its frame boundary (step 58) and the
    /// mid-frame step 59 inside f30 (still opening in f30), not step 60.
    #[test]
    fn the_orb_cut_counts_frames_under_the_split_frame() {
        let closed = RefEngine::new(&Settings { room: (5, 2), ..Default::default() }).expect("ref engine").initial().expect("the start");
        let skip = |step: u32, h: u32, per_frame: u32| orb_deadline_skip(&closed, step, h, crate::settings::Clock { per_frame })[0];
        assert!(!skip(29, 100, 1) && skip(30, 100, 1));
        assert!(!skip(58, 200, 2) && !skip(59, 200, 2) && skip(60, 200, 2));
    }

    /// The summit wins at the flag: the TAS31 reference wins at frame 55, not before.
    #[test]
    fn the_summit_reference_wins_at_the_flag() {
        let s = Settings { room: (6, 3), ..Default::default() };
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
            let won = wins_of(&next, &s).expect("wins")[0];
            assert_eq!(won, i + 1 == 55, "frame {}: won {won}", i + 1);
            st = next;
        }
    }

    /// THE SPLIT FRAME at a near level reaches the unsplit (pinned) state
    /// counts at every boundary, room (2,1) `r0sxn`. Guards nil-global
    /// removal and the mid-frame cut not widening floors the player reads.
    /// (A mid-frame state's hull merges its sources' and part b moves no
    /// remainder, so the boundaries' hulls are the unsplit frame's.)
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (2,1)'s split kernels and a forward, ~55 CPU-s"]
    fn split_frame_reaches_the_unsplit_frontier_at_a_near_level() {
        let s = Settings { room: (2, 1), split_frame: true, ..Default::default() };
        let level = Level::parse("r0sxn").expect("level");
        let dir = &std::path::PathBuf::from(format!("/var/tmp/celeste-frame-split-near-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("checkpoint dir");
        let engine = crate::compiled::FrameEngine::new(&s, level).expect("engine");
        let init = start_blocks(&s);
        // Unsplit, frame by frame: 1 state a frame through the spawn, then these.
        let unsplit: Vec<(u32, usize)> = (0..24).map(|f| (f, 1)).chain([(24, 27), (25, 191), (26, 676), (27, 1883), (28, 4514)]).collect();
        let last = unsplit.last().unwrap().0;
        forward_run(&engine, init, dir, 2 * last).expect("split forward");
        let tree = crate::storage::layer::Tree::<P8>::open(dir, 2 * last).expect("the tree");
        for (f, want) in unsplit {
            let got = tree.states(2 * f) as usize;
            assert_eq!(got, want, "frame {f} (step {}): the split frame reached {got} states, the unsplit {want}", 2 * f);
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
