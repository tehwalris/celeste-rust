//! THE FORWARD'S PRUNING (plans/architecture.md "The forward: hulls and
//! storage", "Checks"): every row the forward drops or leaves unexpanded is
//! one `Prune`'s.
//! * FILTERS drop a row where it is made - an emission, a re-push, the
//!   frame's end - so at every step but the start: level -1 (`TreeL1`), the
//!   objects ladder (`MarkFilter`).
//! * CUTS keep a row and never expand it (no successors, no edges, never
//!   re-pushed): an exit (`Exit`), a row out of the room under a win
//!   rectangle (`OutOfRoom`), a 100% row whose berry is lost (`BerryLost`),
//!   an orb-room row whose chest opens too late (`OrbCut`).
//!
//! Each names its PROJECTION (what of the level's row it decides by), its
//! TREE-RECORD line and its HORIZON (`Prune`). `Prunes` holds a forward's,
//! each in its concrete form: the wave reads level -1 per lane at every
//! emission and the ladder in batches as fields (never through `dyn`), and
//! the cuts once a unit through `Prunes::not_expanded`. The known-route
//! check (`search::known`) holds a route against `Prunes::each`, the list
//! `not_expanded` cuts by and that names every field: a prune the forward
//! applies is one the check sees, by construction.
//!
//! The prunes are the FORWARD's alone: the backward, the reach and the
//! concrete search are exact or exhaustive inside W and read none.
#![warn(unreachable_pub)]

use anyhow::{Context, Result};

use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::Rt2;

use crate::frame::Visited;
use crate::settings::Settings;

/// What a prune does with the rows it refuses.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    /// Drops them where they are made (at every step but the start).
    Filter,
    /// Keeps them and never expands them.
    Cut,
}

/// What a prune is: its name in a report, its kind, and its PROJECTION:
/// what of the level's row (`Rt2::widen_to`, as the tree holds it) it
/// decides by.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct About {
    pub(crate) name: &'static str,
    pub(crate) kind: Kind,
    pub(crate) projection: &'static str,
}

/// ONE PRUNING STEP of the forward, as a check reads it (`Prunes::each`);
/// the forward applies each through its concrete form.
pub(crate) trait Prune<N: Arithmetic>: Sync {
    fn about(&self) -> About;
    /// Its line in the tree's record (`storage::record`; a setting's,
    /// `Settings::row`), `(key, value)`.
    fn record(&self, s: &Settings) -> (&'static str, String);
    /// Its horizon in steps (`None`: none). The tree's frames depend on it,
    /// so the tree serves horizons up to it (`frame::grow_tree`).
    fn horizon(&self) -> Option<u32> {
        None
    }
    /// Per lane of `rows` (step `step`'s rows of the level, as the tree
    /// holds them): does it prune the lane?
    fn prunes(&self, rows: &Rt2<N>, step: u32, s: &Settings) -> Result<Vec<bool>>;
    /// Why it prunes the one-lane `row` at `step` (a report's).
    fn why(&self, row: &Rt2<N>, step: u32, s: &Settings) -> Result<String>;
}

/// TESTS ONLY: a deliberately WRONG cut, `(name, step, cell)`: the cut named
/// `name` (`About::name`) also cuts the rows of step `step` at `cell`, in the
/// forward (`Prunes::not_expanded`) and in a check (`verdict`) alike, as a
/// bug in the cut would. Process-global: nextest runs each test in its own
/// process.
#[cfg(test)]
pub(crate) static FAULT: std::sync::Mutex<Option<(&'static str, u32, u32)>> = std::sync::Mutex::new(None);

/// Per lane of `rows`: does the TEST FAULT (`FAULT`) cut it under `p`?
#[cfg(test)]
fn faulted<N: Arithmetic>(p: &dyn Prune<N>, rows: &Rt2<N>, step: u32) -> Result<Vec<bool>> {
    let fault = *FAULT.lock().expect("the fault");
    Ok(match fault {
        Some((name, at, cell)) if name == p.about().name && at == step && p.about().kind == Kind::Cut => crate::storage::cells::block_cells(rows)?.into_iter().map(|c| c == cell).collect(),
        _ => vec![false; rows.width],
    })
}

/// Does `p` prune the one-lane `row` at `step`, and why (`None`: it keeps
/// it)? What the forward decides for the row's lane (`Prunes::not_expanded`
/// for a cut).
pub(crate) fn verdict<N: Arithmetic>(p: &dyn Prune<N>, row: &Rt2<N>, step: u32, s: &Settings) -> Result<Option<String>> {
    anyhow::ensure!(row.width == 1, "a prune's verdict on {} lanes", row.width);
    #[cfg(test)]
    if faulted(p, row, step)?[0] {
        return Ok(Some("a TEST FAULT (prune::FAULT)".to_string()));
    }
    Ok(if p.prunes(row, step, s)?[0] { Some(p.why(row, step, s)?) } else { None })
}

/// THE PRUNES a TREE records, which its frames depend on: level -1 and the
/// orb cut (the others follow from its settings and the ladder's marks).
#[derive(Clone, Copy, Default)]
pub(crate) struct Recorded {
    pub(crate) minus_one: Option<TreeL1>,
    pub(crate) orb: Option<OrbCut>,
}

impl Recorded {
    /// A new tree's in `dir`, for horizon `to` (steps) under `want` and `s`,
    /// recorded.
    pub(crate) fn start(dir: &std::path::Path, to: u32, want: Option<MinusOne>, s: &Settings) -> Result<Recorded> {
        Ok(Recorded { minus_one: TreeL1::start(want, dir, s)?, orb: OrbCut::start(to, dir, s)? })
    }

    /// The tree `dir`'s (`TreeL1::read`, `OrbCut::recorded`).
    pub(crate) fn read(dir: &std::path::Path) -> Result<Recorded> {
        Ok(Recorded { minus_one: TreeL1::read(dir)?, orb: OrbCut::recorded(dir)? })
    }
}

/// A FORWARD'S PRUNES under its settings: the tree's (`Recorded`), the
/// ladder's filter, the cuts the settings imply.
pub(crate) struct Prunes<'a, N: Arithmetic> {
    s: &'a Settings,
    /// The hot forms the wave reads: level -1 at every emission and re-push
    /// (`TreeL1::drops`), the ladder per batch (`MarkFilter::allowed`).
    pub(crate) minus_one: Option<TreeL1>,
    pub(crate) ladder: Option<&'a MarkFilter<'a, N>>,
    exit: Exit,
    out_of_room: Option<OutOfRoom>,
    berry: Option<BerryLost>,
    orb: Option<OrbCut>,
}

impl<'a, N: Arithmetic> Prunes<'a, N> {
    /// Under `s`: the tree's `tree`, the coarser level's marks `ladder`.
    pub(crate) fn of(s: &'a Settings, tree: Recorded, ladder: Option<&'a MarkFilter<'a, N>>) -> Self {
        Prunes {
            s,
            minus_one: tree.minus_one,
            ladder,
            exit: Exit,
            out_of_room: crate::frame::win_rect(s).is_some().then_some(OutOfRoom),
            berry: s.category.hundred().then_some(BerryLost),
            orb: tree.orb,
        }
    }

    /// EVERY PRUNE, filters first: each field named (a new one must be).
    pub(crate) fn each(&self) -> Vec<&dyn Prune<N>> {
        let Prunes { s: _, minus_one, ladder, exit, out_of_room, berry, orb } = self;
        let mut out: Vec<&dyn Prune<N>> = Vec::with_capacity(6);
        if let Some(p) = minus_one {
            out.push(p);
        }
        if let Some(p) = ladder {
            out.push(*p);
        }
        out.push(exit);
        if let Some(p) = out_of_room {
            out.push(p);
        }
        if let Some(p) = berry {
            out.push(p);
        }
        if let Some(p) = orb {
            out.push(p);
        }
        out
    }

    /// Per lane of a frame's rows: NOT expanded further, by some cut of
    /// `each`. The one place it is decided, on every unit's decoded rows
    /// (`wave::run_unit`), so a resume expands exactly what an uninterrupted
    /// run does. `step`: the rows' layer. A state not expanded has no edges,
    /// so it is never re-pushed; one expanded is re-pushed whatever a cut
    /// says later (the orb's deadline): sound, less pruned.
    pub(crate) fn not_expanded(&self, rows: &Rt2<N>, step: u32) -> Result<Vec<bool>> {
        let mut skip = vec![false; rows.width];
        for p in self.each().into_iter().filter(|p| p.about().kind == Kind::Cut) {
            for (w, cut) in skip.iter_mut().zip(p.prunes(rows, step, self.s)?) {
                *w |= cut;
            }
            #[cfg(test)]
            for (w, cut) in skip.iter_mut().zip(faulted(p, rows, step)?) {
                *w |= cut;
            }
        }
        Ok(skip)
    }
}

/// THE EXIT CUT: a row that left the start room or met the win rectangle
/// (`frame::exits_of`) is a win or a dying exit, and its next room has no
/// kernels.
pub(crate) struct Exit;

impl<N: Arithmetic> Prune<N> for Exit {
    fn about(&self) -> About {
        About { name: "the exit cut", kind: Kind::Cut, projection: "the player's position and the `room` global" }
    }
    fn record(&self, s: &Settings) -> (&'static str, String) {
        match s.win_at {
            Some(_) => ("win_at", s.row("win_at")),
            None => ("room", s.row("room")),
        }
    }
    fn prunes(&self, rows: &Rt2<N>, _step: u32, s: &Settings) -> Result<Vec<bool>> {
        crate::frame::exits_of(rows, s)
    }
    fn why(&self, _row: &Rt2<N>, _step: u32, s: &Settings) -> Result<String> {
        Ok(match crate::frame::win_rect(s) {
            Some(r) => format!("its player meets the win rectangle {r:?}"),
            None => format!("it left the start room for {:?}", s.win_room()),
        })
    }
}

/// THE OUT-OF-ROOM CUT, under a win rectangle (`--win-at`, the summit's
/// flag): a row that LEFT the room is no win, and its shape has no kernel.
pub(crate) struct OutOfRoom;

impl<N: Arithmetic> Prune<N> for OutOfRoom {
    fn about(&self) -> About {
        About { name: "the out-of-room cut", kind: Kind::Cut, projection: "the `room` global" }
    }
    fn record(&self, s: &Settings) -> (&'static str, String) {
        ("win_at", s.row("win_at"))
    }
    fn prunes(&self, rows: &Rt2<N>, _step: u32, s: &Settings) -> Result<Vec<bool>> {
        Ok(crate::frame::room_is(rows, s.room)?.into_iter().map(|here| !here).collect())
    }
    fn why(&self, _row: &Rt2<N>, _step: u32, s: &Settings) -> Result<String> {
        Ok(format!("it is out of the start room {:?}, under a win rectangle", s.room))
    }
}

/// THE LOST-BERRY CUT, 100%: a row whose berry can no longer be taken
/// (`berry_lost`) can win no more.
pub(crate) struct BerryLost;

impl<N: Arithmetic> Prune<N> for BerryLost {
    fn about(&self) -> About {
        About { name: "the lost-berry cut", kind: Kind::Cut, projection: "the berry's holders (fruit, fly fruit, key, chest, fake wall) and `got_fruit`" }
    }
    fn record(&self, s: &Settings) -> (&'static str, String) {
        ("category", s.row("category"))
    }
    fn prunes(&self, rows: &Rt2<N>, _step: u32, s: &Settings) -> Result<Vec<bool>> {
        berry_lost(rows, s)
    }
    fn why(&self, _row: &Rt2<N>, _step: u32, _s: &Settings) -> Result<String> {
        Ok("the room's berry is not taken and nothing is left that holds or drops it".to_string())
    }
}

/// 100%, per lane: can no successor take the room's berry any more? Not
/// taken (`got_fruit` false, not unknown) and nothing left that holds or
/// drops it - the fruit, the fly fruit (gone once it flew off), the key and
/// its chest, a fake wall. Exact: every state the row stands for is lost.
pub(crate) fn berry_lost<N: Arithmetic>(rt2: &Rt2<N>, s: &Settings) -> Result<Vec<bool>> {
    let lanes = rt2.width;
    let ids = crate::compiled::ids();
    let sources = [ids.g_fruit, ids.g_fly_fruit, ids.g_key, ids.g_chest, ids.g_fake_wall];
    if !s.category.hundred() || sources.iter().any(|&g| !rt2.objects_of_type(g).is_empty()) {
        return Ok(vec![false; lanes]);
    }
    Ok(crate::frame::got_fruit(rt2, s.room)?.into_iter().map(|taken| !taken).collect())
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

/// THE ORB CUT: in the orb room, where a win needs the orb, a row whose big
/// chest is still closed too late for the tree's horizon is not expanded
/// (`orb_deadline_skip`). The horizon is the step the tree was started for
/// (the search's, the forward's `--steps`), its record's `orb_cut` line
/// (`off` elsewhere, or `h H`). The tree's frames depend on it, so it
/// serves horizons up to H and is never raised past it (`grow_tree`), as
/// under level -1.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct OrbCut {
    pub(crate) h: u32,
}

impl OrbCut {
    /// The record's key.
    const KEY: &'static str = "orb_cut";

    /// A new tree's cut for horizon `to` (steps) under `s`, recorded.
    fn start(to: u32, dir: &std::path::Path, s: &Settings) -> Result<Option<OrbCut>> {
        let cut = crate::frame::orb_required(s).then_some(OrbCut { h: to });
        crate::storage::record::update(dir, Self::KEY, Self::text(cut))?;
        Ok(cut)
    }

    /// The record line's value (`None`: off).
    fn text(this: Option<OrbCut>) -> String {
        this.map_or("off".to_string(), |c| format!("h {}", c.h))
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

impl<N: Arithmetic> Prune<N> for OrbCut {
    fn about(&self) -> About {
        About { name: "the orb cut", kind: Kind::Cut, projection: "the big chest's `state`" }
    }
    fn record(&self, _s: &Settings) -> (&'static str, String) {
        (Self::KEY, Self::text(Some(*self)))
    }
    fn horizon(&self) -> Option<u32> {
        Some(self.h)
    }
    fn prunes(&self, rows: &Rt2<N>, step: u32, s: &Settings) -> Result<Vec<bool>> {
        Ok(orb_deadline_skip(rows, step, self.h, s.clock()))
    }
    fn why(&self, _row: &Rt2<N>, step: u32, s: &Settings) -> Result<String> {
        let clock = s.clock();
        Ok(format!(
            "its big chest is still closed after step {step}: opening in frame {} at the earliest, a win {ORB_MIN_FRAMES_AFTER_CHEST} frames after passes the horizon's frame {}",
            clock.frame_of(step + 1),
            clock.frame_of(self.h)
        ))
    }
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
    pub(crate) fn table(&self, s: &Settings) -> Result<&'static crate::search::level_minus_one::CostToGo> {
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
    fn start(want: Option<MinusOne>, dir: &std::path::Path, s: &Settings) -> Result<Option<TreeL1>> {
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

impl<N: Arithmetic> Prune<N> for TreeL1 {
    fn about(&self) -> About {
        About { name: "level -1", kind: Kind::Filter, projection: "the row's shape and cell" }
    }
    fn record(&self, _s: &Settings) -> (&'static str, String) {
        (Self::KEY, Self::text(Some(self)))
    }
    fn horizon(&self) -> Option<u32> {
        Some(self.h)
    }
    fn prunes(&self, rows: &Rt2<N>, step: u32, _s: &Settings) -> Result<Vec<bool>> {
        let shape = rows.shape_hash();
        Ok(crate::storage::cells::block_cells(rows)?.into_iter().map(|c| self.drops(shape, c, step)).collect())
    }
    fn why(&self, row: &Rt2<N>, step: u32, _s: &Settings) -> Result<String> {
        let cell = crate::storage::cells::block_cells(row)?[0];
        Ok(format!("its cell's distance d {:?} from step {step} exceeds its horizon {}", self.table.d_of(row.shape_hash(), cell), self.h))
    }
}

/// THE OBJECTS LADDER's link: a finer forward keeps a row at frame t only if
/// its projection (`Rt2::widen_to`) is a coarse node ARC-MARKED with deadline
/// >= t. Sound: a fine state winning from t with remainder r projects to a
/// coarse node winning from t with r (coarse over-approximates, r exact).
/// The projection is keyed in the COARSE tree's key space (the marks'): a
/// code it lacks is a state the coarse tree never reached.
pub struct MarkFilter<'a, N: Arithmetic> {
    marked: &'a Visited<N>,
    coarser: crate::abstraction::Level,
    clock: crate::settings::Clock,
    /// The horizon the marks are for, in steps (the search's).
    horizon: u32,
}

impl<'a, N: Arithmetic> MarkFilter<'a, N> {
    /// The record's key: a ladder level's tree says what it was filtered for.
    pub(crate) const KEY: &'static str = "filtered_for";

    pub(crate) fn new(marked: &'a Visited<N>, coarser: crate::abstraction::Level, clock: crate::settings::Clock, horizon: u32) -> Self {
        MarkFilter { marked, coarser, clock, horizon }
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
        let (shape, keys, cells) = crate::frame::widened_keys(rt2, self.coarser, self.marked.keys())?;
        Ok(keys.iter().zip(&cells).map(|(k, &c)| k.and_then(|k| self.marked.deadline(shape, k, c))).collect())
    }

    /// Its record line's value under `s`: the horizon, the run's level -1
    /// and the marks (their count and `Visited::filter_fingerprint`). A
    /// ladder level's tree is kept only for the same (`search::driver`).
    pub(crate) fn record_line(&self, s: &Settings) -> String {
        format!("steps {} level-1 {} marks {} {:016x}", self.horizon, MinusOne::text(s.minus_one().as_ref()), self.marked.len(), self.marked.filter_fingerprint())
    }

    /// The horizon (steps) a `record_line` value names.
    pub(crate) fn recorded_steps(value: &str) -> Option<u32> {
        value.strip_prefix("steps ")?.split_whitespace().next()?.parse().ok()
    }
}

impl<N: Arithmetic> Prune<N> for MarkFilter<'_, N> {
    fn about(&self) -> About {
        About { name: "the ladder's filter", kind: Kind::Filter, projection: "the coarser level's node (`Rt2::widen_to`), at frame boundaries" }
    }
    fn record(&self, s: &Settings) -> (&'static str, String) {
        (Self::KEY, self.record_line(s))
    }
    fn horizon(&self) -> Option<u32> {
        Some(self.horizon)
    }
    fn prunes(&self, rows: &Rt2<N>, step: u32, _s: &Settings) -> Result<Vec<bool>> {
        Ok(self.allowed(rows, step)?.into_iter().map(|ok| !ok).collect())
    }
    fn why(&self, row: &Rt2<N>, step: u32, _s: &Settings) -> Result<String> {
        Ok(format!("its projection onto {} has deadline {:?}, step {step} needs one >= {step}", self.coarser, self.deadlines(row)?[0]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abstraction::Level;
    use crate::engines::reference::RefEngine;
    use celeste_engine::runtime2::{P8, AV};

    /// The mark filter admits a marked node's rows up to its deadline only.
    #[test]
    fn the_mark_filter_admits_up_to_the_deadline() {
        let s = Settings::default();
        let block = RefEngine::new(&s).expect("ref engine").initial().expect("initial state").canonicalized();
        let coarse = Level::parse("r0sx").expect("level");
        // The coarse key space holds the projection's codes.
        let mut w = block.clone_block();
        w.widen_to(coarse);
        w.canonical();
        let mut space = celeste_engine::exact::KeySpace::new();
        space.absorb(&[&w]);
        let (shape, keys, cells) = crate::frame::widened_keys(&block, coarse, &space).expect("keys");
        let mut marked = Visited::new(space.clone());
        marked.insert_until(shape, keys[0].expect("keyed"), cells[0], 5);
        let clock = crate::settings::Clock { per_frame: 1 };
        let f = MarkFilter::new(&marked, coarse, clock, 9);
        assert_eq!(f.allowed(&block, 5).expect("filter"), vec![true], "at the deadline");
        assert_eq!(f.allowed(&block, 6).expect("filter"), vec![false], "past it");
        assert_eq!(MarkFilter::new(&Visited::new(Default::default()), coarse, clock, 9).allowed(&block, 0).expect("filter"), vec![false], "a key space without its codes");
        let empty = Visited::new(space);
        assert_eq!(MarkFilter::new(&empty, coarse, clock, 9).allowed(&block, 0).expect("filter"), vec![false], "not marked");
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
        let num = |b: &Rt2<P8>, c: u32| match b.cols[c as usize].at(0) {
            AV::Num(n) => n,
            other => panic!("not a number: {other:?}"),
        };
        let global = |b: &Rt2<P8>, name: &str| num(b, b.shape.globals()[celeste_names::global(name) as usize]);
        let chest_state = |b: &Rt2<P8>| b.objects_of_type(ids.g_big_chest).first().map(|&o| num(b, b.obj_field_cell(o, ids.f_state).expect("a state")));
        let orb_still = |b: &Rt2<P8>| {
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
            let frame = |eng: &mut RefEngine, b: &Rt2<P8>, input: u8| {
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
                if crate::frame::exits_of(&st, &s).expect("exits")[0] {
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

    /// EVERY CUT `each` lists is one `not_expanded` cuts by, and the prunes
    /// a run has follow from its settings and the tree's record: room (1,0)
    /// any% holds the exit's alone; room (5,2) under a win rectangle with an
    /// orb cut the out-of-room cut and the orb's too, and its start (no
    /// player, the chest closed) at step 30 of h100 meets the orb cut alone;
    /// in 100% the lost-berry cut, which takes the start (the room holds no
    /// berry).
    #[test]
    fn the_prunes_follow_from_the_settings_and_the_record() {
        let names = |p: &Prunes<P8>| p.each().iter().map(|p| (p.about().name, p.about().kind)).collect::<Vec<_>>();
        let plain = Settings::default();
        assert_eq!(names(&Prunes::of(&plain, Recorded::default(), None)), [("the exit cut", Kind::Cut)]);
        let orb = Recorded { minus_one: None, orb: Some(OrbCut { h: 100 }) };
        let rect = Settings { room: (5, 2), win_at: Some((9, 9, 101, 101)), ..Default::default() };
        let cuts = Prunes::<P8>::of(&rect, orb, None);
        assert_eq!(names(&cuts), [("the exit cut", Kind::Cut), ("the out-of-room cut", Kind::Cut), ("the orb cut", Kind::Cut)]);
        let start = RefEngine::new(&rect).expect("ref engine").initial().expect("the start");
        let by = |p: &Prunes<P8>, s: &Settings, step: u32| p.each().into_iter().map(|q| verdict(q, &start, step, s).expect("verdict").is_some()).collect::<Vec<_>>();
        assert_eq!(by(&cuts, &rect, 30), [false, false, true]);
        assert_eq!((cuts.not_expanded(&start, 30).expect("cuts"), cuts.not_expanded(&start, 29).expect("cuts")), (vec![true], vec![false]));
        let q = cuts.each()[2];
        assert_eq!((q.record(&rect), q.horizon()), (("orb_cut", "h 100".to_string()), Some(100)));
        let hundred = Settings { room: (5, 2), category: crate::settings::Category::Hundred, ..Default::default() };
        let berry = Prunes::<P8>::of(&hundred, orb, None);
        assert_eq!(names(&berry), [("the exit cut", Kind::Cut), ("the lost-berry cut", Kind::Cut), ("the orb cut", Kind::Cut)]);
        assert_eq!(by(&berry, &hundred, 29), [false, true, false]);
    }
}
