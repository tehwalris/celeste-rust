//! THE SEARCH'S DRIVER (`rewrite search`, `rewrite dual`, `rewrite check
//! known`; plans/architecture.md "The search"): per level, coarsest first,
//! the forward (`frame::grow_tree`) and its arc phase (`arc_level`), then
//! the concrete search at the last level (the dual search: `dual::search`
//! over pairs). The outcome is recorded (`result.json`) before it is returned.

use std::path::Path;

use anyhow::Result;
use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::Rt2;
use celeste_core::pico8_num::Pico8Num as P8;

use super::backward::{backward, optimum, Seeds, Winning, BLOCK};
use super::concrete::{concrete_search, inputs_text, NodeKeys, Side, Witness};
use super::reach::{inside_w, ReachEdges};
use super::rem::{start_point, Rem};
use super::result::{DualResult, LevelResult, Marks, SearchResult, Verdict};
use crate::abstraction::Level;
use crate::compiled::KernelLanes;
use crate::frame::{MarkFilter, Visited};
use crate::settings::{Run, Settings};

pub struct SearchSpec<'a> {
    /// The run's settings (a dual search's fixed half: real play).
    pub settings: &'a Settings,
    /// The levels, comma-separated, coarsest first (`--level`, as recorded).
    pub level: &'a str,
    /// The horizon in frames, and whether it is a KNOWN solution's
    /// (`--ceiling`: a refutation, or no witness by it, is an error).
    pub horizon: u32,
    pub ceiling: bool,
    /// The search's directory: level i's tree in `level{i:02}/` (a dual search's
    /// under `fixed/` and `float/`), the witness and `result.json`.
    pub dir: &'a Path,
    /// Stop at the arc bound (`--no-witness`).
    pub bound_only: bool,
    /// A known route (`--prefer`): tried first, checked against every
    /// pruning step (`search::known`).
    pub prefer: Option<&'a [u8]>,
    /// THE DUAL SEARCH: the float half's settings (`Settings::float_half`).
    pub dual: Option<&'a Settings>,
}

/// Run the search `spec` describes; its record. A failed level -1 table is
/// the verdict `LevelMinusOneRefused` (with its error), not an error.
pub fn run(spec: &SearchSpec) -> Result<SearchResult> {
    let levels: Vec<Level> = spec.level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let t0 = std::time::Instant::now();
    // The record of THIS run: none until it decides, and its marks record
    // none until its final level's arc phase writes it.
    std::fs::create_dir_all(spec.dir)?;
    for p in [super::result::path(spec.dir), super::result::marks_path(spec.dir, Marks::Support), super::result::marks_path(spec.dir, Marks::Arc)] {
        match std::fs::remove_file(p) {
            Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(e.into()),
            _ => {}
        }
    }
    let s = spec.settings;
    let mut res = SearchResult {
        format: super::result::FORMAT,
        room: s.room,
        level: spec.level.to_string(),
        horizon: spec.horizon,
        ceiling: spec.ceiling,
        steps_per_frame: s.clock().per_frame,
        levels: Vec::new(),
        verdict: Verdict::BoundOnly,
        optimal: None,
        witness: None,
        witness_file: None,
        witness_path: None,
        marks: None,
        wall_s: 0.0,
        peak_gb: 0.0,
        error: None,
        dual: spec.dual.map(|f| DualResult { float: f.arith.to_string(), float_loading_jank: f.loading_jank, fixed_exit: None }),
    };
    let verdict = match spec.dual {
        None => crate::with_arith!(s.arith, N => single::<N>(spec, &levels, &mut res)?),
        Some(float) => dual(spec, float, &levels, &mut res)?,
    };
    res.verdict = verdict;
    res.wall_s = t0.elapsed().as_secs_f64();
    res.peak_gb = crate::metrics::peak_rss_gb();
    res.write(spec.dir)?;
    Ok(res)
}

/// The level -1 table of each of `halves` first: one that cannot be built
/// is a recorded refusal, not a panic somewhere in the forward.
fn tables(halves: &[&Settings], res: &mut SearchResult) -> bool {
    for s in halves {
        if let Some(m) = s.minus_one() {
            if let Err(e) = m.table(s) {
                let half = if halves.len() > 1 { format!("{}: ", s.arith) } else { String::new() };
                res.error = Some(format!("{half}{e:#}"));
                return false;
            }
        }
    }
    true
}

fn single<N: KernelLanes>(spec: &SearchSpec, levels: &[Level], res: &mut SearchResult) -> Result<Verdict> {
    let s = spec.settings;
    let room = format!("{},{}", s.room.0, s.room.1);
    eprintln!("[search] room {room}, levels {}, horizon {}", spec.level, spec.horizon);
    if !tables(&[s], res) {
        return Ok(Verdict::LevelMinusOneRefused);
    }
    let finish = if spec.bound_only { Finish::Bound } else { Finish::Concrete };
    let solved = match run_levels::<N>(s, s.clock().steps(spec.horizon), levels, spec.dir, "", finish, Some(spec.dir), spec.prefer, res)? {
        None => return Ok(Verdict::Refuted),
        Some(solved) => solved,
    };
    let Some(w) = &solved.concrete else { return Ok(if spec.bound_only { Verdict::BoundOnly } else { Verdict::NoConcreteWin }) };
    let r = |a: i16, b: i16| if a == b { a.to_string() } else { format!("{a}..{b}") };
    let win_at = s.win_at.map_or(String::new(), |(x0, x1, y0, y1)| format!(" --win-at {},{}", r(x0, x1), r(y0, y1)));
    let last = res.levels.last().expect("a level");
    let header = format!(
        "# Room ({room}): the concrete witness of `rewrite search --level {} --frames {}`{win_at}.\n\
         # Arc bound f{} (level {}, {}); no concrete win before f{} inside the winning sets (exhaustive).\n\
         # A balloon room's win may hold for some `rnd` draws only: replay with seeds.\n",
        spec.level,
        spec.horizon,
        solved.arc,
        levels.len() - 1,
        last.spec,
        w.inputs.len()
    );
    witness(spec.dir, res, w, &header)?;
    Ok(Verdict::Optimal)
}

/// The witness file `witness_frame_F.txt` (`header`, the inputs), recorded
/// with its path.
fn witness(dir: &Path, res: &mut SearchResult, w: &Witness, header: &str) -> Result<()> {
    let f = w.inputs.len();
    let path = dir.join(format!("witness_frame_{f}.txt"));
    std::fs::write(&path, format!("{header}{}\n", inputs_text(&w.inputs)))?;
    res.optimal = Some(f as u32);
    res.witness = Some(w.inputs.clone());
    res.witness_path = Some(w.cells.clone());
    res.witness_file = Some(path.file_name().expect("a file").to_string_lossy().into_owned());
    Ok(())
}

/// THE DUAL SEARCH (`search::dual`): each half's levels to one horizon
/// (`<dir>/fixed`, real play; `<dir>/float`), the dual concrete search from
/// the larger arc bound, its witness replayed in both arithmetics.
fn dual(spec: &SearchSpec, float: &Settings, levels: &[Level], res: &mut SearchResult) -> Result<Verdict> {
    let cfg = spec.settings;
    let room = format!("{},{}", cfg.room.0, cfg.room.1);
    let (horizon, steps) = (spec.horizon, cfg.clock().steps(spec.horizon));
    eprintln!("[dual] room {room}, levels {}, horizon {horizon}; the fixed half real play, the float half {}", spec.level, float.arith);
    if !tables(&[cfg, float], res) {
        return Ok(Verdict::LevelMinusOneRefused);
    }
    let Some(sx) = run_levels::<P8>(cfg, steps, levels, &spec.dir.join("fixed"), "fixed/", Finish::Arcs, None, spec.prefer, res)? else { return Ok(Verdict::Refuted) };
    // The marks record: the float half's (the count's).
    let Some(sf) = run_levels::<LuaF64>(float, steps, levels, &spec.dir.join("float"), "float/", Finish::Arcs, Some(spec.dir), spec.prefer, res)? else { return Ok(Verdict::Refuted) };
    let (bx, bf) = (sx.arc, sf.arc);
    let bound = bx.max(bf);
    println!("DUAL BOUND: no dual win before f{bound} (the fixed half's arc bound f{bx}, the float half's f{bf})");
    if spec.bound_only {
        return Ok(Verdict::BoundOnly);
    }
    let (ax, af) = (sx.arcs.expect("kept"), sf.arcs.expect("kept"));
    let Some(w) = super::dual::search(&super::dual::Half { arcs: &ax, settings: cfg }, &super::dual::Half { arcs: &af, settings: float }, bound, spec.prefer)? else { return Ok(Verdict::NoConcreteWin) };
    super::dual::replay::<P8, LuaF64>(&w, cfg, float)?;
    let f = w.inputs.len();
    let header = format!(
        "# Room ({room}): the DUAL witness of `rewrite dual --level {} --frames {horizon}`: {} exits on frame {f} (the count, Philippe's rule (b)), real play (16.16) on frame {}.\n\
         # Dual bound f{bound} (fixed f{bx}, float f{bf}); no dual win before f{f} inside both halves' winning sets (exhaustive).\n\
         # A balloon room's real-play exit may hold for some `rnd` draws only.\n",
        spec.level, float.arith, w.fixed_exit
    );
    // Its path: the float half's cells along it.
    let cells = super::replay::Replay::<LuaF64>::of(&w.inputs, None, float)?.win_cells(float.clock())?;
    witness(spec.dir, res, &Witness { inputs: w.inputs.clone(), cells }, &header)?;
    if let Some(d) = &mut res.dual {
        d.fixed_exit = Some(w.fixed_exit);
    }
    Ok(Verdict::Optimal)
}

/// How a search's LAST level ends (`run_levels`).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Finish {
    /// The concrete search inside its W: the witness.
    Concrete,
    /// The arc bound only (`--no-witness`).
    Bound,
    /// Its arcs kept for the dual search (`search::dual`).
    Arcs,
}

/// THE LEVEL LOOP of a search and of `check known`: per level `(level,
/// clock)` (coarsest first) `step(li, last, filter)`, `filter` the previous
/// level's arc marks (`MarkFilter`); a level's marks (`Some`) go to the
/// next, `None` ends the loop.
fn for_levels<N: Rem>(levels: &[(Level, crate::settings::Clock)], mut step: impl FnMut(usize, bool, Option<&MarkFilter<N>>) -> Result<Option<Visited<N>>>) -> Result<()> {
    let mut prev: Option<Visited<N>> = None;
    for (li, &(_, clock)) in levels.iter().enumerate() {
        let filter = prev.as_ref().map(|m| MarkFilter::new(m, levels[li - 1].0, clock));
        match step(li, li + 1 == levels.len(), filter.as_ref())? {
            Some(marks) => prev = Some(marks),
            None => break,
        }
    }
    Ok(())
}

/// The LEVELS of a search under `base`, each a forward and its arc phase,
/// recorded in `res.levels` (dirs under `prefix`); the last ends as `finish`
/// says; with `record` the final level's marks go to that directory
/// (`result::marks_path`, `res.marks`). `None`: a level's arc phase REFUTED
/// the horizon.
#[allow(clippy::too_many_arguments)]
fn run_levels<N: KernelLanes>(
    cfg: &Settings,
    steps: u32,
    levels: &[Level],
    base: &Path,
    prefix: &str,
    finish: Finish,
    record: Option<&Path>,
    prefer: Option<&[u8]>,
    res: &mut SearchResult,
) -> Result<Option<Solved<N>>> {
    let mut solved = None;
    let clocked: Vec<(Level, crate::settings::Clock)> = levels.iter().map(|&l| (l, cfg.clock())).collect();
    for_levels::<N>(&clocked, |li, last, filter| {
        let (lvl, dir) = (levels[li], base.join(format!("level{li:02}")));
        // THE FORWARD. Level 0's tree is reused or extended (`grow_tree`
        // refuses one filtered by level -1 below this horizon); a finer
        // level's is kept only for the same marks, horizon and level -1, else
        // rebuilt (it is the cheap one).
        let t = std::time::Instant::now();
        let want = cfg.minus_one();
        if let Some(f) = filter {
            // The tree's record says what its forward was filtered for.
            let m = f.marked();
            let key = format!("steps {steps} level-1 {} marks {} {:016x}", crate::frame::MinusOne::text(want.as_ref()), m.len(), m.filter_fingerprint());
            let have = crate::storage::record::Record::read(&dir).ok().flatten().and_then(|r| r.get("filtered_for").map(str::to_string));
            if have.as_deref() != Some(key.as_str()) {
                if dir.exists() {
                    eprintln!("[search] {}: filtered for other marks or another horizon: built again", dir.display());
                    std::fs::remove_dir_all(&dir)?;
                }
                crate::storage::record::update(&dir, "filtered_for", key)?;
            }
        }
        let run = Run::new(cfg, lvl);
        let engine = || -> Result<Box<dyn crate::frame::FrameStep<N>>> { Ok(Box::new(crate::compiled::Kernels::<N>::new(&run)?)) };
        let initial = || -> Result<Vec<Rt2<N>>> { Ok(vec![crate::engines::reference::ReferenceEngine::<N>::new(cfg)?.initial()?.canonicalized()]) };
        let first_win = crate::frame::grow_tree(&dir, steps, want, filter, &run, engine, initial)?.win_frame;
        let forward_s = t.elapsed().as_secs_f64();
        // `f` counts search steps.
        eprintln!("[search] level {li} ({lvl}): forward to f{steps} in {forward_s:.1} s; first win {first_win:?}");
        crate::metrics::mem_phase("forward");
        // THE ARC PHASE (the final level's marks recorded), and the concrete
        // search at the LAST level only: a coarser level's bound is loose
        // wherever its objects are (a try at it can cost most of a search),
        // and the finer level decides anyway.
        let known = prefer.map(|inputs| super::known::Route { inputs, filter, hulls: false });
        let (arcs, marks, _) = arc_level::<N>(&dir, &run, steps, !last, record, known.as_ref())?;
        let bound = arcs.bound(cfg.clock());
        let concrete = match bound {
            Some(b) if last && finish == Finish::Concrete => concrete_level(&arcs, b, prefer)?,
            _ => None,
        };
        let tree = format!("{prefix}level{li:02}");
        if record.is_some() && (last || bound.is_none()) {
            res.marks = Some(tree.clone());
        }
        res.levels.push(LevelResult { spec: lvl.to_string(), dir: tree, forward_s, first_win, arc_bound: bound });
        let Some(arc) = bound else { return Ok(None) };
        println!("ARC BOUND: no win before f{arc} (level {li}, {lvl}: remainder-exact, the level's objects over-approximated)");
        if last {
            solved = Some(Solved { arc, concrete, arcs: (finish == Finish::Arcs).then_some(arcs) });
        }
        Ok(marks)
    })?;
    Ok(solved)
}

/// A search's last level, its horizon not refuted.
pub(crate) struct Solved<N: Rem> {
    /// The rotation graph's optimum, a lower bound on the game's, in FRAMES.
    pub(crate) arc: u32,
    /// The concrete optimum and its witness, when asked for and found.
    pub(crate) concrete: Option<Witness>,
    /// Its arcs, kept for a dual search's half (`Finish::Arcs`).
    pub(crate) arcs: Option<Arcs<N>>,
}

/// A level's arc phase as the concrete searches read it: W, the tree's
/// shapes (W's nodes' regions) and its key space, the start's node and the
/// optimum.
pub(crate) struct Arcs<N: Rem> {
    pub(crate) w: Winning<N::C>,
    pub(crate) shapes: Vec<u64>,
    pub(crate) space: celeste_engine::exact::KeySpace<N>,
    /// The run its nodes key at (its tree's), and the horizon in search
    /// steps.
    pub(crate) run: Run,
    pub(crate) horizon: u32,
    /// The start's node (`None`: no winning set at any step; not looked up
    /// by its projection: the tree holds the start unwidened), and the
    /// optimum in search steps (`None`: the horizon is REFUTED).
    pub(crate) start: Option<u32>,
    pub(crate) steps: Option<u32>,
}

impl<N: Rem> Arcs<N> {
    /// The optimum in FRAMES, a lower bound on the game's: a win at step s
    /// is during the frame it completes (a frame's win is a win at its last
    /// step at the latest). `None`: the horizon is refuted.
    pub(crate) fn bound(&self, clock: crate::settings::Clock) -> Option<u32> {
        self.steps.map(|k| clock.frame_of(k))
    }

    /// The level's states as the concrete searches admit them.
    pub(super) fn side(&self) -> Result<Side<'_, N>> {
        Ok(Side { w: &self.w, node: NodeKeys::new(&self.w, &self.shapes)?, keys: &self.space, run: &self.run, seeded: self.run.settings.concrete().1 })
    }
}

/// THE ARC PHASE over a finished tree, before the edge files close: the
/// backward from its wins and the optimum, the edges kept for the reach.
/// Its readers: the search, `check known`, `check::gate`.
pub(crate) struct ArcPhase<N: Rem> {
    pub(crate) seeds: Seeds<N>,
    pub(crate) arcs: Arcs<N>,
}

impl<N: Rem> ArcPhase<N> {
    /// `backward` and `optimum` over the tree in `dir` (of `run`) to step
    /// `horizon`, its closed spans and W's nodes in files under `scratch`
    /// (mapped, unlinked; the directory replaced).
    pub(crate) fn of(dir: &Path, run: &Run, horizon: u32, scratch: &Path) -> Result<Self> {
        run.check(dir)?;
        let s = &*run.settings;
        crate::metrics::mem_phase("arc: start");
        let seeds = Seeds::<N>::load(dir, horizon)?;
        crate::metrics::mem_phase("arc: edge files opened");
        let tb = std::time::Instant::now();
        if scratch.exists() {
            std::fs::remove_dir_all(scratch)?;
        }
        let w = backward(&seeds.store, &seeds.pairs, &seeds.wins, horizon, s.threads(), Some(scratch))?;
        crate::metrics::mem_phase("arc: backward");
        let tb = tb.elapsed().as_secs_f64();
        let start = w.index(seeds.start);
        let steps = optimum(&w, start, start_point::<N>(), horizon);
        eprintln!("[arc] backward {tb:.2} s; optimum {steps:?} (steps)");
        let arcs = Arcs { w, shapes: seeds.shapes.clone(), space: seeds.space.clone(), run: run.clone(), horizon, start, steps };
        Ok(ArcPhase { seeds, arcs })
    }

    /// W's support as marks rows, each node's deadline its last frame
    /// with a set (the marks record's support, `result::marks_path`).
    fn support_rows(&self, threads: usize) -> Vec<crate::frame::MarkRow> {
        let (w, horizon) = (&self.arcs.w, self.arcs.horizon);
        let parts = crate::par::Pool::new(threads).chunks(w.len().div_ceil(BLOCK), 4096, |lo, hi| {
            w.nodes_from(lo * BLOCK, hi * BLOCK)
                .map(|x| {
                    let (shape, key, cell) = self.seeds.state(x.id);
                    crate::frame::mark_row(shape, key, cell, x.last() as u16, horizon)
                })
                .collect::<Vec<_>>()
        });
        parts.into_iter().flatten().collect()
    }

    /// Per node the LAST frame it is reached inside W from the start
    /// (`reach::inside_w`; at most its W deadline; `u32::MAX`: never): the
    /// next level's filter, and the UI's arc marks.
    fn reach(&self, threads: usize) -> Result<Vec<u32>> {
        let tr = std::time::Instant::now();
        let mut g = ReachEdges::new(&self.seeds.store, &self.arcs.w, self.seeds.win_ids(), threads);
        let last = inside_w(&self.arcs.w, &mut g, &self.seeds.pairs, self.arcs.start, start_point::<N>(), self.arcs.horizon, threads)?;
        eprintln!("[arc] reach {:.2} s ({} reached nodes' {} out-edges loaded)", tr.elapsed().as_secs_f64(), g.sizes().0, g.sizes().1);
        drop(g);
        crate::metrics::mem_phase("arc: reach");
        Ok(last)
    }

    /// The reached nodes (`reach`) with their deadlines, as `(shape, key,
    /// cell, deadline)`.
    fn reached<'a>(&'a self, last: &'a [u32]) -> impl Iterator<Item = (u64, (u64, u64), u32, u16)> + 'a {
        (0..self.arcs.w.len() as u32).filter(|&i| last[i as usize] != u32::MAX).map(|i| {
            let (shape, key, cell) = self.seeds.state(self.arcs.w.id(i));
            (shape, key, cell, last[i as usize] as u16)
        })
    }
}

/// A level's ARC PHASE and what follows it on every path: with `next` its
/// reach marks (the next level's filter); with `record` at the search's
/// FINAL level (the last, `!next`, or the one that refutes the horizon) THE
/// MARKS RECORD into that directory (`result::marks_path`: W's support, and
/// unless refuted what `reach::inside_w` reaches); with `known` a known
/// route checked against every pruning step (an error if one drops it;
/// `Some(f)`: it exits in frame `f` and survives). The edge files closed.
#[allow(clippy::type_complexity)]
fn arc_level<N: KernelLanes>(dir: &Path, run: &Run, horizon: u32, next: bool, record: Option<&Path>, known: Option<&super::known::Route<N>>) -> Result<(Arcs<N>, Option<Visited<N>>, Option<u32>)> {
    use super::result::marks_path;
    use crate::frame::{mark_row, save_marks};
    let s = &*run.settings;
    // The scratch beside the tree (the search's own).
    let ap = ArcPhase::<N>::of(dir, run, horizon, &dir.join("arc-scratch"))?;
    let space = &ap.seeds.space;
    let refuted = ap.arcs.steps.is_none();
    let record = record.filter(|_| !next || refuted);
    if let Some(out) = record {
        save_marks(&marks_path(out, Marks::Support), ap.support_rows(s.threads()), horizon, space)?;
    }
    let reached = record.filter(|_| !refuted);
    let mut marks = None;
    if next || reached.is_some() {
        let last = ap.reach(s.threads())?;
        if next {
            let mut m = Visited::new(space.clone());
            for (shape, key, cell, d) in ap.reached(&last) {
                m.insert_until(shape, key, cell, d);
            }
            marks = Some(m);
        }
        if let Some(out) = reached {
            save_marks(&marks_path(out, Marks::Arc), ap.reached(&last).map(|(shape, key, cell, d)| mark_row(shape, key, cell, d, horizon)).collect(), horizon, space)?;
        }
    }
    // The concrete searches read W and the node keys, not the edge files.
    let arcs = ap.arcs;
    drop(ap.seeds);
    crate::metrics::mem_phase("arc: marks for the next level, edge files closed");
    let exit = match known {
        Some(route) => super::known::check_level(route, &arcs.side()?, &arcs, marks.as_ref(), dir)?,
        None => None,
    };
    Ok((arcs, marks, exit))
}

/// THE CONCRETE SEARCH of a search's last level (inside its W, from the arc
/// bound `bound`): its witness.
fn concrete_level<N: KernelLanes>(arcs: &Arcs<N>, bound: u32, prefer: Option<&[u8]>) -> Result<Option<Witness>> {
    concrete_search(&arcs.side()?, arcs.horizon, bound, prefer)
}

/// A finished search's record (`result.json` in `search_dir`) and the trees
/// of the levels it ran, in its order (a dual search's halves, fixed
/// first): each one's directory and run (its record's, with the run's own
/// settings from `run`), its level checked against the record's.
pub(crate) fn levels_of(search_dir: &Path, run: &Settings) -> Result<(SearchResult, Vec<(std::path::PathBuf, Run)>)> {
    let res = SearchResult::read(search_dir)?;
    let mut trees = Vec::new();
    for l in &res.levels {
        let d = search_dir.join(&l.dir);
        let tree = Run::of_tree(&d, run)?;
        anyhow::ensure!(tree.level.to_string() == Level::parse(&l.spec).map_err(|e| anyhow::anyhow!(e))?.to_string(), "{}: level {}, the search's record says {}", d.display(), tree.level, l.spec);
        trees.push((d, tree));
    }
    Ok((res, trees))
}

/// `rewrite check known`: a KNOWN route against the finished search in
/// `dir`: per level the arc phase rerun, the route checked at every pruning
/// step (the hull forward's claim too). Levels and horizon from
/// `result.json` (`levels_of`; a dual half: its parent's, the half's levels)
/// unless `horizon`. An error at the first check that drops the route.
pub fn check_known(dir: &Path, inputs: &[u8], horizon: Option<u32>, run: &Settings) -> Result<()> {
    let (res, trees) = match dir.parent() {
        Some(p) if !super::result::path(dir).exists() && super::result::path(p).exists() => {
            let (res, trees) = levels_of(p, run)?;
            (res, trees.into_iter().filter(|(d, ..)| d.starts_with(dir)).collect())
        }
        _ => levels_of(dir, run)?,
    };
    anyhow::ensure!(!trees.is_empty(), "{}: the search ran no level here", dir.display());
    let horizon = horizon.unwrap_or(res.horizon);
    crate::with_arith!(trees[0].1.settings.arith, N => check_known_in::<N>(&trees, inputs, horizon))
}

fn check_known_in<N: KernelLanes>(trees: &[(std::path::PathBuf, Run)], inputs: &[u8], horizon: u32) -> Result<()> {
    let t0 = std::time::Instant::now();
    let clocked: Vec<(Level, crate::settings::Clock)> = trees.iter().map(|(_, r)| (r.level, r.settings.clock())).collect();
    for_levels::<N>(&clocked, |li, last, filter| {
        let (dir, tree) = &trees[li];
        let (cfg, lvl) = (&*tree.settings, tree.level);
        let steps = cfg.clock().steps(horizon);
        anyhow::ensure!(crate::frame::tree_first_win_through::<N>(dir, steps)?.is_some(), "{}: no tree through step {steps} (run the search first)", dir.display());
        let route = super::known::Route { inputs, filter, hulls: true };
        let (arcs, marks, known) = arc_level::<N>(dir, tree, steps, !last, None, Some(&route))?;
        let f = known.ok_or_else(|| anyhow::anyhow!("the known route does not exit by f{horizon}: nothing to check"))?;
        println!("[check known] level {li} ({lvl}): the known route (exit f{f}) survives (arc bound {:?})", arcs.bound(cfg.clock()));
        Ok(marks)
    })?;
    println!("[check known] every level passes ({:.1} s)", t0.elapsed().as_secs_f64());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// THE BREADTH-FIRST LAYERS (the gates' witnesses come from the
    /// depth-first try, skipped here): room (1,0)'s arc gate in 16.16, Celia's
    /// floats and dual, each optimum and witness pinned, the same at 3 and 32
    /// threads (the layers merge their chunks in order).
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels in both arithmetics, three searches at two thread counts, ~60 CPU-s"]
    fn the_breadth_first_searches_are_pinned() {
        super::super::concrete::NO_DFS.store(true, std::sync::atomic::Ordering::Relaxed);
        let base = crate::testutil::TestDir::new("bfs");
        let mut runs: Vec<String> = Vec::new();
        for threads in [3, 32] {
            let fixed = Settings { win_at: Some((9, 9, 101, 101)), threads: Some(threads), ..Settings::default() };
            let float = Settings { arith: crate::settings::Arith::Float(crate::settings::FloatTool::Celia), ..fixed.clone() }.resolved().expect("float settings");
            let dual = fixed.float_half(crate::settings::FloatTool::Celia, None).expect("the float half");
            let mut lines = Vec::new();
            for (name, s, half) in [("search fixed", &fixed, None), ("search float", &float, None), ("dual", &fixed, Some(&dual))] {
                let dir = base.join(format!("{name}-{threads}").replace(' ', "-"));
                let spec = SearchSpec { settings: s, level: "r0sx", horizon: 35, ceiling: false, dir: &dir, bound_only: false, prefer: None, dual: half };
                let r = run(&spec).expect("the search");
                assert_eq!(r.verdict, Verdict::Optimal, "{name} at {threads} threads");
                let inputs = r.witness.expect("a witness").iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",");
                let exit = r.dual.and_then(|d| d.fixed_exit).map_or(String::new(), |x| format!(" fixed exit {x}"));
                lines.push(format!("{name}: optimum {}{exit} inputs {inputs}", r.optimal.expect("an optimum")));
            }
            runs.push(lines.join("\n") + "\n");
        }
        assert_eq!(runs[0], runs[1], "the breadth-first searches differ at 3 and 32 threads");
        let pin = concat!(env!("CARGO_MANIFEST_DIR"), "/gates/bfs_room10_win9-101_h35.txt");
        if std::env::var_os("CELESTE_REGENERATE").is_some() {
            std::fs::write(pin, &runs[0]).expect("write the pin");
        }
        assert_eq!(std::fs::read_to_string(pin).expect("the pin (CELESTE_REGENERATE=1 writes it)"), runs[0], "the breadth-first searches moved (gates/bfs_room10_win9-101_h35.txt)");
    }
}
