//! THE SEARCH'S DRIVER (`rewrite search`, `rewrite dual`, `rewrite
//! check-known`; plans/architecture.md "The search"): per level of
//! `--level`, coarsest first, the forward (`frame::grow_tree`; a finer
//! level filtered by the coarser one's arc-marked nodes) and its arc phase
//! (`arc_dp::solve`), then at the last level the concrete search - or, for
//! the dual search, both arithmetics' levels and `dual::search` over pairs.
//! The outcome is recorded (`result.json`, `search::result`) before it is
//! returned; the binary prints it and maps it to the exit code.

use std::path::Path;

use anyhow::Result;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use super::arc_dp::{solve, Solved, Witness};
use super::result::{DualResult, LevelResult, SearchResult, Verdict};
use crate::abstraction::Level;
use crate::compiled::KernelLanes;
use crate::frame::{Block, MarkFilter, Visited};
use crate::settings::{Arith, Settings};

/// A search to run.
pub struct SearchSpec<'a> {
    /// The run's settings (a dual search's fixed half: real play).
    pub settings: &'a Settings,
    /// The levels, comma-separated, coarsest first (`--level`, as recorded).
    pub level: &'a str,
    /// The horizon in frames, and whether it is a KNOWN solution's
    /// (`--ceiling`: a refutation, or no witness by it, is an error).
    pub horizon: u32,
    pub ceiling: bool,
    /// The checkpoint dir: level i's tree in `level{i:02}/` (a dual search's
    /// under `fixed/` and `float/`), the witness and `result.json`.
    pub dir: &'a Path,
    /// Stop at the arc bound (`--no-witness`).
    pub bound_only: bool,
    /// The web UI's arc pass into this directory (`--save-marks`; a dual
    /// search's: the float half's, with the dual witness).
    pub save_marks: Option<&'a Path>,
    /// A known route: tried first, and checked against every pruning step
    /// (`--prefer`, `search::known`).
    pub prefer: Option<&'a [u8]>,
    /// THE DUAL SEARCH: the float half's settings (`Settings::float_half`).
    pub dual: Option<&'a Settings>,
}

/// Run the search `spec` describes; its record. A failed level -1 table is
/// the verdict `LevelMinusOneRefused` (with its error), not an error.
pub fn run(spec: &SearchSpec) -> Result<SearchResult> {
    let levels: Vec<Level> = spec.level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let t0 = std::time::Instant::now();
    // The record of THIS run: none until it decides.
    std::fs::create_dir_all(spec.dir)?;
    match std::fs::remove_file(super::result::path(spec.dir)) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(e.into()),
        _ => {}
    }
    let s = spec.settings;
    let mut res = SearchResult {
        format: super::result::FORMAT,
        room: s.room,
        level: spec.level.to_string(),
        horizon: spec.horizon,
        ceiling: spec.ceiling,
        steps_per_frame: s.steps_per_frame(),
        levels: Vec::new(),
        verdict: Verdict::BoundOnly,
        optimal: None,
        witness: None,
        witness_file: None,
        wall_s: 0.0,
        peak_gb: 0.0,
        error: None,
        dual: spec.dual.map(|f| DualResult { float: f.arith.to_string(), float_loading_jank: f.loading_jank, fixed_exit: None }),
    };
    let verdict = match spec.dual {
        None => match s.arith {
            Arith::Fixed => single::<P8>(spec, &levels, &mut res)?,
            Arith::Float(_) => single::<LuaF64>(spec, &levels, &mut res)?,
        },
        Some(float) => dual(spec, float, &levels, &mut res)?,
    };
    res.verdict = verdict;
    res.wall_s = t0.elapsed().as_secs_f64();
    res.peak_gb = crate::metrics::peak_rss_gb();
    res.write(spec.dir)?;
    Ok(res)
}

/// The level -1 table of each of `halves` first: a room whose table cannot
/// be built is a recorded refusal (rerun without the filter), not a panic
/// somewhere in the forward.
fn tables(halves: &[&Settings], res: &mut SearchResult) -> bool {
    for s in halves {
        if let Some(m) = s.minus_one() {
            if let Err(e) = m.try_table(s) {
                // A dual search's names the half.
                let half = if halves.len() > 1 { format!("{}: ", s.arith) } else { String::new() };
                res.error = Some(format!("{half}{e:#}"));
                return false;
            }
        }
    }
    true
}

/// A single search in arithmetic `N`.
fn single<N: KernelLanes>(spec: &SearchSpec, levels: &[Level], res: &mut SearchResult) -> Result<Verdict> {
    let s = spec.settings;
    let room = format!("{},{}", s.room.0, s.room.1);
    eprintln!("[search] room {room}, levels {}, horizon {}", spec.level, spec.horizon);
    if !tables(&[s], res) {
        return Ok(Verdict::LevelMinusOneRefused);
    }
    let finish = if spec.bound_only { Finish::Bound } else { Finish::Concrete };
    let solved = match run_levels::<N>(s, spec.horizon * s.steps_per_frame(), levels, spec.dir, "", finish, spec.save_marks, spec.prefer, res)? {
        None => return Ok(Verdict::Refuted),
        Some(solved) => solved,
    };
    let Some(w) = &solved.concrete else { return Ok(if spec.bound_only { Verdict::BoundOnly } else { Verdict::NoConcreteWin }) };
    let r = |a: i16, b: i16| if a == b { a.to_string() } else { format!("{a}..{b}") };
    let win_at = s.win_at.map_or(String::new(), |(x0, x1, y0, y1)| format!(" --win-at {},{}", r(x0, x1), r(y0, y1)));
    let last = res.levels.last().expect("a level");
    let header = format!(
        "# Room ({room}): the concrete witness of `rewrite search --level {} --to {}`{win_at}.\n\
         # Arc bound f{} (level {}, {}); no concrete win before f{} inside the winning sets (exhaustive).\n\
         # A balloon room's win may hold for some `rnd` draws only: replay with seeds.\n",
        spec.level,
        spec.horizon,
        solved.arc.expect("the last level's bound"),
        levels.len() - 1,
        last.spec,
        w.inputs.len()
    );
    witness(spec.dir, res, w, &header)?;
    Ok(Verdict::Optimal)
}

/// The witness file `witness_frame_F.txt` (`header`, then the inputs),
/// recorded.
fn witness(dir: &Path, res: &mut SearchResult, w: &Witness, header: &str) -> Result<()> {
    let f = w.inputs.len();
    let path = dir.join(format!("witness_frame_{f}.txt"));
    std::fs::write(&path, format!("{header}{}\n", w.inputs_text()))?;
    res.optimal = Some(f as u32);
    res.witness = Some(w.inputs.clone());
    res.witness_file = Some(path.file_name().expect("a file").to_string_lossy().into_owned());
    Ok(())
}

/// THE DUAL SEARCH (`search::dual`): the fixed half under the spec's
/// settings (real play), the float half under `float`, each its levels to
/// the same horizon under `<dir>/fixed` and `<dir>/float`, then the dual
/// concrete search between the larger arc bound and the horizon; its
/// witness replayed in both arithmetics.
fn dual(spec: &SearchSpec, float: &Settings, levels: &[Level], res: &mut SearchResult) -> Result<Verdict> {
    let cfg = spec.settings;
    let room = format!("{},{}", cfg.room.0, cfg.room.1);
    let (horizon, steps) = (spec.horizon, spec.horizon * cfg.steps_per_frame());
    eprintln!("[dual] room {room}, levels {}, horizon {horizon}; the fixed half real play, the float half {}", spec.level, float.arith);
    if !tables(&[cfg, float], res) {
        return Ok(Verdict::LevelMinusOneRefused);
    }
    let Some(sx) = run_levels::<P8>(cfg, steps, levels, &spec.dir.join("fixed"), "fixed/", Finish::Arcs, None, spec.prefer, res)? else { return Ok(Verdict::Refuted) };
    let Some(sf) = run_levels::<LuaF64>(float, steps, levels, &spec.dir.join("float"), "float/", Finish::Arcs, spec.save_marks, spec.prefer, res)? else { return Ok(Verdict::Refuted) };
    let (bx, bf) = (sx.arc.expect("a bound"), sf.arc.expect("a bound"));
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
        "# Room ({room}): the DUAL witness of `rewrite dual --level {} --to {horizon}`: {} exits on frame {f} (the count, Philippe's rule (b)), real play (16.16) on frame {}.\n\
         # Dual bound f{bound} (fixed f{bx}, float f{bf}); no dual win before f{f} inside both halves' winning sets (exhaustive).\n\
         # A balloon room's real-play exit may hold for some `rnd` draws only.\n",
        spec.level, float.arith, w.fixed_exit
    );
    let wit = Witness { inputs: w.inputs.clone(), cells: Vec::new() };
    witness(spec.dir, res, &wit, &header)?;
    if let Some(d) = &mut res.dual {
        d.fixed_exit = Some(w.fixed_exit);
    }
    // The UI's path: the float half's cells along the witness.
    if let Some(out) = spec.save_marks {
        let cells = super::dual::path_cells::<LuaF64>(&w.inputs, float)?;
        let path = out.join("witness.txt");
        Witness { inputs: w.inputs.clone(), cells }.save(&path)?;
        let label = format!("label dual witness, valid in both arithmetics: {} exits on frame {f}, real play on frame {} (the float half's path)\n", float.arith, w.fixed_exit);
        std::fs::write(&path, label + &std::fs::read_to_string(&path)?)?;
    }
    println!("[gate] h{horizon} dual optimum {f} fixed exit {} inputs {}", w.fixed_exit, w.inputs_text());
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

/// The LEVELS of a search in arithmetic `N`, coarsest first, under `base`:
/// each a forward (the level's kernels from the reference engine's start; a
/// finer level filtered by the coarser one's arc marks) and its arc phase,
/// recorded in `res.levels` (their dirs named under `prefix`). The last
/// level ends as `finish` says. `None`: a level's arc phase REFUTED the
/// horizon.
#[allow(clippy::too_many_arguments)]
fn run_levels<N: KernelLanes>(
    cfg: &Settings,
    steps: u32,
    levels: &[Level],
    base: &Path,
    prefix: &str,
    finish: Finish,
    save_marks: Option<&Path>,
    prefer: Option<&[u8]>,
    res: &mut SearchResult,
) -> Result<Option<Solved<N>>> {
    // The previous level's arc-marked nodes: the next forward's filter.
    let mut prev: Option<(Visited<N>, Level)> = None;
    for (li, &lvl) in levels.iter().enumerate() {
        let last = li + 1 == levels.len();
        let dir = base.join(format!("level{li:02}"));
        // THE FORWARD. Level 0's tree is reused or extended (`grow_tree`:
        // one filtered by level -1 below this horizon is refused, a hull
        // tree cannot be raised). A finer level's tree is filtered by the
        // coarser marks for one horizon: kept only for the same marks,
        // horizon and level -1, else built again (it is the cheap one).
        let t = std::time::Instant::now();
        let want = cfg.minus_one();
        let filter = prev.as_ref().map(|(m, l)| MarkFilter::new(m, *l, cfg.steps_per_frame()));
        if let Some((m, _)) = &prev {
            let key = format!("steps {steps} level-1 {want:?} marks {} {:016x}\n", m.len(), m.filter_fingerprint());
            let key_path = dir.join("filtered_for.txt");
            if std::fs::read_to_string(&key_path).ok().as_deref() != Some(key.as_str()) {
                if dir.exists() {
                    eprintln!("[search] {}: filtered for other marks or another horizon: built again", dir.display());
                    std::fs::remove_dir_all(&dir)?;
                }
                std::fs::create_dir_all(&dir)?;
                std::fs::write(&key_path, key)?;
            }
        }
        let engine = || -> Result<Box<dyn crate::frame::FrameStep<N>>> { Ok(Box::new(crate::compiled::Kernels::<N>::new(cfg, lvl)?)) };
        let initial = || -> Result<Vec<Block<N>>> { Ok(vec![Block::canonical(crate::trace::refengine::ReferenceEngine::<N::A>::new(cfg)?.initial()?)?]) };
        let first_win = crate::frame::grow_tree(&dir, steps, want, filter.as_ref(), lvl, engine, initial, cfg)?.win_frame;
        let forward_s = t.elapsed().as_secs_f64();
        // `f` counts search steps.
        eprintln!("[search] level {li} ({lvl}): forward to f{steps} in {forward_s:.1} s; first win {first_win:?}");
        crate::metrics::mem_phase("forward");
        // THE ARC PHASE, and the concrete search at the LAST level only: a
        // coarser level's bound is loose wherever its objects are
        // (gemskip-nodiag 2800m h192: 379 s of a 450 s search went to a
        // fruitless try at level 0's bound f186; the optimum was 191), and
        // the finer level decides the optimum anyway.
        let concrete = last && finish == Finish::Concrete;
        let keep = last && finish == Finish::Arcs;
        // Every level saves its marks (the next overwrites).
        let known = prefer.map(|inputs| super::known::Route { inputs, filter: filter.as_ref(), hulls: false });
        let s = solve::<N>(&dir, lvl, steps, concrete, keep, !last, save_marks, prefer, known.as_ref(), cfg)?;
        res.levels.push(LevelResult { spec: lvl.to_string(), dir: format!("{prefix}level{li:02}"), forward_s, first_win, arc_bound: s.arc });
        let Some(bound) = s.arc else { return Ok(None) };
        println!("ARC BOUND: no win before f{bound} (level {li}, {lvl}: remainder-exact, the level's objects over-approximated)");
        if last {
            return Ok(Some(s));
        }
        prev = Some((s.arc_marks.expect("asked for"), lvl));
    }
    anyhow::bail!("no level given")
}

/// `rewrite check-known`: a KNOWN route (`inputs`) against the pruning of
/// the finished search in `dir` (levels `level`, horizon `horizon` frames):
/// per level the arc phase rerun as the search ran it, without the
/// concrete search, and the route checked at every step (`search::known`,
/// the hull forward's claim too). An error at the first check that drops
/// it.
pub fn check_known(dir: &Path, level: &str, inputs: &[u8], horizon: u32, cfg: &Settings) -> Result<()> {
    match cfg.arith {
        Arith::Fixed => check_known_in::<P8>(dir, level, inputs, horizon, cfg),
        Arith::Float(_) => check_known_in::<LuaF64>(dir, level, inputs, horizon, cfg),
    }
}

fn check_known_in<N: KernelLanes>(base: &Path, level: &str, inputs: &[u8], horizon: u32, cfg: &Settings) -> Result<()> {
    let steps = horizon * cfg.steps_per_frame();
    let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let t0 = std::time::Instant::now();
    let mut prev: Option<(Visited<N>, Level)> = None;
    for (li, &lvl) in levels.iter().enumerate() {
        let last = li + 1 == levels.len();
        let dir = base.join(format!("level{li:02}"));
        anyhow::ensure!(crate::frame::tree_first_win_through::<N>(&dir, steps)?.is_some(), "{}: no tree through step {steps} (run the search first)", dir.display());
        let filter = prev.as_ref().map(|(m, l)| MarkFilter::new(m, *l, cfg.steps_per_frame()));
        let route = super::known::Route { inputs, filter: filter.as_ref(), hulls: true };
        let s = solve::<N>(&dir, lvl, steps, false, false, !last, None, None, Some(&route), cfg)?;
        let f = s.known.ok_or_else(|| anyhow::anyhow!("the known route does not exit by f{horizon}: nothing to check"))?;
        println!("[check-known] level {li} ({lvl}): the known route (exit f{f}) survives (arc bound {:?})", s.arc);
        if last {
            break;
        }
        prev = Some((s.arc_marks.expect("asked for"), lvl));
    }
    println!("[check-known] every level passes ({:.1} s)", t0.elapsed().as_secs_f64());
    Ok(())
}
