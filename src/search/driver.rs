//! THE SEARCH'S DRIVER (`rewrite search`, `rewrite dual`, `rewrite check
//! known`; plans/architecture.md "The search"): per level, coarsest first,
//! the forward (`frame::grow_tree`) and its arc phase (`solve`), then the
//! concrete search at the last level (the dual search: `dual::search` over
//! pairs). The outcome is recorded (`result.json`) before it is returned.

use std::path::Path;

use anyhow::Result;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

use super::backward::{backward, gate_w_hash, optimum, Seeds, Winning, BLOCK};
use super::concrete::{concrete_search, NodeKeys, Side, Witness};
use super::reach::{inside_w, ReachEdges};
use super::rem::{start_point, Rem};
use super::result::{DualResult, LevelResult, SearchResult, Verdict};
use crate::abstraction::Level;
use crate::compiled::KernelLanes;
use crate::frame::{Block, MarkFilter, Visited};
use crate::settings::{Arith, Settings};

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
    /// The UI's arc pass (`--save-marks`; dual: the float half's).
    pub save_marks: Option<&'a Path>,
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
        steps_per_frame: s.clock().per_frame,
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

/// The level -1 table of each of `halves` first: one that cannot be built
/// is a recorded refusal, not a panic somewhere in the forward.
fn tables(halves: &[&Settings], res: &mut SearchResult) -> bool {
    for s in halves {
        if let Some(m) = s.minus_one() {
            if let Err(e) = m.try_table(s) {
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
    let solved = match run_levels::<N>(s, s.clock().steps(spec.horizon), levels, spec.dir, "", finish, spec.save_marks, spec.prefer, res)? {
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

/// The witness file `witness_frame_F.txt` (`header`, the inputs), recorded.
fn witness(dir: &Path, res: &mut SearchResult, w: &Witness, header: &str) -> Result<()> {
    let f = w.inputs.len();
    let path = dir.join(format!("witness_frame_{f}.txt"));
    std::fs::write(&path, format!("{header}{}\n", w.inputs_text()))?;
    res.optimal = Some(f as u32);
    res.witness = Some(w.inputs.clone());
    res.witness_file = Some(path.file_name().expect("a file").to_string_lossy().into_owned());
    Ok(())
}

/// THE DUAL SEARCH (`search::dual`): the fixed half (real play) and the
/// float half each its levels to one horizon (`<dir>/fixed`, `<dir>/float`),
/// then the dual concrete search from the larger arc bound; its witness
/// replayed in both arithmetics.
fn dual(spec: &SearchSpec, float: &Settings, levels: &[Level], res: &mut SearchResult) -> Result<Verdict> {
    let cfg = spec.settings;
    let room = format!("{},{}", cfg.room.0, cfg.room.1);
    let (horizon, steps) = (spec.horizon, cfg.clock().steps(spec.horizon));
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

/// The LEVELS of a search, coarsest first, under `base`: each a forward (a
/// finer level filtered by the coarser one's arc marks) and its arc phase,
/// recorded in `res.levels` (dirs named under `prefix`); the last ends as
/// `finish` says. `None`: a level's arc phase REFUTED the horizon.
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
        // THE FORWARD. Level 0's tree is reused or extended (`grow_tree`
        // refuses one filtered by level -1 below this horizon). A finer
        // level's tree is filtered for one horizon: kept only for the same
        // marks, horizon and level -1, else built again (the cheap one).
        let t = std::time::Instant::now();
        let want = cfg.minus_one();
        let filter = prev.as_ref().map(|(m, l)| MarkFilter::new(m, *l, cfg.clock()));
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
        let initial = || -> Result<Vec<Block<N>>> { Ok(vec![Block::canonical(crate::engines::reference::ReferenceEngine::<N::A>::new(cfg)?.initial()?)?]) };
        let first_win = crate::frame::grow_tree(&dir, steps, want, filter.as_ref(), lvl, engine, initial, cfg)?.win_frame;
        let forward_s = t.elapsed().as_secs_f64();
        // `f` counts search steps.
        eprintln!("[search] level {li} ({lvl}): forward to f{steps} in {forward_s:.1} s; first win {first_win:?}");
        crate::metrics::mem_phase("forward");
        // THE ARC PHASE, and the concrete search at the LAST level only: a
        // coarser level's bound is loose wherever its objects are (a try at
        // it can cost most of a search), and the finer level decides anyway.
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

/// What the arc phase of a search found (`solve`).
pub struct Solved<N: Rem = P8> {
    /// The optimum over the rotation graph, a lower bound on the game's, in
    /// FRAMES (`horizon` and the marks' deadlines are in search steps).
    /// `None`: the horizon is REFUTED.
    pub arc: Option<u32>,
    /// The concrete optimum and its witness, when asked for and found.
    pub concrete: Option<Witness>,
    /// The ARC-MARKED nodes with their deadlines (the last frame their
    /// winning set is non-empty): the next level's `frame::MarkFilter`.
    pub arc_marks: Option<crate::frame::Visited<N>>,
    /// The known route's exit frame when it was checked (`search::known`):
    /// it wins by the horizon and survives every pruning step.
    pub known: Option<u32>,
    /// W and its node keys, with `keep_arcs`: a dual search's half.
    pub arcs: Option<Arcs<N>>,
}

/// A level's arc phase as the concrete searches read it: W, the tree's
/// shapes (W's nodes' regions) and its key space.
pub struct Arcs<N: Rem = P8> {
    pub w: Winning<N::C>,
    pub shapes: Vec<u64>,
    pub space: celeste_engine::exact::KeySpace<N>,
    /// The level its nodes key at, and the horizon in search steps.
    pub level: crate::abstraction::Level,
    pub horizon: u32,
}

/// THE ARC PHASE over a finished tree in `dir`: `backward`, `optimum`, and
/// as much of `concrete_search` as `concrete` says. Prints the `[gate]`
/// fingerprints (over (shape, key, cell), independent of scheduling): W's
/// support, W at every frame, the optimum. With `save`, the UI's arc pass:
/// `level0.marks.bin` (W's support, each node to its last frame),
/// `arc.marks.bin` (what `reach::inside_w` reaches) and the witness. With
/// `known`, checks a known solution against every pruning step first
/// (`search::known`): an error if one drops it.
#[allow(clippy::too_many_arguments)]
pub fn solve<N: crate::compiled::KernelLanes>(
    dir: &std::path::Path,
    level: crate::abstraction::Level,
    horizon: u32,
    concrete: bool,
    keep_arcs: bool,
    want_marks: bool,
    save: Option<&std::path::Path>,
    prefer: Option<&[u8]>,
    known: Option<&super::known::Route<N>>,
    s: &crate::settings::Settings,
) -> anyhow::Result<Solved<N>> {
    use crate::frame::{mark_row, save_marks, MarkRow, Visited};
    s.check(dir, level)?;
    crate::metrics::mem_phase("arc: start");
    let threads = s.threads();
    let seeds = Seeds::<N>::load(dir, horizon)?;
    crate::metrics::mem_phase("arc: edge files opened");
    let tb = std::time::Instant::now();
    // The closed spans and W's nodes go to scratch files beside the tree
    // (mapped, unlinked).
    let scratch = dir.join("arc-scratch");
    if scratch.exists() {
        std::fs::remove_dir_all(&scratch)?;
    }
    let w = backward(&seeds.store, &seeds.pairs, &seeds.wins, horizon, threads, Some(&scratch))?;
    crate::metrics::mem_phase("arc: backward");
    let tb = tb.elapsed().as_secs_f64();
    let start = w.index(seeds.start);
    let p0 = start_point::<N>();
    let arc_steps = optimum(&w, start, p0, horizon);
    // In frames: a win at step s is during the frame it completes (a lower
    // bound still: a frame's win is a win at its last step at the latest).
    let arc = arc_steps.map(|k| s.clock().frame_of(k));
    eprintln!("[arc] backward {tb:.2} s; optimum {arc_steps:?} (steps)");
    // Fingerprints over CONTENT (`exact::ContentHash`): independent of the
    // order the tree's dictionaries grew in.
    let content = seeds.space.content_hash();
    // Per frame, the nodes with a set and the sum of their (node, set)
    // hashes: each span added at its first frame and taken off past its
    // last (`delta`), summed frame by frame; the sums commute, so W's
    // nodes go in parallel block ranges.
    type Part = (u64, Vec<(i64, u64)>, Vec<MarkRow>);
    let blocks = w.len().div_ceil(BLOCK);
    let parts: Vec<anyhow::Result<Part>> = crate::par::Pool::new(threads).chunks(blocks, 4096, |lo, hi| {
        let (mut fp, mut delta, mut rows) = (0u64, vec![(0i64, 0u64); horizon as usize + 2], Vec::new());
        for x in w.nodes_from(lo * BLOCK, hi * BLOCK) {
            let (shape, key, cell) = seeds.state(x.id);
            // `Visited::fingerprint`: the nodes are distinct states.
            let h = content.state(shape, key, cell);
            fp = fp.wrapping_add(h);
            for (lo, hi, r) in x.spans() {
                let g = gate_w_hash(h, r);
                delta[lo as usize] = (delta[lo as usize].0 + 1, delta[lo as usize].1.wrapping_add(g));
                delta[hi as usize + 1] = (delta[hi as usize + 1].0 - 1, delta[hi as usize + 1].1.wrapping_sub(g));
            }
            if save.is_some() {
                rows.push(mark_row(shape, key, cell, x.last() as u16, horizon));
            }
        }
        Ok((fp, delta, rows))
    });
    let (mut fp, mut delta, mut rows) = (0u64, vec![(0i64, 0u64); horizon as usize + 2], Vec::<MarkRow>::new());
    for part in parts {
        let (f, d, r) = part?;
        fp = fp.wrapping_add(f);
        for (a, b) in delta.iter_mut().zip(d) {
            *a = (a.0 + b.0, a.1.wrapping_add(b.1));
        }
        rows.extend(r);
    }
    println!("[gate] h{horizon} support {} {fp:016x}", w.len());
    if let Some(out) = save {
        std::fs::create_dir_all(out)?;
        save_marks(&out.join("level0.marks.bin"), std::mem::take(&mut rows), horizon, &seeds.space)?;
    }
    let (mut n, mut acc) = (0i64, 0u64);
    for (t, d) in delta[..=horizon as usize].iter().enumerate() {
        (n, acc) = (n + d.0, acc.wrapping_add(d.1));
        println!("[gate] h{horizon} W f{t:03} {n} {acc:016x}");
    }
    println!("[gate] h{horizon} arc optimum {}", arc_steps.map_or("none".to_string(), |f| f.to_string()));
    crate::metrics::mem_phase("arc: fingerprints");
    // Per node the LAST frame it is reached inside W from the start (`reach::inside_w`;
    // at most its W deadline): the next level's filter, and the UI's arc marks.
    let mut arc_marks = want_marks.then(|| Visited::new(seeds.space.clone()));
    if want_marks || save.is_some() {
        let tr = std::time::Instant::now();
        let mut g = ReachEdges::new(&seeds.store, &w, seeds.win_ids(), threads);
        let last = inside_w(&w, &mut g, &seeds.pairs, start, p0, horizon, threads)?;
        eprintln!("[arc] reach {:.2} s ({} reached nodes' {} out-edges loaded)", tr.elapsed().as_secs_f64(), g.sizes().0, g.sizes().1);
        drop(g);
        crate::metrics::mem_phase("arc: reach");
        for i in (0..w.len() as u32).filter(|&i| last[i as usize] != u32::MAX) {
            let (shape, key, cell) = seeds.state(w.id(i));
            let d = last[i as usize] as u16;
            if let Some(am) = &mut arc_marks {
                am.insert_until(shape, key, cell, d);
            }
            if save.is_some() {
                rows.push(mark_row(shape, key, cell, d, horizon));
            }
        }
    }
    if let Some(out) = save {
        save_marks(&out.join("arc.marks.bin"), rows, horizon, &seeds.space)?;
    }
    // The concrete search reads W and the node keys, not the edge files.
    let Seeds { shapes, space, store, .. } = seeds;
    drop(store);
    let keyed = (concrete && arc.is_some()) || known.is_some();
    let side = if keyed { Some(Side { w: &w, node: NodeKeys::new(&w, &shapes)?, keys: &space, level, s, seeded: s.concrete().1 }) } else { None };
    let known = match (known, &side) {
        (Some(route), Some(side)) => {
            let minus_one = crate::frame::TreeFilter::of_tree(dir, s)?;
            // The hull forward's claim keys every step's state: whole frames only.
            if route.hulls && s.split_frame {
                eprintln!("[known] the hull forward's claim is checked on whole frames only: not under --split-frame");
            }
            let hulls = (route.hulls && !s.split_frame).then(|| super::known::Hulls::open(dir, horizon, s)).transpose()?;
            let p = super::known::Pruning { side, horizon, start, reached: arc_marks.as_ref(), minus_one, dir, hulls: hulls.as_ref() };
            super::known::check(route, &p)?
        }
        _ => None,
    };
    crate::metrics::mem_phase("arc: marks for the next level, edge files closed");
    let found = match (concrete, arc, &side) {
        (true, Some(f), Some(side)) => concrete_search(side, horizon, f, prefer)?,
        _ => None,
    };
    if let Some(wt) = &found {
        println!("[gate] h{horizon} concrete optimum {} inputs {}", wt.inputs.len(), wt.inputs_text());
        if let Some(out) = save {
            wt.save(&out.join("witness.txt"))?;
        }
    }
    drop(side);
    let arcs = keep_arcs.then(|| Arcs { w, shapes, space, level, horizon });
    Ok(Solved { arc, concrete: found, arc_marks, known, arcs })
}

/// `rewrite check known`: a KNOWN route (`inputs`) against the pruning of
/// the finished search in `dir`: per level the arc phase rerun as the
/// search ran it (no concrete search), the route checked at every step
/// (`search::known`, the hull forward's claim too). Levels and horizon from
/// `result.json` (a dual half `DIR/fixed` reads `DIR`'s) unless `horizon`;
/// each tree's settings from its record, the run's own from `run`. An error
/// at the first check that drops the route.
pub fn check_known(dir: &Path, inputs: &[u8], horizon: Option<u32>, run: &Settings) -> Result<()> {
    let res = SearchResult::read(dir).or_else(|e| match dir.parent() {
        Some(p) if super::result::path(p).exists() => SearchResult::read(p),
        _ => Err(e),
    })?;
    let horizon = horizon.unwrap_or(res.horizon);
    let mut trees = Vec::new();
    for (li, spec) in res.level.split(',').enumerate() {
        let d = dir.join(format!("level{li:02}"));
        let (s, lvl) = Settings::of_tree(&d, run)?;
        anyhow::ensure!(lvl.to_string() == Level::parse(spec).map_err(|e| anyhow::anyhow!(e))?.to_string(), "{}: level {lvl}, the search's record says {spec}", d.display());
        trees.push((d, s, lvl));
    }
    match trees[0].1.arith {
        Arith::Fixed => check_known_in::<P8>(&trees, inputs, horizon),
        Arith::Float(_) => check_known_in::<LuaF64>(&trees, inputs, horizon),
    }
}

fn check_known_in<N: KernelLanes>(trees: &[(std::path::PathBuf, Settings, Level)], inputs: &[u8], horizon: u32) -> Result<()> {
    let t0 = std::time::Instant::now();
    let mut prev: Option<(Visited<N>, Level)> = None;
    for (li, (dir, cfg, lvl)) in trees.iter().enumerate() {
        let (last, lvl) = (li + 1 == trees.len(), *lvl);
        let steps = cfg.clock().steps(horizon);
        anyhow::ensure!(crate::frame::tree_first_win_through::<N>(dir, steps)?.is_some(), "{}: no tree through step {steps} (run the search first)", dir.display());
        let filter = prev.as_ref().map(|(m, l)| MarkFilter::new(m, *l, cfg.clock()));
        let route = super::known::Route { inputs, filter: filter.as_ref(), hulls: true };
        let s = solve::<N>(dir, lvl, steps, false, false, !last, None, None, Some(&route), cfg)?;
        let f = s.known.ok_or_else(|| anyhow::anyhow!("the known route does not exit by f{horizon}: nothing to check"))?;
        println!("[check known] level {li} ({lvl}): the known route (exit f{f}) survives (arc bound {:?})", s.arc);
        if last {
            break;
        }
        prev = Some((s.arc_marks.expect("asked for"), lvl));
    }
    println!("[check known] every level passes ({:.1} s)", t0.elapsed().as_secs_f64());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// THE BREADTH-FIRST LAYERS of the concrete and dual searches (the gates'
    /// witnesses come from the depth-first try, skipped here: `NO_DFS`). Room
    /// (1,0)'s arc gate in 16.16, in Celia's floats and dual: each optimum
    /// and witness pinned, the same at 3 and 32 threads (the layers merge
    /// their chunks in order).
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: room (1,0)'s kernels in both arithmetics, three searches at two thread counts, ~60 CPU-s"]
    fn the_breadth_first_searches_are_pinned() {
        super::super::concrete::NO_DFS.store(true, std::sync::atomic::Ordering::Relaxed);
        let base = std::env::temp_dir().join(format!("celeste-bfs-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let mut runs: Vec<String> = Vec::new();
        for threads in [3, 32] {
            let fixed = Settings { win_at: Some((9, 9, 101, 101)), threads: Some(threads), ..Settings::default() };
            let float = Settings { arith: Arith::Float(crate::settings::FloatTool::Celia), ..fixed.clone() }.resolved().expect("float settings");
            let dual = fixed.float_half(crate::settings::FloatTool::Celia, None).expect("the float half");
            let mut lines = Vec::new();
            for (name, s, half) in [("search fixed", &fixed, None), ("search float", &float, None), ("dual", &fixed, Some(&dual))] {
                let dir = base.join(format!("{name}-{threads}").replace(' ', "-"));
                let spec = SearchSpec { settings: s, level: "r0sx", horizon: 35, ceiling: false, dir: &dir, bound_only: false, save_marks: None, prefer: None, dual: half };
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
        let _ = std::fs::remove_dir_all(&base);
    }
}
