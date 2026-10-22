//! THE CHECKS of a finished tree (`rewrite check ...`), each against
//! something independent: the kernels against the reference engine row by
//! row (`ref`), the recorded transfers against it (`arcs`), the widening
//! table's two sides (`widen`), a known route against the search's pruning
//! (`known`), the tree against its own recorded graph (`replay`), and the
//! kernels' tree against the reference engine's (`reference`). The
//! sampled ones run on one harness (`differential`).

use anyhow::Result;
use celeste_engine::runtime2::Rt2;
use celeste_rust::abstraction::Level;
use celeste_rust::frame::{wins_of, FrameStep};
use celeste_rust::search::inspect::{brief, cell_names, project_all, project_onto, project_row, Proj};
use celeste_rust::search::rem::Rem;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::Settings;
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use std::path::PathBuf;

use celeste_rust::compiled::{KernelLanes, Kernels};
use celeste_rust::differential;

use crate::{where_, Rows};

#[derive(clap::Subcommand)]
pub enum Command {
    /// THE KERNELS AGAINST THE REFERENCE ENGINE, row by row.
    ///
    /// `--samples` stored rows of `--step` (`--row N`: the N-th alone),
    /// each one step through the tree level's kernels and through
    /// `RefEngine`, both projected onto the level and compared by named
    /// fields. A reference successor the kernels miss is a SOUNDNESS gap; a
    /// kernel successor the reference never makes is a PRECISION gap. Every
    /// row, kernel successor and reference leaf must also convert alike on
    /// the tracer's side and the reference's (`block::check_agreement`).
    /// With `--row`, the row's named fields and every kernel successor (its
    /// position and the fields that differ from the row) are printed too.
    Ref(RefArgs),
    /// THE TRANSFERS of a tree's edges, against the reference engine.
    ///
    /// Every recorded edge carries a transfer; then `--samples` records per
    /// frame, the source row stepped by the REFERENCE engine at the tree's
    /// level (all 64 inputs, the row's unknowns forked as the kernels fork
    /// them: `RefEngine::step_at`): at remainders INSIDE the guard some
    /// input must reach the target with the predicted remainder and every
    /// one that reaches it must land where some record of that (pred,
    /// target) taking the point predicts, and at points no record of the
    /// pair covers none may. Fails (exit != 0) on any disagreement.
    Arcs(ArcsArgs),
    /// THE WIDENING TABLE'S TWO SIDES AGREE (`celeste_engine::widening`).
    ///
    /// Every row stored at `--step` (widened in the kernels' graph) is a
    /// fixed point of the block side's widening at the tree's level
    /// (`Rt2::widen_to`), compared exactly, and so is every row the kernels
    /// emit from `--emitted` of them (evenly spaced). A row the block side
    /// changes is printed with the fields that moved; the run exits
    /// non-zero. Not mid split frame at a near level: there the graph side
    /// keeps a floor's computed `collideable` in the player's probe window,
    /// which a frame boundary's widening does not.
    Widen(WidenArgs),
    /// A KNOWN solution against the pruning of a finished search (`search::known`).
    ///
    /// Stepped through the reference engine, at every step it must pass
    /// level -1 (the filter the tree recorded) and the objects ladder's
    /// filter, be in the tree with its exact remainder inside its state's
    /// hull, reached from its parent by a recorded edge that maps the
    /// remainder (whole frames only), and at every frame boundary project
    /// onto an arc-graph node whose W holds its exact remainder, reached (at
    /// a level that filters the next) - per level, the arc phase rerun over
    /// `<search-dir>/level{i:02}` as `search` ran it, without the
    /// concrete search. Exits non-zero on the first check that drops it,
    /// with the step, the node and the state (and, where the tree lost it,
    /// the kernels' successors of its state a frame before).
    Known(KnownArgs),
    /// THE TREE REPLAYS FROM ITS RECORDED GRAPH (`search::hull_replay`).
    ///
    /// The start's hull pushed along the recorded edges through their
    /// transfers, frame by frame, must reach exactly the tree's states first
    /// at each frame (else an error: an edge was not recorded). Prints per
    /// frame the states first reached and the edges recorded into it.
    Replay(ReplayArgs),
    /// THE REFERENCE ENGINE'S FORWARD IS THE KERNELS': the tree's first
    /// `--steps` grown again from the start by the reference engine (as
    /// `forward --reference`, under the tree's settings and level) into a
    /// scratch tree, and both fingerprinted (`ckhash --edges --hulls`): the
    /// states, frontiers and edges with their transfers must agree at every
    /// step. Slow; not at platform (`p`) levels.
    Reference(ReferenceArgs),
}

#[derive(clap::Args)]
pub struct RefArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    #[command(flatten)]
    rows: Rows,
    /// Rows sampled.
    #[arg(long, default_value_t = 20)]
    samples: usize,
    /// Only the N-th state of the frame (in the tree's order: by region,
    /// key, cell; of `--cell`'s if given), its kernel successors printed.
    #[arg(long)]
    row: Option<usize>,
    /// Show up to this many differing successors per row and side.
    #[arg(long, default_value_t = 3)]
    show: usize,
    /// At most this many reference fork paths a row: a row that takes
    /// more is SKIPPED (counted and reported), as in `check arcs`.
    #[arg(long)]
    path_cap: Option<usize>,
    /// A differing row's successors in full (every named field, both
    /// sides, the row itself), not the player's fields alone.
    #[arg(long)]
    full: bool,
}

#[derive(clap::Args)]
pub struct ArcsArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The first step whose records are checked.
    #[arg(long, default_value_t = 1)]
    from_step: u32,
    /// The last step whose records are checked (default: the tree's last).
    #[arg(long)]
    to_step: Option<u32>,
    /// Records sampled per frame.
    #[arg(long, default_value_t = 20)]
    samples: usize,
    /// Check the CHECK: corrupt every decoded transfer in memory first
    /// (`action`: the x action moved by one point; `guard`: the x guard's
    /// lowest point dropped), which must make it fail.
    #[arg(long)]
    fault: Option<String>,
    /// At most this many reference fork paths a step: a sample whose
    /// source takes more is SKIPPED (counted and reported; it bounds a
    /// sample's time and memory where widened objects fork the
    /// reference into thousands of paths).
    #[arg(long)]
    path_cap: Option<usize>,
}

#[derive(clap::Args)]
pub struct WidenArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The step whose stored rows are checked (default: the tree's last).
    #[arg(long)]
    step: Option<u32>,
    /// Also run this many of the rows (evenly spaced) through the kernels and
    /// check what they emit.
    #[arg(long, default_value_t = 0)]
    emitted: usize,
}

#[derive(clap::Args)]
pub struct KnownArgs {
    /// The search's directory (its `result.json` and `level{i:02}` trees;
    /// a dual search's half: `DIR/fixed` or `DIR/float`).
    #[arg(long)]
    search_dir: PathBuf,
    /// The known solution: a `tas/` file or a comma list of input bytes,
    /// the spawn prologue included.
    #[arg(long)]
    inputs: String,
    /// The search's horizon in game frames (default: its `result.json`'s).
    #[arg(long)]
    frames: Option<u32>,
}

#[derive(clap::Args)]
pub struct ReferenceArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The last step compared (default: the tree's last).
    #[arg(long)]
    steps: Option<u32>,
}

#[derive(clap::Args)]
pub struct ReplayArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The last step replayed (default: the tree's last).
    #[arg(long)]
    steps: Option<u32>,
}

/// `check ...` under the run's settings `run`.
pub fn run(run: &Settings, c: Command) -> Result<()> {
    match c {
        Command::Ref(a) => {
            let t = a.tree.open(run)?;
            celeste_rust::with_arith!(t.settings.arith, N => ref_check::<N>(&t, &a))
        }
        Command::Arcs(a) => {
            let t = a.tree.open(run)?;
            let o = celeste_rust::search::arc_check::Opts { dir: &t.dir, from: a.from_step, to: t.frame(a.to_step)?, samples: a.samples, level: t.level, fault: a.fault.as_deref(), path_cap: a.path_cap, settings: &t.settings };
            celeste_rust::with_arith!(t.settings.arith, N => celeste_rust::search::arc_check::run::<N>(&o))
        }
        Command::Widen(a) => {
            let t = a.tree.open(run)?;
            let frame = t.frame(a.step)?;
            celeste_rust::with_arith!(t.settings.arith, N => widen_check::<N>(&t, frame, a.emitted))
        }
        Command::Known(a) => {
            let inputs = celeste_rust::concrete::read_inputs(&a.inputs)?;
            celeste_rust::search::check_known(&a.search_dir, &inputs, a.frames, run)
        }
        Command::Reference(a) => {
            let t = a.tree.open(run)?;
            let to = t.frame(a.steps)?;
            let scratch = PathBuf::from(format!("/var/tmp/celeste-check-reference-{}", std::process::id()));
            let _ = std::fs::remove_dir_all(&scratch);
            let res = celeste_rust::with_arith!(t.settings.arith, N => reference_check::<N>(&t, to, &scratch));
            let _ = std::fs::remove_dir_all(&scratch);
            res
        }
        Command::Replay(a) => {
            use celeste_rust::search::hull_replay::replay;
            let t = a.tree.open(run)?;
            let to = t.frame(a.steps)?;
            let tm = std::time::Instant::now();
            let frames = celeste_rust::with_arith!(t.settings.arith, N => replay::<N>(&t.dir, to, t.settings.threads())?);
            println!("frame states edges");
            for (f, x) in frames.iter().enumerate() {
                println!("f{f:03} {} {}", x.states, x.edges);
            }
            println!("[check replay] {}: the replay reaches the tree's states at every frame to f{to} ({:.1} s)", t.dir.display(), tm.elapsed().as_secs_f64());
            Ok(())
        }
    }
}

/// Every row of `rt2` (of layer `layer`) through ONE forward frame of
/// `engine` (an empty visited set, no filter): the successors and whether
/// one wins.
pub(crate) fn rerun<N: celeste_rust::search::rem::Rem>(engine: &dyn FrameStep<N>, rt2: Rt2<N>, layer: u32) -> Result<(Vec<Rt2<N>>, bool)> {
    let next = celeste_rust::storage::wave::one_frame(engine, vec![rt2], layer + 1)?;
    let mut won = false;
    for b in &next {
        won |= wins_of(b, &engine.settings())?.contains(&true);
    }
    Ok((next, won))
}

/// `check ref` in arithmetic `A`.
fn ref_check<A>(t: &TreeDir, a: &RefArgs) -> Result<()>
where
    A: celeste_rust::transpile::garith::GraphArith + Rem + KernelLanes,
{
    let (cfg, level) = (&t.settings, t.level);
    let kernels = Kernels::<A>::new(cfg, level)?;
    kernels.warm();
    // Every candidate state, then `samples` evenly spaced.
    let (tree, frame, all) = a.rows.states::<A>(t)?;
    let picked: Vec<Sk> = match a.row {
        Some(r) => vec![*all.get(r).ok_or_else(|| anyhow::anyhow!("--row {r}: step {frame} has {} states", all.len()))?],
        None => differential::spread(all.len(), a.samples).into_iter().map(|k| all[k]).collect(),
    };
    let n = picked.len();
    let ids = celeste_rust::compiled::ids();
    // One row: (its report, reference-only successors, kernel-only,
    // skipped over the path cap, lanes converted alike).
    let one = |reference: &mut celeste_rust::engines::reference::ReferenceEngine<A>, sk: Sk| -> Result<(String, usize, usize, usize, usize)> {
        let rt2 = tree.row(frame, sk, cfg.room)?;
        let at = where_(sk.cell());
        let id = sk.show();
        let mut alike = 0;
        let mut agree = |reference: &celeste_rust::engines::reference::ReferenceEngine<A>, b: &Rt2<A>| -> Result<()> {
            reference.check_agreement(b).map_err(|e| e.context(format!("state {id} at {at}: the one converter")))?;
            alike += b.width;
            Ok(())
        };
        agree(reference, &rt2)?;
        // The kernels' successors (with `--row`, printed).
        let (next, _) = rerun(&kernels, rt2.clone_block(), frame)?;
        let mut kset: std::collections::BTreeSet<Proj> = Default::default();
        let mut shown = String::new();
        let me = project_row(&rt2, &cell_names(&rt2, ids), 0, &[]);
        if a.row.is_some() {
            shown += &format!("    the row: {}\n", me.iter().map(|(k, v)| format!("{k}={v}")).collect::<Vec<_>>().join(" "));
        }
        for mut b in next {
            agree(reference, &b)?;
            if a.row.is_some() {
                let (cells, names) = (celeste_rust::storage::cells::block_cells(&b)?, cell_names(&b, ids));
                for i in 0..b.width {
                    let p = project_row(&b, &names, i as u32, &[]);
                    let changed: Vec<String> = p.iter().filter(|(k, v)| me.get(*k) != Some(*v)).map(|(k, v)| format!("{k}={v}")).collect();
                    shown += &format!("    -> {} {}\n", where_(cells[i]), changed.join(" "));
                }
            }
            kset.extend(project_onto(&mut b, level));
        }
        // The reference engine's, from the row projected onto the level.
        let mut input = rt2.clone_block();
        input.widen_to(level);
        let mut rset: std::collections::BTreeSet<Proj> = Default::default();
        let leaves = match reference.run_lane(&input, 0) {
            Err(e) if e.downcast_ref::<celeste_rust::engines::reference::TooManyPaths>().is_some() => {
                return Ok((format!("[check ref] state {id} at {at}: SKIPPED ({e})\n"), 0, 0, 1, alike));
            }
            other => other?,
        };
        for (mut leaf, _) in leaves {
            agree(reference, &leaf)?;
            rset.extend(project_onto(&mut leaf, level));
        }
        let missing: Vec<&Proj> = rset.difference(&kset).collect();
        let extra: Vec<&Proj> = kset.difference(&rset).collect();
        let verdict = if missing.is_empty() && extra.is_empty() { "ok" } else { "DIFFERS" };
        let mut out = format!("[check ref] state {id} at {at}: kernels {} successors, reference {}: {verdict} ({} only in the reference, {} only in the kernels)\n{shown}", kset.len(), rset.len(), missing.len(), extra.len());
        let says = |p: &Proj| if a.full { format!("{p:?}") } else { brief(p) };
        for p in missing.iter().take(a.show) {
            out += &format!("    only in the REFERENCE (soundness gap): {}\n", says(p));
        }
        for p in extra.iter().take(a.show) {
            out += &format!("    only in the KERNELS (precision gap): {}\n", says(p));
        }
        if a.full && verdict != "ok" {
            out += &format!("    the row: {:?}\n", project_all(&rt2));
            for p in kset.iter().take(a.show) {
                out += &format!("    a kernel successor: {p:?}\n");
            }
        }
        Ok((out, missing.len(), extra.len(), 0, alike))
    };
    // The rows in parallel (the kernels' engine shared, a reference
    // engine per worker), reported in row order.
    let done = differential::run(
        cfg.threads(),
        n,
        || {
            let mut reference = celeste_rust::engines::reference::ReferenceEngine::<A>::new(cfg)?.at_level(level);
            if let Some(cap) = a.path_cap {
                reference.set_path_cap(cap);
            }
            Ok(reference)
        },
        |reference, k| one(reference, picked[k]),
    )?;
    let (mut sound_gaps, mut precision_gaps, mut rows_bad, mut skipped, mut alike) = (0usize, 0usize, 0usize, 0usize, 0usize);
    for (text, missing, extra, skip, lanes) in done.into_iter().map(|r| match r {
        differential::Ran::Done(r) => r,
        // (`one` takes its own skips, its lanes counted.)
        differential::Ran::Skipped(why) => (format!("[check ref] SKIPPED ({why})\n"), 0, 0, 1, 0),
    }) {
        print!("{text}");
        sound_gaps += missing;
        precision_gaps += extra;
        rows_bad += (missing + extra > 0) as usize;
        skipped += skip;
        alike += lanes;
    }
    println!("[check ref] {n} rows ({skipped} SKIPPED over the path cap): {rows_bad} differ; {sound_gaps} successors only in the reference, {precision_gaps} only in the kernels");
    println!("[check ref] the one converter: {alike} lanes (the rows, their kernel successors and reference leaves) convert alike on both sides");
    celeste_rust::compiled::dispatch::print_kernel_hits();
    differential::verdict(n - skipped, sound_gaps, || format!("check ref: {sound_gaps} reference successors the kernels miss (a SOUNDNESS gap)"))
}

/// `check widen` of `frame` in the arithmetic of `N`.
fn widen_check<N: Rem + KernelLanes>(t: &TreeDir, frame: u32, emitted: usize) -> Result<()> {
    let (cfg, level): (&Settings, Level) = (&t.settings, t.level);
    anyhow::ensure!(
        !(level.floors_near && !cfg.clock().boundary(frame)),
        "step {frame} is mid split frame at a near level: the graph side keeps a floor's computed `collideable` there, the block side does not"
    );
    // Every lane of `rt2` against its block-side widening, exactly:
    // the lanes it moved (the first few printed, `shown` so far).
    let check = |rt2: &Rt2<N>, what: &str, shown: usize| -> usize {
        let mut moved = 0;
        let mut a = rt2.clone_block();
        a.canonical();
        let mut b = rt2.clone_block();
        b.widen_to(level);
        b.canonical();
        let (ea, eb) = (celeste_engine::exact::exact_rows(&a), celeste_engine::exact::exact_rows(&b));
        for l in 0..rt2.width {
            if a.shape_hash == b.shape_hash && ea[l] == eb[l] {
                continue;
            }
            moved += 1;
            if shown + moved <= 8 {
                let (pa, pb) = (project_all(&a).swap_remove(l), project_all(&b).swap_remove(l));
                println!("[check widen] {what} row {l} of shape {:#x} MOVED: {}", a.shape_hash, differential::field_diff(&pa, &pb).join(", "));
            }
        }
        moved
    };
    let (mut stored, mut emitted_rows, mut moved) = (0usize, 0usize, 0usize);
    let tree = Tree::<N>::open(&t.dir, frame)?;
    let blocks = tree.rows(frame, cfg.room, 1 << 20, &|_| true, cfg.threads())?;
    for (_, rt2) in &blocks {
        stored += rt2.width;
        moved += check(rt2, "stored", moved);
    }
    if emitted > 0 {
        let kernels = Kernels::<N>::new(cfg, level)?;
        kernels.warm();
        let all: Vec<(usize, u32)> = blocks.iter().enumerate().flat_map(|(bi, (_, rt2))| (0..rt2.width as u32).map(move |r| (bi, r))).collect();
        for k in differential::spread(all.len(), emitted) {
            let (bi, r) = all[k];
            let mut rt2 = blocks[bi].1.clone_block();
            rt2.retain_lanes(&[r]);
            for b in rerun(&kernels, rt2, frame)?.0 {
                emitted_rows += b.width;
                moved += check(&b, "emitted", moved);
            }
        }
    }
    println!("[check widen] step {frame} at {level}: {stored} stored rows, {emitted_rows} emitted from {emitted}; {moved} moved by the block side's widening");
    differential::verdict(stored + emitted_rows, moved, || format!("check widen: {moved} rows the block side's widening changes (the table's two sides disagree)"))
}

/// `check reference` of `t` to step `to` in arithmetic `N`, the reference
/// engine's tree grown in `scratch`.
fn reference_check<N: Rem>(t: &TreeDir, to: u32, scratch: &std::path::Path) -> Result<()> {
    use celeste_rust::engines::reference::{RefFrameStep, ReferenceEngine};
    let (cfg, level) = (&t.settings, t.level);
    let tm = std::time::Instant::now();
    let engine: Box<dyn FrameStep<N>> = Box::new(RefFrameStep::<N>::new(cfg, level)?);
    celeste_rust::frame::grow_tree(scratch, to, cfg.minus_one(), None, level, || Ok(engine), || Ok(vec![ReferenceEngine::<N>::new(cfg)?.initial()?.canonicalized()]), cfg)?;
    let kernels = celeste_rust::storage::fingerprint::<N>(&t.dir, to, true, true, cfg.threads())?;
    let reference = celeste_rust::storage::fingerprint::<N>(scratch, to, true, true, cfg.threads())?;
    let mut bad = 0;
    for (k, r) in kernels.iter().zip(&reference) {
        if k != r {
            bad += 1;
            println!("[check reference] the kernels' {k}\n[check reference] the reference's {r}");
        }
    }
    bad += kernels.len().abs_diff(reference.len());
    println!("[check reference] {}: steps 0-{to} at {level}, {} fingerprint lines, {bad} differ ({:.1} s)", t.dir.display(), kernels.len(), tm.elapsed().as_secs_f64());
    differential::verdict(kernels.len(), bad, || format!("check reference: {bad} of the kernels' fingerprint lines are not the reference engine's"))
}
