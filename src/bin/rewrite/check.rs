//! THE CHECKS of a finished tree (`rewrite check ...`), each against
//! something independent: the kernels against the reference engine row by
//! row (`ref`), the recorded transfers against it (`arcs`), the widening
//! table's two sides (`widen`), a known route against the search's pruning
//! (`known`), and the tree against its own recorded graph (`replay`).

use anyhow::Result;
use celeste_engine::runtime2::Rt2;
use celeste_rust::abstraction::Level;
use celeste_rust::frame::{widen_rt2_to, wins_of, Block, FrameStep};
use celeste_rust::search::inspect::{brief, cell_names, project_all, project_onto, project_row, Proj};
use celeste_rust::search::rem::Rem;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::P8;
use std::path::PathBuf;

use crate::{where_, KernelsNew, Rows};

#[derive(clap::Subcommand)]
pub enum Command {
    /// THE KERNELS AGAINST THE REFERENCE ENGINE, row by row.
    ///
    /// `--samples` stored rows of `--frame` (`--row N`: the N-th alone),
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
    /// Every row stored at `--frame` (widened in the kernels' graph) is a
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
    /// `<checkpoint-dir>/level{i:02}` as `search` ran it, without the
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
}

#[derive(clap::Args)]
pub struct RefArgs {
    /// The tree (a forward's checkpoint dir, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
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
    /// The tree (a forward's checkpoint dir, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
    /// The first frame whose records are checked.
    #[arg(long, default_value_t = 1)]
    from: u32,
    /// The last frame whose records are checked (default: the tree's last).
    #[arg(long)]
    to: Option<u32>,
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
    /// The tree (a forward's checkpoint dir, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
    /// The step whose stored rows are checked (default: the tree's last).
    #[arg(long)]
    frame: Option<u32>,
    /// Also run this many of the rows (evenly spaced) through the kernels and
    /// check what they emit.
    #[arg(long, default_value_t = 0)]
    emitted: usize,
}

#[derive(clap::Args)]
pub struct KnownArgs {
    /// The search's checkpoint dir (its `result.json` and `level{i:02}`
    /// trees; a dual search's half: `DIR/fixed` or `DIR/float`).
    #[arg(long)]
    checkpoint_dir: PathBuf,
    /// The known solution: a `tas/` file or a comma list of input bytes,
    /// the spawn prologue included.
    #[arg(long)]
    inputs: String,
    /// The search's horizon in frames (default: its `result.json`'s).
    #[arg(long)]
    horizon: Option<u32>,
}

#[derive(clap::Args)]
pub struct ReplayArgs {
    /// The tree (a forward's checkpoint dir, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
    /// The last frame replayed (default: the tree's last).
    #[arg(long)]
    to: Option<u32>,
}

/// `check ...` under the run's settings `run`.
pub fn run(run: &Settings, c: Command) -> Result<()> {
    match c {
        Command::Ref(a) => {
            let t = TreeDir::open(&a.tree, run)?;
            match t.settings.arith {
                Arith::Fixed => ref_check::<celeste_rust::compiled::FrameEngine, celeste_core::arith::Fixed>(&t, &a),
                Arith::Float(_) => ref_check::<celeste_rust::compiled::FloatKernels, celeste_core::arith::Float>(&t, &a),
            }
        }
        Command::Arcs(a) => {
            let t = TreeDir::open(&a.tree, run)?;
            let o = celeste_rust::search::arc_check::Opts { dir: &t.dir, from: a.from, to: t.frame(a.to)?, samples: a.samples, level: t.level, fault: a.fault.as_deref(), path_cap: a.path_cap, settings: &t.settings };
            match t.settings.arith {
                Arith::Fixed => celeste_rust::search::arc_check::run::<P8>(&o),
                Arith::Float(_) => celeste_rust::search::arc_check::run::<LuaF64>(&o),
            }
        }
        Command::Widen(a) => {
            let t = TreeDir::open(&a.tree, run)?;
            let frame = t.frame(a.frame)?;
            match t.settings.arith {
                Arith::Fixed => widen_check::<celeste_rust::compiled::FrameEngine, P8>(&t, frame, a.emitted),
                Arith::Float(_) => widen_check::<celeste_rust::compiled::FloatKernels, LuaF64>(&t, frame, a.emitted),
            }
        }
        Command::Known(a) => {
            let inputs = celeste_rust::concrete::read_inputs(&a.inputs)?;
            celeste_rust::search::check_known(&a.checkpoint_dir, &inputs, a.horizon, run)
        }
        Command::Replay(a) => {
            use celeste_rust::search::hull_replay::replay;
            let t = TreeDir::open(&a.tree, run)?;
            let to = t.frame(a.to)?;
            let tm = std::time::Instant::now();
            let frames = match t.settings.arith {
                Arith::Fixed => replay::<P8>(&t.dir, to, t.settings.threads())?,
                Arith::Float(_) => replay::<LuaF64>(&t.dir, to, t.settings.threads())?,
            };
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
pub(crate) fn rerun<N: celeste_rust::search::rem::Rem>(engine: &dyn FrameStep<N>, rt2: Rt2<N>, layer: u32) -> Result<(Vec<Block<N>>, bool)> {
    let next = celeste_rust::storage::wave::one_frame(engine, vec![Block::from_rt2(rt2)], layer + 1)?;
    let mut won = false;
    for b in &next {
        won |= wins_of(b.rt2(), &engine.settings())?.contains(&true);
    }
    Ok((next, won))
}

/// `check ref` in arithmetic `A`, its kernels `K`.
fn ref_check<K, A>(t: &TreeDir, a: &RefArgs) -> Result<()>
where
    K: FrameStep<A::Num> + KernelsNew,
    A: celeste_rust::transpile::garith::GraphArith,
    A::Num: Rem,
{
    let (cfg, level) = (&t.settings, t.level);
    let kernels = K::new(cfg, level)?;
    kernels.warm();
    // Every candidate state, then `samples` evenly spaced.
    let (tree, frame, all) = a.rows.states::<A::Num>(t)?;
    let geo = celeste_rust::storage::GEOMETRY;
    let picked: Vec<Sk> = match a.row {
        Some(r) => vec![*all.get(r).ok_or_else(|| anyhow::anyhow!("--row {r}: step {frame} has {} states", all.len()))?],
        None => {
            let n = a.samples.min(all.len());
            (0..n).map(|k| all[k * all.len() / n]).collect()
        }
    };
    let n = picked.len();
    let ids = celeste_rust::compiled::ids();
    // One row: (its report, reference-only successors, kernel-only,
    // skipped over the path cap, lanes converted alike).
    let one = |reference: &mut celeste_rust::engines::reference::ReferenceEngine<A>, sk: Sk| -> Result<(String, usize, usize, usize, usize)> {
        let rt2 = tree.row(frame, sk, cfg.room)?;
        let at = where_(sk.cell(&geo));
        let id = sk.show();
        let mut alike = 0;
        let mut agree = |reference: &celeste_rust::engines::reference::ReferenceEngine<A>, b: &Rt2<A::Num>| -> Result<()> {
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
        for b in next {
            agree(reference, b.rt2())?;
            if a.row.is_some() {
                let (cells, names) = (celeste_rust::search::pos_graph::block_cells(b.rt2())?, cell_names(b.rt2(), ids));
                for i in 0..b.rt2().width {
                    let p = project_row(b.rt2(), &names, i as u32, &[]);
                    let changed: Vec<String> = p.iter().filter(|(k, v)| me.get(*k) != Some(*v)).map(|(k, v)| format!("{k}={v}")).collect();
                    shown += &format!("    -> {} {}\n", where_(cells[i]), changed.join(" "));
                }
            }
            kset.extend(project_onto(&mut b.into_rt2(), level));
        }
        // The reference engine's, from the row projected onto the level.
        let mut input = rt2.clone_block();
        widen_rt2_to(&mut input, level);
        let mut rset: std::collections::BTreeSet<Proj> = Default::default();
        let leaves = match reference.run_lane(&input, 0) {
            Err(e) if e.downcast_ref::<celeste_rust::engines::reference::TooManyPaths>().is_some() => {
                return Ok((format!("[check ref] state {id} at {at}: SKIPPED ({e})\n"), 0, 0, 1, alike));
            }
            other => other?,
        };
        for (leaf, _) in leaves {
            agree(reference, leaf.rt2())?;
            rset.extend(project_onto(&mut leaf.into_rt2(), level));
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
    let done = celeste_rust::par::Pool::new(cfg.threads()).map_with(
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
    for (text, missing, extra, skip, lanes) in done {
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
    anyhow::ensure!(sound_gaps == 0, "check ref: {sound_gaps} reference successors the kernels miss (a SOUNDNESS gap)");
    Ok(())
}

/// `check widen` of `frame` in the arithmetic of `N`, its kernels `K`.
fn widen_check<K, N>(t: &TreeDir, frame: u32, emitted: usize) -> Result<()>
where
    K: FrameStep<N> + KernelsNew,
    N: Rem,
{
    let (cfg, level): (&Settings, Level) = (&t.settings, t.level);
    anyhow::ensure!(
        !(level.floors_near && !cfg.clock().boundary(frame)),
        "step {frame} is mid split frame at a near level: the graph side keeps a floor's computed `collideable` there, the block side does not"
    );
    let ids = celeste_rust::compiled::ids();
    // Every lane of `rt2` against its block-side widening, exactly:
    // the lanes it moved (the first few printed, `shown` so far).
    let check = |rt2: &Rt2<N>, what: &str, shown: usize| -> usize {
        let mut moved = 0;
        let mut a = rt2.clone_block();
        a.canonical();
        let mut b = rt2.clone_block();
        widen_rt2_to(&mut b, level);
        b.canonical();
        let (ea, eb) = (celeste_engine::exact::exact_rows(&a, ids), celeste_engine::exact::exact_rows(&b, ids));
        for l in 0..rt2.width {
            if a.shape_hash == b.shape_hash && ea[l] == eb[l] {
                continue;
            }
            moved += 1;
            if shown + moved <= 8 {
                let (pa, pb) = (project_all(&a).swap_remove(l), project_all(&b).swap_remove(l));
                let diff: Vec<String> = pa.iter().filter(|(k, v)| pb.get(*k) != Some(v)).map(|(k, v)| format!("{k}: {v} -> {}", pb.get(k).map_or("-", |s| s.as_str()))).collect();
                println!("[check widen] {what} row {l} of shape {:#x} MOVED: {}", a.shape_hash, diff.join(", "));
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
        let kernels = K::new(cfg, level)?;
        kernels.warm();
        let all: Vec<(usize, u32)> = blocks.iter().enumerate().flat_map(|(bi, (_, rt2))| (0..rt2.width as u32).map(move |r| (bi, r))).collect();
        let n = emitted.min(all.len());
        for k in 0..n {
            let (bi, r) = all[k * all.len() / n];
            let mut rt2 = blocks[bi].1.clone_block();
            rt2.retain_lanes(&[r]);
            for b in rerun(&kernels, rt2, frame)?.0 {
                emitted_rows += b.lanes();
                moved += check(b.rt2(), "emitted", moved);
            }
        }
    }
    println!("[check widen] step {frame} at {level}: {stored} stored rows, {emitted_rows} emitted from {emitted}; {moved} moved by the block side's widening");
    anyhow::ensure!(moved == 0, "check widen: {moved} rows the block side's widening changes (the table's two sides disagree)");
    Ok(())
}
