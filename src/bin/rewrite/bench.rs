//! THE BENCHMARKS of a finished tree (`rewrite bench ...`): one forward
//! frame (`frame`: its fingerprints, its files' bytes and its costs) and
//! one backward frame (`backward`), each repeatable.

use anyhow::{Context, Result};
use celeste_rust::frame::FrameStep;
use celeste_rust::search::rem::Rem;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::Settings;
use std::path::PathBuf;


#[derive(clap::Subcommand)]
pub enum Command {
    /// ONE FORWARD FRAME of a tree, F -> F+1, repeatably.
    ///
    /// The forward as it stood after `--step` F (`ForwardState::at`: the
    /// tree's visited set, transfers and level -1 filter at F;
    /// later frames ignored) runs the wave of F+1 `--reps` times, the edges
    /// into a scratch dir. Prints (stdout, the same every rep, else an
    /// error) `ckhash`'s lines for F+1 - `f{F+1}` of the new states,
    /// `h{F+1}` of the frontier, with `--edges` `e{F+1}` of the edges, with
    /// `--checkpoint` `b{F+1}` over the bytes of the frame's files - so a
    /// tree extended past F checks it; per rep (stderr) the wave, the
    /// frame's end walls and the workers' phases; `kernel lanes: ... missed
    /// 0`. Its PIN: `--expect FILE` (the lines, those of the kinds this run
    /// prints), compared and every difference shown, or written with
    /// `--pin`.
    Frame(FrameArgs),
    /// ONE BACKWARD FRAME of the arc phase, W_F from W_{F+1}, repeatably.
    ///
    /// A finished search tree's edge files to `--steps` opened (timed),
    /// the winning sets pushed down to `W_{F+1}`, then `W_F` from them
    /// `--reps` times from a copy. Prints (stdout) `W_F`'s fingerprint as
    /// the arc gate's line (`[gate] hH W fF count hash`, over each node's
    /// (shape, key, cell) and its set), the same every rep; per rep the
    /// read, pull and merge walls.
    Backward(BackwardArgs),
}

#[derive(clap::Args)]
pub struct FrameArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The step F to run F -> F+1 from (default: the tree's last).
    #[arg(long)]
    step: Option<u32>,
    /// Run the wave this many times, each from a copy (every rep's lines must
    /// agree).
    #[arg(long, default_value_t = 1)]
    reps: usize,
    /// Record the frame's edges (into the run's scratch dir, `Scratch`,
    /// cleared per rep), as the search's forward does.
    #[arg(long)]
    edges: bool,
    /// The printed lines must be this pinned file's lines of their kinds
    /// (with `--pin`: written to it).
    #[arg(long)]
    expect: Option<PathBuf>,
    /// Write `--expect` instead of comparing (a re-pin: only on evidence,
    /// said in the commit).
    #[arg(long)]
    pin: bool,
    /// Checkpoint each rep's frame as the forward does (the frame file,
    /// read back and checked),
    /// into the scratch dir, timed; prints the `b{F+1}` line (needs
    /// `--edges`).
    #[arg(long)]
    checkpoint: bool,
}

#[derive(clap::Args)]
pub struct BackwardArgs {
    #[command(flatten)]
    tree: crate::TreeArg,
    /// The search's horizon, in steps (default: the tree's last); the graph
    /// and the winning sets are built for it.
    #[arg(long)]
    steps: Option<u32>,
    /// The step F whose winning sets are computed (from F+1's).
    #[arg(long)]
    step: u32,
    /// Compute W_F this many times, each from a copy.
    #[arg(long, default_value_t = 1)]
    reps: usize,
}

/// `bench ...` under the run's settings `run`.
pub fn run(run: &Settings, c: Command) -> Result<()> {
    match c {
        Command::Frame(a) => {
            let t = a.tree.open(run)?;
            celeste_rust::with_arith!(t.run.settings.arith, N => bench_frame::<N>(&t, &a))
        }
        Command::Backward(a) => {
            let t = a.tree.open(run)?;
            let horizon = t.frame(a.steps)?;
            celeste_rust::with_arith!(t.run.settings.arith, N => bench_backward::<N>(&t, horizon, a.step, a.reps))
        }
    }
}

/// `bench frame` in the arithmetic of `N`.
fn bench_frame<N: Rem + celeste_rust::compiled::KernelLanes>(t: &TreeDir, a: &FrameArgs) -> Result<()> {
    let (run, dir) = (&t.run, &t.dir);
    let cfg = &*run.settings;
    let (edges, checkpoint, reps) = (a.edges, a.checkpoint, a.reps);
    anyhow::ensure!(!checkpoint || edges, "--checkpoint needs --edges (the frame's files as the forward writes them)");
    anyhow::ensure!(!a.pin || a.expect.is_some(), "--pin writes --expect: give it");
    anyhow::ensure!(!a.pin || a.expect.is_none() || checkpoint, "--pin --expect: a pin holds every line (--edges --checkpoint)");
    let frame = t.frame(a.step)?;
    let engine = celeste_rust::compiled::Kernels::<N>::new(run)?;
    let t = std::time::Instant::now();
    engine.warm();
    let t_kernels = t.elapsed().as_secs_f64();
    eprintln!("[bench] kernels built: rss {:.2} GB", celeste_rust::metrics::current_rss_gb());
    let t = std::time::Instant::now();
    let mut st = celeste_rust::frame::ForwardState::<N>::at(dir, frame, run)?;
    eprintln!(
        "[bench] kernels built in {t_kernels:.2} s; f{frame} loaded in {:.2} s: {} lanes in {} entries ({:.3} GB, {:.2} B a state) and {} old states, visited {} states in {} entries ({:.3} GB, {:.2} B a state); {} threads; rss {:.2} GB",
        t.elapsed().as_secs_f64(),
        st.frontier().new.states(),
        st.frontier().new.entries(),
        st.frontier().new.alloc_bytes() as f64 / 1e9,
        st.frontier().new.alloc_bytes() as f64 / st.frontier().new.states().max(1) as f64,
        st.frontier().old_states(),
        st.visited_len(),
        st.visited().entries(),
        st.visited().alloc_bytes() as f64 / 1e9,
        st.visited().alloc_bytes() as f64 / st.visited_len().max(1) as f64,
        cfg.threads(),
        celeste_rust::metrics::current_rss_gb()
    );
    let scratch_dir = Scratch::new("bench-frame")?;
    let scratch = scratch_dir.path();
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    let mut lines: Option<String> = None;
    let mut costs: Vec<celeste_rust::storage::bench::Rep> = Vec::new();
    let counters = celeste_rust::storage::bench::Counters::open().map(celeste_rust::storage::bench::count_steps);
    for rep in 0..reps {
        let _ = std::fs::remove_dir_all(scratch);
        let load = celeste_rust::storage::bench::load_average();
        let at = counters.as_ref().map(|c| c.start());
        let t = std::time::Instant::now();
        // A rep runs on a copy of the state; the last on the state itself
        // (no copy beside it).
        let mut copy;
        let s = if rep + 1 < reps {
            copy = st.clone();
            &mut copy
        } else {
            &mut st
        };
        let (ckpt, edges_dir) = (scratch.join("ckpt"), scratch.join("edges"));
        let stats = s.step(&engine, edges.then_some(edges_dir.as_path()), checkpoint.then_some(ckpt.as_path()), None)?;
        let t_all = t.elapsed();
        let counts = counters.as_ref().zip(at).map(|(c, at)| c.stop(at));
        eprintln!("[bench] rep {rep}: {} | rep {:.0} ms", stats.line(), ms(t_all));
        costs.push(celeste_rust::storage::bench::Rep { stats, workers: cfg.threads(), load, counts });
        let out = s.lines(edges.then_some(scratch), checkpoint.then_some(scratch), cfg.threads())?;
        match &lines {
            None => print!("{out}"),
            Some(first) => anyhow::ensure!(*first == out, "rep {rep} differs from rep 0:\n{out}against\n{first}"),
        }
        if rep == 0 {
            celeste_rust::compiled::print_kernel_stats(&costs[0].stats)?;
        }
        lines = Some(out);
    }
    celeste_rust::storage::bench::print_cost(frame + 1, &costs);
    if let (Some(path), Some(out)) = (&a.expect, &lines) {
        pinned(path, out, a.pin, line_moves)?;
    }
    Ok(())
}

/// A PIN: `got` against the file at `path` (`moves`: every difference, as
/// lines; none passes), or written there (`pin`).
pub(crate) fn pinned(path: &std::path::Path, got: &str, pin: bool, moves: fn(&str, &str) -> Vec<String>) -> Result<()> {
    if pin {
        std::fs::write(path, got).with_context(|| path.display().to_string())?;
        eprintln!("[bench] re-pinned {}", path.display());
        return Ok(());
    }
    let want = std::fs::read_to_string(path).with_context(|| format!("{}: the pin (--pin writes it)", path.display()))?;
    let moved = moves(&want, got);
    anyhow::ensure!(moved.is_empty(), "{}: not as pinned (re-pin with --pin only on evidence):\n{}", path.display(), moved.join("\n"));
    Ok(())
}

/// The printed lines against the pinned ones of their kinds (a line's
/// first letter: `f`, `h`, `e`, `b`).
pub(crate) fn line_moves(want: &str, got: &str) -> Vec<String> {
    let kinds: Vec<char> = got.lines().filter_map(|l| l.chars().next()).collect();
    let want: Vec<&str> = want.lines().filter(|l| l.chars().next().is_some_and(|c| kinds.contains(&c))).collect();
    let got: Vec<&str> = got.lines().collect();
    if want == got {
        return Vec::new();
    }
    want.iter().map(|l| format!("  pinned {l}")).chain(got.iter().map(|l| format!("  now    {l}"))).collect()
}

/// A run's own scratch dir, `/var/tmp/celeste-NAME-PID` (on disk: a frame's
/// files can be GBs), removed when dropped. Never inside the tree: a check
/// only reads its tree (the shared fixtures are read-only).
struct Scratch(PathBuf);

impl Scratch {
    fn new(name: &str) -> Result<Self> {
        let p = PathBuf::from(format!("/var/tmp/celeste-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&p);
        std::fs::create_dir_all(&p).with_context(|| p.display().to_string())?;
        Ok(Scratch(p))
    }

    fn path(&self) -> &std::path::Path {
        &self.0
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// `bench backward` of `frame` in the arithmetic of `N`, the winning sets
/// for `horizon`.
fn bench_backward<N: Rem>(tree: &TreeDir, horizon: u32, frame: u32, reps: usize) -> Result<()>
where
    N::C: celeste_rust::check::gate::GateWord,
{
    let cfg = &tree.run.settings;
    anyhow::ensure!(frame < horizon, "--step {frame}: below the horizon {horizon}");
    let t = std::time::Instant::now();
    let seeds = celeste_rust::search::backward::Seeds::<N>::load(&tree.dir, horizon)?;
    let content = seeds.space.content_hash();
    eprintln!("[bench backward] h{horizon}'s edge files opened in {:.2} s: {} win rows, {} edges; {} threads", t.elapsed().as_secs_f64(), seeds.wins.len(), seeds.store.edges, cfg.threads());
    let mut first: Option<String> = None;
    for rep in 0..reps {
        // Each rep pushes W down to F+1 afresh, then times the step to W_F.
        let t = std::time::Instant::now();
        let mut c = celeste_rust::search::backward::Backward::new(&seeds.store, &seeds.pairs, &seeds.wins, horizon, cfg.threads());
        for f in (frame + 1..horizon).rev() {
            c.step(f)?;
        }
        if rep == 0 {
            eprintln!("[bench backward] W from f{horizon} down to f{} in {:.2} s", frame + 1, t.elapsed().as_secs_f64());
        }
        let before = c.walls();
        let t = std::time::Instant::now();
        c.step(frame)?;
        let wall = t.elapsed();
        let after = c.walls();
        // The gate's W line: per node, its (shape, key, cell) and set, summed.
        let (mut n, mut acc) = (0usize, 0u64);
        for (id, r) in c.current() {
            let (shape, key, cell) = seeds.state(id);
            n += 1;
            acc = acc.wrapping_add(celeste_rust::check::gate::w_hash(content.state(shape, key, cell), r));
        }
        let ms = |k: usize| (after[k] - before[k]).as_secs_f64() * 1e3;
        eprintln!("[bench backward] rep {rep}: W f{frame} in {:.1} ms (read {:.1}, pull {:.1}, merge {:.1} ms)", wall.as_secs_f64() * 1e3, ms(0), ms(1), ms(2));
        let line = format!("[gate] h{horizon} W f{frame:03} {n} {acc:016x}");
        match &first {
            None => println!("{line}"),
            Some(l) => anyhow::ensure!(*l == line, "rep {rep} differs from rep 0: {line} against {l}"),
        }
        first = Some(line);
    }
    Ok(())
}
