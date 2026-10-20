//! THE BENCHMARKS of a finished tree (`rewrite bench ...`): one forward
//! frame (`frame`: its fingerprints, its files' bytes, its exact metrics
//! and its costs) and one backward frame (`backward`), each repeatable.

use anyhow::{Context, Result};
use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::P8;
use celeste_rust::abstraction::Level;
use celeste_rust::frame::FrameStep;
use celeste_rust::search::rem::Rem;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
use std::path::PathBuf;

use crate::KernelsNew;

#[derive(clap::Subcommand)]
pub enum Command {
    /// ONE FORWARD FRAME of a tree, F -> F+1, repeatably.
    ///
    /// The forward as it stood after `--step` F (`ForwardState::at`: the
    /// tree's visited set, transfers, level -1 filter and pos graph at F;
    /// later frames ignored) runs the wave of F+1 `--reps` times, the edges
    /// into a scratch dir. Prints (stdout, the same every rep, else an
    /// error) `ckhash`'s lines for F+1 - `f{F+1}` of the new states,
    /// `h{F+1}` of the frontier, with `--edges` `e{F+1}` of the edges, with
    /// `--checkpoint` `b{F+1}` over the bytes of the frame's files - so a
    /// tree extended past F checks it; per rep (stderr) the wave, the
    /// frame's end walls and the workers' phases; `kernel lanes: ... missed
    /// 0`. With `--metrics FILE`, the EXACT metrics of the first rep (kernel
    /// census and dynamic instructions, lanes, emissions, states, edges:
    /// `tools/metrics_diff.py` compares them with a pinned file); the same
    /// at every `--threads`.
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
    /// The tree (a forward's `--tree`, or a search's `levelNN`).
    #[arg(long)]
    tree: PathBuf,
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
    /// Write the first rep's exact metrics to this file (`compiled::mix`).
    #[arg(long)]
    pub metrics: Option<PathBuf>,
    /// Checkpoint each rep's frame as the forward does (the frame file
    /// and the hull file, each read back and checked; the pos graph),
    /// into the scratch dir, timed; prints the `b{F+1}` line (needs
    /// `--edges`).
    #[arg(long)]
    checkpoint: bool,
}

#[derive(clap::Args)]
pub struct BackwardArgs {
    /// A search's tree (its `levelNN`: `frames/`, `edges/` under it).
    #[arg(long)]
    tree: PathBuf,
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
            let t = TreeDir::open(&a.tree, run)?;
            match t.settings.arith {
                Arith::Fixed => bench_frame::<celeste_rust::compiled::FrameEngine, P8>(&t, &a),
                Arith::Float(_) => {
                    anyhow::ensure!(a.metrics.is_none(), "a float tree: bench frame --metrics (the kernel census) is 16.16's");
                    bench_frame::<celeste_rust::compiled::FloatKernels, LuaF64>(&t, &a)
                }
            }
        }
        Command::Backward(a) => {
            let t = TreeDir::open(&a.tree, run)?;
            let horizon = t.frame(a.steps)?;
            match t.settings.arith {
                Arith::Fixed => bench_backward::<P8>(&t, horizon, a.step, a.reps),
                Arith::Float(_) => bench_backward::<LuaF64>(&t, horizon, a.step, a.reps),
            }
        }
    }
}

/// `bench frame` in the arithmetic of `N`, its kernels `K` (`--metrics`,
/// the kernel census, is 16.16's).
fn bench_frame<K, N>(t: &TreeDir, a: &FrameArgs) -> Result<()>
where
    K: FrameStep<N> + KernelsNew,
    N: Rem,
{
    use celeste_rust::compiled::dispatch::{print_stats, take_kernel_stats};
    let (cfg, level, dir) = (&t.settings, t.level, &t.dir);
    let (edges, checkpoint, reps) = (a.edges, a.checkpoint, a.reps);
    anyhow::ensure!(!checkpoint || edges, "--checkpoint needs --edges (the frame's files as the forward writes them)");
    let frame = t.frame(a.step)?;
    let engine = K::new(cfg, level)?;
    let t = std::time::Instant::now();
    engine.warm();
    let t_kernels = t.elapsed().as_secs_f64();
    eprintln!("[bench] kernels built: rss {:.2} GB", celeste_rust::metrics::current_rss_gb());
    let t = std::time::Instant::now();
    let mut st = celeste_rust::frame::ForwardState::<N>::at(dir, frame, true, level, cfg)?;
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
    {
        let (fr, (outs, xfers)) = (st.frontier(), st.outs_and_xfers());
        celeste_rust::storage::bench::print_memory(&format!("f{frame} loaded"), st.visited(), &fr.new, &fr.hulls, &fr.old, outs, xfers);
    }
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
        // The last rep runs on the state's own visited set (no copy beside it).
        let (wave, visited, xfers) = st.bench_wave(&engine, dir, edges.then_some(scratch), rep + 1 < reps)?;
        let ckpt = if checkpoint { Some(st.bench_checkpoint(&wave, &scratch.join("ckpt"), cfg)?) } else { None };
        let t_all = t.elapsed();
        let counts = counters.as_ref().zip(at).map(|(c, at)| c.stop(at));
        let w = &wave.stats;
        eprintln!(
            "[bench] rep {rep}: wave {:.0} ms (idle {:.0}%, regions {:.0} worker-ms) end {:.0}{} total {:.0} ms | in {} old {} emitted {} re-pushed {} pushes {} kept {} grown {} | units {} slices {} routes {} | edges {} bundles {} ({:.3} B; out {:.3} B) | rss start {:.2} wave {:.2} end {:.2} peak {:.2} GB",
            ms(w.t_wave),
            w.wave_idle * 100.0,
            ms(w.t_regions),
            ms(w.t_layer),
            ckpt.map_or(String::new(), |(c, pos)| format!(" checkpoint {:.0}", ms(c.t.iter().sum::<std::time::Duration>() + pos))),
            ms(t_all),
            w.lanes_in,
            w.old_in,
            w.lanes_raw,
            w.repush_edges,
            w.pushes,
            w.lanes_kept,
            w.grown,
            w.units,
            w.slices,
            w.routes,
            w.edge_records,
            w.edge_bundles,
            w.edge_bytes as f64 / w.edge_records.max(1) as f64,
            w.out_bytes as f64 / w.edge_records.max(1) as f64,
            w.rss_start,
            w.rss_wave,
            w.rss_end,
            celeste_rust::metrics::peak_rss_gb(),
        );
        costs.push(celeste_rust::storage::bench::Rep { stats: *w, ckpt, workers: cfg.threads(), wall: t_all, load, counts });
        if rep + 1 == reps {
            celeste_rust::storage::bench::print_memory(&format!("f{} made (the out store as loaded)", frame + 1), &visited, &wave.next, &wave.hulls, &wave.grown, st.outs_and_xfers().0, &xfers);
        }
        let (n, acc) = wave.next.content_fingerprint(&visited.keys, visited.shapes())?;
        let mut out = format!("f{:03} {n} {acc:016x}\n", frame + 1);
        let (nh, ah) = celeste_rust::storage::hulls::fingerprint(wave.frontier_states(frame + 1), &visited.keys, visited.shapes())?;
        out += &format!("h{:03} {nh} {ah:016x}\n", frame + 1);
        if edges {
            let (n, acc) = celeste_rust::storage::bench::edge_fingerprint(scratch, frame + 1, &xfers, &visited, cfg.threads())?;
            out += &format!("e{:03} {n} {acc:016x}\n", frame + 1);
        }
        if checkpoint {
            let (files, bytes, acc) = celeste_rust::storage::bench::dir_fingerprint(scratch)?;
            out += &format!("b{:03} {files} {bytes} {acc:016x}\n", frame + 1);
        }
        match &lines {
            None => print!("{out}"),
            Some(first) => anyhow::ensure!(*first == out, "rep {rep} differs from rep 0:\n{out}against\n{first}"),
        }
        if rep == 0 {
            let ks = take_kernel_stats();
            print_stats(&ks);
            anyhow::ensure!(ks.missed == 0, "KERNEL COVERAGE GAP: {} lanes missed", ks.missed);
            if let Some(path) = &a.metrics {
                let kernels = celeste_rust::compiled::mix::kernel_metrics();
                let slices: u64 = kernels.iter().map(|k| k.1).sum();
                let insts: u64 = kernels.iter().map(|(_, n, c)| n * c.get("insts").copied().unwrap_or(0)).sum();
                eprintln!("[bench] kernels: {insts} dynamic instructions over {slices} slices ({} a slice; call-outs' callees aside)", insts.checked_div(slices).unwrap_or(0));
                let text = frame_metrics(cfg.room, level, frame, w, &ks);
                std::fs::write(path, text).with_context(|| path.display().to_string())?;
            }
            celeste_rust::compiled::mix::reset_slices();
        }
        lines = Some(out);
    }
    let ks = take_kernel_stats();
    anyhow::ensure!(ks.missed == 0, "KERNEL COVERAGE GAP: {} lanes missed", ks.missed);
    celeste_rust::storage::bench::print_cost(frame + 1, &costs);
    Ok(())
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
fn bench_backward<N: Rem>(tree: &TreeDir, horizon: u32, frame: u32, reps: usize) -> Result<()> {
    let cfg = &tree.settings;
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
            acc = acc.wrapping_add(celeste_rust::search::backward::gate_w_hash(content.state(shape, key, cell), r));
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

/// `bench frame --metrics`: one frame's EXACT metrics as `key value` lines
/// (no timings: the same at every thread count): the frame's, the calls',
/// every built kernel's census summed (`all.*`), the dynamic instructions
/// (`dyn.*`: slices x static count) and per kernel that ran (`k.<sym>.*`).
fn frame_metrics((rx, ry): (i16, i16), level: Level, frame: u32, w: &celeste_rust::frame::FrameStats, ks: &celeste_rust::compiled::dispatch::KernelStats) -> String {
    use std::collections::BTreeMap;
    use std::fmt::Write as _;
    let mut m: BTreeMap<String, u64> = BTreeMap::new();
    let mut put = |k: &str, v: u64| *m.entry(k.to_string()).or_default() += v;
    put("frame.lanes_in", w.lanes_in as u64);
    put("frame.emissions", w.lanes_raw as u64);
    put("frame.kept", w.lanes_kept as u64);
    put("frame.units", w.units as u64);
    put("frame.slices", w.slices as u64);
    put("frame.routes", w.routes);
    put("frame.edges", w.edge_records);
    put("frame.bundles", w.edge_bundles);
    // Exact: a segment is a function of its route's records (global
    // transfer ids, sorted bundles, zstd -1).
    put("frame.edge_bytes", w.edge_bytes);
    put("frame.edge_millibytes_per_edge", (w.edge_bytes * 1000).checked_div(w.edge_records).unwrap_or(0));
    put("calls.calls", ks.calls);
    put("calls.rows", ks.rows);
    put("calls.slice_lanes", ks.slice_lanes);
    put("calls.bodies", ks.bodies);
    put("calls.bodies_taken", ks.bodies_taken);
    put("calls.lane_emits", ks.lane_emits);
    put("calls.unique", ks.unique);
    put("calls.lanes_missed", ks.missed);
    let kernels = celeste_rust::compiled::mix::kernel_metrics();
    put("kernels.built", kernels.len() as u64);
    // Every kernel's census summed.
    for (sym, slices, census) in &kernels {
        for (k, v) in census {
            put(&format!("all.{k}"), *v);
        }
        if *slices == 0 {
            continue;
        }
        put("kernels.ran", 1);
        put("dyn.slices", *slices);
        let insts = census.get("insts").copied().unwrap_or(0);
        put("dyn.insts", slices * insts);
        for (k, v) in census.iter().filter(|(k, _)| k.starts_with("cat.")) {
            put(&format!("dyn.{k}"), slices * v);
        }
        for k in ["traced.nodes", "fused_nodes", "nodes", "insts", "spill_slots"] {
            put(&format!("k.{sym}.{k}"), census.get(k).copied().unwrap_or(0));
        }
        put(&format!("k.{sym}.slices"), *slices);
        put(&format!("k.{sym}.dyn_insts"), slices * insts);
    }
    let mut out = format!("# rewrite bench frame --metrics: room {rx},{ry}, {level}, f{frame} -> f{}\n", frame + 1);
    for (k, v) in &m {
        writeln!(out, "{k} {v}").unwrap();
    }
    out
}
