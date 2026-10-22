//! THE BENCHMARKS of a finished tree (`rewrite bench ...`): one forward
//! frame (`frame`: its fingerprints, its files' bytes, its exact metrics
//! and its costs) and one backward frame (`backward`), each repeatable.

use anyhow::{Context, Result};
use celeste_rust::abstraction::Level;
use celeste_rust::frame::FrameStep;
use celeste_rust::search::rem::Rem;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
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
    /// 0`. Its PINS: `--expect FILE` (the lines, those of the kinds this run
    /// prints) and `--metrics FILE` (the first rep's EXACT metrics: kernel
    /// census and dynamic instructions, lanes, emissions, states, edges; the
    /// same at every `--threads`), each compared and every difference
    /// shown, or written with `--pin`.
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
    /// PER-LINE CYCLES THROUGH INLINED FRAMES, from a `perf record` of the
    /// release binary (`perf report` lumps every inlined callee into its
    /// outer function; the binary has line tables, not frame pointers).
    ///
    /// Each sampled address's inline chain is resolved with `addr2line -i`;
    /// the samples are attributed by outermost function, or with `--within
    /// FUNC` by line inside the outermost frame whose function contains FUNC
    /// (`--at FILE:LINE`: only samples at that line of it, split `--depth`
    /// frames further in):
    ///
    ///   perf script -i perf.data -F tid,time,ip,sym,dso -G > samples.txt
    ///   perf script -i perf.data --show-mmap-events | grep MMAP | grep release/rewrite
    ///     # -> [0xBASE(...) @ 0xPGOFF ...]: the binary's text mapping
    ///   rewrite bench inline --samples samples.txt --binary BIN --base BASE --delta DELTA
    ///
    /// DELTA = PGOFF + (the R E segment's vaddr - its file offset), e.g.
    /// 0x11b000 + 0x1000 (`readelf -lW BIN | grep LOAD`).
    Inline(InlineArgs),
}

#[derive(clap::Args)]
pub struct InlineArgs {
    /// `perf script -F tid,time,ip,sym,dso -G` output.
    #[arg(long)]
    samples: PathBuf,
    /// The profiled binary.
    #[arg(long)]
    binary: PathBuf,
    /// Its text mapping's start (hex).
    #[arg(long, value_parser = hex)]
    base: u64,
    /// Its file offset plus the segment's vaddr less offset (hex).
    #[arg(long, value_parser = hex)]
    delta: u64,
    /// Only these threads' samples (default: all).
    #[arg(long, value_delimiter = ',')]
    tids: Vec<u64>,
    /// By line inside the outermost frame whose function contains this.
    #[arg(long)]
    within: Option<String>,
    /// With `--within`: only samples at this `file:line` of it.
    #[arg(long, requires = "within")]
    at: Option<String>,
    /// With `--within`: the frames further in that split a line.
    #[arg(long, default_value_t = 1)]
    depth: usize,
}

fn hex(s: &str) -> std::result::Result<u64, String> {
    u64::from_str_radix(s.trim_start_matches("0x"), 16).map_err(|e| format!("{s:?}: {e}"))
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
    /// The first rep's exact metrics (`compiled::mix`) must be this pinned
    /// file's (with `--pin`: written to it).
    #[arg(long)]
    pub metrics: Option<PathBuf>,
    /// The printed lines must be this pinned file's lines of their kinds
    /// (with `--pin`: written to it).
    #[arg(long)]
    expect: Option<PathBuf>,
    /// Write `--expect` and `--metrics` instead of comparing (a re-pin: only
    /// on evidence, said in the commit).
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
            if t.settings.arith != Arith::Fixed {
                anyhow::ensure!(a.metrics.is_none(), "a float tree: bench frame --metrics (the kernel census) is 16.16's");
            }
            celeste_rust::with_arith!(t.settings.arith, N => bench_frame::<N>(&t, &a))
        }
        Command::Backward(a) => {
            let t = a.tree.open(run)?;
            let horizon = t.frame(a.steps)?;
            celeste_rust::with_arith!(t.settings.arith, N => bench_backward::<N>(&t, horizon, a.step, a.reps))
        }
        Command::Inline(a) => inline(&a),
    }
}

/// `bench frame` in the arithmetic of `N` (`--metrics`, the kernel census,
/// is 16.16's).
fn bench_frame<N: Rem + celeste_rust::compiled::KernelLanes>(t: &TreeDir, a: &FrameArgs) -> Result<()> {
    let (cfg, level, dir) = (&t.settings, t.level, &t.dir);
    let (edges, checkpoint, reps) = (a.edges, a.checkpoint, a.reps);
    anyhow::ensure!(!checkpoint || edges, "--checkpoint needs --edges (the frame's files as the forward writes them)");
    anyhow::ensure!(!a.pin || (a.expect.is_some() || a.metrics.is_some()), "--pin writes --expect and --metrics: give one");
    anyhow::ensure!(!a.pin || a.expect.is_none() || checkpoint, "--pin --expect: a pin holds every line (--edges --checkpoint)");
    let frame = t.frame(a.step)?;
    let engine = celeste_rust::compiled::Kernels::<N>::new(cfg, level)?;
    let t = std::time::Instant::now();
    engine.warm();
    let t_kernels = t.elapsed().as_secs_f64();
    eprintln!("[bench] kernels built: rss {:.2} GB", celeste_rust::metrics::current_rss_gb());
    let t = std::time::Instant::now();
    let mut st = celeste_rust::frame::ForwardState::<N>::at(dir, frame, level, cfg)?;
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
        let (visited, front) = (s.visited(), s.frontier());
        let (n, acc) = front.new.content_fingerprint(&visited.keys, visited.shapes())?;
        let mut out = format!("f{:03} {n} {acc:016x}\n", frame + 1);
        let (nh, ah) = celeste_rust::storage::hulls::fingerprint(front.states(frame + 1), &visited.keys, visited.shapes())?;
        out += &format!("h{:03} {nh} {ah:016x}\n", frame + 1);
        if edges {
            let (n, acc) = celeste_rust::storage::bench::edge_fingerprint(scratch, frame + 1, s.xfers(), visited, cfg.threads())?;
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
            celeste_rust::compiled::print_kernel_stats(&costs[0].stats);
            if let Some(path) = &a.metrics {
                let kernels = celeste_rust::compiled::mix::kernel_metrics();
                let slices: u64 = kernels.iter().map(|k| k.1).sum();
                let insts: u64 = kernels.iter().map(|(_, n, c)| n * c.get("insts").copied().unwrap_or(0)).sum();
                eprintln!("[bench] kernels: {insts} dynamic instructions over {slices} slices ({} a slice; call-outs' callees aside)", insts.checked_div(slices).unwrap_or(0));
                pinned(path, &frame_metrics(cfg.room, level, frame, &costs[0].stats), a.pin, metrics_moves)?;
            }
            celeste_rust::compiled::mix::reset_slices();
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
fn pinned(path: &std::path::Path, got: &str, pin: bool, moves: fn(&str, &str) -> Vec<String>) -> Result<()> {
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
fn line_moves(want: &str, got: &str) -> Vec<String> {
    let kinds: Vec<char> = got.lines().filter_map(|l| l.chars().next()).collect();
    let want: Vec<&str> = want.lines().filter(|l| l.chars().next().is_some_and(|c| kinds.contains(&c))).collect();
    let got: Vec<&str> = got.lines().collect();
    if want == got {
        return Vec::new();
    }
    want.iter().map(|l| format!("  pinned {l}")).chain(got.iter().map(|l| format!("  now    {l}"))).collect()
}

/// The exact metrics against the pinned ones: every key that moved, with
/// its change, and per kernel (`k.<sym>.*`) the ones that came, went or
/// changed, the largest moves first.
fn metrics_moves(want: &str, got: &str) -> Vec<String> {
    use std::collections::{BTreeMap, BTreeSet};
    let load = |t: &str| -> (Vec<String>, BTreeMap<String, i64>) {
        let (mut head, mut vals) = (Vec::new(), BTreeMap::new());
        for l in t.lines() {
            match l.rsplit_once(' ').and_then(|(k, v)| Some((k, v.parse::<i64>().ok()?))) {
                Some((k, v)) if !l.starts_with('#') => {
                    vals.insert(k.to_string(), v);
                }
                _ => head.push(l.to_string()),
            }
        }
        (head, vals)
    };
    let ((ha, a), (hb, b)) = (load(want), load(got));
    let pct = |x: i64, y: i64| if x == 0 { if y == 0 { "0%".to_string() } else { "new".to_string() } } else { format!("{:+.2}%", 100.0 * (y - x) as f64 / x as f64) };
    let show = |v: Option<&i64>| v.map_or("-".to_string(), i64::to_string);
    let mut out = Vec::new();
    if ha != hb {
        out.push(format!("  header: {ha:?} -> {hb:?}"));
    }
    let keys: BTreeSet<&String> = a.keys().chain(b.keys()).collect();
    for k in keys.iter().filter(|k| !k.starts_with("k.")) {
        let (x, y) = (a.get(*k), b.get(*k));
        if x != y {
            let change = match (x, y) {
                (Some(x), Some(y)) => pct(*x, *y),
                (None, _) => "added".to_string(),
                (_, None) => "removed".to_string(),
            };
            out.push(format!("  {k:<40} {:>14} -> {:>14}  {change}", show(x), show(y)));
        }
    }
    let syms = |d: &BTreeMap<String, i64>| -> BTreeSet<String> { d.keys().filter_map(|k| k.strip_prefix("k.")).filter_map(|k| k.split('.').next()).map(str::to_string).collect() };
    let (sa, sb) = (syms(&a), syms(&b));
    let mut moved: Vec<(i64, &String)> = keys.iter().filter(|k| k.strip_prefix("k.").and_then(|r| r.split('.').next()).is_some_and(|s| sa.contains(s) && sb.contains(s))).filter(|k| a.get(**k) != b.get(**k)).map(|k| ((b.get(*k).copied().unwrap_or(0) - a.get(*k).copied().unwrap_or(0)).abs(), *k)).collect();
    let (gone, came): (Vec<&String>, Vec<&String>) = (sa.difference(&sb).collect(), sb.difference(&sa).collect());
    if !gone.is_empty() || !came.is_empty() || !moved.is_empty() {
        let changed: BTreeSet<&str> = moved.iter().filter_map(|(_, k)| k.split('.').nth(1)).collect();
        out.push(format!("  kernels that ran: {} gone, {} new, {} changed", gone.len(), came.len(), changed.len()));
        moved.sort_by(|x, y| y.cmp(x));
        for (_, k) in moved.iter().take(12) {
            let (x, y) = (a.get(*k).copied().unwrap_or(0), b.get(*k).copied().unwrap_or(0));
            out.push(format!("    {k:<54} {x} -> {y}  {}", pct(x, y)));
        }
        out.extend(gone.iter().take(5).map(|s| format!("    gone: {s}")));
        out.extend(came.iter().take(5).map(|s| format!("    new:  {s}")));
    }
    out
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
fn frame_metrics((rx, ry): (i16, i16), level: Level, frame: u32, w: &celeste_rust::storage::stats::Stats) -> String {
    use celeste_rust::storage::stats::S;
    use std::collections::BTreeMap;
    use std::fmt::Write as _;
    let mut m: BTreeMap<String, u64> = BTreeMap::new();
    let mut put = |k: &str, v: u64| *m.entry(k.to_string()).or_default() += v;
    put("frame.lanes_in", w.get(S::LanesIn));
    put("frame.emissions", w.get(S::Emissions));
    put("frame.kept", w.get(S::Kept));
    put("frame.units", w.get(S::Units));
    put("frame.slices", w.get(S::Slices));
    put("frame.routes", w.get(S::Routes));
    put("frame.edges", w.get(S::Edges));
    put("frame.bundles", w.get(S::Bundles));
    // Exact: a segment is a function of its route's records (global
    // transfer ids, sorted bundles, zstd -1).
    put("frame.edge_bytes", w.get(S::EdgeBytes));
    put("frame.edge_millibytes_per_edge", (w.get(S::EdgeBytes) * 1000).checked_div(w.get(S::Edges)).unwrap_or(0));
    put("calls.calls", w.get(S::KernelCalls));
    put("calls.rows", w.get(S::KernelRows));
    put("calls.slice_lanes", 16 * w.get(S::KernelSlices));
    put("calls.bodies", w.get(S::KernelBodies));
    put("calls.bodies_taken", w.get(S::KernelBodiesTaken));
    put("calls.lane_emits", w.get(S::KernelTakes));
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

/// `bench inline`.
fn inline(a: &InlineArgs) -> Result<()> {
    use std::collections::HashMap;
    let bin = a.binary.to_string_lossy();
    let (mut ips, mut other, mut total): (HashMap<u64, usize>, HashMap<String, usize>, usize) = Default::default();
    for line in std::fs::read_to_string(&a.samples).with_context(|| a.samples.display().to_string())?.lines() {
        // Three whitespace-separated words, then the rest.
        let mut p: Vec<&str> = Vec::with_capacity(4);
        let mut rest = line.trim_start();
        while p.len() < 3 && !rest.is_empty() {
            let end = rest.find(char::is_whitespace).unwrap_or(rest.len());
            p.push(&rest[..end]);
            rest = rest[end..].trim_start();
        }
        if !rest.is_empty() {
            p.push(rest);
        }
        let Some(tid) = p.first().and_then(|t| t.parse::<u64>().ok()).filter(|_| p.len() == 4) else { continue };
        if !a.tids.is_empty() && !a.tids.contains(&tid) {
            continue;
        }
        total += 1;
        if p[3].contains(&format!("({bin})")) {
            *ips.entry(hex(p[2]).map_err(anyhow::Error::msg)? - a.base + a.delta).or_default() += 1;
        } else {
            let rest = p[3].trim();
            let (sym, dso) = rest.rsplit_once(" (").map_or(("?", "?"), |(s, d)| (s, d.trim_end_matches(')')));
            *other.entry(if dso.contains("asm-scratch") { "KERNEL .so".into() } else if dso.contains("kallsyms") { "[k]".into() } else { sym.to_string() }).or_default() += 1;
        }
    }
    let mut addrs: Vec<u64> = ips.keys().copied().collect();
    addrs.sort();
    let input: String = addrs.iter().map(|a| format!("{a:#x}\n")).collect();
    let mut child = std::process::Command::new("addr2line").args(["-i", "-f", "-C", "-a", "-e"]).arg(&a.binary).stdin(std::process::Stdio::piped()).stdout(std::process::Stdio::piped()).spawn().context("addr2line")?;
    let mut stdin = child.stdin.take().expect("piped");
    let writer = std::thread::spawn(move || std::io::Write::write_all(&mut stdin, input.as_bytes()));
    let out = child.wait_with_output()?;
    writer.join().expect("the writer")?;
    // Per address its inline chain, innermost first: (function, file:line).
    let mut chains: HashMap<u64, Vec<(String, String)>> = HashMap::new();
    let (mut cur, lines) = (None, String::from_utf8_lossy(&out.stdout).lines().map(str::to_string).collect::<Vec<_>>());
    let mut i = 0;
    while i < lines.len() {
        if let Some(h) = lines[i].strip_prefix("0x") {
            cur = Some(u64::from_str_radix(h, 16)?);
            i += 1;
        } else if let (Some(c), Some(loc)) = (cur, lines.get(i + 1)) {
            chains.entry(c).or_default().push((lines[i].clone(), loc.clone()));
            i += 2;
        } else {
            i += 1;
        }
    }
    println!("{total} samples, {} addresses", addrs.len());
    let short = |loc: &str| loc.rsplit('/').next().unwrap_or(loc).split(" (").next().unwrap_or(loc).to_string();
    let pct = |c: usize| 100.0 * c as f64 / total.max(1) as f64;
    let mut by: HashMap<String, usize> = HashMap::new();
    let mut within = 0;
    for (addr, &c) in &ips {
        let ch = chains.get(addr).map_or(&[][..], |v| &v[..]);
        match &a.within {
            None => *by.entry(ch.last().map_or("??".into(), |f| f.0.chars().take(120).collect())).or_default() += c,
            Some(func) => {
                let Some(j) = ch.iter().rposition(|(f, l)| f.contains(func.as_str()) && a.at.as_ref().is_none_or(|at| l.contains(at.as_str()))) else { continue };
                within += c;
                let inner: Vec<String> = (j.saturating_sub(a.depth)..j).rev().map(|k| format!("{}@{}", ch[k].0.rsplit("::").next().unwrap_or("").chars().take(30).collect::<String>(), short(&ch[k].1))).collect();
                *by.entry(format!("{}  {}", short(&ch[j].1), if inner.is_empty() { "(own)".to_string() } else { inner.join(" > ") })).or_default() += c;
            }
        }
    }
    if a.within.is_none() {
        for (k, c) in other {
            *by.entry(k).or_default() += c;
        }
    } else {
        println!("{}{}: {:.2}% of {total} samples", a.within.as_deref().unwrap_or(""), a.at.as_ref().map_or(String::new(), |l| format!(" at {l}")), pct(within));
    }
    let mut rows: Vec<(String, usize)> = by.into_iter().collect();
    rows.sort_by(|x, y| y.1.cmp(&x.1).then_with(|| x.0.cmp(&y.0)));
    for (k, c) in rows.into_iter().take(if a.within.is_some() { 40 } else { 30 }) {
        println!("{:6.2}%  {k}", pct(c));
    }
    Ok(())
}
