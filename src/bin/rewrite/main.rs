//! `rewrite`: the search (`search`, `dual`), its forward alone
//! (`forward`), one concrete run (`run`), and the commands that read
//! finished trees - their fingerprints (`ckhash`), the checks (`check
//! ...`), the benchmarks (`bench ...`), the diagnostics and the UI's data.
//! A command that reads a tree takes the tree's settings from its records
//! (`search::tree::TreeDir`) and refuses them on its command line.

use anyhow::Result;
use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::P8;
use celeste_rust::abstraction::Level;
use celeste_rust::frame::{Block, FrameStep};
use celeste_rust::search::inspect::parse_xy;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use celeste_rust::engines::reference::{FloatRefEngine, RefEngine};
use clap::{CommandFactory, FromArgMatches, Parser, Subcommand};

mod bench;
mod check;
mod diag;
mod l1;
mod run;

/// mimalloc, not glibc: glibc retains gigabytes of freed slot chunks across
/// its arenas (plans/lessons.md). Counted (`metrics::Counting`): the `[mem]`
/// lines' live heap.
#[global_allocator]
static GLOBAL: celeste_rust::metrics::Counting<mimalloc::MiMalloc> = celeste_rust::metrics::Counting(mimalloc::MiMalloc);

/// On DISK, not the tmpfs at `/tmp`: a full-room tree is gigabytes.
const DEFAULT_CHECKPOINT_DIR: &str = "/var/tmp/celeste-checkpoints";

/// `rewrite --help`'s overview: the pipeline, the settings, the checks.
const LONG_ABOUT: &str = "The abstract TAS search for PICO-8 Celeste, and its diagnostics.

`search` runs the whole pipeline per level of `--level`: the level-0 forward
(abstract states, every edge with its remainder transfer), the backward over
the rotation graph (an exact-in-the-remainder lower bound), then the
exhaustive concrete search inside its winning sets. Its first concrete win is
the optimum; the witness goes to <checkpoint-dir>/witness_frame_F.txt.

THE SETTINGS are the flags below (Options), given anywhere on the line. A
tree records the ones that change it (<level dir>/settings.txt) and refuses a
run under others; flags never default to the record. A command that reads a
finished tree (ckhash, census, spurious, export-ui, check, bench) takes them
from the tree's record instead, and refuses them on its command line.
--override-settings proceeds past a different --region only. CELESTE_*
environment variables are refused. Run everything under ./safe-run.sh (a
memory cap).

Checking a change: ./check.sh t0|t1|t2|big (plans/devloop.md).";

/// `rewrite --help`'s examples.
const EXAMPLES: &str = "Examples:
  # A room's optimum, a known solution's length as the horizon:
  ./safe-run.sh -- target/release/rewrite search --room 1,0 --ceiling 99 --checkpoint-dir DIR
  # A room with objects, level -1 filtered at the horizon, a category, the real-play entry
  # (plans/results.md \"How to run a room\"); --prefer FILE checks a known route too:
  ./safe-run.sh -- target/release/rewrite search --room X,Y --level r0sxhn --ceiling C \\
      --level-minus-one C,5 [--hundred | --nodiag | --gemskip] [--loading-jank J] --checkpoint-dir DIR
  # The objects ladder where r0sxhn's bound is loose (room (7,0): 80 against 84):
  ./safe-run.sh -- target/release/rewrite search --room 7,0 --level r0sxhn,r0sxh \\
      --level-minus-one 84,5 --ceiling 84 --checkpoint-dir DIR
  # One forward and its fingerprint (the room (1,0) gate):
  ./safe-run.sh -- target/quick/rewrite forward --room 1,0 --to 44 --checkpoint-dir D
  target/quick/rewrite ckhash --tree D --to 44 --edges
  # A known route against a finished search's pruning:
  ./safe-run.sh -- target/quick/rewrite check known --checkpoint-dir DIR --inputs tas/FILE.txt
  # Help on one command:
  target/quick/rewrite search --help";

#[derive(Parser)]
#[command(about = "The abstract TAS search", long_about = LONG_ABOUT, after_long_help = EXAMPLES)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    /// The run's settings (`settings::Settings`, its flags): given anywhere on the line.
    #[command(flatten)]
    settings: Settings,
}

#[derive(Subcommand)]
enum Command {
    /// THE SEARCH (the arc pipeline), per level of `--level` (coarsest
    /// first): the forward to the horizon recording every edge with its
    /// remainder transfer, filtered by the previous level's arc-marked
    /// nodes; the rotation graph's winning sets backward from the wins
    /// (`arc_dp::solve`), whose optimum is a LOWER BOUND on the game's (no
    /// win refutes the horizon); then, at the LAST level only, the concrete
    /// search inside the winning sets (a try at the bound, then breadth-first).
    /// The first concrete win is the optimum, its inputs written to
    /// `<checkpoint-dir>/witness_frame_F.txt`. The outcome, per level and
    /// the verdict, goes to `<checkpoint-dir>/result.json`
    /// (`search::result`) before the exit code says it.
    Search(SearchArgs),
    /// THE DUAL SEARCH (`search::dual`): the shortest file valid in BOTH
    /// arithmetics under rule (b) - the float tool's exit is the count, and
    /// real play (16.16, from `--loading-jank`) must have left by then. Each
    /// half runs its levels (forward and arc phase) to the same horizon,
    /// under `<checkpoint-dir>/fixed` and `/float`; then the concrete search
    /// over pairs of states, each half inside its own winning sets, from the
    /// larger arc bound. The global settings are the fixed half's (no
    /// `--arith float`); `--float-half` picks the float tool, which starts
    /// from its own IL load (Celia's jank unless `--float-loading-jank`).
    /// The witness is replayed in both arithmetics and goes to
    /// `<checkpoint-dir>/witness_frame_F.txt`, the outcome to `result.json`.
    Dual(DualArgs),
    /// ONE forward pass at ONE level, exactly as the search runs it, with the
    /// per-frame timing line. The profiling entry point for the forward.
    Forward(ForwardArgs),
    /// ONE CONCRETE RUN: the reference engine (the interpreter on the
    /// traced cart) runs an input sequence from the room's entry and prints
    /// the player each frame (whole frames under `--split-frame`): the
    /// basis for checking a witness against a real PICO-8
    /// (`pico8_diff/replay.py`, the same frame convention). A frame that
    /// forks (`rnd`) is an error: give the balloons' seeds.
    Run(run::Args),
    /// FINGERPRINT a tree: per frame the states first reached there, counted,
    /// and an order-independent hash of their (shape, key, cell) by content,
    /// independent of the storage format (`f{frame} count hash`).
    Ckhash(diag::CkhashArgs),
    /// THE CHECKS of a finished tree, each against something independent.
    #[command(subcommand)]
    Check(check::Command),
    /// THE BENCHMARKS of a finished tree: one forward or backward frame.
    #[command(subcommand)]
    Bench(bench::Command),
    /// DIAGNOSTIC: a tree's states, COUNTED (cells named by `inspect::cell_names`). Per
    /// shape at `--frame`: its rows, every varying column's distinct values
    /// (capped at `--cap`) with the rows holding an interval or an unknown
    /// bool, and its distinct (spd.x, spd.y) pairs; with `--cell x,y` that
    /// player position's rows only, and the values of a column with few. With
    /// `--erase` or `--cell`, whether they SATURATE: from frame 0, every
    /// `--every` frames, the new states, and the distinct ones (new, and so
    /// far) with the named cells under an `--erase` prefix left out. With
    /// `--cells K`, the K player positions with the most states through
    /// `--frame` (their new states in its last 12 frames), and every
    /// position's new states at `--frame` as a share of its states.
    Census(diag::CensusArgs),
    /// DIAGNOSTIC: WHERE A TREE'S STATES COME FROM. `--samples` states of `--tree` first
    /// reached at `--step` in `--cell` are walked back through the recorded
    /// edges, `--depth` steps, printing each predecessor's cell and what
    /// changed (named value cells but the `--erase` prefixes).
    ///
    /// With `--real` (a finer tree): a state whose projection the real tree
    /// never reached in its cell by `--step` is SPURIOUS; field distributions
    /// are compared, and each walk stops at the first predecessor the real tree
    /// did reach - the first spurious transition.
    Spurious(diag::SpuriousArgs),
    /// A concrete input sequence that follows a given TRAJECTORY of player
    /// positions (one "x,y" per line from frame 1; `-` accepts any), found
    /// breadth-first over the reference engine's concrete step, deduplicated
    /// exactly. Prints the input bytes for `rewrite run -i` /
    /// `pico8_diff/replay.py`, or the first frame it could not follow.
    Trajectory(diag::TrajectoryArgs),
    /// A float witness's ROBUSTNESS to the host libm (plans/float.md, "The
    /// robustness report"; `search::robust`): `--inputs` replayed in float (the
    /// run's float tool, its seeds) under glibc's `sin`, a correctly rounded
    /// `sin` (MPFR), every `sin` call one ulp up, every call one ulp down, and
    /// `--random` runs moving each call by -1, 0 or +1 ulp at random; the frame
    /// each first wins on. A variant that differs from glibc's is reported
    /// FRAGILE (flagged, not rejected); the exit code is 0 either way.
    Robust(diag::RobustArgs),
    /// The level -1 table (`--room`) against a known solution: its concrete
    /// states (reference engine) at every frame f before the exit, each with
    /// the table's d. One that is too late for `--horizon` (f + d > H, H the
    /// solution's exit frame) is a SOUNDNESS violation of the table: exits
    /// non-zero. An `rnd` fork checks every leaf.
    L1Check(l1::Args),
    /// Export a finished search for the web UI (`ui/`) from its record
    /// (`result.json`), the trees' checkpoint HEADERS and forward records
    /// (`forward.tsv`) and the marks files (`search::ui_export`); its room
    /// the search's record's.
    ExportUi(ExportUiArgs),
    /// The room (`--room`) as the tools need it, as JSON on stdout: its
    /// level and its objects in creation order (the original cart's: tile,
    /// tile position, type, whether the minimal cart has it, whether a TAS
    /// file's `[seeds]` list runs over it - in this order).
    Room,
    /// The start states real play can enter the room (`--room`) with
    /// (`starts`), as JSON on stdout: every loading-frame J a play of the
    /// previous room can give, the J's grouped by the exact start state they
    /// make (one search per class), and the J of Celia's IL load. Give the
    /// search's settings (the category; `--balloon-seeds` for a room with
    /// balloons); not `--loading-jank`.
    Starts,
}

impl Command {
    /// A command that reads finished trees: their records give the settings.
    fn reads_trees(&self) -> bool {
        matches!(self, Command::Ckhash(_) | Command::Check(_) | Command::Bench(_) | Command::Census(_) | Command::Spurious(_) | Command::ExportUi(_))
    }
}

#[derive(clap::Args)]
pub struct SearchArgs {
    /// The horizon: every frame up to it is searched.
    #[arg(long)]
    to: Option<u32>,
    /// A KNOWN solution's frame (a replayed TAS) as the horizon: a
    /// refutation, or no concrete witness by it, is an error.
    #[arg(long)]
    ceiling: Option<u32>,
    /// The levels (`Level::parse`), comma-separated, coarsest first:
    /// `r0sx` for a room without objects, `r0sxhn` with objects,
    /// `r0sxhn,r0sxh` for an exact-objects level filtered by the abstract one.
    #[arg(long, default_value = "r0sx")]
    level: String,
    /// The checkpoint dir: level i's tree goes to `level{i:02}/` under it.
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    checkpoint_dir: String,
    /// Stop at the arc bound (no concrete search).
    #[arg(long)]
    no_witness: bool,
    /// Write the web UI's arc pass into this directory (`export-ui
    /// --arc DIR`): the winning sets' support, each node to its last
    /// frame (`level0.marks.bin`), and the ARC-MARKED nodes (`arc.marks.bin`),
    /// both as `(shape, cell, key, dist)` rows with `dist` the horizon
    /// minus the last frame the node still wins from;
    /// `witness.txt` (inputs and player position per frame).
    #[arg(long)]
    save_marks: Option<String>,
    /// A known solution (a `tas/` file or a comma list of input bytes,
    /// the spawn prologue included): the concrete search tries its input
    /// first at every frame, so among the optimal routes the witness
    /// follows it wherever it can - the smallest change to an existing
    /// TAS. Every input is still tried; only WHICH optimum is found.
    /// When it exits by the horizon it is also CHECKED against every
    /// pruning step at every level (`search::known`, as `check known`):
    /// the search fails if one drops it.
    #[arg(long)]
    prefer: Option<String>,
}

#[derive(clap::Args)]
pub struct DualArgs {
    #[arg(long)]
    to: Option<u32>,
    /// A KNOWN dual run's frame: a refutation, or no dual witness by it, is an error.
    #[arg(long)]
    ceiling: Option<u32>,
    #[arg(long, default_value = "r0sx")]
    level: String,
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    checkpoint_dir: String,
    /// Stop at the dual bound (no concrete search).
    #[arg(long)]
    no_witness: bool,
    /// A known run, tried first and checked against each half's pruning (as `search --prefer`).
    #[arg(long)]
    prefer: Option<String>,
    /// The float half's tool: `celia` (default) or `uct`.
    #[arg(long, default_value = "celia")]
    float_half: String,
    /// The float half's loading jank J (Celia only; default: its IL load's).
    #[arg(long)]
    float_loading_jank: Option<usize>,
    /// The web UI's arc pass of the FLOAT half (the count) into this
    /// directory, as `search --save-marks` writes it, with the dual
    /// witness's path (`export-ui --arc DIR`).
    #[arg(long)]
    save_marks: Option<String>,
}

#[derive(clap::Args)]
pub struct ForwardArgs {
    /// Last frame to compute.
    #[arg(long)]
    to: u32,
    /// The level (`Level::parse`, e.g. `r0sxhn`).
    #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
    level: Level,
    /// Checkpoint dir (frames land under it).
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    checkpoint_dir: String,
    /// Run the REFERENCE engine instead of the kernels: `ckhash` of the
    /// two trees must agree. Slow. Not at held-unknown levels.
    #[arg(long)]
    reference: bool,
}

#[derive(clap::Args)]
pub struct ExportUiArgs {
    /// A `rewrite search` checkpoint dir (`result.json`, `level00/`, ...).
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    checkpoint_dir: String,
    /// Output directory (the UI serves it as `data/`).
    #[arg(long, default_value = "/var/tmp/celeste-ui/data")]
    out: String,
    /// The search's `--save-marks` directory: the winning sets' support
    /// and the arc-marked nodes over the last level's tree (and the witness).
    #[arg(long)]
    arc: Option<String>,
    /// A concrete run to draw instead of the arc directory's witness:
    /// `[label TEXT]`, `inputs a,b,..`, `[dashes F:DIR ..]`, then `f x y`
    /// per frame from 0 (`tools/align_tas.py ... OUTDIR` writes one).
    #[arg(long)]
    witness: Option<String>,
    /// Another concrete run to draw next to it (same format): e.g. the
    /// community TAS replayed in the original cart.
    #[arg(long)]
    reference: Option<String>,
    /// The file the UI's "Download" button offers, embedded byte for
    /// byte (the tasdatabase upload the category runner verified).
    #[arg(long)]
    download: Option<String>,
}

/// THE ROW SELECTOR of the commands that read a frame's states: the frame
/// (its states, first reached there) and optionally one player position.
#[derive(clap::Args)]
pub(crate) struct Rows {
    /// The frame (search step) whose states, first reached there, are read
    /// (default: the tree's last).
    #[arg(long)]
    frame: Option<u32>,
    /// Only the states at player position "x,y".
    #[arg(long, value_parser = parse_xy)]
    cell: Option<(i32, i32)>,
}

impl Rows {
    /// The frame, and the player cell (`None`: every cell).
    pub(crate) fn of(&self, t: &TreeDir) -> Result<(u32, Option<u32>)> {
        Ok((t.frame(self.frame)?, self.cell.map(cell_at).transpose()?))
    }

    /// The tree to the frame, the frame, and its states (of the cell) in the
    /// tree's order (by region, key, cell).
    pub(crate) fn states<N: celeste_core::arith::EngineNum>(&self, t: &TreeDir) -> Result<(Tree<N>, u32, Vec<Sk>)> {
        let (frame, cell) = self.of(t)?;
        let tree = Tree::<N>::open(&t.dir, frame)?;
        let geo = celeste_rust::storage::GEOMETRY;
        let all: Vec<Sk> = tree.layer(frame, None, t.settings.threads())?.0.states_iter().filter(|s| cell.is_none_or(|c| s.cell(&geo) == c)).collect();
        anyhow::ensure!(!all.is_empty(), "{}: no states at step {frame}{}", t.dir.display(), self.cell.map_or(String::new(), |(x, y)| format!(" at ({x}, {y})")));
        Ok((tree, frame, all))
    }
}

/// A comma-separated list of name prefixes (`--erase`).
pub(crate) fn prefixes(list: &str) -> Vec<String> {
    list.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect()
}

/// A player position's cell (`--cell x,y`).
pub(crate) fn cell_at((x, y): (i32, i32)) -> Result<u32> {
    celeste_rust::search::pos_graph::cell_of(x, y)
}

/// A cell, for reading: the player's position.
pub(crate) fn where_(c: u32) -> String {
    celeste_rust::search::pos_graph::cell_xy(c).map(|(x, y)| format!("({x}, {y})")).unwrap_or_else(|| "no player".to_string())
}

/// The kernels of a level (`compiled::Kernels::new`), for the commands
/// generic over the arithmetic.
pub(crate) trait KernelsNew: Sized {
    fn new(cfg: &celeste_rust::settings::Settings, level: Level) -> Result<Self>;
}
impl<N: celeste_rust::compiled::KernelLanes> KernelsNew for celeste_rust::compiled::Kernels<N> {
    fn new(cfg: &celeste_rust::settings::Settings, level: Level) -> Result<Self> {
        celeste_rust::compiled::Kernels::<N>::new(cfg, level)
    }
}
/// A search's horizon in frames (`--to H` or `--ceiling H`).
fn horizon_of(to: Option<u32>, ceiling: Option<u32>) -> Result<u32> {
    match (ceiling, to) {
        (Some(h), None) | (None, Some(h)) => Ok(h),
        _ => anyhow::bail!("give the horizon: --to H, or --ceiling H for a known solution"),
    }
}
/// `search` and `dual` (`float`: the float half's settings): `search::run`,
/// its verdict reported.
#[allow(clippy::too_many_arguments)]
fn search(cfg: &Settings, float: Option<&Settings>, horizon: u32, ceiling: bool, level: &str, dir: &str, bound_only: bool, save_marks: Option<&str>, prefer: Option<&str>) -> Result<()> {
    let prefer = prefer.map(celeste_rust::concrete::read_inputs).transpose()?;
    let dir = std::path::Path::new(dir);
    let spec = celeste_rust::search::SearchSpec { settings: cfg, level, horizon, ceiling, dir, bound_only, save_marks: save_marks.map(std::path::Path::new), prefer: prefer.as_deref(), dual: float };
    report(&celeste_rust::search::run(&spec)?, dir)
}
/// A search's verdict (`search::run`'s record), printed; an error where it
/// fails the run: a level -1 table that cannot be built, a `--ceiling`
/// refuted or without a witness.
fn report(res: &celeste_rust::search::result::SearchResult, dir: &std::path::Path) -> Result<()> {
    use celeste_rust::search::result::Verdict;
    let (h, wall) = (res.horizon, res.wall_s);
    // The level that decided, and the bound: the last level's (a dual
    // search's halves' larger).
    let at = res.levels.last().map_or(String::new(), |l| format!("{}, {}", l.dir, l.spec));
    let mut last_of: std::collections::BTreeMap<&str, Option<u32>> = Default::default();
    for l in &res.levels {
        last_of.insert(l.dir.rsplit_once('/').map_or("", |x| x.0), l.arc_bound);
    }
    let bound = last_of.values().flatten().max();
    let what = if res.dual.is_some() { "dual " } else { "" };
    match res.verdict {
        Verdict::LevelMinusOneRefused => anyhow::bail!("--level-minus-one: building the level -1 table: {}", res.error.as_deref().unwrap_or("")),
        Verdict::Refuted => {
            anyhow::ensure!(!res.ceiling, "ceiling {h} REFUTED by the arc search at {at}: a known solution the model cannot reproduce");
            println!("REFUTED: no {what}win by f{h} ({at}; {wall:.1} s)");
        }
        Verdict::NoConcreteWin => {
            anyhow::ensure!(!res.ceiling, "ceiling {h}: no concrete {what}witness by it - a known solution the model cannot reproduce");
            println!("no concrete {what}win by f{h} ({what}bound f{}; {wall:.1} s)", bound.expect("a bound"));
        }
        Verdict::BoundOnly => {}
        Verdict::Optimal => {
            let f = res.optimal.expect("an optimum");
            let file = dir.join(res.witness_file.as_deref().expect("a witness file"));
            let exit = res.dual.as_ref().and_then(|d| d.fixed_exit).map_or(String::new(), |x| format!(", real play leaving on frame {x}"));
            println!("OPTIMAL {what}win frame: {f}{exit} ({wall:.1} s); witness {}", file.display());
        }
    }
    Ok(())
}
/// `rewrite forward` in arithmetic `N`: the tree at `dir` grown (or
/// resumed) to frame `to` by `engine` from `initial`; its first win, wall
/// time and position graph printed.
fn forward<N: celeste_rust::search::rem::Rem>(
    dir: &std::path::Path,
    to: u32,
    level: Level,
    cfg: &celeste_rust::settings::Settings,
    engine: Box<dyn FrameStep<N>>,
    initial: impl FnOnce() -> Result<Vec<Block<N>>>,
) -> Result<()> {
    let t = std::time::Instant::now();
    // Resumed or started (`grow_tree`).
    let fwd = celeste_rust::frame::grow_tree(dir, to, cfg.minus_one(), None, level, || Ok(engine), initial, cfg)?;
    let wall = t.elapsed().as_secs_f64();
    match fwd.win_frame {
        Some(h) => println!("win at f{h} ({wall:.2} s)"),
        None => println!("no win by f{} ({wall:.2} s)", fwd.frames),
    }
    if let Some(pg) = &fwd.pos_graph {
        let (pairs, fp) = pg.fingerprint();
        println!("posgraph f{:03} {pairs} {fp:016x}", fwd.frames);
    }
    celeste_rust::metrics::dump("forward", Some(dir), &[("level", level.to_string())]);
    Ok(())
}

/// The flags a command that reads trees refuses: the settings their records
/// give, given on its command line (in any subcommand's matches).
fn tree_flags_given(m: &clap::ArgMatches) -> Vec<String> {
    let given = |id: &str| {
        let mut m = m;
        loop {
            if m.value_source(id) == Some(clap::parser::ValueSource::CommandLine) {
                return true;
            }
            match m.subcommand() {
                Some((_, sub)) => m = sub,
                None => return false,
            }
        }
    };
    celeste_rust::settings::tree_keys().into_iter().filter(|k| given(k)).map(|k| format!("--{}", k.replace('_', "-"))).collect()
}

fn main() -> Result<()> {
    let matches = Cli::command().get_matches();
    let cli = Cli::from_arg_matches(&matches).unwrap_or_else(|e| e.exit());
    if cli.command.reads_trees() {
        let given = tree_flags_given(&matches);
        anyhow::ensure!(given.is_empty(), "{}: this command reads a finished tree and takes its settings from the tree's records (settings.txt, level_minus_one.txt); give only the run's own (--threads, --l1-table, the diagnostics)", given.join(" "));
    }
    // The commands that run in float (a tree's arithmetic is its record's).
    let float = cli.command.reads_trees() || matches!(cli.command, Command::Search(_) | Command::Forward(_) | Command::Robust(_) | Command::L1Check(_));
    let cfg = celeste_rust::settings::startup(&cli.settings, float, |s| {
        // `bench frame` times the workers' phases; `--metrics` counts the
        // kernels' census and slices (read when the kernels are built).
        if let Command::Bench(bench::Command::Frame(a)) = &cli.command {
            s.diag.phases = true;
            s.diag.kernel_metrics = a.metrics.is_some();
        }
    })?;
    match cli.command {
        Command::Search(a) => search(&cfg, None, horizon_of(a.to, a.ceiling)?, a.ceiling.is_some(), &a.level, &a.checkpoint_dir, a.no_witness, a.save_marks.as_deref(), a.prefer.as_deref()),
        Command::Dual(a) => {
            let tool = match a.float_half.as_str() {
                "celia" => celeste_rust::settings::FloatTool::Celia,
                "uct" => celeste_rust::settings::FloatTool::Uct,
                t => anyhow::bail!("--float-half: `celia` or `uct`, not {t:?}"),
            };
            let float = cfg.float_half(tool, a.float_loading_jank)?;
            search(&cfg, Some(&float), horizon_of(a.to, a.ceiling)?, a.ceiling.is_some(), &a.level, &a.checkpoint_dir, a.no_witness, a.save_marks.as_deref(), a.prefer.as_deref())
        }
        Command::Forward(a) => {
            let t = std::time::Instant::now();
            let (dir, level, reference) = (std::path::Path::new(&a.checkpoint_dir), a.level, a.reference);
            let what = if reference { ", REFERENCE engine" } else { "" };
            match cfg.arith {
                Arith::Fixed => {
                    let engine: Box<dyn FrameStep> = if reference { Box::new(celeste_rust::engines::reference::RefFrameStep::<P8>::new(&cfg, level)?) } else { Box::new(celeste_rust::compiled::FrameEngine::new(&cfg, level)?) };
                    eprintln!("[fwd] engine up in {:.2} s ({level}{what})", t.elapsed().as_secs_f64());
                    forward(dir, a.to, level, &cfg, engine, || Ok(vec![Block::canonical(RefEngine::new(&cfg)?.initial()?)?]))
                }
                Arith::Float(_) => {
                    let engine: Box<dyn FrameStep<LuaF64>> = if reference { Box::new(celeste_rust::engines::reference::RefFrameStep::<LuaF64>::new(&cfg, level)?) } else { Box::new(celeste_rust::compiled::FloatKernels::new(&cfg, level)?) };
                    eprintln!("[fwd] engine up in {:.2} s ({level}{what}, {})", t.elapsed().as_secs_f64(), cfg.arith);
                    forward(dir, a.to, level, &cfg, engine, || Ok(vec![Block::canonical(FloatRefEngine::new(&cfg)?.initial()?)?]))
                }
            }
        }
        Command::Run(a) => run::run(&cfg, a),
        Command::Ckhash(a) => diag::ckhash(&cfg, a),
        Command::Check(c) => check::run(&cfg, c),
        Command::Bench(c) => bench::run(&cfg, c),
        Command::Census(a) => diag::census(&cfg, a),
        Command::Spurious(a) => diag::spurious(&cfg, a),
        Command::Trajectory(a) => diag::trajectory(&cfg, a),
        Command::Robust(a) => diag::robust(&cfg, a),
        Command::L1Check(a) => l1::run(&cfg, a),
        Command::ExportUi(a) => celeste_rust::search::ui_export::export(
            std::path::Path::new(&a.checkpoint_dir),
            std::path::Path::new(&a.out),
            cfg.threads(),
            a.arc.as_deref().map(std::path::Path::new),
            celeste_rust::search::ui_export::Paths {
                witness: a.witness.as_deref().map(std::path::Path::new),
                reference: a.reference.as_deref().map(std::path::Path::new),
                download: a.download.as_deref().map(std::path::Path::new),
            },
        ),
        Command::Room => {
            let cart = celeste_core::cart_data::CartData::load("cart")?;
            let objects = celeste_rust::trace::cart::room_objects(&cart, cfg.room);
            let level = celeste_rust::game_runner::level_index(cfg.room.0, cfg.room.1) + 1;
            println!("{}", serde_json::json!({ "room": cfg.room, "level": level, "objects": objects }));
            Ok(())
        }
        Command::Starts => {
            println!("{}", serde_json::to_string(&celeste_rust::starts::starts(&cfg)?)?);
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    /// The command line is well-formed: no flag defined twice (a
    /// subcommand's own against the shared settings), every one parseable
    /// (`clap`'s debug assertions, which a release build skips).
    #[test]
    fn the_command_line_is_well_formed() {
        use clap::CommandFactory;
        let cmd = super::Cli::command();
        assert_eq!(celeste_rust::settings::duplicate_flags(&cmd), Vec::<String>::new(), "flags defined twice");
        cmd.debug_assert();
    }
}

