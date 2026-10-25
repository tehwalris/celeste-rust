//! `rewrite`: the search (`search`, `dual`), its forward alone
//! (`forward`), one concrete run (`run`), and the commands that read
//! finished trees - their fingerprints (`ckhash`), the checks (`check
//! ...`), the benchmarks (`bench ...`), the diagnostics and the UI's data.
//! A command that reads a tree takes the tree's settings from its records
//! (`search::tree::TreeDir`) and refuses them on its command line.

use anyhow::Result;
use celeste_engine::runtime2::Rt2;
use celeste_rust::abstraction::Level;
use celeste_rust::frame::FrameStep;
use celeste_rust::search::inspect::parse_xy;
use celeste_rust::search::tree::TreeDir;
use celeste_rust::settings::{Arith, Settings};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use clap::{CommandFactory, FromArgMatches, Parser, Subcommand};

mod bench;
mod check;
mod diag;
mod l1;
mod pico8;
mod run;
mod validate;

/// mimalloc, not glibc: glibc retains gigabytes of freed slot chunks across
/// its arenas (plans/lessons.md). Counted (`metrics::Counting`): the `[mem]`
/// lines' live heap.
#[global_allocator]
static GLOBAL: celeste_rust::metrics::Counting<mimalloc::MiMalloc> = celeste_rust::metrics::Counting(mimalloc::MiMalloc);

/// On DISK, not the tmpfs at `/tmp`: a full-room tree is gigabytes.
const DEFAULT_CHECKPOINT_DIR: &str = "/var/tmp/celeste-checkpoints";

const LONG_ABOUT: &str = "The abstract TAS search for PICO-8 Celeste, and its diagnostics.

`search` runs the whole pipeline per level of `--level`: the level-0 forward
(abstract states, every edge with its remainder transfer), the backward over
the rotation graph (an exact-in-the-remainder lower bound), then the
exhaustive concrete search inside its winning sets. Its first concrete win is
the optimum; the witness goes to <search-dir>/witness_frame_F.txt.

THE SETTINGS are the flags below (Options), given anywhere on the line. A
tree records the ones that change it (<level dir>/tree.txt) and refuses a
run under others; flags never default to the record. A command that reads a
finished tree (ckhash, census, export-ui, check, bench) takes them
from the tree's record instead, and refuses them on its command line.
CELESTE_* environment variables are refused. Run everything under ./safe-run.sh (a
memory cap).

UNITS are in the flag names: --frames counts game frames (the search's
horizon, its results, --level-minus-one's H), --steps and --step a tree's
search steps (two a frame under --split-frame: `settings::Clock`). A tree
is --tree DIR, a search's directory (its levels' trees under it)
--search-dir DIR.

Checking a change: ./check.sh t0 t1 t2 big (plans/devloop.md).";

const EXAMPLES: &str = "Examples:
  # A room's optimum, a known solution's length as the horizon:
  ./safe-run.sh -- target/release/rewrite search --room 1,0 --frames 99 --ceiling --search-dir DIR
  # A room with objects, level -1 filtered at the horizon, a category, the real-play entry
  # (plans/results.md \"How to run a room\"); --prefer FILE checks a known route too:
  ./safe-run.sh -- target/release/rewrite search --room X,Y --level r0sxn --frames C --ceiling \\
      --level-minus-one C,5 [--category CAT] [--loading-jank J] --search-dir DIR
  # The objects ladder where r0sxn's bound is loose (room (7,0): 80 against 84):
  ./safe-run.sh -- target/release/rewrite search --room 7,0 --level r0sxn,r0sx \\
      --level-minus-one 84,5 --frames 84 --ceiling --search-dir DIR
  # One forward and its fingerprint (the room (1,0) gate):
  ./safe-run.sh -- target/quick/rewrite forward --room 1,0 --steps 44 --tree D
  target/quick/rewrite ckhash --tree D --steps 44 --edges
  # A known route against a finished search's pruning:
  ./safe-run.sh -- target/quick/rewrite check known --search-dir DIR --inputs tas/FILE.txt
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
    /// THE SEARCH: the forward, the backward and the concrete search.
    ///
    /// Per level of `--level` (coarsest first): the forward to the horizon,
    /// every edge recorded with its remainder transfer, filtered by the
    /// previous level's arc-marked nodes; then the winning sets backward
    /// from the wins, whose optimum is a LOWER BOUND on the game's (no win
    /// refutes the horizon). At the LAST level, the concrete search inside
    /// the winning sets: its first win is the optimum, its inputs written to
    /// `<search-dir>/witness_frame_F.txt`. The verdict goes to
    /// `<search-dir>/result.json` before the exit code says it.
    Search(SearchArgs),
    /// THE DUAL SEARCH (`search::dual`): the shortest file valid in BOTH arithmetics.
    ///
    /// Rule (b): the float tool's exit is the count, and real play (16.16,
    /// from `--loading-jank`) must have left by then. Each half runs its
    /// levels to the same horizon, under `<search-dir>/fixed` and
    /// `/float`; then the concrete search over pairs of states, each half
    /// inside its own winning sets, from the larger arc bound. The global
    /// settings are the fixed half's (no `--arith float`); `--float-half`
    /// picks the float tool, which starts from its own IL load (Celia's jank
    /// unless `--float-loading-jank`). The witness, replayed in both
    /// arithmetics, goes to `<search-dir>/witness_frame_F.txt`, the
    /// outcome to `result.json`.
    Dual(DualArgs),
    /// ONE forward at ONE level, as the search runs it, with the per-frame
    /// timing line: the forward's profiling entry point.
    Forward(ForwardArgs),
    /// ONE CONCRETE RUN: the reference engine plays inputs from the room's entry.
    ///
    /// Prints the player each frame (whole frames under `--split-frame`), in
    /// `rewrite pico8 replay`'s frame convention: the basis for checking a
    /// witness against a real PICO-8. A frame that forks (`rnd`) is an
    /// error: give the balloons' seeds.
    Run(run::Args),
    /// FINGERPRINT a tree: per frame (`f{frame} count hash`) its new states
    /// counted, and an order-independent hash of their (shape, key, cell).
    Ckhash(diag::CkhashArgs),
    /// THE CHECKS of a finished tree, each against something independent.
    #[command(subcommand)]
    Check(check::Command),
    /// THE BENCHMARKS of a finished tree: one forward or backward frame.
    #[command(subcommand)]
    Bench(bench::Command),
    /// THE ARC GATE'S FINGERPRINTS of a finished search (`check::gate`;
    /// CLAUDE.md "Gates"): its `[gate]` lines, on stdout.
    ///
    /// Per level the search ran (a dual search's halves in its order), the
    /// arc phase rerun over its tree as the search ran it: W's support (its
    /// nodes and a content hash), per frame the nodes with a set and a hash
    /// of their (node, set), the arc optimum in steps; then the concrete
    /// optimum and witness from `result.json` (a dual search's: its exit and
    /// real play's). Content only: the same at every thread count.
    Gate(GateArgs),
    /// WHY A KERNEL DECLINES A STATE (`check::explain`; the decline report
    /// prints this command).
    ///
    /// The state's row (of the tree's layers through `--step`, where the
    /// forward ran it) and its kernel REBUILT as the set builds it (its text
    /// checked against the running kernel's), the kernel's graph evaluated
    /// on the row: per body that declines it (live and error both may hold)
    /// the error's disjuncts that are not false, each followed down to its
    /// first undecided condition, and its failing premises.
    Explain(ExplainArgs),
    /// DIAGNOSTIC: a tree's states at `--step`, COUNTED per shape.
    ///
    /// Per shape: its rows, every varying column's distinct values (capped
    /// at `--cap`) with the rows holding an interval or an unknown bool,
    /// and its distinct (spd.x, spd.y) pairs; with `--cell x,y` that player
    /// position's rows only, and the values of a column with few. With
    /// `--erase` or `--cell`, whether they SATURATE: every `--every` steps
    /// from 0, the new states, and the distinct ones (new, and so far) with
    /// the cells under an `--erase` prefix left out. With `--cells K`, the K
    /// positions with the most states through `--step` (their new states
    /// in its last 12 steps), and every position's new states at `--step`
    /// as a share of its states.
    Census(diag::CensusArgs),
    /// A concrete input sequence that follows a TRAJECTORY of player positions.
    ///
    /// One "x,y" per line from frame 1 (`-` accepts any), followed
    /// breadth-first over the reference engine's concrete step, deduplicated
    /// exactly, to its first win. Prints the input bytes for `rewrite run -i`
    /// / `rewrite pico8 replay`, or the first frame it could not follow.
    Trajectory(diag::TrajectoryArgs),
    /// A float witness's ROBUSTNESS to the host libm (plans/float.md, `search::robust`).
    ///
    /// `--inputs` replayed in float (the run's float tool, its seeds) under
    /// glibc's `sin`, a correctly rounded `sin` (MPFR), every `sin` call one
    /// ulp up, every call one ulp down, and `--random` runs moving each call
    /// by -1, 0 or +1 ulp at random: the frame each first wins on. A variant
    /// that differs from glibc's is reported FRAGILE (flagged, not
    /// rejected); the exit code is 0 either way.
    Robust(diag::RobustArgs),
    /// THE LEVEL -1 TABLE (`--level-minus-one`): built alone, probed against
    /// a tree, checked against a known solution.
    #[command(subcommand)]
    L1(l1::Command),
    /// Export a finished search for the web UI (`ui/`, `ui_export`).
    ///
    /// Reads its record (`result.json`: the levels, the witness and its
    /// path), the trees' checkpoint headers and forward records
    /// (`forward.tsv`) and its marks record (W's support and the arc-marked
    /// nodes over its final level's tree, `search::result::marks_path`); its
    /// room is the search's record's.
    ExportUi(ExportUiArgs),
    /// The room (`--room`) as the tools need it, as JSON on stdout.
    ///
    /// Its level; its objects in creation order (the original cart's:
    /// tile, tile position, type, whether the minimal cart has it, whether a
    /// TAS file's `[seeds]` list runs over it); what `--category` means:
    /// whether a win needs the berry (`hundred`), which dash starts are
    /// forbidden (`nodiag`, `onlydiag`), one dash past the orb (`gemskip`);
    /// and the START STATES real play can enter it with (`starts`): every
    /// loading-frame J a play of the previous room can give (`feasible`),
    /// the J's grouped by the exact start state they make (`classes`: one
    /// search per class) and the J of Celia's IL load (`celia`). Give the
    /// search's settings (the category; `--balloon-seeds` for a room with
    /// balloons), not `--loading-jank` (what it enumerates).
    Room,
    /// A WITNESS as the tools need it (`concrete::witness`), as JSON on
    /// stdout: `--inputs` replayed on the reference engine (the minimal
    /// cart) from the room's start, its balloons and chests seeded by
    /// `--balloon-seeds` as the TAS tools seed them: the frame it leaves
    /// the room (`exit`), and in a 100% category the chest seed lists to
    /// try (`chest_seed_variants`).
    Witness(run::WitnessArgs),
    /// A REAL PICO-8, headless: one room replayed, a room in real play
    /// through the chain of rooms before it, the start real play enters a
    /// room with, the probe goldens (`tools::pico8`, `tools::chain`).
    #[command(subcommand)]
    Pico8(pico8::Command),
    /// The community's TAS tools: UniversalClassicTas and Celia headless,
    /// and tasdatabase files in every tool and start mode (`tools::love`,
    /// `tools::uploads`).
    #[command(subcommand)]
    Validate(validate::Command),
    /// THE FLOAT ORACLE'S GOLDENS (`tools::golden`): the tool's exact traces
    /// of the tasdatabase's files, random files and our witnesses, to
    /// gates/TOOL (`local`: /var/tmp/float-goldens/TOOL) and its manifest.
    /// READ THE DIFF: a golden that moved means the tool's run did.
    Golden(validate::GoldenArgs),
    /// THE CATEGORY RUNNER (`tools::category`): a queue of (category, room)
    /// searches against the community tasdatabase, each result verified end
    /// to end (the reference, the search, the witness in the original cart,
    /// the upload valid in real play), one JSON line per job in
    /// OUTDIR/results.jsonl. OUTDIR/STOP stops it before the next job.
    Category(validate::CategoryArgs),
}

impl Command {
    /// A command that reads finished trees: their records give the settings.
    fn reads_trees(&self) -> bool {
        matches!(self, Command::Ckhash(_) | Command::Check(_) | Command::Bench(_) | Command::Gate(_) | Command::Explain(_) | Command::Census(_) | Command::ExportUi(_) | Command::L1(l1::Command::Probe(_)))
    }
}

/// A search's horizon, levels, directory and outputs (`search`, `dual`).
#[derive(clap::Args)]
pub struct SearchArgs {
    /// The horizon in game frames (two steps each under --split-frame):
    /// every frame up to it is searched.
    #[arg(long)]
    frames: u32,
    /// The horizon is a KNOWN solution's frame (a replayed TAS; `dual`: a
    /// known dual run's): a refutation, or no concrete witness by it, is an
    /// error.
    #[arg(long)]
    ceiling: bool,
    /// The levels (`Level::parse`), comma-separated, coarsest first:
    /// `r0sx` for a room without objects, `r0sxn` with objects,
    /// `r0sxn,r0sx` for an exact-objects level filtered by the abstract one.
    #[arg(long, default_value = "r0sx")]
    level: String,
    /// The search's directory: level i's tree goes to `level{i:02}/` under it.
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    search_dir: String,
    /// Stop at the arc bound (`dual`: the dual bound), no concrete search.
    #[arg(long)]
    no_witness: bool,
    /// A known solution (a `tas/` file or a comma list of input bytes, the
    /// spawn prologue included): the concrete search tries its input first
    /// at every frame, so the witness is the optimum closest to it (every
    /// input is still tried). When it exits by the horizon it is also
    /// CHECKED against every pruning step at every level (as `check known`;
    /// `dual`: each half's): the search fails if one drops it.
    #[arg(long)]
    prefer: Option<String>,
}

#[derive(clap::Args)]
pub struct DualArgs {
    #[command(flatten)]
    search: SearchArgs,
    /// The float half's tool.
    #[arg(long, value_enum, default_value = "celia")]
    float_half: celeste_rust::settings::FloatTool,
    /// The float half's loading jank J (Celia only; default: its IL load's).
    #[arg(long)]
    float_loading_jank: Option<usize>,
}

#[derive(clap::Args)]
pub struct ForwardArgs {
    /// The tree's last step to compute (two a game frame under
    /// --split-frame; `settings::Clock`).
    #[arg(long)]
    steps: u32,
    /// The level (`Level::parse`, e.g. `r0sxn`).
    #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
    level: Level,
    /// The tree (its files land under it).
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    tree: String,
    /// Run the REFERENCE engine instead of the kernels: `ckhash` of the
    /// two trees must agree. Slow.
    #[arg(long)]
    reference: bool,
}

#[derive(clap::Args)]
pub struct ExportUiArgs {
    /// A `rewrite search`'s directory (`result.json`, `level00/`, ...).
    #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
    search_dir: String,
    /// Output directory (the UI serves it as `data/`).
    #[arg(long, default_value = "/var/tmp/celeste-ui/data")]
    out: String,
    /// A concrete run to draw instead of the search's witness:
    /// `[label TEXT]`, `inputs a,b,..`, `[dashes F:DIR ..]`, then `f x y`
    /// per frame from 0 (`rewrite pico8 align --out DIR` writes one).
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

#[derive(clap::Args)]
pub struct GateArgs {
    /// The search's directory (its `result.json` and levels' trees).
    #[arg(long)]
    search_dir: std::path::PathBuf,
}

#[derive(clap::Args)]
pub struct ExplainArgs {
    #[command(flatten)]
    tree: TreeArg,
    /// The step the forward ran the state from (its layers through it are read).
    #[arg(long)]
    step: u32,
    /// The state, as the decline report shows it (`r{region}.{key}.c{cell}`).
    #[arg(long, value_parser = Sk::parse)]
    state: Sk,
}

/// THE TREE a command reads, its settings its records' (`TreeDir`).
#[derive(clap::Args)]
pub(crate) struct TreeArg {
    /// The tree (a forward's `--tree`, or a search's `levelNN`).
    #[arg(long)]
    tree: std::path::PathBuf,
}

impl TreeArg {
    pub(crate) fn open(&self, run: &Settings) -> Result<TreeDir> {
        TreeDir::open(&self.tree, run)
    }
}

/// THE ROW SELECTOR of the commands that read a frame's states: the frame
/// (its states, first reached there) and optionally one player position.
#[derive(clap::Args)]
pub(crate) struct Rows {
    /// The tree step whose states, first reached there, are read (default:
    /// the tree's last).
    #[arg(long)]
    step: Option<u32>,
    /// Only the states at player position "x,y".
    #[arg(long, value_parser = parse_xy)]
    cell: Option<(i32, i32)>,
}

impl Rows {
    /// The frame, and the player cell (`None`: every cell).
    pub(crate) fn of(&self, t: &TreeDir) -> Result<(u32, Option<u32>)> {
        Ok((t.frame(self.step)?, self.cell.map(cell_at).transpose()?))
    }

    /// The tree to the frame, the frame, and its states (of the cell) in the
    /// tree's order (by region, key, cell).
    pub(crate) fn states<N: celeste_core::arith::Arithmetic>(&self, t: &TreeDir) -> Result<(Tree<N>, u32, Vec<Sk>)> {
        let (frame, cell) = self.of(t)?;
        let tree = Tree::<N>::open(&t.dir, frame)?;
        let all: Vec<Sk> = tree.layer(frame, None, t.settings.threads())?.0.states_iter().filter(|s| cell.is_none_or(|c| s.cell() == c)).collect();
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
    celeste_rust::storage::cells::cell_of(x, y)
}

/// A cell, for reading: the player's position.
pub(crate) fn where_(c: u32) -> String {
    celeste_rust::storage::cells::cell_xy(c).map(|(x, y)| format!("({x}, {y})")).unwrap_or_else(|| "no player".to_string())
}

/// `search` and `dual` (`float`: the float half's settings): `search::run`,
/// its verdict reported.
fn search(cfg: &Settings, float: Option<&Settings>, a: &SearchArgs) -> Result<()> {
    let prefer = a.prefer.as_deref().map(celeste_rust::concrete::read_inputs).transpose()?;
    let dir = std::path::Path::new(&a.search_dir);
    let spec = celeste_rust::search::SearchSpec { settings: cfg, level: &a.level, horizon: a.frames, ceiling: a.ceiling, dir, bound_only: a.no_witness, prefer: prefer.as_deref(), dual: float };
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
/// resumed) to step `to` by `engine` from `initial`; its first win, wall
/// time and position graph printed.
fn forward<N: celeste_rust::search::rem::Rem>(
    dir: &std::path::Path,
    to: u32,
    level: Level,
    cfg: &celeste_rust::settings::Settings,
    engine: Box<dyn FrameStep<N>>,
    initial: impl FnOnce() -> Result<Vec<Rt2<N>>>,
) -> Result<()> {
    let t = std::time::Instant::now();
    // Resumed or started (`grow_tree`).
    let fwd = celeste_rust::frame::grow_tree(dir, to, cfg.minus_one(), None, level, || Ok(engine), initial, cfg)?;
    let wall = t.elapsed().as_secs_f64();
    match fwd.win_frame {
        Some(h) => println!("win at f{h} ({wall:.2} s)"),
        None => println!("no win by f{} ({wall:.2} s)", fwd.frames),
    }
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
    // Before anything frees: the allocator's purge delay (why: there).
    celeste_rust::compiled::set_purge_delay(100);
    // `--version`: the file formats a tree is written in (tools/fixtures.sh).
    let matches = Cli::command().version(&*Box::leak(celeste_rust::settings::formats().into_boxed_str())).get_matches();
    let cli = Cli::from_arg_matches(&matches).unwrap_or_else(|e| e.exit());
    if cli.command.reads_trees() {
        let given = tree_flags_given(&matches);
        anyhow::ensure!(given.is_empty(), "{}: this command reads a finished tree and takes its settings from the tree's record (tree.txt); give only the run's own (--threads, --phases)", given.join(" "));
    }
    // The commands that run in float (a tree's arithmetic is its record's).
    let float = cli.command.reads_trees() || matches!(cli.command, Command::Search(_) | Command::Forward(_) | Command::Robust(_) | Command::L1(_));
    let cfg = celeste_rust::settings::startup(&cli.settings, float, |s| {
        // `bench frame` times the workers' phases.
        if let Command::Bench(bench::Command::Frame(_)) = &cli.command {
            s.phases = true;
        }
    })?;
    match cli.command {
        Command::Search(a) => search(&cfg, None, &a),
        Command::Dual(a) => {
            let float = cfg.float_half(a.float_half, a.float_loading_jank)?;
            search(&cfg, Some(&float), &a.search)
        }
        Command::Forward(a) => {
            let t = std::time::Instant::now();
            let (dir, level, reference) = (std::path::Path::new(&a.tree), a.level, a.reference);
            let what = if reference { ", REFERENCE engine" } else { "" };
            let arith = if cfg.arith == Arith::Fixed { String::new() } else { format!(", {}", cfg.arith) };
            celeste_rust::with_arith!(cfg.arith, N => {
                let engine: Box<dyn FrameStep<N>> = if reference { Box::new(celeste_rust::engines::reference::RefFrameStep::<N>::new(&cfg, level)?) } else { Box::new(celeste_rust::compiled::Kernels::<N>::new(&cfg, level)?) };
                eprintln!("[fwd] engine up in {:.2} s ({level}{what}{arith})", t.elapsed().as_secs_f64());
                forward(dir, a.steps, level, &cfg, engine, || Ok(vec![celeste_rust::engines::reference::ReferenceEngine::<N>::new(&cfg)?.initial()?.canonicalized()]))
            })
        }
        Command::Run(a) => run::run(&cfg, a),
        Command::Witness(a) => run::witness(&cfg, a),
        Command::Pico8(c) => pico8::run(&cfg, c),
        Command::Validate(c) => validate::run(c),
        Command::Golden(a) => validate::golden(a),
        Command::Category(a) => validate::category(a),
        Command::Ckhash(a) => diag::ckhash(&cfg, a),
        Command::Check(c) => check::run(&cfg, c),
        Command::Bench(c) => bench::run(&cfg, c),
        Command::Gate(a) => {
            for l in celeste_rust::check::gate::search_lines(&a.search_dir, &cfg)? {
                println!("{l}");
            }
            Ok(())
        }
        Command::Explain(a) => {
            let t = a.tree.open(&cfg)?;
            print!("{}", celeste_rust::with_arith!(t.settings.arith, N => celeste_rust::check::explain::explain::<N>(&t, a.step, a.state)?));
            Ok(())
        }
        Command::Census(a) => diag::census(&cfg, a),
        Command::Trajectory(a) => diag::trajectory(&cfg, a),
        Command::Robust(a) => diag::robust(&cfg, a),
        Command::L1(c) => l1::run(&cfg, c),
        Command::ExportUi(a) => celeste_rust::ui_export::export(
            std::path::Path::new(&a.search_dir),
            std::path::Path::new(&a.out),
            cfg.threads(),
            celeste_rust::ui_export::Paths {
                witness: a.witness.as_deref().map(std::path::Path::new),
                reference: a.reference.as_deref().map(std::path::Path::new),
                download: a.download.as_deref().map(std::path::Path::new),
            },
        ),
        Command::Room => {
            let mut room = serde_json::to_value(celeste_rust::starts::starts(&cfg)?)?;
            let c = cfg.category;
            room["objects"] = serde_json::to_value(celeste_rust::trace::cart::room_objects(cfg.room))?;
            room["category"] = serde_json::json!({ "name": c.to_string(), "hundred": c.hundred(), "nodiag": c.nodiag(), "onlydiag": c.onlydiag(), "gemskip": c.gemskip() });
            println!("{room}");
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

    /// `l1 probe` reads a finished tree: its settings are the tree's
    /// record, and one given on its command line (a wrong `--room` would
    /// probe another room's table against the tree) is refused.
    #[test]
    fn the_level_minus_one_probe_takes_the_trees_settings() {
        use clap::{CommandFactory, FromArgMatches};
        let m = super::Cli::command().try_get_matches_from(["rewrite", "l1", "probe", "--room", "2,0", "--tree", "T", "--steps", "9", "--from-step", "1", "--to-step", "2"]).expect("the line parses");
        let cli = super::Cli::from_arg_matches(&m).expect("the line parses");
        assert!(cli.command.reads_trees());
        assert_eq!(super::tree_flags_given(&m), ["--room"]);
    }
}
