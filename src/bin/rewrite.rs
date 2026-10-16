//! The search driver: `rewrite search` (the level-0 forward, the rotation
//! graph's backward and the concrete search), `rewrite forward` (one
//! forward pass with timing), `rewrite ckhash` (checkpoint fingerprints),
//! `rewrite export-ui` (the web UI's data), and the diagnostics that read a
//! finished tree (rows by name: `search::inspect`).

use anyhow::{Context, Result};
use celeste_core::arith::{Arithmetic, Fixed};
use celeste_core::lua_f64::LuaF64;
use celeste_engine::runtime2::{Rt2, P8};
use celeste_rust::settings::Arith;
use celeste_rust::trace::refengine::{FloatRefEngine, RefEngine};
use celeste_rust::frame::{widen_rt2_to, widened_keys, wins_of, Block, FrameStep, Visited};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use celeste_rust::abstraction::Level;
use celeste_rust::search::rem::Rem;
use celeste_rust::search::inspect::{brief, cell_names, parse_xy, player_summary, project_all, project_onto, project_row, Proj};
use clap::{Parser, Subcommand};

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
run under others; flags never default to the record. --override-settings
proceeds past a different --region only. CELESTE_* environment variables are
refused. Run everything under ./safe-run.sh (a memory cap).

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
  target/quick/rewrite ckhash --room 1,0 --to 44 --checkpoint-dir D --edges
  # Help on one command:
  target/quick/rewrite search --help";

#[derive(Parser)]
#[command(about = "The abstract TAS search", long_about = LONG_ABOUT, after_long_help = EXAMPLES)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    /// The run's settings (`settings::SettingsArgs`): given anywhere on the line.
    #[command(flatten)]
    settings: celeste_rust::settings::SettingsArgs,
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
    Search {
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
        /// pruning step at every level (`search::known`, as `check-known`):
        /// the search fails if one drops it.
        #[arg(long)]
        prefer: Option<String>,
    },
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
    Dual {
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
    },
    /// A float witness's ROBUSTNESS to the host libm (plans/float.md,
    /// "The robustness report"; `search::robust`): `--inputs` replayed in float (the
    /// run's float tool, its seeds) under glibc's `sin`, a correctly rounded
    /// `sin` (MPFR), every `sin` call one ulp up, every call one ulp down,
    /// and `--random` runs moving each call by -1, 0 or +1 ulp at random;
    /// the frame each first wins on. A variant that differs from glibc's is
    /// reported FRAGILE (flagged, not rejected); the exit code is 0 either
    /// way.
    Robust {
        #[arg(long)]
        inputs: String,
        #[arg(long, default_value_t = 8)]
        random: u64,
    },
    /// A KNOWN solution against the pruning of finished search trees
    /// (`search::known`): stepped through the reference engine, at every
    /// step it must pass level -1 (the filter the tree recorded) and the
    /// objects ladder's filter, be in the tree with its exact remainder
    /// inside its state's hull, reached from its parent by a recorded edge
    /// that maps the remainder (the hull forward's claim; whole frames
    /// only), and at every frame boundary project onto an arc-graph node
    /// whose W holds its exact remainder, reached (at a level that filters
    /// the next) - per level of `--level`, the arc phase rerun over
    /// `<checkpoint-dir>/level{i:02}` as `search` runs it, without the
    /// concrete search. Exits non-zero on the first check that drops it,
    /// with the step, the node and the state.
    CheckKnown {
        /// The search's checkpoint dir (its `level{i:02}` trees).
        #[arg(long)]
        checkpoint_dir: String,
        /// The search's levels, comma-separated, coarsest first.
        #[arg(long, default_value = "r0sx")]
        level: String,
        /// The known solution: a `tas/` file or a comma list of input bytes,
        /// the spawn prologue included.
        #[arg(long)]
        inputs: String,
        /// The search's horizon in frames (the trees must reach it).
        #[arg(long)]
        horizon: u32,
    },
    /// ONE forward pass at ONE level, exactly as the search runs it, with the
    /// per-frame timing line. The profiling entry point for the forward.
    Forward {
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
    },
    /// THE TREE REPLAYS FROM ITS RECORDED GRAPH (`search::hull_replay`): the
    /// start's hull pushed along the recorded edges through their transfers,
    /// frame by frame, must reach exactly the tree's states first at each
    /// frame (else an error: an edge was not recorded). Prints per frame
    /// the states first reached and the edges recorded into it.
    HullReplay {
        /// The tree (a `forward` checkpoint dir, or a search's `levelNN`).
        #[arg(long)]
        checkpoint_dir: String,
        /// Last frame.
        #[arg(long)]
        to: u32,
    },
    /// Fingerprint a forward checkpoint tree: per frame, the lane count and an
    /// order-independent hash of its (row key, cell) set, independent of the
    /// storage format.
    Ckhash {
        /// The tree (the dir holding `frames/`: a `forward` checkpoint dir, or a
        /// search's `levelNN`).
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// Last frame to fingerprint.
        #[arg(long)]
        to: u32,
        /// Also list the N most populated player positions at frame `to`
        /// (to pick a synthetic `--win-at` target with real fan-out).
        #[arg(long)]
        cells: Option<usize>,
        /// Also fingerprint each frame's recorded edges (`e{frame} count
        /// hash`) by the states they join and their transfers: two trees
        /// with the same lines have the same graph, whatever ids they gave
        /// its transfers.
        #[arg(long)]
        edges: bool,
        /// Also fingerprint each frame's FRONTIER (`h{frame} count hash`,
        /// its hull file): every state whose remainder hull grew there, by
        /// its (shape, key, cell), layer and hull.
        #[arg(long)]
        hulls: bool,
        /// With `--edges`: print frame F's edges (target, source, transfer)
        /// on stderr, to diff two trees' edges at the frame whose line differs.
        #[arg(long)]
        dump_edges: Option<u32>,
    },
    /// ONE FORWARD FRAME of a tree, repeatably: the forward as it stood
    /// after `--frame` F (`ForwardState::at`: the tree's visited set,
    /// transfers, level -1 filter and pos graph at F; later frames ignored)
    /// runs the wave of F+1 `--reps` times, the edges into a scratch dir.
    /// Prints (stdout, the same every rep, else an error) `ckhash --edges`'s
    /// lines for F+1 - `f{F+1}` of the new states and, with `--edges`,
    /// `e{F+1}` of the edges - so a tree extended past F checks it; per rep
    /// (stderr) the wave (the region phase's worker time inside it) and the
    /// frame's end walls and the workers' phases (`--phases`: pack, kernel,
    /// emit, unit end, regions, frame end);
    /// `kernel lanes: ... missed 0`. With `--metrics FILE`, the EXACT
    /// metrics of the first rep (kernel census and dynamic instructions,
    /// lanes, emissions, states, edges: `tools/metrics_diff.py` compares
    /// them with a pinned file); the same at every `--threads`.
    BenchFrame {
        /// The tree (`frames/` under it).
        #[arg(long)]
        level_dir: String,
        /// The frame F to run F -> F+1 from (default: the tree's last).
        #[arg(long)]
        frame: Option<u32>,
        /// Run the wave this many times, each from a copy (every rep's lines must
        /// agree).
        #[arg(long, default_value_t = 1)]
        reps: usize,
        /// The level to run at (the tree's).
        #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
        level: Level,
        /// Record the frame's edges (into a scratch dir under the tree,
        /// deleted per rep), as the search's forward does.
        #[arg(long, default_value_t = false)]
        edges: bool,
        /// Write the first rep's exact metrics to this file.
        #[arg(long)]
        metrics: Option<String>,
        /// Checkpoint each rep's frame as the forward does (the frame file
        /// and the hull file, each read back and checked; the pos graph),
        /// into the scratch dir, timed; prints a `b{F+1}` line over the
        /// bytes of every file the frame wrote (needs `--edges`).
        #[arg(long, default_value_t = false)]
        checkpoint: bool,
    },
    /// ONE BACKWARD FRAME of the arc phase, repeatably: a finished level-0
    /// tree's edge files to `--horizon` opened (`arc_dp::Seeds`; timed), the
    /// winning sets pushed down to `W_{F+1}` (`arc_dp::Backward`), then `W_F`
    /// from them `--reps` times from a copy. Prints (stdout) `W_F`'s
    /// fingerprint as the arc gate's line (`[gate] hH W fF count hash`, over
    /// each node's (shape, key, cell) and its set), the same every rep; per
    /// rep the read, pull and merge walls.
    BenchBackward {
        /// The level-0 tree (`frames/`, `edges/` under it).
        #[arg(long)]
        level_dir: String,
        /// The search's horizon, in steps: the tree must reach it; the graph and
        /// the winning sets are built for it.
        #[arg(long)]
        horizon: u32,
        /// The frame F whose winning sets are computed (from F+1's).
        #[arg(long)]
        frame: u32,
        /// Compute W_F this many times, each from a copy.
        #[arg(long, default_value_t = 1)]
        reps: usize,
    },
    /// BENCH: replay one captured frame through the REAL storage code
    /// (`storage::bench`): a forward or `bench-frame` run with
    /// `--emit-capture DIR` writes each wave's `DIR/f{F}/`; this replays
    /// its units against `--tree`'s visited set at frame F-1 through the
    /// whole wave - the sinks, the region phase, the frame's end, the edge
    /// file - printing per rep the wall times and the `ckhash` lines of the
    /// new states (`f`) and of the edges (`e`), to compare with the real
    /// tree's (`ckhash --edges`). Run it with the capture's settings
    /// (`DIR/f{F}/env.txt`: the level and the settings line).
    BenchStorage {
        /// A captured frame's directory (`DIR/f{F}`).
        #[arg(long)]
        capture: String,
        /// The level dir the capture's frame was run on (its frames to F-1).
        #[arg(long)]
        tree: String,
        /// The level the capture was run at (its `env.txt`).
        #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
        level: Level,
        /// Replay the capture this many times.
        #[arg(long, default_value_t = 3)]
        reps: usize,
    },
    /// DIAGNOSTIC: the TRANSFERS of a level-0 tree's edges, checked. Every
    /// recorded edge carries a transfer; then `--samples` records per frame,
    /// the source row stepped by the REFERENCE engine at the level (all 64
    /// inputs, the row's unknowns forked as the kernels fork them:
    /// `RefEngine::step_at`): at remainders INSIDE the guard some input must
    /// reach the target with the predicted remainder and every one that
    /// reaches it must land where some record of that (pred, target) taking
    /// the point predicts, and at points no record of the pair covers none
    /// may. Fails (exit != 0) on any disagreement.
    ArcCheck {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The first frame whose records are checked.
        #[arg(long, default_value_t = 1)]
        from: u32,
        /// The last frame whose records are checked.
        #[arg(long)]
        to: u32,
        /// Records sampled per frame.
        #[arg(long, default_value_t = 20)]
        samples: usize,
        /// The tree's level (what a successor is projected onto).
        #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
        level: Level,
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
    },
    /// THE WIDENING TABLE'S TWO SIDES AGREE (`celeste_engine::widening`):
    /// every row stored at step `frame` - widened in the kernels' graph - is
    /// a fixed point of the block side's widening at `--level`
    /// (`Rt2::widen_to`), compared exactly (canonical bytes), and so is every
    /// row the kernels emit from `--emitted` of them (evenly spaced). A row
    /// the block side changes is printed with the fields that moved; the run
    /// exits non-zero. (Not mid split frame at a near level: there the
    /// graph side keeps a floor's computed `collideable` in the player's
    /// probe window, which a frame boundary's widening does not.)
    WidenCheck {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The step whose stored rows are checked.
        #[arg(long)]
        frame: u32,
        /// The tree's level (the widening checked).
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// Also run this many of the rows (evenly spaced) through the kernels and
        /// check what they emit.
        #[arg(long, default_value_t = 0)]
        emitted: usize,
    },
    /// THE KERNELS AGAINST THE REFERENCE ENGINE, row by row: `samples` stored
    /// rows first reached at step `frame` (in `--cell x,y` if given) each run
    /// one step through the kernels of `--level` and through `RefEngine`, both
    /// sides projected onto the level and compared by named fields. A
    /// reference successor the kernels miss is a SOUNDNESS gap; a kernel
    /// successor the reference never makes is a PRECISION gap. Every row,
    /// kernel successor and reference leaf must also convert alike on the
    /// tracer's side and the reference's (`block::check_agreement`).
    RefCheck {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The step whose rows (first reached there) are sampled.
        #[arg(long)]
        frame: u32,
        /// The tree's level (the kernels run, and both sides are projected onto
        /// it).
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// Rows sampled.
        #[arg(long, default_value_t = 20)]
        samples: usize,
        /// Only rows at player position "x,y".
        #[arg(long, value_parser = parse_xy)]
        cell: Option<(i32, i32)>,
        /// Show up to this many differing successors per row and side.
        #[arg(long, default_value_t = 3)]
        show: usize,
        /// At most this many reference fork paths a row: a row that takes
        /// more is SKIPPED (counted and reported), as in `arc-check`.
        #[arg(long)]
        path_cap: Option<usize>,
        /// A differing row's successors in full (every named field, both
        /// sides, the row itself), not the player's fields alone.
        #[arg(long)]
        full: bool,
    },
    /// DIAGNOSTIC: re-run ONE stored state through the kernels of `--level`
    /// and print every successor (projected without `--erase`), next to the
    /// state itself: what the kernel makes of a single row. `--row L:S:R` is
    /// the row's (layer, seq, row) in the tree's frame files.
    RerunRow {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The row, `LAYER:N`: the N-th state of frame LAYER in the tree's
        /// order (by region, key, cell).
        #[arg(long)]
        row: String,
        /// The level whose kernels run it (the tree's).
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// Named-cell prefixes left out of the printed projections, comma-
        /// separated.
        #[arg(long, default_value = "fall_floor")]
        erase: String,
    },
    /// DIAGNOSTIC: where a tree's states come from. `samples` states of
    /// `--coarse` first reached at `step` in `--cell` are walked back through
    /// the recorded edges, `depth` steps, printing each predecessor's cell and
    /// what changed (named value cells but the `--erase` prefixes).
    ///
    /// With `--real` (a finer tree): a coarse state whose projection the real
    /// tree never reached in its cell by `step` is SPURIOUS; field
    /// distributions are compared, and each walk stops at the first
    /// predecessor the real tree did reach - the first spurious transition.
    Spurious {
        /// The coarse level's tree, whose states are walked back.
        #[arg(long)]
        coarse: String,
        /// A finer level's tree (the real states the coarse ones should project
        /// from).
        #[arg(long)]
        real: Option<String>,
        /// The step at which the sampled states were first reached.
        #[arg(long)]
        step: u32,
        /// The player cell `x,y`, or a rect `x0,x1,y0,y1` of cells (inclusive).
        #[arg(long)]
        cell: String,
        /// Named-cell prefixes left out of what changed, comma-separated.
        #[arg(long, default_value = "fall_floor")]
        erase: String,
        /// States sampled.
        #[arg(long, default_value_t = 8)]
        samples: usize,
        /// Steps walked back per sample.
        #[arg(long, default_value_t = 40)]
        depth: u32,
        /// Write sample 0's chain, frame 1 to `step`, as a keyed trajectory
        /// (`trajectory --trajectory FILE --spec LEVEL --tree COARSE` realizes
        /// it: the keys are the coarse tree's). Not
        /// with `--real`.
        #[arg(long)]
        chain_out: Option<String>,
    },
    /// DIAGNOSTIC: a tree's states, counted (cells named by
    /// `inspect::cell_names`). Per shape at `--frame`: its rows, every
    /// varying column's distinct values (capped at `--cap`) with the rows
    /// holding an interval or an unknown bool, and its distinct (spd.x,
    /// spd.y) pairs; with `--cell x,y` that player position's rows only, and
    /// the values of a column with few. With `--erase` or `--cell`, whether
    /// they SATURATE: from frame 0, every `--every` frames, the new states,
    /// and the distinct ones (new, and so far) with the named cells under
    /// an `--erase` prefix left out. With `--cells K`, the K player
    /// positions with the most states through `--frame` (their new states
    /// in its last 12 frames), and every position's new states at `--frame`
    /// as a share of its states.
    Census {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The frame (step) counted.
        #[arg(long)]
        frame: u32,
        /// Stop counting a column's distinct values past this many.
        #[arg(long, default_value_t = 1 << 24)]
        cap: usize,
        /// Only the states at player position "x,y".
        #[arg(long, value_parser = parse_xy)]
        cell: Option<(i32, i32)>,
        /// Named-cell prefixes left out of a state, comma-separated (e.g.
        /// `spd.,rem.,player[3].dash`).
        #[arg(long)]
        erase: Option<String>,
        /// The saturation every this many frames.
        #[arg(long, default_value_t = 4)]
        every: u32,
        /// The player positions with the most states, this many.
        #[arg(long)]
        cells: Option<usize>,
    },
    /// Export a finished search for the web UI (`ui/`) from its record
    /// (`result.json`), the trees' checkpoint HEADERS and forward records
    /// (`forward.tsv`) and the marks files. See `search::ui_export`.
    ExportUi {
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
    },
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
    /// A concrete input sequence that follows a given TRAJECTORY of player
    /// positions (one "x,y" per line from frame 1; `-` accepts any), found
    /// breadth-first over the reference engine's concrete step, deduplicated
    /// exactly. Prints the input bytes for `concrete_run -i` /
    /// `pico8_diff/replay.py`, or the first frame it could not follow.
    Trajectory {
        /// One line per frame: `x,y`, `-`, or `x,y K0 K1` - a position AND
        /// the row key (hex) its state must have projected onto `--spec`: an
        /// abstract chain to realize (`spurious --chain-out` writes one).
        #[arg(long)]
        trajectory: String,
        /// Stop once a win is reached (default), else follow to the end.
        #[arg(long, default_value_t = true)]
        stop_at_win: bool,
        /// The level the keyed lines are projections onto (e.g. `r0sxhn`).
        #[arg(long, value_parser = Level::parse)]
        spec: Option<Level>,
        /// The tree (level dir) whose key space the keyed lines' keys are in
        /// (exact keys are a tree's dictionaries' indices).
        #[arg(long)]
        tree: Option<String>,
    },
    /// DIAGNOSTIC: follow a CONCRETE input sequence through one level's tree:
    /// per frame, whether the projected concrete state is a row of the tree.
    /// At the first frame it is not, or at the win (never stored), the parent
    /// row goes through the kernels and the concrete successor is printed
    /// next to the closest kernel successors: no equal one is a SOUNDNESS
    /// gap; an equal one was not stored (a win, or the level -1 filter).
    Follow {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The tree's level (the concrete states are projected onto it).
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// The input bytes: a file in the `tas/` format, or a comma list.
        #[arg(long)]
        inputs: String,
        /// How many of the closest kernel successors to print.
        #[arg(long, default_value_t = 5)]
        show: usize,
    },
    /// The level -1 table (`--room`) against a known solution:
    /// its concrete states (reference engine) at every frame f before the
    /// exit, each with the table's d. One that is too late for `--horizon`
    /// (f + d > H, H the solution's exit frame) is a SOUNDNESS violation of
    /// the table: exits non-zero. An `rnd` fork checks every leaf.
    L1Check {
        /// The input bytes: a file in the `tas/` format, or a comma list.
        #[arg(long)]
        inputs: String,
        /// H: the frame the solution exits at (a state with f + d > H is too
        /// late).
        #[arg(long)]
        horizon: u32,
        /// The table's speed bound S (px per frame).
        #[arg(long, default_value_t = 5)]
        spd: i32,
    },
}

/// A tree through its last frame (`storage::layer::Tree`): its codebooks,
/// its frames' states, their rows decoded on demand.
fn tree_of(dir: &std::path::Path) -> Result<Tree> {
    Tree::open_all(dir)
}

/// A comma-separated list of name prefixes (`--erase`).
fn prefixes(list: &str) -> Vec<String> {
    list.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect()
}

/// A player position's cell (`--cell x,y`).
fn cell_at((x, y): (i32, i32)) -> Result<u32> {
    celeste_rust::search::pos_graph::cell_of(x, y)
}

/// A cell, for reading: the player's position.
fn where_(c: u32) -> String {
    celeste_rust::search::pos_graph::cell_xy(c).map(|(x, y)| format!("({x}, {y})")).unwrap_or_else(|| "no player".to_string())
}

/// The predecessors of state `s`, first reached at `layer`, recorded at
/// that frame (states of layer `layer - 1`).
fn preds_of(edges: &celeste_rust::storage::edges::EdgeStore, s: Sk, layer: u32) -> Result<Vec<Sk>> {
    Ok(edges.preds_at(s, layer)?.into_iter().map(|(p, _)| p).collect())
}

/// Every row of `rt2` (of layer `layer`) through ONE forward frame of
/// `engine` (an empty visited set, no filter): the successors and whether
/// one wins.
fn rerun<N: celeste_rust::search::rem::Rem>(engine: &dyn FrameStep<N>, rt2: Rt2<N>, layer: u32) -> Result<(Vec<Block<N>>, bool)> {
    let wave = celeste_rust::storage::wave::one_frame(engine, vec![Block::from_rt2(rt2)], layer + 1)?;
    let mut won = false;
    for b in &wave.next {
        won |= wins_of(b.rt2(), &engine.settings())?.contains(&true);
    }
    Ok((wave.next, won))
}

/// `ref-check`'s arguments (`Command::RefCheck`).
struct RefCheckArgs {
    level_dir: String,
    frame: u32,
    level: Level,
    samples: usize,
    cell: Option<(i32, i32)>,
    show: usize,
    path_cap: Option<usize>,
    full: bool,
}

/// `ref-check` in arithmetic `A`, its kernels `K`.
fn ref_check<K, A>(cfg: &celeste_rust::settings::Settings, o: RefCheckArgs) -> Result<()>
where
    K: celeste_rust::frame::FrameStep<A::Num> + KernelsNew,
    A: celeste_rust::transpile::garith::GraphArith,
    A::Num: celeste_rust::search::rem::Rem,
{
            let RefCheckArgs { level_dir, frame, level, samples, cell, show, path_cap, full } = o;
            let dir = std::path::Path::new(&level_dir);
            cfg.check(dir, level)?;
            let kernels = K::new(cfg, level)?;
            kernels.warm();
            let only = cell.map(cell_at).transpose()?;
            // Every candidate state, then `samples` evenly spaced.
            let tree = Tree::<A::Num>::open(dir, frame)?;
            let geo = celeste_rust::storage::GEOMETRY;
            let all: Vec<Sk> = tree.layer(frame, None, cfg.threads())?.0.states_iter().filter(|s| only.is_none_or(|c| s.cell(&geo) == c)).collect();
            anyhow::ensure!(!all.is_empty(), "no rows at step {frame}");
            let n = samples.min(all.len());
            let picked: Vec<Sk> = (0..n).map(|k| all[k * all.len() / n]).collect();
            // One row: (its report, reference-only successors, kernel-only,
            // skipped over the path cap, lanes converted alike).
            let one = |reference: &mut celeste_rust::trace::refengine::ReferenceEngine<A>, sk: Sk| -> Result<(String, usize, usize, usize, usize)> {
                let rt2 = tree.row(frame, sk, cfg.room)?;
                let at = where_(sk.cell(&geo));
                let id = sk.show();
                // The row, its kernel successors and its reference leaves
                // convert alike on the tracer's side and the reference's
                // (`block::check_agreement`).
                let mut alike = 0;
                let mut agree = |reference: &celeste_rust::trace::refengine::ReferenceEngine<A>, b: &Rt2<A::Num>| -> Result<()> {
                    reference.check_agreement(b).map_err(|e| e.context(format!("state {id} at {at}: the one converter")))?;
                    alike += b.width;
                    Ok(())
                };
                agree(reference, &rt2)?;
                // The kernels' successors.
                let (next, _) = rerun(&kernels, rt2.clone_block(), frame)?;
                let mut kset: std::collections::BTreeSet<Proj> = Default::default();
                for b in next {
                    agree(reference, b.rt2())?;
                    kset.extend(project_onto(&mut b.into_rt2(), level));
                }
                // The reference engine's, from the row projected onto the level.
                let mut input = rt2.clone_block();
                widen_rt2_to(&mut input, level);
                let mut rset: std::collections::BTreeSet<Proj> = Default::default();
                let leaves = match reference.run_lane(&input, 0) {
                    Err(e) if e.downcast_ref::<celeste_rust::trace::refdriver::TooManyPaths>().is_some() => {
                        return Ok((format!("[ref-check] state {id} at {at}: SKIPPED ({e})\n"), 0, 0, 1, alike));
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
                let mut out = format!("[ref-check] state {id} at {at}: kernels {} successors, reference {}: {verdict} ({} only in the reference, {} only in the kernels)\n", kset.len(), rset.len(), missing.len(), extra.len());
                let says = |p: &Proj| if full { format!("{p:?}") } else { brief(p) };
                for p in missing.iter().take(show) {
                    out += &format!("    only in the REFERENCE (soundness gap): {}\n", says(p));
                }
                for p in extra.iter().take(show) {
                    out += &format!("    only in the KERNELS (precision gap): {}\n", says(p));
                }
                if full && verdict != "ok" {
                    out += &format!("    the row: {:?}\n", project_all(&rt2));
                    for p in kset.iter().take(show) {
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
                    let mut reference = celeste_rust::trace::refengine::ReferenceEngine::<A>::new(cfg)?.at_level(level);
                    if let Some(cap) = path_cap {
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
            println!("[ref-check] {n} rows ({skipped} SKIPPED over the path cap): {rows_bad} differ; {sound_gaps} successors only in the reference, {precision_gaps} only in the kernels");
            println!("[ref-check] the one converter: {alike} lanes (the rows, their kernel successors and reference leaves) convert alike on both sides");
            celeste_rust::compiled::dispatch::print_kernel_hits();
            anyhow::ensure!(sound_gaps == 0, "ref-check: {sound_gaps} reference successors the kernels miss (a SOUNDNESS gap)");
            Ok(())
}

/// `census`'s arguments (`Command::Census`).
struct CensusArgs {
    level_dir: String,
    frame: u32,
    cap: usize,
    cell: Option<(i32, i32)>,
    erase: Option<String>,
    every: u32,
    cells: Option<usize>,
}

/// `census` (`Command::Census`).
fn census(cfg: &celeste_rust::settings::Settings, a: CensusArgs) -> Result<()> {
    use celeste_engine::exact::Code;
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let dir = std::path::Path::new(&a.level_dir);
    let tree = Tree::<P8>::open(dir, a.frame)?;
    let ids = celeste_rust::compiled::ids();
    let geo = celeste_rust::storage::GEOMETRY;
    let only = a.cell.map(cell_at).transpose()?;
    let keep = |s: Sk| only.is_none_or(|c| s.cell(&geo) == c);
    // Per shape: its rows, per column its distinct codes (capped), whether
    // it varies, the rows holding an interval or an unknown bool, the named
    // cells and the distinct (spd.x, spd.y) pairs.
    struct S {
        rows: u64,
        cols: Vec<rustc_hash::FxHashSet<Code>>,
        varying: Vec<bool>,
        ivals: Vec<u64>,
        ubools: Vec<u64>,
        names: std::collections::HashMap<usize, String>,
        spd: rustc_hash::FxHashSet<(Code, Code)>,
    }
    let mut by_shape: std::collections::BTreeMap<u64, S> = Default::default();
    for (_, rt2) in tree.rows(a.frame, cfg.room, 1 << 20, &keep, cfg.threads())? {
        let n = rt2.cols.len();
        let st = by_shape.entry(rt2.shape_hash).or_insert_with(|| S { rows: 0, cols: vec![Default::default(); n], varying: vec![false; n], ivals: vec![0; n], ubools: vec![0; n], names: cell_names(&rt2, ids), spd: Default::default() });
        st.rows += rt2.width as u64;
        for (c, col) in rt2.cols.iter().enumerate() {
            if !matches!(rt2.structure[c], Cell2::Val) {
                continue;
            }
            if let Col::U(v) = col {
                if st.cols[c].len() < a.cap {
                    st.cols[c].insert(Code::of(*v));
                }
                continue;
            }
            st.varying[c] = true;
            for r in 0..rt2.width {
                let v = col.at(r);
                match v {
                    AV::Ival(lo, hi) if lo != hi => st.ivals[c] += 1,
                    AV::UBool => st.ubools[c] += 1,
                    _ => {}
                }
                if st.cols[c].len() < a.cap {
                    st.cols[c].insert(Code::of(v));
                }
            }
        }
        let named = |want: &str| st.names.iter().find(|(_, n)| n.as_str() == want).map(|(c, _)| *c);
        if let (Some(sx), Some(sy)) = (named("spd.x"), named("spd.y")) {
            for r in 0..rt2.width {
                if st.spd.len() >= a.cap {
                    break;
                }
                st.spd.insert((Code::of(rt2.cols[sx].at(r)), Code::of(rt2.cols[sy].at(r))));
            }
        }
    }
    for (shape, st) in &by_shape {
        let mut cards: Vec<(usize, usize)> = (0..st.cols.len()).filter(|&c| st.cols[c].len() > 1 || st.varying[c]).map(|c| (st.cols[c].len(), c)).collect();
        cards.sort_unstable_by(|a, b| b.cmp(a));
        let product: f64 = cards.iter().map(|&(n, _)| n as f64).product();
        println!("shape {shape:#x}: {} rows, {} varying columns, cardinality product {:.2e}, spd pairs {}", st.rows, cards.len(), product, st.spd.len());
        for &(n, c) in cards.iter().take(24) {
            println!("  c{c:<4} {n:>10} {:<18} intervals {:>10} unknown-bools {:>10}", st.names.get(&c).map_or("", |s| s.as_str()), st.ivals[c], st.ubools[c]);
            // One position's census lists the numbers themselves.
            if only.is_some() && n <= 64 {
                let mut vals: Vec<f64> = st.cols[c].iter().filter(|v| v.kind == 0 && v.a == v.b).map(|v| v.a as i32 as f64 / 65536.0).collect();
                vals.sort_by(|a, b| a.total_cmp(b));
                if !vals.is_empty() {
                    println!("        {}", vals.iter().map(|v| format!("{v:.4}")).collect::<Vec<_>>().join(" "));
                }
            }
        }
    }
    // SATURATION: the distinct states with the erased cells left out, frame
    // by frame (pointers count as structure).
    if a.erase.is_some() || only.is_some() {
        let erase = prefixes(a.erase.as_deref().unwrap_or(""));
        let code = |v: AV| -> Code { Code::of(if let AV::Ptr(_) = v { AV::Ptr(0) } else { v }) };
        let mut through: rustc_hash::FxHashSet<Box<[u8]>> = Default::default();
        println!("frame | new | distinct new | distinct through");
        for f in 0..=a.frame {
            let (mut new, mut here) = (0u64, rustc_hash::FxHashSet::<Box<[u8]>>::default());
            for (_, rt2) in tree.rows(f, cfg.room, 1 << 20, &keep, cfg.threads())? {
                new += rt2.width as u64;
                let names = cell_names(&rt2, ids);
                let mut cells: Vec<(String, usize)> = (0..rt2.cols.len())
                    .filter(|&c| matches!(rt2.structure[c], Cell2::Val))
                    .map(|c| (names.get(&c).cloned().unwrap_or_else(|| format!("cell{c}")), c))
                    .filter(|(nm, _)| !erase.iter().any(|p| nm.starts_with(p.as_str())))
                    .collect();
                cells.sort();
                for r in 0..rt2.width {
                    let mut b: Vec<u8> = Vec::new();
                    for (nm, c) in &cells {
                        let k = code(rt2.cols[*c].at(r));
                        b.extend_from_slice(nm.as_bytes());
                        b.push(0);
                        b.push(k.kind);
                        b.extend_from_slice(&k.a.to_le_bytes());
                        b.extend_from_slice(&k.b.to_le_bytes());
                    }
                    here.insert(b.into_boxed_slice());
                }
            }
            let distinct = here.len();
            through.extend(here);
            if !through.is_empty() && (f % a.every.max(1) == 0 || f == a.frame) {
                println!("f{f:03} | {new:>9} | {distinct:>9} | {:>9}", through.len());
            }
        }
    }
    // THE POSITIONS: per player cell, its new states per frame.
    if let Some(top) = a.cells {
        let n_frames = a.frame as usize + 1;
        let mut per: rustc_hash::FxHashMap<u32, Vec<u64>> = Default::default();
        for f in 0..=a.frame {
            for s in tree.layer(f, None, cfg.threads())?.0.states_iter() {
                per.entry(s.cell(&geo)).or_insert_with(|| vec![0; n_frames])[f as usize] += 1;
            }
        }
        let total = |v: &[u64]| v.iter().sum::<u64>();
        let mut by_total: Vec<(u32, u64)> = per.iter().map(|(c, v)| (*c, total(v))).collect();
        by_total.sort_by_key(|&(c, n)| (std::cmp::Reverse(n), c));
        let shown = n_frames.min(12);
        println!("position: states through f{:03} | new per frame, f{:03}..f{:03}", a.frame, n_frames - shown, a.frame);
        for &(c, n) in by_total.iter().take(top) {
            let tail: Vec<String> = per[&c][n_frames - shown..].iter().map(|n| n.to_string()).collect();
            println!("{}: {n} | {}", where_(c), tail.join(" "));
        }
        // Every position's new states at the frame as a share of its states.
        let edges = [0.01, 0.05, 0.20];
        let (mut cells_in, mut states_in) = ([0usize; 4], [0u64; 4]);
        for v in per.values() {
            let share = v[n_frames - 1] as f64 / total(v).max(1) as f64;
            let b = edges.iter().filter(|&&e| share >= e).count();
            cells_in[b] += 1;
            states_in[b] += total(v);
        }
        println!(
            "{} positions; new at f{:03} as a share of their states: under 1%: {} positions ({} states), 1-5%: {} ({}), 5-20%: {} ({}), 20% or more: {} ({})",
            per.len(),
            a.frame,
            cells_in[0],
            states_in[0],
            cells_in[1],
            states_in[1],
            cells_in[2],
            states_in[2],
            cells_in[3],
            states_in[3]
        );
    }
    Ok(())
}

/// The kernels of a level (`compiled::Kernels::new`), for the commands
/// generic over the arithmetic.
trait KernelsNew: Sized {
    fn new(cfg: &celeste_rust::settings::Settings, level: Level) -> Result<Self>;
}

impl<N: celeste_rust::compiled::KernelLanes> KernelsNew for celeste_rust::compiled::Kernels<N> {
    fn new(cfg: &celeste_rust::settings::Settings, level: Level) -> Result<Self> {
        celeste_rust::compiled::Kernels::<N>::new(cfg, level)
    }
}

/// The run's room, "x,y" (`--room`).
fn room_text(cfg: &celeste_rust::settings::Settings) -> String {
    let (x, y) = cfg.room;
    format!("{x},{y}")
}

/// The run's `--win-at`, as given (`x,y` or `x0..x1,y0..y1`).
fn win_at_text(cfg: &celeste_rust::settings::Settings) -> Option<String> {
    let r = |a: i16, b: i16| if a == b { a.to_string() } else { format!("{a}..{b}") };
    cfg.win_at.map(|(x0, x1, y0, y1)| format!("{},{}", r(x0, x1), r(y0, y1)))
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

/// How a search's levels ended.
enum Levels<N: Rem> {
    /// No win by the horizon at level `li`'s arc phase.
    Refuted { li: usize, lvl: Level },
    /// The last level's arc phase (its bound is `Some`).
    Last { li: usize, lvl: Level, solved: celeste_rust::search::arc_dp::Solved<N> },
}

/// The LEVELS of a search in arithmetic `N`, coarsest first, under `base`:
/// each a forward (`engine` makes a level's frame step, `initial` the
/// start; a finer level filtered by the coarser one's arc marks) and its arc
/// phase, recorded in `res.levels` (their dirs named under `prefix`). The
/// last level ends as `finish` says.
#[allow(clippy::too_many_arguments)]
fn run_levels<N: Rem>(
    cfg: &celeste_rust::settings::Settings,
    steps: u32,
    levels: &[Level],
    base: &std::path::Path,
    prefix: &str,
    finish: Finish,
    save_marks: Option<&str>,
    prefer: Option<&[u8]>,
    res: &mut celeste_rust::search::result::SearchResult,
    engine: impl Fn(Level) -> Result<Box<dyn FrameStep<N>>>,
    initial: impl Fn() -> Result<Vec<Block<N>>>,
) -> Result<Levels<N>> {
    use celeste_rust::search::arc_dp::solve;
    use celeste_rust::search::result::LevelResult;
    // The previous level's arc-marked nodes: the next forward's filter.
    let mut prev: Option<(Visited<N>, Level)> = None;
    for (li, &lvl) in levels.iter().enumerate() {
        let last = li + 1 == levels.len();
        let dir = base.join(format!("level{li:02}"));
        // THE FORWARD. Level 0's tree is reused or extended
        // (`grow_tree`: one filtered by level -1 below this horizon
        // is refused, a hull tree cannot be raised). A finer
        // level's tree is filtered by the coarser marks for one
        // horizon: kept only for the same marks, horizon and level
        // -1, else built again (it is the cheap one).
        let t = std::time::Instant::now();
        let want = cfg.minus_one();
        let filter = prev.as_ref().map(|(m, l)| celeste_rust::frame::MarkFilter::new(m, *l, cfg.steps_per_frame()));
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
        let first_win = celeste_rust::frame::grow_tree(&dir, steps, want, filter.as_ref(), lvl, || engine(lvl), &initial, cfg)?.win_frame;
        let forward_s = t.elapsed().as_secs_f64();
        // `f` counts search steps.
        eprintln!("[search] level {li} ({lvl}): forward to f{steps} in {forward_s:.1} s; first win {first_win:?}");
        celeste_rust::metrics::mem_phase("forward");
        // THE ARC PHASE, and the concrete search at the LAST level only:
        // a coarser level's bound is loose wherever its objects are
        // (gemskip-nodiag 2800m h192: 379 s of a 450 s search went to a
        // fruitless try at level 0's bound f186; the optimum was 191),
        // and the finer level decides the optimum anyway.
        let concrete = last && finish == Finish::Concrete;
        let keep = last && finish == Finish::Arcs;
        // Every level saves its marks (the next overwrites).
        let known = prefer.map(|inputs| celeste_rust::search::known::Route { inputs, filter: filter.as_ref(), hulls: false });
        let s = solve::<N>(&dir, lvl, steps, concrete, keep, !last, save_marks.map(std::path::Path::new), prefer, known.as_ref(), cfg)?;
        res.levels.push(LevelResult { spec: lvl.to_string(), dir: format!("{prefix}level{li:02}"), forward_s, first_win, arc_bound: s.arc });
        let Some(bound) = s.arc else { return Ok(Levels::Refuted { li, lvl }) };
        println!("ARC BOUND: no win before f{bound} (level {li}, {lvl}: remainder-exact, the level's objects over-approximated)");
        if last {
            return Ok(Levels::Last { li, lvl, solved: s });
        }
        prev = Some((s.arc_marks.expect("asked for"), lvl));
    }
    anyhow::bail!("no level given")
}

/// A search's record (`result.json`), before it decides.
fn search_record(cfg: &celeste_rust::settings::Settings, level: &str, horizon: u32, ceiling: bool) -> celeste_rust::search::result::SearchResult {
    use celeste_rust::search::result::{SearchResult, Verdict};
    SearchResult {
        format: celeste_rust::search::result::FORMAT,
        room: cfg.room,
        level: level.to_string(),
        horizon,
        ceiling,
        steps_per_frame: cfg.steps_per_frame(),
        levels: Vec::new(),
        verdict: Verdict::BoundOnly,
        optimal: None,
        witness: None,
        witness_file: None,
        wall_s: 0.0,
        peak_gb: 0.0,
        error: None,
        dual: None,
    }
}

/// A search's horizon in frames (`--to H` or `--ceiling H`).
fn horizon_of(to: Option<u32>, ceiling: Option<u32>) -> Result<u32> {
    match (ceiling, to) {
        (Some(h), None) | (None, Some(h)) => Ok(h),
        _ => anyhow::bail!("give the horizon: --to H, or --ceiling H for a known solution"),
    }
}

/// A fresh checkpoint dir's record: the previous run's removed.
fn clear_record(base: &std::path::Path) -> Result<()> {
    std::fs::create_dir_all(base)?;
    match std::fs::remove_file(celeste_rust::search::result::path(base)) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => Err(e.into()),
        _ => Ok(()),
    }
}

/// `rewrite search` in arithmetic `N`: the levels' forwards (`engine` makes
/// a level's frame step, `initial` the start), each level's arc phase, and
/// the concrete search at the last; the result recorded beside the trees.
#[allow(clippy::too_many_arguments)]
fn search<N: celeste_rust::search::rem::Rem>(
    cfg: &celeste_rust::settings::Settings,
    to: Option<u32>,
    ceiling: Option<u32>,
    level: String,
    checkpoint_dir: String,
    no_witness: bool,
    save_marks: Option<String>,
    prefer: Option<String>,
    engine: impl Fn(Level) -> Result<Box<dyn FrameStep<N>>>,
    initial: impl Fn() -> Result<Vec<Block<N>>>,
) -> Result<()> {
    use celeste_rust::search::result::Verdict;
    let prefer: Option<Vec<u8>> = prefer.as_deref().map(celeste_rust::concrete::read_inputs).transpose()?;
    let cfg = cfg.clone();
    let horizon = horizon_of(to, ceiling)?;
    // The horizon is in FRAMES; the tree, the arcs and the marks
    // count search steps (two a frame under the split frame).
    let steps = horizon * cfg.steps_per_frame();
    let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let room = room_text(&cfg);
    eprintln!("[search] room {room}, levels {level}, horizon {horizon}");
    let t0 = std::time::Instant::now();
    let base = std::path::Path::new(&checkpoint_dir);
    // The record of THIS run: none until it decides.
    clear_record(base)?;
    let mut res = search_record(&cfg, &level, horizon, ceiling.is_some());
    let record = |res: &mut celeste_rust::search::result::SearchResult, verdict: Verdict| -> Result<()> {
        res.verdict = verdict;
        res.wall_s = t0.elapsed().as_secs_f64();
        res.peak_gb = celeste_rust::metrics::peak_rss_gb();
        res.write(base)
    };
    // The level -1 table first: a room whose table cannot be built
    // is a recorded refusal (rerun without the filter), not a panic
    // somewhere in the forward.
    if let Some(m) = cfg.minus_one() {
        if let Err(e) = m.try_table(&cfg) {
            res.error = Some(format!("{e:#}"));
            record(&mut res, Verdict::LevelMinusOneRefused)?;
            return Err(e.context("--level-minus-one: building the level -1 table"));
        }
    }
    let finish = if no_witness { Finish::Bound } else { Finish::Concrete };
    let (li, lvl, s) = match run_levels::<N>(&cfg, steps, &levels, base, "", finish, save_marks.as_deref(), prefer.as_deref(), &mut res, engine, initial)? {
        Levels::Refuted { li, lvl } => {
            let wall = t0.elapsed().as_secs_f64();
            record(&mut res, Verdict::Refuted)?;
            anyhow::ensure!(ceiling.is_none(), "ceiling {horizon} REFUTED by the arc search at level {li} ({lvl}): a known solution the model cannot reproduce");
            println!("REFUTED: no win by f{horizon} (level {li}, {lvl}; {wall:.1} s)");
            return Ok(());
        }
        Levels::Last { li, lvl, solved } => (li, lvl, solved),
    };
    let bound = s.arc.expect("the last level's bound");
    let wall = t0.elapsed().as_secs_f64();
    if let Some(w) = &s.concrete {
        let f = w.inputs.len();
        let path = base.join(format!("witness_frame_{f}.txt"));
        std::fs::write(
            &path,
            format!(
                "# Room ({room}): the concrete witness of `rewrite search --level {level} --to {horizon}`{}.\n\
                 # Arc bound f{bound} (level {li}, {lvl}); no concrete win before f{f} inside the winning sets (exhaustive).\n\
                 # A balloon room's win may hold for some `rnd` draws only: replay with seeds.\n{}\n",
                win_at_text(&cfg).map_or(String::new(), |w| format!(" --win-at {w}")),
                w.inputs_text()
            ),
        )?;
        res.optimal = Some(f as u32);
        res.witness = Some(w.inputs.clone());
        res.witness_file = Some(path.file_name().expect("a file").to_string_lossy().into_owned());
        record(&mut res, Verdict::Optimal)?;
        println!("OPTIMAL win frame: {f} ({wall:.1} s); witness {}", path.display());
        return Ok(());
    }
    if no_witness {
        return record(&mut res, Verdict::BoundOnly);
    }
    record(&mut res, Verdict::NoConcreteWin)?;
    anyhow::ensure!(ceiling.is_none(), "ceiling {horizon}: no concrete witness by it - a known solution the model cannot reproduce");
    println!("no concrete win by f{horizon} (arc bound f{bound}; {wall:.1} s)");
    Ok(())
}

/// `rewrite dual`: THE DUAL SEARCH (`search::dual`). The fixed half under
/// `cfg` (real play), the float half under `float`, each its levels to the
/// same horizon under `<dir>/fixed` and `<dir>/float`, then the dual
/// concrete search between the larger arc bound and the horizon; its
/// witness replayed in both arithmetics and recorded.
#[allow(clippy::too_many_arguments)]
fn dual(
    cfg: &celeste_rust::settings::Settings,
    float: &celeste_rust::settings::Settings,
    to: Option<u32>,
    ceiling: Option<u32>,
    level: String,
    checkpoint_dir: String,
    no_witness: bool,
    prefer: Option<String>,
    save_marks: Option<String>,
) -> Result<()> {
    use celeste_rust::search::result::{DualResult, Verdict};
    let prefer: Option<Vec<u8>> = prefer.as_deref().map(celeste_rust::concrete::read_inputs).transpose()?;
    let horizon = horizon_of(to, ceiling)?;
    let steps = horizon * cfg.steps_per_frame();
    let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let room = room_text(cfg);
    eprintln!("[dual] room {room}, levels {level}, horizon {horizon}; the fixed half real play, the float half {}", float.arith);
    let t0 = std::time::Instant::now();
    let base = std::path::Path::new(&checkpoint_dir);
    clear_record(base)?;
    let mut res = search_record(cfg, &level, horizon, ceiling.is_some());
    res.dual = Some(DualResult { float: float.arith.to_string(), float_loading_jank: float.loading_jank, fixed_exit: None });
    let record = |res: &mut celeste_rust::search::result::SearchResult, verdict: Verdict| -> Result<()> {
        res.verdict = verdict;
        res.wall_s = t0.elapsed().as_secs_f64();
        res.peak_gb = celeste_rust::metrics::peak_rss_gb();
        res.write(base)
    };
    // Both halves' level -1 tables first (each its arithmetic's).
    for s in [cfg, float] {
        if let Some(m) = s.minus_one() {
            if let Err(e) = m.try_table(s) {
                res.error = Some(format!("{}: {e:#}", s.arith));
                record(&mut res, Verdict::LevelMinusOneRefused)?;
                return Err(e.context(format!("--level-minus-one: building the {} half's level -1 table", s.arith)));
            }
        }
    }
    let refuted = |res: &mut celeste_rust::search::result::SearchResult, half: &str, li: usize, lvl: Level| -> Result<()> {
        let wall = t0.elapsed().as_secs_f64();
        record(res, Verdict::Refuted)?;
        anyhow::ensure!(ceiling.is_none(), "ceiling {horizon} REFUTED by the {half} half's arc search at level {li} ({lvl}): a known solution the model cannot reproduce");
        println!("REFUTED: no {half} win by f{horizon} (level {li}, {lvl}; {wall:.1} s), so no dual one");
        Ok(())
    };
    let fixed_half = run_levels::<P8>(
        cfg,
        steps,
        &levels,
        &base.join("fixed"),
        "fixed/",
        Finish::Arcs,
        None,
        prefer.as_deref(),
        &mut res,
        |lvl| Ok(Box::new(celeste_rust::compiled::FrameEngine::new(cfg, lvl)?)),
        || Ok(vec![Block::canonical(RefEngine::new(cfg)?.initial()?)?]),
    )?;
    let sx = match fixed_half {
        Levels::Refuted { li, lvl } => return refuted(&mut res, "fixed", li, lvl),
        Levels::Last { solved, .. } => solved,
    };
    let float_half = run_levels::<LuaF64>(
        float,
        steps,
        &levels,
        &base.join("float"),
        "float/",
        Finish::Arcs,
        save_marks.as_deref(),
        prefer.as_deref(),
        &mut res,
        |lvl| Ok(Box::new(celeste_rust::compiled::FloatKernels::new(float, lvl)?)),
        || Ok(vec![Block::canonical(FloatRefEngine::new(float)?.initial()?)?]),
    )?;
    let sf = match float_half {
        Levels::Refuted { li, lvl } => return refuted(&mut res, "float", li, lvl),
        Levels::Last { solved, .. } => solved,
    };
    let (bx, bf) = (sx.arc.expect("a bound"), sf.arc.expect("a bound"));
    let bound = bx.max(bf);
    println!("DUAL BOUND: no dual win before f{bound} (the fixed half's arc bound f{bx}, the float half's f{bf})");
    if no_witness {
        return record(&mut res, Verdict::BoundOnly);
    }
    let (ax, af) = (sx.arcs.expect("kept"), sf.arcs.expect("kept"));
    let found = celeste_rust::search::dual::search(&celeste_rust::search::dual::Half { arcs: &ax, settings: cfg }, &celeste_rust::search::dual::Half { arcs: &af, settings: float }, bound, prefer.as_deref())?;
    let wall = t0.elapsed().as_secs_f64();
    let Some(w) = found else {
        record(&mut res, Verdict::NoConcreteWin)?;
        anyhow::ensure!(ceiling.is_none(), "ceiling {horizon}: no dual witness by it - a known solution the model cannot reproduce");
        println!("no dual win by f{horizon} (dual bound f{bound}; {wall:.1} s)");
        return Ok(());
    };
    celeste_rust::search::dual::replay::<P8, LuaF64>(&w, cfg, float)?;
    let f = w.inputs.len();
    let path = base.join(format!("witness_frame_{f}.txt"));
    std::fs::write(
        &path,
        format!(
            "# Room ({room}): the DUAL witness of `rewrite dual --level {level} --to {horizon}`: {} exits on frame {f} (the count, Philippe's rule (b)), real play (16.16) on frame {}.\n\
             # Dual bound f{bound} (fixed f{bx}, float f{bf}); no dual win before f{f} inside both halves' winning sets (exhaustive).\n\
             # A balloon room's real-play exit may hold for some `rnd` draws only.\n{}\n",
            float.arith,
            w.fixed_exit,
            w.inputs_text()
        ),
    )?;
    res.optimal = Some(f as u32);
    res.witness = Some(w.inputs.clone());
    res.witness_file = Some(path.file_name().expect("a file").to_string_lossy().into_owned());
    if let Some(d) = &mut res.dual {
        d.fixed_exit = Some(w.fixed_exit);
    }
    // The UI's path: the float half's cells along the witness.
    if let Some(out) = &save_marks {
        let cells = celeste_rust::search::dual::path_cells::<LuaF64>(&w.inputs, float)?;
        let path = std::path::Path::new(out).join("witness.txt");
        celeste_rust::search::arc_dp::Witness { inputs: w.inputs.clone(), cells }.save(&path)?;
        let label = format!("label dual witness, valid in both arithmetics: {} exits on frame {f}, real play on frame {} (the float half's path)\n", float.arith, w.fixed_exit);
        std::fs::write(&path, label + &std::fs::read_to_string(&path)?)?;
    }
    record(&mut res, Verdict::Optimal)?;
    println!("[gate] h{horizon} dual optimum {f} fixed exit {} inputs {}", w.fixed_exit, w.inputs_text());
    println!("OPTIMAL dual win frame: {f}, real play leaving on frame {} ({wall:.1} s); witness {}", w.fixed_exit, path.display());
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

/// `check-known` (`Command::CheckKnown`) in the arithmetic of `N`.
fn check_known<N: Rem>(checkpoint_dir: &str, level: &str, inputs: &str, horizon: u32, cfg: &celeste_rust::settings::Settings) -> Result<()> {
    use celeste_rust::search::arc_dp::solve;
    let inputs = celeste_rust::concrete::read_inputs(inputs)?;
    let steps = horizon * cfg.steps_per_frame();
    let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
    let t0 = std::time::Instant::now();
    let mut prev: Option<(Visited<N>, Level)> = None;
    for (li, &lvl) in levels.iter().enumerate() {
        let last = li + 1 == levels.len();
        let dir = std::path::Path::new(checkpoint_dir).join(format!("level{li:02}"));
        anyhow::ensure!(
            celeste_rust::frame::tree_first_win_through::<N>(&dir, steps)?.is_some(),
            "{}: no tree through step {steps} (run the search first)",
            dir.display()
        );
        let filter = prev.as_ref().map(|(m, l)| celeste_rust::frame::MarkFilter::new(m, *l, cfg.steps_per_frame()));
        let route = celeste_rust::search::known::Route { inputs: &inputs, filter: filter.as_ref(), hulls: true };
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

/// `l1-check` (`Command::L1Check`): a known solution against the level -1
/// table, in the arithmetic `A` (its table, its reference engine).
fn l1_check<A: Arithmetic>(inputs: &str, horizon: u32, spd: i32, cfg: &celeste_rust::settings::Settings) -> Result<()>
where
    A::Num: celeste_core::arith::EngineNum<A = A>,
{
    let bytes = celeste_rust::concrete::read_inputs(inputs)?;
    let threads = cfg.threads();
    let owned = cfg.clone();
    let table = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || celeste_rust::trace::level_minus_one::cost_to_go(std::path::Path::new("."), spd, threads, &owned))?
        .join()
        .map_err(|_| anyhow::anyhow!("the level -1 builder panicked"))??;
    println!("[l1-check] table: {} entries, start d {}, fingerprint {:016x}", table.len(), table.start_d, table.fingerprint());
    // The known run: under the concrete run's seeds (a float tool's).
    let mut eng = celeste_rust::trace::refengine::ReferenceEngine::<A>::new(&cfg.concrete().0)?;
    // Each leaf with its lineage's states too late (an `rnd` leaf
    // that never exits may rightly be too late: only an exiting
    // lineage is the solution).
    let mut states: Vec<(Rt2<A::Num>, Vec<u32>)> = vec![(eng.initial()?, Vec::new())];
    let (mut checked, mut unknown) = (0usize, 0usize);
    let mut exited: Option<Vec<Vec<u32>>> = None;
    for f in 0..=bytes.len() as u32 {
        let mut line = Vec::new();
        for (s, late) in &mut states {
            let shape = s.shape_hash_of();
            for cell in celeste_rust::search::pos_graph::block_cells(s)? {
                let at = celeste_rust::search::pos_graph::cell_xy(cell);
                match table.d_of(shape, cell) {
                    None => {
                        unknown += 1;
                        line.push(format!("{at:?} not a table node"));
                    }
                    Some(d) => {
                        checked += 1;
                        let bad = table.too_late(shape, cell, f, horizon);
                        if bad {
                            late.push(f);
                        }
                        line.push(format!("{at:?} d {}{}", if d == u32::MAX { "inf".to_string() } else { d.to_string() }, if bad { " too late" } else { "" }));
                    }
                }
            }
        }
        println!("[l1-check] f{f}: {}", line.join(", "));
        let Some(&byte) = bytes.get(f as usize) else { break };
        let mut next = Vec::new();
        let mut wins = Vec::new();
        for (s, late) in &states {
            for b in eng.step(s, byte)? {
                if wins_of(b.rt2(), cfg)?.iter().any(|&x| x) {
                    wins.push(late.clone());
                } else {
                    next.push((b.into_rt2(), late.clone()));
                }
            }
        }
        if !wins.is_empty() {
            println!("[l1-check] exit during frame {} ({} of {} leaves)", f + 1, wins.len(), wins.len() + next.len());
            exited = Some(wins);
            break;
        }
        states = next;
    }
    let wins = exited.ok_or_else(|| anyhow::anyhow!("the inputs never exit the room"))?;
    let late: usize = wins.iter().map(|l| l.len()).sum();
    println!("[l1-check] {checked} states checked, {unknown} not table nodes; on the exiting lineages {late} TOO LATE for horizon {horizon} (frames {:?})", wins.iter().flatten().collect::<std::collections::BTreeSet<_>>());
    anyhow::ensure!(late == 0, "the level -1 table calls {late} states of a known solution too late: UNSOUND");
    Ok(())
}

/// `widen-check` (`Command::WidenCheck`) in the arithmetic of `N`, its kernels `K`.
fn widen_check<K, N>(cfg: &celeste_rust::settings::Settings, level_dir: &str, frame: u32, level: Level, emitted: usize) -> Result<()>
where
    K: FrameStep<N> + KernelsNew,
    N: Rem,
{
    anyhow::ensure!(
        !(level.floors_near && frame % cfg.steps_per_frame() != 0),
        "step {frame} is mid split frame at a near level: the graph side keeps a floor's computed `collideable` there, the block side does not"
    );
    let dir = std::path::Path::new(level_dir);
    cfg.check(dir, level)?;
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
                println!("[widen-check] {what} row {l} of shape {:#x} MOVED: {}", a.shape_hash, diff.join(", "));
            }
        }
        moved
    };
    let (mut stored, mut emitted_rows, mut moved) = (0usize, 0usize, 0usize);
    let tree = Tree::<N>::open(dir, frame)?;
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
    println!("[widen-check] step {frame} at {level}: {stored} stored rows, {emitted_rows} emitted from {emitted}; {moved} moved by the block side's widening");
    anyhow::ensure!(moved == 0, "widen-check: {moved} rows the block side's widening changes (the table's two sides disagree)");
    Ok(())
}

/// `bench-frame` (`Command::BenchFrame`) in the arithmetic of `N`, its
/// kernels `K` (`--metrics`, the kernel mix census, is 16.16's).
#[allow(clippy::too_many_arguments)]
fn bench_frame<K, N>(cfg: &celeste_rust::settings::Settings, level_dir: &str, frame: Option<u32>, reps: usize, level: Level, edges: bool, metrics: Option<String>, checkpoint: bool) -> Result<()>
where
    K: FrameStep<N> + KernelsNew,
    N: Rem,
{
    use celeste_rust::compiled::dispatch::{print_stats, take_kernel_stats};
    anyhow::ensure!(!checkpoint || edges, "--checkpoint needs --edges (the frame's files as the forward writes them)");
    let dir = std::path::PathBuf::from(&level_dir);
    let frame = match frame {
        Some(f) => f,
        None => celeste_rust::storage::edges::done_frame(&dir.join("edges")).ok_or_else(|| anyhow::anyhow!("{level_dir}: no finished frame"))?,
    };
    let engine = K::new(cfg, level)?;
    let t = std::time::Instant::now();
    engine.warm();
    let t_kernels = t.elapsed().as_secs_f64();
    eprintln!("[bench] kernels built: rss {:.2} GB", celeste_rust::metrics::current_rss_gb());
    let t = std::time::Instant::now();
    let mut st = celeste_rust::frame::ForwardState::<N>::at(&dir, frame, true, level, cfg)?;
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
    let scratch = dir.join("bench-edges");
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    let mut lines: Option<String> = None;
    let mut costs: Vec<celeste_rust::storage::bench::Rep> = Vec::new();
    let counters = celeste_rust::storage::bench::Counters::open().map(celeste_rust::storage::bench::count_steps);
    for rep in 0..reps {
        let _ = std::fs::remove_dir_all(&scratch);
        let load = celeste_rust::storage::bench::load_average();
        let at = counters.as_ref().map(|c| c.start());
        let t = std::time::Instant::now();
        // The last rep runs on the state's own visited set (no copy beside it).
        let (wave, visited, xfers) = st.bench_wave(&engine, &dir, edges.then_some(scratch.as_path()), rep + 1 < reps)?;
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
            let (n, acc) = celeste_rust::storage::bench::edge_fingerprint(&scratch, frame + 1, &xfers, &visited, cfg.threads())?;
            out += &format!("e{:03} {n} {acc:016x}\n", frame + 1);
        }
        if checkpoint {
            let (files, bytes, acc) = celeste_rust::storage::bench::dir_fingerprint(&scratch)?;
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
            if let Some(path) = &metrics {
                let kernels = celeste_rust::compiled::mix::kernel_metrics();
                let slices: u64 = kernels.iter().map(|k| k.1).sum();
                let insts: u64 = kernels.iter().map(|(_, n, c)| n * c.get("insts").copied().unwrap_or(0)).sum();
                eprintln!("[bench] kernels: {insts} dynamic instructions over {slices} slices ({} a slice; call-outs' callees aside)", insts.checked_div(slices).unwrap_or(0));
                let text = frame_metrics(&room_text(cfg), level, frame, w, &ks);
                std::fs::write(path, text).with_context(|| path.clone())?;
            }
            celeste_rust::compiled::mix::reset_slices();
        }
        lines = Some(out);
    }
    let _ = std::fs::remove_dir_all(&scratch);
    let ks = take_kernel_stats();
    anyhow::ensure!(ks.missed == 0, "KERNEL COVERAGE GAP: {} lanes missed", ks.missed);
    celeste_rust::storage::bench::print_cost(frame + 1, &costs);
    Ok(())
}

/// `bench-backward` (`Command::BenchBackward`) in the arithmetic of `N`.
fn bench_backward<N: Rem>(cfg: &celeste_rust::settings::Settings, level_dir: &str, horizon: u32, frame: u32, reps: usize) -> Result<()> {
    anyhow::ensure!(frame < horizon, "--frame {frame}: below the horizon {horizon}");
    let dir = std::path::Path::new(level_dir);
    let t = std::time::Instant::now();
    let seeds = celeste_rust::search::arc_dp::Seeds::<N>::load(dir, horizon)?;
    let content = seeds.space.content_hash();
    eprintln!("[bench-backward] h{horizon}'s edge files opened in {:.2} s: {} win rows, {} edges; {} threads", t.elapsed().as_secs_f64(), seeds.wins.len(), seeds.store.edges, cfg.threads());
    let mut first: Option<String> = None;
    for rep in 0..reps {
        // Each rep pushes W down to F+1 afresh (untimed but the
        // first), then times the step to W_F.
        let t = std::time::Instant::now();
        let mut c = celeste_rust::search::arc_dp::Backward::new(&seeds.store, &seeds.pairs, &seeds.wins, horizon, cfg.threads());
        for f in (frame + 1..horizon).rev() {
            c.step(f)?;
        }
        if rep == 0 {
            eprintln!("[bench-backward] W from f{horizon} down to f{} in {:.2} s", frame + 1, t.elapsed().as_secs_f64());
        }
        let before = c.walls();
        let t = std::time::Instant::now();
        c.step(frame)?;
        let wall = t.elapsed();
        let after = c.walls();
        // The gate's W line: per node with a set, its (shape, key,
        // cell) and its set's rectangles, summed.
        let (mut n, mut acc) = (0usize, 0u64);
        for (id, r) in c.current() {
            let (shape, key, cell) = seeds.state(id);
            n += 1;
            acc = acc.wrapping_add(celeste_rust::search::arc_dp::gate_w_hash(content.state(shape, key, cell), r));
        }
        let ms = |k: usize| (after[k] - before[k]).as_secs_f64() * 1e3;
        eprintln!("[bench-backward] rep {rep}: W f{frame} in {:.1} ms (read {:.1}, pull {:.1}, merge {:.1} ms)", wall.as_secs_f64() * 1e3, ms(0), ms(1), ms(2));
        let line = format!("[gate] h{horizon} W f{frame:03} {n} {acc:016x}");
        match &first {
            None => println!("{line}"),
            Some(l) => anyhow::ensure!(*l == line, "rep {rep} differs from rep 0: {line} against {l}"),
        }
        first = Some(line);
    }
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    // `bench-frame` times the workers' phases; `--metrics` counts the
    // kernels' census and slices (read when the kernels are built).
    // Only these run in float so far.
    let float = matches!(
        cli.command,
        Command::Search { .. }
            | Command::ArcCheck { .. }
            | Command::Forward { .. }
            | Command::Ckhash { .. }
            | Command::HullReplay { .. }
            | Command::RefCheck { .. }
            | Command::WidenCheck { .. }
            | Command::CheckKnown { .. }
            | Command::L1Check { .. }
            | Command::Robust { .. }
            | Command::ExportUi { .. }
            | Command::BenchFrame { .. }
            | Command::BenchBackward { .. }
    );
    let cfg = celeste_rust::settings::startup(&cli.settings, float, |s| {
        if let Command::BenchFrame { metrics, .. } = &cli.command {
            s.diag.phases = true;
            s.diag.kernel_metrics = metrics.is_some();
        }
    })?;

    match cli.command {
        Command::Search { to, ceiling, level, checkpoint_dir, no_witness, save_marks, prefer } => match cfg.arith {
            Arith::Fixed => search::<P8>(
                &cfg,
                to,
                ceiling,
                level,
                checkpoint_dir,
                no_witness,
                save_marks,
                prefer,
                |lvl| Ok(Box::new(celeste_rust::compiled::FrameEngine::new(&cfg, lvl)?)),
                || Ok(vec![Block::canonical(RefEngine::new(&cfg)?.initial()?)?]),
            )?,
            Arith::Float(_) => search::<LuaF64>(
                &cfg,
                to,
                ceiling,
                level,
                checkpoint_dir,
                no_witness,
                save_marks,
                prefer,
                |lvl| Ok(Box::new(celeste_rust::compiled::FloatKernels::new(&cfg, lvl)?)),
                || Ok(vec![Block::canonical(FloatRefEngine::new(&cfg)?.initial()?)?]),
            )?,
        },
        Command::Dual { to, ceiling, level, checkpoint_dir, no_witness, prefer, float_half, float_loading_jank, save_marks } => {
            let tool = match float_half.as_str() {
                "celia" => celeste_rust::settings::FloatTool::Celia,
                "uct" => celeste_rust::settings::FloatTool::Uct,
                t => anyhow::bail!("--float-half: `celia` or `uct`, not {t:?}"),
            };
            let float = cfg.float_half(tool, float_loading_jank)?;
            dual(&cfg, &float, to, ceiling, level, checkpoint_dir, no_witness, prefer, save_marks)?;
        }
        Command::Robust { inputs, random } => {
            anyhow::ensure!(matches!(cfg.arith, Arith::Float(_)), "rewrite robust: a float witness (--arith float)");
            let bytes = celeste_rust::concrete::read_inputs(&inputs)?;
            let rows = celeste_rust::search::robust::report(&bytes, &cfg, random)?;
            let base = rows[0].1;
            let fragile: Vec<&str> = rows.iter().filter(|r| r.1 != base).map(|r| r.0.as_str()).collect();
            for (name, w) in &rows {
                println!("[robust] {name}: first win {w:?}{}", if *w == base { "" } else { "  <- DIFFERS" });
            }
            if fragile.is_empty() {
                println!("[robust] ROBUST: every variant wins on glibc's frame ({} variants)", rows.len() - 1);
            } else {
                println!("[robust] FRAGILE: {} of {} variants differ ({})", fragile.len(), rows.len() - 1, fragile.join(", "));
            }
        }
        Command::CheckKnown { checkpoint_dir, level, inputs, horizon } => match cfg.arith {
            Arith::Fixed => check_known::<P8>(&checkpoint_dir, &level, &inputs, horizon, &cfg)?,
            Arith::Float(_) => check_known::<LuaF64>(&checkpoint_dir, &level, &inputs, horizon, &cfg)?,
        },
        Command::Forward {
            to,
            level,
            checkpoint_dir,
            reference,
        } => {
            let t = std::time::Instant::now();
            let dir = std::path::Path::new(&checkpoint_dir);
            match cfg.arith {
                Arith::Fixed => {
                    let engine: Box<dyn FrameStep> = if reference {
                        Box::new(celeste_rust::trace::refengine::RefFrameStep::<P8>::new(&cfg, level)?)
                    } else {
                        Box::new(celeste_rust::compiled::FrameEngine::new(&cfg, level)?)
                    };
                    eprintln!("[fwd] engine up in {:.2} s ({level}{})", t.elapsed().as_secs_f64(), if reference { ", REFERENCE engine" } else { "" });
                    forward(dir, to, level, &cfg, engine, || Ok(vec![Block::canonical(RefEngine::new(&cfg)?.initial()?)?]))?;
                }
                Arith::Float(_) => {
                    let engine: Box<dyn FrameStep<LuaF64>> = if reference {
                        Box::new(celeste_rust::trace::refengine::RefFrameStep::<LuaF64>::new(&cfg, level)?)
                    } else {
                        Box::new(celeste_rust::compiled::FloatKernels::new(&cfg, level)?)
                    };
                    eprintln!("[fwd] engine up in {:.2} s ({level}{}, {})", t.elapsed().as_secs_f64(), if reference { ", REFERENCE engine" } else { "" }, cfg.arith);
                    forward(dir, to, level, &cfg, engine, || Ok(vec![Block::canonical(FloatRefEngine::new(&cfg)?.initial()?)?]))?;
                }
            }
        }
        Command::HullReplay { checkpoint_dir, to } => {
            let dir = std::path::Path::new(&checkpoint_dir);
            let t = std::time::Instant::now();
            let frames = match cfg.arith {
                Arith::Fixed => celeste_rust::search::hull_replay::replay::<P8>(dir, to, P8::start_hull(&cfg)?, cfg.threads())?,
                Arith::Float(_) => celeste_rust::search::hull_replay::replay::<LuaF64>(dir, to, LuaF64::start_hull(&cfg)?, cfg.threads())?,
            };
            println!("frame states edges");
            for (f, x) in frames.iter().enumerate() {
                println!("f{f:03} {} {}", x.states, x.edges);
            }
            println!("[hull-replay] {}: the replay reaches the tree's states at every frame to f{to} ({:.1} s)", dir.display(), t.elapsed().as_secs_f64());
        }
        Command::Ckhash {
            checkpoint_dir,
            to,
            cells: top_cells,
            edges,
            hulls,
            dump_edges,
        } => {
            let dir = std::path::Path::new(&checkpoint_dir);
            let lines = match cfg.arith {
                Arith::Fixed => celeste_rust::storage::fingerprint::<P8>(dir, to, edges, hulls, cfg.threads())?,
                Arith::Float(_) => {
                    anyhow::ensure!(dump_edges.is_none() && top_cells.is_none(), "--dump-edges, --cells: fixed trees only so far");
                    celeste_rust::storage::fingerprint::<LuaF64>(dir, to, edges, hulls, cfg.threads())?
                }
            };
            for line in lines {
                println!("{line}");
            }
            let geo = celeste_rust::storage::GEOMETRY;
            if let Some(frame) = dump_edges {
                let tree = Tree::<P8>::open(dir, to)?;
                let content = tree.space.content_hash();
                let eg = celeste_rust::storage::edges::EdgeStore::<P8>::open(&dir.join("edges"), to)?;
                let node = |s: Sk| content.state(tree.shapes[s.shape(&geo) as usize], s.key, s.cell(&geo));
                for e in eg.edges_at(frame, cfg.threads())? {
                    eprintln!("edge {:016x} {:016x} {:?}", node(e.dst), node(e.src), eg.pair(e.xfer));
                }
            }
            if let Some(top) = top_cells {
                let tree = Tree::<P8>::open(dir, to)?;
                let mut by_cell: rustc_hash::FxHashMap<u32, usize> = Default::default();
                for s in tree.layer(to, None, cfg.threads())?.0.states_iter() {
                    *by_cell.entry(s.cell(&geo)).or_default() += 1;
                }
                let mut v: Vec<(u32, usize)> = by_cell.into_iter().collect();
                v.sort_by_key(|&(c, n)| (std::cmp::Reverse(n), c));
                for (c, n) in v.into_iter().take(top) {
                    match celeste_rust::search::pos_graph::cell_xy(c) {
                        Some((x, y)) => println!("cell {c}: player ({x},{y}) x{n}"),
                        None => println!("cell {c}: no player x{n}"),
                    }
                }
            }
        }
        Command::BenchFrame { level_dir, frame, reps, level, edges, metrics, checkpoint } => match cfg.arith {
            Arith::Fixed => bench_frame::<celeste_rust::compiled::FrameEngine, P8>(&cfg, &level_dir, frame, reps, level, edges, metrics, checkpoint)?,
            Arith::Float(_) => {
                anyhow::ensure!(metrics.is_none(), "--arith float: bench-frame --metrics (the kernel mix census) is 16.16's");
                bench_frame::<celeste_rust::compiled::FloatKernels, LuaF64>(&cfg, &level_dir, frame, reps, level, edges, None, checkpoint)?
            }
        },
        Command::BenchBackward { level_dir, horizon, frame, reps } => match cfg.arith {
            Arith::Fixed => bench_backward::<P8>(&cfg, &level_dir, horizon, frame, reps)?,
            Arith::Float(_) => bench_backward::<LuaF64>(&cfg, &level_dir, horizon, frame, reps)?,
        },
        Command::BenchStorage { capture, tree, level, reps } => {
            cfg.check(std::path::Path::new(&tree), level)?;
            celeste_rust::storage::bench::bench_storage(&celeste_rust::storage::bench::BenchArgs {
                capture: std::path::Path::new(&capture),
                tree: std::path::Path::new(&tree),
                level,
                settings: &cfg,
                reps,
            })?;
        }
        Command::ArcCheck { level_dir, from, to, samples, level, fault, path_cap } => {
            use celeste_rust::search::arc_check::{run, Opts};
            let o = Opts { dir: std::path::Path::new(&level_dir), from, to, samples, level, fault: fault.as_deref(), path_cap, settings: &cfg };
            match cfg.arith {
                Arith::Fixed => run::<P8>(&o)?,
                Arith::Float(_) => run::<LuaF64>(&o)?,
            }
        }
        Command::WidenCheck { level_dir, frame, level, emitted } => match cfg.arith {
            Arith::Fixed => widen_check::<celeste_rust::compiled::FrameEngine, P8>(&cfg, &level_dir, frame, level, emitted)?,
            Arith::Float(_) => widen_check::<celeste_rust::compiled::FloatKernels, LuaF64>(&cfg, &level_dir, frame, level, emitted)?,
        },
        Command::RefCheck { level_dir, frame, level, samples, cell, show, path_cap, full } => {
            let o = RefCheckArgs { level_dir, frame, level, samples, cell, show, path_cap, full };
            match cfg.arith {
                Arith::Fixed => ref_check::<celeste_rust::compiled::FrameEngine, celeste_core::arith::Fixed>(&cfg, o)?,
                Arith::Float(_) => ref_check::<celeste_rust::compiled::FloatKernels, celeste_core::arith::Float>(&cfg, o)?,
            }
        }
        Command::RerunRow { level_dir, row, level, erase } => {
            let ids = celeste_rust::compiled::ids();
            let dir = std::path::Path::new(&level_dir);
            let erase = prefixes(&erase);
            let parts: Vec<u64> = row.split(':').map(|s| s.trim().parse()).collect::<Result<_, _>>()?;
            let [layer, r] = parts[..] else { anyhow::bail!("--row L:N") };
            let layer = layer as u32;
            cfg.check(dir, level)?;
            let engine = celeste_rust::compiled::FrameEngine::new(&cfg, level)?;
            let tree = Tree::<P8>::open(dir, layer)?;
            let (states, _) = tree.layer(layer, None, cfg.threads())?;
            let sk = states.states_iter().nth(r as usize).ok_or_else(|| anyhow::anyhow!("state {r} past frame {layer}'s {}", states.states()))?;
            let rt2 = tree.row(layer, sk, cfg.room)?;
            let me = project_row(&rt2, &cell_names(&rt2, ids), 0, &erase);
            println!("[rerun] l{layer} n{r} (state {}) at {}:", sk.show(), where_(sk.cell(&celeste_rust::storage::GEOMETRY)));
            for (n, v) in &me {
                println!("    {n} = {v}");
            }
            let (next, won) = rerun(&engine, rt2, layer)?;
            let mut n = 0;
            for b in &next {
                let cells = celeste_rust::search::pos_graph::block_cells(b.rt2())?;
                let names = cell_names(b.rt2(), ids);
                for i in 0..b.rt2().width {
                    let p = project_row(b.rt2(), &names, i as u32, &erase);
                    let changed: Vec<String> = p.iter().filter(|(k, v)| me.get(*k) != Some(*v)).map(|(k, v)| format!("{k}={v}")).collect();
                    println!("  -> {} {}", where_(cells[i]), changed.join(" "));
                    n += 1;
                }
            }
            println!("[rerun] {n} successors, win {won}");
        }
        Command::Spurious { coarse, real, step, cell, erase, samples, depth, chain_out } => {
            use std::collections::{BTreeMap, HashMap};
            type Dist = BTreeMap<String, BTreeMap<String, u64>>;
            let ids = celeste_rust::compiled::ids();
            let coarse = std::path::Path::new(&coarse);
            let real = real.as_deref().map(std::path::Path::new);
            anyhow::ensure!(real.is_none() || chain_out.is_none(), "--chain-out writes a whole chain: not with --real");
            let coarse_tree = tree_of(coarse)?;
            let real_tree = real.map(tree_of).transpose()?;
            let erase = prefixes(&erase);
            let v: Vec<i32> = cell.split(',').map(|t| t.trim().parse()).collect::<std::result::Result<_, _>>()?;
            let (x0, x1, y0, y1) = match v[..] {
                [x, y] => (x, x, y, y),
                [x0, x1, y0, y1] => (x0, x1, y0, y1),
                _ => anyhow::bail!("--cell x,y or x0,x1,y0,y1"),
            };
            let wants: Vec<u32> = (x0..=x1).flat_map(|x| (y0..=y1).map(move |y| (x, y))).map(cell_at).collect::<Result<_>>()?;
            let project = |rt2: &Rt2, r: u32| project_row(rt2, &cell_names(rt2, ids), r, &erase);
            let tally = |d: &mut Dist, p: &Proj| {
                for (n, v) in p {
                    *d.entry(n.clone()).or_default().entry(v.clone()).or_default() += 1;
                }
            };
            // The real tree's projections per cell with their first step.
            // Projections compared EXACTLY (the projection itself as the key).
            let mut real_cells: HashMap<u32, HashMap<Proj, u32>> = Default::default();
            let mut real_vals = Dist::new();
            let geo = celeste_rust::storage::GEOMETRY;
            let mut real_of = |c: u32, vals: Option<&mut Dist>| -> Result<HashMap<Proj, u32>> {
                if let Some(m) = real_cells.get(&c) {
                    return Ok(m.clone());
                }
                let (Some(real_tree), mut vals) = (real_tree.as_ref(), vals) else { return Ok(HashMap::new()) };
                let mut m: HashMap<Proj, u32> = Default::default();
                for s in 0..=step {
                    for (_, rt2) in real_tree.rows(s, cfg.room, 1 << 20, &|x| x.cell(&geo) == c, 1)? {
                        for r in 0..rt2.width {
                            let p = project(&rt2, r as u32);
                            if let (Some(vals), true) = (vals.as_deref_mut(), s == step) {
                                tally(vals, &p);
                            }
                            m.entry(p).or_insert(s);
                        }
                    }
                }
                real_cells.insert(c, m.clone());
                Ok(m)
            };
            // The samples: the coarse tree's new states at `step` in the cells.
            let (mut total, mut sp_vals, mut co_vals) = (0u64, Dist::new(), Dist::new());
            let mut chosen: Vec<Sk> = Vec::new();
            for &want in &wants {
                let here_real = real_of(want, Some(&mut real_vals))?;
                for (sks, rt2) in coarse_tree.rows(step, cfg.room, 1 << 20, &|x| x.cell(&geo) == want, 1)? {
                    for r in 0..rt2.width {
                        total += 1;
                        let p = project(&rt2, r as u32);
                        let is_real = real.is_some() && here_real.get(&p).is_some_and(|&s| s <= step);
                        tally(if is_real { &mut co_vals } else { &mut sp_vals }, &p);
                        if !is_real {
                            chosen.push(sks[r]);
                        }
                    }
                }
            }
            if real.is_none() {
                println!("[spurious] {total} states first reached at f{step:03} in x {x0}..={x1}, y {y0}..={y1}");
            } else {
                println!("[spurious] x {x0}..={x1}, y {y0}..={y1}, step {step}: coarse {total} new states, {} spurious (projection never reached by the real tree)", chosen.len());
                // Per-field distributions: real, coarse-and-real, spurious.
                let fmt_dist = |m: Option<&BTreeMap<String, u64>>| -> String {
                    let Some(m) = m else { return "-".into() };
                    let mut v: Vec<(&String, &u64)> = m.iter().collect();
                    v.sort_by(|a, b| b.1.cmp(a.1));
                    let top: Vec<String> = v.iter().take(8).map(|(k, c)| format!("{k}:{c}")).collect();
                    format!("{} values; {}", v.len(), top.join(" "))
                };
                let mut fields: Vec<&String> = sp_vals.keys().chain(real_vals.keys()).collect();
                fields.sort();
                fields.dedup();
                for nm in fields {
                    let distinct = |m: &Dist| m.get(nm).map_or(0, |x| x.len());
                    if distinct(&sp_vals).max(distinct(&real_vals)).max(distinct(&co_vals)) <= 1 {
                        continue;
                    }
                    println!("  {nm}\n    real     {}\n    coarse-ok {}\n    spurious {}", fmt_dist(real_vals.get(nm)), fmt_dist(co_vals.get(nm)), fmt_dist(sp_vals.get(nm)));
                }
            }
            // Walk the samples back.
            let edges = celeste_rust::storage::edges::EdgeStore::<P8>::open(&coarse.join("edges"), step)?;
            let n = samples.min(chosen.len());
            for k in 0..n {
                let (mut id, mut layer) = (chosen[k * chosen.len() / n], step);
                let rt2 = coarse_tree.row(layer, id, cfg.room)?;
                let (shape, _, c) = coarse_tree.state(id)?;
                let mut cur = project(&rt2, 0);
                println!("\n[spurious] sample {k}: f{layer} {} shape {shape:016x} {}", id.show(), where_(c));
                if real.is_none() {
                    for (name, v) in &cur {
                        println!("    {name} = {v}");
                    }
                }
                // (frame, cell, key) along the chain, for `--chain-out`.
                let mut chain: Vec<(u32, u32, (u64, u64))> = vec![(layer, c, rt2.row_keys[0])];
                let mut found = false;
                for _ in 0..depth {
                    if layer == 0 {
                        break;
                    }
                    let preds = preds_of(&edges, id, layer)?;
                    // Prefer a REAL predecessor: then this step is the first
                    // spurious one.
                    let mut pick: Option<(Sk, Rt2, Proj, u32, bool)> = None;
                    for &p in &preds {
                        let rt = coarse_tree.row(layer - 1, p, cfg.room)?;
                        let pc = p.cell(&geo);
                        let pp = project(&rt, 0);
                        let is_real = real.is_some() && real_of(pc, None)?.get(&pp).is_some_and(|&s| s < layer);
                        if is_real || pick.is_none() {
                            pick = Some((p, rt, pp, pc, is_real));
                        }
                        if is_real || real.is_none() {
                            break;
                        }
                    }
                    let Some((p, rt, pp, pc, is_real)) = pick else {
                        println!("  <- no recorded predecessor at f{layer:03}");
                        break;
                    };
                    let changed: Vec<String> = cur
                        .iter()
                        .filter(|(nm, v)| pp.get(*nm) != Some(*v))
                        .map(|(nm, v)| format!("{nm}: {} -> {v}", pp.get(nm).map(|s| s.as_str()).unwrap_or("absent")))
                        .collect();
                    if is_real {
                        println!("  FIRST SPURIOUS STEP f{} {} -> f{layer}: {} ({} preds): {}", layer - 1, p.show(), where_(pc), preds.len(), changed.join(", "));
                        // The whole real predecessor, unprojected: the floors too.
                        let full = project_row(&rt, &cell_names(&rt, ids), 0, &[]);
                        println!("    predecessor (full): {}", full.iter().filter(|(n, _)| !n.contains("hitbox") && !n.contains(".type.") && !n.ends_with(".solids") && !n.contains(".flip.y") && !n.ends_with(".spr") || n.starts_with("spring") || n.starts_with("balloon")).map(|(n, v)| format!("{n}={v}")).collect::<Vec<_>>().join(" "));
                        found = true;
                        break;
                    }
                    println!("  <- f{:03} {} [{}] ({} preds){}: {}", layer - 1, where_(pc), p.show(), preds.len(), if real.is_some() { " still spurious" } else { "" }, changed.join(", "));
                    chain.push((layer - 1, pc, rt.row_keys[0]));
                    id = p;
                    layer -= 1;
                    cur = pp;
                }
                if real.is_some() && !found {
                    println!("  (no real predecessor within {depth} steps)");
                }
                if let (0, Some(path)) = (k, &chain_out) {
                    chain.sort_by_key(|(f, ..)| *f);
                    anyhow::ensure!(chain.first().map(|(f, ..)| *f) == Some(0), "--chain-out needs the chain back to frame 0: raise --depth");
                    let mut out = String::new();
                    for (_, c, key) in chain.iter().skip(1) {
                        match celeste_rust::search::pos_graph::cell_xy(*c) {
                            Some((x, y)) => out.push_str(&format!("{x},{y} {:016x} {:016x}\n", key.0, key.1)),
                            None => out.push_str("-\n"),
                        }
                    }
                    std::fs::write(path, out)?;
                    println!("[spurious] wrote the chain (frames 1..={step}) to {path}");
                }
            }
        }
        Command::Census { level_dir, frame, cap, cell, erase, every, cells } => census(&cfg, CensusArgs { level_dir, frame, cap, cell, erase, every, cells })?,
        Command::ExportUi { checkpoint_dir, out, arc, witness, reference, download } => {
            celeste_rust::search::ui_export::export(
                std::path::Path::new(&checkpoint_dir),
                std::path::Path::new(&out),
                &cfg,
                arc.as_deref().map(std::path::Path::new),
                celeste_rust::search::ui_export::Paths {
                    witness: witness.as_deref().map(std::path::Path::new),
                    reference: reference.as_deref().map(std::path::Path::new),
                    download: download.as_deref().map(std::path::Path::new),
                },
            )?;
        }
        Command::Room => {
            let cart = celeste_core::cart_data::CartData::load("cart")?;
            let objects = celeste_rust::trace::cart::room_objects(&cart, cfg.room);
            let level = celeste_rust::game_runner::level_index(cfg.room.0, cfg.room.1) + 1;
            println!("{}", serde_json::json!({ "room": cfg.room, "level": level, "objects": objects }));
        }
        Command::Starts => {
            println!("{}", serde_json::to_string(&celeste_rust::starts::starts(&cfg)?)?);
        }
        Command::Trajectory {
            trajectory,
            stop_at_win,
            spec,
            tree,
        } => {
            let lines: Vec<(Option<(i32, i32)>, Option<(u64, u64)>)> = std::fs::read_to_string(&trajectory)?
                .lines()
                .map(|l| l.trim())
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .map(|l| {
                    let mut it = l.split_whitespace();
                    let p = it.next().expect("a position");
                    let pos = if p == "-" {
                        None
                    } else {
                        let (x, y) = p.split_once(',').expect("x,y");
                        Some((x.trim().parse().expect("x"), y.trim().parse().expect("y")))
                    };
                    let key = match (it.next(), it.next()) {
                        (Some(a), Some(b)) => Some((u64::from_str_radix(a, 16).expect("key hex"), u64::from_str_radix(b, 16).expect("key hex"))),
                        _ => None,
                    };
                    (pos, key)
                })
                .collect();
            let traj: Vec<Option<(i32, i32)>> = lines.iter().map(|(p, _)| *p).collect();
            let keyed: Vec<Option<(u64, u64)>> = lines.iter().map(|(_, k)| *k).collect();
            let level = spec;
            anyhow::ensure!(level.is_some() || keyed.iter().all(|k| k.is_none()), "keyed trajectory lines need --spec");
            let space = match &tree {
                Some(t) => tree_of(std::path::Path::new(t))?.space,
                None => {
                    anyhow::ensure!(keyed.iter().all(|k| k.is_none()), "keyed trajectory lines need --tree (the tree their keys are in)");
                    Default::default()
                }
            };
            let mut eng = RefEngine::new(&cfg)?;
            // (state, the inputs that led to it)
            let mut layer: Vec<(Rt2, Vec<u8>)> = vec![(eng.initial()?, Vec::new())];
            for (i, want) in traj.iter().enumerate() {
                let f = i as u32 + 1;
                let mut next: Vec<(Rt2, Vec<u8>)> = Vec::new();
                let mut seen: rustc_hash::FxHashSet<celeste_engine::exact::ExactRow> = Default::default();
                let mut tried = 0usize;
                // Where the successors went: reported when none is at `want`.
                let mut reached: std::collections::BTreeMap<Option<(i32, i32)>, usize> = Default::default();
                // At the right position but projected onto another row.
                let mut off_chain = 0usize;
                let mut off_chain_sample: Option<celeste_engine::runtime2::Rt2> = None;
                // A `-` frame (the spawn) takes only the idle input: inputs
                // before the player exists cannot matter.
                let inputs: std::ops::Range<u8> = if want.is_some() { 0..64 } else { 0..1 };
                for (st, path) in &layer {
                    for byte in inputs.clone() {
                        // Every leaf at the wanted position is a candidate;
                        // the real-PICO-8 replay settles it.
                        for block in eng.step(st, byte)? {
                            tried += 1;
                            let cell = block.positions()?[0];
                            let pos = celeste_rust::search::pos_graph::cell_xy(cell);
                            if let Some(w) = want {
                                if pos != Some(*w) {
                                    *reached.entry(pos).or_default() += 1;
                                    continue;
                                }
                            }
                            if let (Some(k), Some(l)) = (keyed[i], level) {
                                let (_, keys, _) = widened_keys(&block, l, &space)?;
                                if keys[0] != Some(k) {
                                    off_chain += 1;
                                    off_chain_sample.get_or_insert_with(|| block.rt2().clone_block());
                                    continue;
                                }
                            }
                            // Deduplicated on the EXACT state.
                            let mut exact = celeste_engine::exact::exact_rows(block.rt2(), celeste_rust::compiled::ids()).swap_remove(0).into_vec();
                            exact.extend_from_slice(&cell.to_le_bytes());
                            if !seen.insert(exact.into_boxed_slice()) {
                                continue;
                            }
                            let mut p = path.clone();
                            p.push(byte);
                            if stop_at_win && wins_of(block.rt2(), &cfg)?.iter().any(|&w| w) {
                                println!("[trajectory] WIN at f{f} after {} frames; inputs:", p.len());
                                println!("{}", p.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
                                return Ok(());
                            }
                            next.push((block.into_rt2(), p));
                        }
                    }
                }
                eprintln!(
                    "[trajectory] f{f:03} want {:?}: {} states ({tried} steps)",
                    want,
                    next.len()
                );
                if next.is_empty() {
                    if let (Some(sample), Some(l)) = (&off_chain_sample, level) {
                        let mut w = sample.clone_block();
                        widen_rt2_to(&mut w, l);
                        eprintln!("[trajectory] f{f}: {off_chain} successors at the position but on another row; one, projected: {}", player_summary(&w, 0));
                    }
                    eprintln!("[trajectory] f{f}: the successors reached {} positions instead:", reached.len());
                    for (pos, n) in &reached {
                        eprintln!("[trajectory]   {pos:?}: {n}");
                    }
                    anyhow::bail!("the trajectory cannot be followed at f{f} (wanted {:?})", want);
                }
                layer = next;
            }
            println!("[trajectory] followed to the end: {} states; one input sequence:", layer.len());
            println!("{}", layer[0].1.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
        }
        Command::Follow { level_dir, level, inputs, show } => {
            let dir = std::path::Path::new(&level_dir);
            let tree = tree_of(dir)?;
            let lvl = level;
            cfg.check(dir, lvl)?;
            let bytes = celeste_rust::concrete::read_inputs(&inputs)?;
            // (key, cell) -> (layer, state): where the tree first has it.
            let mut at: rustc_hash::FxHashMap<(u64, u64, u32), (u32, Sk)> = Default::default();
            let geo = celeste_rust::storage::GEOMETRY;
            let layers = tree.last + 1;
            for f in 0..layers {
                for sk in tree.layer(f, None, cfg.threads())?.0.states_iter() {
                    at.entry((sk.key.0, sk.key.1, sk.cell(&geo))).or_insert((f, sk));
                }
            }
            let space = &tree.space;
            eprintln!("[follow] {} rows in {layers} layers, {} inputs", at.len(), bytes.len());
            let mut eng = RefEngine::new(&cfg)?;
            let mut states = vec![eng.initial()?];
            let mut parent: Option<(u32, Sk)> = None;
            for (f, &byte) in bytes.iter().enumerate() {
                let f = f as u32 + 1;
                let mut next = Vec::new();
                for s0 in &states {
                    next.extend(eng.step(s0, byte)?);
                }
                // Where the tree has one of the concrete leaves (an `rnd` fork
                // makes several).
                let mut found = None;
                let mut won = false;
                for block in &next {
                    if wins_of(block.rt2(), &cfg)?.iter().any(|&w| w) {
                        won = true;
                    }
                    let (_, keys, cells) = widened_keys(block, lvl, space)?;
                    if let Some(&loc) = keys[0].and_then(|k| at.get(&(k.0, k.1, cells[0]))) {
                        found = Some(loc);
                    }
                }
                // A win is not stored in the tree: check the kernels make it.
                let Some(loc) = found.filter(|_| !won) else {
                    println!("[follow] f{f} input {byte}: {} ({} concrete leaves)", if won { "WIN" } else { "NOT IN THE TREE" }, next.len());
                    let (layer, sk) = parent.ok_or_else(|| anyhow::anyhow!("the first step is already missing"))?;
                    let rt2 = tree.row(layer, sk, cfg.room)?;
                    println!("    parent row (layer {layer}, state {}): {}", sk.show(), brief(&project_onto(&mut rt2.clone_block(), lvl)[0]));
                    for s in &states {
                        println!("    concrete parent: {}", brief(&project_all(s)[0]));
                    }
                    let kernels = celeste_rust::compiled::FrameEngine::new(&cfg, lvl)?;
                    let (out, _) = rerun(&kernels, rt2, layer)?;
                    let mut kset: std::collections::BTreeSet<Proj> = Default::default();
                    let mut kwins = 0;
                    for b in out {
                        let mut w = b.into_rt2();
                        let wins = wins_of(&w, &cfg)?;
                        kwins += wins.iter().filter(|&&x| x).count();
                        kset.extend(project_onto(&mut w, lvl));
                    }
                    println!("    {} kernel successors, {kwins} of them wins", kset.len());
                    for succ in &next {
                        let p = project_onto(&mut succ.rt2().clone_block(), lvl).remove(0);
                        let verdict = if kset.contains(&p) { "a kernel successor (not stored: a win, or dropped by level -1)" } else { "NOT a kernel successor" };
                        println!("    concrete successor, {verdict}: {}", brief(&p));
                        let dist = |k: &Proj| k.iter().filter(|(n, v)| p.get(*n) != Some(*v)).count() + p.keys().filter(|n| !k.contains_key(*n)).count();
                        let mut near: Vec<&Proj> = kset.iter().collect();
                        near.sort_by_key(|k| dist(k));
                        for k in near.iter().take(show) {
                            let diff: Vec<String> = k
                                .iter()
                                .filter(|(n, v)| p.get(*n) != Some(*v))
                                .map(|(n, v)| format!("{n}: kernel {v}, concrete {}", p.get(n).map(String::as_str).unwrap_or("-")))
                                .collect();
                            println!("      kernel successor at distance {}: {}", dist(k), diff.join("; "));
                        }
                    }
                    return Ok(());
                };
                println!("[follow] f{f} input {byte}: in the tree at layer {} (state {})", loc.0, loc.1.show());
                parent = Some(loc);
                states = next.into_iter().map(Block::into_rt2).collect();
            }
            println!("[follow] every frame is in the tree");
        }
        Command::L1Check { inputs, horizon, spd } => match cfg.arith {
            Arith::Fixed => l1_check::<Fixed>(&inputs, horizon, spd, &cfg)?,
            Arith::Float(_) => l1_check::<celeste_core::arith::Float>(&inputs, horizon, spd, &cfg)?,
        },
    }

    Ok(())
}

/// `bench-frame --metrics`: one frame's EXACT metrics, `key value` lines
/// (the same at every thread count; no timings). Kernels: every
/// built kernel's census summed (`all.*`: the traced and fused graphs'
/// nodes by kind, guard roots, static instructions by category, spill
/// slots), the frame's dynamic instructions (slices x static count, by
/// category: `dyn.*`), and per kernel that ran its own line set
/// (`k.<sym>.*`); the frame: lanes, emissions, states kept, units,
/// slices, routes, edges, bundles, the edge file's bytes.
fn frame_metrics(room: &str, level: Level, frame: u32, w: &celeste_rust::frame::FrameStats, ks: &celeste_rust::compiled::dispatch::KernelStats) -> String {
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
    // The per-kernel census keys summed; the census's guard-family and
    // leaf-class detail (the mix diagnostic's) left out.
    let summed = |k: &str| {
        k.starts_with("kind.") || k.starts_with("traced.") || k.starts_with("cat.") || k.starts_with("rootrole.")
            || matches!(k, "nodes" | "fused_nodes" | "insts" | "spill_slots" | "ssa" | "bodies" | "bodies.distinct_error" | "bodies.distinct_live" | "error_terms" | "live_conjuncts" | "roots")
    };
    for (sym, slices, census) in &kernels {
        for (k, v) in census.iter().filter(|(k, _)| summed(k)) {
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
    let mut out = format!("# rewrite bench-frame --metrics: room {room}, {level}, f{frame} -> f{}\n", frame + 1);
    for (k, v) in &m {
        writeln!(out, "{k} {v}").unwrap();
    }
    out
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
