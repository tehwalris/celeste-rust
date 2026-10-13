//! The search driver: `rewrite search` (the level-0 forward, the rotation
//! graph's backward and the concrete search), `rewrite forward` (one
//! forward pass with timing), `rewrite ckhash` (checkpoint fingerprints),
//! `rewrite export-ui` (the web UI's data), and the diagnostics that read a
//! finished tree (rows by name: `search::inspect`).

use anyhow::{Context, Result};
use celeste_engine::runtime2::Rt2;
use celeste_rust::trace::refengine::RefEngine;
use celeste_rust::frame::{widen_rt2_to, widened_keys, wins_of, Block, FrameStep, Visited};
use celeste_rust::storage::layer::Tree;
use celeste_rust::storage::Sk;
use celeste_rust::abstraction::Level;
use celeste_rust::search::inspect::{brief, cell_names, parse_xy, player_summary, project_all, project_onto, project_row, Proj};
use clap::{Parser, Subcommand};

/// mimalloc, not glibc: glibc retains gigabytes of freed slot chunks across
/// its arenas (plans/lessons.md).
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

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
        /// Collect EVERY concrete win inside the last level's W by the
        /// horizon into FILE (`f<frame> inputs a,b,.. | field=value ..` a
        /// line), not only the first: the waypoints a `--start-inputs`
        /// search continues from. The breadth-first search runs to the horizon.
        #[arg(long)]
        all_wins: Option<String>,
    },
    /// A KNOWN solution against the pruning of finished search trees
    /// (`search::known`): stepped through the reference engine, at every
    /// step it must pass level -1 (the filter the tree recorded) and the
    /// objects ladder's filter, and at every frame boundary
    /// project onto an arc-graph node whose W holds its exact remainder,
    /// reached (at a level that filters the next) - per level of `--level`,
    /// the arc phase rerun over `<checkpoint-dir>/level{i:02}` as `search`
    /// runs it, without the concrete search. Exits non-zero on the first
    /// check that drops it, with the step, the node and the state.
    /// Every tree state whose player cell lies in a rectangle, frames FROM
    /// to TO, decoded: per frame the count of each value of the fields
    /// named by `--field` substrings (e.g. `djump`, `balloon`): what the
    /// forward ever held there (a superset of the game's states).
    ScanRect {
        #[arg(long)]
        level_dir: String,
        #[arg(long)]
        from: u32,
        #[arg(long)]
        to: u32,
        /// "x0..x1,y0..y1", inclusive.
        #[arg(long, allow_hyphen_values = true)]
        rect: String,
        /// Field-name substrings to tally (every field whose name holds one).
        #[arg(long, num_args = 1.., required = true)]
        field: Vec<String>,
    },
    /// EXACT-REMAINDER REACH over a finished tree's edges (search::reach):
    /// from the start at its exact remainder, every frame's reached states
    /// pushed along their out-edges through the transfers - the arc phase's
    /// reach with W = everything. Prints per frame the reached states,
    /// their remainder points and cells against the forward's layer, the
    /// x/y extent, the FIRST ARRIVAL in (state, remainder) (the states an
    /// exact-remainder forward would expand), the edge pushes, and (a level
    /// -1 tree) the forward's states with a dropped successor; then the
    /// totals against the forward (`summary.txt`). Exact in the remainder
    /// up to the coarsened sets (counted), bounded by the tree: what the
    /// forward did not keep (level -1, a filter) is not reached.
    Reach {
        /// The search's checkpoint dir (its `level{i:02}` trees).
        #[arg(long)]
        checkpoint_dir: String,
        /// Which level's tree.
        #[arg(long, default_value_t = 0)]
        level_index: usize,
        /// The horizon in frames (the tree must reach it).
        #[arg(long)]
        to: u32,
        /// Write `reach.marks.bin` (each reached state, dist = horizon - the
        /// last frame it is reached at: `export-ui --reach DIR`) and
        /// `reach.tsv` (the per-frame table) into DIR.
        #[arg(long)]
        save_marks: Option<String>,
        /// Also write every frame's reached cells (`frame x y states`) to FILE.
        #[arg(long)]
        cells: Option<String>,
        /// Decode and print the reached states whose player is in
        /// "x0..x1,y0..y1" (their named fields), at most `--decode-max` a
        /// frame.
        #[arg(long)]
        decode_rect: Option<String>,
        /// At most this many states a frame decoded in `--decode-rect`.
        #[arg(long, default_value_t = 20)]
        decode_max: usize,
        /// Routes from the load: a comma list of input bytes, or a FILE of
        /// them one a line (`#` lines skipped; a line's text after
        /// `inputs ` if it has one). Each is stepped by the reference
        /// engine; its state at every frame must be reached with its exact
        /// remainder, else the run fails naming the first frame that loses
        /// it. A route's check stops where its player dies; a route past a
        /// win is lost there (a win is not expanded).
        #[arg(long)]
        route: Option<String>,
        /// The most x segments a reached set keeps exactly (past it: snapped
        /// outward, INEXACT): lower for less memory on a long reach.
        #[arg(long, default_value_t = 1024)]
        max_segs: usize,
        /// Also count FIRST ARRIVAL (`entered`, the summary's expansions): a
        /// union per state ever reached, kept to the end.
        #[arg(long)]
        first_arrival: bool,
    },
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
    /// A BEAM SEARCH for ANY route out of the start room (search::beam):
    /// exact concrete states (the reference engine, every input), the
    /// `width` best a frame by level -1's d (give `--level-minus-one H,S`),
    /// a dash, height, speed. Its route is REAL but NOT PROVEN OPTIMAL, and
    /// no route proves nothing. Writes the inputs to `--out`.
    Beam {
        /// States kept a frame.
        #[arg(long, default_value_t = 10000)]
        width: usize,
        /// At most this many kept states at one player cell (0: no cap).
        #[arg(long, default_value_t = 0)]
        per_cell: usize,
        /// Give up after this many frames.
        #[arg(long, default_value_t = 400)]
        max_frames: u32,
        /// The level the level -1 table's shapes are keyed at (the forward's).
        #[arg(long, value_parser = Level::parse, default_value = "r0sxhn")]
        level: Level,
        /// The route's inputs, comma-separated, one line.
        #[arg(long)]
        out: Option<String>,
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
    /// DIAGNOSTIC: is a coarser level's tree closed over a finer one's rows?
    /// Every row of the FINE tree at frames `from..=to` (with `--fine-marks`,
    /// only marked rows), projected onto `level` (`frame::widened_keys`), must
    /// be a row of the COARSE tree at that frame or before - and with
    /// `--coarse-marks`, a marked one. Per frame: the fine rows, the
    /// projections not reached (a forward loss) and those reached but
    /// unmarked (a backward loss); then the first misses' cells.
    DiagProject {
        /// The FINE level's tree.
        #[arg(long)]
        fine_dir: String,
        /// The COARSE level's tree.
        #[arg(long)]
        coarse_dir: String,
        /// The coarse level.
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// The first frame checked.
        #[arg(long, default_value_t = 1)]
        from: u32,
        /// The last frame checked (both trees must reach it).
        #[arg(long)]
        to: u32,
        /// The fine level's marks (`hNNN/levelNN.marks.bin`): only its
        /// marked rows are projected.
        #[arg(long)]
        fine_marks: Option<String>,
        /// The coarse level's marks: a reached projection must be marked.
        #[arg(long)]
        coarse_marks: Option<String>,
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
        /// with the same lines have the same graph (a raised tree against a
        /// fresh one).
        #[arg(long)]
        edges: bool,
        /// Also fingerprint each frame's level -1 drop notes (`d{frame}
        /// count hash`) by their sources' (shape, key, cell) and horizons.
        #[arg(long)]
        dropped: bool,
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
        /// A tree too big to load a frame's records whole: per frame, this
        /// many SOURCES (evenly spaced over the states first reached the
        /// frame before, those in `--source-rect` if given) and all their
        /// out-records; `--samples` of those checked. 0: every record.
        #[arg(long, default_value_t = 0)]
        sources: usize,
        /// "x0..x1,y0..y1": only sources whose player cell is inside.
        #[arg(long, allow_hyphen_values = true)]
        source_rect: Option<String>,
    },
    /// DIAGNOSTIC: how coarse a level could be. Per frame, the distinct
    /// states of a tree with the named cells (`inspect::cell_names`) whose
    /// names start with any `--erase` prefix left out (comma-separated, e.g.
    /// `spd.,rem.,player[3].dash`), and cumulatively through that frame.
    CoarseCensus {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The first frame counted.
        #[arg(long)]
        from: u32,
        /// The last frame counted.
        #[arg(long)]
        to: u32,
        /// Named-cell prefixes left out, comma-separated (e.g. `spd.,rem.`).
        #[arg(long, default_value = "")]
        erase: String,
    },
    /// DIAGNOSTIC: does a cell's visited set saturate? (A frame's rows are the
    /// states NEW at it.) From the cell indexes only: the `top` cells by
    /// visited count (and `--at x,y`) with their new states per frame, and
    /// every cell's new states at `to` as a share of its visited set.
    CellGrowth {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The first frame read.
        #[arg(long, default_value_t = 0)]
        from: u32,
        /// The last frame read (the shares are taken there).
        #[arg(long)]
        to: u32,
        /// How many of the most visited cells to list.
        #[arg(long, default_value_t = 8)]
        top: usize,
        /// Also list the cell at player position "x,y".
        #[arg(long, value_parser = parse_xy)]
        at: Option<(i32, i32)>,
        /// Also the growth by AGE (frames since a cell's first state, in
        /// steps of this): the median, p90 and mean of the states a cell had
        /// visited by that age. Comparable across rooms.
        #[arg(long)]
        by_age: Option<usize>,
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
    /// DIAGNOSTIC: the shape of a frame's recorded edges. Groups the edges
    /// recorded at `frame` by (source, target) and reports how many
    /// transfers a pair carries, how its x and y parts vary (a product of
    /// per-axis pieces?), and how many distinct transfer SETS there are (what
    /// one edge per pair with an interned set would store).
    EdgeCensus {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The frame whose recorded edges are grouped.
        #[arg(long)]
        frame: u32,
        /// The last frame of the tree's edge files to open (at least `--frame`).
        #[arg(long)]
        horizon: u32,
    },
    /// DIAGNOSTIC: per-column cardinalities of one frame, streamed. Per
    /// shape: rows, every varying column's distinct value count (capped at
    /// `cap`), named, and the distinct (spd.x, spd.y) pairs.
    ///
    /// With `--cell x,y`, only that position's rows (values listed), and
    /// whether it SATURATES: every `every` steps up to `frame`, its cumulative
    /// distinct states (without the `--erase` prefixes), speed pairs, and
    /// states without speed.
    ColCensus {
        /// The level's tree (the dir holding `frames/` and `edges/`; a search's
        /// `<checkpoint-dir>/levelNN`).
        #[arg(long)]
        level_dir: String,
        /// The frame (step) whose rows are counted.
        #[arg(long)]
        frame: u32,
        /// Stop counting a column's distinct values past this many.
        #[arg(long, default_value_t = 1 << 24)]
        cap: usize,
        /// Only the rows at player position "x,y", and its saturation over the
        /// steps.
        #[arg(long, value_parser = parse_xy)]
        cell: Option<(i32, i32)>,
        /// With `--cell`: named-cell prefixes left out of a state, comma-
        /// separated.
        #[arg(long, default_value = "")]
        erase: String,
        /// With `--cell`: report the saturation every this many steps.
        #[arg(long, default_value_t = 4)]
        every: u32,
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
        /// `rewrite reach --save-marks DIR` of the last level's tree: its
        /// exactly reached states as a level of their own.
        #[arg(long)]
        reach: Option<String>,
        /// That level's label.
        #[arg(long)]
        reach_label: Option<String>,
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

/// `coarse-census`: one frame's states EXACTLY (each row's named value
/// cells and their codes, by name), the named value cells under an `erase`
/// prefix left out (pointers count as structure); and the frame's row count.
fn erased_states(dir: &std::path::Path, frame: u32, erase: &[String], room: &celeste_rust::settings::Settings, threads: usize) -> Result<(u64, rustc_hash::FxHashSet<Box<[u8]>>)> {
    use celeste_engine::exact::Code;
    use celeste_engine::runtime2::{Cell2, AV};
    let ids = celeste_rust::compiled::ids();
    let code = |v: AV| -> Code { Code::of(if let AV::Ptr(_) = v { AV::Ptr(0) } else { v }) };
    let (mut rows, mut states) = (0u64, rustc_hash::FxHashSet::default());
    let tree = Tree::open(dir, frame)?;
    for (_, rt2) in tree.rows(frame, room, 1 << 20, &|_| true, threads)? {
        rows += rt2.width as u64;
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
            states.insert(b.into_boxed_slice());
        }
    }
    Ok((rows, states))
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
fn rerun(engine: &dyn FrameStep, rt2: Rt2, layer: u32) -> Result<(Vec<Block>, bool)> {
    let wave = celeste_rust::storage::wave::one_frame(engine, vec![Block::from_rt2(rt2)], layer + 1)?;
    let mut won = false;
    for b in &wave.next {
        won |= wins_of(b.rt2(), &engine.settings())?.contains(&true);
    }
    Ok((wave.next, won))
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

fn main() -> Result<()> {
    let cli = Cli::parse();
    // `bench-frame` times the workers' phases; `--metrics` counts the
    // kernels' census and slices (read when the kernels are built).
    let cfg = celeste_rust::settings::startup(&cli.settings, |s| {
        if let Command::BenchFrame { metrics, .. } = &cli.command {
            s.diag.phases = true;
            s.diag.kernel_metrics = metrics.is_some();
        }
    })?;

    match cli.command {
        Command::Search { to, ceiling, level, checkpoint_dir, no_witness, save_marks, prefer, all_wins } => {
            use celeste_rust::search::result::{LevelResult, SearchResult, Verdict};
            let prefer: Option<Vec<u8>> = prefer.as_deref().map(celeste_rust::concrete::read_inputs).transpose()?;
            use celeste_rust::search::arc_dp::solve;
            let horizon = match (ceiling, to) {
                (Some(h), None) | (None, Some(h)) => h,
                _ => anyhow::bail!("give the horizon: --to H, or --ceiling H for a known solution"),
            };
            // The horizon is in FRAMES; the tree, the arcs and the marks
            // count search steps (two a frame under the split frame).
            let steps = horizon * cfg.steps_per_frame();
            let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
            let room = room_text(&cfg);
            eprintln!("[search] room {room}, levels {level}, horizon {horizon}");
            let t0 = std::time::Instant::now();
            let base = std::path::Path::new(&checkpoint_dir);
            // The record of THIS run: none until it decides.
            std::fs::create_dir_all(base)?;
            match std::fs::remove_file(celeste_rust::search::result::path(base)) {
                Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(e.into()),
                _ => {}
            }
            let mut res = SearchResult {
                format: celeste_rust::search::result::FORMAT,
                room: cfg.room,
                level: level.clone(),
                horizon,
                ceiling: ceiling.is_some(),
                steps_per_frame: cfg.steps_per_frame(),
                levels: Vec::new(),
                verdict: Verdict::BoundOnly,
                optimal: None,
                witness: None,
                witness_file: None,
                wall_s: 0.0,
                peak_gb: 0.0,
                error: None,
                heuristic: cfg.is_heuristic().then(|| format!("--prune-rect {:?}: rows dropped by position, NOT a proof", cfg.prune_rects)),
            };
            // A heuristic run says so on every verdict line.
            let caveat = if cfg.is_heuristic() { " (HEURISTIC PRUNE ACTIVE, not a proof)" } else { "" };
            let record = |res: &mut SearchResult, verdict: Verdict| -> Result<()> {
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
            // The previous level's arc-marked nodes: the next forward's filter.
            let mut prev: Option<(Visited, Level)> = None;
            for (li, &lvl) in levels.iter().enumerate() {
                let last = li + 1 == levels.len();
                let dir = base.join(format!("level{li:02}"));
                // THE FORWARD. Level 0's tree is reused, extended, or RAISED
                // to this run's level -1 horizon (`grow_tree`). A finer
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
                let first_win = celeste_rust::frame::grow_tree(
                    &dir,
                    steps,
                    want,
                    filter.as_ref(),
                    lvl,
                    || Ok(Box::new(celeste_rust::compiled::FrameEngine::new(&cfg, lvl)?)),
                    || Ok(vec![Block::canonical(RefEngine::new(&cfg)?.initial()?)?]),
                    &cfg,
                )?
                .win_frame;
                let forward_s = t.elapsed().as_secs_f64();
                // `f` counts search steps.
                eprintln!("[search] level {li} ({lvl}): forward to f{steps} in {forward_s:.1} s; first win {first_win:?}");
                celeste_rust::metrics::mem_phase("forward");
                // THE ARC PHASE, and the concrete search at the LAST level only:
                // a coarser level's bound is loose wherever its objects are
                // (gemskip-nodiag 2800m h192: 379 s of a 450 s search went to a
                // fruitless try at level 0's bound f186; the optimum was 191),
                // and the finer level decides the optimum anyway.
                let concrete = last && !no_witness;
                // Every level saves its marks (the next overwrites).
                let known = prefer.as_deref().map(|inputs| celeste_rust::search::known::Route { inputs, filter: filter.as_ref() });
                let s = solve(&dir, lvl, steps, concrete, !last, save_marks.as_deref().map(std::path::Path::new), prefer.as_deref(), known.as_ref(), all_wins.as_deref().map(std::path::Path::new), &cfg)?;
                let wall = t0.elapsed().as_secs_f64();
                res.levels.push(LevelResult { spec: lvl.to_string(), dir: format!("level{li:02}"), forward_s, first_win, arc_bound: s.arc });
                let Some(bound) = s.arc else {
                    record(&mut res, Verdict::Refuted)?;
                    anyhow::ensure!(ceiling.is_none(), "ceiling {horizon} REFUTED by the arc search at level {li} ({lvl}): a known solution the model cannot reproduce");
                    println!("REFUTED: no win by f{horizon}{caveat} (level {li}, {lvl}; {wall:.1} s)");
                    return Ok(());
                };
                println!("ARC BOUND: no win before f{bound}{caveat} (level {li}, {lvl}: remainder-exact, the level's objects over-approximated)");
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
                    if let Some(prefix) = &cfg.start_inputs {
                        // A second half: the frames count from the start state;
                        // the route from the room's load is the prefix + this.
                        let full: Vec<String> = prefix.iter().chain(&w.inputs).map(|b| b.to_string()).collect();
                        let fpath = base.join(format!("route_frame_{}.txt", prefix.len() + f));
                        std::fs::write(&fpath, format!("# --start-inputs ({} frames) + the witness ({f} frames): optimal FROM the start state only\n{}\n", prefix.len(), full.join(",")))?;
                        println!("OPTIMAL from the start state{caveat}: {f} frames ({wall:.1} s); witness {}; the route from the room's load: {} frames, {}", path.display(), prefix.len() + f, fpath.display());
                        return Ok(());
                    }
                    println!("OPTIMAL win frame: {f}{caveat} ({wall:.1} s); witness {}", path.display());
                    return Ok(());
                }
                if no_witness && last {
                    return record(&mut res, Verdict::BoundOnly);
                }
                if last {
                    record(&mut res, Verdict::NoConcreteWin)?;
                    anyhow::ensure!(ceiling.is_none(), "ceiling {horizon}: no concrete witness by it - a known solution the model cannot reproduce");
                    println!("no concrete win by f{horizon}{caveat} (arc bound f{bound}; {wall:.1} s)");
                    return Ok(());
                }
                prev = Some((s.arc_marks.expect("asked for"), lvl));
            }
        }
        Command::Beam { width, per_cell, max_frames, level, out } => {
            let args = celeste_rust::search::beam::BeamArgs { width, per_cell, max_frames, level };
            println!("[beam] NOT A PROOF: a route found here is real but NOT proven optimal; none found proves nothing");
            match celeste_rust::search::beam::beam(&args, &cfg)? {
                Some(r) => {
                    let line = r.inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",");
                    println!("beam route: exits on frame {} (NOT proven optimal); inputs {line}", r.inputs.len());
                    if let Some(p) = out {
                        std::fs::write(&p, format!("{line}\n"))?;
                    }
                }
                None => println!("beam: no route within {max_frames} frames (proves nothing)"),
            }
        }
        Command::ScanRect { level_dir, from, to, rect, field } => {
            use celeste_rust::search::pos_graph::cell_xy;
            let (x, y) = rect.split_once(',').ok_or_else(|| anyhow::anyhow!("--rect x0..x1,y0..y1"))?;
            let range = |t: &str| -> Result<(i32, i32)> {
                let (a, b) = t.split_once("..").ok_or_else(|| anyhow::anyhow!("--rect x0..x1,y0..y1"))?;
                Ok((a.trim().parse()?, b.trim().parse()?))
            };
            let ((x0, x1), (y0, y1)) = (range(x)?, range(y)?);
            let dir = std::path::Path::new(&level_dir);
            let tree = tree_of(dir)?;
            let geo = celeste_rust::storage::GEOMETRY;
            let inside = |s: celeste_rust::storage::Sk| cell_xy(s.cell(&geo)).is_some_and(|(cx, cy)| (x0..=x1).contains(&cx) && (y0..=y1).contains(&cy));
            for f in from..=to.min(tree.last) {
                let mut tally: std::collections::BTreeMap<String, u64> = Default::default();
                let mut n = 0u64;
                for (_, rt2) in tree.rows(f, &cfg, 1 << 16, &inside, cfg.threads())? {
                    for p in celeste_rust::search::inspect::project_all(&rt2) {
                        n += 1;
                        let mut key: Vec<String> = p.iter().filter(|(k, _)| field.iter().any(|w| k.contains(w.as_str()))).map(|(k, v)| format!("{k}={v}")).collect();
                        key.sort();
                        *tally.entry(key.join(" ")).or_default() += 1;
                    }
                }
                println!("f{f:03}: {n} states in the rect");
                for (k, c) in &tally {
                    println!("  {c:>10}  {k}");
                }
            }
        }
        Command::Reach { checkpoint_dir, level_index, to, save_marks, cells, decode_rect, decode_max, route, max_segs, first_arrival } => {
            let opts = celeste_rust::search::reach::ReachOpts { max_segs, first_arrival };
            reach_command(&checkpoint_dir, level_index, to, save_marks.as_deref(), cells.as_deref(), decode_rect.as_deref(), decode_max, route.as_deref(), opts, &cfg)?;
        }
        Command::CheckKnown { checkpoint_dir, level, inputs, horizon } => {
            use celeste_rust::search::arc_dp::solve;
            let inputs = celeste_rust::concrete::read_inputs(&inputs)?;
            let steps = horizon * cfg.steps_per_frame();
            let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
            let t0 = std::time::Instant::now();
            let mut prev: Option<(Visited, Level)> = None;
            for (li, &lvl) in levels.iter().enumerate() {
                let last = li + 1 == levels.len();
                let dir = std::path::Path::new(&checkpoint_dir).join(format!("level{li:02}"));
                anyhow::ensure!(
                    celeste_rust::frame::tree_first_win_through(&dir, steps)?.is_some(),
                    "{}: no tree through step {steps} (run the search first)",
                    dir.display()
                );
                let filter = prev.as_ref().map(|(m, l)| celeste_rust::frame::MarkFilter::new(m, *l, cfg.steps_per_frame()));
                let route = celeste_rust::search::known::Route { inputs: &inputs, filter: filter.as_ref() };
                let s = solve(&dir, lvl, steps, false, !last, None, None, Some(&route), None, &cfg)?;
                let f = s.known.ok_or_else(|| anyhow::anyhow!("the known route does not exit by f{horizon}: nothing to check"))?;
                println!("[check-known] level {li} ({lvl}): the known route (exit f{f}) survives (arc bound {:?})", s.arc);
                if last {
                    break;
                }
                prev = Some((s.arc_marks.expect("asked for"), lvl));
            }
            println!("[check-known] every level passes ({:.1} s)", t0.elapsed().as_secs_f64());
        }
        Command::Forward {
            to,
            level,
            checkpoint_dir,
            reference,
        } => {
            let t = std::time::Instant::now();
            let engine: Box<dyn FrameStep> = if reference {
                Box::new(std::sync::Mutex::new(celeste_rust::trace::refengine::RefEngine::new(&cfg)?.at_level(level)))
            } else {
                Box::new(celeste_rust::compiled::FrameEngine::new(&cfg, level)?)
            };
            eprintln!("[fwd] engine up in {:.2} s ({level}{})", t.elapsed().as_secs_f64(), if reference { ", REFERENCE engine" } else { "" });
            let dir = std::path::Path::new(&checkpoint_dir);
            let t = std::time::Instant::now();
            // Resumed, RAISED to this run's level -1 horizon, or started.
            let fwd = celeste_rust::frame::grow_tree(
                dir,
                to,
                cfg.minus_one(),
                None,
                level,
                || Ok(engine),
                || Ok(vec![Block::canonical(RefEngine::new(&cfg)?.initial()?)?]),
                &cfg,
            )?;
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
        }
        Command::DiagProject { fine_dir, coarse_dir, level, from, to, fine_marks, coarse_marks } => {
            let load_marks = |p: Option<String>| p.map(|p| Visited::load(std::path::Path::new(&p))).transpose();
            let (fine_marks, coarse_marks) = (load_marks(fine_marks)?, load_marks(coarse_marks)?);
            let (fine, coarse) = (std::path::Path::new(&fine_dir), std::path::Path::new(&coarse_dir));
            let (fine_tree, coarse_tree) = (Tree::open(fine, to)?, Tree::open(coarse, to)?);
            // The fine rows' projections keyed in the COARSE tree's key space.
            let mut reached = Visited::new(coarse_tree.space.clone());
            let (mut total, mut missed, mut unmarked) = (0usize, 0usize, 0usize);
            let mut misses: Vec<String> = Vec::new();
            for f in 0..=to {
                for s in coarse_tree.layer(f, None, cfg.threads())?.0.states_iter() {
                    let (shape, key, cell) = coarse_tree.state(s)?;
                    reached.insert(shape, key, cell);
                }
                if f < from {
                    continue;
                }
                let (mut n, mut miss, mut unm) = (0usize, 0usize, 0usize);
                for (sks, rt2) in fine_tree.rows(f, &cfg, 1 << 20, &|_| true, cfg.threads())? {
                    let block = Block::from_rt2(rt2);
                    let (ws, wk, wc) = widened_keys(&block, level, reached.keys())?;
                    for (i, sk) in sks.iter().enumerate() {
                        let (shape, key, cell) = fine_tree.state(*sk)?;
                        if fine_marks.as_ref().is_some_and(|m| !m.contains(shape, key, cell)) {
                            continue;
                        }
                        n += 1;
                        let Some(k) = wk[i].filter(|&k| reached.contains(ws, k, wc[i])) else {
                            miss += 1;
                            if misses.len() < 8 {
                                misses.push(format!("f{f:03}: a fine row at {} projects to {}, which the coarse tree has not reached", where_(wc[i]), wk[i].map_or("a code the coarse tree never stored".to_string(), |k| format!("key {:016x}{:016x}", k.0, k.1))));
                            }
                            continue;
                        };
                        if coarse_marks.as_ref().is_some_and(|m| !m.contains(ws, k, wc[i])) {
                            unm += 1;
                        }
                    }
                }
                println!("f{f:03}: fine rows {n}, projections not reached {miss}, reached but unmarked {unm}");
                total += n;
                missed += miss;
                unmarked += unm;
            }
            for m in &misses {
                println!("MISS {m}");
            }
            println!("of {total} fine rows' projections: {missed} not reached by the coarse tree (a forward loss), {unmarked} reached but unmarked (a backward loss)");
        }
        Command::Ckhash {
            checkpoint_dir,
            to,
            cells: top_cells,
            edges,
            dropped,
            dump_edges,
        } => {
            let dir = std::path::Path::new(&checkpoint_dir);
            // Fingerprints over CONTENT (`exact::ContentHash`): a raised tree's
            // dictionaries grew in another order than a fresh one's.
            let tree = Tree::open(dir, to)?;
            let content = tree.space.content_hash();
            let geo = celeste_rust::storage::GEOMETRY;
            let mut by_cell: rustc_hash::FxHashMap<u32, usize> = Default::default();
            for frame in 0..=to {
                // A forward whose frontier died has no frames past the empty
                // one: their layers are empty.
                let (layer, _) = tree.layer(frame, None, cfg.threads())?;
                let (n, acc) = layer.content_fingerprint(&tree.space, &tree.shapes)?;
                if frame == to && top_cells.is_some() {
                    for s in layer.states_iter() {
                        *by_cell.entry(s.cell(&geo)).or_default() += 1;
                    }
                }
                println!("f{frame:03} {n} {acc:016x}");
            }
            // The recorded edges per frame, by what they join: (shape, key,
            // cell) of target and source, and the transfer - table positions
            // depend on a raise, these do not. A state by what it is
            // (`ContentHash::state`: `mix64(key hash ^ cell << 1)`).
            if edges {
                let eg = celeste_rust::storage::edges::EdgeStore::open(&dir.join("edges"), to)?;
                // STREAMED: one frame at a time, its blocks on every thread
                // (each key hashed once a block, each pair once).
                let hashes = celeste_rust::storage::bench::EdgeHashes::new(eg.pairs());
                let shapes = &tree.shapes;
                for frame in 1..=to {
                    let (n, acc) = hashes.frame(&eg, frame, &content, shapes, cfg.threads())?;
                    println!("e{frame:03} {n} {acc:016x}");
                    if dump_edges == Some(frame) {
                        let node = |s: Sk| content.state(shapes[s.shape(&geo) as usize], s.key, s.cell(&geo));
                        for e in eg.edges_at(frame, cfg.threads())? {
                            eprintln!("edge {:016x} {:016x} {:?}", node(e.dst), node(e.src), eg.pair(e.xfer));
                        }
                    }
                }
            }
            // The level -1 drop notes per frame: each noted source by what it
            // is and its horizon.
            if dropped {
                for frame in 1..=to {
                    // A forward whose frontier died has no frames past it.
                    if !celeste_rust::storage::layer::frame_exists(dir, frame) {
                        break;
                    }
                    let notes = celeste_rust::storage::layer::read_notes(&celeste_rust::storage::layer::notes_path(dir, frame))?;
                    let (mut n, mut acc) = (0u64, 0u64);
                    for &(region, key, h, mask) in &notes {
                        let shape = tree.shape_of(region)?;
                        for local in celeste_rust::storage::layer::bits(mask) {
                            let node = content.state(shape, key, geo.cell(region % geo.slots, local));
                            acc = acc.wrapping_add(celeste_engine::runtime2::mix64(node ^ (h as u64).rotate_left(32)));
                            n += 1;
                        }
                    }
                    println!("d{frame:03} {n} {acc:016x}");
                }
            }
            if let Some(top) = top_cells {
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
        Command::BenchFrame { level_dir, frame, reps, level, edges, metrics } => {
            use celeste_rust::compiled::dispatch::{print_stats, take_kernel_stats};
            let dir = std::path::PathBuf::from(&level_dir);
            let frame = match frame {
                Some(f) => f,
                None => celeste_rust::storage::edges::done_frame(&dir.join("edges")).ok_or_else(|| anyhow::anyhow!("{level_dir}: no finished frame"))?,
            };
            let engine = celeste_rust::compiled::FrameEngine::new(&cfg, level)?;
            let t = std::time::Instant::now();
            engine.warm();
            let t_kernels = t.elapsed().as_secs_f64();
            let t = std::time::Instant::now();
            let st = celeste_rust::frame::ForwardState::at(&dir, frame, true, level, &cfg)?;
            eprintln!(
                "[bench] kernels built in {t_kernels:.2} s; f{frame} loaded in {:.2} s: {} lanes in {} entries ({:.3} GB, {:.2} B a state), visited {} states in {} entries ({:.3} GB, {:.2} B a state); {} threads; rss {:.2} GB",
                t.elapsed().as_secs_f64(),
                st.frontier().states(),
                st.frontier().entries(),
                st.frontier().alloc_bytes() as f64 / 1e9,
                st.frontier().alloc_bytes() as f64 / st.frontier().states().max(1) as f64,
                st.visited_len(),
                st.visited().entries(),
                st.visited().alloc_bytes() as f64 / 1e9,
                st.visited().alloc_bytes() as f64 / st.visited_len().max(1) as f64,
                cfg.threads(),
                celeste_rust::metrics::current_rss_gb()
            );
            let scratch = dir.join("bench-edges");
            let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
            let mut lines: Option<String> = None;
            for rep in 0..reps {
                let _ = std::fs::remove_dir_all(&scratch);
                let t = std::time::Instant::now();
                let (wave, visited, xfers) = st.bench_wave(&engine, &dir, edges.then_some(scratch.as_path()), false)?;
                let t_all = t.elapsed();
                let w = &wave.stats;
                eprintln!(
                    "[bench] rep {rep}: wave {:.0} ms (idle {:.0}%, regions {:.0} worker-ms) end {:.0} total {:.0} ms | in {} emitted {} kept {} | units {} slices {} routes {} | edges {} bundles {} ({:.3} B) | rss start {:.2} wave {:.2} end {:.2} peak {:.2} GB",
                    ms(w.t_wave),
                    w.wave_idle * 100.0,
                    ms(w.t_regions),
                    ms(w.t_layer),
                    ms(t_all),
                    w.lanes_in,
                    w.lanes_raw,
                    w.lanes_kept,
                    w.units,
                    w.slices,
                    w.routes,
                    w.edge_records,
                    w.edge_bundles,
                    w.edge_bytes as f64 / w.edge_records.max(1) as f64,
                    w.rss_start,
                    w.rss_wave,
                    w.rss_end,
                    celeste_rust::metrics::peak_rss_gb(),
                );
                let (n, acc) = wave.next.content_fingerprint(&visited.keys, visited.shapes())?;
                let mut out = format!("f{:03} {n} {acc:016x}\n", frame + 1);
                if edges {
                    let (n, acc) = celeste_rust::storage::bench::edge_fingerprint(&scratch, frame + 1, &xfers, &visited, cfg.threads())?;
                    out += &format!("e{:03} {n} {acc:016x}\n", frame + 1);
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
                        let text = frame_metrics(&room_text(&cfg), level, frame, w, &ks);
                        std::fs::write(path, text).with_context(|| path.clone())?;
                    }
                    celeste_rust::compiled::mix::reset_slices();
                }
                lines = Some(out);
            }
            let _ = std::fs::remove_dir_all(&scratch);
            let ks = take_kernel_stats();
            anyhow::ensure!(ks.missed == 0, "KERNEL COVERAGE GAP: {} lanes missed", ks.missed);
        }
        Command::BenchBackward { level_dir, horizon, frame, reps } => {
            anyhow::ensure!(frame < horizon, "--frame {frame}: below the horizon {horizon}");
            let dir = std::path::Path::new(&level_dir);
            let t = std::time::Instant::now();
            let seeds = celeste_rust::search::arc_dp::Seeds::load(dir, horizon)?;
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
        }
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
        Command::CellGrowth { level_dir, from, to, top, at, by_age } => {
            let dir = std::path::Path::new(&level_dir);
            let n_frames = (to - from + 1) as usize;
            // Per cell, its new states per frame.
            let mut per: std::collections::HashMap<u32, Vec<u64>> = Default::default();
            let tree = Tree::open(dir, to)?;
            let geo = celeste_rust::storage::GEOMETRY;
            for f in from..=to {
                for s in tree.layer(f, None, cfg.threads())?.0.states_iter() {
                    per.entry(s.cell(&geo)).or_insert_with(|| vec![0; n_frames])[(f - from) as usize] += 1;
                }
            }
            let visited = |v: &[u64]| v.iter().sum::<u64>();
            if let Some(stepw) = by_age {
                // Per cell: its states visited by each age since its first.
                let series: Vec<Vec<u64>> = per
                    .values()
                    .filter_map(|v| {
                        let first = v.iter().position(|&n| n > 0)?;
                        Some(v[first..].iter().scan(0u64, |acc, &n| {
                            *acc += n;
                            Some(*acc)
                        }).collect())
                    })
                    .collect();
                println!("age | cells | median p90 mean (states visited by that age)");
                for a in (0..n_frames).step_by(stepw.max(1)) {
                    let mut at_a: Vec<u64> = series.iter().filter(|s| s.len() > a).map(|s| s[a]).collect();
                    if at_a.is_empty() {
                        break;
                    }
                    at_a.sort_unstable();
                    let mean = at_a.iter().sum::<u64>() as f64 / at_a.len() as f64;
                    println!("{a:>3} | {:>6} | {} {} {:.0}", at_a.len(), at_a[at_a.len() / 2], at_a[at_a.len() * 9 / 10], mean);
                }
            }
            let mut by_visited: Vec<(u32, u64)> = per.iter().map(|(c, v)| (*c, visited(v))).collect();
            by_visited.sort_by_key(|&(c, n)| (std::cmp::Reverse(n), c));
            let mut chosen: Vec<u32> = by_visited.iter().take(top).map(|&(c, _)| c).collect();
            if let Some(c) = at.map(cell_at).transpose()? {
                if !chosen.contains(&c) {
                    chosen.push(c);
                }
            }
            let shown = n_frames.min(12);
            println!("cell: visited through f{to:03} | new per frame, f{:03}..f{to:03}", to - shown as u32 + 1);
            for c in &chosen {
                match per.get(c) {
                    Some(v) => {
                        let tail: Vec<String> = v[n_frames - shown..].iter().map(|n| n.to_string()).collect();
                        println!("{}: {} | {}", where_(*c), visited(v), tail.join(" "));
                    }
                    None => println!("{}: never visited", where_(*c)),
                }
            }
            // Every cell's new states at `to` as a share of its visited set.
            let edges = [0.01, 0.05, 0.20];
            let mut cells_in = [0usize; 4];
            let mut states_in = [0u64; 4];
            for v in per.values() {
                let share = v[n_frames - 1] as f64 / visited(v).max(1) as f64;
                let b = edges.iter().filter(|&&e| share >= e).count();
                cells_in[b] += 1;
                states_in[b] += visited(v);
            }
            println!(
                "{} cells visited; new at f{to:03} as a share of visited: under 1%: {} cells ({} states), 1-5%: {} ({}), 5-20%: {} ({}), 20% or more: {} ({})",
                per.len(),
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
        Command::ArcCheck { level_dir, from, to, samples, level, fault, path_cap, sources, source_rect } => {
            use celeste_engine::runtime2::{Col, AV};
            use celeste_rust::search::arcs::{point, Rect, Rects, Set, CIRCLE};
            let dir = std::path::Path::new(&level_dir);
            cfg.check(dir, level)?;
            let edges_dir = dir.join("edges");
            let ids = celeste_rust::compiled::ids();
            let graph = celeste_rust::storage::edges::EdgeStore::open(&edges_dir, to)?;
            let rem_cells = |rt2: &Rt2| rt2.player_xy_cells(ids, ids.f_rem);
            // One stored state's row, as a one-lane block.
            let tree = Tree::open(dir, to)?;
            let row_of = |s: Sk| -> Result<Block> {
                let f = tree.layer_of(s, to)?.ok_or_else(|| anyhow::anyhow!("state {}: in no frame through f{to}", s.show()))?;
                Ok(Block::from_rt2(tree.row(f, s, &cfg)?))
            };
            // Every successor of `row` at remainder `rem` over all 64 inputs:
            // its projection onto the level and its player's remainder; and
            // the reference steps it took.
            type Succ = ((u64, Option<(u64, u64)>, u32), Option<(u32, u32)>);
            // The tree's key space: projections keyed as its rows (`None`: a
            // code it never stored).
            let space = &tree.space;
            let successors = |eng: &mut RefEngine, row: &Block, rem: (u32, u32)| -> Result<(Vec<Succ>, u64)> {
                // As the kernels read it: projected onto the level (the start
                // row is stored exact), then the probe's remainder.
                let mut rt2 = row.rt2().clone_block();
                widen_rt2_to(&mut rt2, level);
                if let Some((cx, cy)) = rem_cells(&rt2) {
                    rt2.cols[cx] = Col::U(AV::Num(celeste_rust::pico8_num::Pico8Num::from_raw(rem.0 as i32 - 32768)));
                    rt2.cols[cy] = Col::U(AV::Num(celeste_rust::pico8_num::Pico8Num::from_raw(rem.1 as i32 - 32768)));
                }
                let (mut out, mut steps) = (Vec::new(), 0u64);
                // An input that agrees with one already run on the buttons it
                // read makes the same successors (`RefEngine::step_reads`).
                let mut ran: Vec<(u8, u8)> = Vec::new();
                for b in 0..64u8 {
                    if ran.iter().any(|(r, m)| (r ^ b) & m == 0) {
                        continue;
                    }
                    let (blocks, read) = eng.step_at(&rt2, b, level)?;
                    ran.push((b, read));
                    for blk in blocks {
                        steps += 1;
                        let q = match rem_cells(blk.rt2()) {
                            Some((cx, cy)) => {
                                let raw = |c: usize| -> Result<u32> {
                                    match blk.rt2().cols[c].at(0) {
                                        AV::Num(n) => Ok(point(n.as_raw_u32() as i32)),
                                        other => anyhow::bail!("arc-check: a concrete remainder expected, got {other:?}"),
                                    }
                                };
                                Some((raw(cx)?, raw(cy)?))
                            }
                            None => None,
                        };
                        let (shape, keys, cells) = widened_keys(&blk, level, space)?;
                        out.push(((shape, keys[0], cells[0]), q));
                    }
                }
                Ok((out, steps))
            };
            // The injected fault (`--fault`), on one decoded x transfer.
            let corrupt = |mut t: celeste_rust::search::arcs::Transfer| -> Result<celeste_rust::search::arcs::Transfer> {
                use celeste_rust::search::arcs::Action;
                match fault.as_deref() {
                    None => {}
                    Some("action") => {
                        t.action = match t.action {
                            Action::Rotate(v) => Action::Rotate(v + 1),
                            Action::Const(c) => Action::Const((c + 1) % CIRCLE),
                        }
                    }
                    Some("guard") => t.guard.lo = (t.guard.lo + 1).min(t.guard.hi - 1),
                    Some(other) => anyhow::bail!("--fault {other}: `action` or `guard`"),
                }
                Ok(t)
            };
            /// A record with its transfer decoded.
            struct Rec {
                target: celeste_rust::storage::Sk,
                src: celeste_rust::storage::Sk,
                x: celeste_rust::search::arcs::Transfer,
                y: celeste_rust::search::arcs::Transfer,
            }
            /// One sampled record's verdicts, and what it printed.
            #[derive(Default)]
            struct Sampled {
                inside: u64,
                inside_bad: u64,
                unpredicted: u64,
                outside: u64,
                outside_bad: u64,
                no_player: u64,
                steps: u64,
                /// Over the path cap (`--path-cap`): not checked.
                skipped: u64,
                msgs: Vec<String>,
            }
            // One sampled record against the reference engine.
            let check = |eng: &mut RefEngine, frame: u32, r: &Rec, recs: &[Rec]| -> Result<Sampled> {
                let mut o = Sampled::default();
                let src = r.src;
                let src_row = row_of(src)?;
                let tgt_row = row_of(r.target)?;
                let (tshape, tkeys, tcells) = widened_keys(&tgt_row, level, space)?;
                anyhow::ensure!(tkeys[0].is_some(), "arc-check: the target {} does not key in its own tree", r.target.show());
                let target = (tshape, tkeys[0], tcells[0]);
                let (gx, gy) = (r.x.guard, r.y.guard);
                // Every record of this (pred, target), and its guards as rectangles.
                let pair: Vec<&Rec> = recs.iter().filter(|q| q.target == r.target && q.src == src).collect();
                let mut union = Rects::empty();
                for q in &pair {
                    union.add(Rect { x: q.x.guard_set(), y: q.y.guard_set() });
                }
                let has_player = rem_cells(src_row.rt2()).is_some();
                if !has_player {
                    o.no_player += 1;
                }
                let mid = |g: celeste_rust::search::arcs::Seg| (g.lo + g.hi) / 2;
                let probes: Vec<(u32, u32)> = if has_player {
                    vec![(gx.lo, gy.lo), (gx.hi - 1, gy.hi - 1), (mid(gx), mid(gy)), (gx.lo, gy.hi - 1)]
                } else {
                    vec![(CIRCLE / 2, CIRCLE / 2)]
                };
                for p in probes {
                    o.inside += 1;
                    let want = (r.x.action.image(&Set::point(p.0)), r.y.action.image(&Set::point(p.1)));
                    let (succs, steps) = successors(eng, &src_row, p)?;
                    o.steps += steps;
                    let ok = succs.iter().any(|(id, q)| {
                        *id == target
                            && match q {
                                Some((qx, qy)) => want.0.contains(*qx) && want.1.contains(*qy),
                                None => true,
                            }
                    });
                    // Every successor on the target lands where a record
                    // of the pair that takes `p` predicts.
                    let predicted = |q: &(u32, u32)| {
                        pair.iter().any(|o| {
                            o.x.takes(p.0)
                                && o.y.takes(p.1)
                                && o.x.action.image(&Set::point(p.0)).contains(q.0)
                                && o.y.action.image(&Set::point(p.1)).contains(q.1)
                        })
                    };
                    let stray: Vec<(u32, u32)> = succs.iter().filter(|(id, _)| *id == target).filter_map(|(_, q)| *q).filter(|q| !predicted(q)).collect();
                    if !stray.is_empty() {
                        o.unpredicted += 1;
                        o.msgs.push(format!("[arc-check] f{frame} pred {} -> {}: inside {p:?} the reference reaches the target at {stray:?}, which no record of the pair taking the point predicts", src.show(), r.target.show()));
                    }
                    if !ok {
                        o.inside_bad += 1;
                        let reached: Vec<&Option<(u32, u32)>> = succs.iter().filter(|(id, _)| *id == target).map(|(_, q)| q).collect();
                        o.msgs.push(format!(
                            "[arc-check] f{frame} pred {} -> {}: inside {p:?} the transfer x {:?} y {:?} predicts {want:?}; the reference reaches the target with remainders {reached:?}",
                            src.show(), r.target.show(), r.x, r.y
                        ));
                    }
                }
                if has_player {
                    let mut outs: Vec<(u32, u32)> = Vec::new();
                    for (x, y) in [(gx.lo.wrapping_sub(1), mid(gy)), (gx.hi, mid(gy)), (mid(gx), gy.lo.wrapping_sub(1)), (mid(gx), gy.hi), (gx.hi, gy.hi)] {
                        if x < CIRCLE && y < CIRCLE && !union.contains(x, y) {
                            outs.push((x, y));
                        }
                    }
                    for p in outs {
                        o.outside += 1;
                        let (succs, steps) = successors(eng, &src_row, p)?;
                        o.steps += steps;
                        if succs.iter().any(|(id, _)| *id == target) {
                            o.outside_bad += 1;
                            o.msgs.push(format!("[arc-check] f{frame} pred {} -> {}: OUTSIDE every guard of the pair at {p:?}, the reference reaches the target", src.show(), r.target.show()));
                        }
                    }
                }
                Ok(o)
            };
            let (mut missing_total, mut records_total, mut sampled) = (0u64, 0u64, 0u64);
            let mut total = Sampled::default();
            let mut kinds = std::collections::BTreeMap::<String, u64>::new();
            let t0 = std::time::Instant::now();
            let workers = cfg.threads();
            let rect = source_rect
                .as_deref()
                .map(|r| -> Result<((i32, i32), (i32, i32))> {
                    let (x, y) = r.split_once(',').ok_or_else(|| anyhow::anyhow!("--source-rect x0..x1,y0..y1"))?;
                    let range = |t: &str| -> Result<(i32, i32)> {
                        let (a, b) = t.split_once("..").ok_or_else(|| anyhow::anyhow!("--source-rect x0..x1,y0..y1"))?;
                        Ok((a.trim().parse()?, b.trim().parse()?))
                    };
                    Ok((range(x)?, range(y)?))
                })
                .transpose()?;
            let geo = celeste_rust::storage::GEOMETRY;
            for frame in from..=to {
                let mut missing = 0u64;
                let recs: Vec<Rec> = if sources == 0 {
                    let all = graph.edges_at(frame, cfg.threads())?;
                    missing = all.iter().filter(|e| graph.pair(e.xfer).is_none()).count() as u64;
                    all.iter()
                        .filter_map(|e| graph.pair(e.xfer).map(|p| (e, p)))
                        .map(|(e, p)| Ok(Rec { target: e.dst, src: e.src, x: corrupt(p.0.transfer())?, y: p.1.transfer() }))
                        .collect::<Result<_>>()?
                } else {
                    // Sources from the layer before (their out-records are
                    // this frame's), evenly spaced; all their out-records.
                    let (layer, _) = tree.layer(frame - 1, None, cfg.threads())?;
                    let inside = |s: &Sk| rect.is_none_or(|((x0, x1), (y0, y1))| celeste_rust::search::pos_graph::cell_xy(s.cell(&geo)).is_some_and(|(x, y)| (x0..=x1).contains(&x) && (y0..=y1).contains(&y)));
                    let all: Vec<Sk> = layer.states_iter().filter(inside).collect();
                    let stride = (all.len() / sources).max(1);
                    let srcs: Vec<Sk> = all.iter().step_by(stride).take(sources).copied().collect();
                    let mut out = Vec::new();
                    for (k, dst, x) in celeste_rust::search::arc_dp::out_edges(&graph, frame, &srcs, cfg.threads())? {
                        match graph.pair(x) {
                            Some(p) => out.push(Rec { target: dst, src: srcs[k], x: corrupt(p.0.transfer())?, y: p.1.transfer() }),
                            None => missing += 1,
                        }
                    }
                    out.sort_by_key(|r| (r.src, r.target));
                    out
                };
                missing_total += missing;
                records_total += recs.len() as u64;
                for r in &recs {
                    for (axis, t) in [("x", &r.x), ("y", &r.y)] {
                        let whole = t.guard == celeste_rust::search::arcs::Seg { lo: 0, hi: celeste_rust::search::arcs::CIRCLE };
                        let k = format!("{axis} {} {}", if whole { "whole" } else { "piece" }, match t.action { celeste_rust::search::arcs::Action::Rotate(_) => "rotate", _ => "const" });
                        *kinds.entry(k).or_default() += 1;
                    }
                }
                // `samples` records evenly spaced, checked in parallel (a
                // reference engine per worker), reported in record order.
                let stride = (recs.len() / samples.max(1)).max(1);
                let picked: Vec<&Rec> = recs.iter().step_by(stride).take(samples).collect();
                let next = std::sync::atomic::AtomicUsize::new(0);
                let mut done: Vec<(usize, Sampled)> = std::thread::scope(|sc| {
                    let hs: Vec<_> = (0..workers.min(picked.len()))
                        .map(|_| {
                            sc.spawn(|| -> Result<Vec<(usize, Sampled)>> {
                                let mut eng = RefEngine::new(&cfg)?;
                                if let Some(cap) = path_cap {
                                    eng.set_path_cap(cap);
                                }
                                let mut out = Vec::new();
                                loop {
                                    let k = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                    let Some(r) = picked.get(k) else { return Ok(out) };
                                    match check(&mut eng, frame, r, &recs) {
                                        Ok(o) => out.push((k, o)),
                                        Err(e) if e.downcast_ref::<celeste_rust::trace::refdriver::TooManyPaths>().is_some() => {
                                            let msg = format!("[arc-check] f{frame} pred {} -> {}: SKIPPED ({e})", r.src.show(), r.target.show());
                                            out.push((k, Sampled { skipped: 1, msgs: vec![msg], ..Default::default() }));
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                            })
                        })
                        .collect();
                    hs.into_iter().map(|h| h.join().expect("an arc-check worker panicked")).collect::<Result<Vec<_>>>()
                })?
                .into_iter()
                .flatten()
                .collect();
                done.sort_by_key(|d| d.0);
                for (_, o) in done {
                    for m in &o.msgs {
                        eprintln!("{m}");
                    }
                    sampled += 1;
                    total.skipped += o.skipped;
                    total.inside += o.inside;
                    total.inside_bad += o.inside_bad;
                    total.unpredicted += o.unpredicted;
                    total.outside += o.outside;
                    total.outside_bad += o.outside_bad;
                    total.no_player += o.no_player;
                    total.steps += o.steps;
                }
                eprintln!(
                    "[arc-check] f{frame:03}: {} records, {missing} without a transfer; so far inside {} ({} bad, {} unpredicted), outside {} ({} bad), {} ref steps, {:.1} s",
                    recs.len(),
                    total.inside,
                    total.inside_bad,
                    total.unpredicted,
                    total.outside,
                    total.outside_bad,
                    total.steps,
                    t0.elapsed().as_secs_f64()
                );
            }
            println!("arc-check f{from}-f{to}: {records_total} records; transfers per axis {kinds:?}");
            println!("completeness: {missing_total} recorded edges without a transfer");
            println!(
                "agreement: {sampled} sampled records ({} from a row without a player, {} SKIPPED over the path cap); inside the guard {} probes, {} disagree, {} reach the target unpredicted; outside every guard {} probes, {} reach the target; {} reference steps",
                total.no_player, total.skipped, total.inside, total.inside_bad, total.unpredicted, total.outside, total.outside_bad, total.steps
            );
            anyhow::ensure!(missing_total == 0 && total.inside_bad == 0 && total.unpredicted == 0 && total.outside_bad == 0, "arc-check: disagreement");
        }
        Command::CoarseCensus { level_dir, from, to, erase } => {
            let erase = prefixes(&erase);
            let mut through: rustc_hash::FxHashSet<Box<[u8]>> = Default::default();
            for frame in from..=to {
                let (rows, states) = erased_states(std::path::Path::new(&level_dir), frame, &erase, &cfg, cfg.threads())?;
                let n = states.len();
                through.extend(states);
                println!("f{frame:03}: {rows} rows -> {n} states ({:.2}x fewer); {} states through f{frame:03}", rows as f64 / n.max(1) as f64, through.len());
            }
        }
        Command::WidenCheck { level_dir, frame, level, emitted } => {
            anyhow::ensure!(
                !(level.floors_near && frame % cfg.steps_per_frame() != 0),
                "step {frame} is mid split frame at a near level: the graph side keeps a floor's computed `collideable` there, the block side does not"
            );
            let dir = std::path::Path::new(&level_dir);
            cfg.check(dir, level)?;
            let ids = celeste_rust::compiled::ids();
            // Every lane of `rt2` against its block-side widening, exactly:
            // the lanes it moved (the first few printed, `shown` so far).
            let check = |rt2: &Rt2, what: &str, shown: usize| -> usize {
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
            let tree = Tree::open(dir, frame)?;
            let blocks = tree.rows(frame, &cfg, 1 << 20, &|_| true, cfg.threads())?;
            for (_, rt2) in &blocks {
                stored += rt2.width;
                moved += check(rt2, "stored", moved);
            }
            if emitted > 0 {
                let kernels = celeste_rust::compiled::FrameEngine::new(&cfg, level)?;
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
        }
        Command::RefCheck { level_dir, frame, level, samples, cell, show, path_cap } => {
            let dir = std::path::Path::new(&level_dir);
            cfg.check(dir, level)?;
            let kernels = celeste_rust::compiled::FrameEngine::new(&cfg, level)?;
            kernels.warm();
            let only = cell.map(cell_at).transpose()?;
            // Every candidate state, then `samples` evenly spaced.
            let tree = Tree::open(dir, frame)?;
            let geo = celeste_rust::storage::GEOMETRY;
            let all: Vec<Sk> = tree.layer(frame, None, cfg.threads())?.0.states_iter().filter(|s| only.is_none_or(|c| s.cell(&geo) == c)).collect();
            anyhow::ensure!(!all.is_empty(), "no rows at step {frame}");
            let n = samples.min(all.len());
            let picked: Vec<Sk> = (0..n).map(|k| all[k * all.len() / n]).collect();
            // One row: (its report, reference-only successors, kernel-only,
            // skipped over the path cap, lanes converted alike).
            let one = |reference: &mut RefEngine, sk: Sk| -> Result<(String, usize, usize, usize, usize)> {
                let rt2 = tree.row(frame, sk, &cfg)?;
                let at = where_(sk.cell(&geo));
                let id = sk.show();
                // The row, its kernel successors and its reference leaves
                // convert alike on the tracer's side and the reference's
                // (`block::check_agreement`).
                let mut alike = 0;
                let mut agree = |reference: &RefEngine, b: &Rt2| -> Result<()> {
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
                for leaf in leaves {
                    agree(reference, leaf.rt2())?;
                    rset.extend(project_onto(&mut leaf.into_rt2(), level));
                }
                let missing: Vec<&Proj> = rset.difference(&kset).collect();
                let extra: Vec<&Proj> = kset.difference(&rset).collect();
                let verdict = if missing.is_empty() && extra.is_empty() { "ok" } else { "DIFFERS" };
                let mut out = format!("[ref-check] state {id} at {at}: kernels {} successors, reference {}: {verdict} ({} only in the reference, {} only in the kernels)\n", kset.len(), rset.len(), missing.len(), extra.len());
                for p in missing.iter().take(show) {
                    out += &format!("    only in the REFERENCE (soundness gap): {}\n", brief(p));
                }
                for p in extra.iter().take(show) {
                    out += &format!("    only in the KERNELS (precision gap): {}\n", brief(p));
                }
                Ok((out, missing.len(), extra.len(), 0, alike))
            };
            // The rows in parallel (the kernels' engine shared, a reference
            // engine per worker), reported in row order.
            let done = celeste_rust::par::Pool::new(cfg.threads()).map_with(
                n,
                || {
                    let mut reference = RefEngine::new(&cfg)?.at_level(level);
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
            let tree = Tree::open(dir, layer)?;
            let (states, _) = tree.layer(layer, None, cfg.threads())?;
            let sk = states.states_iter().nth(r as usize).ok_or_else(|| anyhow::anyhow!("state {r} past frame {layer}'s {}", states.states()))?;
            let rt2 = tree.row(layer, sk, &cfg)?;
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
                    for (_, rt2) in real_tree.rows(s, &cfg, 1 << 20, &|x| x.cell(&geo) == c, 1)? {
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
                for (sks, rt2) in coarse_tree.rows(step, &cfg, 1 << 20, &|x| x.cell(&geo) == want, 1)? {
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
            let edges = celeste_rust::storage::edges::EdgeStore::open(&coarse.join("edges"), step)?;
            let n = samples.min(chosen.len());
            for k in 0..n {
                let (mut id, mut layer) = (chosen[k * chosen.len() / n], step);
                let rt2 = coarse_tree.row(layer, id, &cfg)?;
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
                        let rt = coarse_tree.row(layer - 1, p, &cfg)?;
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
        Command::EdgeCensus { level_dir, frame, horizon } => {
            let g = celeste_rust::storage::edges::EdgeStore::open(&std::path::Path::new(&level_dir).join("edges"), horizon)?;
            let pairs = g.pairs();
            let mut recs = g.edges_at(frame, cfg.threads())?;
            recs.sort_unstable();
            let n_edges = recs.len();
            let mut hist = std::collections::BTreeMap::<usize, u64>::new();
            let (mut n_pairs, mut product, mut x_const, mut y_const, mut dup) = (0u64, 0u64, 0u64, 0u64, 0u64);
            let mut sets = rustc_hash::FxHashSet::<Vec<u32>>::default();
            let mut xsets = rustc_hash::FxHashSet::<Vec<(u32, u32, u8, i32)>>::default();
            let mut ysets = rustc_hash::FxHashSet::<Vec<(u32, u32, u8, i32)>>::default();
            let mut i = 0;
            while i < recs.len() {
                let mut j = i;
                while j < recs.len() && (recs[j].src, recs[j].dst) == (recs[i].src, recs[i].dst) {
                    j += 1;
                }
                let mut ids: Vec<u32> = recs[i..j].iter().map(|e| e.xfer).collect();
                let before = ids.len();
                ids.dedup();
                dup += (before - ids.len()) as u64;
                n_pairs += 1;
                *hist.entry(ids.len()).or_default() += 1;
                let key = |a: &celeste_rust::search::arc_edges::AxisXfer| (a.lo, a.hi, a.tag, a.val);
                let mut xs: Vec<_> = ids.iter().map(|&x| key(&pairs[x as usize].0)).collect();
                let mut ys: Vec<_> = ids.iter().map(|&x| key(&pairs[x as usize].1)).collect();
                xs.sort_unstable();
                xs.dedup();
                ys.sort_unstable();
                ys.dedup();
                if xs.len() == 1 {
                    x_const += 1;
                }
                if ys.len() == 1 {
                    y_const += 1;
                }
                if xs.len() * ys.len() == ids.len() {
                    product += 1;
                }
                xsets.insert(xs);
                ysets.insert(ys);
                sets.insert(ids);
                i = j;
            }
            println!("frame {frame}: {n_edges} edges, {n_pairs} (source, target) pairs, {:.2} transfers a pair, {dup} duplicate edges", n_edges as f64 / n_pairs as f64);
            println!("transfers per pair: {hist:?}");
            println!("pairs whose transfers are exactly x-pieces x y-pieces: {product} ({:.1}%); x part constant {x_const} ({:.1}%), y part constant {y_const} ({:.1}%)", 100.0 * product as f64 / n_pairs as f64, 100.0 * x_const as f64 / n_pairs as f64, 100.0 * y_const as f64 / n_pairs as f64);
            println!("distinct transfer sets {}, x sets {}, y sets {} (the tree's table: {} pairs)", sets.len(), xsets.len(), ysets.len(), pairs.len());
        }
        Command::ColCensus { level_dir, frame, cap, cell, erase, every } => {
            use celeste_engine::exact::Code;
            use celeste_engine::runtime2::{Cell2, Col, AV};
            let only_cell = cell.map(cell_at).transpose()?;
            let dir = std::path::Path::new(&level_dir);
            let tree = tree_of(dir)?;
            let ids = celeste_rust::compiled::ids();
            // shape -> (rows, per-column sets, player field names, spd pairs)
            struct S {
                rows: u64,
                cols: Vec<rustc_hash::FxHashSet<Code>>,
                varying: Vec<bool>,
                names: std::collections::HashMap<usize, String>,
                spd: rustc_hash::FxHashSet<(Code, Code)>,
                /// Per column: rows holding an interval, rows holding an unknown bool.
                ivals: Vec<u64>,
                ubools: Vec<u64>,
            }
            let mut by_shape: std::collections::BTreeMap<u64, S> = Default::default();
            let geo = celeste_rust::storage::GEOMETRY;
            // The frame in 1M-row pieces, or one cell's rows.
            for (_, rt2) in tree.rows(frame, &cfg, 1 << 20, &|x| only_cell.is_none_or(|c| x.cell(&geo) == c), cfg.threads())? {
                {
                    let st = by_shape.entry(rt2.shape_hash).or_insert_with(|| {
                        let names = cell_names(&rt2, ids);
                        S { rows: 0, cols: vec![Default::default(); rt2.cols.len()], varying: vec![false; rt2.cols.len()], names, spd: Default::default(), ivals: vec![0; rt2.cols.len()], ubools: vec![0; rt2.cols.len()] }
                    });
                    st.rows += rt2.width as u64;
                    for (c, col) in rt2.cols.iter().enumerate() {
                        if !matches!(rt2.structure[c], Cell2::Val) {
                            continue;
                        }
                        match col {
                            Col::U(v) => {
                                if st.cols[c].len() < cap {
                                    st.cols[c].insert(Code::of(*v));
                                }
                            }
                            _ => {
                                st.varying[c] = true;
                                let set = &mut st.cols[c];
                                for r in 0..rt2.width {
                                    let v = col.at(r);
                                    match v {
                                        AV::Ival(a, b) if a != b => st.ivals[c] += 1,
                                        AV::UBool => st.ubools[c] += 1,
                                        _ => {}
                                    }
                                    if set.len() < cap {
                                        set.insert(Code::of(v));
                                    }
                                }
                            }
                        }
                    }
                    let named = |want: &str| st.names.iter().find(|(_, n)| n.as_str() == want).map(|(c, _)| *c);
                    if let (Some(sx), Some(sy)) = (named("spd.x"), named("spd.y")) {
                        for r in 0..rt2.width {
                            if st.spd.len() >= cap {
                                break;
                            }
                            st.spd.insert((Code::of(rt2.cols[sx].at(r)), Code::of(rt2.cols[sy].at(r))));
                        }
                    }
                }
            }
            for (shape, st) in &by_shape {
                let mut cards: Vec<(usize, usize)> = (0..st.cols.len()).filter(|&c| st.cols[c].len() > 1 || st.varying[c]).map(|c| (st.cols[c].len(), c)).collect();
                cards.sort_unstable_by(|a, b| b.cmp(a));
                let product: f64 = cards.iter().map(|&(n, _)| n as f64).product();
                println!("shape {shape:#x}: {} rows, {} varying columns, cardinality product {:.2e}, spd pairs {}", st.rows, cards.len(), product, st.spd.len());
                for &(n, c) in cards.iter().take(24) {
                    println!("  c{c:<4} {n:>10} {:<18} intervals {:>10} unknown-bools {:>10}", st.names.get(&c).map(|s| s.as_str()).unwrap_or(""), st.ivals[c], st.ubools[c]);
                    // One cell's census lists the numbers themselves.
                    if only_cell.is_some() && n <= 64 {
                        let mut vals: Vec<f64> = st.cols[c].iter().filter(|v| v.kind == 0 && v.a == v.b).map(|v| v.a as i32 as f64 / 65536.0).collect();
                        vals.sort_by(|a, b| a.total_cmp(b));
                        if !vals.is_empty() {
                            println!("        {}", vals.iter().map(|v| format!("{v:.4}")).collect::<Vec<_>>().join(" "));
                        }
                    }
                }
            }
            // Does the position saturate?
            if let Some(want) = only_cell {
                let erase = prefixes(&erase);
                let (mut all, mut spd, mut inner) = (rustc_hash::FxHashSet::<Proj>::default(), rustc_hash::FxHashSet::<Proj>::default(), rustc_hash::FxHashSet::<Proj>::default());
                let mut first: Option<u32> = None;
                println!("step | new | distinct states | speed pairs | inner (no speed)");
                for s in 0..=frame {
                    let mut new = 0u64;
                    for (_, rt2) in tree.rows(s, &cfg, 1 << 20, &|x| x.cell(&geo) == want, cfg.threads())? {
                        let names = cell_names(&rt2, ids);
                        for r in 0..rt2.width {
                            let mut p = project_row(&rt2, &names, r as u32, &erase);
                            new += 1;
                            all.insert(p.clone());
                            let sp: Proj = ["spd.x", "spd.y"].iter().map(|k| (k.to_string(), p.remove(*k).unwrap_or_default())).collect();
                            spd.insert(sp);
                            inner.insert(p);
                        }
                    }
                    if new > 0 && first.is_none() {
                        first = Some(s);
                    }
                    if first.is_some() && (s % every == 0 || s == frame) {
                        println!("{s:>4} | {new:>8} | {:>10} | {:>6} | {:>8}", all.len(), spd.len(), inner.len());
                    }
                }
            }
        }
        Command::ExportUi { checkpoint_dir, out, arc, witness, reference, download, reach, reach_label } => {
            celeste_rust::search::ui_export::export(
                std::path::Path::new(&checkpoint_dir),
                std::path::Path::new(&out),
                &cfg,
                arc.as_deref().map(std::path::Path::new),
                celeste_rust::search::ui_export::Paths {
                    witness: witness.as_deref().map(std::path::Path::new),
                    reference: reference.as_deref().map(std::path::Path::new),
                    download: download.as_deref().map(std::path::Path::new),
                    reach: reach.as_deref().map(std::path::Path::new),
                    reach_label: reach_label.as_deref(),
                },
            )?;
        }
        Command::Room => {
            let cart = cfg.cart_data()?;
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
                    let rt2 = tree.row(layer, sk, &cfg)?;
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
        Command::L1Check { inputs, horizon, spd } => {
            let bytes = celeste_rust::concrete::read_inputs(&inputs)?;
            let threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
            let owned = cfg.clone();
            let table = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || celeste_rust::trace::level_minus_one::cost_to_go(std::path::Path::new("."), spd, threads, &owned))?
                .join()
                .map_err(|_| anyhow::anyhow!("the level -1 builder panicked"))??;
            println!("[l1-check] table: {} entries, start d {}, fingerprint {:016x}", table.len(), table.start_d, table.fingerprint());
            let mut eng = RefEngine::new(&cfg)?;
            // Each leaf with its lineage's states too late (an `rnd` leaf
            // that never exits may rightly be too late: only an exiting
            // lineage is the solution).
            let mut states: Vec<(Rt2, Vec<u32>)> = vec![(eng.initial()?, Vec::new())];
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
                        if wins_of(b.rt2(), &cfg)?.iter().any(|&x| x) {
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
        }
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

/// `rewrite reach` (search::reach): the run, its table and its outputs.
#[allow(clippy::too_many_arguments)]
fn reach_command(checkpoint_dir: &str, level_index: usize, to: u32, save: Option<&str>, cells_out: Option<&str>, decode_rect: Option<&str>, decode_max: usize, route: Option<&str>, opts: celeste_rust::search::reach::ReachOpts, cfg: &celeste_rust::settings::Settings) -> Result<()> {
    use celeste_rust::search::pos_graph::cell_xy;
    use std::io::Write;
    let dir = std::path::Path::new(checkpoint_dir).join(format!("level{level_index:02}"));
    let steps = to * cfg.steps_per_frame();
    let tree = Tree::open(&dir, steps)?;
    let level = tree_level(&dir)?;
    cfg.check(&dir, level)?;
    let geo = celeste_rust::storage::GEOMETRY;
    let rect = decode_rect
        .map(|r| {
            let (x, y) = r.split_once(',').ok_or_else(|| anyhow::anyhow!("--decode-rect x0..x1,y0..y1"))?;
            let range = |t: &str| -> Result<(i32, i32)> {
                let (a, b) = t.split_once("..").ok_or_else(|| anyhow::anyhow!("--decode-rect x0..x1,y0..y1"))?;
                Ok((a.trim().parse()?, b.trim().parse()?))
            };
            Ok::<_, anyhow::Error>((range(x)?, range(y)?))
        })
        .transpose()?;
    let mut cells_file = cells_out.map(|p| std::fs::File::create(p).map(std::io::BufWriter::new)).transpose()?;
    let has_notes = dir.join("dropped").is_dir();
    // `--route`: each route's state and exact remainder per frame (until
    // a death).
    type At = Option<(celeste_rust::storage::Sk, (u32, u32), (i32, i32))>;
    let mut routes: Vec<Vec<At>> = Vec::new();
    let specs: Vec<String> = match route {
        None => Vec::new(),
        Some(spec) if std::path::Path::new(spec).is_file() => std::fs::read_to_string(spec)?
            .lines()
            .filter(|l| !l.trim().is_empty() && !l.starts_with('#'))
            .map(|l| l.split_once("inputs ").map_or(l, |x| x.1).trim().to_string())
            .collect(),
        Some(spec) => vec![spec.to_string()],
    };
    for spec in &specs {
        use celeste_rust::trace::refengine::RefEngine;
        let mut route_at: Vec<At> = Vec::new();
        let inputs = celeste_rust::concrete::read_inputs(spec)?;
        let mut eng = RefEngine::new(cfg)?;
        let mut row = celeste_rust::frame::Block::canonical(eng.initial()?)?;
        route_at.push(None);
        let mut seen_player = false;
        for &b in inputs.iter().take(steps as usize) {
            row = eng.step_one(row.rt2(), b)?;
            let player = !row.rt2().player_objects(celeste_rust::compiled::ids()).is_empty();
            if seen_player && !player {
                break;
            }
            seen_player |= player;
            if !player {
                route_at.push(None);
                continue;
            }
            let (shape, keys, cells) = celeste_rust::frame::widened_keys(&row, level, &tree.space)?;
            let si = tree.shapes.iter().position(|&h| h == shape);
            let (Some(si), Some(k)) = (si, keys[0]) else {
                anyhow::bail!("--route: frame {}: the route's state has no node in the tree (shape or key unknown to it)", route_at.len());
            };
            let sk = celeste_rust::storage::Sk::at(&geo, si as u32, k, cells[0]);
            route_at.push(Some((sk, celeste_rust::search::arc_dp::rem_of(row.rt2())?, cell_xy(cells[0]).unwrap_or((-1, -1)))));
        }
        routes.push(route_at);
    }
    if !routes.is_empty() {
        println!("[reach] --route: {} routes, {} frames to check", routes.len(), routes.iter().flatten().filter(|r| r.is_some()).count());
    }
    let mut route_lost: Option<(usize, u32)> = None;
    let point = celeste_rust::search::arc_dp::start_point(cfg)?;
    let t0 = std::time::Instant::now();
    // Written frame by frame (a run stopped part way keeps them): the
    // frames' sums (`reach.tsv`) and the reached sets' shapes
    // (`rem_stats.tsv`).
    const TSV_HEAD: &str = "frame\tstates\tnew\tentered\tentered_edges\treached_edges\tpushes\tpushes_nonempty\tpoints\tcells\tforward_layer\tcoarsened\tinexact\tmax_k\tx_min\tx_max\ty_min\ty_max\tl1_noted_sources\n";
    let save_dir = save.map(std::path::Path::new);
    if let Some(d) = save_dir {
        std::fs::create_dir_all(d)?;
    }
    let mut tsv_file = save_dir.map(|d| std::fs::File::create(d.join("reach.tsv")).map(std::io::LineWriter::new)).transpose()?;
    let mut rem_file = save_dir.map(|d| std::fs::File::create(d.join("rem_stats.tsv")).map(std::io::LineWriter::new)).transpose()?;
    if let Some(f) = &mut tsv_file {
        f.write_all(TSV_HEAD.as_bytes())?;
    }
    if let Some(f) = &mut rem_file {
        f.write_all(b"frame\taxis\tof\tbucket\tstates\texact_states\n")?;
    }
    let mut tsv = String::from(TSV_HEAD);
    let pool = celeste_rust::par::Pool::new(cfg.threads());
    let r = celeste_rust::search::reach::reach(&dir, steps, point, cfg.threads(), opts, |fr, pushes, cur, inexact| {
        let t = fr.frame;
        for (ri, route_at) in routes.iter().enumerate() {
            if let Some(Some((sk, q, xy))) = route_at.get(t as usize) {
                let hit = cur.binary_search_by_key(sk, |c| c.0).ok().map(|i| cur[i].1.contains(q.0, q.1));
                if hit != Some(true) {
                    println!("[reach] ROUTE {ri} LOST at f{t} (player {xy:?}): {}", if hit.is_none() { "its state is not reached" } else { "its remainder is outside the state's set" });
                    route_lost.get_or_insert((ri, t));
                }
            }
        }
        let shapes = rem_shapes(t, cur, inexact, &pool)?;
        if let Some(f) = &mut rem_file {
            f.write_all(shapes.as_bytes())?;
        }
        let (mut x0, mut x1, mut y0, mut y1) = (i32::MAX, i32::MIN, i32::MAX, i32::MIN);
        let mut per_cell: rustc_hash::FxHashMap<u32, u64> = Default::default();
        for (s, _) in cur {
            let c = s.cell(&geo);
            *per_cell.entry(c).or_default() += 1;
            if let Some((x, y)) = cell_xy(c) {
                (x0, x1, y0, y1) = (x0.min(x), x1.max(x), y0.min(y), y1.max(y));
            }
        }
        if let Some(f) = &mut cells_file {
            let mut v: Vec<(u32, u64)> = per_cell.into_iter().collect();
            v.sort_unstable();
            for (c, n) in v {
                if let Some((x, y)) = cell_xy(c) {
                    writeln!(f, "{t} {x} {y} {n}")?;
                }
            }
        }
        // The forward's states of layer `t` a level -1 drop touched (some
        // successor at `t + 1` dropped as too late): a reached state from
        // an earlier layer has its notes at its own layer, so they are not
        // matched against the reach. A dropped successor of a state reached
        // at `t` is no earlier than `t + 1`: too late for the tree's win.
        let notes_path = celeste_rust::storage::layer::notes_path(&dir, t + 1);
        let noted = if has_notes && notes_path.exists() { celeste_rust::storage::layer::read_notes(&notes_path)?.iter().map(|n| n.3.count_ones() as u64).sum() } else { 0 };
        let p = pushes.unwrap_or_default();
        let line = format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
            t, fr.states, fr.new, fr.entered, fr.entered_edges, fr.reached_edges, p.edges, p.nonempty, fr.points, fr.cells, fr.forward_layer, fr.coarsened, fr.inexact, fr.max_k, x0, x1, y0, y1, noted
        );
        if let Some(f) = &mut tsv_file {
            f.write_all(line.as_bytes())?;
        }
        tsv += &line;
        if let Some(((rx0, rx1), (ry0, ry1))) = rect {
            let inside: Vec<_> = cur
                .iter()
                .filter(|(s, _)| cell_xy(s.cell(&geo)).is_some_and(|(x, y)| (rx0..=rx1).contains(&x) && (ry0..=ry1).contains(&y)))
                .take(decode_max)
                .collect();
            for (s, reg) in inside {
                let layer = (0..=t).find(|&f| tree.mask_at(f, s.region, s.key).is_ok_and(|m| m >> s.local & 1 == 1));
                let Some(layer) = layer else { continue };
                let row = tree.row(layer, *s, cfg)?;
                let named = project_all(&row).swap_remove(0);
                let keep = ["player.x", "player.y", "spd.x", "spd.y", "rem.x", "rem.y", "dash_effect_time"];
                let mut text: Vec<String> = named.iter().filter(|(k, _)| keep.contains(&k.as_str()) || k.ends_with(".djump") || k.ends_with(".dash_time") || k.ends_with(".grace") || (k.starts_with("balloon") && (k.ends_with(".spr") || k.ends_with(".timer")))).map(|(k, v)| format!("{k}={v}")).collect();
                text.push(format!("rem-points={}", celeste_rust::search::reach::points(reg)));
                if let Some(k) = inexact.get(s) {
                    text.push(format!("INEXACT(2^{k} grid)"));
                }
                println!("[reach] f{t:03} in the rect: {}", text.join(" "));
            }
        }
        Ok(())
    })?;
    println!("[reach] {} to f{to}, {:.1} s", dir.display(), t0.elapsed().as_secs_f64());
    if !routes.is_empty() {
        match route_lost {
            None => println!("[reach] --route: every route's state reached at every frame with its exact remainder"),
            Some((ri, t)) => anyhow::bail!("--route: route {ri} is LOST at f{t} (a real state the reach excludes: a bug, unless the route passed a win)"),
        }
    }
    print!("{tsv}");
    // Against the forward: its visited states and its emissions (records)
    // over the same frames.
    let fwd = std::fs::read_to_string(dir.join("forward.tsv")).unwrap_or_default();
    let mut lines = fwd.lines();
    let head: Vec<&str> = lines.next().unwrap_or("").split('\t').collect();
    let col = |name: &str| head.iter().position(|h| *h == name);
    let (mut visited, mut expanded, mut emitted) = (0u64, 0u64, 0u64);
    if let (Some(vf), Some(ve), Some(vv)) = (col("frame"), col("edges"), col("visited")) {
        for l in lines {
            let c: Vec<&str> = l.split('\t').collect();
            let f: u32 = c[vf].parse()?;
            if f <= steps {
                emitted += c[ve].parse::<u64>()?;
                visited = visited.max(c[vv].parse()?);
            }
            if f < steps {
                expanded = expanded.max(c[vv].parse()?);
            }
        }
    }
    let live = r.last.len() as u64;
    let entered: u64 = r.frames.iter().map(|f| f.entered).sum();
    let entered_edges: u64 = r.frames.iter().map(|f| f.entered_edges).sum();
    let reached: u64 = r.frames.iter().filter(|f| f.frame < steps).map(|f| f.states).sum();
    let reached_edges: u64 = r.frames.iter().map(|f| f.reached_edges).sum();
    let e = &r.entries;
    let range = |a: usize, b: usize| e.iter().skip(a).take(b.saturating_sub(a)).sum::<u64>();
    let mut summary = String::new();
    summary += &format!("[reach] states ever reached: {live} (the forward visited {visited}: {:.2}%)\n", 100.0 * live as f64 / visited.max(1) as f64);
    if opts.first_arrival {
        summary += &format!(
            "[reach] first arrival (a lower bound: unions coarsened past 32 segments): entries per state 1: {}, 2: {}, 3-5: {}, 6+: {}; {entered} expansions against the forward's {expanded} ({:.2}%), {entered_edges} distinct out-edges pushed (the forward emitted {emitted} lanes)\n",
            range(1, 2),
            range(2, 3),
            range(3, 6),
            range(6, usize::MAX),
            100.0 * entered as f64 / expanded.max(1) as f64
        );
    }
    summary += &format!(
        "[reach] the upper bound, a state entering at every frame it is reached: {reached} expansions ({:.2}%), {reached_edges} distinct out-edges pushed\n",
        100.0 * reached as f64 / expanded.max(1) as f64
    );
    summary += &format!(
        "[reach] edges of the states ever reached: {} distinct (target, transfer) of {} recorded, {} of those ever pushed non-empty\n",
        r.edges,
        r.edges_raw,
        r.edges_pushed
    );
    print!("{summary}");
    if let Some(d) = save {
        let d = std::path::Path::new(d);
        std::fs::create_dir_all(d)?;
        std::fs::write(d.join("summary.txt"), &summary)?;
        let rows: Vec<celeste_rust::frame::MarkRow> = r
            .last
            .iter()
            .map(|(s, &l)| celeste_rust::frame::mark_row(tree.shapes[s.shape(&geo) as usize], s.key, s.cell(&geo), l.min(u16::MAX as u32) as u16, steps))
            .collect();
        celeste_rust::frame::save_marks(&d.join("reach.marks.bin"), rows, steps, &tree.space)?;
        println!("[reach] {} reached states saved to {}", r.last.len(), d.join("reach.marks.bin").display());
    }
    Ok(())
}

/// One frame's reached sets by shape, as `rem_stats.tsv` rows: per axis
/// (`x`, `y`: the set's projection) the number of segments and the measure
/// (points of the 2^16 circle), and the measure of the whole set (`xy`, of
/// 2^32), each bucketed; every state and the exact ones apart.
fn rem_shapes(t: u32, cur: &[(celeste_rust::storage::Sk, celeste_rust::search::arcs::Region)], inexact: &rustc_hash::FxHashMap<celeste_rust::storage::Sk, u8>, pool: &celeste_rust::par::Pool) -> Result<String> {
    use celeste_rust::search::arcs::Seg;
    const SEGS: [&str; 5] = ["1", "2-4", "5-64", "65-1024", "1025+"];
    const MEASURE: [&str; 6] = ["point", "<=1/256", "<=1/16", "<=1/2", "<full", "full"];
    let segs_bucket = |n: usize| match n {
        0 | 1 => 0,
        2..=4 => 1,
        5..=64 => 2,
        65..=1024 => 3,
        _ => 4,
    };
    let measure_bucket = |p: u64, full: u64| match p {
        _ if p >= full => 5,
        0 | 1 => 0,
        _ if p <= full >> 8 => 1,
        _ if p <= full >> 4 => 2,
        _ if p <= full >> 1 => 3,
        _ => 4,
    };
    // [axis x, y][segs | measure][bucket][all, exact], and xy's measure.
    type Counts = ([[[[u64; 2]; 6]; 2]; 2], [[u64; 2]; 6]);
    const CHUNK: usize = 4096;
    let parts = pool.map_with(
        cur.len().div_ceil(CHUNK),
        || Ok(Vec::<Seg>::new()),
        |xs, c| {
            let mut n: Counts = Default::default();
            for (s, r) in &cur[c * CHUNK..((c + 1) * CHUNK).min(cur.len())] {
                let k = !inexact.contains_key(s) as usize;
                // A projection's (segments, points): merged where they touch.
                let proj = |v: &mut Vec<Seg>| -> (usize, u64) {
                    v.sort_unstable_by_key(|s| s.lo);
                    let (mut n, mut p, mut hi) = (0usize, 0u64, None::<u32>);
                    for s in v.iter() {
                        match hi {
                            Some(h) if s.lo <= h => {
                                if s.hi > h {
                                    p += (s.hi - h) as u64;
                                    hi = Some(s.hi);
                                }
                            }
                            _ => {
                                n += 1;
                                p += (s.hi - s.lo) as u64;
                                hi = Some(s.hi);
                            }
                        }
                    }
                    (n, p)
                };
                xs.clear();
                let mut ys: Vec<Seg> = Vec::new();
                for (y, x) in r.slabs() {
                    ys.push(y);
                    xs.extend(x);
                }
                for (a, v) in [(0, &mut *xs), (1, &mut ys)] {
                    let (segs, p) = proj(v);
                    n.0[a][0][segs_bucket(segs)][0] += 1;
                    n.0[a][1][measure_bucket(p, 1 << 16)][0] += 1;
                    n.0[a][0][segs_bucket(segs)][1] += k as u64;
                    n.0[a][1][measure_bucket(p, 1 << 16)][1] += k as u64;
                }
                let b = measure_bucket(celeste_rust::search::reach::points(r), 1 << 32);
                n.1[b][0] += 1;
                n.1[b][1] += k as u64;
            }
            Ok(n)
        },
    )?;
    let mut n: Counts = Default::default();
    for p in parts {
        for a in 0..2 {
            for m in 0..2 {
                for b in 0..6 {
                    for k in 0..2 {
                        n.0[a][m][b][k] += p.0[a][m][b][k];
                    }
                }
            }
        }
        for b in 0..6 {
            for k in 0..2 {
                n.1[b][k] += p.1[b][k];
            }
        }
    }
    let mut out = String::new();
    for (a, axis) in ["x", "y"].iter().enumerate() {
        for (b, name) in SEGS.iter().enumerate() {
            if n.0[a][0][b][0] > 0 {
                out += &format!("{t}\t{axis}\tsegments\t{name}\t{}\t{}\n", n.0[a][0][b][0], n.0[a][0][b][1]);
            }
        }
        for (b, name) in MEASURE.iter().enumerate() {
            if n.0[a][1][b][0] > 0 {
                out += &format!("{t}\t{axis}\tmeasure\t{name}\t{}\t{}\n", n.0[a][1][b][0], n.0[a][1][b][1]);
            }
        }
    }
    for (b, name) in MEASURE.iter().enumerate() {
        if n.1[b][0] > 0 {
            out += &format!("{t}\txy\tmeasure\t{name}\t{}\t{}\n", n.1[b][0], n.1[b][1]);
        }
    }
    Ok(out)
}

/// The level a tree was widened at (its settings record).
fn tree_level(dir: &std::path::Path) -> Result<Level> {
    let rec = std::fs::read_to_string(celeste_rust::settings::path(dir)).with_context(|| format!("{}: no settings record", dir.display()))?;
    let spec = rec.lines().find_map(|l| l.strip_prefix("level ")).ok_or_else(|| anyhow::anyhow!("{}: the record names no level", dir.display()))?;
    Level::parse(spec).map_err(|e| anyhow::anyhow!(e))
}
