//! The search driver: `rewrite search` (the level-0 forward, the rotation
//! graph's backward and the concrete search), `rewrite forward` (one
//! forward pass with timing), `rewrite ckhash` (checkpoint fingerprints),
//! `rewrite export-ui` (the web UI's data), and the diagnostics that read a
//! finished tree (rows by name: `search::inspect`).

use anyhow::Result;
use celeste_engine::runtime2::Rt2;
use celeste_rust::concrete::{start_states, ConcreteEngine};
use celeste_rust::frame::{forward_frame, frame_files, frame_paths, id_layer, id_row, id_seq, load_row, pack_id, widen_rt2_to, widened_keys, wins_of, Block, FrameStep, Visited};
use celeste_rust::interpreter::abstraction::{set_level, Level};
use celeste_rust::search::inspect::{brief, cell_names, parse_xy, player_summary, project_all, project_onto, project_row, projection_key, Proj};
use clap::{Parser, Subcommand};

/// mimalloc, not glibc: glibc retained ~23 GB of freed slot chunks across
/// its arenas at room (0,0) f90 (RSS 44 GB against ~21 GB live); mimalloc
/// runs the same frame at 24.8 GB and the same speed (plans/memory.md).
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// On DISK, not the tmpfs at `/tmp` (a full-room tree is gigabytes, and the
/// per-block version of it once exhausted /tmp's inodes, 2026-09-07).
const DEFAULT_CHECKPOINT_DIR: &str = "/var/tmp/celeste-checkpoints";

#[derive(Parser)]
#[command(about = "The abstract TAS search")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// THE SEARCH (the arc pipeline), per level of `--level` (coarsest
    /// first): the forward to the horizon, recording every edge with its
    /// remainder transfer (resumed, or reused as it is when the tree already
    /// reaches the horizon), filtered by the previous level's arc-marked
    /// nodes; the rotation graph's winning sets backward from the wins
    /// (`arc_dp::solve`), whose optimum is exact in the remainder and a LOWER
    /// BOUND on the game's (the level's objects may be coarse) - no win
    /// refutes the horizon; then the concrete search inside the winning
    /// sets: a depth-first try at the bound (a win there is the optimum),
    /// and at the LAST level the whole search. The first concrete win is the
    /// CONCRETE optimum, its inputs written to
    /// `<checkpoint-dir>/witness_frame_F.txt`.
    Search {
        /// The horizon: every frame up to it is searched.
        #[arg(long)]
        to: Option<u32>,
        /// A KNOWN solution's frame (the replayed community TAS) as the
        /// horizon: a refutation, or no concrete witness by it, is an error -
        /// the model cannot reproduce a real run.
        #[arg(long)]
        ceiling: Option<u32>,
        /// The levels (`Level::parse`), comma-separated, coarsest first:
        /// `r0sx` for a room without objects, `r0sxhn` with objects,
        /// `r0sxhn,r0sxh` for the objects ladder (an exact-objects level
        /// filtered by the abstract one: room (7,0)).
        #[arg(long, default_value = "r0sx")]
        level: String,
        /// The checkpoint dir: level i's tree goes to `level{i:02}/` under it.
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// Start room "x,y".
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Optional synthetic win "x,y" (CELESTE_WIN_AT_XY) for a cheap run.
        #[arg(long)]
        win_at: Option<String>,
        /// Stop at the arc bound (no concrete search).
        #[arg(long)]
        no_witness: bool,
        /// Write the web UI's arc pass into this directory (`export-ui
        /// --forward-only --arc DIR`): the remainder-free backward's marks
        /// (`level0.marks.bin`), the ARC-MARKED nodes (`arc.marks.bin`: a node
        /// whose winning set is non-empty at some frame), both as `(shape,
        /// cell, key, dist)` rows with `dist` the horizon minus the last frame
        /// the node still wins from; `arc.txt`; `witness.txt` (the inputs and
        /// the player's position per frame).
        #[arg(long)]
        save_marks: Option<String>,
    },
    /// ONE forward pass at ONE level, exactly as the search runs it
    /// (record mode, position partition, sharded checkpoints), with the
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
        /// Start room "x,y".
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Run the REFERENCE engine (the interpreter, one lane per fork leaf)
        /// instead of the kernels: `ckhash` of the two trees must agree. Slow;
        /// for a new room's first frames. Not at held-unknown levels.
        #[arg(long)]
        reference: bool,
    },
    /// DIAGNOSTIC: is a coarser level's tree closed over a finer one's rows
    /// (is the coarse level sound)? Every row of the FINE tree at frames `from..=to`
    /// (with `--fine-marks`, only its marked rows), projected onto `level`
    /// (`frame::widened_keys`, the key the filter looks up), must be a row of
    /// the COARSE tree at that frame or before (the door keeps a state at the
    /// first frame it is reached) - and with `--coarse-marks`, a marked one.
    /// Per frame: the fine rows, the projections the coarse tree has not
    /// reached (a forward loss) and those reached but unmarked (a backward
    /// loss); then the first misses' cells.
    DiagProject {
        #[arg(long)]
        fine_dir: String,
        #[arg(long)]
        coarse_dir: String,
        /// The coarse level.
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        #[arg(long, default_value_t = 1)]
        from: u32,
        #[arg(long)]
        to: u32,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// The fine level's marks (`hNNN/levelNN.marks.bin`): only its
        /// marked rows are projected.
        #[arg(long)]
        fine_marks: Option<String>,
        /// The coarse level's marks: a reached projection must be marked.
        #[arg(long)]
        coarse_marks: Option<String>,
    },

    /// Fingerprint a forward checkpoint tree: per frame, the lane count and an
    /// order-independent hash of its (row key, cell) set. Two runs that agree
    /// here reached the same states; the format they stored them in is
    /// irrelevant.
    Ckhash {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// Last frame to fingerprint.
        #[arg(long)]
        to: u32,
        /// Start room "x,y" (the cell numbering depends on it).
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Also list the N most populated player positions at frame `to`
        /// (to pick a synthetic `--win-at` target with real fan-out).
        #[arg(long)]
        cells: Option<usize>,
    },
    /// Microbenchmark of ONE forward frame: load a checkpointed frame of a
    /// level-0 tree and run `forward_frame` on it `reps` times (empty
    /// visited set, no filter, no pos-graph), printing the per-rep phase
    /// times. Isolates the kernel + append loop from the search around it.
    BenchFrame {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        #[arg(long, default_value_t = 70)]
        frame: u32,
        #[arg(long, default_value_t = 3)]
        reps: usize,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// A level dir (`frames/` under it) instead of `<checkpoint_dir>/level00`.
        #[arg(long)]
        level_dir: Option<String>,
        /// The level to run at.
        #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
        level: Level,
        /// Record the frame's edges (into `<level dir>/bench-edges`, compacted
        /// and deleted per rep, both timed) - the search's forward path.
        #[arg(long, default_value_t = false)]
        edges: bool,
    },
    /// DIAGNOSTIC: the TRANSFERS of a level-0 tree's edges, checked. Per
    /// frame: every recorded edge carries a transfer. Then `--samples` records
    /// per frame, each from one of its preds: the source row stepped by the
    /// REFERENCE engine (all 64 inputs) at remainders INSIDE the record's
    /// guard - some input must reach the target (projected onto `--level`)
    /// with the remainder the action predicts - and just OUTSIDE it, at
    /// points no record of that (pred, target) covers - no input may reach
    /// the target there. Fails (exit != 0) on any disagreement.
    ArcCheck {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// A synthetic win "x,y" (CELESTE_WIN_AT_XY), as the tree was run.
        #[arg(long)]
        win_at: Option<String>,
        #[arg(long, default_value_t = 1)]
        from: u32,
        #[arg(long)]
        to: u32,
        #[arg(long, default_value_t = 20)]
        samples: usize,
        /// The tree's level (what a successor is projected onto).
        #[arg(long, value_parser = Level::parse, default_value = "r0sx")]
        level: Level,
    },
    /// DIAGNOSTIC: how coarse a level could be. Per frame, the distinct
    /// states of a tree with the named cells (`inspect::cell_names`) whose
    /// names start with any `--erase` prefix left out (comma-separated, e.g.
    /// `spd.,rem.,player[3].dash`), and cumulatively through that frame.
    CoarseCensus {
        #[arg(long)]
        level_dir: String,
        #[arg(long)]
        from: u32,
        #[arg(long)]
        to: u32,
        #[arg(long, default_value = "")]
        erase: String,
    },
    /// DIAGNOSTIC: does a cell's visited set saturate? A frame's rows are
    /// the states NEW at it (the door dedupes across frames), so a cell's
    /// visited set is the sum of its rows over frames. Reads the checkpoint
    /// cell indexes only: the `top` cells by visited count (and `--at x,y`)
    /// with their new states per frame, and every cell's new states at `to`
    /// as a share of its visited set.
    CellGrowth {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 0)]
        from: u32,
        #[arg(long)]
        to: u32,
        #[arg(long, default_value_t = 8)]
        top: usize,
        #[arg(long, value_parser = parse_xy)]
        at: Option<(i32, i32)>,
        /// Also the growth by AGE: for every age `a` (frames since a cell's
        /// first state, a multiple of this step), over the cells reached at
        /// least `a` frames before `to`, the median, 90th percentile and mean
        /// of the states a cell had visited by that age - comparable across
        /// rooms, where the per-frame totals are not.
        #[arg(long)]
        by_age: Option<usize>,
    },
    /// THE KERNELS AGAINST THE REFERENCE ENGINE, row by row: `samples` stored
    /// rows first reached at step `frame` (in player cell `--cell x,y` if
    /// given) each run one step through the compiled kernels of `--level` and
    /// through the reference engine (`RefEngine`, the interpreter enumerating
    /// every fork path, unknown inputs forked per path), both sides projected
    /// onto the level (`frame::widen_rt2_to`) and compared by named fields.
    /// A reference successor the kernels miss is a SOUNDNESS gap; a kernel
    /// successor the reference never makes is a PRECISION gap (room (7,0)'s
    /// floor-collision join, `7f6b96e`).
    RefCheck {
        #[arg(long)]
        level_dir: String,
        #[arg(long)]
        frame: u32,
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        #[arg(long, default_value_t = 20)]
        samples: usize,
        #[arg(long, value_parser = parse_xy)]
        cell: Option<(i32, i32)>,
        /// Show up to this many differing successors per row and side.
        #[arg(long, default_value_t = 3)]
        show: usize,
    },
    /// DIAGNOSTIC: re-run ONE stored state through the kernels of `--level`
    /// and print every successor (projected without `--erase`), next to the
    /// state itself: what the kernel makes of a single row. `--row L:S:R` is
    /// the row's (layer, seq, row) in the tree's frame files.
    RerunRow {
        #[arg(long)]
        level_dir: String,
        #[arg(long)]
        row: String,
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        #[arg(long, default_value = "fall_floor")]
        erase: String,
    },
    /// DIAGNOSTIC: where a tree's states come from. `samples` states of
    /// `--coarse` first reached at `step` in the player cell(s) `--cell`
    /// are walked back through the recorded edges, `depth` steps, printing
    /// at each the predecessor's cell and what changed across the step
    /// (every named value cell but the `--erase` prefixes).
    ///
    /// With `--real` (a finer tree), which states are SPURIOUS and where
    /// they first went wrong: a coarse state whose projection the real tree
    /// never reached in its cell by `step` is spurious; the per-field value
    /// distributions are printed against the real ones', the samples are
    /// spurious states, and each walk stops at the first predecessor whose
    /// projection the real tree did reach - the first spurious transition.
    Spurious {
        #[arg(long)]
        coarse: String,
        #[arg(long)]
        real: Option<String>,
        #[arg(long)]
        step: u32,
        /// The player cell `x,y`, or a rect `x0,x1,y0,y1` of cells (inclusive).
        #[arg(long)]
        cell: String,
        #[arg(long, default_value = "fall_floor")]
        erase: String,
        #[arg(long, default_value_t = 8)]
        samples: usize,
        #[arg(long, default_value_t = 40)]
        depth: u32,
        /// Write sample 0's chain, frame 1 to `step`, as a keyed trajectory
        /// (`trajectory --trajectory FILE --spec LEVEL` realizes it). Not
        /// with `--real`.
        #[arg(long)]
        chain_out: Option<String>,
    },
    /// DIAGNOSTIC: per-column cardinalities of one frame, streamed file by
    /// file and row range by row range. Per shape: rows, then every varying
    /// column's distinct value count (capped at `cap`), named
    /// (`inspect::cell_names`), and the distinct (spd.x, spd.y) pairs.
    ///
    /// With `--cell x,y`, only that position's rows (the values themselves
    /// listed), and whether the position SATURATES: over steps 0..=`frame`
    /// every `every` steps, its cumulative distinct states (projected
    /// without the `--erase` prefixes), distinct (spd.x, spd.y) pairs, and
    /// distinct "inner" states (the projection without speed).
    ColCensus {
        #[arg(long)]
        level_dir: String,
        #[arg(long)]
        frame: u32,
        #[arg(long, default_value_t = 1 << 24)]
        cap: usize,
        #[arg(long, value_parser = parse_xy)]
        cell: Option<(i32, i32)>,
        #[arg(long, default_value = "")]
        erase: String,
        #[arg(long, default_value_t = 4)]
        every: u32,
    },
    /// Export a finished run for the web UI (`ui/`): per (horizon, level,
    /// frame) the states per player-position cell and the win cells, per
    /// (horizon, level) the marks per cell by distance, and the log's
    /// per-frame / per-iteration timings - from the checkpoint HEADERS and
    /// the marks files only. See `search::ui_export` for the layout.
    ExportUi {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// The run's log (the `[fwd]` / `[bwd]` / `[ladder]` lines).
        #[arg(long)]
        log: String,
        /// Output directory (the UI serves it as `data/`).
        #[arg(long, default_value = "/var/tmp/celeste-ui/data")]
        out: String,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// The tree and log of one `rewrite forward` (frames under the
        /// directory, no ladder): exported as one partial horizon.
        #[arg(long)]
        forward_only: bool,
        /// With `--forward-only`: the `search --save-marks` directory
        /// over that tree. Level 0 gets the remainder-free backward's marks,
        /// and a second level whose forward is level 0's carries the
        /// remainder-exact arc backward (and the concrete witness, if saved).
        #[arg(long)]
        arc: Option<String>,
    },
    /// A concrete input sequence that follows a given TRAJECTORY of player
    /// positions frame by frame (one "x,y" per line, frame 1 first; a
    /// line `-` accepts any position, e.g. during the spawn), found by a
    /// breadth-first search over the reference engine's concrete step:
    /// every frame, all 64 inputs of every surviving state, keeping the
    /// successors at the trajectory's position, deduplicated exactly.
    /// Prints the input bytes for `concrete_run -i` / `pico8_diff/replay.py`,
    /// or the first frame the trajectory could not be followed.
    Trajectory {
        /// One line per frame: `x,y`, `-`, or `x,y K0 K1` - a position AND
        /// the row key (hex) its state must have projected onto `--spec`: an
        /// abstract chain to realize (`ancestry --chain-out` writes one).
        #[arg(long)]
        trajectory: String,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Stop once a win is reached (default), else follow to the end.
        #[arg(long, default_value_t = true)]
        stop_at_win: bool,
        /// The level the keyed lines are projections onto (e.g. `r6sxhn`).
        #[arg(long, value_parser = Level::parse)]
        spec: Option<Level>,
    },
    /// DIAGNOSTIC: follow a CONCRETE input sequence (the reference engine's
    /// concrete step) through one level's tree: per frame, whether the
    /// concrete state projected onto `--level` is a row of the tree, and
    /// where. At the first frame it is not, or at the win (never stored), the
    /// parent row goes through the level's kernels and the concrete
    /// successor's projection is printed next to the closest kernel
    /// successors: a concrete successor no kernel successor equals is a
    /// SOUNDNESS gap; one that is a kernel successor was not stored (a win,
    /// or the level -1 filter). Found room (1,3)'s win rows keyed
    /// apart from their columns (2026-10-02, `runtime2::av_code`).
    Follow {
        #[arg(long)]
        level_dir: String,
        #[arg(long, value_parser = Level::parse)]
        level: Level,
        /// The input bytes: a file in the `tas/` format, or a comma list.
        #[arg(long)]
        inputs: String,
        #[arg(long, default_value_t = 5)]
        show: usize,
        /// The search's `--start-after`: follow from those states (frame 0
        /// the prefix's end).
        #[arg(long)]
        start_after: Option<String>,
    },
}

/// `coarse-census`: one frame's states with the named value cells
/// (`inspect::cell_names`) under an `erase` prefix left out, as hashes; and
/// the frame's row count. Pointers count as structure (the names describe
/// it), not by their cell numbers.
fn erased_states(dir: &std::path::Path, frame: u32, erase: &[String]) -> Result<(u64, rustc_hash::FxHashSet<u64>)> {
    use celeste_engine::runtime2::{av_code, mix64, Cell2, Col, AV};
    let ids = celeste_rust::compiled::ids();
    let name_hash = |s: &str| -> u64 { s.bytes().fold(0x9e37_79b9_7f4a_7c15u64, |h, b| mix64(h ^ b as u64)) };
    let code = |v: AV| -> u64 { av_code(if let AV::Ptr(_) = v { AV::Ptr(0) } else { v }) };
    let (mut rows, mut states) = (0u64, rustc_hash::FxHashSet::default());
    for (_, path) in frame_paths(dir, frame)? {
        let ff = celeste_rust::search::checkpoint::FrameFile::open(&path)?;
        let width = ff.width();
        for lo in (0..width).step_by(1 << 20) {
            let Some(rt2) = ff.load_rows(&[lo..(lo + (1 << 20)).min(width)])? else { break };
            rows += rt2.width as u64;
            let names = cell_names(&rt2, ids);
            let cells: Vec<(u64, usize)> = (0..rt2.cols.len())
                .filter(|&c| matches!(rt2.structure[c], Cell2::Val))
                .map(|c| (c, names.get(&c).cloned().unwrap_or_else(|| format!("cell{c}"))))
                .filter(|(_, nm)| !erase.iter().any(|p| nm.starts_with(p.as_str())))
                .map(|(c, nm)| (name_hash(&nm), c))
                .collect();
            // The block's uniform cells once; the rest per row.
            let mut base = 0u64;
            let mut varying = Vec::new();
            for &(h, c) in &cells {
                match &rt2.cols[c] {
                    Col::U(v) => base = base.wrapping_add(mix64(h ^ code(*v))),
                    _ => varying.push((h, c)),
                }
            }
            for r in 0..rt2.width {
                states.insert(varying.iter().fold(base, |acc, &(h, c)| acc.wrapping_add(mix64(h ^ code(rt2.cols[c].at(r))))));
            }
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

/// The predecessors of row `id` recorded at its layer, unpacked from the
/// records' lane masks.
fn preds_of(edges: &celeste_rust::search::edges::EdgeGraph, id: u64) -> Vec<u64> {
    let mut buf = Vec::new();
    edges.preds_at(id, id_layer(id), &mut buf);
    buf.iter().flat_map(|e| (0..64u64).filter(move |i| e.mask >> i & 1 == 1).map(move |i| e.base + i)).collect()
}

/// Every row of `rt2` through ONE forward frame of `engine` (an empty door, no
/// filter), its edges recorded into a scratch directory that is removed:
/// the successors and whether one wins.
fn rerun(engine: &dyn FrameStep, rt2: Rt2, id: u64, scratch: &str) -> Result<(Vec<Block>, bool)> {
    let block = Block::with_ids(rt2, vec![id], id_seq(id));
    let tmp = std::env::temp_dir().join(format!("{scratch}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&tmp);
    let door = celeste_rust::search::door::Door::new();
    let (next, won, _) = forward_frame(engine, vec![block], &door, None, None, id_layer(id) + 1, Some(&tmp))?;
    let _ = std::fs::remove_dir_all(&tmp);
    Ok((next, won))
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Search { to, ceiling, level, checkpoint_dir, room, win_at, no_witness, save_marks } => {
            use celeste_rust::search::arc_dp::{solve, Concrete};
            std::env::set_var("CELESTE_START_ROOM", &room);
            if let Some(xy) = &win_at {
                std::env::set_var("CELESTE_WIN_AT_XY", xy);
            }
            anyhow::ensure!(
                std::env::var_os("CELESTE_SPLIT_FRAME").is_none(),
                "the arc search runs whole frames: CELESTE_SPLIT_FRAME is not supported"
            );
            let horizon = match (ceiling, to) {
                (Some(h), None) | (None, Some(h)) => h,
                _ => anyhow::bail!("give the horizon: --to H, or --ceiling H for a known solution"),
            };
            let levels: Vec<Level> = level.split(',').map(Level::parse).collect::<std::result::Result<_, _>>().map_err(|e| anyhow::anyhow!(e))?;
            eprintln!("[search] room {room}, levels {level}, horizon {horizon}");
            let t0 = std::time::Instant::now();
            let base = std::path::Path::new(&checkpoint_dir);
            // The previous level's arc-marked nodes and that level: the
            // filter of the next one's forward (`frame::MarkFilter`).
            let mut prev: Option<(Visited, Level)> = None;
            for (li, &lvl) in levels.iter().enumerate() {
                let last = li + 1 == levels.len();
                set_level(lvl);
                let dir = base.join(format!("level{li:02}"));
                // THE FORWARD. A tree that already reaches the horizon is
                // used as it is (resuming it would rebuild its door to extend
                // nothing); a finer level's tree is the filtered one of this
                // horizon - delete it to change the horizon or the levels.
                let t = std::time::Instant::now();
                let first_win = match celeste_rust::frame::tree_first_win_through(&dir, horizon)? {
                    Some(w) => {
                        eprintln!("[search] {}: the tree reaches f{horizon}", dir.display());
                        w
                    }
                    None => {
                        let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
                        let initial = || -> Result<Vec<Block>> { Ok(vec![Block::from_state(&ConcreteEngine::new()?.initial_state()?)?]) };
                        let mut st = match celeste_rust::frame::ForwardState::resume(&dir, true)? {
                            Some(s) => s,
                            None => celeste_rust::frame::ForwardState::start(initial()?, &dir, true)?,
                        };
                        let filter = prev.as_ref().map(|(m, l)| celeste_rust::frame::MarkFilter::new(m, *l));
                        st.extend(&engine, &dir, horizon, filter.as_ref())?;
                        st.win_frame
                    }
                };
                eprintln!("[search] level {li} ({lvl}): forward to f{horizon} in {:.1} s; first win {first_win:?}", t.elapsed().as_secs_f64());
                // THE ARC PHASE, and the concrete search: at a coarser level
                // the try at the bound only (a win there is the optimum), at
                // the last the whole search.
                let concrete = match (no_witness, last) {
                    (true, _) => Concrete::None,
                    (false, true) => Concrete::Full,
                    (false, false) => Concrete::AtBound,
                };
                let s = solve(&dir, lvl, horizon, concrete, !last, if last { save_marks.as_deref().map(std::path::Path::new) } else { None })?;
                let wall = t0.elapsed().as_secs_f64();
                let Some(bound) = s.arc else {
                    anyhow::ensure!(ceiling.is_none(), "ceiling {horizon} REFUTED by the arc search at level {li} ({lvl}): a known solution the model cannot reproduce");
                    println!("REFUTED: no win by f{horizon} (level {li}, {lvl}; {wall:.1} s)");
                    return Ok(());
                };
                println!("ARC BOUND: no win before f{bound} (level {li}, {lvl}: remainder-exact, the level's objects over-approximated)");
                if let Some(w) = &s.concrete {
                    let f = w.inputs.len();
                    let path = base.join(format!("witness_frame_{f}.txt"));
                    std::fs::write(
                        &path,
                        format!(
                            "# Room ({room}): the concrete witness of `rewrite search --level {level} --to {horizon}`{}.\n\
                             # Arc bound f{bound} (level {li}, {lvl}); no concrete win before f{f} inside the winning sets (exhaustive).\n\
                             # A balloon room's win may hold for some `rnd` draws only: replay with seeds.\n{}\n",
                            win_at.as_ref().map_or(String::new(), |w| format!(" --win-at {w}")),
                            w.inputs_text()
                        ),
                    )?;
                    println!("OPTIMAL win frame: {f} ({wall:.1} s); witness {}", path.display());
                    return Ok(());
                }
                if no_witness && last {
                    return Ok(());
                }
                if last {
                    anyhow::ensure!(ceiling.is_none(), "ceiling {horizon}: no concrete witness by it - a known solution the model cannot reproduce");
                    println!("no concrete win by f{horizon} (arc bound f{bound}; {wall:.1} s)");
                    return Ok(());
                }
                prev = Some((s.arc_marks.expect("asked for"), lvl));
            }
        }
        Command::Forward {
            to,
            level,
            checkpoint_dir,
            room,
            reference,
        } => {
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_level(level);
            let t = std::time::Instant::now();
            let engine: Box<dyn FrameStep> = if reference {
                Box::new(std::sync::Mutex::new(celeste_rust::trace::refengine::RefEngine::new()?))
            } else {
                Box::new(celeste_rust::compiled::FrameEngine::new_for_start_room()?)
            };
            eprintln!("[fwd] engine up in {:.2} s ({level}{})", t.elapsed().as_secs_f64(), if reference { ", REFERENCE engine" } else { "" });
            let initial = vec![Block::from_state(&ConcreteEngine::new()?.initial_state()?)?];
            let dir = std::path::Path::new(&checkpoint_dir);
            let t = std::time::Instant::now();
            let fwd = celeste_rust::frame::forward_resume_or_run(engine.as_ref(), initial, dir, to)?;
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
        Command::DiagProject { fine_dir, coarse_dir, level, from, to, room, fine_marks, coarse_marks } => {
            use celeste_rust::frame::load_frame;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let load_marks = |p: Option<String>| p.map(|p| Visited::load(std::path::Path::new(&p))).transpose();
            let (fine_marks, coarse_marks) = (load_marks(fine_marks)?, load_marks(coarse_marks)?);
            let (fine, coarse) = (std::path::Path::new(&fine_dir), std::path::Path::new(&coarse_dir));
            let mut reached = Visited::new();
            let (mut total, mut missed, mut unmarked) = (0usize, 0usize, 0usize);
            let mut misses: Vec<String> = Vec::new();
            for f in 0..=to {
                for b in load_frame(coarse, f)? {
                    let cells = b.positions()?;
                    for (k, &c) in b.keys().iter().zip(&cells) {
                        reached.insert(b.shard_shape(), *k, c);
                    }
                }
                if f < from {
                    continue;
                }
                let (mut n, mut miss, mut unm) = (0usize, 0usize, 0usize);
                for block in load_frame(fine, f)? {
                    let cells = block.positions()?;
                    let (ws, wk, wc) = widened_keys(&block, level)?;
                    for i in 0..block.lanes() {
                        if fine_marks.as_ref().is_some_and(|m| !m.contains(block.shard_shape(), block.keys()[i], cells[i])) {
                            continue;
                        }
                        n += 1;
                        if !reached.contains(ws, wk[i], wc[i]) {
                            miss += 1;
                            if misses.len() < 8 {
                                misses.push(format!("f{f:03}: a fine row at {} projects to key {:016x}{:016x}, which the coarse tree has not reached", where_(wc[i]), wk[i].0, wk[i].1));
                            }
                        } else if coarse_marks.as_ref().is_some_and(|m| !m.contains(ws, wk[i], wc[i])) {
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
            room,
            cells: top_cells,
        } => {
            std::env::set_var("CELESTE_START_ROOM", &room);
            let dir = std::path::Path::new(&checkpoint_dir);
            let mut by_cell: rustc_hash::FxHashMap<u32, usize> = Default::default();
            for frame in 0..=to {
                let mut n = 0usize;
                let mut acc: u64 = 0;
                for block in celeste_rust::frame::load_frame(dir, frame)? {
                    let cells = block.positions()?;
                    for (k, &c) in block.keys().iter().zip(&cells) {
                        acc = acc.wrapping_add(celeste_engine::runtime2::mix64(
                            k.0 ^ celeste_engine::runtime2::mix64(k.1 ^ (c as u64) << 1),
                        ));
                        n += 1;
                        if frame == to && top_cells.is_some() {
                            *by_cell.entry(c).or_default() += 1;
                        }
                    }
                }
                println!("f{frame:03} {n} {acc:016x}");
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
        Command::BenchFrame {
            checkpoint_dir,
            frame,
            reps,
            room,
            level_dir,
            level,
            edges,
        } => {
            use celeste_rust::frame::{load_frame, threads};
            use celeste_rust::search::door::Door;
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_level(level);
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = match &level_dir {
                Some(d) => std::path::PathBuf::from(d),
                None => std::path::Path::new(&checkpoint_dir).join("level00"),
            };
            let t = std::time::Instant::now();
            let frontier = load_frame(&dir, frame)?;
            let lanes: usize = frontier.iter().map(Block::lanes).sum();
            eprintln!(
                "[bench] f{frame}: {} blocks, {lanes} lanes loaded in {:.2} s; {} threads",
                frontier.len(),
                t.elapsed().as_secs_f64(),
                threads()
            );
            // Warm the kernel registry outside the timed reps.
            {
                let b = Block::from_rt2(frontier[0].rt2().clone_block());
                let n = b.lanes();
                let mask: Vec<bool> = (0..n).map(|i| i < 64).collect();
                let small = vec![b.keep(&mask).expect("a non-empty block")];
                forward_frame(&engine, small, &Door::new(), None, None, frame + 1, None)?;
            }
            let edges_dir = dir.join("bench-edges");
            // With edges, the tree's own door: every re-emitted old state
            // then resolves to its real (earlier) layer, as in the search,
            // which is what the compaction's per-layer split sees.
            let tree_door = if edges {
                let t = std::time::Instant::now();
                let state = celeste_rust::frame::ForwardState::resume(&dir, false)?.expect("a checkpoint tree");
                eprintln!("[bench] tree door loaded in {:.1} s", t.elapsed().as_secs_f64());
                Some(state)
            } else {
                None
            };
            for rep in 0..reps {
                // With their ids (as the search runs them: predecessor masks
                // are tracked whenever the input has ids).
                let input: Vec<Block> = frontier
                    .iter()
                    .map(|b| Block::with_ids(b.rt2().clone_block(), b.ids().to_vec(), b.seq()))
                    .collect();
                let fresh = Door::new();
                let door: &Door = tree_door.as_ref().map_or(&fresh, |s| s.door());
                let _ = std::fs::remove_dir_all(&edges_dir);
                let t = std::time::Instant::now();
                let (next, _won, st) =
                    forward_frame(&engine, input, door, None, None, frame + 1, edges.then_some(edges_dir.as_path()))?;
                let t_fwd = t.elapsed();
                let (t_compact, records) = if edges {
                    let t = std::time::Instant::now();
                    let c = celeste_rust::search::edges::compact_frame(&edges_dir, frame + 1)?;
                    println!(
                        "[bench]   compaction: {} records -> {} pairs, {:.1} MB runs ({:.2} B/pair); read {:.0} sort {:.0} write {:.0} ms",
                        c.records,
                        c.pairs,
                        c.bytes as f64 / 1e6,
                        c.bytes as f64 / c.pairs.max(1) as f64,
                        c.t_read.as_secs_f64() * 1e3,
                        c.t_sort.as_secs_f64() * 1e3,
                        c.t_write.as_secs_f64() * 1e3
                    );
                    (t.elapsed(), c.records)
                } else {
                    (std::time::Duration::ZERO, 0)
                };
                let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
                println!(
                    "[bench] rep {rep}: raw {} kept {} | wave {:.0} ms (idle {:.0}%) door {:.0} total {:.0} ms | flushes {} ({:.0} rows avg) | {} out blocks | edges {} written {:.0} compact {:.0} ms",
                    st.lanes_raw,
                    st.lanes_kept,
                    ms(st.t_wave),
                    st.wave_idle * 100.0,
                    ms(st.t_door),
                    ms(t_fwd),
                    st.flushes,
                    st.flushed_rows as f64 / st.flushes.max(1) as f64,
                    next.len(),
                    records,
                    ms(st.t_edges),
                    ms(t_compact),
                );
            }
            let _ = std::fs::remove_dir_all(&edges_dir);
            celeste_rust::compiled::dispatch::print_kernel_hits();
        }
        Command::CellGrowth { level_dir, from, to, top, at, by_age } => {
            let dir = std::path::Path::new(&level_dir);
            let n_frames = (to - from + 1) as usize;
            // Per cell, its new states per frame.
            let mut per: std::collections::HashMap<u32, Vec<u64>> = Default::default();
            for f in from..=to {
                for (_, file) in frame_files(dir, f)? {
                    for (cell, rows) in file.cell_counts() {
                        per.entry(cell as u32).or_insert_with(|| vec![0; n_frames])[(f - from) as usize] += rows as u64;
                    }
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
        Command::ArcCheck { level_dir, room, win_at, from, to, samples, level } => {
            use celeste_engine::runtime2::{Col, AV};
            use celeste_rust::interpreter::state::State;
            use celeste_rust::search::arcs::{point, Rect, Rects, Set, CIRCLE};
            std::env::set_var("CELESTE_START_ROOM", &room);
            if let Some(xy) = &win_at {
                std::env::set_var("CELESTE_WIN_AT_XY", xy);
            }
            set_level(level);
            let dir = std::path::Path::new(&level_dir);
            let edges_dir = dir.join("edges");
            let ids = celeste_rust::compiled::ids();
            let mut eng = ConcreteEngine::new()?;
            let initial = eng.initial_state()?;
            let graph = celeste_rust::search::edges::EdgeGraph::open(&edges_dir, to)?;
            let rem_cells = |rt2: &Rt2| rt2.player_xy_cells(ids, ids.f_rem);
            // One stored row, by id, as a one-lane block.
            let row_of = |id: u64| -> Result<Block> { Ok(Block::from_rt2(load_row(dir, id)?.0)) };
            // Every successor of `row` at remainder `rem` (circle points;
            // ignored without a player), over all 64 inputs: its projection
            // onto the level and its player's remainder (circle points).
            type Succ = ((u64, (u64, u64), u32), Option<(u32, u32)>);
            let steps = std::cell::Cell::new(0u64);
            let mut successors = |row: &Block, rem: (u32, u32)| -> Result<Vec<Succ>> {
                let mut rt2 = row.rt2().clone_block();
                if let Some((cx, cy)) = rem_cells(&rt2) {
                    rt2.cols[cx] = Col::U(AV::Num(celeste_rust::pico8_num::Pico8Num::from_raw(rem.0 as i32 - 32768)));
                    rt2.cols[cy] = Col::U(AV::Num(celeste_rust::pico8_num::Pico8Num::from_raw(rem.1 as i32 - 32768)));
                }
                let st: State = Block::from_rt2(rt2).to_state();
                let mut out = Vec::new();
                for b in 0..64u8 {
                    for succ in eng.step_all(&st, b, &initial)? {
                        steps.set(steps.get() + 1);
                        let blk = Block::from_state(&succ)?;
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
                        let (shape, keys, cells) = widened_keys(&blk, level)?;
                        out.push(((shape, keys[0], cells[0]), q));
                    }
                }
                Ok(out)
            };
            let (mut missing_total, mut records_total) = (0u64, 0u64);
            let (mut inside, mut inside_bad, mut outside, mut outside_bad, mut no_player) = (0u64, 0u64, 0u64, 0u64, 0u64);
            let (mut sampled, mut kinds) = (0u64, std::collections::BTreeMap::<String, u64>::new());
            let t0 = std::time::Instant::now();
            /// A record with its transfer decoded.
            struct Rec {
                target: u64,
                base: u64,
                mask: u64,
                x: celeste_rust::search::arcs::Transfer,
                y: celeste_rust::search::arcs::Transfer,
            }
            for frame in from..=to {
                let all = graph.records_at(frame);
                let missing = all.iter().filter(|e| graph.pair(frame, e.xfer).is_none()).count() as u64;
                missing_total += missing;
                let recs: Vec<Rec> = all
                    .iter()
                    .filter_map(|e| graph.pair(frame, e.xfer).map(|p| Rec { target: e.target, base: e.base, mask: e.mask, x: p.0.transfer(), y: p.1.transfer() }))
                    .collect();
                records_total += recs.len() as u64;
                for r in &recs {
                    for (axis, t) in [("x", &r.x), ("y", &r.y)] {
                        let whole = t.guard == celeste_rust::search::arcs::Seg { lo: 0, hi: celeste_rust::search::arcs::CIRCLE };
                        let k = format!("{axis} {} {}", if whole { "whole" } else { "piece" }, match t.action { celeste_rust::search::arcs::Action::Rotate(_) => "rotate", _ => "const" });
                        *kinds.entry(k).or_default() += 1;
                    }
                }
                let stride = (recs.len() / samples.max(1)).max(1);
                for r in recs.iter().step_by(stride).take(samples) {
                    sampled += 1;
                    let src = r.base + r.mask.trailing_zeros() as u64;
                    let src_row = row_of(src)?;
                    let tgt_row = row_of(r.target)?;
                    let (tshape, tkeys, tcells) = widened_keys(&tgt_row, level)?;
                    let target = (tshape, tkeys[0], tcells[0]);
                    let (gx, gy) = (r.x.guard, r.y.guard);
                    // Every guard of this (pred, target), as rectangles.
                    let mut union = Rects::empty();
                    for q in recs.iter().filter(|q| q.target == r.target && src >= q.base && src < q.base + 64 && q.mask >> (src - q.base) & 1 == 1) {
                        union.add(Rect { x: q.x.guard_set(), y: q.y.guard_set() });
                    }
                    let has_player = rem_cells(src_row.rt2()).is_some();
                    if !has_player {
                        no_player += 1;
                    }
                    let mid = |g: celeste_rust::search::arcs::Seg| (g.lo + g.hi) / 2;
                    let probes: Vec<(u32, u32)> = if has_player {
                        vec![(gx.lo, gy.lo), (gx.hi - 1, gy.hi - 1), (mid(gx), mid(gy)), (gx.lo, gy.hi - 1)]
                    } else {
                        vec![(CIRCLE / 2, CIRCLE / 2)]
                    };
                    for p in probes {
                        inside += 1;
                        let want = (r.x.action.image(&Set::point(p.0)), r.y.action.image(&Set::point(p.1)));
                        let succs = successors(&src_row, p)?;
                        let ok = succs.iter().any(|(id, q)| {
                            *id == target
                                && match q {
                                    Some((qx, qy)) => want.0.contains(*qx) && want.1.contains(*qy),
                                    None => true,
                                }
                        });
                        if !ok {
                            inside_bad += 1;
                            let reached: Vec<&Option<(u32, u32)>> = succs.iter().filter(|(id, _)| *id == target).map(|(_, q)| q).collect();
                            eprintln!(
                                "[arc-check] f{frame} pred {src:#x} -> {:#x}: inside {p:?} the transfer x {:?} y {:?} predicts {want:?}; the reference reaches the target with remainders {reached:?}",
                                r.target, r.x, r.y
                            );
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
                            outside += 1;
                            if successors(&src_row, p)?.iter().any(|(id, _)| *id == target) {
                                outside_bad += 1;
                                eprintln!("[arc-check] f{frame} pred {src:#x} -> {:#x}: OUTSIDE every guard of the pair at {p:?}, the reference reaches the target", r.target);
                            }
                        }
                    }
                }
                eprintln!(
                    "[arc-check] f{frame:03}: {} records, {missing} without a transfer; so far inside {inside} ({inside_bad} bad), outside {outside} ({outside_bad} bad), {} ref steps, {:.1} s",
                    recs.len(),
                    steps.get(),
                    t0.elapsed().as_secs_f64()
                );
            }
            println!("arc-check f{from}-f{to}: {records_total} records; transfers per axis {kinds:?}");
            println!("completeness: {missing_total} recorded edges without a transfer");
            println!(
                "agreement: {sampled} sampled records ({no_player} from a row without a player); inside the guard {inside} probes, {inside_bad} disagree; outside every guard {outside} probes, {outside_bad} reach the target; {} reference steps",
                steps.get()
            );
            anyhow::ensure!(missing_total == 0 && inside_bad == 0 && outside_bad == 0, "arc-check: disagreement");
        }
        Command::CoarseCensus { level_dir, from, to, erase } => {
            let erase = prefixes(&erase);
            let mut through: rustc_hash::FxHashSet<u64> = Default::default();
            for frame in from..=to {
                let (rows, states) = erased_states(std::path::Path::new(&level_dir), frame, &erase)?;
                let n = states.len();
                through.extend(states);
                println!("f{frame:03}: {rows} rows -> {n} states ({:.2}x fewer); {} states through f{frame:03}", rows as f64 / n.max(1) as f64, through.len());
            }
        }
        Command::RefCheck { level_dir, frame, level, samples, cell, show } => {
            let dir = std::path::Path::new(&level_dir);
            set_level(level);
            let kernels = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let mut reference = celeste_rust::trace::refengine::RefEngine::new()?;
            let only = cell.map(cell_at).transpose()?;
            // Every candidate row (seq, row), then `samples` evenly spaced.
            let files = frame_files(dir, frame)?;
            let mut all: Vec<(usize, u32)> = Vec::new();
            for (fi, (_, f)) in files.iter().enumerate() {
                match only {
                    Some(c) => all.extend(f.rows_of_cell(c).into_iter().flatten().map(|r| (fi, r))),
                    None => all.extend((0..f.width()).map(|r| (fi, r))),
                }
            }
            anyhow::ensure!(!all.is_empty(), "no rows at step {frame}");
            let n = samples.min(all.len());
            let (mut sound_gaps, mut precision_gaps, mut rows_bad) = (0usize, 0usize, 0usize);
            for k in 0..n {
                let (fi, r) = all[k * all.len() / n];
                let (seq, f) = &files[fi];
                let rt2 = f.load_rows(&[r..r + 1])?.ok_or_else(|| anyhow::anyhow!("no row"))?;
                let at = where_(f.row_cells()[r as usize]);
                // The kernels' successors.
                let (next, _) = rerun(&kernels, rt2.clone_block(), pack_id(frame, *seq, r), "ref-check")?;
                let mut kset: std::collections::BTreeSet<Proj> = Default::default();
                for b in next {
                    kset.extend(project_onto(&mut b.into_rt2(), level));
                }
                // The reference engine's, from the same row projected onto the
                // level (the input the kernels' own widening reads).
                let mut input = rt2.clone_block();
                widen_rt2_to(&mut input, level);
                let state = Block::from_rt2(input).to_state();
                let mut rset: std::collections::BTreeSet<Proj> = Default::default();
                for leaf in reference.run_lane(&state, 0)? {
                    rset.extend(project_onto(&mut Block::from_state(&leaf)?.into_rt2(), level));
                }
                let missing: Vec<&Proj> = rset.difference(&kset).collect();
                let extra: Vec<&Proj> = kset.difference(&rset).collect();
                sound_gaps += missing.len();
                precision_gaps += extra.len();
                let verdict = if missing.is_empty() && extra.is_empty() {
                    "ok"
                } else {
                    rows_bad += 1;
                    "DIFFERS"
                };
                println!("[ref-check] row s{seq} r{r} at {at}: kernels {} successors, reference {}: {verdict} ({} only in the reference, {} only in the kernels)", kset.len(), rset.len(), missing.len(), extra.len());
                for p in missing.iter().take(show) {
                    println!("    only in the REFERENCE (soundness gap): {}", brief(p));
                }
                for p in extra.iter().take(show) {
                    println!("    only in the KERNELS (precision gap): {}", brief(p));
                }
            }
            println!("[ref-check] {n} rows: {rows_bad} differ; {sound_gaps} successors only in the reference, {precision_gaps} only in the kernels");
        }
        Command::RerunRow { level_dir, row, level, erase } => {
            let ids = celeste_rust::compiled::ids();
            let dir = std::path::Path::new(&level_dir);
            let erase = prefixes(&erase);
            let parts: Vec<u32> = row.split(':').map(|s| s.trim().parse()).collect::<Result<_, _>>()?;
            let [layer, seq, r] = parts[..] else { anyhow::bail!("--row L:S:R") };
            set_level(level);
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let id = pack_id(layer, seq, r);
            let (rt2, _, cell) = load_row(dir, id)?;
            let me = project_row(&rt2, &cell_names(&rt2, ids), 0, &erase);
            println!("[rerun] l{layer} s{seq} r{r} at {}:", where_(cell));
            for (n, v) in &me {
                println!("    {n} = {v}");
            }
            let (next, won) = rerun(&engine, rt2, id, "rerun-row")?;
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
            // The real tree's projections in a cell, each with the first step
            // it was reached at, through `step`; and their values at `step`.
            let mut real_cells: HashMap<u32, HashMap<u64, u32>> = Default::default();
            let mut real_vals = Dist::new();
            let mut real_of = |c: u32, vals: Option<&mut Dist>| -> Result<HashMap<u64, u32>> {
                if let Some(m) = real_cells.get(&c) {
                    return Ok(m.clone());
                }
                let (Some(real), mut vals) = (real, vals) else { return Ok(HashMap::new()) };
                let mut m: HashMap<u64, u32> = Default::default();
                for s in 0..=step {
                    for (_, f) in frame_files(real, s)? {
                        for range in f.rows_of_cell(c) {
                            let Some(rt2) = f.load_rows(&[range])? else { continue };
                            for r in 0..rt2.width {
                                let p = project(&rt2, r as u32);
                                if let (Some(vals), true) = (vals.as_deref_mut(), s == step) {
                                    tally(vals, &p);
                                }
                                m.entry(projection_key(&p)).or_insert(s);
                            }
                        }
                    }
                }
                real_cells.insert(c, m.clone());
                Ok(m)
            };
            // The coarse tree's new states at `step` in the cells: the samples
            // (with `--real`, the spurious ones).
            let (mut total, mut sp_vals, mut co_vals) = (0u64, Dist::new(), Dist::new());
            let mut chosen: Vec<u64> = Vec::new();
            for &want in &wants {
                let here_real = real_of(want, Some(&mut real_vals))?;
                for (seq, f) in frame_files(coarse, step)? {
                    for range in f.rows_of_cell(want) {
                        let lo = range.start;
                        let Some(rt2) = f.load_rows(&[range])? else { continue };
                        for r in 0..rt2.width {
                            total += 1;
                            let p = project(&rt2, r as u32);
                            let is_real = real.is_some() && here_real.get(&projection_key(&p)).is_some_and(|&s| s <= step);
                            tally(if is_real { &mut co_vals } else { &mut sp_vals }, &p);
                            if !is_real {
                                chosen.push(pack_id(step, seq, lo + r as u32));
                            }
                        }
                    }
                }
            }
            if real.is_none() {
                println!("[spurious] {total} states first reached at f{step:03} in x {x0}..={x1}, y {y0}..={y1}");
            } else {
                println!("[spurious] x {x0}..={x1}, y {y0}..={y1}, step {step}: coarse {total} new states, {} spurious (projection never reached by the real tree)", chosen.len());
                // Per-field distributions: the real tree at this step, the
                // coarse states it has, the spurious ones.
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
            // Walk the samples back (with `--real`, to their first spurious
            // transition).
            let edges = celeste_rust::search::edges::EdgeGraph::open(&coarse.join("edges"), step)?;
            let n = samples.min(chosen.len());
            for k in 0..n {
                let mut id = chosen[k * chosen.len() / n];
                let (rt2, shape, c) = load_row(coarse, id)?;
                let mut cur = project(&rt2, 0);
                println!("\n[spurious] sample {k}: l{} s{} r{} shape {shape:016x} {}", id_layer(id), id_seq(id), id_row(id), where_(c));
                if real.is_none() {
                    for (name, v) in &cur {
                        println!("    {name} = {v}");
                    }
                }
                // (frame, cell, key) along the chain, for `--chain-out`.
                let mut chain: Vec<(u32, u32, (u64, u64))> = vec![(id_layer(id), c, rt2.clone_block().row_keys_canonical()[0])];
                let mut found = false;
                for _ in 0..depth {
                    if id_layer(id) == 0 {
                        break;
                    }
                    let preds = preds_of(&edges, id);
                    // Prefer a REAL predecessor: then this step is the first
                    // spurious one.
                    let mut pick: Option<(u64, Rt2, Proj, u32, bool)> = None;
                    for &p in &preds {
                        let (rt, _, pc) = load_row(coarse, p)?;
                        let pp = project(&rt, 0);
                        let is_real = real.is_some() && real_of(pc, None)?.get(&projection_key(&pp)).is_some_and(|&s| s <= id_layer(p));
                        if is_real || pick.is_none() {
                            pick = Some((p, rt, pp, pc, is_real));
                        }
                        if is_real || real.is_none() {
                            break;
                        }
                    }
                    let Some((p, rt, pp, pc, is_real)) = pick else {
                        println!("  <- no recorded predecessor at f{:03}", id_layer(id));
                        break;
                    };
                    let changed: Vec<String> = cur
                        .iter()
                        .filter(|(nm, v)| pp.get(*nm) != Some(*v))
                        .map(|(nm, v)| format!("{nm}: {} -> {v}", pp.get(nm).map(|s| s.as_str()).unwrap_or("absent")))
                        .collect();
                    if is_real {
                        println!("  FIRST SPURIOUS STEP l{} s{} r{} -> l{}: {} ({} preds): {}", id_layer(p), id_seq(p), id_row(p), id_layer(id), where_(pc), preds.len(), changed.join(", "));
                        // The whole real predecessor, unprojected: the floors too.
                        let full = project_row(&rt, &cell_names(&rt, ids), 0, &[]);
                        println!("    predecessor (full): {}", full.iter().filter(|(n, _)| !n.contains("hitbox") && !n.contains(".type.") && !n.ends_with(".solids") && !n.contains(".flip.y") && !n.ends_with(".spr") || n.starts_with("spring") || n.starts_with("balloon")).map(|(n, v)| format!("{n}={v}")).collect::<Vec<_>>().join(" "));
                        found = true;
                        break;
                    }
                    println!("  <- f{:03} {} [{}:{}:{}] ({} preds){}: {}", id_layer(p), where_(pc), id_layer(p), id_seq(p), id_row(p), preds.len(), if real.is_some() { " still spurious" } else { "" }, changed.join(", "));
                    chain.push((id_layer(p), pc, rt.clone_block().row_keys_canonical()[0]));
                    id = p;
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
        Command::ColCensus { level_dir, frame, cap, cell, erase, every } => {
            use celeste_engine::runtime2::{av_code, num_code, Cell2, Col, AV};
            let only_cell = cell.map(cell_at).transpose()?;
            let dir = std::path::Path::new(&level_dir);
            let ids = celeste_rust::compiled::ids();
            // shape -> (rows, per-column sets, player field names, spd pairs)
            struct S {
                rows: u64,
                cols: Vec<rustc_hash::FxHashSet<u64>>,
                varying: Vec<bool>,
                names: std::collections::HashMap<usize, String>,
                spd: rustc_hash::FxHashSet<u64>,
                /// Per column: rows holding an interval, rows holding an unknown bool.
                ivals: Vec<u64>,
                ubools: Vec<u64>,
            }
            let mut by_shape: std::collections::BTreeMap<u64, S> = Default::default();
            for (_, p) in frame_paths(dir, frame)? {
                let ff = celeste_rust::search::checkpoint::FrameFile::open(&p)?;
                let width = ff.width();
                // The row ranges to read: the whole file in 1M-row pieces, or
                // one cell's rows.
                let pieces: Vec<std::ops::Range<u32>> = match only_cell {
                    Some(c) => ff.rows_of_cell(c),
                    None => (0..width).step_by(1 << 20).map(|lo| lo..(lo + (1 << 20)).min(width)).collect(),
                };
                for piece in pieces {
                    let Some(rt2) = ff.load_rows(&[piece])? else { break };
                    let st = by_shape.entry(ff.shape_hash()).or_insert_with(|| {
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
                                    st.cols[c].insert(av_code(*v));
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
                                        set.insert(av_code(v));
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
                            st.spd.insert(av_code(rt2.cols[sx].at(r)).rotate_left(32) ^ av_code(rt2.cols[sy].at(r)));
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
                        let mut vals: Vec<f64> = st.cols[c].iter().filter(|&&v| v == num_code(v as u32)).map(|&v| v as u32 as i32 as f64 / 65536.0).collect();
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
                let (mut all, mut spd, mut inner) = (rustc_hash::FxHashSet::<u64>::default(), rustc_hash::FxHashSet::<u64>::default(), rustc_hash::FxHashSet::<u64>::default());
                let mut first: Option<u32> = None;
                println!("step | new | distinct states | speed pairs | inner (no speed)");
                for s in 0..=frame {
                    let mut new = 0u64;
                    for (_, f) in frame_files(dir, s)? {
                        for range in f.rows_of_cell(want) {
                            let Some(rt2) = f.load_rows(&[range])? else { continue };
                            let names = cell_names(&rt2, ids);
                            for r in 0..rt2.width {
                                let mut p = project_row(&rt2, &names, r as u32, &erase);
                                new += 1;
                                all.insert(projection_key(&p));
                                let sp: Proj = ["spd.x", "spd.y"].iter().map(|k| (k.to_string(), p.remove(*k).unwrap_or_default())).collect();
                                spd.insert(projection_key(&sp));
                                inner.insert(projection_key(&p));
                            }
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
        Command::ExportUi {
            checkpoint_dir,
            log,
            out,
            room,
            forward_only,
            arc,
        } => {
            let (rx, ry) = room
                .split_once(',')
                .and_then(|(a, b)| Some((a.parse::<i16>().ok()?, b.parse::<i16>().ok()?)))
                .ok_or_else(|| anyhow::anyhow!("--room must be \"x,y\", got {room:?}"))?;
            celeste_rust::search::ui_export::export(
                std::path::Path::new(&checkpoint_dir),
                std::path::Path::new(&log),
                std::path::Path::new(&out),
                (rx, ry),
                forward_only,
                arc.as_deref().map(std::path::Path::new),
            )?;
        }
        Command::Trajectory {
            trajectory,
            room,
            stop_at_win,
            spec,
        } => {
            std::env::set_var("CELESTE_START_ROOM", &room);
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
            let mut eng = ConcreteEngine::new()?;
            let initial = eng.initial_state()?;
            // (state, the inputs that led to it)
            let mut layer: Vec<(celeste_rust::interpreter::state::State, Vec<u8>)> = vec![(initial.clone(), Vec::new())];
            for (i, want) in traj.iter().enumerate() {
                let f = i as u32 + 1;
                let mut next: Vec<(celeste_rust::interpreter::state::State, Vec<u8>)> = Vec::new();
                let mut seen: rustc_hash::FxHashSet<(u64, u64, u32)> = Default::default();
                let mut tried = 0usize;
                // Where the successors went: reported when none is at `want`.
                let mut reached: std::collections::BTreeMap<Option<(i32, i32)>, usize> = Default::default();
                // At the right position but projected onto another row.
                let mut off_chain = 0usize;
                let mut off_chain_sample: Option<celeste_engine::runtime2::Rt2> = None;
                // A `-` frame (any position: the spawn) takes only the idle
                // input; the inputs before the player exists cannot matter
                // to a witness, and 64 x 24 frames of them would.
                let inputs: std::ops::Range<u8> = if want.is_some() { 0..64 } else { 0..1 };
                for (st, path) in &layer {
                    for byte in inputs.clone() {
                        // Every leaf: a leaf at the wanted position is as good
                        // a witness as the frame has. The real-PICO-8 replay
                        // is what settles it.
                        for succ in eng.step_all(st, byte, &initial)? {
                            tried += 1;
                            let block = Block::from_state(&succ)?;
                            let cell = block.positions()?[0];
                            let pos = celeste_rust::search::pos_graph::cell_xy(cell);
                            if let Some(w) = want {
                                if pos != Some(*w) {
                                    *reached.entry(pos).or_default() += 1;
                                    continue;
                                }
                            }
                            if let (Some(k), Some(l)) = (keyed[i], level) {
                                let (_, keys, _) = widened_keys(&block, l)?;
                                if keys[0] != k {
                                    off_chain += 1;
                                    off_chain_sample.get_or_insert_with(|| block.rt2().clone_block());
                                    continue;
                                }
                            }
                            // Deduplicated on the EXACT state (the row key
                            // forgets the remainder).
                            let key = block.rt2().clone_block().row_keys_canonical()[0];
                            if !seen.insert((key.0, key.1, cell)) {
                                continue;
                            }
                            let mut p = path.clone();
                            p.push(byte);
                            if stop_at_win && wins_of(block.rt2())?.iter().any(|&w| w) {
                                println!("[trajectory] WIN at f{f} after {} frames; inputs:", p.len());
                                println!("{}", p.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
                                return Ok(());
                            }
                            next.push((succ, p));
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
        Command::Follow { level_dir, level, inputs, show, start_after } => {
            let dir = std::path::Path::new(&level_dir);
            let lvl = level;
            set_level(lvl);
            let bytes = celeste_rust::concrete::read_inputs(&inputs)?;
            // (key, cell) -> (layer, seq, row): where the tree first has it.
            let mut at: rustc_hash::FxHashMap<(u64, u64, u32), (u32, u32, u32)> = Default::default();
            let mut layers = 0u32;
            while dir.join("frames").join(format!("f{:03}", layers)).exists() {
                for (seq, file) in frame_files(dir, layers)? {
                    if let Some(rt2) = file.load_all()? {
                        let b = Block::from_rt2(rt2);
                        let cells = b.positions()?;
                        for (r, (k, &c)) in b.keys().iter().zip(&cells).enumerate() {
                            at.entry((k.0, k.1, c)).or_insert((layers, seq, r as u32));
                        }
                    }
                }
                layers += 1;
            }
            eprintln!("[follow] {} rows in {layers} layers, {} inputs", at.len(), bytes.len());
            let mut eng = ConcreteEngine::new()?;
            let initial = eng.initial_state()?;
            let mut states = start_states(start_after.as_deref())?;
            let mut parent: Option<(u32, u32, u32)> = None;
            for (f, &byte) in bytes.iter().enumerate() {
                let f = f as u32 + 1;
                let mut next = Vec::new();
                for s0 in &states {
                    next.extend(eng.step_all(s0, byte, &initial)?);
                }
                // Where the tree has one of the concrete leaves (an `rnd` fork
                // makes several).
                let mut found = None;
                let mut won = false;
                for succ in &next {
                    let block = Block::from_state(succ)?;
                    if wins_of(block.rt2())?.iter().any(|&w| w) {
                        won = true;
                    }
                    let (_, keys, cells) = widened_keys(&block, lvl)?;
                    if let Some(&loc) = at.get(&(keys[0].0, keys[0].1, cells[0])) {
                        found = Some(loc);
                    }
                }
                // A win is not stored in the tree: check the kernels make it.
                let Some(loc) = found.filter(|_| !won) else {
                    println!("[follow] f{f} input {byte}: {} ({} concrete leaves)", if won { "WIN" } else { "NOT IN THE TREE" }, next.len());
                    let (layer, seq, r) = parent.ok_or_else(|| anyhow::anyhow!("the first step is already missing"))?;
                    let id = pack_id(layer, seq, r);
                    let (rt2, ..) = load_row(dir, id)?;
                    println!("    parent row (layer {layer} s{seq} r{r}): {}", brief(&project_onto(&mut rt2.clone_block(), lvl)[0]));
                    for s in &states {
                        println!("    concrete parent: {}", brief(&project_all(Block::from_state(s)?.rt2())[0]));
                    }
                    let kernels = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
                    let (out, _) = rerun(&kernels, rt2, id, "follow")?;
                    let mut kset: std::collections::BTreeSet<Proj> = Default::default();
                    let mut kwins = 0;
                    for b in out {
                        let mut w = b.into_rt2();
                        let wins = wins_of(&w)?;
                        kwins += wins.iter().filter(|&&x| x).count();
                        kset.extend(project_onto(&mut w, lvl));
                    }
                    println!("    {} kernel successors, {kwins} of them wins", kset.len());
                    for succ in &next {
                        let p = project_onto(&mut Block::from_state(succ)?.into_rt2(), lvl).remove(0);
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
                println!("[follow] f{f} input {byte}: in the tree at layer {} (s{} r{})", loc.0, loc.1, loc.2);
                parent = Some(loc);
                states = next;
            }
            println!("[follow] every frame is in the tree");
        }
    }

    Ok(())
}
