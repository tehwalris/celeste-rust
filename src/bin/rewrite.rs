//! The search driver: `rewrite search` (the precision ladder), `rewrite
//! forward` (one forward pass with timing), `rewrite ckhash` (checkpoint
//! fingerprints), `rewrite export-ui` (the web UI's data).

use anyhow::Result;
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
    /// The rebuilt search (src/frame.rs): find the minimal winning frame by
    /// running the full precision ladder (Bits 0..=15 then Exact) at rising
    /// horizons until one is confirmed at every level through concrete.
    Search {
        /// Lowest horizon to test; level 0 is extended past it until it
        /// first wins, then the ladder tests each horizon until Confirmed.
        #[arg(long, default_value_t = 1)]
        from: u32,
        /// Last horizon to try.
        #[arg(long)]
        to: Option<u32>,
        /// Deepest bits rung (>=16 means Exact is the top rung).
        #[arg(long, default_value_t = 15)]
        maxk: u8,
        /// Base checkpoint dir (per-horizon, per-level subdirs land under it).
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// Start room "x,y".
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Optional synthetic win "x,y" (CELESTE_WIN_AT_XY) for a cheap run.
        #[arg(long)]
        win_at: Option<String>,
        /// COUNT DOWN from a known concrete solution's frame (the replayed
        /// TAS, `frame::find_optimum_from_ceiling`) instead of up from
        /// level 0's first win: the ceiling must confirm, then each horizon
        /// below is tested until one is refuted. Two ladder runs when the
        /// ceiling is optimal. `--from`/`--to` are ignored.
        #[arg(long)]
        ceiling: Option<u32>,
    },
    /// ONE forward pass at ONE precision level, exactly as the ladder runs it
    /// (record mode, position partition, sharded checkpoints), with the
    /// per-frame timing line. The profiling entry point for the forward.
    Forward {
        /// Last frame to compute.
        #[arg(long)]
        to: u32,
        /// Rem precision: bits 0..=15, or >=16 for Exact.
        #[arg(long, default_value_t = 0)]
        k: u8,
        /// Checkpoint dir (frames land under it).
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// Start room "x,y".
        #[arg(long, default_value = "1,0")]
        room: String,
    },
    /// Fingerprint a forward checkpoint tree: per frame, the lane count and an
    /// order-independent hash of its (row key, cell) set. Two runs that agree
    /// here reached the same states; the format they stored them in is
    /// irrelevant.
    /// Ladder diagnostic: are the FINE level's backward-marked states (its
    /// winning paths) at horizon `fine_h`, widened to the COARSE precision,
    /// all marked - and all reachable - at the coarse level at `coarse_h`?
    /// The first frame where one is not says which abstraction promise
    /// broke. Also checks the coarse level against itself: every coarse
    /// marked state re-keyed through the filter's own widening must pass
    /// the filter.
    DiagMarks {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        #[arg(long)]
        fine_h: u32,
        #[arg(long)]
        fine_level: usize,
        #[arg(long)]
        coarse_h: u32,
        #[arg(long)]
        coarse_level: usize,
        /// The coarse level's rem bits.
        #[arg(long)]
        coarse_bits: u8,
        #[arg(long, default_value = "1,0")]
        room: String,
    },
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
        /// The rung to run at: bits 0..=15, or 16 for Exact (default 0).
        #[arg(long, default_value_t = 0)]
        precision: u8,
        /// A coarser level's marks file: run under its MarkFilter (the
        /// finer levels' path), widening to `--coarser` bits.
        #[arg(long)]
        filter: Option<String>,
        #[arg(long, default_value_t = 0)]
        coarser: u8,
        /// Record the frame's edges (into `<level dir>/bench-edges`, compacted
        /// and deleted per rep, both timed) - the search's forward path.
        #[arg(long, default_value_t = false)]
        edges: bool,
    },
    /// Microbenchmark of ONE backward: a level's tree and pos-graph
    /// (`frames/` and `posgraph.bin` under `level_dir`, as `rewrite forward`
    /// writes them), the walk at `horizon`, timed.
    BenchBackward {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 99)]
        horizon: u32,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Run BOTH the kernel walk and the BFS and print the states marked
        /// by only one of them (with their layer and cell).
        #[arg(long, default_value_t = false)]
        diff: bool,
    },
    /// DIAGNOSTIC: how a waypoint partition would behave. Group the
    /// level's states at `--frame` by the `--square`-pixel square their
    /// player stands in (the biggest 63 squares, the rest lumped), push
    /// the groups forward through the recorded edges to `--to`, and print
    /// per frame each group's states, their sum (the work of running the
    /// groups separately), the distinct states (the work of one run), and
    /// how many states belong to several groups.
    PartitionProbe {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 50)]
        frame: u32,
        #[arg(long, default_value_t = 69)]
        to: u32,
        #[arg(long, default_value_t = 16)]
        square: i32,
    },
    /// DIAGNOSTIC: how much a distance-to-exit bound would prune under a
    /// known ceiling. Reverse-BFS the level's recorded position graph
    /// from the top of the room (the exit is `y < -4`; the goal cells are
    /// the graph's cells within 8 px of its highest), then per frame
    /// count the states whose cell is more frames from the goal than the
    /// ceiling leaves. An ESTIMATE: the graph only has the edges the
    /// forward recorded (missing edges over-state the pruning) and the
    /// goal is the top region, not the exit (under-states it).
    PruneProbe {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 95)]
        ceiling: u32,
        #[arg(long, default_value_t = 50)]
        from: u32,
        #[arg(long, default_value_t = 69)]
        to: u32,
    },
    /// DIAGNOSTIC: how FULL the cells along the spawn-to-top diagonal are.
    /// Per sampled cell, over every frame to `--to`: the states ever
    /// there (each state is in one frame file, its layer), the dominant
    /// shape's product of per-column cardinalities (the largest set of
    /// states those fields could combine into), the fullness ratio, the
    /// distinct player (spd.x, spd.y) pairs and the states per pair.
    FullnessProbe {
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 69)]
        to: u32,
        /// Explicit cells `x,y;x,y;...` (start-room-relative pixels);
        /// default: `--auto` cells along the spawn-to-top segment.
        #[arg(long)]
        cells: Option<String>,
        #[arg(long, default_value_t = 10)]
        auto: usize,
    },
    /// DIAGNOSTIC: the post-hoc census of a POSITION widening. For each
    /// listed frame, the distinct states with the player's x/y bucketed
    /// to 1, 2, 4 and 8 px (every other field exact) - the ceiling of
    /// what a coarser position rung could merge, before its own spread.
    PosCensus {
        #[arg(long)]
        level_dir: String,
        /// Frames, comma-separated.
        #[arg(long)]
        frames: String,
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
    },
    /// Census of one checkpointed frame: how many distinct concrete player
    /// classes (position, spd, every scalar field) its rows fall into, how
    /// many rows each holds, and each field's cardinality - what a
    /// per-class kernel specialization would have to compile for. Also the
    /// visited set's shard model over frames 0..=frame.
    Census {
        /// A level dir (`frames/` under it).
        #[arg(long)]
        level_dir: String,
        #[arg(long, default_value_t = 70)]
        frame: u32,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// With `--to`: the MOTION-FREE kernel variants over frames
        /// `frame..=to` - per frame the distinct (shape, every scalar cell
        /// except the player's x, y, spd, rem) classes over the WHOLE row
        /// (every object, the globals), how many are new, and the union
        /// so far: what a motion-free specialization would compile.
        #[arg(long)]
        to: Option<u32>,
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
        #[arg(long)]
        trajectory: String,
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Stop once a win is reached (default), else follow to the end.
        #[arg(long, default_value_t = true)]
        stop_at_win: bool,
    },
    /// Extract a concrete input sequence that wins by `horizon` from one
    /// level's marks: a DFS from the initial state through the reference
    /// engine's concrete single-input step, admitting a successor only if
    /// it is a marked state in the NEXT BFS layer of that level's tree.
    /// Prints the input bytes for `concrete_run -i`, or reports where the
    /// marked chain has no concrete continuation (a spurious win).
    Witness {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        #[arg(long)]
        horizon: u32,
        /// Ladder level (0..=15 = Bits(k), 16 = Exact).
        #[arg(long, default_value_t = 16)]
        level: usize,
        #[arg(long, default_value = "1,0")]
        room: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Search {
            from,
            to,
            maxk,
            checkpoint_dir,
            room,
            win_at,
            ceiling,
        } => {
            use celeste_rust::frame::{find_optimum, Block, FrameStep};
            use celeste_rust::interpreter::abstraction::{set_level, Level, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            if let Some(xy) = &win_at {
                std::env::set_var("CELESTE_WIN_AT_XY", xy);
            }
            // THE LADDER. `CELESTE_LADDER="r0s16,r1sx,...,rxsx"`: an explicit
            // list of (rem rung, spd width) levels, coarsest first, for
            // experiments on the ladder itself (which refinement to take
            // first, how to interleave rem and speed). `CELESTE_LADDER_RUNGS`
            // ("0,0,1,16", 16 = Exact) is the older rem-only form (e.g. the
            // same rung twice: a second round at one precision narrows
            // nothing, the marks are a fixpoint). Otherwise rem Bits(0..=maxk)
            // then Exact, speed by the `CELESTE_SPD_LADDER` preset.
            let precisions: Vec<Level> = match (std::env::var("CELESTE_LADDER"), std::env::var("CELESTE_LADDER_RUNGS")) {
                (Ok(spec), _) => Level::parse_ladder(&spec).unwrap_or_else(|e| panic!("CELESTE_LADDER: {e}")),
                (_, Ok(list)) => list
                    .split(',')
                    .map(|k| k.trim().parse::<u8>().expect("CELESTE_LADDER_RUNGS: comma-separated rung numbers"))
                    .map(|k| Level::for_rem(if k >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(k) }))
                    .collect(),
                _ => Level::default_ladder(maxk),
            };
            eprintln!(
                "[ladder] levels: {}",
                precisions
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            let make_engine = |p: Level| {
                set_level(p);
                Ok(Box::new(celeste_rust::compiled::FrameEngine::new_for_start_room()?)
                    as Box<dyn FrameStep>)
            };
            let make_initial = || {
                Ok(vec![Block::from_state(
                    &celeste_rust::trace::refengine::RefEngine::new()?.initial_state()?,
                )?])
            };
            let dir = std::path::Path::new(&checkpoint_dir);
            celeste_rust::compiled::prebuild_kernels(&precisions);
            let found = if let Some(c) = ceiling {
                Some(celeste_rust::frame::find_optimum_from_ceiling(make_engine, make_initial, dir, c, &precisions)?)
            } else {
                find_optimum(make_engine, make_initial, dir, from, to.unwrap_or(from), &precisions)?
            };
            match found {
                Some(h) => println!("OPTIMAL win frame: {h}"),
                None => println!("no win confirmed up to horizon {}", to.unwrap_or(from)),
            }
        }
        Command::Forward {
            to,
            k,
            checkpoint_dir,
            room,
        } => {
            use celeste_rust::frame::{forward_run, Block};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            let precision = if k >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(k) };
            set_rem_precision(precision);
            let t = std::time::Instant::now();
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            eprintln!("[fwd] engine up in {:.2} s ({precision:?})", t.elapsed().as_secs_f64());
            let initial = vec![Block::from_state(
                &celeste_rust::trace::refengine::RefEngine::new()?.initial_state()?,
            )?];
            let dir = std::path::Path::new(&checkpoint_dir);
            let t = std::time::Instant::now();
            let fwd = forward_run(&engine, initial, dir, to, true, None)?;
            let wall = t.elapsed().as_secs_f64();
            match fwd.win_frame {
                Some(h) => println!("win at f{h} ({wall:.2} s)"),
                None => println!("no win by f{} ({wall:.2} s)", fwd.frames),
            }
            if let Some(pg) = &fwd.pos_graph {
                let (pairs, fp) = pg.fingerprint();
                println!("posgraph f{:03} {pairs} {fp:016x}", fwd.frames);
            }
            celeste_rust::metrics::dump("forward", Some(dir), &[("k", k.to_string())]);
        }
        Command::DiagMarks {
            checkpoint_dir,
            fine_h,
            fine_level,
            coarse_h,
            coarse_level,
            coarse_bits,
            room,
        } => {
            use celeste_rust::frame::{load_frame, widened_keys, MarkFilter, Visited};
            use celeste_rust::interpreter::abstraction::RemPrecision;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let base = std::path::Path::new(&checkpoint_dir);
            let level_dir = |h: u32, l: usize| {
                if l == 0 {
                    base.join("level00")
                } else {
                    base.join(format!("h{:03}", h)).join(format!("level{:02}", l))
                }
            };
            let fine_dir = level_dir(fine_h, fine_level);
            let coarse_dir = level_dir(coarse_h, coarse_level);
            let fine_marks =
                Visited::load(&celeste_rust::frame::marks_path(base, fine_h, fine_level))?;
            let coarse_marks =
                Visited::load(&celeste_rust::frame::marks_path(base, coarse_h, coarse_level))?;
            let coarser = celeste_rust::interpreter::abstraction::Level::for_rem(RemPrecision::Bits(coarse_bits));
            println!(
                "fine (h{fine_h} level {fine_level}): {} marked; coarse (h{coarse_h} level {coarse_level}, {coarser:?}): {} marked",
                fine_marks.len(),
                coarse_marks.len()
            );
            // Self-consistency of the coarse level: its own marked rows,
            // re-keyed through the filter's widening, must pass the filter.
            let filter = MarkFilter::new(&coarse_marks, coarser);
            let mut self_fail = 0usize;
            let mut self_total = 0usize;
            for f in 1..=coarse_h {
                for block in load_frame(&coarse_dir, f)? {
                    let cells = block.positions()?;
                    let shape = block.shard_shape();
                    let marked: Vec<bool> = block
                        .keys()
                        .iter()
                        .zip(&cells)
                        .map(|(k, &c)| coarse_marks.contains(shape, *k, c))
                        .collect();
                    if !marked.iter().any(|&m| m) {
                        continue;
                    }
                    let allow = filter.allowed(block.rt2())?;
                    for (i, &m) in marked.iter().enumerate() {
                        if m {
                            self_total += 1;
                            if !allow[i] {
                                self_fail += 1;
                            }
                        }
                    }
                }
            }
            println!("coarse self-check: {self_fail} of {self_total} marked rows FAIL their own filter");
            // The fine level's winning paths against the coarse marks.
            let mut first_miss: Option<String> = None;
            for f in 1..=fine_h.min(coarse_h) {
                let (mut n, mut marked_ok, mut reach_ok) = (0usize, 0usize, 0usize);
                let coarse_blocks = load_frame(&coarse_dir, f)?;
                let mut reachable = Visited::new();
                for b in &coarse_blocks {
                    let cells = b.positions()?;
                    for (k, &c) in b.keys().iter().zip(&cells) {
                        reachable.insert(b.shard_shape(), *k, c);
                    }
                }
                for block in load_frame(&fine_dir, f)? {
                    let cells = block.positions()?;
                    let shape = block.shard_shape();
                    let mask: Vec<bool> = block
                        .keys()
                        .iter()
                        .zip(&cells)
                        .map(|(k, &c)| fine_marks.contains(shape, *k, c))
                        .collect();
                    let Some(win_rows) = block.keep(&mask) else { continue };
                    let (ws, wk, wc) = widened_keys(&win_rows, coarser)?;
                    let fine_cells = win_rows.positions()?;
                    for i in 0..win_rows.lanes() {
                        n += 1;
                        let m = coarse_marks.contains(ws, wk[i], wc[i]);
                        let r = reachable.contains(ws, wk[i], wc[i]);
                        marked_ok += m as usize;
                        reach_ok += r as usize;
                        if !m && first_miss.is_none() {
                            let xy = celeste_rust::search::pos_graph::cell_xy(fine_cells[i]);
                            first_miss = Some(format!(
                                "f{f:03}: fine winning state at {xy:?} (key {:016x}{:016x}) widened -> key {:016x}{:016x} cell {} : coarse-marked {m}, coarse-REACHABLE {r}",
                                win_rows.keys()[i].0, win_rows.keys()[i].1, wk[i].0, wk[i].1, wc[i]
                            ));
                        }
                    }
                }
                println!("f{f:03}: fine winning states {n}, widened & coarse-marked {marked_ok}, widened & coarse-reachable {reach_ok}");
            }
            match first_miss {
                Some(m) => println!("FIRST MISS: {m}"),
                None => println!("no miss: every fine winning state's widening is coarse-marked"),
            }
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
            precision,
            filter,
            coarser,
            edges,
        } => {
            use celeste_rust::frame::{forward_frame, load_frame, threads, Block, MarkFilter, Visited};
            use celeste_rust::search::door::Door;
            use celeste_rust::interpreter::abstraction::{set_level, Level, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            let rung = |k: u8| Level::for_rem(if k >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(k) });
            set_level(rung(precision));
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = match &level_dir {
                Some(d) => std::path::PathBuf::from(d),
                None => std::path::Path::new(&checkpoint_dir).join("level00"),
            };
            let marks: Option<Visited> = match &filter {
                Some(p) => Some(Visited::load(std::path::Path::new(p))?),
                None => None,
            };
            let mark_filter = marks.as_ref().map(|m| MarkFilter::new(m, rung(coarser)));
            let mark_filter = mark_filter.as_ref();
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
                forward_frame(&engine, small, &Door::new(), None, mark_filter, frame + 1, None)?;
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
                    forward_frame(&engine, input, door, None, mark_filter, frame + 1, edges.then_some(edges_dir.as_path()))?;
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
                    "[bench] rep {rep}: raw {} kept {} | wave {:.0} ms (idle {:.0}%) door {:.0} total {:.0} ms | flushes {} ({:.0} rows avg) | {} out blocks | edges {} written {:.0} compact {:.0} ms | filter {:.0} thread-ms",
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
                    ms(st.t_filter),
                );
            }
            let _ = std::fs::remove_dir_all(&edges_dir);
            celeste_rust::compiled::dispatch::print_kernel_hits();
            celeste_rust::compiled::dispatch::print_bodysets();
        }
        Command::Census {
            level_dir,
            frame,
            room,
            to,
        } => {
            use celeste_engine::runtime2::{Cell2, Col, AV};
            use celeste_rust::frame::{frame_files, load_frame};
            use rustc_hash::FxHashMap;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let dir = std::path::Path::new(&level_dir);
            let ids = celeste_rust::compiled::ids();
            if let Some(to) = to {
                let mix = celeste_engine::runtime2::mix64;
                let mut union: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
                let mut by_cell: FxHashMap<(u64, u32), u64> = FxHashMap::default();
                let mut by_class: FxHashMap<(u64, u64), u64> = FxHashMap::default();
                let mut by_cell_class: FxHashMap<(u64, u32, u64), u64> = FxHashMap::default();
                println!("[variants] frame | rows | shapes | classes this frame | new | union so far");
                for f in frame..=to {
                    let blocks = load_frame(dir, f)?;
                    let mut here: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
                    let mut rows = 0usize;
                    let mut shapes: rustc_hash::FxHashSet<u64> = Default::default();
                    for b in &blocks {
                        let rt2 = b.rt2();
                        rows += rt2.width;
                        shapes.insert(rt2.shape_hash);
                        // The six motion cells of the player, if any.
                        let mut motion: Vec<u32> = Vec::new();
                        if let Some(obj) = celeste_rust::search::pos_graph::player_object(rt2) {
                            motion.extend(rt2.obj_field_cell(obj, ids.f_x));
                            motion.extend(rt2.obj_field_cell(obj, ids.f_y));
                            for sub in [ids.f_spd, ids.f_rem] {
                                if let Some(c) = rt2.obj_field_cell(obj, sub) {
                                    if let Col::U(AV::Ptr(sp)) = rt2.cols[c as usize] {
                                        motion.extend(rt2.obj_field_cell(sp, ids.f_x));
                                        motion.extend(rt2.obj_field_cell(sp, ids.f_y));
                                    }
                                }
                            }
                        }
                        let mut h: Vec<u64> = vec![rt2.shape_hash; rt2.width];
                        for (c, col) in rt2.cols.iter().enumerate() {
                            if !matches!(rt2.structure[c], Cell2::Val) || motion.contains(&(c as u32)) {
                                continue;
                            }
                            let fold = |acc: u64, v: AV| -> u64 {
                                let (k, a, b) = match v {
                                    AV::Num(n) => (0u64, n.as_raw_u32() as u64, 0u64),
                                    AV::Ival(a, b) => (1, a.as_raw_u32() as u64, b.as_raw_u32() as u64),
                                    AV::Bool(x) => (2, x as u64, 0),
                                    AV::UBool => (3, 0, 0),
                                    AV::Str(s) => (4, s as u64, 0),
                                    AV::Nil => (5, 0, 0),
                                    AV::Ptr(p) => (6, p as u64, 0),
                                    AV::NilPtr => (7, 0, 0),
                                };
                                mix(acc ^ mix((c as u64) << 56 | k << 48 | a << 16 ^ b))
                            };
                            match col {
                                Col::U(v) => {
                                    let hv = fold(0, *v);
                                    for x in h.iter_mut() {
                                        *x = mix(*x ^ hv);
                                    }
                                }
                                _ => {
                                    for (r, x) in h.iter_mut().enumerate() {
                                        *x = mix(*x ^ fold(0, col.at(r)));
                                    }
                                }
                            }
                        }
                        let cells = b.positions()?;
                        for (r, x) in h.into_iter().enumerate() {
                            here.insert((rt2.shape_hash, x));
                            if f == to || f % 10 == 0 {
                                *by_cell.entry((rt2.shape_hash, cells[r])).or_default() += 1;
                                *by_class.entry((rt2.shape_hash, x)).or_default() += 1;
                                *by_cell_class.entry((rt2.shape_hash, cells[r], x)).or_default() += 1;
                            }
                        }
                    }
                    let new = here.iter().filter(|k| !union.contains(k)).count();
                    union.extend(here.iter().copied());
                    println!("[variants] f{f:03} | {rows} | {} | {} | {new} | {}", shapes.len(), here.len(), union.len());
                    if f == to || f % 10 == 0 {
                        let dist = |name: &str, sizes: Vec<u64>| {
                            let mut c = sizes;
                            c.sort_unstable();
                            let n = c.len().max(1);
                            let total: u64 = c.iter().sum();
                            let small: u64 = c.iter().filter(|&&k| k < 16).sum();
                            println!(
                                "[buckets] f{f:03} {name}: {} buckets; rows/bucket median {} p90 {} max {}; {:.1}% of rows in buckets < 16",
                                c.len(),
                                c[n / 2],
                                c[n * 9 / 10],
                                c[n - 1],
                                100.0 * small as f64 / total.max(1) as f64
                            );
                        };
                        dist("(shape, cell)", by_cell.drain().map(|(_, v)| v).collect());
                        dist("(shape, class)", by_class.drain().map(|(_, v)| v).collect());
                        dist("(shape, cell, class)", by_cell_class.drain().map(|(_, v)| v).collect());
                    }
                }
                println!("[variants] union over f{frame}..=f{to}: {} (shape, motion-free class) variants", union.len());
                return Ok(());
            }
            let blocks = load_frame(dir, frame)?;
            let rows: usize = blocks.iter().map(|b| b.lanes()).sum();
            println!("[census] {} f{frame}: {} blocks, {rows} rows", dir.display(), blocks.len());
            // Per row: the player's scalar fields (one level of sub-tables).
            // `(name, value code)` with AV encoded as 3 words.
            let enc = |v: AV| -> (u32, u32, u32) {
                match v {
                    AV::Num(n) => (0, n.as_raw_u32(), 0),
                    AV::Ival(a, b) => (1, a.as_raw_u32(), b.as_raw_u32()),
                    AV::Bool(x) => (2, x as u32, 0),
                    AV::UBool => (3, 0, 0),
                    AV::Str(s) => (4, s, 0),
                    AV::Nil => (5, 0, 0),
                    AV::Ptr(p) => (6, p, 0),
                    AV::NilPtr => (7, 0, 0),
                }
            };
            let mut names: Vec<String> = Vec::new();
            let mut card: Vec<FxHashMap<(u32, u32, u32), u64>> = Vec::new();
            let mut classes: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
            let mut classes_xy: FxHashMap<(u32, u32), u64> = FxHashMap::default();
            let mut classes_xys: FxHashMap<(u32, u32, u32, u32), u64> = FxHashMap::default();
            let mut classes_spd: FxHashMap<(u32, u32), u64> = FxHashMap::default();
            let (mut card_sx, mut card_sy): (FxHashMap<u32, u64>, FxHashMap<u32, u64>) = Default::default();
            let mut classes_w16: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
            let mut classes_w14: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
            let mut classes_nopos: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
            // Every scalar except the player's MOTION (x, y, spd, rem): the
            // variants a motion-free kernel specialization would compile.
            let mut classes_nomotion: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
            let mut no_player = 0usize;
            for b in &blocks {
                let rt2 = b.rt2();
                let Some(obj) = celeste_rust::search::pos_graph::player_object(rt2) else {
                    no_player += b.lanes();
                    continue;
                };
                // The field list of this shape's player (name -> cell).
                let mut fields: Vec<(String, u32)> = Vec::new();
                if let Cell2::Obj(fs) = &rt2.structure[obj as usize] {
                    for &(fid, cell) in fs {
                        let name = celeste_names::FIELD_NAMES[fid as usize].to_string();
                        match &rt2.structure[cell as usize] {
                            Cell2::Val => match &rt2.cols[cell as usize] {
                                Col::U(AV::Ptr(sub)) => {
                                    if let Cell2::Obj(sfs) = &rt2.structure[*sub as usize] {
                                        for &(sfid, scell) in sfs {
                                            if matches!(rt2.structure[scell as usize], Cell2::Val) {
                                                fields.push((format!("{name}.{}", celeste_names::FIELD_NAMES[sfid as usize]), scell));
                                            }
                                        }
                                    }
                                }
                                _ => fields.push((name, cell)),
                            },
                            Cell2::Obj(sfs) => {
                                for &(sfid, scell) in sfs {
                                    if matches!(rt2.structure[scell as usize], Cell2::Val) {
                                        fields.push((format!("{name}.{}", celeste_names::FIELD_NAMES[sfid as usize]), scell));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                for (name, _) in &fields {
                    if !names.contains(name) {
                        names.push(name.clone());
                        card.push(FxHashMap::default());
                    }
                }
                let (cx, cy) = (rt2.obj_field_cell(obj, ids.f_x), rt2.obj_field_cell(obj, ids.f_y));
                let spd = rt2.obj_field_cell(obj, ids.f_spd).and_then(|c| match rt2.cols[c as usize] {
                    Col::U(AV::Ptr(s)) => Some(s),
                    _ => None,
                });
                let (sx, sy) = match spd {
                    Some(s) => (rt2.obj_field_cell(s, ids.f_x), rt2.obj_field_cell(s, ids.f_y)),
                    None => (None, None),
                };
                for lane in 0..rt2.width {
                    let mut tuple: Vec<(u32, u32, u32)> = Vec::with_capacity(names.len());
                    let mut nopos: Vec<(u32, u32, u32)> = Vec::with_capacity(names.len());
                    let mut nomotion: Vec<(u32, u32, u32)> = Vec::with_capacity(names.len());
                    let mut spd_w16: Vec<(u32, u32, u32)> = Vec::with_capacity(names.len());
                    let mut spd_w14: Vec<(u32, u32, u32)> = Vec::with_capacity(names.len());
                    for (name, cell) in &fields {
                        let v = enc(rt2.cols[*cell as usize].at(lane));
                        let ni = names.iter().position(|n| n == name).unwrap();
                        *card[ni].entry(v).or_default() += 1;
                        if name.starts_with("rem.") {
                            continue;
                        }
                        tuple.push(v);
                        // The same class with spd bucketed to 2^w raw units
                        // (w=16: 1 px/frame, w=14: 1/4 px): what a level-0
                        // spd widening would leave.
                        if name.starts_with("spd.") {
                            let raw = v.1 as i32;
                            spd_w16.push((v.0, raw.div_euclid(1 << 16) as u32, 0));
                            spd_w14.push((v.0, raw.div_euclid(1 << 14) as u32, 0));
                        } else {
                            spd_w16.push(v);
                            spd_w14.push(v);
                        }
                        if name != "x" && name != "y" {
                            nopos.push(v);
                            if !name.starts_with("spd.") {
                                nomotion.push(v);
                            }
                        }
                    }
                    *classes.entry(tuple).or_default() += 1;
                    *classes_w16.entry(spd_w16).or_default() += 1;
                    *classes_w14.entry(spd_w14).or_default() += 1;
                    *classes_nopos.entry(nopos).or_default() += 1;
                    *classes_nomotion.entry(nomotion).or_default() += 1;
                    if let (Some(cx), Some(cy)) = (cx, cy) {
                        let x = enc(rt2.cols[cx as usize].at(lane)).1;
                        let y = enc(rt2.cols[cy as usize].at(lane)).1;
                        *classes_xy.entry((x, y)).or_default() += 1;
                        if let (Some(sx), Some(sy)) = (sx, sy) {
                            let vx = enc(rt2.cols[sx as usize].at(lane));
                            let vy = enc(rt2.cols[sy as usize].at(lane));
                            *classes_xys.entry((x, y, vx.1 ^ vx.2.rotate_left(16), vy.1 ^ vy.2.rotate_left(16))).or_default() += 1;
                            *classes_spd.entry((vx.1 ^ vx.2.rotate_left(16), vy.1 ^ vy.2.rotate_left(16))).or_default() += 1;
                            *card_sx.entry(vx.1).or_default() += 1;
                            *card_sy.entry(vy.1).or_default() += 1;
                        }
                    }
                }
            }
            let dist = |m: &FxHashMap<Vec<(u32, u32, u32)>, u64>| -> String {
                let mut c: Vec<u64> = m.values().copied().collect();
                c.sort_unstable();
                let n = c.len().max(1);
                format!("{} classes; rows/class median {} p90 {} max {}", c.len(), c[n / 2], c[n * 9 / 10], c[n - 1])
            };
            println!("[census] rows without a player object: {no_player}");
            println!("[census] distinct (x, y): {}", classes_xy.len());
            println!("[census] distinct (x, y, spd.x, spd.y): {}", classes_xys.len());
            let frac = |m: &FxHashMap<u32, u64>| -> (usize, f64) {
                let n = m.len();
                let rows: u64 = m.values().sum();
                let fractional: u64 = m.iter().filter(|(v, _)| **v & 0xffff != 0).map(|(_, n)| *n).sum();
                (n, 100.0 * fractional as f64 / rows.max(1) as f64)
            };
            let (nx, fx) = frac(&card_sx);
            let (ny, fy) = frac(&card_sy);
            println!(
                "[census] distinct (spd.x, spd.y): {}; spd.x {nx} distinct ({fx:.1}% of rows fractional), spd.y {ny} distinct ({fy:.1}% fractional)",
                classes_spd.len()
            );
            println!("[census] all scalar fields except rem: {}", dist(&classes));
            println!("[census]   same with spd bucketed to 1 px (w=16): {}", dist(&classes_w16));
            println!("[census]   same with spd bucketed to 1/4 px (w=14): {}", dist(&classes_w14));
            println!("[census] all scalar fields except rem and x, y: {}", dist(&classes_nopos));
            println!("[census] all scalar fields except rem, x, y and spd (motion-free classes): {}", dist(&classes_nomotion));
            println!("[census] per-field cardinality (distinct values over the frame):");
            let mut order: Vec<usize> = (0..names.len()).collect();
            order.sort_by_key(|&i| std::cmp::Reverse(card[i].len()));
            for i in order {
                let mut top: Vec<(&(u32, u32, u32), &u64)> = card[i].iter().collect();
                top.sort_by_key(|(_, n)| std::cmp::Reverse(**n));
                let p8 = |raw: u32| -> String {
                    let r = raw as i32;
                    if r & 0xffff == 0 { format!("{}", r >> 16) } else { format!("{:.3}", r as f64 / 65536.0) }
                };
                let show = |v: &(u32, u32, u32)| -> String {
                    match v.0 {
                        0 => p8(v.1),
                        1 => format!("[{},{}]", p8(v.1), p8(v.2)),
                        2 => format!("{}", v.1 == 1),
                        3 => "?".into(),
                        _ => format!("#{}", v.1),
                    }
                };
                let tops: Vec<String> = top.iter().take(4).map(|(v, n)| format!("{}:{:.0}%", show(v), 100.0 * **n as f64 / rows as f64)).collect();
                println!("  {:20} {:6} distinct   {}", names[i], card[i].len(), tops.join(" "));
            }
            // The visited set's shard model over frames 0..=frame: entries
            // per (shape, cell), and hashbrown's bytes for them.
            let mut shards: FxHashMap<(u64, u32), u64> = FxHashMap::default();
            for f in 0..=frame {
                for file in frame_files(dir, f)? {
                    for (cell, n) in file.cell_counts() {
                        *shards.entry((file.shape_hash(), cell)).or_default() += n as u64;
                    }
                }
            }
            let entries: u64 = shards.values().sum();
            let modeled: u64 = shards
                .values()
                .map(|&n| {
                    // hashbrown: buckets = next_power_of_two(n * 8 / 7), 16 B key + 1 control byte each, plus 16 B of header.
                    let buckets = ((n * 8).div_ceil(7)).max(4).next_power_of_two();
                    buckets * 17 + 16 + 64
                })
                .sum();
            println!(
                "[census] visited model through f{frame}: {} shards, {entries} entries, ~{:.2} GB in hashbrown sets ({:.0} B/entry); shards with <= 8 entries: {}",
                shards.len(),
                modeled as f64 / 1e9,
                modeled as f64 / entries.max(1) as f64,
                shards.values().filter(|&&n| n <= 8).count()
            );
        }
        Command::BenchBackward {
            level_dir,
            horizon,
            room,
            diff,
        } => {
            use celeste_rust::frame::{backward_run, pos_graph_path, threads};
            use celeste_rust::interpreter::abstraction::{set_level, Level, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_level(Level::for_rem(RemPrecision::Bits(0)));
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = std::path::Path::new(&level_dir);
            let graph = celeste_rust::search::pos_graph::PosGraph::load(&pos_graph_path(dir))?;
            eprintln!("[bench] level-0 tree {}, pos-graph {} pairs; {} threads", dir.display(), graph.pairs(), threads());
            let t = std::time::Instant::now();
            if diff {
                let kern = backward_run(&engine, dir, horizon, &graph)?;
                let bfs = celeste_rust::search::edges::backward(dir, horizon)?;
                let a = kern.marked.entries();
                let b = bfs.marked.entries();
                let sa: std::collections::BTreeSet<_> = a.iter().copied().collect();
                let sb: std::collections::BTreeSet<_> = b.iter().copied().collect();
                // Where a state lives: its (layer, seq, row) through the tree.
                let mut where_is: rustc_hash::FxHashMap<(u64, u32, (u64, u64)), Vec<u64>> = Default::default();
                for layer in 0..=horizon {
                    for (seq, file) in celeste_rust::frame::frame_files_seq(dir, layer)? {
                        let cells = file.row_cells();
                        for row in 0..file.width() {
                            let k = (file.shape_hash(), cells[row as usize], file.key_at(row));
                            if sa.contains(&k) != sb.contains(&k) {
                                where_is.entry(k).or_default().push(celeste_rust::frame::pack_id(layer, seq, row));
                            }
                        }
                    }
                }
                println!("[diff] kernel {} states, bfs {} states", a.len(), b.len());
                let edges = celeste_rust::search::edges::EdgeGraph::open(&dir.join("edges"), horizon)?;
                let only_kernel: Vec<_> = sa.difference(&sb).copied().collect();
                let only_bfs: Vec<_> = sb.difference(&sa).copied().collect();
                println!("[diff] only kernel: {} states; only bfs: {} states", only_kernel.len(), only_bfs.len());
                let mut by_layer: std::collections::BTreeMap<u32, usize> = Default::default();
                for k in &only_bfs {
                    for &id in where_is.get(k).map(|v| v.as_slice()).unwrap_or(&[]) {
                        *by_layer.entry(celeste_rust::frame::id_layer(id)).or_default() += 1;
                    }
                }
                println!("[diff] only-bfs states per layer: {:?}", by_layer);
                let shown: Vec<_> = only_kernel.iter().map(|k| ("only kernel", *k)).chain(only_bfs.iter().take(6).map(|k| ("only bfs", *k))).collect();
                for (which, k) in shown {
                    let k = &k;
                    let ids: Vec<String> = where_is.get(k).map(|v| v.iter().map(|&id| format!("l{} s{} r{}", celeste_rust::frame::id_layer(id), celeste_rust::frame::id_seq(id), celeste_rust::frame::id_row(id))).collect()).unwrap_or_default();
                    println!("[diff] {which}: shape {:016x} cell {} key {:016x}{:016x} at {:?}", k.0, k.1, k.2 .0, k.2 .1, ids);
                    // Expand the disputed row alone and look at its successors.
                    for &id in where_is.get(k).map(|v| v.as_slice()).unwrap_or(&[]) {
                        let (layer, seq, row) = (celeste_rust::frame::id_layer(id), celeste_rust::frame::id_seq(id), celeste_rust::frame::id_row(id));
                        let files = celeste_rust::frame::frame_files_seq(dir, layer)?;
                        let (_, file) = files.iter().find(|(s, _)| *s == seq).expect("the file");
                        let win = file.win_rows().iter().any(|&(r, _)| r == row);
                        let rt2 = file.load_rows(&[row..row + 1])?.expect("the row");
                        let block = celeste_rust::frame::Block::with_ids(rt2, vec![id], seq);
                        let tmp = dir.join("diff-edges");
                        let _ = std::fs::remove_dir_all(&tmp);
                        let door = celeste_rust::search::door::Door::new();
                        let (next, _won, _st) = celeste_rust::frame::forward_frame(&engine, vec![block], &door, None, None, layer + 1, Some(&tmp))?;
                        println!("[diff]   win row: {win}; successors: {} blocks", next.len());
                        // Every recorded edge FROM this row, against its real successors.
                        let claimed = edges.edges_from(layer + 1, &[id]);
                        let mut real: Vec<(u64, (u64, u64), u32)> = Vec::new();
                        for b in &next {
                            let cells = celeste_rust::search::pos_graph::block_cells(b.rt2())?;
                            for (i, key) in b.keys().iter().enumerate() {
                                real.push((b.rt2().shape_hash, *key, cells[i]));
                            }
                        }
                        for &(_, t) in &claimed {
                            let (tl, ts, tr) = (celeste_rust::frame::id_layer(t), celeste_rust::frame::id_seq(t), celeste_rust::frame::id_row(t));
                            let files = celeste_rust::frame::frame_files_seq(dir, tl)?;
                            let desc = match files.iter().find(|(s, _)| *s == ts) {
                                Some((_, f)) if tr < f.width() => {
                                    let k = (f.shape_hash(), f.key_at(tr), f.row_cells()[tr as usize]);
                                    let is_real = real.contains(&k);
                                    format!("cell {} key {:016x}{:016x} real successor: {is_real}, marked kernel {} bfs {}", k.2, k.1 .0, k.1 .1, kern.marked.contains(k.0, k.1, k.2), bfs.marked.contains(k.0, k.1, k.2))
                                }
                                _ => "NOT IN THE TREE".to_string(),
                            };
                            println!("[diff]   recorded edge -> l{tl} s{ts} r{tr}: {desc}");
                        }
                        println!("[diff]   ({} recorded edges from this row, {} real successors)", claimed.len(), real.len());
                        // Real successors with no record.
                        let mut recorded_keys: Vec<(u64, (u64, u64), u32)> = Vec::new();
                        for &(_, t) in &claimed {
                            let (tl, ts, tr) = (celeste_rust::frame::id_layer(t), celeste_rust::frame::id_seq(t), celeste_rust::frame::id_row(t));
                            let files = celeste_rust::frame::frame_files_seq(dir, tl)?;
                            if let Some((_, f)) = files.iter().find(|(s, _)| *s == ts) {
                                if tr < f.width() {
                                    recorded_keys.push((f.shape_hash(), f.key_at(tr), f.row_cells()[tr as usize]));
                                }
                            }
                        }
                        for k in &real {
                            if !recorded_keys.contains(k) {
                                println!("[diff]   UNRECORDED real successor: cell {} key {:016x}{:016x}", k.2, k.1 .0, k.1 .1);
                            }
                        }
                        if which == "only bfs" && std::env::var_os("CELESTE_DIFF_RERUN").is_some() {
                            // The whole frame again with the tree's door: is the
                            // spurious edge reproduced?
                            let state = celeste_rust::frame::ForwardState::resume(dir, false)?.expect("a tree");
                            let blocks = celeste_rust::frame::load_frame(dir, layer)?;
                            let tmp2 = dir.join("diff-edges-full");
                            let _ = std::fs::remove_dir_all(&tmp2);
                            let _ = celeste_rust::frame::forward_frame(&engine, blocks, state.door(), None, None, layer + 1, Some(&tmp2))?;
                            celeste_rust::search::edges::compact_frame(&tmp2, layer + 1)?;
                            let g2 = celeste_rust::search::edges::EdgeGraph::open(&tmp2, layer + 1)?;
                            let again = g2.edges_from(layer + 1, &[id]);
                            let spurious: Vec<u64> = claimed.iter().map(|&(_, t)| t).filter(|&t| {
                                let (tl, ts, tr) = (celeste_rust::frame::id_layer(t), celeste_rust::frame::id_seq(t), celeste_rust::frame::id_row(t));
                                let files = celeste_rust::frame::frame_files_seq(dir, tl).unwrap();
                                match files.iter().find(|(s, _)| *s == ts) { Some((_, f)) if tr < f.width() => !real.contains(&(f.shape_hash(), f.key_at(tr), f.row_cells()[tr as usize])), _ => true }
                            }).collect();
                            for t in &spurious {
                                println!("[diff]   full re-run f{}: spurious target l{} s{} r{} recorded again: {}", layer + 1, celeste_rust::frame::id_layer(*t), celeste_rust::frame::id_seq(*t), celeste_rust::frame::id_row(*t), again.iter().any(|&(_, x)| x == *t));
                            }
                            println!("[diff]   full re-run f{}: {} recorded edges from this row (was {})", layer + 1, again.len(), claimed.len());
                            let _ = std::fs::remove_dir_all(&tmp2);
                            std::process::exit(0);
                        }
                        for b in &next {
                            let cells = celeste_rust::search::pos_graph::block_cells(b.rt2())?;
                            for (i, key) in b.keys().iter().enumerate() {
                                let shape = b.rt2().shape_hash;
                                let km = kern.marked.contains(shape, *key, cells[i]);
                                let bm = bfs.marked.contains(shape, *key, cells[i]);
                                if !km && !bm {
                                    continue;
                                }
                                // Where is this successor in the tree?
                                let mut found: Vec<u64> = Vec::new();
                                for l2 in 0..=horizon {
                                    for (s2, f2) in celeste_rust::frame::frame_files_seq(dir, l2)? {
                                        if f2.shape_hash() != shape {
                                            continue;
                                        }
                                        let c2 = f2.row_cells();
                                        for r2 in 0..f2.width() {
                                            if f2.key_at(r2) == *key && c2[r2 as usize] == cells[i] {
                                                found.push(celeste_rust::frame::pack_id(l2, s2, r2));
                                            }
                                        }
                                    }
                                }
                                let mut recorded = Vec::new();
                                for &t in &found {
                                    let mut buf = Vec::new();
                                    edges.preds_at(t, layer + 1, &mut buf);
                                    recorded.push((t, buf.iter().filter(|e| e.base <= id && id < e.base + 64 && e.mask & (1u64 << (id - e.base)) != 0).count(), buf.len()));
                                }
                                println!(
                                    "[diff]   succ cell {} key {:016x}{:016x} marked kernel {km} bfs {bm}; in tree at {:?}; edges from this row at f{}: {:?}",
                                    cells[i], key.0, key.1,
                                    found.iter().map(|&t| format!("l{} s{} r{}", celeste_rust::frame::id_layer(t), celeste_rust::frame::id_seq(t), celeste_rust::frame::id_row(t))).collect::<Vec<_>>(),
                                    layer + 1,
                                    recorded
                                );
                            }
                        }
                        let _ = std::fs::remove_dir_all(&tmp);
                        let _ = std::fs::remove_dir_all(&tmp);
                    }
                }
                return Ok(());
            }
            if celeste_rust::frame::bfs_backward() {
                let bwd = celeste_rust::search::edges::backward(dir, horizon)?;
                let (n, fp) = bwd.marked.fingerprint();
                println!(
                    "[bench] BFS backward h{horizon}: {:.2} s, marked {n} (fingerprint {fp:016x}), {} edges read",
                    t.elapsed().as_secs_f64(),
                    bwd.stats.edges_read
                );
                return Ok(());
            }
            let bwd = backward_run(&engine, dir, horizon, &graph)?;
            let (n, fp) = bwd.marked.fingerprint();
            println!(
                "[bench] backward h{horizon}: {:.2} s, marked {n} (fingerprint {fp:016x}), {} re-runs",
                t.elapsed().as_secs_f64(),
                bwd.reruns
            );
            celeste_rust::compiled::dispatch::print_kernel_hits();
            celeste_rust::metrics::dump("bench-backward", None, &[]);
        }
        Command::PartitionProbe { level_dir, frame, to, square } => {
            use celeste_rust::search::checkpoint::FrameFile;
            use celeste_rust::search::edges::{EdgeGraph, GroupMasks};
            use celeste_rust::search::pos_graph::cell_xy;
            let dir = std::path::Path::new(&level_dir);
            let fdir = dir.join("frames").join(format!("f{frame:03}"));
            // Square -> (count, rows) over the frame's files.
            let mut files: Vec<(u32, FrameFile)> = Vec::new();
            for e in std::fs::read_dir(&fdir)? {
                let p = e?.path();
                let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("").to_string();
                if !(name.starts_with('s') && name.ends_with(".bin")) {
                    continue;
                }
                let seq: u32 = name.trim_end_matches(".bin").rsplit('_').next().unwrap().parse()?;
                files.push((seq, FrameFile::open(&p)?));
            }
            let mut per_square: std::collections::BTreeMap<(i32, i32), u64> = Default::default();
            let mut row_sq: Vec<Vec<(i32, i32)>> = Vec::new();
            for (_, f) in &files {
                let sq: Vec<(i32, i32)> = f
                    .row_cells()
                    .iter()
                    .map(|&c| match cell_xy(c) {
                        Some((x, y)) => (x.div_euclid(square), y.div_euclid(square)),
                        None => (i32::MIN, i32::MIN),
                    })
                    .collect();
                for s in &sq {
                    *per_square.entry(*s).or_default() += 1;
                }
                row_sq.push(sq);
            }
            let mut ranked: Vec<((i32, i32), u64)> = per_square.iter().map(|(k, v)| (*k, *v)).collect();
            ranked.sort_by_key(|&(_, n)| std::cmp::Reverse(n));
            let total: u64 = ranked.iter().map(|x| x.1).sum();
            let mut group_of: std::collections::HashMap<(i32, i32), u32> = Default::default();
            let mut names: Vec<String> = Vec::new();
            for (i, (sq, n)) in ranked.iter().enumerate() {
                let g = i.min(63) as u32;
                group_of.insert(*sq, g);
                if i < 63 {
                    names.push(format!("sq({},{}) {}px n={}", sq.0 * square, sq.1 * square, square, n));
                } else if i == 63 {
                    names.push(format!("rest ({} squares)", ranked.len() - 63));
                }
            }
            println!("frame {frame}: {total} states in {} squares of {square} px -> {} groups", ranked.len(), names.len());
            for (g, nm) in names.iter().enumerate() {
                println!("  g{g:02} {nm}");
            }
            let mut masks: GroupMasks = Default::default();
            for ((seq, f), sq) in files.iter().zip(&row_sq) {
                let v: Vec<u64> = sq.iter().map(|s| 1u64 << group_of[s]).collect();
                assert_eq!(v.len(), f.width() as usize);
                masks.insert((frame, *seq), v);
            }
            let graph = EdgeGraph::open(&dir.join("edges"), to)?;
            {
                let (seq, f) = &files[0];
                let id = celeste_rust::frame::pack_id(frame, *seq, 0);
                let succ = graph.edges_from(frame + 1, &[id]);
                println!(
                    "graph: {} records, {} MB; probe state l{frame} s{seq} r0 (of {} rows): {} successors at f{}",
                    graph.records,
                    graph.bytes >> 20,
                    f.width(),
                    succ.len(),
                    frame + 1
                );
            }
            println!("frame | per-group states (g00..)                | sum | distinct | in>1 groups | old-layer targets");
            for f in frame + 1..=to {
                let t = std::time::Instant::now();
                let (next, old) = graph.push_groups(f, &masks);
                let mut per = vec![0u64; names.len()];
                let (mut distinct, mut multi) = (0u64, 0u64);
                for v in next.values() {
                    for &m in v {
                        if m == 0 {
                            continue;
                        }
                        distinct += 1;
                        if m.count_ones() > 1 {
                            multi += 1;
                        }
                        let mut b = m;
                        while b != 0 {
                            per[b.trailing_zeros() as usize] += 1;
                            b &= b - 1;
                        }
                    }
                }
                let sum: u64 = per.iter().sum();
                let top: Vec<String> = per.iter().take(8).map(|n| n.to_string()).collect();
                println!(
                    "f{f:03} | {} | {sum} | {distinct} | {multi} | {old} | {:.1} s",
                    top.join(" "),
                    t.elapsed().as_secs_f64()
                );
                masks = next;
            }
        }
        Command::PruneProbe { level_dir, ceiling, from, to } => {
            use celeste_rust::search::checkpoint::FrameFile;
            use celeste_rust::search::pos_graph::{cell_xy, PosGraph, CELL_COUNT};
            let dir = std::path::Path::new(&level_dir);
            let graph = PosGraph::load(&celeste_rust::frame::pos_graph_path(dir))?;
            // Live cells and the room's top.
            let mut live = vec![false; CELL_COUNT];
            let mut top = i32::MAX;
            for d in 0..CELL_COUNT as u32 {
                let srcs = graph.srcs_of(d);
                if srcs.is_empty() {
                    continue;
                }
                live[d as usize] = true;
                for &sc in srcs {
                    live[sc as usize] = true;
                }
                if let Some((_, y)) = cell_xy(d) {
                    top = top.min(y);
                }
            }
            let mut dist = vec![u32::MAX; CELL_COUNT];
            let mut queue = std::collections::VecDeque::new();
            for c in 0..CELL_COUNT as u32 {
                if live[c as usize] && cell_xy(c).is_some_and(|(_, y)| y <= top + 8) {
                    dist[c as usize] = 0;
                    queue.push_back(c);
                }
            }
            let goals = queue.len();
            while let Some(d) = queue.pop_front() {
                let nd = dist[d as usize] + 1;
                for &sc in graph.srcs_of(d) {
                    if dist[sc as usize] == u32::MAX {
                        dist[sc as usize] = nd;
                        queue.push_back(sc);
                    }
                }
            }
            let reached = dist.iter().filter(|&&d| d != u32::MAX).count();
            println!("pos graph: {} pairs; top y = {top}; {goals} goal cells; {reached} cells reach the goal", graph.pairs());
            println!("frame | states | dead under ceiling {ceiling} | unreachable-in-graph | dist histogram (0-9,10-19,..)");
            for f in from..=to {
                let fdir = dir.join("frames").join(format!("f{f:03}"));
                let (mut n, mut dead, mut unreach) = (0u64, 0u64, 0u64);
                let mut hist = vec![0u64; 12];
                for e in std::fs::read_dir(&fdir)? {
                    let p = e?.path();
                    let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("").to_string();
                    if !(name.starts_with('s') && name.ends_with(".bin")) {
                        continue;
                    }
                    let ff = FrameFile::open(&p)?;
                    for (cell, rows) in ff.cell_counts() {
                        n += rows as u64;
                        let d = dist[cell as usize];
                        if d == u32::MAX {
                            unreach += rows as u64;
                            continue;
                        }
                        hist[(d as usize / 10).min(11)] += rows as u64;
                        if f + d > ceiling {
                            dead += rows as u64;
                        }
                    }
                }
                let h: Vec<String> = hist.iter().map(|x| x.to_string()).collect();
                println!("f{f:03} | {n} | {dead} ({:.1}%) | {unreach} | {}", 100.0 * dead as f64 / n.max(1) as f64, h.join(" "));
            }
        }
        Command::FullnessProbe { level_dir, to, cells, auto } => {
            use celeste_engine::runtime2::{Col, AV};
            use celeste_rust::search::checkpoint::FrameFile;
            use celeste_rust::search::pos_graph::{cell_of, cell_xy, player_object, NO_CELL};
            let dir = std::path::Path::new(&level_dir);
            let frame_files = |f: u32| -> Result<Vec<std::path::PathBuf>> {
                let fdir = dir.join("frames").join(format!("f{f:03}"));
                let mut v = Vec::new();
                if !fdir.is_dir() {
                    return Ok(v);
                }
                for e in std::fs::read_dir(&fdir)? {
                    let p = e?.path();
                    let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("").to_string();
                    if name.starts_with('s') && name.ends_with(".bin") {
                        v.push(p);
                    }
                }
                Ok(v)
            };
            // Rows per cell over all frames; the spawn cell (first frame with a
            // located player) and the top (lowest y among cells with >= 100 rows).
            let mut rows_at: std::collections::HashMap<u32, u64> = Default::default();
            let mut spawn: Option<u32> = None;
            for f in 0..=to {
                for p in frame_files(f)? {
                    let ff = FrameFile::open(&p)?;
                    for (cell, n) in ff.cell_counts() {
                        if cell == NO_CELL {
                            continue;
                        }
                        *rows_at.entry(cell).or_default() += n as u64;
                        if spawn.is_none() {
                            spawn = Some(cell);
                        }
                    }
                }
            }
            let spawn = spawn.ok_or_else(|| anyhow::anyhow!("no located player in the tree"))?;
            let (sx, sy) = cell_xy(spawn).unwrap();
            let (mut tx, mut ty) = (sx, i32::MAX);
            for (&c, &n) in &rows_at {
                if n >= 100 {
                    let (x, y) = cell_xy(c).unwrap();
                    if y < ty {
                        (tx, ty) = (x, y);
                    }
                }
            }
            let chosen: Vec<u32> = match cells {
                Some(spec) => spec
                    .split(';')
                    .map(|xy| {
                        let (x, y) = xy.split_once(',').ok_or_else(|| anyhow::anyhow!("cell {xy}"))?;
                        cell_of(x.trim().parse()?, y.trim().parse()?)
                    })
                    .collect::<Result<_>>()?,
                None => {
                    let mut v = Vec::new();
                    for t in 0..=auto {
                        let px = sx + (tx - sx) * t as i32 / auto as i32;
                        let py = sy + (ty - sy) * t as i32 / auto as i32;
                        let mut best: Option<(u32, u64)> = None;
                        for dx in -4..=4 {
                            for dy in -4..=4 {
                                if let Ok(c) = cell_of(px + dx, py + dy) {
                                    let n = rows_at.get(&c).copied().unwrap_or(0);
                                    if n > 0 && best.is_none_or(|b| n > b.1) {
                                        best = Some((c, n));
                                    }
                                }
                            }
                        }
                        if let Some((c, _)) = best {
                            if !v.contains(&c) {
                                v.push(c);
                            }
                        }
                    }
                    v
                }
            };
            println!(
                "spawn ({sx},{sy}), top ({tx},{ty}); {} cells with rows, {} rows total; sampling {} cells",
                rows_at.len(),
                rows_at.values().sum::<u64>(),
                chosen.len()
            );
            let ids = celeste_rust::compiled::ids();
            for cell in chosen {
                let (cx, cy) = cell_xy(cell).unwrap();
                // per shape: rows, per-column value sets, spd pairs
                struct ShapeStats {
                    rows: u64,
                    cols: Vec<std::collections::HashSet<String>>,
                    uniform: Vec<bool>,
                    spd: std::collections::HashSet<(u32, u32)>,
                }
                let mut by_shape: std::collections::HashMap<u64, ShapeStats> = Default::default();
                for f in 0..=to {
                    for p in frame_files(f)? {
                        let ff = FrameFile::open(&p)?;
                        let ranges = ff.rows_of_cell(cell);
                        if ranges.is_empty() {
                            continue;
                        }
                        let Some(rt2) = ff.load_rows(&ranges)? else { continue };
                        let st = by_shape.entry(ff.shape_hash()).or_insert_with(|| ShapeStats {
                            rows: 0,
                            cols: vec![Default::default(); rt2.cols.len()],
                            uniform: vec![true; rt2.cols.len()],
                            spd: Default::default(),
                        });
                        st.rows += rt2.width as u64;
                        for (ci, col) in rt2.cols.iter().enumerate() {
                            match col {
                                Col::U(av) => {
                                    st.cols[ci].insert(format!("{av:?}"));
                                }
                                _ => {
                                    st.uniform[ci] = false;
                                    for lane in 0..rt2.width {
                                        st.cols[ci].insert(format!("{:?}", col.at(lane)));
                                    }
                                }
                            }
                        }
                        if let Some(obj) = player_object(&rt2) {
                            if let Some(pc) = rt2.obj_field_cell(obj, ids.f_spd) {
                                if let Col::U(AV::Ptr(sub)) = rt2.cols[pc as usize] {
                                    if let (Some(cxs), Some(cys)) = (rt2.obj_field_cell(sub, ids.f_x), rt2.obj_field_cell(sub, ids.f_y)) {
                                        for lane in 0..rt2.width {
                                            let raw = |av: AV| match av {
                                                AV::Num(p) => p.as_raw_u32(),
                                                AV::Ival(a, _) => a.as_raw_u32() ^ 0x8000_0000,
                                                _ => u32::MAX,
                                            };
                                            st.spd.insert((raw(rt2.cols[cxs as usize].at(lane)), raw(rt2.cols[cys as usize].at(lane))));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                let total: u64 = by_shape.values().map(|s| s.rows).sum();
                let Some((shape, st)) = by_shape.iter().max_by_key(|(_, s)| s.rows) else {
                    println!("cell ({cx},{cy}): no rows");
                    continue;
                };
                let mut cards: Vec<(usize, usize)> = st
                    .cols
                    .iter()
                    .enumerate()
                    .filter(|(ci, _)| !st.uniform[*ci])
                    .map(|(ci, set)| (set.len(), ci))
                    .collect();
                cards.sort_unstable_by(|a, b| b.cmp(a));
                let product: f64 = cards.iter().map(|&(n, _)| n as f64).product();
                let top: Vec<String> = cards.iter().take(8).map(|&(n, ci)| format!("c{ci}:{n}")).collect();
                println!(
                    "cell ({cx},{cy}): {total} states in {} shapes; dominant shape {shape:#x}: {} states, {} varying cols, product {:.3e}, fullness {:.2e}; spd pairs {}, states/pair {:.1}; top cols {}",
                    by_shape.len(),
                    st.rows,
                    cards.len(),
                    product,
                    st.rows as f64 / product,
                    st.spd.len(),
                    st.rows as f64 / st.spd.len().max(1) as f64,
                    top.join(" ")
                );
            }
        }
        Command::PosCensus { level_dir, frames } => {
            use celeste_engine::runtime2::{mix64, Cell2, Col, AV};
            use celeste_rust::frame::load_frame;
            let dir = std::path::Path::new(&level_dir);
            let ids = celeste_rust::compiled::ids();
            // (x bucket, y bucket) in px: square buckets, then each axis alone.
            const SIZES: [(i32, i32); 9] = [(1, 1), (2, 2), (4, 4), (8, 8), (2, 1), (1, 2), (4, 1), (1, 4), (4, 2)];
            println!("frame | rows | distinct at (x,y) px: {}", SIZES.iter().map(|s| format!("{}x{}", s.0, s.1)).collect::<Vec<_>>().join(" | "));
            for f in frames.split(',') {
                let f: u32 = f.trim().parse()?;
                let blocks = load_frame(dir, f)?;
                let mut sets: Vec<rustc_hash::FxHashSet<u64>> = vec![Default::default(); SIZES.len()];
                let mut rows = 0usize;
                for b in &blocks {
                    let rt2 = b.rt2();
                    rows += rt2.width;
                    let (px, py) = match celeste_rust::search::pos_graph::player_object(rt2) {
                        Some(obj) => (rt2.obj_field_cell(obj, ids.f_x), rt2.obj_field_cell(obj, ids.f_y)),
                        None => (None, None),
                    };
                    // Hash of everything but the player's x/y, per row.
                    let mut h: Vec<u64> = vec![rt2.shape_hash; rt2.width];
                    let fold = |acc: u64, c: usize, v: AV| -> u64 {
                        let (k, a, bb) = match v {
                            AV::Num(n) => (0u64, n.as_raw_u32() as u64, 0u64),
                            AV::Ival(a, bb) => (1, a.as_raw_u32() as u64, bb.as_raw_u32() as u64),
                            AV::Bool(x) => (2, x as u64, 0),
                            AV::UBool => (3, 0, 0),
                            AV::Str(s) => (4, s as u64, 0),
                            AV::Nil => (5, 0, 0),
                            AV::Ptr(p) => (6, p as u64, 0),
                            AV::NilPtr => (7, 0, 0),
                        };
                        mix64(acc ^ mix64((c as u64) << 56 | k << 48 | a << 16 ^ bb))
                    };
                    for (c, col) in rt2.cols.iter().enumerate() {
                        if !matches!(rt2.structure[c], Cell2::Val) || Some(c as u32) == px || Some(c as u32) == py {
                            continue;
                        }
                        match col {
                            Col::U(v) => {
                                let hv = fold(0, c, *v);
                                for x in h.iter_mut() {
                                    *x = mix64(*x ^ hv);
                                }
                            }
                            _ => {
                                for (r, x) in h.iter_mut().enumerate() {
                                    *x = mix64(*x ^ fold(0, c, col.at(r)));
                                }
                            }
                        }
                    }
                    let whole = |c: Option<u32>, r: usize| -> i32 {
                        match c.map(|c| rt2.cols[c as usize].at(r)) {
                            Some(AV::Num(n)) => n.whole_part_as_i16() as i32,
                            _ => i32::MIN / 2,
                        }
                    };
                    for r in 0..rt2.width {
                        let (x, y) = (whole(px, r), whole(py, r));
                        for (si, &(sx, sy)) in SIZES.iter().enumerate() {
                            let bx = x.div_euclid(sx) as u64 as u32 as u64;
                            let by = y.div_euclid(sy) as u64 as u32 as u64;
                            sets[si].insert(mix64(h[r] ^ mix64(bx << 32 | by)));
                        }
                    }
                }
                let d: Vec<String> = sets.iter().map(|s| s.len().to_string()).collect();
                println!(
                    "f{f:03} | {rows} | {} | ratios: {}",
                    d.join(" | "),
                    sets.iter().map(|s| format!("{:.2}x", sets[0].len() as f64 / s.len().max(1) as f64)).collect::<Vec<_>>().join(" ")
                );
            }
        }
        Command::ExportUi {
            checkpoint_dir,
            log,
            out,
            room,
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
            )?;
        }
        Command::Trajectory {
            trajectory,
            room,
            stop_at_win,
        } => {
            use celeste_rust::frame::{wins_of, Block};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_rem_precision(RemPrecision::Exact);
            let traj: Vec<Option<(i32, i32)>> = std::fs::read_to_string(&trajectory)?
                .lines()
                .map(|l| l.trim())
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .map(|l| {
                    if l == "-" {
                        None
                    } else {
                        let (x, y) = l.split_once(',').expect("x,y");
                        Some((x.trim().parse().expect("x"), y.trim().parse().expect("y")))
                    }
                })
                .collect();
            let mut eng = celeste_rust::trace::refengine::RefEngine::new()?;
            let initial = eng.initial_state()?;
            // (state, the inputs that led to it)
            let mut layer: Vec<(celeste_rust::interpreter::state::State, Vec<u8>)> = vec![(initial.clone(), Vec::new())];
            for (i, want) in traj.iter().enumerate() {
                let f = i as u32 + 1;
                let mut next: Vec<(celeste_rust::interpreter::state::State, Vec<u8>)> = Vec::new();
                let mut seen: rustc_hash::FxHashSet<(u64, u64, u32)> = Default::default();
                let mut tried = 0usize;
                // A `-` frame (any position: the spawn) takes only the idle
                // input; the inputs before the player exists cannot matter
                // to a witness, and 64 x 24 frames of them would.
                let inputs: std::ops::Range<u8> = if want.is_some() { 0..64 } else { 0..1 };
                for (st, path) in &layer {
                    for byte in inputs.clone() {
                        let mut s = st.clone();
                        celeste_rust::concrete::set_concrete_buttons(&mut s, byte)?;
                        let mut succ = eng.run_frame_concrete(&s)?;
                        celeste_rust::concrete::restore_buttons(&initial, &mut succ)?;
                        tried += 1;
                        let block = Block::from_state(&succ)?;
                        let cell = block.positions()?[0];
                        let pos = celeste_rust::search::pos_graph::cell_xy(cell);
                        if let Some(w) = want {
                            if pos != Some(*w) {
                                continue;
                            }
                        }
                        let key = block.keys()[0];
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
                eprintln!(
                    "[trajectory] f{f:03} want {:?}: {} states ({tried} steps)",
                    want,
                    next.len()
                );
                if next.is_empty() {
                    anyhow::bail!("the trajectory cannot be followed at f{f} (wanted {:?})", want);
                }
                layer = next;
            }
            println!("[trajectory] followed to the end: {} states; one input sequence:", layer.len());
            println!("{}", layer[0].1.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
        }
        Command::Witness {
            checkpoint_dir,
            horizon,
            level,
            room,
        } => {
            use celeste_rust::frame::{frame_files, marks_path, widened_keys, wins_of, Block, Visited};
            use celeste_rust::interpreter::abstraction::{set_level, Level, RemPrecision};
            use rustc_hash::FxHashMap;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let precision = Level::for_rem(if level >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(level as u8) });
            set_level(precision);
            let base = std::path::Path::new(&checkpoint_dir);
            let dir = if level == 0 {
                base.join("level00")
            } else {
                base.join(format!("h{:03}", horizon)).join(format!("level{:02}", level))
            };
            let marks = Visited::load(&marks_path(base, horizon, level))?;
            // (key, cell) -> the layer it was first reached at.
            let mut layer_of: FxHashMap<(u64, u64, u32), u32> = FxHashMap::default();
            for f in 0..=horizon {
                for file in frame_files(&dir, f)? {
                    if let Some(rt2) = file.load_all()? {
                        let b = Block::from_rt2(rt2);
                        let cells = b.positions()?;
                        for (k, &c) in b.keys().iter().zip(&cells) {
                            layer_of.entry((k.0, k.1, c)).or_insert(f);
                        }
                    }
                }
            }
            eprintln!(
                "[witness] h{horizon} level {level} ({precision:?}): {} marks, {} rows in the tree",
                marks.len(),
                layer_of.len()
            );
            let mut eng = celeste_rust::trace::refengine::RefEngine::new()?;
            let initial = eng.initial_state()?;
            {
                let b = Block::from_state(&initial)?;
                let (_, keys, cells) = widened_keys(&b, precision)?;
                let id = (keys[0].0, keys[0].1, cells[0]);
                eprintln!(
                    "[witness] initial state: cell {} layer {:?} (expected Some(0))",
                    cells[0],
                    layer_of.get(&id)
                );
            }
            // The search's rows carry the buttons as the boundary leaves
            // them (read symbolically inside the frame); a concretely
            // stepped state carries the bytes it was stepped with. Restore
            // the boundary's representation before keying.
            let mut dead: rustc_hash::FxHashSet<(u64, u64, u32)> = Default::default();
            let mut path: Vec<u8> = Vec::new();
            let mut steps: u64 = 0;
            fn dfs(
                eng: &mut celeste_rust::trace::refengine::RefEngine,
                initial: &celeste_rust::interpreter::state::State,
                state: &celeste_rust::interpreter::state::State,
                f: u32,
                horizon: u32,
                precision: Level,
                marks: &Visited,
                layer_of: &FxHashMap<(u64, u64, u32), u32>,
                dead: &mut rustc_hash::FxHashSet<(u64, u64, u32)>,
                path: &mut Vec<u8>,
                steps: &mut u64,
                dir: &std::path::Path,
            ) -> Result<bool> {
                if f >= horizon {
                    return Ok(false);
                }
                for byte in 0u8..64 {
                    let mut s = state.clone();
                    celeste_rust::concrete::set_concrete_buttons(&mut s, byte)?;
                    let mut succ = eng.run_frame_concrete(&s)?;
                    celeste_rust::concrete::restore_buttons(initial, &mut succ)?;
                    *steps += 1;
                    let block = Block::from_state(&succ)?;
                    if wins_of(block.rt2())?.iter().any(|&w| w) {
                        path.push(byte);
                        eprintln!("[witness] WIN at f{} via input {}", f + 1, byte);
                        return Ok(true);
                    }
                    let (shape, keys, cells) = widened_keys(&block, precision)?;
                    let id = (keys[0].0, keys[0].1, cells[0]);
                    if f == 0 && byte == 0 && layer_of.get(&id) != Some(&1) {
                        eprintln!(
                            "[witness] f1 successor: cell {} layer {:?} marked {} - diffing against layer 1",
                            cells[0],
                            layer_of.get(&id),
                            marks.contains(shape, keys[0], cells[0])
                        );
                        // Column-by-column diff of the widened concrete
                        // successor against every layer-1 row.
                        let mut mine = block.into_rt2();
                        if let RemPrecision::Bits(b) = precision.rem {
                            mine.widen_to(celeste_rust::compiled::ids(), b, celeste_rust::frame::spd_width_log2(precision.spd), (precision.pos.x, precision.pos.y));
                        }
                        for file in frame_files(dir, 1)? {
                            let Some(theirs) = file.load_all()? else { continue };
                            eprintln!(
                                "[witness]   layer-1 file shape {:#x} width {} (mine shape {:#x}); structures {}",
                                theirs.shape_hash,
                                theirs.width,
                                mine.shape_hash,
                                if theirs.structure == mine.structure { "EQUAL" } else { "DIFFER" }
                            );
                            let name_of = |cell: usize| -> String {
                                for (g, &c) in mine.globals.iter().enumerate() {
                                    if c as usize == cell {
                                        return format!("global {}", celeste_names::GLOBAL_NAMES[g]);
                                    }
                                }
                                for (obj, node) in mine.structure.iter().enumerate() {
                                    if let celeste_engine::runtime2::Cell2::Obj(fields) = node {
                                        for &(fid, c) in fields {
                                            if c as usize == cell {
                                                return format!("obj#{obj}.{}", celeste_names::FIELD_NAMES[fid as usize]);
                                            }
                                        }
                                    }
                                }
                                String::from("?")
                            };
                            let n = mine.cols.len().min(theirs.cols.len());
                            let mut shown = 0;
                            for c in 0..n {
                                if !matches!(mine.structure[c], celeste_engine::runtime2::Cell2::Val) {
                                    continue;
                                }
                                for lane in 0..theirs.width {
                                    let (a, b) = (mine.cols[c].at(0), theirs.cols[c].at(lane));
                                    if a != b && shown < 40 {
                                        eprintln!("[witness]   cell {c} ({}): mine {:?} vs theirs[{lane}] {:?}", name_of(c), a, b);
                                        shown += 1;
                                    }
                                }
                            }
                        }
                        anyhow::bail!("stopping after the diff");
                    }
                    if dead.contains(&id) || !marks.contains(shape, keys[0], cells[0]) {
                        continue;
                    }
                    if layer_of.get(&id) != Some(&(f + 1)) {
                        continue;
                    }
                    path.push(byte);
                    if dfs(eng, initial, &succ, f + 1, horizon, precision, marks, layer_of, dead, path, steps, dir)? {
                        return Ok(true);
                    }
                    path.pop();
                    dead.insert(id);
                }
                Ok(false)
            }
            let found = dfs(
                &mut eng, &initial, &initial, 0, horizon, precision, &marks, &layer_of, &mut dead, &mut path,
                &mut steps, &dir,
            )?;
            eprintln!("[witness] {} concrete steps, {} dead ends", steps, dead.len());
            if found {
                println!("win at f{}: {}", path.len(), path.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(","));
            } else {
                println!("NO WITNESS: no marked chain has a concrete continuation to a win by f{horizon}");
            }
        }
    }

    Ok(())
}
