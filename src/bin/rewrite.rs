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
    },
    /// PROTOTYPE (plans/waves.md): record one real frame's emission stream
    /// - every row the kernels emit after the within-call dedup, as
    /// (shape, cell, key), unit by unit, with the input canonicalized to
    /// cell-sorted chunks per shape and run in wave order - to a file for
    /// `bench-door`.
    DumpEmissions {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        /// The INPUT frame (level 0); the stream is what frame+1 is made of.
        #[arg(long, default_value_t = 80)]
        frame: u32,
        #[arg(long, default_value = "1,0")]
        room: String,
        #[arg(long)]
        out: String,
        /// Lanes per kernel call (the within-call dedup window).
        #[arg(long, default_value_t = 16384)]
        grab: usize,
    },
    /// PROTOTYPE (plans/waves.md): the dedup tiers on a recorded emission
    /// stream, without the kernels: N workers pull the stream's units in
    /// order, push rows into fixed per-worker queue pools keyed by (shape,
    /// cell), flush full/evicted queues (sort, admit at the door, copy
    /// the survivors' payload into the worker's piece), then end the
    /// frame. The door is preloaded with layers 0..=frame. The admitted
    /// count must equal the real frame+1's row count.
    BenchDoor {
        #[arg(long, default_value = DEFAULT_CHECKPOINT_DIR)]
        checkpoint_dir: String,
        #[arg(long, default_value_t = 80)]
        frame: u32,
        #[arg(long)]
        dump: String,
        #[arg(long)]
        threads: Option<usize>,
        /// Queues per worker.
        #[arg(long, default_value_t = 256)]
        pool: usize,
        /// Rows per queue.
        #[arg(long, default_value_t = 4096)]
        queue_rows: usize,
        /// Payload bytes per row beyond the key (the typed columns).
        #[arg(long, default_value_t = 96)]
        payload: usize,
        /// `sorted` (base + delta), `hash` (today's sets).
        #[arg(long, default_value = "sorted")]
        door: String,
        #[arg(long, default_value_t = 3)]
        reps: usize,
        /// `wave`: every worker pulls the next unit (one wave, all workers
        /// in one band); `regions`: worker w takes the w-th contiguous
        /// block of units (its own band).
        #[arg(long, default_value = "wave")]
        assign: String,
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
        } => {
            use celeste_rust::frame::{find_optimum, Block, FrameStep};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            if let Some(xy) = &win_at {
                std::env::set_var("CELESTE_WIN_AT_XY", xy);
            }
            let precisions: Vec<RemPrecision> = (0u8..=maxk.min(15))
                .map(RemPrecision::Bits)
                .chain(std::iter::once(RemPrecision::Exact))
                .collect();
            let make_engine = |p: RemPrecision| {
                set_rem_precision(p);
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
            match find_optimum(make_engine, make_initial, dir, from, to.unwrap_or(from), &precisions)? {
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
            let coarser = RemPrecision::Bits(coarse_bits);
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
        } => {
            use celeste_rust::frame::{forward_frame, load_frame, threads, Block, Visited};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_rem_precision(RemPrecision::Bits(0));
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = std::path::Path::new(&checkpoint_dir).join("level00");
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
                let mut v: Vec<Visited> = (0..threads()).map(|_| Visited::new()).collect();
                forward_frame(&engine, small, &mut v, None, None)?;
            }
            for rep in 0..reps {
                let input: Vec<Block> = frontier.iter().map(|b| Block::from_rt2(b.rt2().clone_block())).collect();
                let mut visited: Vec<Visited> = (0..threads()).map(|_| Visited::new()).collect();
                let t = std::time::Instant::now();
                let (next, _won, st) = forward_frame(&engine, input, &mut visited, None, None)?;
                let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
                println!(
                    "[bench] rep {rep}: raw {} kept {} | emit {:.0} ms (idle {:.0}%) own {:.0} ms (idle {:.0}%) total {:.0} ms | {} out blocks",
                    st.lanes_raw,
                    st.lanes_kept,
                    ms(st.t_emit),
                    st.emit_idle * 100.0,
                    ms(st.t_own),
                    st.own_idle * 100.0,
                    ms(t.elapsed()),
                    next.len()
                );
            }
            celeste_rust::compiled::dispatch::print_kernel_hits();
        }
        Command::DumpEmissions {
            checkpoint_dir,
            frame,
            room,
            out,
            grab,
        } => {
            use celeste_rust::frame::{load_frame, Block, ForwardSink, FrameStep};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            use std::io::Write;
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_rem_precision(RemPrecision::Bits(0));
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = std::path::Path::new(&checkpoint_dir).join("level00");
            let frontier = canonical_frontier(load_frame(&dir, frame)?)?;
            let lanes: usize = frontier.iter().map(Block::lanes).sum();
            eprintln!("[dump] f{frame}: {} shapes, {lanes} lanes, canonical (cell-sorted)", frontier.len());
            // Units in wave order: every chunk's lane ranges, sorted by first cell.
            let cells: Vec<Vec<u32>> = frontier.iter().map(Block::positions).collect::<Result<_>>()?;
            let mut units: Vec<(u32, usize, usize, usize)> = Vec::new();
            for (bi, b) in frontier.iter().enumerate() {
                let mut lo = 0;
                while lo < b.lanes() {
                    let hi = (lo + grab).min(b.lanes());
                    units.push((cells[bi][lo], bi, lo, hi));
                    lo = hi;
                }
            }
            units.sort();
            let mut w = std::io::BufWriter::new(std::fs::File::create(&out)?);
            w.write_all(&(units.len() as u64).to_le_bytes())?;
            let mut sink = ForwardSink::new(1, false);
            let (mut rows_total, mut runs) = (0u64, 0u64);
            let t = std::time::Instant::now();
            for &(_, bi, lo, hi) in &units {
                sink.clear();
                engine.run(&frontier[bi], &cells[bi], lo..hi, &mut sink)?;
                let n: usize = sink.slots.iter().map(|s| s.rows()).sum();
                w.write_all(&((hi - lo) as u32).to_le_bytes())?;
                w.write_all(&(n as u32).to_le_bytes())?;
                let mut last = (u64::MAX, u32::MAX);
                for slot in &sink.slots {
                    for i in 0..slot.rows() {
                        let (k, c) = (slot.keys[i], slot.cells[i]);
                        if (slot.shape, c) != last {
                            runs += 1;
                            last = (slot.shape, c);
                        }
                        w.write_all(&slot.shape.to_le_bytes())?;
                        w.write_all(&c.to_le_bytes())?;
                        w.write_all(&k.0.to_le_bytes())?;
                        w.write_all(&k.1.to_le_bytes())?;
                    }
                }
                rows_total += n as u64;
            }
            w.flush()?;
            eprintln!(
                "[dump] {} units, {rows_total} rows ({:.1} per lane), {runs} (shape, cell) runs ({:.1} rows/run), {:.1} s -> {out}",
                units.len(),
                rows_total as f64 / lanes as f64,
                rows_total as f64 / runs.max(1) as f64,
                t.elapsed().as_secs_f64()
            );
        }
        Command::BenchDoor {
            checkpoint_dir,
            frame,
            dump,
            threads,
            pool,
            queue_rows,
            payload,
            door,
            reps,
            assign,
        } => {
            use celeste_rust::frame::frame_files;
            use celeste_rust::search::door::{Admit, Door, HashDoor, Key};
            use rustc_hash::FxHashMap;
            let workers = threads.unwrap_or_else(celeste_rust::frame::threads);
            let dir = std::path::Path::new(&checkpoint_dir).join("level00");
            // The stream.
            let t = std::time::Instant::now();
            let bytes = std::fs::read(&dump)?;
            let mut units: Vec<(u32, Vec<(u64, u32, Key)>)> = Vec::new();
            let mut p = 8usize;
            let n_units = u64::from_le_bytes(bytes[0..8].try_into().unwrap()) as usize;
            let rd32 = |p: usize| u32::from_le_bytes(bytes[p..p + 4].try_into().unwrap());
            let rd64 = |p: usize| u64::from_le_bytes(bytes[p..p + 8].try_into().unwrap());
            for _ in 0..n_units {
                let lanes = rd32(p);
                let n = rd32(p + 4) as usize;
                p += 8;
                let mut rows = Vec::with_capacity(n);
                for _ in 0..n {
                    rows.push((rd64(p), rd32(p + 8), (rd64(p + 12), rd64(p + 20))));
                    p += 28;
                }
                units.push((lanes, rows));
            }
            let stream_rows: usize = units.iter().map(|(_, r)| r.len()).sum();
            eprintln!("[bench-door] stream: {n_units} units, {stream_rows} rows, read in {:.1} s", t.elapsed().as_secs_f64());
            // The expected admissions: the real frame+1's rows.
            let expected: usize = frame_files(&dir, frame + 1)?.iter().map(|f| f.width() as usize).sum();
            // The door's base: layers 0..=frame, per (shape, cell).
            let t = std::time::Instant::now();
            let files: Vec<(u32, std::path::PathBuf)> = (0..=frame)
                .flat_map(|f| {
                    let fdir = dir.join("frames").join(format!("f{:03}", f));
                    std::fs::read_dir(&fdir)
                        .into_iter()
                        .flatten()
                        .filter_map(|e| e.ok().map(|e| e.path()))
                        .filter(|p| p.file_name().and_then(|s| s.to_str()).is_some_and(|n| n.starts_with('s') && n.ends_with(".bin")))
                        .map(move |p| (f, p))
                })
                .collect();
            let next = std::sync::atomic::AtomicUsize::new(0);
            let partial: Vec<FxHashMap<(u64, u32), Vec<Key>>> = std::thread::scope(|scope| {
                let hs: Vec<_> = (0..workers)
                    .map(|_| {
                        let (files, next) = (&files, &next);
                        scope.spawn(move || -> Result<FxHashMap<(u64, u32), Vec<Key>>> {
                            let mut m: FxHashMap<(u64, u32), Vec<Key>> = FxHashMap::default();
                            loop {
                                let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                let Some((_, path)) = files.get(i) else { break };
                                let file = celeste_rust::search::checkpoint::FrameFile::open(path)?;
                                let shape = file.shape_hash();
                                for (cell, key) in file.cell_keys() {
                                    m.entry((shape, cell)).or_default().push(key);
                                }
                            }
                            Ok(m)
                        })
                    })
                    .collect();
                hs.into_iter().map(|h| h.join().expect("loader")).collect::<Result<Vec<_>>>()
            })?;
            let mut shards: FxHashMap<(u64, u32), Vec<Key>> = FxHashMap::default();
            for m in partial {
                for (k, mut v) in m {
                    shards.entry(k).or_default().append(&mut v);
                }
            }
            let base_entries: usize = shards.values().map(Vec::len).sum();
            eprintln!(
                "[bench-door] base: layers 0..={frame}, {} shards, {base_entries} entries, loaded in {:.1} s; expecting {expected} admissions",
                shards.len(),
                t.elapsed().as_secs_f64()
            );
            for rep in 0..reps {
                let t = std::time::Instant::now();
                let d: Box<dyn Admit> = match door.as_str() {
                    "sorted" => Box::new(Door::from_shards(shards.clone())),
                    "hash" => Box::new(HashDoor::from_shards(shards.clone())),
                    other => anyhow::bail!("--door {other}: sorted | hash"),
                };
                let t_build = t.elapsed();
                let door_bytes0 = d.alloc_bytes();
                let rss0 = celeste_rust::metrics::current_rss_gb();
                let t = std::time::Instant::now();
                let next = std::sync::atomic::AtomicUsize::new(0);
                let regions = assign == "regions";
                let stats: Vec<WaveStats> = std::thread::scope(|scope| {
                    let hs: Vec<_> = (0..workers)
                        .map(|wi| {
                            let (units, next, d) = (&units, &next, d.as_ref());
                            scope.spawn(move || {
                                let mut w = Wave::new(pool, queue_rows, payload);
                                let per = units.len().div_ceil(workers);
                                let mut i = wi * per;
                                loop {
                                    let u = if regions {
                                        let u = i;
                                        i += 1;
                                        if u >= ((wi + 1) * per).min(units.len()) {
                                            break;
                                        }
                                        u
                                    } else {
                                        next.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                                    };
                                    let Some((_, rows)) = units.get(u) else { break };
                                    for &(shape, cell, key) in rows {
                                        w.push(shape, cell, key, d);
                                    }
                                }
                                w.finish(d)
                            })
                        })
                        .collect();
                    hs.into_iter().map(|h| h.join().expect("wave worker")).collect()
                });
                let t_wave = t.elapsed();
                let rss1 = celeste_rust::metrics::current_rss_gb();
                let t = std::time::Instant::now();
                d.end_frame(workers);
                let t_end = t.elapsed();
                let mut sum = WaveStats::default();
                for s in &stats {
                    sum.add(s);
                }
                let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
                println!(
                    "[bench-door] rep {rep} {door} {assign} x{workers}: admitted {} (expected {expected}{}) | wave {:.0} ms ({:.0} ns/row; busy {:.0}%; thread-ms push {:.0} sort {:.0} admit {:.0} gather {:.0}) end_frame {:.0} ms build {:.0} ms | flushes {} ({:.0} rows avg; {} full, {} evicted, {} final) run-cache {:.1}% | queues {:.2} GB pieces {:.2} GB door {:.2} -> {:.2} GB ({:.1} B/entry) rss {:.2} -> {:.2} GB",
                    sum.admitted,
                    if sum.admitted == expected { ", OK" } else { ", MISMATCH" },
                    ms(t_wave),
                    t_wave.as_nanos() as f64 / stream_rows as f64 * workers as f64,
                    100.0 * sum.busy.as_secs_f64() / (workers as f64 * t_wave.as_secs_f64()),
                    ms(sum.busy.saturating_sub(sum.t_sort + sum.t_admit + sum.t_gather)),
                    ms(sum.t_sort),
                    ms(sum.t_admit),
                    ms(sum.t_gather),
                    ms(t_end),
                    ms(t_build),
                    sum.flushes,
                    sum.flushed_rows as f64 / sum.flushes.max(1) as f64,
                    sum.full,
                    sum.evicted,
                    sum.final_,
                    100.0 * sum.cache_hits as f64 / stream_rows as f64,
                    (workers * pool * queue_rows * (payload + 20)) as f64 / 1e9,
                    sum.piece_bytes as f64 / 1e9,
                    door_bytes0 as f64 / 1e9,
                    d.alloc_bytes() as f64 / 1e9,
                    d.alloc_bytes() as f64 / d.len().max(1) as f64,
                    rss0,
                    rss1,
                );
                if sum.admitted != expected {
                    anyhow::bail!("admitted {} != expected {}", sum.admitted, expected);
                }
            }
        }
        Command::Census {
            level_dir,
            frame,
            room,
        } => {
            use celeste_engine::runtime2::{Cell2, Col, AV};
            use celeste_rust::frame::{frame_files, load_frame};
            use rustc_hash::FxHashMap;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let dir = std::path::Path::new(&level_dir);
            let ids = celeste_rust::compiled::ids();
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
            let mut classes_nopos: FxHashMap<Vec<(u32, u32, u32)>, u64> = FxHashMap::default();
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
                    for (name, cell) in &fields {
                        let v = enc(rt2.cols[*cell as usize].at(lane));
                        let ni = names.iter().position(|n| n == name).unwrap();
                        *card[ni].entry(v).or_default() += 1;
                        if name.starts_with("rem.") {
                            continue;
                        }
                        tuple.push(v);
                        if name != "x" && name != "y" {
                            nopos.push(v);
                        }
                    }
                    *classes.entry(tuple).or_default() += 1;
                    *classes_nopos.entry(nopos).or_default() += 1;
                    if let (Some(cx), Some(cy)) = (cx, cy) {
                        let x = enc(rt2.cols[cx as usize].at(lane)).1;
                        let y = enc(rt2.cols[cy as usize].at(lane)).1;
                        *classes_xy.entry((x, y)).or_default() += 1;
                        if let (Some(sx), Some(sy)) = (sx, sy) {
                            let vx = enc(rt2.cols[sx as usize].at(lane));
                            let vy = enc(rt2.cols[sy as usize].at(lane));
                            *classes_xys.entry((x, y, vx.1 ^ vx.2.rotate_left(16), vy.1 ^ vy.2.rotate_left(16))).or_default() += 1;
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
            println!("[census] all scalar fields except rem: {}", dist(&classes));
            println!("[census] all scalar fields except rem and x, y: {}", dist(&classes_nopos));
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
        } => {
            use celeste_rust::frame::{backward_run, pos_graph_path, threads};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            std::env::set_var("CELESTE_START_ROOM", &room);
            set_rem_precision(RemPrecision::Bits(0));
            let engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            let dir = std::path::Path::new(&level_dir);
            let graph = celeste_rust::search::pos_graph::PosGraph::load(&pos_graph_path(dir))?;
            eprintln!("[bench] level-0 tree {}, pos-graph {} pairs; {} threads", dir.display(), graph.pairs(), threads());
            let t = std::time::Instant::now();
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
        Command::Witness {
            checkpoint_dir,
            horizon,
            level,
            room,
        } => {
            use celeste_rust::frame::{frame_files, marks_path, widened_keys, wins_of, Block, Visited};
            use celeste_rust::interpreter::abstraction::{set_rem_precision, RemPrecision};
            use rustc_hash::FxHashMap;
            std::env::set_var("CELESTE_START_ROOM", &room);
            let precision = if level >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(level as u8) };
            set_rem_precision(precision);
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
            fn restore_buttons(
                from: &celeste_rust::interpreter::state::State,
                to: &mut celeste_rust::interpreter::state::State,
            ) -> Result<()> {
                use celeste_rust::interpreter::value::{HeapValue, Value};
                let arr_of = |st: &celeste_rust::interpreter::state::State| -> Result<Vec<_>> {
                    let cell = *st
                        .global_env
                        .get("__button_states")
                        .ok_or_else(|| anyhow::anyhow!("no __button_states"))?;
                    let arr = match st.heap.get_opt(cell) {
                        Some(HeapValue::Value(Value::Pointer(id))) => *id,
                        _ => cell,
                    };
                    let items = match st.heap.get_opt(arr) {
                        Some(HeapValue::ArrayTable(items)) => items.clone(),
                        other => anyhow::bail!("button array shape: {:?}", other),
                    };
                    Ok(items
                        .iter()
                        .map(|item| match st.heap.get_opt(*item) {
                            Some(HeapValue::Value(Value::Pointer(id))) => *id,
                            _ => *item,
                        })
                        .collect())
                };
                let src = arr_of(from)?;
                let dst = arr_of(to)?;
                anyhow::ensure!(src.len() == dst.len(), "button arrays differ in length");
                for (s, d) in src.iter().zip(&dst) {
                    let v = from.heap.get(*s).clone();
                    to.heap.set(*d, v);
                }
                Ok(())
            }
            // DFS with memoized dead ends per (key, cell).
            let mut dead: rustc_hash::FxHashSet<(u64, u64, u32)> = Default::default();
            let mut path: Vec<u8> = Vec::new();
            let mut steps: u64 = 0;
            fn dfs(
                eng: &mut celeste_rust::trace::refengine::RefEngine,
                initial: &celeste_rust::interpreter::state::State,
                state: &celeste_rust::interpreter::state::State,
                f: u32,
                horizon: u32,
                precision: RemPrecision,
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
                    restore_buttons(initial, &mut succ)?;
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
                        if let RemPrecision::Bits(b) = precision {
                            mine.widen_to(celeste_rust::compiled::ids(), b);
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

/// The frontier as ONE cell-sorted block per shape (plans/waves.md,
/// invariant 1): every block of a shape appended into one, then sorted by
/// (cell, key).
fn canonical_frontier(blocks: Vec<celeste_rust::frame::Block>) -> Result<Vec<celeste_rust::frame::Block>> {
    use celeste_rust::frame::Block;
    let mut by_shape: rustc_hash::FxHashMap<u64, celeste_engine::runtime2::Rt2> = Default::default();
    let mut order: Vec<u64> = Vec::new();
    for b in blocks {
        let shape = b.shard_shape();
        let rt2 = b.rt2();
        let rows: Vec<u32> = (0..rt2.width as u32).collect();
        let piece = by_shape.entry(shape).or_insert_with(|| {
            order.push(shape);
            let mut p = celeste_engine::slots::reshape(rt2, 0);
            p.shape_hash = rt2.shape_hash;
            p
        });
        piece.append_rows(rt2, &rows);
    }
    let mut out = Vec::new();
    for shape in order {
        let mut b = Block::from_rt2(by_shape.remove(&shape).unwrap());
        let cells = b.positions()?;
        let mut perm: Vec<u32> = (0..b.lanes() as u32).collect();
        perm.sort_unstable_by_key(|&i| (cells[i as usize], b.rt2().row_keys[i as usize]));
        b.rt2_mut().gather_lanes(&perm);
        out.push(b);
    }
    Ok(out)
}

/// PROTOTYPE of the worker's queue pool (plans/waves.md): fixed queues of
/// fixed capacity keyed by (shape, cell), a run cache, second-chance
/// eviction, flush = sort + door + payload copy into the worker's piece.
struct Queue {
    shape: u64,
    cell: u32,
    keys: Vec<(u64, u64)>,
    payload: Vec<u8>,
    touched: bool,
    live: bool,
}

#[derive(Default, Clone)]
struct WaveStats {
    admitted: usize,
    flushes: u64,
    flushed_rows: u64,
    full: u64,
    evicted: u64,
    final_: u64,
    cache_hits: u64,
    piece_bytes: usize,
    busy: std::time::Duration,
    t_sort: std::time::Duration,
    t_admit: std::time::Duration,
    t_gather: std::time::Duration,
}

impl WaveStats {
    fn add(&mut self, o: &WaveStats) {
        self.admitted += o.admitted;
        self.flushes += o.flushes;
        self.flushed_rows += o.flushed_rows;
        self.full += o.full;
        self.evicted += o.evicted;
        self.final_ += o.final_;
        self.cache_hits += o.cache_hits;
        self.piece_bytes += o.piece_bytes;
        self.busy += o.busy;
        self.t_sort += o.t_sort;
        self.t_admit += o.t_admit;
        self.t_gather += o.t_gather;
    }
}

struct Wave {
    queues: Vec<Queue>,
    index: rustc_hash::FxHashMap<(u64, u32), u32>,
    free: Vec<u32>,
    last: ((u64, u32), u32),
    clock: usize,
    cap: usize,
    payload: usize,
    pieces: rustc_hash::FxHashMap<u64, Vec<u8>>,
    row: Vec<u8>,
    sort_buf: Vec<((u64, u64), u32)>,
    new: Vec<u32>,
    stats: WaveStats,
    t0: std::time::Instant,
}

impl Wave {
    fn new(pool: usize, cap: usize, payload: usize) -> Self {
        // Allocated once per level in the real thing and reused every
        // frame; pre-touch so the bench does not time the page faults.
        let queues = (0..pool)
            .map(|_| {
                let mut keys = vec![(0u64, 0u64); cap];
                keys.clear();
                let mut payload_v = vec![0u8; cap * payload];
                payload_v.clear();
                Queue { shape: 0, cell: 0, keys, payload: payload_v, touched: false, live: false }
            })
            .collect();
        Wave {
            queues,
            index: Default::default(),
            free: (0..pool as u32).rev().collect(),
            last: ((u64::MAX, u32::MAX), u32::MAX),
            clock: 0,
            cap,
            payload,
            pieces: Default::default(),
            row: vec![0u8; payload],
            sort_buf: Vec::with_capacity(cap),
            new: Vec::with_capacity(cap),
            stats: WaveStats::default(),
            t0: std::time::Instant::now(), // after the pool's allocation
        }
    }

    fn queue_for(&mut self, shape: u64, cell: u32, door: &dyn celeste_rust::search::door::Admit) -> u32 {
        if self.last.0 == (shape, cell) {
            self.stats.cache_hits += 1;
            return self.last.1;
        }
        let q = if let Some(&q) = self.index.get(&(shape, cell)) {
            q
        } else {
            let q = match self.free.pop() {
                Some(q) => q,
                None => {
                    // Second chance: skip queues touched since the hand last passed.
                    let n = self.queues.len();
                    loop {
                        let i = self.clock % n;
                        self.clock += 1;
                        if self.queues[i].touched {
                            self.queues[i].touched = false;
                        } else {
                            self.stats.evicted += 1;
                            self.flush(i as u32, door);
                            // The flush returned it to the free list; take it back.
                            break self.free.pop().expect("the flushed queue is free");
                        }
                    }
                }
            };
            let qq = &mut self.queues[q as usize];
            qq.shape = shape;
            qq.cell = cell;
            qq.live = true;
            self.index.insert((shape, cell), q);
            q
        };
        self.last = ((shape, cell), q);
        q
    }

    fn push(&mut self, shape: u64, cell: u32, key: (u64, u64), door: &dyn celeste_rust::search::door::Admit) {
        let q = self.queue_for(shape, cell, door);
        // The payload: what the kernel's typed columns would hold.
        if self.row.len() >= 8 {
            self.row[0..8].copy_from_slice(&key.0.to_le_bytes());
        }
        let qq = &mut self.queues[q as usize];
        qq.keys.push(key);
        qq.payload.extend_from_slice(&self.row);
        qq.touched = true;
        if qq.keys.len() == self.cap {
            self.stats.full += 1;
            self.flush(q, door);
        }
    }

    fn flush(&mut self, q: u32, door: &dyn celeste_rust::search::door::Admit) {
        let payload = self.payload;
        let qq = &mut self.queues[q as usize];
        if !qq.live {
            return;
        }
        self.stats.flushes += 1;
        self.stats.flushed_rows += qq.keys.len() as u64;
        // Sort (key, row) and collapse duplicates within the queue.
        let t = std::time::Instant::now();
        self.sort_buf.clear();
        self.sort_buf.extend(qq.keys.iter().enumerate().map(|(i, k)| (*k, i as u32)));
        self.sort_buf.sort_unstable();
        self.sort_buf.dedup_by_key(|e| e.0);
        // Reuse the queue's key vector as the sorted key batch.
        qq.keys.clear();
        qq.keys.extend(self.sort_buf.iter().map(|e| e.0));
        self.new.clear();
        let t1 = std::time::Instant::now();
        self.stats.t_sort += t1 - t;
        door.admit(qq.shape, qq.cell, &qq.keys, &mut self.new);
        let t2 = std::time::Instant::now();
        self.stats.t_admit += t2 - t1;
        let piece = self.pieces.entry(qq.shape).or_default();
        for &i in &self.new {
            let r = self.sort_buf[i as usize].1 as usize;
            piece.extend_from_slice(&qq.payload[r * payload..(r + 1) * payload]);
        }
        self.stats.t_gather += t2.elapsed();
        self.stats.admitted += self.new.len();
        qq.keys.clear();
        qq.payload.clear();
        qq.live = false;
        qq.touched = false;
        self.index.remove(&(qq.shape, qq.cell));
        if self.last.1 == q {
            self.last = ((u64::MAX, u32::MAX), u32::MAX);
        }
        self.free.push(q);
    }

    fn finish(mut self, door: &dyn celeste_rust::search::door::Admit) -> WaveStats {
        for q in 0..self.queues.len() as u32 {
            if self.queues[q as usize].live {
                self.stats.final_ += 1;
                self.flush(q, door);
            }
        }
        self.stats.piece_bytes = self.pieces.values().map(Vec::len).sum();
        self.stats.busy = self.t0.elapsed();
        self.stats
    }
}
