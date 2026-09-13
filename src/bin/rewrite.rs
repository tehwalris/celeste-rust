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
