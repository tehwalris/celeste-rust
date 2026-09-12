//! The search driver: `rewrite search` (the precision ladder), `rewrite
//! forward` (one forward pass with timing), `rewrite ckhash` (checkpoint
//! fingerprints).

use anyhow::Result;
use clap::{Parser, Subcommand};

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
            let mut engine = celeste_rust::compiled::FrameEngine::new_for_start_room()?;
            eprintln!("[fwd] engine up in {:.2} s ({precision:?})", t.elapsed().as_secs_f64());
            let initial = vec![Block::from_state(
                &celeste_rust::trace::refengine::RefEngine::new()?.initial_state()?,
            )?];
            let dir = std::path::Path::new(&checkpoint_dir);
            let t = std::time::Instant::now();
            let fwd = forward_run(&mut engine, initial, dir, to, true, None)?;
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
                    let marked: Vec<bool> = block
                        .keys()
                        .iter()
                        .zip(&cells)
                        .map(|(k, &c)| coarse_marks.contains(*k, c))
                        .collect();
                    if !marked.iter().any(|&m| m) {
                        continue;
                    }
                    let allow = filter.allowed(&block)?;
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
                        reachable.insert(*k, c);
                    }
                }
                for block in load_frame(&fine_dir, f)? {
                    let cells = block.positions()?;
                    let mask: Vec<bool> = block
                        .keys()
                        .iter()
                        .zip(&cells)
                        .map(|(k, &c)| fine_marks.contains(*k, c))
                        .collect();
                    let Some(win_rows) = block.keep(&mask) else { continue };
                    let (wk, wc) = widened_keys(&win_rows, coarser)?;
                    let fine_cells = win_rows.positions()?;
                    for i in 0..win_rows.lanes() {
                        n += 1;
                        let m = coarse_marks.contains(wk[i], wc[i]);
                        let r = reachable.contains(wk[i], wc[i]);
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
    }

    Ok(())
}
