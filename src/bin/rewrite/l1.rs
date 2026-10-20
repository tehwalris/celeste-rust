//! THE LEVEL -1 TABLE (`search::level_minus_one`, plans/level-minus-one.md):
//! the table alone, its probe against a tree, a known solution against it.
//! Every one is the search's table (`level_minus_one::table`).

use anyhow::{anyhow, Context, Result};
use celeste_rust::search::level_minus_one;
use celeste_rust::settings::{Arith, Settings};

#[derive(clap::Subcommand)]
pub enum Command {
    /// The table as the search builds it (`--level-minus-one H,S`): its
    /// entries, the start state's d and its fingerprint (the A/B check of a
    /// change to the build).
    Table(TableArgs),
    /// The table against a tree's recorded transitions, frames FROM..=TO
    /// under CEILING (and the backward's MARKS at that horizon).
    Probe(ProbeArgs),
    /// The table against a known solution: none of its states may be too late.
    ///
    /// Its concrete states (reference engine) at every frame f before the
    /// exit, each with the table's d. One too late for `--horizon` (f + d >
    /// H, H the solution's exit frame) is a SOUNDNESS violation of the
    /// table: exits non-zero. An `rnd` fork checks every leaf.
    Check(CheckArgs),
}

#[derive(clap::Args)]
pub struct TableArgs {
    /// The table's speed bound S (px per frame).
    #[arg(long, default_value_t = 5)]
    spd: i32,
}

#[derive(clap::Args)]
pub struct ProbeArgs {
    /// The table's speed bound S (px per frame).
    #[arg(long, default_value_t = 5)]
    spd: i32,
    /// The tree (a level directory) whose recorded transitions are checked.
    #[arg(long)]
    level_dir: std::path::PathBuf,
    /// The horizon the tree's states are judged against.
    #[arg(long)]
    ceiling: u32,
    /// The first frame checked.
    #[arg(long)]
    from: u32,
    /// The last frame checked.
    #[arg(long)]
    to: u32,
    /// The backward's marks at that horizon (`search --save-marks`).
    #[arg(long)]
    marks: Option<std::path::PathBuf>,
}

#[derive(clap::Args)]
pub struct CheckArgs {
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
}

/// `rewrite l1` under the run's settings `cfg`, in its arithmetic.
pub fn run(cfg: &Settings, c: Command) -> Result<()> {
    match c {
        Command::Table(a) => {
            let t = std::time::Instant::now();
            let table = level_minus_one::table(a.spd, cfg)?;
            println!("level -1 table: {} entries, start d {}, fingerprint {:016x}, {:.1} s", table.len(), table.start_d, table.fingerprint(), t.elapsed().as_secs_f64());
            Ok(())
        }
        Command::Probe(a) => {
            let opts = level_minus_one::Opts { spd_px: a.spd, level_dir: a.level_dir, ceiling: a.ceiling, from: a.from, to: a.to, marks: a.marks, threads: cfg.threads() };
            let cfg = cfg.clone();
            // The tracer recurses through the cart's AST: a big stack.
            let report = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || level_minus_one::probe(&opts, &cfg))
                .context("spawn the level -1 probe")?
                .join()
                .map_err(|_| anyhow!("the level -1 probe panicked"))??;
            print!("{report}");
            Ok(())
        }
        Command::Check(a) => {
            let bytes = celeste_rust::concrete::read_inputs(&a.inputs)?;
            let rep = match cfg.arith {
                Arith::Fixed => level_minus_one::check_route::<celeste_engine::runtime2::P8>(&bytes, a.horizon, a.spd, cfg)?,
                Arith::Float(_) => level_minus_one::check_route::<celeste_core::lua_f64::LuaF64>(&bytes, a.horizon, a.spd, cfg)?,
            };
            print!("{rep}");
            Ok(())
        }
    }
}
