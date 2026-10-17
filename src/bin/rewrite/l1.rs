//! THE LEVEL -1 TABLE (`search::level_minus_one`, plans/level-minus-one.md):
//! the table alone, its probe against a tree, a known solution against it.
//! Every one is the search's table (`level_minus_one::table`).

use anyhow::{anyhow, Context, Result};
use celeste_core::arith::{Arithmetic, Fixed};
use celeste_engine::runtime2::Rt2;
use celeste_rust::frame::wins_of;
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
                .spawn(move || level_minus_one::probe(std::path::Path::new("."), &opts, &cfg))
                .context("spawn the level -1 probe")?
                .join()
                .map_err(|_| anyhow!("the level -1 probe panicked"))??;
            print!("{report}");
            Ok(())
        }
        Command::Check(a) => match cfg.arith {
            Arith::Fixed => l1_check::<Fixed>(&a.inputs, a.horizon, a.spd, cfg),
            Arith::Float(_) => l1_check::<celeste_core::arith::Float>(&a.inputs, a.horizon, a.spd, cfg),
        },
    }
}

/// `l1 check`: a known solution against the level -1 table, in the
/// arithmetic `A` (its table, its reference engine).
fn l1_check<A: Arithmetic>(inputs: &str, horizon: u32, spd: i32, cfg: &celeste_rust::settings::Settings) -> Result<()>
where
    A::Num: celeste_core::arith::EngineNum<A = A>,
{
    let bytes = celeste_rust::concrete::read_inputs(inputs)?;
    let table = level_minus_one::table(spd, cfg)?;
    println!("[l1 check] table: {} entries, start d {}, fingerprint {:016x}", table.len(), table.start_d, table.fingerprint());
    // The known run: under the concrete run's seeds (a float tool's).
    let mut eng = celeste_rust::engines::reference::ReferenceEngine::<A>::new(&cfg.concrete().0)?;
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
        println!("[l1 check] f{f}: {}", line.join(", "));
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
            println!("[l1 check] exit during frame {} ({} of {} leaves)", f + 1, wins.len(), wins.len() + next.len());
            exited = Some(wins);
            break;
        }
        states = next;
    }
    let wins = exited.ok_or_else(|| anyhow::anyhow!("the inputs never exit the room"))?;
    let late: usize = wins.iter().map(|l| l.len()).sum();
    println!("[l1 check] {checked} states checked, {unknown} not table nodes; on the exiting lineages {late} TOO LATE for horizon {horizon} (frames {:?})", wins.iter().flatten().collect::<std::collections::BTreeSet<_>>());
    anyhow::ensure!(late == 0, "the level -1 table calls {late} states of a known solution too late: UNSOUND");
    Ok(())
}
