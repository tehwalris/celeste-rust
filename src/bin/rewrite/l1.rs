//! THE LEVEL -1 TABLE against a known solution (`rewrite l1-check`).

use anyhow::Result;
use celeste_core::arith::{Arithmetic, Fixed};
use celeste_engine::runtime2::Rt2;
use celeste_rust::frame::wins_of;
use celeste_rust::settings::{Arith, Settings};

/// `l1-check`'s arguments.
#[derive(clap::Args)]
pub struct Args {
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

/// `l1-check` under the run's settings `cfg`, in its arithmetic.
pub fn run(cfg: &Settings, a: Args) -> Result<()> {
    match cfg.arith {
        Arith::Fixed => l1_check::<Fixed>(&a.inputs, a.horizon, a.spd, cfg),
        Arith::Float(_) => l1_check::<celeste_core::arith::Float>(&a.inputs, a.horizon, a.spd, cfg),
    }
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
