//! CLI for the level -1 cost-to-go table (`trace::level_minus_one`,
//! plans/level-minus-one.md), player speed in [-S, S] px/frame:
//!
//!   transpile --room X,Y --probe S LEVEL_DIR CEILING FROM TO [MARKS]
//!       check it against LEVEL_DIR's recorded transitions, frames FROM..=TO
//!       under CEILING (and the backward's MARKS at that horizon)
//!   transpile --room X,Y --level-minus-one-table S   build the table alone, its fingerprint
//!
//! The settings (`--room`, the category, `--threads`, ...) are the shared
//! flags (`settings::SettingsArgs`).

use anyhow::{anyhow, Context, Result};
use clap::Parser;

#[derive(Parser)]
#[command(
    about = "The level -1 table: its probe against a tree, or the table alone",
    long_about = "The level -1 cost-to-go table (plans/level-minus-one.md): per (shape, cell) a sound lower bound on the frames to the room's exit, the player's speed in [-S, S] px/frame. The search builds it itself (--level-minus-one H,S); this binary builds it alone, or checks it against a tree's recorded transitions. The settings (--room, the category, --threads, ...) are the shared flags; the table is cached in /var/tmp/celeste-l1-cache.",
    after_long_help = "Examples:\n  ./safe-run.sh -- target/quick/transpile --room 2,0 --level-minus-one-table 5\n  ./safe-run.sh -- target/quick/transpile --room 2,0 --probe 5 LEVEL_DIR CEILING FROM TO [MARKS]"
)]
struct Cli {
    /// S LEVEL_DIR CEILING FROM TO [MARKS]: the table at speed bound S
    /// against LEVEL_DIR's recorded transitions, frames FROM..=TO under
    /// CEILING (and the backward's MARKS at that horizon). (Not
    /// `--level-minus-one`: that is the search's filter, a shared setting.)
    #[arg(long, num_args = 5..=6, value_names = ["S", "LEVEL_DIR", "CEILING", "FROM", "TO", "MARKS"])]
    probe: Option<Vec<String>>,
    /// S: build the table as the search does, print its fingerprint (the A/B
    /// check for a change to the build).
    #[arg(long, value_name = "S")]
    level_minus_one_table: Option<i32>,
    #[command(flatten)]
    settings: celeste_rust::settings::SettingsArgs,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    // Both commands are the level -1 table's, built in the run's arithmetic.
    let cfg = celeste_rust::settings::startup(&cli.settings, true, |_| {})?;
    let threads_or = |n: usize| cfg.threads.unwrap_or(n);
    match (cli.probe, cli.level_minus_one_table) {
        (Some(args), None) => {
            let mut args = args.into_iter();
            let mut next = |what: &str| args.next().ok_or_else(|| anyhow!("--probe S LEVEL_DIR CEILING FROM TO: missing {what}"));
            let spd_px: i32 = next("S")?.parse().context("--probe S")?;
            let level_dir = std::path::PathBuf::from(next("LEVEL_DIR")?);
            let ceiling: u32 = next("CEILING")?.parse().context("--probe CEILING")?;
            let from: u32 = next("FROM")?.parse().context("--probe FROM")?;
            let to: u32 = next("TO")?.parse().context("--probe TO")?;
            let marks = args.next().map(std::path::PathBuf::from);
            let opts = celeste_rust::trace::level_minus_one::Opts { spd_px, level_dir, ceiling, from, to, marks, threads: threads_or(8) };
            // The tracer recurses through the cart's AST: a big stack.
            let report = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || celeste_rust::trace::level_minus_one::probe(std::path::Path::new("."), &opts, &cfg))
                .context("spawn the level -1 probe")?
                .join()
                .map_err(|_| anyhow!("the level -1 probe panicked"))??;
            print!("{}", report);
            Ok(())
        }
        (None, Some(spd_px)) => {
            let threads = threads_or(std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4));
            let t = std::time::Instant::now();
            let table = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || celeste_rust::trace::level_minus_one::cost_to_go(std::path::Path::new("."), spd_px, threads, &cfg))
                .context("spawn the level -1 builder")?
                .join()
                .map_err(|_| anyhow!("the level -1 builder panicked"))??;
            println!("level -1 table: {} entries, start d {}, fingerprint {:016x}, {:.1} s", table.len(), table.start_d, table.fingerprint(), t.elapsed().as_secs_f64());
            Ok(())
        }
        _ => Err(anyhow!("usage: transpile --probe S LEVEL_DIR CEILING FROM TO [MARKS] | --level-minus-one-table S")),
    }
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
