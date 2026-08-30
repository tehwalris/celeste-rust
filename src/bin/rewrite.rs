//! Driver for the program rewriting system. See plans/rewrite-plan.md.
//!
//!     rewrite search --checkpoint-dir DIR   # the rebuilt precision-ladder search

use anyhow::Result;
use clap::{Parser, Subcommand};

const DEFAULT_RECIPE: &str = "rewrites.jsonl";

#[derive(Parser)]
#[command(about = "Apply and verify program rewrites")]
struct Cli {
    #[arg(long, default_value = DEFAULT_RECIPE)]
    recipe: String,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// The rebuilt search (src/frame.rs): find the minimal winning frame by
    /// running the full precision ladder (Bits 0..=15 then Exact) at rising
    /// horizons until one is confirmed at every level through concrete.
    Search {
        /// First horizon to test (bump upward until Confirmed).
        #[arg(long, default_value_t = 94)]
        from: u32,
        /// Last horizon to try.
        #[arg(long)]
        to: Option<u32>,
        /// Deepest bits rung (>=16 means Exact is the top rung).
        #[arg(long, default_value_t = 15)]
        maxk: u8,
        /// Base checkpoint dir (per-horizon, per-level subdirs land under it).
        #[arg(long)]
        checkpoint_dir: String,
        /// Start room "x,y".
        #[arg(long, default_value = "1,0")]
        room: String,
        /// Optional synthetic win "x,y" (CELESTE_WIN_AT_XY) for a cheap run.
        #[arg(long)]
        win_at: Option<String>,
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
            let program = celeste_rust::program::frozen::rewritten(&cli.recipe)?;
            let precisions: Vec<RemPrecision> = (0u8..=maxk.min(15))
                .map(RemPrecision::Bits)
                .chain(std::iter::once(RemPrecision::Exact))
                .collect();
            let make_engine = |p: RemPrecision| {
                set_rem_precision(p);
                Ok(Box::new(celeste_rust::compiled::FrameEngine::new_for_start_room(&program)?)
                    as Box<dyn FrameStep>)
            };
            let make_initial = || {
                Ok(vec![Block::new(
                    celeste_rust::trace::refengine::RefEngine::new()?.initial_state()?,
                )])
            };
            let dir = std::path::Path::new(&checkpoint_dir);
            match find_optimum(make_engine, make_initial, dir, from, to.unwrap_or(from), &precisions)? {
                Some(h) => println!("OPTIMAL win frame: {h}"),
                None => println!("no win confirmed up to horizon {}", to.unwrap_or(from)),
            }
        }
    }

    Ok(())
}
