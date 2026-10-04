//! CLI for the tracer's level -1 probe (`celeste_rust::trace::level_minus_one`):
//!
//!   transpile --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS]
//!
//! the position-only cost-to-go table for the start room
//! (plans/level-minus-one.md), with the player's speed in [-S, S] px/frame,
//! checked against LEVEL_DIR's recorded transitions and compared with its
//! frames FROM..=TO under CEILING (and against the backward's MARKS file at
//! that horizon). CELESTE_THREADS workers (default 8).

use anyhow::{anyhow, Context, Result};

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        Some("--level-minus-one") => {
            let mut next = |what: &str| args.next().ok_or_else(|| anyhow!("--level-minus-one S LEVEL_DIR CEILING FROM TO: missing {what}"));
            let spd_px: i32 = next("S")?.parse().context("--level-minus-one S")?;
            let level_dir = std::path::PathBuf::from(next("LEVEL_DIR")?);
            let ceiling: u32 = next("CEILING")?.parse().context("--level-minus-one CEILING")?;
            let from: u32 = next("FROM")?.parse().context("--level-minus-one FROM")?;
            let to: u32 = next("TO")?.parse().context("--level-minus-one TO")?;
            let marks = args.next().map(std::path::PathBuf::from);
            let threads = std::env::var("CELESTE_THREADS").ok().map(|s| s.parse().context("CELESTE_THREADS")).transpose()?.unwrap_or(8);
            let opts = celeste_rust::trace::level_minus_one::Opts { spd_px, level_dir, ceiling, from, to, marks, threads };
            // The tracer recurses through the cart's AST: a big stack.
            let report = std::thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || celeste_rust::trace::level_minus_one::probe(std::path::Path::new("."), &opts))
                .context("spawn the level -1 probe")?
                .join()
                .map_err(|_| anyhow!("the level -1 probe panicked"))??;
            print!("{}", report);
            Ok(())
        }
        other => Err(anyhow!("usage: transpile --level-minus-one S LEVEL_DIR CEILING FROM TO [MARKS] (got {other:?})")),
    }
}
