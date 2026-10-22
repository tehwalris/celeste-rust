//! `rewrite validate ...` and `rewrite golden ...`: tasdatabase files in
//! the community's TAS tools (`tools::love`), in every tool and start mode
//! (`tools::uploads`), and the float oracle's goldens (`tools::golden`).

use anyhow::{ensure, Result};
use celeste_rust::tools::tasdb::Cat;
use celeste_rust::tools::love::{self, Tool};
use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    /// UNIVERSALCLASSICTAS, headless: each LEVEL FILE pair loaded (W),
    /// restarted (D) and clean-saved (U): `level L FILE: finished, clean
    /// save, N inputs = N-1f -> SAVED` or `DID NOT FINISH (why), ...`.
    Uct(LoveArgs),
    /// CELIA (cctas, the tool that models the loading jank), headless: each
    /// LEVEL FILE pair reached as a user does (F), loaded (Shift+W) and
    /// clean-saved (U): `level L FILE: finished, Nf -> SAVED` or `DID NOT
    /// FINISH (why), 0f`.
    Celia(LoveArgs),
    /// TASDATABASE FILES in every tool and start mode (`tools::uploads`):
    /// one tab-separated row per file, VALID where the boot chain (real
    /// play) exits by the file's count (100%: with the berry) and Celia
    /// agrees. The level is the file name's (`TAS<level>.tas`), the category
    /// its directory's (or `--cat`).
    Uploads(UploadsArgs),
}

#[derive(clap::Args)]
pub struct LoveArgs {
    #[command(flatten)]
    opts: love::Opts,
    /// LEVEL FILE [LEVEL FILE ...]
    #[arg(required = true)]
    pairs: Vec<String>,
}

#[derive(clap::Args)]
pub struct UploadsArgs {
    #[arg(required = true)]
    files: Vec<PathBuf>,
    /// Where UCT did not finish, the balloon seed nudges (`love::nudged_seeds`),
    /// each checked in PICO-8 (the IL exit must not move) and UCT.
    #[arg(long)]
    nudge: bool,
    /// The category (default: the file's directory's name).
    #[arg(long)]
    cat: Option<Cat>,
    /// Files checked at a time.
    #[arg(long, default_value_t = 4)]
    jobs: usize,
    /// The PICO-8 binary (default ~/pico-8/pico8).
    #[arg(long)]
    pico8: Option<PathBuf>,
}

pub fn run(c: Command) -> Result<()> {
    let (tool, a) = match c {
        Command::Uct(a) => (Tool::Uct, a),
        Command::Celia(a) => (Tool::Celia, a),
        Command::Uploads(a) => return celeste_rust::tools::uploads::check_all(&a.files, a.nudge, a.cat, a.jobs, a.pico8.as_deref()),
    };
    ensure!(a.pairs.len() % 2 == 0, "LEVEL FILE pairs, not {:?}", a.pairs);
    let pairs = a.pairs.chunks(2).map(|p| Ok((p[0].parse::<i16>().map_err(|_| anyhow::anyhow!("level {} is not a number", p[0]))?, PathBuf::from(&p[1])))).collect::<Result<Vec<_>>>()?;
    for o in love::run(tool, &a.opts, &pairs)? {
        for l in o.lines {
            println!("{l}");
        }
    }
    Ok(())
}

#[derive(clap::Args)]
pub struct GoldenArgs {
    /// The tool whose traces become goldens.
    #[arg(value_enum)]
    tool: Tool,
    /// The jobs: `CAT LEVEL...`, `CAT all`, `random N LEVEL...|all`,
    /// `witness all` (gates/witnesses/witnesses.txt), `local all` (our
    /// unsubmitted runs, /var/tmp/float-goldens).
    #[arg(required = true)]
    jobs: Vec<String>,
}

pub fn golden(a: GoldenArgs) -> Result<()> {
    celeste_rust::tools::golden::run(a.tool, &a.jobs)
}

#[derive(clap::Args)]
pub struct CategoryArgs {
    /// The jobs file (JSON; re-read before every job: `tools::category`).
    jobs: PathBuf,
    /// The results (`results.jsonl`), the jobs' directories and uploads.
    outdir: PathBuf,
    /// The rewrite binary the searches run (default OUTDIR/rewrite: a copy,
    /// so a rebuild cannot swap it mid-queue).
    #[arg(long)]
    bin: Option<PathBuf>,
}

pub fn category(a: CategoryArgs) -> Result<()> {
    let binary = a.bin.unwrap_or_else(|| a.outdir.join("rewrite"));
    celeste_rust::tools::category::queue(&a.jobs, &a.outdir, &binary)
}
