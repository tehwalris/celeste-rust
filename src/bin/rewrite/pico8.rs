//! `rewrite pico8 ...`: a REAL PICO-8, headless (`tools::pico8`): one room
//! replayed, a room in real play through the chain of rooms before it
//! (`tools::chain`), the start real play enters a room with, and the
//! tracer's probe goldens.

use anyhow::Result;
use celeste_rust::settings::Settings;
use celeste_rust::tools::chain;
use celeste_rust::tools::pico8::{original_cart, Cart, Fenced, LoadingMove, Play, Runner, SeedOrder, Spec};
use celeste_rust::tools::tasdb::{self, Cat};
use std::path::PathBuf;

#[derive(clap::Subcommand)]
pub enum Command {
    /// ONE ROOM from its load on a real PICO-8: the frame lines.
    ///
    /// `fF room X,Y freeze N` and, as the frame has them, ` player X,Y spd
    /// SX,SY rem RX,RY` (`tostr(v, true)`), ` player_spawn X,Y`, ` fly_fruit
    /// X,Y`, ` fruit X,Y`, ` lifeup X,Y` and ` dash DIR` (the dash the cart's
    /// own dash branch started this frame: R L U D UR UL DR DL). Frame f plays
    /// input f-1, as `rewrite run` counts. The settings give the room, the
    /// category (gemskip: one dash), the loading frame (`--loading-jank`:
    /// the ORIGINAL cart's index) and the balloons' and chests' seeds
    /// (`--balloon-seeds`; `none`: the balloons' own `rnd(1)`).
    Replay(ReplayArgs),
    /// A room's TAS in REAL PLAY: the original cart from an earlier room's
    /// load through the real room transitions, the database's files for
    /// the rooms before it (`tasdb::chain_file`), then FILE.
    ///
    /// Prints each segment's exit (frame, the room's count, the leaving
    /// player's index J, the berry) and the last segment's result; with
    /// `--modes` the IL load's and the jank load's (from the J the chain
    /// measured) too, their room-entry states diffed against the chain's
    /// field by field.
    Chain(ChainArgs),
    /// The J real play enters `--room` with: the leaving player's index in
    /// the category's boot chain (`none`: 100m, or a chain that breaks just
    /// before the room).
    EntryJank(P8Args),
    /// THE SEARCH'S START against real play: this binary's `run
    /// --dump-start` under the boot chain's J against the chain's room
    /// entry on PICO-8, field by field (exit 1 on a difference).
    CheckStart(P8Args),
    /// `--room`'s entry state after every category's boot chain, and the
    /// fields that differ between them.
    Vary(P8Args),
    /// The tracer's probe corpora (`lua/probe/*.lua`) on PICO-8: their
    /// goldens rewritten where PICO-8 now says something else. READ THE DIFF.
    ProbeGoldens(P8Args),
    /// The community TAS (`--name`, behind `--prologue` frames) and ours
    /// (`--inputs`) frame by frame in the ORIGINAL cart, both started as the
    /// search starts the room: a table; with `--out DIR` both paths for
    /// `export-ui --witness DIR/ours.txt --reference DIR/reference.txt`.
    Align(AlignArgs),
}

#[derive(clap::Args)]
pub struct AlignArgs {
    /// The database's name of the room's entry (`2900m`).
    #[arg(long)]
    name: String,
    /// The frames before the community file's first input.
    #[arg(long)]
    prologue: i64,
    /// Our witness (a `tas/` file or a comma list, from the room's load).
    #[arg(long)]
    inputs: String,
    /// Where the two paths go.
    #[arg(long)]
    out: Option<PathBuf>,
    #[command(flatten)]
    p8: P8Args,
}

#[derive(clap::Args)]
pub struct P8Args {
    /// The PICO-8 binary (default ~/pico-8/pico8).
    #[arg(long)]
    pico8: Option<PathBuf>,
    /// Keep the carts in this directory (`replay-NNN.p8` in run order).
    #[arg(long)]
    keep: Option<PathBuf>,
}

impl P8Args {
    fn runner(&self) -> Result<Runner> {
        Runner::new(self.pico8.as_deref(), self.keep.as_deref())
    }
}

#[derive(clap::Args)]
pub struct ReplayArgs {
    /// The inputs from the room's load: a `tas/` file or a comma list.
    #[arg(long)]
    inputs: String,
    /// Frames to run (default: the inputs'; past them, no input).
    #[arg(long)]
    frames: Option<usize>,
    /// The game as released (`--original-lua`), not the traced program.
    #[arg(long)]
    original: bool,
    /// The original cart (default celeste_ocaml's `celeste.lua`).
    #[arg(long, requires = "original")]
    original_lua: Option<PathBuf>,
    /// Before frame 1, the start state: the globals, every object's fields
    /// (`dump entry` lines).
    #[arg(long)]
    dump: bool,
    /// Every object's position, remainder and phase on the frame lines.
    #[arg(long)]
    trace_objects: bool,
    /// FENCED (plans/correctness.md C4): the loading frame's move.
    #[arg(long, value_enum, default_value = "unguarded")]
    loading_move: LoadingMove,
    /// FENCED (plans/correctness.md C11): which object a seed is.
    #[arg(long, value_enum, default_value = "pop")]
    seed_order: SeedOrder,
    #[command(flatten)]
    p8: P8Args,
}

#[derive(clap::Args)]
pub struct ChainArgs {
    /// The room's TAS (the tasdatabase's format: `[seeds]` and inputs from
    /// the player's first frame).
    file: PathBuf,
    /// The chain's first level (default: the previous one).
    #[arg(long, conflicts_with = "boot")]
    from: Option<i16>,
    /// The chain from 100m (the game's start).
    #[arg(long)]
    boot: bool,
    /// Also the IL and jank modes, their entry states diffed against the chain's.
    #[arg(long)]
    modes: bool,
    /// The last room's frame lines, every object on them.
    #[arg(long)]
    trace_objects: bool,
    #[command(flatten)]
    p8: P8Args,
}

pub fn run(cfg: &Settings, c: Command) -> Result<()> {
    let level = tasdb::level_of(cfg.room);
    // The database's category (a full run's files) of the search's.
    let cat = || Cat::try_from(cfg.category);
    match c {
        Command::Replay(a) => {
            let inputs = celeste_rust::concrete::read_inputs(&a.inputs)?;
            let seeds = cfg.balloon_seeds.as_deref().map(|s| s.split(',').filter(|t| !t.is_empty()).map(celeste_rust::tools::pico8::py_float).collect::<Result<Vec<_>>>()).transpose()?;
            let cart = if a.original { Cart::Original(a.original_lua.unwrap_or_else(original_cart)) } else { Cart::Minimal };
            let frames = a.frames.unwrap_or(inputs.len());
            let play = Play::Room { room: cfg.room, inputs, seeds };
            let fenced = Fenced { loading_move: a.loading_move, seed_order: a.seed_order };
            let spec = Spec { cart, play, frames, one_dash: cfg.category.gemskip(), jank: cfg.loading_jank, load_draw: false, dump: a.dump, trace_objects: a.trace_objects, fenced };
            for l in a.p8.runner()?.run(&spec)? {
                println!("{l}");
            }
            Ok(())
        }
        Command::Chain(a) => chain_cmd(cat()?, level, a),
        Command::EntryJank(a) => {
            println!("{}", py_opt(chain::entry_jank(&a.runner()?, cat()?, level)?));
            Ok(())
        }
        Command::CheckStart(a) => {
            let r = a.runner()?;
            let j = chain::entry_jank(&r, cat()?, level)?;
            let (_, _, reference) = chain::parse(&chain::chain(&r, cat()?, level, &[], &[0], 1, false, true)?);
            let ours = dump_start(cfg.room, cfg.category, j)?;
            let (diffs, only) = chain::check_start(&reference, &ours)?;
            let verdict = if diffs.is_empty() { "start state MATCHES the boot chain's entry".to_string() } else { format!("{} DIFFERENCES", diffs.len()) };
            println!("{} level {level}: J {}: {verdict}", cfg.category, py_opt(j));
            for d in &diffs {
                println!("    {d}");
            }
            if !only.is_empty() {
                println!("    (not compared, one cart only: {})", only.join(", "));
            }
            anyhow::ensure!(diffs.is_empty(), "the search's start differs from the boot chain's entry");
            Ok(())
        }
        Command::Vary(a) => {
            let cats: Vec<Cat> = Cat::all().filter(|c| c.name() != "gemskip100").collect();
            let (runs, differ) = chain::vary(&a.runner()?, level, &cats)?;
            for (c, st, _) in &runs {
                println!("{c}: {st}");
            }
            let first = runs.iter().find(|r| r.2.is_some()).map(|r| r.0.to_string());
            for (field, cs) in &differ {
                let mut w = field.split_whitespace();
                let kind = if w.next() == Some("globals") && w.next().is_some_and(|g| chain::COSMETIC.contains(&g)) { "cosmetic" } else { "STATE" };
                println!("varies ({kind}): {field}  [{} differ from {}]", cs.iter().cloned().collect::<Vec<_>>().join(", "), first.as_deref().unwrap_or(""));
            }
            Ok(())
        }
        Command::Align(a) => {
            let seeds: Vec<String> = cfg.balloon_seeds.as_deref().unwrap_or("").split(',').filter(|s| !s.is_empty()).map(str::to_string).collect();
            let table = celeste_rust::tools::category::align(&a.p8.runner()?, cfg.room, cfg.category, &a.name, a.prologue, &celeste_rust::concrete::read_inputs(&a.inputs)?, a.out.as_deref(), &seeds, cfg.loading_jank)?;
            print!("{table}");
            Ok(())
        }
        Command::ProbeGoldens(a) => {
            let pico8 = a.pico8.unwrap_or_else(celeste_rust::tools::pico8::default_pico8);
            let changed = celeste_rust::tools::pico8::probe_goldens(&pico8, std::path::Path::new(env!("CARGO_MANIFEST_DIR")))?;
            if !changed.is_empty() {
                println!("CHANGED (real PICO-8 now says something else; read the diff): {}", changed.join(", "));
            }
            Ok(())
        }
    }
}

/// `rewrite pico8 chain`.
fn chain_cmd(cat: Cat, level: i16, a: ChainArgs) -> Result<()> {
    let r = a.p8.runner()?;
    let (seeds, inputs) = tasdb::read(&a.file)?;
    // 100m has no previous level: its chain is the boot.
    let first = if a.boot { 1 } else { a.from.unwrap_or((level - 1).max(1)) };
    let lines = chain::chain(&r, cat, level, &seeds, &inputs, first, a.trace_objects, true)?;
    let (exits, _, reference) = chain::parse(&lines);
    for l in &lines {
        if ["exit ", "death ", "unexpected "].iter().any(|p| l.starts_with(p)) || (a.trace_objects && l.starts_with('f')) {
            println!("{l}");
        }
    }
    let n = (level - first + 1) as usize;
    let (s, _) = chain::summary(&format!("chain from level {first}"), &lines, n);
    // The jank the last room was entered with: the leaving player's index.
    let j = if n >= 2 { exits.get(n - 2).and_then(|e| e.get("player_idx")).cloned() } else { None };
    println!("{s}{}", j.as_ref().map_or(String::new(), |j| format!(" (entered with J {j})")));
    if a.modes {
        let il = chain::single(&r, cat, level, &seeds, &inputs, None, false)?;
        println!("{}", chain::summary("IL", &il, 1).0);
        for d in chain::diff_dumps(&chain::parse(&il).2, &reference)? {
            println!("   IL entry differs: {d}");
        }
        if let Some(j) = j {
            let jk = chain::single(&r, cat, level, &seeds, &inputs, Some(j.parse()?), false)?;
            println!("{}", chain::summary(&format!("jank J={j}"), &jk, 1).0);
            for d in chain::diff_dumps(&chain::parse(&jk).2, &reference)? {
                println!("   jank entry differs: {d}");
            }
        }
    }
    Ok(())
}

/// A J as this tool has always printed one (`None`: none).
fn py_opt(j: Option<usize>) -> String {
    j.map_or("None".to_string(), |j| j.to_string())
}

/// The search's start state as `rewrite run --dump-start` prints it under
/// the loading frame `j`, every seed 0.
fn dump_start(room: (i16, i16), cat: celeste_rust::settings::Category, j: Option<usize>) -> Result<Vec<String>> {
    let args = ["run", "-i", "0", "--dump-start", "--room", &format!("{},{}", room.0, room.1), "--loading-jank", &j.map_or("none".to_string(), |j| j.to_string()), "--balloon-seeds", "0", "--category", &cat.to_string()];
    let out = std::process::Command::new(std::env::current_exe()?).args(args).output()?;
    anyhow::ensure!(out.status.success(), "rewrite {}: {}", args.join(" "), String::from_utf8_lossy(&out.stderr).trim());
    Ok(String::from_utf8(out.stdout)?.lines().map(str::to_string).collect())
}
