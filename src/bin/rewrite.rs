//! Driver for the program rewriting system. See plans/rewrite-plan.md.
//!
//!     rewrite build                 # replay the recipe, report progress
//!     rewrite print [--fn NAME]     # dump textual IR
//!     rewrite diff --id ID          # what one entry changed
//!     rewrite check                 # structural validation only
//!     rewrite verify [--frames N]   # differential run against the original
//!     rewrite bisect [--frames N]   # find the first entry that breaks it

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};

use celeste_rust::rewrite::print::{format_function, format_program};
use celeste_rust::rewrite::program::Program;
use celeste_rust::rewrite::recipe::{apply_entry, build, Recipe};
use celeste_rust::rewrite::validate::validate_program;
use celeste_rust::rewrite::verify::differential_abstract;

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
    /// Replay the recipe and report what each entry did.
    Build,
    /// Print the textual IR of the rewritten program.
    Print {
        /// Only this function.
        #[arg(long)]
        r#fn: Option<String>,
    },
    /// Show what one recipe entry changed, as a textual diff.
    Diff {
        #[arg(long)]
        id: String,
        #[arg(long)]
        r#fn: Option<String>,
    },
    /// Structural validation of the rewritten program.
    Check,
    /// Differentially run the rewritten program against the original.
    Verify {
        #[arg(long, default_value_t = 30)]
        frames: u32,
    },
    /// Find the first recipe entry after which the differential run fails.
    Bisect {
        #[arg(long, default_value_t = 30)]
        frames: u32,
    },
    /// Propose recipe entries. This is the untrusted half of the system: the
    /// output is a suggestion, and only survives if the rule's verifier accepts
    /// it. Pipe it into the recipe and re-run `build`.
    Suggest {
        /// What to look for: "promote-cell" or "inline".
        #[arg(default_value = "promote-cell")]
        what: String,
        /// Prefix for the generated ids.
        #[arg(long, default_value = "p")]
        prefix: String,
    },
    /// Report how many LocalEnv slots each function needs now versus how many
    /// it would need if values with disjoint live ranges shared a slot.
    Slots {
        /// Only show functions needing at least this many slots today.
        #[arg(long, default_value_t = 40)]
        min: usize,
    },
    /// Run the rewritten program and report time, memory and lane counts.
    ///
    /// Note peak RSS is the process-wide high-water mark, so with `--baseline`
    /// the second figure includes the first run's peak. For an accurate memory
    /// comparison, run the two separately.
    Bench {
        #[arg(long, default_value_t = 34)]
        frames: u32,
        /// Also run the unmodified program, for comparison.
        #[arg(long)]
        baseline: bool,
        /// Print a self-time breakdown by span. Costs a few percent.
        #[arg(long)]
        profile: bool,
    },
}

fn peak_rss_kb() -> u64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmHWM:"))
                .and_then(|l| l.split_whitespace().nth(1)?.parse().ok())
        })
        .unwrap_or(0)
}

fn bench(label: &str, program: &Program, frames: u32, profile: bool) -> Result<()> {
    let mut run = celeste_rust::rewrite::verify::AbstractRun::start(program)?;
    if profile {
        celeste_rust::interpreter::tracing::reset_tracing();
        celeste_rust::interpreter::tracing::enable_tracing();
    }
    let start = std::time::Instant::now();
    for _ in 1..=frames {
        run.step()?;
    }
    let elapsed = start.elapsed();
    let lanes = run.lane_count();
    let heap_len: usize = run.states().iter().map(|s| s.heap.len()).max().unwrap_or(0);
    let env_len: usize = run
        .states()
        .iter()
        .map(|s| s.local_env.iter().count())
        .max()
        .unwrap_or(0);
    println!(
        "{:<10} {:>3} frames  {:>8.2}s  {:>10} lanes  {:>8.2} GB peak  {:>7.1} us/lane",
        label,
        frames,
        elapsed.as_secs_f64(),
        lanes,
        peak_rss_kb() as f64 / 1048576.0,
        elapsed.as_secs_f64() * 1e6 / lanes.max(1) as f64
    );
    println!(
        "{:<10} end-of-frame state: heap {} cells, local_env {} entries",
        "", heap_len, env_len
    );

    if profile {
        let rows = celeste_rust::interpreter::tracing::span_self_time_summary();
        let measured: u64 = rows.iter().map(|r| r.self_us).sum();
        println!();
        println!(
            "{:<28} {:>9} {:>7} {:>9} {:>12}",
            "span", "self (s)", "%", "total (s)", "count"
        );
        for row in rows.iter().take(20) {
            println!(
                "{:<28} {:>9.2} {:>6.1}% {:>9.2} {:>12}",
                format!("{}:{}", row.category, row.name),
                row.self_us as f64 / 1e6,
                100.0 * row.self_us as f64 / measured.max(1) as f64,
                row.total_us as f64 / 1e6,
                row.count
            );
        }
        println!(
            "{:<28} {:>9.2} of {:.2}s wall clock (the rest is outside any span)",
            "measured", measured as f64 / 1e6, elapsed.as_secs_f64()
        );

        // Spans in the `merge_site` category exist only to bracket other work,
        // so they have no self time worth reporting - what matters is how much
        // is under them.
        let sites: Vec<_> = rows.iter().filter(|r| r.category == "merge_site").collect();
        if !sites.is_empty() {
            println!();
            for row in sites {
                println!(
                    "{:<28} {:>9} {:>7} {:>9.2} {:>12}",
                    format!("{}:{}", row.category, row.name),
                    "-",
                    "-",
                    row.total_us as f64 / 1e6,
                    row.count
                );
            }
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let recipe = Recipe::load(&cli.recipe)?;

    match cli.command {
        Command::Build => {
            let (program, reports) = build(&recipe)?;
            if reports.is_empty() {
                println!("recipe is empty; program is as compiled");
            }
            for r in &reports {
                println!(
                    "{:<10} {:<14} {:>5} change(s)   {:>6} -> {:<6} instrs   {:>4} -> {:<4} blocks",
                    r.id,
                    r.rule,
                    r.changes,
                    r.instructions_before,
                    r.instructions_after,
                    r.blocks_before,
                    r.blocks_after
                );
            }
            println!();
            println!(
                "final: {} functions, {} blocks, {} instructions",
                program.functions.len(),
                program.block_count(),
                program.instruction_count()
            );
        }

        Command::Print { r#fn } => {
            let (program, _) = build(&recipe)?;
            match r#fn {
                Some(name) => print!("{}", format_function(program.get(&name)?)),
                None => print!("{}", format_program(&program)),
            }
        }

        Command::Diff { id, r#fn } => {
            let index = recipe
                .entries
                .iter()
                .position(|e| e.id == id)
                .ok_or_else(|| anyhow!("no recipe entry with id {:?}", id))?;

            let mut program = Program::compile_from_disk()?;
            for entry in &recipe.entries[..index] {
                apply_entry(&mut program, entry)?;
            }
            let before = render(&program, &r#fn)?;
            let report = apply_entry(&mut program, &recipe.entries[index])?;
            let after = render(&program, &r#fn)?;

            println!(
                "# {} ({}) - {} change(s)",
                report.id, report.rule, report.changes
            );
            print!("{}", unified_diff(&before, &after));
        }

        Command::Check => {
            let (program, _) = build(&recipe)?;
            let errors = validate_program(&program);
            if errors.is_empty() {
                println!(
                    "ok: {} functions, {} blocks, {} instructions",
                    program.functions.len(),
                    program.block_count(),
                    program.instruction_count()
                );
            } else {
                for e in errors.iter().take(40) {
                    println!("{}", e);
                }
                return Err(anyhow!("{} structural error(s)", errors.len()));
            }
        }

        Command::Verify { frames } => {
            let baseline = Program::compile_from_disk()?;
            let (candidate, _) = build(&recipe)?;
            println!("running {} frames of both programs...", frames);
            let start = std::time::Instant::now();
            match differential_abstract(&baseline, &candidate, frames)? {
                None => println!(
                    "ok: identical through frame {} ({:.1}s)",
                    frames,
                    start.elapsed().as_secs_f64()
                ),
                Some(d) => {
                    println!("DIVERGED at frame {}:\n  {}", d.frame, d.detail);
                    return Err(anyhow!("differential verification failed"));
                }
            }
        }

        Command::Suggest { what, prefix } => {
            // Suggestions are made against the program as the recipe leaves it,
            // so running suggest / append / build repeatedly converges.
            let (program, _) = build(&recipe)?;
            let mut n = 0;
            match what.as_str() {
                "promote-cell" => {
                    let mut total_loads = 0;
                    for (name, fun) in &program.functions {
                        for (cell, loads) in
                            celeste_rust::rewrite::rules::promote_cell::candidates(fun)
                        {
                            println!(
                                "{}",
                                serde_json::json!({
                                    "id": format!("{}{:03}", prefix, n),
                                    "rule": "promote_cell",
                                    "fn": name.as_str(),
                                    "cell": format!("%{}", usize::from(cell)),
                                })
                            );
                            n += 1;
                            total_loads += loads;
                        }
                    }
                    eprintln!(
                        "# {} promotable cell(s), {} load(s) they would remove",
                        n, total_loads
                    );
                }
                "inline" => {
                    for (name, fun) in &program.functions {
                        for (at, callee) in
                            celeste_rust::rewrite::rules::inline::candidates(&program, fun)
                        {
                            println!(
                                "{}",
                                serde_json::json!({
                                    "id": format!("{}{:03}", prefix, n),
                                    "rule": "inline",
                                    "fn": name.as_str(),
                                    "at": format!("%{}", usize::from(at)),
                                    "callee": callee,
                                })
                            );
                            n += 1;
                        }
                    }
                    eprintln!("# {} inlinable call site(s)", n);
                }
                other => return Err(anyhow!("unknown suggestion kind {:?}", other)),
            }
        }

        Command::Slots { min } => {
            let (program, _) = build(&recipe)?;
            let mut reports: Vec<_> = program
                .functions
                .values()
                .map(celeste_rust::rewrite::liveness::slot_report)
                .filter(|r| r.env_slots_now >= min)
                .collect();
            reports.sort_by_key(|r| std::cmp::Reverse(r.env_slots_now));
            println!(
                "{:<34} {:>10} {:>10} {:>8}  {}",
                "function", "slots now", "if packed", "defs", "saving"
            );
            let mut total_now = 0;
            let mut total_packed = 0;
            for r in &reports {
                total_now += r.env_slots_now;
                total_packed += r.env_slots_packed;
                println!(
                    "{:<34} {:>10} {:>10} {:>8}  {:.1}x",
                    r.name,
                    r.env_slots_now,
                    r.env_slots_packed,
                    r.definitions,
                    r.env_slots_now as f64 / r.env_slots_packed.max(1) as f64
                );
            }
            println!();
            println!(
                "total over shown functions: {} -> {} ({:.1}x)",
                total_now,
                total_packed,
                total_now as f64 / total_packed.max(1) as f64
            );
        }

        Command::Bench { frames, baseline, profile } => {
            if baseline {
                bench("original", &Program::compile_from_disk()?, frames, profile)?;
            }
            let (program, _) = build(&recipe)?;
            bench("rewritten", &program, frames, profile)?;
        }

        Command::Bisect { frames } => {
            let baseline = Program::compile_from_disk()?;
            let mut program = Program::compile_from_disk()?;
            for entry in &recipe.entries {
                apply_entry(&mut program, entry)?;
                print!("after {:<10} ... ", entry.id);
                use std::io::Write;
                std::io::stdout().flush().ok();
                match differential_abstract(&baseline, &program, frames)? {
                    None => println!("ok"),
                    Some(d) => {
                        println!("DIVERGED at frame {}", d.frame);
                        println!("  {}", d.detail);
                        return Err(anyhow!("first bad entry: {}", entry.id));
                    }
                }
            }
            println!("all {} entries verify", recipe.entries.len());
        }
    }

    Ok(())
}

fn render(program: &Program, only: &Option<String>) -> Result<String> {
    Ok(match only {
        Some(name) => format_function(program.get(name)?),
        None => format_program(program),
    })
}

/// Minimal unified-ish diff. Good enough for eyeballing what a rewrite did;
/// pipe through a real differ if you want more.
fn unified_diff(before: &str, after: &str) -> String {
    let a: Vec<&str> = before.lines().collect();
    let b: Vec<&str> = after.lines().collect();

    // Longest common subsequence over lines. CFG dumps are small.
    let mut lcs = vec![vec![0usize; b.len() + 1]; a.len() + 1];
    for i in (0..a.len()).rev() {
        for j in (0..b.len()).rev() {
            lcs[i][j] = if a[i] == b[j] {
                lcs[i + 1][j + 1] + 1
            } else {
                lcs[i + 1][j].max(lcs[i][j + 1])
            };
        }
    }

    let mut out = String::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        if a[i] == b[j] {
            i += 1;
            j += 1;
        } else if lcs[i + 1][j] >= lcs[i][j + 1] {
            out.push_str(&format!("-{}\n", a[i]));
            i += 1;
        } else {
            out.push_str(&format!("+{}\n", b[j]));
            j += 1;
        }
    }
    for line in &a[i..] {
        out.push_str(&format!("-{}\n", line));
    }
    for line in &b[j..] {
        out.push_str(&format!("+{}\n", line));
    }
    out
}
