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
        /// What to look for: "promote-cell", "promote-capture", "inline",
        /// "if-convert" or "demote-create".
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
    /// Try each candidate entry on top of the recipe, keep the ones that
    /// verify, and print them.
    ///
    /// For rules that are *expected* to fail at some sites. `if_convert` is the
    /// motivating case: its transformation is always structurally valid, but
    /// the resulting `select` may be asked to combine values the representation
    /// cannot hold per lane, and the only way to find out is to run it.
    Screen {
        /// File of candidate entries, one JSON object per line, as `suggest`
        /// emits.
        #[arg(long)]
        candidates: String,
        /// Frames to run when screening. Lower is faster and less thorough;
        /// re-verify the accepted set at full length afterwards.
        #[arg(long, default_value_t = 20)]
        frames: u32,
    },
    /// Print a digest of the canonical observation after each frame.
    ///
    /// `verify` compares two programs built from the same sources, so it cannot
    /// check a change to the *harness* - a new builtin, a different lane
    /// layout. This prints something stable that can be compared across such a
    /// change by hand: run it, make the change, run it again, diff.
    ///
    /// The digest is over lane content, and lane rows are a set, so it is
    /// deliberately blind to how lanes are distributed across states.
    Observe {
        #[arg(long, default_value_t = 30)]
        frames: u32,
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
        celeste_rust::branch_sites::reset();
        celeste_rust::create_sites::reset();
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
    let (fragments, mean_fragments, max_fragments) = run.states_before_merge();
    println!(
        "{:<10} fragments before merge: {} total, {:.0} mean, {} max per frame",
        "", fragments, mean_fragments, max_fragments
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

        let (splits, executions, distinct) = celeste_rust::branch_sites::totals();
        println!();
        println!(
            "branches: {} of {} executions split the state, across {} distinct sites",
            splits, executions, distinct
        );
        println!("{:<34} {:<32} {:>9} {:>9}", "function", "block", "splits", "uniform");
        for (function, block, site) in
            celeste_rust::branch_sites::report().into_iter().take(15)
        {
            println!(
                "{:<34} {:<32} {:>9} {:>9}",
                function, block, site.splits, site.uniform
            );
        }

        // Which convertible triangles are actually worth converting.
        //
        // The blocker analysis says which triangles *can* be converted. That is
        // a different question from whether they *cost* anything, and the two
        // have already been confused once: the first 81 conversions were chosen
        // on convertibility alone, removed 4.6% of splits, and made the frame
        // slower. A branch is free when its condition is uniform across lanes,
        // and converting a free branch is worse than leaving it - the arm stops
        // being skipped and runs on every execution instead.
        {
            let mut rows: Vec<(bool, String, String, celeste_rust::branch_sites::BranchSite)> =
                Vec::new();
            for (name, fun) in &program.functions {
                for join in fun.cfg.named.keys() {
                    use celeste_rust::rewrite::rules::if_convert;
                    let convertible = if_convert::triangle_at(&fun.cfg, join).is_some();
                    let Some(t) = if_convert::triangle_shaped(&fun.cfg, join) else { continue };
                    let head = t
                        .head
                        .as_ref()
                        .map_or("__entry".to_string(), |l| l.as_str().to_string());
                    let site = celeste_rust::branch_sites::lookup(name.as_str(), &head);
                    rows.push((
                        convertible,
                        name.as_str().to_string(),
                        join.as_str().to_string(),
                        site,
                    ));
                }
            }
            rows.sort_by_key(|(_, f, j, s)| (std::cmp::Reverse(s.splits), f.clone(), j.clone()));
            let (all_splits, _, _) = celeste_rust::branch_sites::totals();
            println!();
            println!(
                "{:<14} {:>10} {:>10} {:>10} {:>10}",
                "triangles", "count", "ever split", "splits", "% of all"
            );
            for (label, want) in [("convertible", true), ("blocked", false)] {
                let group: Vec<_> = rows.iter().filter(|(c, ..)| *c == want).collect();
                let splits: u64 = group.iter().map(|(_, _, _, s)| s.splits).sum();
                println!(
                    "{:<14} {:>10} {:>10} {:>10} {:>9.1}%",
                    label,
                    group.len(),
                    group.iter().filter(|(_, _, _, s)| s.splits > 0).count(),
                    splits,
                    100.0 * splits as f64 / all_splits.max(1) as f64
                );
            }
            println!(
                "{:<14} {:>10} {:>10} {:>10} {:>9.1}%",
                "all branches", "-", "-", all_splits, 100.0
            );
            println!();
            // What is in the way, for the ones that are worth clearing. The
            // blocked triangles that never split do not matter however easy
            // they look; these are the whole of the prize.
            let mut why: std::collections::BTreeMap<(String, String), String> = Default::default();
            for (name, fun) in &program.functions {
                for (join, reasons) in celeste_rust::rewrite::rules::if_convert::blockers(fun) {
                    let mut kinds: Vec<String> = reasons
                        .iter()
                        .map(|r| {
                            let mut k = r.split_whitespace().next().unwrap_or("?").to_string();
                            if r.ends_with(" create") {
                                k.push_str(" create");
                            }
                            k
                        })
                        .collect();
                    kinds.sort();
                    kinds.dedup();
                    why.insert(
                        (name.as_str().to_string(), join.as_str().to_string()),
                        kinds.join(" + "),
                    );
                }
            }
            println!(
                "{:<8} {:<20} {:<30} {:>7} {:>7}  {}",
                "state", "function", "join", "splits", "uniform", "in the way"
            );
            for (convertible, function, join, site) in rows.iter().take(12) {
                println!(
                    "{:<8} {:<20} {:<30} {:>7} {:>7}  {}",
                    if *convertible { "ready" } else { "blocked" },
                    function,
                    join,
                    site.splits,
                    site.uniform,
                    why.get(&(function.clone(), join.clone()))
                        .map_or("-", |s| s.as_str())
                );
            }
        }

        // A `create` accessor that never creates is a read wearing a mutation's
        // clothes, and it is the mutation that blocks if-conversion.
        println!();
        println!(
            "{:<20} {:>12} {:>12} {:>10}",
            "create accessor", "found", "created", "create rate"
        );
        for (site, found, created) in celeste_rust::create_sites::summary() {
            let total = found + created;
            if total == 0 {
                continue;
            }
            println!(
                "{:<20} {:>12} {:>12} {:>9.2}%",
                site.name(),
                found,
                created,
                100.0 * created as f64 / total as f64
            );
        }

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

            // Replaying the recipe is the prefix of every command in this tool,
            // so it decides how fast the project is to work on. Print the split
            // so that it stays visible instead of quietly growing.
            let mut total = celeste_rust::rewrite::recipe::StepTiming::default();
            for r in &reports {
                total.add(&r.timing);
            }
            println!(
                "replay: {:.1}s total - clone {:.1}s, apply {:.1}s, validate {:.1}s, verify {:.1}s",
                total.total().as_secs_f64(),
                total.clone.as_secs_f64(),
                total.apply.as_secs_f64(),
                total.validate.as_secs_f64(),
                total.verify.as_secs_f64(),
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
                        for (at, callee, captures) in
                            celeste_rust::rewrite::rules::inline::candidates(&program, fun)
                        {
                            let captures: Vec<String> = captures
                                .iter()
                                .map(|c| format!("%{}", usize::from(*c)))
                                .collect();
                            let mut entry = serde_json::json!({
                                "id": format!("{}{:03}", prefix, n),
                                "rule": "inline",
                                "fn": name.as_str(),
                                "at": format!("%{}", usize::from(at)),
                                "callee": callee,
                            });
                            if !captures.is_empty() {
                                entry["captures"] = serde_json::json!(captures);
                            }
                            println!("{}", entry);
                            n += 1;
                        }
                    }
                    eprintln!("# {} inlinable call site(s)", n);
                }
                "if-convert" => {
                    for (name, fun) in &program.functions {
                        for join in
                            celeste_rust::rewrite::rules::if_convert::candidates(fun)
                        {
                            println!(
                                "{}",
                                serde_json::json!({
                                    "id": format!("{}{:03}", prefix, n),
                                    "rule": "if_convert",
                                    "fn": name.as_str(),
                                    "join": join.as_str(),
                                })
                            );
                            n += 1;
                        }
                    }
                    // What is blocking the rest matters as much as what is
                    // convertible: it says which earlier stage to work on.
                    let mut blocked = 0;
                    let mut by_kind: std::collections::BTreeMap<String, usize> =
                        Default::default();
                    for (_, fun) in &program.functions {
                        for (_, reasons) in
                            celeste_rust::rewrite::rules::if_convert::blockers(fun)
                        {
                            blocked += 1;
                            for reason in reasons {
                                // `create` accessors are kept separate: they are
                                // blocked because they *mutate*, which a
                                // different rule has to discharge than a plain
                                // read that might fault.
                                let mut kind = reason
                                    .split_whitespace()
                                    .next()
                                    .unwrap_or("?")
                                    .to_string();
                                if reason.ends_with(" create") {
                                    kind.push_str(" create");
                                }
                                *by_kind.entry(kind).or_default() += 1;
                            }
                        }
                    }
                    // Per-triangle blocker *sets*, not per-instruction counts.
                    // A triangle is unblocked only when its last blocker goes,
                    // so the counts above say what work exists while this says
                    // what would actually pay: 46 `create` accessors were
                    // demoted and not one triangle came free, because each
                    // still had the `store` that followed it.
                    let mut by_set: std::collections::BTreeMap<String, usize> =
                        Default::default();
                    for (_, fun) in &program.functions {
                        for (_, reasons) in
                            celeste_rust::rewrite::rules::if_convert::blockers(fun)
                        {
                            let mut kinds: Vec<String> = reasons
                                .iter()
                                .map(|r| {
                                    let mut k = r
                                        .split_whitespace()
                                        .next()
                                        .unwrap_or("?")
                                        .to_string();
                                    if r.ends_with(" create") {
                                        k.push_str(" create");
                                    }
                                    k
                                })
                                .collect();
                            kinds.sort();
                            kinds.dedup();
                            *by_set.entry(kinds.join(" + ")).or_default() += 1;
                        }
                    }
                    eprintln!("#");
                    eprintln!("# blocked triangles by the *set* of things in the way:");
                    let mut sets: Vec<_> = by_set.into_iter().collect();
                    sets.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
                    for (set, count) in sets {
                        eprintln!("#   {:>6}  {}", count, set);
                    }
                    eprintln!("# {} if-convertible join(s)", n);
                    eprintln!(
                        "# {} more triangle(s) blocked by unspeculatable arms:",
                        blocked
                    );
                    let mut kinds: Vec<_> = by_kind.into_iter().collect();
                    kinds.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
                    for (kind, count) in kinds {
                        eprintln!("#   {:>6}  {}", count, kind);
                    }
                }
                "demote-create" => {
                    let candidates =
                        celeste_rust::rewrite::rules::demote_create::candidates(&program);
                    for (i, (function, at)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "demote_create",
                                "fn": function,
                                "at": format!("%{}", usize::from(*at)),
                            })
                        );
                    }
                    eprintln!(
                        "# {} creating accessor(s) blocking an if_convert triangle.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each is a claim about the run, not a theorem. Screen at the depth \
                         the result will be used at:"
                    );
                    eprintln!("#   the field creations that do happen fall in 2 frames out of 34.");
                }
                "promote-capture" => {
                    let mut total_sites = 0;
                    for (function, index, sites) in
                        celeste_rust::rewrite::rules::promote_capture::candidates(&program)
                    {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, n),
                                "rule": "promote_capture",
                                "fn": function,
                                "index": index,
                            })
                        );
                        n += 1;
                        total_sites += sites;
                    }
                    eprintln!(
                        "# {} promotable capture(s) over {} creation site(s)",
                        n, total_sites
                    );
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

        Command::Screen { candidates, frames } => {
            let (program, _) = build(&recipe)?;
            let baseline = Program::compile_from_disk()?;
            let trace = celeste_rust::rewrite::verify::observation_trace(&baseline, frames)?;

            let text = std::fs::read_to_string(&candidates)?;
            let candidates = Recipe::parse(&text)?;
            let total = candidates.entries.len();
            let (accepted, runs) = screen_group(&program, &candidates.entries, &trace, frames);
            for entry in &accepted {
                println!("{}", serde_json::to_string(entry)?);
            }
            eprintln!(
                "# kept {} of {} candidate(s), screened over {} frames in {} run(s)",
                accepted.len(),
                total,
                frames,
                runs
            );
        }

        Command::Observe { frames } => {
            let (program, _) = build(&recipe)?;
            let trace = celeste_rust::rewrite::verify::observation_trace(&program, frames)?;
            println!("{:>6} {:>8} {:>20}", "frame", "states", "digest");
            for (frame, observation) in trace.iter().enumerate() {
                use std::hash::{Hash, Hasher};
                let mut hasher = rustc_hash::FxHasher::default();
                format!("{:?}", observation).hash(&mut hasher);
                println!(
                    "{:>6} {:>8} {:>20x}",
                    frame,
                    observation.len(),
                    hasher.finish()
                );
            }
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

/// One screening trial: apply `entries` to a copy of `base` and check the
/// result against the baseline observation trace.
///
/// A candidate may not merely diverge - it may panic. Speculating an arm runs it
/// on lanes that would have skipped it, and the interpreter's arithmetic asserts
/// on values it should never have seen (`Pico8Num::rem` on a negative number,
/// say). That is the loud failure the design accepts, but it must not take the
/// screener down with it: the trial program is discarded either way, so
/// unwinding across it is safe.
fn screen_trial(
    base: &Program,
    entries: &[celeste_rust::rewrite::recipe::RewriteEntry],
    trace: &[std::collections::BTreeSet<celeste_rust::rewrite::verify::StateObservation>],
    frames: u32,
) -> std::result::Result<Program, String> {
    let mut trial = base.clone();
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        for entry in entries {
            apply_entry(&mut trial, entry)?;
        }
        celeste_rust::rewrite::verify::differential_against_trace(trace, &trial, frames)
    }));
    match outcome {
        Ok(Ok(None)) => Ok(trial),
        Ok(Ok(Some(d))) => Err(format!("frame {}: {}", d.frame, first_line(&d.detail))),
        Ok(Err(e)) => Err(first_line(&format!("{}", e))),
        Err(_) => Err("panicked - see the message above".to_string()),
    }
}

/// Screen candidates by group testing rather than one at a time.
///
/// A screening run costs a full differential execution - about 14 seconds at 34
/// frames - and candidate sets are usually almost all good. Trying them one at a
/// time therefore spends N runs to learn something that one run would usually
/// settle: 38 `if_convert` candidates took 9 minutes to accept all 38, and 48
/// `demote_create` candidates would have taken 11 minutes to reject 2.
///
/// So try the whole group at once. If it passes, every entry in it is accepted
/// together. If it fails, split it in half and try each half on top of what has
/// been accepted so far, recursively, until a failing group is a single entry -
/// which is then the one to drop. That is `bisect` applied to a set rather than
/// to a recipe, and it costs roughly `k * log(N/k)` runs for `k` bad entries out
/// of `N`, against `N` for the sequential version. All good is one run.
///
/// Two properties are worth being explicit about, because this is a *trusted*
/// check and a faster trusted check is only worth having if it is still trusted:
///
/// * **The accepted set is always one that has actually been run.** Every
///   acceptance runs the differential on the cumulative program, not on the
///   group in isolation, so the program returned here is exactly the one the
///   last passing run verified. No separate confirmation pass is needed.
/// * **Interaction between candidates is handled the same way as before.** Two
///   entries that conflict - structurally, or by diverging only together - make
///   their group fail, and the split then tries the second on top of the first,
///   which is precisely what the sequential version did.
fn screen_group(
    base: &Program,
    entries: &[celeste_rust::rewrite::recipe::RewriteEntry],
    trace: &[std::collections::BTreeSet<celeste_rust::rewrite::verify::StateObservation>],
    frames: u32,
) -> (Vec<celeste_rust::rewrite::recipe::RewriteEntry>, usize) {
    let mut program = base.clone();
    let mut accepted = Vec::new();
    let mut runs = 0;
    // Groups still to try, in recipe order. `push_front` keeps a split group's
    // halves ahead of everything queued behind it.
    let mut queue: std::collections::VecDeque<Vec<_>> = std::collections::VecDeque::new();
    if !entries.is_empty() {
        queue.push_back(entries.to_vec());
    }
    while let Some(group) = queue.pop_front() {
        runs += 1;
        let ids = |g: &[celeste_rust::rewrite::recipe::RewriteEntry]| match g {
            [one] => one.id.clone(),
            _ => format!("{}..{} ({})", g[0].id, g[g.len() - 1].id, g.len()),
        };
        match screen_trial(&program, &group, trace, frames) {
            Ok(next) => {
                eprintln!("run {:>3}  keep {}", runs, ids(&group));
                program = next;
                accepted.extend(group);
            }
            Err(why) if group.len() == 1 => {
                eprintln!("run {:>3}  DROP {} - {}", runs, group[0].id, why);
            }
            Err(why) => {
                eprintln!("run {:>3}  split {} - {}", runs, ids(&group), why);
                let second = group[group.len() / 2..].to_vec();
                let first = group[..group.len() / 2].to_vec();
                queue.push_front(second);
                queue.push_front(first);
            }
        }
    }
    (accepted, runs)
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").chars().take(140).collect()
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
