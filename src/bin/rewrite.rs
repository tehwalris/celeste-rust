//! Driver for the program rewriting system. See plans/rewrite-plan.md.
//!
//!     rewrite build                 # replay the recipe, report progress
//!     rewrite print [--fn NAME]     # dump textual IR
//!     rewrite diff --id ID          # what one entry changed
//!     rewrite check                 # structural validation only
//!     rewrite verify [--frames N]   # differential run against the original
//!     rewrite bisect [--frames N]   # find the first entry that breaks it

use anyhow::{anyhow, Context, Result};
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
        /// "if-convert", "demote-create", "pin-builtin", "convert-ternary",
        /// "decompose-truthy", "speculate", "speculate-region", "sink-store",
        /// "absorb-stores", "expand-bool", "convert-assert",
        /// "collapse-loop", "collapse-break-loop" or "unroll-loop".
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
    /// Estimate the per-class specialization prize: instructions whose
    /// value the merge-partition cells determine, and the speculated
    /// select arms they discard per class, weighted by a measured
    /// per-instruction time profile.
    Classdead {
        #[arg(long, default_value_t = 40)]
        frames: u32,
    },
    /// Run the rewritten program and report time, memory and lane counts.
    ///
    /// Note peak RSS is the process-wide high-water mark, so with `--baseline`
    /// the second figure includes the first run's peak. For an accurate memory
    /// comparison, run the two separately.
    /// Certify that the conservative boundary widenings (timer pins,
    /// dash_effect_time clamp) do not change the reachable set: run the
    /// search with only the historic rem widening, apply the conservative
    /// widenings post hoc each frame, and compare against the widen-every-
    /// boundary run. Equal through N frames means the widened fields did not
    /// influence gameplay within that horizon.
    Widencheck {
        #[arg(long, default_value_t = 34)]
        frames: u32,
    },
    /// Certify the canonical-state mapping and the deopt path: run the
    /// rewritten program with deopt *forced* on every state of every frame -
    /// so each frame goes specialized-input -> to_canonical -> plain program
    /// -> from_canonical - and compare observations frame by frame against a
    /// plain-program run. Equal through N frames means both mapping
    /// directions and the plain re-run are exercised on every reachable state
    /// without changing meaning.
    Deoptcheck {
        #[arg(long, default_value_t = 34)]
        frames: u32,
    },
    /// Replay a concrete input sequence (the reference TAS) and probe every
    /// refinement level's row table and (e, g) band per frame. The first
    /// frame where the true winning path is missing from a level's table or
    /// fails its band test is the exact address of a soundness leak; a fully
    /// passing trace is the strongest possible certificate that the bands
    /// contain the real optimum.
    TraceWitness {
        /// TAS file: comment lines, then comma-separated input bytes.
        #[arg(long)]
        tas: String,
        #[arg(long)]
        horizon: u32,
        /// Comma-separated precision levels to probe, e.g. "0,1,2,3".
        #[arg(long)]
        levels: String,
        /// Checkpoint base: level 0 at <base>/<room-stem>, level k at
        /// <base>/<room-stem>-k<k> (stem "room1" for the default room,
        /// "room<x><y>" otherwise; see game_runner::room_dir_stem).
        #[arg(long)]
        base_dir: String,
    },
    /// Extract an optimal TAS from a refined level's band by forward greedy
    /// walk: from the initial state, try every input byte each frame,
    /// keep the ones whose concrete successor lands in the level's row table
    /// with e <= f and g <= horizon - f, and follow one of them. With every
    /// per-frame candidate set nonempty through the horizon, the walk is
    /// guaranteed to end on a g=0 (winning) row - the band is exactly the
    /// set of states on some optimal-horizon path. Optionally compares
    /// against a reference TAS, preferring its byte whenever it qualifies.
    ExtractTas {
        #[arg(long)]
        horizon: u32,
        /// Precision level whose band to walk (16 = exact rem).
        #[arg(long, default_value_t = 16)]
        level: u8,
        /// Checkpoint base: level 0 at <base>/<room-stem>, level k at
        /// <base>/<room-stem>-k<k> (stem "room1" for the default room,
        /// "room<x><y>" otherwise; see game_runner::room_dir_stem).
        #[arg(long)]
        base_dir: String,
        /// Optional reference TAS to compare against.
        #[arg(long)]
        tas: Option<String>,
    },
    /// Enumerate ALL optimal-horizon trajectories in a refined level's band:
    /// layered BFS from the spawn state trying every input byte, deduping
    /// concrete successors by row key. Counts distinct row trajectories,
    /// distinct whole-pixel (x, y) position sequences (identical position
    /// histories collapsed via subset construction), and raw input-byte
    /// sequences (in log10 - don't-care buttons inflate these enormously).
    CountOptimal {
        #[arg(long)]
        horizon: u32,
        /// Precision level whose band to walk (16 = exact rem).
        #[arg(long, default_value_t = 16)]
        level: u8,
        /// Checkpoint base: level 0 at <base>/<room-stem>, level k at
        /// <base>/<room-stem>-k<k> (stem "room1" for the default room,
        /// "room<x><y>" otherwise; see game_runner::room_dir_stem).
        #[arg(long)]
        base_dir: String,
    },
    /// Census of a saved frame batch: lanes grouped by object-array shape
    /// (abstraction::object_shape), with the fruit `off`-counter spread when a
    /// fruit is alive. For diagnosing frontier bloat - e.g. room (0,0)'s
    /// post-break lanes, whose ever-incrementing `off` defeats cross-frame
    /// dedup (plans/room00-plan.md).
    ShapeCensus {
        /// Checkpoint dir with saved frames (bench --save-frames).
        #[arg(long)]
        checkpoint_dir: String,
        #[arg(long)]
        frame: u32,
    },
    Bench {
        #[arg(long, default_value_t = 34)]
        frames: u32,
        /// Also run the unmodified program, for comparison.
        #[arg(long)]
        baseline: bool,
        /// Print a self-time breakdown by span. Costs a few percent.
        #[arg(long)]
        profile: bool,
        /// On a frame failure (a specialization premise such as the collapsed
        /// loops' "#objects == 1" firing), re-run that state's frame under the
        /// plain program via the canonical-state mapping instead of aborting.
        /// Required to search past frame 58, where states start dying.
        #[arg(long)]
        deopt: bool,
        /// Write checkpoints (boundary states + row table) under this
        /// directory. See plans/refinement-plan.md for the format.
        #[arg(long)]
        checkpoint_dir: Option<String>,
        /// Checkpoint every N frames (and always at the final frame).
        #[arg(long, default_value_t = 5)]
        checkpoint_every: u32,
        /// Resume from the latest checkpoint in --checkpoint-dir. Fails
        /// loudly if the recipe, lua sources or search flags differ from
        /// the run that wrote it.
        #[arg(long)]
        resume: bool,
        /// Also save every frame's post-subtract boundary states under
        /// <checkpoint-dir>/frames/ - the input of the backward sweep.
        #[arg(long)]
        save_frames: bool,
        /// Previous precision level's checkpoint dir: confine this run to
        /// its band (rows with e <= f and g <= horizon - f, after
        /// coarsening each lane to that level's rem precision).
        #[arg(long)]
        band_dir: Option<String>,
        /// Horizon N for the band restriction.
        #[arg(long)]
        band_horizon: Option<u32>,
        /// The previous level's rem bits (0 = the historic full widening,
        /// 16 = exact). Validated against the previous dir's fingerprint.
        #[arg(long)]
        band_prev_bits: Option<u8>,
        /// Register a shape-dispatched variant: SHAPES=RECIPE_PATH, where
        /// SHAPES is a |-separated list of object-array shapes and each
        /// shape is a comma-separated object type-name list. Example:
        /// --variant 'player|player_spawn=rewrites.jsonl'. A state whose
        /// object-array shape matches runs its frames under that recipe's
        /// program, through the canonical mapping both ways; every other
        /// state uses the base recipe. Dispatch is semantically invisible
        /// (boundary states are identical with or without variants), so it
        /// is deliberately NOT part of the checkpoint fingerprint - the
        /// gate for registering a variant is a lane-count/observation
        /// comparison against a variant-free run.
        #[arg(long = "variant")]
        variants: Vec<String>,
    },
    /// The backward pass (see plans/refinement-plan.md): replay the saved
    /// frame batches one frame each with an origin column, build the row
    /// successor graph, and compute g(row) = min frames to the room exit by
    /// reverse BFS. Prints min(e+g) (must equal the forward first-win frame)
    /// and per-frame band sizes for the horizon; writes g.bin.
    Sweep {
        #[arg(long)]
        checkpoint_dir: String,
        /// The forward pass's last frame (its checkpoint must exist).
        #[arg(long)]
        frames: u32,
        /// Horizon N for the band statistics (defaults to --frames).
        #[arg(long)]
        horizon: Option<u32>,
        /// The forward pass being swept was band-restricted: successors
        /// outside its row table were pruned by the band, and their edges
        /// are dropped (sound) instead of being a replay-divergence error.
        #[arg(long)]
        banded: bool,
    },
}

fn peak_rss_kb() -> u64 {
    read_status_kb("VmHWM:")
}

/// Current resident set, as opposed to the `VmHWM` peak: at a frame
/// boundary the difference between the two is the mid-frame transient
/// (fragments + append-only heap-storage growth), which is invisible in
/// the peak-only number and turned out to dominate room (0,0)'s OOM.
fn current_rss_kb() -> u64 {
    read_status_kb("VmRSS:")
}

fn read_status_kb(field: &str) -> u64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with(field))
                .and_then(|l| l.split_whitespace().nth(1)?.parse().ok())
        })
        .unwrap_or(0)
}

/// Build the shape-dispatched variant registry from `--variant` specs
/// (SHAPES=RECIPE_PATH; see the flag's doc comment). `base_program` is the
/// bench's base program - its merge-partition cells must agree with every
/// variant's, because the partition patterns are process-global and mid-frame
/// hint merges inside a variant frame use them too.
fn build_variants(
    specs: &[String],
    base_program: &Program,
) -> Result<Vec<celeste_rust::rewrite::verify::Variant>> {
    use celeste_rust::rewrite::state_mapping::StateMapping;
    let mut out = Vec::new();
    for spec in specs {
        let (shapes_part, path) = spec
            .split_once('=')
            .ok_or_else(|| anyhow!("--variant must be SHAPES=RECIPE_PATH, got {:?}", spec))?;
        let shapes: Vec<Vec<String>> = shapes_part
            .split('|')
            .map(|shape| shape.split(',').map(|t| t.trim().to_string()).collect())
            .collect();
        if shapes.iter().any(|s: &Vec<String>| s.is_empty() || s.iter().any(|t| t.is_empty())) {
            return Err(anyhow!("--variant {:?}: empty shape or type name", spec));
        }
        let recipe = Recipe::load(path)
            .with_context(|| format!("--variant {:?}: loading recipe", spec))?;
        let (program, _) = build(&recipe)
            .with_context(|| format!("--variant {:?}: building program", spec))?;
        if program.merge_partition_cells != base_program.merge_partition_cells {
            return Err(anyhow!(
                "--variant {:?}: merge-partition cells {:?} differ from the base \
                 program's {:?}; the partition patterns are process-global",
                spec,
                program.merge_partition_cells,
                base_program.merge_partition_cells
            ));
        }
        out.push(celeste_rust::rewrite::verify::Variant {
            label: format!("{}[{}]", path, shapes_part),
            shapes,
            frame_cfg: celeste_rust::interpreter::fixed_env::PreparedCfg::new(
                program.frame_cfg().clone(),
            ),
            fixed_env: program.fixed_env(),
            mapping: StateMapping::from_recipe(&recipe),
        });
    }
    Ok(out)
}

/// Checkpoint configuration for a bench run.
struct CheckpointCfg {
    dir: std::path::PathBuf,
    every: u32,
    resume: bool,
    save_frames: bool,
    /// See `checkpoint::config_fingerprint`.
    fingerprint: String,
}

fn bench(
    label: &str,
    program: &Program,
    frames: u32,
    profile: bool,
    deopt: Option<(&Program, celeste_rust::rewrite::state_mapping::StateMapping)>,
    checkpoint: Option<CheckpointCfg>,
    band: Option<celeste_rust::rewrite::verify::BandFilter>,
    variants: Vec<celeste_rust::rewrite::verify::Variant>,
    variant_base_mapping: Option<celeste_rust::rewrite::state_mapping::StateMapping>,
) -> Result<()> {
    use celeste_rust::rewrite::checkpoint;
    let mut run = match deopt {
        Some((plain, mapping)) => celeste_rust::rewrite::verify::AbstractRun::start_with_deopt(
            program, plain, mapping, false,
        )?,
        None => celeste_rust::rewrite::verify::AbstractRun::start(program)?,
    };
    if !variants.is_empty() {
        let base_mapping = variant_base_mapping
            .ok_or_else(|| anyhow!("variants need the base recipe's mapping"))?;
        run.set_variants(base_mapping, variants);
    }
    if let Some(band) = band {
        run.set_band(band);
    }
    let mut start_frame = 1u32;
    if let Some(cfg) = checkpoint.as_ref().filter(|c| c.resume) {
        match checkpoint::latest(&cfg.dir)? {
            Some(frame) if frame <= frames => {
                let loaded = checkpoint::load(&cfg.dir, frame, &cfg.fingerprint)?;
                let visited = loaded.visited;
                let visited = if run.visited_table().is_some() { Some(visited) } else { None };
                run.restore(
                    loaded.states,
                    visited,
                    (loaded.meta.deopt_states as usize, loaded.meta.deopt_lanes as usize),
                )?;
                start_frame = frame + 1;
                println!(
                    "resumed from checkpoint f{:03}: {} rows, {} states",
                    frame, loaded.meta.row_count, loaded.meta.state_count
                );
                // The restored frontier may contain won lanes (room exited);
                // they are absorbing and must not be expanded.
                run.absorb_won_lanes();
            }
            Some(frame) => {
                return Err(anyhow!(
                    "latest checkpoint f{:03} is beyond --frames {}",
                    frame,
                    frames
                ))
            }
            None => println!("--resume: no checkpoint found, starting fresh"),
        }
    }
    if profile {
        celeste_rust::interpreter::tracing::reset_tracing();
        celeste_rust::branch_sites::reset();
        celeste_rust::create_sites::reset();
        celeste_rust::interpreter::tracing::enable_tracing();
        celeste_rust::instr_time::reset();
        celeste_rust::instr_time::enable();
        if std::env::var("CELESTE_INSTR_CARD").is_ok() {
            celeste_rust::instr_time::enable_cardinality();
        }
        celeste_rust::merge_stats::reset();
    }
    // Per-coordinate saturation dump (analysis): one CSV line per occupied
    // player pixel per frame, counting the frontier lanes there. With
    // frontier-only search the counts are NEW rows per (frame, x, y), which
    // is exactly the per-coordinate arrival/taper data.
    let mut xy_dump: Option<std::io::BufWriter<std::fs::File>> =
        match std::env::var("CELESTE_XY_DUMP") {
            Ok(path) => {
                let mut w = std::io::BufWriter::new(std::fs::File::create(&path)?);
                use std::io::Write;
                writeln!(w, "frame,x,y,lanes")?;
                Some(w)
            }
            Err(_) => None,
        };
    let start = std::time::Instant::now();
    let mut first_win: Option<u32> = None;
    for frame in start_frame..=frames {
        let frame_start = std::time::Instant::now();
        run.step()?;
        if let Some(cfg) = checkpoint.as_ref() {
            if cfg.save_frames {
                celeste_rust::metrics::time("fwd.save_frames", || {
                    checkpoint::save_frame_states(&cfg.dir, frame, run.states())
                })?;
            }
            if frame % cfg.every == 0 || frame == frames {
                let t = std::time::Instant::now();
                let empty = celeste_rust::interpreter::row_table::RowTable::default();
                let path = checkpoint::save(
                    &cfg.dir,
                    frame,
                    &cfg.fingerprint,
                    run.states(),
                    run.visited_table().unwrap_or(&empty),
                    run.deopt_events(),
                )?;
                println!(
                    "  checkpoint {} written in {:.1}s",
                    path.display(),
                    t.elapsed().as_secs_f64()
                );
            }
        }
        if let Some(w) = xy_dump.as_mut() {
            use std::io::Write;
            let mut hist: std::collections::BTreeMap<(i16, i16), u64> = Default::default();
            for state in run.states() {
                if let Some(points) =
                    celeste_rust::interpreter::abstraction::player_xy_per_lane(state)
                {
                    for p in points {
                        *hist.entry(p).or_default() += 1;
                    }
                }
            }
            for ((x, y), lanes) in hist {
                writeln!(w, "{},{},{},{}", frame, x, y, lanes)?;
            }
        }
        // Room-exit probe: lanes whose room.x reached the win value have won
        // the start room. The earliest such frame is the optimal TAS length
        // under the search's abstractions.
        let win_x = celeste_rust::game_runner::win_room_x();
        let win_lanes: usize = run
            .states()
            .iter()
            .map(|s| celeste_rust::interpreter::abstraction::count_room_x_lanes(s, win_x))
            .sum();
        if win_lanes > 0 && first_win.is_none() {
            first_win = Some(frame);
        }
        // Won lanes are absorbing for a room-scoped search: they stay in the
        // row table (their arrival frame IS the result) but must not be
        // expanded - next-room simulation is out of scope (and, for the
        // default room, would hit the deliberate `sin` guard). Drop them here,
        // after the probe counted them and after checkpoint/frame saving
        // recorded them.
        if win_lanes > 0 {
            run.absorb_won_lanes();
        }
        println!(
            "frame {:>3}: {:>7.2}s  {:>10} lanes  rss {:>5.1} GB (peak {:>5.1}){}",
            frame,
            frame_start.elapsed().as_secs_f64(),
            run.lane_count(),
            current_rss_kb() as f64 / 1048576.0,
            peak_rss_kb() as f64 / 1048576.0,
            if win_lanes > 0 {
                format!("  WIN: {} lanes in room ({},_)", win_lanes, win_x)
            } else {
                String::new()
            }
        );
    }
    if let Some(frame) = first_win {
        println!("first room-exit lanes appeared at frame {}", frame);
    }
    let elapsed = start.elapsed();
    celeste_rust::metrics::dump(
        "bench",
        checkpoint.as_ref().map(|c| c.dir.as_path()),
        &[
            ("frames", frames.to_string()),
            ("lanes", run.lane_count().to_string()),
            ("peak_rss_kb", peak_rss_kb().to_string()),
        ],
    );
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
    let (deopt_states, deopt_lanes) = run.deopt_events();
    if deopt_states > 0 {
        println!(
            "{:<10} deopt: {} state(s) / {} lanes re-ran under the plain program",
            "", deopt_states, deopt_lanes
        );
    }
    let (variant_states, variant_lanes, variant_fallbacks) = run.variant_events();
    if variant_states > 0 || variant_fallbacks > 0 {
        println!(
            "{:<10} variants: {} state(s) / {} lanes ran under shape variants{}",
            "",
            variant_states,
            variant_lanes,
            if variant_fallbacks > 0 {
                format!(
                    " - {} FALLBACKS to the base program (a registered variant's \
                     premises failed; fix the registry)",
                    variant_fallbacks
                )
            } else {
                String::new()
            }
        );
    }
    // How many rows are distinct if the rem cells are ignored: the gap
    // between this and the lane count is interval-refinement multiplicity
    // (the rem/flr cycle), i.e. abstraction cost rather than game states.
    if std::env::var_os("CELESTE_DISTINCT_MODULO").is_some() {
        let patterns: Vec<String> = std::env::var("CELESTE_DISTINCT_MODULO")
            .unwrap()
            .split(',')
            .map(|x| x.trim().to_string())
            .collect();
        for state in run.states() {
            let names = celeste_rust::interpreter::merge_dump::cell_names(state);
            let Some((columns, origins)) =
                celeste_rust::interpreter::virtual_merge::collect_columns_labeled(
                    std::slice::from_ref(state),
                )
            else {
                continue;
            };
            let full: Vec<_> = columns.iter().filter(|c| !c.is_uniform()).collect();
            let mut kept_names: Vec<String> = Vec::new();
            let mut excluded_names: Vec<String> = Vec::new();
            let modulo: Vec<_> = columns
                .iter()
                .zip(&origins)
                .filter(|(c, origin)| {
                    if c.is_uniform() {
                        return false;
                    }
                    let name = match origin {
                        celeste_rust::interpreter::virtual_merge::Origin::Heap(cell) => {
                            names
                                .get(cell)
                                .cloned()
                                .unwrap_or_else(|| format!("cell{}", cell))
                        }
                        other => format!("{:?}", other),
                    };
                    let matched = patterns
                        .iter()
                        .any(|p| name == *p || name.ends_with(&format!(".{}", p)));
                    if matched {
                        excluded_names.push(name);
                    } else {
                        kept_names.push(name);
                    }
                    !matched
                })
                .map(|(c, _)| c)
                .collect();
            let distinct = |cols: &[&celeste_rust::interpreter::virtual_merge::Column]| {
                let hashes =
                    celeste_rust::interpreter::virtual_merge::hash_rows(cols, state.vector_size);
                hashes.iter().collect::<std::collections::HashSet<_>>().len()
            };
            println!(
                "distinct-modulo [{}]: {} lanes, {} full cols -> {} kept, {} distinct full, {} distinct modulo ({:.1}x multiplicity)",
                patterns.join(","),
                state.vector_size,
                full.len(),
                modulo.len(),
                distinct(&full),
                distinct(&modulo),
                state.vector_size as f64 / distinct(&modulo).max(1) as f64,
            );
            println!(
                "  excluded: [{}]  kept: [{}]",
                excluded_names.join(", "),
                kept_names.join(", ")
            );
        }
    }
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

        // The merge machinery's actual data volume, so its span times can be
        // read as throughput. `dedup elements` is rows x vector columns - the
        // row-key material actually hashed; `heap cells` counts every cell of
        // every merged state that the concatenation cloned.
        {
            let m = celeste_rust::merge_stats::snapshot();
            let span_s = |name: &str| -> f64 {
                rows.iter()
                    .find(|r| r.name == name)
                    .map(|r| r.self_us as f64 / 1e6)
                    .unwrap_or(0.0)
            };
            let dedup_s = span_s("dedup_state");
            let merge_self_s = span_s("merge_groups");
            println!();
            println!(
                "merge data volume: {} vectorize calls, {} states in -> {} out ({} groups)",
                m.vectorize_calls, m.states_in, m.states_out, m.groups
            );
            println!(
                "  concatenate: {} states x ~{} heap cells = {:.1}M cell clones ({:.2}s merge_groups self, {:.0} ns/cell)",
                m.concat_states,
                m.concat_cells.checked_div(m.concat_states).unwrap_or(0),
                m.concat_cells as f64 / 1e6,
                merge_self_s,
                merge_self_s * 1e9 / m.concat_cells.max(1) as f64
            );
            println!(
                "  dedup: {} calls, {:.2}M rows x {:.1} row-weighted vector columns = {:.1}M elements, {:.2}M rows removed ({:.0}%)",
                m.dedup_calls,
                m.dedup_rows as f64 / 1e6,
                m.dedup_elems as f64 / m.dedup_rows.max(1) as f64,
                m.dedup_elems as f64 / 1e6,
                m.dedup_rows_removed as f64 / 1e6,
                100.0 * m.dedup_rows_removed as f64 / m.dedup_rows.max(1) as f64
            );
            println!(
                "  dedup throughput: {:.2}s dedup_state self = {:.1} ns/element ({:.1} ns/row)",
                dedup_s,
                dedup_s * 1e9 / m.dedup_elems.max(1) as f64,
                dedup_s * 1e9 / m.dedup_rows.max(1) as f64
            );
        }

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

        // Where the program-under-test's own time goes, resolved to blocks
        // and individual instructions. Timer overhead inflates everything
        // roughly uniformly; read the shares, not the absolute seconds. A
        // `call`'s time includes its whole callee.
        {
            // (function, local id) -> (block label, instruction text). The
            // frame driver executes as "__main" and is not one of the
            // program's functions, so it is indexed separately.
            let mut locate: std::collections::HashMap<(String, usize), (String, String)> =
                std::collections::HashMap::new();
            {
                let mut index = |name: &str, cfg: &celeste_rust::ir::Cfg| {
                    let mut visit = |label: &str, block: &celeste_rust::ir::Block| {
                        for (id, instr) in &block.instructions {
                            locate.insert(
                                (name.to_string(), usize::from(*id)),
                                (
                                    label.to_string(),
                                    celeste_rust::rewrite::print::format_instruction(instr),
                                ),
                            );
                        }
                    };
                    visit("__entry", &cfg.entry);
                    for (label, block) in &cfg.named {
                        visit(label.as_str(), block);
                    }
                };
                for (name, fun) in &program.functions {
                    index(name.as_str(), &fun.cfg);
                }
                index("__main", program.frame_cfg());
            }

            let rows = celeste_rust::instr_time::report();
            let total_us: u128 = rows.iter().map(|(_, _, d, _)| d.as_micros()).sum();

            let mut by_block: std::collections::HashMap<(String, String), (u128, u64)> =
                std::collections::HashMap::new();
            let mut by_kind: std::collections::HashMap<String, u128> =
                std::collections::HashMap::new();
            for (function, id, duration, samples) in &rows {
                let (block, text) = locate
                    .get(&(function.clone(), *id))
                    .cloned()
                    .unwrap_or_else(|| ("?".to_string(), "?".to_string()));
                let entry = by_block.entry((function.clone(), block)).or_default();
                entry.0 += duration.as_micros();
                entry.1 += samples;
                let kind = text
                    .split([' ', '('])
                    .find(|w| !w.starts_with('%') && !w.is_empty() && *w != "=")
                    .unwrap_or("?")
                    .to_string();
                *by_kind.entry(kind).or_default() += duration.as_micros();
            }

            println!();
            println!(
                "program time under test: {:.2}s measured across {} distinct instructions",
                total_us as f64 / 1e6,
                rows.len()
            );

            let mut kind_rows: Vec<(String, u128)> = by_kind.into_iter().collect();
            kind_rows.sort_by_key(|(k, us)| (std::cmp::Reverse(*us), k.clone()));
            println!("{:<24} {:>9} {:>7}", "by instruction kind", "time (s)", "%");
            for (kind, us) in kind_rows.iter().take(12) {
                println!(
                    "{:<24} {:>9.3} {:>6.1}%",
                    kind,
                    *us as f64 / 1e6,
                    100.0 * *us as f64 / total_us.max(1) as f64
                );
            }

            let mut block_rows: Vec<((String, String), (u128, u64))> =
                by_block.into_iter().collect();
            block_rows.sort_by_key(|(k, (us, _))| (std::cmp::Reverse(*us), k.clone()));
            println!();
            println!(
                "{:<58} {:>9} {:>7} {:>10}",
                "hottest blocks (function::block)", "time (s)", "%", "samples"
            );
            for ((function, block), (us, samples)) in block_rows.iter().take(15) {
                println!(
                    "{:<58} {:>9.3} {:>6.1}% {:>10}",
                    format!("{}::{}", function, block),
                    *us as f64 / 1e6,
                    100.0 * *us as f64 / total_us.max(1) as f64,
                    samples
                );
            }

            println!();
            println!(
                "{:<7} {:>9} {:>7} {:>9}  {}",
                "id", "time (s)", "%", "samples", "hottest instructions"
            );
            for (function, id, duration, samples) in rows.iter().take(30) {
                let (block, text) = locate
                    .get(&(function.clone(), *id))
                    .cloned()
                    .unwrap_or_else(|| ("?".to_string(), "?".to_string()));
                println!(
                    "%{:<6} {:>9.3} {:>6.1}% {:>9}  {}::{}: {}",
                    id,
                    duration.as_secs_f64(),
                    100.0 * duration.as_micros() as f64 / total_us.max(1) as f64,
                    samples,
                    function,
                    block,
                    text
                );
            }

            // Output-cardinality census (CELESTE_INSTR_CARD=1): how much of
            // each instruction's time went into lanes whose value already
            // existed elsewhere in the same output vector. `ideal` is
            // time x distinct/lanes - what the instruction would cost if it
            // computed each distinct result once; `waste` is the rest.
            let card_rows = celeste_rust::instr_time::cardinality_report();
            if !card_rows.is_empty() {
                let time_of: std::collections::HashMap<(String, usize), std::time::Duration> = rows
                    .iter()
                    .map(|(f, id, d, _)| ((f.clone(), *id), *d))
                    .collect();
                struct WasteRow {
                    function: String,
                    id: usize,
                    time: std::time::Duration,
                    waste_us: f64,
                    stat: celeste_rust::instr_time::CardStat,
                    is_call: bool,
                    text: String,
                    block: String,
                }
                let mut waste_rows: Vec<WasteRow> = Vec::new();
                for (function, id, stat) in &card_rows {
                    let Some(&time) = time_of.get(&(function.clone(), *id)) else {
                        continue;
                    };
                    let (block, text) = locate
                        .get(&(function.clone(), *id))
                        .cloned()
                        .unwrap_or_else(|| ("?".to_string(), "?".to_string()));
                    let density = stat.distinct as f64 / stat.lanes.max(1) as f64;
                    waste_rows.push(WasteRow {
                        function: function.clone(),
                        id: *id,
                        time,
                        waste_us: time.as_micros() as f64 * (1.0 - density),
                        stat: *stat,
                        is_call: text.starts_with("call") || text.contains("= call"),
                        text,
                        block,
                    });
                }
                // Calls nest their callee's instructions, which are counted
                // themselves - excluding calls keeps the totals flat.
                let flat: Vec<&WasteRow> = waste_rows.iter().filter(|r| !r.is_call).collect();
                let flat_time_us: f64 = flat.iter().map(|r| r.time.as_micros() as f64).sum();
                let flat_waste_us: f64 = flat.iter().map(|r| r.waste_us).sum();
                println!();
                println!(
                    "output-cardinality census (non-call): {:.2}s measured, {:.2}s ({:.0}%) spent on lanes duplicating a value already in the same vector",
                    flat_time_us / 1e6,
                    flat_waste_us / 1e6,
                    100.0 * flat_waste_us / flat_time_us.max(1.0)
                );
                let mut ranked: Vec<&WasteRow> = flat.clone();
                ranked.sort_by(|a, b| b.waste_us.total_cmp(&a.waste_us));
                println!(
                    "{:<7} {:>9} {:>9} {:>10} {:>9} {:>8}  {}",
                    "id", "waste(s)", "time (s)", "lanes/ex", "dist/ex", "maxdist", "instruction"
                );
                for r in ranked.iter().take(30) {
                    println!(
                        "%{:<6} {:>9.3} {:>9.3} {:>10.0} {:>9.1} {:>8}  {}::{}: {}",
                        r.id,
                        r.waste_us / 1e6,
                        r.time.as_secs_f64(),
                        r.stat.lanes as f64 / r.stat.execs.max(1) as f64,
                        r.stat.distinct as f64 / r.stat.execs.max(1) as f64,
                        r.stat.max_distinct,
                        r.function,
                        r.block,
                        r.text
                    );
                }
            }
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

/// Checkpoint dir for a refinement level under the base dir, for the
/// configured start room: level 0 at `<base>/<stem>`, level k at
/// `<base>/<stem>-k<k>`, where the stem is `game_runner::room_dir_stem()`
/// ("room1" for the default room, "room<x><y>" otherwise).
fn level_checkpoint_dir(base_dir: &str, level: u8) -> std::path::PathBuf {
    let stem = celeste_rust::game_runner::room_dir_stem();
    if level == 0 {
        std::path::PathBuf::from(base_dir).join(stem)
    } else {
        std::path::PathBuf::from(base_dir).join(format!("{}-k{}", stem, level))
    }
}

/// A refinement level's persisted result, loaded with the level's own
/// fingerprint (each level's precision is part of its fingerprint).
struct LoadedLevel {
    k: u8,
    precision: celeste_rust::interpreter::abstraction::RemPrecision,
    /// Latest checkpointed frame the table runs through.
    frame: u32,
    table: celeste_rust::interpreter::row_table::RowTable,
    /// Min frames to the exit per row id (`sweep::save_g`); empty when
    /// g.bin is absent and `g_required` was false - probes then check
    /// table membership and e only.
    g: Vec<u16>,
}

/// The rem precision of ladder level `k`: 16 means exact.
fn precision_for_level(k: u8) -> celeste_rust::interpreter::abstraction::RemPrecision {
    use celeste_rust::interpreter::abstraction::RemPrecision;
    if k >= 16 { RemPrecision::Exact } else { RemPrecision::Bits(k) }
}

/// Load level `k`'s row table (and g array) from under `base_dir`.
fn load_level(
    recipe_path: &str,
    base_dir: &str,
    k: u8,
    g_required: bool,
) -> Result<LoadedLevel> {
    use celeste_rust::rewrite::{checkpoint, sweep};
    let precision = precision_for_level(k);
    let dir = level_checkpoint_dir(base_dir, k);
    let recipe_text = std::fs::read_to_string(recipe_path).unwrap_or_default();
    let fp = checkpoint::config_fingerprint_with_precision(&recipe_text, precision);
    let frame = checkpoint::latest(&dir)?
        .ok_or_else(|| anyhow!("level {}: no checkpoint in {}", k, dir.display()))?;
    let ck = checkpoint::load(&dir, frame, &fp)?;
    let g = match sweep::load_g(&dir) {
        Ok(g) => {
            if g.len() != ck.visited.len() {
                return Err(anyhow!("level {}: g/table size mismatch", k));
            }
            g
        }
        Err(e) if g_required => return Err(e),
        Err(_) => Vec::new(),
    };
    Ok(LoadedLevel { k, precision, frame, table: ck.visited, g })
}

/// The row key of an already-canonical concrete state at `precision`:
/// widen, apply the conservative boundary widenings, gc (the row-hash
/// canonicalizer) and hash. A concrete rem is a point value, so widening
/// lands in exactly one bucket - no straddle split needed.
fn widened_row_key(
    canon: celeste_rust::interpreter::state::State,
    precision: celeste_rust::interpreter::abstraction::RemPrecision,
) -> Result<(u64, u64)> {
    use celeste_rust::interpreter::abstraction::{
        apply_conservative_widenings, make_state_abstract_rem,
    };
    let mut a = make_state_abstract_rem(canon, precision);
    a = apply_conservative_widenings(a);
    a.gc();
    Ok(celeste_rust::rewrite::sweep::row_keys(&a)?[0])
}

/// Parse a TAS file: comment lines start with '#', the rest is
/// comma-separated input bytes (bit i of a byte = PICO-8 button i).
fn parse_tas(path: &str) -> Result<Vec<u8>> {
    let text = std::fs::read_to_string(path)?;
    text.lines()
        .filter(|l| !l.trim().is_empty() && !l.trim_start().starts_with('#'))
        .flat_map(|l| l.split(','))
        .filter(|t| !t.trim().is_empty())
        .map(|t| t.trim().parse::<u8>().map_err(|e| anyhow!("bad input byte: {}", e)))
        .collect()
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
            let baseline = Program::compile_executable_from_disk()?;
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
                "pin-builtin" => {
                    let candidates =
                        celeste_rust::rewrite::rules::pin_builtin::candidates(&program);
                    for (i, (function, at, name)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "pin_builtin",
                                "fn": function,
                                "at": format!("%{}", usize::from(*at)),
                                "name": name,
                            })
                        );
                    }
                    eprintln!(
                        "# {} call(s) to a pure builtin blocking an if_convert triangle.",
                        candidates.len()
                    );
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
                "convert-ternary" => {
                    let candidates =
                        celeste_rust::rewrite::rules::convert_ternary::candidates(&program);
                    for (i, (function, join)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "convert_ternary",
                                "fn": function,
                                "join": join.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} and/or pair(s) whose and-arm value is statically truthy.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Apply only where the pair actually splits - see bench --profile."
                    );
                }
                "decompose-truthy" => {
                    let candidates =
                        celeste_rust::rewrite::rules::decompose_truthy::candidates(&program);
                    for (i, (function, root)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "decompose_truthy",
                                "fn": function,
                                "root": format!("%{}", usize::from(*root)),
                            })
                        );
                    }
                    eprintln!(
                        "# {} always-truthy and/or cascade(s), mixed selects and all.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Free on select-only chains; phi chains pay off once the \
                         triangle is if-converted."
                    );
                }
                "speculate" => {
                    let candidates =
                        celeste_rust::rewrite::rules::speculate::candidates(&program);
                    for (i, (function, join, arm)) in candidates.iter().enumerate() {
                        let mut entry = serde_json::json!({
                            "id": format!("{}{:03}", prefix, i),
                            "rule": "speculate",
                            "fn": function,
                            "join": join.as_str(),
                        });
                        if let Some(arm) = arm {
                            entry["arm"] = serde_json::json!(arm.as_str());
                        }
                        println!("{}", entry);
                    }
                    eprintln!(
                        "# {} arm(s) blocked by nothing but stores, hoists commuting; \
                         entries with \"arm\" are diamond arms.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Triangles: follow with sink_store per store, then if_convert. \
                         Diamonds: absorb_stores once both arms are bare."
                    );
                }
                "speculate-region" => {
                    let candidates =
                        celeste_rust::rewrite::rules::speculate_region::candidates(&program);
                    for (i, (function, head, arm)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "speculate_region",
                                "fn": function,
                                "head": head.as_str(),
                                "arm": arm.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} branch(es) skipping a pure single-entry single-exit region.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Convert splitting heads only - an eager region that never \
                         split is pure cost."
                    );
                }
                "absorb-stores" => {
                    let candidates =
                        celeste_rust::rewrite::rules::absorb_stores::candidates(&program);
                    for (i, (function, head)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "absorb_stores",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} head(s) whose arms are bare stores.",
                        candidates.len()
                    );
                }
                "sink-store" => {
                    let candidates =
                        celeste_rust::rewrite::rules::sink_store::candidates(&program);
                    for (i, (function, at)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "sink_store",
                                "fn": function,
                                "at": format!("%{}", usize::from(*at)),
                            })
                        );
                    }
                    eprintln!(
                        "# {} trailing store(s) of a triangle arm, target defined outside it.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each plants an assert_value_cell: a cell that ever holds a \
                         closure or table fails loudly. Screen at full depth."
                    );
                }
                "expand-bool" => {
                    let candidates =
                        celeste_rust::rewrite::rules::expand_bool::candidates(&program);
                    for (i, (function, head)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "expand_bool",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} bool-concretization diamond(s) - branch on an unknown \
                         bool, arms storing constants back.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each becomes lane expansion. Convert together with masking \
                         its consumers; expansion alone just moves the split. Screen \
                         at full depth."
                    );
                }
                "convert-assert" => {
                    let candidates =
                        celeste_rust::rewrite::rules::convert_assert::candidates(&program);
                    for (i, (function, head)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "convert_assert",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} inlined `__assert` failure diamond(s) - uniform \
                         never-taken branches into print + error.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each becomes a straight-line `assert_true`. Safe to apply \
                         everywhere; screen at full depth anyway."
                    );
                }
                "collapse-loop" => {
                    let candidates =
                        celeste_rust::rewrite::rules::collapse_loop::candidates(&program);
                    for (i, (function, head)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "collapse_loop",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} counted loop(s) whose shape allows the singleton \
                         collapse.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each claims `bound == init` at runtime - only sound for \
                         loops over `objects` while the room holds one object. Check \
                         the bound's provenance before applying; screen at full depth."
                    );
                }
                "collapse-break-loop" => {
                    let candidates =
                        celeste_rust::rewrite::rules::collapse_break_loop::candidates(&program);
                    for (i, (function, head)) in candidates.iter().enumerate() {
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "collapse_break_loop",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} sentinel loop(s) with an in-body `#tbl < i` break - \
                         the inlined foreach/del shape.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Each claims the break fires on iteration 2 - the singleton \
                         table premise again. Apply only where the loop executes \
                         (measure_k --blocks); screen at full depth."
                    );
                }
                "unroll-loop" => {
                    let candidates =
                        celeste_rust::rewrite::rules::unroll_loop::candidates(&program);
                    for (i, (function, head, trip_count)) in candidates.iter().enumerate() {
                        eprintln!("# {} runs {} time(s)", head.as_str(), trip_count);
                        println!(
                            "{}",
                            serde_json::json!({
                                "id": format!("{}{:03}", prefix, i),
                                "rule": "unroll_loop",
                                "fn": function,
                                "head": head.as_str(),
                            })
                        );
                    }
                    eprintln!(
                        "# {} counted loop(s) with a statically known trip count.",
                        candidates.len()
                    );
                    eprintln!(
                        "# Semantically neutral (the trip count is simulated, not \
                         assumed), but each multiplies its body's instruction count; \
                         apply only where the loop is hot and follow with \
                         merge_blocks + cse forward + dce."
                    );
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
            let baseline = Program::compile_executable_from_disk()?;
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

        Command::Classdead { frames } => {
            let (program, _) = build(&recipe)?;
            // Measure per-instruction time on the partitioned program.
            celeste_rust::instr_time::reset();
            celeste_rust::instr_time::enable();
            let mut run = celeste_rust::rewrite::verify::AbstractRun::start(&program)?;
            for _ in 1..=frames {
                run.step()?;
            }
            let rows = celeste_rust::instr_time::report();
            let total_us: f64 = rows.iter().map(|(_, _, d, _)| d.as_micros() as f64).sum();

            let analysis = celeste_rust::rewrite::class_dead::analyze(&program);
            let fwd_class = celeste_rust::rewrite::class_dead::analyze_forwarding(&program, false);
            let fwd_buttons = celeste_rust::rewrite::class_dead::analyze_forwarding(&program, true);
            let mut determined_us = 0.0f64;
            let mut at_risk_us = 0.0f64;
            let mut at_risk_count = 0usize;
            let mut determined_count = 0usize;
            for (function, id, duration, _) in &rows {
                let Some(fun) = analysis.get(function) else { continue };
                let local = celeste_rust::ir::LocalId::from(*id);
                if fun.determined.contains(&local) {
                    determined_us += duration.as_micros() as f64;
                    determined_count += 1;
                } else if fun.at_risk.contains(&local) {
                    at_risk_us += duration.as_micros() as f64;
                    at_risk_count += 1;
                }
            }
            println!(
                "class-dead analysis over {} frames ({:.2}s measured instruction time), partition cells {:?}:",
                frames,
                total_us / 1e6,
                program.merge_partition_cells,
            );
            println!(
                "  class-determined: {} instructions, {:.2}s ({:.1}%) - scalar per state today;                  zero under per-class folding",
                determined_count,
                determined_us / 1e6,
                100.0 * determined_us / total_us.max(1.0),
            );
            println!(
                "  at-risk (exclusive select-arm chains): {} instructions, {:.2}s ({:.1}%) -                  one arm's share dies per class under specialization",
                at_risk_count,
                at_risk_us / 1e6,
                100.0 * at_risk_us / total_us.max(1.0),
            );
            for (label, fwd) in [
                ("cell-forwarding, class only", &fwd_class),
                ("cell-forwarding + buttons (per input-combo variants)", &fwd_buttons),
            ] {
                let mut det_us = 0.0f64;
                let mut det_n = 0usize;
                let mut risk_us = 0.0f64;
                let mut risk_n = 0usize;
                for (function, id, duration, _) in &rows {
                    let Some(fun) = fwd.per_function.get(function) else { continue };
                    let local = celeste_rust::ir::LocalId::from(*id);
                    if fun.determined.contains(&local) {
                        det_us += duration.as_micros() as f64;
                        det_n += 1;
                    } else if fun.at_risk.contains(&local) {
                        risk_us += duration.as_micros() as f64;
                        risk_n += 1;
                    }
                }
                println!(
                    "  [{}] determined: {} instrs, {:.2}s ({:.1}%); at-risk: {} instrs, {:.2}s ({:.1}%); determined cells: {}",
                    label,
                    det_n,
                    det_us / 1e6,
                    100.0 * det_us / total_us.max(1.0),
                    risk_n,
                    risk_us / 1e6,
                    100.0 * risk_us / total_us.max(1.0),
                    fwd.determined_cells.len(),
                );
            }
        }
        Command::Widencheck { frames } => {
            use celeste_rust::interpreter::abstraction::apply_conservative_widenings;
            use celeste_rust::interpreter::vectorize::vectorize_states;
            use celeste_rust::rewrite::verify::{observe_frame, AbstractRun};
            let (program, _) = build(&recipe)?;
            let mut widened = AbstractRun::start(&program)?;
            let mut exact = AbstractRun::start_rem_only(&program)?;
            for frame in 1..=frames {
                widened.step()?;
                exact.step()?;
                let a = observe_frame(widened.states());
                let post_hoc: Vec<_> = exact
                    .states()
                    .iter()
                    .cloned()
                    .map(apply_conservative_widenings)
                    .collect();
                let b = observe_frame(&vectorize_states(post_hoc));
                if a != b {
                    println!(
                        "DIVERGED at frame {}: widen-every-boundary != post-hoc-widened exact \
                         ({} vs {} state observations). A conservative widening influenced \
                         gameplay.",
                        frame,
                        a.len(),
                        b.len()
                    );
                    std::process::exit(1);
                }
                if frame % 5 == 0 || frame == frames {
                    println!(
                        "frame {}: identical ({} lanes widened / {} lanes exact side)",
                        frame,
                        widened.lane_count(),
                        exact.lane_count()
                    );
                }
            }
            println!(
                "ok: conservative widenings certified through frame {} - widening at every \
                 boundary equals post-hoc widening of the exact sets",
                frames
            );
        }
        Command::Deoptcheck { frames } => {
            use celeste_rust::rewrite::state_mapping::StateMapping;
            use celeste_rust::rewrite::verify::{observe_frame, AbstractRun};
            let plain = Program::compile_executable_from_disk()?;
            let (program, _) = build(&recipe)?;
            let mapping = StateMapping::from_recipe(&recipe);
            println!(
                "canonical-state mapping: {} (function, capture) pair(s)",
                mapping.pair_count()
            );
            // Order matters: `set_merge_partition_patterns` is process-global,
            // so start the plain run first and the specialized run second -
            // then both merge under the specialized partition key, exactly as
            // `differential_abstract` does.
            let mut plain_run = AbstractRun::start(&plain)?;
            let mut forced =
                AbstractRun::start_with_deopt(&program, &plain, mapping, true)?;
            for frame in 1..=frames {
                plain_run.step()?;
                forced.step()?;
                let a = observe_frame(plain_run.states());
                let b = observe_frame(forced.states());
                if a != b {
                    println!(
                        "DIVERGED at frame {}: plain run != forced-deopt run ({} vs {} \
                         state observations). The canonical-state mapping or the deopt \
                         path changed meaning.",
                        frame,
                        a.len(),
                        b.len()
                    );
                    std::process::exit(1);
                }
                if frame % 5 == 0 || frame == frames {
                    println!(
                        "frame {}: identical ({} lanes, {} deopted states so far)",
                        frame,
                        forced.lane_count(),
                        forced.deopt_events().0
                    );
                }
            }
            println!(
                "ok: deopt path certified through frame {} - every state of every frame \
                 went specialized -> canonical -> plain program -> specialized without \
                 changing the reachable set",
                frames
            );
        }
        Command::ShapeCensus { checkpoint_dir, frame } => {
            use celeste_rust::interpreter::abstraction::object_shape;
            use celeste_rust::interpreter::inspect::StateHelper;
            use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
            use celeste_rust::rewrite::checkpoint;
            let dir = std::path::PathBuf::from(&checkpoint_dir);
            let states = checkpoint::load_frame_states(&dir, frame)?;
            // shape -> (states, lanes)
            let mut by_shape: std::collections::BTreeMap<String, (usize, usize)> =
                Default::default();
            // Distinct fruit `off` values across all fruit-alive lanes.
            let mut off_values: std::collections::BTreeSet<i32> = Default::default();
            let mut fruit_lanes = 0usize;
            for state in &states {
                let shape = match object_shape(state) {
                    Ok(v) => v.join(","),
                    Err(e) => format!("<unreadable: {:#}>", e),
                };
                let entry = by_shape.entry(shape.clone()).or_insert((0, 0));
                entry.0 += 1;
                entry.1 += state.vector_size;
                if shape.split(',').any(|t| t == "fruit") {
                    fruit_lanes += state.vector_size;
                    let helper = StateHelper::new(state);
                    let arr = helper
                        .find_global("objects")
                        .and_then(|id| helper.unwrap_pointer(helper.load(id)));
                    if let Some(arr_id) = arr {
                        for obj_id in helper
                            .find_objects_by_type(arr_id, "fruit")
                            .map_err(|e| anyhow!("{}", e))?
                        {
                            let HeapValue::ObjectTable(obj) = helper.load(obj_id) else {
                                continue;
                            };
                            let Some(off_cell) = obj.get("off") else { continue };
                            match helper.load(*off_cell) {
                                HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => {
                                    off_values.insert(n.as_raw_u32() as i32);
                                }
                                HeapValue::Value(Value::Number(MaybeVector::Vector(ns))) => {
                                    for n in ns.iter() {
                                        off_values.insert(n.as_raw_u32() as i32);
                                    }
                                }
                                other => println!("  fruit off is not a number: {:?}", other),
                            }
                        }
                    }
                }
            }
            let total_lanes: usize = by_shape.values().map(|(_, l)| l).sum();
            println!("frame f{:03}: {} states, {} lanes", frame, states.len(), total_lanes);
            for (shape, (st, lanes)) in &by_shape {
                println!(
                    "  [{}] {} states, {} lanes ({:.1}%)",
                    shape,
                    st,
                    lanes,
                    100.0 * *lanes as f64 / total_lanes.max(1) as f64
                );
            }
            if fruit_lanes > 0 {
                let min = off_values.iter().next().copied().unwrap_or(0);
                let max = off_values.iter().next_back().copied().unwrap_or(0);
                println!(
                    "  fruit off: {} distinct raw values across {} fruit lanes, raw range [{}, {}]",
                    off_values.len(),
                    fruit_lanes,
                    min,
                    max
                );
            }
        }

        Command::Bench {
            frames,
            baseline,
            profile,
            deopt,
            checkpoint_dir,
            checkpoint_every,
            resume,
            save_frames,
            band_dir,
            band_horizon,
            band_prev_bits,
            variants,
        } => {
            if baseline {
                bench("original", &Program::compile_executable_from_disk()?, frames, profile, None, None, None, vec![], None)?;
            }
            let (program, _) = build(&recipe)?;
            let deopt_setup = if deopt {
                Some((
                    Program::compile_executable_from_disk()?,
                    celeste_rust::rewrite::state_mapping::StateMapping::from_recipe(&recipe),
                ))
            } else {
                None
            };
            let built_variants = build_variants(&variants, &program)?;
            let variant_base_mapping = if built_variants.is_empty() {
                None
            } else {
                Some(celeste_rust::rewrite::state_mapping::StateMapping::from_recipe(&recipe))
            };
            let checkpoint_cfg = match checkpoint_dir {
                Some(dir) => {
                    let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
                    Some(CheckpointCfg {
                        dir: std::path::PathBuf::from(dir),
                        every: checkpoint_every,
                        resume,
                        save_frames,
                        fingerprint: celeste_rust::rewrite::checkpoint::config_fingerprint(
                            &recipe_text,
                        ),
                    })
                }
                None if resume => {
                    return Err(anyhow!("--resume requires --checkpoint-dir"))
                }
                None => None,
            };
            let band = match (band_dir, band_horizon, band_prev_bits) {
                (Some(dir), Some(horizon), Some(prev_bits)) => {
                    use celeste_rust::interpreter::abstraction::RemPrecision;
                    use celeste_rust::rewrite::checkpoint;
                    use celeste_rust::rewrite::sweep;
                    let prev_precision = if prev_bits >= 16 {
                        RemPrecision::Exact
                    } else {
                        RemPrecision::Bits(prev_bits)
                    };
                    let dir = std::path::PathBuf::from(dir);
                    let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
                    let prev_fp = checkpoint::config_fingerprint_with_precision(
                        &recipe_text,
                        prev_precision,
                    );
                    let prev_frame = checkpoint::latest(&dir)?
                        .ok_or_else(|| anyhow!("--band-dir has no checkpoint"))?;
                    if prev_frame < horizon {
                        return Err(anyhow!(
                            "--band-dir's latest checkpoint f{:03} is before the horizon {}",
                            prev_frame,
                            horizon
                        ));
                    }
                    let ck = checkpoint::load(&dir, prev_frame, &prev_fp)?;
                    let g_prev = sweep::load_g(&dir)?;
                    if g_prev.len() != ck.visited.len() {
                        return Err(anyhow!(
                            "g.bin has {} rows but the row table has {}",
                            g_prev.len(),
                            ck.visited.len()
                        ));
                    }
                    println!(
                        "band: previous level {:?}, {} rows, horizon {}",
                        prev_precision,
                        ck.visited.len(),
                        horizon
                    );
                    Some(celeste_rust::rewrite::verify::BandFilter {
                        prev_table: ck.visited,
                        g_prev,
                        horizon,
                        prev_precision,
                    })
                }
                (None, None, None) => None,
                _ => {
                    return Err(anyhow!(
                        "--band-dir, --band-horizon and --band-prev-bits go together"
                    ))
                }
            };
            bench(
                "rewritten",
                &program,
                frames,
                profile,
                deopt_setup.as_ref().map(|(p, m)| (p, m.clone())),
                checkpoint_cfg,
                band,
                built_variants,
                variant_base_mapping,
            )?;
        }

        Command::TraceWitness { tas, horizon, levels, base_dir } => {
            use celeste_rust::rewrite::state_mapping::StateMapping;
            use celeste_rust::rewrite::sweep;

            let inputs = parse_tas(&tas)?;
            println!("witness: {} input bytes, horizon {}", inputs.len(), horizon);

            // Load every requested level's table and g array (g.bin is
            // optional: without it the probe checks table membership and e
            // only).
            let mut level_data: Vec<LoadedLevel> = Vec::new();
            for part in levels.split(',') {
                let k: u8 = part.trim().parse().map_err(|e| anyhow!("bad level: {}", e))?;
                let level = load_level(&cli.recipe, &base_dir, k, false)?;
                println!(
                    "level {}: {} rows (through f{:03})",
                    k,
                    level.table.len(),
                    level.frame
                );
                level_data.push(level);
            }
            let plain = Program::compile_executable_from_disk()?;
            let mapping = StateMapping::from_recipe(&recipe);
            let fixed_env = plain.fixed_env();
            let mut state = celeste_rust::concrete::initial_state(&plain, &fixed_env)?;

            let frame_cfg = celeste_rust::interpreter::fixed_env::PreparedCfg::new(
                plain.frame_cfg().clone(),
            );
            let mut first_fail: Option<(u32, u8, String)> = None;
            for frame in 1..=horizon {
                let byte = inputs.get(frame as usize - 1).copied().unwrap_or(0);
                state = celeste_rust::concrete::step_frame(&frame_cfg, state, &fixed_env, byte)?;

                // Canonicalize per level and probe.
                let mut canon = state.clone();
                mapping.from_canonical(&mut canon)?;
                let pos = celeste_rust::interpreter::abstraction::player_xy_per_lane(&state)
                    .and_then(|v| v.first().copied());
                let mut line = match pos {
                    Some((x, y)) => format!("f{:03} ({:>3},{:>3}):", frame, x, y),
                    None => format!("f{:03} (no player):", frame),
                };
                for level in &level_data {
                    let key = widened_row_key(canon.clone(), level.precision)?;
                    let status = match level.table.id_of(key) {
                        None => "MISS".to_string(),
                        Some(id) => {
                            let e = level.table.earliest_frame(id).unwrap_or(u32::MAX);
                            let e_ok = e <= frame;
                            if level.g.is_empty() {
                                if e_ok {
                                    format!("ok(e={})", e)
                                } else {
                                    format!("BAD(e={})", e)
                                }
                            } else {
                                let g = level.g[id as usize];
                                let g_ok = g != sweep::G_UNREACHABLE
                                    && (g as u32) <= horizon - frame;
                                if e_ok && g_ok {
                                    format!("ok(e={},g={})", e, g)
                                } else {
                                    format!("BAD(e={},g={})", e, g)
                                }
                            }
                        }
                    };
                    if status.contains("MISS") || status.contains("BAD") {
                        if first_fail.is_none() {
                            first_fail = Some((frame, level.k, status.clone()));
                        }
                    }
                    line.push_str(&format!("  k{}:{}", level.k, status));
                }
                let interesting = line.contains("MISS") || line.contains("BAD");
                if interesting || frame % 10 == 0 || frame >= horizon - 2 {
                    println!("{}", line);
                }
            }
            match first_fail {
                Some((frame, k, status)) => println!(
                    "FIRST FAILURE: frame {}, level {}: {} - the true path leaves \
                     this level's band there",
                    frame, k, status
                ),
                None => println!(
                    "witness trace PASSES every probed level's band at every frame"
                ),
            }
        }
        Command::ExtractTas { horizon, level, base_dir, tas } => {
            use celeste_rust::interpreter::fixed_env::PreparedCfg;
            use celeste_rust::interpreter::glue::interpret_prepared_cfg;
            use celeste_rust::interpreter::abstraction::count_room_x_lanes;
            use celeste_rust::interpreter::state::State;
            use celeste_rust::rewrite::state_mapping::StateMapping;
            use celeste_rust::rewrite::sweep;

            let reference: Option<Vec<u8>> = match &tas {
                Some(path) => Some(parse_tas(path)?),
                None => None,
            };
            let loaded = load_level(&cli.recipe, &base_dir, level, true)?;
            let (precision, table, g) = (loaded.precision, loaded.table, loaded.g);
            println!(
                "level {}: {} rows (through f{:03}), walking horizon {}",
                level,
                table.len(),
                loaded.frame,
                horizon
            );

            let plain = Program::compile_executable_from_disk()?;
            let mapping = StateMapping::from_recipe(&recipe);
            let fixed_env = plain.fixed_env();
            let mut state = celeste_rust::concrete::initial_state(&plain, &fixed_env)?;
            let frame_cfg = PreparedCfg::new(plain.frame_cfg().clone());

            // Probe one concrete state's row in the level's band. A concrete
            // rem is a point value, so widening lands in exactly one bucket -
            // no straddle split needed here.
            let probe = |s: &State| -> Result<Option<(u32, u16)>> {
                let mut canon = s.clone();
                mapping.from_canonical(&mut canon)?;
                let key = widened_row_key(canon, precision)?;
                Ok(table.id_of(key).map(|id| {
                    (table.earliest_frame(id).unwrap_or(u32::MAX), g[id as usize])
                }))
            };

            let mut extracted: Vec<u8> = Vec::new();
            let mut ref_missing_frames: Vec<u32> = Vec::new();
            for frame in 1..=horizon {
                let budget = horizon - frame;
                // Try every input byte (bits 0-5 = the six PICO-8 buttons);
                // keep those whose successor stays in the band.
                let mut candidates: Vec<(u8, State)> = Vec::new();
                for byte in 0u8..64 {
                    let mut s = state.clone();
                    celeste_rust::concrete::set_concrete_buttons(&mut s, byte)?;
                    let result = interpret_prepared_cfg(&frame_cfg, s, &fixed_env)?;
                    if result.len() != 1 {
                        return Err(anyhow!(
                            "frame {}: {} states (branching!)",
                            frame,
                            result.len()
                        ));
                    }
                    let s = result.into_iter().next().unwrap().0;
                    if let Some((e, gv)) = probe(&s)? {
                        if e <= frame
                            && gv != sweep::G_UNREACHABLE
                            && (gv as u32) <= budget
                        {
                            candidates.push((byte, s));
                        }
                    }
                }
                if candidates.is_empty() {
                    return Err(anyhow!(
                        "frame {}: no input keeps the walk inside the band",
                        frame
                    ));
                }
                let ref_byte =
                    reference.as_ref().and_then(|r| r.get(frame as usize - 1).copied());
                let ref_ok =
                    ref_byte.is_some_and(|b| candidates.iter().any(|(c, _)| *c == b));
                if ref_byte.is_some() && !ref_ok {
                    ref_missing_frames.push(frame);
                }
                let chosen = if ref_ok { ref_byte.unwrap() } else { candidates[0].0 };
                let n = candidates.len();
                state = candidates
                    .into_iter()
                    .find(|(b, _)| *b == chosen)
                    .unwrap()
                    .1;
                extracted.push(chosen);
                println!(
                    "f{:03}: byte {:2}  ({:2} optimal input bytes{})",
                    frame,
                    chosen,
                    n,
                    if ref_byte.is_some() && !ref_ok { ", reference byte NOT optimal" } else { "" }
                );
            }

            // The walk must have ended on a winning row: g = 0 and the
            // player concretely in the next room.
            let win_x = celeste_rust::game_runner::win_room_x();
            let final_probe = probe(&state)?;
            let mut canon = state.clone();
            mapping.from_canonical(&mut canon)?;
            let concrete_win = count_room_x_lanes(&canon, win_x) == 1;
            println!(
                "final row: {:?} (want g=0), concrete room.x=={}: {}",
                final_probe, win_x, concrete_win
            );
            if final_probe.map(|(_, g)| g) != Some(0) || !concrete_win {
                return Err(anyhow!("walk did not end on a winning state"));
            }
            println!(
                "extracted TAS ({} frames): {}",
                extracted.len(),
                extracted
                    .iter()
                    .map(|b| b.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            );
            if let Some(r) = &reference {
                let identical = r.len() == extracted.len()
                    && r.iter().zip(&extracted).all(|(a, b)| a == b);
                if identical {
                    println!("extracted TAS is byte-identical to the reference");
                } else if ref_missing_frames.is_empty() {
                    println!(
                        "reference byte was in the optimal set at every frame \
                         (differences only past the reference's length)"
                    );
                } else {
                    println!(
                        "reference byte fell out of the optimal set at frames {:?}",
                        ref_missing_frames
                    );
                }
            }
        }
        Command::CountOptimal { horizon, level, base_dir } => {
            use celeste_rust::interpreter::fixed_env::PreparedCfg;
            use celeste_rust::interpreter::glue::interpret_prepared_cfg;
            use celeste_rust::interpreter::abstraction::{
                count_room_x_lanes, player_xy_per_lane,
            };
            use celeste_rust::interpreter::state::State;
            use celeste_rust::rewrite::state_mapping::StateMapping;
            use std::collections::HashMap;

            let loaded = load_level(&cli.recipe, &base_dir, level, true)?;
            let (precision, table, g) = (loaded.precision, loaded.table, loaded.g);
            println!("level {}: {} rows, enumerating horizon {}", level, table.len(), horizon);

            let plain = Program::compile_executable_from_disk()?;
            let mapping = StateMapping::from_recipe(&recipe);
            let fixed_env = plain.fixed_env();
            let spawn = celeste_rust::concrete::initial_state(&plain, &fixed_env)?;
            let frame_cfg = PreparedCfg::new(plain.frame_cfg().clone());

            type Key = (u64, u64);
            let probe = |s: &State| -> Result<Option<(Key, u32, u16)>> {
                let mut canon = s.clone();
                mapping.from_canonical(&mut canon)?;
                let key = widened_row_key(canon, precision)?;
                Ok(table.id_of(key).map(|id| {
                    (key, table.earliest_frame(id).unwrap_or(u32::MAX), g[id as usize])
                }))
            };
            let position = |s: &State| -> (i16, i16) {
                player_xy_per_lane(s)
                    .and_then(|v| v.first().copied())
                    .unwrap_or((i16::MIN, i16::MIN))
            };

            // Layer 0: the spawn state. Per node: concrete state, count of
            // distinct row trajectories reaching it (exact), and count of
            // distinct input-byte sequences reaching it (f64 - the totals
            // overflow u128 long before precision matters).
            struct Node {
                state: State,
                traj: u128,
                byte_seqs: f64,
            }
            let mut layer: HashMap<Key, Node> = HashMap::new();
            let spawn_key = {
                let p = probe(&spawn)?;
                p.map(|(k, _, _)| k).unwrap_or((0, 0))
            };
            layer.insert(spawn_key, Node { state: spawn, traj: 1, byte_seqs: 1.0 });
            // Position-sequence classes: distinct position histories collapse
            // when they lead to the same set of current rows, so track
            // (sorted row-key set) -> number of distinct position sequences.
            let mut pos_classes: HashMap<Vec<Key>, u128> = HashMap::new();
            pos_classes.insert(vec![spawn_key], 1);

            let mut total_edges = 0usize;
            for frame in 1..=horizon {
                let budget = (horizon - frame) as u16;
                // Expand every node with every byte; dedup successors by row.
                let mut next: HashMap<Key, Node> = HashMap::new();
                // src key -> distinct successors as (dst key, position).
                let mut succs: HashMap<Key, Vec<(Key, (i16, i16))>> = HashMap::new();
                // (src, dst) -> byte multiplicity.
                let mut edge_bytes: HashMap<(Key, Key), u32> = HashMap::new();
                for (src_key, node) in &layer {
                    for byte in 0u8..64 {
                        let mut s = node.state.clone();
                        celeste_rust::concrete::set_concrete_buttons(&mut s, byte)?;
                        let result = interpret_prepared_cfg(&frame_cfg, s, &fixed_env)?;
                        if result.len() != 1 {
                            return Err(anyhow!("frame {}: branching", frame));
                        }
                        let s = result.into_iter().next().unwrap().0;
                        let Some((key, e, gv)) = probe(&s)? else { continue };
                        // Tight band: winning exactly at the horizon needs
                        // g == budget (g < budget would beat the optimum).
                        if e > frame || gv != budget {
                            continue;
                        }
                        let entry = edge_bytes.entry((*src_key, key)).or_insert(0);
                        if *entry == 0 {
                            succs
                                .entry(*src_key)
                                .or_default()
                                .push((key, position(&s)));
                            next.entry(key).or_insert_with(|| Node {
                                state: s,
                                traj: 0,
                                byte_seqs: 0.0,
                            });
                        }
                        *entry += 1;
                    }
                }
                if next.is_empty() {
                    return Err(anyhow!("frame {}: band walk died", frame));
                }
                total_edges += edge_bytes.len();
                for ((src, dst), mult) in &edge_bytes {
                    let (traj, byte_seqs) = {
                        let s = &layer[src];
                        (s.traj, s.byte_seqs)
                    };
                    let d = next.get_mut(dst).unwrap();
                    d.traj += traj;
                    d.byte_seqs += byte_seqs * *mult as f64;
                }
                // Advance the position-sequence classes.
                let mut next_classes: HashMap<Vec<Key>, u128> = HashMap::new();
                for (rows, mult) in &pos_classes {
                    let mut by_pos: HashMap<(i16, i16), Vec<Key>> = HashMap::new();
                    for row in rows {
                        for (dst, pos) in succs.get(row).map(|v| v.as_slice()).unwrap_or(&[])
                        {
                            let set = by_pos.entry(*pos).or_default();
                            if !set.contains(dst) {
                                set.push(*dst);
                            }
                        }
                    }
                    for (_, mut set) in by_pos {
                        set.sort_unstable();
                        *next_classes.entry(set).or_insert(0) += mult;
                    }
                }
                let positions: std::collections::BTreeSet<(i16, i16)> = next
                    .values()
                    .map(|n| position(&n.state))
                    .collect();
                let pos_str = if positions.len() <= 4 {
                    format!(
                        "  at {}",
                        positions
                            .iter()
                            .map(|(x, y)| format!("({},{})", x, y))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                } else {
                    String::new()
                };
                println!(
                    "f{:03}: {:2} rows, {:2} positions, {} pos-seqs, {} trajs{}",
                    frame,
                    next.len(),
                    positions.len(),
                    next_classes.values().sum::<u128>(),
                    next.values().map(|n| n.traj).sum::<u128>(),
                    pos_str
                );
                layer = next;
                pos_classes = next_classes;
            }

            // Every final node must be a concrete win.
            let win_x = celeste_rust::game_runner::win_room_x();
            for node in layer.values() {
                let mut canon = node.state.clone();
                mapping.from_canonical(&mut canon)?;
                if count_room_x_lanes(&canon, win_x) != 1 {
                    return Err(anyhow!("final node is not a concrete win"));
                }
            }
            let trajs: u128 = layer.values().map(|n| n.traj).sum();
            let pos_seqs: u128 = pos_classes.values().sum();
            let byte_seqs: f64 = layer.values().map(|n| n.byte_seqs).sum();
            println!("band edges walked: {}", total_edges);
            println!("distinct optimal row trajectories:      {}", trajs);
            println!("distinct optimal (x,y) pixel sequences: {}", pos_seqs);
            println!(
                "distinct optimal input-byte sequences:  ~10^{:.1}",
                byte_seqs.log10()
            );
        }
        Command::Sweep { checkpoint_dir, frames, horizon, banded } => {
            use celeste_rust::rewrite::state_mapping::StateMapping;
            use celeste_rust::rewrite::sweep;
            let horizon = horizon.unwrap_or(frames);
            let dir = std::path::PathBuf::from(checkpoint_dir);
            let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
            let fingerprint =
                celeste_rust::rewrite::checkpoint::config_fingerprint(&recipe_text);
            let plain = Program::compile_executable_from_disk()?;
            let (program, _) = build(&recipe)?;
            let mapping = StateMapping::from_recipe(&recipe);
            let result =
                sweep::backward_sweep(&dir, frames, &fingerprint, &program, &plain, mapping, banded)?;
            sweep::save_g(&dir, &result.g)?;
            let reachable = result.g.iter().filter(|&&v| v != sweep::G_UNREACHABLE).count();
            println!(
                "sweep: {} of {} rows can reach the exit; {} edges",
                reachable,
                result.g.len(),
                result.edge_count
            );
            match result.optimal_frame {
                Some(win) => println!(
                    "abstract optimal win frame from e+g: {} (forward first-win must match)",
                    win
                ),
                None => println!("no row reaches the exit - the horizon is too short"),
            }
            // Band sizes for the horizon: the k=1 forward pass's budget.
            let ck = celeste_rust::rewrite::checkpoint::load(&dir, frames, &fingerprint)?;
            println!("band sizes for horizon {} (frame, rows):", horizon);
            for (f, size) in sweep::band_sizes(&ck.visited, &result.g, horizon) {
                if size > 0 || f % 10 == 0 {
                    println!("  f{:03}: {}", f, size);
                }
            }
            celeste_rust::metrics::dump(
                "sweep",
                Some(dir.as_path()),
                &[
                    ("frames", frames.to_string()),
                    ("horizon", horizon.to_string()),
                    ("edges", result.edge_count.to_string()),
                    ("rows", result.g.len().to_string()),
                ],
            );
        }
        Command::Bisect { frames } => {
            let baseline = Program::compile_executable_from_disk()?;
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
        // `{}` on an anyhow error shows only the outermost context, which for a
        // rule failure is just "verifying b000 (pin_builtin)" - true, and no
        // help at all. The root cause is the interesting line.
        Ok(Err(e)) => Err(first_line(
            &e.chain().last().map_or_else(|| e.to_string(), |c| c.to_string()),
        )),
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
