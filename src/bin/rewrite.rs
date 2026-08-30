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

use celeste_rust::program::recipe::Recipe;
use celeste_rust::program::Program;

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
    Bench {
        #[arg(long, default_value_t = 34)]
        frames: u32,
        /// Also run the unmodified program, for comparison.
        #[arg(long)]
        baseline: bool,
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
        /// Record the position-transition table DURING this pass and write
        /// it to <checkpoint-dir>/posgraph.bin, instead of paying a whole
        /// second pass over the room for it (`rewrite pos-graph`). The
        /// per-lane cell tag is stripped at the end of every chunk, so it
        /// never reaches a boundary row key; mid-frame grouping does change,
        /// so the gate is that the row table comes out element-wise equal.
        #[arg(long)]
        record_pos_graph: bool,
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
        /// The previous level's spd bucket width (log2 raw units;
        /// plans/spd-rung.md). Absent = the previous level had spd
        /// Exact - correct for every rem-ladder level.
        #[arg(long)]
        band_prev_spd_width: Option<u8>,
        /// Register a shape-dispatched variant: SHAPES[@PM1]=RECIPE_PATH,
        /// where SHAPES is a |-separated list of object-array shapes and
        /// each shape is a comma-separated object type-name list. Example:
        /// --variant 'player|player_spawn=rewrites.jsonl'. The optional
        /// @PM1 suffix keys dispatch on (shape, pm1 class): comma-separated
        /// NAME:VALUE pairs, e.g. 'player@freeze:0,dash_time:0=steady.jsonl'
        /// runs only player states whose named cells are per-state scalars
        /// equal to VALUE. Registry order is dispatch order: list a
        /// pm1-keyed overlay before a shape-only variant of the same shape.
        /// A state whose key matches runs its frames under that recipe's
        /// program, through the canonical mapping both ways; every other
        /// state uses the base recipe. Dispatch is semantically invisible
        /// (boundary states are identical with or without variants), so it
        /// is deliberately NOT part of the checkpoint fingerprint - the
        /// gate for registering a variant is a lane-count/observation
        /// comparison against a variant-free run.
        ///
        /// MEASURED (2026-08-16), and narrower than it first looks:
        /// "invisible" holds for the row SETS and not for the row IDS.
        /// Room (0,0) f040 under campaign settings, with and against the
        /// S2 variant, gives an identical fingerprint, row_count 387,443,
        /// all 40 per-frame watermarks, state_count, lane_count and
        /// per-frame frontier line - and `states.bin` (319,986 vs 316,684
        /// bytes) and `visited.bin` both differ, because a variant frame
        /// emits its raw lanes in a different order and multiplicity and
        /// ids are assigned in insertion order.
        ///
        /// This does NOT make checkpoints non-interchangeable: everything
        /// downstream reads ids out of the tree it was given, so resuming
        /// a variant run from a variant-free checkpoint is fine (see the
        /// VARIANTS note in ladder.sh). What it does mean is that
        /// artifacts are not BYTE-comparable across the setting, so a
        /// `parcheck.sh`-style byte gate has to hold the variant set
        /// fixed, and a `g.bin` is only meaningful against the row table
        /// it was computed from - which is one more reason to apply the
        /// same set to every stage, as ladder.sh does.
        #[arg(long = "variant")]
        variants: Vec<String>,
    },
    /// The backward pass (`search::sweep_time`): for each row, g(row) =
    /// min frames to the room exit, swept backward in time-expanded space
    /// with the position graph as a predecessor filter. Prints min(e+g)
    /// (must equal the forward first-win frame) and per-frame band sizes
    /// for the horizon; writes g.bin, and posgraph.bin if it is missing.
    Sweep {
        #[arg(long)]
        checkpoint_dir: String,
        /// The forward pass's last frame (its checkpoint must exist).
        #[arg(long)]
        frames: u32,
        /// Horizon N. `g` is only populated where e + g <= N, so this is
        /// part of the answer, not just of the statistics. Defaults to
        /// --frames.
        #[arg(long)]
        horizon: Option<u32>,
        /// The forward pass being swept was band-restricted: successors
        /// outside its row table were pruned by the band, so they are
        /// counted rather than treated as a replay divergence.
        #[arg(long)]
        banded: bool,
        /// Use the position graph in DIR - another PRECISION LEVEL of this
        /// same campaign, normally level 0 - instead of building this
        /// level its own. A coarser level's table contains a finer
        /// level's, and a superset only shrinks the candidate set, so this
        /// is sound; see `sweep_time::borrow_pos_graph` for what is
        /// checked. Saves ~20 s x 16 levels per horizon.
        #[arg(long)]
        pos_graph_from: Option<String>,
        /// Register a shape-dispatched variant, exactly as `bench
        /// --variant` spells it (SHAPES[@PM1]=RECIPE_PATH). This stage
        /// replays the forward pass, so it takes the same set - not
        /// because correctness needs it (dispatch is semantically
        /// invisible; a variant-free replay of a variant-recorded forward
        /// pass is legal) but so that a WRONG variant shows up as a
        /// disagreement between stages rather than as a campaign whose
        /// stages quietly disagree. ladder.sh passes the same
        /// `${VARIANT_ARGS[@]}` to every stage.
        #[arg(long = "variant")]
        variants: Vec<String>,
    },
    /// The whole precision ladder in ONE process - the in-binary
    /// replacement for `ladder.sh` (default path: SHARE_POSGRAPH=1, no
    /// variants). For each horizon it extends level 0 (forward + fused
    /// position graph), sweeps it, then runs banded rungs k=1..=maxk;
    /// a rung that never reaches the exit REFUTES the horizon, and a
    /// horizon every rung wins is the concrete optimum. Sets the ladder's
    /// standard env itself (frontier-only compiled forward, deopt-collect,
    /// strict kernels, 8000-lane cap) so one command runs the campaign.
    Ladder {
        /// First (and, without --to, only) horizon to test.
        #[arg(long, default_value_t = 94)]
        from: u32,
        /// Last horizon to test. Defaults to --from (a single horizon).
        #[arg(long)]
        to: Option<u32>,
        /// Deepest refinement rung to run per horizon.
        #[arg(long, default_value_t = 16)]
        maxk: u8,
        /// Base checkpoint dir; each level lands under
        /// `<dir>/<room-stem>[-k<k>]` (same layout as ladder.sh's L0/KROOT).
        #[arg(long)]
        checkpoint_dir: String,
        /// Start room "x,y" (feeds CELESTE_START_ROOM).
        #[arg(long, default_value = "1,0")]
        room: String,
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

/// A |-separated list of object-array shapes, each a comma-separated list of
/// object type names, as `--variant` and `--variant-shapes` both spell it.
fn parse_shapes(text: &str) -> Result<Vec<Vec<String>>> {
    let shapes: Vec<Vec<String>> = text
        .split('|')
        .map(|shape| {
            // `[]` is the EMPTY object array - the shape of a dead state
            // (every object destroyed, will_restart counting down). The
            // runtime side always supported `vec![]`; this is its spelling.
            if shape.trim() == "[]" {
                return Vec::new();
            }
            shape.split(',').map(|t| t.trim().to_string()).collect()
        })
        .collect();
    if shapes
        .iter()
        .any(|s: &Vec<String>| s.iter().any(|t| t.is_empty()))
    {
        return Err(anyhow!("empty shape or object type name in {:?}", text));
    }
    Ok(shapes)
}

/// Comma-separated NAME:VALUE pm1-class conditions, as `--variant-pm1` and
/// the `@PM1` suffix of `--variant` both spell them. Values are integer
/// PICO-8 numbers; anything else is a loud error, not a silent non-match.
fn parse_pm1(text: &str) -> Result<Vec<(String, celeste_rust::pico8_num::Pico8Num)>> {
    text.split(',')
        .map(|pair| {
            let (name, value) = pair
                .split_once(':')
                .ok_or_else(|| anyhow!("pm1 condition must be NAME:VALUE, got {:?}", pair))?;
            let name = name.trim();
            if name.is_empty() {
                return Err(anyhow!("empty cell name in pm1 condition {:?}", pair));
            }
            let value: i16 = value
                .trim()
                .parse()
                .with_context(|| format!("pm1 condition {:?}: integer value expected", pair))?;
            Ok((
                name.to_string(),
                celeste_rust::pico8_num::Pico8Num::from_i16(value),
            ))
        })
        .collect()
}

/// Package a built program as a variant for `shapes`. `host_program` is the
/// program that will dispatch to it: the partition patterns are
/// process-global (`vectorize::set_merge_partition_patterns`, installed by
/// whichever `AbstractRun::start` ran) and mid-frame hint merges inside a
/// variant frame use them too, so a variant that states a DIFFERENT list
/// would silently be ignored. An EMPTY list is allowed and means the same
/// thing there as here - no opinion, inherit the host's. That is what a
/// screening trial has, since `partition_merge` is the recipe's last entry
/// and candidates are applied before it.
fn make_variant(
    program: &Program,
    mapping: celeste_rust::search::state_mapping::StateMapping,
    shapes: Vec<Vec<String>>,
    pm1: Vec<(String, celeste_rust::pico8_num::Pico8Num)>,
    label: String,
    host_program: &Program,
) -> Result<celeste_rust::search::run::Variant> {
    if !program.merge_partition_cells.is_empty()
        && program.merge_partition_cells != host_program.merge_partition_cells
    {
        return Err(anyhow!(
            "variant {}: merge-partition cells {:?} differ from the host program's {:?}; \
             the partition patterns are process-global",
            label,
            program.merge_partition_cells,
            host_program.merge_partition_cells
        ));
    }
    Ok(celeste_rust::search::run::Variant {
        label,
        shapes,
        pm1,
        frame_cfg: celeste_rust::interpreter::fixed_env::PreparedCfg::new(
            program.frame_cfg().clone(),
        ),
        fixed_env: program.fixed_env(),
        mapping,
    })
}

/// Build the shape-dispatched variant registry from `--variant` specs
/// (SHAPES[@PM1]=RECIPE_PATH; see the flag's doc comment). Registry order
/// is dispatch order, so a (shape, pm1)-keyed overlay must be listed
/// before a shape-only variant of the same shape or it never runs.
fn build_variants(
    specs: &[String],
    base_program: &Program,
) -> Result<Vec<celeste_rust::search::run::Variant>> {
    use celeste_rust::search::state_mapping::StateMapping;
    let mut out = Vec::new();
    for spec in specs {
        let (key_part, path) = spec
            .split_once('=')
            .ok_or_else(|| anyhow!("--variant must be SHAPES[@PM1]=RECIPE_PATH, got {:?}", spec))?;
        let (shapes_part, pm1) = match key_part.split_once('@') {
            None => (key_part, Vec::new()),
            Some((shapes_part, pm1_part)) => (
                shapes_part,
                parse_pm1(pm1_part).with_context(|| format!("--variant {:?}", spec))?,
            ),
        };
        let shapes = parse_shapes(shapes_part).with_context(|| format!("--variant {:?}", spec))?;
        let recipe =
            Recipe::load(path).with_context(|| format!("--variant {:?}: loading recipe", spec))?;
        let program = celeste_rust::program::frozen::rewritten(path)
            .with_context(|| format!("--variant {:?}: the frozen program", spec))?;
        out.push(make_variant(
            &program,
            StateMapping::from_recipe(&recipe),
            shapes,
            pm1,
            format!("{}[{}]", path, key_part),
            base_program,
        )?);
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

/// What a forward pass concluded, for callers that orchestrate several of
/// them (the `ladder` subcommand). `first_win` is the frame the first
/// room-exit lane appeared at - `None` means the room exit was never
/// reached within the horizon, which is exactly the k-rung REFUTED test.
struct BenchOutcome {
    first_win: Option<u32>,
}

fn bench(
    label: &str,
    program: &Program,
    frames: u32,
    deopt: Option<(&Program, celeste_rust::search::state_mapping::StateMapping)>,
    checkpoint: Option<CheckpointCfg>,
    band: Option<celeste_rust::search::run::BandFilter>,
    variants: Vec<celeste_rust::search::run::Variant>,
    variant_base_mapping: Option<celeste_rust::search::state_mapping::StateMapping>,
    record_pos_graph: bool,
) -> Result<BenchOutcome> {
    use celeste_rust::search::checkpoint;
    let mut run = match deopt {
        Some((plain, mapping)) => celeste_rust::search::run::AbstractRun::start_with_deopt(
            program, plain, mapping, false,
        )?,
        None => celeste_rust::search::run::AbstractRun::start(program)?,
    };
    if !variants.is_empty() {
        let base_mapping = variant_base_mapping
            .ok_or_else(|| anyhow!("variants need the base recipe's mapping"))?;
        for v in &variants {
            println!("variant {} registered for shapes {:?}", v.label, v.shapes);
        }
        run.set_variants(base_mapping, variants);
    }
    if let Some(band) = band {
        run.set_band(band);
    }
    let mut start_frame = 1u32;
    if let Some(cfg) = checkpoint.as_ref() {
        // Wire the visited set to the dir: `.rowkeys` land there at every
        // boundary, and CELESTE_VISITED_ENGINE picks the engine.
        run.configure_visited_dir(&cfg.dir);
    }
    if let Some(cfg) = checkpoint.as_ref().filter(|c| c.resume) {
        match checkpoint::latest(&cfg.dir)? {
            Some(frame) if frame <= frames => {
                use celeste_rust::interpreter::visited::{mmap_engine_selected, Visited};
                let frontier = run.visited_table().is_some();
                let (meta, states, visited) = if frontier && mmap_engine_selected() {
                    // The mmap engine's keys are the frames/*.rowkeys
                    // files; no 25 GiB map rebuild on resume.
                    let (meta, states) = checkpoint::load_light(&cfg.dir, frame, &cfg.fingerprint)?;
                    let visited = Visited::mmap_open(&cfg.dir, meta.watermarks.clone())?;
                    (meta, states, Some(visited))
                } else {
                    let loaded = checkpoint::load(&cfg.dir, frame, &cfg.fingerprint)?;
                    let visited = frontier.then(|| Visited::map_with_dir(loaded.visited, &cfg.dir));
                    (loaded.meta, loaded.states, visited)
                };
                run.restore(
                    states,
                    visited,
                    (meta.deopt_states as usize, meta.deopt_lanes as usize),
                )?;
                start_frame = frame + 1;
                println!(
                    "resumed from checkpoint f{:03}: {} rows, {} states",
                    frame, meta.row_count, meta.state_count
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
    // After the resume, so a run that picks up at frame N keeps the
    // transitions of frames 1..N-1 instead of writing a table that claims
    // to cover them and does not.
    if record_pos_graph {
        let existing = match checkpoint.as_ref() {
            Some(cfg) => celeste_rust::search::pos_graph::PosGraph::load(&cfg.dir)?.filter(|g| {
                if g.fingerprint() == cfg.fingerprint {
                    true
                } else {
                    panic!(
                        "{}/posgraph.bin was recorded from the forward pass {}, \
                             but this configuration is {}. Delete it.",
                        cfg.dir.display(),
                        g.fingerprint(),
                        cfg.fingerprint
                    )
                }
            }),
            None => None,
        };
        match existing {
            Some(graph) => {
                if graph.frames() + 1 < start_frame {
                    return Err(anyhow!(
                        "--record-pos-graph resuming at frame {}, but the table on \
                         disk only covers frames 1..{} - the gap would never be \
                         recorded. Delete posgraph.bin and rebuild it.",
                        start_frame,
                        graph.frames()
                    ));
                }
                println!(
                    "  position graph: extending the recorded table ({} pairs, \
                     frames 1..{})",
                    graph.pairs(),
                    graph.frames()
                );
                run.record_pos_graph_from(graph);
            }
            None => {
                if start_frame > 1 {
                    return Err(anyhow!(
                        "--record-pos-graph resuming at frame {} with no table on \
                         disk: frames 1..{} would be missing from it",
                        start_frame,
                        start_frame - 1
                    ));
                }
                run.record_pos_graph();
            }
        }
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
    // A campaign is hours long and its tail frames are the expensive ones;
    // never go longer than this without a restart point.
    let checkpoint_seconds: u64 = std::env::var("CELESTE_CHECKPOINT_SECONDS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(600);
    let mut last_checkpoint = std::time::Instant::now();
    for frame in start_frame..=frames {
        let frame_start = std::time::Instant::now();
        run.step()?;
        if let Some(cfg) = checkpoint.as_ref() {
            if cfg.save_frames {
                celeste_rust::metrics::time("fwd.save_frames", || {
                    checkpoint::save_frame_states(&cfg.dir, frame, run.states())
                })?;
            }
            // Frame count is the wrong unit for this. A frame costs 2 s at
            // f55 and 100 s at f75 on room (0,0), so "every 5 frames" is a
            // 10-second interval early and a half-hour one exactly where
            // the run is most expensive to lose. Whichever comes first,
            // frames or CELESTE_CHECKPOINT_SECONDS (default 10 min).
            let overdue = last_checkpoint.elapsed().as_secs() >= checkpoint_seconds;
            if frame % cfg.every == 0 || frame == frames || overdue {
                let t = std::time::Instant::now();
                let path = checkpoint::save(
                    &cfg.dir,
                    frame,
                    &cfg.fingerprint,
                    run.states(),
                    run.visited_table(),
                    run.deopt_events(),
                )?;
                println!(
                    "  checkpoint {} written in {:.1}s{}",
                    path.display(),
                    t.elapsed().as_secs_f64(),
                    if overdue { " (time-triggered)" } else { "" }
                );
                last_checkpoint = std::time::Instant::now();
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

        let win_lanes: usize = run
            .states()
            .iter()
            .map(celeste_rust::interpreter::abstraction::count_win_lanes)
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
                format!(
                    "  WIN: {} lanes {}",
                    win_lanes,
                    celeste_rust::interpreter::abstraction::win_label()
                )
            } else {
                String::new()
            }
        );
    }
    if let Some(frame) = first_win {
        println!("first room-exit lanes appeared at frame {}", frame);
    }
    // Write the fused table before the timing line, so the reported wall
    // clock covers the whole stage this is meant to REPLACE.
    if record_pos_graph {
        let cfg = checkpoint.as_ref().ok_or_else(|| {
            anyhow!("--record-pos-graph needs --checkpoint-dir to write posgraph.bin into")
        })?;
        let pairs = run.pos_graph_pairs().unwrap_or(0);
        let graph = run
            .take_pos_graph(frames, &cfg.fingerprint)
            .ok_or_else(|| anyhow!("recording was enabled but produced no table"))?;
        graph.save(&cfg.dir)?;
        println!(
            "  position graph recorded IN the forward pass: {} pairs over {} \
             destination cells",
            pairs,
            graph.live_cells()
        );
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
    // The census report is wired into main.rs only, so the `rewrite`
    // driver - which is where every search actually runs - never emitted
    // it. This one is unconditional (two atomic adds per construction) and
    // prints only when something was counted.
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
                        celeste_rust::interpreter::virtual_merge::Origin::Heap(cell) => names
                            .get(cell)
                            .cloned()
                            .unwrap_or_else(|| format!("cell{}", cell)),
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
                hashes
                    .iter()
                    .collect::<std::collections::HashSet<_>>()
                    .len()
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

    Ok(BenchOutcome { first_win })
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

/// Build a band filter from a previous precision level's checkpoint dir -
/// the same construction the `bench` handler inlines, factored out so the
/// in-process `ladder` can call it per rung. `prev_bits` is the previous
/// level's rem precision (0 = level 0's full widening, 16 = exact).
fn build_band(
    recipe_text: &str,
    prev_dir: &std::path::Path,
    horizon: u32,
    prev_bits: u8,
) -> Result<celeste_rust::search::run::BandFilter> {
    use celeste_rust::interpreter::abstraction::{LadderPrecision, RemPrecision, SpdPrecision};
    use celeste_rust::search::{checkpoint, sweep};
    let prev_precision = LadderPrecision {
        // The rem ladder leaves spd Exact at every level (plans/spd-rung.md).
        spd: SpdPrecision::Exact,
        rem: if prev_bits >= 16 {
            RemPrecision::Exact
        } else {
            RemPrecision::Bits(prev_bits)
        },
    };
    let prev_fp = checkpoint::config_fingerprint_with_precision(recipe_text, prev_precision);
    let prev_frame = checkpoint::latest(prev_dir)?
        .ok_or_else(|| anyhow!("band dir {} has no checkpoint", prev_dir.display()))?;
    if prev_frame < horizon {
        return Err(anyhow!(
            "band dir's latest checkpoint f{:03} is before the horizon {}",
            prev_frame,
            horizon
        ));
    }
    let ck = checkpoint::load(prev_dir, prev_frame, &prev_fp)?;
    let g_prev = sweep::load_g(prev_dir)?;
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
    Ok(celeste_rust::search::run::BandFilter {
        prev_table: ck.visited,
        g_prev,
        horizon,
        prev_precision,
    })
}

/// One ladder stage's wall clock and peak RSS.
struct StageTiming {
    name: String,
    wall_s: f64,
    peak_gb: f64,
}

/// Per-stage peak RSS in a SINGLE process. `/proc/self/status`' VmHWM is a
/// process-lifetime high-water mark, so it cannot attribute a peak to one
/// stage; instead sample VmRSS on a background thread and take the max over
/// the stage. Stages are multi-second, so 150 ms sampling is plenty.
struct RssSampler {
    stop: std::sync::Arc<std::sync::atomic::AtomicBool>,
    peak_kb: std::sync::Arc<std::sync::atomic::AtomicU64>,
    handle: Option<std::thread::JoinHandle<()>>,
}

impl RssSampler {
    fn start() -> Self {
        use std::sync::atomic::Ordering::Relaxed;
        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let peak_kb = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
        let (s, p) = (stop.clone(), peak_kb.clone());
        let handle = std::thread::spawn(move || {
            while !s.load(Relaxed) {
                p.fetch_max(current_rss_kb(), Relaxed);
                std::thread::sleep(std::time::Duration::from_millis(150));
            }
            p.fetch_max(current_rss_kb(), Relaxed);
        });
        Self {
            stop,
            peak_kb,
            handle: Some(handle),
        }
    }

    fn finish(mut self) -> f64 {
        use std::sync::atomic::Ordering::Relaxed;
        self.stop.store(true, Relaxed);
        if let Some(h) = self.handle.take() {
            let _ = h.join();
        }
        self.peak_kb.load(Relaxed) as f64 / 1048576.0
    }
}

/// Run one ladder stage, timing wall clock and peak RSS around `f`. Prints a
/// Hand freed heap pages back to the OS between ladder stages (glibc keeps
/// them mapped otherwise). No-op off glibc.
fn release_arenas() {
    #[cfg(target_env = "gnu")]
    {
        extern "C" {
            fn malloc_trim(pad: usize) -> std::os::raw::c_int;
        }
        unsafe {
            malloc_trim(0);
        }
    }
}

/// `name\twall\tpeak\trc` line matching ladder.sh's `stage` for grep-parity.
fn stage<T>(
    timings: &mut Vec<StageTiming>,
    name: &str,
    f: impl FnOnce() -> Result<T>,
) -> Result<T> {
    println!("=== stage {} ===", name);
    let sampler = RssSampler::start();
    let t0 = std::time::Instant::now();
    let r = f();
    let wall = t0.elapsed().as_secs_f64();
    let peak = sampler.finish();
    // Return this stage's freed transient to the OS before the next stage
    // allocates. glibc's malloc keeps freed arenas mapped, so in ONE process
    // the level-0 forward's ~13 GB would sit under the sweep's ~28 GB peak
    // (room (0,0)'s 101 GB sweep is why ladder.sh used a separate process
    // per stage). `malloc_trim(0)` hands the free pages back; the whole
    // point of retiring ladder.sh is that the in-process ladder no longer
    // needs per-stage processes to stay under the memory cap.
    release_arenas();
    println!(
        "{}\t{:.0}\t{:.2}\t{}",
        name,
        wall,
        peak,
        i32::from(r.is_err())
    );
    timings.push(StageTiming {
        name: name.to_string(),
        wall_s: wall,
        peak_gb: peak,
    });
    r
}

/// The whole precision ladder in one process: for each horizon, extend
/// level 0 (forward with the fused position graph) and sweep it, then run
/// the banded refinement rungs k=1..=maxk. A rung that never reaches the
/// room exit REFUTES the horizon; if every rung wins, the concrete optimum
/// is that horizon. This replaces `ladder.sh`'s default (SHARE_POSGRAPH=1,
/// no variants) path with no subprocess orchestration - every stage calls
/// the same `bench` / `backward_sweep_time` the individual subcommands do.
fn run_ladder(
    recipe_path: &str,
    recipe: &Recipe,
    from: u32,
    to: u32,
    maxk: u8,
    base_dir: &str,
    room: &str,
) -> Result<()> {
    use celeste_rust::search::state_mapping::StateMapping;
    use celeste_rust::search::{checkpoint, sweep, sweep_time};

    // The env the ladder defines (matches ladder.sh): a frontier-only
    // compiled forward with deopt-collect-first, strict kernels, and the
    // 8000-lane chunk cap. START_ROOM feeds a OnceLock, so it MUST be set
    // before any state init - which is here, before the first bench().
    std::env::set_var("CELESTE_START_ROOM", room);
    std::env::set_var("CELESTE_FRONTIER_ONLY", "1");
    std::env::set_var("CELESTE_DEOPT_COLLECT_FIRST", "1");
    // The ladder runs the INTERPRETER forward by default. The plumbing for
    // a KERNEL (compiled) forward is READY - checkpoints are sweep-readable
    // (one `engine_row_keys`, gated), the compiled forward reproduces the
    // interpreter at every rung (the Bits(2) artifact was fixed by moving
    // the widening into the graph), the per-rung registry serves the right
    // set per rung, and a uniformly-compiled ladder shares one
    // `compiled_engine` fingerprint so the bands validate - but there is
    // ONE open blocker: the ASM-kernel compiled forward + `--record-pos-graph`
    // (which the sweep needs) GRINDS in `vectorize_states` /
    // `split_by_condition` at ~f28 on room (1,0). It is PRE-EXISTING (the
    // ASM cutover, not the widening work - it hangs with CELESTE_WIDEN_IN_GRAPH=0
    // CELESTE_WRAPPER_SKIP=0), and it hits ladder.sh KERNELS=1 the same way,
    // so it is not a shell-vs-binary issue. Until it is fixed,
    // CELESTE_LADDER_KERNELS=1 opts into the compiled forward (for
    // debugging the hang); the default stays on the correct interpreter.
    if std::env::var_os("CELESTE_LADDER_KERNELS").is_some_and(|v| v != "0") {
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
    }
    if std::env::var_os("CELESTE_MAX_STATE_LANES").is_none() {
        std::env::set_var("CELESTE_MAX_STATE_LANES", "8000");
    }

    // EAGER kernel assembly: build every rung's set (0..=maxk) UP FRONT -
    // shapes within a rung assemble in parallel - so no rung stalls its
    // forward pass building kernels mid-search. The registry is indexed by
    // rem precision, so cycling the precision here fills each rung's slot;
    // restored to Bits(0) for level 0's actual run. (Across-rung builds are
    // sequential because the widening reads the process precision; the
    // per-shape gcc is the parallel part.)
    if std::env::var_os("CELESTE_COMPILED_FORWARD").is_some() {
        let t0 = std::time::Instant::now();
        for k in 0..=maxk {
            celeste_rust::interpreter::abstraction::set_rem_precision(precision_for_level(k));
            celeste_rust::compiled::prewarm_kernels();
        }
        celeste_rust::interpreter::abstraction::set_rem_precision(
            celeste_rust::interpreter::abstraction::RemPrecision::Bits(0),
        );
        println!(
            "=== eager kernel assembly: rungs 0..={} in {:.1}s ===",
            maxk,
            t0.elapsed().as_secs_f64()
        );
    }

    let recipe_text = std::fs::read_to_string(recipe_path).unwrap_or_default();
    let no_variants: Vec<String> = Vec::new();
    let build_none = |p: &Program| build_variants(&no_variants, p);
    let mut timings: Vec<StageTiming> = Vec::new();
    let mut concrete_optimum: Option<u32> = None;

    'horizons: for h in from..=to {
        println!("=== horizon {}: level 0 extend + sweep ===", h);
        // Level 0 is Bits(0). `set_rem_precision` is the in-process source of
        // truth; also clear CELESTE_REM_BITS so nothing reads a stale env
        // default (and any subprocess agrees).
        std::env::remove_var("CELESTE_REM_BITS");
        celeste_rust::interpreter::abstraction::set_rem_precision(
            celeste_rust::interpreter::abstraction::RemPrecision::Bits(0),
        );
        let l0 = level_checkpoint_dir(base_dir, 0);
        let plain = Program::compile_executable_from_disk()?;
        let program = celeste_rust::program::frozen::rewritten(recipe_path)?;
        let mapping = StateMapping::from_recipe(recipe);
        let fp = checkpoint::config_fingerprint(&recipe_text);

        if l0.join(format!("f{:03}", h)).is_dir() {
            println!("level 0 already covers f{} - skipping the extend", h);
        } else {
            let ckcfg = CheckpointCfg {
                dir: l0.clone(),
                every: h,
                resume: true,
                save_frames: true,
                fingerprint: fp.clone(),
            };
            stage(&mut timings, &format!("l0-bench-h{}", h), || {
                bench(
                    "rewritten",
                    &program,
                    h,
                    Some((&plain, mapping.clone())),
                    Some(ckcfg),
                    None,
                    vec![],
                    None,
                    true,
                )
            })?;
        }

        // Level 0 backward sweep. It loads the fused posgraph.bin the forward
        // just wrote (NOT banded, no borrow).
        let l0_sweep = stage(&mut timings, &format!("l0-sweep-h{}", h), || {
            let res = sweep_time::backward_sweep_time(
                &l0,
                h,
                h,
                &fp,
                &recipe_text,
                None,
                &program,
                &plain,
                mapping.clone(),
                &build_none,
                false,
            )?;
            sweep::save_g(&l0, &res.g)?;
            Ok(res)
        })?;
        println!(
            "=== horizon {} level 0: optimal win frame {:?} ===",
            h, l0_sweep.optimal_frame
        );

        let mut refuted = false;
        for k in 1..=maxk {
            // Each rung's rem precision, read by the fingerprint and the
            // abstraction. `set_rem_precision` is the in-process source of
            // truth; CELESTE_REM_BITS is set too so any subprocess agrees.
            std::env::set_var("CELESTE_REM_BITS", k.to_string());
            celeste_rust::interpreter::abstraction::set_rem_precision(if k >= 16 {
                celeste_rust::interpreter::abstraction::RemPrecision::Exact
            } else {
                celeste_rust::interpreter::abstraction::RemPrecision::Bits(k)
            });
            let prev_dir = level_checkpoint_dir(base_dir, k - 1);
            let kdir = level_checkpoint_dir(base_dir, k);
            std::fs::remove_dir_all(&kdir).ok();
            let kfp = checkpoint::config_fingerprint(&recipe_text);
            let program_k = celeste_rust::program::frozen::rewritten(recipe_path)?;
            let band = build_band(&recipe_text, &prev_dir, h, k - 1)?;
            let kck = CheckpointCfg {
                dir: kdir.clone(),
                every: h,
                resume: false,
                save_frames: true,
                fingerprint: kfp.clone(),
            };
            let outcome = stage(&mut timings, &format!("k{}-bench-h{}", k, h), || {
                bench(
                    "rewritten",
                    &program_k,
                    h,
                    Some((&plain, mapping.clone())),
                    Some(kck),
                    Some(band),
                    vec![],
                    None,
                    false,
                )
            })?;
            if outcome.first_win.is_none() {
                println!("=== horizon {} REFUTED at k={} ===", h, k);
                refuted = true;
                break;
            }
            let ksweep = stage(&mut timings, &format!("k{}-sweep-h{}", k, h), || {
                let res = sweep_time::backward_sweep_time(
                    &kdir,
                    h,
                    h,
                    &kfp,
                    &recipe_text,
                    Some(l0.as_path()),
                    &program_k,
                    &plain,
                    mapping.clone(),
                    &build_none,
                    true,
                )?;
                sweep::save_g(&kdir, &res.g)?;
                Ok(res)
            })?;
            println!(
                "=== horizon {} k={}: optimal win frame {:?} ===",
                h, k, ksweep.optimal_frame
            );
        }
        if !refuted {
            println!(
                "=== ALL LEVELS THROUGH k={} WIN AT HORIZON {}: CONCRETE OPTIMUM = {} ===",
                maxk, h, h
            );
            concrete_optimum = Some(h);
            break 'horizons;
        }
    }

    println!("\n=== ladder stage timings (name  wall_s  peak_GB) ===");
    let mut total = 0.0;
    for t in &timings {
        println!(
            "  {:<22} {:>7.0} s  {:>7.2} GB",
            t.name, t.wall_s, t.peak_gb
        );
        total += t.wall_s;
    }
    println!("  {:<22} {:>7.0} s", "TOTAL", total);
    match concrete_optimum {
        Some(h) => println!("ladder: CONCRETE OPTIMUM = {}", h),
        None => println!(
            "ladder: no horizon in {}..={} confirmed (all refuted or range exhausted)",
            from, to
        ),
    }
    Ok(())
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
    if k >= 16 {
        RemPrecision::Exact
    } else {
        RemPrecision::Bits(k)
    }
}

/// Load level `k`'s row table (and g array) from under `base_dir`.
fn load_level(recipe_path: &str, base_dir: &str, k: u8, g_required: bool) -> Result<LoadedLevel> {
    use celeste_rust::search::{checkpoint, sweep};
    let precision = precision_for_level(k);
    let dir = level_checkpoint_dir(base_dir, k);
    let recipe_text = std::fs::read_to_string(recipe_path).unwrap_or_default();
    // The k-ladder's levels all have spd Exact; the spd rungs below
    // level 0 get their own loading path when witness tracing crosses
    // them.
    let fp = checkpoint::config_fingerprint_with_precision(
        &recipe_text,
        celeste_rust::interpreter::abstraction::LadderPrecision {
            spd: celeste_rust::interpreter::abstraction::SpdPrecision::Exact,
            rem: precision,
        },
    );
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
    Ok(LoadedLevel {
        k,
        precision,
        frame,
        table: ck.visited,
        g,
    })
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
    Ok(celeste_rust::search::sweep::row_keys(&a)?[0])
}

/// Parse a TAS file: comment lines start with '#', the rest is
/// comma-separated input bytes (bit i of a byte = PICO-8 button i).
fn parse_tas(path: &str) -> Result<Vec<u8>> {
    let text = std::fs::read_to_string(path)?;
    text.lines()
        .filter(|l| !l.trim().is_empty() && !l.trim_start().starts_with('#'))
        .flat_map(|l| l.split(','))
        .filter(|t| !t.trim().is_empty())
        .map(|t| {
            t.trim()
                .parse::<u8>()
                .map_err(|e| anyhow!("bad input byte: {}", e))
        })
        .collect()
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let recipe = Recipe::load(&cli.recipe)?;

    match cli.command {
        Command::Bench {
            frames,
            baseline,
            deopt,
            checkpoint_dir,
            checkpoint_every,
            resume,
            save_frames,
            record_pos_graph,
            band_dir,
            band_horizon,
            band_prev_bits,
            band_prev_spd_width,
            variants,
        } => {
            if baseline {
                bench(
                    "original",
                    &Program::compile_executable_from_disk()?,
                    frames,
                    None,
                    None,
                    None,
                    vec![],
                    None,
                    false,
                )?;
            }
            let program = celeste_rust::program::frozen::rewritten(&cli.recipe)?;
            let deopt_setup = if deopt {
                Some((
                    Program::compile_executable_from_disk()?,
                    celeste_rust::search::state_mapping::StateMapping::from_recipe(&recipe),
                ))
            } else {
                None
            };
            let built_variants = build_variants(&variants, &program)?;
            let variant_base_mapping = if built_variants.is_empty() {
                None
            } else {
                Some(celeste_rust::search::state_mapping::StateMapping::from_recipe(&recipe))
            };
            let checkpoint_cfg = match checkpoint_dir {
                Some(dir) => {
                    let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
                    Some(CheckpointCfg {
                        dir: std::path::PathBuf::from(dir),
                        every: checkpoint_every,
                        resume,
                        save_frames,
                        fingerprint: celeste_rust::search::checkpoint::config_fingerprint(
                            &recipe_text,
                        ),
                    })
                }
                None if resume => return Err(anyhow!("--resume requires --checkpoint-dir")),
                None => None,
            };
            let band = match (band_dir, band_horizon, band_prev_bits) {
                (Some(dir), Some(horizon), Some(prev_bits)) => {
                    use celeste_rust::interpreter::abstraction::{
                        LadderPrecision, RemPrecision, SpdPrecision,
                    };
                    use celeste_rust::search::checkpoint;
                    use celeste_rust::search::sweep;
                    let prev_precision = LadderPrecision {
                        spd: match band_prev_spd_width {
                            Some(w) => SpdPrecision::WidthLog2(w),
                            None => SpdPrecision::Exact,
                        },
                        rem: if prev_bits >= 16 {
                            RemPrecision::Exact
                        } else {
                            RemPrecision::Bits(prev_bits)
                        },
                    };
                    let dir = std::path::PathBuf::from(dir);
                    let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
                    let prev_fp =
                        checkpoint::config_fingerprint_with_precision(&recipe_text, prev_precision);
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
                    Some(celeste_rust::search::run::BandFilter {
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
                deopt_setup.as_ref().map(|(p, m)| (p, m.clone())),
                checkpoint_cfg,
                band,
                built_variants,
                variant_base_mapping,
                record_pos_graph,
            )?;
        }

        Command::TraceWitness {
            tas,
            horizon,
            levels,
            base_dir,
        } => {
            use celeste_rust::search::state_mapping::StateMapping;
            use celeste_rust::search::sweep;

            let inputs = parse_tas(&tas)?;
            println!("witness: {} input bytes, horizon {}", inputs.len(), horizon);

            // Load every requested level's table and g array (g.bin is
            // optional: without it the probe checks table membership and e
            // only).
            let mut level_data: Vec<LoadedLevel> = Vec::new();
            for part in levels.split(',') {
                let k: u8 = part
                    .trim()
                    .parse()
                    .map_err(|e| anyhow!("bad level: {}", e))?;
                let level = load_level(&cli.recipe, &base_dir, k, false)?;
                println!(
                    "level {}: {} rows (through f{:03})",
                    k,
                    level.table.len(),
                    level.frame
                );
                level_data.push(level);
            }
            let mapping = StateMapping::from_recipe(&recipe);
            let mut ce = celeste_rust::concrete::ConcreteEngine::new()?;
            let mut state = ce.initial_state()?;

            let mut first_fail: Option<(u32, u8, String)> = None;
            for frame in 1..=horizon {
                let byte = inputs.get(frame as usize - 1).copied().unwrap_or(0);
                state = ce.step_frame(state, byte)?;

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
                                let g_ok =
                                    g != sweep::G_UNREACHABLE && (g as u32) <= horizon - frame;
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
                None => println!("witness trace PASSES every probed level's band at every frame"),
            }
        }
        Command::ExtractTas {
            horizon,
            level,
            base_dir,
            tas,
        } => {
            use celeste_rust::interpreter::abstraction::count_win_lanes;
            use celeste_rust::interpreter::state::State;
            use celeste_rust::search::state_mapping::StateMapping;
            use celeste_rust::search::sweep;

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

            let mapping = StateMapping::from_recipe(&recipe);
            let mut ce = celeste_rust::concrete::ConcreteEngine::new()?;
            let mut state = ce.initial_state()?;

            // Probe one concrete state's row in the level's band. A concrete
            // rem is a point value, so widening lands in exactly one bucket -
            // no straddle split needed here.
            let probe = |s: &State| -> Result<Option<(u32, u16)>> {
                let mut canon = s.clone();
                mapping.from_canonical(&mut canon)?;
                let key = widened_row_key(canon, precision)?;
                Ok(table
                    .id_of(key)
                    .map(|id| (table.earliest_frame(id).unwrap_or(u32::MAX), g[id as usize])))
            };

            let mut extracted: Vec<u8> = Vec::new();
            let mut ref_missing_frames: Vec<u32> = Vec::new();
            for frame in 1..=horizon {
                let budget = horizon - frame;
                // Try every input byte (bits 0-5 = the six PICO-8 buttons);
                // keep those whose successor stays in the band.
                let mut candidates: Vec<(u8, State)> = Vec::new();
                for byte in 0u8..64 {
                    let s = ce.step_frame(state.clone(), byte)?;
                    if let Some((e, gv)) = probe(&s)? {
                        if e <= frame && gv != sweep::G_UNREACHABLE && (gv as u32) <= budget {
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
                let ref_byte = reference
                    .as_ref()
                    .and_then(|r| r.get(frame as usize - 1).copied());
                let ref_ok = ref_byte.is_some_and(|b| candidates.iter().any(|(c, _)| *c == b));
                if ref_byte.is_some() && !ref_ok {
                    ref_missing_frames.push(frame);
                }
                let chosen = if ref_ok {
                    ref_byte.unwrap()
                } else {
                    candidates[0].0
                };
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
                    if ref_byte.is_some() && !ref_ok {
                        ", reference byte NOT optimal"
                    } else {
                        ""
                    }
                );
            }

            // The walk must have ended on a winning row: g = 0 and the
            // player concretely in the next room.

            let final_probe = probe(&state)?;
            let mut canon = state.clone();
            mapping.from_canonical(&mut canon)?;
            let concrete_win = count_win_lanes(&canon) == 1;
            println!(
                "final row: {:?} (want g=0), concrete win {}: {}",
                final_probe,
                celeste_rust::interpreter::abstraction::win_label(),
                concrete_win
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
                let identical =
                    r.len() == extracted.len() && r.iter().zip(&extracted).all(|(a, b)| a == b);
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
        Command::CountOptimal {
            horizon,
            level,
            base_dir,
        } => {
            use celeste_rust::interpreter::abstraction::{count_win_lanes, player_xy_per_lane};
            use celeste_rust::interpreter::state::State;
            use celeste_rust::search::state_mapping::StateMapping;
            use std::collections::HashMap;

            let loaded = load_level(&cli.recipe, &base_dir, level, true)?;
            let (precision, table, g) = (loaded.precision, loaded.table, loaded.g);
            println!(
                "level {}: {} rows, enumerating horizon {}",
                level,
                table.len(),
                horizon
            );

            let mapping = StateMapping::from_recipe(&recipe);
            let mut ce = celeste_rust::concrete::ConcreteEngine::new()?;
            let spawn = ce.initial_state()?;

            type Key = (u64, u64);
            let probe = |s: &State| -> Result<Option<(Key, u32, u16)>> {
                let mut canon = s.clone();
                mapping.from_canonical(&mut canon)?;
                let key = widened_row_key(canon, precision)?;
                Ok(table.id_of(key).map(|id| {
                    (
                        key,
                        table.earliest_frame(id).unwrap_or(u32::MAX),
                        g[id as usize],
                    )
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
            layer.insert(
                spawn_key,
                Node {
                    state: spawn,
                    traj: 1,
                    byte_seqs: 1.0,
                },
            );
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
                        let s = ce.step_frame(node.state.clone(), byte)?;
                        let Some((key, e, gv)) = probe(&s)? else {
                            continue;
                        };
                        // Tight band: winning exactly at the horizon needs
                        // g == budget (g < budget would beat the optimum).
                        if e > frame || gv != budget {
                            continue;
                        }
                        let entry = edge_bytes.entry((*src_key, key)).or_insert(0);
                        if *entry == 0 {
                            succs.entry(*src_key).or_default().push((key, position(&s)));
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
                        for (dst, pos) in succs.get(row).map(|v| v.as_slice()).unwrap_or(&[]) {
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
                let positions: std::collections::BTreeSet<(i16, i16)> =
                    next.values().map(|n| position(&n.state)).collect();
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

            for node in layer.values() {
                let mut canon = node.state.clone();
                mapping.from_canonical(&mut canon)?;
                if count_win_lanes(&canon) != 1 {
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
        Command::Sweep {
            checkpoint_dir,
            frames,
            horizon,
            banded,
            pos_graph_from,
            variants,
        } => {
            use celeste_rust::search::state_mapping::StateMapping;
            use celeste_rust::search::{sweep, sweep_time};
            let horizon = horizon.unwrap_or(frames);
            let dir = std::path::PathBuf::from(checkpoint_dir);
            let from = pos_graph_from.map(std::path::PathBuf::from);
            let recipe_text = std::fs::read_to_string(&cli.recipe).unwrap_or_default();
            let fingerprint = celeste_rust::search::checkpoint::config_fingerprint(&recipe_text);
            let plain = Program::compile_executable_from_disk()?;
            let program = celeste_rust::program::frozen::rewritten(&cli.recipe)?;
            let mapping = StateMapping::from_recipe(&recipe);
            let build = |p: &Program| build_variants(&variants, p);
            let result = sweep_time::backward_sweep_time(
                &dir,
                frames,
                horizon,
                &fingerprint,
                &recipe_text,
                from.as_deref(),
                &program,
                &plain,
                mapping,
                &build,
                banded,
            )?;
            sweep::save_g(&dir, &result.g)?;
            let reachable = result
                .g
                .iter()
                .filter(|&&v| v != sweep::G_UNREACHABLE)
                .count();
            println!(
                "sweep: {} of {} rows can reach the exit within the horizon; \
                 {} expansions, {} successors outside the row table",
                reachable,
                result.g.len(),
                result.expansions,
                result.out_of_table
            );
            match result.optimal_frame {
                Some(win) => println!(
                    "abstract optimal win frame from e+g: {} (forward first-win must match)",
                    win
                ),
                None => println!("no row reaches the exit - the horizon is too short"),
            }
            // Band sizes for the horizon: the k=1 forward pass's budget.
            let ck = celeste_rust::search::checkpoint::load(&dir, frames, &fingerprint)?;
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
                    ("rows", result.g.len().to_string()),
                    ("expansions", result.expansions.to_string()),
                    ("out_of_table", result.out_of_table.to_string()),
                ],
            );
        }
        Command::Ladder {
            from,
            to,
            maxk,
            checkpoint_dir,
            room,
        } => {
            run_ladder(
                &cli.recipe,
                &recipe,
                from,
                to.unwrap_or(from),
                maxk,
                &checkpoint_dir,
                &room,
            )?;
        }
    }

    Ok(())
}
