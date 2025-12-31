//! Test binary for the symbolic tracing approach.
//!
//! Runs the game using symbolic tracing and compares with the reference implementation.

use std::time::Instant;
use anyhow::Result;
use clap::Parser;

use celeste_rust::{
    frontend,
    game_runner::{create_fixed_env_with_game_builtins, create_initial_state_with_builtins},
    interpreter::glue::interpret_cfg,
    interpreter::state::State,
    interpreter::fixed_env::FixedEnv,
    symbolic_tracing::{TraceCache, run_traced, RunStats},
};

#[derive(Parser)]
#[command(name = "trace_test")]
#[command(about = "Test symbolic tracing on Celeste")]
struct Args {
    /// Number of frames to run
    #[arg(short = 'n', long, default_value_t = 30)]
    num_frames: u32,

    /// Run reference implementation for comparison
    #[arg(long)]
    compare: bool,

    /// Run only reference (for baseline timing)
    #[arg(long)]
    reference_only: bool,

    /// Run only symbolic tracing
    #[arg(long)]
    symbolic_only: bool,
}

/// Run the reference (vectorized) implementation for one frame
fn run_reference_frame(
    frame_cfg: &celeste_rust::ir::Cfg,
    states: Vec<State>,
    fixed_env: &FixedEnv,
) -> Result<Vec<State>> {
    let mut all_results = Vec::new();
    for state in states {
        let results = interpret_cfg(frame_cfg.clone(), state, fixed_env)?;
        for (s, _) in results {
            all_results.push(s);
        }
    }
    Ok(all_results)
}

/// Count total expanded states (sum of vector_size)
fn count_expanded(states: &[State]) -> usize {
    states.iter().map(|s| s.vector_size).sum()
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!("=== Symbolic Tracing Test ===\n");

    // Load and compile the game
    let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")?;
    let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = std::fs::read_to_string("lua/celeste-minimal.lua")?;

    let init_suffix = r#"
_init()
__reset_button_states()
"#;

    let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, init_suffix);

    println!("Parsing and compiling game...");
    let ast = full_moon::parse(&full_code)?;
    let (cfg, fun_defs) = frontend::compile(&ast)?;

    let mut fixed_env = create_fixed_env_with_game_builtins();
    for fun_def in fun_defs {
        fixed_env.add_fun_def(fun_def);
    }

    let initial_state = create_initial_state_with_builtins(&fixed_env);

    // Initialize the game (_init function)
    println!("Initializing game...");
    let init_result = interpret_cfg(cfg, initial_state, &fixed_env)?;
    let init_states: Vec<_> = init_result.into_iter().map(|(s, _)| s).collect();
    println!("After _init: {} states ({} expanded)\n",
             init_states.len(), count_expanded(&init_states));

    // Compile the frame code
    let frame_code = r#"
__reset_button_states()
_update()
_draw()
"#;
    let frame_ast = full_moon::parse(frame_code)?;
    let (frame_cfg, frame_fun_defs) = frontend::compile(&frame_ast)?;
    assert!(frame_fun_defs.is_empty(), "Frame code shouldn't define new functions");

    let run_reference = !args.symbolic_only;
    let run_symbolic = !args.reference_only;

    // Initialize state for each implementation
    let mut ref_states = if run_reference { init_states.clone() } else { Vec::new() };
    let mut sym_states = if run_symbolic { init_states } else { Vec::new() };
    let mut cache = TraceCache::new();
    let mut total_stats = RunStats::default();

    let total_start = Instant::now();
    let mut all_matched = true;

    println!("Running {} frames...\n", args.num_frames);
    println!("{:>5} {:>12} {:>12} {:>10} {:>10} {:>8}",
             "Frame", "Ref Exp", "Sym Exp", "Match", "Ref ms", "Sym ms");
    println!("{}", "-".repeat(70));

    for frame in 1..=args.num_frames {
        // Run reference implementation
        let (ref_count, ref_time) = if run_reference {
            let start = Instant::now();
            ref_states = run_reference_frame(&frame_cfg, ref_states, &fixed_env)?;
            let elapsed = start.elapsed();
            (count_expanded(&ref_states), elapsed.as_millis())
        } else {
            (0, 0)
        };

        // Run symbolic tracing
        let (sym_count, sym_time, stats) = if run_symbolic {
            let start = Instant::now();
            let (new_states, stats) = run_traced(
                &frame_cfg,
                sym_states,
                &fixed_env,
                &mut cache,
            )?;
            let elapsed = start.elapsed();

            // Symbolic tracing returns scalar states, so count = len
            let count = new_states.len();
            sym_states = new_states;

            (count, elapsed.as_millis(), Some(stats))
        } else {
            (0, 0, None)
        };

        // Compare
        let matched = if run_reference && run_symbolic {
            ref_count == sym_count
        } else {
            true
        };

        if !matched {
            all_matched = false;
        }

        let match_str = if !run_reference || !run_symbolic {
            "-"
        } else if matched {
            "✓"
        } else {
            "✗"
        };

        println!("{:>5} {:>12} {:>12} {:>10} {:>10} {:>8}",
                 frame,
                 if run_reference { ref_count.to_string() } else { "-".to_string() },
                 if run_symbolic { sym_count.to_string() } else { "-".to_string() },
                 match_str,
                 if run_reference { ref_time.to_string() } else { "-".to_string() },
                 if run_symbolic { sym_time.to_string() } else { "-".to_string() });

        // Update totals
        if let Some(stats) = stats {
            total_stats.states_processed += stats.states_processed;
            total_stats.new_traces += stats.new_traces;
            total_stats.cache_hits += stats.cache_hits;
            total_stats.cache_misses += stats.cache_misses;
            total_stats.forced_choices += stats.forced_choices;
            total_stats.unique_shape_paths += stats.unique_shape_paths;
            total_stats.potential_cache_hits += stats.potential_cache_hits;
        }
    }

    let total_elapsed = total_start.elapsed();

    println!("\n=== Summary ===");
    println!("Total time: {:.2}s", total_elapsed.as_secs_f64());

    if run_reference {
        println!("Reference final: {} states ({} expanded)",
                 ref_states.len(), count_expanded(&ref_states));
    }

    if run_symbolic {
        println!("Symbolic final: {} states", sym_states.len());
        println!("\nSymbolic tracing stats:");
        println!("  States processed: {}", total_stats.states_processed);
        println!("  New traces: {}", total_stats.new_traces);
        println!("  Unique (shape,path) pairs: {}", total_stats.unique_shape_paths);
        println!("  Potential cache hits: {} ({:.1}%)",
                 total_stats.potential_cache_hits,
                 100.0 * total_stats.potential_cache_hits as f64 / total_stats.states_processed.max(1) as f64);
        println!("  Cache size: {} traces", cache.len());
    }

    if run_reference && run_symbolic {
        println!("\nComparison: {}", if all_matched { "ALL MATCHED ✓" } else { "MISMATCH ✗" });
    }

    Ok(())
}
