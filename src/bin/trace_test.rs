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
    symbolic_tracing::{TraceCache, run_traced, RunStats},
};

#[derive(Parser)]
#[command(name = "trace_test")]
#[command(about = "Test symbolic tracing on Celeste")]
struct Args {
    /// Number of frames to run
    #[arg(short = 'n', long, default_value_t = 5)]
    num_frames: u32,

    /// Compare with reference implementation
    #[arg(long)]
    compare: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!("=== Symbolic Tracing Test ===\n");

    // Load and compile the game (same as concrete_run.rs)
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
    let mut states: Vec<_> = init_result.into_iter().map(|(s, _)| s).collect();
    println!("After _init: {} states\n", states.len());

    // Compile the frame code - must reset buttons to UnknownBool before each frame!
    let frame_code = r#"
__reset_button_states()
_update()
_draw()
"#;
    let frame_ast = full_moon::parse(frame_code)?;
    let (frame_cfg, frame_fun_defs) = frontend::compile(&frame_ast)?;
    assert!(frame_fun_defs.is_empty(), "Frame code shouldn't define new functions");

    println!("Frame CFG has {} blocks\n", frame_cfg.named.len());

    // Run frames with symbolic tracing
    let mut cache = TraceCache::new();
    let mut total_stats = RunStats::default();

    println!("Running {} frames with symbolic tracing...\n", args.num_frames);

    for frame in 1..=args.num_frames {
        let frame_start = Instant::now();

        let (new_states, stats) = run_traced(
            &frame_cfg,
            states,
            &fixed_env,
            &mut cache,
        )?;

        let elapsed = frame_start.elapsed();

        let total_vector_size: usize = new_states.iter().map(|s| s.vector_size).sum();
        println!(
            "Frame {}: {} states ({} expanded), {} ms",
            frame,
            new_states.len(),
            total_vector_size,
            elapsed.as_millis()
        );
        println!(
            "  Cache: {} hits, {} misses, {} new traces",
            stats.cache_hits, stats.cache_misses, stats.new_traces
        );
        println!(
            "  Forced choices: {}, States processed: {}",
            stats.forced_choices, stats.states_processed
        );

        // Update totals
        total_stats.states_processed += stats.states_processed;
        total_stats.new_traces += stats.new_traces;
        total_stats.cache_hits += stats.cache_hits;
        total_stats.cache_misses += stats.cache_misses;
        total_stats.forced_choices += stats.forced_choices;

        states = new_states;
    }

    println!("\n=== Summary ===");
    println!("Total states processed: {}", total_stats.states_processed);
    println!("Total new traces: {}", total_stats.new_traces);
    println!("Total cache hits: {}", total_stats.cache_hits);
    println!("Total cache misses: {}", total_stats.cache_misses);
    println!("Cache size: {} traces", cache.len());
    println!("{}", cache.stats());

    // Optionally compare with reference
    if args.compare {
        println!("\n=== Comparison with Reference ===");
        // TODO: Implement comparison
        println!("(comparison not yet implemented)");
    }

    Ok(())
}
