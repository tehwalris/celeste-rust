//! Test binary for comparing symbolic tracing against reference interpreter.
//!
//! Run with: cargo run --release --bin trace_test -- --compare -n 28
//!
//! This tests that symbolic tracing produces the same state counts as the
//! reference vectorized interpreter for each frame.

use std::sync::Arc;
use std::time::Instant;

use anyhow::Result;
use clap::Parser;
use rayon::prelude::*;

use celeste_rust::{
    frontend,
    game_runner::{create_fixed_env_with_game_builtins, create_initial_state_with_builtins},
    interpreter::glue::interpret_cfg,
    interpreter::state::State,
    interpreter::fixed_env::FixedEnv,
    interpreter::inspect::make_state_abstract,
    interpreter::vectorize::vectorize_states,
    ir::Cfg,
    symbolic_tracing::{TraceCache, run_traced, run_traced_parallel, run_traced_parallel_cached, RunStats},
};

#[derive(Parser)]
struct Args {
    /// Number of frames to run
    #[arg(short = 'n', long, default_value = "28")]
    num_frames: usize,

    /// Only run reference interpreter (skip symbolic)
    #[arg(long)]
    reference_only: bool,

    /// Only run symbolic tracing (skip reference)
    #[arg(long)]
    symbolic_only: bool,

    /// Run both and compare (default)
    #[arg(long)]
    compare: bool,

    /// Use parallel tracing (multi-threaded)
    #[arg(long)]
    parallel: bool,

    /// Use parallel tracing with caching
    #[arg(long)]
    cached: bool,
}

/// Run the reference (vectorized) implementation for one frame
/// This matches main.rs exactly: interpret, GC, abstract, vectorize
fn run_reference_frame(
    frame_cfg: &Arc<celeste_rust::ir::Cfg>,
    states: Vec<State>,
    fixed_env: &FixedEnv,
) -> Result<Vec<State>> {
    let mut all_results = Vec::new();
    for state in states {
        // Reference interpreter takes owned Cfg, so clone the inner value
        let results = interpret_cfg(Cfg::clone(&*frame_cfg), state, fixed_env)?;
        for (s, _) in results {
            all_results.push(s);
        }
    }

    // GC states (like main.rs) - parallelize for many states
    all_results.par_iter_mut().for_each(|state| {
        state.gc();
    });

    // Make states abstract - widen player.rem to interval (like main.rs)
    all_results = all_results.into_par_iter().map(make_state_abstract).collect();

    // Vectorize states to combine similar ones (like main.rs)
    all_results = vectorize_states(all_results);

    Ok(all_results)
}

/// Count total expanded states (sum of vector_size)
fn count_expanded(states: &[State]) -> usize {
    states.iter().map(|s| s.vector_size).sum()
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!("=== Symbolic Tracing Test ===\n");

    // Parse and compile the game (same as main.rs)
    println!("Parsing and compiling game...");
    let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")?;
    let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = std::fs::read_to_string("lua/celeste-minimal.lua")?;
    let init_suffix = r#"
_init()
__reset_button_states()
"#;
    let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, init_suffix);
    let ast = full_moon::parse(&full_code)?;
    let (cfg, fun_defs) = frontend::compile(&ast)?;

    // Create fixed environment with game builtins
    let mut fixed_env = create_fixed_env_with_game_builtins();
    for fun_def in fun_defs {
        fixed_env.add_fun_def(fun_def);
    }

    // Initialize game state
    println!("Initializing game...");
    let init_state = create_initial_state_with_builtins(&fixed_env);
    let init_results = interpret_cfg(cfg.clone(), init_state, &fixed_env)?;
    let init_states: Vec<State> = init_results.into_iter().map(|(s, _)| s).collect();
    let init_expanded: usize = init_states.iter().map(|s| s.vector_size).sum();
    println!("After _init: {} states ({} expanded)\n", init_states.len(), init_expanded);

    // Compile the frame code (must match main.rs order)
    let frame_code = r#"
_update()
_draw()
__reset_button_states()
"#;
    let frame_ast = full_moon::parse(frame_code)?;
    let (frame_cfg, frame_fun_defs) = frontend::compile(&frame_ast)?;
    let frame_cfg = Arc::new(frame_cfg);  // Wrap in Arc to avoid cloning
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
        let (sym_count, sym_time, stats, trace_ms, gc_ms, abstract_ms, vectorize_ms, pre_vectorize_count) = if run_symbolic {
            // Clear cache between frames - HeapIds change after GC
            cache.clear();

            let trace_start = Instant::now();
            let (mut new_states, stats) = if args.cached {
                run_traced_parallel_cached(
                    frame_cfg.clone(),
                    sym_states,
                    &fixed_env,
                    &mut cache,
                )?
            } else if args.parallel {
                run_traced_parallel(
                    frame_cfg.clone(),
                    sym_states,
                    &fixed_env,
                    &mut cache,
                )?
            } else {
                run_traced(
                    frame_cfg.clone(),
                    sym_states,
                    &fixed_env,
                    &mut cache,
                )?
            };
            let trace_ms = trace_start.elapsed().as_millis();

            // Apply same post-processing as reference: GC, abstract, vectorize
            // Use parallel GC since we have many states
            let gc_start = Instant::now();
            new_states.par_iter_mut().for_each(|state| {
                state.gc();
            });
            let gc_ms = gc_start.elapsed().as_millis();

            let abstract_start = Instant::now();
            new_states = new_states.into_par_iter().map(make_state_abstract).collect();
            let abstract_ms = abstract_start.elapsed().as_millis();

            // Log state count before vectorization for larger frames
            let pre_vectorize_count = new_states.len();

            let vectorize_start = Instant::now();
            new_states = vectorize_states(new_states);
            let vectorize_ms = vectorize_start.elapsed().as_millis();

            let elapsed = trace_start.elapsed();

            // Count expanded states (sum of vector_size after vectorization)
            let count = count_expanded(&new_states);
            sym_states = new_states;

            (count, elapsed.as_millis(), Some(stats), trace_ms, gc_ms, abstract_ms, vectorize_ms, pre_vectorize_count)
        } else {
            (0, 0, None, 0, 0, 0, 0, 0)
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

        // Update cumulative stats
        if let Some(ref stats) = stats {
            total_stats = total_stats.merge(&stats);
        }

        // Print row
        let match_str = if run_reference && run_symbolic {
            if matched { "✓" } else { "✗" }
        } else {
            "-"
        };
        println!("{:>5} {:>12} {:>12} {:>10} {:>10} {:>8}",
                 frame,
                 if run_reference { format!("{}", ref_count) } else { "-".to_string() },
                 if run_symbolic { format!("{}", sym_count) } else { "-".to_string() },
                 match_str,
                 if run_reference { format!("{}", ref_time) } else { "-".to_string() },
                 if run_symbolic { format!("{}", sym_time) } else { "-".to_string() });

        // For larger frames, show timing breakdown
        if run_symbolic && sym_time > 1000 {
            if let Some(ref s) = stats {
                println!("      Stats: {} traces, {} forced_choices, {} pre-vectorize states",
                         s.new_traces, s.forced_choices, pre_vectorize_count);
            }
            println!("      Timing: trace={}ms gc={}ms abstract={}ms vectorize={}ms",
                     trace_ms, gc_ms, abstract_ms, vectorize_ms);
        }
    }

    let total_time = total_start.elapsed();

    println!("\n=== Summary ===");
    println!("Total time: {:.2}s", total_time.as_secs_f64());

    if run_reference {
        println!("Reference final: {} states ({} expanded)",
                 ref_states.len(), count_expanded(&ref_states));
    }

    if run_symbolic {
        println!("Symbolic final: {} states", sym_states.len());
        println!("\nSymbolic tracing stats:");
        println!("  States processed: {}", total_stats.states_processed);
        println!("  New traces: {}", total_stats.new_traces);
        println!("  Forced choices (path forks): {}", total_stats.forced_choices);
        println!("  Cache hits: {}", total_stats.cache_hits);
        println!("  Cache misses: {}", total_stats.cache_misses);
        let cache_stats = cache.stats();
        println!("  No-template misses: {}", cache_stats.no_template_misses);
        println!("  Trace-not-found misses: {}", cache_stats.trace_not_found_misses);
        println!("  Condition evals: {}", cache_stats.condition_evals);
        println!("  Avg evals per lookup: {:.1}",
                 if total_stats.states_processed > 0 {
                     cache_stats.condition_evals as f64 / total_stats.states_processed as f64
                 } else { 0.0 });
        println!("  Reusable traces (no concrete branches): {}", total_stats.reusable_traces);
        println!("  Unreusable traces (has concrete branches): {}", total_stats.unreusable_traces);
        println!("  Total concrete branches: {} (avg {:.1} per trace)",
                 total_stats.total_concrete_branches,
                 if total_stats.new_traces > 0 {
                     total_stats.total_concrete_branches as f64 / total_stats.new_traces as f64
                 } else { 0.0 });
        println!("  Total function calls: {} (avg {:.1} per trace)",
                 total_stats.total_function_calls,
                 if total_stats.new_traces > 0 {
                     total_stats.total_function_calls as f64 / total_stats.new_traces as f64
                 } else { 0.0 });
        println!("  Unique (shape,path) pairs: {}", total_stats.unique_shape_paths);
        println!("  Unique (shape,path,concrete_path) tuples: {}", total_stats.unique_full_paths);
        println!("  Template mismatches: {}", total_stats.template_mismatches);
        println!("  Potential cache hits: {} ({:.1}%)",
                 total_stats.potential_cache_hits,
                 if total_stats.states_processed > 0 {
                     100.0 * total_stats.potential_cache_hits as f64 / total_stats.states_processed as f64
                 } else { 0.0 });
        println!("  Cache: {} traces, {} templates",
                 cache.len(),
                 cache.num_templates());
    }

    if run_reference && run_symbolic {
        println!("\nComparison: {}", if all_matched { "MATCH ✓" } else { "MISMATCH ✗" });
    }

    Ok(())
}
