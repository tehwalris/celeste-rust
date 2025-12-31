//! Benchmark binary for profiling captured slow function calls.
//!
//! Usage:
//!   cargo build --release --bin slow_call_benchmark
//!   ./target/release/slow_call_benchmark /tmp/slow_call.json.zst
//!
//! For perf profiling:
//!   perf record -g ./target/release/slow_call_benchmark /tmp/slow_call.json.zst
//!   perf report

use celeste_rust::frontend;
use celeste_rust::game_runner::create_fixed_env_with_game_builtins;
use celeste_rust::interpreter::input_capture::{load_slow_call, CapturedSlowCall};
use celeste_rust::interpreter::local_env::LocalEnv;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::Value;
use celeste_rust::ir::GlobalId;
use std::time::Instant;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <slow_call.json.zst> [iterations]", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];
    let iterations: u32 = args.get(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);

    println!("Loading slow call from {}...", path);
    let captured = load_slow_call(path).expect("Failed to load slow call");

    println!("Captured call info:");
    println!("  Function: {}", captured.function_name);
    println!("  Args: {} values", captured.args.len());
    println!("  State vector_size: {}", captured.state.vector_size);
    println!("  State heap_len: {}", captured.state.heap.len());
    println!("  Original duration: {:.2}ms", captured.duration_us as f64 / 1000.0);
    println!();

    // Load and compile the game to get function definitions
    println!("Loading game code...");
    let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
        .expect("Failed to read builtin_level_3.lua");
    let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
        .expect("Failed to read builtin_level_4.lua");
    let game = std::fs::read_to_string("lua/celeste-minimal.lua")
        .expect("Failed to read celeste-minimal.lua");

    let full_code = format!("{}\n{}\n{}\n", level_3, level_4, game);
    let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
    let (_cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

    let mut fixed_env = create_fixed_env_with_game_builtins();
    for fun_def in fun_defs {
        fixed_env.add_fun_def(fun_def);
    }
    println!("Loaded {} function definitions", fixed_env.fun_defs.len());
    println!();

    // Look up the function
    let function_id: GlobalId = captured.function_name.clone().into();
    let (fun_def, prepared_cfg) = fixed_env
        .fun_defs
        .get(&function_id)
        .unwrap_or_else(|| panic!("Unknown function: {}", captured.function_name));

    // Warm up
    println!("Warming up...");
    for _ in 0..3 {
        let state = setup_function_state(&captured, fun_def, &captured.state);
        let result = celeste_rust::interpreter::glue::interpret_prepared_cfg_with_name(
            prepared_cfg,
            state,
            &fixed_env,
            Some(captured.function_name.clone()),
            fun_def.source_span,
        );
        std::hint::black_box(result);
    }

    // Benchmark
    println!("Running {} iterations...", iterations);
    let start = Instant::now();
    for i in 0..iterations {
        let state = setup_function_state(&captured, fun_def, &captured.state);
        let iter_start = Instant::now();
        let result = celeste_rust::interpreter::glue::interpret_prepared_cfg_with_name(
            prepared_cfg,
            state,
            &fixed_env,
            Some(captured.function_name.clone()),
            fun_def.source_span,
        );
        let iter_time = iter_start.elapsed();
        println!("  Iteration {}: {:.2}ms", i + 1, iter_time.as_secs_f64() * 1000.0);
        std::hint::black_box(result);
    }
    let total = start.elapsed();
    let avg = total / iterations;

    println!();
    println!("=== Results ===");
    println!("Total time: {:?}", total);
    println!("Average per call: {:?} ({:.2}ms)", avg, avg.as_secs_f64() * 1000.0);
    println!("Original captured duration: {:.2}ms", captured.duration_us as f64 / 1000.0);
}

fn setup_function_state(
    captured: &CapturedSlowCall,
    fun_def: &celeste_rust::ir::FunDef,
    original_state: &State,
) -> State {
    // Create a new local_env for the function body
    let mut new_local_env = LocalEnv::new();

    // Set up argument values (from captured call)
    for (i, arg_id) in fun_def.arg_ids.iter().enumerate() {
        if let Some(arg_id) = arg_id {
            let value = captured.args
                .get(i)
                .cloned()
                .unwrap_or(Value::Nil(Some("missing argument".to_string())));
            new_local_env.set(*arg_id, value);
        }
    }

    // Create the function state - similar to how core_interpreter does it
    // Note: captured_values would normally come from the closure, but for tile_flag_at
    // (a global function), there are no captured values
    let mut new_outer_local_envs = vec![original_state.local_env.clone()];
    new_outer_local_envs.extend(original_state.outer_local_envs.clone());

    State {
        heap: original_state.heap.clone(),
        local_env: new_local_env,
        outer_local_envs: new_outer_local_envs,
        global_env: original_state.global_env.clone(),
        prints: original_state.prints.clone(),
        vector_size: original_state.vector_size,
    }
}
