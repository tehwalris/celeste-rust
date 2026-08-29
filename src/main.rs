// Use mimalloc as the global allocator for better performance
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::Result;
use clap::Parser;

use celeste_rust::game_runner;
use celeste_rust::interpreter;

#[derive(Parser, Debug)]
#[command(name = "celeste-rust")]
#[command(about = "Abstract interpreter for PICO-8 Celeste")]
struct Args {
    /// Number of frames to run
    #[arg(short = 'n', long, default_value_t = 30)]
    frames: u32,

    /// Run the RECIPE-REWRITTEN program through the same search loop
    /// (states verified identical; see rewrite verify). Prototype for
    /// making the rewritten program the search's program.
    #[arg(long)]
    rewritten: bool,

    /// Show detailed state info for frames starting at this number
    #[arg(long, default_value_t = 25)]
    detail_from: u32,

    /// Show detailed state info for frames up to this number
    #[arg(long, default_value_t = 27)]
    detail_to: u32,

    /// Output JSONL file for frame dumps (state summaries)
    #[arg(long)]
    dump: Option<String>,

    /// Dump full states to JSONL file at this frame number
    #[arg(long)]
    dump_states_at: Option<u32>,

    /// Output file for full state dump (use with --dump-states-at)
    #[arg(long)]
    states_file: Option<String>,

    /// Directory for checkpoints (enables checkpoint saving)
    #[arg(long)]
    checkpoint_dir: Option<String>,

    /// Save checkpoint every N frames (default: 1 = every frame)
    #[arg(long, default_value_t = 1)]
    checkpoint_interval: u32,

    /// Resume from the latest checkpoint in the checkpoint directory
    #[arg(long)]
    resume: bool,

}

fn main() -> Result<()> {
    let args = Args::parse();

    if args.rewritten {
        let program = celeste_rust::program::frozen::rewritten("rewrites.jsonl")?;
        let mut run = celeste_rust::search::run::AbstractRun::start(&program)?;
        for frame in 1..=args.frames {
            let t = std::time::Instant::now();
            run.step()?;
            // Won lanes have exited the room; they are absorbing (their
            // arrival frame is the result) and must NOT be expanded - the
            // next-room state is out of scope and, on the kernel engine, has
            // no kernel for that room's shape. The ladder/bench drivers do
            // this; the raw forward must too, or it re-dispatches an exit
            // state and hits a coverage gap near the exit frame.
            run.absorb_won_lanes();
            println!(
                "Frame {}: {} states ({} expanded) in {:?}",
                frame,
                run.states().len(),
                run.lane_count(),
                t.elapsed()
            );
        }
        println!(
            "\nTotal: {} states ({} expanded) after {} frames",
            run.states().len(),
            run.lane_count(),
            args.frames
        );
        celeste_rust::compiled::print_asm_append_stats();
        return Ok(());
    }
    run_game_frames(
        args.frames,
        args.detail_from,
        args.detail_to,
        args.dump.as_deref(),
        args.dump_states_at,
        args.states_file.as_deref(),
        args.checkpoint_dir.as_deref(),
        args.checkpoint_interval,
        args.resume,
    )
}

/// Find the latest checkpoint in a directory
fn find_latest_checkpoint(dir: &str) -> Option<(String, u32)> {
    let mut latest: Option<(String, u32)> = None;

    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(filename) = path.file_name().and_then(|f| f.to_str()) {
                // Parse checkpoint_frameNNNN.jsonl.zst
                if filename.starts_with("checkpoint_frame") && filename.ends_with(".jsonl.zst") {
                    if let Some(frame_str) = filename
                        .strip_prefix("checkpoint_frame")
                        .and_then(|s| s.strip_suffix(".jsonl.zst"))
                    {
                        if let Ok(frame) = frame_str.parse::<u32>() {
                            if latest.is_none() || frame > latest.as_ref().unwrap().1 {
                                latest = Some((path.to_string_lossy().to_string(), frame));
                            }
                        }
                    }
                }
            }
        }
    }

    latest
}

fn run_game_frames(
    num_frames: u32,
    detail_from: u32,
    detail_to: u32,
    dump_path: Option<&str>,
    dump_states_at: Option<u32>,
    states_file: Option<&str>,
    checkpoint_dir: Option<&str>,
    checkpoint_interval: u32,
    resume: bool,
) -> Result<()> {
    use crate::interpreter::glue::interpret_cfg;
    use crate::interpreter::abstraction::make_state_abstract;
    use crate::interpreter::inspect::{create_frame_dump, write_frame_dump_jsonl, dump_states_to_file, save_checkpoint, load_checkpoint, checkpoint_filename, Checkpoint};
    use crate::game_runner::{create_initial_state_with_builtins, inject_tile_flag_at_builtin};
    use std::io::BufWriter;
    use std::fs::File;


    // Create checkpoint directory if needed
    if let Some(dir) = checkpoint_dir {
        std::fs::create_dir_all(dir)?;
    }

    // The program is the shared, derived one - the same assembly every
    // other tool runs (sources + start-room substitution + init/frame
    // chunks); see the program module.
    let sources = celeste_rust::program::Sources::load_from_disk()
        .expect("Failed to load game sources");
    let mut program = celeste_rust::program::Program::compile(&sources)
        .expect("Failed to compile game program");
    program
        .pin_native_builtins()
        .expect("Failed to pin native builtins");
    let fixed_env = program.fixed_env();
    let cfg = program.init_cfg().clone();
    let frame_cfg = program.frame_cfg().clone();

    // Try to resume from checkpoint if requested
    let (mut states, start_frame) = if resume && checkpoint_dir.is_some() {
        let dir = checkpoint_dir.unwrap();
        match find_latest_checkpoint(dir) {
            Some((checkpoint_path, frame)) => {
                println!("Resuming from checkpoint: {} (frame {})", checkpoint_path, frame);
                let load_start = std::time::Instant::now();
                let checkpoint = load_checkpoint(&checkpoint_path)
                    .expect("Failed to load checkpoint");
                println!("Loaded {} states ({} expanded) in {:?}",
                    checkpoint.states.len(),
                    checkpoint.states.iter().map(|s| s.vector_size).sum::<usize>(),
                    load_start.elapsed());
                (checkpoint.states, frame + 1)
            }
            None => {
                println!("No checkpoint found, starting from init");
                let initial_state = create_initial_state_with_builtins(&fixed_env);
                println!("Running game init...");
                let start = std::time::Instant::now();
                let init_result_states =
                    interpret_cfg(cfg.clone(), initial_state, &fixed_env).expect("Init interpretation failed");
                println!("Game init completed in {:?}", start.elapsed());
                println!("States after init: {}", init_result_states.len());
                (init_result_states.into_iter().map(|(s, _)| s).collect(), 1)
            }
        }
    } else {
        let initial_state = create_initial_state_with_builtins(&fixed_env);
        println!("Running game init...");
        let start = std::time::Instant::now();
        let init_result_states =
            interpret_cfg(cfg.clone(), initial_state, &fixed_env).expect("Init interpretation failed");
        println!("Game init completed in {:?}", start.elapsed());
        println!("States after init: {}", init_result_states.len());
        (init_result_states.into_iter().map(|(s, _)| s).collect(), 1)
    };

    // Inject tile_flag_at builtin into all states (replaces Lua version with faster Rust builtin)
    // This is done after Lua init so we don't need to modify the Lua source
    for state in &mut states {
        inject_tile_flag_at_builtin(state);
    }
    println!("Injected tile_flag_at builtin into {} states", states.len());

    // Open dump file if requested
    let mut dump_writer = dump_path.map(|path| {
        let file = File::create(path).expect("Failed to create dump file");
        BufWriter::new(file)
    });

    // Dump frame 0 (init state) only if starting fresh
    if start_frame == 1 {
        if let Some(ref mut writer) = dump_writer {
            let dump = create_frame_dump(0, &states);
            write_frame_dump_jsonl(&dump, writer).expect("Failed to write dump");
        }
    }

    // Frontier-only search: persistent cross-frame visited set of canonical
    // 128-bit row hashes, keyed by shape hash. Not exact-key, but the
    // birthday collision risk at 10^8 rows is negligible. Sound only from a
    // fresh start - on --resume the visited set is empty, which loses dedup
    // but never completeness.
    let mut visited_rows: Option<crate::interpreter::visited::Visited> =
        if std::env::var_os("CELESTE_FRONTIER_ONLY").is_some() {
            println!("frontier-only search ENABLED (128-bit hashed visited set)");
            Some(crate::interpreter::visited::Visited::in_memory())
        } else {
            None
        };

    for frame_num in start_frame..=num_frames {
        print!("Frame {}: ", frame_num);

        let start = std::time::Instant::now();

        let mut new_states = Vec::new();

        for state in states {
            let result = interpret_cfg(frame_cfg.clone(), state, &fixed_env)
                .expect("Frame interpretation failed");
            new_states.extend(result.into_iter().map(|(s, _)| s));
        }

        // Make states abstract (widen player.rem to interval)
        new_states = new_states
            .into_iter()
            .flat_map(crate::interpreter::abstraction::split_precision_straddles)
            .map(make_state_abstract)
            .collect();

        let before_vec = new_states.len();

        // Vectorize states (GC + materialize + merge by shape)
        let new_states = crate::interpreter::vectorize::vectorize_states(new_states);

        let after_vec = new_states.len();
        if before_vec > 0 && before_vec != after_vec {
            let avg_vs: f64 = new_states.iter().map(|s| s.vector_size as f64).sum::<f64>() / new_states.len() as f64;
            println!("  (vec: {} -> {} states, merged {}, avg_vs={:.1})",
                before_vec, after_vec, before_vec - after_vec, avg_vs);
        }

        // Frontier-only search (CELESTE_FRONTIER_ONLY=1): drop lanes already
        // reached at an earlier frame; expand only the new ones next frame.
        // Experimental sizing version - see subtract_visited's soundness note.
        let new_states = if let Some(visited) = visited_rows.as_mut() {
            // The ONE row key (kernel/engine key), recomputed per state, then
            // the local dedup + probe + insert.
            let (mut kept, mut lanes_before, mut lanes_after) = (Vec::new(), 0usize, 0usize);
            for state in new_states {
                let engine_keys = celeste_rust::compiled::engine_row_keys(&state)
                    .expect("engine row keys");
                let vk = crate::interpreter::vectorize::candidates_from_keys(
                    engine_keys,
                    state.vector_size,
                    visited,
                );
                let (survivors, b, a) = crate::interpreter::vectorize::subtract_precomputed(
                    state,
                    Some(vk),
                    visited,
                );
                lanes_before += b;
                lanes_after += a;
                kept.extend(survivors);
            }
            visited.end_frame().expect("in-memory visited end_frame cannot fail");
            println!(
                "  (frontier-only: {} -> {} new lanes, visited total {})",
                lanes_before, lanes_after, visited.len()
            );
            kept
        } else {
            new_states
        };

        let expanded_output: usize = new_states.iter().map(|s| s.vector_size).sum();
        println!("{} states ({} expanded) in {:?}",
            new_states.len(), expanded_output, start.elapsed());

        // Show detailed state info for specified frame range
        if frame_num >= detail_from && frame_num <= detail_to {
            for (i, state) in new_states.iter().enumerate() {
                println!("  State {}: vector_size={}, heap_len={}",
                    i, state.vector_size, state.heap.len());
            }
        }

        // Dump frame data
        if let Some(ref mut writer) = dump_writer {
            let dump = create_frame_dump(frame_num, &new_states);
            write_frame_dump_jsonl(&dump, writer).expect("Failed to write dump");
        }

        // Dump full states at specified frame
        if dump_states_at == Some(frame_num) {
            let output_path = states_file.unwrap_or("/tmp/states_dump.jsonl");
            println!("Dumping {} full states to {}", new_states.len(), output_path);
            dump_states_to_file(&new_states, output_path).expect("Failed to dump states");
        }

        states = new_states;

        // Save checkpoint if at interval
        if let Some(dir) = checkpoint_dir {
            if checkpoint_interval > 0 && frame_num % checkpoint_interval == 0 {
                let checkpoint_path = format!("{}/{}", dir, checkpoint_filename(frame_num));
                print!("  Saving checkpoint to {}... ", checkpoint_path);
                std::io::Write::flush(&mut std::io::stdout()).ok();
                let save_start = std::time::Instant::now();
                let checkpoint = Checkpoint {
                    frame: frame_num,
                    states: states.clone(),
                };
                save_checkpoint(&checkpoint, &checkpoint_path)
                    .expect("Failed to save checkpoint");
                let file_size = std::fs::metadata(&checkpoint_path)
                    .map(|m| m.len())
                    .unwrap_or(0);
                println!("done ({} bytes) in {:?}", file_size, save_start.elapsed());
            }
        }
    }

    // Flush dump file
    if let Some(ref mut writer) = dump_writer {
        use std::io::Write;
        writer.flush().expect("Failed to flush dump file");
    }

    println!("\nTotal: {} states ({} expanded) after {} frames",
        states.len(),
        states.iter().map(|s| s.vector_size).sum::<usize>(),
        num_frames);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_rust::frontend;
    use crate::game_runner::{
        create_fixed_env_with_builtins,
        create_fixed_env_with_game_builtins,
        create_initial_state_with_builtins,
    };
    use crate::interpreter::value::HeapValue;
    #[test]
    fn test_parse_hello_world() {
        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");

        // Just test that parsing works
        assert!(ast.nodes().stmts().count() > 0 || ast.nodes().last_stmt().is_some());
    }

    #[test]
    fn test_compile_hello_world() {
        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");

        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        // The code has no function definitions
        assert!(fun_defs.is_empty());

        // The CFG should have instructions
        assert!(!cfg.entry.instructions.is_empty());
    }

    #[test]
    fn test_builtin_setup() {
        let fixed_env = create_fixed_env_with_builtins();
        let state = create_initial_state_with_builtins(&fixed_env);

        // Check that __print is in the global_env
        assert!(state.global_env.contains_key("__print"));

        // Get the heap_id for __print
        let heap_id = state.global_env.get("__print").unwrap();

        // Check that it's a BuiltinFun
        match state.heap.get(*heap_id) {
            HeapValue::BuiltinFun(name) => assert_eq!(name, "__print"),
            _ => panic!("Expected BuiltinFun"),
        }
    }

    #[test]
    fn test_interpret_hello_world() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, _fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let fixed_env = create_fixed_env_with_builtins();
        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should have exactly one resulting state
        assert_eq!(result_states.len(), 1);

        // Check that "walrus" was printed
        assert_eq!(result_states[0].0.prints, vec!["walrus"]);
    }

    #[test]
    fn test_interpret_simple_function() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"
function greet(name)
    __print(name)
end

greet("bob")
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        // Should have one function definition
        assert_eq!(fun_defs.len(), 1);

        let mut fixed_env = create_fixed_env_with_builtins();
        // Add the function definitions to the fixed_env
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should have exactly one resulting state
        assert_eq!(result_states.len(), 1);

        // Check that "bob" was printed
        assert_eq!(result_states[0].0.prints, vec!["bob"]);
    }

    #[test]
    fn test_interpret_function_with_return() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"
function add_one(x)
    return x + 1
end

__print(add_one(5))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(result_states[0].0.prints, vec!["6"]);
    }

    #[test]
    fn test_interpret_if_scopes() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/if_scopes.lua
        let code = r#"
y = nil

function f(x)
  if x then
    local y = 5
  else
    y = 7
  end
end

f(true)
__print(y)
f(false)
__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // First call f(true) - local y shadows global, so global y stays nil
        // Second call f(false) - global y becomes 7
        assert_eq!(result_states[0].0.prints, vec!["nil", "7"]);
    }

    #[test]
    fn test_interpret_scopes() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/scopes.lua
        let code = r#"
x = 0
y = 7

function g()
  y = 6

  local y = 4

  local f = function(x)
    __print(x)
    x = 2
    __print(x)
    __print(y)
    y = 3
  end

  local h = function()
    local x = 5
    f(x)
    __print(y)
    __print(x)
  end

  y = 5

  return h
end

__print(y)
f = g()
f()
__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Expected: 7, 5, 2, 5, 3, 5, 6
        // Line by line:
        // __print(y) -> 7 (global y)
        // g() sets global y = 6, creates local y = 4, then sets local y = 5
        // f = g() returns h
        // f() calls h which:
        //   - creates local x = 5
        //   - calls f(5) which:
        //     - __print(x) -> 5 (argument x)
        //     - x = 2 (modifies local x)
        //     - __print(x) -> 2
        //     - __print(y) -> 5 (captured from g, local y was 5)
        //     - y = 3 (modifies captured y)
        //   - __print(y) -> 3 (captured y, now 3)
        //   - __print(x) -> 5 (h's local x, unchanged)
        // __print(y) -> 6 (global y, was set by g())
        assert_eq!(
            result_states[0].0.prints,
            vec!["7", "5", "2", "5", "3", "5", "6"]
        );
    }

    #[test]
    fn test_interpret_call_order() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/call_order.lua
        let code = r#"
function fa(v)
    __print('a')
    __print(v)
    return 'a'
end

function fb(v)
    __print('b')
    __print(v)
    return 'b'
end

function fc()
    fd = fa
    __print('c')
    return 'c'
end

function fe(v1, v2)
    __print('e')
    __print(v1)
    __print(v2)
end

-- fd(fc()) -- attempt to call global 'fd' (a nil value)

fe(fc(), fd('x'))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Call order:
        // 1. fc() is called first, prints "c", sets fd = fa, returns "c"
        // 2. fd('x') is called (which is fa('x')), prints "a", "x", returns "a"
        // 3. fe("c", "a") is called, prints "e", "c", "a"
        assert_eq!(
            result_states[0].0.prints,
            vec!["c", "a", "x", "e", "c", "a"]
        );
    }

    #[test]
    fn test_interpret_call_with_different_number_of_args() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/call_with_different_number_of_args.lua
        let code = r#"
function f(a, b)
  __print(a)
  __print(b)
end

function g(v)
  __print(v)
  return v
end

f(g(1))
f(g(2), g(3))
f(g(4), g(5), g(6))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // f(g(1)): prints 1, then f(1, nil) prints 1, nil
        // f(g(2), g(3)): prints 2, 3, then f(2, 3) prints 2, 3
        // f(g(4), g(5), g(6)): prints 4, 5, 6, then f(4, 5) prints 4, 5 (6 is extra arg, ignored)
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "1", "nil", "2", "3", "2", "3", "4", "5", "6", "4", "5"]
        );
    }

    #[test]
    fn test_interpret_every_kind_of_if_else() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/every_kind_of_if_else.lua
        let code = r#"
x = true

if x then
  y = 1
  __print(y)
end

if x then
  y = 2
  __print(y)
else
  y = 3
  __print(y)
end

if x then
  y = 4
  __print(y)
elseif x then
  y = 5
  __print(y)
elseif x then
  y = 6
  __print(y)
end

if x then
  y = 7
  __print(y)
elseif x then
  y = 8
  __print(y)
elseif x then
  y = 9
  __print(y)
else
  y = 10
  __print(y)
end

__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // x is true, so we take the first branch in each if-chain
        // Prints: 1, 2, 4, 7, 7 (final y is 7)
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "2", "4", "7", "7"]
        );
    }

    #[test]
    fn test_interpret_short_circuit_operators() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/short_circuit_operators.lua
        let code = r#"
function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", true) and f("c", true))
__print(f("a", false) and f("b", true) and f("c", true))
__print(f("a", true) and f("b", false) and f("c", true))
__print(f("a", true) and f("b", true) and f("c", false))
__print(f("a", false) and f("b", false) and f("c", false))

__print(f("a", true) or f("b", true) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", true))
__print(f("a", true) or f("b", false) or f("c", true))
__print(f("a", true) or f("b", true) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", false))
__print(f("a", true) or f("b", false) or f("c", false))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Short-circuit 'and': returns first falsy value or last truthy
        // Short-circuit 'or': returns first truthy value or last falsy
        //
        // f("a", true) and f("b", true) and f("c", true) -> prints a, b, c; result is true
        // f("a", false) and f("b", true) and f("c", true) -> prints a; result is false (short-circuit)
        // f("a", true) and f("b", false) and f("c", true) -> prints a, b; result is false (short-circuit)
        // f("a", true) and f("b", true) and f("c", false) -> prints a, b, c; result is false
        // f("a", false) and f("b", false) and f("c", false) -> prints a; result is false (short-circuit)
        // f("a", true) or f("b", true) or f("c", true) -> prints a; result is true (short-circuit)
        // f("a", false) or f("b", true) or f("c", true) -> prints a, b; result is true (short-circuit)
        // f("a", true) or f("b", false) or f("c", true) -> prints a; result is true (short-circuit)
        // f("a", true) or f("b", true) or f("c", false) -> prints a; result is true (short-circuit)
        // f("a", false) or f("b", false) or f("c", false) -> prints a, b, c; result is false
        // f("a", false) or f("b", false) or f("c", true) -> prints a, b, c; result is true
        // f("a", false) or f("b", true) or f("c", false) -> prints a, b; result is true (short-circuit)
        // f("a", true) or f("b", false) or f("c", false) -> prints a; result is true (short-circuit)
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "a", "b", "c", "true",   // and chain 1
                "a", "false",            // and chain 2
                "a", "b", "false",       // and chain 3
                "a", "b", "c", "false",  // and chain 4
                "a", "false",            // and chain 5
                "a", "true",             // or chain 1
                "a", "b", "true",        // or chain 2
                "a", "true",             // or chain 3
                "a", "true",             // or chain 4
                "a", "b", "c", "false",  // or chain 5
                "a", "b", "c", "true",   // or chain 6
                "a", "b", "true",        // or chain 7
                "a", "true",             // or chain 8
            ]
        );
    }

    #[test]
    fn test_interpret_string_length() {
        use crate::interpreter::glue::interpret_cfg;

        // Basic string length test (subset of lua_tests/string_length.lua without tables)
        let code = r#"
__print(#"")
__print(#" ")
__print(#"walrus")
s = "walrus"
__print(#s)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // #"" = 0
        // #" " = 1
        // #"walrus" = 6
        // #s (s = "walrus") = 6
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "6", "6"]
        );
    }

    #[test]
    fn test_interpret_normal_operators() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/normal_operators.lua (lines 1-22, without table operations)
        let code = r#"
__print(1 + 1)
__print(5 - 7)
__print(7 - 5)
__print(3 * 4)
__print(12 / 3)
__print(2 - 2 / 4 + 0.5)
__print(12 % 5)
-- __print(12.5 % 5 + 0.5)  -- Skip: fractional modulo not yet implemented in Pico8Num
__print(-7)
__print(-(-7))
__print(1 < 2)
__print(1 <= 2)
__print(1 > 2)
__print(1 >= 2)
__print(1 == 2)
__print(1 ~= 2)
__print(not (1 == 2))
__print(not true)
__print("wal".."rus")
__print("12"..3)
__print((1).."23")
__print(((1).."23") == "123")

function_a = function () end
function_b = function () end
__print(function_a == function_a)
__print(function_a ~= function_a)
__print(function_a == function_b)
__print(function_a ~= function_b)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "2",        // 1 + 1
                "-2",       // 5 - 7
                "2",        // 7 - 5
                "12",       // 3 * 4
                "4",        // 12 / 3
                "2",        // 2 - 2/4 + 0.5 = 2 - 0.5 + 0.5 = 2
                "2",        // 12 % 5
                // (skipped fractional modulo)
                "-7",       // -7
                "7",        // -(-7)
                "true",     // 1 < 2
                "true",     // 1 <= 2
                "false",    // 1 > 2
                "false",    // 1 >= 2
                "false",    // 1 == 2
                "true",     // 1 ~= 2
                "true",     // not (1 == 2)
                "false",    // not true
                "walrus",   // "wal".."rus"
                "123",      // "12"..3
                "123",      // (1).."23"
                "true",     // ((1).."23") == "123"
                "true",     // function_a == function_a
                "false",    // function_a ~= function_a
                "false",    // function_a == function_b (different closures)
                "true",     // function_a ~= function_b
            ]
        );
    }

    /// `t[3] = v` on an empty table: legal Lua, and what room (2,0)'s fruit
    /// does (`got_fruit[1 + level_index()]` with level_index() == 2). The
    /// skipped indices must read back as nil, and a later append must land
    /// after them.
    #[test]
    fn test_interpret_index_assignment_past_the_end() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"
local t = {}
t[3] = true
__print(t[1])
__print(t[2])
__print(t[3])
__print(#t)
t[4] = "after"
__print(t[4])
__print(#t)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["nil", "nil", "true", "3", "after", "4"]
        );
    }

    #[test]
    fn test_interpret_for_range() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_range.lua (first part)
        let code = r#"
for i=0,5 do
    __print(i)
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Should print 0, 1, 2, 3, 4, 5
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "2", "3", "4", "5"]
        );
    }

    #[test]
    fn test_interpret_for_break() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_break.lua
        let code = r#"
for i=0,5 do
    if i == 3 then
        break
    end
    __print(i)
end
__print("end")
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Should print 0, 1, 2, then break, then "end"
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "2", "end"]
        );
    }

    #[test]
    fn test_interpret_for_range_vector_low() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_range_vector_low.lua
        // Tests for loops with vector start values
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

for i=low,1.5 do
  __print("low")
  __print(low)
  __print("i")
  __print(i)
end
__print("low after loop")
__print(low)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Expected output options from OCaml tests:
        // Option 1: for vector element 1 (starts at 1, 1 <= 1.5, print and exit loop)
        //   low, V[0, 1], i, V[0, 1], low after loop, 1
        // Option 2: for vector element 0 (starts at 0, iterates twice: 0 and 1)
        //   low, V[0, 1], i, V[0, 1], low, 0, i, 1, low after loop, 0

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            // Element 1: iterates once (1 <= 1.5)
            vec![
                "low".to_string(),
                "V[0, 1]".to_string(),
                "i".to_string(),
                "V[0, 1]".to_string(),
                "low after loop".to_string(),
                "1".to_string(),
            ],
            // Element 0: iterates twice (0 and 1 both <= 1.5)
            vec![
                "low".to_string(),
                "V[0, 1]".to_string(),
                "i".to_string(),
                "V[0, 1]".to_string(),
                "low".to_string(),
                "0".to_string(),
                "i".to_string(),
                "1".to_string(),
                "low after loop".to_string(),
                "0".to_string(),
            ],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_for_range_vector_both() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_range_vector_both.lua
        // Tests for loops with vector start and end values
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

for i=low,high do
  __print("low")
  __print(low)
  __print("i")
  __print(i)
end
__print("low after loop")
__print(low)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Expected output options from OCaml tests:
        // Option 1: both elements iterate once (low <= high for both)
        //   low, V[0, 1], i, V[0, 1], low after loop, 1
        // Option 2: element 0 iterates twice (0 and 1 both <= 1.5)
        //   low, V[0, 1], i, V[0, 1], low, 0, i, 1, low after loop, 0

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            // Element 1: iterates once (1 <= 1.8)
            vec![
                "low".to_string(),
                "V[0, 1]".to_string(),
                "i".to_string(),
                "V[0, 1]".to_string(),
                "low after loop".to_string(),
                "1".to_string(),
            ],
            // Element 0: iterates twice (0 and 1 both <= 1.5)
            vec![
                "low".to_string(),
                "V[0, 1]".to_string(),
                "i".to_string(),
                "V[0, 1]".to_string(),
                "low".to_string(),
                "0".to_string(),
                "i".to_string(),
                "1".to_string(),
                "low after loop".to_string(),
                "0".to_string(),
            ],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_simple_table() {
        use crate::interpreter::glue::interpret_cfg;

        // Simple nested table test
        let code = r#"
a = { b = 10 }
__print(a.b)
a.b = 20
__print(a.b)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["10", "20"]
        );
    }

    #[test]
    fn test_interpret_properties() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/properties.lua
        let code = r#"
a = { b = { c = 0, d = nil } }
__print(a.b.c)
a.b.c = 123
__print(a.b.c)
__print(a.b.d)
a.b.d = 456
__print(a.b.d)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "123", "nil", "456"]
        );
    }

    #[test]
    fn test_interpret_tables_object() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/tables.lua (first two parts, without array tables)
        let code = r#"
x = {}
x.a = 1
x.b = 7
x.c = 'hello'
__print(x.a)
__print(x.b)
__print(x.c)
x.c = 'world'
__print(x.c)

y = {a = 1, b = 7, c = 'hello'}
__print(y.a)
__print(y.b)
__print(y.c)
y.c = 'world'
__print(y.c)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "7", "hello", "world", "1", "7", "hello", "world"]
        );
    }

    #[test]
    fn test_interpret_tables_array() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/tables.lua (array part)
        let code = r#"
z = {}
add(z, 1)
add(z, 7)
add(z, 'hello')
__print(z[1])
__print(z[2])
__print(z[3])
z[3] = 'world'
__print(z[3])
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "7", "hello", "world"]
        );
    }

    #[test]
    fn test_interpret_abstract_boolean_no_call() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/abstract_boolean_no_call.lua
        let code = r#"
v = __new_unknown_boolean()
if v then
  __print("a")
else
  __print("b")
end
if v then
  __print("c")
else
  __print("d")
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // With unknown boolean v, we should get 4 possible outcomes:
        // - v=true in both ifs: ["a", "c"]
        // - v=true then false: ["a", "d"]
        // - v=false then true: ["b", "c"]
        // - v=false then false: ["b", "d"]
        // However since v is the *same* unknown boolean, it could be either true or false
        // and the interpreter should explore both branches independently at each if statement.
        // This should result in 4 distinct output states.

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["a".to_string(), "c".to_string()],
            vec!["a".to_string(), "d".to_string()],
            vec!["b".to_string(), "c".to_string()],
            vec!["b".to_string(), "d".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_vectors() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/vector.lua
        let code = r#"
x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

y_values = {}
add(y_values, 2)
add(y_values, 4)
add(y_values, 6)
y = __new_vector(y_values)

function f(v)
  return v + 1
end

__print(x)
__print(x + y)
__print(x + 3)
__print(3 + x)
__print((x + 0.5) + (y + 0.5))
x = y
__print(x + y)
__print(f(x))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "V[6, 3, 8]",
                "V[8, 7, 14]",
                "V[9, 6, 11]",
                "V[9, 6, 11]",
                "V[9, 8, 15]",
                "V[4, 8, 12]",
                "V[3, 5, 7]",
            ]
        );
    }

    #[test]
    fn test_interpret_undefined_fields() {
        use crate::interpreter::glue::interpret_cfg;

        // Simplified undefined_fields.lua test
        let code = r#"
x = {}
__print(x.y)

x = {y='a', z=nil}
__print(x.y)
__print(x.z)
__print(x.a)
__print(x.a == nil)
__print(x.z == nil)
__print(x.a == x.z)

__print(global_which_does_not_exist)
__print(x.a == global_which_does_not_exist)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "nil",      // x.y (undefined)
                "a",        // x.y
                "nil",      // x.z (explicitly nil)
                "nil",      // x.a (undefined)
                "true",     // x.a == nil
                "true",     // x.z == nil
                "true",     // x.a == x.z (both nil)
                "nil",      // global_which_does_not_exist
                "true",     // x.a == global_which_does_not_exist
            ]
        );
    }

    #[test]
    fn test_interpret_short_circuit_operators_non_boolean() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/short_circuit_operators_non_boolean.lua
        // In Lua, `and` and `or` return the actual value (not a boolean):
        // - `a and b` returns `a` if `a` is falsy, otherwise returns `b`
        // - `a or b` returns `a` if `a` is truthy, otherwise returns `b`
        let code = r#"
function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", -4) or f("c", 0))
__print(f("a", false) and f("b", -4) or f("c", 0))
__print(f("a", 3) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", 0) or f("c", 4))

__print(f("a", -3) and f("b", 0))
__print(f("a", 0) and f("b", 0))
__print(f("a", 3) and f("b", 0))
__print(f("a", 0.3) and f("b", 0))
__print(f("a", "") and f("b", 0))
__print(f("a", " ") and f("b", 0))
__print(f("a", "a") and f("b", 0))
__print(f("a", nil) and f("b", 0))
__print(f("a", {}) and f("b", 0))
__print(f("a", f) and f("b", 0))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);

        // Expected behavior:
        // Line 6: f("a", true) returns true (truthy) -> eval f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 7: f("a", false) returns false (falsy) -> false (short-circuit and) -> or f("c", 0) returns 0
        //         prints: a, c, then 0
        // Line 8: f("a", 3) returns 3 (truthy) -> f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 9: f("a", 0) returns 0 (truthy in Lua!) -> f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 10: f("a", 0) returns 0 (truthy) -> f("b", 0) returns 0 (truthy) -> 0
        //          prints: a, b, then 0
        // Line 12: f("a", -3) returns -3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 13: f("a", 0) returns 0 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 14: f("a", 3) returns 3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 15: f("a", 0.3) returns 0.3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 16: f("a", "") returns "" (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 17: f("a", " ") returns " " (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 18: f("a", "a") returns "a" (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 19: f("a", nil) returns nil (falsy) -> nil (short-circuit, don't eval b)
        //          prints: a, then nil
        // Line 20: f("a", {}) returns {} (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 21: f("a", f) returns f (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0

        let expected = vec![
            "a", "b", "-4",   // line 6
            "a", "c", "0",    // line 7
            "a", "b", "-4",   // line 8
            "a", "b", "-4",   // line 9
            "a", "b", "0",    // line 10
            "a", "b", "0",    // line 12
            "a", "b", "0",    // line 13
            "a", "b", "0",    // line 14
            "a", "b", "0",    // line 15
            "a", "b", "0",    // line 16
            "a", "b", "0",    // line 17
            "a", "b", "0",    // line 18
            "a", "nil",       // line 19 (nil short-circuits)
            "a", "b", "0",    // line 20
            "a", "b", "0",    // line 21
        ];

        assert_eq!(result_states[0].0.prints, expected);
    }

    #[test]
    fn test_interpret_liveness_issue_for_in_function() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/liveness_issue_for_in_function.lua
        // The interpreter used to crash at "print" because the function call was
        // causing the outer locals to be lost. This only happened with functions that
        // have multiple cfg blocks.
        let code = r#"
function function_with_for()
  for i = 1, 7 do
  end
end

local y = {}
function_with_for()
__print(#y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(result_states[0].0.prints, vec!["0"]);
    }

    #[test]
    fn test_interpret_abstract_boolean_with_call() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/abstract_boolean.lua
        // Tests that abstract boolean branching creates 4 independent output states
        // when the same unknown boolean is used in two different if statements.
        let code = r#"
function f(v)
  if v then
    __print("a")
  else
    __print("b")
  end
  if v then
    __print("c")
  else
    __print("d")
  end
end

f(__new_unknown_boolean())
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // The expected outputs are:
        // - ["a", "c"] (v=true, v=true)
        // - ["a", "d"] (v=true, v=false)
        // - ["b", "c"] (v=false, v=true)
        // - ["b", "d"] (v=false, v=false)

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["a".to_string(), "c".to_string()],
            vec!["a".to_string(), "d".to_string()],
            vec!["b".to_string(), "c".to_string()],
            vec!["b".to_string(), "d".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_foreach() {
        use crate::interpreter::glue::interpret_cfg;

        // TODO(fidelity, foreach): OUR `foreach` IS NOT PICO-8's, AND THE
        // DIFFERENCE LOSES REAL TRANSITIONS. Ours advances the index
        // unconditionally; PICO-8's `foreach(t,f)` is `for v in all(t) do
        // f(v) end`, and `all` advances ONLY IF the element at the cursor is
        // still the one it just returned. So when an object destroys itself
        // during its own update, PICO-8 still visits the object that shifts
        // into its slot and we skip it. Three consequences, all real in this
        // cart: `load_room`'s foreach(objects,destroy_object) destroys only
        // indices 1,3,5,... so a death leaves stale objects behind (observed
        // in room (2,0)'s shape census); the player misses its creation-frame
        // update where player_spawn is last, which makes our room (0,0)=94
        // and (1,0)=100 PICO-8's 93 and 99 (a label - the input-frame counts
        // 66 and 76 agree either way); and a spring that shifts into a
        // collected fruit's slot loses an update, so a bounce PICO-8 would
        // deliver is absent from our reachable set - which is the unsound
        // direction, since it can refute an achievable horizon.
        //
        // Not fixed yet because it moves foreach_1's instruction numbering,
        // which 13 recipe entries address by %n, changes every row hash, and
        // can move rooms (0,0)/(1,0)'s absolute optima by one frame. The
        // cheap moment to do it is while those rooms need re-deriving anyway.
        //
        // TODO(fidelity, general): this is one instance of a class - our
        // builtins are reimplementations and have not been differentially
        // tested against real PICO-8. Same treatment wanted for the numerics
        // (fixed-point rounding, division, sin/cos tables, rnd).
        //
        // From lua_tests/foreach.lua - testing the foreach function implementation
        // foreach is implemented in Lua (builtin_level_3.lua):
        //   function foreach(tbl, func)
        //     for i=1,32767 do
        //       if #tbl < i then break end
        //       func(tbl[i])
        //     end
        //   end
        let code = r#"
function foreach(tbl, func)
  for i=1,32767 do
    if #tbl < i then
      break
    end
    func(tbl[i])
  end
end

function __assert(cond, msg)
  if not cond then
    __print("Assertion failed")
    if msg == nil then
      error()
    else
      __print(msg)
      error(msg)
    end
  end
end

function make_example_table()
  local x = {}
  add(x, 3)
  add(x, 1)
  add(x, 'walrus')
  return x
end

function test_normal_foreach()
  local x = make_example_table()

  local y = {}
  foreach(x, function(v)
    add(y, v)
  end)

  __assert(#y == 3)
  __assert(y[1] == 3)
  __assert(y[2] == 1)
  __assert(y[3] == 'walrus')
end

foreach(make_example_table(), __print)
test_normal_foreach()
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // foreach(make_example_table(), __print) should print: 3, 1, walrus
        assert_eq!(result_states[0].0.prints, vec!["3", "1", "walrus"]);
    }

    #[test]
    fn test_interpret_less_than_with_vectors() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/less_than.lua
        // Tests comparison operations on vectors
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(0 < 1.5)
__print(1 < 1.8)
__print(low < 1.5)
__print(1 < high)
__print(low < high)
__print((0 + 1) < 1.5)
__print((1 + 1) < 1.8)
__print((low + 1) < high)
__print(low)
__print(flr(high))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Expected output. Uniform vector results collapse to scalars at
        // construction (`MaybeVector::vector`), so an all-equal comparison
        // result is a plain "true"/"false"/"1" rather than "V[...]".
        // true        - 0 < 1.5
        // true        - 1 < 1.8
        // true        - V[0, 1] < 1.5 -> uniform, collapses to scalar true
        // true        - 1 < V[1.5, 1.8] -> uniform true
        // true        - V[0, 1] < V[1.5, 1.8] -> uniform true
        // true        - (0 + 1) < 1.5
        // false       - (1 + 1) < 1.8 (2 < 1.8 is false)
        // V[true, false] - (V[0, 1] + 1) < V[1.5, 1.8] -> V[1, 2] < V[1.5, 1.8] -> V[true, false]
        // V[0, 1]     - low unchanged
        // 1           - flr(V[1.5, 1.8]) -> uniform 1

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "true",
                "true",
                "true",
                "true",
                "true",
                "true",
                "false",
                "V[true, false]",
                "V[0, 1]",
                "1",
            ]
        );
    }

    #[test]
    fn test_interpret_vector_branch() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/vector_branch.lua
        // Tests vector branching - when branching on a vector comparison,
        // the vector gets filtered to only include elements matching the branch condition
        let code = r#"
x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

__print(x)
if x < 5 then
  __print(x)
else
  __print(x)
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Expected output options:
        // Option 1: x < 5 branch (only element 3 is < 5)
        //   V[6, 3, 8]
        //   3
        // Option 2: x >= 5 branch (elements 6 and 8 are >= 5)
        //   V[6, 3, 8]
        //   V[6, 8]

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["V[6, 3, 8]".to_string(), "3".to_string()],
            vec!["V[6, 3, 8]".to_string(), "V[6, 8]".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_if_vector() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/if_vector.lua
        // Tests if statements with vector comparisons that cause branching
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(low)
if (low + 1) < high then
  __print("below")
  __print(low)
end
__print(low)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Expected output options:
        // Option 1: (low + 1) < high is true for element 0 (0+1=1 < 1.5)
        //   V[0, 1]
        //   below
        //   0
        //   0
        // Option 2: (low + 1) < high is false for element 1 (1+1=2 < 1.8 is false)
        //   V[0, 1]
        //   1

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec![
                "V[0, 1]".to_string(),
                "below".to_string(),
                "0".to_string(),
                "0".to_string(),
            ],
            vec!["V[0, 1]".to_string(), "1".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_branching_with_irrelevant_locals() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_with_irrelevant_locals.lua
        // Both branches produce the same output ("a"), so they should merge to 1 state
        let code = r#"
if __new_unknown_boolean() then
  __print("a")
else
  local s = "a"
  __print(s)
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Both branches print "a", so there should be 1 branch with prints ["a"]
        // Note: deduplication might not happen in current implementation,
        // but both outputs should still be ["a"]
        for (state, _) in &result_states {
            assert_eq!(state.prints, vec!["a"]);
        }
    }

    #[test]
    fn test_interpret_branching_with_allocations() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_with_allocations.lua
        // Both branches create tables and set same values, should produce equivalent outputs
        let code = r#"
if __new_unknown_boolean() then
  x = {}
  x[1] = "a"
  y = {}
  y[1] = "b"
else
  y = {}
  y[1] = "b"
  x = {}
  x[1] = "a"
end
__print(x[1])
__print(y[1])
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Both branches should produce ["a", "b"]
        for (state, _) in &result_states {
            assert_eq!(state.prints, vec!["a", "b"]);
        }
    }

    #[test]
    fn test_interpret_branching_into_return() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_into_return.lua
        // Both branches return, so there should be 1 equivalent state
        let code = r#"
if __new_unknown_boolean() then
  local x = 1
  return
else
  local x = 2
  return
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Both branches end with return, and both have no prints
        for (state, _) in &result_states {
            assert_eq!(state.prints, Vec::<String>::new());
        }
    }

    #[test]
    fn test_load_and_compile_celeste_game() {
        // Test that we can parse and compile the celeste-minimal.lua game
        let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game = std::fs::read_to_string("lua/celeste-minimal.lua")
            .expect("Failed to read celeste-minimal.lua");

        // Suffix code to call _init
        let suffix = r#"
_init()
__reset_button_states()
"#;

        let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, suffix);

        let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

        println!("Game compiled successfully!");
        println!("  CFG entry instructions: {}", cfg.entry.instructions.len());
        println!("  Function definitions: {}", fun_defs.len());

        // Just test that compilation works for now
        assert!(!fun_defs.is_empty(), "Game should have function definitions");
    }

    #[test]
    fn test_run_celeste_game_init() {
        use crate::interpreter::glue::interpret_cfg;

        // Load and compile the game
        let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game = std::fs::read_to_string("lua/celeste-minimal.lua")
            .expect("Failed to read celeste-minimal.lua");

        // Suffix code to call _init
        let suffix = r#"
_init()
__reset_button_states()
"#;

        let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, suffix);

        let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

        let mut fixed_env = create_fixed_env_with_game_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        println!("Starting game interpretation...");
        let start = std::time::Instant::now();
        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");
        let elapsed = start.elapsed();

        println!("Game init completed in {:?}", elapsed);
        println!("Result states: {}", result_states.len());

        // The game should produce at least one state
        assert!(!result_states.is_empty(), "Game should produce at least one state");
    }

    #[test]
    fn test_run_celeste_game_frame() {
        // Run 26 frames (enough to see player spawn at frame 25)
        // For longer runs, use the binary: cargo run -- -n 30
        run_game_frames(26, 25, 26, None, None, None, None, 1, false).expect("Game frames should complete");
    }

    #[test]
    fn test_obj_move_basic() {
        use crate::interpreter::glue::interpret_cfg;

        // Test that obj.move correctly updates position
        let code = std::fs::read_to_string("lua_tests/obj_move_test.lua")
            .expect("Failed to read obj_move_test.lua");

        let ast = full_moon::parse(&code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        println!("obj_move_test output:");
        for line in &result_states[0].0.prints {
            println!("  {}", line);
        }

        // Verify that x changed after move(2, 0)
        // Test 1 output should be: "Test 1", "8", "10"
        let prints = &result_states[0].0.prints;
        // After move(2, 0), x should be 10 (8 + 2)
        assert!(prints.contains(&"10".to_string()),
            "Expected position to change to 10 after move(2,0), got: {:?}", prints);
    }

    #[test]
    fn test_foreach_move() {
        use crate::interpreter::glue::interpret_cfg;

        // Test foreach with objects, simulating _update loop
        let code = std::fs::read_to_string("lua_tests/foreach_move_test.lua")
            .expect("Failed to read foreach_move_test.lua");

        let ast = full_moon::parse(&code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        println!("foreach_move_test output:");
        for line in &result_states[0].0.prints {
            println!("  {}", line);
        }

        let prints = &result_states[0].0.prints;
        // Test 1: 8 -> 10
        // Test 2: 8 -> 8 (no move)
        // Test 3: 8 -> 6
        // Test 4: 10 and 9
        assert!(prints.contains(&"10".to_string()), "Test 1 failed: {:?}", prints);
        assert!(prints.contains(&"6".to_string()), "Test 3 failed: {:?}", prints);
        assert!(prints.contains(&"9".to_string()), "Test 4 failed: {:?}", prints);
    }

    #[test]
    fn test_move_y() {
        use crate::interpreter::glue::interpret_cfg;

        let code = std::fs::read_to_string("lua_tests/move_y_test.lua")
            .expect("Failed to read move_y_test.lua");

        let ast = full_moon::parse(&code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        println!("move_y_test output:");
        for line in &result_states[0].0.prints {
            println!("  {}", line);
        }

        let prints = &result_states[0].0.prints;
        // Y should change from 112 to 110 after move(0, -2)
        assert!(prints.iter().any(|s| s.contains("110")),
            "Expected y to be 110 after move with spd.y=-2, got: {:?}", prints);
    }

    #[test]
    fn test_jump_trajectory() {
        use crate::interpreter::glue::interpret_cfg;

        let code = std::fs::read_to_string("lua_tests/jump_trajectory_test.lua")
            .expect("Failed to read jump_trajectory_test.lua");

        let ast = full_moon::parse(&code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        println!("jump_trajectory_test output:");
        for line in &result_states[0].0.prints {
            println!("  {}", line);
        }

        // After jumping for 5 frames with spd_y=-2, y should be 104
        // Frame 1: y=112 (no move yet), Frame 2: y=110, Frame 3: y=108, Frame 4: y=106, Frame 5: y=104
        let prints = &result_states[0].0.prints;
        assert!(prints.iter().any(|s| s.contains("y=104")),
            "Expected y to reach 104 after jumping, got: {:?}", prints);
    }

    #[test]
    fn test_hint_normalize_merges_states() {
        use crate::interpreter::glue::interpret_cfg;

        // Code that:
        // 1. Uses __new_unknown_boolean() which returns unknown bool, causing state split
        // 2. Sets different values in each branch
        // 3. Uses _hint_normalize() to merge states back
        // 4. After merge, should have 1 vectorized state instead of 2 scalar states
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            __print(x)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // With hint_normalize, states should be merged
        // We should have 1 state with vector_size=2, not 2 states with vector_size=1
        assert_eq!(result_states.len(), 1, "Expected states to be merged by hint_normalize");
        assert_eq!(result_states[0].0.vector_size, 2, "Expected vectorized state with size 2");
    }

    #[test]
    fn test_without_hint_normalize_has_multiple_states() {
        use crate::interpreter::glue::interpret_cfg;

        // Same code but WITHOUT _hint_normalize()
        // Should result in 2 separate states
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            __print(x)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Without hint_normalize, we should have 2 separate states
        assert_eq!(result_states.len(), 2, "Expected 2 separate states without hint_normalize");
    }

    #[test]
    fn test_hint_normalize_nested_conditionals() {
        use crate::interpreter::glue::interpret_cfg;

        // This test checks nested conditionals with hint_normalize
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            -- Now we have 1 state with vector_size=2

            local y = 0
            if __new_unknown_boolean() then
                y = 1
            else
                y = 2
            end
            _hint_normalize()
            __print(x)
            __print(y)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // We should end up with merged states
        assert!(!result_states.is_empty(), "Expected at least one result state");
    }

    #[test]
    fn test_hint_normalize_vectorized_then_split_by_vector_condition() {
        use crate::interpreter::glue::interpret_cfg;

        // This test checks that vectorized states split by vector conditions work correctly
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            -- Now we have 1 state with vector_size=2, x is Vector[1, 2]

            local y = 0
            -- This condition is based on vectorized x, so it's a Bool vector
            if x == 1 then
                y = 10
            else
                y = 20
            end
            _hint_normalize()
            __print(y)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should complete without vector length mismatches
        assert!(!result_states.is_empty(), "Expected at least one result state");
    }

    #[test]
    fn test_hint_normalize_with_function_splitting_vectorized_state() {
        use crate::interpreter::glue::interpret_cfg;

        // This test simulates what happens in the game:
        // 1. A vectorized state (from previous hint_normalize) enters a function
        // 2. The function has conditionals on __new_unknown_boolean()
        // 3. This creates NEW scalar UnknownBool conditions in a vectorized state
        // 4. The UnknownBool splits the state into 2, each with the SAME vector_size
        // 5. When these flow to hint_normalize, they should merge correctly
        let code = r#"
            function maybe_modify(val)
                if __new_unknown_boolean() then
                    return val + 1
                else
                    return val
                end
            end

            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            -- Now we have 1 state with vector_size=2

            -- Call function on vectorized state - this should split into 2 states
            -- each with vector_size=2, then hint_normalize should merge them
            local y = maybe_modify(x)
            _hint_normalize()
            __print(y)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should complete without vector length mismatches
        assert!(!result_states.is_empty(), "Expected at least one result state");
        for (i, (state, _)) in result_states.iter().enumerate() {
            println!("State {}: vector_size={}", i, state.vector_size);
        }
    }

    #[test]
    fn test_function_filters_vectorized_state() {
        use crate::interpreter::glue::interpret_cfg;

        // This test checks a tricky case:
        // 1. Caller has vectorized state (vector_size=2) with x = Vector[1, 2]
        // 2. Caller calls function f(x)
        // 3. Inside function, we branch on the vector condition (x < 2)
        // 4. This FILTERS the state - some lanes are removed
        // 5. Function returns with different vector_size than it was called with
        // 6. What happens to caller's local variables?
        let code = r#"
            function filter_by_arg(v)
                if v < 2 then
                    return v + 10
                else
                    return v + 20
                end
            end

            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            -- Now we have 1 state with vector_size=2, x = Vector[1, 2]

            local y = 100  -- This is a scalar in a vectorized state

            -- Call function - inside it branches on x, filtering the vector
            local z = filter_by_arg(x)

            -- After the call:
            -- - For x=1 branch: z = 11, vector_size should be 1
            -- - For x=2 branch: z = 22, vector_size should be 1
            -- What is y in each state? It should be 100 (scalar)

            __print(y)
            __print(z)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        println!("Result states: {}", result_states.len());
        for (i, (state, _)) in result_states.iter().enumerate() {
            println!("State {}: vector_size={}, prints={:?}", i, state.vector_size, state.prints);
        }

        // We should get 2 states, each with y=100 and z in {11, 22}
        assert_eq!(result_states.len(), 2);
    }

    #[test]
    fn test_function_filters_vectorized_state_with_vector_caller_var() {
        use crate::interpreter::glue::interpret_cfg;

        // This test checks the problematic case you identified:
        // 1. Caller has vectorized state (vector_size=2) with x = Vector[1, 2]
        // 2. Caller ALSO has another vector y = Vector[10, 20] (same length as vector_size)
        // 3. Caller calls function f(x)
        // 4. Inside function, we branch on the vector condition (x < 2)
        // 5. This FILTERS the state - vector_size goes from 2 to 1
        // 6. What happens to caller's y? It still has length 2 but vector_size is now 1!
        let code = r#"
            function filter_by_arg(v)
                if v < 2 then
                    return v + 10
                else
                    return v + 20
                end
            end

            local x = 0
            local y = 0
            if __new_unknown_boolean() then
                x = 1
                y = 10
            else
                x = 2
                y = 20
            end
            _hint_normalize()
            -- Now we have 1 state with vector_size=2
            -- x = Vector[1, 2]
            -- y = Vector[10, 20]

            -- Call function - inside it branches on x, filtering the vector
            local z = filter_by_arg(x)

            -- After the call:
            -- - For x=1 branch: z = 11, vector_size = 1
            --   But caller's y was Vector[10, 20] with length 2!
            --   What should y be? It should be filtered to match: y = 10
            -- - For x=2 branch: z = 22, vector_size = 1
            --   y should be filtered to: y = 20

            __print(y)
            __print(z)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        println!("Result states: {}", result_states.len());
        for (i, (state, _)) in result_states.iter().enumerate() {
            println!("State {}: vector_size={}, prints={:?}", i, state.vector_size, state.prints);
        }

        // We should get 2 states with correct filtered y values
        assert_eq!(result_states.len(), 2);
        // Check that y was correctly filtered
        let mut print_sets: Vec<_> = result_states.iter()
            .map(|(s, _)| s.prints.clone())
            .collect();
        print_sets.sort();
        // y=10 with z=11, and y=20 with z=22
        assert!(print_sets.contains(&vec!["10".to_string(), "11".to_string()]) ||
                print_sets.contains(&vec!["20".to_string(), "22".to_string()]),
                "Expected y to be filtered correctly, got: {:?}", print_sets);
    }

    #[test]
    fn test_hint_normalize_deduplicates_identical_states() {
        use crate::interpreter::glue::interpret_cfg;

        // This test verifies that when the same state reaches a hint_normalize block
        // via multiple paths, it's properly deduplicated.
        //
        // The code creates a diamond pattern:
        //   - Branch on unknown boolean
        //   - Both branches set x = 42 (same value!)
        //   - Both branches converge at hint_normalize
        //
        // Without proper union_diff:
        //   - We'd get TWO identical states (x=42, x=42)
        //   - After vectorization, we'd have one state with vector_size=2 containing [42, 42]
        //
        // With proper union_diff:
        //   - The second arrival of x=42 should be recognized as "already seen"
        //   - We should have one state with vector_size=1 containing just 42
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 42
            else
                x = 42
            end
            _hint_normalize()
            __print(x)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        println!("Result states: {}", result_states.len());
        for (i, (state, _)) in result_states.iter().enumerate() {
            println!("State {}: vector_size={}, prints={:?}", i, state.vector_size, state.prints);
        }

        // One state after the merge - but TWO lanes, not one, since the
        // branch-on-UnknownBool EDGE REFINEMENT (flow.rs; plans/
        // spd-rung.md round 2): each arm learns its condition value
        // (true/false), so the arm states differ in that live local
        // until it dies. In the real pipeline the recipe's liveness
        // kills retire the condition before any merge (verified
        // byte-identical at room (1,0) f30); this toy has no kills, so
        // the refined local is visible here by design.
        assert_eq!(result_states.len(), 1, "Expected 1 merged state");
        assert_eq!(result_states[0].0.vector_size, 2,
                   "the refined condition local keeps the arm lanes distinct in this kill-less toy");
        assert_eq!(result_states[0].0.prints, vec!["42"]);
    }

    #[test]
    fn test_hint_normalize_partial_deduplication() {
        use crate::interpreter::glue::interpret_cfg;

        // This test verifies partial deduplication: when some states are new and some are duplicates.
        //
        // The code:
        //   - First split: x = 1 or x = 2
        //   - hint_normalize (merges to vector_size=2)
        //   - Second split: y = 10 or y = 10 (same value in both branches!)
        //   - hint_normalize again
        //
        // After the second hint_normalize:
        //   - We should NOT have 4 states (2 x values * 2 y values)
        //   - We should have 2 states (2 x values * 1 y value since y is deduplicated)
        let code = r#"
            local x = 0
            if __new_unknown_boolean() then
                x = 1
            else
                x = 2
            end
            _hint_normalize()
            -- Now: 1 state, vector_size=2, x = [1, 2]

            local y = 0
            if __new_unknown_boolean() then
                y = 10
            else
                y = 10
            end
            _hint_normalize()
            -- Without dedup: 2 states, each with vector_size=2
            -- With dedup: 1 state, vector_size=2, x = [1, 2], y = 10

            __print(x)
            __print(y)
        "#;

        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        println!("Result states: {}", result_states.len());
        for (i, (state, _)) in result_states.iter().enumerate() {
            println!("State {}: vector_size={}, prints={:?}", i, state.vector_size, state.prints);
        }

        // One merged state; FOUR lanes, not two: both diamonds' branch
        // conditions are edge-refined (flow.rs), so the second diamond's
        // arms stay distinct per condition value in this kill-less toy
        // (2 x-values x 2 refined y-conditions). See the comment in
        // test_hint_normalize_deduplicates_identical_states; the real
        // pipeline's liveness kills make this invisible (verified
        // byte-identical at room (1,0) f30).
        assert_eq!(result_states.len(), 1, "Expected 1 merged state");
        assert_eq!(result_states[0].0.vector_size, 4,
                   "2 x-values x 2 refined condition values in this kill-less toy");
    }
}
