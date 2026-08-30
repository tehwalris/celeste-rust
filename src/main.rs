// Use mimalloc as the global allocator for better performance
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use anyhow::Result;
use clap::Parser;

use celeste_rust::interpreter;

#[derive(Parser, Debug)]
#[command(name = "celeste-rust")]
#[command(about = "Abstract interpreter for PICO-8 Celeste")]
struct Args {
    /// Number of frames to run
    #[arg(short = 'n', long, default_value_t = 30)]
    frames: u32,

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
    use crate::interpreter::abstraction::make_state_abstract;
    use crate::interpreter::inspect::{create_frame_dump, write_frame_dump_jsonl, dump_states_to_file, save_checkpoint, load_checkpoint, checkpoint_filename, Checkpoint};
    use std::io::BufWriter;
    use std::fs::File;


    // Create checkpoint directory if needed
    if let Some(dir) = checkpoint_dir {
        std::fs::create_dir_all(dir)?;
    }

    // Single-lane-per-fork execution via the REFERENCE interpreter
    // (`RefEngine`, the AST oracle that replaced the CFG interpreter). This
    // legacy runner is unvectorized now, so it is far slower than the
    // compiled `rewrite search`; it is kept for its dump/checkpoint
    // diagnostics.
    let mut refeng = celeste_rust::trace::refengine::RefEngine::new()
        .expect("Failed to build the reference engine");

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
                let start = std::time::Instant::now();
                let init_result_states: Vec<crate::interpreter::state::State> =
                    vec![refeng.initial_state().expect("Init failed")];
                println!("Game init completed in {:?}", start.elapsed());
                println!("States after init: {}", init_result_states.len());
                (init_result_states, 1)
            }
        }
    } else {
        println!("Running game init...");
        let start = std::time::Instant::now();
        let init_result_states: Vec<crate::interpreter::state::State> =
            vec![refeng.initial_state().expect("Init failed")];
        println!("Game init completed in {:?}", start.elapsed());
        println!("States after init: {}", init_result_states.len());
        (init_result_states, 1)
    };

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
            let result = refeng.run_frame(&state).expect("Frame interpretation failed");
            new_states.extend(result);
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
