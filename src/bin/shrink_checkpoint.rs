//! Tool to shrink checkpoint states by randomly filtering vector elements.
//! Usage: shrink_checkpoint <input_checkpoint.jsonl.zst> <output_checkpoint.jsonl.zst> <keep_fraction>

use celeste_rust::interpreter::inspect::{load_checkpoint, save_checkpoint, Checkpoint};
use rand::prelude::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 4 {
        eprintln!("Usage: {} <input.jsonl.zst> <output.jsonl.zst> <keep_fraction>", args[0]);
        eprintln!("Example: {} checkpoint_frame0036.jsonl.zst checkpoint_frame0036_half.jsonl.zst 0.5", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let output_path = &args[2];
    let keep_fraction: f64 = args[3].parse().expect("keep_fraction must be a number between 0 and 1");

    if keep_fraction <= 0.0 || keep_fraction > 1.0 {
        eprintln!("keep_fraction must be between 0 and 1");
        std::process::exit(1);
    }

    println!("Loading checkpoint from {}...", input_path);
    let checkpoint = load_checkpoint(input_path).expect("Failed to load checkpoint");
    println!("Loaded {} states", checkpoint.states.len());

    let mut rng = rand::thread_rng();
    let mut new_states = Vec::with_capacity(checkpoint.states.len());
    let mut total_original = 0usize;
    let mut total_filtered = 0usize;

    for state in checkpoint.states {
        let original_size = state.vector_size;
        total_original += original_size;

        // Create a random mask that keeps approximately keep_fraction of elements
        let mask: Vec<bool> = (0..original_size)
            .map(|_| rng.gen::<f64>() < keep_fraction)
            .collect();

        // Make sure we keep at least 1 element
        let kept_count = mask.iter().filter(|&&b| b).count();
        let mask = if kept_count == 0 {
            // Force keep the first element
            let mut m = mask;
            m[0] = true;
            m
        } else {
            mask
        };

        let new_state = state.filter_by_mask(&mask);
        total_filtered += new_state.vector_size;
        new_states.push(new_state);
    }

    println!("Filtered: {} -> {} elements ({:.1}% kept)",
        total_original, total_filtered,
        100.0 * total_filtered as f64 / total_original as f64);

    let new_checkpoint = Checkpoint {
        frame: checkpoint.frame,
        states: new_states,
    };

    println!("Saving to {}...", output_path);
    save_checkpoint(&new_checkpoint, output_path).expect("Failed to save checkpoint");
    println!("Done!");
}
