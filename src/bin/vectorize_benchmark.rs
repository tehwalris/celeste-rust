use celeste_rust::interpreter::vectorize::{vectorize_states, get_last_vectorize_stats};
use celeste_rust::interpreter::state::State;
use std::collections::HashMap;
use std::time::Instant;

/// A cheap "coarse shape" that can be computed in O(1) per state
/// States with different coarse shapes can NEVER merge
///
/// IMPORTANT: Only use fields that are NOT affected by normalization!
/// - heap.len() - never changes ✓
/// - outer_local_envs.len() - never changes ✓
/// - local_env.len() - CHANGES with clean_local_envs_for_merging ✗
/// - vector_size - states with different sizes CAN merge ✗
type CoarseShape = (usize, usize); // (heap_len, outer_envs_len) - ONLY safe fields!

fn coarse_shape_of_state(state: &State) -> CoarseShape {
    (
        state.heap.len(),
        state.outer_local_envs.len(),
    )
}

fn main() {
    // Load the wasted states
    let wasted_path = "profiles/frame28_analysis/wasted_before_2.json";
    let wasted_json = std::fs::read_to_string(wasted_path)
        .expect("Failed to read wasted states");
    let wasted_states: Vec<State> = serde_json::from_str(&wasted_json)
        .expect("Failed to parse wasted states");

    println!("Loaded {} wasted states", wasted_states.len());
    println!();

    // Analyze coarse shapes
    println!("=== Coarse Shape Analysis ===");
    let mut coarse_groups: HashMap<CoarseShape, usize> = HashMap::new();
    for state in &wasted_states {
        let shape = coarse_shape_of_state(state);
        *coarse_groups.entry(shape).or_insert(0) += 1;
    }
    println!("Unique coarse shapes: {}", coarse_groups.len());
    let singletons = coarse_groups.values().filter(|&&c| c == 1).count();
    let mergeable_groups = coarse_groups.values().filter(|&&c| c > 1).count();
    let states_in_mergeable = coarse_groups.values().filter(|&&c| c > 1).sum::<usize>();
    println!("Singleton groups (can't merge): {}", singletons);
    println!("Groups with 2+ states (might merge): {} ({} states)", mergeable_groups, states_in_mergeable);
    println!();

    // Benchmark coarse shape computation
    let coarse_iterations = 10000;
    let coarse_start = Instant::now();
    for _ in 0..coarse_iterations {
        for state in &wasted_states {
            std::hint::black_box(coarse_shape_of_state(state));
        }
    }
    let coarse_elapsed = coarse_start.elapsed();
    let coarse_per_iter = coarse_elapsed / coarse_iterations;
    println!("=== Coarse Shape Benchmark ===");
    println!("Coarse shape for {} states: {:?} per iteration", wasted_states.len(), coarse_per_iter);
    println!("Time per state: {:?}", coarse_per_iter / wasted_states.len() as u32);
    println!();

    // Run once to get detailed stats
    let _result = vectorize_states(wasted_states.clone());
    if let Some(stats) = get_last_vectorize_stats() {
        println!("=== Full Vectorize Timing Breakdown ===");
        println!("Input count: {}", stats.input_count);
        println!("Output count: {}", stats.output_count);
        println!("Group count: {}", stats.group_count);
        println!();
        println!("Input validation:  {:.3}ms", stats.input_validation_ns as f64 / 1_000_000.0);
        println!("Clean local_envs:  {:.3}ms", stats.clean_local_envs_ns as f64 / 1_000_000.0);
        println!("Shape grouping:    {:.3}ms", stats.shape_grouping_ns as f64 / 1_000_000.0);
        println!("Vectorize groups:  {:.3}ms", stats.vectorize_groups_ns as f64 / 1_000_000.0);
        println!("Output validation: {:.3}ms", stats.output_validation_ns as f64 / 1_000_000.0);
        let total_ns = stats.input_validation_ns + stats.clean_local_envs_ns +
                       stats.shape_grouping_ns + stats.vectorize_groups_ns + stats.output_validation_ns;
        println!("Total measured:    {:.3}ms", total_ns as f64 / 1_000_000.0);
    }
    println!();

    // Warm up
    for _ in 0..3 {
        let _ = vectorize_states(wasted_states.clone());
    }

    // Benchmark full vectorize
    let iterations = 100;
    let start = Instant::now();
    for _ in 0..iterations {
        let result = vectorize_states(wasted_states.clone());
        std::hint::black_box(result);
    }
    let elapsed = start.elapsed();
    let per_iter = elapsed / iterations;

    println!("=== Full Vectorize Benchmark ===");
    println!("Vectorize {} states: {:?} per iteration", wasted_states.len(), per_iter);
    println!("Total time for {} iterations: {:?}", iterations, elapsed);
    println!("Time per state: {:?}", per_iter / wasted_states.len() as u32);
    println!();

    // Compare: what if we used coarse pre-filter?
    println!("=== Potential Savings ===");
    let coarse_ns = coarse_per_iter.as_nanos() as f64;
    let full_ns = per_iter.as_nanos() as f64;
    println!("Coarse check: {:.3}ms", coarse_ns / 1_000_000.0);
    println!("Full vectorize: {:.3}ms", full_ns / 1_000_000.0);
    println!("Ratio: {:.1}x faster with coarse check", full_ns / coarse_ns);
    println!();
    println!("If {} of {} states are singletons after coarse check,", singletons, wasted_states.len());
    println!("we'd avoid computing full shapes for {} states ({:.1}% savings)",
        singletons, 100.0 * singletons as f64 / wasted_states.len() as f64);
}
