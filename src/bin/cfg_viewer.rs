//! Binary to analyze CFGs from the Celeste game and generate data for the web viewer.
//!
//! Usage:
//!   cargo run --release --bin cfg_viewer
//!
//! This will:
//! 1. Load and compile the Celeste game
//! 2. Analyze each function's CFG
//! 3. Run optimization pipeline (mem2reg, heap elimination)
//! 4. Generate JSON data for the web viewer
//! 5. Output to serve/cfg_analysis.json

use std::fs;

use anyhow::Result;
use clap::Parser;

use celeste_rust::frontend;
use celeste_rust::interpreter::cfg_analysis::{
    analyze_cfg, run_optimization_pipeline, CfgTestCase, CfgTestCases, HeapEliminationStatus,
    Mem2RegStatus, SerializableCfg,
};

#[derive(Parser, Debug)]
#[command(name = "cfg_viewer")]
#[command(about = "Analyze Celeste CFGs and generate viewer data")]
struct Args {
    /// Output JSON file path
    #[arg(short, long, default_value = "serve/cfg_analysis.json")]
    output: String,

    /// Filter functions by name prefix (empty = all)
    #[arg(short, long, default_value = "")]
    filter: String,

    /// Print detailed analysis to stdout
    #[arg(short, long)]
    verbose: bool,

    /// Only analyze functions with heap operations
    #[arg(long)]
    heap_only: bool,

    /// Only analyze functions that are "pure" (no side effects)
    #[arg(long)]
    pure_only: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Load the game source
    println!("Loading Celeste source...");
    let level_3 = fs::read_to_string("lua/builtin_level_3.lua")?;
    let level_4 = fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = fs::read_to_string("lua/celeste-minimal.lua")?;

    let full_code = format!("{}\n{}\n{}", level_3, level_4, game);

    println!("Parsing...");
    let ast = full_moon::parse(&full_code)?;

    println!("Compiling...");
    let (_main_cfg, fun_defs) = frontend::compile(&ast)?;

    println!("Found {} function definitions", fun_defs.len());

    // Analyze each function
    let mut test_cases = CfgTestCases::new();
    let mut stats = AnalysisStats::default();

    for fun_def in &fun_defs {
        let name = fun_def.name.as_str();

        // Apply filter
        if !args.filter.is_empty() && !name.contains(&args.filter) {
            continue;
        }

        // Analyze the CFG
        let analysis = analyze_cfg(&fun_def.cfg);

        // Apply filters
        if args.heap_only && analysis.is_heap_free() {
            continue;
        }
        if args.pure_only && !analysis.is_pure() {
            continue;
        }

        // Update stats
        stats.total += 1;
        if analysis.is_heap_free() {
            stats.heap_free += 1;
        }
        if analysis.is_pure() {
            stats.pure += 1;
        }
        if analysis.has_calls {
            stats.has_calls += 1;
        }
        if analysis.reads_heap {
            stats.reads_heap += 1;
        }
        if analysis.writes_heap {
            stats.writes_heap += 1;
        }
        if analysis.modifies_heap_shape {
            stats.modifies_heap_shape += 1;
        }

        // Run optimization pipeline
        let (opt_result, after_mem2reg, after_heap_elim) =
            run_optimization_pipeline(&fun_def.cfg, &analysis);

        // Update analysis with heap elimination status for legacy compatibility
        let mut analysis = analysis;
        analysis.heap_elimination_status = opt_result.heap_elimination.clone();

        // Track stats
        match &opt_result.mem2reg {
            Mem2RegStatus::Success { cells_promoted } => {
                stats.mem2reg_success += 1;
                stats.cells_promoted += cells_promoted;
            }
            Mem2RegStatus::Partial { cells_promoted, .. } => {
                stats.mem2reg_partial += 1;
                stats.cells_promoted += cells_promoted;
            }
            _ => {}
        }
        match &opt_result.heap_elimination {
            HeapEliminationStatus::Success { .. } => stats.heap_elim_success += 1,
            HeapEliminationStatus::Failed(_) => stats.heap_elim_failed += 1,
            _ => {}
        }

        if args.verbose {
            println!("\n=== {} ===", name);
            println!("  Blocks: {}", analysis.instruction_counts.total_blocks);
            println!(
                "  Instructions: {}",
                analysis.instruction_counts.total_instructions
            );
            println!("  Reads heap: {}", analysis.reads_heap);
            println!("  Writes heap: {}", analysis.writes_heap);
            println!("  Modifies heap shape: {}", analysis.modifies_heap_shape);
            println!("  Has calls: {}", analysis.has_calls);
            println!("  Is heap-free: {}", analysis.is_heap_free());
            println!("  Is pure: {}", analysis.is_pure());
            println!("  Local cells: {}", analysis.local_only_allocs);
            println!("  mem2reg: {:?}", opt_result.mem2reg);
            println!("  Heap elimination: {:?}", opt_result.heap_elimination);
            if !analysis.accessed_globals.is_empty() {
                println!("  Globals: {:?}", analysis.accessed_globals);
            }
        }

        // Create serializable CFGs
        let original_cfg: SerializableCfg = (&fun_def.cfg).into();
        let after_mem2reg_cfg = after_mem2reg.as_ref().map(|c| c.into());
        let after_heap_elim_cfg = after_heap_elim.as_ref().map(|c| c.into());

        // Determine final optimized CFG
        let optimized_cfg = after_heap_elim_cfg
            .clone()
            .or_else(|| after_mem2reg_cfg.clone())
            .unwrap_or_else(|| original_cfg.clone());

        // Get source span if available
        let source_span = fun_def.source_span.as_ref().map(|s| (s.start.line, s.end.line));

        let test_case = CfgTestCase {
            name: name.to_string(),
            original_cfg,
            after_mem2reg: after_mem2reg_cfg,
            after_heap_elim: after_heap_elim_cfg,
            optimized_cfg,
            analysis,
            optimization_result: opt_result,
            reference_shape: None,
            source_span,
        };

        test_cases.add_case(test_case);
    }

    // Print summary stats
    println!("\n=== Summary ===");
    println!("Total functions analyzed: {}", stats.total);
    println!("  Heap-free: {} ({:.1}%)", stats.heap_free, pct(stats.heap_free, stats.total));
    println!("  Pure: {} ({:.1}%)", stats.pure, pct(stats.pure, stats.total));
    println!("  Has calls: {} ({:.1}%)", stats.has_calls, pct(stats.has_calls, stats.total));
    println!("  Reads heap: {} ({:.1}%)", stats.reads_heap, pct(stats.reads_heap, stats.total));
    println!("  Writes heap: {} ({:.1}%)", stats.writes_heap, pct(stats.writes_heap, stats.total));
    println!("  Modifies heap shape: {} ({:.1}%)", stats.modifies_heap_shape, pct(stats.modifies_heap_shape, stats.total));

    println!("\n=== mem2reg (local cell elimination) ===");
    println!("  Success: {} ({:.1}%)", stats.mem2reg_success, pct(stats.mem2reg_success, stats.total));
    println!("  Partial: {} ({:.1}%)", stats.mem2reg_partial, pct(stats.mem2reg_partial, stats.total));
    println!("  Cells promoted: {}", stats.cells_promoted);

    println!("\n=== Heap Elimination (globals) ===");
    println!("  Success: {} ({:.1}%)", stats.heap_elim_success, pct(stats.heap_elim_success, stats.total));
    println!("  Failed: {} ({:.1}%)", stats.heap_elim_failed, pct(stats.heap_elim_failed, stats.total));

    // Write output
    println!("\nWriting to {}...", args.output);
    let json = serde_json::to_string_pretty(&test_cases)?;
    fs::write(&args.output, json)?;
    println!("Done! {} test cases written.", test_cases.test_cases.len());

    Ok(())
}

fn pct(n: usize, total: usize) -> f64 {
    if total == 0 { 0.0 } else { 100.0 * n as f64 / total as f64 }
}

#[derive(Default)]
struct AnalysisStats {
    total: usize,
    heap_free: usize,
    pure: usize,
    has_calls: usize,
    reads_heap: usize,
    writes_heap: usize,
    modifies_heap_shape: usize,
    mem2reg_success: usize,
    mem2reg_partial: usize,
    cells_promoted: usize,
    heap_elim_success: usize,
    heap_elim_failed: usize,
}
