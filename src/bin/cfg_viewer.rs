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
    analyze_cfg, optimize_all_functions, run_optimization_pipeline_with_interprocedural,
    BlockCoalesceStatus, CfgTestCase, CfgTestCases, DceStatus, HeapEliminationStatus, Mem2RegStatus,
    CallResolutionStatus, InliningStatus, SerializableCfg,
};
use celeste_rust::interpreter::call_resolution::build_global_closure_map_from_fun_defs;

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

    // Build global closure map for call resolution
    let global_closure_map = build_global_closure_map_from_fun_defs(fun_defs.iter());

    // Pre-optimize all functions in dependency order (callees before callers)
    // This ensures that when we inline, we use the already-optimized version of callees
    println!("Optimizing all functions in dependency order...");
    let fun_def_refs: Vec<_> = fun_defs.iter().collect();
    let _optimized_funs = optimize_all_functions(&fun_def_refs, &global_closure_map);

    // Build optimized function definition map for inlining
    // The optimized map contains FunDefs with already-optimized CFGs
    use std::hash::BuildHasherDefault;
    use rustc_hash::FxHasher;
    type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

    // We need to build a map of optimized FunDefs for the pipeline
    // First, get the optimized CFGs and create FunDefs from them
    let optimized_fun_def_map: FxHashMap<_, _> = fun_defs
        .iter()
        .map(|fd| {
            // Look up the optimized version if it exists
            let optimized_cfg = _optimized_funs
                .get(&fd.name)
                .map(|opt| opt.cfg.clone())
                .unwrap_or_else(|| fd.cfg.clone());

            let optimized_fd = celeste_rust::ir::FunDef {
                name: fd.name.clone(),
                capture_ids: fd.capture_ids.clone(),
                arg_ids: fd.arg_ids.clone(),
                cfg: optimized_cfg,
                source_span: fd.source_span.clone(),
            };
            (fd.name.clone(), optimized_fd)
        })
        .collect();

    println!("Built inter-procedural analysis maps");

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

        // Run optimization pipeline with inter-procedural passes
        // Uses pre-optimized callees so inlined code is already optimized
        let (opt_result, cfgs) = run_optimization_pipeline_with_interprocedural(
            &fun_def.cfg,
            &analysis,
            &global_closure_map,
            &optimized_fun_def_map,
        );

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
        match &opt_result.block_coalesce {
            BlockCoalesceStatus::Success { blocks_removed } => {
                stats.block_coalesce_success += 1;
                stats.blocks_removed += blocks_removed;
            }
            _ => {}
        }
        match &opt_result.call_resolution {
            CallResolutionStatus::Success { calls_resolved } => {
                stats.call_resolution_success += 1;
                stats.calls_resolved += calls_resolved;
            }
            _ => {}
        }
        match &opt_result.inlining {
            InliningStatus::Success { calls_inlined } => {
                stats.inlining_success += 1;
                stats.calls_inlined += calls_inlined;
            }
            _ => {}
        }
        match &opt_result.dce {
            DceStatus::Success { instructions_removed } => {
                stats.dce_success += 1;
                stats.instructions_removed += instructions_removed;
            }
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
            println!("  Block coalesce: {:?}", opt_result.block_coalesce);
            if !analysis.accessed_globals.is_empty() {
                println!("  Globals: {:?}", analysis.accessed_globals);
            }
        }

        // Create serializable CFGs
        let original_cfg: SerializableCfg = (&fun_def.cfg).into();
        let after_mem2reg_cfg = cfgs.after_mem2reg.as_ref().map(|c| c.into());
        let after_call_resolution_cfg = cfgs.after_call_resolution.as_ref().map(|c| c.into());
        let after_inlining_cfg = cfgs.after_inlining.as_ref().map(|c| c.into());
        let after_dce_cfg = cfgs.after_dce.as_ref().map(|c| c.into());
        let after_block_coalesce_cfg = cfgs.after_block_coalesce.as_ref().map(|c| c.into());
        let after_heap_elim_cfg = cfgs.after_heap_elim.as_ref().map(|c| c.into());

        // Determine final optimized CFG (use the latest successful pass)
        let optimized_cfg = after_heap_elim_cfg
            .clone()
            .or_else(|| after_block_coalesce_cfg.clone())
            .or_else(|| after_dce_cfg.clone())
            .or_else(|| after_inlining_cfg.clone())
            .or_else(|| after_call_resolution_cfg.clone())
            .or_else(|| after_mem2reg_cfg.clone())
            .unwrap_or_else(|| original_cfg.clone());

        // Get source span if available
        let source_span = fun_def.source_span.as_ref().map(|s| (s.start.line, s.end.line));

        let test_case = CfgTestCase {
            name: name.to_string(),
            original_cfg,
            after_mem2reg: after_mem2reg_cfg,
            after_heap_elim: after_heap_elim_cfg,
            after_block_coalesce: after_block_coalesce_cfg,
            after_call_resolution: after_call_resolution_cfg,
            after_inlining: after_inlining_cfg,
            after_dce: after_dce_cfg,
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

    println!("\n=== Block Coalescing ===");
    println!("  Success: {} ({:.1}%)", stats.block_coalesce_success, pct(stats.block_coalesce_success, stats.total));
    println!("  Blocks removed: {}", stats.blocks_removed);

    println!("\n=== Call Resolution ===");
    println!("  Success: {} ({:.1}%)", stats.call_resolution_success, pct(stats.call_resolution_success, stats.total));
    println!("  Calls resolved: {}", stats.calls_resolved);

    println!("\n=== Inlining ===");
    println!("  Success: {} ({:.1}%)", stats.inlining_success, pct(stats.inlining_success, stats.total));
    println!("  Calls inlined: {}", stats.calls_inlined);

    println!("\n=== Dead Code Elimination ===");
    println!("  Success: {} ({:.1}%)", stats.dce_success, pct(stats.dce_success, stats.total));
    println!("  Instructions removed: {}", stats.instructions_removed);

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
    block_coalesce_success: usize,
    blocks_removed: usize,
    call_resolution_success: usize,
    calls_resolved: usize,
    inlining_success: usize,
    calls_inlined: usize,
    dce_success: usize,
    instructions_removed: usize,
}
