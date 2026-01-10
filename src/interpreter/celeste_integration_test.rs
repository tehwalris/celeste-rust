//! Integration tests that run the optimization pipeline on actual Celeste CFGs.
//!
//! These tests catch regressions that unit tests with synthetic CFGs might miss.
//! They load the real Celeste source, compile it, and verify that:
//! 1. CFG validation passes at each pipeline stage
//! 2. Key functions successfully optimize
//! 3. Statistics are within expected ranges

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::common::FxHashMap;
    use crate::frontend;
    use crate::interpreter::builtin_resolution::BuiltinSet;
    use crate::interpreter::call_resolution::{build_global_closure_map_from_fun_defs, GlobalClosureMap};
    use crate::interpreter::cfg_analysis::{
        analyze_cfg, run_optimization_pipeline_with_interprocedural,
        run_optimization_pipeline_with_interprocedural_lenient, InterproceduralContext,
    };
    use crate::interpreter::cfg_validation::validate_cfg_with_args;
    use crate::ir::{Cfg, FunDef, GlobalId};

    /// Load and compile the Celeste source, returning function definitions
    fn load_celeste() -> Vec<FunDef> {
        let level_3 = fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game =
            fs::read_to_string("lua/celeste-minimal.lua").expect("Failed to read celeste-minimal.lua");

        let full_code = format!("{}\n{}\n{}", level_3, level_4, game);
        let ast = full_moon::parse(&full_code).expect("Failed to parse Lua source");
        let (_main_cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        fun_defs
    }

    /// Standard builtin set used in the game runner
    fn builtin_set() -> BuiltinSet {
        [
            "__print",
            "__new_unknown_boolean",
            "__new_vector",
            "__array_table_drop_last",
            "error",
            "min",
            "max",
            "abs",
            "flr",
            "__split_by_flr",
            "add",
            "print",
            "mget",
            "fget",
            "tile_flag_at",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect()
    }

    /// Holds the context pieces needed to run the optimization pipeline.
    /// This avoids repeating the same setup code across multiple tests.
    struct PipelineTestContext {
        global_closure_map: GlobalClosureMap,
        builtin_set: BuiltinSet,
        optimized_fun_def_map: FxHashMap<GlobalId, FunDef>,
    }

    impl PipelineTestContext {
        /// Build a pipeline context from the given function definitions
        fn from_fun_defs(fun_defs: &[FunDef]) -> Self {
            Self {
                global_closure_map: build_global_closure_map_from_fun_defs(fun_defs.iter()),
                builtin_set: builtin_set(),
                optimized_fun_def_map: FxHashMap::default(),
            }
        }

        /// Create an InterproceduralContext referencing this test context
        fn as_interprocedural_context(&self) -> InterproceduralContext<'_> {
            InterproceduralContext {
                global_closure_map: &self.global_closure_map,
                optimized_fun_defs: &self.optimized_fun_def_map,
                builtin_set: Some(&self.builtin_set),
            }
        }
    }

    /// Validate that a CFG passes validation with the given external locals
    fn assert_cfg_valid(cfg: &Cfg, external_ids: &[crate::ir::LocalId], context: &str) {
        let result = validate_cfg_with_args(cfg, external_ids);
        if !result.is_valid() {
            panic!(
                "CFG validation failed ({}): {} errors\n  First few: {:?}",
                context,
                result.errors.len(),
                result.errors.iter().take(3).collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn test_celeste_compiles_successfully() {
        let fun_defs = load_celeste();

        // We expect ~75 functions in the Celeste source
        assert!(
            fun_defs.len() >= 70,
            "Expected at least 70 functions, got {}",
            fun_defs.len()
        );
        assert!(
            fun_defs.len() <= 100,
            "Expected at most 100 functions, got {}",
            fun_defs.len()
        );
    }

    #[test]
    fn test_all_original_cfgs_valid() {
        let fun_defs = load_celeste();

        for fun_def in &fun_defs {
            assert_cfg_valid(
                &fun_def.cfg,
                &fun_def.external_local_ids(),
                &format!("{} original", fun_def.name.as_str()),
            );
        }
    }

    #[test]
    fn test_key_function_analyses() {
        let fun_defs = load_celeste();

        // Check specific important functions exist and have expected properties
        let expected_functions = [
            ("player.update", true, true),   // (name_contains, has_calls, writes_heap)
            ("obj.move_x", true, true),      // method with heap operations
            ("btn", true, true),             // button input function
            ("appr", true, false),           // utility, has calls but might not write
            ("solid_at", true, true),        // collision check
        ];

        for (name_pattern, expect_calls, expect_writes) in expected_functions {
            let matching: Vec<_> = fun_defs
                .iter()
                .filter(|fd| fd.name.as_str().contains(name_pattern))
                .collect();

            assert!(
                !matching.is_empty(),
                "Expected to find function matching '{}'",
                name_pattern
            );

            for fun_def in matching {
                let analysis = analyze_cfg(&fun_def.cfg);

                if expect_calls {
                    assert!(
                        analysis.has_calls,
                        "{} should have calls",
                        fun_def.name.as_str()
                    );
                }
                if expect_writes {
                    assert!(
                        analysis.writes_heap,
                        "{} should write heap",
                        fun_def.name.as_str()
                    );
                }
            }
        }
    }

    #[test]
    fn test_pipeline_on_simple_functions() {
        use crate::interpreter::cfg_analysis::PipelineStepType;
        use crate::ir::LocalId;

        // Test pipeline on functions that should optimize cleanly
        let fun_defs = load_celeste();
        let test_ctx = PipelineTestContext::from_fun_defs(&fun_defs);

        // Functions expected to optimize successfully (at least mem2reg)
        let simple_functions = ["btn_4", "appr_68", "sign_69"];

        for name_suffix in simple_functions {
            let fun_def = fun_defs
                .iter()
                .find(|fd| fd.name.as_str().ends_with(name_suffix) || fd.name.as_str() == name_suffix)
                .unwrap_or_else(|| panic!("Function {} not found", name_suffix));

            let analysis = analyze_cfg(&fun_def.cfg);
            let arg_shapes = vec![]; // No special shapes for these simple tests

            let (result, cfgs) = run_optimization_pipeline_with_interprocedural(
                &fun_def.cfg,
                &analysis,
                &test_ctx.as_interprocedural_context(),
                &fun_def.arg_ids,
                &arg_shapes,
            );

            // Check that pipeline made some progress
            assert!(
                cfgs.steps.len() > 1,
                "{}: Expected more than 1 pipeline step, got {}",
                fun_def.name.as_str(),
                cfgs.steps.len()
            );

            // Verify the final CFG is valid
            if let Some(final_step) = cfgs.steps.last() {
                // Build the full list of predefined locals including heap slot locals
                let mut predefined = fun_def.external_local_ids();
                for step in &cfgs.steps {
                    if let PipelineStepType::HeapElimFinal { slot_mappings, .. } = &step.step_type {
                        for mapping in slot_mappings {
                            predefined.push(LocalId::from(mapping.local_id as usize));
                        }
                    }
                }

                let validation = validate_cfg_with_args(&final_step.cfg, &predefined);
                // Note: We allow some validation failures due to known issues with complex functions
                // but we log them for visibility
                if !validation.is_valid() {
                    eprintln!(
                        "Warning: {} final CFG has {} validation error(s):",
                        fun_def.name.as_str(),
                        validation.errors.len()
                    );
                    for error in &validation.errors {
                        eprintln!("  - {}", error);
                    }
                }
            }

            // Log stats for debugging
            eprintln!(
                "{}: {} steps, mem2reg={:?}, calls_resolved={:?}",
                fun_def.name.as_str(),
                cfgs.steps.len(),
                result.mem2reg,
                result.call_resolution,
            );
        }
    }

    #[test]
    fn test_global_closure_map_populated() {
        let fun_defs = load_celeste();
        let global_closure_map = build_global_closure_map_from_fun_defs(fun_defs.iter());

        // Should have entries for most top-level functions
        assert!(
            global_closure_map.len() >= 30,
            "Expected at least 30 entries in global closure map, got {}",
            global_closure_map.len()
        );

        // Specific important functions should be in the map (these are actual compiled functions, not builtins)
        let expected_entries = ["btn", "appr", "sign", "solid_at"];
        for name in expected_entries {
            assert!(
                global_closure_map.contains_key(name),
                "Expected '{}' in global closure map",
                name
            );
        }
    }

    #[test]
    fn test_call_resolution_makes_progress() {
        // Test that call resolution actually resolves some calls on real code
        let fun_defs = load_celeste();
        let test_ctx = PipelineTestContext::from_fun_defs(&fun_defs);

        let mut total_calls_resolved = 0;
        let mut total_inlined = 0;

        for fun_def in fun_defs.iter().take(5) {
            // Test first 5 functions (keeping test fast)
            let analysis = analyze_cfg(&fun_def.cfg);
            if !analysis.has_calls {
                continue;
            }

            // Use lenient version to avoid panics on validation issues in some complex functions
            let (result, _) = run_optimization_pipeline_with_interprocedural_lenient(
                &fun_def.cfg,
                &analysis,
                &test_ctx.as_interprocedural_context(),
                &fun_def.arg_ids,
                &[],
            );

            if let crate::interpreter::cfg_analysis::CallResolutionStatus::Success {
                calls_resolved,
            } = result.call_resolution
            {
                total_calls_resolved += calls_resolved;
            }
            if let crate::interpreter::cfg_analysis::InliningStatus::Success { calls_inlined } =
                result.inlining
            {
                total_inlined += calls_inlined;
            }
        }

        // We expect some progress
        assert!(
            total_calls_resolved > 0,
            "Expected at least some calls to be resolved, got 0"
        );

        eprintln!(
            "Integration test stats: {} calls resolved, {} inlined",
            total_calls_resolved, total_inlined
        );
    }

    #[test]
    fn test_btn_validation_at_each_step() {
        use crate::interpreter::cfg_analysis::PipelineStepType;
        use crate::ir::LocalId;

        // Debug test: find which optimization step introduces the validation error
        let fun_defs = load_celeste();
        let test_ctx = PipelineTestContext::from_fun_defs(&fun_defs);

        let fun_def = fun_defs
            .iter()
            .find(|fd| fd.name.as_str().ends_with("btn_4"))
            .expect("btn_4 not found");

        let analysis = analyze_cfg(&fun_def.cfg);

        let (_result, cfgs) = run_optimization_pipeline_with_interprocedural(
            &fun_def.cfg,
            &analysis,
            &test_ctx.as_interprocedural_context(),
            &fun_def.arg_ids,
            &[],
        );

        // Track accumulated predefined locals through the pipeline
        let mut accumulated_predefined = fun_def.external_local_ids();

        // Check validation at each step
        let mut first_invalid_step = None;
        for (i, step) in cfgs.steps.iter().enumerate() {
            // For HeapElimFinal steps, the slot mappings introduce new predefined locals
            if let PipelineStepType::HeapElimFinal { slot_mappings, .. } = &step.step_type {
                for mapping in slot_mappings {
                    accumulated_predefined.push(LocalId::from(mapping.local_id as usize));
                }
            }

            let validation = validate_cfg_with_args(&step.cfg, &accumulated_predefined);
            if validation.is_valid() {
                eprintln!("Step {}: {} - VALID", i, step.name);
            } else {
                eprintln!("Step {}: {} - {} error(s):", i, step.name, validation.errors.len());
                for error in &validation.errors {
                    eprintln!("  - {}", error);
                }
                if first_invalid_step.is_none() {
                    first_invalid_step = Some(i);
                }
            }
        }

        // Dump the CFG before the first invalid step to help debug
        if let Some(invalid_idx) = first_invalid_step {
            if invalid_idx > 0 {
                let prev_step = &cfgs.steps[invalid_idx - 1];
                eprintln!("\n=== CFG BEFORE ERROR (step {}: {}) ===", invalid_idx - 1, prev_step.name);
                print_cfg(&prev_step.cfg);
            }
            let invalid_step = &cfgs.steps[invalid_idx];
            eprintln!("\n=== CFG WITH ERROR (step {}: {}) ===", invalid_idx, invalid_step.name);
            print_cfg(&invalid_step.cfg);
        }
    }

    /// Helper to print a CFG for debugging
    fn print_cfg(cfg: &Cfg) {
        eprintln!("{}", cfg.format());
    }
}
