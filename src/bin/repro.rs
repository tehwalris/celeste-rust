//! Minimal-repro bisection harness for a compiled-forward divergence.
//!
//! Applies ONE frame to a FIXED captured input state through BOTH engines -
//! the abstract interpreter (unoptimized IR, `interpret_prepared_cfg`) and the
//! trace+ASM engine (`FrameEngine::run_frame_chunk`) - and reports the rung
//! row-set difference. The input state does not change, so stripping the game
//! Lua (`lua/celeste-minimal.lua`) and re-running isolates exactly which code
//! produces the divergence, without re-running the 25-frame trajectory.
//!
//! Both engines read the SAME Lua from disk (the interpreter via
//! `Program::compile_from_disk`, the ASM registry via the tracer's
//! `sources_in`), so editing the Lua file re-drives both sides.
//!
//! Usage:
//!   CELESTE_REM_BITS=2 REPRO_STATE=/tmp/diverging-state.json \
//!     ./target/quick/repro
//!
//! Capture the state first with the check's `CELESTE_DUMP_STATE`:
//!   CELESTE_REM_BITS=2 CELESTE_COMPILED_FORWARD=check CELESTE_FRONTIER_ONLY=1 \
//!     CELESTE_KERNEL_STRICT=0 CELESTE_DUMP_STATE=/tmp/diverging-state.json \
//!     ./target/quick/celeste-rust --rewritten -n 30

use anyhow::{Context, Result};

use celeste_rust::compiled::FrameEngine;
use celeste_rust::interpreter::abstraction::{make_state_abstract, split_precision_straddles};
use celeste_rust::interpreter::fixed_env::PreparedCfg;
use celeste_rust::interpreter::glue::interpret_prepared_cfg;
use celeste_rust::interpreter::inspect::state_from_json;
use celeste_rust::interpreter::state::State;
use celeste_rust::program::Program;

/// The rung (widened) row-key set: the abstraction the compiled-forward check
/// compares on. `split_precision_straddles` + `make_state_abstract` +
/// `sweep::row_keys`, funnelled the same way on both sides.
fn rung_keys(states: &[State]) -> Result<std::collections::HashSet<(u64, u64)>> {
    let mut keys = std::collections::HashSet::new();
    for s in states {
        if s.vector_size == 0 {
            continue;
        }
        for st in split_precision_straddles(s.clone()) {
            let mut st = make_state_abstract(st);
            st.gc();
            keys.extend(celeste_rust::search::sweep::row_keys(&st)?);
        }
    }
    Ok(keys)
}

fn main() -> Result<()> {
    let state_path =
        std::env::var("REPRO_STATE").unwrap_or_else(|_| "/tmp/diverging-state.json".to_string());
    let json = std::fs::read_to_string(&state_path)
        .with_context(|| format!("reading captured state {state_path}"))?;
    let state = state_from_json(&json).context("deserializing captured state")?;
    eprintln!(
        "[repro] loaded {}-lane input state from {}",
        state.vector_size, state_path
    );

    // BOTH sides compile from the SAME Lua on disk. Editing
    // lua/celeste-minimal.lua re-drives both. The interpreter uses the
    // (unoptimized) executable program; the ASM registry is retraced from the
    // start room at engine construction (first frame).
    //
    // REPRO_PARTITION=1 loads the campaign's COMPILE recipe program instead,
    // and sets its pm1 merge-partition patterns - the campaign's exact
    // configuration. The divergence was found to depend on this partition, so
    // the harness must be able to reproduce that setting.
    let use_partition = std::env::var_os("REPRO_PARTITION").is_some();
    let program = if use_partition {
        let p = celeste_rust::program::frozen::rewritten("rewrites-compile.jsonl")
            .context("loading frozen compile program")?;
        celeste_rust::interpreter::vectorize::set_merge_partition_patterns(
            &p.merge_partition_cells,
        );
        eprintln!(
            "[repro] pm1 merge-partition cells: {:?}",
            p.merge_partition_cells
        );
        p
    } else {
        Program::compile_executable_from_disk().context("compiling program")?
    };
    let engine = FrameEngine::new_for_start_room(&program).context("building frame engine")?;
    let frame_cfg = PreparedCfg::new(program.frame_cfg().clone());
    let fixed_env = program.fixed_env();

    // Interpreter side (the oracle).
    let reference: Vec<State> = interpret_prepared_cfg(&frame_cfg, state.clone(), &fixed_env)
        .context("interpreter frame")?
        .into_iter()
        .map(|(s, _)| s)
        .collect();

    // ASM engine side. The campaign fallback is provided, so a kernel MISS
    // falls to the interpreter (and would hide a divergence); the diverging
    // shape binds a kernel, so this exercises the ASM.
    let got: Vec<State> = engine
        .run_frame_chunk(&state, Some((&frame_cfg, &fixed_env)))
        .into_iter()
        .map(|(s, _)| s)
        .collect();

    let want = rung_keys(&reference).context("rung keys (interp)")?;
    let gotk = rung_keys(&got).context("rung keys (asm)")?;
    let missing = want.difference(&gotk).count();
    let extra = gotk.difference(&want).count();

    println!(
        "interp {} rows, asm {} rows | missing (interp-only) {}, extra (asm-only) {} | {}",
        want.len(),
        gotk.len(),
        missing,
        extra,
        if missing == 0 && extra == 0 {
            "MATCH"
        } else {
            "DIVERGES"
        }
    );
    // Non-zero exit on divergence, so a bisect script can branch on it.
    if missing != 0 || extra != 0 {
        std::process::exit(1);
    }
    Ok(())
}
