//! CLI for the two code generators (`celeste_rust::transpile`).
//!
//!   transpile [--recipe R] [OUT]              name tables -> OUT
//!   transpile [--recipe R] --kernel W OUT     class kernel from witness W
//!   transpile [--recipe R] --kernel-recon     the emitter's own recon dump
//!
//! Defaults land in the checked-in generated crates. The canonical regen of
//! everything is `./regen-generated.sh`.

use anyhow::{anyhow, Context, Result};
use celeste_rust::rewrite::program::Program;
use celeste_rust::transpile::{kernel, names};

fn main() -> Result<()> {
    let mut recipe_path: Option<String> = None;
    let mut out_path = "crates/celeste-names/src/gen.rs".to_string();
    let mut kernel_recon_flag = false;
    let mut kernel_out: Option<(String, String)> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--kernel-recon" => kernel_recon_flag = true,
            "--kernel" => {
                let witness = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                let out = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                kernel_out = Some((witness, out));
            }
            // `--rewritten` is the default recipe under its historical name.
            "--rewritten" => recipe_path = Some("rewrites.jsonl".to_string()),
            "--recipe" => {
                recipe_path =
                    Some(args.next().ok_or_else(|| anyhow!("--recipe needs a path"))?);
            }
            other => out_path = other.to_string(),
        }
    }

    let program = match &recipe_path {
        // The program the abstract search actually executes: plain compile +
        // the full recipe. Its concrete semantics must equal the plain
        // program's (each entry is differentially verified), so the same
        // concrete_run oracle applies - transpiling it exercises the
        // recipe-planted instructions (Select/Expand/Kill/guards) natively.
        // `--recipe` selects a different recipe file - the compile-only
        // overlay recipes (rewrites-compile.jsonl) live here, never in the
        // runner.
        Some(path) => {
            let recipe = celeste_rust::rewrite::recipe::Recipe::load(path)?;
            let (program, _) = celeste_rust::rewrite::recipe::build(&recipe)
                .with_context(|| format!("apply {} (run from the repo root)", path))?;
            program
        }
        None => Program::compile_executable_from_disk()
            .context("compile the plain executable program (run from the repo root)")?,
    };

    if kernel_recon_flag {
        names::kernel_recon(&program);
        return Ok(());
    }
    if let Some((witness, out)) = kernel_out {
        return kernel::emit_kernel(&program, &witness, &out);
    }

    let text = names::emit_names(&program, recipe_path.as_deref())?;
    std::fs::write(&out_path, &text).with_context(|| format!("write {}", out_path))?;
    eprintln!("wrote {} ({} bytes)", out_path, text.len());
    Ok(())
}
