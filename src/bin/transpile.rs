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
    let mut fuse_census: Option<(Vec<String>, String)> = None;
    let mut fuse_out: Option<(Vec<String>, String, String)> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--kernel-recon" => kernel_recon_flag = true,
            // --fuse-census R1,R2,.. WITNESS: lower each recipe as a
            // specialization-set member and print the sharing census
            // (transpile::fuse). Ignores --recipe.
            "--fuse-census" => {
                let recipes = args
                    .next()
                    .ok_or_else(|| anyhow!("--fuse-census R1,R2,.. WITNESS"))?;
                let witness = args
                    .next()
                    .ok_or_else(|| anyhow!("--fuse-census R1,R2,.. WITNESS"))?;
                fuse_census = Some((
                    recipes.split(',').map(|s| s.to_string()).collect(),
                    witness,
                ));
            }
            // --fuse R1,R2,.. WITNESS OUT.rs: emit the fused artifact for
            // the specialization set (member 0 = the primary). The output
            // is generated per campaign, never committed (feature `fused`).
            "--fuse" => {
                let recipes =
                    args.next().ok_or_else(|| anyhow!("--fuse R1,R2,.. WITNESS OUT"))?;
                let witness =
                    args.next().ok_or_else(|| anyhow!("--fuse R1,R2,.. WITNESS OUT"))?;
                let out = args.next().ok_or_else(|| anyhow!("--fuse R1,R2,.. WITNESS OUT"))?;
                fuse_out = Some((
                    recipes.split(',').map(|s| s.to_string()).collect(),
                    witness,
                    out,
                ));
            }
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

    let load_members = |recipes: &[String]| -> Result<Vec<(String, Program)>> {
        recipes
            .iter()
            .map(|path| -> Result<(String, Program)> {
                let recipe = celeste_rust::rewrite::recipe::Recipe::load(path)?;
                let (program, _) = celeste_rust::rewrite::recipe::build(&recipe)
                    .with_context(|| format!("apply {}", path))?;
                Ok((path.clone(), program))
            })
            .collect()
    };
    if let Some((recipes, witness)) = fuse_census {
        let members = load_members(&recipes)?;
        return celeste_rust::transpile::fuse::fuse_census(&members, &witness);
    }
    if let Some((recipes, witness, out)) = fuse_out {
        let members = load_members(&recipes)?;
        let text = celeste_rust::transpile::fuse::emit_fused(&members, &witness)?;
        std::fs::write(&out, &text).with_context(|| format!("write {}", out))?;
        eprintln!("wrote {} ({} bytes)", out, text.len());
        return Ok(());
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
