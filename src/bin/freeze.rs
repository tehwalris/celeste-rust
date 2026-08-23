//! Build a rewritten program from a recipe and FREEZE it as data.
//!
//! One-shot tool. Its whole purpose is to stop the ~28k lines of
//! `rewrite::rules` from being needed at run time (`plans/deletion.md`),
//! so it is deleted along with them once the artifacts exist and the
//! equality gate has run.
//!
//!   cargo run --release --bin freeze -- rewrites.jsonl rewrites-compile.jsonl
//!
//! With no arguments, freezes every `rewrites*.jsonl` in the working
//! directory.

use anyhow::{Context, Result};

use celeste_rust::rewrite::{frozen, recipe};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let recipes: Vec<std::path::PathBuf> = if args.is_empty() {
        let mut v: Vec<std::path::PathBuf> = std::fs::read_dir(".")?
            .filter_map(|e| e.ok().map(|e| e.path()))
            .filter(|p| {
                p.file_name()
                    .and_then(|n| n.to_str())
                    .map(|n| n.starts_with("rewrites") && n.ends_with(".jsonl"))
                    .unwrap_or(false)
            })
            .collect();
        v.sort();
        v
    } else {
        args.iter().map(std::path::PathBuf::from).collect()
    };

    for path in &recipes {
        let r = recipe::Recipe::load(&path.to_string_lossy())
            .with_context(|| format!("load {}", path.display()))?;
        let (program, _) = recipe::build(&r)
            .with_context(|| format!("build {}", path.display()))?;
        let out = frozen::freeze(&program, path)?;
        // Read it back and compare BEFORE claiming success: a frozen
        // program that does not equal what the rules produced is worse
        // than no frozen program, because everything downstream would
        // quietly run a different program.
        let back = frozen::load(path)?;
        anyhow::ensure!(
            back.functions.len() == program.functions.len()
                && back.merge_partition_cells == program.merge_partition_cells
                && back
                    .functions
                    .iter()
                    .zip(program.functions.iter())
                    .all(|((ak, av), (bk, bv))| ak == bk && av == bv),
            "{} does not round-trip",
            out.display()
        );
        let bytes = std::fs::metadata(&out)?.len();
        println!(
            "[freeze] {} -> {} ({} functions, {} KiB)",
            path.display(),
            out.display(),
            program.functions.len(),
            bytes / 1024
        );
    }
    Ok(())
}
