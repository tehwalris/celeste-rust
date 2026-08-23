//! The rewritten program, FROZEN as data.
//!
//! A `Program` is normally *derived*: compile the Lua, then replay a
//! recipe of ~1000 rewrite instructions through the 40 rules in
//! `rewrite::rules`. That derivation is the only thing keeping ~28k lines
//! of rules alive (`plans/deletion.md`), and it is pure - same Lua, same
//! recipe, same program, no cart, no clock, no RNG.
//!
//! So it can be done ONCE and checked in, exactly the way
//! `crates/celeste-names/src/gen.rs` already is. This module is that
//! artifact's reader and writer.
//!
//! The cost, stated out loud because it is real: once the rules are gone,
//! a change to `lua/*.lua` or to a recipe can no longer be turned into a
//! new program. The rules stay in git history and can be restored; the
//! campaign that needed them is over. `freeze` exists so the artifact can
//! be regenerated for as long as they are still here.
//!
//! Ordering is EXPLICIT. `Program::functions` is an insertion-ordered
//! `IndexMap` and that order is what `FN_NAMES` and every deterministic
//! print depend on, so the artifact stores a `Vec` of pairs rather than a
//! map - a `HashMap` round-trip would not promise to give it back.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use celeste_ir::ir::{FunDef, GlobalId};

use super::program::Program;

/// Bumped whenever the encoding changes in a way that makes an existing
/// artifact unreadable. A mismatch is an error rather than a silent
/// reinterpretation of the bytes.
const VERSION: u32 = 1;

#[derive(Serialize, Deserialize)]
struct Frozen {
    version: u32,
    /// The recipe this was built from, for provenance in the file itself.
    recipe: String,
    functions: Vec<(GlobalId, FunDef)>,
    merge_partition_cells: Vec<String>,
}

/// Where the frozen program for a recipe lives: alongside it, same stem.
pub fn artifact_path(recipe: &Path) -> PathBuf {
    let mut p = recipe.to_path_buf();
    p.set_extension("program.zst");
    p
}

/// Write `program` as the frozen artifact for `recipe`.
pub fn freeze(program: &Program, recipe: &Path) -> Result<PathBuf> {
    let f = Frozen {
        version: VERSION,
        recipe: recipe.display().to_string(),
        functions: program.functions.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
        merge_partition_cells: program.merge_partition_cells.clone(),
    };
    let raw = bincode::serialize(&f).context("encode the frozen program")?;
    let out = artifact_path(recipe);
    let file = std::fs::File::create(&out)
        .with_context(|| format!("create {}", out.display()))?;
    let mut enc = zstd::Encoder::new(file, 19).context("zstd encoder")?;
    std::io::Write::write_all(&mut enc, &raw).context("write the frozen program")?;
    enc.finish().context("finish the frozen program")?;
    Ok(out)
}

/// Read the frozen program for `recipe`.
pub fn load(recipe: &Path) -> Result<Program> {
    let path = artifact_path(recipe);
    let file = std::fs::File::open(&path).with_context(|| {
        format!(
            "open {} - the frozen program for {}. Regenerate it with `cargo run --bin freeze`.",
            path.display(),
            recipe.display()
        )
    })?;
    let raw = zstd::decode_all(file).with_context(|| format!("decompress {}", path.display()))?;
    let f: Frozen =
        bincode::deserialize(&raw).with_context(|| format!("decode {}", path.display()))?;
    anyhow::ensure!(
        f.version == VERSION,
        "{} is version {}, this build reads version {}",
        path.display(),
        f.version,
        VERSION
    );
    Ok(Program {
        functions: f.functions.into_iter().collect(),
        merge_partition_cells: f.merge_partition_cells,
    })
}

/// Is there a frozen artifact for this recipe?
pub fn exists(recipe: &Path) -> bool {
    artifact_path(recipe).exists()
}
