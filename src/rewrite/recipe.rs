//! The recipe: an ordered list of rewrite instructions, checked into git.
//!
//! The program is always *derived* - `lua sources -> compile -> apply recipe`.
//! Nothing is ever edited by hand, so the recipe is the artifact we author and
//! review, and replaying it from scratch always reproduces the same program.
//!
//! Each entry carries a stable `id` chosen by the author, not its position in
//! the list. Ids are what generated names are derived from, so inserting an
//! entry in the middle does not invalidate the ones after it.
//!
//! `shape` names the heap shape the entry was authored against, for rules whose
//! side conditions are discharged against a shape rather than proved (see
//! plans/rewrite-plan.md section 2b). Bulk rules leave it null.

use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};

use super::program::Program;
use super::rules::{dce, fold, merge_blocks};
use super::validate::validate_program;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RewriteEntry {
    /// Stable identifier, unique within the recipe. Author-chosen.
    pub id: String,
    #[serde(flatten)]
    pub rule: Rule,
    /// Heap shape this entry was authored against, if the rule needs one.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub shape: Option<String>,
    /// Free-text justification. Required for rules that are not
    /// semantics-preserving on their own.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub why: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "rule", rename_all = "snake_case")]
pub enum Rule {
    /// Remove dead instructions and unreachable blocks.
    Dce,
    /// Merge blocks into single-successor predecessors.
    MergeBlocks,
    /// Local simplifications: constant conditions, degenerate phis.
    Fold,
}

impl Rule {
    pub fn name(&self) -> &'static str {
        match self {
            Rule::Dce => "dce",
            Rule::MergeBlocks => "merge_blocks",
            Rule::Fold => "fold",
        }
    }
}

#[derive(Debug, Default)]
pub struct Recipe {
    pub entries: Vec<RewriteEntry>,
}

impl Recipe {
    pub fn parse(text: &str) -> Result<Self> {
        let mut entries = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for (n, line) in text.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
                continue;
            }
            let entry: RewriteEntry = serde_json::from_str(line)
                .with_context(|| format!("recipe line {}", n + 1))?;
            if !seen.insert(entry.id.clone()) {
                return Err(anyhow!(
                    "recipe line {}: duplicate id {:?}. Ids must be unique - \
                     generated names are derived from them.",
                    n + 1,
                    entry.id
                ));
            }
            entries.push(entry);
        }
        Ok(Self { entries })
    }

    pub fn load(path: &str) -> Result<Self> {
        if !std::path::Path::new(path).exists() {
            return Ok(Self::default());
        }
        Self::parse(&std::fs::read_to_string(path)?)
    }

    pub fn to_text(&self) -> Result<String> {
        let mut out = String::new();
        for entry in &self.entries {
            out.push_str(&serde_json::to_string(entry)?);
            out.push('\n');
        }
        Ok(out)
    }
}

pub struct StepReport {
    pub id: String,
    pub rule: &'static str,
    pub changes: usize,
    pub instructions_before: usize,
    pub instructions_after: usize,
    pub blocks_before: usize,
    pub blocks_after: usize,
}

/// Applies one entry, then validates and verifies it.
///
/// Verification is per-rule and deliberately written independently of the
/// applier. Structural validation (including dominance) runs on top, because it
/// catches whole classes of mistake that no individual rule verifier would
/// think to look for.
pub fn apply_entry(program: &mut Program, entry: &RewriteEntry) -> Result<StepReport> {
    let before = program.clone();
    let instructions_before = program.instruction_count();
    let blocks_before = program.block_count();

    let changes = match &entry.rule {
        Rule::Dce => dce::apply(program),
        Rule::MergeBlocks => merge_blocks::apply(program),
        Rule::Fold => fold::apply(program),
    }
    .with_context(|| format!("applying {} ({})", entry.id, entry.rule.name()))?;

    let errors = validate_program(program);
    if !errors.is_empty() {
        let shown: Vec<String> = errors.iter().take(10).map(|e| e.to_string()).collect();
        return Err(anyhow!(
            "{} ({}) produced {} structural error(s):\n  {}",
            entry.id,
            entry.rule.name(),
            errors.len(),
            shown.join("\n  ")
        ));
    }

    match &entry.rule {
        Rule::Dce => dce::verify(&before, program),
        Rule::MergeBlocks => merge_blocks::verify(&before, program),
        Rule::Fold => fold::verify(&before, program),
    }
    .with_context(|| format!("verifying {} ({})", entry.id, entry.rule.name()))?;

    Ok(StepReport {
        id: entry.id.clone(),
        rule: entry.rule.name(),
        changes,
        instructions_before,
        instructions_after: program.instruction_count(),
        blocks_before,
        blocks_after: program.block_count(),
    })
}

/// Builds the program from source and replays the whole recipe.
pub fn build(recipe: &Recipe) -> Result<(Program, Vec<StepReport>)> {
    let mut program = Program::compile_from_disk()?;

    let errors = validate_program(&program);
    if !errors.is_empty() {
        let shown: Vec<String> = errors.iter().take(10).map(|e| e.to_string()).collect();
        return Err(anyhow!(
            "the freshly compiled program has {} structural error(s), before any rewrites:\n  {}",
            errors.len(),
            shown.join("\n  ")
        ));
    }

    let mut reports = Vec::new();
    for entry in &recipe.entries {
        reports.push(apply_entry(&mut program, entry)?);
    }
    Ok((program, reports))
}
