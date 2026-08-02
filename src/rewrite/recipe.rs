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
use super::rules::{
    allocate_slots, dce, fold, if_convert, inline, merge_blocks, promote_capture, promote_cell,
};
use crate::ir::LocalId;
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
    /// Repack locals so that values with disjoint live ranges share a slot.
    /// Changes no instructions - only where they are stored.
    AllocateSlots,
    /// Merge blocks into single-successor predecessors.
    MergeBlocks,
    /// Local simplifications: constant conditions, degenerate phis.
    Fold,
    /// Splice a capture-free callee's body into one call site, guarded by an
    /// `assert_closure`.
    Inline {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// The function to splice in.
        callee: String,
    },
    /// Collapse a branch whose arm is safe to run unconditionally into a
    /// `select` at the named join block.
    IfConvert {
        #[serde(rename = "fn")]
        function: String,
        /// The block the two paths join at.
        join: String,
    },
    /// Make every closure of a function capture a value rather than the cell
    /// holding it. Keyed by the callee, because one `FunDef` is shared by all
    /// its creation sites and they must change together.
    PromoteCapture {
        #[serde(rename = "fn")]
        function: String,
        /// Which capture position, counting from 0.
        index: usize,
    },
    /// Replace one non-escaping, single-store heap cell with SSA values.
    PromoteCell {
        #[serde(rename = "fn")]
        function: String,
        /// The `alloc` that defines the cell, as `%N`.
        cell: String,
    },
}

impl Rule {
    pub fn name(&self) -> &'static str {
        match self {
            Rule::Dce => "dce",
            Rule::AllocateSlots => "allocate_slots",
            Rule::MergeBlocks => "merge_blocks",
            Rule::Fold => "fold",
            Rule::Inline { .. } => "inline",
            Rule::IfConvert { .. } => "if_convert",
            Rule::PromoteCapture { .. } => "promote_capture",
            Rule::PromoteCell { .. } => "promote_cell",
        }
    }
}

fn parse_cell(text: &str) -> Result<LocalId> {
    super::print::parse_local_name(text)
        .ok_or_else(|| anyhow!("cell must look like %17, got {:?}", text))
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
        Rule::AllocateSlots => allocate_slots::apply(program),
        Rule::MergeBlocks => merge_blocks::apply(program),
        Rule::Fold => fold::apply(program),
        Rule::IfConvert { function, join } => if_convert::apply(program, function, join),
        Rule::PromoteCapture { function, index } => {
            promote_capture::apply(program, function, *index)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::apply(program, function, parse_cell(cell)?)
        }
        Rule::Inline { function, at, callee } => {
            inline::apply(program, &entry.id, function, parse_cell(at)?, callee)
        }
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
        Rule::AllocateSlots => allocate_slots::verify(&before, program),
        Rule::MergeBlocks => merge_blocks::verify(&before, program),
        Rule::Fold => fold::verify(&before, program),
        Rule::IfConvert { function, join } => {
            if_convert::verify(&before, program, function, join)
        }
        Rule::PromoteCapture { function, index } => {
            promote_capture::verify(&before, program, function, *index)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::verify(&before, program, function, parse_cell(cell)?)
        }
        Rule::Inline { function, at, callee } => {
            inline::verify(&before, program, &entry.id, function, parse_cell(at)?, callee)
        }
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
