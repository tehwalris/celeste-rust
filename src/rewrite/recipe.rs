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
    absorb_stores, allocate_slots, assume_eq, collapse_loop, convert_assert, convert_ternary,
    cse, dce, decompose_branch,
    decompose_truthy, demote_create,
    expand_bool, fold, fold_reflexive, fold_select, fuse_breaks, if_convert, inline,
    mask_loop, merge_blocks,
    pin_builtin,
    promote_capture,
    promote_cell, sink_store, speculate, speculate_region,
};
use crate::ir::LocalId;
use super::validate::{validate_function, validate_program};

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
    /// One instruction per value within a basic block. A prerequisite for
    /// promoting object fields, which needs each cell to have a single
    /// accessor.
    Cse {
        /// Opt-in store-to-load forwarding with field-name alias refinement
        /// and heap-oblivious call exemption - see the `forward` section of
        /// the rule's docs. Off by default so old entries replay
        /// byte-identically.
        #[serde(default, skip_serializing_if = "is_false")]
        forward: bool,
    },
    /// Repack locals so that values with disjoint live ranges share a slot.
    /// Changes no instructions - only where they are stored.
    AllocateSlots,
    /// Merge blocks into single-successor predecessors.
    MergeBlocks,
    /// Local simplifications: constant conditions, degenerate phis.
    Fold,
    /// A comparison of a number constant with itself becomes the constant
    /// reflexivity dictates. Kept apart from `fold` so older `fold` entries
    /// replay byte-identically.
    FoldReflexive {
        /// Opt-in for the pointer families: witnessed-pointer reflexive
        /// equality, nil against nil, witnessed pointer against nil.
        /// Off by default so entries written before these existed keep
        /// replaying byte-identically.
        #[serde(default, skip_serializing_if = "is_false")]
        pointers: bool,
    },
    /// Resolve selects and branches whose condition's truthiness is static:
    /// an `and` chain poisoned by a constant `false` is falsy all the way
    /// down, whatever the opaque links hold. Kept apart from `fold` for the
    /// same replay reason.
    FoldSelect,
    /// Splice a callee's body into one call site, guarded by an
    /// `assert_closure`.
    Inline {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// The function to splice in.
        callee: String,
        /// Locals holding the callee's captured values, one per capture, as
        /// `%N`. The guard asserts the closure really did capture these, so a
        /// wrong guess fails loudly rather than reading the wrong object.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        captures: Vec<String>,
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
    /// Name the callee of a call to a pure builtin, so that `if_convert` can
    /// speculate it and `cse` can see through it. Guarded: the call fails if
    /// the callee is not the builtin named.
    PinBuiltin {
        #[serde(rename = "fn")]
        function: String,
        /// The `call` instruction, as `%N`.
        at: String,
        /// Which builtin. Must be one of `fixed_env::PURE_BUILTINS`.
        name: String,
    },
    /// Turn one creating accessor into a plain read, guarded by an
    /// `assert_pointer`. Not semantics-preserving: it claims the field already
    /// exists there, and the guard is what makes a wrong claim loud instead of
    /// silent. Screen at full depth - see the rule's docs.
    DemoteCreate {
        #[serde(rename = "fn")]
        function: String,
        /// The accessor, as `%N`.
        at: String,
    },
    /// Convert the two joins Lua's `a and b or c` compiles to into one select
    /// on the original condition, in one step - one at a time is impossible,
    /// because the intermediate phi mixes a bool with a number. Requires the
    /// `and` arm's value to be statically truthy (`pin_builtin` supplies
    /// that for the `appr` sites).
    ConvertTernary {
        #[serde(rename = "fn")]
        function: String,
        /// The outer (`or`) join block.
        join: String,
    },
    /// Split a mixed `and`/`or` select cascade into (truthiness, value) pairs
    /// of homogeneous selects, so no select ever combines a Bool with a
    /// Number. Requires the cascade's root to be `x or k` with `k` statically
    /// truthy; the root keeps its id, every interior id dies.
    DecomposeTruthy {
        #[serde(rename = "fn")]
        function: String,
        /// The cascade's root select, as `%N`.
        root: String,
    },
    /// Hoist an arm's speculatable instructions into the head, leaving only
    /// its stores behind - the state `sink_store` / `absorb_stores` needs.
    /// Refused unless every hoist commutes with every store it crosses.
    Speculate {
        #[serde(rename = "fn")]
        function: String,
        /// The block the two paths join at.
        join: String,
        /// For a *diamond* at `join`: which of its two arms to hoist from.
        /// Absent for a triangle, which has only one.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        arm: Option<String>,
        /// Runtime-distinctness guards: each permits one hoisted load (or
        /// `assert_value_cell`) to cross one store whose cell the rule cannot
        /// prove distinct, paid for with an emitted pointer compare plus
        /// `assert_true` in the head. If the cells ever alias, the run dies
        /// loudly instead of reading the wrong value silently. Opt-in, so
        /// entries without guards emit byte-identically to before.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        guards: Vec<SpeculateGuard>,
    },
    /// Replace a branch whose arms are bare stores with a select-store in the
    /// head. Both arms storing to the same local needs no guard; a one-arm
    /// triangle uses the `sink_store` load-back trick and its
    /// `assert_value_cell` guard. The join is untouched, so it may have any
    /// number of other predecessors - but no phis. Screen at full depth.
    AbsorbStores {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is absorbed.
        head: String,
    },
    /// Run a pure single-entry single-exit subgraph (loops included)
    /// unconditionally instead of behind a branch: the head's branch becomes
    /// a jump into the region, the region is untouched, the join's phis
    /// become selects. Loops must have the guarded counter shape; the bound
    /// gets a loud `assert_true` range guard. Screen at full depth.
    SpeculateRegion {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is removed.
        head: String,
        /// The region's entry: the branch target that is not the join.
        arm: String,
        /// Diamond mode: where both branch targets' regions meet. The two
        /// regions are serialized, named arm first, and this join's phis
        /// become selects. Absent for a triangle, whose join is the head's
        /// other branch target.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        join: Option<String>,
        /// Allow stores in the region: each becomes an assert-load-select-
        /// store masked by the head's condition, landing only on the lanes
        /// that took its region. Opt-in so store-free entries refuse stores
        /// loudly, as before.
        #[serde(default, skip_serializing_if = "is_false")]
        mask: bool,
        /// Allow `expand` plus its concretization store - the load-adjacent
        /// store of the expanded vector back into the very cell the value
        /// was loaded from. Both run unmasked: the store writes the
        /// exhaustive lane-split of exactly what the cell holds, a per-lane
        /// refinement whether or not the lane took the region. Opt-in
        /// because lanes that skipped the region still double.
        #[serde(default, skip_serializing_if = "is_false")]
        expand: bool,
    },
    /// Give a loop with a per-lane trip count a uniform constant trip count:
    /// the head branches on a fresh counter against `limit` (per-state
    /// uniform, so it stops splitting), per-lane progress becomes mask data,
    /// every latch/break store is masked, and the break edge is deleted. A
    /// loud `assert_true(bound <= limit)` guard covers the claim that
    /// `limit` iterations are enough. Screen at full depth.
    MaskLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The loop header: exactly a counter phi, `counter <= bound`, and
        /// the conditional branch on it.
        head: String,
        /// The uniform trip count. Iterations run for counters 0..=limit;
        /// a state whose per-lane bound exceeds it fails loudly. Exactly one
        /// of `limit` and `span` must be given.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        limit: Option<i16>,
        /// Like `limit`, but for a loop whose *init* is per-lane too: the
        /// loop runs `span + 1` uniform iterations, guarded by
        /// `assert_true(init >= 0)` and `assert_true(bound - init < span + 1)`
        /// (strict - a Lua `for` bound may be fractional).
        #[serde(default, skip_serializing_if = "Option::is_none")]
        span: Option<i16>,
        /// Opt-in: the loop's break edge leads to this external join instead
        /// of the exit (a multi-level `break`, as in the inlined
        /// `spikes_at`). The break value - a bool constant in the join's
        /// phis - is re-routed through the exit: as a conditional branch on
        /// the loop's `active` flag, or merged by select when the exit
        /// already falls through to the join.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        break_to: Option<String>,
    },
    /// Merge two consecutive early-exit branches into one: the second test
    /// block's instructions go eager into the head, the conditions combine
    /// through an `or` select, and the first break block goes unreachable
    /// for a later `dce` to sweep. Both break blocks must feed the join's
    /// phis the same bool constant, so the switch of edges is invisible.
    FuseBreaks {
        #[serde(rename = "fn")]
        function: String,
        /// The block holding the earlier of the two early-exit branches.
        head: String,
    },
    /// Move a triangle arm's trailing store past the join: a guarded load
    /// before the branch, a phi at the join, the store after it. The
    /// `assert_value_cell` guard makes the one non-identity case - a closure
    /// or table cell - fail loudly. Screen at full depth.
    SinkStore {
        #[serde(rename = "fn")]
        function: String,
        /// The store, as `%N`.
        at: String,
    },
    /// Replace one non-escaping, single-store heap cell with SSA values.
    PromoteCell {
        #[serde(rename = "fn")]
        function: String,
        /// The `alloc` that defines the cell, as `%N`.
        cell: String,
    },
    /// Branch on an `and`'s condition instead of its mixed value:
    /// `%p = select %c ? %k : %c; br %p` becomes `br %c`, with the true
    /// target's phis taking `%k` on that edge and the select deleted.
    /// Requires `%k` statically truthy. The branch-side counterpart of
    /// `decompose_truthy`, for short-circuits whose skipped path has side
    /// effects and so must remain a branch.
    DecomposeBranch {
        #[serde(rename = "fn")]
        function: String,
        /// The mixed select, as `%N`.
        at: String,
    },
    /// Replace a bool-concretization diamond (`btn`'s tail: branch on an
    /// unknown bool, each arm storing its constant back into the cell) with
    /// an `expand` instruction: lanes duplicate instead of the state
    /// splitting, and the per-lane bool is stored back through the arms'
    /// accessor. The expand fails loudly if the branched-on value is ever
    /// not a bool. Screen at full depth.
    ExpandBool {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch is removed.
        head: String,
    },
    /// Replace an inlined `__assert` failure diamond (`if not cond then
    /// __print(...) ... error(...) end`) with a single `assert_true` on the
    /// condition. Every path through the diamond aborts the run, exactly
    /// like a failing assert, so only the branch disappears - a uniform,
    /// never-taken branch that still blocked speculation.
    ConvertAssert {
        #[serde(rename = "fn")]
        function: String,
        /// The block whose conditional branch into the diamond is removed.
        head: String,
    },
    /// State that two locals hold the same value - `%g = b == a;
    /// assert_true %g` right after `b`'s definition - and replace every
    /// other use of `b` with `a`. For equalities no local analysis can see:
    /// the object read back out of the singleton table is the object being
    /// updated. The premise is checked on every lane of every execution and
    /// fails loudly. Screen at full depth.
    AssumeEq {
        #[serde(rename = "fn")]
        function: String,
        /// The local that survives, as `%N`. Must dominate `b`.
        a: String,
        /// The local that is replaced, as `%N`.
        b: String,
    },
    /// Collapse a counted loop to one guarded execution of its body: a loud
    /// `assert_true(bound == init)` in the preheader states that the trip
    /// count is exactly one, the counter is replaced by its initial value,
    /// and the head disappears - the preheader falls into the body, the
    /// latch falls out to the exit. For the loops bounded by `#objects`,
    /// which room (1, 0) keeps at exactly one object for the whole search.
    /// Screen at full depth.
    CollapseLoop {
        #[serde(rename = "fn")]
        function: String,
        /// The loop header: exactly a counter phi, `counter <= bound`, and
        /// the conditional branch on it.
        head: String,
    },
}

impl Rule {
    pub fn name(&self) -> &'static str {
        match self {
            Rule::Dce => "dce",
            Rule::Cse { .. } => "cse",
            Rule::AllocateSlots => "allocate_slots",
            Rule::MergeBlocks => "merge_blocks",
            Rule::Fold => "fold",
            Rule::FoldReflexive { .. } => "fold_reflexive",
            Rule::FoldSelect => "fold_select",
            Rule::Inline { .. } => "inline",
            Rule::IfConvert { .. } => "if_convert",
            Rule::PromoteCapture { .. } => "promote_capture",
            Rule::PinBuiltin { .. } => "pin_builtin",
            Rule::DemoteCreate { .. } => "demote_create",
            Rule::ConvertTernary { .. } => "convert_ternary",
            Rule::DecomposeTruthy { .. } => "decompose_truthy",
            Rule::Speculate { .. } => "speculate",
            Rule::SpeculateRegion { .. } => "speculate_region",
            Rule::MaskLoop { .. } => "mask_loop",
            Rule::FuseBreaks { .. } => "fuse_breaks",
            Rule::SinkStore { .. } => "sink_store",
            Rule::AbsorbStores { .. } => "absorb_stores",
            Rule::PromoteCell { .. } => "promote_cell",
            Rule::DecomposeBranch { .. } => "decompose_branch",
            Rule::ExpandBool { .. } => "expand_bool",
            Rule::ConvertAssert { .. } => "convert_assert",
            Rule::CollapseLoop { .. } => "collapse_loop",
            Rule::AssumeEq { .. } => "assume_eq",
        }
    }
}

/// One declared crossing for `Rule::Speculate`'s `guards` field.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpeculateGuard {
    /// The hoisted cell-reading instruction, as `%N`.
    pub load: String,
    /// The crossed store's target local, as `%N`.
    pub store: String,
}

/// serde helper: keep serialized entries free of `"mask":false` noise.
fn is_false(value: &bool) -> bool {
    !*value
}

fn parse_cell(text: &str) -> Result<LocalId> {
    super::print::parse_local_name(text)
        .ok_or_else(|| anyhow!("cell must look like %17, got {:?}", text))
}

fn parse_guards(guards: &[SpeculateGuard]) -> Result<Vec<(LocalId, LocalId)>> {
    guards
        .iter()
        .map(|g| Ok((parse_cell(&g.load)?, parse_cell(&g.store)?)))
        .collect()
}

fn parse_cells(texts: &[String]) -> Result<Vec<LocalId>> {
    texts.iter().map(|t| parse_cell(t)).collect()
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
    /// Where the time went. Replaying the recipe is the prefix of every command,
    /// so it is the number that decides how fast this project is to work on -
    /// it is worth being able to see it broken down rather than guessed at.
    pub timing: StepTiming,
}

#[derive(Default, Clone, Copy)]
pub struct StepTiming {
    pub clone: std::time::Duration,
    pub apply: std::time::Duration,
    pub validate: std::time::Duration,
    pub verify: std::time::Duration,
}

impl StepTiming {
    pub fn add(&mut self, other: &StepTiming) {
        self.clone += other.clone;
        self.apply += other.apply;
        self.validate += other.validate;
        self.verify += other.verify;
    }

    pub fn total(&self) -> std::time::Duration {
        self.clone + self.apply + self.validate + self.verify
    }
}

/// Applies one entry, then validates and verifies it.
///
/// Verification is per-rule and deliberately written independently of the
/// applier. Structural validation (including dominance) runs on top, because it
/// catches whole classes of mistake that no individual rule verifier would
/// think to look for.
pub fn apply_entry(program: &mut Program, entry: &RewriteEntry) -> Result<StepReport> {
    let mut timing = StepTiming::default();
    let instructions_before = program.instruction_count();
    let blocks_before = program.block_count();

    // Every rule but `allocate_slots` runs under the identity slot map.
    //
    // A slot map is built for a particular set of instructions and says nothing
    // about instructions minted later, so any rule that adds a `LocalId` after
    // `allocate_slots` leaves the map stale. `validate` catches that - which is
    // how it was found, when `screen` appended candidates to a recipe whose last
    // entry was `allocate_slots` and 36 of 36 died with structural errors - but
    // failing loudly on something avoidable is worse than not creating it.
    // Resetting here makes "the map matches the program" an invariant of the
    // recipe rather than an ordering rule an author has to remember.
    //
    // This happens *before* `before` is taken, so that `before` is the program
    // the rule actually saw. Taking the snapshot first was a real bug: a rule
    // whose verifier checks that it changed nothing else - `pin_builtin` and
    // `demote_create` both do - then sees every untouched function differ,
    // because `FunDef` carries the slot map and the reset had just changed all
    // of them. It only showed up under `screen`, where the base program has been
    // through `allocate_slots`; in a recipe these rules run before it, where the
    // reset is a no-op.
    if !matches!(entry.rule, Rule::AllocateSlots) {
        for fun in program.functions.values_mut() {
            fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
        }
    }

    let clock = std::time::Instant::now();
    let before = program.clone();
    timing.clone = clock.elapsed();
    let clock = std::time::Instant::now();

    let changes = match &entry.rule {
        Rule::Dce => dce::apply(program),
        Rule::Cse { forward } => cse::apply(program, *forward),
        Rule::AllocateSlots => allocate_slots::apply(program),
        Rule::MergeBlocks => merge_blocks::apply(program),
        Rule::Fold => fold::apply(program),
        Rule::FoldReflexive { pointers } => fold_reflexive::apply(program, *pointers),
        Rule::FoldSelect => fold_select::apply(program),
        Rule::IfConvert { function, join } => if_convert::apply(program, function, join),
        Rule::PromoteCapture { function, index } => {
            promote_capture::apply(program, function, *index)
        }
        Rule::PinBuiltin { function, at, name } => {
            pin_builtin::apply(program, function, parse_cell(at)?, name)
        }
        Rule::DemoteCreate { function, at } => {
            demote_create::apply(program, function, parse_cell(at)?)
        }
        Rule::ConvertTernary { function, join } => {
            convert_ternary::apply(program, function, join)
        }
        Rule::DecomposeTruthy { function, root } => {
            decompose_truthy::apply(program, function, parse_cell(root)?)
        }
        Rule::Speculate { function, join, arm, guards } => {
            speculate::apply(program, function, join, arm.as_deref(), &parse_guards(guards)?)
        }
        Rule::SpeculateRegion { function, head, arm, join, mask, expand } => {
            speculate_region::apply(program, function, head, arm, join.as_deref(), *mask, *expand)
        }
        Rule::MaskLoop { function, head, limit, span, break_to } => {
            mask_loop::apply(program, function, head, *limit, *span, break_to.as_deref())
        }
        Rule::FuseBreaks { function, head } => fuse_breaks::apply(program, function, head),
        Rule::AbsorbStores { function, head } => absorb_stores::apply(program, function, head),
        Rule::SinkStore { function, at } => {
            sink_store::apply(program, function, parse_cell(at)?)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::apply(program, function, parse_cell(cell)?)
        }
        Rule::DecomposeBranch { function, at } => {
            decompose_branch::apply(program, function, parse_cell(at)?)
        }
        Rule::ExpandBool { function, head } => expand_bool::apply(program, function, head),
        Rule::ConvertAssert { function, head } => convert_assert::apply(program, function, head),
        Rule::CollapseLoop { function, head } => collapse_loop::apply(program, function, head),
        Rule::AssumeEq { function, a, b } => {
            assume_eq::apply(program, function, parse_cell(a)?, parse_cell(b)?)
        }
        Rule::Inline { function, at, callee, captures } => inline::apply(
            program,
            &entry.id,
            function,
            parse_cell(at)?,
            callee,
            &parse_cells(captures)?,
        ),
    }
    .with_context(|| format!("applying {} ({})", entry.id, entry.rule.name()))?;
    timing.apply = clock.elapsed();

    // Validate the functions this entry actually changed, found by comparing
    // against the copy we already had to take.
    //
    // Whole-program validation after every entry was 97% of recipe replay once
    // inlining had grown the program: dominance is superlinear in block count
    // and `player.update_21` is now hundreds of blocks, so 554 entries x 77
    // functions dominated everything. Validating a function that did not change
    // cannot find anything - `validate_function` reads only that function - and
    // the comparison is exact rather than a guess about what each rule touches,
    // so nothing is taken on trust.
    let clock = std::time::Instant::now();
    let mut errors = Vec::new();
    for (name, after_fun) in program.functions.iter() {
        if before.functions.get(name) == Some(after_fun) {
            continue;
        }
        errors.extend(validate_function(after_fun));
    }
    timing.validate = clock.elapsed();
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

    let clock = std::time::Instant::now();
    match &entry.rule {
        Rule::Dce => dce::verify(&before, program),
        Rule::Cse { forward } => cse::verify(&before, program, *forward),
        Rule::AllocateSlots => allocate_slots::verify(&before, program),
        Rule::MergeBlocks => merge_blocks::verify(&before, program),
        Rule::Fold => fold::verify(&before, program),
        Rule::FoldReflexive { pointers } => {
            fold_reflexive::verify(&before, program, *pointers)
        }
        Rule::FoldSelect => fold_select::verify(&before, program),
        Rule::IfConvert { function, join } => {
            if_convert::verify(&before, program, function, join)
        }
        Rule::PromoteCapture { function, index } => {
            promote_capture::verify(&before, program, function, *index)
        }
        Rule::PinBuiltin { function, at, name } => {
            pin_builtin::verify(&before, program, function, parse_cell(at)?, name)
        }
        Rule::DemoteCreate { function, at } => {
            demote_create::verify(&before, program, function, parse_cell(at)?)
        }
        Rule::ConvertTernary { function, join } => {
            convert_ternary::verify(&before, program, function, join)
        }
        Rule::DecomposeTruthy { function, root } => {
            decompose_truthy::verify(&before, program, function, parse_cell(root)?)
        }
        Rule::Speculate { function, join, arm, guards } => {
            speculate::verify(&before, program, function, join, arm.as_deref(), &parse_guards(guards)?)
        }
        Rule::SpeculateRegion { function, head, arm, join, mask, expand } => {
            speculate_region::verify(
                &before,
                program,
                function,
                head,
                arm,
                join.as_deref(),
                *mask,
                *expand,
            )
        }
        Rule::MaskLoop { function, head, limit, span, break_to } => {
            mask_loop::verify(&before, program, function, head, *limit, *span, break_to.as_deref())
        }
        Rule::FuseBreaks { function, head } => {
            fuse_breaks::verify(&before, program, function, head)
        }
        Rule::AbsorbStores { function, head } => {
            absorb_stores::verify(&before, program, function, head)
        }
        Rule::SinkStore { function, at } => {
            sink_store::verify(&before, program, function, parse_cell(at)?)
        }
        Rule::PromoteCell { function, cell } => {
            promote_cell::verify(&before, program, function, parse_cell(cell)?)
        }
        Rule::DecomposeBranch { function, at } => {
            decompose_branch::verify(&before, program, function, parse_cell(at)?)
        }
        Rule::ExpandBool { function, head } => {
            expand_bool::verify(&before, program, function, head)
        }
        Rule::ConvertAssert { function, head } => {
            convert_assert::verify(&before, program, function, head)
        }
        Rule::CollapseLoop { function, head } => {
            collapse_loop::verify(&before, program, function, head)
        }
        Rule::AssumeEq { function, a, b } => {
            assume_eq::verify(&before, program, function, parse_cell(a)?, parse_cell(b)?)
        }
        Rule::Inline { function, at, callee, captures } => inline::verify(
            &before,
            program,
            &entry.id,
            function,
            parse_cell(at)?,
            callee,
            &parse_cells(captures)?,
        ),
    }
    .with_context(|| format!("verifying {} ({})", entry.id, entry.rule.name()))?;
    timing.verify = clock.elapsed();

    Ok(StepReport {
        id: entry.id.clone(),
        rule: entry.rule.name(),
        timing,
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
