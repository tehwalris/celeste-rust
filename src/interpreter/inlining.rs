//! Inlining Pass: Inline resolved calls into their callers.
//!
//! This pass transforms `CallResolved` instructions by substituting the callee's
//! CFG inline. For example:
//! ```text
//! %5 = CallResolved(foo_1, captures=[], args=[%2, %3])
//! ```
//! Becomes the inlined body of `foo_1` with:
//! - All locals renumbered to avoid conflicts
//! - Captures mapped to the provided capture values
//! - Arguments mapped to the provided argument values
//! - Returns converted to assignments + branches to a continuation block

use crate::interpreter::common::FxHashMap;
use crate::ir::{
    Block, Cfg, FunDef, GlobalId, Instruction, Label, LabelGenerator, LocalId, LocalIdGenerator,
    Terminator,
};

/// Result from inlining a single block's body.
/// Contains:
/// - The remapped instructions
/// - The terminator (LocalId, Terminator pair)
/// - Optional return info: (label of this block, return value local) if this block returns
type InlineBlockBodyResult = (
    Vec<(LocalId, Instruction)>,
    (LocalId, Terminator),
    Option<(Label, LocalId)>,
);

/// Result of the inlining pass.
#[derive(Debug)]
pub enum InliningResult {
    /// Successfully inlined some calls
    Success {
        cfg: Cfg,
        /// Number of calls inlined
        calls_inlined: usize,
    },
    /// No calls could be inlined
    NoChange,
}

/// Context for inlining a single function call.
struct InlineContext<'a> {
    /// Mapping from callee local IDs to caller local IDs
    local_mapping: FxHashMap<LocalId, LocalId>,
    /// Mapping from callee labels to caller labels
    label_mapping: FxHashMap<Label, Label>,
    /// Local ID generator for creating new locals in caller
    local_gen: &'a mut LocalIdGenerator,
    /// Label generator for creating new labels in caller
    label_gen: &'a mut LabelGenerator,
}

impl<'a> InlineContext<'a> {
    fn new(local_gen: &'a mut LocalIdGenerator, label_gen: &'a mut LabelGenerator) -> Self {
        InlineContext {
            local_mapping: FxHashMap::default(),
            label_mapping: FxHashMap::default(),
            local_gen,
            label_gen,
        }
    }

    /// Get or create a caller local ID for a callee local ID.
    fn map_local(&mut self, callee_local: LocalId) -> LocalId {
        *self
            .local_mapping
            .entry(callee_local)
            .or_insert_with(|| self.local_gen.fresh_id())
    }

    /// Get or create a caller label for a callee label.
    fn map_label(&mut self, callee_label: &Label) -> Label {
        self.label_mapping
            .entry(callee_label.clone())
            .or_insert_with(|| self.label_gen.next("inlined"))
            .clone()
    }

    /// Set up a mapping from callee local to a specific caller local (for captures/args).
    fn set_local_mapping(&mut self, callee_local: LocalId, caller_local: LocalId) {
        self.local_mapping.insert(callee_local, caller_local);
    }

    /// Remap an instruction, translating all local IDs and labels (for phi nodes).
    fn remap_instruction(&mut self, instruction: &Instruction) -> Instruction {
        let remapped = instruction.map_local_ids(|id| self.map_local(id));
        // Also remap labels in phi nodes
        match remapped {
            Instruction::Phi { branches } => Instruction::Phi {
                branches: branches
                    .into_iter()
                    .map(|(label, id)| (self.map_label(&label), id))
                    .collect(),
            },
            other => other,
        }
    }

    /// Remap a terminator, translating local IDs and labels.
    fn remap_terminator(&mut self, terminator: &Terminator) -> Terminator {
        let remapped = terminator.map_local_ids(|id| self.map_local(id));
        match remapped {
            Terminator::Return { value } => Terminator::Return { value },
            Terminator::UnconditionalBranch { target } => Terminator::UnconditionalBranch {
                target: self.map_label(&target),
            },
            Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } => Terminator::ConditionalBranch {
                condition,
                true_target: self.map_label(&true_target),
                false_target: self.map_label(&false_target),
            },
            Terminator::Deopt { reason } => Terminator::Deopt { reason },
        }
    }
}

/// Run the inlining pass on a CFG.
///
/// This looks for `CallResolved` instructions and inlines the callee's CFG.
/// The function definitions must be provided to look up callees.
pub fn inline_calls(
    cfg: &Cfg,
    fun_defs: &FxHashMap<GlobalId, FunDef>,
    local_gen: &mut LocalIdGenerator,
    label_gen: &mut LabelGenerator,
) -> InliningResult {
    let mut total_inlined = 0;

    // We need to process blocks iteratively because inlining adds new blocks.
    // Start with entry block, then process named blocks.

    let mut new_entry = cfg.entry.clone();
    let mut new_named: FxHashMap<Label, Block> = cfg.named.clone();

    // Process entry block
    let (entry_result, entry_inlined, entry_new_blocks) =
        inline_calls_in_block(&new_entry, fun_defs, local_gen, label_gen);
    new_entry = entry_result;
    total_inlined += entry_inlined;
    for (label, block) in entry_new_blocks {
        new_named.insert(label, block);
    }

    // Process named blocks - we need to iterate over existing keys to avoid modifying while iterating
    let block_labels: Vec<Label> = new_named.keys().cloned().collect();
    for label in block_labels {
        let block = new_named.get(&label).unwrap().clone();
        let (new_block, block_inlined, block_new_blocks) =
            inline_calls_in_block(&block, fun_defs, local_gen, label_gen);
        total_inlined += block_inlined;
        new_named.insert(label, new_block);
        for (new_label, new_block) in block_new_blocks {
            new_named.insert(new_label, new_block);
        }
    }

    if total_inlined == 0 {
        return InliningResult::NoChange;
    }

    InliningResult::Success {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        calls_inlined: total_inlined,
    }
}

/// Inline calls in a single block.
///
/// Returns:
/// - The modified block (with calls replaced)
/// - Number of calls inlined
/// - New blocks created from inlining (callee bodies)
fn inline_calls_in_block(
    block: &Block,
    fun_defs: &FxHashMap<GlobalId, FunDef>,
    local_gen: &mut LocalIdGenerator,
    label_gen: &mut LabelGenerator,
) -> (Block, usize, Vec<(Label, Block)>) {
    let mut new_instructions = Vec::new();
    let mut new_blocks = Vec::new();
    let mut inlined_count = 0;

    // We need to track which instruction index we're splitting at when we inline
    let mut i = 0;
    while i < block.instructions.len() {
        let (local_id, instruction) = &block.instructions[i];

        match instruction {
            Instruction::CallResolved {
                fun_name,
                captures,
                args,
            } => {
                // Look up the function definition
                if let Some(callee) = fun_defs.get(fun_name) {
                    // Create labels before creating InlineContext to avoid borrow conflicts
                    let continuation_label = label_gen.next("cont");
                    let entry_label = label_gen.next("inlined_entry");

                    // Create inline context
                    let mut ctx = InlineContext::new(local_gen, label_gen);

                    // Map the callee's entry block label to the new inlined entry label
                    // This is needed for phi nodes in the callee that reference the entry block
                    ctx.label_mapping.insert(Label::entry(), entry_label.clone());

                    // Map captures: callee capture IDs -> caller capture locals
                    for (callee_capture_id, caller_capture_local) in
                        callee.capture_ids.iter().zip(captures.iter())
                    {
                        ctx.set_local_mapping(*callee_capture_id, *caller_capture_local);
                    }

                    // Map arguments: callee arg IDs -> caller arg locals
                    // In Lua, if fewer args are provided than parameters, the extras are nil
                    for (i, callee_arg_opt) in callee.arg_ids.iter().enumerate() {
                        if let Some(callee_arg_id) = callee_arg_opt {
                            if i < args.len() {
                                // Arg provided by caller
                                ctx.set_local_mapping(*callee_arg_id, args[i]);
                            } else {
                                // Missing arg - create a nil constant
                                let nil_local = ctx.local_gen.fresh_id();
                                new_instructions.push((nil_local, Instruction::NilConstant));
                                ctx.set_local_mapping(*callee_arg_id, nil_local);
                            }
                        }
                    }

                    // Collect return info from all blocks for the phi node
                    let mut return_infos: Vec<(Label, LocalId)> = Vec::new();

                    // Inline the entry block of the callee as a separate block
                    // (We used to merge it into the current block, but this caused
                    // issues with phi nodes referencing a non-existent block label)
                    let (inlined_entry_instrs, inlined_entry_term, entry_return_info) =
                        inline_block_body(&callee.cfg.entry, &mut ctx, &entry_label, &continuation_label);

                    if let Some(info) = entry_return_info {
                        return_infos.push(info);
                    }

                    // Create a separate block for the inlined entry
                    let inlined_entry_block = Block {
                        instructions: inlined_entry_instrs,
                        terminator: inlined_entry_term,
                        hint_normalize: callee.cfg.entry.hint_normalize,
                    };
                    new_blocks.push((entry_label.clone(), inlined_entry_block));

                    // Create inlined named blocks
                    for (callee_label, callee_block) in &callee.cfg.named {
                        let caller_label = ctx.map_label(callee_label);
                        let (inlined_instrs, inlined_term, block_return_info) =
                            inline_block_body(callee_block, &mut ctx, &caller_label, &continuation_label);

                        if let Some(info) = block_return_info {
                            return_infos.push(info);
                        }

                        let inlined_block = Block {
                            instructions: inlined_instrs,
                            terminator: inlined_term,
                            hint_normalize: callee_block.hint_normalize,
                        };
                        new_blocks.push((caller_label, inlined_block));
                    }

                    // Now we need to handle the rest of the current block's instructions
                    // after the call. These go into the continuation block.
                    let remaining_instructions: Vec<_> =
                        block.instructions[i + 1..].to_vec();

                    // Build the continuation block with a phi node for the return value
                    let mut continuation_instructions = Vec::new();

                    // Add phi node for return value if there are return points
                    if !return_infos.is_empty() {
                        continuation_instructions.push((
                            *local_id,
                            Instruction::Phi { branches: return_infos },
                        ));
                    }

                    continuation_instructions.extend(remaining_instructions);

                    let continuation_block = Block {
                        instructions: continuation_instructions,
                        terminator: block.terminator.clone(),
                        hint_normalize: block.hint_normalize,
                    };
                    new_blocks.push((continuation_label.clone(), continuation_block));

                    // The current block now branches to the inlined entry block
                    let result_block = Block {
                        instructions: new_instructions,
                        terminator: (
                            ctx.local_gen.fresh_id(),
                            Terminator::UnconditionalBranch { target: entry_label.clone() },
                        ),
                        hint_normalize: block.hint_normalize,
                    };

                    inlined_count += 1;
                    return (result_block, inlined_count, new_blocks);
                } else {
                    // Function not found - keep the call as is
                    new_instructions.push((*local_id, instruction.clone()));
                }
            }
            _ => {
                new_instructions.push((*local_id, instruction.clone()));
            }
        }
        i += 1;
    }

    // No inlining happened - return the block unchanged
    (block.with_instructions(new_instructions), inlined_count, new_blocks)
}

/// Inline a block's body (instructions and terminator).
fn inline_block_body(
    block: &Block,
    ctx: &mut InlineContext,
    block_label: &Label,
    continuation_label: &Label,
) -> InlineBlockBodyResult {
    // Remap instructions
    let mut new_instructions = Vec::new();
    for (callee_local, instruction) in &block.instructions {
        let caller_local = ctx.map_local(*callee_local);
        let remapped_instruction = ctx.remap_instruction(instruction);
        new_instructions.push((caller_local, remapped_instruction));
    }

    // Remap terminator, converting Return to branch to continuation
    let (term_local, terminator) = &block.terminator;
    let caller_term_local = ctx.map_local(*term_local);

    let (new_terminator, return_info) = match terminator {
        Terminator::Return { value } => {
            // Get the return value local (or create a nil constant)
            let return_value_local = if let Some(return_value_local) = value {
                ctx.map_local(*return_value_local)
            } else {
                // No return value - create a nil constant
                let nil_local = ctx.local_gen.fresh_id();
                new_instructions.push((nil_local, Instruction::NilConstant));
                nil_local
            };

            // Branch to continuation
            let term = (
                caller_term_local,
                Terminator::UnconditionalBranch {
                    target: continuation_label.clone(),
                },
            );

            // Record this return point for the phi node
            (term, Some((block_label.clone(), return_value_local)))
        }
        Terminator::UnconditionalBranch { target } => {
            let mapped_target = ctx.map_label(target);
            (
                (caller_term_local, Terminator::UnconditionalBranch { target: mapped_target }),
                None,
            )
        }
        Terminator::ConditionalBranch {
            condition,
            true_target,
            false_target,
        } => {
            let mapped_condition = ctx.map_local(*condition);
            let mapped_true = ctx.map_label(true_target);
            let mapped_false = ctx.map_label(false_target);
            (
                (
                    caller_term_local,
                    Terminator::ConditionalBranch {
                        condition: mapped_condition,
                        true_target: mapped_true,
                        false_target: mapped_false,
                    },
                ),
                None,
            )
        }
        Terminator::Deopt { reason } => {
            // Deopt is preserved as-is during inlining
            (
                (caller_term_local, Terminator::Deopt { reason: reason.clone() }),
                None,
            )
        }
    };

    (new_instructions, new_terminator, return_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    fn make_simple_callee() -> FunDef {
        // A simple function: fn(x) -> x + 1
        // %0 = arg x
        // %1 = NumberConstant(1)
        // %2 = BinaryOp(%0, Plus, %1)
        // return %2
        let entry = Block::new_for_test(
            vec![
                (
                    LocalId::from(1),
                    Instruction::NumberConstant {
                        value: Pico8Num::from_i16(1),
                    },
                ),
                (
                    LocalId::from(2),
                    Instruction::BinaryOp {
                        left: LocalId::from(0),
                        op: crate::ir::BinaryOp::Plus,
                        right: LocalId::from(1),
                    },
                ),
            ],
            (
                LocalId::from(3),
                Terminator::Return {
                    value: Some(LocalId::from(2)),
                },
            ),
        );

        FunDef {
            name: GlobalId::from("add_one_1".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(LocalId::from(0))],
            cfg: Cfg::single_entry(entry),
            source_span: None,
        }
    }

    fn make_caller_cfg() -> Cfg {
        // Caller: %0 = NumberConstant(5); %1 = CallResolved(add_one_1, [], [%0]); return %1
        let entry = Block::new_for_test(
            vec![
                (
                    LocalId::from(0),
                    Instruction::NumberConstant {
                        value: Pico8Num::from_i16(5),
                    },
                ),
                (
                    LocalId::from(1),
                    Instruction::CallResolved {
                        fun_name: GlobalId::from("add_one_1".to_string()),
                        captures: vec![],
                        args: vec![LocalId::from(0)],
                    },
                ),
            ],
            (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
        );

        Cfg::single_entry(entry)
    }

    #[test]
    fn test_inline_simple_call() {
        let callee = make_simple_callee();
        let caller = make_caller_cfg();

        let mut fun_defs = FxHashMap::default();
        fun_defs.insert(callee.name.clone(), callee);

        let mut local_gen = LocalIdGenerator::new();
        // Skip the first few IDs that the caller already uses
        for _ in 0..10 {
            local_gen.fresh_id();
        }
        let mut label_gen = LabelGenerator::new();

        let result = inline_calls(&caller, &fun_defs, &mut local_gen, &mut label_gen);

        match result {
            InliningResult::Success { cfg, calls_inlined } => {
                assert_eq!(calls_inlined, 1);
                // The entry block should have been modified
                // It should now contain the inlined instructions
                println!("Entry block instructions: {:?}", cfg.entry.instructions);
                println!("Entry block terminator: {:?}", cfg.entry.terminator);
                println!("Named blocks: {:?}", cfg.named.keys().collect::<Vec<_>>());

                // Check that there's a continuation block
                assert!(cfg.named.len() >= 1, "Should have at least one continuation block");
            }
            InliningResult::NoChange => {
                panic!("Expected inlining to succeed");
            }
        }
    }

    #[test]
    fn test_no_inline_unknown_function() {
        let caller = make_caller_cfg();

        // Empty fun_defs - function not found
        let fun_defs = FxHashMap::default();

        let mut local_gen = LocalIdGenerator::new();
        let mut label_gen = LabelGenerator::new();

        let result = inline_calls(&caller, &fun_defs, &mut local_gen, &mut label_gen);

        assert!(matches!(result, InliningResult::NoChange));
    }
}
