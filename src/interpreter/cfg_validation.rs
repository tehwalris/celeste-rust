//! CFG Validation
//!
//! This module provides validation for CFGs to catch issues like:
//! - References to undefined local IDs
//! - Phi nodes referencing non-existent predecessor blocks
//! - Branch targets pointing to non-existent blocks
//! - Duplicate local ID definitions

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};

type FxHashSet<T> = HashSet<T, BuildHasherDefault<FxHasher>>;
type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// Validation error types
#[derive(Debug, Clone)]
pub enum ValidationError {
    /// A local ID is used but never defined
    UndefinedLocal {
        used_in_block: String,
        local_id: LocalId,
        context: String,
    },
    /// A phi node references a block that doesn't exist
    PhiReferencesNonExistentBlock {
        block: String,
        phi_local: LocalId,
        referenced_block: Label,
    },
    /// A phi node references a block that isn't a predecessor
    PhiReferencesNonPredecessor {
        block: String,
        phi_local: LocalId,
        referenced_block: Label,
    },
    /// A branch target doesn't exist
    BranchTargetNotFound {
        from_block: String,
        target: Label,
    },
    /// A local ID is defined multiple times
    DuplicateDefinition {
        local_id: LocalId,
        first_block: String,
        second_block: String,
    },
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::UndefinedLocal { used_in_block, local_id, context } => {
                write!(f, "Undefined local %{} used in block '{}' ({})", usize::from(*local_id), used_in_block, context)
            }
            ValidationError::PhiReferencesNonExistentBlock { block, phi_local, referenced_block } => {
                write!(f, "Phi %{} in block '{}' references non-existent block '{}'", usize::from(*phi_local), block, referenced_block.as_str())
            }
            ValidationError::PhiReferencesNonPredecessor { block, phi_local, referenced_block } => {
                write!(f, "Phi %{} in block '{}' references '{}' which is not a predecessor", usize::from(*phi_local), block, referenced_block.as_str())
            }
            ValidationError::BranchTargetNotFound { from_block, target } => {
                write!(f, "Branch in block '{}' targets non-existent block '{}'", from_block, target.as_str())
            }
            ValidationError::DuplicateDefinition { local_id, first_block, second_block } => {
                write!(f, "Local %{} defined in both '{}' and '{}'", usize::from(*local_id), first_block, second_block)
            }
        }
    }
}

/// Result of CFG validation
#[derive(Debug)]
pub struct ValidationResult {
    pub errors: Vec<ValidationError>,
}

impl ValidationResult {
    pub fn is_valid(&self) -> bool {
        self.errors.is_empty()
    }
}

/// Validate a CFG for common issues
pub fn validate_cfg(cfg: &Cfg) -> ValidationResult {
    validate_cfg_with_args(cfg, &[])
}

/// Validate a CFG, treating the given local IDs as already defined (function arguments)
pub fn validate_cfg_with_args(cfg: &Cfg, arg_ids: &[LocalId]) -> ValidationResult {
    use crate::ir::ENTRY_BLOCK_LABEL;

    let mut errors = Vec::new();

    // Collect all block names
    let mut block_names: FxHashSet<String> = FxHashSet::default();
    // The entry block can be referenced by its canonical name in phi nodes
    block_names.insert(ENTRY_BLOCK_LABEL.to_string());
    for label in cfg.named.keys() {
        block_names.insert(label.as_str().to_string());
    }

    // Collect all defined locals and their defining blocks
    let mut defined_locals: FxHashMap<LocalId, String> = FxHashMap::default();

    // Function arguments are pre-defined (as if from "args" block)
    for arg_id in arg_ids {
        defined_locals.insert(*arg_id, "<args>".to_string());
    }

    // Check entry block
    collect_definitions(&cfg.entry, "entry", &mut defined_locals, &mut errors);

    // Check named blocks
    for (label, block) in &cfg.named {
        collect_definitions(block, label.as_str(), &mut defined_locals, &mut errors);
    }

    // Build predecessor map for phi validation
    let predecessors = build_predecessor_map(cfg);

    // Now validate uses
    validate_block_uses(&cfg.entry, "entry", &defined_locals, &block_names, &predecessors, &mut errors);
    for (label, block) in &cfg.named {
        validate_block_uses(block, label.as_str(), &defined_locals, &block_names, &predecessors, &mut errors);
    }

    // Validate branch targets
    validate_branch_targets(&cfg.entry, "entry", &block_names, &mut errors);
    for (label, block) in &cfg.named {
        validate_branch_targets(block, label.as_str(), &block_names, &mut errors);
    }

    ValidationResult { errors }
}

/// Collect all local definitions in a block
fn collect_definitions(
    block: &Block,
    block_name: &str,
    defined_locals: &mut FxHashMap<LocalId, String>,
    errors: &mut Vec<ValidationError>,
) {
    for (local_id, _) in &block.instructions {
        if let Some(first_block) = defined_locals.get(local_id) {
            errors.push(ValidationError::DuplicateDefinition {
                local_id: *local_id,
                first_block: first_block.clone(),
                second_block: block_name.to_string(),
            });
        } else {
            defined_locals.insert(*local_id, block_name.to_string());
        }
    }

    // Terminator also defines a local
    let (term_local, _) = &block.terminator;
    if let Some(first_block) = defined_locals.get(term_local) {
        errors.push(ValidationError::DuplicateDefinition {
            local_id: *term_local,
            first_block: first_block.clone(),
            second_block: block_name.to_string(),
        });
    } else {
        defined_locals.insert(*term_local, block_name.to_string());
    }
}

/// Build a map from block name to its predecessors
fn build_predecessor_map(cfg: &Cfg) -> FxHashMap<String, FxHashSet<String>> {
    use crate::ir::ENTRY_BLOCK_LABEL;

    let mut predecessors: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();

    // Initialize all blocks with empty predecessor sets
    // The entry block is referenced by ENTRY_BLOCK_LABEL in phi nodes
    predecessors.insert(ENTRY_BLOCK_LABEL.to_string(), FxHashSet::default());
    for label in cfg.named.keys() {
        predecessors.insert(label.as_str().to_string(), FxHashSet::default());
    }

    // Add predecessors from entry block
    add_predecessors_from_terminator(&cfg.entry.terminator.1, ENTRY_BLOCK_LABEL, &mut predecessors);

    // Add predecessors from named blocks
    for (label, block) in &cfg.named {
        add_predecessors_from_terminator(&block.terminator.1, label.as_str(), &mut predecessors);
    }

    predecessors
}

/// Add predecessor edges from a terminator
fn add_predecessors_from_terminator(
    terminator: &Terminator,
    from_block: &str,
    predecessors: &mut FxHashMap<String, FxHashSet<String>>,
) {
    match terminator {
        Terminator::Return { .. } => {}
        Terminator::UnconditionalBranch { target } => {
            if let Some(preds) = predecessors.get_mut(target.as_str()) {
                preds.insert(from_block.to_string());
            }
        }
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            if let Some(preds) = predecessors.get_mut(true_target.as_str()) {
                preds.insert(from_block.to_string());
            }
            if let Some(preds) = predecessors.get_mut(false_target.as_str()) {
                preds.insert(from_block.to_string());
            }
        }
    }
}

/// Validate that all used locals are defined
fn validate_block_uses(
    block: &Block,
    block_name: &str,
    defined_locals: &FxHashMap<LocalId, String>,
    block_names: &FxHashSet<String>,
    predecessors: &FxHashMap<String, FxHashSet<String>>,
    errors: &mut Vec<ValidationError>,
) {
    for (local_id, instruction) in &block.instructions {
        // Check instruction operands
        let used_locals = get_used_locals(instruction);
        for used in used_locals {
            if !defined_locals.contains_key(&used) {
                errors.push(ValidationError::UndefinedLocal {
                    used_in_block: block_name.to_string(),
                    local_id: used,
                    context: format!("instruction %{}", usize::from(*local_id)),
                });
            }
        }

        // Special check for phi nodes
        if let Instruction::Phi { branches } = instruction {
            let block_preds = predecessors.get(block_name);
            for (label, _) in branches {
                // Check that referenced block exists
                if !block_names.contains(label.as_str()) {
                    errors.push(ValidationError::PhiReferencesNonExistentBlock {
                        block: block_name.to_string(),
                        phi_local: *local_id,
                        referenced_block: label.clone(),
                    });
                }
                // Check that referenced block is a predecessor
                else if let Some(preds) = block_preds {
                    if !preds.contains(label.as_str()) {
                        errors.push(ValidationError::PhiReferencesNonPredecessor {
                            block: block_name.to_string(),
                            phi_local: *local_id,
                            referenced_block: label.clone(),
                        });
                    }
                }
            }
        }
    }

    // Check terminator operands
    let term_used = get_terminator_used_locals(&block.terminator.1);
    for used in term_used {
        if !defined_locals.contains_key(&used) {
            errors.push(ValidationError::UndefinedLocal {
                used_in_block: block_name.to_string(),
                local_id: used,
                context: "terminator".to_string(),
            });
        }
    }
}

/// Validate that branch targets exist
fn validate_branch_targets(
    block: &Block,
    block_name: &str,
    block_names: &FxHashSet<String>,
    errors: &mut Vec<ValidationError>,
) {
    match &block.terminator.1 {
        Terminator::Return { .. } => {}
        Terminator::UnconditionalBranch { target } => {
            if !block_names.contains(target.as_str()) {
                errors.push(ValidationError::BranchTargetNotFound {
                    from_block: block_name.to_string(),
                    target: target.clone(),
                });
            }
        }
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            if !block_names.contains(true_target.as_str()) {
                errors.push(ValidationError::BranchTargetNotFound {
                    from_block: block_name.to_string(),
                    target: true_target.clone(),
                });
            }
            if !block_names.contains(false_target.as_str()) {
                errors.push(ValidationError::BranchTargetNotFound {
                    from_block: block_name.to_string(),
                    target: false_target.clone(),
                });
            }
        }
    }
}

/// Get all local IDs used by an instruction
fn get_used_locals(instruction: &Instruction) -> Vec<LocalId> {
    let mut result = Vec::new();
    match instruction {
        Instruction::Alloc => {}
        Instruction::GetGlobal { .. } => {}
        Instruction::Load { source } => {
            result.push(*source);
        }
        Instruction::Store { target, source } => {
            result.push(*target);
            result.push(*source);
        }
        Instruction::StoreEmptyTable { target } => {
            result.push(*target);
        }
        Instruction::StoreClosure { target, captures, .. } => {
            result.push(*target);
            result.extend(captures.iter().copied());
        }
        Instruction::GetField { receiver, .. } => {
            result.push(*receiver);
        }
        Instruction::GetIndex { receiver, index, .. } => {
            result.push(*receiver);
            result.push(*index);
        }
        Instruction::NumberConstant { .. } => {}
        Instruction::BoolConstant { .. } => {}
        Instruction::StringConstant { .. } => {}
        Instruction::NilConstant => {}
        Instruction::BinaryOp { left, right, .. } => {
            result.push(*left);
            result.push(*right);
        }
        Instruction::UnaryOp { arg, .. } => {
            result.push(*arg);
        }
        Instruction::Call { closure, args } => {
            result.push(*closure);
            result.extend(args.iter().copied());
        }
        Instruction::CallResolved { captures, args, .. } => {
            result.extend(captures.iter().copied());
            result.extend(args.iter().copied());
        }
        Instruction::CallBuiltin { args, .. } => {
            result.extend(args.iter().copied());
        }
        Instruction::Phi { branches } => {
            for (_, local) in branches {
                result.push(*local);
            }
        }
    }
    result
}

/// Get all local IDs used by a terminator
fn get_terminator_used_locals(terminator: &Terminator) -> Vec<LocalId> {
    match terminator {
        Terminator::Return { value: Some(v) } => vec![*v],
        Terminator::Return { value: None } => vec![],
        Terminator::UnconditionalBranch { .. } => vec![],
        Terminator::ConditionalBranch { condition, .. } => vec![*condition],
    }
}

/// Validate a CFG and panic with details if invalid
pub fn assert_valid_cfg(cfg: &Cfg, context: &str) {
    assert_valid_cfg_with_args(cfg, &[], context)
}

/// Validate a CFG with function arguments and panic with details if invalid
pub fn assert_valid_cfg_with_args(cfg: &Cfg, arg_ids: &[LocalId], context: &str) {
    let result = validate_cfg_with_args(cfg, arg_ids);
    if !result.is_valid() {
        let mut msg = format!("CFG validation failed ({}): {} errors\n", context, result.errors.len());
        for error in &result.errors {
            msg.push_str(&format!("  - {}\n", error));
        }
        panic!("{}", msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};
    use crate::pico8_num::Pico8Num;

    #[test]
    fn test_valid_simple_cfg() {
        let cfg = Cfg {
            entry: Block {
                instructions: vec![
                    (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(42) }),
                ],
                terminator: (LocalId::from(1), Terminator::Return { value: Some(LocalId::from(0)) }),
                hint_normalize: false,
            },
            named: Default::default(),
        };

        let result = validate_cfg(&cfg);
        assert!(result.is_valid(), "Expected valid CFG, got errors: {:?}", result.errors);
    }

    #[test]
    fn test_undefined_local() {
        let cfg = Cfg {
            entry: Block {
                instructions: vec![],
                terminator: (LocalId::from(0), Terminator::Return { value: Some(LocalId::from(99)) }), // 99 is undefined
                hint_normalize: false,
            },
            named: Default::default(),
        };

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::UndefinedLocal { local_id, .. } if usize::from(*local_id) == 99)));
    }

    #[test]
    fn test_duplicate_definition() {
        type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

        let mut named = FxHashMap::default();
        named.insert(
            Label::from("block1".to_string()),
            Block {
                instructions: vec![
                    (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(1) }), // Same ID as entry
                ],
                terminator: (LocalId::from(2), Terminator::Return { value: Some(LocalId::from(0)) }),
                hint_normalize: false,
            },
        );

        let cfg = Cfg {
            entry: Block {
                instructions: vec![
                    (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(42) }),
                ],
                terminator: (LocalId::from(1), Terminator::UnconditionalBranch { target: Label::from("block1".to_string()) }),
                hint_normalize: false,
            },
            named,
        };

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::DuplicateDefinition { local_id, .. } if usize::from(*local_id) == 0)));
    }

    #[test]
    fn test_phi_references_nonexistent_block() {
        type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

        let mut named = FxHashMap::default();
        named.insert(
            Label::from("join".to_string()),
            Block {
                instructions: vec![
                    (LocalId::from(2), Instruction::Phi {
                        branches: vec![
                            (Label::from("nonexistent".to_string()), LocalId::from(0)),
                        ],
                    }),
                ],
                terminator: (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
                hint_normalize: false,
            },
        );

        let cfg = Cfg {
            entry: Block {
                instructions: vec![
                    (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(42) }),
                ],
                terminator: (LocalId::from(1), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
                hint_normalize: false,
            },
            named,
        };

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::PhiReferencesNonExistentBlock { .. })));
    }

    #[test]
    fn test_branch_target_not_found() {
        let cfg = Cfg {
            entry: Block {
                instructions: vec![],
                terminator: (LocalId::from(0), Terminator::UnconditionalBranch { target: Label::from("nonexistent".to_string()) }),
                hint_normalize: false,
            },
            named: Default::default(),
        };

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::BranchTargetNotFound { .. })));
    }
}
