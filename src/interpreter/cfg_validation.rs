//! CFG Validation
//!
//! This module provides validation for CFGs to catch issues like:
//! - References to undefined local IDs
//! - Phi nodes referencing non-existent predecessor blocks
//! - Branch targets pointing to non-existent blocks
//! - Duplicate local ID definitions
//! - Type mismatches (e.g., Load from a value instead of pointer)

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{Block, BlockId, Cfg, Instruction, Label, LocalId};

/// The "type" of a value in SSA form - used for validation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaType {
    /// A pointer/reference that can be dereferenced with Load
    Pointer,
    /// A value (number, bool, string, nil, or computed result)
    Value,
    /// Unknown - for args or when type cannot be determined
    Unknown,
}

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
    /// Load from a non-pointer (e.g., Load from Phi result which is a value)
    LoadFromNonPointer {
        block: String,
        load_local: LocalId,
        source_local: LocalId,
        source_type: SsaType,
        source_instruction: String,
    },
    /// Store to a non-pointer
    StoreToNonPointer {
        block: String,
        store_local: LocalId,
        target_local: LocalId,
        target_type: SsaType,
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
            ValidationError::LoadFromNonPointer { block, load_local, source_local, source_type, source_instruction } => {
                write!(f, "Load %{} in block '{}' reads from %{} which is {:?} ({}), not a pointer",
                    usize::from(*load_local), block, usize::from(*source_local), source_type, source_instruction)
            }
            ValidationError::StoreToNonPointer { block, store_local, target_local, target_type } => {
                write!(f, "Store %{} in block '{}' writes to %{} which is {:?}, not a pointer",
                    usize::from(*store_local), block, usize::from(*target_local), target_type)
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

    // Collect definitions from all blocks
    for (label, block) in cfg.iter_blocks_with_label() {
        collect_definitions(block, label, &mut defined_locals, &mut errors);
    }

    // Build predecessor map for phi validation
    let predecessors = build_predecessor_map(cfg);

    // Validate uses and branch targets for all blocks
    for (label, block) in cfg.iter_blocks_with_label() {
        validate_block_uses(block, label, &defined_locals, &block_names, &predecessors, &mut errors);
        validate_branch_targets(block, label, &block_names, &mut errors);
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

/// Build a map from block name to its predecessors.
///
/// Uses the centralized `Cfg::compute_predecessors()` and converts to string-based keys
/// for compatibility with the validation logic.
fn build_predecessor_map(cfg: &Cfg) -> FxHashMap<String, FxHashSet<String>> {
    let block_preds = cfg.compute_predecessors();
    let mut result: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();

    for (block_id, preds) in block_preds {
        let block_name = match &block_id {
            BlockId::Entry => continue, // Entry block has no predecessors; skip it
            BlockId::Named(label) => label.as_str().to_string(),
        };
        let pred_names: FxHashSet<String> = preds
            .into_iter()
            .map(|p| p.label().as_str().to_string())
            .collect();
        result.insert(block_name, pred_names);
    }

    result
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
        for used in instruction.get_used_locals() {
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
    for used in block.terminator_kind().get_used_locals() {
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
    for target in block.terminator_kind().get_successor_labels() {
        if !block_names.contains(target.as_str()) {
            errors.push(ValidationError::BranchTargetNotFound {
                from_block: block_name.to_string(),
                target: target.clone(),
            });
        }
    }
}


/// Classify an instruction by its SSA type (what kind of value it produces)
fn classify_instruction(instruction: &Instruction) -> SsaType {
    match instruction {
        // Pointer-producing instructions
        Instruction::Alloc => SsaType::Pointer,
        Instruction::GetGlobal { .. } => SsaType::Pointer,
        Instruction::GetField { .. } => SsaType::Pointer,
        Instruction::GetIndex { .. } => SsaType::Pointer,

        // Value-producing instructions
        Instruction::NumberConstant { .. } => SsaType::Value,
        Instruction::BoolConstant { .. } => SsaType::Value,
        Instruction::StringConstant { .. } => SsaType::Value,
        Instruction::NilConstant => SsaType::Value,
        Instruction::BinaryOp { .. } => SsaType::Value,
        Instruction::UnaryOp { .. } => SsaType::Value,
        Instruction::Load { .. } => SsaType::Value, // Load dereferences a pointer to get a value
        Instruction::Phi { .. } => SsaType::Value, // Phi merges values (in SSA form after heap elim)

        // These don't produce meaningful values for Load/Store purposes
        Instruction::Store { .. } => SsaType::Value, // Store returns unit/nil
        Instruction::StoreEmptyTable { .. } => SsaType::Value,
        Instruction::StoreClosure { .. } => SsaType::Value,

        // Calls can return anything - we don't know statically
        Instruction::Call { .. } => SsaType::Unknown,
        Instruction::CallResolved { .. } => SsaType::Unknown,
        Instruction::CallBuiltin { .. } => SsaType::Unknown,
    }
}

/// Validate type constraints in a CFG (e.g., Load must read from a pointer)
pub fn validate_types(cfg: &Cfg, arg_types: &[(LocalId, SsaType)]) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    // Build a map of local ID -> (SsaType, description)
    let mut local_types: FxHashMap<LocalId, (SsaType, String)> = FxHashMap::default();

    // Add argument types
    for (local_id, ssa_type) in arg_types {
        local_types.insert(*local_id, (*ssa_type, "<arg>".to_string()));
    }

    // Collect types from all blocks
    for block in cfg.iter_blocks() {
        collect_types(block, &mut local_types);
    }

    // Validate types in all blocks
    for (label, block) in cfg.iter_blocks_with_label() {
        validate_block_types(block, label, &local_types, &mut errors);
    }

    errors
}

/// Collect SSA types from a block
fn collect_types(block: &Block, local_types: &mut FxHashMap<LocalId, (SsaType, String)>) {
    for (local_id, instruction) in &block.instructions {
        let ssa_type = classify_instruction(instruction);
        let description = instruction.describe();
        local_types.insert(*local_id, (ssa_type, description));
    }
}

/// Validate type constraints in a block
fn validate_block_types(
    block: &Block,
    block_name: &str,
    local_types: &FxHashMap<LocalId, (SsaType, String)>,
    errors: &mut Vec<ValidationError>,
) {
    for (local_id, instruction) in &block.instructions {
        match instruction {
            Instruction::Load { source } => {
                if let Some((source_type, source_desc)) = local_types.get(source) {
                    if *source_type == SsaType::Value {
                        errors.push(ValidationError::LoadFromNonPointer {
                            block: block_name.to_string(),
                            load_local: *local_id,
                            source_local: *source,
                            source_type: *source_type,
                            source_instruction: source_desc.clone(),
                        });
                    }
                }
            }
            Instruction::Store { target, .. } => {
                if let Some((target_type, _)) = local_types.get(target) {
                    if *target_type == SsaType::Value {
                        errors.push(ValidationError::StoreToNonPointer {
                            block: block_name.to_string(),
                            store_local: *local_id,
                            target_local: *target,
                            target_type: *target_type,
                        });
                    }
                }
            }
            _ => {}
        }
    }
}

/// Validate a CFG fully (structural + type checks)
pub fn validate_cfg_full(cfg: &Cfg, arg_ids: &[LocalId], arg_types: &[(LocalId, SsaType)]) -> ValidationResult {
    let mut result = validate_cfg_with_args(cfg, arg_ids);

    // Add type validation errors
    let type_errors = validate_types(cfg, arg_types);
    result.errors.extend(type_errors);

    result
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

    #[test]
    fn test_valid_simple_cfg() {
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(42)),
            ],
            (LocalId::from(1), Terminator::ret(Some(LocalId::from(0)))),
        ));

        let result = validate_cfg(&cfg);
        assert!(result.is_valid(), "Expected valid CFG, got errors: {:?}", result.errors);
    }

    #[test]
    fn test_undefined_local() {
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![],
            (LocalId::from(0), Terminator::ret(Some(LocalId::from(99)))), // 99 is undefined
        ));

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::UndefinedLocal { local_id, .. } if usize::from(*local_id) == 99)));
    }

    #[test]
    fn test_duplicate_definition() {
        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(42))],
            (LocalId::from(1), Terminator::branch("block1")),
        );
        let block1 = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(1)), // Same ID as entry
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(0)))),
        );

        let cfg = Cfg::with_blocks(entry, [("block1", block1)]);

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::DuplicateDefinition { local_id, .. } if usize::from(*local_id) == 0)));
    }

    #[test]
    fn test_phi_references_nonexistent_block() {
        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(42))],
            (LocalId::from(1), Terminator::branch("join")),
        );
        let join = Block::new_for_test(
            vec![(
                LocalId::from(2),
                Instruction::phi(vec![(Label::from("nonexistent".to_string()), LocalId::from(0))]),
            )],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        );

        let cfg = Cfg::with_blocks(entry, [("join", join)]);

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::PhiReferencesNonExistentBlock { .. })));
    }

    #[test]
    fn test_branch_target_not_found() {
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![],
            (LocalId::from(0), Terminator::branch("nonexistent")),
        ));

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid());
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::BranchTargetNotFound { .. })));
    }

    #[test]
    fn test_load_from_phi_is_type_error() {
        // This tests the Load(Phi) pattern that heap elimination produces
        // Phi produces a VALUE, not a pointer, so Load(Phi) is a type error
        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(42))],
            (LocalId::from(1), Terminator::branch("join")),
        );
        let other = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(99))],
            (LocalId::from(6), Terminator::branch("join")),
        );
        let join = Block::new_for_test(
            vec![
                // Phi merges values from two branches
                (
                    LocalId::from(3),
                    Instruction::phi(vec![
                        (Label::from("__entry".to_string()), LocalId::from(0)),
                        (Label::from("other".to_string()), LocalId::from(2)),
                    ]),
                ),
                // This is the type error being tested: Load from Phi result.
                // Phi produces a VALUE, not a pointer, so this should be flagged.
                (LocalId::from(4), Instruction::load(LocalId::from(3))),
            ],
            (LocalId::from(5), Terminator::ret(Some(LocalId::from(4)))),
        );

        let cfg = Cfg::with_blocks(entry, [("other", other), ("join", join)]);

        // Structural validation should pass
        let structural_result = validate_cfg(&cfg);
        assert!(structural_result.is_valid(), "Structural validation should pass: {:?}", structural_result.errors);

        // Type validation should catch Load(Phi)
        let type_errors = validate_types(&cfg, &[]);
        assert!(!type_errors.is_empty(), "Expected type errors for Load(Phi)");
        assert!(type_errors.iter().any(|e| matches!(e, ValidationError::LoadFromNonPointer { source_instruction, .. } if source_instruction.contains("Phi"))),
            "Expected LoadFromNonPointer error for Phi source: {:?}", type_errors);
    }

    #[test]
    fn test_load_from_alloc_is_valid() {
        // Load from Alloc is valid - Alloc produces a pointer
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::Alloc),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        ));

        let type_errors = validate_types(&cfg, &[]);
        assert!(type_errors.is_empty(), "Load from Alloc should be valid: {:?}", type_errors);
    }

    #[test]
    fn test_load_from_getfield_is_valid() {
        // Load from GetField is valid - GetField produces a pointer
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::Alloc),
                (LocalId::from(1), Instruction::get_field(LocalId::from(0), "x", false)),
                (LocalId::from(2), Instruction::load(LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        ));

        let type_errors = validate_types(&cfg, &[]);
        assert!(type_errors.is_empty(), "Load from GetField should be valid: {:?}", type_errors);
    }

    #[test]
    fn test_load_from_number_constant_is_type_error() {
        // Load from NumberConstant is a type error
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(42)),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        ));

        let type_errors = validate_types(&cfg, &[]);
        assert!(!type_errors.is_empty(), "Expected type error for Load(NumberConstant)");
        assert!(type_errors.iter().any(|e| matches!(e, ValidationError::LoadFromNonPointer { .. })));
    }

    #[test]
    fn test_store_to_number_constant_is_type_error() {
        // Store to NumberConstant is a type error - you can only store to pointers
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(42)),
                (LocalId::from(1), Instruction::num_const(99)),
                // This is the type error being tested: Store to a NumberConstant.
                // NumberConstant produces a VALUE, not a pointer, so this should be flagged.
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(None)),
        ));

        // Structural validation should pass
        let structural_result = validate_cfg(&cfg);
        assert!(structural_result.is_valid(), "Structural validation should pass: {:?}", structural_result.errors);

        // Type validation should catch Store(NumberConstant)
        let type_errors = validate_types(&cfg, &[]);
        assert!(!type_errors.is_empty(), "Expected type error for Store(NumberConstant)");
        assert!(type_errors.iter().any(|e| matches!(e, ValidationError::StoreToNonPointer { target_type: SsaType::Value, .. })),
            "Expected StoreToNonPointer error: {:?}", type_errors);
    }

    #[test]
    fn test_store_to_alloc_is_valid() {
        // Store to Alloc is valid - Alloc produces a pointer
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::Alloc),
                (LocalId::from(1), Instruction::num_const(42)),
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(None)),
        ));

        let type_errors = validate_types(&cfg, &[]);
        assert!(type_errors.is_empty(), "Store to Alloc should be valid: {:?}", type_errors);
    }

    #[test]
    fn test_phi_references_non_predecessor() {
        // A phi that references a block that exists but is not a predecessor
        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(42))],
            // Entry jumps to join, not to unreachable
            (LocalId::from(1), Terminator::branch("join")),
        );
        let unreachable = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(99))],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        );
        let join = Block::new_for_test(
            vec![
                // Phi references "unreachable" which exists but is NOT a predecessor of "join"
                (
                    LocalId::from(4),
                    Instruction::phi(vec![
                        (Label::from("__entry".to_string()), LocalId::from(0)),
                        (Label::from("unreachable".to_string()), LocalId::from(2)),
                    ]),
                ),
            ],
            (LocalId::from(5), Terminator::ret(Some(LocalId::from(4)))),
        );

        let cfg = Cfg::with_blocks(entry, [("unreachable", unreachable), ("join", join)]);

        let result = validate_cfg(&cfg);
        assert!(!result.is_valid(), "Expected validation errors");
        assert!(result.errors.iter().any(|e| matches!(e, ValidationError::PhiReferencesNonPredecessor { referenced_block, .. }
            if referenced_block.as_str() == "unreachable")),
            "Expected PhiReferencesNonPredecessor error: {:?}", result.errors);
    }
}
