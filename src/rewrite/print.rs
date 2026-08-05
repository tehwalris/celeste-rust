//! Textual IR, for reading and for diffing one rewrite against the next.
//!
//! Everything else in the rewrite system depends on being able to see what a
//! transformation did, so this comes first. The format is deliberately boring
//! and line-oriented so `diff` is useful on it.

use std::fmt::Write;

use crate::ir::{BinaryOp, Block, Cfg, FunDef, Instruction, Label, LocalId, Terminator, UnaryOp};

use super::program::Program;

/// How an instruction is addressed by a rewrite instruction.
///
/// Frontend-produced instructions are named after the `LocalId` they define
/// (`%17`). That is stable as long as nothing ever renumbers existing ids -
/// rewrites only ever mint fresh ones - so no side table is needed for them.
/// Instructions created *by* a rewrite are named `<rewrite id>.<n>` and do need
/// one; see `names.rs`.
pub fn local_name(id: LocalId) -> String {
    format!("%{}", usize::from(id))
}

pub fn parse_local_name(name: &str) -> Option<LocalId> {
    name.strip_prefix('%')?.parse::<usize>().ok().map(LocalId::from)
}

fn unary_op_str(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Minus => "-",
        UnaryOp::Not => "not",
        UnaryOp::Hash => "#",
    }
}

fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Caret => "^",
        BinaryOp::GreaterThan => ">",
        BinaryOp::GreaterThanEqual => ">=",
        BinaryOp::LessThan => "<",
        BinaryOp::LessThanEqual => "<=",
        BinaryOp::Minus => "-",
        BinaryOp::Percent => "%",
        BinaryOp::Plus => "+",
        BinaryOp::Slash => "/",
        BinaryOp::Star => "*",
        BinaryOp::TildeEqual => "~=",
        BinaryOp::TwoEqual => "==",
        BinaryOp::TwoDots => "..",
    }
}

pub fn format_instruction(instr: &Instruction) -> String {
    let n = local_name;
    match instr {
        Instruction::Alloc => "alloc".to_string(),
        Instruction::Kill { values } => format!(
            "kill {}",
            values.iter().map(|v| n(*v)).collect::<Vec<_>>().join(", ")
        ),
        Instruction::GetGlobal { name, create_if_missing } => format!(
            "get_global {:?}{}",
            name,
            if *create_if_missing { " create" } else { "" }
        ),
        Instruction::Load { source } => format!("load {}", n(*source)),
        Instruction::Store { target, source } => format!("store {} <- {}", n(*target), n(*source)),
        Instruction::StoreEmptyTable { target } => format!("store_empty_table {}", n(*target)),
        Instruction::StoreClosure { target, fun_def, captures } => format!(
            "store_closure {} <- {} [{}]",
            n(*target),
            fun_def.as_str(),
            captures.iter().map(|c| n(*c)).collect::<Vec<_>>().join(", ")
        ),
        Instruction::GetField { receiver, field, create_if_missing } => format!(
            "get_field {}.{}{}",
            n(*receiver),
            field,
            if *create_if_missing { " create" } else { "" }
        ),
        Instruction::GetIndex { receiver, index, create_if_missing } => format!(
            "get_index {}[{}]{}",
            n(*receiver),
            n(*index),
            if *create_if_missing { " create" } else { "" }
        ),
        Instruction::NumberConstant { value } => format!("num {:?}", value),
        Instruction::BoolConstant { value } => format!("bool {}", value),
        Instruction::StringConstant { value } => format!("str {:?}", value),
        Instruction::NilConstant => "nil".to_string(),
        Instruction::Call { closure, args } => format!(
            "call {}({})",
            n(*closure),
            args.iter().map(|a| n(*a)).collect::<Vec<_>>().join(", ")
        ),
        Instruction::UnaryOp { op, arg } => format!("{} {}", unary_op_str(*op), n(*arg)),
        Instruction::BinaryOp { left, op, right } => {
            format!("{} {} {}", n(*left), binary_op_str(*op), n(*right))
        }
        Instruction::Select { condition, if_true, if_false } => format!(
            "select {} ? {} : {}",
            n(*condition),
            n(*if_true),
            n(*if_false)
        ),
        Instruction::Expand { value } => format!("expand {}", n(*value)),
        Instruction::AssertPointer { value } => {
            format!("assert_pointer {}", n(*value))
        }
        Instruction::AssertValueCell { target } => {
            format!("assert_value_cell {}", n(*target))
        }
        Instruction::AssertTrue { value } => {
            format!("assert_true {}", n(*value))
        }
        Instruction::CallBuiltin { callee, name, args } => format!(
            "call_builtin {:?} via {}({})",
            name,
            n(*callee),
            args.iter().map(|a| n(*a)).collect::<Vec<_>>().join(", ")
        ),
        Instruction::AssertClosure { value, fun_def, captures } => format!(
            "assert_closure {} is {}{}",
            n(*value),
            fun_def.as_str(),
            if captures.is_empty() {
                String::new()
            } else {
                format!(
                    " with captures [{}]",
                    captures.iter().map(|c| n(*c)).collect::<Vec<_>>().join(", ")
                )
            }
        ),
        Instruction::Phi { branches } => format!(
            "phi [{}]",
            branches
                .iter()
                .map(|(label, id)| format!("{}: {}", label.as_str(), n(*id)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

pub fn format_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return { value: Some(v) } => format!("return {}", local_name(*v)),
        Terminator::Return { value: None } => "return".to_string(),
        Terminator::UnconditionalBranch { target } => format!("br {}", target.as_str()),
        Terminator::ConditionalBranch { condition, true_target, false_target } => format!(
            "br {} ? {} : {}",
            local_name(*condition),
            true_target.as_str(),
            false_target.as_str()
        ),
        Terminator::ConditionalSkip { condition, skip_target, enter_target } => format!(
            "skip_if_none {} ? {} : {}",
            local_name(*condition),
            skip_target.as_str(),
            enter_target.as_str()
        ),
    }
}

/// One block's instructions and terminator, without its label. Used by rule
/// verifiers to compare blocks structurally.
pub fn format_block(block: &Block) -> String {
    let mut out = String::new();
    for (id, instr) in &block.instructions {
        let _ = writeln!(out, "{} = {}", local_name(*id), format_instruction(instr));
    }
    let _ = writeln!(
        out,
        "{} = {}",
        local_name(block.terminator_id()),
        format_terminator(block.terminator_kind())
    );
    out
}

fn write_block(out: &mut String, label: &str, block: &Block) {
    let _ = writeln!(
        out,
        "  {}:{}",
        label,
        if block.hint_normalize { "  [hint_normalize]" } else { "" }
    );
    for (id, instr) in &block.instructions {
        let _ = writeln!(out, "    {} = {}", local_name(*id), format_instruction(instr));
    }
    let _ = writeln!(
        out,
        "    {} = {}",
        local_name(block.terminator_id()),
        format_terminator(block.terminator_kind())
    );
}

/// Blocks in a deterministic order: entry first, then named blocks sorted by
/// label. `Cfg::named` is an `FxHashMap`, so iteration order is not stable and
/// printing it directly would produce spurious diffs.
pub fn blocks_in_order(cfg: &Cfg) -> Vec<(String, &Block)> {
    let mut out = vec![("__entry".to_string(), &cfg.entry)];
    let mut named: Vec<(&Label, &Block)> = cfg.named.iter().collect();
    named.sort_by_key(|(label, _)| label.as_str().to_string());
    out.extend(named.into_iter().map(|(l, b)| (l.as_str().to_string(), b)));
    out
}

pub fn format_function(fun: &FunDef) -> String {
    let mut out = String::new();
    let args: Vec<String> = fun
        .arg_ids
        .iter()
        .map(|a| match a {
            Some(id) => local_name(*id),
            None => "_".to_string(),
        })
        .collect();
    let captures: Vec<String> = fun.capture_ids.iter().map(|c| local_name(*c)).collect();
    let _ = writeln!(
        out,
        "fn {}({}){}",
        fun.name.as_str(),
        args.join(", "),
        if captures.is_empty() {
            String::new()
        } else {
            format!(" captures [{}]", captures.join(", "))
        }
    );
    for (label, block) in blocks_in_order(&fun.cfg) {
        write_block(&mut out, &label, block);
    }
    out
}

pub fn format_program(program: &Program) -> String {
    let mut out = String::new();
    for fun in program.functions.values() {
        out.push_str(&format_function(fun));
        out.push('\n');
    }
    out
}
