//! IR -> Rust transpiler for the native-compile probe (task #124,
//! plans/native-probe.md).
//!
//! Emits `native-probe/src/gen.rs`: one Rust `fn` per `FunDef` of the plain
//! executable program (the exact program `concrete_run` interprets), lowered
//! for a single concrete lane. Blocks become a `loop { match b }` state
//! machine, phis are destructed into parallel copies on the edges, calls
//! dispatch through a generated `match` on dense function ids. All value
//! semantics live in `native-probe/src/runtime.rs` (hand-written, with
//! file:line pointers to the interpreter code each op mirrors).
//!
//! The generated crate is the measurement instrument; hex-exactness against
//! `concrete_run` is the gate that makes its numbers mean anything.

use std::collections::{BTreeSet, HashMap};
use std::fmt::Write as _;

use anyhow::{anyhow, bail, Context, Result};
use celeste_rust::ir::{BinaryOp, Block, FunDef, Instruction, LocalId, Terminator, UnaryOp};
use celeste_rust::rewrite::print::blocks_in_order;
use celeste_rust::rewrite::program::Program;

/// Builtin id ABI. MUST match `native-probe/src/runtime.rs BUILTIN_NAMES`.
const BUILTIN_NAMES: [&str; 18] = [
    "__print",
    "__new_unknown_boolean",
    "__widen_rem",
    "__new_vector",
    "__array_table_drop_last",
    "error",
    "min",
    "max",
    "abs",
    "flr",
    "__split_by_flr",
    "__split_at",
    "add",
    "print",
    "sin",
    "mget",
    "fget",
    "tile_flag_at",
];

#[derive(Default)]
struct Interner {
    names: Vec<String>,
    ids: HashMap<String, u32>,
}

impl Interner {
    fn intern(&mut self, name: &str) -> u32 {
        if let Some(&id) = self.ids.get(name) {
            return id;
        }
        let id = self.names.len() as u32;
        self.names.push(name.to_string());
        self.ids.insert(name.to_string(), id);
        id
    }
}

fn l(id: LocalId) -> String {
    format!("l{}", usize::from(id))
}

struct Gen {
    /// Name of the function currently being emitted (assert diagnostics).
    current_fn: String,
    /// (kind, fn name, interned field id or 0) per get_field/get_index
    /// site, in site-id order - the gap census's site table.
    site_info: Vec<(&'static str, String, u32, usize)>,
    /// (fn name, block label) per conditional-branch site (SIMD
    /// divergence census).
    branch_info: Vec<String>,
    strings: Interner,
    globals: Interner,
    fields: Interner,
    /// GlobalId (function name) -> dense fn id, insertion order of
    /// `program.functions`.
    fn_ids: HashMap<String, u32>,
    fn_names: Vec<String>,
}

impl Gen {
    fn builtin_id(name: &str) -> Result<u32> {
        BUILTIN_NAMES
            .iter()
            .position(|n| *n == name)
            .map(|i| i as u32)
            .ok_or_else(|| anyhow!("no builtin id for {:?}", name))
    }

    /// The locals a function mentions anywhere (defs, uses, args, captures).
    fn collect_locals(fun: &FunDef) -> BTreeSet<usize> {
        let mut out: BTreeSet<usize> = BTreeSet::new();
        for id in fun.capture_ids.iter() {
            out.insert(usize::from(*id));
        }
        for id in fun.arg_ids.iter().flatten() {
            out.insert(usize::from(*id));
        }
        for (_, block) in blocks_in_order(&fun.cfg) {
            for (id, instr) in &block.instructions {
                out.insert(usize::from(*id));
                for used in instr.get_used_locals() {
                    out.insert(usize::from(used));
                }
                if let Instruction::Phi { branches } = instr {
                    for (_, src) in branches {
                        out.insert(usize::from(*src));
                    }
                }
            }
            for used in block.terminator.1.get_used_locals() {
                out.insert(usize::from(used));
            }
        }
        out
    }

    /// Parallel phi copies for the edge `pred -> target`. `pred_label` is
    /// None for the entry block, whose label never appears in `named`; the
    /// matching phi branch is found by elimination against the labels of
    /// the target's OTHER predecessors.
    fn phi_copies(
        target: &Block,
        pred_label: Option<&str>,
        named_pred_labels: &BTreeSet<String>,
        fn_name: &str,
    ) -> Result<String> {
        let mut copies: Vec<(LocalId, LocalId)> = Vec::new();
        for (dst, instr) in &target.instructions {
            let Instruction::Phi { branches } = instr else {
                break; // phis lead the block (ir.rs split_block_phi_instructions)
            };
            let src = match pred_label {
                Some(label) => {
                    let matches: Vec<_> = branches
                        .iter()
                        .filter(|(b, _)| b.as_str() == label)
                        .collect();
                    match matches.as_slice() {
                        [(_, src)] => *src,
                        _ => bail!(
                            "{}: phi %{} has {} branches for pred {:?}",
                            fn_name,
                            usize::from(*dst),
                            matches.len(),
                            label
                        ),
                    }
                }
                None => {
                    // Entry edge: the one branch whose label is not any
                    // named predecessor's label.
                    let candidates: Vec<_> = branches
                        .iter()
                        .filter(|(b, _)| !named_pred_labels.contains(b.as_str()))
                        .collect();
                    match candidates.as_slice() {
                        [(_, src)] => *src,
                        _ => bail!(
                            "{}: phi %{}: cannot resolve the entry edge \
                             ({} candidate branches, named preds {:?})",
                            fn_name,
                            usize::from(*dst),
                            candidates.len(),
                            named_pred_labels
                        ),
                    }
                }
            };
            copies.push((*dst, src));
        }
        if copies.is_empty() {
            return Ok(String::new());
        }
        // Parallel semantics: all sources read before any destination is
        // written (a phi's source may be another phi's destination).
        let mut out = String::new();
        for (i, (_, src)) in copies.iter().enumerate() {
            write!(out, "let __t{} = {}; ", i, l(*src)).unwrap();
        }
        for (i, (dst, _)) in copies.iter().enumerate() {
            write!(out, "{} = __t{}; ", l(*dst), i).unwrap();
        }
        Ok(out)
    }

    fn emit_instruction(&mut self, id: LocalId, instr: &Instruction, out: &mut String) -> Result<()> {
        let d = l(id);
        match instr {
            Instruction::Phi { .. } => unreachable!("phis are destructed on edges"),
            Instruction::Alloc => writeln!(out, "{} = rt.alloc_nil();", d)?,
            Instruction::GetGlobal { name, create_if_missing } => {
                let g = self.globals.intern(name);
                writeln!(out, "{} = rt.get_global({}, {}); // {}", d, g, create_if_missing, name)?;
            }
            Instruction::Load { source } => writeln!(out, "{} = rt.load({});", d, l(*source))?,
            Instruction::Store { target, source } => {
                writeln!(out, "rt.store({}, {});", l(*target), l(*source))?;
            }
            Instruction::StoreEmptyTable { target } => {
                writeln!(out, "rt.store_empty_table({});", l(*target))?;
            }
            Instruction::StoreClosure { target, fun_def, captures } => {
                let f = *self
                    .fn_ids
                    .get(fun_def.as_str())
                    .ok_or_else(|| anyhow!("StoreClosure of unknown fn {:?}", fun_def))?;
                let caps: Vec<String> = captures.iter().map(|c| l(*c)).collect();
                writeln!(
                    out,
                    "rt.store_closure({}, {}, &[{}]); // {}",
                    l(*target),
                    f,
                    caps.join(", "),
                    fun_def.as_str()
                )?;
            }
            Instruction::GetField { receiver, field, create_if_missing } => {
                let f = self.fields.intern(field);
                let site = self.site_info.len() as u32;
                self.site_info.push(("field", self.current_fn.clone(), f, usize::from(id)));
                writeln!(
                    out,
                    "{} = rt.get_field({}, {}, {}, {}); // .{}",
                    d,
                    l(*receiver),
                    f,
                    create_if_missing,
                    site,
                    field
                )?;
            }
            Instruction::GetIndex { receiver, index, create_if_missing } => {
                let site = self.site_info.len() as u32;
                self.site_info.push(("index", self.current_fn.clone(), 0, usize::from(id)));
                writeln!(
                    out,
                    "{} = rt.get_index({}, {}, {}, {});",
                    d,
                    l(*receiver),
                    l(*index),
                    create_if_missing,
                    site
                )?;
            }
            Instruction::NumberConstant { value } => {
                let bits = value.to_bits();
                writeln!(
                    out,
                    "{} = V::Num(P8::from_parts({}, {})); // {:?}",
                    d,
                    (bits >> 16) as i16,
                    bits as u16,
                    value
                )?;
            }
            Instruction::BoolConstant { value } => writeln!(out, "{} = V::Bool({});", d, value)?,
            Instruction::StringConstant { value } => {
                let s = self.strings.intern(value);
                writeln!(out, "{} = V::Str({}); // {:?}", d, s, value)?;
            }
            Instruction::NilConstant => writeln!(out, "{} = V::Nil;", d)?,
            Instruction::Call { closure, args } => {
                let args: Vec<String> = args.iter().map(|a| l(*a)).collect();
                writeln!(
                    out,
                    "{} = call_value(rt, {}, &[{}], {:?});",
                    d,
                    l(*closure),
                    args.join(", "),
                    format!("{} %{}", self.current_fn, usize::from(id))
                )?;
            }
            Instruction::CallBuiltin { callee, name, args } => {
                let bi = Self::builtin_id(name)?;
                write!(out, "rt.assert_builtin({}, {}); ", l(*callee), bi)?;
                let a: Vec<String> = args.iter().map(|x| l(*x)).collect();
                let call = match (name.as_str(), a.as_slice()) {
                    ("min", [x, y]) => format!("rt.bi_min({}, {})", x, y),
                    ("max", [x, y]) => format!("rt.bi_max({}, {})", x, y),
                    ("abs", [x]) => format!("rt.bi_abs({})", x),
                    ("flr", [x]) => format!("rt.bi_flr({})", x),
                    ("sin", [x]) => format!("rt.bi_sin({})", x),
                    ("mget", [x, y]) => format!("rt.bi_mget({}, {})", x, y),
                    ("tile_flag_at", [x, y, w, h, f]) => {
                        format!("rt.bi_tile_flag_at({}, {}, {}, {}, {})", x, y, w, h, f)
                    }
                    _ => bail!("CallBuiltin {:?} with {} args has no direct lowering", name, a.len()),
                };
                writeln!(out, "{} = {};", d, call)?;
            }
            Instruction::UnaryOp { op, arg } => {
                let m = match op {
                    UnaryOp::Minus => "un_minus",
                    UnaryOp::Not => "un_not",
                    UnaryOp::Hash => "un_hash",
                };
                writeln!(out, "{} = rt.{}({});", d, m, l(*arg))?;
            }
            Instruction::BinaryOp { left, op, right } => {
                let m = match op {
                    BinaryOp::Plus => "op_add",
                    BinaryOp::Minus => "op_sub",
                    BinaryOp::Star => "op_mul",
                    BinaryOp::Slash => "op_div",
                    BinaryOp::Percent => "op_rem",
                    BinaryOp::Caret => "op_pow",
                    BinaryOp::TwoEqual => "eq",
                    BinaryOp::TildeEqual => "ne",
                    BinaryOp::LessThan => "lt",
                    BinaryOp::LessThanEqual => "le",
                    BinaryOp::GreaterThan => "gt",
                    BinaryOp::GreaterThanEqual => "ge",
                    BinaryOp::TwoDots => "concat",
                };
                writeln!(out, "{} = rt.{}({}, {});", d, m, l(*left), l(*right))?;
            }
            Instruction::Select { condition, if_true, if_false } => {
                writeln!(
                    out,
                    "{} = if rt.sel_bool({}) {{ {} }} else {{ {} }};",
                    d,
                    l(*condition),
                    l(*if_true),
                    l(*if_false)
                )?;
            }
            Instruction::Kill { .. } => {} // deadness annotation; nothing to run
            Instruction::AssertClosure { value, fun_def, captures } => {
                let f = *self
                    .fn_ids
                    .get(fun_def.as_str())
                    .ok_or_else(|| anyhow!("AssertClosure of unknown fn {:?}", fun_def))?;
                let caps: Vec<String> = captures.iter().map(|c| l(*c)).collect();
                writeln!(out, "rt.assert_closure({}, {}, &[{}], {:?});", l(*value), f, caps.join(", "), format!("{} %{}", self.current_fn, usize::from(id)))?;
            }
            Instruction::AssertPointer { value } => {
                writeln!(out, "rt.assert_pointer({}, {:?});", l(*value), format!("{} %{}", self.current_fn, usize::from(id)))?;
            }
            Instruction::AssertValueCell { target } => {
                writeln!(out, "rt.assert_value_cell({}, {:?});", l(*target), format!("{} %{}", self.current_fn, usize::from(id)))?;
            }
            Instruction::AssertTrue { value } => writeln!(out, "rt.assert_true({}, {:?});", l(*value), format!("{} %{}", self.current_fn, usize::from(id)))?,
            Instruction::Expand { value } => writeln!(out, "{} = rt.expand({});", d, l(*value))?,
        }
        Ok(())
    }

    fn emit_function(&mut self, fn_id: u32, fun: &FunDef) -> Result<String> {
        let fn_name = fun.name.as_str().to_string();
        self.current_fn = fn_name.clone();
        let blocks = blocks_in_order(&fun.cfg);
        assert!(
            !fun.cfg.named.keys().any(|k| k.as_str() == "__entry"),
            "{}: a named block collides with the entry pseudo-label",
            fn_name
        );
        // Label -> block index ("__entry" is index 0 by construction).
        let index_of: HashMap<&str, usize> = blocks
            .iter()
            .enumerate()
            .map(|(i, (label, _))| (label.as_str(), i))
            .collect();
        // Predecessor labels per target index (named preds only; the entry
        // block has no label and is handled by elimination in phi_copies).
        let mut named_pred_labels: Vec<BTreeSet<String>> = vec![BTreeSet::new(); blocks.len()];
        for (i, (label, block)) in blocks.iter().enumerate() {
            let mut note = |target: &str| {
                let t = index_of[target];
                if i != 0 {
                    named_pred_labels[t].insert(label.clone());
                }
            };
            match &block.terminator.1 {
                Terminator::Return { .. } => {}
                Terminator::UnconditionalBranch { target } => note(target.as_str()),
                Terminator::ConditionalBranch { true_target, false_target, .. } => {
                    note(true_target.as_str());
                    note(false_target.as_str());
                }
            }
        }

        let mut out = String::new();
        writeln!(out, "/// `{}`", fn_name)?;
        writeln!(out, "pub fn f_{}(rt: &mut Rt, caps: &[V], args: &[V]) -> V {{", fn_id)?;
        for id in Self::collect_locals(fun) {
            writeln!(out, "    let mut l{}: V = V::Nil;", id)?;
        }
        for (i, cap) in fun.capture_ids.iter().enumerate() {
            writeln!(out, "    {} = caps[{}];", l(*cap), i)?;
        }
        for (i, arg) in fun.arg_ids.iter().enumerate() {
            if let Some(arg) = arg {
                // Missing arguments pad with Nil (core_interpreter.rs:762).
                writeln!(out, "    {} = args.get({}).copied().unwrap_or(V::Nil);", l(*arg), i)?;
            }
        }
        writeln!(out, "    let mut b: u32 = 0;")?;
        writeln!(out, "    loop {{ match b {{")?;
        for (i, (label, block)) in blocks.iter().enumerate() {
            writeln!(out, "    {} => {{ // {}", i, label)?;
            let mut body = String::new();
            for (id, instr) in &block.instructions {
                if matches!(instr, Instruction::Phi { .. }) {
                    continue;
                }
                self.emit_instruction(*id, instr, &mut body)?;
            }
            let pred_label = if i == 0 { None } else { Some(label.as_str()) };
            let edge = |target: &str| -> Result<String> {
                let t = index_of[target];
                let copies = Self::phi_copies(
                    blocks[t].1,
                    pred_label,
                    &named_pred_labels[t],
                    &fn_name,
                )?;
                Ok(format!("{}b = {};", copies, t))
            };
            match &block.terminator.1 {
                Terminator::Return { value } => match value {
                    Some(v) => writeln!(body, "return {};", l(*v))?,
                    None => writeln!(body, "return V::Nil;")?,
                },
                Terminator::UnconditionalBranch { target } => {
                    writeln!(body, "{} continue;", edge(target.as_str())?)?;
                }
                Terminator::ConditionalBranch { condition, true_target, false_target } => {
                    let bsite = self.branch_info.len() as u32;
                    self.branch_info.push(format!("{} @{}", fn_name, label));
                    writeln!(
                        body,
                        "if rt.truthy_b({}, {}) {{ {} }} else {{ {} }} continue;",
                        l(*condition),
                        bsite,
                        edge(true_target.as_str())?,
                        edge(false_target.as_str())?
                    )?;
                }
            }
            for line in body.lines() {
                writeln!(out, "        {}", line)?;
            }
            writeln!(out, "    }}")?;
        }
        writeln!(out, "    _ => unreachable!(),")?;
        writeln!(out, "    }} }}")?;
        writeln!(out, "}}")?;
        Ok(out)
    }
}

fn str_array(name: &str, items: &[String]) -> String {
    let mut out = format!("pub static {}: &[&str] = &[\n", name);
    for item in items {
        out.push_str(&format!("    {:?},\n", item));
    }
    out.push_str("];\n");
    out
}

fn main() -> Result<()> {
    let mut rewritten = false;
    let mut recipe_path = "rewrites.jsonl".to_string();
    let mut out_path = "native-probe/src/gen.rs".to_string();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--rewritten" => rewritten = true,
            "--recipe" => {
                recipe_path = args
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("--recipe needs a path"))?;
                rewritten = true;
            }
            other => out_path = other.to_string(),
        }
    }

    let program = if rewritten {
        // The program the abstract search actually executes: plain compile +
        // the full recipe. Its concrete semantics must equal the plain
        // program's (each entry is differentially verified), so the same
        // concrete_run oracle applies - transpiling it exercises the
        // recipe-planted instructions (Select/Expand/Kill/guards) natively.
        // `--recipe` selects a different recipe file - the compile-only
        // overlay recipes (rewrites-compile.jsonl) live here, never in the
        // runner.
        let recipe = celeste_rust::rewrite::recipe::Recipe::load(&recipe_path)?;
        let (program, _) = celeste_rust::rewrite::recipe::build(&recipe)
            .with_context(|| format!("apply {} (run from the repo root)", recipe_path))?;
        program
    } else {
        Program::compile_executable_from_disk()
            .context("compile the plain executable program (run from the repo root)")?
    };

    let mut gen = Gen {
        current_fn: String::new(),
        site_info: Vec::new(),
        branch_info: Vec::new(),
        strings: Interner::default(),
        globals: Interner::default(),
        fields: Interner::default(),
        fn_ids: HashMap::new(),
        fn_names: Vec::new(),
    };
    for (i, (name, _)) in program.functions.iter().enumerate() {
        gen.fn_ids.insert(name.as_str().to_string(), i as u32);
        gen.fn_names.push(name.as_str().to_string());
    }
    // The driver pokes these by name even if the program text never does.
    for name in BUILTIN_NAMES {
        gen.globals.intern(name);
    }
    for name in ["__button_states", "objects", "freeze", "player", "player_spawn"] {
        gen.globals.intern(name);
    }
    for name in ["type", "x", "y", "spd", "rem"] {
        gen.fields.intern(name);
    }

    let mut fns = String::new();
    let mut dispatch = String::new();
    for (i, (_, fun)) in program.functions.iter().enumerate() {
        let code = gen
            .emit_function(i as u32, fun)
            .with_context(|| format!("emit {}", fun.name.as_str()))?;
        fns.push_str(&code);
        fns.push('\n');
        writeln!(dispatch, "        {} => f_{}(rt, caps, args),", i, i)?;
    }

    let fn_init = gen.fn_ids["__init"];
    let fn_frame = gen.fn_ids["__frame"];

    let mut out = String::new();
    out.push_str("// GENERATED by `cargo run --release --bin transpile` in the parent repo.\n");
    out.push_str("// Source program: Program::compile_executable_from_disk() (the exact\n");
    out.push_str("// program concrete_run interprets). Do not edit.\n\n");
    out.push_str("use crate::runtime::*;\n\n");
    out.push_str(&str_array("STRINGS", &gen.strings.names));
    out.push_str(&str_array("GLOBAL_NAMES", &gen.globals.names));
    out.push_str(&str_array("FIELD_NAMES", &gen.fields.names));
    out.push_str(&str_array("FN_NAMES", &gen.fn_names));
    out.push_str("/// (kind, fn, interned field id, instruction id) per site.\n");
    out.push_str("pub static SITE_INFO: &[(&str, &str, u32, u32)] = &[\n");
    for (kind, fn_name, f, iid) in &gen.site_info {
        out.push_str(&format!("    ({:?}, {:?}, {}, {}),\n", kind, fn_name, f, iid));
    }
    out.push_str("];\n");
    out.push_str("/// fn name per conditional-branch site.\n");
    out.push_str("pub static BRANCH_INFO: &[&str] = &[\n");
    for f in &gen.branch_info {
        out.push_str(&format!("    {:?},\n", f));
    }
    out.push_str("];\n");
    out.push_str(&format!("pub const FN_INIT: u32 = {};\n", fn_init));
    out.push_str(&format!("pub const FN_FRAME: u32 = {};\n\n", fn_frame));
    out.push_str(
        "pub fn global_id(name: &str) -> Option<u32> {\n    \
         GLOBAL_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n\
         pub fn field_id(name: &str) -> Option<u32> {\n    \
         FIELD_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n",
    );
    out.push_str(
        "/// Call through a value: closure cells dispatch on their dense fn id,\n\
         /// builtin cells go to the runtime (core_interpreter.rs:697).\n\
         pub fn call_value(rt: &mut Rt, c: V, args: &[V], ctx: &str) -> V {\n\
         \x20   let V::Ptr(p) = c else { panic!(\"call on a non-pointer at {}: {:?}\", ctx, c) };\n\
         \x20   match &rt.heap[p as usize] {\n\
         \x20       Cell::Clo(f, caps) => {\n\
         \x20           let f = *f;\n\
         \x20           let caps: Box<[V]> = caps.clone();\n\
         \x20           call_fn(rt, f, &caps, args)\n\
         \x20       }\n\
         \x20       Cell::Bi(b) => {\n\
         \x20           let b = *b;\n\
         \x20           rt.call_builtin(b, args)\n\
         \x20       }\n\
         \x20       other => panic!(\"call on a non-callable cell: {:?}\", other),\n\
         \x20   }\n\
         }\n\n",
    );
    out.push_str("pub fn call_fn(rt: &mut Rt, f: u32, caps: &[V], args: &[V]) -> V {\n");
    out.push_str("    match f {\n");
    out.push_str(&dispatch);
    out.push_str("        _ => panic!(\"unknown fn id {}\", f),\n    }\n}\n\n");
    out.push_str(&fns);

    std::fs::write(&out_path, out).with_context(|| format!("write {}", out_path))?;
    eprintln!(
        "wrote {} ({} functions, {} globals, {} fields, {} strings)",
        out_path,
        gen.fn_names.len(),
        gen.globals.names.len(),
        gen.fields.names.len(),
        gen.strings.names.len()
    );
    Ok(())
}
