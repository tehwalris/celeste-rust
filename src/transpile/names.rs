//! The name-table generator.
//!
//! Writes `crates/celeste-names/src/gen.rs`, which is now ~230 lines of TABLES:
//! `STRINGS`, `GLOBAL_NAMES`, `FIELD_NAMES`, `FN_NAMES` and the two
//! lookups over them. Their numbering is the side effect of a walk over
//! every function's blocks in emission order (see `walk_instruction`) -
//! `FIELD_NAMES`' order in particular is the canonical field ordering the
//! boundary hashes, so the gate on any change here is that the tables come
//! out byte-identical.
//!
//! It used to emit a whole PROGRAM as well: one Rust `fn` per `FunDef`,
//! blocks as a `loop { match b }` state machine, phis destructed on the
//! edges, calls dispatched through a generated `match` on dense fn ids -
//! 29,672 lines running over the `Engine` trait. That engine is retired
//! (plans/k4-retirement-plan.md): the class kernels are the compiled path
//! and the celeste-rust interpreter is the fallback, so a third executor
//! was only a third thing to keep in agreement. `kernel_recon` and the
//! `kernel` module - the actual emitters for the class kernels - are what
//! survived the deletion, along with this walk.

use std::collections::HashMap;

use anyhow::{anyhow, bail, Context, Result};

use crate::builtins::BUILTIN_NAMES;
use crate::ir::{FunDef, Instruction, Terminator};
use crate::rewrite::print::blocks_in_order;
use crate::rewrite::program::Program;


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

struct Gen {
    /// Name of the function being walked (assert diagnostics).
    current_fn: String,
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

    /// Intern the names one instruction mentions.
    ///
    /// This used to emit that instruction as Rust. It does not any more -
    /// nothing consumes a generated program body - but the WALK has to
    /// stay, because the name tables are its side effect: `STRINGS`,
    /// `GLOBAL_NAMES` and `FIELD_NAMES` are numbered in the order this
    /// traversal first meets each name, and `FIELD_NAMES`' order is the
    /// canonical field ordering the boundary hashes. Reorder the walk and
    /// you get a different shape hash, a different row key, a different
    /// search - silently.
    ///
    /// So the shape of the match is deliberate: an arm per instruction
    /// kind, in the same order, interning exactly what its emitted form
    /// used to. An arm that interns nothing is still an arm, so that a
    /// new instruction kind cannot slip through as "nothing to do".
    fn walk_instruction(&mut self, instr: &Instruction) -> Result<()> {
        match instr {
            Instruction::Phi { .. } => unreachable!("phis are destructed on edges"),
            Instruction::GetGlobal { name, .. } => {
                self.globals.intern(name);
            }
            Instruction::GetField { field, .. } => {
                self.fields.intern(field);
            }
            Instruction::StringConstant { value } => {
                self.strings.intern(value);
            }
            // Not interning, but still checked: a closure target that is
            // not a known function, or a builtin outside the ABI, is a
            // program this toolchain cannot represent, and it should say
            // so here rather than produce tables that look fine.
            Instruction::StoreClosure { fun_def, .. } | Instruction::AssertClosure { fun_def, .. } => {
                self.fn_ids
                    .get(fun_def.as_str())
                    .ok_or_else(|| anyhow!("closure of unknown fn {:?}", fun_def))?;
            }
            Instruction::CallBuiltin { name, args, .. } => {
                Self::builtin_id(name)?;
                let arity_ok = matches!(
                    (name.as_str(), args.len()),
                    ("min", 2) | ("max", 2) | ("abs", 1) | ("flr", 1) | ("sin", 1)
                        | ("mget", 2) | ("tile_flag_at", 5)
                );
                if !arity_ok {
                    bail!("CallBuiltin {:?} with {} args has no direct lowering", name, args.len());
                }
            }
            Instruction::Alloc
            | Instruction::Load { .. }
            | Instruction::Store { .. }
            | Instruction::StoreEmptyTable { .. }
            | Instruction::GetIndex { .. }
            | Instruction::NumberConstant { .. }
            | Instruction::BoolConstant { .. }
            | Instruction::NilConstant
            | Instruction::Call { .. }
            | Instruction::UnaryOp { .. }
            | Instruction::BinaryOp { .. }
            | Instruction::Select { .. }
            | Instruction::Kill { .. }
            | Instruction::AssertPointer { .. }
            | Instruction::AssertValueCell { .. }
            | Instruction::AssertTrue { .. }
            | Instruction::Expand { .. } => {}
        }
        Ok(())
    }

    /// Walk one function's blocks in emission order, interning as it goes.
    fn walk_function(&mut self, fun: &FunDef) -> Result<()> {
        let fn_name = fun.name.as_str().to_string();
        self.current_fn = fn_name.clone();
        assert!(
            !fun.cfg.named.keys().any(|k| k.as_str() == "__entry"),
            "{}: a named block collides with the entry pseudo-label",
            fn_name
        );
        // blocks_in_order, not `cfg.named`'s hash order: this is the
        // traversal the tables are numbered by.
        for (_, block) in blocks_in_order(&fun.cfg) {
            for (_, instr) in &block.instructions {
                if matches!(instr, Instruction::Phi { .. }) {
                    continue;
                }
                self.walk_instruction(instr)?;
            }
        }
        Ok(())
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

/// Feasibility recon for the SIMD kernel emitter (plans/columnar-engine.md
/// stage 2): from `__frame`, how much of the program inlines statically?
/// - calls: resolvable via a dominating AssertClosure (approximated
///   per-function - the devirt entries assert right before the call)?
/// - control flow: is every reachable CFG a DAG (if-convertible)?
/// - size: multiplicity-weighted instruction/branch counts after full
///   inlining - the straight-line kernel's length.
pub fn kernel_recon(program: &crate::rewrite::program::Program) {
    use std::collections::VecDeque;
    struct Info {
        calls: Vec<String>,
        unresolved: usize,
        branches: usize,
        instrs: usize,
        cyclic: bool,
        expands: usize,
        heap_sites: usize,
    }
    let mut infos: HashMap<String, Info> = HashMap::new();
    for (name, fun) in &program.functions {
        let blocks = blocks_in_order(&fun.cfg);
        let index_of: HashMap<&str, usize> =
            blocks.iter().enumerate().map(|(i, (l, _))| (l.as_str(), i)).collect();
        // Cycle check: iterative DFS with an on-stack mark.
        let mut state = vec![0u8; blocks.len()]; // 0 unvisited, 1 on stack, 2 done
        let mut cyclic = false;
        let mut stack: Vec<(usize, usize)> = vec![(0, 0)];
        state[0] = 1;
        while let Some((b, si)) = stack.pop() {
            let succs = blocks[b].1.terminator.1.successor_labels();
            if si < succs.len() {
                stack.push((b, si + 1));
                let t = index_of[succs[si].as_str()];
                match state[t] {
                    0 => {
                        state[t] = 1;
                        stack.push((t, 0));
                    }
                    1 => cyclic = true,
                    _ => {}
                }
            } else {
                state[b] = 2;
            }
        }
        let mut info = Info {
            calls: Vec::new(),
            unresolved: 0,
            branches: 0,
            instrs: 0,
            cyclic,
            expands: 0,
            heap_sites: 0,
        };
        let mut asserted: HashMap<usize, String> = HashMap::new();
        for (_, block) in &blocks {
            for (_, instr) in &block.instructions {
                info.instrs += 1;
                match instr {
                    Instruction::AssertClosure { value, fun_def, .. } => {
                        asserted.insert(usize::from(*value), fun_def.as_str().to_string());
                    }
                    Instruction::Call { closure, .. } => {
                        match asserted.get(&usize::from(*closure)) {
                            Some(f) => info.calls.push(f.clone()),
                            None => info.unresolved += 1,
                        }
                    }
                    Instruction::Expand { .. } => info.expands += 1,
                    Instruction::GetField { .. } | Instruction::GetIndex { .. } => {
                        info.heap_sites += 1
                    }
                    _ => {}
                }
            }
            if matches!(block.terminator.1, Terminator::ConditionalBranch { .. }) {
                info.branches += 1;
            }
        }
        infos.insert(name.as_str().to_string(), info);
    }
    // Reachability + multiplicity-weighted inlined totals from __frame.
    let mut mult: HashMap<String, u64> = HashMap::new();
    let mut queue: VecDeque<(String, u64)> = VecDeque::new();
    queue.push_back(("__frame".to_string(), 1));
    let mut inline_depth_guard = 0u64;
    while let Some((f, m)) = queue.pop_front() {
        inline_depth_guard += 1;
        assert!(inline_depth_guard < 1_000_000, "runaway inlining (recursion?)");
        *mult.entry(f.clone()).or_insert(0) += m;
        let info = &infos[&f];
        for callee in info.calls.clone() {
            queue.push_back((callee, m));
        }
    }
    let (mut t_instr, mut t_branch, mut t_unres, mut t_expand, mut t_heap) =
        (0u64, 0u64, 0u64, 0u64, 0u64);
    let mut cyclic_reachable: Vec<&str> = Vec::new();
    println!("kernel recon (reachable from __frame, multiplicity-weighted):");
    let mut rows: Vec<(&String, &u64)> = mult.iter().collect();
    rows.sort_by_key(|(_, m)| std::cmp::Reverse(**m));
    for (f, m) in rows {
        let i = &infos[f.as_str()];
        t_instr += m * i.instrs as u64;
        t_branch += m * i.branches as u64;
        t_unres += m * i.unresolved as u64;
        t_expand += m * i.expands as u64;
        t_heap += m * i.heap_sites as u64;
        if i.cyclic {
            cyclic_reachable.push(f);
        }
        println!(
            "  x{:<4} {:30} {:5} instrs, {:3} branches, {:2} calls, {} unresolved{}",
            m,
            f,
            i.instrs,
            i.branches,
            i.calls.len(),
            i.unresolved,
            if i.cyclic { "  CYCLIC" } else { "" }
        );
    }
    println!(
        "TOTAL inlined: {} instrs, {} branches to if-convert, {} unresolved calls, \
         {} expand sites, {} field/index sites; cyclic reachable fns: {:?}",
        t_instr, t_branch, t_unres, t_expand, t_heap, cyclic_reachable
    );
}

/// Walk the program and render the name tables as the text of
/// `crates/celeste-names/src/gen.rs`.
///
/// Returns the TEXT rather than writing it, so the staleness gate
/// (`tests` below) can regenerate and compare in-process instead of
/// shelling out to the binary. `recipe_path` is `Some` when `program`
/// came from a recipe - it only names the source in the header, but it
/// names it CORRECTLY, which cost an afternoon once (see below).
pub fn emit_names(program: &Program, recipe_path: Option<&str>) -> Result<String> {
    let rewritten = recipe_path.is_some();
    let mut gen = Gen {
        current_fn: String::new(),
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

    // The tables below are the SIDE EFFECT of this walk (see
    // `walk_instruction`), so the walk stays even though nothing is
    // emitted from it any more.
    for (_, fun) in program.functions.iter() {
        gen.walk_function(fun)
            .with_context(|| format!("walk {}", fun.name.as_str()))?;
    }

    // Not emitted any more (nothing dispatches on a dense fn id), but
    // still looked up: a program without __init or __frame is not the
    // game, and this is the cheapest place to say so.
    gen.fn_ids["__init"];
    gen.fn_ids["__frame"];

    let mut out = String::new();
    out.push_str("// GENERATED by `cargo run --release --bin transpile` in the parent repo.\n");
    // Name the ACTUAL source program. This line used to claim the plain
    // program unconditionally, which was false for every canonical regen
    // (`transpile --recipe rewrites-compile.jsonl`) and cost real time:
    // anything written against the header - an interpreter reference, a
    // differential oracle - runs a different program, and the recipe's
    // rewrites change heap SHARING, so the mismatch shows up as "every
    // row differs" rather than as anything pointing back here.
    if rewritten {
        out.push_str(&format!(
            "// Source program: recipe::build(Recipe::load({:?})) - the REWRITTEN\n\
             // program, not the plain one. Do not edit.\n\n",
            recipe_path.unwrap()
        ));
    } else {
        out.push_str("// Source program: Program::compile_executable_from_disk() (the exact\n");
        out.push_str("// program concrete_run interprets). Do not edit.\n\n");
    }
    out.push_str(&str_array("STRINGS", &gen.strings.names));
    out.push_str(&str_array("GLOBAL_NAMES", &gen.globals.names));
    out.push_str(&str_array("FIELD_NAMES", &gen.fields.names));
    out.push_str(&str_array("FN_NAMES", &gen.fn_names));
    out.push_str(
        "pub fn global_id(name: &str) -> Option<u32> {\n    \
         GLOBAL_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n\
         pub fn field_id(name: &str) -> Option<u32> {\n    \
         FIELD_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n",
    );
    Ok(out)
}

#[cfg(test)]
mod tests {
    /// The gate that makes "checked in" safe.
    ///
    /// `crates/celeste-names/src/gen.rs` and
    /// `crates/celeste-kernels/src/kernel_gen_*.rs` are committed, so
    /// nothing in the build forces them to match the emitter that claims to
    /// produce them. This regenerates all four from the same inputs the
    /// canonical regen uses - the recipes and the frozen shape witnesses,
    /// both in git - and compares byte for byte.
    ///
    /// Byte-for-byte and not "semantically equivalent" on purpose.
    /// `FIELD_NAMES`' ORDER is the canonical field ordering `Cell2::Obj`
    /// interns against, so it feeds the shape hash, which feeds the row key,
    /// which is what the search dedups on. A reordering that a looser
    /// comparison would pass is a different search.
    ///
    /// If this fails, run `./regen-generated.sh` and commit the result -
    /// after checking the diff is what you meant, because a diff here moves
    /// the row keys.
    ///
    /// IGNORED BY DEFAULT (~44 s): it regenerates and compares. Still a
    /// GATE, and it runs on EVERY commit now. It used to cost ~44 s
    /// because it replayed ~1000 rewrite instructions to rebuild the
    /// program; the program is frozen data now, so it costs ~40 ms and
    /// there is no reason to hide it behind `--ignored`.
    #[test]
    fn generated_is_current() {
        let regen = |recipe: &str| {
            let program = crate::rewrite::frozen::rewritten(recipe)
                .unwrap_or_else(|e| panic!("load {} (run from the repo root): {}", recipe, e));
            program
        };
        let check = |path: &str, fresh: String| {
            let on_disk = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("read {}: {}", path, e));
            if on_disk != fresh {
                let (a, b) = (on_disk.lines().count(), fresh.lines().count());
                let first = on_disk
                    .lines()
                    .zip(fresh.lines())
                    .position(|(x, y)| x != y)
                    .map(|i| i + 1);
                panic!(
                    "{} is STALE: on disk {} lines, emitter says {} lines, \
                     first differing line {:?}. Run ./regen-generated.sh. \
                     A diff here moves the row keys - read it before committing.",
                    path, a, b, first
                );
            }
        };

        let names_program = regen("rewrites-compile.jsonl");
        check(
            "crates/celeste-names/src/gen.rs",
            super::emit_names(&names_program, Some("rewrites-compile.jsonl")).unwrap(),
        );

        for class in ["steady", "dash", "frozen"] {
            let recipe = format!("rewrites-trace10-{}.jsonl", class);
            let program = regen(&recipe);
            let witness = format!("crates/celeste-kernels/witness/{}-shape.json", class);
            let fresh = crate::transpile::kernel::emit_kernel_text(&program, &witness)
                .unwrap_or_else(|e| panic!("emit {} kernel: {:?}", class, e));
            check(&format!("crates/celeste-kernels/src/kernel_gen_{}.rs", class), fresh);
        }
    }

    /// The room (2,0) siblings of `generated_is_current`. A separate test
    /// because the start room is a process-global OnceLock: under nextest
    /// (the mandated runner) each test owns its process, so setting the
    /// env var first pins THIS process to the (2,0) compile.
    ///
    /// Runs on every commit, same as `generated_is_current`, and for the
    /// same reason: the (2,0) program is frozen data, so this is ~50 ms
    /// rather than the ~70 s it cost when it replayed the recipe.
    #[test]
    fn generated_is_current_r20() {
        std::env::set_var("CELESTE_START_ROOM", "2,0");
        let regen = |recipe: &str| {
            let program = crate::rewrite::frozen::rewritten(recipe)
                .unwrap_or_else(|e| panic!("load {} (run from the repo root): {}", recipe, e));
            program
        };
        let check = |path: &str, fresh: String| {
            let on_disk = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("read {}: {}", path, e));
            if on_disk != fresh {
                let (a, b) = (on_disk.lines().count(), fresh.lines().count());
                let first = on_disk
                    .lines()
                    .zip(fresh.lines())
                    .position(|(x, y)| x != y)
                    .map(|i| i + 1);
                panic!(
                    "{} is STALE: on disk {} lines, emitter says {} lines, \
                     first differing line {:?}. Run ./regen-generated.sh. \
                     A diff here moves the row keys - read it before committing.",
                    path, a, b, first
                );
            }
        };
        for class in ["steady", "dash", "frozen"] {
            let recipe = format!("rewrites-trace20-{}.jsonl", class);
            let program = regen(&recipe);
            let witness = format!("crates/celeste-kernels/witness/r20-{}-shape.json", class);
            let fresh = crate::transpile::kernel::emit_kernel_text(&program, &witness)
                .unwrap_or_else(|e| panic!("emit r20 {} kernel: {:?}", class, e));
            check(&format!("crates/celeste-kernels/src/kernel_gen_r20_{}.rs", class), fresh);
        }
    }
}
