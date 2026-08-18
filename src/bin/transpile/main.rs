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

mod kernel;

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

/// Slot binding loaded from a census `--emit-slots` dump: sites (keyed by
/// (fn name, instruction id) - stable across regenerations) whose result
/// is a fixed canonical boundary cell. See plans/columnar-engine.md
/// "Slot compilation".
struct SlotMap {
    /// (fn, iid) -> slot index.
    of_site: HashMap<(String, usize), u32>,
    /// Canonical boundary cell id per slot.
    cells: Vec<u32>,
    /// Slots disqualified by the escape analysis (any use of a bound
    /// site's result other than Load/Store/Kill means the pointer
    /// escapes; the CELL then has an access path outside the slot
    /// binding, so every site of that cell reverts to the generic path).
    bad: std::collections::HashSet<u32>,
    /// Original slot -> dense eligible-slot index. Bad slots get None
    /// and MUST NOT be bound at runtime: their cells are mutated by the
    /// generic path mid-tile, so a writeback would clobber them.
    dense: Vec<Option<u32>>,
    /// Canonical cell per DENSE slot (what gen.rs SLOT_CELLS emits).
    dense_cells: Vec<u32>,
    /// Canonical shape hash of the census states: the slot cell ids are
    /// only meaningful on this shape; runtimes must deopt off-shape.
    shape_hash: u64,
}

impl SlotMap {
    fn load(path: &str) -> Result<SlotMap> {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("read slot map {}", path))?;
        let json: serde_json::Value = serde_json::from_str(&text)?;
        let mut of_site = HashMap::new();
        let mut cells = Vec::new();
        for slot in json["slots"].as_array().context("slot map: no slots array")? {
            let k = slot["slot"].as_u64().context("slot id")? as u32;
            assert_eq!(k as usize, cells.len(), "slot ids must be dense and ordered");
            cells.push(slot["cell"].as_u64().context("slot cell")? as u32);
            for site in slot["sites"].as_array().context("slot sites")? {
                let fn_name = site["fn"].as_str().context("site fn")?.to_string();
                let iid = site["iid"].as_u64().context("site iid")? as usize;
                let prev = of_site.insert((fn_name, iid), k);
                assert!(prev.is_none(), "duplicate site in slot map");
            }
        }
        Ok(SlotMap {
            of_site,
            cells,
            bad: Default::default(),
            dense: Vec::new(),
            dense_cells: Vec::new(),
            shape_hash: json["shape_hash"]
                .as_u64()
                .context("slot map: no shape_hash (regenerate with --emit-slots)")?,
        })
    }

    /// After `analyze`: number the surviving slots densely.
    fn finalize(&mut self) {
        for (k, &cell) in self.cells.iter().enumerate() {
            if self.bad.contains(&(k as u32)) {
                self.dense.push(None);
            } else {
                self.dense.push(Some(self.dense_cells.len() as u32));
                self.dense_cells.push(cell);
            }
        }
    }

    /// Whole-program escape analysis: disqualify the slot of any bound
    /// site whose result local is used as anything but Load source /
    /// Store target / Kill hint (Kill is a liveness no-op on every
    /// probe engine, and slot engines see a nil there). Also checks
    /// every mapped site exists (stale-map detection).
    fn analyze(&mut self, program: &Program) {
        let mut seen = 0usize;
        for (_, fun) in program.functions.iter() {
            let fn_name = fun.name.as_str();
            // Locals defined by bound sites in this function.
            let mut site_local: HashMap<LocalId, u32> = HashMap::new();
            for (_, block) in blocks_in_order(&fun.cfg) {
                for (id, instr) in &block.instructions {
                    if matches!(
                        instr,
                        Instruction::GetField { .. } | Instruction::GetIndex { .. }
                    ) {
                        if let Some(&k) =
                            self.of_site.get(&(fn_name.to_string(), usize::from(*id)))
                        {
                            site_local.insert(*id, k);
                            seen += 1;
                        }
                    }
                }
            }
            if site_local.is_empty() {
                continue;
            }
            for (_, block) in blocks_in_order(&fun.cfg) {
                for (_, instr) in &block.instructions {
                    match instr {
                        Instruction::Load { source } => {
                            let _ = source; // Load through the pointer: fine.
                        }
                        Instruction::Store { target, source } => {
                            // Writing THROUGH the pointer is fine; storing
                            // the pointer AS A VALUE escapes it.
                            let _ = target;
                            if let Some(&k) = site_local.get(source) {
                                self.bad.insert(k);
                            }
                        }
                        Instruction::Kill { .. } => {}
                        other => {
                            for used in other.get_used_locals() {
                                if let Some(&k) = site_local.get(&used) {
                                    self.bad.insert(k);
                                }
                            }
                        }
                    }
                }
                for used in block.terminator.1.get_used_locals() {
                    if let Some(&k) = site_local.get(&used) {
                        self.bad.insert(k);
                    }
                }
            }
        }
        assert_eq!(
            seen,
            self.of_site.len(),
            "slot map has sites the program does not (stale map? regenerate with --emit-slots)"
        );
    }
}

struct Gen {
    /// Name of the function currently being emitted (assert diagnostics).
    current_fn: String,
    /// Defining instruction per local of the current function (static
    /// chain walks, e.g. expand -> button resolution).
    defs: HashMap<LocalId, Instruction>,
    /// Slot binding (None without --site-slots).
    slots: Option<SlotMap>,
    /// Locals of the CURRENT function holding an eligible bound site's
    /// result -> slot index (loads/stores through them become slot ops).
    local_slot: HashMap<LocalId, u32>,
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
    /// The DENSE slot for a site of the current function, if bound and
    /// eligible.
    fn slot_of(&self, iid: usize) -> Option<u32> {
        let s = self.slots.as_ref()?;
        let k = *s.of_site.get(&(self.current_fn.clone(), iid))?;
        s.dense[k as usize]
    }

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

    /// Statically resolve which button an `expand` site reads: walk the
    /// def chain expand <- load <- get_index(_, k_NAME+1) <- ... <-
    /// get_global "k_NAME". Returns the button BIT (the k_* value the
    /// harness Lua assigns, which is also the set_buttons array position).
    fn button_of_expand(defs: &HashMap<LocalId, Instruction>, value: LocalId) -> Option<u32> {
        const BITS: [(&str, u32); 6] = [
            ("k_left", 0),
            ("k_right", 1),
            ("k_up", 2),
            ("k_down", 3),
            ("k_jump", 4),
            ("k_dash", 5),
        ];
        let mut seen = 0;
        let mut stack = vec![value];
        while let Some(id) = stack.pop() {
            seen += 1;
            if seen > 64 {
                return None;
            }
            match defs.get(&id)? {
                Instruction::GetGlobal { name, .. } => {
                    if let Some((_, bit)) = BITS.iter().find(|(n, _)| n == name) {
                        return Some(*bit);
                    }
                }
                Instruction::Load { source } => stack.push(*source),
                Instruction::GetIndex { index, .. } => stack.push(*index),
                Instruction::BinaryOp { left, right, .. } => {
                    stack.push(*left);
                    stack.push(*right);
                }
                _ => {}
            }
        }
        None
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
            Instruction::Load { source } => match self.local_slot.get(source) {
                Some(&k) => writeln!(
                    out,
                    "{} = if E::HAS_SLOTS {{ rt.slot_get({}) }} else {{ rt.load({}) }};",
                    d,
                    k,
                    l(*source)
                )?,
                None => writeln!(out, "{} = rt.load({});", d, l(*source))?,
            },
            Instruction::Store { target, source } => match self.local_slot.get(target) {
                Some(&k) => writeln!(
                    out,
                    "if E::HAS_SLOTS {{ rt.slot_set({}, {}) }} else {{ rt.store({}, {}) }};",
                    k,
                    l(*source),
                    l(*target),
                    l(*source)
                )?,
                None => writeln!(out, "rt.store({}, {});", l(*target), l(*source))?,
            },
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
                let generic = format!(
                    "rt.get_field({}, {}, {}, {})",
                    l(*receiver),
                    f,
                    create_if_missing,
                    site
                );
                match self.slot_of(usize::from(id)) {
                    Some(k) => {
                        self.local_slot.insert(id, k);
                        writeln!(
                            out,
                            "{} = if E::HAS_SLOTS {{ rt.c_nil() }} else {{ {} }}; // .{} slot {}",
                            d, generic, field, k
                        )?;
                    }
                    None => writeln!(out, "{} = {}; // .{}", d, generic, field)?,
                }
            }
            Instruction::GetIndex { receiver, index, create_if_missing } => {
                let site = self.site_info.len() as u32;
                self.site_info.push(("index", self.current_fn.clone(), 0, usize::from(id)));
                let generic = format!(
                    "rt.get_index({}, {}, {}, {})",
                    l(*receiver),
                    l(*index),
                    create_if_missing,
                    site
                );
                match self.slot_of(usize::from(id)) {
                    Some(k) => {
                        self.local_slot.insert(id, k);
                        writeln!(
                            out,
                            "{} = if E::HAS_SLOTS {{ rt.c_nil() }} else {{ {} }}; // slot {}",
                            d, generic, k
                        )?;
                    }
                    None => writeln!(out, "{} = {};", d, generic)?,
                }
            }
            Instruction::NumberConstant { value } => {
                let bits = value.to_bits();
                writeln!(
                    out,
                    "{} = rt.c_num({}, {}); // {:?}",
                    d,
                    (bits >> 16) as i16,
                    bits as u16,
                    value
                )?;
            }
            Instruction::BoolConstant { value } => writeln!(out, "{} = rt.c_bool({});", d, value)?,
            Instruction::StringConstant { value } => {
                let s = self.strings.intern(value);
                writeln!(out, "{} = rt.c_str({}); // {:?}", d, s, value)?;
            }
            Instruction::NilConstant => writeln!(out, "{} = rt.c_nil();", d)?,
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
                    "{} = rt.select({}, {}, {});",
                    d,
                    l(*condition),
                    l(*if_true),
                    l(*if_false)
                )?;
            }
            Instruction::Kill { .. } => {
                let used: Vec<String> = instr.get_used_locals().into_iter().map(l).collect();
                writeln!(out, "rt.kill(&[{}]);", used.join(", "))?;
            }
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
            Instruction::Expand { value } => match Self::button_of_expand(&self.defs, *value) {
                Some(bit) => writeln!(
                    out,
                    "{} = rt.expand_btn::<{}>({});",
                    d,
                    bit,
                    l(*value)
                )?,
                None => writeln!(out, "{} = rt.expand({});", d, l(*value))?,
            },
        }
        Ok(())
    }

    fn emit_function(&mut self, fn_id: u32, fun: &FunDef) -> Result<String> {
        let fn_name = fun.name.as_str().to_string();
        self.current_fn = fn_name.clone();
        self.local_slot.clear();
        self.defs = fun
            .cfg
            .iter_blocks()
            .flat_map(|b| b.instructions.iter().cloned())
            .collect();
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

        // Local scoping: a single-def local used only inside its own
        // block is declared INLINE (`let lN = ...`) so LLVM can keep it
        // in a register. The old scheme - fn-scope `let mut` for every
        // local - put ~3200 temporaries on one huge stack frame, and
        // the profile showed f_15's self time dominated by stack
        // reloads of those E::V values. Phi targets/sources, captures
        // and args stay fn-scope (multi-assigned or cross-block).
        let mut upfront: BTreeSet<usize> = BTreeSet::new();
        let mut def_blk: HashMap<usize, usize> = HashMap::new();
        for id in fun.capture_ids.iter() {
            upfront.insert(usize::from(*id));
        }
        for id in fun.arg_ids.iter().flatten() {
            upfront.insert(usize::from(*id));
        }
        for (bi, (_, block)) in blocks.iter().enumerate() {
            for (id, instr) in &block.instructions {
                if let Instruction::Phi { branches } = instr {
                    upfront.insert(usize::from(*id));
                    for (_, src) in branches {
                        upfront.insert(usize::from(*src));
                    }
                    continue;
                }
                let prev = def_blk.insert(usize::from(*id), bi);
                assert!(prev.is_none(), "{}: %{} defined twice", fn_name, usize::from(*id));
            }
        }
        let mut used_outside: BTreeSet<usize> = BTreeSet::new();
        for (bi, (_, block)) in blocks.iter().enumerate() {
            let mark = |id: LocalId, used_outside: &mut BTreeSet<usize>| {
                let id = usize::from(id);
                if def_blk.get(&id) != Some(&bi) {
                    used_outside.insert(id);
                }
            };
            for (_, instr) in &block.instructions {
                if matches!(instr, Instruction::Phi { .. }) {
                    continue;
                }
                for u in instr.get_used_locals() {
                    mark(u, &mut used_outside);
                }
            }
            for u in block.terminator.1.get_used_locals() {
                mark(u, &mut used_outside);
            }
        }
        let inline_locals: BTreeSet<usize> = def_blk
            .keys()
            .copied()
            .filter(|id| !upfront.contains(id) && !used_outside.contains(id))
            .collect();

        let mut out = String::new();
        writeln!(out, "/// `{}`", fn_name)?;
        writeln!(out, "pub fn f_{}<E: Engine>(rt: &mut E, caps: &[E::V], args: &[E::V]) -> E::V {{", fn_id)?;
        for id in Self::collect_locals(fun) {
            if !inline_locals.contains(&id) {
                writeln!(out, "    let mut l{}: E::V = rt.c_nil();", id)?;
            }
        }
        for (i, cap) in fun.capture_ids.iter().enumerate() {
            writeln!(out, "    {} = caps[{}];", l(*cap), i)?;
        }
        for (i, arg) in fun.arg_ids.iter().enumerate() {
            if let Some(arg) = arg {
                // Missing arguments pad with Nil (core_interpreter.rs:762).
                writeln!(out, "    {} = match args.get({}) {{ Some(v) => *v, None => rt.c_nil() }};", l(*arg), i)?;
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
                if inline_locals.contains(&usize::from(*id)) {
                    // Block-scoped single-def local: `let lN = ...`.
                    let mut one = String::new();
                    self.emit_instruction(*id, instr, &mut one)?;
                    // Statement instructions (Store/Kill/asserts) carry
                    // an id but assign nothing - leave those untouched.
                    let pat = format!("{} = ", l(*id));
                    if let Some(at) = one.find(&pat) {
                        one.insert_str(at, "let ");
                    }
                    body.push_str(&one);
                } else {
                    self.emit_instruction(*id, instr, &mut body)?;
                }
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
                    None => writeln!(body, "return rt.c_nil();")?,
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

/// Feasibility recon for the SIMD kernel emitter (plans/columnar-engine.md
/// stage 2): from `__frame`, how much of the program inlines statically?
/// - calls: resolvable via a dominating AssertClosure (approximated
///   per-function - the devirt entries assert right before the call)?
/// - control flow: is every reachable CFG a DAG (if-convertible)?
/// - size: multiplicity-weighted instruction/branch counts after full
///   inlining - the straight-line kernel's length.
fn kernel_recon(program: &celeste_rust::rewrite::program::Program) {
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

fn main() -> Result<()> {
    let mut rewritten = false;
    let mut recipe_path = "rewrites.jsonl".to_string();
    let mut out_path = "native-probe/src/gen.rs".to_string();
    let mut site_slots: Option<String> = None;
    let mut kernel_recon_flag = false;
    let mut kernel_out: Option<(String, String)> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--kernel-recon" => kernel_recon_flag = true,
            "--kernel" => {
                let witness = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                let out = args.next().ok_or_else(|| anyhow!("--kernel WITNESS OUT"))?;
                kernel_out = Some((witness, out));
            }
            "--rewritten" => rewritten = true,
            "--recipe" => {
                recipe_path = args
                    .next()
                    .ok_or_else(|| anyhow::anyhow!("--recipe needs a path"))?;
                rewritten = true;
            }
            "--site-slots" => {
                site_slots = Some(
                    args.next()
                        .ok_or_else(|| anyhow::anyhow!("--site-slots needs a path"))?,
                );
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

    if kernel_recon_flag {
        kernel_recon(&program);
        return Ok(());
    }
    if let Some((witness, out)) = kernel_out {
        return kernel::emit_kernel(&program, &witness, &out);
    }

    let slots = match &site_slots {
        Some(path) => {
            let mut map = SlotMap::load(path)?;
            map.analyze(&program);
            map.finalize();
            let n_sites = map.of_site.len();
            let n_bad_sites = map
                .of_site
                .values()
                .filter(|k| map.bad.contains(k))
                .count();
            eprintln!(
                "site-slots: {} of {} slots eligible ({} sites bound, {} reverted by escape analysis)",
                map.dense_cells.len(),
                map.cells.len(),
                n_sites - n_bad_sites,
                n_bad_sites
            );
            Some(map)
        }
        None => None,
    };

    let mut gen = Gen {
        current_fn: String::new(),
        defs: HashMap::new(),
        slots,
        local_slot: HashMap::new(),
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

    // The walk still RUNS, and its output is still thrown away on purpose.
    //
    // gen.rs no longer carries a program body - the interpreter is the only
    // reference implementation of a frame (K4 stage 2) and the kernels are
    // the only compiled one - but the name tables above are a SIDE EFFECT of
    // this walk: `emit_function` is what interns strings, globals, fields,
    // sites and branches, in the order it meets them. FIELD_NAMES in
    // particular is the canonical field ordering the boundary hashes, so a
    // walk that visits in a different order silently produces a different
    // shape hash, a different row key and a different search.
    //
    // So: keep the walk, drop the text. Turning the emitter into a pure
    // interning walk is a separate change with its own gate (regenerate and
    // require the name-table section byte-identical) - doing it here would
    // mean two candidate causes for any table that moved.
    for (i, (_, fun)) in program.functions.iter().enumerate() {
        let code = gen
            .emit_function(i as u32, fun)
            .with_context(|| format!("emit {}", fun.name.as_str()))?;
        drop(code);
    }

    let fn_init = gen.fn_ids["__init"];
    let fn_frame = gen.fn_ids["__frame"];

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
            recipe_path
        ));
    } else {
        out.push_str("// Source program: Program::compile_executable_from_disk() (the exact\n");
        out.push_str("// program concrete_run interprets). Do not edit.\n\n");
    }
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
    out.push_str(&format!("pub const FN_FRAME: u32 = {};\n", fn_frame));
    let slot_cells: &[u32] = gen
        .slots
        .as_ref()
        .map(|s| s.dense_cells.as_slice())
        .unwrap_or(&[]);
    out.push_str(
        "/// Canonical boundary cell per slot (slot compilation; empty\n\
         /// without --site-slots). Engines with HAS_SLOTS bind these\n\
         /// cells to a dense array at block entry and write back at exit.\n",
    );
    out.push_str(&format!("pub const N_SLOTS: usize = {};\n", slot_cells.len()));
    out.push_str(&format!(
        "pub static SLOT_CELLS: &[u32] = &{:?};\n",
        slot_cells
    ));
    out.push_str(
        "/// Canonical shape hash the slot binding is valid for; slot\n\
         /// engines must deopt on any other shape.\n",
    );
    out.push_str(&format!(
        "pub const SLOT_SHAPE: u64 = {:#018x};\n\n",
        gen.slots.as_ref().map(|s| s.shape_hash).unwrap_or(0)
    ));
    out.push_str(
        "pub fn global_id(name: &str) -> Option<u32> {\n    \
         GLOBAL_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n\
         pub fn field_id(name: &str) -> Option<u32> {\n    \
         FIELD_NAMES.iter().position(|n| *n == name).map(|i| i as u32)\n}\n\n",
    );
    std::fs::write(&out_path, out).with_context(|| format!("write {}", out_path))?;
    eprintln!(
        "wrote {} ({} functions walked, {} globals, {} fields, {} strings)",
        out_path,
        gen.fn_names.len(),
        gen.globals.names.len(),
        gen.fields.names.len(),
        gen.strings.names.len()
    );
    Ok(())
}
