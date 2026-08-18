//! The KERNEL EMITTER (plans/kernel-plan.md K1).
//!
//! Emits `native-probe/src/kernel_gen.rs`: a fully-typed, straight-line,
//! branch-free lane program for the STEADY class (player, freeze=0,
//! dash_time=0), compiled from the certified steady overlay
//! (`rewrites-trace10-steady.jsonl`) against a SHAPE WITNESS
//! (`native-probe --emit-shape`).
//!
//! The emitter is an abstract evaluator run at EMIT TIME:
//! - pointer topology (get_global/get_field/get_index) resolves against
//!   the witness, so no heap exists at runtime;
//! - stores are SSA renames of an emit-time cell map, so the program's
//!   387 loads / 129 stores become register traffic;
//! - every value has a static class: emit-time constant, S (block-uniform
//!   scalar, computed once per block), or Z (per-lane, 16 rows per zmm);
//! - the 6 button UBools + 8 expand sites become the prefix/suffix cut:
//!   one shared prefix per row slice, 64 monomorphized suffixes with the
//!   buttons as compile-time constants (LLVM folds each variant);
//! - abstract-domain limits (straddling splits, unknown compares, masked
//!   guard failures) are PER-LANE DEOPT bits, never errors. Uniform-value
//!   limits set a whole-slice deopt flag.
//!
//! Anything outside the certified steady trace's shape is a loud
//! emit-time error - the witness IS the domain, and silence would hide a
//! soundness hole.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write as _;

use anyhow::{anyhow, bail, Context, Result};
use celeste_rust::ir::{BinaryOp, Instruction, LocalId, Terminator, UnaryOp};
use celeste_rust::pico8_num::Pico8Num as P8;
use celeste_rust::rewrite::print::blocks_in_order;
use celeste_rust::rewrite::program::Program;

/// Emit-time value. `S*` strings are generated VARIABLE NAMES (each IR
/// instruction result is let-bound), never raw expressions.
#[derive(Clone, Debug, PartialEq)]
enum K {
    Nil,
    NilPtr,
    /// Pointer to witness/scratch cell.
    Ptr(u32),
    /// Emit-time constant number.
    NumC(P8),
    BoolC(bool),
    StrC(String),
    /// Uniform unknown bool; `btn` = the __button_states index it was
    /// loaded from (expansion needs it).
    UBool { btn: Option<u8> },
    /// Block-uniform runtime scalars: P8 / (P8, P8) / bool / Option<bool>.
    SN(String),
    SI(String),
    SB(String),
    STri(String),
    /// Per-lane: ZN / ZI / ZB (kernel.rs types).
    ZN(String),
    ZI(String),
    ZB(String),
    /// __split_at result: per-lane interval + "exactly the split point"
    /// mask (those lanes behave as the NUMBER `at` under equality).
    ZIP { v: String, pt: String, at: P8 },
}

/// Emit-time cell content.
#[derive(Clone, Debug)]
enum CellT {
    Val(K),
    Obj(BTreeMap<String, u32>),
    Arr(Vec<u32>),
    Clo(String),
    Bi(String),
    Unk,
}

struct Emit {
    /// Number of witness cells (ids below this are boundary state; at or
    /// above are frame-local scratch).
    witness_len: u32,
    cells: HashMap<u32, CellT>,
    globals: HashMap<String, u32>,
    next_cell: u32,
    env: HashMap<LocalId, K>,
    /// Uniform cells bound at runtime: cell id -> kind ("num"|"ival"|"bool").
    uni: BTreeMap<u32, &'static str>,
    /// Varying input cells: id -> ("num"|"bool").
    vary_in: BTreeMap<u32, &'static str>,
    /// Cells written by a Store anywhere in the frame.
    dirty: BTreeSet<u32>,
    pre: String,
    suf: String,
    n: usize,
    /// name -> rust type of every generated let (for the Pre struct).
    var_ty: HashMap<String, &'static str>,
    /// Names defined in the prefix (crossing detection).
    pre_defs: BTreeSet<String>,
    shape_hash: String,
    /// Cross-block-stable uniform num values from the witness. A fold
    /// that CONSUMES one records a pin: bind() then guards the cell's
    /// runtime value against it (mismatch = the block takes the
    /// interpreter path).
    stable: HashMap<u32, P8>,
    /// var name -> (known value, uniform cells it derives from).
    pin_val: HashMap<String, (P8, BTreeSet<u32>)>,
    /// Cells whose stable value a fold consumed (bind-time guards).
    pins: BTreeSet<u32>,
    /// __button_states cells: cell id -> button index 0..5. An UnknownBool
    /// loaded from one carries that provenance into `expand` even when it
    /// was freshly minted this frame (mint-store-reload strips the tag).
    button_cells: HashMap<u32, u8>,
    /// Open fork loops (each __split_by_flr on a per-lane interval is a
    /// <=2-way fork emitted as a runtime loop; the rest of the program
    /// nests inside). Render closes this many braces at the end.
    fork_depth: usize,
    /// Name of the current per-lane validity mask ("ALL" at depth 0).
    valid_expr: String,
    /// Button-TAINT tracking (cross-variant sharing): only instructions
    /// whose value depends on a button land in the x64 suffix; everything
    /// else is hoisted to the shared per-fork-config prefix, which is
    /// sound because an untainted op only reads untainted defs (all in
    /// the prefix) and the emit-time SSA evaluation already captured the
    /// correct pre/post-store cell values.
    tainted_vars: BTreeSet<String>,
    tainted_cells: BTreeSet<u32>,
    /// Taint of the instruction currently being emitted (routes buffers).
    cur_tainted: bool,
}

impl Emit {
    fn buf(&mut self) -> &mut String {
        if self.cur_tainted {
            &mut self.suf
        } else {
            &mut self.pre
        }
    }
    fn line(&mut self, s: &str) {
        let indent = "    ";
        let text = format!("{}{}\n", indent, s);
        self.buf().push_str(&text);
    }
    /// Let-bind `expr` of rust type `ty`; returns the variable name.
    fn bind(&mut self, ty: &'static str, expr: &str) -> String {
        let name = format!("v{}", self.n);
        self.n += 1;
        self.line(&format!("let {}: {} = {};", name, ty, expr));
        self.var_ty.insert(name.clone(), ty);
        if self.cur_tainted {
            self.tainted_vars.insert(name.clone());
        } else {
            self.pre_defs.insert(name.clone());
        }
        name
    }
    /// Is this emit-time value button-dependent?
    fn k_tainted(&self, k: &K) -> bool {
        match k {
            K::SN(v) | K::SI(v) | K::SB(v) | K::STri(v) | K::ZN(v) | K::ZI(v) | K::ZB(v) => {
                self.tainted_vars.contains(v)
            }
            K::ZIP { v, pt, .. } => {
                self.tainted_vars.contains(v) || self.tainted_vars.contains(pt)
            }
            _ => false,
        }
    }

    // ---- lifts ----
    /// Any numeric K as a ZN variable (broadcast scalars).
    fn as_zn(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZN(v) => v.clone(),
            K::NumC(c) => self.bind("ZN", &format!("zn_splat({})", p8(c))),
            K::SN(v) => self.bind("ZN", &format!("zn_splat({})", v)),
            other => bail!("as_zn on {:?}", other),
        })
    }
    /// Any interval-or-number K as a ZI variable.
    fn as_zi(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZI(v) => v.clone(),
            K::ZIP { v, .. } => v.clone(),
            K::ZN(v) => self.bind("ZI", &format!("zi_of_zn({})", v)),
            K::NumC(c) => self.bind("ZI", &format!("zi_splat({}, {})", p8(c), p8(c))),
            K::SN(v) => self.bind("ZI", &format!("zi_splat({}, {})", v, v)),
            K::SI(v) => self.bind("ZI", &format!("zi_splat({}.0, {}.1)", v, v)),
            other => bail!("as_zi on {:?}", other),
        })
    }
    fn as_zb(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZB(v) => v.clone(),
            K::BoolC(b) => self.bind("ZB", &format!("zb_splat({})", b)),
            K::SB(v) => self.bind("ZB", &format!("zb_splat({})", v)),
            other => bail!("as_zb on {:?}", other),
        })
    }
    /// Scalar interval expression for S-level interval math.
    fn as_si(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::SI(v) => v.clone(),
            K::SN(v) => self.bind("(P8, P8)", &format!("({}, {})", v, v)),
            K::NumC(c) => self.bind("(P8, P8)", &format!("({}, {})", p8(&c.clone()), p8(c))),
            other => bail!("as_si on {:?}", other),
        })
    }

    fn is_z(k: &K) -> bool {
        matches!(k, K::ZN(_) | K::ZI(_) | K::ZB(_) | K::ZIP { .. })
    }
    fn is_ival(k: &K) -> bool {
        matches!(k, K::SI(_) | K::ZI(_) | K::ZIP { .. })
    }
    fn is_num(k: &K) -> bool {
        matches!(k, K::NumC(_) | K::SN(_) | K::ZN(_))
    }
}

fn p8(v: &P8) -> String {
    format!("P8::from_raw({}i32)", v.as_raw_u32() as i32)
}

/// Load the shape witness JSON into the emit-time cell map.
fn load_witness(path: &str, e: &mut Emit) -> Result<()> {
    let text = std::fs::read_to_string(path).with_context(|| format!("witness {}", path))?;
    let w: serde_json::Value = serde_json::from_str(&text)?;
    e.shape_hash = w["shape_hash"].as_str().unwrap_or("0").to_string();
    let cells = w["cells"].as_array().ok_or_else(|| anyhow!("witness: no cells"))?;
    for (id, c) in cells.iter().enumerate() {
        let id = id as u32;
        let name = c["name"].as_str().unwrap_or("");
        let vary = c["vary"].as_bool().unwrap_or(false);
        let t = match c["k"].as_str().unwrap_or("") {
            "val" => {
                let content = &c["content"];
                let k = if vary {
                    match content.as_str() {
                        Some("num") => {
                            e.vary_in.insert(id, "num");
                            K::ZN(format!("r_c{}", id))
                        }
                        Some("bool") => {
                            e.vary_in.insert(id, "bool");
                            K::ZB(format!("r_c{}", id))
                        }
                        other => bail!("cell {} ({}): varying {:?} unsupported", id, name, other),
                    }
                } else {
                    match content.as_str() {
                        Some("num") => {
                            e.uni.insert(id, "num");
                            if let Some(v) = c["val"].as_i64() {
                                e.stable.insert(id, P8::from_raw(v as i32));
                            }
                            K::SN(format!("u.c{}", id))
                        }
                        Some("ival") => {
                            e.uni.insert(id, "ival");
                            K::SI(format!("u.c{}", id))
                        }
                        Some("bool") => {
                            e.uni.insert(id, "bool");
                            K::SB(format!("u.c{}", id))
                        }
                        Some("ubool") => {
                            let btn = name
                                .strip_prefix("__button_states.")
                                .and_then(|s| s.parse::<u8>().ok())
                                .map(|n| n - 1);
                            if let Some(b) = btn {
                                e.button_cells.insert(id, b);
                            }
                            K::UBool { btn }
                        }
                        Some("nil") => K::Nil,
                        Some("nilptr") => K::NilPtr,
                        Some("str") => K::StrC(String::new()),
                        None => {
                            if let Some(t) = content["ptr"].as_u64() {
                                K::Ptr(t as u32)
                            } else {
                                bail!("cell {} ({}): content {:?}", id, name, content)
                            }
                        }
                        other => bail!("cell {} ({}): content {:?}", id, name, other),
                    }
                };
                CellT::Val(k)
            }
            "obj" => {
                let fields = c["fields"]
                    .as_object()
                    .ok_or_else(|| anyhow!("obj cell {} without fields", id))?
                    .iter()
                    .map(|(k, v)| (k.clone(), v.as_u64().unwrap() as u32))
                    .collect();
                CellT::Obj(fields)
            }
            "arr" => CellT::Arr(
                c["items"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| v.as_u64().unwrap() as u32)
                    .collect(),
            ),
            "clo" => CellT::Clo(c["fn"].as_str().unwrap().to_string()),
            "bi" => CellT::Bi(c["name"].as_str().unwrap().to_string()),
            "unk" => CellT::Unk,
            other => bail!("cell {}: kind {:?}", id, other),
        };
        e.cells.insert(id, t);
    }
    e.next_cell = cells.len() as u32;
    e.witness_len = cells.len() as u32;
    for (name, id) in w["globals"].as_object().ok_or_else(|| anyhow!("no globals"))? {
        e.globals.insert(name.clone(), id.as_u64().unwrap() as u32);
    }
    Ok(())
}

pub fn emit_kernel(program: &Program, witness_path: &str, out_path: &str) -> Result<()> {
    let mut e = Emit {
        witness_len: 0,
        cells: HashMap::new(),
        globals: HashMap::new(),
        next_cell: 0,
        env: HashMap::new(),
        uni: BTreeMap::new(),
        vary_in: BTreeMap::new(),
        dirty: BTreeSet::new(),
        pre: String::new(),
        suf: String::new(),
        n: 0,
        var_ty: HashMap::new(),
        pre_defs: BTreeSet::new(),
        shape_hash: String::new(),
        stable: HashMap::new(),
        pin_val: HashMap::new(),
        pins: BTreeSet::new(),
        button_cells: HashMap::new(),
        fork_depth: 0,
        valid_expr: "ALL".to_string(),
        tainted_vars: BTreeSet::new(),
        tainted_cells: BTreeSet::new(),
        cur_tainted: false,
    };
    load_witness(witness_path, &mut e)?;
    for k in 0..6 {
        e.tainted_vars.insert(format!("kb{}", k));
    }

    let fun = program
        .functions
        .iter()
        .find(|(name, _)| name.as_str() == "__frame")
        .map(|(_, f)| f)
        .ok_or_else(|| anyhow!("no __frame"))?;
    // Row inputs materialize first (prefix reads them by name).
    for (id, kind) in e.vary_in.clone() {
        match kind {
            "num" => e.line(&format!("let r_c{}: ZN = rin.c{};", id, id)),
            "bool" => e.line(&format!(
                "let r_c{}: ZB = ZB {{ val: rin.c{}, known: ALL }};",
                id, id
            )),
            _ => unreachable!(),
        }
        if kind == "num" {
            e.var_ty.insert(format!("r_c{}", id), "ZN");
        } else {
            e.var_ty.insert(format!("r_c{}", id), "ZB");
        }
        e.pre_defs.insert(format!("r_c{}", id));
    }

    // Walk the straight line in CONTROL order (blocks_in_order is storage
    // order; the mint/reset block sits after the update's return).
    let by_label: HashMap<String, _> = blocks_in_order(&fun.cfg)
        .into_iter()
        .map(|(label, block)| (label.to_string(), block))
        .collect();
    let mut label = "__entry".to_string();
    let mut visited = 0usize;
    loop {
        let block = by_label
            .get(&label)
            .ok_or_else(|| anyhow!("no block {:?}", label))?;
        visited += 1;
        if visited > by_label.len() {
            bail!("control cycle through {:?}", label);
        }
        for (id, instr) in &block.instructions {
            // Button-taint of this instruction: any tainted operand, a
            // load through a tainted cell, or the expand itself.
            let mut t = instr
                .get_used_locals()
                .iter()
                .any(|u| e.env.get(u).map(|k| e.k_tainted(k)).unwrap_or(false));
            if let Instruction::Load { source } = instr {
                if let Some(K::Ptr(c)) = e.env.get(source) {
                    t |= e.tainted_cells.contains(c);
                }
            }
            if matches!(instr, Instruction::Expand { .. }) {
                t = true;
            }
            e.cur_tainted = t;
            let k = eval(&mut e, instr)
                .with_context(|| format!("{}: %{} = {:?}", label, usize::from(*id), instr))?;
            if let Some(k) = k {
                e.env.insert(*id, k);
            }
        }
        match &block.terminator.1 {
            Terminator::UnconditionalBranch { target } => label = target.as_str().to_string(),
            Terminator::Return { .. } => break,
            other => bail!("{}: terminator {:?} is not straight-line", label, other),
        }
    }

    render(&e, out_path)
}

/// One instruction of the straight line, evaluated at emit time.
fn eval(e: &mut Emit, instr: &Instruction) -> Result<Option<K>> {
    use Instruction as I;
    Ok(match instr {
        I::Alloc => {
            let id = e.next_cell;
            e.next_cell += 1;
            e.cells.insert(id, CellT::Val(K::Nil));
            Some(K::Ptr(id))
        }
        I::GetGlobal { name, create_if_missing } => match e.globals.get(name) {
            Some(id) => Some(K::Ptr(*id)),
            None if *create_if_missing => {
                let id = e.next_cell;
                e.next_cell += 1;
                e.cells.insert(id, CellT::Val(K::Nil));
                e.globals.insert(name.clone(), id);
                Some(K::Ptr(id))
            }
            None => bail!("get_global {:?}: not in witness", name),
        },
        I::Load { source } => {
            let k = e.env.get(source).cloned().ok_or_else(|| anyhow!("load of unset"))?;
            match k {
                // Load through a nil pointer is Nil (core_interpreter.rs).
                K::NilPtr => Some(K::Nil),
                K::Ptr(c) => match e
                    .cells
                    .get(&c)
                    .cloned()
                    .ok_or_else(|| anyhow!("load of missing cell {}", c))?
                {
                    CellT::Val(v) => {
                        // Seed value provenance for stable uniforms so
                        // topology-relevant folds (get_index) can consume
                        // them, guarded by a bind-time pin.
                        if let (K::SN(name), Some(val)) = (&v, e.stable.get(&c)) {
                            let mut cells = BTreeSet::new();
                            cells.insert(c);
                            e.pin_val.insert(name.clone(), (*val, cells));
                        }
                        // Button-cell provenance survives mint-store-reload.
                        if let (K::UBool { btn: None }, Some(b)) = (&v, e.button_cells.get(&c)) {
                            Some(K::UBool { btn: Some(*b) })
                        } else {
                            Some(v)
                        }
                    }
                    // Tables/closures/builtins load as a pointer to the cell.
                    _ => Some(K::Ptr(c)),
                },
                other => bail!("load of {:?}", other),
            }
        }
        I::Store { target, source } => {
            let t = e.env.get(target).cloned().ok_or_else(|| anyhow!("store target unset"))?;
            let K::Ptr(c) = t else { bail!("store into {:?}", t) };
            let v = e.env.get(source).cloned().ok_or_else(|| anyhow!("store source unset"))?;
            if e.cur_tainted || e.k_tainted(&v) {
                e.tainted_cells.insert(c);
            } else {
                e.tainted_cells.remove(&c);
            }
            e.cells.insert(c, CellT::Val(v));
            e.dirty.insert(c);
            None
        }
        I::StoreEmptyTable { target } => {
            let t = e.env.get(target).cloned().unwrap();
            let K::Ptr(c) = t else { bail!("store_empty_table into {:?}", t) };
            e.cells.insert(c, CellT::Obj(BTreeMap::new()));
            e.dirty.insert(c);
            None
        }
        I::StoreClosure { target, fun_def, .. } => {
            let t = e.env.get(target).cloned().unwrap();
            let K::Ptr(c) = t else { bail!("store_closure into {:?}", t) };
            e.cells.insert(c, CellT::Clo(fun_def.as_str().to_string()));
            e.dirty.insert(c);
            None
        }
        I::GetField { receiver, field, create_if_missing } => {
            let r = e.env.get(receiver).cloned().unwrap();
            let K::Ptr(c) = r else { bail!("get_field on {:?}", r) };
            let cell = e.cells.get(&c).cloned().unwrap();
            match cell {
                CellT::Obj(mut fields) => match fields.get(field) {
                    Some(f) => Some(K::Ptr(*f)),
                    None if *create_if_missing => {
                        let id = e.next_cell;
                        e.next_cell += 1;
                        e.cells.insert(id, CellT::Val(K::Nil));
                        fields.insert(field.clone(), id);
                        e.cells.insert(c, CellT::Obj(fields));
                        Some(K::Ptr(id))
                    }
                    None => Some(K::NilPtr),
                },
                other => bail!("get_field {:?} on {:?}", field, other),
            }
        }
        I::GetIndex { receiver, index, create_if_missing } => {
            let r = e.env.get(receiver).cloned().unwrap();
            let K::Ptr(c) = r else { bail!("get_index on {:?}", r) };
            let CellT::Arr(items) = e.cells.get(&c).cloned().unwrap() else {
                bail!("get_index on non-array cell {}", c)
            };
            let i = e.env.get(index).cloned().unwrap();
            let n = match &i {
                K::NumC(n) => *n,
                other => match known_of(e, other) {
                    Some((n, cells)) => {
                        e.pins.extend(cells);
                        n
                    }
                    None => bail!("get_index with non-constant index {:?} (fold failed)", i),
                },
            };
            let idx = n
                .as_i16()
                .ok_or_else(|| anyhow!("get_index: fractional index {:?}", n))?;
            if idx >= 1 && (idx as usize) <= items.len() {
                Some(K::Ptr(items[idx as usize - 1]))
            } else if *create_if_missing {
                bail!("get_index create past the end ({} of {})", idx, items.len())
            } else {
                Some(K::NilPtr)
            }
        }
        I::NumberConstant { value } => Some(K::NumC(*value)),
        I::BoolConstant { value } => Some(K::BoolC(*value)),
        I::StringConstant { value } => Some(K::StrC(value.clone())),
        I::NilConstant => Some(K::Nil),
        I::UnaryOp { op, arg } => {
            let a = e.env.get(arg).cloned().unwrap();
            Some(unop(e, *op, &a)?)
        }
        I::BinaryOp { left, op, right } => {
            let l = e.env.get(left).cloned().unwrap();
            let r = e.env.get(right).cloned().unwrap();
            Some(binop(e, *op, &l, &r)?)
        }
        I::Select { condition, if_true, if_false } => {
            let c = e.env.get(condition).cloned().unwrap();
            let t = e.env.get(if_true).cloned().unwrap();
            let f = e.env.get(if_false).cloned().unwrap();
            Some(select(e, &c, &t, &f)?)
        }
        I::AssertTrue { value } => {
            let v = e.env.get(value).cloned().unwrap();
            match v {
                K::BoolC(true) => {}
                K::BoolC(false) => e.line("*bd = true; // assert_true of constant false"),
                K::SB(v) => e.line(&format!("if !{} {{ *bd = true; }}", v)),
                K::STri(v) => e.line(&format!("if {} != Some(true) {{ *bd = true; }}", v)),
                K::ZB(v) => e.line(&format!("zguard({}, &mut dp);", v)),
                K::UBool { .. } => e.line("*bd = true; // assert_true of UnknownBool"),
                other => bail!("assert_true on {:?}", other),
            }
            None
        }
        I::AssertClosure { value, fun_def, .. } => {
            let v = e.env.get(value).cloned().unwrap();
            let K::Ptr(c) = v else { bail!("assert_closure on {:?}", v) };
            match e.cells.get(&c) {
                Some(CellT::Clo(name)) if name == fun_def.as_str() => None,
                other => bail!(
                    "assert_closure {:?}: witness cell {} is {:?}",
                    fun_def.as_str(),
                    c,
                    other
                ),
            }
        }
        I::AssertPointer { value } => {
            let v = e.env.get(value).cloned().unwrap();
            match v {
                K::Ptr(_) => None,
                other => bail!("assert_pointer on {:?}", other),
            }
        }
        I::AssertValueCell { target } => {
            let v = e.env.get(target).cloned().unwrap();
            let K::Ptr(c) = v else { bail!("assert_value_cell on {:?}", v) };
            match e.cells.get(&c) {
                Some(CellT::Val(_)) => None,
                other => bail!("assert_value_cell: cell {} is {:?}", c, other),
            }
        }
        I::Call { closure, args } => {
            let cv = e.env.get(closure).cloned().unwrap();
            let K::Ptr(c) = cv else { bail!("call of {:?}", cv) };
            let name = match e.cells.get(&c) {
                Some(CellT::Bi(name)) => name.clone(),
                other => bail!("call of non-builtin cell {:?}", other),
            };
            Some(call_intrinsic(e, &name, args)?)
        }
        I::CallBuiltin { name, args, .. } => Some(call_pure(e, name, args)?),
        I::Expand { value } => {
            let v = e.env.get(value).cloned().unwrap();
            match v {
                K::UBool { btn: Some(k) } => Some(K::SB(format!("kb{}", k))),
                K::UBool { btn: None } => bail!("expand of UBool without button provenance"),
                K::BoolC(_) | K::SB(_) | K::ZB(_) => Some(v),
                other => bail!("expand of {:?}", other),
            }
        }
        I::Kill { .. } => None,
        I::Phi { .. } => bail!("phi in the straightened program"),
    })
}

fn unop(e: &mut Emit, op: UnaryOp, a: &K) -> Result<K> {
    Ok(match (op, a) {
        (UnaryOp::Not, K::BoolC(b)) => K::BoolC(!b),
        (UnaryOp::Not, K::SB(v)) => K::SB(e.bind("bool", &format!("!{}", v))),
        (UnaryOp::Not, K::STri(v)) => K::STri(e.bind("Option<bool>", &format!("{}.map(|b| !b)", v))),
        (UnaryOp::Not, K::ZB(v)) => K::ZB(e.bind("ZB", &format!("zb_not({})", v))),
        (UnaryOp::Not, K::UBool { .. }) => K::UBool { btn: None },
        (UnaryOp::Minus, K::NumC(c)) => K::NumC(-*c),
        (UnaryOp::Minus, K::SN(v)) => K::SN(e.bind("P8", &format!("-{}", v))),
        (UnaryOp::Minus, K::ZN(v)) => K::ZN(e.bind("ZN", &format!("zn_neg({})", v))),
        (UnaryOp::Minus, K::SI(v)) => {
            K::SI(e.bind("(P8, P8)", &format!("(-{v}.1, -{v}.0)", v = v)))
        }
        (UnaryOp::Minus, K::ZI(v)) => K::ZI(e.bind("ZI", &format!("zi_neg({})", v))),
        (UnaryOp::Hash, K::Ptr(c)) => match e.cells.get(c) {
            Some(CellT::Arr(items)) => K::NumC(P8::from_i16(items.len() as i16)),
            other => bail!("# on {:?}", other),
        },
        other => bail!("unop {:?}", other),
    })
}

fn binop(e: &mut Emit, op: BinaryOp, l: &K, r: &K) -> Result<K> {
    use BinaryOp as B;
    // Constant folds first.
    if let (K::NumC(a), K::NumC(b)) = (l, r) {
        return Ok(match op {
            B::Plus => K::NumC(*a + *b),
            B::Minus => K::NumC(*a - *b),
            B::Star => K::NumC(*a * *b),
            B::Slash => K::NumC(*a / *b),
            B::Percent => K::NumC(*a % *b),
            B::TwoEqual => K::BoolC(a == b),
            B::TildeEqual => K::BoolC(a != b),
            B::LessThan => K::BoolC(a < b),
            B::LessThanEqual => K::BoolC(a <= b),
            B::GreaterThan => K::BoolC(a > b),
            B::GreaterThanEqual => K::BoolC(a >= b),
            other => bail!("const binop {:?}", other),
        });
    }
    match op {
        B::Plus | B::Minus => arith_addsub(e, op == B::Minus, l, r),
        B::Star => arith_mul(e, l, r),
        B::Slash => arith_div(e, l, r),
        B::Percent => {
            // nums only (av_rem)
            if Emit::is_z(l) || Emit::is_z(r) {
                let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
                Ok(K::ZN(e.bind("ZN", &format!("zn_rem({}, {})", a, b))))
            } else {
                let (a, b) = (sn(e, l)?, sn(e, r)?);
                Ok(K::SN(e.bind("P8", &format!("{} % {}", a, b))))
            }
        }
        B::TwoEqual => eq(e, l, r),
        B::TildeEqual => {
            let v = eq(e, l, r)?;
            unop(e, UnaryOp::Not, &v)
        }
        B::LessThan | B::LessThanEqual | B::GreaterThan | B::GreaterThanEqual => cmp(e, op, l, r),
        other => bail!("binop {:?} on ({:?}, {:?})", other, l, r),
    }
}

/// Scalar num expression of a NumC/SN.
fn sn(e: &mut Emit, k: &K) -> Result<String> {
    Ok(match k {
        K::SN(v) => v.clone(),
        K::NumC(c) => e.bind("P8", &p8(c)),
        other => bail!("sn on {:?}", other),
    })
}

/// Known value + provenance of a scalar num K, when the emitter can prove
/// one (constants, or stable uniforms and their arithmetic).
fn known_of(e: &Emit, k: &K) -> Option<(P8, BTreeSet<u32>)> {
    match k {
        K::NumC(c) => Some((*c, BTreeSet::new())),
        K::SN(v) => e.pin_val.get(v).cloned(),
        _ => None,
    }
}

fn arith_addsub(e: &mut Emit, sub: bool, l: &K, r: &K) -> Result<K> {
    let ival = Emit::is_ival(l) || Emit::is_ival(r);
    let z = Emit::is_z(l) || Emit::is_z(r);
    Ok(match (ival, z) {
        (false, false) => {
            let known = match (known_of(e, l), known_of(e, r)) {
                (Some((a, ca)), Some((b, cb))) => {
                    let mut cells = ca;
                    cells.extend(cb);
                    Some((if sub { a - b } else { a + b }, cells))
                }
                _ => None,
            };
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            let var = e.bind("P8", &format!("{} {} {}", a, if sub { "-" } else { "+" }, b));
            if let Some(k) = known {
                e.pin_val.insert(var.clone(), k);
            }
            K::SN(var)
        }
        (false, true) => {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            K::ZN(e.bind(
                "ZN",
                &format!("{}({}, {})", if sub { "zn_sub" } else { "zn_add" }, a, b),
            ))
        }
        (true, false) => {
            let (a, b) = (e.as_si(l)?, e.as_si(r)?);
            K::SI(e.bind(
                "(P8, P8)",
                &format!("{}({}, {})", if sub { "si_sub" } else { "si_add" }, a, b),
            ))
        }
        (true, true) => {
            let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
            K::ZI(e.bind(
                "ZI",
                &format!("{}({}, {})", if sub { "zi_sub" } else { "zi_add" }, a, b),
            ))
        }
    })
}

fn arith_mul(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    if !Emit::is_ival(l) && !Emit::is_ival(r) {
        return if Emit::is_z(l) || Emit::is_z(r) {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            Ok(K::ZN(e.bind("ZN", &format!("zn_mul({}, {})", a, b))))
        } else {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            Ok(K::SN(e.bind("P8", &format!("{} * {}", a, b))))
        };
    }
    // interval * positive number (av_mul's only interval arm)
    let (iv, num) = if Emit::is_ival(l) { (l, r) } else { (r, l) };
    if !Emit::is_num(num) {
        bail!("* on ({:?}, {:?}): no interpreter arm", l, r);
    }
    if Emit::is_z(iv) || Emit::is_z(num) {
        let (a, b) = (e.as_zi(iv)?, e.as_zn(num)?);
        Ok(K::ZI(e.bind("ZI", &format!("zi_mul_pos({}, {}, &mut dp)", a, b))))
    } else {
        let a = e.as_si(iv)?;
        let b = sn(e, num)?;
        e.line(&format!("if {} <= P8::from_i16(0) {{ *bd = true; }}", b));
        Ok(K::SI(e.bind(
            "(P8, P8)",
            &format!(
                "{{ let r = IV::new({a}.0, {a}.1).scale_positive({b}); (r.low, r.high) }}",
                a = a,
                b = b
            ),
        )))
    }
}

fn arith_div(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    if !Emit::is_ival(l) && !Emit::is_ival(r) {
        return if Emit::is_z(l) || Emit::is_z(r) {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            Ok(K::ZN(e.bind("ZN", &format!("zn_div({}, {})", a, b))))
        } else {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            Ok(K::SN(e.bind("P8", &format!("{} / {}", a, b))))
        };
    }
    if !Emit::is_ival(l) || !Emit::is_num(r) {
        bail!("/ on ({:?}, {:?}): no interpreter arm", l, r);
    }
    if Emit::is_z(l) || Emit::is_z(r) {
        let (a, b) = (e.as_zi(l)?, e.as_zn(r)?);
        Ok(K::ZI(e.bind("ZI", &format!("zi_div_pos({}, {}, &mut dp)", a, b))))
    } else {
        let a = e.as_si(l)?;
        let b = sn(e, r)?;
        e.line(&format!("if {} <= P8::from_i16(0) {{ *bd = true; }}", b));
        Ok(K::SI(e.bind(
            "(P8, P8)",
            &format!(
                "{{ let r = IV::new({a}.0, {a}.1).div_positive({b}); (r.low, r.high) }}",
                a = a,
                b = b
            ),
        )))
    }
}

/// av_eq, kind-directed.
fn eq(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    use K::*;
    Ok(match (l, r) {
        // ZIP: point lanes are the NUMBER `at`; interval lanes are never equal.
        (ZIP { pt, at, .. }, NumC(c)) | (NumC(c), ZIP { pt, at, .. }) => {
            if c == at {
                K::ZB(e.bind("ZB", &format!("ZB {{ val: {}, known: ALL }}", pt)))
            } else {
                K::BoolC(false)
            }
        }
        (ZIP { .. }, _) | (_, ZIP { .. }) => K::BoolC(false),
        (SI(_) | ZI(_), _) | (_, SI(_) | ZI(_)) => K::BoolC(false),
        (NumC(_) | SN(_) | ZN(_), NumC(_) | SN(_) | ZN(_)) => {
            if Emit::is_z(l) || Emit::is_z(r) {
                let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
                K::ZB(e.bind("ZB", &format!("zn_eq({}, {})", a, b)))
            } else {
                let (a, b) = (sn(e, l)?, sn(e, r)?);
                K::SB(e.bind("bool", &format!("{} == {}", a, b)))
            }
        }
        (NumC(_) | SN(_) | ZN(_), _) | (_, NumC(_) | SN(_) | ZN(_)) => K::BoolC(false),
        (BoolC(a), BoolC(b)) => K::BoolC(a == b),
        (BoolC(_) | SB(_) | ZB(_), UBool { .. }) | (UBool { .. }, BoolC(_) | SB(_) | ZB(_)) => {
            K::UBool { btn: None }
        }
        (UBool { .. }, UBool { .. }) => K::UBool { btn: None },
        (SB(_) | BoolC(_) | ZB(_), SB(_) | BoolC(_) | ZB(_)) => {
            if Emit::is_z(l) || Emit::is_z(r) {
                let (a, b) = (e.as_zb(l)?, e.as_zb(r)?);
                K::ZB(e.bind("ZB", &format!("zb_eq({}, {})", a, b)))
            } else {
                let a = sb(e, l)?;
                let b = sb(e, r)?;
                K::SB(e.bind("bool", &format!("{} == {}", a, b)))
            }
        }
        (BoolC(_) | SB(_) | ZB(_) | UBool { .. }, _) | (_, BoolC(_) | SB(_) | ZB(_) | UBool { .. }) => {
            K::BoolC(false)
        }
        (Nil, Nil) => K::BoolC(true),
        (Nil, _) | (_, Nil) => K::BoolC(false),
        (Ptr(a), Ptr(b)) => K::BoolC(a == b),
        (Ptr(_), _) | (_, Ptr(_)) => K::BoolC(false),
        (NilPtr, _) | (_, NilPtr) => bail!("== on a nil pointer"),
        other => bail!("== on {:?}", other),
    })
}

fn sb(e: &mut Emit, k: &K) -> Result<String> {
    Ok(match k {
        K::SB(v) => v.clone(),
        K::BoolC(b) => e.bind("bool", &format!("{}", b)),
        other => bail!("sb on {:?}", other),
    })
}

fn cmp(e: &mut Emit, op: BinaryOp, l: &K, r: &K) -> Result<K> {
    use BinaryOp as B;
    let (zop, sop, jop) = match op {
        B::LessThan => ("zn_lt", "<", "Lt"),
        B::LessThanEqual => ("zn_le", "<=", "Le"),
        B::GreaterThan => ("zn_gt", ">", "Gt"),
        B::GreaterThanEqual => ("zn_ge", ">=", "Ge"),
        _ => unreachable!(),
    };
    let ival = Emit::is_ival(l) || Emit::is_ival(r);
    let z = Emit::is_z(l) || Emit::is_z(r);
    Ok(match (ival, z) {
        (false, false) => {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            K::SB(e.bind("bool", &format!("{} {} {}", a, sop, b)))
        }
        (false, true) => {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            K::ZB(e.bind("ZB", &format!("{}({}, {})", zop, a, b)))
        }
        (true, true) => {
            let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
            K::ZB(e.bind("ZB", &format!("zi_cmp(Cmp::{}, {}, {})", jop, a, b)))
        }
        (true, false) => {
            let (a, b) = (e.as_si(l)?, e.as_si(r)?);
            K::STri(e.bind(
                "Option<bool>",
                &format!("si_cmp(Cmp::{}, {}, {})", jop, a, b),
            ))
        }
    })
}

fn select(e: &mut Emit, c: &K, t: &K, f: &K) -> Result<K> {
    match c {
        K::BoolC(b) => return Ok(if *b { t.clone() } else { f.clone() }),
        _ => {}
    }
    // Unify the sides' class.
    let z = Emit::is_z(t) || Emit::is_z(f) || matches!(c, K::ZB(_));
    let ival = Emit::is_ival(t) || Emit::is_ival(f);
    match c {
        K::SB(cv) => Ok(if z {
            if ival {
                let (a, b) = (e.as_zi(t)?, e.as_zi(f)?);
                K::ZI(e.bind("ZI", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
            } else if matches!(t, K::ZB(_) | K::BoolC(_) | K::SB(_))
                && matches!(f, K::ZB(_) | K::BoolC(_) | K::SB(_))
                && (matches!(t, K::ZB(_)) || matches!(f, K::ZB(_)))
            {
                let (a, b) = (e.as_zb(t)?, e.as_zb(f)?);
                K::ZB(e.bind("ZB", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
            } else {
                let (a, b) = (e.as_zn(t)?, e.as_zn(f)?);
                K::ZN(e.bind("ZN", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
            }
        } else {
            // scalar sides
            match (t, f) {
                (K::SN(_) | K::NumC(_), K::SN(_) | K::NumC(_)) => {
                    let (a, b) = (sn(e, t)?, sn(e, f)?);
                    K::SN(e.bind("P8", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
                }
                (K::SB(_) | K::BoolC(_), K::SB(_) | K::BoolC(_)) => {
                    let (a, b) = (sb(e, t)?, sb(e, f)?);
                    K::SB(e.bind("bool", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
                }
                (K::SI(_) | K::SN(_) | K::NumC(_), K::SI(_) | K::SN(_) | K::NumC(_)) => {
                    let (a, b) = (e.as_si(t)?, e.as_si(f)?);
                    K::SI(e.bind("(P8, P8)", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b)))
                }
                _ => bail!("select scalar sides {:?} / {:?}", t, f),
            }
        }),
        K::ZB(cv) => Ok(if ival {
            let (a, b) = (e.as_zi(t)?, e.as_zi(f)?);
            K::ZI(e.bind("ZI", &format!("zsel_i({}, {}, {}, &mut dp)", cv, a, b)))
        } else if matches!(t, K::ZB(_) | K::BoolC(_) | K::SB(_))
            && matches!(f, K::ZB(_) | K::BoolC(_) | K::SB(_))
        {
            let (a, b) = (e.as_zb(t)?, e.as_zb(f)?);
            K::ZB(e.bind("ZB", &format!("zsel_b({}, {}, {}, &mut dp)", cv, a, b)))
        } else {
            let (a, b) = (e.as_zn(t)?, e.as_zn(f)?);
            K::ZN(e.bind("ZN", &format!("zsel_n({}, {}, {}, &mut dp)", cv, a, b)))
        }),
        K::STri(cv) => {
            // Uniform tri-state: unknown means the whole slice deopts.
            e.line(&format!("if {}.is_none() {{ *bd = true; }}", cv));
            let cb = e.bind("bool", &format!("{}.unwrap_or(false)", cv));
            select(e, &K::SB(cb), t, f)
        }
        K::UBool { .. } => {
            e.line("*bd = true; // select on uniform UnknownBool");
            Ok(t.clone())
        }
        other => bail!("select on condition {:?}", other),
    }
}

fn call_intrinsic(e: &mut Emit, name: &str, args: &[LocalId]) -> Result<K> {
    let vals: Vec<K> = args.iter().map(|a| e.env.get(a).cloned().unwrap()).collect();
    match name {
        "__new_unknown_boolean" => Ok(K::UBool { btn: None }),
        "__split_by_flr" => {
            let a = &vals[0];
            match a {
                K::ZI(v) | K::ZIP { v, .. } => {
                    // A <=2-way FORK: open a runtime loop; everything after
                    // this instruction nests inside it. dp/bd are shadowed
                    // so one configuration's failures do not leak into the
                    // next.
                    let v = v.clone();
                    if e.cur_tainted {
                        bail!("fork on a button-dependent value is not supported (v1)");
                    }
                    let d = e.fork_depth;
                    e.line(&format!("for c{} in 0..2usize {{", d));
                    e.line("let mut dp = dp;");
                    e.line("let mut bd_l: bool = *bd;");
                    e.line("let bd: &mut bool = &mut bd_l;");
                    let frag = format!("v{}", e.n);
                    e.n += 1;
                    e.line(&format!(
                        "let ({f}, {f}_fv): (ZI, u16) = zi_fork_flr({v}, c{d}, &mut dp);",
                        f = frag,
                        v = v,
                        d = d
                    ));
                    let valid = format!("valid{}", d);
                    e.line(&format!(
                        "let {}: u16 = {} & {}_fv;",
                        valid, e.valid_expr, frag
                    ));
                    e.line(&format!("if {} == 0 {{ continue; }}", valid));
                    e.var_ty.insert(frag.clone(), "ZI");
                    e.var_ty.insert(valid.clone(), "u16");
                    e.pre_defs.insert(frag.clone());
                    e.pre_defs.insert(valid.clone());
                    e.valid_expr = valid;
                    e.fork_depth += 1;
                    Ok(K::ZI(frag))
                }
                K::SI(v) => {
                    e.line(&format!(
                        "if {v}.0.flr() != {v}.1.flr() {{ *bd = true; }}",
                        v = v
                    ));
                    Ok(a.clone())
                }
                K::ZN(_) | K::SN(_) | K::NumC(_) => Ok(a.clone()),
                other => bail!("__split_by_flr on {:?}", other),
            }
        }
        "__split_at" => {
            let at = match &vals[1] {
                K::NumC(c) => *c,
                other => bail!("__split_at with non-constant threshold {:?}", other),
            };
            match &vals[0] {
                K::ZI(v) => {
                    let name = format!("v{}", e.n);
                    e.n += 1;
                    e.line(&format!(
                        "let ({}, {}_pt): (ZI, u16) = zi_split_at({}, {}, &mut dp);",
                        name,
                        name,
                        v,
                        p8(&at)
                    ));
                    e.var_ty.insert(name.clone(), "ZI");
                    e.var_ty.insert(format!("{}_pt", name), "u16");
                    if e.cur_tainted {
                        e.tainted_vars.insert(name.clone());
                        e.tainted_vars.insert(format!("{}_pt", name));
                    } else {
                        e.pre_defs.insert(name.clone());
                        e.pre_defs.insert(format!("{}_pt", name));
                    }
                    Ok(K::ZIP { v: name.clone(), pt: format!("{}_pt", name), at })
                }
                K::ZN(_) | K::SN(_) | K::NumC(_) => Ok(vals[0].clone()),
                other => bail!("__split_at on {:?}", other),
            }
        }
        other => bail!("call of builtin {:?} (not a kernel intrinsic)", other),
    }
}

fn call_pure(e: &mut Emit, name: &str, args: &[LocalId]) -> Result<K> {
    let vals: Vec<K> = args.iter().map(|a| e.env.get(a).cloned().unwrap()).collect();
    match name {
        "flr" => match &vals[0] {
            K::NumC(c) => Ok(K::NumC(c.flr())),
            K::SN(v) => Ok(K::SN(e.bind("P8", &format!("{}.flr()", v)))),
            K::ZN(v) => Ok(K::ZN(e.bind("ZN", &format!("zn_flr({})", v)))),
            K::ZI(v) => Ok(K::ZN(e.bind("ZN", &format!("zi_flr({}, &mut dp)", v)))),
            K::ZIP { v, .. } => Ok(K::ZN(e.bind("ZN", &format!("zi_flr({}, &mut dp)", v)))),
            K::SI(v) => {
                e.line(&format!("if {v}.0.flr() != {v}.1.flr() {{ *bd = true; }}", v = v));
                Ok(K::SN(e.bind("P8", &format!("{}.0.flr()", v))))
            }
            other => bail!("flr on {:?}", other),
        },
        "abs" => match &vals[0] {
            K::NumC(c) => Ok(K::NumC(c.abs())),
            K::SN(v) => Ok(K::SN(e.bind("P8", &format!("{}.abs()", v)))),
            K::ZN(v) => Ok(K::ZN(e.bind("ZN", &format!("zn_abs({})", v)))),
            K::ZI(v) | K::ZIP { v, .. } => Ok(K::ZI(e.bind("ZI", &format!("zi_abs({})", v)))),
            other => bail!("abs on {:?}", other),
        },
        "min" | "max" => {
            let is_min = name == "min";
            let (l, r) = (&vals[0], &vals[1]);
            if let (K::NumC(a), K::NumC(b)) = (l, r) {
                return Ok(K::NumC(if is_min { (*a).min(*b) } else { (*a).max(*b) }));
            }
            let ival = Emit::is_ival(l) || Emit::is_ival(r);
            let z = Emit::is_z(l) || Emit::is_z(r);
            Ok(match (ival, z) {
                (false, false) => {
                    let (a, b) = (sn(e, l)?, sn(e, r)?);
                    K::SN(e.bind(
                        "P8",
                        &format!("{}.{}({})", a, if is_min { "min" } else { "max" }, b),
                    ))
                }
                (false, true) => {
                    let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
                    K::ZN(e.bind(
                        "ZN",
                        &format!("{}({}, {})", if is_min { "zn_min" } else { "zn_max" }, a, b),
                    ))
                }
                (true, true) => {
                    let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
                    K::ZI(e.bind(
                        "ZI",
                        &format!("{}({}, {})", if is_min { "zi_min" } else { "zi_max" }, a, b),
                    ))
                }
                (true, false) => {
                    let (a, b) = (e.as_si(l)?, e.as_si(r)?);
                    let f = if is_min { "min" } else { "max" };
                    K::SI(e.bind(
                        "(P8, P8)",
                        &format!("({a}.0.{f}({b}.0), {a}.1.{f}({b}.1))", a = a, b = b, f = f),
                    ))
                }
            })
        }
        "mget" => {
            let (x, y) = (&vals[0], &vals[1]);
            if Emit::is_z(x) || Emit::is_z(y) {
                let (a, b) = (e.as_zn(x)?, e.as_zn(y)?);
                Ok(K::ZN(e.bind("ZN", &format!("zn_mget(g.cart, {}, {})", a, b))))
            } else {
                let (a, b) = (sn(e, x)?, sn(e, y)?);
                Ok(K::SN(e.bind(
                    "P8",
                    &format!(
                        "P8::from_i16(g.cart.mget({}, {}).expect(\"mget\") as i16)",
                        a, b
                    ),
                )))
            }
        }
        "tile_flag_at" => {
            let (x, y, w, h, f) = (&vals[0], &vals[1], &vals[2], &vals[3], &vals[4]);
            let (wv, hv, fv) = (sn(e, w)?, sn(e, h)?, sn(e, f)?);
            if Emit::is_z(x) || Emit::is_z(y) {
                let (a, b) = (e.as_zn(x)?, e.as_zn(y)?);
                Ok(K::ZB(e.bind(
                    "ZB",
                    &format!(
                        "zn_tile_flag_at(g.cache, g.cart, {}, {}, {}, {}, {})",
                        a, b, wv, hv, fv
                    ),
                )))
            } else {
                let (a, b) = (sn(e, x)?, sn(e, y)?);
                Ok(K::SB(e.bind(
                    "bool",
                    &format!(
                        "{{ let z = zn_tile_flag_at(g.cache, g.cart, zn_splat({}), zn_splat({}), {}, {}, {}); z.val & 1 != 0 }}",
                        a, b, wv, hv, fv
                    ),
                )))
            }
        }
        "sin" => match &vals[0] {
            K::SN(v) => Ok(K::SN(e.bind("P8", &format!("{}.pico8_sin()", v)))),
            K::NumC(c) => Ok(K::NumC(c.pico8_sin())),
            K::ZN(v) => Ok(K::ZN(e.bind("ZN", &format!("zn_sin({})", v)))),
            K::SI(_) | K::ZI(_) | K::ZIP { .. } => Ok(K::SI(e.bind(
                "(P8, P8)",
                "(P8::from_i16(-1), P8::from_i16(1))",
            ))),
            other => bail!("sin on {:?}", other),
        },
        other => bail!("call_builtin {:?} not implemented in the kernel", other),
    }
}

/// Assemble kernel_gen.rs.
fn render(e: &Emit, out_path: &str) -> Result<()> {
    let mut out = String::new();
    writeln!(
        out,
        "// GENERATED by `transpile --kernel` (plans/kernel-plan.md). Do not edit.\n\
         //\n\
         // The STEADY-CLASS lane kernel: shape (player), pm1 freeze=0 dash_time=0.\n\
         // Shared prefix per 16-row slice, 64 monomorphized button suffixes.\n\
         #![allow(unused_variables, unused_mut, clippy::all)]\n\
         use crate::kernel::*;\n\
         use celeste_rust::pico8_num::{{Pico8Num as P8, Pico8NumInterval as IV}};\n\
         use celeste_rust::cart_data::CartData;\n\
         use celeste_rust::collision_cache::CollisionCache;\n"
    )?;
    writeln!(out, "pub const SHAPE_HASH: u64 = 0x{};", e.shape_hash)?;
    writeln!(
        out,
        "pub struct G<'a> {{ pub cart: &'a CartData, pub cache: &'a CollisionCache }}\n"
    )?;

    // Uniform binding struct.
    writeln!(out, "/// Block-uniform inputs, bound per block (kind-checked).")?;
    writeln!(out, "pub struct Uni {{")?;
    for (id, kind) in &e.uni {
        let ty = match *kind {
            "num" => "P8",
            "ival" => "(P8, P8)",
            "bool" => "bool",
            _ => unreachable!(),
        };
        writeln!(out, "    pub c{}: {},", id, ty)?;
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "pub const UNI_CELLS: &[(u32, &str)] = &[")?;
    for (id, kind) in &e.uni {
        writeln!(out, "    ({}, \"{}\"),", id, kind)?;
    }
    writeln!(out, "];\n")?;
    writeln!(
        out,
        "/// Folds consumed these uniform cells' witness values; bind must\n\
         /// guard them (raw bits) or send the block to the interpreter."
    )?;
    writeln!(out, "pub const PIN_CELLS: &[(u32, i32)] = &[")?;
    for id in &e.pins {
        let v = e.stable.get(id).expect("pin without stable value");
        writeln!(out, "    ({}, {}i32),", id, v.as_raw_u32() as i32)?;
    }
    writeln!(out, "];\n")?;

    // Row input struct.
    writeln!(out, "/// Per-lane inputs: 16 rows per slice.")?;
    writeln!(out, "pub struct RowsIn {{")?;
    for (id, kind) in &e.vary_in {
        let ty = if *kind == "num" { "ZN" } else { "u16" };
        writeln!(out, "    pub c{}: {},", id, ty)?;
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "pub const VARY_CELLS: &[(u32, &str)] = &[")?;
    for (id, kind) in &e.vary_in {
        writeln!(out, "    ({}, \"{}\"),", id, kind)?;
    }
    writeln!(out, "];\n")?;

    // Output struct: dirty original cells (scratch cells never escape).
    // (cell, rust ty, expr, tainted): tainted outputs differ per button
    // variant; untainted ones are identical across all 64 and are
    // reported once per fork config in KOutShared.
    let mut out_fields: Vec<(u32, &'static str, String, bool)> = Vec::new();
    let mut out_ubool: Vec<u32> = Vec::new();
    for id in &e.dirty {
        // Scratch cells (allocated during the frame) are region-private.
        if *id >= e.witness_len {
            continue;
        }
        // Button cells end the frame holding NEXT frame's fresh unknowns -
        // a constant fact, not per-lane data.
        if let Some(CellT::Val(K::UBool { .. })) = e.cells.get(id) {
            out_ubool.push(*id);
            continue;
        }
        let Some(CellT::Val(k)) = e.cells.get(id) else { continue };
        let (ty, expr) = match k {
            K::ZN(v) => ("ZN", v.clone()),
            K::ZI(v) => ("ZI", v.clone()),
            K::ZIP { v, .. } => ("ZI", v.clone()),
            K::ZB(v) => ("ZB", v.clone()),
            K::SN(v) => ("P8", v.clone()),
            K::SI(v) => ("(P8, P8)", v.clone()),
            K::SB(v) => ("bool", v.clone()),
            K::NumC(c) => ("P8", p8(c)),
            K::BoolC(b) => ("bool", format!("{}", b)),
            K::Nil => continue, // a cell reset to nil is dropped from rows
            other => bail!("output cell {} holds {:?}", id, other),
        };
        let tainted = e
            .cells
            .get(id)
            .map(|c| matches!(c, CellT::Val(k) if e.k_tainted(k)))
            .unwrap_or(false);
        out_fields.push((*id, ty, expr, tainted));
    }
    writeln!(out, "/// Button-independent outputs: one per fork config.")?;
    writeln!(out, "pub struct KOutShared {{")?;
    for (id, ty, _, tainted) in &out_fields {
        if !*tainted {
            writeln!(out, "    pub c{}: {},", id, ty)?;
        }
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "/// Button-dependent outputs: one per (config, variant).")?;
    writeln!(out, "pub struct KOut {{")?;
    writeln!(out, "    /// Lanes that EXIST in this fork configuration.")?;
    writeln!(out, "    pub valid: u16,")?;
    writeln!(out, "    pub deopt: u16,")?;
    writeln!(out, "    pub bd: bool,")?;
    for (id, ty, _, tainted) in &out_fields {
        if *tainted {
            writeln!(out, "    pub c{}: {},", id, ty)?;
        }
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "pub const OUT_CELLS: &[u32] = &[")?;
    for (id, _, _, _) in &out_fields {
        writeln!(out, "    {},", id)?;
    }
    writeln!(out, "];\n")?;
    writeln!(
        out,
        "/// Cells that end every frame as a fresh UnknownBool (the next\n\
         /// frame's button inputs); the boundary writes UBool, no data."
    )?;
    writeln!(out, "pub const OUT_UBOOL_CELLS: &[u32] = &[")?;
    for id in &out_ubool {
        writeln!(out, "    {},", id)?;
    }
    writeln!(out, "];\n")?;

    // Runtime glue: bind uniforms from a block (kind + pin checked),
    // gather a 16-row slice, and apply an output back onto a sliced block.
    writeln!(
        out,
        "use crate::runtime2::{{Rt2, Col, AV}};\n\n\
         /// Bind the block-uniform inputs. None = off-shape / off-kind /\n\
         /// pin mismatch: the block takes the reference path.\n\
         pub fn bind(b: &Rt2) -> Option<Uni> {{\n\
         \x20   if b.shape_hash != SHAPE_HASH {{ return None; }}\n\
         \x20   for (cell, pin) in PIN_CELLS {{\n\
         \x20       match &b.cols[*cell as usize] {{\n\
         \x20           Col::U(AV::Num(n)) if n.as_raw_u32() as i32 == *pin => {{}}\n\
         \x20           _ => return None,\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   Some(Uni {{"
    )?;
    for (id, kind) in &e.uni {
        match *kind {
            "num" => writeln!(
                out,
                "        c{id}: match &b.cols[{id}] {{ Col::U(AV::Num(n)) => *n, _ => return None }},",
                id = id
            )?,
            "ival" => writeln!(
                out,
                "        c{id}: match &b.cols[{id}] {{ Col::U(AV::Ival(a, b)) => (*a, *b), _ => return None }},",
                id = id
            )?,
            "bool" => writeln!(
                out,
                "        c{id}: match &b.cols[{id}] {{ Col::U(AV::Bool(v)) => *v, _ => return None }},",
                id = id
            )?,
            _ => unreachable!(),
        }
    }
    writeln!(out, "    }})\n}}\n")?;

    writeln!(
        out,
        "/// Gather rows [lo, lo+16) into lane arrays; short slices pad by\n\
         /// repeating the last row (padded outputs are ignored by width).\n\
         pub fn rows(b: &Rt2, lo: usize) -> Option<RowsIn> {{\n\
         \x20   let at = |i: usize| -> usize {{ (lo + i).min(b.width - 1) }};\n\
         \x20   Some(RowsIn {{"
    )?;
    for (id, kind) in &e.vary_in {
        match *kind {
            "num" => writeln!(
                out,
                "        c{id}: match &b.cols[{id}] {{\n\
                 \x20           Col::N(v) => core::array::from_fn(|i| v[at(i)]),\n\
                 \x20           Col::U(AV::Num(n)) => [*n; W],\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                id = id
            )?,
            "bool" => writeln!(
                out,
                "        c{id}: match &b.cols[{id}] {{\n\
                 \x20           Col::V(v) => {{\n\
                 \x20               let mut m = 0u16;\n\
                 \x20               for i in 0..W {{ if matches!(v[at(i)], AV::Bool(true)) {{ m |= 1 << i; }} \
                 else if !matches!(v[at(i)], AV::Bool(false)) {{ return None; }} }}\n\
                 \x20               m\n\
                 \x20           }}\n\
                 \x20           Col::U(AV::Bool(t)) => if *t {{ 0xffff }} else {{ 0 }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                id = id
            )?,
            _ => unreachable!(),
        }
    }
    writeln!(out, "    }})\n}}\n")?;

    writeln!(
        out,
        "/// Write one (config, variant) result onto a width-`n` sliced block\n\
         /// (the block already carries the input row values).\n\
         pub fn apply(sh: &KOutShared, kv: &KOut, b: &mut Rt2, n: usize) {{"
    )?;
    for (id, ty, _, tainted) in &out_fields {
        let src = if *tainted { "kv" } else { "sh" };
        match *ty {
            "ZN" => writeln!(
                out,
                "    b.cols[{id}] = Col::N({src}.c{id}[..n].to_vec());",
                id = id, src = src
            )?,
            "ZI" => writeln!(
                out,
                "    b.cols[{id}] = Col::I((0..n).map(|i| ({src}.c{id}.lo[i], {src}.c{id}.hi[i])).collect());",
                id = id, src = src
            )?,
            "ZB" => writeln!(
                out,
                "    b.cols[{id}] = Col::V((0..n).map(|i| AV::Bool({src}.c{id}.val & (1 << i) != 0)).collect());",
                id = id, src = src
            )?,
            "P8" => writeln!(out, "    b.cols[{id}] = Col::U(AV::Num({src}.c{id}));", id = id, src = src)?,
            "bool" => {
                writeln!(out, "    b.cols[{id}] = Col::U(AV::Bool({src}.c{id}));", id = id, src = src)?
            }
            "(P8, P8)" => writeln!(
                out,
                "    b.cols[{id}] = Col::U(AV::Ival({src}.c{id}.0, {src}.c{id}.1));",
                id = id, src = src
            )?,
            other => bail!("apply: type {}", other),
        }
    }
    writeln!(
        out,
        "    for cell in OUT_UBOOL_CELLS {{\n\
         \x20       b.cols[*cell as usize] = Col::U(AV::UBool);\n\
         \x20   }}\n\
         }}\n"
    )?;

    // Pre struct: prefix values the suffix reads (word-boundary search,
    // so v1 does not match inside v17).
    let word_used = |text: &str, name: &str| -> bool {
        let bytes = text.as_bytes();
        let mut from = 0;
        while let Some(pos) = text[from..].find(name) {
            let start = from + pos;
            let end = start + name.len();
            let pre_ok = start == 0
                || !(bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_');
            let post_ok =
                end >= bytes.len() || !(bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_');
            if pre_ok && post_ok {
                return true;
            }
            from = end;
        }
        false
    };
    let mut crossing: BTreeSet<String> = BTreeSet::new();
    for name in &e.pre_defs {
        if word_used(&e.suf, name) {
            crossing.insert(name.clone());
        }
    }
    // The out_fields' exprs referenced from the suffix epilogue also cross.
    for (_, _, expr, _) in &out_fields {
        if e.pre_defs.contains(expr) {
            crossing.insert(expr.clone());
        }
    }
    writeln!(out, "pub struct Pre {{")?;
    for name in &crossing {
        let ty = e
            .var_ty
            .get(name)
            .ok_or_else(|| anyhow!("no type for crossing var {}", name))?;
        writeln!(out, "    {}: {},", name, ty)?;
    }
    writeln!(out, "    valid: u16,")?;
    writeln!(out, "    dp: u16,")?;
    writeln!(out, "    bd: bool,")?;
    writeln!(out, "}}\n")?;

    // frame()
    writeln!(
        out,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u8, &KOutShared, &KOut)) {{\n\
         \x20   let mut dp: u16 = 0;\n\
         \x20   let mut bd_flag: bool = false;\n\
         \x20   let bd: &mut bool = &mut bd_flag;"
    )?;
    out.push_str(&e.pre);
    writeln!(out, "    let p = Pre {{")?;
    for name in &crossing {
        writeln!(out, "        {},", name)?;
    }
    writeln!(out, "        valid: {},", e.valid_expr)?;
    writeln!(out, "        dp,")?;
    writeln!(out, "        bd: *bd,")?;
    writeln!(out, "    }};")?;
    writeln!(out, "    let osh = KOutShared {{")?;
    for (id, _, expr, tainted) in &out_fields {
        if !*tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }};")?;
    for b in 0..64 {
        writeln!(out, "    suffix::<{}>(u, g, &p, &osh, out);", b)?;
    }
    for _ in 0..e.fork_depth {
        writeln!(out, "    }}")?;
    }
    writeln!(out, "}}\n")?;

    // suffix()
    writeln!(
        out,
        "#[inline(never)]\n\
         fn suffix<const B: u8>(u: &Uni, g: &G, p: &Pre, osh: &KOutShared, out: &mut impl FnMut(u8, &KOutShared, &KOut)) {{"
    )?;
    for name in &crossing {
        writeln!(out, "    let {} = p.{};", name, name)?;
    }
    writeln!(out, "    let mut dp: u16 = p.dp;")?;
    writeln!(out, "    let mut bd_flag: bool = p.bd;")?;
    writeln!(out, "    let bd: &mut bool = &mut bd_flag;")?;
    for k in 0..6 {
        writeln!(out, "    let kb{}: bool = (B >> {}) & 1 != 0;", k, k)?;
    }
    out.push_str(&e.suf);
    writeln!(out, "    out(B, osh, &KOut {{")?;
    writeln!(out, "        valid: p.valid,")?;
    writeln!(out, "        deopt: dp,")?;
    writeln!(out, "        bd: *bd,")?;
    for (id, _, expr, tainted) in &out_fields {
        if *tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }});")?;
    writeln!(out, "}}")?;

    std::fs::write(out_path, out)?;
    eprintln!(
        "kernel: {} uniform cells, {} row cells, {} out cells, {} crossing values, prefix {} lines, suffix {} lines -> {}",
        e.uni.len(),
        e.vary_in.len(),
        out_fields.len(),
        crossing.len(),
        e.pre.lines().count(),
        e.suf.lines().count(),
        out_path
    );
    Ok(())
}

