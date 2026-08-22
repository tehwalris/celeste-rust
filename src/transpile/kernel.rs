//! The KERNEL EMITTER (plans/kernel-plan.md K1).
//!
//! Emits `crates/celeste-kernels/src/kernel_gen_CLASS.rs`: fully-typed, straight-line,
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
use crate::ir::{BinaryOp, Instruction, LocalId, Terminator, UnaryOp};
use crate::pico8_num::Pico8Num as P8;
use crate::rewrite::print::blocks_in_order;
use super::graph::{Graph, NodeId, Op as GOp};
use crate::rewrite::program::Program;

/// Emit-time value. `S*` strings are generated VARIABLE NAMES (each IR
/// instruction result is let-bound), never raw expressions.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum K {
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
}

/// Emit-time cell content.
#[derive(Clone, Debug)]
pub(crate) enum CellT {
    Val(K),
    Obj(BTreeMap<String, u32>),
    Arr(Vec<u32>),
    Clo(String),
    Bi(String),
    Unk,
}

/// One line of an emitted kernel body - and, for `Let`, one NODE of the
/// member's pure expression graph (plans/shape-tag-plan.md step 2b). The
/// emitter used to stream text; keeping the bindings structured is what the
/// specialization-fusion pass consumes (value numbering over `Let` nodes),
/// while `render` reproduces the streamed text byte-for-byte for the
/// generated crates (`generated_is_current` is the gate on that).
#[derive(Clone, Debug)]
pub enum Line {
    /// A structural or effect statement: fork-loop headers, `zguard`/`*bd`
    /// guards, comments. Classified by the fusion pass by its (regular,
    /// machine-generated) text.
    Raw(String),
    /// `let name: ty = expr;` - a value node. `expr`'s arguments are
    /// whole-identifier variable names, witness field accesses (`u.cN`,
    /// `rin.cN`) or literals, so the graph edges recover by identifier scan.
    Let {
        name: String,
        ty: &'static str,
        expr: String,
    },
}

pub(crate) fn render_lines(lines: &[Line]) -> String {
    let mut out = String::new();
    for line in lines {
        match line {
            Line::Raw(s) => {
                out.push_str("    ");
                out.push_str(s);
                out.push('\n');
            }
            Line::Let { name, ty, expr } => {
                out.push_str(&format!("    let {}: {} = {};\n", name, ty, expr));
            }
        }
    }
    out
}

pub(crate) struct Emit {
    /// Number of witness cells (ids below this are boundary state; at or
    /// above are frame-local scratch).
    pub(crate) witness_len: u32,
    pub(crate) cells: HashMap<u32, CellT>,
    pub(crate) globals: HashMap<String, u32>,
    pub(crate) next_cell: u32,
    pub(crate) env: HashMap<LocalId, K>,
    /// Uniform cells bound at runtime: cell id -> kind ("num"|"ival"|"bool").
    pub(crate) uni: BTreeMap<u32, &'static str>,
    /// Varying input cells: id -> ("num"|"bool").
    pub(crate) vary_in: BTreeMap<u32, &'static str>,
    /// Cells written by a Store anywhere in the frame.
    pub(crate) dirty: BTreeSet<u32>,
    /// The emitted body, in one piece. There is no prefix/suffix split
    /// any more: `transpile::lower` eliminates the free choices by
    /// specializing the graph on all 64 assignments into one interned
    /// arena, so what two assignments share is one node rather than
    /// something the compiler has to rediscover per monomorphization.
    pub(crate) body: Vec<Line>,
    /// One entry per DISTINCT free assignment (36 of 64 on steady).
    pub(crate) variants: Vec<super::lower::Variant>,
    /// Assignment -> the representative it collapsed onto.
    pub(crate) rep_of: [u8; 64],
    /// The walk's own discarded line stream; see `Emit::buf`.
    pub(crate) scratch: Vec<Line>,
    pub(crate) n: usize,
    /// name -> rust type of every generated let (for the Pre struct).
    pub(crate) var_ty: HashMap<String, &'static str>,
    /// Names defined in the prefix (crossing detection).
    pub(crate) pre_defs: BTreeSet<String>,
    pub(crate) shape_hash: String,
    /// Cross-block-stable uniform num values from the witness. A fold
    /// that CONSUMES one records a pin: bind() then guards the cell's
    /// runtime value against it (mismatch = the block takes the
    /// interpreter path).
    pub(crate) stable: HashMap<u32, P8>,
    /// var name -> (known value, uniform cells it derives from).
    pub(crate) pin_val: HashMap<String, (P8, BTreeSet<u32>)>,
    /// Cells whose stable value a fold consumed (bind-time guards).
    pub(crate) pins: BTreeSet<u32>,
    /// __button_states cells: cell id -> button index 0..5. An UnknownBool
    /// loaded from one carries that provenance into `expand` even when it
    /// was freshly minted this frame (mint-store-reload strips the tag).
    pub(crate) button_cells: HashMap<u32, u8>,
    /// Open fork loops (each __split_by_flr on a per-lane interval is a
    /// <=2-way fork emitted as a runtime loop; the rest of the program
    /// nests inside). Render closes this many braces at the end.
    pub(crate) fork_depth: usize,
    /// Name of the current per-lane validity mask ("ALL" at depth 0).
    pub(crate) valid_expr: String,
    /// Free-choice TAINT tracking (cross-variant sharing): only
    /// instructions whose value depends on a button land in the x64
    /// suffix; everything
    /// else is hoisted to the shared per-fork-config prefix, which is
    /// sound because an untainted op only reads untainted defs (all in
    /// the prefix) and the emit-time SSA evaluation already captured the
    /// correct pre/post-store cell values.
    pub(crate) tainted_vars: BTreeSet<String>,
    pub(crate) tainted_cells: BTreeSet<u32>,
    /// Taint of the instruction currently being emitted (routes buffers).
    pub(crate) cur_tainted: bool,
    /// The member's value graph (plans/multi-output-fusion.md, P1'). Built
    /// alongside the emitted lines while the 71 bind sites migrate; every
    /// value not yet described structurally is an `Op::Unmigrated` leaf.
    pub(crate) graph: Graph,
    /// Emitted variable name -> its node in `graph`.
    pub(crate) node_of: HashMap<String, NodeId>,
    /// Does this lane BELONG to the fork configuration being emitted?
    /// The AND of every `ForkValid(d)`. A lane that is not live is not
    /// wrong - it is computed by a sibling configuration.
    pub(crate) live: NodeId,
    /// Did the kernel COMPUTE this lane correctly? The AND of every
    /// condition under which the abstract domain did not give up: guard
    /// conditions, "this select's condition is decided", "this interval's
    /// floor is unique", "this scalar is positive". Its negation is what
    /// the emitted code spells as `dp` (per-lane) or `*bd` (whole slice) -
    /// which of the two is DERIVED from whether the condition is uniform,
    /// so this one node replaces both side channels.
    pub(crate) ok: NodeId,
}

impl Emit {
    /// Where the WALK's own text goes - and it goes nowhere. `lower`
    /// rebuilds the body from the graph, so this stream is discarded; the
    /// walk still produces it only because `Graph::operand` recovers the
    /// graph's edges by parsing identifiers back out of the text it
    /// emits. It disappears when `K` carries a `NodeId` instead of a
    /// `String` (plans/tracing.md).
    fn buf(&mut self) -> &mut Vec<Line> {
        &mut self.scratch
    }
    fn line(&mut self, s: &str) {
        self.buf().push(Line::Raw(s.to_string()));
    }

    /// Let-bind `expr`, and record the VALUE it computes as `op(args)` in
    /// the member graph (`plans/multi-output-fusion.md`, P1'). `args` are
    /// operand spellings as they appear in `expr`: bound names, witness
    /// cells, or literals.
    fn bind_op(&mut self, ty: &'static str, expr: &str, op: GOp, args: &[&str]) -> String {
        let ids: Vec<_> = args
            .iter()
            .map(|a| {
                self.graph
                    .operand(a, &self.node_of)
                    .unwrap_or_else(|e| panic!("bind_op({:?}): {:#}", expr, e))
            })
            .collect();
        let node = self.graph.add(op, ids);
        self.bind_node(ty, expr, node)
    }

    /// Let-bind a value-preserving REPRESENTATION change: a splat
    /// (uniform -> per-lane) or a widening (exact -> singleton interval).
    /// The emitted line still happens; the graph does not gain a node,
    /// because in a one-lane graph these are the identity. The bound name
    /// simply aliases the operand's node.
    ///
    /// This is where 35% of the emitted kernel's nodes disappear, and with
    /// them a class of spurious divergence between members (20 of the 31
    /// divergence roots measured between the steady and pinned-dying
    /// members were splats and constants).
    fn bind_alias(&mut self, ty: &'static str, expr: &str, arg: &str) -> String {
        let node = self
            .graph
            .operand(arg, &self.node_of)
            .unwrap_or_else(|e| panic!("bind_alias({:?}): {:#}", expr, e));
        self.bind_node(ty, expr, node)
    }

    /// Like `bind_op`, but the operands are already node ids - for a site
    /// whose MEANING needs an inner node the emitted text does not name.
    fn bind_op_nodes(&mut self, ty: &'static str, expr: &str, op: GOp, ids: Vec<NodeId>) -> String {
        let node = self.graph.add(op, ids);
        self.bind_node(ty, expr, node)
    }

    /// `ok &= cond` - record a condition under which the abstract domain
    /// did NOT give up on this lane. The emitted code is untouched; this
    /// builds the node that replaces the `dp`/`*bd` side channels.
    fn require(&mut self, cond: NodeId) {
        let v = self.ok;
        self.ok = self.graph.fold(GOp::And, vec![v, cond]);
    }

    /// `live &= cond` - record fork-configuration membership. Separate
    /// from `ok` because the two mean different things to the caller: a
    /// non-live lane belongs to a sibling configuration, a non-ok lane
    /// belongs to the interpreter.
    fn require_live(&mut self, cond: NodeId) {
        let v = self.live;
        self.live = self.graph.fold(GOp::And, vec![v, cond]);
    }

    /// `ok &= <this scalar is strictly positive>` - the premise the
    /// interval scale/divide helpers are monotone under.
    fn require_positive(&mut self, name: &str) -> NodeId {
        let zero = self.graph.leaf(GOp::Const(0, 0));
        let n = self
            .graph
            .operand(name, &self.node_of.clone())
            .unwrap_or_else(|e| panic!("require_positive({:?}): {:#}", name, e));
        let pos = self.graph.fold(GOp::Gt, vec![n, zero]);
        self.require(pos);
        pos
    }

    /// `ok &= <the floor of this interval is unique>`. The guard behind
    /// every "an interval was consumed where an integer was needed" site.
    fn require_flr_known(&mut self, name: &str) {
        let n = self
            .graph
            .operand(name, &self.node_of.clone())
            .unwrap_or_else(|e| panic!("require_flr_known({:?}): {:#}", name, e));
        let f = self.graph.add(GOp::Flr, vec![n]);
        let k = self.graph.add(GOp::Known, vec![f]);
        self.require(k);
    }

    /// `ok &= false` - the kernel cannot represent this at all.
    fn require_never(&mut self) {
        let f = self.graph.leaf(GOp::ConstBool(false));
        self.require(f);
    }

    /// `valid &= <name is determined>` - the condition a select imposes
    /// (it cannot pick on an undecided condition).
    fn require_known(&mut self, name: &str) -> NodeId {
        let n = self
            .graph
            .operand(name, &self.node_of.clone())
            .unwrap_or_else(|e| panic!("require_known({:?}): {:#}", name, e));
        let k = self.graph.add(GOp::Known, vec![n]);
        self.require(k);
        k
    }

    fn bind_node(&mut self, ty: &'static str, expr: &str, node: NodeId) -> String {
        let name = format!("v{}", self.n);
        self.n += 1;
        self.node_of.insert(name.clone(), node);
        self.buf().push(Line::Let {
            name: name.clone(),
            ty,
            expr: expr.to_string(),
        });
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
            _ => false,
        }
    }

    // ---- lifts ----
    /// Any numeric K as a ZN variable (broadcast scalars).
    fn as_zn(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZN(v) => v.clone(),
            K::NumC(c) => self.bind_op("ZN", &format!("zn_splat({})", p8(c)), GOp::Const(c.as_raw_u32() as i32, c.as_raw_u32() as i32), &[]),
            K::SN(v) => self.bind_alias("ZN", &format!("zn_splat({})", v), v),
            other => bail!("as_zn on {:?}", other),
        })
    }
    /// Any interval-or-number K as a ZI variable.
    fn as_zi(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZI(v) => v.clone(),
            K::ZN(v) => self.bind_alias("ZI", &format!("zi_of_zn({})", v), v),
            K::NumC(c) => self.bind_op("ZI", &format!("zi_splat({}, {})", p8(c), p8(c)), GOp::Const(c.as_raw_u32() as i32, c.as_raw_u32() as i32), &[]),
            K::SN(v) => self.bind_alias("ZI", &format!("zi_splat({}, {})", v, v), v),
            K::SI(v) => self.bind_alias("ZI", &format!("zi_splat({}.0, {}.1)", v, v), v),
            other => bail!("as_zi on {:?}", other),
        })
    }
    fn as_zb(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::ZB(v) => v.clone(),
            K::BoolC(b) => self.bind_op("ZB", &format!("zb_splat({})", b), GOp::ConstBool(*b), &[]),
            K::SB(v) => self.bind_alias("ZB", &format!("zb_splat({})", v), v),
            other => bail!("as_zb on {:?}", other),
        })
    }
    /// Scalar interval expression for S-level interval math.
    fn as_si(&mut self, k: &K) -> Result<String> {
        Ok(match k {
            K::SI(v) => v.clone(),
            K::SN(v) => self.bind_alias("(P8, P8)", &format!("({}, {})", v, v), v),
            K::NumC(c) => self.bind_op("(P8, P8)", &format!("({}, {})", p8(&c.clone()), p8(c)), GOp::Const(c.as_raw_u32() as i32, c.as_raw_u32() as i32), &[]),
            other => bail!("as_si on {:?}", other),
        })
    }

    fn is_z(k: &K) -> bool {
        matches!(k, K::ZN(_) | K::ZI(_) | K::ZB(_))
    }
    fn is_ival(k: &K) -> bool {
        matches!(k, K::SI(_) | K::ZI(_))
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

/// Emit one class kernel and write it to `out_path`.
pub fn emit_kernel(program: &Program, witness_path: &str, out_path: &str) -> Result<()> {
    let text = emit_kernel_text(program, witness_path)?;
    std::fs::write(out_path, &text).with_context(|| format!("write {}", out_path))?;
    eprintln!("wrote {} ({} bytes)", out_path, text.len());
    Ok(())
}

/// The same emission, returning the TEXT. This is what the staleness gate
/// (`transpile::names`'s `generated_is_current`) calls: it regenerates every
/// checked-in kernel in-process and compares, so a change to the emitter
/// that nobody re-ran fails a test instead of silently leaving the
/// committed kernels describing an older frame.
pub fn emit_kernel_text(program: &Program, witness_path: &str) -> Result<String> {
    render(&mut emit_walk(program, witness_path)?)
}

/// The emit-time WALK alone: evaluate `__frame` against the witness and
/// return the full emitter state - node lists (`pre`/`suf`), the final
/// cell topology, taint, pins, output facts. `render` turns one walk into
/// class-kernel text; the fusion pass (`transpile::fuse`) merges several
/// members' walks into one fused artifact. One lowering, two consumers -
/// there is no second walk to drift.
pub(crate) fn emit_walk(program: &Program, witness_path: &str) -> Result<Emit> {
    let mut e = Emit {
        witness_len: 0,
        cells: HashMap::new(),
        globals: HashMap::new(),
        next_cell: 0,
        env: HashMap::new(),
        uni: BTreeMap::new(),
        vary_in: BTreeMap::new(),
        dirty: BTreeSet::new(),
        body: Vec::new(),
        variants: Vec::new(),
        rep_of: [0u8; 64],
        scratch: Vec::new(),
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
        graph: Graph::new(),
        node_of: HashMap::new(),
        live: 0,
        ok: 0,
    };
    let all = e.graph.leaf(GOp::ConstBool(true));
    e.live = all;
    e.ok = all;
    load_witness(witness_path, &mut e)?;
    // The six buttons are the graph's FREE CHOICES, and leaves: nothing
    // computes them, the search does. They are also emitted names (the
    // suffix binds `kbK` from its const generic), and registering them
    // here is what lets a value written straight through to an output
    // cell - dash stores kb4/kb5 - resolve like any other name.
    for k in 0..6 {
        e.tainted_vars.insert(format!("kb{}", k));
        let n = e.graph.leaf(GOp::Free(k as u8));
        e.node_of.insert(format!("kb{}", k), n);
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

    Ok(e)
}

/// Cells REACHABLE from the globals table in the walk's FINAL heap
/// topology. For a dying member the player was deleted at emit time
/// (`__array_table_drop_last`), so its `objects.1.*` cells drop out of
/// this set: what remains is exactly the data the dead boundary state can
/// depend on. The fusion pass uses that to prove a member's boundary rows
/// are block-uniform.
pub(crate) fn reachable_cells(e: &Emit) -> BTreeSet<u32> {
    let mut seen: BTreeSet<u32> = BTreeSet::new();
    let mut stack: Vec<u32> = e.globals.values().copied().collect();
    while let Some(id) = stack.pop() {
        if !seen.insert(id) {
            continue;
        }
        match e.cells.get(&id) {
            Some(CellT::Obj(fields)) => stack.extend(fields.values().copied()),
            Some(CellT::Arr(items)) => stack.extend(items.iter().copied()),
            Some(CellT::Val(K::Ptr(c))) => stack.push(*c),
            _ => {}
        }
    }
    seen
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
                K::BoolC(false) => {
                    e.require_never();
                    e.line("*bd = true; // assert_true of constant false");
                }
                K::SB(ref v) => {
                    let n = e.graph.operand(v, &e.node_of.clone()).unwrap();
                    e.require(n);
                    e.line(&format!("if !{} {{ *bd = true; }}", v));
                }
                K::STri(ref v) => {
                    // `!= Some(true)` fails on unknown AND on known-false,
                    // so the surviving condition is `Known(c) AND c` - the
                    // same shape as zguard, one lane wide.
                    let n = e.graph.operand(v, &e.node_of.clone()).unwrap();
                    let kn = e.graph.add(GOp::Known, vec![n]);
                    let both = e.graph.fold(GOp::And, vec![kn, n]);
                    e.require(both);
                    e.line(&format!("if {} != Some(true) {{ *bd = true; }}", v));
                }
                K::ZB(v) => {
                    // zguard deopts a lane whose condition is unknown OR
                    // known-false, so the surviving condition is exactly
                    // `Known(c) AND c`.
                    let n = e.graph.operand(v.as_str(), &e.node_of.clone()).unwrap();
                    let kn = e.graph.add(GOp::Known, vec![n]);
                    let both = e.graph.fold(GOp::And, vec![kn, n]);
                    e.require(both);
                    e.line(&format!("zguard({}, &mut dp);", v));
                }
                K::UBool { .. } => {
                    e.require_never();
                    e.line("*bd = true; // assert_true of UnknownBool");
                }
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
        (UnaryOp::Not, K::SB(v)) => K::SB(e.bind_op("bool", &format!("!{}", v), GOp::Not, &[v])),
        (UnaryOp::Not, K::STri(v)) => K::STri(e.bind_op("Option<bool>", &format!("{}.map(|b| !b)", v), GOp::Not, &[v])),
        (UnaryOp::Not, K::ZB(v)) => K::ZB(e.bind_op("ZB", &format!("zb_not({})", v), GOp::Not, &[v])),
        (UnaryOp::Not, K::UBool { .. }) => K::UBool { btn: None },
        (UnaryOp::Minus, K::NumC(c)) => K::NumC(-*c),
        (UnaryOp::Minus, K::SN(v)) => K::SN(e.bind_op("P8", &format!("-{}", v), GOp::Neg, &[v])),
        (UnaryOp::Minus, K::ZN(v)) => K::ZN(e.bind_op("ZN", &format!("zn_neg({})", v), GOp::Neg, &[v])),
        (UnaryOp::Minus, K::SI(v)) => {
            K::SI(e.bind_op("(P8, P8)", &format!("(-{v}.1, -{v}.0)", v = v), GOp::Neg, &[v]))
        }
        (UnaryOp::Minus, K::ZI(v)) => K::ZI(e.bind_op("ZI", &format!("zi_neg({})", v), GOp::Neg, &[v])),
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
                Ok(K::ZN(e.bind_op("ZN", &format!("zn_rem({}, {})", a, b), GOp::Rem, &[a.as_str(), b.as_str()])))
            } else {
                let (a, b) = (sn(e, l)?, sn(e, r)?);
                Ok(K::SN(e.bind_op("P8", &format!("{} % {}", a, b), GOp::Rem, &[a.as_str(), b.as_str()])))
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
        K::NumC(c) => e.bind_op("P8", &p8(c), GOp::Const(c.as_raw_u32() as i32, c.as_raw_u32() as i32), &[]),
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
            let var = e.bind_op("P8", &format!("{} {} {}", a, if sub { "-" } else { "+" }, b), if sub { GOp::Sub } else { GOp::Add }, &[a.as_str(), b.as_str()]);
            if let Some(k) = known {
                e.pin_val.insert(var.clone(), k);
            }
            K::SN(var)
        }
        (false, true) => {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            K::ZN(e.bind_op(
                "ZN",
                &format!("{}({}, {})", if sub { "zn_sub" } else { "zn_add" }, a, b),
                if sub { GOp::Sub } else { GOp::Add },
                &[a.as_str(), b.as_str()],
            ))
        }
        (true, false) => {
            let (a, b) = (e.as_si(l)?, e.as_si(r)?);
            K::SI(e.bind_op(
                "(P8, P8)",
                &format!("{}({}, {})", if sub { "si_sub" } else { "si_add" }, a, b),
                if sub { GOp::Sub } else { GOp::Add },
                &[a.as_str(), b.as_str()],
            ))
        }
        (true, true) => {
            let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
            K::ZI(e.bind_op(
                "ZI",
                &format!("{}({}, {})", if sub { "zi_sub" } else { "zi_add" }, a, b),
                if sub { GOp::Sub } else { GOp::Add },
                &[a.as_str(), b.as_str()],
            ))
        }
    })
}

fn arith_mul(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    if !Emit::is_ival(l) && !Emit::is_ival(r) {
        return if Emit::is_z(l) || Emit::is_z(r) {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            Ok(K::ZN(e.bind_op("ZN", &format!("zn_mul({}, {})", a, b), GOp::Mul, &[a.as_str(), b.as_str()])))
        } else {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            Ok(K::SN(e.bind_op("P8", &format!("{} * {}", a, b), GOp::Mul, &[a.as_str(), b.as_str()])))
        };
    }
    // interval * positive number (av_mul's only interval arm)
    let (iv, num) = if Emit::is_ival(l) { (l, r) } else { (r, l) };
    if !Emit::is_num(num) {
        bail!("* on ({:?}, {:?}): no interpreter arm", l, r);
    }
    if Emit::is_z(iv) || Emit::is_z(num) {
        let (a, b) = (e.as_zi(iv)?, e.as_zn(num)?);
        let v = e.bind_op(
            "ZI",
            &format!("zi_mul_pos({}, {}, &mut dp)", a, b),
            GOp::Mul,
            &[a.as_str(), b.as_str()],
        );
        e.require_positive(&b);
        Ok(K::ZI(v))
    } else {
        let a = e.as_si(iv)?;
        let b = sn(e, num)?;
        e.line(&format!("if {} <= P8::from_i16(0) {{ *bd = true; }}", b));
        e.require_positive(&b);
        Ok(K::SI(e.bind_op(
            "(P8, P8)",
            &format!(
                "{{ let r = IV::new({a}.0, {a}.1).scale_positive({b}); (r.low, r.high) }}",
                a = a,
                b = b
            ),
            GOp::Mul,
            &[a.as_str(), b.as_str()],
        )))
    }
}

fn arith_div(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    if !Emit::is_ival(l) && !Emit::is_ival(r) {
        return if Emit::is_z(l) || Emit::is_z(r) {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            Ok(K::ZN(e.bind_op("ZN", &format!("zn_div({}, {})", a, b), GOp::Div, &[a.as_str(), b.as_str()])))
        } else {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            Ok(K::SN(e.bind_op("P8", &format!("{} / {}", a, b), GOp::Div, &[a.as_str(), b.as_str()])))
        };
    }
    if !Emit::is_ival(l) || !Emit::is_num(r) {
        bail!("/ on ({:?}, {:?}): no interpreter arm", l, r);
    }
    if Emit::is_z(l) || Emit::is_z(r) {
        let (a, b) = (e.as_zi(l)?, e.as_zn(r)?);
        let v = e.bind_op(
            "ZI",
            &format!("zi_div_pos({}, {}, &mut dp)", a, b),
            GOp::Div,
            &[a.as_str(), b.as_str()],
        );
        e.require_positive(&b);
        Ok(K::ZI(v))
    } else {
        let a = e.as_si(l)?;
        let b = sn(e, r)?;
        e.line(&format!("if {} <= P8::from_i16(0) {{ *bd = true; }}", b));
        e.require_positive(&b);
        Ok(K::SI(e.bind_op(
            "(P8, P8)",
            &format!(
                "{{ let r = IV::new({a}.0, {a}.1).div_positive({b}); (r.low, r.high) }}",
                a = a,
                b = b
            ),
            GOp::Div,
            &[a.as_str(), b.as_str()],
        )))
    }
}

/// av_eq, kind-directed.
fn eq(e: &mut Emit, l: &K, r: &K) -> Result<K> {
    use K::*;
    Ok(match (l, r) {
        (SI(_) | ZI(_), _) | (_, SI(_) | ZI(_)) => K::BoolC(false),
        (NumC(_) | SN(_) | ZN(_), NumC(_) | SN(_) | ZN(_)) => {
            if Emit::is_z(l) || Emit::is_z(r) {
                let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
                K::ZB(e.bind_op("ZB", &format!("zn_eq({}, {})", a, b), GOp::Eq, &[a.as_str(), b.as_str()]))
            } else {
                let (a, b) = (sn(e, l)?, sn(e, r)?);
                K::SB(e.bind_op("bool", &format!("{} == {}", a, b), GOp::Eq, &[a.as_str(), b.as_str()]))
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
                K::ZB(e.bind_op("ZB", &format!("zb_eq({}, {})", a, b), GOp::Eq, &[a.as_str(), b.as_str()]))
            } else {
                let a = sb(e, l)?;
                let b = sb(e, r)?;
                K::SB(e.bind_op("bool", &format!("{} == {}", a, b), GOp::Eq, &[a.as_str(), b.as_str()]))
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
        K::BoolC(b) => e.bind_op("bool", &format!("{}", b), GOp::ConstBool(*b), &[]),
        other => bail!("sb on {:?}", other),
    })
}

fn cmp(e: &mut Emit, op: BinaryOp, l: &K, r: &K) -> Result<K> {
    use BinaryOp as B;
    let (zop, sop, jop, gop) = match op {
        B::LessThan => ("zn_lt", "<", "Lt", GOp::Lt),
        B::LessThanEqual => ("zn_le", "<=", "Le", GOp::Le),
        B::GreaterThan => ("zn_gt", ">", "Gt", GOp::Gt),
        B::GreaterThanEqual => ("zn_ge", ">=", "Ge", GOp::Ge),
        _ => unreachable!(),
    };
    let ival = Emit::is_ival(l) || Emit::is_ival(r);
    let z = Emit::is_z(l) || Emit::is_z(r);
    Ok(match (ival, z) {
        (false, false) => {
            let (a, b) = (sn(e, l)?, sn(e, r)?);
            K::SB(e.bind_op("bool", &format!("{} {} {}", a, sop, b), gop, &[a.as_str(), b.as_str()]))
        }
        (false, true) => {
            let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
            K::ZB(e.bind_op("ZB", &format!("{}({}, {})", zop, a, b), gop, &[a.as_str(), b.as_str()]))
        }
        (true, true) => {
            let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
            K::ZB(e.bind_op("ZB", &format!("zi_cmp(Cmp::{}, {}, {})", jop, a, b), gop, &[a.as_str(), b.as_str()]))
        }
        (true, false) => {
            let (a, b) = (e.as_si(l)?, e.as_si(r)?);
            K::STri(e.bind_op(
                "Option<bool>",
                &format!("si_cmp(Cmp::{}, {}, {})", jop, a, b),
                gop,
                &[a.as_str(), b.as_str()],
            ))
        }
    })
}

fn select(e: &mut Emit, c: &K, t: &K, f: &K) -> Result<K> {
    match c {
        K::BoolC(b) => return Ok(if *b { t.clone() } else { f.clone() }),
        _ => {}
    }
    // Identical arms: the value does not depend on the condition, so
    // neither does its VALIDITY. Returning early matters for more than
    // node count - the per-lane paths below record `Known(cond)` as a
    // validity conjunct, and a lane whose condition is undecided would be
    // sent to the interpreter over a select that could not have used it.
    if t == f {
        return Ok(t.clone());
    }
    // Unify the sides' class.
    let z = Emit::is_z(t) || Emit::is_z(f) || matches!(c, K::ZB(_));
    let ival = Emit::is_ival(t) || Emit::is_ival(f);
    match c {
        K::SB(cv) => Ok(if z {
            if ival {
                let (a, b) = (e.as_zi(t)?, e.as_zi(f)?);
                K::ZI(e.bind_op("ZI", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
            } else if matches!(t, K::ZB(_) | K::BoolC(_) | K::SB(_))
                && matches!(f, K::ZB(_) | K::BoolC(_) | K::SB(_))
                && (matches!(t, K::ZB(_)) || matches!(f, K::ZB(_)))
            {
                let (a, b) = (e.as_zb(t)?, e.as_zb(f)?);
                K::ZB(e.bind_op("ZB", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
            } else {
                let (a, b) = (e.as_zn(t)?, e.as_zn(f)?);
                K::ZN(e.bind_op("ZN", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
            }
        } else {
            // scalar sides
            match (t, f) {
                (K::SN(_) | K::NumC(_), K::SN(_) | K::NumC(_)) => {
                    let (a, b) = (sn(e, t)?, sn(e, f)?);
                    K::SN(e.bind_op("P8", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
                }
                (K::SB(_) | K::BoolC(_), K::SB(_) | K::BoolC(_)) => {
                    let (a, b) = (sb(e, t)?, sb(e, f)?);
                    K::SB(e.bind_op("bool", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
                }
                (K::SI(_) | K::SN(_) | K::NumC(_), K::SI(_) | K::SN(_) | K::NumC(_)) => {
                    let (a, b) = (e.as_si(t)?, e.as_si(f)?);
                    K::SI(e.bind_op("(P8, P8)", &format!("if {} {{ {} }} else {{ {} }}", cv, a, b), GOp::Sel, &[cv.as_str(), a.as_str(), b.as_str()]))
                }
                _ => bail!("select scalar sides {:?} / {:?}", t, f),
            }
        }),
        K::ZB(cv) => Ok(if ival {
            let (a, b) = (e.as_zi(t)?, e.as_zi(f)?);
            K::ZI({
                let v = e.bind_op(
                    "ZI",
                    &format!("zsel_i({}, {}, {}, &mut dp)", cv, a, b),
                    GOp::Sel,
                    &[cv.as_str(), a.as_str(), b.as_str()],
                );
                e.require_known(cv.as_str());
                v
            })
        } else if matches!(t, K::ZB(_) | K::BoolC(_) | K::SB(_))
            && matches!(f, K::ZB(_) | K::BoolC(_) | K::SB(_))
        {
            let (a, b) = (e.as_zb(t)?, e.as_zb(f)?);
            K::ZB({
                let v = e.bind_op(
                    "ZB",
                    &format!("zsel_b({}, {}, {}, &mut dp)", cv, a, b),
                    GOp::Sel,
                    &[cv.as_str(), a.as_str(), b.as_str()],
                );
                e.require_known(cv.as_str());
                v
            })
        } else {
            let (a, b) = (e.as_zn(t)?, e.as_zn(f)?);
            K::ZN({
                let v = e.bind_op(
                    "ZN",
                    &format!("zsel_n({}, {}, {}, &mut dp)", cv, a, b),
                    GOp::Sel,
                    &[cv.as_str(), a.as_str(), b.as_str()],
                );
                e.require_known(cv.as_str());
                v
            })
        }),
        K::STri(cv) => {
            // Uniform tri-state: unknown means the whole slice deopts.
            e.line(&format!("if {}.is_none() {{ *bd = true; }}", cv));
            {
                let cn = e.graph.operand(&cv, &e.node_of.clone()).unwrap();
                let known = e.graph.add(GOp::Known, vec![cn]);
                e.require(known);
            }
            let cb = {
                let cn = e
                    .graph
                    .operand(&cv, &e.node_of)
                    .unwrap_or_else(|err| panic!("unwrap_or operand {:?}: {:#}", cv, err));
                let known = e.graph.add(GOp::Known, vec![cn]);
                e.bind_op_nodes("bool", &format!("{}.unwrap_or(false)", cv), GOp::And, vec![known, cn])
            };
            select(e, &K::SB(cb), t, f)
        }
        K::UBool { .. } => {
            e.require_never();
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
                K::ZI(v) => {
                    // A <=2-way FORK: open a runtime loop; everything after
                    // this instruction nests inside it. dp/bd are shadowed
                    // so one configuration's failures do not leak into the
                    // next.
                    let v = v.clone();
                    if e.cur_tainted {
                        bail!("split of a button-dependent value is not supported (v1)");
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
                    // The fork mints its own name, so register its node by
                    // hand: the narrowed value is Fork(depth) over the
                    // operand. (`valid{d}` stays a plain emitted variable
                    // until the validity path is migrated.)
                    let fork_arg = e
                        .graph
                        .operand(&v, &e.node_of)
                        .unwrap_or_else(|err| panic!("fork operand {:?}: {:#}", v, err));
                    let fork_node = e.graph.add(GOp::Split(d as u8), vec![fork_arg]);
                    e.node_of.insert(frag.clone(), fork_node);
                    // The fork returns (value, validity); the mask is the
                    // second node, and it narrows this configuration's
                    // validity exactly as `valid{d} = valid & frag_fv` does
                    // in the emitted code.
                    let fv = e.graph.add(GOp::SplitValid(d as u8), vec![fork_arg]);
                    e.require_live(fv);
                    // zi_fork_flr also DEOPTS a lane whose interval spans
                    // more than two floors - there is no third fragment to
                    // put it in. That is an ok condition, not a liveness
                    // one, and the fork call is what enforces it.
                    let fok = e.graph.add(GOp::SplitOk, vec![fork_arg]);
                    e.require(fok);
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
                    e.require_flr_known(v);
                    Ok(a.clone())
                }
                K::ZN(_) | K::SN(_) | K::NumC(_) => Ok(a.clone()),
                other => bail!("__split_by_flr on {:?}", other),
            }
        }
        "__split_at" => {
            match &vals[1] {
                K::NumC(_) => {}
                other => bail!("__split_at with non-constant threshold {:?}", other),
            }
            match &vals[0] {
                // On an exact value the split is a no-op: every lane is
                // already on one side of the threshold.
                K::ZN(_) | K::SN(_) | K::NumC(_) => Ok(vals[0].clone()),
                // On an INTERVAL it is a two-way narrowing, and the graph
                // has no op for it. No checked-in kernel reaches this (no
                // `zi_split_at` appears in any generated file), so rather
                // than carry an unexercised lowering that the graph cannot
                // describe, say so.
                other => bail!(
                    "__split_at on {:?}: the interval split is not modelled in the \
                     graph IR (plans/multi-output-fusion.md). No class kernel has \
                     needed it; add Op::SplitAt + its rendering if one does.",
                    other
                ),
            }
        }
        "__array_table_drop_last" => {
            // Structural: shrink the emit-time array cell. No runtime code -
            // the heap is folded at emit time, so dropping the last element
            // only changes the emit-time topology. This is how a DYING
            // member's `del` tail lowers (plans/shape-tag-plan.md): what
            // survives is the changed OUTPUT SHAPE, which the boundary
            // materializer owns, not an op here.
            let ptr = match e.env.get(&args[0]) {
                Some(K::Ptr(id)) => *id,
                other => bail!("__array_table_drop_last on {:?}", other),
            };
            match e.cells.get_mut(&ptr) {
                Some(CellT::Arr(items)) => {
                    items
                        .pop()
                        .ok_or_else(|| anyhow!("__array_table_drop_last on empty array"))?;
                    Ok(K::Nil)
                }
                other => bail!("__array_table_drop_last on cell {:?}", other),
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
            K::SN(v) => Ok(K::SN(e.bind_op("P8", &format!("{}.flr()", v), GOp::Flr, &[v]))),
            K::ZN(v) => Ok(K::ZN(e.bind_op("ZN", &format!("zn_flr({})", v), GOp::Flr, &[v]))),
            // zi_flr deopts the lanes whose interval straddles an integer,
            // so the surviving condition is exactly `Known(flr(v))`, and
            // the RESULT is exact wherever the lane survives.
            K::ZI(v) => Ok(K::ZN({
                let r = e.bind_op("ZN", &format!("zi_flr({}, &mut dp)", v), GOp::Flr, &[v]);
                let rn = e.node_of[&r];
                let kn = e.graph.add(GOp::Known, vec![rn]);
                e.require(kn);
                r
            })),
            K::SI(v) => {
                e.line(&format!("if {v}.0.flr() != {v}.1.flr() {{ *bd = true; }}", v = v));
                e.require_flr_known(v);
                Ok(K::SN(e.bind_op("P8", &format!("{}.0.flr()", v), GOp::Flr, &[v])))
            }
            other => bail!("flr on {:?}", other),
        },
        "abs" => match &vals[0] {
            K::NumC(c) => Ok(K::NumC(c.abs())),
            K::SN(v) => Ok(K::SN(e.bind_op("P8", &format!("{}.abs()", v), GOp::Abs, &[v]))),
            K::ZN(v) => Ok(K::ZN(e.bind_op("ZN", &format!("zn_abs({})", v), GOp::Abs, &[v]))),
            K::ZI(v) => Ok(K::ZI(e.bind_op("ZI", &format!("zi_abs({})", v), GOp::Abs, &[v]))),
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
                    K::SN(e.bind_op(
                        "P8",
                        &format!("{}.{}({})", a, if is_min { "min" } else { "max" }, b),
                        if is_min { GOp::Min } else { GOp::Max },
                        &[a.as_str(), b.as_str()],
                    ))
                }
                (false, true) => {
                    let (a, b) = (e.as_zn(l)?, e.as_zn(r)?);
                    K::ZN(e.bind_op(
                        "ZN",
                        &format!("{}({}, {})", if is_min { "zn_min" } else { "zn_max" }, a, b),
                        if is_min { GOp::Min } else { GOp::Max },
                        &[a.as_str(), b.as_str()],
                    ))
                }
                (true, true) => {
                    let (a, b) = (e.as_zi(l)?, e.as_zi(r)?);
                    K::ZI(e.bind_op(
                        "ZI",
                        &format!("{}({}, {})", if is_min { "zi_min" } else { "zi_max" }, a, b),
                        if is_min { GOp::Min } else { GOp::Max },
                        &[a.as_str(), b.as_str()],
                    ))
                }
                (true, false) => {
                    let (a, b) = (e.as_si(l)?, e.as_si(r)?);
                    let f = if is_min { "min" } else { "max" };
                    K::SI(e.bind_op(
                        "(P8, P8)",
                        &format!("({a}.0.{f}({b}.0), {a}.1.{f}({b}.1))", a = a, b = b, f = f),
                        if is_min { GOp::Min } else { GOp::Max },
                        &[a.as_str(), b.as_str()],
                    ))
                }
            })
        }
        "mget" => {
            let (x, y) = (&vals[0], &vals[1]);
            if Emit::is_z(x) || Emit::is_z(y) {
                let (a, b) = (e.as_zn(x)?, e.as_zn(y)?);
                Ok(K::ZN(e.bind_op("ZN", &format!("zn_mget(g.cart, {}, {})", a, b), GOp::Mget, &[a.as_str(), b.as_str()])))
            } else {
                let (a, b) = (sn(e, x)?, sn(e, y)?);
                Ok(K::SN(e.bind_op(
                    "P8",
                    &format!(
                        "P8::from_i16(g.cart.mget({}, {}).expect(\"mget\") as i16)",
                        a, b
                    ),
                    GOp::Mget,
                    &[a.as_str(), b.as_str()],
                )))
            }
        }
        "tile_flag_at" => {
            let (x, y, w, h, f) = (&vals[0], &vals[1], &vals[2], &vals[3], &vals[4]);
            let (wv, hv, fv) = (sn(e, w)?, sn(e, h)?, sn(e, f)?);
            if Emit::is_z(x) || Emit::is_z(y) {
                let (a, b) = (e.as_zn(x)?, e.as_zn(y)?);
                Ok(K::ZB(e.bind_op(
                    "ZB",
                    &format!(
                        "zn_tile_flag_at(g.cache, g.cart, {}, {}, {}, {}, {})",
                        a, b, wv, hv, fv
                    ),
                    GOp::TileFlagAt,
                    &[a.as_str(), b.as_str(), wv.as_str(), hv.as_str(), fv.as_str()],
                )))
            } else {
                let (a, b) = (sn(e, x)?, sn(e, y)?);
                Ok(K::SB(e.bind_op(
                    "bool",
                    &format!(
                        "{{ let z = zn_tile_flag_at(g.cache, g.cart, zn_splat({}), zn_splat({}), {}, {}, {}); z.val & 1 != 0 }}",
                        a, b, wv, hv, fv
                    ),
                    GOp::TileFlagAt,
                    &[a.as_str(), b.as_str(), wv.as_str(), hv.as_str(), fv.as_str()],
                )))
            }
        }
        "sin" => match &vals[0] {
            K::SN(v) => Ok(K::SN(e.bind_op("P8", &format!("{}.pico8_sin()", v), GOp::Sin, &[v]))),
            K::NumC(c) => Ok(K::NumC(c.pico8_sin())),
            K::ZN(v) => Ok(K::ZN(e.bind_op("ZN", &format!("zn_sin({})", v), GOp::Sin, &[v]))),
            // The emitter gives up on sin over a non-exact input and emits
            // sin's RANGE as a literal, ignoring the operand. So the node is
            // that constant: a Sin(v) node would claim a dependency the
            // generated code does not have, and would stop two members from
            // sharing it when they reach this site with different operands.
            K::SI(_) | K::ZI(_) => Ok(K::SI(e.bind_op(
                "(P8, P8)",
                "(P8::from_i16(-1), P8::from_i16(1))",
                GOp::Const(-1 << 16, 1 << 16),
                &[],
            ))),
            other => bail!("sin on {:?}", other),
        },
        other => bail!("call_builtin {:?} not implemented in the kernel", other),
    }
}

/// One boundary output of a walk.
pub(crate) struct OutField {
    /// Witness cell id this value lands in.
    pub(crate) cell: u32,
    pub(crate) ty: &'static str,
    /// The emitted expression. Legacy: it is what the TEXT fuser reads;
    /// the graph-driven emitter uses `node`.
    pub(crate) expr: String,
    /// Button-dependent (differs between the 64 variants).
    pub(crate) tainted: bool,
    /// The graph node this cell ends the frame holding. Every output has
    /// one: a value the emitter could not describe structurally would have
    /// failed at its bind site, not here.
    pub(crate) node: NodeId,
}

/// Output cells of one walk, plus `ubool` - cells ending the frame as
/// fresh UnknownBools (next frame's button inputs).
pub(crate) struct OutFields {
    pub(crate) fields: Vec<OutField>,
    pub(crate) ubool: Vec<u32>,
}

/// Resolve one walk into the LINES a kernel is made of, and the output
/// fields that read them.
///
/// This is the single producer of emitted bodies. `render` (the class
/// kernels) and `transpile::fuse` (the fused artifact) both go through
/// it, so there is no second lowering to drift - which nearly happened:
/// for one commit `render` emitted from the graph while `fuse` still read
/// the walk's own `Line` pushes, and the two would have diverged silently
/// the moment the graph learned anything the text stream did not know.
pub(crate) fn lower_walk(e: &mut Emit) -> Result<OutFields> {
    let mut of = compute_out_fields(e)?;
    // Replaces `e.pre`/`e.suf` (and the bookkeeping both consumers read
    // off them) with lines derived from `e.graph`, and rewrites the output
    // fields to read graph nodes.
    super::lower::emit_body(e, &mut of)?;
    Ok(of)
}

fn compute_out_fields(e: &mut Emit) -> Result<OutFields> {
    // Output struct: dirty original cells (scratch cells never escape).
    // (cell, rust ty, expr, tainted): tainted outputs differ per button
    // variant; untainted ones are identical across all 64 and are
    // reported once per fork config in KOutShared.
    let mut out_fields: Vec<OutField> = Vec::new();
    let mut out_ubool: Vec<u32> = Vec::new();
    for id in &e.dirty.clone() {
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
        // Resolve through `operand`, not `node_of`: an output can be a
        // bound name, a button bit written straight through (dash keeps
        // last frame's kb4/kb5 for edge detection), or a literal. Anything
        // else is a value the graph does not describe, and skipping it
        // silently is how this measurement once reported a false 4x.
        let named = e.node_of.clone();
        let node = e
            .graph
            .operand(&expr, &named)
            .with_context(|| format!("output cell {}", id))?;
        out_fields.push(OutField { cell: *id, ty, expr, tainted, node });
    }
    Ok(OutFields { fields: out_fields, ubool: out_ubool })
}

/// The kernel's block-facing interface - everything from the file header
/// through `row_keys()`. Shared byte-for-byte between the class-kernel
/// renderer and the fused-artifact emitter (transpile::fuse), which is
/// the point: the fused artifact IS a class kernel to the dispatcher.
pub(crate) fn emit_interface(out: &mut String, e: &Emit, of: &OutFields) -> Result<()> {
    let out_fields = &of.fields;
    let out_ubool = &of.ubool;
    writeln!(
        out,
        "// GENERATED by `transpile --kernel` (plans/kernel-plan.md). Do not edit.\n\
         //\n\
         // The STEADY-CLASS lane kernel: shape (player), pm1 freeze=0 dash_time=0.\n\
         // Shared prefix per 16-row slice, 64 monomorphized button suffixes.\n\
         #![allow(unused_variables, unused_mut, unused_imports, clippy::all)]\n\
         use celeste_engine::kernel::*;\n\
         use celeste_core::pico8_num::{{Pico8Num as P8, Pico8NumInterval as IV}};\n\
         use celeste_core::cart_data::CartData;\n\
         use celeste_core::collision_cache::CollisionCache;\n"
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

    writeln!(out, "/// Button-independent outputs: one per fork config.")?;
    writeln!(out, "pub struct KOutShared {{")?;
    for OutField { cell: id, ty, tainted, .. } in out_fields {
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
    for OutField { cell: id, ty, tainted, .. } in out_fields {
        if *tainted {
            writeln!(out, "    pub c{}: {},", id, ty)?;
        }
    }
    writeln!(out, "}}\n")?;
    writeln!(out, "pub const OUT_CELLS: &[u32] = &[")?;
    for OutField { cell: id, .. } in out_fields {
        writeln!(out, "    {},", id)?;
    }
    writeln!(out, "];\n")?;
    writeln!(
        out,
        "/// Cells that end every frame as a fresh UnknownBool (the next\n\
         /// frame's button inputs); the boundary writes UBool, no data."
    )?;
    writeln!(out, "pub const OUT_UBOOL_CELLS: &[u32] = &[")?;
    for id in out_ubool {
        writeln!(out, "    {},", id)?;
    }
    writeln!(out, "];\n")?;

    // Runtime glue: bind uniforms from a block (kind + pin checked),
    // gather a 16-row slice, and apply an output back onto a sliced block.
    writeln!(
        out,
        "use celeste_engine::runtime2::{{Rt2, Col, AV}};\n\n\
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
    for OutField { cell: id, ty, tainted, .. } in out_fields {
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

    // Direct-append output path: one accumulator block per chunk, typed
    // pushes per (config, variant) - no per-slice block cloning.
    let out_set: BTreeSet<u32> = out_fields.iter().map(|f| f.cell).collect();
    writeln!(
        out,
        "/// One wide output block per chunk: structure + uniforms cloned\n\
         /// once, varying/output columns start empty and grow by appends.\n\
         pub fn acc_init(chunk: &Rt2) -> Rt2 {{\n\
         \x20   let mut acc = Rt2::empty(0, chunk.globals.len(), &[], chunk.cart.clone(), chunk.cache.clone());\n\
         \x20   acc.strings = chunk.strings.clone();\n\
         \x20   acc.globals = chunk.globals.clone();\n\
         \x20   acc.structure = chunk.structure.clone();\n\
         \x20   acc.cols = chunk.cols.clone();\n\
         \x20   acc.shape_hash = chunk.shape_hash;"
    )?;
    for OutField { cell: id, ty, tainted, .. } in out_fields {
        match (*ty, *tainted) {
            ("ZN", _) | ("P8", true) => writeln!(out, "    acc.cols[{}] = Col::N(Vec::new());", id)?,
            ("ZI", _) | ("(P8, P8)", true) => {
                writeln!(out, "    acc.cols[{}] = Col::I(Vec::new());", id)?
            }
            ("ZB", _) | ("bool", true) => writeln!(out, "    acc.cols[{}] = Col::V(Vec::new());", id)?,
            // Untainted scalar outputs stay UNIFORM; append_out
            // overwrites the value (identical across appends - the
            // gate-2 oracle certifies that premise).
            ("P8", false) | ("bool", false) | ("(P8, P8)", false) => {}
            (other, _) => bail!("acc_init: type {}", other),
        }
    }
    for (id, kind) in &e.vary_in {
        if out_set.contains(id) {
            continue;
        }
        match *kind {
            "num" => writeln!(out, "    acc.cols[{}] = Col::N(Vec::new());", id)?,
            "bool" => writeln!(out, "    acc.cols[{}] = Col::V(Vec::new());", id)?,
            _ => unreachable!(),
        }
    }
    writeln!(
        out,
        "    for cell in OUT_UBOOL_CELLS {{\n\
         \x20       acc.cols[*cell as usize] = Col::U(AV::UBool);\n\
         \x20   }}\n\
         \x20   acc\n\
         }}\n"
    )?;

    writeln!(
        out,
        "/// Append one (config, variant)'s live lanes onto the accumulator.\n\
         /// `lo` is the slice base in `chunk` (identity columns read there).\n\
         #[allow(unused_variables)]\n\
         pub fn append_out(acc: &mut Rt2, chunk: &Rt2, lo: usize, n: usize, live: u16, sh: &KOutShared, kv: &KOut, bd: &mut bool) {{\n\
         \x20   // fresh: no fork config has written yet, so a uniform slot\n\
         \x20   // still holds the chunk's INPUT value (legitimately different).\n\
         \x20   let fresh = acc.width == 0;\n\
         \x20   for i in 0..n {{\n\
         \x20       if live & (1 << i) == 0 {{ continue; }}"
    )?;
    for OutField { cell: id, ty, tainted, .. } in out_fields {
        let src = if *tainted { "kv" } else { "sh" };
        match *ty {
            "ZN" => writeln!(
                out,
                "        if let Col::N(v) = &mut acc.cols[{id}] {{ v.push({src}.c{id}[i]); }}",
                id = id, src = src
            )?,
            "P8" if *tainted => writeln!(
                out,
                "        if let Col::N(v) = &mut acc.cols[{id}] {{ v.push({src}.c{id}); }}",
                id = id, src = src
            )?,
            "P8" => writeln!(
                out,
                "        if !fresh {{ if let Col::U(AV::Num(prev)) = &acc.cols[{id}] {{ if *prev != {src}.c{id} {{ *bd = true; }} }} }}\n\
                 \x20       acc.cols[{id}] = Col::U(AV::Num({src}.c{id}));",
                id = id, src = src
            )?,
            "ZI" => writeln!(
                out,
                "        if let Col::I(v) = &mut acc.cols[{id}] {{ v.push(({src}.c{id}.lo[i], {src}.c{id}.hi[i])); }}",
                id = id, src = src
            )?,
            "(P8, P8)" if *tainted => writeln!(
                out,
                "        if let Col::I(v) = &mut acc.cols[{id}] {{ v.push(({src}.c{id}.0, {src}.c{id}.1)); }}",
                id = id, src = src
            )?,
            "(P8, P8)" => writeln!(
                out,
                "        if !fresh {{ if let Col::U(AV::Ival(a, b)) = &acc.cols[{id}] {{ if (*a, *b) != {src}.c{id} {{ *bd = true; }} }} }}\n\
                 \x20       acc.cols[{id}] = Col::U(AV::Ival({src}.c{id}.0, {src}.c{id}.1));",
                id = id, src = src
            )?,
            "ZB" => writeln!(
                out,
                "        if let Col::V(v) = &mut acc.cols[{id}] {{ v.push(AV::Bool({src}.c{id}.val & (1 << i) != 0)); }}",
                id = id, src = src
            )?,
            "bool" if *tainted => writeln!(
                out,
                "        if let Col::V(v) = &mut acc.cols[{id}] {{ v.push(AV::Bool({src}.c{id})); }}",
                id = id, src = src
            )?,
            "bool" => writeln!(
                out,
                "        if !fresh {{ if let Col::U(AV::Bool(prev)) = &acc.cols[{id}] {{ if *prev != {src}.c{id} {{ *bd = true; }} }} }}\n\
                 \x20       acc.cols[{id}] = Col::U(AV::Bool({src}.c{id}));",
                id = id, src = src
            )?,
            other => bail!("append_out: type {}", other),
        }
    }
    for (id, kind) in &e.vary_in {
        if out_set.contains(id) {
            continue;
        }
        match *kind {
            "num" => writeln!(
                out,
                "        let val = match &chunk.cols[{id}] {{ Col::N(s) => s[(lo + i).min(chunk.width - 1)], Col::U(AV::Num(u)) => *u, _ => unreachable!() }};\n\
                 \x20       if let Col::N(v) = &mut acc.cols[{id}] {{ v.push(val); }}",
                id = id
            )?,
            "bool" => writeln!(
                out,
                "        let val = match &chunk.cols[{id}] {{ Col::V(s) => s[(lo + i).min(chunk.width - 1)], Col::U(a) => *a, _ => unreachable!() }};\n\
                 \x20       if let Col::V(v) = &mut acc.cols[{id}] {{ v.push(val); }}",
                id = id
            )?,
            _ => unreachable!(),
        }
    }
    writeln!(
        out,
        "        acc.width += 1;\n\
         \x20   }}\n\
         }}\n"
    )?;

    // row_keys(): the pre-dedup key, over EXACTLY the per-lane values
    // append_out would push. Rows with equal keys are the same row, so a
    // per-chunk seen-set can drop duplicates BEFORE materializing them
    // (plans/dedup-on-the-fly-plan.md; at f35 a 64-lane chunk emits 8.3
    // rows per surviving row).
    //
    // This is NOT boundary's key - it mixes ORIGINAL cell ids, not the
    // canonical BFS numbering - so it is only valid within one chunk,
    // which is all the pre-dedup needs. It uses boundary's primitives
    // and mirrors the two value canonicalizations boundary applies
    // before hashing, so it collapses exactly what boundary collapses:
    //   - rem cells: replaced by ONE wide interval -> no per-lane
    //     contribution at all, so they are skipped;
    //   - dash_effect_time cells: clamped at 0 before mixing.
    // Which cells those are is a property of the heap walk, not of the
    // shape witness, so the runner passes them in as masks over
    // KEY_CELLS (built from Rt2::mark_walk) and the generated code
    // checks the types it can handle.
    let key_cells = key_cells(e, of)?;
    writeln!(out, "/// Cells the pre-dedup key covers, in key-bit order.")?;
    writeln!(out, "pub const KEY_CELLS: &[u32] = &[")?;
    for (id, _, _) in &key_cells {
        writeln!(out, "    {},", id)?;
    }
    writeln!(out, "];\n")?;
    writeln!(
        out,
        "#[allow(unused_variables)]\n\
         pub fn row_keys(chunk: &Rt2, lo: usize, n: usize, sh: &KOutShared, kv: &KOut, plan: &KeyPlan, keys: &mut [(u64, u64); W]) {{\n\
         \x20   let mut h1 = [0u64; W];\n\
         \x20   let mut h2 = [0u64; W];"
    )?;
    for (j, (id, ty, tainted)) in key_cells.iter().enumerate() {
        emit_key_cell(out, j, *id, ty, *tainted)?;
    }
    writeln!(
        out,
        "    for i in 0..W {{\n\
         \x20       keys[i] = (mix64(h1[i]), mix64(h2[i]));\n\
         \x20   }}\n\
         }}\n"
    )?;

    Ok(())
}

/// The pre-dedup key-cell list, in key-bit order: out cells first, then
/// pass-through vary_in cells. Shared between the monolithic `row_keys`
/// above and the fused artifact's support-factored partial-key functions
/// (transpile::fuse) - both must agree on the ORDER, since the runtime
/// KeyPlan skip masks index into it.
pub(crate) fn key_cells(e: &Emit, of: &OutFields) -> Result<Vec<(u32, &'static str, bool)>> {
    let mut cells: Vec<(u32, &'static str, bool)> = Vec::new();
    let out_set: BTreeSet<u32> = of.fields.iter().map(|f| f.cell).collect();
    for f in &of.fields {
        cells.push((f.cell, f.ty, f.tainted));
    }
    for (id, kind) in &e.vary_in {
        if out_set.contains(id) {
            continue;
        }
        cells.push((*id, if *kind == "num" { "IN_N" } else { "IN_B" }, true));
    }
    if cells.len() > 64 {
        bail!("more than 64 key cells - the KeyPlan masks are u64");
    }
    Ok(cells)
}

/// One key cell's hash contribution into local `h1`/`h2` accumulators:
/// `h[i] += mix64(cell_const ^ code(v))`, guarded by the runtime KeyPlan
/// skip mask at the cell's ORIGINAL key-bit index `j`. Extracted so the
/// fused emitter can regroup cells into support-factored partial-key
/// functions: the whole key is a commutative per-cell SUM, so any
/// regrouping that preserves `j` produces byte-identical keys.
pub(crate) fn emit_key_cell(
    out: &mut String,
    j: usize,
    id: u32,
    ty: &str,
    tainted: bool,
) -> Result<()> {
    let src = if tainted { "kv" } else { "sh" };
    writeln!(out, "    // cell {} ({})", id, ty)?;
    writeln!(out, "    if plan.rem & (1 << {}) == 0 {{", j)?;
    let per_lane_num = |val: &str| {
        format!(
            "        let c1 = 0x5bf0_3635u64 ^ ({id}u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);\n\
             \x20       let c2 = 0x27d4_eb2fu64 ^ ({id}u64).wrapping_mul(0x9e37_79b9_7f4a_7c15);\n\
             \x20       for i in 0..W {{\n\
             \x20           let v = {val};\n\
             \x20           let v = if plan.det & (1 << {j}) != 0 && v < P8::from_raw(0i32) {{ P8::from_raw(0i32) }} else {{ v }};\n\
             \x20           let code = 1u64 << 56 | v.to_bits() as u64;\n\
             \x20           h1[i] = h1[i].wrapping_add(mix64(c1 ^ code));\n\
             \x20           h2[i] = h2[i].wrapping_add(mix64(c2 ^ code));\n\
             \x20       }}",
            id = id, j = j, val = val
        )
    };
    let per_lane_av = |val: &str| {
        format!(
            "        for i in 0..W {{\n\
             \x20           let v = {val};\n\
             \x20           h1[i] = h1[i].wrapping_add(cell_mix({id}u64, v, 0x5bf0_3635));\n\
             \x20           h2[i] = h2[i].wrapping_add(cell_mix({id}u64, v, 0x27d4_eb2f));\n\
             \x20       }}",
            id = id, val = val
        )
    };
    let body = match ty {
        "ZN" => per_lane_num(&format!("{}.c{}[i]", src, id)),
        "P8" => per_lane_num(&format!("{}.c{}", src, id)),
        "IN_N" => per_lane_num(&format!(
            "match &chunk.cols[{id}] {{ Col::N(s) => s[(lo + i).min(chunk.width - 1)], Col::U(AV::Num(u)) => *u, _ => unreachable!() }}",
            id = id
        )),
        "ZI" => per_lane_av(&format!("AV::Ival({src}.c{id}.lo[i], {src}.c{id}.hi[i])", src = src, id = id)),
        "(P8, P8)" => per_lane_av(&format!("AV::Ival({src}.c{id}.0, {src}.c{id}.1)", src = src, id = id)),
        "ZB" => per_lane_av(&format!("AV::Bool({src}.c{id}.val & (1 << i) != 0)", src = src, id = id)),
        "bool" => per_lane_av(&format!("AV::Bool({src}.c{id})", src = src, id = id)),
        "IN_B" => per_lane_av(&format!(
            "match &chunk.cols[{id}] {{ Col::V(s) => s[(lo + i).min(chunk.width - 1)], Col::U(a) => *a, _ => unreachable!() }}",
            id = id
        )),
        other => bail!("row_keys: type {}", other),
    };
    // Only numeric cells can carry the dash_effect_time clamp; a
    // det mask on anything else means the heap walk and the shape
    // witness disagree, which must not pass silently.
    if !matches!(ty, "ZN" | "P8" | "IN_N") {
        writeln!(
            out,
            "        assert!(plan.det & (1 << {}) == 0, \"det clamp on non-numeric cell {}\");",
            j, id
        )?;
    }
    writeln!(out, "{}", body)?;
    writeln!(out, "    }}")?;
    Ok(())
}

/// Assemble kernel_gen.rs.
fn render(e: &mut Emit) -> Result<String> {
    let mut out = String::new();
    let of = lower_walk(e)?;
    emit_interface(&mut out, e, &of)?;
    let out_fields = &of.fields;
    let body_text = render_lines(&e.body);

    // frame(): one function, one straight line, one `out` call per
    // DISTINCT free assignment. There is no `Pre` struct and no
    // `suffix::<const B: u8>` because there is nothing to hand across a
    // boundary - `transpile::lower` already interned what the assignments
    // share into single nodes, so the sharing is in the code rather than
    // something the compiler is asked to rediscover 64 times.
    writeln!(
        out,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u8, &KOutShared, &KOut)) {{"
    )?;
    out.push_str(&body_text);
    writeln!(out, "    let osh = KOutShared {{")?;
    for OutField { cell: id, expr, tainted, .. } in out_fields {
        if !*tainted {
            writeln!(out, "        c{}: {},", id, expr)?;
        }
    }
    writeln!(out, "    }};")?;
    writeln!(
        out,
        "    // {} of 64 free assignments are distinct successors",
        e.variants.len()
    )?;
    for v in &e.variants {
        writeln!(out, "    out({}, &osh, &KOut {{", v.mask)?;
        writeln!(out, "        valid: {},", e.valid_expr)?;
        writeln!(out, "        deopt: !{},", v.ok)?;
        writeln!(out, "        bd: {},", v.bd)?;
        for OutField { cell: id, tainted, .. } in out_fields {
            if *tainted {
                writeln!(out, "        c{}: {},", id, v.outputs[id])?;
            }
        }
        writeln!(out, "    }});")?;
    }
    for _ in 0..e.fork_depth {
        writeln!(out, "    }}")?;
    }
    writeln!(out, "}}")?;

    eprintln!(
        "kernel: {} uniform cells, {} row cells, {} out cells ({} per-variant), \
         {} distinct assignments of 64, body {} lines, witness {} facts",
        e.uni.len(),
        e.vary_in.len(),
        out_fields.len(),
        out_fields.iter().filter(|f| f.tainted).count(),
        e.variants.len(),
        body_text.lines().count(),
        e.witness_len,
    );
    // Which buttons can reach an OUTPUT (plans/multi-output-fusion.md).
    // Every output is a graph node, so this is plain reachability.
    let cones = e.graph.free_cones();
    let reaching = out_fields
        .iter()
        .fold(0u8, |m, f| m | cones[f.node as usize]);
    let bits: Vec<u8> = (0..6).filter(|b| reaching & (1 << b) != 0).collect();
    // How many of the 2^6 button combinations are actually DISTINCT?
    // Specialize on each mask into one shared hash-consed arena and compare
    // the results: equal results mean the same successor for every lane, so
    // one of the two variants could be dropped and dedup would never know.
    //
    // The signature is the output cells PLUS `ok` and `live`. An earlier
    // version compared outputs alone and reported 36/64 for steady; that
    // over-counts, because two variants that write the same cells but deopt
    // DIFFERENT lanes are not interchangeable - dropping one would silently
    // keep a lane on the kernel that belongs to the interpreter.
    let mut shared = Graph::new();
    let mut sigs: std::collections::BTreeMap<Vec<NodeId>, Vec<u8>> = Default::default();
    let mut out_only: std::collections::BTreeSet<Vec<NodeId>> = Default::default();
    for m in 0u8..64 {
        let map = e.graph.specialize_into(m, &mut shared);
        let outs: Vec<NodeId> = out_fields.iter().map(|f| map[f.node as usize]).collect();
        out_only.insert(outs.clone());
        let mut sig = outs;
        sig.push(map[e.ok as usize]);
        sig.push(map[e.live as usize]);
        sigs.entry(sig).or_default().push(m);
    }
    // What the BINARY prefix/suffix taint costs. A node truly needs
    // 2^|cone| evaluations; the split rounds that to 1 (cone empty) or 64
    // (anything else), so a node depending on one button is computed 64
    // times instead of 2.
    let mut hist = [0usize; 7];
    let mut ideal = 0usize;
    let mut binary = 0usize;
    for c in &cones {
        let k = c.count_ones() as usize;
        hist[k] += 1;
        ideal += 1usize << k;
        binary += if k == 0 { 1 } else { 64 };
    }
    eprintln!(
        "  member: {} output cells, live is node {}, ok is node {} (button cones {}/{})",
        out_fields.len(),
        e.live,
        e.ok,
        cones[e.live as usize].count_ones(),
        cones[e.ok as usize].count_ones(),
    );
    eprintln!(
        "  cone sizes {:?}; node-evaluations per frame: binary split {}, exact 2^|cone| {} ({:.1}x)",
        hist,
        binary,
        ideal,
        binary as f64 / ideal.max(1) as f64,
    );
    eprintln!(
        "graph: {} nodes, {} out cells; buttons reaching an output {:?} -> {} variant(s); \
         DISTINCT button combinations: {}/64 ({}/64 counting outputs alone); \
         fully specialized arena {} nodes",
        e.graph.len(),
        out_fields.len(),
        bits,
        1usize << bits.len(),
        sigs.len(),
        out_only.len(),
        shared.len(),
    );
    Ok(out)
}

