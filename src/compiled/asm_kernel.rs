//! Runtime AVX-512 kernels: retrace the start room at startup, assemble the
//! FUSED graph of each shape, and dispatch chunks to it - the replacement
//! for the generated Rust kernel crates.
//!
//! The search is single-room (an exited-room lane is absorbing, and
//! `Dispatch` refuses cross-room shape collisions), so a process only ever
//! dispatches `start_room()`'s shapes. So the registry retraces just the
//! start room (`trace::kernel::room_kernels_in`), turns each shape's fused,
//! fork-free graph (`trace::emit::asm_fused` -> `lower::specialize_frame`)
//! into one assembly kernel (`transpile::asm::compile_and_load_reprs`), and
//! keys them by `shape_hash`. No checked-in artifact and no multi-room
//! retrace: the compile-time win is that `gcc` assembles the graph in
//! milliseconds where rustc+LLVM took minutes over ~900k lines.
//!
//! The compute is the SAME fused graph the Rust kernel is emitted from; the
//! append here is the generic runtime equivalent of the generated
//! `acc{i}`/`append{i}`: clone the per-outcome template block, push each
//! `live & ok` lane's ASM-computed field values into it, and let
//! `Rt2::boundary` recompute the row keys (its `boundary_finish` folds the
//! cell contents into the exact same key the generated kernel precomputed)
//! and dedup. See plans/asm-and-posgraph-execution.md.

use std::collections::HashMap;
use std::os::raw::c_void;
use std::path::Path;

use anyhow::{Context, Result};

use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{self, Col, Rt2, AV};
use celeste_engine::slots::reshape;

use crate::transpile::asm::{AsmCtx, CellRepr, CollisionEnv, Compiled, Loaded, RootKind};
use crate::transpile::graph::Room;

/// One output field of a body: which output cell it writes, the flat root
/// slot the assembly wrote it to, and how to read that slot.
struct AsmField {
    cell: usize,
    root: usize,
    kind: RootKind,
}

/// One fused body (a distinct (outcome, choices) with distinct outputs):
/// its output fields plus the `ok`/`live` mask roots. A lane is materialized
/// into outcome `outcome`'s block iff `live & ok`.
struct AsmBody {
    outcome: usize,
    fields: Vec<AsmField>,
    ok_root: usize,
    live_root: usize,
}

/// How to initialize one output column of an accumulator: a uniform
/// constant (written once) or an empty typed vector (grown per row).
enum ColInit {
    Uniform(AV),
    EmptyNum,
    EmptyBool,
    EmptyIval,
}

/// A per-outcome accumulator recipe: an empty structural skeleton plus the
/// column initializers, so a fresh acc is `reshape` + apply.
struct AccTemplate {
    skeleton: Rt2,
    inits: Vec<(usize, ColInit)>,
}

impl AccTemplate {
    fn build(&self) -> Rt2 {
        let mut b = reshape(&self.skeleton, 0);
        for (c, init) in &self.inits {
            b.cols[*c] = match init {
                ColInit::Uniform(av) => Col::U(*av),
                ColInit::EmptyNum => Col::N(Vec::new()),
                ColInit::EmptyBool => Col::V(Vec::new()),
                ColInit::EmptyIval => Col::I(Vec::new()),
            };
        }
        b
    }
}

/// One shape's assembled kernel: the loaded function, its input/output
/// layout, and the per-body/per-outcome metadata the append needs.
struct AsmKernel {
    loaded: Loaded,
    compiled: Compiled,
    bodies: Vec<AsmBody>,
    acc_templates: Vec<AccTemplate>,
}

impl AsmKernel {
    /// Run one chunk. Mirrors `dispatch::run_traced_kernel`: per 16-lane
    /// slice, pack inputs, call the assembly, and for each body materialize
    /// its `live & ok` lanes into the outcome's acc; a nonzero `live & !ok`
    /// (declined) drops the whole chunk to the reference path. Then
    /// `boundary` for keys + within-block dedup, and push to `done`.
    fn run(
        &self,
        chunk: &Rt2,
        ids: &runtime2::BoundaryIds,
        done: &mut Vec<Rt2>,
        exact: bool,
    ) -> bool {
        let mut accs: Vec<Rt2> = self.acc_templates.iter().map(|t| t.build()).collect();
        let mut inbuf = vec![0u8; self.compiled.input_bytes as usize];
        let mut outbuf = vec![0u8; self.compiled.n_roots * 128];
        let env = CollisionEnv { cart: &chunk.cart, cache: &chunk.cache };
        let ctx = AsmCtx::new(&env as *const CollisionEnv as *const c_void);
        let track_origin = !chunk.origin.is_empty();

        let mut lo = 0usize;
        while lo < chunk.width {
            let n = 16.min(chunk.width - lo);
            self.pack_input(chunk, lo, n, &mut inbuf);
            unsafe {
                (self.loaded.func)(
                    inbuf.as_ptr(),
                    outbuf.as_mut_ptr(),
                    &ctx as *const AsmCtx as *const c_void,
                );
            }
            let valid = ((1u32 << n) - 1) as u16;
            for body in &self.bodies {
                let ok = read_val_mask(&outbuf, body.ok_root);
                let live = read_val_mask(&outbuf, body.live_root);
                if live & !ok & valid != 0 {
                    // Declined: a live lane the kernel could not keep. The
                    // rows already in accs are only a prefix, so drop and let
                    // the whole chunk take the reference path.
                    return false;
                }
                let take = live & ok & valid;
                if take == 0 {
                    continue;
                }
                let acc = &mut accs[body.outcome];
                for i in 0..n {
                    if take & (1 << i) == 0 {
                        continue;
                    }
                    for f in &body.fields {
                        push_field(acc, f, &outbuf, i);
                    }
                    if track_origin {
                        acc.origin.push(chunk.origin[lo + i]);
                    }
                    acc.width += 1;
                }
            }
            lo += n;
        }

        for mut acc in accs {
            if acc.width > 0 {
                if exact {
                    // The rung-agnostic / exact sets hand back EXACT rows;
                    // widening here would pre-empt the campaign's rung.
                    acc.boundary_exact();
                } else {
                    // The level-0 traced set pre-widened in the graph; the
                    // boundary's own widening is then a free agreement check
                    // and computes the row keys + dedups within the block.
                    acc.boundary(ids);
                }
                // Option 1/4: drop rows already in the frozen frontier or
                // emitted this frame (the boundary just computed the keys).
                // Off (no frozen frontier) => chunk_skip is false => no-op.
                if !acc.row_keys.is_empty() {
                    let keep: Vec<u32> = (0..acc.width as u32)
                        .filter(|&i| !super::dispatch::chunk_skip(acc.row_keys[i as usize]))
                        .collect();
                    if keep.len() < acc.width {
                        acc.retain_lanes(&keep);
                    }
                }
                if acc.width > 0 {
                    done.push(acc);
                }
            }
        }
        true
    }

    /// Pack `chunk`'s input columns for lanes `[lo, lo+16)` into `buf`. Tail
    /// lanes past `n` clamp to the last valid lane, so the assembly's
    /// per-lane call-outs (div/mget/...) never fault on garbage - the `take`
    /// mask discards those lanes anyway.
    fn pack_input(&self, chunk: &Rt2, lo: usize, n: usize, buf: &mut [u8]) {
        for i in 0..self.compiled.input_cells.len() {
            let cell = self.compiled.input_cells[i] as usize;
            let off = self.compiled.input_offsets[i] as usize;
            let col = &chunk.cols[cell];
            match self.compiled.input_reprs[i] {
                CellRepr::Num => {
                    for l in 0..16 {
                        let raw = num_raw(col.at(lo + l.min(n - 1)));
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&raw.to_le_bytes());
                    }
                }
                CellRepr::Bool => {
                    let mut mask = 0u16;
                    for l in 0..16 {
                        if let AV::Bool(true) = col.at(lo + l.min(n - 1)) {
                            mask |= 1 << l;
                        }
                    }
                    buf[off..off + 2].copy_from_slice(&mask.to_le_bytes());
                }
                CellRepr::Ival => {
                    for l in 0..16 {
                        let (a, b) = ival_raw(col.at(lo + l.min(n - 1)));
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&a.to_le_bytes());
                        buf[off + 64 + l * 4..off + 64 + l * 4 + 4].copy_from_slice(&b.to_le_bytes());
                    }
                }
            }
        }
    }
}

fn num_raw(av: AV) -> i32 {
    match av {
        AV::Num(p) => p.as_raw_u32() as i32,
        other => panic!("ASM num input got {other:?}"),
    }
}

fn ival_raw(av: AV) -> (i32, i32) {
    match av {
        AV::Ival(a, b) => (a.as_raw_u32() as i32, b.as_raw_u32() as i32),
        AV::Num(p) => (p.as_raw_u32() as i32, p.as_raw_u32() as i32),
        other => panic!("ASM ival input got {other:?}"),
    }
}

/// The `val` plane (a 16-lane mask) of a Bool root's 128-byte slot.
fn read_val_mask(buf: &[u8], root: usize) -> u16 {
    let off = root * 128;
    u16::from_le_bytes([buf[off], buf[off + 1]])
}

fn push_field(acc: &mut Rt2, f: &AsmField, buf: &[u8], i: usize) {
    let base = f.root * 128;
    match f.kind {
        RootKind::Num => {
            let raw = i32::from_le_bytes(buf[base + i * 4..base + i * 4 + 4].try_into().unwrap());
            if let Col::N(v) = &mut acc.cols[f.cell] {
                v.push(P8::from_raw(raw));
            }
        }
        RootKind::Bool => {
            let val = u16::from_le_bytes([buf[base], buf[base + 1]]);
            let known = u16::from_le_bytes([buf[base + 2], buf[base + 3]]);
            let av = if known & (1 << i) != 0 {
                AV::Bool(val & (1 << i) != 0)
            } else {
                AV::UBool
            };
            if let Col::V(v) = &mut acc.cols[f.cell] {
                v.push(av);
            }
        }
        RootKind::Ival => {
            let lo = i32::from_le_bytes(buf[base + i * 4..base + i * 4 + 4].try_into().unwrap());
            let hi =
                i32::from_le_bytes(buf[base + 64 + i * 4..base + 64 + i * 4 + 4].try_into().unwrap());
            if let Col::I(v) = &mut acc.cols[f.cell] {
                v.push((P8::from_raw(lo), P8::from_raw(hi)));
            }
        }
        RootKind::Word => panic!("an output field cannot be a Word root"),
    }
}

/// The start room's assembled kernels, keyed by input shape hash, for one
/// rem-precision mode.
pub(crate) struct Registry {
    by_shape: HashMap<u64, AsmKernel>,
    /// The rung-agnostic / exact sets go through `boundary_exact`; the
    /// level-0 set through `boundary`.
    exact_boundary: bool,
}

impl Registry {
    pub fn len(&self) -> usize {
        self.by_shape.len()
    }

    /// Run `chunk` on the matching shape's kernel; `false` if no kernel binds
    /// (a miss) or the chunk declined.
    pub fn run_chunk(
        &self,
        chunk: &Rt2,
        ids: &runtime2::BoundaryIds,
        done: &mut Vec<Rt2>,
    ) -> bool {
        match self.by_shape.get(&chunk.shape_hash) {
            Some(k) => k.run(chunk, ids, done, self.exact_boundary),
            None => false,
        }
    }

    /// Retrace the start room and assemble every shape for one lattice set
    /// (`opts`). `root` is the repo root (Lua sources + cart). `decide =
    /// true` matches the Rust kernel so the assembled compute is the same
    /// fused graph. `exact_boundary` selects `boundary_exact` for the
    /// rung-agnostic / exact sets.
    pub fn build_for_start_room(
        root: &Path,
        opts: crate::trace::shapes::WalkOpts,
        exact_boundary: bool,
    ) -> Result<Registry> {
        let refs = crate::trace::kernel::lattice_kernel_refs(root, opts)
            .context("retracing the start room for ASM kernels")?;
        let mut by_shape = HashMap::new();
        for (si, r) in refs.iter().enumerate() {
            let shape = r.frame.in_rt2.shape_hash_of();
            let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
            let (fused, bodies, flat_roots, reprs) =
                crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
                    .with_context(|| format!("fusing shape {si} (hash {shape:#x})"))?;
            let (compiled, loaded) = crate::transpile::asm::compile_and_load_reprs(
                &fused,
                &flat_roots,
                &format!("k{shape:016x}"),
                &reprs,
            )
            .with_context(|| format!("assembling shape {si} (hash {shape:#x})"))?;

            // Map each fused body's roots onto flat root SLOTS. `flat_roots`
            // is the bodies' roots concatenated in order, and `compile` keeps
            // root k at slot k, so a running offset locates each body.
            let mut off = 0usize;
            let mut asm_bodies = Vec::with_capacity(bodies.len());
            for b in &bodies {
                let nfields = b.roots.len() - 2; // roots = [fields.., ok, live]
                let outputs = &r.bound.outcomes[b.outcome].outputs;
                anyhow::ensure!(
                    outputs.len() == nfields,
                    "shape {si} outcome {}: {} outputs vs {} field roots",
                    b.outcome,
                    outputs.len(),
                    nfields
                );
                let fields = (0..nfields)
                    .map(|j| AsmField {
                        cell: outputs[j].0 as usize,
                        root: off + j,
                        kind: compiled.root_kinds[off + j],
                    })
                    .collect();
                asm_bodies.push(AsmBody {
                    outcome: b.outcome,
                    fields,
                    ok_root: off + nfields,
                    live_root: off + nfields + 1,
                });
                off += b.roots.len();
            }

            // Per-outcome acc templates, from the traced output structure +
            // the OutField decisions (uniform-const vs varying-by-type) +
            // the UBool cells - the runtime equivalent of the generated
            // `acc{i}`.
            let acc_templates = (0..r.bound.outcomes.len())
                .map(|oi| acc_template(r, oi))
                .collect::<Result<Vec<_>>>()?;

            if by_shape
                .insert(shape, AsmKernel { loaded, compiled, bodies: asm_bodies, acc_templates })
                .is_some()
            {
                anyhow::bail!("two start-room shapes hash to {shape:#x}");
            }
        }
        Ok(Registry { by_shape, exact_boundary })
    }
}

/// The accumulator recipe for one outcome, mirroring the generated `acc{i}`:
/// the outcome's structural template, each output field as a uniform
/// constant (`konst_av`) or an empty typed column (by `ty`), and the UBool
/// cells.
fn acc_template(r: &crate::trace::kernel::Reference, oi: usize) -> Result<AccTemplate> {
    let skeleton = reshape(&r.frame.outs[oi].rt2, 0);
    let mut inits: Vec<(usize, ColInit)> = Vec::new();
    for f in &r.lowered.outs[oi].fields {
        let cell = f.cell as usize;
        if let Some(av) = f.konst_av {
            inits.push((cell, ColInit::Uniform(av)));
        } else {
            inits.push((
                cell,
                match f.ty {
                    "ZN" => ColInit::EmptyNum,
                    "ZB" => ColInit::EmptyBool,
                    "ZI" => ColInit::EmptyIval,
                    other => anyhow::bail!("outcome {oi} field {cell}: output type {other}"),
                },
            ));
        }
    }
    for &cell in &r.frame.outs[oi].ubool_cells {
        inits.push((cell as usize, ColInit::Uniform(AV::UBool)));
    }
    Ok(AccTemplate { skeleton, inits })
}

/// The process-wide registry, built once on a big stack (the retrace's init
/// interpret recurses deeper than a worker thread's default). The ASM
/// kernels are THE kernel backend now, so this always builds when the
/// compiled engine runs a chunk; `CELESTE_NO_ASM_KERNELS` opts out (pure
/// reference, for debugging). The root is `CELESTE_ROOT` or the CWD.
pub(crate) fn registry() -> Option<&'static Registry> {
    static REG: std::sync::OnceLock<Option<Registry>> = std::sync::OnceLock::new();
    REG.get_or_init(|| {
        if std::env::var_os("CELESTE_NO_ASM_KERNELS").is_some() {
            return None;
        }
        let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
        // Build for the active rem-precision mode, matching the generated
        // set the engine would otherwise dispatch (dispatch::traced_mode).
        use crate::trace::shapes::WalkOpts;
        let (opts, exact) = match super::dispatch::traced_mode() {
            super::dispatch::TracedMode::Level0 => (WalkOpts::LEVEL0, false),
            super::dispatch::TracedMode::Level0Agnostic => (WalkOpts::LADDER, true),
            super::dispatch::TracedMode::ExactRem => (WalkOpts::EXACT, true),
        };
        let built = std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(move || Registry::build_for_start_room(Path::new(&root), opts, exact))
            .expect("spawn asm-kernel builder")
            .join()
            .expect("asm-kernel builder panicked");
        match built {
            Ok(reg) => {
                eprintln!(
                    "ASM kernels ENABLED ({:?}): {} start-room shapes assembled",
                    super::dispatch::traced_mode(),
                    reg.len()
                );
                Some(reg)
            }
            Err(e) => panic!("building ASM kernels: {e:#}"),
        }
    })
    .as_ref()
}

/// A stable structural hash of a graph's nodes (op + args), so a change to
/// what the kernels COMPUTE changes the fingerprint.
fn graph_fingerprint(g: &crate::transpile::graph::Graph) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = rustc_hash::FxHasher::default();
    for id in 0..g.len() as crate::transpile::graph::NodeId {
        let n = g.get(id);
        n.op.hash(&mut h);
        n.args.hash(&mut h);
    }
    h.finish()
}

/// The ASM engine's content identity, for the checkpoint fingerprint
/// (`compiled_engine`). MODE-INDEPENDENT on purpose: the rem rung is already
/// a separate fingerprint component (`CampaignConfig::precision`), so this
/// only has to (a) differ from the interpreter / any other engine and (b)
/// change when the assembled COMPUTE changes. It hashes the fused graphs of
/// ALL THREE lattice sets, so a codegen or tracer change in any of them
/// moves it, and every process computes the same value regardless of which
/// rung it runs (the band loader recomputes a previous level's fingerprint
/// in-process and must match). Retraced once, on a big stack, then cached.
pub(crate) fn engine_fingerprint() -> u64 {
    static FP: std::sync::OnceLock<u64> = std::sync::OnceLock::new();
    *FP.get_or_init(|| {
        use crate::trace::shapes::WalkOpts;
        let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
        std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(move || {
                let mut acc: u64 = 0xa500_f16e_1230_0001;
                for (tag, opts) in [
                    (1u64, WalkOpts::LEVEL0),
                    (2, WalkOpts::LADDER),
                    (3, WalkOpts::EXACT),
                ] {
                    let refs = crate::trace::kernel::lattice_kernel_refs(Path::new(&root), opts)
                        .expect("retrace for ASM fingerprint");
                    for r in &refs {
                        let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
                        let (fused, _, _, _) = crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
                            .expect("fuse for ASM fingerprint");
                        // Order-independent over shapes: XOR each shape's
                        // (graph hash mixed with its shape hash and set tag).
                        let shape = r.frame.in_rt2.shape_hash_of();
                        acc ^= runtime2::mix64(graph_fingerprint(&fused) ^ shape.rotate_left(17) ^ tag);
                    }
                }
                acc
            })
            .expect("spawn ASM fingerprint builder")
            .join()
            .expect("ASM fingerprint builder panicked")
    })
}

/// Run one chunk through the ASM kernels. `false` = miss or declined (the
/// caller routes to the reference path).
pub(crate) fn run_chunk(
    chunk: &Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<Rt2>,
) -> bool {
    match registry() {
        Some(reg) => reg.run_chunk(chunk, ids, done),
        None => false,
    }
}
