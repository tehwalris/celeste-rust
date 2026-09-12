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
/// slot the assembly wrote it to, and how to read that slot. Which fields
/// enter the per-row key is decided at build (`build_one_shape`): a per-row
/// column (`konst_av` = None) that the boundary does not widen to uniform
/// (`widen_uniform` = None); the rest are in the outcome's `part`. Every
/// non-konst field is still PUSHED (`push_field` no-ops on the konst
/// `Col::U` cells).
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
    /// The two row-key word roots: `Σ cell_mix` over the body's varying
    /// fields, per half, computed in the kernel (`Op::CellMix`/`AddW`).
    key_roots: (usize, usize),
    /// Where an emitted row's player x, player y, room x, room y come from
    /// (`search::pos_graph::block_cells`' inputs), so the append step can
    /// compute the row's position cell straight off the output buffer.
    /// `None`: this outcome has no player object, or its `x`/`y` is not a
    /// plain number (every row is `NO_CELL`, as `block_cells` says).
    pos: Option<[PosSrc; 4]>,
}

/// One coordinate of an emitted row, as a whole pixel: read from a numeric
/// output root of the body (its byte base in the output buffer), or a
/// constant of the outcome's template.
#[derive(Clone, Copy)]
enum PosSrc {
    Root(usize),
    Konst(i16),
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
/// column initializers, so a fresh acc is `reshape` + apply - and the
/// boundary's per-outcome constants, so the append step can hand back a
/// block that IS at the boundary (canonical structure, exact row keys)
/// without running `Rt2::boundary` over it.
struct AccTemplate {
    skeleton: Rt2,
    inits: Vec<(usize, ColInit)>,
    /// The skeleton's canonical structure hash - the block's shape key.
    shape_hash: u64,
    /// The boundary key's uniform part `(part1, part2)`
    /// (`Rt2::boundary_finish`): the shape hash plus every cell that is
    /// uniform AT THE BOUNDARY - pointers, nils, konst outputs, UBool
    /// cells, and the level-0 widened-to-uniform cells (rem, timers) at
    /// their WIDENED value. A row's key is `mix64(part + h)` with `h` the
    /// per-row fold over the remaining (varying) fields.
    part: (u64, u64),
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
    /// DEBUG (CELESTE_ASM_EVAL_CHECK): the fused graph + its flat roots +
    /// the room, so `run` can re-evaluate the SAME fused graph with the pure
    /// interval evaluator (`eval_narrow_top_in`) per lane and diff it against the
    /// assembled kernel's output. Separates an ASM-codegen bug (asm != eval)
    /// from a fused-graph bug (asm == eval, both != interpreter).
    fused: crate::transpile::graph::Graph,
    flat_roots: Vec<crate::transpile::graph::NodeId>,
    room: Room,
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
        cell_in: &[u32],
        sink: &mut crate::frame::ForwardSink,
        exact: bool,
    ) -> bool {
        debug_assert_eq!(cell_in.len(), chunk.width, "one input cell per input row");
        let start = crate::game_runner::start_room();
        // Consecutive emissions mostly repeat one (input cell, output cell)
        // pair; skip the set insert for those.
        let mut last_edge = (u32::MAX, u32::MAX);
        let mut accs: Vec<Rt2> = self.acc_templates.iter().map(|t| t.build()).collect();
        // Per-acc row keys, pushed alongside the rows: the exact boundary
        // key, `mix64(part + h)`.
        let mut keys: Vec<Vec<(u64, u64)>> = vec![Vec::new(); accs.len()];
        let mut inbuf = vec![0u8; self.compiled.input_bytes as usize];
        let mut outbuf = vec![0u8; self.compiled.n_roots * 128];
        let env = CollisionEnv { cart: &chunk.cart, cache: &chunk.cache };
        let ctx = AsmCtx::new(&env as *const CollisionEnv as *const c_void);
        // Within-chunk dedup, per outcome: the same row key (the boundary's,
        // via the (h1,h2) fold - the uniform `part` is constant per outcome,
        // so deduping on the fold is exactly deduping on the key) skips
        // materializing a row a sibling body already produced. This is the
        // generated kernel's `seen` set: without it the fused graph's many
        // configurations re-emit the same row ~76x before the boundary dedup
        // drops them. The boundary is still the final authority.
        let mut seen: Vec<celeste_engine::kernel::RowSet> =
            (0..self.acc_templates.len())
                .map(|_| celeste_engine::kernel::RowSet::new())
                .collect();
        // Debug: CELESTE_ASM_NO_SEEN keeps the seen fold running (so its cost
        // is unchanged) but never drops a row, to isolate the dedup from the
        // compute when a divergence appears.
        let no_seen = {
            static NS: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *NS.get_or_init(|| std::env::var_os("CELESTE_ASM_NO_SEEN").is_some())
        };
        // Pure-kernel throughput floor (CELESTE_KERNEL_DRYRUN=1): pack the
        // inputs and call the kernel, then discard - no fold, no seen-dedup,
        // no materialize, no boundary. Produces no rows, so it is a
        // MEASUREMENT MODE ONLY (the frame comes out empty). Isolates the raw
        // kernel compute from all the dedup/store machinery around it.
        let dryrun = {
            static DR: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *DR.get_or_init(|| std::env::var_os("CELESTE_KERNEL_DRYRUN").is_some())
        };

        CALL_STATS[0].fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        CALL_STATS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        CALL_STATS[2].fetch_add(chunk.width.div_ceil(16) as u64 * 16, std::sync::atomic::Ordering::Relaxed);
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
            if dryrun {
                lo += n;
                continue;
            }
            if eval_check_on() {
                self.eval_check(chunk, lo, n, &outbuf);
            }
            let valid = ((1u32 << n) - 1) as u16;
            if liveok_on() {
                // Per-lane coverage census: OR of live&ok over all bodies
                // (lane kept by SOME outcome), and OR of live&!ok (lane
                // DECLINED by some outcome). A lane that is neither kept nor
                // declined is DROPPED as not-live - the graph considers it dead.
                let (mut kept, mut declined, mut any_live) = (0u16, 0u16, 0u16);
                for body in &self.bodies {
                    let ok = read_zb_holds(&outbuf, body.ok_root);
                    let live = read_zb_holds(&outbuf, body.live_root);
                    kept |= live & ok & valid;
                    declined |= live & !ok & valid;
                    any_live |= live & valid;
                }
                let dropped = valid & !kept & !declined;
                eprintln!(
                    "[liveok] slice lanes={n} valid={valid:04x} kept={kept:04x} \
                     declined={declined:04x} dropped(not-live)={dropped:04x} any_live={any_live:04x}"
                );
            }
            for body in &self.bodies {
                // `ok`/`live` are tri-state ZB masks; the kernel keeps a lane
                // only where they are KNOWN-TRUE (val & known - `zb_holds`,
                // exactly what the generated `frame` applied). Reading `val`
                // alone dropped the decidedness check: an UNKNOWN condition
                // (a comparison landing inside an interval, e.g. a rem-derived
                // guard at a fine rem rung) would silently use the garbage
                // `val` bit instead of declining. A lane whose `ok` is unknown
                // must fall to the reference, so the trace's failure to
                // fork/decide it surfaces instead of producing a coarse row.
                let ok = read_zb_holds(&outbuf, body.ok_root);
                let live = read_zb_holds(&outbuf, body.live_root);
                if live & !ok & valid != 0 {
                    // Declined: a live lane the kernel could not keep (or
                    // could not DECIDE). Drop and let the whole chunk take the
                    // reference path.
                    return false;
                }
                let take = live & ok & valid;
                if take == 0 {
                    continue;
                }
                let acc = &mut accs[body.outcome];
                let seen = &mut seen[body.outcome];
                for i in 0..n {
                    if take & (1 << i) == 0 {
                        continue;
                    }
                    // The row's (h1,h2) fold over the varying, non-widened
                    // cells, computed by the kernel (the body's key roots).
                    let h1 = read_word(&outbuf, body.key_roots.0, i);
                    let h2 = read_word(&outbuf, body.key_roots.1, i);
                    let part = self.acc_templates[body.outcome].part;
                    let key = (
                        runtime2::mix64(part.0.wrapping_add(h1)),
                        runtime2::mix64(part.1.wrapping_add(h2)),
                    );
                    let cin = cell_in[lo + i];
                    // EMISSION-TIME PROVENANCE (plans/buckets.md). The
                    // source of this row is lane `i` of this slice, and
                    // everything that needs to know is told right here:
                    // the pos-graph edge, the door dedup - or, in backward
                    // mode, the mark on the input row.
                    if let Some(targets) = sink.targets {
                        // BACKWARD: does this output hit a marked state? Then
                        // input row `lo + i` is marked. Nothing is
                        // materialized. The `seen` tag is the hit bit, so a
                        // re-emission from another input row marks it too.
                        if !no_seen {
                            if let Some(prev) = seen.insert_tagged(key, 0) {
                                if prev != 0 {
                                    sink.hits[lo + i] = true;
                                }
                                continue;
                            }
                        }
                        sink.emitted += 1;
                        let cout = cell_out(body, &outbuf, i, start);
                        if targets.contains(&(key.0, key.1, cout)) {
                            sink.hits[lo + i] = true;
                            if !no_seen {
                                seen.set_last_tag(1);
                            }
                        }
                        continue;
                    }
                    if !no_seen {
                        if let Some(first_cin) = seen.insert_tagged(key, cin) {
                            // A re-emission of a row this call already
                            // produced. Nothing to materialize - but if it
                            // came from a DIFFERENT input cell, that is a
                            // pos-graph edge the first emission did not record.
                            if sink.edges_on && first_cin != cin {
                                sink.edges.insert((cin, cell_out(body, &outbuf, i, start)));
                            }
                            continue;
                        }
                    }
                    let cout = cell_out(body, &outbuf, i, start);
                    if sink.edges_on && last_edge != (cin, cout) {
                        last_edge = (cin, cout);
                        sink.edges.insert((cin, cout));
                    }
                    sink.emitted += 1;
                    if let Some(v) = sink.visited.as_deref_mut() {
                        if !v.insert(key, cout) {
                            continue;
                        }
                    }
                    for f in &body.fields {
                        push_field(acc, f, &outbuf, i);
                    }
                    keys[body.outcome].push(key);
                    acc.width += 1;
                }
            }
            lo += n;
        }

        for (oi, mut acc) in accs.into_iter().enumerate() {
            if acc.width == 0 {
                continue;
            }
            // The acc IS at the boundary: its structure is the outcome's
            // canonical one (`bind::structure_of`, checked below), the
            // graph applied every level-0 widening (`trace::widen`), `seen`
            // deduped on the key, and the key itself was computed per row
            // above. So no `Rt2::boundary` here - just the two fields it
            // would have set.
            acc.shape_hash = self.acc_templates[oi].shape_hash;
            acc.row_keys = std::mem::take(&mut keys[oi]);
            if key_check_on() {
                self.check_against_boundary(&acc, ids, exact, oi);
            }
            sink.out.push(acc);
        }
        true
    }

    /// `CELESTE_KERNEL_KEY_CHECK=1`: run the real `Rt2::boundary` over a
    /// copy of the finished acc and demand it changes NOTHING - same
    /// structure, same shape hash, same row keys, same width (no
    /// within-block duplicates left for its dedup). This is the gate that
    /// the append step's shortcut is exact; a difference here would
    /// silently change what the search dedups on.
    fn check_against_boundary(&self, acc: &Rt2, ids: &runtime2::BoundaryIds, exact: bool, oi: usize) {
        let mut b = acc.clone_block();
        if exact {
            b.boundary_exact();
        } else {
            b.boundary(ids);
        }
        assert!(
            b.width == acc.width
                && b.shape_hash == acc.shape_hash
                && b.structure == acc.structure
                && b.row_keys == acc.row_keys,
            "KERNEL KEY CHECK: outcome {oi}: the boundary disagrees with the append step \
             (width {} vs {}, shape {:#x} vs {:#x}, structure {}, keys {})",
            b.width,
            acc.width,
            b.shape_hash,
            acc.shape_hash,
            if b.structure == acc.structure { "same" } else { "DIFFERENT" },
            if b.row_keys == acc.row_keys { "same" } else { "DIFFERENT" },
        );
    }

    /// DEBUG: re-evaluate the fused graph with the pure interval evaluator
    /// for lanes `[lo, lo+n)` and diff its per-field outputs against the
    /// assembled kernel's `outbuf`. An `asm != eval` mismatch is an ASM
    /// codegen bug; agreement means the fused graph itself is what diverges
    /// from the interpreter. Prints at most a few mismatches per chunk.
    fn eval_check(&self, chunk: &Rt2, lo: usize, n: usize, outbuf: &[u8]) {
        use crate::transpile::graph::Val;
        static PRINTED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        for i in 0..n {
            // Build the per-lane input cells from the block columns.
            let mut cells: std::collections::HashMap<u32, Val> = Default::default();
            for &cell in &self.compiled.input_cells {
                let v = match chunk.cols[cell as usize].at(lo + i) {
                    AV::Num(p) => Val::Num(crate::pico8_num::Pico8NumInterval::new(p, p)),
                    AV::Ival(a, b) => Val::Num(crate::pico8_num::Pico8NumInterval::new(a, b)),
                    AV::Bool(b) => Val::Bool(Some(b)),
                    AV::UBool => Val::Bool(None),
                    _ => continue,
                };
                cells.insert(cell, v);
            }
            let vals = match self.fused.eval_narrow_top_in(&cells, &self.room) {
                Ok(v) => v,
                Err(e) => {
                    if PRINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed) < 4 {
                        eprintln!("[eval-check] lane {} eval err: {e:#}", lo + i);
                    }
                    return;
                }
            };
            for body in &self.bodies {
                for f in &body.fields {
                    let asm = read_field_av(f, outbuf, i);
                    let node = self.flat_roots[f.root];
                    let ev = vals[node as usize];
                    let agree = match (asm, ev) {
                        (AV::Num(p), Val::Num(iv)) => {
                            p.as_raw_u32() == iv.low.as_raw_u32()
                                && iv.low.as_raw_u32() == iv.high.as_raw_u32()
                        }
                        (AV::Ival(a, b), Val::Num(iv)) => {
                            a.as_raw_u32() == iv.low.as_raw_u32()
                                && b.as_raw_u32() == iv.high.as_raw_u32()
                        }
                        (AV::Bool(x), Val::Bool(Some(y))) => x == y,
                        (AV::UBool, Val::Bool(None)) => true,
                        _ => false,
                    };
                    if !agree && PRINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed) < 24 {
                        eprintln!(
                            "[eval-check] lane {} cell {} root {}: ASM {:x?} vs EVAL {:x?}",
                            lo + i,
                            f.cell,
                            f.root,
                            asm,
                            ev,
                        );
                    }
                }
            }
        }
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

/// DEBUG gate for the per-lane fused-graph re-evaluation (`eval_check`).
fn liveok_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_ASM_LIVEOK").is_some())
}

/// `CELESTE_KERNEL_KEY_CHECK=1`: verify every finished acc against
/// `Rt2::boundary` (see `check_against_boundary`).
fn key_check_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_KERNEL_KEY_CHECK").is_some())
}

fn eval_check_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_ASM_EVAL_CHECK").is_some())
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

/// `zb_holds` of a Bool root: the lanes where it is KNOWN-TRUE (val & known).
/// `val` at +0, `known` at +2 of the 128-byte slot.
fn read_zb_holds(buf: &[u8], root: usize) -> u16 {
    let off = root * 128;
    let val = u16::from_le_bytes([buf[off], buf[off + 1]]);
    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
    val & known
}

/// The boundary's row-key mix seeds (see `runtime2::boundary_finish`); the
/// append folds the same cell_mix so its dedup key equals the boundary's.
use celeste_engine::runtime2::{KEY_SEED1, KEY_SEED2};

/// Lane `i` of a word root: lanes 0-7 in the slot's first 64 bytes, 8-15
/// in the second.
#[inline]
fn read_word(buf: &[u8], root: usize, i: usize) -> u64 {
    let base = root * 128 + (i / 8) * 64 + (i % 8) * 8;
    u64::from_le_bytes(buf[base..base + 8].try_into().unwrap())
}

/// The `AV` a field root holds for lane `i` (for the dedup fold).
fn read_field_av(f: &AsmField, buf: &[u8], i: usize) -> AV {
    let base = f.root * 128;
    match f.kind {
        RootKind::Num => {
            AV::Num(P8::from_raw(i32::from_le_bytes(
                buf[base + i * 4..base + i * 4 + 4].try_into().unwrap(),
            )))
        }
        RootKind::Bool => {
            let val = u16::from_le_bytes([buf[base], buf[base + 1]]);
            let known = u16::from_le_bytes([buf[base + 2], buf[base + 3]]);
            if known & (1 << i) != 0 {
                AV::Bool(val & (1 << i) != 0)
            } else {
                AV::UBool
            }
        }
        RootKind::Ival => {
            let lo = i32::from_le_bytes(buf[base + i * 4..base + i * 4 + 4].try_into().unwrap());
            let hi =
                i32::from_le_bytes(buf[base + 64 + i * 4..base + 64 + i * 4 + 4].try_into().unwrap());
            AV::Ival(P8::from_raw(lo), P8::from_raw(hi))
        }
        RootKind::Word => unreachable!("an output field is never a word root"),
    }
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
        RootKind::Word => unreachable!("an output field is never a word root"),
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
        cell_in: &[u32],
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        match self.by_shape.get(&chunk.shape_hash) {
            Some(k) => k.run(chunk, ids, cell_in, sink, self.exact_boundary),
            None => {
                // Diagnose a coverage gap: which shape has no assembled
                // kernel. Printed once per distinct missing shape.
                static SEEN: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<u64>>> =
                    std::sync::OnceLock::new();
                let seen = SEEN.get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()));
                if seen.lock().unwrap().insert(chunk.shape_hash) {
                    eprintln!(
                        "[asm] MISS: no kernel for shape {:#018x} ({} lanes); registry has {} shapes: {:?}",
                        chunk.shape_hash,
                        chunk.width,
                        self.by_shape.len(),
                        self.by_shape.keys().map(|k| format!("{k:#018x}")).collect::<Vec<_>>(),
                    );
                }
                false
            }
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
        let t_trace = std::time::Instant::now();
        let refs = crate::trace::kernel::lattice_kernel_refs(root, opts)
            .context("retracing the start room for ASM kernels")?;
        let trace_s = t_trace.elapsed().as_secs_f64();
        let t_asm = std::time::Instant::now();
        // Assemble every shape IN PARALLEL (`build_one_shape`: fuse ->
        // specialize -> gcc -> dlopen, independent per shape). The traced
        // graph is `Send + Sync` now (`Value::Str` is `Arc`), so workers
        // share `&refs` and steal shapes off one atomic index; each gets a
        // big stack because the fuse recurses. Insertion stays serial and
        // ordered by shape index so the dup error is deterministic.
        let n_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(refs.len().max(1));
        let next = std::sync::atomic::AtomicUsize::new(0);
        let refs_ref = &refs;
        let mut built: Vec<(usize, Result<(u64, AsmKernel)>)> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..n_workers)
                .map(|_| {
                    let next = &next;
                    std::thread::Builder::new()
                        .stack_size(128 * 1024 * 1024)
                        .spawn_scoped(scope, move || {
                            let mut out = Vec::new();
                            loop {
                                let si =
                                    next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                                if si >= refs_ref.len() {
                                    break;
                                }
                                out.push((si, build_one_shape(&refs_ref[si], si)));
                            }
                            out
                        })
                        .expect("spawn asm shape builder")
                })
                .collect();
            handles.into_iter().flat_map(|h| h.join().unwrap()).collect()
        });
        built.sort_by_key(|(si, _)| *si);
        let mut by_shape = HashMap::new();
        for (_si, res) in built {
            let (shape, kernel) = res?;
            if by_shape.insert(shape, kernel).is_some() {
                anyhow::bail!("two start-room shapes hash to {shape:#x}");
            }
        }
        eprintln!(
            "[asm build] {} shapes: trace {:.2}s, assemble {:.2}s ({} workers)",
            by_shape.len(),
            trace_s,
            t_asm.elapsed().as_secs_f64(),
            n_workers,
        );
        Ok(Registry { by_shape, exact_boundary })
    }
}

/// Assemble ONE start-room shape: fuse its graph, gcc + dlopen it, and map
/// its bodies' roots onto flat slots. Independent per shape, so
/// `build_for_start_room` runs these in parallel.
fn build_one_shape(
    r: &crate::trace::kernel::Reference,
    si: usize,
) -> Result<(u64, AsmKernel)> {
    let shape = r.frame.in_rt2.shape_hash_of();
    let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
    let (mut fused, bodies, mut flat_roots, reprs) =
        crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
            .with_context(|| format!("fusing shape {si} (hash {shape:#x})"))?;
    // THE ROW KEY, AS GRAPH ROOTS. Per body, the per-lane sum of
    // `cell_mix` over its varying fields (a per-row column that the
    // boundary does not widen to uniform), one node chain per half,
    // appended AFTER every body's own roots so the field/ok/live slots
    // keep their layout. `mix64(part + this)` is the exact boundary key
    // (`acc_template` computes `part`; `check_against_boundary` gates it).
    let mut key_roots: Vec<(usize, usize)> = Vec::with_capacity(bodies.len());
    for b in &bodies {
        use crate::transpile::graph::Op;
        let nfields = b.roots.len() - 2;
        let outputs = &r.bound.outcomes[b.outcome].outputs;
        let out_fields = &r.lowered.outs[b.outcome].fields;
        let (mut h1, mut h2) = (fused.leaf(Op::Word(0)), fused.leaf(Op::Word(0)));
        for j in 0..nfields {
            if out_fields[j].konst_av.is_some() || out_fields[j].widen_uniform.is_some() {
                continue;
            }
            let cell = outputs[j].0;
            let m1 = fused.add(Op::CellMix(cell, 0), vec![b.roots[j]]);
            let m2 = fused.add(Op::CellMix(cell, 1), vec![b.roots[j]]);
            h1 = fused.add(Op::AddW, vec![h1, m1]);
            h2 = fused.add(Op::AddW, vec![h2, m2]);
        }
        key_roots.push((flat_roots.len(), flat_roots.len() + 1));
        flat_roots.push(h1);
        flat_roots.push(h2);
    }
    let (compiled, loaded) = crate::transpile::asm::compile_and_load_reprs(
        &fused,
        &flat_roots,
        &format!("k{shape:016x}"),
        &reprs,
    )
    .with_context(|| format!("assembling shape {si} (hash {shape:#x})"))?;

    // Map each fused body's roots onto flat root SLOTS. `flat_roots` is the
    // bodies' roots concatenated in order, and `compile` keeps root k at slot
    // k, so a running offset locates each body.
    let mut off = 0usize;
    let mut asm_bodies = Vec::with_capacity(bodies.len());
    for (bi, b) in bodies.iter().enumerate() {
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
            key_roots: key_roots[bi],
            pos: None,
        });
        off += b.roots.len();
    }

    let acc_templates = (0..r.bound.outcomes.len())
        .map(|oi| acc_template(r, oi))
        .collect::<Result<Vec<_>>>()?;
    for b in asm_bodies.iter_mut() {
        b.pos = pos_sources(&acc_templates[b.outcome].build(), &b.fields)?;
    }

    Ok((
        shape,
        AsmKernel {
            loaded,
            compiled,
            bodies: asm_bodies,
            acc_templates,
            fused,
            flat_roots,
            room,
        },
    ))
}

/// Where a body's emitted rows carry their position: the player object's
/// `x`/`y` and the room's `x`/`y`, each either a numeric output root of the
/// body or a numeric constant of the outcome's template. `None` when the
/// outcome has no player object (the death countdown) or its `x`/`y` is
/// not a plain number - every row is then at `NO_CELL`, the
/// `pos_graph::block_cells` rule. A non-numeric room coordinate is an
/// error, as there.
fn pos_sources(template: &Rt2, fields: &[AsmField]) -> Result<Option<[PosSrc; 4]>> {
    let ids = crate::compiled::ids();
    let Some(obj) = crate::search::pos_graph::player_object(template) else {
        return Ok(None);
    };
    let room = template
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow::anyhow!("outcome template has no `room` global"))?;
    // `None` = not a plain number in every row.
    let src_of = |cell: u32| -> Result<Option<PosSrc>> {
        if let Some(f) = fields.iter().find(|f| f.cell == cell as usize) {
            return Ok(match f.kind {
                RootKind::Num => Some(PosSrc::Root(f.root * 128)),
                _ => None,
            });
        }
        match template.cols[cell as usize] {
            Col::U(AV::Num(n)) => Ok(Some(PosSrc::Konst(n.whole_part_as_i16()))),
            Col::U(_) => Ok(None),
            _ => anyhow::bail!("position cell {cell} is neither an output field nor a constant"),
        }
    };
    let cell = |o: u32, f: u32, what: &str| -> Result<u32> {
        template
            .obj_field_cell(o, f)
            .ok_or_else(|| anyhow::anyhow!("outcome template: no `{what}` field"))
    };
    let (Some(rx), Some(ry)) =
        (src_of(cell(room, ids.f_x, "room.x")?)?, src_of(cell(room, ids.f_y, "room.y")?)?)
    else {
        anyhow::bail!("outcome template: room.x/room.y are not numbers");
    };
    let (Some(px), Some(py)) =
        (src_of(cell(obj, ids.f_x, "player.x")?)?, src_of(cell(obj, ids.f_y, "player.y")?)?)
    else {
        return Ok(None);
    };
    Ok(Some([px, py, rx, ry]))
}

/// The position cell of lane `i` of a body's output, read straight off the
/// output buffer. `Pico8Num::whole_part_as_i16` is `raw >> 16`.
#[inline]
fn cell_out(body: &AsmBody, buf: &[u8], i: usize, start: (i16, i16)) -> u32 {
    use crate::search::pos_graph::{cell_of, NO_CELL};
    let Some(srcs) = &body.pos else {
        return NO_CELL;
    };
    let read = |s: PosSrc| -> i16 {
        match s {
            PosSrc::Root(base) => {
                let o = base + i * 4;
                (i32::from_le_bytes(buf[o..o + 4].try_into().unwrap()) >> 16) as i16
            }
            PosSrc::Konst(v) => v,
        }
    };
    let (px, py, rx, ry) = (read(srcs[0]), read(srcs[1]), read(srcs[2]), read(srcs[3]));
    cell_of(
        px as i32 + (rx - start.0) as i32 * 128,
        py as i32 + (ry - start.1) as i32 * 128,
    )
    .unwrap_or_else(|e| panic!("emitted row: {e}"))
}

/// The accumulator recipe for one outcome: the outcome's structural
/// template, each output field as a uniform constant (`konst_av`) or an
/// empty typed column (by `ty`), the UBool cells - and the boundary's
/// per-outcome constants (`shape_hash`, `part`).
fn acc_template(r: &crate::trace::kernel::Reference, oi: usize) -> Result<AccTemplate> {
    let skeleton = reshape(&r.frame.outs[oi].rt2, 0);
    // The structure must be canonical already: the boundary's renumbering
    // is a no-op on it. Checked against the one implementation of the rule
    // rather than trusted.
    {
        let mut probe = skeleton.clone_block();
        probe.canonicalize_ids();
        anyhow::ensure!(
            probe.structure == skeleton.structure,
            "outcome {oi}: the traced output structure is not canonical"
        );
    }
    let shape_hash = skeleton.shape_hash_of();
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
    let mut t = AccTemplate { skeleton, inits, shape_hash, part: (0, 0) };
    // The uniform part of the key, exactly as `boundary_finish` folds it:
    // every `Cell2::Val` cell that is `Col::U` at the boundary. Non-output
    // cells are uniform in the skeleton (pointers, nils); konst and UBool
    // outputs are uniform by `inits`; the level-0 widened cells (rem,
    // timers) are pushed per row but the boundary replaces them with the
    // widened uniform value, so they fold in at that value.
    let widen = &r.bound.outcomes[oi].widen;
    let empty = t.build();
    let mut part1: u64 = shape_hash;
    let mut part2: u64 = 0xa076_1d64_78bd_642f ^ shape_hash;
    for (c, cell) in empty.structure.iter().enumerate() {
        if !matches!(cell, celeste_engine::runtime2::Cell2::Val) {
            continue;
        }
        let uniform = match widen.iter().find(|(wc, _)| *wc as usize == c) {
            Some((_, av)) => Some(*av),
            None => match empty.cols[c] {
                Col::U(v) => Some(v),
                _ => None,
            },
        };
        if let Some(v) = uniform {
            part1 = part1.wrapping_add(runtime2::cell_mix(c as u64, v, KEY_SEED1));
            part2 = part2.wrapping_add(runtime2::cell_mix(c as u64, v, KEY_SEED2));
        }
    }
    t.part = (part1, part2);
    Ok(t)
}

/// The kernel set for the CURRENT rung, built once per rung on a big stack
/// (the retrace's init interpret recurses deeper than a worker thread's
/// default). The root is `CELESTE_ROOT` or the CWD.
///
/// PER-RUNG cache (not a single process-wide set): with the widening in the
/// graph the ladder kernels specialize to the active rem precision, so an
/// IN-PROCESS ladder that walks rungs (`rewrite ladder`) needs a distinct
/// set per rung - a single `OnceLock` would freeze it at level 0's set and
/// serve the wrong kernels to every rung above. Indexed by rem precision
/// (Bits(0..15) -> 0..15, Exact/Bits(>=16) -> 16); each slot is its own
/// lock-free `OnceLock`, so per-chunk dispatch pays one atomic load once the
/// rung is built. `traced_mode()` (which the build respects) is a function
/// of the precision plus the process-constant `CELESTE_TRACED_SET` override,
/// so precision alone keys the set within a process. `ladder.sh`'s
/// per-process rungs no longer buy anything the in-process cache does not.
/// Kernel call shape: [0] calls, [1] rows offered, [2] slice-lanes executed
/// (16 per slice, so `[2] - [1]` is the padding). The per-call fixed cost
/// (accumulators, buffers, seen sets, boundary) is paid once per [0].
static CALL_STATS: [std::sync::atomic::AtomicU64; 3] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Take (and reset) the call-shape counters.
pub(crate) fn take_call_stats() -> [u64; 3] {
    std::array::from_fn(|i| CALL_STATS[i].swap(0, std::sync::atomic::Ordering::Relaxed))
}

pub(crate) fn registry() -> Option<&'static Registry> {
    use crate::interpreter::abstraction::{rem_precision_from_env, RemPrecision};
    static REGS: [std::sync::OnceLock<Option<Registry>>; 17] =
        [const { std::sync::OnceLock::new() }; 17];
    let slot = match rem_precision_from_env() {
        RemPrecision::Exact => 16,
        RemPrecision::Bits(b) => (b as usize).min(16),
    };
    REGS[slot].get_or_init(build_registry_for_current_rung).as_ref()
}

fn build_registry_for_current_rung() -> Option<Registry> {
    {
        let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
        // Build for the active rem-precision mode, matching the generated
        // set the engine would otherwise dispatch (dispatch::traced_mode).
        use crate::trace::shapes::WalkOpts;
        let (opts, exact) = match super::dispatch::traced_mode() {
            super::dispatch::TracedMode::Level0 => (WalkOpts::LEVEL0, false),
            super::dispatch::TracedMode::Level0Agnostic => {
                // Phase 1: the opt-in rung-specific variant bakes the rem
                // widening into the graph (`LADDER_WIDEN`), still through
                // `boundary_exact` (it emits the widened rem and keys the
                // emitted field). Default `LADDER` is unchanged.
                let opts = if super::dispatch::widen_in_graph() {
                    WalkOpts::LADDER_WIDEN
                } else {
                    WalkOpts::LADDER
                };
                (opts, true)
            }
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
    }
}

/// Run one chunk through the ASM kernels. `false` = miss or declined (the
/// caller routes to the reference path).
pub(crate) fn run_chunk(
    chunk: &Rt2,
    ids: &runtime2::BoundaryIds,
    cell_in: &[u32],
    sink: &mut crate::frame::ForwardSink,
) -> bool {
    match registry() {
        Some(reg) => reg.run_chunk(chunk, ids, cell_in, sink),
        None => false,
    }
}
