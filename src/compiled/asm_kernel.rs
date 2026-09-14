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
use celeste_engine::kernel::RowCache;
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
    /// This outcome's own skeleton (`build()`), kept for the uniform
    /// writes: cells this outcome holds uniform that the shape's union
    /// makes typed.
    own: Rt2,
    /// The SHAPE's skeleton: the union over every outcome of this shape
    /// in the registry of the cells any of them varies (plans/waves.md,
    /// invariant 3). Every queue and piece of the shape has these columns,
    /// so a flush appends column for column. Set by `Registry::unify`.
    union: std::sync::Arc<Rt2>,
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
    /// The traced frame's fork count (`Frame::forks`): binary splits the
    /// bodies enumerate.
    forks: u8,
    /// Per body: where its output roots (and its outcome's uniform cells)
    /// go in the shape's union columns. Built by `Registry::unify`.
    body_cols: Vec<BodyCols>,
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
    /// Run lanes `lanes` of one block. Per 16-lane slice: pack inputs, call
    /// the assembly, and for each body push its `live & ok` lanes into the
    /// sink's slot for (owner, outcome shape) - keyed, at the boundary, with
    /// the pos-graph edge recorded - or, in backward mode, mark the input
    /// rows whose outputs hit a target. A nonzero `live & !ok` (declined)
    /// fails the whole call (a coverage gap).
    fn run(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        debug_assert_eq!(cell_in.len(), chunk.width, "one input cell per input row");
        debug_assert!(lanes.end <= chunk.width);
        let start = crate::game_runner::start_room();
        // Consecutive emissions mostly repeat one (input cell, output cell)
        // pair; skip the set insert for those.
        let mut last_edge = (u32::MAX, u32::MAX);
        let views = self.input_views(chunk);
        let env = CollisionEnv { cart: &chunk.cart, cache: &chunk.cache };
        let ctx = AsmCtx::new(&env as *const CollisionEnv as *const c_void);
        // The call's scratch - buffers, the per-outcome staging, the dedup
        // cache - lives with the thread and is reused call after call
        // (`Scratch::take`); a fresh multi-megabyte allocation per unit
        // was a page fault per page.
        let mut sc = Scratch::take(self);
        let Scratch { inbuf, outbuf, .. } = &mut sc;
        // Within-call dedup (`ForwardSink::seen`, a `RowCache`): the fused
        // graph's configurations re-emit the same row from neighbouring
        // lanes ~8x over; the cache catches those (keyed by the row key,
        // which is unique across outcomes) and the owner's door catches
        // the rest. It lives in the sink because the flush writes each
        // row's id back into it (the edges).
        sink.seen.clear();
        let body_cols = &self.body_cols;
        // Pure-kernel throughput floor (CELESTE_KERNEL_DRYRUN=1): pack the
        // inputs and call the kernel, then discard - no dedup, no
        // materialize. Produces no rows, so it is a MEASUREMENT MODE ONLY
        // (the frame comes out empty).
        let dryrun = {
            static DR: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *DR.get_or_init(|| std::env::var_os("CELESTE_KERNEL_DRYRUN").is_some())
        };

        CALL_STATS[0].fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        CALL_STATS[1].fetch_add(lanes.len() as u64, std::sync::atomic::Ordering::Relaxed);
        CALL_STATS[2].fetch_add(lanes.len().div_ceil(16) as u64 * 16, std::sync::atomic::Ordering::Relaxed);
        // The call's utilization tallies (folded into `CALL_STATS` once at
        // the end): (body, slice) pairs evaluated and those with any taken
        // lane, lane emissions before and after the dedup cache.
        let (mut n_bodies, mut n_bodies_taken, mut n_lanes, mut n_unique) = (0u64, 0u64, 0u64, 0u64);
        let mut lo = lanes.start;
        while lo < lanes.end {
            let n = 16.min(lanes.end - lo);
            // The slice's first input id (ids are consecutive within a
            // block), and the lanes to skip (won rows: checkpointed, never
            // expanded).
            // The slice's predecessor GROUP: 64 consecutive input lanes
            // (ids are consecutive within a block, units start at
            // multiples of 64), so a state's producers from four adjacent
            // slices share one record. `lane0` is this slice's first lane
            // within the group.
            let slice_base: Option<u64> = sink.ids_in.map(|ids| ids[lo & !63]);
            let lane0 = lo & 63;
            let skip: u16 = match sink.skip_in {
                Some(sk) => (0..n).filter(|&i| sk[lo + i]).fold(0u16, |m, i| m | (1 << i)),
                None => 0,
            };
            self.pack_input(&views, lo, n, inbuf);
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
                self.eval_check(chunk, lo, n, outbuf);
            }
            let valid = ((1u32 << n) - 1) as u16;
            if liveok_on() {
                // Per-lane coverage census: OR of live&ok over all bodies
                // (lane kept by SOME outcome), and OR of live&!ok (lane
                // DECLINED by some outcome). A lane that is neither kept nor
                // declined is DROPPED as not-live - the graph considers it dead.
                let (mut kept, mut declined, mut any_live) = (0u16, 0u16, 0u16);
                for body in &self.bodies {
                    let ok = read_zb_holds(outbuf, body.ok_root);
                    let live = read_zb_holds(outbuf, body.live_root);
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
            // DIAGNOSTIC (CELESTE_BODYSETS=1): which bodies take each lane -
            // the per-lane body SET, whose distinct count over a frame is
            // how many kernels a dispatch keyed on it would need.
            let mut lane_sets: Option<Vec<Vec<u64>>> = bodysets_on().then(|| vec![vec![0u64; self.bodies.len().div_ceil(64)]; n]);
            // BACKWARD: lanes already known to reach a target; nothing more
            // to learn from them.
            let mut hit_lanes: u16 = 0;
            for (bi, (body, cols)) in self.bodies.iter().zip(body_cols).enumerate() {
                // `ok`/`live` are tri-state ZB masks; the kernel keeps a lane
                // only where they are KNOWN-TRUE (val & known - `zb_holds`,
                // exactly what the generated `frame` applied). Reading `val`
                // alone dropped the decidedness check: an UNKNOWN condition
                // (a comparison landing inside an interval, e.g. a rem-derived
                // guard at a fine rem rung) would silently use the garbage
                // `val` bit instead of declining. A lane whose `ok` is unknown
                // must fall to the reference, so the trace's failure to
                // fork/decide it surfaces instead of producing a coarse row.
                let ok = read_zb_holds(outbuf, body.ok_root);
                let live = read_zb_holds(outbuf, body.live_root);
                if live & !ok & valid != 0 {
                    // Declined: a live lane the kernel could not keep (or
                    // could not DECIDE). A coverage gap for the whole call -
                    // fatal in the caller, so say what was declined: the
                    // body's outcome and the lanes' player rem / spd (the
                    // interval slots the rungs fork on).
                    let declined = live & !ok & valid;
                    let ids = crate::compiled::ids();
                    let mut rows = String::new();
                    let mut m = declined;
                    while m != 0 {
                        let i = m.trailing_zeros() as usize;
                        m &= m - 1;
                        let lane = lo + i;
                        let mut desc = format!("lane {lane}:");
                        for obj in chunk.player_objects(ids) {
                            for (name, f) in [("rem", ids.f_rem), ("spd", ids.f_spd)] {
                                if let Some(pc) = chunk.obj_field_cell(obj, f) {
                                    if let Col::U(AV::Ptr(sub)) = chunk.cols[pc as usize] {
                                        for (axis, g) in [("x", ids.f_x), ("y", ids.f_y)] {
                                            if let Some(c) = chunk.obj_field_cell(sub, g) {
                                                desc.push_str(&format!(" {name}.{axis}={:?}", chunk.cols[c as usize].at(lane)));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        rows.push_str(&desc);
                        rows.push('\n');
                    }
                    eprintln!(
                        "[kernel] shape {:#x} body of outcome {} declined lanes {:#06x} (live {:#06x} ok {:#06x}):\n{rows}",
                        chunk.shape_hash, body.outcome, declined, live, ok
                    );
                    self.explain_ok(chunk, lo + declined.trailing_zeros() as usize, self.flat_roots[body.ok_root]);
                    return false;
                }
                let mut take = live & ok & valid & !skip;
                n_bodies += 1;
                if take == 0 {
                    continue;
                }
                if let Some(ls) = lane_sets.as_mut() {
                    let mut t = take;
                    while t != 0 {
                        let i = t.trailing_zeros() as usize;
                        t &= t - 1;
                        ls[i][bi / 64] |= 1 << (bi % 64);
                    }
                }
                n_bodies_taken += 1;
                n_lanes += take.count_ones() as u64;
                let template = &self.acc_templates[body.outcome];
                if let Some(targets) = sink.targets {
                    // BACKWARD: per lane not yet hit, is this output in the
                    // target set? One bit per input row is the whole result:
                    // no dedup, nothing materialized, and a lane that has
                    // hit is done for the rest of the slice.
                    take &= !hit_lanes;
                    while take != 0 {
                        let i = take.trailing_zeros() as usize;
                        take &= take - 1;
                        let h1 = read_word(outbuf, body.key_roots.0, i);
                        let h2 = read_word(outbuf, body.key_roots.1, i);
                        let key = (
                            runtime2::mix64(template.part.0.wrapping_add(h1)),
                            runtime2::mix64(template.part.1.wrapping_add(h2)),
                        );
                        n_unique += 1;
                        let cout = cell_out(body, outbuf, i, start);
                        if targets.contains(key, cout) {
                            sink.hit(lo + i);
                            hit_lanes |= 1 << i;
                        }
                    }
                    continue;
                }
                while take != 0 {
                    let i = take.trailing_zeros() as usize;
                    take &= take - 1;
                    // The row's (h1,h2) fold over the varying, non-widened
                    // cells, computed by the kernel (the body's key roots).
                    let h1 = read_word(outbuf, body.key_roots.0, i);
                    let h2 = read_word(outbuf, body.key_roots.1, i);
                    let part = template.part;
                    let key = (
                        runtime2::mix64(part.0.wrapping_add(h1)),
                        runtime2::mix64(part.1.wrapping_add(h2)),
                    );
                    let cin = cell_in[lo + i];
                    // EMISSION-TIME PROVENANCE (plans/buckets.md). The
                    // source of this row is lane `i` of this slice, and
                    // everything that needs to know is told right here:
                    // the pos-graph edge and THE EDGE (plans/waves.md, the
                    // explicit graph): the input rows are lanes `lo..lo+n`,
                    // consecutive ids from `slice_base`, and a queued row
                    // carries a 16-bit mask of the lanes that produced it,
                    // reached through the cache's row ref.
                    if let Some((first_cin, r)) = sink.seen.insert_ref(key, cin, 0) {
                        // A re-emission of a row this call already produced.
                        // Nothing to push - but if it came from a DIFFERENT
                        // input cell, that is a pos-graph edge the first
                        // emission did not record - and this lane is one
                        // more predecessor of it: onto the queued row's
                        // mask, or straight to a record if the row was
                        // flushed (its id is in the cache then).
                        if sink.edges_on && first_cin != cin {
                            sink.edges.insert((cin, cell_out(body, outbuf, i, start)));
                        }
                        match slice_base {
                            Some(b) if r & RowCache::ID_FLAG != 0 => {
                                sink.direct_edge(r & !RowCache::ID_FLAG, b, lane0 + i);
                                continue;
                            }
                            Some(_) if r & RowCache::DROP_FLAG != 0 => continue,
                            // The row was flushed (a stale ref, only if
                            // the cache lost the flush's write-back): push
                            // it again; the flush merges duplicates by key.
                            Some(b) if !sink.mark_pred(r, b, lane0 + i) => {}
                            _ => continue,
                        }
                    }
                    sink.emitted += 1;
                    n_unique += 1;
                    let cout = cell_out(body, outbuf, i, start);
                    if sink.edges_on && last_edge != (cin, cout) {
                        last_edge = (cin, cout);
                        sink.edges.insert((cin, cout));
                    }
                    // The queue for (shape, cell), over the shape's union
                    // skeleton: the run cache makes this one compare for a
                    // run of rows at one cell.
                    let q = sink.queue(template.shape_hash, cout, || (*template.union).clone_block());
                    cols.push_row(&mut sink.slots[q], outbuf, i, key, cout);
                    if let Some(b) = slice_base {
                        sink.slots[q].pred_base.push(b);
                        sink.slots[q].pred_mask.push(1u64 << (lane0 + i));
                        sink.slots[q].last_extra.push(u32::MAX);
                        sink.seen.set_ref(key, sink.row_ref(q));
                    }
                    sink.pushed(q).expect("flushing a full queue");
                }
            }
            if let Some(ls) = lane_sets {
                let mut m = BODYSETS.lock().expect("bodysets");
                let e = m.entry(chunk.shape_hash).or_default();
                for s in ls {
                    *e.entry(s).or_default() += 1;
                }
            }
            lo += n;
        }
        sink.end_call();
        sc.put_back();
        for (i, n) in [n_bodies, n_bodies_taken, n_lanes, n_unique].into_iter().enumerate() {
            CALL_STATS[3 + i].fetch_add(n, std::sync::atomic::Ordering::Relaxed);
        }
        true
    }

    /// from the interpreter. Prints at most a few mismatches per chunk.
    /// On a decline: evaluate the fused graph for `lane` and print the
    /// conjuncts of the body's `ok` (its And-tree's leaves) that are not
    /// decided true - the premise the lane fails.
    fn explain_ok(&self, chunk: &Rt2, lane: usize, ok_node: crate::transpile::graph::NodeId) {
        use crate::transpile::graph::{Op, Val};
        let mut cells: std::collections::HashMap<u32, Val> = Default::default();
        for &cell in &self.compiled.input_cells {
            let v = match chunk.cols[cell as usize].at(lane) {
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
                eprintln!("[kernel]   (graph evaluation failed: {e:#})");
                return;
            }
        };
        let mut stack = vec![ok_node];
        let mut seen = std::collections::HashSet::new();
        let mut shown = 0;
        while let Some(n) = stack.pop() {
            if !seen.insert(n) {
                continue;
            }
            let node = self.fused.get(n);
            if matches!(node.op, Op::And) {
                stack.extend(node.args.iter().copied());
                continue;
            }
            if vals[n as usize] != Val::Bool(Some(true)) && shown < 8 {
                shown += 1;
                let args: Vec<String> = node
                    .args
                    .iter()
                    .map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize]))
                    .collect();
                eprintln!("[kernel]   ok conjunct {} {:?} = {:?}; args {}", n, node.op, vals[n as usize], args.join(", "));
            }
        }
    }

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

    /// The input columns of `chunk` as slices, resolved once per call:
    /// packing a slice is then a copy per field, not a `col.at()` match
    /// per value.
    fn input_views<'c>(&self, chunk: &'c Rt2) -> Vec<InputView<'c>> {
        self.compiled
            .input_cells
            .iter()
            .zip(&self.compiled.input_reprs)
            .map(|(&cell, repr)| InputView::of(&chunk.cols[cell as usize], *repr))
            .collect()
    }

    /// Pack lanes `[lo, lo+16)` into `buf` from the resolved `views`. Tail
    /// lanes past `n` clamp to the last valid lane, so the assembly's
    /// per-lane call-outs (div/mget/...) never fault on garbage - the `take`
    /// mask discards those lanes anyway.
    fn pack_input(&self, views: &[InputView], lo: usize, n: usize, buf: &mut [u8]) {
        for (view, &off) in views.iter().zip(&self.compiled.input_offsets) {
            let off = off as usize;
            let lane = |l: usize| lo + l.min(n - 1);
            match view {
                InputView::Num(col) => {
                    for l in 0..16 {
                        let raw = col[lane(l)].as_raw_u32();
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&raw.to_le_bytes());
                    }
                }
                InputView::NumU(raw) => {
                    for l in 0..16 {
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&raw.to_le_bytes());
                    }
                }
                InputView::Ival(col) => {
                    for l in 0..16 {
                        let (a, b) = col[lane(l)];
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&a.as_raw_u32().to_le_bytes());
                        buf[off + 64 + l * 4..off + 64 + l * 4 + 4]
                            .copy_from_slice(&b.as_raw_u32().to_le_bytes());
                    }
                }
                InputView::IvalOfNum(col) => {
                    for l in 0..16 {
                        let raw = col[lane(l)].as_raw_u32().to_le_bytes();
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&raw);
                        buf[off + 64 + l * 4..off + 64 + l * 4 + 4].copy_from_slice(&raw);
                    }
                }
                InputView::IvalU(a, b) => {
                    for l in 0..16 {
                        buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&a.to_le_bytes());
                        buf[off + 64 + l * 4..off + 64 + l * 4 + 4].copy_from_slice(&b.to_le_bytes());
                    }
                }
                InputView::BoolU(mask) => {
                    buf[off..off + 2].copy_from_slice(&mask.to_le_bytes());
                }
                InputView::Bool(col) => {
                    let mut mask = 0u16;
                    for l in 0..16 {
                        if let AV::Bool(true) = col[lane(l)] {
                            mask |= 1 << l;
                        }
                    }
                    buf[off..off + 2].copy_from_slice(&mask.to_le_bytes());
                }
                InputView::Any(col, repr) => {
                    // The general case (a materialized `AV` column feeding a
                    // numeric input): per value, as before.
                    for l in 0..16 {
                        let v = col.at(lane(l));
                        match repr {
                            CellRepr::Num => {
                                let raw = num_raw(v);
                                buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&raw.to_le_bytes());
                            }
                            CellRepr::Ival => {
                                let (a, b) = ival_raw(v);
                                buf[off + l * 4..off + l * 4 + 4].copy_from_slice(&a.to_le_bytes());
                                buf[off + 64 + l * 4..off + 64 + l * 4 + 4]
                                    .copy_from_slice(&b.to_le_bytes());
                            }
                            CellRepr::Bool => unreachable!("bool inputs are Bool/BoolU views"),
                        }
                    }
                }
            }
        }
    }
}

/// A thread's reusable scratch for one kernel: the packed input and the
/// output buffer and the dedup cache. Kept per (thread, kernel) across
/// calls so no unit allocates them afresh.
fn bodysets_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_BODYSETS").is_some())
}

/// DIAGNOSTIC: per input shape, the distinct per-lane body sets and how
/// many lanes had each (`CELESTE_BODYSETS=1`, printed by `print_bodysets`).
static BODYSETS: std::sync::Mutex<std::collections::BTreeMap<u64, std::collections::HashMap<Vec<u64>, u64>>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

pub fn print_bodysets() {
    let m = BODYSETS.lock().expect("bodysets");
    if m.is_empty() {
        return;
    }
    for (shape, sets) in m.iter() {
        let lanes: u64 = sets.values().sum();
        let mut counts: Vec<u64> = sets.values().copied().collect();
        counts.sort_unstable_by(|a, b| b.cmp(a));
        let top: u64 = counts.iter().take(16).sum();
        let top64: u64 = counts.iter().take(64).sum();
        let sizes: Vec<u32> = sets.keys().map(|s| s.iter().map(|w| w.count_ones()).sum()).collect();
        let avg_size = sizes.iter().map(|&x| x as f64).sum::<f64>() / sizes.len().max(1) as f64;
        eprintln!(
            "[bodysets] shape {:016x}: {} lanes, {} distinct body sets (avg {:.1} bodies per set); top 16 sets cover {:.1}% of lanes, top 64 {:.1}%",
            shape,
            lanes,
            sets.len(),
            avg_size,
            100.0 * top as f64 / lanes.max(1) as f64,
            100.0 * top64 as f64 / lanes.max(1) as f64
        );
    }
}

struct Scratch {
    kernel: usize,
    inbuf: Vec<u8>,
    outbuf: Vec<u8>,
}

thread_local! {
    static SCRATCH: std::cell::RefCell<Vec<Scratch>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Scratch {
    /// This thread's scratch for `k`, created on first use. Taken out of
    /// the thread-local for the duration of the call (`put_back`).
    fn take(k: &AsmKernel) -> Scratch {
        let id = k as *const AsmKernel as usize;
        SCRATCH.with(|c| {
            let mut c = c.borrow_mut();
            match c.iter().position(|s| s.kernel == id) {
                Some(i) => c.swap_remove(i),
                None => Scratch {
                    kernel: id,
                    inbuf: vec![0u8; k.compiled.input_bytes as usize],
                    outbuf: vec![0u8; k.compiled.n_roots * 128],
                },
            }
        })
    }

    fn put_back(self) {
        SCRATCH.with(|c| c.borrow_mut().push(self));
    }
}

/// A body's output roots, grouped by kind, each paired with the index of
/// its queue column (`Slot::cols`, the shape's union skeleton) - plus the
/// union columns this body's outcome holds UNIFORM, with the value to
/// write per row.
struct BodyCols {
    num: Vec<(usize, usize)>,
    ival: Vec<(usize, usize)>,
    bool_: Vec<(usize, usize)>,
    uniform_num: Vec<(usize, u32)>,
    uniform_ival: Vec<(usize, (u32, u32))>,
    uniform_bool: Vec<(usize, u8)>,
}

impl BodyCols {
    fn of(body: &AsmBody, proto: &crate::frame::Slot, own: &Rt2) -> Result<Self> {
        use crate::frame::TCol;
        let (mut num, mut ival, mut bool_) = (Vec::new(), Vec::new(), Vec::new());
        let mut written = vec![false; proto.cols.len()];
        for f in &body.fields {
            // A field whose output column the union makes uniform (a
            // widened-to-uniform cell in every outcome of the shape, e.g.
            // level 0's `rem`) is computed by the kernel but not stored:
            // the column already holds the widened value.
            let Some(ci) = proto.cols.iter().position(|(cell, _)| *cell == f.cell) else {
                continue;
            };
            written[ci] = true;
            match (f.kind, &proto.cols[ci].1) {
                (RootKind::Num, TCol::Num(_)) => num.push((ci, f.root)),
                (RootKind::Ival, TCol::Ival(_)) => ival.push((ci, f.root)),
                (RootKind::Bool, TCol::Bool(_)) => bool_.push((ci, f.root)),
                (k, _) => anyhow::bail!("body field cell {} kind {:?} disagrees with the shape's column", f.cell, k),
            }
        }
        let (mut uniform_num, mut uniform_ival, mut uniform_bool) = (Vec::new(), Vec::new(), Vec::new());
        for (ci, (cell, col)) in proto.cols.iter().enumerate() {
            if written[ci] {
                continue;
            }
            let Col::U(av) = &own.cols[*cell] else {
                anyhow::bail!("union column at cell {cell} is neither a body root nor uniform in its outcome");
            };
            match (col, av) {
                (TCol::Num(_), AV::Num(n)) => uniform_num.push((ci, n.as_raw_u32())),
                (TCol::Ival(_), AV::Ival(a, b)) => uniform_ival.push((ci, (a.as_raw_u32(), b.as_raw_u32()))),
                (TCol::Ival(_), AV::Num(n)) => uniform_ival.push((ci, (n.as_raw_u32(), n.as_raw_u32()))),
                (TCol::Bool(_), AV::Bool(x)) => uniform_bool.push((ci, *x as u8)),
                (TCol::Bool(_), AV::UBool) => uniform_bool.push((ci, 2)),
                (_, v) => anyhow::bail!("union column at cell {cell}: the outcome's uniform {v:?} does not fit its kind"),
            }
        }
        Ok(BodyCols { num, ival, bool_, uniform_num, uniform_ival, uniform_bool })
    }

    /// Append lane `i` of the output buffer to `slot` as one row.
    #[inline]
    fn push_row(&self, slot: &mut crate::frame::Slot, buf: &[u8], i: usize, key: (u64, u64), cell: u32) {
        use crate::frame::TCol;
        let word = |base: usize| u32::from_le_bytes(buf[base..base + 4].try_into().unwrap());
        for &(ci, root) in &self.num {
            if let TCol::Num(v) = &mut slot.cols[ci].1 {
                v.push(word(root * 128 + i * 4));
            }
        }
        for &(ci, root) in &self.ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                v.push((word(root * 128 + i * 4), word(root * 128 + 64 + i * 4)));
            }
        }
        for &(ci, root) in &self.bool_ {
            if let TCol::Bool(v) = &mut slot.cols[ci].1 {
                let val = u16::from_le_bytes([buf[root * 128], buf[root * 128 + 1]]);
                let known = u16::from_le_bytes([buf[root * 128 + 2], buf[root * 128 + 3]]);
                v.push(if known >> i & 1 == 0 { 2 } else { (val >> i & 1) as u8 });
            }
        }
        for &(ci, n) in &self.uniform_num {
            if let TCol::Num(v) = &mut slot.cols[ci].1 {
                v.push(n);
            }
        }
        for &(ci, ab) in &self.uniform_ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                v.push(ab);
            }
        }
        for &(ci, b) in &self.uniform_bool {
            if let TCol::Bool(v) = &mut slot.cols[ci].1 {
                v.push(b);
            }
        }
        slot.keys.push(key);
        slot.cells.push(cell);
    }
}

/// One input column of a call, resolved to what the packer copies from.
enum InputView<'c> {
    Num(&'c [P8]),
    NumU(u32),
    Ival(&'c [(P8, P8)]),
    IvalOfNum(&'c [P8]),
    IvalU(u32, u32),
    Bool(&'c [AV]),
    BoolU(u16),
    Any(&'c Col, CellRepr),
}

impl<'c> InputView<'c> {
    fn of(col: &'c Col, repr: CellRepr) -> Self {
        match (repr, col) {
            (CellRepr::Num, Col::N(v)) => InputView::Num(v),
            (CellRepr::Num, Col::U(AV::Num(n))) => InputView::NumU(n.as_raw_u32()),
            (CellRepr::Ival, Col::I(v)) => InputView::Ival(v),
            (CellRepr::Ival, Col::N(v)) => InputView::IvalOfNum(v),
            (CellRepr::Ival, Col::U(AV::Ival(a, b))) => InputView::IvalU(a.as_raw_u32(), b.as_raw_u32()),
            (CellRepr::Ival, Col::U(AV::Num(n))) => InputView::IvalU(n.as_raw_u32(), n.as_raw_u32()),
            (CellRepr::Bool, Col::V(v)) => InputView::Bool(v),
            (CellRepr::Bool, Col::U(v)) => {
                InputView::BoolU(if matches!(v, AV::Bool(true)) { 0xffff } else { 0 })
            }
            (CellRepr::Bool, other) => panic!("ASM bool input column is {:?}", other),
            (repr, other) => InputView::Any(other, repr),
        }
    }
}

/// `CELESTE_KERNEL_KEY_CHECK=1`: run the real `Rt2::boundary` over a copy
/// of an emitted slot block and demand it changes NOTHING - same
/// structure, same shape hash, same row keys, same width (no within-block
/// duplicates left for its dedup). This is the gate that the append step's
/// shortcut is exact; a difference here would silently change what the
/// search dedups on. A no-op unless the variable is set.
pub(crate) fn key_check(slot: &crate::frame::Slot) {
    if !key_check_on() {
        return;
    }
    let acc = slot.to_rt2();
    let exact = registry().map(|r| r.exact_boundary).unwrap_or(false);
    let mut b = acc.clone_block();
    if exact {
        b.boundary_exact();
    } else {
        b.boundary(&super::boundary_ids());
    }
    assert!(
        b.width == acc.width
            && b.shape_hash == acc.shape_hash
            && b.structure == acc.structure
            && b.row_keys == acc.row_keys,
        "KERNEL KEY CHECK: shape {:#x}: the boundary disagrees with the append step \
         (width {} vs {}, shape {:#x} vs {:#x}, structure {}, keys {})",
        acc.shape_hash,
        b.width,
        acc.width,
        b.shape_hash,
        acc.shape_hash,
        if b.structure == acc.structure { "same" } else { "DIFFERENT" },
        if b.row_keys == acc.row_keys { "same" } else { "DIFFERENT" },
    );
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
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        match self.by_shape.get(&chunk.shape_hash) {
            Some(k) => k.run(chunk, cell_in, lanes, sink),
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
        unify(&mut by_shape)?;
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

/// The kind of a typed column or of a uniform value that a typed column
/// could hold.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Kind {
    Num,
    Ival,
    Bool,
}

fn kind_of(col: &Col) -> Option<Kind> {
    match col {
        Col::N(_) | Col::U(AV::Num(_)) => Some(Kind::Num),
        Col::I(_) | Col::U(AV::Ival(..)) => Some(Kind::Ival),
        Col::V(_) | Col::U(AV::Bool(_)) | Col::U(AV::UBool) => Some(Kind::Bool),
        _ => None,
    }
}

fn typed(kind: Kind) -> Col {
    match kind {
        Kind::Num => Col::N(Vec::new()),
        Kind::Ival => Col::I(Vec::new()),
        Kind::Bool => Col::V(Vec::new()),
    }
}

/// Give every outcome template of a SHAPE the shape's union skeleton: a
/// cell is typed in the union if any outcome of the shape varies it or
/// two outcomes hold it uniform at different values (Num and Ival unify
/// to Ival); otherwise it stays uniform at the common value. Then map
/// every body's roots and its outcome's uniform cells onto the union's
/// columns (`BodyCols`). The reference engine's rule (every numeric and
/// boolean cell typed) is the same idea without the registry to narrow
/// it.
fn unify(by_shape: &mut HashMap<u64, AsmKernel>) -> Result<()> {
    let mut unions: HashMap<u64, Rt2> = HashMap::new();
    for kernel in by_shape.values() {
        for t in &kernel.acc_templates {
            let own = &t.own;
            match unions.get_mut(&t.shape_hash) {
                None => {
                    let mut u = own.clone_block();
                    u.shape_hash = t.shape_hash;
                    unions.insert(t.shape_hash, u);
                }
                Some(u) => {
                    anyhow::ensure!(u.cols.len() == own.cols.len(), "shape {:#x}: outcomes with different widths", t.shape_hash);
                    for c in 0..u.cols.len() {
                        let (a, b) = (&u.cols[c], &own.cols[c]);
                        if let (Col::U(x), Col::U(y)) = (a, b) {
                            if x == y {
                                continue;
                            }
                        }
                        let (ka, kb) = (kind_of(a), kind_of(b));
                        let k = match (ka, kb) {
                            (Some(x), Some(y)) if x == y => x,
                            (Some(Kind::Num), Some(Kind::Ival)) | (Some(Kind::Ival), Some(Kind::Num)) => Kind::Ival,
                            _ => anyhow::bail!(
                                "shape {:#x} cell {c}: outcomes disagree on a cell that cannot be a typed column ({a:?} vs {b:?})",
                                t.shape_hash
                            ),
                        };
                        u.cols[c] = typed(k);
                    }
                }
            }
        }
    }
    let unions: HashMap<u64, std::sync::Arc<Rt2>> = unions.into_iter().map(|(k, v)| (k, std::sync::Arc::new(v))).collect();
    let mut widened = 0usize;
    for kernel in by_shape.values_mut() {
        for t in &mut kernel.acc_templates {
            t.union = unions[&t.shape_hash].clone();
            widened += t.union.cols.iter().zip(&t.own.cols).filter(|(u, o)| !matches!(u, Col::U(_)) && matches!(o, Col::U(_))).count();
        }
        let mut body_cols = Vec::with_capacity(kernel.bodies.len());
        for b in &kernel.bodies {
            let t = &kernel.acc_templates[b.outcome];
            let proto = crate::frame::Slot::new((*t.union).clone_block());
            body_cols.push(BodyCols::of(b, &proto, &t.own)?);
        }
        kernel.body_cols = body_cols;
    }
    let templates: usize = by_shape.values().map(|k| k.acc_templates.len()).sum();
    let bodies: usize = by_shape.values().map(|k| k.bodies.len()).sum();
    let fused: usize = by_shape.values().map(|k| k.fused.len()).sum();
    let mut per_shape: Vec<(usize, u8, usize, u8)> =
        by_shape.values().map(|k| (k.bodies.len(), k.forks, k.fused.len(), k.fused.fork_bits())).collect();
    per_shape.sort_unstable_by_key(|s| std::cmp::Reverse(s.0));
    eprintln!(
        "[asm build] {} output shapes over {templates} outcome templates; {widened} template cells widened uniform -> typed by the union ({:.2} per template); {bodies} bodies, {fused} fused nodes over {} input shapes; per input shape (bodies, forks, fused nodes, fork grid bits): {:?}",
        unions.len(),
        widened as f64 / templates.max(1) as f64,
        by_shape.len(),
        per_shape
    );
    Ok(())
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
        crate::trace::emit::asm_fused_from(&r.bound, &r.lowered.spec)
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
            forks: r.bound.forks,
            body_cols: Vec::new(),
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
    let empty0 = reshape(&skeleton, 0);
    let mut t = AccTemplate {
        skeleton,
        inits,
        shape_hash,
        part: (0, 0),
        own: empty0.clone_block(),
        union: std::sync::Arc::new(empty0),
    };
    t.own = t.build();
    t.union = std::sync::Arc::new(t.build());
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
static CALL_STATS: [std::sync::atomic::AtomicU64; 7] =
    [const { std::sync::atomic::AtomicU64::new(0) }; 7];

/// Take (and reset) the call-shape counters.
pub(crate) fn take_call_stats() -> [u64; 7] {
    std::array::from_fn(|i| CALL_STATS[i].swap(0, std::sync::atomic::Ordering::Relaxed))
}

/// The kernel set of the process-global rem precision (built on first use).
pub(crate) fn registry() -> Option<&'static Registry> {
    registry_for(crate::interpreter::abstraction::rem_precision_from_env())
}

/// One kernel set per rung, each built once, on first use or by
/// `prebuild` - the rung is an explicit input of the build, not read from
/// the process-global precision, so the sets can be built in parallel.
fn registry_for(rem: crate::interpreter::abstraction::RemPrecision) -> Option<&'static Registry> {
    use crate::interpreter::abstraction::RemPrecision;
    static REGS: [std::sync::OnceLock<Option<Registry>>; 17] =
        [const { std::sync::OnceLock::new() }; 17];
    let slot = match rem {
        RemPrecision::Exact => 16,
        RemPrecision::Bits(b) => (b as usize).min(16),
    };
    REGS[slot].get_or_init(|| build_registry_for_rung(rem)).as_ref()
}

/// Build every rung's kernel set now, all rungs at once (one builder
/// thread per rung, each assembling its shapes in parallel). The ladder
/// calls this up front: lazily, the 17 builds landed one at a time inside
/// the first frame of each level - 82 s of a 524 s room (1,0) run.
pub fn prebuild(precisions: &[crate::interpreter::abstraction::RemPrecision]) {
    let t = std::time::Instant::now();
    std::thread::scope(|scope| {
        for &p in precisions {
            scope.spawn(move || {
                registry_for(p);
            });
        }
    });
    eprintln!("[asm build] {} rungs prebuilt in {:.1} s", precisions.len(), t.elapsed().as_secs_f64());
}

fn build_registry_for_rung(rem: crate::interpreter::abstraction::RemPrecision) -> Option<Registry> {
    use crate::interpreter::abstraction::RemPrecision;
    use crate::trace::shapes::WalkOpts;
    let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
    let mode = super::dispatch::traced_mode_for(rem);
    let (opts, exact) = match mode {
        super::dispatch::TracedMode::Level0 => (WalkOpts::LEVEL0, false),
        super::dispatch::TracedMode::Level0Agnostic => {
            // Phase 1: the opt-in rung-specific variant bakes the rem
            // widening into the graph (`ladder_widen`), still through
            // `boundary_exact` (it emits the widened rem and keys the
            // emitted field). Default `LADDER` is unchanged.
            let opts = match (super::dispatch::widen_in_graph(), rem) {
                (true, RemPrecision::Bits(b)) => WalkOpts::ladder_widen(b),
                _ => WalkOpts::LADDER,
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
            eprintln!("ASM kernels ENABLED ({mode:?}, {rem:?}): {} start-room shapes assembled", reg.len());
            Some(reg)
        }
        Err(e) => panic!("building ASM kernels for {rem:?}: {e:#}"),
    }
}

/// Run one chunk through the ASM kernels. `false` = miss or declined (the
/// caller routes to the reference path).
pub(crate) fn run_chunk(
    chunk: &Rt2,
    cell_in: &[u32],
    lanes: std::ops::Range<usize>,
    sink: &mut crate::frame::ForwardSink,
) -> bool {
    match registry() {
        Some(reg) => reg.run_chunk(chunk, cell_in, lanes, sink),
        None => false,
    }
}
