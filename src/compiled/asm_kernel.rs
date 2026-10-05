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
//! `live & !error` lane's ASM-computed field values into it, and let
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
    /// The root's byte offset in the output buffer (`Compiled::root_offsets`).
    off: usize,
    kind: RootKind,
    /// A boolean root an output widening may write UNKNOWN in some lanes (it
    /// reads the canonical output unknown, `Symbolic::unknown_bool_output`: a
    /// near level's floor `collideable`). Any other undecided boolean is a
    /// branch on an unknown that no premise declined, and fatal (`push_row`).
    may_unknown: bool,
}

/// One fused body (a distinct (outcome, choices) with distinct outputs):
/// its output fields plus the `error`/`live` mask roots. A lane is
/// materialized into outcome `outcome`'s block iff `live & !error`.
struct AsmBody {
    outcome: usize,
    /// The fork configuration (two bits per fork), for diagnostics.
    splits: Vec<u8>,
    fields: Vec<AsmField>,
    /// `error`'s slot (`CELESTE_KERNEL_EXPLAIN`).
    error_root: usize,
    /// `error`/`live`'s byte offsets in the output buffer (`Compiled::
    /// root_offsets`).
    error_off: usize,
    live_off: usize,
    /// The fields the row key folds (`key_words`): the body's varying ones -
    /// a per-row column the boundary does not widen to uniform.
    key_fields: Vec<KeyField>,
    /// Where an emitted row's player x, player y, room x, room y come from
    /// (`search::pos_graph::block_cells`' inputs), so the append step can
    /// compute the row's position cell straight off the output buffer.
    /// `None`: this outcome has no player object, or its `x`/`y` is not a
    /// plain number (every row is `NO_CELL`, as `block_cells` says).
    pos: Option<[PosSrc; 4]>,
    /// The transfer roots (`search::arc_edges`).
    arc: ArcSlots,
}

/// Where a body's transfer roots are (`trace::verify::FrameOut::arc`): per
/// axis took, pre, frag, ox, fin, each a (byte offset, kind) of the output
/// buffer; and whether the outcome has a player at its end.
#[derive(Clone, Copy)]
struct ArcSlots {
    roots: [(usize, RootKind); 2 * crate::trace::verify::ARC_AXIS_ROOTS],
    fin: bool,
}

impl ArcSlots {
    /// Lane `i`'s transfer, decoded (`arc_edges::decode_axis`). A refusal,
    /// or a `took` the lane does not decide, is FATAL: the record would be
    /// a claim about the remainder nothing checked.
    fn transfer(&self, buf: &[u8], i: usize, chunk: &Rt2, lane: usize, outcome: usize) -> crate::search::arc_edges::Pair {
        use crate::search::arc_edges::{decode_axis, RawAxis};
        let ival = |k: usize| -> (i32, i32) {
            let (off, kind) = self.roots[k];
            match read_root_av(off, kind, buf, i) {
                AV::Num(p) => (p.as_raw_u32() as i32, p.as_raw_u32() as i32),
                AV::Ival(a, b) => (a.as_raw_u32() as i32, b.as_raw_u32() as i32),
                other => panic!("arc edges: transfer root {k} of a {kind:?} root holds {other:?}"),
            }
        };
        let axis = |a: usize| -> crate::search::arc_edges::AxisXfer {
            let base = a * crate::trace::verify::ARC_AXIS_ROOTS;
            let (off, kind) = self.roots[base];
            let took = match read_root_av(off, kind, buf, i) {
                AV::Bool(b) => b,
                other => panic!("arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}'s `took` is {other:?}, not a decided boolean", chunk.shape_hash),
            };
            let raw = RawAxis { took, pre: ival(base + 1), frag: ival(base + 2), ox: ival(base + 3), fin: self.fin.then(|| ival(base + 4)) };
            decode_axis(&raw).unwrap_or_else(|e| panic!("arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}: {e} (raw {raw:?})", chunk.shape_hash))
        };
        (axis(0), axis(1))
    }
}

/// One field of a body's row key: its cell, the byte offset it is read from,
/// and how its slot is read. A number and the point interval `[v, v]` key alike (`runtime2::av_code`),
/// so a number root keys the same whether its column stores it as a number
/// or, typed by the shape's union, as `[v, v]`.
///
/// Specialized at build time: `c` is `seed ^ cell * CELL_K` per half, and
/// `read` reads the slot straight into `runtime2::av_code`'s code - no `AV`
/// is built per row (the fold was ~16% of a big frame's cycles through
/// `read_root_av` and `av_code`, 2026-09-18).
struct KeyField {
    c: [u64; 2],
    root: usize,
    read: KeyRead,
}

/// How a key field's slot becomes its `av_code`.
#[derive(Clone, Copy)]
enum KeyRead {
    Num,
    Ival,
    Bool,
}

impl KeyField {
    fn new(cell: u64, root: usize, kind: RootKind) -> KeyField {
        use celeste_engine::runtime2::CELL_K;
        let ck = cell.wrapping_mul(CELL_K);
        let read = match kind {
            RootKind::Num => KeyRead::Num,
            RootKind::Ival => KeyRead::Ival,
            RootKind::Bool => KeyRead::Bool,
        };
        KeyField { c: [KEY_SEED1 ^ ck, KEY_SEED2 ^ ck], root, read }
    }

    /// `runtime2::av_code` of lane `i`'s value, off the output slot.
    #[inline]
    fn code(&self, buf: &[u8], i: usize) -> u64 {
        let base = self.root;
        let word = |o: usize| u32::from_le_bytes(buf[o..o + 4].try_into().unwrap());
        match self.read {
            KeyRead::Num => celeste_engine::runtime2::num_code(word(base + i * 4)),
            KeyRead::Ival => celeste_engine::runtime2::ival_code(word(base + i * 4), word(base + 64 + i * 4)),
            KeyRead::Bool => {
                let val = u16::from_le_bytes([buf[base], buf[base + 1]]);
                let known = u16::from_le_bytes([buf[base + 2], buf[base + 3]]);
                if known & (1 << i) != 0 {
                    3u64 << 56 | (val >> i & 1) as u64
                } else {
                    4u64 << 56
                }
            }
        }
    }
}

impl AsmBody {
    /// Row `i`'s `(h1, h2)`: `Σ cell_mix` over the key fields, per half.
    /// `mix64(part + h)` is the boundary's key exactly (`acc_template`
    /// computes `part`; `check_against_boundary` gates it).
    #[inline]
    fn key_words(&self, buf: &[u8], i: usize) -> (u64, u64) {
        use celeste_engine::runtime2::mix64;
        let (mut h1, mut h2) = (0u64, 0u64);
        for f in &self.key_fields {
            let code = f.code(buf, i);
            h1 = h1.wrapping_add(mix64(f.c[0] ^ code));
            h2 = h2.wrapping_add(mix64(f.c[1] ^ code));
        }
        (h1, h2)
    }
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

/// Rust's default stack for a spawned thread: what a thread that never called
/// `set_thread_stack` is assumed to have.
const DEFAULT_THREAD_STACK: usize = 2 << 20;
/// Room a kernel call leaves for its callers' frames and its call-outs.
const KERNEL_STACK_MARGIN: usize = 1 << 20;

thread_local! {
    static THREAD_STACK: std::cell::Cell<usize> = const { std::cell::Cell::new(DEFAULT_THREAD_STACK) };
}

/// Record this thread's stack size. Every kernel call checks that its spill
/// frame fits (`Compiled::frame_bytes`): a kernel whose frame outgrew the
/// forward workers' stack touched past it and segfaulted (room (3,0) with the
/// fruit and the floors unknown, an 83 MB frame on a 64 MB stack, 2026-09-18).
/// Call it first thing in a thread spawned with `stack_size(bytes)` that runs
/// kernels.
pub fn set_thread_stack(bytes: usize) {
    THREAD_STACK.with(|s| s.set(bytes));
}

fn thread_stack() -> usize {
    THREAD_STACK.with(|s| s.get())
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
    /// The fused graph + its flat roots + the room, what a decline's
    /// explanation evaluates (`explain_error`). KEPT ONLY under
    /// `kernel_graph_kept()`: otherwise empty, since every kernel set holding
    /// its graphs was gigabytes for nothing (2026-09-18).
    fused: crate::transpile::graph::Graph,
    flat_roots: Vec<crate::transpile::graph::NodeId>,
    room: Room,
    /// The fused graph's node count, for the build report.
    fused_nodes: usize,
    /// Per input cell (parallel to `compiled.input_cells`), the traced
    /// frame's path for it: what a packing failure names.
    input_names: Vec<String>,
}

impl AsmKernel {
    /// Run lanes `lanes` of one block. Per 16-lane slice: pack inputs, call
    /// the assembly, and for each body push its `live & !error` lanes into
    /// the sink's slot for (owner, outcome shape) - keyed, at the boundary,
    /// with the pos-graph edge recorded - or, in backward mode, mark the
    /// input rows whose outputs hit a target. A nonzero `live & error`
    /// (declined) fails the whole call (a coverage gap).
    fn run(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        debug_assert_eq!(cell_in.len(), chunk.width, "one input cell per input row");
        debug_assert!(lanes.end <= chunk.width);
        CALL_STATS[0].fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        // Within-call dedup (`ForwardSink::seen`, a `RowCache`): the fused
        // graph's configurations re-emit the same row from neighbouring
        // lanes ~8x over; the cache catches those (keyed by the row key,
        // which is unique across outcomes) and the owner's door catches
        // the rest. It lives in the sink because the flush writes each
        // row's id back into it (the edges).
        sink.seen.clear();
        let mut lo = lanes.start;
        let mut idx = [0usize; 16];
        while lo < lanes.end {
            // A slice never crosses a 64-lane id group: its predecessor
            // records are (the group's first id, a bit per lane). A bucket
            // dispatch's run of one speed key starts at any lane, and a
            // 16-lane slice from lane 60 recorded lanes 64..75 against
            // group 0 - edges to the wrong states, a backward that lost
            // exact marks (2026-09-15).
            let n = 16.min(lanes.end - lo).min(64 - (lo & 63));
            for (i, l) in idx.iter_mut().enumerate().take(n) {
                *l = lo + i;
            }
            if !self.run_slice(chunk, cell_in, &idx[..n], u16::MAX, sink) {
                return false;
            }
            lo += n;
        }
        sink.end_call();
        true
    }

    /// Run ONE slice: the up-to-16 lanes `lanes` of `chunk` (all within one
    /// 64-lane id group, in any order), keeping only the lanes of `only`.
    fn run_slice(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: &[usize],
        only: u16,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        let n = lanes.len();
        debug_assert!((1..=16).contains(&n));
        assert!(lanes.iter().all(|&l| l & !63 == lanes[0] & !63), "a slice within one id group: lanes {lanes:?}");
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
        let body_cols = &self.body_cols;

        CALL_STATS[1].fetch_add(only.count_ones().min(n as u32) as u64, std::sync::atomic::Ordering::Relaxed);
        CALL_STATS[2].fetch_add(16, std::sync::atomic::Ordering::Relaxed);
        // The call's utilization tallies (folded into `CALL_STATS` once at
        // the end): (body, slice) pairs evaluated and those with any taken
        // lane, lane emissions before and after the dedup cache.
        let (mut n_bodies, mut n_bodies_taken, mut n_lanes, mut n_unique) = (0u64, 0u64, 0u64, 0u64);
        {
            let lo = lanes[0];
            // The slice's predecessor GROUP: 64 consecutive input lanes
            // (ids are consecutive within a block, units start at
            // multiples of 64), so a state's producers from four adjacent
            // slices share one record. Lane `i` of the slice is lane
            // `lanes[i] & 63` of the group.
            let slice_base: Option<u64> = sink.ids_in.map(|ids| ids[lo & !63]);
            let glane = |i: usize| lanes[i] & 63;
            let skip: u16 = match sink.skip_in {
                Some(sk) => (0..n).filter(|&i| sk[lanes[i]]).fold(0u16, |m, i| m | (1 << i)),
                None => 0,
            };
            self.pack_input(&views, lanes, inbuf);
            // The kernel's spill frame lives on THIS thread's stack: refuse
            // loudly rather than touch past its end (`set_thread_stack`).
            let (frame, stack) = (self.compiled.frame_bytes as usize, thread_stack());
            assert!(
                frame + KERNEL_STACK_MARGIN <= stack,
                "kernel {}: its {:.1} MB stack frame does not fit this thread's {:.1} MB stack (a kernel thread needs `set_thread_stack` and a larger `stack_size`)",
                self.compiled.sym,
                frame as f64 / 1048576.0,
                stack as f64 / 1048576.0
            );
            unsafe {
                (self.loaded.func)(
                    inbuf.as_ptr(),
                    outbuf.as_mut_ptr(),
                    &ctx as *const AsmCtx as *const c_void,
                );
            }
            let valid = (((1u32 << n) - 1) as u16) & only;
            for (body, cols) in self.bodies.iter().zip(body_cols) {
                // `error`/`live` are tri-state ZB masks, read with ONE
                // polarity (plans/graph-model.md section 5): each where it
                // MAY hold. An unknown `live` reads as live (the row's hull
                // over-approximates, the concrete search refutes); an unknown
                // `error` reads as error, so a lane the trace failed to
                // fork or decide declines loudly instead of producing a row
                // from the garbage `val` bit.
                let error = read_zb_may(outbuf, body.error_off);
                let live = read_zb_may(outbuf, body.live_off);
                if live & error & valid != 0 {
                    // Declined: a live lane the kernel could not keep (or
                    // could not DECIDE). A coverage gap for the whole call -
                    // fatal in the caller, so say what was declined: the
                    // body's outcome and the lanes' player rem / spd (the
                    // interval slots the rungs fork on).
                    let declined = live & error & valid;
                    let ids = crate::compiled::ids();
                    let mut rows = String::new();
                    let mut m = declined;
                    while m != 0 {
                        let i = m.trailing_zeros() as usize;
                        m &= m - 1;
                        let lane = lanes[i];
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
                        "[kernel] shape {:#x} body of outcome {} declined lanes {:#06x} (live {:#06x} error {:#06x}):\n{rows}",
                        chunk.shape_hash, body.outcome, declined, live, error
                    );
                    match self.flat_roots.get(body.error_root) {
                        Some(&error_node) => self.explain_error(chunk, lanes[declined.trailing_zeros() as usize], error_node),
                        None => eprintln!("[kernel] rerun with CELESTE_KERNEL_EXPLAIN=1 for the error the lane holds"),
                    }
                    sc.put_back();
                    return false;
                }
                let mut take = live & !error & valid & !skip;
                n_bodies += 1;
                if take == 0 {
                    continue;
                }
                n_bodies_taken += 1;
                n_lanes += take.count_ones() as u64;
                let template = &self.acc_templates[body.outcome];
                while take != 0 {
                    let i = take.trailing_zeros() as usize;
                    take &= take - 1;
                    // The row's (h1,h2) fold over the varying, non-widened
                    // cells.
                    let (h1, h2) = body.key_words(outbuf, i);
                    let part = template.part;
                    let key = (
                        runtime2::mix64(part.0.wrapping_add(h1)),
                        runtime2::mix64(part.1.wrapping_add(h2)),
                    );
                    let cin = cell_in[lanes[i]];
                    // THE TRANSFER (`search::arc_edges`): per producer,
                    // carried by the edge, never in the row.
                    // (Read only where the slice has ids, `slice_base`.)
                    let xfer = match slice_base {
                        Some(_) => sink.xfer_id(body.arc.transfer(outbuf, i, chunk, lanes[i], body.outcome)),
                        None => 0,
                    };
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
                            sink.edges.insert((cin, cell_out(body, outbuf, i)));
                        }
                        match slice_base {
                            Some(b) if r & RowCache::ID_FLAG != 0 => {
                                sink.direct_edge(r & !RowCache::ID_FLAG, b, xfer, glane(i));
                                continue;
                            }
                            Some(_) if r & RowCache::DROP_FLAG != 0 => continue,
                            // The row was flushed (a stale ref, only if
                            // the cache lost the flush's write-back): push
                            // it again; the flush merges duplicates by key.
                            Some(b) if !sink.mark_pred(r, b, xfer, glane(i)) => {}
                            Some(_) => continue,
                            None => continue,
                        }
                    }
                    sink.emitted += 1;
                    n_unique += 1;
                    let cout = cell_out(body, outbuf, i);
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
                        sink.slots[q].pred_mask.push(1u64 << (glane(i)));
                        sink.slots[q].pred_xfer.push(xfer);
                        sink.slots[q].last_extra.push(u32::MAX);
                        sink.seen.set_ref(key, sink.row_ref(q));
                    }
                    sink.pushed(q).expect("flushing a full queue");
                }
            }
        }
        sc.put_back();
        for (i, n) in [n_bodies, n_bodies_taken, n_lanes, n_unique].into_iter().enumerate() {
            CALL_STATS[3 + i].fetch_add(n, std::sync::atomic::Ordering::Relaxed);
        }
        true
    }

    /// On a decline: evaluate the fused graph for `lane` and print the
    /// disjuncts of the body's `error` (its Or-tree's leaves) that are not
    /// decided false - the error the lane holds.
    fn explain_error(&self, chunk: &Rt2, lane: usize, error_node: crate::transpile::graph::NodeId) {
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
        let mut stack = vec![error_node];
        let mut seen = std::collections::HashSet::new();
        let mut shown = 0;
        while let Some(n) = stack.pop() {
            if !seen.insert(n) {
                continue;
            }
            let node = self.fused.get(n);
            if matches!(node.op, Op::Or) {
                stack.extend(node.args.iter().copied());
                continue;
            }
            if vals[n as usize] != Val::Bool(Some(false)) && shown < 6 {
                shown += 1;
                let args: Vec<String> = node
                    .args
                    .iter()
                    .map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize]))
                    .collect();
                eprintln!("[kernel]   error disjunct {} {:?} = {:?}; args {}", n, node.op, vals[n as usize], args.join(", "));
                // The undecided CONDITION behind it: follow Sels through
                // their known conditions (and a comparison's Sel operands)
                // to the first condition the lane cannot decide.
                let mut cur = n;
                for _ in 0..12 {
                    let nd = self.fused.get(cur);
                    let next = match nd.op {
                        Op::Sel => match vals[nd.args[0] as usize] {
                            Val::Bool(Some(true)) => Some(nd.args[1]),
                            Val::Bool(Some(false)) => Some(nd.args[2]),
                            _ => {
                                let c = nd.args[0];
                                let cn = self.fused.get(c);
                                let cargs: Vec<String> = cn
                                    .args
                                    .iter()
                                    .map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize]))
                                    .collect();
                                eprintln!("[kernel]     undecided condition {} {:?} = {:?}; args {}", c, cn.op, vals[c as usize], cargs.join(", "));
                                // and one level into the comparison's operands
                                for &a in &cn.args {
                                    let an = self.fused.get(a);
                                    if matches!(an.op, Op::Sel) {
                                        let aargs: Vec<String> = an.args.iter().map(|&b| format!("{}={:?}<{:?}>", b, self.fused.get(b).op, vals[b as usize])).collect();
                                        eprintln!("[kernel]       operand {} Sel; args {}", a, aargs.join(", "));
                                    }
                                }
                                None
                            }
                        },
                        Op::Le | Op::Ge | Op::Lt | Op::Gt | Op::Eq | Op::SplitOk(_) => {
                            nd.args.iter().copied().find(|&a| matches!(self.fused.get(a).op, Op::Sel))
                        }
                        _ => None,
                    };
                    match next {
                        Some(x) => cur = x,
                        None => break,
                    }
                }
                // The FAILING premise: follow a false conjunct through the
                // strict merges (`And(Known(c), Sel(c, ok_a, ok_b))`) - an
                // And into its false children, a decided Sel into its taken
                // arm, an Or into every child - to the leaves that are
                // decided false, which name the premise the lane fails.
                let mut stack = vec![n];
                let mut walked = std::collections::HashSet::new();
                let mut leaves = 0;
                while let Some(x) = stack.pop() {
                    if !walked.insert(x) || leaves >= 8 {
                        continue;
                    }
                    let nd = self.fused.get(x);
                    match nd.op {
                        Op::And | Op::Or => stack.extend(nd.args.iter().copied().filter(|&a| vals[a as usize] == Val::Bool(Some(false)))),
                        Op::Sel => match vals[nd.args[0] as usize] {
                            Val::Bool(Some(true)) => stack.push(nd.args[1]),
                            Val::Bool(Some(false)) => stack.push(nd.args[2]),
                            _ => {}
                        },
                        _ => {
                            leaves += 1;
                            let args: Vec<String> = nd.args.iter().map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize])).collect();
                            eprintln!("[kernel]     failing premise {} {:?} = {:?}; args {}", x, nd.op, vals[x as usize], args.join(", "));
                            // A `Known` premise fails on an undecided
                            // condition: its undecided operands, down to the
                            // comparisons the lane cannot decide.
                            if matches!(nd.op, Op::Known) {
                                let mut todo = vec![(nd.args[0], 0usize)];
                                let mut shown = std::collections::HashSet::new();
                                while let Some((y, depth)) = todo.pop() {
                                    if depth > 6 || !shown.insert(y) || vals[y as usize] != Val::Bool(None) {
                                        continue;
                                    }
                                    let yn = self.fused.get(y);
                                    let yargs: Vec<String> = yn.args.iter().map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize])).collect();
                                    eprintln!("[kernel]       undecided {}{} {:?}; args {}", "  ".repeat(depth), y, yn.op, yargs.join(", "));
                                    todo.extend(yn.args.iter().map(|&a| (a, depth + 1)));
                                }
                            }
                        }
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
            .zip(&self.input_names)
            .map(|((&cell, repr), name)| {
                InputView::of(&chunk.cols[cell as usize], *repr)
                    .unwrap_or_else(|e| panic!("{e}: input cell {cell} = {name} of shape {:#x}", chunk.shape_hash))
            })
            .collect()
    }

    /// Pack lanes `[lo, lo+16)` into `buf` from the resolved `views`. Tail
    /// lanes past `n` clamp to the last valid lane, so the assembly's
    /// per-lane call-outs (div/mget/...) never fault on garbage - the `take`
    /// mask discards those lanes anyway.
    fn pack_input(&self, views: &[InputView], lanes: &[usize], buf: &mut [u8]) {
        let n = lanes.len();
        for (view, &off) in views.iter().zip(&self.compiled.input_offsets) {
            let off = off as usize;
            let lane = |l: usize| lanes[l.min(n - 1)];
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
                        match col[lane(l)] {
                            AV::Bool(true) => mask |= 1 << l,
                            AV::Bool(false) => {}
                            // See `InputView::of`: never pack an unknown as false.
                            other => panic!("ASM bool input lane holds {other:?}: a kernel reads a boolean the block does not decide"),
                        }
                    }
                    buf[off..off + 2].copy_from_slice(&mask.to_le_bytes());
                }
                InputView::UBool(col) => {
                    let (mut val, mut known) = (0u16, 0u16);
                    for l in 0..16 {
                        match col[lane(l)] {
                            AV::Bool(b) => {
                                known |= 1 << l;
                                val |= (b as u16) << l;
                            }
                            AV::UBool => {}
                            other => panic!("ASM maybe-unknown bool input lane holds {other:?}"),
                        }
                    }
                    buf[off..off + 2].copy_from_slice(&val.to_le_bytes());
                    buf[off + 2..off + 4].copy_from_slice(&known.to_le_bytes());
                }
                InputView::UBoolU(val, known) => {
                    buf[off..off + 2].copy_from_slice(&val.to_le_bytes());
                    buf[off + 2..off + 4].copy_from_slice(&known.to_le_bytes());
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
                            CellRepr::Bool | CellRepr::UBool => unreachable!("bool inputs are Bool/BoolU/UBool/UBoolU views"),
                        }
                    }
                }
            }
        }
    }
}

/// A thread's reusable scratch for one kernel: the packed input and the
/// output buffer. Kept per (thread, kernel) across calls so no unit
/// allocates them afresh.
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
                    outbuf: vec![0u8; k.compiled.out_bytes as usize],
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
    /// A NUMBER root into an interval column (the shape's union types a
    /// cell `Ival` where another outcome holds an interval), stored `[v, v]`.
    num_as_ival: Vec<(usize, usize)>,
    /// `(column, root, may the root be unknown)` (`AsmField::may_unknown`).
    bool_: Vec<(usize, usize, bool)>,
    uniform_num: Vec<(usize, u32)>,
    uniform_ival: Vec<(usize, (u32, u32))>,
    uniform_bool: Vec<(usize, u8)>,
}

impl BodyCols {
    fn of(body: &AsmBody, proto: &crate::frame::Slot, own: &Rt2) -> Result<Self> {
        use crate::frame::TCol;
        let (mut num, mut ival, mut num_as_ival, mut bool_) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
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
                (RootKind::Num, TCol::Num(_)) => num.push((ci, f.off)),
                (RootKind::Ival, TCol::Ival(_)) => ival.push((ci, f.off)),
                (RootKind::Num, TCol::Ival(_)) => num_as_ival.push((ci, f.off)),
                (RootKind::Bool, TCol::Bool(_)) => bool_.push((ci, f.off, f.may_unknown)),
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
                // The canonical output unknown an output widening writes
                // (`TCol::Bool`'s 2): a near level's floor `collideable` is
                // unknown in the outcomes where the player is statically far
                // from the floor, and per lane in the others.
                (TCol::Bool(_), AV::UBool) => uniform_bool.push((ci, 2)),
                (_, v) => anyhow::bail!("union column at cell {cell}: the outcome's uniform {v:?} does not fit its kind"),
            }
        }
        Ok(BodyCols { num, ival, num_as_ival, bool_, uniform_num, uniform_ival, uniform_bool })
    }

    /// Append lane `i` of the output buffer to `slot` as one row.
    #[inline]
    fn push_row(&self, slot: &mut crate::frame::Slot, buf: &[u8], i: usize, key: (u64, u64), cell: u32) {
        use crate::frame::TCol;
        let word = |base: usize| u32::from_le_bytes(buf[base..base + 4].try_into().unwrap());
        for &(ci, root) in &self.num {
            if let TCol::Num(v) = &mut slot.cols[ci].1 {
                v.push(word(root + i * 4));
            }
        }
        for &(ci, root) in &self.ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                v.push((word(root + i * 4), word(root + 64 + i * 4)));
            }
        }
        for &(ci, root) in &self.num_as_ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                let n = word(root + i * 4);
                v.push((n, n));
            }
        }
        for &(ci, root, may_unknown) in &self.bool_ {
            if let TCol::Bool(v) = &mut slot.cols[ci].1 {
                let val = u16::from_le_bytes([buf[root], buf[root + 1]]);
                let known = u16::from_le_bytes([buf[root + 2], buf[root + 3]]);
                if known >> i & 1 == 0 {
                    // FATAL, not a `2`, unless an output widening wrote it
                    // (`AsmField::may_unknown`): an undecided boolean here
                    // means a branch on an unknown reached the output without
                    // its `Known` premise declining the lane.
                    assert!(may_unknown, "emitted row holds an undecided boolean (column {ci}, cell {}, root {root}): no premise declined it", slot.cols[ci].0);
                    v.push(2);
                } else {
                    v.push((val >> i & 1) as u8);
                }
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
    /// A bool a lane may hold unknown (`CellRepr::UBool`): per lane, or
    /// uniform `(val, known)`.
    UBool(&'c [AV]),
    UBoolU(u16, u16),
    Any(&'c Col, CellRepr),
}

impl<'c> InputView<'c> {
    fn of(col: &'c Col, repr: CellRepr) -> Result<Self, String> {
        Ok(match (repr, col) {
            (CellRepr::Num, Col::N(v)) => InputView::Num(v),
            (CellRepr::Num, Col::U(AV::Num(n))) => InputView::NumU(n.as_raw_u32()),
            (CellRepr::Ival, Col::I(v)) => InputView::Ival(v),
            (CellRepr::Ival, Col::N(v)) => InputView::IvalOfNum(v),
            (CellRepr::Ival, Col::U(AV::Ival(a, b))) => InputView::IvalU(a.as_raw_u32(), b.as_raw_u32()),
            (CellRepr::Ival, Col::U(AV::Num(n))) => InputView::IvalU(n.as_raw_u32(), n.as_raw_u32()),
            (CellRepr::Bool, Col::V(v)) => InputView::Bool(v),
            (CellRepr::Bool, Col::U(v)) => match v {
                AV::Bool(b) => InputView::BoolU(if *b { 0xffff } else { 0 }),
                // A kernel reads this cell, and a bool input is DECIDED
                // (`CellRepr::Bool`): packing an unknown as false would
                // silently drop the true arm.
                other => return Err(format!("ASM bool input column holds {other:?}: a kernel reads a boolean the block does not decide")),
            },
            (CellRepr::Bool, other) => return Err(format!("ASM bool input column is {other:?}")),
            (CellRepr::UBool, Col::V(v)) => InputView::UBool(v),
            (CellRepr::UBool, Col::U(AV::Bool(b))) => InputView::UBoolU(if *b { 0xffff } else { 0 }, 0xffff),
            (CellRepr::UBool, Col::U(AV::UBool)) => InputView::UBoolU(0, 0),
            (CellRepr::UBool, other) => return Err(format!("ASM maybe-unknown bool input column is {other:?}")),
            (repr, other) => InputView::Any(other, repr),
        })
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
    let mut b = acc.clone_block();
    b.boundary(&super::boundary_ids());
    // A queue holds rows from SEVERAL kernel calls (the call's dedup cache
    // is per call), and the
    // flush collapses equal keys - so the append step's DISTINCT keys must
    // be the boundary's, duplicates allowed. Demanding no duplicates fired
    // on the exact-speed forward that reproduces every pinned gate
    // (width 56 vs 57, 2026-09-15).
    let mut distinct = acc.row_keys.clone();
    distinct.sort_unstable();
    distinct.dedup();
    let mut bkeys = b.row_keys.clone();
    bkeys.sort_unstable();
    if b.shape_hash == acc.shape_hash && b.structure == acc.structure && bkeys == distinct {
        return;
    }
    // Say WHICH rows and cells: each block's first rows, their keys, and the
    // cells whose values are not the same in every row of the append step's
    // block (the fields that tell its rows apart).
    let varying: Vec<usize> = (0..acc.cols.len())
        .filter(|&c| matches!(acc.structure.get(c), Some(celeste_engine::runtime2::Cell2::Val)))
        .filter(|&c| (1..acc.width).any(|i| acc.cols[c].at(i) != acc.cols[c].at(0)))
        .collect();
    let rows = |blk: &Rt2| -> String {
        (0..blk.width.min(8))
            .map(|i| {
                let cells: Vec<String> = varying.iter().filter(|&&c| c < blk.cols.len()).map(|&c| format!("{c}={:?}", blk.cols[c].at(i))).collect();
                format!("    row {i} key {:?}: {}", blk.row_keys.get(i), cells.join(" "))
            })
            .collect::<Vec<_>>()
            .join("\n")
    };
    panic!(
        "KERNEL KEY CHECK: shape {:#x}: the boundary disagrees with the append step \
         (width {} vs {}, shape {:#x} vs {:#x}, structure {}, keys {})\n  append step:\n{}\n  boundary:\n{}",
        acc.shape_hash,
        b.width,
        acc.width,
        b.shape_hash,
        acc.shape_hash,
        if b.structure == acc.structure { "same" } else { "DIFFERENT" },
        if b.row_keys == acc.row_keys { "same" } else { "DIFFERENT" },
        rows(&acc),
        rows(&b),
    );
}

/// `CELESTE_KERNEL_KEY_CHECK=1`: verify every finished acc against
/// `Rt2::boundary` (see `check_against_boundary`).
fn key_check_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_KERNEL_KEY_CHECK").is_some())
}

/// Whether kernels keep their fused graphs after assembly
/// (`CELESTE_KERNEL_EXPLAIN`): a decline then names the premise it failed.
fn kernel_graph_kept() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_KERNEL_EXPLAIN").is_some())
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

/// A tri-state mask read where it MAY hold - `val`, or not known - which
/// is the one polarity for both of a body's masks (plans/graph-model.md
/// section 5). `val` at +0, `known` at +2 of the 128-byte slot.
///
/// For `live` it means an UNKNOWN guard reads as live. An unknown guard is
/// a branch condition on a value the lane holds an INTERVAL of, which the
/// tracer could not merge away - the two arms have different shapes, or the
/// merged guard `Or(g & c, g & !c)` is itself unknown where `c` is. The lane
/// stands for points on both sides, so the body's hull covers some of them:
/// emitting the row over-approximates (the concrete search refutes the spurious
/// half), while NOT emitting it loses real successors (room (2,0)'s speed
/// buckets silently dropped lanes in 813 slices that way, 2026-09-14).
///
/// For `error` it means an undecided error is a decline: strict.
fn read_zb_may(buf: &[u8], off: usize) -> u16 {
    let val = u16::from_le_bytes([buf[off], buf[off + 1]]);
    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
    val | !known
}

/// The boundary's row-key mix seeds (see `runtime2::boundary_finish`); the
/// append folds the same cell_mix so its dedup key equals the boundary's.
use celeste_engine::runtime2::{KEY_SEED1, KEY_SEED2};

/// The `AV` the output root at byte offset `base`, of kind `kind`, holds for
/// lane `i`.
#[inline]
fn read_root_av(base: usize, kind: RootKind, buf: &[u8], i: usize) -> AV {
    match kind {
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
    }
}


/// The start room's assembled kernels, keyed by input shape hash, for one
/// rem-precision mode.
pub(crate) struct Registry {
    /// One kernel per input shape - with a region key
    /// (`trace::kernel::RegionGrid`), per reached (shape, region) - and a row
    /// with a key that is not here is a coverage gap.
    kernels: HashMap<(u64, Option<Region>), AsmKernel>,
    /// The region grid the set was built on.
    grid: Option<crate::trace::kernel::RegionGrid>,
}

use crate::trace::kernel::Region;

impl Registry {
    pub fn len(&self) -> usize {
        self.kernels.len()
    }

    /// Run `chunk` on the matching kernel; `false` if no kernel binds (a
    /// miss) or the chunk declined. With a region key the rows come in cell
    /// order, so in runs of one region, and each run goes to its region's
    /// kernel as a contiguous range.
    pub fn run_chunk(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        if self.grid.is_none() {
            return self.run_key(chunk, None, cell_in, lanes, sink);
        }
        // A region only where the shape has a PLAYER (`shapes::player_path`,
        // the walk's rule): a row's cell also locates a `player_spawn`, whose
        // shape's kernel has no region (room (3,0) f1: the spawn at y=128).
        let grid = self.grid.filter(|_| !chunk.player_objects(crate::compiled::ids()).is_empty());
        let key_of = |lane: usize| -> Option<Region> { grid.and_then(|g| g.of_cell(cell_in[lane])) };
        let mut lo = lanes.start;
        while lo < lanes.end {
            let k = key_of(lo);
            let mut hi = lo + 1;
            while hi < lanes.end && key_of(hi) == k {
                hi += 1;
            }
            if !self.run_key(chunk, k, cell_in, lo..hi, sink) {
                return false;
            }
            lo = hi;
        }
        true
    }

    fn run_key(
        &self,
        chunk: &Rt2,
        key: Option<Region>,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        match self.kernels.get(&(chunk.shape_hash, key)) {
            Some(k) => k.run(chunk, cell_in, lanes, sink),
            None => {
                // Diagnose a coverage gap: which (shape, key) has no
                // assembled kernel. Printed once per distinct one.
                static SEEN: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<(u64, Option<Region>)>>> =
                    std::sync::OnceLock::new();
                let seen = SEEN.get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()));
                if seen.lock().unwrap().insert((chunk.shape_hash, key)) {
                    eprintln!(
                        "[asm] MISS: no kernel for shape {:#018x} key {key:?} ({} lanes); registry has {} kernels over shapes {:?}",
                        chunk.shape_hash,
                        lanes.len(),
                        self.kernels.len(),
                        self.kernels.keys().map(|k| format!("{:#018x}", k.0)).collect::<std::collections::BTreeSet<_>>(),
                    );
                }
                false
            }
        }
    }

    /// Retrace the start room and assemble every shape for one lattice set
    /// (`opts`). `root` is the repo root (Lua sources + cart).
    pub fn build_for_start_room(root: &Path, opts: crate::abstraction::Level) -> Result<Registry> {
        let t_trace = std::time::Instant::now();
        let refs = crate::trace::kernel::lattice_kernel_refs(root, opts)
            .context("retracing the start room for ASM kernels")?;
        let trace_s = t_trace.elapsed().as_secs_f64();
        let t_asm = std::time::Instant::now();
        let n_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(refs.len().max(1));
        let next = std::sync::atomic::AtomicUsize::new(0);
        let refs_ref = &refs;
        let mut built: Vec<(usize, Result<(u64, Option<Region>, AsmKernel)>)> = std::thread::scope(|scope| {
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
                                let (key, r) = &refs_ref[si];
                                out.push((si, build_one_shape(r, si, &format!("_{si}")).map(|(shape, kernel)| (shape, *key, kernel))));
                            }
                            out
                        })
                        .expect("spawn asm shape builder")
                })
                .collect();
            handles.into_iter().flat_map(|h| h.join().unwrap()).collect()
        });
        built.sort_by_key(|(si, _)| *si);
        let mut kernels = HashMap::new();
        let mut first: HashMap<(u64, Option<Region>), usize> = HashMap::new();
        for (si, res) in built {
            let (shape, key, kernel) = res?;
            if kernels.insert((shape, key), kernel).is_some() {
                let slots = |i: usize| refs[i].1.frame.iface.slots.iter().map(crate::trace::iface::show).filter(|p| p.starts_with("__")).collect::<Vec<_>>();
                anyhow::bail!(
                    "two start-room frames hash to {shape:#x} with key {key:?} (frames {} and {si}: underscore slots {:?} / {:?})",
                    first[&(shape, key)],
                    slots(first[&(shape, key)]),
                    slots(si)
                );
            }
            first.insert((shape, key), si);
        }
        unify(&mut kernels)?;
        eprintln!(
            "[asm build] {} kernels: trace {:.2}s, assemble {:.2}s ({} workers)",
            kernels.len(),
            trace_s,
            t_asm.elapsed().as_secs_f64(),
            n_workers,
        );
        eprintln!("[asm build]   phases, summed over workers and concurrent builds: {}", crate::transpile::lower::build_profile());
        Ok(Registry { kernels, grid: crate::trace::kernel::region_grid() })
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
fn unify(by_shape: &mut HashMap<(u64, Option<Region>), AsmKernel>) -> Result<()> {
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
        apply_union(kernel, &unions)?;
        for t in &kernel.acc_templates {
            widened += t.union.cols.iter().zip(&t.own.cols).filter(|(u, o)| !matches!(u, Col::U(_)) && matches!(o, Col::U(_))).count();
        }
    }
    let templates: usize = by_shape.values().map(|k| k.acc_templates.len()).sum();
    let bodies: usize = by_shape.values().map(|k| k.bodies.len()).sum();
    let fused: usize = by_shape.values().map(|k| k.fused_nodes).sum();
    // Per kernel, largest first: bodies, the traced forks, the forks its bodies
    // ENUMERATE (a split that differs between two of its bodies), fused nodes,
    // the spill frame, the region.
    let mut per_shape: Vec<(usize, u8, usize, usize, String, String, String)> = by_shape
        .iter()
        .map(|((_, key), k)| {
            let first = k.bodies.first().map(|b| b.splits.clone()).unwrap_or_default();
            let enumerated = (0..k.forks as usize).filter(|&d| k.bodies.iter().any(|b| b.splits.get(d) != first.get(d))).count();
            let region = key.map_or("-".to_string(), |r| format!("({},{})", r.ix, r.iy));
            (k.bodies.len(), k.forks, enumerated, k.fused_nodes, format!("{:.1} MB", k.compiled.frame_bytes as f64 / 1048576.0), region, k.compiled.sym.clone())
        })
        .collect();
    per_shape.sort_unstable_by(|a, b| b.0.cmp(&a.0).then_with(|| a.5.cmp(&b.5)));
    per_shape.truncate(12);
    eprintln!(
        "[asm build] {} output shapes over {templates} outcome templates; {widened} template cells widened uniform -> typed by the union ({:.2} per template); {bodies} bodies, {fused} fused nodes over {} input shapes; largest kernels (bodies, forks, forks enumerated, fused nodes, frame, region): {:?}",
        unions.len(),
        widened as f64 / templates.max(1) as f64,
        by_shape.len(),
        per_shape
    );
    Ok(())
}

/// Give `kernel`'s outcome templates the shape unions and map its bodies'
/// roots onto the union columns.
fn apply_union(kernel: &mut AsmKernel, unions: &HashMap<u64, std::sync::Arc<Rt2>>) -> Result<()> {
    for t in &mut kernel.acc_templates {
        t.union = unions
            .get(&t.shape_hash)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("no union skeleton for output shape {:#x}", t.shape_hash))?;
    }
    let mut body_cols = Vec::with_capacity(kernel.bodies.len());
    for b in &kernel.bodies {
        let t = &kernel.acc_templates[b.outcome];
        let proto = crate::frame::Slot::new((*t.union).clone_block());
        body_cols.push(BodyCols::of(b, &proto, &t.own)?);
    }
    kernel.body_cols = body_cols;
    Ok(())
}

/// Assemble ONE start-room shape: fuse its graph, gcc + dlopen it, and map
/// its bodies' roots onto flat slots. Independent per shape, so
/// `build_for_start_room` runs these in parallel. `tag` distinguishes the
/// region-keyed kernels of one shape.
fn build_one_shape(r: &crate::trace::kernel::Reference, si: usize, tag: &str) -> Result<(u64, AsmKernel)> {
    let shape = r.frame.in_rt2.shape_hash_of();
    let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
    let t_fuse = std::time::Instant::now();
    let (fused, bodies, flat_roots, reprs) =
        crate::trace::emit::asm_fused_from(&r.bound, &r.lowered.spec)
            .with_context(|| format!("fusing shape {si} (hash {shape:#x})"))?;
    // THE ROW KEY is folded per EMITTED row, in the append step
    // (`AsmBody::key_words`, over `key_fields`), not in the kernel. In the
    // kernel every body hashed every lane, and a big kernel's bodies are
    // mostly not taken on a lane: room (3,0)'s square (6,5), 13,812 bodies,
    // ~22 rows per lane - the key chains were 40% of its 9.7M instructions and
    // most of its 9.3 MB spill frame (2026-09-18).
    // One output SLOT per DISTINCT root node. Bodies share most of their
    // roots (a fork configuration changes a handful of fields), and the
    // kernel writes every slot per slice: room (2,0)'s level-0 bucket set
    // has a 6,369-body shape whose bodies' roots concatenated were ~255k
    // slots - 32 MB of stores per 16-lane slice, most of them the same
    // value again (2026-09-14). `slot_of[k]` is the slot of concatenated
    // root k.
    let mut slot_of: Vec<usize> = Vec::with_capacity(flat_roots.len());
    let mut distinct: Vec<crate::transpile::graph::NodeId> = Vec::new();
    {
        let mut index: std::collections::HashMap<crate::transpile::graph::NodeId, usize> =
            std::collections::HashMap::with_capacity(flat_roots.len());
        for &n in &flat_roots {
            let slot = *index.entry(n).or_insert_with(|| {
                distinct.push(n);
                distinct.len() - 1
            });
            slot_of.push(slot);
        }
    }
    let flat_roots = distinct;
    crate::transpile::lower::build_add(7, t_fuse);
    let t_asm = std::time::Instant::now();
    let (compiled, loaded) = crate::transpile::asm::compile_and_load_reprs(
        &fused,
        &flat_roots,
        &format!("k{shape:016x}{tag}"),
        &reprs,
    )
    .with_context(|| format!("assembling shape {si} (hash {shape:#x})"))?;
    crate::transpile::lower::build_add(8, t_asm);

    // Which fused nodes read the canonical output unknown
    // (`AsmField::may_unknown`): one pass in node order, operands first.
    let reads_unknown_output: Vec<bool> = {
        let mut out = vec![false; fused.len()];
        for n in 0..fused.len() {
            let node = fused.get(n as crate::transpile::graph::NodeId);
            out[n] = node.op == crate::transpile::graph::Op::UnknownBool(u32::MAX) || node.args.iter().any(|a| out[*a as usize]);
        }
        out
    };
    // Map each fused body's roots onto their SLOTS: the bodies' roots
    // concatenated in order, through `slot_of`.
    let mut off = 0usize;
    let mut asm_bodies = Vec::with_capacity(bodies.len());
    for b in &bodies {
        let narc = r.bound.outcomes[b.outcome].arc.len();
        let nfields = b.roots.len() - 2 - narc; // roots = [fields.., error, live, arc..]
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
                off: compiled.root_offsets[slot_of[off + j]] as usize,
                kind: compiled.root_kinds[slot_of[off + j]],
                may_unknown: reads_unknown_output[b.roots[j] as usize],
            })
            .collect();
        let out_fields = &r.lowered.outs[b.outcome].fields;
        let key_fields = (0..nfields)
            .filter_map(|j| {
                if out_fields[j].konst_av.is_some() || out_fields[j].widen_uniform.is_some() {
                    return None;
                }
                let root = slot_of[off + j];
                // A number root of an INTERVAL column is stored `[v, v]`
                // (`BodyCols::num_as_ival`), which keys as the number.
                Some(KeyField::new(outputs[j].0 as u64, compiled.root_offsets[root] as usize, compiled.root_kinds[root]))
            })
            .collect();
        // Every body computes its transfer: an edge without one could not
        // be in the rotation graph.
        anyhow::ensure!(narc == 2 * crate::trace::verify::ARC_AXIS_ROOTS, "shape {si} outcome {}: {narc} transfer roots", b.outcome);
        let at = off + nfields + 2;
        let roots = std::array::from_fn(|k| (compiled.root_offsets[slot_of[at + k]] as usize, compiled.root_kinds[slot_of[at + k]]));
        let arc = ArcSlots { roots, fin: r.bound.outcomes[b.outcome].arc_fin };
        asm_bodies.push(AsmBody {
            outcome: b.outcome,
            splits: b.splits.clone(),
            fields,
            error_root: slot_of[off + nfields],
            error_off: compiled.root_offsets[slot_of[off + nfields]] as usize,
            live_off: compiled.root_offsets[slot_of[off + nfields + 1]] as usize,
            key_fields,
            pos: None,
            arc,
        });
        off += b.roots.len();
    }

    let acc_templates = (0..r.bound.outcomes.len())
        .map(|oi| acc_template(r, oi))
        .collect::<Result<Vec<_>>>()?;
    for b in asm_bodies.iter_mut() {
        b.pos = pos_sources(&acc_templates[b.outcome].build(), &b.fields)?;
    }

    let input_names = compiled
        .input_cells
        .iter()
        .map(|c| {
            r.frame
                .in_cells
                .iter()
                .position(|x| x == c)
                .map(|i| crate::trace::iface::show(&r.frame.iface.slots[i]))
                .unwrap_or_else(|| format!("cell {c}, not an input slot"))
        })
        .collect();
    Ok((
        shape,
        AsmKernel {
            loaded,
            compiled,
            bodies: asm_bodies,
            acc_templates,
            forks: r.bound.forks,
            body_cols: Vec::new(),
            fused_nodes: fused.len(),
            fused: if kernel_graph_kept() { fused } else { crate::transpile::graph::Graph::new() },
            flat_roots: if kernel_graph_kept() { flat_roots } else { Vec::new() },
            room,
            input_names,
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
            // An interval root's low lanes sit at the same offset as a
            // number's; its cell is the low corner
            // (`pos_graph::whole_i16_col`).
            return Ok(match f.kind {
                RootKind::Num | RootKind::Ival => Some(PosSrc::Root(f.off)),
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
fn cell_out(body: &AsmBody, buf: &[u8], i: usize) -> u32 {
    use crate::search::pos_graph::{cell_of, room_offset, NO_CELL};
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
    // The same labelling as `pos_graph::block_cells` (`room_offset`).
    let (ox, oy) = room_offset(rx, ry).unwrap_or_else(|e| panic!("emitted row: {e:#}"));
    cell_of(px as i32 + ox, py as i32 + oy).unwrap_or_else(|e| panic!("emitted row: {e}"))
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
        // A boundary-widened-to-uniform cell holds its widened value, whatever
        // the body computes for it: rem and the timers are constants in the
        // graph too, the unknown numbers are written `AV::UNum` (`emit::bind`)
        // - the value its key folds into `part`. The unknown booleans an output
        // widening writes are no fields (`ubool_cells` below).
        if let Some(av) = f.widen_uniform.or(f.konst_av) {
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

/// Kernel call shape: [0] calls, [1] rows offered, [2] slice-lanes executed
/// (16 per slice, so `[2] - [1]` is the padding). The per-call fixed cost
/// (accumulators, buffers, seen sets, boundary) is paid once per [0].
static CALL_STATS: [std::sync::atomic::AtomicU64; 7] =
    [const { std::sync::atomic::AtomicU64::new(0) }; 7];

/// Take (and reset) the call-shape counters.
pub(crate) fn take_call_stats() -> [u64; 7] {
    std::array::from_fn(|i| CALL_STATS[i].swap(0, std::sync::atomic::Ordering::Relaxed))
}

/// The kernel set of the process-global level, built on first use. ONE set
/// is resident (a set is ~1.7 GB, room (3,0)): a call at another level drops
/// it and builds that level's, on a big stack (the retrace's init interpret
/// recurses deeper than a worker thread's default); concurrent callers wait
/// for the build. A caller holds its set by `Arc` for the call, so a dropped
/// set lives on until its last chunk ends. Levels only change between
/// forwards (the objects ladder runs its levels in order).
pub(crate) fn registry() -> Option<std::sync::Arc<Registry>> {
    type Resident = Option<(crate::abstraction::Level, Option<std::sync::Arc<Registry>>)>;
    static SET: std::sync::Mutex<Resident> = std::sync::Mutex::new(None);
    let level = crate::abstraction::current_level();
    let mut set = SET.lock().unwrap();
    match &*set {
        Some((l, r)) if *l == level => r.clone(),
        _ => {
            *set = None;
            let built = build_registry_for_level(level).map(std::sync::Arc::new);
            *set = Some((level, built.clone()));
            built
        }
    }
}

/// THE KERNEL BUILD'S PURGE DELAY (2026-09-18). `safe-run.sh` has mimalloc
/// return freed pages at once (`MIMALLOC_PURGE_DELAY=0`, for the search's
/// resident memory), and under the build's tracer threads those purges were a
/// third of the room walk, in TLB shootdowns (room (3,0) on an 8 px grid: the
/// walk 26.0 s, 16.5 s with a 1 s delay). While any kernel set builds, purges
/// wait a second; the process's own setting is back when the last build ends.
struct BuildPurgeDelay;

/// Builds in progress, and the purge delay they found.
static BUILDS: std::sync::Mutex<(usize, std::os::raw::c_long)> = std::sync::Mutex::new((0, 0));

/// `mi_option_purge_delay` in mimalloc.h's `mi_option_t`, right after
/// `mi_option_eager_commit_delay` (14); libmimalloc-sys 0.1.44 binds no
/// constant for it. Checked against the environment's value in `start`.
const MI_OPTION_PURGE_DELAY: libmimalloc_sys::mi_option_t = 15;

impl BuildPurgeDelay {
    fn start() -> BuildPurgeDelay {
        let mut b = BUILDS.lock().unwrap();
        if b.0 == 0 {
            // SAFETY: plain option reads and writes, mimalloc's documented API.
            let before = unsafe { libmimalloc_sys::mi_option_get(MI_OPTION_PURGE_DELAY) };
            if let Some(env) = std::env::var("MIMALLOC_PURGE_DELAY").ok().and_then(|v| v.parse::<std::os::raw::c_long>().ok()) {
                assert_eq!(before, env, "mimalloc option {MI_OPTION_PURGE_DELAY} is not the purge delay MIMALLOC_PURGE_DELAY set");
            }
            b.1 = before;
            if (0..1000).contains(&before) {
                unsafe { libmimalloc_sys::mi_option_set(MI_OPTION_PURGE_DELAY, 1000) };
            }
        }
        b.0 += 1;
        BuildPurgeDelay
    }
}

impl Drop for BuildPurgeDelay {
    fn drop(&mut self) {
        let mut b = BUILDS.lock().unwrap();
        b.0 -= 1;
        if b.0 == 0 {
            unsafe { libmimalloc_sys::mi_option_set(MI_OPTION_PURGE_DELAY, b.1) };
            // And purge what the build freed under the delay: the builder
            // threads have exited, so nothing else runs their delayed purges.
            // (Not what made a built set big: that was every kernel keeping
            // its fused graph, `kernel_graph_kept`.)
            unsafe { libmimalloc_sys::mi_collect(true) };
        }
    }
}

fn build_registry_for_level(level: crate::abstraction::Level) -> Option<Registry> {
    let _purge = BuildPurgeDelay::start();
    let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
    let built = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || Registry::build_for_start_room(Path::new(&root), level))
        .expect("spawn asm-kernel builder")
        .join()
        .expect("asm-kernel builder panicked");
    match built {
        Ok(reg) => {
            eprintln!("ASM kernels ENABLED ({level}): {} start-room shapes assembled", reg.len());
            Some(reg)
        }
        Err(e) => panic!("building ASM kernels for {level}: {e:#}"),
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
