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
    /// The fork configuration (two bits per fork), for diagnostics.
    splits: Vec<u8>,
    fields: Vec<AsmField>,
    ok_root: usize,
    live_root: usize,
    /// The fields the row key folds (`key_words`): the body's varying ones -
    /// a per-row column the boundary does not widen to uniform - and every
    /// keyed one.
    key_fields: Vec<KeyField>,
    /// Where an emitted row's player x, player y, room x, room y come from
    /// (`search::pos_graph::block_cells`' inputs), so the append step can
    /// compute the row's position cell straight off the output buffer.
    /// `None`: this outcome has no player object, or its `x`/`y` is not a
    /// plain number (every row is `NO_CELL`, as `block_cells` says).
    pos: Option<[PosSrc; 4]>,
    /// Where this body's rows' speed key comes from (the bucket dispatch,
    /// `dkey_of`). `None` at an exact-speed level or without a player.
    dkey: Option<DKey>,
}

/// A body's rows' speed key: the dash constants per body; the buckets and
/// the dash class per ROW - the speed's key roots (`SplitKeyTab`: which
/// bucket a lane lands in is data, not a configuration) and `dash_time`.
#[derive(Clone, Copy)]
struct DKey {
    dash: [i32; 4],
    /// The output slot of spd.x's key root (low end first).
    bucket_x: usize,
    /// spd.y's, where the level buckets y (`SpdPrecision::buckets_y`); an
    /// x-only level has no y key root and every row's `by` is 0.
    bucket_y: Option<usize>,
    dash_time_slot: usize,
    /// The edge tables, looked up once (`spd_buckets::edges` locks).
    edges: [&'static [i32]; 2],
}

/// One field of a body's row key: its cell, the output slot it is read from,
/// and whether it is hashed as an INTERVAL. A field keyed on another node
/// (the speed under a bucket: stored tight, keyed on its bucket) is hashed
/// per row even where its stored value is constant (`acc_template` leaves it
/// out of `part`), and as an interval: the boundary keys it on `widen_to`'s
/// `AV::Ival` bucket, and `av_code` tells a number from a point interval - a
/// singleton bucket the fold pinned to `Const(t, t)` reads as a number (25 of
/// 262 exact marks lost, 2026-09-15).
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
    /// A number keyed as the point interval `[v, v]` (`KeyField`'s doc).
    NumAsIval,
    Ival,
    Bool,
}

impl KeyField {
    fn new(cell: u64, root: usize, kind: RootKind, span: bool) -> KeyField {
        use celeste_engine::runtime2::CELL_K;
        let ck = cell.wrapping_mul(CELL_K);
        let read = match (kind, span) {
            (RootKind::Num, false) => KeyRead::Num,
            (RootKind::Num, true) => KeyRead::NumAsIval,
            (RootKind::Ival, _) => KeyRead::Ival,
            (RootKind::Bool, _) => KeyRead::Bool,
        };
        KeyField { c: [KEY_SEED1 ^ ck, KEY_SEED2 ^ ck], root, read }
    }

    /// `runtime2::av_code` of lane `i`'s value, off the output slot.
    #[inline]
    fn code(&self, buf: &[u8], i: usize) -> u64 {
        use celeste_engine::runtime2::mix64;
        let base = self.root * 128;
        let word = |o: usize| u32::from_le_bytes(buf[o..o + 4].try_into().unwrap()) as u64;
        let ival = |lo: u64, hi: u64| 2u64 << 56 | lo << 24 ^ mix64(hi << 1);
        match self.read {
            KeyRead::Num => 1u64 << 56 | word(base + i * 4),
            KeyRead::NumAsIval => {
                let v = word(base + i * 4);
                ival(v, v)
            }
            KeyRead::Ival => ival(word(base + i * 4), word(base + 64 + i * 4)),
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

    /// Row `i`'s speed key: its buckets and dash class read off the row,
    /// the dash constants only where it is dashing.
    #[inline]
    fn dkey_of(&self, buf: &[u8], i: usize) -> Option<SpeedKey> {
        let k = self.dkey?;
        let word = |slot: usize| u32::from_le_bytes(buf[slot * 128 + i * 4..slot * 128 + i * 4 + 4].try_into().unwrap()) as i32;
        let dashing = word(k.dash_time_slot) > 0;
        let bucket = |axis: usize, slot: usize| k.edges[axis].partition_point(|&e| e <= word(slot)) as u16;
        let by = k.bucket_y.map_or(0, |s| bucket(1, s));
        Some(SpeedKey { bx: bucket(0, k.bucket_x), by, dashing, dash: if dashing { k.dash } else { [0; 4] } })
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
    /// DEBUG (CELESTE_ASM_EVAL_CHECK): the fused graph + its flat roots +
    /// the room, so `run` can re-evaluate the SAME fused graph with the pure
    /// interval evaluator (`eval_narrow_top_in`) per lane and diff it against the
    /// assembled kernel's output. Separates an ASM-codegen bug (asm != eval)
    /// from a fused-graph bug (asm == eval, both != interpreter). Also what a
    /// decline's explanation evaluates (`explain_ok`). KEPT ONLY under
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
    /// 64-lane id group, in any order - the bucket dispatch sorts a group's
    /// lanes by speed bucket), keeping only the lanes of `only`. What the
    /// contiguous `run` above and the bucket dispatch share.
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
        let body_cols = &self.body_cols;
        // Pure-kernel throughput floor (CELESTE_KERNEL_DRYRUN=1): pack the
        // inputs and call the kernel, then discard - no dedup, no
        // materialize. Produces no rows, so it is a MEASUREMENT MODE ONLY
        // (the frame comes out empty).
        let dryrun = {
            static DR: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
            *DR.get_or_init(|| std::env::var_os("CELESTE_KERNEL_DRYRUN").is_some())
        };

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
            if dryrun {
                sc.put_back();
                return true;
            }
            if eval_check_on() {
                self.eval_check(chunk, lanes, outbuf);
            }
            let valid = (((1u32 << n) - 1) as u16) & only;
            if liveok_on() {
                // Per-lane coverage census: OR of live&ok over all bodies
                // (lane kept by SOME outcome), and OR of live&!ok (lane
                // DECLINED by some outcome). A lane that is neither kept nor
                // declined is DROPPED as not-live - the graph considers it dead.
                let (mut kept, mut declined, mut any_live) = (0u16, 0u16, 0u16);
                for body in &self.bodies {
                    let ok = read_zb_holds(outbuf, body.ok_root);
                    let live = read_zb_live(outbuf, body.live_root);
                    kept |= live & ok & valid;
                    declined |= live & !ok & valid;
                    any_live |= live & valid;
                }
                let dropped = valid & !kept & !declined;
                eprintln!(
                    "[liveok] slice lanes={n} valid={valid:04x} kept={kept:04x} \
                     declined={declined:04x} dropped(not-live)={dropped:04x} any_live={any_live:04x}"
                );
                if dropped != 0 {
                    self.explain_dropped(chunk, lanes[dropped.trailing_zeros() as usize], dropped.trailing_zeros() as usize, outbuf);
                }
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
                let live = read_zb_live(outbuf, body.live_root);
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
                        "[kernel] shape {:#x} body of outcome {} declined lanes {:#06x} (live {:#06x} ok {:#06x}):\n{rows}",
                        chunk.shape_hash, body.outcome, declined, live, ok
                    );
                    match self.flat_roots.get(body.ok_root) {
                        Some(&ok_node) => self.explain_ok(chunk, lanes[declined.trailing_zeros() as usize], ok_node),
                        None => eprintln!("[kernel] rerun with CELESTE_KERNEL_EXPLAIN=1 for the premise the lane failed"),
                    }
                    sc.put_back();
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
                        let (h1, h2) = body.key_words(outbuf, i);
                        let key = (
                            runtime2::mix64(template.part.0.wrapping_add(h1)),
                            runtime2::mix64(template.part.1.wrapping_add(h2)),
                        );
                        n_unique += 1;
                        let cout = cell_out(body, outbuf, i, start);
                        if targets.contains(key, cout) {
                            sink.hit(lanes[i]);
                            hit_lanes |= 1 << i;
                        }
                    }
                    continue;
                }
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
                        // THE SPEED HULL: the same key with another speed
                        // fragment is not the same row. Still queued: its
                        // hull widens in place. Already flushed (an id in
                        // the cache): pushed again, and the flush's door
                        // decides - inside the hull, the old id; outside, a
                        // grown row.
                        let hull = cols.lane_hull(outbuf, i);
                        match slice_base {
                            Some(b) if r & RowCache::ID_FLAG != 0 && hull.is_none() => {
                                sink.direct_edge(r & !RowCache::ID_FLAG, b, glane(i));
                                continue;
                            }
                            Some(_) if r & RowCache::ID_FLAG != 0 => {}
                            Some(_) if r & RowCache::DROP_FLAG != 0 => continue,
                            // The row was flushed (a stale ref, only if
                            // the cache lost the flush's write-back): push
                            // it again; the flush merges duplicates by key.
                            Some(b) if !sink.mark_pred(r, b, glane(i)) => {}
                            Some(_) => {
                                if let (Some(h), Some(s)) = (hull, cols.spd) {
                                    sink.hull_union_at(r, s[0].0, s[1].0, h);
                                }
                                continue;
                            }
                            None => continue,
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
                    let q = sink.queue(template.shape_hash, cout, body.dkey_of(outbuf, i), || (*template.union).clone_block());
                    cols.push_row(&mut sink.slots[q], outbuf, i, key, cout);
                    if let Some(b) = slice_base {
                        sink.slots[q].pred_base.push(b);
                        sink.slots[q].pred_mask.push(1u64 << (glane(i)));
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
        }
        sc.put_back();
        for (i, n) in [n_bodies, n_bodies_taken, n_lanes, n_unique].into_iter().enumerate() {
            CALL_STATS[3 + i].fetch_add(n, std::sync::atomic::Ordering::Relaxed);
        }
        true
    }

    /// from the interpreter. Prints at most a few mismatches per chunk.
    /// A lane no body is live for: evaluate the fused graph for it and
    /// print every fork node's value and each body's (live, ok) under
    /// the evaluator against the kernel's. Diagnostic for the liveok
    /// census (`CELESTE_ASM_LIVEOK`), first such lane per call.
    fn explain_dropped(&self, chunk: &Rt2, lane: usize, i: usize, outbuf: &[u8]) {
        use crate::transpile::graph::{Op, Val};
        static SHOWN: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        if SHOWN.fetch_add(1, std::sync::atomic::Ordering::Relaxed) >= 2 {
            return;
        }
        let ids = crate::compiled::ids();
        let mut desc = String::new();
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
        eprintln!("[dropped] shape {:#x} lane {lane}:{desc}", chunk.shape_hash);
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
                eprintln!("[dropped]   graph evaluation failed: {e:#}");
                return;
            }
        };
        for id in 0..self.fused.len() as crate::transpile::graph::NodeId {
            let node = self.fused.get(id);
            if matches!(node.op, Op::SplitOk(_) | Op::FragOk(_) | Op::Frag(_)) {
                let x = node.args[0];
                eprintln!("[dropped]   {id} {:?} = {:?}; operand {x} {:?} = {:?}", node.op, vals[id as usize], self.fused.get(x).op, vals[x as usize]);
            }
        }
        let mut n_live_eval = 0;
        for (bi, body) in self.bodies.iter().enumerate() {
            let live_asm = read_zb_live(outbuf, body.live_root) & (1 << i) != 0;
            let ok_asm = read_zb_holds(outbuf, body.ok_root) & (1 << i) != 0;
            let live_ev = vals[self.flat_roots[body.live_root] as usize];
            let ok_ev = vals[self.flat_roots[body.ok_root] as usize];
            if live_ev != Val::Bool(Some(false)) || live_asm {
                n_live_eval += 1;
                if n_live_eval <= 6 {
                    eprintln!("[dropped]   body {bi} outcome {} splits {:?}: live eval {:?} asm {live_asm}; ok eval {:?} asm {ok_asm}", body.outcome, body.splits, live_ev, ok_ev);
                    eprintln!("[dropped]   the live leaves the evaluator cannot decide:");
                    self.explain_bool(chunk, lane, self.flat_roots[body.live_root]);
                }
            }
        }
        eprintln!("[dropped]   {n_live_eval} bodies live under the evaluator or the kernel");
    }

    /// Descend a boolean DAG from `root` through And/Or/Not along the
    /// undecided operands and print the undecided LEAVES (comparisons,
    /// forks, cart lookups) with their operands' values.
    fn explain_bool(&self, chunk: &Rt2, lane: usize, root: crate::transpile::graph::NodeId) {
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
        let Ok(vals) = self.fused.eval_narrow_top_in(&cells, &self.room) else { return };
        let mut stack = vec![root];
        let mut seen = std::collections::HashSet::new();
        let mut shown = 0;
        while let Some(n) = stack.pop() {
            if !seen.insert(n) || shown >= 10 {
                continue;
            }
            let node = self.fused.get(n);
            let v = vals[n as usize];
            match node.op {
                Op::And => stack.extend(node.args.iter().copied().filter(|&a| vals[a as usize] != Val::Bool(Some(true)))),
                Op::Or => stack.extend(node.args.iter().copied().filter(|&a| vals[a as usize] != Val::Bool(Some(false)))),
                Op::Not => stack.push(node.args[0]),
                Op::Sel => stack.extend(node.args.iter().copied()),
                _ => {
                    shown += 1;
                    let args: Vec<String> = node
                        .args
                        .iter()
                        .map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize]))
                        .collect();
                    eprintln!("[kernel]   leaf {} {:?} = {:?}; args {}", n, node.op, v, args.join(", "));
                }
            }
        }
    }

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
            if vals[n as usize] != Val::Bool(Some(true)) && shown < 6 {
                shown += 1;
                let args: Vec<String> = node
                    .args
                    .iter()
                    .map(|&a| format!("{}={:?}<{:?}>", a, self.fused.get(a).op, vals[a as usize]))
                    .collect();
                eprintln!("[kernel]   ok conjunct {} {:?} = {:?}; args {}", n, node.op, vals[n as usize], args.join(", "));
                // The whole sub-DAG under it (bounded), when asked: what
                // `CELESTE_EXPLAIN_DEPTH=N` prints is every node within N
                // steps of the conjunct, once, with its value.
                if let Some(depth) = std::env::var("CELESTE_EXPLAIN_DEPTH").ok().and_then(|v| v.parse::<usize>().ok()) {
                    let mut frontier = vec![n];
                    let mut printed = std::collections::HashSet::new();
                    for _ in 0..depth {
                        let mut next = Vec::new();
                        for id in frontier {
                            if !printed.insert(id) {
                                continue;
                            }
                            let nd = self.fused.get(id);
                            if matches!(nd.op, Op::Const(..) | Op::ConstBool(_)) {
                                continue;
                            }
                            let args: Vec<String> = nd.args.iter().map(|&a| format!("{a}")).collect();
                            eprintln!("[kernel]     {} {:?} = {:?}; args {}", id, nd.op, vals[id as usize], args.join(","));
                            next.extend(nd.args.iter().copied());
                        }
                        frontier = next;
                    }
                }
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

    fn eval_check(&self, chunk: &Rt2, lanes: &[usize], outbuf: &[u8]) {
        let n = lanes.len();
        use crate::transpile::graph::Val;
        static PRINTED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        for i in 0..n {
            // Build the per-lane input cells from the block columns.
            let mut cells: std::collections::HashMap<u32, Val> = Default::default();
            for &cell in &self.compiled.input_cells {
                let v = match chunk.cols[cell as usize].at(lanes[i]) {
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
                        eprintln!("[eval-check] lane {} eval err: {e:#}", lanes[i]);
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
                            lanes[i],
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
    /// A NUMBER root into an interval column, stored as `[v, v]`: the
    /// speed under a bucket is an interval column of the shape, and an
    /// outcome that sets it exactly (`spd.x = 0` on a wall, the spring's
    /// `spd.y = -3`) computes a number for it (the speed hull, 2026-09-15).
    num_as_ival: Vec<(usize, usize)>,
    bool_: Vec<(usize, usize)>,
    /// The speed hull's columns when the level buckets the speed: the
    /// queue column and output root of `spd.x` and `spd.y`, each root a
    /// number (`is_num`) or an interval. `None` at exact speed.
    spd: Option<[(usize, usize, bool); 2]>,
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
                (RootKind::Num, TCol::Num(_)) => num.push((ci, f.root)),
                (RootKind::Ival, TCol::Ival(_)) => ival.push((ci, f.root)),
                (RootKind::Num, TCol::Ival(_)) => num_as_ival.push((ci, f.root)),
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
                // A per-row column never stores an undecided boolean: bool
                // inputs are decided (`InputView::of` refuses an unknown), a
                // branch on an unknown declines through its `Known` premise,
                // and the one widened-to-unknown boolean (the held-button
                // trails) is a uniform output column, not a root.
                (TCol::Bool(_), AV::UBool) => anyhow::bail!("union column at cell {cell} is a uniform undecided boolean"),
                (_, v) => anyhow::bail!("union column at cell {cell}: the outcome's uniform {v:?} does not fit its kind"),
            }
        }
        let spd = proto.speed_typed_cols().and_then(|(tx, ty)| {
            let find = |ci: usize| -> Option<(usize, usize, bool)> {
                ival.iter().find(|(c, _)| *c == ci).map(|&(c, r)| (c, r, false))
                    .or_else(|| num_as_ival.iter().find(|(c, _)| *c == ci).map(|&(c, r)| (c, r, true)))
            };
            Some([find(tx)?, find(ty)?])
        });
        Ok(BodyCols { num, ival, num_as_ival, bool_, spd, uniform_num, uniform_ival, uniform_bool })
    }

    /// Lane `i`'s speed hull off the output buffer (`spd` columns).
    #[inline]
    fn lane_hull(&self, buf: &[u8], i: usize) -> Option<crate::search::door::Hull> {
        let s = self.spd?;
        let word = |base: usize| u32::from_le_bytes(buf[base..base + 4].try_into().unwrap()) as i32;
        let range = |(_, root, is_num): (usize, usize, bool)| -> (i32, i32) {
            let lo = word(root * 128 + i * 4);
            if is_num {
                (lo, lo)
            } else {
                (lo, word(root * 128 + 64 + i * 4))
            }
        };
        let (x, y) = (range(s[0]), range(s[1]));
        Some([x.0, x.1, y.0, y.1])
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
        for &(ci, root) in &self.num_as_ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                let n = word(root * 128 + i * 4);
                v.push((n, n));
            }
        }
        for &(ci, root) in &self.bool_ {
            if let TCol::Bool(v) = &mut slot.cols[ci].1 {
                let val = u16::from_le_bytes([buf[root * 128], buf[root * 128 + 1]]);
                let known = u16::from_le_bytes([buf[root * 128 + 2], buf[root * 128 + 3]]);
                // FATAL, not a `2`: an undecided boolean here means a branch
                // on an unknown reached the output without its `Known`
                // premise declining the lane - and a stored one would be
                // refused as an input next frame (`InputView::of`).
                assert!(
                    known >> i & 1 != 0,
                    "emitted row holds an undecided boolean (column {ci}, root {root}): no premise declined it"
                );
                v.push((val >> i & 1) as u8);
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
    let exact = registry().map(|r| r.exact_boundary).unwrap_or(false);
    let mut b = acc.clone_block();
    if exact {
        b.boundary_exact();
    } else {
        // The key hashes the speed BUCKET (the row stores the hull).
        if let Some(w) = crate::interpreter::abstraction::spd_precision().width_log2() {
            b.widen_to(&super::boundary_ids(), 0, Some((w, crate::interpreter::abstraction::spd_precision().buckets_y())), (1, 1), false, false, false);
        }
        b.boundary(&super::boundary_ids());
    }
    // A queue holds rows from SEVERAL kernel calls (the call's dedup cache
    // is per call; a bucketed chunk is one call per speed key), and the
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

/// Whether kernels keep their fused graphs after assembly: for the eval check,
/// and for `CELESTE_KERNEL_EXPLAIN` (a decline names the premise it failed).
fn kernel_graph_kept() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| eval_check_on() || std::env::var_os("CELESTE_KERNEL_EXPLAIN").is_some())
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
/// A body's `live` per lane: KNOWN-TRUE or UNKNOWN. An unknown guard is
/// a branch condition on a value the lane holds an INTERVAL of (a
/// bucketed speed: `spd.x ~= 0` on `[0, 1)`), which the tracer could not
/// merge away - the two arms have different shapes, or the merged guard
/// `Or(g & c, g & !c)` is itself unknown where `c` is. The lane stands
/// for points on both sides, so the body's hull covers some of them:
/// emitting the row over-approximates (a finer level, where the speed is
/// exact and `c` decided, refutes the spurious half), while NOT emitting
/// it loses real successors. Before this read unknown as not-live, and
/// room (2,0) under `CELESTE_SPD_LADDER=level0` silently dropped lanes
/// in 813 slices of its first 32 frames (2026-09-14). `ok` keeps the
/// strict reading: an undecided OBLIGATION is a decline.
fn read_zb_live(buf: &[u8], root: usize) -> u16 {
    let off = root * 128;
    let val = u16::from_le_bytes([buf[off], buf[off + 1]]);
    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
    val | !known
}

fn read_zb_holds(buf: &[u8], root: usize) -> u16 {
    let off = root * 128;
    let val = u16::from_le_bytes([buf[off], buf[off + 1]]);
    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
    val & known
}

/// The boundary's row-key mix seeds (see `runtime2::boundary_finish`); the
/// append folds the same cell_mix so its dedup key equals the boundary's.
use celeste_engine::runtime2::{KEY_SEED1, KEY_SEED2};

/// The `AV` a field root holds for lane `i` (for the dedup fold).
fn read_field_av(f: &AsmField, buf: &[u8], i: usize) -> AV {
    read_root_av(f.root, f.kind, buf, i)
}

/// The `AV` output slot `root` of kind `kind` holds for lane `i`.
#[inline]
fn read_root_av(root: usize, kind: RootKind, buf: &[u8], i: usize) -> AV {
    let base = root * 128;
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
    /// One kernel per (input shape, speed key): the key is `None` at an
    /// exact-speed level and for a shape without a player; at a bucketed
    /// level every key the key fixpoint reached (`trace::kernel::
    /// key_fixpoint`) has its kernel here, built up front, and a row with
    /// a key that is not here is a coverage gap.
    /// With a region key (`trace::kernel::RegionGrid`), one per reached
    /// (shape, region) too.
    kernels: HashMap<(u64, KernelKey), AsmKernel>,
    /// The rung-agnostic / exact sets go through `boundary_exact`; the
    /// level-0 set through `boundary`.
    exact_boundary: bool,
    /// The grid width `2^w` of a bucketed level's edge tables.
    w: Option<u8>,
    /// The region grid the set was built on.
    grid: Option<crate::trace::kernel::RegionGrid>,
}

use crate::trace::kernel::{KernelKey, SpeedKey};

/// The speed key of row `lane` of `rt2` at the current level: `None` at an
/// exact-speed level or without a player (the reference path's rows).
pub fn speed_key_of_row(rt2: &Rt2, lane: usize) -> Option<SpeedKey> {
    let w = crate::interpreter::abstraction::spd_precision().width_log2()?;
    speed_keys(rt2, w).map(|k| k[lane])
}

/// Per lane of `chunk`, what its kernel is specialized on; `None` for a
/// shape without a player.
pub(crate) fn speed_keys(chunk: &Rt2, w: u8) -> Option<Vec<SpeedKey>> {
    // `WidthLog2X`: spd.y is exact and not part of the key.
    let buckets_y = crate::interpreter::abstraction::spd_precision().buckets_y();
    let ids = crate::compiled::ids();
    let hulls = chunk.speed_hulls(ids)?;
    let obj = *chunk.player_objects(ids).first()?;
    let num_col = |f: u32| -> Option<u32> { chunk.obj_field_cell(obj, f) };
    let xy = |f: u32| -> Option<(u32, u32)> {
        let pc = chunk.obj_field_cell(obj, f)?;
        let Col::U(AV::Ptr(sub)) = chunk.cols[pc as usize] else { return None };
        Some((chunk.obj_field_cell(sub, ids.f_x)?, chunk.obj_field_cell(sub, ids.f_y)?))
    };
    let dt = num_col(ids.f_dash_time);
    let (target, accel) = (xy(ids.f_dash_target), xy(ids.f_dash_accel));
    let raw = |c: Option<u32>, lane: usize| -> i32 {
        match c.map(|c| chunk.cols[c as usize].at(lane)) {
            Some(AV::Num(n)) => n.as_raw_u32() as i32,
            Some(other) => panic!("dispatch key cell holds {other:?}, not a number"),
            None => 0,
        }
    };
    Some(
        (0..chunk.width)
            .map(|lane| {
                let h = hulls[lane];
                let bx = celeste_core::spd_buckets::index(h[0], w, 0);
                let by = if buckets_y { celeste_core::spd_buckets::index(h[2], w, 1) } else { 0 };
                debug_assert!(
                    celeste_core::spd_buckets::index(h[1], w, 0) == bx && (!buckets_y || celeste_core::spd_buckets::index(h[3], w, 1) == by),
                    "lane {lane}: speed hull {h:?} straddles a bucket edge"
                );
                let dashing = raw(dt, lane) > 0;
                let dash = if dashing {
                    [raw(target.map(|t| t.0), lane), raw(target.map(|t| t.1), lane), raw(accel.map(|t| t.0), lane), raw(accel.map(|t| t.1), lane)]
                } else {
                    [0; 4]
                };
                SpeedKey { bx, by, dashing, dash }
            })
            .collect(),
    )
}

impl Registry {
    pub fn len(&self) -> usize {
        self.kernels.len()
    }

    /// Run `chunk` on the matching kernel; `false` if no kernel binds (a
    /// miss) or the chunk declined. At a bucketed level the rows of a
    /// block come in RUNS of one speed key (the queues are keyed by it),
    /// and each run goes to its key's kernel as a contiguous range; with a
    /// region key the rows come in cell order, so in runs of one region.
    pub fn run_chunk(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        if self.w.is_none() && self.grid.is_none() {
            return self.run_key(chunk, KernelKey { speed: None, region: None }, cell_in, lanes, sink);
        }
        let keys = self.w.and_then(|w| speed_keys(chunk, w));
        // A region only where the shape has a PLAYER (`shapes::player_path`,
        // the walk's rule): a row's cell also locates a `player_spawn`, whose
        // shape's kernel has no region (room (3,0) f1: the spawn at y=128).
        let grid = self.grid.filter(|_| !chunk.player_objects(crate::compiled::ids()).is_empty());
        let key_of = |lane: usize| -> KernelKey {
            KernelKey { speed: keys.as_ref().map(|k| k[lane]), region: grid.and_then(|g| g.of_cell(cell_in[lane])) }
        };
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
        key: KernelKey,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        match self.kernels.get(&(chunk.shape_hash, key)) {
            Some(k) => k.run(chunk, cell_in, lanes, sink),
            None => {
                // Diagnose a coverage gap: which (shape, key) has no
                // assembled kernel. Printed once per distinct one.
                static SEEN: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<(u64, KernelKey)>>> =
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
    /// (`opts`). `root` is the repo root (Lua sources + cart). `decide =
    /// true` matches the Rust kernel so the assembled compute is the same
    /// fused graph. `exact_boundary` selects `boundary_exact` for the
    /// rung-agnostic / exact sets.
    /// How many kernels the set has.
    pub fn kernel_count(&self) -> usize {
        self.kernels.len()
    }

    pub fn build_for_start_room(
        root: &Path,
        opts: crate::trace::shapes::WalkOpts,
        exact_boundary: bool,
    ) -> Result<Registry> {
        let t_trace = std::time::Instant::now();
        let refs = crate::trace::kernel::lattice_kernel_refs(root, opts)
            .context("retracing the start room for ASM kernels")?;
        let w = opts.spd.width_log2();
        // Of the level being built, not the process-global one: the sets of
        // several levels build in parallel.
        let buckets_y = opts.spd.buckets_y();
        let trace_s = t_trace.elapsed().as_secs_f64();
        let t_asm = std::time::Instant::now();
        let n_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4)
            .min(refs.len().max(1));
        let next = std::sync::atomic::AtomicUsize::new(0);
        let refs_ref = &refs;
        let mut built: Vec<(usize, Result<(u64, KernelKey, AsmKernel)>)> = std::thread::scope(|scope| {
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
                                out.push((si, build_one_shape(r, si, &format!("_{si}"), key.speed, key.region, w, buckets_y).map(|(shape, _, kernel)| (shape, *key, kernel))));
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
        for (_si, res) in built {
            let (shape, key, kernel) = res?;
            if kernels.insert((shape, key), kernel).is_some() {
                anyhow::bail!("two start-room frames hash to {shape:#x} with key {key:?}");
            }
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
        Ok(Registry { kernels, exact_boundary, w, grid: crate::trace::kernel::region_grid_for(opts)? })
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
fn unify(by_shape: &mut HashMap<(u64, KernelKey), AsmKernel>) -> Result<()> {
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
            let region = key.region.map_or("-".to_string(), |r| format!("({},{})", r.ix, r.iy));
            (k.bodies.len(), k.forks, enumerated, k.fused_nodes, format!("{:.1} MB", k.compiled.frame_bytes as f64 / 1048576.0), region, k.compiled.sym.clone())
        })
        .collect();
    per_shape.sort_unstable_by(|a, b| b.0.cmp(&a.0).then_with(|| a.5.cmp(&b.5)));
    if std::env::var_os("CELESTE_KERNEL_REPORT").is_some() {
        // The symbol names the kernel's .so in `target/asm-scratch` (what a
        // profile of a live run attributes samples to).
        for s in &per_shape {
            eprintln!("[asm kernel] region {} bodies {} forks {} enumerated {} fused nodes {} frame {} {}", s.5, s.0, s.1, s.2, s.3, s.4, s.6);
        }
    }
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

/// What one kernel is made of (`CELESTE_KERNEL_DUMP=<substring of its symbol>`):
/// its region, its traced forks and what each splits, per outcome the forks its
/// bodies enumerate, the fused graph's ops, and the codegen's frame. A
/// diagnostic for why a kernel is as big as it is.
fn dump_kernel(
    r: &crate::trace::kernel::Reference,
    sym: &str,
    region: Option<crate::trace::kernel::Region>,
    fused: &crate::transpile::graph::Graph,
    bodies: &[crate::trace::emit::AsmBody],
    slots: usize,
    compiled: &crate::transpile::asm::Compiled,
) {
    use crate::transpile::graph::Op;
    let op_name = |op: &Op| -> String { format!("{op:?}").split('(').next().unwrap_or("").to_string() };
    let region = region.map_or("-".to_string(), |g| format!("({},{})", g.ix, g.iy));
    eprintln!(
        "[kernel dump] {sym} region {region}: {} outcomes, {} bodies, {} traced forks, {} fused nodes, {slots} root slots, {} spill slots, frame {:.1} MB, {} asm lines",
        r.bound.outcomes.len(),
        bodies.len(),
        r.frame.forks,
        fused.len(),
        compiled.spill_slots,
        compiled.frame_bytes as f64 / 1048576.0,
        compiled.asm.lines().count()
    );
    // Each fork: its arity and what it splits (the operand's op, one level).
    let g = &r.bound.graph;
    for d in 0..r.frame.forks {
        let site = (0..g.len() as crate::transpile::graph::NodeId).find(|&n| {
            matches!(g.get(n).op, Op::Split(x) | Op::SplitInt(x) | Op::SplitTab(x) if x == d)
        });
        let what = match site {
            Some(n) => {
                let node = g.get(n);
                let arg = g.get(node.args[0]);
                let args: Vec<String> = arg.args.iter().map(|a| op_name(&g.get(*a).op)).collect();
                format!("{} of {}({})", op_name(&node.op), op_name(&arg.op), args.join(", "))
            }
            None => "not in the bound graph".to_string(),
        };
        let origin = r.frame.fork_origins.iter().find(|(x, _)| *x == d).map_or("-", |(_, o)| o.as_str());
        eprintln!("[kernel dump]   fork {d}: {} ways, {what}, made for {origin}", r.frame.fork_ways[d as usize]);
    }
    // Per outcome: its bodies and the forks they enumerate.
    for oi in 0..r.bound.outcomes.len() {
        let mine: Vec<&crate::trace::emit::AsmBody> = bodies.iter().filter(|b| b.outcome == oi).collect();
        let Some(first) = mine.first() else { continue };
        let enumerated: Vec<usize> = (0..first.splits.len()).filter(|&d| mine.iter().any(|b| b.splits[d] != first.splits[d])).collect();
        let reps: std::collections::BTreeSet<u8> = mine.iter().map(|b| b.frees).collect();
        eprintln!(
            "[kernel dump]   outcome {oi}: {} bodies over {} button reps, forks enumerated {:?}, {} fields",
            mine.len(),
            reps.len(),
            enumerated,
            r.bound.outcomes[oi].outputs.len()
        );
        // What keeps bodies apart: the distinct root tuples when a body is
        // compared by its fields only, then with `live`, with `ok`, and whole.
        // Roots are `[fields.., ok, live, keys..]`.
        let nf = r.bound.outcomes[oi].outputs.len();
        let distinct = |f: &dyn Fn(&[crate::transpile::graph::NodeId]) -> Vec<crate::transpile::graph::NodeId>| -> usize {
            mine.iter().map(|b| f(&b.roots)).collect::<std::collections::BTreeSet<_>>().len()
        };
        eprintln!(
            "[kernel dump]     distinct roots: fields {}, fields+live {}, fields+ok {}, all {}; distinct live {}, distinct ok {}",
            distinct(&|x| x[..nf].to_vec()),
            distinct(&|x| x[..nf].iter().chain(std::iter::once(&x[nf + 1])).copied().collect()),
            distinct(&|x| x[..nf + 1].to_vec()),
            distinct(&|x| x.to_vec()),
            distinct(&|x| vec![x[nf + 1]]),
            distinct(&|x| vec![x[nf]]),
        );
        // Which fields keep bodies apart: per field, its distinct nodes.
        let mut varying: Vec<(usize, usize)> = (0..nf)
            .map(|j| (mine.iter().map(|b| b.roots[j]).collect::<std::collections::BTreeSet<_>>().len(), j))
            .filter(|(n, _)| *n > 1)
            .collect();
        varying.sort_by(|a, b| b.0.cmp(&a.0));
        let names: Vec<String> = varying
            .iter()
            .take(10)
            .map(|(n, j)| {
                let path = r.frame.outs.get(oi).and_then(|o| o.fields.get(*j)).map(|f| crate::trace::iface::show(&f.0)).unwrap_or_else(|| format!("field {j}"));
                format!("{path} {n}")
            })
            .collect();
        eprintln!("[kernel dump]     varying fields ({} of {nf}): {}", varying.len(), names.join(", "));
        // Two bodies with the same fields and different `ok`: the conjuncts in
        // one `ok` and not the other.
        let conjuncts = |n: crate::transpile::graph::NodeId| -> std::collections::BTreeSet<crate::transpile::graph::NodeId> {
            let (mut out, mut stack) = (std::collections::BTreeSet::new(), vec![n]);
            while let Some(x) = stack.pop() {
                let node = fused.get(x);
                if node.op == Op::And {
                    stack.extend(node.args.iter().copied());
                } else {
                    out.insert(x);
                }
            }
            out
        };
        let mut ok_by_fields: std::collections::BTreeMap<Vec<crate::transpile::graph::NodeId>, crate::transpile::graph::NodeId> = Default::default();
        for b in &mine {
            let ok = b.roots[nf];
            match ok_by_fields.get(&b.roots[..nf].to_vec()) {
                Some(&other) if other != ok => {
                    let (a, c) = (conjuncts(other), conjuncts(ok));
                    let only = |x: &std::collections::BTreeSet<crate::transpile::graph::NodeId>, y: &std::collections::BTreeSet<crate::transpile::graph::NodeId>| -> Vec<String> {
                        x.difference(y)
                            .take(4)
                            .map(|n| {
                                let node = fused.get(*n);
                                let args: Vec<String> = node.args.iter().map(|q| op_name(&fused.get(*q).op)).collect();
                                format!("{}({})", op_name(&node.op), args.join(", "))
                            })
                            .collect()
                    };
                    eprintln!(
                        "[kernel dump]     same fields, different ok: {} and {} conjuncts, {} shared; only in the first: {:?}; only in the second: {:?}",
                        a.len(),
                        c.len(),
                        a.intersection(&c).count(),
                        only(&a, &c),
                        only(&c, &a)
                    );
                    break;
                }
                Some(_) => {}
                None => {
                    ok_by_fields.insert(b.roots[..nf].to_vec(), ok);
                }
            }
        }
        // The first two bodies whose fields agree but whose `live` differs:
        // what the two `live` roots are.
        let mut by_fields: std::collections::BTreeMap<Vec<crate::transpile::graph::NodeId>, crate::transpile::graph::NodeId> = Default::default();
        for b in &mine {
            let live = b.roots[nf + 1];
            match by_fields.get(&b.roots[..nf].to_vec()) {
                Some(&other) if other != live => {
                    let show = |n: crate::transpile::graph::NodeId| {
                        let node = fused.get(n);
                        let args: Vec<String> = node.args.iter().map(|a| op_name(&fused.get(*a).op)).collect();
                        format!("{}({})", op_name(&node.op), args.join(", "))
                    };
                    eprintln!("[kernel dump]     same fields, different live: {} against {}", show(other), show(live));
                    break;
                }
                Some(_) => {}
                None => {
                    by_fields.insert(b.roots[..nf].to_vec(), live);
                }
            }
        }
    }
    // CELESTE_KERNEL_SPLIT=1: per outcome and per fork its bodies enumerate, up
    // to 20 body pairs that differ ONLY in that fork (same button rep), how many
    // fields differ, and how many of those are `Sel(c, x, y)` with one arm the
    // other body's value - what splitting the body on `c` and fusing would take
    // away.
    if std::env::var_os("CELESTE_KERNEL_SPLIT").is_some() {
        for oi in 0..r.bound.outcomes.len() {
            let mine: Vec<&crate::trace::emit::AsmBody> = bodies.iter().filter(|b| b.outcome == oi).collect();
            let Some(first) = mine.first() else { continue };
            let nf = r.bound.outcomes[oi].outputs.len();
            for d in (0..first.splits.len()).filter(|&d| mine.iter().any(|b| b.splits[d] != first.splits[d])) {
                let (mut pairs, mut differing, mut sel_else_other) = (0usize, 0usize, 0usize);
                'pairs: for (i, a) in mine.iter().enumerate() {
                    for b in &mine[i + 1..] {
                        let one_fork = a.frees == b.frees && a.splits.iter().zip(&b.splits).enumerate().all(|(x, (p, q))| (x == d) != (p == q));
                        if !one_fork {
                            continue;
                        }
                        pairs += 1;
                        for j in 0..nf {
                            let (x, y) = (a.roots[j], b.roots[j]);
                            if x == y {
                                continue;
                            }
                            differing += 1;
                            let arm_is = |n: crate::transpile::graph::NodeId, other: crate::transpile::graph::NodeId| {
                                let node = fused.get(n);
                                node.op == Op::Sel && (node.args[1] == other || node.args[2] == other)
                            };
                            if arm_is(x, y) || arm_is(y, x) {
                                sel_else_other += 1;
                            }
                        }
                        if pairs >= 20 {
                            break 'pairs;
                        }
                    }
                }
                if pairs == 0 {
                    continue;
                }
                let origin = r.frame.fork_origins.iter().find(|(x, _)| *x as usize == d).map_or("move/table", |(_, o)| o.as_str());
                eprintln!(
                    "[kernel split] outcome {oi} ({} bodies): fork {d} ({origin}): {pairs} pairs, {:.1} fields differ per pair, {:.0}% of them Sel with the other body's value as an arm",
                    mine.len(),
                    differing as f64 / pairs as f64,
                    sel_else_other as f64 * 100.0 / differing.max(1) as f64
                );
            }
        }
    }
    // CELESTE_KERNEL_DIFF=<outcome>: two of its bodies with the same button rep
    // whose fork configurations differ in ONE fork, and where their fields
    // differ, walked down to the nodes that actually diverge.
    // `<outcome>`: the two bodies differ in one fork; `<outcome>:<bit>`: in that
    // button alone, forks equal (bits 0..5 = left, right, up, down, jump, dash).
    if let Some(spec) = std::env::var("CELESTE_KERNEL_DIFF").ok() {
        // `<outcome>@<fork>`: one fork apart, in that fork.
        let (spec, only_fork) = match spec.split_once('@') {
            Some((s, f)) => (s.to_string(), f.parse::<usize>().ok()),
            None => (spec.clone(), None),
        };
        let (want, button) = match spec.split_once(':') {
            Some((o, b)) => (o.parse::<usize>().ok(), b.parse::<u8>().ok()),
            None => (spec.parse::<usize>().ok(), None),
        };
        let want = want.unwrap_or(usize::MAX);
        let mine: Vec<&crate::trace::emit::AsmBody> = bodies.iter().filter(|b| b.outcome == want).collect();
        let pair = mine.iter().enumerate().find_map(|(i, a)| {
            mine[i + 1..]
                .iter()
                .find(|b| match button {
                    Some(bit) => a.splits == b.splits && a.frees ^ b.frees == 1 << bit,
                    None => {
                        let apart: Vec<usize> = a.splits.iter().zip(&b.splits).enumerate().filter(|(_, (x, y))| x != y).map(|(i, _)| i).collect();
                        a.frees == b.frees && apart.len() == 1 && only_fork.is_none_or(|f| apart[0] == f)
                    }
                })
                .map(|b| (*a, *b))
        });
        match pair {
            None => eprintln!("[kernel diff] outcome {want}: no two bodies differ in exactly {}", if button.is_some() { "that button" } else { "one fork" }),
            Some((a, b)) => {
                let what = match button {
                    Some(bit) => format!("button bit {bit}, reps {:#08b} against {:#08b}", a.frees, b.frees),
                    None => {
                        let d = a.splits.iter().zip(&b.splits).position(|(x, y)| x != y).unwrap_or(0);
                        let origin = r.frame.fork_origins.iter().find(|(x, _)| *x as usize == d).map_or("-", |(_, o)| o.as_str());
                        format!("button rep {}: fork {d} ({origin}) = {} against {}", a.frees, a.splits[d], b.splits[d])
                    }
                };
                let nf = r.bound.outcomes[want].outputs.len();
                let differing: Vec<usize> = (0..nf).filter(|&j| a.roots[j] != b.roots[j]).collect();
                eprintln!(
                    "[kernel diff] outcome {want}, {what}; {} of {nf} fields differ, live {}, ok {}",
                    differing.len(),
                    if a.roots[nf + 1] == b.roots[nf + 1] { "same" } else { "differs" },
                    if a.roots[nf] == b.roots[nf] { "same" } else { "differs" },
                );
                type Memo = std::collections::HashMap<crate::transpile::graph::NodeId, Option<crate::transpile::graph::Pieces>>;
                type Seeds = std::collections::HashMap<crate::transpile::graph::NodeId, (i64, i64)>;
                // The region's seeded cells, by node of the fused graph: what the
                // range analysis starts from.
                let seeds: Seeds = (0..fused.len() as crate::transpile::graph::NodeId)
                    .filter_map(|n| match fused.get(n).op {
                        Op::Cell(c) => r.lowered.ranges.get(&c).map(|(lo, hi)| (n, (*lo as i64, *hi as i64))),
                        _ => None,
                    })
                    .collect();
                let mut memo: Memo = Default::default();
                // A node as an expression, `depth` levels deep; a number with its
                // static range's hull in pixels, `{lo..hi}`, where it has one.
                fn show(g: &crate::transpile::graph::Graph, seeds: &Seeds, memo: &mut Memo, n: crate::transpile::graph::NodeId, depth: usize) -> String {
                    let node = g.get(n);
                    let mut op = format!("{:?}", node.op);
                    if !matches!(node.op, Op::Const(..)) {
                        if let Some(p) = crate::transpile::graph::pieces_of(g, seeds, memo, n) {
                            if let (Some(lo), Some(hi)) = (p.first(), p.last()) {
                                op = format!("{op}{{{}..{}}}", lo.0 as f64 / 65536.0, hi.1 as f64 / 65536.0);
                            }
                        }
                    }
                    if node.args.is_empty() || depth == 0 {
                        return if node.args.is_empty() { op } else { format!("{op}(..)") };
                    }
                    let args: Vec<String> = node.args.iter().map(|x| show(g, seeds, memo, *x, depth - 1)).collect();
                    format!("{op}({})", args.join(", "))
                }
                // Down both expressions while they agree on the op, to where they diverge.
                fn diverge(g: &crate::transpile::graph::Graph, seeds: &Seeds, memo: &mut Memo, x: crate::transpile::graph::NodeId, y: crate::transpile::graph::NodeId, depth: usize, out: &mut Vec<String>) {
                    if x == y || out.len() >= 6 {
                        return;
                    }
                    let (nx, ny) = (g.get(x), g.get(y));
                    if nx.op != ny.op || nx.args.len() != ny.args.len() || depth == 0 {
                        let deep = std::env::var("CELESTE_KERNEL_DIFF_DEPTH").ok().and_then(|v| v.parse().ok()).unwrap_or(3);
                        out.push(format!("{}  AGAINST  {}", show(g, seeds, memo, x, deep), show(g, seeds, memo, y, deep)));
                        return;
                    }
                    for (p, q) in nx.args.iter().zip(&ny.args) {
                        diverge(g, seeds, memo, *p, *q, depth - 1, out);
                    }
                }
                // `CELESTE_KERNEL_CONE=N`: the second body's field as a DAG, its
                // first N nodes breadth-first, each once with its range.
                let cone = std::env::var("CELESTE_KERNEL_CONE").ok().and_then(|v| v.parse::<usize>().ok());
                for &j in differing.iter().take(4) {
                    let path = r.frame.outs.get(want).and_then(|o| o.fields.get(j)).map(|f| crate::trace::iface::show(&f.0)).unwrap_or_else(|| format!("field {j}"));
                    let mut out = Vec::new();
                    diverge(fused, &seeds, &mut memo, a.roots[j], b.roots[j], 12, &mut out);
                    eprintln!("[kernel diff]   {path}:");
                    for line in out {
                        eprintln!("[kernel diff]     {line}");
                    }
                    if let Some(max) = cone {
                        // `CELESTE_KERNEL_CONE_ROOT=<node>`: from that node instead.
                        let root = std::env::var("CELESTE_KERNEL_CONE_ROOT").ok().and_then(|v| v.parse().ok()).unwrap_or(b.roots[j]);
                        let (mut queue, mut seen) = (std::collections::VecDeque::from([root]), std::collections::HashSet::new());
                        while let Some(n) = queue.pop_front() {
                            if seen.len() >= max || !seen.insert(n) {
                                continue;
                            }
                            let node = fused.get(n);
                            let args: Vec<String> = node.args.iter().map(|x| format!("#{x}")).collect();
                            eprintln!("[kernel cone]     #{n} = {}({})", show(fused, &seeds, &mut memo, n, 0).trim_end_matches("(..)"), args.join(", "));
                            queue.extend(node.args.iter().copied());
                        }
                    }
                }
            }
        }
    }
    // The fused graph's ops, most common first.
    let mut hist: std::collections::BTreeMap<String, usize> = Default::default();
    for n in 0..fused.len() as crate::transpile::graph::NodeId {
        *hist.entry(op_name(&fused.get(n).op)).or_default() += 1;
    }
    let mut hist: Vec<(String, usize)> = hist.into_iter().collect();
    hist.sort_by(|a, b| b.1.cmp(&a.1));
    let top: Vec<String> = hist.iter().take(20).map(|(o, c)| format!("{o} {c}")).collect();
    eprintln!("[kernel dump]   ops: {}", top.join(", "));
}

/// Assemble ONE start-room shape: fuse its graph, gcc + dlopen it, and map
/// its bodies' roots onto flat slots. Independent per shape, so
/// `build_for_start_room` runs these in parallel. `tag` distinguishes the
/// bucket-specialized kernels of one shape.
fn build_one_shape(
    r: &crate::trace::kernel::Reference,
    si: usize,
    tag: &str,
    key: Option<SpeedKey>,
    // For the report only (`CELESTE_KERNEL_DUMP`).
    region: Option<crate::trace::kernel::Region>,
    w: Option<u8>,
    // Does the level bucket spd.y (`SpdPrecision::buckets_y`): only then does
    // a body have a y key root to dispatch on.
    buckets_y: bool,
) -> Result<(u64, Option<SpeedKey>, AsmKernel)> {
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
    if let Some(want) = std::env::var_os("CELESTE_KERNEL_DUMP") {
        let (sym, want) = (format!("k{shape:016x}{tag}"), want.to_string_lossy().to_string());
        let at = region.map(|g| format!("({},{})", g.ix, g.iy));
        if sym.contains(&want) || at.as_deref() == Some(want.as_str()) {
            dump_kernel(r, &sym, region, &fused, &bodies, flat_roots.len(), &compiled);
        }
    }

    // Map each fused body's roots onto their SLOTS: the bodies' roots
    // concatenated in order, through `slot_of`.
    let mut off = 0usize;
    let mut asm_bodies = Vec::with_capacity(bodies.len());
    for (bi, b) in bodies.iter().enumerate() {
        let nfields = b.roots.len() - 2 - r.bound.outcomes[b.outcome].keys.len(); // roots = [fields.., ok, live, keys..]
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
                root: slot_of[off + j],
                kind: compiled.root_kinds[slot_of[off + j]],
            })
            .collect();
        // The body's rows' speed key (the queue they land in): the bucket
        // its speed KEY roots name (a constant per body: the table fork's
        // fragment, or the one bucket) and its constant dash outputs.
        let dkey = match (w, crate::trace::shapes::player_path(&r.frame.outs[b.outcome].st)) {
            (Some(w), Some(pl)) => {
                let paths = crate::trace::kernel::key_paths_of(&pl);
                let out_fields = &r.frame.outs[b.outcome].fields;
                let field_index = |p: &crate::trace::iface::Path| -> Result<usize> {
                    out_fields.iter().position(|(q, _, _)| q == p).ok_or_else(|| anyhow::anyhow!("shape {si}: no output field {}", crate::trace::iface::show(p)))
                };
                let okeys = &r.bound.outcomes[b.outcome].keys;
                // The speed's key root: the bucket, per lane (its low end at
                // the slot's base, a number or an interval alike).
                let bucket_slot = |axis: usize| -> Result<usize> {
                    let fi = field_index(&paths[axis])?;
                    let k = okeys.iter().position(|(j, _)| *j == fi).ok_or_else(|| anyhow::anyhow!("shape {si} body {bi}: spd axis {axis} has no key node"))?;
                    Ok(slot_of[off + nfields + 2 + k])
                };
                // `dash_time` is read per ROW at append time (`dash_time_slot`):
                // a mid-dash body's `dash_time - 1` ends the dash on some
                // lanes and not others. The dash constants are per body,
                // read only where the row is still dashing.
                let dash_time_slot = slot_of[off + field_index(&paths[2])?];
                let mut dash = [0i32; 4];
                for (i, p) in paths[3..].iter().enumerate() {
                    let root = b.roots[field_index(p)?];
                    // Unchanged from the (unbounded) input on a lane that is
                    // not dashing: only a constant where it is read.
                    dash[i] = match fused.get(root).op {
                        crate::transpile::graph::Op::Const(lo, _) => lo,
                        _ => 0,
                    };
                }
                Some(DKey {
                    dash,
                    bucket_x: bucket_slot(0)?,
                    bucket_y: if buckets_y { Some(bucket_slot(1)?) } else { None },
                    dash_time_slot,
                    edges: [celeste_core::spd_buckets::edges(w, 0), celeste_core::spd_buckets::edges(w, 1)],
                })
            }
            _ => None,
        };
        let okeys = &r.bound.outcomes[b.outcome].keys;
        let out_fields = &r.lowered.outs[b.outcome].fields;
        let key_fields = (0..nfields)
            .filter_map(|j| {
                let keyed = okeys.iter().position(|(fi, _)| *fi == j);
                if keyed.is_none() && (out_fields[j].konst_av.is_some() || out_fields[j].widen_uniform.is_some()) {
                    return None;
                }
                let root = match keyed {
                    Some(k) => slot_of[off + nfields + 2 + k],
                    None => slot_of[off + j],
                };
                Some(KeyField::new(outputs[j].0 as u64, root, compiled.root_kinds[root], keyed.is_some()))
            })
            .collect();
        asm_bodies.push(AsmBody {
            outcome: b.outcome,
            splits: b.splits.clone(),
            fields,
            ok_root: slot_of[off + nfields],
            live_root: slot_of[off + nfields + 1],
            key_fields,
            pos: None,
            dkey,
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
        key,
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
            // A position bucket (`Span`, the rung below level 0) is an
            // interval root whose low lanes sit at the same offset as a
            // number's; its cell is the bucket's low corner
            // (`pos_graph::whole_i16_col`).
            return Ok(match f.kind {
                RootKind::Num | RootKind::Ival => Some(PosSrc::Root(f.root * 128)),
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
    // The cells keyed on another node (the speed's bucket) are hashed per
    // lane by the kernel whatever their stored value (`build_one_shape`),
    // never folded in here at that value.
    let keyed: Vec<usize> = r.bound.outcomes[oi].keys.iter().map(|(fi, _)| r.bound.outcomes[oi].outputs[*fi].0 as usize).collect();
    let empty = t.build();
    let mut part1: u64 = shape_hash;
    let mut part2: u64 = 0xa076_1d64_78bd_642f ^ shape_hash;
    for (c, cell) in empty.structure.iter().enumerate() {
        if !matches!(cell, celeste_engine::runtime2::Cell2::Val) || keyed.contains(&c) {
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
    registry_for(crate::interpreter::abstraction::current_level())
}

/// One kernel set per LEVEL (rem rung x spd precision), each built once,
/// on first use or by `prebuild` - the level is an explicit input of the
/// build, not read from the process-global precision, so the sets can
/// be built in parallel.
fn registry_for(level: crate::interpreter::abstraction::Level) -> Option<&'static Registry> {
    use crate::interpreter::abstraction::{RemPrecision, SpdPrecision};
    // Widths 1..=20 both axes, 1..=20 x-only, and exact.
    const SPD_SLOTS: usize = 41;
    const POS_SLOTS: usize = 4;
    // Held buttons exact or unknown.
    const HELD_SLOTS: usize = 2;
    // The fly fruit exact or unknown, the fall floors exact or unknown.
    const FRUIT_SLOTS: usize = 2;
    const FLOORS_SLOTS: usize = 2;
    static REGS: [std::sync::OnceLock<Option<Registry>>; 17 * SPD_SLOTS * POS_SLOTS * HELD_SLOTS * FRUIT_SLOTS * FLOORS_SLOTS] =
        [const { std::sync::OnceLock::new() }; 17 * SPD_SLOTS * POS_SLOTS * HELD_SLOTS * FRUIT_SLOTS * FLOORS_SLOTS];
    let rem_slot = match level.rem {
        RemPrecision::Exact => 16,
        RemPrecision::Bits(b) => (b as usize).min(16),
    };
    let spd_slot = match level.spd {
        SpdPrecision::Exact => 40,
        SpdPrecision::WidthLog2(w) => (w as usize).clamp(1, 20) - 1,
        SpdPrecision::WidthLog2X(w) => 20 + (w as usize).clamp(1, 20) - 1,
    };
    let pos_slot = (level.pos.x.clamp(1, 2) as usize - 1) + 2 * (level.pos.y.clamp(1, 2) as usize - 1);
    let held_slot = level.held.is_unknown() as usize;
    let fruit_slot = level.fruit.is_unknown() as usize;
    let floors_slot = level.floors.is_unknown() as usize;
    let slot = ((((rem_slot * SPD_SLOTS + spd_slot) * POS_SLOTS + pos_slot) * HELD_SLOTS + held_slot) * FRUIT_SLOTS + fruit_slot) * FLOORS_SLOTS + floors_slot;
    REGS[slot].get_or_init(|| build_registry_for_rung(level)).as_ref()
}

/// Build every rung's kernel set now, all rungs at once (one builder
/// thread per rung, each assembling its shapes in parallel). The ladder
/// calls this up front: lazily, the 17 builds landed one at a time inside
/// the first frame of each level - 82 s of a 524 s room (1,0) run.
pub fn prebuild(levels: &[crate::interpreter::abstraction::Level]) {
    let t = std::time::Instant::now();
    std::thread::scope(|scope| {
        for &l in levels {
            scope.spawn(move || {
                registry_for(l);
            });
        }
    });
    eprintln!("[asm build] {} levels prebuilt in {:.1} s", levels.len(), t.elapsed().as_secs_f64());
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

fn build_registry_for_rung(level: crate::interpreter::abstraction::Level) -> Option<Registry> {
    use crate::interpreter::abstraction::RemPrecision;
    use crate::trace::shapes::WalkOpts;
    let _purge = BuildPurgeDelay::start();
    let rem = level.rem;
    let root = std::env::var("CELESTE_ROOT").unwrap_or_else(|_| ".".to_string());
    let mode = super::dispatch::traced_mode_for(rem);
    let (opts, exact) = match mode {
        super::dispatch::TracedMode::Level0 => (WalkOpts::level0(level.spd, level.pos).with_held(level.held.is_unknown()).with_fruit(level.fruit.is_unknown()).with_floors(level.floors.is_unknown()), false),
        super::dispatch::TracedMode::Level0Agnostic => {
            // Phase 1: the opt-in rung-specific variant bakes the rem
            // widening into the graph (`ladder_widen`), still through
            // `boundary_exact` (it emits the widened rem and keys the
            // emitted field). Default `LADDER` is unchanged.
            let opts = match (super::dispatch::widen_in_graph(), rem) {
                (true, RemPrecision::Bits(b)) => WalkOpts::ladder_widen(b, level.spd),
                _ => WalkOpts::LADDER,
            };
            (opts.with_held(level.held.is_unknown()), true)
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
            eprintln!("ASM kernels ENABLED ({mode:?}, {level}): {} start-room shapes assembled", reg.len());
            if let Some(only) = crate::trace::kernel::kernel_only() {
                eprintln!("[kernel only] built the kernels of {only:?}; exiting (CELESTE_KERNEL_ONLY)");
                std::process::exit(0);
            }
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
