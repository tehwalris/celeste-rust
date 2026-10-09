//! Runtime AVX-512 kernels: retrace the start room's shapes at startup,
//! assemble each (shape, region)'s fused fork-free graph, and dispatch
//! chunks to it. The APPEND step emits each `live & !error` lane at the
//! boundary: canonical structure, its row key exactly as
//! `Rt2::boundary_finish` (`CELESTE_KERNEL_KEY_CHECK=1`), its pos-graph edge
//! and its transfer.

use crate::search::arc_edges::{decode_axis, RawAxis};
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

/// One output field of a body: its output cell, slot and kind.
struct AsmField {
    cell: usize,
    /// The root's byte offset in the output buffer.
    off: usize,
    kind: RootKind,
    /// A boolean an output widening may write UNKNOWN; any other undecided
    /// boolean is a branch on an unknown no premise declined: fatal.
    may_unknown: bool,
}

/// One fused body (outcome, fork configuration): its fields and `error` /
/// `live` roots; a lane is emitted iff `live & !error`.
struct AsmBody {
    outcome: usize,
    /// The fork configuration (two bits per fork), for diagnostics.
    splits: Vec<u8>,
    fields: Vec<AsmField>,
    /// `error`'s slot (`CELESTE_KERNEL_EXPLAIN`).
    error_root: usize,
    error_off: usize,
    live_off: usize,
    /// The per-row fields the row key folds; the rest are in the template's `part`.
    key_fields: Vec<KeyField>,
    /// Sources of player x/y and room x/y, for the row's cell (`pos_sources`).
    pos: Option<[PosSrc; 4]>,
    /// The transfer roots (`search::arc_edges`).
    arc: ArcSlots,
    /// Earlier bodies of the same outcome one fork away, with the root
    /// slots whose equality makes a lane's emission theirs (`DupRef`).
    dup_refs: Vec<DupRef>,
}

/// An earlier body `body` and the (its slot, my slot, kind) pairs that
/// differ in slot: every root the emission reads (stored fields, key
/// fields, position sources, transfer roots). Equal on a lane in all of
/// them, the two bodies emit the identical row, edge and transfer.
struct DupRef {
    body: usize,
    pairs: Vec<(usize, usize, RootKind)>,
}

impl DupRef {
    /// The lanes of `cand` on which every pair is equal.
    #[inline]
    fn eq_mask(&self, buf: &[u8], cand: u16) -> u16 {
        let block = |o: usize| -> [u32; 16] {
            let b: &[u8; 64] = buf[o..o + 64].try_into().unwrap();
            std::array::from_fn(|i| u32::from_le_bytes([b[4 * i], b[4 * i + 1], b[4 * i + 2], b[4 * i + 3]]))
        };
        let eq16 = |a: usize, b: usize| -> u16 {
            let (x, y) = (block(a), block(b));
            let mut m = 0u16;
            for i in 0..16 {
                m |= ((x[i] == y[i]) as u16) << i;
            }
            m
        };
        let mut m = cand;
        for &(a, b, kind) in &self.pairs {
            m &= match kind {
                RootKind::Num => eq16(a, b),
                RootKind::Ival => eq16(a, b) & eq16(a + 64, b + 64),
                RootKind::Bool => {
                    let w = |o: usize| (u16::from_le_bytes([buf[o], buf[o + 1]]), u16::from_le_bytes([buf[o + 2], buf[o + 3]]));
                    let ((va, ka), (vb, kb)) = (w(a), w(b));
                    !((va ^ vb) | (ka ^ kb))
                }
            };
            if m == 0 {
                break;
            }
        }
        m
    }
}

/// A body's transfer roots (`FrameOut::arc`): per axis took, pre, frag, ox,
/// fin as (offset, kind); `fin` whether the outcome ends with a player.
/// `ArcSlots::words`: two words per transfer root, then the outcome's
/// `fin` flag - the words alone do not decode: without a fin root its words
/// are 0 and the axis has no `fin`, with one they are `fin = Some((0, 0))`.
/// Keyed on the words alone, `ForwardSink::xfer_id_raw`'s cache gave a lane
/// whichever body's transfer was interned first (a scheduling-dependent edge,
/// room (1,0) f61, 2026-10-09).
pub(crate) const RAW_WORDS: usize = 4 * crate::trace::verify::ARC_AXIS_ROOTS + 1;
pub(crate) type RawWords = [u32; RAW_WORDS];

#[derive(Clone, Copy)]
struct ArcSlots {
    roots: [(usize, RootKind); 2 * crate::trace::verify::ARC_AXIS_ROOTS],
    fin: bool,
}

impl ArcSlots {
    /// Lane `i`'s transfer as plain words off the output slots: per root
    /// its (low, high) 16.16 words, a boolean root's (value, known) bits,
    /// the fin roots 0 where the outcome has none, and the `fin` flag last.
    /// (Grouping a slice's lanes by their words, word-major, cost more than
    /// it saved at ~7 live lanes a body: room (6,2) f57 emit 104 -> 181 s.)
    #[inline]
    fn words(&self, buf: &[u8], i: usize) -> RawWords {
        let w = |o: usize| u32::from_le_bytes(buf[o..o + 4].try_into().unwrap());
        let mut out = [0u32; RAW_WORDS];
        for (k, &(off, kind)) in self.roots.iter().enumerate() {
            if !self.fin && k % crate::trace::verify::ARC_AXIS_ROOTS == 4 {
                continue;
            }
            let (lo, hi) = match kind {
                RootKind::Num => (w(off + i * 4), w(off + i * 4)),
                RootKind::Ival => (w(off + i * 4), w(off + 64 + i * 4)),
                RootKind::Bool => {
                    let v = u16::from_le_bytes([buf[off], buf[off + 1]]);
                    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
                    ((v >> i & 1) as u32, (known >> i & 1) as u32)
                }
            };
            out[2 * k] = lo;
            out[2 * k + 1] = hi;
        }
        out[RAW_WORDS - 1] = self.fin as u32;
        out
    }

    /// The raw transfer of `words`. An undecided `took` is FATAL: the
    /// record would be an unchecked claim about the remainder.
    fn raw(&self, words: &RawWords, chunk: &Rt2, lane: usize, outcome: usize) -> (RawAxis, RawAxis) {
        let axis = |a: usize| -> RawAxis {
            let base = a * crate::trace::verify::ARC_AXIS_ROOTS;
            let pair = |k: usize| (words[2 * (base + k)] as i32, words[2 * (base + k) + 1] as i32);
            assert!(
                self.roots[base].1 == RootKind::Bool && words[2 * base + 1] == 1,
                "arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}'s `took` is not a decided boolean",
                chunk.shape_hash
            );
            RawAxis { took: words[2 * base] == 1, pre: pair(1), frag: pair(2), ox: pair(3), fin: self.fin.then(|| pair(4)) }
        };
        (axis(0), axis(1))
    }

    /// The record's pair of a raw transfer (`raw`).
    fn decode(raw: &(RawAxis, RawAxis), chunk: &Rt2, lane: usize, outcome: usize) -> crate::search::arc_edges::Pair {
        let axis = |a: usize, r: &RawAxis| {
            decode_axis(r).unwrap_or_else(|e| panic!("arc edges: shape {:#x} outcome {outcome} lane {lane}: axis {a}: {e} (raw {r:?})", chunk.shape_hash))
        };
        (axis(0, &raw.0), axis(1, &raw.1))
    }
}

/// One field of a body's row key, specialized: `c` is `seed ^ cell * CELL_K`
/// per half, `read` yields `runtime2::av_code` without building an `AV`.
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

    /// `runtime2::av_code` of every lane's value, off the output slot.
    #[inline]
    fn codes(&self, buf: &[u8], out: &mut [u64; 16]) {
        use celeste_engine::runtime2::{ival_code, num_code};
        let base = self.root;
        let words = |o: usize| -> [u32; 16] {
            let b: &[u8; 64] = buf[o..o + 64].try_into().unwrap();
            std::array::from_fn(|i| u32::from_le_bytes([b[4 * i], b[4 * i + 1], b[4 * i + 2], b[4 * i + 3]]))
        };
        match self.read {
            KeyRead::Num => {
                let w = words(base);
                for i in 0..16 {
                    out[i] = num_code(w[i]);
                }
            }
            KeyRead::Ival => {
                let (lo, hi) = (words(base), words(base + 64));
                for i in 0..16 {
                    out[i] = ival_code(lo[i], hi[i]);
                }
            }
            KeyRead::Bool => {
                let val = u16::from_le_bytes([buf[base], buf[base + 1]]);
                let known = u16::from_le_bytes([buf[base + 2], buf[base + 3]]);
                for (i, o) in out.iter_mut().enumerate() {
                    *o = if known & (1 << i) != 0 { 3u64 << 56 | (val >> i & 1) as u64 } else { 4u64 << 56 };
                }
            }
        }
    }
}

impl AsmBody {
    /// Every lane's `Σ cell_mix` per half (`mix64(part + h)` is the boundary's
    /// key), LANE-MAJOR so it vectorizes: one pass per field over 16 lanes
    /// (a body that fires averages ~7 live lanes; one key per emission
    /// in scalar code was a third of the forward, room (6,2) 100% f57).
    #[inline]
    fn key_words16(&self, buf: &[u8]) -> ([u64; 16], [u64; 16]) {
        use celeste_engine::runtime2::mix64;
        let (mut h1, mut h2) = ([0u64; 16], [0u64; 16]);
        let mut code = [0u64; 16];
        for f in &self.key_fields {
            f.codes(buf, &mut code);
            for i in 0..16 {
                h1[i] = h1[i].wrapping_add(mix64(f.c[0] ^ code[i]));
                h2[i] = h2[i].wrapping_add(mix64(f.c[1] ^ code[i]));
            }
        }
        (h1, h2)
    }
}

/// One whole-pixel coordinate of an emitted row: an output root or a constant.
#[derive(Clone, Copy)]
enum PosSrc {
    Root(usize),
    Konst(i16),
}

/// An accumulator column: a uniform constant or an empty typed vector.
enum ColInit {
    Uniform(AV),
    EmptyNum,
    EmptyBool,
    EmptyIval,
}

/// A per-outcome accumulator recipe (skeleton, column inits, the boundary's
/// constants), so the append emits rows already at the boundary.
struct AccTemplate {
    skeleton: Rt2,
    inits: Vec<(usize, ColInit)>,
    shape_hash: u64,
    /// The key's uniform part: the shape hash plus every cell uniform at the
    /// boundary, at its WIDENED value; a row's key is `mix64(part + h)`.
    part: (u64, u64),
    /// This outcome's own skeleton, for cells the union makes typed.
    own: Rt2,
    /// The SHAPE's skeleton, typed wherever any outcome varies a cell: every
    /// queue and piece of the shape has these columns. Set by `unify`.
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

/// Rust's default spawned-thread stack, assumed without `set_thread_stack`.
const DEFAULT_THREAD_STACK: usize = 2 << 20;
/// Room a kernel call leaves for its callers' frames and its call-outs.
const KERNEL_STACK_MARGIN: usize = 1 << 20;

thread_local! {
    static THREAD_STACK: std::cell::Cell<usize> = const { std::cell::Cell::new(DEFAULT_THREAD_STACK) };
}

/// Record this thread's stack size so kernel calls check their frame fits;
/// call it first in a kernel thread spawned with `stack_size(bytes)`.
pub fn set_thread_stack(bytes: usize) {
    THREAD_STACK.with(|s| s.set(bytes));
}

fn thread_stack() -> usize {
    THREAD_STACK.with(|s| s.get())
}

/// One shape's assembled kernel and the metadata the append needs.
struct AsmKernel {
    /// DIAGNOSTIC: per body (lanes taken, lanes emitted after the dup mask).
    body_stats: Vec<(std::sync::atomic::AtomicU64, std::sync::atomic::AtomicU64)>,
    loaded: Loaded,
    compiled: Compiled,
    bodies: Vec<AsmBody>,
    acc_templates: Vec<AccTemplate>,
    forks: u8,
    /// Per body: where its roots go in the shape's union columns (`unify`).
    body_cols: Vec<BodyCols>,
    /// The fused graph, roots and room for `explain_error`; empty unless
    /// `kernel_graph_kept()` (gigabytes per set).
    fused: crate::transpile::graph::Graph,
    flat_roots: Vec<crate::transpile::graph::NodeId>,
    room: Room,
    fused_nodes: usize,
    /// Per input cell, its traced path (named by packing failures).
    input_names: Vec<String>,
}

impl AsmKernel {
    /// Run `lanes` of one block in 16-lane slices, pushing each body's
    /// `live & !error` lanes into the sink's (shape, cell) queue with their
    /// edges. Any `live & error` (declined) fails the call: a coverage gap.
    fn run(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        debug_assert_eq!(cell_in.len(), chunk.width, "one input cell per input row");
        debug_assert!(lanes.end <= chunk.width);
        stats([1, 0, 0, 0, 0, 0, 0]);
        let mut lo = lanes.start;
        let mut idx = [0usize; 16];
        while lo < lanes.end {
            // A slice stays in one 64-lane id group (predecessor records are
            // the group's first id plus a lane bit).
            let n = 16.min(lanes.end - lo).min(64 - (lo & 63));
            for (i, l) in idx.iter_mut().enumerate().take(n) {
                *l = lo + i;
            }
            // A slice of only skipped lanes emits nothing (a raise re-expands
            // a few rows of each 64-row group it loads).
            if sink.skip_in.is_some_and(|sk| idx[..n].iter().all(|&l| sk[l])) {
                lo += n;
                continue;
            }
            if !self.run_slice(chunk, cell_in, &idx[..n], u16::MAX, sink) {
                return false;
            }
            lo += n;
        }
        sink.end_call();
        true
    }

    /// Run ONE slice of up to 16 lanes (of any id groups), keeping lanes in `only`.
    fn run_slice(
        &self,
        chunk: &Rt2,
        cell_in: &[u32],
        lanes: &[usize],
        only: u16,
        sink: &mut crate::frame::ForwardSink,
    ) -> bool {
        let t_setup = crate::frame::phases::start();
        let n = lanes.len();
        debug_assert!((1..=16).contains(&n));
        // Skip the set insert for a repeated (input cell, output cell) pair.
        let mut last_edge = (u32::MAX, u32::MAX);
        let views = self.input_views(chunk);
        let env = CollisionEnv { cart: &chunk.cart, cache: &chunk.cache };
        let ctx = AsmCtx::new(&env as *const CollisionEnv as *const c_void);
        // Per-thread reused buffers: a fresh allocation faults every page.
        let mut sc = Scratch::take(self);
        let Scratch { inbuf, outbuf, took, .. } = &mut sc;
        let body_cols = &self.body_cols;

        // Utilization tallies, folded into `CALL_STATS[3..]` at the end.
        let (mut n_bodies, mut n_bodies_taken, mut n_lanes, mut n_unique) = (0u64, 0u64, 0u64, 0u64);
        {
            // Per lane its predecessor GROUP's first id (ids are consecutive
            // in a block; a slice may span groups): a predecessor record is
            // that id plus the lane's bit in its group.
            let bases: Option<[u64; 16]> = sink.ids_in.map(|ids| std::array::from_fn(|i| if i < n { ids[lanes[i] & !63] } else { 0 }));
            let base = |i: usize| bases.map(|b| b[i]);
            let glane = |i: usize| lanes[i] & 63;
            let skip: u16 = match sink.skip_in {
                Some(sk) => (0..n).filter(|&i| sk[lanes[i]]).fold(0u16, |m, i| m | (1 << i)),
                None => 0,
            };
            let t_ph = crate::frame::phases::add(crate::frame::phases::SETUP, t_setup);
            self.pack_input(&views, lanes, inbuf);
            let t_ph = crate::frame::phases::add(crate::frame::phases::PACK, t_ph);
            // The spill frame is on THIS thread's stack: refuse rather than overrun.
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
            let t_emit = crate::frame::phases::add(crate::frame::phases::KERNEL, t_ph);
            let flush_before = sink.flush_ticks;
            let valid = (((1u32 << n) - 1) as u16) & only;
            for (bi, (body, cols)) in self.bodies.iter().zip(body_cols).enumerate() {
                // Tri-state masks, each read where it MAY hold.
                let error = read_zb_may(outbuf, body.error_off);
                let live = read_zb_may(outbuf, body.live_off);
                // A SKIPPED lane (an exit, a lost berry: never expanded) has no
                // successors to lose, so its error is no decline. Room (6,2)
                // 100% at f84: an exited row (room (7,2), a restart pending)
                // shared a slice with a live respawn and its error was fatal.
                if live & error & valid & !skip != 0 {
                    // Declined (fatal in the caller): report the lanes.
                    let declined = live & error & valid & !skip;
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
                // A lane whose emission equals an earlier body's (same
                // outcome, every stored root and transfer root equal) is
                // that body's emission again: skip it (`DupRef`).
                took[bi] = take;
                let take_before = take;
                if take != 0 {
                    for r in &body.dup_refs {
                        let cand = take & took[r.body];
                        if cand != 0 {
                            take &= !(cand & r.eq_mask(outbuf, cand));
                            if take == 0 {
                                break;
                            }
                        }
                    }
                }
                if take_before != 0 {
                    self.body_stats[bi].0.fetch_add(take_before.count_ones() as u64, std::sync::atomic::Ordering::Relaxed);
                    self.body_stats[bi].1.fetch_add(take.count_ones() as u64, std::sync::atomic::Ordering::Relaxed);
                }
                n_bodies += 1;
                if take == 0 {
                    continue;
                }
                n_bodies_taken += 1;
                n_lanes += take.count_ones() as u64;
                let template = &self.acc_templates[body.outcome];
                // Level -1 first: a dropped cell's row needs no key, transfer
                // or queue, only its pos-graph edge and its sources' notes
                // (what `flush` records for a dropped queue).
                let mut couts = [0u32; 16];
                let mut m = take;
                while m != 0 {
                    let i = m.trailing_zeros() as usize;
                    m &= m - 1;
                    let cout = cell_out(body, outbuf, i);
                    couts[i] = cout;
                    if let Some(from) = sink.minus_one_drop(template.union.shape_hash, cout) {
                        take &= !(1 << i);
                        let cin = cell_in[lanes[i]];
                        if sink.edges_on && last_edge != (cin, cout) {
                            last_edge = (cin, cout);
                            sink.edges.insert((cin, cout));
                        }
                        if let Some(b) = base(i) {
                            sink.dropped_again(b, glane(i), from as u64);
                        }
                    }
                }
                if take == 0 {
                    continue;
                }
                let keys = body.key_words16(outbuf);
                while take != 0 {
                    let i = take.trailing_zeros() as usize;
                    take &= take - 1;
                    let (h1, h2) = (keys.0[i], keys.1[i]);
                    let part = template.part;
                    let key = (
                        runtime2::mix64(part.0.wrapping_add(h1)),
                        runtime2::mix64(part.1.wrapping_add(h2)),
                    );
                    let cin = cell_in[lanes[i]];
                    // THE TRANSFER: per producer, on the edge, never in the row.
                    let xfer = match base(i) {
                        Some(_) => {
                            let words = body.arc.words(outbuf, i);
                            sink.xfer_id_raw(&words, |w| ArcSlots::decode(&body.arc.raw(w, chunk, lanes[i], body.outcome), chunk, lanes[i], body.outcome))
                        }
                        None => 0,
                    };
                    // EMISSION-TIME PROVENANCE: the pos-graph and backward
                    // edges from lane `i` are recorded right here.
                    if let Some((first_cin, r)) = sink.seen.insert_ref(key, cin, 0) {
                        // A re-emission: one more predecessor (on the queued
                        // row's mask, or direct if flushed).
                        if sink.edges_on && first_cin != cin {
                            sink.edges.insert((cin, couts[i]));
                        }
                        match base(i) {
                            Some(b) if r & RowCache::ID_FLAG != 0 => {
                                sink.direct_edge(r & !RowCache::ID_FLAG, b, xfer, glane(i));
                                continue;
                            }
                            Some(b) if r & RowCache::DROP_FLAG != 0 => {
                                sink.dropped_again(b, glane(i), r & !RowCache::DROP_FLAG);
                                continue;
                            }
                            // A stale ref: push again; the flush merges by key.
                            Some(b) if !sink.mark_pred(r, b, xfer, glane(i)) => {}
                            Some(_) => continue,
                            None => continue,
                        }
                    }
                    sink.emitted += 1;
                    n_unique += 1;
                    let cout = couts[i];
                    if sink.edges_on && last_edge != (cin, cout) {
                        last_edge = (cin, cout);
                        sink.edges.insert((cin, cout));
                    }
                    let q = sink.queue(template.shape_hash, cout, || (*template.union).clone_block());
                    // `minus_one_drop` judged the row by the union's shape:
                    // the queue's must be that one (flush filters by it).
                    assert_eq!(sink.slots[q].shape, template.union.shape_hash, "a queue's shape is its template union's");
                    cols.push_row(&mut sink.slots[q], outbuf, i, key, cout);
                    if let Some(b) = base(i) {
                        sink.slots[q].pred_base.push(b);
                        sink.slots[q].pred_mask.push(1u64 << (glane(i)));
                        sink.slots[q].pred_xfer.push(xfer);
                        sink.slots[q].last_extra.push(u32::MAX);
                        sink.seen.set_ref(key, sink.row_ref(q));
                    }
                    sink.pushed(q).expect("flushing a full queue");
                }
            }
            if t_emit != 0 {
                crate::frame::phases::add(crate::frame::phases::EMIT, t_emit);
                crate::frame::phases::sub(crate::frame::phases::EMIT, sink.flush_ticks - flush_before);
            }
        }
        sc.put_back();
        stats([0, only.count_ones().min(n as u32) as u64, 16, n_bodies, n_bodies_taken, n_lanes, n_unique]);
        true
    }

    /// On a decline: print the `error` disjuncts not false for `lane`.
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
                // Follow Sels to the first undecided condition.
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
                // The FAILING premise: false conjuncts through the strict
                // merges down to the leaves decided false.
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
                            // A `Known` premise: its undecided operands.
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

    /// The input columns of `chunk` as slices, resolved once per call.
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

    /// Pack `lanes` into `buf`; tail lanes repeat the last valid one so
    /// call-outs never see garbage (the `take` mask discards them).
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
                    // The general case: a materialized `AV` column, per value.
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

/// Per (thread, kernel) reusable input and output buffers.
struct Scratch {
    kernel: usize,
    inbuf: Vec<u8>,
    outbuf: Vec<u8>,
    /// Per body, the lanes it took this slice before `dup_refs` masking.
    took: Vec<u16>,
}

thread_local! {
    static SCRATCH: std::cell::RefCell<Vec<Scratch>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Scratch {
    /// This thread's scratch for `k`, taken out until `put_back`.
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
                    took: vec![0u16; k.bodies.len()],
                },
            }
        })
    }

    fn put_back(self) {
        SCRATCH.with(|c| c.borrow_mut().push(self));
    }
}

/// A body's roots by kind with their union columns, plus its uniform cells.
struct BodyCols {
    num: Vec<(usize, usize)>,
    ival: Vec<(usize, usize)>,
    /// A NUMBER root into a union `Ival` column, stored `[v, v]`.
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
            // A field the union holds uniform (e.g. `rem`) is not stored.
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
                // An output widening's canonical unknown (`TCol::Bool`'s 2).
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
                    // FATAL unless an output widening wrote it (`may_unknown`).
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
    /// A maybe-unknown bool (`CellRepr::UBool`), per lane or uniform.
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
                // DECIDED only: an unknown packed as false drops the true arm.
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

/// `CELESTE_KERNEL_KEY_CHECK=1`: the real `Rt2::boundary` over a copy of an
/// emitted block must change nothing (structure, shape hash, row keys): the
/// gate that the append's keys are exact.
pub(crate) fn key_check(slot: &crate::frame::Slot) {
    if !key_check_on() {
        return;
    }
    let acc = slot.to_rt2();
    let mut b = acc.clone_block();
    b.boundary(&super::boundary_ids());
    // A queue spans calls (duplicates allowed): compare DISTINCT keys.
    let mut distinct = acc.row_keys.clone();
    distinct.sort_unstable();
    distinct.dedup();
    let mut bkeys = b.row_keys.clone();
    bkeys.sort_unstable();
    if b.shape_hash == acc.shape_hash && b.structure == acc.structure && bkeys == distinct {
        return;
    }
    // Say WHICH rows and cells differ.
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

/// `CELESTE_KERNEL_KEY_CHECK=1` (see `key_check`).
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

/// A tri-state mask read where it MAY hold (`val | !known`; `val` at +0,
/// `known` at +2). An unknown `live` reads live: emitting over-approximates
/// (sound, the concrete search refutes) where dropping loses successors. An
/// unknown `error` is a decline: strict.
fn read_zb_may(buf: &[u8], off: usize) -> u16 {
    let val = u16::from_le_bytes([buf[off], buf[off + 1]]);
    let known = u16::from_le_bytes([buf[off + 2], buf[off + 3]]);
    val | !known
}

/// The boundary's row-key mix seeds (`runtime2::boundary_finish`).
use celeste_engine::runtime2::{KEY_SEED1, KEY_SEED2};



/// The start room's assembled kernels for one level.
pub(crate) struct Registry {
    /// One kernel per reached (shape, region); a missing key is a coverage gap.
    kernels: HashMap<(u64, Option<Region>), AsmKernel>,
    /// The region grid the set was built on.
    grid: Option<crate::trace::kernel::RegionGrid>,
}

use crate::trace::kernel::Region;

impl Registry {
    pub fn len(&self) -> usize {
        self.kernels.len()
    }

    /// Run `chunk` (rows in cell order) on the matching kernels; `false` on a
    /// miss or a decline.
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
        // Regions only for shapes with a PLAYER (not `player_spawn`).
        let Some(grid) = self.grid.filter(|_| !chunk.player_objects(crate::compiled::ids()).is_empty()) else {
            return self.run_key(chunk, None, cell_in, lanes, sink);
        };
        // The unit's live lanes bucketed by region, each bucket cut into
        // slices of 16 (a slice may span id groups: `run_slice` keys each
        // lane's predecessor records by its own group). Slicing in lane
        // order ran every slice once per region it held (a unit's cell order
        // changes region every ~12 rows); bucketing per 64-lane group still
        // left room (6,2) 100% f57 36% padding and room (3,0) at 8 px 53%.
        stats([1, 0, 0, 0, 0, 0, 0]);
        let mut by_region: Vec<(Option<Region>, usize)> =
            lanes.filter(|&l| !sink.skip_in.is_some_and(|sk| sk[l])).map(|l| (grid.of_cell(cell_in[l]), l)).collect();
        by_region.sort_unstable();
        let mut idx: Vec<usize> = Vec::with_capacity(16);
        for run in by_region.chunk_by(|a, b| a.0 == b.0) {
            let key = run[0].0;
            let Some(k) = self.kernels.get(&(chunk.shape_hash, key)) else {
                return self.run_key(chunk, key, cell_in, run[0].1..run[0].1 + 1, sink);
            };
            for part in run.chunks(16) {
                idx.clear();
                idx.extend(part.iter().map(|&(_, l)| l));
                if !k.run_slice(chunk, cell_in, &idx, ((1u32 << idx.len()) - 1) as u16, sink) {
                    return false;
                }
            }
        }
        sink.end_call();
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
                // A coverage gap, printed once per (shape, key).
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

    /// Retrace the start room and assemble every shape for `opts` (`root`: repo).
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

/// The kind of a typed column (or of a uniform value one could hold).
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

/// Give every outcome template of a SHAPE the union skeleton (a cell is
/// typed if any outcome varies it or two disagree; Num + Ival -> Ival) and
/// map every body onto its columns (`BodyCols`).
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
    // Per kernel, largest first, for the build report.
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

/// Apply the shape unions to one kernel's templates and bodies.
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

/// Assemble ONE frame (gcc + dlopen) and map its bodies onto output slots;
/// `tag` distinguishes a shape's region kernels.
fn build_one_shape(r: &crate::trace::kernel::Reference, si: usize, tag: &str) -> Result<(u64, AsmKernel)> {
    let shape = r.frame.in_rt2.shape_hash_of();
    let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
    let t_fuse = std::time::Instant::now();
    let (fused, bodies, flat_roots, reprs) =
        crate::trace::emit::asm_fused_from(&r.bound, &r.lowered.spec)
            .with_context(|| format!("fusing shape {si} (hash {shape:#x})"))?;
    // Row keys are folded per EMITTED row, not per lane in the kernel.
    // One output SLOT per DISTINCT root node; `slot_of[k]` for root k.
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

    // Which fused nodes read the canonical output unknown (`may_unknown`).
    let reads_unknown_output: Vec<bool> = {
        let mut out = vec![false; fused.len()];
        for n in 0..fused.len() {
            let node = fused.get(n as crate::transpile::graph::NodeId);
            out[n] = node.op == crate::transpile::graph::Op::UnknownBool(u32::MAX) || node.args.iter().any(|a| out[*a as usize]);
        }
        out
    };
    // Each body's roots onto their slots, through `slot_of`.
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
                // A number stored `[v, v]` keys as the number.
                Some(KeyField::new(outputs[j].0 as u64, compiled.root_offsets[root] as usize, compiled.root_kinds[root]))
            })
            .collect();
        // Every body computes its transfer (the rotation graph needs it).
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
            dup_refs: Vec::new(),
        });
        off += b.roots.len();
    }

    let acc_templates = (0..r.bound.outcomes.len())
        .map(|oi| acc_template(r, oi))
        .collect::<Result<Vec<_>>>()?;
    for b in asm_bodies.iter_mut() {
        b.pos = pos_sources(&acc_templates[b.outcome].build(), &b.fields)?;
    }
    dup_refs(&mut asm_bodies);
    if let Ok(th) = std::env::var("CELESTE_GRAPH_REPORT") {
        static DONE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
        let th: usize = th.parse().unwrap_or(300);
        if bodies.len() >= th && !DONE.swap(true, std::sync::atomic::Ordering::Relaxed) {
            use crate::transpile::graph::{NodeId, Op};
            let nm = crate::search::inspect::cell_names(&r.frame.in_rt2, crate::compiled::ids());
            let labels: Vec<String> = { let mut nb = 0; let names = ["left", "right", "up", "down", "jump", "dash"]; r.frame.fork_origins.iter().map(|(_, o)| if o == crate::trace::domain::UNKNOWN_BOOL_ORIGIN { nb += 1; names.get(nb - 1).unwrap_or(&"btn?").to_string() } else { o.chars().take(30).collect() }).collect() };
            { let mut v: Vec<_> = nm.iter().filter(|(c, _)| [20usize, 259, 260, 271, 282, 283, 324, 41].contains(c)).collect(); v.sort(); eprintln!("[graph] names {:?}", v); }
            eprintln!("[graph] kernel shape {si} {tag}: {} bodies, {} fused nodes, {} outcomes; forks {:?}", bodies.len(), fused.len(), r.bound.outcomes.len(), labels);
            for (o, out) in r.bound.outcomes.iter().enumerate() {
                let bs: Vec<usize> = (0..bodies.len()).filter(|&i| bodies[i].outcome == o).collect();
                let nf = out.outputs.len();
                eprintln!("[graph] outcome {o}: {} bodies, {} output fields", bs.len(), nf);
                if bs.len() < 2 { continue; }
                // divergence: lockstep walk collecting (guard chain, a, b)
                fn diverge(g: &crate::transpile::graph::Graph, a: NodeId, b: NodeId, path: &mut Vec<(NodeId, bool)>, out: &mut Vec<(Vec<(NodeId, bool)>, NodeId, NodeId)>, budget: &mut usize) {
                    if a == b || *budget == 0 { return; }
                    *budget -= 1;
                    let (na, nb) = (g.get(a), g.get(b));
                    if na.op == nb.op && na.args.len() == nb.args.len() && !na.args.is_empty() {
                        let is_sel = matches!(na.op, Op::Sel) && na.args[0] == nb.args[0];
                        for k in 0..na.args.len() {
                            if na.args[k] == nb.args[k] { continue; }
                            if is_sel && k > 0 { path.push((na.args[0], k == 1)); diverge(g, na.args[k], nb.args[k], path, out, budget); path.pop(); }
                            else { diverge(g, na.args[k], nb.args[k], path, out, budget); }
                        }
                    } else { out.push((path.clone(), a, b)); }
                }
                for d in 0..labels.len() {
                    let (mut pairs, mut diff_pairs) = (0usize, 0usize);
                    let mut fields: std::collections::BTreeMap<String, usize> = Default::default();
                    let mut guards: std::collections::BTreeMap<String, usize> = Default::default();
                    let mut sample: Option<String> = None;
                    for &j in &bs { for &i in &bs { if i >= j { continue; }
                        let (a, b) = (&bodies[i], &bodies[j]);
                        if (0..a.splits.len()).filter(|&k| a.splits.get(k) != b.splits.get(k)).collect::<Vec<_>>() != vec![d] { continue; }
                        pairs += 1;
                        let dk: Vec<usize> = (0..a.roots.len()).filter(|&k| a.roots[k] != b.roots[k]).collect();
                        if dk.is_empty() { continue; }
                        diff_pairs += 1;
                        for &k in &dk {
                            let fname = if k < nf { nm.get(&(out.outputs[k].0 as usize)).cloned().unwrap_or(format!("cell{}", out.outputs[k].0)) } else if k == nf { "ERROR".into() } else if k == nf + 1 { "LIVE".into() } else { format!("transfer{}", k - nf - 2) };
                            *fields.entry(fname.clone()).or_default() += 1;
                            let mut outv = Vec::new(); let mut budget = 2000;
                            diverge(&fused, a.roots[k], b.roots[k], &mut Vec::new(), &mut outv, &mut budget);
                            for (path, x, y) in &outv {
                                let gs: Vec<String> = path.iter().map(|(c, pos)| format!("{}{}", if *pos {""} else {"NOT "}, crate::trace::emit::show_tree(&fused, *c, 2))).collect();
                                *guards.entry(format!("[{}] depth {}", gs.join(" & ").chars().take(220).collect::<String>(), path.len())).or_default() += 1;
                                if sample.is_none() && k < nf { sample = Some(format!("{fname}: guards {:?}\n        A {}\n        B {}", gs, crate::trace::emit::show_tree(&fused, *x, 3), crate::trace::emit::show_tree(&fused, *y, 3))); }
                            }
                        }
                    } }
                    if pairs == 0 { continue; }
                    eprintln!("[graph]   fork {} ({}): {pairs} one-flip pairs, {diff_pairs} with differing roots", d, labels[d]);
                    let mut fv: Vec<_> = fields.into_iter().collect(); fv.sort_by_key(|x| std::cmp::Reverse(x.1));
                    eprintln!("[graph]     fields differing (pairs): {:?}", fv.iter().take(14).collect::<Vec<_>>());
                    let mut gv: Vec<_> = guards.into_iter().collect(); gv.sort_by_key(|x| std::cmp::Reverse(x.1));
                    for (g, n) in gv.iter().take(5) { eprintln!("[graph]     {n:5} x guards {g}"); }
                    if let Some(s) = sample { eprintln!("[graph]     sample {s}"); }
                }
            }
        }
    }
    if let Ok(th) = std::env::var("CELESTE_EQ_REPORT") {
        static DONE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
        let th: usize = th.parse().unwrap_or(300);
        if bodies.len() >= th && !DONE.swap(true, std::sync::atomic::Ordering::Relaxed) {
            eq_report(&fused, &bodies, r, si);
        }
    }
    if std::env::var_os("CELESTE_OUTCOME_DUMP").is_some() && si == 1 {
        let nm = crate::search::inspect::cell_names(&r.frame.in_rt2, crate::compiled::ids());
        let want = ["freeze", "has_dashed", "player[0].dash_time", "player[0].djump", "player.x", "player.y"];
        eprintln!("[odump] shape {si}: {} outcomes, {} bodies, forks {:?}", r.bound.outcomes.len(), bodies.len(), r.frame.fork_origins.iter().map(|(d,o)| format!("{d}:{}", if o == crate::trace::domain::UNKNOWN_BOOL_ORIGIN {"btn"} else {o.as_str()})).collect::<Vec<_>>());
        for (o, out) in r.bound.outcomes.iter().enumerate() {
            let bs: Vec<&crate::trace::emit::AsmBody> = bodies.iter().filter(|b| b.outcome == o).collect();
            eprintln!("[odump] outcome {o}: {} bodies, {} output fields", bs.len(), out.outputs.len());
            // the body with only the dash button pressed, else the first
            let pick = bs.iter().find(|b| b.splits.iter().enumerate().all(|(d, v)| if d == 5 { *v == 1 } else { *v == 0 })).or(bs.first());
            if let Some(b) = pick {
                eprintln!("[odump]   body splits {:?}", b.splits);
                for (k, (cell, _, _)) in out.outputs.iter().enumerate() {
                    if let Some(n) = nm.get(&(*cell as usize)) { if want.contains(&n.as_str()) {
                        eprintln!("[odump]     {n} = {}", crate::trace::emit::show_tree(&fused, b.roots[k], 4));
                    } }
                }
                let nf = out.outputs.len();
                eprintln!("[odump]     live = {}", crate::trace::emit::show_tree(&fused, b.roots[nf + 1], 3));
            }
        }
    }
    if std::env::var_os("CELESTE_DUP_EXPLAIN").is_some() {
        static DONE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let btn: Vec<u8> = r.frame.fork_origins.iter().filter(|(_, o)| o == crate::trace::domain::UNKNOWN_BOOL_ORIGIN).map(|(d, _)| *d).collect();
        let names = ["left", "right", "up", "down", "jump", "dash"];
        let want_btn = std::env::var("CELESTE_DUP_EXPLAIN").unwrap().parse::<usize>().unwrap_or(5);
        if let Some(&dd) = btn.get(want_btn) {
            'outer: for j in 0..bodies.len() {
                for i in 0..j {
                    let (a, b) = (&bodies[i], &bodies[j]);
                    if a.outcome != b.outcome { continue; }
                    let diff: Vec<usize> = (0..a.splits.len()).filter(|&d| a.splits.get(d) != b.splits.get(d)).collect();
                    if diff != vec![dd as usize] { continue; }
                    let nf = r.bound.outcomes[a.outcome].outputs.len();
                    let fields: Vec<usize> = (0..nf).filter(|&k| a.roots[k] != b.roots[k]).collect();
                    if fields.is_empty() || fields.len() > 6 { continue; }
                    if DONE.fetch_add(1, std::sync::atomic::Ordering::Relaxed) >= 2 { break 'outer; }
                    eprintln!("[explain] shape {si} outcome {}: bodies {i} and {j} differ only in btn {} ({:?} vs {:?}); forks: {:?}", a.outcome, names[want_btn], a.splits.get(dd as usize), b.splits.get(dd as usize), r.frame.fork_origins.iter().map(|(d,o)| format!("{d}:{}", if o == crate::trace::domain::UNKNOWN_BOOL_ORIGIN {"btn"} else {o.as_str()})).collect::<Vec<_>>());
                    eprintln!("[explain]   split values a {:?}", a.splits);
                    let nm = crate::search::inspect::cell_names(&r.frame.in_rt2, crate::compiled::ids());
                    let mut ks: Vec<_> = nm.iter().filter(|(c, _)| [20usize, 41, 237, 238, 247].contains(c)).collect(); ks.sort();
                    eprintln!("[explain]   names {:?}", ks);
                    let nf2 = r.bound.outcomes[a.outcome].outputs.len();
                    eprintln!("[explain]   live A = {}\n      live B = {}", crate::trace::emit::show_tree(&fused, a.roots[nf2 + 1], 9), crate::trace::emit::show_tree(&fused, b.roots[nf2 + 1], 9));
                    for k in 0..nf2 { let c = r.bound.outcomes[a.outcome].outputs[k].0; if let Some(n) = nm.get(&(c as usize)) { if n.contains("dash_time") || n.contains("djump") || n.contains("spd") { eprintln!("[explain]   {n}: A = {} | B = {}", crate::trace::emit::show_tree(&fused, a.roots[k], 3), crate::trace::emit::show_tree(&fused, b.roots[k], 3)); } } }
                    for k in fields {
                        let cell = r.bound.outcomes[a.outcome].outputs[k].0;
                        eprintln!("[explain]   field {k} (cell {cell}):\n      A = {}\n      B = {}", crate::trace::emit::show_tree(&fused, a.roots[k], 7), crate::trace::emit::show_tree(&fused, b.roots[k], 7));
                    }
                    break 'outer;
                }
            }
        }
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
    let asm_bodies_len = asm_bodies.len();
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
            body_stats: (0..asm_bodies_len).map(|_| Default::default()).collect(),
        },
    ))
}

/// Every body's `dup_refs`: the earlier bodies of its outcome whose fork
/// configuration differs from its own in exactly one fork. Every root slot
/// is compared (fields, so whatever the union stores; key fields and
/// position sources are fields); a pair of kinds that differ is no ref.
fn dup_refs(bodies: &mut [AsmBody]) {
    let n = bodies.len();
    for j in 0..n {
        let mut refs = Vec::new();
        for i in 0..j {
            let (a, b) = (&bodies[i], &bodies[j]);
            if a.outcome != b.outcome || a.splits.len() != b.splits.len() || a.arc.fin != b.arc.fin {
                continue;
            }
            if a.splits.iter().zip(&b.splits).filter(|(x, y)| x != y).count() != 1 {
                continue;
            }
            assert_eq!(a.fields.len(), b.fields.len(), "two bodies of one outcome");
            let mut pairs: Vec<(usize, usize, RootKind)> = Vec::new();
            let mut ok = true;
            for (fa, fb) in a.fields.iter().zip(&b.fields) {
                assert_eq!(fa.cell, fb.cell, "two bodies of one outcome");
                // The kinds, and the undecided-boolean guard (`may_unknown`),
                // must agree: a masked lane skips its own body's push.
                if fa.kind != fb.kind || fa.may_unknown != fb.may_unknown {
                    ok = false;
                    break;
                }
                if fa.off != fb.off {
                    pairs.push((fa.off, fb.off, fa.kind));
                }
            }
            for k in 0..a.arc.roots.len() {
                if !a.arc.fin && k % crate::trace::verify::ARC_AXIS_ROOTS == 4 {
                    continue;
                }
                let ((oa, ka), (ob, kb)) = (a.arc.roots[k], b.arc.roots[k]);
                if ka != kb {
                    ok = false;
                    break;
                }
                if oa != ob {
                    pairs.push((oa, ob, ka));
                }
            }
            if ok {
                pairs.sort_unstable_by_key(|p| (p.0, p.1));
                pairs.dedup_by_key(|p| (p.0, p.1));
                refs.push(DupRef { body: i, pairs });
            }
        }
        bodies[j].dup_refs = refs;
    }
}

/// Where a body's rows get player and room `x`/`y`; `None` (`NO_CELL`)
/// without a numeric player position, as `pos_graph::block_cells`.
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
            // An interval's cell is its low corner (at a number's offset).
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

/// The position cell of lane `i` of a body's output.
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

/// The accumulator recipe for one outcome, with `shape_hash` and `part`.
fn acc_template(r: &crate::trace::kernel::Reference, oi: usize) -> Result<AccTemplate> {
    let skeleton = reshape(&r.frame.outs[oi].rt2, 0);
    // The structure must be canonical already (checked, not trusted).
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
        // A widened-to-uniform cell holds its widened value (as `part` folds).
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
    // The key's uniform part, exactly as `boundary_finish` folds it.
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

/// Kernel call counters: [0] calls, [1] rows offered, [2] slice-lanes
/// executed (`[2] - [1]` is padding), [3] (body, slice) evaluations, [4]
/// those that took a lane, [5] lane emissions, [6] after the dedup cache.
static CALL_STATS: [std::sync::atomic::AtomicU64; 7] =
    [const { std::sync::atomic::AtomicU64::new(0) }; 7];

thread_local! {
    /// This thread's share of `CALL_STATS`, folded in by `fold_call_stats`:
    /// six shared atomic adds per slice from every worker cost more than the
    /// slice's kernel (room (6,2) f57: ~60 worker-seconds of 260).
    static LOCAL_STATS: std::cell::Cell<[u64; 7]> = const { std::cell::Cell::new([0; 7]) };
}

/// Add `d` to this thread's counters (one thread-local access per slice).
#[inline]
fn stats(d: [u64; 7]) {
    LOCAL_STATS.with(|c| {
        let mut v = c.get();
        for (a, b) in v.iter_mut().zip(d) {
            *a += b;
        }
        c.set(v);
    });
}

/// Fold this thread's counters into the shared ones (a wave worker's end).
pub(crate) fn fold_call_stats() {
    let v = LOCAL_STATS.with(|c| c.replace([0; 7]));
    for (a, n) in CALL_STATS.iter().zip(v) {
        if n != 0 {
            a.fetch_add(n, std::sync::atomic::Ordering::Relaxed);
        }
    }
}

/// Take (and reset) the call-shape counters.
pub(crate) fn take_call_stats() -> [u64; 7] {
    fold_call_stats();
    std::array::from_fn(|i| CALL_STATS[i].swap(0, std::sync::atomic::Ordering::Relaxed))
}

/// The process-global level's kernel set, built on first use. ONE set is
/// resident (GBs): another level replaces it; callers hold theirs by `Arc`.
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

/// THE KERNEL BUILD'S PURGE DELAY: immediate purges cost the build a third
/// of its walk in TLB shootdowns, so purges wait a second while any build runs.
struct BuildPurgeDelay;

/// Builds in progress, and the purge delay they found.
static BUILDS: std::sync::Mutex<(usize, std::os::raw::c_long)> = std::sync::Mutex::new((0, 0));

/// `mi_option_purge_delay` (unbound in libmimalloc-sys 0.1.44); checked
/// against the environment's value in `start`.
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
            // Purge what the (exited) builder threads freed under the delay.
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

/// Run one chunk through the kernels; `false` (miss or decline) is fatal.
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


/// DIAGNOSTIC (CELESTE_EQ_REPORT): the symbolic "same successor" predicate of
/// one-fork body pairs, built by structural rules over the fused graph.
#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
enum EqE {
    T,
    F,
    /// A guard: node `c` is true (`true`) or false (`false`).
    C(crate::transpile::graph::NodeId, bool),
    /// Two nodes the rules cannot relate: a per-lane comparison.
    Opaque(crate::transpile::graph::NodeId, crate::transpile::graph::NodeId),
    And(Vec<EqE>),
    Or(Vec<EqE>),
}

impl EqE {
    fn and(v: Vec<EqE>) -> EqE {
        let mut out = Vec::new();
        for e in v {
            match e {
                EqE::T => {}
                EqE::F => return EqE::F,
                EqE::And(xs) => out.extend(xs),
                x => out.push(x),
            }
        }
        out.sort();
        out.dedup();
        match out.len() { 0 => EqE::T, 1 => out.pop().unwrap(), _ => EqE::And(out) }
    }
    fn or(v: Vec<EqE>) -> EqE {
        let mut out = Vec::new();
        for e in v {
            match e {
                EqE::F => {}
                EqE::T => return EqE::T,
                EqE::Or(xs) => out.extend(xs),
                x => out.push(x),
            }
        }
        out.sort();
        out.dedup();
        // c or not c
        for x in &out { if let EqE::C(n, b) = x { if out.contains(&EqE::C(*n, !b)) { return EqE::T; } } }
        match out.len() { 0 => EqE::F, 1 => out.pop().unwrap(), _ => EqE::Or(out) }
    }
    fn size(&self) -> (usize, usize) {
        match self {
            EqE::T | EqE::F => (0, 0),
            EqE::C(..) => (1, 0),
            EqE::Opaque(..) => (0, 1),
            EqE::And(v) | EqE::Or(v) => v.iter().fold((0, 0), |a, e| { let s = e.size(); (a.0 + s.0, a.1 + s.1) }),
        }
    }
    fn show(&self, g: &crate::transpile::graph::Graph, names: &dyn Fn(crate::transpile::graph::NodeId) -> String) -> String {
        match self {
            EqE::T => "T".into(),
            EqE::F => "F".into(),
            EqE::C(c, b) => format!("{}{}", if *b { "" } else { "!" }, names(*c)),
            EqE::Opaque(a, b) => format!("[{} == {}]", crate::trace::emit::show_tree(g, *a, 1), crate::trace::emit::show_tree(g, *b, 1)),
            EqE::And(v) => format!("({})", v.iter().map(|e| e.show(g, names)).collect::<Vec<_>>().join(" & ")),
            EqE::Or(v) => format!("({})", v.iter().map(|e| e.show(g, names)).collect::<Vec<_>>().join(" | ")),
        }
    }
}

fn eq_nodes(
    g: &crate::transpile::graph::Graph,
    a: crate::transpile::graph::NodeId,
    b: crate::transpile::graph::NodeId,
    memo: &mut HashMap<(u32, u32), EqE>,
    depth: usize,
) -> EqE {
    use crate::transpile::graph::Op;
    if a == b { return EqE::T; }
    let key = (a.min(b) as u32, a.max(b) as u32);
    if let Some(e) = memo.get(&key) { return e.clone(); }
    let (na, nb) = (g.get(a), g.get(b));
    let e = if depth > 40 {
        EqE::Opaque(a, b)
    } else {
        match (&na.op, &nb.op) {
            (Op::Const(l1, h1), Op::Const(l2, h2)) => if (l1, h1) == (l2, h2) { EqE::T } else { EqE::F },
            (Op::ConstBool(x), Op::ConstBool(y)) => if x == y { EqE::T } else { EqE::F },
            (Op::Sel, Op::Sel) if na.args[0] == nb.args[0] => {
                let c = na.args[0];
                EqE::or(vec![
                    EqE::and(vec![EqE::C(c, true), eq_nodes(g, na.args[1], nb.args[1], memo, depth + 1)]),
                    EqE::and(vec![EqE::C(c, false), eq_nodes(g, na.args[2], nb.args[2], memo, depth + 1)]),
                ])
            }
            (Op::Sel, _) => {
                let c = na.args[0];
                EqE::or(vec![
                    EqE::and(vec![EqE::C(c, true), eq_nodes(g, na.args[1], b, memo, depth + 1)]),
                    EqE::and(vec![EqE::C(c, false), eq_nodes(g, na.args[2], b, memo, depth + 1)]),
                ])
            }
            (_, Op::Sel) => {
                let c = nb.args[0];
                EqE::or(vec![
                    EqE::and(vec![EqE::C(c, true), eq_nodes(g, a, nb.args[1], memo, depth + 1)]),
                    EqE::and(vec![EqE::C(c, false), eq_nodes(g, a, nb.args[2], memo, depth + 1)]),
                ])
            }
            (x, y) if x == y && na.args.len() == nb.args.len() && !na.args.is_empty() => {
                // Equal operands give equal results (sufficient, not necessary).
                let parts: Vec<EqE> = na.args.iter().zip(&nb.args).map(|(p, q)| eq_nodes(g, *p, *q, memo, depth + 1)).collect();
                let all = EqE::and(parts);
                match all { EqE::F => EqE::Opaque(a, b), x => if x.size().1 > 0 && matches!(x, EqE::And(_)) { EqE::Opaque(a, b) } else { x } }
            }
            _ => EqE::Opaque(a, b),
        }
    };
    memo.insert(key, e.clone());
    e
}

fn eq_report(fused: &crate::transpile::graph::Graph, bodies: &[crate::trace::emit::AsmBody], r: &crate::trace::kernel::Reference, si: usize) {
    let nm = crate::search::inspect::cell_names(&r.frame.in_rt2, crate::compiled::ids());
    let names = |c: crate::transpile::graph::NodeId| -> String {
        let s = crate::trace::emit::show_tree(fused, c, 2);
        let mut out = s.clone();
        for (cell, n) in &nm { out = out.replace(&format!("Cell({cell})"), n); }
        out.chars().take(70).collect()
    };
    let labels: Vec<String> = { let mut nb = 0; let ns = ["left", "right", "up", "down", "jump", "dash"]; r.frame.fork_origins.iter().map(|(_, o)| if o == crate::trace::domain::UNKNOWN_BOOL_ORIGIN { nb += 1; ns.get(nb - 1).unwrap_or(&"btn?").to_string() } else { o.chars().take(24).collect() }).collect() };
    eprintln!("[eq] kernel shape {si}: {} bodies, {} outcomes, forks {:?}", bodies.len(), r.bound.outcomes.len(), labels);
    let mut memo: HashMap<(u32, u32), EqE> = HashMap::new();
    for d in 0..labels.len() {
        let (mut pairs, mut f, mut t, mut guard_only, mut opaque) = (0, 0, 0, 0, 0);
        let (mut gsum, mut osum) = (0usize, 0usize);
        let mut sample: Option<String> = None;
        for j in 0..bodies.len() { for i in 0..j {
            let (a, b) = (&bodies[i], &bodies[j]);
            if a.outcome != b.outcome { continue; }
            if (0..a.splits.len()).filter(|&k| a.splits.get(k) != b.splits.get(k)).collect::<Vec<_>>() != vec![d] { continue; }
            pairs += 1;
            let e = EqE::and(a.roots.iter().zip(&b.roots).map(|(x, y)| eq_nodes(fused, *x, *y, &mut memo, 0)).collect());
            let (gs, os) = e.size();
            match e { EqE::F => f += 1, EqE::T => t += 1, _ => if os == 0 { guard_only += 1; gsum += gs } else { opaque += 1; gsum += gs; osum += os } }
            if sample.is_none() && e != EqE::F && a.outcome == 0 { sample = Some(e.show(fused, &names).chars().take(900).collect()); }
        } }
        if pairs == 0 { continue; }
        eprintln!("[eq] fork {d} ({}): {pairs} pairs: Eq=F {f}, Eq=T {t}, guards only {guard_only}, with per-lane comparisons {opaque}; avg guard atoms {:.1}, avg comparisons {:.1}",
            labels[d], gsum as f64 / (guard_only + opaque).max(1) as f64, osum as f64 / opaque.max(1) as f64);
        if let Some(s) = sample { eprintln!("[eq]   sample (outcome 0): {s}"); }
    }
    // Divergence points: the first node pairs where two formulas differ
    // (memoized DAG walk, path-insensitive). A point `sel(c, x, e)` vs `e`
    // (or `sel(c, t, x)` vs `t`) is a GUARDED alternative: equal unless `c`
    // (resp. `!c`) holds. Anything else is a raw difference.
    fn points(g: &crate::transpile::graph::Graph, a: crate::transpile::graph::NodeId, b: crate::transpile::graph::NodeId, seen: &mut std::collections::HashSet<(u32, u32)>, out: &mut Vec<(crate::transpile::graph::NodeId, crate::transpile::graph::NodeId)>) {
        if a == b || !seen.insert((a, b)) { return; }
        let (na, nb) = (g.get(a), g.get(b));
        let guarded = |s: &crate::transpile::graph::Node, other: crate::transpile::graph::NodeId| matches!(s.op, crate::transpile::graph::Op::Sel) && (s.args[1] == other || s.args[2] == other);
        if guarded(na, b) || guarded(nb, a) { out.push((a, b)); return; }
        if na.op == nb.op && na.args.len() == nb.args.len() && !na.args.is_empty() {
            for k in 0..na.args.len() { points(g, na.args[k], nb.args[k], seen, out); }
        } else { out.push((a, b)); }
    }
    for d in 0..labels.len() {
        let (mut pairs, mut pts_sum, mut guarded_sum, mut raw_sum, mut conds_sum, mut all_guarded) = (0usize, 0usize, 0usize, 0usize, 0usize, 0usize);
        let mut sample: Option<String> = None;
        for j in 0..bodies.len() { for i in 0..j {
            let (a, b) = (&bodies[i], &bodies[j]);
            if a.outcome != b.outcome { continue; }
            if (0..a.splits.len()).filter(|&k| a.splits.get(k) != b.splits.get(k)).collect::<Vec<_>>() != vec![d] { continue; }
            pairs += 1;
            let mut seen = std::collections::HashSet::new();
            let mut pts = Vec::new();
            for (x, y) in a.roots.iter().zip(&b.roots) { points(fused, *x, *y, &mut seen, &mut pts); }
            let mut conds = std::collections::BTreeSet::new();
            let (mut gd, mut raw) = (0, 0);
            let mut raws = Vec::new();
            for &(x, y) in &pts {
                let (nx, ny) = (fused.get(x), fused.get(y));
                if matches!(nx.op, crate::transpile::graph::Op::Sel) && (nx.args[1] == y || nx.args[2] == y) { gd += 1; conds.insert((nx.args[0], nx.args[2] == y)); }
                else if matches!(ny.op, crate::transpile::graph::Op::Sel) && (ny.args[1] == x || ny.args[2] == x) { gd += 1; conds.insert((ny.args[0], ny.args[2] == x)); }
                else { raw += 1; raws.push((x, y)); }
            }
            pts_sum += pts.len(); guarded_sum += gd; raw_sum += raw; conds_sum += conds.len();
            if raw == 0 { all_guarded += 1; }
            if sample.is_none() && a.outcome == 0 {
                let cs: Vec<String> = conds.iter().map(|(c, pos)| format!("{}{}", if *pos { "" } else { "!" }, names(*c))).collect();
                let rs: Vec<String> = raws.iter().take(3).map(|(x, y)| format!("[{} | {}]", crate::trace::emit::show_tree(fused, *x, 2), crate::trace::emit::show_tree(fused, *y, 2))).collect();
                sample = Some(format!("conds that must not fire {:?}; raw {:?}", cs, rs).chars().take(1200).collect());
            }
        } }
        if pairs == 0 { continue; }
        let p = pairs as f64;
        eprintln!("[div] fork {d} ({}): {pairs} pairs: per pair {:.1} divergence points ({:.1} guarded, {:.1} raw), {:.1} distinct guard conditions; {} pairs fully guarded",
            labels[d], pts_sum as f64 / p, guarded_sum as f64 / p, raw_sum as f64 / p, conds_sum as f64 / p, all_guarded);
        if let Some(s) = sample { eprintln!("[div]   sample: {s}"); }
    }
}


/// DIAGNOSTIC (CELESTE_BODY_STATS): per body, lanes taken vs emitted after the dup mask.
pub(crate) fn print_body_stats() {
    if std::env::var_os("CELESTE_BODY_STATS").is_none() { return; }
    let Some(reg) = registry() else { return };
    use std::sync::atomic::Ordering::Relaxed;
    let (mut bodies, mut never, mut always_dup, mut some, mut taken_all, mut taken_dupbody, mut emitted) = (0u64, 0u64, 0u64, 0u64, 0u64, 0u64, 0u64);
    let mut frac_hist = [0u64; 11];
    let mut worst: Vec<(u64, String)> = Vec::new();
    for ((shape, key), k) in &reg.kernels {
        let mut kn = (0u64, 0u64, 0u64);
        for (bi, (t, e)) in k.body_stats.iter().enumerate() {
            let (t, e) = (t.load(Relaxed), e.load(Relaxed));
            bodies += 1;
            taken_all += t; emitted += e;
            if t == 0 { never += 1; continue; }
            if e == 0 { always_dup += 1; taken_dupbody += t; kn.0 += 1; } else { some += 1; }
            frac_hist[((e * 10) / t) as usize] += 1;
            kn.1 += 1; kn.2 += t;
            let _ = bi;
        }
        if kn.2 > 0 { worst.push((kn.2, format!("shape {shape:#x} key {key:?}: {} bodies, {} taken a lane, {} of them never emitting anything new, {} lanes", k.bodies.len(), kn.1, kn.0, kn.2))); }
    }
    eprintln!("[bodies] {bodies} bodies over {} kernels: {never} never took a lane; {always_dup} took lanes but NEVER emitted anything new (all their lanes duplicates; {taken_dupbody} lanes); {some} emitted something new", reg.kernels.len());
    eprintln!("[bodies] lanes taken {taken_all}, emitted {emitted} ({:.1}%)", 100.0 * emitted as f64 / taken_all.max(1) as f64);
    eprintln!("[bodies] bodies by share of their lanes that were new (0-10%, 10-20%, ..., 100%): {:?}", frac_hist);
    worst.sort_by_key(|w| std::cmp::Reverse(w.0));
    for (_, w) in worst.iter().take(6) { eprintln!("[bodies]   {w}"); }
}
