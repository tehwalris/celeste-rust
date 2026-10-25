//! Runtime AVX-512 kernels: retrace the start room's shapes at startup,
//! assemble each (shape, region)'s fused fork-free graph, and dispatch
//! chunks to it. The APPEND step emits each `live & !error` lane at the
//! boundary: canonical structure, its row key exactly as
//! `Rt2::boundary_finish` (`--key-check`), its cell and transfer.

use std::collections::HashMap;
use std::os::raw::c_void;

use anyhow::{Context, Result};

use celeste_core::arith::Arithmetic;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::exact::{Code, KeySpace, Provisional, ShapeRecord};
use celeste_engine::runtime2::{Col, Rt2, AV};
use celeste_engine::slots::reshape;

use crate::storage::stats::S;
use crate::transpile::asm::{AsmCtx, CollisionEnv, Compiled, Loaded};
use crate::transpile::graph::Ty;
use crate::transpile::graph::{Eval, Room};

mod emit;
mod lanes;
use emit::{ArcSlots, KeyField, PartKey, PosSrc};
pub(crate) use emit::{RawWords, RAW_WORDS};
pub use lanes::KernelLanes;

/// One output field of a body: its output cell, slot and kind.
struct AsmField {
    cell: usize,
    /// The root's byte offset in the output buffer.
    off: usize,
    kind: Ty,
    /// A boolean an output widening may write UNKNOWN; any other undecided
    /// boolean is a branch on an unknown no premise declined: fatal.
    may_unknown: bool,
}

/// One fused body (outcome, fork configuration): its fields and `error` /
/// `live` roots; a lane is emitted iff `live & !error`.
struct AsmBody {
    outcome: usize,
    /// The fork configuration (a fragment a fork, `graph::ANY_VALID` for a
    /// dead one): the build report's, and `dup_refs`' "one fork away".
    splits: Vec<u8>,
    fields: Vec<AsmField>,
    /// `error`'s slot (`--kernel-explain`).
    error_root: usize,
    error_off: usize,
    live_off: usize,
    /// Earlier bodies of the same outcome one fork away, with the root
    /// slots whose equality makes a lane's emission theirs (`DupRef`).
    dup_refs: Vec<DupRef>,
}

/// An earlier body `body` and the `(its slot, my slot, kind)` pairs, as
/// `DupPairs` ids, that differ among the roots the emission reads: equal on
/// a lane in all of them, the two bodies emit the same row, edge and transfer.
struct DupRef {
    body: usize,
    pairs: Vec<u32>,
}

/// A kernel's distinct dup-ref slot pairs `(a, b, kind)`, each one's
/// equality mask computed at most once a slice (`DupMemo`).
#[derive(Default)]
struct DupPairs {
    pairs: Vec<(usize, usize, Ty)>,
    index: HashMap<(usize, usize, Ty), u32>,
}

impl DupPairs {
    fn id(&mut self, p: (usize, usize, Ty)) -> u32 {
        let n = self.pairs.len() as u32;
        *self.index.entry(p).or_insert_with(|| {
            self.pairs.push(p);
            n
        })
    }
}

/// A slice's dup-pair equality masks (per thread and kernel): `mask[p]`
/// holds when `stamp[p]` is the slice's.
#[derive(Default)]
struct DupMemo {
    slice: u32,
    stamp: Vec<u32>,
    mask: Vec<u16>,
}

impl DupMemo {
    /// The next slice (`pairs` many pairs).
    fn next(&mut self, pairs: usize) {
        if self.stamp.len() < pairs {
            self.stamp.resize(pairs, 0);
            self.mask.resize(pairs, 0);
        }
        self.slice = self.slice.wrapping_add(1);
        if self.slice == 0 {
            self.stamp.fill(0);
            self.slice = 1;
        }
    }

    /// The lanes where pair `p` of `pairs` is equal (`half`: the output
    /// buffer's halves' distance).
    #[inline]
    fn eq<N: KernelLanes>(&mut self, pairs: &DupPairs, p: u32, buf: &[u8], half: usize) -> u16 {
        let p = p as usize;
        if self.stamp[p] == self.slice {
            return self.mask[p];
        }
        let (a, b, kind) = pairs.pairs[p];
        let m = match kind {
            Ty::Num => N::eq_slots(buf, half, a, b),
            Ty::Ival => N::eq_slots(buf, half, a, b) & N::eq_slots(buf, half, a + 64, b + 64),
            Ty::Bool | Ty::UBool => {
                let ((va, ka), (vb, kb)) = (N::masks(buf, half, a), N::masks(buf, half, b));
                !((va ^ vb) | (ka ^ kb))
            }
        };
        self.stamp[p] = self.slice;
        self.mask[p] = m;
        m
    }
}

impl DupRef {
    /// The lanes of `cand` on which every pair is equal.
    #[inline]
    fn eq_mask<N: KernelLanes>(&self, pairs: &DupPairs, memo: &mut DupMemo, buf: &[u8], half: usize, cand: u16) -> u16 {
        let mut m = cand;
        for &p in &self.pairs {
            m &= memo.eq::<N>(pairs, p, buf, half);
            if m == 0 {
                break;
            }
        }
        m
    }
}

/// A per-outcome accumulator recipe: its rows' skeleton and the boundary's
/// constants, so the append emits rows already at the boundary.
struct AccTemplate<N: Arithmetic = P8> {
    shape_hash: u64,
    /// The key's uniform fields: every KEYED value cell uniform at the
    /// boundary, at its WIDENED value, as `(cell, code)` by cell.
    uniform: Vec<(u32, Code<N>)>,
    /// The uniform FOLLOWERS (`exact::Deps`, the record's): `(dependency,
    /// slot, cell, code)` by cell.
    uniform_follow: Vec<(u32, u32, u32, Code<N>)>,
    /// The shape as a key space records it (checked against its record).
    record: ShapeRecord,
    /// This outcome's own skeleton: a field's column empty and typed, or
    /// uniform where every row holds one value (`OutField::konst_av`, a
    /// widened field's `widen_uniform`, the buttons' unknown).
    own: Rt2<N>,
    /// The SHAPE's skeleton, typed wherever any outcome varies a cell: every
    /// queue and piece of the shape has these columns. Set by `unify`.
    union: std::sync::Arc<Rt2<N>>,
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

/// One shape's assembled kernel and the metadata the append needs, in the
/// lanes of `N` (`KernelLanes`).
struct AsmKernel<N: KernelLanes = P8> {
    loaded: Loaded,
    compiled: Compiled,
    bodies: Vec<AsmBody>,
    dup_pairs: DupPairs,
    /// What the bodies read at emission (`emit`).
    plan: emit::Plan,
    acc_templates: Vec<AccTemplate<N>>,
    forks: u8,
    /// Per body: where its roots go in the shape's union columns (`unify`).
    body_cols: Vec<BodyCols<N>>,
    /// The fused graph, roots and room for `explain_error`; empty unless
    /// `--kernel-explain` (`BuildOptions::keep_graphs`; gigabytes per set).
    fused: crate::transpile::graph::Graph<N>,
    flat_roots: Vec<crate::transpile::graph::NodeId>,
    room: Room,
    fused_nodes: usize,
    /// Per input cell, its traced path (named by packing failures).
    input_names: Vec<String>,
    /// `bench frame --metrics`: the slices this kernel ran (`compiled::mix`).
    mix_slices: Option<std::sync::Arc<std::sync::atomic::AtomicU64>>,
}

impl<N: KernelLanes> AsmKernel<N> {
    /// The input and output buffers' halves' distances (`Compiled::halves`;
    /// 0 for one half), and the buffers' sizes.
    fn halves(&self) -> ((usize, usize), (usize, usize)) {
        let (i, o) = (self.compiled.input_bytes as usize, self.compiled.out_bytes as usize);
        if self.compiled.halves {
            ((i, o), (2 * i, 2 * o))
        } else {
            ((0, 0), (i, o))
        }
    }

    /// Run `lanes` of one block in 16-lane slices, emitting each body's
    /// `live & !error` lanes into the sink with their edges. Any `live &
    /// error` (declined) fails the call: a coverage gap.
    fn run(&self, chunk: &Rt2<N>, lanes: &[usize], sink: &mut crate::storage::unit::UnitSink<N>) -> bool {
        sink.stats.add(S::KernelCalls, 1);
        for part in lanes.chunks(16) {
            if !self.run_slice(chunk, part, sink) {
                return false;
            }
        }
        true
    }

    /// Run ONE slice of up to 16 lanes (of any id groups): the kernel, each
    /// body's lanes (`live & !error`, less those an earlier body emits
    /// identically), then their EMISSION (`emit`).
    fn run_slice(&self, chunk: &Rt2<N>, lanes: &[usize], sink: &mut crate::storage::unit::UnitSink<N>) -> bool {
        let t_setup = sink.stats.tick_start();
        let n = lanes.len();
        debug_assert!((1..=16).contains(&n));
        if let Some(c) = &self.mix_slices {
            c.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        let views = self.input_views(chunk);
        let env = CollisionEnv { cache: &chunk.cache };
        let ctx = AsmCtx::<N>::new(&env as *const CollisionEnv as *const c_void);
        // Per-thread reused buffers: a fresh allocation faults every page.
        let mut sc = Scratch::take(self);
        let Scratch { inbuf, outbuf, took, takes, parts, dups, .. } = &mut sc;
        dups.next(self.dup_pairs.pairs.len());

        // Utilization tallies, into the sink's stats at the end.
        let (mut n_bodies, mut n_bodies_taken, mut n_lanes) = (0u64, 0u64, 0u64);
        let t_ph = sink.stats.tick(S::PhSetup, t_setup);
        let ((ihalf, ohalf), _) = self.halves();
        self.pack_input(&views, lanes, inbuf, ihalf);
        let t_ph = sink.stats.tick(S::PhPack, t_ph);
        // The spill frame is on THIS thread's stack: refuse rather than overrun.
        let (frame, stack) = (self.compiled.frame_bytes as usize, thread_stack());
        assert!(
            frame + KERNEL_STACK_MARGIN <= stack,
            "kernel {}: its {:.1} MB stack frame does not fit this thread's {:.1} MB stack (a kernel thread needs `set_thread_stack` and a larger `stack_size`)",
            self.compiled.sym,
            frame as f64 / 1048576.0,
            stack as f64 / 1048576.0
        );
        N::before_call();
        unsafe {
            (self.loaded.func)(
                inbuf.as_ptr(),
                outbuf.as_mut_ptr(),
                &ctx as *const AsmCtx<N> as *const c_void,
            );
        }
        let t_emit = sink.stats.tick(S::PhKernel, t_ph);
        let valid = ((1u32 << n) - 1) as u16;
        for (bi, body) in self.bodies.iter().enumerate() {
            // Tri-state masks, each read where it MAY hold.
            let error = read_zb_may::<N>(outbuf, ohalf, body.error_off);
            let live = read_zb_may::<N>(outbuf, ohalf, body.live_off);
            // Only expanded lanes reach a slice: an unexpanded row's (an
            // exit, a lost berry) error would be no decline.
            if live & error & valid != 0 {
                // Declined: fatal in the caller.
                self.report_decline(chunk, lanes, body, live, error, live & error & valid);
                sc.put_back();
                return false;
            }
            let mut take = live & !error & valid;
            // A lane whose emission equals an earlier body's is that
            // body's emission again: skip it (`DupRef`).
            took[bi] = take;
            if take != 0 {
                for r in &body.dup_refs {
                    let cand = take & took[r.body];
                    if cand != 0 {
                        take &= !(cand & r.eq_mask::<N>(&self.dup_pairs, dups, outbuf, ohalf, cand));
                        if take == 0 {
                            break;
                        }
                    }
                }
            }
            takes[bi] = take;
            n_bodies += 1;
            if take != 0 {
                n_bodies_taken += 1;
                n_lanes += take.count_ones() as u64;
            }
        }
        self.emit(emit::Slice { chunk, lanes, buf: outbuf, half: ohalf, takes, parts }, sink);
        if t_emit != 0 {
            sink.stats.tick(S::PhEmit, t_emit);
        }
        sc.put_back();
        for (k, v) in [(S::KernelRows, n as u64), (S::KernelSlices, 1), (S::KernelBodies, n_bodies), (S::KernelBodiesTaken, n_bodies_taken), (S::KernelTakes, n_lanes)] {
            sink.stats.add(k, v);
        }
        true
    }

    /// A decline (`declined`: the lanes `live & error`): the lanes, their
    /// player's `rem` and `spd`, and (`--kernel-explain`) why the first
    /// one's `error` holds.
    #[cold]
    fn report_decline(&self, chunk: &Rt2<N>, lanes: &[usize], body: &AsmBody, live: u16, error: u16, declined: u16) {
        let ids = crate::compiled::ids();
        let mut rows = String::new();
        for i in crate::storage::unit::lanes_of(declined) {
            let lane = lanes[i];
            rows.push_str(&format!("lane {lane}:"));
            for obj in chunk.player_objects() {
                for (name, f) in [("rem", ids.f_rem), ("spd", ids.f_spd)] {
                    let Some(Col::U(AV::Ptr(sub))) = chunk.obj_field_cell(obj, f).map(|pc| &chunk.cols[pc as usize]) else { continue };
                    for (axis, g) in [("x", ids.f_x), ("y", ids.f_y)] {
                        if let Some(c) = chunk.obj_field_cell(*sub, g) {
                            rows.push_str(&format!(" {name}.{axis}={:?}", chunk.cols[c as usize].at(lane)));
                        }
                    }
                }
            }
            rows.push('\n');
        }
        eprintln!(
            "[kernel] shape {:#x} body of outcome {} declined lanes {:#06x} (live {:#06x} error {:#06x}):\n{rows}",
            chunk.shape_hash, body.outcome, declined, live, error
        );
        match self.flat_roots.get(body.error_root) {
            Some(&error_node) => self.explain_error(chunk, lanes[declined.trailing_zeros() as usize], error_node),
            None => eprintln!("[kernel] rerun with --kernel-explain for the error the lane holds"),
        }
    }

    /// Why `error_node` holds on `lane`: its disjuncts not false, each
    /// followed down to its first undecided condition, and its failing
    /// premises.
    fn explain_error(&self, chunk: &Rt2<N>, lane: usize, error_node: crate::transpile::graph::NodeId) {
        use crate::transpile::graph::{Op, Val};
        let mut cells: std::collections::HashMap<u32, Val<N>> = Default::default();
        for &cell in &self.compiled.input_cells {
            let v = match chunk.cols[cell as usize].at(lane) {AV::Num(p) => Val::Num(N::point(p)), AV::Ival(a, b) => Val::Num(N::ival(a, b)), AV::Bool(b) => Val::Bool(Some(b)), AV::UBool => Val::Bool(None), _ => continue};
            cells.insert(cell, v);
        }
        let vals = match self.fused.eval(&cells, Eval::NarrowTop, Some(&self.room)) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("[kernel]   (graph evaluation failed: {e:#})");
                return;
            }
        };
        let g = &self.fused;
        let why = crate::transpile::why::Explain { g, vals: Some(&vals) };
        let val = |n: crate::transpile::graph::NodeId| vals[n as usize];
        for n in why.disjuncts(error_node).into_iter().filter(|&n| val(n) != Val::Bool(Some(false))).take(6) {
            eprintln!("[kernel]   error disjunct {}", why.with_args(n));
            // Down Sels (and undecided booleans) to the first undecided condition.
            let chain = why.chain(n, 12, |cur| {
                let nd = g.get(cur);
                match nd.op {
                    Op::Sel => match val(nd.args[0]) {
                        Val::Bool(Some(b)) => Some(nd.args[if b { 1 } else { 2 }]),
                        _ => None,
                    },
                    Op::Le | Op::Ge | Op::Lt | Op::Gt | Op::Eq | Op::Same | Op::ForkOk(..) => nd.args.iter().copied().find(|&a| g.get(a).op == Op::Sel),
                    Op::And | Op::Or | Op::Not => nd.args.iter().copied().find(|&a| val(a) == Val::Bool(None)),
                    _ => None,
                }
            });
            let end = chain[chain.len() - 1];
            if chain.len() <= 12 {
                let nd = g.get(end);
                if nd.op == Op::Sel {
                    // Its condition is undecided, and one level into the comparison's operands.
                    let c = nd.args[0];
                    eprintln!("[kernel]     undecided condition {}", why.with_args(c));
                    for &a in g.get(c).args.iter().filter(|&&a| g.get(a).op == Op::Sel) {
                        eprintln!("[kernel]       operand {}", why.with_args(a));
                    }
                } else {
                    eprintln!("[kernel]     undecided at {}", why.with_args(end));
                    eprintln!("[kernel]       = {}", why.tree(end, 6));
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
                let nd = g.get(x);
                match nd.op {
                    Op::And | Op::Or => stack.extend(nd.args.iter().copied().filter(|&a| val(a) == Val::Bool(Some(false)))),
                    Op::Sel => match val(nd.args[0]) {
                        Val::Bool(Some(b)) => stack.push(nd.args[if b { 1 } else { 2 }]),
                        _ => {}
                    },
                    _ => {
                        leaves += 1;
                        eprintln!("[kernel]     failing premise {}", why.with_args(x));
                        // A `Known` premise: its undecided operands.
                        if nd.op == Op::Known {
                            let mut todo = vec![(nd.args[0], 0usize)];
                            let mut shown = std::collections::HashSet::new();
                            while let Some((y, depth)) = todo.pop() {
                                if depth > 6 || !shown.insert(y) || val(y) != Val::Bool(None) {
                                    continue;
                                }
                                eprintln!("[kernel]       undecided {}{}", "  ".repeat(depth), why.with_args(y));
                                todo.extend(g.get(y).args.iter().map(|&a| (a, depth + 1)));
                            }
                        }
                    }
                }
            }
        }
    }

    /// The input columns of `chunk` as slices, resolved once per call.
    fn input_views<'c>(&self, chunk: &'c Rt2<N>) -> Vec<InputView<'c, N>> {
        self.compiled
            .input_cells
            .iter()
            .zip(&self.compiled.input_types)
            .zip(&self.input_names)
            .map(|((&cell, ty), name)| {
                InputView::of(&chunk.cols[cell as usize], *ty)
                    .unwrap_or_else(|e| panic!("{e}: input cell {cell} = {name} of shape {:#x}", chunk.shape_hash))
            })
            .collect()
    }

    /// Pack `lanes` into `buf` (`half`: its halves' distance); tail lanes
    /// repeat the last valid one so call-outs never see garbage (the
    /// `take` mask discards them).
    fn pack_input(&self, views: &[InputView<N>], lanes: &[usize], buf: &mut [u8], half: usize) {
        let n = lanes.len();
        for (view, &off) in views.iter().zip(&self.compiled.input_offsets) {
            let off = off as usize;
            let lane = |l: usize| lanes[l.min(n - 1)];
            match view {
                InputView::Num(col) => {
                    for l in 0..16 {
                        N::put(buf, half, off, l, col[lane(l)].raw());
                    }
                }
                InputView::NumU(raw) => {
                    for l in 0..16 {
                        N::put(buf, half, off, l, *raw);
                    }
                }
                InputView::Ival(col) => {
                    for l in 0..16 {
                        let (a, b) = col[lane(l)];
                        N::put(buf, half, off, l, a.raw());
                        N::put(buf, half, off + 64, l, b.raw());
                    }
                }
                InputView::IvalOfNum(col) => {
                    for l in 0..16 {
                        let raw = col[lane(l)].raw();
                        N::put(buf, half, off, l, raw);
                        N::put(buf, half, off + 64, l, raw);
                    }
                }
                InputView::IvalU(a, b) => {
                    for l in 0..16 {
                        N::put(buf, half, off, l, *a);
                        N::put(buf, half, off + 64, l, *b);
                    }
                }
                InputView::BoolU(mask) => N::put_masks(buf, half, off, *mask, None),
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
                    N::put_masks(buf, half, off, mask, None);
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
                    N::put_masks(buf, half, off, val, Some(known));
                }
                InputView::UBoolU(val, known) => N::put_masks(buf, half, off, *val, Some(*known)),
                InputView::Any(col, ty) => {
                    // The general case: a materialized `AV` column, per value.
                    for l in 0..16 {
                        let v = col.at(lane(l));
                        match ty {
                            Ty::Num => N::put(buf, half, off, l, num_raw(v)),
                            Ty::Ival => {
                                let (a, b) = ival_raw(v);
                                N::put(buf, half, off, l, a);
                                N::put(buf, half, off + 64, l, b);
                            }
                            Ty::Bool | Ty::UBool => unreachable!("bool inputs are Bool/BoolU/UBool/UBoolU views"),
                        }
                    }
                }
            }
        }
    }
}

/// Per (thread, kernel) reusable input and output buffers.
pub struct Scratch<N: KernelLanes = P8> {
    kernel: usize,
    inbuf: Vec<u8>,
    outbuf: Vec<u8>,
    /// Per body, the lanes it took this slice before `dup_refs` masking,
    /// and after.
    took: Vec<u16>,
    takes: Vec<u16>,
    dups: DupMemo,
    /// Per outcome, its uniform fields keyed in the key space of generation
    /// `.0` (`PartKey`).
    parts: Vec<(u64, PartKey<N>)>,
}

impl<N: KernelLanes> Scratch<N> {
    /// This thread's scratch for `k`, taken out until `put_back`.
    fn take(k: &AsmKernel<N>) -> Scratch<N> {
        let id = k as *const AsmKernel<N> as usize;
        let (_, (ib, ob)) = k.halves();
        N::scratch().with(|c| {
            let mut c = c.borrow_mut();
            match c.iter().position(|s| s.kernel == id) {
                Some(i) => c.swap_remove(i),
                None => Scratch {
                    kernel: id,
                    inbuf: vec![0u8; ib],
                    outbuf: vec![0u8; ob],
                    took: vec![0u16; k.bodies.len()],
                    takes: vec![0u16; k.bodies.len()],
                    dups: DupMemo::default(),
                    parts: (0..k.acc_templates.len()).map(|_| (0, PartKey::default())).collect(),
                },
            }
        })
    }

    fn put_back(self) {
        N::scratch().with(|c| c.borrow_mut().push(self));
    }
}

/// A body's roots by kind with their union columns, plus its uniform cells.
struct BodyCols<N: Arithmetic = P8> {
    num: Vec<(usize, usize)>,
    ival: Vec<(usize, usize)>,
    /// A NUMBER root into a union `Ival` column, stored `[v, v]`.
    num_as_ival: Vec<(usize, usize)>,
    /// `(column, root, may the root be unknown)` (`AsmField::may_unknown`).
    bool_: Vec<(usize, usize, bool)>,
    uniform_num: Vec<(usize, N::Raw)>,
    uniform_ival: Vec<(usize, (N::Raw, N::Raw))>,
    uniform_bool: Vec<(usize, u8)>,
}

impl<N: KernelLanes> BodyCols<N> {
    fn of(body: &AsmBody, proto: &crate::storage::unit::RowBuf<N>, own: &Rt2<N>) -> Result<Self> {
        use crate::storage::unit::TCol;
        let (mut num, mut ival, mut num_as_ival, mut bool_) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
        let mut written = vec![false; proto.cols.len()];
        for f in &body.fields {
            // A field the union holds uniform (e.g. `rem`) is not stored.
            let Some(ci) = proto.cols.iter().position(|(cell, _)| *cell == f.cell) else {
                continue;
            };
            written[ci] = true;
            match (f.kind, &proto.cols[ci].1) {
                (Ty::Num, TCol::Num(_)) => num.push((ci, f.off)),
                (Ty::Ival, TCol::Ival(_)) => ival.push((ci, f.off)),
                (Ty::Num, TCol::Ival(_)) => num_as_ival.push((ci, f.off)),
                (Ty::Bool, TCol::Bool(_)) => bool_.push((ci, f.off, f.may_unknown)),
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
                (TCol::Num(_), AV::Num(n)) => uniform_num.push((ci, n.raw())),
                (TCol::Ival(_), AV::Ival(a, b)) => uniform_ival.push((ci, (a.raw(), b.raw()))),
                (TCol::Ival(_), AV::Num(n)) => uniform_ival.push((ci, (n.raw(), n.raw()))),
                (TCol::Bool(_), AV::Bool(x)) => uniform_bool.push((ci, *x as u8)),
                // An output widening's canonical unknown (`TCol::Bool`'s 2).
                (TCol::Bool(_), AV::UBool) => uniform_bool.push((ci, 2)),
                (_, v) => anyhow::bail!("union column at cell {cell}: the outcome's uniform {v:?} does not fit its kind"),
            }
        }
        Ok(BodyCols { num, ival, num_as_ival, bool_, uniform_num, uniform_ival, uniform_bool })
    }

    /// Append lane `i` of the output buffer (halves `half` apart) to `slot`
    /// as one row.
    #[inline]
    fn push_row(&self, slot: &mut crate::storage::unit::RowBuf<N>, buf: &[u8], half: usize, i: usize, key: (u64, u64), cell: u32) {
        use crate::storage::unit::TCol;
        let word = |base: usize| N::lane(buf, half, base, i);
        for &(ci, root) in &self.num {
            if let TCol::Num(v) = &mut slot.cols[ci].1 {
                v.push(word(root));
            }
        }
        for &(ci, root) in &self.ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                v.push((word(root), word(root + 64)));
            }
        }
        for &(ci, root) in &self.num_as_ival {
            if let TCol::Ival(v) = &mut slot.cols[ci].1 {
                let n = word(root);
                v.push((n, n));
            }
        }
        for &(ci, root, may_unknown) in &self.bool_ {
            if let TCol::Bool(v) = &mut slot.cols[ci].1 {
                let (val, known) = N::masks(buf, half, root);
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
enum InputView<'c, N: Arithmetic = P8> {
    Num(&'c [N]),
    NumU(N::Raw),
    Ival(&'c [(N, N)]),
    IvalOfNum(&'c [N]),
    IvalU(N::Raw, N::Raw),
    Bool(&'c [AV<N>]),
    BoolU(u16),
    /// A maybe-unknown bool (`Ty::UBool`), per lane or uniform.
    UBool(&'c [AV<N>]),
    UBoolU(u16, u16),
    Any(&'c Col<N>, Ty),
}

impl<'c, N: Arithmetic> InputView<'c, N> {
    fn of(col: &'c Col<N>, ty: Ty) -> Result<Self, String> {
        Ok(match (ty, col) {
            (Ty::Num, Col::N(v)) => InputView::Num(v),
            (Ty::Num, Col::U(AV::Num(n))) => InputView::NumU(n.raw()),
            (Ty::Ival, Col::I(v)) => InputView::Ival(v),
            (Ty::Ival, Col::N(v)) => InputView::IvalOfNum(v),
            (Ty::Ival, Col::U(AV::Ival(a, b))) => InputView::IvalU(a.raw(), b.raw()),
            (Ty::Ival, Col::U(AV::Num(n))) => InputView::IvalU(n.raw(), n.raw()),
            (Ty::Bool, Col::V(v)) => InputView::Bool(v),
            (Ty::Bool, Col::U(v)) => match v {
                AV::Bool(b) => InputView::BoolU(if *b { 0xffff } else { 0 }),
                // DECIDED only: an unknown packed as false drops the true arm.
                other => return Err(format!("ASM bool input column holds {other:?}: a kernel reads a boolean the block does not decide")),
            },
            (Ty::Bool, other) => return Err(format!("ASM bool input column is {other:?}")),
            (Ty::UBool, Col::V(v)) => InputView::UBool(v),
            (Ty::UBool, Col::U(AV::Bool(b))) => InputView::UBoolU(if *b { 0xffff } else { 0 }, 0xffff),
            (Ty::UBool, Col::U(AV::UBool)) => InputView::UBoolU(0, 0),
            (Ty::UBool, other) => return Err(format!("ASM maybe-unknown bool input column is {other:?}")),
            (ty, other) => InputView::Any(other, ty),
        })
    }
}

/// `--key-check`: the boundary over a copy of an emitted block changes
/// nothing (structure, shape hash), and every row's key is what the key
/// space makes of the boundary's row (packed, or provisional of equal content).
pub(crate) fn key_check<N: crate::search::rem::Rem>(slot: &crate::storage::unit::RowBuf<N>, keys: &KeySpace<N>, prov: &Provisional<N>) {
    use celeste_engine::exact::{is_provisional, RowKey};
    let acc = slot.to_rt2();
    let mut b = acc.clone_block();
    b.boundary_canonicalize();
    let want = keys.row_keys(&b);
    let bad: Vec<usize> = (0..acc.width)
        .filter(|&i| match &want[i] {
            RowKey::Hit(k) => acc.row_keys[i] != *k,
            RowKey::Miss(m) => !is_provisional(acc.row_keys[i]) || prov.content(acc.row_keys[i]) != *m,
        })
        .collect();
    if b.shape_hash == acc.shape_hash && b.structure == acc.structure && bad.is_empty() {
        return;
    }
    // Say WHICH rows and cells differ.
    let varying: Vec<usize> = (0..acc.cols.len())
        .filter(|&c| matches!(acc.structure.get(c), Some(celeste_engine::runtime2::Cell2::Val)))
        .filter(|&c| (1..acc.width).any(|i| acc.cols[c].at(i) != acc.cols[c].at(0)))
        .collect();
    let rows: Vec<String> = bad
        .iter()
        .take(8)
        .map(|&i| {
            let cells: Vec<String> = varying.iter().map(|&c| format!("{c}={:?}", acc.cols[c].at(i))).collect();
            format!("    row {i} append key {:?}, boundary {:?}: {}", acc.row_keys[i], want[i], cells.join(" "))
        })
        .collect();
    panic!(
        "KERNEL KEY CHECK: shape {:#x}: the boundary disagrees with the append step \
         (width {}, shape {:#x} vs {:#x}, structure {}, {} keys differ)\n{}",
        acc.shape_hash,
        acc.width,
        b.shape_hash,
        acc.shape_hash,
        if b.structure == acc.structure { "same" } else { "DIFFERENT" },
        bad.len(),
        rows.join("\n"),
    );
}

/// `--key-check` on a block the forward STORES (a layer's rows, decoded at
/// the frame's end): the boundary over a copy keeps its shape and
/// structure, and the key space keys its rows as stored.
pub(crate) fn key_check_block<N: celeste_core::arith::Arithmetic>(blk: &Rt2<N>, keys: &KeySpace<N>) {
    let mut b = blk.clone_block();
    b.boundary_canonicalize();
    let got: Vec<Option<(u64, u64)>> = keys.lookup_keys(&b);
    let same = got.iter().zip(&blk.row_keys).all(|(g, w)| *g == Some(*w));
    assert!(
        b.shape_hash == blk.shape_hash && b.structure == blk.structure && same,
        "KERNEL KEY CHECK: a stored piece of shape {:#x} ({} rows) is not what the boundary keys it as (shape {:#x}, structure {}, keys {})",
        blk.shape_hash,
        blk.width,
        b.shape_hash,
        if b.structure == blk.structure { "same" } else { "DIFFERENT" },
        if same { "same" } else { "DIFFERENT" },
    );
}

fn num_raw<N: Arithmetic>(av: AV<N>) -> N::Raw {
    match av {
        AV::Num(p) => p.raw(),
        other => panic!("ASM num input got {other:?}"),
    }
}

fn ival_raw<N: Arithmetic>(av: AV<N>) -> (N::Raw, N::Raw) {
    match av {
        AV::Ival(a, b) => (a.raw(), b.raw()),
        AV::Num(p) => (p.raw(), p.raw()),
        other => panic!("ASM ival input got {other:?}"),
    }
}

/// A tri-state mask read where it MAY hold (`val | !known`; `val` at +0,
/// `known` at +2). An unknown `live` reads live: emitting over-approximates
/// (sound, the concrete search refutes) where dropping loses successors. An
/// unknown `error` is a decline: strict.
fn read_zb_may<N: KernelLanes>(buf: &[u8], half: usize, off: usize) -> u16 {
    let (val, known) = N::masks(buf, half, off);
    val | !known
}

/// The start room's assembled kernels for one level.
pub struct Registry<N: KernelLanes = P8> {
    /// One kernel per reached (shape, region); a missing key is a coverage gap.
    kernels: HashMap<(u64, Option<Region>), AsmKernel<N>>,
    /// The region grid the set was built on.
    grid: Option<crate::trace::walk::RegionGrid>,
}

/// What a kernel set is built with besides its level and the settings that
/// shape the cart: the build's diagnostics.
struct BuildOptions {
    asm: crate::transpile::asm::AsmOptions,
    /// `--kernel-explain`: kernels keep their fused graphs, so a decline
    /// names its premise.
    keep_graphs: bool,
}

use crate::trace::walk::Region;

impl<N: KernelLanes> Registry<N> {
    pub fn len(&self) -> usize {
        self.kernels.len()
    }

    /// Its kernels' text, fingerprinted (`text_digest`).
    #[cfg(test)]
    pub(crate) fn text_digest(&self) -> u64 {
        text_digest(&self.kernels)
    }

    /// Its kernels' KEY LAYOUTS (`exact::Deps`), in key order: per outcome
    /// its record and uniform followers, per body its leaders' and per-row
    /// followers' cells (a cached set's must be the fresh one's).
    #[cfg(test)]
    pub(crate) fn key_layouts(&self) -> Vec<KeyLayout<N>> {
        let mut out: Vec<KeyLayout<N>> = self
            .kernels
            .iter()
            .map(|(k, v)| {
                let outcomes = v.acc_templates.iter().map(|t| (t.record.clone(), t.uniform_follow.clone())).collect();
                let bodies = (0..v.plan.bodies.len()).map(|bi| v.plan.follow_layout(bi)).collect();
                (*k, outcomes, bodies)
            })
            .collect();
        out.sort_unstable_by_key(|l| l.0);
        out
    }

    /// Run `chunk` (rows in cell order) on the matching kernels; `false` on a
    /// miss or a decline.
    pub fn run_chunk(
        &self,
        chunk: &Rt2<N>,
        cell_in: &[u32],
        lanes: &[usize],
        sink: &mut crate::storage::unit::UnitSink<N>,
    ) -> bool {
        if self.grid.is_none() {
            return self.run_key(chunk, None, lanes, sink);
        }
        // Regions only for shapes with a PLAYER (not `player_spawn`).
        let Some(grid) = self.grid.filter(|_| !chunk.player_objects().is_empty()) else {
            return self.run_key(chunk, None, lanes, sink);
        };
        // The unit's live lanes bucketed by region, each bucket cut into
        // slices of 16 (a slice may span id groups): a unit's cell order
        // changes region every ~12 rows, so slices in lane order would run
        // once per region they hold.
        sink.stats.add(S::KernelCalls, 1);
        let mut by_region: Vec<(Option<Region>, usize)> =
            lanes.iter().map(|&l| (grid.of_cell(cell_in[l]), l)).collect();
        by_region.sort_unstable();
        let mut idx: Vec<usize> = Vec::with_capacity(16);
        for run in by_region.chunk_by(|a, b| a.0 == b.0) {
            let key = run[0].0;
            let Some(k) = self.kernels.get(&(chunk.shape_hash, key)) else {
                return self.run_key(chunk, key, &[run[0].1], sink);
            };
            for part in run.chunks(16) {
                idx.clear();
                idx.extend(part.iter().map(|&(_, l)| l));
                if !k.run_slice(chunk, &idx, sink) {
                    return false;
                }
            }
        }
        true
    }

    fn run_key(&self, chunk: &Rt2<N>, key: Option<Region>, lanes: &[usize], sink: &mut crate::storage::unit::UnitSink<N>) -> bool {
        match self.kernels.get(&(chunk.shape_hash, key)) {
            Some(k) => k.run(chunk, lanes, sink),
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

    /// Retrace the start room and assemble every shape for level `opts`
    /// under settings `s`.
    pub fn build_for_start_room(opts: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Registry<N>> {
        match s.region_grid() {
            Some(g) => eprintln!("[region] kernels keyed on a {} px grid, player speed asserted within [-{}, {}] px", g.px, g.speed, g.speed),
            None => eprintln!("[region] no region key (--region off)"),
        }
        // The kernel cache, but for the diagnostic that keeps the build
        // (`--kernel-explain`).
        let store = (!s.diag.kernel_explain).then(super::cache::Store::default);
        Registry::build_with(opts, s, store.as_ref())
    }

    /// `build_for_start_room` with the kernel cache `store` (none: build).
    pub(crate) fn build_with(opts: crate::abstraction::Level, s: &crate::settings::Settings, store: Option<&super::cache::Store>) -> Result<Registry<N>> {
        let t0 = std::time::Instant::now();
        let key = store.map(|_| super::cache::key::<N>(opts, s)).transpose()?;
        if let (Some(store), Some(key)) = (store, &key) {
            if let Some(entry) = store.load::<N>(key)? {
                let cache = crate::compiled::collision_cache(s.room);
                let mut kernels = HashMap::new();
                for (i, (recipe, so)) in entry.into_iter().enumerate() {
                    let loaded = Loaded::from_bytes(&so, &format!("cached_{i}"), &recipe.compiled.sym)?;
                    let k = (recipe.shape, recipe.region);
                    anyhow::ensure!(kernels.insert(k, AsmKernel::of(recipe, loaded, cache.clone(), None)?).is_none(), "the kernel cache's entry {} holds kernel {k:?} twice", key.name);
                }
                unify(&mut kernels)?;
                eprintln!("[asm build] {} kernels: kernel cache HIT {}, {:.2}s; kernel text {:016x}", kernels.len(), key.name, t0.elapsed().as_secs_f64(), text_digest(&kernels));
                return Ok(Registry { kernels, grid: s.region_grid() });
            }
        }
        let build = BuildOptions {
            asm: crate::transpile::asm::AsmOptions { mix: s.diag.kernel_metrics },
            keep_graphs: s.diag.kernel_explain,
        };
        let build = &build;
        let refs = crate::trace::lower::lattice_kernel_refs::<N>(opts, s)
            .context("retracing the start room for ASM kernels")?;
        let trace_s = t0.elapsed().as_secs_f64();
        let t_asm = std::time::Instant::now();
        // The shapes in parallel; a shape's build recurses deeply.
        let pool = crate::par::Pool::per_core().stack(128 * 1024 * 1024);
        let n_workers = pool.workers(refs.len());
        let built = pool.map(refs.len(), |si| {
            let (region, r) = &refs[si];
            let (recipe, loaded, graphs) = build_one_shape::<N>(r, (r.frame.in_rt2.shape_hash_of(), *region), si, build)?;
            let stored = store.is_some().then(|| Ok::<_, anyhow::Error>((recipe.clone(), loaded.bytes()?))).transpose()?;
            let key = (recipe.shape, recipe.region);
            Ok((key, AsmKernel::of(recipe, loaded, r.cache.clone(), graphs)?, stored))
        })?;
        let mut kernels = HashMap::new();
        let mut first: HashMap<(u64, Option<Region>), usize> = HashMap::new();
        let mut entry: Vec<(Recipe<N>, Vec<u8>)> = Vec::new();
        for (si, ((shape, key), kernel, stored)) in built.into_iter().enumerate() {
            entry.extend(stored);
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
        let cached = match (store, &key) {
            (Some(store), Some(key)) => {
                store.store::<N>(key, &entry.iter().map(|(r, so)| (r, so.as_slice())).collect::<Vec<_>>())?;
                format!("kernel cache MISS {}, stored", key.name)
            }
            _ => "kernel cache off".to_string(),
        };
        eprintln!(
            "[asm build] {} kernels: trace {:.2}s, assemble {:.2}s ({} workers); {cached}; kernel text {:016x}",
            kernels.len(),
            trace_s,
            t_asm.elapsed().as_secs_f64(),
            n_workers,
            text_digest(&kernels),
        );
        Ok(Registry { kernels, grid: s.region_grid() })
    }
}

/// A kernel's key layout (`Registry::key_layouts`).
#[cfg(test)]
pub(crate) type KeyLayout<N> = ((u64, Option<Region>), Vec<(ShapeRecord, Vec<(u32, u32, u32, Code<N>)>)>, Vec<emit::FollowLayout>);

/// A kernel set's text, fingerprinted: each kernel's (shape, region) and
/// text digest (`Compiled::text_digest`), in key order.
fn text_digest<N: KernelLanes>(kernels: &HashMap<(u64, Option<Region>), AsmKernel<N>>) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut all: Vec<((u64, Option<Region>), u64)> = kernels.iter().map(|(k, v)| (*k, v.compiled.text_digest)).collect();
    all.sort_unstable();
    let mut h = std::collections::hash_map::DefaultHasher::new();
    all.hash(&mut h);
    h.finish()
}

/// The type of a typed column (or of a uniform value one could hold).
fn kind_of<N: Arithmetic>(col: &Col<N>) -> Option<Ty> {
    match col {
        Col::N(_) | Col::U(AV::Num(_)) => Some(Ty::Num),
        Col::I(_) | Col::U(AV::Ival(..)) => Some(Ty::Ival),
        Col::V(_) | Col::U(AV::Bool(_)) | Col::U(AV::UBool) => Some(Ty::Bool),
        _ => None,
    }
}

fn typed<N: Arithmetic>(kind: Ty) -> Col<N> {
    match kind {
        Ty::Num => Col::N(Vec::new()),
        Ty::Ival => Col::I(Vec::new()),
        Ty::Bool | Ty::UBool => Col::V(Vec::new()),
    }
}

/// Give every outcome template of a SHAPE the union skeleton (a cell is
/// typed if any outcome varies it or two disagree; Num + Ival -> Ival) and
/// map every body onto its columns (`BodyCols`).
fn unify<N: KernelLanes>(by_shape: &mut HashMap<(u64, Option<Region>), AsmKernel<N>>) -> Result<()> {
    let mut unions: HashMap<u64, Rt2<N>> = HashMap::new();
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
                            (Some(Ty::Num), Some(Ty::Ival)) | (Some(Ty::Ival), Some(Ty::Num)) => Ty::Ival,
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
    let unions: HashMap<u64, std::sync::Arc<Rt2<N>>> = unions.into_iter().map(|(k, v)| (k, std::sync::Arc::new(v))).collect();
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

fn apply_union<N: KernelLanes>(kernel: &mut AsmKernel<N>, unions: &HashMap<u64, std::sync::Arc<Rt2<N>>>) -> Result<()> {
    for t in &mut kernel.acc_templates {
        t.union = unions
            .get(&t.shape_hash)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("no union skeleton for output shape {:#x}", t.shape_hash))?;
    }
    let mut body_cols = Vec::with_capacity(kernel.bodies.len());
    for b in &kernel.bodies {
        let t = &kernel.acc_templates[b.outcome];
        let proto = crate::storage::unit::RowBuf::new((*t.union).clone_block());
        body_cols.push(BodyCols::of(b, &proto, &t.own)?);
    }
    kernel.body_cols = body_cols;
    Ok(())
}

/// A KERNEL AS ITS BUILD LEFT IT: what tracing, lowering and assembling
/// one (shape, region) made, as plain data; `AsmKernel::of` makes the
/// rest, alike for a fresh build and a cached one (`compiled::cache`).
#[derive(Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub(crate) struct Recipe<N: Arithmetic = P8> {
    pub(crate) shape: u64,
    pub(crate) region: Option<Region>,
    pub(crate) compiled: Compiled,
    pub(crate) forks: u8,
    pub(crate) fused_nodes: usize,
    /// Per root slot: does it read the canonical output unknown
    /// (`AsmField::may_unknown`)?
    pub(crate) may_unknown: Vec<bool>,
    pub(crate) bodies: Vec<BodySlots>,
    /// Per outcome, its rows' skeleton (`AccTemplate::own`).
    pub(crate) outcomes: Vec<Skeleton<N>>,
    /// Per input cell, its traced path (named by packing failures).
    pub(crate) input_names: Vec<String>,
    /// `--metrics`: the kernel census.
    pub(crate) census: Option<crate::compiled::mix::Census>,
}

/// A fused body's roots as output slots (`Compiled::root_offsets`'
/// indices).
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct BodySlots {
    outcome: usize,
    splits: Vec<u8>,
    /// Per stored field, its cell and slot.
    fields: Vec<(u32, u32)>,
    error: u32,
    live: u32,
    /// The transfer roots' slots (`ArcRoots::nodes`' order), and `fin`.
    arc: ([u32; 2 * crate::transpile::kernel::AxisRoots::N], bool),
}

/// An outcome's rows' skeleton: a field's column empty and typed, or
/// uniform where every row holds one value (`OutField::uniform`, the
/// buttons' unknown); its shape hash taken before (`Rt2::shape_hash_of`).
#[derive(Clone, serde::Serialize, serde::Deserialize)]
#[serde(bound = "")]
pub(crate) struct Skeleton<N: Arithmetic = P8> {
    shape_hash: u64,
    structure: Vec<celeste_engine::runtime2::Cell2<N>>,
    cols: Vec<Col<N>>,
    globals: Vec<u32>,
}

impl<N: Arithmetic> Skeleton<N> {
    /// Outcome `oi` of a lowered frame's.
    fn of<A: KernelLanes>(r: &crate::trace::lower::LoweredFrame<A>, oi: usize) -> Skeleton<A> {
        // Canonical: `block::skeleton` ends in `Rt2::canonicalize_ids`.
        let mut own = reshape(&r.frame.outs[oi].rt2, 0);
        let shape_hash = own.shape_hash_of();
        for f in &r.bound.outcomes[oi].fields {
            own.cols[f.cell as usize] = match f.uniform {
                Some(av) => Col::U(av),
                None => typed(f.ty),
            };
        }
        for &cell in &r.frame.outs[oi].ubool_cells {
            own.cols[cell as usize] = Col::U(AV::UBool);
        }
        Skeleton { shape_hash, structure: own.structure, cols: own.cols, globals: own.globals }
    }

    /// As a block of no rows in `cache`'s room.
    fn rt2(&self, cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>) -> Rt2<N> {
        let mut b = Rt2::empty(0, self.globals.len(), celeste_names::STRINGS, cache);
        b.globals = self.globals.clone();
        b.structure = self.structure.clone();
        b.cols = self.cols.clone();
        b
    }
}

impl<N: Arithmetic> AccTemplate<N> {
    /// The template of skeleton `own` (`shape_hash` its shape's): the key's
    /// uniform fields (every value cell uniform here, at its widened value;
    /// a follower's among `uniform_follow`) and the shape's record, its key
    /// laid out as the shape declares (`widening::key_deps`).
    fn of(own: Rt2<N>, shape_hash: u64) -> Result<AccTemplate<N>> {
        let pos = own.position_cells();
        let mut uniform = Vec::new();
        for (c, cell) in own.structure.iter().enumerate() {
            let (celeste_engine::runtime2::Cell2::Val, Col::U(v)) = (cell, &own.cols[c]) else {
                continue;
            };
            let pos = pos.is_some_and(|(x, y)| c as u32 == x || c as u32 == y);
            uniform.push((c as u32, if pos { Code::of_pos(*v) } else { Code::of(*v) }));
        }
        let mut canon = own.clone_block();
        canon.shape_hash = shape_hash;
        let record = ShapeRecord::of(&canon);
        anyhow::ensure!(record.deps.len() <= emit::MAX_DEPS, "shape {shape_hash:#x}: {} key dependencies, past the emission's {}", record.deps.len(), emit::MAX_DEPS);
        // The followers are no dictionary's: theirs is the record by leader.
        let (follow, uniform): (Vec<_>, Vec<_>) = uniform.into_iter().partition(|(c, _)| follower_of(&record, *c).is_some());
        let uniform_follow = follow
            .into_iter()
            .map(|(c, code)| {
                let (g, slot) = follower_of(&record, c).expect("a follower");
                (g, slot, c, code)
            })
            .collect();
        Ok(AccTemplate { shape_hash, uniform, uniform_follow, record, union: std::sync::Arc::new(own.clone_block()), own })
    }
}

/// `--kernel-explain`'s fused graph and roots of a kernel.
type Graphs<N> = Option<(crate::transpile::graph::Graph<N>, Vec<crate::transpile::graph::NodeId>)>;

/// Assemble ONE lowered frame (gcc + dlopen) as kernel `key`'s recipe and
/// its loaded code; `tag` distinguishes a shape's region kernels. With
/// `keep_graphs` (`--kernel-explain`), also its fused graph and roots.
fn build_one_shape<N: KernelLanes>(
    r: &crate::trace::lower::LoweredFrame<N>,
    key: (u64, Option<Region>),
    si: usize,
    build: &BuildOptions,
) -> Result<(Recipe<N>, Loaded, Graphs<N>)> {
    use crate::transpile::graph::NodeId;
    let shape = key.0;
    let (fused, bodies) = (&r.lowered.fused, &r.lowered.bodies);
    // One output SLOT per DISTINCT root node, numbered by first appearance
    // in the bodies' roots in the kernel's order (`Roots::flat`).
    let mut flat_roots: Vec<NodeId> = Vec::new();
    let mut slot_of: HashMap<NodeId, u32> = HashMap::new();
    for n in bodies.iter().flat_map(|b| b.roots.flat()) {
        slot_of.entry(n).or_insert_with(|| {
            flat_roots.push(n);
            flat_roots.len() as u32 - 1
        });
    }
    let slot = |n: NodeId| slot_of[&n];
    let tag = format!("_{si}");
    let (mut compiled, loaded) = crate::transpile::asm::compile_and_load(fused, &flat_roots, &format!("k{shape:016x}{tag}"), build.asm).with_context(|| format!("assembling shape {si} (hash {shape:#x})"))?;
    // Which fused nodes read the canonical output unknown (`may_unknown`).
    let reads_unknown_output: Vec<bool> = {
        let mut out = vec![false; fused.len()];
        for n in 0..fused.len() {
            let node = fused.get(n as NodeId);
            out[n] = node.op == crate::transpile::graph::Op::UnknownBool(crate::transpile::graph::Unknown::Output) || node.args.iter().any(|a| out[*a as usize]);
        }
        out
    };
    let mut slots = Vec::with_capacity(bodies.len());
    for b in bodies {
        let outcome = &r.bound.outcomes[b.outcome];
        anyhow::ensure!(
            outcome.fields.len() == b.roots.fields.len(),
            "shape {si} outcome {}: {} outputs vs {} field roots",
            b.outcome,
            outcome.fields.len(),
            b.roots.fields.len()
        );
        // Every body computes its transfer (the rotation graph needs it).
        let a = b.roots.arc.ok_or_else(|| anyhow::anyhow!("shape {si} outcome {}: no transfer roots", b.outcome))?;
        slots.push(BodySlots {
            outcome: b.outcome,
            splits: b.splits.clone(),
            fields: outcome.fields.iter().zip(&b.roots.fields).map(|(f, &n)| (f.cell, slot(n))).collect(),
            error: slot(b.roots.error),
            live: slot(b.roots.live),
            arc: (a.nodes().map(slot), a.fin),
        });
    }
    let input_names: Vec<String> = compiled
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
    let census = if build.asm.mix {
        use crate::compiled::mix::{ROLE_ARC, ROLE_ERROR, ROLE_FIELD, ROLE_LIVE};
        let mut roles = vec![0u8; flat_roots.len()];
        for b in bodies {
            for &n in &b.roots.fields {
                roles[slot(n) as usize] |= ROLE_FIELD;
            }
            roles[slot(b.roots.error) as usize] |= ROLE_ERROR;
            roles[slot(b.roots.live) as usize] |= ROLE_LIVE;
            for n in b.roots.arc.iter().flat_map(|a| a.nodes()) {
                roles[slot(n) as usize] |= ROLE_ARC;
            }
        }
        let guards: Vec<_> = bodies.iter().map(|b| (b.roots.error, b.roots.live)).collect();
        let census = N::mix_report(&crate::compiled::mix::Input { traced: &r.bound.graph, fused, roots: &flat_roots, roles: &roles, bodies: &guards, compiled: &compiled })
            .with_context(|| format!("the census of {}", compiled.sym))?;
        compiled.asm = String::new();
        Some(census)
    } else {
        None
    };
    let recipe = Recipe {
        shape,
        region: key.1,
        compiled,
        forks: r.bound.forks,
        fused_nodes: fused.len(),
        may_unknown: flat_roots.iter().map(|n| reads_unknown_output[*n as usize]).collect(),
        bodies: slots,
        outcomes: (0..r.bound.outcomes.len()).map(|oi| Skeleton::<N>::of(r, oi)).collect(),
        input_names,
        census,
    };
    let graphs = build.keep_graphs.then(|| (fused.clone(), flat_roots));
    Ok((recipe, loaded, graphs))
}

impl<N: KernelLanes> AsmKernel<N> {
    /// The kernel of `recipe`, its code `loaded`, in `cache`'s room (`graphs`:
    /// `--kernel-explain`'s fused graph and roots): its accumulator
    /// templates, its bodies on their slots with what each reads at
    /// emission (`emit::Plan`), the earlier bodies each repeats.
    fn of(
        recipe: Recipe<N>,
        loaded: Loaded,
        cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
        graphs: Graphs<N>,
    ) -> Result<AsmKernel<N>> {
        let Recipe { shape: recipe_shape, compiled, forks, fused_nodes, may_unknown, bodies, outcomes, input_names, census, .. } = recipe;
        let acc_templates: Vec<AccTemplate<N>> = outcomes.iter().map(|sk| AccTemplate::of(sk.rt2(cache.clone()), sk.shape_hash)).collect::<Result<_>>()?;
        // Per outcome, its position coordinates' cells (keyed without their whole pixels).
        let pos_cells: Vec<Option<(u32, u32)>> = acc_templates.iter().map(|t| t.own.position_cells()).collect();
        let off = |s: u32| compiled.root_offsets[s as usize] as usize;
        let kind = |s: u32| compiled.root_kinds[s as usize];
        let mut asm_bodies = Vec::with_capacity(bodies.len());
        // What each body reads at emission (`emit::Plan`).
        let mut reads = Vec::with_capacity(bodies.len());
        for b in bodies {
            let own = &acc_templates[b.outcome].own;
            let fields = b.fields.iter().map(|&(cell, s)| AsmField { cell: cell as usize, off: off(s), kind: kind(s), may_unknown: may_unknown[s as usize] }).collect();
            // A uniform field is the key's constant prefix, not a per-lane field.
            let key_fields = b
                .fields
                .iter()
                .filter(|(cell, _)| !matches!(own.cols[*cell as usize], Col::U(_)))
                .map(|&(cell, s)| {
                    let pos = pos_cells[b.outcome].is_some_and(|(x, y)| cell == x || cell == y);
                    // A number stored `[v, v]` keys as the number.
                    KeyField::new(cell, off(s), kind(s), pos)
                })
                .collect::<Result<Vec<_>>>()?;
            // The FOLLOWERS (`exact::Deps`) are checked, not keyed; every
            // one, and its leader, is per-row or uniform in each outcome
            // (else a row's follower would go unchecked).
            let t = &acc_templates[b.outcome];
            let (follow, key_fields): (Vec<KeyField>, Vec<KeyField>) = key_fields.into_iter().partition(|f| follower_of(&t.record, f.cell).is_some());
            let follow_fields: Vec<(KeyField, u32, u32)> = follow
                .into_iter()
                .map(|f| {
                    let (g, slot) = follower_of(&t.record, f.cell).expect("a follower");
                    (f, g, slot)
                })
                .collect();
            for &c in t.record.deps.iter().flat_map(|d| std::iter::once(&d.leader).chain(&d.followers)) {
                let seen = key_fields.iter().any(|f| f.cell == c) || follow_fields.iter().any(|f| f.0.cell == c) || t.uniform.iter().any(|u| u.0 == c) || t.uniform_follow.iter().any(|u| u.2 == c);
                anyhow::ensure!(seen, "shape {:#x} outcome {}: the key dependency's cell {c} is neither per-row nor uniform: it would go unchecked", recipe_shape, b.outcome);
            }
            let leaders: Vec<u32> = t.record.deps.iter().map(|d| d.leader).collect();
            let arc = ArcSlots { roots: b.arc.0.map(|s| (off(s), kind(s))), fin: b.arc.1 };
            asm_bodies.push(AsmBody { outcome: b.outcome, splits: b.splits, fields, error_root: b.error as usize, error_off: off(b.error), live_off: off(b.live), dup_refs: Vec::new() });
            reads.push(emit::BodyReads { outcome: b.outcome, shape: t.shape_hash, key_fields, follow_fields, leaders, arc, pos: None });
        }
        for (b, rd) in asm_bodies.iter().zip(reads.iter_mut()) {
            rd.pos = pos_sources(&acc_templates[b.outcome].own, &b.fields)?;
        }
        let dup_pairs = dup_refs(&mut asm_bodies, &reads);
        let plan = emit::Plan::of(&reads);
        let mix_slices = census.map(|c| crate::compiled::mix::counter(&compiled.sym, c));
        let (fused, flat_roots) = graphs.unwrap_or_default();
        Ok(AsmKernel {
            loaded,
            compiled,
            bodies: asm_bodies,
            dup_pairs,
            plan,
            acc_templates,
            forks,
            body_cols: Vec::new(),
            fused_nodes,
            mix_slices,
            fused,
            flat_roots,
            room: Room { cache },
            input_names,
        })
    }
}

/// Every body's `dup_refs`: the earlier bodies of its outcome one fork away.
/// Every root slot is compared (key fields and position sources are
/// fields); differing kinds are no ref.
fn dup_refs(bodies: &mut [AsmBody], reads: &[emit::BodyReads]) -> DupPairs {
    let mut ids = DupPairs::default();
    let n = bodies.len();
    for j in 0..n {
        let mut refs = Vec::new();
        for i in 0..j {
            let (a, b) = (&bodies[i], &bodies[j]);
            let (aa, ab) = (&reads[i].arc, &reads[j].arc);
            if a.outcome != b.outcome || a.splits.len() != b.splits.len() || aa.fin != ab.fin {
                continue;
            }
            if a.splits.iter().zip(&b.splits).filter(|(x, y)| x != y).count() != 1 {
                continue;
            }
            assert_eq!(a.fields.len(), b.fields.len(), "two bodies of one outcome");
            let mut pairs: Vec<(usize, usize, Ty)> = Vec::new();
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
            for ((_, (oa, ka)), (_, (ob, kb))) in aa.read_roots().zip(ab.read_roots()) {
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
                refs.push(DupRef { body: i, pairs: pairs.into_iter().map(|p| ids.id(p)).collect() });
            }
        }
        bodies[j].dup_refs = refs;
    }
    ids
}

/// Where a body's rows get player and room `x`/`y`; `None` (`NO_CELL`)
/// without a numeric player position, as `cells::block_cells`.
fn pos_sources<N: Arithmetic>(template: &Rt2<N>, fields: &[AsmField]) -> Result<Option<[PosSrc; 4]>> {
    let ids = crate::compiled::ids();
    let Some(obj) = template.position_object() else {
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
                Ty::Num | Ty::Ival => Some(PosSrc::Root(f.off)),
                _ => None,
            });
        }
        match template.cols[cell as usize] {
            Col::U(AV::Num(n)) => Ok(Some(PosSrc::Konst(n.whole_i16().ok_or_else(|| anyhow::anyhow!("a constant position {n:?} past the i16 pixels"))?))),
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

/// A follower's dependency and slot in `r`'s layout (`exact::Deps`).
fn follower_of(r: &ShapeRecord, cell: u32) -> Option<(u32, u32)> {
    r.deps.iter().enumerate().find_map(|(g, d)| Some((g as u32, d.followers.binary_search(&cell).ok()? as u32)))
}

/// An arithmetic's resident kernel set and what it was built under.
pub type Resident<N> = Option<(crate::abstraction::Level, crate::settings::Settings, std::sync::Arc<Registry<N>>)>;

/// `level`'s kernel set under settings `s`, built on first use. ONE set an
/// arithmetic is resident (GBs), keyed on the level AND the settings it was
/// built under: another key replaces it; callers hold theirs by `Arc`.
pub(crate) fn registry<N: KernelLanes>(level: crate::abstraction::Level, s: &crate::settings::Settings) -> std::sync::Arc<Registry<N>> {
    let mut set = N::resident().lock().unwrap();
    match &*set {
        Some((l, k, r)) if *l == level && k == s => r.clone(),
        _ => {
            *set = None;
            let built = std::sync::Arc::new(build_registry_for_level::<N>(level, s));
            *set = Some((level, s.clone(), built.clone()));
            built
        }
    }
}

/// THE KERNEL BUILD'S PURGE DELAY: immediate purges cost the build a third
/// of its walk in TLB shootdowns, so purges wait a second while any build runs.
struct BuildPurgeDelay;

/// Builds in progress, and the purge delay they found.
static BUILDS: std::sync::Mutex<(usize, std::os::raw::c_long)> = std::sync::Mutex::new((0, 0));

impl BuildPurgeDelay {
    fn start() -> BuildPurgeDelay {
        let mut b = BUILDS.lock().unwrap();
        if b.0 == 0 {
            // SAFETY: a plain option read, mimalloc's documented API.
            let before = unsafe { libmimalloc_sys::mi_option_get(super::MI_OPTION_PURGE_DELAY) };
            b.1 = before;
            if (0..1000).contains(&before) {
                super::set_purge_delay(1000);
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
            super::set_purge_delay(b.1);
            // Purge what the (exited) builder threads freed under the delay.
            unsafe { libmimalloc_sys::mi_collect(true) };
        }
    }
}

fn build_registry_for_level<N: KernelLanes>(level: crate::abstraction::Level, s: &crate::settings::Settings) -> Registry<N> {
    let _purge = BuildPurgeDelay::start();
    let s = s.clone();
    let built = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(move || Registry::<N>::build_for_start_room(level, &s))
        .expect("spawn asm-kernel builder")
        .join()
        .expect("asm-kernel builder panicked");
    match built {
        Ok(reg) => {
            eprintln!("ASM kernels ENABLED ({level}, {}): {} start-room shapes assembled", N::NAME, reg.len());
            reg
        }
        Err(e) => panic!("building ASM kernels for {level}: {e:#}"),
    }
}
