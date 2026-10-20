//! THE WAVE: one frame of the hull forward (plans/architecture.md "The
//! forward: hulls and storage"): NEW states through the kernels (`unit`),
//! OLD ones re-pushed, region jobs (`route`) overlapping the units, the
//! frame's end. A function of the frame (pushes merge order-free).

use crate::search::hull::{AccOf, HullOf};
use anyhow::{anyhow, ensure, Result};
use std::sync::atomic::{AtomicUsize, Ordering};

use rustc_hash::FxHashMap;

use super::decode::Skeletons;
use super::edges::XferTable;
use super::hulls::Grown;
use super::layer::{bits, Layer};
use super::route::{Chunk, RegionCtx, RouteState, Routes};
use super::unit::UnitSink;
use super::visited::{Key, VisitedSet};
use super::{Sk, GEOMETRY};
use crate::frame::{phases, Block, Filters, FrameStats, FrameStep};
use crate::search::rem::Rem;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::exact::{Code, KeyAdditions, MissKey, Provisional};

/// An OLD frontier state: its hull grown since its last push (`was`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Old<N: Rem = P8> {
    pub(crate) sk: Sk,
    pub(crate) layer: u16,
    pub(crate) hull: HullOf<N>,
    pub(crate) was: HullOf<N>,
}

/// OLD states by layer (ascending; no empty layer), each layer's sorted.
pub(crate) type ByLayer<N = P8> = Vec<OldLayer<N>>;

pub(crate) fn by_layer_states<N: Rem>(by: &ByLayer<N>) -> usize {
    by.iter().map(OldLayer::len).sum()
}

/// The OLD states of one LAYER, sorted: per ENTRY (region, key) its cells
/// and first state, per state its hull and `was` (32 B an entry, 16 a state).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OldLayer<N: Rem = P8> {
    pub(crate) layer: u16,
    entries: Vec<OldEntry>,
    hulls: Vec<[HullOf<N>; 2]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct OldEntry {
    key: Key,
    cells: u64,
    region: u32,
    first: u32,
}

impl<N: Rem> OldLayer<N> {
    pub(crate) fn new(layer: u16, states: usize) -> Self {
        OldLayer { layer, entries: Vec::new(), hulls: Vec::with_capacity(states) }
    }

    /// Append `o` (of this layer, past every state held).
    pub(crate) fn push(&mut self, o: &Old<N>) {
        debug_assert_eq!(o.layer, self.layer, "an old state of another layer");
        let bit = 1u64 << o.sk.local;
        match self.entries.last_mut() {
            Some(e) if (e.region, e.key) == (o.sk.region, o.sk.key) => {
                debug_assert!(e.cells < bit, "an old state before the last");
                e.cells |= bit;
            }
            last => {
                debug_assert!(last.is_none_or(|e| (e.region, e.key) < (o.sk.region, o.sk.key)), "an old state before the last");
                let first = u32::try_from(self.hulls.len()).expect("a layer past u32 old states");
                self.entries.push(OldEntry { key: o.sk.key, cells: bit, region: o.sk.region, first });
            }
        }
        self.hulls.push([o.hull, o.was]);
    }

    pub(crate) fn len(&self) -> usize {
        self.hulls.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.hulls.is_empty()
    }

    pub(crate) fn bytes(&self) -> usize {
        self.entries.capacity() * std::mem::size_of::<OldEntry>() + self.hulls.capacity() * std::mem::size_of::<[HullOf<N>; 2]>()
    }

    fn entry_olds(&self, e: usize) -> impl Iterator<Item = Old<N>> + '_ {
        let x = self.entries[e];
        super::layer::bits(x.cells).enumerate().map(move |(k, c)| {
            let [hull, was] = self.hulls[x.first as usize + k];
            Old { sk: Sk { region: x.region, key: x.key, local: c as u8 }, layer: self.layer, hull, was }
        })
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = Old<N>> + '_ {
        (0..self.entries.len()).flat_map(move |e| self.entry_olds(e))
    }

    /// States `r` (by place), in order.
    pub(crate) fn range(&self, r: std::ops::Range<usize>) -> impl Iterator<Item = Old<N>> + '_ {
        let e = self.entries.partition_point(|x| x.first as usize <= r.start).saturating_sub(1);
        let skip = r.start - self.entries.get(e).map_or(0, |x| x.first as usize);
        (e..self.entries.len()).flat_map(move |e| self.entry_olds(e)).skip(skip).take(r.len())
    }
}

/// A FRONTIER: the states whose hull grew at a frame - the NEW ones (a
/// layer, their hulls in its state order) and the OLD ones (expanded before).
#[derive(Clone)]
pub struct Frontier<N: Rem = P8> {
    pub new: Layer<N>,
    pub hulls: Vec<HullOf<N>>,
    pub old: ByLayer<N>,
}

impl<N: Rem> Default for Frontier<N> {
    fn default() -> Self {
        Frontier { new: Layer::default(), hulls: Vec::new(), old: Vec::new() }
    }
}

impl<N: Rem> Frontier<N> {
    pub(crate) fn is_empty(&self) -> bool {
        self.new.is_empty() && self.old.is_empty()
    }

    pub fn old_states(&self) -> usize {
        by_layer_states(&self.old)
    }

}

pub struct Wave<N: Rem = P8> {
    /// The NEW states (with their skeletons), their hulls in its state order.
    pub next: Layer<N>,
    pub hulls: Vec<HullOf<N>>,
    /// The states expanded before whose hull GREW.
    pub grown: ByLayer<N>,
    /// The new states' wins: per entry of `next` (ascending) its win cells.
    pub(crate) wins: Vec<(u32, u64)>,
    pub stats: FrameStats,
    /// The shapes the wave numbered and what the key space gained.
    pub(crate) shapes: Vec<(u32, u64)>,
    pub(crate) keys: KeyAdditions<N>,
}

impl<N: Rem> Wave<N> {
    pub(crate) fn frontier(self) -> Frontier<N> {
        Frontier { new: self.next, hulls: self.hulls, old: self.grown }
    }

    /// The next frontier's states, layers, hulls: the new, then the grown.
    pub fn frontier_states(&self, frame: u32) -> impl Iterator<Item = Grown<N>> + '_ {
        let layer = u16::try_from(frame).expect("a frame past u16");
        self.next.states_iter().zip(&self.hulls).map(move |(s, &h)| (s, layer, h)).chain(self.grown.iter().flat_map(OldLayer::<N>::iter).map(|o| (o.sk, o.layer, o.hull)))
    }
}

/// Lanes per unit (one kernel call): a 256th of the frame, in 1024..=4096
/// (less padding; enough units to balance). A function of the frame, not of
/// the threads: the units cut the bundles.
fn unit_lanes(lanes: u64) -> usize {
    ((lanes / 256) as usize).clamp(1024, 4096) & !63
}

/// A KERNEL unit: whole entries of one shape of the new layer (at most
/// `unit_lanes` + 63 states) and its first state's place; a RE-PUSH unit:
/// whole entries of the `layer`th list of old states.
enum UnitSpec {
    Kernel { entries: std::ops::Range<usize>, state: usize, lanes: u32 },
    Repush { layer: usize, entries: std::ops::Range<usize>, states: usize },
}

impl UnitSpec {
    /// The states it runs (its weight in a slice).
    fn lanes(&self) -> usize {
        match self {
            UnitSpec::Kernel { lanes, .. } => *lanes as usize,
            UnitSpec::Repush { states, .. } => *states,
        }
    }
}

/// Units of about `unit` lanes, never across a shape (entries set-wide).
fn units_of<N: Rem>(layer: &Layer<N>, unit: usize) -> Vec<UnitSpec> {
    let slots = GEOMETRY.slots;
    let mut out = Vec::new();
    let mut cur: Option<(u32, usize, usize, u32)> = None;
    let (mut e, mut st) = (0usize, 0usize);
    for (region, t) in layer.set.tables() {
        let shape = region / slots;
        for m in t.masks() {
            let n = m.count_ones();
            cur = match cur {
                Some((sh, a, s0, l)) if sh == shape && (l as usize) < unit => Some((sh, a, s0, l + n)),
                Some((_, a, s0, l)) => {
                    out.push(UnitSpec::Kernel { entries: a..e, state: s0, lanes: l });
                    Some((shape, e, st, n))
                }
                None => Some((shape, e, st, n)),
            };
            e += 1;
            st += n as usize;
        }
    }
    if let Some((_, a, s0, l)) = cur {
        out.push(UnitSpec::Kernel { entries: a..e, state: s0, lanes: l });
    }
    out
}

/// States per RE-PUSH unit (whole entries: up to 63 more); it runs no kernel.
const REPUSH_UNIT: usize = 1 << 14;

fn repush_units_of<N: Rem>(by: &ByLayer<N>) -> Vec<UnitSpec> {
    let mut out = Vec::new();
    for (layer, old) in by.iter().enumerate() {
        let n = old.entries.len();
        let first = |e: usize| old.entries.get(e).map_or(old.len(), |x| x.first as usize);
        let mut a = 0usize;
        while a < n {
            let mut b = a + 1;
            while b < n && first(b) - first(a) < REPUSH_UNIT {
                b += 1;
            }
            out.push(UnitSpec::Repush { layer, entries: a..b, states: first(b) - first(a) });
            a = b;
        }
    }
    out
}

/// Input lanes per SLICE (slices cut the segments): units run at most two
/// slices past the gathered. 256k against 1M: a quarter of the records
/// held, the same wall.
const SLICE_LANES: usize = 1 << 18;

fn slices_of(units: &[UnitSpec]) -> Vec<std::ops::Range<usize>> {
    let mut out = Vec::new();
    let (mut a, mut lanes) = (0usize, 0usize);
    for (k, u) in units.iter().enumerate() {
        lanes += u.lanes();
        if lanes >= SLICE_LANES {
            out.push(a..k + 1);
            (a, lanes) = (k + 1, 0);
        }
    }
    if a < units.len() {
        out.push(a..units.len());
    }
    out
}

/// Stack per frame worker: kernel spill frames reach ~83 MB (virtual; each
/// call checks it fits, `asm_kernel::set_thread_stack`).
const WORKER_STACK: usize = 512 << 20;

/// The share of `workers x wall` spent waiting at the barrier.
fn idle_fraction(wall: std::time::Duration, workers: usize, busy: std::time::Duration) -> f64 {
    if workers == 0 || wall.is_zero() {
        return 0.0;
    }
    (1.0 - busy.as_secs_f64() / (workers as f64 * wall.as_secs_f64())).max(0.0)
}

pub(crate) struct WaveCtx<'a, N: Rem = P8> {
    pub(crate) visited: &'a mut VisitedSet<N>,
    pub(crate) xfers: &'a mut XferTable<N>,
    pub(crate) filters: Filters<'a, N>,
    pub(crate) frame: u32,
    /// The level's `edges/` dir: edges are recorded only with one.
    pub(crate) edges_dir: Option<&'a std::path::Path>,
    /// The tree's out-edges (`None`: a frontier without old states).
    pub(crate) outs: Option<&'a super::out::OutStore>,
    /// The forward's skeletons, registered by the emissions: the next layer's.
    pub(crate) skeletons: &'a Skeletons<N>,
    /// The tree's ceiling (its level -1 horizon), for `frame::not_expanded`.
    pub(crate) ceiling: Option<u32>,
}

#[derive(Default)]
struct UnitScratch {
    rows: Vec<(Key, u32)>,
    cells: Vec<u32>,
    live: Vec<usize>,
}

/// ONE KERNEL UNIT: its states DECODED, `frame::not_expanded` (decided here,
/// fresh or resumed) left out, the rest through the engine with their hulls.
#[allow(clippy::too_many_arguments)]
fn run_unit<N: Rem>(engine: &dyn FrameStep<N>, u: u32, entries: std::ops::Range<usize>, state: usize, layer: &Layer<N>, hulls: &[HullOf<N>], starts: &[usize], shapes: &[u64], sink: &mut UnitSink<'_, N>, sc: &mut UnitScratch, layer_frame: u32, ceiling: Option<u32>, s: &crate::settings::Settings) -> Result<()> {
    let geo = GEOMETRY;
    let t_dec = phases::start(s.diag.phases);
    sink.begin(u);
    sc.rows.clear();
    sc.cells.clear();
    let mut state = state;
    let tables = layer.set.tables();
    let (mut ti, mut local) = layer.set.locate(starts, entries.start)?;
    let mut region = tables[ti].0;
    for _ in entries {
        while local as usize >= tables[ti].1.len() {
            (ti, local) = (ti + 1, 0);
            region = tables[ti].0;
        }
        let t = &tables[ti].1;
        let (key, mask) = (t.key(local), t.mask(local));
        local += 1;
        let si = sink.push_source(region, key);
        for local in bits(mask) {
            let cell = geo.cell(region % geo.slots, local);
            sc.rows.push((key, cell));
            sc.cells.push(cell);
            sink.push_lane(si, cell, *hulls.get(state).ok_or_else(|| anyhow!("unit {u}: a state past the layer's hulls"))?);
            state += 1;
        }
    }
    sink.sources_done();
    let shape = *shapes.get((region / geo.slots) as usize).ok_or_else(|| anyhow!("a frontier region of no shape"))?;
    let skel = layer.skeletons.get(&shape).ok_or_else(|| anyhow!("ROWS FROM KEYS: the frontier holds no skeleton of shape {shape:016x}"))?;
    let rt2 = super::decode::decode_rows(sink.keys(), skel, &sc.rows)?;
    let block = Block::from_rt2(rt2);
    let skip = crate::frame::not_expanded(&block, layer_frame, ceiling, s)?;
    sc.live.clear();
    sc.live.extend((0..block.lanes()).filter(|&l| !skip[l]));
    phases::add(phases::DECODE, t_dec);
    // A unit of exits only (the frame after a win) runs no kernel: their
    // shapes have none.
    if !sc.live.is_empty() {
        let t_unit = phases::start(s.diag.phases);
        engine.run(&block, &sc.cells, &sc.live, sink)?;
        phases::add(phases::UNIT, t_unit);
    }
    sink.end()
}

/// ONE FORWARD FRAME: frame `cx.frame` from `front` (grown the frame before).
pub(crate) fn run_wave<N: Rem>(engine: &dyn FrameStep<N>, front: Frontier<N>, cx: WaveCtx<N>) -> Result<Wave<N>> {
    use std::time::Instant;
    let WaveCtx { visited, xfers, filters, frame, edges_dir, outs, skeletons, ceiling } = cx;
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let (level, s) = (engine.level(), &engine.settings());
    let game_frame = s.clock().frame_of(frame);
    ensure!(
        !level.platforms || game_frame as usize <= crate::trace::walk::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::walk::PLATFORM_WORLD_FRAMES
    );
    ensure!(frame > 0, "a wave makes frame 1 on");
    ensure!(front.hulls.len() as u64 == front.new.states(), "frame {frame}: {} hulls for the frontier's {} new states", front.hulls.len(), front.new.states());
    ensure!(outs.is_some() || front.old.is_empty(), "frame {frame}: old states to re-push without the tree's out-edges");
    let t_warm = Instant::now();
    engine.warm();
    if let Some(m) = filters.minus_one {
        let _ = m.table(s);
    }
    if t_warm.elapsed().as_secs_f64() > 0.5 {
        // Not `[fwd] f...`: log readers (`export-ui`) parse those as frames.
        eprintln!("[warm] f{frame}: engine and level -1 table built in {:.1} s, before the wave", t_warm.elapsed().as_secs_f64());
    }
    // What the last frame's end and checkpoint freed back to the OS before
    // the wave's threads (new heaps) allocate: kept, it adds to the peak.
    collect_freed();
    let mut st = FrameStats::default();
    let workers = s.threads();
    let t_frame = Instant::now();
    let layer = &front.new;
    st.entries_in = layer.entries();
    st.lanes_in = layer.states() as usize;
    st.old_in = front.old_states();
    st.frontier_bytes = layer.alloc_bytes() + std::mem::size_of_val(front.hulls.as_slice()) + front.old.iter().map(OldLayer::bytes).sum::<usize>();
    st.rss_start = crate::metrics::current_rss_gb();
    let record = edges_dir.is_some();
    // The out blocks and segments wait in the SPOOL, not in memory.
    let spool: Option<std::sync::Arc<super::spool::Spool>> = match edges_dir {
        Some(dir) => {
            std::fs::create_dir_all(dir)?;
            Some(std::sync::Arc::new(super::spool::Spool::create(&dir.join(format!("f{frame:03}.spool")))?))
        }
        None => None,
    };
    let mut units = units_of(layer, unit_lanes(st.lanes_in as u64));
    let n_kernel = units.len();
    units.extend(repush_units_of(&front.old));
    let starts: Vec<usize> = layer.set.entry_starts();
    let slices = slices_of(&units);
    let routes = Routes::default();
    let prov = Provisional::default();
    // The visited set and the tree's transfer table are read only until the
    // frame's end; the gather numbers the workers' new pairs (`Gather::fresh`).
    let xtabs: Vec<std::sync::RwLock<Vec<N::Pair>>> = (0..workers).map(|_| Default::default()).collect();
    let xfers_before = xfers.pairs.len();
    let base: &XferTable<N> = xfers;
    let mut sinks: Vec<UnitSink<N>> = (0..workers).map(|w| UnitSink::new(visited, &routes, skeletons, &prov, filters, frame, level, w as u32, base, &xtabs[w], record, s)).collect();
    // Per kernel unit its out block (raw while a target key is provisional).
    let mut out_blocks: Vec<std::sync::Mutex<Vec<OutBlock>>> = (0..n_kernel).map(|_| Default::default()).collect();
    let repushed = std::sync::atomic::AtomicU64::new(0);
    let mut scratch: Vec<super::route::Scratch> = (0..workers).map(|_| super::route::Scratch::default()).collect();
    let mut slice_of = vec![0u32; units.len()];
    for (k, r) in slices.iter().enumerate() {
        slice_of[r.clone()].fill(k as u32);
    }
    let pipe: Pipe<N> = Pipe {
        xtabs: &xtabs,
        next_unit: AtomicUsize::new(0),
        left: slices.iter().map(|r| AtomicUsize::new(r.len())).collect(),
        upto: slices.iter().map(|_| (0..workers).map(|_| AtomicUsize::new(0)).collect()).collect(),
        chunks: slices.iter().map(|_| Default::default()).collect(),
        gathered: AtomicUsize::new(0),
        gather: std::sync::Mutex::new(Gather { fresh: XferTable::default(), first: xfers_before as u32, merged: vec![0; workers], remaps: vec![Vec::new(); workers], routes: 0 }),
        states: Default::default(),
        jobs: Default::default(),
        jobs_left: AtomicUsize::new(0),
        failed: std::sync::atomic::AtomicBool::new(false),
        inflight: AtomicUsize::new(0),
        inflight_max: AtomicUsize::new(0),
    };
    let shapes: Vec<u64> = visited.shapes().to_vec();
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} start"));
    }
    let t_pipeline = Instant::now();
    // THE PIPELINE: a region job first (the oldest slice's, heaviest first),
    // else a unit of the two slices past the gathered; a slice's last unit's
    // worker GATHERS (`Pipe::gather_ready`). No barrier but the frame's end.
    let spent: Vec<(std::time::Duration, std::time::Duration, [u64; 4])> = std::thread::scope(|scope| {
        let handles: Vec<_> = sinks
            .iter_mut()
            .zip(scratch.iter_mut())
            .map(|(sink, sc)| {
                let (units, pipe, slice_of, slices, routes, shapes, starts, hulls, old_by, out_blocks, repushed, spool) = (&units, &pipe, &slice_of, &slices, &routes, &shapes, &starts, &front.hulls, &front.old, &out_blocks, &repushed, spool.as_deref());
                let visited: &VisitedSet<N> = visited;
                std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<(std::time::Duration, std::time::Duration, [u64; 4])> {
                    crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
                    // Any way out of this worker but the end of the frame -
                    // an error returned, a panic - stops the others.
                    struct Stop<'p>(&'p std::sync::atomic::AtomicBool, bool);
                    impl Drop for Stop<'_> {
                        fn drop(&mut self) {
                            if !self.1 {
                                self.0.store(true, Ordering::Release);
                            }
                        }
                    }
                    let mut stop = Stop(&pipe.failed, false);
                    let (mut busy, mut in_regions) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
                    let mut idle_spins = 0u32;
                    let mut us = UnitScratch::default();
                    let mut rs = RepushScratch::default();
                    let mut z: Option<zstd::bulk::Compressor<'static>> = None;
                    loop {
                        if pipe.failed.load(Ordering::Acquire) {
                            stop.1 = true;
                            return Err(anyhow!("frame {frame}: another wave worker failed"));
                        }
                        if let Some(job) = pipe.pop_job() {
                            let t = Instant::now();
                            let tp = phases::start(s.diag.phases);
                            let rcx = RegionCtx { frame, record, remaps: &job.remaps, new_xfers: xfers_before as u32, spool };
                            let cs: Vec<&Chunk<N>> = job.chunks.iter().collect();
                            let mut route = job.state.lock().expect("a route");
                            let r = super::route::run_route(&mut route, job.slice, &cs, &rcx, sc);
                            drop(route);
                            if s.diag.phases {
                                pipe.inflight.fetch_sub(job.chunks.iter().map(Chunk::alloc_bytes).sum(), Ordering::AcqRel);
                            }
                            drop(cs);
                            drop(job);
                            pipe.jobs_left.fetch_sub(1, Ordering::AcqRel);
                            r?;
                            phases::add(phases::REGIONS, tp);
                            busy += t.elapsed();
                            in_regions += t.elapsed();
                            idle_spins = 0;
                            continue;
                        }
                        let u = pipe.next_unit.load(Ordering::Acquire);
                        if u < units.len() {
                            if slice_of[u] as usize > pipe.gathered.load(Ordering::Acquire) + 1 || pipe.next_unit.compare_exchange(u, u + 1, Ordering::AcqRel, Ordering::Acquire).is_err() {
                                pipe.wait(&mut idle_spins);
                                continue;
                            }
                            let t = Instant::now();
                            match &units[u] {
                                UnitSpec::Kernel { entries, state, .. } => {
                                    run_unit(engine, u as u32, entries.clone(), *state, layer, hulls, starts, shapes, sink, &mut us, frame - 1, ceiling, s)?;
                                    let to = phases::start(s.diag.phases);
                                    let mut v = Vec::new();
                                    for (raw, prov) in std::mem::take(&mut sink.out_blocks) {
                                        v.push(if prov {
                                            OutBlock::Raw(raw)
                                        } else {
                                            OutBlock::Sealed(super::out::Sealed::of::<N>(&raw, z.get_or_insert_with(|| zstd::bulk::Compressor::new(1).expect("a zstd compressor")))?.map(|x| x.spill(spool)).transpose()?)
                                        });
                                    }
                                    *out_blocks[u].lock().expect("an out block") = v;
                                    phases::add(phases::OUT_BLOCKS, to);
                                }
                                UnitSpec::Repush { layer, entries, .. } => {
                                    let tr = phases::start(s.diag.phases);
                                    let n = run_repush(u as u32, &old_by[*layer], entries.clone(), outs.expect("checked above"), sink, &mut rs)?;
                                    repushed.fetch_add(n, Ordering::Relaxed);
                                    phases::add(phases::REPUSH, tr);
                                }
                            }
                            let k = slice_of[u] as usize;
                            pipe.upto[k][sink.worker as usize].fetch_max(sink.xfers_met(), Ordering::AcqRel);
                            if s.diag.phases {
                                let b: usize = sink.outbox.iter().map(Chunk::alloc_bytes).sum();
                                let now = pipe.inflight.fetch_add(b, Ordering::AcqRel) + b;
                                pipe.inflight_max.fetch_max(now, Ordering::AcqRel);
                            }
                            pipe.chunks[k].lock().expect("a slice's chunks").append(&mut sink.outbox);
                            busy += t.elapsed();
                            idle_spins = 0;
                            if pipe.left[k].fetch_sub(1, Ordering::AcqRel) == 1 {
                                let t = Instant::now();
                                let tg = phases::start(s.diag.phases);
                                pipe.gather_ready(slices.len(), routes, visited)?;
                                phases::add(phases::GATHER, tg);
                                busy += t.elapsed();
                            }
                            continue;
                        }
                        if pipe.gathered.load(Ordering::Acquire) == slices.len() && pipe.jobs_left.load(Ordering::Acquire) == 0 {
                            break;
                        }
                        pipe.wait(&mut idle_spins);
                    }
                    crate::compiled::dispatch::fold_hits();
                    stop.1 = true;
                    Ok((busy, in_regions, rs.read))
                })
                .expect("spawn wave worker")
            })
            .collect();
        // The first failure's error (a worker that only stopped for it says so).
        let mut out = Vec::new();
        let mut first: Option<anyhow::Error> = None;
        for h in handles {
            match h.join().expect("wave worker panicked") {
                Ok(x) => out.push(x),
                Err(e) if first.is_none() || first.as_ref().is_some_and(|f| f.to_string().contains("another wave worker failed")) => first = Some(e),
                Err(_) => {}
            }
        }
        match first {
            Some(e) => Err(e),
            None => Ok(out),
        }
    })?;
    let t_units = t_pipeline.elapsed();
    collect_freed();
    if s.diag.phases {
        // The wave's anonymous peak (sampled) apart from the frame end's.
        crate::metrics::mem_phase(&format!("f{frame} units and region jobs"));
    }
    st.repush_edges = repushed.into_inner();
    let (had_old, n_units) = (!front.old.is_empty(), units.len());
    // The frontier is spent: dropped before the frame's end builds the next.
    drop(units);
    drop(starts);
    drop(front);
    if s.diag.phases && had_old {
        let r = spent.iter().fold([0u64; 4], |a, w| std::array::from_fn(|k| a[k] + w.2[k]));
        eprintln!(
            "[repush] f{frame}: {} old states in {} units read {} out blocks ({:.1} MB raw), {} entries in them, {} read ({:.1}%), {} edges looked at",
            st.old_in,
            n_units - n_kernel,
            r[0],
            r[1] as f64 / 1e6,
            r[2],
            r[3],
            100.0 * r[3] as f64 / r[2].max(1) as f64,
            st.repush_edges
        );
    }
    let busy: std::time::Duration = spent.iter().map(|w| w.0).sum();
    st.t_regions = spent.iter().map(|w| w.1).sum();
    st.t_wave = t_units;
    st.wave_idle = idle_fraction(t_units, workers, busy);
    st.slices = slices.len();
    let Pipe { gather, states, inflight_max, .. } = pipe;
    let Gather { fresh, remaps, .. } = gather.into_inner().expect("the gather");
    let mut states: Vec<RouteState<N>> = states
        .into_inner()
        .expect("the routes")
        .into_iter()
        .map(|a| std::sync::Arc::try_unwrap(a).map_err(|_| anyhow::anyhow!("a route still shared")).and_then(|m| m.into_inner().map_err(|_| anyhow::anyhow!("a route lock poisoned"))))
        .collect::<Result<_>>()?;
    st.rss_wave = crate::metrics::current_rss_gb();
    if s.diag.phases {
        let gb = |b: usize| b as f64 / 1e9;
        let rest = states.iter().fold([0usize; 3], |a, r| {
            let b = r.alloc_bytes();
            [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
        });
        let sc = scratch.iter().fold([0usize; 4], |a, k| {
            let b = k.alloc_bytes();
            [a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]]
        });
        let sk = sinks.iter().fold([0usize; 4], |a, k| {
            let b = k.alloc_bytes();
            std::array::from_fn(|i| a[i] + b[i])
        });
        let (x_all, x_max) = sinks.iter().fold((0usize, 0usize), |a, k| (a.0 + k.xfers_met(), a.1.max(k.xfers_met())));
        let out_held: usize = out_blocks
            .iter()
            .map(|b| {
                b.lock()
                    .expect("an out block")
                    .iter()
                    .map(|x| match x {
                        OutBlock::Sealed(z) => z.as_ref().map_or(0, |z| z.z.held()),
                        OutBlock::Raw(r) => r.capacity(),
                    })
                    .sum::<usize>()
            })
            .sum();
        let tb = states.iter().fold([0usize; 4], |a, r| {
            let b = r.touched.bytes();
            std::array::from_fn(|k| a[k] + b[k])
        });
        let (tk, ts): (usize, usize) = states.iter().fold((0, 0), |a, r| (a.0 + r.touched.key_count(), a.1 + r.touched.len()));
        eprintln!(
            "[wavemem] f{frame}: touched {ts} states in {tk} keys: index {:.3}, keys {:.3}, live slots {:.3}, dead or spare slots {:.3} GB ({:.1} B a touched state)",
            gb(tb[0]),
            gb(tb[1]),
            gb(tb[2]),
            gb(tb[3]),
            tb.iter().sum::<usize>() as f64 / ts.max(1) as f64
        );
        eprintln!(
            "[wavemem] f{frame}: frontier {:.3} GB ({} entries, {} old states), visited {:.3} GB ({} entries), transfers {:.3}, the out store's index {:.3}, the routes' touched states {:.3}, provisional bundles {:.3}, segments {:.3}, out blocks {:.3}, spooled {:.3}, chunks in flight at most {:.3}, sinks {:.3} (bundles {:.3}, lane hulls {:.3}, transfers {:.3}: {x_all} met, at most {x_max} a worker, rest {:.3}), region scratch {:.3} (bundles {:.3}, encoder {:.3}, raw {:.3}, rest {:.3}) GB; anon {:.2} GB",
            gb(st.frontier_bytes),
            st.entries_in,
            st.old_in,
            gb(visited.alloc_bytes()),
            visited.entries(),
            gb(base.bytes().iter().sum::<usize>() + fresh.bytes().iter().sum::<usize>()),
            gb(outs.map_or(0, |o| o.index_bytes())),
            gb(rest[0]),
            gb(rest[1]),
            gb(rest[2]),
            gb(out_held),
            gb(spool.as_ref().map_or(0, |sp| sp.bytes() as usize)),
            gb(inflight_max.load(Ordering::Acquire)),
            gb(sk.iter().sum()),
            gb(sk[0]),
            gb(sk[1]),
            gb(sk[2]),
            gb(sk[3]),
            gb(sc.iter().sum()),
            gb(sc[0]),
            gb(sc[1]),
            gb(sc[2]),
            gb(sc[3]),
            crate::metrics::current_rss_gb()
        );
    }
    let mut xfer_counts = vec![0u64; fresh.pairs.len()];
    for (sink, remap) in sinks.iter().zip(&remaps) {
        for (&n, &g) in sink.xfer_counts().iter().zip(remap) {
            if g as usize >= xfers_before {
                xfer_counts[g as usize - xfers_before] += n;
            }
        }
    }
    for sink in &mut sinks {
        sink.finish();
        st.lanes_raw += sink.emitted as usize;
    }
    st.units = n_units;
    drop(sinks);
    xfers.append(fresh);
    // The exited workers' buffers back before the frame's end allocates.
    collect_freed();

    let t = Instant::now();
    let tl = phases::start(s.diag.phases);
    let mut adds = KeyAdditions::default();
    let mut lap = Instant::now();
    let mut at = super::bench::step_counts();
    let mut mark = |what: &'static str, st: &mut FrameStats| {
        let k = crate::frame::END_STEPS.iter().position(|&w| w == what).expect("a frame end step");
        st.t_end[k] += lap.elapsed();
        let (d, now) = super::bench::counts_since(at);
        at = now;
        for (a, b) in st.end_counts[k].iter_mut().zip(d) {
            *a += b;
        }
        lap = Instant::now();
    };
    let finals = std::sync::Arc::new(finalize_keys(visited, &states, &prov, &mut adds));
    mark("keys", &mut st);
    // THE OUT FILE beside the frame's end (provisional blocks with final keys).
    let out_file = match edges_dir {
        Some(dir) => {
            let (path, finals, threads, out_blocks, spool) = (super::out::path(dir, frame), finals.clone(), s.threads(), std::mem::take(&mut out_blocks), spool.clone());
            Some(std::thread::Builder::new().name("out file".into()).spawn(move || -> Result<u64> {
                let blocks = par_map(&out_blocks, threads, |b| -> Result<Vec<super::out::Sealed>> {
                    let mut out = Vec::new();
                    for x in std::mem::take(&mut *b.lock().expect("an out block")) {
                        match x {
                            OutBlock::Sealed(x) => out.extend(x),
                            OutBlock::Raw(raw) => {
                                let mut fixed = Vec::new();
                                super::out::fixup::<N>(&raw, &finals, &mut fixed)?;
                                out.extend(super::out::Sealed::of::<N>(&fixed, &mut zstd::bulk::Compressor::new(1)?)?);
                            }
                        }
                    }
                    Ok(out)
                });
                let mut all: Vec<super::out::Sealed> = Vec::new();
                for b in blocks {
                    all.extend(b?);
                }
                super::out::write(&path, frame, &all, spool.as_deref())
            })?)
        }
        None => None,
    };
    drop(prov);
    (st.key_bits, st.key_codes, st.new_codes) = (visited.keys.max_bits(), visited.keys.codes(), adds.codes.len());
    // New shapes numbered (in hash order): a route's with touched states or
    // edges (a target nothing pushed into still has its edges recorded).
    let mut new_shapes: Vec<u64> = states.iter().filter(|r| r.is_used()).map(|r| r.shape).filter(|&h| visited.shape_index(h).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    let mut shapes_numbered = Vec::new();
    for h in new_shapes {
        let i = visited.add_shape(h);
        shapes_numbered.push((i, h));
    }
    let regions: Vec<Option<u32>> = states.iter().map(|r| visited.region(r.shape, r.slot)).collect();
    st.pushes = states.iter().map(|r| r.pushes).sum();
    st.touched = states.iter().map(|r| r.touched.len() as u64).sum();
    // The frame's new transfers in canonical order (by their edges, then
    // content), whatever the slices that met them first.
    let perm = xfers.canonical(xfers_before, &xfer_counts);

    type Sealed<N> = super::route::Sealed<N>;
    let sealed: Vec<Result<Option<Sealed<N>>>> = {
        // SEALED: touched keys sorted (a route a thread, biggest first),
        // compared in parts (a big route's on every thread), put together.
        let cells: Vec<std::sync::Mutex<(usize, &mut RouteState<N>)>> = states.iter_mut().enumerate().map(std::sync::Mutex::new).collect();
        let mut order: Vec<&std::sync::Mutex<(usize, &mut RouteState<N>)>> = cells.iter().collect();
        order.sort_by_cached_key(|c| std::cmp::Reverse(c.lock().expect("a route").1.touched.len()));
        let touches = par_map(&order, workers, |c| -> Result<(usize, Option<super::route::Touch<N>>)> {
            let mut g = c.lock().expect("a route");
            let k = g.0;
            match regions[k] {
                Some(region) => Ok((k, Some(super::route::seal_sort(&mut g.1, region, &finals)?))),
                // A new shape's route whose records the coarser level dropped.
                None if !g.1.is_used() => Ok((k, None)),
                None => Err(anyhow::anyhow!("frame {frame}: a used route of an unnumbered shape")),
            }
        });
        let mut touches: Vec<(usize, Option<super::route::Touch<N>>)> = touches.into_iter().collect::<Result<_>>()?;
        touches.sort_by_key(|x| x.0);
        // The parts by route, biggest first: a route's last part puts it
        // together, so its touched keys and parts go as soon as it is sealed.
        let mut by_size: Vec<usize> = (0..touches.len()).filter(|&i| touches[i].1.is_some()).collect();
        by_size.sort_by_key(|&i| std::cmp::Reverse(touches[i].1.as_ref().map_or(0, |t| t.touched.len())));
        let mut parts: Vec<(usize, std::ops::Range<usize>)> = Vec::new();
        let mut left: Vec<AtomicUsize> = (0..touches.len()).map(|_| AtomicUsize::new(0)).collect();
        for i in by_size {
            let n = touches[i].1.as_ref().map_or(0, |t| t.touched.len());
            let before = parts.len();
            parts.extend((0..n.max(1)).step_by(super::route::SEAL_PART).map(|a| (i, a..(a + super::route::SEAL_PART).min(n))));
            *left[i].get_mut() = parts.len() - before;
        }
        let touches: Vec<std::sync::RwLock<Option<super::route::Touch<N>>>> = touches.into_iter().map(|(_, t)| std::sync::RwLock::new(t)).collect();
        let got: Vec<std::sync::Mutex<Vec<(usize, super::route::SealPart<N>)>>> = (0..touches.len()).map(|_| Default::default()).collect();
        let out: Vec<std::sync::Mutex<Option<Sealed<N>>>> = (0..touches.len()).map(|_| Default::default()).collect();
        let visited: &VisitedSet<N> = visited;
        let (left, touches_ref, got_ref, out_ref) = (&left, &touches, &got, &out);
        for r in par_map(&parts, workers, |(i, rg)| -> Result<()> {
            let p = {
                let t = touches_ref[*i].read().expect("a route's touched keys");
                super::route::seal_part(t.as_ref().expect("a part of a route with touched keys"), rg.clone(), frame, visited)?
            };
            got_ref[*i].lock().expect("a route's parts").push((rg.start, p));
            if left[*i].fetch_sub(1, Ordering::AcqRel) == 1 {
                let t = touches_ref[*i].write().expect("a route's touched keys").take().expect("sealed once");
                let mut p = std::mem::take(&mut *got_ref[*i].lock().expect("a route's parts"));
                p.sort_by_key(|x| x.0);
                *out_ref[*i].lock().expect("a sealed route") = Some(super::route::sealed(t, p.into_iter().map(|x| x.1).collect())?);
            }
            Ok(())
        }) {
            r?;
        }
        out.into_iter().map(|o| Ok(o.into_inner().expect("a sealed route"))).collect::<Vec<Result<Option<Sealed<N>>>>>()
    };
    // The routes' touched tables, freed off their (exited) threads: back now.
    collect_freed();
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the routes sealed (keys final, new and grown states)"));
    }
    let mut new: Vec<Sealed<N>> = Vec::new();
    for x in sealed {
        if let Some(sl) = x? {
            new.push(sl);
        }
    }
    new.sort_unstable_by_key(|x| x.region);
    mark("seal", &mut st);
    // THE EDGE FILE beside the frame's end: per route the jobs' segments
    // (final once sealed), then the waited ones (`route::reencode`).
    type RouteSegs = (u32, Vec<((u8, u32), super::edges::Segment)>, Vec<(u32, super::edges::Segment)>);
    let mut routes_segs: Vec<RouteSegs> = Vec::new();
    let mut src_index: Vec<(u32, Vec<u32>)> = Vec::new();
    for (r, region) in states.into_iter().zip(&regions) {
        let Some(region) = *region else { continue };
        st.edge_bundles += r.segs.iter().map(|g| g.1.bundles as u64).sum::<u64>() + r.prov.iter().map(|g| g.1.bundles as u64).sum::<u64>();
        st.edge_records += r.segs.iter().map(|g| g.1.edges).sum::<u64>() + r.prov.iter().map(|g| g.1.edges).sum::<u64>();
        st.routes += 1;
        if !r.sources.is_empty() {
            src_index.push((region, r.sources.into_iter().collect()));
        }
        routes_segs.push((region, r.segs, r.prov));
    }
    let edge_file = match edges_dir {
        Some(dir) => {
            let (dir, flush, finals, threads, spool) = (dir.to_path_buf(), xfers.take_flush(), finals.clone(), s.threads(), spool.clone());
            Some(std::thread::Builder::new().name("edge file".into()).spawn(move || -> Result<u64> {
                flush.write(&dir)?;
                let renumber = perm.as_deref().map(|perm| super::route::Renumber { first: xfers_before as u32, perm });
                // The waited segments, one job each, on every thread.
                let waited: Vec<(usize, u32, &super::edges::Segment)> = routes_segs.iter().enumerate().flat_map(|(k, r)| r.2.iter().map(move |(slice, seg)| (k, *slice, seg))).collect();
                let again = par_map(&waited, threads, |&(k, _, seg)| super::route::reencode(seg, routes_segs[k].0, &finals, renumber.as_ref(), &mut super::bundle::Encoder::default(), &mut zstd::bulk::Compressor::new(1)?));
                let mut per: Vec<Vec<((u8, u32), super::edges::Segment)>> = (0..routes_segs.len()).map(|_| Vec::new()).collect();
                for (&(k, slice, _), seg) in waited.iter().zip(again) {
                    per[k].push(((1, slice), seg?));
                }
                drop(waited);
                let mut segs = Vec::new();
                for ((_, mut jobs, _), waited) in routes_segs.into_iter().zip(per) {
                    jobs.extend(waited);
                    jobs.sort_unstable_by_key(|g| g.0);
                    segs.extend(jobs.into_iter().map(|g| g.1));
                }
                super::edges::write_file(&super::edges::file_path(&dir, frame), frame, segs, src_index, spool.as_deref())
            })?)
        }
        None => None,
    };
    if s.diag.phases && !new.is_empty() {
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        let sum = new.iter().fold([0.0f64; 3], |a, sl| std::array::from_fn(|k| a[k] + ms(sl.t[k])));
        let mut big: Vec<&Sealed<N>> = new.iter().collect();
        big.sort_by(|a, b| b.t.iter().sum::<std::time::Duration>().cmp(&a.t.iter().sum()));
        let parts = |t: &[f64; 3]| format!("sort {:.0}, compare {:.0}, table {:.0} ms", t[0], t[1], t[2]);
        let ages = new.iter().fold([[0u64; 7]; 2], |a, sl| std::array::from_fn(|i| std::array::from_fn(|k| a[i][k] + sl.held_by_age[i][k])));
        eprintln!("[seal] f{frame}: held states compared: {} full, by age (frame less layer) 1: {}, 2-3: {}, 4-7: {}, 8-15: {}, 16-31: {}, 32+: {}; of them grown: {}, {}, {}, {}, {}, {}", ages[0][0], ages[0][1], ages[0][2], ages[0][3], ages[0][4], ages[0][5], ages[0][6], ages[1][1], ages[1][2], ages[1][3], ages[1][4], ages[1][5], ages[1][6]);
        eprintln!(
            "[seal] f{frame}: {} routes, {} waiting bundles in {} segments, worker time {}; the longest: {}",
            new.len(),
            new.iter().map(|sl| sl.waited.1).sum::<u64>(),
            new.iter().map(|sl| sl.waited.0).sum::<usize>(),
            parts(&sum),
            big.iter().take(3).map(|sl| format!("r{} ({} states, {} waiting bundles in {} segments: {})", sl.region, sl.touched, sl.waited.1, sl.waited.0, parts(&sl.t.map(ms)))).collect::<Vec<_>>().join("; ")
        );
    }
    if let Some(f) = filters.marks {
        mark_filter(&mut new, f, visited, skeletons, frame)?;
    }
    mark("filter", &mut st);
    // Into the visited set: the new states (new: none held) with their
    // layer and hull, the grown states' hulls.
    let layer16 = u16::try_from(frame).map_err(|_| anyhow!("frame {frame}: past a u16 layer"))?;
    let parts: Vec<(u32, &super::set::RegionTable, &[HullOf<N>], &[Old<N>])> = new.iter().map(|sl| (sl.region, &sl.table, sl.hulls.as_slice(), sl.grown.as_slice())).collect();
    let up = visited.absorb(&parts, layer16, s.threads())?;
    drop(parts);
    ensure!(up.held_new == 0, "frame {frame}: {} of the next layer's states the visited set held already", up.held_new);
    if s.diag.phases {
        eprintln!("[absorb] f{frame}: {} states added; packed blocks kept {}, decoded {}, made {}", up.added, up.kept, up.decoded, up.made);
    }
    collect_freed();
    mark("absorb", &mut st);
    // The grown states by layer, then state: each route's sorted, appended
    // and freed a group at a time (the runs and lists are never both whole).
    let mut runs: Vec<Vec<Old<N>>> = Vec::with_capacity(new.len());
    let mut next = Layer::<N>::default();
    let mut hulls: Vec<HullOf<N>> = Vec::with_capacity(new.iter().map(|sl| sl.hulls.len()).sum());
    for sl in new {
        runs.push(sl.grown);
        if !sl.table.is_empty() {
            next.set.push(sl.region, sl.table)?;
            hulls.extend(sl.hulls);
        }
    }
    let runs: Vec<std::sync::Mutex<Vec<Old<N>>>> = runs.into_iter().map(std::sync::Mutex::new).collect();
    // (layer, state): a route's are by state, so a stable sort by layer.
    par_map(&runs, s.threads(), |r| r.lock().expect("a route's grown states").sort_unstable_by_key(|o| (o.layer, o.sk)));
    let runs: Vec<Vec<Old<N>>> = runs.into_iter().map(|r| r.into_inner().expect("a route's grown states")).collect();
    let mut count = vec![0usize; frame as usize + 1];
    for o in runs.iter().flatten() {
        count[o.layer as usize] += 1;
    }
    // Groups of about a 32nd of the grown states, in region order.
    let by: Vec<std::sync::Mutex<OldLayer<N>>> = count.iter().enumerate().map(|(l, &n)| std::sync::Mutex::new(OldLayer::new(l as u16, n))).collect();
    let layers: Vec<usize> = (0..count.len()).filter(|&l| count[l] > 0).collect();
    let group_states = count.iter().sum::<usize>() / 32 + 1;
    let mut runs = runs;
    while !runs.is_empty() {
        let (mut k, mut n) = (0, 0);
        while k < runs.len() && n < group_states {
            n += runs[k].len();
            k += 1;
        }
        let group: Vec<Vec<Old<N>>> = runs.drain(..k).collect();
        par_map(&layers, s.threads(), |&l| {
            let mut v = by[l].lock().expect("a layer's grown states");
            for r in &group {
                let (a, b) = (r.partition_point(|o| (o.layer as usize) < l), r.partition_point(|o| (o.layer as usize) <= l));
                for o in &r[a..b] {
                    v.push(o);
                }
            }
        });
    }
    let grown: ByLayer<N> = by.into_iter().map(|v| v.into_inner().expect("a layer's grown states")).filter(|x| !x.is_empty()).collect();
    let mut shapes_in: Vec<u32> = next.set.tables().iter().map(|(r, _)| r / GEOMETRY.slots).collect();
    shapes_in.dedup();
    for si in shapes_in {
        let h = visited.shape_hash(si);
        next.skeletons.insert(h, skeletons.get(h)?);
    }
    ensure!(next.states() == hulls.len() as u64, "frame {frame}: the next layer holds {} states, {} hulls", next.states(), hulls.len());
    mark("layer", &mut st);
    let wins = layer_wins(visited, &next, frame, s)?;
    mark("wins", &mut st);
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the next layer's wins decoded"));
    }
    st.lanes_kept = next.states() as usize;
    st.grown = by_layer_states(&grown);
    // The writers joined: the files are whole when the wave returns.
    if let Some(h) = edge_file {
        st.edge_bytes = h.join().map_err(|_| anyhow!("frame {frame}: the edge file's writer panicked"))??;
        mark("edge file", &mut st);
    }
    if let Some(h) = out_file {
        st.out_bytes = h.join().map_err(|_| anyhow!("frame {frame}: the out file's writer panicked"))??;
        mark("out file", &mut st);
    }
    if s.diag.phases {
        eprintln!("[end] f{frame}: {}", crate::frame::END_STEPS.iter().zip(&st.t_end).map(|(w, d)| format!("{w} {:.0} ms", d.as_secs_f64() * 1e3)).collect::<Vec<_>>().join(", "));
    }
    phases::add(phases::LAYER, tl);
    collect_freed();
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the frame's end"));
    }
    st.t_layer = t.elapsed();
    st.visited_bytes = visited.alloc_bytes();
    st.rss_end = crate::metrics::current_rss_gb();
    st.rss_file = crate::metrics::current_file_rss_gb();
    st.entries_out = next.entries();
    st.lanes_out = next.states() as usize;
    if s.diag.phases {
        st.phase_s = phases::take();
        phases::print(&st.phase_s, t_frame.elapsed(), workers);
    }
    Ok(Wave { next, hulls, grown, wins, stats: st, shapes: shapes_numbered, keys: adds })
}

/// THE OBJECTS LADDER's filter (`frame::MarkFilter`) on the new and grown
/// states: a re-push reaches a state along an edge recorded earlier, whose
/// verdict does not hold now; every state kept is checked at this frame.
fn mark_filter<N: Rem>(sealed: &mut [super::route::Sealed<N>], f: &crate::frame::MarkFilter<N>, visited: &VisitedSet<N>, skeletons: &Skeletons<N>, frame: u32) -> Result<()> {
    let geo = GEOMETRY;
    for sl in sealed.iter_mut() {
        let shape = visited.shape_hash(sl.region / geo.slots);
        let slot = sl.region % geo.slots;
        let skel = skeletons.get(shape)?;
        // The region's kept states, new then grown.
        let mut rows: Vec<(Key, u32)> = Vec::new();
        for (key, mask) in sl.table.iter() {
            rows.extend(bits(mask).map(|c| (key, geo.cell(slot, c))));
        }
        rows.extend(sl.grown.iter().map(|g| (g.sk.key, geo.cell(slot, g.sk.local as u32))));
        if rows.is_empty() {
            continue;
        }
        let rt2 = super::decode::decode_rows(&visited.keys, &skel, &rows)?;
        let ok = f.allowed(&rt2, frame)?;
        let n_new = sl.table.states() as usize;
        if ok.iter().all(|&x| x) {
            continue;
        }
        let (mut keys, mut masks, mut hulls) = (Vec::new(), Vec::new(), Vec::new());
        let mut i = 0usize;
        for (key, mask) in sl.table.iter() {
            let mut kept = 0u64;
            for c in bits(mask) {
                if ok[i] {
                    kept |= 1u64 << c;
                    hulls.push(sl.hulls[i]);
                }
                i += 1;
            }
            if kept != 0 {
                keys.push(key);
                masks.push(kept);
            }
        }
        sl.table = super::set::RegionTable::from_sorted(&keys, &masks)?;
        sl.hulls = hulls;
        let mut k = n_new;
        sl.grown.retain(|_| {
            k += 1;
            ok[k - 1]
        });
    }
    Ok(())
}

/// A re-push unit's pushes by target (route, key): its cells' merged hulls,
/// a run of `slots` by cell (moved to the end, capacity doubled, when full).
struct CellAccs<N: Rem> {
    idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    /// Per target: (route, key, cells), (its run's start, its capacity).
    targets: Vec<(u32, Key, u64)>,
    runs: Vec<(u32, u32)>,
    slots: Vec<AccOf<N>>,
}

impl<N: Rem> Default for CellAccs<N> {
    fn default() -> Self {
        CellAccs { idx: Default::default(), targets: Vec::new(), runs: Vec::new(), slots: Vec::new() }
    }
}

impl<N: Rem> CellAccs<N> {
    fn clear(&mut self) {
        self.idx.clear();
        self.targets.clear();
        self.runs.clear();
        self.slots.clear();
    }

    /// Target (route, key)'s index (new: no cell yet).
    fn target(&mut self, route: u32, key: Key) -> u32 {
        let n = self.targets.len() as u32;
        let t = *self.idx.entry((route, key)).or_insert(n);
        if t == n {
            self.targets.push((route, key, 0));
            self.runs.push((self.slots.len() as u32, 0));
        }
        t
    }

    #[inline]
    fn merge(&mut self, t: u32, c: u32, a: &AccOf<N>) {
        let m = self.targets[t as usize].2;
        let (start, cap) = self.runs[t as usize];
        let rank = (m & ((1u64 << c) - 1)).count_ones();
        if m >> c & 1 != 0 {
            self.slots[(start + rank) as usize].merge(a);
            return;
        }
        let len = m.count_ones();
        let start = if len < cap {
            start
        } else {
            let at = self.slots.len() as u32;
            let cap = (cap * 2).clamp(4, 64);
            self.slots.extend_from_within(start as usize..(start + len) as usize);
            self.slots.resize((at + cap) as usize, AccOf::<N>::default());
            self.runs[t as usize] = (at, cap);
            at
        };
        let (s, r, l) = (start as usize, rank as usize, len as usize);
        self.slots.copy_within(s + r..s + l, s + r + 1);
        self.slots[s + r] = *a;
        self.targets[t as usize].2 = m | 1u64 << c;
    }
}

struct RepushScratch<N: Rem> {
    dec: Option<zstd::bulk::Decompressor<'static>>,
    raw: Vec<u8>,
    accs: CellAccs<N>,
    /// Per block target its unit target (`u32::MAX`: none yet), per block
    /// route its cells level -1 drops at the frame (`None`: not yet asked).
    t_of: Vec<u32>,
    late: Vec<Option<u64>>,
    /// Pushes by (hull, last pushed hull, the block's transfer index).
    cache: crate::search::hull::PushCache<N>,
    /// Out blocks read, their raw bytes, their entries, the entries
    /// re-pushed (`--phases`' `[repush]`).
    read: [u64; 4],
}

impl<N: Rem> Default for RepushScratch<N> {
    fn default() -> Self {
        RepushScratch { dec: None, raw: Vec::new(), accs: CellAccs::default(), t_of: Vec::new(), late: Vec::new(), cache: Default::default(), read: [0; 4] }
    }
}

/// ONE RE-PUSH UNIT: old states `entries` of `layer` push their grown hulls
/// along their OUT-EDGES (layer + 1's out file) where an image grows
/// (`Hull::push_grown`), level -1 dropping as at an emission. Returns the
/// edges looked at.
#[inline(never)]
fn run_repush<N: Rem>(u: u32, layer: &OldLayer<N>, entries: std::ops::Range<usize>, outs: &super::out::OutStore, sink: &mut UnitSink<'_, N>, sc: &mut RepushScratch<N>) -> Result<u64> {
    use super::bundle::{source_cell, source_cells, TO_NONE};
    use std::cell::Cell;
    let geo = GEOMETRY;
    let none_slot = geo.no_cell_slot();
    sink.begin(u);
    sc.accs.clear();
    let old = &layer.entries[entries];
    let (Some(first), Some(last)) = (old.first(), old.last()) else { return Ok(0) };
    let f = layer.layer as u32 + 1;
    let file = outs.file(f).ok_or_else(|| anyhow!("re-push unit {u}: no out-edges of frame {f}"))?;
    let (lo, hi) = ((first.region, first.key), (last.region, last.key));
    let dec = sc.dec.get_or_insert_with(|| zstd::bulk::Decompressor::new().expect("a zstd decompressor"));
    let mut edges = 0u64;
    let cursor = Cell::new(0usize);
    // The entry being read: its first old state, and its old cells.
    let entry = Cell::new((0usize, 0u64));
    // A bundle's edges out of old states: (source cell, target cell).
    let mut pairs_of = [(0u8, 0u8); 64];
    let wanted = Cell::new(0u64);
    for bi in file.blocks(lo, hi) {
        // A block none of the unit's states is in is not read.
        let (bf, bl) = file.range(bi);
        let k = old.partition_point(|o| (o.region, o.key) < bf);
        if old.get(k).is_none_or(|o| (o.region, o.key) > bl) {
            continue;
        }
        file.block(bi, dec, &mut sc.raw)?;
        let blk = super::out::Block::<N>::open(&sc.raw)?;
        sc.read[0] += 1;
        sc.read[1] += sc.raw.len() as u64;
        sc.read[2] += blk.len() as u64;
        let routes: Vec<u32> = blk.routes.iter().map(|&(shape, slot)| sink.route(shape, slot)).collect();
        let pairs: Vec<crate::search::rem::Xfer<N>> = blk.pairs.iter().map(N::xfer).collect();
        sc.t_of.clear();
        sc.t_of.resize(blk.targets.len(), u32::MAX);
        sc.late.clear();
        sc.late.resize(blk.routes.len(), None);
        let (accs, t_of, late, cache) = (&mut sc.accs, &mut sc.t_of, &mut sc.late, &mut sc.cache);
        cache.next_epoch();
        let mut bad = false;
        blk.entries(
            |region, key| {
                let mut c = cursor.get();
                while c < old.len() && (old[c].region, old[c].key) < (region, key) {
                    c += 1;
                }
                cursor.set(c);
                if c < old.len() && (old[c].region, old[c].key) == (region, key) {
                    entry.set((old[c].first as usize, old[c].cells));
                    wanted.set(wanted.get() + 1);
                    true
                } else {
                    false
                }
            },
            |region, _, it| {
                let (a, cells) = entry.get();
                let s_slot = region % geo.slots;
                for ob in it {
                    let (Some(&(r, t_key)), Some(xfer)) = (blk.targets.get(ob.t as usize), pairs.get(ob.x as usize)) else {
                        bad = true;
                        return;
                    };
                    let ((shape, t_slot), route) = (blk.routes[r as usize], routes[r as usize]);
                    let dropped = *late[r as usize].get_or_insert_with(|| (0..64u32).filter(|&c| (t_slot != none_slot || c == 0) && sink.minus_one_drop(shape, geo.cell(t_slot, c))).fold(0u64, |a, c| a | 1u64 << c));
                    let m = ob.mask & !dropped;
                    if m == 0 {
                        continue;
                    }
                    // Target cells shifted to their sources, else cell by cell.
                    let mut n = 0usize;
                    if ob.o < TO_NONE && t_slot != none_slot && s_slot != none_slot {
                        let Some((src, k)) = source_cells(t_slot, s_slot, ob.o, m) else {
                            bad = true;
                            return;
                        };
                        let mut live = src & cells;
                        while live != 0 {
                            let s = live.trailing_zeros();
                            live &= live - 1;
                            pairs_of[n] = (s as u8, (s as i32 - k) as u8);
                            n += 1;
                        }
                    } else {
                        let mut mm = m;
                        while mm != 0 {
                            let c = mm.trailing_zeros();
                            mm &= mm - 1;
                            let (slot, s) = geo.slot_local(source_cell(geo.cell(t_slot, c), ob.o));
                            if slot != s_slot {
                                bad = true;
                                return;
                            }
                            if cells >> s & 1 != 0 {
                                pairs_of[n] = (s as u8, c as u8);
                                n += 1;
                            }
                        }
                    }
                    edges += n as u64;
                    for &(s, c) in &pairs_of[..n] {
                        // Old states of an entry are by cell: a cell's rank among them.
                        let [hull, was] = &layer.hulls[a + (cells & ((1u64 << s) - 1)).count_ones() as usize];
                        let Some(acc) = cache.get(hull, was, ob.x, || {
                            let mut a = AccOf::<N>::default();
                            hull.push_grown(was, xfer, &mut a).then_some(a)
                        }) else {
                            continue;
                        };
                        let t = match t_of[ob.t as usize] {
                            u32::MAX => {
                                let t = accs.target(route, t_key);
                                t_of[ob.t as usize] = t;
                                t
                            }
                            t => t,
                        };
                        accs.merge(t, c as u32, &acc);
                    }
                }
            },
        )?;
        ensure!(!bad, "re-push unit {u}: an out block's bundle names a target or transfer past its tables, or a source cell outside its entry's region");
    }
    sc.read[3] += wanted.get();
    sink.repush_chunks(&sc.accs.targets, &sc.accs.runs, &sc.accs.slots);
    Ok(edges)
}

/// The freed pages of the frame's (exited) worker threads back to the OS
/// now: mimalloc reclaims an exited thread's segments lazily otherwise.
fn collect_freed() {
    // SAFETY: mimalloc's documented API, callable from any thread.
    unsafe { libmimalloc_sys::mi_collect(true) };
}

/// States decoded at a time at the frame's end (a chunk of a region's).
const WIN_CHUNK: u32 = 1 << 12;

/// The new layer's WINS: per entry of `next` (ascending) its win cells.
/// `--key-check`: each decoded row's key is the boundary's, position its cell.
fn layer_wins<N: Rem>(visited: &VisitedSet<N>, next: &Layer<N>, frame: u32, s: &crate::settings::Settings) -> Result<Vec<(u32, u64)>> {
    let geo = GEOMETRY;
    let tables = next.set.tables();
    let starts = next.set.entry_starts();
    // Chunks of a table's entries: (table, entries).
    let mut chunks: Vec<(usize, std::ops::Range<u32>)> = Vec::new();
    for (ti, (_, t)) in tables.iter().enumerate() {
        let (mut a, mut n) = (0u32, 0u32);
        for (e, m) in t.masks().enumerate() {
            n += m.count_ones();
            if n >= WIN_CHUNK {
                chunks.push((ti, a..e as u32 + 1));
                (a, n) = (e as u32 + 1, 0);
            }
        }
        if (a as usize) < t.len() {
            chunks.push((ti, a..t.len() as u32));
        }
    }
    let parts = par_map(&chunks, s.threads(), |(ti, rg)| -> Result<Vec<(u32, u64)>> {
        let (region, t) = (tables[*ti].0, &tables[*ti].1);
        let shape = visited.shape_hash(region / geo.slots);
        let skel = next.skeletons.get(&shape).ok_or_else(|| anyhow!("no skeleton of shape {shape:016x}"))?;
        let slot = region % geo.slots;
        let mut rows: Vec<(Key, u32)> = Vec::with_capacity(WIN_CHUNK as usize + 64);
        let mut at: Vec<(u32, u32)> = Vec::with_capacity(WIN_CHUNK as usize + 64);
        for e in rg.clone() {
            let (key, mask) = (t.key(e), t.mask(e));
            for local in bits(mask) {
                rows.push((key, geo.cell(slot, local)));
                at.push((starts[*ti] as u32 + e, local));
            }
        }
        let rt2 = super::decode::decode_rows(&visited.keys, skel, &rows)?;
        if s.diag.key_check {
            crate::compiled::asm_kernel::key_check_block(&rt2, &visited.keys);
            for (c, r) in crate::search::pos_graph::block_cells(&rt2)?.into_iter().zip(rows.iter()) {
                ensure!(c == r.1, "frame {frame}: a state of r{region} decoded at cell {c}, not its own {}", r.1);
            }
        }
        let mut wins: Vec<(u32, u64)> = Vec::new();
        for (w, &(e, local)) in crate::frame::wins_of(&rt2, s)?.into_iter().zip(&at) {
            if w {
                match wins.last_mut() {
                    Some(l) if l.0 == e => l.1 |= 1u64 << local,
                    _ => wins.push((e, 1u64 << local)),
                }
            }
        }
        Ok(wins)
    });
    let mut out = Vec::new();
    for p in parts {
        out.extend(p?);
    }
    Ok(out)
}

/// A kernel unit's out block: compressed, or raw until its keys are final.
enum OutBlock {
    Sealed(Option<super::out::Sealed>),
    Raw(Vec<u8>),
}

/// A region job: one route's chunks of one slice (in unit order), its remaps.
struct Job<N: Rem> {
    state: std::sync::Arc<std::sync::Mutex<RouteState<N>>>,
    slice: u32,
    chunks: Vec<Chunk<N>>,
    remaps: std::sync::Arc<Vec<Vec<u32>>>,
}

/// The gather's: the new transfers (global ids `first` on), per worker its
/// merged length and local -> global ids, the routes made so far.
struct Gather<N: Rem> {
    fresh: XferTable<N>,
    first: u32,
    merged: Vec<usize>,
    remaps: Vec<Vec<u32>>,
    routes: usize,
}

/// The wave's pipeline: per slice its units left and chunks, the slices
/// gathered, the routes, the jobs (by slice, heaviest first within one).
struct Pipe<'a, N: Rem> {
    xtabs: &'a [std::sync::RwLock<Vec<N::Pair>>],
    next_unit: AtomicUsize,
    left: Vec<AtomicUsize>,
    /// Per slice, per worker its transfer table's length after its units of
    /// the slice (the pairs it met there are below).
    upto: Vec<Vec<AtomicUsize>>,
    chunks: Vec<std::sync::Mutex<Vec<Chunk<N>>>>,
    gathered: AtomicUsize,
    gather: std::sync::Mutex<Gather<N>>,
    states: std::sync::RwLock<Vec<std::sync::Arc<std::sync::Mutex<RouteState<N>>>>>,
    jobs: std::sync::Mutex<std::collections::VecDeque<Job<N>>>,
    jobs_left: AtomicUsize,
    /// A worker failed (an error or a panic): the others stop, so the
    /// frame's error comes back instead of a wait that never ends.
    failed: std::sync::atomic::AtomicBool,
    /// The chunks' bytes in flight and their most (`--phases`' `[wavemem]`).
    inflight: AtomicUsize,
    inflight_max: AtomicUsize,
}

impl<N: Rem> Pipe<'_, N> {
    fn pop_job(&self) -> Option<Job<N>> {
        self.jobs.lock().expect("the jobs").pop_front()
    }

    /// Nothing to take now: spin a little, then sleep a little.
    fn wait(&self, spins: &mut u32) {
        *spins += 1;
        if *spins < 64 {
            std::hint::spin_loop();
        } else if *spins < 256 {
            std::thread::yield_now();
        } else {
            std::thread::sleep(std::time::Duration::from_micros(50));
        }
    }

    /// GATHER the complete slices in order, one thread at a time: global ids
    /// for the pairs first met in each (tables up to `upto`: a function of
    /// the slice), the new routes, its chunks by route as jobs.
    fn gather_ready(&self, n_slices: usize, routes: &Routes, visited: &VisitedSet<N>) -> Result<()> {
        let mut g = self.gather.lock().expect("the gather");
        loop {
            let k = self.gathered.load(Ordering::Acquire);
            if k >= n_slices || self.left[k].load(Ordering::Acquire) != 0 {
                return Ok(());
            }
            let mut chunks = std::mem::take(&mut *self.chunks[k].lock().expect("a slice's chunks"));
            let g = &mut *g;
            let upto: Vec<usize> = self.upto[k].iter().zip(&g.merged).map(|(u, &m)| u.load(Ordering::Acquire).max(m)).collect();
            let tabs: Vec<std::sync::RwLockReadGuard<Vec<N::Pair>>> = (0..g.merged.len()).map(|w| self.xtab(w)).collect();
            let tails: Vec<&[N::Pair]> = tabs.iter().enumerate().map(|(w, t)| &t[g.merged[w]..upto[w]]).collect();
            let first = g.first;
            for (w, ids) in g.fresh.merge(&tails).into_iter().enumerate() {
                g.remaps[w].extend(ids.into_iter().map(|i| first + i));
                g.merged[w] = upto[w];
            }
            drop(tails);
            drop(tabs);
            let remaps = std::sync::Arc::new(g.remaps.clone());
            {
                let mut states = self.states.write().expect("the routes");
                for (shape, slot) in routes.list().into_iter().skip(g.routes) {
                    let region = visited.region(shape, slot);
                    states.push(std::sync::Arc::new(std::sync::Mutex::new(RouteState::new(shape, slot, region))));
                }
                g.routes = states.len();
            }
            // One job a route: its chunks in unit order; heaviest first.
            chunks.sort_unstable_by_key(|c| std::cmp::Reverse((c.route, c.unit)));
            let mut jobs: Vec<(usize, u32, Vec<Chunk<N>>)> = Vec::new();
            while let Some(c) = chunks.pop() {
                match jobs.last_mut() {
                    Some(j) if j.1 == c.route => {
                        j.0 += c.bundles.len();
                        j.2.push(c);
                    }
                    _ => jobs.push((c.bundles.len(), c.route, vec![c])),
                }
            }
            jobs.sort_by_key(|j| (std::cmp::Reverse(j.0), j.1));
            let states = self.states.read().expect("the routes");
            let mut q = self.jobs.lock().expect("the jobs");
            self.jobs_left.fetch_add(jobs.len(), Ordering::AcqRel);
            for (_, r, chunks) in jobs {
                q.push_back(Job { state: states[r as usize].clone(), slice: k as u32, chunks, remaps: remaps.clone() });
            }
            drop(q);
            drop(states);
            self.gathered.store(k + 1, Ordering::Release);
        }
    }

    fn xtab(&self, w: usize) -> std::sync::RwLockReadGuard<'_, Vec<N::Pair>> {
        self.xtabs[w].read().expect("a transfer table")
    }
}

/// PROVISIONAL keys made final (plans/architecture.md "Keys"): the missing
/// codes per (shape, field) SORTED and appended (new shapes first, by hash),
/// so the dictionaries are a function of the frame's new states.
fn finalize_keys<N: Rem>(visited: &mut VisitedSet<N>, states: &[RouteState<N>], prov: &Provisional<N>, adds: &mut KeyAdditions<N>) -> FxHashMap<Key, Key> {
    let mut keys: Vec<Key> = states.iter().flat_map(|r| r.prov_keys.iter().copied()).collect();
    keys.sort_unstable();
    keys.dedup();
    if keys.is_empty() {
        return FxHashMap::default();
    }
    let contents: Vec<MissKey<N>> = keys.iter().map(|&k| prov.content(k)).collect();
    let mut new_shapes: Vec<u64> = contents.iter().map(|m| m.shape).filter(|&s| visited.keys.shape(s).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for s in new_shapes {
        let r = prov.shape_record(s).unwrap_or_else(|| panic!("exact keys: the new shape {s:#x} was not noted"));
        visited.keys.add_shape(&r, adds);
    }
    let mut codes: std::collections::BTreeMap<(u64, u32), std::collections::BTreeSet<Code<N>>> = Default::default();
    for m in &contents {
        for &(c, code) in &m.missing {
            codes.entry((m.shape, c)).or_default().insert(code);
        }
    }
    for ((shape, cell), set) in codes {
        visited.keys.add_codes(shape, cell, &set.into_iter().collect::<Vec<_>>(), adds);
    }
    keys.iter().zip(&contents).map(|(&k, m)| (k, visited.keys.complete(m))).collect()
}

/// `f` over `items` on `threads` threads, in `items`' order.
pub(crate) fn par_map<T: Sync, R: Send>(items: &[T], threads: usize, f: impl Fn(&T) -> R + Sync) -> Vec<R> {
    crate::par::Pool::new(threads).map(items.len(), |i| Ok(f(&items[i]))).expect("an infallible map")
}

/// Frame 0: the initial states with the hull `start`, each KEYED AS IT IS,
/// not by its boundary view (`Rt2::widen_to`): where a `Flag::Always` entry
/// moves the view, a row keyed by it decodes to the view - the start stored
/// lossy, declined by the kernels' pins (the start's constants,
/// `trace::walk`) - and nothing looks the start up by projection
/// (`backward::Seeds::load`, `known::Pruning::start`). Each row must decode
/// to itself (`check_piece`).
pub(crate) fn seed<N: Rem>(visited: &mut VisitedSet<N>, blocks: &[Block<N>], start: HullOf<N>, s: &crate::settings::Settings) -> Result<Wave<N>> {
    let geo = visited.geo;
    let ids = crate::compiled::ids();
    let mut canon: Vec<celeste_engine::runtime2::Rt2<N>> = Vec::with_capacity(blocks.len());
    for b in blocks {
        let mut r = b.rt2().clone_block();
        r.canonical();
        canon.push(r);
    }
    let keys_added = visited.keys.absorb(&canon.iter().collect::<Vec<_>>(), ids);
    let mut shapes: Vec<u64> = canon.iter().map(|r| r.shape_hash).filter(|s| visited.shape_index(*s).is_none()).collect();
    shapes.sort_unstable();
    shapes.dedup();
    let mut numbered = Vec::new();
    for h in shapes {
        let i = visited.add_shape(h);
        numbered.push((i, h));
    }
    // Every row's state, checked to decode to the row; its win.
    let mut states: Vec<(Sk, bool)> = Vec::new();
    let mut skels: FxHashMap<u64, super::layer::SkelRecord<N>> = FxHashMap::default();
    for r in canon.iter_mut() {
        let keys: Vec<Key> = visited.keys.lookup_keys(r, ids).into_iter().map(|k| k.expect("seeded codes")).collect();
        let cells = crate::search::pos_graph::block_cells(r)?;
        r.row_keys = keys.clone();
        super::decode::check_piece(&visited.keys, r, &cells).map_err(|e| e.context("frame 0"))?;
        let rec = super::layer::SkelRecord::of(r)?;
        ensure!(skels.get(&r.shape_hash).is_none_or(|x| *x == rec), "two initial blocks of shape {:016x} with other skeletons", r.shape_hash);
        skels.insert(r.shape_hash, rec);
        let si = visited.shape_index(r.shape_hash).expect("numbered above");
        for ((&k, &c), w) in keys.iter().zip(&cells).zip(crate::frame::wins_of(r, s)?) {
            states.push((Sk::at(&geo, si, k, c), w));
        }
    }
    states.sort_unstable();
    ensure!(states.windows(2).all(|w| w[0].0 != w[1].0), "the initial states hold one state twice");
    let mut next = Layer::<N>::default();
    let mut wins: Vec<(u32, u64)> = Vec::new();
    let mut i = 0;
    while i < states.len() {
        let region = states[i].0.region;
        let j = i + states[i..].iter().take_while(|x| x.0.region == region).count();
        let (mut keys, mut masks): (Vec<Key>, Vec<u64>) = (Vec::new(), Vec::new());
        for &(sk, w) in &states[i..j] {
            if keys.last() != Some(&sk.key) {
                keys.push(sk.key);
                masks.push(0);
            }
            *masks.last_mut().expect("pushed") |= 1u64 << sk.local;
            if w {
                let e = (next.entries() + keys.len() - 1) as u32;
                match wins.last_mut() {
                    Some(l) if l.0 == e => l.1 |= 1u64 << sk.local,
                    _ => wins.push((e, 1u64 << sk.local)),
                }
            }
        }
        next.push_region(region, &keys, &masks)?;
        i = j;
    }
    let hulls = vec![start; next.states() as usize];
    let mut parts: Vec<(u32, &super::set::RegionTable, &[HullOf<N>], &[Old<N>])> = Vec::new();
    let mut at = 0usize;
    for (region, t) in next.set.tables() {
        let n = t.states() as usize;
        parts.push((*region, t, &hulls[at..at + n], &[]));
        at += n;
    }
    let held = visited.absorb(&parts, 0, s.threads())?.held_new;
    drop(parts);
    ensure!(held == 0, "frame 0 into a visited set that held {held} of its states");
    for (h, rec) in skels {
        let (cart, cache) = (canon[0].cart.clone(), canon[0].cache.clone());
        next.skeletons.insert(h, std::sync::Arc::new(rec.rt2(cart, cache)));
    }
    let n = next.states() as usize;
    let stats = FrameStats { lanes_kept: n, lanes_out: n, entries_out: next.entries(), ..Default::default() };
    Ok(Wave { next, hulls, grown: Vec::new(), wins, stats, shapes: numbered, keys: keys_added })
}

/// ONE FRAME of `rows` (widened to the engine's level) against an EMPTY
/// tree: every successor new, a row's own too (the seeds are forgotten once
/// keyed); the whole circle as their hulls. The successors, with their keys.
pub fn one_frame<N: Rem>(engine: &dyn FrameStep<N>, rows: Vec<Block<N>>, frame: u32) -> Result<Vec<Block<N>>> {
    let mut visited = VisitedSet::<N>::new(super::GEOMETRY);
    let mut xfers = XferTable::<N>::default();
    let skeletons = Skeletons::<N>::default();
    let s = engine.settings();
    let start = seed(&mut visited, &rows, HullOf::<N>::full(), &s)?.frontier();
    visited.forget_states();
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, filters: Filters::default(), frame, edges_dir: None, outs: None, skeletons: &skeletons, ceiling: None };
    let wave = run_wave(engine, start, cx)?;
    Ok(super::layer::decode_states(&wave.next, &visited.keys, visited.shapes(), usize::MAX, &|_| true)?.into_iter().map(|(_, rt2)| Block::from_rt2(rt2)).collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::hull::Hull;
    use crate::settings::Settings;

    /// FRAME 0 IS THE ENGINE'S START, also where its view is another row: a
    /// strawberry's bob (room (2,0) 100%), a key's sprite (room (0,3) gemskip
    /// 100%, J 1), a balloon's phase (room (7,0), J 1).
    #[test]
    fn frame_zero_is_the_start_where_its_view_is_not() {
        let ids = crate::compiled::ids();
        let cases = [
            Settings { room: (2, 0), hundred: true, ..Default::default() },
            Settings { room: (0, 3), hundred: true, gemskip: true, loading_jank: Some(1), ..Default::default() },
            Settings { room: (7, 0), loading_jank: Some(1), ..Default::default() },
        ];
        for s in cases {
            let start = Block::canonical(crate::engines::reference::RefEngine::new(&s).expect("engine").initial().expect("start")).expect("block");
            let mut view = start.rt2().clone_block();
            crate::frame::widen_rt2_to(&mut view, crate::abstraction::Level::EXACT);
            view.canonical();
            assert_ne!(celeste_engine::exact::exact_rows(&view, ids), celeste_engine::exact::exact_rows(start.rt2(), ids), "room {:?}: the start is its own view", s.room);
            let mut visited = VisitedSet::new(GEOMETRY);
            let wave = seed(&mut visited, std::slice::from_ref(&start), Hull::full(), &s).unwrap_or_else(|e| panic!("room {:?}: frame 0: {e:#}", s.room));
            let decoded = super::super::layer::decode_states(&wave.next, &visited.keys, visited.shapes(), usize::MAX, &|_| true).expect("decode");
            let [(sks, row)] = &decoded[..] else { panic!("room {:?}: frame 0 is {} blocks", s.room, decoded.len()) };
            assert_eq!(sks.len(), 1, "room {:?}: frame 0 is one state", s.room);
            assert_eq!(
                (celeste_engine::exact::exact_rows(row, ids), sks[0].cell(&GEOMETRY)),
                (celeste_engine::exact::exact_rows(start.rt2(), ids), start.positions().expect("cell")[0]),
                "room {:?}: frame 0 is not the start",
                s.room
            );
        }
    }

    /// An `OldLayer` gives back its states, whole and by range; its re-push
    /// units are whole entries of `REPUSH_UNIT` states or more (but the last).
    #[test]
    fn an_old_layer_gives_back_its_states() {
        let mut v = 7u64;
        let mut next = move || {
            v = v.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            v >> 11
        };
        let hull = |x: u64| Hull::from_bytes(&(x & 0x7fff_7fff_7fff_7fff).to_le_bytes());
        let mut olds: Vec<Old> = (0..60_000)
            .map(|_| {
                let (r, k, c) = (next() % 5, next() % 9000, next() % 64);
                Old { sk: Sk { region: r as u32, key: (0, k), local: c as u8 }, layer: 9, hull: hull(next()), was: hull(next()) }
            })
            .collect();
        olds.sort_unstable_by_key(|o| o.sk);
        olds.dedup_by_key(|o| o.sk);
        let mut l = OldLayer::new(9, olds.len());
        for o in &olds {
            l.push(o);
        }
        assert_eq!(l.len(), olds.len());
        assert!(l.entries.len() < olds.len(), "entries of several states");
        assert_eq!(l.iter().collect::<Vec<_>>(), olds);
        for r in [0..0, 0..1, 3..4, 100..2000, 17..olds.len(), olds.len() - 1..olds.len()] {
            assert_eq!(l.range(r.clone()).collect::<Vec<_>>(), olds[r.clone()], "range {r:?}");
        }
        // The units: REPUSH_UNIT states, then the last one's entry whole.
        let units = repush_units_of(&vec![l.clone()]);
        let mut a = 0usize;
        for u in &units {
            let UnitSpec::Repush { layer: 0, states, .. } = u else { panic!("a re-push unit") };
            let mut b = (a + REPUSH_UNIT).min(olds.len());
            while b < olds.len() && (olds[b].sk.region, olds[b].sk.key) == (olds[b - 1].sk.region, olds[b - 1].sk.key) {
                b += 1;
            }
            assert_eq!(*states, b - a);
            a = b;
        }
        assert_eq!(a, olds.len());
    }
}
