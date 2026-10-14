//! THE FRAME of the hull forward (plans/architecture.md "The forward
//! frame"; plans/edges-target.md, plans/keys-only.md): its frontier is the
//! states whose remainder HULL grew at the frame before - the NEW ones
//! (first reached then) and OLD ones (expanded before).
//!
//! The new states - a LAYER, per region its entries' keys and cell masks,
//! with their hulls - are cut into UNITS (runs of whole entries of one
//! shape), each DECODED to rows right before the kernels; every emission
//! ORed into its bundle of its target's ROUTE, and at the unit's end the
//! sources' hulls PUSHED along the bundles (`unit`); the REGION PHASE there
//! (a job a route and slice: the pushes merged into the route's touched
//! states, the slice's segment of the edge file). One PIPELINE: the units
//! are cut into SLICES, a slice's chunks become region jobs once its units
//! are done (and its transfers have global ids), and the jobs run while the
//! next slice's units do - the records in flight stay bounded.
//!
//! The old states run no kernel: their RECORDED edges (at their layer + 1)
//! are read by target region and their hulls pushed again (`repush`).
//!
//! Then the frame's END: provisional keys made final; per route its touched
//! states against the visited set - NEW states (the NEXT LAYER, no row
//! copied, decoded once more for their wins) and states whose hull GREW;
//! the frame's edge file. The result is a function of the frame, not of the
//! scheduling (the pushes merge order-free: `search::hull`).

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
use crate::search::hull::{Hull, HullAcc};
use celeste_engine::exact::{Code, KeyAdditions, MissKey, Provisional};

/// An OLD frontier state: expanded at its layer, its hull grown since its
/// last push (`was`: the hull it last pushed).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Old {
    pub sk: Sk,
    pub layer: u16,
    pub hull: Hull,
    pub was: Hull,
}

/// OLD states by layer: per layer (ascending; none without a state) its
/// states, sorted.
pub type ByLayer = Vec<(u16, Vec<Old>)>;

/// The states of `by`.
pub fn by_layer_states(by: &ByLayer) -> usize {
    by.iter().map(|x| x.1.len()).sum()
}

/// A FRONTIER (module doc): the states whose hull grew at a frame - the
/// NEW ones (first reached there: a layer, its skeletons, their hulls in
/// its state order), expanded by the kernels, and the OLD ones (expanded
/// at their layer before), re-pushed along their recorded edges.
#[derive(Default, Clone)]
pub struct Frontier {
    pub new: Layer,
    pub hulls: Vec<Hull>,
    pub old: ByLayer,
}

impl Frontier {
    pub fn is_empty(&self) -> bool {
        self.new.is_empty() && self.old.is_empty()
    }

    /// Old states held.
    pub fn old_states(&self) -> usize {
        by_layer_states(&self.old)
    }

    /// States held.
    pub fn states(&self) -> u64 {
        self.new.states() + self.old_states() as u64
    }
}

/// One wave's output.
pub struct Wave {
    /// The NEW states, by region and key, with their skeletons, and their
    /// hulls in its state order.
    pub next: Layer,
    pub hulls: Vec<Hull>,
    /// The states expanded before whose hull GREW.
    pub grown: ByLayer,
    /// The win states among the new ones: per entry of `next` (ascending)
    /// its win cells.
    pub wins: Vec<(u32, u64)>,
    pub stats: FrameStats,
    /// The shapes the wave numbered and what the key space gained.
    pub shapes: Vec<(u32, u64)>,
    pub keys: KeyAdditions,
}

impl Wave {
    /// The next frontier.
    pub fn frontier(self) -> Frontier {
        Frontier { new: self.next, hulls: self.hulls, old: self.grown }
    }

    /// The next frontier's states with their layers and hulls (`frame`:
    /// the wave's): the new ones in order, then the grown ones.
    pub fn frontier_states(&self, frame: u32) -> impl Iterator<Item = Grown> + '_ {
        let layer = u16::try_from(frame).expect("a frame past u16");
        self.next.states_iter().zip(&self.hulls).map(move |(s, &h)| (s, layer, h)).chain(self.grown.iter().flat_map(|x| &x.1).map(|o| (o.sk, o.layer, o.hull)))
    }
}

/// Lanes per unit (one kernel call) for a frame of `lanes` input states: a
/// 256th of the frame (16 units a worker at 16 threads), between 1024 and
/// 4096 lanes. A bigger unit fills more of each region's slices (room (3,0)
/// 100% r0sxhfn at 8 px, f49: 1024 -> 4096 lanes, padding 22% -> 11%); a
/// small frame keeps enough units to balance. A function of the frame
/// alone, not of the threads: the units cut the bundles and the slices
/// number the transfers, so the edge files are the same at every thread
/// count.
fn unit_lanes(lanes: u64) -> usize {
    ((lanes / 256) as usize).clamp(1024, 4096) & !63
}

/// A unit: a run of whole entries of one shape of the frontier's new
/// layer (at most `unit_lanes` + 63 states), and its first state's place
/// in the layer's state order (its hulls'); or a RE-PUSH unit, a run of
/// whole entries of the old states of one layer (the frontier's `layer`th
/// list, `old` the run's place in it).
enum UnitSpec {
    Kernel { entries: std::ops::Range<usize>, state: usize, lanes: u32 },
    Repush { layer: usize, old: std::ops::Range<usize> },
}

impl UnitSpec {
    /// The states it runs (its weight in a slice).
    fn lanes(&self) -> usize {
        match self {
            UnitSpec::Kernel { lanes, .. } => *lanes as usize,
            UnitSpec::Repush { old, .. } => old.len(),
        }
    }
}

/// Cut the layer's entries into units of about `unit` lanes, never across a
/// shape (`entries`: numbered over the layer's set, `RegionSet::entry_starts`).
fn units_of(layer: &Layer, unit: usize) -> Vec<UnitSpec> {
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

/// States per RE-PUSH unit (whole entries: at most 63 more): a re-push runs
/// no kernel, so a unit takes more states than a kernel unit.
const REPUSH_UNIT: usize = 1 << 14;

/// Cut the old states into re-push units of one layer each, never cutting
/// an entry.
fn repush_units_of(by: &ByLayer) -> Vec<UnitSpec> {
    let mut out = Vec::new();
    for (layer, (_, old)) in by.iter().enumerate() {
        let mut a = 0usize;
        while a < old.len() {
            let mut b = (a + REPUSH_UNIT).min(old.len());
            // Whole entries: the run's last entry's cells all in.
            while b < old.len() && (old[b].sk.region, old[b].sk.key) == (old[b - 1].sk.region, old[b - 1].sk.key) {
                b += 1;
            }
            out.push(UnitSpec::Repush { layer, old: a..b });
            a = b;
        }
    }
    out
}

/// Input lanes per SLICE: a slice's units run, then its records go through
/// the region phase, before the next slice's units run - so a frame holds
/// about one slice's records, not the whole frame's. Slices are runs of
/// units, so a function of the frontier, as everything they cut is. 256k
/// (2026-10-12; 1M before): the chunks in flight and the region jobs'
/// buffers a quarter (a big room's f77: -1.4 GB of the wave's peak; the
/// reference frame -0.27 GB), the region phase's worker time -30% (smaller
/// jobs), the wave the same; edge files -4% (a big room) to +3.5% (the
/// reference frame).
const SLICE_LANES: usize = 1 << 18;

/// The units cut into slices of about `SLICE_LANES` lanes.
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

/// What a wave reads and writes beyond its frontier.
pub struct WaveCtx<'a> {
    pub visited: &'a mut VisitedSet,
    pub xfers: &'a mut XferTable,
    pub pos: Option<&'a crate::search::pos_graph::PosObserver>,
    pub filters: Filters<'a>,
    pub frame: u32,
    /// The level's `edges/` dir: edges recorded (and the frame's edge file
    /// written) only with one.
    pub edges_dir: Option<&'a std::path::Path>,
    /// The tree's out-edges so far (the old states' re-push reads them;
    /// `None`: a frontier without old states).
    pub outs: Option<&'a super::out::OutStore>,
    /// The shapes' skeletons the emissions register (the forward's, kept
    /// across frames): the next layer's.
    pub skeletons: &'a Skeletons,
    /// The tree's ceiling (its level -1 horizon): which frontier states
    /// are expanded (`frame::not_expanded`, the orb's deadline).
    pub ceiling: Option<u32>,
}

/// A worker's buffers for its units: the rows `(key, cell)` a unit decodes,
/// their cells, its expanded lanes.
#[derive(Default)]
struct UnitScratch {
    rows: Vec<(Key, u32)>,
    cells: Vec<u32>,
    live: Vec<usize>,
}

/// ONE UNIT: its entries' states DECODED to rows (the layer's skeleton of
/// their shape), the rows the forward does not expand set aside
/// (`frame::not_expanded`, decided here for every frontier, fresh or
/// resumed), the rest through the engine into the sink, each lane with its
/// hull (`hulls`: the layer's, in its state order).
#[allow(clippy::too_many_arguments)]
fn run_unit(engine: &dyn FrameStep, u: u32, entries: std::ops::Range<usize>, state: usize, layer: &Layer, hulls: &[Hull], starts: &[usize], shapes: &[u64], sink: &mut UnitSink, sc: &mut UnitScratch, layer_frame: u32, ceiling: Option<u32>, s: &crate::settings::Settings) -> Result<()> {
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

/// ONE FORWARD FRAME (module doc): the wave of frame `cx.frame` from the
/// frontier `front` (the states whose hull grew at `cx.frame - 1`).
pub fn run_wave(engine: &dyn FrameStep, front: Frontier, cx: WaveCtx) -> Result<Wave> {
    use std::time::Instant;
    let WaveCtx { visited, xfers, pos, filters, frame, edges_dir, outs, skeletons, ceiling } = cx;
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let (level, s) = (engine.level(), &engine.settings());
    let game_frame = frame.div_ceil(s.steps_per_frame());
    ensure!(
        !level.platforms || game_frame as usize <= crate::trace::kernel::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::kernel::PLATFORM_WORLD_FRAMES
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
    // the wave's threads (new heaps) allocate: kept, it adds to the wave's
    // peak (the fixture room's f279 started with 6.3 GB of it).
    collect_freed();
    let mut st = FrameStats::default();
    let workers = s.threads();
    let t_frame = Instant::now();
    let layer = &front.new;
    st.entries_in = layer.entries();
    st.lanes_in = layer.states() as usize;
    st.old_in = front.old_states();
    st.frontier_bytes = layer.alloc_bytes() + std::mem::size_of_val(front.hulls.as_slice()) + front.old_states() * std::mem::size_of::<Old>();
    st.rss_start = crate::metrics::current_rss_gb();
    let record = edges_dir.is_some();
    // The frame's SPOOL (`storage::spool`): the out blocks and the jobs'
    // segments wait there for the frame's end, not in memory.
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
    // The visited set is read only until the frame's end; the workers'
    // transfer tables are shared with the gather.
    let xtabs: Vec<std::sync::RwLock<Vec<crate::search::arc_edges::Pair>>> = (0..workers).map(|_| Default::default()).collect();
    // The frame's new transfers are numbered from here (canonically at its end).
    // The tree's table is read only (shared by the workers) until the
    // frame's end; the gather numbers the frame's new pairs after it, in a
    // table of their own (`Gather::fresh`).
    let xfers_before = xfers.pairs.len();
    let base: &XferTable = xfers;
    let mut sinks: Vec<UnitSink> = (0..workers).map(|w| UnitSink::new(visited, &routes, skeletons, &prov, filters, frame, level, w as u32, base, &xtabs[w], pos.is_some(), record, s)).collect();
    // Per kernel unit its out-edge block (`storage::out`): compressed, or
    // raw where a target key is provisional (encoded again at the end).
    let mut out_blocks: Vec<std::sync::Mutex<Vec<OutBlock>>> = (0..n_kernel).map(|_| Default::default()).collect();
    let repushed = std::sync::atomic::AtomicU64::new(0);
    let mut scratch: Vec<super::route::Scratch> = (0..workers).map(|_| super::route::Scratch::default()).collect();
    let mut slice_of = vec![0u32; units.len()];
    for (k, r) in slices.iter().enumerate() {
        slice_of[r.clone()].fill(k as u32);
    }
    let pipe = Pipe {
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
    // THE PIPELINE (one scope for the frame): a worker takes a region job
    // first (the oldest slice's, heaviest first), else the next unit - at
    // most one slice past the last gathered - and the worker that ends a
    // slice's last unit GATHERS the slices that are complete, in order:
    // their transfers' global ids, new routes, their chunks as jobs. Region
    // work overlaps the next slices' kernels; no barrier but the frame's end.
    let spent: Vec<(std::time::Duration, std::time::Duration, [u64; 4])> = std::thread::scope(|scope| {
        let handles: Vec<_> = sinks
            .iter_mut()
            .zip(scratch.iter_mut())
            .map(|(sink, sc)| {
                let (units, pipe, slice_of, slices, routes, shapes, starts, hulls, old_by, out_blocks, repushed, spool) = (&units, &pipe, &slice_of, &slices, &routes, &shapes, &starts, &front.hulls, &front.old, &out_blocks, &repushed, spool.as_deref());
                let visited: &VisitedSet = visited;
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
                            let cs: Vec<&Chunk> = job.chunks.iter().collect();
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
                                            OutBlock::Sealed(super::out::Sealed::of(&raw, z.get_or_insert_with(|| zstd::bulk::Compressor::new(1).expect("a zstd compressor")))?.map(|x| x.spill(spool)).transpose()?)
                                        });
                                    }
                                    *out_blocks[u].lock().expect("an out block") = v;
                                    phases::add(phases::OUT_BLOCKS, to);
                                }
                                UnitSpec::Repush { layer, old } => {
                                    let tr = phases::start(s.diag.phases);
                                    let n = run_repush(u as u32, &old_by[*layer].1[old.clone()], outs.expect("checked above"), sink, &mut rs)?;
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
    let mut states: Vec<RouteState> = states
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
        let sk = sinks.iter().fold([0usize; 5], |a, k| {
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
            "[wavemem] f{frame}: frontier {:.3} GB ({} entries, {} old states), visited {:.3} GB ({} entries), transfers {:.3}, the out store's index {:.3}, the routes' touched states {:.3}, provisional bundles {:.3}, segments {:.3}, out blocks {:.3}, spooled {:.3}, chunks in flight at most {:.3}, sinks {:.3} (bundles {:.3}, lane hulls {:.3}, pos pairs {:.3}, transfers {:.3}: {x_all} met, at most {x_max} a worker, rest {:.3}), region scratch {:.3} (bundles {:.3}, encoder {:.3}, raw {:.3}, rest {:.3}) GB; anon {:.2} GB",
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
            gb(sk[4]),
            gb(sc.iter().sum()),
            gb(sc[0]),
            gb(sc[1]),
            gb(sc[2]),
            gb(sc[3]),
            crate::metrics::current_rss_gb()
        );
    }
    // Per new transfer, the edges the frame recorded with it.
    let mut xfer_counts = vec![0u64; fresh.pairs.len()];
    for (sink, remap) in sinks.iter().zip(&remaps) {
        for (&n, &g) in sink.xfer_counts().iter().zip(remap) {
            if g as usize >= xfers_before {
                xfer_counts[g as usize - xfers_before] += n;
            }
        }
    }
    for sink in &mut sinks {
        sink.finish()?;
        st.lanes_raw += sink.emitted as usize;
        if let Some(p) = pos {
            p.record_pairs(sink.pos_pairs());
        }
    }
    st.units = n_units;
    drop(sinks);
    xfers.append(fresh);
    // The workers' buffers (freed here, made by the exited workers) back
    // before the frame's end allocates.
    collect_freed();

    // THE FRAME'S END.
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
    // THE OUT FILE, written beside the rest of the frame's end (it needs
    // the final keys only): the kernel units' blocks in unit order, the
    // provisional ones encoded with their final keys.
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
                                super::out::fixup(&raw, &finals, &mut fixed)?;
                                out.extend(super::out::Sealed::of(&fixed, &mut zstd::bulk::Compressor::new(1)?)?);
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
    // content), whatever the slices that met them first (their bundles
    // waited for their ids).
    let perm = xfers.canonical(xfers_before, &xfer_counts);

    // Per route (in parallel): sealed - its new and grown states.
    type Sealed = super::route::Sealed;
    let sealed: Vec<Result<Option<Sealed>>> = {
        // The routes' touched keys sorted (each route on a thread, the
        // biggest first), then compared with the visited set in parts (a
        // big route's on every thread), then each route's parts put
        // together.
        let cells: Vec<std::sync::Mutex<(usize, &mut RouteState)>> = states.iter_mut().enumerate().map(std::sync::Mutex::new).collect();
        let mut order: Vec<&std::sync::Mutex<(usize, &mut RouteState)>> = cells.iter().collect();
        order.sort_by_cached_key(|c| std::cmp::Reverse(c.lock().expect("a route").1.touched.len()));
        let touches = par_map(&order, workers, |c| -> Result<(usize, Option<super::route::Touch>)> {
            let mut g = c.lock().expect("a route");
            let k = g.0;
            match regions[k] {
                Some(region) => Ok((k, Some(super::route::seal_sort(&mut g.1, region, &finals)?))),
                // A route of a new shape whose records the coarser level
                // all dropped.
                None if !g.1.is_used() => Ok((k, None)),
                None => Err(anyhow::anyhow!("frame {frame}: a used route of an unnumbered shape")),
            }
        });
        let mut touches: Vec<(usize, Option<super::route::Touch>)> = touches.into_iter().collect::<Result<_>>()?;
        touches.sort_by_key(|x| x.0);
        // The parts route by route (the biggest first), in order: a
        // route's last part puts it together on the worker that ends it,
        // so its touched keys and its parts go as soon as it is sealed
        // (all the routes' were held until every part was done).
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
        let touches: Vec<std::sync::RwLock<Option<super::route::Touch>>> = touches.into_iter().map(|(_, t)| std::sync::RwLock::new(t)).collect();
        let got: Vec<std::sync::Mutex<Vec<(usize, super::route::SealPart)>>> = (0..touches.len()).map(|_| Default::default()).collect();
        let out: Vec<std::sync::Mutex<Option<Sealed>>> = (0..touches.len()).map(|_| Default::default()).collect();
        let visited: &VisitedSet = visited;
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
        out.into_iter().map(|o| Ok(o.into_inner().expect("a sealed route"))).collect::<Vec<Result<Option<Sealed>>>>()
    };
    // The routes' touched tables are freed (by other threads than the ones
    // that made them, which have exited): their pages back before the next
    // steps allocate.
    collect_freed();
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the routes sealed (keys final, new and grown states)"));
    }
    // THE NEXT LAYER (the routes' new states, frozen, by region, with their
    // hulls) and the grown states.
    let mut new: Vec<Sealed> = Vec::new();
    for x in sealed {
        if let Some(sl) = x? {
            new.push(sl);
        }
    }
    new.sort_unstable_by_key(|x| x.region);
    mark("seal", &mut st);
    // THE EDGE FILE (and the frame's new transfers), written beside the
    // rest of the frame's end: per route its jobs' segments (final once
    // sealed) and those that waited, encoded again with their final keys
    // and transfer ids (`route::reencode`, here) - in the file per route
    // the jobs' by slice, then the waited ones by slice.
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
        let mut big: Vec<&Sealed> = new.iter().collect();
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
        mark_filter(&mut new, f, visited, skeletons, frame, s)?;
    }
    mark("filter", &mut st);
    // Into the visited set: the new states (new: none held) with their
    // layer and hull, the grown states' hulls.
    let layer16 = u16::try_from(frame).map_err(|_| anyhow!("frame {frame}: past a u16 layer"))?;
    let parts: Vec<(u32, &super::set::RegionTable, &[Hull], &[Old])> = new.iter().map(|sl| (sl.region, &sl.table, sl.hulls.as_slice(), sl.grown.as_slice())).collect();
    let up = visited.absorb(&parts, layer16, s.threads())?;
    drop(parts);
    ensure!(up.held_new == 0, "frame {frame}: {} of the next layer's states the visited set held already", up.held_new);
    if s.diag.phases {
        eprintln!("[absorb] f{frame}: {} states added; packed blocks kept {}, decoded {}, made {}", up.added, up.kept, up.decoded, up.made);
    }
    collect_freed();
    mark("absorb", &mut st);
    // The grown states by layer, each layer's by state: every route's
    // (sorted by state) put in layer order (in parallel), then the routes'
    // runs, in region order, appended to their layers' lists and freed one
    // by one - the runs and the lists are never both whole (both were: 48 B
    // a grown state twice at the frame's end).
    let mut runs: Vec<Vec<Old>> = Vec::with_capacity(new.len());
    let mut next = Layer::default();
    let mut hulls: Vec<Hull> = Vec::with_capacity(new.iter().map(|sl| sl.hulls.len()).sum());
    for sl in new {
        runs.push(sl.grown);
        if !sl.table.is_empty() {
            next.set.push(sl.region, sl.table)?;
            hulls.extend(sl.hulls);
        }
    }
    let runs: Vec<std::sync::Mutex<Vec<Old>>> = runs.into_iter().map(std::sync::Mutex::new).collect();
    // By layer, then state (a route's are by state: the stable sort by layer,
    // without its buffer).
    par_map(&runs, s.threads(), |r| r.lock().expect("a route's grown states").sort_unstable_by_key(|o| (o.layer, o.sk)));
    let runs: Vec<Vec<Old>> = runs.into_iter().map(|r| r.into_inner().expect("a route's grown states")).collect();
    let mut count = vec![0usize; frame as usize + 1];
    for o in runs.iter().flatten() {
        count[o.layer as usize] += 1;
    }
    let mut by: Vec<Vec<Old>> = count.iter().map(|&n| Vec::with_capacity(n)).collect();
    for r in runs {
        let mut a = 0;
        while a < r.len() {
            let l = r[a].layer;
            let b = a + r[a..].partition_point(|o| o.layer == l);
            by[l as usize].extend_from_slice(&r[a..b]);
            a = b;
        }
    }
    let grown: ByLayer = by.into_iter().enumerate().filter(|x| !x.1.is_empty()).map(|(l, v)| (l as u16, v)).collect();
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

    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the next layer merged into the visited set"));
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

/// THE OBJECTS LADDER's filter (`frame::MarkFilter`) on the frame's new and
/// grown states: a re-push reaches a state through an edge recorded at an
/// earlier frame, so the verdict the units took at that emission (its
/// frame's) does not hold for this one; every state the frame keeps is
/// checked at this frame. Rows decoded per region (the new states' with
/// the layer's skeletons, every shape's registered).
fn mark_filter(sealed: &mut [super::route::Sealed], f: &crate::frame::MarkFilter, visited: &VisitedSet, skeletons: &Skeletons, frame: u32, _s: &crate::settings::Settings) -> Result<()> {
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
        if ok[..n_new].iter().all(|&x| x) && ok[n_new..].iter().all(|&x| x) {
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

/// A re-push unit's pushes by target (route, key): per target its cells
/// and their merged hulls, a run of `slots` in cell order (moved to the
/// end, its capacity doubled, when a cell joins a full one).
#[derive(Default)]
struct CellAccs {
    idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    /// Per target: (route, key, cells), (its run's start, its capacity).
    targets: Vec<(u32, Key, u64)>,
    runs: Vec<(u32, u32)>,
    slots: Vec<HullAcc>,
}

impl CellAccs {
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

    /// `a` merged into target `t`'s cell `c`.
    #[inline]
    fn merge(&mut self, t: u32, c: u32, a: &HullAcc) {
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
            self.slots.resize((at + cap) as usize, HullAcc::EMPTY);
            self.runs[t as usize] = (at, cap);
            at
        };
        let (s, r, l) = (start as usize, rank as usize, len as usize);
        self.slots.copy_within(s + r..s + l, s + r + 1);
        self.slots[s + r] = *a;
        self.targets[t as usize].2 = m | 1u64 << c;
    }
}

/// A worker's buffers for its re-push units: the out file's block and the
/// pushes by target.
#[derive(Default)]
struct RepushScratch {
    dec: Option<zstd::bulk::Decompressor<'static>>,
    raw: Vec<u8>,
    accs: CellAccs,
    /// Per block target its unit target (`u32::MAX`: none yet), per block
    /// route its cells level -1 drops at the frame (`None`: not yet asked).
    t_of: Vec<u32>,
    late: Vec<Option<u64>>,
    /// Pushes by (hull, last pushed hull, the block's transfer index).
    cache: crate::search::hull::PushCache,
    /// Out blocks read, their raw bytes, their entries, the entries
    /// re-pushed (`--phases`' `[repush]`).
    read: [u64; 4],
}

/// ONE RE-PUSH UNIT (module doc): old states `old` of one layer (whole
/// entries, sorted by state), each pushing its grown hull along its
/// OUT-EDGES (`storage::out`: its layer + 1's file) where an edge's image
/// grows (`Hull::push_grown`); level -1 drops a target too late at this
/// frame, as at an emission; the pushes into the sink's chunks by route.
/// Returns the edges looked at.
#[inline(never)]
fn run_repush(u: u32, old: &[Old], outs: &super::out::OutStore, sink: &mut UnitSink, sc: &mut RepushScratch) -> Result<u64> {
    use super::bundle::{source_cell, source_cells, TO_NONE};
    use std::cell::Cell;
    let geo = GEOMETRY;
    let none_slot = geo.no_cell_slot();
    sink.begin(u);
    sc.accs.clear();
    let Some(first) = old.first() else { return Ok(0) };
    let f = first.layer as u32 + 1;
    let file = outs.file(f).ok_or_else(|| anyhow!("re-push unit {u}: no out-edges of frame {f}"))?;
    let last = old.last().expect("a unit's states");
    let (lo, hi) = ((first.sk.region, first.sk.key), (last.sk.region, last.sk.key));
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
        let k = old.partition_point(|o| (o.sk.region, o.sk.key) < bf);
        if old.get(k).is_none_or(|o| (o.sk.region, o.sk.key) > bl) {
            continue;
        }
        file.block(bi, dec, &mut sc.raw)?;
        let blk = super::out::Block::open(&sc.raw)?;
        sc.read[0] += 1;
        sc.read[1] += sc.raw.len() as u64;
        sc.read[2] += blk.len() as u64;
        let routes: Vec<u32> = blk.routes.iter().map(|&(shape, slot)| sink.route(shape, slot)).collect();
        let pairs: Vec<(crate::search::arcs::Transfer, crate::search::arcs::Transfer)> = blk.pairs.iter().map(|p| (p.0.transfer(), p.1.transfer())).collect();
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
                while c < old.len() && (old[c].sk.region, old[c].sk.key) < (region, key) {
                    c += 1;
                }
                cursor.set(c);
                if c < old.len() && (old[c].sk.region, old[c].sk.key) == (region, key) {
                    let cells = old[c..].iter().take_while(|o| (o.sk.region, o.sk.key) == (region, key)).fold(0u64, |m, o| m | 1u64 << o.sk.local);
                    entry.set((c, cells));
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
                    let (Some(&(r, t_key)), Some(&(tx, ty))) = (blk.targets.get(ob.t as usize), pairs.get(ob.x as usize)) else {
                        bad = true;
                        return;
                    };
                    let ((shape, t_slot), route) = (blk.routes[r as usize], routes[r as usize]);
                    let dropped = *late[r as usize].get_or_insert_with(|| (0..64u32).filter(|&c| (t_slot != none_slot || c == 0) && sink.minus_one_drop(shape, geo.cell(t_slot, c))).fold(0u64, |a, c| a | 1u64 << c));
                    let m = ob.mask & !dropped;
                    if m == 0 {
                        continue;
                    }
                    // The edges out of old cells: the target cells shifted
                    // to their sources (the common case), else cell by cell.
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
                        let o = &old[a + (cells & ((1u64 << s) - 1)).count_ones() as usize];
                        let Some(acc) = cache.get(&o.hull, &o.was, ob.x, || {
                            let mut a = HullAcc::EMPTY;
                            o.hull.push_grown(&o.was, tx, ty, &mut a).then_some(a)
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
/// now: mimalloc reclaims an exited thread's segments lazily otherwise (the
/// arc phase's ~3 GB on a big room, plans/arc-memory.md).
fn collect_freed() {
    // SAFETY: mimalloc's documented API, callable from any thread.
    unsafe { libmimalloc_sys::mi_collect(true) };
}

/// States decoded at a time at the frame's end (a chunk of a region's).
const WIN_CHUNK: u32 = 1 << 12;

/// The new layer's states DECODED (their shapes' skeletons, the layer's),
/// for their WINS (`frame::wins_of`): per entry of `next` (ascending) its
/// win cells. Chunks of a region's entries on every thread. With
/// `--key-check`, each decoded row's key is the boundary's and its
/// position its cell.
fn layer_wins(visited: &VisitedSet, next: &Layer, frame: u32, s: &crate::settings::Settings) -> Result<Vec<(u32, u64)>> {
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

/// A kernel unit's out-edge block: compressed, or raw until its
/// provisional target keys are final.
enum OutBlock {
    Sealed(Option<super::out::Sealed>),
    Raw(Vec<u8>),
}

/// A region job: one route's chunks of one slice (in unit order), and the
/// slice's transfer remaps.
struct Job {
    state: std::sync::Arc<std::sync::Mutex<RouteState>>,
    slice: u32,
    chunks: Vec<Chunk>,
    remaps: std::sync::Arc<Vec<Vec<u32>>>,
}

/// What the gather owns: the frame's new transfers (`fresh`: their global
/// ids are `first` on), per worker its merged length and its local ids'
/// global ones, the routes made so far.
struct Gather {
    fresh: XferTable,
    first: u32,
    merged: Vec<usize>,
    remaps: Vec<Vec<u32>>,
    routes: usize,
}

/// The wave's pipeline (`run_wave`): units by index, per slice its units
/// left and its chunks, the slices gathered, the routes, the jobs (by
/// slice, heaviest first within one).
struct Pipe<'a> {
    xtabs: &'a [std::sync::RwLock<Vec<crate::search::arc_edges::Pair>>],
    next_unit: AtomicUsize,
    left: Vec<AtomicUsize>,
    /// Per slice, per worker its transfer table's length after its units of
    /// the slice (the pairs it met there are below).
    upto: Vec<Vec<AtomicUsize>>,
    chunks: Vec<std::sync::Mutex<Vec<Chunk>>>,
    gathered: AtomicUsize,
    gather: std::sync::Mutex<Gather>,
    states: std::sync::RwLock<Vec<std::sync::Arc<std::sync::Mutex<RouteState>>>>,
    jobs: std::sync::Mutex<std::collections::VecDeque<Job>>,
    jobs_left: AtomicUsize,
    /// A worker failed (an error or a panic): the others stop, so the
    /// frame's error comes back instead of a wait that never ends.
    failed: std::sync::atomic::AtomicBool,
    /// The chunks' bytes in flight (handed over, their job not done) and
    /// their most (`--phases`' `[wavemem]`).
    inflight: AtomicUsize,
    inflight_max: AtomicUsize,
}

impl Pipe<'_> {
    fn pop_job(&self) -> Option<Job> {
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

    /// GATHER every complete slice past the gathered ones, in order (one
    /// thread at a time): its transfers' global ids - each worker's pairs
    /// first met in it, its table's new pairs up to its length after its
    /// units of the slice (`upto`): the slice's pairs not merged before, a
    /// function of the slice - the new routes with their regions,
    /// its chunks by route as jobs.
    fn gather_ready(&self, n_slices: usize, routes: &Routes, visited: &VisitedSet) -> Result<()> {
        let mut g = self.gather.lock().expect("the gather");
        loop {
            let k = self.gathered.load(Ordering::Acquire);
            if k >= n_slices || self.left[k].load(Ordering::Acquire) != 0 {
                return Ok(());
            }
            let mut chunks = std::mem::take(&mut *self.chunks[k].lock().expect("a slice's chunks"));
            let g = &mut *g;
            let upto: Vec<usize> = self.upto[k].iter().zip(&g.merged).map(|(u, &m)| u.load(Ordering::Acquire).max(m)).collect();
            let tabs: Vec<std::sync::RwLockReadGuard<Vec<crate::search::arc_edges::Pair>>> = (0..g.merged.len()).map(|w| self.xtab(w)).collect();
            let tails: Vec<&[crate::search::arc_edges::Pair]> = tabs.iter().enumerate().map(|(w, t)| &t[g.merged[w]..upto[w]]).collect();
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
            let mut jobs: Vec<(usize, u32, Vec<Chunk>)> = Vec::new();
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

    /// Worker `w`'s transfer table.
    fn xtab(&self, w: usize) -> std::sync::RwLockReadGuard<'_, Vec<crate::search::arc_edges::Pair>> {
        self.xtabs[w].read().expect("a transfer table")
    }
}

/// The frame's PROVISIONAL keys made final (plans/exact-keys.md): the new
/// states' codes the key space lacks, per (shape, field) SORTED and
/// appended (new shapes first, by hash); returns each provisional key's
/// packed key. The dictionaries then are a function of the frame's new
/// states.
fn finalize_keys(visited: &mut VisitedSet, states: &[RouteState], prov: &Provisional, adds: &mut KeyAdditions) -> FxHashMap<Key, Key> {
    let mut keys: Vec<Key> = states.iter().flat_map(|r| r.prov_keys.iter().copied()).collect();
    keys.sort_unstable();
    keys.dedup();
    if keys.is_empty() {
        return FxHashMap::default();
    }
    let contents: Vec<MissKey> = keys.iter().map(|&k| prov.content(k)).collect();
    let mut new_shapes: Vec<u64> = contents.iter().map(|m| m.shape).filter(|&s| visited.keys.shape(s).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for s in new_shapes {
        let r = prov.shape_record(s).unwrap_or_else(|| panic!("exact keys: the new shape {s:#x} was not noted"));
        visited.keys.add_shape(&r, adds);
    }
    let mut codes: std::collections::BTreeMap<(u64, u32), std::collections::BTreeSet<Code>> = Default::default();
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

/// `f` over `items` on `threads` threads, results in `items`' order.
pub fn par_map<T: Sync, R: Send>(items: &[T], threads: usize, f: impl Fn(&T) -> R + Sync) -> Vec<R> {
    let next = AtomicUsize::new(0);
    let mut out: Vec<(usize, R)> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..threads.clamp(1, items.len().max(1)))
            .map(|_| {
                let (next, f) = (&next, &f);
                scope.spawn(move || {
                    let mut mine = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(x) = items.get(i) else { break };
                        mine.push((i, f(x)));
                    }
                    mine
                })
            })
            .collect();
        hs.into_iter().flat_map(|h| h.join().expect("a parallel worker panicked")).collect()
    });
    out.sort_unstable_by_key(|e| e.0);
    out.into_iter().map(|e| e.1).collect()
}

/// Frame 0: the initial states into an empty visited set - keyed as the
/// forward keys a reference row (`UnitSink::row_key`: the level's held
/// buttons widened) and stored as they are, so each must DECODE from its
/// key and cell to exactly itself (`decode::check_piece`; a row its key does
/// not determine is fatal) - each with the hull `start` (the start's
/// remainder; the whole circle for rows of no known one). Returns the
/// layer (its skeletons the rows' own), its hulls, its wins, the shapes
/// numbered and the key space's codes.
pub fn seed(visited: &mut VisitedSet, blocks: &[Block], level: crate::abstraction::Level, start: Hull, s: &crate::settings::Settings) -> Result<Wave> {
    let geo = visited.geo;
    let ids = crate::compiled::ids();
    let level = crate::abstraction::Level { held: level.held, ..crate::abstraction::Level::EXACT };
    let mut canon: Vec<celeste_engine::runtime2::Rt2> = Vec::with_capacity(blocks.len());
    let mut views: Vec<celeste_engine::runtime2::Rt2> = Vec::with_capacity(blocks.len());
    for b in blocks {
        let mut r = b.rt2().clone_block();
        r.canonical();
        let mut v = r.clone_block();
        crate::frame::widen_rt2_to(&mut v, level);
        v.canonical();
        ensure!(v.shape_hash == r.shape_hash, "an initial row's key view changed its shape");
        canon.push(r);
        views.push(v);
    }
    let keys_added = visited.keys.absorb(&views.iter().collect::<Vec<_>>(), ids);
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
    let mut skels: FxHashMap<u64, super::layer::SkelRecord> = FxHashMap::default();
    for (r, v) in canon.iter_mut().zip(&views) {
        let keys: Vec<Key> = visited.keys.lookup_keys(v, ids).into_iter().map(|k| k.expect("seeded codes")).collect();
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
    let mut next = Layer::default();
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
    let mut parts: Vec<(u32, &super::set::RegionTable, &[Hull], &[Old])> = Vec::new();
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

/// What `one_frame` makes: the successors (rows with their keys) and the
/// key space their keys are in.
pub struct OneFrame {
    pub next: Vec<Block>,
    pub keys: celeste_engine::exact::KeySpace,
}

/// ONE FRAME of the rows `rows` against an EMPTY tree, no edges: every
/// successor is new (the diagnostics' `rerun`, the tests). The rows are
/// seeded as a tree's start is (`seed`: each must decode from its key),
/// with the whole circle as their hulls (a row's remainder is widened:
/// every successor of every remainder).
pub fn one_frame(engine: &dyn FrameStep, rows: Vec<Block>, frame: u32) -> Result<OneFrame> {
    let mut visited = VisitedSet::new(super::GEOMETRY);
    let mut xfers = XferTable::default();
    let skeletons = Skeletons::default();
    let s = engine.settings();
    let start = seed(&mut visited, &rows, engine.level(), Hull::full(), &s)?.frontier();
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, pos: None, filters: Filters::default(), frame, edges_dir: None, outs: None, skeletons: &skeletons, ceiling: None };
    let wave = run_wave(engine, start, cx)?;
    let next = super::layer::decode_states(&wave.next, &visited.keys, visited.shapes(), usize::MAX, &|_| true)?.into_iter().map(|(_, rt2)| Block::from_rt2(rt2)).collect();
    Ok(OneFrame { next, keys: visited.keys })
}
