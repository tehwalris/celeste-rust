//! THE FRAME (plans/edges-target.md "The forward", plans/keys-only.md): the
//! frontier - a LAYER, per region its entries' keys and cell masks - cut
//! into UNITS (runs of whole entries of one shape), each DECODED to rows
//! right before the kernels; every emission ORed into its bundle of its
//! target's ROUTE (`route`); the REGION PHASE there (a job a route and
//! slice: the dedup against the region's table, the slice's segment of the
//! edge file). One PIPELINE a frame: the units are cut into SLICES, a
//! slice's chunks become region jobs once its units are done (and its
//! transfers have global ids), and the jobs run while the next slice's
//! units do - the records in flight stay bounded. Then the frame's END:
//! provisional keys made final, the routes' new states listed by key - the
//! NEXT LAYER, no row copied - and decoded once more for their wins, the
//! frame's edge file. The result is a function of the frame, not of the
//! scheduling.

use anyhow::{anyhow, ensure, Result};
use std::sync::atomic::{AtomicUsize, Ordering};

use rustc_hash::FxHashMap;

use super::decode::Skeletons;
use super::edges::XferTable;
use super::layer::{bits, Layer, Note};
use super::route::{Chunk, RegionCtx, RouteState, Routes};
use super::unit::{RaiseCtx, UnitSink};
use super::visited::{Key, VisitedSet};
use super::{Sk, GEOMETRY};
use crate::frame::{phases, Block, Filters, FrameStats, FrameStep, Raised};
use celeste_engine::exact::{is_provisional, Code, KeyAdditions, MissKey, Provisional};

/// Every state's LAYER (first frame), for a raise: a hit on a state of a
/// later layer than the raised frame is refused. Per region, `cells` u16
/// per (table place, cell) - in-memory only, beside the visited set whose
/// places it follows.
pub struct StateLayers {
    cells: usize,
    by_region: rustc_hash::FxHashMap<u32, Vec<u16>>,
}

impl StateLayers {
    pub fn new(cells: usize) -> Self {
        StateLayers { cells, by_region: Default::default() }
    }

    /// The state at `local` of entry `e` (its place in region `region`'s
    /// table) is first reached at `layer`.
    pub fn set(&mut self, region: u32, e: u32, local: u32, layer: u32) {
        let v = self.by_region.entry(region).or_default();
        let at = e as usize * self.cells + local as usize;
        if v.len() <= at {
            v.resize((at / self.cells + 1) * self.cells, u16::MAX);
        }
        v[at] = u16::try_from(layer).expect("a layer past u16");
    }

    /// The layer of a state the tree holds.
    pub fn layer(&self, region: u32, e: u32, local: u32) -> u32 {
        let at = e as usize * self.cells + local as usize;
        let l = self.by_region.get(&region).and_then(|v| v.get(at)).copied().unwrap_or(u16::MAX);
        assert!(l != u16::MAX, "state r{region} place {e} cell {local} has no layer");
        l as u32
    }

    /// Every state of `layer` (a frame's, in `visited`) first reached at `frame`.
    pub fn set_layer(&mut self, visited: &VisitedSet, layer: &Layer, frame: u32) -> Result<()> {
        // (The layer is merged into the visited set already.)
        for (region, key, mask) in layer.iter() {
            let e = visited.table(region).and_then(|t| t.find(key)).ok_or_else(|| anyhow!("a state of r{region} the visited set lacks"))?;
            for local in bits(mask) {
                self.set(region, e, local, frame);
            }
        }
        Ok(())
    }
}

/// A part of a wave's frontier: a layer, and whether its states are the
/// tree's (a raise re-expanding noted sources) rather than new.
#[derive(Clone, Copy)]
pub struct Part<'a> {
    pub layer: &'a Layer,
    pub old: bool,
}

/// One wave's output.
pub struct Wave {
    /// The NEW states, by region and key, with their skeletons.
    pub next: Layer,
    /// The win states among them: per entry of `next` (ascending) its win cells.
    pub wins: Vec<(u32, u64)>,
    pub stats: FrameStats,
    /// The level -1 drops' sources (`Filters::notes_drops`), with the
    /// smallest horizon admitting one of their dropped successors.
    pub dropped: Vec<Note>,
    /// The shapes the wave numbered and what the key space gained.
    pub shapes: Vec<(u32, u64)>,
    pub keys: KeyAdditions,
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

/// A unit: a run of whole entries of one shape of a frontier part (at most
/// `unit_lanes` + 63 states).
struct UnitSpec {
    part: u32,
    entries: std::ops::Range<usize>,
    lanes: u32,
}

/// Cut each part's entries into units of about `unit` lanes, never across
/// a shape (`entries`: numbered over the part's set, `RegionSet::entry_starts`).
fn units_of(parts: &[Part], unit: usize) -> Vec<UnitSpec> {
    let slots = GEOMETRY.slots;
    let mut out = Vec::new();
    for (pi, p) in parts.iter().enumerate() {
        let mut cur: Option<(u32, usize, u32)> = None;
        let mut e = 0usize;
        for (region, t) in p.layer.set.tables() {
            let shape = region / slots;
            for m in t.masks() {
                let n = m.count_ones();
                cur = match cur {
                    Some((sh, a, l)) if sh == shape && (l as usize) < unit => Some((sh, a, l + n)),
                    Some((_, a, l)) => {
                        out.push(UnitSpec { part: pi as u32, entries: a..e, lanes: l });
                        Some((shape, e, n))
                    }
                    None => Some((shape, e, n)),
                };
                e += 1;
            }
        }
        if let Some((_, a, l)) = cur {
            out.push(UnitSpec { part: pi as u32, entries: a..e, lanes: l });
        }
    }
    out
}

/// Input lanes per SLICE: a slice's units run, then its records go through
/// the region phase, before the next slice's units run - so a frame holds
/// one slice's records (~24 B an edge, ~45 edges a lane in room (6,2) 100%:
/// ~1.1 GB a slice), not the whole frame's. Slices are runs of units, so a
/// function of the frontier, as everything they cut is.
const SLICE_LANES: usize = 1 << 20;

/// The units cut into slices of about `SLICE_LANES` lanes.
fn slices_of(units: &[UnitSpec]) -> Vec<std::ops::Range<usize>> {
    let mut out = Vec::new();
    let (mut a, mut lanes) = (0usize, 0usize);
    for (k, u) in units.iter().enumerate() {
        lanes += u.lanes as usize;
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
    /// A RAISE of the frame (`None`: a new frame).
    pub raised: Option<Raised>,
    /// A raise's per-state layers.
    pub layers: Option<&'a StateLayers>,
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

/// ONE UNIT: its entries' states DECODED to rows (the part's skeleton of
/// their shape), the rows the forward does not expand set aside
/// (`frame::not_expanded`, decided here for every frontier, fresh or
/// resumed), the rest through the engine into the sink.
#[allow(clippy::too_many_arguments)]
fn run_unit(engine: &dyn FrameStep, u: u32, spec: &UnitSpec, part: Part, starts: &[usize], shapes: &[u64], sink: &mut UnitSink, sc: &mut UnitScratch, layer_frame: u32, ceiling: Option<u32>, s: &crate::settings::Settings) -> Result<()> {
    let geo = GEOMETRY;
    let t_dec = phases::start(s.diag.phases);
    sink.begin(u, part.old);
    sc.rows.clear();
    sc.cells.clear();
    let layer = part.layer;
    let tables = layer.set.tables();
    let (mut ti, mut local) = layer.set.locate(starts, spec.entries.start)?;
    let mut region = tables[ti].0;
    for _ in spec.entries.clone() {
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
            sink.push_lane(si, cell);
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
/// frontier `parts` (states of frame `cx.frame - 1`).
pub fn run_wave(engine: &dyn FrameStep, parts: &[Part], cx: WaveCtx) -> Result<Wave> {
    use std::time::Instant;
    let WaveCtx { visited, xfers, pos, filters, frame, edges_dir, raised, layers, skeletons, ceiling } = cx;
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let (level, s) = (engine.level(), &engine.settings());
    let game_frame = frame.div_ceil(s.steps_per_frame());
    ensure!(
        !level.platforms || game_frame as usize <= crate::trace::kernel::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::kernel::PLATFORM_WORLD_FRAMES
    );
    ensure!(frame > 0, "a wave makes frame 1 on");
    let t_warm = Instant::now();
    engine.warm();
    if let Some(m) = filters.minus_one {
        let _ = m.table(s);
    }
    if t_warm.elapsed().as_secs_f64() > 0.5 {
        // Not `[fwd] f...`: log readers (`export-ui`) parse those as frames.
        eprintln!("[warm] f{frame}: engine and level -1 table built in {:.1} s, before the wave", t_warm.elapsed().as_secs_f64());
    }
    let mut st = FrameStats::default();
    let workers = s.threads();
    let t_frame = Instant::now();
    st.entries_in = parts.iter().map(|p| p.layer.entries()).sum();
    st.lanes_in = parts.iter().map(|p| p.layer.states() as usize).sum();
    st.frontier_bytes = parts.iter().map(|p| p.layer.alloc_bytes()).sum();
    st.rss_start = crate::metrics::current_rss_gb();
    let record = edges_dir.is_some();
    ensure!(raised.is_none() || layers.is_some(), "frame {frame}: a raise needs the tree's layers");
    ensure!(raised.is_some() || parts.iter().all(|p| !p.old), "frame {frame}: the tree's own sources re-expanded outside a raise");
    let units = units_of(parts, unit_lanes(st.lanes_in as u64));
    let starts: Vec<Vec<usize>> = parts.iter().map(|p| p.layer.set.entry_starts()).collect();
    let slices = slices_of(&units);
    let routes = Routes::default();
    let prov = Provisional::default();
    // The visited set is read only until the frame's end; the workers'
    // transfer tables are shared with the gather.
    let xtabs: Vec<std::sync::RwLock<Vec<crate::search::arc_edges::Pair>>> = (0..workers).map(|_| Default::default()).collect();
    let mut sinks: Vec<UnitSink> = (0..workers)
        .map(|w| UnitSink::new(visited, &routes, skeletons, &prov, filters, frame, level, w as u32, &xtabs[w], record, pos.is_some(), raised.map(|r| RaiseCtx { raised: r }), s))
        .collect();
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
        gather: std::sync::Mutex::new(Gather { xfers, merged: vec![0; workers], remaps: vec![Vec::new(); workers], routes: 0 }),
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
    let spent: Vec<(std::time::Duration, std::time::Duration)> = std::thread::scope(|scope| {
        let handles: Vec<_> = sinks
            .iter_mut()
            .zip(scratch.iter_mut())
            .map(|(sink, sc)| {
                let (units, pipe, slice_of, slices, routes, shapes, starts) = (&units, &pipe, &slice_of, &slices, &routes, &shapes, &starts);
                let visited: &VisitedSet = visited;
                std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<(std::time::Duration, std::time::Duration)> {
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
                    loop {
                        if pipe.failed.load(Ordering::Acquire) {
                            stop.1 = true;
                            return Err(anyhow!("frame {frame}: another wave worker failed"));
                        }
                        if let Some(job) = pipe.pop_job() {
                            let t = Instant::now();
                            let tp = phases::start(s.diag.phases);
                            let rcx = RegionCtx { frame, record, remaps: &job.remaps, layers: raised.and(layers) };
                            let cs: Vec<&Chunk> = job.chunks.iter().collect();
                            let mut route = job.state.lock().expect("a route");
                            let vt = route.region.and_then(|r| visited.table(r));
                            let r = super::route::run_route(&mut route, vt, job.slice, &cs, &rcx, sc);
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
                            let spec = &units[u];
                            run_unit(engine, u as u32, spec, parts[spec.part as usize], &starts[spec.part as usize], shapes, sink, &mut us, frame - 1, ceiling, s)?;
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
                                pipe.gather_ready(slices.len(), routes, visited)?;
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
                    Ok((busy, in_regions))
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
    let busy: std::time::Duration = spent.iter().map(|w| w.0).sum();
    st.t_regions = spent.iter().map(|w| w.1).sum();
    st.t_wave = t_units;
    st.wave_idle = idle_fraction(t_units, workers, busy);
    st.rss_wave = crate::metrics::current_rss_gb();
    st.slices = slices.len();
    if s.diag.phases {
        let gb = |b: usize| b as f64 / 1e9;
        let routes = pipe.states.read().expect("the routes");
        let rest = routes.iter().fold([0usize; 3], |a, r| {
            let b = r.lock().expect("a route").alloc_bytes();
            [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
        });
        drop(routes);
        let sc = scratch.iter().fold([0usize; 4], |a, k| {
            let b = k.alloc_bytes();
            [a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]]
        });
        let sk = sinks.iter().fold([0usize; 4], |a, k| {
            let b = k.alloc_bytes();
            [a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]]
        });
        eprintln!(
            "[wavemem] f{frame}: frontier {:.3} GB ({} entries), visited {:.3} GB ({} entries), the next layer's tables {:.3}, provisional bundles {:.3}, segments {:.3}, chunks in flight at most {:.3}, sinks {:.3} (bundles {:.3}, drop notes {:.3}, pos pairs {:.3}, rest {:.3}), region scratch {:.3} (bundles {:.3}, encoder {:.3}, raw {:.3}, rest {:.3}) GB; anon {:.2} GB",
            gb(st.frontier_bytes),
            st.entries_in,
            gb(visited.alloc_bytes()),
            visited.entries(),
            gb(rest[0]),
            gb(rest[1]),
            gb(rest[2]),
            gb(pipe.inflight_max.load(Ordering::Acquire)),
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
    let mut notes: Vec<Note> = Vec::new();
    for sink in &mut sinks {
        sink.finish()?;
        st.lanes_raw += sink.emitted as usize;
        if let Some(p) = pos {
            p.record_pairs(sink.pos_edges.iter().copied());
        }
        notes.append(&mut sink.notes);
    }
    st.units = units.len();
    drop(sinks);
    let Pipe { gather, states, .. } = pipe;
    let Gather { xfers, .. } = gather.into_inner().expect("the gather");
    let mut states: Vec<RouteState> = states
        .into_inner()
        .expect("the routes")
        .into_iter()
        .map(|a| std::sync::Arc::try_unwrap(a).map_err(|_| anyhow::anyhow!("a route still shared")).and_then(|m| m.into_inner().map_err(|_| anyhow::anyhow!("a route lock poisoned"))))
        .collect::<Result<_>>()?;

    // THE FRAME'S END.
    let t = Instant::now();
    let tl = phases::start(s.diag.phases);
    let mut adds = KeyAdditions::default();
    let finals = finalize_keys(visited, &states, &prov, &mut adds);
    drop(prov);
    (st.key_bits, st.key_codes, st.new_codes) = (visited.keys.max_bits(), visited.keys.codes(), adds.codes.len());
    // New shapes numbered (in hash order); every route gets its region.
    let mut new_shapes: Vec<u64> = states.iter().filter(|r| r.new_states > 0).map(|r| r.shape).filter(|&h| visited.shape_index(h).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    let mut shapes_numbered = Vec::new();
    for h in new_shapes {
        let i = visited.add_shape(h);
        shapes_numbered.push((i, h));
    }
    let regions: Vec<Option<u32>> = states.iter().map(|r| visited.region(r.shape, r.slot)).collect();
    let inserted: u64 = states.iter().map(|r| r.new_states).sum();
    // Per route (in parallel): sealed - its new states listed by key.
    type Sealed = super::route::Sealed;
    let sealed: Vec<Result<Option<Sealed>>> = {
        let next = AtomicUsize::new(0);
        let cells: Vec<std::sync::Mutex<&mut RouteState>> = states.iter_mut().map(std::sync::Mutex::new).collect();
        std::thread::scope(|scope| {
            let hs: Vec<_> = scratch
                .iter_mut()
                .map(|sc| {
                    let (next, cells, regions, finals) = (&next, &cells, &regions, &finals);
                    scope.spawn(move || {
                        let mut out = Vec::new();
                        loop {
                            let k = next.fetch_add(1, Ordering::Relaxed);
                            let Some(c) = cells.get(k) else { return out };
                            let mut st = c.lock().expect("a route");
                            let r = match regions[k] {
                                Some(region) => super::route::seal(&mut st, region, finals, sc).map(Some),
                                // A route of a new shape whose records the
                                // coarser level all dropped.
                                None if st.new_states == 0 => Ok(None),
                                None => Err(anyhow::anyhow!("frame {frame}: a route of an unnumbered shape with new states")),
                            };
                            out.push((k, r));
                        }
                    })
                })
                .collect();
            let mut all: Vec<(usize, Result<Option<Sealed>>)> = hs.into_iter().flat_map(|h| h.join().expect("a seal worker panicked")).collect();
            all.sort_by_key(|x| x.0);
            all.into_iter().map(|x| x.1).collect()
        })
    };
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the routes sealed (keys final, the next layer frozen)"));
    }
    // THE NEXT LAYER: the routes' new states, frozen, by region.
    let mut new: Vec<Sealed> = Vec::new();
    for x in sealed {
        if let Some(sl) = x? {
            if !sl.table.is_empty() {
                new.push(sl);
            }
        }
    }
    new.sort_unstable_by_key(|x| x.region);
    let mut next = Layer::default();
    for sl in new {
        next.set.push(sl.region, sl.table)?;
    }
    let mut shapes_in: Vec<u32> = next.set.tables().iter().map(|(r, _)| r / GEOMETRY.slots).collect();
    shapes_in.dedup();
    for si in shapes_in {
        let h = visited.shape_hash(si);
        next.skeletons.insert(h, skeletons.get(h)?);
    }
    ensure!(next.states() == inserted, "frame {frame}: the region phase inserted {inserted} new states, the next layer lists {}", next.states());
    let wins = layer_wins(visited, &next, frame, s)?;
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the next layer's wins decoded"));
    }
    // The new states into the visited set (they are new: none held).
    let held = visited.merge(&next.set, s.threads())?;
    if s.diag.phases {
        crate::metrics::mem_phase(&format!("f{frame} the next layer merged into the visited set"));
    }
    ensure!(held == 0, "frame {frame}: {held} of the next layer's states the visited set held already");
    // The routes' segments into the file.
    let mut segs = Vec::new();
    let mut src_index: Vec<(u32, Vec<u32>)> = Vec::new();
    for (r, region) in states.into_iter().zip(&regions) {
        let Some(region) = *region else { continue };
        st.edge_bundles += r.segs.iter().map(|g| g.1.bundles as u64).sum::<u64>();
        st.edge_records += r.segs.iter().map(|g| g.1.edges).sum::<u64>();
        st.routes += 1;
        if !r.sources.is_empty() {
            src_index.push((region, r.sources.into_iter().collect()));
        }
        segs.extend(r.segs.into_iter().map(|g| g.1));
    }
    st.lanes_kept = next.states() as usize;
    if let Some(dir) = edges_dir {
        xfers.flush(dir)?;
        let path = super::edges::file_path(dir, frame, raised.map(|r| r.seq));
        st.edge_bytes = super::edges::write_file(&path, frame, segs, src_index)?;
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
    phases::print_phases(s.diag.phases, t_frame.elapsed(), workers);
    Ok(Wave { next, wins, stats: st, dropped: super::layer::notes_of(notes)?, shapes: shapes_numbered, keys: adds })
}

/// The freed pages of the frame's (exited) worker threads back to the OS
/// now: mimalloc reclaims an exited thread's segments lazily otherwise (the
/// arc phase's ~3 GB on the mod room, plans/arc-memory.md).
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

/// A region job: one route's chunks of one slice (in unit order), and the
/// slice's transfer remaps.
struct Job {
    state: std::sync::Arc<std::sync::Mutex<RouteState>>,
    slice: u32,
    chunks: Vec<Chunk>,
    remaps: std::sync::Arc<Vec<Vec<u32>>>,
}

/// What the gather owns: the global transfer table, per worker its merged
/// length and its ids' global ones, the routes made so far.
struct Gather<'a> {
    xfers: &'a mut XferTable,
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
    gather: std::sync::Mutex<Gather<'a>>,
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
            for (w, ids) in g.xfers.merge(&tails).into_iter().enumerate() {
                g.remaps[w].extend(ids);
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
    let mut keys: Vec<Key> = states.iter().flat_map(|r| r.next.iter().map(|x| x.0).filter(|&k| is_provisional(k))).collect();
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
/// not determine is fatal). Returns the layer (its skeletons the rows'
/// own), its wins, the shapes numbered and the key space's codes.
pub fn seed(visited: &mut VisitedSet, blocks: &[Block], level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Wave> {
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
    let held = visited.merge(&next.set, s.threads())?;
    ensure!(held == 0, "frame 0 into a visited set that held {held} of its states");
    for (h, rec) in skels {
        let (cart, cache) = (canon[0].cart.clone(), canon[0].cache.clone());
        next.skeletons.insert(h, std::sync::Arc::new(rec.rt2(cart, cache)));
    }
    let n = next.states() as usize;
    let stats = FrameStats { lanes_kept: n, lanes_out: n, entries_out: next.entries(), ..Default::default() };
    Ok(Wave { next, wins, stats, dropped: Vec::new(), shapes: numbered, keys: keys_added })
}

/// What `one_frame` makes: the successors (rows with their keys) and the
/// key space their keys are in.
pub struct OneFrame {
    pub next: Vec<Block>,
    pub keys: celeste_engine::exact::KeySpace,
}

/// ONE FRAME of the rows `rows` against an EMPTY tree, no edges: every
/// successor is new (the diagnostics' `rerun`, the tests). The rows are
/// seeded as a tree's start is (`seed`: each must decode from its key).
pub fn one_frame(engine: &dyn FrameStep, rows: Vec<Block>, frame: u32) -> Result<OneFrame> {
    let mut visited = VisitedSet::new(super::GEOMETRY);
    let mut xfers = XferTable::default();
    let skeletons = Skeletons::default();
    let s = engine.settings();
    let start = seed(&mut visited, &rows, engine.level(), &s)?;
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, pos: None, filters: Filters::default(), frame, edges_dir: None, raised: None, layers: None, skeletons: &skeletons, ceiling: None };
    let wave = run_wave(engine, &[Part { layer: &start.next, old: false }], cx)?;
    let next = super::layer::decode_states(&wave.next, &visited.keys, visited.shapes(), usize::MAX, &|_| true)?.into_iter().map(|(_, rt2)| Block::from_rt2(rt2)).collect();
    Ok(OneFrame { next, keys: visited.keys })
}
