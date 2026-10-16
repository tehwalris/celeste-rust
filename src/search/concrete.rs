//! THE CONCRETE SEARCH (plans/architecture.md "The search", step 3): from
//! the room's start, every input (one per class of the buttons a frame
//! reads: `Covered`) and every `rnd` leaf, a successor admitted only where
//! the level's winning sets hold its exact remainder, each exact state
//! once. First a depth-first try for a win AT the bound (at most
//! `dfs_budget` steps; a win there is optimal, the bound being a lower
//! bound), then breadth-first layers: the first layer with a win is the
//! optimum, its path the witness.
//!
//! Sound: every concrete path that wins by the horizon stays inside W, so
//! the search is EXHAUSTIVE inside W and prunes by nothing else. States are
//! told apart by their EXACT identity, never the level's widened key
//! (`exact_state`, `522de36`).
//!
//! One search (`search`) for what it steps (`Concrete`): one arithmetic's
//! reference engine (`Single`), or a pair of them (`dual::Pairs`, the dual
//! search). Layers run in parallel, chunks of parents merged in order, so
//! the witness is the same at any thread count.

use anyhow::{ensure, Result};
use celeste_core::arith::{Arithmetic, EngineNum};
use celeste_engine::exact::{ExactRow, KeySpace};
use celeste_engine::runtime2::Rt2;
use rustc_hash::FxHashSet;

use super::arc_dp::Winning;
use super::arcs::Circle;
use super::rem::{Point, Rem};
use crate::abstraction::Level;
use crate::frame::{wins_of, Block};
use crate::settings::Settings;
use crate::storage::{Sk, GEOMETRY};
use crate::trace::refengine::ReferenceEngine;

/// What the concrete search steps: its engine (one a worker), its states,
/// their identity (dedup and the depth-first try's memo), and what an edge
/// records for the witness (`Label`).
pub(super) trait Concrete: Sync {
    type Engine: Send;
    type State: Send + Sync;
    type Key: Clone + Eq + std::hash::Hash + Send;
    type Label: Copy + Send;
    /// The logs' name (`concrete`, `dual`).
    const NAME: &'static str;
    /// `n` engines, one a worker.
    fn engines(&self, n: usize) -> Result<Vec<Self::Engine>>;
    /// The start, and its label (the witness's first).
    fn start(&self, eng: &mut Self::Engine) -> Result<(Self::State, Self::Label)>;
    /// `st`'s successors under `byte`, admitted at W step `t`, in leaf
    /// order: a win (its label), or a state inside W (its key and label);
    /// and the buttons the frame read. A state `known` already may be left
    /// out before its W lookup. `steps` counts the engines' leaves.
    #[allow(clippy::too_many_arguments)]
    fn successors(&self, eng: &mut Self::Engine, st: &Self::State, byte: u8, t: u32, known: &dyn Fn(&Self::Key) -> bool, steps: &mut u64) -> Result<(Vec<Next<Self>>, u8)>;
    /// The depth-first try's budget, in steps.
    fn budget(&self) -> u64;
    /// What a layer's log adds about its states.
    fn note(&self, _layer: &[Self::State]) -> String {
        String::new()
    }
}

/// A successor (`Concrete::successors`).
pub(super) enum Next<C: Concrete + ?Sized> {
    Win(C::Label),
    State(C::State, C::Key, C::Label),
}

/// A concrete path: its inputs, and each state's label (the start's first).
pub(super) struct Path<L> {
    pub inputs: Vec<u8>,
    pub labels: Vec<L>,
}

/// THE CONCRETE SEARCH of `c` from its start: a depth-first try at `bound`
/// (frames), then breadth-first to `frames`; W at frame `k` is step `spf *
/// k` (a frame boundary: there a state keys exactly as a whole frame's).
/// `prefer` orders the inputs (a known route first: only WHICH optimum is
/// found). `None`: no win by `frames`.
pub(super) fn search<C: Concrete>(c: &C, frames: u32, bound: u32, spf: u32, prefer: Option<&[u8]>, workers: usize) -> Result<Option<Path<C::Label>>> {
    let name = C::NAME;
    ensure!(bound <= frames, "the bound f{bound} is past the horizon f{frames}");
    let engines: Vec<std::sync::Mutex<C::Engine>> = c.engines(workers.max(1))?.into_iter().map(std::sync::Mutex::new).collect();
    let (start, start_label) = c.start(&mut engines[0].lock().expect("an engine"))?;
    let t0 = std::time::Instant::now();
    crate::metrics::mem_phase(&format!("{name}: engines up"));

    // THE FAST PATH: depth-first for a win AT the bound, inside `W_{H -
    // bound + k}`, on one engine. Sound: a win here is at the lower bound,
    // hence optimal; an exhausted or abandoned try proves nothing and the
    // breadth-first search runs.
    {
        struct Dfs<'a, C: Concrete> {
            c: &'a C,
            eng: &'a mut C::Engine,
            from: u32,
            frames: u32,
            spf: u32,
            prefer: Option<&'a [u8]>,
            /// Exhausted states by (identity, frame).
            dead: FxHashSet<(C::Key, u32)>,
            path: Vec<(u8, C::Label)>,
            steps: u64,
            budget: u64,
        }
        /// `Ok(None)`: out of budget.
        fn dfs<C: Concrete>(cx: &mut Dfs<C>, st: &C::State, k: u32) -> Result<Option<bool>> {
            if k >= cx.frames {
                return Ok(Some(false));
            }
            let mut ran = Covered::default();
            for byte in input_order(cx.prefer, k) {
                if ran.covers(byte) {
                    continue;
                }
                let t = cx.spf * (cx.from + k + 1);
                let (next, read) = cx.c.successors(cx.eng, st, byte, t, &|_| false, &mut cx.steps).map_err(|e| e.context(format!("the frame after {k} frames of the depth-first try, with input {byte}")))?;
                ran.push(byte, read);
                if cx.steps > cx.budget {
                    return Ok(None);
                }
                for n in next {
                    match n {
                        Next::Win(label) => {
                            // A win before the lower bound means a lost path.
                            ensure!(k + 1 == cx.frames, "a {} win at f{} before the bound f{}: a backward lost a path", C::NAME, k + 1, cx.frames);
                            cx.path.push((byte, label));
                            return Ok(Some(true));
                        }
                        Next::State(next, key, label) => {
                            let memo = (key, k + 1);
                            if cx.dead.contains(&memo) {
                                continue;
                            }
                            cx.path.push((byte, label));
                            match dfs(cx, &next, k + 1)? {
                                Some(true) => return Ok(Some(true)),
                                None => return Ok(None),
                                Some(false) => {}
                            }
                            cx.path.pop();
                            cx.dead.insert(memo);
                        }
                    }
                }
            }
            Ok(Some(false))
        }
        let t = std::time::Instant::now();
        let mut eng = engines[0].lock().expect("an engine");
        let mut cx = Dfs { c, eng: &mut eng, from: frames - bound, frames: bound, spf, prefer, dead: FxHashSet::default(), path: Vec::new(), steps: 0, budget: c.budget() };
        let found = dfs(&mut cx, &start, 0)?;
        eprintln!(
            "[{name}] depth-first at the bound f{bound}: {} after {} steps, {:.1} s",
            match found {
                Some(true) => "a win",
                Some(false) => "no win (exhaustive)",
                None => "out of budget",
            },
            cx.steps,
            t.elapsed().as_secs_f64()
        );
        if found == Some(true) {
            let (inputs, labels) = cx.path.iter().copied().unzip::<_, _, Vec<u8>, Vec<C::Label>>();
            return Ok(Some(Path { inputs, labels: std::iter::once(start_label).chain(labels).collect() }));
        }
    }

    // THE BREADTH-FIRST LAYERS. Per layer k >= 1, each state's (parent index
    // in layer k-1, input, label).
    let mut back: Vec<Vec<(u32, u8, C::Label)>> = Vec::new();
    let mut cur: Vec<C::State> = vec![start];
    let mut steps = 0u64;
    /// A successor in (parent, input, leaf) order within its chunk;
    /// `next: None` the win.
    struct Succ<C: Concrete> {
        parent: u32,
        byte: u8,
        label: C::Label,
        next: Option<(C::State, C::Key)>,
    }
    for k in 0..frames {
        let t = std::time::Instant::now();
        let t_w = spf * (k + 1);
        // In chunks of 16 parents, an engine a worker (each takes a free
        // one: as many as workers), successors in chunk order.
        const CHUNK: usize = 16;
        let parts: Vec<(Vec<Succ<C>>, u64)> = crate::par::Pool::new(engines.len()).map_with(
            cur.len().div_ceil(CHUNK),
            || engines.iter().find_map(|e| e.try_lock().ok()).ok_or_else(|| anyhow::anyhow!("no free {name} engine")),
            |eng, ch| {
                let (mut got, mut steps) = (Vec::new(), 0u64);
                // Dedup within the chunk already (the first in order, as the
                // merge keeps it): most successors are repeats.
                let mut chunk_seen: FxHashSet<C::Key> = FxHashSet::default();
                'chunk: for (p, st) in cur.iter().enumerate().skip(ch * CHUNK).take(CHUNK) {
                    let mut ran = Covered::default();
                    for byte in input_order(prefer, k) {
                        if ran.covers(byte) {
                            continue;
                        }
                        let (next, read) = c.successors(eng, st, byte, t_w, &|key| chunk_seen.contains(key), &mut steps).map_err(|e| e.context(format!("the frame after layer {k} with input {byte}")))?;
                        ran.push(byte, read);
                        for n in next {
                            match n {
                                // A win keeps no state, and the chunk ends at
                                // its first: the earliest win (chunks merge in
                                // order) ends the search. Every successor of
                                // the last layer can win - kept whole, room
                                // (4,3) 100% reached 100 GB there.
                                Next::Win(label) => {
                                    got.push(Succ { parent: p as u32, byte, label, next: None });
                                    break 'chunk;
                                }
                                Next::State(next, key, label) => {
                                    if chunk_seen.insert(key.clone()) {
                                        got.push(Succ { parent: p as u32, byte, label, next: Some((next, key)) });
                                    }
                                }
                            }
                        }
                    }
                }
                Ok((got, steps))
            },
        )?;
        steps += parts.iter().map(|p| p.1).sum::<u64>();
        // Each state once, in order; the first win ends the search.
        let mut seen: FxHashSet<C::Key> = FxHashSet::default();
        let (mut next, mut links): (Vec<C::State>, Vec<(u32, u8, C::Label)>) = (Vec::new(), Vec::new());
        let mut won: Option<(u32, u8, C::Label)> = None;
        for s in parts.into_iter().flat_map(|p| p.0) {
            match s.next {
                None => {
                    won = Some((s.parent, s.byte, s.label));
                    break;
                }
                Some((state, key)) => {
                    if seen.insert(key) {
                        next.push(state);
                        links.push((s.parent, s.byte, s.label));
                    }
                }
            }
        }
        eprintln!(
            "[{name}] layer {:3}: {} states -> {} inside W{}{}; {steps} steps so far, {:.1} s, rss {:.1} GB",
            k + 1,
            cur.len(),
            next.len(),
            c.note(&next),
            if won.is_some() { ", A WIN" } else { "" },
            t.elapsed().as_secs_f64(),
            crate::metrics::current_rss_gb()
        );
        if let Some((parent, byte, label)) = won {
            ensure!(k + 1 >= bound, "a {name} win at f{} before the bound f{bound}: a backward lost a path", k + 1);
            // The path, back through the layers.
            let (mut inputs, mut labels) = (vec![byte], vec![label]);
            let mut p = parent;
            for layer in back.iter().rev() {
                let (pp, b, l) = layer[p as usize];
                inputs.push(b);
                labels.push(l);
                p = pp;
            }
            inputs.reverse();
            labels.push(start_label);
            labels.reverse();
            eprintln!("[{name}] a win at f{} after {steps} steps, {:.1} s", inputs.len(), t0.elapsed().as_secs_f64());
            return Ok(Some(Path { inputs, labels }));
        }
        if next.is_empty() {
            println!("NO {} WIN by f{frames}: no state inside W past layer {k} ({steps} steps)", name.to_uppercase());
            return Ok(None);
        }
        back.push(links);
        cur = next;
    }
    println!("NO {} WIN by f{frames} ({steps} steps)", name.to_uppercase());
    Ok(None)
}

/// One arithmetic's level as the concrete searches admit its states: its
/// winning sets, what keys their nodes, its settings, and whether its
/// concrete states key through `lookup_view` (seeded balloons).
pub(super) struct Side<'a, N: Rem> {
    pub w: &'a Winning<N::C>,
    pub node: NodeKeys<'a, N::C>,
    pub keys: &'a KeySpace<N>,
    pub level: Level,
    pub s: &'a Settings,
    pub seeded: bool,
}

impl<N: Rem> Side<'_, N> {
    /// Does `b` win (the room's win, `wins_of`)?
    pub fn wins(&self, b: &Block<N>) -> Result<bool> {
        Ok(wins_of(b.rt2(), self.s)?.iter().any(|&x| x))
    }

    /// Is `b` a node whose W at step `t` holds its exact remainder?
    pub fn inside(&self, b: &Block<N>, t: u32) -> Result<bool> {
        let (shape, nk, cells) = lookup_keys(b, self.level, self.seeded, self.keys)?;
        let Some(i) = self.node.get(shape, nk[0], cells[0]) else { return Ok(false) };
        let q = rem_of::<N>(b.rt2())?;
        Ok(self.w.at(t, i).is_some_and(|r| r.contains(q.0, q.1)))
    }
}

/// The concrete search of one arithmetic's reference engine: a state is
/// its row, its identity its exact state, an edge's label the player's
/// cell after it (the witness's path).
pub(super) struct Single<'s, 'a, N: Rem>(pub &'s Side<'a, N>);

impl<N: Rem> Concrete for Single<'_, '_, N> {
    type Engine = ReferenceEngine<N::A>;
    type State = Rt2<N>;
    type Key = ExactRow;
    type Label = u32;
    const NAME: &'static str = "concrete";

    fn engines(&self, n: usize) -> Result<Vec<Self::Engine>> {
        Ok(concrete_engines::<N>(n, self.0.s)?.0)
    }

    fn start(&self, eng: &mut Self::Engine) -> Result<(Rt2<N>, u32)> {
        let start = Block::canonical(eng.initial()?)?;
        let cell = start.positions()?[0];
        Ok((start.into_rt2(), cell))
    }

    fn successors(&self, eng: &mut Self::Engine, st: &Rt2<N>, byte: u8, t: u32, known: &dyn Fn(&ExactRow) -> bool, steps: &mut u64) -> Result<(Vec<Next<Self>>, u8)> {
        let (succ, read) = eng.frame_reads(st, byte)?;
        let mut out = Vec::new();
        for b in succ {
            *steps += 1;
            let cell = b.positions()?[0];
            if self.0.wins(&b)? {
                out.push(Next::Win(cell));
                continue;
            }
            let exact = exact_state(&b, cell);
            if known(&exact) || !self.0.inside(&b, t)? {
                continue;
            }
            out.push(Next::State(b.into_rt2(), exact, cell));
        }
        Ok((out, read))
    }

    fn budget(&self) -> u64 {
        dfs_budget()
    }
}

/// A concrete witness: the inputs, and the player's cell after each (the
/// start's first).
pub struct Witness {
    pub inputs: Vec<u8>,
    pub cells: Vec<u32>,
}

impl Witness {
    /// The inputs as a comma list (`rewrite run -i`, `pico8_diff/replay.py`).
    pub fn inputs_text(&self) -> String {
        self.inputs.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")
    }

    /// `witness.txt`: `inputs ...`, then `f x y` per frame (frame 0 the start).
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        let mut text = format!("inputs {}\n", self.inputs_text());
        for (f, &c) in self.cells.iter().enumerate() {
            match super::pos_graph::cell_xy(c) {
                Some((x, y)) => text += &format!("{f} {x} {y}\n"),
                None => text += &format!("{f} - -\n"),
            }
        }
        Ok(std::fs::write(path, text)?)
    }
}

/// THE CONCRETE SEARCH of one level (`search` over `Single`): `horizon` in
/// search steps (W's index), `bound` the arc optimum in frames.
pub(super) fn concrete_search<N: Rem>(side: &Side<'_, N>, horizon: u32, bound: u32, prefer: Option<&[u8]>) -> Result<Option<Witness>> {
    let (s, spf) = (side.s, side.s.steps_per_frame());
    ensure!(horizon % spf == 0, "the horizon {horizon} is not a whole number of frames ({spf} steps each)");
    eprintln!("[concrete] {} nodes; {} workers; the arc bound f{bound}", side.w.len(), s.threads().max(1));
    let path = search(&Single(side), horizon / spf, bound, spf, prefer, s.threads())?;
    Ok(path.map(|p| Witness { inputs: p.inputs, cells: p.labels }))
}

/// W's nodes by `(shape, key, cell)`, for the concrete search and the known
/// route: a node IS its state - its region (the shape's index and the
/// cell's slot), its key and its cell - so only the tree's shapes are
/// listed.
pub struct NodeKeys<'w, C: Circle = super::arcs::Fixed16> {
    w: &'w Winning<C>,
    geo: crate::storage::Geometry,
    /// Shape hash -> shape index, sorted.
    shapes: Vec<(u64, u32)>,
}

impl<'w, C: Circle> NodeKeys<'w, C> {
    /// Over `w`, the tree's shapes by index.
    pub fn new(w: &'w Winning<C>, shapes: &[u64]) -> Result<Self> {
        let mut by: Vec<(u64, u32)> = shapes.iter().enumerate().map(|(i, &h)| (h, i as u32)).collect();
        by.sort_unstable();
        ensure!(by.windows(2).all(|p| p[0].0 != p[1].0), "a shape with two indices");
        Ok(NodeKeys { w, geo: GEOMETRY, shapes: by })
    }
    pub(super) fn get(&self, shape: u64, key: Option<(u64, u64)>, cell: u32) -> Option<u32> {
        let key = key?;
        let k = self.shapes.binary_search_by_key(&shape, |s| s.0).ok()?;
        self.w.index(Sk::at(&self.geo, self.shapes[k].1, key, cell))
    }
}

/// A concrete state's node key at `level` (`lookup_view`, widened) in the
/// tree's key space (`None`: a code the tree never stored - no node).
pub(super) fn lookup_keys<N: EngineNum>(b: &Block<N>, level: Level, seeded: bool, keys: &KeySpace<N>) -> Result<(u64, Vec<Option<(u64, u64)>>, Vec<u32>)> {
    if !seeded {
        return crate::frame::widened_keys(b, level, keys);
    }
    crate::frame::widened_keys_rt2(&lookup_view(b.rt2(), seeded)?, level, keys)
}

/// A concrete state's EXACT identity, UNWIDENED: its canonical bytes
/// (`exact::exact_rows`) and its cell. Never the level's widened key:
/// states sharing it need not share their fate (`522de36`).
pub(super) fn exact_state<N: EngineNum>(b: &Block<N>, cell: u32) -> ExactRow {
    let mut v = celeste_engine::exact::exact_rows(b.rt2(), crate::compiled::ids()).swap_remove(0).into_vec();
    v.extend_from_slice(&cell.to_le_bytes());
    v.into_boxed_slice()
}

/// A concrete state as the tree holds it before the level's widening. With
/// seeded balloons (`concrete_search`), the state holds each balloon's
/// phase as one number and its `y` as one point, where the tree - built
/// with the phase an interval - holds the canonical phase `[0, 1)` and the
/// bob `y` `[start - 2, start + 2]` (the forward recomputes `y` from the
/// interval phase every frame); the view maps them there, so its key is the
/// node that over-approximates the state.
pub(super) fn lookup_view<N: EngineNum>(row: &Rt2<N>, seeded: bool) -> Result<Rt2<N>> {
    use celeste_engine::runtime2::{Col, AV};
    use celeste_engine::widening::{BALLOON_BOB_RAW, BALLOON_PERIOD_RAW};
    let mut rt2 = row.clone_block();
    if !seeded {
        return Ok(rt2);
    }
    let ids = crate::compiled::ids();
    let num = <N::A as Arithmetic>::from_fixed_raw;
    for obj in rt2.objects_of_type(ids, ids.g_balloon) {
        let (Some(oc), Some(yc), Some(sc)) = (rt2.obj_field_cell(obj, ids.f_offset), rt2.obj_field_cell(obj, ids.f_y), rt2.obj_field_cell(obj, ids.f_start)) else { continue };
        let AV::Num(start) = rt2.cols[sc as usize].at(0) else { anyhow::bail!("a balloon's `start` is not a number") };
        let bob = |f: fn(N, N) -> Option<N>| f(start, num(BALLOON_BOB_RAW)).ok_or_else(|| anyhow::anyhow!("a balloon's bob out of range"));
        rt2.cols[oc as usize] = Col::U(AV::Ival(num(0), num(BALLOON_PERIOD_RAW)));
        rt2.cols[yc as usize] = Col::U(AV::Ival(bob(<N::A as Arithmetic>::sub)?, bob(<N::A as Arithmetic>::add)?));
    }
    Ok(rt2)
}

/// A concrete state's remainder as a point of the torus (no player: the
/// point of 0 on both axes).
pub fn rem_of<N: Rem>(rt2: &Rt2<N>) -> Result<(Point<N>, Point<N>)> {
    use celeste_engine::runtime2::AV;
    let ids = crate::compiled::ids();
    let zero = || N::point(<N::A as Arithmetic>::from_i16(0)).expect("0 is a remainder");
    let Some((cx, cy)) = rt2.player_xy_cells(ids, ids.f_rem) else { return Ok((zero(), zero())) };
    let at = |c: usize| -> Result<Point<N>> {
        match rt2.cols[c].at(0) {
            AV::Num(n) => N::point(n).ok_or_else(|| anyhow::anyhow!("a remainder {n:?} outside [-1/2, 1/2)")),
            other => anyhow::bail!("a concrete remainder expected, got {other:?}"),
        }
    };
    Ok((at(cx)?, at(cy)?))
}

/// `n` reference engines for concrete steps, and whether they run seeded
/// (`Settings::concrete`): `--concrete-balloon-seeds` (a tasdatabase file's
/// seeds), or a float tool's own seeds, fix the balloons' phases, so a
/// concrete run is one real run under them - the tree and W keep the phase
/// an interval (sound for every seed), and a concrete state's key maps its
/// phase to that interval (`lookup_view`). Built under the run's settings
/// `run`: `_init` runs under them.
pub(super) fn concrete_engines<N: Rem>(n: usize, run: &Settings) -> Result<(Vec<ReferenceEngine<N::A>>, bool)> {
    let (s, seeded) = run.concrete();
    let engines = (0..n).map(|_| ReferenceEngine::<N::A>::new(&s)).collect::<Result<Vec<_>>>()?;
    Ok((engines, seeded))
}

/// The concrete steps (reference-engine successors) the depth-first try at
/// the bound may take before it gives up (`concrete_search`; the dual
/// search's pairs count both halves' leaves: twice this).
const DFS_BUDGET: u64 = 200_000;

/// TESTS ONLY: no depth-first try (a budget of 0), so the breadth-first
/// layers find the witness - the gates' witnesses all come from the try at
/// the bound (`driver`'s breadth-first test). Process-global: nextest runs
/// each test in its own process.
#[cfg(test)]
pub(super) static NO_DFS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// The depth-first try's budget (`DFS_BUDGET`; 0 under `NO_DFS`).
pub(super) fn dfs_budget() -> u64 {
    #[cfg(test)]
    if NO_DFS.load(std::sync::atomic::Ordering::Relaxed) {
        return 0;
    }
    DFS_BUDGET
}

/// The inputs a state has been stepped under, each with the buttons its
/// frame read (`RefEngine::frame_reads`). An input that agrees with one of
/// them on every button that one read runs the same frame - the same reads,
/// the same values, the same successors (the output writes no button) - so
/// its successors are repeats of states already emitted in (parent, input)
/// order, which the search's exact dedup would drop: skipping it changes
/// nothing but the step count.
#[derive(Default)]
pub(super) struct Covered(Vec<(u8, u8)>);

impl Covered {
    pub(super) fn covers(&self, byte: u8) -> bool {
        self.0.iter().any(|&(b, read)| (b ^ byte) & read == 0)
    }

    pub(super) fn push(&mut self, byte: u8, read: u8) {
        self.0.push((byte, read));
    }
}

/// The 64 inputs in the order the concrete search tries them at the frame
/// after `k`: `prefer[k]` first (`rewrite search --prefer`: a known TAS, so
/// the witness follows it wherever an optimal route allows), then the rest.
/// Every input is still tried: the order only picks WHICH optimal witness.
pub(super) fn input_order(prefer: Option<&[u8]>, k: u32) -> impl Iterator<Item = u8> {
    let first = prefer.and_then(|p| p.get(k as usize).copied()).filter(|&b| b < 64);
    first.into_iter().chain((0u8..64).filter(move |&b| Some(b) != first))
}
