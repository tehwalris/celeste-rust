//! The frame-step interface, the block, and the minimal outer loop
//! (plans/architecture.md).
//!
//! Rebuilt core, written fresh against the agreed interfaces rather than copied
//! from `search/run.rs`. A block is OPAQUE to the loop except for two exposed
//! columns - its KEYS and its POSITIONS; the only other thing that crosses is
//! the frame-step call.
//!
//! Impls (kernel runner + `trace::refengine`) and the loop's checkpoint /
//! position-graph growth come next; this is the interface + the frame spine.

use anyhow::Result;
use std::ops::Range;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::interpreter::state::State;
use celeste_engine::runtime2::Rt2;

/// The data currency (interface #2). A columnar batch of lanes - the engine's
/// own block (`Rt2`: one shape, one column per value cell, the key column the
/// boundary computed), carried as-is. Two columns are EXPOSED - the per-lane
/// key and the per-lane position - and nothing else; the field data stays
/// inside. A whole block serializes compactly (the lanes share structure), so
/// the loop always moves blocks, never individual lanes.
///
/// It IS the kernel's block, so there is no bridge in the loop: the frame
/// step consumes and produces it directly, `keep` is a column filter, the
/// regroup is a column append, and the checkpoint is the columns. The
/// interpreter `State` appears only at the edges - the initial state, the
/// reference engine, and the ladder filter's coarsening - through
/// `from_state` / `to_state`.
pub struct Block {
    rt2: Rt2,
}

impl Block {
    /// Wrap an engine block. Its key column must be present: every producer
    /// (the kernel boundary, `from_state`, the checkpoint loader) attaches
    /// it, and nothing downstream re-hashes.
    pub fn from_rt2(rt2: Rt2) -> Self {
        assert_eq!(
            rt2.row_keys.len(),
            rt2.width,
            "block without its key column ({} keys for {} lanes)",
            rt2.row_keys.len(),
            rt2.width
        );
        Block { rt2 }
    }

    /// A block from a reference-interpreter `State`, keyed by the one
    /// canonical rule (`Rt2::row_keys_canonical`: what `engine_row_keys`
    /// computed). The state must already be at its rung's abstraction.
    pub fn from_state(state: &State) -> Result<Self> {
        let (cart, cache) = crate::compiled::room_context()?;
        let mut rt2 = crate::compiled::bridge::import_block(state, cart, cache);
        rt2.row_keys_canonical();
        Ok(Block { rt2 })
    }

    /// The block as an interpreter `State` (`bridge::export_block`), for the
    /// reference engine and the ladder filter's coarsening.
    pub fn to_state(&self) -> State {
        crate::compiled::bridge::export_block(&self.rt2)
    }

    pub fn into_rt2(self) -> Rt2 {
        self.rt2
    }

    pub fn rt2(&self) -> &Rt2 {
        &self.rt2
    }

    /// The key column: the 128-bit canonical row key per lane (shape + content).
    /// Identity for dedup, the visited set, and checkpoints. One entry per lane.
    pub fn keys(&self) -> &[(u64, u64)] {
        &self.rt2.row_keys
    }

    /// The position column: the player-position cell per lane, for grouping and
    /// the position graph. Same length as `keys`.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::block_cells(&self.rt2)
    }

    /// Per lane: does it sit on the room's win target (`wins_of`)?
    pub fn wins(&self) -> Result<Vec<bool>> {
        wins_of(&self.rt2)
    }

    /// Keep only the lanes whose mask entry is true, as a new block; `None` if
    /// none survive. The one splitting primitive the loop needs (dedup at the
    /// door repacks with this).
    pub fn keep(mut self, mask: &[bool]) -> Option<Block> {
        let keep: Vec<u32> = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &m)| m.then_some(i as u32))
            .collect();
        if keep.is_empty() {
            return None;
        }
        self.rt2.retain_lanes(&keep);
        Some(self)
    }

    /// The block's shard shape. A block is one shape.
    pub fn shard_shape(&self) -> u64 {
        self.rt2.shape_hash
    }

    /// Number of lanes in this block.
    pub fn lanes(&self) -> usize {
        self.rt2.width
    }
}

/// Per lane: does it sit on the room's win target? The room-exit test
/// (`room.x` past the start room) or, under `CELESTE_WIN_AT_XY`, the
/// synthetic player-position target. Pure position; no peeking inside.
pub fn wins_of(rt2: &Rt2) -> Result<Vec<bool>> {
    use celeste_engine::runtime2::{Col, AV};
    let ids = crate::compiled::ids();
    let lanes = rt2.width;
    if let Some(target) = crate::interpreter::abstraction::synthetic_win_xy() {
        let Some(obj) = crate::search::pos_graph::player_object(rt2) else {
            return Ok(vec![false; lanes]);
        };
        let axis = |f: u32| {
            rt2.obj_field_cell(obj, f)
                .and_then(|c| crate::search::pos_graph::whole_i16_col(rt2, c))
        };
        return Ok(match (axis(ids.f_x), axis(ids.f_y)) {
            (Some(xs), Some(ys)) => {
                xs.iter().zip(&ys).map(|(&x, &y)| (x, y) == target).collect()
            }
            _ => vec![false; lanes],
        });
    }
    let want = crate::pico8_num::Pico8Num::from_i16(crate::game_runner::win_room_x());
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow::anyhow!("wins: no `room` global"))?;
    let x = rt2
        .obj_field_cell(room, ids.f_x)
        .ok_or_else(|| anyhow::anyhow!("wins: room table has no x field"))?;
    Ok(match &rt2.cols[x as usize] {
        Col::U(AV::Num(n)) => vec![*n == want; lanes],
        Col::N(vs) => vs.iter().map(|n| *n == want).collect(),
        Col::V(vs) => vs.iter().map(|v| *v == AV::Num(want)).collect(),
        other => anyhow::bail!("wins: room.x is not a number column: {:?}", other),
    })
}

/// Worker (and owner) count: `CELESTE_THREADS`, else half the logical
/// CPUs - one per physical core; the kernels are AVX-512 bound and SMT
/// siblings share those units - and at least 1.
pub fn threads() -> usize {
    static N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *N.get_or_init(|| {
        std::env::var("CELESTE_THREADS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&n| n >= 1)
            .unwrap_or_else(|| {
                (std::thread::available_parallelism().map(|n| n.get()).unwrap_or(2) / 2).max(1)
            })
    })
}

/// The OWNER of a `(shape, cell)`: the worker whose visited shard, output
/// pieces and checkpoint files hold every row at that shape and cell. A
/// hash of the cell (not a range) for balance; the two-tier visited set
/// keeps the per-cell locality inside the owner.
pub fn owner_of(owners: u32, shape: u64, cell: u32) -> u32 {
    let h = celeste_engine::runtime2::mix64(shape ^ (cell as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15));
    (h % owners as u64) as u32
}

/// Rows emitted for one owner at one output shape: a packed block (its key
/// column filled as rows land) and the rows' cells.
pub struct Slot {
    pub rt2: Rt2,
    pub cells: Vec<u32>,
}

/// What a frame step EMITS INTO (interface #1's output side): one worker's
/// emission state for a frame. The step consumes provenance at emission -
/// it knows which input row each output row came from - and reports
/// through this: the pos-graph edges `(input cell, output cell)` of every
/// raw output, and the output rows themselves, keyed and PARTITIONED BY
/// OWNER (`owner_of` of their shape and cell). Nothing here is shared: the
/// door dedup happens afterwards, owner by owner (`forward_frame`'s second
/// phase), against that owner's private visited shard. The step's own
/// within-call dedup is the only dedup at emission.
pub struct ForwardSink<'a> {
    pub owners: u32,
    /// `(owner, output shape)` -> the rows emitted for it.
    pub slots: rustc_hash::FxHashMap<(u32, u64), Slot>,
    /// Record `edges` at all (off for the backward, which has the graph).
    pub edges_on: bool,
    /// The distinct edges this worker produced (a set: a block's rows fan
    /// out into a few hundred distinct cell pairs, not one per row).
    pub edges: rustc_hash::FxHashSet<(u32, u32)>,
    /// Rows emitted after the step's within-call dedup (the raw fan-out).
    pub emitted: u64,
    /// BACKWARD MODE: the marked `(shape, content, cell)` set at frame i+1.
    /// When set, the step materializes NOTHING; an emitted row that is in
    /// the set marks its INPUT row (`hit`). Provenance consumed at
    /// emission, as in the forward - the step's within-call dedup carries
    /// "hit" as its tag so a re-emission from another input row marks that
    /// row too.
    pub targets: Option<&'a rustc_hash::FxHashSet<(u64, u64, u32)>>,
    /// Backward mode: one flag per input row of the lane range being run.
    hits: Vec<bool>,
    hit_base: usize,
}

impl<'a> ForwardSink<'a> {
    pub fn new(owners: u32, edges_on: bool) -> Self {
        ForwardSink {
            owners,
            slots: Default::default(),
            edges_on,
            edges: Default::default(),
            emitted: 0,
            targets: None,
            hits: Vec::new(),
            hit_base: 0,
        }
    }

    /// The backward's sink for the candidate rows `lanes` of a block.
    pub fn backward(targets: &'a rustc_hash::FxHashSet<(u64, u64, u32)>, lanes: Range<usize>) -> Self {
        let mut s = Self::new(1, false);
        s.targets = Some(targets);
        s.hits = vec![false; lanes.len()];
        s.hit_base = lanes.start;
        s
    }

    /// Backward mode: input row `lane` (a block lane index) reaches a target.
    pub fn hit(&mut self, lane: usize) {
        self.hits[lane - self.hit_base] = true;
    }

    /// Backward mode: the hit flags, one per lane of the range run.
    pub fn hits(&self) -> &[bool] {
        &self.hits
    }

    /// The slot for `(owner, shape)`, created from `init` on first use.
    pub fn slot(&mut self, owner: u32, shape: u64, init: impl FnOnce() -> Rt2) -> &mut Slot {
        self.slots
            .entry((owner, shape))
            .or_insert_with(|| Slot { rt2: init(), cells: Vec::new() })
    }

    /// Emit one materialized single-row block at `cell` (the reference
    /// engine's path; the kernels push fields into the slot directly).
    pub fn emit_row(&mut self, row: &Rt2, cell: u32) {
        debug_assert_eq!(row.width, 1);
        debug_assert_eq!(row.row_keys.len(), 1, "an emitted row carries its key");
        let owner = owner_of(self.owners, row.shape_hash, cell);
        let slot = self.slot(owner, row.shape_hash, || {
            let mut b = celeste_engine::slots::reshape(row, 0);
            b.shape_hash = row.shape_hash;
            b
        });
        slot.rt2.append_rows(row, &[0]);
        slot.cells.push(cell);
        self.emitted += 1;
    }
}

/// The ladder's forward discard-filter. A state generated at precision r+1 is
/// KEPT only if its widened-to-precision-r form was marked by the previous
/// level's backward pass - "immediately, during the forward, the whole time".
/// The marked set is coarse, so this narrows every finer level to the coarse
/// winning envelope. This is what replaces the e/g/band numbering as the
/// cross-precision link: a set membership, not a distance threshold.
pub struct MarkFilter<'a> {
    /// The marked set from the previous, COARSER precision level.
    marked: &'a Marks,
    /// The coarser precision to widen down to before the membership test.
    coarser: crate::interpreter::abstraction::RemPrecision,
}

impl<'a> MarkFilter<'a> {
    pub fn new(
        marked: &'a Marks,
        coarser: crate::interpreter::abstraction::RemPrecision,
    ) -> Self {
        Self { marked, coarser }
    }

    /// Per-lane: keep lane `i` iff its widened-to-coarser form was marked.
    /// Rem widening never moves the integer cell and (coarsening) never splits
    /// a lane, so the widened block is lane-aligned with the input.
    ///
    /// Crosses to the interpreter `State` for the coarsening: the widenings
    /// live there (`abstraction.rs`) and this is the one place the loop
    /// still needs them. Paid only at levels >= 1, whose frontiers the
    /// filter itself keeps small.
    pub fn allowed(&self, rt2: &Rt2) -> Result<Vec<bool>> {
        let (shape, keys, cells) = widened_keys_rt2(rt2, self.coarser)?;
        Ok(keys
            .iter()
            .zip(&cells)
            .map(|(k, &c)| self.marked.contains(shape, *k, c))
            .collect())
    }
}

/// Each lane's `(key, cell)` as the `coarser` level keys it - what
/// `MarkFilter` looks up. At `Bits(k)` that is the COMPLETE coarsening the
/// level's kernels bake into their rows (`Rt2::widen_to`: rem bucketing,
/// the dash clamp, the fruit widening, the timer pins), then the canonical
/// key - on the block's columns, lane for lane. At `Exact` it is the plain
/// canonical key: the Exact kernel set (`WalkOpts::EXACT`) widens NOTHING,
/// its rows are the concrete state, timers included. Public for the ladder
/// diagnostics and the witness extraction.
pub fn widened_keys(
    block: &Block,
    coarser: crate::interpreter::abstraction::RemPrecision,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    widened_keys_rt2(&block.rt2, coarser)
}

/// `(shape, keys, cells)` of the widened rows - the shape is the widened
/// block's, which is what the coarser level's marks are sharded by.
pub fn widened_keys_rt2(
    rt2: &Rt2,
    coarser: crate::interpreter::abstraction::RemPrecision,
) -> Result<(u64, Vec<(u64, u64)>, Vec<u32>)> {
    use crate::interpreter::abstraction::RemPrecision;
    let mut w = rt2.clone_block();
    if let RemPrecision::Bits(b) = coarser {
        w.widen_to(crate::compiled::ids(), b);
    }
    let keys = w.row_keys_canonical();
    let cells = crate::search::pos_graph::block_cells(&w)?;
    anyhow::ensure!(
        keys.len() == rt2.width && cells.len() == rt2.width,
        "the widening changed the lane count"
    );
    Ok((w.shape_hash, keys, cells))
}

/// The one interface between the outer loop and the engines (interface #1).
/// `run` takes a block of input lanes and returns every output block - all
/// internal branching enumerated, already widened for the engine's precision,
/// each output block's key/position columns already computable. Branching,
/// widening, and keying happen INSIDE.
///
/// Two implementations behind this one trait: the compiled kernels (fast) and
/// the scalar interpreter (`trace::refengine`, the trusted reference). The loop
/// holds a `&dyn FrameStep` and never knows which.
pub trait FrameStep: Sync {
    /// One frame of lanes `lanes` of `block` (whose per-lane cells are
    /// `cell_in`), emitted into `sink`. Called from many workers at once on
    /// disjoint lane ranges: an implementation keeps its scratch in the
    /// sink or behind a lock.
    fn run(&self, block: &Block, cell_in: &[u32], lanes: Range<usize>, sink: &mut ForwardSink) -> Result<()>;
}

/// Lanes per unit of the emit phase for `lanes` lanes over `workers`: the
/// kernel runs 16-lane slices, and the step's within-call dedup (`seen`)
/// only sees one unit, so bigger units re-emit fewer rows to the owners;
/// ~16 units per worker keeps them balanced. Clamped to [2048, 16384].
fn unit_lanes(lanes: usize, workers: usize) -> usize {
    (lanes / (workers * 16)).clamp(2048, 16384)
}

/// Cut `blocks` (by lane count) into `(block, lo, hi)` units.
fn units_of(lanes: impl Iterator<Item = usize>, unit: usize) -> Vec<(usize, usize, usize)> {
    let mut units = Vec::new();
    for (bi, n) in lanes.enumerate() {
        let mut lo = 0;
        while lo < n {
            let hi = (lo + unit).min(n);
            units.push((bi, lo, hi));
            lo = hi;
        }
    }
    units
}

/// One forward frame, in two parallel phases with one barrier between
/// them (plans/parallel.md):
///
/// 1. EMIT, partitioned by input: units of `UNIT_LANES` lanes of the
///    frontier's blocks, pulled by `threads()` workers. Each worker runs
///    the frame step on its units into its own `ForwardSink`, which sorts
///    every emitted row into the slot of its OWNER (`owner_of` of the
///    row's shape and cell). No shared state is touched.
/// 2. OWN, partitioned by output: worker `o` walks every sink's slots
///    addressed to it, applies the ladder filter, dedups at the door
///    against ITS OWN visited shard, and appends the survivors into its
///    own next-frame pieces (one per shape). No shared state, no locks; the
///    door sees each `(shape, cell)` on exactly one thread.
///
/// The next frontier is the union of the owners' pieces (up to
/// `threads()` blocks per shape); the pos-graph sees every raw output's
/// edge. Rows emitted twice by different units are both handed to the
/// owner and the door keeps the first - the same set as one call would.
pub fn forward_frame(
    engine: &dyn FrameStep,
    frontier: Vec<Block>,
    visited: &mut [Visited],
    pos: Option<&crate::search::pos_graph::PosObserver>,
    filter: Option<&MarkFilter>,
) -> Result<(Vec<Block>, bool, FrameStats)> {
    use std::time::Instant;
    let mut st = FrameStats::default();
    let owners = visited.len() as u32;
    let workers = threads();
    st.blocks_in = frontier.len();
    st.lanes_in = frontier.iter().map(Block::lanes).sum();

    // Phase 1: emit.
    let t = Instant::now();
    let cells: Vec<Vec<u32>> = frontier.iter().map(Block::positions).collect::<Result<_>>()?;
    let units = units_of(frontier.iter().map(Block::lanes), unit_lanes(st.lanes_in, workers));
    let next_unit = AtomicUsize::new(0);
    let sinks: Vec<ForwardSink> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers)
            .map(|_| {
                let (frontier, cells, units, next_unit) = (&frontier, &cells, &units, &next_unit);
                scope.spawn(move || -> Result<ForwardSink> {
                    let mut sink = ForwardSink::new(owners, pos.is_some());
                    loop {
                        let u = next_unit.fetch_add(1, Ordering::Relaxed);
                        let Some(&(bi, lo, hi)) = units.get(u) else { break };
                        engine.run(&frontier[bi], &cells[bi], lo..hi, &mut sink)?;
                    }
                    Ok(sink)
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("emit worker panicked"))
            .collect::<Result<Vec<_>>>()
    })?;
    st.t_emit = t.elapsed();
    st.lanes_raw = sinks.iter().map(|s| s.emitted as usize).sum();
    if let Some(p) = pos {
        for sink in &sinks {
            p.record_pairs(sink.edges.iter().copied());
        }
    }

    // Phase 2: own.
    let t = Instant::now();
    let owned: Vec<(Vec<Rt2>, usize, bool)> = std::thread::scope(|scope| {
        let handles: Vec<_> = visited
            .iter_mut()
            .enumerate()
            .map(|(o, vis)| {
                let sinks = &sinks;
                scope.spawn(move || -> Result<(Vec<Rt2>, usize, bool)> {
                    let mut pieces: rustc_hash::FxHashMap<u64, Rt2> = Default::default();
                    let (mut kept_n, mut won) = (0usize, false);
                    for sink in sinks {
                        for (&(owner, shape), slot) in &sink.slots {
                            if owner != o as u32 {
                                continue;
                            }
                            crate::compiled::asm_kernel::key_check(&slot.rt2);
                            // Ladder filter (coarser level's marked set), then
                            // the door: `&&` short-circuits, so a filtered-out
                            // row is never inserted into `visited`.
                            let allow = match filter {
                                Some(f) => Some(f.allowed(&slot.rt2)?),
                                None => None,
                            };
                            // The door, one shard lookup per run of rows at
                            // the same cell (a slice's lanes are spatially
                            // sorted, so runs are common).
                            let mut rows: Vec<u32> = Vec::new();
                            let (keys, cells) = (&slot.rt2.row_keys, &slot.cells);
                            let mut i = 0;
                            while i < keys.len() {
                                let cell = cells[i];
                                let mut j = i;
                                while j < keys.len() && cells[j] == cell {
                                    j += 1;
                                }
                                let shard = vis.shard_mut(shape, cell);
                                for r in i..j {
                                    if allow.as_ref().is_none_or(|a| a[r]) && shard.insert(keys[r]) {
                                        rows.push(r as u32);
                                    }
                                }
                                i = j;
                            }
                            if rows.is_empty() {
                                continue;
                            }
                            kept_n += rows.len();
                            let wins = wins_of(&slot.rt2)?;
                            won |= rows.iter().any(|&r| wins[r as usize]);
                            let piece = pieces.entry(shape).or_insert_with(|| {
                                let mut b = celeste_engine::slots::reshape(&slot.rt2, 0);
                                b.shape_hash = shape;
                                b
                            });
                            piece.append_rows(&slot.rt2, &rows);
                        }
                    }
                    Ok((pieces.into_values().collect(), kept_n, won))
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("owner worker panicked"))
            .collect::<Result<Vec<_>>>()
    })?;
    st.t_own = t.elapsed();
    let mut won = false;
    let mut next: Vec<Block> = Vec::new();
    for (pieces, kept, w) in owned {
        st.lanes_kept += kept;
        won |= w;
        next.extend(pieces.into_iter().map(Block::from_rt2));
    }
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    Ok((next, won, st))
}

/// Where one forward frame's time went and what it moved, for the
/// per-frame log line and the phase totals.
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    pub blocks_in: usize,
    pub lanes_in: usize,
    /// Output lanes as the workers emitted them, before the filter/door.
    pub lanes_raw: usize,
    /// Lanes that passed the filter and were new to `visited`.
    pub lanes_kept: usize,
    pub blocks_out: usize,
    pub lanes_out: usize,
    /// The emit phase (kernel calls, partitioned by input), wall.
    pub t_emit: std::time::Duration,
    /// The own phase (filter, door, append, partitioned by owner), wall.
    pub t_own: std::time::Duration,
}

/// The result of a backward pass: the marked set plus how many row re-runs
/// it cost - the number to compare against the forward pass (backward must
/// not exceed it; if it does, the narrowing is broken).
pub struct BackwardResult {
    pub marked: Marks,
    pub reruns: u64,
}

/// A backward pass's marked set: every state that can reach a win by the
/// horizon, with its DISTANCE - the length of the shortest path from it to
/// a win (0 = a win itself). Sharded like `Visited`. The distance is what
/// makes the set INCREMENTAL across horizons (`backward_walk`): a state's
/// distance does not depend on the horizon, only on whether it is marked
/// at all, so the marks of horizon H are a subset of those of H+1 and a
/// run at H+1 starts from them.
#[derive(Default, Clone)]
pub struct Marks {
    /// `(shape, cell)` -> key -> `(dist, generation)`; the generation is
    /// the run that last set the distance.
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashMap<(u64, u64), (u32, u32)>>,
    /// Marks by distance, for the targets of an iteration. May hold a
    /// state at a distance it has since improved on; `at_dist` filters.
    by_dist: Vec<Vec<(u64, (u64, u64), u32)>>,
    /// The current run.
    generation: u32,
}

impl Marks {
    pub fn new() -> Self {
        Self::default()
    }
    /// Start a run: marks set from now on carry a new generation.
    pub fn next_generation(&mut self) {
        self.generation += 1;
    }
    /// Mark at `dist`, if the state is new or `dist` is shorter than its
    /// current distance. True if it changed.
    pub fn mark(&mut self, shape: u64, key: (u64, u64), cell: u32, dist: u32) -> bool {
        let e = self.shards.entry((shape, cell)).or_default().entry(key).or_insert((u32::MAX, 0));
        if e.0 <= dist {
            return false;
        }
        *e = (dist, self.generation);
        if self.by_dist.len() <= dist as usize {
            self.by_dist.resize(dist as usize + 1, Vec::new());
        }
        self.by_dist[dist as usize].push((shape, key, cell));
        true
    }
    pub fn contains(&self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.dist_of(shape, key, cell).is_some()
    }
    pub fn dist_of(&self, shape: u64, key: (u64, u64), cell: u32) -> Option<u32> {
        self.shards.get(&(shape, cell)).and_then(|s| s.get(&key)).map(|e| e.0)
    }
    /// The marks at exactly `dist`, split into (set by an earlier run,
    /// set by the current run).
    fn at_dist(&self, dist: u32) -> (Vec<((u64, u64), u32)>, Vec<((u64, u64), u32)>) {
        let (mut old, mut new) = (Vec::new(), Vec::new());
        if let Some(v) = self.by_dist.get(dist as usize) {
            for &(shape, key, cell) in v {
                let Some(&(d, g)) = self.shards.get(&(shape, cell)).and_then(|s| s.get(&key)) else {
                    continue;
                };
                if d != dist {
                    continue; // improved on since
                }
                if g == self.generation {
                    new.push((key, cell));
                } else {
                    old.push((key, cell));
                }
            }
        }
        (old, new)
    }
    pub fn len(&self) -> usize {
        self.shards.values().map(|s| s.len()).sum()
    }
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
    /// `(entries, order-independent hash of the (content, cell) set)` - the
    /// gate that two backward passes marked the same states. Distances and
    /// sharding are not part of it.
    pub fn fingerprint(&self) -> (usize, u64) {
        use celeste_engine::runtime2::mix64;
        let mut acc = 0u64;
        for ((_shape, cell), keys) in &self.shards {
            for &(k0, k1) in keys.keys() {
                acc = acc.wrapping_add(mix64(k0 ^ mix64(k1 ^ (*cell as u64) << 1)));
            }
        }
        (self.len(), acc)
    }
    /// Persist as `(shape, cell, key, dist)` rows.
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        let mut v: Vec<(u64, u32, u64, u64, u32)> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            v.extend(keys.iter().map(|(&(k0, k1), &(d, _))| (*shape, *cell, k0, k1, d)));
        }
        crate::search::checkpoint::save_value_to(path, &v)
    }
    pub fn load(path: &std::path::Path) -> Result<Self> {
        let v: Vec<(u64, u32, u64, u64, u32)> = crate::search::checkpoint::load_value_from(path)?;
        let mut out = Self::new();
        for (shape, cell, k0, k1, d) in v {
            out.mark(shape, (k0, k1), cell, d);
        }
        Ok(out)
    }
}

/// Backward marking - SINGLE PASS, position-narrowed (plans/architecture.md
/// sentence 8; the horizon-anchored minimal form, no distance DP). The
/// marked set is "the states that can reach a win by `horizon`": the
/// filter the next precision level needs at that horizon.
///
/// It is a reverse BFS from ALL the win states, `horizon - 1` steps deep,
/// with the frame as the anchor. The forward dedups across frames, so
/// checkpoint f holds the states FIRST reached at f - a BFS distance
/// layer, not "the states at frame f" - and a state reached at or before
/// frame i may be at frame i. So: the initial targets are the wins of
/// EVERY layer <= horizon; at iteration i the candidates are the rows of
/// every layer <= i (narrowed to the cells that can step into a target)
/// not yet marked at distance <= horizon - i, re-run against the marks
/// at distance horizon - i - 1. A state marked at iteration i lies in a
/// layer <= i and is `horizon - i` steps from a win: a path of length <=
/// horizon. Complete for the same reason: a state that wins by the horizon
/// from frame f >= its layer has a marked successor at the right
/// iteration. (Two earlier forms of this walk were wrong: candidates from
/// layer i only, and wins injected at their own layer's iteration - each
/// missed every path link whose other end was first reached earlier, and
/// refuted achievable horizons.)
///
/// INCREMENTAL over `prev`, the marks of an earlier horizon on the same
/// tree (level 0 persists across horizons): a mark's distance does not
/// depend on the horizon, so `prev`'s marks are all still marks, and the
/// only pairs (candidate, target) an earlier run has not already tested
/// are (a) rows of the NEWLY admitted layer i against the old targets at
/// distance `horizon - i - 1`, and (b) rows of every layer <= i against
/// the targets this run marked (fresh, or an old mark now at a shorter
/// distance). Everything else was tested with the same outcome. Without
/// `prev` this is the from-scratch walk.
///
/// The re-run is WIDE: every candidate row goes through the frame step in
/// backward mode (`ForwardSink::backward`) - the step materializes nothing
/// and reports, per input row, whether any of its outputs is a target
/// (provenance consumed at emission, plans/buckets.md). Marks are matched
/// by key.
pub fn backward_run(
    engine: &dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
    prev: Option<Marks>,
) -> Result<BackwardResult> {
    // Seeds: the win states of every layer 1..=horizon (listed in each
    // checkpoint file's header; nothing is decoded).
    let mut seeds: Vec<(u64, (u64, u64), u32)> = Vec::new();
    for f in 1..=horizon {
        for file in frame_files(dir, f)? {
            seeds.extend(file.wins());
        }
    }
    backward_walk(engine, dir, horizon, graph, seeds, prev)
}

/// The backward walk itself, given the seeds (the states that count as
/// marked on their own, i.e. the wins, from any layer). Factored out so a
/// test can seed it directly; `backward_run` seeds from the win lanes.
pub fn backward_walk(
    engine: &dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
    seeds: Vec<(u64, (u64, u64), u32)>,
    prev: Option<Marks>,
) -> Result<BackwardResult> {
    use rustc_hash::FxHashSet;

    let mut marks = prev.unwrap_or_default();
    marks.next_generation();
    let mut reruns: u64 = 0;
    for (shape, k, c) in seeds {
        marks.mark(shape, k, c, 0);
    }
    let cells_of = |targets: &[((u64, u64), u32)]| -> FxHashSet<u32> {
        let mut cells = FxHashSet::default();
        for &(_, c) in targets {
            cells.extend(graph.srcs_of(c).iter().copied());
        }
        cells
    };
    for i in (1..horizon).rev() {
        let t_frame = std::time::Instant::now();
        // The targets: the marks at distance `horizon - i - 1`, split into
        // those an earlier run set (already tested against every layer
        // < i) and this run's (tested against nothing yet).
        let (old_targets, new_targets) = marks.at_dist(horizon - i - 1);
        let d_mark = horizon - i;
        let targets: FxHashSet<(u64, u64, u32)> = old_targets
            .iter()
            .chain(&new_targets)
            .map(|&(k, c)| (k.0, k.1, c))
            .collect();
        let cells_new = cells_of(&new_targets);
        let cells_old = cells_of(&old_targets);
        let cells_layer_i: FxHashSet<u32> = cells_new.union(&cells_old).copied().collect();

        let (mut t_load, mut t_run) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
        let (mut loaded, mut frame_reruns, mut marked_now) = (0usize, 0u64, 0usize);
        let t = std::time::Instant::now();
        // Candidates: rows of every layer <= i in the narrowed cells - and
        // in the old targets' cells only layer i, the one layer an earlier
        // run did not test against them (at iteration i-1); at i == 1
        // there was no iteration 0, so layers 0 and 1 are both untested.
        // Not yet marked at distance <= d_mark. The layers are loaded in
        // parallel, a range copy per file and cell.
        let old_from = if i == 1 { 0 } else { i };
        let layers: Vec<Vec<Block>> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..=i)
                .map(|f| {
                    let cells = if f >= old_from { &cells_layer_i } else { &cells_new };
                    scope.spawn(move || {
                        if cells.is_empty() {
                            Ok(Vec::new())
                        } else {
                            load_frame_cells(dir, f, cells)
                        }
                    })
                })
                .collect();
            handles
                .into_iter()
                .map(|h| h.join().expect("layer loader panicked"))
                .collect::<Result<Vec<_>>>()
        })?;
        let mut cands: Vec<Block> = Vec::new();
        for block in layers.into_iter().flatten() {
            loaded += block.lanes();
            let cells = block.positions()?;
            let shape = block.shard_shape();
            let mask: Vec<bool> = block
                .keys()
                .iter()
                .zip(&cells)
                .map(|(k, &c)| marks.dist_of(shape, *k, c).is_none_or(|d| d > d_mark))
                .collect();
            if let Some(cand) = block.keep(&mask) {
                cands.push(cand);
            }
        }
        t_load += t.elapsed();
        // The re-runs: units of candidate lanes over the workers, each
        // reporting which of its input rows hit a target.
        let t = std::time::Instant::now();
        let cand_cells_of: Vec<Vec<u32>> = cands.iter().map(Block::positions).collect::<Result<_>>()?;
        frame_reruns += cands.iter().map(|b| b.lanes() as u64).sum::<u64>();
        let units = units_of(
            cands.iter().map(Block::lanes),
            unit_lanes(frame_reruns as usize, threads()),
        );
        let next_unit = AtomicUsize::new(0);
        let hits: Vec<(usize, usize, Vec<bool>)> = std::thread::scope(|scope| {
            let handles: Vec<_> = (0..threads())
                .map(|_| {
                    let (cands, cand_cells_of, units, next_unit, targets) =
                        (&cands, &cand_cells_of, &units, &next_unit, &targets);
                    scope.spawn(move || -> Result<Vec<(usize, usize, Vec<bool>)>> {
                        let mut out = Vec::new();
                        loop {
                            let u = next_unit.fetch_add(1, Ordering::Relaxed);
                            let Some(&(bi, lo, hi)) = units.get(u) else { break };
                            let mut sink = ForwardSink::backward(targets, lo..hi);
                            engine.run(&cands[bi], &cand_cells_of[bi], lo..hi, &mut sink)?;
                            out.push((bi, lo, sink.hits().to_vec()));
                        }
                        Ok(out)
                    })
                })
                .collect();
            handles
                .into_iter()
                .map(|h| h.join().expect("backward worker panicked"))
                .collect::<Result<Vec<_>>>()
                .map(|v| v.into_iter().flatten().collect())
        })?;
        t_run += t.elapsed();
        for (bi, lo, unit_hits) in hits {
            let keys = cands[bi].keys();
            let cells = &cand_cells_of[bi];
            let shape = cands[bi].shard_shape();
            for (j, hit) in unit_hits.iter().enumerate() {
                let lane = lo + j;
                if *hit && marks.mark(shape, keys[lane], cells[lane], d_mark) {
                    marked_now += 1;
                }
            }
        }
        reruns += frame_reruns;
        let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
        eprintln!(
            "[bwd] f{i:03} targets {}+{} cand-cells {} loaded {} rerun {} marked {} | \
             load {:.0} run {:.0} total {:.0} ms",
            old_targets.len(),
            new_targets.len(),
            cells_layer_i.len(),
            loaded,
            frame_reruns,
            marked_now,
            ms(t_load),
            ms(t_run),
            ms(t_frame.elapsed()),
        );
        crate::metrics::record("bwd.load", t_load);
        crate::metrics::record("bwd.run", t_run);
        crate::metrics::record("bwd.frame", t_frame.elapsed());
    }
    Ok(BackwardResult { marked: marks, reruns })
}

/// The forward search driver's state: the frontier, the visited set and
/// the position-graph observer, held in memory so a forward can be
/// EXTENDED frame by frame (the outer loop's "one more frame at rem zero")
/// rather than rerun. Every frame is checkpointed as it is produced.
pub struct ForwardState {
    frontier: Vec<Block>,
    /// One visited shard per owner (`owner_of`).
    visited: Vec<Visited>,
    observer: Option<crate::search::pos_graph::PosObserver>,
    /// The last frame computed (and checkpointed).
    pub frames: u32,
    /// The first frame a lane won, if any so far.
    pub win_frame: Option<u32>,
}

/// What a forward run reports.
pub struct ForwardResult {
    /// The frame a lane first won, if any.
    pub win_frame: Option<u32>,
    /// The number of frames actually run (the last checkpointed frame).
    pub frames: u32,
    /// The position graph, when recording was on - backward's input.
    pub pos_graph: Option<crate::search::pos_graph::PosGraph>,
}

impl ForwardState {
    /// Frame 0: seed the visited set from `initial`, checkpoint it, start
    /// recording the position graph if `record`.
    pub fn start(mut initial: Vec<Block>, dir: &std::path::Path, record: bool) -> Result<Self> {
        let owners = threads();
        let mut visited: Vec<Visited> = (0..owners).map(|_| Visited::new()).collect();
        for b in &initial {
            let cells = b.positions()?;
            for (k, c) in b.keys().iter().zip(&cells) {
                let shape = b.shard_shape();
                visited[owner_of(owners as u32, shape, *c) as usize].insert(shape, *k, *c);
            }
        }
        checkpoint_frontier(dir, 0, &mut initial)?;
        Ok(ForwardState {
            frontier: initial,
            visited,
            observer: record.then(crate::search::pos_graph::PosObserver::default),
            frames: 0,
            win_frame: None,
        })
    }

    /// Compute and checkpoint frames `frames+1 ..= to`. A win does NOT stop
    /// the run: a horizon is a bound on the win frame, and the backward
    /// needs every frame up to it (its seeds are the wins at each). Only an
    /// empty frontier stops it.
    /// The visited set's size, over all owners.
    pub fn visited_len(&self) -> usize {
        self.visited.iter().map(Visited::len).sum()
    }

    pub fn extend(
        &mut self,
        engine: &dyn FrameStep,
        dir: &std::path::Path,
        to: u32,
        filter: Option<&MarkFilter>,
    ) -> Result<()> {
        while self.frames < to && !self.frontier.is_empty() {
            let frame = self.frames + 1;
            let t_frame = std::time::Instant::now();
            let frontier = std::mem::take(&mut self.frontier);
            let (mut next, won, st) =
                forward_frame(engine, frontier, &mut self.visited, self.observer.as_ref(), filter)?;
            let t = std::time::Instant::now();
            checkpoint_frontier(dir, frame, &mut next)?;
            let t_ckpt = t.elapsed();
            let t = std::time::Instant::now();
            if let Some(o) = self.observer.as_ref() {
                o.flush();
            }
            let t_pos = t.elapsed();
            log_frame(frame, &st, t_ckpt, t_pos, t_frame.elapsed(), self.visited_len());
            self.frames = frame;
            if won && self.win_frame.is_none() {
                self.win_frame = Some(frame);
                eprintln!("[fwd] first win at f{frame}");
            }
            // A won row is checkpointed (it is a backward seed) but never
            // expanded: its successors have left the room, and the search
            // is about reaching the exit, not what lies past it (and the
            // start room's kernel set does not cover the next room's
            // shapes).
            self.frontier = if won {
                std::thread::scope(|scope| {
                    let handles: Vec<_> = next
                        .into_iter()
                        .map(|b| {
                            scope.spawn(move || -> Result<Option<Block>> {
                                let wins = b.wins()?;
                                let mask: Vec<bool> = wins.iter().map(|w| !w).collect();
                                Ok(b.keep(&mask))
                            })
                        })
                        .collect();
                    handles
                        .into_iter()
                        .map(|h| h.join().expect("win-drop worker panicked"))
                        .collect::<Result<Vec<_>>>()
                })?
                .into_iter()
                .flatten()
                .collect()
            } else {
                next
            };
        }
        Ok(())
    }

    /// The position graph recorded so far (None when not recording).
    pub fn pos_graph(&self) -> Option<crate::search::pos_graph::PosGraph> {
        self.observer.as_ref().map(|o| o.snapshot())
    }
}

/// A whole forward run in one call: frame 0 from `initial`, then frames
/// `1..=max_frames`.
pub fn forward_run(
    engine: &dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
    filter: Option<&MarkFilter>,
) -> Result<ForwardResult> {
    let mut st = ForwardState::start(initial, dir, record)?;
    st.extend(engine, dir, max_frames, filter)?;
    Ok(ForwardResult { win_frame: st.win_frame, frames: st.frames, pos_graph: st.pos_graph() })
}

/// One line per forward frame on stderr, plus the phase totals under
/// `metrics` (`fwd.*`), so a run's wall time is attributable without
/// re-running it under a profiler. Times in ms.
fn log_frame(
    frame: u32,
    st: &FrameStats,
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    eprintln!(
        "[fwd] f{frame:03} in {}/{} raw {} kept {} out {}/{} visited {} | \
         emit {:.0} own {:.0} ckpt {:.0} pos {:.0} total {:.0} ms | rss {:.2} GB",
        st.blocks_in,
        st.lanes_in,
        st.lanes_raw,
        st.lanes_kept,
        st.blocks_out,
        st.lanes_out,
        visited,
        ms(st.t_emit),
        ms(st.t_own),
        ms(t_ckpt),
        ms(t_pos),
        ms(t_total),
        crate::metrics::peak_rss_gb(),
    );
    crate::metrics::record("fwd.emit", st.t_emit);
    crate::metrics::record("fwd.own", st.t_own);
    crate::metrics::record("fwd.checkpoint", t_ckpt);
    crate::metrics::record("fwd.posgraph", t_pos);
    crate::metrics::record("fwd.frame", t_total);
}

/// Checkpoint a frontier: one file per BUCKET under `frames/fNNN/`, named
/// `b{seq}_s{shape}.bin`.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &mut [Block]) -> Result<()> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    // Fresh: a re-run / resumed frame must not leave stale files behind.
    let _ = std::fs::remove_dir_all(&fdir);
    std::fs::create_dir_all(&fdir)?;
    // One file per block (an owner's piece of a shape), written in
    // parallel. Canonical order inside each: by (cell, key), and the block
    // itself is sorted in place, so the file order and the kernel's lane
    // order agree and neither depends on how the frame was scheduled.
    std::thread::scope(|scope| {
        let handles: Vec<_> = frontier
            .iter_mut()
            .enumerate()
            .map(|(seq, block)| {
                let fdir = &fdir;
                scope.spawn(move || -> Result<()> {
                    let cells = block.positions()?;
                    let mut perm: Vec<u32> = (0..block.lanes() as u32).collect();
                    perm.sort_unstable_by_key(|&i| (cells[i as usize], block.rt2.row_keys[i as usize]));
                    block.rt2.gather_lanes(&perm);
                    let cells: Vec<u32> = perm.iter().map(|&i| cells[i as usize]).collect();
                    let wins = block.wins()?;
                    let path = fdir.join(format!("s{:016x}_{:03}.bin", block.shard_shape(), seq));
                    crate::search::checkpoint::save_block(&path, &block.rt2, &cells, &wins)
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("checkpoint worker panicked"))
            .collect::<Result<()>>()
    })
}

/// The checkpoint files of a frame (one per block written), mapped.
pub fn frame_files(
    dir: &std::path::Path,
    frame: u32,
) -> Result<Vec<crate::search::checkpoint::FrameFile>> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    let mut names: Vec<std::path::PathBuf> = std::fs::read_dir(&fdir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            let n = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            n.starts_with('s') && n.ends_with(".bin")
        })
        .collect();
    names.sort();
    names.iter().map(|p| crate::search::checkpoint::FrameFile::open(p)).collect()
}

/// Load every block of a checkpointed frame.
pub fn load_frame(dir: &std::path::Path, frame: u32) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    for file in frame_files(dir, frame)? {
        if let Some(rt2) = file.load_all()? {
            out.push(Block::from_rt2(rt2));
        }
    }
    Ok(out)
}

/// Load only the ROWS whose cell is in `cells` - the backward's per-cell
/// load, a range copy per (file, cell) out of the cell index.
pub fn load_frame_cells(
    dir: &std::path::Path,
    frame: u32,
    cells: &rustc_hash::FxHashSet<u32>,
) -> Result<Vec<Block>> {
    let mut out = Vec::new();
    for file in frame_files(dir, frame)? {
        if let Some(rt2) = file.load_cells(cells)? {
            out.push(Block::from_rt2(rt2));
        }
    }
    Ok(out)
}

/// The visited set: the keys of every lane ever kept into a frontier. A lane is
/// kept iff its key was not already here (this is both the within-frame and the
/// across-frame dedup - one structure, one lookup at the door).
///
/// Placeholder over an in-RAM set; the production one is tiered/mmap-backed
/// (`interpreter::visited`) but that is an implementation swap behind
/// `insert`/`contains`, not part of this interface.
#[derive(Default)]
pub struct Visited {
    /// Sharded by (shape hash, cell): a shard holds the 128-bit content
    /// keys seen at that shape and cell. Two states in different shards can
    /// never be duplicates (different shape or different content ->
    /// different cell), so the sharding is a free refinement: small sets,
    /// per-cell locality, and a run of rows at one cell is one outer
    /// lookup (`shard_mut`).
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashSet<(u64, u64)>>,
}

impl Visited {
    pub fn new() -> Self {
        Self::default()
    }
    /// `(entries, order-independent hash of the (content, cell) set)` - the
    /// gate that two backward passes marked the same states. A function of
    /// the keys and cells only, not of how they are sharded.
    pub fn fingerprint(&self) -> (usize, u64) {
        use celeste_engine::runtime2::mix64;
        let mut acc = 0u64;
        for ((_shape, cell), keys) in &self.shards {
            for &(k0, k1) in keys {
                acc = acc.wrapping_add(mix64(k0 ^ mix64(k1 ^ (*cell as u64) << 1)));
            }
        }
        (self.len(), acc)
    }
    /// True if `key` (at `shape`, `cell`) was NOT already present - this
    /// lane is new, keep it.
    pub fn insert(&mut self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.shard_mut(shape, cell).insert(key)
    }
    /// The key set of one `(shape, cell)` shard, created if absent.
    pub fn shard_mut(&mut self, shape: u64, cell: u32) -> &mut rustc_hash::FxHashSet<(u64, u64)> {
        self.shards.entry((shape, cell)).or_default()
    }
    pub fn contains(&self, shape: u64, key: (u64, u64), cell: u32) -> bool {
        self.shards.get(&(shape, cell)).is_some_and(|s| s.contains(&key))
    }
    pub fn len(&self) -> usize {
        self.shards.values().map(|s| s.len()).sum()
    }

    /// Persist the set as `(shape, cell, key)` rows.
    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        let mut v: Vec<(u64, u32, u64, u64)> = Vec::with_capacity(self.len());
        for ((shape, cell), keys) in &self.shards {
            v.extend(keys.iter().map(|&(k0, k1)| (*shape, *cell, k0, k1)));
        }
        crate::search::checkpoint::save_value_to(path, &v)
    }

    pub fn load(path: &std::path::Path) -> Result<Self> {
        let v: Vec<(u64, u32, u64, u64)> = crate::search::checkpoint::load_value_from(path)?;
        let mut out = Self::new();
        for (shape, cell, k0, k1) in v {
            out.insert(shape, (k0, k1), cell);
        }
        Ok(out)
    }
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
}

/// A fixed-horizon ladder result (`ladder_at_horizon`).
pub enum HorizonOutcome {
    /// Every precision level - through fully concrete - won by the horizon. The
    /// horizon is achievable; the concrete level yields a real winning trace.
    Confirmed,
    /// A precision level, filtered by the coarser level's marks, found no win by
    /// the horizon. A coarser level over-approximates concrete reachability, so
    /// this SOUNDLY excludes the horizon: no concrete play wins by it. `level`
    /// is the index into `precisions` that refuted.
    Refuted { level: usize },
}

/// The precision ladder over rising horizons (semantics pinned with Philippe
/// 2026-08-30; incremental level 0 2026-09-12).
///
/// At a horizon H, every level in `precisions` (coarsest first, ending at
/// `RemPrecision::Exact` = fully concrete) runs its forward TO H, filtered
/// by the previous level's marks, and its backward marks every state that
/// can reach a win by H. The instant a level has no win by H the horizon is
/// `Refuted` - a coarser level over-approximates concrete reachability, so
/// that soundly excludes it. Only if EVERY level wins by H is it `Confirmed`.
///
/// Level 0 is PERSISTENT across horizons: its forward is extended by the
/// frames the new horizon adds (`ForwardState::extend`), never rerun, and
/// its backward is recomputed from the extended checkpoints. The finer
/// levels are filtered by marks that change with H, so they rerun.
pub struct Ladder<'a, E, I>
where
    E: FnMut(crate::interpreter::abstraction::RemPrecision) -> Result<Box<dyn FrameStep>>,
    I: FnMut() -> Result<Vec<Block>>,
{
    make_engine: E,
    make_initial: I,
    base_dir: &'a std::path::Path,
    precisions: &'a [crate::interpreter::abstraction::RemPrecision],
    level0: Option<(Box<dyn FrameStep>, ForwardState)>,
    /// Level 0's marks from the last horizon, the next horizon's backward
    /// starts from them (`backward_walk` is incremental).
    level0_marks: Option<Marks>,
}

impl<'a, E, I> Ladder<'a, E, I>
where
    E: FnMut(crate::interpreter::abstraction::RemPrecision) -> Result<Box<dyn FrameStep>>,
    I: FnMut() -> Result<Vec<Block>>,
{
    pub fn new(
        make_engine: E,
        make_initial: I,
        base_dir: &'a std::path::Path,
        precisions: &'a [crate::interpreter::abstraction::RemPrecision],
    ) -> Self {
        Ladder { make_engine, make_initial, base_dir, precisions, level0: None, level0_marks: None }
    }

    fn level_dir(&self, horizon: u32, level: usize) -> std::path::PathBuf {
        if level == 0 {
            self.base_dir.join("level00")
        } else {
            self.base_dir.join(format!("h{:03}", horizon)).join(format!("level{:02}", level))
        }
    }

    /// Extend level 0 to `horizon` (starting it if needed) and report its
    /// first win frame, if it has one by then.
    pub fn extend_level0(&mut self, horizon: u32) -> Result<Option<u32>> {
        anyhow::ensure!(!self.precisions.is_empty(), "an empty ladder");
        // The kernel set an engine dispatches to follows the PROCESS-GLOBAL
        // rem precision (`asm_kernel::registry`), and a finer level ran
        // since level 0 last did: say which level is running before it runs.
        crate::interpreter::abstraction::set_rem_precision(self.precisions[0]);
        let dir = self.level_dir(horizon, 0);
        if self.level0.is_none() {
            let engine = (self.make_engine)(self.precisions[0])?;
            let state = ForwardState::start((self.make_initial)()?, &dir, true)?;
            self.level0 = Some((engine, state));
        }
        let (engine, state) = self.level0.as_mut().expect("just started");
        state.extend(engine.as_ref(), &dir, horizon, None)?;
        Ok(state.win_frame.filter(|&h| h <= horizon))
    }

    pub fn at_horizon(&mut self, horizon: u32) -> Result<HorizonOutcome> {
        let mut prev: Option<(Marks, crate::interpreter::abstraction::RemPrecision)> = None;
        for level in 0..self.precisions.len() {
            let precision = self.precisions[level];
            let dir = self.level_dir(horizon, level);
            // The level's forward to `horizon`: level 0 extended in place,
            // a finer level fresh, under the previous level's marks.
            let mut fresh: Option<Box<dyn FrameStep>> = None;
            let (win, graph) = if level == 0 {
                let win = self.extend_level0(horizon)?;
                let state = &self.level0.as_ref().expect("started by extend_level0").1;
                (win, state.pos_graph().expect("level 0 records"))
            } else {
                crate::interpreter::abstraction::set_rem_precision(precision);
                let engine = (self.make_engine)(precision)?;
                let filter = prev.as_ref().map(|(m, p)| MarkFilter::new(m, *p));
                let fwd = forward_run(
                    engine.as_ref(),
                    (self.make_initial)()?,
                    &dir,
                    horizon,
                    true,
                    filter.as_ref(),
                )?;
                fresh = Some(engine);
                (fwd.win_frame, fwd.pos_graph.expect("record mode always builds the pos graph"))
            };
            let Some(h) = win else {
                eprintln!("[ladder] h{horizon} level {level} ({precision:?}): NO WIN -> Refuted");
                return Ok(HorizonOutcome::Refuted { level });
            };
            let engine: &dyn FrameStep = match fresh.as_ref() {
                Some(e) => e.as_ref(),
                None => self.level0.as_ref().expect("level 0").0.as_ref(),
            };
            // `CELESTE_BACKWARD_SCRATCH=1`: level 0's backward from scratch at
            // every horizon - the A/B for the incremental walk.
            let scratch = std::env::var_os("CELESTE_BACKWARD_SCRATCH").is_some();
            let prev_marks = if level == 0 && !scratch { self.level0_marks.take() } else { None };
            let bwd = backward_run(engine, &dir, horizon, &graph, prev_marks)?;
            bwd.marked.save(&marks_path(self.base_dir, horizon, level))?;
            if level == 0 {
                self.level0_marks = Some(bwd.marked.clone());
            }
            let (n, fp) = bwd.marked.fingerprint();
            eprintln!(
                "[ladder] h{horizon} level {level} ({precision:?}): first win f{h}, marked {n} states \
                 (fingerprint {fp:016x}), {} re-runs",
                bwd.reruns
            );
            prev = Some((bwd.marked, precision));
        }
        Ok(HorizonOutcome::Confirmed)
    }
}

/// Where a level's marked set at a horizon is saved (level 0's checkpoints
/// are shared across horizons; its marks are not).
pub fn marks_path(base_dir: &std::path::Path, horizon: u32, level: usize) -> std::path::PathBuf {
    base_dir.join(format!("h{:03}", horizon)).join(format!("level{:02}.marks.bin", level))
}

/// The inner ladder at ONE horizon, from scratch - the tests' entry point;
/// the search uses `Ladder` so level 0 persists across horizons.
pub fn ladder_at_horizon(
    make_engine: impl FnMut(
        crate::interpreter::abstraction::RemPrecision,
    ) -> Result<Box<dyn FrameStep>>,
    make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    horizon: u32,
    precisions: &[crate::interpreter::abstraction::RemPrecision],
) -> Result<HorizonOutcome> {
    Ladder::new(make_engine, make_initial, base_dir, precisions).at_horizon(horizon)
}

/// The OUTER loop: the minimal winning frame. Level 0 is extended until it
/// first wins (the abstract lower bound, or `first_win` if that is later);
/// from there the horizon steps up by one until the ladder Confirms it -
/// that horizon is the optimum, and (once trace extraction lands) the
/// concrete level's winning trace is the witness. `None` if nothing confirms
/// by `max_horizon`.
pub fn find_optimum(
    make_engine: impl FnMut(
        crate::interpreter::abstraction::RemPrecision,
    ) -> Result<Box<dyn FrameStep>>,
    make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    first_win: u32,
    max_horizon: u32,
    precisions: &[crate::interpreter::abstraction::RemPrecision],
) -> Result<Option<u32>> {
    let mut ladder = Ladder::new(make_engine, make_initial, base_dir, precisions);
    let mut horizon = first_win.max(1);
    // Level 0 first: extend until it wins, one frame at a time past
    // `first_win`, without touching the finer levels.
    while horizon <= max_horizon && ladder.extend_level0(horizon)?.is_none() {
        eprintln!("[search] level 0 has no win by f{horizon}");
        horizon += 1;
    }
    while horizon <= max_horizon {
        match ladder.at_horizon(horizon)? {
            HorizonOutcome::Confirmed => return Ok(Some(horizon)),
            HorizonOutcome::Refuted { level } => {
                eprintln!("[search] horizon {horizon} refuted at level {level}");
                horizon += 1;
            }
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::refengine::RefEngine;
    use std::sync::Mutex;

    /// End-to-end proof that the rebuilt outer loop runs: drive `forward_run`
    /// over the trusted reference engine for a few frames from the initial
    /// block, checkpointing each frontier, and prove the checkpoint round-trips
    /// (reload a frame's states and match the count). Uses the interpreter (the
    /// oracle), so it is slow and needs the cart on disk; run explicitly.
    #[test]
    #[ignore]
    fn forward_run_drives_the_reference_engine() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];

        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let result = forward_run(&Mutex::new(engine), init, dir, 4, true, None).expect("forward_run");
        // No win in the 4-frame intro; the run reaches the horizon.
        assert_eq!(result.win_frame, None, "unexpected early win in the intro");
        assert_eq!(result.frames, 4, "expected 4 frames run");
        // Recording was on: a position graph was built.
        let pg = result.pos_graph.expect("pos graph built in record mode");
        eprintln!("[forward_run] pos-graph: {} live cells", pg.live_cells());

        // Every frame 0..=4 was checkpointed sharded and reloads block-by-block.
        for frame in 0..=4 {
            let blocks = load_frame(dir, frame)
                .unwrap_or_else(|e| panic!("reload frame {frame}: {e}"));
            assert!(!blocks.is_empty(), "frame {frame} checkpoint is empty");
            // Per-cell load of every cell returns every row.
            let cells: rustc_hash::FxHashSet<u32> = blocks
                .iter()
                .flat_map(|b| b.positions().unwrap())
                .collect();
            let by_cell = load_frame_cells(dir, frame, &cells).expect("per-cell load");
            let lanes: usize = blocks.iter().map(Block::lanes).sum();
            let lanes_by_cell: usize = by_cell.iter().map(Block::lanes).sum();
            assert_eq!(lanes_by_cell, lanes, "per-cell load lost rows at frame {frame}");
            eprintln!("[forward_run] f{frame}: {} buckets across {} cells", blocks.len(), cells.len());
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the ladder forward filter: one frame, then re-run it
    /// with (a) a marked set containing exactly the widened frame-1 states -
    /// nothing is discarded - and (b) an empty marked set - everything is
    /// discarded and the frontier empties. Uses Bits(0) as the coarser level.
    #[test]
    fn forward_filter_keeps_marked_and_discards_unmarked() {
        use crate::interpreter::abstraction::RemPrecision;
        let bits0 = RemPrecision::Bits(0);
        let engine = RefEngine::new().expect("ref engine");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-filter-test");
        let seed = || vec![Block::from_state(&engine.initial_state().expect("init")).expect("block")];

        // Baseline one frame, no filter.
        {
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, None).expect("baseline");
        }
        let frame1 = load_frame(dir, 1).expect("load f1");
        assert!(!frame1.is_empty(), "baseline produced no frame 1");

        // marked_full = the widened-to-Bits(0) keys of every frame-1 state.
        let mut marked_full = Marks::new();
        for b in &frame1 {
            let (shape, keys, cells) = widened_keys(b, bits0).expect("widened keys");
            for (k, &c) in keys.iter().zip(&cells) {
                marked_full.mark(shape, *k, c, 0);
            }
        }

        // With the full marked set, frame 1 survives.
        {
            let f = MarkFilter::new(&marked_full, bits0);
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, Some(&f)).expect("full-filter run");
            let kept = load_frame(dir, 1).expect("load f1 full");
            assert!(!kept.is_empty(), "full marked set wrongly discarded frame 1");
        }

        // With an empty marked set, everything is discarded.
        {
            let empty = Marks::new();
            let f = MarkFilter::new(&empty, bits0);
            let e = engine_clone(&engine);
            forward_run(&e, seed(), dir, 1, false, Some(&f)).expect("empty-filter run");
            let kept = load_frame(dir, 1).expect("load f1 empty");
            assert!(kept.is_empty(), "empty marked set failed to discard frame 1");
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A fresh reference engine behind the lock the `FrameStep` impl needs
    /// (RefEngine isn't Clone). Cheap enough for a test.
    fn engine_clone(_e: &RefEngine) -> Mutex<RefEngine> {
        Mutex::new(RefEngine::new().expect("ref engine"))
    }

    /// The FULL ladder to the concrete level: run every precision - Bits 0..=16
    /// then Exact - on the compiled engine with the synthetic early win, and
    /// require Confirmed (every level, including fully concrete, reaches the win
    /// when filtered by the coarser level's backward marks). This proves the new
    /// search works end to end through refinement, the gate for deleting the old
    /// search. Slow (many compiled forward+backward passes); run explicitly.
    #[test]
    #[ignore]
    fn new_ladder_confirms_to_concrete() {
        use crate::interpreter::abstraction::{set_rem_precision, RemPrecision};
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-full");
        let _ = std::fs::remove_dir_all(dir);

        // The real ladder maps k>=16 to Exact (rewrite.rs: `if prev_bits >= 16
        // { Exact }`), so the distinct precisions are Bits(0..=15) then Exact.
        // CELESTE_LADDER_MAXBITS bisects: Bits(0..=maxbits) then Exact.
        let maxbits: u8 = std::env::var("CELESTE_LADDER_MAXBITS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(15);
        let precisions: Vec<RemPrecision> = (0u8..=maxbits)
            .map(RemPrecision::Bits)
            .chain(std::iter::once(RemPrecision::Exact))
            .collect();

        let make_engine = |precision: RemPrecision| {
            set_rem_precision(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome =
            ladder_at_horizon(make_engine, make_initial, dir, 14, &precisions).expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder-full] all 17 levels through Exact reached the win");
            }
            HorizonOutcome::Refuted { level } => {
                panic!("level {level} (of 17) refuted - the new ladder breaks before concrete");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// THE LADDER VALIDATION (Philippe's "validate the per-rem-level forwards,
    /// backward equality is implied"). Run the new ladder over rem 0 then rem 1
    /// on the compiled engine, with a synthetic early win at (8,107) in room
    /// (1,0) - reached ~frame 8 on the fall path, with many states per frame.
    /// rem-1's forward is filtered by rem-0's backward marks; if it still finds
    /// the win, `Confirmed`, the backward did NOT drop the winning path. A
    /// backward that under-marks would over-filter rem 1 and it would refute.
    /// This exercises forward + backward + MarkFilter end to end on the kernels.
    #[test]
    #[ignore]
    fn new_ladder_backward_preserves_win_across_rem_levels() {
        use crate::interpreter::abstraction::{set_rem_precision, RemPrecision};
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");

        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-test");
        let _ = std::fs::remove_dir_all(dir);

        let make_engine = |precision: RemPrecision| {
            set_rem_precision(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room()?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome = ladder_at_horizon(
            make_engine,
            make_initial,
            dir,
            14,
            &[RemPrecision::Bits(0), RemPrecision::Bits(1)],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder] rem0 and rem1 both reached the synthetic win - backward preserved it");
            }
            HorizonOutcome::Refuted { level } => {
                panic!(
                    "level {level} lost the win that a coarser level found - \
                     the backward under-marked (dropped a winning-path state)"
                );
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// forward_resume, extending a checkpointed forward, reproduces a fresh run:
    /// run fresh to frame 4, capture frame 4's key set, then resume from frame 2
    /// out to 4 (rebuilding visited from the checkpoints) and check frame 4 is
    /// byte-for-byte the same key set.
    #[test]
    #[ignore]
    fn forward_extended_frame_by_frame_matches_fresh() {
        use rustc_hash::FxHashSet;
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-resume-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        let keyset = |frame: u32| -> FxHashSet<(u64, u64)> {
            load_frame(dir, frame)
                .expect("load")
                .iter()
                .flat_map(|b| b.keys().to_vec())
                .collect()
        };

        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            forward_run(&Mutex::new(e), init, dir, 4, true, None).expect("fresh");
        }
        let fresh4 = keyset(4);
        assert!(!fresh4.is_empty(), "fresh frame 4 empty");

        // The same run extended one frame at a time - the outer loop's
        // "one more frame at rem zero" - lands on the same key set.
        {
            let e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            let e = Mutex::new(e);
            let mut st = ForwardState::start(init, dir, true).expect("start");
            for to in 1..=4 {
                st.extend(&e, dir, to, None).expect("extend");
                assert_eq!(st.frames, to);
            }
            assert!(st.pos_graph().is_some());
        }
        let extended4 = keyset(4);
        assert_eq!(fresh4, extended4, "extension diverged from fresh at frame 4");
        eprintln!("[extend] frame 4 key set identical: {} keys", fresh4.len());
        let _ = std::fs::remove_dir_all(dir);
    }

    /// End-to-end wiring of the ladder's forward+refute path: run one level with
    /// the reference over the intro (which has no win in 4 frames), and check
    /// the ladder reports Refuted. Exercises make_engine/make_initial, the
    /// filtered forward, and the no-win -> Refuted branch. (The Optimal/backward
    /// branch needs a real winning room - the "together" validation.)
    #[test]
    #[ignore]
    fn ladder_refutes_when_no_win_by_horizon() {
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-ladder-test");
        let _ = std::fs::remove_dir_all(dir);
        use crate::interpreter::abstraction::RemPrecision;
        let outcome = ladder_at_horizon(
            |_precision| Ok(Box::new(Mutex::new(RefEngine::new()?)) as Box<dyn FrameStep>),
            || Ok(vec![Block::from_state(&RefEngine::new()?.initial_state()?)?]),
            dir,
            4,
            &[RemPrecision::Bits(0)],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Refuted { level } => {
                assert_eq!(level, 0, "refuted at the wrong level");
                eprintln!("[ladder] refuted at level={level} (no win in the 4-frame intro)");
            }
            HorizonOutcome::Confirmed => {
                panic!("ladder wrongly reported Confirmed for the win-less intro");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the backward walk (the intro has no real win, so we
    /// seed artificially). Run 4 forward frames recording the pos-graph, then
    /// seed the backward from ALL of frame 4's states and walk back. The intro
    /// is a deterministic chain (one state per frame), so backward must mark a
    /// state at every earlier frame, and - the invariant Philippe named - it
    /// must re-run no more states than forward produced.
    #[test]
    #[ignore]
    fn backward_walk_propagates_along_the_intro_chain() {
        let engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-bwd-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let horizon = 4;
        let engine = Mutex::new(engine);
        let fwd = forward_run(&engine, init, dir, horizon, true, None).expect("forward");
        let graph = fwd.pos_graph.expect("pos graph");

        // Seed from every state at the horizon frame.
        let seed: Vec<(u64, (u64, u64), u32)> = load_frame(dir, horizon)
            .expect("load horizon")
            .iter()
            .flat_map(|b| {
                let shape = b.shard_shape();
                let keys = b.keys().to_vec();
                let cells = b.positions().expect("cells");
                keys.into_iter().zip(cells).map(move |(k, c)| (shape, k, c)).collect::<Vec<_>>()
            })
            .collect();
        assert!(!seed.is_empty(), "no states at the horizon to seed from");

        let bwd = backward_walk(&engine, dir, horizon, &graph, seed, None).expect("backward");
        eprintln!(
            "[backward] marked {} states, {} re-runs",
            bwd.marked.len(),
            bwd.reruns
        );
        // The deterministic chain: a state marked at every frame 1..=horizon.
        assert!(
            bwd.marked.len() >= horizon as usize,
            "backward marked {} states, expected >= {}",
            bwd.marked.len(),
            horizon
        );
        // Philippe's cost invariant: backward re-runs a position-filtered subset,
        // so it must not exceed the forward frame count (states produced).
        let fwd_states: usize = (1..=horizon)
            .map(|f| load_frame(dir, f).expect("load").iter().map(|b| b.lanes()).sum::<usize>())
            .sum();
        assert!(
            (bwd.reruns as usize) <= fwd_states,
            "backward re-ran {} > forward {} states - narrowing is broken",
            bwd.reruns,
            fwd_states
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}
