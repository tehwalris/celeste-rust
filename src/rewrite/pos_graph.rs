//! A compact over-approximation of the successor relation, PROJECTED ONTO
//! THE PLAYER'S POSITION - and nothing else.
//!
//! The backward sweep needs predecessors: "who steps into the rows that just
//! got marked?". The frame function only answers successors, so today that
//! inversion is materialized as an explicit edge graph - 10,072,724,145
//! `(src row, dst row)` pairs and 58 GB for room (1,0), and an outright OOM
//! on room (0,0). This replaces it with the same question asked about
//! POSITIONS: "which positions can step into this position?". For room (1,0)
//! the answer is 383,528 `(dst cell, src cell)` pairs - about 1.5 MB.
//!
//! **This is not, and must never become, a set of predecessor states.** It
//! has one node per position cell (65,535 of them for a room, plus one for
//! "no player object"), never one per row; it is a static property of the
//! game's geometry, not of any particular search; and it is used only to
//! shrink the candidate set that the sweep then actually expands. The
//! expansion is what establishes an edge. If this table were too small the
//! sweep would be wrong, so it is RECORDED - by replaying the saved frontier
//! batches, which is the same work the forward pass did and so sees every
//! transition exactly once (frontier-only, and the successor relation is a
//! static graph over rows) - never guessed. If it is too LARGE the sweep is
//! merely slower, which is why an over-approximation is safe.
//!
//! It is HORIZON-INDEPENDENT by construction: nothing here reads a horizon,
//! a band or a `g`. It answers "which positions can step into this
//! position?", which is a property of the game's dynamics. Build once per
//! room, reuse for every horizon of the ladder.
//!
//! Positions are measured relative to the START room. Object coordinates in
//! the cart are room-local, so the one frame that crosses into the next room
//! wraps the player's x from ~128 back to ~0; measured that way it looks
//! like a 128-pixel teleport, and since the cell it lands in is the seed of
//! the backward sweep, every candidate mask that touched it covered the
//! whole room. See BENCHMARK_DATA.md.

use anyhow::{anyhow, Result};
use std::path::Path;

use crate::interpreter::abstraction::{player_xy_per_lane, room_xy_per_lane};
use crate::interpreter::state::State;

/// Side of the position grid, in pixels. A room is 128x128; the grid is
/// bigger because positions are start-room-relative, so a row that has
/// already crossed into the next room sits past 128.
pub const GRID: i32 = 256;
/// Pixel coordinate mapped to grid index 0.
pub const ORIGIN: i32 = -64;
/// Side of a room in pixels - the stride between rooms in the grid.
const ROOM_PX: i32 = 128;

/// The cell of a row with no player object: the countdown states after a
/// death, and any state whose objects array has neither `player` nor
/// `player_spawn`. It is a normal node of the graph - such a row still has
/// predecessors and successors, they just cannot be located - so it must
/// participate, not be dropped.
///
/// It doubles as the grid's out-of-range marker, so `cell_of` refuses to
/// produce it: the single grid position it would collide with is rejected
/// loudly rather than silently merging with "nowhere".
pub const NO_CELL: u16 = u16::MAX;

/// Number of nodes, including `NO_CELL`.
pub const CELL_COUNT: usize = 1 << 16;
/// `u64` words in a bitmap over all nodes.
pub const CELL_WORDS: usize = CELL_COUNT / 64;

/// Grid cell of a whole-pixel START-ROOM-RELATIVE position. Loud rather
/// than clamping: a position outside the grid would silently distort the
/// whole table.
pub fn cell_of(x: i32, y: i32) -> Result<u16> {
    let (gx, gy) = (x - ORIGIN, y - ORIGIN);
    if !(0..GRID).contains(&gx) || !(0..GRID).contains(&gy) {
        return Err(anyhow!("position ({}, {}) is outside the position grid", x, y));
    }
    let cell = (gy * GRID + gx) as u16;
    if cell == NO_CELL {
        return Err(anyhow!("position ({}, {}) collides with the no-position node", x, y));
    }
    Ok(cell)
}

/// Human-readable start-room-relative pixels of a cell.
pub fn cell_xy(cell: u16) -> Option<(i32, i32)> {
    (cell != NO_CELL)
        .then(|| (cell as i32 % GRID + ORIGIN, cell as i32 / GRID + ORIGIN))
}

/// Per-lane cell of a boundary state.
pub fn state_cells(state: &State) -> Result<Vec<u16>> {
    let start = crate::game_runner::start_room();
    let rooms = room_xy_per_lane(state)
        .ok_or_else(|| anyhow!("state_cells: no readable `room` global"))?;
    let lanes = state.vector_size.max(1);
    if rooms.len() != lanes {
        return Err(anyhow!("state_cells: {} rooms for {} lanes", rooms.len(), lanes));
    }
    let Some(xy) = player_xy_per_lane(state) else {
        return Ok(vec![NO_CELL; lanes]);
    };
    if xy.len() != lanes {
        return Err(anyhow!("state_cells: {} positions for {} lanes", xy.len(), lanes));
    }
    xy.iter()
        .zip(&rooms)
        .map(|(&(px, py), &(rx, ry))| {
            cell_of(
                px as i32 + (rx - start.0) as i32 * ROOM_PX,
                py as i32 + (ry - start.1) as i32 * ROOM_PX,
            )
        })
        .collect()
}

/// The finished table: for each destination cell, the cells a predecessor
/// of it was ever in, as a sorted CSR. Deliberately the transpose - the
/// sweep only ever asks the backward question.
#[derive(Default)]
pub struct PosGraph {
    /// `srcs[offsets[d] .. offsets[d + 1]]` for destination cell `d`.
    offsets: Vec<u32>,
    srcs: Vec<u16>,
    /// The `frames` the table was recorded for: it has seen every
    /// transition taken out of frames `1..frames`. Horizon-independent but
    /// NOT frame-independent - extending the forward pass adds frames whose
    /// transitions this has never seen, and a missing pair means the sweep's
    /// filter never generates that candidate and nothing notices. Stored so
    /// the sweep can check it rather than assume.
    frames: u32,
}

impl PosGraph {
    pub fn pairs(&self) -> usize {
        self.srcs.len()
    }

    /// The frame count this table covers (transitions out of `1..frames`).
    pub fn frames(&self) -> u32 {
        self.frames
    }

    /// Back to a builder, so a table can be extended over new frames.
    pub fn into_builder(self) -> PosGraphBuilder {
        let mut by_dst: rustc_hash::FxHashMap<u16, Vec<u16>> = Default::default();
        for d in 0..self.offsets.len().saturating_sub(1) {
            let (lo, hi) = (self.offsets[d] as usize, self.offsets[d + 1] as usize);
            if lo != hi {
                by_dst.insert(d as u16, self.srcs[lo..hi].to_vec());
            }
        }
        PosGraphBuilder { by_dst }
    }

    /// Destination cells with at least one recorded predecessor.
    pub fn live_cells(&self) -> usize {
        self.offsets.windows(2).filter(|w| w[1] > w[0]).count()
    }

    pub fn srcs_of(&self, dst: u16) -> &[u16] {
        if self.offsets.is_empty() {
            return &[];
        }
        let (lo, hi) = (self.offsets[dst as usize] as usize, self.offsets[dst as usize + 1] as usize);
        &self.srcs[lo..hi]
    }

    /// Union of the predecessor cells of `dsts`, as a node bitmap.
    ///
    /// A destination with no recorded predecessor keeps only ITSELF. That is
    /// the sound reading: the forward pass recorded every transition, so a
    /// cell nothing was ever seen to step into is only reachable by being
    /// there already (the start row), and dropping it entirely would lose
    /// that row.
    pub fn union_into(&self, dsts: &[u16], out: &mut [u64]) {
        out.fill(0);
        for &d in dsts {
            let srcs = self.srcs_of(d);
            if srcs.is_empty() {
                out[d as usize / 64] |= 1 << (d % 64);
                continue;
            }
            for &s in srcs {
                out[s as usize / 64] |= 1 << (s % 64);
            }
        }
    }

    /// `<dir>/posgraph.bin`.
    ///
    /// The magic is `C8PX`; the `C8PW` files the frame-count-less first
    /// version wrote are refused by `load` rather than read as covering an
    /// unknown range, because a table short of the horizon silently
    /// under-generates candidates.
    pub fn save(&self, dir: &Path) -> Result<()> {
        use std::io::Write;
        let tmp = dir.join("tmp-posgraph.bin");
        {
            let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
            w.write_all(b"C8PX")?;
            w.write_all(&self.frames.to_le_bytes())?;
            w.write_all(&(self.srcs.len() as u64).to_le_bytes())?;
            for v in &self.offsets {
                w.write_all(&v.to_le_bytes())?;
            }
            for v in &self.srcs {
                w.write_all(&v.to_le_bytes())?;
            }
            w.flush()?;
        }
        std::fs::rename(&tmp, dir.join("posgraph.bin"))?;
        Ok(())
    }

    /// Load `save`'s output, or `None` when the forward pass never wrote one.
    pub fn load(dir: &Path) -> Result<Option<PosGraph>> {
        use std::io::Read;
        let path = dir.join("posgraph.bin");
        if !path.exists() {
            return Ok(None);
        }
        let mut file = std::io::BufReader::new(std::fs::File::open(&path)?);
        let mut magic = [0u8; 4];
        file.read_exact(&mut magic)?;
        if &magic == b"C8PW" {
            return Err(anyhow!(
                "{}: written by the version that did not record its frame \
                 coverage - delete it and let the sweep rebuild it",
                path.display()
            ));
        }
        if &magic != b"C8PX" {
            return Err(anyhow!("{}: bad magic", path.display()));
        }
        let mut buf4 = [0u8; 4];
        file.read_exact(&mut buf4)?;
        let frames = u32::from_le_bytes(buf4);
        let mut buf8 = [0u8; 8];
        file.read_exact(&mut buf8)?;
        let pairs = u64::from_le_bytes(buf8) as usize;
        let mut buf = vec![0u8; (CELL_COUNT + 1) * 4];
        file.read_exact(&mut buf)?;
        let offsets: Vec<u32> =
            buf.chunks_exact(4).map(|c| u32::from_le_bytes(c.try_into().unwrap())).collect();
        let mut buf = vec![0u8; pairs * 2];
        file.read_exact(&mut buf)?;
        let srcs: Vec<u16> =
            buf.chunks_exact(2).map(|c| u16::from_le_bytes(c.try_into().unwrap())).collect();
        if offsets[CELL_COUNT] as usize != pairs {
            return Err(anyhow!("{}: offsets end at {}, not {}", path.display(), offsets[CELL_COUNT], pairs));
        }
        Ok(Some(PosGraph { offsets, srcs, frames }))
    }
}

/// Accumulates `(dst, src)` pairs during the forward pass.
///
/// A dense `CELL_COUNT * CELL_COUNT` bitmap would be 512 MB and a hash set
/// of pairs would cost a lookup per successor LANE - at 645M lanes in one
/// frame of room (0,0) that is the wrong shape. Instead each destination
/// keeps a small sorted set, which is what the pairs actually are: 47
/// sources per destination on room (1,0).
#[derive(Default)]
pub struct PosGraphBuilder {
    /// Destination cell -> its (small) sorted set of source cells.
    by_dst: rustc_hash::FxHashMap<u16, Vec<u16>>,
}

impl PosGraphBuilder {
    pub fn record(&mut self, src: u16, dst: u16) {
        let set = self.by_dst.entry(dst).or_default();
        if let Err(at) = set.binary_search(&src) {
            set.insert(at, src);
        }
    }

    pub fn merge(&mut self, other: PosGraphBuilder) {
        for (dst, srcs) in other.by_dst {
            for src in srcs {
                self.record(src, dst);
            }
        }
    }

    pub fn pairs(&self) -> usize {
        self.by_dst.values().map(|v| v.len()).sum()
    }

    /// `frames` records what range the table has seen (see
    /// `PosGraph::frames`); it is the caller's claim, not a measurement.
    pub fn build(self, frames: u32) -> PosGraph {
        let mut offsets = Vec::with_capacity(CELL_COUNT + 1);
        let mut srcs = Vec::with_capacity(self.pairs());
        let mut by_dst = self.by_dst;
        for d in 0..CELL_COUNT {
            offsets.push(srcs.len() as u32);
            if let Some(set) = by_dst.remove(&(d as u16)) {
                srcs.extend_from_slice(&set);
            }
        }
        offsets.push(srcs.len() as u32);
        PosGraph { offsets, srcs, frames }
    }
}

/// Build the table by replaying the saved frontier batches one frame each,
/// which is the same work the forward pass did and therefore the same
/// chunking - `AbstractRun::step` re-chunks the restored batch exactly as
/// the forward pass chunked it.
///
/// This exists so the table can be produced for a room whose forward pass
/// has already run (and, for room (0,0), whose edge-based sweep cannot
/// run at all). A forward pass that enables `record_pos_graph` gets the
/// same table for free; nothing here needs the edges.
///
/// `from` is the first frame to replay, so a table that already covers
/// `1..from` can be extended without redoing it.
pub fn build_from_replay(
    dir: &Path,
    from: u32,
    frames: u32,
    engine: &mut crate::rewrite::verify::AbstractRun,
) -> Result<PosGraph> {
    use crate::interpreter::abstraction::room_x_lane_mask;
    use crate::interpreter::state::FILTER_BAND;
    use crate::rewrite::checkpoint;

    engine.disable_frontier();
    engine.record_pos_graph();
    let win_x = crate::game_runner::win_room_x();
    for f in from.max(1)..frames {
        let t = std::time::Instant::now();
        let states = checkpoint::load_frame_states(dir, f)
            .map_err(|e| anyhow!("loading frame batch f{:03}: {:#}", f, e))?;
        // Won lanes are absorbing - the forward pass drops them after
        // saving (`absorb_won_lanes`), so it never stepped them and the
        // table must not claim it did. The transition INTO the win is
        // recorded here, by the frame that takes it.
        let mut batch = Vec::with_capacity(states.len());
        let mut lanes = 0usize;
        for state in states {
            let keep: Vec<bool> =
                room_x_lane_mask(&state, win_x).into_iter().map(|w| !w).collect();
            let kept = keep.iter().filter(|b| **b).count();
            if kept == 0 {
                continue;
            }
            lanes += kept;
            batch.push(if kept == state.vector_size {
                state
            } else {
                state.filter_by_mask_clone(&keep, FILTER_BAND)
            });
        }
        if batch.is_empty() {
            continue;
        }
        // Expand in bounded groups. The replay takes the PHASED path, which
        // accumulates a frame's whole raw successor set before merging, and
        // that transient is what OOMed the edge sweep on room (0,0): a
        // 2M-lane batch of room (1,0) at f065 was already 28 GB in one go.
        // Nothing here needs the outputs to meet, since the observer has
        // read their positions by the time the group is dropped.
        let group_lanes: usize = std::env::var("CELESTE_POSGRAPH_GROUP_LANES")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(250_000);
        let mut group: Vec<crate::interpreter::state::State> = Vec::new();
        let mut group_size = 0usize;
        let mut queue: std::collections::VecDeque<_> = batch.into();
        while let Some(state) = queue.pop_front() {
            group_size += state.vector_size;
            group.push(state);
            if group_size < group_lanes && !queue.is_empty() {
                continue;
            }
            let deopt_events = engine.deopt_events();
            engine.restore(std::mem::take(&mut group), None, deopt_events)?;
            group_size = 0;
            engine.step().map_err(|e| anyhow!("replaying frame f{:03}: {:#}", f, e))?;
            // The outputs are only needed for their positions, which the
            // observer already read; drop them before the next group.
            let deopt_events = engine.deopt_events();
            engine.restore(Vec::new(), None, deopt_events)?;
        }
        if f % 10 == 0 || f + 1 == frames {
            println!(
                "  posgraph f{:03}: {} lanes, {} pairs so far ({:.1}s)",
                f,
                lanes,
                engine.pos_graph_pairs().unwrap_or(0),
                t.elapsed().as_secs_f64()
            );
        }
    }
    engine
        .take_pos_graph(frames)
        .ok_or_else(|| anyhow!("recording was enabled but produced no table"))
}

/// The per-lane origin column the recorder injects: each lane carries the
/// CELL its frame-input lane was in. Distinct from the sweep's
/// `SWEEP_ORIGIN`, which carries a row id.
pub const POS_ORIGIN: &str = "__pos_origin";

/// Collects the table while a run steps frames.
///
/// Attribution is PER LANE, via `POS_ORIGIN`: the recorder tags each input
/// lane with its own cell and reads the tag back off each output lane, so a
/// pair is a real transition rather than a chunk-wide cross product. The
/// first version of this was read-only and recorded every source cell of a
/// chunk against every destination cell of it; that is conservative and
/// completely useless - it kept 93.6% of the unfiltered candidate set,
/// because an 8,000-lane chunk spans thousands of cells and the cross
/// product squares that. See BENCHMARK_DATA.md.
///
/// The column is why recording belongs in the REPLAY and not in the forward
/// pass. A per-lane column is per-lane distinct, so it forbids the boundary
/// dedup that decides how coarse the whole over-approximation is: a tagged
/// forward pass would be a DIFFERENT SEARCH from the certified one. The
/// replay runs with `disable_frontier` and so has no boundary dedup to
/// lose, which is exactly why the cost lands there. `AbstractRun::step`
/// refuses to record on a streaming run rather than trusting that.
///
/// The tag is a cell, not a row id, on purpose: ~8,000 distinct values
/// instead of 213M, so what lane dedup remains inside a frame still fires.
pub struct PosObserver {
    /// Per-lane `(src cell, dst cell)` pairs, pushed under a lock by
    /// whichever worker ran the chunk. Folded into the table in `flush`, off
    /// the workers' critical section.
    pending: std::sync::Mutex<Vec<(u16, u16)>>,
    graph: std::sync::Mutex<PosGraphBuilder>,
}

impl Default for PosObserver {
    fn default() -> Self {
        Self { pending: std::sync::Mutex::new(Vec::new()), graph: Default::default() }
    }
}

impl PosObserver {
    /// Tag every input lane with its own cell, so the outputs can be
    /// attributed back to it.
    pub fn tag(&self, state: &mut State) -> Result<()> {
        let cells: Vec<u32> = state_cells(state)?.into_iter().map(u32::from).collect();
        crate::interpreter::deopt_collect::inject_named(state, POS_ORIGIN, &cells);
        Ok(())
    }

    /// Read the tags back off one chunk's outputs and pair each with the
    /// cell that output lane is in.
    pub fn record(&self, outputs: &[State]) -> Result<()> {
        let mut pairs = Vec::new();
        for out in outputs {
            let srcs = crate::interpreter::deopt_collect::read_origins_named(out, POS_ORIGIN);
            let dsts = state_cells(out)?;
            if srcs.len() != dsts.len() {
                return Err(anyhow!(
                    "pos observer: {} origins for {} lanes",
                    srcs.len(),
                    dsts.len()
                ));
            }
            pairs.extend(srcs.iter().zip(&dsts).map(|(&s, &d)| (s as u16, d)));
        }
        pairs.sort_unstable();
        pairs.dedup();
        self.pending.lock().expect("pos observer").extend(pairs);
        Ok(())
    }

    /// Fold the frame's observations into the table. Called once per frame
    /// so the pending list stays a frame's worth, not a run's.
    pub fn flush(&self) {
        let pending = std::mem::take(&mut *self.pending.lock().expect("pos observer"));
        let mut graph = self.graph.lock().expect("pos observer");
        for (s, d) in pending {
            graph.record(s, d);
        }
    }

    pub fn pairs(&self) -> usize {
        self.graph.lock().expect("pos observer").pairs()
    }

    pub fn build(self, frames: u32) -> PosGraph {
        self.flush();
        self.graph.into_inner().expect("pos observer").build(frames)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cells_round_trip_and_reject_out_of_grid() {
        assert_eq!(cell_of(0, 0).unwrap(), (64 * GRID + 64) as u16);
        assert_eq!(cell_xy(cell_of(3, -7).unwrap()).unwrap(), (3, -7));
        assert!(cell_of(-64, 0).is_ok());
        assert!(cell_of(-65, 0).is_err());
        assert!(cell_of(0, 192).is_err());
        // The one grid position that would alias the no-position node.
        assert!(cell_of(191, 191).is_err());
        assert_eq!(cell_xy(NO_CELL), None);
    }

    #[test]
    fn builder_dedups_and_csr_round_trips() {
        let mut b = PosGraphBuilder::default();
        b.record(7, 3);
        b.record(5, 3);
        b.record(7, 3); // duplicate
        b.record(9, NO_CELL);
        assert_eq!(b.pairs(), 3);
        let g = b.build(42);
        assert_eq!(g.frames(), 42);
        assert_eq!(g.srcs_of(3), &[5, 7], "sorted and deduped");
        assert_eq!(g.srcs_of(NO_CELL), &[9], "the no-position node participates");
        assert_eq!(g.srcs_of(4), &[] as &[u16]);
        assert_eq!(g.pairs(), 3);
        assert_eq!(g.live_cells(), 2);
    }

    #[test]
    fn union_keeps_an_unrecorded_destination_as_itself() {
        let mut b = PosGraphBuilder::default();
        b.record(7, 3);
        let g = b.build(1);
        let mut out = vec![0u64; CELL_WORDS];
        let is_set = |out: &[u64], c: u16| out[c as usize / 64] & (1 << (c % 64)) != 0;

        g.union_into(&[3], &mut out);
        assert!(is_set(&out, 7));
        assert!(!is_set(&out, 3), "the destination itself is not a predecessor");

        // A destination nothing was ever seen to step into: the row that is
        // already there must survive, so the cell keeps itself.
        g.union_into(&[4], &mut out);
        assert!(is_set(&out, 4));
        assert_eq!(out.iter().map(|w| w.count_ones()).sum::<u32>(), 1);
    }

    #[test]
    fn merge_is_the_union_of_two_builders() {
        let mut a = PosGraphBuilder::default();
        a.record(1, 10);
        a.record(2, 10);
        let mut b = PosGraphBuilder::default();
        b.record(2, 10);
        b.record(3, 11);
        a.merge(b);
        let g = a.build(7);
        assert_eq!(g.srcs_of(10), &[1, 2]);
        assert_eq!(g.srcs_of(11), &[3]);
    }

    /// A table that covers fewer frames than the sweep needs would silently
    /// under-generate candidates, so the coverage travels with the file -
    /// and the format that had no room for it is refused, not guessed at.
    #[test]
    fn a_saved_table_carries_its_frame_coverage_and_extends() {
        let dir = std::env::temp_dir().join(format!("posgraph-test-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();

        let mut b = PosGraphBuilder::default();
        b.record(1, 10);
        b.build(50).save(&dir).unwrap();
        let loaded = PosGraph::load(&dir).unwrap().expect("just saved");
        assert_eq!(loaded.frames(), 50);
        assert_eq!(loaded.srcs_of(10), &[1]);

        // Extension: the old table back to a builder, plus the new frames.
        let mut extended = loaded.into_builder();
        extended.record(2, 10);
        let extended = extended.build(60);
        assert_eq!(extended.srcs_of(10), &[1, 2]);
        assert_eq!(extended.frames(), 60);

        // The first format wrote no frame count; reading it as one would
        // invent coverage the table does not have.
        let mut raw = std::fs::read(dir.join("posgraph.bin")).unwrap();
        raw[..4].copy_from_slice(b"C8PW");
        std::fs::write(dir.join("posgraph.bin"), &raw).unwrap();
        let err = match PosGraph::load(&dir) {
            Err(e) => e.to_string(),
            Ok(_) => panic!("the frame-count-less format must be refused"),
        };
        assert!(err.contains("frame coverage"), "{}", err);

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
