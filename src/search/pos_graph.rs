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
//!
//! The next room is placed one ROOM_PX to the right, because rooms advance
//! along `room.x`. That is a labelling, not a geometry: room (0,0) is left
//! by going UP, so its exit lanes keep their local x (~110) and land at
//! start-relative x ~238. The grid has to hold the start room AND a whole
//! room's width past it in both axes, which is why it is 512 wide and why
//! cells no longer fit in a u16.

use anyhow::{anyhow, Result};
use std::path::Path;

use crate::interpreter::abstraction::{player_xy_per_lane, room_xy_per_lane};
use crate::interpreter::state::State;
use celeste_engine::runtime2::{Col, Rt2, AV};

/// Side of the position grid, in pixels. A room is 128x128; the grid holds
/// the start room plus a whole room's width past it in each axis, because a
/// row that has crossed into the next room is labelled one ROOM_PX to the
/// right whichever edge it actually left by. 256 was not enough: room (0,0)
/// exits upward and its crossing lanes land at start-relative x ~238.
pub const GRID: i32 = 512;
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
/// A node of its own past the end of the grid, so no position can alias it.
pub const NO_CELL: u32 = (GRID * GRID) as u32;

/// Number of nodes, including `NO_CELL`.
pub const CELL_COUNT: usize = (GRID * GRID) as usize + 1;
/// `u64` words in a bitmap over all nodes.
pub const CELL_WORDS: usize = CELL_COUNT.div_ceil(64);

/// Grid cell of a whole-pixel START-ROOM-RELATIVE position. Loud rather
/// than clamping: a position outside the grid would silently distort the
/// whole table.
pub fn cell_of(x: i32, y: i32) -> Result<u32> {
    let (gx, gy) = (x - ORIGIN, y - ORIGIN);
    if !(0..GRID).contains(&gx) || !(0..GRID).contains(&gy) {
        return Err(anyhow!(
            "position ({}, {}) is outside the position grid ({}..{} in both axes)",
            x,
            y,
            ORIGIN,
            ORIGIN + GRID - 1
        ));
    }
    Ok((gy * GRID + gx) as u32)
}

/// Human-readable start-room-relative pixels of a cell.
pub fn cell_xy(cell: u32) -> Option<(i32, i32)> {
    (cell != NO_CELL).then(|| (cell as i32 % GRID + ORIGIN, cell as i32 / GRID + ORIGIN))
}

/// A numeric column's whole parts, one per lane, or `None` if any lane is
/// not a plain number (an interval, a pointer, nil).
pub(crate) fn whole_i16_col(rt2: &Rt2, cell: u32) -> Option<Vec<i16>> {
    match &rt2.cols[cell as usize] {
        Col::U(AV::Num(n)) => Some(vec![n.whole_part_as_i16(); rt2.width]),
        Col::N(vs) => Some(vs.iter().map(|n| n.whole_part_as_i16()).collect()),
        Col::V(vs) => vs
            .iter()
            .map(|v| match v {
                AV::Num(n) => Some(n.whole_part_as_i16()),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

/// The player object of a block - the `player` instance, or the
/// `player_spawn` one during the spawn animation - if it has one. Blocks
/// share structure across lanes, so this is per block, not per lane.
pub(crate) fn player_object(rt2: &Rt2) -> Option<u32> {
    let ids = crate::compiled::ids();
    rt2.player_objects(ids)
        .first()
        .copied()
        .or_else(|| rt2.objects_of_type(ids, ids.g_player_spawn).first().copied())
}

/// Per-lane cell of an engine block: the SAME rule as `state_cells`, read
/// off the block's columns. `room.x`/`room.y` must be numbers (a block
/// without a readable room is an error); a block with no player object, or
/// a player whose `x`/`y` is not a plain number, is at `NO_CELL`.
pub fn block_cells(rt2: &Rt2) -> Result<Vec<u32>> {
    let ids = crate::compiled::ids();
    let start = crate::game_runner::start_room();
    let lanes = rt2.width;
    let axis = |obj: u32, f: u32| rt2.obj_field_cell(obj, f).and_then(|c| whole_i16_col(rt2, c));
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow!("block_cells: no readable `room` global"))?;
    let (Some(rx), Some(ry)) = (axis(room, ids.f_x), axis(room, ids.f_y)) else {
        return Err(anyhow!("block_cells: room.x/room.y are not numbers"));
    };
    let Some(obj) = player_object(rt2) else {
        return Ok(vec![NO_CELL; lanes]);
    };
    let (Some(px), Some(py)) = (axis(obj, ids.f_x), axis(obj, ids.f_y)) else {
        return Ok(vec![NO_CELL; lanes]);
    };
    (0..lanes)
        .map(|i| {
            cell_of(
                px[i] as i32 + (rx[i] - start.0) as i32 * ROOM_PX,
                py[i] as i32 + (ry[i] - start.1) as i32 * ROOM_PX,
            )
        })
        .collect()
}

/// Per-lane cell of a boundary state.
pub fn state_cells(state: &State) -> Result<Vec<u32>> {
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
    srcs: Vec<u32>,
    /// The `frames` the table was recorded for: it has seen every
    /// transition taken out of frames `1..frames`. Horizon-independent but
    /// NOT frame-independent - extending the forward pass adds frames whose
    /// transitions this has never seen, and a missing pair means the sweep's
    /// filter never generates that candidate and nothing notices. Stored so
    /// the sweep can check it rather than assume.
    frames: u32,
    /// The config fingerprint of the forward pass whose batches were
    /// replayed. A table records the transitions of ONE search; replayed
    /// against a different one - a recipe change, a precision level, or an
    /// interpreter fix that makes the abstraction finer - it describes
    /// positions that search never visits and, worse, misses ones it does.
    /// Checked on load, because the failure is silent: a missing pair means
    /// the sweep never generates that candidate.
    fingerprint: String,
}

impl PosGraph {
    pub fn pairs(&self) -> usize {
        self.srcs.len()
    }

    /// The frame count this table covers (transitions out of `1..frames`).
    pub fn frames(&self) -> u32 {
        self.frames
    }

    /// The forward pass this table was recorded from.
    pub fn fingerprint(&self) -> &str {
        &self.fingerprint
    }

    /// Back to a builder, so a table can be extended over new frames.
    pub fn into_builder(self) -> PosGraphBuilder {
        let mut by_dst: rustc_hash::FxHashMap<u32, Vec<u32>> = Default::default();
        for d in 0..self.offsets.len().saturating_sub(1) {
            let (lo, hi) = (self.offsets[d] as usize, self.offsets[d + 1] as usize);
            if lo != hi {
                by_dst.insert(d as u32, self.srcs[lo..hi].to_vec());
            }
        }
        PosGraphBuilder { by_dst }
    }

    /// Destination cells with at least one recorded predecessor.
    pub fn live_cells(&self) -> usize {
        self.offsets.windows(2).filter(|w| w[1] > w[0]).count()
    }

    pub fn srcs_of(&self, dst: u32) -> &[u32] {
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
    pub fn union_into(&self, dsts: &[u32], out: &mut [u64]) {
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
    /// The header carries the frame coverage, the grid the cells are
    /// numbered in, and the fingerprint of the forward pass they came from;
    /// `load` refuses anything it does not recognise. A table short of the
    /// horizon silently under-generates candidates, one numbered in a
    /// different grid is a permutation of the room, and one from a different
    /// search describes transitions that search never took. `C8PW` (no
    /// frame count), `C8PX` (no grid) and `C8PY` (no fingerprint) are all
    /// refused rather than guessed at.
    pub fn save(&self, dir: &Path) -> Result<()> {
        use std::io::Write;
        let tmp = dir.join("tmp-posgraph.bin");
        {
            let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
            w.write_all(b"C8PZ")?;
            w.write_all(&GRID.to_le_bytes())?;
            w.write_all(&ORIGIN.to_le_bytes())?;
            w.write_all(&self.frames.to_le_bytes())?;
            w.write_all(&(self.fingerprint.len() as u32).to_le_bytes())?;
            w.write_all(self.fingerprint.as_bytes())?;
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
        for (old, missing) in
            [(b"C8PW", "frame coverage"), (b"C8PX", "position grid"), (b"C8PY", "forward pass")]
        {
            if &magic == old {
                return Err(anyhow!(
                    "{}: written by an older version, whose header does not pin \
                     the {} - delete it and let the sweep rebuild it",
                    path.display(),
                    missing
                ));
            }
        }
        if &magic != b"C8PZ" {
            return Err(anyhow!("{}: bad magic", path.display()));
        }
        let mut buf4 = [0u8; 4];
        file.read_exact(&mut buf4)?;
        let grid = i32::from_le_bytes(buf4);
        file.read_exact(&mut buf4)?;
        let origin = i32::from_le_bytes(buf4);
        if (grid, origin) != (GRID, ORIGIN) {
            return Err(anyhow!(
                "{}: cells are numbered in a {}px grid at origin {}, this \
                 build uses {}px at {} - the numbering is a permutation of \
                 the room, so delete it and let the sweep rebuild it",
                path.display(),
                grid,
                origin,
                GRID,
                ORIGIN
            ));
        }
        file.read_exact(&mut buf4)?;
        let frames = u32::from_le_bytes(buf4);
        file.read_exact(&mut buf4)?;
        let mut fp = vec![0u8; u32::from_le_bytes(buf4) as usize];
        file.read_exact(&mut fp)?;
        let fingerprint = String::from_utf8(fp)
            .map_err(|_| anyhow!("{}: fingerprint is not text", path.display()))?;
        let mut buf8 = [0u8; 8];
        file.read_exact(&mut buf8)?;
        let pairs = u64::from_le_bytes(buf8) as usize;
        let mut buf = vec![0u8; (CELL_COUNT + 1) * 4];
        file.read_exact(&mut buf)?;
        let offsets: Vec<u32> =
            buf.chunks_exact(4).map(|c| u32::from_le_bytes(c.try_into().unwrap())).collect();
        let mut buf = vec![0u8; pairs * 4];
        file.read_exact(&mut buf)?;
        let srcs: Vec<u32> =
            buf.chunks_exact(4).map(|c| u32::from_le_bytes(c.try_into().unwrap())).collect();
        if offsets[CELL_COUNT] as usize != pairs {
            return Err(anyhow!("{}: offsets end at {}, not {}", path.display(), offsets[CELL_COUNT], pairs));
        }
        Ok(Some(PosGraph { offsets, srcs, frames, fingerprint }))
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
    by_dst: rustc_hash::FxHashMap<u32, Vec<u32>>,
}

impl PosGraphBuilder {
    pub fn record(&mut self, src: u32, dst: u32) {
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

    /// `frames` and `fingerprint` record WHAT the table has seen (see the
    /// fields); they are the caller's claim, not a measurement.
    pub fn build(self, frames: u32, fingerprint: &str) -> PosGraph {
        let mut offsets = Vec::with_capacity(CELL_COUNT + 1);
        let mut srcs = Vec::with_capacity(self.pairs());
        let mut by_dst = self.by_dst;
        for d in 0..CELL_COUNT {
            offsets.push(srcs.len() as u32);
            if let Some(set) = by_dst.remove(&(d as u32)) {
                srcs.extend_from_slice(&set);
            }
        }
        offsets.push(srcs.len() as u32);
        PosGraph { offsets, srcs, frames, fingerprint: fingerprint.to_string() }
    }
}


/// Collects the table while a run steps frames.
///
/// Attribution is by PARTITION, not by a per-lane tag: while recording, the
/// forward pass regroups its frontier by player-position cell
/// (`frame::forward_frame`'s `by_cell`), so every frame-input block is
/// uniform in position. The recorder reads that ONE input cell off the
/// block's position column (`input_cell`) and pairs it with every output
/// cell, which is exact rather than a cross product: all the block's lanes
/// really did start at that cell.
///
/// The first version of this was read-only and recorded every source cell of
/// a chunk against every destination cell of it - conservative and useless
/// (kept 93.6% of the unfiltered candidate set, because an 8,000-lane chunk
/// spans thousands of cells and the cross product squares that). The second
/// used a per-lane `POS_ORIGIN` tag; that attributed correctly but, being
/// per-lane distinct, forbade ALL mid-frame dedup and ballooned room (0,0) to
/// 101 GB. Partitioning by input position keeps attribution exact while
/// letting lanes that share an input position AND converge still dedup within
/// the chunk. See BENCHMARK_DATA.md.
pub struct PosObserver {
    /// Per-lane `(src cell, dst cell)` pairs, pushed under a lock by
    /// whichever worker ran the chunk. Folded into the table in `flush`, off
    /// the workers' critical section.
    pending: std::sync::Mutex<Vec<(u32, u32)>>,
    graph: std::sync::Mutex<PosGraphBuilder>,
}

impl Default for PosObserver {
    fn default() -> Self {
        Self { pending: std::sync::Mutex::new(Vec::new()), graph: Default::default() }
    }
}

impl PosObserver {
    /// Start from an already-built table, so a RESUMED forward pass keeps
    /// the transitions of the frames it is not re-running. Without this the
    /// fused recording writes a table stamped with the full horizon but
    /// holding only the resumed tail, and a table that is too small makes
    /// the sweep drop real predecessors - silently, and in the direction
    /// that loses winning paths.
    pub fn seeded(graph: PosGraph) -> Self {
        Self {
            pending: std::sync::Mutex::new(Vec::new()),
            graph: std::sync::Mutex::new(graph.into_builder()),
        }
    }

    /// The single input cell of a frame-input block, given its position
    /// column. Every lane must be at the same cell - which the by-cell
    /// regrouping guarantees while recording (see the struct doc). A
    /// non-uniform block is a loud error, not a silent cross product: it
    /// means the regrouping was not by cell, and recording without it would
    /// attribute every source cell to every destination cell (the useless
    /// first version).
    pub fn input_cell(&self, cells: &[u32]) -> Result<u32> {
        let Some((&first, rest)) = cells.split_first() else {
            return Err(anyhow!("pos observer: empty input block"));
        };
        if let Some(&other) = rest.iter().find(|&&c| c != first) {
            return Err(anyhow!(
                "pos observer: input block spans cells {} and {} - the frontier \
                 must be regrouped by cell while recording (`forward_frame`'s \
                 `by_cell`)",
                first,
                other
            ));
        }
        Ok(first)
    }

    /// Record edges from one input cell to a set of output cells (the output
    /// block's position column). Sound because the input block is uniform in
    /// position (`input_cell`): all its lanes started at `c_in`, so every
    /// `dst` really is a successor of it.
    pub fn record_dsts(&self, c_in: u32, dsts: &[u32]) {
        let mut pairs: Vec<(u32, u32)> = dsts.iter().map(|&d| (c_in, d)).collect();
        pairs.sort_unstable();
        pairs.dedup();
        self.pending.lock().expect("pos observer").extend(pairs);
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

    pub fn build(self, frames: u32, fingerprint: &str) -> PosGraph {
        self.flush();
        self.graph.into_inner().expect("pos observer").build(frames, fingerprint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cells_round_trip_and_reject_out_of_grid() {
        assert_eq!(cell_of(0, 0).unwrap(), (64 * GRID + 64) as u32);
        assert_eq!(cell_xy(cell_of(3, -7).unwrap()).unwrap(), (3, -7));
        assert!(cell_of(-64, 0).is_ok());
        assert!(cell_of(-65, 0).is_err());
        // Room (0,0)'s exit lanes land here, and a 256px grid refused them.
        assert!(cell_of(238, -5).is_ok());
        assert!(cell_of(0, ORIGIN + GRID).is_err());
        // No position can alias the no-position node.
        assert!(cell_of(ORIGIN + GRID - 1, ORIGIN + GRID - 1).unwrap() < NO_CELL);
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
        let g = b.build(42, "fp");
        assert_eq!(g.frames(), 42);
        assert_eq!(g.srcs_of(3), &[5, 7], "sorted and deduped");
        assert_eq!(g.srcs_of(NO_CELL), &[9], "the no-position node participates");
        assert_eq!(g.srcs_of(4), &[] as &[u32]);
        assert_eq!(g.pairs(), 3);
        assert_eq!(g.live_cells(), 2);
    }

    #[test]
    fn union_keeps_an_unrecorded_destination_as_itself() {
        let mut b = PosGraphBuilder::default();
        b.record(7, 3);
        let g = b.build(1, "fp");
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
        let g = a.build(7, "fp");
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
        b.build(50, "abc123").save(&dir).unwrap();
        let loaded = PosGraph::load(&dir).unwrap().expect("just saved");
        assert_eq!(loaded.frames(), 50);
        assert_eq!(loaded.fingerprint(), "abc123");
        assert_eq!(loaded.srcs_of(10), &[1]);

        // Extension: the old table back to a builder, plus the new frames.
        let mut extended = loaded.into_builder();
        extended.record(2, 10);
        let extended = extended.build(60, "abc123");
        assert_eq!(extended.srcs_of(10), &[1, 2]);
        assert_eq!(extended.frames(), 60);

        // Each older format left out something the sweep cannot infer, so
        // each is refused by name rather than read hopefully.
        for (magic, missing) in
            [(b"C8PW", "frame coverage"), (b"C8PX", "position grid"), (b"C8PY", "forward pass")]
        {
            let mut raw = std::fs::read(dir.join("posgraph.bin")).unwrap();
            raw[..4].copy_from_slice(magic);
            std::fs::write(dir.join("posgraph.bin"), &raw).unwrap();
            let err = match PosGraph::load(&dir) {
                Err(e) => e.to_string(),
                Ok(_) => panic!("the {:?} format must be refused", magic),
            };
            assert!(err.contains(missing), "{}", err);
        }

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
