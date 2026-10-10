//! The player-position cells, and the position graph: the successor
//! relation PROJECTED ONTO THE PLAYER'S POSITION, "which positions can step
//! into this one?". Recorded by the forward (`PosObserver`), read by the
//! level -1 probe and the UI, pinned as a gate
//! (`gates/posgraph_room10_f044.txt`).
//!
//! It has one node per position cell, never one per row: never a set of
//! predecessor states. It is horizon-independent and RECORDED, never guessed.
//!
//! Positions are relative to the START room (cart coordinates are
//! room-local, so a crossing would look like a 128 px teleport). The next
//! room is labelled one ROOM_PX to the right whichever edge it was left by.

use anyhow::{anyhow, Result};

use celeste_engine::runtime2::{Col, Rt2, AV};

/// Side of the position grid, in pixels: the start room plus a room's width
/// past it in each axis (room (0,0) exits upward, landing at x ~238).
pub const GRID: i32 = 512;
/// Pixel coordinate mapped to grid index 0.
pub const ORIGIN: i32 = -64;
/// Side of a room in pixels - the stride between rooms in the grid.
const ROOM_PX: i32 = 128;

/// The cell of a row with no player object (e.g. after a death): a normal
/// node, past the end of the grid so no position aliases it.
pub const NO_CELL: u32 = (GRID * GRID) as u32;

/// Number of nodes, including `NO_CELL`.
pub const CELL_COUNT: usize = (GRID * GRID) as usize + 1;
/// Grid cell of a whole-pixel start-room-relative position; an error
/// outside the grid, never a clamp.
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

/// A numeric column's whole parts per lane (`None` if a lane is neither a
/// number nor an interval). An INTERVAL's cell is its low corner: ONE rule,
/// shared with the kernel's `pos_sources`.
pub(crate) fn whole_i16_col(rt2: &Rt2, cell: u32) -> Option<Vec<i16>> {
    match &rt2.cols[cell as usize] {
        Col::U(AV::Num(n)) => Some(vec![n.whole_part_as_i16(); rt2.width]),
        Col::U(AV::Ival(a, _)) => Some(vec![a.whole_part_as_i16(); rt2.width]),
        Col::N(vs) => Some(vs.iter().map(|n| n.whole_part_as_i16()).collect()),
        Col::I(vs) => Some(vs.iter().map(|(a, _)| a.whole_part_as_i16()).collect()),
        Col::V(vs) => vs
            .iter()
            .map(|v| match v {
                AV::Num(n) => Some(n.whole_part_as_i16()),
                AV::Ival(a, _) => Some(a.whole_part_as_i16()),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

/// A position column's whole-pixel RANGE per lane (`None` if a lane is
/// neither number nor interval), for the win tests.
pub fn whole_range_col(rt2: &Rt2, cell: u32) -> Option<Vec<(i16, i16)>> {
    let one = |v: AV| -> Option<(i16, i16)> {
        match v {
            AV::Num(n) => Some((n.whole_part_as_i16(), n.whole_part_as_i16())),
            AV::Ival(a, b) => Some((a.whole_part_as_i16(), b.whole_part_as_i16())),
            _ => None,
        }
    };
    match &rt2.cols[cell as usize] {
        Col::U(v) => one(*v).map(|r| vec![r; rt2.width]),
        Col::N(vs) => Some(vs.iter().map(|n| (n.whole_part_as_i16(), n.whole_part_as_i16())).collect()),
        Col::I(vs) => Some(vs.iter().map(|(a, b)| (a.whole_part_as_i16(), b.whole_part_as_i16())).collect()),
        Col::V(vs) => vs.iter().map(|v| one(*v)).collect(),
    }
}

/// The block's player object (`player`, else `player_spawn`), if any. Per
/// block: lanes share structure.
pub fn player_object(rt2: &Rt2) -> Option<u32> {
    rt2.position_object(crate::compiled::ids())
}

/// Per-lane cell of a block. `room.x`/`room.y` must be numbers; no player
/// object, or a position that is not numeric, is `NO_CELL`.
pub fn block_cells(rt2: &Rt2) -> Result<Vec<u32>> {
    let ids = crate::compiled::ids();
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
            let (ox, oy) = room_offset(rx[i], ry[i])?;
            cell_of(px[i] as i32 + ox, py[i] as i32 + oy)
        })
        .collect()
}

/// The labelling offset of a row in room `(rx, ry)`: the start room at 0,
/// `game_runner::win_room` one ROOM_PX to the right (also across the wrap
/// (7,y) -> (0,y+1)). No other room occurs: a death reloads the start room.
pub fn room_offset(rx: i16, ry: i16) -> Result<(i32, i32)> {
    let start = crate::game_runner::start_room();
    if (rx, ry) == start {
        Ok((0, 0))
    } else if (rx, ry) == crate::game_runner::win_room() {
        Ok((ROOM_PX, 0))
    } else {
        Err(anyhow!("a row in room ({rx},{ry}): neither the start room {start:?} nor the one it exits to {:?}", crate::game_runner::win_room()))
    }
}

/// The finished graph, by destination: each cell's predecessor cells, a
/// sorted CSR.
#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct PosGraph {
    /// `srcs[offsets[d] .. offsets[d + 1]]` for destination cell `d`.
    offsets: Vec<u32>,
    srcs: Vec<u32>,
}

impl PosGraph {
    /// Persist (`frame::save_pos_graph`).
    pub fn save(&self, path: &std::path::Path) -> anyhow::Result<()> {
        crate::search::checkpoint::save_value_to(path, self)
    }

    pub fn load(path: &std::path::Path) -> anyhow::Result<Self> {
        crate::search::checkpoint::load_value_from(path)
    }

    pub fn pairs(&self) -> usize {
        self.srcs.len()
    }

    /// The recorded source cells of destination cell `dst`, sorted.
    pub fn sources(&self, dst: u32) -> &[u32] {
        let d = dst as usize;
        if d + 1 >= self.offsets.len() {
            return &[];
        }
        &self.srcs[self.offsets[d] as usize..self.offsets[d + 1] as usize]
    }

    /// Destination cells with at least one recorded predecessor.
    pub fn live_cells(&self) -> usize {
        self.offsets.windows(2).filter(|w| w[1] > w[0]).count()
    }

    /// `(pairs, order-independent hash of the edge set)`: the posgraph gate.
    pub fn fingerprint(&self) -> (usize, u64) {
        use celeste_engine::runtime2::mix64;
        let mut acc = 0u64;
        for d in 0..self.offsets.len().saturating_sub(1) {
            for &s in self.srcs_of(d as u32) {
                acc = acc.wrapping_add(mix64((d as u64) << 32 ^ mix64(s as u64 + 1)));
            }
        }
        (self.pairs(), acc)
    }

    pub fn srcs_of(&self, dst: u32) -> &[u32] {
        if self.offsets.is_empty() {
            return &[];
        }
        let (lo, hi) = (self.offsets[dst as usize] as usize, self.offsets[dst as usize + 1] as usize);
        &self.srcs[lo..hi]
    }
}

/// Accumulates `(dst, src)` pairs: per destination a small sorted set of
/// sources (tens each), not a dense bitmap or a hash set of pairs.
#[derive(Default, Clone)]
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

    pub fn pairs(&self) -> usize {
        self.by_dst.values().map(|v| v.len()).sum()
    }

    pub fn build(self) -> PosGraph {
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
        PosGraph { offsets, srcs }
    }
}


/// Collects the graph while the forward steps frames. Attribution is AT
/// EMISSION: the frame step records `(input row's cell, output row's cell)`
/// as it produces each row, before any dedup; nothing is stored on a row.
pub struct PosObserver {
    /// `(src cell, dst cell)` pairs pushed by the workers, folded into the
    /// graph in `flush`.
    pending: std::sync::Mutex<Vec<(u32, u32)>>,
    graph: std::sync::Mutex<PosGraphBuilder>,
}

impl Default for PosObserver {
    fn default() -> Self {
        Self { pending: std::sync::Mutex::new(Vec::new()), graph: Default::default() }
    }
}

impl PosObserver {
    /// An observer already holding `graph`'s pairs (a resumed forward).
    pub fn from_graph(graph: &PosGraph) -> Self {
        let mut b = PosGraphBuilder::default();
        for d in 0..CELL_COUNT {
            let (lo, hi) = (graph.offsets[d] as usize, graph.offsets[d + 1] as usize);
            if lo < hi {
                b.by_dst.insert(d as u32, graph.srcs[lo..hi].to_vec());
            }
        }
        Self { pending: std::sync::Mutex::new(Vec::new()), graph: std::sync::Mutex::new(b) }
    }

    /// Record `(src cell, dst cell)` edges observed at emission.
    pub fn record_pairs(&self, pairs: impl Iterator<Item = (u32, u32)>) {
        self.pending.lock().expect("pos observer").extend(pairs);
    }

    /// Fold the pending observations into the graph.
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

    pub fn build(self) -> PosGraph {
        self.flush();
        self.graph.into_inner().expect("pos observer").build()
    }

    /// The graph as recorded so far; the observer keeps recording.
    pub fn snapshot(&self) -> PosGraph {
        self.flush();
        self.graph.lock().expect("pos observer").clone().build()
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
        // Room (0,0)'s exit lanes land here.
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
        let g = b.build();
        assert_eq!(g.srcs_of(3), &[5, 7], "sorted and deduped");
        assert_eq!(g.srcs_of(NO_CELL), &[9], "the no-position node participates");
        assert_eq!(g.srcs_of(4), &[] as &[u32]);
        assert_eq!(g.pairs(), 3);
        assert_eq!(g.live_cells(), 2);
    }
}
