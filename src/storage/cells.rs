//! The player-position CELLS: a state's player position on a grid relative
//! to the START room (cart coordinates are room-local: a crossing would look
//! like a 128 px teleport); the next room is one ROOM_PX to the right
//! whichever edge it was left by.

use anyhow::{anyhow, Result};

use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Col, Rt2, AV};

/// Side of the position grid, in pixels: the start room plus a room's width
/// past it in each axis (room (0,0) exits upward, landing at x ~238).
pub(crate) const GRID: i32 = 512;
pub(crate) const ORIGIN: i32 = -64;
const ROOM_PX: i32 = 128;

/// The cell of a row with no player object (e.g. after a death): a normal
/// node, past the end of the grid so no position aliases it.
pub(crate) const NO_CELL: u32 = (GRID * GRID) as u32;

/// THE STORAGE REGIONS: `SIDE` x `SIDE` cells (an entry's cells: a `u64`
/// mask), aligned at 0 as the kernels' `RegionGrid::of` (the grid's origin
/// -64 is a multiple of `SIDE`), each inside one kernel region (`--region`:
/// a multiple of `SIDE` px). A SLOT is a region's place on the grid,
/// row-major, `NO_CELL`'s last.
pub(crate) const SIDE: u32 = 8;
const _: () = assert!(SIDE * SIDE == 64 && ORIGIN % SIDE as i32 == 0, "a storage region's cells are a u64 mask, on the grid's origin");
pub(crate) const PER_ROW: u32 = GRID as u32 / SIDE;
/// Slots per shape: the grid's regions and `NO_CELL`'s.
pub(crate) const SLOTS: u32 = PER_ROW * PER_ROW + 1;
pub(crate) const NO_CELL_SLOT: u32 = SLOTS - 1;

/// A cell's region slot and its index in the region.
#[inline]
pub(crate) fn slot_local(cell: u32) -> (u32, u32) {
    if cell == NO_CELL {
        return (NO_CELL_SLOT, 0);
    }
    let (gx, gy) = (cell % GRID as u32, cell / GRID as u32);
    ((gy / SIDE) * PER_ROW + gx / SIDE, (gy % SIDE) * SIDE + gx % SIDE)
}

/// `slot_local` inverted.
#[inline]
pub(crate) fn cell(slot: u32, local: u32) -> u32 {
    if slot == NO_CELL_SLOT {
        debug_assert_eq!(local, 0);
        return NO_CELL;
    }
    let (rx, ry) = (slot % PER_ROW, slot / PER_ROW);
    let (gx, gy) = (rx * SIDE + local % SIDE, ry * SIDE + local / SIDE);
    gy * GRID as u32 + gx
}

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

/// A cell's start-room-relative pixels; `None` for `NO_CELL`.
pub fn cell_xy(cell: u32) -> Option<(i32, i32)> {
    (cell != NO_CELL).then(|| (cell as i32 % GRID + ORIGIN, cell as i32 / GRID + ORIGIN))
}

/// A numeric column's whole parts per lane (`None` if a lane is neither a
/// number nor an interval). An INTERVAL's cell is its low corner: ONE rule,
/// shared with the kernel's `pos_sources`.
pub(crate) fn whole_i16_col<N: Arithmetic>(rt2: &Rt2<N>, cell: u32) -> Option<Vec<i16>> {
    match &rt2.cols[cell as usize] {
        Col::U(AV::Num(n)) => Some(vec![n.whole_i16()?; rt2.width]),
        Col::U(AV::Ival(a, _)) => Some(vec![a.whole_i16()?; rt2.width]),
        Col::N(vs) => vs.iter().map(|n| n.whole_i16()).collect(),
        Col::I(vs) => vs.iter().map(|(a, _)| a.whole_i16()).collect(),
        Col::V(vs) => vs
            .iter()
            .map(|v| match v {
                AV::Num(n) => n.whole_i16(),
                AV::Ival(a, _) => a.whole_i16(),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

/// A position column's whole-pixel RANGE per lane (`None` if a lane is
/// neither number nor interval), for the win tests.
pub(crate) fn whole_range_col<N: Arithmetic>(rt2: &Rt2<N>, cell: u32) -> Option<Vec<(i16, i16)>> {
    let one = |v: AV<N>| -> Option<(i16, i16)> {
        match v {
            AV::Num(n) => Some((n.whole_i16()?, n.whole_i16()?)),
            AV::Ival(a, b) => Some((a.whole_i16()?, b.whole_i16()?)),
            _ => None,
        }
    };
    match &rt2.cols[cell as usize] {
        Col::U(v) => one(*v).map(|r| vec![r; rt2.width]),
        Col::N(vs) => vs.iter().map(|n| Some((n.whole_i16()?, n.whole_i16()?))).collect(),
        Col::I(vs) => vs.iter().map(|(a, b)| Some((a.whole_i16()?, b.whole_i16()?))).collect(),
        Col::V(vs) => vs.iter().map(|v| one(*v)).collect(),
    }
}

/// Per-lane cell of a block (`room.x`/`room.y` must be numbers; no player or
/// a non-numeric position: `NO_CELL`); the start room is its `room`.
pub fn block_cells<N: Arithmetic>(rt2: &Rt2<N>) -> Result<Vec<u32>> {
    let anchors = celeste_engine::shape::Anchors::of(rt2);
    let start = (rt2.room.x, rt2.room.y);
    let lanes = rt2.width;
    let axis = |c: u32| whole_i16_col(rt2, c);
    let [rxc, ryc] = anchors.room.ok_or_else(|| anyhow!("block_cells: no readable `room` global with `x` and `y`"))?;
    let (Some(rx), Some(ry)) = (axis(rxc), axis(ryc)) else {
        return Err(anyhow!("block_cells: room.x/room.y are not numbers"));
    };
    let Some(p) = anchors.position else {
        return Ok(vec![NO_CELL; lanes]);
    };
    let (Some(px), Some(py)) = (axis(p.x), axis(p.y)) else {
        return Ok(vec![NO_CELL; lanes]);
    };
    (0..lanes)
        .map(|i| {
            let (ox, oy) = room_offset(start, rx[i], ry[i])?;
            cell_of(px[i] as i32 + ox, py[i] as i32 + oy)
        })
        .collect()
}

/// The labelling offset of room `(rx, ry)` in a search from `start`: 0, or
/// `game_runner::win_room` one ROOM_PX right (the wrap (7,y) -> (0,y+1) too).
/// No other room occurs: a death reloads the start room.
pub(crate) fn room_offset(start: (i16, i16), rx: i16, ry: i16) -> Result<(i32, i32)> {
    let win = crate::game_runner::win_room(start);
    if (rx, ry) == start {
        Ok((0, 0))
    } else if (rx, ry) == win {
        Ok((ROOM_PX, 0))
    } else {
        Err(anyhow!("a row in room ({rx},{ry}): neither the start room {start:?} nor the one it exits to {win:?}"))
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

    /// Slots and cells round-trip through the storage regions, each inside
    /// a 16 px kernel region.
    #[test]
    fn cells_round_trip_through_regions() {
        let side = SIDE as i32;
        for (x, y) in [(-64, -64), (-1, 0), (0, -1), (5, 7), (127, 127), (200, 40), (447, 447)] {
            let c = cell_of(x, y).unwrap();
            let (slot, local) = slot_local(c);
            assert!(slot < NO_CELL_SLOT && local < SIDE * SIDE);
            assert_eq!(cell(slot, local), c);
            let (x0, y0) = ((slot % PER_ROW) as i32 * side - 64, (slot / PER_ROW) as i32 * side - 64);
            assert_eq!((x0, y0), (x.div_euclid(side) * side, y.div_euclid(side) * side));
            let k = |v: i32| v.div_euclid(16);
            assert_eq!((k(x0), k(y0)), (k(x0 + side - 1), k(y0 + side - 1)));
        }
        assert_eq!(slot_local(NO_CELL), (NO_CELL_SLOT, 0));
        assert_eq!(cell(NO_CELL_SLOT, 0), NO_CELL);
    }
}
