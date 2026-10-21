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
/// a non-numeric position: `NO_CELL`); the start room is its cache's.
pub fn block_cells<N: Arithmetic>(rt2: &Rt2<N>) -> Result<Vec<u32>> {
    let ids = crate::compiled::ids();
    let start = (rt2.cache.room_x, rt2.cache.room_y);
    let lanes = rt2.width;
    let axis = |obj: u32, f: u32| rt2.obj_field_cell(obj, f).and_then(|c| whole_i16_col(rt2, c));
    let room = rt2
        .global_target(ids.g_room)
        .ok_or_else(|| anyhow!("block_cells: no readable `room` global"))?;
    let (Some(rx), Some(ry)) = (axis(room, ids.f_x), axis(room, ids.f_y)) else {
        return Err(anyhow!("block_cells: room.x/room.y are not numbers"));
    };
    let Some(obj) = rt2.position_object() else {
        return Ok(vec![NO_CELL; lanes]);
    };
    let (Some(px), Some(py)) = (axis(obj, ids.f_x), axis(obj, ids.f_y)) else {
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
}
