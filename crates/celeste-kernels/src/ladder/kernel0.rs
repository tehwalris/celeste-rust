// GENERATED from a TRACED frame (shape 0). Do not edit.
//
// One input shape, 4 output shapes, 49 distinct button
// assignments. See `trace::kernel` for what this interface is
// and why it is not the walk's.
#![allow(unused_variables, unused_mut, unused_imports, clippy::all)]
use celeste_engine::kernel::*;
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as IV};
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::{Rt2, Col, AV};
use celeste_engine::slots::{build_block, resolve_path, SCell};
use std::sync::Arc;

pub struct G<'a> { pub cart: &'a CartData, pub cache: &'a CollisionCache }

/// The canonical shape this kernel was traced for.
pub const SHAPE: u64 = 4684018979372124476;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c264: P8,
    pub c265: P8,
    pub c266: P8,
    pub c267: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("minutes", "num"),
    ("objects[0].collideable", "bool"),
    ("objects[0].delay", "num"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].state", "num"),
    ("objects[0].target.x", "num"),
    ("objects[0].target.y", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("pause_player", "bool"),
    ("seconds", "num"),
    ("will_restart", "bool"),
];

/// Per-lane inputs, in `ROW_SLOTS` order.
pub struct RowsIn {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c232: u16,
    pub c233: ZN,
    pub c262: u16,
    pub c263: u16,
    pub c268: ZN,
    pub c269: ZN,
    pub c242: u16,
    pub c270: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c43: u16,
    pub c85: ZN,
    pub c38: u16,
}

/// Cell ids for `ROW_SLOTS` in the bound block.
pub struct RowSlots {
    pub c87: u32,
    pub c39: u32,
    pub c84: u32,
    pub c20: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c232: u32,
    pub c233: u32,
    pub c262: u32,
    pub c263: u32,
    pub c268: u32,
    pub c269: u32,
    pub c242: u32,
    pub c270: u32,
    pub c271: u32,
    pub c244: u32,
    pub c245: u32,
    pub c272: u32,
    pub c273: u32,
    pub c248: u32,
    pub c249: u32,
    pub c43: u32,
    pub c85: u32,
    pub c38: u32,
}

/// Bind to a block by PATH. `None` means this block does not
/// have a slot the kernel needs, or holds it at the wrong kind -
/// either way the block takes the interpreter path.
pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {
    let cell = |p: &str| resolve_path(b, p).ok();
    let u = Uni {
        c264: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c265: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c266: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c267: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c232: cell("objects[0].collideable")?,
        c233: cell("objects[0].delay")?,
        c262: cell("objects[0].flip.x")?,
        c263: cell("objects[0].flip.y")?,
        c268: cell("objects[0].rem.x")?,
        c269: cell("objects[0].rem.y")?,
        c242: cell("objects[0].solids")?,
        c270: cell("objects[0].spd.x")?,
        c271: cell("objects[0].spd.y")?,
        c244: cell("objects[0].spr")?,
        c245: cell("objects[0].state")?,
        c272: cell("objects[0].target.x")?,
        c273: cell("objects[0].target.y")?,
        c248: cell("objects[0].x")?,
        c249: cell("objects[0].y")?,
        c43: cell("pause_player")?,
        c85: cell("seconds")?,
        c38: cell("will_restart")?,
    };
    Some((u, s))
}

/// Which slot stopped `bind`. `None` means it would have bound.
pub fn bind_why(b: &Rt2) -> Option<String> {
    for (p, kind) in UNI_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let ok = matches!(
            (*kind, col),
            ("num", Col::U(AV::Num(_))) | ("bool", Col::U(AV::Bool(_)))
        );
        if !ok {
            return Some(format!(
                "{}: block-uniform {} slot holds {:?}",
                p, kind, col
            ));
        }
    }
    for (p, kind) in ROW_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let lanes = (0..b.width).map(|i| col.at(i));
        let ok = match *kind {
            "num" => lanes.clone().all(|v| matches!(v, AV::Num(_))),
            _ => lanes.clone().all(|v| matches!(v, AV::Bool(_))),
        };
        if !ok {
            let bad = lanes.enumerate().find(|(_, v)| match *kind {
                "num" => !matches!(v, AV::Num(_)),
                _ => !matches!(v, AV::Bool(_)),
            });
            return Some(format!(
                "{}: per-lane {} slot holds {:?} at lane {:?}",
                p, kind, bad.map(|(_, v)| v), bad.map(|(i, _)| i)
            ));
        }
    }
    // Every slot resolves and every lane is the right kind, so
    // what stopped `rows` is REPRESENTATION: a column whose
    // values are right but whose storage the gather does not
    // accept (a `Col::V` of numbers where it wants `Col::N`).
    // `collapse_uniform_cols` is the usual missing step.
    None
}

/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by
/// repeating the last row; padded lanes are ignored by width.
pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {
    let at = |i: usize| -> usize { (lo + i).min(b.width - 1) };
    Some(RowsIn {
        c87: match &b.cols[s.c87 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c39: match &b.cols[s.c39 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c84: match &b.cols[s.c84 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c20: match &b.cols[s.c20 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c41: match &b.cols[s.c41 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c42: match &b.cols[s.c42 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c88: match &b.cols[s.c88 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c86: match &b.cols[s.c86 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c232: match &b.cols[s.c232 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c233: match &b.cols[s.c233 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c262: match &b.cols[s.c262 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c269: match &b.cols[s.c269 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c242: match &b.cols[s.c242 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c270: match &b.cols[s.c270 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c244: match &b.cols[s.c244 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c249: match &b.cols[s.c249 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c43: match &b.cols[s.c43 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c85: match &b.cols[s.c85 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c38: match &b.cols[s.c38 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
    })
}

// ---------------- outcome 0 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_0: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (43, "pause_player"),
    (156, "player_spawn.tile"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_0: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_0: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
];

pub const OUT_GLOBALS_0: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_0: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (152, 204),
    (153, 205),
    (154, 206),
    (155, 207),
    (157, 208),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 209),
    (172, 210),
    (173, 211),
    (175, 212),
    (176, 213),
    (178, 214),
    (180, 215),
    (182, 216),
    (184, 217),
    (186, 218),
    (189, 219),
    (192, 220),
    (194, 221),
    (196, 222),
    (197, 223),
    (198, 224),
    (199, 225),
    (200, 226),
    (202, 227),
    (203, 228),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c84: ZN,
    pub c42: ZB,
    pub c88: ZN,
    pub c86: ZN,
    pub c43: ZB,
    pub c85: ZN,
}

/// Outcome 0's per-assignment values and lane masks.
/// The cells of outcome 0 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared0`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut0 {
    pub c20: ZN,
    pub c41: ZB,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 1 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_1: &[(u32, &str)] = &[
    (177, "balloon.tile"),
    (204, "big_chest.tile"),
    (196, "chest.if_not_fruit"),
    (198, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (190, "fake_wall.if_not_fruit"),
    (191, "fake_wall.tile"),
    (180, "fall_floor.tile"),
    (186, "fly_fruit.if_not_fruit"),
    (188, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (182, "fruit.if_not_fruit"),
    (184, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (193, "key.if_not_fruit"),
    (194, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (238, "objects[0].collideable"),
    (358, "objects[0].flip.x"),
    (359, "objects[0].flip.y"),
    (360, "objects[0].hitbox.h"),
    (361, "objects[0].hitbox.w"),
    (362, "objects[0].hitbox.x"),
    (363, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (364, "objects[0].rem.x"),
    (365, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (366, "objects[0].spd.x"),
    (367, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (368, "objects[1].flip.x"),
    (369, "objects[1].flip.y"),
    (370, "objects[1].hitbox.h"),
    (371, "objects[1].hitbox.w"),
    (372, "objects[1].hitbox.x"),
    (373, "objects[1].hitbox.y"),
    (374, "objects[1].rem.x"),
    (375, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (376, "objects[1].spd.x"),
    (377, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (270, "objects[1].state"),
    (378, "objects[1].target.x"),
    (379, "objects[1].target.y"),
    (159, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (380, "objects[2].flip.x"),
    (381, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (382, "objects[2].hitbox.h"),
    (383, "objects[2].hitbox.w"),
    (384, "objects[2].hitbox.x"),
    (385, "objects[2].hitbox.y"),
    (386, "objects[2].rem.x"),
    (387, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (388, "objects[2].spd.x"),
    (389, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (174, "objects[2].type.tile"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (390, "objects[3].flip.x"),
    (391, "objects[3].flip.y"),
    (298, "objects[3].hide_for"),
    (299, "objects[3].hide_in"),
    (392, "objects[3].hitbox.h"),
    (393, "objects[3].hitbox.w"),
    (394, "objects[3].hitbox.x"),
    (395, "objects[3].hitbox.y"),
    (396, "objects[3].rem.x"),
    (397, "objects[3].rem.y"),
    (307, "objects[3].solids"),
    (398, "objects[3].spd.x"),
    (399, "objects[3].spd.y"),
    (309, "objects[3].spr"),
    (311, "objects[3].x"),
    (312, "objects[3].y"),
    (43, "pause_player"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_1: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_1: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154]),
    SCell::Obj(&[(7, 155), (5, 156), (6, 157)]),
    SCell::Obj(&[(5, 158), (8, 159), (6, 160)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 161), (2, 162)]),
    SCell::Arr(&[163, 164, 165, 166, 167, 168, 169, 170, 171, 172]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 182), (5, 183), (8, 184), (6, 185)]),
    SCell::Obj(&[(9, 186), (5, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(5, 200), (6, 201)]),
    SCell::Obj(&[(7, 202), (5, 203), (8, 204)]),
    SCell::Obj(&[(7, 205), (5, 206)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 236), (20, 237), (11, 238), (14, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (41, 246), (4, 247), (12, 248), (3, 249), (13, 250), (38, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (15, 260), (19, 261), (18, 262), (22, 263), (23, 264), (24, 265), (4, 266), (12, 267), (3, 268), (13, 269), (25, 270), (34, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (14, 297), (45, 298), (42, 299), (15, 300), (19, 301), (18, 302), (22, 303), (23, 304), (24, 305), (4, 306), (12, 307), (3, 308), (13, 309), (0, 310), (1, 311), (2, 312)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(17, 360), (16, 361), (1, 362), (2, 363)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 364), (2, 365)]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Obj(&[(17, 370), (16, 371), (1, 372), (2, 373)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Obj(&[(1, 376), (2, 377)]),
    SCell::Obj(&[(1, 378), (2, 379)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 380), (2, 381)]),
    SCell::Obj(&[(17, 382), (16, 383), (1, 384), (2, 385)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 386), (2, 387)]),
    SCell::Obj(&[(1, 388), (2, 389)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 390), (2, 391)]),
    SCell::Obj(&[(17, 392), (16, 393), (1, 394), (2, 395)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_1: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_1: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 207),
    (152, 208),
    (153, 209),
    (154, 210),
    (155, 211),
    (156, 212),
    (157, 213),
    (158, 214),
    (160, 215),
    (163, 94),
    (164, 116),
    (165, 118),
    (166, 119),
    (167, 121),
    (168, 122),
    (169, 123),
    (170, 124),
    (171, 125),
    (172, 127),
    (173, 216),
    (175, 217),
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (183, 222),
    (185, 223),
    (187, 224),
    (189, 225),
    (192, 226),
    (195, 227),
    (197, 228),
    (199, 229),
    (200, 230),
    (201, 231),
    (202, 232),
    (203, 233),
    (205, 234),
    (206, 235),
    (236, 313),
    (237, 314),
    (239, 315),
    (240, 316),
    (241, 317),
    (242, 318),
    (243, 319),
    (244, 320),
    (245, 321),
    (247, 322),
    (249, 323),
    (252, 121),
    (255, 324),
    (256, 325),
    (259, 326),
    (260, 327),
    (261, 328),
    (262, 329),
    (263, 330),
    (264, 331),
    (265, 332),
    (266, 333),
    (268, 334),
    (271, 335),
    (272, 94),
    (275, 336),
    (276, 337),
    (278, 338),
    (281, 339),
    (282, 340),
    (283, 341),
    (284, 342),
    (285, 343),
    (286, 344),
    (287, 345),
    (289, 346),
    (291, 116),
    (294, 347),
    (295, 348),
    (297, 349),
    (300, 350),
    (301, 351),
    (302, 352),
    (303, 353),
    (304, 354),
    (305, 355),
    (306, 356),
    (308, 357),
    (310, 116),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c84: ZN,
    pub c88: ZN,
    pub c86: ZN,
    pub c43: ZB,
    pub c85: ZN,
}

/// Outcome 1's per-assignment values and lane masks.
/// The cells of outcome 1 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared1`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut1 {
    pub c87: ZN,
    pub c39: ZN,
    pub c20: ZN,
    pub c38: ZB,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (232, "objects[0].collideable"),
    (268, "objects[0].dash_accel.x"),
    (269, "objects[0].dash_accel.y"),
    (234, "objects[0].dash_effect_time"),
    (270, "objects[0].dash_target.x"),
    (271, "objects[0].dash_target.y"),
    (236, "objects[0].dash_time"),
    (237, "objects[0].djump"),
    (272, "objects[0].flip.x"),
    (273, "objects[0].flip.y"),
    (239, "objects[0].grace"),
    (274, "objects[0].hitbox.h"),
    (275, "objects[0].hitbox.w"),
    (276, "objects[0].hitbox.x"),
    (277, "objects[0].hitbox.y"),
    (246, "objects[0].p_dash"),
    (247, "objects[0].p_jump"),
    (278, "objects[0].rem.x"),
    (279, "objects[0].rem.y"),
    (249, "objects[0].solids"),
    (280, "objects[0].spd.x"),
    (281, "objects[0].spd.y"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (43, "pause_player"),
    (156, "player_spawn.tile"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_2: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_2: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 230), (20, 231), (11, 232), (28, 233), (33, 234), (27, 235), (26, 236), (30, 237), (14, 238), (29, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (32, 246), (31, 247), (4, 248), (12, 249), (3, 250), (13, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[204]),
    SCell::Clo(25, &[204]),
    SCell::Obj(&[(1, 268), (2, 269)]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(17, 274), (16, 275), (1, 276), (2, 277)]),
    SCell::Clo(24, &[204]),
    SCell::Clo(23, &[204]),
    SCell::Clo(27, &[204]),
    SCell::Clo(28, &[204]),
    SCell::Clo(29, &[204]),
    SCell::Obj(&[(1, 278), (2, 279)]),
    SCell::Obj(&[(1, 280), (2, 281)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_2: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_2: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 204),
    (152, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (157, 209),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 210),
    (172, 211),
    (173, 212),
    (175, 213),
    (176, 214),
    (178, 215),
    (180, 216),
    (182, 217),
    (184, 218),
    (186, 219),
    (189, 220),
    (192, 221),
    (194, 222),
    (196, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (202, 228),
    (203, 229),
    (230, 255),
    (231, 256),
    (233, 257),
    (235, 258),
    (238, 259),
    (240, 260),
    (241, 261),
    (242, 262),
    (243, 263),
    (244, 264),
    (245, 265),
    (248, 266),
    (250, 267),
    (252, 93),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c42: ZB,
    pub c88: ZN,
    pub c86: ZN,
    pub c254: ZN,
    pub c43: ZB,
    pub c85: ZN,
    pub c38: ZB,
}

/// Outcome 2's per-assignment values and lane masks.
/// The cells of outcome 2 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared2`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut2 {
    pub c20: ZN,
    pub c41: ZB,
    pub c268: ZN,
    pub c269: ZN,
    pub c234: ZN,
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c237: ZN,
    pub c272: ZB,
    pub c239: ZN,
    pub c246: ZB,
    pub c247: ZB,
    pub c280: ZN,
    pub c281: ZN,
    pub c253: ZN,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 3 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_3: &[(u32, &str)] = &[
    (174, "balloon.tile"),
    (201, "big_chest.tile"),
    (193, "chest.if_not_fruit"),
    (195, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (187, "fake_wall.if_not_fruit"),
    (188, "fake_wall.tile"),
    (177, "fall_floor.tile"),
    (183, "fly_fruit.if_not_fruit"),
    (185, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (179, "fruit.if_not_fruit"),
    (181, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (190, "key.if_not_fruit"),
    (191, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (232, "objects[0].collideable"),
    (233, "objects[0].delay"),
    (262, "objects[0].flip.x"),
    (263, "objects[0].flip.y"),
    (264, "objects[0].hitbox.h"),
    (265, "objects[0].hitbox.w"),
    (266, "objects[0].hitbox.x"),
    (267, "objects[0].hitbox.y"),
    (268, "objects[0].rem.x"),
    (269, "objects[0].rem.y"),
    (242, "objects[0].solids"),
    (270, "objects[0].spd.x"),
    (271, "objects[0].spd.y"),
    (244, "objects[0].spr"),
    (245, "objects[0].state"),
    (272, "objects[0].target.x"),
    (273, "objects[0].target.y"),
    (156, "objects[0].type.tile"),
    (248, "objects[0].x"),
    (249, "objects[0].y"),
    (43, "pause_player"),
    (158, "room.x"),
    (159, "room.y"),
    (85, "seconds"),
    (171, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_3: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_3: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151]),
    SCell::Obj(&[(7, 152), (5, 153), (6, 154)]),
    SCell::Obj(&[(5, 155), (8, 156), (6, 157)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 158), (2, 159)]),
    SCell::Arr(&[160, 161, 162, 163, 164, 165, 166, 167, 168, 169]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 170), (8, 171), (6, 172)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 179), (5, 180), (8, 181), (6, 182)]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (5, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(5, 197), (6, 198)]),
    SCell::Obj(&[(7, 199), (5, 200), (8, 201)]),
    SCell::Obj(&[(7, 202), (5, 203)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 230), (20, 231), (11, 232), (35, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (4, 241), (12, 242), (3, 243), (13, 244), (25, 245), (34, 246), (0, 247), (1, 248), (2, 249)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[204]),
    SCell::Clo(25, &[204]),
    SCell::Obj(&[(1, 262), (2, 263)]),
    SCell::Obj(&[(17, 264), (16, 265), (1, 266), (2, 267)]),
    SCell::Clo(24, &[204]),
    SCell::Clo(23, &[204]),
    SCell::Clo(27, &[204]),
    SCell::Clo(28, &[204]),
    SCell::Clo(29, &[204]),
    SCell::Obj(&[(1, 268), (2, 269)]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_3: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_3: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 204),
    (152, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (157, 209),
    (160, 94),
    (161, 116),
    (162, 118),
    (163, 119),
    (164, 121),
    (165, 122),
    (166, 123),
    (167, 124),
    (168, 125),
    (169, 127),
    (170, 210),
    (172, 211),
    (173, 212),
    (175, 213),
    (176, 214),
    (178, 215),
    (180, 216),
    (182, 217),
    (184, 218),
    (186, 219),
    (189, 220),
    (192, 221),
    (194, 222),
    (196, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (202, 228),
    (203, 229),
    (230, 250),
    (231, 251),
    (234, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (243, 260),
    (246, 261),
    (247, 94),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: ZB,
    pub c42: ZB,
    pub c88: ZN,
    pub c86: ZN,
    pub c232: ZB,
    pub c233: ZN,
    pub c262: ZB,
    pub c263: ZB,
    pub c264: ZN,
    pub c265: ZN,
    pub c266: ZN,
    pub c267: ZN,
    pub c268: ZN,
    pub c269: ZN,
    pub c242: ZB,
    pub c270: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c43: ZB,
    pub c85: ZN,
    pub c38: ZB,
}

/// Outcome 3's per-assignment values and lane masks.
/// The cells of outcome 3 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared3`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut3 {
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

/// An EMPTY accumulator with outcome 0's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append0`.
pub fn acc0(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_0, OUT_GLOBALS_0, OUT_PTRS_0, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::N(Vec::new());
    b.cols[43] = Col::V(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_0 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[182] = Col::U(AV::Bool(true));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::N(Vec::new());
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(3120073i32)));
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[368] = Col::U(AV::Bool(false));
    b.cols[369] = Col::U(AV::Bool(false));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[375] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[380] = Col::U(AV::Bool(false));
    b.cols[381] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[385] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[390] = Col::U(AV::Bool(false));
    b.cols[391] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[395] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[396] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[397] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Bool(true));
    b.cols[398] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[399] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::V(Vec::new());
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_1 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::N(Vec::new());
    b.cols[232] = Col::U(AV::Bool(true));
    b.cols[268] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[234] = Col::N(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[237] = Col::N(Vec::new());
    b.cols[272] = Col::V(Vec::new());
    b.cols[273] = Col::U(AV::Bool(false));
    b.cols[239] = Col::N(Vec::new());
    b.cols[274] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[246] = Col::V(Vec::new());
    b.cols[247] = Col::V(Vec::new());
    b.cols[278] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Bool(true));
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[254] = Col::N(Vec::new());
    b.cols[43] = Col::V(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_2 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(kv.c268.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(kv.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[234] { v.push(kv.c234.lane(i)); }
        if let Col::N(v) = &mut acc.cols[270] { v.push(kv.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(kv.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[237] { v.push(kv.c237.lane(i)); }
        if let Col::V(v) = &mut acc.cols[272] {
            v.push(if kv.c272.known & (1 << i) != 0 {
                AV::Bool(kv.c272.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::V(v) = &mut acc.cols[246] {
            v.push(if kv.c246.known & (1 << i) != 0 {
                AV::Bool(kv.c246.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[247] {
            v.push(if kv.c247.known & (1 << i) != 0 {
                AV::Bool(kv.c247.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[280] { v.push(kv.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(kv.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[179] = Col::U(AV::Bool(true));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::V(Vec::new());
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::N(Vec::new());
    b.cols[232] = Col::V(Vec::new());
    b.cols[233] = Col::N(Vec::new());
    b.cols[262] = Col::V(Vec::new());
    b.cols[263] = Col::V(Vec::new());
    b.cols[264] = Col::N(Vec::new());
    b.cols[265] = Col::N(Vec::new());
    b.cols[266] = Col::N(Vec::new());
    b.cols[267] = Col::N(Vec::new());
    b.cols[268] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[242] = Col::V(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[244] = Col::N(Vec::new());
    b.cols[245] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[248] = Col::N(Vec::new());
    b.cols[249] = Col::N(Vec::new());
    b.cols[43] = Col::V(Vec::new());
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_3 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 3 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if sh.c41.known & (1 << i) != 0 {
                AV::Bool(sh.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[42] {
            v.push(if sh.c42.known & (1 << i) != 0 {
                AV::Bool(sh.c42.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[88] { v.push(sh.c88.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::V(v) = &mut acc.cols[232] {
            v.push(if sh.c232.known & (1 << i) != 0 {
                AV::Bool(sh.c232.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[233] { v.push(sh.c233.lane(i)); }
        if let Col::V(v) = &mut acc.cols[262] {
            v.push(if sh.c262.known & (1 << i) != 0 {
                AV::Bool(sh.c262.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[263] {
            v.push(if sh.c263.known & (1 << i) != 0 {
                AV::Bool(sh.c263.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[264] { v.push(sh.c264.lane(i)); }
        if let Col::N(v) = &mut acc.cols[265] { v.push(sh.c265.lane(i)); }
        if let Col::N(v) = &mut acc.cols[266] { v.push(sh.c266.lane(i)); }
        if let Col::N(v) = &mut acc.cols[267] { v.push(sh.c267.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(sh.c268.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::V(v) = &mut acc.cols[242] {
            v.push(if sh.c242.known & (1 << i) != 0 {
                AV::Bool(sh.c242.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[270] { v.push(sh.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(sh.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[244] { v.push(sh.c244.lane(i)); }
        if let Col::N(v) = &mut acc.cols[245] { v.push(sh.c245.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(sh.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[248] { v.push(sh.c248.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::V(v) = &mut acc.cols[43] {
            v.push(if sh.c43.known & (1 << i) != 0 {
                AV::Bool(sh.c43.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 4;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        _ => panic!("outcome {} of 4", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        _ => panic!("outcome {} of 4", i),
    }
}

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32] }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
) -> Option<u16> {
    let (u, s) = bind(b)?;
    let rin = rows(b, &s, lo)?;
    let g = G { cart: &b.cart, cache: &b.cache };
    // The row sets are the CALLER's and reset in O(1) here.
    // Slice-local because that is where the duplication is:
    // it comes from configurations agreeing on one lane, and
    // a lane lives in one slice.
    seen.iter_mut().for_each(|s| s.next_slice());
    // Engine-carried origin metadata for this slice's lanes
    // (empty = untracked); see `Rt2::origin`.
    let org: &[u32] = if b.origin.is_empty() { &[] } else { &b.origin[lo..lo + n] };
    let mut sink = Append { accs, seen, n, org };
    Some(frame(&u, &rin, &g, &mut sink))
}

/// Where a frame's rows go. One call per (outcome, GROUP), not
/// per outcome per variant: variants that write identical
/// values are one call whose `take` is the union of their
/// lanes. `mask` is the group's REPRESENTATIVE assignment -
/// every member computes the same values, so any of them
/// identifies the row for a caller that wants to check it
/// against the graph.
pub trait Sink {
    fn o0(&mut self, mask: u8, take: u16, sh: &KShared0, v: &KOut0);
    fn o1(&mut self, mask: u8, take: u16, sh: &KShared1, v: &KOut1);
    fn o2(&mut self, mask: u8, take: u16, sh: &KShared2, v: &KOut2);
    fn o3(&mut self, mask: u8, take: u16, sh: &KShared3, v: &KOut3);
}

/// Run one frame over 16 lanes. Returns the lanes DECLINED -
/// live but not `ok` - which the never-deopt doctrine turns
/// into a stopped run.
#[inline(never)]
pub fn frame(u: &Uni, rin: &RowsIn, g: &G, sink: &mut dyn Sink) -> u16 {
    let mut declined: u16 = 0;
    let r_c20: ZN = rin.c20;
    let r_c38: ZB = ZB { val: rin.c38, known: ALL };
    let r_c39: ZN = rin.c39;
    let r_c41: ZB = ZB { val: rin.c41, known: ALL };
    let r_c42: ZB = ZB { val: rin.c42, known: ALL };
    let r_c43: ZB = ZB { val: rin.c43, known: ALL };
    let r_c84: ZN = rin.c84;
    let r_c85: ZN = rin.c85;
    let r_c86: ZN = rin.c86;
    let r_c87: ZN = rin.c87;
    let r_c88: ZN = rin.c88;
    let r_c232: ZB = ZB { val: rin.c232, known: ALL };
    let r_c233: ZN = rin.c233;
    let r_c242: ZB = ZB { val: rin.c242, known: ALL };
    let r_c244: ZN = rin.c244;
    let r_c245: ZN = rin.c245;
    let r_c248: ZN = rin.c248;
    let r_c249: ZN = rin.c249;
    let r_c262: ZB = ZB { val: rin.c262, known: ALL };
    let r_c263: ZB = ZB { val: rin.c263, known: ALL };
    let r_c268: ZN = rin.c268;
    let r_c269: ZN = rin.c269;
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let n50: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n51: ZN = zn_rem(n50, zn_splat(P8::from_raw(1966080i32)));
    let n52: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n51);
    let n53: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n55: ZN = zn_rem(n53, zn_splat(P8::from_raw(3932160i32)));
    let n56: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n55);
    let n57: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n58: ZN = zsel_n(n56, n57, r_c86);
    let n59: ZN = zsel_n(n52, n58, r_c86);
    let n60: ZN = zsel_n(n52, n55, r_c85);
    let n61: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n62: ZB = zn_gt(r_c39, zn_splat(P8::from_raw(0i32)));
    let n63: ZB = zb_and(r_c38, n62);
    let n64: ZN = zn_sub(r_c39, zn_splat(P8::from_raw(65536i32)));
    let n65: ZB = zn_le(n64, zn_splat(P8::from_raw(0i32)));
    let n66: ZB = zn_gt(n64, zn_splat(P8::from_raw(0i32)));
    let n67: ZB = zb_and(r_c41, n66);
    let n68: ZB = zb_and(r_c42, n66);
    let n69: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), r_c233);
    let n70: ZB = zb_and(r_c242, n66);
    let n71: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), r_c245);
    let n72: ZN = zsel_n(n65, zn_splat(P8::from_raw(524288i32)), r_c248);
    let n73: ZN = zsel_n(n65, zn_splat(P8::from_raw(8388608i32)), r_c249);
    let n74: ZN = zsel_n(n65, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c264));
    let n75: ZN = zsel_n(n65, zn_splat(P8::from_raw(524288i32)), zn_splat(u.c265));
    let n76: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), zn_splat(u.c266));
    let n77: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), zn_splat(u.c267));
    let n78: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), r_c268);
    let n79: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), r_c269);
    let n80: ZN = zsel_n(n65, zn_splat(P8::from_raw(0i32)), r_c270);
    let n81: ZN = zsel_n(n65, zn_splat(P8::from_raw(-262144i32)), r_c271);
    let n82: ZB = zsel_b(n63, n67, r_c41);
    let n83: ZB = zsel_b(n63, n68, r_c42);
    let n84: ZN = zsel_n(n63, n69, r_c233);
    let n85: ZB = zsel_b(n63, n70, r_c242);
    let n86: ZN = zsel_n(n63, n71, r_c245);
    let n87: ZN = zsel_n(n63, n72, r_c248);
    let n88: ZN = zsel_n(n63, n73, r_c249);
    let n89: ZN = zsel_n(n63, n74, zn_splat(u.c264));
    let n90: ZN = zsel_n(n63, n75, zn_splat(u.c265));
    let n91: ZN = zsel_n(n63, n76, zn_splat(u.c266));
    let n92: ZN = zsel_n(n63, n77, zn_splat(u.c267));
    let n93: ZN = zsel_n(n63, n78, r_c268);
    let n94: ZN = zsel_n(n63, n79, r_c269);
    let n95: ZN = zsel_n(n63, n80, r_c270);
    let n96: ZN = zsel_n(n63, n81, r_c271);
    let n97: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n95);
    let n98: ZB = zb_not(n97);
    let n99: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n96);
    let n100: ZB = zb_not(n99);
    let n101: ZB = zb_or(n98, n100);
    let n102: ZB = zb_not(n101);
    let n103: ZN = zn_add(n93, n95);
    let n105: ZN = zn_add(n103, zn_splat(P8::from_raw(32768i32)));
    let n106: ZN = zn_flr(n105);
    let n107: ZB = zb_not(n85);
    let n108: ZB = zn_gt(n106, zn_splat(P8::from_raw(0i32)));
    let n109: ZB = zn_lt(n106, zn_splat(P8::from_raw(0i32)));
    let n111: ZN = zsel_n(n109, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n112: ZN = zsel_n(n108, zn_splat(P8::from_raw(65536i32)), n111);
    let n113: ZN = zn_abs(n106);
    let n114: ZN = zn_add(n87, n91);
    let n115: ZN = zn_add(n112, n114);
    let n116: ZN = zn_add(n88, n92);
    let n117: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n116);
    let n118: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n115, n117, n90, n89, P8::from_raw(0i32));
    let n119: ZN = zn_add(n87, n112);
    let n120: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n113);
    let n121: ZN = zn_add(n91, n119);
    let n122: ZN = zn_add(n112, n121);
    let n123: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n122, n117, n90, n89, P8::from_raw(0i32));
    let n124: ZN = zn_add(n112, n119);
    let n125: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n113);
    let n126: ZN = zn_add(n91, n124);
    let n127: ZN = zn_add(n112, n126);
    let n128: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n127, n117, n90, n89, P8::from_raw(0i32));
    let n129: ZN = zn_add(n112, n124);
    let n130: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n113);
    let n131: ZN = zn_add(n91, n129);
    let n132: ZN = zn_add(n112, n131);
    let n133: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n132, n117, n90, n89, P8::from_raw(0i32));
    let n134: ZN = zn_add(n112, n129);
    let n135: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n113);
    let n136: ZN = zn_add(n91, n134);
    let n137: ZN = zn_add(n112, n136);
    let n138: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n137, n117, n90, n89, P8::from_raw(0i32));
    let n139: ZN = zn_add(n112, n134);
    let n140: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n113);
    let n141: ZN = zn_add(n91, n139);
    let n142: ZN = zn_add(n112, n141);
    let n143: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n142, n117, n90, n89, P8::from_raw(0i32));
    let n144: ZN = zn_add(n112, n139);
    let n145: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n113);
    let n146: ZN = zn_add(n91, n144);
    let n147: ZN = zn_add(n112, n146);
    let n148: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n147, n117, n90, n89, P8::from_raw(0i32));
    let n149: ZN = zn_add(n112, n144);
    let n150: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n113);
    let n151: ZN = zn_add(n91, n149);
    let n152: ZN = zn_add(n112, n151);
    let n153: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n152, n117, n90, n89, P8::from_raw(0i32));
    let n154: ZN = zn_add(n112, n149);
    let n155: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n113);
    let n156: ZN = zsel_n(n153, n149, n154);
    let n157: ZB = zb_or(n153, n155);
    let n158: ZN = zsel_n(n150, n149, n156);
    let n159: ZB = zb_or(n150, n157);
    let n160: ZN = zsel_n(n148, n144, n158);
    let n161: ZB = zb_or(n148, n159);
    let n162: ZN = zsel_n(n145, n144, n160);
    let n163: ZB = zb_or(n145, n161);
    let n164: ZN = zsel_n(n143, n139, n162);
    let n165: ZB = zb_or(n143, n163);
    let n166: ZN = zsel_n(n140, n139, n164);
    let n167: ZB = zb_or(n140, n165);
    let n168: ZN = zsel_n(n138, n134, n166);
    let n169: ZB = zb_or(n138, n167);
    let n170: ZN = zsel_n(n135, n134, n168);
    let n171: ZB = zb_or(n135, n169);
    let n172: ZN = zsel_n(n133, n129, n170);
    let n173: ZB = zb_or(n133, n171);
    let n174: ZN = zsel_n(n130, n129, n172);
    let n175: ZB = zb_or(n130, n173);
    let n176: ZN = zsel_n(n128, n124, n174);
    let n177: ZB = zb_or(n128, n175);
    let n178: ZN = zsel_n(n125, n124, n176);
    let n179: ZB = zb_or(n125, n177);
    let n180: ZN = zsel_n(n123, n119, n178);
    let n181: ZB = zb_or(n123, n179);
    let n182: ZN = zsel_n(n120, n119, n180);
    let n183: ZB = zb_or(n120, n181);
    let n184: ZN = zsel_n(n118, n87, n182);
    let n185: ZB = zb_or(n118, n183);
    let n186: ZN = zn_add(n87, n106);
    let n187: ZN = zsel_n(n85, n184, n186);
    let n188: ZB = zb_or(n107, n185);
    let n189: ZN = zn_add(n94, n96);
    let n190: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n189);
    let n191: ZN = zn_flr(n190);
    let n192: ZB = zn_gt(n191, zn_splat(P8::from_raw(0i32)));
    let n193: ZB = zn_lt(n191, zn_splat(P8::from_raw(0i32)));
    let n194: ZN = zsel_n(n193, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n195: ZN = zsel_n(n192, zn_splat(P8::from_raw(65536i32)), n194);
    let n196: ZN = zn_abs(n191);
    let n197: ZN = zn_add(n91, n187);
    let n198: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n197);
    let n199: ZN = zn_add(n116, n195);
    let n200: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n199, n90, n89, P8::from_raw(0i32));
    let n201: ZN = zn_add(n88, n195);
    let n202: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n196);
    let n203: ZN = zn_add(n92, n201);
    let n204: ZN = zn_add(n195, n203);
    let n205: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n204, n90, n89, P8::from_raw(0i32));
    let n206: ZN = zn_add(n195, n201);
    let n207: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n196);
    let n208: ZN = zn_add(n92, n206);
    let n209: ZN = zn_add(n195, n208);
    let n210: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n209, n90, n89, P8::from_raw(0i32));
    let n211: ZN = zn_add(n195, n206);
    let n212: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n196);
    let n213: ZN = zn_add(n92, n211);
    let n214: ZN = zn_add(n195, n213);
    let n215: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n214, n90, n89, P8::from_raw(0i32));
    let n216: ZN = zn_add(n195, n211);
    let n217: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n196);
    let n218: ZN = zn_add(n92, n216);
    let n219: ZN = zn_add(n195, n218);
    let n220: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n219, n90, n89, P8::from_raw(0i32));
    let n221: ZN = zn_add(n195, n216);
    let n222: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n196);
    let n223: ZN = zn_add(n92, n221);
    let n224: ZN = zn_add(n195, n223);
    let n225: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n224, n90, n89, P8::from_raw(0i32));
    let n226: ZN = zn_add(n195, n221);
    let n227: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n196);
    let n228: ZN = zn_add(n92, n226);
    let n229: ZN = zn_add(n195, n228);
    let n230: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n229, n90, n89, P8::from_raw(0i32));
    let n231: ZN = zn_add(n195, n226);
    let n232: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n196);
    let n233: ZN = zn_add(n92, n231);
    let n234: ZN = zn_add(n195, n233);
    let n235: ZB = zn_tile_flag_at_lanes(g.cache, g.cart, n198, n234, n90, n89, P8::from_raw(0i32));
    let n236: ZN = zn_add(n195, n231);
    let n237: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n196);
    let n238: ZB = zb_and(n188, n237);
    let n239: ZN = zsel_n(n235, n231, n236);
    let n240: ZB = zsel_b(n235, n188, n238);
    let n241: ZN = zsel_n(n232, n231, n239);
    let n242: ZB = zsel_b(n232, n188, n240);
    let n243: ZN = zsel_n(n230, n226, n241);
    let n244: ZB = zsel_b(n230, n188, n242);
    let n245: ZN = zsel_n(n227, n226, n243);
    let n246: ZB = zsel_b(n227, n188, n244);
    let n247: ZN = zsel_n(n225, n221, n245);
    let n248: ZB = zsel_b(n225, n188, n246);
    let n249: ZN = zsel_n(n222, n221, n247);
    let n250: ZB = zsel_b(n222, n188, n248);
    let n251: ZN = zsel_n(n220, n216, n249);
    let n252: ZB = zsel_b(n220, n188, n250);
    let n253: ZN = zsel_n(n217, n216, n251);
    let n254: ZB = zsel_b(n217, n188, n252);
    let n255: ZN = zsel_n(n215, n211, n253);
    let n256: ZB = zsel_b(n215, n188, n254);
    let n257: ZN = zsel_n(n212, n211, n255);
    let n258: ZB = zsel_b(n212, n188, n256);
    let n259: ZN = zsel_n(n210, n206, n257);
    let n260: ZB = zsel_b(n210, n188, n258);
    let n261: ZN = zsel_n(n207, n206, n259);
    let n262: ZB = zsel_b(n207, n188, n260);
    let n263: ZN = zsel_n(n205, n201, n261);
    let n264: ZB = zsel_b(n205, n188, n262);
    let n265: ZN = zsel_n(n202, n201, n263);
    let n266: ZB = zsel_b(n202, n188, n264);
    let n267: ZN = zsel_n(n200, n88, n265);
    let n268: ZB = zsel_b(n200, n188, n266);
    let n269: ZN = zn_add(n88, n191);
    let n270: ZN = zsel_n(n85, n267, n269);
    let n271: ZB = zsel_b(n85, n268, n188);
    let n272: ZN = zsel_n(n101, n187, n87);
    let n273: ZN = zsel_n(n101, n270, n88);
    let n274: ZB = zb_or(n102, n271);
    let n275: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n86);
    let n276: ZB = zb_not(n275);
    let n277: ZB = zb_and(n61, n276);
    let n278: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), n86);
    let n279: ZB = zb_not(n278);
    let n280: ZB = zb_and(n277, n279);
    let n281: ZN = zn_sub(n84, zn_splat(P8::from_raw(65536i32)));
    let n282: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), n86);
    let n283: ZB = zb_and(n280, n282);
    let n284: ZB = zn_lt(n281, zn_splat(P8::from_raw(0i32)));
    let n285: ZB = zb_and(n283, n284);
    let n286: ZB = zb_not(r_c43);
    let n287: ZB = zb_and(n285, n286);
    let n288: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n272);
    let n289: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n273);
    let n290: ZN = zn_div(n288, zn_splat(P8::from_raw(524288i32)));
    let n291: ZN = zn_flr(n290);
    let n292: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n291);
    let n293: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n288);
    let n294: ZN = zn_sub(n293, zn_splat(P8::from_raw(65536i32)));
    let n295: ZN = zn_div(n294, zn_splat(P8::from_raw(524288i32)));
    let n296: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n295);
    let n297: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n292);
    let n298: ZB = zn_le(n297, n296);
    let n299: ZB = zn_gt(n297, n296);
    let n300: ZB = zb_and(n287, n298);
    let n301: ZB = zb_and(n287, n299);
    let n302: ZN = zn_div(n289, zn_splat(P8::from_raw(524288i32)));
    let n303: ZN = zn_flr(n302);
    let n304: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n303);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n289);
    let n306: ZN = zn_sub(n305, zn_splat(P8::from_raw(65536i32)));
    let n307: ZN = zn_div(n306, zn_splat(P8::from_raw(524288i32)));
    let n308: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n307);
    let n309: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n304);
    let n310: ZB = zn_le(n309, n308);
    let n311: ZB = zn_gt(n309, n308);
    let n312: ZB = zb_and(n300, n310);
    let n313: ZB = zb_and(n300, n311);
    let n314: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n297);
    let n315: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n309);
    let n316: ZN = zn_mget(g.cart, n314, n315);
    let n317: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n316);
    let n318: ZB = zb_not(n317);
    let n319: ZB = zb_and(n312, n317);
    let n320: ZB = zb_and(n312, n318);
    let n321: ZN = zn_rem(n306, zn_splat(P8::from_raw(524288i32)));
    let n322: ZB = zn_ge(n321, zn_splat(P8::from_raw(393216i32)));
    let n323: ZB = zn_lt(n321, zn_splat(P8::from_raw(393216i32)));
    let n324: ZB = zb_and(n319, n323);
    let n325: ZB = zb_and(n319, n322);
    let n326: ZN = zn_mul(n309, zn_splat(P8::from_raw(524288i32)));
    let n327: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n326);
    let n328: ZB = zn_eq(n305, n327);
    let n329: ZB = zb_or(n324, n325);
    let n330: ZB = zb_or(n322, n328);
    let n331: ZB = zb_or(n320, n329);
    let n332: ZB = zb_and(n317, n330);
    let n333: ZB = zb_not(n332);
    let n334: ZB = zb_and(n331, n332);
    let n335: ZB = zb_and(n331, n333);
    let n336: ZB = zb_or(n334, n335);
    let n337: ZB = zb_and(n332, n336);
    let n338: ZB = zb_and(n333, n336);
    let n339: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n316);
    let n340: ZB = zb_not(n339);
    let n341: ZB = zb_and(n338, n339);
    let n342: ZB = zb_and(n338, n340);
    let n343: ZN = zn_rem(n289, zn_splat(P8::from_raw(524288i32)));
    let n344: ZB = zn_le(n343, zn_splat(P8::from_raw(131072i32)));
    let n345: ZB = zb_or(n341, n342);
    let n346: ZB = zb_and(n339, n344);
    let n347: ZB = zb_not(n346);
    let n348: ZB = zb_and(n345, n346);
    let n349: ZB = zb_and(n345, n347);
    let n350: ZB = zb_or(n348, n349);
    let n351: ZB = zb_and(n346, n350);
    let n352: ZB = zb_and(n347, n350);
    let n354: ZB = zn_eq(n316, zn_splat(P8::from_raw(2818048i32)));
    let n355: ZB = zb_not(n354);
    let n356: ZB = zb_and(n352, n354);
    let n357: ZB = zb_and(n352, n355);
    let n358: ZN = zn_rem(n288, zn_splat(P8::from_raw(524288i32)));
    let n359: ZB = zn_le(n358, zn_splat(P8::from_raw(131072i32)));
    let n360: ZB = zb_or(n356, n357);
    let n361: ZB = zb_and(n354, n359);
    let n362: ZB = zb_not(n361);
    let n363: ZB = zb_and(n360, n361);
    let n364: ZB = zb_and(n360, n362);
    let n365: ZB = zb_or(n363, n364);
    let n366: ZB = zb_and(n361, n365);
    let n367: ZB = zb_and(n362, n365);
    let n369: ZB = zn_eq(n316, zn_splat(P8::from_raw(3866624i32)));
    let n370: ZB = zb_not(n369);
    let n371: ZB = zb_and(n367, n369);
    let n372: ZB = zb_and(n367, n370);
    let n373: ZN = zn_rem(n294, zn_splat(P8::from_raw(524288i32)));
    let n374: ZB = zn_ge(n373, zn_splat(P8::from_raw(393216i32)));
    let n375: ZB = zn_lt(n373, zn_splat(P8::from_raw(393216i32)));
    let n376: ZB = zb_and(n371, n375);
    let n377: ZB = zb_and(n371, n374);
    let n378: ZN = zn_mul(n297, zn_splat(P8::from_raw(524288i32)));
    let n379: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n378);
    let n380: ZB = zn_eq(n293, n379);
    let n381: ZB = zb_or(n376, n377);
    let n382: ZB = zb_or(n374, n380);
    let n383: ZB = zb_or(n372, n381);
    let n384: ZB = zb_and(n369, n382);
    let n385: ZB = zb_not(n384);
    let n386: ZB = zb_and(n383, n384);
    let n387: ZB = zb_and(n383, n385);
    let n388: ZB = zb_or(n386, n387);
    let n389: ZB = zb_and(n384, n388);
    let n390: ZB = zb_and(n385, n388);
    let n391: ZB = zb_or(n366, n389);
    let n392: ZB = zb_or(n351, n391);
    let n393: ZB = zb_or(n337, n392);
    let n394: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n304);
    let n395: ZB = zn_le(n394, n308);
    let n396: ZB = zn_gt(n394, n308);
    let n397: ZB = zb_and(n390, n395);
    let n398: ZB = zb_and(n390, n396);
    let n399: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n394);
    let n400: ZN = zn_mget(g.cart, n314, n399);
    let n401: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n400);
    let n402: ZB = zb_not(n401);
    let n403: ZB = zb_and(n397, n401);
    let n404: ZB = zb_and(n397, n402);
    let n405: ZB = zb_and(n323, n403);
    let n406: ZB = zb_and(n322, n403);
    let n407: ZN = zn_mul(n394, zn_splat(P8::from_raw(524288i32)));
    let n408: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n407);
    let n409: ZB = zn_eq(n305, n408);
    let n410: ZB = zb_or(n405, n406);
    let n411: ZB = zb_or(n322, n409);
    let n412: ZB = zb_or(n404, n410);
    let n413: ZB = zb_and(n401, n411);
    let n414: ZB = zb_not(n413);
    let n415: ZB = zb_and(n412, n413);
    let n416: ZB = zb_and(n412, n414);
    let n417: ZB = zb_or(n415, n416);
    let n418: ZB = zb_and(n413, n417);
    let n419: ZB = zb_and(n414, n417);
    let n420: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n400);
    let n421: ZB = zb_not(n420);
    let n422: ZB = zb_and(n419, n420);
    let n423: ZB = zb_and(n419, n421);
    let n424: ZB = zb_or(n422, n423);
    let n425: ZB = zb_and(n344, n420);
    let n426: ZB = zb_not(n425);
    let n427: ZB = zb_and(n424, n425);
    let n428: ZB = zb_and(n424, n426);
    let n429: ZB = zb_or(n427, n428);
    let n430: ZB = zb_and(n425, n429);
    let n431: ZB = zb_and(n426, n429);
    let n432: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n400);
    let n433: ZB = zb_not(n432);
    let n434: ZB = zb_and(n431, n432);
    let n435: ZB = zb_and(n431, n433);
    let n436: ZB = zb_or(n434, n435);
    let n437: ZB = zb_and(n359, n432);
    let n438: ZB = zb_not(n437);
    let n439: ZB = zb_and(n436, n437);
    let n440: ZB = zb_and(n436, n438);
    let n441: ZB = zb_or(n439, n440);
    let n442: ZB = zb_and(n437, n441);
    let n443: ZB = zb_and(n438, n441);
    let n444: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n400);
    let n445: ZB = zb_not(n444);
    let n446: ZB = zb_and(n443, n444);
    let n447: ZB = zb_and(n443, n445);
    let n448: ZB = zb_and(n375, n446);
    let n449: ZB = zb_and(n374, n446);
    let n450: ZB = zb_or(n448, n449);
    let n451: ZB = zb_or(n447, n450);
    let n452: ZB = zb_and(n382, n444);
    let n453: ZB = zb_not(n452);
    let n454: ZB = zb_and(n451, n452);
    let n455: ZB = zb_and(n451, n453);
    let n456: ZB = zb_or(n454, n455);
    let n457: ZB = zb_and(n452, n456);
    let n458: ZB = zb_and(n453, n456);
    let n459: ZB = zb_or(n442, n457);
    let n460: ZB = zb_or(n430, n459);
    let n461: ZB = zb_or(n418, n460);
    let n462: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n304);
    let n463: ZB = zn_le(n462, n308);
    let n464: ZB = zn_gt(n462, n308);
    let n465: ZB = zb_and(n458, n463);
    let n466: ZB = zb_and(n458, n464);
    let n467: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n462);
    let n468: ZN = zn_mget(g.cart, n314, n467);
    let n469: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n468);
    let n470: ZB = zb_not(n469);
    let n471: ZB = zb_and(n465, n469);
    let n472: ZB = zb_and(n465, n470);
    let n473: ZB = zb_and(n323, n471);
    let n474: ZB = zb_and(n322, n471);
    let n475: ZN = zn_mul(n462, zn_splat(P8::from_raw(524288i32)));
    let n476: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n475);
    let n477: ZB = zn_eq(n305, n476);
    let n478: ZB = zb_or(n473, n474);
    let n479: ZB = zb_or(n322, n477);
    let n480: ZB = zb_or(n472, n478);
    let n481: ZB = zb_and(n469, n479);
    let n482: ZB = zb_not(n481);
    let n483: ZB = zb_and(n480, n481);
    let n484: ZB = zb_and(n480, n482);
    let n485: ZB = zb_or(n483, n484);
    let n486: ZB = zb_and(n481, n485);
    let n487: ZB = zb_and(n482, n485);
    let n488: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n468);
    let n489: ZB = zb_not(n488);
    let n490: ZB = zb_and(n487, n488);
    let n491: ZB = zb_and(n487, n489);
    let n492: ZB = zb_or(n490, n491);
    let n493: ZB = zb_and(n344, n488);
    let n494: ZB = zb_not(n493);
    let n495: ZB = zb_and(n492, n493);
    let n496: ZB = zb_and(n492, n494);
    let n497: ZB = zb_or(n495, n496);
    let n498: ZB = zb_and(n493, n497);
    let n499: ZB = zb_and(n494, n497);
    let n500: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n468);
    let n501: ZB = zb_not(n500);
    let n502: ZB = zb_and(n499, n500);
    let n503: ZB = zb_and(n499, n501);
    let n504: ZB = zb_or(n502, n503);
    let n505: ZB = zb_and(n359, n500);
    let n506: ZB = zb_not(n505);
    let n507: ZB = zb_and(n504, n505);
    let n508: ZB = zb_and(n504, n506);
    let n509: ZB = zb_or(n507, n508);
    let n510: ZB = zb_and(n505, n509);
    let n511: ZB = zb_and(n506, n509);
    let n512: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n468);
    let n513: ZB = zb_not(n512);
    let n514: ZB = zb_and(n511, n512);
    let n515: ZB = zb_and(n511, n513);
    let n516: ZB = zb_and(n375, n514);
    let n517: ZB = zb_and(n374, n514);
    let n518: ZB = zb_or(n516, n517);
    let n519: ZB = zb_or(n515, n518);
    let n520: ZB = zb_and(n382, n512);
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n519, n520);
    let n523: ZB = zb_and(n519, n521);
    let n524: ZB = zb_or(n522, n523);
    let n525: ZB = zb_and(n520, n524);
    let n526: ZB = zb_and(n521, n524);
    let n527: ZB = zb_or(n510, n525);
    let n528: ZB = zb_or(n498, n527);
    let n529: ZB = zb_or(n486, n528);
    let n530: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n304);
    let n531: ZB = zn_gt(n530, n308);
    let n532: ZB = zb_and(n274, n531);
    let n533: ZB = zb_or(n466, n526);
    let n534: ZB = zsel_b(n464, n274, n532);
    let n535: ZB = zb_or(n461, n529);
    let n536: ZB = zb_or(n398, n533);
    let n537: ZB = zsel_b(n396, n274, n534);
    let n538: ZB = zb_or(n393, n535);
    let n539: ZB = zb_or(n313, n536);
    let n540: ZB = zsel_b(n311, n274, n537);
    let n541: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n292);
    let n542: ZB = zn_le(n541, n296);
    let n543: ZB = zn_gt(n541, n296);
    let n544: ZB = zb_and(n539, n542);
    let n545: ZB = zb_and(n539, n543);
    let n546: ZB = zb_and(n310, n544);
    let n547: ZB = zb_and(n311, n544);
    let n548: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n541);
    let n549: ZN = zn_mget(g.cart, n548, n315);
    let n550: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n549);
    let n551: ZB = zb_not(n550);
    let n552: ZB = zb_and(n546, n550);
    let n553: ZB = zb_and(n546, n551);
    let n554: ZB = zb_and(n323, n552);
    let n555: ZB = zb_and(n322, n552);
    let n556: ZB = zb_or(n554, n555);
    let n557: ZB = zb_or(n553, n556);
    let n558: ZB = zb_and(n330, n550);
    let n559: ZB = zb_not(n558);
    let n560: ZB = zb_and(n557, n558);
    let n561: ZB = zb_and(n557, n559);
    let n562: ZB = zb_or(n560, n561);
    let n563: ZB = zb_and(n558, n562);
    let n564: ZB = zb_and(n559, n562);
    let n565: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n549);
    let n566: ZB = zb_not(n565);
    let n567: ZB = zb_and(n564, n565);
    let n568: ZB = zb_and(n564, n566);
    let n569: ZB = zb_or(n567, n568);
    let n570: ZB = zb_and(n344, n565);
    let n571: ZB = zb_not(n570);
    let n572: ZB = zb_and(n569, n570);
    let n573: ZB = zb_and(n569, n571);
    let n574: ZB = zb_or(n572, n573);
    let n575: ZB = zb_and(n570, n574);
    let n576: ZB = zb_and(n571, n574);
    let n577: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n549);
    let n578: ZB = zb_not(n577);
    let n579: ZB = zb_and(n576, n577);
    let n580: ZB = zb_and(n576, n578);
    let n581: ZB = zb_or(n579, n580);
    let n582: ZB = zb_and(n359, n577);
    let n583: ZB = zb_not(n582);
    let n584: ZB = zb_and(n581, n582);
    let n585: ZB = zb_and(n581, n583);
    let n586: ZB = zb_or(n584, n585);
    let n587: ZB = zb_and(n582, n586);
    let n588: ZB = zb_and(n583, n586);
    let n589: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n549);
    let n590: ZB = zb_not(n589);
    let n591: ZB = zb_and(n588, n589);
    let n592: ZB = zb_and(n588, n590);
    let n593: ZB = zb_and(n375, n591);
    let n594: ZB = zb_and(n374, n591);
    let n595: ZN = zn_mul(n541, zn_splat(P8::from_raw(524288i32)));
    let n596: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n595);
    let n597: ZB = zn_eq(n293, n596);
    let n598: ZB = zb_or(n593, n594);
    let n599: ZB = zb_or(n374, n597);
    let n600: ZB = zb_or(n592, n598);
    let n601: ZB = zb_and(n589, n599);
    let n602: ZB = zb_not(n601);
    let n603: ZB = zb_and(n600, n601);
    let n604: ZB = zb_and(n600, n602);
    let n605: ZB = zb_or(n603, n604);
    let n606: ZB = zb_and(n601, n605);
    let n607: ZB = zb_and(n602, n605);
    let n608: ZB = zb_or(n587, n606);
    let n609: ZB = zb_or(n575, n608);
    let n610: ZB = zb_or(n563, n609);
    let n611: ZB = zb_and(n395, n607);
    let n612: ZB = zb_and(n396, n607);
    let n613: ZN = zn_mget(g.cart, n548, n399);
    let n614: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n613);
    let n615: ZB = zb_not(n614);
    let n616: ZB = zb_and(n611, n614);
    let n617: ZB = zb_and(n611, n615);
    let n618: ZB = zb_and(n323, n616);
    let n619: ZB = zb_and(n322, n616);
    let n620: ZB = zb_or(n618, n619);
    let n621: ZB = zb_or(n617, n620);
    let n622: ZB = zb_and(n411, n614);
    let n623: ZB = zb_not(n622);
    let n624: ZB = zb_and(n621, n622);
    let n625: ZB = zb_and(n621, n623);
    let n626: ZB = zb_or(n624, n625);
    let n627: ZB = zb_and(n622, n626);
    let n628: ZB = zb_and(n623, n626);
    let n629: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n613);
    let n630: ZB = zb_not(n629);
    let n631: ZB = zb_and(n628, n629);
    let n632: ZB = zb_and(n628, n630);
    let n633: ZB = zb_or(n631, n632);
    let n634: ZB = zb_and(n344, n629);
    let n635: ZB = zb_not(n634);
    let n636: ZB = zb_and(n633, n634);
    let n637: ZB = zb_and(n633, n635);
    let n638: ZB = zb_or(n636, n637);
    let n639: ZB = zb_and(n634, n638);
    let n640: ZB = zb_and(n635, n638);
    let n641: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n613);
    let n642: ZB = zb_not(n641);
    let n643: ZB = zb_and(n640, n641);
    let n644: ZB = zb_and(n640, n642);
    let n645: ZB = zb_or(n643, n644);
    let n646: ZB = zb_and(n359, n641);
    let n647: ZB = zb_not(n646);
    let n648: ZB = zb_and(n645, n646);
    let n649: ZB = zb_and(n645, n647);
    let n650: ZB = zb_or(n648, n649);
    let n651: ZB = zb_and(n646, n650);
    let n652: ZB = zb_and(n647, n650);
    let n653: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n613);
    let n654: ZB = zb_not(n653);
    let n655: ZB = zb_and(n652, n653);
    let n656: ZB = zb_and(n652, n654);
    let n657: ZB = zb_and(n375, n655);
    let n658: ZB = zb_and(n374, n655);
    let n659: ZB = zb_or(n657, n658);
    let n660: ZB = zb_or(n656, n659);
    let n661: ZB = zb_and(n599, n653);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n660, n661);
    let n664: ZB = zb_and(n660, n662);
    let n665: ZB = zb_or(n663, n664);
    let n666: ZB = zb_and(n661, n665);
    let n667: ZB = zb_and(n662, n665);
    let n668: ZB = zb_or(n651, n666);
    let n669: ZB = zb_or(n639, n668);
    let n670: ZB = zb_or(n627, n669);
    let n671: ZB = zb_and(n463, n667);
    let n672: ZB = zb_and(n464, n667);
    let n673: ZN = zn_mget(g.cart, n548, n467);
    let n674: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n673);
    let n675: ZB = zb_not(n674);
    let n676: ZB = zb_and(n671, n674);
    let n677: ZB = zb_and(n671, n675);
    let n678: ZB = zb_and(n323, n676);
    let n679: ZB = zb_and(n322, n676);
    let n680: ZB = zb_or(n678, n679);
    let n681: ZB = zb_or(n677, n680);
    let n682: ZB = zb_and(n479, n674);
    let n683: ZB = zb_not(n682);
    let n684: ZB = zb_and(n681, n682);
    let n685: ZB = zb_and(n681, n683);
    let n686: ZB = zb_or(n684, n685);
    let n687: ZB = zb_and(n682, n686);
    let n688: ZB = zb_and(n683, n686);
    let n689: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n673);
    let n690: ZB = zb_not(n689);
    let n691: ZB = zb_and(n688, n689);
    let n692: ZB = zb_and(n688, n690);
    let n693: ZB = zb_or(n691, n692);
    let n694: ZB = zb_and(n344, n689);
    let n695: ZB = zb_not(n694);
    let n696: ZB = zb_and(n693, n694);
    let n697: ZB = zb_and(n693, n695);
    let n698: ZB = zb_or(n696, n697);
    let n699: ZB = zb_and(n694, n698);
    let n700: ZB = zb_and(n695, n698);
    let n701: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n673);
    let n702: ZB = zb_not(n701);
    let n703: ZB = zb_and(n700, n701);
    let n704: ZB = zb_and(n700, n702);
    let n705: ZB = zb_or(n703, n704);
    let n706: ZB = zb_and(n359, n701);
    let n707: ZB = zb_not(n706);
    let n708: ZB = zb_and(n705, n706);
    let n709: ZB = zb_and(n705, n707);
    let n710: ZB = zb_or(n708, n709);
    let n711: ZB = zb_and(n706, n710);
    let n712: ZB = zb_and(n707, n710);
    let n713: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n673);
    let n714: ZB = zb_not(n713);
    let n715: ZB = zb_and(n712, n713);
    let n716: ZB = zb_and(n712, n714);
    let n717: ZB = zb_and(n375, n715);
    let n718: ZB = zb_and(n374, n715);
    let n719: ZB = zb_or(n717, n718);
    let n720: ZB = zb_or(n716, n719);
    let n721: ZB = zb_and(n599, n713);
    let n722: ZB = zb_not(n721);
    let n723: ZB = zb_and(n720, n721);
    let n724: ZB = zb_and(n720, n722);
    let n725: ZB = zb_or(n723, n724);
    let n726: ZB = zb_and(n721, n725);
    let n727: ZB = zb_and(n722, n725);
    let n728: ZB = zb_or(n711, n726);
    let n729: ZB = zb_or(n699, n728);
    let n730: ZB = zb_or(n687, n729);
    let n731: ZB = zb_and(n531, n540);
    let n732: ZB = zb_or(n672, n727);
    let n733: ZB = zsel_b(n464, n540, n731);
    let n734: ZB = zb_or(n670, n730);
    let n735: ZB = zb_or(n612, n732);
    let n736: ZB = zsel_b(n396, n540, n733);
    let n737: ZB = zb_or(n610, n734);
    let n738: ZB = zb_or(n547, n735);
    let n739: ZB = zsel_b(n311, n540, n736);
    let n740: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n292);
    let n741: ZB = zn_le(n740, n296);
    let n742: ZB = zn_gt(n740, n296);
    let n743: ZB = zb_and(n738, n741);
    let n744: ZB = zb_and(n738, n742);
    let n745: ZB = zb_and(n310, n743);
    let n746: ZB = zb_and(n311, n743);
    let n747: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n740);
    let n748: ZN = zn_mget(g.cart, n747, n315);
    let n749: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n748);
    let n750: ZB = zb_not(n749);
    let n751: ZB = zb_and(n745, n749);
    let n752: ZB = zb_and(n745, n750);
    let n753: ZB = zb_and(n323, n751);
    let n754: ZB = zb_and(n322, n751);
    let n755: ZB = zb_or(n753, n754);
    let n756: ZB = zb_or(n752, n755);
    let n757: ZB = zb_and(n330, n749);
    let n758: ZB = zb_not(n757);
    let n759: ZB = zb_and(n756, n757);
    let n760: ZB = zb_and(n756, n758);
    let n761: ZB = zb_or(n759, n760);
    let n762: ZB = zb_and(n757, n761);
    let n763: ZB = zb_and(n758, n761);
    let n764: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n748);
    let n765: ZB = zb_not(n764);
    let n766: ZB = zb_and(n763, n764);
    let n767: ZB = zb_and(n763, n765);
    let n768: ZB = zb_or(n766, n767);
    let n769: ZB = zb_and(n344, n764);
    let n770: ZB = zb_not(n769);
    let n771: ZB = zb_and(n768, n769);
    let n772: ZB = zb_and(n768, n770);
    let n773: ZB = zb_or(n771, n772);
    let n774: ZB = zb_and(n769, n773);
    let n775: ZB = zb_and(n770, n773);
    let n776: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n748);
    let n777: ZB = zb_not(n776);
    let n778: ZB = zb_and(n775, n776);
    let n779: ZB = zb_and(n775, n777);
    let n780: ZB = zb_or(n778, n779);
    let n781: ZB = zb_and(n359, n776);
    let n782: ZB = zb_not(n781);
    let n783: ZB = zb_and(n780, n781);
    let n784: ZB = zb_and(n780, n782);
    let n785: ZB = zb_or(n783, n784);
    let n786: ZB = zb_and(n781, n785);
    let n787: ZB = zb_and(n782, n785);
    let n788: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n748);
    let n789: ZB = zb_not(n788);
    let n790: ZB = zb_and(n787, n788);
    let n791: ZB = zb_and(n787, n789);
    let n792: ZB = zb_and(n375, n790);
    let n793: ZB = zb_and(n374, n790);
    let n794: ZN = zn_mul(n740, zn_splat(P8::from_raw(524288i32)));
    let n795: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n794);
    let n796: ZB = zn_eq(n293, n795);
    let n797: ZB = zb_or(n792, n793);
    let n798: ZB = zb_or(n374, n796);
    let n799: ZB = zb_or(n791, n797);
    let n800: ZB = zb_and(n788, n798);
    let n801: ZB = zb_not(n800);
    let n802: ZB = zb_and(n799, n800);
    let n803: ZB = zb_and(n799, n801);
    let n804: ZB = zb_or(n802, n803);
    let n805: ZB = zb_and(n800, n804);
    let n806: ZB = zb_and(n801, n804);
    let n807: ZB = zb_or(n786, n805);
    let n808: ZB = zb_or(n774, n807);
    let n809: ZB = zb_or(n762, n808);
    let n810: ZB = zb_and(n395, n806);
    let n811: ZB = zb_and(n396, n806);
    let n812: ZN = zn_mget(g.cart, n747, n399);
    let n813: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n812);
    let n814: ZB = zb_not(n813);
    let n815: ZB = zb_and(n810, n813);
    let n816: ZB = zb_and(n810, n814);
    let n817: ZB = zb_and(n323, n815);
    let n818: ZB = zb_and(n322, n815);
    let n819: ZB = zb_or(n817, n818);
    let n820: ZB = zb_or(n816, n819);
    let n821: ZB = zb_and(n411, n813);
    let n822: ZB = zb_not(n821);
    let n823: ZB = zb_and(n820, n821);
    let n824: ZB = zb_and(n820, n822);
    let n825: ZB = zb_or(n823, n824);
    let n826: ZB = zb_and(n821, n825);
    let n827: ZB = zb_and(n822, n825);
    let n828: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n812);
    let n829: ZB = zb_not(n828);
    let n830: ZB = zb_and(n827, n828);
    let n831: ZB = zb_and(n827, n829);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_and(n344, n828);
    let n834: ZB = zb_not(n833);
    let n835: ZB = zb_and(n832, n833);
    let n836: ZB = zb_and(n832, n834);
    let n837: ZB = zb_or(n835, n836);
    let n838: ZB = zb_and(n833, n837);
    let n839: ZB = zb_and(n834, n837);
    let n840: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n812);
    let n841: ZB = zb_not(n840);
    let n842: ZB = zb_and(n839, n840);
    let n843: ZB = zb_and(n839, n841);
    let n844: ZB = zb_or(n842, n843);
    let n845: ZB = zb_and(n359, n840);
    let n846: ZB = zb_not(n845);
    let n847: ZB = zb_and(n844, n845);
    let n848: ZB = zb_and(n844, n846);
    let n849: ZB = zb_or(n847, n848);
    let n850: ZB = zb_and(n845, n849);
    let n851: ZB = zb_and(n846, n849);
    let n852: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n812);
    let n853: ZB = zb_not(n852);
    let n854: ZB = zb_and(n851, n852);
    let n855: ZB = zb_and(n851, n853);
    let n856: ZB = zb_and(n375, n854);
    let n857: ZB = zb_and(n374, n854);
    let n858: ZB = zb_or(n856, n857);
    let n859: ZB = zb_or(n855, n858);
    let n860: ZB = zb_and(n798, n852);
    let n861: ZB = zb_not(n860);
    let n862: ZB = zb_and(n859, n860);
    let n863: ZB = zb_and(n859, n861);
    let n864: ZB = zb_or(n862, n863);
    let n865: ZB = zb_and(n860, n864);
    let n866: ZB = zb_and(n861, n864);
    let n867: ZB = zb_or(n850, n865);
    let n868: ZB = zb_or(n838, n867);
    let n869: ZB = zb_or(n826, n868);
    let n870: ZB = zb_and(n463, n866);
    let n871: ZB = zb_and(n464, n866);
    let n872: ZN = zn_mget(g.cart, n747, n467);
    let n873: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n872);
    let n874: ZB = zb_not(n873);
    let n875: ZB = zb_and(n870, n873);
    let n876: ZB = zb_and(n870, n874);
    let n877: ZB = zb_and(n323, n875);
    let n878: ZB = zb_and(n322, n875);
    let n879: ZB = zb_or(n877, n878);
    let n880: ZB = zb_or(n876, n879);
    let n881: ZB = zb_and(n479, n873);
    let n882: ZB = zb_not(n881);
    let n883: ZB = zb_and(n880, n881);
    let n884: ZB = zb_and(n880, n882);
    let n885: ZB = zb_or(n883, n884);
    let n886: ZB = zb_and(n881, n885);
    let n887: ZB = zb_and(n882, n885);
    let n888: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n872);
    let n889: ZB = zb_not(n888);
    let n890: ZB = zb_and(n887, n888);
    let n891: ZB = zb_and(n887, n889);
    let n892: ZB = zb_or(n890, n891);
    let n893: ZB = zb_and(n344, n888);
    let n894: ZB = zb_not(n893);
    let n895: ZB = zb_and(n892, n893);
    let n896: ZB = zb_and(n892, n894);
    let n897: ZB = zb_or(n895, n896);
    let n898: ZB = zb_and(n893, n897);
    let n899: ZB = zb_and(n894, n897);
    let n900: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n872);
    let n901: ZB = zb_not(n900);
    let n902: ZB = zb_and(n899, n900);
    let n903: ZB = zb_and(n899, n901);
    let n904: ZB = zb_or(n902, n903);
    let n905: ZB = zb_and(n359, n900);
    let n906: ZB = zb_not(n905);
    let n907: ZB = zb_and(n904, n905);
    let n908: ZB = zb_and(n904, n906);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_and(n905, n909);
    let n911: ZB = zb_and(n906, n909);
    let n912: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n872);
    let n913: ZB = zb_not(n912);
    let n914: ZB = zb_and(n911, n912);
    let n915: ZB = zb_and(n911, n913);
    let n916: ZB = zb_and(n375, n914);
    let n917: ZB = zb_and(n374, n914);
    let n918: ZB = zb_or(n916, n917);
    let n919: ZB = zb_or(n915, n918);
    let n920: ZB = zb_and(n798, n912);
    let n921: ZB = zb_not(n920);
    let n922: ZB = zb_and(n919, n920);
    let n923: ZB = zb_and(n919, n921);
    let n924: ZB = zb_or(n922, n923);
    let n925: ZB = zb_and(n920, n924);
    let n926: ZB = zb_and(n921, n924);
    let n927: ZB = zb_or(n910, n925);
    let n928: ZB = zb_or(n898, n927);
    let n929: ZB = zb_or(n886, n928);
    let n930: ZB = zb_and(n531, n739);
    let n931: ZB = zb_or(n871, n926);
    let n932: ZB = zsel_b(n464, n739, n930);
    let n933: ZB = zb_or(n869, n929);
    let n934: ZB = zb_or(n811, n931);
    let n935: ZB = zsel_b(n396, n739, n932);
    let n936: ZB = zb_or(n809, n933);
    let n937: ZB = zb_or(n746, n934);
    let n938: ZB = zsel_b(n311, n739, n935);
    let n939: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n292);
    let n940: ZB = zn_gt(n939, n296);
    let n941: ZB = zb_and(n938, n940);
    let n942: ZB = zb_or(n737, n936);
    let n943: ZB = zsel_b(n737, n540, n739);
    let n944: ZB = zb_or(n744, n937);
    let n945: ZB = zsel_b(n742, n739, n941);
    let n946: ZB = zb_or(n538, n942);
    let n947: ZB = zsel_b(n538, n274, n943);
    let n948: ZB = zb_or(n545, n944);
    let n949: ZB = zsel_b(n543, n540, n945);
    let n950: ZB = zb_or(n301, n948);
    let n951: ZB = zsel_b(n299, n274, n949);
    let n952: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n953: ZB = zn_gt(n273, zn_splat(P8::from_raw(8388608i32)));
    let n954: ZB = zn_le(n273, zn_splat(P8::from_raw(8388608i32)));
    let n955: ZB = zb_and(n946, n953);
    let n956: ZB = zb_and(n946, n954);
    let n957: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n952);
    let n958: ZN = zsel_n(n953, n957, n952);
    let n959: ZB = zb_or(n955, n956);
    let n960: ZB = zb_and(n950, n953);
    let n961: ZN = zsel_n(n959, n958, n952);
    let n962: ZB = zb_or(n959, n960);
    let n963: ZB = zsel_b(n959, n947, n951);
    let n964: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n288);
    let n965: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n289);
    let n966: ZB = zn_tile_flag_at(g.cache, g.cart, n964, n965, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n967: ZB = zb_not(n966);
    let n968: ZB = zb_and(n962, n967);
    let n969: ZB = zb_and(n962, n966);
    let n970: ZB = zb_or(n968, n969);
    let n971: ZB = zb_and(n967, n970);
    let n972: ZB = zb_and(n966, n970);
    let n973: ZB = zb_or(n971, n972);
    let n974: ZB = zn_lt(r_c88, r_c88);
    let n975: ZB = zn_ge(r_c88, r_c88);
    let n976: ZN = zsel_n(n966, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n977: ZB = zb_and(n966, n973);
    let n978: ZB = zb_and(n967, n973);
    let n979: ZB = zb_and(n974, n977);
    let n980: ZB = zb_and(n975, n977);
    let n981: ZB = zb_or(n979, n980);
    let n982: ZB = zb_or(n978, n981);
    let n985: ZN = zsel_n(n967, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n986: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n985);
    let n987: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n985);
    let n988: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n289);
    let n989: ZB = zn_gt(n976, zn_splat(P8::from_raw(0i32)));
    let n990: ZB = zn_le(n976, zn_splat(P8::from_raw(0i32)));
    let n992: ZN = zn_add(n288, zn_splat(P8::from_raw(-196608i32)));
    let n993: ZB = zn_tile_flag_at(g.cache, g.cart, n992, n988, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n994: ZB = zb_not(n993);
    let n995: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n288);
    let n996: ZB = zn_tile_flag_at(g.cache, g.cart, n995, n988, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n997: ZB = zb_not(n996);
    let n998: ZN = zsel_n(n996, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n999: ZN = zsel_n(n993, zn_splat(P8::from_raw(-65536i32)), n998);
    let n1000: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n999);
    let n1001: ZB = zb_not(n1000);
    let n1003: ZB = zn_gt(r_c88, zn_splat(P8::from_raw(0i32)));
    let n1004: ZB = zn_le(r_c88, zn_splat(P8::from_raw(0i32)));
    let n1005: ZB = zb_and(n967, n982);
    let n1006: ZB = zb_and(n966, n982);
    let n1007: ZB = zb_or(n1005, n1006);
    let n1008: ZB = zb_and(n967, n1007);
    let n1009: ZB = zb_and(n966, n1007);
    let n1010: ZB = zb_or(n1008, n1009);
    let n1011: ZB = zb_and(n1003, n1010);
    let n1012: ZB = zb_and(n1004, n1010);
    let n1013: ZB = zb_or(n1011, n1012);
    let n1014: ZB = zn_lt(n273, zn_splat(P8::from_raw(-262144i32)));
    let n1015: ZB = zn_ge(n273, zn_splat(P8::from_raw(-262144i32)));
    let n1016: ZB = zb_and(n1013, n1014);
    let n1017: ZB = zb_and(n1013, n1015);
    let n1018: ZB = zb_or(n1016, n1017);
    let n1020: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1022: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n986);
    let n1023: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n288);
    let n1024: ZB = zn_tile_flag_at(g.cache, g.cart, n1023, n988, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1025: ZB = zb_not(n1024);
    let n1028: ZB = zb_and(n1007, n1025);
    let n1029: ZB = zb_and(n1007, n1024);
    let n1030: ZB = zb_or(n1028, n1029);
    let n1031: ZB = zb_and(n1025, n1030);
    let n1032: ZB = zb_and(n1024, n1030);
    let n1033: ZB = zb_or(n1031, n1032);
    let n1034: ZB = zb_and(n1024, n1033);
    let n1035: ZB = zb_and(n1025, n1033);
    let n1036: ZB = zb_or(n1034, n1035);
    let n1037: ZB = zb_and(n1024, n1036);
    let n1038: ZB = zb_and(n1025, n1036);
    let n1039: ZB = zb_or(n1037, n1038);
    let n1040: ZB = zb_and(n967, n1039);
    let n1041: ZB = zb_and(n966, n1039);
    let n1042: ZB = zb_or(n1040, n1041);
    let n1043: ZB = zb_and(n1003, n1042);
    let n1044: ZB = zb_and(n1004, n1042);
    let n1045: ZB = zb_or(n1043, n1044);
    let n1046: ZB = zb_and(n1014, n1045);
    let n1047: ZB = zb_and(n1015, n1045);
    let n1048: ZB = zb_or(n1046, n1047);
    let n1051: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n987);
    let n1052: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n288);
    let n1053: ZB = zn_tile_flag_at(g.cache, g.cart, n1052, n988, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1054: ZB = zb_not(n1053);
    let n1055: ZB = zb_and(n1007, n1054);
    let n1056: ZB = zb_and(n1007, n1053);
    let n1057: ZB = zb_or(n1055, n1056);
    let n1058: ZB = zb_and(n1054, n1057);
    let n1059: ZB = zb_and(n1053, n1057);
    let n1060: ZB = zb_or(n1058, n1059);
    let n1061: ZB = zb_and(n1053, n1060);
    let n1062: ZB = zb_and(n1054, n1060);
    let n1063: ZB = zb_or(n1061, n1062);
    let n1064: ZB = zb_and(n1053, n1063);
    let n1065: ZB = zb_and(n1054, n1063);
    let n1066: ZB = zb_or(n1064, n1065);
    let n1067: ZB = zb_and(n967, n1066);
    let n1068: ZB = zb_and(n966, n1066);
    let n1069: ZB = zb_or(n1067, n1068);
    let n1070: ZB = zb_and(n1003, n1069);
    let n1071: ZB = zb_and(n1004, n1069);
    let n1072: ZB = zb_or(n1070, n1071);
    let n1073: ZB = zb_and(n1014, n1072);
    let n1074: ZB = zb_and(n1015, n1072);
    let n1075: ZB = zb_or(n1073, n1074);
    let n1078: ZB = zb_and(n989, n1010);
    let n1079: ZB = zb_and(n990, n1010);
    let n1080: ZB = zb_and(n994, n1079);
    let n1081: ZB = zb_and(n993, n1079);
    let n1082: ZB = zb_or(n1080, n1081);
    let n1083: ZB = zb_and(n994, n1082);
    let n1084: ZB = zb_and(n993, n1082);
    let n1085: ZB = zb_or(n1083, n1084);
    let n1086: ZB = zb_and(n993, n1085);
    let n1087: ZB = zb_and(n994, n1085);
    let n1088: ZB = zb_and(n997, n1087);
    let n1089: ZB = zb_and(n996, n1087);
    let n1090: ZB = zb_or(n1088, n1089);
    let n1091: ZB = zb_and(n997, n1090);
    let n1092: ZB = zb_and(n996, n1090);
    let n1093: ZB = zb_or(n1091, n1092);
    let n1094: ZB = zb_and(n996, n1093);
    let n1095: ZB = zb_and(n997, n1093);
    let n1096: ZB = zb_or(n1094, n1095);
    let n1097: ZB = zb_or(n1086, n1096);
    let n1098: ZB = zb_and(n1001, n1097);
    let n1099: ZB = zb_and(n1000, n1097);
    let n1100: ZB = zb_or(n1098, n1099);
    let n1101: ZB = zb_or(n1078, n1100);
    let n1102: ZB = zb_and(n1003, n1101);
    let n1103: ZB = zb_and(n1004, n1101);
    let n1104: ZB = zb_or(n1102, n1103);
    let n1105: ZB = zb_and(n1014, n1104);
    let n1106: ZB = zb_and(n1015, n1104);
    let n1107: ZB = zb_or(n1105, n1106);
    let n1110: ZB = zb_and(n989, n1042);
    let n1111: ZB = zb_and(n990, n1042);
    let n1112: ZB = zb_and(n994, n1111);
    let n1113: ZB = zb_and(n993, n1111);
    let n1114: ZB = zb_or(n1112, n1113);
    let n1115: ZB = zb_and(n994, n1114);
    let n1116: ZB = zb_and(n993, n1114);
    let n1117: ZB = zb_or(n1115, n1116);
    let n1118: ZB = zb_and(n993, n1117);
    let n1119: ZB = zb_and(n994, n1117);
    let n1120: ZB = zb_and(n997, n1119);
    let n1121: ZB = zb_and(n996, n1119);
    let n1122: ZB = zb_or(n1120, n1121);
    let n1123: ZB = zb_and(n997, n1122);
    let n1124: ZB = zb_and(n996, n1122);
    let n1125: ZB = zb_or(n1123, n1124);
    let n1126: ZB = zb_and(n996, n1125);
    let n1127: ZB = zb_and(n997, n1125);
    let n1128: ZB = zb_or(n1126, n1127);
    let n1129: ZB = zb_or(n1118, n1128);
    let n1130: ZB = zb_and(n1001, n1129);
    let n1131: ZB = zb_and(n1000, n1129);
    let n1132: ZB = zb_or(n1130, n1131);
    let n1133: ZB = zb_or(n1110, n1132);
    let n1134: ZB = zb_and(n1003, n1133);
    let n1135: ZB = zb_and(n1004, n1133);
    let n1136: ZB = zb_or(n1134, n1135);
    let n1137: ZB = zb_and(n1014, n1136);
    let n1138: ZB = zb_and(n1015, n1136);
    let n1139: ZB = zb_or(n1137, n1138);
    let n1142: ZB = zb_and(n989, n1069);
    let n1143: ZB = zb_and(n990, n1069);
    let n1144: ZB = zb_and(n994, n1143);
    let n1145: ZB = zb_and(n993, n1143);
    let n1146: ZB = zb_or(n1144, n1145);
    let n1147: ZB = zb_and(n994, n1146);
    let n1148: ZB = zb_and(n993, n1146);
    let n1149: ZB = zb_or(n1147, n1148);
    let n1150: ZB = zb_and(n993, n1149);
    let n1151: ZB = zb_and(n994, n1149);
    let n1152: ZB = zb_and(n997, n1151);
    let n1153: ZB = zb_and(n996, n1151);
    let n1154: ZB = zb_or(n1152, n1153);
    let n1155: ZB = zb_and(n997, n1154);
    let n1156: ZB = zb_and(n996, n1154);
    let n1157: ZB = zb_or(n1155, n1156);
    let n1158: ZB = zb_and(n996, n1157);
    let n1159: ZB = zb_and(n997, n1157);
    let n1160: ZB = zb_or(n1158, n1159);
    let n1161: ZB = zb_or(n1150, n1160);
    let n1162: ZB = zb_and(n1001, n1161);
    let n1163: ZB = zb_and(n1000, n1161);
    let n1164: ZB = zb_or(n1162, n1163);
    let n1165: ZB = zb_or(n1142, n1164);
    let n1166: ZB = zb_and(n1003, n1165);
    let n1167: ZB = zb_and(n1004, n1165);
    let n1168: ZB = zb_or(n1166, n1167);
    let n1169: ZB = zb_and(n1014, n1168);
    let n1170: ZB = zb_and(n1015, n1168);
    let n1171: ZB = zb_or(n1169, n1170);
    let n1174: ZN = zsel_n(n1003, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1175: ZB = zb_or(n82, n1003);
    let n1176: ZB = zb_and(n1003, n1013);
    let n1177: ZB = zb_and(n1004, n1013);
    let n1178: ZB = zb_or(n1176, n1177);
    let n1179: ZB = zb_and(n1014, n1178);
    let n1180: ZB = zb_and(n1015, n1178);
    let n1181: ZB = zb_or(n1179, n1180);
    let n1182: ZB = zb_and(n1015, n1181);
    let n1183: ZB = zn_gt(n1174, zn_splat(P8::from_raw(0i32)));
    let n1184: ZB = zn_le(n1174, zn_splat(P8::from_raw(0i32)));
    let n1185: ZB = zb_and(n1182, n1183);
    let n1186: ZB = zb_and(n1182, n1184);
    let n1187: ZB = zb_or(n1185, n1186);
    let n1188: ZB = zb_and(n1003, n1045);
    let n1189: ZB = zb_and(n1004, n1045);
    let n1190: ZB = zb_or(n1188, n1189);
    let n1191: ZB = zb_and(n1014, n1190);
    let n1192: ZB = zb_and(n1015, n1190);
    let n1193: ZB = zb_or(n1191, n1192);
    let n1194: ZB = zb_and(n1015, n1193);
    let n1195: ZB = zb_and(n1183, n1194);
    let n1196: ZB = zb_and(n1184, n1194);
    let n1197: ZB = zb_or(n1195, n1196);
    let n1198: ZB = zb_and(n1003, n1072);
    let n1199: ZB = zb_and(n1004, n1072);
    let n1200: ZB = zb_or(n1198, n1199);
    let n1201: ZB = zb_and(n1014, n1200);
    let n1202: ZB = zb_and(n1015, n1200);
    let n1203: ZB = zb_or(n1201, n1202);
    let n1204: ZB = zb_and(n1015, n1203);
    let n1205: ZB = zb_and(n1183, n1204);
    let n1206: ZB = zb_and(n1184, n1204);
    let n1207: ZB = zb_or(n1205, n1206);
    let n1208: ZB = zb_and(n1003, n1104);
    let n1209: ZB = zb_and(n1004, n1104);
    let n1210: ZB = zb_or(n1208, n1209);
    let n1211: ZB = zb_and(n1014, n1210);
    let n1212: ZB = zb_and(n1015, n1210);
    let n1213: ZB = zb_or(n1211, n1212);
    let n1214: ZB = zb_and(n1015, n1213);
    let n1215: ZB = zb_and(n1183, n1214);
    let n1216: ZB = zb_and(n1184, n1214);
    let n1217: ZB = zb_or(n1215, n1216);
    let n1218: ZB = zb_and(n1003, n1136);
    let n1219: ZB = zb_and(n1004, n1136);
    let n1220: ZB = zb_or(n1218, n1219);
    let n1221: ZB = zb_and(n1014, n1220);
    let n1222: ZB = zb_and(n1015, n1220);
    let n1223: ZB = zb_or(n1221, n1222);
    let n1224: ZB = zb_and(n1015, n1223);
    let n1225: ZB = zb_and(n1183, n1224);
    let n1226: ZB = zb_and(n1184, n1224);
    let n1227: ZB = zb_or(n1225, n1226);
    let n1228: ZB = zb_and(n1003, n1168);
    let n1229: ZB = zb_and(n1004, n1168);
    let n1230: ZB = zb_or(n1228, n1229);
    let n1231: ZB = zb_and(n1014, n1230);
    let n1232: ZB = zb_and(n1015, n1230);
    let n1233: ZB = zb_or(n1231, n1232);
    let n1234: ZB = zb_and(n1015, n1233);
    let n1235: ZB = zb_and(n1183, n1234);
    let n1236: ZB = zb_and(n1184, n1234);
    let n1237: ZB = zb_or(n1235, n1236);
    let n1242: ZB = zb_and(r_c38, n66);
    let n1243: ZN = zsel_n(n63, n64, r_c39);
    let n1244: ZB = zsel_b(n63, n1242, r_c38);
    let n1245: ZB = zb_and(n950, n954);
    let n1246: ZB = zb_and(n967, n1245);
    let n1247: ZB = zb_and(n966, n1245);
    let n1248: ZB = zb_or(n1246, n1247);
    let n1249: ZB = zb_and(n967, n1248);
    let n1250: ZB = zb_and(n966, n1248);
    let n1251: ZB = zb_or(n1249, n1250);
    let n1252: ZB = zb_and(n966, n1251);
    let n1253: ZB = zb_and(n967, n1251);
    let n1254: ZB = zb_and(n974, n1252);
    let n1255: ZB = zb_and(n975, n1252);
    let n1256: ZB = zb_or(n1254, n1255);
    let n1257: ZB = zb_or(n1253, n1256);
    let n1258: ZB = zb_and(n967, n1257);
    let n1259: ZB = zb_and(n966, n1257);
    let n1260: ZB = zb_or(n1258, n1259);
    let n1261: ZB = zb_and(n967, n1260);
    let n1262: ZB = zb_and(n966, n1260);
    let n1263: ZB = zb_or(n1261, n1262);
    let n1264: ZB = zb_and(n1003, n1263);
    let n1265: ZB = zb_and(n1004, n1263);
    let n1266: ZB = zb_or(n1264, n1265);
    let n1267: ZB = zb_and(n1014, n1266);
    let n1268: ZB = zb_and(n1015, n1266);
    let n1269: ZB = zb_or(n1267, n1268);
    let n1270: ZB = zb_and(n1014, n1269);
    let n1271: ZB = zb_and(n1014, n1018);
    let n1272: ZN = zsel_n(n1270, r_c87, n961);
    let n1273: ZN = zsel_n(n1270, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1274: ZB = zb_not(n1270);
    let n1275: ZB = zb_or(n1244, n1274);
    let n1276: ZB = zb_or(n1270, n1271);
    let n1277: ZB = zsel_b(n1270, n951, n963);
    let n1281: ZB = zb_and(n1025, n1260);
    let n1282: ZB = zb_and(n1024, n1260);
    let n1283: ZB = zb_or(n1281, n1282);
    let n1284: ZB = zb_and(n1025, n1283);
    let n1285: ZB = zb_and(n1024, n1283);
    let n1286: ZB = zb_or(n1284, n1285);
    let n1287: ZB = zb_and(n1024, n1286);
    let n1288: ZB = zb_and(n1025, n1286);
    let n1289: ZB = zb_or(n1287, n1288);
    let n1290: ZB = zb_and(n1024, n1289);
    let n1291: ZB = zb_and(n1025, n1289);
    let n1292: ZB = zb_or(n1290, n1291);
    let n1293: ZB = zb_and(n967, n1292);
    let n1294: ZB = zb_and(n966, n1292);
    let n1295: ZB = zb_or(n1293, n1294);
    let n1296: ZB = zb_and(n1003, n1295);
    let n1297: ZB = zb_and(n1004, n1295);
    let n1298: ZB = zb_or(n1296, n1297);
    let n1299: ZB = zb_and(n1014, n1298);
    let n1300: ZB = zb_and(n1015, n1298);
    let n1301: ZB = zb_or(n1299, n1300);
    let n1302: ZB = zb_and(n1014, n1301);
    let n1303: ZB = zb_and(n1014, n1048);
    let n1304: ZN = zsel_n(n1302, r_c87, n961);
    let n1305: ZN = zsel_n(n1302, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1306: ZB = zb_not(n1302);
    let n1307: ZB = zb_or(n1244, n1306);
    let n1308: ZB = zb_or(n1302, n1303);
    let n1309: ZB = zsel_b(n1302, n951, n963);
    let n1311: ZB = zb_and(n1054, n1260);
    let n1312: ZB = zb_and(n1053, n1260);
    let n1313: ZB = zb_or(n1311, n1312);
    let n1314: ZB = zb_and(n1054, n1313);
    let n1315: ZB = zb_and(n1053, n1313);
    let n1316: ZB = zb_or(n1314, n1315);
    let n1317: ZB = zb_and(n1053, n1316);
    let n1318: ZB = zb_and(n1054, n1316);
    let n1319: ZB = zb_or(n1317, n1318);
    let n1320: ZB = zb_and(n1053, n1319);
    let n1321: ZB = zb_and(n1054, n1319);
    let n1322: ZB = zb_or(n1320, n1321);
    let n1323: ZB = zb_and(n967, n1322);
    let n1324: ZB = zb_and(n966, n1322);
    let n1325: ZB = zb_or(n1323, n1324);
    let n1326: ZB = zb_and(n1003, n1325);
    let n1327: ZB = zb_and(n1004, n1325);
    let n1328: ZB = zb_or(n1326, n1327);
    let n1329: ZB = zb_and(n1014, n1328);
    let n1330: ZB = zb_and(n1015, n1328);
    let n1331: ZB = zb_or(n1329, n1330);
    let n1332: ZB = zb_and(n1014, n1331);
    let n1333: ZB = zb_and(n1014, n1075);
    let n1334: ZN = zsel_n(n1332, r_c87, n961);
    let n1335: ZN = zsel_n(n1332, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1336: ZB = zb_not(n1332);
    let n1337: ZB = zb_or(n1244, n1336);
    let n1338: ZB = zb_or(n1332, n1333);
    let n1339: ZB = zsel_b(n1332, n951, n963);
    let n1341: ZB = zb_and(n989, n1263);
    let n1342: ZB = zb_and(n990, n1263);
    let n1343: ZB = zb_and(n994, n1342);
    let n1344: ZB = zb_and(n993, n1342);
    let n1345: ZB = zb_or(n1343, n1344);
    let n1346: ZB = zb_and(n994, n1345);
    let n1347: ZB = zb_and(n993, n1345);
    let n1348: ZB = zb_or(n1346, n1347);
    let n1349: ZB = zb_and(n993, n1348);
    let n1350: ZB = zb_and(n994, n1348);
    let n1351: ZB = zb_and(n997, n1350);
    let n1352: ZB = zb_and(n996, n1350);
    let n1353: ZB = zb_or(n1351, n1352);
    let n1354: ZB = zb_and(n997, n1353);
    let n1355: ZB = zb_and(n996, n1353);
    let n1356: ZB = zb_or(n1354, n1355);
    let n1357: ZB = zb_and(n996, n1356);
    let n1358: ZB = zb_and(n997, n1356);
    let n1359: ZB = zb_or(n1357, n1358);
    let n1360: ZB = zb_or(n1349, n1359);
    let n1361: ZB = zb_and(n1001, n1360);
    let n1362: ZB = zb_and(n1000, n1360);
    let n1363: ZB = zb_or(n1361, n1362);
    let n1364: ZB = zb_or(n1341, n1363);
    let n1365: ZB = zb_and(n1003, n1364);
    let n1366: ZB = zb_and(n1004, n1364);
    let n1367: ZB = zb_or(n1365, n1366);
    let n1368: ZB = zb_and(n1014, n1367);
    let n1369: ZB = zb_and(n1015, n1367);
    let n1370: ZB = zb_or(n1368, n1369);
    let n1371: ZB = zb_and(n1014, n1370);
    let n1372: ZB = zb_and(n1014, n1107);
    let n1373: ZN = zsel_n(n1371, r_c87, n961);
    let n1374: ZN = zsel_n(n1371, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1375: ZB = zb_not(n1371);
    let n1376: ZB = zb_or(n1244, n1375);
    let n1377: ZB = zb_or(n1371, n1372);
    let n1378: ZB = zsel_b(n1371, n951, n963);
    let n1380: ZB = zb_and(n989, n1295);
    let n1381: ZB = zb_and(n990, n1295);
    let n1382: ZB = zb_and(n994, n1381);
    let n1383: ZB = zb_and(n993, n1381);
    let n1384: ZB = zb_or(n1382, n1383);
    let n1385: ZB = zb_and(n994, n1384);
    let n1386: ZB = zb_and(n993, n1384);
    let n1387: ZB = zb_or(n1385, n1386);
    let n1388: ZB = zb_and(n993, n1387);
    let n1389: ZB = zb_and(n994, n1387);
    let n1390: ZB = zb_and(n997, n1389);
    let n1391: ZB = zb_and(n996, n1389);
    let n1392: ZB = zb_or(n1390, n1391);
    let n1393: ZB = zb_and(n997, n1392);
    let n1394: ZB = zb_and(n996, n1392);
    let n1395: ZB = zb_or(n1393, n1394);
    let n1396: ZB = zb_and(n996, n1395);
    let n1397: ZB = zb_and(n997, n1395);
    let n1398: ZB = zb_or(n1396, n1397);
    let n1399: ZB = zb_or(n1388, n1398);
    let n1400: ZB = zb_and(n1001, n1399);
    let n1401: ZB = zb_and(n1000, n1399);
    let n1402: ZB = zb_or(n1400, n1401);
    let n1403: ZB = zb_or(n1380, n1402);
    let n1404: ZB = zb_and(n1003, n1403);
    let n1405: ZB = zb_and(n1004, n1403);
    let n1406: ZB = zb_or(n1404, n1405);
    let n1407: ZB = zb_and(n1014, n1406);
    let n1408: ZB = zb_and(n1015, n1406);
    let n1409: ZB = zb_or(n1407, n1408);
    let n1410: ZB = zb_and(n1014, n1409);
    let n1411: ZB = zb_and(n1014, n1139);
    let n1412: ZN = zsel_n(n1410, r_c87, n961);
    let n1413: ZN = zsel_n(n1410, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1414: ZB = zb_not(n1410);
    let n1415: ZB = zb_or(n1244, n1414);
    let n1416: ZB = zb_or(n1410, n1411);
    let n1417: ZB = zsel_b(n1410, n951, n963);
    let n1419: ZB = zb_and(n989, n1325);
    let n1420: ZB = zb_and(n990, n1325);
    let n1421: ZB = zb_and(n994, n1420);
    let n1422: ZB = zb_and(n993, n1420);
    let n1423: ZB = zb_or(n1421, n1422);
    let n1424: ZB = zb_and(n994, n1423);
    let n1425: ZB = zb_and(n993, n1423);
    let n1426: ZB = zb_or(n1424, n1425);
    let n1427: ZB = zb_and(n993, n1426);
    let n1428: ZB = zb_and(n994, n1426);
    let n1429: ZB = zb_and(n997, n1428);
    let n1430: ZB = zb_and(n996, n1428);
    let n1431: ZB = zb_or(n1429, n1430);
    let n1432: ZB = zb_and(n997, n1431);
    let n1433: ZB = zb_and(n996, n1431);
    let n1434: ZB = zb_or(n1432, n1433);
    let n1435: ZB = zb_and(n996, n1434);
    let n1436: ZB = zb_and(n997, n1434);
    let n1437: ZB = zb_or(n1435, n1436);
    let n1438: ZB = zb_or(n1427, n1437);
    let n1439: ZB = zb_and(n1001, n1438);
    let n1440: ZB = zb_and(n1000, n1438);
    let n1441: ZB = zb_or(n1439, n1440);
    let n1442: ZB = zb_or(n1419, n1441);
    let n1443: ZB = zb_and(n1003, n1442);
    let n1444: ZB = zb_and(n1004, n1442);
    let n1445: ZB = zb_or(n1443, n1444);
    let n1446: ZB = zb_and(n1014, n1445);
    let n1447: ZB = zb_and(n1015, n1445);
    let n1448: ZB = zb_or(n1446, n1447);
    let n1449: ZB = zb_and(n1014, n1448);
    let n1450: ZB = zb_and(n1014, n1171);
    let n1451: ZN = zsel_n(n1449, r_c87, n961);
    let n1452: ZN = zsel_n(n1449, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1453: ZB = zb_not(n1449);
    let n1454: ZB = zb_or(n1244, n1453);
    let n1455: ZB = zb_or(n1449, n1450);
    let n1456: ZB = zsel_b(n1449, n951, n963);
    let n1458: ZB = zb_and(n1003, n1266);
    let n1459: ZB = zb_and(n1004, n1266);
    let n1460: ZB = zb_or(n1458, n1459);
    let n1461: ZB = zb_and(n1014, n1460);
    let n1462: ZB = zb_and(n1015, n1460);
    let n1463: ZB = zb_or(n1461, n1462);
    let n1464: ZB = zb_and(n1014, n1463);
    let n1465: ZB = zb_and(n1014, n1181);
    let n1466: ZN = zsel_n(n1464, r_c87, n961);
    let n1467: ZN = zsel_n(n1464, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1468: ZB = zb_not(n1464);
    let n1469: ZB = zb_or(n1244, n1468);
    let n1470: ZB = zb_or(n1464, n1465);
    let n1471: ZB = zsel_b(n1464, n951, n963);
    let n1472: ZB = zb_and(n1183, n1470);
    let n1473: ZB = zb_and(n1184, n1470);
    let n1474: ZB = zb_or(n1472, n1473);
    let n1475: ZB = zb_and(n1003, n1298);
    let n1476: ZB = zb_and(n1004, n1298);
    let n1477: ZB = zb_or(n1475, n1476);
    let n1478: ZB = zb_and(n1014, n1477);
    let n1479: ZB = zb_and(n1015, n1477);
    let n1480: ZB = zb_or(n1478, n1479);
    let n1481: ZB = zb_and(n1014, n1480);
    let n1482: ZB = zb_and(n1014, n1193);
    let n1483: ZN = zsel_n(n1481, r_c87, n961);
    let n1484: ZN = zsel_n(n1481, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1485: ZB = zb_not(n1481);
    let n1486: ZB = zb_or(n1244, n1485);
    let n1487: ZB = zb_or(n1481, n1482);
    let n1488: ZB = zsel_b(n1481, n951, n963);
    let n1489: ZB = zb_and(n1183, n1487);
    let n1490: ZB = zb_and(n1184, n1487);
    let n1491: ZB = zb_or(n1489, n1490);
    let n1492: ZB = zb_and(n1003, n1328);
    let n1493: ZB = zb_and(n1004, n1328);
    let n1494: ZB = zb_or(n1492, n1493);
    let n1495: ZB = zb_and(n1014, n1494);
    let n1496: ZB = zb_and(n1015, n1494);
    let n1497: ZB = zb_or(n1495, n1496);
    let n1498: ZB = zb_and(n1014, n1497);
    let n1499: ZB = zb_and(n1014, n1203);
    let n1500: ZN = zsel_n(n1498, r_c87, n961);
    let n1501: ZN = zsel_n(n1498, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1502: ZB = zb_not(n1498);
    let n1503: ZB = zb_or(n1244, n1502);
    let n1504: ZB = zb_or(n1498, n1499);
    let n1505: ZB = zsel_b(n1498, n951, n963);
    let n1506: ZB = zb_and(n1183, n1504);
    let n1507: ZB = zb_and(n1184, n1504);
    let n1508: ZB = zb_or(n1506, n1507);
    let n1509: ZB = zb_and(n1003, n1367);
    let n1510: ZB = zb_and(n1004, n1367);
    let n1511: ZB = zb_or(n1509, n1510);
    let n1512: ZB = zb_and(n1014, n1511);
    let n1513: ZB = zb_and(n1015, n1511);
    let n1514: ZB = zb_or(n1512, n1513);
    let n1515: ZB = zb_and(n1014, n1514);
    let n1516: ZB = zb_and(n1014, n1213);
    let n1517: ZN = zsel_n(n1515, r_c87, n961);
    let n1518: ZN = zsel_n(n1515, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1519: ZB = zb_not(n1515);
    let n1520: ZB = zb_or(n1244, n1519);
    let n1521: ZB = zb_or(n1515, n1516);
    let n1522: ZB = zsel_b(n1515, n951, n963);
    let n1523: ZB = zb_and(n1183, n1521);
    let n1524: ZB = zb_and(n1184, n1521);
    let n1525: ZB = zb_or(n1523, n1524);
    let n1526: ZB = zb_and(n1003, n1406);
    let n1527: ZB = zb_and(n1004, n1406);
    let n1528: ZB = zb_or(n1526, n1527);
    let n1529: ZB = zb_and(n1014, n1528);
    let n1530: ZB = zb_and(n1015, n1528);
    let n1531: ZB = zb_or(n1529, n1530);
    let n1532: ZB = zb_and(n1014, n1531);
    let n1533: ZB = zb_and(n1014, n1223);
    let n1534: ZN = zsel_n(n1532, r_c87, n961);
    let n1535: ZN = zsel_n(n1532, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1536: ZB = zb_not(n1532);
    let n1537: ZB = zb_or(n1244, n1536);
    let n1538: ZB = zb_or(n1532, n1533);
    let n1539: ZB = zsel_b(n1532, n951, n963);
    let n1540: ZB = zb_and(n1183, n1538);
    let n1541: ZB = zb_and(n1184, n1538);
    let n1542: ZB = zb_or(n1540, n1541);
    let n1543: ZB = zb_and(n1003, n1445);
    let n1544: ZB = zb_and(n1004, n1445);
    let n1545: ZB = zb_or(n1543, n1544);
    let n1546: ZB = zb_and(n1014, n1545);
    let n1547: ZB = zb_and(n1015, n1545);
    let n1548: ZB = zb_or(n1546, n1547);
    let n1549: ZB = zb_and(n1014, n1548);
    let n1550: ZB = zb_and(n1014, n1233);
    let n1551: ZN = zsel_n(n1549, r_c87, n961);
    let n1552: ZN = zsel_n(n1549, n1243, zn_splat(P8::from_raw(983040i32)));
    let n1553: ZB = zb_not(n1549);
    let n1554: ZB = zb_or(n1244, n1553);
    let n1555: ZB = zb_or(n1549, n1550);
    let n1556: ZB = zsel_b(n1549, n951, n963);
    let n1557: ZB = zb_and(n1183, n1555);
    let n1558: ZB = zb_and(n1184, n1555);
    let n1559: ZB = zb_or(n1557, n1558);
    let n1561: ZB = zb_and(r_c43, n285);
    let n1563: ZN = zsel_n(n967, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n1565: ZN = zn_neg(n999);
    let n1566: ZN = zn_mul(n1565, zn_splat(P8::from_raw(131072i32)));
    let n1567: ZN = zsel_n(n1001, n1566, zn_splat(P8::from_raw(0i32)));
    let n1568: ZN = zsel_n(n1001, zn_splat(P8::from_raw(-131072i32)), n1563);
    let n1569: ZN = zsel_n(n989, zn_splat(P8::from_raw(0i32)), n976);
    let n1570: ZN = zsel_n(n989, zn_splat(P8::from_raw(0i32)), n1567);
    let n1571: ZN = zsel_n(n989, zn_splat(P8::from_raw(-131072i32)), n1568);
    let n1572: ZN = zn_sub(r_c88, zn_splat(P8::from_raw(65536i32)));
    let n1575: ZB = zb_and(n1015, n1269);
    let n1576: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-65536i32)));
    let n1577: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n976);
    let n1578: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1563);
    let n1579: ZB = zb_or(n1561, n1575);
    let n1580: ZB = zsel_b(r_c43, n274, n951);
    let n1581: ZB = zb_and(n61, n1579);
    let n1582: ZB = zn_lt(n272, zn_splat(P8::from_raw(-65536i32)));
    let n1583: ZB = zn_ge(n272, zn_splat(P8::from_raw(-65536i32)));
    let n1584: ZB = zb_and(n1581, n1583);
    let n1585: ZB = zb_and(n1581, n1582);
    let n1587: ZB = zn_gt(n272, zn_splat(P8::from_raw(7929856i32)));
    let n1588: ZB = zb_or(n1584, n1585);
    let n1589: ZB = zb_or(n1582, n1587);
    let n1590: ZB = zb_not(n1589);
    let n1591: ZB = zb_and(n1588, n1589);
    let n1592: ZB = zb_and(n1588, n1590);
    let n1593: ZN = zn_min(n272, zn_splat(P8::from_raw(7929856i32)));
    let n1594: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1593);
    let n1595: ZN = zsel_n(n1589, n1594, n272);
    let n1596: ZB = zb_or(n1591, n1592);
    let n1597: ZN = zsel_n(n1020, n272, n1595);
    let n1598: ZN = zsel_n(n1001, n1566, n1022);
    let n1599: ZN = zsel_n(n989, n1022, n1598);
    let n1600: ZB = zb_and(n1015, n1301);
    let n1601: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1022);
    let n1602: ZB = zb_or(n1561, n1600);
    let n1603: ZB = zb_and(n61, n1602);
    let n1604: ZB = zb_and(n1583, n1603);
    let n1605: ZB = zb_and(n1582, n1603);
    let n1606: ZB = zb_or(n1604, n1605);
    let n1607: ZB = zb_and(n1589, n1606);
    let n1608: ZB = zb_and(n1590, n1606);
    let n1609: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1601);
    let n1610: ZB = zb_or(n1607, n1608);
    let n1611: ZN = zsel_n(n1020, n1601, n1609);
    let n1612: ZN = zsel_n(n1001, n1566, n1051);
    let n1613: ZN = zsel_n(n989, n1051, n1612);
    let n1614: ZB = zb_and(n1015, n1331);
    let n1615: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1051);
    let n1616: ZB = zb_or(n1561, n1614);
    let n1617: ZB = zb_and(n61, n1616);
    let n1618: ZB = zb_and(n1583, n1617);
    let n1619: ZB = zb_and(n1582, n1617);
    let n1620: ZB = zb_or(n1618, n1619);
    let n1621: ZB = zb_and(n1589, n1620);
    let n1622: ZB = zb_and(n1590, n1620);
    let n1623: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1615);
    let n1624: ZB = zb_or(n1621, n1622);
    let n1625: ZN = zsel_n(n1020, n1615, n1623);
    let n1626: ZB = zb_and(n1015, n1370);
    let n1627: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1569);
    let n1628: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1570);
    let n1629: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1571);
    let n1630: ZB = zb_or(n1561, n1626);
    let n1631: ZB = zb_and(n61, n1630);
    let n1632: ZB = zb_and(n1583, n1631);
    let n1633: ZB = zb_and(n1582, n1631);
    let n1634: ZB = zb_or(n1632, n1633);
    let n1635: ZB = zb_and(n1589, n1634);
    let n1636: ZB = zb_and(n1590, n1634);
    let n1637: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1628);
    let n1638: ZB = zb_or(n1635, n1636);
    let n1639: ZN = zsel_n(n1020, n1628, n1637);
    let n1640: ZB = zb_and(n1015, n1409);
    let n1641: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1599);
    let n1642: ZB = zb_or(n1561, n1640);
    let n1643: ZB = zb_and(n61, n1642);
    let n1644: ZB = zb_and(n1583, n1643);
    let n1645: ZB = zb_and(n1582, n1643);
    let n1646: ZB = zb_or(n1644, n1645);
    let n1647: ZB = zb_and(n1589, n1646);
    let n1648: ZB = zb_and(n1590, n1646);
    let n1649: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1641);
    let n1650: ZB = zb_or(n1647, n1648);
    let n1651: ZN = zsel_n(n1020, n1641, n1649);
    let n1652: ZB = zb_and(n1015, n1448);
    let n1653: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1613);
    let n1654: ZB = zb_or(n1561, n1652);
    let n1655: ZB = zb_and(n61, n1654);
    let n1656: ZB = zb_and(n1583, n1655);
    let n1657: ZB = zb_and(n1582, n1655);
    let n1658: ZB = zb_or(n1656, n1657);
    let n1659: ZB = zb_and(n1589, n1658);
    let n1660: ZB = zb_and(n1590, n1658);
    let n1661: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1653);
    let n1662: ZB = zb_or(n1659, n1660);
    let n1663: ZN = zsel_n(n1020, n1653, n1661);
    let n1664: ZN = zsel_n(n1003, zn_splat(P8::from_raw(655360i32)), zn_splat(P8::from_raw(-65536i32)));
    let n1665: ZN = zsel_n(n1003, zn_splat(P8::from_raw(262144i32)), zn_splat(P8::from_raw(0i32)));
    let n1666: ZN = zsel_n(n1003, n1572, r_c88);
    let n1667: ZN = zsel_n(n1003, zn_splat(P8::from_raw(98304i32)), zn_splat(P8::from_raw(0i32)));
    let n1668: ZN = zsel_n(n1003, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(0i32)));
    let n1669: ZN = zsel_n(n1003, zn_splat(P8::from_raw(131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1670: ZN = zsel_n(n1003, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1671: ZN = zsel_n(n1003, zn_splat(P8::from_raw(0i32)), n1563);
    let n1672: ZB = zb_and(n1015, n1463);
    let n1673: ZN = zsel_n(r_c43, r_c20, n1174);
    let n1674: ZB = zsel_b(r_c43, n82, n1175);
    let n1675: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1664);
    let n1676: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1665);
    let n1677: ZN = zsel_n(r_c43, r_c88, n1666);
    let n1678: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1667);
    let n1679: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1668);
    let n1680: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1669);
    let n1681: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1670);
    let n1682: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1671);
    let n1683: ZB = zb_or(n1561, n1672);
    let n1684: ZB = zn_gt(n1673, zn_splat(P8::from_raw(0i32)));
    let n1685: ZB = zn_le(n1673, zn_splat(P8::from_raw(0i32)));
    let n1686: ZB = zb_and(n1683, n1684);
    let n1687: ZB = zb_and(n1683, n1685);
    let n1688: ZB = zb_and(n1583, n1687);
    let n1689: ZB = zb_and(n1582, n1687);
    let n1690: ZB = zb_or(n1688, n1689);
    let n1691: ZB = zb_and(n1589, n1690);
    let n1692: ZB = zb_and(n1590, n1690);
    let n1693: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1681);
    let n1694: ZB = zb_or(n1691, n1692);
    let n1695: ZN = zsel_n(n1684, n272, n1595);
    let n1696: ZN = zsel_n(n1684, n1681, n1693);
    let n1697: ZB = zb_or(n1686, n1694);
    let n1698: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1699: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-327680i32)), n1022);
    let n1700: ZB = zb_and(n1015, n1480);
    let n1701: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1698);
    let n1702: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1699);
    let n1703: ZB = zb_or(n1561, n1700);
    let n1704: ZB = zb_and(n1684, n1703);
    let n1705: ZB = zb_and(n1685, n1703);
    let n1706: ZB = zb_and(n1583, n1705);
    let n1707: ZB = zb_and(n1582, n1705);
    let n1708: ZB = zb_or(n1706, n1707);
    let n1709: ZB = zb_and(n1589, n1708);
    let n1710: ZB = zb_and(n1590, n1708);
    let n1711: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1702);
    let n1712: ZB = zb_or(n1709, n1710);
    let n1713: ZN = zsel_n(n1684, n1702, n1711);
    let n1714: ZB = zb_or(n1704, n1712);
    let n1715: ZN = zsel_n(n1003, zn_splat(P8::from_raw(327680i32)), n1051);
    let n1716: ZB = zb_and(n1015, n1497);
    let n1717: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1715);
    let n1718: ZB = zb_or(n1561, n1716);
    let n1719: ZB = zb_and(n1684, n1718);
    let n1720: ZB = zb_and(n1685, n1718);
    let n1721: ZB = zb_and(n1583, n1720);
    let n1722: ZB = zb_and(n1582, n1720);
    let n1723: ZB = zb_or(n1721, n1722);
    let n1724: ZB = zb_and(n1589, n1723);
    let n1725: ZB = zb_and(n1590, n1723);
    let n1726: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1717);
    let n1727: ZB = zb_or(n1724, n1725);
    let n1728: ZN = zsel_n(n1684, n1717, n1726);
    let n1729: ZB = zb_or(n1719, n1727);
    let n1731: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-98304i32)), zn_splat(P8::from_raw(0i32)));
    let n1732: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-327680i32)), n1563);
    let n1733: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1731);
    let n1734: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1732);
    let n1735: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-231700i32)), n1022);
    let n1736: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-231700i32)), n1563);
    let n1737: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1735);
    let n1738: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1736);
    let n1739: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1737);
    let n1740: ZN = zsel_n(n1684, n1737, n1739);
    let n1741: ZN = zsel_n(n1003, zn_splat(P8::from_raw(231700i32)), n1051);
    let n1742: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1741);
    let n1743: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1742);
    let n1744: ZN = zsel_n(n1684, n1742, n1743);
    let n1745: ZN = zsel_n(n1003, zn_splat(P8::from_raw(327680i32)), n1563);
    let n1746: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1745);
    let n1747: ZN = zsel_n(n1003, zn_splat(P8::from_raw(231700i32)), n1563);
    let n1748: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1747);
    let n1749: ZN = zsel_n(n1003, zn_splat(P8::from_raw(65536i32)), n1570);
    let n1750: ZN = zsel_n(n1003, zn_splat(P8::from_raw(0i32)), n1571);
    let n1751: ZB = zb_and(n1015, n1514);
    let n1752: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1749);
    let n1753: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1750);
    let n1754: ZB = zb_or(n1561, n1751);
    let n1755: ZB = zb_and(n1684, n1754);
    let n1756: ZB = zb_and(n1685, n1754);
    let n1757: ZB = zb_and(n1583, n1756);
    let n1758: ZB = zb_and(n1582, n1756);
    let n1759: ZB = zb_or(n1757, n1758);
    let n1760: ZB = zb_and(n1589, n1759);
    let n1761: ZB = zb_and(n1590, n1759);
    let n1762: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1752);
    let n1763: ZB = zb_or(n1760, n1761);
    let n1764: ZN = zsel_n(n1684, n1752, n1762);
    let n1765: ZB = zb_or(n1755, n1763);
    let n1766: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-327680i32)), n1599);
    let n1767: ZB = zb_and(n1015, n1531);
    let n1768: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1766);
    let n1769: ZB = zb_or(n1561, n1767);
    let n1770: ZB = zb_and(n1684, n1769);
    let n1771: ZB = zb_and(n1685, n1769);
    let n1772: ZB = zb_and(n1583, n1771);
    let n1773: ZB = zb_and(n1582, n1771);
    let n1774: ZB = zb_or(n1772, n1773);
    let n1775: ZB = zb_and(n1589, n1774);
    let n1776: ZB = zb_and(n1590, n1774);
    let n1777: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1768);
    let n1778: ZB = zb_or(n1775, n1776);
    let n1779: ZN = zsel_n(n1684, n1768, n1777);
    let n1780: ZB = zb_or(n1770, n1778);
    let n1781: ZN = zsel_n(n1003, zn_splat(P8::from_raw(327680i32)), n1613);
    let n1782: ZB = zb_and(n1015, n1548);
    let n1783: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1781);
    let n1784: ZB = zb_or(n1561, n1782);
    let n1785: ZB = zb_and(n1684, n1784);
    let n1786: ZB = zb_and(n1685, n1784);
    let n1787: ZB = zb_and(n1583, n1786);
    let n1788: ZB = zb_and(n1582, n1786);
    let n1789: ZB = zb_or(n1787, n1788);
    let n1790: ZB = zb_and(n1589, n1789);
    let n1791: ZB = zb_and(n1590, n1789);
    let n1792: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1783);
    let n1793: ZB = zb_or(n1790, n1791);
    let n1794: ZN = zsel_n(n1684, n1783, n1792);
    let n1795: ZB = zb_or(n1785, n1793);
    let n1796: ZN = zsel_n(n1003, zn_splat(P8::from_raw(0i32)), n1570);
    let n1797: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-327680i32)), n1571);
    let n1798: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1796);
    let n1799: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1797);
    let n1800: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1798);
    let n1801: ZN = zsel_n(n1684, n1798, n1800);
    let n1802: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-231700i32)), n1599);
    let n1803: ZN = zsel_n(n1003, zn_splat(P8::from_raw(-231700i32)), n1571);
    let n1804: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1802);
    let n1805: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1803);
    let n1806: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1804);
    let n1807: ZN = zsel_n(n1684, n1804, n1806);
    let n1808: ZN = zsel_n(n1003, zn_splat(P8::from_raw(231700i32)), n1613);
    let n1809: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1808);
    let n1810: ZN = zsel_n(n1589, zn_splat(P8::from_raw(0i32)), n1809);
    let n1811: ZN = zsel_n(n1684, n1809, n1810);
    let n1812: ZN = zsel_n(n1003, zn_splat(P8::from_raw(327680i32)), n1571);
    let n1813: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1812);
    let n1814: ZN = zsel_n(n1003, zn_splat(P8::from_raw(231700i32)), n1571);
    let n1815: ZN = zsel_n(r_c43, zn_splat(P8::from_raw(0i32)), n1814);
    let n1822: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1823: ZB = zb_or(n65, r_c232);
    let n1824: ZN = zsel_n(n65, zn_splat(P8::from_raw(196608i32)), r_c244);
    let n1825: ZB = zb_and(n66, r_c262);
    let n1826: ZB = zb_and(n66, r_c263);
    let n1827: ZN = zsel_n(n65, zn_splat(P8::from_raw(524288i32)), r_c272);
    let n1828: ZN = zsel_n(n65, zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n1829: ZB = zsel_b(n63, n1823, r_c232);
    let n1830: ZN = zsel_n(n63, n1824, r_c244);
    let n1831: ZB = zsel_b(n63, n1825, r_c262);
    let n1832: ZB = zsel_b(n63, n1826, r_c263);
    let n1833: ZN = zsel_n(n63, n1827, r_c272);
    let n1834: ZN = zsel_n(n63, n1828, r_c273);
    let n1835: ZN = zn_sub(n105, zn_splat(P8::from_raw(32768i32)));
    let n1836: ZN = zn_sub(n1835, n106);
    let n1837: ZN = zsel_n(n153, zn_splat(P8::from_raw(0i32)), n1836);
    let n1838: ZN = zsel_n(n153, zn_splat(P8::from_raw(0i32)), n95);
    let n1839: ZN = zsel_n(n150, n1836, n1837);
    let n1840: ZN = zsel_n(n150, n95, n1838);
    let n1841: ZN = zsel_n(n148, zn_splat(P8::from_raw(0i32)), n1839);
    let n1842: ZN = zsel_n(n148, zn_splat(P8::from_raw(0i32)), n1840);
    let n1843: ZN = zsel_n(n145, n1836, n1841);
    let n1844: ZN = zsel_n(n145, n95, n1842);
    let n1845: ZN = zsel_n(n143, zn_splat(P8::from_raw(0i32)), n1843);
    let n1846: ZN = zsel_n(n143, zn_splat(P8::from_raw(0i32)), n1844);
    let n1847: ZN = zsel_n(n140, n1836, n1845);
    let n1848: ZN = zsel_n(n140, n95, n1846);
    let n1849: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n1847);
    let n1850: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n1848);
    let n1851: ZN = zsel_n(n135, n1836, n1849);
    let n1852: ZN = zsel_n(n135, n95, n1850);
    let n1853: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1851);
    let n1854: ZN = zsel_n(n133, zn_splat(P8::from_raw(0i32)), n1852);
    let n1855: ZN = zsel_n(n130, n1836, n1853);
    let n1856: ZN = zsel_n(n130, n95, n1854);
    let n1857: ZN = zsel_n(n128, zn_splat(P8::from_raw(0i32)), n1855);
    let n1858: ZN = zsel_n(n128, zn_splat(P8::from_raw(0i32)), n1856);
    let n1859: ZN = zsel_n(n125, n1836, n1857);
    let n1860: ZN = zsel_n(n125, n95, n1858);
    let n1861: ZN = zsel_n(n123, zn_splat(P8::from_raw(0i32)), n1859);
    let n1862: ZN = zsel_n(n123, zn_splat(P8::from_raw(0i32)), n1860);
    let n1863: ZN = zsel_n(n120, n1836, n1861);
    let n1864: ZN = zsel_n(n120, n95, n1862);
    let n1865: ZN = zsel_n(n118, zn_splat(P8::from_raw(0i32)), n1863);
    let n1866: ZN = zsel_n(n118, zn_splat(P8::from_raw(0i32)), n1864);
    let n1867: ZN = zsel_n(n85, n1865, n1836);
    let n1868: ZN = zsel_n(n85, n1866, n95);
    let n1869: ZN = zn_sub(n190, zn_splat(P8::from_raw(32768i32)));
    let n1870: ZN = zn_sub(n1869, n191);
    let n1871: ZN = zsel_n(n235, zn_splat(P8::from_raw(0i32)), n1870);
    let n1872: ZN = zsel_n(n235, zn_splat(P8::from_raw(0i32)), n96);
    let n1873: ZN = zsel_n(n232, n1870, n1871);
    let n1874: ZN = zsel_n(n232, n96, n1872);
    let n1875: ZN = zsel_n(n230, zn_splat(P8::from_raw(0i32)), n1873);
    let n1876: ZN = zsel_n(n230, zn_splat(P8::from_raw(0i32)), n1874);
    let n1877: ZN = zsel_n(n227, n1870, n1875);
    let n1878: ZN = zsel_n(n227, n96, n1876);
    let n1879: ZN = zsel_n(n225, zn_splat(P8::from_raw(0i32)), n1877);
    let n1880: ZN = zsel_n(n225, zn_splat(P8::from_raw(0i32)), n1878);
    let n1881: ZN = zsel_n(n222, n1870, n1879);
    let n1882: ZN = zsel_n(n222, n96, n1880);
    let n1883: ZN = zsel_n(n220, zn_splat(P8::from_raw(0i32)), n1881);
    let n1884: ZN = zsel_n(n220, zn_splat(P8::from_raw(0i32)), n1882);
    let n1885: ZN = zsel_n(n217, n1870, n1883);
    let n1886: ZN = zsel_n(n217, n96, n1884);
    let n1887: ZN = zsel_n(n215, zn_splat(P8::from_raw(0i32)), n1885);
    let n1888: ZN = zsel_n(n215, zn_splat(P8::from_raw(0i32)), n1886);
    let n1889: ZN = zsel_n(n212, n1870, n1887);
    let n1890: ZN = zsel_n(n212, n96, n1888);
    let n1891: ZN = zsel_n(n210, zn_splat(P8::from_raw(0i32)), n1889);
    let n1892: ZN = zsel_n(n210, zn_splat(P8::from_raw(0i32)), n1890);
    let n1893: ZN = zsel_n(n207, n1870, n1891);
    let n1894: ZN = zsel_n(n207, n96, n1892);
    let n1895: ZN = zsel_n(n205, zn_splat(P8::from_raw(0i32)), n1893);
    let n1896: ZN = zsel_n(n205, zn_splat(P8::from_raw(0i32)), n1894);
    let n1897: ZN = zsel_n(n202, n1870, n1895);
    let n1898: ZN = zsel_n(n202, n96, n1896);
    let n1899: ZN = zsel_n(n200, zn_splat(P8::from_raw(0i32)), n1897);
    let n1900: ZN = zsel_n(n200, zn_splat(P8::from_raw(0i32)), n1898);
    let n1901: ZN = zsel_n(n85, n1899, n1870);
    let n1902: ZN = zsel_n(n85, n1900, n96);
    let n1903: ZN = zsel_n(n101, n1867, n93);
    let n1904: ZN = zsel_n(n101, n1901, n94);
    let n1905: ZN = zsel_n(n101, n1868, n95);
    let n1906: ZN = zsel_n(n101, n1902, n96);
    let n1907: ZB = zb_and(n61, n275);
    let n1908: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n1834);
    let n1909: ZB = zn_lt(n273, n1908);
    let n1910: ZB = zn_ge(n273, n1908);
    let n1911: ZB = zb_and(n1907, n1909);
    let n1912: ZB = zb_and(n1907, n1910);
    let n1913: ZN = zsel_n(n1909, zn_splat(P8::from_raw(196608i32)), n84);
    let n1914: ZN = zsel_n(n1909, zn_splat(P8::from_raw(65536i32)), n86);
    let n1915: ZB = zb_or(n1911, n1912);
    let n1916: ZB = zb_and(n277, n278);
    let n1917: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1906);
    let n1918: ZB = zn_gt(n1917, zn_splat(P8::from_raw(0i32)));
    let n1919: ZB = zn_le(n1917, zn_splat(P8::from_raw(0i32)));
    let n1920: ZB = zb_and(n1916, n1918);
    let n1921: ZB = zb_and(n1916, n1919);
    let n1922: ZB = zn_gt(n84, zn_splat(P8::from_raw(0i32)));
    let n1923: ZB = zb_or(n1920, n1921);
    let n1924: ZB = zb_and(n1918, n1922);
    let n1925: ZB = zb_not(n1924);
    let n1926: ZB = zb_and(n1923, n1924);
    let n1927: ZB = zb_and(n1923, n1925);
    let n1928: ZN = zsel_n(n1924, n281, n84);
    let n1929: ZN = zsel_n(n1924, zn_splat(P8::from_raw(0i32)), n1917);
    let n1930: ZB = zb_or(n1926, n1927);
    let n1931: ZB = zn_gt(n1929, zn_splat(P8::from_raw(0i32)));
    let n1932: ZB = zn_le(n1929, zn_splat(P8::from_raw(0i32)));
    let n1933: ZB = zb_and(n1930, n1931);
    let n1934: ZB = zb_and(n1930, n1932);
    let n1935: ZB = zn_gt(n273, n1834);
    let n1936: ZB = zb_or(n1933, n1934);
    let n1937: ZB = zb_and(n1931, n1935);
    let n1938: ZB = zb_not(n1937);
    let n1939: ZB = zb_and(n1936, n1937);
    let n1940: ZB = zb_and(n1936, n1938);
    let n1941: ZN = zsel_n(n1937, zn_splat(P8::from_raw(327680i32)), n1928);
    let n1942: ZN = zsel_n(n1937, zn_splat(P8::from_raw(131072i32)), n86);
    let n1943: ZN = zsel_n(n1937, n1834, n273);
    let n1944: ZN = zsel_n(n1937, zn_splat(P8::from_raw(0i32)), n1905);
    let n1945: ZN = zsel_n(n1937, zn_splat(P8::from_raw(0i32)), n1929);
    let n1946: ZB = zb_or(n1939, n1940);
    let n1947: ZB = zb_not(n282);
    let n1948: ZB = zb_and(n280, n1947);
    let n1949: ZB = zn_ge(n281, zn_splat(P8::from_raw(0i32)));
    let n1950: ZB = zb_and(n283, n1949);
    let n1951: ZN = zsel_n(n282, n281, n84);
    let n1952: ZN = zsel_n(n282, zn_splat(P8::from_raw(393216i32)), n1830);
    let n1953: ZB = zb_or(n1948, n1950);
    let n1954: ZN = zsel_n(n278, n1941, n1951);
    let n1955: ZN = zsel_n(n278, n1830, n1952);
    let n1956: ZN = zsel_n(n278, n1942, n86);
    let n1957: ZN = zsel_n(n278, n1943, n273);
    let n1958: ZN = zsel_n(n278, n1944, n1905);
    let n1959: ZN = zsel_n(n278, n1945, n1906);
    let n1960: ZB = zb_or(n1946, n1953);
    let n1961: ZN = zsel_n(n275, n1913, n1954);
    let n1962: ZN = zsel_n(n275, n1830, n1955);
    let n1963: ZN = zsel_n(n275, n1914, n1956);
    let n1964: ZN = zsel_n(n275, n273, n1957);
    let n1965: ZN = zsel_n(n275, n1905, n1958);
    let n1966: ZN = zsel_n(n275, n1906, n1959);
    let n1967: ZB = zb_or(n1915, n1960);
    let n1968: ZN = zsel_n(n1020, r_c39, n1243);
    let n1969: ZN = zsel_n(n1020, n1822, r_c20);
    let n1970: ZB = zsel_b(n1020, r_c41, n82);
    let n1971: ZB = zsel_b(n1020, r_c42, n83);
    let n1972: ZB = zsel_b(n1020, r_c38, n1244);
    let n1973: ZB = zsel_b(n1020, r_c232, n1829);
    let n1974: ZN = zsel_n(n1020, r_c233, n1961);
    let n1975: ZB = zsel_b(n1020, r_c242, n85);
    let n1976: ZN = zsel_n(n1020, r_c244, n1962);
    let n1977: ZN = zsel_n(n1020, r_c245, n1963);
    let n1978: ZN = zsel_n(n1020, r_c248, n272);
    let n1979: ZN = zsel_n(n1020, r_c249, n1964);
    let n1980: ZB = zsel_b(n1020, r_c262, n1831);
    let n1981: ZB = zsel_b(n1020, r_c263, n1832);
    let n1982: ZN = zsel_n(n1020, zn_splat(u.c264), n89);
    let n1983: ZN = zsel_n(n1020, zn_splat(u.c265), n90);
    let n1984: ZN = zsel_n(n1020, zn_splat(u.c266), n91);
    let n1985: ZN = zsel_n(n1020, zn_splat(u.c267), n92);
    let n1986: ZN = zsel_n(n1020, r_c268, n1903);
    let n1987: ZN = zsel_n(n1020, r_c269, n1904);
    let n1988: ZN = zsel_n(n1020, r_c270, n1965);
    let n1989: ZN = zsel_n(n1020, r_c271, n1966);
    let n1990: ZN = zsel_n(n1020, r_c272, n1833);
    let n1991: ZN = zsel_n(n1020, r_c273, n1834);
    let n1992: ZB = zb_or(n1020, n1967);
    let n1993: ZB = zb_or(n274, n1020);
    let n1994: ZB = zn_gt(n1969, zn_splat(P8::from_raw(0i32)));
    let n1995: ZB = zn_le(n1969, zn_splat(P8::from_raw(0i32)));
    let n1996: ZB = zb_and(n1992, n1994);
    let n1997: ZB = zb_and(n1992, n1995);
    let n1998: ZB = zb_or(n1996, n1997);
    let n2001: ZW = zw_bits_b(n83);
    let n2002: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2001, 42u64);
    let n2003: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2001, 42u64);
    let n2004: ZW = zw_bits_b(r_c43);
    let n2005: ZW = zw_mix1(n2002, n2004, 43u64);
    let n2006: ZW = zw_mix2(n2003, n2004, 43u64);
    let n2007: ZW = zw_bits_n(n51);
    let n2008: ZW = zw_mix1(n2005, n2007, 84u64);
    let n2009: ZW = zw_mix2(n2006, n2007, 84u64);
    let n2010: ZW = zw_bits_n(n60);
    let n2011: ZW = zw_mix1(n2008, n2010, 85u64);
    let n2012: ZW = zw_mix2(n2009, n2010, 85u64);
    let n2013: ZW = zw_bits_n(n59);
    let n2014: ZW = zw_mix1(n2011, n2013, 86u64);
    let n2015: ZW = zw_mix2(n2012, n2013, 86u64);
    let n2016: ZW = zw_bits_n(n961);
    let n2017: ZW = zw_mix1(n2014, n2016, 87u64);
    let n2018: ZW = zw_mix2(n2015, n2016, 87u64);
    let n2019: ZW = zw_bits_n(r_c88);
    let n2020: ZW = zw_mix1(n2017, n2019, 88u64);
    let n2021: ZW = zw_mix2(n2018, n2019, 88u64);
    let n2022: ZW = zw_bits_n(r_c20);
    let n2023: ZW = zw_mix1(n2020, n2022, 20u64);
    let n2024: ZW = zw_mix2(n2021, n2022, 20u64);
    let n2025: ZW = zw_bits_b(n82);
    let n2026: ZW = zw_mix1(n2023, n2025, 41u64);
    let n2027: ZW = zw_mix2(n2024, n2025, 41u64);
    let n2028: ZW = zw_bits_n(n1174);
    let n2029: ZW = zw_mix1(n2020, n2028, 20u64);
    let n2030: ZW = zw_mix2(n2021, n2028, 20u64);
    let n2031: ZW = zw_bits_b(n1175);
    let n2032: ZW = zw_mix1(n2029, n2031, 41u64);
    let n2033: ZW = zw_mix2(n2030, n2031, 41u64);
    let n2034: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2004, 43u64);
    let n2035: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2004, 43u64);
    let n2036: ZW = zw_mix1(n2034, n2007, 84u64);
    let n2037: ZW = zw_mix2(n2035, n2007, 84u64);
    let n2038: ZW = zw_mix1(n2036, n2010, 85u64);
    let n2039: ZW = zw_mix2(n2037, n2010, 85u64);
    let n2040: ZW = zw_mix1(n2038, n2013, 86u64);
    let n2041: ZW = zw_mix2(n2039, n2013, 86u64);
    let n2042: ZW = zw_mix1(n2040, n2019, 88u64);
    let n2043: ZW = zw_mix2(n2041, n2019, 88u64);
    let n2044: ZW = zw_mix1(n2042, n2022, 20u64);
    let n2045: ZW = zw_mix2(n2043, n2022, 20u64);
    let n2046: ZW = zw_bits_b(n1275);
    let n2047: ZW = zw_mix1(n2044, n2046, 38u64);
    let n2048: ZW = zw_mix2(n2045, n2046, 38u64);
    let n2049: ZW = zw_bits_n(n1273);
    let n2050: ZW = zw_mix1(n2047, n2049, 39u64);
    let n2051: ZW = zw_mix2(n2048, n2049, 39u64);
    let n2052: ZW = zw_bits_n(n1272);
    let n2053: ZW = zw_mix1(n2050, n2052, 87u64);
    let n2054: ZW = zw_mix2(n2051, n2052, 87u64);
    let n2055: ZW = zw_bits_b(n1307);
    let n2056: ZW = zw_mix1(n2044, n2055, 38u64);
    let n2057: ZW = zw_mix2(n2045, n2055, 38u64);
    let n2058: ZW = zw_bits_n(n1305);
    let n2059: ZW = zw_mix1(n2056, n2058, 39u64);
    let n2060: ZW = zw_mix2(n2057, n2058, 39u64);
    let n2061: ZW = zw_bits_n(n1304);
    let n2062: ZW = zw_mix1(n2059, n2061, 87u64);
    let n2063: ZW = zw_mix2(n2060, n2061, 87u64);
    let n2064: ZW = zw_bits_b(n1337);
    let n2065: ZW = zw_mix1(n2044, n2064, 38u64);
    let n2066: ZW = zw_mix2(n2045, n2064, 38u64);
    let n2067: ZW = zw_bits_n(n1335);
    let n2068: ZW = zw_mix1(n2065, n2067, 39u64);
    let n2069: ZW = zw_mix2(n2066, n2067, 39u64);
    let n2070: ZW = zw_bits_n(n1334);
    let n2071: ZW = zw_mix1(n2068, n2070, 87u64);
    let n2072: ZW = zw_mix2(n2069, n2070, 87u64);
    let n2073: ZW = zw_bits_b(n1376);
    let n2074: ZW = zw_mix1(n2044, n2073, 38u64);
    let n2075: ZW = zw_mix2(n2045, n2073, 38u64);
    let n2076: ZW = zw_bits_n(n1374);
    let n2077: ZW = zw_mix1(n2074, n2076, 39u64);
    let n2078: ZW = zw_mix2(n2075, n2076, 39u64);
    let n2079: ZW = zw_bits_n(n1373);
    let n2080: ZW = zw_mix1(n2077, n2079, 87u64);
    let n2081: ZW = zw_mix2(n2078, n2079, 87u64);
    let n2082: ZW = zw_bits_b(n1415);
    let n2083: ZW = zw_mix1(n2044, n2082, 38u64);
    let n2084: ZW = zw_mix2(n2045, n2082, 38u64);
    let n2085: ZW = zw_bits_n(n1413);
    let n2086: ZW = zw_mix1(n2083, n2085, 39u64);
    let n2087: ZW = zw_mix2(n2084, n2085, 39u64);
    let n2088: ZW = zw_bits_n(n1412);
    let n2089: ZW = zw_mix1(n2086, n2088, 87u64);
    let n2090: ZW = zw_mix2(n2087, n2088, 87u64);
    let n2091: ZW = zw_bits_b(n1454);
    let n2092: ZW = zw_mix1(n2044, n2091, 38u64);
    let n2093: ZW = zw_mix2(n2045, n2091, 38u64);
    let n2094: ZW = zw_bits_n(n1452);
    let n2095: ZW = zw_mix1(n2092, n2094, 39u64);
    let n2096: ZW = zw_mix2(n2093, n2094, 39u64);
    let n2097: ZW = zw_bits_n(n1451);
    let n2098: ZW = zw_mix1(n2095, n2097, 87u64);
    let n2099: ZW = zw_mix2(n2096, n2097, 87u64);
    let n2100: ZW = zw_mix1(n2042, n2028, 20u64);
    let n2101: ZW = zw_mix2(n2043, n2028, 20u64);
    let n2102: ZW = zw_bits_b(n1469);
    let n2103: ZW = zw_mix1(n2100, n2102, 38u64);
    let n2104: ZW = zw_mix2(n2101, n2102, 38u64);
    let n2105: ZW = zw_bits_n(n1467);
    let n2106: ZW = zw_mix1(n2103, n2105, 39u64);
    let n2107: ZW = zw_mix2(n2104, n2105, 39u64);
    let n2108: ZW = zw_bits_n(n1466);
    let n2109: ZW = zw_mix1(n2106, n2108, 87u64);
    let n2110: ZW = zw_mix2(n2107, n2108, 87u64);
    let n2111: ZW = zw_bits_b(n1486);
    let n2112: ZW = zw_mix1(n2100, n2111, 38u64);
    let n2113: ZW = zw_mix2(n2101, n2111, 38u64);
    let n2114: ZW = zw_bits_n(n1484);
    let n2115: ZW = zw_mix1(n2112, n2114, 39u64);
    let n2116: ZW = zw_mix2(n2113, n2114, 39u64);
    let n2117: ZW = zw_bits_n(n1483);
    let n2118: ZW = zw_mix1(n2115, n2117, 87u64);
    let n2119: ZW = zw_mix2(n2116, n2117, 87u64);
    let n2120: ZW = zw_bits_b(n1503);
    let n2121: ZW = zw_mix1(n2100, n2120, 38u64);
    let n2122: ZW = zw_mix2(n2101, n2120, 38u64);
    let n2123: ZW = zw_bits_n(n1501);
    let n2124: ZW = zw_mix1(n2121, n2123, 39u64);
    let n2125: ZW = zw_mix2(n2122, n2123, 39u64);
    let n2126: ZW = zw_bits_n(n1500);
    let n2127: ZW = zw_mix1(n2124, n2126, 87u64);
    let n2128: ZW = zw_mix2(n2125, n2126, 87u64);
    let n2129: ZW = zw_bits_b(n1520);
    let n2130: ZW = zw_mix1(n2100, n2129, 38u64);
    let n2131: ZW = zw_mix2(n2101, n2129, 38u64);
    let n2132: ZW = zw_bits_n(n1518);
    let n2133: ZW = zw_mix1(n2130, n2132, 39u64);
    let n2134: ZW = zw_mix2(n2131, n2132, 39u64);
    let n2135: ZW = zw_bits_n(n1517);
    let n2136: ZW = zw_mix1(n2133, n2135, 87u64);
    let n2137: ZW = zw_mix2(n2134, n2135, 87u64);
    let n2138: ZW = zw_bits_b(n1537);
    let n2139: ZW = zw_mix1(n2100, n2138, 38u64);
    let n2140: ZW = zw_mix2(n2101, n2138, 38u64);
    let n2141: ZW = zw_bits_n(n1535);
    let n2142: ZW = zw_mix1(n2139, n2141, 39u64);
    let n2143: ZW = zw_mix2(n2140, n2141, 39u64);
    let n2144: ZW = zw_bits_n(n1534);
    let n2145: ZW = zw_mix1(n2142, n2144, 87u64);
    let n2146: ZW = zw_mix2(n2143, n2144, 87u64);
    let n2147: ZW = zw_bits_b(n1554);
    let n2148: ZW = zw_mix1(n2100, n2147, 38u64);
    let n2149: ZW = zw_mix2(n2101, n2147, 38u64);
    let n2150: ZW = zw_bits_n(n1552);
    let n2151: ZW = zw_mix1(n2148, n2150, 39u64);
    let n2152: ZW = zw_mix2(n2149, n2150, 39u64);
    let n2153: ZW = zw_bits_n(n1551);
    let n2154: ZW = zw_mix1(n2151, n2153, 87u64);
    let n2155: ZW = zw_mix2(n2152, n2153, 87u64);
    let n2156: ZW = zw_bits_b(n1244);
    let n2157: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2156, 38u64);
    let n2158: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2156, 38u64);
    let n2159: ZW = zw_bits_n(n1243);
    let n2160: ZW = zw_mix1(n2157, n2159, 39u64);
    let n2161: ZW = zw_mix2(n2158, n2159, 39u64);
    let n2162: ZW = zw_mix1(n2160, n2001, 42u64);
    let n2163: ZW = zw_mix2(n2161, n2001, 42u64);
    let n2164: ZW = zw_mix1(n2162, n2004, 43u64);
    let n2165: ZW = zw_mix2(n2163, n2004, 43u64);
    let n2166: ZW = zw_mix1(n2164, n2007, 84u64);
    let n2167: ZW = zw_mix2(n2165, n2007, 84u64);
    let n2168: ZW = zw_mix1(n2166, n2010, 85u64);
    let n2169: ZW = zw_mix2(n2167, n2010, 85u64);
    let n2170: ZW = zw_mix1(n2168, n2013, 86u64);
    let n2171: ZW = zw_mix2(n2169, n2013, 86u64);
    let n2172: ZW = zw_bits_n(r_c87);
    let n2173: ZW = zw_mix1(n2170, n2172, 87u64);
    let n2174: ZW = zw_mix2(n2171, n2172, 87u64);
    let n2175: ZW = zw_mix1(n2173, n2019, 88u64);
    let n2176: ZW = zw_mix2(n2174, n2019, 88u64);
    let n2177: ZW = zw_bits_n(n273);
    let n2178: ZW = zw_mix1(n2175, n2177, 254u64);
    let n2179: ZW = zw_mix2(n2176, n2177, 254u64);
    let n2180: ZW = zw_mix1(n2178, n2022, 20u64);
    let n2181: ZW = zw_mix2(n2179, n2022, 20u64);
    let n2182: ZW = zw_mix1(n2180, n2025, 41u64);
    let n2183: ZW = zw_mix2(n2181, n2025, 41u64);
    let n2184: ZW = zw_bits_n(n1576);
    let n2185: ZW = zw_mix1(n2182, n2184, 234u64);
    let n2186: ZW = zw_mix2(n2183, n2184, 234u64);
    let n2187: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n2188: ZW = zw_mix1(n2185, zw_splat(n2187), 236u64);
    let n2189: ZW = zw_mix2(n2186, zw_splat(n2187), 236u64);
    let n2190: ZW = zw_mix1(n2188, n2019, 237u64);
    let n2191: ZW = zw_mix2(n2189, n2019, 237u64);
    let n2192: ZW = zw_bits_n(n1577);
    let n2193: ZW = zw_mix1(n2190, n2192, 239u64);
    let n2194: ZW = zw_mix2(n2191, n2192, 239u64);
    let n2195: u64 = false as u64;
    let n2196: ZW = zw_mix1(n2193, zw_splat(n2195), 246u64);
    let n2197: ZW = zw_mix2(n2194, zw_splat(n2195), 246u64);
    let n2198: ZW = zw_mix1(n2196, zw_splat(n2195), 247u64);
    let n2199: ZW = zw_mix2(n2197, zw_splat(n2195), 247u64);
    let n2200: ZW = zw_bits_n(n1597);
    let n2201: ZW = zw_mix1(n2198, n2200, 253u64);
    let n2202: ZW = zw_mix2(n2199, n2200, 253u64);
    let n2203: ZW = zw_mix1(n2201, zw_splat(n2187), 268u64);
    let n2204: ZW = zw_mix2(n2202, zw_splat(n2187), 268u64);
    let n2205: ZW = zw_mix1(n2203, zw_splat(n2187), 269u64);
    let n2206: ZW = zw_mix2(n2204, zw_splat(n2187), 269u64);
    let n2207: ZW = zw_mix1(n2205, zw_splat(n2187), 270u64);
    let n2208: ZW = zw_mix2(n2206, zw_splat(n2187), 270u64);
    let n2209: ZW = zw_mix1(n2207, zw_splat(n2187), 271u64);
    let n2210: ZW = zw_mix2(n2208, zw_splat(n2187), 271u64);
    let n2211: ZW = zw_mix1(n2209, zw_splat(n2195), 272u64);
    let n2212: ZW = zw_mix2(n2210, zw_splat(n2195), 272u64);
    let n2213: ZW = zw_mix1(n2211, zw_splat(n2187), 280u64);
    let n2214: ZW = zw_mix2(n2212, zw_splat(n2187), 280u64);
    let n2215: ZW = zw_bits_n(n1578);
    let n2216: ZW = zw_mix1(n2213, n2215, 281u64);
    let n2217: ZW = zw_mix2(n2214, n2215, 281u64);
    let n2218: ZW = zw_bits_b(n286);
    let n2219: ZW = zw_mix1(n2209, n2218, 272u64);
    let n2220: ZW = zw_mix2(n2210, n2218, 272u64);
    let n2221: ZW = zw_bits_n(n1611);
    let n2222: ZW = zw_mix1(n2219, n2221, 280u64);
    let n2223: ZW = zw_mix2(n2220, n2221, 280u64);
    let n2224: ZW = zw_mix1(n2222, n2215, 281u64);
    let n2225: ZW = zw_mix2(n2223, n2215, 281u64);
    let n2226: ZW = zw_bits_n(n1625);
    let n2227: ZW = zw_mix1(n2211, n2226, 280u64);
    let n2228: ZW = zw_mix2(n2212, n2226, 280u64);
    let n2229: ZW = zw_mix1(n2227, n2215, 281u64);
    let n2230: ZW = zw_mix2(n2228, n2215, 281u64);
    let n2231: ZW = zw_bits_n(n1627);
    let n2232: ZW = zw_mix1(n2190, n2231, 239u64);
    let n2233: ZW = zw_mix2(n2191, n2231, 239u64);
    let n2234: ZW = zw_mix1(n2232, zw_splat(n2195), 246u64);
    let n2235: ZW = zw_mix2(n2233, zw_splat(n2195), 246u64);
    let n2236: ZW = zw_mix1(n2234, n2218, 247u64);
    let n2237: ZW = zw_mix2(n2235, n2218, 247u64);
    let n2238: ZW = zw_mix1(n2236, n2200, 253u64);
    let n2239: ZW = zw_mix2(n2237, n2200, 253u64);
    let n2240: ZW = zw_mix1(n2238, zw_splat(n2187), 268u64);
    let n2241: ZW = zw_mix2(n2239, zw_splat(n2187), 268u64);
    let n2242: ZW = zw_mix1(n2240, zw_splat(n2187), 269u64);
    let n2243: ZW = zw_mix2(n2241, zw_splat(n2187), 269u64);
    let n2244: ZW = zw_mix1(n2242, zw_splat(n2187), 270u64);
    let n2245: ZW = zw_mix2(n2243, zw_splat(n2187), 270u64);
    let n2246: ZW = zw_mix1(n2244, zw_splat(n2187), 271u64);
    let n2247: ZW = zw_mix2(n2245, zw_splat(n2187), 271u64);
    let n2248: ZW = zw_mix1(n2246, zw_splat(n2195), 272u64);
    let n2249: ZW = zw_mix2(n2247, zw_splat(n2195), 272u64);
    let n2250: ZW = zw_bits_n(n1639);
    let n2251: ZW = zw_mix1(n2248, n2250, 280u64);
    let n2252: ZW = zw_mix2(n2249, n2250, 280u64);
    let n2253: ZW = zw_bits_n(n1629);
    let n2254: ZW = zw_mix1(n2251, n2253, 281u64);
    let n2255: ZW = zw_mix2(n2252, n2253, 281u64);
    let n2256: ZW = zw_mix1(n2246, n2218, 272u64);
    let n2257: ZW = zw_mix2(n2247, n2218, 272u64);
    let n2258: ZW = zw_bits_n(n1651);
    let n2259: ZW = zw_mix1(n2256, n2258, 280u64);
    let n2260: ZW = zw_mix2(n2257, n2258, 280u64);
    let n2261: ZW = zw_mix1(n2259, n2253, 281u64);
    let n2262: ZW = zw_mix2(n2260, n2253, 281u64);
    let n2263: ZW = zw_bits_n(n1663);
    let n2264: ZW = zw_mix1(n2248, n2263, 280u64);
    let n2265: ZW = zw_mix2(n2249, n2263, 280u64);
    let n2266: ZW = zw_mix1(n2264, n2253, 281u64);
    let n2267: ZW = zw_mix2(n2265, n2253, 281u64);
    let n2268: ZW = zw_bits_n(n1673);
    let n2269: ZW = zw_mix1(n2178, n2268, 20u64);
    let n2270: ZW = zw_mix2(n2179, n2268, 20u64);
    let n2271: ZW = zw_bits_b(n1674);
    let n2272: ZW = zw_mix1(n2269, n2271, 41u64);
    let n2273: ZW = zw_mix2(n2270, n2271, 41u64);
    let n2274: ZW = zw_bits_n(n1675);
    let n2275: ZW = zw_mix1(n2272, n2274, 234u64);
    let n2276: ZW = zw_mix2(n2273, n2274, 234u64);
    let n2277: ZW = zw_bits_n(n1676);
    let n2278: ZW = zw_mix1(n2275, n2277, 236u64);
    let n2279: ZW = zw_mix2(n2276, n2277, 236u64);
    let n2280: ZW = zw_bits_n(n1677);
    let n2281: ZW = zw_mix1(n2278, n2280, 237u64);
    let n2282: ZW = zw_mix2(n2279, n2280, 237u64);
    let n2283: ZW = zw_mix1(n2281, n2192, 239u64);
    let n2284: ZW = zw_mix2(n2282, n2192, 239u64);
    let n2285: ZW = zw_mix1(n2283, n2218, 246u64);
    let n2286: ZW = zw_mix2(n2284, n2218, 246u64);
    let n2287: ZW = zw_mix1(n2285, zw_splat(n2195), 247u64);
    let n2288: ZW = zw_mix2(n2286, zw_splat(n2195), 247u64);
    let n2289: ZW = zw_bits_n(n1695);
    let n2290: ZW = zw_mix1(n2287, n2289, 253u64);
    let n2291: ZW = zw_mix2(n2288, n2289, 253u64);
    let n2292: ZW = zw_bits_n(n1678);
    let n2293: ZW = zw_mix1(n2290, n2292, 268u64);
    let n2294: ZW = zw_mix2(n2291, n2292, 268u64);
    let n2295: ZW = zw_bits_n(n1679);
    let n2296: ZW = zw_mix1(n2293, n2295, 269u64);
    let n2297: ZW = zw_mix2(n2294, n2295, 269u64);
    let n2298: ZW = zw_bits_n(n1680);
    let n2299: ZW = zw_mix1(n2296, n2298, 270u64);
    let n2300: ZW = zw_mix2(n2297, n2298, 270u64);
    let n2301: ZW = zw_mix1(n2299, zw_splat(n2187), 271u64);
    let n2302: ZW = zw_mix2(n2300, zw_splat(n2187), 271u64);
    let n2303: ZW = zw_mix1(n2301, zw_splat(n2195), 272u64);
    let n2304: ZW = zw_mix2(n2302, zw_splat(n2195), 272u64);
    let n2305: ZW = zw_bits_n(n1696);
    let n2306: ZW = zw_mix1(n2303, n2305, 280u64);
    let n2307: ZW = zw_mix2(n2304, n2305, 280u64);
    let n2308: ZW = zw_bits_n(n1682);
    let n2309: ZW = zw_mix1(n2306, n2308, 281u64);
    let n2310: ZW = zw_mix2(n2307, n2308, 281u64);
    let n2311: ZW = zw_bits_n(n1701);
    let n2312: ZW = zw_mix1(n2296, n2311, 270u64);
    let n2313: ZW = zw_mix2(n2297, n2311, 270u64);
    let n2314: ZW = zw_mix1(n2312, zw_splat(n2187), 271u64);
    let n2315: ZW = zw_mix2(n2313, zw_splat(n2187), 271u64);
    let n2316: ZW = zw_mix1(n2314, n2218, 272u64);
    let n2317: ZW = zw_mix2(n2315, n2218, 272u64);
    let n2318: ZW = zw_bits_n(n1713);
    let n2319: ZW = zw_mix1(n2316, n2318, 280u64);
    let n2320: ZW = zw_mix2(n2317, n2318, 280u64);
    let n2321: ZW = zw_mix1(n2319, n2308, 281u64);
    let n2322: ZW = zw_mix2(n2320, n2308, 281u64);
    let n2323: ZW = zw_bits_n(n1728);
    let n2324: ZW = zw_mix1(n2303, n2323, 280u64);
    let n2325: ZW = zw_mix2(n2304, n2323, 280u64);
    let n2326: ZW = zw_mix1(n2324, n2308, 281u64);
    let n2327: ZW = zw_mix2(n2325, n2308, 281u64);
    let n2328: ZW = zw_mix1(n2290, n2295, 268u64);
    let n2329: ZW = zw_mix2(n2291, n2295, 268u64);
    let n2330: ZW = zw_mix1(n2328, n2292, 269u64);
    let n2331: ZW = zw_mix2(n2329, n2292, 269u64);
    let n2332: ZW = zw_mix1(n2330, zw_splat(n2187), 270u64);
    let n2333: ZW = zw_mix2(n2331, zw_splat(n2187), 270u64);
    let n2334: ZW = zw_bits_n(n1733);
    let n2335: ZW = zw_mix1(n2332, n2334, 271u64);
    let n2336: ZW = zw_mix2(n2333, n2334, 271u64);
    let n2337: ZW = zw_mix1(n2335, zw_splat(n2195), 272u64);
    let n2338: ZW = zw_mix2(n2336, zw_splat(n2195), 272u64);
    let n2339: ZW = zw_mix1(n2337, zw_splat(n2187), 280u64);
    let n2340: ZW = zw_mix2(n2338, zw_splat(n2187), 280u64);
    let n2341: ZW = zw_bits_n(n1734);
    let n2342: ZW = zw_mix1(n2339, n2341, 281u64);
    let n2343: ZW = zw_mix2(n2340, n2341, 281u64);
    let n2344: ZW = zw_mix1(n2328, n2295, 269u64);
    let n2345: ZW = zw_mix2(n2329, n2295, 269u64);
    let n2346: ZW = zw_mix1(n2344, n2311, 270u64);
    let n2347: ZW = zw_mix2(n2345, n2311, 270u64);
    let n2348: ZW = zw_mix1(n2346, n2334, 271u64);
    let n2349: ZW = zw_mix2(n2347, n2334, 271u64);
    let n2350: ZW = zw_mix1(n2348, n2218, 272u64);
    let n2351: ZW = zw_mix2(n2349, n2218, 272u64);
    let n2352: ZW = zw_bits_n(n1740);
    let n2353: ZW = zw_mix1(n2350, n2352, 280u64);
    let n2354: ZW = zw_mix2(n2351, n2352, 280u64);
    let n2355: ZW = zw_bits_n(n1738);
    let n2356: ZW = zw_mix1(n2353, n2355, 281u64);
    let n2357: ZW = zw_mix2(n2354, n2355, 281u64);
    let n2358: ZW = zw_mix1(n2344, n2298, 270u64);
    let n2359: ZW = zw_mix2(n2345, n2298, 270u64);
    let n2360: ZW = zw_mix1(n2358, n2334, 271u64);
    let n2361: ZW = zw_mix2(n2359, n2334, 271u64);
    let n2362: ZW = zw_mix1(n2360, zw_splat(n2195), 272u64);
    let n2363: ZW = zw_mix2(n2361, zw_splat(n2195), 272u64);
    let n2364: ZW = zw_bits_n(n1744);
    let n2365: ZW = zw_mix1(n2362, n2364, 280u64);
    let n2366: ZW = zw_mix2(n2363, n2364, 280u64);
    let n2367: ZW = zw_mix1(n2365, n2355, 281u64);
    let n2368: ZW = zw_mix2(n2366, n2355, 281u64);
    let n2369: ZW = zw_mix1(n2332, n2298, 271u64);
    let n2370: ZW = zw_mix2(n2333, n2298, 271u64);
    let n2371: ZW = zw_mix1(n2369, zw_splat(n2195), 272u64);
    let n2372: ZW = zw_mix2(n2370, zw_splat(n2195), 272u64);
    let n2373: ZW = zw_mix1(n2371, zw_splat(n2187), 280u64);
    let n2374: ZW = zw_mix2(n2372, zw_splat(n2187), 280u64);
    let n2375: ZW = zw_bits_n(n1746);
    let n2376: ZW = zw_mix1(n2373, n2375, 281u64);
    let n2377: ZW = zw_mix2(n2374, n2375, 281u64);
    let n2378: ZW = zw_mix1(n2346, n2298, 271u64);
    let n2379: ZW = zw_mix2(n2347, n2298, 271u64);
    let n2380: ZW = zw_mix1(n2378, n2218, 272u64);
    let n2381: ZW = zw_mix2(n2379, n2218, 272u64);
    let n2382: ZW = zw_mix1(n2380, n2352, 280u64);
    let n2383: ZW = zw_mix2(n2381, n2352, 280u64);
    let n2384: ZW = zw_bits_n(n1748);
    let n2385: ZW = zw_mix1(n2382, n2384, 281u64);
    let n2386: ZW = zw_mix2(n2383, n2384, 281u64);
    let n2387: ZW = zw_mix1(n2358, n2298, 271u64);
    let n2388: ZW = zw_mix2(n2359, n2298, 271u64);
    let n2389: ZW = zw_mix1(n2387, zw_splat(n2195), 272u64);
    let n2390: ZW = zw_mix2(n2388, zw_splat(n2195), 272u64);
    let n2391: ZW = zw_mix1(n2389, n2364, 280u64);
    let n2392: ZW = zw_mix2(n2390, n2364, 280u64);
    let n2393: ZW = zw_mix1(n2391, n2384, 281u64);
    let n2394: ZW = zw_mix2(n2392, n2384, 281u64);
    let n2395: ZW = zw_mix1(n2281, n2231, 239u64);
    let n2396: ZW = zw_mix2(n2282, n2231, 239u64);
    let n2397: ZW = zw_mix1(n2395, n2218, 246u64);
    let n2398: ZW = zw_mix2(n2396, n2218, 246u64);
    let n2399: ZW = zw_mix1(n2397, n2218, 247u64);
    let n2400: ZW = zw_mix2(n2398, n2218, 247u64);
    let n2401: ZW = zw_mix1(n2399, n2289, 253u64);
    let n2402: ZW = zw_mix2(n2400, n2289, 253u64);
    let n2403: ZW = zw_mix1(n2401, n2292, 268u64);
    let n2404: ZW = zw_mix2(n2402, n2292, 268u64);
    let n2405: ZW = zw_mix1(n2403, n2295, 269u64);
    let n2406: ZW = zw_mix2(n2404, n2295, 269u64);
    let n2407: ZW = zw_mix1(n2405, n2298, 270u64);
    let n2408: ZW = zw_mix2(n2406, n2298, 270u64);
    let n2409: ZW = zw_mix1(n2407, zw_splat(n2187), 271u64);
    let n2410: ZW = zw_mix2(n2408, zw_splat(n2187), 271u64);
    let n2411: ZW = zw_mix1(n2409, zw_splat(n2195), 272u64);
    let n2412: ZW = zw_mix2(n2410, zw_splat(n2195), 272u64);
    let n2413: ZW = zw_bits_n(n1764);
    let n2414: ZW = zw_mix1(n2411, n2413, 280u64);
    let n2415: ZW = zw_mix2(n2412, n2413, 280u64);
    let n2416: ZW = zw_bits_n(n1753);
    let n2417: ZW = zw_mix1(n2414, n2416, 281u64);
    let n2418: ZW = zw_mix2(n2415, n2416, 281u64);
    let n2419: ZW = zw_mix1(n2405, n2311, 270u64);
    let n2420: ZW = zw_mix2(n2406, n2311, 270u64);
    let n2421: ZW = zw_mix1(n2419, zw_splat(n2187), 271u64);
    let n2422: ZW = zw_mix2(n2420, zw_splat(n2187), 271u64);
    let n2423: ZW = zw_mix1(n2421, n2218, 272u64);
    let n2424: ZW = zw_mix2(n2422, n2218, 272u64);
    let n2425: ZW = zw_bits_n(n1779);
    let n2426: ZW = zw_mix1(n2423, n2425, 280u64);
    let n2427: ZW = zw_mix2(n2424, n2425, 280u64);
    let n2428: ZW = zw_mix1(n2426, n2416, 281u64);
    let n2429: ZW = zw_mix2(n2427, n2416, 281u64);
    let n2430: ZW = zw_bits_n(n1794);
    let n2431: ZW = zw_mix1(n2411, n2430, 280u64);
    let n2432: ZW = zw_mix2(n2412, n2430, 280u64);
    let n2433: ZW = zw_mix1(n2431, n2416, 281u64);
    let n2434: ZW = zw_mix2(n2432, n2416, 281u64);
    let n2435: ZW = zw_mix1(n2401, n2295, 268u64);
    let n2436: ZW = zw_mix2(n2402, n2295, 268u64);
    let n2437: ZW = zw_mix1(n2435, n2292, 269u64);
    let n2438: ZW = zw_mix2(n2436, n2292, 269u64);
    let n2439: ZW = zw_mix1(n2437, zw_splat(n2187), 270u64);
    let n2440: ZW = zw_mix2(n2438, zw_splat(n2187), 270u64);
    let n2441: ZW = zw_mix1(n2439, n2334, 271u64);
    let n2442: ZW = zw_mix2(n2440, n2334, 271u64);
    let n2443: ZW = zw_mix1(n2441, zw_splat(n2195), 272u64);
    let n2444: ZW = zw_mix2(n2442, zw_splat(n2195), 272u64);
    let n2445: ZW = zw_bits_n(n1801);
    let n2446: ZW = zw_mix1(n2443, n2445, 280u64);
    let n2447: ZW = zw_mix2(n2444, n2445, 280u64);
    let n2448: ZW = zw_bits_n(n1799);
    let n2449: ZW = zw_mix1(n2446, n2448, 281u64);
    let n2450: ZW = zw_mix2(n2447, n2448, 281u64);
    let n2451: ZW = zw_mix1(n2435, n2295, 269u64);
    let n2452: ZW = zw_mix2(n2436, n2295, 269u64);
    let n2453: ZW = zw_mix1(n2451, n2311, 270u64);
    let n2454: ZW = zw_mix2(n2452, n2311, 270u64);
    let n2455: ZW = zw_mix1(n2453, n2334, 271u64);
    let n2456: ZW = zw_mix2(n2454, n2334, 271u64);
    let n2457: ZW = zw_mix1(n2455, n2218, 272u64);
    let n2458: ZW = zw_mix2(n2456, n2218, 272u64);
    let n2459: ZW = zw_bits_n(n1807);
    let n2460: ZW = zw_mix1(n2457, n2459, 280u64);
    let n2461: ZW = zw_mix2(n2458, n2459, 280u64);
    let n2462: ZW = zw_bits_n(n1805);
    let n2463: ZW = zw_mix1(n2460, n2462, 281u64);
    let n2464: ZW = zw_mix2(n2461, n2462, 281u64);
    let n2465: ZW = zw_mix1(n2451, n2298, 270u64);
    let n2466: ZW = zw_mix2(n2452, n2298, 270u64);
    let n2467: ZW = zw_mix1(n2465, n2334, 271u64);
    let n2468: ZW = zw_mix2(n2466, n2334, 271u64);
    let n2469: ZW = zw_mix1(n2467, zw_splat(n2195), 272u64);
    let n2470: ZW = zw_mix2(n2468, zw_splat(n2195), 272u64);
    let n2471: ZW = zw_bits_n(n1811);
    let n2472: ZW = zw_mix1(n2469, n2471, 280u64);
    let n2473: ZW = zw_mix2(n2470, n2471, 280u64);
    let n2474: ZW = zw_mix1(n2472, n2462, 281u64);
    let n2475: ZW = zw_mix2(n2473, n2462, 281u64);
    let n2476: ZW = zw_mix1(n2439, n2298, 271u64);
    let n2477: ZW = zw_mix2(n2440, n2298, 271u64);
    let n2478: ZW = zw_mix1(n2476, zw_splat(n2195), 272u64);
    let n2479: ZW = zw_mix2(n2477, zw_splat(n2195), 272u64);
    let n2480: ZW = zw_mix1(n2478, n2445, 280u64);
    let n2481: ZW = zw_mix2(n2479, n2445, 280u64);
    let n2482: ZW = zw_bits_n(n1813);
    let n2483: ZW = zw_mix1(n2480, n2482, 281u64);
    let n2484: ZW = zw_mix2(n2481, n2482, 281u64);
    let n2485: ZW = zw_mix1(n2453, n2298, 271u64);
    let n2486: ZW = zw_mix2(n2454, n2298, 271u64);
    let n2487: ZW = zw_mix1(n2485, n2218, 272u64);
    let n2488: ZW = zw_mix2(n2486, n2218, 272u64);
    let n2489: ZW = zw_mix1(n2487, n2459, 280u64);
    let n2490: ZW = zw_mix2(n2488, n2459, 280u64);
    let n2491: ZW = zw_bits_n(n1815);
    let n2492: ZW = zw_mix1(n2489, n2491, 281u64);
    let n2493: ZW = zw_mix2(n2490, n2491, 281u64);
    let n2494: ZW = zw_mix1(n2465, n2298, 271u64);
    let n2495: ZW = zw_mix2(n2466, n2298, 271u64);
    let n2496: ZW = zw_mix1(n2494, zw_splat(n2195), 272u64);
    let n2497: ZW = zw_mix2(n2495, zw_splat(n2195), 272u64);
    let n2498: ZW = zw_mix1(n2496, n2471, 280u64);
    let n2499: ZW = zw_mix2(n2497, n2471, 280u64);
    let n2500: ZW = zw_mix1(n2498, n2491, 281u64);
    let n2501: ZW = zw_mix2(n2499, n2491, 281u64);
    let n2502: ZW = zw_bits_n(n1969);
    let n2503: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2502, 20u64);
    let n2504: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2502, 20u64);
    let n2505: ZW = zw_bits_b(n1972);
    let n2506: ZW = zw_mix1(n2503, n2505, 38u64);
    let n2507: ZW = zw_mix2(n2504, n2505, 38u64);
    let n2508: ZW = zw_bits_n(n1968);
    let n2509: ZW = zw_mix1(n2506, n2508, 39u64);
    let n2510: ZW = zw_mix2(n2507, n2508, 39u64);
    let n2511: ZW = zw_bits_b(n1970);
    let n2512: ZW = zw_mix1(n2509, n2511, 41u64);
    let n2513: ZW = zw_mix2(n2510, n2511, 41u64);
    let n2514: ZW = zw_bits_b(n1971);
    let n2515: ZW = zw_mix1(n2512, n2514, 42u64);
    let n2516: ZW = zw_mix2(n2513, n2514, 42u64);
    let n2517: ZW = zw_mix1(n2515, n2004, 43u64);
    let n2518: ZW = zw_mix2(n2516, n2004, 43u64);
    let n2519: ZW = zw_mix1(n2517, n2007, 84u64);
    let n2520: ZW = zw_mix2(n2518, n2007, 84u64);
    let n2521: ZW = zw_mix1(n2519, n2010, 85u64);
    let n2522: ZW = zw_mix2(n2520, n2010, 85u64);
    let n2523: ZW = zw_mix1(n2521, n2013, 86u64);
    let n2524: ZW = zw_mix2(n2522, n2013, 86u64);
    let n2525: ZW = zw_mix1(n2523, n2172, 87u64);
    let n2526: ZW = zw_mix2(n2524, n2172, 87u64);
    let n2527: ZW = zw_mix1(n2525, n2019, 88u64);
    let n2528: ZW = zw_mix2(n2526, n2019, 88u64);
    let n2529: ZW = zw_bits_b(n1973);
    let n2530: ZW = zw_mix1(n2527, n2529, 232u64);
    let n2531: ZW = zw_mix2(n2528, n2529, 232u64);
    let n2532: ZW = zw_bits_n(n1974);
    let n2533: ZW = zw_mix1(n2530, n2532, 233u64);
    let n2534: ZW = zw_mix2(n2531, n2532, 233u64);
    let n2535: ZW = zw_bits_b(n1975);
    let n2536: ZW = zw_mix1(n2533, n2535, 242u64);
    let n2537: ZW = zw_mix2(n2534, n2535, 242u64);
    let n2538: ZW = zw_bits_n(n1976);
    let n2539: ZW = zw_mix1(n2536, n2538, 244u64);
    let n2540: ZW = zw_mix2(n2537, n2538, 244u64);
    let n2541: ZW = zw_bits_n(n1977);
    let n2542: ZW = zw_mix1(n2539, n2541, 245u64);
    let n2543: ZW = zw_mix2(n2540, n2541, 245u64);
    let n2544: ZW = zw_bits_n(n1978);
    let n2545: ZW = zw_mix1(n2542, n2544, 248u64);
    let n2546: ZW = zw_mix2(n2543, n2544, 248u64);
    let n2547: ZW = zw_bits_n(n1979);
    let n2548: ZW = zw_mix1(n2545, n2547, 249u64);
    let n2549: ZW = zw_mix2(n2546, n2547, 249u64);
    let n2550: ZW = zw_bits_b(n1980);
    let n2551: ZW = zw_mix1(n2548, n2550, 262u64);
    let n2552: ZW = zw_mix2(n2549, n2550, 262u64);
    let n2553: ZW = zw_bits_b(n1981);
    let n2554: ZW = zw_mix1(n2551, n2553, 263u64);
    let n2555: ZW = zw_mix2(n2552, n2553, 263u64);
    let n2556: ZW = zw_bits_n(n1982);
    let n2557: ZW = zw_mix1(n2554, n2556, 264u64);
    let n2558: ZW = zw_mix2(n2555, n2556, 264u64);
    let n2559: ZW = zw_bits_n(n1983);
    let n2560: ZW = zw_mix1(n2557, n2559, 265u64);
    let n2561: ZW = zw_mix2(n2558, n2559, 265u64);
    let n2562: ZW = zw_bits_n(n1984);
    let n2563: ZW = zw_mix1(n2560, n2562, 266u64);
    let n2564: ZW = zw_mix2(n2561, n2562, 266u64);
    let n2565: ZW = zw_bits_n(n1985);
    let n2566: ZW = zw_mix1(n2563, n2565, 267u64);
    let n2567: ZW = zw_mix2(n2564, n2565, 267u64);
    let n2568: ZW = zw_bits_n(n1986);
    let n2569: ZW = zw_mix1(n2566, n2568, 268u64);
    let n2570: ZW = zw_mix2(n2567, n2568, 268u64);
    let n2571: ZW = zw_bits_n(n1987);
    let n2572: ZW = zw_mix1(n2569, n2571, 269u64);
    let n2573: ZW = zw_mix2(n2570, n2571, 269u64);
    let n2574: ZW = zw_bits_n(n1988);
    let n2575: ZW = zw_mix1(n2572, n2574, 270u64);
    let n2576: ZW = zw_mix2(n2573, n2574, 270u64);
    let n2577: ZW = zw_bits_n(n1989);
    let n2578: ZW = zw_mix1(n2575, n2577, 271u64);
    let n2579: ZW = zw_mix2(n2576, n2577, 271u64);
    let n2580: ZW = zw_bits_n(n1990);
    let n2581: ZW = zw_mix1(n2578, n2580, 272u64);
    let n2582: ZW = zw_mix2(n2579, n2580, 272u64);
    let n2583: ZW = zw_bits_n(n1991);
    let n2584: ZW = zw_mix1(n2581, n2583, 273u64);
    let n2585: ZW = zw_mix2(n2582, n2583, 273u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n963);
    let bd_v0_b0: bool = false;
    let live_v0_b0: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1018);
    let ok_v1_b1: u16 = ALL & zb_holds(n963);
    let bd_v1_b1: bool = false;
    let live_v1_b1: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1048);
    let ok_v2_b2: u16 = ALL & zb_holds(n963);
    let bd_v2_b2: bool = false;
    let live_v2_b2: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1075);
    let ok_v16_b3: u16 = ALL & zb_holds(n963);
    let bd_v16_b3: bool = false;
    let live_v16_b3: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1107);
    let ok_v17_b4: u16 = ALL & zb_holds(n963);
    let bd_v17_b4: bool = false;
    let live_v17_b4: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1139);
    let ok_v18_b5: u16 = ALL & zb_holds(n963);
    let bd_v18_b5: bool = false;
    let live_v18_b5: u16 = ALL & zb_holds(n61) & zb_holds(n1015) & zb_holds(n1171);
    let ok_v32_b6: u16 = ALL & zb_holds(n963);
    let bd_v32_b6: bool = false;
    let live_v32_b6: u16 = ALL & zb_holds(n1187);
    let ok_v33_b7: u16 = ALL & zb_holds(n963);
    let bd_v33_b7: bool = false;
    let live_v33_b7: u16 = ALL & zb_holds(n1197);
    let ok_v34_b8: u16 = ALL & zb_holds(n963);
    let bd_v34_b8: bool = false;
    let live_v34_b8: u16 = ALL & zb_holds(n1207);
    let ok_v48_b9: u16 = ALL & zb_holds(n963);
    let bd_v48_b9: bool = false;
    let live_v48_b9: u16 = ALL & zb_holds(n1217);
    let ok_v49_b10: u16 = ALL & zb_holds(n963);
    let bd_v49_b10: bool = false;
    let live_v49_b10: u16 = ALL & zb_holds(n1227);
    let ok_v50_b11: u16 = ALL & zb_holds(n963);
    let bd_v50_b11: bool = false;
    let live_v50_b11: u16 = ALL & zb_holds(n1237);
    let ok_v0_b12: u16 = ALL & zb_holds(n1277);
    let bd_v0_b12: bool = false;
    let live_v0_b12: u16 = ALL & zb_holds(n61) & zb_holds(n1276);
    let ok_v1_b13: u16 = ALL & zb_holds(n1309);
    let bd_v1_b13: bool = false;
    let live_v1_b13: u16 = ALL & zb_holds(n61) & zb_holds(n1308);
    let ok_v2_b14: u16 = ALL & zb_holds(n1339);
    let bd_v2_b14: bool = false;
    let live_v2_b14: u16 = ALL & zb_holds(n61) & zb_holds(n1338);
    let ok_v16_b15: u16 = ALL & zb_holds(n1378);
    let bd_v16_b15: bool = false;
    let live_v16_b15: u16 = ALL & zb_holds(n61) & zb_holds(n1377);
    let ok_v17_b16: u16 = ALL & zb_holds(n1417);
    let bd_v17_b16: bool = false;
    let live_v17_b16: u16 = ALL & zb_holds(n61) & zb_holds(n1416);
    let ok_v18_b17: u16 = ALL & zb_holds(n1456);
    let bd_v18_b17: bool = false;
    let live_v18_b17: u16 = ALL & zb_holds(n61) & zb_holds(n1455);
    let ok_v32_b18: u16 = ALL & zb_holds(n1471);
    let bd_v32_b18: bool = false;
    let live_v32_b18: u16 = ALL & zb_holds(n1474);
    let ok_v33_b19: u16 = ALL & zb_holds(n1488);
    let bd_v33_b19: bool = false;
    let live_v33_b19: u16 = ALL & zb_holds(n1491);
    let ok_v34_b20: u16 = ALL & zb_holds(n1505);
    let bd_v34_b20: bool = false;
    let live_v34_b20: u16 = ALL & zb_holds(n1508);
    let ok_v48_b21: u16 = ALL & zb_holds(n1522);
    let bd_v48_b21: bool = false;
    let live_v48_b21: u16 = ALL & zb_holds(n1525);
    let ok_v49_b22: u16 = ALL & zb_holds(n1539);
    let bd_v49_b22: bool = false;
    let live_v49_b22: u16 = ALL & zb_holds(n1542);
    let ok_v50_b23: u16 = ALL & zb_holds(n1556);
    let bd_v50_b23: bool = false;
    let live_v50_b23: u16 = ALL & zb_holds(n1559);
    let ok_v0_b24: u16 = ALL & zb_holds(n1580);
    let bd_v0_b24: bool = false;
    let live_v0_b24: u16 = ALL & zb_holds(n1596);
    let ok_v1_b25: u16 = ALL & zb_holds(n1580);
    let bd_v1_b25: bool = false;
    let live_v1_b25: u16 = ALL & zb_holds(n1610);
    let ok_v2_b26: u16 = ALL & zb_holds(n1580);
    let bd_v2_b26: bool = false;
    let live_v2_b26: u16 = ALL & zb_holds(n1624);
    let ok_v16_b27: u16 = ALL & zb_holds(n1580);
    let bd_v16_b27: bool = false;
    let live_v16_b27: u16 = ALL & zb_holds(n1638);
    let ok_v17_b28: u16 = ALL & zb_holds(n1580);
    let bd_v17_b28: bool = false;
    let live_v17_b28: u16 = ALL & zb_holds(n1650);
    let ok_v18_b29: u16 = ALL & zb_holds(n1580);
    let bd_v18_b29: bool = false;
    let live_v18_b29: u16 = ALL & zb_holds(n1662);
    let ok_v32_b30: u16 = ALL & zb_holds(n1580);
    let bd_v32_b30: bool = false;
    let live_v32_b30: u16 = ALL & zb_holds(n1697);
    let ok_v33_b31: u16 = ALL & zb_holds(n1580);
    let bd_v33_b31: bool = false;
    let live_v33_b31: u16 = ALL & zb_holds(n1714);
    let ok_v34_b32: u16 = ALL & zb_holds(n1580);
    let bd_v34_b32: bool = false;
    let live_v34_b32: u16 = ALL & zb_holds(n1729);
    let ok_v36_b33: u16 = ALL & zb_holds(n1580);
    let bd_v36_b33: bool = false;
    let live_v36_b33: u16 = ALL & zb_holds(n1697);
    let ok_v37_b34: u16 = ALL & zb_holds(n1580);
    let bd_v37_b34: bool = false;
    let live_v37_b34: u16 = ALL & zb_holds(n1714);
    let ok_v38_b35: u16 = ALL & zb_holds(n1580);
    let bd_v38_b35: bool = false;
    let live_v38_b35: u16 = ALL & zb_holds(n1729);
    let ok_v40_b36: u16 = ALL & zb_holds(n1580);
    let bd_v40_b36: bool = false;
    let live_v40_b36: u16 = ALL & zb_holds(n1697);
    let ok_v41_b37: u16 = ALL & zb_holds(n1580);
    let bd_v41_b37: bool = false;
    let live_v41_b37: u16 = ALL & zb_holds(n1714);
    let ok_v42_b38: u16 = ALL & zb_holds(n1580);
    let bd_v42_b38: bool = false;
    let live_v42_b38: u16 = ALL & zb_holds(n1729);
    let ok_v48_b39: u16 = ALL & zb_holds(n1580);
    let bd_v48_b39: bool = false;
    let live_v48_b39: u16 = ALL & zb_holds(n1765);
    let ok_v49_b40: u16 = ALL & zb_holds(n1580);
    let bd_v49_b40: bool = false;
    let live_v49_b40: u16 = ALL & zb_holds(n1780);
    let ok_v50_b41: u16 = ALL & zb_holds(n1580);
    let bd_v50_b41: bool = false;
    let live_v50_b41: u16 = ALL & zb_holds(n1795);
    let ok_v52_b42: u16 = ALL & zb_holds(n1580);
    let bd_v52_b42: bool = false;
    let live_v52_b42: u16 = ALL & zb_holds(n1765);
    let ok_v53_b43: u16 = ALL & zb_holds(n1580);
    let bd_v53_b43: bool = false;
    let live_v53_b43: u16 = ALL & zb_holds(n1780);
    let ok_v54_b44: u16 = ALL & zb_holds(n1580);
    let bd_v54_b44: bool = false;
    let live_v54_b44: u16 = ALL & zb_holds(n1795);
    let ok_v56_b45: u16 = ALL & zb_holds(n1580);
    let bd_v56_b45: bool = false;
    let live_v56_b45: u16 = ALL & zb_holds(n1765);
    let ok_v57_b46: u16 = ALL & zb_holds(n1580);
    let bd_v57_b46: bool = false;
    let live_v57_b46: u16 = ALL & zb_holds(n1780);
    let ok_v58_b47: u16 = ALL & zb_holds(n1580);
    let bd_v58_b47: bool = false;
    let live_v58_b47: u16 = ALL & zb_holds(n1795);
    let ok_v0_b48: u16 = ALL & zb_holds(n1993);
    let bd_v0_b48: bool = false;
    let live_v0_b48: u16 = ALL & zb_holds(n1998);
    let sh0 = KShared0 {
        c87: n961,
        c84: n51,
        c42: n83,
        c88: r_c88,
        c86: n59,
        c43: r_c43,
        c85: n60,
    };
    let sh1 = KShared1 {
        c84: n51,
        c88: r_c88,
        c86: n59,
        c43: r_c43,
        c85: n60,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: n1243,
        c84: n51,
        c42: n83,
        c88: r_c88,
        c86: n59,
        c254: n273,
        c43: r_c43,
        c85: n60,
        c38: n1244,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: n1968,
        c84: n51,
        c20: n1969,
        c41: n1970,
        c42: n1971,
        c88: r_c88,
        c86: n59,
        c232: n1973,
        c233: n1974,
        c262: n1980,
        c263: n1981,
        c264: n1982,
        c265: n1983,
        c266: n1984,
        c267: n1985,
        c268: n1986,
        c269: n1987,
        c242: n1975,
        c270: n1988,
        c271: n1989,
        c244: n1976,
        c245: n1977,
        c272: n1990,
        c273: n1991,
        c248: n1978,
        c249: n1979,
        c43: r_c43,
        c85: n60,
        c38: n1972,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
    let mut take_1_2: u16 = 0;
    let mut take_1_3: u16 = 0;
    let mut take_1_4: u16 = 0;
    let mut take_1_5: u16 = 0;
    let mut take_1_6: u16 = 0;
    let mut take_1_7: u16 = 0;
    let mut take_1_8: u16 = 0;
    let mut take_1_9: u16 = 0;
    let mut take_1_10: u16 = 0;
    let mut take_1_11: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_2_2: u16 = 0;
    let mut take_2_3: u16 = 0;
    let mut take_2_4: u16 = 0;
    let mut take_2_5: u16 = 0;
    let mut take_2_6: u16 = 0;
    let mut take_2_7: u16 = 0;
    let mut take_2_8: u16 = 0;
    let mut take_2_9: u16 = 0;
    let mut take_2_10: u16 = 0;
    let mut take_2_11: u16 = 0;
    let mut take_2_12: u16 = 0;
    let mut take_2_13: u16 = 0;
    let mut take_2_14: u16 = 0;
    let mut take_2_15: u16 = 0;
    let mut take_2_16: u16 = 0;
    let mut take_2_17: u16 = 0;
    let mut take_2_18: u16 = 0;
    let mut take_2_19: u16 = 0;
    let mut take_2_20: u16 = 0;
    let mut take_2_21: u16 = 0;
    let mut take_2_22: u16 = 0;
    let mut take_2_23: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 49 distinct button assignments; per outcome they fall
    // into [2, 12, 24, 1] groups that write identical values.
    declined |= live_v0_b0 & !ok_v0_b0;
    take_0_0 |= live_v0_b0 & ok_v0_b0;
    declined |= live_v1_b1 & !ok_v1_b1;
    take_0_0 |= live_v1_b1 & ok_v1_b1;
    declined |= live_v2_b2 & !ok_v2_b2;
    take_0_0 |= live_v2_b2 & ok_v2_b2;
    declined |= live_v16_b3 & !ok_v16_b3;
    take_0_0 |= live_v16_b3 & ok_v16_b3;
    declined |= live_v17_b4 & !ok_v17_b4;
    take_0_0 |= live_v17_b4 & ok_v17_b4;
    declined |= live_v18_b5 & !ok_v18_b5;
    take_0_0 |= live_v18_b5 & ok_v18_b5;
    let o0 = KOut0 {
        c20: r_c20,
        c41: n82,
        h1: n2026, h2: n2027,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v32_b6 & !ok_v32_b6;
    take_0_1 |= live_v32_b6 & ok_v32_b6;
    declined |= live_v33_b7 & !ok_v33_b7;
    take_0_1 |= live_v33_b7 & ok_v33_b7;
    declined |= live_v34_b8 & !ok_v34_b8;
    take_0_1 |= live_v34_b8 & ok_v34_b8;
    declined |= live_v48_b9 & !ok_v48_b9;
    take_0_1 |= live_v48_b9 & ok_v48_b9;
    declined |= live_v49_b10 & !ok_v49_b10;
    take_0_1 |= live_v49_b10 & ok_v49_b10;
    declined |= live_v50_b11 & !ok_v50_b11;
    take_0_1 |= live_v50_b11 & ok_v50_b11;
    let o0 = KOut0 {
        c20: n1174,
        c41: n1175,
        h1: n2032, h2: n2033,
    };
    // body 11: buttons 0x32, forks 0x0
    sink.o0(50, take_0_1, &sh0, &o0);
    declined |= live_v0_b12 & !ok_v0_b12;
    take_1_0 |= live_v0_b12 & ok_v0_b12;
    let o1 = KOut1 {
        c87: n1272,
        c39: n1273,
        c20: r_c20,
        c38: n1275,
        h1: n2053, h2: n2054,
    };
    // body 12: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b13 & !ok_v1_b13;
    take_1_1 |= live_v1_b13 & ok_v1_b13;
    let o1 = KOut1 {
        c87: n1304,
        c39: n1305,
        c20: r_c20,
        c38: n1307,
        h1: n2062, h2: n2063,
    };
    // body 13: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b14 & !ok_v2_b14;
    take_1_2 |= live_v2_b14 & ok_v2_b14;
    let o1 = KOut1 {
        c87: n1334,
        c39: n1335,
        c20: r_c20,
        c38: n1337,
        h1: n2071, h2: n2072,
    };
    // body 14: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b15 & !ok_v16_b15;
    take_1_3 |= live_v16_b15 & ok_v16_b15;
    let o1 = KOut1 {
        c87: n1373,
        c39: n1374,
        c20: r_c20,
        c38: n1376,
        h1: n2080, h2: n2081,
    };
    // body 15: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b16 & !ok_v17_b16;
    take_1_4 |= live_v17_b16 & ok_v17_b16;
    let o1 = KOut1 {
        c87: n1412,
        c39: n1413,
        c20: r_c20,
        c38: n1415,
        h1: n2089, h2: n2090,
    };
    // body 16: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b17 & !ok_v18_b17;
    take_1_5 |= live_v18_b17 & ok_v18_b17;
    let o1 = KOut1 {
        c87: n1451,
        c39: n1452,
        c20: r_c20,
        c38: n1454,
        h1: n2098, h2: n2099,
    };
    // body 17: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b18 & !ok_v32_b18;
    take_1_6 |= live_v32_b18 & ok_v32_b18;
    let o1 = KOut1 {
        c87: n1466,
        c39: n1467,
        c20: n1174,
        c38: n1469,
        h1: n2109, h2: n2110,
    };
    // body 18: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b19 & !ok_v33_b19;
    take_1_7 |= live_v33_b19 & ok_v33_b19;
    let o1 = KOut1 {
        c87: n1483,
        c39: n1484,
        c20: n1174,
        c38: n1486,
        h1: n2118, h2: n2119,
    };
    // body 19: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b20 & !ok_v34_b20;
    take_1_8 |= live_v34_b20 & ok_v34_b20;
    let o1 = KOut1 {
        c87: n1500,
        c39: n1501,
        c20: n1174,
        c38: n1503,
        h1: n2127, h2: n2128,
    };
    // body 20: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v48_b21 & !ok_v48_b21;
    take_1_9 |= live_v48_b21 & ok_v48_b21;
    let o1 = KOut1 {
        c87: n1517,
        c39: n1518,
        c20: n1174,
        c38: n1520,
        h1: n2136, h2: n2137,
    };
    // body 21: buttons 0x30, forks 0x0
    sink.o1(48, take_1_9, &sh1, &o1);
    declined |= live_v49_b22 & !ok_v49_b22;
    take_1_10 |= live_v49_b22 & ok_v49_b22;
    let o1 = KOut1 {
        c87: n1534,
        c39: n1535,
        c20: n1174,
        c38: n1537,
        h1: n2145, h2: n2146,
    };
    // body 22: buttons 0x31, forks 0x0
    sink.o1(49, take_1_10, &sh1, &o1);
    declined |= live_v50_b23 & !ok_v50_b23;
    take_1_11 |= live_v50_b23 & ok_v50_b23;
    let o1 = KOut1 {
        c87: n1551,
        c39: n1552,
        c20: n1174,
        c38: n1554,
        h1: n2154, h2: n2155,
    };
    // body 23: buttons 0x32, forks 0x0
    sink.o1(50, take_1_11, &sh1, &o1);
    declined |= live_v0_b24 & !ok_v0_b24;
    take_2_0 |= live_v0_b24 & ok_v0_b24;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1577,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1578,
        c253: n1597,
        h1: n2216, h2: n2217,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b25 & !ok_v1_b25;
    take_2_1 |= live_v1_b25 & ok_v1_b25;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n286,
        c239: n1577,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1611,
        c281: n1578,
        c253: n1597,
        h1: n2224, h2: n2225,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b26 & !ok_v2_b26;
    take_2_2 |= live_v2_b26 & ok_v2_b26;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1577,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n1625,
        c281: n1578,
        c253: n1597,
        h1: n2229, h2: n2230,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b27 & !ok_v16_b27;
    take_2_3 |= live_v16_b27 & ok_v16_b27;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1627,
        c246: zb_splat(false),
        c247: n286,
        c280: n1639,
        c281: n1629,
        c253: n1597,
        h1: n2254, h2: n2255,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b28 & !ok_v17_b28;
    take_2_4 |= live_v17_b28 & ok_v17_b28;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: n286,
        c239: n1627,
        c246: zb_splat(false),
        c247: n286,
        c280: n1651,
        c281: n1629,
        c253: n1597,
        h1: n2261, h2: n2262,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b29 & !ok_v18_b29;
    take_2_5 |= live_v18_b29 & ok_v18_b29;
    let o2 = KOut2 {
        c20: r_c20,
        c41: n82,
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: n1576,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: r_c88,
        c272: zb_splat(false),
        c239: n1627,
        c246: zb_splat(false),
        c247: n286,
        c280: n1663,
        c281: n1629,
        c253: n1597,
        h1: n2266, h2: n2267,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b30 & !ok_v32_b30;
    take_2_6 |= live_v32_b30 & ok_v32_b30;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1696,
        c281: n1682,
        c253: n1695,
        h1: n2309, h2: n2310,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b31 & !ok_v33_b31;
    take_2_7 |= live_v33_b31 & ok_v33_b31;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1713,
        c281: n1682,
        c253: n1695,
        h1: n2321, h2: n2322,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b32 & !ok_v34_b32;
    take_2_8 |= live_v34_b32 & ok_v34_b32;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1728,
        c281: n1682,
        c253: n1695,
        h1: n2326, h2: n2327,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b33 & !ok_v36_b33;
    take_2_9 |= live_v36_b33 & ok_v36_b33;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1678,
        c234: n1675,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1734,
        c253: n1695,
        h1: n2342, h2: n2343,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b34 & !ok_v37_b34;
    take_2_10 |= live_v37_b34 & ok_v37_b34;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1740,
        c281: n1738,
        c253: n1695,
        h1: n2356, h2: n2357,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b35 & !ok_v38_b35;
    take_2_11 |= live_v38_b35 & ok_v38_b35;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1744,
        c281: n1738,
        c253: n1695,
        h1: n2367, h2: n2368,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b36 & !ok_v40_b36;
    take_2_12 |= live_v40_b36 & ok_v40_b36;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1678,
        c234: n1675,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n1746,
        c253: n1695,
        h1: n2376, h2: n2377,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b37 & !ok_v41_b37;
    take_2_13 |= live_v41_b37 & ok_v41_b37;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1740,
        c281: n1748,
        c253: n1695,
        h1: n2385, h2: n2386,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b38 & !ok_v42_b38;
    take_2_14 |= live_v42_b38 & ok_v42_b38;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1577,
        c246: n286,
        c247: zb_splat(false),
        c280: n1744,
        c281: n1748,
        c253: n1695,
        h1: n2393, h2: n2394,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b39 & !ok_v48_b39;
    take_2_15 |= live_v48_b39 & ok_v48_b39;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1764,
        c281: n1753,
        c253: n1695,
        h1: n2417, h2: n2418,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b40 & !ok_v49_b40;
    take_2_16 |= live_v49_b40 & ok_v49_b40;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1779,
        c281: n1753,
        c253: n1695,
        h1: n2428, h2: n2429,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b41 & !ok_v50_b41;
    take_2_17 |= live_v50_b41 & ok_v50_b41;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1678,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: zn_splat(P8::from_raw(0i32)),
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1794,
        c281: n1753,
        c253: n1695,
        h1: n2433, h2: n2434,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b42 & !ok_v52_b42;
    take_2_18 |= live_v52_b42 & ok_v52_b42;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1678,
        c234: n1675,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1801,
        c281: n1799,
        c253: n1695,
        h1: n2449, h2: n2450,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b43 & !ok_v53_b43;
    take_2_19 |= live_v53_b43 & ok_v53_b43;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1807,
        c281: n1805,
        c253: n1695,
        h1: n2463, h2: n2464,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b44 & !ok_v54_b44;
    take_2_20 |= live_v54_b44 & ok_v54_b44;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: n1733,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1811,
        c281: n1805,
        c253: n1695,
        h1: n2474, h2: n2475,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b45 & !ok_v56_b45;
    take_2_21 |= live_v56_b45 & ok_v56_b45;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1678,
        c234: n1675,
        c270: zn_splat(P8::from_raw(0i32)),
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1801,
        c281: n1813,
        c253: n1695,
        h1: n2483, h2: n2484,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b46 & !ok_v57_b46;
    take_2_22 |= live_v57_b46 & ok_v57_b46;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1701,
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: n286,
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1807,
        c281: n1815,
        c253: n1695,
        h1: n2492, h2: n2493,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b47 & !ok_v58_b47;
    take_2_23 |= live_v58_b47 & ok_v58_b47;
    let o2 = KOut2 {
        c20: n1673,
        c41: n1674,
        c268: n1679,
        c269: n1679,
        c234: n1675,
        c270: n1680,
        c271: n1680,
        c236: n1676,
        c237: n1677,
        c272: zb_splat(false),
        c239: n1627,
        c246: n286,
        c247: n286,
        c280: n1811,
        c281: n1815,
        c253: n1695,
        h1: n2500, h2: n2501,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined |= live_v0_b48 & !ok_v0_b48;
    take_3_0 |= live_v0_b48 & ok_v0_b48;
    let o3 = KOut3 {
        h1: n2584, h2: n2585,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
