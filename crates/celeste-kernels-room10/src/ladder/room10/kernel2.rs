// GENERATED from a TRACED frame (shape 2). Do not edit.
//
// One input shape, 4 output shapes, 29 distinct button
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

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c254: ZN,
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
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

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
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

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
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
    pub c86: ZN,
    pub c233: ZN,
    pub c269: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c249: ZN,
    pub c85: ZN,
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
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
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
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_0 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_0: u64 = 12326174089916472897;
pub const KPART2_0: u64 = 11498443345394464153;

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
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
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_0.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_0.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
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
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_1 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_1: u64 = 1165628102232147319;
pub const KPART2_1: u64 = 4665604857065634973;

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
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
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_1.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_1.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
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
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
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
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_2 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 3885141205346150141;
pub const KPART2_2: u64 = 2992409829084096670;

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
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
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_2.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_2.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
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
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[232] = Col::U(AV::Bool(true));
    b.cols[233] = Col::N(Vec::new());
    b.cols[262] = Col::U(AV::Bool(false));
    b.cols[263] = Col::U(AV::Bool(false));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::N(Vec::new());
    b.cols[242] = Col::U(AV::Bool(false));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::N(Vec::new());
    b.cols[244] = Col::N(Vec::new());
    b.cols[245] = Col::N(Vec::new());
    b.cols[272] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[249] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[171] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_3 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 9441057012664292836;
pub const KPART2_3: u64 = 5930149933371155848;

/// Append this assignment's lanes that TAKE outcome 3 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
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
    skip: &dyn Fn((u64, u64)) -> bool,
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
        let k0 = mix64(KPART1_3.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_3.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[233] { v.push(sh.c233.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(sh.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[244] { v.push(sh.c244.lane(i)); }
        if let Col::N(v) = &mut acc.cols[245] { v.push(sh.c245.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
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

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32], skip: &'a dyn Fn((u64, u64)) -> bool }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org, self.skip);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org, self.skip);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org, self.skip);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org, self.skip);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
    skip: &dyn Fn((u64, u64)) -> bool,
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
    let mut sink = Append { accs, seen, n, org, skip };
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
    let n57: ZB = zb_not(r_c41);
    let n58: ZB = zb_not(r_c42);
    let n59: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n60: ZB = zb_not(r_c262);
    let n61: ZB = zb_not(r_c263);
    let n62: bool = P8::from_raw(524288i32) == u.c264;
    let n63: bool = P8::from_raw(524288i32) == u.c265;
    let n64: bool = P8::from_raw(0i32) == u.c266;
    let n65: bool = P8::from_raw(0i32) == u.c267;
    let n66: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c268);
    let n67: ZB = zb_not(r_c242);
    let n68: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n69: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c245);
    let n70: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c272);
    let n71: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n72: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c248);
    let n73: ZB = zb_not(r_c43);
    let n74: ZB = zb_not(r_c38);
    let n76: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n77: ZN = zn_rem(n76, zn_splat(P8::from_raw(1966080i32)));
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n77);
    let n80: ZB = zb_not(n69);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c245);
    let n82: ZB = zb_not(n81);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c245);
    let n84: ZN = zn_sub(r_c233, zn_splat(P8::from_raw(65536i32)));
    let n85: ZB = zn_lt(n84, zn_splat(P8::from_raw(0i32)));
    let n96: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n97: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n108: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n109: ZN = zn_rem(n108, zn_splat(P8::from_raw(3932160i32)));
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n109);
    let n111: ZN = zsel_n(n110, n97, r_c86);
    let n112: ZN = zsel_n(n78, n111, r_c86);
    let n113: ZN = zsel_n(n78, n109, r_c85);
    let n114: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c271);
    let n115: ZB = zb_not(n114);
    let n116: ZB = zb_and(n80, n96);
    let n117: ZB = zb_and(n82, n116);
    let n118: ZB = zb_and(n83, n117);
    let n119: ZB = zb_and(n85, n118);
    let n120: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n128: ZN = zn_add(r_c269, r_c271);
    let n129: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n128);
    let n130: ZN = zn_flr(n129);
    let n131: ZN = zn_add(r_c249, n130);
    let n132: ZN = zsel_n(n115, n131, r_c249);
    let n133: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n132);
    let n134: ZN = zn_div(n133, zn_splat(P8::from_raw(524288i32)));
    let n135: ZN = zn_flr(n134);
    let n136: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n135);
    let n137: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n133);
    let n138: ZN = zn_sub(n137, zn_splat(P8::from_raw(65536i32)));
    let n139: ZN = zn_div(n138, zn_splat(P8::from_raw(524288i32)));
    let n140: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n139);
    let n141: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n136);
    let n142: ZB = zn_le(n141, n140);
    let n143: ZB = zn_gt(n141, n140);
    let n144: ZB = zb_and(n119, n143);
    let n145: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n141);
    let n146: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n145);
    let n147: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n146);
    let n148: ZN = zn_rem(n138, zn_splat(P8::from_raw(524288i32)));
    let n149: ZB = zn_ge(n148, zn_splat(P8::from_raw(393216i32)));
    let n150: ZN = zn_mul(n141, zn_splat(P8::from_raw(524288i32)));
    let n151: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n150);
    let n152: ZB = zn_eq(n137, n151);
    let n153: ZB = zb_and(n80, n82);
    let n154: ZB = zb_and(n83, n153);
    let n155: ZB = zb_and(n85, n154);
    let n156: ZB = zb_and(n96, n155);
    let n157: ZB = zb_and(n142, n156);
    let n158: ZB = zb_or(n149, n152);
    let n159: ZB = zb_and(n147, n158);
    let n160: ZB = zb_not(n159);
    let n161: ZB = zb_and(n157, n159);
    let n162: ZB = zb_and(n157, n160);
    let n163: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n146);
    let n164: ZN = zn_rem(n133, zn_splat(P8::from_raw(524288i32)));
    let n165: ZB = zn_le(n164, zn_splat(P8::from_raw(131072i32)));
    let n166: ZB = zb_and(n163, n165);
    let n167: ZB = zb_not(n166);
    let n168: ZB = zb_and(n162, n166);
    let n169: ZB = zb_and(n162, n167);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n146);
    let n171: ZB = zb_not(n170);
    let n172: ZB = zb_and(n169, n170);
    let n173: ZB = zb_and(n169, n171);
    let n174: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n146);
    let n175: ZB = zb_not(n174);
    let n176: ZB = zb_and(n173, n174);
    let n177: ZB = zb_and(n173, n175);
    let n178: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n136);
    let n179: ZB = zn_le(n178, n140);
    let n180: ZB = zn_gt(n178, n140);
    let n181: ZB = zb_and(n177, n179);
    let n182: ZB = zb_and(n177, n180);
    let n183: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n178);
    let n184: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n183);
    let n185: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n184);
    let n186: ZN = zn_mul(n178, zn_splat(P8::from_raw(524288i32)));
    let n187: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n186);
    let n188: ZB = zn_eq(n137, n187);
    let n189: ZB = zb_or(n149, n188);
    let n190: ZB = zb_and(n185, n189);
    let n191: ZB = zb_not(n190);
    let n192: ZB = zb_and(n181, n190);
    let n193: ZB = zb_and(n181, n191);
    let n194: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n184);
    let n195: ZB = zb_and(n165, n194);
    let n196: ZB = zb_not(n195);
    let n197: ZB = zb_and(n193, n195);
    let n198: ZB = zb_and(n193, n196);
    let n199: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n184);
    let n200: ZB = zb_not(n199);
    let n201: ZB = zb_and(n198, n199);
    let n202: ZB = zb_and(n198, n200);
    let n203: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n184);
    let n204: ZB = zb_not(n203);
    let n205: ZB = zb_and(n202, n203);
    let n206: ZB = zb_and(n202, n204);
    let n207: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n136);
    let n208: ZB = zn_le(n207, n140);
    let n209: ZB = zn_gt(n207, n140);
    let n210: ZB = zb_and(n206, n208);
    let n211: ZB = zb_and(n206, n209);
    let n212: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n207);
    let n213: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n212);
    let n214: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n213);
    let n215: ZN = zn_mul(n207, zn_splat(P8::from_raw(524288i32)));
    let n216: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n215);
    let n217: ZB = zn_eq(n137, n216);
    let n218: ZB = zb_or(n149, n217);
    let n219: ZB = zb_and(n214, n218);
    let n220: ZB = zb_not(n219);
    let n221: ZB = zb_and(n210, n219);
    let n222: ZB = zb_and(n210, n220);
    let n223: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n213);
    let n224: ZB = zb_and(n165, n223);
    let n225: ZB = zb_not(n224);
    let n226: ZB = zb_and(n222, n224);
    let n227: ZB = zb_and(n222, n225);
    let n228: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n213);
    let n229: ZB = zb_not(n228);
    let n230: ZB = zb_and(n227, n228);
    let n231: ZB = zb_and(n227, n229);
    let n232: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n213);
    let n233: ZB = zb_not(n232);
    let n234: ZB = zb_and(n231, n232);
    let n235: ZB = zb_and(n231, n233);
    let n236: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n136);
    let n237: ZB = zn_gt(n236, n140);
    let n238: ZB = zb_or(n211, n235);
    let n239: ZB = zb_or(n209, n237);
    let n240: ZB = zb_or(n182, n238);
    let n241: ZB = zb_or(n180, n239);
    let n242: ZB = zb_or(n144, n240);
    let n243: ZB = zb_or(n143, n241);
    let n244: ZB = zn_le(n132, zn_splat(P8::from_raw(8388608i32)));
    let n245: ZB = zb_and(n242, n244);
    let n246: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n133);
    let n247: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n246, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n248: ZB = zb_not(n247);
    let n249: ZN = zsel_n(n247, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n250: ZN = zsel_n(n248, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n251: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n250);
    let n252: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n250);
    let n253: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n133);
    let n254: ZN = zsel_n(n248, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n255: ZB = zn_gt(n249, zn_splat(P8::from_raw(0i32)));
    let n256: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n253, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n257: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n253, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n258: ZN = zsel_n(n257, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n259: ZN = zsel_n(n256, zn_splat(P8::from_raw(-65536i32)), n258);
    let n260: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n259);
    let n261: ZB = zb_not(n260);
    let n262: ZN = zn_neg(n259);
    let n263: ZN = zn_mul(n262, zn_splat(P8::from_raw(131072i32)));
    let n264: ZN = zsel_n(n261, n263, zn_splat(P8::from_raw(0i32)));
    let n265: ZN = zsel_n(n261, zn_splat(P8::from_raw(-131072i32)), n254);
    let n266: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n249);
    let n267: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n264);
    let n268: ZN = zsel_n(n255, zn_splat(P8::from_raw(-131072i32)), n265);
    let n269: ZB = zn_lt(n132, zn_splat(P8::from_raw(-262144i32)));
    let n270: ZB = zn_ge(n132, zn_splat(P8::from_raw(-262144i32)));
    let n271: ZB = zb_and(n245, n269);
    let n276: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n251);
    let n277: ZN = zsel_n(n261, n263, n276);
    let n278: ZN = zsel_n(n255, n276, n277);
    let n279: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n252);
    let n280: ZN = zsel_n(n261, n263, n279);
    let n281: ZN = zsel_n(n255, n279, n280);
    let n283: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n284: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n283);
    let n285: ZB = zb_or(n172, n176);
    let n286: ZB = zb_or(n168, n285);
    let n287: ZB = zb_or(n161, n286);
    let n288: ZB = zb_or(n201, n205);
    let n289: ZB = zb_or(n197, n288);
    let n290: ZB = zb_or(n192, n289);
    let n291: ZB = zb_or(n230, n234);
    let n292: ZB = zb_or(n226, n291);
    let n293: ZB = zb_or(n221, n292);
    let n294: ZB = zb_or(n290, n293);
    let n295: ZB = zb_or(n287, n294);
    let n296: ZB = zn_gt(n132, zn_splat(P8::from_raw(8388608i32)));
    let n297: ZN = zsel_n(n296, n284, n283);
    let n298: ZB = zb_and(n242, n296);
    let n299: ZN = zsel_n(n295, n297, n283);
    let n300: ZB = zb_or(n295, n298);
    let n301: ZB = zb_or(n243, n295);
    let n302: ZB = zb_and(n269, n300);
    let n310: ZN = zsel_n(n271, r_c87, n299);
    let n311: ZN = zsel_n(n271, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n312: ZB = zb_not(n271);
    let n313: ZB = zb_or(n271, n302);
    let n314: ZB = zsel_b(n271, n243, n301);
    let n317: ZB = zb_not(n83);
    let n318: ZB = zn_ge(n84, zn_splat(P8::from_raw(0i32)));
    let n319: ZN = zsel_n(n83, n84, r_c233);
    let n320: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n321: ZN = zsel_n(n120, n320, r_c20);
    let n322: ZB = zb_and(n69, n96);
    let n323: ZN = zn_add(r_c271, zn_splat(P8::from_raw(32768i32)));
    let n324: ZB = zn_gt(n323, zn_splat(P8::from_raw(0i32)));
    let n325: ZB = zn_gt(r_c233, zn_splat(P8::from_raw(0i32)));
    let n326: ZB = zb_and(n80, n81);
    let n327: ZB = zb_and(n96, n326);
    let n328: ZB = zb_and(n324, n325);
    let n329: ZN = zsel_n(n328, n84, r_c233);
    let n330: ZN = zsel_n(n328, zn_splat(P8::from_raw(0i32)), n323);
    let n331: ZB = zn_gt(n330, zn_splat(P8::from_raw(0i32)));
    let n332: ZB = zb_and(n117, n317);
    let n333: ZB = zb_and(n118, n318);
    let n334: ZN = zsel_n(n83, zn_splat(P8::from_raw(393216i32)), r_c244);
    let n335: ZB = zb_or(n332, n333);
    let n336: ZN = zsel_n(n81, r_c244, n334);
    let n337: ZN = zsel_n(n69, r_c244, n336);
    let n338: ZN = zsel_n(n120, r_c244, n337);
    let n339: ZN = zn_sub(n129, zn_splat(P8::from_raw(32768i32)));
    let n340: ZN = zn_sub(n339, n130);
    let n341: ZN = zsel_n(n115, n340, r_c269);
    let n342: ZB = zn_lt(n132, zn_splat(P8::from_raw(8388608i32)));
    let n343: ZN = zsel_n(n342, zn_splat(P8::from_raw(196608i32)), r_c233);
    let n344: ZN = zsel_n(n342, zn_splat(P8::from_raw(65536i32)), r_c245);
    let n345: ZB = zn_gt(n132, zn_splat(P8::from_raw(7340032i32)));
    let n346: ZB = zb_and(n331, n345);
    let n347: ZN = zsel_n(n346, zn_splat(P8::from_raw(327680i32)), n329);
    let n348: ZN = zsel_n(n346, zn_splat(P8::from_raw(131072i32)), r_c245);
    let n349: ZN = zsel_n(n346, zn_splat(P8::from_raw(7340032i32)), n132);
    let n350: ZN = zsel_n(n346, zn_splat(P8::from_raw(0i32)), n330);
    let n351: ZN = zsel_n(n81, n347, n319);
    let n352: ZN = zsel_n(n81, n348, r_c245);
    let n353: ZN = zsel_n(n81, n349, n132);
    let n354: ZN = zsel_n(n81, n350, r_c271);
    let n355: ZB = zb_or(n327, n335);
    let n356: ZN = zsel_n(n69, n343, n351);
    let n357: ZN = zsel_n(n69, n344, n352);
    let n358: ZN = zsel_n(n69, n132, n353);
    let n359: ZN = zsel_n(n69, r_c271, n354);
    let n360: ZB = zb_or(n322, n355);
    let n361: ZN = zsel_n(n120, r_c233, n356);
    let n362: ZN = zsel_n(n120, r_c245, n357);
    let n363: ZN = zsel_n(n120, r_c249, n358);
    let n364: ZN = zsel_n(n120, r_c269, n341);
    let n365: ZN = zsel_n(n120, r_c271, n359);
    let n366: ZB = zb_or(n120, n360);
    let n368: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n369: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n370: ZW = zw_add(zw_splat(0u64), n368);
    let n371: ZW = zw_add(zw_splat(0u64), n369);
    let n372: ZW = zw_cellmix_n(84u64, n77, 1542469173u64);
    let n373: ZW = zw_cellmix_n(84u64, n77, 668265263u64);
    let n374: ZW = zw_add(n370, n372);
    let n375: ZW = zw_add(n371, n373);
    let n376: ZW = zw_cellmix_n(85u64, n113, 1542469173u64);
    let n377: ZW = zw_cellmix_n(85u64, n113, 668265263u64);
    let n378: ZW = zw_add(n374, n376);
    let n379: ZW = zw_add(n375, n377);
    let n380: ZW = zw_cellmix_n(86u64, n112, 1542469173u64);
    let n381: ZW = zw_cellmix_n(86u64, n112, 668265263u64);
    let n382: ZW = zw_add(n378, n380);
    let n383: ZW = zw_add(n379, n381);
    let n384: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n385: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n386: ZW = zw_add(n382, n384);
    let n387: ZW = zw_add(n383, n385);
    let n388: ZW = zw_cellmix_n(254u64, n132, 1542469173u64);
    let n389: ZW = zw_cellmix_n(254u64, n132, 668265263u64);
    let n390: ZW = zw_add(n386, n388);
    let n391: ZW = zw_add(n387, n389);
    let n392: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n393: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n394: ZW = zw_add(n390, n392);
    let n395: ZW = zw_add(n391, n393);
    let n396: ZW = zw_cellmix_b(41u64, zb_splat(false), 1542469173u64);
    let n397: ZW = zw_cellmix_b(41u64, zb_splat(false), 668265263u64);
    let n398: ZW = zw_add(n394, n396);
    let n399: ZW = zw_add(n395, n397);
    let n400: ZW = zw_cellmix_n(234u64, zn_splat(P8::from_raw(-65536i32)), 1542469173u64);
    let n401: ZW = zw_cellmix_n(234u64, zn_splat(P8::from_raw(-65536i32)), 668265263u64);
    let n402: ZW = zw_add(n398, n400);
    let n403: ZW = zw_add(n399, n401);
    let n404: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n405: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n406: ZW = zw_add(n402, n404);
    let n407: ZW = zw_add(n403, n405);
    let n408: ZW = zw_cellmix_n(237u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n409: ZW = zw_cellmix_n(237u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n410: ZW = zw_add(n406, n408);
    let n411: ZW = zw_add(n407, n409);
    let n412: ZW = zw_cellmix_n(239u64, n249, 1542469173u64);
    let n413: ZW = zw_cellmix_n(239u64, n249, 668265263u64);
    let n414: ZW = zw_add(n410, n412);
    let n415: ZW = zw_add(n411, n413);
    let n416: ZW = zw_cellmix_b(246u64, zb_splat(false), 1542469173u64);
    let n417: ZW = zw_cellmix_b(246u64, zb_splat(false), 668265263u64);
    let n418: ZW = zw_add(n414, n416);
    let n419: ZW = zw_add(n415, n417);
    let n420: ZW = zw_cellmix_b(247u64, zb_splat(false), 1542469173u64);
    let n421: ZW = zw_cellmix_b(247u64, zb_splat(false), 668265263u64);
    let n422: ZW = zw_add(n418, n420);
    let n423: ZW = zw_add(n419, n421);
    let n424: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n425: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n426: ZW = zw_add(n422, n424);
    let n427: ZW = zw_add(n423, n425);
    let n428: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n429: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n430: ZW = zw_add(n426, n428);
    let n431: ZW = zw_add(n427, n429);
    let n432: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n433: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n434: ZW = zw_add(n430, n432);
    let n435: ZW = zw_add(n431, n433);
    let n436: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n437: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n438: ZW = zw_add(n434, n436);
    let n439: ZW = zw_add(n435, n437);
    let n440: ZW = zw_cellmix_b(272u64, zb_splat(false), 1542469173u64);
    let n441: ZW = zw_cellmix_b(272u64, zb_splat(false), 668265263u64);
    let n442: ZW = zw_add(n438, n440);
    let n443: ZW = zw_add(n439, n441);
    let n444: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n445: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n446: ZW = zw_add(n442, n444);
    let n447: ZW = zw_add(n443, n445);
    let n448: ZW = zw_cellmix_n(281u64, n254, 1542469173u64);
    let n449: ZW = zw_cellmix_n(281u64, n254, 668265263u64);
    let n450: ZW = zw_add(n446, n448);
    let n451: ZW = zw_add(n447, n449);
    let n452: ZW = zw_cellmix_b(272u64, zb_splat(true), 1542469173u64);
    let n453: ZW = zw_cellmix_b(272u64, zb_splat(true), 668265263u64);
    let n454: ZW = zw_add(n438, n452);
    let n455: ZW = zw_add(n439, n453);
    let n456: ZW = zw_cellmix_n(280u64, n276, 1542469173u64);
    let n457: ZW = zw_cellmix_n(280u64, n276, 668265263u64);
    let n458: ZW = zw_add(n454, n456);
    let n459: ZW = zw_add(n455, n457);
    let n460: ZW = zw_add(n458, n448);
    let n461: ZW = zw_add(n459, n449);
    let n462: ZW = zw_cellmix_n(280u64, n279, 1542469173u64);
    let n463: ZW = zw_cellmix_n(280u64, n279, 668265263u64);
    let n464: ZW = zw_add(n442, n462);
    let n465: ZW = zw_add(n443, n463);
    let n466: ZW = zw_add(n464, n448);
    let n467: ZW = zw_add(n465, n449);
    let n468: ZW = zw_cellmix_n(239u64, n266, 1542469173u64);
    let n469: ZW = zw_cellmix_n(239u64, n266, 668265263u64);
    let n470: ZW = zw_add(n410, n468);
    let n471: ZW = zw_add(n411, n469);
    let n472: ZW = zw_add(n470, n416);
    let n473: ZW = zw_add(n471, n417);
    let n474: ZW = zw_cellmix_b(247u64, zb_splat(true), 1542469173u64);
    let n475: ZW = zw_cellmix_b(247u64, zb_splat(true), 668265263u64);
    let n476: ZW = zw_add(n472, n474);
    let n477: ZW = zw_add(n473, n475);
    let n478: ZW = zw_add(n476, n424);
    let n479: ZW = zw_add(n477, n425);
    let n480: ZW = zw_add(n478, n428);
    let n481: ZW = zw_add(n479, n429);
    let n482: ZW = zw_add(n480, n432);
    let n483: ZW = zw_add(n481, n433);
    let n484: ZW = zw_add(n482, n436);
    let n485: ZW = zw_add(n483, n437);
    let n486: ZW = zw_add(n484, n440);
    let n487: ZW = zw_add(n485, n441);
    let n488: ZW = zw_cellmix_n(280u64, n267, 1542469173u64);
    let n489: ZW = zw_cellmix_n(280u64, n267, 668265263u64);
    let n490: ZW = zw_add(n486, n488);
    let n491: ZW = zw_add(n487, n489);
    let n492: ZW = zw_cellmix_n(281u64, n268, 1542469173u64);
    let n493: ZW = zw_cellmix_n(281u64, n268, 668265263u64);
    let n494: ZW = zw_add(n490, n492);
    let n495: ZW = zw_add(n491, n493);
    let n496: ZW = zw_add(n484, n452);
    let n497: ZW = zw_add(n485, n453);
    let n498: ZW = zw_cellmix_n(280u64, n278, 1542469173u64);
    let n499: ZW = zw_cellmix_n(280u64, n278, 668265263u64);
    let n500: ZW = zw_add(n496, n498);
    let n501: ZW = zw_add(n497, n499);
    let n502: ZW = zw_add(n500, n492);
    let n503: ZW = zw_add(n501, n493);
    let n504: ZW = zw_cellmix_n(280u64, n281, 1542469173u64);
    let n505: ZW = zw_cellmix_n(280u64, n281, 668265263u64);
    let n506: ZW = zw_add(n486, n504);
    let n507: ZW = zw_add(n487, n505);
    let n508: ZW = zw_add(n506, n492);
    let n509: ZW = zw_add(n507, n493);
    let n510: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n511: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n512: ZW = zw_add(n390, n510);
    let n513: ZW = zw_add(n391, n511);
    let n514: ZW = zw_cellmix_b(41u64, zb_splat(true), 1542469173u64);
    let n515: ZW = zw_cellmix_b(41u64, zb_splat(true), 668265263u64);
    let n516: ZW = zw_add(n512, n514);
    let n517: ZW = zw_add(n513, n515);
    let n518: ZW = zw_cellmix_n(234u64, zn_splat(P8::from_raw(655360i32)), 1542469173u64);
    let n519: ZW = zw_cellmix_n(234u64, zn_splat(P8::from_raw(655360i32)), 668265263u64);
    let n520: ZW = zw_add(n516, n518);
    let n521: ZW = zw_add(n517, n519);
    let n522: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(262144i32)), 1542469173u64);
    let n523: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(262144i32)), 668265263u64);
    let n524: ZW = zw_add(n520, n522);
    let n525: ZW = zw_add(n521, n523);
    let n526: ZW = zw_cellmix_n(237u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n527: ZW = zw_cellmix_n(237u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n528: ZW = zw_add(n524, n526);
    let n529: ZW = zw_add(n525, n527);
    let n530: ZW = zw_add(n528, n412);
    let n531: ZW = zw_add(n529, n413);
    let n532: ZW = zw_cellmix_b(246u64, zb_splat(true), 1542469173u64);
    let n533: ZW = zw_cellmix_b(246u64, zb_splat(true), 668265263u64);
    let n534: ZW = zw_add(n530, n532);
    let n535: ZW = zw_add(n531, n533);
    let n536: ZW = zw_add(n534, n420);
    let n537: ZW = zw_add(n535, n421);
    let n538: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n539: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n540: ZW = zw_add(n536, n538);
    let n541: ZW = zw_add(n537, n539);
    let n542: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n543: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n544: ZW = zw_add(n540, n542);
    let n545: ZW = zw_add(n541, n543);
    let n546: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n547: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n548: ZW = zw_add(n544, n546);
    let n549: ZW = zw_add(n545, n547);
    let n550: ZW = zw_add(n548, n436);
    let n551: ZW = zw_add(n549, n437);
    let n552: ZW = zw_add(n550, n440);
    let n553: ZW = zw_add(n551, n441);
    let n554: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n555: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n556: ZW = zw_add(n552, n554);
    let n557: ZW = zw_add(n553, n555);
    let n558: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n559: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n560: ZW = zw_add(n556, n558);
    let n561: ZW = zw_add(n557, n559);
    let n562: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(-131072i32)), 1542469173u64);
    let n563: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(-131072i32)), 668265263u64);
    let n564: ZW = zw_add(n544, n562);
    let n565: ZW = zw_add(n545, n563);
    let n566: ZW = zw_add(n564, n436);
    let n567: ZW = zw_add(n565, n437);
    let n568: ZW = zw_add(n566, n452);
    let n569: ZW = zw_add(n567, n453);
    let n570: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n571: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n572: ZW = zw_add(n568, n570);
    let n573: ZW = zw_add(n569, n571);
    let n574: ZW = zw_add(n572, n558);
    let n575: ZW = zw_add(n573, n559);
    let n576: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n577: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n578: ZW = zw_add(n552, n576);
    let n579: ZW = zw_add(n553, n577);
    let n580: ZW = zw_add(n578, n558);
    let n581: ZW = zw_add(n579, n559);
    let n582: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n583: ZW = zw_cellmix_n(268u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n584: ZW = zw_add(n536, n582);
    let n585: ZW = zw_add(n537, n583);
    let n586: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n587: ZW = zw_cellmix_n(269u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n588: ZW = zw_add(n584, n586);
    let n589: ZW = zw_add(n585, n587);
    let n590: ZW = zw_add(n588, n432);
    let n591: ZW = zw_add(n589, n433);
    let n592: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(-98304i32)), 1542469173u64);
    let n593: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(-98304i32)), 668265263u64);
    let n594: ZW = zw_add(n590, n592);
    let n595: ZW = zw_add(n591, n593);
    let n596: ZW = zw_add(n594, n440);
    let n597: ZW = zw_add(n595, n441);
    let n598: ZW = zw_add(n596, n444);
    let n599: ZW = zw_add(n597, n445);
    let n600: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n601: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n602: ZW = zw_add(n598, n600);
    let n603: ZW = zw_add(n599, n601);
    let n604: ZW = zw_add(n584, n542);
    let n605: ZW = zw_add(n585, n543);
    let n606: ZW = zw_add(n604, n562);
    let n607: ZW = zw_add(n605, n563);
    let n608: ZW = zw_add(n606, n592);
    let n609: ZW = zw_add(n607, n593);
    let n610: ZW = zw_add(n608, n452);
    let n611: ZW = zw_add(n609, n453);
    let n612: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n613: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n614: ZW = zw_add(n610, n612);
    let n615: ZW = zw_add(n611, n613);
    let n616: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n617: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n618: ZW = zw_add(n614, n616);
    let n619: ZW = zw_add(n615, n617);
    let n620: ZW = zw_add(n604, n546);
    let n621: ZW = zw_add(n605, n547);
    let n622: ZW = zw_add(n620, n592);
    let n623: ZW = zw_add(n621, n593);
    let n624: ZW = zw_add(n622, n440);
    let n625: ZW = zw_add(n623, n441);
    let n626: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n627: ZW = zw_cellmix_n(280u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n628: ZW = zw_add(n624, n626);
    let n629: ZW = zw_add(n625, n627);
    let n630: ZW = zw_add(n628, n616);
    let n631: ZW = zw_add(n629, n617);
    let n632: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n633: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n634: ZW = zw_add(n590, n632);
    let n635: ZW = zw_add(n591, n633);
    let n636: ZW = zw_add(n634, n440);
    let n637: ZW = zw_add(n635, n441);
    let n638: ZW = zw_add(n636, n444);
    let n639: ZW = zw_add(n637, n445);
    let n640: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n641: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n642: ZW = zw_add(n638, n640);
    let n643: ZW = zw_add(n639, n641);
    let n644: ZW = zw_add(n606, n632);
    let n645: ZW = zw_add(n607, n633);
    let n646: ZW = zw_add(n644, n452);
    let n647: ZW = zw_add(n645, n453);
    let n648: ZW = zw_add(n646, n612);
    let n649: ZW = zw_add(n647, n613);
    let n650: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n651: ZW = zw_cellmix_n(281u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n652: ZW = zw_add(n648, n650);
    let n653: ZW = zw_add(n649, n651);
    let n654: ZW = zw_add(n620, n632);
    let n655: ZW = zw_add(n621, n633);
    let n656: ZW = zw_add(n654, n440);
    let n657: ZW = zw_add(n655, n441);
    let n658: ZW = zw_add(n656, n626);
    let n659: ZW = zw_add(n657, n627);
    let n660: ZW = zw_add(n658, n650);
    let n661: ZW = zw_add(n659, n651);
    let n662: ZW = zw_add(n528, n468);
    let n663: ZW = zw_add(n529, n469);
    let n664: ZW = zw_add(n662, n532);
    let n665: ZW = zw_add(n663, n533);
    let n666: ZW = zw_add(n664, n474);
    let n667: ZW = zw_add(n665, n475);
    let n668: ZW = zw_add(n666, n538);
    let n669: ZW = zw_add(n667, n539);
    let n670: ZW = zw_add(n668, n542);
    let n671: ZW = zw_add(n669, n543);
    let n672: ZW = zw_add(n670, n546);
    let n673: ZW = zw_add(n671, n547);
    let n674: ZW = zw_add(n672, n436);
    let n675: ZW = zw_add(n673, n437);
    let n676: ZW = zw_add(n674, n440);
    let n677: ZW = zw_add(n675, n441);
    let n678: ZW = zw_add(n676, n554);
    let n679: ZW = zw_add(n677, n555);
    let n680: ZW = zw_add(n678, n558);
    let n681: ZW = zw_add(n679, n559);
    let n682: ZW = zw_add(n670, n562);
    let n683: ZW = zw_add(n671, n563);
    let n684: ZW = zw_add(n682, n436);
    let n685: ZW = zw_add(n683, n437);
    let n686: ZW = zw_add(n684, n452);
    let n687: ZW = zw_add(n685, n453);
    let n688: ZW = zw_add(n686, n570);
    let n689: ZW = zw_add(n687, n571);
    let n690: ZW = zw_add(n688, n558);
    let n691: ZW = zw_add(n689, n559);
    let n692: ZW = zw_add(n676, n576);
    let n693: ZW = zw_add(n677, n577);
    let n694: ZW = zw_add(n692, n558);
    let n695: ZW = zw_add(n693, n559);
    let n696: ZW = zw_add(n666, n582);
    let n697: ZW = zw_add(n667, n583);
    let n698: ZW = zw_add(n696, n586);
    let n699: ZW = zw_add(n697, n587);
    let n700: ZW = zw_add(n698, n432);
    let n701: ZW = zw_add(n699, n433);
    let n702: ZW = zw_add(n700, n592);
    let n703: ZW = zw_add(n701, n593);
    let n704: ZW = zw_add(n702, n440);
    let n705: ZW = zw_add(n703, n441);
    let n706: ZW = zw_add(n704, n444);
    let n707: ZW = zw_add(n705, n445);
    let n708: ZW = zw_add(n706, n600);
    let n709: ZW = zw_add(n707, n601);
    let n710: ZW = zw_add(n696, n542);
    let n711: ZW = zw_add(n697, n543);
    let n712: ZW = zw_add(n710, n562);
    let n713: ZW = zw_add(n711, n563);
    let n714: ZW = zw_add(n712, n592);
    let n715: ZW = zw_add(n713, n593);
    let n716: ZW = zw_add(n714, n452);
    let n717: ZW = zw_add(n715, n453);
    let n718: ZW = zw_add(n716, n612);
    let n719: ZW = zw_add(n717, n613);
    let n720: ZW = zw_add(n718, n616);
    let n721: ZW = zw_add(n719, n617);
    let n722: ZW = zw_add(n710, n546);
    let n723: ZW = zw_add(n711, n547);
    let n724: ZW = zw_add(n722, n592);
    let n725: ZW = zw_add(n723, n593);
    let n726: ZW = zw_add(n724, n440);
    let n727: ZW = zw_add(n725, n441);
    let n728: ZW = zw_add(n726, n626);
    let n729: ZW = zw_add(n727, n627);
    let n730: ZW = zw_add(n728, n616);
    let n731: ZW = zw_add(n729, n617);
    let n732: ZW = zw_add(n700, n632);
    let n733: ZW = zw_add(n701, n633);
    let n734: ZW = zw_add(n732, n440);
    let n735: ZW = zw_add(n733, n441);
    let n736: ZW = zw_add(n734, n444);
    let n737: ZW = zw_add(n735, n445);
    let n738: ZW = zw_add(n736, n640);
    let n739: ZW = zw_add(n737, n641);
    let n740: ZW = zw_add(n712, n632);
    let n741: ZW = zw_add(n713, n633);
    let n742: ZW = zw_add(n740, n452);
    let n743: ZW = zw_add(n741, n453);
    let n744: ZW = zw_add(n742, n612);
    let n745: ZW = zw_add(n743, n613);
    let n746: ZW = zw_add(n744, n650);
    let n747: ZW = zw_add(n745, n651);
    let n748: ZW = zw_add(n722, n632);
    let n749: ZW = zw_add(n723, n633);
    let n750: ZW = zw_add(n748, n440);
    let n751: ZW = zw_add(n749, n441);
    let n752: ZW = zw_add(n750, n626);
    let n753: ZW = zw_add(n751, n627);
    let n754: ZW = zw_add(n752, n650);
    let n755: ZW = zw_add(n753, n651);
    let n756: ZW = zw_add(zw_splat(0u64), n372);
    let n757: ZW = zw_add(zw_splat(0u64), n373);
    let n758: ZW = zw_add(n756, n376);
    let n759: ZW = zw_add(n757, n377);
    let n760: ZW = zw_add(n758, n380);
    let n761: ZW = zw_add(n759, n381);
    let n762: ZW = zw_cellmix_n(87u64, n299, 1542469173u64);
    let n763: ZW = zw_cellmix_n(87u64, n299, 668265263u64);
    let n764: ZW = zw_add(n760, n762);
    let n765: ZW = zw_add(n761, n763);
    let n766: ZW = zw_add(n764, n392);
    let n767: ZW = zw_add(n765, n393);
    let n768: ZW = zw_add(n766, n396);
    let n769: ZW = zw_add(n767, n397);
    let n770: ZW = zw_add(n764, n510);
    let n771: ZW = zw_add(n765, n511);
    let n772: ZW = zw_add(n770, n514);
    let n773: ZW = zw_add(n771, n515);
    let n774: ZW = zw_cellmix_b(38u64, n312, 1542469173u64);
    let n775: ZW = zw_cellmix_b(38u64, n312, 668265263u64);
    let n776: ZW = zw_add(zw_splat(0u64), n774);
    let n777: ZW = zw_add(zw_splat(0u64), n775);
    let n778: ZW = zw_cellmix_n(39u64, n311, 1542469173u64);
    let n779: ZW = zw_cellmix_n(39u64, n311, 668265263u64);
    let n780: ZW = zw_add(n776, n778);
    let n781: ZW = zw_add(n777, n779);
    let n782: ZW = zw_add(n780, n372);
    let n783: ZW = zw_add(n781, n373);
    let n784: ZW = zw_add(n782, n376);
    let n785: ZW = zw_add(n783, n377);
    let n786: ZW = zw_add(n784, n380);
    let n787: ZW = zw_add(n785, n381);
    let n788: ZW = zw_cellmix_n(87u64, n310, 1542469173u64);
    let n789: ZW = zw_cellmix_n(87u64, n310, 668265263u64);
    let n790: ZW = zw_add(n786, n788);
    let n791: ZW = zw_add(n787, n789);
    let n792: ZW = zw_add(n790, n392);
    let n793: ZW = zw_add(n791, n393);
    let n794: ZW = zw_add(n790, n510);
    let n795: ZW = zw_add(n791, n511);
    let n796: ZW = zw_cellmix_n(20u64, n321, 1542469173u64);
    let n797: ZW = zw_cellmix_n(20u64, n321, 668265263u64);
    let n798: ZW = zw_add(zw_splat(0u64), n796);
    let n799: ZW = zw_add(zw_splat(0u64), n797);
    let n800: ZW = zw_add(n798, n368);
    let n801: ZW = zw_add(n799, n369);
    let n802: ZW = zw_add(n800, n372);
    let n803: ZW = zw_add(n801, n373);
    let n804: ZW = zw_add(n802, n376);
    let n805: ZW = zw_add(n803, n377);
    let n806: ZW = zw_add(n804, n380);
    let n807: ZW = zw_add(n805, n381);
    let n808: ZW = zw_add(n806, n384);
    let n809: ZW = zw_add(n807, n385);
    let n810: ZW = zw_cellmix_n(233u64, n361, 1542469173u64);
    let n811: ZW = zw_cellmix_n(233u64, n361, 668265263u64);
    let n812: ZW = zw_add(n808, n810);
    let n813: ZW = zw_add(n809, n811);
    let n814: ZW = zw_cellmix_n(244u64, n338, 1542469173u64);
    let n815: ZW = zw_cellmix_n(244u64, n338, 668265263u64);
    let n816: ZW = zw_add(n812, n814);
    let n817: ZW = zw_add(n813, n815);
    let n818: ZW = zw_cellmix_n(245u64, n362, 1542469173u64);
    let n819: ZW = zw_cellmix_n(245u64, n362, 668265263u64);
    let n820: ZW = zw_add(n816, n818);
    let n821: ZW = zw_add(n817, n819);
    let n822: ZW = zw_cellmix_n(249u64, n363, 1542469173u64);
    let n823: ZW = zw_cellmix_n(249u64, n363, 668265263u64);
    let n824: ZW = zw_add(n820, n822);
    let n825: ZW = zw_add(n821, n823);
    let n826: ZW = zw_cellmix_n(269u64, n364, 1542469173u64);
    let n827: ZW = zw_cellmix_n(269u64, n364, 668265263u64);
    let n828: ZW = zw_add(n824, n826);
    let n829: ZW = zw_add(n825, n827);
    let n830: ZW = zw_cellmix_n(271u64, n365, 1542469173u64);
    let n831: ZW = zw_cellmix_n(271u64, n365, 668265263u64);
    let n832: ZW = zw_add(n828, n830);
    let n833: ZW = zw_add(n829, n831);
    let ok_v0_b0: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v0_b0: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b0: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v1_b1: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v1_b1: bool = !n65 || !n64 || !n63 || !n62;
    let live_v1_b1: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v2_b2: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v2_b2: bool = !n65 || !n64 || !n63 || !n62;
    let live_v2_b2: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v16_b3: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v16_b3: bool = !n65 || !n64 || !n63 || !n62;
    let live_v16_b3: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v17_b4: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v17_b4: bool = !n65 || !n64 || !n63 || !n62;
    let live_v17_b4: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v18_b5: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v18_b5: bool = !n65 || !n64 || !n63 || !n62;
    let live_v18_b5: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v32_b6: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v32_b6: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b6: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v33_b7: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v33_b7: bool = !n65 || !n64 || !n63 || !n62;
    let live_v33_b7: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v34_b8: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v34_b8: bool = !n65 || !n64 || !n63 || !n62;
    let live_v34_b8: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v36_b9: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v36_b9: bool = !n65 || !n64 || !n63 || !n62;
    let live_v36_b9: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v37_b10: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v37_b10: bool = !n65 || !n64 || !n63 || !n62;
    let live_v37_b10: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v38_b11: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v38_b11: bool = !n65 || !n64 || !n63 || !n62;
    let live_v38_b11: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v40_b12: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v40_b12: bool = !n65 || !n64 || !n63 || !n62;
    let live_v40_b12: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v41_b13: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v41_b13: bool = !n65 || !n64 || !n63 || !n62;
    let live_v41_b13: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v42_b14: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v42_b14: bool = !n65 || !n64 || !n63 || !n62;
    let live_v42_b14: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v48_b15: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v48_b15: bool = !n65 || !n64 || !n63 || !n62;
    let live_v48_b15: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v49_b16: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v49_b16: bool = !n65 || !n64 || !n63 || !n62;
    let live_v49_b16: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v50_b17: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v50_b17: bool = !n65 || !n64 || !n63 || !n62;
    let live_v50_b17: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v52_b18: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v52_b18: bool = !n65 || !n64 || !n63 || !n62;
    let live_v52_b18: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v53_b19: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v53_b19: bool = !n65 || !n64 || !n63 || !n62;
    let live_v53_b19: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v54_b20: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v54_b20: bool = !n65 || !n64 || !n63 || !n62;
    let live_v54_b20: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v56_b21: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v56_b21: bool = !n65 || !n64 || !n63 || !n62;
    let live_v56_b21: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v57_b22: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v57_b22: bool = !n65 || !n64 || !n63 || !n62;
    let live_v57_b22: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v58_b23: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n243);
    let bd_v58_b23: bool = !n65 || !n64 || !n63 || !n62;
    let live_v58_b23: u16 = ALL & zb_holds(n242) & zb_holds(n244) & zb_holds(n270);
    let ok_v0_b24: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n301);
    let bd_v0_b24: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b24: u16 = ALL & zb_holds(n270) & zb_holds(n300);
    let ok_v32_b25: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n301);
    let bd_v32_b25: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b25: u16 = ALL & zb_holds(n270) & zb_holds(n300);
    let ok_v0_b26: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n314);
    let bd_v0_b26: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b26: u16 = ALL & zb_holds(n313);
    let ok_v32_b27: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n314);
    let bd_v32_b27: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b27: u16 = ALL & zb_holds(n313);
    let ok_v0_b28: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58);
    let bd_v0_b28: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b28: u16 = ALL & zb_holds(n366);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n77,
        c86: n112,
        c254: n132,
        c85: n113,
    };
    let sh1 = KShared1 {
        c87: n299,
        c84: n77,
        c86: n112,
        c85: n113,
    };
    let sh2 = KShared2 {
        c87: n310,
        c39: n311,
        c84: n77,
        c86: n112,
        c85: n113,
        c38: n312,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n77,
        c20: n321,
        c86: n112,
        c233: n361,
        c269: n364,
        c271: n365,
        c244: n338,
        c245: n362,
        c249: n363,
        c85: n113,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
    let mut take_0_2: u16 = 0;
    let mut take_0_3: u16 = 0;
    let mut take_0_4: u16 = 0;
    let mut take_0_5: u16 = 0;
    let mut take_0_6: u16 = 0;
    let mut take_0_7: u16 = 0;
    let mut take_0_8: u16 = 0;
    let mut take_0_9: u16 = 0;
    let mut take_0_10: u16 = 0;
    let mut take_0_11: u16 = 0;
    let mut take_0_12: u16 = 0;
    let mut take_0_13: u16 = 0;
    let mut take_0_14: u16 = 0;
    let mut take_0_15: u16 = 0;
    let mut take_0_16: u16 = 0;
    let mut take_0_17: u16 = 0;
    let mut take_0_18: u16 = 0;
    let mut take_0_19: u16 = 0;
    let mut take_0_20: u16 = 0;
    let mut take_0_21: u16 = 0;
    let mut take_0_22: u16 = 0;
    let mut take_0_23: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 29 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 1] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n254,
        h1: n450, h2: n451,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(true),
        c239: n249,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n276,
        c281: n254,
        h1: n460, h2: n461,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n279,
        c281: n254,
        h1: n466, h2: n467,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n267,
        c281: n268,
        h1: n494, h2: n495,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(true),
        c239: n266,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n278,
        c281: n268,
        h1: n502, h2: n503,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c268: zn_splat(P8::from_raw(0i32)),
        c269: zn_splat(P8::from_raw(0i32)),
        c234: zn_splat(P8::from_raw(-65536i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n281,
        c281: n268,
        h1: n508, h2: n509,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n560, h2: n561,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n574, h2: n575,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n580, h2: n581,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(98304i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n602, h2: n603,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n618, h2: n619,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n630, h2: n631,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(98304i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n642, h2: n643,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n652, h2: n653,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n249,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n660, h2: n661,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n680, h2: n681,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n690, h2: n691,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(98304i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n694, h2: n695,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(98304i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n708, h2: n709,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n720, h2: n721,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(-98304i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n730, h2: n731,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(98304i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n738, h2: n739,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(-131072i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(true),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n746, h2: n747,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c268: zn_splat(P8::from_raw(69510i32)),
        c269: zn_splat(P8::from_raw(69510i32)),
        c234: zn_splat(P8::from_raw(655360i32)),
        c270: zn_splat(P8::from_raw(131072i32)),
        c271: zn_splat(P8::from_raw(131072i32)),
        c236: zn_splat(P8::from_raw(262144i32)),
        c237: zn_splat(P8::from_raw(0i32)),
        c272: zb_splat(false),
        c239: n266,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n754, h2: n755,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n768, h2: n769,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: n772, h2: n773,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n792, h2: n793,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: zn_splat(P8::from_raw(131072i32)),
        h1: n794, h2: n795,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n832, h2: n833,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
