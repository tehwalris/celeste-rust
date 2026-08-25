// GENERATED from a TRACED frame (shape 2). Do not edit.
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
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
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
    let n144: ZB = zb_and(n119, n142);
    let n145: ZB = zb_and(n119, n143);
    let n146: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n141);
    let n147: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n146);
    let n148: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n147);
    let n149: ZB = zb_not(n148);
    let n150: ZB = zb_and(n144, n148);
    let n151: ZB = zb_and(n144, n149);
    let n152: ZN = zn_rem(n138, zn_splat(P8::from_raw(524288i32)));
    let n153: ZB = zn_ge(n152, zn_splat(P8::from_raw(393216i32)));
    let n154: ZB = zn_lt(n152, zn_splat(P8::from_raw(393216i32)));
    let n155: ZB = zb_and(n150, n154);
    let n156: ZB = zb_and(n150, n153);
    let n157: ZN = zn_mul(n141, zn_splat(P8::from_raw(524288i32)));
    let n158: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n157);
    let n159: ZB = zn_eq(n137, n158);
    let n160: ZB = zb_or(n155, n156);
    let n161: ZB = zb_or(n153, n159);
    let n162: ZB = zb_or(n151, n160);
    let n163: ZB = zb_and(n148, n161);
    let n164: ZB = zb_not(n163);
    let n165: ZB = zb_and(n162, n163);
    let n166: ZB = zb_and(n162, n164);
    let n167: ZB = zb_or(n165, n166);
    let n168: ZB = zb_and(n164, n167);
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n147);
    let n170: ZB = zb_not(n169);
    let n171: ZB = zb_and(n168, n169);
    let n172: ZB = zb_and(n168, n170);
    let n173: ZN = zn_rem(n133, zn_splat(P8::from_raw(524288i32)));
    let n174: ZB = zn_le(n173, zn_splat(P8::from_raw(131072i32)));
    let n175: ZB = zb_or(n171, n172);
    let n176: ZB = zb_and(n169, n174);
    let n177: ZB = zb_not(n176);
    let n178: ZB = zb_and(n175, n176);
    let n179: ZB = zb_and(n175, n177);
    let n180: ZB = zb_or(n178, n179);
    let n181: ZB = zb_and(n177, n180);
    let n182: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n147);
    let n183: ZB = zb_not(n182);
    let n184: ZB = zb_and(n181, n182);
    let n185: ZB = zb_and(n181, n183);
    let n186: ZB = zb_or(n184, n185);
    let n187: ZB = zb_and(n182, n186);
    let n188: ZB = zb_and(n183, n186);
    let n189: ZB = zb_or(n187, n188);
    let n190: ZB = zb_and(n183, n189);
    let n191: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n147);
    let n192: ZB = zb_not(n191);
    let n193: ZB = zb_and(n190, n191);
    let n194: ZB = zb_and(n190, n192);
    let n195: ZB = zb_or(n193, n194);
    let n196: ZB = zb_and(n191, n195);
    let n197: ZB = zb_and(n192, n195);
    let n198: ZB = zb_or(n196, n197);
    let n199: ZB = zb_and(n192, n198);
    let n200: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n136);
    let n201: ZB = zn_le(n200, n140);
    let n202: ZB = zn_gt(n200, n140);
    let n203: ZB = zb_and(n199, n201);
    let n204: ZB = zb_and(n199, n202);
    let n205: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n200);
    let n206: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n205);
    let n207: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n206);
    let n208: ZB = zb_not(n207);
    let n209: ZB = zb_and(n203, n207);
    let n210: ZB = zb_and(n203, n208);
    let n211: ZB = zb_and(n154, n209);
    let n212: ZB = zb_and(n153, n209);
    let n213: ZN = zn_mul(n200, zn_splat(P8::from_raw(524288i32)));
    let n214: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n213);
    let n215: ZB = zn_eq(n137, n214);
    let n216: ZB = zb_or(n211, n212);
    let n217: ZB = zb_or(n153, n215);
    let n218: ZB = zb_or(n210, n216);
    let n219: ZB = zb_and(n207, n217);
    let n220: ZB = zb_not(n219);
    let n221: ZB = zb_and(n218, n219);
    let n222: ZB = zb_and(n218, n220);
    let n223: ZB = zb_or(n221, n222);
    let n224: ZB = zb_and(n220, n223);
    let n225: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n206);
    let n226: ZB = zb_not(n225);
    let n227: ZB = zb_and(n224, n225);
    let n228: ZB = zb_and(n224, n226);
    let n229: ZB = zb_or(n227, n228);
    let n230: ZB = zb_and(n174, n225);
    let n231: ZB = zb_not(n230);
    let n232: ZB = zb_and(n229, n230);
    let n233: ZB = zb_and(n229, n231);
    let n234: ZB = zb_or(n232, n233);
    let n235: ZB = zb_and(n231, n234);
    let n236: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n206);
    let n237: ZB = zb_not(n236);
    let n238: ZB = zb_and(n235, n236);
    let n239: ZB = zb_and(n235, n237);
    let n240: ZB = zb_or(n238, n239);
    let n241: ZB = zb_and(n236, n240);
    let n242: ZB = zb_and(n237, n240);
    let n243: ZB = zb_or(n241, n242);
    let n244: ZB = zb_and(n237, n243);
    let n245: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n206);
    let n246: ZB = zb_not(n245);
    let n247: ZB = zb_and(n244, n245);
    let n248: ZB = zb_and(n244, n246);
    let n249: ZB = zb_or(n247, n248);
    let n250: ZB = zb_and(n245, n249);
    let n251: ZB = zb_and(n246, n249);
    let n252: ZB = zb_or(n250, n251);
    let n253: ZB = zb_and(n246, n252);
    let n254: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n136);
    let n255: ZB = zn_le(n254, n140);
    let n256: ZB = zn_gt(n254, n140);
    let n257: ZB = zb_and(n253, n255);
    let n258: ZB = zb_and(n253, n256);
    let n259: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n254);
    let n260: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n259);
    let n261: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n260);
    let n262: ZB = zb_not(n261);
    let n263: ZB = zb_and(n257, n261);
    let n264: ZB = zb_and(n257, n262);
    let n265: ZB = zb_and(n154, n263);
    let n266: ZB = zb_and(n153, n263);
    let n267: ZN = zn_mul(n254, zn_splat(P8::from_raw(524288i32)));
    let n268: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n267);
    let n269: ZB = zn_eq(n137, n268);
    let n270: ZB = zb_or(n265, n266);
    let n271: ZB = zb_or(n153, n269);
    let n272: ZB = zb_or(n264, n270);
    let n273: ZB = zb_and(n261, n271);
    let n274: ZB = zb_not(n273);
    let n275: ZB = zb_and(n272, n273);
    let n276: ZB = zb_and(n272, n274);
    let n277: ZB = zb_or(n275, n276);
    let n278: ZB = zb_and(n274, n277);
    let n279: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n260);
    let n280: ZB = zb_not(n279);
    let n281: ZB = zb_and(n278, n279);
    let n282: ZB = zb_and(n278, n280);
    let n283: ZB = zb_or(n281, n282);
    let n284: ZB = zb_and(n174, n279);
    let n285: ZB = zb_not(n284);
    let n286: ZB = zb_and(n283, n284);
    let n287: ZB = zb_and(n283, n285);
    let n288: ZB = zb_or(n286, n287);
    let n289: ZB = zb_and(n285, n288);
    let n290: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n260);
    let n291: ZB = zb_not(n290);
    let n292: ZB = zb_and(n289, n290);
    let n293: ZB = zb_and(n289, n291);
    let n294: ZB = zb_or(n292, n293);
    let n295: ZB = zb_and(n290, n294);
    let n296: ZB = zb_and(n291, n294);
    let n297: ZB = zb_or(n295, n296);
    let n298: ZB = zb_and(n291, n297);
    let n299: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n260);
    let n300: ZB = zb_not(n299);
    let n301: ZB = zb_and(n298, n299);
    let n302: ZB = zb_and(n298, n300);
    let n303: ZB = zb_or(n301, n302);
    let n304: ZB = zb_and(n299, n303);
    let n305: ZB = zb_and(n300, n303);
    let n306: ZB = zb_or(n304, n305);
    let n307: ZB = zb_and(n300, n306);
    let n308: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n136);
    let n309: ZB = zn_gt(n308, n140);
    let n310: ZB = zb_or(n258, n307);
    let n311: ZB = zb_or(n256, n309);
    let n312: ZB = zb_or(n204, n310);
    let n313: ZB = zb_or(n202, n311);
    let n314: ZB = zb_or(n145, n312);
    let n315: ZB = zb_or(n143, n313);
    let n316: ZB = zn_le(n132, zn_splat(P8::from_raw(8388608i32)));
    let n317: ZB = zb_and(n314, n316);
    let n318: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n133);
    let n319: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n318, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n317, n320);
    let n322: ZB = zb_and(n317, n319);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZB = zb_and(n320, n323);
    let n325: ZB = zb_and(n319, n323);
    let n326: ZB = zb_or(n324, n325);
    let n327: ZB = zb_and(n319, n326);
    let n328: ZB = zb_and(n320, n326);
    let n329: ZN = zsel_n(n319, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n330: ZB = zb_or(n327, n328);
    let n331: ZB = zb_and(n320, n330);
    let n332: ZB = zb_and(n319, n330);
    let n333: ZN = zsel_n(n320, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n334: ZB = zb_or(n331, n332);
    let n335: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n333);
    let n336: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n333);
    let n337: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n133);
    let n338: ZB = zb_and(n320, n334);
    let n339: ZB = zb_and(n319, n334);
    let n340: ZN = zsel_n(n320, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n341: ZB = zb_or(n338, n339);
    let n342: ZB = zn_gt(n329, zn_splat(P8::from_raw(0i32)));
    let n343: ZB = zn_le(n329, zn_splat(P8::from_raw(0i32)));
    let n344: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n345: ZB = zb_not(n344);
    let n346: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n347: ZB = zb_not(n346);
    let n348: ZN = zsel_n(n346, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n349: ZN = zsel_n(n344, zn_splat(P8::from_raw(-65536i32)), n348);
    let n350: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n349);
    let n351: ZB = zb_not(n350);
    let n352: ZN = zn_neg(n349);
    let n353: ZN = zn_mul(n352, zn_splat(P8::from_raw(131072i32)));
    let n354: ZN = zsel_n(n351, n353, zn_splat(P8::from_raw(0i32)));
    let n355: ZN = zsel_n(n351, zn_splat(P8::from_raw(-131072i32)), n340);
    let n356: ZN = zsel_n(n342, zn_splat(P8::from_raw(0i32)), n329);
    let n357: ZN = zsel_n(n342, zn_splat(P8::from_raw(0i32)), n354);
    let n358: ZN = zsel_n(n342, zn_splat(P8::from_raw(-131072i32)), n355);
    let n359: ZB = zn_lt(n132, zn_splat(P8::from_raw(-262144i32)));
    let n360: ZB = zn_ge(n132, zn_splat(P8::from_raw(-262144i32)));
    let n361: ZB = zb_and(n341, n359);
    let n362: ZB = zb_and(n341, n360);
    let n363: ZB = zb_or(n361, n362);
    let n369: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n335);
    let n370: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n371: ZB = zb_not(n370);
    let n372: ZB = zb_and(n334, n371);
    let n373: ZB = zb_and(n334, n370);
    let n374: ZB = zb_or(n372, n373);
    let n375: ZB = zb_and(n371, n374);
    let n376: ZB = zb_and(n370, n374);
    let n377: ZB = zb_or(n375, n376);
    let n378: ZB = zb_and(n370, n377);
    let n379: ZB = zb_and(n371, n377);
    let n380: ZB = zb_or(n378, n379);
    let n381: ZB = zb_and(n370, n380);
    let n382: ZB = zb_and(n371, n380);
    let n383: ZB = zb_or(n381, n382);
    let n384: ZB = zb_and(n320, n383);
    let n385: ZB = zb_and(n319, n383);
    let n386: ZB = zb_or(n384, n385);
    let n387: ZN = zsel_n(n351, n353, n369);
    let n388: ZN = zsel_n(n342, n369, n387);
    let n389: ZB = zb_and(n359, n386);
    let n390: ZB = zb_and(n360, n386);
    let n391: ZB = zb_or(n389, n390);
    let n394: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n336);
    let n395: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n396: ZB = zb_not(n395);
    let n397: ZB = zb_and(n334, n396);
    let n398: ZB = zb_and(n334, n395);
    let n399: ZB = zb_or(n397, n398);
    let n400: ZB = zb_and(n396, n399);
    let n401: ZB = zb_and(n395, n399);
    let n402: ZB = zb_or(n400, n401);
    let n403: ZB = zb_and(n395, n402);
    let n404: ZB = zb_and(n396, n402);
    let n405: ZB = zb_or(n403, n404);
    let n406: ZB = zb_and(n395, n405);
    let n407: ZB = zb_and(n396, n405);
    let n408: ZB = zb_or(n406, n407);
    let n409: ZB = zb_and(n320, n408);
    let n410: ZB = zb_and(n319, n408);
    let n411: ZB = zb_or(n409, n410);
    let n412: ZN = zsel_n(n351, n353, n394);
    let n413: ZN = zsel_n(n342, n394, n412);
    let n414: ZB = zb_and(n359, n411);
    let n415: ZB = zb_and(n360, n411);
    let n416: ZB = zb_or(n414, n415);
    let n419: ZB = zb_and(n341, n342);
    let n420: ZB = zb_and(n341, n343);
    let n421: ZB = zb_and(n345, n420);
    let n422: ZB = zb_and(n344, n420);
    let n423: ZB = zb_or(n421, n422);
    let n424: ZB = zb_and(n345, n423);
    let n425: ZB = zb_and(n344, n423);
    let n426: ZB = zb_or(n424, n425);
    let n427: ZB = zb_and(n344, n426);
    let n428: ZB = zb_and(n345, n426);
    let n429: ZB = zb_and(n347, n428);
    let n430: ZB = zb_and(n346, n428);
    let n431: ZB = zb_or(n429, n430);
    let n432: ZB = zb_and(n347, n431);
    let n433: ZB = zb_and(n346, n431);
    let n434: ZB = zb_or(n432, n433);
    let n435: ZB = zb_and(n346, n434);
    let n436: ZB = zb_and(n347, n434);
    let n437: ZB = zb_or(n435, n436);
    let n438: ZB = zb_or(n427, n437);
    let n439: ZB = zb_and(n351, n438);
    let n440: ZB = zb_and(n350, n438);
    let n441: ZB = zb_or(n439, n440);
    let n442: ZB = zb_or(n419, n441);
    let n443: ZB = zb_and(n359, n442);
    let n444: ZB = zb_and(n360, n442);
    let n445: ZB = zb_or(n443, n444);
    let n448: ZB = zb_and(n342, n386);
    let n449: ZB = zb_and(n343, n386);
    let n450: ZB = zb_and(n345, n449);
    let n451: ZB = zb_and(n344, n449);
    let n452: ZB = zb_or(n450, n451);
    let n453: ZB = zb_and(n345, n452);
    let n454: ZB = zb_and(n344, n452);
    let n455: ZB = zb_or(n453, n454);
    let n456: ZB = zb_and(n344, n455);
    let n457: ZB = zb_and(n345, n455);
    let n458: ZB = zb_and(n347, n457);
    let n459: ZB = zb_and(n346, n457);
    let n460: ZB = zb_or(n458, n459);
    let n461: ZB = zb_and(n347, n460);
    let n462: ZB = zb_and(n346, n460);
    let n463: ZB = zb_or(n461, n462);
    let n464: ZB = zb_and(n346, n463);
    let n465: ZB = zb_and(n347, n463);
    let n466: ZB = zb_or(n464, n465);
    let n467: ZB = zb_or(n456, n466);
    let n468: ZB = zb_and(n351, n467);
    let n469: ZB = zb_and(n350, n467);
    let n470: ZB = zb_or(n468, n469);
    let n471: ZB = zb_or(n448, n470);
    let n472: ZB = zb_and(n359, n471);
    let n473: ZB = zb_and(n360, n471);
    let n474: ZB = zb_or(n472, n473);
    let n477: ZB = zb_and(n342, n411);
    let n478: ZB = zb_and(n343, n411);
    let n479: ZB = zb_and(n345, n478);
    let n480: ZB = zb_and(n344, n478);
    let n481: ZB = zb_or(n479, n480);
    let n482: ZB = zb_and(n345, n481);
    let n483: ZB = zb_and(n344, n481);
    let n484: ZB = zb_or(n482, n483);
    let n485: ZB = zb_and(n344, n484);
    let n486: ZB = zb_and(n345, n484);
    let n487: ZB = zb_and(n347, n486);
    let n488: ZB = zb_and(n346, n486);
    let n489: ZB = zb_or(n487, n488);
    let n490: ZB = zb_and(n347, n489);
    let n491: ZB = zb_and(n346, n489);
    let n492: ZB = zb_or(n490, n491);
    let n493: ZB = zb_and(n346, n492);
    let n494: ZB = zb_and(n347, n492);
    let n495: ZB = zb_or(n493, n494);
    let n496: ZB = zb_or(n485, n495);
    let n497: ZB = zb_and(n351, n496);
    let n498: ZB = zb_and(n350, n496);
    let n499: ZB = zb_or(n497, n498);
    let n500: ZB = zb_or(n477, n499);
    let n501: ZB = zb_and(n359, n500);
    let n502: ZB = zb_and(n360, n500);
    let n503: ZB = zb_or(n501, n502);
    let n507: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n508: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n507);
    let n509: ZB = zb_and(n163, n167);
    let n510: ZB = zb_and(n176, n180);
    let n511: ZB = zb_and(n182, n189);
    let n512: ZB = zb_and(n191, n198);
    let n513: ZB = zb_or(n511, n512);
    let n514: ZB = zb_or(n510, n513);
    let n515: ZB = zb_or(n509, n514);
    let n516: ZB = zb_and(n219, n223);
    let n517: ZB = zb_and(n230, n234);
    let n518: ZB = zb_and(n236, n243);
    let n519: ZB = zb_and(n245, n252);
    let n520: ZB = zb_or(n518, n519);
    let n521: ZB = zb_or(n517, n520);
    let n522: ZB = zb_or(n516, n521);
    let n523: ZB = zb_and(n273, n277);
    let n524: ZB = zb_and(n284, n288);
    let n525: ZB = zb_and(n290, n297);
    let n526: ZB = zb_and(n299, n306);
    let n527: ZB = zb_or(n525, n526);
    let n528: ZB = zb_or(n524, n527);
    let n529: ZB = zb_or(n523, n528);
    let n530: ZB = zb_or(n522, n529);
    let n531: ZB = zb_or(n515, n530);
    let n532: ZB = zn_gt(n132, zn_splat(P8::from_raw(8388608i32)));
    let n533: ZB = zb_and(n531, n532);
    let n534: ZB = zb_and(n316, n531);
    let n535: ZN = zsel_n(n532, n508, n507);
    let n536: ZB = zb_or(n533, n534);
    let n537: ZB = zb_and(n314, n532);
    let n538: ZN = zsel_n(n536, n535, n507);
    let n539: ZB = zb_or(n536, n537);
    let n540: ZB = zb_or(n315, n536);
    let n541: ZB = zb_and(n320, n539);
    let n542: ZB = zb_and(n319, n539);
    let n543: ZB = zb_or(n541, n542);
    let n544: ZB = zb_and(n320, n543);
    let n545: ZB = zb_and(n319, n543);
    let n546: ZB = zb_or(n544, n545);
    let n547: ZB = zb_and(n319, n546);
    let n548: ZB = zb_and(n320, n546);
    let n549: ZB = zb_or(n547, n548);
    let n550: ZB = zb_and(n320, n549);
    let n551: ZB = zb_and(n319, n549);
    let n552: ZB = zb_or(n550, n551);
    let n553: ZB = zb_and(n320, n552);
    let n554: ZB = zb_and(n319, n552);
    let n555: ZB = zb_or(n553, n554);
    let n556: ZB = zb_and(n359, n555);
    let n557: ZB = zb_and(n360, n555);
    let n558: ZB = zb_or(n556, n557);
    let n562: ZB = zb_and(n371, n552);
    let n563: ZB = zb_and(n370, n552);
    let n564: ZB = zb_or(n562, n563);
    let n565: ZB = zb_and(n371, n564);
    let n566: ZB = zb_and(n370, n564);
    let n567: ZB = zb_or(n565, n566);
    let n568: ZB = zb_and(n370, n567);
    let n569: ZB = zb_and(n371, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n370, n570);
    let n572: ZB = zb_and(n371, n570);
    let n573: ZB = zb_or(n571, n572);
    let n574: ZB = zb_and(n320, n573);
    let n575: ZB = zb_and(n319, n573);
    let n576: ZB = zb_or(n574, n575);
    let n577: ZB = zb_and(n359, n576);
    let n578: ZB = zb_and(n360, n576);
    let n579: ZB = zb_or(n577, n578);
    let n582: ZB = zb_and(n396, n552);
    let n583: ZB = zb_and(n395, n552);
    let n584: ZB = zb_or(n582, n583);
    let n585: ZB = zb_and(n396, n584);
    let n586: ZB = zb_and(n395, n584);
    let n587: ZB = zb_or(n585, n586);
    let n588: ZB = zb_and(n395, n587);
    let n589: ZB = zb_and(n396, n587);
    let n590: ZB = zb_or(n588, n589);
    let n591: ZB = zb_and(n395, n590);
    let n592: ZB = zb_and(n396, n590);
    let n593: ZB = zb_or(n591, n592);
    let n594: ZB = zb_and(n320, n593);
    let n595: ZB = zb_and(n319, n593);
    let n596: ZB = zb_or(n594, n595);
    let n597: ZB = zb_and(n359, n596);
    let n598: ZB = zb_and(n360, n596);
    let n599: ZB = zb_or(n597, n598);
    let n602: ZB = zb_and(n342, n555);
    let n603: ZB = zb_and(n343, n555);
    let n604: ZB = zb_and(n345, n603);
    let n605: ZB = zb_and(n344, n603);
    let n606: ZB = zb_or(n604, n605);
    let n607: ZB = zb_and(n345, n606);
    let n608: ZB = zb_and(n344, n606);
    let n609: ZB = zb_or(n607, n608);
    let n610: ZB = zb_and(n344, n609);
    let n611: ZB = zb_and(n345, n609);
    let n612: ZB = zb_and(n347, n611);
    let n613: ZB = zb_and(n346, n611);
    let n614: ZB = zb_or(n612, n613);
    let n615: ZB = zb_and(n347, n614);
    let n616: ZB = zb_and(n346, n614);
    let n617: ZB = zb_or(n615, n616);
    let n618: ZB = zb_and(n346, n617);
    let n619: ZB = zb_and(n347, n617);
    let n620: ZB = zb_or(n618, n619);
    let n621: ZB = zb_or(n610, n620);
    let n622: ZB = zb_and(n351, n621);
    let n623: ZB = zb_and(n350, n621);
    let n624: ZB = zb_or(n622, n623);
    let n625: ZB = zb_or(n602, n624);
    let n626: ZB = zb_and(n359, n625);
    let n627: ZB = zb_and(n360, n625);
    let n628: ZB = zb_or(n626, n627);
    let n631: ZB = zb_and(n342, n576);
    let n632: ZB = zb_and(n343, n576);
    let n633: ZB = zb_and(n345, n632);
    let n634: ZB = zb_and(n344, n632);
    let n635: ZB = zb_or(n633, n634);
    let n636: ZB = zb_and(n345, n635);
    let n637: ZB = zb_and(n344, n635);
    let n638: ZB = zb_or(n636, n637);
    let n639: ZB = zb_and(n344, n638);
    let n640: ZB = zb_and(n345, n638);
    let n641: ZB = zb_and(n347, n640);
    let n642: ZB = zb_and(n346, n640);
    let n643: ZB = zb_or(n641, n642);
    let n644: ZB = zb_and(n347, n643);
    let n645: ZB = zb_and(n346, n643);
    let n646: ZB = zb_or(n644, n645);
    let n647: ZB = zb_and(n346, n646);
    let n648: ZB = zb_and(n347, n646);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_or(n639, n649);
    let n651: ZB = zb_and(n351, n650);
    let n652: ZB = zb_and(n350, n650);
    let n653: ZB = zb_or(n651, n652);
    let n654: ZB = zb_or(n631, n653);
    let n655: ZB = zb_and(n359, n654);
    let n656: ZB = zb_and(n360, n654);
    let n657: ZB = zb_or(n655, n656);
    let n660: ZB = zb_and(n342, n596);
    let n661: ZB = zb_and(n343, n596);
    let n662: ZB = zb_and(n345, n661);
    let n663: ZB = zb_and(n344, n661);
    let n664: ZB = zb_or(n662, n663);
    let n665: ZB = zb_and(n345, n664);
    let n666: ZB = zb_and(n344, n664);
    let n667: ZB = zb_or(n665, n666);
    let n668: ZB = zb_and(n344, n667);
    let n669: ZB = zb_and(n345, n667);
    let n670: ZB = zb_and(n347, n669);
    let n671: ZB = zb_and(n346, n669);
    let n672: ZB = zb_or(n670, n671);
    let n673: ZB = zb_and(n347, n672);
    let n674: ZB = zb_and(n346, n672);
    let n675: ZB = zb_or(n673, n674);
    let n676: ZB = zb_and(n346, n675);
    let n677: ZB = zb_and(n347, n675);
    let n678: ZB = zb_or(n676, n677);
    let n679: ZB = zb_or(n668, n678);
    let n680: ZB = zb_and(n351, n679);
    let n681: ZB = zb_and(n350, n679);
    let n682: ZB = zb_or(n680, n681);
    let n683: ZB = zb_or(n660, n682);
    let n684: ZB = zb_and(n359, n683);
    let n685: ZB = zb_and(n360, n683);
    let n686: ZB = zb_or(n684, n685);
    let n694: ZB = zb_and(n359, n363);
    let n695: ZB = zb_and(n359, n558);
    let n696: ZN = zsel_n(n694, r_c87, n538);
    let n697: ZN = zsel_n(n694, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n698: ZB = zb_not(n694);
    let n699: ZB = zb_or(n694, n695);
    let n700: ZB = zsel_b(n694, n315, n540);
    let n703: ZB = zb_and(n359, n391);
    let n704: ZB = zb_and(n359, n579);
    let n705: ZN = zsel_n(n703, r_c87, n538);
    let n706: ZN = zsel_n(n703, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n707: ZB = zb_not(n703);
    let n708: ZB = zb_or(n703, n704);
    let n709: ZB = zsel_b(n703, n315, n540);
    let n712: ZB = zb_and(n359, n416);
    let n713: ZB = zb_and(n359, n599);
    let n714: ZN = zsel_n(n712, r_c87, n538);
    let n715: ZN = zsel_n(n712, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n716: ZB = zb_not(n712);
    let n717: ZB = zb_or(n712, n713);
    let n718: ZB = zsel_b(n712, n315, n540);
    let n721: ZB = zb_and(n359, n445);
    let n722: ZB = zb_and(n359, n628);
    let n723: ZN = zsel_n(n721, r_c87, n538);
    let n724: ZN = zsel_n(n721, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n725: ZB = zb_not(n721);
    let n726: ZB = zb_or(n721, n722);
    let n727: ZB = zsel_b(n721, n315, n540);
    let n730: ZB = zb_and(n359, n474);
    let n731: ZB = zb_and(n359, n657);
    let n732: ZN = zsel_n(n730, r_c87, n538);
    let n733: ZN = zsel_n(n730, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n734: ZB = zb_not(n730);
    let n735: ZB = zb_or(n730, n731);
    let n736: ZB = zsel_b(n730, n315, n540);
    let n739: ZB = zb_and(n359, n503);
    let n740: ZB = zb_and(n359, n686);
    let n741: ZN = zsel_n(n739, r_c87, n538);
    let n742: ZN = zsel_n(n739, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n743: ZB = zb_not(n739);
    let n744: ZB = zb_or(n739, n740);
    let n745: ZB = zsel_b(n739, n315, n540);
    let n749: ZB = zb_not(n83);
    let n750: ZB = zn_ge(n84, zn_splat(P8::from_raw(0i32)));
    let n751: ZN = zsel_n(n83, n84, r_c233);
    let n752: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n753: ZN = zsel_n(n120, n752, r_c20);
    let n754: ZB = zn_gt(n753, zn_splat(P8::from_raw(0i32)));
    let n755: ZB = zn_le(n753, zn_splat(P8::from_raw(0i32)));
    let n756: ZB = zb_and(n69, n96);
    let n757: ZB = zb_and(n81, n116);
    let n758: ZN = zn_add(r_c271, zn_splat(P8::from_raw(32768i32)));
    let n759: ZB = zn_gt(n758, zn_splat(P8::from_raw(0i32)));
    let n760: ZB = zn_le(n758, zn_splat(P8::from_raw(0i32)));
    let n761: ZB = zb_and(n757, n759);
    let n762: ZB = zb_and(n757, n760);
    let n763: ZB = zn_gt(r_c233, zn_splat(P8::from_raw(0i32)));
    let n764: ZB = zb_or(n761, n762);
    let n765: ZB = zb_and(n759, n763);
    let n766: ZB = zb_not(n765);
    let n767: ZB = zb_and(n764, n765);
    let n768: ZB = zb_and(n764, n766);
    let n769: ZN = zsel_n(n765, n84, r_c233);
    let n770: ZN = zsel_n(n765, zn_splat(P8::from_raw(0i32)), n758);
    let n771: ZB = zb_or(n767, n768);
    let n772: ZB = zn_gt(n770, zn_splat(P8::from_raw(0i32)));
    let n773: ZB = zn_le(n770, zn_splat(P8::from_raw(0i32)));
    let n774: ZB = zb_and(n771, n772);
    let n775: ZB = zb_and(n771, n773);
    let n776: ZB = zb_or(n774, n775);
    let n777: ZB = zb_and(n117, n749);
    let n778: ZB = zb_and(n118, n750);
    let n779: ZN = zsel_n(n83, zn_splat(P8::from_raw(393216i32)), r_c244);
    let n780: ZB = zb_or(n777, n778);
    let n781: ZN = zsel_n(n81, r_c244, n779);
    let n782: ZN = zsel_n(n69, r_c244, n781);
    let n783: ZN = zsel_n(n120, r_c244, n782);
    let n784: ZN = zn_sub(n129, zn_splat(P8::from_raw(32768i32)));
    let n785: ZN = zn_sub(n784, n130);
    let n786: ZN = zsel_n(n115, n785, r_c269);
    let n787: ZB = zn_lt(n132, zn_splat(P8::from_raw(8388608i32)));
    let n788: ZB = zn_ge(n132, zn_splat(P8::from_raw(8388608i32)));
    let n789: ZB = zb_and(n756, n787);
    let n790: ZB = zb_and(n756, n788);
    let n791: ZN = zsel_n(n787, zn_splat(P8::from_raw(196608i32)), r_c233);
    let n792: ZN = zsel_n(n787, zn_splat(P8::from_raw(65536i32)), r_c245);
    let n793: ZB = zb_or(n789, n790);
    let n794: ZB = zn_gt(n132, zn_splat(P8::from_raw(7340032i32)));
    let n795: ZB = zb_and(n772, n794);
    let n796: ZB = zb_not(n795);
    let n797: ZB = zb_and(n776, n795);
    let n798: ZB = zb_and(n776, n796);
    let n799: ZN = zsel_n(n795, zn_splat(P8::from_raw(327680i32)), n769);
    let n800: ZN = zsel_n(n795, zn_splat(P8::from_raw(131072i32)), r_c245);
    let n801: ZN = zsel_n(n795, zn_splat(P8::from_raw(7340032i32)), n132);
    let n802: ZN = zsel_n(n795, zn_splat(P8::from_raw(0i32)), n770);
    let n803: ZB = zb_or(n797, n798);
    let n804: ZN = zsel_n(n81, n799, n751);
    let n805: ZN = zsel_n(n81, n800, r_c245);
    let n806: ZN = zsel_n(n81, n801, n132);
    let n807: ZN = zsel_n(n81, n802, r_c271);
    let n808: ZB = zb_or(n780, n803);
    let n809: ZN = zsel_n(n69, n791, n804);
    let n810: ZN = zsel_n(n69, n792, n805);
    let n811: ZN = zsel_n(n69, n132, n806);
    let n812: ZN = zsel_n(n69, r_c271, n807);
    let n813: ZB = zb_or(n793, n808);
    let n814: ZN = zsel_n(n120, r_c233, n809);
    let n815: ZN = zsel_n(n120, r_c245, n810);
    let n816: ZN = zsel_n(n120, r_c249, n811);
    let n817: ZN = zsel_n(n120, r_c269, n786);
    let n818: ZN = zsel_n(n120, r_c271, n812);
    let n819: ZB = zb_or(n120, n813);
    let n820: ZB = zb_and(n754, n819);
    let n821: ZB = zb_and(n755, n819);
    let n822: ZB = zb_or(n820, n821);
    let n825: ZW = zw_bits_n(r_c39);
    let n826: ZW = zw_mix1(zw_splat(11400714819323198485u64), n825, 39u64);
    let n827: ZW = zw_mix2(zw_splat(11562461410679940143u64), n825, 39u64);
    let n828: ZW = zw_bits_n(n77);
    let n829: ZW = zw_mix1(n826, n828, 84u64);
    let n830: ZW = zw_mix2(n827, n828, 84u64);
    let n831: ZW = zw_bits_n(n113);
    let n832: ZW = zw_mix1(n829, n831, 85u64);
    let n833: ZW = zw_mix2(n830, n831, 85u64);
    let n834: ZW = zw_bits_n(n112);
    let n835: ZW = zw_mix1(n832, n834, 86u64);
    let n836: ZW = zw_mix2(n833, n834, 86u64);
    let n837: ZW = zw_bits_n(r_c87);
    let n838: ZW = zw_mix1(n835, n837, 87u64);
    let n839: ZW = zw_mix2(n836, n837, 87u64);
    let n840: ZW = zw_bits_n(n132);
    let n841: ZW = zw_mix1(n838, n840, 254u64);
    let n842: ZW = zw_mix2(n839, n840, 254u64);
    let n843: ZW = zw_bits_n(r_c20);
    let n844: ZW = zw_mix1(n841, n843, 20u64);
    let n845: ZW = zw_mix2(n842, n843, 20u64);
    let n846: u64 = false as u64;
    let n847: ZW = zw_mix1(n844, zw_splat(n846), 41u64);
    let n848: ZW = zw_mix2(n845, zw_splat(n846), 41u64);
    let n849: u64 = P8::from_raw(-65536i32).as_raw_u32() as u64;
    let n850: ZW = zw_mix1(n847, zw_splat(n849), 234u64);
    let n851: ZW = zw_mix2(n848, zw_splat(n849), 234u64);
    let n852: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n853: ZW = zw_mix1(n850, zw_splat(n852), 236u64);
    let n854: ZW = zw_mix2(n851, zw_splat(n852), 236u64);
    let n855: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n856: ZW = zw_mix1(n853, zw_splat(n855), 237u64);
    let n857: ZW = zw_mix2(n854, zw_splat(n855), 237u64);
    let n858: ZW = zw_bits_n(n329);
    let n859: ZW = zw_mix1(n856, n858, 239u64);
    let n860: ZW = zw_mix2(n857, n858, 239u64);
    let n861: ZW = zw_mix1(n859, zw_splat(n846), 246u64);
    let n862: ZW = zw_mix2(n860, zw_splat(n846), 246u64);
    let n863: ZW = zw_mix1(n861, zw_splat(n846), 247u64);
    let n864: ZW = zw_mix2(n862, zw_splat(n846), 247u64);
    let n865: ZW = zw_mix1(n863, zw_splat(n852), 268u64);
    let n866: ZW = zw_mix2(n864, zw_splat(n852), 268u64);
    let n867: ZW = zw_mix1(n865, zw_splat(n852), 269u64);
    let n868: ZW = zw_mix2(n866, zw_splat(n852), 269u64);
    let n869: ZW = zw_mix1(n867, zw_splat(n852), 270u64);
    let n870: ZW = zw_mix2(n868, zw_splat(n852), 270u64);
    let n871: ZW = zw_mix1(n869, zw_splat(n852), 271u64);
    let n872: ZW = zw_mix2(n870, zw_splat(n852), 271u64);
    let n873: ZW = zw_mix1(n871, zw_splat(n846), 272u64);
    let n874: ZW = zw_mix2(n872, zw_splat(n846), 272u64);
    let n875: ZW = zw_mix1(n873, zw_splat(n852), 280u64);
    let n876: ZW = zw_mix2(n874, zw_splat(n852), 280u64);
    let n877: ZW = zw_bits_n(n340);
    let n878: ZW = zw_mix1(n875, n877, 281u64);
    let n879: ZW = zw_mix2(n876, n877, 281u64);
    let n880: u64 = true as u64;
    let n881: ZW = zw_mix1(n871, zw_splat(n880), 272u64);
    let n882: ZW = zw_mix2(n872, zw_splat(n880), 272u64);
    let n883: ZW = zw_bits_n(n369);
    let n884: ZW = zw_mix1(n881, n883, 280u64);
    let n885: ZW = zw_mix2(n882, n883, 280u64);
    let n886: ZW = zw_mix1(n884, n877, 281u64);
    let n887: ZW = zw_mix2(n885, n877, 281u64);
    let n888: ZW = zw_bits_n(n394);
    let n889: ZW = zw_mix1(n873, n888, 280u64);
    let n890: ZW = zw_mix2(n874, n888, 280u64);
    let n891: ZW = zw_mix1(n889, n877, 281u64);
    let n892: ZW = zw_mix2(n890, n877, 281u64);
    let n893: ZW = zw_bits_n(n356);
    let n894: ZW = zw_mix1(n856, n893, 239u64);
    let n895: ZW = zw_mix2(n857, n893, 239u64);
    let n896: ZW = zw_mix1(n894, zw_splat(n846), 246u64);
    let n897: ZW = zw_mix2(n895, zw_splat(n846), 246u64);
    let n898: ZW = zw_mix1(n896, zw_splat(n880), 247u64);
    let n899: ZW = zw_mix2(n897, zw_splat(n880), 247u64);
    let n900: ZW = zw_mix1(n898, zw_splat(n852), 268u64);
    let n901: ZW = zw_mix2(n899, zw_splat(n852), 268u64);
    let n902: ZW = zw_mix1(n900, zw_splat(n852), 269u64);
    let n903: ZW = zw_mix2(n901, zw_splat(n852), 269u64);
    let n904: ZW = zw_mix1(n902, zw_splat(n852), 270u64);
    let n905: ZW = zw_mix2(n903, zw_splat(n852), 270u64);
    let n906: ZW = zw_mix1(n904, zw_splat(n852), 271u64);
    let n907: ZW = zw_mix2(n905, zw_splat(n852), 271u64);
    let n908: ZW = zw_mix1(n906, zw_splat(n846), 272u64);
    let n909: ZW = zw_mix2(n907, zw_splat(n846), 272u64);
    let n910: ZW = zw_bits_n(n357);
    let n911: ZW = zw_mix1(n908, n910, 280u64);
    let n912: ZW = zw_mix2(n909, n910, 280u64);
    let n913: ZW = zw_bits_n(n358);
    let n914: ZW = zw_mix1(n911, n913, 281u64);
    let n915: ZW = zw_mix2(n912, n913, 281u64);
    let n916: ZW = zw_mix1(n906, zw_splat(n880), 272u64);
    let n917: ZW = zw_mix2(n907, zw_splat(n880), 272u64);
    let n918: ZW = zw_bits_n(n388);
    let n919: ZW = zw_mix1(n916, n918, 280u64);
    let n920: ZW = zw_mix2(n917, n918, 280u64);
    let n921: ZW = zw_mix1(n919, n913, 281u64);
    let n922: ZW = zw_mix2(n920, n913, 281u64);
    let n923: ZW = zw_bits_n(n413);
    let n924: ZW = zw_mix1(n908, n923, 280u64);
    let n925: ZW = zw_mix2(n909, n923, 280u64);
    let n926: ZW = zw_mix1(n924, n913, 281u64);
    let n927: ZW = zw_mix2(n925, n913, 281u64);
    let n928: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n929: ZW = zw_mix1(n841, zw_splat(n928), 20u64);
    let n930: ZW = zw_mix2(n842, zw_splat(n928), 20u64);
    let n931: ZW = zw_mix1(n929, zw_splat(n880), 41u64);
    let n932: ZW = zw_mix2(n930, zw_splat(n880), 41u64);
    let n933: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n934: ZW = zw_mix1(n931, zw_splat(n933), 234u64);
    let n935: ZW = zw_mix2(n932, zw_splat(n933), 234u64);
    let n936: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n937: ZW = zw_mix1(n934, zw_splat(n936), 236u64);
    let n938: ZW = zw_mix2(n935, zw_splat(n936), 236u64);
    let n939: ZW = zw_mix1(n937, zw_splat(n852), 237u64);
    let n940: ZW = zw_mix2(n938, zw_splat(n852), 237u64);
    let n941: ZW = zw_mix1(n939, n858, 239u64);
    let n942: ZW = zw_mix2(n940, n858, 239u64);
    let n943: ZW = zw_mix1(n941, zw_splat(n880), 246u64);
    let n944: ZW = zw_mix2(n942, zw_splat(n880), 246u64);
    let n945: ZW = zw_mix1(n943, zw_splat(n846), 247u64);
    let n946: ZW = zw_mix2(n944, zw_splat(n846), 247u64);
    let n947: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n948: ZW = zw_mix1(n945, zw_splat(n947), 268u64);
    let n949: ZW = zw_mix2(n946, zw_splat(n947), 268u64);
    let n950: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n951: ZW = zw_mix1(n948, zw_splat(n950), 269u64);
    let n952: ZW = zw_mix2(n949, zw_splat(n950), 269u64);
    let n953: ZW = zw_mix1(n951, zw_splat(n928), 270u64);
    let n954: ZW = zw_mix2(n952, zw_splat(n928), 270u64);
    let n955: ZW = zw_mix1(n953, zw_splat(n852), 271u64);
    let n956: ZW = zw_mix2(n954, zw_splat(n852), 271u64);
    let n957: ZW = zw_mix1(n955, zw_splat(n846), 272u64);
    let n958: ZW = zw_mix2(n956, zw_splat(n846), 272u64);
    let n959: ZW = zw_mix1(n957, zw_splat(n855), 280u64);
    let n960: ZW = zw_mix2(n958, zw_splat(n855), 280u64);
    let n961: ZW = zw_mix1(n959, zw_splat(n852), 281u64);
    let n962: ZW = zw_mix2(n960, zw_splat(n852), 281u64);
    let n963: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n964: ZW = zw_mix1(n951, zw_splat(n963), 270u64);
    let n965: ZW = zw_mix2(n952, zw_splat(n963), 270u64);
    let n966: ZW = zw_mix1(n964, zw_splat(n852), 271u64);
    let n967: ZW = zw_mix2(n965, zw_splat(n852), 271u64);
    let n968: ZW = zw_mix1(n966, zw_splat(n880), 272u64);
    let n969: ZW = zw_mix2(n967, zw_splat(n880), 272u64);
    let n970: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n971: ZW = zw_mix1(n968, zw_splat(n970), 280u64);
    let n972: ZW = zw_mix2(n969, zw_splat(n970), 280u64);
    let n973: ZW = zw_mix1(n971, zw_splat(n852), 281u64);
    let n974: ZW = zw_mix2(n972, zw_splat(n852), 281u64);
    let n975: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n976: ZW = zw_mix1(n957, zw_splat(n975), 280u64);
    let n977: ZW = zw_mix2(n958, zw_splat(n975), 280u64);
    let n978: ZW = zw_mix1(n976, zw_splat(n852), 281u64);
    let n979: ZW = zw_mix2(n977, zw_splat(n852), 281u64);
    let n980: ZW = zw_mix1(n945, zw_splat(n950), 268u64);
    let n981: ZW = zw_mix2(n946, zw_splat(n950), 268u64);
    let n982: ZW = zw_mix1(n980, zw_splat(n947), 269u64);
    let n983: ZW = zw_mix2(n981, zw_splat(n947), 269u64);
    let n984: ZW = zw_mix1(n982, zw_splat(n852), 270u64);
    let n985: ZW = zw_mix2(n983, zw_splat(n852), 270u64);
    let n986: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n987: ZW = zw_mix1(n984, zw_splat(n986), 271u64);
    let n988: ZW = zw_mix2(n985, zw_splat(n986), 271u64);
    let n989: ZW = zw_mix1(n987, zw_splat(n846), 272u64);
    let n990: ZW = zw_mix2(n988, zw_splat(n846), 272u64);
    let n991: ZW = zw_mix1(n989, zw_splat(n852), 280u64);
    let n992: ZW = zw_mix2(n990, zw_splat(n852), 280u64);
    let n993: ZW = zw_mix1(n991, zw_splat(n970), 281u64);
    let n994: ZW = zw_mix2(n992, zw_splat(n970), 281u64);
    let n995: ZW = zw_mix1(n980, zw_splat(n950), 269u64);
    let n996: ZW = zw_mix2(n981, zw_splat(n950), 269u64);
    let n997: ZW = zw_mix1(n995, zw_splat(n963), 270u64);
    let n998: ZW = zw_mix2(n996, zw_splat(n963), 270u64);
    let n999: ZW = zw_mix1(n997, zw_splat(n986), 271u64);
    let n1000: ZW = zw_mix2(n998, zw_splat(n986), 271u64);
    let n1001: ZW = zw_mix1(n999, zw_splat(n880), 272u64);
    let n1002: ZW = zw_mix2(n1000, zw_splat(n880), 272u64);
    let n1003: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1004: ZW = zw_mix1(n1001, zw_splat(n1003), 280u64);
    let n1005: ZW = zw_mix2(n1002, zw_splat(n1003), 280u64);
    let n1006: ZW = zw_mix1(n1004, zw_splat(n1003), 281u64);
    let n1007: ZW = zw_mix2(n1005, zw_splat(n1003), 281u64);
    let n1008: ZW = zw_mix1(n995, zw_splat(n928), 270u64);
    let n1009: ZW = zw_mix2(n996, zw_splat(n928), 270u64);
    let n1010: ZW = zw_mix1(n1008, zw_splat(n986), 271u64);
    let n1011: ZW = zw_mix2(n1009, zw_splat(n986), 271u64);
    let n1012: ZW = zw_mix1(n1010, zw_splat(n846), 272u64);
    let n1013: ZW = zw_mix2(n1011, zw_splat(n846), 272u64);
    let n1014: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1015: ZW = zw_mix1(n1012, zw_splat(n1014), 280u64);
    let n1016: ZW = zw_mix2(n1013, zw_splat(n1014), 280u64);
    let n1017: ZW = zw_mix1(n1015, zw_splat(n1003), 281u64);
    let n1018: ZW = zw_mix2(n1016, zw_splat(n1003), 281u64);
    let n1019: ZW = zw_mix1(n984, zw_splat(n928), 271u64);
    let n1020: ZW = zw_mix2(n985, zw_splat(n928), 271u64);
    let n1021: ZW = zw_mix1(n1019, zw_splat(n846), 272u64);
    let n1022: ZW = zw_mix2(n1020, zw_splat(n846), 272u64);
    let n1023: ZW = zw_mix1(n1021, zw_splat(n852), 280u64);
    let n1024: ZW = zw_mix2(n1022, zw_splat(n852), 280u64);
    let n1025: ZW = zw_mix1(n1023, zw_splat(n975), 281u64);
    let n1026: ZW = zw_mix2(n1024, zw_splat(n975), 281u64);
    let n1027: ZW = zw_mix1(n997, zw_splat(n928), 271u64);
    let n1028: ZW = zw_mix2(n998, zw_splat(n928), 271u64);
    let n1029: ZW = zw_mix1(n1027, zw_splat(n880), 272u64);
    let n1030: ZW = zw_mix2(n1028, zw_splat(n880), 272u64);
    let n1031: ZW = zw_mix1(n1029, zw_splat(n1003), 280u64);
    let n1032: ZW = zw_mix2(n1030, zw_splat(n1003), 280u64);
    let n1033: ZW = zw_mix1(n1031, zw_splat(n1014), 281u64);
    let n1034: ZW = zw_mix2(n1032, zw_splat(n1014), 281u64);
    let n1035: ZW = zw_mix1(n1008, zw_splat(n928), 271u64);
    let n1036: ZW = zw_mix2(n1009, zw_splat(n928), 271u64);
    let n1037: ZW = zw_mix1(n1035, zw_splat(n846), 272u64);
    let n1038: ZW = zw_mix2(n1036, zw_splat(n846), 272u64);
    let n1039: ZW = zw_mix1(n1037, zw_splat(n1014), 280u64);
    let n1040: ZW = zw_mix2(n1038, zw_splat(n1014), 280u64);
    let n1041: ZW = zw_mix1(n1039, zw_splat(n1014), 281u64);
    let n1042: ZW = zw_mix2(n1040, zw_splat(n1014), 281u64);
    let n1043: ZW = zw_mix1(n939, n893, 239u64);
    let n1044: ZW = zw_mix2(n940, n893, 239u64);
    let n1045: ZW = zw_mix1(n1043, zw_splat(n880), 246u64);
    let n1046: ZW = zw_mix2(n1044, zw_splat(n880), 246u64);
    let n1047: ZW = zw_mix1(n1045, zw_splat(n880), 247u64);
    let n1048: ZW = zw_mix2(n1046, zw_splat(n880), 247u64);
    let n1049: ZW = zw_mix1(n1047, zw_splat(n947), 268u64);
    let n1050: ZW = zw_mix2(n1048, zw_splat(n947), 268u64);
    let n1051: ZW = zw_mix1(n1049, zw_splat(n950), 269u64);
    let n1052: ZW = zw_mix2(n1050, zw_splat(n950), 269u64);
    let n1053: ZW = zw_mix1(n1051, zw_splat(n928), 270u64);
    let n1054: ZW = zw_mix2(n1052, zw_splat(n928), 270u64);
    let n1055: ZW = zw_mix1(n1053, zw_splat(n852), 271u64);
    let n1056: ZW = zw_mix2(n1054, zw_splat(n852), 271u64);
    let n1057: ZW = zw_mix1(n1055, zw_splat(n846), 272u64);
    let n1058: ZW = zw_mix2(n1056, zw_splat(n846), 272u64);
    let n1059: ZW = zw_mix1(n1057, zw_splat(n855), 280u64);
    let n1060: ZW = zw_mix2(n1058, zw_splat(n855), 280u64);
    let n1061: ZW = zw_mix1(n1059, zw_splat(n852), 281u64);
    let n1062: ZW = zw_mix2(n1060, zw_splat(n852), 281u64);
    let n1063: ZW = zw_mix1(n1051, zw_splat(n963), 270u64);
    let n1064: ZW = zw_mix2(n1052, zw_splat(n963), 270u64);
    let n1065: ZW = zw_mix1(n1063, zw_splat(n852), 271u64);
    let n1066: ZW = zw_mix2(n1064, zw_splat(n852), 271u64);
    let n1067: ZW = zw_mix1(n1065, zw_splat(n880), 272u64);
    let n1068: ZW = zw_mix2(n1066, zw_splat(n880), 272u64);
    let n1069: ZW = zw_mix1(n1067, zw_splat(n970), 280u64);
    let n1070: ZW = zw_mix2(n1068, zw_splat(n970), 280u64);
    let n1071: ZW = zw_mix1(n1069, zw_splat(n852), 281u64);
    let n1072: ZW = zw_mix2(n1070, zw_splat(n852), 281u64);
    let n1073: ZW = zw_mix1(n1057, zw_splat(n975), 280u64);
    let n1074: ZW = zw_mix2(n1058, zw_splat(n975), 280u64);
    let n1075: ZW = zw_mix1(n1073, zw_splat(n852), 281u64);
    let n1076: ZW = zw_mix2(n1074, zw_splat(n852), 281u64);
    let n1077: ZW = zw_mix1(n1047, zw_splat(n950), 268u64);
    let n1078: ZW = zw_mix2(n1048, zw_splat(n950), 268u64);
    let n1079: ZW = zw_mix1(n1077, zw_splat(n947), 269u64);
    let n1080: ZW = zw_mix2(n1078, zw_splat(n947), 269u64);
    let n1081: ZW = zw_mix1(n1079, zw_splat(n852), 270u64);
    let n1082: ZW = zw_mix2(n1080, zw_splat(n852), 270u64);
    let n1083: ZW = zw_mix1(n1081, zw_splat(n986), 271u64);
    let n1084: ZW = zw_mix2(n1082, zw_splat(n986), 271u64);
    let n1085: ZW = zw_mix1(n1083, zw_splat(n846), 272u64);
    let n1086: ZW = zw_mix2(n1084, zw_splat(n846), 272u64);
    let n1087: ZW = zw_mix1(n1085, zw_splat(n852), 280u64);
    let n1088: ZW = zw_mix2(n1086, zw_splat(n852), 280u64);
    let n1089: ZW = zw_mix1(n1087, zw_splat(n970), 281u64);
    let n1090: ZW = zw_mix2(n1088, zw_splat(n970), 281u64);
    let n1091: ZW = zw_mix1(n1077, zw_splat(n950), 269u64);
    let n1092: ZW = zw_mix2(n1078, zw_splat(n950), 269u64);
    let n1093: ZW = zw_mix1(n1091, zw_splat(n963), 270u64);
    let n1094: ZW = zw_mix2(n1092, zw_splat(n963), 270u64);
    let n1095: ZW = zw_mix1(n1093, zw_splat(n986), 271u64);
    let n1096: ZW = zw_mix2(n1094, zw_splat(n986), 271u64);
    let n1097: ZW = zw_mix1(n1095, zw_splat(n880), 272u64);
    let n1098: ZW = zw_mix2(n1096, zw_splat(n880), 272u64);
    let n1099: ZW = zw_mix1(n1097, zw_splat(n1003), 280u64);
    let n1100: ZW = zw_mix2(n1098, zw_splat(n1003), 280u64);
    let n1101: ZW = zw_mix1(n1099, zw_splat(n1003), 281u64);
    let n1102: ZW = zw_mix2(n1100, zw_splat(n1003), 281u64);
    let n1103: ZW = zw_mix1(n1091, zw_splat(n928), 270u64);
    let n1104: ZW = zw_mix2(n1092, zw_splat(n928), 270u64);
    let n1105: ZW = zw_mix1(n1103, zw_splat(n986), 271u64);
    let n1106: ZW = zw_mix2(n1104, zw_splat(n986), 271u64);
    let n1107: ZW = zw_mix1(n1105, zw_splat(n846), 272u64);
    let n1108: ZW = zw_mix2(n1106, zw_splat(n846), 272u64);
    let n1109: ZW = zw_mix1(n1107, zw_splat(n1014), 280u64);
    let n1110: ZW = zw_mix2(n1108, zw_splat(n1014), 280u64);
    let n1111: ZW = zw_mix1(n1109, zw_splat(n1003), 281u64);
    let n1112: ZW = zw_mix2(n1110, zw_splat(n1003), 281u64);
    let n1113: ZW = zw_mix1(n1081, zw_splat(n928), 271u64);
    let n1114: ZW = zw_mix2(n1082, zw_splat(n928), 271u64);
    let n1115: ZW = zw_mix1(n1113, zw_splat(n846), 272u64);
    let n1116: ZW = zw_mix2(n1114, zw_splat(n846), 272u64);
    let n1117: ZW = zw_mix1(n1115, zw_splat(n852), 280u64);
    let n1118: ZW = zw_mix2(n1116, zw_splat(n852), 280u64);
    let n1119: ZW = zw_mix1(n1117, zw_splat(n975), 281u64);
    let n1120: ZW = zw_mix2(n1118, zw_splat(n975), 281u64);
    let n1121: ZW = zw_mix1(n1093, zw_splat(n928), 271u64);
    let n1122: ZW = zw_mix2(n1094, zw_splat(n928), 271u64);
    let n1123: ZW = zw_mix1(n1121, zw_splat(n880), 272u64);
    let n1124: ZW = zw_mix2(n1122, zw_splat(n880), 272u64);
    let n1125: ZW = zw_mix1(n1123, zw_splat(n1003), 280u64);
    let n1126: ZW = zw_mix2(n1124, zw_splat(n1003), 280u64);
    let n1127: ZW = zw_mix1(n1125, zw_splat(n1014), 281u64);
    let n1128: ZW = zw_mix2(n1126, zw_splat(n1014), 281u64);
    let n1129: ZW = zw_mix1(n1103, zw_splat(n928), 271u64);
    let n1130: ZW = zw_mix2(n1104, zw_splat(n928), 271u64);
    let n1131: ZW = zw_mix1(n1129, zw_splat(n846), 272u64);
    let n1132: ZW = zw_mix2(n1130, zw_splat(n846), 272u64);
    let n1133: ZW = zw_mix1(n1131, zw_splat(n1014), 280u64);
    let n1134: ZW = zw_mix2(n1132, zw_splat(n1014), 280u64);
    let n1135: ZW = zw_mix1(n1133, zw_splat(n1014), 281u64);
    let n1136: ZW = zw_mix2(n1134, zw_splat(n1014), 281u64);
    let n1137: ZW = zw_mix1(zw_splat(11400714819323198485u64), n828, 84u64);
    let n1138: ZW = zw_mix2(zw_splat(11562461410679940143u64), n828, 84u64);
    let n1139: ZW = zw_mix1(n1137, n831, 85u64);
    let n1140: ZW = zw_mix2(n1138, n831, 85u64);
    let n1141: ZW = zw_mix1(n1139, n834, 86u64);
    let n1142: ZW = zw_mix2(n1140, n834, 86u64);
    let n1143: ZW = zw_bits_n(n538);
    let n1144: ZW = zw_mix1(n1141, n1143, 87u64);
    let n1145: ZW = zw_mix2(n1142, n1143, 87u64);
    let n1146: ZW = zw_mix1(n1144, n843, 20u64);
    let n1147: ZW = zw_mix2(n1145, n843, 20u64);
    let n1148: ZW = zw_mix1(n1146, zw_splat(n846), 41u64);
    let n1149: ZW = zw_mix2(n1147, zw_splat(n846), 41u64);
    let n1150: ZW = zw_mix1(n1144, zw_splat(n928), 20u64);
    let n1151: ZW = zw_mix2(n1145, zw_splat(n928), 20u64);
    let n1152: ZW = zw_mix1(n1150, zw_splat(n880), 41u64);
    let n1153: ZW = zw_mix2(n1151, zw_splat(n880), 41u64);
    let n1154: ZW = zw_mix1(n1141, n843, 20u64);
    let n1155: ZW = zw_mix2(n1142, n843, 20u64);
    let n1156: ZW = zw_bits_b(n698);
    let n1157: ZW = zw_mix1(n1154, n1156, 38u64);
    let n1158: ZW = zw_mix2(n1155, n1156, 38u64);
    let n1159: ZW = zw_bits_n(n697);
    let n1160: ZW = zw_mix1(n1157, n1159, 39u64);
    let n1161: ZW = zw_mix2(n1158, n1159, 39u64);
    let n1162: ZW = zw_bits_n(n696);
    let n1163: ZW = zw_mix1(n1160, n1162, 87u64);
    let n1164: ZW = zw_mix2(n1161, n1162, 87u64);
    let n1165: ZW = zw_bits_b(n707);
    let n1166: ZW = zw_mix1(n1154, n1165, 38u64);
    let n1167: ZW = zw_mix2(n1155, n1165, 38u64);
    let n1168: ZW = zw_bits_n(n706);
    let n1169: ZW = zw_mix1(n1166, n1168, 39u64);
    let n1170: ZW = zw_mix2(n1167, n1168, 39u64);
    let n1171: ZW = zw_bits_n(n705);
    let n1172: ZW = zw_mix1(n1169, n1171, 87u64);
    let n1173: ZW = zw_mix2(n1170, n1171, 87u64);
    let n1174: ZW = zw_bits_b(n716);
    let n1175: ZW = zw_mix1(n1154, n1174, 38u64);
    let n1176: ZW = zw_mix2(n1155, n1174, 38u64);
    let n1177: ZW = zw_bits_n(n715);
    let n1178: ZW = zw_mix1(n1175, n1177, 39u64);
    let n1179: ZW = zw_mix2(n1176, n1177, 39u64);
    let n1180: ZW = zw_bits_n(n714);
    let n1181: ZW = zw_mix1(n1178, n1180, 87u64);
    let n1182: ZW = zw_mix2(n1179, n1180, 87u64);
    let n1183: ZW = zw_bits_b(n725);
    let n1184: ZW = zw_mix1(n1154, n1183, 38u64);
    let n1185: ZW = zw_mix2(n1155, n1183, 38u64);
    let n1186: ZW = zw_bits_n(n724);
    let n1187: ZW = zw_mix1(n1184, n1186, 39u64);
    let n1188: ZW = zw_mix2(n1185, n1186, 39u64);
    let n1189: ZW = zw_bits_n(n723);
    let n1190: ZW = zw_mix1(n1187, n1189, 87u64);
    let n1191: ZW = zw_mix2(n1188, n1189, 87u64);
    let n1192: ZW = zw_bits_b(n734);
    let n1193: ZW = zw_mix1(n1154, n1192, 38u64);
    let n1194: ZW = zw_mix2(n1155, n1192, 38u64);
    let n1195: ZW = zw_bits_n(n733);
    let n1196: ZW = zw_mix1(n1193, n1195, 39u64);
    let n1197: ZW = zw_mix2(n1194, n1195, 39u64);
    let n1198: ZW = zw_bits_n(n732);
    let n1199: ZW = zw_mix1(n1196, n1198, 87u64);
    let n1200: ZW = zw_mix2(n1197, n1198, 87u64);
    let n1201: ZW = zw_bits_b(n743);
    let n1202: ZW = zw_mix1(n1154, n1201, 38u64);
    let n1203: ZW = zw_mix2(n1155, n1201, 38u64);
    let n1204: ZW = zw_bits_n(n742);
    let n1205: ZW = zw_mix1(n1202, n1204, 39u64);
    let n1206: ZW = zw_mix2(n1203, n1204, 39u64);
    let n1207: ZW = zw_bits_n(n741);
    let n1208: ZW = zw_mix1(n1205, n1207, 87u64);
    let n1209: ZW = zw_mix2(n1206, n1207, 87u64);
    let n1210: ZW = zw_mix1(n1141, zw_splat(n928), 20u64);
    let n1211: ZW = zw_mix2(n1142, zw_splat(n928), 20u64);
    let n1212: ZW = zw_mix1(n1210, n1156, 38u64);
    let n1213: ZW = zw_mix2(n1211, n1156, 38u64);
    let n1214: ZW = zw_mix1(n1212, n1159, 39u64);
    let n1215: ZW = zw_mix2(n1213, n1159, 39u64);
    let n1216: ZW = zw_mix1(n1214, n1162, 87u64);
    let n1217: ZW = zw_mix2(n1215, n1162, 87u64);
    let n1218: ZW = zw_mix1(n1210, n1165, 38u64);
    let n1219: ZW = zw_mix2(n1211, n1165, 38u64);
    let n1220: ZW = zw_mix1(n1218, n1168, 39u64);
    let n1221: ZW = zw_mix2(n1219, n1168, 39u64);
    let n1222: ZW = zw_mix1(n1220, n1171, 87u64);
    let n1223: ZW = zw_mix2(n1221, n1171, 87u64);
    let n1224: ZW = zw_mix1(n1210, n1174, 38u64);
    let n1225: ZW = zw_mix2(n1211, n1174, 38u64);
    let n1226: ZW = zw_mix1(n1224, n1177, 39u64);
    let n1227: ZW = zw_mix2(n1225, n1177, 39u64);
    let n1228: ZW = zw_mix1(n1226, n1180, 87u64);
    let n1229: ZW = zw_mix2(n1227, n1180, 87u64);
    let n1230: ZW = zw_mix1(n1210, n1183, 38u64);
    let n1231: ZW = zw_mix2(n1211, n1183, 38u64);
    let n1232: ZW = zw_mix1(n1230, n1186, 39u64);
    let n1233: ZW = zw_mix2(n1231, n1186, 39u64);
    let n1234: ZW = zw_mix1(n1232, n1189, 87u64);
    let n1235: ZW = zw_mix2(n1233, n1189, 87u64);
    let n1236: ZW = zw_mix1(n1210, n1192, 38u64);
    let n1237: ZW = zw_mix2(n1211, n1192, 38u64);
    let n1238: ZW = zw_mix1(n1236, n1195, 39u64);
    let n1239: ZW = zw_mix2(n1237, n1195, 39u64);
    let n1240: ZW = zw_mix1(n1238, n1198, 87u64);
    let n1241: ZW = zw_mix2(n1239, n1198, 87u64);
    let n1242: ZW = zw_mix1(n1210, n1201, 38u64);
    let n1243: ZW = zw_mix2(n1211, n1201, 38u64);
    let n1244: ZW = zw_mix1(n1242, n1204, 39u64);
    let n1245: ZW = zw_mix2(n1243, n1204, 39u64);
    let n1246: ZW = zw_mix1(n1244, n1207, 87u64);
    let n1247: ZW = zw_mix2(n1245, n1207, 87u64);
    let n1248: ZW = zw_bits_n(n753);
    let n1249: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1248, 20u64);
    let n1250: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1248, 20u64);
    let n1251: ZW = zw_mix1(n1249, n825, 39u64);
    let n1252: ZW = zw_mix2(n1250, n825, 39u64);
    let n1253: ZW = zw_mix1(n1251, n828, 84u64);
    let n1254: ZW = zw_mix2(n1252, n828, 84u64);
    let n1255: ZW = zw_mix1(n1253, n831, 85u64);
    let n1256: ZW = zw_mix2(n1254, n831, 85u64);
    let n1257: ZW = zw_mix1(n1255, n834, 86u64);
    let n1258: ZW = zw_mix2(n1256, n834, 86u64);
    let n1259: ZW = zw_mix1(n1257, n837, 87u64);
    let n1260: ZW = zw_mix2(n1258, n837, 87u64);
    let n1261: ZW = zw_bits_n(n814);
    let n1262: ZW = zw_mix1(n1259, n1261, 233u64);
    let n1263: ZW = zw_mix2(n1260, n1261, 233u64);
    let n1264: ZW = zw_bits_n(n783);
    let n1265: ZW = zw_mix1(n1262, n1264, 244u64);
    let n1266: ZW = zw_mix2(n1263, n1264, 244u64);
    let n1267: ZW = zw_bits_n(n815);
    let n1268: ZW = zw_mix1(n1265, n1267, 245u64);
    let n1269: ZW = zw_mix2(n1266, n1267, 245u64);
    let n1270: ZW = zw_bits_n(n816);
    let n1271: ZW = zw_mix1(n1268, n1270, 249u64);
    let n1272: ZW = zw_mix2(n1269, n1270, 249u64);
    let n1273: ZW = zw_bits_n(n817);
    let n1274: ZW = zw_mix1(n1271, n1273, 269u64);
    let n1275: ZW = zw_mix2(n1272, n1273, 269u64);
    let n1276: ZW = zw_bits_n(n818);
    let n1277: ZW = zw_mix1(n1274, n1276, 271u64);
    let n1278: ZW = zw_mix2(n1275, n1276, 271u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v0_b0: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b0: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n363);
    let ok_v1_b1: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v1_b1: bool = !n65 || !n64 || !n63 || !n62;
    let live_v1_b1: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n391);
    let ok_v2_b2: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v2_b2: bool = !n65 || !n64 || !n63 || !n62;
    let live_v2_b2: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n416);
    let ok_v16_b3: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v16_b3: bool = !n65 || !n64 || !n63 || !n62;
    let live_v16_b3: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n445);
    let ok_v17_b4: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v17_b4: bool = !n65 || !n64 || !n63 || !n62;
    let live_v17_b4: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n474);
    let ok_v18_b5: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v18_b5: bool = !n65 || !n64 || !n63 || !n62;
    let live_v18_b5: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n503);
    let ok_v32_b6: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v32_b6: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b6: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v33_b7: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v33_b7: bool = !n65 || !n64 || !n63 || !n62;
    let live_v33_b7: u16 = ALL & zb_holds(n360) & zb_holds(n391);
    let ok_v34_b8: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v34_b8: bool = !n65 || !n64 || !n63 || !n62;
    let live_v34_b8: u16 = ALL & zb_holds(n360) & zb_holds(n416);
    let ok_v36_b9: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v36_b9: bool = !n65 || !n64 || !n63 || !n62;
    let live_v36_b9: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v37_b10: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v37_b10: bool = !n65 || !n64 || !n63 || !n62;
    let live_v37_b10: u16 = ALL & zb_holds(n360) & zb_holds(n391);
    let ok_v38_b11: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v38_b11: bool = !n65 || !n64 || !n63 || !n62;
    let live_v38_b11: u16 = ALL & zb_holds(n360) & zb_holds(n416);
    let ok_v40_b12: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v40_b12: bool = !n65 || !n64 || !n63 || !n62;
    let live_v40_b12: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v41_b13: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v41_b13: bool = !n65 || !n64 || !n63 || !n62;
    let live_v41_b13: u16 = ALL & zb_holds(n360) & zb_holds(n391);
    let ok_v42_b14: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v42_b14: bool = !n65 || !n64 || !n63 || !n62;
    let live_v42_b14: u16 = ALL & zb_holds(n360) & zb_holds(n416);
    let ok_v48_b15: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v48_b15: bool = !n65 || !n64 || !n63 || !n62;
    let live_v48_b15: u16 = ALL & zb_holds(n360) & zb_holds(n445);
    let ok_v49_b16: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v49_b16: bool = !n65 || !n64 || !n63 || !n62;
    let live_v49_b16: u16 = ALL & zb_holds(n360) & zb_holds(n474);
    let ok_v50_b17: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v50_b17: bool = !n65 || !n64 || !n63 || !n62;
    let live_v50_b17: u16 = ALL & zb_holds(n360) & zb_holds(n503);
    let ok_v52_b18: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v52_b18: bool = !n65 || !n64 || !n63 || !n62;
    let live_v52_b18: u16 = ALL & zb_holds(n360) & zb_holds(n445);
    let ok_v53_b19: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v53_b19: bool = !n65 || !n64 || !n63 || !n62;
    let live_v53_b19: u16 = ALL & zb_holds(n360) & zb_holds(n474);
    let ok_v54_b20: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v54_b20: bool = !n65 || !n64 || !n63 || !n62;
    let live_v54_b20: u16 = ALL & zb_holds(n360) & zb_holds(n503);
    let ok_v56_b21: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v56_b21: bool = !n65 || !n64 || !n63 || !n62;
    let live_v56_b21: u16 = ALL & zb_holds(n360) & zb_holds(n445);
    let ok_v57_b22: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v57_b22: bool = !n65 || !n64 || !n63 || !n62;
    let live_v57_b22: u16 = ALL & zb_holds(n360) & zb_holds(n474);
    let ok_v58_b23: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n315);
    let bd_v58_b23: bool = !n65 || !n64 || !n63 || !n62;
    let live_v58_b23: u16 = ALL & zb_holds(n360) & zb_holds(n503);
    let ok_v0_b24: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v0_b24: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b24: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n558);
    let ok_v1_b25: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v1_b25: bool = !n65 || !n64 || !n63 || !n62;
    let live_v1_b25: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n579);
    let ok_v2_b26: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v2_b26: bool = !n65 || !n64 || !n63 || !n62;
    let live_v2_b26: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n599);
    let ok_v16_b27: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v16_b27: bool = !n65 || !n64 || !n63 || !n62;
    let live_v16_b27: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n628);
    let ok_v17_b28: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v17_b28: bool = !n65 || !n64 || !n63 || !n62;
    let live_v17_b28: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n657);
    let ok_v18_b29: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v18_b29: bool = !n65 || !n64 || !n63 || !n62;
    let live_v18_b29: u16 = ALL & zb_holds(n96) & zb_holds(n360) & zb_holds(n686);
    let ok_v32_b30: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v32_b30: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b30: u16 = ALL & zb_holds(n360) & zb_holds(n558);
    let ok_v33_b31: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v33_b31: bool = !n65 || !n64 || !n63 || !n62;
    let live_v33_b31: u16 = ALL & zb_holds(n360) & zb_holds(n579);
    let ok_v34_b32: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v34_b32: bool = !n65 || !n64 || !n63 || !n62;
    let live_v34_b32: u16 = ALL & zb_holds(n360) & zb_holds(n599);
    let ok_v48_b33: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v48_b33: bool = !n65 || !n64 || !n63 || !n62;
    let live_v48_b33: u16 = ALL & zb_holds(n360) & zb_holds(n628);
    let ok_v49_b34: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v49_b34: bool = !n65 || !n64 || !n63 || !n62;
    let live_v49_b34: u16 = ALL & zb_holds(n360) & zb_holds(n657);
    let ok_v50_b35: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n540);
    let bd_v50_b35: bool = !n65 || !n64 || !n63 || !n62;
    let live_v50_b35: u16 = ALL & zb_holds(n360) & zb_holds(n686);
    let ok_v0_b36: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n700);
    let bd_v0_b36: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b36: u16 = ALL & zb_holds(n96) & zb_holds(n699);
    let ok_v1_b37: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n709);
    let bd_v1_b37: bool = !n65 || !n64 || !n63 || !n62;
    let live_v1_b37: u16 = ALL & zb_holds(n96) & zb_holds(n708);
    let ok_v2_b38: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n718);
    let bd_v2_b38: bool = !n65 || !n64 || !n63 || !n62;
    let live_v2_b38: u16 = ALL & zb_holds(n96) & zb_holds(n717);
    let ok_v16_b39: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n727);
    let bd_v16_b39: bool = !n65 || !n64 || !n63 || !n62;
    let live_v16_b39: u16 = ALL & zb_holds(n96) & zb_holds(n726);
    let ok_v17_b40: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n736);
    let bd_v17_b40: bool = !n65 || !n64 || !n63 || !n62;
    let live_v17_b40: u16 = ALL & zb_holds(n96) & zb_holds(n735);
    let ok_v18_b41: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n745);
    let bd_v18_b41: bool = !n65 || !n64 || !n63 || !n62;
    let live_v18_b41: u16 = ALL & zb_holds(n96) & zb_holds(n744);
    let ok_v32_b42: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n700);
    let bd_v32_b42: bool = !n65 || !n64 || !n63 || !n62;
    let live_v32_b42: u16 = ALL & zb_holds(n699);
    let ok_v33_b43: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n709);
    let bd_v33_b43: bool = !n65 || !n64 || !n63 || !n62;
    let live_v33_b43: u16 = ALL & zb_holds(n708);
    let ok_v34_b44: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n718);
    let bd_v34_b44: bool = !n65 || !n64 || !n63 || !n62;
    let live_v34_b44: u16 = ALL & zb_holds(n717);
    let ok_v48_b45: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n727);
    let bd_v48_b45: bool = !n65 || !n64 || !n63 || !n62;
    let live_v48_b45: u16 = ALL & zb_holds(n726);
    let ok_v49_b46: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n736);
    let bd_v49_b46: bool = !n65 || !n64 || !n63 || !n62;
    let live_v49_b46: u16 = ALL & zb_holds(n735);
    let ok_v50_b47: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58) & zb_holds(n745);
    let bd_v50_b47: bool = !n65 || !n64 || !n63 || !n62;
    let live_v50_b47: u16 = ALL & zb_holds(n744);
    let ok_v0_b48: u16 = ALL & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n70) & zb_holds(n68) & zb_holds(n67) & zb_holds(n66) & zb_holds(n61) & zb_holds(n60) & zb_holds(r_c232) & zb_holds(n59) & zb_holds(n57) & zb_holds(n58);
    let bd_v0_b48: bool = !n65 || !n64 || !n63 || !n62;
    let live_v0_b48: u16 = ALL & zb_holds(n822);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n77,
        c86: n112,
        c254: n132,
        c85: n113,
    };
    let sh1 = KShared1 {
        c87: n538,
        c84: n77,
        c86: n112,
        c85: n113,
    };
    let sh2 = KShared2 {
        c84: n77,
        c86: n112,
        c85: n113,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n77,
        c20: n753,
        c86: n112,
        c233: n814,
        c269: n817,
        c271: n818,
        c244: n783,
        c245: n815,
        c249: n816,
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
    let mut take_3_0: u16 = 0;
    // 49 distinct button assignments; per outcome they fall
    // into [24, 2, 12, 1] groups that write identical values.
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
        c239: n329,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n340,
        h1: n878, h2: n879,
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
        c239: n329,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n369,
        c281: n340,
        h1: n886, h2: n887,
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
        c239: n329,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n394,
        c281: n340,
        h1: n891, h2: n892,
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
        c239: n356,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n357,
        c281: n358,
        h1: n914, h2: n915,
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
        c239: n356,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n388,
        c281: n358,
        h1: n921, h2: n922,
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
        c239: n356,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n413,
        c281: n358,
        h1: n926, h2: n927,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n961, h2: n962,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n973, h2: n974,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n978, h2: n979,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n993, h2: n994,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1006, h2: n1007,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1017, h2: n1018,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n1025, h2: n1026,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1033, h2: n1034,
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
        c239: n329,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1041, h2: n1042,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1061, h2: n1062,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1071, h2: n1072,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1075, h2: n1076,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n1089, h2: n1090,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1101, h2: n1102,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1111, h2: n1112,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n1119, h2: n1120,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1127, h2: n1128,
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
        c239: n356,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1135, h2: n1136,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_0 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_0 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_0 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_0 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_0 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n1148, h2: n1149,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_1 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_1 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_1 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    declined |= live_v48_b33 & (if bd_v48_b33 { ALL } else { !ok_v48_b33 });
    take_1_1 |= live_v48_b33 & ok_v48_b33 & (if bd_v48_b33 { 0 } else { ALL });
    declined |= live_v49_b34 & (if bd_v49_b34 { ALL } else { !ok_v49_b34 });
    take_1_1 |= live_v49_b34 & ok_v49_b34 & (if bd_v49_b34 { 0 } else { ALL });
    declined |= live_v50_b35 & (if bd_v50_b35 { ALL } else { !ok_v50_b35 });
    take_1_1 |= live_v50_b35 & ok_v50_b35 & (if bd_v50_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: n1152, h2: n1153,
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n696,
        c39: n697,
        c20: r_c20,
        c38: n698,
        h1: n1163, h2: n1164,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n705,
        c39: n706,
        c20: r_c20,
        c38: n707,
        h1: n1172, h2: n1173,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n714,
        c39: n715,
        c20: r_c20,
        c38: n716,
        h1: n1181, h2: n1182,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n723,
        c39: n724,
        c20: r_c20,
        c38: n725,
        h1: n1190, h2: n1191,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n732,
        c39: n733,
        c20: r_c20,
        c38: n734,
        h1: n1199, h2: n1200,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n741,
        c39: n742,
        c20: r_c20,
        c38: n743,
        h1: n1208, h2: n1209,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n696,
        c39: n697,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n698,
        h1: n1216, h2: n1217,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n705,
        c39: n706,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n707,
        h1: n1222, h2: n1223,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n714,
        c39: n715,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n716,
        h1: n1228, h2: n1229,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n723,
        c39: n724,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n725,
        h1: n1234, h2: n1235,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n732,
        c39: n733,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n734,
        h1: n1240, h2: n1241,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n741,
        c39: n742,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n743,
        h1: n1246, h2: n1247,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n1277, h2: n1278,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
