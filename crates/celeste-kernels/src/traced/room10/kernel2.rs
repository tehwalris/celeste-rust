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
    pub c39: ZN,
    pub c254: ZN,
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
    pub c39: ZN,
    pub c20: ZN,
    pub c233: ZN,
    pub c269: ZN,
    pub c271: ZN,
    pub c244: ZN,
    pub c245: ZN,
    pub c249: ZN,
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[278] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[279] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[249] = Col::U(AV::Bool(true));
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[158] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
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
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Ival(P8::from_raw(0i32), P8::from_raw(2555904i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Ival(P8::from_raw(2981888i32), P8::from_raw(3309568i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[233] { v.push(sh.c233.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(sh.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[244] { v.push(sh.c244.lane(i)); }
        if let Col::N(v) = &mut acc.cols[245] { v.push(sh.c245.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
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
    let n56: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n57: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n58: ZB = zb_not(r_c41);
    let n59: ZB = zb_not(r_c42);
    let n60: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n61: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n62: ZB = zb_not(r_c262);
    let n63: ZB = zb_not(r_c263);
    let n64: bool = P8::from_raw(524288i32) == u.c264;
    let n65: bool = P8::from_raw(524288i32) == u.c265;
    let n66: bool = P8::from_raw(0i32) == u.c266;
    let n67: bool = P8::from_raw(0i32) == u.c267;
    let n68: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c268);
    let n69: ZB = zb_not(r_c242);
    let n70: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n71: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c245);
    let n72: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c272);
    let n73: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n74: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c248);
    let n75: ZB = zb_not(r_c43);
    let n76: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n77: ZB = zb_not(r_c38);
    let n79: ZB = zb_not(n71);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c245);
    let n81: ZB = zb_not(n80);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c245);
    let n83: ZN = zn_sub(r_c233, zn_splat(P8::from_raw(65536i32)));
    let n84: ZB = zn_lt(n83, zn_splat(P8::from_raw(0i32)));
    let n96: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c271);
    let n111: ZB = zb_not(n110);
    let n112: ZB = zb_and(n79, n96);
    let n113: ZB = zb_and(n81, n112);
    let n114: ZB = zb_and(n82, n113);
    let n115: ZB = zb_and(n84, n114);
    let n116: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n125: ZN = zn_add(r_c269, r_c271);
    let n126: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n125);
    let n127: ZN = zn_flr(n126);
    let n128: ZN = zn_add(r_c249, n127);
    let n129: ZN = zsel_n(n111, n128, r_c249);
    let n130: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n129);
    let n131: ZN = zn_div(n130, zn_splat(P8::from_raw(524288i32)));
    let n132: ZN = zn_flr(n131);
    let n133: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n132);
    let n134: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n130);
    let n135: ZN = zn_sub(n134, zn_splat(P8::from_raw(65536i32)));
    let n136: ZN = zn_div(n135, zn_splat(P8::from_raw(524288i32)));
    let n137: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n136);
    let n138: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n133);
    let n139: ZB = zn_le(n138, n137);
    let n140: ZB = zn_gt(n138, n137);
    let n141: ZB = zb_and(n115, n139);
    let n142: ZB = zb_and(n115, n140);
    let n143: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n138);
    let n144: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n143);
    let n145: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n144);
    let n146: ZB = zb_not(n145);
    let n147: ZB = zb_and(n141, n145);
    let n148: ZB = zb_and(n141, n146);
    let n149: ZN = zn_rem(n135, zn_splat(P8::from_raw(524288i32)));
    let n150: ZB = zn_ge(n149, zn_splat(P8::from_raw(393216i32)));
    let n151: ZB = zn_lt(n149, zn_splat(P8::from_raw(393216i32)));
    let n152: ZB = zb_and(n147, n151);
    let n153: ZB = zb_and(n147, n150);
    let n154: ZN = zn_mul(n138, zn_splat(P8::from_raw(524288i32)));
    let n155: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n154);
    let n156: ZB = zn_eq(n134, n155);
    let n157: ZB = zb_or(n152, n153);
    let n158: ZB = zb_or(n150, n156);
    let n159: ZB = zb_or(n148, n157);
    let n160: ZB = zb_and(n145, n158);
    let n161: ZB = zb_not(n160);
    let n162: ZB = zb_and(n159, n160);
    let n163: ZB = zb_and(n159, n161);
    let n164: ZB = zb_or(n162, n163);
    let n165: ZB = zb_and(n161, n164);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n144);
    let n167: ZB = zb_not(n166);
    let n168: ZB = zb_and(n165, n166);
    let n169: ZB = zb_and(n165, n167);
    let n170: ZN = zn_rem(n130, zn_splat(P8::from_raw(524288i32)));
    let n171: ZB = zn_le(n170, zn_splat(P8::from_raw(131072i32)));
    let n172: ZB = zb_or(n168, n169);
    let n173: ZB = zb_and(n166, n171);
    let n174: ZB = zb_not(n173);
    let n175: ZB = zb_and(n172, n173);
    let n176: ZB = zb_and(n172, n174);
    let n177: ZB = zb_or(n175, n176);
    let n178: ZB = zb_and(n174, n177);
    let n179: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n144);
    let n180: ZB = zb_not(n179);
    let n181: ZB = zb_and(n178, n179);
    let n182: ZB = zb_and(n178, n180);
    let n183: ZB = zb_or(n181, n182);
    let n184: ZB = zb_and(n179, n183);
    let n185: ZB = zb_and(n180, n183);
    let n186: ZB = zb_or(n184, n185);
    let n187: ZB = zb_and(n180, n186);
    let n188: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n144);
    let n189: ZB = zb_not(n188);
    let n190: ZB = zb_and(n187, n188);
    let n191: ZB = zb_and(n187, n189);
    let n192: ZB = zb_or(n190, n191);
    let n193: ZB = zb_and(n188, n192);
    let n194: ZB = zb_and(n189, n192);
    let n195: ZB = zb_or(n193, n194);
    let n196: ZB = zb_and(n189, n195);
    let n197: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n133);
    let n198: ZB = zn_le(n197, n137);
    let n199: ZB = zn_gt(n197, n137);
    let n200: ZB = zb_and(n196, n198);
    let n201: ZB = zb_and(n196, n199);
    let n202: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n197);
    let n203: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n202);
    let n204: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n203);
    let n205: ZB = zb_not(n204);
    let n206: ZB = zb_and(n200, n204);
    let n207: ZB = zb_and(n200, n205);
    let n208: ZB = zb_and(n151, n206);
    let n209: ZB = zb_and(n150, n206);
    let n210: ZN = zn_mul(n197, zn_splat(P8::from_raw(524288i32)));
    let n211: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n210);
    let n212: ZB = zn_eq(n134, n211);
    let n213: ZB = zb_or(n208, n209);
    let n214: ZB = zb_or(n150, n212);
    let n215: ZB = zb_or(n207, n213);
    let n216: ZB = zb_and(n204, n214);
    let n217: ZB = zb_not(n216);
    let n218: ZB = zb_and(n215, n216);
    let n219: ZB = zb_and(n215, n217);
    let n220: ZB = zb_or(n218, n219);
    let n221: ZB = zb_and(n217, n220);
    let n222: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n203);
    let n223: ZB = zb_not(n222);
    let n224: ZB = zb_and(n221, n222);
    let n225: ZB = zb_and(n221, n223);
    let n226: ZB = zb_or(n224, n225);
    let n227: ZB = zb_and(n171, n222);
    let n228: ZB = zb_not(n227);
    let n229: ZB = zb_and(n226, n227);
    let n230: ZB = zb_and(n226, n228);
    let n231: ZB = zb_or(n229, n230);
    let n232: ZB = zb_and(n228, n231);
    let n233: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n203);
    let n234: ZB = zb_not(n233);
    let n235: ZB = zb_and(n232, n233);
    let n236: ZB = zb_and(n232, n234);
    let n237: ZB = zb_or(n235, n236);
    let n238: ZB = zb_and(n233, n237);
    let n239: ZB = zb_and(n234, n237);
    let n240: ZB = zb_or(n238, n239);
    let n241: ZB = zb_and(n234, n240);
    let n242: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n203);
    let n243: ZB = zb_not(n242);
    let n244: ZB = zb_and(n241, n242);
    let n245: ZB = zb_and(n241, n243);
    let n246: ZB = zb_or(n244, n245);
    let n247: ZB = zb_and(n242, n246);
    let n248: ZB = zb_and(n243, n246);
    let n249: ZB = zb_or(n247, n248);
    let n250: ZB = zb_and(n243, n249);
    let n251: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n133);
    let n252: ZB = zn_le(n251, n137);
    let n253: ZB = zn_gt(n251, n137);
    let n254: ZB = zb_and(n250, n252);
    let n255: ZB = zb_and(n250, n253);
    let n256: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n251);
    let n257: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(1114112i32)), n256);
    let n258: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n257);
    let n259: ZB = zb_not(n258);
    let n260: ZB = zb_and(n254, n258);
    let n261: ZB = zb_and(n254, n259);
    let n262: ZB = zb_and(n151, n260);
    let n263: ZB = zb_and(n150, n260);
    let n264: ZN = zn_mul(n251, zn_splat(P8::from_raw(524288i32)));
    let n265: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n264);
    let n266: ZB = zn_eq(n134, n265);
    let n267: ZB = zb_or(n262, n263);
    let n268: ZB = zb_or(n150, n266);
    let n269: ZB = zb_or(n261, n267);
    let n270: ZB = zb_and(n258, n268);
    let n271: ZB = zb_not(n270);
    let n272: ZB = zb_and(n269, n270);
    let n273: ZB = zb_and(n269, n271);
    let n274: ZB = zb_or(n272, n273);
    let n275: ZB = zb_and(n271, n274);
    let n276: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n257);
    let n277: ZB = zb_not(n276);
    let n278: ZB = zb_and(n275, n276);
    let n279: ZB = zb_and(n275, n277);
    let n280: ZB = zb_or(n278, n279);
    let n281: ZB = zb_and(n171, n276);
    let n282: ZB = zb_not(n281);
    let n283: ZB = zb_and(n280, n281);
    let n284: ZB = zb_and(n280, n282);
    let n285: ZB = zb_or(n283, n284);
    let n286: ZB = zb_and(n282, n285);
    let n287: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n257);
    let n288: ZB = zb_not(n287);
    let n289: ZB = zb_and(n286, n287);
    let n290: ZB = zb_and(n286, n288);
    let n291: ZB = zb_or(n289, n290);
    let n292: ZB = zb_and(n287, n291);
    let n293: ZB = zb_and(n288, n291);
    let n294: ZB = zb_or(n292, n293);
    let n295: ZB = zb_and(n288, n294);
    let n296: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n257);
    let n297: ZB = zb_not(n296);
    let n298: ZB = zb_and(n295, n296);
    let n299: ZB = zb_and(n295, n297);
    let n300: ZB = zb_or(n298, n299);
    let n301: ZB = zb_and(n296, n300);
    let n302: ZB = zb_and(n297, n300);
    let n303: ZB = zb_or(n301, n302);
    let n304: ZB = zb_and(n297, n303);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n133);
    let n306: ZB = zn_gt(n305, n137);
    let n307: ZB = zb_or(n255, n304);
    let n308: ZB = zb_or(n253, n306);
    let n309: ZB = zb_or(n201, n307);
    let n310: ZB = zb_or(n199, n308);
    let n311: ZB = zb_or(n142, n309);
    let n312: ZB = zb_or(n140, n310);
    let n313: ZB = zn_le(n129, zn_splat(P8::from_raw(8388608i32)));
    let n314: ZB = zb_and(n311, n313);
    let n315: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n130);
    let n316: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n315, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n317: ZB = zb_not(n316);
    let n318: ZB = zb_and(n314, n317);
    let n319: ZB = zb_and(n314, n316);
    let n320: ZB = zb_or(n318, n319);
    let n321: ZB = zb_and(n317, n320);
    let n322: ZB = zb_and(n316, n320);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZB = zb_and(n316, n323);
    let n325: ZB = zb_and(n317, n323);
    let n326: ZN = zsel_n(n316, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n327: ZB = zb_or(n324, n325);
    let n328: ZB = zb_and(n317, n327);
    let n329: ZB = zb_and(n316, n327);
    let n330: ZN = zsel_n(n317, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n331: ZB = zb_or(n328, n329);
    let n332: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n330);
    let n333: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n330);
    let n334: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n130);
    let n335: ZB = zb_and(n317, n331);
    let n336: ZB = zb_and(n316, n331);
    let n337: ZN = zsel_n(n317, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n338: ZB = zb_or(n335, n336);
    let n339: ZB = zn_gt(n326, zn_splat(P8::from_raw(0i32)));
    let n340: ZB = zn_le(n326, zn_splat(P8::from_raw(0i32)));
    let n341: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n342: ZB = zb_not(n341);
    let n343: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n344: ZB = zb_not(n343);
    let n345: ZN = zsel_n(n343, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n346: ZN = zsel_n(n341, zn_splat(P8::from_raw(-65536i32)), n345);
    let n347: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n346);
    let n348: ZB = zb_not(n347);
    let n349: ZN = zn_neg(n346);
    let n350: ZN = zn_mul(n349, zn_splat(P8::from_raw(131072i32)));
    let n351: ZN = zsel_n(n348, n350, zn_splat(P8::from_raw(0i32)));
    let n352: ZN = zsel_n(n348, zn_splat(P8::from_raw(-131072i32)), n337);
    let n353: ZN = zsel_n(n339, zn_splat(P8::from_raw(0i32)), n326);
    let n354: ZN = zsel_n(n339, zn_splat(P8::from_raw(0i32)), n351);
    let n355: ZN = zsel_n(n339, zn_splat(P8::from_raw(-131072i32)), n352);
    let n356: ZB = zn_lt(n129, zn_splat(P8::from_raw(-262144i32)));
    let n357: ZB = zn_ge(n129, zn_splat(P8::from_raw(-262144i32)));
    let n358: ZB = zb_and(n338, n356);
    let n359: ZB = zb_and(n338, n357);
    let n360: ZB = zb_or(n358, n359);
    let n366: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n332);
    let n367: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n368: ZB = zb_not(n367);
    let n369: ZB = zb_and(n331, n368);
    let n370: ZB = zb_and(n331, n367);
    let n371: ZB = zb_or(n369, n370);
    let n372: ZB = zb_and(n368, n371);
    let n373: ZB = zb_and(n367, n371);
    let n374: ZB = zb_or(n372, n373);
    let n375: ZB = zb_and(n367, n374);
    let n376: ZB = zb_and(n368, n374);
    let n377: ZB = zb_or(n375, n376);
    let n378: ZB = zb_and(n367, n377);
    let n379: ZB = zb_and(n368, n377);
    let n380: ZB = zb_or(n378, n379);
    let n381: ZB = zb_and(n317, n380);
    let n382: ZB = zb_and(n316, n380);
    let n383: ZB = zb_or(n381, n382);
    let n384: ZN = zsel_n(n348, n350, n366);
    let n385: ZN = zsel_n(n339, n366, n384);
    let n386: ZB = zb_and(n356, n383);
    let n387: ZB = zb_and(n357, n383);
    let n388: ZB = zb_or(n386, n387);
    let n391: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n333);
    let n392: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n393: ZB = zb_not(n392);
    let n394: ZB = zb_and(n331, n393);
    let n395: ZB = zb_and(n331, n392);
    let n396: ZB = zb_or(n394, n395);
    let n397: ZB = zb_and(n393, n396);
    let n398: ZB = zb_and(n392, n396);
    let n399: ZB = zb_or(n397, n398);
    let n400: ZB = zb_and(n392, n399);
    let n401: ZB = zb_and(n393, n399);
    let n402: ZB = zb_or(n400, n401);
    let n403: ZB = zb_and(n392, n402);
    let n404: ZB = zb_and(n393, n402);
    let n405: ZB = zb_or(n403, n404);
    let n406: ZB = zb_and(n317, n405);
    let n407: ZB = zb_and(n316, n405);
    let n408: ZB = zb_or(n406, n407);
    let n409: ZN = zsel_n(n348, n350, n391);
    let n410: ZN = zsel_n(n339, n391, n409);
    let n411: ZB = zb_and(n356, n408);
    let n412: ZB = zb_and(n357, n408);
    let n413: ZB = zb_or(n411, n412);
    let n416: ZB = zb_and(n338, n339);
    let n417: ZB = zb_and(n338, n340);
    let n418: ZB = zb_and(n342, n417);
    let n419: ZB = zb_and(n341, n417);
    let n420: ZB = zb_or(n418, n419);
    let n421: ZB = zb_and(n342, n420);
    let n422: ZB = zb_and(n341, n420);
    let n423: ZB = zb_or(n421, n422);
    let n424: ZB = zb_and(n341, n423);
    let n425: ZB = zb_and(n342, n423);
    let n426: ZB = zb_and(n344, n425);
    let n427: ZB = zb_and(n343, n425);
    let n428: ZB = zb_or(n426, n427);
    let n429: ZB = zb_and(n344, n428);
    let n430: ZB = zb_and(n343, n428);
    let n431: ZB = zb_or(n429, n430);
    let n432: ZB = zb_and(n343, n431);
    let n433: ZB = zb_and(n344, n431);
    let n434: ZB = zb_or(n432, n433);
    let n435: ZB = zb_or(n424, n434);
    let n436: ZB = zb_and(n348, n435);
    let n437: ZB = zb_and(n347, n435);
    let n438: ZB = zb_or(n436, n437);
    let n439: ZB = zb_or(n416, n438);
    let n440: ZB = zb_and(n356, n439);
    let n441: ZB = zb_and(n357, n439);
    let n442: ZB = zb_or(n440, n441);
    let n445: ZB = zb_and(n339, n383);
    let n446: ZB = zb_and(n340, n383);
    let n447: ZB = zb_and(n342, n446);
    let n448: ZB = zb_and(n341, n446);
    let n449: ZB = zb_or(n447, n448);
    let n450: ZB = zb_and(n342, n449);
    let n451: ZB = zb_and(n341, n449);
    let n452: ZB = zb_or(n450, n451);
    let n453: ZB = zb_and(n341, n452);
    let n454: ZB = zb_and(n342, n452);
    let n455: ZB = zb_and(n344, n454);
    let n456: ZB = zb_and(n343, n454);
    let n457: ZB = zb_or(n455, n456);
    let n458: ZB = zb_and(n344, n457);
    let n459: ZB = zb_and(n343, n457);
    let n460: ZB = zb_or(n458, n459);
    let n461: ZB = zb_and(n343, n460);
    let n462: ZB = zb_and(n344, n460);
    let n463: ZB = zb_or(n461, n462);
    let n464: ZB = zb_or(n453, n463);
    let n465: ZB = zb_and(n348, n464);
    let n466: ZB = zb_and(n347, n464);
    let n467: ZB = zb_or(n465, n466);
    let n468: ZB = zb_or(n445, n467);
    let n469: ZB = zb_and(n356, n468);
    let n470: ZB = zb_and(n357, n468);
    let n471: ZB = zb_or(n469, n470);
    let n474: ZB = zb_and(n339, n408);
    let n475: ZB = zb_and(n340, n408);
    let n476: ZB = zb_and(n342, n475);
    let n477: ZB = zb_and(n341, n475);
    let n478: ZB = zb_or(n476, n477);
    let n479: ZB = zb_and(n342, n478);
    let n480: ZB = zb_and(n341, n478);
    let n481: ZB = zb_or(n479, n480);
    let n482: ZB = zb_and(n341, n481);
    let n483: ZB = zb_and(n342, n481);
    let n484: ZB = zb_and(n344, n483);
    let n485: ZB = zb_and(n343, n483);
    let n486: ZB = zb_or(n484, n485);
    let n487: ZB = zb_and(n344, n486);
    let n488: ZB = zb_and(n343, n486);
    let n489: ZB = zb_or(n487, n488);
    let n490: ZB = zb_and(n343, n489);
    let n491: ZB = zb_and(n344, n489);
    let n492: ZB = zb_or(n490, n491);
    let n493: ZB = zb_or(n482, n492);
    let n494: ZB = zb_and(n348, n493);
    let n495: ZB = zb_and(n347, n493);
    let n496: ZB = zb_or(n494, n495);
    let n497: ZB = zb_or(n474, n496);
    let n498: ZB = zb_and(n356, n497);
    let n499: ZB = zb_and(n357, n497);
    let n500: ZB = zb_or(n498, n499);
    let n504: ZB = zb_and(n160, n164);
    let n505: ZB = zb_and(n173, n177);
    let n506: ZB = zb_and(n179, n186);
    let n507: ZB = zb_and(n188, n195);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_or(n505, n508);
    let n510: ZB = zb_or(n504, n509);
    let n511: ZB = zb_and(n216, n220);
    let n512: ZB = zb_and(n227, n231);
    let n513: ZB = zb_and(n233, n240);
    let n514: ZB = zb_and(n242, n249);
    let n515: ZB = zb_or(n513, n514);
    let n516: ZB = zb_or(n512, n515);
    let n517: ZB = zb_or(n511, n516);
    let n518: ZB = zb_and(n270, n274);
    let n519: ZB = zb_and(n281, n285);
    let n520: ZB = zb_and(n287, n294);
    let n521: ZB = zb_and(n296, n303);
    let n522: ZB = zb_or(n520, n521);
    let n523: ZB = zb_or(n519, n522);
    let n524: ZB = zb_or(n518, n523);
    let n525: ZB = zb_or(n517, n524);
    let n526: ZB = zb_or(n510, n525);
    let n527: ZB = zn_gt(n129, zn_splat(P8::from_raw(8388608i32)));
    let n528: ZB = zb_and(n526, n527);
    let n529: ZB = zb_and(n313, n526);
    let n530: ZB = zb_or(n528, n529);
    let n531: ZB = zb_and(n311, n527);
    let n532: ZB = zb_or(n530, n531);
    let n533: ZB = zb_or(n312, n530);
    let n534: ZB = zb_and(n317, n532);
    let n535: ZB = zb_and(n316, n532);
    let n536: ZB = zb_or(n534, n535);
    let n537: ZB = zb_and(n317, n536);
    let n538: ZB = zb_and(n316, n536);
    let n539: ZB = zb_or(n537, n538);
    let n540: ZB = zb_and(n316, n539);
    let n541: ZB = zb_and(n317, n539);
    let n542: ZB = zb_or(n540, n541);
    let n543: ZB = zb_and(n317, n542);
    let n544: ZB = zb_and(n316, n542);
    let n545: ZB = zb_or(n543, n544);
    let n546: ZB = zb_and(n317, n545);
    let n547: ZB = zb_and(n316, n545);
    let n548: ZB = zb_or(n546, n547);
    let n549: ZB = zb_and(n356, n548);
    let n550: ZB = zb_and(n357, n548);
    let n551: ZB = zb_or(n549, n550);
    let n555: ZB = zb_and(n368, n545);
    let n556: ZB = zb_and(n367, n545);
    let n557: ZB = zb_or(n555, n556);
    let n558: ZB = zb_and(n368, n557);
    let n559: ZB = zb_and(n367, n557);
    let n560: ZB = zb_or(n558, n559);
    let n561: ZB = zb_and(n367, n560);
    let n562: ZB = zb_and(n368, n560);
    let n563: ZB = zb_or(n561, n562);
    let n564: ZB = zb_and(n367, n563);
    let n565: ZB = zb_and(n368, n563);
    let n566: ZB = zb_or(n564, n565);
    let n567: ZB = zb_and(n317, n566);
    let n568: ZB = zb_and(n316, n566);
    let n569: ZB = zb_or(n567, n568);
    let n570: ZB = zb_and(n356, n569);
    let n571: ZB = zb_and(n357, n569);
    let n572: ZB = zb_or(n570, n571);
    let n575: ZB = zb_and(n393, n545);
    let n576: ZB = zb_and(n392, n545);
    let n577: ZB = zb_or(n575, n576);
    let n578: ZB = zb_and(n393, n577);
    let n579: ZB = zb_and(n392, n577);
    let n580: ZB = zb_or(n578, n579);
    let n581: ZB = zb_and(n392, n580);
    let n582: ZB = zb_and(n393, n580);
    let n583: ZB = zb_or(n581, n582);
    let n584: ZB = zb_and(n392, n583);
    let n585: ZB = zb_and(n393, n583);
    let n586: ZB = zb_or(n584, n585);
    let n587: ZB = zb_and(n317, n586);
    let n588: ZB = zb_and(n316, n586);
    let n589: ZB = zb_or(n587, n588);
    let n590: ZB = zb_and(n356, n589);
    let n591: ZB = zb_and(n357, n589);
    let n592: ZB = zb_or(n590, n591);
    let n595: ZB = zb_and(n339, n548);
    let n596: ZB = zb_and(n340, n548);
    let n597: ZB = zb_and(n342, n596);
    let n598: ZB = zb_and(n341, n596);
    let n599: ZB = zb_or(n597, n598);
    let n600: ZB = zb_and(n342, n599);
    let n601: ZB = zb_and(n341, n599);
    let n602: ZB = zb_or(n600, n601);
    let n603: ZB = zb_and(n341, n602);
    let n604: ZB = zb_and(n342, n602);
    let n605: ZB = zb_and(n344, n604);
    let n606: ZB = zb_and(n343, n604);
    let n607: ZB = zb_or(n605, n606);
    let n608: ZB = zb_and(n344, n607);
    let n609: ZB = zb_and(n343, n607);
    let n610: ZB = zb_or(n608, n609);
    let n611: ZB = zb_and(n343, n610);
    let n612: ZB = zb_and(n344, n610);
    let n613: ZB = zb_or(n611, n612);
    let n614: ZB = zb_or(n603, n613);
    let n615: ZB = zb_and(n348, n614);
    let n616: ZB = zb_and(n347, n614);
    let n617: ZB = zb_or(n615, n616);
    let n618: ZB = zb_or(n595, n617);
    let n619: ZB = zb_and(n356, n618);
    let n620: ZB = zb_and(n357, n618);
    let n621: ZB = zb_or(n619, n620);
    let n624: ZB = zb_and(n339, n569);
    let n625: ZB = zb_and(n340, n569);
    let n626: ZB = zb_and(n342, n625);
    let n627: ZB = zb_and(n341, n625);
    let n628: ZB = zb_or(n626, n627);
    let n629: ZB = zb_and(n342, n628);
    let n630: ZB = zb_and(n341, n628);
    let n631: ZB = zb_or(n629, n630);
    let n632: ZB = zb_and(n341, n631);
    let n633: ZB = zb_and(n342, n631);
    let n634: ZB = zb_and(n344, n633);
    let n635: ZB = zb_and(n343, n633);
    let n636: ZB = zb_or(n634, n635);
    let n637: ZB = zb_and(n344, n636);
    let n638: ZB = zb_and(n343, n636);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_and(n343, n639);
    let n641: ZB = zb_and(n344, n639);
    let n642: ZB = zb_or(n640, n641);
    let n643: ZB = zb_or(n632, n642);
    let n644: ZB = zb_and(n348, n643);
    let n645: ZB = zb_and(n347, n643);
    let n646: ZB = zb_or(n644, n645);
    let n647: ZB = zb_or(n624, n646);
    let n648: ZB = zb_and(n356, n647);
    let n649: ZB = zb_and(n357, n647);
    let n650: ZB = zb_or(n648, n649);
    let n653: ZB = zb_and(n339, n589);
    let n654: ZB = zb_and(n340, n589);
    let n655: ZB = zb_and(n342, n654);
    let n656: ZB = zb_and(n341, n654);
    let n657: ZB = zb_or(n655, n656);
    let n658: ZB = zb_and(n342, n657);
    let n659: ZB = zb_and(n341, n657);
    let n660: ZB = zb_or(n658, n659);
    let n661: ZB = zb_and(n341, n660);
    let n662: ZB = zb_and(n342, n660);
    let n663: ZB = zb_and(n344, n662);
    let n664: ZB = zb_and(n343, n662);
    let n665: ZB = zb_or(n663, n664);
    let n666: ZB = zb_and(n344, n665);
    let n667: ZB = zb_and(n343, n665);
    let n668: ZB = zb_or(n666, n667);
    let n669: ZB = zb_and(n343, n668);
    let n670: ZB = zb_and(n344, n668);
    let n671: ZB = zb_or(n669, n670);
    let n672: ZB = zb_or(n661, n671);
    let n673: ZB = zb_and(n348, n672);
    let n674: ZB = zb_and(n347, n672);
    let n675: ZB = zb_or(n673, n674);
    let n676: ZB = zb_or(n653, n675);
    let n677: ZB = zb_and(n356, n676);
    let n678: ZB = zb_and(n357, n676);
    let n679: ZB = zb_or(n677, n678);
    let n688: ZB = zb_and(n356, n360);
    let n689: ZB = zb_and(n356, n551);
    let n690: ZN = zsel_n(n688, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n691: ZB = zb_not(n688);
    let n692: ZB = zb_or(n688, n689);
    let n693: ZB = zsel_b(n688, n312, n533);
    let n696: ZB = zb_and(n356, n388);
    let n697: ZB = zb_and(n356, n572);
    let n698: ZN = zsel_n(n696, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n699: ZB = zb_not(n696);
    let n700: ZB = zb_or(n696, n697);
    let n701: ZB = zsel_b(n696, n312, n533);
    let n704: ZB = zb_and(n356, n413);
    let n705: ZB = zb_and(n356, n592);
    let n706: ZN = zsel_n(n704, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n707: ZB = zb_not(n704);
    let n708: ZB = zb_or(n704, n705);
    let n709: ZB = zsel_b(n704, n312, n533);
    let n712: ZB = zb_and(n356, n442);
    let n713: ZB = zb_and(n356, n621);
    let n714: ZN = zsel_n(n712, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n715: ZB = zb_not(n712);
    let n716: ZB = zb_or(n712, n713);
    let n717: ZB = zsel_b(n712, n312, n533);
    let n720: ZB = zb_and(n356, n471);
    let n721: ZB = zb_and(n356, n650);
    let n722: ZN = zsel_n(n720, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n723: ZB = zb_not(n720);
    let n724: ZB = zb_or(n720, n721);
    let n725: ZB = zsel_b(n720, n312, n533);
    let n728: ZB = zb_and(n356, n500);
    let n729: ZB = zb_and(n356, n679);
    let n730: ZN = zsel_n(n728, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n731: ZB = zb_not(n728);
    let n732: ZB = zb_or(n728, n729);
    let n733: ZB = zsel_b(n728, n312, n533);
    let n737: ZB = zb_not(n82);
    let n738: ZB = zn_ge(n83, zn_splat(P8::from_raw(0i32)));
    let n739: ZN = zsel_n(n82, n83, r_c233);
    let n740: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n741: ZN = zsel_n(n116, n740, r_c20);
    let n742: ZB = zn_gt(n741, zn_splat(P8::from_raw(0i32)));
    let n743: ZB = zn_le(n741, zn_splat(P8::from_raw(0i32)));
    let n744: ZB = zb_and(n71, n96);
    let n745: ZB = zb_and(n80, n112);
    let n746: ZN = zn_add(r_c271, zn_splat(P8::from_raw(32768i32)));
    let n747: ZB = zn_gt(n746, zn_splat(P8::from_raw(0i32)));
    let n748: ZB = zn_le(n746, zn_splat(P8::from_raw(0i32)));
    let n749: ZB = zb_and(n745, n747);
    let n750: ZB = zb_and(n745, n748);
    let n751: ZB = zn_gt(r_c233, zn_splat(P8::from_raw(0i32)));
    let n752: ZB = zb_or(n749, n750);
    let n753: ZB = zb_and(n747, n751);
    let n754: ZB = zb_not(n753);
    let n755: ZB = zb_and(n752, n753);
    let n756: ZB = zb_and(n752, n754);
    let n757: ZN = zsel_n(n753, n83, r_c233);
    let n758: ZN = zsel_n(n753, zn_splat(P8::from_raw(0i32)), n746);
    let n759: ZB = zb_or(n755, n756);
    let n760: ZB = zn_gt(n758, zn_splat(P8::from_raw(0i32)));
    let n761: ZB = zn_le(n758, zn_splat(P8::from_raw(0i32)));
    let n762: ZB = zb_and(n759, n760);
    let n763: ZB = zb_and(n759, n761);
    let n764: ZB = zb_or(n762, n763);
    let n765: ZB = zb_and(n113, n737);
    let n766: ZB = zb_and(n114, n738);
    let n767: ZN = zsel_n(n82, zn_splat(P8::from_raw(393216i32)), r_c244);
    let n768: ZB = zb_or(n765, n766);
    let n769: ZN = zsel_n(n80, r_c244, n767);
    let n770: ZN = zsel_n(n71, r_c244, n769);
    let n771: ZN = zsel_n(n116, r_c244, n770);
    let n772: ZN = zn_sub(n126, zn_splat(P8::from_raw(32768i32)));
    let n773: ZN = zn_sub(n772, n127);
    let n774: ZN = zsel_n(n111, n773, r_c269);
    let n775: ZB = zn_lt(n129, zn_splat(P8::from_raw(8388608i32)));
    let n776: ZB = zn_ge(n129, zn_splat(P8::from_raw(8388608i32)));
    let n777: ZB = zb_and(n744, n775);
    let n778: ZB = zb_and(n744, n776);
    let n779: ZN = zsel_n(n775, zn_splat(P8::from_raw(196608i32)), r_c233);
    let n780: ZN = zsel_n(n775, zn_splat(P8::from_raw(65536i32)), r_c245);
    let n781: ZB = zb_or(n777, n778);
    let n782: ZB = zn_gt(n129, zn_splat(P8::from_raw(7340032i32)));
    let n783: ZB = zb_and(n760, n782);
    let n784: ZB = zb_not(n783);
    let n785: ZB = zb_and(n764, n783);
    let n786: ZB = zb_and(n764, n784);
    let n787: ZN = zsel_n(n783, zn_splat(P8::from_raw(327680i32)), n757);
    let n788: ZN = zsel_n(n783, zn_splat(P8::from_raw(131072i32)), r_c245);
    let n789: ZN = zsel_n(n783, zn_splat(P8::from_raw(7340032i32)), n129);
    let n790: ZN = zsel_n(n783, zn_splat(P8::from_raw(0i32)), n758);
    let n791: ZB = zb_or(n785, n786);
    let n792: ZN = zsel_n(n80, n787, n739);
    let n793: ZN = zsel_n(n80, n788, r_c245);
    let n794: ZN = zsel_n(n80, n789, n129);
    let n795: ZN = zsel_n(n80, n790, r_c271);
    let n796: ZB = zb_or(n768, n791);
    let n797: ZN = zsel_n(n71, n779, n792);
    let n798: ZN = zsel_n(n71, n780, n793);
    let n799: ZN = zsel_n(n71, n129, n794);
    let n800: ZN = zsel_n(n71, r_c271, n795);
    let n801: ZB = zb_or(n781, n796);
    let n802: ZN = zsel_n(n116, r_c233, n797);
    let n803: ZN = zsel_n(n116, r_c245, n798);
    let n804: ZN = zsel_n(n116, r_c249, n799);
    let n805: ZN = zsel_n(n116, r_c269, n774);
    let n806: ZN = zsel_n(n116, r_c271, n800);
    let n807: ZB = zb_or(n116, n801);
    let n808: ZB = zb_and(n742, n807);
    let n809: ZB = zb_and(n743, n807);
    let n810: ZB = zb_or(n808, n809);
    let n813: ZW = zw_bits_n(r_c39);
    let n814: ZW = zw_mix1(zw_splat(11400714819323198485u64), n813, 39u64);
    let n815: ZW = zw_mix2(zw_splat(11562461410679940143u64), n813, 39u64);
    let n816: ZW = zw_bits_n(n129);
    let n817: ZW = zw_mix1(n814, n816, 254u64);
    let n818: ZW = zw_mix2(n815, n816, 254u64);
    let n819: ZW = zw_bits_n(r_c20);
    let n820: ZW = zw_mix1(n817, n819, 20u64);
    let n821: ZW = zw_mix2(n818, n819, 20u64);
    let n822: u64 = false as u64;
    let n823: ZW = zw_mix1(n820, zw_splat(n822), 41u64);
    let n824: ZW = zw_mix2(n821, zw_splat(n822), 41u64);
    let n825: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n826: ZW = zw_mix1(n823, zw_splat(n825), 234u64);
    let n827: ZW = zw_mix2(n824, zw_splat(n825), 234u64);
    let n828: ZW = zw_mix1(n826, zw_splat(n825), 236u64);
    let n829: ZW = zw_mix2(n827, zw_splat(n825), 236u64);
    let n830: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n831: ZW = zw_mix1(n828, zw_splat(n830), 237u64);
    let n832: ZW = zw_mix2(n829, zw_splat(n830), 237u64);
    let n833: ZW = zw_bits_n(n326);
    let n834: ZW = zw_mix1(n831, n833, 239u64);
    let n835: ZW = zw_mix2(n832, n833, 239u64);
    let n836: ZW = zw_mix1(n834, zw_splat(n822), 246u64);
    let n837: ZW = zw_mix2(n835, zw_splat(n822), 246u64);
    let n838: ZW = zw_mix1(n836, zw_splat(n822), 247u64);
    let n839: ZW = zw_mix2(n837, zw_splat(n822), 247u64);
    let n840: ZW = zw_mix1(n838, zw_splat(n825), 268u64);
    let n841: ZW = zw_mix2(n839, zw_splat(n825), 268u64);
    let n842: ZW = zw_mix1(n840, zw_splat(n825), 269u64);
    let n843: ZW = zw_mix2(n841, zw_splat(n825), 269u64);
    let n844: ZW = zw_mix1(n842, zw_splat(n825), 270u64);
    let n845: ZW = zw_mix2(n843, zw_splat(n825), 270u64);
    let n846: ZW = zw_mix1(n844, zw_splat(n825), 271u64);
    let n847: ZW = zw_mix2(n845, zw_splat(n825), 271u64);
    let n848: ZW = zw_mix1(n846, zw_splat(n822), 272u64);
    let n849: ZW = zw_mix2(n847, zw_splat(n822), 272u64);
    let n850: ZW = zw_mix1(n848, zw_splat(n825), 280u64);
    let n851: ZW = zw_mix2(n849, zw_splat(n825), 280u64);
    let n852: ZW = zw_bits_n(n337);
    let n853: ZW = zw_mix1(n850, n852, 281u64);
    let n854: ZW = zw_mix2(n851, n852, 281u64);
    let n855: u64 = true as u64;
    let n856: ZW = zw_mix1(n846, zw_splat(n855), 272u64);
    let n857: ZW = zw_mix2(n847, zw_splat(n855), 272u64);
    let n858: ZW = zw_bits_n(n366);
    let n859: ZW = zw_mix1(n856, n858, 280u64);
    let n860: ZW = zw_mix2(n857, n858, 280u64);
    let n861: ZW = zw_mix1(n859, n852, 281u64);
    let n862: ZW = zw_mix2(n860, n852, 281u64);
    let n863: ZW = zw_bits_n(n391);
    let n864: ZW = zw_mix1(n848, n863, 280u64);
    let n865: ZW = zw_mix2(n849, n863, 280u64);
    let n866: ZW = zw_mix1(n864, n852, 281u64);
    let n867: ZW = zw_mix2(n865, n852, 281u64);
    let n868: ZW = zw_bits_n(n353);
    let n869: ZW = zw_mix1(n831, n868, 239u64);
    let n870: ZW = zw_mix2(n832, n868, 239u64);
    let n871: ZW = zw_mix1(n869, zw_splat(n822), 246u64);
    let n872: ZW = zw_mix2(n870, zw_splat(n822), 246u64);
    let n873: ZW = zw_mix1(n871, zw_splat(n855), 247u64);
    let n874: ZW = zw_mix2(n872, zw_splat(n855), 247u64);
    let n875: ZW = zw_mix1(n873, zw_splat(n825), 268u64);
    let n876: ZW = zw_mix2(n874, zw_splat(n825), 268u64);
    let n877: ZW = zw_mix1(n875, zw_splat(n825), 269u64);
    let n878: ZW = zw_mix2(n876, zw_splat(n825), 269u64);
    let n879: ZW = zw_mix1(n877, zw_splat(n825), 270u64);
    let n880: ZW = zw_mix2(n878, zw_splat(n825), 270u64);
    let n881: ZW = zw_mix1(n879, zw_splat(n825), 271u64);
    let n882: ZW = zw_mix2(n880, zw_splat(n825), 271u64);
    let n883: ZW = zw_mix1(n881, zw_splat(n822), 272u64);
    let n884: ZW = zw_mix2(n882, zw_splat(n822), 272u64);
    let n885: ZW = zw_bits_n(n354);
    let n886: ZW = zw_mix1(n883, n885, 280u64);
    let n887: ZW = zw_mix2(n884, n885, 280u64);
    let n888: ZW = zw_bits_n(n355);
    let n889: ZW = zw_mix1(n886, n888, 281u64);
    let n890: ZW = zw_mix2(n887, n888, 281u64);
    let n891: ZW = zw_mix1(n881, zw_splat(n855), 272u64);
    let n892: ZW = zw_mix2(n882, zw_splat(n855), 272u64);
    let n893: ZW = zw_bits_n(n385);
    let n894: ZW = zw_mix1(n891, n893, 280u64);
    let n895: ZW = zw_mix2(n892, n893, 280u64);
    let n896: ZW = zw_mix1(n894, n888, 281u64);
    let n897: ZW = zw_mix2(n895, n888, 281u64);
    let n898: ZW = zw_bits_n(n410);
    let n899: ZW = zw_mix1(n883, n898, 280u64);
    let n900: ZW = zw_mix2(n884, n898, 280u64);
    let n901: ZW = zw_mix1(n899, n888, 281u64);
    let n902: ZW = zw_mix2(n900, n888, 281u64);
    let n903: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n904: ZW = zw_mix1(n817, zw_splat(n903), 20u64);
    let n905: ZW = zw_mix2(n818, zw_splat(n903), 20u64);
    let n906: ZW = zw_mix1(n904, zw_splat(n855), 41u64);
    let n907: ZW = zw_mix2(n905, zw_splat(n855), 41u64);
    let n908: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n909: ZW = zw_mix1(n906, zw_splat(n908), 234u64);
    let n910: ZW = zw_mix2(n907, zw_splat(n908), 234u64);
    let n911: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n912: ZW = zw_mix1(n909, zw_splat(n911), 236u64);
    let n913: ZW = zw_mix2(n910, zw_splat(n911), 236u64);
    let n914: ZW = zw_mix1(n912, zw_splat(n825), 237u64);
    let n915: ZW = zw_mix2(n913, zw_splat(n825), 237u64);
    let n916: ZW = zw_mix1(n914, n833, 239u64);
    let n917: ZW = zw_mix2(n915, n833, 239u64);
    let n918: ZW = zw_mix1(n916, zw_splat(n855), 246u64);
    let n919: ZW = zw_mix2(n917, zw_splat(n855), 246u64);
    let n920: ZW = zw_mix1(n918, zw_splat(n822), 247u64);
    let n921: ZW = zw_mix2(n919, zw_splat(n822), 247u64);
    let n922: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n923: ZW = zw_mix1(n920, zw_splat(n922), 268u64);
    let n924: ZW = zw_mix2(n921, zw_splat(n922), 268u64);
    let n925: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n926: ZW = zw_mix1(n923, zw_splat(n925), 269u64);
    let n927: ZW = zw_mix2(n924, zw_splat(n925), 269u64);
    let n928: ZW = zw_mix1(n926, zw_splat(n903), 270u64);
    let n929: ZW = zw_mix2(n927, zw_splat(n903), 270u64);
    let n930: ZW = zw_mix1(n928, zw_splat(n825), 271u64);
    let n931: ZW = zw_mix2(n929, zw_splat(n825), 271u64);
    let n932: ZW = zw_mix1(n930, zw_splat(n822), 272u64);
    let n933: ZW = zw_mix2(n931, zw_splat(n822), 272u64);
    let n934: ZW = zw_mix1(n932, zw_splat(n830), 280u64);
    let n935: ZW = zw_mix2(n933, zw_splat(n830), 280u64);
    let n936: ZW = zw_mix1(n934, zw_splat(n825), 281u64);
    let n937: ZW = zw_mix2(n935, zw_splat(n825), 281u64);
    let n938: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n939: ZW = zw_mix1(n926, zw_splat(n938), 270u64);
    let n940: ZW = zw_mix2(n927, zw_splat(n938), 270u64);
    let n941: ZW = zw_mix1(n939, zw_splat(n825), 271u64);
    let n942: ZW = zw_mix2(n940, zw_splat(n825), 271u64);
    let n943: ZW = zw_mix1(n941, zw_splat(n855), 272u64);
    let n944: ZW = zw_mix2(n942, zw_splat(n855), 272u64);
    let n945: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n946: ZW = zw_mix1(n943, zw_splat(n945), 280u64);
    let n947: ZW = zw_mix2(n944, zw_splat(n945), 280u64);
    let n948: ZW = zw_mix1(n946, zw_splat(n825), 281u64);
    let n949: ZW = zw_mix2(n947, zw_splat(n825), 281u64);
    let n950: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n951: ZW = zw_mix1(n932, zw_splat(n950), 280u64);
    let n952: ZW = zw_mix2(n933, zw_splat(n950), 280u64);
    let n953: ZW = zw_mix1(n951, zw_splat(n825), 281u64);
    let n954: ZW = zw_mix2(n952, zw_splat(n825), 281u64);
    let n955: ZW = zw_mix1(n920, zw_splat(n925), 268u64);
    let n956: ZW = zw_mix2(n921, zw_splat(n925), 268u64);
    let n957: ZW = zw_mix1(n955, zw_splat(n922), 269u64);
    let n958: ZW = zw_mix2(n956, zw_splat(n922), 269u64);
    let n959: ZW = zw_mix1(n957, zw_splat(n825), 270u64);
    let n960: ZW = zw_mix2(n958, zw_splat(n825), 270u64);
    let n961: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n962: ZW = zw_mix1(n959, zw_splat(n961), 271u64);
    let n963: ZW = zw_mix2(n960, zw_splat(n961), 271u64);
    let n964: ZW = zw_mix1(n962, zw_splat(n822), 272u64);
    let n965: ZW = zw_mix2(n963, zw_splat(n822), 272u64);
    let n966: ZW = zw_mix1(n964, zw_splat(n825), 280u64);
    let n967: ZW = zw_mix2(n965, zw_splat(n825), 280u64);
    let n968: ZW = zw_mix1(n966, zw_splat(n945), 281u64);
    let n969: ZW = zw_mix2(n967, zw_splat(n945), 281u64);
    let n970: ZW = zw_mix1(n955, zw_splat(n925), 269u64);
    let n971: ZW = zw_mix2(n956, zw_splat(n925), 269u64);
    let n972: ZW = zw_mix1(n970, zw_splat(n938), 270u64);
    let n973: ZW = zw_mix2(n971, zw_splat(n938), 270u64);
    let n974: ZW = zw_mix1(n972, zw_splat(n961), 271u64);
    let n975: ZW = zw_mix2(n973, zw_splat(n961), 271u64);
    let n976: ZW = zw_mix1(n974, zw_splat(n855), 272u64);
    let n977: ZW = zw_mix2(n975, zw_splat(n855), 272u64);
    let n978: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n979: ZW = zw_mix1(n976, zw_splat(n978), 280u64);
    let n980: ZW = zw_mix2(n977, zw_splat(n978), 280u64);
    let n981: ZW = zw_mix1(n979, zw_splat(n978), 281u64);
    let n982: ZW = zw_mix2(n980, zw_splat(n978), 281u64);
    let n983: ZW = zw_mix1(n970, zw_splat(n903), 270u64);
    let n984: ZW = zw_mix2(n971, zw_splat(n903), 270u64);
    let n985: ZW = zw_mix1(n983, zw_splat(n961), 271u64);
    let n986: ZW = zw_mix2(n984, zw_splat(n961), 271u64);
    let n987: ZW = zw_mix1(n985, zw_splat(n822), 272u64);
    let n988: ZW = zw_mix2(n986, zw_splat(n822), 272u64);
    let n989: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n990: ZW = zw_mix1(n987, zw_splat(n989), 280u64);
    let n991: ZW = zw_mix2(n988, zw_splat(n989), 280u64);
    let n992: ZW = zw_mix1(n990, zw_splat(n978), 281u64);
    let n993: ZW = zw_mix2(n991, zw_splat(n978), 281u64);
    let n994: ZW = zw_mix1(n959, zw_splat(n903), 271u64);
    let n995: ZW = zw_mix2(n960, zw_splat(n903), 271u64);
    let n996: ZW = zw_mix1(n994, zw_splat(n822), 272u64);
    let n997: ZW = zw_mix2(n995, zw_splat(n822), 272u64);
    let n998: ZW = zw_mix1(n996, zw_splat(n825), 280u64);
    let n999: ZW = zw_mix2(n997, zw_splat(n825), 280u64);
    let n1000: ZW = zw_mix1(n998, zw_splat(n950), 281u64);
    let n1001: ZW = zw_mix2(n999, zw_splat(n950), 281u64);
    let n1002: ZW = zw_mix1(n972, zw_splat(n903), 271u64);
    let n1003: ZW = zw_mix2(n973, zw_splat(n903), 271u64);
    let n1004: ZW = zw_mix1(n1002, zw_splat(n855), 272u64);
    let n1005: ZW = zw_mix2(n1003, zw_splat(n855), 272u64);
    let n1006: ZW = zw_mix1(n1004, zw_splat(n978), 280u64);
    let n1007: ZW = zw_mix2(n1005, zw_splat(n978), 280u64);
    let n1008: ZW = zw_mix1(n1006, zw_splat(n989), 281u64);
    let n1009: ZW = zw_mix2(n1007, zw_splat(n989), 281u64);
    let n1010: ZW = zw_mix1(n983, zw_splat(n903), 271u64);
    let n1011: ZW = zw_mix2(n984, zw_splat(n903), 271u64);
    let n1012: ZW = zw_mix1(n1010, zw_splat(n822), 272u64);
    let n1013: ZW = zw_mix2(n1011, zw_splat(n822), 272u64);
    let n1014: ZW = zw_mix1(n1012, zw_splat(n989), 280u64);
    let n1015: ZW = zw_mix2(n1013, zw_splat(n989), 280u64);
    let n1016: ZW = zw_mix1(n1014, zw_splat(n989), 281u64);
    let n1017: ZW = zw_mix2(n1015, zw_splat(n989), 281u64);
    let n1018: ZW = zw_mix1(n914, n868, 239u64);
    let n1019: ZW = zw_mix2(n915, n868, 239u64);
    let n1020: ZW = zw_mix1(n1018, zw_splat(n855), 246u64);
    let n1021: ZW = zw_mix2(n1019, zw_splat(n855), 246u64);
    let n1022: ZW = zw_mix1(n1020, zw_splat(n855), 247u64);
    let n1023: ZW = zw_mix2(n1021, zw_splat(n855), 247u64);
    let n1024: ZW = zw_mix1(n1022, zw_splat(n922), 268u64);
    let n1025: ZW = zw_mix2(n1023, zw_splat(n922), 268u64);
    let n1026: ZW = zw_mix1(n1024, zw_splat(n925), 269u64);
    let n1027: ZW = zw_mix2(n1025, zw_splat(n925), 269u64);
    let n1028: ZW = zw_mix1(n1026, zw_splat(n903), 270u64);
    let n1029: ZW = zw_mix2(n1027, zw_splat(n903), 270u64);
    let n1030: ZW = zw_mix1(n1028, zw_splat(n825), 271u64);
    let n1031: ZW = zw_mix2(n1029, zw_splat(n825), 271u64);
    let n1032: ZW = zw_mix1(n1030, zw_splat(n822), 272u64);
    let n1033: ZW = zw_mix2(n1031, zw_splat(n822), 272u64);
    let n1034: ZW = zw_mix1(n1032, zw_splat(n830), 280u64);
    let n1035: ZW = zw_mix2(n1033, zw_splat(n830), 280u64);
    let n1036: ZW = zw_mix1(n1034, zw_splat(n825), 281u64);
    let n1037: ZW = zw_mix2(n1035, zw_splat(n825), 281u64);
    let n1038: ZW = zw_mix1(n1026, zw_splat(n938), 270u64);
    let n1039: ZW = zw_mix2(n1027, zw_splat(n938), 270u64);
    let n1040: ZW = zw_mix1(n1038, zw_splat(n825), 271u64);
    let n1041: ZW = zw_mix2(n1039, zw_splat(n825), 271u64);
    let n1042: ZW = zw_mix1(n1040, zw_splat(n855), 272u64);
    let n1043: ZW = zw_mix2(n1041, zw_splat(n855), 272u64);
    let n1044: ZW = zw_mix1(n1042, zw_splat(n945), 280u64);
    let n1045: ZW = zw_mix2(n1043, zw_splat(n945), 280u64);
    let n1046: ZW = zw_mix1(n1044, zw_splat(n825), 281u64);
    let n1047: ZW = zw_mix2(n1045, zw_splat(n825), 281u64);
    let n1048: ZW = zw_mix1(n1032, zw_splat(n950), 280u64);
    let n1049: ZW = zw_mix2(n1033, zw_splat(n950), 280u64);
    let n1050: ZW = zw_mix1(n1048, zw_splat(n825), 281u64);
    let n1051: ZW = zw_mix2(n1049, zw_splat(n825), 281u64);
    let n1052: ZW = zw_mix1(n1022, zw_splat(n925), 268u64);
    let n1053: ZW = zw_mix2(n1023, zw_splat(n925), 268u64);
    let n1054: ZW = zw_mix1(n1052, zw_splat(n922), 269u64);
    let n1055: ZW = zw_mix2(n1053, zw_splat(n922), 269u64);
    let n1056: ZW = zw_mix1(n1054, zw_splat(n825), 270u64);
    let n1057: ZW = zw_mix2(n1055, zw_splat(n825), 270u64);
    let n1058: ZW = zw_mix1(n1056, zw_splat(n961), 271u64);
    let n1059: ZW = zw_mix2(n1057, zw_splat(n961), 271u64);
    let n1060: ZW = zw_mix1(n1058, zw_splat(n822), 272u64);
    let n1061: ZW = zw_mix2(n1059, zw_splat(n822), 272u64);
    let n1062: ZW = zw_mix1(n1060, zw_splat(n825), 280u64);
    let n1063: ZW = zw_mix2(n1061, zw_splat(n825), 280u64);
    let n1064: ZW = zw_mix1(n1062, zw_splat(n945), 281u64);
    let n1065: ZW = zw_mix2(n1063, zw_splat(n945), 281u64);
    let n1066: ZW = zw_mix1(n1052, zw_splat(n925), 269u64);
    let n1067: ZW = zw_mix2(n1053, zw_splat(n925), 269u64);
    let n1068: ZW = zw_mix1(n1066, zw_splat(n938), 270u64);
    let n1069: ZW = zw_mix2(n1067, zw_splat(n938), 270u64);
    let n1070: ZW = zw_mix1(n1068, zw_splat(n961), 271u64);
    let n1071: ZW = zw_mix2(n1069, zw_splat(n961), 271u64);
    let n1072: ZW = zw_mix1(n1070, zw_splat(n855), 272u64);
    let n1073: ZW = zw_mix2(n1071, zw_splat(n855), 272u64);
    let n1074: ZW = zw_mix1(n1072, zw_splat(n978), 280u64);
    let n1075: ZW = zw_mix2(n1073, zw_splat(n978), 280u64);
    let n1076: ZW = zw_mix1(n1074, zw_splat(n978), 281u64);
    let n1077: ZW = zw_mix2(n1075, zw_splat(n978), 281u64);
    let n1078: ZW = zw_mix1(n1066, zw_splat(n903), 270u64);
    let n1079: ZW = zw_mix2(n1067, zw_splat(n903), 270u64);
    let n1080: ZW = zw_mix1(n1078, zw_splat(n961), 271u64);
    let n1081: ZW = zw_mix2(n1079, zw_splat(n961), 271u64);
    let n1082: ZW = zw_mix1(n1080, zw_splat(n822), 272u64);
    let n1083: ZW = zw_mix2(n1081, zw_splat(n822), 272u64);
    let n1084: ZW = zw_mix1(n1082, zw_splat(n989), 280u64);
    let n1085: ZW = zw_mix2(n1083, zw_splat(n989), 280u64);
    let n1086: ZW = zw_mix1(n1084, zw_splat(n978), 281u64);
    let n1087: ZW = zw_mix2(n1085, zw_splat(n978), 281u64);
    let n1088: ZW = zw_mix1(n1056, zw_splat(n903), 271u64);
    let n1089: ZW = zw_mix2(n1057, zw_splat(n903), 271u64);
    let n1090: ZW = zw_mix1(n1088, zw_splat(n822), 272u64);
    let n1091: ZW = zw_mix2(n1089, zw_splat(n822), 272u64);
    let n1092: ZW = zw_mix1(n1090, zw_splat(n825), 280u64);
    let n1093: ZW = zw_mix2(n1091, zw_splat(n825), 280u64);
    let n1094: ZW = zw_mix1(n1092, zw_splat(n950), 281u64);
    let n1095: ZW = zw_mix2(n1093, zw_splat(n950), 281u64);
    let n1096: ZW = zw_mix1(n1068, zw_splat(n903), 271u64);
    let n1097: ZW = zw_mix2(n1069, zw_splat(n903), 271u64);
    let n1098: ZW = zw_mix1(n1096, zw_splat(n855), 272u64);
    let n1099: ZW = zw_mix2(n1097, zw_splat(n855), 272u64);
    let n1100: ZW = zw_mix1(n1098, zw_splat(n978), 280u64);
    let n1101: ZW = zw_mix2(n1099, zw_splat(n978), 280u64);
    let n1102: ZW = zw_mix1(n1100, zw_splat(n989), 281u64);
    let n1103: ZW = zw_mix2(n1101, zw_splat(n989), 281u64);
    let n1104: ZW = zw_mix1(n1078, zw_splat(n903), 271u64);
    let n1105: ZW = zw_mix2(n1079, zw_splat(n903), 271u64);
    let n1106: ZW = zw_mix1(n1104, zw_splat(n822), 272u64);
    let n1107: ZW = zw_mix2(n1105, zw_splat(n822), 272u64);
    let n1108: ZW = zw_mix1(n1106, zw_splat(n989), 280u64);
    let n1109: ZW = zw_mix2(n1107, zw_splat(n989), 280u64);
    let n1110: ZW = zw_mix1(n1108, zw_splat(n989), 281u64);
    let n1111: ZW = zw_mix2(n1109, zw_splat(n989), 281u64);
    let n1112: ZW = zw_mix1(zw_splat(11400714819323198485u64), n819, 20u64);
    let n1113: ZW = zw_mix2(zw_splat(11562461410679940143u64), n819, 20u64);
    let n1114: ZW = zw_mix1(n1112, zw_splat(n822), 41u64);
    let n1115: ZW = zw_mix2(n1113, zw_splat(n822), 41u64);
    let n1116: u64 = mix64(11400714819323198485u64 ^ mix64(n903 ^ 20u64));
    let n1117: u64 = 11562461410679940143u64.wrapping_add(mix64(n903.wrapping_mul((20u64 << 1) | 1)));
    let n1118: u64 = mix64(n1116 ^ mix64(n855 ^ 41u64));
    let n1119: u64 = n1117.wrapping_add(mix64(n855.wrapping_mul((41u64 << 1) | 1)));
    let n1120: ZW = zw_bits_b(n691);
    let n1121: ZW = zw_mix1(n1112, n1120, 38u64);
    let n1122: ZW = zw_mix2(n1113, n1120, 38u64);
    let n1123: ZW = zw_bits_n(n690);
    let n1124: ZW = zw_mix1(n1121, n1123, 39u64);
    let n1125: ZW = zw_mix2(n1122, n1123, 39u64);
    let n1126: ZW = zw_bits_b(n699);
    let n1127: ZW = zw_mix1(n1112, n1126, 38u64);
    let n1128: ZW = zw_mix2(n1113, n1126, 38u64);
    let n1129: ZW = zw_bits_n(n698);
    let n1130: ZW = zw_mix1(n1127, n1129, 39u64);
    let n1131: ZW = zw_mix2(n1128, n1129, 39u64);
    let n1132: ZW = zw_bits_b(n707);
    let n1133: ZW = zw_mix1(n1112, n1132, 38u64);
    let n1134: ZW = zw_mix2(n1113, n1132, 38u64);
    let n1135: ZW = zw_bits_n(n706);
    let n1136: ZW = zw_mix1(n1133, n1135, 39u64);
    let n1137: ZW = zw_mix2(n1134, n1135, 39u64);
    let n1138: ZW = zw_bits_b(n715);
    let n1139: ZW = zw_mix1(n1112, n1138, 38u64);
    let n1140: ZW = zw_mix2(n1113, n1138, 38u64);
    let n1141: ZW = zw_bits_n(n714);
    let n1142: ZW = zw_mix1(n1139, n1141, 39u64);
    let n1143: ZW = zw_mix2(n1140, n1141, 39u64);
    let n1144: ZW = zw_bits_b(n723);
    let n1145: ZW = zw_mix1(n1112, n1144, 38u64);
    let n1146: ZW = zw_mix2(n1113, n1144, 38u64);
    let n1147: ZW = zw_bits_n(n722);
    let n1148: ZW = zw_mix1(n1145, n1147, 39u64);
    let n1149: ZW = zw_mix2(n1146, n1147, 39u64);
    let n1150: ZW = zw_bits_b(n731);
    let n1151: ZW = zw_mix1(n1112, n1150, 38u64);
    let n1152: ZW = zw_mix2(n1113, n1150, 38u64);
    let n1153: ZW = zw_bits_n(n730);
    let n1154: ZW = zw_mix1(n1151, n1153, 39u64);
    let n1155: ZW = zw_mix2(n1152, n1153, 39u64);
    let n1156: ZW = zw_mix1(zw_splat(n1116), n1120, 38u64);
    let n1157: ZW = zw_mix2(zw_splat(n1117), n1120, 38u64);
    let n1158: ZW = zw_mix1(n1156, n1123, 39u64);
    let n1159: ZW = zw_mix2(n1157, n1123, 39u64);
    let n1160: ZW = zw_mix1(zw_splat(n1116), n1126, 38u64);
    let n1161: ZW = zw_mix2(zw_splat(n1117), n1126, 38u64);
    let n1162: ZW = zw_mix1(n1160, n1129, 39u64);
    let n1163: ZW = zw_mix2(n1161, n1129, 39u64);
    let n1164: ZW = zw_mix1(zw_splat(n1116), n1132, 38u64);
    let n1165: ZW = zw_mix2(zw_splat(n1117), n1132, 38u64);
    let n1166: ZW = zw_mix1(n1164, n1135, 39u64);
    let n1167: ZW = zw_mix2(n1165, n1135, 39u64);
    let n1168: ZW = zw_mix1(zw_splat(n1116), n1138, 38u64);
    let n1169: ZW = zw_mix2(zw_splat(n1117), n1138, 38u64);
    let n1170: ZW = zw_mix1(n1168, n1141, 39u64);
    let n1171: ZW = zw_mix2(n1169, n1141, 39u64);
    let n1172: ZW = zw_mix1(zw_splat(n1116), n1144, 38u64);
    let n1173: ZW = zw_mix2(zw_splat(n1117), n1144, 38u64);
    let n1174: ZW = zw_mix1(n1172, n1147, 39u64);
    let n1175: ZW = zw_mix2(n1173, n1147, 39u64);
    let n1176: ZW = zw_mix1(zw_splat(n1116), n1150, 38u64);
    let n1177: ZW = zw_mix2(zw_splat(n1117), n1150, 38u64);
    let n1178: ZW = zw_mix1(n1176, n1153, 39u64);
    let n1179: ZW = zw_mix2(n1177, n1153, 39u64);
    let n1180: ZW = zw_bits_n(n741);
    let n1181: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1180, 20u64);
    let n1182: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1180, 20u64);
    let n1183: ZW = zw_mix1(n1181, n813, 39u64);
    let n1184: ZW = zw_mix2(n1182, n813, 39u64);
    let n1185: ZW = zw_bits_n(n802);
    let n1186: ZW = zw_mix1(n1183, n1185, 233u64);
    let n1187: ZW = zw_mix2(n1184, n1185, 233u64);
    let n1188: ZW = zw_bits_n(n771);
    let n1189: ZW = zw_mix1(n1186, n1188, 244u64);
    let n1190: ZW = zw_mix2(n1187, n1188, 244u64);
    let n1191: ZW = zw_bits_n(n803);
    let n1192: ZW = zw_mix1(n1189, n1191, 245u64);
    let n1193: ZW = zw_mix2(n1190, n1191, 245u64);
    let n1194: ZW = zw_bits_n(n804);
    let n1195: ZW = zw_mix1(n1192, n1194, 249u64);
    let n1196: ZW = zw_mix2(n1193, n1194, 249u64);
    let n1197: ZW = zw_bits_n(n805);
    let n1198: ZW = zw_mix1(n1195, n1197, 269u64);
    let n1199: ZW = zw_mix2(n1196, n1197, 269u64);
    let n1200: ZW = zw_bits_n(n806);
    let n1201: ZW = zw_mix1(n1198, n1200, 271u64);
    let n1202: ZW = zw_mix2(n1199, n1200, 271u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v0_b0: bool = !n67 || !n66 || !n65 || !n64;
    let live_v0_b0: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n360);
    let ok_v1_b1: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v1_b1: bool = !n67 || !n66 || !n65 || !n64;
    let live_v1_b1: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n388);
    let ok_v2_b2: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v2_b2: bool = !n67 || !n66 || !n65 || !n64;
    let live_v2_b2: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n413);
    let ok_v16_b3: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v16_b3: bool = !n67 || !n66 || !n65 || !n64;
    let live_v16_b3: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n442);
    let ok_v17_b4: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v17_b4: bool = !n67 || !n66 || !n65 || !n64;
    let live_v17_b4: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n471);
    let ok_v18_b5: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v18_b5: bool = !n67 || !n66 || !n65 || !n64;
    let live_v18_b5: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n500);
    let ok_v32_b6: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v32_b6: bool = !n67 || !n66 || !n65 || !n64;
    let live_v32_b6: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v33_b7: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v33_b7: bool = !n67 || !n66 || !n65 || !n64;
    let live_v33_b7: u16 = ALL & zb_holds(n357) & zb_holds(n388);
    let ok_v34_b8: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v34_b8: bool = !n67 || !n66 || !n65 || !n64;
    let live_v34_b8: u16 = ALL & zb_holds(n357) & zb_holds(n413);
    let ok_v36_b9: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v36_b9: bool = !n67 || !n66 || !n65 || !n64;
    let live_v36_b9: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v37_b10: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v37_b10: bool = !n67 || !n66 || !n65 || !n64;
    let live_v37_b10: u16 = ALL & zb_holds(n357) & zb_holds(n388);
    let ok_v38_b11: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v38_b11: bool = !n67 || !n66 || !n65 || !n64;
    let live_v38_b11: u16 = ALL & zb_holds(n357) & zb_holds(n413);
    let ok_v40_b12: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v40_b12: bool = !n67 || !n66 || !n65 || !n64;
    let live_v40_b12: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v41_b13: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v41_b13: bool = !n67 || !n66 || !n65 || !n64;
    let live_v41_b13: u16 = ALL & zb_holds(n357) & zb_holds(n388);
    let ok_v42_b14: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v42_b14: bool = !n67 || !n66 || !n65 || !n64;
    let live_v42_b14: u16 = ALL & zb_holds(n357) & zb_holds(n413);
    let ok_v48_b15: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v48_b15: bool = !n67 || !n66 || !n65 || !n64;
    let live_v48_b15: u16 = ALL & zb_holds(n357) & zb_holds(n442);
    let ok_v49_b16: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v49_b16: bool = !n67 || !n66 || !n65 || !n64;
    let live_v49_b16: u16 = ALL & zb_holds(n357) & zb_holds(n471);
    let ok_v50_b17: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v50_b17: bool = !n67 || !n66 || !n65 || !n64;
    let live_v50_b17: u16 = ALL & zb_holds(n357) & zb_holds(n500);
    let ok_v52_b18: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v52_b18: bool = !n67 || !n66 || !n65 || !n64;
    let live_v52_b18: u16 = ALL & zb_holds(n357) & zb_holds(n442);
    let ok_v53_b19: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v53_b19: bool = !n67 || !n66 || !n65 || !n64;
    let live_v53_b19: u16 = ALL & zb_holds(n357) & zb_holds(n471);
    let ok_v54_b20: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v54_b20: bool = !n67 || !n66 || !n65 || !n64;
    let live_v54_b20: u16 = ALL & zb_holds(n357) & zb_holds(n500);
    let ok_v56_b21: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v56_b21: bool = !n67 || !n66 || !n65 || !n64;
    let live_v56_b21: u16 = ALL & zb_holds(n357) & zb_holds(n442);
    let ok_v57_b22: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v57_b22: bool = !n67 || !n66 || !n65 || !n64;
    let live_v57_b22: u16 = ALL & zb_holds(n357) & zb_holds(n471);
    let ok_v58_b23: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n312);
    let bd_v58_b23: bool = !n67 || !n66 || !n65 || !n64;
    let live_v58_b23: u16 = ALL & zb_holds(n357) & zb_holds(n500);
    let ok_v0_b24: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v0_b24: bool = !n67 || !n66 || !n65 || !n64;
    let live_v0_b24: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n551);
    let ok_v1_b25: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v1_b25: bool = !n67 || !n66 || !n65 || !n64;
    let live_v1_b25: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n572);
    let ok_v2_b26: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v2_b26: bool = !n67 || !n66 || !n65 || !n64;
    let live_v2_b26: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n592);
    let ok_v16_b27: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v16_b27: bool = !n67 || !n66 || !n65 || !n64;
    let live_v16_b27: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n621);
    let ok_v17_b28: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v17_b28: bool = !n67 || !n66 || !n65 || !n64;
    let live_v17_b28: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n650);
    let ok_v18_b29: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v18_b29: bool = !n67 || !n66 || !n65 || !n64;
    let live_v18_b29: u16 = ALL & zb_holds(n96) & zb_holds(n357) & zb_holds(n679);
    let ok_v32_b30: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v32_b30: bool = !n67 || !n66 || !n65 || !n64;
    let live_v32_b30: u16 = ALL & zb_holds(n357) & zb_holds(n551);
    let ok_v33_b31: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v33_b31: bool = !n67 || !n66 || !n65 || !n64;
    let live_v33_b31: u16 = ALL & zb_holds(n357) & zb_holds(n572);
    let ok_v34_b32: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v34_b32: bool = !n67 || !n66 || !n65 || !n64;
    let live_v34_b32: u16 = ALL & zb_holds(n357) & zb_holds(n592);
    let ok_v48_b33: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v48_b33: bool = !n67 || !n66 || !n65 || !n64;
    let live_v48_b33: u16 = ALL & zb_holds(n357) & zb_holds(n621);
    let ok_v49_b34: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v49_b34: bool = !n67 || !n66 || !n65 || !n64;
    let live_v49_b34: u16 = ALL & zb_holds(n357) & zb_holds(n650);
    let ok_v50_b35: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n533);
    let bd_v50_b35: bool = !n67 || !n66 || !n65 || !n64;
    let live_v50_b35: u16 = ALL & zb_holds(n357) & zb_holds(n679);
    let ok_v0_b36: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n693);
    let bd_v0_b36: bool = !n67 || !n66 || !n65 || !n64;
    let live_v0_b36: u16 = ALL & zb_holds(n96) & zb_holds(n692);
    let ok_v1_b37: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n701);
    let bd_v1_b37: bool = !n67 || !n66 || !n65 || !n64;
    let live_v1_b37: u16 = ALL & zb_holds(n96) & zb_holds(n700);
    let ok_v2_b38: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n709);
    let bd_v2_b38: bool = !n67 || !n66 || !n65 || !n64;
    let live_v2_b38: u16 = ALL & zb_holds(n96) & zb_holds(n708);
    let ok_v16_b39: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n717);
    let bd_v16_b39: bool = !n67 || !n66 || !n65 || !n64;
    let live_v16_b39: u16 = ALL & zb_holds(n96) & zb_holds(n716);
    let ok_v17_b40: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n725);
    let bd_v17_b40: bool = !n67 || !n66 || !n65 || !n64;
    let live_v17_b40: u16 = ALL & zb_holds(n96) & zb_holds(n724);
    let ok_v18_b41: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n733);
    let bd_v18_b41: bool = !n67 || !n66 || !n65 || !n64;
    let live_v18_b41: u16 = ALL & zb_holds(n96) & zb_holds(n732);
    let ok_v32_b42: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n693);
    let bd_v32_b42: bool = !n67 || !n66 || !n65 || !n64;
    let live_v32_b42: u16 = ALL & zb_holds(n692);
    let ok_v33_b43: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n701);
    let bd_v33_b43: bool = !n67 || !n66 || !n65 || !n64;
    let live_v33_b43: u16 = ALL & zb_holds(n700);
    let ok_v34_b44: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n709);
    let bd_v34_b44: bool = !n67 || !n66 || !n65 || !n64;
    let live_v34_b44: u16 = ALL & zb_holds(n708);
    let ok_v48_b45: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n717);
    let bd_v48_b45: bool = !n67 || !n66 || !n65 || !n64;
    let live_v48_b45: u16 = ALL & zb_holds(n716);
    let ok_v49_b46: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n725);
    let bd_v49_b46: bool = !n67 || !n66 || !n65 || !n64;
    let live_v49_b46: u16 = ALL & zb_holds(n724);
    let ok_v50_b47: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57) & zb_holds(n733);
    let bd_v50_b47: bool = !n67 || !n66 || !n65 || !n64;
    let live_v50_b47: u16 = ALL & zb_holds(n732);
    let ok_v0_b48: u16 = ALL & zb_holds(n77) & zb_holds(n76) & zb_holds(n75) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n70) & zb_holds(n69) & zb_holds(n68) & zb_holds(n63) & zb_holds(n62) & zb_holds(r_c232) & zb_holds(n61) & zb_holds(n60) & zb_holds(n59) & zb_holds(n58) & zb_holds(n56) & zb_holds(n57);
    let bd_v0_b48: bool = !n67 || !n66 || !n65 || !n64;
    let live_v0_b48: u16 = ALL & zb_holds(n810);
    let sh0 = KShared0 {
        c39: r_c39,
        c254: n129,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
    };
    let sh3 = KShared3 {
        c39: r_c39,
        c20: n741,
        c233: n802,
        c269: n805,
        c271: n806,
        c244: n771,
        c245: n803,
        c249: n804,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n326,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: n337,
        h1: n853, h2: n854,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(true),
        c239: n326,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n366,
        c281: n337,
        h1: n861, h2: n862,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n326,
        c246: zb_splat(false),
        c247: zb_splat(false),
        c280: n391,
        c281: n337,
        h1: n866, h2: n867,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n353,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n354,
        c281: n355,
        h1: n889, h2: n890,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(true),
        c239: n353,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n385,
        c281: n355,
        h1: n896, h2: n897,
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
        c234: zn_splat(P8::from_raw(0i32)),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(0i32)),
        c237: zn_splat(P8::from_raw(65536i32)),
        c272: zb_splat(false),
        c239: n353,
        c246: zb_splat(false),
        c247: zb_splat(true),
        c280: n410,
        c281: n355,
        h1: n901, h2: n902,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n936, h2: n937,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n948, h2: n949,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n953, h2: n954,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n968, h2: n969,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n981, h2: n982,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n992, h2: n993,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n1000, h2: n1001,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1008, h2: n1009,
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
        c239: n326,
        c246: zb_splat(true),
        c247: zb_splat(false),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1016, h2: n1017,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(65536i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1036, h2: n1037,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1046, h2: n1047,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(327680i32)),
        c281: zn_splat(P8::from_raw(0i32)),
        h1: n1050, h2: n1051,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(-327680i32)),
        h1: n1064, h2: n1065,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1076, h2: n1077,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(-231700i32)),
        h1: n1086, h2: n1087,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(0i32)),
        c281: zn_splat(P8::from_raw(327680i32)),
        h1: n1094, h2: n1095,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(-231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1102, h2: n1103,
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
        c239: n353,
        c246: zb_splat(true),
        c247: zb_splat(true),
        c280: zn_splat(P8::from_raw(231700i32)),
        c281: zn_splat(P8::from_raw(231700i32)),
        h1: n1110, h2: n1111,
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
        h1: n1114, h2: n1115,
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
        h1: zw_splat(n1118), h2: zw_splat(n1119),
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n690,
        c20: r_c20,
        c38: n691,
        h1: n1124, h2: n1125,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n698,
        c20: r_c20,
        c38: n699,
        h1: n1130, h2: n1131,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n706,
        c20: r_c20,
        c38: n707,
        h1: n1136, h2: n1137,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n714,
        c20: r_c20,
        c38: n715,
        h1: n1142, h2: n1143,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n722,
        c20: r_c20,
        c38: n723,
        h1: n1148, h2: n1149,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n730,
        c20: r_c20,
        c38: n731,
        h1: n1154, h2: n1155,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n690,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n691,
        h1: n1158, h2: n1159,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n698,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n699,
        h1: n1162, h2: n1163,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n706,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n707,
        h1: n1166, h2: n1167,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n714,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n715,
        h1: n1170, h2: n1171,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n722,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n723,
        h1: n1174, h2: n1175,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n730,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n731,
        h1: n1178, h2: n1179,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n1201, h2: n1202,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
