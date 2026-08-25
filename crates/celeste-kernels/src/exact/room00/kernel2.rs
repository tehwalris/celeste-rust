// GENERATED from a TRACED frame (shape 2). Do not edit.
//
// One input shape, 4 output shapes, 60 distinct button
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
pub const SHAPE: u64 = 4373601964246994127;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c266: P8,
    pub c267: P8,
    pub c268: P8,
    pub c269: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("got_fruit[0]", "bool"),
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
    pub c171: u16,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c234: u16,
    pub c235: ZN,
    pub c264: u16,
    pub c265: u16,
    pub c270: ZN,
    pub c271: ZN,
    pub c244: u16,
    pub c272: ZN,
    pub c273: ZN,
    pub c246: ZN,
    pub c247: ZN,
    pub c274: ZN,
    pub c275: ZN,
    pub c250: ZN,
    pub c251: ZN,
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
    pub c171: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c234: u32,
    pub c235: u32,
    pub c264: u32,
    pub c265: u32,
    pub c270: u32,
    pub c271: u32,
    pub c244: u32,
    pub c272: u32,
    pub c273: u32,
    pub c246: u32,
    pub c247: u32,
    pub c274: u32,
    pub c275: u32,
    pub c250: u32,
    pub c251: u32,
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
        c266: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c267: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c268: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c269: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c171: cell("got_fruit[0]")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c234: cell("objects[0].collideable")?,
        c235: cell("objects[0].delay")?,
        c264: cell("objects[0].flip.x")?,
        c265: cell("objects[0].flip.y")?,
        c270: cell("objects[0].rem.x")?,
        c271: cell("objects[0].rem.y")?,
        c244: cell("objects[0].solids")?,
        c272: cell("objects[0].spd.x")?,
        c273: cell("objects[0].spd.y")?,
        c246: cell("objects[0].spr")?,
        c247: cell("objects[0].state")?,
        c274: cell("objects[0].target.x")?,
        c275: cell("objects[0].target.y")?,
        c250: cell("objects[0].x")?,
        c251: cell("objects[0].y")?,
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
        c171: match &b.cols[s.c171 as usize] {
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
        c234: match &b.cols[s.c234 as usize] {
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
        c235: match &b.cols[s.c235 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c264: match &b.cols[s.c264 as usize] {
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
        c265: match &b.cols[s.c265 as usize] {
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
        c246: match &b.cols[s.c246 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c247: match &b.cols[s.c247 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c274: match &b.cols[s.c274 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c275: match &b.cols[s.c275 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c250: match &b.cols[s.c250 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c251: match &b.cols[s.c251 as usize] {
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
    (176, "balloon.tile"),
    (203, "big_chest.tile"),
    (195, "chest.if_not_fruit"),
    (197, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (189, "fake_wall.if_not_fruit"),
    (190, "fake_wall.tile"),
    (179, "fall_floor.tile"),
    (185, "fly_fruit.if_not_fruit"),
    (187, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (181, "fruit.if_not_fruit"),
    (183, "fruit.tile"),
    (171, "got_fruit[0]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (192, "key.if_not_fruit"),
    (193, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (270, "objects[0].dash_accel.x"),
    (271, "objects[0].dash_accel.y"),
    (236, "objects[0].dash_effect_time"),
    (272, "objects[0].dash_target.x"),
    (273, "objects[0].dash_target.y"),
    (238, "objects[0].dash_time"),
    (239, "objects[0].djump"),
    (274, "objects[0].flip.x"),
    (275, "objects[0].flip.y"),
    (241, "objects[0].grace"),
    (276, "objects[0].hitbox.h"),
    (277, "objects[0].hitbox.w"),
    (278, "objects[0].hitbox.x"),
    (279, "objects[0].hitbox.y"),
    (248, "objects[0].p_dash"),
    (249, "objects[0].p_jump"),
    (280, "objects[0].rem.x"),
    (281, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (282, "objects[0].spd.x"),
    (283, "objects[0].spd.y"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (173, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Arr(&[171]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 172), (8, 173), (6, 174)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 175), (8, 176), (6, 177)]),
    SCell::Obj(&[(5, 178), (8, 179), (6, 180)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 181), (5, 182), (8, 183), (6, 184)]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(9, 192), (8, 193), (6, 194)]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(5, 199), (6, 200)]),
    SCell::Obj(&[(7, 201), (5, 202), (8, 203)]),
    SCell::Obj(&[(7, 204), (5, 205)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (28, 235), (33, 236), (27, 237), (26, 238), (30, 239), (14, 240), (29, 241), (15, 242), (19, 243), (18, 244), (22, 245), (23, 246), (24, 247), (32, 248), (31, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
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
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(1, 274), (2, 275)]),
    SCell::Obj(&[(17, 276), (16, 277), (1, 278), (2, 279)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 280), (2, 281)]),
    SCell::Obj(&[(1, 282), (2, 283)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (172, 212),
    (174, 213),
    (175, 214),
    (177, 215),
    (178, 216),
    (180, 217),
    (182, 218),
    (184, 219),
    (186, 220),
    (188, 221),
    (191, 222),
    (194, 223),
    (196, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (202, 229),
    (204, 230),
    (205, 231),
    (232, 257),
    (233, 258),
    (235, 259),
    (237, 260),
    (240, 261),
    (242, 262),
    (243, 263),
    (244, 264),
    (245, 265),
    (246, 266),
    (247, 267),
    (250, 268),
    (252, 269),
    (254, 93),
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
    pub c256: ZN,
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
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c274: ZB,
    pub c241: ZN,
    pub c248: ZB,
    pub c249: ZB,
    pub c282: ZN,
    pub c283: ZN,
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
    (176, "balloon.tile"),
    (203, "big_chest.tile"),
    (195, "chest.if_not_fruit"),
    (197, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (189, "fake_wall.if_not_fruit"),
    (190, "fake_wall.tile"),
    (179, "fall_floor.tile"),
    (185, "fly_fruit.if_not_fruit"),
    (187, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (181, "fruit.if_not_fruit"),
    (183, "fruit.tile"),
    (171, "got_fruit[0]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (192, "key.if_not_fruit"),
    (193, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (173, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Arr(&[171]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 172), (8, 173), (6, 174)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 175), (8, 176), (6, 177)]),
    SCell::Obj(&[(5, 178), (8, 179), (6, 180)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 181), (5, 182), (8, 183), (6, 184)]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(9, 192), (8, 193), (6, 194)]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(5, 199), (6, 200)]),
    SCell::Obj(&[(7, 201), (5, 202), (8, 203)]),
    SCell::Obj(&[(7, 204), (5, 205)]),
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
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (172, 211),
    (174, 212),
    (175, 213),
    (177, 214),
    (178, 215),
    (180, 216),
    (182, 217),
    (184, 218),
    (186, 219),
    (188, 220),
    (191, 221),
    (194, 222),
    (196, 223),
    (198, 224),
    (199, 225),
    (200, 226),
    (201, 227),
    (202, 228),
    (204, 229),
    (205, 230),
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
    (178, "balloon.tile"),
    (205, "big_chest.tile"),
    (197, "chest.if_not_fruit"),
    (199, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (191, "fake_wall.if_not_fruit"),
    (192, "fake_wall.tile"),
    (181, "fall_floor.tile"),
    (187, "fly_fruit.if_not_fruit"),
    (189, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (183, "fruit.if_not_fruit"),
    (185, "fruit.tile"),
    (173, "got_fruit[0]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (194, "key.if_not_fruit"),
    (195, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (236, "objects[0].collideable"),
    (237, "objects[0].delay"),
    (266, "objects[0].flip.x"),
    (267, "objects[0].flip.y"),
    (268, "objects[0].hitbox.h"),
    (269, "objects[0].hitbox.w"),
    (270, "objects[0].hitbox.x"),
    (271, "objects[0].hitbox.y"),
    (272, "objects[0].rem.x"),
    (273, "objects[0].rem.y"),
    (246, "objects[0].solids"),
    (274, "objects[0].spd.x"),
    (275, "objects[0].spd.y"),
    (248, "objects[0].spr"),
    (249, "objects[0].state"),
    (276, "objects[0].target.x"),
    (277, "objects[0].target.y"),
    (159, "objects[0].type.tile"),
    (252, "objects[0].x"),
    (253, "objects[0].y"),
    (43, "pause_player"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (175, "spring.tile"),
    (89, "start_game"),
    (90, "start_game_flash"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_2: &[(u32, &str)] = &[
    (147, "__button_states[0]"),
    (148, "__button_states[1]"),
    (149, "__button_states[2]"),
    (150, "__button_states[3]"),
    (151, "__button_states[4]"),
    (152, "__button_states[5]"),
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
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[147, 148, 149, 150, 151, 152]),
    SCell::Arr(&[153, 154]),
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
    SCell::Arr(&[173]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Obj(&[(5, 180), (8, 181), (6, 182)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 183), (5, 184), (8, 185), (6, 186)]),
    SCell::Obj(&[(9, 187), (5, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (8, 195), (6, 196)]),
    SCell::Obj(&[(9, 197), (5, 198), (8, 199), (6, 200)]),
    SCell::Obj(&[(5, 201), (6, 202)]),
    SCell::Obj(&[(7, 203), (5, 204), (8, 205)]),
    SCell::Obj(&[(7, 206), (5, 207)]),
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
    SCell::Obj(&[(21, 234), (20, 235), (11, 236), (35, 237), (14, 238), (15, 239), (19, 240), (18, 241), (22, 242), (23, 243), (24, 244), (4, 245), (12, 246), (3, 247), (13, 248), (25, 249), (34, 250), (0, 251), (1, 252), (2, 253)]),
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
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 266), (2, 267)]),
    SCell::Obj(&[(17, 268), (16, 269), (1, 270), (2, 271)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(1, 274), (2, 275)]),
    SCell::Obj(&[(1, 276), (2, 277)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_2: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_2: &[(u32, u32)] = &[
    (12, 91),
    (13, 92),
    (18, 93),
    (19, 94),
    (21, 95),
    (22, 96),
    (23, 97),
    (24, 98),
    (25, 99),
    (26, 100),
    (27, 101),
    (28, 102),
    (29, 103),
    (30, 104),
    (31, 105),
    (32, 106),
    (33, 107),
    (34, 108),
    (35, 109),
    (36, 110),
    (37, 111),
    (40, 112),
    (50, 113),
    (51, 114),
    (52, 115),
    (53, 116),
    (54, 117),
    (55, 118),
    (56, 119),
    (57, 120),
    (58, 121),
    (59, 122),
    (60, 123),
    (61, 124),
    (62, 125),
    (63, 126),
    (64, 127),
    (65, 128),
    (66, 129),
    (67, 130),
    (68, 131),
    (69, 132),
    (70, 133),
    (71, 134),
    (72, 135),
    (73, 136),
    (74, 137),
    (75, 138),
    (76, 139),
    (77, 140),
    (78, 141),
    (79, 142),
    (80, 143),
    (81, 144),
    (82, 145),
    (83, 146),
    (153, 208),
    (155, 209),
    (156, 210),
    (157, 211),
    (158, 212),
    (160, 213),
    (163, 96),
    (164, 118),
    (165, 120),
    (166, 121),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 126),
    (171, 127),
    (172, 129),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (180, 218),
    (182, 219),
    (184, 220),
    (186, 221),
    (188, 222),
    (190, 223),
    (193, 224),
    (196, 225),
    (198, 226),
    (200, 227),
    (201, 228),
    (202, 229),
    (203, 230),
    (204, 231),
    (206, 232),
    (207, 233),
    (234, 254),
    (235, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (242, 260),
    (243, 261),
    (244, 262),
    (245, 263),
    (247, 264),
    (250, 265),
    (251, 96),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c84: ZN,
    pub c86: ZN,
    pub c237: ZN,
    pub c275: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c277: ZN,
    pub c253: ZN,
    pub c161: ZN,
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
    (176, "balloon.tile"),
    (203, "big_chest.tile"),
    (195, "chest.if_not_fruit"),
    (197, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (189, "fake_wall.if_not_fruit"),
    (190, "fake_wall.tile"),
    (179, "fall_floor.tile"),
    (185, "fly_fruit.if_not_fruit"),
    (187, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (181, "fruit.if_not_fruit"),
    (183, "fruit.tile"),
    (171, "got_fruit[0]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (192, "key.if_not_fruit"),
    (193, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (235, "objects[0].delay"),
    (264, "objects[0].flip.x"),
    (265, "objects[0].flip.y"),
    (266, "objects[0].hitbox.h"),
    (267, "objects[0].hitbox.w"),
    (268, "objects[0].hitbox.x"),
    (269, "objects[0].hitbox.y"),
    (270, "objects[0].rem.x"),
    (271, "objects[0].rem.y"),
    (244, "objects[0].solids"),
    (272, "objects[0].spd.x"),
    (273, "objects[0].spd.y"),
    (246, "objects[0].spr"),
    (247, "objects[0].state"),
    (274, "objects[0].target.x"),
    (275, "objects[0].target.y"),
    (157, "objects[0].type.tile"),
    (250, "objects[0].x"),
    (251, "objects[0].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (173, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Arr(&[171]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 172), (8, 173), (6, 174)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 175), (8, 176), (6, 177)]),
    SCell::Obj(&[(5, 178), (8, 179), (6, 180)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 181), (5, 182), (8, 183), (6, 184)]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(9, 192), (8, 193), (6, 194)]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(5, 199), (6, 200)]),
    SCell::Obj(&[(7, 201), (5, 202), (8, 203)]),
    SCell::Obj(&[(7, 204), (5, 205)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (35, 235), (14, 236), (15, 237), (19, 238), (18, 239), (22, 240), (23, 241), (24, 242), (4, 243), (12, 244), (3, 245), (13, 246), (25, 247), (34, 248), (0, 249), (1, 250), (2, 251)]),
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
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 264), (2, 265)]),
    SCell::Obj(&[(17, 266), (16, 267), (1, 268), (2, 269)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 270), (2, 271)]),
    SCell::Obj(&[(1, 272), (2, 273)]),
    SCell::Obj(&[(1, 274), (2, 275)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (172, 212),
    (174, 213),
    (175, 214),
    (177, 215),
    (178, 216),
    (180, 217),
    (182, 218),
    (184, 219),
    (186, 220),
    (188, 221),
    (191, 222),
    (194, 223),
    (196, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (202, 229),
    (204, 230),
    (205, 231),
    (232, 252),
    (233, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (242, 260),
    (243, 261),
    (245, 262),
    (248, 263),
    (249, 94),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c84: ZN,
    pub c86: ZN,
    pub c235: ZN,
    pub c273: ZN,
    pub c246: ZN,
    pub c247: ZN,
    pub c275: ZN,
    pub c251: ZN,
    pub c159: ZN,
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

/// An EMPTY accumulator with outcome 0's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append0`.
pub fn acc0(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_0, OUT_GLOBALS_0, OUT_PTRS_0, 0, cart, cache);
    b.cols[176] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[203] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[181] = Col::U(AV::Bool(true));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[171] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[192] = Col::U(AV::Bool(true));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[272] = Col::N(Vec::new());
    b.cols[273] = Col::N(Vec::new());
    b.cols[238] = Col::N(Vec::new());
    b.cols[239] = Col::N(Vec::new());
    b.cols[274] = Col::V(Vec::new());
    b.cols[275] = Col::U(AV::Bool(false));
    b.cols[241] = Col::N(Vec::new());
    b.cols[276] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[248] = Col::V(Vec::new());
    b.cols[249] = Col::V(Vec::new());
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[281] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[255] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[256] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
        if let Col::N(v) = &mut acc.cols[270] { v.push(kv.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(kv.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[236] { v.push(kv.c236.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(kv.c272.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(kv.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[238] { v.push(kv.c238.lane(i)); }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::V(v) = &mut acc.cols[274] {
            v.push(if kv.c274.known & (1 << i) != 0 {
                AV::Bool(kv.c274.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[241] { v.push(kv.c241.lane(i)); }
        if let Col::V(v) = &mut acc.cols[248] {
            v.push(if kv.c248.known & (1 << i) != 0 {
                AV::Bool(kv.c248.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[249] {
            v.push(if kv.c249.known & (1 << i) != 0 {
                AV::Bool(kv.c249.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(sh.c256.lane(i)); }
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
    b.cols[176] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[203] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[181] = Col::U(AV::Bool(true));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[171] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[192] = Col::U(AV::Bool(true));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[205] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[197] = Col::U(AV::Bool(true));
    b.cols[199] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[183] = Col::U(AV::Bool(true));
    b.cols[185] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[173] = Col::U(AV::Bool(true));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[195] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[236] = Col::U(AV::Bool(true));
    b.cols[237] = Col::N(Vec::new());
    b.cols[266] = Col::U(AV::Bool(false));
    b.cols[267] = Col::U(AV::Bool(false));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Bool(false));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[275] = Col::N(Vec::new());
    b.cols[248] = Col::N(Vec::new());
    b.cols[249] = Col::N(Vec::new());
    b.cols[276] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[277] = Col::N(Vec::new());
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[161] = Col::N(Vec::new());
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[89] = Col::U(AV::Bool(true));
    b.cols[90] = Col::U(AV::Num(P8::from_raw(3211264i32)));
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
        if let Col::N(v) = &mut acc.cols[237] { v.push(sh.c237.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[248] { v.push(sh.c248.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[161] { v.push(sh.c161.lane(i)); }
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
    b.cols[176] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[203] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[181] = Col::U(AV::Bool(true));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[171] = Col::U(AV::Bool(true));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[192] = Col::U(AV::Bool(true));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[235] = Col::N(Vec::new());
    b.cols[264] = Col::U(AV::Bool(false));
    b.cols[265] = Col::U(AV::Bool(false));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Bool(false));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::N(Vec::new());
    b.cols[246] = Col::N(Vec::new());
    b.cols[247] = Col::N(Vec::new());
    b.cols[274] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[275] = Col::N(Vec::new());
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[251] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::N(Vec::new());
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[235] { v.push(sh.c235.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[247] { v.push(sh.c247.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[251] { v.push(sh.c251.lane(i)); }
        if let Col::N(v) = &mut acc.cols[159] { v.push(sh.c159.lane(i)); }
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
    let r_c171: ZB = ZB { val: rin.c171, known: ALL };
    let r_c234: ZB = ZB { val: rin.c234, known: ALL };
    let r_c235: ZN = rin.c235;
    let r_c244: ZB = ZB { val: rin.c244, known: ALL };
    let r_c246: ZN = rin.c246;
    let r_c247: ZN = rin.c247;
    let r_c250: ZN = rin.c250;
    let r_c251: ZN = rin.c251;
    let r_c264: ZB = ZB { val: rin.c264, known: ALL };
    let r_c265: ZB = ZB { val: rin.c265, known: ALL };
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let r_c274: ZN = rin.c274;
    let r_c275: ZN = rin.c275;
    let n56: ZB = zb_not(r_c41);
    let n57: bool = P8::from_raw(0i32) == u.c268;
    let n58: bool = P8::from_raw(0i32) == u.c269;
    let n59: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c271);
    let n62: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n63: ZN = zn_rem(n62, zn_splat(P8::from_raw(1966080i32)));
    let n64: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n63);
    let n76: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n77: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c272);
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(196608i32)), r_c246);
    let n80: ZB = zb_not(r_c42);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n82: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n87: ZB = zb_not(r_c264);
    let n88: ZB = zb_not(r_c265);
    let n89: bool = P8::from_raw(524288i32) == u.c266;
    let n90: bool = P8::from_raw(524288i32) == u.c267;
    let n91: ZB = zb_not(r_c244);
    let n92: ZB = zn_eq(zn_splat(P8::from_raw(-262144i32)), r_c273);
    let n93: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c247);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c274);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(6291456i32)), r_c275);
    let n96: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c250);
    let n97: ZB = zb_not(r_c43);
    let n98: ZB = zb_not(r_c38);
    let n99: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n100: ZN = zn_rem(n99, zn_splat(P8::from_raw(3932160i32)));
    let n101: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n100);
    let n102: ZN = zsel_n(n101, n82, r_c86);
    let n103: ZN = zsel_n(n64, n102, r_c86);
    let n104: ZN = zsel_n(n64, n100, r_c85);
    let n105: ZN = zn_add(zn_splat(P8::from_raw(-262144i32)), r_c251);
    let n123: ZB = zb_not(n93);
    let n124: ZB = zb_and(n76, n123);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c247);
    let n126: ZB = zb_not(n125);
    let n127: ZB = zb_and(n124, n126);
    let n128: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c247);
    let n129: ZB = zb_and(n127, n128);
    let n130: ZN = zn_sub(r_c235, zn_splat(P8::from_raw(65536i32)));
    let n131: ZB = zn_lt(n130, zn_splat(P8::from_raw(0i32)));
    let n132: ZB = zb_and(n129, n131);
    let n133: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n105);
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
    let n144: ZB = zb_and(n132, n142);
    let n145: ZB = zb_and(n132, n143);
    let n146: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n141);
    let n147: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n146);
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
    let n206: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n205);
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
    let n260: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n259);
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
    let n316: ZB = zn_le(n105, zn_splat(P8::from_raw(8388608i32)));
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
    let n359: ZB = zn_lt(n105, zn_splat(P8::from_raw(-262144i32)));
    let n360: ZB = zn_ge(n105, zn_splat(P8::from_raw(-262144i32)));
    let n361: ZB = zb_and(n341, n359);
    let n362: ZB = zb_and(n341, n360);
    let n363: ZB = zb_or(n361, n362);
    let n365: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n370: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n335);
    let n371: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n372: ZB = zb_not(n371);
    let n373: ZB = zb_and(n334, n372);
    let n374: ZB = zb_and(n334, n371);
    let n375: ZB = zb_or(n373, n374);
    let n376: ZB = zb_and(n372, n375);
    let n377: ZB = zb_and(n371, n375);
    let n378: ZB = zb_or(n376, n377);
    let n379: ZB = zb_and(n371, n378);
    let n380: ZB = zb_and(n372, n378);
    let n381: ZB = zb_or(n379, n380);
    let n382: ZB = zb_and(n371, n381);
    let n383: ZB = zb_and(n372, n381);
    let n384: ZB = zb_or(n382, n383);
    let n385: ZB = zb_and(n320, n384);
    let n386: ZB = zb_and(n319, n384);
    let n387: ZB = zb_or(n385, n386);
    let n388: ZN = zsel_n(n351, n353, n370);
    let n389: ZN = zsel_n(n342, n370, n388);
    let n390: ZB = zb_and(n359, n387);
    let n391: ZB = zb_and(n360, n387);
    let n392: ZB = zb_or(n390, n391);
    let n395: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n336);
    let n396: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n337, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n397: ZB = zb_not(n396);
    let n398: ZB = zb_and(n334, n397);
    let n399: ZB = zb_and(n334, n396);
    let n400: ZB = zb_or(n398, n399);
    let n401: ZB = zb_and(n397, n400);
    let n402: ZB = zb_and(n396, n400);
    let n403: ZB = zb_or(n401, n402);
    let n404: ZB = zb_and(n396, n403);
    let n405: ZB = zb_and(n397, n403);
    let n406: ZB = zb_or(n404, n405);
    let n407: ZB = zb_and(n396, n406);
    let n408: ZB = zb_and(n397, n406);
    let n409: ZB = zb_or(n407, n408);
    let n410: ZB = zb_and(n320, n409);
    let n411: ZB = zb_and(n319, n409);
    let n412: ZB = zb_or(n410, n411);
    let n413: ZN = zsel_n(n351, n353, n395);
    let n414: ZN = zsel_n(n342, n395, n413);
    let n415: ZB = zb_and(n359, n412);
    let n416: ZB = zb_and(n360, n412);
    let n417: ZB = zb_or(n415, n416);
    let n420: ZB = zb_and(n341, n342);
    let n421: ZB = zb_and(n341, n343);
    let n422: ZB = zb_and(n345, n421);
    let n423: ZB = zb_and(n344, n421);
    let n424: ZB = zb_or(n422, n423);
    let n425: ZB = zb_and(n345, n424);
    let n426: ZB = zb_and(n344, n424);
    let n427: ZB = zb_or(n425, n426);
    let n428: ZB = zb_and(n344, n427);
    let n429: ZB = zb_and(n345, n427);
    let n430: ZB = zb_and(n347, n429);
    let n431: ZB = zb_and(n346, n429);
    let n432: ZB = zb_or(n430, n431);
    let n433: ZB = zb_and(n347, n432);
    let n434: ZB = zb_and(n346, n432);
    let n435: ZB = zb_or(n433, n434);
    let n436: ZB = zb_and(n346, n435);
    let n437: ZB = zb_and(n347, n435);
    let n438: ZB = zb_or(n436, n437);
    let n439: ZB = zb_or(n428, n438);
    let n440: ZB = zb_and(n351, n439);
    let n441: ZB = zb_and(n350, n439);
    let n442: ZB = zb_or(n440, n441);
    let n443: ZB = zb_or(n420, n442);
    let n444: ZB = zb_and(n359, n443);
    let n445: ZB = zb_and(n360, n443);
    let n446: ZB = zb_or(n444, n445);
    let n449: ZB = zb_and(n342, n387);
    let n450: ZB = zb_and(n343, n387);
    let n451: ZB = zb_and(n345, n450);
    let n452: ZB = zb_and(n344, n450);
    let n453: ZB = zb_or(n451, n452);
    let n454: ZB = zb_and(n345, n453);
    let n455: ZB = zb_and(n344, n453);
    let n456: ZB = zb_or(n454, n455);
    let n457: ZB = zb_and(n344, n456);
    let n458: ZB = zb_and(n345, n456);
    let n459: ZB = zb_and(n347, n458);
    let n460: ZB = zb_and(n346, n458);
    let n461: ZB = zb_or(n459, n460);
    let n462: ZB = zb_and(n347, n461);
    let n463: ZB = zb_and(n346, n461);
    let n464: ZB = zb_or(n462, n463);
    let n465: ZB = zb_and(n346, n464);
    let n466: ZB = zb_and(n347, n464);
    let n467: ZB = zb_or(n465, n466);
    let n468: ZB = zb_or(n457, n467);
    let n469: ZB = zb_and(n351, n468);
    let n470: ZB = zb_and(n350, n468);
    let n471: ZB = zb_or(n469, n470);
    let n472: ZB = zb_or(n449, n471);
    let n473: ZB = zb_and(n359, n472);
    let n474: ZB = zb_and(n360, n472);
    let n475: ZB = zb_or(n473, n474);
    let n478: ZB = zb_and(n342, n412);
    let n479: ZB = zb_and(n343, n412);
    let n480: ZB = zb_and(n345, n479);
    let n481: ZB = zb_and(n344, n479);
    let n482: ZB = zb_or(n480, n481);
    let n483: ZB = zb_and(n345, n482);
    let n484: ZB = zb_and(n344, n482);
    let n485: ZB = zb_or(n483, n484);
    let n486: ZB = zb_and(n344, n485);
    let n487: ZB = zb_and(n345, n485);
    let n488: ZB = zb_and(n347, n487);
    let n489: ZB = zb_and(n346, n487);
    let n490: ZB = zb_or(n488, n489);
    let n491: ZB = zb_and(n347, n490);
    let n492: ZB = zb_and(n346, n490);
    let n493: ZB = zb_or(n491, n492);
    let n494: ZB = zb_and(n346, n493);
    let n495: ZB = zb_and(n347, n493);
    let n496: ZB = zb_or(n494, n495);
    let n497: ZB = zb_or(n486, n496);
    let n498: ZB = zb_and(n351, n497);
    let n499: ZB = zb_and(n350, n497);
    let n500: ZB = zb_or(n498, n499);
    let n501: ZB = zb_or(n478, n500);
    let n502: ZB = zb_and(n359, n501);
    let n503: ZB = zb_and(n360, n501);
    let n504: ZB = zb_or(n502, n503);
    let n508: ZB = zb_and(n163, n167);
    let n509: ZB = zb_and(n176, n180);
    let n510: ZB = zb_and(n182, n189);
    let n511: ZB = zb_and(n191, n198);
    let n512: ZB = zb_or(n510, n511);
    let n513: ZB = zb_or(n509, n512);
    let n514: ZB = zb_or(n508, n513);
    let n515: ZB = zb_and(n219, n223);
    let n516: ZB = zb_and(n230, n234);
    let n517: ZB = zb_and(n236, n243);
    let n518: ZB = zb_and(n245, n252);
    let n519: ZB = zb_or(n517, n518);
    let n520: ZB = zb_or(n516, n519);
    let n521: ZB = zb_or(n515, n520);
    let n522: ZB = zb_and(n273, n277);
    let n523: ZB = zb_and(n284, n288);
    let n524: ZB = zb_and(n290, n297);
    let n525: ZB = zb_and(n299, n306);
    let n526: ZB = zb_or(n524, n525);
    let n527: ZB = zb_or(n523, n526);
    let n528: ZB = zb_or(n522, n527);
    let n529: ZB = zb_or(n521, n528);
    let n530: ZB = zb_or(n514, n529);
    let n531: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n532: ZB = zn_gt(n105, zn_splat(P8::from_raw(8388608i32)));
    let n533: ZB = zb_and(n530, n532);
    let n534: ZB = zb_and(n316, n530);
    let n535: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n531);
    let n536: ZN = zsel_n(n532, n535, n531);
    let n537: ZB = zb_or(n533, n534);
    let n538: ZB = zb_and(n314, n532);
    let n539: ZN = zsel_n(n537, n536, n531);
    let n540: ZB = zb_or(n537, n538);
    let n541: ZB = zb_or(n315, n537);
    let n542: ZB = zb_and(n320, n540);
    let n543: ZB = zb_and(n319, n540);
    let n544: ZB = zb_or(n542, n543);
    let n545: ZB = zb_and(n320, n544);
    let n546: ZB = zb_and(n319, n544);
    let n547: ZB = zb_or(n545, n546);
    let n548: ZB = zb_and(n319, n547);
    let n549: ZB = zb_and(n320, n547);
    let n550: ZB = zb_or(n548, n549);
    let n551: ZB = zb_and(n320, n550);
    let n552: ZB = zb_and(n319, n550);
    let n553: ZB = zb_or(n551, n552);
    let n554: ZB = zb_and(n320, n553);
    let n555: ZB = zb_and(n319, n553);
    let n556: ZB = zb_or(n554, n555);
    let n557: ZB = zb_and(n359, n556);
    let n558: ZB = zb_and(n360, n556);
    let n559: ZB = zb_or(n557, n558);
    let n563: ZB = zb_and(n372, n553);
    let n564: ZB = zb_and(n371, n553);
    let n565: ZB = zb_or(n563, n564);
    let n566: ZB = zb_and(n372, n565);
    let n567: ZB = zb_and(n371, n565);
    let n568: ZB = zb_or(n566, n567);
    let n569: ZB = zb_and(n371, n568);
    let n570: ZB = zb_and(n372, n568);
    let n571: ZB = zb_or(n569, n570);
    let n572: ZB = zb_and(n371, n571);
    let n573: ZB = zb_and(n372, n571);
    let n574: ZB = zb_or(n572, n573);
    let n575: ZB = zb_and(n320, n574);
    let n576: ZB = zb_and(n319, n574);
    let n577: ZB = zb_or(n575, n576);
    let n578: ZB = zb_and(n359, n577);
    let n579: ZB = zb_and(n360, n577);
    let n580: ZB = zb_or(n578, n579);
    let n583: ZB = zb_and(n397, n553);
    let n584: ZB = zb_and(n396, n553);
    let n585: ZB = zb_or(n583, n584);
    let n586: ZB = zb_and(n397, n585);
    let n587: ZB = zb_and(n396, n585);
    let n588: ZB = zb_or(n586, n587);
    let n589: ZB = zb_and(n396, n588);
    let n590: ZB = zb_and(n397, n588);
    let n591: ZB = zb_or(n589, n590);
    let n592: ZB = zb_and(n396, n591);
    let n593: ZB = zb_and(n397, n591);
    let n594: ZB = zb_or(n592, n593);
    let n595: ZB = zb_and(n320, n594);
    let n596: ZB = zb_and(n319, n594);
    let n597: ZB = zb_or(n595, n596);
    let n598: ZB = zb_and(n359, n597);
    let n599: ZB = zb_and(n360, n597);
    let n600: ZB = zb_or(n598, n599);
    let n603: ZB = zb_and(n342, n556);
    let n604: ZB = zb_and(n343, n556);
    let n605: ZB = zb_and(n345, n604);
    let n606: ZB = zb_and(n344, n604);
    let n607: ZB = zb_or(n605, n606);
    let n608: ZB = zb_and(n345, n607);
    let n609: ZB = zb_and(n344, n607);
    let n610: ZB = zb_or(n608, n609);
    let n611: ZB = zb_and(n344, n610);
    let n612: ZB = zb_and(n345, n610);
    let n613: ZB = zb_and(n347, n612);
    let n614: ZB = zb_and(n346, n612);
    let n615: ZB = zb_or(n613, n614);
    let n616: ZB = zb_and(n347, n615);
    let n617: ZB = zb_and(n346, n615);
    let n618: ZB = zb_or(n616, n617);
    let n619: ZB = zb_and(n346, n618);
    let n620: ZB = zb_and(n347, n618);
    let n621: ZB = zb_or(n619, n620);
    let n622: ZB = zb_or(n611, n621);
    let n623: ZB = zb_and(n351, n622);
    let n624: ZB = zb_and(n350, n622);
    let n625: ZB = zb_or(n623, n624);
    let n626: ZB = zb_or(n603, n625);
    let n627: ZB = zb_and(n359, n626);
    let n628: ZB = zb_and(n360, n626);
    let n629: ZB = zb_or(n627, n628);
    let n632: ZB = zb_and(n342, n577);
    let n633: ZB = zb_and(n343, n577);
    let n634: ZB = zb_and(n345, n633);
    let n635: ZB = zb_and(n344, n633);
    let n636: ZB = zb_or(n634, n635);
    let n637: ZB = zb_and(n345, n636);
    let n638: ZB = zb_and(n344, n636);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_and(n344, n639);
    let n641: ZB = zb_and(n345, n639);
    let n642: ZB = zb_and(n347, n641);
    let n643: ZB = zb_and(n346, n641);
    let n644: ZB = zb_or(n642, n643);
    let n645: ZB = zb_and(n347, n644);
    let n646: ZB = zb_and(n346, n644);
    let n647: ZB = zb_or(n645, n646);
    let n648: ZB = zb_and(n346, n647);
    let n649: ZB = zb_and(n347, n647);
    let n650: ZB = zb_or(n648, n649);
    let n651: ZB = zb_or(n640, n650);
    let n652: ZB = zb_and(n351, n651);
    let n653: ZB = zb_and(n350, n651);
    let n654: ZB = zb_or(n652, n653);
    let n655: ZB = zb_or(n632, n654);
    let n656: ZB = zb_and(n359, n655);
    let n657: ZB = zb_and(n360, n655);
    let n658: ZB = zb_or(n656, n657);
    let n661: ZB = zb_and(n342, n597);
    let n662: ZB = zb_and(n343, n597);
    let n663: ZB = zb_and(n345, n662);
    let n664: ZB = zb_and(n344, n662);
    let n665: ZB = zb_or(n663, n664);
    let n666: ZB = zb_and(n345, n665);
    let n667: ZB = zb_and(n344, n665);
    let n668: ZB = zb_or(n666, n667);
    let n669: ZB = zb_and(n344, n668);
    let n670: ZB = zb_and(n345, n668);
    let n671: ZB = zb_and(n347, n670);
    let n672: ZB = zb_and(n346, n670);
    let n673: ZB = zb_or(n671, n672);
    let n674: ZB = zb_and(n347, n673);
    let n675: ZB = zb_and(n346, n673);
    let n676: ZB = zb_or(n674, n675);
    let n677: ZB = zb_and(n346, n676);
    let n678: ZB = zb_and(n347, n676);
    let n679: ZB = zb_or(n677, n678);
    let n680: ZB = zb_or(n669, n679);
    let n681: ZB = zb_and(n351, n680);
    let n682: ZB = zb_and(n350, n680);
    let n683: ZB = zb_or(n681, n682);
    let n684: ZB = zb_or(n661, n683);
    let n685: ZB = zb_and(n359, n684);
    let n686: ZB = zb_and(n360, n684);
    let n687: ZB = zb_or(n685, n686);
    let n695: ZB = zn_lt(n105, zn_splat(P8::from_raw(7340032i32)));
    let n696: ZB = zn_ge(n105, zn_splat(P8::from_raw(7340032i32)));
    let n697: ZB = zb_and(n76, n93);
    let n698: ZB = zb_and(n695, n697);
    let n699: ZB = zb_and(n696, n697);
    let n700: ZN = zsel_n(n695, zn_splat(P8::from_raw(196608i32)), r_c235);
    let n701: ZN = zsel_n(n695, zn_splat(P8::from_raw(65536i32)), r_c247);
    let n702: ZB = zb_or(n698, n699);
    let n703: ZB = zb_and(n124, n125);
    let n704: ZB = zb_not(n128);
    let n705: ZB = zb_and(n127, n704);
    let n706: ZB = zn_ge(n130, zn_splat(P8::from_raw(0i32)));
    let n707: ZB = zb_and(n129, n706);
    let n708: ZN = zsel_n(n128, n130, r_c235);
    let n709: ZN = zsel_n(n128, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(196608i32)));
    let n710: ZB = zb_or(n705, n707);
    let n711: ZN = zsel_n(n125, r_c235, n708);
    let n712: ZN = zsel_n(n125, zn_splat(P8::from_raw(196608i32)), n709);
    let n713: ZN = zsel_n(n125, zn_splat(P8::from_raw(-229376i32)), zn_splat(P8::from_raw(-262144i32)));
    let n714: ZB = zb_or(n703, n710);
    let n715: ZN = zsel_n(n93, n700, n711);
    let n716: ZN = zsel_n(n93, zn_splat(P8::from_raw(196608i32)), n712);
    let n717: ZN = zsel_n(n93, n701, r_c247);
    let n718: ZN = zsel_n(n93, zn_splat(P8::from_raw(-262144i32)), n713);
    let n719: ZB = zb_or(n702, n714);
    let n720: ZB = zb_and(n359, n363);
    let n721: ZB = zb_and(n359, n559);
    let n722: ZN = zsel_n(n720, r_c87, n539);
    let n723: ZN = zsel_n(n720, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n724: ZB = zb_not(n720);
    let n725: ZB = zb_or(n720, n721);
    let n726: ZB = zsel_b(n720, n315, n541);
    let n727: ZN = zsel_n(n719, r_c87, n722);
    let n728: ZN = zsel_n(n719, r_c39, n723);
    let n729: ZB = zb_not(n719);
    let n730: ZB = zb_and(n724, n729);
    let n731: ZN = zsel_n(n719, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n732: ZN = zsel_n(n719, n715, zn_splat(P8::from_raw(196608i32)));
    let n733: ZN = zsel_n(n719, n716, zn_splat(P8::from_raw(196608i32)));
    let n734: ZN = zsel_n(n719, n717, zn_splat(P8::from_raw(65536i32)));
    let n735: ZN = zsel_n(n719, n105, zn_splat(P8::from_raw(8126464i32)));
    let n736: ZN = zsel_n(n719, n718, zn_splat(P8::from_raw(-262144i32)));
    let n737: ZN = zsel_n(n719, zn_splat(P8::from_raw(6291456i32)), zn_splat(P8::from_raw(7340032i32)));
    let n738: ZB = zb_or(n719, n725);
    let n739: ZB = zb_or(n719, n726);
    let n740: ZN = zn_rem(n731, zn_splat(P8::from_raw(524288i32)));
    let n741: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n740);
    let n742: ZB = zn_eq(zn_splat(P8::from_raw(2031616i32)), n741);
    let n743: ZB = zb_and(n738, n742);
    let n745: ZB = zb_and(n359, n392);
    let n746: ZB = zb_and(n359, n580);
    let n747: ZN = zsel_n(n745, r_c87, n539);
    let n748: ZN = zsel_n(n745, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n749: ZB = zb_not(n745);
    let n750: ZB = zb_or(n745, n746);
    let n751: ZB = zsel_b(n745, n315, n541);
    let n752: ZN = zsel_n(n719, r_c87, n747);
    let n753: ZN = zsel_n(n719, r_c39, n748);
    let n754: ZB = zb_and(n729, n749);
    let n755: ZB = zb_or(n719, n750);
    let n756: ZB = zb_or(n719, n751);
    let n757: ZB = zb_and(n742, n755);
    let n759: ZB = zb_and(n359, n417);
    let n760: ZB = zb_and(n359, n600);
    let n761: ZN = zsel_n(n759, r_c87, n539);
    let n762: ZN = zsel_n(n759, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n763: ZB = zb_not(n759);
    let n764: ZB = zb_or(n759, n760);
    let n765: ZB = zsel_b(n759, n315, n541);
    let n766: ZN = zsel_n(n719, r_c87, n761);
    let n767: ZN = zsel_n(n719, r_c39, n762);
    let n768: ZB = zb_and(n729, n763);
    let n769: ZB = zb_or(n719, n764);
    let n770: ZB = zb_or(n719, n765);
    let n771: ZB = zb_and(n742, n769);
    let n773: ZB = zb_and(n359, n446);
    let n774: ZB = zb_and(n359, n629);
    let n775: ZN = zsel_n(n773, r_c87, n539);
    let n776: ZN = zsel_n(n773, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n777: ZB = zb_not(n773);
    let n778: ZB = zb_or(n773, n774);
    let n779: ZB = zsel_b(n773, n315, n541);
    let n780: ZN = zsel_n(n719, r_c87, n775);
    let n781: ZN = zsel_n(n719, r_c39, n776);
    let n782: ZB = zb_and(n729, n777);
    let n783: ZB = zb_or(n719, n778);
    let n784: ZB = zb_or(n719, n779);
    let n785: ZB = zb_and(n742, n783);
    let n788: ZB = zb_and(n359, n475);
    let n789: ZB = zb_and(n359, n658);
    let n790: ZN = zsel_n(n788, r_c87, n539);
    let n791: ZN = zsel_n(n788, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n792: ZB = zb_not(n788);
    let n793: ZB = zb_or(n788, n789);
    let n794: ZB = zsel_b(n788, n315, n541);
    let n795: ZN = zsel_n(n719, r_c87, n790);
    let n796: ZN = zsel_n(n719, r_c39, n791);
    let n797: ZB = zb_and(n729, n792);
    let n798: ZB = zb_or(n719, n793);
    let n799: ZB = zb_or(n719, n794);
    let n800: ZB = zb_and(n742, n798);
    let n803: ZB = zb_and(n359, n504);
    let n804: ZB = zb_and(n359, n687);
    let n805: ZN = zsel_n(n803, r_c87, n539);
    let n806: ZN = zsel_n(n803, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n807: ZB = zb_not(n803);
    let n808: ZB = zb_or(n803, n804);
    let n809: ZB = zsel_b(n803, n315, n541);
    let n810: ZN = zsel_n(n719, r_c87, n805);
    let n811: ZN = zsel_n(n719, r_c39, n806);
    let n812: ZB = zb_and(n729, n807);
    let n813: ZB = zb_or(n719, n808);
    let n814: ZB = zb_or(n719, n809);
    let n815: ZB = zb_and(n742, n813);
    let n818: ZN = zsel_n(n719, r_c20, zn_splat(P8::from_raw(131072i32)));
    let n819: ZB = zn_gt(n818, zn_splat(P8::from_raw(0i32)));
    let n820: ZB = zn_le(n818, zn_splat(P8::from_raw(0i32)));
    let n821: ZB = zb_and(n743, n819);
    let n822: ZB = zb_and(n743, n820);
    let n823: ZB = zb_or(n821, n822);
    let n824: ZB = zb_and(n757, n819);
    let n825: ZB = zb_and(n757, n820);
    let n826: ZB = zb_or(n824, n825);
    let n827: ZB = zb_and(n771, n819);
    let n828: ZB = zb_and(n771, n820);
    let n829: ZB = zb_or(n827, n828);
    let n830: ZB = zb_and(n785, n819);
    let n831: ZB = zb_and(n785, n820);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_and(n800, n819);
    let n834: ZB = zb_and(n800, n820);
    let n835: ZB = zb_or(n833, n834);
    let n836: ZB = zb_and(n815, n819);
    let n837: ZB = zb_and(n815, n820);
    let n838: ZB = zb_or(n836, n837);
    let n839: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n840: ZB = zb_not(n742);
    let n841: ZB = zb_and(n738, n840);
    let n842: ZB = zb_or(n743, n841);
    let n843: ZN = zsel_n(n365, r_c87, n727);
    let n844: ZN = zsel_n(n365, r_c39, n728);
    let n845: ZN = zsel_n(n365, n839, r_c20);
    let n846: ZB = zb_and(n76, n730);
    let n847: ZN = zsel_n(n365, zn_splat(P8::from_raw(0i32)), n731);
    let n848: ZN = zsel_n(n365, r_c235, n732);
    let n849: ZN = zsel_n(n365, zn_splat(P8::from_raw(196608i32)), n733);
    let n850: ZN = zsel_n(n365, r_c247, n734);
    let n851: ZN = zsel_n(n365, r_c251, n735);
    let n852: ZN = zsel_n(n365, zn_splat(P8::from_raw(-262144i32)), n736);
    let n853: ZN = zsel_n(n365, zn_splat(P8::from_raw(6291456i32)), n737);
    let n854: ZB = zb_or(n365, n842);
    let n855: ZB = zb_or(n365, n739);
    let n856: ZB = zn_gt(n845, zn_splat(P8::from_raw(0i32)));
    let n857: ZB = zn_le(n845, zn_splat(P8::from_raw(0i32)));
    let n858: ZB = zb_and(n854, n856);
    let n859: ZB = zb_and(n854, n857);
    let n860: ZB = zb_or(n858, n859);
    let n862: ZB = zb_and(n755, n840);
    let n863: ZB = zb_or(n757, n862);
    let n864: ZN = zsel_n(n365, r_c87, n752);
    let n865: ZN = zsel_n(n365, r_c39, n753);
    let n866: ZB = zb_and(n76, n754);
    let n867: ZB = zb_or(n365, n863);
    let n868: ZB = zb_or(n365, n756);
    let n869: ZB = zb_and(n856, n867);
    let n870: ZB = zb_and(n857, n867);
    let n871: ZB = zb_or(n869, n870);
    let n873: ZB = zb_and(n769, n840);
    let n874: ZB = zb_or(n771, n873);
    let n875: ZN = zsel_n(n365, r_c87, n766);
    let n876: ZN = zsel_n(n365, r_c39, n767);
    let n877: ZB = zb_and(n76, n768);
    let n878: ZB = zb_or(n365, n874);
    let n879: ZB = zb_or(n365, n770);
    let n880: ZB = zb_and(n856, n878);
    let n881: ZB = zb_and(n857, n878);
    let n882: ZB = zb_or(n880, n881);
    let n884: ZB = zb_and(n783, n840);
    let n885: ZN = zsel_n(n365, r_c87, n780);
    let n886: ZN = zsel_n(n365, r_c39, n781);
    let n887: ZB = zb_and(n76, n782);
    let n888: ZB = zb_or(n365, n884);
    let n889: ZB = zb_or(n365, n784);
    let n890: ZB = zb_and(n856, n888);
    let n891: ZB = zb_and(n857, n888);
    let n892: ZB = zb_or(n890, n891);
    let n894: ZB = zb_and(n798, n840);
    let n895: ZN = zsel_n(n365, r_c87, n795);
    let n896: ZN = zsel_n(n365, r_c39, n796);
    let n897: ZB = zb_and(n76, n797);
    let n898: ZB = zb_or(n365, n894);
    let n899: ZB = zb_or(n365, n799);
    let n900: ZB = zb_and(n856, n898);
    let n901: ZB = zb_and(n857, n898);
    let n902: ZB = zb_or(n900, n901);
    let n904: ZB = zb_and(n813, n840);
    let n905: ZN = zsel_n(n365, r_c87, n810);
    let n906: ZN = zsel_n(n365, r_c39, n811);
    let n907: ZB = zb_and(n76, n812);
    let n908: ZB = zb_or(n365, n904);
    let n909: ZB = zb_or(n365, n814);
    let n910: ZB = zb_and(n856, n908);
    let n911: ZB = zb_and(n857, n908);
    let n912: ZB = zb_or(n910, n911);
    let n914: ZN = zsel_n(n365, n839, n818);
    let n915: ZB = zb_or(n365, n841);
    let n916: ZB = zn_gt(n914, zn_splat(P8::from_raw(0i32)));
    let n917: ZB = zn_le(n914, zn_splat(P8::from_raw(0i32)));
    let n918: ZB = zb_and(n915, n916);
    let n919: ZB = zb_and(n915, n917);
    let n920: ZB = zb_or(n918, n919);
    let n921: ZB = zb_or(n365, n862);
    let n922: ZB = zb_and(n916, n921);
    let n923: ZB = zb_and(n917, n921);
    let n924: ZB = zb_or(n922, n923);
    let n925: ZB = zb_or(n365, n873);
    let n926: ZB = zb_and(n916, n925);
    let n927: ZB = zb_and(n917, n925);
    let n928: ZB = zb_or(n926, n927);
    let n929: ZB = zb_and(n888, n916);
    let n930: ZB = zb_and(n888, n917);
    let n931: ZB = zb_or(n929, n930);
    let n932: ZB = zb_and(n898, n916);
    let n933: ZB = zb_and(n898, n917);
    let n934: ZB = zb_or(n932, n933);
    let n935: ZB = zb_and(n908, n916);
    let n936: ZB = zb_and(n908, n917);
    let n937: ZB = zb_or(n935, n936);
    let n940: ZW = zw_bits_n(r_c39);
    let n941: ZW = zw_mix1(zw_splat(11400714819323198485u64), n940, 39u64);
    let n942: ZW = zw_mix2(zw_splat(11562461410679940143u64), n940, 39u64);
    let n943: ZW = zw_bits_n(n63);
    let n944: ZW = zw_mix1(n941, n943, 84u64);
    let n945: ZW = zw_mix2(n942, n943, 84u64);
    let n946: ZW = zw_bits_n(n104);
    let n947: ZW = zw_mix1(n944, n946, 85u64);
    let n948: ZW = zw_mix2(n945, n946, 85u64);
    let n949: ZW = zw_bits_n(n103);
    let n950: ZW = zw_mix1(n947, n949, 86u64);
    let n951: ZW = zw_mix2(n948, n949, 86u64);
    let n952: ZW = zw_bits_n(r_c87);
    let n953: ZW = zw_mix1(n950, n952, 87u64);
    let n954: ZW = zw_mix2(n951, n952, 87u64);
    let n955: ZW = zw_bits_n(n105);
    let n956: ZW = zw_mix1(n953, n955, 256u64);
    let n957: ZW = zw_mix2(n954, n955, 256u64);
    let n958: ZW = zw_bits_n(r_c20);
    let n959: ZW = zw_mix1(n956, n958, 20u64);
    let n960: ZW = zw_mix2(n957, n958, 20u64);
    let n961: u64 = false as u64;
    let n962: ZW = zw_mix1(n959, zw_splat(n961), 41u64);
    let n963: ZW = zw_mix2(n960, zw_splat(n961), 41u64);
    let n964: u64 = P8::from_raw(-65536i32).as_raw_u32() as u64;
    let n965: ZW = zw_mix1(n962, zw_splat(n964), 236u64);
    let n966: ZW = zw_mix2(n963, zw_splat(n964), 236u64);
    let n967: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n968: ZW = zw_mix1(n965, zw_splat(n967), 238u64);
    let n969: ZW = zw_mix2(n966, zw_splat(n967), 238u64);
    let n970: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n971: ZW = zw_mix1(n968, zw_splat(n970), 239u64);
    let n972: ZW = zw_mix2(n969, zw_splat(n970), 239u64);
    let n973: ZW = zw_bits_n(n329);
    let n974: ZW = zw_mix1(n971, n973, 241u64);
    let n975: ZW = zw_mix2(n972, n973, 241u64);
    let n976: ZW = zw_mix1(n974, zw_splat(n961), 248u64);
    let n977: ZW = zw_mix2(n975, zw_splat(n961), 248u64);
    let n978: ZW = zw_mix1(n976, zw_splat(n961), 249u64);
    let n979: ZW = zw_mix2(n977, zw_splat(n961), 249u64);
    let n980: ZW = zw_mix1(n978, zw_splat(n967), 270u64);
    let n981: ZW = zw_mix2(n979, zw_splat(n967), 270u64);
    let n982: ZW = zw_mix1(n980, zw_splat(n967), 271u64);
    let n983: ZW = zw_mix2(n981, zw_splat(n967), 271u64);
    let n984: ZW = zw_mix1(n982, zw_splat(n967), 272u64);
    let n985: ZW = zw_mix2(n983, zw_splat(n967), 272u64);
    let n986: ZW = zw_mix1(n984, zw_splat(n967), 273u64);
    let n987: ZW = zw_mix2(n985, zw_splat(n967), 273u64);
    let n988: ZW = zw_mix1(n986, zw_splat(n961), 274u64);
    let n989: ZW = zw_mix2(n987, zw_splat(n961), 274u64);
    let n990: ZW = zw_mix1(n988, zw_splat(n967), 282u64);
    let n991: ZW = zw_mix2(n989, zw_splat(n967), 282u64);
    let n992: ZW = zw_bits_n(n340);
    let n993: ZW = zw_mix1(n990, n992, 283u64);
    let n994: ZW = zw_mix2(n991, n992, 283u64);
    let n995: u64 = true as u64;
    let n996: ZW = zw_mix1(n986, zw_splat(n995), 274u64);
    let n997: ZW = zw_mix2(n987, zw_splat(n995), 274u64);
    let n998: ZW = zw_bits_n(n370);
    let n999: ZW = zw_mix1(n996, n998, 282u64);
    let n1000: ZW = zw_mix2(n997, n998, 282u64);
    let n1001: ZW = zw_mix1(n999, n992, 283u64);
    let n1002: ZW = zw_mix2(n1000, n992, 283u64);
    let n1003: ZW = zw_bits_n(n395);
    let n1004: ZW = zw_mix1(n988, n1003, 282u64);
    let n1005: ZW = zw_mix2(n989, n1003, 282u64);
    let n1006: ZW = zw_mix1(n1004, n992, 283u64);
    let n1007: ZW = zw_mix2(n1005, n992, 283u64);
    let n1008: ZW = zw_bits_n(n356);
    let n1009: ZW = zw_mix1(n971, n1008, 241u64);
    let n1010: ZW = zw_mix2(n972, n1008, 241u64);
    let n1011: ZW = zw_mix1(n1009, zw_splat(n961), 248u64);
    let n1012: ZW = zw_mix2(n1010, zw_splat(n961), 248u64);
    let n1013: ZW = zw_mix1(n1011, zw_splat(n995), 249u64);
    let n1014: ZW = zw_mix2(n1012, zw_splat(n995), 249u64);
    let n1015: ZW = zw_mix1(n1013, zw_splat(n967), 270u64);
    let n1016: ZW = zw_mix2(n1014, zw_splat(n967), 270u64);
    let n1017: ZW = zw_mix1(n1015, zw_splat(n967), 271u64);
    let n1018: ZW = zw_mix2(n1016, zw_splat(n967), 271u64);
    let n1019: ZW = zw_mix1(n1017, zw_splat(n967), 272u64);
    let n1020: ZW = zw_mix2(n1018, zw_splat(n967), 272u64);
    let n1021: ZW = zw_mix1(n1019, zw_splat(n967), 273u64);
    let n1022: ZW = zw_mix2(n1020, zw_splat(n967), 273u64);
    let n1023: ZW = zw_mix1(n1021, zw_splat(n961), 274u64);
    let n1024: ZW = zw_mix2(n1022, zw_splat(n961), 274u64);
    let n1025: ZW = zw_bits_n(n357);
    let n1026: ZW = zw_mix1(n1023, n1025, 282u64);
    let n1027: ZW = zw_mix2(n1024, n1025, 282u64);
    let n1028: ZW = zw_bits_n(n358);
    let n1029: ZW = zw_mix1(n1026, n1028, 283u64);
    let n1030: ZW = zw_mix2(n1027, n1028, 283u64);
    let n1031: ZW = zw_mix1(n1021, zw_splat(n995), 274u64);
    let n1032: ZW = zw_mix2(n1022, zw_splat(n995), 274u64);
    let n1033: ZW = zw_bits_n(n389);
    let n1034: ZW = zw_mix1(n1031, n1033, 282u64);
    let n1035: ZW = zw_mix2(n1032, n1033, 282u64);
    let n1036: ZW = zw_mix1(n1034, n1028, 283u64);
    let n1037: ZW = zw_mix2(n1035, n1028, 283u64);
    let n1038: ZW = zw_bits_n(n414);
    let n1039: ZW = zw_mix1(n1023, n1038, 282u64);
    let n1040: ZW = zw_mix2(n1024, n1038, 282u64);
    let n1041: ZW = zw_mix1(n1039, n1028, 283u64);
    let n1042: ZW = zw_mix2(n1040, n1028, 283u64);
    let n1043: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n1044: ZW = zw_mix1(n956, zw_splat(n1043), 20u64);
    let n1045: ZW = zw_mix2(n957, zw_splat(n1043), 20u64);
    let n1046: ZW = zw_mix1(n1044, zw_splat(n995), 41u64);
    let n1047: ZW = zw_mix2(n1045, zw_splat(n995), 41u64);
    let n1048: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n1049: ZW = zw_mix1(n1046, zw_splat(n1048), 236u64);
    let n1050: ZW = zw_mix2(n1047, zw_splat(n1048), 236u64);
    let n1051: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n1052: ZW = zw_mix1(n1049, zw_splat(n1051), 238u64);
    let n1053: ZW = zw_mix2(n1050, zw_splat(n1051), 238u64);
    let n1054: ZW = zw_mix1(n1052, zw_splat(n967), 239u64);
    let n1055: ZW = zw_mix2(n1053, zw_splat(n967), 239u64);
    let n1056: ZW = zw_mix1(n1054, n973, 241u64);
    let n1057: ZW = zw_mix2(n1055, n973, 241u64);
    let n1058: ZW = zw_mix1(n1056, zw_splat(n995), 248u64);
    let n1059: ZW = zw_mix2(n1057, zw_splat(n995), 248u64);
    let n1060: ZW = zw_mix1(n1058, zw_splat(n961), 249u64);
    let n1061: ZW = zw_mix2(n1059, zw_splat(n961), 249u64);
    let n1062: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n1063: ZW = zw_mix1(n1060, zw_splat(n1062), 270u64);
    let n1064: ZW = zw_mix2(n1061, zw_splat(n1062), 270u64);
    let n1065: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n1066: ZW = zw_mix1(n1063, zw_splat(n1065), 271u64);
    let n1067: ZW = zw_mix2(n1064, zw_splat(n1065), 271u64);
    let n1068: ZW = zw_mix1(n1066, zw_splat(n1043), 272u64);
    let n1069: ZW = zw_mix2(n1067, zw_splat(n1043), 272u64);
    let n1070: ZW = zw_mix1(n1068, zw_splat(n967), 273u64);
    let n1071: ZW = zw_mix2(n1069, zw_splat(n967), 273u64);
    let n1072: ZW = zw_mix1(n1070, zw_splat(n961), 274u64);
    let n1073: ZW = zw_mix2(n1071, zw_splat(n961), 274u64);
    let n1074: ZW = zw_mix1(n1072, zw_splat(n970), 282u64);
    let n1075: ZW = zw_mix2(n1073, zw_splat(n970), 282u64);
    let n1076: ZW = zw_mix1(n1074, zw_splat(n967), 283u64);
    let n1077: ZW = zw_mix2(n1075, zw_splat(n967), 283u64);
    let n1078: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n1079: ZW = zw_mix1(n1066, zw_splat(n1078), 272u64);
    let n1080: ZW = zw_mix2(n1067, zw_splat(n1078), 272u64);
    let n1081: ZW = zw_mix1(n1079, zw_splat(n967), 273u64);
    let n1082: ZW = zw_mix2(n1080, zw_splat(n967), 273u64);
    let n1083: ZW = zw_mix1(n1081, zw_splat(n995), 274u64);
    let n1084: ZW = zw_mix2(n1082, zw_splat(n995), 274u64);
    let n1085: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n1086: ZW = zw_mix1(n1083, zw_splat(n1085), 282u64);
    let n1087: ZW = zw_mix2(n1084, zw_splat(n1085), 282u64);
    let n1088: ZW = zw_mix1(n1086, zw_splat(n967), 283u64);
    let n1089: ZW = zw_mix2(n1087, zw_splat(n967), 283u64);
    let n1090: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n1091: ZW = zw_mix1(n1072, zw_splat(n1090), 282u64);
    let n1092: ZW = zw_mix2(n1073, zw_splat(n1090), 282u64);
    let n1093: ZW = zw_mix1(n1091, zw_splat(n967), 283u64);
    let n1094: ZW = zw_mix2(n1092, zw_splat(n967), 283u64);
    let n1095: ZW = zw_mix1(n1060, zw_splat(n1065), 270u64);
    let n1096: ZW = zw_mix2(n1061, zw_splat(n1065), 270u64);
    let n1097: ZW = zw_mix1(n1095, zw_splat(n1062), 271u64);
    let n1098: ZW = zw_mix2(n1096, zw_splat(n1062), 271u64);
    let n1099: ZW = zw_mix1(n1097, zw_splat(n967), 272u64);
    let n1100: ZW = zw_mix2(n1098, zw_splat(n967), 272u64);
    let n1101: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n1102: ZW = zw_mix1(n1099, zw_splat(n1101), 273u64);
    let n1103: ZW = zw_mix2(n1100, zw_splat(n1101), 273u64);
    let n1104: ZW = zw_mix1(n1102, zw_splat(n961), 274u64);
    let n1105: ZW = zw_mix2(n1103, zw_splat(n961), 274u64);
    let n1106: ZW = zw_mix1(n1104, zw_splat(n967), 282u64);
    let n1107: ZW = zw_mix2(n1105, zw_splat(n967), 282u64);
    let n1108: ZW = zw_mix1(n1106, zw_splat(n1085), 283u64);
    let n1109: ZW = zw_mix2(n1107, zw_splat(n1085), 283u64);
    let n1110: ZW = zw_mix1(n1095, zw_splat(n1065), 271u64);
    let n1111: ZW = zw_mix2(n1096, zw_splat(n1065), 271u64);
    let n1112: ZW = zw_mix1(n1110, zw_splat(n1078), 272u64);
    let n1113: ZW = zw_mix2(n1111, zw_splat(n1078), 272u64);
    let n1114: ZW = zw_mix1(n1112, zw_splat(n1101), 273u64);
    let n1115: ZW = zw_mix2(n1113, zw_splat(n1101), 273u64);
    let n1116: ZW = zw_mix1(n1114, zw_splat(n995), 274u64);
    let n1117: ZW = zw_mix2(n1115, zw_splat(n995), 274u64);
    let n1118: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1119: ZW = zw_mix1(n1116, zw_splat(n1118), 282u64);
    let n1120: ZW = zw_mix2(n1117, zw_splat(n1118), 282u64);
    let n1121: ZW = zw_mix1(n1119, zw_splat(n1118), 283u64);
    let n1122: ZW = zw_mix2(n1120, zw_splat(n1118), 283u64);
    let n1123: ZW = zw_mix1(n1110, zw_splat(n1043), 272u64);
    let n1124: ZW = zw_mix2(n1111, zw_splat(n1043), 272u64);
    let n1125: ZW = zw_mix1(n1123, zw_splat(n1101), 273u64);
    let n1126: ZW = zw_mix2(n1124, zw_splat(n1101), 273u64);
    let n1127: ZW = zw_mix1(n1125, zw_splat(n961), 274u64);
    let n1128: ZW = zw_mix2(n1126, zw_splat(n961), 274u64);
    let n1129: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1130: ZW = zw_mix1(n1127, zw_splat(n1129), 282u64);
    let n1131: ZW = zw_mix2(n1128, zw_splat(n1129), 282u64);
    let n1132: ZW = zw_mix1(n1130, zw_splat(n1118), 283u64);
    let n1133: ZW = zw_mix2(n1131, zw_splat(n1118), 283u64);
    let n1134: ZW = zw_mix1(n1099, zw_splat(n1043), 273u64);
    let n1135: ZW = zw_mix2(n1100, zw_splat(n1043), 273u64);
    let n1136: ZW = zw_mix1(n1134, zw_splat(n961), 274u64);
    let n1137: ZW = zw_mix2(n1135, zw_splat(n961), 274u64);
    let n1138: ZW = zw_mix1(n1136, zw_splat(n967), 282u64);
    let n1139: ZW = zw_mix2(n1137, zw_splat(n967), 282u64);
    let n1140: ZW = zw_mix1(n1138, zw_splat(n1090), 283u64);
    let n1141: ZW = zw_mix2(n1139, zw_splat(n1090), 283u64);
    let n1142: ZW = zw_mix1(n1112, zw_splat(n1043), 273u64);
    let n1143: ZW = zw_mix2(n1113, zw_splat(n1043), 273u64);
    let n1144: ZW = zw_mix1(n1142, zw_splat(n995), 274u64);
    let n1145: ZW = zw_mix2(n1143, zw_splat(n995), 274u64);
    let n1146: ZW = zw_mix1(n1144, zw_splat(n1118), 282u64);
    let n1147: ZW = zw_mix2(n1145, zw_splat(n1118), 282u64);
    let n1148: ZW = zw_mix1(n1146, zw_splat(n1129), 283u64);
    let n1149: ZW = zw_mix2(n1147, zw_splat(n1129), 283u64);
    let n1150: ZW = zw_mix1(n1123, zw_splat(n1043), 273u64);
    let n1151: ZW = zw_mix2(n1124, zw_splat(n1043), 273u64);
    let n1152: ZW = zw_mix1(n1150, zw_splat(n961), 274u64);
    let n1153: ZW = zw_mix2(n1151, zw_splat(n961), 274u64);
    let n1154: ZW = zw_mix1(n1152, zw_splat(n1129), 282u64);
    let n1155: ZW = zw_mix2(n1153, zw_splat(n1129), 282u64);
    let n1156: ZW = zw_mix1(n1154, zw_splat(n1129), 283u64);
    let n1157: ZW = zw_mix2(n1155, zw_splat(n1129), 283u64);
    let n1158: ZW = zw_mix1(n1054, n1008, 241u64);
    let n1159: ZW = zw_mix2(n1055, n1008, 241u64);
    let n1160: ZW = zw_mix1(n1158, zw_splat(n995), 248u64);
    let n1161: ZW = zw_mix2(n1159, zw_splat(n995), 248u64);
    let n1162: ZW = zw_mix1(n1160, zw_splat(n995), 249u64);
    let n1163: ZW = zw_mix2(n1161, zw_splat(n995), 249u64);
    let n1164: ZW = zw_mix1(n1162, zw_splat(n1062), 270u64);
    let n1165: ZW = zw_mix2(n1163, zw_splat(n1062), 270u64);
    let n1166: ZW = zw_mix1(n1164, zw_splat(n1065), 271u64);
    let n1167: ZW = zw_mix2(n1165, zw_splat(n1065), 271u64);
    let n1168: ZW = zw_mix1(n1166, zw_splat(n1043), 272u64);
    let n1169: ZW = zw_mix2(n1167, zw_splat(n1043), 272u64);
    let n1170: ZW = zw_mix1(n1168, zw_splat(n967), 273u64);
    let n1171: ZW = zw_mix2(n1169, zw_splat(n967), 273u64);
    let n1172: ZW = zw_mix1(n1170, zw_splat(n961), 274u64);
    let n1173: ZW = zw_mix2(n1171, zw_splat(n961), 274u64);
    let n1174: ZW = zw_mix1(n1172, zw_splat(n970), 282u64);
    let n1175: ZW = zw_mix2(n1173, zw_splat(n970), 282u64);
    let n1176: ZW = zw_mix1(n1174, zw_splat(n967), 283u64);
    let n1177: ZW = zw_mix2(n1175, zw_splat(n967), 283u64);
    let n1178: ZW = zw_mix1(n1166, zw_splat(n1078), 272u64);
    let n1179: ZW = zw_mix2(n1167, zw_splat(n1078), 272u64);
    let n1180: ZW = zw_mix1(n1178, zw_splat(n967), 273u64);
    let n1181: ZW = zw_mix2(n1179, zw_splat(n967), 273u64);
    let n1182: ZW = zw_mix1(n1180, zw_splat(n995), 274u64);
    let n1183: ZW = zw_mix2(n1181, zw_splat(n995), 274u64);
    let n1184: ZW = zw_mix1(n1182, zw_splat(n1085), 282u64);
    let n1185: ZW = zw_mix2(n1183, zw_splat(n1085), 282u64);
    let n1186: ZW = zw_mix1(n1184, zw_splat(n967), 283u64);
    let n1187: ZW = zw_mix2(n1185, zw_splat(n967), 283u64);
    let n1188: ZW = zw_mix1(n1172, zw_splat(n1090), 282u64);
    let n1189: ZW = zw_mix2(n1173, zw_splat(n1090), 282u64);
    let n1190: ZW = zw_mix1(n1188, zw_splat(n967), 283u64);
    let n1191: ZW = zw_mix2(n1189, zw_splat(n967), 283u64);
    let n1192: ZW = zw_mix1(n1162, zw_splat(n1065), 270u64);
    let n1193: ZW = zw_mix2(n1163, zw_splat(n1065), 270u64);
    let n1194: ZW = zw_mix1(n1192, zw_splat(n1062), 271u64);
    let n1195: ZW = zw_mix2(n1193, zw_splat(n1062), 271u64);
    let n1196: ZW = zw_mix1(n1194, zw_splat(n967), 272u64);
    let n1197: ZW = zw_mix2(n1195, zw_splat(n967), 272u64);
    let n1198: ZW = zw_mix1(n1196, zw_splat(n1101), 273u64);
    let n1199: ZW = zw_mix2(n1197, zw_splat(n1101), 273u64);
    let n1200: ZW = zw_mix1(n1198, zw_splat(n961), 274u64);
    let n1201: ZW = zw_mix2(n1199, zw_splat(n961), 274u64);
    let n1202: ZW = zw_mix1(n1200, zw_splat(n967), 282u64);
    let n1203: ZW = zw_mix2(n1201, zw_splat(n967), 282u64);
    let n1204: ZW = zw_mix1(n1202, zw_splat(n1085), 283u64);
    let n1205: ZW = zw_mix2(n1203, zw_splat(n1085), 283u64);
    let n1206: ZW = zw_mix1(n1192, zw_splat(n1065), 271u64);
    let n1207: ZW = zw_mix2(n1193, zw_splat(n1065), 271u64);
    let n1208: ZW = zw_mix1(n1206, zw_splat(n1078), 272u64);
    let n1209: ZW = zw_mix2(n1207, zw_splat(n1078), 272u64);
    let n1210: ZW = zw_mix1(n1208, zw_splat(n1101), 273u64);
    let n1211: ZW = zw_mix2(n1209, zw_splat(n1101), 273u64);
    let n1212: ZW = zw_mix1(n1210, zw_splat(n995), 274u64);
    let n1213: ZW = zw_mix2(n1211, zw_splat(n995), 274u64);
    let n1214: ZW = zw_mix1(n1212, zw_splat(n1118), 282u64);
    let n1215: ZW = zw_mix2(n1213, zw_splat(n1118), 282u64);
    let n1216: ZW = zw_mix1(n1214, zw_splat(n1118), 283u64);
    let n1217: ZW = zw_mix2(n1215, zw_splat(n1118), 283u64);
    let n1218: ZW = zw_mix1(n1206, zw_splat(n1043), 272u64);
    let n1219: ZW = zw_mix2(n1207, zw_splat(n1043), 272u64);
    let n1220: ZW = zw_mix1(n1218, zw_splat(n1101), 273u64);
    let n1221: ZW = zw_mix2(n1219, zw_splat(n1101), 273u64);
    let n1222: ZW = zw_mix1(n1220, zw_splat(n961), 274u64);
    let n1223: ZW = zw_mix2(n1221, zw_splat(n961), 274u64);
    let n1224: ZW = zw_mix1(n1222, zw_splat(n1129), 282u64);
    let n1225: ZW = zw_mix2(n1223, zw_splat(n1129), 282u64);
    let n1226: ZW = zw_mix1(n1224, zw_splat(n1118), 283u64);
    let n1227: ZW = zw_mix2(n1225, zw_splat(n1118), 283u64);
    let n1228: ZW = zw_mix1(n1196, zw_splat(n1043), 273u64);
    let n1229: ZW = zw_mix2(n1197, zw_splat(n1043), 273u64);
    let n1230: ZW = zw_mix1(n1228, zw_splat(n961), 274u64);
    let n1231: ZW = zw_mix2(n1229, zw_splat(n961), 274u64);
    let n1232: ZW = zw_mix1(n1230, zw_splat(n967), 282u64);
    let n1233: ZW = zw_mix2(n1231, zw_splat(n967), 282u64);
    let n1234: ZW = zw_mix1(n1232, zw_splat(n1090), 283u64);
    let n1235: ZW = zw_mix2(n1233, zw_splat(n1090), 283u64);
    let n1236: ZW = zw_mix1(n1208, zw_splat(n1043), 273u64);
    let n1237: ZW = zw_mix2(n1209, zw_splat(n1043), 273u64);
    let n1238: ZW = zw_mix1(n1236, zw_splat(n995), 274u64);
    let n1239: ZW = zw_mix2(n1237, zw_splat(n995), 274u64);
    let n1240: ZW = zw_mix1(n1238, zw_splat(n1118), 282u64);
    let n1241: ZW = zw_mix2(n1239, zw_splat(n1118), 282u64);
    let n1242: ZW = zw_mix1(n1240, zw_splat(n1129), 283u64);
    let n1243: ZW = zw_mix2(n1241, zw_splat(n1129), 283u64);
    let n1244: ZW = zw_mix1(n1218, zw_splat(n1043), 273u64);
    let n1245: ZW = zw_mix2(n1219, zw_splat(n1043), 273u64);
    let n1246: ZW = zw_mix1(n1244, zw_splat(n961), 274u64);
    let n1247: ZW = zw_mix2(n1245, zw_splat(n961), 274u64);
    let n1248: ZW = zw_mix1(n1246, zw_splat(n1129), 282u64);
    let n1249: ZW = zw_mix2(n1247, zw_splat(n1129), 282u64);
    let n1250: ZW = zw_mix1(n1248, zw_splat(n1129), 283u64);
    let n1251: ZW = zw_mix2(n1249, zw_splat(n1129), 283u64);
    let n1252: ZW = zw_mix1(zw_splat(11400714819323198485u64), n943, 84u64);
    let n1253: ZW = zw_mix2(zw_splat(11562461410679940143u64), n943, 84u64);
    let n1254: ZW = zw_mix1(n1252, n946, 85u64);
    let n1255: ZW = zw_mix2(n1253, n946, 85u64);
    let n1256: ZW = zw_mix1(n1254, n949, 86u64);
    let n1257: ZW = zw_mix2(n1255, n949, 86u64);
    let n1258: ZW = zw_bits_n(n539);
    let n1259: ZW = zw_mix1(n1256, n1258, 87u64);
    let n1260: ZW = zw_mix2(n1257, n1258, 87u64);
    let n1261: ZW = zw_mix1(n1259, n958, 20u64);
    let n1262: ZW = zw_mix2(n1260, n958, 20u64);
    let n1263: ZW = zw_mix1(n1261, zw_splat(n961), 41u64);
    let n1264: ZW = zw_mix2(n1262, zw_splat(n961), 41u64);
    let n1265: ZW = zw_mix1(n1259, zw_splat(n1043), 20u64);
    let n1266: ZW = zw_mix2(n1260, zw_splat(n1043), 20u64);
    let n1267: ZW = zw_mix1(n1265, zw_splat(n995), 41u64);
    let n1268: ZW = zw_mix2(n1266, zw_splat(n995), 41u64);
    let n1269: ZW = zw_bits_n(n731);
    let n1270: ZW = zw_mix1(n1256, n1269, 161u64);
    let n1271: ZW = zw_mix2(n1257, n1269, 161u64);
    let n1272: ZW = zw_bits_n(n732);
    let n1273: ZW = zw_mix1(n1270, n1272, 237u64);
    let n1274: ZW = zw_mix2(n1271, n1272, 237u64);
    let n1275: ZW = zw_bits_n(n733);
    let n1276: ZW = zw_mix1(n1273, n1275, 248u64);
    let n1277: ZW = zw_mix2(n1274, n1275, 248u64);
    let n1278: ZW = zw_bits_n(n734);
    let n1279: ZW = zw_mix1(n1276, n1278, 249u64);
    let n1280: ZW = zw_mix2(n1277, n1278, 249u64);
    let n1281: ZW = zw_bits_n(n735);
    let n1282: ZW = zw_mix1(n1279, n1281, 253u64);
    let n1283: ZW = zw_mix2(n1280, n1281, 253u64);
    let n1284: ZW = zw_bits_n(n736);
    let n1285: ZW = zw_mix1(n1282, n1284, 275u64);
    let n1286: ZW = zw_mix2(n1283, n1284, 275u64);
    let n1287: ZW = zw_bits_n(n737);
    let n1288: ZW = zw_mix1(n1285, n1287, 277u64);
    let n1289: ZW = zw_mix2(n1286, n1287, 277u64);
    let n1290: ZW = zw_mix1(n1288, n958, 20u64);
    let n1291: ZW = zw_mix2(n1289, n958, 20u64);
    let n1292: ZW = zw_bits_b(n730);
    let n1293: ZW = zw_mix1(n1290, n1292, 38u64);
    let n1294: ZW = zw_mix2(n1291, n1292, 38u64);
    let n1295: ZW = zw_bits_n(n728);
    let n1296: ZW = zw_mix1(n1293, n1295, 39u64);
    let n1297: ZW = zw_mix2(n1294, n1295, 39u64);
    let n1298: ZW = zw_bits_n(n727);
    let n1299: ZW = zw_mix1(n1296, n1298, 87u64);
    let n1300: ZW = zw_mix2(n1297, n1298, 87u64);
    let n1301: ZW = zw_bits_b(n754);
    let n1302: ZW = zw_mix1(n1290, n1301, 38u64);
    let n1303: ZW = zw_mix2(n1291, n1301, 38u64);
    let n1304: ZW = zw_bits_n(n753);
    let n1305: ZW = zw_mix1(n1302, n1304, 39u64);
    let n1306: ZW = zw_mix2(n1303, n1304, 39u64);
    let n1307: ZW = zw_bits_n(n752);
    let n1308: ZW = zw_mix1(n1305, n1307, 87u64);
    let n1309: ZW = zw_mix2(n1306, n1307, 87u64);
    let n1310: ZW = zw_bits_b(n768);
    let n1311: ZW = zw_mix1(n1290, n1310, 38u64);
    let n1312: ZW = zw_mix2(n1291, n1310, 38u64);
    let n1313: ZW = zw_bits_n(n767);
    let n1314: ZW = zw_mix1(n1311, n1313, 39u64);
    let n1315: ZW = zw_mix2(n1312, n1313, 39u64);
    let n1316: ZW = zw_bits_n(n766);
    let n1317: ZW = zw_mix1(n1314, n1316, 87u64);
    let n1318: ZW = zw_mix2(n1315, n1316, 87u64);
    let n1319: ZW = zw_bits_b(n782);
    let n1320: ZW = zw_mix1(n1290, n1319, 38u64);
    let n1321: ZW = zw_mix2(n1291, n1319, 38u64);
    let n1322: ZW = zw_bits_n(n781);
    let n1323: ZW = zw_mix1(n1320, n1322, 39u64);
    let n1324: ZW = zw_mix2(n1321, n1322, 39u64);
    let n1325: ZW = zw_bits_n(n780);
    let n1326: ZW = zw_mix1(n1323, n1325, 87u64);
    let n1327: ZW = zw_mix2(n1324, n1325, 87u64);
    let n1328: ZW = zw_bits_b(n797);
    let n1329: ZW = zw_mix1(n1290, n1328, 38u64);
    let n1330: ZW = zw_mix2(n1291, n1328, 38u64);
    let n1331: ZW = zw_bits_n(n796);
    let n1332: ZW = zw_mix1(n1329, n1331, 39u64);
    let n1333: ZW = zw_mix2(n1330, n1331, 39u64);
    let n1334: ZW = zw_bits_n(n795);
    let n1335: ZW = zw_mix1(n1332, n1334, 87u64);
    let n1336: ZW = zw_mix2(n1333, n1334, 87u64);
    let n1337: ZW = zw_bits_b(n812);
    let n1338: ZW = zw_mix1(n1290, n1337, 38u64);
    let n1339: ZW = zw_mix2(n1291, n1337, 38u64);
    let n1340: ZW = zw_bits_n(n811);
    let n1341: ZW = zw_mix1(n1338, n1340, 39u64);
    let n1342: ZW = zw_mix2(n1339, n1340, 39u64);
    let n1343: ZW = zw_bits_n(n810);
    let n1344: ZW = zw_mix1(n1341, n1343, 87u64);
    let n1345: ZW = zw_mix2(n1342, n1343, 87u64);
    let n1346: ZW = zw_bits_n(n818);
    let n1347: ZW = zw_mix1(n1288, n1346, 20u64);
    let n1348: ZW = zw_mix2(n1289, n1346, 20u64);
    let n1349: ZW = zw_mix1(n1347, n1292, 38u64);
    let n1350: ZW = zw_mix2(n1348, n1292, 38u64);
    let n1351: ZW = zw_mix1(n1349, n1295, 39u64);
    let n1352: ZW = zw_mix2(n1350, n1295, 39u64);
    let n1353: ZW = zw_mix1(n1351, n1298, 87u64);
    let n1354: ZW = zw_mix2(n1352, n1298, 87u64);
    let n1355: ZW = zw_mix1(n1347, n1301, 38u64);
    let n1356: ZW = zw_mix2(n1348, n1301, 38u64);
    let n1357: ZW = zw_mix1(n1355, n1304, 39u64);
    let n1358: ZW = zw_mix2(n1356, n1304, 39u64);
    let n1359: ZW = zw_mix1(n1357, n1307, 87u64);
    let n1360: ZW = zw_mix2(n1358, n1307, 87u64);
    let n1361: ZW = zw_mix1(n1347, n1310, 38u64);
    let n1362: ZW = zw_mix2(n1348, n1310, 38u64);
    let n1363: ZW = zw_mix1(n1361, n1313, 39u64);
    let n1364: ZW = zw_mix2(n1362, n1313, 39u64);
    let n1365: ZW = zw_mix1(n1363, n1316, 87u64);
    let n1366: ZW = zw_mix2(n1364, n1316, 87u64);
    let n1367: ZW = zw_mix1(n1347, n1319, 38u64);
    let n1368: ZW = zw_mix2(n1348, n1319, 38u64);
    let n1369: ZW = zw_mix1(n1367, n1322, 39u64);
    let n1370: ZW = zw_mix2(n1368, n1322, 39u64);
    let n1371: ZW = zw_mix1(n1369, n1325, 87u64);
    let n1372: ZW = zw_mix2(n1370, n1325, 87u64);
    let n1373: ZW = zw_mix1(n1347, n1328, 38u64);
    let n1374: ZW = zw_mix2(n1348, n1328, 38u64);
    let n1375: ZW = zw_mix1(n1373, n1331, 39u64);
    let n1376: ZW = zw_mix2(n1374, n1331, 39u64);
    let n1377: ZW = zw_mix1(n1375, n1334, 87u64);
    let n1378: ZW = zw_mix2(n1376, n1334, 87u64);
    let n1379: ZW = zw_mix1(n1347, n1337, 38u64);
    let n1380: ZW = zw_mix2(n1348, n1337, 38u64);
    let n1381: ZW = zw_mix1(n1379, n1340, 39u64);
    let n1382: ZW = zw_mix2(n1380, n1340, 39u64);
    let n1383: ZW = zw_mix1(n1381, n1343, 87u64);
    let n1384: ZW = zw_mix2(n1382, n1343, 87u64);
    let n1385: ZW = zw_bits_n(n847);
    let n1386: ZW = zw_mix1(n1256, n1385, 159u64);
    let n1387: ZW = zw_mix2(n1257, n1385, 159u64);
    let n1388: ZW = zw_bits_n(n848);
    let n1389: ZW = zw_mix1(n1386, n1388, 235u64);
    let n1390: ZW = zw_mix2(n1387, n1388, 235u64);
    let n1391: ZW = zw_bits_n(n849);
    let n1392: ZW = zw_mix1(n1389, n1391, 246u64);
    let n1393: ZW = zw_mix2(n1390, n1391, 246u64);
    let n1394: ZW = zw_bits_n(n850);
    let n1395: ZW = zw_mix1(n1392, n1394, 247u64);
    let n1396: ZW = zw_mix2(n1393, n1394, 247u64);
    let n1397: ZW = zw_bits_n(n851);
    let n1398: ZW = zw_mix1(n1395, n1397, 251u64);
    let n1399: ZW = zw_mix2(n1396, n1397, 251u64);
    let n1400: ZW = zw_bits_n(n852);
    let n1401: ZW = zw_mix1(n1398, n1400, 273u64);
    let n1402: ZW = zw_mix2(n1399, n1400, 273u64);
    let n1403: ZW = zw_bits_n(n853);
    let n1404: ZW = zw_mix1(n1401, n1403, 275u64);
    let n1405: ZW = zw_mix2(n1402, n1403, 275u64);
    let n1406: ZW = zw_bits_n(n845);
    let n1407: ZW = zw_mix1(n1404, n1406, 20u64);
    let n1408: ZW = zw_mix2(n1405, n1406, 20u64);
    let n1409: ZW = zw_bits_b(n846);
    let n1410: ZW = zw_mix1(n1407, n1409, 38u64);
    let n1411: ZW = zw_mix2(n1408, n1409, 38u64);
    let n1412: ZW = zw_bits_n(n844);
    let n1413: ZW = zw_mix1(n1410, n1412, 39u64);
    let n1414: ZW = zw_mix2(n1411, n1412, 39u64);
    let n1415: ZW = zw_bits_n(n843);
    let n1416: ZW = zw_mix1(n1413, n1415, 87u64);
    let n1417: ZW = zw_mix2(n1414, n1415, 87u64);
    let n1418: ZW = zw_bits_b(n866);
    let n1419: ZW = zw_mix1(n1407, n1418, 38u64);
    let n1420: ZW = zw_mix2(n1408, n1418, 38u64);
    let n1421: ZW = zw_bits_n(n865);
    let n1422: ZW = zw_mix1(n1419, n1421, 39u64);
    let n1423: ZW = zw_mix2(n1420, n1421, 39u64);
    let n1424: ZW = zw_bits_n(n864);
    let n1425: ZW = zw_mix1(n1422, n1424, 87u64);
    let n1426: ZW = zw_mix2(n1423, n1424, 87u64);
    let n1427: ZW = zw_bits_b(n877);
    let n1428: ZW = zw_mix1(n1407, n1427, 38u64);
    let n1429: ZW = zw_mix2(n1408, n1427, 38u64);
    let n1430: ZW = zw_bits_n(n876);
    let n1431: ZW = zw_mix1(n1428, n1430, 39u64);
    let n1432: ZW = zw_mix2(n1429, n1430, 39u64);
    let n1433: ZW = zw_bits_n(n875);
    let n1434: ZW = zw_mix1(n1431, n1433, 87u64);
    let n1435: ZW = zw_mix2(n1432, n1433, 87u64);
    let n1436: ZW = zw_bits_b(n887);
    let n1437: ZW = zw_mix1(n1407, n1436, 38u64);
    let n1438: ZW = zw_mix2(n1408, n1436, 38u64);
    let n1439: ZW = zw_bits_n(n886);
    let n1440: ZW = zw_mix1(n1437, n1439, 39u64);
    let n1441: ZW = zw_mix2(n1438, n1439, 39u64);
    let n1442: ZW = zw_bits_n(n885);
    let n1443: ZW = zw_mix1(n1440, n1442, 87u64);
    let n1444: ZW = zw_mix2(n1441, n1442, 87u64);
    let n1445: ZW = zw_bits_b(n897);
    let n1446: ZW = zw_mix1(n1407, n1445, 38u64);
    let n1447: ZW = zw_mix2(n1408, n1445, 38u64);
    let n1448: ZW = zw_bits_n(n896);
    let n1449: ZW = zw_mix1(n1446, n1448, 39u64);
    let n1450: ZW = zw_mix2(n1447, n1448, 39u64);
    let n1451: ZW = zw_bits_n(n895);
    let n1452: ZW = zw_mix1(n1449, n1451, 87u64);
    let n1453: ZW = zw_mix2(n1450, n1451, 87u64);
    let n1454: ZW = zw_bits_b(n907);
    let n1455: ZW = zw_mix1(n1407, n1454, 38u64);
    let n1456: ZW = zw_mix2(n1408, n1454, 38u64);
    let n1457: ZW = zw_bits_n(n906);
    let n1458: ZW = zw_mix1(n1455, n1457, 39u64);
    let n1459: ZW = zw_mix2(n1456, n1457, 39u64);
    let n1460: ZW = zw_bits_n(n905);
    let n1461: ZW = zw_mix1(n1458, n1460, 87u64);
    let n1462: ZW = zw_mix2(n1459, n1460, 87u64);
    let n1463: ZW = zw_bits_n(n914);
    let n1464: ZW = zw_mix1(n1404, n1463, 20u64);
    let n1465: ZW = zw_mix2(n1405, n1463, 20u64);
    let n1466: ZW = zw_mix1(n1464, n1409, 38u64);
    let n1467: ZW = zw_mix2(n1465, n1409, 38u64);
    let n1468: ZW = zw_mix1(n1466, n1412, 39u64);
    let n1469: ZW = zw_mix2(n1467, n1412, 39u64);
    let n1470: ZW = zw_mix1(n1468, n1415, 87u64);
    let n1471: ZW = zw_mix2(n1469, n1415, 87u64);
    let n1472: ZW = zw_mix1(n1464, n1418, 38u64);
    let n1473: ZW = zw_mix2(n1465, n1418, 38u64);
    let n1474: ZW = zw_mix1(n1472, n1421, 39u64);
    let n1475: ZW = zw_mix2(n1473, n1421, 39u64);
    let n1476: ZW = zw_mix1(n1474, n1424, 87u64);
    let n1477: ZW = zw_mix2(n1475, n1424, 87u64);
    let n1478: ZW = zw_mix1(n1464, n1427, 38u64);
    let n1479: ZW = zw_mix2(n1465, n1427, 38u64);
    let n1480: ZW = zw_mix1(n1478, n1430, 39u64);
    let n1481: ZW = zw_mix2(n1479, n1430, 39u64);
    let n1482: ZW = zw_mix1(n1480, n1433, 87u64);
    let n1483: ZW = zw_mix2(n1481, n1433, 87u64);
    let n1484: ZW = zw_mix1(n1464, n1436, 38u64);
    let n1485: ZW = zw_mix2(n1465, n1436, 38u64);
    let n1486: ZW = zw_mix1(n1484, n1439, 39u64);
    let n1487: ZW = zw_mix2(n1485, n1439, 39u64);
    let n1488: ZW = zw_mix1(n1486, n1442, 87u64);
    let n1489: ZW = zw_mix2(n1487, n1442, 87u64);
    let n1490: ZW = zw_mix1(n1464, n1445, 38u64);
    let n1491: ZW = zw_mix2(n1465, n1445, 38u64);
    let n1492: ZW = zw_mix1(n1490, n1448, 39u64);
    let n1493: ZW = zw_mix2(n1491, n1448, 39u64);
    let n1494: ZW = zw_mix1(n1492, n1451, 87u64);
    let n1495: ZW = zw_mix2(n1493, n1451, 87u64);
    let n1496: ZW = zw_mix1(n1464, n1454, 38u64);
    let n1497: ZW = zw_mix2(n1465, n1454, 38u64);
    let n1498: ZW = zw_mix1(n1496, n1457, 39u64);
    let n1499: ZW = zw_mix2(n1497, n1457, 39u64);
    let n1500: ZW = zw_mix1(n1498, n1460, 87u64);
    let n1501: ZW = zw_mix2(n1499, n1460, 87u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v0_b0: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b0: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n363);
    let ok_v1_b1: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v1_b1: bool = !n58 || !n57 || !n90 || !n89;
    let live_v1_b1: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n392);
    let ok_v2_b2: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v2_b2: bool = !n58 || !n57 || !n90 || !n89;
    let live_v2_b2: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n417);
    let ok_v16_b3: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v16_b3: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b3: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n446);
    let ok_v17_b4: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v17_b4: bool = !n58 || !n57 || !n90 || !n89;
    let live_v17_b4: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n475);
    let ok_v18_b5: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v18_b5: bool = !n58 || !n57 || !n90 || !n89;
    let live_v18_b5: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n504);
    let ok_v32_b6: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v32_b6: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b6: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v33_b7: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v33_b7: bool = !n58 || !n57 || !n90 || !n89;
    let live_v33_b7: u16 = ALL & zb_holds(n360) & zb_holds(n392);
    let ok_v34_b8: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v34_b8: bool = !n58 || !n57 || !n90 || !n89;
    let live_v34_b8: u16 = ALL & zb_holds(n360) & zb_holds(n417);
    let ok_v36_b9: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v36_b9: bool = !n58 || !n57 || !n90 || !n89;
    let live_v36_b9: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v37_b10: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v37_b10: bool = !n58 || !n57 || !n90 || !n89;
    let live_v37_b10: u16 = ALL & zb_holds(n360) & zb_holds(n392);
    let ok_v38_b11: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v38_b11: bool = !n58 || !n57 || !n90 || !n89;
    let live_v38_b11: u16 = ALL & zb_holds(n360) & zb_holds(n417);
    let ok_v40_b12: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v40_b12: bool = !n58 || !n57 || !n90 || !n89;
    let live_v40_b12: u16 = ALL & zb_holds(n360) & zb_holds(n363);
    let ok_v41_b13: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v41_b13: bool = !n58 || !n57 || !n90 || !n89;
    let live_v41_b13: u16 = ALL & zb_holds(n360) & zb_holds(n392);
    let ok_v42_b14: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v42_b14: bool = !n58 || !n57 || !n90 || !n89;
    let live_v42_b14: u16 = ALL & zb_holds(n360) & zb_holds(n417);
    let ok_v48_b15: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v48_b15: bool = !n58 || !n57 || !n90 || !n89;
    let live_v48_b15: u16 = ALL & zb_holds(n360) & zb_holds(n446);
    let ok_v49_b16: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v49_b16: bool = !n58 || !n57 || !n90 || !n89;
    let live_v49_b16: u16 = ALL & zb_holds(n360) & zb_holds(n475);
    let ok_v50_b17: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v50_b17: bool = !n58 || !n57 || !n90 || !n89;
    let live_v50_b17: u16 = ALL & zb_holds(n360) & zb_holds(n504);
    let ok_v52_b18: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v52_b18: bool = !n58 || !n57 || !n90 || !n89;
    let live_v52_b18: u16 = ALL & zb_holds(n360) & zb_holds(n446);
    let ok_v53_b19: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v53_b19: bool = !n58 || !n57 || !n90 || !n89;
    let live_v53_b19: u16 = ALL & zb_holds(n360) & zb_holds(n475);
    let ok_v54_b20: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v54_b20: bool = !n58 || !n57 || !n90 || !n89;
    let live_v54_b20: u16 = ALL & zb_holds(n360) & zb_holds(n504);
    let ok_v56_b21: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v56_b21: bool = !n58 || !n57 || !n90 || !n89;
    let live_v56_b21: u16 = ALL & zb_holds(n360) & zb_holds(n446);
    let ok_v57_b22: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v57_b22: bool = !n58 || !n57 || !n90 || !n89;
    let live_v57_b22: u16 = ALL & zb_holds(n360) & zb_holds(n475);
    let ok_v58_b23: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n315);
    let bd_v58_b23: bool = !n58 || !n57 || !n90 || !n89;
    let live_v58_b23: u16 = ALL & zb_holds(n360) & zb_holds(n504);
    let ok_v0_b24: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v0_b24: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b24: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n559);
    let ok_v1_b25: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v1_b25: bool = !n58 || !n57 || !n90 || !n89;
    let live_v1_b25: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n580);
    let ok_v2_b26: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v2_b26: bool = !n58 || !n57 || !n90 || !n89;
    let live_v2_b26: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n600);
    let ok_v16_b27: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v16_b27: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b27: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n629);
    let ok_v17_b28: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v17_b28: bool = !n58 || !n57 || !n90 || !n89;
    let live_v17_b28: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n658);
    let ok_v18_b29: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v18_b29: bool = !n58 || !n57 || !n90 || !n89;
    let live_v18_b29: u16 = ALL & zb_holds(n76) & zb_holds(n360) & zb_holds(n687);
    let ok_v32_b30: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v32_b30: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b30: u16 = ALL & zb_holds(n360) & zb_holds(n559);
    let ok_v33_b31: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v33_b31: bool = !n58 || !n57 || !n90 || !n89;
    let live_v33_b31: u16 = ALL & zb_holds(n360) & zb_holds(n580);
    let ok_v34_b32: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v34_b32: bool = !n58 || !n57 || !n90 || !n89;
    let live_v34_b32: u16 = ALL & zb_holds(n360) & zb_holds(n600);
    let ok_v48_b33: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v48_b33: bool = !n58 || !n57 || !n90 || !n89;
    let live_v48_b33: u16 = ALL & zb_holds(n360) & zb_holds(n629);
    let ok_v49_b34: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v49_b34: bool = !n58 || !n57 || !n90 || !n89;
    let live_v49_b34: u16 = ALL & zb_holds(n360) & zb_holds(n658);
    let ok_v50_b35: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n541);
    let bd_v50_b35: bool = !n58 || !n57 || !n90 || !n89;
    let live_v50_b35: u16 = ALL & zb_holds(n360) & zb_holds(n687);
    let ok_v0_b36: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n739);
    let bd_v0_b36: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b36: u16 = if true { 0 } else { ALL };
    let ok_v1_b37: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n756);
    let bd_v1_b37: bool = !n58 || !n57 || !n90 || !n89;
    let live_v1_b37: u16 = if true { 0 } else { ALL };
    let ok_v2_b38: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n770);
    let bd_v2_b38: bool = !n58 || !n57 || !n90 || !n89;
    let live_v2_b38: u16 = if true { 0 } else { ALL };
    let ok_v16_b39: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n784);
    let bd_v16_b39: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b39: u16 = ALL & zb_holds(n76) & zb_holds(n742) & zb_holds(n783);
    let ok_v17_b40: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n799);
    let bd_v17_b40: bool = !n58 || !n57 || !n90 || !n89;
    let live_v17_b40: u16 = ALL & zb_holds(n76) & zb_holds(n742) & zb_holds(n798);
    let ok_v18_b41: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n814);
    let bd_v18_b41: bool = !n58 || !n57 || !n90 || !n89;
    let live_v18_b41: u16 = ALL & zb_holds(n76) & zb_holds(n742) & zb_holds(n813);
    let ok_v32_b42: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n739);
    let bd_v32_b42: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b42: u16 = ALL & zb_holds(n823);
    let ok_v33_b43: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n756);
    let bd_v33_b43: bool = !n58 || !n57 || !n90 || !n89;
    let live_v33_b43: u16 = ALL & zb_holds(n826);
    let ok_v34_b44: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n770);
    let bd_v34_b44: bool = !n58 || !n57 || !n90 || !n89;
    let live_v34_b44: u16 = ALL & zb_holds(n829);
    let ok_v48_b45: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n784);
    let bd_v48_b45: bool = !n58 || !n57 || !n90 || !n89;
    let live_v48_b45: u16 = ALL & zb_holds(n832);
    let ok_v49_b46: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n799);
    let bd_v49_b46: bool = !n58 || !n57 || !n90 || !n89;
    let live_v49_b46: u16 = ALL & zb_holds(n835);
    let ok_v50_b47: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n814);
    let bd_v50_b47: bool = !n58 || !n57 || !n90 || !n89;
    let live_v50_b47: u16 = ALL & zb_holds(n838);
    let ok_v0_b48: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n855);
    let bd_v0_b48: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b48: u16 = ALL & zb_holds(n860);
    let ok_v1_b49: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n868);
    let bd_v1_b49: bool = !n58 || !n57 || !n90 || !n89;
    let live_v1_b49: u16 = ALL & zb_holds(n871);
    let ok_v2_b50: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n879);
    let bd_v2_b50: bool = !n58 || !n57 || !n90 || !n89;
    let live_v2_b50: u16 = ALL & zb_holds(n882);
    let ok_v16_b51: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n889);
    let bd_v16_b51: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b51: u16 = ALL & zb_holds(n892);
    let ok_v17_b52: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n899);
    let bd_v17_b52: bool = !n58 || !n57 || !n90 || !n89;
    let live_v17_b52: u16 = ALL & zb_holds(n902);
    let ok_v18_b53: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n909);
    let bd_v18_b53: bool = !n58 || !n57 || !n90 || !n89;
    let live_v18_b53: u16 = ALL & zb_holds(n912);
    let ok_v32_b54: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n855);
    let bd_v32_b54: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b54: u16 = ALL & zb_holds(n920);
    let ok_v33_b55: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n868);
    let bd_v33_b55: bool = !n58 || !n57 || !n90 || !n89;
    let live_v33_b55: u16 = ALL & zb_holds(n924);
    let ok_v34_b56: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n879);
    let bd_v34_b56: bool = !n58 || !n57 || !n90 || !n89;
    let live_v34_b56: u16 = ALL & zb_holds(n928);
    let ok_v48_b57: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n889);
    let bd_v48_b57: bool = !n58 || !n57 || !n90 || !n89;
    let live_v48_b57: u16 = ALL & zb_holds(n931);
    let ok_v49_b58: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n899);
    let bd_v49_b58: bool = !n58 || !n57 || !n90 || !n89;
    let live_v49_b58: u16 = ALL & zb_holds(n934);
    let ok_v50_b59: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n909);
    let bd_v50_b59: bool = !n58 || !n57 || !n90 || !n89;
    let live_v50_b59: u16 = ALL & zb_holds(n937);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n63,
        c86: n103,
        c256: n105,
        c85: n104,
    };
    let sh1 = KShared1 {
        c87: n539,
        c84: n63,
        c86: n103,
        c85: n104,
    };
    let sh2 = KShared2 {
        c84: n63,
        c86: n103,
        c237: n732,
        c275: n736,
        c248: n733,
        c249: n734,
        c277: n737,
        c253: n735,
        c161: n731,
        c85: n104,
    };
    let sh3 = KShared3 {
        c84: n63,
        c86: n103,
        c235: n848,
        c273: n852,
        c246: n849,
        c247: n850,
        c275: n853,
        c251: n851,
        c159: n847,
        c85: n104,
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
    let mut take_3_1: u16 = 0;
    let mut take_3_2: u16 = 0;
    let mut take_3_3: u16 = 0;
    let mut take_3_4: u16 = 0;
    let mut take_3_5: u16 = 0;
    let mut take_3_6: u16 = 0;
    let mut take_3_7: u16 = 0;
    let mut take_3_8: u16 = 0;
    let mut take_3_9: u16 = 0;
    let mut take_3_10: u16 = 0;
    let mut take_3_11: u16 = 0;
    // 60 distinct button assignments; per outcome they fall
    // into [24, 2, 12, 12] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: n340,
        h1: n993, h2: n994,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(true),
        c241: n329,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n370,
        c283: n340,
        h1: n1001, h2: n1002,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n395,
        c283: n340,
        h1: n1006, h2: n1007,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n357,
        c283: n358,
        h1: n1029, h2: n1030,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(true),
        c241: n356,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n389,
        c283: n358,
        h1: n1036, h2: n1037,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c270: zn_splat(P8::from_raw(0i32)),
        c271: zn_splat(P8::from_raw(0i32)),
        c236: zn_splat(P8::from_raw(-65536i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n414,
        c283: n358,
        h1: n1041, h2: n1042,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1076, h2: n1077,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1088, h2: n1089,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1093, h2: n1094,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(98304i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n1108, h2: n1109,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1121, h2: n1122,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1132, h2: n1133,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(98304i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n1140, h2: n1141,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1148, h2: n1149,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n329,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1156, h2: n1157,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1176, h2: n1177,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1186, h2: n1187,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(98304i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1190, h2: n1191,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(98304i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n1204, h2: n1205,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1216, h2: n1217,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(-98304i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1226, h2: n1227,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(98304i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n1234, h2: n1235,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(-131072i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(true),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1242, h2: n1243,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c270: zn_splat(P8::from_raw(69510i32)),
        c271: zn_splat(P8::from_raw(69510i32)),
        c236: zn_splat(P8::from_raw(655360i32)),
        c272: zn_splat(P8::from_raw(131072i32)),
        c273: zn_splat(P8::from_raw(131072i32)),
        c238: zn_splat(P8::from_raw(262144i32)),
        c239: zn_splat(P8::from_raw(0i32)),
        c274: zb_splat(false),
        c241: n356,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1250, h2: n1251,
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
        h1: n1263, h2: n1264,
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
        h1: n1267, h2: n1268,
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n727,
        c39: n728,
        c20: r_c20,
        c38: n730,
        h1: n1299, h2: n1300,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n752,
        c39: n753,
        c20: r_c20,
        c38: n754,
        h1: n1308, h2: n1309,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n766,
        c39: n767,
        c20: r_c20,
        c38: n768,
        h1: n1317, h2: n1318,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n780,
        c39: n781,
        c20: r_c20,
        c38: n782,
        h1: n1326, h2: n1327,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n795,
        c39: n796,
        c20: r_c20,
        c38: n797,
        h1: n1335, h2: n1336,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n810,
        c39: n811,
        c20: r_c20,
        c38: n812,
        h1: n1344, h2: n1345,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n727,
        c39: n728,
        c20: n818,
        c38: n730,
        h1: n1353, h2: n1354,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n752,
        c39: n753,
        c20: n818,
        c38: n754,
        h1: n1359, h2: n1360,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n766,
        c39: n767,
        c20: n818,
        c38: n768,
        h1: n1365, h2: n1366,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n780,
        c39: n781,
        c20: n818,
        c38: n782,
        h1: n1371, h2: n1372,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n795,
        c39: n796,
        c20: n818,
        c38: n797,
        h1: n1377, h2: n1378,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n810,
        c39: n811,
        c20: n818,
        c38: n812,
        h1: n1383, h2: n1384,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n843,
        c39: n844,
        c20: n845,
        c38: n846,
        h1: n1416, h2: n1417,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b49 & (if bd_v1_b49 { ALL } else { !ok_v1_b49 });
    take_3_1 |= live_v1_b49 & ok_v1_b49 & (if bd_v1_b49 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n864,
        c39: n865,
        c20: n845,
        c38: n866,
        h1: n1425, h2: n1426,
    };
    // body 49: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b50 & (if bd_v2_b50 { ALL } else { !ok_v2_b50 });
    take_3_2 |= live_v2_b50 & ok_v2_b50 & (if bd_v2_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n875,
        c39: n876,
        c20: n845,
        c38: n877,
        h1: n1434, h2: n1435,
    };
    // body 50: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b51 & (if bd_v16_b51 { ALL } else { !ok_v16_b51 });
    take_3_3 |= live_v16_b51 & ok_v16_b51 & (if bd_v16_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n885,
        c39: n886,
        c20: n845,
        c38: n887,
        h1: n1443, h2: n1444,
    };
    // body 51: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b52 & (if bd_v17_b52 { ALL } else { !ok_v17_b52 });
    take_3_4 |= live_v17_b52 & ok_v17_b52 & (if bd_v17_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n895,
        c39: n896,
        c20: n845,
        c38: n897,
        h1: n1452, h2: n1453,
    };
    // body 52: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b53 & (if bd_v18_b53 { ALL } else { !ok_v18_b53 });
    take_3_5 |= live_v18_b53 & ok_v18_b53 & (if bd_v18_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n905,
        c39: n906,
        c20: n845,
        c38: n907,
        h1: n1461, h2: n1462,
    };
    // body 53: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b54 & (if bd_v32_b54 { ALL } else { !ok_v32_b54 });
    take_3_6 |= live_v32_b54 & ok_v32_b54 & (if bd_v32_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n843,
        c39: n844,
        c20: n914,
        c38: n846,
        h1: n1470, h2: n1471,
    };
    // body 54: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b55 & (if bd_v33_b55 { ALL } else { !ok_v33_b55 });
    take_3_7 |= live_v33_b55 & ok_v33_b55 & (if bd_v33_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n864,
        c39: n865,
        c20: n914,
        c38: n866,
        h1: n1476, h2: n1477,
    };
    // body 55: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b56 & (if bd_v34_b56 { ALL } else { !ok_v34_b56 });
    take_3_8 |= live_v34_b56 & ok_v34_b56 & (if bd_v34_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n875,
        c39: n876,
        c20: n914,
        c38: n877,
        h1: n1482, h2: n1483,
    };
    // body 56: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v48_b57 & (if bd_v48_b57 { ALL } else { !ok_v48_b57 });
    take_3_9 |= live_v48_b57 & ok_v48_b57 & (if bd_v48_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n885,
        c39: n886,
        c20: n914,
        c38: n887,
        h1: n1488, h2: n1489,
    };
    // body 57: buttons 0x30, forks 0x0
    sink.o3(48, take_3_9, &sh3, &o3);
    declined |= live_v49_b58 & (if bd_v49_b58 { ALL } else { !ok_v49_b58 });
    take_3_10 |= live_v49_b58 & ok_v49_b58 & (if bd_v49_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n895,
        c39: n896,
        c20: n914,
        c38: n897,
        h1: n1494, h2: n1495,
    };
    // body 58: buttons 0x31, forks 0x0
    sink.o3(49, take_3_10, &sh3, &o3);
    declined |= live_v50_b59 & (if bd_v50_b59 { ALL } else { !ok_v50_b59 });
    take_3_11 |= live_v50_b59 & ok_v50_b59 & (if bd_v50_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n905,
        c39: n906,
        c20: n914,
        c38: n907,
        h1: n1500, h2: n1501,
    };
    // body 59: buttons 0x32, forks 0x0
    sink.o3(50, take_3_11, &sh3, &o3);
    declined
}
