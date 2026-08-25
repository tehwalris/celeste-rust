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
    pub c39: ZN,
    pub c256: ZN,
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
    pub c237: ZN,
    pub c275: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c277: ZN,
    pub c253: ZN,
    pub c161: ZN,
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
    pub c235: ZN,
    pub c273: ZN,
    pub c246: ZN,
    pub c247: ZN,
    pub c275: ZN,
    pub c251: ZN,
    pub c159: ZN,
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[280] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[281] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[255] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[256] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[205] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[197] = Col::U(AV::Bool(true));
    b.cols[199] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[181] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[187] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[237] { v.push(sh.c237.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[248] { v.push(sh.c248.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[161] { v.push(sh.c161.lane(i)); }
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[179] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[235] { v.push(sh.c235.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[247] { v.push(sh.c247.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[251] { v.push(sh.c251.lane(i)); }
        if let Col::N(v) = &mut acc.cols[159] { v.push(sh.c159.lane(i)); }
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
    let n56: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n57: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n58: ZB = zb_not(r_c41);
    let n59: bool = P8::from_raw(0i32) == u.c268;
    let n60: bool = P8::from_raw(0i32) == u.c269;
    let n61: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c271);
    let n62: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n75: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n77: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c272);
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(196608i32)), r_c246);
    let n80: ZB = zb_not(r_c42);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n89: ZB = zb_not(r_c264);
    let n90: ZB = zb_not(r_c265);
    let n91: bool = P8::from_raw(524288i32) == u.c266;
    let n92: bool = P8::from_raw(524288i32) == u.c267;
    let n93: ZB = zb_not(r_c244);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(-262144i32)), r_c273);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c247);
    let n96: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c274);
    let n97: ZB = zn_eq(zn_splat(P8::from_raw(6291456i32)), r_c275);
    let n98: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c250);
    let n99: ZB = zb_not(r_c43);
    let n100: ZB = zb_not(r_c38);
    let n101: ZN = zn_add(zn_splat(P8::from_raw(-262144i32)), r_c251);
    let n120: ZB = zb_not(n95);
    let n121: ZB = zb_and(n75, n120);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c247);
    let n123: ZB = zb_not(n122);
    let n124: ZB = zb_and(n121, n123);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c247);
    let n126: ZB = zb_and(n124, n125);
    let n127: ZN = zn_sub(r_c235, zn_splat(P8::from_raw(65536i32)));
    let n128: ZB = zn_lt(n127, zn_splat(P8::from_raw(0i32)));
    let n129: ZB = zb_and(n126, n128);
    let n130: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n101);
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
    let n141: ZB = zb_and(n129, n139);
    let n142: ZB = zb_and(n129, n140);
    let n143: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n138);
    let n144: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n143);
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
    let n203: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n202);
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
    let n257: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n256);
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
    let n313: ZB = zn_le(n101, zn_splat(P8::from_raw(8388608i32)));
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
    let n356: ZB = zn_lt(n101, zn_splat(P8::from_raw(-262144i32)));
    let n357: ZB = zn_ge(n101, zn_splat(P8::from_raw(-262144i32)));
    let n358: ZB = zb_and(n338, n356);
    let n359: ZB = zb_and(n338, n357);
    let n360: ZB = zb_or(n358, n359);
    let n362: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n367: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n332);
    let n368: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n369: ZB = zb_not(n368);
    let n370: ZB = zb_and(n331, n369);
    let n371: ZB = zb_and(n331, n368);
    let n372: ZB = zb_or(n370, n371);
    let n373: ZB = zb_and(n369, n372);
    let n374: ZB = zb_and(n368, n372);
    let n375: ZB = zb_or(n373, n374);
    let n376: ZB = zb_and(n368, n375);
    let n377: ZB = zb_and(n369, n375);
    let n378: ZB = zb_or(n376, n377);
    let n379: ZB = zb_and(n368, n378);
    let n380: ZB = zb_and(n369, n378);
    let n381: ZB = zb_or(n379, n380);
    let n382: ZB = zb_and(n317, n381);
    let n383: ZB = zb_and(n316, n381);
    let n384: ZB = zb_or(n382, n383);
    let n385: ZN = zsel_n(n348, n350, n367);
    let n386: ZN = zsel_n(n339, n367, n385);
    let n387: ZB = zb_and(n356, n384);
    let n388: ZB = zb_and(n357, n384);
    let n389: ZB = zb_or(n387, n388);
    let n392: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n333);
    let n393: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n334, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n394: ZB = zb_not(n393);
    let n395: ZB = zb_and(n331, n394);
    let n396: ZB = zb_and(n331, n393);
    let n397: ZB = zb_or(n395, n396);
    let n398: ZB = zb_and(n394, n397);
    let n399: ZB = zb_and(n393, n397);
    let n400: ZB = zb_or(n398, n399);
    let n401: ZB = zb_and(n393, n400);
    let n402: ZB = zb_and(n394, n400);
    let n403: ZB = zb_or(n401, n402);
    let n404: ZB = zb_and(n393, n403);
    let n405: ZB = zb_and(n394, n403);
    let n406: ZB = zb_or(n404, n405);
    let n407: ZB = zb_and(n317, n406);
    let n408: ZB = zb_and(n316, n406);
    let n409: ZB = zb_or(n407, n408);
    let n410: ZN = zsel_n(n348, n350, n392);
    let n411: ZN = zsel_n(n339, n392, n410);
    let n412: ZB = zb_and(n356, n409);
    let n413: ZB = zb_and(n357, n409);
    let n414: ZB = zb_or(n412, n413);
    let n417: ZB = zb_and(n338, n339);
    let n418: ZB = zb_and(n338, n340);
    let n419: ZB = zb_and(n342, n418);
    let n420: ZB = zb_and(n341, n418);
    let n421: ZB = zb_or(n419, n420);
    let n422: ZB = zb_and(n342, n421);
    let n423: ZB = zb_and(n341, n421);
    let n424: ZB = zb_or(n422, n423);
    let n425: ZB = zb_and(n341, n424);
    let n426: ZB = zb_and(n342, n424);
    let n427: ZB = zb_and(n344, n426);
    let n428: ZB = zb_and(n343, n426);
    let n429: ZB = zb_or(n427, n428);
    let n430: ZB = zb_and(n344, n429);
    let n431: ZB = zb_and(n343, n429);
    let n432: ZB = zb_or(n430, n431);
    let n433: ZB = zb_and(n343, n432);
    let n434: ZB = zb_and(n344, n432);
    let n435: ZB = zb_or(n433, n434);
    let n436: ZB = zb_or(n425, n435);
    let n437: ZB = zb_and(n348, n436);
    let n438: ZB = zb_and(n347, n436);
    let n439: ZB = zb_or(n437, n438);
    let n440: ZB = zb_or(n417, n439);
    let n441: ZB = zb_and(n356, n440);
    let n442: ZB = zb_and(n357, n440);
    let n443: ZB = zb_or(n441, n442);
    let n446: ZB = zb_and(n339, n384);
    let n447: ZB = zb_and(n340, n384);
    let n448: ZB = zb_and(n342, n447);
    let n449: ZB = zb_and(n341, n447);
    let n450: ZB = zb_or(n448, n449);
    let n451: ZB = zb_and(n342, n450);
    let n452: ZB = zb_and(n341, n450);
    let n453: ZB = zb_or(n451, n452);
    let n454: ZB = zb_and(n341, n453);
    let n455: ZB = zb_and(n342, n453);
    let n456: ZB = zb_and(n344, n455);
    let n457: ZB = zb_and(n343, n455);
    let n458: ZB = zb_or(n456, n457);
    let n459: ZB = zb_and(n344, n458);
    let n460: ZB = zb_and(n343, n458);
    let n461: ZB = zb_or(n459, n460);
    let n462: ZB = zb_and(n343, n461);
    let n463: ZB = zb_and(n344, n461);
    let n464: ZB = zb_or(n462, n463);
    let n465: ZB = zb_or(n454, n464);
    let n466: ZB = zb_and(n348, n465);
    let n467: ZB = zb_and(n347, n465);
    let n468: ZB = zb_or(n466, n467);
    let n469: ZB = zb_or(n446, n468);
    let n470: ZB = zb_and(n356, n469);
    let n471: ZB = zb_and(n357, n469);
    let n472: ZB = zb_or(n470, n471);
    let n475: ZB = zb_and(n339, n409);
    let n476: ZB = zb_and(n340, n409);
    let n477: ZB = zb_and(n342, n476);
    let n478: ZB = zb_and(n341, n476);
    let n479: ZB = zb_or(n477, n478);
    let n480: ZB = zb_and(n342, n479);
    let n481: ZB = zb_and(n341, n479);
    let n482: ZB = zb_or(n480, n481);
    let n483: ZB = zb_and(n341, n482);
    let n484: ZB = zb_and(n342, n482);
    let n485: ZB = zb_and(n344, n484);
    let n486: ZB = zb_and(n343, n484);
    let n487: ZB = zb_or(n485, n486);
    let n488: ZB = zb_and(n344, n487);
    let n489: ZB = zb_and(n343, n487);
    let n490: ZB = zb_or(n488, n489);
    let n491: ZB = zb_and(n343, n490);
    let n492: ZB = zb_and(n344, n490);
    let n493: ZB = zb_or(n491, n492);
    let n494: ZB = zb_or(n483, n493);
    let n495: ZB = zb_and(n348, n494);
    let n496: ZB = zb_and(n347, n494);
    let n497: ZB = zb_or(n495, n496);
    let n498: ZB = zb_or(n475, n497);
    let n499: ZB = zb_and(n356, n498);
    let n500: ZB = zb_and(n357, n498);
    let n501: ZB = zb_or(n499, n500);
    let n505: ZB = zb_and(n160, n164);
    let n506: ZB = zb_and(n173, n177);
    let n507: ZB = zb_and(n179, n186);
    let n508: ZB = zb_and(n188, n195);
    let n509: ZB = zb_or(n507, n508);
    let n510: ZB = zb_or(n506, n509);
    let n511: ZB = zb_or(n505, n510);
    let n512: ZB = zb_and(n216, n220);
    let n513: ZB = zb_and(n227, n231);
    let n514: ZB = zb_and(n233, n240);
    let n515: ZB = zb_and(n242, n249);
    let n516: ZB = zb_or(n514, n515);
    let n517: ZB = zb_or(n513, n516);
    let n518: ZB = zb_or(n512, n517);
    let n519: ZB = zb_and(n270, n274);
    let n520: ZB = zb_and(n281, n285);
    let n521: ZB = zb_and(n287, n294);
    let n522: ZB = zb_and(n296, n303);
    let n523: ZB = zb_or(n521, n522);
    let n524: ZB = zb_or(n520, n523);
    let n525: ZB = zb_or(n519, n524);
    let n526: ZB = zb_or(n518, n525);
    let n527: ZB = zb_or(n511, n526);
    let n528: ZB = zn_gt(n101, zn_splat(P8::from_raw(8388608i32)));
    let n529: ZB = zb_and(n527, n528);
    let n530: ZB = zb_and(n313, n527);
    let n531: ZB = zb_or(n529, n530);
    let n532: ZB = zb_and(n311, n528);
    let n533: ZB = zb_or(n531, n532);
    let n534: ZB = zb_or(n312, n531);
    let n535: ZB = zb_and(n317, n533);
    let n536: ZB = zb_and(n316, n533);
    let n537: ZB = zb_or(n535, n536);
    let n538: ZB = zb_and(n317, n537);
    let n539: ZB = zb_and(n316, n537);
    let n540: ZB = zb_or(n538, n539);
    let n541: ZB = zb_and(n316, n540);
    let n542: ZB = zb_and(n317, n540);
    let n543: ZB = zb_or(n541, n542);
    let n544: ZB = zb_and(n317, n543);
    let n545: ZB = zb_and(n316, n543);
    let n546: ZB = zb_or(n544, n545);
    let n547: ZB = zb_and(n317, n546);
    let n548: ZB = zb_and(n316, n546);
    let n549: ZB = zb_or(n547, n548);
    let n550: ZB = zb_and(n356, n549);
    let n551: ZB = zb_and(n357, n549);
    let n552: ZB = zb_or(n550, n551);
    let n556: ZB = zb_and(n369, n546);
    let n557: ZB = zb_and(n368, n546);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n369, n558);
    let n560: ZB = zb_and(n368, n558);
    let n561: ZB = zb_or(n559, n560);
    let n562: ZB = zb_and(n368, n561);
    let n563: ZB = zb_and(n369, n561);
    let n564: ZB = zb_or(n562, n563);
    let n565: ZB = zb_and(n368, n564);
    let n566: ZB = zb_and(n369, n564);
    let n567: ZB = zb_or(n565, n566);
    let n568: ZB = zb_and(n317, n567);
    let n569: ZB = zb_and(n316, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n356, n570);
    let n572: ZB = zb_and(n357, n570);
    let n573: ZB = zb_or(n571, n572);
    let n576: ZB = zb_and(n394, n546);
    let n577: ZB = zb_and(n393, n546);
    let n578: ZB = zb_or(n576, n577);
    let n579: ZB = zb_and(n394, n578);
    let n580: ZB = zb_and(n393, n578);
    let n581: ZB = zb_or(n579, n580);
    let n582: ZB = zb_and(n393, n581);
    let n583: ZB = zb_and(n394, n581);
    let n584: ZB = zb_or(n582, n583);
    let n585: ZB = zb_and(n393, n584);
    let n586: ZB = zb_and(n394, n584);
    let n587: ZB = zb_or(n585, n586);
    let n588: ZB = zb_and(n317, n587);
    let n589: ZB = zb_and(n316, n587);
    let n590: ZB = zb_or(n588, n589);
    let n591: ZB = zb_and(n356, n590);
    let n592: ZB = zb_and(n357, n590);
    let n593: ZB = zb_or(n591, n592);
    let n596: ZB = zb_and(n339, n549);
    let n597: ZB = zb_and(n340, n549);
    let n598: ZB = zb_and(n342, n597);
    let n599: ZB = zb_and(n341, n597);
    let n600: ZB = zb_or(n598, n599);
    let n601: ZB = zb_and(n342, n600);
    let n602: ZB = zb_and(n341, n600);
    let n603: ZB = zb_or(n601, n602);
    let n604: ZB = zb_and(n341, n603);
    let n605: ZB = zb_and(n342, n603);
    let n606: ZB = zb_and(n344, n605);
    let n607: ZB = zb_and(n343, n605);
    let n608: ZB = zb_or(n606, n607);
    let n609: ZB = zb_and(n344, n608);
    let n610: ZB = zb_and(n343, n608);
    let n611: ZB = zb_or(n609, n610);
    let n612: ZB = zb_and(n343, n611);
    let n613: ZB = zb_and(n344, n611);
    let n614: ZB = zb_or(n612, n613);
    let n615: ZB = zb_or(n604, n614);
    let n616: ZB = zb_and(n348, n615);
    let n617: ZB = zb_and(n347, n615);
    let n618: ZB = zb_or(n616, n617);
    let n619: ZB = zb_or(n596, n618);
    let n620: ZB = zb_and(n356, n619);
    let n621: ZB = zb_and(n357, n619);
    let n622: ZB = zb_or(n620, n621);
    let n625: ZB = zb_and(n339, n570);
    let n626: ZB = zb_and(n340, n570);
    let n627: ZB = zb_and(n342, n626);
    let n628: ZB = zb_and(n341, n626);
    let n629: ZB = zb_or(n627, n628);
    let n630: ZB = zb_and(n342, n629);
    let n631: ZB = zb_and(n341, n629);
    let n632: ZB = zb_or(n630, n631);
    let n633: ZB = zb_and(n341, n632);
    let n634: ZB = zb_and(n342, n632);
    let n635: ZB = zb_and(n344, n634);
    let n636: ZB = zb_and(n343, n634);
    let n637: ZB = zb_or(n635, n636);
    let n638: ZB = zb_and(n344, n637);
    let n639: ZB = zb_and(n343, n637);
    let n640: ZB = zb_or(n638, n639);
    let n641: ZB = zb_and(n343, n640);
    let n642: ZB = zb_and(n344, n640);
    let n643: ZB = zb_or(n641, n642);
    let n644: ZB = zb_or(n633, n643);
    let n645: ZB = zb_and(n348, n644);
    let n646: ZB = zb_and(n347, n644);
    let n647: ZB = zb_or(n645, n646);
    let n648: ZB = zb_or(n625, n647);
    let n649: ZB = zb_and(n356, n648);
    let n650: ZB = zb_and(n357, n648);
    let n651: ZB = zb_or(n649, n650);
    let n654: ZB = zb_and(n339, n590);
    let n655: ZB = zb_and(n340, n590);
    let n656: ZB = zb_and(n342, n655);
    let n657: ZB = zb_and(n341, n655);
    let n658: ZB = zb_or(n656, n657);
    let n659: ZB = zb_and(n342, n658);
    let n660: ZB = zb_and(n341, n658);
    let n661: ZB = zb_or(n659, n660);
    let n662: ZB = zb_and(n341, n661);
    let n663: ZB = zb_and(n342, n661);
    let n664: ZB = zb_and(n344, n663);
    let n665: ZB = zb_and(n343, n663);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZB = zb_and(n344, n666);
    let n668: ZB = zb_and(n343, n666);
    let n669: ZB = zb_or(n667, n668);
    let n670: ZB = zb_and(n343, n669);
    let n671: ZB = zb_and(n344, n669);
    let n672: ZB = zb_or(n670, n671);
    let n673: ZB = zb_or(n662, n672);
    let n674: ZB = zb_and(n348, n673);
    let n675: ZB = zb_and(n347, n673);
    let n676: ZB = zb_or(n674, n675);
    let n677: ZB = zb_or(n654, n676);
    let n678: ZB = zb_and(n356, n677);
    let n679: ZB = zb_and(n357, n677);
    let n680: ZB = zb_or(n678, n679);
    let n688: ZB = zn_lt(n101, zn_splat(P8::from_raw(7340032i32)));
    let n689: ZB = zn_ge(n101, zn_splat(P8::from_raw(7340032i32)));
    let n690: ZB = zb_and(n75, n95);
    let n691: ZB = zb_and(n688, n690);
    let n692: ZB = zb_and(n689, n690);
    let n693: ZN = zsel_n(n688, zn_splat(P8::from_raw(196608i32)), r_c235);
    let n694: ZN = zsel_n(n688, zn_splat(P8::from_raw(65536i32)), r_c247);
    let n695: ZB = zb_or(n691, n692);
    let n696: ZB = zb_and(n121, n122);
    let n697: ZB = zb_not(n125);
    let n698: ZB = zb_and(n124, n697);
    let n699: ZB = zn_ge(n127, zn_splat(P8::from_raw(0i32)));
    let n700: ZB = zb_and(n126, n699);
    let n701: ZN = zsel_n(n125, n127, r_c235);
    let n702: ZN = zsel_n(n125, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(196608i32)));
    let n703: ZB = zb_or(n698, n700);
    let n704: ZN = zsel_n(n122, r_c235, n701);
    let n705: ZN = zsel_n(n122, zn_splat(P8::from_raw(196608i32)), n702);
    let n706: ZN = zsel_n(n122, zn_splat(P8::from_raw(-229376i32)), zn_splat(P8::from_raw(-262144i32)));
    let n707: ZB = zb_or(n696, n703);
    let n708: ZN = zsel_n(n95, n693, n704);
    let n709: ZN = zsel_n(n95, zn_splat(P8::from_raw(196608i32)), n705);
    let n710: ZN = zsel_n(n95, n694, r_c247);
    let n711: ZN = zsel_n(n95, zn_splat(P8::from_raw(-262144i32)), n706);
    let n712: ZB = zb_or(n695, n707);
    let n713: ZB = zb_and(n356, n360);
    let n714: ZB = zb_and(n356, n552);
    let n715: ZN = zsel_n(n713, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n716: ZB = zb_not(n713);
    let n717: ZB = zb_or(n713, n714);
    let n718: ZB = zsel_b(n713, n312, n534);
    let n719: ZN = zsel_n(n712, r_c39, n715);
    let n720: ZB = zb_not(n712);
    let n721: ZB = zb_and(n716, n720);
    let n722: ZN = zsel_n(n712, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n723: ZN = zsel_n(n712, n708, zn_splat(P8::from_raw(196608i32)));
    let n724: ZN = zsel_n(n712, n709, zn_splat(P8::from_raw(196608i32)));
    let n725: ZN = zsel_n(n712, n710, zn_splat(P8::from_raw(65536i32)));
    let n726: ZN = zsel_n(n712, n101, zn_splat(P8::from_raw(8126464i32)));
    let n727: ZN = zsel_n(n712, n711, zn_splat(P8::from_raw(-262144i32)));
    let n728: ZN = zsel_n(n712, zn_splat(P8::from_raw(6291456i32)), zn_splat(P8::from_raw(7340032i32)));
    let n729: ZB = zb_or(n712, n717);
    let n730: ZB = zb_or(n712, n718);
    let n731: ZN = zn_rem(n722, zn_splat(P8::from_raw(524288i32)));
    let n732: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n731);
    let n733: ZB = zn_eq(zn_splat(P8::from_raw(2031616i32)), n732);
    let n734: ZB = zb_and(n729, n733);
    let n736: ZB = zb_and(n356, n389);
    let n737: ZB = zb_and(n356, n573);
    let n738: ZN = zsel_n(n736, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n739: ZB = zb_not(n736);
    let n740: ZB = zb_or(n736, n737);
    let n741: ZB = zsel_b(n736, n312, n534);
    let n742: ZN = zsel_n(n712, r_c39, n738);
    let n743: ZB = zb_and(n720, n739);
    let n744: ZB = zb_or(n712, n740);
    let n745: ZB = zb_or(n712, n741);
    let n746: ZB = zb_and(n733, n744);
    let n748: ZB = zb_and(n356, n414);
    let n749: ZB = zb_and(n356, n593);
    let n750: ZN = zsel_n(n748, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n751: ZB = zb_not(n748);
    let n752: ZB = zb_or(n748, n749);
    let n753: ZB = zsel_b(n748, n312, n534);
    let n754: ZN = zsel_n(n712, r_c39, n750);
    let n755: ZB = zb_and(n720, n751);
    let n756: ZB = zb_or(n712, n752);
    let n757: ZB = zb_or(n712, n753);
    let n758: ZB = zb_and(n733, n756);
    let n760: ZB = zb_and(n356, n443);
    let n761: ZB = zb_and(n356, n622);
    let n762: ZN = zsel_n(n760, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n763: ZB = zb_not(n760);
    let n764: ZB = zb_or(n760, n761);
    let n765: ZB = zsel_b(n760, n312, n534);
    let n766: ZN = zsel_n(n712, r_c39, n762);
    let n767: ZB = zb_and(n720, n763);
    let n768: ZB = zb_or(n712, n764);
    let n769: ZB = zb_or(n712, n765);
    let n770: ZB = zb_and(n733, n768);
    let n773: ZB = zb_and(n356, n472);
    let n774: ZB = zb_and(n356, n651);
    let n775: ZN = zsel_n(n773, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n776: ZB = zb_not(n773);
    let n777: ZB = zb_or(n773, n774);
    let n778: ZB = zsel_b(n773, n312, n534);
    let n779: ZN = zsel_n(n712, r_c39, n775);
    let n780: ZB = zb_and(n720, n776);
    let n781: ZB = zb_or(n712, n777);
    let n782: ZB = zb_or(n712, n778);
    let n783: ZB = zb_and(n733, n781);
    let n786: ZB = zb_and(n356, n501);
    let n787: ZB = zb_and(n356, n680);
    let n788: ZN = zsel_n(n786, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n789: ZB = zb_not(n786);
    let n790: ZB = zb_or(n786, n787);
    let n791: ZB = zsel_b(n786, n312, n534);
    let n792: ZN = zsel_n(n712, r_c39, n788);
    let n793: ZB = zb_and(n720, n789);
    let n794: ZB = zb_or(n712, n790);
    let n795: ZB = zb_or(n712, n791);
    let n796: ZB = zb_and(n733, n794);
    let n799: ZN = zsel_n(n712, r_c20, zn_splat(P8::from_raw(131072i32)));
    let n800: ZB = zn_gt(n799, zn_splat(P8::from_raw(0i32)));
    let n801: ZB = zn_le(n799, zn_splat(P8::from_raw(0i32)));
    let n802: ZB = zb_and(n734, n800);
    let n803: ZB = zb_and(n734, n801);
    let n804: ZB = zb_or(n802, n803);
    let n805: ZB = zb_and(n746, n800);
    let n806: ZB = zb_and(n746, n801);
    let n807: ZB = zb_or(n805, n806);
    let n808: ZB = zb_and(n758, n800);
    let n809: ZB = zb_and(n758, n801);
    let n810: ZB = zb_or(n808, n809);
    let n811: ZB = zb_and(n770, n800);
    let n812: ZB = zb_and(n770, n801);
    let n813: ZB = zb_or(n811, n812);
    let n814: ZB = zb_and(n783, n800);
    let n815: ZB = zb_and(n783, n801);
    let n816: ZB = zb_or(n814, n815);
    let n817: ZB = zb_and(n796, n800);
    let n818: ZB = zb_and(n796, n801);
    let n819: ZB = zb_or(n817, n818);
    let n820: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n821: ZB = zb_not(n733);
    let n822: ZB = zb_and(n729, n821);
    let n823: ZB = zb_or(n734, n822);
    let n824: ZN = zsel_n(n362, r_c39, n719);
    let n825: ZN = zsel_n(n362, n820, r_c20);
    let n826: ZB = zb_and(n75, n721);
    let n827: ZN = zsel_n(n362, zn_splat(P8::from_raw(0i32)), n722);
    let n828: ZN = zsel_n(n362, r_c235, n723);
    let n829: ZN = zsel_n(n362, zn_splat(P8::from_raw(196608i32)), n724);
    let n830: ZN = zsel_n(n362, r_c247, n725);
    let n831: ZN = zsel_n(n362, r_c251, n726);
    let n832: ZN = zsel_n(n362, zn_splat(P8::from_raw(-262144i32)), n727);
    let n833: ZN = zsel_n(n362, zn_splat(P8::from_raw(6291456i32)), n728);
    let n834: ZB = zb_or(n362, n823);
    let n835: ZB = zb_or(n362, n730);
    let n836: ZB = zn_gt(n825, zn_splat(P8::from_raw(0i32)));
    let n837: ZB = zn_le(n825, zn_splat(P8::from_raw(0i32)));
    let n838: ZB = zb_and(n834, n836);
    let n839: ZB = zb_and(n834, n837);
    let n840: ZB = zb_or(n838, n839);
    let n842: ZB = zb_and(n744, n821);
    let n843: ZB = zb_or(n746, n842);
    let n844: ZN = zsel_n(n362, r_c39, n742);
    let n845: ZB = zb_and(n75, n743);
    let n846: ZB = zb_or(n362, n843);
    let n847: ZB = zb_or(n362, n745);
    let n848: ZB = zb_and(n836, n846);
    let n849: ZB = zb_and(n837, n846);
    let n850: ZB = zb_or(n848, n849);
    let n852: ZB = zb_and(n756, n821);
    let n853: ZB = zb_or(n758, n852);
    let n854: ZN = zsel_n(n362, r_c39, n754);
    let n855: ZB = zb_and(n75, n755);
    let n856: ZB = zb_or(n362, n853);
    let n857: ZB = zb_or(n362, n757);
    let n858: ZB = zb_and(n836, n856);
    let n859: ZB = zb_and(n837, n856);
    let n860: ZB = zb_or(n858, n859);
    let n862: ZB = zb_and(n768, n821);
    let n863: ZN = zsel_n(n362, r_c39, n766);
    let n864: ZB = zb_and(n75, n767);
    let n865: ZB = zb_or(n362, n862);
    let n866: ZB = zb_or(n362, n769);
    let n867: ZB = zb_and(n836, n865);
    let n868: ZB = zb_and(n837, n865);
    let n869: ZB = zb_or(n867, n868);
    let n871: ZB = zb_and(n781, n821);
    let n872: ZN = zsel_n(n362, r_c39, n779);
    let n873: ZB = zb_and(n75, n780);
    let n874: ZB = zb_or(n362, n871);
    let n875: ZB = zb_or(n362, n782);
    let n876: ZB = zb_and(n836, n874);
    let n877: ZB = zb_and(n837, n874);
    let n878: ZB = zb_or(n876, n877);
    let n880: ZB = zb_and(n794, n821);
    let n881: ZN = zsel_n(n362, r_c39, n792);
    let n882: ZB = zb_and(n75, n793);
    let n883: ZB = zb_or(n362, n880);
    let n884: ZB = zb_or(n362, n795);
    let n885: ZB = zb_and(n836, n883);
    let n886: ZB = zb_and(n837, n883);
    let n887: ZB = zb_or(n885, n886);
    let n889: ZN = zsel_n(n362, n820, n799);
    let n890: ZB = zb_or(n362, n822);
    let n891: ZB = zn_gt(n889, zn_splat(P8::from_raw(0i32)));
    let n892: ZB = zn_le(n889, zn_splat(P8::from_raw(0i32)));
    let n893: ZB = zb_and(n890, n891);
    let n894: ZB = zb_and(n890, n892);
    let n895: ZB = zb_or(n893, n894);
    let n896: ZB = zb_or(n362, n842);
    let n897: ZB = zb_and(n891, n896);
    let n898: ZB = zb_and(n892, n896);
    let n899: ZB = zb_or(n897, n898);
    let n900: ZB = zb_or(n362, n852);
    let n901: ZB = zb_and(n891, n900);
    let n902: ZB = zb_and(n892, n900);
    let n903: ZB = zb_or(n901, n902);
    let n904: ZB = zb_and(n865, n891);
    let n905: ZB = zb_and(n865, n892);
    let n906: ZB = zb_or(n904, n905);
    let n907: ZB = zb_and(n874, n891);
    let n908: ZB = zb_and(n874, n892);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_and(n883, n891);
    let n911: ZB = zb_and(n883, n892);
    let n912: ZB = zb_or(n910, n911);
    let n915: ZW = zw_bits_n(r_c39);
    let n916: ZW = zw_mix1(zw_splat(11400714819323198485u64), n915, 39u64);
    let n917: ZW = zw_mix2(zw_splat(11562461410679940143u64), n915, 39u64);
    let n918: ZW = zw_bits_n(n101);
    let n919: ZW = zw_mix1(n916, n918, 256u64);
    let n920: ZW = zw_mix2(n917, n918, 256u64);
    let n921: ZW = zw_bits_n(r_c20);
    let n922: ZW = zw_mix1(n919, n921, 20u64);
    let n923: ZW = zw_mix2(n920, n921, 20u64);
    let n924: u64 = false as u64;
    let n925: ZW = zw_mix1(n922, zw_splat(n924), 41u64);
    let n926: ZW = zw_mix2(n923, zw_splat(n924), 41u64);
    let n927: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n928: ZW = zw_mix1(n925, zw_splat(n927), 236u64);
    let n929: ZW = zw_mix2(n926, zw_splat(n927), 236u64);
    let n930: ZW = zw_mix1(n928, zw_splat(n927), 238u64);
    let n931: ZW = zw_mix2(n929, zw_splat(n927), 238u64);
    let n932: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n933: ZW = zw_mix1(n930, zw_splat(n932), 239u64);
    let n934: ZW = zw_mix2(n931, zw_splat(n932), 239u64);
    let n935: ZW = zw_bits_n(n326);
    let n936: ZW = zw_mix1(n933, n935, 241u64);
    let n937: ZW = zw_mix2(n934, n935, 241u64);
    let n938: ZW = zw_mix1(n936, zw_splat(n924), 248u64);
    let n939: ZW = zw_mix2(n937, zw_splat(n924), 248u64);
    let n940: ZW = zw_mix1(n938, zw_splat(n924), 249u64);
    let n941: ZW = zw_mix2(n939, zw_splat(n924), 249u64);
    let n942: ZW = zw_mix1(n940, zw_splat(n927), 270u64);
    let n943: ZW = zw_mix2(n941, zw_splat(n927), 270u64);
    let n944: ZW = zw_mix1(n942, zw_splat(n927), 271u64);
    let n945: ZW = zw_mix2(n943, zw_splat(n927), 271u64);
    let n946: ZW = zw_mix1(n944, zw_splat(n927), 272u64);
    let n947: ZW = zw_mix2(n945, zw_splat(n927), 272u64);
    let n948: ZW = zw_mix1(n946, zw_splat(n927), 273u64);
    let n949: ZW = zw_mix2(n947, zw_splat(n927), 273u64);
    let n950: ZW = zw_mix1(n948, zw_splat(n924), 274u64);
    let n951: ZW = zw_mix2(n949, zw_splat(n924), 274u64);
    let n952: ZW = zw_mix1(n950, zw_splat(n927), 282u64);
    let n953: ZW = zw_mix2(n951, zw_splat(n927), 282u64);
    let n954: ZW = zw_bits_n(n337);
    let n955: ZW = zw_mix1(n952, n954, 283u64);
    let n956: ZW = zw_mix2(n953, n954, 283u64);
    let n957: u64 = true as u64;
    let n958: ZW = zw_mix1(n948, zw_splat(n957), 274u64);
    let n959: ZW = zw_mix2(n949, zw_splat(n957), 274u64);
    let n960: ZW = zw_bits_n(n367);
    let n961: ZW = zw_mix1(n958, n960, 282u64);
    let n962: ZW = zw_mix2(n959, n960, 282u64);
    let n963: ZW = zw_mix1(n961, n954, 283u64);
    let n964: ZW = zw_mix2(n962, n954, 283u64);
    let n965: ZW = zw_bits_n(n392);
    let n966: ZW = zw_mix1(n950, n965, 282u64);
    let n967: ZW = zw_mix2(n951, n965, 282u64);
    let n968: ZW = zw_mix1(n966, n954, 283u64);
    let n969: ZW = zw_mix2(n967, n954, 283u64);
    let n970: ZW = zw_bits_n(n353);
    let n971: ZW = zw_mix1(n933, n970, 241u64);
    let n972: ZW = zw_mix2(n934, n970, 241u64);
    let n973: ZW = zw_mix1(n971, zw_splat(n924), 248u64);
    let n974: ZW = zw_mix2(n972, zw_splat(n924), 248u64);
    let n975: ZW = zw_mix1(n973, zw_splat(n957), 249u64);
    let n976: ZW = zw_mix2(n974, zw_splat(n957), 249u64);
    let n977: ZW = zw_mix1(n975, zw_splat(n927), 270u64);
    let n978: ZW = zw_mix2(n976, zw_splat(n927), 270u64);
    let n979: ZW = zw_mix1(n977, zw_splat(n927), 271u64);
    let n980: ZW = zw_mix2(n978, zw_splat(n927), 271u64);
    let n981: ZW = zw_mix1(n979, zw_splat(n927), 272u64);
    let n982: ZW = zw_mix2(n980, zw_splat(n927), 272u64);
    let n983: ZW = zw_mix1(n981, zw_splat(n927), 273u64);
    let n984: ZW = zw_mix2(n982, zw_splat(n927), 273u64);
    let n985: ZW = zw_mix1(n983, zw_splat(n924), 274u64);
    let n986: ZW = zw_mix2(n984, zw_splat(n924), 274u64);
    let n987: ZW = zw_bits_n(n354);
    let n988: ZW = zw_mix1(n985, n987, 282u64);
    let n989: ZW = zw_mix2(n986, n987, 282u64);
    let n990: ZW = zw_bits_n(n355);
    let n991: ZW = zw_mix1(n988, n990, 283u64);
    let n992: ZW = zw_mix2(n989, n990, 283u64);
    let n993: ZW = zw_mix1(n983, zw_splat(n957), 274u64);
    let n994: ZW = zw_mix2(n984, zw_splat(n957), 274u64);
    let n995: ZW = zw_bits_n(n386);
    let n996: ZW = zw_mix1(n993, n995, 282u64);
    let n997: ZW = zw_mix2(n994, n995, 282u64);
    let n998: ZW = zw_mix1(n996, n990, 283u64);
    let n999: ZW = zw_mix2(n997, n990, 283u64);
    let n1000: ZW = zw_bits_n(n411);
    let n1001: ZW = zw_mix1(n985, n1000, 282u64);
    let n1002: ZW = zw_mix2(n986, n1000, 282u64);
    let n1003: ZW = zw_mix1(n1001, n990, 283u64);
    let n1004: ZW = zw_mix2(n1002, n990, 283u64);
    let n1005: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n1006: ZW = zw_mix1(n919, zw_splat(n1005), 20u64);
    let n1007: ZW = zw_mix2(n920, zw_splat(n1005), 20u64);
    let n1008: ZW = zw_mix1(n1006, zw_splat(n957), 41u64);
    let n1009: ZW = zw_mix2(n1007, zw_splat(n957), 41u64);
    let n1010: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n1011: ZW = zw_mix1(n1008, zw_splat(n1010), 236u64);
    let n1012: ZW = zw_mix2(n1009, zw_splat(n1010), 236u64);
    let n1013: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n1014: ZW = zw_mix1(n1011, zw_splat(n1013), 238u64);
    let n1015: ZW = zw_mix2(n1012, zw_splat(n1013), 238u64);
    let n1016: ZW = zw_mix1(n1014, zw_splat(n927), 239u64);
    let n1017: ZW = zw_mix2(n1015, zw_splat(n927), 239u64);
    let n1018: ZW = zw_mix1(n1016, n935, 241u64);
    let n1019: ZW = zw_mix2(n1017, n935, 241u64);
    let n1020: ZW = zw_mix1(n1018, zw_splat(n957), 248u64);
    let n1021: ZW = zw_mix2(n1019, zw_splat(n957), 248u64);
    let n1022: ZW = zw_mix1(n1020, zw_splat(n924), 249u64);
    let n1023: ZW = zw_mix2(n1021, zw_splat(n924), 249u64);
    let n1024: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n1025: ZW = zw_mix1(n1022, zw_splat(n1024), 270u64);
    let n1026: ZW = zw_mix2(n1023, zw_splat(n1024), 270u64);
    let n1027: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n1028: ZW = zw_mix1(n1025, zw_splat(n1027), 271u64);
    let n1029: ZW = zw_mix2(n1026, zw_splat(n1027), 271u64);
    let n1030: ZW = zw_mix1(n1028, zw_splat(n1005), 272u64);
    let n1031: ZW = zw_mix2(n1029, zw_splat(n1005), 272u64);
    let n1032: ZW = zw_mix1(n1030, zw_splat(n927), 273u64);
    let n1033: ZW = zw_mix2(n1031, zw_splat(n927), 273u64);
    let n1034: ZW = zw_mix1(n1032, zw_splat(n924), 274u64);
    let n1035: ZW = zw_mix2(n1033, zw_splat(n924), 274u64);
    let n1036: ZW = zw_mix1(n1034, zw_splat(n932), 282u64);
    let n1037: ZW = zw_mix2(n1035, zw_splat(n932), 282u64);
    let n1038: ZW = zw_mix1(n1036, zw_splat(n927), 283u64);
    let n1039: ZW = zw_mix2(n1037, zw_splat(n927), 283u64);
    let n1040: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n1041: ZW = zw_mix1(n1028, zw_splat(n1040), 272u64);
    let n1042: ZW = zw_mix2(n1029, zw_splat(n1040), 272u64);
    let n1043: ZW = zw_mix1(n1041, zw_splat(n927), 273u64);
    let n1044: ZW = zw_mix2(n1042, zw_splat(n927), 273u64);
    let n1045: ZW = zw_mix1(n1043, zw_splat(n957), 274u64);
    let n1046: ZW = zw_mix2(n1044, zw_splat(n957), 274u64);
    let n1047: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n1048: ZW = zw_mix1(n1045, zw_splat(n1047), 282u64);
    let n1049: ZW = zw_mix2(n1046, zw_splat(n1047), 282u64);
    let n1050: ZW = zw_mix1(n1048, zw_splat(n927), 283u64);
    let n1051: ZW = zw_mix2(n1049, zw_splat(n927), 283u64);
    let n1052: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n1053: ZW = zw_mix1(n1034, zw_splat(n1052), 282u64);
    let n1054: ZW = zw_mix2(n1035, zw_splat(n1052), 282u64);
    let n1055: ZW = zw_mix1(n1053, zw_splat(n927), 283u64);
    let n1056: ZW = zw_mix2(n1054, zw_splat(n927), 283u64);
    let n1057: ZW = zw_mix1(n1022, zw_splat(n1027), 270u64);
    let n1058: ZW = zw_mix2(n1023, zw_splat(n1027), 270u64);
    let n1059: ZW = zw_mix1(n1057, zw_splat(n1024), 271u64);
    let n1060: ZW = zw_mix2(n1058, zw_splat(n1024), 271u64);
    let n1061: ZW = zw_mix1(n1059, zw_splat(n927), 272u64);
    let n1062: ZW = zw_mix2(n1060, zw_splat(n927), 272u64);
    let n1063: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n1064: ZW = zw_mix1(n1061, zw_splat(n1063), 273u64);
    let n1065: ZW = zw_mix2(n1062, zw_splat(n1063), 273u64);
    let n1066: ZW = zw_mix1(n1064, zw_splat(n924), 274u64);
    let n1067: ZW = zw_mix2(n1065, zw_splat(n924), 274u64);
    let n1068: ZW = zw_mix1(n1066, zw_splat(n927), 282u64);
    let n1069: ZW = zw_mix2(n1067, zw_splat(n927), 282u64);
    let n1070: ZW = zw_mix1(n1068, zw_splat(n1047), 283u64);
    let n1071: ZW = zw_mix2(n1069, zw_splat(n1047), 283u64);
    let n1072: ZW = zw_mix1(n1057, zw_splat(n1027), 271u64);
    let n1073: ZW = zw_mix2(n1058, zw_splat(n1027), 271u64);
    let n1074: ZW = zw_mix1(n1072, zw_splat(n1040), 272u64);
    let n1075: ZW = zw_mix2(n1073, zw_splat(n1040), 272u64);
    let n1076: ZW = zw_mix1(n1074, zw_splat(n1063), 273u64);
    let n1077: ZW = zw_mix2(n1075, zw_splat(n1063), 273u64);
    let n1078: ZW = zw_mix1(n1076, zw_splat(n957), 274u64);
    let n1079: ZW = zw_mix2(n1077, zw_splat(n957), 274u64);
    let n1080: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1081: ZW = zw_mix1(n1078, zw_splat(n1080), 282u64);
    let n1082: ZW = zw_mix2(n1079, zw_splat(n1080), 282u64);
    let n1083: ZW = zw_mix1(n1081, zw_splat(n1080), 283u64);
    let n1084: ZW = zw_mix2(n1082, zw_splat(n1080), 283u64);
    let n1085: ZW = zw_mix1(n1072, zw_splat(n1005), 272u64);
    let n1086: ZW = zw_mix2(n1073, zw_splat(n1005), 272u64);
    let n1087: ZW = zw_mix1(n1085, zw_splat(n1063), 273u64);
    let n1088: ZW = zw_mix2(n1086, zw_splat(n1063), 273u64);
    let n1089: ZW = zw_mix1(n1087, zw_splat(n924), 274u64);
    let n1090: ZW = zw_mix2(n1088, zw_splat(n924), 274u64);
    let n1091: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1092: ZW = zw_mix1(n1089, zw_splat(n1091), 282u64);
    let n1093: ZW = zw_mix2(n1090, zw_splat(n1091), 282u64);
    let n1094: ZW = zw_mix1(n1092, zw_splat(n1080), 283u64);
    let n1095: ZW = zw_mix2(n1093, zw_splat(n1080), 283u64);
    let n1096: ZW = zw_mix1(n1061, zw_splat(n1005), 273u64);
    let n1097: ZW = zw_mix2(n1062, zw_splat(n1005), 273u64);
    let n1098: ZW = zw_mix1(n1096, zw_splat(n924), 274u64);
    let n1099: ZW = zw_mix2(n1097, zw_splat(n924), 274u64);
    let n1100: ZW = zw_mix1(n1098, zw_splat(n927), 282u64);
    let n1101: ZW = zw_mix2(n1099, zw_splat(n927), 282u64);
    let n1102: ZW = zw_mix1(n1100, zw_splat(n1052), 283u64);
    let n1103: ZW = zw_mix2(n1101, zw_splat(n1052), 283u64);
    let n1104: ZW = zw_mix1(n1074, zw_splat(n1005), 273u64);
    let n1105: ZW = zw_mix2(n1075, zw_splat(n1005), 273u64);
    let n1106: ZW = zw_mix1(n1104, zw_splat(n957), 274u64);
    let n1107: ZW = zw_mix2(n1105, zw_splat(n957), 274u64);
    let n1108: ZW = zw_mix1(n1106, zw_splat(n1080), 282u64);
    let n1109: ZW = zw_mix2(n1107, zw_splat(n1080), 282u64);
    let n1110: ZW = zw_mix1(n1108, zw_splat(n1091), 283u64);
    let n1111: ZW = zw_mix2(n1109, zw_splat(n1091), 283u64);
    let n1112: ZW = zw_mix1(n1085, zw_splat(n1005), 273u64);
    let n1113: ZW = zw_mix2(n1086, zw_splat(n1005), 273u64);
    let n1114: ZW = zw_mix1(n1112, zw_splat(n924), 274u64);
    let n1115: ZW = zw_mix2(n1113, zw_splat(n924), 274u64);
    let n1116: ZW = zw_mix1(n1114, zw_splat(n1091), 282u64);
    let n1117: ZW = zw_mix2(n1115, zw_splat(n1091), 282u64);
    let n1118: ZW = zw_mix1(n1116, zw_splat(n1091), 283u64);
    let n1119: ZW = zw_mix2(n1117, zw_splat(n1091), 283u64);
    let n1120: ZW = zw_mix1(n1016, n970, 241u64);
    let n1121: ZW = zw_mix2(n1017, n970, 241u64);
    let n1122: ZW = zw_mix1(n1120, zw_splat(n957), 248u64);
    let n1123: ZW = zw_mix2(n1121, zw_splat(n957), 248u64);
    let n1124: ZW = zw_mix1(n1122, zw_splat(n957), 249u64);
    let n1125: ZW = zw_mix2(n1123, zw_splat(n957), 249u64);
    let n1126: ZW = zw_mix1(n1124, zw_splat(n1024), 270u64);
    let n1127: ZW = zw_mix2(n1125, zw_splat(n1024), 270u64);
    let n1128: ZW = zw_mix1(n1126, zw_splat(n1027), 271u64);
    let n1129: ZW = zw_mix2(n1127, zw_splat(n1027), 271u64);
    let n1130: ZW = zw_mix1(n1128, zw_splat(n1005), 272u64);
    let n1131: ZW = zw_mix2(n1129, zw_splat(n1005), 272u64);
    let n1132: ZW = zw_mix1(n1130, zw_splat(n927), 273u64);
    let n1133: ZW = zw_mix2(n1131, zw_splat(n927), 273u64);
    let n1134: ZW = zw_mix1(n1132, zw_splat(n924), 274u64);
    let n1135: ZW = zw_mix2(n1133, zw_splat(n924), 274u64);
    let n1136: ZW = zw_mix1(n1134, zw_splat(n932), 282u64);
    let n1137: ZW = zw_mix2(n1135, zw_splat(n932), 282u64);
    let n1138: ZW = zw_mix1(n1136, zw_splat(n927), 283u64);
    let n1139: ZW = zw_mix2(n1137, zw_splat(n927), 283u64);
    let n1140: ZW = zw_mix1(n1128, zw_splat(n1040), 272u64);
    let n1141: ZW = zw_mix2(n1129, zw_splat(n1040), 272u64);
    let n1142: ZW = zw_mix1(n1140, zw_splat(n927), 273u64);
    let n1143: ZW = zw_mix2(n1141, zw_splat(n927), 273u64);
    let n1144: ZW = zw_mix1(n1142, zw_splat(n957), 274u64);
    let n1145: ZW = zw_mix2(n1143, zw_splat(n957), 274u64);
    let n1146: ZW = zw_mix1(n1144, zw_splat(n1047), 282u64);
    let n1147: ZW = zw_mix2(n1145, zw_splat(n1047), 282u64);
    let n1148: ZW = zw_mix1(n1146, zw_splat(n927), 283u64);
    let n1149: ZW = zw_mix2(n1147, zw_splat(n927), 283u64);
    let n1150: ZW = zw_mix1(n1134, zw_splat(n1052), 282u64);
    let n1151: ZW = zw_mix2(n1135, zw_splat(n1052), 282u64);
    let n1152: ZW = zw_mix1(n1150, zw_splat(n927), 283u64);
    let n1153: ZW = zw_mix2(n1151, zw_splat(n927), 283u64);
    let n1154: ZW = zw_mix1(n1124, zw_splat(n1027), 270u64);
    let n1155: ZW = zw_mix2(n1125, zw_splat(n1027), 270u64);
    let n1156: ZW = zw_mix1(n1154, zw_splat(n1024), 271u64);
    let n1157: ZW = zw_mix2(n1155, zw_splat(n1024), 271u64);
    let n1158: ZW = zw_mix1(n1156, zw_splat(n927), 272u64);
    let n1159: ZW = zw_mix2(n1157, zw_splat(n927), 272u64);
    let n1160: ZW = zw_mix1(n1158, zw_splat(n1063), 273u64);
    let n1161: ZW = zw_mix2(n1159, zw_splat(n1063), 273u64);
    let n1162: ZW = zw_mix1(n1160, zw_splat(n924), 274u64);
    let n1163: ZW = zw_mix2(n1161, zw_splat(n924), 274u64);
    let n1164: ZW = zw_mix1(n1162, zw_splat(n927), 282u64);
    let n1165: ZW = zw_mix2(n1163, zw_splat(n927), 282u64);
    let n1166: ZW = zw_mix1(n1164, zw_splat(n1047), 283u64);
    let n1167: ZW = zw_mix2(n1165, zw_splat(n1047), 283u64);
    let n1168: ZW = zw_mix1(n1154, zw_splat(n1027), 271u64);
    let n1169: ZW = zw_mix2(n1155, zw_splat(n1027), 271u64);
    let n1170: ZW = zw_mix1(n1168, zw_splat(n1040), 272u64);
    let n1171: ZW = zw_mix2(n1169, zw_splat(n1040), 272u64);
    let n1172: ZW = zw_mix1(n1170, zw_splat(n1063), 273u64);
    let n1173: ZW = zw_mix2(n1171, zw_splat(n1063), 273u64);
    let n1174: ZW = zw_mix1(n1172, zw_splat(n957), 274u64);
    let n1175: ZW = zw_mix2(n1173, zw_splat(n957), 274u64);
    let n1176: ZW = zw_mix1(n1174, zw_splat(n1080), 282u64);
    let n1177: ZW = zw_mix2(n1175, zw_splat(n1080), 282u64);
    let n1178: ZW = zw_mix1(n1176, zw_splat(n1080), 283u64);
    let n1179: ZW = zw_mix2(n1177, zw_splat(n1080), 283u64);
    let n1180: ZW = zw_mix1(n1168, zw_splat(n1005), 272u64);
    let n1181: ZW = zw_mix2(n1169, zw_splat(n1005), 272u64);
    let n1182: ZW = zw_mix1(n1180, zw_splat(n1063), 273u64);
    let n1183: ZW = zw_mix2(n1181, zw_splat(n1063), 273u64);
    let n1184: ZW = zw_mix1(n1182, zw_splat(n924), 274u64);
    let n1185: ZW = zw_mix2(n1183, zw_splat(n924), 274u64);
    let n1186: ZW = zw_mix1(n1184, zw_splat(n1091), 282u64);
    let n1187: ZW = zw_mix2(n1185, zw_splat(n1091), 282u64);
    let n1188: ZW = zw_mix1(n1186, zw_splat(n1080), 283u64);
    let n1189: ZW = zw_mix2(n1187, zw_splat(n1080), 283u64);
    let n1190: ZW = zw_mix1(n1158, zw_splat(n1005), 273u64);
    let n1191: ZW = zw_mix2(n1159, zw_splat(n1005), 273u64);
    let n1192: ZW = zw_mix1(n1190, zw_splat(n924), 274u64);
    let n1193: ZW = zw_mix2(n1191, zw_splat(n924), 274u64);
    let n1194: ZW = zw_mix1(n1192, zw_splat(n927), 282u64);
    let n1195: ZW = zw_mix2(n1193, zw_splat(n927), 282u64);
    let n1196: ZW = zw_mix1(n1194, zw_splat(n1052), 283u64);
    let n1197: ZW = zw_mix2(n1195, zw_splat(n1052), 283u64);
    let n1198: ZW = zw_mix1(n1170, zw_splat(n1005), 273u64);
    let n1199: ZW = zw_mix2(n1171, zw_splat(n1005), 273u64);
    let n1200: ZW = zw_mix1(n1198, zw_splat(n957), 274u64);
    let n1201: ZW = zw_mix2(n1199, zw_splat(n957), 274u64);
    let n1202: ZW = zw_mix1(n1200, zw_splat(n1080), 282u64);
    let n1203: ZW = zw_mix2(n1201, zw_splat(n1080), 282u64);
    let n1204: ZW = zw_mix1(n1202, zw_splat(n1091), 283u64);
    let n1205: ZW = zw_mix2(n1203, zw_splat(n1091), 283u64);
    let n1206: ZW = zw_mix1(n1180, zw_splat(n1005), 273u64);
    let n1207: ZW = zw_mix2(n1181, zw_splat(n1005), 273u64);
    let n1208: ZW = zw_mix1(n1206, zw_splat(n924), 274u64);
    let n1209: ZW = zw_mix2(n1207, zw_splat(n924), 274u64);
    let n1210: ZW = zw_mix1(n1208, zw_splat(n1091), 282u64);
    let n1211: ZW = zw_mix2(n1209, zw_splat(n1091), 282u64);
    let n1212: ZW = zw_mix1(n1210, zw_splat(n1091), 283u64);
    let n1213: ZW = zw_mix2(n1211, zw_splat(n1091), 283u64);
    let n1214: ZW = zw_mix1(zw_splat(11400714819323198485u64), n921, 20u64);
    let n1215: ZW = zw_mix2(zw_splat(11562461410679940143u64), n921, 20u64);
    let n1216: ZW = zw_mix1(n1214, zw_splat(n924), 41u64);
    let n1217: ZW = zw_mix2(n1215, zw_splat(n924), 41u64);
    let n1218: u64 = mix64(11400714819323198485u64 ^ mix64(n1005 ^ 20u64));
    let n1219: u64 = 11562461410679940143u64.wrapping_add(mix64(n1005.wrapping_mul((20u64 << 1) | 1)));
    let n1220: u64 = mix64(n1218 ^ mix64(n957 ^ 41u64));
    let n1221: u64 = n1219.wrapping_add(mix64(n957.wrapping_mul((41u64 << 1) | 1)));
    let n1222: ZW = zw_bits_n(n722);
    let n1223: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1222, 161u64);
    let n1224: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1222, 161u64);
    let n1225: ZW = zw_bits_n(n723);
    let n1226: ZW = zw_mix1(n1223, n1225, 237u64);
    let n1227: ZW = zw_mix2(n1224, n1225, 237u64);
    let n1228: ZW = zw_bits_n(n724);
    let n1229: ZW = zw_mix1(n1226, n1228, 248u64);
    let n1230: ZW = zw_mix2(n1227, n1228, 248u64);
    let n1231: ZW = zw_bits_n(n725);
    let n1232: ZW = zw_mix1(n1229, n1231, 249u64);
    let n1233: ZW = zw_mix2(n1230, n1231, 249u64);
    let n1234: ZW = zw_bits_n(n726);
    let n1235: ZW = zw_mix1(n1232, n1234, 253u64);
    let n1236: ZW = zw_mix2(n1233, n1234, 253u64);
    let n1237: ZW = zw_bits_n(n727);
    let n1238: ZW = zw_mix1(n1235, n1237, 275u64);
    let n1239: ZW = zw_mix2(n1236, n1237, 275u64);
    let n1240: ZW = zw_bits_n(n728);
    let n1241: ZW = zw_mix1(n1238, n1240, 277u64);
    let n1242: ZW = zw_mix2(n1239, n1240, 277u64);
    let n1243: ZW = zw_mix1(n1241, n921, 20u64);
    let n1244: ZW = zw_mix2(n1242, n921, 20u64);
    let n1245: ZW = zw_bits_b(n721);
    let n1246: ZW = zw_mix1(n1243, n1245, 38u64);
    let n1247: ZW = zw_mix2(n1244, n1245, 38u64);
    let n1248: ZW = zw_bits_n(n719);
    let n1249: ZW = zw_mix1(n1246, n1248, 39u64);
    let n1250: ZW = zw_mix2(n1247, n1248, 39u64);
    let n1251: ZW = zw_bits_b(n743);
    let n1252: ZW = zw_mix1(n1243, n1251, 38u64);
    let n1253: ZW = zw_mix2(n1244, n1251, 38u64);
    let n1254: ZW = zw_bits_n(n742);
    let n1255: ZW = zw_mix1(n1252, n1254, 39u64);
    let n1256: ZW = zw_mix2(n1253, n1254, 39u64);
    let n1257: ZW = zw_bits_b(n755);
    let n1258: ZW = zw_mix1(n1243, n1257, 38u64);
    let n1259: ZW = zw_mix2(n1244, n1257, 38u64);
    let n1260: ZW = zw_bits_n(n754);
    let n1261: ZW = zw_mix1(n1258, n1260, 39u64);
    let n1262: ZW = zw_mix2(n1259, n1260, 39u64);
    let n1263: ZW = zw_bits_b(n767);
    let n1264: ZW = zw_mix1(n1243, n1263, 38u64);
    let n1265: ZW = zw_mix2(n1244, n1263, 38u64);
    let n1266: ZW = zw_bits_n(n766);
    let n1267: ZW = zw_mix1(n1264, n1266, 39u64);
    let n1268: ZW = zw_mix2(n1265, n1266, 39u64);
    let n1269: ZW = zw_bits_b(n780);
    let n1270: ZW = zw_mix1(n1243, n1269, 38u64);
    let n1271: ZW = zw_mix2(n1244, n1269, 38u64);
    let n1272: ZW = zw_bits_n(n779);
    let n1273: ZW = zw_mix1(n1270, n1272, 39u64);
    let n1274: ZW = zw_mix2(n1271, n1272, 39u64);
    let n1275: ZW = zw_bits_b(n793);
    let n1276: ZW = zw_mix1(n1243, n1275, 38u64);
    let n1277: ZW = zw_mix2(n1244, n1275, 38u64);
    let n1278: ZW = zw_bits_n(n792);
    let n1279: ZW = zw_mix1(n1276, n1278, 39u64);
    let n1280: ZW = zw_mix2(n1277, n1278, 39u64);
    let n1281: ZW = zw_bits_n(n799);
    let n1282: ZW = zw_mix1(n1241, n1281, 20u64);
    let n1283: ZW = zw_mix2(n1242, n1281, 20u64);
    let n1284: ZW = zw_mix1(n1282, n1245, 38u64);
    let n1285: ZW = zw_mix2(n1283, n1245, 38u64);
    let n1286: ZW = zw_mix1(n1284, n1248, 39u64);
    let n1287: ZW = zw_mix2(n1285, n1248, 39u64);
    let n1288: ZW = zw_mix1(n1282, n1251, 38u64);
    let n1289: ZW = zw_mix2(n1283, n1251, 38u64);
    let n1290: ZW = zw_mix1(n1288, n1254, 39u64);
    let n1291: ZW = zw_mix2(n1289, n1254, 39u64);
    let n1292: ZW = zw_mix1(n1282, n1257, 38u64);
    let n1293: ZW = zw_mix2(n1283, n1257, 38u64);
    let n1294: ZW = zw_mix1(n1292, n1260, 39u64);
    let n1295: ZW = zw_mix2(n1293, n1260, 39u64);
    let n1296: ZW = zw_mix1(n1282, n1263, 38u64);
    let n1297: ZW = zw_mix2(n1283, n1263, 38u64);
    let n1298: ZW = zw_mix1(n1296, n1266, 39u64);
    let n1299: ZW = zw_mix2(n1297, n1266, 39u64);
    let n1300: ZW = zw_mix1(n1282, n1269, 38u64);
    let n1301: ZW = zw_mix2(n1283, n1269, 38u64);
    let n1302: ZW = zw_mix1(n1300, n1272, 39u64);
    let n1303: ZW = zw_mix2(n1301, n1272, 39u64);
    let n1304: ZW = zw_mix1(n1282, n1275, 38u64);
    let n1305: ZW = zw_mix2(n1283, n1275, 38u64);
    let n1306: ZW = zw_mix1(n1304, n1278, 39u64);
    let n1307: ZW = zw_mix2(n1305, n1278, 39u64);
    let n1308: ZW = zw_bits_n(n827);
    let n1309: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1308, 159u64);
    let n1310: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1308, 159u64);
    let n1311: ZW = zw_bits_n(n828);
    let n1312: ZW = zw_mix1(n1309, n1311, 235u64);
    let n1313: ZW = zw_mix2(n1310, n1311, 235u64);
    let n1314: ZW = zw_bits_n(n829);
    let n1315: ZW = zw_mix1(n1312, n1314, 246u64);
    let n1316: ZW = zw_mix2(n1313, n1314, 246u64);
    let n1317: ZW = zw_bits_n(n830);
    let n1318: ZW = zw_mix1(n1315, n1317, 247u64);
    let n1319: ZW = zw_mix2(n1316, n1317, 247u64);
    let n1320: ZW = zw_bits_n(n831);
    let n1321: ZW = zw_mix1(n1318, n1320, 251u64);
    let n1322: ZW = zw_mix2(n1319, n1320, 251u64);
    let n1323: ZW = zw_bits_n(n832);
    let n1324: ZW = zw_mix1(n1321, n1323, 273u64);
    let n1325: ZW = zw_mix2(n1322, n1323, 273u64);
    let n1326: ZW = zw_bits_n(n833);
    let n1327: ZW = zw_mix1(n1324, n1326, 275u64);
    let n1328: ZW = zw_mix2(n1325, n1326, 275u64);
    let n1329: ZW = zw_bits_n(n825);
    let n1330: ZW = zw_mix1(n1327, n1329, 20u64);
    let n1331: ZW = zw_mix2(n1328, n1329, 20u64);
    let n1332: ZW = zw_bits_b(n826);
    let n1333: ZW = zw_mix1(n1330, n1332, 38u64);
    let n1334: ZW = zw_mix2(n1331, n1332, 38u64);
    let n1335: ZW = zw_bits_n(n824);
    let n1336: ZW = zw_mix1(n1333, n1335, 39u64);
    let n1337: ZW = zw_mix2(n1334, n1335, 39u64);
    let n1338: ZW = zw_bits_b(n845);
    let n1339: ZW = zw_mix1(n1330, n1338, 38u64);
    let n1340: ZW = zw_mix2(n1331, n1338, 38u64);
    let n1341: ZW = zw_bits_n(n844);
    let n1342: ZW = zw_mix1(n1339, n1341, 39u64);
    let n1343: ZW = zw_mix2(n1340, n1341, 39u64);
    let n1344: ZW = zw_bits_b(n855);
    let n1345: ZW = zw_mix1(n1330, n1344, 38u64);
    let n1346: ZW = zw_mix2(n1331, n1344, 38u64);
    let n1347: ZW = zw_bits_n(n854);
    let n1348: ZW = zw_mix1(n1345, n1347, 39u64);
    let n1349: ZW = zw_mix2(n1346, n1347, 39u64);
    let n1350: ZW = zw_bits_b(n864);
    let n1351: ZW = zw_mix1(n1330, n1350, 38u64);
    let n1352: ZW = zw_mix2(n1331, n1350, 38u64);
    let n1353: ZW = zw_bits_n(n863);
    let n1354: ZW = zw_mix1(n1351, n1353, 39u64);
    let n1355: ZW = zw_mix2(n1352, n1353, 39u64);
    let n1356: ZW = zw_bits_b(n873);
    let n1357: ZW = zw_mix1(n1330, n1356, 38u64);
    let n1358: ZW = zw_mix2(n1331, n1356, 38u64);
    let n1359: ZW = zw_bits_n(n872);
    let n1360: ZW = zw_mix1(n1357, n1359, 39u64);
    let n1361: ZW = zw_mix2(n1358, n1359, 39u64);
    let n1362: ZW = zw_bits_b(n882);
    let n1363: ZW = zw_mix1(n1330, n1362, 38u64);
    let n1364: ZW = zw_mix2(n1331, n1362, 38u64);
    let n1365: ZW = zw_bits_n(n881);
    let n1366: ZW = zw_mix1(n1363, n1365, 39u64);
    let n1367: ZW = zw_mix2(n1364, n1365, 39u64);
    let n1368: ZW = zw_bits_n(n889);
    let n1369: ZW = zw_mix1(n1327, n1368, 20u64);
    let n1370: ZW = zw_mix2(n1328, n1368, 20u64);
    let n1371: ZW = zw_mix1(n1369, n1332, 38u64);
    let n1372: ZW = zw_mix2(n1370, n1332, 38u64);
    let n1373: ZW = zw_mix1(n1371, n1335, 39u64);
    let n1374: ZW = zw_mix2(n1372, n1335, 39u64);
    let n1375: ZW = zw_mix1(n1369, n1338, 38u64);
    let n1376: ZW = zw_mix2(n1370, n1338, 38u64);
    let n1377: ZW = zw_mix1(n1375, n1341, 39u64);
    let n1378: ZW = zw_mix2(n1376, n1341, 39u64);
    let n1379: ZW = zw_mix1(n1369, n1344, 38u64);
    let n1380: ZW = zw_mix2(n1370, n1344, 38u64);
    let n1381: ZW = zw_mix1(n1379, n1347, 39u64);
    let n1382: ZW = zw_mix2(n1380, n1347, 39u64);
    let n1383: ZW = zw_mix1(n1369, n1350, 38u64);
    let n1384: ZW = zw_mix2(n1370, n1350, 38u64);
    let n1385: ZW = zw_mix1(n1383, n1353, 39u64);
    let n1386: ZW = zw_mix2(n1384, n1353, 39u64);
    let n1387: ZW = zw_mix1(n1369, n1356, 38u64);
    let n1388: ZW = zw_mix2(n1370, n1356, 38u64);
    let n1389: ZW = zw_mix1(n1387, n1359, 39u64);
    let n1390: ZW = zw_mix2(n1388, n1359, 39u64);
    let n1391: ZW = zw_mix1(n1369, n1362, 38u64);
    let n1392: ZW = zw_mix2(n1370, n1362, 38u64);
    let n1393: ZW = zw_mix1(n1391, n1365, 39u64);
    let n1394: ZW = zw_mix2(n1392, n1365, 39u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v0_b0: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b0: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n360);
    let ok_v1_b1: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v1_b1: bool = !n60 || !n59 || !n92 || !n91;
    let live_v1_b1: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n389);
    let ok_v2_b2: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v2_b2: bool = !n60 || !n59 || !n92 || !n91;
    let live_v2_b2: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n414);
    let ok_v16_b3: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v16_b3: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b3: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n443);
    let ok_v17_b4: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v17_b4: bool = !n60 || !n59 || !n92 || !n91;
    let live_v17_b4: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n472);
    let ok_v18_b5: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v18_b5: bool = !n60 || !n59 || !n92 || !n91;
    let live_v18_b5: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n501);
    let ok_v32_b6: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v32_b6: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b6: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v33_b7: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v33_b7: bool = !n60 || !n59 || !n92 || !n91;
    let live_v33_b7: u16 = ALL & zb_holds(n357) & zb_holds(n389);
    let ok_v34_b8: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v34_b8: bool = !n60 || !n59 || !n92 || !n91;
    let live_v34_b8: u16 = ALL & zb_holds(n357) & zb_holds(n414);
    let ok_v36_b9: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v36_b9: bool = !n60 || !n59 || !n92 || !n91;
    let live_v36_b9: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v37_b10: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v37_b10: bool = !n60 || !n59 || !n92 || !n91;
    let live_v37_b10: u16 = ALL & zb_holds(n357) & zb_holds(n389);
    let ok_v38_b11: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v38_b11: bool = !n60 || !n59 || !n92 || !n91;
    let live_v38_b11: u16 = ALL & zb_holds(n357) & zb_holds(n414);
    let ok_v40_b12: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v40_b12: bool = !n60 || !n59 || !n92 || !n91;
    let live_v40_b12: u16 = ALL & zb_holds(n357) & zb_holds(n360);
    let ok_v41_b13: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v41_b13: bool = !n60 || !n59 || !n92 || !n91;
    let live_v41_b13: u16 = ALL & zb_holds(n357) & zb_holds(n389);
    let ok_v42_b14: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v42_b14: bool = !n60 || !n59 || !n92 || !n91;
    let live_v42_b14: u16 = ALL & zb_holds(n357) & zb_holds(n414);
    let ok_v48_b15: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v48_b15: bool = !n60 || !n59 || !n92 || !n91;
    let live_v48_b15: u16 = ALL & zb_holds(n357) & zb_holds(n443);
    let ok_v49_b16: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v49_b16: bool = !n60 || !n59 || !n92 || !n91;
    let live_v49_b16: u16 = ALL & zb_holds(n357) & zb_holds(n472);
    let ok_v50_b17: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v50_b17: bool = !n60 || !n59 || !n92 || !n91;
    let live_v50_b17: u16 = ALL & zb_holds(n357) & zb_holds(n501);
    let ok_v52_b18: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v52_b18: bool = !n60 || !n59 || !n92 || !n91;
    let live_v52_b18: u16 = ALL & zb_holds(n357) & zb_holds(n443);
    let ok_v53_b19: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v53_b19: bool = !n60 || !n59 || !n92 || !n91;
    let live_v53_b19: u16 = ALL & zb_holds(n357) & zb_holds(n472);
    let ok_v54_b20: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v54_b20: bool = !n60 || !n59 || !n92 || !n91;
    let live_v54_b20: u16 = ALL & zb_holds(n357) & zb_holds(n501);
    let ok_v56_b21: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v56_b21: bool = !n60 || !n59 || !n92 || !n91;
    let live_v56_b21: u16 = ALL & zb_holds(n357) & zb_holds(n443);
    let ok_v57_b22: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v57_b22: bool = !n60 || !n59 || !n92 || !n91;
    let live_v57_b22: u16 = ALL & zb_holds(n357) & zb_holds(n472);
    let ok_v58_b23: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n312);
    let bd_v58_b23: bool = !n60 || !n59 || !n92 || !n91;
    let live_v58_b23: u16 = ALL & zb_holds(n357) & zb_holds(n501);
    let ok_v0_b24: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v0_b24: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b24: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n552);
    let ok_v1_b25: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v1_b25: bool = !n60 || !n59 || !n92 || !n91;
    let live_v1_b25: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n573);
    let ok_v2_b26: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v2_b26: bool = !n60 || !n59 || !n92 || !n91;
    let live_v2_b26: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n593);
    let ok_v16_b27: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v16_b27: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b27: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n622);
    let ok_v17_b28: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v17_b28: bool = !n60 || !n59 || !n92 || !n91;
    let live_v17_b28: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n651);
    let ok_v18_b29: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v18_b29: bool = !n60 || !n59 || !n92 || !n91;
    let live_v18_b29: u16 = ALL & zb_holds(n75) & zb_holds(n357) & zb_holds(n680);
    let ok_v32_b30: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v32_b30: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b30: u16 = ALL & zb_holds(n357) & zb_holds(n552);
    let ok_v33_b31: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v33_b31: bool = !n60 || !n59 || !n92 || !n91;
    let live_v33_b31: u16 = ALL & zb_holds(n357) & zb_holds(n573);
    let ok_v34_b32: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v34_b32: bool = !n60 || !n59 || !n92 || !n91;
    let live_v34_b32: u16 = ALL & zb_holds(n357) & zb_holds(n593);
    let ok_v48_b33: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v48_b33: bool = !n60 || !n59 || !n92 || !n91;
    let live_v48_b33: u16 = ALL & zb_holds(n357) & zb_holds(n622);
    let ok_v49_b34: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v49_b34: bool = !n60 || !n59 || !n92 || !n91;
    let live_v49_b34: u16 = ALL & zb_holds(n357) & zb_holds(n651);
    let ok_v50_b35: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n534);
    let bd_v50_b35: bool = !n60 || !n59 || !n92 || !n91;
    let live_v50_b35: u16 = ALL & zb_holds(n357) & zb_holds(n680);
    let ok_v0_b36: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n730);
    let bd_v0_b36: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b36: u16 = if true { 0 } else { ALL };
    let ok_v1_b37: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n745);
    let bd_v1_b37: bool = !n60 || !n59 || !n92 || !n91;
    let live_v1_b37: u16 = if true { 0 } else { ALL };
    let ok_v2_b38: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n757);
    let bd_v2_b38: bool = !n60 || !n59 || !n92 || !n91;
    let live_v2_b38: u16 = if true { 0 } else { ALL };
    let ok_v16_b39: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n769);
    let bd_v16_b39: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b39: u16 = ALL & zb_holds(n75) & zb_holds(n733) & zb_holds(n768);
    let ok_v17_b40: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n782);
    let bd_v17_b40: bool = !n60 || !n59 || !n92 || !n91;
    let live_v17_b40: u16 = ALL & zb_holds(n75) & zb_holds(n733) & zb_holds(n781);
    let ok_v18_b41: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n795);
    let bd_v18_b41: bool = !n60 || !n59 || !n92 || !n91;
    let live_v18_b41: u16 = ALL & zb_holds(n75) & zb_holds(n733) & zb_holds(n794);
    let ok_v32_b42: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n730);
    let bd_v32_b42: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b42: u16 = ALL & zb_holds(n804);
    let ok_v33_b43: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n745);
    let bd_v33_b43: bool = !n60 || !n59 || !n92 || !n91;
    let live_v33_b43: u16 = ALL & zb_holds(n807);
    let ok_v34_b44: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n757);
    let bd_v34_b44: bool = !n60 || !n59 || !n92 || !n91;
    let live_v34_b44: u16 = ALL & zb_holds(n810);
    let ok_v48_b45: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n769);
    let bd_v48_b45: bool = !n60 || !n59 || !n92 || !n91;
    let live_v48_b45: u16 = ALL & zb_holds(n813);
    let ok_v49_b46: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n782);
    let bd_v49_b46: bool = !n60 || !n59 || !n92 || !n91;
    let live_v49_b46: u16 = ALL & zb_holds(n816);
    let ok_v50_b47: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n795);
    let bd_v50_b47: bool = !n60 || !n59 || !n92 || !n91;
    let live_v50_b47: u16 = ALL & zb_holds(n819);
    let ok_v0_b48: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n835);
    let bd_v0_b48: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b48: u16 = ALL & zb_holds(n840);
    let ok_v1_b49: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n847);
    let bd_v1_b49: bool = !n60 || !n59 || !n92 || !n91;
    let live_v1_b49: u16 = ALL & zb_holds(n850);
    let ok_v2_b50: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n857);
    let bd_v2_b50: bool = !n60 || !n59 || !n92 || !n91;
    let live_v2_b50: u16 = ALL & zb_holds(n860);
    let ok_v16_b51: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n866);
    let bd_v16_b51: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b51: u16 = ALL & zb_holds(n869);
    let ok_v17_b52: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n875);
    let bd_v17_b52: bool = !n60 || !n59 || !n92 || !n91;
    let live_v17_b52: u16 = ALL & zb_holds(n878);
    let ok_v18_b53: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n884);
    let bd_v18_b53: bool = !n60 || !n59 || !n92 || !n91;
    let live_v18_b53: u16 = ALL & zb_holds(n887);
    let ok_v32_b54: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n835);
    let bd_v32_b54: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b54: u16 = ALL & zb_holds(n895);
    let ok_v33_b55: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n847);
    let bd_v33_b55: bool = !n60 || !n59 || !n92 || !n91;
    let live_v33_b55: u16 = ALL & zb_holds(n899);
    let ok_v34_b56: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n857);
    let bd_v34_b56: bool = !n60 || !n59 || !n92 || !n91;
    let live_v34_b56: u16 = ALL & zb_holds(n903);
    let ok_v48_b57: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n866);
    let bd_v48_b57: bool = !n60 || !n59 || !n92 || !n91;
    let live_v48_b57: u16 = ALL & zb_holds(n906);
    let ok_v49_b58: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n875);
    let bd_v49_b58: bool = !n60 || !n59 || !n92 || !n91;
    let live_v49_b58: u16 = ALL & zb_holds(n909);
    let ok_v50_b59: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n884);
    let bd_v50_b59: bool = !n60 || !n59 || !n92 || !n91;
    let live_v50_b59: u16 = ALL & zb_holds(n912);
    let sh0 = KShared0 {
        c39: r_c39,
        c256: n101,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
        c237: n723,
        c275: n727,
        c248: n724,
        c249: n725,
        c277: n728,
        c253: n726,
        c161: n722,
    };
    let sh3 = KShared3 {
        c235: n828,
        c273: n832,
        c246: n829,
        c247: n830,
        c275: n833,
        c251: n831,
        c159: n827,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n326,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: n337,
        h1: n955, h2: n956,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(true),
        c241: n326,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n367,
        c283: n337,
        h1: n963, h2: n964,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n326,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n392,
        c283: n337,
        h1: n968, h2: n969,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n353,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n354,
        c283: n355,
        h1: n991, h2: n992,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(true),
        c241: n353,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n386,
        c283: n355,
        h1: n998, h2: n999,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n353,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n411,
        c283: n355,
        h1: n1003, h2: n1004,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1038, h2: n1039,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1050, h2: n1051,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1055, h2: n1056,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n1070, h2: n1071,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1083, h2: n1084,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1094, h2: n1095,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n1102, h2: n1103,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1110, h2: n1111,
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
        c241: n326,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1118, h2: n1119,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1138, h2: n1139,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1148, h2: n1149,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n1152, h2: n1153,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n1166, h2: n1167,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1178, h2: n1179,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n1188, h2: n1189,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n1196, h2: n1197,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1204, h2: n1205,
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
        c241: n353,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n1212, h2: n1213,
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
        h1: n1216, h2: n1217,
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
        h1: zw_splat(n1220), h2: zw_splat(n1221),
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n719,
        c20: r_c20,
        c38: n721,
        h1: n1249, h2: n1250,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n742,
        c20: r_c20,
        c38: n743,
        h1: n1255, h2: n1256,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n754,
        c20: r_c20,
        c38: n755,
        h1: n1261, h2: n1262,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n766,
        c20: r_c20,
        c38: n767,
        h1: n1267, h2: n1268,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n779,
        c20: r_c20,
        c38: n780,
        h1: n1273, h2: n1274,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n792,
        c20: r_c20,
        c38: n793,
        h1: n1279, h2: n1280,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n719,
        c20: n799,
        c38: n721,
        h1: n1286, h2: n1287,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n742,
        c20: n799,
        c38: n743,
        h1: n1290, h2: n1291,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n754,
        c20: n799,
        c38: n755,
        h1: n1294, h2: n1295,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n766,
        c20: n799,
        c38: n767,
        h1: n1298, h2: n1299,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n779,
        c20: n799,
        c38: n780,
        h1: n1302, h2: n1303,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n792,
        c20: n799,
        c38: n793,
        h1: n1306, h2: n1307,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n824,
        c20: n825,
        c38: n826,
        h1: n1336, h2: n1337,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b49 & (if bd_v1_b49 { ALL } else { !ok_v1_b49 });
    take_3_1 |= live_v1_b49 & ok_v1_b49 & (if bd_v1_b49 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n844,
        c20: n825,
        c38: n845,
        h1: n1342, h2: n1343,
    };
    // body 49: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b50 & (if bd_v2_b50 { ALL } else { !ok_v2_b50 });
    take_3_2 |= live_v2_b50 & ok_v2_b50 & (if bd_v2_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n854,
        c20: n825,
        c38: n855,
        h1: n1348, h2: n1349,
    };
    // body 50: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b51 & (if bd_v16_b51 { ALL } else { !ok_v16_b51 });
    take_3_3 |= live_v16_b51 & ok_v16_b51 & (if bd_v16_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n863,
        c20: n825,
        c38: n864,
        h1: n1354, h2: n1355,
    };
    // body 51: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b52 & (if bd_v17_b52 { ALL } else { !ok_v17_b52 });
    take_3_4 |= live_v17_b52 & ok_v17_b52 & (if bd_v17_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n872,
        c20: n825,
        c38: n873,
        h1: n1360, h2: n1361,
    };
    // body 52: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b53 & (if bd_v18_b53 { ALL } else { !ok_v18_b53 });
    take_3_5 |= live_v18_b53 & ok_v18_b53 & (if bd_v18_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n881,
        c20: n825,
        c38: n882,
        h1: n1366, h2: n1367,
    };
    // body 53: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b54 & (if bd_v32_b54 { ALL } else { !ok_v32_b54 });
    take_3_6 |= live_v32_b54 & ok_v32_b54 & (if bd_v32_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n824,
        c20: n889,
        c38: n826,
        h1: n1373, h2: n1374,
    };
    // body 54: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b55 & (if bd_v33_b55 { ALL } else { !ok_v33_b55 });
    take_3_7 |= live_v33_b55 & ok_v33_b55 & (if bd_v33_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n844,
        c20: n889,
        c38: n845,
        h1: n1377, h2: n1378,
    };
    // body 55: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b56 & (if bd_v34_b56 { ALL } else { !ok_v34_b56 });
    take_3_8 |= live_v34_b56 & ok_v34_b56 & (if bd_v34_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n854,
        c20: n889,
        c38: n855,
        h1: n1381, h2: n1382,
    };
    // body 56: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v48_b57 & (if bd_v48_b57 { ALL } else { !ok_v48_b57 });
    take_3_9 |= live_v48_b57 & ok_v48_b57 & (if bd_v48_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n863,
        c20: n889,
        c38: n864,
        h1: n1385, h2: n1386,
    };
    // body 57: buttons 0x30, forks 0x0
    sink.o3(48, take_3_9, &sh3, &o3);
    declined |= live_v49_b58 & (if bd_v49_b58 { ALL } else { !ok_v49_b58 });
    take_3_10 |= live_v49_b58 & ok_v49_b58 & (if bd_v49_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n872,
        c20: n889,
        c38: n873,
        h1: n1389, h2: n1390,
    };
    // body 58: buttons 0x31, forks 0x0
    sink.o3(49, take_3_10, &sh3, &o3);
    declined |= live_v50_b59 & (if bd_v50_b59 { ALL } else { !ok_v50_b59 });
    take_3_11 |= live_v50_b59 & ok_v50_b59 & (if bd_v50_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c39: n881,
        c20: n889,
        c38: n882,
        h1: n1393, h2: n1394,
    };
    // body 59: buttons 0x32, forks 0x0
    sink.o3(50, take_3_11, &sh3, &o3);
    declined
}
