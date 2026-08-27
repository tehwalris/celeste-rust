// GENERATED from a TRACED frame (shape 2). Do not edit.
//
// One input shape, 4 output shapes, 32 distinct button
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
    pub c87: ZN,
    pub c39: ZN,
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
    pub c87: ZN,
    pub c39: ZN,
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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_0: u64 = 17308370761569747974;
pub const KPART2_0: u64 = 11997737073559572730;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_1: u64 = 16914069593006527232;
pub const KPART2_1: u64 = 11398472687916843512;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 18212845208833864313;
pub const KPART2_2: u64 = 8472442021916746622;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 17450879701650017427;
pub const KPART2_3: u64 = 4594267955108647308;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
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
    let n149: ZN = zn_rem(n138, zn_splat(P8::from_raw(524288i32)));
    let n150: ZB = zn_ge(n149, zn_splat(P8::from_raw(393216i32)));
    let n151: ZN = zn_mul(n141, zn_splat(P8::from_raw(524288i32)));
    let n152: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n151);
    let n153: ZB = zn_eq(n137, n152);
    let n154: ZB = zb_or(n150, n153);
    let n155: ZB = zb_and(n148, n154);
    let n156: ZB = zb_not(n155);
    let n157: ZB = zb_and(n144, n155);
    let n158: ZB = zb_and(n144, n156);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n147);
    let n160: ZN = zn_rem(n133, zn_splat(P8::from_raw(524288i32)));
    let n161: ZB = zn_le(n160, zn_splat(P8::from_raw(131072i32)));
    let n162: ZB = zb_and(n159, n161);
    let n163: ZB = zb_not(n162);
    let n164: ZB = zb_and(n158, n162);
    let n165: ZB = zb_and(n158, n163);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n147);
    let n167: ZB = zb_not(n166);
    let n168: ZB = zb_and(n165, n166);
    let n169: ZB = zb_and(n165, n167);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n147);
    let n171: ZB = zb_not(n170);
    let n172: ZB = zb_and(n169, n170);
    let n173: ZB = zb_and(n169, n171);
    let n174: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n136);
    let n175: ZB = zn_le(n174, n140);
    let n176: ZB = zn_gt(n174, n140);
    let n177: ZB = zb_and(n173, n175);
    let n178: ZB = zb_and(n173, n176);
    let n179: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n174);
    let n180: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n179);
    let n181: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n180);
    let n182: ZN = zn_mul(n174, zn_splat(P8::from_raw(524288i32)));
    let n183: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n182);
    let n184: ZB = zn_eq(n137, n183);
    let n185: ZB = zb_or(n150, n184);
    let n186: ZB = zb_and(n181, n185);
    let n187: ZB = zb_not(n186);
    let n188: ZB = zb_and(n177, n186);
    let n189: ZB = zb_and(n177, n187);
    let n190: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n180);
    let n191: ZB = zb_and(n161, n190);
    let n192: ZB = zb_not(n191);
    let n193: ZB = zb_and(n189, n191);
    let n194: ZB = zb_and(n189, n192);
    let n195: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n180);
    let n196: ZB = zb_not(n195);
    let n197: ZB = zb_and(n194, n195);
    let n198: ZB = zb_and(n194, n196);
    let n199: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n180);
    let n200: ZB = zb_not(n199);
    let n201: ZB = zb_and(n198, n199);
    let n202: ZB = zb_and(n198, n200);
    let n203: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n136);
    let n204: ZB = zn_le(n203, n140);
    let n205: ZB = zn_gt(n203, n140);
    let n206: ZB = zb_and(n202, n204);
    let n207: ZB = zb_and(n202, n205);
    let n208: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n203);
    let n209: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n208);
    let n210: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n209);
    let n211: ZN = zn_mul(n203, zn_splat(P8::from_raw(524288i32)));
    let n212: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n211);
    let n213: ZB = zn_eq(n137, n212);
    let n214: ZB = zb_or(n150, n213);
    let n215: ZB = zb_and(n210, n214);
    let n216: ZB = zb_not(n215);
    let n217: ZB = zb_and(n206, n215);
    let n218: ZB = zb_and(n206, n216);
    let n219: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n209);
    let n220: ZB = zb_and(n161, n219);
    let n221: ZB = zb_not(n220);
    let n222: ZB = zb_and(n218, n220);
    let n223: ZB = zb_and(n218, n221);
    let n224: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n209);
    let n225: ZB = zb_not(n224);
    let n226: ZB = zb_and(n223, n224);
    let n227: ZB = zb_and(n223, n225);
    let n228: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n209);
    let n229: ZB = zb_not(n228);
    let n230: ZB = zb_and(n227, n228);
    let n231: ZB = zb_and(n227, n229);
    let n232: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n136);
    let n233: ZB = zn_gt(n232, n140);
    let n234: ZB = zb_or(n207, n231);
    let n235: ZB = zb_or(n205, n233);
    let n236: ZB = zb_or(n178, n234);
    let n237: ZB = zb_or(n176, n235);
    let n238: ZB = zb_or(n145, n236);
    let n239: ZB = zb_or(n143, n237);
    let n240: ZB = zn_le(n105, zn_splat(P8::from_raw(8388608i32)));
    let n241: ZB = zb_and(n238, n240);
    let n242: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n133);
    let n243: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n242, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n244: ZB = zb_not(n243);
    let n245: ZN = zsel_n(n243, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n246: ZN = zsel_n(n244, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n247: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n246);
    let n248: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n246);
    let n249: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n133);
    let n250: ZN = zsel_n(n244, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n251: ZB = zn_gt(n245, zn_splat(P8::from_raw(0i32)));
    let n252: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n249, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n253: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n249, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n254: ZN = zsel_n(n253, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n255: ZN = zsel_n(n252, zn_splat(P8::from_raw(-65536i32)), n254);
    let n256: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n255);
    let n257: ZB = zb_not(n256);
    let n258: ZN = zn_neg(n255);
    let n259: ZN = zn_mul(n258, zn_splat(P8::from_raw(131072i32)));
    let n260: ZN = zsel_n(n257, n259, zn_splat(P8::from_raw(0i32)));
    let n261: ZN = zsel_n(n257, zn_splat(P8::from_raw(-131072i32)), n250);
    let n262: ZN = zsel_n(n251, zn_splat(P8::from_raw(0i32)), n245);
    let n263: ZN = zsel_n(n251, zn_splat(P8::from_raw(0i32)), n260);
    let n264: ZN = zsel_n(n251, zn_splat(P8::from_raw(-131072i32)), n261);
    let n265: ZB = zn_lt(n105, zn_splat(P8::from_raw(-262144i32)));
    let n266: ZB = zn_ge(n105, zn_splat(P8::from_raw(-262144i32)));
    let n267: ZB = zb_and(n241, n265);
    let n269: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n273: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n247);
    let n274: ZN = zsel_n(n257, n259, n273);
    let n275: ZN = zsel_n(n251, n273, n274);
    let n276: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n248);
    let n277: ZN = zsel_n(n257, n259, n276);
    let n278: ZN = zsel_n(n251, n276, n277);
    let n280: ZB = zb_or(n168, n172);
    let n281: ZB = zb_or(n164, n280);
    let n282: ZB = zb_or(n157, n281);
    let n283: ZB = zb_or(n197, n201);
    let n284: ZB = zb_or(n193, n283);
    let n285: ZB = zb_or(n188, n284);
    let n286: ZB = zb_or(n226, n230);
    let n287: ZB = zb_or(n222, n286);
    let n288: ZB = zb_or(n217, n287);
    let n289: ZB = zb_or(n285, n288);
    let n290: ZB = zb_or(n282, n289);
    let n291: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n292: ZB = zn_gt(n105, zn_splat(P8::from_raw(8388608i32)));
    let n293: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n291);
    let n294: ZN = zsel_n(n292, n293, n291);
    let n295: ZB = zb_and(n238, n292);
    let n296: ZN = zsel_n(n290, n294, n291);
    let n297: ZB = zb_or(n290, n295);
    let n298: ZB = zb_or(n239, n290);
    let n299: ZB = zb_and(n265, n297);
    let n307: ZB = zn_lt(n105, zn_splat(P8::from_raw(7340032i32)));
    let n308: ZB = zb_and(n76, n93);
    let n309: ZN = zsel_n(n307, zn_splat(P8::from_raw(196608i32)), r_c235);
    let n310: ZN = zsel_n(n307, zn_splat(P8::from_raw(65536i32)), r_c247);
    let n311: ZB = zb_and(n124, n125);
    let n312: ZB = zb_not(n128);
    let n313: ZB = zb_and(n127, n312);
    let n314: ZB = zn_ge(n130, zn_splat(P8::from_raw(0i32)));
    let n315: ZB = zb_and(n129, n314);
    let n316: ZN = zsel_n(n128, n130, r_c235);
    let n317: ZN = zsel_n(n128, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(196608i32)));
    let n318: ZB = zb_or(n313, n315);
    let n319: ZN = zsel_n(n125, r_c235, n316);
    let n320: ZN = zsel_n(n125, zn_splat(P8::from_raw(196608i32)), n317);
    let n321: ZN = zsel_n(n125, zn_splat(P8::from_raw(-229376i32)), zn_splat(P8::from_raw(-262144i32)));
    let n322: ZB = zb_or(n311, n318);
    let n323: ZN = zsel_n(n93, n309, n319);
    let n324: ZN = zsel_n(n93, zn_splat(P8::from_raw(196608i32)), n320);
    let n325: ZN = zsel_n(n93, n310, r_c247);
    let n326: ZN = zsel_n(n93, zn_splat(P8::from_raw(-262144i32)), n321);
    let n327: ZB = zb_or(n308, n322);
    let n328: ZN = zsel_n(n267, r_c87, n296);
    let n329: ZN = zsel_n(n267, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n330: ZB = zb_not(n267);
    let n331: ZB = zb_or(n267, n299);
    let n332: ZB = zsel_b(n267, n239, n298);
    let n333: ZN = zsel_n(n327, r_c87, n328);
    let n334: ZN = zsel_n(n327, r_c39, n329);
    let n335: ZB = zb_not(n327);
    let n336: ZB = zb_and(n330, n335);
    let n337: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n338: ZN = zsel_n(n327, n323, zn_splat(P8::from_raw(196608i32)));
    let n339: ZN = zsel_n(n327, n324, zn_splat(P8::from_raw(196608i32)));
    let n340: ZN = zsel_n(n327, n325, zn_splat(P8::from_raw(65536i32)));
    let n341: ZN = zsel_n(n327, n105, zn_splat(P8::from_raw(8126464i32)));
    let n342: ZN = zsel_n(n327, n326, zn_splat(P8::from_raw(-262144i32)));
    let n343: ZN = zsel_n(n327, zn_splat(P8::from_raw(6291456i32)), zn_splat(P8::from_raw(7340032i32)));
    let n344: ZB = zb_or(n327, n331);
    let n345: ZB = zb_or(n327, n332);
    let n346: ZN = zn_rem(n337, zn_splat(P8::from_raw(524288i32)));
    let n347: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n346);
    let n348: ZB = zn_eq(zn_splat(P8::from_raw(2031616i32)), n347);
    let n351: ZN = zsel_n(n327, r_c20, zn_splat(P8::from_raw(131072i32)));
    let n352: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n353: ZB = zb_not(n348);
    let n354: ZB = zb_and(n344, n353);
    let n355: ZN = zsel_n(n269, r_c87, n333);
    let n356: ZN = zsel_n(n269, r_c39, n334);
    let n357: ZN = zsel_n(n269, n352, r_c20);
    let n358: ZB = zb_and(n76, n336);
    let n359: ZN = zsel_n(n269, zn_splat(P8::from_raw(0i32)), n337);
    let n360: ZN = zsel_n(n269, r_c235, n338);
    let n361: ZN = zsel_n(n269, zn_splat(P8::from_raw(196608i32)), n339);
    let n362: ZN = zsel_n(n269, r_c247, n340);
    let n363: ZN = zsel_n(n269, r_c251, n341);
    let n364: ZN = zsel_n(n269, zn_splat(P8::from_raw(-262144i32)), n342);
    let n365: ZN = zsel_n(n269, zn_splat(P8::from_raw(6291456i32)), n343);
    let n366: ZB = zb_or(n269, n344);
    let n367: ZB = zb_or(n269, n345);
    let n369: ZB = zb_or(n269, n354);
    let n370: ZN = zsel_n(n269, n352, n351);
    let n372: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n373: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n374: ZW = zw_add(zw_splat(0u64), n372);
    let n375: ZW = zw_add(zw_splat(0u64), n373);
    let n376: ZW = zw_cellmix_n(84u64, n63, 1542469173u64);
    let n377: ZW = zw_cellmix_n(84u64, n63, 668265263u64);
    let n378: ZW = zw_add(n374, n376);
    let n379: ZW = zw_add(n375, n377);
    let n380: ZW = zw_cellmix_n(85u64, n104, 1542469173u64);
    let n381: ZW = zw_cellmix_n(85u64, n104, 668265263u64);
    let n382: ZW = zw_add(n378, n380);
    let n383: ZW = zw_add(n379, n381);
    let n384: ZW = zw_cellmix_n(86u64, n103, 1542469173u64);
    let n385: ZW = zw_cellmix_n(86u64, n103, 668265263u64);
    let n386: ZW = zw_add(n382, n384);
    let n387: ZW = zw_add(n383, n385);
    let n388: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n389: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n390: ZW = zw_add(n386, n388);
    let n391: ZW = zw_add(n387, n389);
    let n392: ZW = zw_cellmix_n(256u64, n105, 1542469173u64);
    let n393: ZW = zw_cellmix_n(256u64, n105, 668265263u64);
    let n394: ZW = zw_add(n390, n392);
    let n395: ZW = zw_add(n391, n393);
    let n396: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n397: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n398: ZW = zw_add(n394, n396);
    let n399: ZW = zw_add(n395, n397);
    let n400: ZW = zw_cellmix_b(41u64, zb_splat(false), 1542469173u64);
    let n401: ZW = zw_cellmix_b(41u64, zb_splat(false), 668265263u64);
    let n402: ZW = zw_add(n398, n400);
    let n403: ZW = zw_add(n399, n401);
    let n404: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(-65536i32)), 1542469173u64);
    let n405: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(-65536i32)), 668265263u64);
    let n406: ZW = zw_add(n402, n404);
    let n407: ZW = zw_add(n403, n405);
    let n408: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n409: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n410: ZW = zw_add(n406, n408);
    let n411: ZW = zw_add(n407, n409);
    let n412: ZW = zw_cellmix_n(239u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n413: ZW = zw_cellmix_n(239u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n414: ZW = zw_add(n410, n412);
    let n415: ZW = zw_add(n411, n413);
    let n416: ZW = zw_cellmix_n(241u64, n245, 1542469173u64);
    let n417: ZW = zw_cellmix_n(241u64, n245, 668265263u64);
    let n418: ZW = zw_add(n414, n416);
    let n419: ZW = zw_add(n415, n417);
    let n420: ZW = zw_cellmix_b(248u64, zb_splat(false), 1542469173u64);
    let n421: ZW = zw_cellmix_b(248u64, zb_splat(false), 668265263u64);
    let n422: ZW = zw_add(n418, n420);
    let n423: ZW = zw_add(n419, n421);
    let n424: ZW = zw_cellmix_b(249u64, zb_splat(false), 1542469173u64);
    let n425: ZW = zw_cellmix_b(249u64, zb_splat(false), 668265263u64);
    let n426: ZW = zw_add(n422, n424);
    let n427: ZW = zw_add(n423, n425);
    let n428: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n429: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n430: ZW = zw_add(n426, n428);
    let n431: ZW = zw_add(n427, n429);
    let n432: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n433: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n434: ZW = zw_add(n430, n432);
    let n435: ZW = zw_add(n431, n433);
    let n436: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n437: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n438: ZW = zw_add(n434, n436);
    let n439: ZW = zw_add(n435, n437);
    let n440: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n441: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n442: ZW = zw_add(n438, n440);
    let n443: ZW = zw_add(n439, n441);
    let n444: ZW = zw_cellmix_b(274u64, zb_splat(false), 1542469173u64);
    let n445: ZW = zw_cellmix_b(274u64, zb_splat(false), 668265263u64);
    let n446: ZW = zw_add(n442, n444);
    let n447: ZW = zw_add(n443, n445);
    let n448: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n449: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n450: ZW = zw_add(n446, n448);
    let n451: ZW = zw_add(n447, n449);
    let n452: ZW = zw_cellmix_n(283u64, n250, 1542469173u64);
    let n453: ZW = zw_cellmix_n(283u64, n250, 668265263u64);
    let n454: ZW = zw_add(n450, n452);
    let n455: ZW = zw_add(n451, n453);
    let n456: ZW = zw_cellmix_b(274u64, zb_splat(true), 1542469173u64);
    let n457: ZW = zw_cellmix_b(274u64, zb_splat(true), 668265263u64);
    let n458: ZW = zw_add(n442, n456);
    let n459: ZW = zw_add(n443, n457);
    let n460: ZW = zw_cellmix_n(282u64, n273, 1542469173u64);
    let n461: ZW = zw_cellmix_n(282u64, n273, 668265263u64);
    let n462: ZW = zw_add(n458, n460);
    let n463: ZW = zw_add(n459, n461);
    let n464: ZW = zw_add(n462, n452);
    let n465: ZW = zw_add(n463, n453);
    let n466: ZW = zw_cellmix_n(282u64, n276, 1542469173u64);
    let n467: ZW = zw_cellmix_n(282u64, n276, 668265263u64);
    let n468: ZW = zw_add(n446, n466);
    let n469: ZW = zw_add(n447, n467);
    let n470: ZW = zw_add(n468, n452);
    let n471: ZW = zw_add(n469, n453);
    let n472: ZW = zw_cellmix_n(241u64, n262, 1542469173u64);
    let n473: ZW = zw_cellmix_n(241u64, n262, 668265263u64);
    let n474: ZW = zw_add(n414, n472);
    let n475: ZW = zw_add(n415, n473);
    let n476: ZW = zw_add(n474, n420);
    let n477: ZW = zw_add(n475, n421);
    let n478: ZW = zw_cellmix_b(249u64, zb_splat(true), 1542469173u64);
    let n479: ZW = zw_cellmix_b(249u64, zb_splat(true), 668265263u64);
    let n480: ZW = zw_add(n476, n478);
    let n481: ZW = zw_add(n477, n479);
    let n482: ZW = zw_add(n480, n428);
    let n483: ZW = zw_add(n481, n429);
    let n484: ZW = zw_add(n482, n432);
    let n485: ZW = zw_add(n483, n433);
    let n486: ZW = zw_add(n484, n436);
    let n487: ZW = zw_add(n485, n437);
    let n488: ZW = zw_add(n486, n440);
    let n489: ZW = zw_add(n487, n441);
    let n490: ZW = zw_add(n488, n444);
    let n491: ZW = zw_add(n489, n445);
    let n492: ZW = zw_cellmix_n(282u64, n263, 1542469173u64);
    let n493: ZW = zw_cellmix_n(282u64, n263, 668265263u64);
    let n494: ZW = zw_add(n490, n492);
    let n495: ZW = zw_add(n491, n493);
    let n496: ZW = zw_cellmix_n(283u64, n264, 1542469173u64);
    let n497: ZW = zw_cellmix_n(283u64, n264, 668265263u64);
    let n498: ZW = zw_add(n494, n496);
    let n499: ZW = zw_add(n495, n497);
    let n500: ZW = zw_add(n488, n456);
    let n501: ZW = zw_add(n489, n457);
    let n502: ZW = zw_cellmix_n(282u64, n275, 1542469173u64);
    let n503: ZW = zw_cellmix_n(282u64, n275, 668265263u64);
    let n504: ZW = zw_add(n500, n502);
    let n505: ZW = zw_add(n501, n503);
    let n506: ZW = zw_add(n504, n496);
    let n507: ZW = zw_add(n505, n497);
    let n508: ZW = zw_cellmix_n(282u64, n278, 1542469173u64);
    let n509: ZW = zw_cellmix_n(282u64, n278, 668265263u64);
    let n510: ZW = zw_add(n490, n508);
    let n511: ZW = zw_add(n491, n509);
    let n512: ZW = zw_add(n510, n496);
    let n513: ZW = zw_add(n511, n497);
    let n514: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n515: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n516: ZW = zw_add(n394, n514);
    let n517: ZW = zw_add(n395, n515);
    let n518: ZW = zw_cellmix_b(41u64, zb_splat(true), 1542469173u64);
    let n519: ZW = zw_cellmix_b(41u64, zb_splat(true), 668265263u64);
    let n520: ZW = zw_add(n516, n518);
    let n521: ZW = zw_add(n517, n519);
    let n522: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(655360i32)), 1542469173u64);
    let n523: ZW = zw_cellmix_n(236u64, zn_splat(P8::from_raw(655360i32)), 668265263u64);
    let n524: ZW = zw_add(n520, n522);
    let n525: ZW = zw_add(n521, n523);
    let n526: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(262144i32)), 1542469173u64);
    let n527: ZW = zw_cellmix_n(238u64, zn_splat(P8::from_raw(262144i32)), 668265263u64);
    let n528: ZW = zw_add(n524, n526);
    let n529: ZW = zw_add(n525, n527);
    let n530: ZW = zw_cellmix_n(239u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n531: ZW = zw_cellmix_n(239u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n532: ZW = zw_add(n528, n530);
    let n533: ZW = zw_add(n529, n531);
    let n534: ZW = zw_add(n532, n416);
    let n535: ZW = zw_add(n533, n417);
    let n536: ZW = zw_cellmix_b(248u64, zb_splat(true), 1542469173u64);
    let n537: ZW = zw_cellmix_b(248u64, zb_splat(true), 668265263u64);
    let n538: ZW = zw_add(n534, n536);
    let n539: ZW = zw_add(n535, n537);
    let n540: ZW = zw_add(n538, n424);
    let n541: ZW = zw_add(n539, n425);
    let n542: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n543: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n544: ZW = zw_add(n540, n542);
    let n545: ZW = zw_add(n541, n543);
    let n546: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n547: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n548: ZW = zw_add(n544, n546);
    let n549: ZW = zw_add(n545, n547);
    let n550: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n551: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n552: ZW = zw_add(n548, n550);
    let n553: ZW = zw_add(n549, n551);
    let n554: ZW = zw_add(n552, n440);
    let n555: ZW = zw_add(n553, n441);
    let n556: ZW = zw_add(n554, n444);
    let n557: ZW = zw_add(n555, n445);
    let n558: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n559: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n560: ZW = zw_add(n556, n558);
    let n561: ZW = zw_add(n557, n559);
    let n562: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n563: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n564: ZW = zw_add(n560, n562);
    let n565: ZW = zw_add(n561, n563);
    let n566: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(-131072i32)), 1542469173u64);
    let n567: ZW = zw_cellmix_n(272u64, zn_splat(P8::from_raw(-131072i32)), 668265263u64);
    let n568: ZW = zw_add(n548, n566);
    let n569: ZW = zw_add(n549, n567);
    let n570: ZW = zw_add(n568, n440);
    let n571: ZW = zw_add(n569, n441);
    let n572: ZW = zw_add(n570, n456);
    let n573: ZW = zw_add(n571, n457);
    let n574: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n575: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n576: ZW = zw_add(n572, n574);
    let n577: ZW = zw_add(n573, n575);
    let n578: ZW = zw_add(n576, n562);
    let n579: ZW = zw_add(n577, n563);
    let n580: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n581: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n582: ZW = zw_add(n556, n580);
    let n583: ZW = zw_add(n557, n581);
    let n584: ZW = zw_add(n582, n562);
    let n585: ZW = zw_add(n583, n563);
    let n586: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n587: ZW = zw_cellmix_n(270u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n588: ZW = zw_add(n540, n586);
    let n589: ZW = zw_add(n541, n587);
    let n590: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n591: ZW = zw_cellmix_n(271u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n592: ZW = zw_add(n588, n590);
    let n593: ZW = zw_add(n589, n591);
    let n594: ZW = zw_add(n592, n436);
    let n595: ZW = zw_add(n593, n437);
    let n596: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(-98304i32)), 1542469173u64);
    let n597: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(-98304i32)), 668265263u64);
    let n598: ZW = zw_add(n594, n596);
    let n599: ZW = zw_add(n595, n597);
    let n600: ZW = zw_add(n598, n444);
    let n601: ZW = zw_add(n599, n445);
    let n602: ZW = zw_add(n600, n448);
    let n603: ZW = zw_add(n601, n449);
    let n604: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n605: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n606: ZW = zw_add(n602, n604);
    let n607: ZW = zw_add(n603, n605);
    let n608: ZW = zw_add(n588, n546);
    let n609: ZW = zw_add(n589, n547);
    let n610: ZW = zw_add(n608, n566);
    let n611: ZW = zw_add(n609, n567);
    let n612: ZW = zw_add(n610, n596);
    let n613: ZW = zw_add(n611, n597);
    let n614: ZW = zw_add(n612, n456);
    let n615: ZW = zw_add(n613, n457);
    let n616: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n617: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n618: ZW = zw_add(n614, n616);
    let n619: ZW = zw_add(n615, n617);
    let n620: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n621: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n622: ZW = zw_add(n618, n620);
    let n623: ZW = zw_add(n619, n621);
    let n624: ZW = zw_add(n608, n550);
    let n625: ZW = zw_add(n609, n551);
    let n626: ZW = zw_add(n624, n596);
    let n627: ZW = zw_add(n625, n597);
    let n628: ZW = zw_add(n626, n444);
    let n629: ZW = zw_add(n627, n445);
    let n630: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n631: ZW = zw_cellmix_n(282u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n632: ZW = zw_add(n628, n630);
    let n633: ZW = zw_add(n629, n631);
    let n634: ZW = zw_add(n632, n620);
    let n635: ZW = zw_add(n633, n621);
    let n636: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n637: ZW = zw_cellmix_n(273u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n638: ZW = zw_add(n594, n636);
    let n639: ZW = zw_add(n595, n637);
    let n640: ZW = zw_add(n638, n444);
    let n641: ZW = zw_add(n639, n445);
    let n642: ZW = zw_add(n640, n448);
    let n643: ZW = zw_add(n641, n449);
    let n644: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n645: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n646: ZW = zw_add(n642, n644);
    let n647: ZW = zw_add(n643, n645);
    let n648: ZW = zw_add(n610, n636);
    let n649: ZW = zw_add(n611, n637);
    let n650: ZW = zw_add(n648, n456);
    let n651: ZW = zw_add(n649, n457);
    let n652: ZW = zw_add(n650, n616);
    let n653: ZW = zw_add(n651, n617);
    let n654: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n655: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n656: ZW = zw_add(n652, n654);
    let n657: ZW = zw_add(n653, n655);
    let n658: ZW = zw_add(n624, n636);
    let n659: ZW = zw_add(n625, n637);
    let n660: ZW = zw_add(n658, n444);
    let n661: ZW = zw_add(n659, n445);
    let n662: ZW = zw_add(n660, n630);
    let n663: ZW = zw_add(n661, n631);
    let n664: ZW = zw_add(n662, n654);
    let n665: ZW = zw_add(n663, n655);
    let n666: ZW = zw_add(n532, n472);
    let n667: ZW = zw_add(n533, n473);
    let n668: ZW = zw_add(n666, n536);
    let n669: ZW = zw_add(n667, n537);
    let n670: ZW = zw_add(n668, n478);
    let n671: ZW = zw_add(n669, n479);
    let n672: ZW = zw_add(n670, n542);
    let n673: ZW = zw_add(n671, n543);
    let n674: ZW = zw_add(n672, n546);
    let n675: ZW = zw_add(n673, n547);
    let n676: ZW = zw_add(n674, n550);
    let n677: ZW = zw_add(n675, n551);
    let n678: ZW = zw_add(n676, n440);
    let n679: ZW = zw_add(n677, n441);
    let n680: ZW = zw_add(n678, n444);
    let n681: ZW = zw_add(n679, n445);
    let n682: ZW = zw_add(n680, n558);
    let n683: ZW = zw_add(n681, n559);
    let n684: ZW = zw_add(n682, n562);
    let n685: ZW = zw_add(n683, n563);
    let n686: ZW = zw_add(n674, n566);
    let n687: ZW = zw_add(n675, n567);
    let n688: ZW = zw_add(n686, n440);
    let n689: ZW = zw_add(n687, n441);
    let n690: ZW = zw_add(n688, n456);
    let n691: ZW = zw_add(n689, n457);
    let n692: ZW = zw_add(n690, n574);
    let n693: ZW = zw_add(n691, n575);
    let n694: ZW = zw_add(n692, n562);
    let n695: ZW = zw_add(n693, n563);
    let n696: ZW = zw_add(n680, n580);
    let n697: ZW = zw_add(n681, n581);
    let n698: ZW = zw_add(n696, n562);
    let n699: ZW = zw_add(n697, n563);
    let n700: ZW = zw_add(n670, n586);
    let n701: ZW = zw_add(n671, n587);
    let n702: ZW = zw_add(n700, n590);
    let n703: ZW = zw_add(n701, n591);
    let n704: ZW = zw_add(n702, n436);
    let n705: ZW = zw_add(n703, n437);
    let n706: ZW = zw_add(n704, n596);
    let n707: ZW = zw_add(n705, n597);
    let n708: ZW = zw_add(n706, n444);
    let n709: ZW = zw_add(n707, n445);
    let n710: ZW = zw_add(n708, n448);
    let n711: ZW = zw_add(n709, n449);
    let n712: ZW = zw_add(n710, n604);
    let n713: ZW = zw_add(n711, n605);
    let n714: ZW = zw_add(n700, n546);
    let n715: ZW = zw_add(n701, n547);
    let n716: ZW = zw_add(n714, n566);
    let n717: ZW = zw_add(n715, n567);
    let n718: ZW = zw_add(n716, n596);
    let n719: ZW = zw_add(n717, n597);
    let n720: ZW = zw_add(n718, n456);
    let n721: ZW = zw_add(n719, n457);
    let n722: ZW = zw_add(n720, n616);
    let n723: ZW = zw_add(n721, n617);
    let n724: ZW = zw_add(n722, n620);
    let n725: ZW = zw_add(n723, n621);
    let n726: ZW = zw_add(n714, n550);
    let n727: ZW = zw_add(n715, n551);
    let n728: ZW = zw_add(n726, n596);
    let n729: ZW = zw_add(n727, n597);
    let n730: ZW = zw_add(n728, n444);
    let n731: ZW = zw_add(n729, n445);
    let n732: ZW = zw_add(n730, n630);
    let n733: ZW = zw_add(n731, n631);
    let n734: ZW = zw_add(n732, n620);
    let n735: ZW = zw_add(n733, n621);
    let n736: ZW = zw_add(n704, n636);
    let n737: ZW = zw_add(n705, n637);
    let n738: ZW = zw_add(n736, n444);
    let n739: ZW = zw_add(n737, n445);
    let n740: ZW = zw_add(n738, n448);
    let n741: ZW = zw_add(n739, n449);
    let n742: ZW = zw_add(n740, n644);
    let n743: ZW = zw_add(n741, n645);
    let n744: ZW = zw_add(n716, n636);
    let n745: ZW = zw_add(n717, n637);
    let n746: ZW = zw_add(n744, n456);
    let n747: ZW = zw_add(n745, n457);
    let n748: ZW = zw_add(n746, n616);
    let n749: ZW = zw_add(n747, n617);
    let n750: ZW = zw_add(n748, n654);
    let n751: ZW = zw_add(n749, n655);
    let n752: ZW = zw_add(n726, n636);
    let n753: ZW = zw_add(n727, n637);
    let n754: ZW = zw_add(n752, n444);
    let n755: ZW = zw_add(n753, n445);
    let n756: ZW = zw_add(n754, n630);
    let n757: ZW = zw_add(n755, n631);
    let n758: ZW = zw_add(n756, n654);
    let n759: ZW = zw_add(n757, n655);
    let n760: ZW = zw_add(zw_splat(0u64), n376);
    let n761: ZW = zw_add(zw_splat(0u64), n377);
    let n762: ZW = zw_add(n760, n380);
    let n763: ZW = zw_add(n761, n381);
    let n764: ZW = zw_add(n762, n384);
    let n765: ZW = zw_add(n763, n385);
    let n766: ZW = zw_cellmix_n(87u64, n296, 1542469173u64);
    let n767: ZW = zw_cellmix_n(87u64, n296, 668265263u64);
    let n768: ZW = zw_add(n764, n766);
    let n769: ZW = zw_add(n765, n767);
    let n770: ZW = zw_add(n768, n396);
    let n771: ZW = zw_add(n769, n397);
    let n772: ZW = zw_add(n770, n400);
    let n773: ZW = zw_add(n771, n401);
    let n774: ZW = zw_add(n768, n514);
    let n775: ZW = zw_add(n769, n515);
    let n776: ZW = zw_add(n774, n518);
    let n777: ZW = zw_add(n775, n519);
    let n778: ZW = zw_cellmix_b(38u64, n336, 1542469173u64);
    let n779: ZW = zw_cellmix_b(38u64, n336, 668265263u64);
    let n780: ZW = zw_add(zw_splat(0u64), n778);
    let n781: ZW = zw_add(zw_splat(0u64), n779);
    let n782: ZW = zw_cellmix_n(39u64, n334, 1542469173u64);
    let n783: ZW = zw_cellmix_n(39u64, n334, 668265263u64);
    let n784: ZW = zw_add(n780, n782);
    let n785: ZW = zw_add(n781, n783);
    let n786: ZW = zw_add(n784, n376);
    let n787: ZW = zw_add(n785, n377);
    let n788: ZW = zw_add(n786, n380);
    let n789: ZW = zw_add(n787, n381);
    let n790: ZW = zw_add(n788, n384);
    let n791: ZW = zw_add(n789, n385);
    let n792: ZW = zw_cellmix_n(87u64, n333, 1542469173u64);
    let n793: ZW = zw_cellmix_n(87u64, n333, 668265263u64);
    let n794: ZW = zw_add(n790, n792);
    let n795: ZW = zw_add(n791, n793);
    let n796: ZW = zw_cellmix_n(161u64, n337, 1542469173u64);
    let n797: ZW = zw_cellmix_n(161u64, n337, 668265263u64);
    let n798: ZW = zw_add(n794, n796);
    let n799: ZW = zw_add(n795, n797);
    let n800: ZW = zw_cellmix_n(237u64, n338, 1542469173u64);
    let n801: ZW = zw_cellmix_n(237u64, n338, 668265263u64);
    let n802: ZW = zw_add(n798, n800);
    let n803: ZW = zw_add(n799, n801);
    let n804: ZW = zw_cellmix_n(248u64, n339, 1542469173u64);
    let n805: ZW = zw_cellmix_n(248u64, n339, 668265263u64);
    let n806: ZW = zw_add(n802, n804);
    let n807: ZW = zw_add(n803, n805);
    let n808: ZW = zw_cellmix_n(249u64, n340, 1542469173u64);
    let n809: ZW = zw_cellmix_n(249u64, n340, 668265263u64);
    let n810: ZW = zw_add(n806, n808);
    let n811: ZW = zw_add(n807, n809);
    let n812: ZW = zw_cellmix_n(253u64, n341, 1542469173u64);
    let n813: ZW = zw_cellmix_n(253u64, n341, 668265263u64);
    let n814: ZW = zw_add(n810, n812);
    let n815: ZW = zw_add(n811, n813);
    let n816: ZW = zw_cellmix_n(275u64, n342, 1542469173u64);
    let n817: ZW = zw_cellmix_n(275u64, n342, 668265263u64);
    let n818: ZW = zw_add(n814, n816);
    let n819: ZW = zw_add(n815, n817);
    let n820: ZW = zw_cellmix_n(277u64, n343, 1542469173u64);
    let n821: ZW = zw_cellmix_n(277u64, n343, 668265263u64);
    let n822: ZW = zw_add(n818, n820);
    let n823: ZW = zw_add(n819, n821);
    let n824: ZW = zw_add(n822, n396);
    let n825: ZW = zw_add(n823, n397);
    let n826: ZW = zw_cellmix_n(20u64, n351, 1542469173u64);
    let n827: ZW = zw_cellmix_n(20u64, n351, 668265263u64);
    let n828: ZW = zw_add(n822, n826);
    let n829: ZW = zw_add(n823, n827);
    let n830: ZW = zw_cellmix_b(38u64, n358, 1542469173u64);
    let n831: ZW = zw_cellmix_b(38u64, n358, 668265263u64);
    let n832: ZW = zw_add(zw_splat(0u64), n830);
    let n833: ZW = zw_add(zw_splat(0u64), n831);
    let n834: ZW = zw_cellmix_n(39u64, n356, 1542469173u64);
    let n835: ZW = zw_cellmix_n(39u64, n356, 668265263u64);
    let n836: ZW = zw_add(n832, n834);
    let n837: ZW = zw_add(n833, n835);
    let n838: ZW = zw_add(n836, n376);
    let n839: ZW = zw_add(n837, n377);
    let n840: ZW = zw_add(n838, n380);
    let n841: ZW = zw_add(n839, n381);
    let n842: ZW = zw_add(n840, n384);
    let n843: ZW = zw_add(n841, n385);
    let n844: ZW = zw_cellmix_n(87u64, n355, 1542469173u64);
    let n845: ZW = zw_cellmix_n(87u64, n355, 668265263u64);
    let n846: ZW = zw_add(n842, n844);
    let n847: ZW = zw_add(n843, n845);
    let n848: ZW = zw_cellmix_n(159u64, n359, 1542469173u64);
    let n849: ZW = zw_cellmix_n(159u64, n359, 668265263u64);
    let n850: ZW = zw_add(n846, n848);
    let n851: ZW = zw_add(n847, n849);
    let n852: ZW = zw_cellmix_n(235u64, n360, 1542469173u64);
    let n853: ZW = zw_cellmix_n(235u64, n360, 668265263u64);
    let n854: ZW = zw_add(n850, n852);
    let n855: ZW = zw_add(n851, n853);
    let n856: ZW = zw_cellmix_n(246u64, n361, 1542469173u64);
    let n857: ZW = zw_cellmix_n(246u64, n361, 668265263u64);
    let n858: ZW = zw_add(n854, n856);
    let n859: ZW = zw_add(n855, n857);
    let n860: ZW = zw_cellmix_n(247u64, n362, 1542469173u64);
    let n861: ZW = zw_cellmix_n(247u64, n362, 668265263u64);
    let n862: ZW = zw_add(n858, n860);
    let n863: ZW = zw_add(n859, n861);
    let n864: ZW = zw_cellmix_n(251u64, n363, 1542469173u64);
    let n865: ZW = zw_cellmix_n(251u64, n363, 668265263u64);
    let n866: ZW = zw_add(n862, n864);
    let n867: ZW = zw_add(n863, n865);
    let n868: ZW = zw_cellmix_n(273u64, n364, 1542469173u64);
    let n869: ZW = zw_cellmix_n(273u64, n364, 668265263u64);
    let n870: ZW = zw_add(n866, n868);
    let n871: ZW = zw_add(n867, n869);
    let n872: ZW = zw_cellmix_n(275u64, n365, 1542469173u64);
    let n873: ZW = zw_cellmix_n(275u64, n365, 668265263u64);
    let n874: ZW = zw_add(n870, n872);
    let n875: ZW = zw_add(n871, n873);
    let n876: ZW = zw_cellmix_n(20u64, n357, 1542469173u64);
    let n877: ZW = zw_cellmix_n(20u64, n357, 668265263u64);
    let n878: ZW = zw_add(n874, n876);
    let n879: ZW = zw_add(n875, n877);
    let n880: ZW = zw_cellmix_n(20u64, n370, 1542469173u64);
    let n881: ZW = zw_cellmix_n(20u64, n370, 668265263u64);
    let n882: ZW = zw_add(n874, n880);
    let n883: ZW = zw_add(n875, n881);
    let ok_v0_b0: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v0_b0: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b0: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v1_b1: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v1_b1: bool = !n58 || !n57 || !n90 || !n89;
    let live_v1_b1: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v2_b2: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v2_b2: bool = !n58 || !n57 || !n90 || !n89;
    let live_v2_b2: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v16_b3: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v16_b3: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b3: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v17_b4: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v17_b4: bool = !n58 || !n57 || !n90 || !n89;
    let live_v17_b4: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v18_b5: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v18_b5: bool = !n58 || !n57 || !n90 || !n89;
    let live_v18_b5: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v32_b6: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v32_b6: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b6: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v33_b7: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v33_b7: bool = !n58 || !n57 || !n90 || !n89;
    let live_v33_b7: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v34_b8: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v34_b8: bool = !n58 || !n57 || !n90 || !n89;
    let live_v34_b8: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v36_b9: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v36_b9: bool = !n58 || !n57 || !n90 || !n89;
    let live_v36_b9: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v37_b10: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v37_b10: bool = !n58 || !n57 || !n90 || !n89;
    let live_v37_b10: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v38_b11: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v38_b11: bool = !n58 || !n57 || !n90 || !n89;
    let live_v38_b11: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v40_b12: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v40_b12: bool = !n58 || !n57 || !n90 || !n89;
    let live_v40_b12: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v41_b13: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v41_b13: bool = !n58 || !n57 || !n90 || !n89;
    let live_v41_b13: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v42_b14: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v42_b14: bool = !n58 || !n57 || !n90 || !n89;
    let live_v42_b14: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v48_b15: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v48_b15: bool = !n58 || !n57 || !n90 || !n89;
    let live_v48_b15: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v49_b16: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v49_b16: bool = !n58 || !n57 || !n90 || !n89;
    let live_v49_b16: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v50_b17: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v50_b17: bool = !n58 || !n57 || !n90 || !n89;
    let live_v50_b17: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v52_b18: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v52_b18: bool = !n58 || !n57 || !n90 || !n89;
    let live_v52_b18: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v53_b19: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v53_b19: bool = !n58 || !n57 || !n90 || !n89;
    let live_v53_b19: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v54_b20: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v54_b20: bool = !n58 || !n57 || !n90 || !n89;
    let live_v54_b20: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v56_b21: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v56_b21: bool = !n58 || !n57 || !n90 || !n89;
    let live_v56_b21: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v57_b22: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v57_b22: bool = !n58 || !n57 || !n90 || !n89;
    let live_v57_b22: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v58_b23: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n239);
    let bd_v58_b23: bool = !n58 || !n57 || !n90 || !n89;
    let live_v58_b23: u16 = ALL & zb_holds(n238) & zb_holds(n240) & zb_holds(n266);
    let ok_v0_b24: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n298);
    let bd_v0_b24: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b24: u16 = ALL & zb_holds(n266) & zb_holds(n297);
    let ok_v32_b25: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n298);
    let bd_v32_b25: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b25: u16 = ALL & zb_holds(n266) & zb_holds(n297);
    let ok_v0_b26: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n345);
    let bd_v0_b26: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b26: u16 = if true { 0 } else { ALL };
    let ok_v16_b27: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n345);
    let bd_v16_b27: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b27: u16 = ALL & zb_holds(n344) & zb_holds(n348);
    let ok_v32_b28: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n345);
    let bd_v32_b28: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b28: u16 = ALL & zb_holds(n344) & zb_holds(n348);
    let ok_v0_b29: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n367);
    let bd_v0_b29: bool = !n58 || !n57 || !n90 || !n89;
    let live_v0_b29: u16 = ALL & zb_holds(n366);
    let ok_v16_b30: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n367);
    let bd_v16_b30: bool = !n58 || !n57 || !n90 || !n89;
    let live_v16_b30: u16 = ALL & zb_holds(n369);
    let ok_v32_b31: u16 = ALL & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n79) & zb_holds(n92) & zb_holds(n78) & zb_holds(n91) & zb_holds(n59) & zb_holds(n77) & zb_holds(n88) & zb_holds(r_c234) & zb_holds(n81) & zb_holds(n80) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n87) & zb_holds(n367);
    let bd_v32_b31: bool = !n58 || !n57 || !n90 || !n89;
    let live_v32_b31: u16 = ALL & zb_holds(n369);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n63,
        c86: n103,
        c256: n105,
        c85: n104,
    };
    let sh1 = KShared1 {
        c87: n296,
        c84: n63,
        c86: n103,
        c85: n104,
    };
    let sh2 = KShared2 {
        c87: n333,
        c39: n334,
        c84: n63,
        c86: n103,
        c237: n338,
        c275: n342,
        c248: n339,
        c249: n340,
        c277: n343,
        c253: n341,
        c161: n337,
        c85: n104,
        c38: n336,
    };
    let sh3 = KShared3 {
        c87: n355,
        c39: n356,
        c84: n63,
        c86: n103,
        c235: n360,
        c273: n364,
        c246: n361,
        c247: n362,
        c275: n365,
        c251: n363,
        c159: n359,
        c85: n104,
        c38: n358,
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
    let mut take_3_1: u16 = 0;
    // 32 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 2] groups that write identical values.
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
        c241: n245,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: n250,
        h1: n454, h2: n455,
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
        c241: n245,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n273,
        c283: n250,
        h1: n464, h2: n465,
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
        c241: n245,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n276,
        c283: n250,
        h1: n470, h2: n471,
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
        c241: n262,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n263,
        c283: n264,
        h1: n498, h2: n499,
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
        c241: n262,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n275,
        c283: n264,
        h1: n506, h2: n507,
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
        c241: n262,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n278,
        c283: n264,
        h1: n512, h2: n513,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n564, h2: n565,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n578, h2: n579,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n584, h2: n585,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n606, h2: n607,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n622, h2: n623,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n634, h2: n635,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n646, h2: n647,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n656, h2: n657,
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
        c241: n245,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n664, h2: n665,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n684, h2: n685,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n694, h2: n695,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n698, h2: n699,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n712, h2: n713,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n724, h2: n725,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n734, h2: n735,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n742, h2: n743,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n750, h2: n751,
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
        c241: n262,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n758, h2: n759,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n772, h2: n773,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: n776, h2: n777,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_2_0 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n824, h2: n825,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o2(16, take_2_0, &sh2, &o2);
    declined |= live_v32_b28 & (if bd_v32_b28 { ALL } else { !ok_v32_b28 });
    take_2_1 |= live_v32_b28 & ok_v32_b28 & (if bd_v32_b28 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n351,
        h1: n828, h2: n829,
    };
    // body 28: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b29 & (if bd_v0_b29 { ALL } else { !ok_v0_b29 });
    take_3_0 |= live_v0_b29 & ok_v0_b29 & (if bd_v0_b29 { 0 } else { ALL });
    declined |= live_v16_b30 & (if bd_v16_b30 { ALL } else { !ok_v16_b30 });
    take_3_0 |= live_v16_b30 & ok_v16_b30 & (if bd_v16_b30 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n357,
        h1: n878, h2: n879,
    };
    // body 30: buttons 0x10, forks 0x0
    sink.o3(16, take_3_0, &sh3, &o3);
    declined |= live_v32_b31 & (if bd_v32_b31 { ALL } else { !ok_v32_b31 });
    take_3_1 |= live_v32_b31 & ok_v32_b31 & (if bd_v32_b31 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n370,
        h1: n882, h2: n883,
    };
    // body 31: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined
}
