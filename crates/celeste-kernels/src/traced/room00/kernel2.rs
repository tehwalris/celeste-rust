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
    pub c39: ZN,
    pub c237: ZN,
    pub c275: ZN,
    pub c248: ZN,
    pub c249: ZN,
    pub c277: ZN,
    pub c253: ZN,
    pub c161: ZN,
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
    pub c39: ZN,
    pub c235: ZN,
    pub c273: ZN,
    pub c246: ZN,
    pub c247: ZN,
    pub c275: ZN,
    pub c251: ZN,
    pub c159: ZN,
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[237] { v.push(sh.c237.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[248] { v.push(sh.c248.lane(i)); }
        if let Col::N(v) = &mut acc.cols[249] { v.push(sh.c249.lane(i)); }
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[161] { v.push(sh.c161.lane(i)); }
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[235] { v.push(sh.c235.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[247] { v.push(sh.c247.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[251] { v.push(sh.c251.lane(i)); }
        if let Col::N(v) = &mut acc.cols[159] { v.push(sh.c159.lane(i)); }
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
    let n146: ZN = zn_rem(n135, zn_splat(P8::from_raw(524288i32)));
    let n147: ZB = zn_ge(n146, zn_splat(P8::from_raw(393216i32)));
    let n148: ZN = zn_mul(n138, zn_splat(P8::from_raw(524288i32)));
    let n149: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n148);
    let n150: ZB = zn_eq(n134, n149);
    let n151: ZB = zb_or(n147, n150);
    let n152: ZB = zb_and(n145, n151);
    let n153: ZB = zb_not(n152);
    let n154: ZB = zb_and(n141, n152);
    let n155: ZB = zb_and(n141, n153);
    let n156: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n144);
    let n157: ZN = zn_rem(n130, zn_splat(P8::from_raw(524288i32)));
    let n158: ZB = zn_le(n157, zn_splat(P8::from_raw(131072i32)));
    let n159: ZB = zb_and(n156, n158);
    let n160: ZB = zb_not(n159);
    let n161: ZB = zb_and(n155, n159);
    let n162: ZB = zb_and(n155, n160);
    let n163: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n144);
    let n164: ZB = zb_not(n163);
    let n165: ZB = zb_and(n162, n163);
    let n166: ZB = zb_and(n162, n164);
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n144);
    let n168: ZB = zb_not(n167);
    let n169: ZB = zb_and(n166, n167);
    let n170: ZB = zb_and(n166, n168);
    let n171: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n133);
    let n172: ZB = zn_le(n171, n137);
    let n173: ZB = zn_gt(n171, n137);
    let n174: ZB = zb_and(n170, n172);
    let n175: ZB = zb_and(n170, n173);
    let n176: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n171);
    let n177: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n176);
    let n178: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n177);
    let n179: ZN = zn_mul(n171, zn_splat(P8::from_raw(524288i32)));
    let n180: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n179);
    let n181: ZB = zn_eq(n134, n180);
    let n182: ZB = zb_or(n147, n181);
    let n183: ZB = zb_and(n178, n182);
    let n184: ZB = zb_not(n183);
    let n185: ZB = zb_and(n174, n183);
    let n186: ZB = zb_and(n174, n184);
    let n187: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n177);
    let n188: ZB = zb_and(n158, n187);
    let n189: ZB = zb_not(n188);
    let n190: ZB = zb_and(n186, n188);
    let n191: ZB = zb_and(n186, n189);
    let n192: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n177);
    let n193: ZB = zb_not(n192);
    let n194: ZB = zb_and(n191, n192);
    let n195: ZB = zb_and(n191, n193);
    let n196: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n177);
    let n197: ZB = zb_not(n196);
    let n198: ZB = zb_and(n195, n196);
    let n199: ZB = zb_and(n195, n197);
    let n200: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n133);
    let n201: ZB = zn_le(n200, n137);
    let n202: ZB = zn_gt(n200, n137);
    let n203: ZB = zb_and(n199, n201);
    let n204: ZB = zb_and(n199, n202);
    let n205: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n200);
    let n206: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n205);
    let n207: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n206);
    let n208: ZN = zn_mul(n200, zn_splat(P8::from_raw(524288i32)));
    let n209: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n208);
    let n210: ZB = zn_eq(n134, n209);
    let n211: ZB = zb_or(n147, n210);
    let n212: ZB = zb_and(n207, n211);
    let n213: ZB = zb_not(n212);
    let n214: ZB = zb_and(n203, n212);
    let n215: ZB = zb_and(n203, n213);
    let n216: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n206);
    let n217: ZB = zb_and(n158, n216);
    let n218: ZB = zb_not(n217);
    let n219: ZB = zb_and(n215, n217);
    let n220: ZB = zb_and(n215, n218);
    let n221: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n206);
    let n222: ZB = zb_not(n221);
    let n223: ZB = zb_and(n220, n221);
    let n224: ZB = zb_and(n220, n222);
    let n225: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n206);
    let n226: ZB = zb_not(n225);
    let n227: ZB = zb_and(n224, n225);
    let n228: ZB = zb_and(n224, n226);
    let n229: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n133);
    let n230: ZB = zn_gt(n229, n137);
    let n231: ZB = zb_or(n204, n228);
    let n232: ZB = zb_or(n202, n230);
    let n233: ZB = zb_or(n175, n231);
    let n234: ZB = zb_or(n173, n232);
    let n235: ZB = zb_or(n142, n233);
    let n236: ZB = zb_or(n140, n234);
    let n237: ZB = zn_le(n101, zn_splat(P8::from_raw(8388608i32)));
    let n238: ZB = zb_and(n235, n237);
    let n239: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n130);
    let n240: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n239, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n241: ZB = zb_not(n240);
    let n242: ZN = zsel_n(n240, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n243: ZN = zsel_n(n241, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n244: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n243);
    let n245: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n243);
    let n246: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n130);
    let n247: ZN = zsel_n(n241, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n248: ZB = zn_gt(n242, zn_splat(P8::from_raw(0i32)));
    let n249: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n246, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n250: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n246, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n251: ZN = zsel_n(n250, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n252: ZN = zsel_n(n249, zn_splat(P8::from_raw(-65536i32)), n251);
    let n253: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n252);
    let n254: ZB = zb_not(n253);
    let n255: ZN = zn_neg(n252);
    let n256: ZN = zn_mul(n255, zn_splat(P8::from_raw(131072i32)));
    let n257: ZN = zsel_n(n254, n256, zn_splat(P8::from_raw(0i32)));
    let n258: ZN = zsel_n(n254, zn_splat(P8::from_raw(-131072i32)), n247);
    let n259: ZN = zsel_n(n248, zn_splat(P8::from_raw(0i32)), n242);
    let n260: ZN = zsel_n(n248, zn_splat(P8::from_raw(0i32)), n257);
    let n261: ZN = zsel_n(n248, zn_splat(P8::from_raw(-131072i32)), n258);
    let n262: ZB = zn_lt(n101, zn_splat(P8::from_raw(-262144i32)));
    let n263: ZB = zn_ge(n101, zn_splat(P8::from_raw(-262144i32)));
    let n264: ZB = zb_and(n238, n262);
    let n266: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n270: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n244);
    let n271: ZN = zsel_n(n254, n256, n270);
    let n272: ZN = zsel_n(n248, n270, n271);
    let n273: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n245);
    let n274: ZN = zsel_n(n254, n256, n273);
    let n275: ZN = zsel_n(n248, n273, n274);
    let n277: ZB = zb_or(n165, n169);
    let n278: ZB = zb_or(n161, n277);
    let n279: ZB = zb_or(n154, n278);
    let n280: ZB = zb_or(n194, n198);
    let n281: ZB = zb_or(n190, n280);
    let n282: ZB = zb_or(n185, n281);
    let n283: ZB = zb_or(n223, n227);
    let n284: ZB = zb_or(n219, n283);
    let n285: ZB = zb_or(n214, n284);
    let n286: ZB = zb_or(n282, n285);
    let n287: ZB = zb_or(n279, n286);
    let n288: ZB = zn_gt(n101, zn_splat(P8::from_raw(8388608i32)));
    let n289: ZB = zb_and(n235, n288);
    let n290: ZB = zb_or(n287, n289);
    let n291: ZB = zb_or(n236, n287);
    let n292: ZB = zb_and(n262, n290);
    let n300: ZB = zn_lt(n101, zn_splat(P8::from_raw(7340032i32)));
    let n301: ZB = zb_and(n75, n95);
    let n302: ZN = zsel_n(n300, zn_splat(P8::from_raw(196608i32)), r_c235);
    let n303: ZN = zsel_n(n300, zn_splat(P8::from_raw(65536i32)), r_c247);
    let n304: ZB = zb_and(n121, n122);
    let n305: ZB = zb_not(n125);
    let n306: ZB = zb_and(n124, n305);
    let n307: ZB = zn_ge(n127, zn_splat(P8::from_raw(0i32)));
    let n308: ZB = zb_and(n126, n307);
    let n309: ZN = zsel_n(n125, n127, r_c235);
    let n310: ZN = zsel_n(n125, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(196608i32)));
    let n311: ZB = zb_or(n306, n308);
    let n312: ZN = zsel_n(n122, r_c235, n309);
    let n313: ZN = zsel_n(n122, zn_splat(P8::from_raw(196608i32)), n310);
    let n314: ZN = zsel_n(n122, zn_splat(P8::from_raw(-229376i32)), zn_splat(P8::from_raw(-262144i32)));
    let n315: ZB = zb_or(n304, n311);
    let n316: ZN = zsel_n(n95, n302, n312);
    let n317: ZN = zsel_n(n95, zn_splat(P8::from_raw(196608i32)), n313);
    let n318: ZN = zsel_n(n95, n303, r_c247);
    let n319: ZN = zsel_n(n95, zn_splat(P8::from_raw(-262144i32)), n314);
    let n320: ZB = zb_or(n301, n315);
    let n321: ZN = zsel_n(n264, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n322: ZB = zb_not(n264);
    let n323: ZB = zb_or(n264, n292);
    let n324: ZB = zsel_b(n264, n236, n291);
    let n325: ZN = zsel_n(n320, r_c39, n321);
    let n326: ZB = zb_not(n320);
    let n327: ZB = zb_and(n322, n326);
    let n328: ZN = zsel_n(n320, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n329: ZN = zsel_n(n320, n316, zn_splat(P8::from_raw(196608i32)));
    let n330: ZN = zsel_n(n320, n317, zn_splat(P8::from_raw(196608i32)));
    let n331: ZN = zsel_n(n320, n318, zn_splat(P8::from_raw(65536i32)));
    let n332: ZN = zsel_n(n320, n101, zn_splat(P8::from_raw(8126464i32)));
    let n333: ZN = zsel_n(n320, n319, zn_splat(P8::from_raw(-262144i32)));
    let n334: ZN = zsel_n(n320, zn_splat(P8::from_raw(6291456i32)), zn_splat(P8::from_raw(7340032i32)));
    let n335: ZB = zb_or(n320, n323);
    let n336: ZB = zb_or(n320, n324);
    let n337: ZN = zn_rem(n328, zn_splat(P8::from_raw(524288i32)));
    let n338: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n337);
    let n339: ZB = zn_eq(zn_splat(P8::from_raw(2031616i32)), n338);
    let n342: ZN = zsel_n(n320, r_c20, zn_splat(P8::from_raw(131072i32)));
    let n343: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n344: ZB = zb_not(n339);
    let n345: ZB = zb_and(n335, n344);
    let n346: ZN = zsel_n(n266, r_c39, n325);
    let n347: ZN = zsel_n(n266, n343, r_c20);
    let n348: ZB = zb_and(n75, n327);
    let n349: ZN = zsel_n(n266, zn_splat(P8::from_raw(0i32)), n328);
    let n350: ZN = zsel_n(n266, r_c235, n329);
    let n351: ZN = zsel_n(n266, zn_splat(P8::from_raw(196608i32)), n330);
    let n352: ZN = zsel_n(n266, r_c247, n331);
    let n353: ZN = zsel_n(n266, r_c251, n332);
    let n354: ZN = zsel_n(n266, zn_splat(P8::from_raw(-262144i32)), n333);
    let n355: ZN = zsel_n(n266, zn_splat(P8::from_raw(6291456i32)), n334);
    let n356: ZB = zb_or(n266, n335);
    let n357: ZB = zb_or(n266, n336);
    let n359: ZB = zb_or(n266, n345);
    let n360: ZN = zsel_n(n266, n343, n342);
    let n363: ZW = zw_bits_n(r_c39);
    let n364: ZW = zw_mix1(zw_splat(11400714819323198485u64), n363, 39u64);
    let n365: ZW = zw_mix2(zw_splat(11562461410679940143u64), n363, 39u64);
    let n366: ZW = zw_bits_n(n101);
    let n367: ZW = zw_mix1(n364, n366, 256u64);
    let n368: ZW = zw_mix2(n365, n366, 256u64);
    let n369: ZW = zw_bits_n(r_c20);
    let n370: ZW = zw_mix1(n367, n369, 20u64);
    let n371: ZW = zw_mix2(n368, n369, 20u64);
    let n372: u64 = false as u64;
    let n373: ZW = zw_mix1(n370, zw_splat(n372), 41u64);
    let n374: ZW = zw_mix2(n371, zw_splat(n372), 41u64);
    let n375: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n376: ZW = zw_mix1(n373, zw_splat(n375), 236u64);
    let n377: ZW = zw_mix2(n374, zw_splat(n375), 236u64);
    let n378: ZW = zw_mix1(n376, zw_splat(n375), 238u64);
    let n379: ZW = zw_mix2(n377, zw_splat(n375), 238u64);
    let n380: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n381: ZW = zw_mix1(n378, zw_splat(n380), 239u64);
    let n382: ZW = zw_mix2(n379, zw_splat(n380), 239u64);
    let n383: ZW = zw_bits_n(n242);
    let n384: ZW = zw_mix1(n381, n383, 241u64);
    let n385: ZW = zw_mix2(n382, n383, 241u64);
    let n386: ZW = zw_mix1(n384, zw_splat(n372), 248u64);
    let n387: ZW = zw_mix2(n385, zw_splat(n372), 248u64);
    let n388: ZW = zw_mix1(n386, zw_splat(n372), 249u64);
    let n389: ZW = zw_mix2(n387, zw_splat(n372), 249u64);
    let n390: ZW = zw_mix1(n388, zw_splat(n375), 270u64);
    let n391: ZW = zw_mix2(n389, zw_splat(n375), 270u64);
    let n392: ZW = zw_mix1(n390, zw_splat(n375), 271u64);
    let n393: ZW = zw_mix2(n391, zw_splat(n375), 271u64);
    let n394: ZW = zw_mix1(n392, zw_splat(n375), 272u64);
    let n395: ZW = zw_mix2(n393, zw_splat(n375), 272u64);
    let n396: ZW = zw_mix1(n394, zw_splat(n375), 273u64);
    let n397: ZW = zw_mix2(n395, zw_splat(n375), 273u64);
    let n398: ZW = zw_mix1(n396, zw_splat(n372), 274u64);
    let n399: ZW = zw_mix2(n397, zw_splat(n372), 274u64);
    let n400: ZW = zw_mix1(n398, zw_splat(n375), 282u64);
    let n401: ZW = zw_mix2(n399, zw_splat(n375), 282u64);
    let n402: ZW = zw_bits_n(n247);
    let n403: ZW = zw_mix1(n400, n402, 283u64);
    let n404: ZW = zw_mix2(n401, n402, 283u64);
    let n405: u64 = true as u64;
    let n406: ZW = zw_mix1(n396, zw_splat(n405), 274u64);
    let n407: ZW = zw_mix2(n397, zw_splat(n405), 274u64);
    let n408: ZW = zw_bits_n(n270);
    let n409: ZW = zw_mix1(n406, n408, 282u64);
    let n410: ZW = zw_mix2(n407, n408, 282u64);
    let n411: ZW = zw_mix1(n409, n402, 283u64);
    let n412: ZW = zw_mix2(n410, n402, 283u64);
    let n413: ZW = zw_bits_n(n273);
    let n414: ZW = zw_mix1(n398, n413, 282u64);
    let n415: ZW = zw_mix2(n399, n413, 282u64);
    let n416: ZW = zw_mix1(n414, n402, 283u64);
    let n417: ZW = zw_mix2(n415, n402, 283u64);
    let n418: ZW = zw_bits_n(n259);
    let n419: ZW = zw_mix1(n381, n418, 241u64);
    let n420: ZW = zw_mix2(n382, n418, 241u64);
    let n421: ZW = zw_mix1(n419, zw_splat(n372), 248u64);
    let n422: ZW = zw_mix2(n420, zw_splat(n372), 248u64);
    let n423: ZW = zw_mix1(n421, zw_splat(n405), 249u64);
    let n424: ZW = zw_mix2(n422, zw_splat(n405), 249u64);
    let n425: ZW = zw_mix1(n423, zw_splat(n375), 270u64);
    let n426: ZW = zw_mix2(n424, zw_splat(n375), 270u64);
    let n427: ZW = zw_mix1(n425, zw_splat(n375), 271u64);
    let n428: ZW = zw_mix2(n426, zw_splat(n375), 271u64);
    let n429: ZW = zw_mix1(n427, zw_splat(n375), 272u64);
    let n430: ZW = zw_mix2(n428, zw_splat(n375), 272u64);
    let n431: ZW = zw_mix1(n429, zw_splat(n375), 273u64);
    let n432: ZW = zw_mix2(n430, zw_splat(n375), 273u64);
    let n433: ZW = zw_mix1(n431, zw_splat(n372), 274u64);
    let n434: ZW = zw_mix2(n432, zw_splat(n372), 274u64);
    let n435: ZW = zw_bits_n(n260);
    let n436: ZW = zw_mix1(n433, n435, 282u64);
    let n437: ZW = zw_mix2(n434, n435, 282u64);
    let n438: ZW = zw_bits_n(n261);
    let n439: ZW = zw_mix1(n436, n438, 283u64);
    let n440: ZW = zw_mix2(n437, n438, 283u64);
    let n441: ZW = zw_mix1(n431, zw_splat(n405), 274u64);
    let n442: ZW = zw_mix2(n432, zw_splat(n405), 274u64);
    let n443: ZW = zw_bits_n(n272);
    let n444: ZW = zw_mix1(n441, n443, 282u64);
    let n445: ZW = zw_mix2(n442, n443, 282u64);
    let n446: ZW = zw_mix1(n444, n438, 283u64);
    let n447: ZW = zw_mix2(n445, n438, 283u64);
    let n448: ZW = zw_bits_n(n275);
    let n449: ZW = zw_mix1(n433, n448, 282u64);
    let n450: ZW = zw_mix2(n434, n448, 282u64);
    let n451: ZW = zw_mix1(n449, n438, 283u64);
    let n452: ZW = zw_mix2(n450, n438, 283u64);
    let n453: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n454: ZW = zw_mix1(n367, zw_splat(n453), 20u64);
    let n455: ZW = zw_mix2(n368, zw_splat(n453), 20u64);
    let n456: ZW = zw_mix1(n454, zw_splat(n405), 41u64);
    let n457: ZW = zw_mix2(n455, zw_splat(n405), 41u64);
    let n458: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n459: ZW = zw_mix1(n456, zw_splat(n458), 236u64);
    let n460: ZW = zw_mix2(n457, zw_splat(n458), 236u64);
    let n461: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n462: ZW = zw_mix1(n459, zw_splat(n461), 238u64);
    let n463: ZW = zw_mix2(n460, zw_splat(n461), 238u64);
    let n464: ZW = zw_mix1(n462, zw_splat(n375), 239u64);
    let n465: ZW = zw_mix2(n463, zw_splat(n375), 239u64);
    let n466: ZW = zw_mix1(n464, n383, 241u64);
    let n467: ZW = zw_mix2(n465, n383, 241u64);
    let n468: ZW = zw_mix1(n466, zw_splat(n405), 248u64);
    let n469: ZW = zw_mix2(n467, zw_splat(n405), 248u64);
    let n470: ZW = zw_mix1(n468, zw_splat(n372), 249u64);
    let n471: ZW = zw_mix2(n469, zw_splat(n372), 249u64);
    let n472: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n473: ZW = zw_mix1(n470, zw_splat(n472), 270u64);
    let n474: ZW = zw_mix2(n471, zw_splat(n472), 270u64);
    let n475: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n476: ZW = zw_mix1(n473, zw_splat(n475), 271u64);
    let n477: ZW = zw_mix2(n474, zw_splat(n475), 271u64);
    let n478: ZW = zw_mix1(n476, zw_splat(n453), 272u64);
    let n479: ZW = zw_mix2(n477, zw_splat(n453), 272u64);
    let n480: ZW = zw_mix1(n478, zw_splat(n375), 273u64);
    let n481: ZW = zw_mix2(n479, zw_splat(n375), 273u64);
    let n482: ZW = zw_mix1(n480, zw_splat(n372), 274u64);
    let n483: ZW = zw_mix2(n481, zw_splat(n372), 274u64);
    let n484: ZW = zw_mix1(n482, zw_splat(n380), 282u64);
    let n485: ZW = zw_mix2(n483, zw_splat(n380), 282u64);
    let n486: ZW = zw_mix1(n484, zw_splat(n375), 283u64);
    let n487: ZW = zw_mix2(n485, zw_splat(n375), 283u64);
    let n488: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n489: ZW = zw_mix1(n476, zw_splat(n488), 272u64);
    let n490: ZW = zw_mix2(n477, zw_splat(n488), 272u64);
    let n491: ZW = zw_mix1(n489, zw_splat(n375), 273u64);
    let n492: ZW = zw_mix2(n490, zw_splat(n375), 273u64);
    let n493: ZW = zw_mix1(n491, zw_splat(n405), 274u64);
    let n494: ZW = zw_mix2(n492, zw_splat(n405), 274u64);
    let n495: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n496: ZW = zw_mix1(n493, zw_splat(n495), 282u64);
    let n497: ZW = zw_mix2(n494, zw_splat(n495), 282u64);
    let n498: ZW = zw_mix1(n496, zw_splat(n375), 283u64);
    let n499: ZW = zw_mix2(n497, zw_splat(n375), 283u64);
    let n500: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n501: ZW = zw_mix1(n482, zw_splat(n500), 282u64);
    let n502: ZW = zw_mix2(n483, zw_splat(n500), 282u64);
    let n503: ZW = zw_mix1(n501, zw_splat(n375), 283u64);
    let n504: ZW = zw_mix2(n502, zw_splat(n375), 283u64);
    let n505: ZW = zw_mix1(n470, zw_splat(n475), 270u64);
    let n506: ZW = zw_mix2(n471, zw_splat(n475), 270u64);
    let n507: ZW = zw_mix1(n505, zw_splat(n472), 271u64);
    let n508: ZW = zw_mix2(n506, zw_splat(n472), 271u64);
    let n509: ZW = zw_mix1(n507, zw_splat(n375), 272u64);
    let n510: ZW = zw_mix2(n508, zw_splat(n375), 272u64);
    let n511: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n512: ZW = zw_mix1(n509, zw_splat(n511), 273u64);
    let n513: ZW = zw_mix2(n510, zw_splat(n511), 273u64);
    let n514: ZW = zw_mix1(n512, zw_splat(n372), 274u64);
    let n515: ZW = zw_mix2(n513, zw_splat(n372), 274u64);
    let n516: ZW = zw_mix1(n514, zw_splat(n375), 282u64);
    let n517: ZW = zw_mix2(n515, zw_splat(n375), 282u64);
    let n518: ZW = zw_mix1(n516, zw_splat(n495), 283u64);
    let n519: ZW = zw_mix2(n517, zw_splat(n495), 283u64);
    let n520: ZW = zw_mix1(n505, zw_splat(n475), 271u64);
    let n521: ZW = zw_mix2(n506, zw_splat(n475), 271u64);
    let n522: ZW = zw_mix1(n520, zw_splat(n488), 272u64);
    let n523: ZW = zw_mix2(n521, zw_splat(n488), 272u64);
    let n524: ZW = zw_mix1(n522, zw_splat(n511), 273u64);
    let n525: ZW = zw_mix2(n523, zw_splat(n511), 273u64);
    let n526: ZW = zw_mix1(n524, zw_splat(n405), 274u64);
    let n527: ZW = zw_mix2(n525, zw_splat(n405), 274u64);
    let n528: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n529: ZW = zw_mix1(n526, zw_splat(n528), 282u64);
    let n530: ZW = zw_mix2(n527, zw_splat(n528), 282u64);
    let n531: ZW = zw_mix1(n529, zw_splat(n528), 283u64);
    let n532: ZW = zw_mix2(n530, zw_splat(n528), 283u64);
    let n533: ZW = zw_mix1(n520, zw_splat(n453), 272u64);
    let n534: ZW = zw_mix2(n521, zw_splat(n453), 272u64);
    let n535: ZW = zw_mix1(n533, zw_splat(n511), 273u64);
    let n536: ZW = zw_mix2(n534, zw_splat(n511), 273u64);
    let n537: ZW = zw_mix1(n535, zw_splat(n372), 274u64);
    let n538: ZW = zw_mix2(n536, zw_splat(n372), 274u64);
    let n539: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n540: ZW = zw_mix1(n537, zw_splat(n539), 282u64);
    let n541: ZW = zw_mix2(n538, zw_splat(n539), 282u64);
    let n542: ZW = zw_mix1(n540, zw_splat(n528), 283u64);
    let n543: ZW = zw_mix2(n541, zw_splat(n528), 283u64);
    let n544: ZW = zw_mix1(n509, zw_splat(n453), 273u64);
    let n545: ZW = zw_mix2(n510, zw_splat(n453), 273u64);
    let n546: ZW = zw_mix1(n544, zw_splat(n372), 274u64);
    let n547: ZW = zw_mix2(n545, zw_splat(n372), 274u64);
    let n548: ZW = zw_mix1(n546, zw_splat(n375), 282u64);
    let n549: ZW = zw_mix2(n547, zw_splat(n375), 282u64);
    let n550: ZW = zw_mix1(n548, zw_splat(n500), 283u64);
    let n551: ZW = zw_mix2(n549, zw_splat(n500), 283u64);
    let n552: ZW = zw_mix1(n522, zw_splat(n453), 273u64);
    let n553: ZW = zw_mix2(n523, zw_splat(n453), 273u64);
    let n554: ZW = zw_mix1(n552, zw_splat(n405), 274u64);
    let n555: ZW = zw_mix2(n553, zw_splat(n405), 274u64);
    let n556: ZW = zw_mix1(n554, zw_splat(n528), 282u64);
    let n557: ZW = zw_mix2(n555, zw_splat(n528), 282u64);
    let n558: ZW = zw_mix1(n556, zw_splat(n539), 283u64);
    let n559: ZW = zw_mix2(n557, zw_splat(n539), 283u64);
    let n560: ZW = zw_mix1(n533, zw_splat(n453), 273u64);
    let n561: ZW = zw_mix2(n534, zw_splat(n453), 273u64);
    let n562: ZW = zw_mix1(n560, zw_splat(n372), 274u64);
    let n563: ZW = zw_mix2(n561, zw_splat(n372), 274u64);
    let n564: ZW = zw_mix1(n562, zw_splat(n539), 282u64);
    let n565: ZW = zw_mix2(n563, zw_splat(n539), 282u64);
    let n566: ZW = zw_mix1(n564, zw_splat(n539), 283u64);
    let n567: ZW = zw_mix2(n565, zw_splat(n539), 283u64);
    let n568: ZW = zw_mix1(n464, n418, 241u64);
    let n569: ZW = zw_mix2(n465, n418, 241u64);
    let n570: ZW = zw_mix1(n568, zw_splat(n405), 248u64);
    let n571: ZW = zw_mix2(n569, zw_splat(n405), 248u64);
    let n572: ZW = zw_mix1(n570, zw_splat(n405), 249u64);
    let n573: ZW = zw_mix2(n571, zw_splat(n405), 249u64);
    let n574: ZW = zw_mix1(n572, zw_splat(n472), 270u64);
    let n575: ZW = zw_mix2(n573, zw_splat(n472), 270u64);
    let n576: ZW = zw_mix1(n574, zw_splat(n475), 271u64);
    let n577: ZW = zw_mix2(n575, zw_splat(n475), 271u64);
    let n578: ZW = zw_mix1(n576, zw_splat(n453), 272u64);
    let n579: ZW = zw_mix2(n577, zw_splat(n453), 272u64);
    let n580: ZW = zw_mix1(n578, zw_splat(n375), 273u64);
    let n581: ZW = zw_mix2(n579, zw_splat(n375), 273u64);
    let n582: ZW = zw_mix1(n580, zw_splat(n372), 274u64);
    let n583: ZW = zw_mix2(n581, zw_splat(n372), 274u64);
    let n584: ZW = zw_mix1(n582, zw_splat(n380), 282u64);
    let n585: ZW = zw_mix2(n583, zw_splat(n380), 282u64);
    let n586: ZW = zw_mix1(n584, zw_splat(n375), 283u64);
    let n587: ZW = zw_mix2(n585, zw_splat(n375), 283u64);
    let n588: ZW = zw_mix1(n576, zw_splat(n488), 272u64);
    let n589: ZW = zw_mix2(n577, zw_splat(n488), 272u64);
    let n590: ZW = zw_mix1(n588, zw_splat(n375), 273u64);
    let n591: ZW = zw_mix2(n589, zw_splat(n375), 273u64);
    let n592: ZW = zw_mix1(n590, zw_splat(n405), 274u64);
    let n593: ZW = zw_mix2(n591, zw_splat(n405), 274u64);
    let n594: ZW = zw_mix1(n592, zw_splat(n495), 282u64);
    let n595: ZW = zw_mix2(n593, zw_splat(n495), 282u64);
    let n596: ZW = zw_mix1(n594, zw_splat(n375), 283u64);
    let n597: ZW = zw_mix2(n595, zw_splat(n375), 283u64);
    let n598: ZW = zw_mix1(n582, zw_splat(n500), 282u64);
    let n599: ZW = zw_mix2(n583, zw_splat(n500), 282u64);
    let n600: ZW = zw_mix1(n598, zw_splat(n375), 283u64);
    let n601: ZW = zw_mix2(n599, zw_splat(n375), 283u64);
    let n602: ZW = zw_mix1(n572, zw_splat(n475), 270u64);
    let n603: ZW = zw_mix2(n573, zw_splat(n475), 270u64);
    let n604: ZW = zw_mix1(n602, zw_splat(n472), 271u64);
    let n605: ZW = zw_mix2(n603, zw_splat(n472), 271u64);
    let n606: ZW = zw_mix1(n604, zw_splat(n375), 272u64);
    let n607: ZW = zw_mix2(n605, zw_splat(n375), 272u64);
    let n608: ZW = zw_mix1(n606, zw_splat(n511), 273u64);
    let n609: ZW = zw_mix2(n607, zw_splat(n511), 273u64);
    let n610: ZW = zw_mix1(n608, zw_splat(n372), 274u64);
    let n611: ZW = zw_mix2(n609, zw_splat(n372), 274u64);
    let n612: ZW = zw_mix1(n610, zw_splat(n375), 282u64);
    let n613: ZW = zw_mix2(n611, zw_splat(n375), 282u64);
    let n614: ZW = zw_mix1(n612, zw_splat(n495), 283u64);
    let n615: ZW = zw_mix2(n613, zw_splat(n495), 283u64);
    let n616: ZW = zw_mix1(n602, zw_splat(n475), 271u64);
    let n617: ZW = zw_mix2(n603, zw_splat(n475), 271u64);
    let n618: ZW = zw_mix1(n616, zw_splat(n488), 272u64);
    let n619: ZW = zw_mix2(n617, zw_splat(n488), 272u64);
    let n620: ZW = zw_mix1(n618, zw_splat(n511), 273u64);
    let n621: ZW = zw_mix2(n619, zw_splat(n511), 273u64);
    let n622: ZW = zw_mix1(n620, zw_splat(n405), 274u64);
    let n623: ZW = zw_mix2(n621, zw_splat(n405), 274u64);
    let n624: ZW = zw_mix1(n622, zw_splat(n528), 282u64);
    let n625: ZW = zw_mix2(n623, zw_splat(n528), 282u64);
    let n626: ZW = zw_mix1(n624, zw_splat(n528), 283u64);
    let n627: ZW = zw_mix2(n625, zw_splat(n528), 283u64);
    let n628: ZW = zw_mix1(n616, zw_splat(n453), 272u64);
    let n629: ZW = zw_mix2(n617, zw_splat(n453), 272u64);
    let n630: ZW = zw_mix1(n628, zw_splat(n511), 273u64);
    let n631: ZW = zw_mix2(n629, zw_splat(n511), 273u64);
    let n632: ZW = zw_mix1(n630, zw_splat(n372), 274u64);
    let n633: ZW = zw_mix2(n631, zw_splat(n372), 274u64);
    let n634: ZW = zw_mix1(n632, zw_splat(n539), 282u64);
    let n635: ZW = zw_mix2(n633, zw_splat(n539), 282u64);
    let n636: ZW = zw_mix1(n634, zw_splat(n528), 283u64);
    let n637: ZW = zw_mix2(n635, zw_splat(n528), 283u64);
    let n638: ZW = zw_mix1(n606, zw_splat(n453), 273u64);
    let n639: ZW = zw_mix2(n607, zw_splat(n453), 273u64);
    let n640: ZW = zw_mix1(n638, zw_splat(n372), 274u64);
    let n641: ZW = zw_mix2(n639, zw_splat(n372), 274u64);
    let n642: ZW = zw_mix1(n640, zw_splat(n375), 282u64);
    let n643: ZW = zw_mix2(n641, zw_splat(n375), 282u64);
    let n644: ZW = zw_mix1(n642, zw_splat(n500), 283u64);
    let n645: ZW = zw_mix2(n643, zw_splat(n500), 283u64);
    let n646: ZW = zw_mix1(n618, zw_splat(n453), 273u64);
    let n647: ZW = zw_mix2(n619, zw_splat(n453), 273u64);
    let n648: ZW = zw_mix1(n646, zw_splat(n405), 274u64);
    let n649: ZW = zw_mix2(n647, zw_splat(n405), 274u64);
    let n650: ZW = zw_mix1(n648, zw_splat(n528), 282u64);
    let n651: ZW = zw_mix2(n649, zw_splat(n528), 282u64);
    let n652: ZW = zw_mix1(n650, zw_splat(n539), 283u64);
    let n653: ZW = zw_mix2(n651, zw_splat(n539), 283u64);
    let n654: ZW = zw_mix1(n628, zw_splat(n453), 273u64);
    let n655: ZW = zw_mix2(n629, zw_splat(n453), 273u64);
    let n656: ZW = zw_mix1(n654, zw_splat(n372), 274u64);
    let n657: ZW = zw_mix2(n655, zw_splat(n372), 274u64);
    let n658: ZW = zw_mix1(n656, zw_splat(n539), 282u64);
    let n659: ZW = zw_mix2(n657, zw_splat(n539), 282u64);
    let n660: ZW = zw_mix1(n658, zw_splat(n539), 283u64);
    let n661: ZW = zw_mix2(n659, zw_splat(n539), 283u64);
    let n662: ZW = zw_mix1(zw_splat(11400714819323198485u64), n369, 20u64);
    let n663: ZW = zw_mix2(zw_splat(11562461410679940143u64), n369, 20u64);
    let n664: ZW = zw_mix1(n662, zw_splat(n372), 41u64);
    let n665: ZW = zw_mix2(n663, zw_splat(n372), 41u64);
    let n666: u64 = mix64(11400714819323198485u64 ^ mix64(n453 ^ 20u64));
    let n667: u64 = 11562461410679940143u64.wrapping_add(mix64(n453.wrapping_mul((20u64 << 1) | 1)));
    let n668: u64 = mix64(n666 ^ mix64(n405 ^ 41u64));
    let n669: u64 = n667.wrapping_add(mix64(n405.wrapping_mul((41u64 << 1) | 1)));
    let n670: ZW = zw_bits_b(n327);
    let n671: ZW = zw_mix1(zw_splat(11400714819323198485u64), n670, 38u64);
    let n672: ZW = zw_mix2(zw_splat(11562461410679940143u64), n670, 38u64);
    let n673: ZW = zw_bits_n(n325);
    let n674: ZW = zw_mix1(n671, n673, 39u64);
    let n675: ZW = zw_mix2(n672, n673, 39u64);
    let n676: ZW = zw_bits_n(n328);
    let n677: ZW = zw_mix1(n674, n676, 161u64);
    let n678: ZW = zw_mix2(n675, n676, 161u64);
    let n679: ZW = zw_bits_n(n329);
    let n680: ZW = zw_mix1(n677, n679, 237u64);
    let n681: ZW = zw_mix2(n678, n679, 237u64);
    let n682: ZW = zw_bits_n(n330);
    let n683: ZW = zw_mix1(n680, n682, 248u64);
    let n684: ZW = zw_mix2(n681, n682, 248u64);
    let n685: ZW = zw_bits_n(n331);
    let n686: ZW = zw_mix1(n683, n685, 249u64);
    let n687: ZW = zw_mix2(n684, n685, 249u64);
    let n688: ZW = zw_bits_n(n332);
    let n689: ZW = zw_mix1(n686, n688, 253u64);
    let n690: ZW = zw_mix2(n687, n688, 253u64);
    let n691: ZW = zw_bits_n(n333);
    let n692: ZW = zw_mix1(n689, n691, 275u64);
    let n693: ZW = zw_mix2(n690, n691, 275u64);
    let n694: ZW = zw_bits_n(n334);
    let n695: ZW = zw_mix1(n692, n694, 277u64);
    let n696: ZW = zw_mix2(n693, n694, 277u64);
    let n697: ZW = zw_mix1(n695, n369, 20u64);
    let n698: ZW = zw_mix2(n696, n369, 20u64);
    let n699: ZW = zw_bits_n(n342);
    let n700: ZW = zw_mix1(n695, n699, 20u64);
    let n701: ZW = zw_mix2(n696, n699, 20u64);
    let n702: ZW = zw_bits_b(n348);
    let n703: ZW = zw_mix1(zw_splat(11400714819323198485u64), n702, 38u64);
    let n704: ZW = zw_mix2(zw_splat(11562461410679940143u64), n702, 38u64);
    let n705: ZW = zw_bits_n(n346);
    let n706: ZW = zw_mix1(n703, n705, 39u64);
    let n707: ZW = zw_mix2(n704, n705, 39u64);
    let n708: ZW = zw_bits_n(n349);
    let n709: ZW = zw_mix1(n706, n708, 159u64);
    let n710: ZW = zw_mix2(n707, n708, 159u64);
    let n711: ZW = zw_bits_n(n350);
    let n712: ZW = zw_mix1(n709, n711, 235u64);
    let n713: ZW = zw_mix2(n710, n711, 235u64);
    let n714: ZW = zw_bits_n(n351);
    let n715: ZW = zw_mix1(n712, n714, 246u64);
    let n716: ZW = zw_mix2(n713, n714, 246u64);
    let n717: ZW = zw_bits_n(n352);
    let n718: ZW = zw_mix1(n715, n717, 247u64);
    let n719: ZW = zw_mix2(n716, n717, 247u64);
    let n720: ZW = zw_bits_n(n353);
    let n721: ZW = zw_mix1(n718, n720, 251u64);
    let n722: ZW = zw_mix2(n719, n720, 251u64);
    let n723: ZW = zw_bits_n(n354);
    let n724: ZW = zw_mix1(n721, n723, 273u64);
    let n725: ZW = zw_mix2(n722, n723, 273u64);
    let n726: ZW = zw_bits_n(n355);
    let n727: ZW = zw_mix1(n724, n726, 275u64);
    let n728: ZW = zw_mix2(n725, n726, 275u64);
    let n729: ZW = zw_bits_n(n347);
    let n730: ZW = zw_mix1(n727, n729, 20u64);
    let n731: ZW = zw_mix2(n728, n729, 20u64);
    let n732: ZW = zw_bits_n(n360);
    let n733: ZW = zw_mix1(n727, n732, 20u64);
    let n734: ZW = zw_mix2(n728, n732, 20u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v0_b0: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b0: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v1_b1: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v1_b1: bool = !n60 || !n59 || !n92 || !n91;
    let live_v1_b1: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v2_b2: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v2_b2: bool = !n60 || !n59 || !n92 || !n91;
    let live_v2_b2: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v16_b3: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v16_b3: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b3: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v17_b4: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v17_b4: bool = !n60 || !n59 || !n92 || !n91;
    let live_v17_b4: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v18_b5: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v18_b5: bool = !n60 || !n59 || !n92 || !n91;
    let live_v18_b5: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v32_b6: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v32_b6: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b6: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v33_b7: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v33_b7: bool = !n60 || !n59 || !n92 || !n91;
    let live_v33_b7: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v34_b8: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v34_b8: bool = !n60 || !n59 || !n92 || !n91;
    let live_v34_b8: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v36_b9: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v36_b9: bool = !n60 || !n59 || !n92 || !n91;
    let live_v36_b9: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v37_b10: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v37_b10: bool = !n60 || !n59 || !n92 || !n91;
    let live_v37_b10: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v38_b11: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v38_b11: bool = !n60 || !n59 || !n92 || !n91;
    let live_v38_b11: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v40_b12: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v40_b12: bool = !n60 || !n59 || !n92 || !n91;
    let live_v40_b12: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v41_b13: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v41_b13: bool = !n60 || !n59 || !n92 || !n91;
    let live_v41_b13: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v42_b14: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v42_b14: bool = !n60 || !n59 || !n92 || !n91;
    let live_v42_b14: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v48_b15: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v48_b15: bool = !n60 || !n59 || !n92 || !n91;
    let live_v48_b15: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v49_b16: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v49_b16: bool = !n60 || !n59 || !n92 || !n91;
    let live_v49_b16: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v50_b17: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v50_b17: bool = !n60 || !n59 || !n92 || !n91;
    let live_v50_b17: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v52_b18: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v52_b18: bool = !n60 || !n59 || !n92 || !n91;
    let live_v52_b18: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v53_b19: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v53_b19: bool = !n60 || !n59 || !n92 || !n91;
    let live_v53_b19: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v54_b20: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v54_b20: bool = !n60 || !n59 || !n92 || !n91;
    let live_v54_b20: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v56_b21: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v56_b21: bool = !n60 || !n59 || !n92 || !n91;
    let live_v56_b21: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v57_b22: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v57_b22: bool = !n60 || !n59 || !n92 || !n91;
    let live_v57_b22: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v58_b23: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n236);
    let bd_v58_b23: bool = !n60 || !n59 || !n92 || !n91;
    let live_v58_b23: u16 = ALL & zb_holds(n235) & zb_holds(n237) & zb_holds(n263);
    let ok_v0_b24: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n291);
    let bd_v0_b24: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b24: u16 = ALL & zb_holds(n263) & zb_holds(n290);
    let ok_v32_b25: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n291);
    let bd_v32_b25: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b25: u16 = ALL & zb_holds(n263) & zb_holds(n290);
    let ok_v0_b26: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n336);
    let bd_v0_b26: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b26: u16 = if true { 0 } else { ALL };
    let ok_v16_b27: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n336);
    let bd_v16_b27: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b27: u16 = ALL & zb_holds(n335) & zb_holds(n339);
    let ok_v32_b28: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n336);
    let bd_v32_b28: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b28: u16 = ALL & zb_holds(n335) & zb_holds(n339);
    let ok_v0_b29: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n357);
    let bd_v0_b29: bool = !n60 || !n59 || !n92 || !n91;
    let live_v0_b29: u16 = ALL & zb_holds(n356);
    let ok_v16_b30: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n357);
    let bd_v16_b30: bool = !n60 || !n59 || !n92 || !n91;
    let live_v16_b30: u16 = ALL & zb_holds(n359);
    let ok_v32_b31: u16 = ALL & zb_holds(n100) & zb_holds(n62) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n79) & zb_holds(n94) & zb_holds(n78) & zb_holds(n93) & zb_holds(n61) & zb_holds(n77) & zb_holds(n90) & zb_holds(r_c234) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n58) & zb_holds(r_c171) & zb_holds(n56) & zb_holds(n57) & zb_holds(n89) & zb_holds(n357);
    let bd_v32_b31: bool = !n60 || !n59 || !n92 || !n91;
    let live_v32_b31: u16 = ALL & zb_holds(n359);
    let sh0 = KShared0 {
        c39: r_c39,
        c256: n101,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
        c39: n325,
        c237: n329,
        c275: n333,
        c248: n330,
        c249: n331,
        c277: n334,
        c253: n332,
        c161: n328,
        c38: n327,
    };
    let sh3 = KShared3 {
        c39: n346,
        c235: n350,
        c273: n354,
        c246: n351,
        c247: n352,
        c275: n355,
        c251: n353,
        c159: n349,
        c38: n348,
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
        c236: zn_splat(P8::from_raw(0i32)),
        c272: zn_splat(P8::from_raw(0i32)),
        c273: zn_splat(P8::from_raw(0i32)),
        c238: zn_splat(P8::from_raw(0i32)),
        c239: zn_splat(P8::from_raw(65536i32)),
        c274: zb_splat(false),
        c241: n242,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: n247,
        h1: n403, h2: n404,
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
        c241: n242,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n270,
        c283: n247,
        h1: n411, h2: n412,
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
        c241: n242,
        c248: zb_splat(false),
        c249: zb_splat(false),
        c282: n273,
        c283: n247,
        h1: n416, h2: n417,
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
        c241: n259,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n260,
        c283: n261,
        h1: n439, h2: n440,
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
        c241: n259,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n272,
        c283: n261,
        h1: n446, h2: n447,
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
        c241: n259,
        c248: zb_splat(false),
        c249: zb_splat(true),
        c282: n275,
        c283: n261,
        h1: n451, h2: n452,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n486, h2: n487,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n498, h2: n499,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n503, h2: n504,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n518, h2: n519,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n531, h2: n532,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n542, h2: n543,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n550, h2: n551,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n558, h2: n559,
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
        c241: n242,
        c248: zb_splat(true),
        c249: zb_splat(false),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n566, h2: n567,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(65536i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n586, h2: n587,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n596, h2: n597,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(327680i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        h1: n600, h2: n601,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(-327680i32)),
        h1: n614, h2: n615,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n626, h2: n627,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(-231700i32)),
        h1: n636, h2: n637,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(327680i32)),
        h1: n644, h2: n645,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(-231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n652, h2: n653,
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
        c241: n259,
        c248: zb_splat(true),
        c249: zb_splat(true),
        c282: zn_splat(P8::from_raw(231700i32)),
        c283: zn_splat(P8::from_raw(231700i32)),
        h1: n660, h2: n661,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n664, h2: n665,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: zw_splat(n668), h2: zw_splat(n669),
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_2_0 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n697, h2: n698,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o2(16, take_2_0, &sh2, &o2);
    declined |= live_v32_b28 & (if bd_v32_b28 { ALL } else { !ok_v32_b28 });
    take_2_1 |= live_v32_b28 & ok_v32_b28 & (if bd_v32_b28 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n342,
        h1: n700, h2: n701,
    };
    // body 28: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b29 & (if bd_v0_b29 { ALL } else { !ok_v0_b29 });
    take_3_0 |= live_v0_b29 & ok_v0_b29 & (if bd_v0_b29 { 0 } else { ALL });
    declined |= live_v16_b30 & (if bd_v16_b30 { ALL } else { !ok_v16_b30 });
    take_3_0 |= live_v16_b30 & ok_v16_b30 & (if bd_v16_b30 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n347,
        h1: n730, h2: n731,
    };
    // body 30: buttons 0x10, forks 0x0
    sink.o3(16, take_3_0, &sh3, &o3);
    declined |= live_v32_b31 & (if bd_v32_b31 { ALL } else { !ok_v32_b31 });
    take_3_1 |= live_v32_b31 & ok_v32_b31 & (if bd_v32_b31 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n360,
        h1: n733, h2: n734,
    };
    // body 31: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined
}
