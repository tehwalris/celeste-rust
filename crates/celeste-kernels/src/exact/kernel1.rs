// GENERATED from a TRACED frame (shape 1). Do not edit.
//
// One input shape, 4 output shapes, 53 distinct button
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
pub const SHAPE: u64 = 14202976571271913900;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c274: P8,
    pub c275: P8,
    pub c276: P8,
    pub c277: P8,
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
    ("objects[0].dash_accel.x", "num"),
    ("objects[0].dash_accel.y", "num"),
    ("objects[0].dash_effect_time", "num"),
    ("objects[0].dash_target.x", "num"),
    ("objects[0].dash_target.y", "num"),
    ("objects[0].dash_time", "num"),
    ("objects[0].djump", "num"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].grace", "num"),
    ("objects[0].p_dash", "bool"),
    ("objects[0].p_jump", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
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
    pub c268: ZN,
    pub c269: ZN,
    pub c234: ZN,
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c237: ZN,
    pub c272: u16,
    pub c273: u16,
    pub c239: ZN,
    pub c246: u16,
    pub c247: u16,
    pub c278: ZN,
    pub c279: ZN,
    pub c249: u16,
    pub c280: ZN,
    pub c281: ZN,
    pub c253: ZN,
    pub c254: ZN,
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
    pub c268: u32,
    pub c269: u32,
    pub c234: u32,
    pub c270: u32,
    pub c271: u32,
    pub c236: u32,
    pub c237: u32,
    pub c272: u32,
    pub c273: u32,
    pub c239: u32,
    pub c246: u32,
    pub c247: u32,
    pub c278: u32,
    pub c279: u32,
    pub c249: u32,
    pub c280: u32,
    pub c281: u32,
    pub c253: u32,
    pub c254: u32,
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
        c274: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c275: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c276: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c277: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c268: cell("objects[0].dash_accel.x")?,
        c269: cell("objects[0].dash_accel.y")?,
        c234: cell("objects[0].dash_effect_time")?,
        c270: cell("objects[0].dash_target.x")?,
        c271: cell("objects[0].dash_target.y")?,
        c236: cell("objects[0].dash_time")?,
        c237: cell("objects[0].djump")?,
        c272: cell("objects[0].flip.x")?,
        c273: cell("objects[0].flip.y")?,
        c239: cell("objects[0].grace")?,
        c246: cell("objects[0].p_dash")?,
        c247: cell("objects[0].p_jump")?,
        c278: cell("objects[0].rem.x")?,
        c279: cell("objects[0].rem.y")?,
        c249: cell("objects[0].solids")?,
        c280: cell("objects[0].spd.x")?,
        c281: cell("objects[0].spd.y")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
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
        c234: match &b.cols[s.c234 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c236: match &b.cols[s.c236 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c237: match &b.cols[s.c237 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
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
        c273: match &b.cols[s.c273 as usize] {
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
        c239: match &b.cols[s.c239 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c246: match &b.cols[s.c246 as usize] {
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
        c247: match &b.cols[s.c247 as usize] {
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
        c278: match &b.cols[s.c278 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c249: match &b.cols[s.c249 as usize] {
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
        c280: match &b.cols[s.c280 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c281: match &b.cols[s.c281 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c254: match &b.cols[s.c254 as usize] {
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

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
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
    pub c42: ZB,
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
    pub c88: ZN,
    pub c86: ZN,
    pub c43: ZB,
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

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c42: ZB,
    pub c88: ZN,
    pub c86: ZN,
    pub c232: ZB,
    pub c273: ZB,
    pub c274: ZN,
    pub c275: ZN,
    pub c276: ZN,
    pub c277: ZN,
    pub c278: ZN,
    pub c279: ZN,
    pub c249: ZB,
    pub c254: ZN,
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
    b.cols[88] = Col::N(Vec::new());
    b.cols[86] = Col::N(Vec::new());
    b.cols[232] = Col::U(AV::Bool(true));
    b.cols[233] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[262] = Col::U(AV::Bool(false));
    b.cols[263] = Col::U(AV::Bool(false));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Bool(false));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[156] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[43] = Col::V(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
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
    b.cols[268] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[234] = Col::N(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[271] = Col::N(Vec::new());
    b.cols[236] = Col::N(Vec::new());
    b.cols[237] = Col::N(Vec::new());
    b.cols[272] = Col::V(Vec::new());
    b.cols[273] = Col::V(Vec::new());
    b.cols[239] = Col::N(Vec::new());
    b.cols[274] = Col::N(Vec::new());
    b.cols[275] = Col::N(Vec::new());
    b.cols[276] = Col::N(Vec::new());
    b.cols[277] = Col::N(Vec::new());
    b.cols[246] = Col::V(Vec::new());
    b.cols[247] = Col::V(Vec::new());
    b.cols[278] = Col::N(Vec::new());
    b.cols[279] = Col::N(Vec::new());
    b.cols[249] = Col::V(Vec::new());
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
        if let Col::V(v) = &mut acc.cols[232] {
            v.push(if sh.c232.known & (1 << i) != 0 {
                AV::Bool(sh.c232.val & (1 << i) != 0)
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
        if let Col::V(v) = &mut acc.cols[273] {
            v.push(if sh.c273.known & (1 << i) != 0 {
                AV::Bool(sh.c273.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[239] { v.push(kv.c239.lane(i)); }
        if let Col::N(v) = &mut acc.cols[274] { v.push(sh.c274.lane(i)); }
        if let Col::N(v) = &mut acc.cols[275] { v.push(sh.c275.lane(i)); }
        if let Col::N(v) = &mut acc.cols[276] { v.push(sh.c276.lane(i)); }
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
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
        if let Col::N(v) = &mut acc.cols[278] { v.push(sh.c278.lane(i)); }
        if let Col::N(v) = &mut acc.cols[279] { v.push(sh.c279.lane(i)); }
        if let Col::V(v) = &mut acc.cols[249] {
            v.push(if sh.c249.known & (1 << i) != 0 {
                AV::Bool(sh.c249.val & (1 << i) != 0)
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
    let r_c234: ZN = rin.c234;
    let r_c236: ZN = rin.c236;
    let r_c237: ZN = rin.c237;
    let r_c239: ZN = rin.c239;
    let r_c246: ZB = ZB { val: rin.c246, known: ALL };
    let r_c247: ZB = ZB { val: rin.c247, known: ALL };
    let r_c249: ZB = ZB { val: rin.c249, known: ALL };
    let r_c253: ZN = rin.c253;
    let r_c254: ZN = rin.c254;
    let r_c268: ZN = rin.c268;
    let r_c269: ZN = rin.c269;
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZB = ZB { val: rin.c272, known: ALL };
    let r_c273: ZB = ZB { val: rin.c273, known: ALL };
    let r_c278: ZN = rin.c278;
    let r_c279: ZN = rin.c279;
    let r_c280: ZN = rin.c280;
    let r_c281: ZN = rin.c281;
    let n26: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n27: ZN = zn_rem(n26, zn_splat(P8::from_raw(1966080i32)));
    let n28: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n27);
    let n30: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n31: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n32: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n33: ZB = zn_gt(r_c39, zn_splat(P8::from_raw(0i32)));
    let n34: ZN = zn_sub(r_c39, zn_splat(P8::from_raw(65536i32)));
    let n35: ZB = zn_le(n34, zn_splat(P8::from_raw(0i32)));
    let n40: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n41: ZN = zn_rem(n40, zn_splat(P8::from_raw(3932160i32)));
    let n42: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n41);
    let n43: ZN = zsel_n(n42, n30, r_c86);
    let n44: ZN = zsel_n(n28, n43, r_c86);
    let n45: ZN = zsel_n(n28, n41, r_c85);
    let n46: ZB = zb_and(n33, r_c38);
    let n47: ZB = zb_and(n32, n46);
    let n76: ZB = zn_gt(n34, zn_splat(P8::from_raw(0i32)));
    let n79: ZB = zb_not(r_c249);
    let n82: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n83: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n82);
    let n91: ZB = zb_not(n46);
    let n92: ZB = zb_and(n32, n91);
    let n93: ZB = zb_and(n47, n76);
    let n94: ZB = zb_or(n92, n93);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n96: ZB = zb_not(n95);
    let n97: ZB = zb_and(n94, n95);
    let n98: ZB = zb_and(n94, n96);
    let n99: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c281);
    let n100: ZB = zb_not(n99);
    let n101: ZB = zb_or(n97, n98);
    let n102: ZB = zb_or(n96, n100);
    let n103: ZB = zb_not(n102);
    let n104: ZB = zb_and(n101, n102);
    let n105: ZB = zb_and(n101, n103);
    let n106: ZN = zn_add(r_c278, r_c280);
    let n107: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n106);
    let n108: ZN = zn_flr(n107);
    let n109: ZB = zb_and(r_c249, n104);
    let n110: ZB = zb_and(n79, n104);
    let n111: ZB = zn_gt(n108, zn_splat(P8::from_raw(0i32)));
    let n112: ZB = zn_le(n108, zn_splat(P8::from_raw(0i32)));
    let n113: ZB = zb_and(n109, n111);
    let n114: ZB = zb_and(n109, n112);
    let n115: ZB = zn_lt(n108, zn_splat(P8::from_raw(0i32)));
    let n116: ZB = zn_ge(n108, zn_splat(P8::from_raw(0i32)));
    let n117: ZB = zb_and(n114, n115);
    let n118: ZB = zb_and(n114, n116);
    let n119: ZN = zsel_n(n115, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n120: ZB = zb_or(n117, n118);
    let n121: ZN = zsel_n(n111, zn_splat(P8::from_raw(65536i32)), n119);
    let n122: ZB = zb_or(n113, n120);
    let n123: ZN = zn_abs(n108);
    let n124: ZN = zn_add(zn_splat(u.c276), r_c253);
    let n125: ZN = zn_add(n121, n124);
    let n126: ZN = zn_add(zn_splat(u.c277), r_c254);
    let n127: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n126);
    let n128: ZB = zn_tile_flag_at(g.cache, g.cart, n125, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n129: ZB = zb_not(n128);
    let n130: ZB = zb_and(n122, n129);
    let n131: ZB = zb_and(n122, n128);
    let n132: ZB = zb_or(n130, n131);
    let n133: ZB = zb_and(n129, n132);
    let n134: ZB = zb_and(n128, n132);
    let n135: ZB = zb_or(n133, n134);
    let n136: ZB = zb_and(n129, n135);
    let n137: ZB = zb_and(n128, n135);
    let n138: ZN = zn_add(r_c253, n121);
    let n139: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n123);
    let n140: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n123);
    let n141: ZB = zb_and(n136, n139);
    let n142: ZB = zb_and(n136, n140);
    let n143: ZN = zn_add(zn_splat(u.c276), n138);
    let n144: ZN = zn_add(n121, n143);
    let n145: ZB = zn_tile_flag_at(g.cache, g.cart, n144, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n146: ZB = zb_not(n145);
    let n147: ZB = zb_and(n141, n146);
    let n148: ZB = zb_and(n141, n145);
    let n149: ZB = zb_or(n147, n148);
    let n150: ZB = zb_and(n146, n149);
    let n151: ZB = zb_and(n145, n149);
    let n152: ZB = zb_or(n150, n151);
    let n153: ZB = zb_and(n146, n152);
    let n154: ZB = zb_and(n145, n152);
    let n155: ZN = zn_add(n121, n138);
    let n156: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n123);
    let n157: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n123);
    let n158: ZB = zb_and(n153, n156);
    let n159: ZB = zb_and(n153, n157);
    let n160: ZN = zn_add(zn_splat(u.c276), n155);
    let n161: ZN = zn_add(n121, n160);
    let n162: ZB = zn_tile_flag_at(g.cache, g.cart, n161, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n163: ZB = zb_not(n162);
    let n164: ZB = zb_and(n158, n163);
    let n165: ZB = zb_and(n158, n162);
    let n166: ZB = zb_or(n164, n165);
    let n167: ZB = zb_and(n163, n166);
    let n168: ZB = zb_and(n162, n166);
    let n169: ZB = zb_or(n167, n168);
    let n170: ZB = zb_and(n163, n169);
    let n171: ZB = zb_and(n162, n169);
    let n172: ZN = zn_add(n121, n155);
    let n173: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n123);
    let n174: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n123);
    let n175: ZB = zb_and(n170, n173);
    let n176: ZB = zb_and(n170, n174);
    let n177: ZN = zn_add(zn_splat(u.c276), n172);
    let n178: ZN = zn_add(n121, n177);
    let n179: ZB = zn_tile_flag_at(g.cache, g.cart, n178, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n180: ZB = zb_not(n179);
    let n181: ZB = zb_and(n175, n180);
    let n182: ZB = zb_and(n175, n179);
    let n183: ZB = zb_or(n181, n182);
    let n184: ZB = zb_and(n180, n183);
    let n185: ZB = zb_and(n179, n183);
    let n186: ZB = zb_or(n184, n185);
    let n187: ZB = zb_and(n180, n186);
    let n188: ZB = zb_and(n179, n186);
    let n189: ZN = zn_add(n121, n172);
    let n190: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n123);
    let n191: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n123);
    let n192: ZB = zb_and(n187, n190);
    let n193: ZB = zb_and(n187, n191);
    let n194: ZN = zn_add(zn_splat(u.c276), n189);
    let n195: ZN = zn_add(n121, n194);
    let n196: ZB = zn_tile_flag_at(g.cache, g.cart, n195, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n197: ZB = zb_not(n196);
    let n198: ZB = zb_and(n192, n197);
    let n199: ZB = zb_and(n192, n196);
    let n200: ZB = zb_or(n198, n199);
    let n201: ZB = zb_and(n197, n200);
    let n202: ZB = zb_and(n196, n200);
    let n203: ZB = zb_or(n201, n202);
    let n204: ZB = zb_and(n197, n203);
    let n205: ZB = zb_and(n196, n203);
    let n206: ZN = zn_add(n121, n189);
    let n207: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n123);
    let n208: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n123);
    let n209: ZB = zb_and(n204, n207);
    let n210: ZB = zb_and(n204, n208);
    let n211: ZN = zn_add(zn_splat(u.c276), n206);
    let n212: ZN = zn_add(n121, n211);
    let n213: ZB = zn_tile_flag_at(g.cache, g.cart, n212, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n214: ZB = zb_not(n213);
    let n215: ZB = zb_and(n209, n214);
    let n216: ZB = zb_and(n209, n213);
    let n217: ZB = zb_or(n215, n216);
    let n218: ZB = zb_and(n214, n217);
    let n219: ZB = zb_and(n213, n217);
    let n220: ZB = zb_or(n218, n219);
    let n221: ZB = zb_and(n214, n220);
    let n222: ZB = zb_and(n213, n220);
    let n223: ZN = zn_add(n121, n206);
    let n224: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n123);
    let n225: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n123);
    let n226: ZB = zb_and(n221, n224);
    let n227: ZB = zb_and(n221, n225);
    let n228: ZN = zn_add(zn_splat(u.c276), n223);
    let n229: ZN = zn_add(n121, n228);
    let n230: ZB = zn_tile_flag_at(g.cache, g.cart, n229, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n231: ZB = zb_not(n230);
    let n232: ZB = zb_and(n226, n231);
    let n233: ZB = zb_and(n226, n230);
    let n234: ZB = zb_or(n232, n233);
    let n235: ZB = zb_and(n231, n234);
    let n236: ZB = zb_and(n230, n234);
    let n237: ZB = zb_or(n235, n236);
    let n238: ZB = zb_and(n231, n237);
    let n239: ZB = zb_and(n230, n237);
    let n240: ZN = zn_add(n121, n223);
    let n241: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n123);
    let n242: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n123);
    let n243: ZB = zb_and(n238, n241);
    let n244: ZB = zb_and(n238, n242);
    let n245: ZN = zn_add(zn_splat(u.c276), n240);
    let n246: ZN = zn_add(n121, n245);
    let n247: ZB = zn_tile_flag_at(g.cache, g.cart, n246, n127, u.c275, u.c274, P8::from_raw(0i32));
    let n248: ZB = zb_not(n247);
    let n249: ZB = zb_and(n243, n248);
    let n250: ZB = zb_and(n243, n247);
    let n251: ZB = zb_or(n249, n250);
    let n252: ZB = zb_and(n248, n251);
    let n253: ZB = zb_and(n247, n251);
    let n254: ZB = zb_or(n252, n253);
    let n255: ZB = zb_and(n248, n254);
    let n256: ZB = zb_and(n247, n254);
    let n257: ZN = zn_add(n121, n240);
    let n258: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n123);
    let n259: ZN = zsel_n(n247, n240, n257);
    let n260: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), r_c280);
    let n261: ZB = zb_or(n255, n256);
    let n262: ZB = zb_or(n247, n258);
    let n263: ZN = zsel_n(n242, n240, n259);
    let n264: ZN = zsel_n(n242, r_c280, n260);
    let n265: ZB = zb_or(n244, n261);
    let n266: ZB = zb_or(n242, n262);
    let n267: ZN = zsel_n(n230, n223, n263);
    let n268: ZN = zsel_n(n230, zn_splat(P8::from_raw(0i32)), n264);
    let n269: ZB = zb_or(n239, n265);
    let n270: ZB = zb_or(n230, n266);
    let n271: ZN = zsel_n(n225, n223, n267);
    let n272: ZN = zsel_n(n225, r_c280, n268);
    let n273: ZB = zb_or(n227, n269);
    let n274: ZB = zb_or(n225, n270);
    let n275: ZN = zsel_n(n213, n206, n271);
    let n276: ZN = zsel_n(n213, zn_splat(P8::from_raw(0i32)), n272);
    let n277: ZB = zb_or(n222, n273);
    let n278: ZB = zb_or(n213, n274);
    let n279: ZN = zsel_n(n208, n206, n275);
    let n280: ZN = zsel_n(n208, r_c280, n276);
    let n281: ZB = zb_or(n210, n277);
    let n282: ZB = zb_or(n208, n278);
    let n283: ZN = zsel_n(n196, n189, n279);
    let n284: ZN = zsel_n(n196, zn_splat(P8::from_raw(0i32)), n280);
    let n285: ZB = zb_or(n205, n281);
    let n286: ZB = zb_or(n196, n282);
    let n287: ZN = zsel_n(n191, n189, n283);
    let n288: ZN = zsel_n(n191, r_c280, n284);
    let n289: ZB = zb_or(n193, n285);
    let n290: ZB = zb_or(n191, n286);
    let n291: ZN = zsel_n(n179, n172, n287);
    let n292: ZN = zsel_n(n179, zn_splat(P8::from_raw(0i32)), n288);
    let n293: ZB = zb_or(n188, n289);
    let n294: ZB = zb_or(n179, n290);
    let n295: ZN = zsel_n(n174, n172, n291);
    let n296: ZN = zsel_n(n174, r_c280, n292);
    let n297: ZB = zb_or(n176, n293);
    let n298: ZB = zb_or(n174, n294);
    let n299: ZN = zsel_n(n162, n155, n295);
    let n300: ZN = zsel_n(n162, zn_splat(P8::from_raw(0i32)), n296);
    let n301: ZB = zb_or(n171, n297);
    let n302: ZB = zb_or(n162, n298);
    let n303: ZN = zsel_n(n157, n155, n299);
    let n304: ZN = zsel_n(n157, r_c280, n300);
    let n305: ZB = zb_or(n159, n301);
    let n306: ZB = zb_or(n157, n302);
    let n307: ZN = zsel_n(n145, n138, n303);
    let n308: ZN = zsel_n(n145, zn_splat(P8::from_raw(0i32)), n304);
    let n309: ZB = zb_or(n154, n305);
    let n310: ZB = zb_or(n145, n306);
    let n311: ZN = zsel_n(n140, n138, n307);
    let n312: ZN = zsel_n(n140, r_c280, n308);
    let n313: ZB = zb_or(n142, n309);
    let n314: ZB = zb_or(n140, n310);
    let n315: ZN = zsel_n(n128, r_c253, n311);
    let n316: ZN = zsel_n(n128, zn_splat(P8::from_raw(0i32)), n312);
    let n317: ZB = zb_or(n137, n313);
    let n318: ZB = zb_or(n128, n314);
    let n319: ZN = zn_add(r_c253, n108);
    let n320: ZN = zsel_n(r_c249, n315, n319);
    let n321: ZN = zsel_n(r_c249, n316, r_c280);
    let n322: ZB = zb_or(n110, n317);
    let n323: ZB = zb_or(n79, n318);
    let n324: ZN = zn_add(r_c279, r_c281);
    let n325: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n324);
    let n326: ZN = zn_flr(n325);
    let n327: ZB = zb_and(r_c249, n322);
    let n328: ZB = zb_and(n79, n322);
    let n329: ZB = zn_gt(n326, zn_splat(P8::from_raw(0i32)));
    let n330: ZB = zn_le(n326, zn_splat(P8::from_raw(0i32)));
    let n331: ZB = zb_and(n327, n329);
    let n332: ZB = zb_and(n327, n330);
    let n333: ZB = zn_lt(n326, zn_splat(P8::from_raw(0i32)));
    let n334: ZB = zn_ge(n326, zn_splat(P8::from_raw(0i32)));
    let n335: ZB = zb_and(n332, n333);
    let n336: ZB = zb_and(n332, n334);
    let n337: ZN = zsel_n(n333, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n338: ZB = zb_or(n335, n336);
    let n339: ZN = zsel_n(n329, zn_splat(P8::from_raw(65536i32)), n337);
    let n340: ZB = zb_or(n331, n338);
    let n341: ZN = zn_abs(n326);
    let n342: ZB = zn_gt(n339, zn_splat(P8::from_raw(0i32)));
    let n343: ZB = zn_le(n339, zn_splat(P8::from_raw(0i32)));
    let n344: ZB = zb_and(n340, n342);
    let n345: ZB = zb_and(n340, n343);
    let n346: ZB = zb_or(n344, n345);
    let n347: ZB = zb_and(n342, n346);
    let n348: ZB = zb_and(n343, n346);
    let n349: ZB = zb_or(n347, n348);
    let n350: ZN = zn_add(zn_splat(u.c276), n320);
    let n351: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n350);
    let n352: ZN = zn_add(n126, n339);
    let n353: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n352, u.c275, u.c274, P8::from_raw(0i32));
    let n354: ZB = zb_not(n353);
    let n355: ZB = zb_and(n349, n354);
    let n356: ZB = zb_and(n349, n353);
    let n357: ZB = zb_or(n355, n356);
    let n358: ZB = zb_and(n354, n357);
    let n359: ZB = zb_and(n353, n357);
    let n360: ZB = zb_or(n358, n359);
    let n361: ZB = zb_and(n354, n360);
    let n362: ZB = zb_and(n353, n360);
    let n363: ZN = zn_add(r_c254, n339);
    let n364: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n341);
    let n365: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n341);
    let n366: ZB = zb_and(n361, n364);
    let n367: ZB = zb_and(n361, n365);
    let n368: ZB = zb_and(n342, n366);
    let n369: ZB = zb_and(n343, n366);
    let n370: ZB = zb_or(n368, n369);
    let n371: ZB = zb_and(n342, n370);
    let n372: ZB = zb_and(n343, n370);
    let n373: ZB = zb_or(n371, n372);
    let n374: ZN = zn_add(zn_splat(u.c277), n363);
    let n375: ZN = zn_add(n339, n374);
    let n376: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n375, u.c275, u.c274, P8::from_raw(0i32));
    let n377: ZB = zb_not(n376);
    let n378: ZB = zb_and(n373, n377);
    let n379: ZB = zb_and(n373, n376);
    let n380: ZB = zb_or(n378, n379);
    let n381: ZB = zb_and(n377, n380);
    let n382: ZB = zb_and(n376, n380);
    let n383: ZB = zb_or(n381, n382);
    let n384: ZB = zb_and(n377, n383);
    let n385: ZB = zb_and(n376, n383);
    let n386: ZN = zn_add(n339, n363);
    let n387: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n341);
    let n388: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n341);
    let n389: ZB = zb_and(n384, n387);
    let n390: ZB = zb_and(n384, n388);
    let n391: ZB = zb_and(n342, n389);
    let n392: ZB = zb_and(n343, n389);
    let n393: ZB = zb_or(n391, n392);
    let n394: ZB = zb_and(n342, n393);
    let n395: ZB = zb_and(n343, n393);
    let n396: ZB = zb_or(n394, n395);
    let n397: ZN = zn_add(zn_splat(u.c277), n386);
    let n398: ZN = zn_add(n339, n397);
    let n399: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n398, u.c275, u.c274, P8::from_raw(0i32));
    let n400: ZB = zb_not(n399);
    let n401: ZB = zb_and(n396, n400);
    let n402: ZB = zb_and(n396, n399);
    let n403: ZB = zb_or(n401, n402);
    let n404: ZB = zb_and(n400, n403);
    let n405: ZB = zb_and(n399, n403);
    let n406: ZB = zb_or(n404, n405);
    let n407: ZB = zb_and(n400, n406);
    let n408: ZB = zb_and(n399, n406);
    let n409: ZN = zn_add(n339, n386);
    let n410: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n341);
    let n411: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n341);
    let n412: ZB = zb_and(n407, n410);
    let n413: ZB = zb_and(n407, n411);
    let n414: ZB = zb_and(n342, n412);
    let n415: ZB = zb_and(n343, n412);
    let n416: ZB = zb_or(n414, n415);
    let n417: ZB = zb_and(n342, n416);
    let n418: ZB = zb_and(n343, n416);
    let n419: ZB = zb_or(n417, n418);
    let n420: ZN = zn_add(zn_splat(u.c277), n409);
    let n421: ZN = zn_add(n339, n420);
    let n422: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n421, u.c275, u.c274, P8::from_raw(0i32));
    let n423: ZB = zb_not(n422);
    let n424: ZB = zb_and(n419, n423);
    let n425: ZB = zb_and(n419, n422);
    let n426: ZB = zb_or(n424, n425);
    let n427: ZB = zb_and(n423, n426);
    let n428: ZB = zb_and(n422, n426);
    let n429: ZB = zb_or(n427, n428);
    let n430: ZB = zb_and(n423, n429);
    let n431: ZB = zb_and(n422, n429);
    let n432: ZN = zn_add(n339, n409);
    let n433: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n341);
    let n434: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n341);
    let n435: ZB = zb_and(n430, n433);
    let n436: ZB = zb_and(n430, n434);
    let n437: ZB = zb_and(n342, n435);
    let n438: ZB = zb_and(n343, n435);
    let n439: ZB = zb_or(n437, n438);
    let n440: ZB = zb_and(n342, n439);
    let n441: ZB = zb_and(n343, n439);
    let n442: ZB = zb_or(n440, n441);
    let n443: ZN = zn_add(zn_splat(u.c277), n432);
    let n444: ZN = zn_add(n339, n443);
    let n445: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n444, u.c275, u.c274, P8::from_raw(0i32));
    let n446: ZB = zb_not(n445);
    let n447: ZB = zb_and(n442, n446);
    let n448: ZB = zb_and(n442, n445);
    let n449: ZB = zb_or(n447, n448);
    let n450: ZB = zb_and(n446, n449);
    let n451: ZB = zb_and(n445, n449);
    let n452: ZB = zb_or(n450, n451);
    let n453: ZB = zb_and(n446, n452);
    let n454: ZB = zb_and(n445, n452);
    let n455: ZN = zn_add(n339, n432);
    let n456: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n341);
    let n457: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n341);
    let n458: ZB = zb_and(n453, n456);
    let n459: ZB = zb_and(n453, n457);
    let n460: ZB = zb_and(n342, n458);
    let n461: ZB = zb_and(n343, n458);
    let n462: ZB = zb_or(n460, n461);
    let n463: ZB = zb_and(n342, n462);
    let n464: ZB = zb_and(n343, n462);
    let n465: ZB = zb_or(n463, n464);
    let n466: ZN = zn_add(zn_splat(u.c277), n455);
    let n467: ZN = zn_add(n339, n466);
    let n468: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n467, u.c275, u.c274, P8::from_raw(0i32));
    let n469: ZB = zb_not(n468);
    let n470: ZB = zb_and(n465, n469);
    let n471: ZB = zb_and(n465, n468);
    let n472: ZB = zb_or(n470, n471);
    let n473: ZB = zb_and(n469, n472);
    let n474: ZB = zb_and(n468, n472);
    let n475: ZB = zb_or(n473, n474);
    let n476: ZB = zb_and(n469, n475);
    let n477: ZB = zb_and(n468, n475);
    let n478: ZN = zn_add(n339, n455);
    let n479: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n341);
    let n480: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n341);
    let n481: ZB = zb_and(n476, n479);
    let n482: ZB = zb_and(n476, n480);
    let n483: ZB = zb_and(n342, n481);
    let n484: ZB = zb_and(n343, n481);
    let n485: ZB = zb_or(n483, n484);
    let n486: ZB = zb_and(n342, n485);
    let n487: ZB = zb_and(n343, n485);
    let n488: ZB = zb_or(n486, n487);
    let n489: ZN = zn_add(zn_splat(u.c277), n478);
    let n490: ZN = zn_add(n339, n489);
    let n491: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n490, u.c275, u.c274, P8::from_raw(0i32));
    let n492: ZB = zb_not(n491);
    let n493: ZB = zb_and(n488, n492);
    let n494: ZB = zb_and(n488, n491);
    let n495: ZB = zb_or(n493, n494);
    let n496: ZB = zb_and(n492, n495);
    let n497: ZB = zb_and(n491, n495);
    let n498: ZB = zb_or(n496, n497);
    let n499: ZB = zb_and(n492, n498);
    let n500: ZB = zb_and(n491, n498);
    let n501: ZN = zn_add(n339, n478);
    let n502: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n341);
    let n503: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n341);
    let n504: ZB = zb_and(n499, n502);
    let n505: ZB = zb_and(n499, n503);
    let n506: ZB = zb_and(n342, n504);
    let n507: ZB = zb_and(n343, n504);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_and(n342, n508);
    let n510: ZB = zb_and(n343, n508);
    let n511: ZB = zb_or(n509, n510);
    let n512: ZN = zn_add(zn_splat(u.c277), n501);
    let n513: ZN = zn_add(n339, n512);
    let n514: ZB = zn_tile_flag_at(g.cache, g.cart, n351, n513, u.c275, u.c274, P8::from_raw(0i32));
    let n515: ZB = zb_not(n514);
    let n516: ZB = zb_and(n511, n515);
    let n517: ZB = zb_and(n511, n514);
    let n518: ZB = zb_or(n516, n517);
    let n519: ZB = zb_and(n515, n518);
    let n520: ZB = zb_and(n514, n518);
    let n521: ZB = zb_or(n519, n520);
    let n522: ZB = zb_and(n515, n521);
    let n523: ZB = zb_and(n514, n521);
    let n524: ZN = zn_add(n339, n501);
    let n525: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n341);
    let n526: ZB = zb_and(n323, n525);
    let n527: ZN = zsel_n(n514, n501, n524);
    let n528: ZN = zsel_n(n514, zn_splat(P8::from_raw(0i32)), r_c281);
    let n529: ZB = zb_or(n522, n523);
    let n530: ZB = zsel_b(n514, n323, n526);
    let n531: ZN = zsel_n(n503, n501, n527);
    let n532: ZN = zsel_n(n503, r_c281, n528);
    let n533: ZB = zb_or(n505, n529);
    let n534: ZB = zsel_b(n503, n323, n530);
    let n535: ZN = zsel_n(n491, n478, n531);
    let n536: ZN = zsel_n(n491, zn_splat(P8::from_raw(0i32)), n532);
    let n537: ZB = zb_or(n500, n533);
    let n538: ZB = zsel_b(n491, n323, n534);
    let n539: ZN = zsel_n(n480, n478, n535);
    let n540: ZN = zsel_n(n480, r_c281, n536);
    let n541: ZB = zb_or(n482, n537);
    let n542: ZB = zsel_b(n480, n323, n538);
    let n543: ZN = zsel_n(n468, n455, n539);
    let n544: ZN = zsel_n(n468, zn_splat(P8::from_raw(0i32)), n540);
    let n545: ZB = zb_or(n477, n541);
    let n546: ZB = zsel_b(n468, n323, n542);
    let n547: ZN = zsel_n(n457, n455, n543);
    let n548: ZN = zsel_n(n457, r_c281, n544);
    let n549: ZB = zb_or(n459, n545);
    let n550: ZB = zsel_b(n457, n323, n546);
    let n551: ZN = zsel_n(n445, n432, n547);
    let n552: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n548);
    let n553: ZB = zb_or(n454, n549);
    let n554: ZB = zsel_b(n445, n323, n550);
    let n555: ZN = zsel_n(n434, n432, n551);
    let n556: ZN = zsel_n(n434, r_c281, n552);
    let n557: ZB = zb_or(n436, n553);
    let n558: ZB = zsel_b(n434, n323, n554);
    let n559: ZN = zsel_n(n422, n409, n555);
    let n560: ZN = zsel_n(n422, zn_splat(P8::from_raw(0i32)), n556);
    let n561: ZB = zb_or(n431, n557);
    let n562: ZB = zsel_b(n422, n323, n558);
    let n563: ZN = zsel_n(n411, n409, n559);
    let n564: ZN = zsel_n(n411, r_c281, n560);
    let n565: ZB = zb_or(n413, n561);
    let n566: ZB = zsel_b(n411, n323, n562);
    let n567: ZN = zsel_n(n399, n386, n563);
    let n568: ZN = zsel_n(n399, zn_splat(P8::from_raw(0i32)), n564);
    let n569: ZB = zb_or(n408, n565);
    let n570: ZB = zsel_b(n399, n323, n566);
    let n571: ZN = zsel_n(n388, n386, n567);
    let n572: ZN = zsel_n(n388, r_c281, n568);
    let n573: ZB = zb_or(n390, n569);
    let n574: ZB = zsel_b(n388, n323, n570);
    let n575: ZN = zsel_n(n376, n363, n571);
    let n576: ZN = zsel_n(n376, zn_splat(P8::from_raw(0i32)), n572);
    let n577: ZB = zb_or(n385, n573);
    let n578: ZB = zsel_b(n376, n323, n574);
    let n579: ZN = zsel_n(n365, n363, n575);
    let n580: ZN = zsel_n(n365, r_c281, n576);
    let n581: ZB = zb_or(n367, n577);
    let n582: ZB = zsel_b(n365, n323, n578);
    let n583: ZN = zsel_n(n353, r_c254, n579);
    let n584: ZN = zsel_n(n353, zn_splat(P8::from_raw(0i32)), n580);
    let n585: ZB = zb_or(n362, n581);
    let n586: ZB = zsel_b(n353, n323, n582);
    let n587: ZN = zn_add(r_c254, n326);
    let n588: ZN = zsel_n(r_c249, n583, n587);
    let n589: ZN = zsel_n(r_c249, n584, r_c281);
    let n590: ZB = zb_or(n328, n585);
    let n591: ZB = zsel_b(r_c249, n586, n323);
    let n592: ZN = zsel_n(n102, n320, r_c253);
    let n593: ZN = zsel_n(n102, n588, r_c254);
    let n594: ZN = zsel_n(n102, n321, r_c280);
    let n595: ZN = zsel_n(n102, n589, r_c281);
    let n596: ZB = zb_or(n105, n590);
    let n597: ZB = zb_or(n103, n591);
    let n598: ZB = zb_not(r_c43);
    let n599: ZB = zb_and(n596, n598);
    let n600: ZN = zn_add(zn_splat(u.c276), n592);
    let n601: ZN = zn_add(zn_splat(u.c277), n593);
    let n602: ZN = zn_div(n600, zn_splat(P8::from_raw(524288i32)));
    let n603: ZN = zn_flr(n602);
    let n604: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n603);
    let n605: ZN = zn_add(zn_splat(u.c275), n600);
    let n606: ZN = zn_sub(n605, zn_splat(P8::from_raw(65536i32)));
    let n607: ZN = zn_div(n606, zn_splat(P8::from_raw(524288i32)));
    let n608: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n607);
    let n609: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n604);
    let n610: ZB = zn_le(n609, n608);
    let n611: ZB = zn_gt(n609, n608);
    let n612: ZB = zb_and(n599, n610);
    let n613: ZB = zb_and(n599, n611);
    let n614: ZN = zn_div(n601, zn_splat(P8::from_raw(524288i32)));
    let n615: ZN = zn_flr(n614);
    let n616: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n615);
    let n617: ZN = zn_add(zn_splat(u.c274), n601);
    let n618: ZN = zn_sub(n617, zn_splat(P8::from_raw(65536i32)));
    let n619: ZN = zn_div(n618, zn_splat(P8::from_raw(524288i32)));
    let n620: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n619);
    let n621: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n616);
    let n622: ZB = zn_le(n621, n620);
    let n623: ZB = zn_gt(n621, n620);
    let n624: ZB = zb_and(n612, n622);
    let n625: ZB = zb_and(n612, n623);
    let n626: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n609);
    let n627: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n621);
    let n628: ZN = zn_mget(g.cart, n626, n627);
    let n629: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n628);
    let n630: ZB = zb_not(n629);
    let n631: ZB = zb_and(n624, n629);
    let n632: ZB = zb_and(n624, n630);
    let n633: ZN = zn_rem(n618, zn_splat(P8::from_raw(524288i32)));
    let n634: ZB = zn_ge(n633, zn_splat(P8::from_raw(393216i32)));
    let n635: ZB = zn_lt(n633, zn_splat(P8::from_raw(393216i32)));
    let n636: ZB = zb_and(n631, n635);
    let n637: ZB = zb_and(n631, n634);
    let n638: ZN = zn_mul(n621, zn_splat(P8::from_raw(524288i32)));
    let n639: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n638);
    let n640: ZB = zn_eq(n617, n639);
    let n641: ZB = zb_or(n636, n637);
    let n642: ZB = zb_or(n634, n640);
    let n643: ZB = zb_or(n632, n641);
    let n644: ZB = zb_and(n629, n642);
    let n645: ZB = zb_not(n644);
    let n646: ZB = zb_and(n643, n644);
    let n647: ZB = zb_and(n643, n645);
    let n648: ZB = zn_ge(n595, zn_splat(P8::from_raw(0i32)));
    let n649: ZB = zb_or(n646, n647);
    let n650: ZB = zb_and(n644, n648);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n649, n650);
    let n653: ZB = zb_and(n649, n651);
    let n654: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n628);
    let n655: ZB = zb_not(n654);
    let n656: ZB = zb_and(n653, n654);
    let n657: ZB = zb_and(n653, n655);
    let n658: ZN = zn_rem(n601, zn_splat(P8::from_raw(524288i32)));
    let n659: ZB = zn_le(n658, zn_splat(P8::from_raw(131072i32)));
    let n660: ZB = zb_or(n656, n657);
    let n661: ZB = zb_and(n654, n659);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n660, n661);
    let n664: ZB = zb_and(n660, n662);
    let n665: ZB = zn_le(n595, zn_splat(P8::from_raw(0i32)));
    let n666: ZB = zb_or(n663, n664);
    let n667: ZB = zb_and(n661, n665);
    let n668: ZB = zb_not(n667);
    let n669: ZB = zb_and(n666, n667);
    let n670: ZB = zb_and(n666, n668);
    let n671: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n628);
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n670, n671);
    let n674: ZB = zb_and(n670, n672);
    let n675: ZN = zn_rem(n600, zn_splat(P8::from_raw(524288i32)));
    let n676: ZB = zn_le(n675, zn_splat(P8::from_raw(131072i32)));
    let n677: ZB = zb_or(n673, n674);
    let n678: ZB = zb_and(n671, n676);
    let n679: ZB = zb_not(n678);
    let n680: ZB = zb_and(n677, n678);
    let n681: ZB = zb_and(n677, n679);
    let n682: ZB = zn_le(n594, zn_splat(P8::from_raw(0i32)));
    let n683: ZB = zb_or(n680, n681);
    let n684: ZB = zb_and(n678, n682);
    let n685: ZB = zb_not(n684);
    let n686: ZB = zb_and(n683, n684);
    let n687: ZB = zb_and(n683, n685);
    let n688: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n628);
    let n689: ZB = zb_not(n688);
    let n690: ZB = zb_and(n687, n688);
    let n691: ZB = zb_and(n687, n689);
    let n692: ZN = zn_rem(n606, zn_splat(P8::from_raw(524288i32)));
    let n693: ZB = zn_ge(n692, zn_splat(P8::from_raw(393216i32)));
    let n694: ZB = zn_lt(n692, zn_splat(P8::from_raw(393216i32)));
    let n695: ZB = zb_and(n690, n694);
    let n696: ZB = zb_and(n690, n693);
    let n697: ZN = zn_mul(n609, zn_splat(P8::from_raw(524288i32)));
    let n698: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n697);
    let n699: ZB = zn_eq(n605, n698);
    let n700: ZB = zb_or(n695, n696);
    let n701: ZB = zb_or(n693, n699);
    let n702: ZB = zb_or(n691, n700);
    let n703: ZB = zb_and(n688, n701);
    let n704: ZB = zb_not(n703);
    let n705: ZB = zb_and(n702, n703);
    let n706: ZB = zb_and(n702, n704);
    let n707: ZB = zn_ge(n594, zn_splat(P8::from_raw(0i32)));
    let n708: ZB = zb_or(n705, n706);
    let n709: ZB = zb_and(n703, n707);
    let n710: ZB = zb_not(n709);
    let n711: ZB = zb_and(n708, n709);
    let n712: ZB = zb_and(n708, n710);
    let n713: ZB = zb_or(n686, n711);
    let n714: ZB = zb_or(n669, n713);
    let n715: ZB = zb_or(n652, n714);
    let n716: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n616);
    let n717: ZB = zn_le(n716, n620);
    let n718: ZB = zn_gt(n716, n620);
    let n719: ZB = zb_and(n712, n717);
    let n720: ZB = zb_and(n712, n718);
    let n721: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n716);
    let n722: ZN = zn_mget(g.cart, n626, n721);
    let n723: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n722);
    let n724: ZB = zb_not(n723);
    let n725: ZB = zb_and(n719, n723);
    let n726: ZB = zb_and(n719, n724);
    let n727: ZB = zb_and(n635, n725);
    let n728: ZB = zb_and(n634, n725);
    let n729: ZN = zn_mul(n716, zn_splat(P8::from_raw(524288i32)));
    let n730: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n729);
    let n731: ZB = zn_eq(n617, n730);
    let n732: ZB = zb_or(n727, n728);
    let n733: ZB = zb_or(n634, n731);
    let n734: ZB = zb_or(n726, n732);
    let n735: ZB = zb_and(n723, n733);
    let n736: ZB = zb_not(n735);
    let n737: ZB = zb_and(n734, n735);
    let n738: ZB = zb_and(n734, n736);
    let n739: ZB = zb_or(n737, n738);
    let n740: ZB = zb_and(n648, n735);
    let n741: ZB = zb_not(n740);
    let n742: ZB = zb_and(n739, n740);
    let n743: ZB = zb_and(n739, n741);
    let n744: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n722);
    let n745: ZB = zb_not(n744);
    let n746: ZB = zb_and(n743, n744);
    let n747: ZB = zb_and(n743, n745);
    let n748: ZB = zb_or(n746, n747);
    let n749: ZB = zb_and(n659, n744);
    let n750: ZB = zb_not(n749);
    let n751: ZB = zb_and(n748, n749);
    let n752: ZB = zb_and(n748, n750);
    let n753: ZB = zb_or(n751, n752);
    let n754: ZB = zb_and(n665, n749);
    let n755: ZB = zb_not(n754);
    let n756: ZB = zb_and(n753, n754);
    let n757: ZB = zb_and(n753, n755);
    let n758: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n722);
    let n759: ZB = zb_not(n758);
    let n760: ZB = zb_and(n757, n758);
    let n761: ZB = zb_and(n757, n759);
    let n762: ZB = zb_or(n760, n761);
    let n763: ZB = zb_and(n676, n758);
    let n764: ZB = zb_not(n763);
    let n765: ZB = zb_and(n762, n763);
    let n766: ZB = zb_and(n762, n764);
    let n767: ZB = zb_or(n765, n766);
    let n768: ZB = zb_and(n682, n763);
    let n769: ZB = zb_not(n768);
    let n770: ZB = zb_and(n767, n768);
    let n771: ZB = zb_and(n767, n769);
    let n772: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n722);
    let n773: ZB = zb_not(n772);
    let n774: ZB = zb_and(n771, n772);
    let n775: ZB = zb_and(n771, n773);
    let n776: ZB = zb_and(n694, n774);
    let n777: ZB = zb_and(n693, n774);
    let n778: ZB = zb_or(n776, n777);
    let n779: ZB = zb_or(n775, n778);
    let n780: ZB = zb_and(n701, n772);
    let n781: ZB = zb_not(n780);
    let n782: ZB = zb_and(n779, n780);
    let n783: ZB = zb_and(n779, n781);
    let n784: ZB = zb_or(n782, n783);
    let n785: ZB = zb_and(n707, n780);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n784, n785);
    let n788: ZB = zb_and(n784, n786);
    let n789: ZB = zb_or(n770, n787);
    let n790: ZB = zb_or(n756, n789);
    let n791: ZB = zb_or(n742, n790);
    let n792: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n616);
    let n793: ZB = zn_le(n792, n620);
    let n794: ZB = zn_gt(n792, n620);
    let n795: ZB = zb_and(n788, n793);
    let n796: ZB = zb_and(n788, n794);
    let n797: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n792);
    let n798: ZN = zn_mget(g.cart, n626, n797);
    let n799: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n798);
    let n800: ZB = zb_not(n799);
    let n801: ZB = zb_and(n795, n799);
    let n802: ZB = zb_and(n795, n800);
    let n803: ZB = zb_and(n635, n801);
    let n804: ZB = zb_and(n634, n801);
    let n805: ZN = zn_mul(n792, zn_splat(P8::from_raw(524288i32)));
    let n806: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n805);
    let n807: ZB = zn_eq(n617, n806);
    let n808: ZB = zb_or(n803, n804);
    let n809: ZB = zb_or(n634, n807);
    let n810: ZB = zb_or(n802, n808);
    let n811: ZB = zb_and(n799, n809);
    let n812: ZB = zb_not(n811);
    let n813: ZB = zb_and(n810, n811);
    let n814: ZB = zb_and(n810, n812);
    let n815: ZB = zb_or(n813, n814);
    let n816: ZB = zb_and(n648, n811);
    let n817: ZB = zb_not(n816);
    let n818: ZB = zb_and(n815, n816);
    let n819: ZB = zb_and(n815, n817);
    let n820: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n798);
    let n821: ZB = zb_not(n820);
    let n822: ZB = zb_and(n819, n820);
    let n823: ZB = zb_and(n819, n821);
    let n824: ZB = zb_or(n822, n823);
    let n825: ZB = zb_and(n659, n820);
    let n826: ZB = zb_not(n825);
    let n827: ZB = zb_and(n824, n825);
    let n828: ZB = zb_and(n824, n826);
    let n829: ZB = zb_or(n827, n828);
    let n830: ZB = zb_and(n665, n825);
    let n831: ZB = zb_not(n830);
    let n832: ZB = zb_and(n829, n830);
    let n833: ZB = zb_and(n829, n831);
    let n834: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n798);
    let n835: ZB = zb_not(n834);
    let n836: ZB = zb_and(n833, n834);
    let n837: ZB = zb_and(n833, n835);
    let n838: ZB = zb_or(n836, n837);
    let n839: ZB = zb_and(n676, n834);
    let n840: ZB = zb_not(n839);
    let n841: ZB = zb_and(n838, n839);
    let n842: ZB = zb_and(n838, n840);
    let n843: ZB = zb_or(n841, n842);
    let n844: ZB = zb_and(n682, n839);
    let n845: ZB = zb_not(n844);
    let n846: ZB = zb_and(n843, n844);
    let n847: ZB = zb_and(n843, n845);
    let n848: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n798);
    let n849: ZB = zb_not(n848);
    let n850: ZB = zb_and(n847, n848);
    let n851: ZB = zb_and(n847, n849);
    let n852: ZB = zb_and(n694, n850);
    let n853: ZB = zb_and(n693, n850);
    let n854: ZB = zb_or(n852, n853);
    let n855: ZB = zb_or(n851, n854);
    let n856: ZB = zb_and(n701, n848);
    let n857: ZB = zb_not(n856);
    let n858: ZB = zb_and(n855, n856);
    let n859: ZB = zb_and(n855, n857);
    let n860: ZB = zb_or(n858, n859);
    let n861: ZB = zb_and(n707, n856);
    let n862: ZB = zb_not(n861);
    let n863: ZB = zb_and(n860, n861);
    let n864: ZB = zb_and(n860, n862);
    let n865: ZB = zb_or(n846, n863);
    let n866: ZB = zb_or(n832, n865);
    let n867: ZB = zb_or(n818, n866);
    let n868: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n616);
    let n869: ZB = zn_gt(n868, n620);
    let n870: ZB = zb_and(n597, n869);
    let n871: ZB = zb_or(n796, n864);
    let n872: ZB = zsel_b(n794, n597, n870);
    let n873: ZB = zb_or(n791, n867);
    let n874: ZB = zb_or(n720, n871);
    let n875: ZB = zsel_b(n718, n597, n872);
    let n876: ZB = zb_or(n715, n873);
    let n877: ZB = zb_or(n625, n874);
    let n878: ZB = zsel_b(n623, n597, n875);
    let n879: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n604);
    let n880: ZB = zn_le(n879, n608);
    let n881: ZB = zn_gt(n879, n608);
    let n882: ZB = zb_and(n877, n880);
    let n883: ZB = zb_and(n877, n881);
    let n884: ZB = zb_and(n622, n882);
    let n885: ZB = zb_and(n623, n882);
    let n886: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n879);
    let n887: ZN = zn_mget(g.cart, n886, n627);
    let n888: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n887);
    let n889: ZB = zb_not(n888);
    let n890: ZB = zb_and(n884, n888);
    let n891: ZB = zb_and(n884, n889);
    let n892: ZB = zb_and(n635, n890);
    let n893: ZB = zb_and(n634, n890);
    let n894: ZB = zb_or(n892, n893);
    let n895: ZB = zb_or(n891, n894);
    let n896: ZB = zb_and(n642, n888);
    let n897: ZB = zb_not(n896);
    let n898: ZB = zb_and(n895, n896);
    let n899: ZB = zb_and(n895, n897);
    let n900: ZB = zb_or(n898, n899);
    let n901: ZB = zb_and(n648, n896);
    let n902: ZB = zb_not(n901);
    let n903: ZB = zb_and(n900, n901);
    let n904: ZB = zb_and(n900, n902);
    let n905: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n887);
    let n906: ZB = zb_not(n905);
    let n907: ZB = zb_and(n904, n905);
    let n908: ZB = zb_and(n904, n906);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_and(n659, n905);
    let n911: ZB = zb_not(n910);
    let n912: ZB = zb_and(n909, n910);
    let n913: ZB = zb_and(n909, n911);
    let n914: ZB = zb_or(n912, n913);
    let n915: ZB = zb_and(n665, n910);
    let n916: ZB = zb_not(n915);
    let n917: ZB = zb_and(n914, n915);
    let n918: ZB = zb_and(n914, n916);
    let n919: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n887);
    let n920: ZB = zb_not(n919);
    let n921: ZB = zb_and(n918, n919);
    let n922: ZB = zb_and(n918, n920);
    let n923: ZB = zb_or(n921, n922);
    let n924: ZB = zb_and(n676, n919);
    let n925: ZB = zb_not(n924);
    let n926: ZB = zb_and(n923, n924);
    let n927: ZB = zb_and(n923, n925);
    let n928: ZB = zb_or(n926, n927);
    let n929: ZB = zb_and(n682, n924);
    let n930: ZB = zb_not(n929);
    let n931: ZB = zb_and(n928, n929);
    let n932: ZB = zb_and(n928, n930);
    let n933: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n887);
    let n934: ZB = zb_not(n933);
    let n935: ZB = zb_and(n932, n933);
    let n936: ZB = zb_and(n932, n934);
    let n937: ZB = zb_and(n694, n935);
    let n938: ZB = zb_and(n693, n935);
    let n939: ZN = zn_mul(n879, zn_splat(P8::from_raw(524288i32)));
    let n940: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n939);
    let n941: ZB = zn_eq(n605, n940);
    let n942: ZB = zb_or(n937, n938);
    let n943: ZB = zb_or(n693, n941);
    let n944: ZB = zb_or(n936, n942);
    let n945: ZB = zb_and(n933, n943);
    let n946: ZB = zb_not(n945);
    let n947: ZB = zb_and(n944, n945);
    let n948: ZB = zb_and(n944, n946);
    let n949: ZB = zb_or(n947, n948);
    let n950: ZB = zb_and(n707, n945);
    let n951: ZB = zb_not(n950);
    let n952: ZB = zb_and(n949, n950);
    let n953: ZB = zb_and(n949, n951);
    let n954: ZB = zb_or(n931, n952);
    let n955: ZB = zb_or(n917, n954);
    let n956: ZB = zb_or(n903, n955);
    let n957: ZB = zb_and(n717, n953);
    let n958: ZB = zb_and(n718, n953);
    let n959: ZN = zn_mget(g.cart, n886, n721);
    let n960: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n959);
    let n961: ZB = zb_not(n960);
    let n962: ZB = zb_and(n957, n960);
    let n963: ZB = zb_and(n957, n961);
    let n964: ZB = zb_and(n635, n962);
    let n965: ZB = zb_and(n634, n962);
    let n966: ZB = zb_or(n964, n965);
    let n967: ZB = zb_or(n963, n966);
    let n968: ZB = zb_and(n733, n960);
    let n969: ZB = zb_not(n968);
    let n970: ZB = zb_and(n967, n968);
    let n971: ZB = zb_and(n967, n969);
    let n972: ZB = zb_or(n970, n971);
    let n973: ZB = zb_and(n648, n968);
    let n974: ZB = zb_not(n973);
    let n975: ZB = zb_and(n972, n973);
    let n976: ZB = zb_and(n972, n974);
    let n977: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n959);
    let n978: ZB = zb_not(n977);
    let n979: ZB = zb_and(n976, n977);
    let n980: ZB = zb_and(n976, n978);
    let n981: ZB = zb_or(n979, n980);
    let n982: ZB = zb_and(n659, n977);
    let n983: ZB = zb_not(n982);
    let n984: ZB = zb_and(n981, n982);
    let n985: ZB = zb_and(n981, n983);
    let n986: ZB = zb_or(n984, n985);
    let n987: ZB = zb_and(n665, n982);
    let n988: ZB = zb_not(n987);
    let n989: ZB = zb_and(n986, n987);
    let n990: ZB = zb_and(n986, n988);
    let n991: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n959);
    let n992: ZB = zb_not(n991);
    let n993: ZB = zb_and(n990, n991);
    let n994: ZB = zb_and(n990, n992);
    let n995: ZB = zb_or(n993, n994);
    let n996: ZB = zb_and(n676, n991);
    let n997: ZB = zb_not(n996);
    let n998: ZB = zb_and(n995, n996);
    let n999: ZB = zb_and(n995, n997);
    let n1000: ZB = zb_or(n998, n999);
    let n1001: ZB = zb_and(n682, n996);
    let n1002: ZB = zb_not(n1001);
    let n1003: ZB = zb_and(n1000, n1001);
    let n1004: ZB = zb_and(n1000, n1002);
    let n1005: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n959);
    let n1006: ZB = zb_not(n1005);
    let n1007: ZB = zb_and(n1004, n1005);
    let n1008: ZB = zb_and(n1004, n1006);
    let n1009: ZB = zb_and(n694, n1007);
    let n1010: ZB = zb_and(n693, n1007);
    let n1011: ZB = zb_or(n1009, n1010);
    let n1012: ZB = zb_or(n1008, n1011);
    let n1013: ZB = zb_and(n943, n1005);
    let n1014: ZB = zb_not(n1013);
    let n1015: ZB = zb_and(n1012, n1013);
    let n1016: ZB = zb_and(n1012, n1014);
    let n1017: ZB = zb_or(n1015, n1016);
    let n1018: ZB = zb_and(n707, n1013);
    let n1019: ZB = zb_not(n1018);
    let n1020: ZB = zb_and(n1017, n1018);
    let n1021: ZB = zb_and(n1017, n1019);
    let n1022: ZB = zb_or(n1003, n1020);
    let n1023: ZB = zb_or(n989, n1022);
    let n1024: ZB = zb_or(n975, n1023);
    let n1025: ZB = zb_and(n793, n1021);
    let n1026: ZB = zb_and(n794, n1021);
    let n1027: ZN = zn_mget(g.cart, n886, n797);
    let n1028: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1027);
    let n1029: ZB = zb_not(n1028);
    let n1030: ZB = zb_and(n1025, n1028);
    let n1031: ZB = zb_and(n1025, n1029);
    let n1032: ZB = zb_and(n635, n1030);
    let n1033: ZB = zb_and(n634, n1030);
    let n1034: ZB = zb_or(n1032, n1033);
    let n1035: ZB = zb_or(n1031, n1034);
    let n1036: ZB = zb_and(n809, n1028);
    let n1037: ZB = zb_not(n1036);
    let n1038: ZB = zb_and(n1035, n1036);
    let n1039: ZB = zb_and(n1035, n1037);
    let n1040: ZB = zb_or(n1038, n1039);
    let n1041: ZB = zb_and(n648, n1036);
    let n1042: ZB = zb_not(n1041);
    let n1043: ZB = zb_and(n1040, n1041);
    let n1044: ZB = zb_and(n1040, n1042);
    let n1045: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1027);
    let n1046: ZB = zb_not(n1045);
    let n1047: ZB = zb_and(n1044, n1045);
    let n1048: ZB = zb_and(n1044, n1046);
    let n1049: ZB = zb_or(n1047, n1048);
    let n1050: ZB = zb_and(n659, n1045);
    let n1051: ZB = zb_not(n1050);
    let n1052: ZB = zb_and(n1049, n1050);
    let n1053: ZB = zb_and(n1049, n1051);
    let n1054: ZB = zb_or(n1052, n1053);
    let n1055: ZB = zb_and(n665, n1050);
    let n1056: ZB = zb_not(n1055);
    let n1057: ZB = zb_and(n1054, n1055);
    let n1058: ZB = zb_and(n1054, n1056);
    let n1059: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1027);
    let n1060: ZB = zb_not(n1059);
    let n1061: ZB = zb_and(n1058, n1059);
    let n1062: ZB = zb_and(n1058, n1060);
    let n1063: ZB = zb_or(n1061, n1062);
    let n1064: ZB = zb_and(n676, n1059);
    let n1065: ZB = zb_not(n1064);
    let n1066: ZB = zb_and(n1063, n1064);
    let n1067: ZB = zb_and(n1063, n1065);
    let n1068: ZB = zb_or(n1066, n1067);
    let n1069: ZB = zb_and(n682, n1064);
    let n1070: ZB = zb_not(n1069);
    let n1071: ZB = zb_and(n1068, n1069);
    let n1072: ZB = zb_and(n1068, n1070);
    let n1073: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1027);
    let n1074: ZB = zb_not(n1073);
    let n1075: ZB = zb_and(n1072, n1073);
    let n1076: ZB = zb_and(n1072, n1074);
    let n1077: ZB = zb_and(n694, n1075);
    let n1078: ZB = zb_and(n693, n1075);
    let n1079: ZB = zb_or(n1077, n1078);
    let n1080: ZB = zb_or(n1076, n1079);
    let n1081: ZB = zb_and(n943, n1073);
    let n1082: ZB = zb_not(n1081);
    let n1083: ZB = zb_and(n1080, n1081);
    let n1084: ZB = zb_and(n1080, n1082);
    let n1085: ZB = zb_or(n1083, n1084);
    let n1086: ZB = zb_and(n707, n1081);
    let n1087: ZB = zb_not(n1086);
    let n1088: ZB = zb_and(n1085, n1086);
    let n1089: ZB = zb_and(n1085, n1087);
    let n1090: ZB = zb_or(n1071, n1088);
    let n1091: ZB = zb_or(n1057, n1090);
    let n1092: ZB = zb_or(n1043, n1091);
    let n1093: ZB = zb_and(n869, n878);
    let n1094: ZB = zb_or(n1026, n1089);
    let n1095: ZB = zsel_b(n794, n878, n1093);
    let n1096: ZB = zb_or(n1024, n1092);
    let n1097: ZB = zb_or(n958, n1094);
    let n1098: ZB = zsel_b(n718, n878, n1095);
    let n1099: ZB = zb_or(n956, n1096);
    let n1100: ZB = zb_or(n885, n1097);
    let n1101: ZB = zsel_b(n623, n878, n1098);
    let n1102: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n604);
    let n1103: ZB = zn_le(n1102, n608);
    let n1104: ZB = zn_gt(n1102, n608);
    let n1105: ZB = zb_and(n1100, n1103);
    let n1106: ZB = zb_and(n1100, n1104);
    let n1107: ZB = zb_and(n622, n1105);
    let n1108: ZB = zb_and(n623, n1105);
    let n1109: ZN = zn_add(zn_splat(P8::from_raw(1048576i32)), n1102);
    let n1110: ZN = zn_mget(g.cart, n1109, n627);
    let n1111: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1110);
    let n1112: ZB = zb_not(n1111);
    let n1113: ZB = zb_and(n1107, n1111);
    let n1114: ZB = zb_and(n1107, n1112);
    let n1115: ZB = zb_and(n635, n1113);
    let n1116: ZB = zb_and(n634, n1113);
    let n1117: ZB = zb_or(n1115, n1116);
    let n1118: ZB = zb_or(n1114, n1117);
    let n1119: ZB = zb_and(n642, n1111);
    let n1120: ZB = zb_not(n1119);
    let n1121: ZB = zb_and(n1118, n1119);
    let n1122: ZB = zb_and(n1118, n1120);
    let n1123: ZB = zb_or(n1121, n1122);
    let n1124: ZB = zb_and(n648, n1119);
    let n1125: ZB = zb_not(n1124);
    let n1126: ZB = zb_and(n1123, n1124);
    let n1127: ZB = zb_and(n1123, n1125);
    let n1128: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1110);
    let n1129: ZB = zb_not(n1128);
    let n1130: ZB = zb_and(n1127, n1128);
    let n1131: ZB = zb_and(n1127, n1129);
    let n1132: ZB = zb_or(n1130, n1131);
    let n1133: ZB = zb_and(n659, n1128);
    let n1134: ZB = zb_not(n1133);
    let n1135: ZB = zb_and(n1132, n1133);
    let n1136: ZB = zb_and(n1132, n1134);
    let n1137: ZB = zb_or(n1135, n1136);
    let n1138: ZB = zb_and(n665, n1133);
    let n1139: ZB = zb_not(n1138);
    let n1140: ZB = zb_and(n1137, n1138);
    let n1141: ZB = zb_and(n1137, n1139);
    let n1142: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1110);
    let n1143: ZB = zb_not(n1142);
    let n1144: ZB = zb_and(n1141, n1142);
    let n1145: ZB = zb_and(n1141, n1143);
    let n1146: ZB = zb_or(n1144, n1145);
    let n1147: ZB = zb_and(n676, n1142);
    let n1148: ZB = zb_not(n1147);
    let n1149: ZB = zb_and(n1146, n1147);
    let n1150: ZB = zb_and(n1146, n1148);
    let n1151: ZB = zb_or(n1149, n1150);
    let n1152: ZB = zb_and(n682, n1147);
    let n1153: ZB = zb_not(n1152);
    let n1154: ZB = zb_and(n1151, n1152);
    let n1155: ZB = zb_and(n1151, n1153);
    let n1156: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1110);
    let n1157: ZB = zb_not(n1156);
    let n1158: ZB = zb_and(n1155, n1156);
    let n1159: ZB = zb_and(n1155, n1157);
    let n1160: ZB = zb_and(n694, n1158);
    let n1161: ZB = zb_and(n693, n1158);
    let n1162: ZN = zn_mul(n1102, zn_splat(P8::from_raw(524288i32)));
    let n1163: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1162);
    let n1164: ZB = zn_eq(n605, n1163);
    let n1165: ZB = zb_or(n1160, n1161);
    let n1166: ZB = zb_or(n693, n1164);
    let n1167: ZB = zb_or(n1159, n1165);
    let n1168: ZB = zb_and(n1156, n1166);
    let n1169: ZB = zb_not(n1168);
    let n1170: ZB = zb_and(n1167, n1168);
    let n1171: ZB = zb_and(n1167, n1169);
    let n1172: ZB = zb_or(n1170, n1171);
    let n1173: ZB = zb_and(n707, n1168);
    let n1174: ZB = zb_not(n1173);
    let n1175: ZB = zb_and(n1172, n1173);
    let n1176: ZB = zb_and(n1172, n1174);
    let n1177: ZB = zb_or(n1154, n1175);
    let n1178: ZB = zb_or(n1140, n1177);
    let n1179: ZB = zb_or(n1126, n1178);
    let n1180: ZB = zb_and(n717, n1176);
    let n1181: ZB = zb_and(n718, n1176);
    let n1182: ZN = zn_mget(g.cart, n1109, n721);
    let n1183: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1182);
    let n1184: ZB = zb_not(n1183);
    let n1185: ZB = zb_and(n1180, n1183);
    let n1186: ZB = zb_and(n1180, n1184);
    let n1187: ZB = zb_and(n635, n1185);
    let n1188: ZB = zb_and(n634, n1185);
    let n1189: ZB = zb_or(n1187, n1188);
    let n1190: ZB = zb_or(n1186, n1189);
    let n1191: ZB = zb_and(n733, n1183);
    let n1192: ZB = zb_not(n1191);
    let n1193: ZB = zb_and(n1190, n1191);
    let n1194: ZB = zb_and(n1190, n1192);
    let n1195: ZB = zb_or(n1193, n1194);
    let n1196: ZB = zb_and(n648, n1191);
    let n1197: ZB = zb_not(n1196);
    let n1198: ZB = zb_and(n1195, n1196);
    let n1199: ZB = zb_and(n1195, n1197);
    let n1200: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1182);
    let n1201: ZB = zb_not(n1200);
    let n1202: ZB = zb_and(n1199, n1200);
    let n1203: ZB = zb_and(n1199, n1201);
    let n1204: ZB = zb_or(n1202, n1203);
    let n1205: ZB = zb_and(n659, n1200);
    let n1206: ZB = zb_not(n1205);
    let n1207: ZB = zb_and(n1204, n1205);
    let n1208: ZB = zb_and(n1204, n1206);
    let n1209: ZB = zb_or(n1207, n1208);
    let n1210: ZB = zb_and(n665, n1205);
    let n1211: ZB = zb_not(n1210);
    let n1212: ZB = zb_and(n1209, n1210);
    let n1213: ZB = zb_and(n1209, n1211);
    let n1214: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1182);
    let n1215: ZB = zb_not(n1214);
    let n1216: ZB = zb_and(n1213, n1214);
    let n1217: ZB = zb_and(n1213, n1215);
    let n1218: ZB = zb_or(n1216, n1217);
    let n1219: ZB = zb_and(n676, n1214);
    let n1220: ZB = zb_not(n1219);
    let n1221: ZB = zb_and(n1218, n1219);
    let n1222: ZB = zb_and(n1218, n1220);
    let n1223: ZB = zb_or(n1221, n1222);
    let n1224: ZB = zb_and(n682, n1219);
    let n1225: ZB = zb_not(n1224);
    let n1226: ZB = zb_and(n1223, n1224);
    let n1227: ZB = zb_and(n1223, n1225);
    let n1228: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1182);
    let n1229: ZB = zb_not(n1228);
    let n1230: ZB = zb_and(n1227, n1228);
    let n1231: ZB = zb_and(n1227, n1229);
    let n1232: ZB = zb_and(n694, n1230);
    let n1233: ZB = zb_and(n693, n1230);
    let n1234: ZB = zb_or(n1232, n1233);
    let n1235: ZB = zb_or(n1231, n1234);
    let n1236: ZB = zb_and(n1166, n1228);
    let n1237: ZB = zb_not(n1236);
    let n1238: ZB = zb_and(n1235, n1236);
    let n1239: ZB = zb_and(n1235, n1237);
    let n1240: ZB = zb_or(n1238, n1239);
    let n1241: ZB = zb_and(n707, n1236);
    let n1242: ZB = zb_not(n1241);
    let n1243: ZB = zb_and(n1240, n1241);
    let n1244: ZB = zb_and(n1240, n1242);
    let n1245: ZB = zb_or(n1226, n1243);
    let n1246: ZB = zb_or(n1212, n1245);
    let n1247: ZB = zb_or(n1198, n1246);
    let n1248: ZB = zb_and(n793, n1244);
    let n1249: ZB = zb_and(n794, n1244);
    let n1250: ZN = zn_mget(g.cart, n1109, n797);
    let n1251: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1250);
    let n1252: ZB = zb_not(n1251);
    let n1253: ZB = zb_and(n1248, n1251);
    let n1254: ZB = zb_and(n1248, n1252);
    let n1255: ZB = zb_and(n635, n1253);
    let n1256: ZB = zb_and(n634, n1253);
    let n1257: ZB = zb_or(n1255, n1256);
    let n1258: ZB = zb_or(n1254, n1257);
    let n1259: ZB = zb_and(n809, n1251);
    let n1260: ZB = zb_not(n1259);
    let n1261: ZB = zb_and(n1258, n1259);
    let n1262: ZB = zb_and(n1258, n1260);
    let n1263: ZB = zb_or(n1261, n1262);
    let n1264: ZB = zb_and(n648, n1259);
    let n1265: ZB = zb_not(n1264);
    let n1266: ZB = zb_and(n1263, n1264);
    let n1267: ZB = zb_and(n1263, n1265);
    let n1268: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1250);
    let n1269: ZB = zb_not(n1268);
    let n1270: ZB = zb_and(n1267, n1268);
    let n1271: ZB = zb_and(n1267, n1269);
    let n1272: ZB = zb_or(n1270, n1271);
    let n1273: ZB = zb_and(n659, n1268);
    let n1274: ZB = zb_not(n1273);
    let n1275: ZB = zb_and(n1272, n1273);
    let n1276: ZB = zb_and(n1272, n1274);
    let n1277: ZB = zb_or(n1275, n1276);
    let n1278: ZB = zb_and(n665, n1273);
    let n1279: ZB = zb_not(n1278);
    let n1280: ZB = zb_and(n1277, n1278);
    let n1281: ZB = zb_and(n1277, n1279);
    let n1282: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1250);
    let n1283: ZB = zb_not(n1282);
    let n1284: ZB = zb_and(n1281, n1282);
    let n1285: ZB = zb_and(n1281, n1283);
    let n1286: ZB = zb_or(n1284, n1285);
    let n1287: ZB = zb_and(n676, n1282);
    let n1288: ZB = zb_not(n1287);
    let n1289: ZB = zb_and(n1286, n1287);
    let n1290: ZB = zb_and(n1286, n1288);
    let n1291: ZB = zb_or(n1289, n1290);
    let n1292: ZB = zb_and(n682, n1287);
    let n1293: ZB = zb_not(n1292);
    let n1294: ZB = zb_and(n1291, n1292);
    let n1295: ZB = zb_and(n1291, n1293);
    let n1296: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1250);
    let n1297: ZB = zb_not(n1296);
    let n1298: ZB = zb_and(n1295, n1296);
    let n1299: ZB = zb_and(n1295, n1297);
    let n1300: ZB = zb_and(n694, n1298);
    let n1301: ZB = zb_and(n693, n1298);
    let n1302: ZB = zb_or(n1300, n1301);
    let n1303: ZB = zb_or(n1299, n1302);
    let n1304: ZB = zb_and(n1166, n1296);
    let n1305: ZB = zb_not(n1304);
    let n1306: ZB = zb_and(n1303, n1304);
    let n1307: ZB = zb_and(n1303, n1305);
    let n1308: ZB = zb_or(n1306, n1307);
    let n1309: ZB = zb_and(n707, n1304);
    let n1310: ZB = zb_not(n1309);
    let n1311: ZB = zb_and(n1308, n1309);
    let n1312: ZB = zb_and(n1308, n1310);
    let n1313: ZB = zb_or(n1294, n1311);
    let n1314: ZB = zb_or(n1280, n1313);
    let n1315: ZB = zb_or(n1266, n1314);
    let n1316: ZB = zb_and(n869, n1101);
    let n1317: ZB = zb_or(n1249, n1312);
    let n1318: ZB = zsel_b(n794, n1101, n1316);
    let n1319: ZB = zb_or(n1247, n1315);
    let n1320: ZB = zb_or(n1181, n1317);
    let n1321: ZB = zsel_b(n718, n1101, n1318);
    let n1322: ZB = zb_or(n1179, n1319);
    let n1323: ZB = zb_or(n1108, n1320);
    let n1324: ZB = zsel_b(n623, n1101, n1321);
    let n1325: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n604);
    let n1326: ZB = zn_gt(n1325, n608);
    let n1327: ZB = zb_and(n1324, n1326);
    let n1328: ZB = zb_or(n1099, n1322);
    let n1329: ZB = zsel_b(n1099, n878, n1101);
    let n1330: ZB = zb_or(n1106, n1323);
    let n1331: ZB = zsel_b(n1104, n1101, n1327);
    let n1332: ZB = zb_or(n876, n1328);
    let n1333: ZB = zsel_b(n876, n597, n1329);
    let n1334: ZB = zb_or(n883, n1330);
    let n1335: ZB = zsel_b(n881, n878, n1331);
    let n1336: ZB = zb_or(n613, n1334);
    let n1337: ZB = zsel_b(n611, n597, n1335);
    let n1338: ZB = zn_gt(n593, zn_splat(P8::from_raw(8388608i32)));
    let n1339: ZB = zn_le(n593, zn_splat(P8::from_raw(8388608i32)));
    let n1340: ZB = zb_and(n1332, n1338);
    let n1341: ZB = zb_and(n1332, n1339);
    let n1342: ZN = zsel_n(n1338, n83, n82);
    let n1343: ZB = zb_or(n1340, n1341);
    let n1344: ZB = zb_and(n1336, n1338);
    let n1345: ZN = zsel_n(n1343, n1342, n82);
    let n1346: ZB = zb_or(n1343, n1344);
    let n1347: ZB = zsel_b(n1343, n1333, n1337);
    let n1348: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n600);
    let n1349: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n601);
    let n1350: ZB = zn_tile_flag_at(g.cache, g.cart, n1348, n1349, u.c275, u.c274, P8::from_raw(0i32));
    let n1351: ZB = zb_not(n1350);
    let n1352: ZB = zb_and(n1346, n1351);
    let n1353: ZB = zb_and(n1346, n1350);
    let n1354: ZB = zb_or(n1352, n1353);
    let n1355: ZB = zb_and(n1351, n1354);
    let n1356: ZB = zb_and(n1350, n1354);
    let n1357: ZB = zb_or(n1355, n1356);
    let n1358: ZB = zb_not(r_c247);
    let n1359: ZB = zb_not(r_c246);
    let n1360: ZB = zn_lt(r_c237, r_c88);
    let n1361: ZB = zn_ge(r_c237, r_c88);
    let n1362: ZN = zsel_n(n1360, r_c88, r_c237);
    let n1363: ZB = zn_gt(r_c239, zn_splat(P8::from_raw(0i32)));
    let n1364: ZB = zn_le(r_c239, zn_splat(P8::from_raw(0i32)));
    let n1365: ZN = zn_sub(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n1366: ZN = zsel_n(n1363, n1365, r_c239);
    let n1367: ZN = zsel_n(n1350, n1362, r_c237);
    let n1368: ZN = zsel_n(n1350, zn_splat(P8::from_raw(393216i32)), n1366);
    let n1369: ZB = zb_and(n1350, n1357);
    let n1370: ZB = zb_and(n1351, n1357);
    let n1371: ZB = zb_and(n1360, n1369);
    let n1372: ZB = zb_and(n1361, n1369);
    let n1373: ZB = zb_or(n1371, n1372);
    let n1374: ZB = zb_and(n1363, n1370);
    let n1375: ZB = zb_and(n1364, n1370);
    let n1376: ZB = zb_or(n1374, n1375);
    let n1377: ZB = zb_or(n1373, n1376);
    let n1378: ZB = zn_gt(r_c236, zn_splat(P8::from_raw(0i32)));
    let n1379: ZB = zn_le(r_c236, zn_splat(P8::from_raw(0i32)));
    let n1380: ZB = zn_gt(n594, r_c270);
    let n1381: ZB = zn_le(n594, r_c270);
    let n1382: ZB = zn_gt(n595, r_c271);
    let n1383: ZB = zn_le(n595, r_c271);
    let n1384: ZN = zsel_n(n1351, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1385: ZN = zn_abs(n594);
    let n1386: ZB = zn_gt(n1385, zn_splat(P8::from_raw(65536i32)));
    let n1387: ZB = zn_le(n1385, zn_splat(P8::from_raw(65536i32)));
    let n1388: ZB = zn_gt(n594, zn_splat(P8::from_raw(0i32)));
    let n1389: ZB = zn_lt(n594, zn_splat(P8::from_raw(0i32)));
    let n1390: ZB = zn_gt(n594, zn_splat(P8::from_raw(65536i32)));
    let n1391: ZB = zn_le(n594, zn_splat(P8::from_raw(65536i32)));
    let n1392: ZN = zn_sub(n594, zn_splat(P8::from_raw(9830i32)));
    let n1393: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1392);
    let n1394: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n594);
    let n1395: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1394);
    let n1396: ZB = zn_gt(n594, zn_splat(P8::from_raw(-65536i32)));
    let n1397: ZB = zn_le(n594, zn_splat(P8::from_raw(-65536i32)));
    let n1398: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1392);
    let n1399: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1394);
    let n1400: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1392);
    let n1401: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1394);
    let n1402: ZN = zsel_n(n1396, n1398, n1399);
    let n1403: ZN = zsel_n(n1388, n1400, n1401);
    let n1404: ZN = zsel_n(n1390, n1393, n1395);
    let n1405: ZN = zsel_n(n1389, n1402, n1403);
    let n1406: ZN = zsel_n(n1388, n1404, n1405);
    let n1407: ZN = zn_sub(n594, n1384);
    let n1408: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1407);
    let n1409: ZN = zn_add(n594, n1384);
    let n1410: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1409);
    let n1411: ZN = zsel_n(n1388, n1408, n1410);
    let n1412: ZN = zsel_n(n1386, n1406, n1411);
    let n1413: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1412);
    let n1414: ZB = zb_not(n1413);
    let n1415: ZB = zn_lt(n1412, zn_splat(P8::from_raw(0i32)));
    let n1416: ZB = zsel_b(n1414, n1415, r_c272);
    let n1417: ZN = zn_abs(n595);
    let n1418: ZB = zn_le(n1417, zn_splat(P8::from_raw(9830i32)));
    let n1419: ZB = zn_gt(n1417, zn_splat(P8::from_raw(9830i32)));
    let n1420: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n601);
    let n1421: ZB = zn_gt(n595, zn_splat(P8::from_raw(131072i32)));
    let n1422: ZB = zn_le(n595, zn_splat(P8::from_raw(131072i32)));
    let n1423: ZB = zn_gt(n1368, zn_splat(P8::from_raw(0i32)));
    let n1424: ZB = zn_le(n1368, zn_splat(P8::from_raw(0i32)));
    let n1425: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n600);
    let n1426: ZB = zn_tile_flag_at(g.cache, g.cart, n1425, n1420, u.c275, u.c274, P8::from_raw(0i32));
    let n1427: ZB = zb_not(n1426);
    let n1428: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n600);
    let n1429: ZB = zn_tile_flag_at(g.cache, g.cart, n1428, n1420, u.c275, u.c274, P8::from_raw(0i32));
    let n1430: ZB = zb_not(n1429);
    let n1431: ZN = zsel_n(n1429, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1432: ZN = zsel_n(n1426, zn_splat(P8::from_raw(-65536i32)), n1431);
    let n1433: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1432);
    let n1434: ZB = zb_not(n1433);
    let n1435: ZB = zn_gt(n1367, zn_splat(P8::from_raw(0i32)));
    let n1436: ZB = zn_le(n1367, zn_splat(P8::from_raw(0i32)));
    let n1437: ZB = zb_not(n1416);
    let n1438: ZN = zsel_n(n1416, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1439: ZB = zn_gt(n1438, zn_splat(P8::from_raw(0i32)));
    let n1440: ZB = zn_le(n1438, zn_splat(P8::from_raw(0i32)));
    let n1441: ZB = zn_lt(n1438, zn_splat(P8::from_raw(0i32)));
    let n1442: ZB = zn_ge(n1438, zn_splat(P8::from_raw(0i32)));
    let n1443: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1438);
    let n1444: ZB = zb_not(n1443);
    let n1445: ZB = zb_and(n1377, n1378);
    let n1446: ZB = zb_and(n1377, n1379);
    let n1447: ZB = zb_and(n1380, n1445);
    let n1448: ZB = zb_and(n1381, n1445);
    let n1449: ZB = zb_or(n1447, n1448);
    let n1450: ZB = zb_and(n1382, n1449);
    let n1451: ZB = zb_and(n1383, n1449);
    let n1452: ZB = zb_or(n1450, n1451);
    let n1453: ZB = zb_and(n1351, n1446);
    let n1454: ZB = zb_and(n1350, n1446);
    let n1455: ZB = zb_or(n1453, n1454);
    let n1456: ZB = zb_and(n1386, n1455);
    let n1457: ZB = zb_and(n1387, n1455);
    let n1458: ZB = zb_and(n1388, n1456);
    let n1459: ZB = zb_and(n682, n1456);
    let n1460: ZB = zb_and(n1389, n1459);
    let n1461: ZB = zb_and(n707, n1459);
    let n1462: ZB = zb_and(n1390, n1458);
    let n1463: ZB = zb_and(n1391, n1458);
    let n1464: ZB = zb_and(n1396, n1460);
    let n1465: ZB = zb_and(n1397, n1460);
    let n1466: ZB = zb_and(n682, n1461);
    let n1467: ZB = zb_or(n1464, n1465);
    let n1468: ZB = zb_or(n1462, n1463);
    let n1469: ZB = zb_or(n1466, n1467);
    let n1470: ZB = zb_or(n1468, n1469);
    let n1471: ZB = zb_and(n1388, n1457);
    let n1472: ZB = zb_and(n682, n1457);
    let n1473: ZB = zb_or(n1471, n1472);
    let n1474: ZB = zb_or(n1470, n1473);
    let n1475: ZB = zb_and(n1414, n1474);
    let n1476: ZB = zb_and(n1413, n1474);
    let n1477: ZB = zb_or(n1475, n1476);
    let n1478: ZB = zb_and(n1418, n1477);
    let n1479: ZB = zb_and(n1419, n1477);
    let n1480: ZB = zb_or(n1478, n1479);
    let n1481: ZB = zb_and(n1351, n1480);
    let n1482: ZB = zb_and(n1350, n1480);
    let n1483: ZB = zb_and(n1421, n1481);
    let n1484: ZB = zb_and(n1422, n1481);
    let n1485: ZB = zb_or(n1483, n1484);
    let n1486: ZB = zb_or(n1482, n1485);
    let n1487: ZB = zb_and(n1435, n1486);
    let n1488: ZB = zb_and(n1436, n1486);
    let n1489: ZB = zb_or(n1487, n1488);
    let n1490: ZB = zb_or(n1452, n1489);
    let n1491: ZB = zn_lt(n593, zn_splat(P8::from_raw(-262144i32)));
    let n1492: ZB = zn_ge(n593, zn_splat(P8::from_raw(-262144i32)));
    let n1493: ZB = zb_and(n1490, n1491);
    let n1494: ZB = zb_and(n1490, n1492);
    let n1495: ZB = zb_or(n1493, n1494);
    let n1500: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1407);
    let n1501: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1409);
    let n1502: ZN = zsel_n(n1396, n1500, n1501);
    let n1503: ZN = zsel_n(n1386, n1406, n1502);
    let n1504: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1503);
    let n1505: ZB = zb_not(n1504);
    let n1506: ZB = zn_lt(n1503, zn_splat(P8::from_raw(0i32)));
    let n1507: ZB = zsel_b(n1505, n1506, r_c272);
    let n1508: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n600);
    let n1509: ZB = zn_tile_flag_at(g.cache, g.cart, n1508, n1420, u.c275, u.c274, P8::from_raw(0i32));
    let n1510: ZB = zb_not(n1509);
    let n1511: ZN = zsel_n(n1509, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1512: ZB = zn_gt(n595, n1511);
    let n1513: ZB = zn_le(n595, n1511);
    let n1514: ZB = zb_and(n1396, n1457);
    let n1515: ZB = zb_and(n1397, n1457);
    let n1516: ZB = zb_or(n1514, n1515);
    let n1517: ZB = zb_or(n1470, n1516);
    let n1518: ZB = zb_and(n1505, n1517);
    let n1519: ZB = zb_and(n1504, n1517);
    let n1520: ZB = zb_or(n1518, n1519);
    let n1521: ZB = zb_and(n1418, n1520);
    let n1522: ZB = zb_and(n1419, n1520);
    let n1523: ZB = zb_or(n1521, n1522);
    let n1524: ZB = zb_and(n1510, n1523);
    let n1525: ZB = zb_and(n1509, n1523);
    let n1526: ZB = zb_or(n1524, n1525);
    let n1527: ZB = zb_and(n1510, n1526);
    let n1528: ZB = zb_and(n1509, n1526);
    let n1529: ZB = zb_or(n1527, n1528);
    let n1530: ZB = zb_and(n1509, n1529);
    let n1531: ZB = zb_and(n1510, n1529);
    let n1532: ZB = zb_or(n1530, n1531);
    let n1533: ZB = zb_and(n1509, n1532);
    let n1534: ZB = zb_and(n1510, n1532);
    let n1535: ZB = zb_or(n1533, n1534);
    let n1536: ZB = zb_and(n1351, n1535);
    let n1537: ZB = zb_and(n1350, n1535);
    let n1538: ZB = zb_and(n1512, n1536);
    let n1539: ZB = zb_and(n1513, n1536);
    let n1540: ZB = zb_or(n1538, n1539);
    let n1541: ZB = zb_or(n1537, n1540);
    let n1542: ZB = zb_and(n1435, n1541);
    let n1543: ZB = zb_and(n1436, n1541);
    let n1544: ZB = zb_or(n1542, n1543);
    let n1545: ZB = zb_or(n1452, n1544);
    let n1546: ZB = zb_and(n1491, n1545);
    let n1547: ZB = zb_and(n1492, n1545);
    let n1548: ZB = zb_or(n1546, n1547);
    let n1551: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1407);
    let n1552: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1409);
    let n1553: ZN = zsel_n(n1390, n1551, n1552);
    let n1554: ZN = zsel_n(n1386, n1406, n1553);
    let n1555: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1554);
    let n1556: ZB = zb_not(n1555);
    let n1557: ZB = zn_lt(n1554, zn_splat(P8::from_raw(0i32)));
    let n1558: ZB = zsel_b(n1556, n1557, r_c272);
    let n1559: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n600);
    let n1560: ZB = zn_tile_flag_at(g.cache, g.cart, n1559, n1420, u.c275, u.c274, P8::from_raw(0i32));
    let n1561: ZB = zb_not(n1560);
    let n1562: ZN = zsel_n(n1560, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1563: ZB = zn_gt(n595, n1562);
    let n1564: ZB = zn_le(n595, n1562);
    let n1565: ZB = zb_and(n1390, n1457);
    let n1566: ZB = zb_and(n1391, n1457);
    let n1567: ZB = zb_or(n1565, n1566);
    let n1568: ZB = zb_or(n1470, n1567);
    let n1569: ZB = zb_and(n1556, n1568);
    let n1570: ZB = zb_and(n1555, n1568);
    let n1571: ZB = zb_or(n1569, n1570);
    let n1572: ZB = zb_and(n1418, n1571);
    let n1573: ZB = zb_and(n1419, n1571);
    let n1574: ZB = zb_or(n1572, n1573);
    let n1575: ZB = zb_and(n1561, n1574);
    let n1576: ZB = zb_and(n1560, n1574);
    let n1577: ZB = zb_or(n1575, n1576);
    let n1578: ZB = zb_and(n1561, n1577);
    let n1579: ZB = zb_and(n1560, n1577);
    let n1580: ZB = zb_or(n1578, n1579);
    let n1581: ZB = zb_and(n1560, n1580);
    let n1582: ZB = zb_and(n1561, n1580);
    let n1583: ZB = zb_or(n1581, n1582);
    let n1584: ZB = zb_and(n1560, n1583);
    let n1585: ZB = zb_and(n1561, n1583);
    let n1586: ZB = zb_or(n1584, n1585);
    let n1587: ZB = zb_and(n1351, n1586);
    let n1588: ZB = zb_and(n1350, n1586);
    let n1589: ZB = zb_and(n1563, n1587);
    let n1590: ZB = zb_and(n1564, n1587);
    let n1591: ZB = zb_or(n1589, n1590);
    let n1592: ZB = zb_or(n1588, n1591);
    let n1593: ZB = zb_and(n1435, n1592);
    let n1594: ZB = zb_and(n1436, n1592);
    let n1595: ZB = zb_or(n1593, n1594);
    let n1596: ZB = zb_or(n1452, n1595);
    let n1597: ZB = zb_and(n1491, n1596);
    let n1598: ZB = zb_and(n1492, n1596);
    let n1599: ZB = zb_or(n1597, n1598);
    let n1602: ZB = zb_and(n1358, n1486);
    let n1603: ZB = zb_and(r_c247, n1486);
    let n1604: ZB = zb_and(n1423, n1602);
    let n1605: ZB = zb_and(n1424, n1602);
    let n1606: ZB = zb_and(n1427, n1605);
    let n1607: ZB = zb_and(n1426, n1605);
    let n1608: ZB = zb_or(n1606, n1607);
    let n1609: ZB = zb_and(n1427, n1608);
    let n1610: ZB = zb_and(n1426, n1608);
    let n1611: ZB = zb_or(n1609, n1610);
    let n1612: ZB = zb_and(n1426, n1611);
    let n1613: ZB = zb_and(n1427, n1611);
    let n1614: ZB = zb_and(n1430, n1613);
    let n1615: ZB = zb_and(n1429, n1613);
    let n1616: ZB = zb_or(n1614, n1615);
    let n1617: ZB = zb_and(n1430, n1616);
    let n1618: ZB = zb_and(n1429, n1616);
    let n1619: ZB = zb_or(n1617, n1618);
    let n1620: ZB = zb_and(n1429, n1619);
    let n1621: ZB = zb_and(n1430, n1619);
    let n1622: ZB = zb_or(n1620, n1621);
    let n1623: ZB = zb_or(n1612, n1622);
    let n1624: ZB = zb_and(n1434, n1623);
    let n1625: ZB = zb_and(n1433, n1623);
    let n1626: ZB = zb_or(n1624, n1625);
    let n1627: ZB = zb_or(n1604, n1626);
    let n1628: ZB = zb_or(n1603, n1627);
    let n1629: ZB = zb_and(n1435, n1628);
    let n1630: ZB = zb_and(n1436, n1628);
    let n1631: ZB = zb_or(n1629, n1630);
    let n1632: ZB = zb_or(n1452, n1631);
    let n1633: ZB = zb_and(n1491, n1632);
    let n1634: ZB = zb_and(n1492, n1632);
    let n1635: ZB = zb_or(n1633, n1634);
    let n1638: ZB = zb_and(n1358, n1541);
    let n1639: ZB = zb_and(r_c247, n1541);
    let n1640: ZB = zb_and(n1423, n1638);
    let n1641: ZB = zb_and(n1424, n1638);
    let n1642: ZB = zb_and(n1427, n1641);
    let n1643: ZB = zb_and(n1426, n1641);
    let n1644: ZB = zb_or(n1642, n1643);
    let n1645: ZB = zb_and(n1427, n1644);
    let n1646: ZB = zb_and(n1426, n1644);
    let n1647: ZB = zb_or(n1645, n1646);
    let n1648: ZB = zb_and(n1426, n1647);
    let n1649: ZB = zb_and(n1427, n1647);
    let n1650: ZB = zb_and(n1430, n1649);
    let n1651: ZB = zb_and(n1429, n1649);
    let n1652: ZB = zb_or(n1650, n1651);
    let n1653: ZB = zb_and(n1430, n1652);
    let n1654: ZB = zb_and(n1429, n1652);
    let n1655: ZB = zb_or(n1653, n1654);
    let n1656: ZB = zb_and(n1429, n1655);
    let n1657: ZB = zb_and(n1430, n1655);
    let n1658: ZB = zb_or(n1656, n1657);
    let n1659: ZB = zb_or(n1648, n1658);
    let n1660: ZB = zb_and(n1434, n1659);
    let n1661: ZB = zb_and(n1433, n1659);
    let n1662: ZB = zb_or(n1660, n1661);
    let n1663: ZB = zb_or(n1640, n1662);
    let n1664: ZB = zb_or(n1639, n1663);
    let n1665: ZB = zb_and(n1435, n1664);
    let n1666: ZB = zb_and(n1436, n1664);
    let n1667: ZB = zb_or(n1665, n1666);
    let n1668: ZB = zb_or(n1452, n1667);
    let n1669: ZB = zb_and(n1491, n1668);
    let n1670: ZB = zb_and(n1492, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1674: ZB = zb_and(n1358, n1592);
    let n1675: ZB = zb_and(r_c247, n1592);
    let n1676: ZB = zb_and(n1423, n1674);
    let n1677: ZB = zb_and(n1424, n1674);
    let n1678: ZB = zb_and(n1427, n1677);
    let n1679: ZB = zb_and(n1426, n1677);
    let n1680: ZB = zb_or(n1678, n1679);
    let n1681: ZB = zb_and(n1427, n1680);
    let n1682: ZB = zb_and(n1426, n1680);
    let n1683: ZB = zb_or(n1681, n1682);
    let n1684: ZB = zb_and(n1426, n1683);
    let n1685: ZB = zb_and(n1427, n1683);
    let n1686: ZB = zb_and(n1430, n1685);
    let n1687: ZB = zb_and(n1429, n1685);
    let n1688: ZB = zb_or(n1686, n1687);
    let n1689: ZB = zb_and(n1430, n1688);
    let n1690: ZB = zb_and(n1429, n1688);
    let n1691: ZB = zb_or(n1689, n1690);
    let n1692: ZB = zb_and(n1429, n1691);
    let n1693: ZB = zb_and(n1430, n1691);
    let n1694: ZB = zb_or(n1692, n1693);
    let n1695: ZB = zb_or(n1684, n1694);
    let n1696: ZB = zb_and(n1434, n1695);
    let n1697: ZB = zb_and(n1433, n1695);
    let n1698: ZB = zb_or(n1696, n1697);
    let n1699: ZB = zb_or(n1676, n1698);
    let n1700: ZB = zb_or(n1675, n1699);
    let n1701: ZB = zb_and(n1435, n1700);
    let n1702: ZB = zb_and(n1436, n1700);
    let n1703: ZB = zb_or(n1701, n1702);
    let n1704: ZB = zb_or(n1452, n1703);
    let n1705: ZB = zb_and(n1491, n1704);
    let n1706: ZB = zb_and(n1492, n1704);
    let n1707: ZB = zb_or(n1705, n1706);
    let n1710: ZB = zb_and(n1359, n1435);
    let n1711: ZB = zb_not(n1710);
    let n1712: ZN = zsel_n(n1710, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1713: ZB = zb_or(r_c41, n1710);
    let n1714: ZN = zsel_n(n1378, r_c20, n1712);
    let n1715: ZB = zsel_b(n1378, r_c41, n1713);
    let n1716: ZB = zb_and(n1489, n1710);
    let n1717: ZB = zb_and(n1489, n1711);
    let n1718: ZB = zb_and(n1416, n1716);
    let n1719: ZB = zb_and(n1437, n1716);
    let n1720: ZB = zb_or(n1718, n1719);
    let n1721: ZB = zb_and(n1439, n1720);
    let n1722: ZB = zb_and(n1440, n1720);
    let n1723: ZB = zb_and(n1441, n1722);
    let n1724: ZB = zb_and(n1442, n1722);
    let n1725: ZB = zb_or(n1723, n1724);
    let n1726: ZB = zb_or(n1721, n1725);
    let n1727: ZB = zb_and(n1444, n1726);
    let n1728: ZB = zb_and(n1443, n1726);
    let n1729: ZB = zb_or(n1727, n1728);
    let n1730: ZB = zb_or(n1717, n1729);
    let n1731: ZB = zb_or(n1452, n1730);
    let n1732: ZB = zb_and(n1491, n1731);
    let n1733: ZB = zb_and(n1492, n1731);
    let n1734: ZB = zb_or(n1732, n1733);
    let n1735: ZB = zb_and(n1492, n1734);
    let n1736: ZB = zn_gt(n1714, zn_splat(P8::from_raw(0i32)));
    let n1737: ZB = zn_le(n1714, zn_splat(P8::from_raw(0i32)));
    let n1738: ZB = zb_and(n1735, n1736);
    let n1739: ZB = zb_and(n1735, n1737);
    let n1740: ZB = zb_or(n1738, n1739);
    let n1741: ZB = zb_and(n1544, n1710);
    let n1742: ZB = zb_and(n1544, n1711);
    let n1743: ZB = zb_or(n1741, n1742);
    let n1744: ZB = zb_or(n1452, n1743);
    let n1745: ZB = zb_and(n1491, n1744);
    let n1746: ZB = zb_and(n1492, n1744);
    let n1747: ZB = zb_or(n1745, n1746);
    let n1748: ZB = zb_and(n1492, n1747);
    let n1749: ZB = zb_and(n1736, n1748);
    let n1750: ZB = zb_and(n1737, n1748);
    let n1751: ZB = zb_or(n1749, n1750);
    let n1752: ZB = zb_and(n1595, n1710);
    let n1753: ZB = zb_and(n1595, n1711);
    let n1754: ZB = zb_or(n1752, n1753);
    let n1755: ZB = zb_or(n1452, n1754);
    let n1756: ZB = zb_and(n1491, n1755);
    let n1757: ZB = zb_and(n1492, n1755);
    let n1758: ZB = zb_or(n1756, n1757);
    let n1759: ZB = zb_and(n1492, n1758);
    let n1760: ZB = zb_and(n1736, n1759);
    let n1761: ZB = zb_and(n1737, n1759);
    let n1762: ZB = zb_or(n1760, n1761);
    let n1763: ZB = zb_or(n1716, n1717);
    let n1764: ZB = zb_or(n1452, n1763);
    let n1765: ZB = zb_and(n1491, n1764);
    let n1766: ZB = zb_and(n1492, n1764);
    let n1767: ZB = zb_or(n1765, n1766);
    let n1768: ZB = zb_and(n1492, n1767);
    let n1769: ZB = zb_and(n1736, n1768);
    let n1770: ZB = zb_and(n1737, n1768);
    let n1771: ZB = zb_or(n1769, n1770);
    let n1772: ZB = zb_and(n1631, n1710);
    let n1773: ZB = zb_and(n1631, n1711);
    let n1774: ZB = zb_and(n1416, n1772);
    let n1775: ZB = zb_and(n1437, n1772);
    let n1776: ZB = zb_or(n1774, n1775);
    let n1777: ZB = zb_and(n1439, n1776);
    let n1778: ZB = zb_and(n1440, n1776);
    let n1779: ZB = zb_and(n1441, n1778);
    let n1780: ZB = zb_and(n1442, n1778);
    let n1781: ZB = zb_or(n1779, n1780);
    let n1782: ZB = zb_or(n1777, n1781);
    let n1783: ZB = zb_and(n1444, n1782);
    let n1784: ZB = zb_and(n1443, n1782);
    let n1785: ZB = zb_or(n1783, n1784);
    let n1786: ZB = zb_or(n1773, n1785);
    let n1787: ZB = zb_or(n1452, n1786);
    let n1788: ZB = zb_and(n1491, n1787);
    let n1789: ZB = zb_and(n1492, n1787);
    let n1790: ZB = zb_or(n1788, n1789);
    let n1791: ZB = zb_and(n1492, n1790);
    let n1792: ZB = zb_and(n1736, n1791);
    let n1793: ZB = zb_and(n1737, n1791);
    let n1794: ZB = zb_or(n1792, n1793);
    let n1795: ZB = zb_and(n1667, n1710);
    let n1796: ZB = zb_and(n1667, n1711);
    let n1797: ZB = zb_or(n1795, n1796);
    let n1798: ZB = zb_or(n1452, n1797);
    let n1799: ZB = zb_and(n1491, n1798);
    let n1800: ZB = zb_and(n1492, n1798);
    let n1801: ZB = zb_or(n1799, n1800);
    let n1802: ZB = zb_and(n1492, n1801);
    let n1803: ZB = zb_and(n1736, n1802);
    let n1804: ZB = zb_and(n1737, n1802);
    let n1805: ZB = zb_or(n1803, n1804);
    let n1806: ZB = zb_and(n1703, n1710);
    let n1807: ZB = zb_and(n1703, n1711);
    let n1808: ZB = zb_or(n1806, n1807);
    let n1809: ZB = zb_or(n1452, n1808);
    let n1810: ZB = zb_and(n1491, n1809);
    let n1811: ZB = zb_and(n1492, n1809);
    let n1812: ZB = zb_or(n1810, n1811);
    let n1813: ZB = zb_and(n1492, n1812);
    let n1814: ZB = zb_and(n1736, n1813);
    let n1815: ZB = zb_and(n1737, n1813);
    let n1816: ZB = zb_or(n1814, n1815);
    let n1817: ZB = zb_or(n1772, n1773);
    let n1818: ZB = zb_or(n1452, n1817);
    let n1819: ZB = zb_and(n1491, n1818);
    let n1820: ZB = zb_and(n1492, n1818);
    let n1821: ZB = zb_or(n1819, n1820);
    let n1822: ZB = zb_and(n1492, n1821);
    let n1823: ZB = zb_and(n1736, n1822);
    let n1824: ZB = zb_and(n1737, n1822);
    let n1825: ZB = zb_or(n1823, n1824);
    let n1830: ZN = zsel_n(n46, n34, r_c39);
    let n1831: ZB = zb_and(n1336, n1339);
    let n1832: ZB = zb_and(n1351, n1831);
    let n1833: ZB = zb_and(n1350, n1831);
    let n1834: ZB = zb_or(n1832, n1833);
    let n1835: ZB = zb_and(n1351, n1834);
    let n1836: ZB = zb_and(n1350, n1834);
    let n1837: ZB = zb_or(n1835, n1836);
    let n1838: ZB = zb_and(n1350, n1837);
    let n1839: ZB = zb_and(n1351, n1837);
    let n1840: ZB = zb_and(n1360, n1838);
    let n1841: ZB = zb_and(n1361, n1838);
    let n1842: ZB = zb_or(n1840, n1841);
    let n1843: ZB = zb_and(n1363, n1839);
    let n1844: ZB = zb_and(n1364, n1839);
    let n1845: ZB = zb_or(n1843, n1844);
    let n1846: ZB = zb_or(n1842, n1845);
    let n1847: ZB = zb_and(n1378, n1846);
    let n1848: ZB = zb_and(n1379, n1846);
    let n1849: ZB = zb_and(n1380, n1847);
    let n1850: ZB = zb_and(n1381, n1847);
    let n1851: ZB = zb_or(n1849, n1850);
    let n1852: ZB = zb_and(n1382, n1851);
    let n1853: ZB = zb_and(n1383, n1851);
    let n1854: ZB = zb_or(n1852, n1853);
    let n1855: ZB = zb_and(n1351, n1848);
    let n1856: ZB = zb_and(n1350, n1848);
    let n1857: ZB = zb_or(n1855, n1856);
    let n1858: ZB = zb_and(n1386, n1857);
    let n1859: ZB = zb_and(n1387, n1857);
    let n1860: ZB = zb_and(n1388, n1858);
    let n1861: ZB = zb_and(n682, n1858);
    let n1862: ZB = zb_and(n1389, n1861);
    let n1863: ZB = zb_and(n707, n1861);
    let n1864: ZB = zb_and(n1390, n1860);
    let n1865: ZB = zb_and(n1391, n1860);
    let n1866: ZB = zb_and(n1396, n1862);
    let n1867: ZB = zb_and(n1397, n1862);
    let n1868: ZB = zb_and(n682, n1863);
    let n1869: ZB = zb_or(n1866, n1867);
    let n1870: ZB = zb_or(n1864, n1865);
    let n1871: ZB = zb_or(n1868, n1869);
    let n1872: ZB = zb_or(n1870, n1871);
    let n1873: ZB = zb_and(n1388, n1859);
    let n1874: ZB = zb_and(n682, n1859);
    let n1875: ZB = zb_or(n1873, n1874);
    let n1876: ZB = zb_or(n1872, n1875);
    let n1877: ZB = zb_and(n1414, n1876);
    let n1878: ZB = zb_and(n1413, n1876);
    let n1879: ZB = zb_or(n1877, n1878);
    let n1880: ZB = zb_and(n1418, n1879);
    let n1881: ZB = zb_and(n1419, n1879);
    let n1882: ZB = zb_or(n1880, n1881);
    let n1883: ZB = zb_and(n1351, n1882);
    let n1884: ZB = zb_and(n1350, n1882);
    let n1885: ZB = zb_and(n1421, n1883);
    let n1886: ZB = zb_and(n1422, n1883);
    let n1887: ZB = zb_or(n1885, n1886);
    let n1888: ZB = zb_or(n1884, n1887);
    let n1889: ZB = zb_and(n1435, n1888);
    let n1890: ZB = zb_and(n1436, n1888);
    let n1891: ZB = zb_or(n1889, n1890);
    let n1892: ZB = zb_or(n1854, n1891);
    let n1893: ZB = zb_and(n1491, n1892);
    let n1894: ZB = zb_and(n1492, n1892);
    let n1895: ZB = zb_or(n1893, n1894);
    let n1896: ZB = zb_and(n1491, n1895);
    let n1897: ZB = zb_and(n1491, n1495);
    let n1898: ZN = zsel_n(n1896, r_c87, n1345);
    let n1899: ZN = zsel_n(n1896, n1830, zn_splat(P8::from_raw(983040i32)));
    let n1900: ZB = zb_not(n1896);
    let n1901: ZB = zb_or(r_c38, n1900);
    let n1902: ZB = zb_or(n1896, n1897);
    let n1903: ZB = zsel_b(n1896, n1337, n1347);
    let n1905: ZB = zb_and(n1396, n1859);
    let n1906: ZB = zb_and(n1397, n1859);
    let n1907: ZB = zb_or(n1905, n1906);
    let n1908: ZB = zb_or(n1872, n1907);
    let n1909: ZB = zb_and(n1505, n1908);
    let n1910: ZB = zb_and(n1504, n1908);
    let n1911: ZB = zb_or(n1909, n1910);
    let n1912: ZB = zb_and(n1418, n1911);
    let n1913: ZB = zb_and(n1419, n1911);
    let n1914: ZB = zb_or(n1912, n1913);
    let n1915: ZB = zb_and(n1510, n1914);
    let n1916: ZB = zb_and(n1509, n1914);
    let n1917: ZB = zb_or(n1915, n1916);
    let n1918: ZB = zb_and(n1510, n1917);
    let n1919: ZB = zb_and(n1509, n1917);
    let n1920: ZB = zb_or(n1918, n1919);
    let n1921: ZB = zb_and(n1509, n1920);
    let n1922: ZB = zb_and(n1510, n1920);
    let n1923: ZB = zb_or(n1921, n1922);
    let n1924: ZB = zb_and(n1509, n1923);
    let n1925: ZB = zb_and(n1510, n1923);
    let n1926: ZB = zb_or(n1924, n1925);
    let n1927: ZB = zb_and(n1351, n1926);
    let n1928: ZB = zb_and(n1350, n1926);
    let n1929: ZB = zb_and(n1512, n1927);
    let n1930: ZB = zb_and(n1513, n1927);
    let n1931: ZB = zb_or(n1929, n1930);
    let n1932: ZB = zb_or(n1928, n1931);
    let n1933: ZB = zb_and(n1435, n1932);
    let n1934: ZB = zb_and(n1436, n1932);
    let n1935: ZB = zb_or(n1933, n1934);
    let n1936: ZB = zb_or(n1854, n1935);
    let n1937: ZB = zb_and(n1491, n1936);
    let n1938: ZB = zb_and(n1492, n1936);
    let n1939: ZB = zb_or(n1937, n1938);
    let n1940: ZB = zb_and(n1491, n1939);
    let n1941: ZB = zb_and(n1491, n1548);
    let n1942: ZN = zsel_n(n1940, r_c87, n1345);
    let n1943: ZN = zsel_n(n1940, n1830, zn_splat(P8::from_raw(983040i32)));
    let n1944: ZB = zb_not(n1940);
    let n1945: ZB = zb_or(r_c38, n1944);
    let n1946: ZB = zb_or(n1940, n1941);
    let n1947: ZB = zsel_b(n1940, n1337, n1347);
    let n1949: ZB = zb_and(n1390, n1859);
    let n1950: ZB = zb_and(n1391, n1859);
    let n1951: ZB = zb_or(n1949, n1950);
    let n1952: ZB = zb_or(n1872, n1951);
    let n1953: ZB = zb_and(n1556, n1952);
    let n1954: ZB = zb_and(n1555, n1952);
    let n1955: ZB = zb_or(n1953, n1954);
    let n1956: ZB = zb_and(n1418, n1955);
    let n1957: ZB = zb_and(n1419, n1955);
    let n1958: ZB = zb_or(n1956, n1957);
    let n1959: ZB = zb_and(n1561, n1958);
    let n1960: ZB = zb_and(n1560, n1958);
    let n1961: ZB = zb_or(n1959, n1960);
    let n1962: ZB = zb_and(n1561, n1961);
    let n1963: ZB = zb_and(n1560, n1961);
    let n1964: ZB = zb_or(n1962, n1963);
    let n1965: ZB = zb_and(n1560, n1964);
    let n1966: ZB = zb_and(n1561, n1964);
    let n1967: ZB = zb_or(n1965, n1966);
    let n1968: ZB = zb_and(n1560, n1967);
    let n1969: ZB = zb_and(n1561, n1967);
    let n1970: ZB = zb_or(n1968, n1969);
    let n1971: ZB = zb_and(n1351, n1970);
    let n1972: ZB = zb_and(n1350, n1970);
    let n1973: ZB = zb_and(n1563, n1971);
    let n1974: ZB = zb_and(n1564, n1971);
    let n1975: ZB = zb_or(n1973, n1974);
    let n1976: ZB = zb_or(n1972, n1975);
    let n1977: ZB = zb_and(n1435, n1976);
    let n1978: ZB = zb_and(n1436, n1976);
    let n1979: ZB = zb_or(n1977, n1978);
    let n1980: ZB = zb_or(n1854, n1979);
    let n1981: ZB = zb_and(n1491, n1980);
    let n1982: ZB = zb_and(n1492, n1980);
    let n1983: ZB = zb_or(n1981, n1982);
    let n1984: ZB = zb_and(n1491, n1983);
    let n1985: ZB = zb_and(n1491, n1599);
    let n1986: ZN = zsel_n(n1984, r_c87, n1345);
    let n1987: ZN = zsel_n(n1984, n1830, zn_splat(P8::from_raw(983040i32)));
    let n1988: ZB = zb_not(n1984);
    let n1989: ZB = zb_or(r_c38, n1988);
    let n1990: ZB = zb_or(n1984, n1985);
    let n1991: ZB = zsel_b(n1984, n1337, n1347);
    let n1993: ZB = zb_and(n1358, n1888);
    let n1994: ZB = zb_and(r_c247, n1888);
    let n1995: ZB = zb_and(n1423, n1993);
    let n1996: ZB = zb_and(n1424, n1993);
    let n1997: ZB = zb_and(n1427, n1996);
    let n1998: ZB = zb_and(n1426, n1996);
    let n1999: ZB = zb_or(n1997, n1998);
    let n2000: ZB = zb_and(n1427, n1999);
    let n2001: ZB = zb_and(n1426, n1999);
    let n2002: ZB = zb_or(n2000, n2001);
    let n2003: ZB = zb_and(n1426, n2002);
    let n2004: ZB = zb_and(n1427, n2002);
    let n2005: ZB = zb_and(n1430, n2004);
    let n2006: ZB = zb_and(n1429, n2004);
    let n2007: ZB = zb_or(n2005, n2006);
    let n2008: ZB = zb_and(n1430, n2007);
    let n2009: ZB = zb_and(n1429, n2007);
    let n2010: ZB = zb_or(n2008, n2009);
    let n2011: ZB = zb_and(n1429, n2010);
    let n2012: ZB = zb_and(n1430, n2010);
    let n2013: ZB = zb_or(n2011, n2012);
    let n2014: ZB = zb_or(n2003, n2013);
    let n2015: ZB = zb_and(n1434, n2014);
    let n2016: ZB = zb_and(n1433, n2014);
    let n2017: ZB = zb_or(n2015, n2016);
    let n2018: ZB = zb_or(n1995, n2017);
    let n2019: ZB = zb_or(n1994, n2018);
    let n2020: ZB = zb_and(n1435, n2019);
    let n2021: ZB = zb_and(n1436, n2019);
    let n2022: ZB = zb_or(n2020, n2021);
    let n2023: ZB = zb_or(n1854, n2022);
    let n2024: ZB = zb_and(n1491, n2023);
    let n2025: ZB = zb_and(n1492, n2023);
    let n2026: ZB = zb_or(n2024, n2025);
    let n2027: ZB = zb_and(n1491, n2026);
    let n2028: ZB = zb_and(n1491, n1635);
    let n2029: ZN = zsel_n(n2027, r_c87, n1345);
    let n2030: ZN = zsel_n(n2027, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2031: ZB = zb_not(n2027);
    let n2032: ZB = zb_or(r_c38, n2031);
    let n2033: ZB = zb_or(n2027, n2028);
    let n2034: ZB = zsel_b(n2027, n1337, n1347);
    let n2036: ZB = zb_and(n1358, n1932);
    let n2037: ZB = zb_and(r_c247, n1932);
    let n2038: ZB = zb_and(n1423, n2036);
    let n2039: ZB = zb_and(n1424, n2036);
    let n2040: ZB = zb_and(n1427, n2039);
    let n2041: ZB = zb_and(n1426, n2039);
    let n2042: ZB = zb_or(n2040, n2041);
    let n2043: ZB = zb_and(n1427, n2042);
    let n2044: ZB = zb_and(n1426, n2042);
    let n2045: ZB = zb_or(n2043, n2044);
    let n2046: ZB = zb_and(n1426, n2045);
    let n2047: ZB = zb_and(n1427, n2045);
    let n2048: ZB = zb_and(n1430, n2047);
    let n2049: ZB = zb_and(n1429, n2047);
    let n2050: ZB = zb_or(n2048, n2049);
    let n2051: ZB = zb_and(n1430, n2050);
    let n2052: ZB = zb_and(n1429, n2050);
    let n2053: ZB = zb_or(n2051, n2052);
    let n2054: ZB = zb_and(n1429, n2053);
    let n2055: ZB = zb_and(n1430, n2053);
    let n2056: ZB = zb_or(n2054, n2055);
    let n2057: ZB = zb_or(n2046, n2056);
    let n2058: ZB = zb_and(n1434, n2057);
    let n2059: ZB = zb_and(n1433, n2057);
    let n2060: ZB = zb_or(n2058, n2059);
    let n2061: ZB = zb_or(n2038, n2060);
    let n2062: ZB = zb_or(n2037, n2061);
    let n2063: ZB = zb_and(n1435, n2062);
    let n2064: ZB = zb_and(n1436, n2062);
    let n2065: ZB = zb_or(n2063, n2064);
    let n2066: ZB = zb_or(n1854, n2065);
    let n2067: ZB = zb_and(n1491, n2066);
    let n2068: ZB = zb_and(n1492, n2066);
    let n2069: ZB = zb_or(n2067, n2068);
    let n2070: ZB = zb_and(n1491, n2069);
    let n2071: ZB = zb_and(n1491, n1671);
    let n2072: ZN = zsel_n(n2070, r_c87, n1345);
    let n2073: ZN = zsel_n(n2070, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2074: ZB = zb_not(n2070);
    let n2075: ZB = zb_or(r_c38, n2074);
    let n2076: ZB = zb_or(n2070, n2071);
    let n2077: ZB = zsel_b(n2070, n1337, n1347);
    let n2079: ZB = zb_and(n1358, n1976);
    let n2080: ZB = zb_and(r_c247, n1976);
    let n2081: ZB = zb_and(n1423, n2079);
    let n2082: ZB = zb_and(n1424, n2079);
    let n2083: ZB = zb_and(n1427, n2082);
    let n2084: ZB = zb_and(n1426, n2082);
    let n2085: ZB = zb_or(n2083, n2084);
    let n2086: ZB = zb_and(n1427, n2085);
    let n2087: ZB = zb_and(n1426, n2085);
    let n2088: ZB = zb_or(n2086, n2087);
    let n2089: ZB = zb_and(n1426, n2088);
    let n2090: ZB = zb_and(n1427, n2088);
    let n2091: ZB = zb_and(n1430, n2090);
    let n2092: ZB = zb_and(n1429, n2090);
    let n2093: ZB = zb_or(n2091, n2092);
    let n2094: ZB = zb_and(n1430, n2093);
    let n2095: ZB = zb_and(n1429, n2093);
    let n2096: ZB = zb_or(n2094, n2095);
    let n2097: ZB = zb_and(n1429, n2096);
    let n2098: ZB = zb_and(n1430, n2096);
    let n2099: ZB = zb_or(n2097, n2098);
    let n2100: ZB = zb_or(n2089, n2099);
    let n2101: ZB = zb_and(n1434, n2100);
    let n2102: ZB = zb_and(n1433, n2100);
    let n2103: ZB = zb_or(n2101, n2102);
    let n2104: ZB = zb_or(n2081, n2103);
    let n2105: ZB = zb_or(n2080, n2104);
    let n2106: ZB = zb_and(n1435, n2105);
    let n2107: ZB = zb_and(n1436, n2105);
    let n2108: ZB = zb_or(n2106, n2107);
    let n2109: ZB = zb_or(n1854, n2108);
    let n2110: ZB = zb_and(n1491, n2109);
    let n2111: ZB = zb_and(n1492, n2109);
    let n2112: ZB = zb_or(n2110, n2111);
    let n2113: ZB = zb_and(n1491, n2112);
    let n2114: ZB = zb_and(n1491, n1707);
    let n2115: ZN = zsel_n(n2113, r_c87, n1345);
    let n2116: ZN = zsel_n(n2113, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2117: ZB = zb_not(n2113);
    let n2118: ZB = zb_or(r_c38, n2117);
    let n2119: ZB = zb_or(n2113, n2114);
    let n2120: ZB = zsel_b(n2113, n1337, n1347);
    let n2122: ZB = zb_and(n1710, n1891);
    let n2123: ZB = zb_and(n1711, n1891);
    let n2124: ZB = zb_and(n1416, n2122);
    let n2125: ZB = zb_and(n1437, n2122);
    let n2126: ZB = zb_or(n2124, n2125);
    let n2127: ZB = zb_and(n1439, n2126);
    let n2128: ZB = zb_and(n1440, n2126);
    let n2129: ZB = zb_and(n1441, n2128);
    let n2130: ZB = zb_and(n1442, n2128);
    let n2131: ZB = zb_or(n2129, n2130);
    let n2132: ZB = zb_or(n2127, n2131);
    let n2133: ZB = zb_and(n1444, n2132);
    let n2134: ZB = zb_and(n1443, n2132);
    let n2135: ZB = zb_or(n2133, n2134);
    let n2136: ZB = zb_or(n2123, n2135);
    let n2137: ZB = zb_or(n1854, n2136);
    let n2138: ZB = zb_and(n1491, n2137);
    let n2139: ZB = zb_and(n1492, n2137);
    let n2140: ZB = zb_or(n2138, n2139);
    let n2141: ZB = zb_and(n1491, n2140);
    let n2142: ZB = zb_and(n1491, n1734);
    let n2143: ZN = zsel_n(n2141, r_c87, n1345);
    let n2144: ZN = zsel_n(n2141, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2145: ZB = zb_not(n2141);
    let n2146: ZB = zb_or(r_c38, n2145);
    let n2147: ZB = zb_or(n2141, n2142);
    let n2148: ZB = zsel_b(n2141, n1337, n1347);
    let n2149: ZB = zb_and(n1736, n2147);
    let n2150: ZB = zb_and(n1737, n2147);
    let n2151: ZB = zb_or(n2149, n2150);
    let n2152: ZB = zb_and(n1710, n1935);
    let n2153: ZB = zb_and(n1711, n1935);
    let n2154: ZB = zb_or(n2152, n2153);
    let n2155: ZB = zb_or(n1854, n2154);
    let n2156: ZB = zb_and(n1491, n2155);
    let n2157: ZB = zb_and(n1492, n2155);
    let n2158: ZB = zb_or(n2156, n2157);
    let n2159: ZB = zb_and(n1491, n2158);
    let n2160: ZB = zb_and(n1491, n1747);
    let n2161: ZN = zsel_n(n2159, r_c87, n1345);
    let n2162: ZN = zsel_n(n2159, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2163: ZB = zb_not(n2159);
    let n2164: ZB = zb_or(r_c38, n2163);
    let n2165: ZB = zb_or(n2159, n2160);
    let n2166: ZB = zsel_b(n2159, n1337, n1347);
    let n2167: ZB = zb_and(n1736, n2165);
    let n2168: ZB = zb_and(n1737, n2165);
    let n2169: ZB = zb_or(n2167, n2168);
    let n2170: ZB = zb_and(n1710, n1979);
    let n2171: ZB = zb_and(n1711, n1979);
    let n2172: ZB = zb_or(n2170, n2171);
    let n2173: ZB = zb_or(n1854, n2172);
    let n2174: ZB = zb_and(n1491, n2173);
    let n2175: ZB = zb_and(n1492, n2173);
    let n2176: ZB = zb_or(n2174, n2175);
    let n2177: ZB = zb_and(n1491, n2176);
    let n2178: ZB = zb_and(n1491, n1758);
    let n2179: ZN = zsel_n(n2177, r_c87, n1345);
    let n2180: ZN = zsel_n(n2177, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2181: ZB = zb_not(n2177);
    let n2182: ZB = zb_or(r_c38, n2181);
    let n2183: ZB = zb_or(n2177, n2178);
    let n2184: ZB = zsel_b(n2177, n1337, n1347);
    let n2185: ZB = zb_and(n1736, n2183);
    let n2186: ZB = zb_and(n1737, n2183);
    let n2187: ZB = zb_or(n2185, n2186);
    let n2188: ZB = zb_or(n2122, n2123);
    let n2189: ZB = zb_or(n1854, n2188);
    let n2190: ZB = zb_and(n1491, n2189);
    let n2191: ZB = zb_and(n1492, n2189);
    let n2192: ZB = zb_or(n2190, n2191);
    let n2193: ZB = zb_and(n1491, n2192);
    let n2194: ZB = zb_and(n1491, n1767);
    let n2195: ZN = zsel_n(n2193, r_c87, n1345);
    let n2196: ZN = zsel_n(n2193, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2197: ZB = zb_not(n2193);
    let n2198: ZB = zb_or(r_c38, n2197);
    let n2199: ZB = zb_or(n2193, n2194);
    let n2200: ZB = zsel_b(n2193, n1337, n1347);
    let n2201: ZB = zb_and(n1736, n2199);
    let n2202: ZB = zb_and(n1737, n2199);
    let n2203: ZB = zb_or(n2201, n2202);
    let n2204: ZB = zb_and(n1710, n2022);
    let n2205: ZB = zb_and(n1711, n2022);
    let n2206: ZB = zb_and(n1416, n2204);
    let n2207: ZB = zb_and(n1437, n2204);
    let n2208: ZB = zb_or(n2206, n2207);
    let n2209: ZB = zb_and(n1439, n2208);
    let n2210: ZB = zb_and(n1440, n2208);
    let n2211: ZB = zb_and(n1441, n2210);
    let n2212: ZB = zb_and(n1442, n2210);
    let n2213: ZB = zb_or(n2211, n2212);
    let n2214: ZB = zb_or(n2209, n2213);
    let n2215: ZB = zb_and(n1444, n2214);
    let n2216: ZB = zb_and(n1443, n2214);
    let n2217: ZB = zb_or(n2215, n2216);
    let n2218: ZB = zb_or(n2205, n2217);
    let n2219: ZB = zb_or(n1854, n2218);
    let n2220: ZB = zb_and(n1491, n2219);
    let n2221: ZB = zb_and(n1492, n2219);
    let n2222: ZB = zb_or(n2220, n2221);
    let n2223: ZB = zb_and(n1491, n2222);
    let n2224: ZB = zb_and(n1491, n1790);
    let n2225: ZN = zsel_n(n2223, r_c87, n1345);
    let n2226: ZN = zsel_n(n2223, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2227: ZB = zb_not(n2223);
    let n2228: ZB = zb_or(r_c38, n2227);
    let n2229: ZB = zb_or(n2223, n2224);
    let n2230: ZB = zsel_b(n2223, n1337, n1347);
    let n2231: ZB = zb_and(n1736, n2229);
    let n2232: ZB = zb_and(n1737, n2229);
    let n2233: ZB = zb_or(n2231, n2232);
    let n2234: ZB = zb_and(n1710, n2065);
    let n2235: ZB = zb_and(n1711, n2065);
    let n2236: ZB = zb_or(n2234, n2235);
    let n2237: ZB = zb_or(n1854, n2236);
    let n2238: ZB = zb_and(n1491, n2237);
    let n2239: ZB = zb_and(n1492, n2237);
    let n2240: ZB = zb_or(n2238, n2239);
    let n2241: ZB = zb_and(n1491, n2240);
    let n2242: ZB = zb_and(n1491, n1801);
    let n2243: ZN = zsel_n(n2241, r_c87, n1345);
    let n2244: ZN = zsel_n(n2241, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2245: ZB = zb_not(n2241);
    let n2246: ZB = zb_or(r_c38, n2245);
    let n2247: ZB = zb_or(n2241, n2242);
    let n2248: ZB = zsel_b(n2241, n1337, n1347);
    let n2249: ZB = zb_and(n1736, n2247);
    let n2250: ZB = zb_and(n1737, n2247);
    let n2251: ZB = zb_or(n2249, n2250);
    let n2252: ZB = zb_and(n1710, n2108);
    let n2253: ZB = zb_and(n1711, n2108);
    let n2254: ZB = zb_or(n2252, n2253);
    let n2255: ZB = zb_or(n1854, n2254);
    let n2256: ZB = zb_and(n1491, n2255);
    let n2257: ZB = zb_and(n1492, n2255);
    let n2258: ZB = zb_or(n2256, n2257);
    let n2259: ZB = zb_and(n1491, n2258);
    let n2260: ZB = zb_and(n1491, n1812);
    let n2261: ZN = zsel_n(n2259, r_c87, n1345);
    let n2262: ZN = zsel_n(n2259, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2263: ZB = zb_not(n2259);
    let n2264: ZB = zb_or(r_c38, n2263);
    let n2265: ZB = zb_or(n2259, n2260);
    let n2266: ZB = zsel_b(n2259, n1337, n1347);
    let n2267: ZB = zb_and(n1736, n2265);
    let n2268: ZB = zb_and(n1737, n2265);
    let n2269: ZB = zb_or(n2267, n2268);
    let n2270: ZB = zb_or(n2204, n2205);
    let n2271: ZB = zb_or(n1854, n2270);
    let n2272: ZB = zb_and(n1491, n2271);
    let n2273: ZB = zb_and(n1492, n2271);
    let n2274: ZB = zb_or(n2272, n2273);
    let n2275: ZB = zb_and(n1491, n2274);
    let n2276: ZB = zb_and(n1491, n1821);
    let n2277: ZN = zsel_n(n2275, r_c87, n1345);
    let n2278: ZN = zsel_n(n2275, n1830, zn_splat(P8::from_raw(983040i32)));
    let n2279: ZB = zb_not(n2275);
    let n2280: ZB = zb_or(r_c38, n2279);
    let n2281: ZB = zb_or(n2275, n2276);
    let n2282: ZB = zsel_b(n2275, n1337, n1347);
    let n2283: ZB = zb_and(n1736, n2281);
    let n2284: ZB = zb_and(n1737, n2281);
    let n2285: ZB = zb_or(n2283, n2284);
    let n2292: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2299: ZN = zn_sub(n107, zn_splat(P8::from_raw(32768i32)));
    let n2300: ZN = zn_sub(n2299, n108);
    let n2301: ZN = zsel_n(n247, zn_splat(P8::from_raw(0i32)), n2300);
    let n2302: ZN = zsel_n(n242, n2300, n2301);
    let n2303: ZN = zsel_n(n230, zn_splat(P8::from_raw(0i32)), n2302);
    let n2304: ZN = zsel_n(n225, n2300, n2303);
    let n2305: ZN = zsel_n(n213, zn_splat(P8::from_raw(0i32)), n2304);
    let n2306: ZN = zsel_n(n208, n2300, n2305);
    let n2307: ZN = zsel_n(n196, zn_splat(P8::from_raw(0i32)), n2306);
    let n2308: ZN = zsel_n(n191, n2300, n2307);
    let n2309: ZN = zsel_n(n179, zn_splat(P8::from_raw(0i32)), n2308);
    let n2310: ZN = zsel_n(n174, n2300, n2309);
    let n2311: ZN = zsel_n(n162, zn_splat(P8::from_raw(0i32)), n2310);
    let n2312: ZN = zsel_n(n157, n2300, n2311);
    let n2313: ZN = zsel_n(n145, zn_splat(P8::from_raw(0i32)), n2312);
    let n2314: ZN = zsel_n(n140, n2300, n2313);
    let n2315: ZN = zsel_n(n128, zn_splat(P8::from_raw(0i32)), n2314);
    let n2316: ZN = zsel_n(r_c249, n2315, n2300);
    let n2317: ZN = zn_sub(n325, zn_splat(P8::from_raw(32768i32)));
    let n2318: ZN = zn_sub(n2317, n326);
    let n2319: ZN = zsel_n(n514, zn_splat(P8::from_raw(0i32)), n2318);
    let n2320: ZN = zsel_n(n503, n2318, n2319);
    let n2321: ZN = zsel_n(n491, zn_splat(P8::from_raw(0i32)), n2320);
    let n2322: ZN = zsel_n(n480, n2318, n2321);
    let n2323: ZN = zsel_n(n468, zn_splat(P8::from_raw(0i32)), n2322);
    let n2324: ZN = zsel_n(n457, n2318, n2323);
    let n2325: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n2324);
    let n2326: ZN = zsel_n(n434, n2318, n2325);
    let n2327: ZN = zsel_n(n422, zn_splat(P8::from_raw(0i32)), n2326);
    let n2328: ZN = zsel_n(n411, n2318, n2327);
    let n2329: ZN = zsel_n(n399, zn_splat(P8::from_raw(0i32)), n2328);
    let n2330: ZN = zsel_n(n388, n2318, n2329);
    let n2331: ZN = zsel_n(n376, zn_splat(P8::from_raw(0i32)), n2330);
    let n2332: ZN = zsel_n(n365, n2318, n2331);
    let n2333: ZN = zsel_n(n353, zn_splat(P8::from_raw(0i32)), n2332);
    let n2334: ZN = zsel_n(r_c249, n2333, n2318);
    let n2335: ZN = zsel_n(n102, n2316, r_c278);
    let n2336: ZN = zsel_n(n102, n2334, r_c279);
    let n2337: ZB = zb_and(r_c43, n596);
    let n2338: ZN = zn_sub(r_c234, zn_splat(P8::from_raw(65536i32)));
    let n2339: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n2340: ZN = zn_sub(n594, r_c268);
    let n2341: ZN = zn_max(r_c270, n2340);
    let n2342: ZN = zn_add(n594, r_c268);
    let n2343: ZN = zn_min(r_c270, n2342);
    let n2344: ZN = zsel_n(n1380, n2341, n2343);
    let n2345: ZN = zn_sub(n595, r_c269);
    let n2346: ZN = zn_max(r_c271, n2345);
    let n2347: ZN = zn_add(n595, r_c269);
    let n2348: ZN = zn_min(r_c271, n2347);
    let n2349: ZN = zsel_n(n1382, n2346, n2348);
    let n2350: ZN = zsel_n(n1418, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2351: ZN = zn_sub(n595, n2350);
    let n2352: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2351);
    let n2353: ZN = zn_add(n595, n2350);
    let n2354: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2353);
    let n2355: ZN = zsel_n(n1421, n2352, n2354);
    let n2356: ZN = zsel_n(n1351, n2355, n595);
    let n2357: ZN = zn_neg(n1432);
    let n2358: ZN = zn_mul(n2357, zn_splat(P8::from_raw(131072i32)));
    let n2359: ZN = zsel_n(n1434, n2358, n1412);
    let n2360: ZN = zsel_n(n1434, zn_splat(P8::from_raw(-131072i32)), n2356);
    let n2361: ZN = zsel_n(n1423, zn_splat(P8::from_raw(0i32)), n1368);
    let n2362: ZN = zsel_n(n1423, n1412, n2359);
    let n2363: ZN = zsel_n(n1423, zn_splat(P8::from_raw(-131072i32)), n2360);
    let n2364: ZN = zn_sub(n1367, zn_splat(P8::from_raw(65536i32)));
    let n2365: ZN = zsel_n(n1441, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2366: ZN = zsel_n(n1439, zn_splat(P8::from_raw(131072i32)), n2365);
    let n2367: ZN = zsel_n(n1444, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2368: ZN = zsel_n(n1378, n2339, r_c236);
    let n2369: ZB = zsel_b(n1378, r_c272, n1416);
    let n2370: ZN = zsel_n(n1378, n2344, n1412);
    let n2371: ZN = zsel_n(n1378, n2349, n2356);
    let n2372: ZB = zb_and(n1492, n1895);
    let n2373: ZN = zsel_n(r_c43, r_c234, n2338);
    let n2374: ZN = zsel_n(r_c43, r_c236, n2368);
    let n2375: ZN = zsel_n(r_c43, r_c237, n1367);
    let n2376: ZN = zsel_n(r_c43, r_c239, n1368);
    let n2377: ZB = zb_and(r_c43, r_c246);
    let n2378: ZB = zb_and(r_c43, r_c247);
    let n2379: ZB = zsel_b(r_c43, r_c272, n2369);
    let n2380: ZN = zsel_n(r_c43, n594, n2370);
    let n2381: ZN = zsel_n(r_c43, n595, n2371);
    let n2382: ZB = zb_or(n2337, n2372);
    let n2383: ZB = zsel_b(r_c43, n597, n1337);
    let n2384: ZN = zsel_n(n31, r_c39, n1830);
    let n2385: ZN = zsel_n(n31, n2292, r_c20);
    let n2386: ZN = zsel_n(n31, r_c234, n2373);
    let n2387: ZN = zsel_n(n31, r_c236, n2374);
    let n2388: ZN = zsel_n(n31, r_c237, n2375);
    let n2389: ZN = zsel_n(n31, r_c239, n2376);
    let n2390: ZB = zsel_b(n31, r_c246, n2377);
    let n2391: ZB = zsel_b(n31, r_c247, n2378);
    let n2392: ZN = zsel_n(n31, r_c253, n592);
    let n2393: ZN = zsel_n(n31, r_c254, n593);
    let n2394: ZB = zsel_b(n31, r_c272, n2379);
    let n2395: ZN = zsel_n(n31, r_c278, n2335);
    let n2396: ZN = zsel_n(n31, r_c279, n2336);
    let n2397: ZN = zsel_n(n31, r_c280, n2380);
    let n2398: ZN = zsel_n(n31, r_c281, n2381);
    let n2399: ZB = zb_or(n31, n2382);
    let n2400: ZB = zb_or(n31, n2383);
    let n2401: ZB = zn_gt(n2385, zn_splat(P8::from_raw(0i32)));
    let n2402: ZB = zn_le(n2385, zn_splat(P8::from_raw(0i32)));
    let n2403: ZB = zb_and(n2399, n2401);
    let n2404: ZB = zb_and(n2399, n2402);
    let n2405: ZB = zn_lt(n2392, zn_splat(P8::from_raw(-65536i32)));
    let n2406: ZB = zn_ge(n2392, zn_splat(P8::from_raw(-65536i32)));
    let n2407: ZB = zb_and(n2404, n2406);
    let n2408: ZB = zb_and(n2404, n2405);
    let n2409: ZB = zn_gt(n2392, zn_splat(P8::from_raw(7929856i32)));
    let n2410: ZB = zb_or(n2407, n2408);
    let n2411: ZB = zb_or(n2405, n2409);
    let n2412: ZB = zb_not(n2411);
    let n2413: ZB = zb_and(n2410, n2411);
    let n2414: ZB = zb_and(n2410, n2412);
    let n2415: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2392);
    let n2416: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2415);
    let n2417: ZN = zsel_n(n2411, n2416, n2392);
    let n2418: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2397);
    let n2419: ZB = zb_or(n2413, n2414);
    let n2420: ZN = zsel_n(n2401, n2392, n2417);
    let n2421: ZN = zsel_n(n2401, n2397, n2418);
    let n2422: ZB = zb_or(n2403, n2419);
    let n2423: ZN = zn_max(n1511, n2351);
    let n2424: ZN = zn_min(n1511, n2353);
    let n2425: ZN = zsel_n(n1512, n2423, n2424);
    let n2426: ZN = zsel_n(n1351, n2425, n595);
    let n2427: ZN = zsel_n(n1434, n2358, n1503);
    let n2428: ZN = zsel_n(n1434, zn_splat(P8::from_raw(-131072i32)), n2426);
    let n2429: ZN = zsel_n(n1423, n1503, n2427);
    let n2430: ZN = zsel_n(n1423, zn_splat(P8::from_raw(-131072i32)), n2428);
    let n2431: ZB = zsel_b(n1378, r_c272, n1507);
    let n2432: ZN = zsel_n(n1378, n2344, n1503);
    let n2433: ZN = zsel_n(n1378, n2349, n2426);
    let n2434: ZB = zb_and(n1492, n1939);
    let n2435: ZB = zsel_b(r_c43, r_c272, n2431);
    let n2436: ZN = zsel_n(r_c43, n594, n2432);
    let n2437: ZN = zsel_n(r_c43, n595, n2433);
    let n2438: ZB = zb_or(n2337, n2434);
    let n2439: ZB = zsel_b(n31, r_c272, n2435);
    let n2440: ZN = zsel_n(n31, r_c280, n2436);
    let n2441: ZN = zsel_n(n31, r_c281, n2437);
    let n2442: ZB = zb_or(n31, n2438);
    let n2443: ZB = zb_and(n2401, n2442);
    let n2444: ZB = zb_and(n2402, n2442);
    let n2445: ZB = zb_and(n2406, n2444);
    let n2446: ZB = zb_and(n2405, n2444);
    let n2447: ZB = zb_or(n2445, n2446);
    let n2448: ZB = zb_and(n2411, n2447);
    let n2449: ZB = zb_and(n2412, n2447);
    let n2450: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2440);
    let n2451: ZB = zb_or(n2448, n2449);
    let n2452: ZN = zsel_n(n2401, n2440, n2450);
    let n2453: ZB = zb_or(n2443, n2451);
    let n2454: ZN = zn_max(n1562, n2351);
    let n2455: ZN = zn_min(n1562, n2353);
    let n2456: ZN = zsel_n(n1563, n2454, n2455);
    let n2457: ZN = zsel_n(n1351, n2456, n595);
    let n2458: ZN = zsel_n(n1434, n2358, n1554);
    let n2459: ZN = zsel_n(n1434, zn_splat(P8::from_raw(-131072i32)), n2457);
    let n2460: ZN = zsel_n(n1423, n1554, n2458);
    let n2461: ZN = zsel_n(n1423, zn_splat(P8::from_raw(-131072i32)), n2459);
    let n2462: ZB = zsel_b(n1378, r_c272, n1558);
    let n2463: ZN = zsel_n(n1378, n2344, n1554);
    let n2464: ZN = zsel_n(n1378, n2349, n2457);
    let n2465: ZB = zb_and(n1492, n1983);
    let n2466: ZB = zsel_b(r_c43, r_c272, n2462);
    let n2467: ZN = zsel_n(r_c43, n594, n2463);
    let n2468: ZN = zsel_n(r_c43, n595, n2464);
    let n2469: ZB = zb_or(n2337, n2465);
    let n2470: ZB = zsel_b(n31, r_c272, n2466);
    let n2471: ZN = zsel_n(n31, r_c280, n2467);
    let n2472: ZN = zsel_n(n31, r_c281, n2468);
    let n2473: ZB = zb_or(n31, n2469);
    let n2474: ZB = zb_and(n2401, n2473);
    let n2475: ZB = zb_and(n2402, n2473);
    let n2476: ZB = zb_and(n2406, n2475);
    let n2477: ZB = zb_and(n2405, n2475);
    let n2478: ZB = zb_or(n2476, n2477);
    let n2479: ZB = zb_and(n2411, n2478);
    let n2480: ZB = zb_and(n2412, n2478);
    let n2481: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2471);
    let n2482: ZB = zb_or(n2479, n2480);
    let n2483: ZN = zsel_n(n2401, n2471, n2481);
    let n2484: ZB = zb_or(n2474, n2482);
    let n2485: ZN = zsel_n(n1358, n2361, n1368);
    let n2486: ZN = zsel_n(n1358, n2362, n1412);
    let n2487: ZN = zsel_n(n1358, n2363, n2356);
    let n2488: ZN = zsel_n(n1378, n1368, n2485);
    let n2489: ZN = zsel_n(n1378, n2344, n2486);
    let n2490: ZN = zsel_n(n1378, n2349, n2487);
    let n2491: ZB = zb_and(n1492, n2026);
    let n2492: ZN = zsel_n(r_c43, r_c239, n2488);
    let n2493: ZB = zb_or(r_c247, n598);
    let n2494: ZN = zsel_n(r_c43, n594, n2489);
    let n2495: ZN = zsel_n(r_c43, n595, n2490);
    let n2496: ZB = zb_or(n2337, n2491);
    let n2497: ZN = zsel_n(n31, r_c239, n2492);
    let n2498: ZB = zsel_b(n31, r_c247, n2493);
    let n2499: ZN = zsel_n(n31, r_c280, n2494);
    let n2500: ZN = zsel_n(n31, r_c281, n2495);
    let n2501: ZB = zb_or(n31, n2496);
    let n2502: ZB = zb_and(n2401, n2501);
    let n2503: ZB = zb_and(n2402, n2501);
    let n2504: ZB = zb_and(n2406, n2503);
    let n2505: ZB = zb_and(n2405, n2503);
    let n2506: ZB = zb_or(n2504, n2505);
    let n2507: ZB = zb_and(n2411, n2506);
    let n2508: ZB = zb_and(n2412, n2506);
    let n2509: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2499);
    let n2510: ZB = zb_or(n2507, n2508);
    let n2511: ZN = zsel_n(n2401, n2499, n2509);
    let n2512: ZB = zb_or(n2502, n2510);
    let n2513: ZN = zsel_n(n1358, n2429, n1503);
    let n2514: ZN = zsel_n(n1358, n2430, n2426);
    let n2515: ZN = zsel_n(n1378, n2344, n2513);
    let n2516: ZN = zsel_n(n1378, n2349, n2514);
    let n2517: ZB = zb_and(n1492, n2069);
    let n2518: ZN = zsel_n(r_c43, n594, n2515);
    let n2519: ZN = zsel_n(r_c43, n595, n2516);
    let n2520: ZB = zb_or(n2337, n2517);
    let n2521: ZN = zsel_n(n31, r_c280, n2518);
    let n2522: ZN = zsel_n(n31, r_c281, n2519);
    let n2523: ZB = zb_or(n31, n2520);
    let n2524: ZB = zb_and(n2401, n2523);
    let n2525: ZB = zb_and(n2402, n2523);
    let n2526: ZB = zb_and(n2406, n2525);
    let n2527: ZB = zb_and(n2405, n2525);
    let n2528: ZB = zb_or(n2526, n2527);
    let n2529: ZB = zb_and(n2411, n2528);
    let n2530: ZB = zb_and(n2412, n2528);
    let n2531: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2521);
    let n2532: ZB = zb_or(n2529, n2530);
    let n2533: ZN = zsel_n(n2401, n2521, n2531);
    let n2534: ZB = zb_or(n2524, n2532);
    let n2535: ZN = zsel_n(n1358, n2460, n1554);
    let n2536: ZN = zsel_n(n1358, n2461, n2457);
    let n2537: ZN = zsel_n(n1378, n2344, n2535);
    let n2538: ZN = zsel_n(n1378, n2349, n2536);
    let n2539: ZB = zb_and(n1492, n2112);
    let n2540: ZN = zsel_n(r_c43, n594, n2537);
    let n2541: ZN = zsel_n(r_c43, n595, n2538);
    let n2542: ZB = zb_or(n2337, n2539);
    let n2543: ZN = zsel_n(n31, r_c280, n2540);
    let n2544: ZN = zsel_n(n31, r_c281, n2541);
    let n2545: ZB = zb_or(n31, n2542);
    let n2546: ZB = zb_and(n2401, n2545);
    let n2547: ZB = zb_and(n2402, n2545);
    let n2548: ZB = zb_and(n2406, n2547);
    let n2549: ZB = zb_and(n2405, n2547);
    let n2550: ZB = zb_or(n2548, n2549);
    let n2551: ZB = zb_and(n2411, n2550);
    let n2552: ZB = zb_and(n2412, n2550);
    let n2553: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2543);
    let n2554: ZB = zb_or(n2551, n2552);
    let n2555: ZN = zsel_n(n2401, n2543, n2553);
    let n2556: ZB = zb_or(n2546, n2554);
    let n2557: ZN = zsel_n(n1710, zn_splat(P8::from_raw(655360i32)), n2338);
    let n2558: ZN = zsel_n(n1710, zn_splat(P8::from_raw(262144i32)), r_c236);
    let n2559: ZN = zsel_n(n1710, n2364, n1367);
    let n2560: ZN = zsel_n(n1710, zn_splat(P8::from_raw(98304i32)), r_c268);
    let n2561: ZN = zsel_n(n1710, n2367, r_c269);
    let n2562: ZN = zsel_n(n1710, n2366, r_c270);
    let n2563: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), r_c271);
    let n2564: ZN = zsel_n(n1710, n1438, n1412);
    let n2565: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2356);
    let n2566: ZN = zsel_n(n1378, n2338, n2557);
    let n2567: ZN = zsel_n(n1378, n2339, n2558);
    let n2568: ZN = zsel_n(n1378, n1367, n2559);
    let n2569: ZN = zsel_n(n1378, r_c268, n2560);
    let n2570: ZN = zsel_n(n1378, r_c269, n2561);
    let n2571: ZN = zsel_n(n1378, r_c270, n2562);
    let n2572: ZN = zsel_n(n1378, r_c271, n2563);
    let n2573: ZN = zsel_n(n1378, n2344, n2564);
    let n2574: ZN = zsel_n(n1378, n2349, n2565);
    let n2575: ZB = zb_and(n1492, n2140);
    let n2576: ZN = zsel_n(r_c43, r_c20, n1714);
    let n2577: ZB = zsel_b(r_c43, r_c41, n1715);
    let n2578: ZN = zsel_n(r_c43, r_c234, n2566);
    let n2579: ZN = zsel_n(r_c43, r_c236, n2567);
    let n2580: ZN = zsel_n(r_c43, r_c237, n2568);
    let n2581: ZB = zb_or(r_c246, n598);
    let n2582: ZN = zsel_n(r_c43, r_c268, n2569);
    let n2583: ZN = zsel_n(r_c43, r_c269, n2570);
    let n2584: ZN = zsel_n(r_c43, r_c270, n2571);
    let n2585: ZN = zsel_n(r_c43, r_c271, n2572);
    let n2586: ZN = zsel_n(r_c43, n594, n2573);
    let n2587: ZN = zsel_n(r_c43, n595, n2574);
    let n2588: ZB = zb_or(n2337, n2575);
    let n2589: ZN = zsel_n(n31, n2292, n2576);
    let n2590: ZB = zsel_b(n31, r_c41, n2577);
    let n2591: ZN = zsel_n(n31, r_c234, n2578);
    let n2592: ZN = zsel_n(n31, r_c236, n2579);
    let n2593: ZN = zsel_n(n31, r_c237, n2580);
    let n2594: ZB = zsel_b(n31, r_c246, n2581);
    let n2595: ZN = zsel_n(n31, r_c268, n2582);
    let n2596: ZN = zsel_n(n31, r_c269, n2583);
    let n2597: ZN = zsel_n(n31, r_c270, n2584);
    let n2598: ZN = zsel_n(n31, r_c271, n2585);
    let n2599: ZN = zsel_n(n31, r_c280, n2586);
    let n2600: ZN = zsel_n(n31, r_c281, n2587);
    let n2601: ZB = zb_or(n31, n2588);
    let n2602: ZB = zn_gt(n2589, zn_splat(P8::from_raw(0i32)));
    let n2603: ZB = zn_le(n2589, zn_splat(P8::from_raw(0i32)));
    let n2604: ZB = zb_and(n2601, n2602);
    let n2605: ZB = zb_and(n2601, n2603);
    let n2606: ZB = zb_and(n2406, n2605);
    let n2607: ZB = zb_and(n2405, n2605);
    let n2608: ZB = zb_or(n2606, n2607);
    let n2609: ZB = zb_and(n2411, n2608);
    let n2610: ZB = zb_and(n2412, n2608);
    let n2611: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2599);
    let n2612: ZB = zb_or(n2609, n2610);
    let n2613: ZN = zsel_n(n2602, n2392, n2417);
    let n2614: ZN = zsel_n(n2602, n2599, n2611);
    let n2615: ZB = zb_or(n2604, n2612);
    let n2616: ZN = zsel_n(n1710, zn_splat(P8::from_raw(69510i32)), r_c269);
    let n2617: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-131072i32)), r_c270);
    let n2618: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-327680i32)), n1503);
    let n2619: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2426);
    let n2620: ZN = zsel_n(n1378, r_c269, n2616);
    let n2621: ZN = zsel_n(n1378, r_c270, n2617);
    let n2622: ZN = zsel_n(n1378, n2344, n2618);
    let n2623: ZN = zsel_n(n1378, n2349, n2619);
    let n2624: ZB = zb_and(n1492, n2158);
    let n2625: ZN = zsel_n(r_c43, r_c269, n2620);
    let n2626: ZN = zsel_n(r_c43, r_c270, n2621);
    let n2627: ZN = zsel_n(r_c43, n594, n2622);
    let n2628: ZN = zsel_n(r_c43, n595, n2623);
    let n2629: ZB = zb_or(n2337, n2624);
    let n2630: ZN = zsel_n(n31, r_c269, n2625);
    let n2631: ZN = zsel_n(n31, r_c270, n2626);
    let n2632: ZN = zsel_n(n31, r_c280, n2627);
    let n2633: ZN = zsel_n(n31, r_c281, n2628);
    let n2634: ZB = zb_or(n31, n2629);
    let n2635: ZB = zb_and(n2602, n2634);
    let n2636: ZB = zb_and(n2603, n2634);
    let n2637: ZB = zb_and(n2406, n2636);
    let n2638: ZB = zb_and(n2405, n2636);
    let n2639: ZB = zb_or(n2637, n2638);
    let n2640: ZB = zb_and(n2411, n2639);
    let n2641: ZB = zb_and(n2412, n2639);
    let n2642: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2632);
    let n2643: ZB = zb_or(n2640, n2641);
    let n2644: ZN = zsel_n(n2602, n2632, n2642);
    let n2645: ZB = zb_or(n2635, n2643);
    let n2646: ZN = zsel_n(n1710, zn_splat(P8::from_raw(131072i32)), r_c270);
    let n2647: ZN = zsel_n(n1710, zn_splat(P8::from_raw(327680i32)), n1554);
    let n2648: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2457);
    let n2649: ZN = zsel_n(n1378, r_c270, n2646);
    let n2650: ZN = zsel_n(n1378, n2344, n2647);
    let n2651: ZN = zsel_n(n1378, n2349, n2648);
    let n2652: ZB = zb_and(n1492, n2176);
    let n2653: ZN = zsel_n(r_c43, r_c270, n2649);
    let n2654: ZN = zsel_n(r_c43, n594, n2650);
    let n2655: ZN = zsel_n(r_c43, n595, n2651);
    let n2656: ZB = zb_or(n2337, n2652);
    let n2657: ZN = zsel_n(n31, r_c270, n2653);
    let n2658: ZN = zsel_n(n31, r_c280, n2654);
    let n2659: ZN = zsel_n(n31, r_c281, n2655);
    let n2660: ZB = zb_or(n31, n2656);
    let n2661: ZB = zb_and(n2602, n2660);
    let n2662: ZB = zb_and(n2603, n2660);
    let n2663: ZB = zb_and(n2406, n2662);
    let n2664: ZB = zb_and(n2405, n2662);
    let n2665: ZB = zb_or(n2663, n2664);
    let n2666: ZB = zb_and(n2411, n2665);
    let n2667: ZB = zb_and(n2412, n2665);
    let n2668: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2658);
    let n2669: ZB = zb_or(n2666, n2667);
    let n2670: ZN = zsel_n(n2602, n2658, n2668);
    let n2671: ZB = zb_or(n2661, n2669);
    let n2673: ZN = zsel_n(n1710, zn_splat(P8::from_raw(69510i32)), r_c268);
    let n2674: ZN = zsel_n(n1710, zn_splat(P8::from_raw(98304i32)), r_c269);
    let n2675: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), r_c270);
    let n2676: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-98304i32)), r_c271);
    let n2677: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n1412);
    let n2678: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-327680i32)), n2356);
    let n2679: ZN = zsel_n(n1378, r_c268, n2673);
    let n2680: ZN = zsel_n(n1378, r_c269, n2674);
    let n2681: ZN = zsel_n(n1378, r_c270, n2675);
    let n2682: ZN = zsel_n(n1378, r_c271, n2676);
    let n2683: ZN = zsel_n(n1378, n2344, n2677);
    let n2684: ZN = zsel_n(n1378, n2349, n2678);
    let n2685: ZB = zb_and(n1492, n2192);
    let n2686: ZN = zsel_n(r_c43, r_c268, n2679);
    let n2687: ZN = zsel_n(r_c43, r_c269, n2680);
    let n2688: ZN = zsel_n(r_c43, r_c270, n2681);
    let n2689: ZN = zsel_n(r_c43, r_c271, n2682);
    let n2690: ZN = zsel_n(r_c43, n594, n2683);
    let n2691: ZN = zsel_n(r_c43, n595, n2684);
    let n2692: ZB = zb_or(n2337, n2685);
    let n2693: ZN = zsel_n(n31, r_c268, n2686);
    let n2694: ZN = zsel_n(n31, r_c269, n2687);
    let n2695: ZN = zsel_n(n31, r_c270, n2688);
    let n2696: ZN = zsel_n(n31, r_c271, n2689);
    let n2697: ZN = zsel_n(n31, r_c280, n2690);
    let n2698: ZN = zsel_n(n31, r_c281, n2691);
    let n2699: ZB = zb_or(n31, n2692);
    let n2700: ZB = zb_and(n2602, n2699);
    let n2701: ZB = zb_and(n2603, n2699);
    let n2702: ZB = zb_and(n2406, n2701);
    let n2703: ZB = zb_and(n2405, n2701);
    let n2704: ZB = zb_or(n2702, n2703);
    let n2705: ZB = zb_and(n2411, n2704);
    let n2706: ZB = zb_and(n2412, n2704);
    let n2707: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2697);
    let n2708: ZB = zb_or(n2705, n2706);
    let n2709: ZN = zsel_n(n2602, n2697, n2707);
    let n2710: ZB = zb_or(n2700, n2708);
    let n2711: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n1503);
    let n2712: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n2426);
    let n2713: ZN = zsel_n(n1378, n2344, n2711);
    let n2714: ZN = zsel_n(n1378, n2349, n2712);
    let n2715: ZN = zsel_n(r_c43, n594, n2713);
    let n2716: ZN = zsel_n(r_c43, n595, n2714);
    let n2717: ZN = zsel_n(n31, r_c280, n2715);
    let n2718: ZN = zsel_n(n31, r_c281, n2716);
    let n2719: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2717);
    let n2720: ZN = zsel_n(n2602, n2717, n2719);
    let n2721: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n1554);
    let n2722: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n2457);
    let n2723: ZN = zsel_n(n1378, n2344, n2721);
    let n2724: ZN = zsel_n(n1378, n2349, n2722);
    let n2725: ZN = zsel_n(r_c43, n594, n2723);
    let n2726: ZN = zsel_n(r_c43, n595, n2724);
    let n2727: ZN = zsel_n(n31, r_c280, n2725);
    let n2728: ZN = zsel_n(n31, r_c281, n2726);
    let n2729: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2727);
    let n2730: ZN = zsel_n(n2602, n2727, n2729);
    let n2731: ZN = zsel_n(n1710, zn_splat(P8::from_raw(131072i32)), r_c271);
    let n2732: ZN = zsel_n(n1710, zn_splat(P8::from_raw(327680i32)), n2356);
    let n2733: ZN = zsel_n(n1378, r_c271, n2731);
    let n2734: ZN = zsel_n(n1378, n2349, n2732);
    let n2735: ZN = zsel_n(r_c43, r_c271, n2733);
    let n2736: ZN = zsel_n(r_c43, n595, n2734);
    let n2737: ZN = zsel_n(n31, r_c271, n2735);
    let n2738: ZN = zsel_n(n31, r_c281, n2736);
    let n2739: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n2426);
    let n2740: ZN = zsel_n(n1378, n2349, n2739);
    let n2741: ZN = zsel_n(r_c43, n595, n2740);
    let n2742: ZN = zsel_n(n31, r_c281, n2741);
    let n2743: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n2457);
    let n2744: ZN = zsel_n(n1378, n2349, n2743);
    let n2745: ZN = zsel_n(r_c43, n595, n2744);
    let n2746: ZN = zsel_n(n31, r_c281, n2745);
    let n2747: ZN = zsel_n(n1710, n1438, n2486);
    let n2748: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2487);
    let n2749: ZN = zsel_n(n1378, n2344, n2747);
    let n2750: ZN = zsel_n(n1378, n2349, n2748);
    let n2751: ZB = zb_and(n1492, n2222);
    let n2752: ZN = zsel_n(r_c43, n594, n2749);
    let n2753: ZN = zsel_n(r_c43, n595, n2750);
    let n2754: ZB = zb_or(n2337, n2751);
    let n2755: ZN = zsel_n(n31, r_c280, n2752);
    let n2756: ZN = zsel_n(n31, r_c281, n2753);
    let n2757: ZB = zb_or(n31, n2754);
    let n2758: ZB = zb_and(n2602, n2757);
    let n2759: ZB = zb_and(n2603, n2757);
    let n2760: ZB = zb_and(n2406, n2759);
    let n2761: ZB = zb_and(n2405, n2759);
    let n2762: ZB = zb_or(n2760, n2761);
    let n2763: ZB = zb_and(n2411, n2762);
    let n2764: ZB = zb_and(n2412, n2762);
    let n2765: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2755);
    let n2766: ZB = zb_or(n2763, n2764);
    let n2767: ZN = zsel_n(n2602, n2755, n2765);
    let n2768: ZB = zb_or(n2758, n2766);
    let n2769: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-327680i32)), n2513);
    let n2770: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2514);
    let n2771: ZN = zsel_n(n1378, n2344, n2769);
    let n2772: ZN = zsel_n(n1378, n2349, n2770);
    let n2773: ZB = zb_and(n1492, n2240);
    let n2774: ZN = zsel_n(r_c43, n594, n2771);
    let n2775: ZN = zsel_n(r_c43, n595, n2772);
    let n2776: ZB = zb_or(n2337, n2773);
    let n2777: ZN = zsel_n(n31, r_c280, n2774);
    let n2778: ZN = zsel_n(n31, r_c281, n2775);
    let n2779: ZB = zb_or(n31, n2776);
    let n2780: ZB = zb_and(n2602, n2779);
    let n2781: ZB = zb_and(n2603, n2779);
    let n2782: ZB = zb_and(n2406, n2781);
    let n2783: ZB = zb_and(n2405, n2781);
    let n2784: ZB = zb_or(n2782, n2783);
    let n2785: ZB = zb_and(n2411, n2784);
    let n2786: ZB = zb_and(n2412, n2784);
    let n2787: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2777);
    let n2788: ZB = zb_or(n2785, n2786);
    let n2789: ZN = zsel_n(n2602, n2777, n2787);
    let n2790: ZB = zb_or(n2780, n2788);
    let n2791: ZN = zsel_n(n1710, zn_splat(P8::from_raw(327680i32)), n2535);
    let n2792: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2536);
    let n2793: ZN = zsel_n(n1378, n2344, n2791);
    let n2794: ZN = zsel_n(n1378, n2349, n2792);
    let n2795: ZB = zb_and(n1492, n2258);
    let n2796: ZN = zsel_n(r_c43, n594, n2793);
    let n2797: ZN = zsel_n(r_c43, n595, n2794);
    let n2798: ZB = zb_or(n2337, n2795);
    let n2799: ZN = zsel_n(n31, r_c280, n2796);
    let n2800: ZN = zsel_n(n31, r_c281, n2797);
    let n2801: ZB = zb_or(n31, n2798);
    let n2802: ZB = zb_and(n2602, n2801);
    let n2803: ZB = zb_and(n2603, n2801);
    let n2804: ZB = zb_and(n2406, n2803);
    let n2805: ZB = zb_and(n2405, n2803);
    let n2806: ZB = zb_or(n2804, n2805);
    let n2807: ZB = zb_and(n2411, n2806);
    let n2808: ZB = zb_and(n2412, n2806);
    let n2809: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2799);
    let n2810: ZB = zb_or(n2807, n2808);
    let n2811: ZN = zsel_n(n2602, n2799, n2809);
    let n2812: ZB = zb_or(n2802, n2810);
    let n2813: ZN = zsel_n(n1710, zn_splat(P8::from_raw(0i32)), n2486);
    let n2814: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-327680i32)), n2487);
    let n2815: ZN = zsel_n(n1378, n2344, n2813);
    let n2816: ZN = zsel_n(n1378, n2349, n2814);
    let n2817: ZB = zb_and(n1492, n2274);
    let n2818: ZN = zsel_n(r_c43, n594, n2815);
    let n2819: ZN = zsel_n(r_c43, n595, n2816);
    let n2820: ZB = zb_or(n2337, n2817);
    let n2821: ZN = zsel_n(n31, r_c280, n2818);
    let n2822: ZN = zsel_n(n31, r_c281, n2819);
    let n2823: ZB = zb_or(n31, n2820);
    let n2824: ZB = zb_and(n2602, n2823);
    let n2825: ZB = zb_and(n2603, n2823);
    let n2826: ZB = zb_and(n2406, n2825);
    let n2827: ZB = zb_and(n2405, n2825);
    let n2828: ZB = zb_or(n2826, n2827);
    let n2829: ZB = zb_and(n2411, n2828);
    let n2830: ZB = zb_and(n2412, n2828);
    let n2831: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2821);
    let n2832: ZB = zb_or(n2829, n2830);
    let n2833: ZN = zsel_n(n2602, n2821, n2831);
    let n2834: ZB = zb_or(n2824, n2832);
    let n2835: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n2513);
    let n2836: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n2514);
    let n2837: ZN = zsel_n(n1378, n2344, n2835);
    let n2838: ZN = zsel_n(n1378, n2349, n2836);
    let n2839: ZN = zsel_n(r_c43, n594, n2837);
    let n2840: ZN = zsel_n(r_c43, n595, n2838);
    let n2841: ZN = zsel_n(n31, r_c280, n2839);
    let n2842: ZN = zsel_n(n31, r_c281, n2840);
    let n2843: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2841);
    let n2844: ZN = zsel_n(n2602, n2841, n2843);
    let n2845: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n2535);
    let n2846: ZN = zsel_n(n1710, zn_splat(P8::from_raw(-231700i32)), n2536);
    let n2847: ZN = zsel_n(n1378, n2344, n2845);
    let n2848: ZN = zsel_n(n1378, n2349, n2846);
    let n2849: ZN = zsel_n(r_c43, n594, n2847);
    let n2850: ZN = zsel_n(r_c43, n595, n2848);
    let n2851: ZN = zsel_n(n31, r_c280, n2849);
    let n2852: ZN = zsel_n(n31, r_c281, n2850);
    let n2853: ZN = zsel_n(n2411, zn_splat(P8::from_raw(0i32)), n2851);
    let n2854: ZN = zsel_n(n2602, n2851, n2853);
    let n2855: ZN = zsel_n(n1710, zn_splat(P8::from_raw(327680i32)), n2487);
    let n2856: ZN = zsel_n(n1378, n2349, n2855);
    let n2857: ZN = zsel_n(r_c43, n595, n2856);
    let n2858: ZN = zsel_n(n31, r_c281, n2857);
    let n2859: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n2514);
    let n2860: ZN = zsel_n(n1378, n2349, n2859);
    let n2861: ZN = zsel_n(r_c43, n595, n2860);
    let n2862: ZN = zsel_n(n31, r_c281, n2861);
    let n2863: ZN = zsel_n(n1710, zn_splat(P8::from_raw(231700i32)), n2536);
    let n2864: ZN = zsel_n(n1378, n2349, n2863);
    let n2865: ZN = zsel_n(r_c43, n595, n2864);
    let n2866: ZN = zsel_n(n31, r_c281, n2865);
    let n2869: ZW = zw_bits_n(r_c20);
    let n2870: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2869, 20u64);
    let n2871: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2869, 20u64);
    let n2872: ZW = zw_bits_n(n34);
    let n2873: ZW = zw_mix1(n2870, n2872, 39u64);
    let n2874: ZW = zw_mix2(n2871, n2872, 39u64);
    let n2875: ZW = zw_bits_b(r_c43);
    let n2876: ZW = zw_mix1(n2873, n2875, 43u64);
    let n2877: ZW = zw_mix2(n2874, n2875, 43u64);
    let n2878: ZW = zw_bits_n(n27);
    let n2879: ZW = zw_mix1(n2876, n2878, 84u64);
    let n2880: ZW = zw_mix2(n2877, n2878, 84u64);
    let n2881: ZW = zw_bits_n(n45);
    let n2882: ZW = zw_mix1(n2879, n2881, 85u64);
    let n2883: ZW = zw_mix2(n2880, n2881, 85u64);
    let n2884: ZW = zw_bits_n(n44);
    let n2885: ZW = zw_mix1(n2882, n2884, 86u64);
    let n2886: ZW = zw_mix2(n2883, n2884, 86u64);
    let n2887: ZW = zw_bits_n(r_c87);
    let n2888: ZW = zw_mix1(n2885, n2887, 87u64);
    let n2889: ZW = zw_mix2(n2886, n2887, 87u64);
    let n2890: ZW = zw_bits_n(r_c88);
    let n2891: ZW = zw_mix1(n2888, n2890, 88u64);
    let n2892: ZW = zw_mix2(n2889, n2890, 88u64);
    let n2893: ZW = zw_bits_b(r_c42);
    let n2894: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2893, 42u64);
    let n2895: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2893, 42u64);
    let n2896: ZW = zw_mix1(n2894, n2875, 43u64);
    let n2897: ZW = zw_mix2(n2895, n2875, 43u64);
    let n2898: ZW = zw_mix1(n2896, n2878, 84u64);
    let n2899: ZW = zw_mix2(n2897, n2878, 84u64);
    let n2900: ZW = zw_mix1(n2898, n2881, 85u64);
    let n2901: ZW = zw_mix2(n2899, n2881, 85u64);
    let n2902: ZW = zw_mix1(n2900, n2884, 86u64);
    let n2903: ZW = zw_mix2(n2901, n2884, 86u64);
    let n2904: ZW = zw_bits_n(n1345);
    let n2905: ZW = zw_mix1(n2902, n2904, 87u64);
    let n2906: ZW = zw_mix2(n2903, n2904, 87u64);
    let n2907: ZW = zw_mix1(n2905, n2890, 88u64);
    let n2908: ZW = zw_mix2(n2906, n2890, 88u64);
    let n2909: ZW = zw_mix1(n2907, n2869, 20u64);
    let n2910: ZW = zw_mix2(n2908, n2869, 20u64);
    let n2911: ZW = zw_bits_b(r_c41);
    let n2912: ZW = zw_mix1(n2909, n2911, 41u64);
    let n2913: ZW = zw_mix2(n2910, n2911, 41u64);
    let n2914: ZW = zw_bits_n(n1714);
    let n2915: ZW = zw_mix1(n2907, n2914, 20u64);
    let n2916: ZW = zw_mix2(n2908, n2914, 20u64);
    let n2917: ZW = zw_bits_b(n1715);
    let n2918: ZW = zw_mix1(n2915, n2917, 41u64);
    let n2919: ZW = zw_mix2(n2916, n2917, 41u64);
    let n2920: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2875, 43u64);
    let n2921: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2875, 43u64);
    let n2922: ZW = zw_mix1(n2920, n2878, 84u64);
    let n2923: ZW = zw_mix2(n2921, n2878, 84u64);
    let n2924: ZW = zw_mix1(n2922, n2881, 85u64);
    let n2925: ZW = zw_mix2(n2923, n2881, 85u64);
    let n2926: ZW = zw_mix1(n2924, n2884, 86u64);
    let n2927: ZW = zw_mix2(n2925, n2884, 86u64);
    let n2928: ZW = zw_mix1(n2926, n2890, 88u64);
    let n2929: ZW = zw_mix2(n2927, n2890, 88u64);
    let n2930: ZW = zw_mix1(n2928, n2869, 20u64);
    let n2931: ZW = zw_mix2(n2929, n2869, 20u64);
    let n2932: ZW = zw_bits_b(n1901);
    let n2933: ZW = zw_mix1(n2930, n2932, 38u64);
    let n2934: ZW = zw_mix2(n2931, n2932, 38u64);
    let n2935: ZW = zw_bits_n(n1899);
    let n2936: ZW = zw_mix1(n2933, n2935, 39u64);
    let n2937: ZW = zw_mix2(n2934, n2935, 39u64);
    let n2938: ZW = zw_bits_n(n1898);
    let n2939: ZW = zw_mix1(n2936, n2938, 87u64);
    let n2940: ZW = zw_mix2(n2937, n2938, 87u64);
    let n2941: ZW = zw_bits_b(n1945);
    let n2942: ZW = zw_mix1(n2930, n2941, 38u64);
    let n2943: ZW = zw_mix2(n2931, n2941, 38u64);
    let n2944: ZW = zw_bits_n(n1943);
    let n2945: ZW = zw_mix1(n2942, n2944, 39u64);
    let n2946: ZW = zw_mix2(n2943, n2944, 39u64);
    let n2947: ZW = zw_bits_n(n1942);
    let n2948: ZW = zw_mix1(n2945, n2947, 87u64);
    let n2949: ZW = zw_mix2(n2946, n2947, 87u64);
    let n2950: ZW = zw_bits_b(n1989);
    let n2951: ZW = zw_mix1(n2930, n2950, 38u64);
    let n2952: ZW = zw_mix2(n2931, n2950, 38u64);
    let n2953: ZW = zw_bits_n(n1987);
    let n2954: ZW = zw_mix1(n2951, n2953, 39u64);
    let n2955: ZW = zw_mix2(n2952, n2953, 39u64);
    let n2956: ZW = zw_bits_n(n1986);
    let n2957: ZW = zw_mix1(n2954, n2956, 87u64);
    let n2958: ZW = zw_mix2(n2955, n2956, 87u64);
    let n2959: ZW = zw_bits_b(n2032);
    let n2960: ZW = zw_mix1(n2930, n2959, 38u64);
    let n2961: ZW = zw_mix2(n2931, n2959, 38u64);
    let n2962: ZW = zw_bits_n(n2030);
    let n2963: ZW = zw_mix1(n2960, n2962, 39u64);
    let n2964: ZW = zw_mix2(n2961, n2962, 39u64);
    let n2965: ZW = zw_bits_n(n2029);
    let n2966: ZW = zw_mix1(n2963, n2965, 87u64);
    let n2967: ZW = zw_mix2(n2964, n2965, 87u64);
    let n2968: ZW = zw_bits_b(n2075);
    let n2969: ZW = zw_mix1(n2930, n2968, 38u64);
    let n2970: ZW = zw_mix2(n2931, n2968, 38u64);
    let n2971: ZW = zw_bits_n(n2073);
    let n2972: ZW = zw_mix1(n2969, n2971, 39u64);
    let n2973: ZW = zw_mix2(n2970, n2971, 39u64);
    let n2974: ZW = zw_bits_n(n2072);
    let n2975: ZW = zw_mix1(n2972, n2974, 87u64);
    let n2976: ZW = zw_mix2(n2973, n2974, 87u64);
    let n2977: ZW = zw_bits_b(n2118);
    let n2978: ZW = zw_mix1(n2930, n2977, 38u64);
    let n2979: ZW = zw_mix2(n2931, n2977, 38u64);
    let n2980: ZW = zw_bits_n(n2116);
    let n2981: ZW = zw_mix1(n2978, n2980, 39u64);
    let n2982: ZW = zw_mix2(n2979, n2980, 39u64);
    let n2983: ZW = zw_bits_n(n2115);
    let n2984: ZW = zw_mix1(n2981, n2983, 87u64);
    let n2985: ZW = zw_mix2(n2982, n2983, 87u64);
    let n2986: ZW = zw_mix1(n2928, n2914, 20u64);
    let n2987: ZW = zw_mix2(n2929, n2914, 20u64);
    let n2988: ZW = zw_bits_b(n2146);
    let n2989: ZW = zw_mix1(n2986, n2988, 38u64);
    let n2990: ZW = zw_mix2(n2987, n2988, 38u64);
    let n2991: ZW = zw_bits_n(n2144);
    let n2992: ZW = zw_mix1(n2989, n2991, 39u64);
    let n2993: ZW = zw_mix2(n2990, n2991, 39u64);
    let n2994: ZW = zw_bits_n(n2143);
    let n2995: ZW = zw_mix1(n2992, n2994, 87u64);
    let n2996: ZW = zw_mix2(n2993, n2994, 87u64);
    let n2997: ZW = zw_bits_b(n2164);
    let n2998: ZW = zw_mix1(n2986, n2997, 38u64);
    let n2999: ZW = zw_mix2(n2987, n2997, 38u64);
    let n3000: ZW = zw_bits_n(n2162);
    let n3001: ZW = zw_mix1(n2998, n3000, 39u64);
    let n3002: ZW = zw_mix2(n2999, n3000, 39u64);
    let n3003: ZW = zw_bits_n(n2161);
    let n3004: ZW = zw_mix1(n3001, n3003, 87u64);
    let n3005: ZW = zw_mix2(n3002, n3003, 87u64);
    let n3006: ZW = zw_bits_b(n2182);
    let n3007: ZW = zw_mix1(n2986, n3006, 38u64);
    let n3008: ZW = zw_mix2(n2987, n3006, 38u64);
    let n3009: ZW = zw_bits_n(n2180);
    let n3010: ZW = zw_mix1(n3007, n3009, 39u64);
    let n3011: ZW = zw_mix2(n3008, n3009, 39u64);
    let n3012: ZW = zw_bits_n(n2179);
    let n3013: ZW = zw_mix1(n3010, n3012, 87u64);
    let n3014: ZW = zw_mix2(n3011, n3012, 87u64);
    let n3015: ZW = zw_bits_b(n2198);
    let n3016: ZW = zw_mix1(n2986, n3015, 38u64);
    let n3017: ZW = zw_mix2(n2987, n3015, 38u64);
    let n3018: ZW = zw_bits_n(n2196);
    let n3019: ZW = zw_mix1(n3016, n3018, 39u64);
    let n3020: ZW = zw_mix2(n3017, n3018, 39u64);
    let n3021: ZW = zw_bits_n(n2195);
    let n3022: ZW = zw_mix1(n3019, n3021, 87u64);
    let n3023: ZW = zw_mix2(n3020, n3021, 87u64);
    let n3024: ZW = zw_bits_b(n2228);
    let n3025: ZW = zw_mix1(n2986, n3024, 38u64);
    let n3026: ZW = zw_mix2(n2987, n3024, 38u64);
    let n3027: ZW = zw_bits_n(n2226);
    let n3028: ZW = zw_mix1(n3025, n3027, 39u64);
    let n3029: ZW = zw_mix2(n3026, n3027, 39u64);
    let n3030: ZW = zw_bits_n(n2225);
    let n3031: ZW = zw_mix1(n3028, n3030, 87u64);
    let n3032: ZW = zw_mix2(n3029, n3030, 87u64);
    let n3033: ZW = zw_bits_b(n2246);
    let n3034: ZW = zw_mix1(n2986, n3033, 38u64);
    let n3035: ZW = zw_mix2(n2987, n3033, 38u64);
    let n3036: ZW = zw_bits_n(n2244);
    let n3037: ZW = zw_mix1(n3034, n3036, 39u64);
    let n3038: ZW = zw_mix2(n3035, n3036, 39u64);
    let n3039: ZW = zw_bits_n(n2243);
    let n3040: ZW = zw_mix1(n3037, n3039, 87u64);
    let n3041: ZW = zw_mix2(n3038, n3039, 87u64);
    let n3042: ZW = zw_bits_b(n2264);
    let n3043: ZW = zw_mix1(n2986, n3042, 38u64);
    let n3044: ZW = zw_mix2(n2987, n3042, 38u64);
    let n3045: ZW = zw_bits_n(n2262);
    let n3046: ZW = zw_mix1(n3043, n3045, 39u64);
    let n3047: ZW = zw_mix2(n3044, n3045, 39u64);
    let n3048: ZW = zw_bits_n(n2261);
    let n3049: ZW = zw_mix1(n3046, n3048, 87u64);
    let n3050: ZW = zw_mix2(n3047, n3048, 87u64);
    let n3051: ZW = zw_bits_b(n2280);
    let n3052: ZW = zw_mix1(n2986, n3051, 38u64);
    let n3053: ZW = zw_mix2(n2987, n3051, 38u64);
    let n3054: ZW = zw_bits_n(n2278);
    let n3055: ZW = zw_mix1(n3052, n3054, 39u64);
    let n3056: ZW = zw_mix2(n3053, n3054, 39u64);
    let n3057: ZW = zw_bits_n(n2277);
    let n3058: ZW = zw_mix1(n3055, n3057, 87u64);
    let n3059: ZW = zw_mix2(n3056, n3057, 87u64);
    let n3060: ZW = zw_bits_b(r_c38);
    let n3061: ZW = zw_mix1(zw_splat(11400714819323198485u64), n3060, 38u64);
    let n3062: ZW = zw_mix2(zw_splat(11562461410679940143u64), n3060, 38u64);
    let n3063: ZW = zw_bits_n(n2384);
    let n3064: ZW = zw_mix1(n3061, n3063, 39u64);
    let n3065: ZW = zw_mix2(n3062, n3063, 39u64);
    let n3066: ZW = zw_mix1(n3064, n2893, 42u64);
    let n3067: ZW = zw_mix2(n3065, n2893, 42u64);
    let n3068: ZW = zw_mix1(n3066, n2875, 43u64);
    let n3069: ZW = zw_mix2(n3067, n2875, 43u64);
    let n3070: ZW = zw_mix1(n3068, n2878, 84u64);
    let n3071: ZW = zw_mix2(n3069, n2878, 84u64);
    let n3072: ZW = zw_mix1(n3070, n2881, 85u64);
    let n3073: ZW = zw_mix2(n3071, n2881, 85u64);
    let n3074: ZW = zw_mix1(n3072, n2884, 86u64);
    let n3075: ZW = zw_mix2(n3073, n2884, 86u64);
    let n3076: ZW = zw_mix1(n3074, n2887, 87u64);
    let n3077: ZW = zw_mix2(n3075, n2887, 87u64);
    let n3078: ZW = zw_mix1(n3076, n2890, 88u64);
    let n3079: ZW = zw_mix2(n3077, n2890, 88u64);
    let n3080: ZW = zw_bits_b(r_c232);
    let n3081: ZW = zw_mix1(n3078, n3080, 232u64);
    let n3082: ZW = zw_mix2(n3079, n3080, 232u64);
    let n3083: ZW = zw_bits_b(r_c249);
    let n3084: ZW = zw_mix1(n3081, n3083, 249u64);
    let n3085: ZW = zw_mix2(n3082, n3083, 249u64);
    let n3086: ZW = zw_bits_n(n2393);
    let n3087: ZW = zw_mix1(n3084, n3086, 254u64);
    let n3088: ZW = zw_mix2(n3085, n3086, 254u64);
    let n3089: ZW = zw_bits_b(r_c273);
    let n3090: ZW = zw_mix1(n3087, n3089, 273u64);
    let n3091: ZW = zw_mix2(n3088, n3089, 273u64);
    let n3092: u64 = u.c274.as_raw_u32() as u64;
    let n3093: ZW = zw_mix1(n3090, zw_splat(n3092), 274u64);
    let n3094: ZW = zw_mix2(n3091, zw_splat(n3092), 274u64);
    let n3095: u64 = u.c275.as_raw_u32() as u64;
    let n3096: ZW = zw_mix1(n3093, zw_splat(n3095), 275u64);
    let n3097: ZW = zw_mix2(n3094, zw_splat(n3095), 275u64);
    let n3098: u64 = u.c276.as_raw_u32() as u64;
    let n3099: ZW = zw_mix1(n3096, zw_splat(n3098), 276u64);
    let n3100: ZW = zw_mix2(n3097, zw_splat(n3098), 276u64);
    let n3101: u64 = u.c277.as_raw_u32() as u64;
    let n3102: ZW = zw_mix1(n3099, zw_splat(n3101), 277u64);
    let n3103: ZW = zw_mix2(n3100, zw_splat(n3101), 277u64);
    let n3104: ZW = zw_bits_n(n2395);
    let n3105: ZW = zw_mix1(n3102, n3104, 278u64);
    let n3106: ZW = zw_mix2(n3103, n3104, 278u64);
    let n3107: ZW = zw_bits_n(n2396);
    let n3108: ZW = zw_mix1(n3105, n3107, 279u64);
    let n3109: ZW = zw_mix2(n3106, n3107, 279u64);
    let n3110: ZW = zw_bits_n(n2385);
    let n3111: ZW = zw_mix1(n3108, n3110, 20u64);
    let n3112: ZW = zw_mix2(n3109, n3110, 20u64);
    let n3113: ZW = zw_mix1(n3111, n2911, 41u64);
    let n3114: ZW = zw_mix2(n3112, n2911, 41u64);
    let n3115: ZW = zw_bits_n(n2386);
    let n3116: ZW = zw_mix1(n3113, n3115, 234u64);
    let n3117: ZW = zw_mix2(n3114, n3115, 234u64);
    let n3118: ZW = zw_bits_n(n2387);
    let n3119: ZW = zw_mix1(n3116, n3118, 236u64);
    let n3120: ZW = zw_mix2(n3117, n3118, 236u64);
    let n3121: ZW = zw_bits_n(n2388);
    let n3122: ZW = zw_mix1(n3119, n3121, 237u64);
    let n3123: ZW = zw_mix2(n3120, n3121, 237u64);
    let n3124: ZW = zw_bits_n(n2389);
    let n3125: ZW = zw_mix1(n3122, n3124, 239u64);
    let n3126: ZW = zw_mix2(n3123, n3124, 239u64);
    let n3127: ZW = zw_bits_b(n2390);
    let n3128: ZW = zw_mix1(n3125, n3127, 246u64);
    let n3129: ZW = zw_mix2(n3126, n3127, 246u64);
    let n3130: ZW = zw_bits_b(n2391);
    let n3131: ZW = zw_mix1(n3128, n3130, 247u64);
    let n3132: ZW = zw_mix2(n3129, n3130, 247u64);
    let n3133: ZW = zw_bits_n(n2420);
    let n3134: ZW = zw_mix1(n3131, n3133, 253u64);
    let n3135: ZW = zw_mix2(n3132, n3133, 253u64);
    let n3136: ZW = zw_bits_n(r_c268);
    let n3137: ZW = zw_mix1(n3134, n3136, 268u64);
    let n3138: ZW = zw_mix2(n3135, n3136, 268u64);
    let n3139: ZW = zw_bits_n(r_c269);
    let n3140: ZW = zw_mix1(n3137, n3139, 269u64);
    let n3141: ZW = zw_mix2(n3138, n3139, 269u64);
    let n3142: ZW = zw_bits_n(r_c270);
    let n3143: ZW = zw_mix1(n3140, n3142, 270u64);
    let n3144: ZW = zw_mix2(n3141, n3142, 270u64);
    let n3145: ZW = zw_bits_n(r_c271);
    let n3146: ZW = zw_mix1(n3143, n3145, 271u64);
    let n3147: ZW = zw_mix2(n3144, n3145, 271u64);
    let n3148: ZW = zw_bits_b(n2394);
    let n3149: ZW = zw_mix1(n3146, n3148, 272u64);
    let n3150: ZW = zw_mix2(n3147, n3148, 272u64);
    let n3151: ZW = zw_bits_n(n2421);
    let n3152: ZW = zw_mix1(n3149, n3151, 280u64);
    let n3153: ZW = zw_mix2(n3150, n3151, 280u64);
    let n3154: ZW = zw_bits_n(n2398);
    let n3155: ZW = zw_mix1(n3152, n3154, 281u64);
    let n3156: ZW = zw_mix2(n3153, n3154, 281u64);
    let n3157: ZW = zw_bits_b(n2439);
    let n3158: ZW = zw_mix1(n3146, n3157, 272u64);
    let n3159: ZW = zw_mix2(n3147, n3157, 272u64);
    let n3160: ZW = zw_bits_n(n2452);
    let n3161: ZW = zw_mix1(n3158, n3160, 280u64);
    let n3162: ZW = zw_mix2(n3159, n3160, 280u64);
    let n3163: ZW = zw_bits_n(n2441);
    let n3164: ZW = zw_mix1(n3161, n3163, 281u64);
    let n3165: ZW = zw_mix2(n3162, n3163, 281u64);
    let n3166: ZW = zw_bits_b(n2470);
    let n3167: ZW = zw_mix1(n3146, n3166, 272u64);
    let n3168: ZW = zw_mix2(n3147, n3166, 272u64);
    let n3169: ZW = zw_bits_n(n2483);
    let n3170: ZW = zw_mix1(n3167, n3169, 280u64);
    let n3171: ZW = zw_mix2(n3168, n3169, 280u64);
    let n3172: ZW = zw_bits_n(n2472);
    let n3173: ZW = zw_mix1(n3170, n3172, 281u64);
    let n3174: ZW = zw_mix2(n3171, n3172, 281u64);
    let n3175: ZW = zw_bits_n(n2497);
    let n3176: ZW = zw_mix1(n3122, n3175, 239u64);
    let n3177: ZW = zw_mix2(n3123, n3175, 239u64);
    let n3178: ZW = zw_mix1(n3176, n3127, 246u64);
    let n3179: ZW = zw_mix2(n3177, n3127, 246u64);
    let n3180: ZW = zw_bits_b(n2498);
    let n3181: ZW = zw_mix1(n3178, n3180, 247u64);
    let n3182: ZW = zw_mix2(n3179, n3180, 247u64);
    let n3183: ZW = zw_mix1(n3181, n3133, 253u64);
    let n3184: ZW = zw_mix2(n3182, n3133, 253u64);
    let n3185: ZW = zw_mix1(n3183, n3136, 268u64);
    let n3186: ZW = zw_mix2(n3184, n3136, 268u64);
    let n3187: ZW = zw_mix1(n3185, n3139, 269u64);
    let n3188: ZW = zw_mix2(n3186, n3139, 269u64);
    let n3189: ZW = zw_mix1(n3187, n3142, 270u64);
    let n3190: ZW = zw_mix2(n3188, n3142, 270u64);
    let n3191: ZW = zw_mix1(n3189, n3145, 271u64);
    let n3192: ZW = zw_mix2(n3190, n3145, 271u64);
    let n3193: ZW = zw_mix1(n3191, n3148, 272u64);
    let n3194: ZW = zw_mix2(n3192, n3148, 272u64);
    let n3195: ZW = zw_bits_n(n2511);
    let n3196: ZW = zw_mix1(n3193, n3195, 280u64);
    let n3197: ZW = zw_mix2(n3194, n3195, 280u64);
    let n3198: ZW = zw_bits_n(n2500);
    let n3199: ZW = zw_mix1(n3196, n3198, 281u64);
    let n3200: ZW = zw_mix2(n3197, n3198, 281u64);
    let n3201: ZW = zw_mix1(n3191, n3157, 272u64);
    let n3202: ZW = zw_mix2(n3192, n3157, 272u64);
    let n3203: ZW = zw_bits_n(n2533);
    let n3204: ZW = zw_mix1(n3201, n3203, 280u64);
    let n3205: ZW = zw_mix2(n3202, n3203, 280u64);
    let n3206: ZW = zw_bits_n(n2522);
    let n3207: ZW = zw_mix1(n3204, n3206, 281u64);
    let n3208: ZW = zw_mix2(n3205, n3206, 281u64);
    let n3209: ZW = zw_mix1(n3191, n3166, 272u64);
    let n3210: ZW = zw_mix2(n3192, n3166, 272u64);
    let n3211: ZW = zw_bits_n(n2555);
    let n3212: ZW = zw_mix1(n3209, n3211, 280u64);
    let n3213: ZW = zw_mix2(n3210, n3211, 280u64);
    let n3214: ZW = zw_bits_n(n2544);
    let n3215: ZW = zw_mix1(n3212, n3214, 281u64);
    let n3216: ZW = zw_mix2(n3213, n3214, 281u64);
    let n3217: ZW = zw_bits_n(n2589);
    let n3218: ZW = zw_mix1(n3108, n3217, 20u64);
    let n3219: ZW = zw_mix2(n3109, n3217, 20u64);
    let n3220: ZW = zw_bits_b(n2590);
    let n3221: ZW = zw_mix1(n3218, n3220, 41u64);
    let n3222: ZW = zw_mix2(n3219, n3220, 41u64);
    let n3223: ZW = zw_bits_n(n2591);
    let n3224: ZW = zw_mix1(n3221, n3223, 234u64);
    let n3225: ZW = zw_mix2(n3222, n3223, 234u64);
    let n3226: ZW = zw_bits_n(n2592);
    let n3227: ZW = zw_mix1(n3224, n3226, 236u64);
    let n3228: ZW = zw_mix2(n3225, n3226, 236u64);
    let n3229: ZW = zw_bits_n(n2593);
    let n3230: ZW = zw_mix1(n3227, n3229, 237u64);
    let n3231: ZW = zw_mix2(n3228, n3229, 237u64);
    let n3232: ZW = zw_mix1(n3230, n3124, 239u64);
    let n3233: ZW = zw_mix2(n3231, n3124, 239u64);
    let n3234: ZW = zw_bits_b(n2594);
    let n3235: ZW = zw_mix1(n3232, n3234, 246u64);
    let n3236: ZW = zw_mix2(n3233, n3234, 246u64);
    let n3237: ZW = zw_mix1(n3235, n3130, 247u64);
    let n3238: ZW = zw_mix2(n3236, n3130, 247u64);
    let n3239: ZW = zw_bits_n(n2613);
    let n3240: ZW = zw_mix1(n3237, n3239, 253u64);
    let n3241: ZW = zw_mix2(n3238, n3239, 253u64);
    let n3242: ZW = zw_bits_n(n2595);
    let n3243: ZW = zw_mix1(n3240, n3242, 268u64);
    let n3244: ZW = zw_mix2(n3241, n3242, 268u64);
    let n3245: ZW = zw_bits_n(n2596);
    let n3246: ZW = zw_mix1(n3243, n3245, 269u64);
    let n3247: ZW = zw_mix2(n3244, n3245, 269u64);
    let n3248: ZW = zw_bits_n(n2597);
    let n3249: ZW = zw_mix1(n3246, n3248, 270u64);
    let n3250: ZW = zw_mix2(n3247, n3248, 270u64);
    let n3251: ZW = zw_bits_n(n2598);
    let n3252: ZW = zw_mix1(n3249, n3251, 271u64);
    let n3253: ZW = zw_mix2(n3250, n3251, 271u64);
    let n3254: ZW = zw_mix1(n3252, n3148, 272u64);
    let n3255: ZW = zw_mix2(n3253, n3148, 272u64);
    let n3256: ZW = zw_bits_n(n2614);
    let n3257: ZW = zw_mix1(n3254, n3256, 280u64);
    let n3258: ZW = zw_mix2(n3255, n3256, 280u64);
    let n3259: ZW = zw_bits_n(n2600);
    let n3260: ZW = zw_mix1(n3257, n3259, 281u64);
    let n3261: ZW = zw_mix2(n3258, n3259, 281u64);
    let n3262: ZW = zw_bits_n(n2630);
    let n3263: ZW = zw_mix1(n3243, n3262, 269u64);
    let n3264: ZW = zw_mix2(n3244, n3262, 269u64);
    let n3265: ZW = zw_bits_n(n2631);
    let n3266: ZW = zw_mix1(n3263, n3265, 270u64);
    let n3267: ZW = zw_mix2(n3264, n3265, 270u64);
    let n3268: ZW = zw_mix1(n3266, n3251, 271u64);
    let n3269: ZW = zw_mix2(n3267, n3251, 271u64);
    let n3270: ZW = zw_mix1(n3268, n3157, 272u64);
    let n3271: ZW = zw_mix2(n3269, n3157, 272u64);
    let n3272: ZW = zw_bits_n(n2644);
    let n3273: ZW = zw_mix1(n3270, n3272, 280u64);
    let n3274: ZW = zw_mix2(n3271, n3272, 280u64);
    let n3275: ZW = zw_bits_n(n2633);
    let n3276: ZW = zw_mix1(n3273, n3275, 281u64);
    let n3277: ZW = zw_mix2(n3274, n3275, 281u64);
    let n3278: ZW = zw_bits_n(n2657);
    let n3279: ZW = zw_mix1(n3263, n3278, 270u64);
    let n3280: ZW = zw_mix2(n3264, n3278, 270u64);
    let n3281: ZW = zw_mix1(n3279, n3251, 271u64);
    let n3282: ZW = zw_mix2(n3280, n3251, 271u64);
    let n3283: ZW = zw_mix1(n3281, n3166, 272u64);
    let n3284: ZW = zw_mix2(n3282, n3166, 272u64);
    let n3285: ZW = zw_bits_n(n2670);
    let n3286: ZW = zw_mix1(n3283, n3285, 280u64);
    let n3287: ZW = zw_mix2(n3284, n3285, 280u64);
    let n3288: ZW = zw_bits_n(n2659);
    let n3289: ZW = zw_mix1(n3286, n3288, 281u64);
    let n3290: ZW = zw_mix2(n3287, n3288, 281u64);
    let n3291: ZW = zw_bits_n(n2693);
    let n3292: ZW = zw_mix1(n3240, n3291, 268u64);
    let n3293: ZW = zw_mix2(n3241, n3291, 268u64);
    let n3294: ZW = zw_bits_n(n2694);
    let n3295: ZW = zw_mix1(n3292, n3294, 269u64);
    let n3296: ZW = zw_mix2(n3293, n3294, 269u64);
    let n3297: ZW = zw_bits_n(n2695);
    let n3298: ZW = zw_mix1(n3295, n3297, 270u64);
    let n3299: ZW = zw_mix2(n3296, n3297, 270u64);
    let n3300: ZW = zw_bits_n(n2696);
    let n3301: ZW = zw_mix1(n3298, n3300, 271u64);
    let n3302: ZW = zw_mix2(n3299, n3300, 271u64);
    let n3303: ZW = zw_mix1(n3301, n3148, 272u64);
    let n3304: ZW = zw_mix2(n3302, n3148, 272u64);
    let n3305: ZW = zw_bits_n(n2709);
    let n3306: ZW = zw_mix1(n3303, n3305, 280u64);
    let n3307: ZW = zw_mix2(n3304, n3305, 280u64);
    let n3308: ZW = zw_bits_n(n2698);
    let n3309: ZW = zw_mix1(n3306, n3308, 281u64);
    let n3310: ZW = zw_mix2(n3307, n3308, 281u64);
    let n3311: ZW = zw_mix1(n3292, n3262, 269u64);
    let n3312: ZW = zw_mix2(n3293, n3262, 269u64);
    let n3313: ZW = zw_mix1(n3311, n3265, 270u64);
    let n3314: ZW = zw_mix2(n3312, n3265, 270u64);
    let n3315: ZW = zw_mix1(n3313, n3300, 271u64);
    let n3316: ZW = zw_mix2(n3314, n3300, 271u64);
    let n3317: ZW = zw_mix1(n3315, n3157, 272u64);
    let n3318: ZW = zw_mix2(n3316, n3157, 272u64);
    let n3319: ZW = zw_bits_n(n2720);
    let n3320: ZW = zw_mix1(n3317, n3319, 280u64);
    let n3321: ZW = zw_mix2(n3318, n3319, 280u64);
    let n3322: ZW = zw_bits_n(n2718);
    let n3323: ZW = zw_mix1(n3320, n3322, 281u64);
    let n3324: ZW = zw_mix2(n3321, n3322, 281u64);
    let n3325: ZW = zw_mix1(n3311, n3278, 270u64);
    let n3326: ZW = zw_mix2(n3312, n3278, 270u64);
    let n3327: ZW = zw_mix1(n3325, n3300, 271u64);
    let n3328: ZW = zw_mix2(n3326, n3300, 271u64);
    let n3329: ZW = zw_mix1(n3327, n3166, 272u64);
    let n3330: ZW = zw_mix2(n3328, n3166, 272u64);
    let n3331: ZW = zw_bits_n(n2730);
    let n3332: ZW = zw_mix1(n3329, n3331, 280u64);
    let n3333: ZW = zw_mix2(n3330, n3331, 280u64);
    let n3334: ZW = zw_bits_n(n2728);
    let n3335: ZW = zw_mix1(n3332, n3334, 281u64);
    let n3336: ZW = zw_mix2(n3333, n3334, 281u64);
    let n3337: ZW = zw_bits_n(n2737);
    let n3338: ZW = zw_mix1(n3298, n3337, 271u64);
    let n3339: ZW = zw_mix2(n3299, n3337, 271u64);
    let n3340: ZW = zw_mix1(n3338, n3148, 272u64);
    let n3341: ZW = zw_mix2(n3339, n3148, 272u64);
    let n3342: ZW = zw_mix1(n3340, n3305, 280u64);
    let n3343: ZW = zw_mix2(n3341, n3305, 280u64);
    let n3344: ZW = zw_bits_n(n2738);
    let n3345: ZW = zw_mix1(n3342, n3344, 281u64);
    let n3346: ZW = zw_mix2(n3343, n3344, 281u64);
    let n3347: ZW = zw_mix1(n3313, n3337, 271u64);
    let n3348: ZW = zw_mix2(n3314, n3337, 271u64);
    let n3349: ZW = zw_mix1(n3347, n3157, 272u64);
    let n3350: ZW = zw_mix2(n3348, n3157, 272u64);
    let n3351: ZW = zw_mix1(n3349, n3319, 280u64);
    let n3352: ZW = zw_mix2(n3350, n3319, 280u64);
    let n3353: ZW = zw_bits_n(n2742);
    let n3354: ZW = zw_mix1(n3351, n3353, 281u64);
    let n3355: ZW = zw_mix2(n3352, n3353, 281u64);
    let n3356: ZW = zw_mix1(n3325, n3337, 271u64);
    let n3357: ZW = zw_mix2(n3326, n3337, 271u64);
    let n3358: ZW = zw_mix1(n3356, n3166, 272u64);
    let n3359: ZW = zw_mix2(n3357, n3166, 272u64);
    let n3360: ZW = zw_mix1(n3358, n3331, 280u64);
    let n3361: ZW = zw_mix2(n3359, n3331, 280u64);
    let n3362: ZW = zw_bits_n(n2746);
    let n3363: ZW = zw_mix1(n3360, n3362, 281u64);
    let n3364: ZW = zw_mix2(n3361, n3362, 281u64);
    let n3365: ZW = zw_mix1(n3230, n3175, 239u64);
    let n3366: ZW = zw_mix2(n3231, n3175, 239u64);
    let n3367: ZW = zw_mix1(n3365, n3234, 246u64);
    let n3368: ZW = zw_mix2(n3366, n3234, 246u64);
    let n3369: ZW = zw_mix1(n3367, n3180, 247u64);
    let n3370: ZW = zw_mix2(n3368, n3180, 247u64);
    let n3371: ZW = zw_mix1(n3369, n3239, 253u64);
    let n3372: ZW = zw_mix2(n3370, n3239, 253u64);
    let n3373: ZW = zw_mix1(n3371, n3242, 268u64);
    let n3374: ZW = zw_mix2(n3372, n3242, 268u64);
    let n3375: ZW = zw_mix1(n3373, n3245, 269u64);
    let n3376: ZW = zw_mix2(n3374, n3245, 269u64);
    let n3377: ZW = zw_mix1(n3375, n3248, 270u64);
    let n3378: ZW = zw_mix2(n3376, n3248, 270u64);
    let n3379: ZW = zw_mix1(n3377, n3251, 271u64);
    let n3380: ZW = zw_mix2(n3378, n3251, 271u64);
    let n3381: ZW = zw_mix1(n3379, n3148, 272u64);
    let n3382: ZW = zw_mix2(n3380, n3148, 272u64);
    let n3383: ZW = zw_bits_n(n2767);
    let n3384: ZW = zw_mix1(n3381, n3383, 280u64);
    let n3385: ZW = zw_mix2(n3382, n3383, 280u64);
    let n3386: ZW = zw_bits_n(n2756);
    let n3387: ZW = zw_mix1(n3384, n3386, 281u64);
    let n3388: ZW = zw_mix2(n3385, n3386, 281u64);
    let n3389: ZW = zw_mix1(n3373, n3262, 269u64);
    let n3390: ZW = zw_mix2(n3374, n3262, 269u64);
    let n3391: ZW = zw_mix1(n3389, n3265, 270u64);
    let n3392: ZW = zw_mix2(n3390, n3265, 270u64);
    let n3393: ZW = zw_mix1(n3391, n3251, 271u64);
    let n3394: ZW = zw_mix2(n3392, n3251, 271u64);
    let n3395: ZW = zw_mix1(n3393, n3157, 272u64);
    let n3396: ZW = zw_mix2(n3394, n3157, 272u64);
    let n3397: ZW = zw_bits_n(n2789);
    let n3398: ZW = zw_mix1(n3395, n3397, 280u64);
    let n3399: ZW = zw_mix2(n3396, n3397, 280u64);
    let n3400: ZW = zw_bits_n(n2778);
    let n3401: ZW = zw_mix1(n3398, n3400, 281u64);
    let n3402: ZW = zw_mix2(n3399, n3400, 281u64);
    let n3403: ZW = zw_mix1(n3389, n3278, 270u64);
    let n3404: ZW = zw_mix2(n3390, n3278, 270u64);
    let n3405: ZW = zw_mix1(n3403, n3251, 271u64);
    let n3406: ZW = zw_mix2(n3404, n3251, 271u64);
    let n3407: ZW = zw_mix1(n3405, n3166, 272u64);
    let n3408: ZW = zw_mix2(n3406, n3166, 272u64);
    let n3409: ZW = zw_bits_n(n2811);
    let n3410: ZW = zw_mix1(n3407, n3409, 280u64);
    let n3411: ZW = zw_mix2(n3408, n3409, 280u64);
    let n3412: ZW = zw_bits_n(n2800);
    let n3413: ZW = zw_mix1(n3410, n3412, 281u64);
    let n3414: ZW = zw_mix2(n3411, n3412, 281u64);
    let n3415: ZW = zw_mix1(n3371, n3291, 268u64);
    let n3416: ZW = zw_mix2(n3372, n3291, 268u64);
    let n3417: ZW = zw_mix1(n3415, n3294, 269u64);
    let n3418: ZW = zw_mix2(n3416, n3294, 269u64);
    let n3419: ZW = zw_mix1(n3417, n3297, 270u64);
    let n3420: ZW = zw_mix2(n3418, n3297, 270u64);
    let n3421: ZW = zw_mix1(n3419, n3300, 271u64);
    let n3422: ZW = zw_mix2(n3420, n3300, 271u64);
    let n3423: ZW = zw_mix1(n3421, n3148, 272u64);
    let n3424: ZW = zw_mix2(n3422, n3148, 272u64);
    let n3425: ZW = zw_bits_n(n2833);
    let n3426: ZW = zw_mix1(n3423, n3425, 280u64);
    let n3427: ZW = zw_mix2(n3424, n3425, 280u64);
    let n3428: ZW = zw_bits_n(n2822);
    let n3429: ZW = zw_mix1(n3426, n3428, 281u64);
    let n3430: ZW = zw_mix2(n3427, n3428, 281u64);
    let n3431: ZW = zw_mix1(n3415, n3262, 269u64);
    let n3432: ZW = zw_mix2(n3416, n3262, 269u64);
    let n3433: ZW = zw_mix1(n3431, n3265, 270u64);
    let n3434: ZW = zw_mix2(n3432, n3265, 270u64);
    let n3435: ZW = zw_mix1(n3433, n3300, 271u64);
    let n3436: ZW = zw_mix2(n3434, n3300, 271u64);
    let n3437: ZW = zw_mix1(n3435, n3157, 272u64);
    let n3438: ZW = zw_mix2(n3436, n3157, 272u64);
    let n3439: ZW = zw_bits_n(n2844);
    let n3440: ZW = zw_mix1(n3437, n3439, 280u64);
    let n3441: ZW = zw_mix2(n3438, n3439, 280u64);
    let n3442: ZW = zw_bits_n(n2842);
    let n3443: ZW = zw_mix1(n3440, n3442, 281u64);
    let n3444: ZW = zw_mix2(n3441, n3442, 281u64);
    let n3445: ZW = zw_mix1(n3431, n3278, 270u64);
    let n3446: ZW = zw_mix2(n3432, n3278, 270u64);
    let n3447: ZW = zw_mix1(n3445, n3300, 271u64);
    let n3448: ZW = zw_mix2(n3446, n3300, 271u64);
    let n3449: ZW = zw_mix1(n3447, n3166, 272u64);
    let n3450: ZW = zw_mix2(n3448, n3166, 272u64);
    let n3451: ZW = zw_bits_n(n2854);
    let n3452: ZW = zw_mix1(n3449, n3451, 280u64);
    let n3453: ZW = zw_mix2(n3450, n3451, 280u64);
    let n3454: ZW = zw_bits_n(n2852);
    let n3455: ZW = zw_mix1(n3452, n3454, 281u64);
    let n3456: ZW = zw_mix2(n3453, n3454, 281u64);
    let n3457: ZW = zw_mix1(n3419, n3337, 271u64);
    let n3458: ZW = zw_mix2(n3420, n3337, 271u64);
    let n3459: ZW = zw_mix1(n3457, n3148, 272u64);
    let n3460: ZW = zw_mix2(n3458, n3148, 272u64);
    let n3461: ZW = zw_mix1(n3459, n3425, 280u64);
    let n3462: ZW = zw_mix2(n3460, n3425, 280u64);
    let n3463: ZW = zw_bits_n(n2858);
    let n3464: ZW = zw_mix1(n3461, n3463, 281u64);
    let n3465: ZW = zw_mix2(n3462, n3463, 281u64);
    let n3466: ZW = zw_mix1(n3433, n3337, 271u64);
    let n3467: ZW = zw_mix2(n3434, n3337, 271u64);
    let n3468: ZW = zw_mix1(n3466, n3157, 272u64);
    let n3469: ZW = zw_mix2(n3467, n3157, 272u64);
    let n3470: ZW = zw_mix1(n3468, n3439, 280u64);
    let n3471: ZW = zw_mix2(n3469, n3439, 280u64);
    let n3472: ZW = zw_bits_n(n2862);
    let n3473: ZW = zw_mix1(n3470, n3472, 281u64);
    let n3474: ZW = zw_mix2(n3471, n3472, 281u64);
    let n3475: ZW = zw_mix1(n3445, n3337, 271u64);
    let n3476: ZW = zw_mix2(n3446, n3337, 271u64);
    let n3477: ZW = zw_mix1(n3475, n3166, 272u64);
    let n3478: ZW = zw_mix2(n3476, n3166, 272u64);
    let n3479: ZW = zw_mix1(n3477, n3451, 280u64);
    let n3480: ZW = zw_mix2(n3478, n3451, 280u64);
    let n3481: ZW = zw_bits_n(n2866);
    let n3482: ZW = zw_mix1(n3479, n3481, 281u64);
    let n3483: ZW = zw_mix2(n3480, n3481, 281u64);
    let ok_v0_b0: u16 = ALL;
    let bd_v0_b0: bool = false;
    let live_v0_b0: u16 = ALL & zb_holds(n32) & zb_holds(n35) & zb_holds(n33) & zb_holds(r_c38);
    let ok_v0_b1: u16 = ALL & zb_holds(n1347);
    let bd_v0_b1: bool = false;
    let live_v0_b1: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1495);
    let ok_v1_b2: u16 = ALL & zb_holds(n1347);
    let bd_v1_b2: bool = false;
    let live_v1_b2: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1548);
    let ok_v2_b3: u16 = ALL & zb_holds(n1347);
    let bd_v2_b3: bool = false;
    let live_v2_b3: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1599);
    let ok_v16_b4: u16 = ALL & zb_holds(n1347);
    let bd_v16_b4: bool = false;
    let live_v16_b4: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1635);
    let ok_v17_b5: u16 = ALL & zb_holds(n1347);
    let bd_v17_b5: bool = false;
    let live_v17_b5: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1671);
    let ok_v18_b6: u16 = ALL & zb_holds(n1347);
    let bd_v18_b6: bool = false;
    let live_v18_b6: u16 = ALL & zb_holds(n32) & zb_holds(n1492) & zb_holds(n1707);
    let ok_v32_b7: u16 = ALL & zb_holds(n1347);
    let bd_v32_b7: bool = false;
    let live_v32_b7: u16 = ALL & zb_holds(n1740);
    let ok_v33_b8: u16 = ALL & zb_holds(n1347);
    let bd_v33_b8: bool = false;
    let live_v33_b8: u16 = ALL & zb_holds(n1751);
    let ok_v34_b9: u16 = ALL & zb_holds(n1347);
    let bd_v34_b9: bool = false;
    let live_v34_b9: u16 = ALL & zb_holds(n1762);
    let ok_v36_b10: u16 = ALL & zb_holds(n1347);
    let bd_v36_b10: bool = false;
    let live_v36_b10: u16 = ALL & zb_holds(n1771);
    let ok_v48_b11: u16 = ALL & zb_holds(n1347);
    let bd_v48_b11: bool = false;
    let live_v48_b11: u16 = ALL & zb_holds(n1794);
    let ok_v49_b12: u16 = ALL & zb_holds(n1347);
    let bd_v49_b12: bool = false;
    let live_v49_b12: u16 = ALL & zb_holds(n1805);
    let ok_v50_b13: u16 = ALL & zb_holds(n1347);
    let bd_v50_b13: bool = false;
    let live_v50_b13: u16 = ALL & zb_holds(n1816);
    let ok_v52_b14: u16 = ALL & zb_holds(n1347);
    let bd_v52_b14: bool = false;
    let live_v52_b14: u16 = ALL & zb_holds(n1825);
    let ok_v0_b15: u16 = ALL & zb_holds(n1903);
    let bd_v0_b15: bool = false;
    let live_v0_b15: u16 = ALL & zb_holds(n32) & zb_holds(n1902);
    let ok_v1_b16: u16 = ALL & zb_holds(n1947);
    let bd_v1_b16: bool = false;
    let live_v1_b16: u16 = ALL & zb_holds(n32) & zb_holds(n1946);
    let ok_v2_b17: u16 = ALL & zb_holds(n1991);
    let bd_v2_b17: bool = false;
    let live_v2_b17: u16 = ALL & zb_holds(n32) & zb_holds(n1990);
    let ok_v16_b18: u16 = ALL & zb_holds(n2034);
    let bd_v16_b18: bool = false;
    let live_v16_b18: u16 = ALL & zb_holds(n32) & zb_holds(n2033);
    let ok_v17_b19: u16 = ALL & zb_holds(n2077);
    let bd_v17_b19: bool = false;
    let live_v17_b19: u16 = ALL & zb_holds(n32) & zb_holds(n2076);
    let ok_v18_b20: u16 = ALL & zb_holds(n2120);
    let bd_v18_b20: bool = false;
    let live_v18_b20: u16 = ALL & zb_holds(n32) & zb_holds(n2119);
    let ok_v32_b21: u16 = ALL & zb_holds(n2148);
    let bd_v32_b21: bool = false;
    let live_v32_b21: u16 = ALL & zb_holds(n2151);
    let ok_v33_b22: u16 = ALL & zb_holds(n2166);
    let bd_v33_b22: bool = false;
    let live_v33_b22: u16 = ALL & zb_holds(n2169);
    let ok_v34_b23: u16 = ALL & zb_holds(n2184);
    let bd_v34_b23: bool = false;
    let live_v34_b23: u16 = ALL & zb_holds(n2187);
    let ok_v36_b24: u16 = ALL & zb_holds(n2200);
    let bd_v36_b24: bool = false;
    let live_v36_b24: u16 = ALL & zb_holds(n2203);
    let ok_v48_b25: u16 = ALL & zb_holds(n2230);
    let bd_v48_b25: bool = false;
    let live_v48_b25: u16 = ALL & zb_holds(n2233);
    let ok_v49_b26: u16 = ALL & zb_holds(n2248);
    let bd_v49_b26: bool = false;
    let live_v49_b26: u16 = ALL & zb_holds(n2251);
    let ok_v50_b27: u16 = ALL & zb_holds(n2266);
    let bd_v50_b27: bool = false;
    let live_v50_b27: u16 = ALL & zb_holds(n2269);
    let ok_v52_b28: u16 = ALL & zb_holds(n2282);
    let bd_v52_b28: bool = false;
    let live_v52_b28: u16 = ALL & zb_holds(n2285);
    let ok_v0_b29: u16 = ALL & zb_holds(n2400);
    let bd_v0_b29: bool = false;
    let live_v0_b29: u16 = ALL & zb_holds(n2422);
    let ok_v1_b30: u16 = ALL & zb_holds(n2400);
    let bd_v1_b30: bool = false;
    let live_v1_b30: u16 = ALL & zb_holds(n2453);
    let ok_v2_b31: u16 = ALL & zb_holds(n2400);
    let bd_v2_b31: bool = false;
    let live_v2_b31: u16 = ALL & zb_holds(n2484);
    let ok_v16_b32: u16 = ALL & zb_holds(n2400);
    let bd_v16_b32: bool = false;
    let live_v16_b32: u16 = ALL & zb_holds(n2512);
    let ok_v17_b33: u16 = ALL & zb_holds(n2400);
    let bd_v17_b33: bool = false;
    let live_v17_b33: u16 = ALL & zb_holds(n2534);
    let ok_v18_b34: u16 = ALL & zb_holds(n2400);
    let bd_v18_b34: bool = false;
    let live_v18_b34: u16 = ALL & zb_holds(n2556);
    let ok_v32_b35: u16 = ALL & zb_holds(n2400);
    let bd_v32_b35: bool = false;
    let live_v32_b35: u16 = ALL & zb_holds(n2615);
    let ok_v33_b36: u16 = ALL & zb_holds(n2400);
    let bd_v33_b36: bool = false;
    let live_v33_b36: u16 = ALL & zb_holds(n2645);
    let ok_v34_b37: u16 = ALL & zb_holds(n2400);
    let bd_v34_b37: bool = false;
    let live_v34_b37: u16 = ALL & zb_holds(n2671);
    let ok_v36_b38: u16 = ALL & zb_holds(n2400);
    let bd_v36_b38: bool = false;
    let live_v36_b38: u16 = ALL & zb_holds(n2710);
    let ok_v37_b39: u16 = ALL & zb_holds(n2400);
    let bd_v37_b39: bool = false;
    let live_v37_b39: u16 = ALL & zb_holds(n2645);
    let ok_v38_b40: u16 = ALL & zb_holds(n2400);
    let bd_v38_b40: bool = false;
    let live_v38_b40: u16 = ALL & zb_holds(n2671);
    let ok_v40_b41: u16 = ALL & zb_holds(n2400);
    let bd_v40_b41: bool = false;
    let live_v40_b41: u16 = ALL & zb_holds(n2710);
    let ok_v41_b42: u16 = ALL & zb_holds(n2400);
    let bd_v41_b42: bool = false;
    let live_v41_b42: u16 = ALL & zb_holds(n2645);
    let ok_v42_b43: u16 = ALL & zb_holds(n2400);
    let bd_v42_b43: bool = false;
    let live_v42_b43: u16 = ALL & zb_holds(n2671);
    let ok_v48_b44: u16 = ALL & zb_holds(n2400);
    let bd_v48_b44: bool = false;
    let live_v48_b44: u16 = ALL & zb_holds(n2768);
    let ok_v49_b45: u16 = ALL & zb_holds(n2400);
    let bd_v49_b45: bool = false;
    let live_v49_b45: u16 = ALL & zb_holds(n2790);
    let ok_v50_b46: u16 = ALL & zb_holds(n2400);
    let bd_v50_b46: bool = false;
    let live_v50_b46: u16 = ALL & zb_holds(n2812);
    let ok_v52_b47: u16 = ALL & zb_holds(n2400);
    let bd_v52_b47: bool = false;
    let live_v52_b47: u16 = ALL & zb_holds(n2834);
    let ok_v53_b48: u16 = ALL & zb_holds(n2400);
    let bd_v53_b48: bool = false;
    let live_v53_b48: u16 = ALL & zb_holds(n2790);
    let ok_v54_b49: u16 = ALL & zb_holds(n2400);
    let bd_v54_b49: bool = false;
    let live_v54_b49: u16 = ALL & zb_holds(n2812);
    let ok_v56_b50: u16 = ALL & zb_holds(n2400);
    let bd_v56_b50: bool = false;
    let live_v56_b50: u16 = ALL & zb_holds(n2834);
    let ok_v57_b51: u16 = ALL & zb_holds(n2400);
    let bd_v57_b51: bool = false;
    let live_v57_b51: u16 = ALL & zb_holds(n2790);
    let ok_v58_b52: u16 = ALL & zb_holds(n2400);
    let bd_v58_b52: bool = false;
    let live_v58_b52: u16 = ALL & zb_holds(n2812);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: n34,
        c84: n27,
        c20: r_c20,
        c88: r_c88,
        c86: n44,
        c43: r_c43,
        c85: n45,
    };
    let sh1 = KShared1 {
        c87: n1345,
        c84: n27,
        c42: r_c42,
        c88: r_c88,
        c86: n44,
        c43: r_c43,
        c85: n45,
    };
    let sh2 = KShared2 {
        c84: n27,
        c88: r_c88,
        c86: n44,
        c43: r_c43,
        c85: n45,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: n2384,
        c84: n27,
        c42: r_c42,
        c88: r_c88,
        c86: n44,
        c232: r_c232,
        c273: r_c273,
        c274: zn_splat(u.c274),
        c275: zn_splat(u.c275),
        c276: zn_splat(u.c276),
        c277: zn_splat(u.c277),
        c278: n2395,
        c279: n2396,
        c249: r_c249,
        c254: n2393,
        c43: r_c43,
        c85: n45,
        c38: r_c38,
    };
    let mut take_0_0: u16 = 0;
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
    let mut take_2_12: u16 = 0;
    let mut take_2_13: u16 = 0;
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
    let mut take_3_12: u16 = 0;
    let mut take_3_13: u16 = 0;
    let mut take_3_14: u16 = 0;
    let mut take_3_15: u16 = 0;
    let mut take_3_16: u16 = 0;
    let mut take_3_17: u16 = 0;
    let mut take_3_18: u16 = 0;
    let mut take_3_19: u16 = 0;
    let mut take_3_20: u16 = 0;
    let mut take_3_21: u16 = 0;
    let mut take_3_22: u16 = 0;
    let mut take_3_23: u16 = 0;
    // 53 distinct button assignments; per outcome they fall
    // into [1, 2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & !ok_v0_b0;
    take_0_0 |= live_v0_b0 & ok_v0_b0;
    let o0 = KOut0 {
        h1: n2891, h2: n2892,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v0_b1 & !ok_v0_b1;
    take_1_0 |= live_v0_b1 & ok_v0_b1;
    declined |= live_v1_b2 & !ok_v1_b2;
    take_1_0 |= live_v1_b2 & ok_v1_b2;
    declined |= live_v2_b3 & !ok_v2_b3;
    take_1_0 |= live_v2_b3 & ok_v2_b3;
    declined |= live_v16_b4 & !ok_v16_b4;
    take_1_0 |= live_v16_b4 & ok_v16_b4;
    declined |= live_v17_b5 & !ok_v17_b5;
    take_1_0 |= live_v17_b5 & ok_v17_b5;
    declined |= live_v18_b6 & !ok_v18_b6;
    take_1_0 |= live_v18_b6 & ok_v18_b6;
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        h1: n2912, h2: n2913,
    };
    // body 6: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b7 & !ok_v32_b7;
    take_1_1 |= live_v32_b7 & ok_v32_b7;
    declined |= live_v33_b8 & !ok_v33_b8;
    take_1_1 |= live_v33_b8 & ok_v33_b8;
    declined |= live_v34_b9 & !ok_v34_b9;
    take_1_1 |= live_v34_b9 & ok_v34_b9;
    declined |= live_v36_b10 & !ok_v36_b10;
    take_1_1 |= live_v36_b10 & ok_v36_b10;
    declined |= live_v48_b11 & !ok_v48_b11;
    take_1_1 |= live_v48_b11 & ok_v48_b11;
    declined |= live_v49_b12 & !ok_v49_b12;
    take_1_1 |= live_v49_b12 & ok_v49_b12;
    declined |= live_v50_b13 & !ok_v50_b13;
    take_1_1 |= live_v50_b13 & ok_v50_b13;
    declined |= live_v52_b14 & !ok_v52_b14;
    take_1_1 |= live_v52_b14 & ok_v52_b14;
    let o1 = KOut1 {
        c20: n1714,
        c41: n1715,
        h1: n2918, h2: n2919,
    };
    // body 14: buttons 0x34, forks 0x0
    sink.o1(52, take_1_1, &sh1, &o1);
    declined |= live_v0_b15 & !ok_v0_b15;
    take_2_0 |= live_v0_b15 & ok_v0_b15;
    let o2 = KOut2 {
        c87: n1898,
        c39: n1899,
        c20: r_c20,
        c38: n1901,
        h1: n2939, h2: n2940,
    };
    // body 15: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b16 & !ok_v1_b16;
    take_2_1 |= live_v1_b16 & ok_v1_b16;
    let o2 = KOut2 {
        c87: n1942,
        c39: n1943,
        c20: r_c20,
        c38: n1945,
        h1: n2948, h2: n2949,
    };
    // body 16: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b17 & !ok_v2_b17;
    take_2_2 |= live_v2_b17 & ok_v2_b17;
    let o2 = KOut2 {
        c87: n1986,
        c39: n1987,
        c20: r_c20,
        c38: n1989,
        h1: n2957, h2: n2958,
    };
    // body 17: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b18 & !ok_v16_b18;
    take_2_3 |= live_v16_b18 & ok_v16_b18;
    let o2 = KOut2 {
        c87: n2029,
        c39: n2030,
        c20: r_c20,
        c38: n2032,
        h1: n2966, h2: n2967,
    };
    // body 18: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b19 & !ok_v17_b19;
    take_2_4 |= live_v17_b19 & ok_v17_b19;
    let o2 = KOut2 {
        c87: n2072,
        c39: n2073,
        c20: r_c20,
        c38: n2075,
        h1: n2975, h2: n2976,
    };
    // body 19: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b20 & !ok_v18_b20;
    take_2_5 |= live_v18_b20 & ok_v18_b20;
    let o2 = KOut2 {
        c87: n2115,
        c39: n2116,
        c20: r_c20,
        c38: n2118,
        h1: n2984, h2: n2985,
    };
    // body 20: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b21 & !ok_v32_b21;
    take_2_6 |= live_v32_b21 & ok_v32_b21;
    let o2 = KOut2 {
        c87: n2143,
        c39: n2144,
        c20: n1714,
        c38: n2146,
        h1: n2995, h2: n2996,
    };
    // body 21: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b22 & !ok_v33_b22;
    take_2_7 |= live_v33_b22 & ok_v33_b22;
    let o2 = KOut2 {
        c87: n2161,
        c39: n2162,
        c20: n1714,
        c38: n2164,
        h1: n3004, h2: n3005,
    };
    // body 22: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b23 & !ok_v34_b23;
    take_2_8 |= live_v34_b23 & ok_v34_b23;
    let o2 = KOut2 {
        c87: n2179,
        c39: n2180,
        c20: n1714,
        c38: n2182,
        h1: n3013, h2: n3014,
    };
    // body 23: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b24 & !ok_v36_b24;
    take_2_9 |= live_v36_b24 & ok_v36_b24;
    let o2 = KOut2 {
        c87: n2195,
        c39: n2196,
        c20: n1714,
        c38: n2198,
        h1: n3022, h2: n3023,
    };
    // body 24: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v48_b25 & !ok_v48_b25;
    take_2_10 |= live_v48_b25 & ok_v48_b25;
    let o2 = KOut2 {
        c87: n2225,
        c39: n2226,
        c20: n1714,
        c38: n2228,
        h1: n3031, h2: n3032,
    };
    // body 25: buttons 0x30, forks 0x0
    sink.o2(48, take_2_10, &sh2, &o2);
    declined |= live_v49_b26 & !ok_v49_b26;
    take_2_11 |= live_v49_b26 & ok_v49_b26;
    let o2 = KOut2 {
        c87: n2243,
        c39: n2244,
        c20: n1714,
        c38: n2246,
        h1: n3040, h2: n3041,
    };
    // body 26: buttons 0x31, forks 0x0
    sink.o2(49, take_2_11, &sh2, &o2);
    declined |= live_v50_b27 & !ok_v50_b27;
    take_2_12 |= live_v50_b27 & ok_v50_b27;
    let o2 = KOut2 {
        c87: n2261,
        c39: n2262,
        c20: n1714,
        c38: n2264,
        h1: n3049, h2: n3050,
    };
    // body 27: buttons 0x32, forks 0x0
    sink.o2(50, take_2_12, &sh2, &o2);
    declined |= live_v52_b28 & !ok_v52_b28;
    take_2_13 |= live_v52_b28 & ok_v52_b28;
    let o2 = KOut2 {
        c87: n2277,
        c39: n2278,
        c20: n1714,
        c38: n2280,
        h1: n3058, h2: n3059,
    };
    // body 28: buttons 0x34, forks 0x0
    sink.o2(52, take_2_13, &sh2, &o2);
    declined |= live_v0_b29 & !ok_v0_b29;
    take_3_0 |= live_v0_b29 & ok_v0_b29;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2394,
        c239: n2389,
        c246: n2390,
        c247: n2391,
        c280: n2421,
        c281: n2398,
        c253: n2420,
        h1: n3155, h2: n3156,
    };
    // body 29: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b30 & !ok_v1_b30;
    take_3_1 |= live_v1_b30 & ok_v1_b30;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2439,
        c239: n2389,
        c246: n2390,
        c247: n2391,
        c280: n2452,
        c281: n2441,
        c253: n2420,
        h1: n3164, h2: n3165,
    };
    // body 30: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b31 & !ok_v2_b31;
    take_3_2 |= live_v2_b31 & ok_v2_b31;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2470,
        c239: n2389,
        c246: n2390,
        c247: n2391,
        c280: n2483,
        c281: n2472,
        c253: n2420,
        h1: n3173, h2: n3174,
    };
    // body 31: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b32 & !ok_v16_b32;
    take_3_3 |= live_v16_b32 & ok_v16_b32;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2394,
        c239: n2497,
        c246: n2390,
        c247: n2498,
        c280: n2511,
        c281: n2500,
        c253: n2420,
        h1: n3199, h2: n3200,
    };
    // body 32: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b33 & !ok_v17_b33;
    take_3_4 |= live_v17_b33 & ok_v17_b33;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2439,
        c239: n2497,
        c246: n2390,
        c247: n2498,
        c280: n2533,
        c281: n2522,
        c253: n2420,
        h1: n3207, h2: n3208,
    };
    // body 33: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b34 & !ok_v18_b34;
    take_3_5 |= live_v18_b34 & ok_v18_b34;
    let o3 = KOut3 {
        c20: n2385,
        c41: r_c41,
        c268: r_c268,
        c269: r_c269,
        c234: n2386,
        c270: r_c270,
        c271: r_c271,
        c236: n2387,
        c237: n2388,
        c272: n2470,
        c239: n2497,
        c246: n2390,
        c247: n2498,
        c280: n2555,
        c281: n2544,
        c253: n2420,
        h1: n3215, h2: n3216,
    };
    // body 34: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b35 & !ok_v32_b35;
    take_3_6 |= live_v32_b35 & ok_v32_b35;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2596,
        c234: n2591,
        c270: n2597,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2614,
        c281: n2600,
        c253: n2613,
        h1: n3260, h2: n3261,
    };
    // body 35: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b36 & !ok_v33_b36;
    take_3_7 |= live_v33_b36 & ok_v33_b36;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2644,
        c281: n2633,
        c253: n2613,
        h1: n3276, h2: n3277,
    };
    // body 36: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b37 & !ok_v34_b37;
    take_3_8 |= live_v34_b37 & ok_v34_b37;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2670,
        c281: n2659,
        c253: n2613,
        h1: n3289, h2: n3290,
    };
    // body 37: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b38 & !ok_v36_b38;
    take_3_9 |= live_v36_b38 & ok_v36_b38;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2694,
        c234: n2591,
        c270: n2695,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2709,
        c281: n2698,
        c253: n2613,
        h1: n3309, h2: n3310,
    };
    // body 38: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v37_b39 & !ok_v37_b39;
    take_3_10 |= live_v37_b39 & ok_v37_b39;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2720,
        c281: n2718,
        c253: n2613,
        h1: n3323, h2: n3324,
    };
    // body 39: buttons 0x25, forks 0x0
    sink.o3(37, take_3_10, &sh3, &o3);
    declined |= live_v38_b40 & !ok_v38_b40;
    take_3_11 |= live_v38_b40 & ok_v38_b40;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2730,
        c281: n2728,
        c253: n2613,
        h1: n3335, h2: n3336,
    };
    // body 40: buttons 0x26, forks 0x0
    sink.o3(38, take_3_11, &sh3, &o3);
    declined |= live_v40_b41 & !ok_v40_b41;
    take_3_12 |= live_v40_b41 & ok_v40_b41;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2694,
        c234: n2591,
        c270: n2695,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2709,
        c281: n2738,
        c253: n2613,
        h1: n3345, h2: n3346,
    };
    // body 41: buttons 0x28, forks 0x0
    sink.o3(40, take_3_12, &sh3, &o3);
    declined |= live_v41_b42 & !ok_v41_b42;
    take_3_13 |= live_v41_b42 & ok_v41_b42;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2720,
        c281: n2742,
        c253: n2613,
        h1: n3354, h2: n3355,
    };
    // body 42: buttons 0x29, forks 0x0
    sink.o3(41, take_3_13, &sh3, &o3);
    declined |= live_v42_b43 & !ok_v42_b43;
    take_3_14 |= live_v42_b43 & ok_v42_b43;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2389,
        c246: n2594,
        c247: n2391,
        c280: n2730,
        c281: n2746,
        c253: n2613,
        h1: n3363, h2: n3364,
    };
    // body 43: buttons 0x2a, forks 0x0
    sink.o3(42, take_3_14, &sh3, &o3);
    declined |= live_v48_b44 & !ok_v48_b44;
    take_3_15 |= live_v48_b44 & ok_v48_b44;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2596,
        c234: n2591,
        c270: n2597,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2767,
        c281: n2756,
        c253: n2613,
        h1: n3387, h2: n3388,
    };
    // body 44: buttons 0x30, forks 0x0
    sink.o3(48, take_3_15, &sh3, &o3);
    declined |= live_v49_b45 & !ok_v49_b45;
    take_3_16 |= live_v49_b45 & ok_v49_b45;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2789,
        c281: n2778,
        c253: n2613,
        h1: n3401, h2: n3402,
    };
    // body 45: buttons 0x31, forks 0x0
    sink.o3(49, take_3_16, &sh3, &o3);
    declined |= live_v50_b46 & !ok_v50_b46;
    take_3_17 |= live_v50_b46 & ok_v50_b46;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2595,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2598,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2811,
        c281: n2800,
        c253: n2613,
        h1: n3413, h2: n3414,
    };
    // body 46: buttons 0x32, forks 0x0
    sink.o3(50, take_3_17, &sh3, &o3);
    declined |= live_v52_b47 & !ok_v52_b47;
    take_3_18 |= live_v52_b47 & ok_v52_b47;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2694,
        c234: n2591,
        c270: n2695,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2833,
        c281: n2822,
        c253: n2613,
        h1: n3429, h2: n3430,
    };
    // body 47: buttons 0x34, forks 0x0
    sink.o3(52, take_3_18, &sh3, &o3);
    declined |= live_v53_b48 & !ok_v53_b48;
    take_3_19 |= live_v53_b48 & ok_v53_b48;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2844,
        c281: n2842,
        c253: n2613,
        h1: n3443, h2: n3444,
    };
    // body 48: buttons 0x35, forks 0x0
    sink.o3(53, take_3_19, &sh3, &o3);
    declined |= live_v54_b49 & !ok_v54_b49;
    take_3_20 |= live_v54_b49 & ok_v54_b49;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2696,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2854,
        c281: n2852,
        c253: n2613,
        h1: n3455, h2: n3456,
    };
    // body 49: buttons 0x36, forks 0x0
    sink.o3(54, take_3_20, &sh3, &o3);
    declined |= live_v56_b50 & !ok_v56_b50;
    take_3_21 |= live_v56_b50 & ok_v56_b50;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2694,
        c234: n2591,
        c270: n2695,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2394,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2833,
        c281: n2858,
        c253: n2613,
        h1: n3464, h2: n3465,
    };
    // body 50: buttons 0x38, forks 0x0
    sink.o3(56, take_3_21, &sh3, &o3);
    declined |= live_v57_b51 & !ok_v57_b51;
    take_3_22 |= live_v57_b51 & ok_v57_b51;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2631,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2439,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2844,
        c281: n2862,
        c253: n2613,
        h1: n3473, h2: n3474,
    };
    // body 51: buttons 0x39, forks 0x0
    sink.o3(57, take_3_22, &sh3, &o3);
    declined |= live_v58_b52 & !ok_v58_b52;
    take_3_23 |= live_v58_b52 & ok_v58_b52;
    let o3 = KOut3 {
        c20: n2589,
        c41: n2590,
        c268: n2693,
        c269: n2630,
        c234: n2591,
        c270: n2657,
        c271: n2737,
        c236: n2592,
        c237: n2593,
        c272: n2470,
        c239: n2497,
        c246: n2594,
        c247: n2498,
        c280: n2854,
        c281: n2866,
        c253: n2613,
        h1: n3482, h2: n3483,
    };
    // body 52: buttons 0x3a, forks 0x0
    sink.o3(58, take_3_23, &sh3, &o3);
    declined
}
