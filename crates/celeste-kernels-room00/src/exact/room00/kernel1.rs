// GENERATED from a TRACED frame (shape 1). Do not edit.
//
// One input shape, 3 output shapes, 28 distinct button
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
pub const SHAPE: u64 = 2812562902515817247;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c276: P8,
    pub c277: P8,
    pub c278: P8,
    pub c279: P8,
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
    pub c171: u16,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c234: u16,
    pub c270: ZN,
    pub c271: ZN,
    pub c236: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c238: ZN,
    pub c239: ZN,
    pub c274: u16,
    pub c275: u16,
    pub c241: ZN,
    pub c248: u16,
    pub c249: u16,
    pub c280: ZN,
    pub c281: ZN,
    pub c251: u16,
    pub c282: ZN,
    pub c283: ZN,
    pub c255: ZN,
    pub c256: ZN,
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
    pub c270: u32,
    pub c271: u32,
    pub c236: u32,
    pub c272: u32,
    pub c273: u32,
    pub c238: u32,
    pub c239: u32,
    pub c274: u32,
    pub c275: u32,
    pub c241: u32,
    pub c248: u32,
    pub c249: u32,
    pub c280: u32,
    pub c281: u32,
    pub c251: u32,
    pub c282: u32,
    pub c283: u32,
    pub c255: u32,
    pub c256: u32,
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
        c276: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c277: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c278: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c279: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c270: cell("objects[0].dash_accel.x")?,
        c271: cell("objects[0].dash_accel.y")?,
        c236: cell("objects[0].dash_effect_time")?,
        c272: cell("objects[0].dash_target.x")?,
        c273: cell("objects[0].dash_target.y")?,
        c238: cell("objects[0].dash_time")?,
        c239: cell("objects[0].djump")?,
        c274: cell("objects[0].flip.x")?,
        c275: cell("objects[0].flip.y")?,
        c241: cell("objects[0].grace")?,
        c248: cell("objects[0].p_dash")?,
        c249: cell("objects[0].p_jump")?,
        c280: cell("objects[0].rem.x")?,
        c281: cell("objects[0].rem.y")?,
        c251: cell("objects[0].solids")?,
        c282: cell("objects[0].spd.x")?,
        c283: cell("objects[0].spd.y")?,
        c255: cell("objects[0].x")?,
        c256: cell("objects[0].y")?,
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
        c238: match &b.cols[s.c238 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c239: match &b.cols[s.c239 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c274: match &b.cols[s.c274 as usize] {
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
        c275: match &b.cols[s.c275 as usize] {
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
        c241: match &b.cols[s.c241 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
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
        c251: match &b.cols[s.c251 as usize] {
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
        c282: match &b.cols[s.c282 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c283: match &b.cols[s.c283 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c255: match &b.cols[s.c255 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c256: match &b.cols[s.c256 as usize] {
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

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
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

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
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
    pub c280: ZN,
    pub c281: ZN,
    pub c256: ZN,
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
    pub c255: ZN,
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
pub const KPART1_0: u64 = 16914069593006527232;
pub const KPART2_0: u64 = 11398472687916843512;

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
    b.cols[235] = Col::U(AV::Num(P8::from_raw(196608i32)));
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
    b.cols[273] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(8126464i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
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
pub const KPART1_1: u64 = 16386877980257891819;
pub const KPART2_1: u64 = 16175718709425361061;

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
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
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
    b.cols[280] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[173] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
pub const KPART1_2: u64 = 7948034168500796330;
pub const KPART2_2: u64 = 12474164470536374018;

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
        if let Col::N(v) = &mut acc.cols[280] { v.push(sh.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(sh.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(sh.c256.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 3;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        _ => panic!("outcome {} of 3", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        _ => panic!("outcome {} of 3", i),
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
    let r_c236: ZN = rin.c236;
    let r_c238: ZN = rin.c238;
    let r_c239: ZN = rin.c239;
    let r_c241: ZN = rin.c241;
    let r_c248: ZB = ZB { val: rin.c248, known: ALL };
    let r_c249: ZB = ZB { val: rin.c249, known: ALL };
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c270: ZN = rin.c270;
    let r_c271: ZN = rin.c271;
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let r_c274: ZB = ZB { val: rin.c274, known: ALL };
    let r_c275: ZB = ZB { val: rin.c275, known: ALL };
    let r_c280: ZN = rin.c280;
    let r_c281: ZN = rin.c281;
    let r_c282: ZN = rin.c282;
    let r_c283: ZN = rin.c283;
    let n55: ZB = zb_not(r_c249);
    let n56: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c282);
    let n57: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c283);
    let n58: ZB = zb_not(r_c43);
    let n62: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n63: ZN = zn_rem(n62, zn_splat(P8::from_raw(1966080i32)));
    let n64: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n63);
    let n74: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n75: ZB = zb_not(n56);
    let n76: ZN = zn_add(r_c280, r_c282);
    let n77: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n76);
    let n78: ZN = zn_flr(n77);
    let n79: ZB = zn_gt(n78, zn_splat(P8::from_raw(0i32)));
    let n80: ZB = zn_lt(n78, zn_splat(P8::from_raw(0i32)));
    let n81: ZN = zsel_n(n80, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n82: ZN = zsel_n(n79, zn_splat(P8::from_raw(65536i32)), n81);
    let n83: ZN = zn_abs(n78);
    let n84: ZN = zn_add(r_c255, n82);
    let n85: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n83);
    let n86: ZN = zn_add(n82, n84);
    let n87: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n83);
    let n88: ZN = zn_add(n82, n86);
    let n89: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n83);
    let n90: ZN = zn_add(n82, n88);
    let n91: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n83);
    let n92: ZN = zn_add(n82, n90);
    let n93: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n83);
    let n94: ZN = zn_add(n82, n92);
    let n95: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n83);
    let n96: ZN = zn_add(n82, n94);
    let n97: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n83);
    let n98: ZN = zn_add(n82, n96);
    let n99: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n83);
    let n100: ZB = zb_not(r_c248);
    let n101: ZB = zb_not(r_c42);
    let n102: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n103: ZB = zb_not(r_c275);
    let n104: bool = P8::from_raw(327680i32) == u.c276;
    let n105: bool = P8::from_raw(393216i32) == u.c277;
    let n106: bool = P8::from_raw(65536i32) == u.c278;
    let n107: bool = P8::from_raw(196608i32) == u.c279;
    let n108: ZB = zb_not(r_c38);
    let n109: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n110: ZN = zn_rem(n109, zn_splat(P8::from_raw(3932160i32)));
    let n111: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n110);
    let n112: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n113: ZN = zsel_n(n111, n112, r_c86);
    let n114: ZN = zsel_n(n64, n113, r_c86);
    let n115: ZN = zsel_n(n64, n110, r_c85);
    let n116: ZB = zb_not(n57);
    let n117: ZB = zb_or(n75, n116);
    let n118: ZB = zb_not(n117);
    let n119: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c255);
    let n120: ZN = zn_add(n82, n119);
    let n121: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c256);
    let n122: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n121);
    let n123: ZB = zn_tile_flag_at(g.cache, g.cart, n120, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n124: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n84);
    let n125: ZN = zn_add(n82, n124);
    let n126: ZB = zn_tile_flag_at(g.cache, g.cart, n125, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n127: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n86);
    let n128: ZN = zn_add(n82, n127);
    let n129: ZB = zn_tile_flag_at(g.cache, g.cart, n128, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n130: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n88);
    let n131: ZN = zn_add(n82, n130);
    let n132: ZB = zn_tile_flag_at(g.cache, g.cart, n131, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n133: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n90);
    let n134: ZN = zn_add(n82, n133);
    let n135: ZB = zn_tile_flag_at(g.cache, g.cart, n134, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n136: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n92);
    let n137: ZN = zn_add(n82, n136);
    let n138: ZB = zn_tile_flag_at(g.cache, g.cart, n137, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n139: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n94);
    let n140: ZN = zn_add(n82, n139);
    let n141: ZB = zn_tile_flag_at(g.cache, g.cart, n140, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n142: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n96);
    let n143: ZN = zn_add(n82, n142);
    let n144: ZB = zn_tile_flag_at(g.cache, g.cart, n143, n122, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n145: ZN = zsel_n(n144, n96, n98);
    let n146: ZN = zsel_n(n144, zn_splat(P8::from_raw(0i32)), r_c282);
    let n147: ZB = zb_or(n99, n144);
    let n148: ZN = zsel_n(n97, n96, n145);
    let n149: ZN = zsel_n(n97, r_c282, n146);
    let n150: ZB = zb_or(n97, n147);
    let n151: ZN = zsel_n(n141, n94, n148);
    let n152: ZN = zsel_n(n141, zn_splat(P8::from_raw(0i32)), n149);
    let n153: ZB = zb_or(n141, n150);
    let n154: ZN = zsel_n(n95, n94, n151);
    let n155: ZN = zsel_n(n95, r_c282, n152);
    let n156: ZB = zb_or(n95, n153);
    let n157: ZN = zsel_n(n138, n92, n154);
    let n158: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n155);
    let n159: ZB = zb_or(n138, n156);
    let n160: ZN = zsel_n(n93, n92, n157);
    let n161: ZN = zsel_n(n93, r_c282, n158);
    let n162: ZB = zb_or(n93, n159);
    let n163: ZN = zsel_n(n135, n90, n160);
    let n164: ZN = zsel_n(n135, zn_splat(P8::from_raw(0i32)), n161);
    let n165: ZB = zb_or(n135, n162);
    let n166: ZN = zsel_n(n91, n90, n163);
    let n167: ZN = zsel_n(n91, r_c282, n164);
    let n168: ZB = zb_or(n91, n165);
    let n169: ZN = zsel_n(n132, n88, n166);
    let n170: ZN = zsel_n(n132, zn_splat(P8::from_raw(0i32)), n167);
    let n171: ZB = zb_or(n132, n168);
    let n172: ZN = zsel_n(n89, n88, n169);
    let n173: ZN = zsel_n(n89, r_c282, n170);
    let n174: ZB = zb_or(n89, n171);
    let n175: ZN = zsel_n(n129, n86, n172);
    let n176: ZN = zsel_n(n129, zn_splat(P8::from_raw(0i32)), n173);
    let n177: ZB = zb_or(n129, n174);
    let n178: ZN = zsel_n(n87, n86, n175);
    let n179: ZN = zsel_n(n87, r_c282, n176);
    let n180: ZB = zb_or(n87, n177);
    let n181: ZN = zsel_n(n126, n84, n178);
    let n182: ZN = zsel_n(n126, zn_splat(P8::from_raw(0i32)), n179);
    let n183: ZB = zb_or(n126, n180);
    let n184: ZN = zsel_n(n85, n84, n181);
    let n185: ZN = zsel_n(n85, r_c282, n182);
    let n186: ZB = zb_or(n85, n183);
    let n187: ZN = zsel_n(n123, r_c255, n184);
    let n188: ZN = zsel_n(n123, zn_splat(P8::from_raw(0i32)), n185);
    let n189: ZB = zb_or(n123, n186);
    let n190: ZN = zn_add(r_c281, r_c283);
    let n191: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n190);
    let n192: ZN = zn_flr(n191);
    let n193: ZB = zn_gt(n192, zn_splat(P8::from_raw(0i32)));
    let n194: ZB = zn_lt(n192, zn_splat(P8::from_raw(0i32)));
    let n195: ZN = zsel_n(n194, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n196: ZN = zsel_n(n193, zn_splat(P8::from_raw(65536i32)), n195);
    let n197: ZN = zn_abs(n192);
    let n198: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n187);
    let n199: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n198);
    let n200: ZN = zn_add(n121, n196);
    let n201: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n200, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n202: ZN = zn_add(r_c256, n196);
    let n203: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n197);
    let n204: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n202);
    let n205: ZN = zn_add(n196, n204);
    let n206: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n205, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n207: ZN = zn_add(n196, n202);
    let n208: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n197);
    let n209: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n207);
    let n210: ZN = zn_add(n196, n209);
    let n211: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n210, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n212: ZN = zn_add(n196, n207);
    let n213: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n197);
    let n214: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n212);
    let n215: ZN = zn_add(n196, n214);
    let n216: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n215, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n217: ZN = zn_add(n196, n212);
    let n218: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n197);
    let n219: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n217);
    let n220: ZN = zn_add(n196, n219);
    let n221: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n220, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n222: ZN = zn_add(n196, n217);
    let n223: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n197);
    let n224: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n222);
    let n225: ZN = zn_add(n196, n224);
    let n226: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n225, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n227: ZN = zn_add(n196, n222);
    let n228: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n197);
    let n229: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n227);
    let n230: ZN = zn_add(n196, n229);
    let n231: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n230, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n232: ZN = zn_add(n196, n227);
    let n233: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n197);
    let n234: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n232);
    let n235: ZN = zn_add(n196, n234);
    let n236: ZB = zn_tile_flag_at(g.cache, g.cart, n199, n235, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n237: ZN = zn_add(n196, n232);
    let n238: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n197);
    let n239: ZB = zb_and(n189, n238);
    let n240: ZN = zsel_n(n236, n232, n237);
    let n241: ZN = zsel_n(n236, zn_splat(P8::from_raw(0i32)), r_c283);
    let n242: ZB = zsel_b(n236, n189, n239);
    let n243: ZN = zsel_n(n233, n232, n240);
    let n244: ZN = zsel_n(n233, r_c283, n241);
    let n245: ZB = zsel_b(n233, n189, n242);
    let n246: ZN = zsel_n(n231, n227, n243);
    let n247: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n244);
    let n248: ZB = zsel_b(n231, n189, n245);
    let n249: ZN = zsel_n(n228, n227, n246);
    let n250: ZN = zsel_n(n228, r_c283, n247);
    let n251: ZB = zsel_b(n228, n189, n248);
    let n252: ZN = zsel_n(n226, n222, n249);
    let n253: ZN = zsel_n(n226, zn_splat(P8::from_raw(0i32)), n250);
    let n254: ZB = zsel_b(n226, n189, n251);
    let n255: ZN = zsel_n(n223, n222, n252);
    let n256: ZN = zsel_n(n223, r_c283, n253);
    let n257: ZB = zsel_b(n223, n189, n254);
    let n258: ZN = zsel_n(n221, n217, n255);
    let n259: ZN = zsel_n(n221, zn_splat(P8::from_raw(0i32)), n256);
    let n260: ZB = zsel_b(n221, n189, n257);
    let n261: ZN = zsel_n(n218, n217, n258);
    let n262: ZN = zsel_n(n218, r_c283, n259);
    let n263: ZB = zsel_b(n218, n189, n260);
    let n264: ZN = zsel_n(n216, n212, n261);
    let n265: ZN = zsel_n(n216, zn_splat(P8::from_raw(0i32)), n262);
    let n266: ZB = zsel_b(n216, n189, n263);
    let n267: ZN = zsel_n(n213, n212, n264);
    let n268: ZN = zsel_n(n213, r_c283, n265);
    let n269: ZB = zsel_b(n213, n189, n266);
    let n270: ZN = zsel_n(n211, n207, n267);
    let n271: ZN = zsel_n(n211, zn_splat(P8::from_raw(0i32)), n268);
    let n272: ZB = zsel_b(n211, n189, n269);
    let n273: ZN = zsel_n(n208, n207, n270);
    let n274: ZN = zsel_n(n208, r_c283, n271);
    let n275: ZB = zsel_b(n208, n189, n272);
    let n276: ZN = zsel_n(n206, n202, n273);
    let n277: ZN = zsel_n(n206, zn_splat(P8::from_raw(0i32)), n274);
    let n278: ZB = zsel_b(n206, n189, n275);
    let n279: ZN = zsel_n(n203, n202, n276);
    let n280: ZN = zsel_n(n203, r_c283, n277);
    let n281: ZB = zsel_b(n203, n189, n278);
    let n282: ZN = zsel_n(n201, r_c256, n279);
    let n283: ZN = zsel_n(n201, zn_splat(P8::from_raw(0i32)), n280);
    let n284: ZB = zsel_b(n201, n189, n281);
    let n285: ZN = zsel_n(n117, n187, r_c255);
    let n286: ZN = zsel_n(n117, n282, r_c256);
    let n287: ZN = zsel_n(n117, n188, r_c282);
    let n288: ZN = zsel_n(n117, n283, r_c283);
    let n289: ZB = zb_or(n118, n284);
    let n290: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n285);
    let n291: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n286);
    let n292: ZN = zn_div(n290, zn_splat(P8::from_raw(524288i32)));
    let n293: ZN = zn_flr(n292);
    let n294: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n293);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n290);
    let n296: ZN = zn_sub(n295, zn_splat(P8::from_raw(65536i32)));
    let n297: ZN = zn_div(n296, zn_splat(P8::from_raw(524288i32)));
    let n298: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n297);
    let n299: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n294);
    let n300: ZB = zn_le(n299, n298);
    let n301: ZB = zn_gt(n299, n298);
    let n302: ZB = zb_and(n74, n300);
    let n303: ZB = zb_and(n74, n301);
    let n304: ZN = zn_div(n291, zn_splat(P8::from_raw(524288i32)));
    let n305: ZN = zn_flr(n304);
    let n306: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n305);
    let n307: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n291);
    let n308: ZN = zn_sub(n307, zn_splat(P8::from_raw(65536i32)));
    let n309: ZN = zn_div(n308, zn_splat(P8::from_raw(524288i32)));
    let n310: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n309);
    let n311: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n306);
    let n312: ZB = zn_le(n311, n310);
    let n313: ZB = zn_gt(n311, n310);
    let n314: ZB = zb_and(n302, n312);
    let n315: ZB = zb_and(n302, n313);
    let n316: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n299);
    let n317: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n311);
    let n318: ZN = zn_mget(g.cart, n316, n317);
    let n319: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n318);
    let n320: ZN = zn_rem(n308, zn_splat(P8::from_raw(524288i32)));
    let n321: ZB = zn_ge(n320, zn_splat(P8::from_raw(393216i32)));
    let n322: ZN = zn_mul(n311, zn_splat(P8::from_raw(524288i32)));
    let n323: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n322);
    let n324: ZB = zn_eq(n307, n323);
    let n325: ZB = zb_or(n321, n324);
    let n326: ZB = zb_and(n319, n325);
    let n327: ZB = zn_ge(n288, zn_splat(P8::from_raw(0i32)));
    let n328: ZB = zb_and(n326, n327);
    let n329: ZB = zb_not(n328);
    let n330: ZB = zb_and(n314, n328);
    let n331: ZB = zb_and(n314, n329);
    let n332: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n318);
    let n333: ZN = zn_rem(n291, zn_splat(P8::from_raw(524288i32)));
    let n334: ZB = zn_le(n333, zn_splat(P8::from_raw(131072i32)));
    let n335: ZB = zb_and(n332, n334);
    let n336: ZB = zn_le(n288, zn_splat(P8::from_raw(0i32)));
    let n337: ZB = zb_and(n335, n336);
    let n338: ZB = zb_not(n337);
    let n339: ZB = zb_and(n331, n337);
    let n340: ZB = zb_and(n331, n338);
    let n341: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n318);
    let n342: ZN = zn_rem(n290, zn_splat(P8::from_raw(524288i32)));
    let n343: ZB = zn_le(n342, zn_splat(P8::from_raw(131072i32)));
    let n344: ZB = zb_and(n341, n343);
    let n345: ZB = zn_le(n287, zn_splat(P8::from_raw(0i32)));
    let n346: ZB = zb_and(n344, n345);
    let n347: ZB = zb_not(n346);
    let n348: ZB = zb_and(n340, n346);
    let n349: ZB = zb_and(n340, n347);
    let n350: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n318);
    let n351: ZN = zn_rem(n296, zn_splat(P8::from_raw(524288i32)));
    let n352: ZB = zn_ge(n351, zn_splat(P8::from_raw(393216i32)));
    let n353: ZN = zn_mul(n299, zn_splat(P8::from_raw(524288i32)));
    let n354: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n353);
    let n355: ZB = zn_eq(n295, n354);
    let n356: ZB = zb_or(n352, n355);
    let n357: ZB = zb_and(n350, n356);
    let n358: ZB = zn_ge(n287, zn_splat(P8::from_raw(0i32)));
    let n359: ZB = zb_and(n357, n358);
    let n360: ZB = zb_not(n359);
    let n361: ZB = zb_and(n349, n359);
    let n362: ZB = zb_and(n349, n360);
    let n363: ZB = zb_or(n348, n361);
    let n364: ZB = zb_or(n339, n363);
    let n365: ZB = zb_or(n330, n364);
    let n366: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n306);
    let n367: ZB = zn_le(n366, n310);
    let n368: ZB = zn_gt(n366, n310);
    let n369: ZB = zb_and(n362, n367);
    let n370: ZB = zb_and(n362, n368);
    let n371: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n366);
    let n372: ZN = zn_mget(g.cart, n316, n371);
    let n373: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n372);
    let n374: ZN = zn_mul(n366, zn_splat(P8::from_raw(524288i32)));
    let n375: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n374);
    let n376: ZB = zn_eq(n307, n375);
    let n377: ZB = zb_or(n321, n376);
    let n378: ZB = zb_and(n373, n377);
    let n379: ZB = zb_and(n327, n378);
    let n380: ZB = zb_not(n379);
    let n381: ZB = zb_and(n369, n379);
    let n382: ZB = zb_and(n369, n380);
    let n383: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n372);
    let n384: ZB = zb_and(n334, n383);
    let n385: ZB = zb_and(n336, n384);
    let n386: ZB = zb_not(n385);
    let n387: ZB = zb_and(n382, n385);
    let n388: ZB = zb_and(n382, n386);
    let n389: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n372);
    let n390: ZB = zb_and(n343, n389);
    let n391: ZB = zb_and(n345, n390);
    let n392: ZB = zb_not(n391);
    let n393: ZB = zb_and(n388, n391);
    let n394: ZB = zb_and(n388, n392);
    let n395: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n372);
    let n396: ZB = zb_and(n356, n395);
    let n397: ZB = zb_and(n358, n396);
    let n398: ZB = zb_not(n397);
    let n399: ZB = zb_and(n394, n397);
    let n400: ZB = zb_and(n394, n398);
    let n401: ZB = zb_or(n393, n399);
    let n402: ZB = zb_or(n387, n401);
    let n403: ZB = zb_or(n381, n402);
    let n404: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n306);
    let n405: ZB = zn_le(n404, n310);
    let n406: ZB = zn_gt(n404, n310);
    let n407: ZB = zb_and(n400, n405);
    let n408: ZB = zb_and(n400, n406);
    let n409: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n404);
    let n410: ZN = zn_mget(g.cart, n316, n409);
    let n411: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n410);
    let n412: ZN = zn_mul(n404, zn_splat(P8::from_raw(524288i32)));
    let n413: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n412);
    let n414: ZB = zn_eq(n307, n413);
    let n415: ZB = zb_or(n321, n414);
    let n416: ZB = zb_and(n411, n415);
    let n417: ZB = zb_and(n327, n416);
    let n418: ZB = zb_not(n417);
    let n419: ZB = zb_and(n407, n417);
    let n420: ZB = zb_and(n407, n418);
    let n421: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n410);
    let n422: ZB = zb_and(n334, n421);
    let n423: ZB = zb_and(n336, n422);
    let n424: ZB = zb_not(n423);
    let n425: ZB = zb_and(n420, n423);
    let n426: ZB = zb_and(n420, n424);
    let n427: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n410);
    let n428: ZB = zb_and(n343, n427);
    let n429: ZB = zb_and(n345, n428);
    let n430: ZB = zb_not(n429);
    let n431: ZB = zb_and(n426, n429);
    let n432: ZB = zb_and(n426, n430);
    let n433: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n410);
    let n434: ZB = zb_and(n356, n433);
    let n435: ZB = zb_and(n358, n434);
    let n436: ZB = zb_not(n435);
    let n437: ZB = zb_and(n432, n435);
    let n438: ZB = zb_and(n432, n436);
    let n439: ZB = zb_or(n431, n437);
    let n440: ZB = zb_or(n425, n439);
    let n441: ZB = zb_or(n419, n440);
    let n442: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n306);
    let n443: ZB = zn_gt(n442, n310);
    let n444: ZB = zb_and(n289, n443);
    let n445: ZB = zb_or(n408, n438);
    let n446: ZB = zsel_b(n406, n289, n444);
    let n447: ZB = zb_or(n403, n441);
    let n448: ZB = zb_or(n370, n445);
    let n449: ZB = zsel_b(n368, n289, n446);
    let n450: ZB = zb_or(n365, n447);
    let n451: ZB = zb_or(n315, n448);
    let n452: ZB = zsel_b(n313, n289, n449);
    let n453: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n294);
    let n454: ZB = zn_le(n453, n298);
    let n455: ZB = zn_gt(n453, n298);
    let n456: ZB = zb_and(n451, n454);
    let n457: ZB = zb_and(n451, n455);
    let n458: ZB = zb_and(n313, n456);
    let n459: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n453);
    let n460: ZN = zn_mget(g.cart, n459, n317);
    let n461: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n460);
    let n462: ZB = zb_and(n312, n451);
    let n463: ZB = zb_and(n454, n462);
    let n464: ZB = zb_and(n325, n461);
    let n465: ZB = zb_and(n327, n464);
    let n466: ZB = zb_not(n465);
    let n467: ZB = zb_and(n463, n465);
    let n468: ZB = zb_and(n463, n466);
    let n469: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n460);
    let n470: ZB = zb_and(n334, n469);
    let n471: ZB = zb_and(n336, n470);
    let n472: ZB = zb_not(n471);
    let n473: ZB = zb_and(n468, n471);
    let n474: ZB = zb_and(n468, n472);
    let n475: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n460);
    let n476: ZB = zb_and(n343, n475);
    let n477: ZB = zb_and(n345, n476);
    let n478: ZB = zb_not(n477);
    let n479: ZB = zb_and(n474, n477);
    let n480: ZB = zb_and(n474, n478);
    let n481: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n460);
    let n482: ZN = zn_mul(n453, zn_splat(P8::from_raw(524288i32)));
    let n483: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n482);
    let n484: ZB = zn_eq(n295, n483);
    let n485: ZB = zb_or(n352, n484);
    let n486: ZB = zb_and(n481, n485);
    let n487: ZB = zb_and(n358, n486);
    let n488: ZB = zb_not(n487);
    let n489: ZB = zb_and(n480, n487);
    let n490: ZB = zb_and(n480, n488);
    let n491: ZB = zb_or(n479, n489);
    let n492: ZB = zb_or(n473, n491);
    let n493: ZB = zb_or(n467, n492);
    let n494: ZB = zb_and(n368, n490);
    let n495: ZN = zn_mget(g.cart, n459, n371);
    let n496: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n495);
    let n497: ZB = zb_and(n367, n480);
    let n498: ZB = zb_and(n488, n497);
    let n499: ZB = zb_and(n377, n496);
    let n500: ZB = zb_and(n327, n499);
    let n501: ZB = zb_not(n500);
    let n502: ZB = zb_and(n498, n500);
    let n503: ZB = zb_and(n498, n501);
    let n504: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n495);
    let n505: ZB = zb_and(n334, n504);
    let n506: ZB = zb_and(n336, n505);
    let n507: ZB = zb_not(n506);
    let n508: ZB = zb_and(n503, n506);
    let n509: ZB = zb_and(n503, n507);
    let n510: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n495);
    let n511: ZB = zb_and(n343, n510);
    let n512: ZB = zb_and(n345, n511);
    let n513: ZB = zb_not(n512);
    let n514: ZB = zb_and(n509, n512);
    let n515: ZB = zb_and(n509, n513);
    let n516: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n495);
    let n517: ZB = zb_and(n485, n516);
    let n518: ZB = zb_and(n358, n517);
    let n519: ZB = zb_not(n518);
    let n520: ZB = zb_and(n515, n518);
    let n521: ZB = zb_and(n515, n519);
    let n522: ZB = zb_or(n514, n520);
    let n523: ZB = zb_or(n508, n522);
    let n524: ZB = zb_or(n502, n523);
    let n525: ZB = zb_and(n406, n521);
    let n526: ZN = zn_mget(g.cart, n459, n409);
    let n527: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n526);
    let n528: ZB = zb_and(n405, n515);
    let n529: ZB = zb_and(n519, n528);
    let n530: ZB = zb_and(n415, n527);
    let n531: ZB = zb_and(n327, n530);
    let n532: ZB = zb_not(n531);
    let n533: ZB = zb_and(n529, n531);
    let n534: ZB = zb_and(n529, n532);
    let n535: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n526);
    let n536: ZB = zb_and(n334, n535);
    let n537: ZB = zb_and(n336, n536);
    let n538: ZB = zb_not(n537);
    let n539: ZB = zb_and(n534, n537);
    let n540: ZB = zb_and(n534, n538);
    let n541: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n526);
    let n542: ZB = zb_and(n343, n541);
    let n543: ZB = zb_and(n345, n542);
    let n544: ZB = zb_not(n543);
    let n545: ZB = zb_and(n540, n543);
    let n546: ZB = zb_and(n540, n544);
    let n547: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n526);
    let n548: ZB = zb_and(n485, n547);
    let n549: ZB = zb_and(n358, n548);
    let n550: ZB = zb_not(n549);
    let n551: ZB = zb_and(n546, n549);
    let n552: ZB = zb_and(n546, n550);
    let n553: ZB = zb_or(n545, n551);
    let n554: ZB = zb_or(n539, n553);
    let n555: ZB = zb_or(n533, n554);
    let n556: ZB = zb_and(n443, n452);
    let n557: ZB = zb_or(n525, n552);
    let n558: ZB = zsel_b(n406, n452, n556);
    let n559: ZB = zb_or(n524, n555);
    let n560: ZB = zb_or(n494, n557);
    let n561: ZB = zsel_b(n368, n452, n558);
    let n562: ZB = zb_or(n493, n559);
    let n563: ZB = zb_or(n458, n560);
    let n564: ZB = zsel_b(n313, n452, n561);
    let n565: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n294);
    let n566: ZB = zn_le(n565, n298);
    let n567: ZB = zn_gt(n565, n298);
    let n568: ZB = zb_and(n563, n566);
    let n569: ZB = zb_and(n563, n567);
    let n570: ZB = zb_and(n313, n568);
    let n571: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n565);
    let n572: ZN = zn_mget(g.cart, n571, n317);
    let n573: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n572);
    let n574: ZB = zb_and(n312, n563);
    let n575: ZB = zb_and(n566, n574);
    let n576: ZB = zb_and(n325, n573);
    let n577: ZB = zb_and(n327, n576);
    let n578: ZB = zb_not(n577);
    let n579: ZB = zb_and(n575, n577);
    let n580: ZB = zb_and(n575, n578);
    let n581: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n572);
    let n582: ZB = zb_and(n334, n581);
    let n583: ZB = zb_and(n336, n582);
    let n584: ZB = zb_not(n583);
    let n585: ZB = zb_and(n580, n583);
    let n586: ZB = zb_and(n580, n584);
    let n587: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n572);
    let n588: ZB = zb_and(n343, n587);
    let n589: ZB = zb_and(n345, n588);
    let n590: ZB = zb_not(n589);
    let n591: ZB = zb_and(n586, n589);
    let n592: ZB = zb_and(n586, n590);
    let n593: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n572);
    let n594: ZN = zn_mul(n565, zn_splat(P8::from_raw(524288i32)));
    let n595: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n594);
    let n596: ZB = zn_eq(n295, n595);
    let n597: ZB = zb_or(n352, n596);
    let n598: ZB = zb_and(n593, n597);
    let n599: ZB = zb_and(n358, n598);
    let n600: ZB = zb_not(n599);
    let n601: ZB = zb_and(n592, n599);
    let n602: ZB = zb_and(n592, n600);
    let n603: ZB = zb_or(n591, n601);
    let n604: ZB = zb_or(n585, n603);
    let n605: ZB = zb_or(n579, n604);
    let n606: ZB = zb_and(n368, n602);
    let n607: ZN = zn_mget(g.cart, n571, n371);
    let n608: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n607);
    let n609: ZB = zb_and(n367, n592);
    let n610: ZB = zb_and(n600, n609);
    let n611: ZB = zb_and(n377, n608);
    let n612: ZB = zb_and(n327, n611);
    let n613: ZB = zb_not(n612);
    let n614: ZB = zb_and(n610, n612);
    let n615: ZB = zb_and(n610, n613);
    let n616: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n607);
    let n617: ZB = zb_and(n334, n616);
    let n618: ZB = zb_and(n336, n617);
    let n619: ZB = zb_not(n618);
    let n620: ZB = zb_and(n615, n618);
    let n621: ZB = zb_and(n615, n619);
    let n622: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n607);
    let n623: ZB = zb_and(n343, n622);
    let n624: ZB = zb_and(n345, n623);
    let n625: ZB = zb_not(n624);
    let n626: ZB = zb_and(n621, n624);
    let n627: ZB = zb_and(n621, n625);
    let n628: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n607);
    let n629: ZB = zb_and(n597, n628);
    let n630: ZB = zb_and(n358, n629);
    let n631: ZB = zb_not(n630);
    let n632: ZB = zb_and(n627, n630);
    let n633: ZB = zb_and(n627, n631);
    let n634: ZB = zb_or(n626, n632);
    let n635: ZB = zb_or(n620, n634);
    let n636: ZB = zb_or(n614, n635);
    let n637: ZB = zb_and(n406, n633);
    let n638: ZN = zn_mget(g.cart, n571, n409);
    let n639: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n638);
    let n640: ZB = zb_and(n405, n627);
    let n641: ZB = zb_and(n631, n640);
    let n642: ZB = zb_and(n415, n639);
    let n643: ZB = zb_and(n327, n642);
    let n644: ZB = zb_not(n643);
    let n645: ZB = zb_and(n641, n643);
    let n646: ZB = zb_and(n641, n644);
    let n647: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n638);
    let n648: ZB = zb_and(n334, n647);
    let n649: ZB = zb_and(n336, n648);
    let n650: ZB = zb_not(n649);
    let n651: ZB = zb_and(n646, n649);
    let n652: ZB = zb_and(n646, n650);
    let n653: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n638);
    let n654: ZB = zb_and(n343, n653);
    let n655: ZB = zb_and(n345, n654);
    let n656: ZB = zb_not(n655);
    let n657: ZB = zb_and(n652, n655);
    let n658: ZB = zb_and(n652, n656);
    let n659: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n638);
    let n660: ZB = zb_and(n597, n659);
    let n661: ZB = zb_and(n358, n660);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n658, n661);
    let n664: ZB = zb_and(n658, n662);
    let n665: ZB = zb_or(n657, n663);
    let n666: ZB = zb_or(n651, n665);
    let n667: ZB = zb_or(n645, n666);
    let n668: ZB = zb_and(n443, n564);
    let n669: ZB = zb_or(n637, n664);
    let n670: ZB = zsel_b(n406, n564, n668);
    let n671: ZB = zb_or(n636, n667);
    let n672: ZB = zb_or(n606, n669);
    let n673: ZB = zsel_b(n368, n564, n670);
    let n674: ZB = zb_or(n605, n671);
    let n675: ZB = zb_or(n570, n672);
    let n676: ZB = zsel_b(n313, n564, n673);
    let n677: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n294);
    let n678: ZB = zn_gt(n677, n298);
    let n679: ZB = zb_and(n676, n678);
    let n680: ZB = zb_or(n562, n674);
    let n681: ZB = zsel_b(n562, n452, n564);
    let n682: ZB = zb_or(n569, n675);
    let n683: ZB = zsel_b(n567, n564, n679);
    let n684: ZB = zb_or(n450, n680);
    let n685: ZB = zsel_b(n450, n289, n681);
    let n686: ZB = zb_or(n457, n682);
    let n687: ZB = zsel_b(n455, n452, n683);
    let n688: ZB = zb_or(n303, n686);
    let n689: ZB = zsel_b(n301, n289, n687);
    let n690: ZB = zn_gt(n286, zn_splat(P8::from_raw(8388608i32)));
    let n691: ZB = zn_le(n286, zn_splat(P8::from_raw(8388608i32)));
    let n692: ZB = zb_and(n688, n690);
    let n693: ZB = zb_or(n684, n692);
    let n694: ZB = zsel_b(n684, n685, n689);
    let n695: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n290);
    let n696: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n291);
    let n697: ZB = zn_tile_flag_at(g.cache, g.cart, n695, n696, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n698: ZB = zb_not(n697);
    let n699: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n700: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n701: ZN = zsel_n(n699, n700, r_c241);
    let n702: ZN = zsel_n(n697, zn_splat(P8::from_raw(393216i32)), n701);
    let n703: ZB = zn_gt(r_c238, zn_splat(P8::from_raw(0i32)));
    let n704: ZB = zn_gt(n287, r_c272);
    let n705: ZB = zn_gt(n288, r_c273);
    let n706: ZN = zsel_n(n698, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n707: ZN = zn_abs(n287);
    let n708: ZB = zn_gt(n707, zn_splat(P8::from_raw(65536i32)));
    let n709: ZB = zn_gt(n287, zn_splat(P8::from_raw(0i32)));
    let n710: ZB = zn_lt(n287, zn_splat(P8::from_raw(0i32)));
    let n711: ZB = zn_gt(n287, zn_splat(P8::from_raw(65536i32)));
    let n712: ZN = zn_sub(n287, zn_splat(P8::from_raw(9830i32)));
    let n713: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n712);
    let n714: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n287);
    let n715: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n714);
    let n716: ZB = zn_gt(n287, zn_splat(P8::from_raw(-65536i32)));
    let n717: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n712);
    let n718: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n714);
    let n719: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n712);
    let n720: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n714);
    let n721: ZN = zsel_n(n716, n717, n718);
    let n722: ZN = zsel_n(n709, n719, n720);
    let n723: ZN = zsel_n(n711, n713, n715);
    let n724: ZN = zsel_n(n710, n721, n722);
    let n725: ZN = zsel_n(n709, n723, n724);
    let n726: ZN = zn_sub(n287, n706);
    let n727: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n726);
    let n728: ZN = zn_add(n287, n706);
    let n729: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n728);
    let n730: ZN = zsel_n(n709, n727, n729);
    let n731: ZN = zsel_n(n708, n725, n730);
    let n732: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n731);
    let n733: ZB = zb_not(n732);
    let n734: ZB = zn_lt(n731, zn_splat(P8::from_raw(0i32)));
    let n735: ZB = zsel_b(n733, n734, r_c274);
    let n736: ZN = zn_abs(n288);
    let n737: ZB = zn_le(n736, zn_splat(P8::from_raw(9830i32)));
    let n738: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n291);
    let n739: ZB = zn_gt(n288, zn_splat(P8::from_raw(131072i32)));
    let n740: ZB = zn_gt(n702, zn_splat(P8::from_raw(0i32)));
    let n741: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n290);
    let n742: ZB = zn_tile_flag_at(g.cache, g.cart, n741, n738, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n743: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n290);
    let n744: ZB = zn_tile_flag_at(g.cache, g.cart, n743, n738, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n745: ZN = zsel_n(n744, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n746: ZN = zsel_n(n742, zn_splat(P8::from_raw(-65536i32)), n745);
    let n747: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n746);
    let n748: ZB = zb_not(n747);
    let n749: ZN = zsel_n(n735, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n750: ZB = zn_gt(n749, zn_splat(P8::from_raw(0i32)));
    let n751: ZB = zn_lt(n749, zn_splat(P8::from_raw(0i32)));
    let n752: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n749);
    let n753: ZB = zb_not(n752);
    let n754: ZB = zn_lt(n286, zn_splat(P8::from_raw(-262144i32)));
    let n755: ZB = zn_ge(n286, zn_splat(P8::from_raw(-262144i32)));
    let n756: ZB = zn_lt(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n757: ZN = zsel_n(n756, zn_splat(P8::from_raw(65536i32)), r_c239);
    let n758: ZN = zsel_n(n697, n757, r_c239);
    let n759: ZB = zn_gt(n758, zn_splat(P8::from_raw(0i32)));
    let n760: ZB = zb_and(n693, n754);
    let n762: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n765: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n766: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n765);
    let n776: ZN = zsel_n(n690, n766, n765);
    let n777: ZN = zsel_n(n684, n776, n765);
    let n781: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n726);
    let n782: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n728);
    let n783: ZN = zsel_n(n716, n781, n782);
    let n784: ZN = zsel_n(n708, n725, n783);
    let n785: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n784);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zn_lt(n784, zn_splat(P8::from_raw(0i32)));
    let n788: ZB = zsel_b(n786, n787, r_c274);
    let n789: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n290);
    let n790: ZB = zn_tile_flag_at(g.cache, g.cart, n789, n738, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n791: ZN = zsel_n(n790, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n792: ZB = zn_gt(n288, n791);
    let n793: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n726);
    let n794: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n728);
    let n795: ZN = zsel_n(n711, n793, n794);
    let n796: ZN = zsel_n(n708, n725, n795);
    let n797: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n796);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zn_lt(n796, zn_splat(P8::from_raw(0i32)));
    let n800: ZB = zsel_b(n798, n799, r_c274);
    let n801: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n290);
    let n802: ZB = zn_tile_flag_at(g.cache, g.cart, n801, n738, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n803: ZN = zsel_n(n802, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n804: ZB = zn_gt(n288, n803);
    let n805: ZB = zb_and(n100, n759);
    let n806: ZN = zsel_n(n805, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n807: ZB = zb_or(r_c41, n805);
    let n808: ZN = zsel_n(n703, r_c20, n806);
    let n809: ZB = zsel_b(n703, r_c41, n807);
    let n813: ZB = zb_and(n688, n691);
    let n814: ZB = zb_and(n754, n813);
    let n815: ZB = zb_and(n755, n813);
    let n816: ZB = zb_not(n814);
    let n817: ZB = zb_or(n760, n814);
    let n818: ZB = zsel_b(n814, n689, n694);
    let n819: ZN = zsel_n(n814, r_c87, n777);
    let n820: ZN = zsel_n(n814, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n832: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n833: ZN = zn_sub(n77, zn_splat(P8::from_raw(32768i32)));
    let n834: ZN = zn_sub(n833, n78);
    let n835: ZN = zsel_n(n144, zn_splat(P8::from_raw(0i32)), n834);
    let n836: ZN = zsel_n(n97, n834, n835);
    let n837: ZN = zsel_n(n141, zn_splat(P8::from_raw(0i32)), n836);
    let n838: ZN = zsel_n(n95, n834, n837);
    let n839: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n838);
    let n840: ZN = zsel_n(n93, n834, n839);
    let n841: ZN = zsel_n(n135, zn_splat(P8::from_raw(0i32)), n840);
    let n842: ZN = zsel_n(n91, n834, n841);
    let n843: ZN = zsel_n(n132, zn_splat(P8::from_raw(0i32)), n842);
    let n844: ZN = zsel_n(n89, n834, n843);
    let n845: ZN = zsel_n(n129, zn_splat(P8::from_raw(0i32)), n844);
    let n846: ZN = zsel_n(n87, n834, n845);
    let n847: ZN = zsel_n(n126, zn_splat(P8::from_raw(0i32)), n846);
    let n848: ZN = zsel_n(n85, n834, n847);
    let n849: ZN = zsel_n(n123, zn_splat(P8::from_raw(0i32)), n848);
    let n850: ZN = zn_sub(n191, zn_splat(P8::from_raw(32768i32)));
    let n851: ZN = zn_sub(n850, n192);
    let n852: ZN = zsel_n(n236, zn_splat(P8::from_raw(0i32)), n851);
    let n853: ZN = zsel_n(n233, n851, n852);
    let n854: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n853);
    let n855: ZN = zsel_n(n228, n851, n854);
    let n856: ZN = zsel_n(n226, zn_splat(P8::from_raw(0i32)), n855);
    let n857: ZN = zsel_n(n223, n851, n856);
    let n858: ZN = zsel_n(n221, zn_splat(P8::from_raw(0i32)), n857);
    let n859: ZN = zsel_n(n218, n851, n858);
    let n860: ZN = zsel_n(n216, zn_splat(P8::from_raw(0i32)), n859);
    let n861: ZN = zsel_n(n213, n851, n860);
    let n862: ZN = zsel_n(n211, zn_splat(P8::from_raw(0i32)), n861);
    let n863: ZN = zsel_n(n208, n851, n862);
    let n864: ZN = zsel_n(n206, zn_splat(P8::from_raw(0i32)), n863);
    let n865: ZN = zsel_n(n203, n851, n864);
    let n866: ZN = zsel_n(n201, zn_splat(P8::from_raw(0i32)), n865);
    let n867: ZN = zsel_n(n117, n849, r_c280);
    let n868: ZN = zsel_n(n117, n866, r_c281);
    let n869: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n870: ZN = zn_sub(r_c238, zn_splat(P8::from_raw(65536i32)));
    let n871: ZN = zn_sub(n287, r_c270);
    let n872: ZN = zn_max(r_c272, n871);
    let n873: ZN = zn_add(n287, r_c270);
    let n874: ZN = zn_min(r_c272, n873);
    let n875: ZN = zsel_n(n704, n872, n874);
    let n876: ZN = zn_sub(n288, r_c271);
    let n877: ZN = zn_max(r_c273, n876);
    let n878: ZN = zn_add(n288, r_c271);
    let n879: ZN = zn_min(r_c273, n878);
    let n880: ZN = zsel_n(n705, n877, n879);
    let n881: ZN = zsel_n(n737, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n882: ZN = zn_sub(n288, n881);
    let n883: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n882);
    let n884: ZN = zn_add(n288, n881);
    let n885: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n884);
    let n886: ZN = zsel_n(n739, n883, n885);
    let n887: ZN = zsel_n(n698, n886, n288);
    let n888: ZN = zn_neg(n746);
    let n889: ZN = zn_mul(n888, zn_splat(P8::from_raw(131072i32)));
    let n890: ZN = zsel_n(n748, n889, n731);
    let n891: ZN = zsel_n(n748, zn_splat(P8::from_raw(-131072i32)), n887);
    let n892: ZN = zsel_n(n740, zn_splat(P8::from_raw(0i32)), n702);
    let n893: ZN = zsel_n(n740, n731, n890);
    let n894: ZN = zsel_n(n740, zn_splat(P8::from_raw(-131072i32)), n891);
    let n895: ZN = zsel_n(n751, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n896: ZN = zsel_n(n750, zn_splat(P8::from_raw(131072i32)), n895);
    let n897: ZN = zsel_n(n753, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n898: ZB = zsel_b(n703, r_c274, n735);
    let n899: ZN = zsel_n(n762, r_c241, n702);
    let n900: ZB = zb_and(r_c248, n762);
    let n901: ZB = zb_and(r_c249, n762);
    let n902: ZN = zsel_n(n762, r_c255, n285);
    let n903: ZN = zsel_n(n762, r_c256, n286);
    let n904: ZB = zsel_b(n762, r_c274, n898);
    let n905: ZN = zsel_n(n762, r_c280, n867);
    let n906: ZN = zsel_n(n762, r_c281, n868);
    let n907: ZB = zb_or(n689, n762);
    let n908: ZB = zn_lt(n902, zn_splat(P8::from_raw(-65536i32)));
    let n909: ZB = zn_gt(n902, zn_splat(P8::from_raw(7929856i32)));
    let n910: ZB = zb_or(n908, n909);
    let n911: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n902);
    let n912: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n911);
    let n913: ZN = zsel_n(n910, n912, n902);
    let n914: ZN = zn_sub(n758, zn_splat(P8::from_raw(65536i32)));
    let n915: ZN = zsel_n(n703, n870, r_c238);
    let n916: ZN = zsel_n(n703, n875, n731);
    let n917: ZN = zsel_n(n703, n880, n887);
    let n918: ZN = zsel_n(n762, n832, r_c20);
    let n919: ZN = zsel_n(n762, r_c236, n869);
    let n920: ZN = zsel_n(n762, r_c238, n915);
    let n921: ZN = zsel_n(n762, r_c239, n758);
    let n922: ZN = zsel_n(n762, r_c282, n916);
    let n923: ZN = zsel_n(n762, r_c283, n917);
    let n924: ZB = zb_or(n762, n815);
    let n925: ZB = zn_gt(n918, zn_splat(P8::from_raw(0i32)));
    let n926: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n922);
    let n927: ZN = zsel_n(n925, n902, n913);
    let n928: ZN = zsel_n(n925, n922, n926);
    let n930: ZN = zn_max(n791, n882);
    let n931: ZN = zn_min(n791, n884);
    let n932: ZN = zsel_n(n792, n930, n931);
    let n933: ZN = zsel_n(n698, n932, n288);
    let n934: ZN = zsel_n(n748, n889, n784);
    let n935: ZN = zsel_n(n748, zn_splat(P8::from_raw(-131072i32)), n933);
    let n936: ZN = zsel_n(n740, n784, n934);
    let n937: ZN = zsel_n(n740, zn_splat(P8::from_raw(-131072i32)), n935);
    let n938: ZB = zsel_b(n703, r_c274, n788);
    let n939: ZB = zsel_b(n762, r_c274, n938);
    let n940: ZN = zsel_n(n703, n875, n784);
    let n941: ZN = zsel_n(n703, n880, n933);
    let n942: ZN = zsel_n(n762, r_c282, n940);
    let n943: ZN = zsel_n(n762, r_c283, n941);
    let n944: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n942);
    let n945: ZN = zsel_n(n925, n942, n944);
    let n946: ZN = zn_max(n803, n882);
    let n947: ZN = zn_min(n803, n884);
    let n948: ZN = zsel_n(n804, n946, n947);
    let n949: ZN = zsel_n(n698, n948, n288);
    let n950: ZN = zsel_n(n748, n889, n796);
    let n951: ZN = zsel_n(n748, zn_splat(P8::from_raw(-131072i32)), n949);
    let n952: ZN = zsel_n(n740, n796, n950);
    let n953: ZN = zsel_n(n740, zn_splat(P8::from_raw(-131072i32)), n951);
    let n954: ZB = zsel_b(n703, r_c274, n800);
    let n955: ZB = zsel_b(n762, r_c274, n954);
    let n956: ZN = zsel_n(n703, n875, n796);
    let n957: ZN = zsel_n(n703, n880, n949);
    let n958: ZN = zsel_n(n762, r_c282, n956);
    let n959: ZN = zsel_n(n762, r_c283, n957);
    let n960: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n958);
    let n961: ZN = zsel_n(n925, n958, n960);
    let n962: ZN = zsel_n(n55, n892, n702);
    let n963: ZN = zsel_n(n55, n893, n731);
    let n964: ZN = zsel_n(n55, n894, n887);
    let n965: ZN = zsel_n(n703, n702, n962);
    let n966: ZN = zsel_n(n762, r_c241, n965);
    let n967: ZB = zb_or(r_c249, n74);
    let n968: ZN = zsel_n(n703, n875, n963);
    let n969: ZN = zsel_n(n703, n880, n964);
    let n970: ZN = zsel_n(n762, r_c282, n968);
    let n971: ZN = zsel_n(n762, r_c283, n969);
    let n972: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n970);
    let n973: ZN = zsel_n(n925, n970, n972);
    let n974: ZN = zsel_n(n55, n936, n784);
    let n975: ZN = zsel_n(n55, n937, n933);
    let n976: ZN = zsel_n(n703, n875, n974);
    let n977: ZN = zsel_n(n703, n880, n975);
    let n978: ZN = zsel_n(n762, r_c282, n976);
    let n979: ZN = zsel_n(n762, r_c283, n977);
    let n980: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n978);
    let n981: ZN = zsel_n(n925, n978, n980);
    let n982: ZN = zsel_n(n55, n952, n796);
    let n983: ZN = zsel_n(n55, n953, n949);
    let n984: ZN = zsel_n(n703, n875, n982);
    let n985: ZN = zsel_n(n703, n880, n983);
    let n986: ZN = zsel_n(n762, r_c282, n984);
    let n987: ZN = zsel_n(n762, r_c283, n985);
    let n988: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n986);
    let n989: ZN = zsel_n(n925, n986, n988);
    let n990: ZB = zb_or(r_c248, n74);
    let n991: ZN = zsel_n(n805, zn_splat(P8::from_raw(655360i32)), n869);
    let n992: ZN = zsel_n(n805, zn_splat(P8::from_raw(262144i32)), r_c238);
    let n993: ZN = zsel_n(n805, n914, n758);
    let n994: ZN = zsel_n(n805, zn_splat(P8::from_raw(98304i32)), r_c270);
    let n995: ZN = zsel_n(n805, n897, r_c271);
    let n996: ZN = zsel_n(n805, n896, r_c272);
    let n997: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), r_c273);
    let n998: ZN = zsel_n(n805, n749, n731);
    let n999: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n887);
    let n1000: ZN = zsel_n(n703, n869, n991);
    let n1001: ZN = zsel_n(n703, n870, n992);
    let n1002: ZN = zsel_n(n703, n758, n993);
    let n1003: ZN = zsel_n(n703, r_c270, n994);
    let n1004: ZN = zsel_n(n703, r_c271, n995);
    let n1005: ZN = zsel_n(n703, r_c272, n996);
    let n1006: ZN = zsel_n(n703, r_c273, n997);
    let n1007: ZN = zsel_n(n703, n875, n998);
    let n1008: ZN = zsel_n(n703, n880, n999);
    let n1009: ZN = zsel_n(n762, n832, n808);
    let n1010: ZB = zsel_b(n762, r_c41, n809);
    let n1011: ZN = zsel_n(n762, r_c236, n1000);
    let n1012: ZN = zsel_n(n762, r_c238, n1001);
    let n1013: ZN = zsel_n(n762, r_c239, n1002);
    let n1014: ZN = zsel_n(n762, r_c270, n1003);
    let n1015: ZN = zsel_n(n762, r_c271, n1004);
    let n1016: ZN = zsel_n(n762, r_c272, n1005);
    let n1017: ZN = zsel_n(n762, r_c273, n1006);
    let n1018: ZN = zsel_n(n762, r_c282, n1007);
    let n1019: ZN = zsel_n(n762, r_c283, n1008);
    let n1020: ZB = zn_gt(n1009, zn_splat(P8::from_raw(0i32)));
    let n1021: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1018);
    let n1022: ZN = zsel_n(n1020, n902, n913);
    let n1023: ZN = zsel_n(n1020, n1018, n1021);
    let n1024: ZN = zsel_n(n805, zn_splat(P8::from_raw(69510i32)), r_c271);
    let n1025: ZN = zsel_n(n805, zn_splat(P8::from_raw(-131072i32)), r_c272);
    let n1026: ZN = zsel_n(n805, zn_splat(P8::from_raw(-327680i32)), n784);
    let n1027: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n933);
    let n1028: ZN = zsel_n(n703, r_c271, n1024);
    let n1029: ZN = zsel_n(n703, r_c272, n1025);
    let n1030: ZN = zsel_n(n703, n875, n1026);
    let n1031: ZN = zsel_n(n703, n880, n1027);
    let n1032: ZN = zsel_n(n762, r_c271, n1028);
    let n1033: ZN = zsel_n(n762, r_c272, n1029);
    let n1034: ZN = zsel_n(n762, r_c282, n1030);
    let n1035: ZN = zsel_n(n762, r_c283, n1031);
    let n1036: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1034);
    let n1037: ZN = zsel_n(n1020, n1034, n1036);
    let n1038: ZN = zsel_n(n805, zn_splat(P8::from_raw(131072i32)), r_c272);
    let n1039: ZN = zsel_n(n805, zn_splat(P8::from_raw(327680i32)), n796);
    let n1040: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n949);
    let n1041: ZN = zsel_n(n703, r_c272, n1038);
    let n1042: ZN = zsel_n(n703, n875, n1039);
    let n1043: ZN = zsel_n(n703, n880, n1040);
    let n1044: ZN = zsel_n(n762, r_c272, n1041);
    let n1045: ZN = zsel_n(n762, r_c282, n1042);
    let n1046: ZN = zsel_n(n762, r_c283, n1043);
    let n1047: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1045);
    let n1048: ZN = zsel_n(n1020, n1045, n1047);
    let n1050: ZN = zsel_n(n805, zn_splat(P8::from_raw(69510i32)), r_c270);
    let n1051: ZN = zsel_n(n805, zn_splat(P8::from_raw(98304i32)), r_c271);
    let n1052: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), r_c272);
    let n1053: ZN = zsel_n(n805, zn_splat(P8::from_raw(-98304i32)), r_c273);
    let n1054: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n731);
    let n1055: ZN = zsel_n(n805, zn_splat(P8::from_raw(-327680i32)), n887);
    let n1056: ZN = zsel_n(n703, r_c270, n1050);
    let n1057: ZN = zsel_n(n703, r_c271, n1051);
    let n1058: ZN = zsel_n(n703, r_c272, n1052);
    let n1059: ZN = zsel_n(n703, r_c273, n1053);
    let n1060: ZN = zsel_n(n703, n875, n1054);
    let n1061: ZN = zsel_n(n703, n880, n1055);
    let n1062: ZN = zsel_n(n762, r_c270, n1056);
    let n1063: ZN = zsel_n(n762, r_c271, n1057);
    let n1064: ZN = zsel_n(n762, r_c272, n1058);
    let n1065: ZN = zsel_n(n762, r_c273, n1059);
    let n1066: ZN = zsel_n(n762, r_c282, n1060);
    let n1067: ZN = zsel_n(n762, r_c283, n1061);
    let n1068: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1066);
    let n1069: ZN = zsel_n(n1020, n1066, n1068);
    let n1070: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n784);
    let n1071: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n933);
    let n1072: ZN = zsel_n(n703, n875, n1070);
    let n1073: ZN = zsel_n(n703, n880, n1071);
    let n1074: ZN = zsel_n(n762, r_c282, n1072);
    let n1075: ZN = zsel_n(n762, r_c283, n1073);
    let n1076: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1074);
    let n1077: ZN = zsel_n(n1020, n1074, n1076);
    let n1078: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n796);
    let n1079: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n949);
    let n1080: ZN = zsel_n(n703, n875, n1078);
    let n1081: ZN = zsel_n(n703, n880, n1079);
    let n1082: ZN = zsel_n(n762, r_c282, n1080);
    let n1083: ZN = zsel_n(n762, r_c283, n1081);
    let n1084: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1082);
    let n1085: ZN = zsel_n(n1020, n1082, n1084);
    let n1086: ZN = zsel_n(n805, zn_splat(P8::from_raw(131072i32)), r_c273);
    let n1087: ZN = zsel_n(n805, zn_splat(P8::from_raw(327680i32)), n887);
    let n1088: ZN = zsel_n(n703, r_c273, n1086);
    let n1089: ZN = zsel_n(n703, n880, n1087);
    let n1090: ZN = zsel_n(n762, r_c273, n1088);
    let n1091: ZN = zsel_n(n762, r_c283, n1089);
    let n1092: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n933);
    let n1093: ZN = zsel_n(n703, n880, n1092);
    let n1094: ZN = zsel_n(n762, r_c283, n1093);
    let n1095: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n949);
    let n1096: ZN = zsel_n(n703, n880, n1095);
    let n1097: ZN = zsel_n(n762, r_c283, n1096);
    let n1098: ZN = zsel_n(n805, n749, n963);
    let n1099: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n964);
    let n1100: ZN = zsel_n(n703, n875, n1098);
    let n1101: ZN = zsel_n(n703, n880, n1099);
    let n1102: ZN = zsel_n(n762, r_c282, n1100);
    let n1103: ZN = zsel_n(n762, r_c283, n1101);
    let n1104: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1102);
    let n1105: ZN = zsel_n(n1020, n1102, n1104);
    let n1106: ZN = zsel_n(n805, zn_splat(P8::from_raw(-327680i32)), n974);
    let n1107: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n975);
    let n1108: ZN = zsel_n(n703, n875, n1106);
    let n1109: ZN = zsel_n(n703, n880, n1107);
    let n1110: ZN = zsel_n(n762, r_c282, n1108);
    let n1111: ZN = zsel_n(n762, r_c283, n1109);
    let n1112: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1110);
    let n1113: ZN = zsel_n(n1020, n1110, n1112);
    let n1114: ZN = zsel_n(n805, zn_splat(P8::from_raw(327680i32)), n982);
    let n1115: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n983);
    let n1116: ZN = zsel_n(n703, n875, n1114);
    let n1117: ZN = zsel_n(n703, n880, n1115);
    let n1118: ZN = zsel_n(n762, r_c282, n1116);
    let n1119: ZN = zsel_n(n762, r_c283, n1117);
    let n1120: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1118);
    let n1121: ZN = zsel_n(n1020, n1118, n1120);
    let n1122: ZN = zsel_n(n805, zn_splat(P8::from_raw(0i32)), n963);
    let n1123: ZN = zsel_n(n805, zn_splat(P8::from_raw(-327680i32)), n964);
    let n1124: ZN = zsel_n(n703, n875, n1122);
    let n1125: ZN = zsel_n(n703, n880, n1123);
    let n1126: ZN = zsel_n(n762, r_c282, n1124);
    let n1127: ZN = zsel_n(n762, r_c283, n1125);
    let n1128: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1126);
    let n1129: ZN = zsel_n(n1020, n1126, n1128);
    let n1130: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n974);
    let n1131: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n975);
    let n1132: ZN = zsel_n(n703, n875, n1130);
    let n1133: ZN = zsel_n(n703, n880, n1131);
    let n1134: ZN = zsel_n(n762, r_c282, n1132);
    let n1135: ZN = zsel_n(n762, r_c283, n1133);
    let n1136: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1134);
    let n1137: ZN = zsel_n(n1020, n1134, n1136);
    let n1138: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n982);
    let n1139: ZN = zsel_n(n805, zn_splat(P8::from_raw(-231700i32)), n983);
    let n1140: ZN = zsel_n(n703, n875, n1138);
    let n1141: ZN = zsel_n(n703, n880, n1139);
    let n1142: ZN = zsel_n(n762, r_c282, n1140);
    let n1143: ZN = zsel_n(n762, r_c283, n1141);
    let n1144: ZN = zsel_n(n910, zn_splat(P8::from_raw(0i32)), n1142);
    let n1145: ZN = zsel_n(n1020, n1142, n1144);
    let n1146: ZN = zsel_n(n805, zn_splat(P8::from_raw(327680i32)), n964);
    let n1147: ZN = zsel_n(n703, n880, n1146);
    let n1148: ZN = zsel_n(n762, r_c283, n1147);
    let n1149: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n975);
    let n1150: ZN = zsel_n(n703, n880, n1149);
    let n1151: ZN = zsel_n(n762, r_c283, n1150);
    let n1152: ZN = zsel_n(n805, zn_splat(P8::from_raw(231700i32)), n983);
    let n1153: ZN = zsel_n(n703, n880, n1152);
    let n1154: ZN = zsel_n(n762, r_c283, n1153);
    let n1156: ZW = zw_cellmix_n(84u64, n63, 1542469173u64);
    let n1157: ZW = zw_cellmix_n(84u64, n63, 668265263u64);
    let n1158: ZW = zw_add(zw_splat(0u64), n1156);
    let n1159: ZW = zw_add(zw_splat(0u64), n1157);
    let n1160: ZW = zw_cellmix_n(85u64, n115, 1542469173u64);
    let n1161: ZW = zw_cellmix_n(85u64, n115, 668265263u64);
    let n1162: ZW = zw_add(n1158, n1160);
    let n1163: ZW = zw_add(n1159, n1161);
    let n1164: ZW = zw_cellmix_n(86u64, n114, 1542469173u64);
    let n1165: ZW = zw_cellmix_n(86u64, n114, 668265263u64);
    let n1166: ZW = zw_add(n1162, n1164);
    let n1167: ZW = zw_add(n1163, n1165);
    let n1168: ZW = zw_cellmix_n(87u64, n777, 1542469173u64);
    let n1169: ZW = zw_cellmix_n(87u64, n777, 668265263u64);
    let n1170: ZW = zw_add(n1166, n1168);
    let n1171: ZW = zw_add(n1167, n1169);
    let n1172: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n1173: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n1174: ZW = zw_add(n1170, n1172);
    let n1175: ZW = zw_add(n1171, n1173);
    let n1176: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n1177: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n1178: ZW = zw_add(n1174, n1176);
    let n1179: ZW = zw_add(n1175, n1177);
    let n1180: ZW = zw_cellmix_n(20u64, n808, 1542469173u64);
    let n1181: ZW = zw_cellmix_n(20u64, n808, 668265263u64);
    let n1182: ZW = zw_add(n1170, n1180);
    let n1183: ZW = zw_add(n1171, n1181);
    let n1184: ZW = zw_cellmix_b(41u64, n809, 1542469173u64);
    let n1185: ZW = zw_cellmix_b(41u64, n809, 668265263u64);
    let n1186: ZW = zw_add(n1182, n1184);
    let n1187: ZW = zw_add(n1183, n1185);
    let n1188: ZW = zw_cellmix_b(38u64, n816, 1542469173u64);
    let n1189: ZW = zw_cellmix_b(38u64, n816, 668265263u64);
    let n1190: ZW = zw_add(zw_splat(0u64), n1188);
    let n1191: ZW = zw_add(zw_splat(0u64), n1189);
    let n1192: ZW = zw_cellmix_n(39u64, n820, 1542469173u64);
    let n1193: ZW = zw_cellmix_n(39u64, n820, 668265263u64);
    let n1194: ZW = zw_add(n1190, n1192);
    let n1195: ZW = zw_add(n1191, n1193);
    let n1196: ZW = zw_add(n1194, n1156);
    let n1197: ZW = zw_add(n1195, n1157);
    let n1198: ZW = zw_add(n1196, n1160);
    let n1199: ZW = zw_add(n1197, n1161);
    let n1200: ZW = zw_add(n1198, n1164);
    let n1201: ZW = zw_add(n1199, n1165);
    let n1202: ZW = zw_cellmix_n(87u64, n819, 1542469173u64);
    let n1203: ZW = zw_cellmix_n(87u64, n819, 668265263u64);
    let n1204: ZW = zw_add(n1200, n1202);
    let n1205: ZW = zw_add(n1201, n1203);
    let n1206: ZW = zw_add(n1204, n1172);
    let n1207: ZW = zw_add(n1205, n1173);
    let n1208: ZW = zw_add(n1204, n1180);
    let n1209: ZW = zw_add(n1205, n1181);
    let n1210: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n1211: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n1212: ZW = zw_add(zw_splat(0u64), n1210);
    let n1213: ZW = zw_add(zw_splat(0u64), n1211);
    let n1214: ZW = zw_add(n1212, n1156);
    let n1215: ZW = zw_add(n1213, n1157);
    let n1216: ZW = zw_add(n1214, n1160);
    let n1217: ZW = zw_add(n1215, n1161);
    let n1218: ZW = zw_add(n1216, n1164);
    let n1219: ZW = zw_add(n1217, n1165);
    let n1220: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n1221: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n1222: ZW = zw_add(n1218, n1220);
    let n1223: ZW = zw_add(n1219, n1221);
    let n1224: ZW = zw_cellmix_n(256u64, n903, 1542469173u64);
    let n1225: ZW = zw_cellmix_n(256u64, n903, 668265263u64);
    let n1226: ZW = zw_add(n1222, n1224);
    let n1227: ZW = zw_add(n1223, n1225);
    let n1228: ZW = zw_cellmix_n(280u64, n905, 1542469173u64);
    let n1229: ZW = zw_cellmix_n(280u64, n905, 668265263u64);
    let n1230: ZW = zw_add(n1226, n1228);
    let n1231: ZW = zw_add(n1227, n1229);
    let n1232: ZW = zw_cellmix_n(281u64, n906, 1542469173u64);
    let n1233: ZW = zw_cellmix_n(281u64, n906, 668265263u64);
    let n1234: ZW = zw_add(n1230, n1232);
    let n1235: ZW = zw_add(n1231, n1233);
    let n1236: ZW = zw_cellmix_n(20u64, n918, 1542469173u64);
    let n1237: ZW = zw_cellmix_n(20u64, n918, 668265263u64);
    let n1238: ZW = zw_add(n1234, n1236);
    let n1239: ZW = zw_add(n1235, n1237);
    let n1240: ZW = zw_add(n1238, n1176);
    let n1241: ZW = zw_add(n1239, n1177);
    let n1242: ZW = zw_cellmix_n(236u64, n919, 1542469173u64);
    let n1243: ZW = zw_cellmix_n(236u64, n919, 668265263u64);
    let n1244: ZW = zw_add(n1240, n1242);
    let n1245: ZW = zw_add(n1241, n1243);
    let n1246: ZW = zw_cellmix_n(238u64, n920, 1542469173u64);
    let n1247: ZW = zw_cellmix_n(238u64, n920, 668265263u64);
    let n1248: ZW = zw_add(n1244, n1246);
    let n1249: ZW = zw_add(n1245, n1247);
    let n1250: ZW = zw_cellmix_n(239u64, n921, 1542469173u64);
    let n1251: ZW = zw_cellmix_n(239u64, n921, 668265263u64);
    let n1252: ZW = zw_add(n1248, n1250);
    let n1253: ZW = zw_add(n1249, n1251);
    let n1254: ZW = zw_cellmix_n(241u64, n899, 1542469173u64);
    let n1255: ZW = zw_cellmix_n(241u64, n899, 668265263u64);
    let n1256: ZW = zw_add(n1252, n1254);
    let n1257: ZW = zw_add(n1253, n1255);
    let n1258: ZW = zw_cellmix_b(248u64, n900, 1542469173u64);
    let n1259: ZW = zw_cellmix_b(248u64, n900, 668265263u64);
    let n1260: ZW = zw_add(n1256, n1258);
    let n1261: ZW = zw_add(n1257, n1259);
    let n1262: ZW = zw_cellmix_b(249u64, n901, 1542469173u64);
    let n1263: ZW = zw_cellmix_b(249u64, n901, 668265263u64);
    let n1264: ZW = zw_add(n1260, n1262);
    let n1265: ZW = zw_add(n1261, n1263);
    let n1266: ZW = zw_cellmix_n(255u64, n927, 1542469173u64);
    let n1267: ZW = zw_cellmix_n(255u64, n927, 668265263u64);
    let n1268: ZW = zw_add(n1264, n1266);
    let n1269: ZW = zw_add(n1265, n1267);
    let n1270: ZW = zw_cellmix_n(270u64, r_c270, 1542469173u64);
    let n1271: ZW = zw_cellmix_n(270u64, r_c270, 668265263u64);
    let n1272: ZW = zw_add(n1268, n1270);
    let n1273: ZW = zw_add(n1269, n1271);
    let n1274: ZW = zw_cellmix_n(271u64, r_c271, 1542469173u64);
    let n1275: ZW = zw_cellmix_n(271u64, r_c271, 668265263u64);
    let n1276: ZW = zw_add(n1272, n1274);
    let n1277: ZW = zw_add(n1273, n1275);
    let n1278: ZW = zw_cellmix_n(272u64, r_c272, 1542469173u64);
    let n1279: ZW = zw_cellmix_n(272u64, r_c272, 668265263u64);
    let n1280: ZW = zw_add(n1276, n1278);
    let n1281: ZW = zw_add(n1277, n1279);
    let n1282: ZW = zw_cellmix_n(273u64, r_c273, 1542469173u64);
    let n1283: ZW = zw_cellmix_n(273u64, r_c273, 668265263u64);
    let n1284: ZW = zw_add(n1280, n1282);
    let n1285: ZW = zw_add(n1281, n1283);
    let n1286: ZW = zw_cellmix_b(274u64, n904, 1542469173u64);
    let n1287: ZW = zw_cellmix_b(274u64, n904, 668265263u64);
    let n1288: ZW = zw_add(n1284, n1286);
    let n1289: ZW = zw_add(n1285, n1287);
    let n1290: ZW = zw_cellmix_n(282u64, n928, 1542469173u64);
    let n1291: ZW = zw_cellmix_n(282u64, n928, 668265263u64);
    let n1292: ZW = zw_add(n1288, n1290);
    let n1293: ZW = zw_add(n1289, n1291);
    let n1294: ZW = zw_cellmix_n(283u64, n923, 1542469173u64);
    let n1295: ZW = zw_cellmix_n(283u64, n923, 668265263u64);
    let n1296: ZW = zw_add(n1292, n1294);
    let n1297: ZW = zw_add(n1293, n1295);
    let n1298: ZW = zw_cellmix_b(274u64, n939, 1542469173u64);
    let n1299: ZW = zw_cellmix_b(274u64, n939, 668265263u64);
    let n1300: ZW = zw_add(n1284, n1298);
    let n1301: ZW = zw_add(n1285, n1299);
    let n1302: ZW = zw_cellmix_n(282u64, n945, 1542469173u64);
    let n1303: ZW = zw_cellmix_n(282u64, n945, 668265263u64);
    let n1304: ZW = zw_add(n1300, n1302);
    let n1305: ZW = zw_add(n1301, n1303);
    let n1306: ZW = zw_cellmix_n(283u64, n943, 1542469173u64);
    let n1307: ZW = zw_cellmix_n(283u64, n943, 668265263u64);
    let n1308: ZW = zw_add(n1304, n1306);
    let n1309: ZW = zw_add(n1305, n1307);
    let n1310: ZW = zw_cellmix_b(274u64, n955, 1542469173u64);
    let n1311: ZW = zw_cellmix_b(274u64, n955, 668265263u64);
    let n1312: ZW = zw_add(n1284, n1310);
    let n1313: ZW = zw_add(n1285, n1311);
    let n1314: ZW = zw_cellmix_n(282u64, n961, 1542469173u64);
    let n1315: ZW = zw_cellmix_n(282u64, n961, 668265263u64);
    let n1316: ZW = zw_add(n1312, n1314);
    let n1317: ZW = zw_add(n1313, n1315);
    let n1318: ZW = zw_cellmix_n(283u64, n959, 1542469173u64);
    let n1319: ZW = zw_cellmix_n(283u64, n959, 668265263u64);
    let n1320: ZW = zw_add(n1316, n1318);
    let n1321: ZW = zw_add(n1317, n1319);
    let n1322: ZW = zw_cellmix_n(241u64, n966, 1542469173u64);
    let n1323: ZW = zw_cellmix_n(241u64, n966, 668265263u64);
    let n1324: ZW = zw_add(n1252, n1322);
    let n1325: ZW = zw_add(n1253, n1323);
    let n1326: ZW = zw_add(n1324, n1258);
    let n1327: ZW = zw_add(n1325, n1259);
    let n1328: ZW = zw_cellmix_b(249u64, n967, 1542469173u64);
    let n1329: ZW = zw_cellmix_b(249u64, n967, 668265263u64);
    let n1330: ZW = zw_add(n1326, n1328);
    let n1331: ZW = zw_add(n1327, n1329);
    let n1332: ZW = zw_add(n1330, n1266);
    let n1333: ZW = zw_add(n1331, n1267);
    let n1334: ZW = zw_add(n1332, n1270);
    let n1335: ZW = zw_add(n1333, n1271);
    let n1336: ZW = zw_add(n1334, n1274);
    let n1337: ZW = zw_add(n1335, n1275);
    let n1338: ZW = zw_add(n1336, n1278);
    let n1339: ZW = zw_add(n1337, n1279);
    let n1340: ZW = zw_add(n1338, n1282);
    let n1341: ZW = zw_add(n1339, n1283);
    let n1342: ZW = zw_add(n1340, n1286);
    let n1343: ZW = zw_add(n1341, n1287);
    let n1344: ZW = zw_cellmix_n(282u64, n973, 1542469173u64);
    let n1345: ZW = zw_cellmix_n(282u64, n973, 668265263u64);
    let n1346: ZW = zw_add(n1342, n1344);
    let n1347: ZW = zw_add(n1343, n1345);
    let n1348: ZW = zw_cellmix_n(283u64, n971, 1542469173u64);
    let n1349: ZW = zw_cellmix_n(283u64, n971, 668265263u64);
    let n1350: ZW = zw_add(n1346, n1348);
    let n1351: ZW = zw_add(n1347, n1349);
    let n1352: ZW = zw_add(n1340, n1298);
    let n1353: ZW = zw_add(n1341, n1299);
    let n1354: ZW = zw_cellmix_n(282u64, n981, 1542469173u64);
    let n1355: ZW = zw_cellmix_n(282u64, n981, 668265263u64);
    let n1356: ZW = zw_add(n1352, n1354);
    let n1357: ZW = zw_add(n1353, n1355);
    let n1358: ZW = zw_cellmix_n(283u64, n979, 1542469173u64);
    let n1359: ZW = zw_cellmix_n(283u64, n979, 668265263u64);
    let n1360: ZW = zw_add(n1356, n1358);
    let n1361: ZW = zw_add(n1357, n1359);
    let n1362: ZW = zw_add(n1340, n1310);
    let n1363: ZW = zw_add(n1341, n1311);
    let n1364: ZW = zw_cellmix_n(282u64, n989, 1542469173u64);
    let n1365: ZW = zw_cellmix_n(282u64, n989, 668265263u64);
    let n1366: ZW = zw_add(n1362, n1364);
    let n1367: ZW = zw_add(n1363, n1365);
    let n1368: ZW = zw_cellmix_n(283u64, n987, 1542469173u64);
    let n1369: ZW = zw_cellmix_n(283u64, n987, 668265263u64);
    let n1370: ZW = zw_add(n1366, n1368);
    let n1371: ZW = zw_add(n1367, n1369);
    let n1372: ZW = zw_cellmix_n(20u64, n1009, 1542469173u64);
    let n1373: ZW = zw_cellmix_n(20u64, n1009, 668265263u64);
    let n1374: ZW = zw_add(n1234, n1372);
    let n1375: ZW = zw_add(n1235, n1373);
    let n1376: ZW = zw_cellmix_b(41u64, n1010, 1542469173u64);
    let n1377: ZW = zw_cellmix_b(41u64, n1010, 668265263u64);
    let n1378: ZW = zw_add(n1374, n1376);
    let n1379: ZW = zw_add(n1375, n1377);
    let n1380: ZW = zw_cellmix_n(236u64, n1011, 1542469173u64);
    let n1381: ZW = zw_cellmix_n(236u64, n1011, 668265263u64);
    let n1382: ZW = zw_add(n1378, n1380);
    let n1383: ZW = zw_add(n1379, n1381);
    let n1384: ZW = zw_cellmix_n(238u64, n1012, 1542469173u64);
    let n1385: ZW = zw_cellmix_n(238u64, n1012, 668265263u64);
    let n1386: ZW = zw_add(n1382, n1384);
    let n1387: ZW = zw_add(n1383, n1385);
    let n1388: ZW = zw_cellmix_n(239u64, n1013, 1542469173u64);
    let n1389: ZW = zw_cellmix_n(239u64, n1013, 668265263u64);
    let n1390: ZW = zw_add(n1386, n1388);
    let n1391: ZW = zw_add(n1387, n1389);
    let n1392: ZW = zw_add(n1390, n1254);
    let n1393: ZW = zw_add(n1391, n1255);
    let n1394: ZW = zw_cellmix_b(248u64, n990, 1542469173u64);
    let n1395: ZW = zw_cellmix_b(248u64, n990, 668265263u64);
    let n1396: ZW = zw_add(n1392, n1394);
    let n1397: ZW = zw_add(n1393, n1395);
    let n1398: ZW = zw_add(n1396, n1262);
    let n1399: ZW = zw_add(n1397, n1263);
    let n1400: ZW = zw_cellmix_n(255u64, n1022, 1542469173u64);
    let n1401: ZW = zw_cellmix_n(255u64, n1022, 668265263u64);
    let n1402: ZW = zw_add(n1398, n1400);
    let n1403: ZW = zw_add(n1399, n1401);
    let n1404: ZW = zw_cellmix_n(270u64, n1014, 1542469173u64);
    let n1405: ZW = zw_cellmix_n(270u64, n1014, 668265263u64);
    let n1406: ZW = zw_add(n1402, n1404);
    let n1407: ZW = zw_add(n1403, n1405);
    let n1408: ZW = zw_cellmix_n(271u64, n1015, 1542469173u64);
    let n1409: ZW = zw_cellmix_n(271u64, n1015, 668265263u64);
    let n1410: ZW = zw_add(n1406, n1408);
    let n1411: ZW = zw_add(n1407, n1409);
    let n1412: ZW = zw_cellmix_n(272u64, n1016, 1542469173u64);
    let n1413: ZW = zw_cellmix_n(272u64, n1016, 668265263u64);
    let n1414: ZW = zw_add(n1410, n1412);
    let n1415: ZW = zw_add(n1411, n1413);
    let n1416: ZW = zw_cellmix_n(273u64, n1017, 1542469173u64);
    let n1417: ZW = zw_cellmix_n(273u64, n1017, 668265263u64);
    let n1418: ZW = zw_add(n1414, n1416);
    let n1419: ZW = zw_add(n1415, n1417);
    let n1420: ZW = zw_add(n1418, n1286);
    let n1421: ZW = zw_add(n1419, n1287);
    let n1422: ZW = zw_cellmix_n(282u64, n1023, 1542469173u64);
    let n1423: ZW = zw_cellmix_n(282u64, n1023, 668265263u64);
    let n1424: ZW = zw_add(n1420, n1422);
    let n1425: ZW = zw_add(n1421, n1423);
    let n1426: ZW = zw_cellmix_n(283u64, n1019, 1542469173u64);
    let n1427: ZW = zw_cellmix_n(283u64, n1019, 668265263u64);
    let n1428: ZW = zw_add(n1424, n1426);
    let n1429: ZW = zw_add(n1425, n1427);
    let n1430: ZW = zw_cellmix_n(271u64, n1032, 1542469173u64);
    let n1431: ZW = zw_cellmix_n(271u64, n1032, 668265263u64);
    let n1432: ZW = zw_add(n1406, n1430);
    let n1433: ZW = zw_add(n1407, n1431);
    let n1434: ZW = zw_cellmix_n(272u64, n1033, 1542469173u64);
    let n1435: ZW = zw_cellmix_n(272u64, n1033, 668265263u64);
    let n1436: ZW = zw_add(n1432, n1434);
    let n1437: ZW = zw_add(n1433, n1435);
    let n1438: ZW = zw_add(n1436, n1416);
    let n1439: ZW = zw_add(n1437, n1417);
    let n1440: ZW = zw_add(n1438, n1298);
    let n1441: ZW = zw_add(n1439, n1299);
    let n1442: ZW = zw_cellmix_n(282u64, n1037, 1542469173u64);
    let n1443: ZW = zw_cellmix_n(282u64, n1037, 668265263u64);
    let n1444: ZW = zw_add(n1440, n1442);
    let n1445: ZW = zw_add(n1441, n1443);
    let n1446: ZW = zw_cellmix_n(283u64, n1035, 1542469173u64);
    let n1447: ZW = zw_cellmix_n(283u64, n1035, 668265263u64);
    let n1448: ZW = zw_add(n1444, n1446);
    let n1449: ZW = zw_add(n1445, n1447);
    let n1450: ZW = zw_cellmix_n(272u64, n1044, 1542469173u64);
    let n1451: ZW = zw_cellmix_n(272u64, n1044, 668265263u64);
    let n1452: ZW = zw_add(n1432, n1450);
    let n1453: ZW = zw_add(n1433, n1451);
    let n1454: ZW = zw_add(n1452, n1416);
    let n1455: ZW = zw_add(n1453, n1417);
    let n1456: ZW = zw_add(n1454, n1310);
    let n1457: ZW = zw_add(n1455, n1311);
    let n1458: ZW = zw_cellmix_n(282u64, n1048, 1542469173u64);
    let n1459: ZW = zw_cellmix_n(282u64, n1048, 668265263u64);
    let n1460: ZW = zw_add(n1456, n1458);
    let n1461: ZW = zw_add(n1457, n1459);
    let n1462: ZW = zw_cellmix_n(283u64, n1046, 1542469173u64);
    let n1463: ZW = zw_cellmix_n(283u64, n1046, 668265263u64);
    let n1464: ZW = zw_add(n1460, n1462);
    let n1465: ZW = zw_add(n1461, n1463);
    let n1466: ZW = zw_cellmix_n(270u64, n1062, 1542469173u64);
    let n1467: ZW = zw_cellmix_n(270u64, n1062, 668265263u64);
    let n1468: ZW = zw_add(n1402, n1466);
    let n1469: ZW = zw_add(n1403, n1467);
    let n1470: ZW = zw_cellmix_n(271u64, n1063, 1542469173u64);
    let n1471: ZW = zw_cellmix_n(271u64, n1063, 668265263u64);
    let n1472: ZW = zw_add(n1468, n1470);
    let n1473: ZW = zw_add(n1469, n1471);
    let n1474: ZW = zw_cellmix_n(272u64, n1064, 1542469173u64);
    let n1475: ZW = zw_cellmix_n(272u64, n1064, 668265263u64);
    let n1476: ZW = zw_add(n1472, n1474);
    let n1477: ZW = zw_add(n1473, n1475);
    let n1478: ZW = zw_cellmix_n(273u64, n1065, 1542469173u64);
    let n1479: ZW = zw_cellmix_n(273u64, n1065, 668265263u64);
    let n1480: ZW = zw_add(n1476, n1478);
    let n1481: ZW = zw_add(n1477, n1479);
    let n1482: ZW = zw_add(n1480, n1286);
    let n1483: ZW = zw_add(n1481, n1287);
    let n1484: ZW = zw_cellmix_n(282u64, n1069, 1542469173u64);
    let n1485: ZW = zw_cellmix_n(282u64, n1069, 668265263u64);
    let n1486: ZW = zw_add(n1482, n1484);
    let n1487: ZW = zw_add(n1483, n1485);
    let n1488: ZW = zw_cellmix_n(283u64, n1067, 1542469173u64);
    let n1489: ZW = zw_cellmix_n(283u64, n1067, 668265263u64);
    let n1490: ZW = zw_add(n1486, n1488);
    let n1491: ZW = zw_add(n1487, n1489);
    let n1492: ZW = zw_add(n1468, n1430);
    let n1493: ZW = zw_add(n1469, n1431);
    let n1494: ZW = zw_add(n1492, n1434);
    let n1495: ZW = zw_add(n1493, n1435);
    let n1496: ZW = zw_add(n1494, n1478);
    let n1497: ZW = zw_add(n1495, n1479);
    let n1498: ZW = zw_add(n1496, n1298);
    let n1499: ZW = zw_add(n1497, n1299);
    let n1500: ZW = zw_cellmix_n(282u64, n1077, 1542469173u64);
    let n1501: ZW = zw_cellmix_n(282u64, n1077, 668265263u64);
    let n1502: ZW = zw_add(n1498, n1500);
    let n1503: ZW = zw_add(n1499, n1501);
    let n1504: ZW = zw_cellmix_n(283u64, n1075, 1542469173u64);
    let n1505: ZW = zw_cellmix_n(283u64, n1075, 668265263u64);
    let n1506: ZW = zw_add(n1502, n1504);
    let n1507: ZW = zw_add(n1503, n1505);
    let n1508: ZW = zw_add(n1492, n1450);
    let n1509: ZW = zw_add(n1493, n1451);
    let n1510: ZW = zw_add(n1508, n1478);
    let n1511: ZW = zw_add(n1509, n1479);
    let n1512: ZW = zw_add(n1510, n1310);
    let n1513: ZW = zw_add(n1511, n1311);
    let n1514: ZW = zw_cellmix_n(282u64, n1085, 1542469173u64);
    let n1515: ZW = zw_cellmix_n(282u64, n1085, 668265263u64);
    let n1516: ZW = zw_add(n1512, n1514);
    let n1517: ZW = zw_add(n1513, n1515);
    let n1518: ZW = zw_cellmix_n(283u64, n1083, 1542469173u64);
    let n1519: ZW = zw_cellmix_n(283u64, n1083, 668265263u64);
    let n1520: ZW = zw_add(n1516, n1518);
    let n1521: ZW = zw_add(n1517, n1519);
    let n1522: ZW = zw_cellmix_n(273u64, n1090, 1542469173u64);
    let n1523: ZW = zw_cellmix_n(273u64, n1090, 668265263u64);
    let n1524: ZW = zw_add(n1476, n1522);
    let n1525: ZW = zw_add(n1477, n1523);
    let n1526: ZW = zw_add(n1524, n1286);
    let n1527: ZW = zw_add(n1525, n1287);
    let n1528: ZW = zw_add(n1526, n1484);
    let n1529: ZW = zw_add(n1527, n1485);
    let n1530: ZW = zw_cellmix_n(283u64, n1091, 1542469173u64);
    let n1531: ZW = zw_cellmix_n(283u64, n1091, 668265263u64);
    let n1532: ZW = zw_add(n1528, n1530);
    let n1533: ZW = zw_add(n1529, n1531);
    let n1534: ZW = zw_add(n1494, n1522);
    let n1535: ZW = zw_add(n1495, n1523);
    let n1536: ZW = zw_add(n1534, n1298);
    let n1537: ZW = zw_add(n1535, n1299);
    let n1538: ZW = zw_add(n1536, n1500);
    let n1539: ZW = zw_add(n1537, n1501);
    let n1540: ZW = zw_cellmix_n(283u64, n1094, 1542469173u64);
    let n1541: ZW = zw_cellmix_n(283u64, n1094, 668265263u64);
    let n1542: ZW = zw_add(n1538, n1540);
    let n1543: ZW = zw_add(n1539, n1541);
    let n1544: ZW = zw_add(n1508, n1522);
    let n1545: ZW = zw_add(n1509, n1523);
    let n1546: ZW = zw_add(n1544, n1310);
    let n1547: ZW = zw_add(n1545, n1311);
    let n1548: ZW = zw_add(n1546, n1514);
    let n1549: ZW = zw_add(n1547, n1515);
    let n1550: ZW = zw_cellmix_n(283u64, n1097, 1542469173u64);
    let n1551: ZW = zw_cellmix_n(283u64, n1097, 668265263u64);
    let n1552: ZW = zw_add(n1548, n1550);
    let n1553: ZW = zw_add(n1549, n1551);
    let n1554: ZW = zw_add(n1390, n1322);
    let n1555: ZW = zw_add(n1391, n1323);
    let n1556: ZW = zw_add(n1554, n1394);
    let n1557: ZW = zw_add(n1555, n1395);
    let n1558: ZW = zw_add(n1556, n1328);
    let n1559: ZW = zw_add(n1557, n1329);
    let n1560: ZW = zw_add(n1558, n1400);
    let n1561: ZW = zw_add(n1559, n1401);
    let n1562: ZW = zw_add(n1560, n1404);
    let n1563: ZW = zw_add(n1561, n1405);
    let n1564: ZW = zw_add(n1562, n1408);
    let n1565: ZW = zw_add(n1563, n1409);
    let n1566: ZW = zw_add(n1564, n1412);
    let n1567: ZW = zw_add(n1565, n1413);
    let n1568: ZW = zw_add(n1566, n1416);
    let n1569: ZW = zw_add(n1567, n1417);
    let n1570: ZW = zw_add(n1568, n1286);
    let n1571: ZW = zw_add(n1569, n1287);
    let n1572: ZW = zw_cellmix_n(282u64, n1105, 1542469173u64);
    let n1573: ZW = zw_cellmix_n(282u64, n1105, 668265263u64);
    let n1574: ZW = zw_add(n1570, n1572);
    let n1575: ZW = zw_add(n1571, n1573);
    let n1576: ZW = zw_cellmix_n(283u64, n1103, 1542469173u64);
    let n1577: ZW = zw_cellmix_n(283u64, n1103, 668265263u64);
    let n1578: ZW = zw_add(n1574, n1576);
    let n1579: ZW = zw_add(n1575, n1577);
    let n1580: ZW = zw_add(n1562, n1430);
    let n1581: ZW = zw_add(n1563, n1431);
    let n1582: ZW = zw_add(n1580, n1434);
    let n1583: ZW = zw_add(n1581, n1435);
    let n1584: ZW = zw_add(n1582, n1416);
    let n1585: ZW = zw_add(n1583, n1417);
    let n1586: ZW = zw_add(n1584, n1298);
    let n1587: ZW = zw_add(n1585, n1299);
    let n1588: ZW = zw_cellmix_n(282u64, n1113, 1542469173u64);
    let n1589: ZW = zw_cellmix_n(282u64, n1113, 668265263u64);
    let n1590: ZW = zw_add(n1586, n1588);
    let n1591: ZW = zw_add(n1587, n1589);
    let n1592: ZW = zw_cellmix_n(283u64, n1111, 1542469173u64);
    let n1593: ZW = zw_cellmix_n(283u64, n1111, 668265263u64);
    let n1594: ZW = zw_add(n1590, n1592);
    let n1595: ZW = zw_add(n1591, n1593);
    let n1596: ZW = zw_add(n1580, n1450);
    let n1597: ZW = zw_add(n1581, n1451);
    let n1598: ZW = zw_add(n1596, n1416);
    let n1599: ZW = zw_add(n1597, n1417);
    let n1600: ZW = zw_add(n1598, n1310);
    let n1601: ZW = zw_add(n1599, n1311);
    let n1602: ZW = zw_cellmix_n(282u64, n1121, 1542469173u64);
    let n1603: ZW = zw_cellmix_n(282u64, n1121, 668265263u64);
    let n1604: ZW = zw_add(n1600, n1602);
    let n1605: ZW = zw_add(n1601, n1603);
    let n1606: ZW = zw_cellmix_n(283u64, n1119, 1542469173u64);
    let n1607: ZW = zw_cellmix_n(283u64, n1119, 668265263u64);
    let n1608: ZW = zw_add(n1604, n1606);
    let n1609: ZW = zw_add(n1605, n1607);
    let n1610: ZW = zw_add(n1560, n1466);
    let n1611: ZW = zw_add(n1561, n1467);
    let n1612: ZW = zw_add(n1610, n1470);
    let n1613: ZW = zw_add(n1611, n1471);
    let n1614: ZW = zw_add(n1612, n1474);
    let n1615: ZW = zw_add(n1613, n1475);
    let n1616: ZW = zw_add(n1614, n1478);
    let n1617: ZW = zw_add(n1615, n1479);
    let n1618: ZW = zw_add(n1616, n1286);
    let n1619: ZW = zw_add(n1617, n1287);
    let n1620: ZW = zw_cellmix_n(282u64, n1129, 1542469173u64);
    let n1621: ZW = zw_cellmix_n(282u64, n1129, 668265263u64);
    let n1622: ZW = zw_add(n1618, n1620);
    let n1623: ZW = zw_add(n1619, n1621);
    let n1624: ZW = zw_cellmix_n(283u64, n1127, 1542469173u64);
    let n1625: ZW = zw_cellmix_n(283u64, n1127, 668265263u64);
    let n1626: ZW = zw_add(n1622, n1624);
    let n1627: ZW = zw_add(n1623, n1625);
    let n1628: ZW = zw_add(n1610, n1430);
    let n1629: ZW = zw_add(n1611, n1431);
    let n1630: ZW = zw_add(n1628, n1434);
    let n1631: ZW = zw_add(n1629, n1435);
    let n1632: ZW = zw_add(n1630, n1478);
    let n1633: ZW = zw_add(n1631, n1479);
    let n1634: ZW = zw_add(n1632, n1298);
    let n1635: ZW = zw_add(n1633, n1299);
    let n1636: ZW = zw_cellmix_n(282u64, n1137, 1542469173u64);
    let n1637: ZW = zw_cellmix_n(282u64, n1137, 668265263u64);
    let n1638: ZW = zw_add(n1634, n1636);
    let n1639: ZW = zw_add(n1635, n1637);
    let n1640: ZW = zw_cellmix_n(283u64, n1135, 1542469173u64);
    let n1641: ZW = zw_cellmix_n(283u64, n1135, 668265263u64);
    let n1642: ZW = zw_add(n1638, n1640);
    let n1643: ZW = zw_add(n1639, n1641);
    let n1644: ZW = zw_add(n1628, n1450);
    let n1645: ZW = zw_add(n1629, n1451);
    let n1646: ZW = zw_add(n1644, n1478);
    let n1647: ZW = zw_add(n1645, n1479);
    let n1648: ZW = zw_add(n1646, n1310);
    let n1649: ZW = zw_add(n1647, n1311);
    let n1650: ZW = zw_cellmix_n(282u64, n1145, 1542469173u64);
    let n1651: ZW = zw_cellmix_n(282u64, n1145, 668265263u64);
    let n1652: ZW = zw_add(n1648, n1650);
    let n1653: ZW = zw_add(n1649, n1651);
    let n1654: ZW = zw_cellmix_n(283u64, n1143, 1542469173u64);
    let n1655: ZW = zw_cellmix_n(283u64, n1143, 668265263u64);
    let n1656: ZW = zw_add(n1652, n1654);
    let n1657: ZW = zw_add(n1653, n1655);
    let n1658: ZW = zw_add(n1614, n1522);
    let n1659: ZW = zw_add(n1615, n1523);
    let n1660: ZW = zw_add(n1658, n1286);
    let n1661: ZW = zw_add(n1659, n1287);
    let n1662: ZW = zw_add(n1660, n1620);
    let n1663: ZW = zw_add(n1661, n1621);
    let n1664: ZW = zw_cellmix_n(283u64, n1148, 1542469173u64);
    let n1665: ZW = zw_cellmix_n(283u64, n1148, 668265263u64);
    let n1666: ZW = zw_add(n1662, n1664);
    let n1667: ZW = zw_add(n1663, n1665);
    let n1668: ZW = zw_add(n1630, n1522);
    let n1669: ZW = zw_add(n1631, n1523);
    let n1670: ZW = zw_add(n1668, n1298);
    let n1671: ZW = zw_add(n1669, n1299);
    let n1672: ZW = zw_add(n1670, n1636);
    let n1673: ZW = zw_add(n1671, n1637);
    let n1674: ZW = zw_cellmix_n(283u64, n1151, 1542469173u64);
    let n1675: ZW = zw_cellmix_n(283u64, n1151, 668265263u64);
    let n1676: ZW = zw_add(n1672, n1674);
    let n1677: ZW = zw_add(n1673, n1675);
    let n1678: ZW = zw_add(n1644, n1522);
    let n1679: ZW = zw_add(n1645, n1523);
    let n1680: ZW = zw_add(n1678, n1310);
    let n1681: ZW = zw_add(n1679, n1311);
    let n1682: ZW = zw_add(n1680, n1650);
    let n1683: ZW = zw_add(n1681, n1651);
    let n1684: ZW = zw_cellmix_n(283u64, n1154, 1542469173u64);
    let n1685: ZW = zw_cellmix_n(283u64, n1154, 668265263u64);
    let n1686: ZW = zw_add(n1682, n1684);
    let n1687: ZW = zw_add(n1683, n1685);
    let ok_v0_b0: u16 = ALL & zb_holds(n694) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v0_b0: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b0: u16 = ALL & zb_holds(n693) & zb_holds(n755);
    let ok_v32_b1: u16 = ALL & zb_holds(n694) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v32_b1: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b1: u16 = ALL & zb_holds(n693) & zb_holds(n755);
    let ok_v0_b2: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n818);
    let bd_v0_b2: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b2: u16 = ALL & zb_holds(n817);
    let ok_v32_b3: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n818);
    let bd_v32_b3: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b3: u16 = ALL & zb_holds(n817);
    let ok_v0_b4: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v0_b4: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b4: u16 = ALL & zb_holds(n924);
    let ok_v1_b5: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v1_b5: bool = !n107 || !n106 || !n105 || !n104;
    let live_v1_b5: u16 = ALL & zb_holds(n924);
    let ok_v2_b6: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v2_b6: bool = !n107 || !n106 || !n105 || !n104;
    let live_v2_b6: u16 = ALL & zb_holds(n924);
    let ok_v16_b7: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v16_b7: bool = !n107 || !n106 || !n105 || !n104;
    let live_v16_b7: u16 = ALL & zb_holds(n924);
    let ok_v17_b8: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v17_b8: bool = !n107 || !n106 || !n105 || !n104;
    let live_v17_b8: u16 = ALL & zb_holds(n924);
    let ok_v18_b9: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v18_b9: bool = !n107 || !n106 || !n105 || !n104;
    let live_v18_b9: u16 = ALL & zb_holds(n924);
    let ok_v32_b10: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v32_b10: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b10: u16 = ALL & zb_holds(n924);
    let ok_v33_b11: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v33_b11: bool = !n107 || !n106 || !n105 || !n104;
    let live_v33_b11: u16 = ALL & zb_holds(n924);
    let ok_v34_b12: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v34_b12: bool = !n107 || !n106 || !n105 || !n104;
    let live_v34_b12: u16 = ALL & zb_holds(n924);
    let ok_v36_b13: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v36_b13: bool = !n107 || !n106 || !n105 || !n104;
    let live_v36_b13: u16 = ALL & zb_holds(n924);
    let ok_v37_b14: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v37_b14: bool = !n107 || !n106 || !n105 || !n104;
    let live_v37_b14: u16 = ALL & zb_holds(n924);
    let ok_v38_b15: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v38_b15: bool = !n107 || !n106 || !n105 || !n104;
    let live_v38_b15: u16 = ALL & zb_holds(n924);
    let ok_v40_b16: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v40_b16: bool = !n107 || !n106 || !n105 || !n104;
    let live_v40_b16: u16 = ALL & zb_holds(n924);
    let ok_v41_b17: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v41_b17: bool = !n107 || !n106 || !n105 || !n104;
    let live_v41_b17: u16 = ALL & zb_holds(n924);
    let ok_v42_b18: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v42_b18: bool = !n107 || !n106 || !n105 || !n104;
    let live_v42_b18: u16 = ALL & zb_holds(n924);
    let ok_v48_b19: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v48_b19: bool = !n107 || !n106 || !n105 || !n104;
    let live_v48_b19: u16 = ALL & zb_holds(n924);
    let ok_v49_b20: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v49_b20: bool = !n107 || !n106 || !n105 || !n104;
    let live_v49_b20: u16 = ALL & zb_holds(n924);
    let ok_v50_b21: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v50_b21: bool = !n107 || !n106 || !n105 || !n104;
    let live_v50_b21: u16 = ALL & zb_holds(n924);
    let ok_v52_b22: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v52_b22: bool = !n107 || !n106 || !n105 || !n104;
    let live_v52_b22: u16 = ALL & zb_holds(n924);
    let ok_v53_b23: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v53_b23: bool = !n107 || !n106 || !n105 || !n104;
    let live_v53_b23: u16 = ALL & zb_holds(n924);
    let ok_v54_b24: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v54_b24: bool = !n107 || !n106 || !n105 || !n104;
    let live_v54_b24: u16 = ALL & zb_holds(n924);
    let ok_v56_b25: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v56_b25: bool = !n107 || !n106 || !n105 || !n104;
    let live_v56_b25: u16 = ALL & zb_holds(n924);
    let ok_v57_b26: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v57_b26: bool = !n107 || !n106 || !n105 || !n104;
    let live_v57_b26: u16 = ALL & zb_holds(n924);
    let ok_v58_b27: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n907);
    let bd_v58_b27: bool = !n107 || !n106 || !n105 || !n104;
    let live_v58_b27: u16 = ALL & zb_holds(n924);
    let sh0 = KShared0 {
        c87: n777,
        c84: n63,
        c86: n114,
        c85: n115,
    };
    let sh1 = KShared1 {
        c87: n819,
        c39: n820,
        c84: n63,
        c86: n114,
        c85: n115,
        c38: n816,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n63,
        c86: n114,
        c280: n905,
        c281: n906,
        c256: n903,
        c85: n115,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
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
    // 28 distinct button assignments; per outcome they fall
    // into [2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n1178, h2: n1179,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v32_b1 & (if bd_v32_b1 { ALL } else { !ok_v32_b1 });
    take_0_1 |= live_v32_b1 & ok_v32_b1 & (if bd_v32_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n808,
        c41: n809,
        h1: n1186, h2: n1187,
    };
    // body 1: buttons 0x20, forks 0x0
    sink.o0(32, take_0_1, &sh0, &o0);
    declined |= live_v0_b2 & (if bd_v0_b2 { ALL } else { !ok_v0_b2 });
    take_1_0 |= live_v0_b2 & ok_v0_b2 & (if bd_v0_b2 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        h1: n1206, h2: n1207,
    };
    // body 2: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b3 & (if bd_v32_b3 { ALL } else { !ok_v32_b3 });
    take_1_1 |= live_v32_b3 & ok_v32_b3 & (if bd_v32_b3 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n808,
        h1: n1208, h2: n1209,
    };
    // body 3: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b4 & (if bd_v0_b4 { ALL } else { !ok_v0_b4 });
    take_2_0 |= live_v0_b4 & ok_v0_b4 & (if bd_v0_b4 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n904,
        c241: n899,
        c248: n900,
        c249: n901,
        c282: n928,
        c283: n923,
        c255: n927,
        h1: n1296, h2: n1297,
    };
    // body 4: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b5 & (if bd_v1_b5 { ALL } else { !ok_v1_b5 });
    take_2_1 |= live_v1_b5 & ok_v1_b5 & (if bd_v1_b5 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n939,
        c241: n899,
        c248: n900,
        c249: n901,
        c282: n945,
        c283: n943,
        c255: n927,
        h1: n1308, h2: n1309,
    };
    // body 5: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b6 & (if bd_v2_b6 { ALL } else { !ok_v2_b6 });
    take_2_2 |= live_v2_b6 & ok_v2_b6 & (if bd_v2_b6 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n955,
        c241: n899,
        c248: n900,
        c249: n901,
        c282: n961,
        c283: n959,
        c255: n927,
        h1: n1320, h2: n1321,
    };
    // body 6: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b7 & (if bd_v16_b7 { ALL } else { !ok_v16_b7 });
    take_2_3 |= live_v16_b7 & ok_v16_b7 & (if bd_v16_b7 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n904,
        c241: n966,
        c248: n900,
        c249: n967,
        c282: n973,
        c283: n971,
        c255: n927,
        h1: n1350, h2: n1351,
    };
    // body 7: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b8 & (if bd_v17_b8 { ALL } else { !ok_v17_b8 });
    take_2_4 |= live_v17_b8 & ok_v17_b8 & (if bd_v17_b8 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n939,
        c241: n966,
        c248: n900,
        c249: n967,
        c282: n981,
        c283: n979,
        c255: n927,
        h1: n1360, h2: n1361,
    };
    // body 8: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b9 & (if bd_v18_b9 { ALL } else { !ok_v18_b9 });
    take_2_5 |= live_v18_b9 & ok_v18_b9 & (if bd_v18_b9 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n918,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n919,
        c272: r_c272,
        c273: r_c273,
        c238: n920,
        c239: n921,
        c274: n955,
        c241: n966,
        c248: n900,
        c249: n967,
        c282: n989,
        c283: n987,
        c255: n927,
        h1: n1370, h2: n1371,
    };
    // body 9: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b10 & (if bd_v32_b10 { ALL } else { !ok_v32_b10 });
    take_2_6 |= live_v32_b10 & ok_v32_b10 & (if bd_v32_b10 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1015,
        c236: n1011,
        c272: n1016,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1023,
        c283: n1019,
        c255: n1022,
        h1: n1428, h2: n1429,
    };
    // body 10: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b11 & (if bd_v33_b11 { ALL } else { !ok_v33_b11 });
    take_2_7 |= live_v33_b11 & ok_v33_b11 & (if bd_v33_b11 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1037,
        c283: n1035,
        c255: n1022,
        h1: n1448, h2: n1449,
    };
    // body 11: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b12 & (if bd_v34_b12 { ALL } else { !ok_v34_b12 });
    take_2_8 |= live_v34_b12 & ok_v34_b12 & (if bd_v34_b12 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1048,
        c283: n1046,
        c255: n1022,
        h1: n1464, h2: n1465,
    };
    // body 12: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b13 & (if bd_v36_b13 { ALL } else { !ok_v36_b13 });
    take_2_9 |= live_v36_b13 & ok_v36_b13 & (if bd_v36_b13 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1063,
        c236: n1011,
        c272: n1064,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1069,
        c283: n1067,
        c255: n1022,
        h1: n1490, h2: n1491,
    };
    // body 13: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b14 & (if bd_v37_b14 { ALL } else { !ok_v37_b14 });
    take_2_10 |= live_v37_b14 & ok_v37_b14 & (if bd_v37_b14 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1077,
        c283: n1075,
        c255: n1022,
        h1: n1506, h2: n1507,
    };
    // body 14: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b15 & (if bd_v38_b15 { ALL } else { !ok_v38_b15 });
    take_2_11 |= live_v38_b15 & ok_v38_b15 & (if bd_v38_b15 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1085,
        c283: n1083,
        c255: n1022,
        h1: n1520, h2: n1521,
    };
    // body 15: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b16 & (if bd_v40_b16 { ALL } else { !ok_v40_b16 });
    take_2_12 |= live_v40_b16 & ok_v40_b16 & (if bd_v40_b16 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1063,
        c236: n1011,
        c272: n1064,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1069,
        c283: n1091,
        c255: n1022,
        h1: n1532, h2: n1533,
    };
    // body 16: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b17 & (if bd_v41_b17 { ALL } else { !ok_v41_b17 });
    take_2_13 |= live_v41_b17 & ok_v41_b17 & (if bd_v41_b17 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1077,
        c283: n1094,
        c255: n1022,
        h1: n1542, h2: n1543,
    };
    // body 17: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b18 & (if bd_v42_b18 { ALL } else { !ok_v42_b18 });
    take_2_14 |= live_v42_b18 & ok_v42_b18 & (if bd_v42_b18 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n899,
        c248: n990,
        c249: n901,
        c282: n1085,
        c283: n1097,
        c255: n1022,
        h1: n1552, h2: n1553,
    };
    // body 18: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b19 & (if bd_v48_b19 { ALL } else { !ok_v48_b19 });
    take_2_15 |= live_v48_b19 & ok_v48_b19 & (if bd_v48_b19 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1015,
        c236: n1011,
        c272: n1016,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1105,
        c283: n1103,
        c255: n1022,
        h1: n1578, h2: n1579,
    };
    // body 19: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b20 & (if bd_v49_b20 { ALL } else { !ok_v49_b20 });
    take_2_16 |= live_v49_b20 & ok_v49_b20 & (if bd_v49_b20 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1113,
        c283: n1111,
        c255: n1022,
        h1: n1594, h2: n1595,
    };
    // body 20: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b21 & (if bd_v50_b21 { ALL } else { !ok_v50_b21 });
    take_2_17 |= live_v50_b21 & ok_v50_b21 & (if bd_v50_b21 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1014,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1017,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1121,
        c283: n1119,
        c255: n1022,
        h1: n1608, h2: n1609,
    };
    // body 21: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b22 & (if bd_v52_b22 { ALL } else { !ok_v52_b22 });
    take_2_18 |= live_v52_b22 & ok_v52_b22 & (if bd_v52_b22 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1063,
        c236: n1011,
        c272: n1064,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1129,
        c283: n1127,
        c255: n1022,
        h1: n1626, h2: n1627,
    };
    // body 22: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b23 & (if bd_v53_b23 { ALL } else { !ok_v53_b23 });
    take_2_19 |= live_v53_b23 & ok_v53_b23 & (if bd_v53_b23 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1137,
        c283: n1135,
        c255: n1022,
        h1: n1642, h2: n1643,
    };
    // body 23: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b24 & (if bd_v54_b24 { ALL } else { !ok_v54_b24 });
    take_2_20 |= live_v54_b24 & ok_v54_b24 & (if bd_v54_b24 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1065,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1145,
        c283: n1143,
        c255: n1022,
        h1: n1656, h2: n1657,
    };
    // body 24: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b25 & (if bd_v56_b25 { ALL } else { !ok_v56_b25 });
    take_2_21 |= live_v56_b25 & ok_v56_b25 & (if bd_v56_b25 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1063,
        c236: n1011,
        c272: n1064,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n904,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1129,
        c283: n1148,
        c255: n1022,
        h1: n1666, h2: n1667,
    };
    // body 25: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b26 & (if bd_v57_b26 { ALL } else { !ok_v57_b26 });
    take_2_22 |= live_v57_b26 & ok_v57_b26 & (if bd_v57_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1033,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n939,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1137,
        c283: n1151,
        c255: n1022,
        h1: n1676, h2: n1677,
    };
    // body 26: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b27 & (if bd_v58_b27 { ALL } else { !ok_v58_b27 });
    take_2_23 |= live_v58_b27 & ok_v58_b27 & (if bd_v58_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1009,
        c41: n1010,
        c270: n1062,
        c271: n1032,
        c236: n1011,
        c272: n1044,
        c273: n1090,
        c238: n1012,
        c239: n1013,
        c274: n955,
        c241: n966,
        c248: n990,
        c249: n967,
        c282: n1145,
        c283: n1154,
        c255: n1022,
        h1: n1686, h2: n1687,
    };
    // body 27: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
