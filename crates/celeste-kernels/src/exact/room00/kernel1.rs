// GENERATED from a TRACED frame (shape 1). Do not edit.
//
// One input shape, 3 output shapes, 52 distinct button
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
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n314, n319);
    let n322: ZB = zb_and(n314, n320);
    let n323: ZN = zn_rem(n308, zn_splat(P8::from_raw(524288i32)));
    let n324: ZB = zn_ge(n323, zn_splat(P8::from_raw(393216i32)));
    let n325: ZB = zn_lt(n323, zn_splat(P8::from_raw(393216i32)));
    let n326: ZB = zb_and(n321, n325);
    let n327: ZB = zb_and(n321, n324);
    let n328: ZN = zn_mul(n311, zn_splat(P8::from_raw(524288i32)));
    let n329: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n328);
    let n330: ZB = zn_eq(n307, n329);
    let n331: ZB = zb_or(n326, n327);
    let n332: ZB = zb_or(n324, n330);
    let n333: ZB = zb_or(n322, n331);
    let n334: ZB = zb_and(n319, n332);
    let n335: ZB = zb_not(n334);
    let n336: ZB = zb_and(n333, n334);
    let n337: ZB = zb_and(n333, n335);
    let n338: ZB = zn_ge(n288, zn_splat(P8::from_raw(0i32)));
    let n339: ZB = zb_or(n336, n337);
    let n340: ZB = zb_and(n334, n338);
    let n341: ZB = zb_not(n340);
    let n342: ZB = zb_and(n339, n340);
    let n343: ZB = zb_and(n339, n341);
    let n344: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n318);
    let n345: ZB = zb_not(n344);
    let n346: ZB = zb_and(n343, n344);
    let n347: ZB = zb_and(n343, n345);
    let n348: ZN = zn_rem(n291, zn_splat(P8::from_raw(524288i32)));
    let n349: ZB = zn_le(n348, zn_splat(P8::from_raw(131072i32)));
    let n350: ZB = zb_or(n346, n347);
    let n351: ZB = zb_and(n344, n349);
    let n352: ZB = zb_not(n351);
    let n353: ZB = zb_and(n350, n351);
    let n354: ZB = zb_and(n350, n352);
    let n355: ZB = zn_le(n288, zn_splat(P8::from_raw(0i32)));
    let n356: ZB = zb_or(n353, n354);
    let n357: ZB = zb_and(n351, n355);
    let n358: ZB = zb_not(n357);
    let n359: ZB = zb_and(n356, n357);
    let n360: ZB = zb_and(n356, n358);
    let n361: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n318);
    let n362: ZB = zb_not(n361);
    let n363: ZB = zb_and(n360, n361);
    let n364: ZB = zb_and(n360, n362);
    let n365: ZN = zn_rem(n290, zn_splat(P8::from_raw(524288i32)));
    let n366: ZB = zn_le(n365, zn_splat(P8::from_raw(131072i32)));
    let n367: ZB = zb_or(n363, n364);
    let n368: ZB = zb_and(n361, n366);
    let n369: ZB = zb_not(n368);
    let n370: ZB = zb_and(n367, n368);
    let n371: ZB = zb_and(n367, n369);
    let n372: ZB = zn_le(n287, zn_splat(P8::from_raw(0i32)));
    let n373: ZB = zb_or(n370, n371);
    let n374: ZB = zb_and(n368, n372);
    let n375: ZB = zb_not(n374);
    let n376: ZB = zb_and(n373, n374);
    let n377: ZB = zb_and(n373, n375);
    let n378: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n318);
    let n379: ZB = zb_not(n378);
    let n380: ZB = zb_and(n377, n378);
    let n381: ZB = zb_and(n377, n379);
    let n382: ZN = zn_rem(n296, zn_splat(P8::from_raw(524288i32)));
    let n383: ZB = zn_ge(n382, zn_splat(P8::from_raw(393216i32)));
    let n384: ZB = zn_lt(n382, zn_splat(P8::from_raw(393216i32)));
    let n385: ZB = zb_and(n380, n384);
    let n386: ZB = zb_and(n380, n383);
    let n387: ZN = zn_mul(n299, zn_splat(P8::from_raw(524288i32)));
    let n388: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n387);
    let n389: ZB = zn_eq(n295, n388);
    let n390: ZB = zb_or(n385, n386);
    let n391: ZB = zb_or(n383, n389);
    let n392: ZB = zb_or(n381, n390);
    let n393: ZB = zb_and(n378, n391);
    let n394: ZB = zb_not(n393);
    let n395: ZB = zb_and(n392, n393);
    let n396: ZB = zb_and(n392, n394);
    let n397: ZB = zn_ge(n287, zn_splat(P8::from_raw(0i32)));
    let n398: ZB = zb_or(n395, n396);
    let n399: ZB = zb_and(n393, n397);
    let n400: ZB = zb_not(n399);
    let n401: ZB = zb_and(n398, n399);
    let n402: ZB = zb_and(n398, n400);
    let n403: ZB = zb_or(n376, n401);
    let n404: ZB = zb_or(n359, n403);
    let n405: ZB = zb_or(n342, n404);
    let n406: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n306);
    let n407: ZB = zn_le(n406, n310);
    let n408: ZB = zn_gt(n406, n310);
    let n409: ZB = zb_and(n402, n407);
    let n410: ZB = zb_and(n402, n408);
    let n411: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n406);
    let n412: ZN = zn_mget(g.cart, n316, n411);
    let n413: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n412);
    let n414: ZB = zb_not(n413);
    let n415: ZB = zb_and(n409, n413);
    let n416: ZB = zb_and(n409, n414);
    let n417: ZB = zb_and(n325, n415);
    let n418: ZB = zb_and(n324, n415);
    let n419: ZN = zn_mul(n406, zn_splat(P8::from_raw(524288i32)));
    let n420: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n419);
    let n421: ZB = zn_eq(n307, n420);
    let n422: ZB = zb_or(n417, n418);
    let n423: ZB = zb_or(n324, n421);
    let n424: ZB = zb_or(n416, n422);
    let n425: ZB = zb_and(n413, n423);
    let n426: ZB = zb_not(n425);
    let n427: ZB = zb_and(n424, n425);
    let n428: ZB = zb_and(n424, n426);
    let n429: ZB = zb_or(n427, n428);
    let n430: ZB = zb_and(n338, n425);
    let n431: ZB = zb_not(n430);
    let n432: ZB = zb_and(n429, n430);
    let n433: ZB = zb_and(n429, n431);
    let n434: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n412);
    let n435: ZB = zb_not(n434);
    let n436: ZB = zb_and(n433, n434);
    let n437: ZB = zb_and(n433, n435);
    let n438: ZB = zb_or(n436, n437);
    let n439: ZB = zb_and(n349, n434);
    let n440: ZB = zb_not(n439);
    let n441: ZB = zb_and(n438, n439);
    let n442: ZB = zb_and(n438, n440);
    let n443: ZB = zb_or(n441, n442);
    let n444: ZB = zb_and(n355, n439);
    let n445: ZB = zb_not(n444);
    let n446: ZB = zb_and(n443, n444);
    let n447: ZB = zb_and(n443, n445);
    let n448: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n412);
    let n449: ZB = zb_not(n448);
    let n450: ZB = zb_and(n447, n448);
    let n451: ZB = zb_and(n447, n449);
    let n452: ZB = zb_or(n450, n451);
    let n453: ZB = zb_and(n366, n448);
    let n454: ZB = zb_not(n453);
    let n455: ZB = zb_and(n452, n453);
    let n456: ZB = zb_and(n452, n454);
    let n457: ZB = zb_or(n455, n456);
    let n458: ZB = zb_and(n372, n453);
    let n459: ZB = zb_not(n458);
    let n460: ZB = zb_and(n457, n458);
    let n461: ZB = zb_and(n457, n459);
    let n462: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n412);
    let n463: ZB = zb_not(n462);
    let n464: ZB = zb_and(n461, n462);
    let n465: ZB = zb_and(n461, n463);
    let n466: ZB = zb_and(n384, n464);
    let n467: ZB = zb_and(n383, n464);
    let n468: ZB = zb_or(n466, n467);
    let n469: ZB = zb_or(n465, n468);
    let n470: ZB = zb_and(n391, n462);
    let n471: ZB = zb_not(n470);
    let n472: ZB = zb_and(n469, n470);
    let n473: ZB = zb_and(n469, n471);
    let n474: ZB = zb_or(n472, n473);
    let n475: ZB = zb_and(n397, n470);
    let n476: ZB = zb_not(n475);
    let n477: ZB = zb_and(n474, n475);
    let n478: ZB = zb_and(n474, n476);
    let n479: ZB = zb_or(n460, n477);
    let n480: ZB = zb_or(n446, n479);
    let n481: ZB = zb_or(n432, n480);
    let n482: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n306);
    let n483: ZB = zn_le(n482, n310);
    let n484: ZB = zn_gt(n482, n310);
    let n485: ZB = zb_and(n478, n483);
    let n486: ZB = zb_and(n478, n484);
    let n487: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n482);
    let n488: ZN = zn_mget(g.cart, n316, n487);
    let n489: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n488);
    let n490: ZB = zb_not(n489);
    let n491: ZB = zb_and(n485, n489);
    let n492: ZB = zb_and(n485, n490);
    let n493: ZB = zb_and(n325, n491);
    let n494: ZB = zb_and(n324, n491);
    let n495: ZN = zn_mul(n482, zn_splat(P8::from_raw(524288i32)));
    let n496: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n495);
    let n497: ZB = zn_eq(n307, n496);
    let n498: ZB = zb_or(n493, n494);
    let n499: ZB = zb_or(n324, n497);
    let n500: ZB = zb_or(n492, n498);
    let n501: ZB = zb_and(n489, n499);
    let n502: ZB = zb_not(n501);
    let n503: ZB = zb_and(n500, n501);
    let n504: ZB = zb_and(n500, n502);
    let n505: ZB = zb_or(n503, n504);
    let n506: ZB = zb_and(n338, n501);
    let n507: ZB = zb_not(n506);
    let n508: ZB = zb_and(n505, n506);
    let n509: ZB = zb_and(n505, n507);
    let n510: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n488);
    let n511: ZB = zb_not(n510);
    let n512: ZB = zb_and(n509, n510);
    let n513: ZB = zb_and(n509, n511);
    let n514: ZB = zb_or(n512, n513);
    let n515: ZB = zb_and(n349, n510);
    let n516: ZB = zb_not(n515);
    let n517: ZB = zb_and(n514, n515);
    let n518: ZB = zb_and(n514, n516);
    let n519: ZB = zb_or(n517, n518);
    let n520: ZB = zb_and(n355, n515);
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n519, n520);
    let n523: ZB = zb_and(n519, n521);
    let n524: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n488);
    let n525: ZB = zb_not(n524);
    let n526: ZB = zb_and(n523, n524);
    let n527: ZB = zb_and(n523, n525);
    let n528: ZB = zb_or(n526, n527);
    let n529: ZB = zb_and(n366, n524);
    let n530: ZB = zb_not(n529);
    let n531: ZB = zb_and(n528, n529);
    let n532: ZB = zb_and(n528, n530);
    let n533: ZB = zb_or(n531, n532);
    let n534: ZB = zb_and(n372, n529);
    let n535: ZB = zb_not(n534);
    let n536: ZB = zb_and(n533, n534);
    let n537: ZB = zb_and(n533, n535);
    let n538: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n488);
    let n539: ZB = zb_not(n538);
    let n540: ZB = zb_and(n537, n538);
    let n541: ZB = zb_and(n537, n539);
    let n542: ZB = zb_and(n384, n540);
    let n543: ZB = zb_and(n383, n540);
    let n544: ZB = zb_or(n542, n543);
    let n545: ZB = zb_or(n541, n544);
    let n546: ZB = zb_and(n391, n538);
    let n547: ZB = zb_not(n546);
    let n548: ZB = zb_and(n545, n546);
    let n549: ZB = zb_and(n545, n547);
    let n550: ZB = zb_or(n548, n549);
    let n551: ZB = zb_and(n397, n546);
    let n552: ZB = zb_not(n551);
    let n553: ZB = zb_and(n550, n551);
    let n554: ZB = zb_and(n550, n552);
    let n555: ZB = zb_or(n536, n553);
    let n556: ZB = zb_or(n522, n555);
    let n557: ZB = zb_or(n508, n556);
    let n558: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n306);
    let n559: ZB = zn_gt(n558, n310);
    let n560: ZB = zb_and(n289, n559);
    let n561: ZB = zb_or(n486, n554);
    let n562: ZB = zsel_b(n484, n289, n560);
    let n563: ZB = zb_or(n481, n557);
    let n564: ZB = zb_or(n410, n561);
    let n565: ZB = zsel_b(n408, n289, n562);
    let n566: ZB = zb_or(n405, n563);
    let n567: ZB = zb_or(n315, n564);
    let n568: ZB = zsel_b(n313, n289, n565);
    let n569: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n294);
    let n570: ZB = zn_le(n569, n298);
    let n571: ZB = zn_gt(n569, n298);
    let n572: ZB = zb_and(n567, n570);
    let n573: ZB = zb_and(n567, n571);
    let n574: ZB = zb_and(n312, n572);
    let n575: ZB = zb_and(n313, n572);
    let n576: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n569);
    let n577: ZN = zn_mget(g.cart, n576, n317);
    let n578: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n577);
    let n579: ZB = zb_not(n578);
    let n580: ZB = zb_and(n574, n578);
    let n581: ZB = zb_and(n574, n579);
    let n582: ZB = zb_and(n325, n580);
    let n583: ZB = zb_and(n324, n580);
    let n584: ZB = zb_or(n582, n583);
    let n585: ZB = zb_or(n581, n584);
    let n586: ZB = zb_and(n332, n578);
    let n587: ZB = zb_not(n586);
    let n588: ZB = zb_and(n585, n586);
    let n589: ZB = zb_and(n585, n587);
    let n590: ZB = zb_or(n588, n589);
    let n591: ZB = zb_and(n338, n586);
    let n592: ZB = zb_not(n591);
    let n593: ZB = zb_and(n590, n591);
    let n594: ZB = zb_and(n590, n592);
    let n595: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n577);
    let n596: ZB = zb_not(n595);
    let n597: ZB = zb_and(n594, n595);
    let n598: ZB = zb_and(n594, n596);
    let n599: ZB = zb_or(n597, n598);
    let n600: ZB = zb_and(n349, n595);
    let n601: ZB = zb_not(n600);
    let n602: ZB = zb_and(n599, n600);
    let n603: ZB = zb_and(n599, n601);
    let n604: ZB = zb_or(n602, n603);
    let n605: ZB = zb_and(n355, n600);
    let n606: ZB = zb_not(n605);
    let n607: ZB = zb_and(n604, n605);
    let n608: ZB = zb_and(n604, n606);
    let n609: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n577);
    let n610: ZB = zb_not(n609);
    let n611: ZB = zb_and(n608, n609);
    let n612: ZB = zb_and(n608, n610);
    let n613: ZB = zb_or(n611, n612);
    let n614: ZB = zb_and(n366, n609);
    let n615: ZB = zb_not(n614);
    let n616: ZB = zb_and(n613, n614);
    let n617: ZB = zb_and(n613, n615);
    let n618: ZB = zb_or(n616, n617);
    let n619: ZB = zb_and(n372, n614);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n618, n619);
    let n622: ZB = zb_and(n618, n620);
    let n623: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n577);
    let n624: ZB = zb_not(n623);
    let n625: ZB = zb_and(n622, n623);
    let n626: ZB = zb_and(n622, n624);
    let n627: ZB = zb_and(n384, n625);
    let n628: ZB = zb_and(n383, n625);
    let n629: ZN = zn_mul(n569, zn_splat(P8::from_raw(524288i32)));
    let n630: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n629);
    let n631: ZB = zn_eq(n295, n630);
    let n632: ZB = zb_or(n627, n628);
    let n633: ZB = zb_or(n383, n631);
    let n634: ZB = zb_or(n626, n632);
    let n635: ZB = zb_and(n623, n633);
    let n636: ZB = zb_not(n635);
    let n637: ZB = zb_and(n634, n635);
    let n638: ZB = zb_and(n634, n636);
    let n639: ZB = zb_or(n637, n638);
    let n640: ZB = zb_and(n397, n635);
    let n641: ZB = zb_not(n640);
    let n642: ZB = zb_and(n639, n640);
    let n643: ZB = zb_and(n639, n641);
    let n644: ZB = zb_or(n621, n642);
    let n645: ZB = zb_or(n607, n644);
    let n646: ZB = zb_or(n593, n645);
    let n647: ZB = zb_and(n407, n643);
    let n648: ZB = zb_and(n408, n643);
    let n649: ZN = zn_mget(g.cart, n576, n411);
    let n650: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n649);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zb_and(n647, n650);
    let n653: ZB = zb_and(n647, n651);
    let n654: ZB = zb_and(n325, n652);
    let n655: ZB = zb_and(n324, n652);
    let n656: ZB = zb_or(n654, n655);
    let n657: ZB = zb_or(n653, n656);
    let n658: ZB = zb_and(n423, n650);
    let n659: ZB = zb_not(n658);
    let n660: ZB = zb_and(n657, n658);
    let n661: ZB = zb_and(n657, n659);
    let n662: ZB = zb_or(n660, n661);
    let n663: ZB = zb_and(n338, n658);
    let n664: ZB = zb_not(n663);
    let n665: ZB = zb_and(n662, n663);
    let n666: ZB = zb_and(n662, n664);
    let n667: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n649);
    let n668: ZB = zb_not(n667);
    let n669: ZB = zb_and(n666, n667);
    let n670: ZB = zb_and(n666, n668);
    let n671: ZB = zb_or(n669, n670);
    let n672: ZB = zb_and(n349, n667);
    let n673: ZB = zb_not(n672);
    let n674: ZB = zb_and(n671, n672);
    let n675: ZB = zb_and(n671, n673);
    let n676: ZB = zb_or(n674, n675);
    let n677: ZB = zb_and(n355, n672);
    let n678: ZB = zb_not(n677);
    let n679: ZB = zb_and(n676, n677);
    let n680: ZB = zb_and(n676, n678);
    let n681: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n649);
    let n682: ZB = zb_not(n681);
    let n683: ZB = zb_and(n680, n681);
    let n684: ZB = zb_and(n680, n682);
    let n685: ZB = zb_or(n683, n684);
    let n686: ZB = zb_and(n366, n681);
    let n687: ZB = zb_not(n686);
    let n688: ZB = zb_and(n685, n686);
    let n689: ZB = zb_and(n685, n687);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZB = zb_and(n372, n686);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n690, n691);
    let n694: ZB = zb_and(n690, n692);
    let n695: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n649);
    let n696: ZB = zb_not(n695);
    let n697: ZB = zb_and(n694, n695);
    let n698: ZB = zb_and(n694, n696);
    let n699: ZB = zb_and(n384, n697);
    let n700: ZB = zb_and(n383, n697);
    let n701: ZB = zb_or(n699, n700);
    let n702: ZB = zb_or(n698, n701);
    let n703: ZB = zb_and(n633, n695);
    let n704: ZB = zb_not(n703);
    let n705: ZB = zb_and(n702, n703);
    let n706: ZB = zb_and(n702, n704);
    let n707: ZB = zb_or(n705, n706);
    let n708: ZB = zb_and(n397, n703);
    let n709: ZB = zb_not(n708);
    let n710: ZB = zb_and(n707, n708);
    let n711: ZB = zb_and(n707, n709);
    let n712: ZB = zb_or(n693, n710);
    let n713: ZB = zb_or(n679, n712);
    let n714: ZB = zb_or(n665, n713);
    let n715: ZB = zb_and(n483, n711);
    let n716: ZB = zb_and(n484, n711);
    let n717: ZN = zn_mget(g.cart, n576, n487);
    let n718: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n717);
    let n719: ZB = zb_not(n718);
    let n720: ZB = zb_and(n715, n718);
    let n721: ZB = zb_and(n715, n719);
    let n722: ZB = zb_and(n325, n720);
    let n723: ZB = zb_and(n324, n720);
    let n724: ZB = zb_or(n722, n723);
    let n725: ZB = zb_or(n721, n724);
    let n726: ZB = zb_and(n499, n718);
    let n727: ZB = zb_not(n726);
    let n728: ZB = zb_and(n725, n726);
    let n729: ZB = zb_and(n725, n727);
    let n730: ZB = zb_or(n728, n729);
    let n731: ZB = zb_and(n338, n726);
    let n732: ZB = zb_not(n731);
    let n733: ZB = zb_and(n730, n731);
    let n734: ZB = zb_and(n730, n732);
    let n735: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n717);
    let n736: ZB = zb_not(n735);
    let n737: ZB = zb_and(n734, n735);
    let n738: ZB = zb_and(n734, n736);
    let n739: ZB = zb_or(n737, n738);
    let n740: ZB = zb_and(n349, n735);
    let n741: ZB = zb_not(n740);
    let n742: ZB = zb_and(n739, n740);
    let n743: ZB = zb_and(n739, n741);
    let n744: ZB = zb_or(n742, n743);
    let n745: ZB = zb_and(n355, n740);
    let n746: ZB = zb_not(n745);
    let n747: ZB = zb_and(n744, n745);
    let n748: ZB = zb_and(n744, n746);
    let n749: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n717);
    let n750: ZB = zb_not(n749);
    let n751: ZB = zb_and(n748, n749);
    let n752: ZB = zb_and(n748, n750);
    let n753: ZB = zb_or(n751, n752);
    let n754: ZB = zb_and(n366, n749);
    let n755: ZB = zb_not(n754);
    let n756: ZB = zb_and(n753, n754);
    let n757: ZB = zb_and(n753, n755);
    let n758: ZB = zb_or(n756, n757);
    let n759: ZB = zb_and(n372, n754);
    let n760: ZB = zb_not(n759);
    let n761: ZB = zb_and(n758, n759);
    let n762: ZB = zb_and(n758, n760);
    let n763: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n717);
    let n764: ZB = zb_not(n763);
    let n765: ZB = zb_and(n762, n763);
    let n766: ZB = zb_and(n762, n764);
    let n767: ZB = zb_and(n384, n765);
    let n768: ZB = zb_and(n383, n765);
    let n769: ZB = zb_or(n767, n768);
    let n770: ZB = zb_or(n766, n769);
    let n771: ZB = zb_and(n633, n763);
    let n772: ZB = zb_not(n771);
    let n773: ZB = zb_and(n770, n771);
    let n774: ZB = zb_and(n770, n772);
    let n775: ZB = zb_or(n773, n774);
    let n776: ZB = zb_and(n397, n771);
    let n777: ZB = zb_not(n776);
    let n778: ZB = zb_and(n775, n776);
    let n779: ZB = zb_and(n775, n777);
    let n780: ZB = zb_or(n761, n778);
    let n781: ZB = zb_or(n747, n780);
    let n782: ZB = zb_or(n733, n781);
    let n783: ZB = zb_and(n559, n568);
    let n784: ZB = zb_or(n716, n779);
    let n785: ZB = zsel_b(n484, n568, n783);
    let n786: ZB = zb_or(n714, n782);
    let n787: ZB = zb_or(n648, n784);
    let n788: ZB = zsel_b(n408, n568, n785);
    let n789: ZB = zb_or(n646, n786);
    let n790: ZB = zb_or(n575, n787);
    let n791: ZB = zsel_b(n313, n568, n788);
    let n792: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n294);
    let n793: ZB = zn_le(n792, n298);
    let n794: ZB = zn_gt(n792, n298);
    let n795: ZB = zb_and(n790, n793);
    let n796: ZB = zb_and(n790, n794);
    let n797: ZB = zb_and(n312, n795);
    let n798: ZB = zb_and(n313, n795);
    let n799: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n792);
    let n800: ZN = zn_mget(g.cart, n799, n317);
    let n801: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n800);
    let n802: ZB = zb_not(n801);
    let n803: ZB = zb_and(n797, n801);
    let n804: ZB = zb_and(n797, n802);
    let n805: ZB = zb_and(n325, n803);
    let n806: ZB = zb_and(n324, n803);
    let n807: ZB = zb_or(n805, n806);
    let n808: ZB = zb_or(n804, n807);
    let n809: ZB = zb_and(n332, n801);
    let n810: ZB = zb_not(n809);
    let n811: ZB = zb_and(n808, n809);
    let n812: ZB = zb_and(n808, n810);
    let n813: ZB = zb_or(n811, n812);
    let n814: ZB = zb_and(n338, n809);
    let n815: ZB = zb_not(n814);
    let n816: ZB = zb_and(n813, n814);
    let n817: ZB = zb_and(n813, n815);
    let n818: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n800);
    let n819: ZB = zb_not(n818);
    let n820: ZB = zb_and(n817, n818);
    let n821: ZB = zb_and(n817, n819);
    let n822: ZB = zb_or(n820, n821);
    let n823: ZB = zb_and(n349, n818);
    let n824: ZB = zb_not(n823);
    let n825: ZB = zb_and(n822, n823);
    let n826: ZB = zb_and(n822, n824);
    let n827: ZB = zb_or(n825, n826);
    let n828: ZB = zb_and(n355, n823);
    let n829: ZB = zb_not(n828);
    let n830: ZB = zb_and(n827, n828);
    let n831: ZB = zb_and(n827, n829);
    let n832: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n800);
    let n833: ZB = zb_not(n832);
    let n834: ZB = zb_and(n831, n832);
    let n835: ZB = zb_and(n831, n833);
    let n836: ZB = zb_or(n834, n835);
    let n837: ZB = zb_and(n366, n832);
    let n838: ZB = zb_not(n837);
    let n839: ZB = zb_and(n836, n837);
    let n840: ZB = zb_and(n836, n838);
    let n841: ZB = zb_or(n839, n840);
    let n842: ZB = zb_and(n372, n837);
    let n843: ZB = zb_not(n842);
    let n844: ZB = zb_and(n841, n842);
    let n845: ZB = zb_and(n841, n843);
    let n846: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n800);
    let n847: ZB = zb_not(n846);
    let n848: ZB = zb_and(n845, n846);
    let n849: ZB = zb_and(n845, n847);
    let n850: ZB = zb_and(n384, n848);
    let n851: ZB = zb_and(n383, n848);
    let n852: ZN = zn_mul(n792, zn_splat(P8::from_raw(524288i32)));
    let n853: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n852);
    let n854: ZB = zn_eq(n295, n853);
    let n855: ZB = zb_or(n850, n851);
    let n856: ZB = zb_or(n383, n854);
    let n857: ZB = zb_or(n849, n855);
    let n858: ZB = zb_and(n846, n856);
    let n859: ZB = zb_not(n858);
    let n860: ZB = zb_and(n857, n858);
    let n861: ZB = zb_and(n857, n859);
    let n862: ZB = zb_or(n860, n861);
    let n863: ZB = zb_and(n397, n858);
    let n864: ZB = zb_not(n863);
    let n865: ZB = zb_and(n862, n863);
    let n866: ZB = zb_and(n862, n864);
    let n867: ZB = zb_or(n844, n865);
    let n868: ZB = zb_or(n830, n867);
    let n869: ZB = zb_or(n816, n868);
    let n870: ZB = zb_and(n407, n866);
    let n871: ZB = zb_and(n408, n866);
    let n872: ZN = zn_mget(g.cart, n799, n411);
    let n873: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n872);
    let n874: ZB = zb_not(n873);
    let n875: ZB = zb_and(n870, n873);
    let n876: ZB = zb_and(n870, n874);
    let n877: ZB = zb_and(n325, n875);
    let n878: ZB = zb_and(n324, n875);
    let n879: ZB = zb_or(n877, n878);
    let n880: ZB = zb_or(n876, n879);
    let n881: ZB = zb_and(n423, n873);
    let n882: ZB = zb_not(n881);
    let n883: ZB = zb_and(n880, n881);
    let n884: ZB = zb_and(n880, n882);
    let n885: ZB = zb_or(n883, n884);
    let n886: ZB = zb_and(n338, n881);
    let n887: ZB = zb_not(n886);
    let n888: ZB = zb_and(n885, n886);
    let n889: ZB = zb_and(n885, n887);
    let n890: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n872);
    let n891: ZB = zb_not(n890);
    let n892: ZB = zb_and(n889, n890);
    let n893: ZB = zb_and(n889, n891);
    let n894: ZB = zb_or(n892, n893);
    let n895: ZB = zb_and(n349, n890);
    let n896: ZB = zb_not(n895);
    let n897: ZB = zb_and(n894, n895);
    let n898: ZB = zb_and(n894, n896);
    let n899: ZB = zb_or(n897, n898);
    let n900: ZB = zb_and(n355, n895);
    let n901: ZB = zb_not(n900);
    let n902: ZB = zb_and(n899, n900);
    let n903: ZB = zb_and(n899, n901);
    let n904: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n872);
    let n905: ZB = zb_not(n904);
    let n906: ZB = zb_and(n903, n904);
    let n907: ZB = zb_and(n903, n905);
    let n908: ZB = zb_or(n906, n907);
    let n909: ZB = zb_and(n366, n904);
    let n910: ZB = zb_not(n909);
    let n911: ZB = zb_and(n908, n909);
    let n912: ZB = zb_and(n908, n910);
    let n913: ZB = zb_or(n911, n912);
    let n914: ZB = zb_and(n372, n909);
    let n915: ZB = zb_not(n914);
    let n916: ZB = zb_and(n913, n914);
    let n917: ZB = zb_and(n913, n915);
    let n918: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n872);
    let n919: ZB = zb_not(n918);
    let n920: ZB = zb_and(n917, n918);
    let n921: ZB = zb_and(n917, n919);
    let n922: ZB = zb_and(n384, n920);
    let n923: ZB = zb_and(n383, n920);
    let n924: ZB = zb_or(n922, n923);
    let n925: ZB = zb_or(n921, n924);
    let n926: ZB = zb_and(n856, n918);
    let n927: ZB = zb_not(n926);
    let n928: ZB = zb_and(n925, n926);
    let n929: ZB = zb_and(n925, n927);
    let n930: ZB = zb_or(n928, n929);
    let n931: ZB = zb_and(n397, n926);
    let n932: ZB = zb_not(n931);
    let n933: ZB = zb_and(n930, n931);
    let n934: ZB = zb_and(n930, n932);
    let n935: ZB = zb_or(n916, n933);
    let n936: ZB = zb_or(n902, n935);
    let n937: ZB = zb_or(n888, n936);
    let n938: ZB = zb_and(n483, n934);
    let n939: ZB = zb_and(n484, n934);
    let n940: ZN = zn_mget(g.cart, n799, n487);
    let n941: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n940);
    let n942: ZB = zb_not(n941);
    let n943: ZB = zb_and(n938, n941);
    let n944: ZB = zb_and(n938, n942);
    let n945: ZB = zb_and(n325, n943);
    let n946: ZB = zb_and(n324, n943);
    let n947: ZB = zb_or(n945, n946);
    let n948: ZB = zb_or(n944, n947);
    let n949: ZB = zb_and(n499, n941);
    let n950: ZB = zb_not(n949);
    let n951: ZB = zb_and(n948, n949);
    let n952: ZB = zb_and(n948, n950);
    let n953: ZB = zb_or(n951, n952);
    let n954: ZB = zb_and(n338, n949);
    let n955: ZB = zb_not(n954);
    let n956: ZB = zb_and(n953, n954);
    let n957: ZB = zb_and(n953, n955);
    let n958: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n940);
    let n959: ZB = zb_not(n958);
    let n960: ZB = zb_and(n957, n958);
    let n961: ZB = zb_and(n957, n959);
    let n962: ZB = zb_or(n960, n961);
    let n963: ZB = zb_and(n349, n958);
    let n964: ZB = zb_not(n963);
    let n965: ZB = zb_and(n962, n963);
    let n966: ZB = zb_and(n962, n964);
    let n967: ZB = zb_or(n965, n966);
    let n968: ZB = zb_and(n355, n963);
    let n969: ZB = zb_not(n968);
    let n970: ZB = zb_and(n967, n968);
    let n971: ZB = zb_and(n967, n969);
    let n972: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n940);
    let n973: ZB = zb_not(n972);
    let n974: ZB = zb_and(n971, n972);
    let n975: ZB = zb_and(n971, n973);
    let n976: ZB = zb_or(n974, n975);
    let n977: ZB = zb_and(n366, n972);
    let n978: ZB = zb_not(n977);
    let n979: ZB = zb_and(n976, n977);
    let n980: ZB = zb_and(n976, n978);
    let n981: ZB = zb_or(n979, n980);
    let n982: ZB = zb_and(n372, n977);
    let n983: ZB = zb_not(n982);
    let n984: ZB = zb_and(n981, n982);
    let n985: ZB = zb_and(n981, n983);
    let n986: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n940);
    let n987: ZB = zb_not(n986);
    let n988: ZB = zb_and(n985, n986);
    let n989: ZB = zb_and(n985, n987);
    let n990: ZB = zb_and(n384, n988);
    let n991: ZB = zb_and(n383, n988);
    let n992: ZB = zb_or(n990, n991);
    let n993: ZB = zb_or(n989, n992);
    let n994: ZB = zb_and(n856, n986);
    let n995: ZB = zb_not(n994);
    let n996: ZB = zb_and(n993, n994);
    let n997: ZB = zb_and(n993, n995);
    let n998: ZB = zb_or(n996, n997);
    let n999: ZB = zb_and(n397, n994);
    let n1000: ZB = zb_not(n999);
    let n1001: ZB = zb_and(n998, n999);
    let n1002: ZB = zb_and(n998, n1000);
    let n1003: ZB = zb_or(n984, n1001);
    let n1004: ZB = zb_or(n970, n1003);
    let n1005: ZB = zb_or(n956, n1004);
    let n1006: ZB = zb_and(n559, n791);
    let n1007: ZB = zb_or(n939, n1002);
    let n1008: ZB = zsel_b(n484, n791, n1006);
    let n1009: ZB = zb_or(n937, n1005);
    let n1010: ZB = zb_or(n871, n1007);
    let n1011: ZB = zsel_b(n408, n791, n1008);
    let n1012: ZB = zb_or(n869, n1009);
    let n1013: ZB = zb_or(n798, n1010);
    let n1014: ZB = zsel_b(n313, n791, n1011);
    let n1015: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n294);
    let n1016: ZB = zn_gt(n1015, n298);
    let n1017: ZB = zb_and(n1014, n1016);
    let n1018: ZB = zb_or(n789, n1012);
    let n1019: ZB = zsel_b(n789, n568, n791);
    let n1020: ZB = zb_or(n796, n1013);
    let n1021: ZB = zsel_b(n794, n791, n1017);
    let n1022: ZB = zb_or(n566, n1018);
    let n1023: ZB = zsel_b(n566, n289, n1019);
    let n1024: ZB = zb_or(n573, n1020);
    let n1025: ZB = zsel_b(n571, n568, n1021);
    let n1026: ZB = zb_or(n303, n1024);
    let n1027: ZB = zsel_b(n301, n289, n1025);
    let n1028: ZB = zn_gt(n286, zn_splat(P8::from_raw(8388608i32)));
    let n1029: ZB = zn_le(n286, zn_splat(P8::from_raw(8388608i32)));
    let n1030: ZB = zb_and(n1022, n1028);
    let n1031: ZB = zb_and(n1022, n1029);
    let n1032: ZB = zb_or(n1030, n1031);
    let n1033: ZB = zb_and(n1026, n1028);
    let n1034: ZB = zb_or(n1032, n1033);
    let n1035: ZB = zsel_b(n1032, n1023, n1027);
    let n1036: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n290);
    let n1037: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n291);
    let n1038: ZB = zn_tile_flag_at(g.cache, g.cart, n1036, n1037, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1039: ZB = zb_not(n1038);
    let n1040: ZB = zb_and(n1034, n1039);
    let n1041: ZB = zb_and(n1034, n1038);
    let n1042: ZB = zb_or(n1040, n1041);
    let n1043: ZB = zb_and(n1039, n1042);
    let n1044: ZB = zb_and(n1038, n1042);
    let n1045: ZB = zb_or(n1043, n1044);
    let n1046: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n1047: ZB = zn_le(r_c241, zn_splat(P8::from_raw(0i32)));
    let n1048: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n1049: ZN = zsel_n(n1046, n1048, r_c241);
    let n1050: ZN = zsel_n(n1038, zn_splat(P8::from_raw(393216i32)), n1049);
    let n1051: ZB = zb_and(n1038, n1045);
    let n1052: ZB = zb_and(n1039, n1045);
    let n1053: ZB = zb_and(n1046, n1052);
    let n1054: ZB = zb_and(n1047, n1052);
    let n1055: ZB = zb_or(n1053, n1054);
    let n1056: ZB = zn_gt(r_c238, zn_splat(P8::from_raw(0i32)));
    let n1057: ZB = zn_le(r_c238, zn_splat(P8::from_raw(0i32)));
    let n1058: ZB = zn_gt(n287, r_c272);
    let n1059: ZB = zn_le(n287, r_c272);
    let n1060: ZB = zn_gt(n288, r_c273);
    let n1061: ZB = zn_le(n288, r_c273);
    let n1062: ZN = zsel_n(n1039, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1063: ZN = zn_abs(n287);
    let n1064: ZB = zn_gt(n1063, zn_splat(P8::from_raw(65536i32)));
    let n1065: ZB = zn_le(n1063, zn_splat(P8::from_raw(65536i32)));
    let n1066: ZB = zn_gt(n287, zn_splat(P8::from_raw(0i32)));
    let n1067: ZB = zn_lt(n287, zn_splat(P8::from_raw(0i32)));
    let n1068: ZB = zn_gt(n287, zn_splat(P8::from_raw(65536i32)));
    let n1069: ZB = zn_le(n287, zn_splat(P8::from_raw(65536i32)));
    let n1070: ZN = zn_sub(n287, zn_splat(P8::from_raw(9830i32)));
    let n1071: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1070);
    let n1072: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n287);
    let n1073: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1072);
    let n1074: ZB = zn_gt(n287, zn_splat(P8::from_raw(-65536i32)));
    let n1075: ZB = zn_le(n287, zn_splat(P8::from_raw(-65536i32)));
    let n1076: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1070);
    let n1077: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1072);
    let n1078: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1070);
    let n1079: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1072);
    let n1080: ZN = zsel_n(n1074, n1076, n1077);
    let n1081: ZN = zsel_n(n1066, n1078, n1079);
    let n1082: ZN = zsel_n(n1068, n1071, n1073);
    let n1083: ZN = zsel_n(n1067, n1080, n1081);
    let n1084: ZN = zsel_n(n1066, n1082, n1083);
    let n1085: ZN = zn_sub(n287, n1062);
    let n1086: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1085);
    let n1087: ZN = zn_add(n287, n1062);
    let n1088: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1087);
    let n1089: ZN = zsel_n(n1066, n1086, n1088);
    let n1090: ZN = zsel_n(n1064, n1084, n1089);
    let n1091: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1090);
    let n1092: ZB = zb_not(n1091);
    let n1093: ZB = zn_lt(n1090, zn_splat(P8::from_raw(0i32)));
    let n1094: ZB = zsel_b(n1092, n1093, r_c274);
    let n1095: ZN = zn_abs(n288);
    let n1096: ZB = zn_le(n1095, zn_splat(P8::from_raw(9830i32)));
    let n1097: ZB = zn_gt(n1095, zn_splat(P8::from_raw(9830i32)));
    let n1098: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n291);
    let n1099: ZB = zn_gt(n288, zn_splat(P8::from_raw(131072i32)));
    let n1100: ZB = zn_le(n288, zn_splat(P8::from_raw(131072i32)));
    let n1101: ZB = zn_gt(n1050, zn_splat(P8::from_raw(0i32)));
    let n1102: ZB = zn_le(n1050, zn_splat(P8::from_raw(0i32)));
    let n1103: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n290);
    let n1104: ZB = zn_tile_flag_at(g.cache, g.cart, n1103, n1098, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1105: ZB = zb_not(n1104);
    let n1106: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n290);
    let n1107: ZB = zn_tile_flag_at(g.cache, g.cart, n1106, n1098, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1108: ZB = zb_not(n1107);
    let n1109: ZN = zsel_n(n1107, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1110: ZN = zsel_n(n1104, zn_splat(P8::from_raw(-65536i32)), n1109);
    let n1111: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1110);
    let n1112: ZB = zb_not(n1111);
    let n1113: ZB = zb_not(n1094);
    let n1114: ZN = zsel_n(n1094, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1115: ZB = zn_gt(n1114, zn_splat(P8::from_raw(0i32)));
    let n1116: ZB = zn_le(n1114, zn_splat(P8::from_raw(0i32)));
    let n1117: ZB = zn_lt(n1114, zn_splat(P8::from_raw(0i32)));
    let n1118: ZB = zn_ge(n1114, zn_splat(P8::from_raw(0i32)));
    let n1119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1114);
    let n1120: ZB = zb_not(n1119);
    let n1121: ZB = zn_lt(n286, zn_splat(P8::from_raw(-262144i32)));
    let n1122: ZB = zn_ge(n286, zn_splat(P8::from_raw(-262144i32)));
    let n1123: ZB = zn_lt(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n1124: ZB = zn_ge(r_c239, zn_splat(P8::from_raw(65536i32)));
    let n1125: ZN = zsel_n(n1123, zn_splat(P8::from_raw(65536i32)), r_c239);
    let n1126: ZN = zsel_n(n1038, n1125, r_c239);
    let n1127: ZB = zb_and(n1051, n1123);
    let n1128: ZB = zb_and(n1051, n1124);
    let n1129: ZB = zb_or(n1127, n1128);
    let n1130: ZB = zb_or(n1055, n1129);
    let n1131: ZB = zn_gt(n1126, zn_splat(P8::from_raw(0i32)));
    let n1132: ZB = zn_le(n1126, zn_splat(P8::from_raw(0i32)));
    let n1133: ZB = zb_and(n1056, n1130);
    let n1134: ZB = zb_and(n1057, n1130);
    let n1135: ZB = zb_and(n1058, n1133);
    let n1136: ZB = zb_and(n1059, n1133);
    let n1137: ZB = zb_or(n1135, n1136);
    let n1138: ZB = zb_and(n1060, n1137);
    let n1139: ZB = zb_and(n1061, n1137);
    let n1140: ZB = zb_or(n1138, n1139);
    let n1141: ZB = zb_and(n1039, n1134);
    let n1142: ZB = zb_and(n1038, n1134);
    let n1143: ZB = zb_or(n1141, n1142);
    let n1144: ZB = zb_and(n1064, n1143);
    let n1145: ZB = zb_and(n1065, n1143);
    let n1146: ZB = zb_and(n1066, n1144);
    let n1147: ZB = zb_and(n372, n1144);
    let n1148: ZB = zb_and(n1067, n1147);
    let n1149: ZB = zb_and(n397, n1147);
    let n1150: ZB = zb_and(n1068, n1146);
    let n1151: ZB = zb_and(n1069, n1146);
    let n1152: ZB = zb_and(n1074, n1148);
    let n1153: ZB = zb_and(n1075, n1148);
    let n1154: ZB = zb_and(n372, n1149);
    let n1155: ZB = zb_or(n1152, n1153);
    let n1156: ZB = zb_or(n1150, n1151);
    let n1157: ZB = zb_or(n1154, n1155);
    let n1158: ZB = zb_or(n1156, n1157);
    let n1159: ZB = zb_and(n1066, n1145);
    let n1160: ZB = zb_and(n372, n1145);
    let n1161: ZB = zb_or(n1159, n1160);
    let n1162: ZB = zb_or(n1158, n1161);
    let n1163: ZB = zb_and(n1092, n1162);
    let n1164: ZB = zb_and(n1091, n1162);
    let n1165: ZB = zb_or(n1163, n1164);
    let n1166: ZB = zb_and(n1096, n1165);
    let n1167: ZB = zb_and(n1097, n1165);
    let n1168: ZB = zb_or(n1166, n1167);
    let n1169: ZB = zb_and(n1039, n1168);
    let n1170: ZB = zb_and(n1038, n1168);
    let n1171: ZB = zb_and(n1099, n1169);
    let n1172: ZB = zb_and(n1100, n1169);
    let n1173: ZB = zb_or(n1171, n1172);
    let n1174: ZB = zb_or(n1170, n1173);
    let n1175: ZB = zb_and(n1131, n1174);
    let n1176: ZB = zb_and(n1132, n1174);
    let n1177: ZB = zb_or(n1175, n1176);
    let n1178: ZB = zb_or(n1140, n1177);
    let n1179: ZB = zb_and(n1121, n1178);
    let n1180: ZB = zb_and(n1122, n1178);
    let n1181: ZB = zb_or(n1179, n1180);
    let n1183: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1187: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1188: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1187);
    let n1198: ZN = zsel_n(n1028, n1188, n1187);
    let n1199: ZN = zsel_n(n1032, n1198, n1187);
    let n1203: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1085);
    let n1204: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1087);
    let n1205: ZN = zsel_n(n1074, n1203, n1204);
    let n1206: ZN = zsel_n(n1064, n1084, n1205);
    let n1207: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1206);
    let n1208: ZB = zb_not(n1207);
    let n1209: ZB = zn_lt(n1206, zn_splat(P8::from_raw(0i32)));
    let n1210: ZB = zsel_b(n1208, n1209, r_c274);
    let n1211: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n290);
    let n1212: ZB = zn_tile_flag_at(g.cache, g.cart, n1211, n1098, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1213: ZB = zb_not(n1212);
    let n1214: ZN = zsel_n(n1212, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1215: ZB = zn_gt(n288, n1214);
    let n1216: ZB = zn_le(n288, n1214);
    let n1217: ZB = zb_and(n1074, n1145);
    let n1218: ZB = zb_and(n1075, n1145);
    let n1219: ZB = zb_or(n1217, n1218);
    let n1220: ZB = zb_or(n1158, n1219);
    let n1221: ZB = zb_and(n1208, n1220);
    let n1222: ZB = zb_and(n1207, n1220);
    let n1223: ZB = zb_or(n1221, n1222);
    let n1224: ZB = zb_and(n1096, n1223);
    let n1225: ZB = zb_and(n1097, n1223);
    let n1226: ZB = zb_or(n1224, n1225);
    let n1227: ZB = zb_and(n1213, n1226);
    let n1228: ZB = zb_and(n1212, n1226);
    let n1229: ZB = zb_or(n1227, n1228);
    let n1230: ZB = zb_and(n1213, n1229);
    let n1231: ZB = zb_and(n1212, n1229);
    let n1232: ZB = zb_or(n1230, n1231);
    let n1233: ZB = zb_and(n1212, n1232);
    let n1234: ZB = zb_and(n1213, n1232);
    let n1235: ZB = zb_or(n1233, n1234);
    let n1236: ZB = zb_and(n1212, n1235);
    let n1237: ZB = zb_and(n1213, n1235);
    let n1238: ZB = zb_or(n1236, n1237);
    let n1239: ZB = zb_and(n1039, n1238);
    let n1240: ZB = zb_and(n1038, n1238);
    let n1241: ZB = zb_and(n1215, n1239);
    let n1242: ZB = zb_and(n1216, n1239);
    let n1243: ZB = zb_or(n1241, n1242);
    let n1244: ZB = zb_or(n1240, n1243);
    let n1245: ZB = zb_and(n1131, n1244);
    let n1246: ZB = zb_and(n1132, n1244);
    let n1247: ZB = zb_or(n1245, n1246);
    let n1248: ZB = zb_or(n1140, n1247);
    let n1249: ZB = zb_and(n1121, n1248);
    let n1250: ZB = zb_and(n1122, n1248);
    let n1251: ZB = zb_or(n1249, n1250);
    let n1254: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1085);
    let n1255: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1087);
    let n1256: ZN = zsel_n(n1068, n1254, n1255);
    let n1257: ZN = zsel_n(n1064, n1084, n1256);
    let n1258: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1257);
    let n1259: ZB = zb_not(n1258);
    let n1260: ZB = zn_lt(n1257, zn_splat(P8::from_raw(0i32)));
    let n1261: ZB = zsel_b(n1259, n1260, r_c274);
    let n1262: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n290);
    let n1263: ZB = zn_tile_flag_at(g.cache, g.cart, n1262, n1098, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1264: ZB = zb_not(n1263);
    let n1265: ZN = zsel_n(n1263, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1266: ZB = zn_gt(n288, n1265);
    let n1267: ZB = zn_le(n288, n1265);
    let n1268: ZB = zb_and(n1068, n1145);
    let n1269: ZB = zb_and(n1069, n1145);
    let n1270: ZB = zb_or(n1268, n1269);
    let n1271: ZB = zb_or(n1158, n1270);
    let n1272: ZB = zb_and(n1259, n1271);
    let n1273: ZB = zb_and(n1258, n1271);
    let n1274: ZB = zb_or(n1272, n1273);
    let n1275: ZB = zb_and(n1096, n1274);
    let n1276: ZB = zb_and(n1097, n1274);
    let n1277: ZB = zb_or(n1275, n1276);
    let n1278: ZB = zb_and(n1264, n1277);
    let n1279: ZB = zb_and(n1263, n1277);
    let n1280: ZB = zb_or(n1278, n1279);
    let n1281: ZB = zb_and(n1264, n1280);
    let n1282: ZB = zb_and(n1263, n1280);
    let n1283: ZB = zb_or(n1281, n1282);
    let n1284: ZB = zb_and(n1263, n1283);
    let n1285: ZB = zb_and(n1264, n1283);
    let n1286: ZB = zb_or(n1284, n1285);
    let n1287: ZB = zb_and(n1263, n1286);
    let n1288: ZB = zb_and(n1264, n1286);
    let n1289: ZB = zb_or(n1287, n1288);
    let n1290: ZB = zb_and(n1039, n1289);
    let n1291: ZB = zb_and(n1038, n1289);
    let n1292: ZB = zb_and(n1266, n1290);
    let n1293: ZB = zb_and(n1267, n1290);
    let n1294: ZB = zb_or(n1292, n1293);
    let n1295: ZB = zb_or(n1291, n1294);
    let n1296: ZB = zb_and(n1131, n1295);
    let n1297: ZB = zb_and(n1132, n1295);
    let n1298: ZB = zb_or(n1296, n1297);
    let n1299: ZB = zb_or(n1140, n1298);
    let n1300: ZB = zb_and(n1121, n1299);
    let n1301: ZB = zb_and(n1122, n1299);
    let n1302: ZB = zb_or(n1300, n1301);
    let n1305: ZB = zb_and(n55, n1174);
    let n1306: ZB = zb_and(r_c249, n1174);
    let n1307: ZB = zb_and(n1101, n1305);
    let n1308: ZB = zb_and(n1102, n1305);
    let n1309: ZB = zb_and(n1105, n1308);
    let n1310: ZB = zb_and(n1104, n1308);
    let n1311: ZB = zb_or(n1309, n1310);
    let n1312: ZB = zb_and(n1105, n1311);
    let n1313: ZB = zb_and(n1104, n1311);
    let n1314: ZB = zb_or(n1312, n1313);
    let n1315: ZB = zb_and(n1104, n1314);
    let n1316: ZB = zb_and(n1105, n1314);
    let n1317: ZB = zb_and(n1108, n1316);
    let n1318: ZB = zb_and(n1107, n1316);
    let n1319: ZB = zb_or(n1317, n1318);
    let n1320: ZB = zb_and(n1108, n1319);
    let n1321: ZB = zb_and(n1107, n1319);
    let n1322: ZB = zb_or(n1320, n1321);
    let n1323: ZB = zb_and(n1107, n1322);
    let n1324: ZB = zb_and(n1108, n1322);
    let n1325: ZB = zb_or(n1323, n1324);
    let n1326: ZB = zb_or(n1315, n1325);
    let n1327: ZB = zb_and(n1112, n1326);
    let n1328: ZB = zb_and(n1111, n1326);
    let n1329: ZB = zb_or(n1327, n1328);
    let n1330: ZB = zb_or(n1307, n1329);
    let n1331: ZB = zb_or(n1306, n1330);
    let n1332: ZB = zb_and(n1131, n1331);
    let n1333: ZB = zb_and(n1132, n1331);
    let n1334: ZB = zb_or(n1332, n1333);
    let n1335: ZB = zb_or(n1140, n1334);
    let n1336: ZB = zb_and(n1121, n1335);
    let n1337: ZB = zb_and(n1122, n1335);
    let n1338: ZB = zb_or(n1336, n1337);
    let n1341: ZB = zb_and(n55, n1244);
    let n1342: ZB = zb_and(r_c249, n1244);
    let n1343: ZB = zb_and(n1101, n1341);
    let n1344: ZB = zb_and(n1102, n1341);
    let n1345: ZB = zb_and(n1105, n1344);
    let n1346: ZB = zb_and(n1104, n1344);
    let n1347: ZB = zb_or(n1345, n1346);
    let n1348: ZB = zb_and(n1105, n1347);
    let n1349: ZB = zb_and(n1104, n1347);
    let n1350: ZB = zb_or(n1348, n1349);
    let n1351: ZB = zb_and(n1104, n1350);
    let n1352: ZB = zb_and(n1105, n1350);
    let n1353: ZB = zb_and(n1108, n1352);
    let n1354: ZB = zb_and(n1107, n1352);
    let n1355: ZB = zb_or(n1353, n1354);
    let n1356: ZB = zb_and(n1108, n1355);
    let n1357: ZB = zb_and(n1107, n1355);
    let n1358: ZB = zb_or(n1356, n1357);
    let n1359: ZB = zb_and(n1107, n1358);
    let n1360: ZB = zb_and(n1108, n1358);
    let n1361: ZB = zb_or(n1359, n1360);
    let n1362: ZB = zb_or(n1351, n1361);
    let n1363: ZB = zb_and(n1112, n1362);
    let n1364: ZB = zb_and(n1111, n1362);
    let n1365: ZB = zb_or(n1363, n1364);
    let n1366: ZB = zb_or(n1343, n1365);
    let n1367: ZB = zb_or(n1342, n1366);
    let n1368: ZB = zb_and(n1131, n1367);
    let n1369: ZB = zb_and(n1132, n1367);
    let n1370: ZB = zb_or(n1368, n1369);
    let n1371: ZB = zb_or(n1140, n1370);
    let n1372: ZB = zb_and(n1121, n1371);
    let n1373: ZB = zb_and(n1122, n1371);
    let n1374: ZB = zb_or(n1372, n1373);
    let n1377: ZB = zb_and(n55, n1295);
    let n1378: ZB = zb_and(r_c249, n1295);
    let n1379: ZB = zb_and(n1101, n1377);
    let n1380: ZB = zb_and(n1102, n1377);
    let n1381: ZB = zb_and(n1105, n1380);
    let n1382: ZB = zb_and(n1104, n1380);
    let n1383: ZB = zb_or(n1381, n1382);
    let n1384: ZB = zb_and(n1105, n1383);
    let n1385: ZB = zb_and(n1104, n1383);
    let n1386: ZB = zb_or(n1384, n1385);
    let n1387: ZB = zb_and(n1104, n1386);
    let n1388: ZB = zb_and(n1105, n1386);
    let n1389: ZB = zb_and(n1108, n1388);
    let n1390: ZB = zb_and(n1107, n1388);
    let n1391: ZB = zb_or(n1389, n1390);
    let n1392: ZB = zb_and(n1108, n1391);
    let n1393: ZB = zb_and(n1107, n1391);
    let n1394: ZB = zb_or(n1392, n1393);
    let n1395: ZB = zb_and(n1107, n1394);
    let n1396: ZB = zb_and(n1108, n1394);
    let n1397: ZB = zb_or(n1395, n1396);
    let n1398: ZB = zb_or(n1387, n1397);
    let n1399: ZB = zb_and(n1112, n1398);
    let n1400: ZB = zb_and(n1111, n1398);
    let n1401: ZB = zb_or(n1399, n1400);
    let n1402: ZB = zb_or(n1379, n1401);
    let n1403: ZB = zb_or(n1378, n1402);
    let n1404: ZB = zb_and(n1131, n1403);
    let n1405: ZB = zb_and(n1132, n1403);
    let n1406: ZB = zb_or(n1404, n1405);
    let n1407: ZB = zb_or(n1140, n1406);
    let n1408: ZB = zb_and(n1121, n1407);
    let n1409: ZB = zb_and(n1122, n1407);
    let n1410: ZB = zb_or(n1408, n1409);
    let n1413: ZB = zb_and(n100, n1131);
    let n1414: ZB = zb_not(n1413);
    let n1415: ZN = zsel_n(n1413, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1416: ZB = zb_or(r_c41, n1413);
    let n1417: ZN = zsel_n(n1056, r_c20, n1415);
    let n1418: ZB = zsel_b(n1056, r_c41, n1416);
    let n1419: ZB = zb_and(n1177, n1413);
    let n1420: ZB = zb_and(n1177, n1414);
    let n1421: ZB = zb_and(n1094, n1419);
    let n1422: ZB = zb_and(n1113, n1419);
    let n1423: ZB = zb_or(n1421, n1422);
    let n1424: ZB = zb_and(n1115, n1423);
    let n1425: ZB = zb_and(n1116, n1423);
    let n1426: ZB = zb_and(n1117, n1425);
    let n1427: ZB = zb_and(n1118, n1425);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_or(n1424, n1428);
    let n1430: ZB = zb_and(n1120, n1429);
    let n1431: ZB = zb_and(n1119, n1429);
    let n1432: ZB = zb_or(n1430, n1431);
    let n1433: ZB = zb_or(n1420, n1432);
    let n1434: ZB = zb_or(n1140, n1433);
    let n1435: ZB = zb_and(n1121, n1434);
    let n1436: ZB = zb_and(n1122, n1434);
    let n1437: ZB = zb_or(n1435, n1436);
    let n1438: ZB = zb_and(n1122, n1437);
    let n1439: ZB = zn_gt(n1417, zn_splat(P8::from_raw(0i32)));
    let n1440: ZB = zn_le(n1417, zn_splat(P8::from_raw(0i32)));
    let n1441: ZB = zb_and(n1438, n1439);
    let n1442: ZB = zb_and(n1438, n1440);
    let n1443: ZB = zb_or(n1441, n1442);
    let n1444: ZB = zb_and(n1247, n1413);
    let n1445: ZB = zb_and(n1247, n1414);
    let n1446: ZB = zb_or(n1444, n1445);
    let n1447: ZB = zb_or(n1140, n1446);
    let n1448: ZB = zb_and(n1121, n1447);
    let n1449: ZB = zb_and(n1122, n1447);
    let n1450: ZB = zb_or(n1448, n1449);
    let n1451: ZB = zb_and(n1122, n1450);
    let n1452: ZB = zb_and(n1439, n1451);
    let n1453: ZB = zb_and(n1440, n1451);
    let n1454: ZB = zb_or(n1452, n1453);
    let n1455: ZB = zb_and(n1298, n1413);
    let n1456: ZB = zb_and(n1298, n1414);
    let n1457: ZB = zb_or(n1455, n1456);
    let n1458: ZB = zb_or(n1140, n1457);
    let n1459: ZB = zb_and(n1121, n1458);
    let n1460: ZB = zb_and(n1122, n1458);
    let n1461: ZB = zb_or(n1459, n1460);
    let n1462: ZB = zb_and(n1122, n1461);
    let n1463: ZB = zb_and(n1439, n1462);
    let n1464: ZB = zb_and(n1440, n1462);
    let n1465: ZB = zb_or(n1463, n1464);
    let n1466: ZB = zb_or(n1419, n1420);
    let n1467: ZB = zb_or(n1140, n1466);
    let n1468: ZB = zb_and(n1121, n1467);
    let n1469: ZB = zb_and(n1122, n1467);
    let n1470: ZB = zb_or(n1468, n1469);
    let n1471: ZB = zb_and(n1122, n1470);
    let n1472: ZB = zb_and(n1439, n1471);
    let n1473: ZB = zb_and(n1440, n1471);
    let n1474: ZB = zb_or(n1472, n1473);
    let n1475: ZB = zb_and(n1334, n1413);
    let n1476: ZB = zb_and(n1334, n1414);
    let n1477: ZB = zb_and(n1094, n1475);
    let n1478: ZB = zb_and(n1113, n1475);
    let n1479: ZB = zb_or(n1477, n1478);
    let n1480: ZB = zb_and(n1115, n1479);
    let n1481: ZB = zb_and(n1116, n1479);
    let n1482: ZB = zb_and(n1117, n1481);
    let n1483: ZB = zb_and(n1118, n1481);
    let n1484: ZB = zb_or(n1482, n1483);
    let n1485: ZB = zb_or(n1480, n1484);
    let n1486: ZB = zb_and(n1120, n1485);
    let n1487: ZB = zb_and(n1119, n1485);
    let n1488: ZB = zb_or(n1486, n1487);
    let n1489: ZB = zb_or(n1476, n1488);
    let n1490: ZB = zb_or(n1140, n1489);
    let n1491: ZB = zb_and(n1121, n1490);
    let n1492: ZB = zb_and(n1122, n1490);
    let n1493: ZB = zb_or(n1491, n1492);
    let n1494: ZB = zb_and(n1122, n1493);
    let n1495: ZB = zb_and(n1439, n1494);
    let n1496: ZB = zb_and(n1440, n1494);
    let n1497: ZB = zb_or(n1495, n1496);
    let n1498: ZB = zb_and(n1370, n1413);
    let n1499: ZB = zb_and(n1370, n1414);
    let n1500: ZB = zb_or(n1498, n1499);
    let n1501: ZB = zb_or(n1140, n1500);
    let n1502: ZB = zb_and(n1121, n1501);
    let n1503: ZB = zb_and(n1122, n1501);
    let n1504: ZB = zb_or(n1502, n1503);
    let n1505: ZB = zb_and(n1122, n1504);
    let n1506: ZB = zb_and(n1439, n1505);
    let n1507: ZB = zb_and(n1440, n1505);
    let n1508: ZB = zb_or(n1506, n1507);
    let n1509: ZB = zb_and(n1406, n1413);
    let n1510: ZB = zb_and(n1406, n1414);
    let n1511: ZB = zb_or(n1509, n1510);
    let n1512: ZB = zb_or(n1140, n1511);
    let n1513: ZB = zb_and(n1121, n1512);
    let n1514: ZB = zb_and(n1122, n1512);
    let n1515: ZB = zb_or(n1513, n1514);
    let n1516: ZB = zb_and(n1122, n1515);
    let n1517: ZB = zb_and(n1439, n1516);
    let n1518: ZB = zb_and(n1440, n1516);
    let n1519: ZB = zb_or(n1517, n1518);
    let n1520: ZB = zb_or(n1475, n1476);
    let n1521: ZB = zb_or(n1140, n1520);
    let n1522: ZB = zb_and(n1121, n1521);
    let n1523: ZB = zb_and(n1122, n1521);
    let n1524: ZB = zb_or(n1522, n1523);
    let n1525: ZB = zb_and(n1122, n1524);
    let n1526: ZB = zb_and(n1439, n1525);
    let n1527: ZB = zb_and(n1440, n1525);
    let n1528: ZB = zb_or(n1526, n1527);
    let n1532: ZB = zb_and(n1026, n1029);
    let n1533: ZB = zb_and(n1039, n1532);
    let n1534: ZB = zb_and(n1038, n1532);
    let n1535: ZB = zb_or(n1533, n1534);
    let n1536: ZB = zb_and(n1039, n1535);
    let n1537: ZB = zb_and(n1038, n1535);
    let n1538: ZB = zb_or(n1536, n1537);
    let n1539: ZB = zb_and(n1038, n1538);
    let n1540: ZB = zb_and(n1039, n1538);
    let n1541: ZB = zb_and(n1046, n1540);
    let n1542: ZB = zb_and(n1047, n1540);
    let n1543: ZB = zb_or(n1541, n1542);
    let n1544: ZB = zb_and(n1123, n1539);
    let n1545: ZB = zb_and(n1124, n1539);
    let n1546: ZB = zb_or(n1544, n1545);
    let n1547: ZB = zb_or(n1543, n1546);
    let n1548: ZB = zb_and(n1056, n1547);
    let n1549: ZB = zb_and(n1057, n1547);
    let n1550: ZB = zb_and(n1058, n1548);
    let n1551: ZB = zb_and(n1059, n1548);
    let n1552: ZB = zb_or(n1550, n1551);
    let n1553: ZB = zb_and(n1060, n1552);
    let n1554: ZB = zb_and(n1061, n1552);
    let n1555: ZB = zb_or(n1553, n1554);
    let n1556: ZB = zb_and(n1039, n1549);
    let n1557: ZB = zb_and(n1038, n1549);
    let n1558: ZB = zb_or(n1556, n1557);
    let n1559: ZB = zb_and(n1064, n1558);
    let n1560: ZB = zb_and(n1065, n1558);
    let n1561: ZB = zb_and(n1066, n1559);
    let n1562: ZB = zb_and(n372, n1559);
    let n1563: ZB = zb_and(n1067, n1562);
    let n1564: ZB = zb_and(n397, n1562);
    let n1565: ZB = zb_and(n1068, n1561);
    let n1566: ZB = zb_and(n1069, n1561);
    let n1567: ZB = zb_and(n1074, n1563);
    let n1568: ZB = zb_and(n1075, n1563);
    let n1569: ZB = zb_and(n372, n1564);
    let n1570: ZB = zb_or(n1567, n1568);
    let n1571: ZB = zb_or(n1565, n1566);
    let n1572: ZB = zb_or(n1569, n1570);
    let n1573: ZB = zb_or(n1571, n1572);
    let n1574: ZB = zb_and(n1066, n1560);
    let n1575: ZB = zb_and(n372, n1560);
    let n1576: ZB = zb_or(n1574, n1575);
    let n1577: ZB = zb_or(n1573, n1576);
    let n1578: ZB = zb_and(n1092, n1577);
    let n1579: ZB = zb_and(n1091, n1577);
    let n1580: ZB = zb_or(n1578, n1579);
    let n1581: ZB = zb_and(n1096, n1580);
    let n1582: ZB = zb_and(n1097, n1580);
    let n1583: ZB = zb_or(n1581, n1582);
    let n1584: ZB = zb_and(n1039, n1583);
    let n1585: ZB = zb_and(n1038, n1583);
    let n1586: ZB = zb_and(n1099, n1584);
    let n1587: ZB = zb_and(n1100, n1584);
    let n1588: ZB = zb_or(n1586, n1587);
    let n1589: ZB = zb_or(n1585, n1588);
    let n1590: ZB = zb_and(n1131, n1589);
    let n1591: ZB = zb_and(n1132, n1589);
    let n1592: ZB = zb_or(n1590, n1591);
    let n1593: ZB = zb_or(n1555, n1592);
    let n1594: ZB = zb_and(n1121, n1593);
    let n1595: ZB = zb_and(n1122, n1593);
    let n1596: ZB = zb_or(n1594, n1595);
    let n1597: ZB = zb_and(n1121, n1596);
    let n1598: ZB = zb_and(n1121, n1181);
    let n1599: ZB = zb_not(n1597);
    let n1600: ZB = zb_or(n1597, n1598);
    let n1601: ZB = zsel_b(n1597, n1027, n1035);
    let n1603: ZN = zsel_n(n1597, r_c87, n1199);
    let n1604: ZN = zsel_n(n1597, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1606: ZB = zb_and(n1074, n1560);
    let n1607: ZB = zb_and(n1075, n1560);
    let n1608: ZB = zb_or(n1606, n1607);
    let n1609: ZB = zb_or(n1573, n1608);
    let n1610: ZB = zb_and(n1208, n1609);
    let n1611: ZB = zb_and(n1207, n1609);
    let n1612: ZB = zb_or(n1610, n1611);
    let n1613: ZB = zb_and(n1096, n1612);
    let n1614: ZB = zb_and(n1097, n1612);
    let n1615: ZB = zb_or(n1613, n1614);
    let n1616: ZB = zb_and(n1213, n1615);
    let n1617: ZB = zb_and(n1212, n1615);
    let n1618: ZB = zb_or(n1616, n1617);
    let n1619: ZB = zb_and(n1213, n1618);
    let n1620: ZB = zb_and(n1212, n1618);
    let n1621: ZB = zb_or(n1619, n1620);
    let n1622: ZB = zb_and(n1212, n1621);
    let n1623: ZB = zb_and(n1213, n1621);
    let n1624: ZB = zb_or(n1622, n1623);
    let n1625: ZB = zb_and(n1212, n1624);
    let n1626: ZB = zb_and(n1213, n1624);
    let n1627: ZB = zb_or(n1625, n1626);
    let n1628: ZB = zb_and(n1039, n1627);
    let n1629: ZB = zb_and(n1038, n1627);
    let n1630: ZB = zb_and(n1215, n1628);
    let n1631: ZB = zb_and(n1216, n1628);
    let n1632: ZB = zb_or(n1630, n1631);
    let n1633: ZB = zb_or(n1629, n1632);
    let n1634: ZB = zb_and(n1131, n1633);
    let n1635: ZB = zb_and(n1132, n1633);
    let n1636: ZB = zb_or(n1634, n1635);
    let n1637: ZB = zb_or(n1555, n1636);
    let n1638: ZB = zb_and(n1121, n1637);
    let n1639: ZB = zb_and(n1122, n1637);
    let n1640: ZB = zb_or(n1638, n1639);
    let n1641: ZB = zb_and(n1121, n1640);
    let n1642: ZB = zb_and(n1121, n1251);
    let n1643: ZB = zb_not(n1641);
    let n1644: ZB = zb_or(n1641, n1642);
    let n1645: ZB = zsel_b(n1641, n1027, n1035);
    let n1647: ZN = zsel_n(n1641, r_c87, n1199);
    let n1648: ZN = zsel_n(n1641, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1650: ZB = zb_and(n1068, n1560);
    let n1651: ZB = zb_and(n1069, n1560);
    let n1652: ZB = zb_or(n1650, n1651);
    let n1653: ZB = zb_or(n1573, n1652);
    let n1654: ZB = zb_and(n1259, n1653);
    let n1655: ZB = zb_and(n1258, n1653);
    let n1656: ZB = zb_or(n1654, n1655);
    let n1657: ZB = zb_and(n1096, n1656);
    let n1658: ZB = zb_and(n1097, n1656);
    let n1659: ZB = zb_or(n1657, n1658);
    let n1660: ZB = zb_and(n1264, n1659);
    let n1661: ZB = zb_and(n1263, n1659);
    let n1662: ZB = zb_or(n1660, n1661);
    let n1663: ZB = zb_and(n1264, n1662);
    let n1664: ZB = zb_and(n1263, n1662);
    let n1665: ZB = zb_or(n1663, n1664);
    let n1666: ZB = zb_and(n1263, n1665);
    let n1667: ZB = zb_and(n1264, n1665);
    let n1668: ZB = zb_or(n1666, n1667);
    let n1669: ZB = zb_and(n1263, n1668);
    let n1670: ZB = zb_and(n1264, n1668);
    let n1671: ZB = zb_or(n1669, n1670);
    let n1672: ZB = zb_and(n1039, n1671);
    let n1673: ZB = zb_and(n1038, n1671);
    let n1674: ZB = zb_and(n1266, n1672);
    let n1675: ZB = zb_and(n1267, n1672);
    let n1676: ZB = zb_or(n1674, n1675);
    let n1677: ZB = zb_or(n1673, n1676);
    let n1678: ZB = zb_and(n1131, n1677);
    let n1679: ZB = zb_and(n1132, n1677);
    let n1680: ZB = zb_or(n1678, n1679);
    let n1681: ZB = zb_or(n1555, n1680);
    let n1682: ZB = zb_and(n1121, n1681);
    let n1683: ZB = zb_and(n1122, n1681);
    let n1684: ZB = zb_or(n1682, n1683);
    let n1685: ZB = zb_and(n1121, n1684);
    let n1686: ZB = zb_and(n1121, n1302);
    let n1687: ZB = zb_not(n1685);
    let n1688: ZB = zb_or(n1685, n1686);
    let n1689: ZB = zsel_b(n1685, n1027, n1035);
    let n1691: ZN = zsel_n(n1685, r_c87, n1199);
    let n1692: ZN = zsel_n(n1685, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1694: ZB = zb_and(n55, n1589);
    let n1695: ZB = zb_and(r_c249, n1589);
    let n1696: ZB = zb_and(n1101, n1694);
    let n1697: ZB = zb_and(n1102, n1694);
    let n1698: ZB = zb_and(n1105, n1697);
    let n1699: ZB = zb_and(n1104, n1697);
    let n1700: ZB = zb_or(n1698, n1699);
    let n1701: ZB = zb_and(n1105, n1700);
    let n1702: ZB = zb_and(n1104, n1700);
    let n1703: ZB = zb_or(n1701, n1702);
    let n1704: ZB = zb_and(n1104, n1703);
    let n1705: ZB = zb_and(n1105, n1703);
    let n1706: ZB = zb_and(n1108, n1705);
    let n1707: ZB = zb_and(n1107, n1705);
    let n1708: ZB = zb_or(n1706, n1707);
    let n1709: ZB = zb_and(n1108, n1708);
    let n1710: ZB = zb_and(n1107, n1708);
    let n1711: ZB = zb_or(n1709, n1710);
    let n1712: ZB = zb_and(n1107, n1711);
    let n1713: ZB = zb_and(n1108, n1711);
    let n1714: ZB = zb_or(n1712, n1713);
    let n1715: ZB = zb_or(n1704, n1714);
    let n1716: ZB = zb_and(n1112, n1715);
    let n1717: ZB = zb_and(n1111, n1715);
    let n1718: ZB = zb_or(n1716, n1717);
    let n1719: ZB = zb_or(n1696, n1718);
    let n1720: ZB = zb_or(n1695, n1719);
    let n1721: ZB = zb_and(n1131, n1720);
    let n1722: ZB = zb_and(n1132, n1720);
    let n1723: ZB = zb_or(n1721, n1722);
    let n1724: ZB = zb_or(n1555, n1723);
    let n1725: ZB = zb_and(n1121, n1724);
    let n1726: ZB = zb_and(n1122, n1724);
    let n1727: ZB = zb_or(n1725, n1726);
    let n1728: ZB = zb_and(n1121, n1727);
    let n1729: ZB = zb_and(n1121, n1338);
    let n1730: ZB = zb_not(n1728);
    let n1731: ZB = zb_or(n1728, n1729);
    let n1732: ZB = zsel_b(n1728, n1027, n1035);
    let n1734: ZN = zsel_n(n1728, r_c87, n1199);
    let n1735: ZN = zsel_n(n1728, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1737: ZB = zb_and(n55, n1633);
    let n1738: ZB = zb_and(r_c249, n1633);
    let n1739: ZB = zb_and(n1101, n1737);
    let n1740: ZB = zb_and(n1102, n1737);
    let n1741: ZB = zb_and(n1105, n1740);
    let n1742: ZB = zb_and(n1104, n1740);
    let n1743: ZB = zb_or(n1741, n1742);
    let n1744: ZB = zb_and(n1105, n1743);
    let n1745: ZB = zb_and(n1104, n1743);
    let n1746: ZB = zb_or(n1744, n1745);
    let n1747: ZB = zb_and(n1104, n1746);
    let n1748: ZB = zb_and(n1105, n1746);
    let n1749: ZB = zb_and(n1108, n1748);
    let n1750: ZB = zb_and(n1107, n1748);
    let n1751: ZB = zb_or(n1749, n1750);
    let n1752: ZB = zb_and(n1108, n1751);
    let n1753: ZB = zb_and(n1107, n1751);
    let n1754: ZB = zb_or(n1752, n1753);
    let n1755: ZB = zb_and(n1107, n1754);
    let n1756: ZB = zb_and(n1108, n1754);
    let n1757: ZB = zb_or(n1755, n1756);
    let n1758: ZB = zb_or(n1747, n1757);
    let n1759: ZB = zb_and(n1112, n1758);
    let n1760: ZB = zb_and(n1111, n1758);
    let n1761: ZB = zb_or(n1759, n1760);
    let n1762: ZB = zb_or(n1739, n1761);
    let n1763: ZB = zb_or(n1738, n1762);
    let n1764: ZB = zb_and(n1131, n1763);
    let n1765: ZB = zb_and(n1132, n1763);
    let n1766: ZB = zb_or(n1764, n1765);
    let n1767: ZB = zb_or(n1555, n1766);
    let n1768: ZB = zb_and(n1121, n1767);
    let n1769: ZB = zb_and(n1122, n1767);
    let n1770: ZB = zb_or(n1768, n1769);
    let n1771: ZB = zb_and(n1121, n1770);
    let n1772: ZB = zb_and(n1121, n1374);
    let n1773: ZB = zb_not(n1771);
    let n1774: ZB = zb_or(n1771, n1772);
    let n1775: ZB = zsel_b(n1771, n1027, n1035);
    let n1777: ZN = zsel_n(n1771, r_c87, n1199);
    let n1778: ZN = zsel_n(n1771, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1780: ZB = zb_and(n55, n1677);
    let n1781: ZB = zb_and(r_c249, n1677);
    let n1782: ZB = zb_and(n1101, n1780);
    let n1783: ZB = zb_and(n1102, n1780);
    let n1784: ZB = zb_and(n1105, n1783);
    let n1785: ZB = zb_and(n1104, n1783);
    let n1786: ZB = zb_or(n1784, n1785);
    let n1787: ZB = zb_and(n1105, n1786);
    let n1788: ZB = zb_and(n1104, n1786);
    let n1789: ZB = zb_or(n1787, n1788);
    let n1790: ZB = zb_and(n1104, n1789);
    let n1791: ZB = zb_and(n1105, n1789);
    let n1792: ZB = zb_and(n1108, n1791);
    let n1793: ZB = zb_and(n1107, n1791);
    let n1794: ZB = zb_or(n1792, n1793);
    let n1795: ZB = zb_and(n1108, n1794);
    let n1796: ZB = zb_and(n1107, n1794);
    let n1797: ZB = zb_or(n1795, n1796);
    let n1798: ZB = zb_and(n1107, n1797);
    let n1799: ZB = zb_and(n1108, n1797);
    let n1800: ZB = zb_or(n1798, n1799);
    let n1801: ZB = zb_or(n1790, n1800);
    let n1802: ZB = zb_and(n1112, n1801);
    let n1803: ZB = zb_and(n1111, n1801);
    let n1804: ZB = zb_or(n1802, n1803);
    let n1805: ZB = zb_or(n1782, n1804);
    let n1806: ZB = zb_or(n1781, n1805);
    let n1807: ZB = zb_and(n1131, n1806);
    let n1808: ZB = zb_and(n1132, n1806);
    let n1809: ZB = zb_or(n1807, n1808);
    let n1810: ZB = zb_or(n1555, n1809);
    let n1811: ZB = zb_and(n1121, n1810);
    let n1812: ZB = zb_and(n1122, n1810);
    let n1813: ZB = zb_or(n1811, n1812);
    let n1814: ZB = zb_and(n1121, n1813);
    let n1815: ZB = zb_and(n1121, n1410);
    let n1816: ZB = zb_not(n1814);
    let n1817: ZB = zb_or(n1814, n1815);
    let n1818: ZB = zsel_b(n1814, n1027, n1035);
    let n1820: ZN = zsel_n(n1814, r_c87, n1199);
    let n1821: ZN = zsel_n(n1814, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1823: ZB = zb_and(n1413, n1592);
    let n1824: ZB = zb_and(n1414, n1592);
    let n1825: ZB = zb_and(n1094, n1823);
    let n1826: ZB = zb_and(n1113, n1823);
    let n1827: ZB = zb_or(n1825, n1826);
    let n1828: ZB = zb_and(n1115, n1827);
    let n1829: ZB = zb_and(n1116, n1827);
    let n1830: ZB = zb_and(n1117, n1829);
    let n1831: ZB = zb_and(n1118, n1829);
    let n1832: ZB = zb_or(n1830, n1831);
    let n1833: ZB = zb_or(n1828, n1832);
    let n1834: ZB = zb_and(n1120, n1833);
    let n1835: ZB = zb_and(n1119, n1833);
    let n1836: ZB = zb_or(n1834, n1835);
    let n1837: ZB = zb_or(n1824, n1836);
    let n1838: ZB = zb_or(n1555, n1837);
    let n1839: ZB = zb_and(n1121, n1838);
    let n1840: ZB = zb_and(n1122, n1838);
    let n1841: ZB = zb_or(n1839, n1840);
    let n1842: ZB = zb_and(n1121, n1841);
    let n1843: ZB = zb_and(n1121, n1437);
    let n1844: ZB = zb_not(n1842);
    let n1845: ZB = zb_or(n1842, n1843);
    let n1846: ZB = zsel_b(n1842, n1027, n1035);
    let n1847: ZB = zb_and(n1439, n1845);
    let n1848: ZB = zb_and(n1440, n1845);
    let n1849: ZB = zb_or(n1847, n1848);
    let n1850: ZN = zsel_n(n1842, r_c87, n1199);
    let n1851: ZN = zsel_n(n1842, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1853: ZB = zb_and(n1413, n1636);
    let n1854: ZB = zb_and(n1414, n1636);
    let n1855: ZB = zb_or(n1853, n1854);
    let n1856: ZB = zb_or(n1555, n1855);
    let n1857: ZB = zb_and(n1121, n1856);
    let n1858: ZB = zb_and(n1122, n1856);
    let n1859: ZB = zb_or(n1857, n1858);
    let n1860: ZB = zb_and(n1121, n1859);
    let n1861: ZB = zb_and(n1121, n1450);
    let n1862: ZB = zb_not(n1860);
    let n1863: ZB = zb_or(n1860, n1861);
    let n1864: ZB = zsel_b(n1860, n1027, n1035);
    let n1865: ZB = zb_and(n1439, n1863);
    let n1866: ZB = zb_and(n1440, n1863);
    let n1867: ZB = zb_or(n1865, n1866);
    let n1868: ZN = zsel_n(n1860, r_c87, n1199);
    let n1869: ZN = zsel_n(n1860, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1871: ZB = zb_and(n1413, n1680);
    let n1872: ZB = zb_and(n1414, n1680);
    let n1873: ZB = zb_or(n1871, n1872);
    let n1874: ZB = zb_or(n1555, n1873);
    let n1875: ZB = zb_and(n1121, n1874);
    let n1876: ZB = zb_and(n1122, n1874);
    let n1877: ZB = zb_or(n1875, n1876);
    let n1878: ZB = zb_and(n1121, n1877);
    let n1879: ZB = zb_and(n1121, n1461);
    let n1880: ZB = zb_not(n1878);
    let n1881: ZB = zb_or(n1878, n1879);
    let n1882: ZB = zsel_b(n1878, n1027, n1035);
    let n1883: ZB = zb_and(n1439, n1881);
    let n1884: ZB = zb_and(n1440, n1881);
    let n1885: ZB = zb_or(n1883, n1884);
    let n1886: ZN = zsel_n(n1878, r_c87, n1199);
    let n1887: ZN = zsel_n(n1878, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1889: ZB = zb_or(n1823, n1824);
    let n1890: ZB = zb_or(n1555, n1889);
    let n1891: ZB = zb_and(n1121, n1890);
    let n1892: ZB = zb_and(n1122, n1890);
    let n1893: ZB = zb_or(n1891, n1892);
    let n1894: ZB = zb_and(n1121, n1893);
    let n1895: ZB = zb_and(n1121, n1470);
    let n1896: ZB = zb_not(n1894);
    let n1897: ZB = zb_or(n1894, n1895);
    let n1898: ZB = zsel_b(n1894, n1027, n1035);
    let n1899: ZB = zb_and(n1439, n1897);
    let n1900: ZB = zb_and(n1440, n1897);
    let n1901: ZB = zb_or(n1899, n1900);
    let n1902: ZN = zsel_n(n1894, r_c87, n1199);
    let n1903: ZN = zsel_n(n1894, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1905: ZB = zb_and(n1413, n1723);
    let n1906: ZB = zb_and(n1414, n1723);
    let n1907: ZB = zb_and(n1094, n1905);
    let n1908: ZB = zb_and(n1113, n1905);
    let n1909: ZB = zb_or(n1907, n1908);
    let n1910: ZB = zb_and(n1115, n1909);
    let n1911: ZB = zb_and(n1116, n1909);
    let n1912: ZB = zb_and(n1117, n1911);
    let n1913: ZB = zb_and(n1118, n1911);
    let n1914: ZB = zb_or(n1912, n1913);
    let n1915: ZB = zb_or(n1910, n1914);
    let n1916: ZB = zb_and(n1120, n1915);
    let n1917: ZB = zb_and(n1119, n1915);
    let n1918: ZB = zb_or(n1916, n1917);
    let n1919: ZB = zb_or(n1906, n1918);
    let n1920: ZB = zb_or(n1555, n1919);
    let n1921: ZB = zb_and(n1121, n1920);
    let n1922: ZB = zb_and(n1122, n1920);
    let n1923: ZB = zb_or(n1921, n1922);
    let n1924: ZB = zb_and(n1121, n1923);
    let n1925: ZB = zb_and(n1121, n1493);
    let n1926: ZB = zb_not(n1924);
    let n1927: ZB = zb_or(n1924, n1925);
    let n1928: ZB = zsel_b(n1924, n1027, n1035);
    let n1929: ZB = zb_and(n1439, n1927);
    let n1930: ZB = zb_and(n1440, n1927);
    let n1931: ZB = zb_or(n1929, n1930);
    let n1932: ZN = zsel_n(n1924, r_c87, n1199);
    let n1933: ZN = zsel_n(n1924, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1935: ZB = zb_and(n1413, n1766);
    let n1936: ZB = zb_and(n1414, n1766);
    let n1937: ZB = zb_or(n1935, n1936);
    let n1938: ZB = zb_or(n1555, n1937);
    let n1939: ZB = zb_and(n1121, n1938);
    let n1940: ZB = zb_and(n1122, n1938);
    let n1941: ZB = zb_or(n1939, n1940);
    let n1942: ZB = zb_and(n1121, n1941);
    let n1943: ZB = zb_and(n1121, n1504);
    let n1944: ZB = zb_not(n1942);
    let n1945: ZB = zb_or(n1942, n1943);
    let n1946: ZB = zsel_b(n1942, n1027, n1035);
    let n1947: ZB = zb_and(n1439, n1945);
    let n1948: ZB = zb_and(n1440, n1945);
    let n1949: ZB = zb_or(n1947, n1948);
    let n1950: ZN = zsel_n(n1942, r_c87, n1199);
    let n1951: ZN = zsel_n(n1942, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1953: ZB = zb_and(n1413, n1809);
    let n1954: ZB = zb_and(n1414, n1809);
    let n1955: ZB = zb_or(n1953, n1954);
    let n1956: ZB = zb_or(n1555, n1955);
    let n1957: ZB = zb_and(n1121, n1956);
    let n1958: ZB = zb_and(n1122, n1956);
    let n1959: ZB = zb_or(n1957, n1958);
    let n1960: ZB = zb_and(n1121, n1959);
    let n1961: ZB = zb_and(n1121, n1515);
    let n1962: ZB = zb_not(n1960);
    let n1963: ZB = zb_or(n1960, n1961);
    let n1964: ZB = zsel_b(n1960, n1027, n1035);
    let n1965: ZB = zb_and(n1439, n1963);
    let n1966: ZB = zb_and(n1440, n1963);
    let n1967: ZB = zb_or(n1965, n1966);
    let n1968: ZN = zsel_n(n1960, r_c87, n1199);
    let n1969: ZN = zsel_n(n1960, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1971: ZB = zb_or(n1905, n1906);
    let n1972: ZB = zb_or(n1555, n1971);
    let n1973: ZB = zb_and(n1121, n1972);
    let n1974: ZB = zb_and(n1122, n1972);
    let n1975: ZB = zb_or(n1973, n1974);
    let n1976: ZB = zb_and(n1121, n1975);
    let n1977: ZB = zb_and(n1121, n1524);
    let n1978: ZB = zb_not(n1976);
    let n1979: ZB = zb_or(n1976, n1977);
    let n1980: ZB = zsel_b(n1976, n1027, n1035);
    let n1981: ZB = zb_and(n1439, n1979);
    let n1982: ZB = zb_and(n1440, n1979);
    let n1983: ZB = zb_or(n1981, n1982);
    let n1984: ZN = zsel_n(n1976, r_c87, n1199);
    let n1985: ZN = zsel_n(n1976, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1997: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1998: ZN = zn_sub(n77, zn_splat(P8::from_raw(32768i32)));
    let n1999: ZN = zn_sub(n1998, n78);
    let n2000: ZN = zsel_n(n144, zn_splat(P8::from_raw(0i32)), n1999);
    let n2001: ZN = zsel_n(n97, n1999, n2000);
    let n2002: ZN = zsel_n(n141, zn_splat(P8::from_raw(0i32)), n2001);
    let n2003: ZN = zsel_n(n95, n1999, n2002);
    let n2004: ZN = zsel_n(n138, zn_splat(P8::from_raw(0i32)), n2003);
    let n2005: ZN = zsel_n(n93, n1999, n2004);
    let n2006: ZN = zsel_n(n135, zn_splat(P8::from_raw(0i32)), n2005);
    let n2007: ZN = zsel_n(n91, n1999, n2006);
    let n2008: ZN = zsel_n(n132, zn_splat(P8::from_raw(0i32)), n2007);
    let n2009: ZN = zsel_n(n89, n1999, n2008);
    let n2010: ZN = zsel_n(n129, zn_splat(P8::from_raw(0i32)), n2009);
    let n2011: ZN = zsel_n(n87, n1999, n2010);
    let n2012: ZN = zsel_n(n126, zn_splat(P8::from_raw(0i32)), n2011);
    let n2013: ZN = zsel_n(n85, n1999, n2012);
    let n2014: ZN = zsel_n(n123, zn_splat(P8::from_raw(0i32)), n2013);
    let n2015: ZN = zn_sub(n191, zn_splat(P8::from_raw(32768i32)));
    let n2016: ZN = zn_sub(n2015, n192);
    let n2017: ZN = zsel_n(n236, zn_splat(P8::from_raw(0i32)), n2016);
    let n2018: ZN = zsel_n(n233, n2016, n2017);
    let n2019: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n2018);
    let n2020: ZN = zsel_n(n228, n2016, n2019);
    let n2021: ZN = zsel_n(n226, zn_splat(P8::from_raw(0i32)), n2020);
    let n2022: ZN = zsel_n(n223, n2016, n2021);
    let n2023: ZN = zsel_n(n221, zn_splat(P8::from_raw(0i32)), n2022);
    let n2024: ZN = zsel_n(n218, n2016, n2023);
    let n2025: ZN = zsel_n(n216, zn_splat(P8::from_raw(0i32)), n2024);
    let n2026: ZN = zsel_n(n213, n2016, n2025);
    let n2027: ZN = zsel_n(n211, zn_splat(P8::from_raw(0i32)), n2026);
    let n2028: ZN = zsel_n(n208, n2016, n2027);
    let n2029: ZN = zsel_n(n206, zn_splat(P8::from_raw(0i32)), n2028);
    let n2030: ZN = zsel_n(n203, n2016, n2029);
    let n2031: ZN = zsel_n(n201, zn_splat(P8::from_raw(0i32)), n2030);
    let n2032: ZN = zsel_n(n117, n2014, r_c280);
    let n2033: ZN = zsel_n(n117, n2031, r_c281);
    let n2034: ZN = zn_sub(r_c236, zn_splat(P8::from_raw(65536i32)));
    let n2035: ZN = zn_sub(r_c238, zn_splat(P8::from_raw(65536i32)));
    let n2036: ZN = zn_sub(n287, r_c270);
    let n2037: ZN = zn_max(r_c272, n2036);
    let n2038: ZN = zn_add(n287, r_c270);
    let n2039: ZN = zn_min(r_c272, n2038);
    let n2040: ZN = zsel_n(n1058, n2037, n2039);
    let n2041: ZN = zn_sub(n288, r_c271);
    let n2042: ZN = zn_max(r_c273, n2041);
    let n2043: ZN = zn_add(n288, r_c271);
    let n2044: ZN = zn_min(r_c273, n2043);
    let n2045: ZN = zsel_n(n1060, n2042, n2044);
    let n2046: ZN = zsel_n(n1096, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2047: ZN = zn_sub(n288, n2046);
    let n2048: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2047);
    let n2049: ZN = zn_add(n288, n2046);
    let n2050: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2049);
    let n2051: ZN = zsel_n(n1099, n2048, n2050);
    let n2052: ZN = zsel_n(n1039, n2051, n288);
    let n2053: ZN = zn_neg(n1110);
    let n2054: ZN = zn_mul(n2053, zn_splat(P8::from_raw(131072i32)));
    let n2055: ZN = zsel_n(n1112, n2054, n1090);
    let n2056: ZN = zsel_n(n1112, zn_splat(P8::from_raw(-131072i32)), n2052);
    let n2057: ZN = zsel_n(n1101, zn_splat(P8::from_raw(0i32)), n1050);
    let n2058: ZN = zsel_n(n1101, n1090, n2055);
    let n2059: ZN = zsel_n(n1101, zn_splat(P8::from_raw(-131072i32)), n2056);
    let n2060: ZN = zsel_n(n1117, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2061: ZN = zsel_n(n1115, zn_splat(P8::from_raw(131072i32)), n2060);
    let n2062: ZN = zsel_n(n1120, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2063: ZB = zsel_b(n1056, r_c274, n1094);
    let n2064: ZN = zsel_n(n1183, r_c241, n1050);
    let n2065: ZB = zb_and(r_c248, n1183);
    let n2066: ZB = zb_and(r_c249, n1183);
    let n2067: ZN = zsel_n(n1183, r_c255, n285);
    let n2068: ZN = zsel_n(n1183, r_c256, n286);
    let n2069: ZB = zsel_b(n1183, r_c274, n2063);
    let n2070: ZN = zsel_n(n1183, r_c280, n2032);
    let n2071: ZN = zsel_n(n1183, r_c281, n2033);
    let n2072: ZB = zb_or(n1027, n1183);
    let n2073: ZB = zn_lt(n2067, zn_splat(P8::from_raw(-65536i32)));
    let n2074: ZB = zn_ge(n2067, zn_splat(P8::from_raw(-65536i32)));
    let n2075: ZB = zn_gt(n2067, zn_splat(P8::from_raw(7929856i32)));
    let n2076: ZB = zb_or(n2073, n2075);
    let n2077: ZB = zb_not(n2076);
    let n2078: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2067);
    let n2079: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2078);
    let n2080: ZN = zsel_n(n2076, n2079, n2067);
    let n2081: ZN = zn_sub(n1126, zn_splat(P8::from_raw(65536i32)));
    let n2082: ZN = zsel_n(n1056, n2035, r_c238);
    let n2083: ZN = zsel_n(n1056, n2040, n1090);
    let n2084: ZN = zsel_n(n1056, n2045, n2052);
    let n2085: ZB = zb_and(n1122, n1596);
    let n2086: ZN = zsel_n(n1183, n1997, r_c20);
    let n2087: ZN = zsel_n(n1183, r_c236, n2034);
    let n2088: ZN = zsel_n(n1183, r_c238, n2082);
    let n2089: ZN = zsel_n(n1183, r_c239, n1126);
    let n2090: ZN = zsel_n(n1183, r_c282, n2083);
    let n2091: ZN = zsel_n(n1183, r_c283, n2084);
    let n2092: ZB = zb_or(n1183, n2085);
    let n2093: ZB = zn_gt(n2086, zn_splat(P8::from_raw(0i32)));
    let n2094: ZB = zn_le(n2086, zn_splat(P8::from_raw(0i32)));
    let n2095: ZB = zb_and(n2092, n2093);
    let n2096: ZB = zb_and(n2092, n2094);
    let n2097: ZB = zb_and(n2074, n2096);
    let n2098: ZB = zb_and(n2073, n2096);
    let n2099: ZB = zb_or(n2097, n2098);
    let n2100: ZB = zb_and(n2076, n2099);
    let n2101: ZB = zb_and(n2077, n2099);
    let n2102: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2090);
    let n2103: ZB = zb_or(n2100, n2101);
    let n2104: ZN = zsel_n(n2093, n2067, n2080);
    let n2105: ZN = zsel_n(n2093, n2090, n2102);
    let n2106: ZB = zb_or(n2095, n2103);
    let n2108: ZN = zn_max(n1214, n2047);
    let n2109: ZN = zn_min(n1214, n2049);
    let n2110: ZN = zsel_n(n1215, n2108, n2109);
    let n2111: ZN = zsel_n(n1039, n2110, n288);
    let n2112: ZN = zsel_n(n1112, n2054, n1206);
    let n2113: ZN = zsel_n(n1112, zn_splat(P8::from_raw(-131072i32)), n2111);
    let n2114: ZN = zsel_n(n1101, n1206, n2112);
    let n2115: ZN = zsel_n(n1101, zn_splat(P8::from_raw(-131072i32)), n2113);
    let n2116: ZB = zsel_b(n1056, r_c274, n1210);
    let n2117: ZB = zsel_b(n1183, r_c274, n2116);
    let n2118: ZN = zsel_n(n1056, n2040, n1206);
    let n2119: ZN = zsel_n(n1056, n2045, n2111);
    let n2120: ZB = zb_and(n1122, n1640);
    let n2121: ZN = zsel_n(n1183, r_c282, n2118);
    let n2122: ZN = zsel_n(n1183, r_c283, n2119);
    let n2123: ZB = zb_or(n1183, n2120);
    let n2124: ZB = zb_and(n2093, n2123);
    let n2125: ZB = zb_and(n2094, n2123);
    let n2126: ZB = zb_and(n2074, n2125);
    let n2127: ZB = zb_and(n2073, n2125);
    let n2128: ZB = zb_or(n2126, n2127);
    let n2129: ZB = zb_and(n2076, n2128);
    let n2130: ZB = zb_and(n2077, n2128);
    let n2131: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2121);
    let n2132: ZB = zb_or(n2129, n2130);
    let n2133: ZN = zsel_n(n2093, n2121, n2131);
    let n2134: ZB = zb_or(n2124, n2132);
    let n2135: ZN = zn_max(n1265, n2047);
    let n2136: ZN = zn_min(n1265, n2049);
    let n2137: ZN = zsel_n(n1266, n2135, n2136);
    let n2138: ZN = zsel_n(n1039, n2137, n288);
    let n2139: ZN = zsel_n(n1112, n2054, n1257);
    let n2140: ZN = zsel_n(n1112, zn_splat(P8::from_raw(-131072i32)), n2138);
    let n2141: ZN = zsel_n(n1101, n1257, n2139);
    let n2142: ZN = zsel_n(n1101, zn_splat(P8::from_raw(-131072i32)), n2140);
    let n2143: ZB = zsel_b(n1056, r_c274, n1261);
    let n2144: ZB = zsel_b(n1183, r_c274, n2143);
    let n2145: ZN = zsel_n(n1056, n2040, n1257);
    let n2146: ZN = zsel_n(n1056, n2045, n2138);
    let n2147: ZB = zb_and(n1122, n1684);
    let n2148: ZN = zsel_n(n1183, r_c282, n2145);
    let n2149: ZN = zsel_n(n1183, r_c283, n2146);
    let n2150: ZB = zb_or(n1183, n2147);
    let n2151: ZB = zb_and(n2093, n2150);
    let n2152: ZB = zb_and(n2094, n2150);
    let n2153: ZB = zb_and(n2074, n2152);
    let n2154: ZB = zb_and(n2073, n2152);
    let n2155: ZB = zb_or(n2153, n2154);
    let n2156: ZB = zb_and(n2076, n2155);
    let n2157: ZB = zb_and(n2077, n2155);
    let n2158: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2148);
    let n2159: ZB = zb_or(n2156, n2157);
    let n2160: ZN = zsel_n(n2093, n2148, n2158);
    let n2161: ZB = zb_or(n2151, n2159);
    let n2162: ZN = zsel_n(n55, n2057, n1050);
    let n2163: ZN = zsel_n(n55, n2058, n1090);
    let n2164: ZN = zsel_n(n55, n2059, n2052);
    let n2165: ZN = zsel_n(n1056, n1050, n2162);
    let n2166: ZN = zsel_n(n1183, r_c241, n2165);
    let n2167: ZB = zb_or(r_c249, n74);
    let n2168: ZN = zsel_n(n1056, n2040, n2163);
    let n2169: ZN = zsel_n(n1056, n2045, n2164);
    let n2170: ZB = zb_and(n1122, n1727);
    let n2171: ZN = zsel_n(n1183, r_c282, n2168);
    let n2172: ZN = zsel_n(n1183, r_c283, n2169);
    let n2173: ZB = zb_or(n1183, n2170);
    let n2174: ZB = zb_and(n2093, n2173);
    let n2175: ZB = zb_and(n2094, n2173);
    let n2176: ZB = zb_and(n2074, n2175);
    let n2177: ZB = zb_and(n2073, n2175);
    let n2178: ZB = zb_or(n2176, n2177);
    let n2179: ZB = zb_and(n2076, n2178);
    let n2180: ZB = zb_and(n2077, n2178);
    let n2181: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2171);
    let n2182: ZB = zb_or(n2179, n2180);
    let n2183: ZN = zsel_n(n2093, n2171, n2181);
    let n2184: ZB = zb_or(n2174, n2182);
    let n2185: ZN = zsel_n(n55, n2114, n1206);
    let n2186: ZN = zsel_n(n55, n2115, n2111);
    let n2187: ZN = zsel_n(n1056, n2040, n2185);
    let n2188: ZN = zsel_n(n1056, n2045, n2186);
    let n2189: ZB = zb_and(n1122, n1770);
    let n2190: ZN = zsel_n(n1183, r_c282, n2187);
    let n2191: ZN = zsel_n(n1183, r_c283, n2188);
    let n2192: ZB = zb_or(n1183, n2189);
    let n2193: ZB = zb_and(n2093, n2192);
    let n2194: ZB = zb_and(n2094, n2192);
    let n2195: ZB = zb_and(n2074, n2194);
    let n2196: ZB = zb_and(n2073, n2194);
    let n2197: ZB = zb_or(n2195, n2196);
    let n2198: ZB = zb_and(n2076, n2197);
    let n2199: ZB = zb_and(n2077, n2197);
    let n2200: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2190);
    let n2201: ZB = zb_or(n2198, n2199);
    let n2202: ZN = zsel_n(n2093, n2190, n2200);
    let n2203: ZB = zb_or(n2193, n2201);
    let n2204: ZN = zsel_n(n55, n2141, n1257);
    let n2205: ZN = zsel_n(n55, n2142, n2138);
    let n2206: ZN = zsel_n(n1056, n2040, n2204);
    let n2207: ZN = zsel_n(n1056, n2045, n2205);
    let n2208: ZB = zb_and(n1122, n1813);
    let n2209: ZN = zsel_n(n1183, r_c282, n2206);
    let n2210: ZN = zsel_n(n1183, r_c283, n2207);
    let n2211: ZB = zb_or(n1183, n2208);
    let n2212: ZB = zb_and(n2093, n2211);
    let n2213: ZB = zb_and(n2094, n2211);
    let n2214: ZB = zb_and(n2074, n2213);
    let n2215: ZB = zb_and(n2073, n2213);
    let n2216: ZB = zb_or(n2214, n2215);
    let n2217: ZB = zb_and(n2076, n2216);
    let n2218: ZB = zb_and(n2077, n2216);
    let n2219: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2209);
    let n2220: ZB = zb_or(n2217, n2218);
    let n2221: ZN = zsel_n(n2093, n2209, n2219);
    let n2222: ZB = zb_or(n2212, n2220);
    let n2223: ZB = zb_or(r_c248, n74);
    let n2224: ZN = zsel_n(n1413, zn_splat(P8::from_raw(655360i32)), n2034);
    let n2225: ZN = zsel_n(n1413, zn_splat(P8::from_raw(262144i32)), r_c238);
    let n2226: ZN = zsel_n(n1413, n2081, n1126);
    let n2227: ZN = zsel_n(n1413, zn_splat(P8::from_raw(98304i32)), r_c270);
    let n2228: ZN = zsel_n(n1413, n2062, r_c271);
    let n2229: ZN = zsel_n(n1413, n2061, r_c272);
    let n2230: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), r_c273);
    let n2231: ZN = zsel_n(n1413, n1114, n1090);
    let n2232: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2052);
    let n2233: ZN = zsel_n(n1056, n2034, n2224);
    let n2234: ZN = zsel_n(n1056, n2035, n2225);
    let n2235: ZN = zsel_n(n1056, n1126, n2226);
    let n2236: ZN = zsel_n(n1056, r_c270, n2227);
    let n2237: ZN = zsel_n(n1056, r_c271, n2228);
    let n2238: ZN = zsel_n(n1056, r_c272, n2229);
    let n2239: ZN = zsel_n(n1056, r_c273, n2230);
    let n2240: ZN = zsel_n(n1056, n2040, n2231);
    let n2241: ZN = zsel_n(n1056, n2045, n2232);
    let n2242: ZB = zb_and(n1122, n1841);
    let n2243: ZN = zsel_n(n1183, n1997, n1417);
    let n2244: ZB = zsel_b(n1183, r_c41, n1418);
    let n2245: ZN = zsel_n(n1183, r_c236, n2233);
    let n2246: ZN = zsel_n(n1183, r_c238, n2234);
    let n2247: ZN = zsel_n(n1183, r_c239, n2235);
    let n2248: ZN = zsel_n(n1183, r_c270, n2236);
    let n2249: ZN = zsel_n(n1183, r_c271, n2237);
    let n2250: ZN = zsel_n(n1183, r_c272, n2238);
    let n2251: ZN = zsel_n(n1183, r_c273, n2239);
    let n2252: ZN = zsel_n(n1183, r_c282, n2240);
    let n2253: ZN = zsel_n(n1183, r_c283, n2241);
    let n2254: ZB = zb_or(n1183, n2242);
    let n2255: ZB = zn_gt(n2243, zn_splat(P8::from_raw(0i32)));
    let n2256: ZB = zn_le(n2243, zn_splat(P8::from_raw(0i32)));
    let n2257: ZB = zb_and(n2254, n2255);
    let n2258: ZB = zb_and(n2254, n2256);
    let n2259: ZB = zb_and(n2074, n2258);
    let n2260: ZB = zb_and(n2073, n2258);
    let n2261: ZB = zb_or(n2259, n2260);
    let n2262: ZB = zb_and(n2076, n2261);
    let n2263: ZB = zb_and(n2077, n2261);
    let n2264: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2252);
    let n2265: ZB = zb_or(n2262, n2263);
    let n2266: ZN = zsel_n(n2255, n2067, n2080);
    let n2267: ZN = zsel_n(n2255, n2252, n2264);
    let n2268: ZB = zb_or(n2257, n2265);
    let n2269: ZN = zsel_n(n1413, zn_splat(P8::from_raw(69510i32)), r_c271);
    let n2270: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-131072i32)), r_c272);
    let n2271: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-327680i32)), n1206);
    let n2272: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2111);
    let n2273: ZN = zsel_n(n1056, r_c271, n2269);
    let n2274: ZN = zsel_n(n1056, r_c272, n2270);
    let n2275: ZN = zsel_n(n1056, n2040, n2271);
    let n2276: ZN = zsel_n(n1056, n2045, n2272);
    let n2277: ZB = zb_and(n1122, n1859);
    let n2278: ZN = zsel_n(n1183, r_c271, n2273);
    let n2279: ZN = zsel_n(n1183, r_c272, n2274);
    let n2280: ZN = zsel_n(n1183, r_c282, n2275);
    let n2281: ZN = zsel_n(n1183, r_c283, n2276);
    let n2282: ZB = zb_or(n1183, n2277);
    let n2283: ZB = zb_and(n2255, n2282);
    let n2284: ZB = zb_and(n2256, n2282);
    let n2285: ZB = zb_and(n2074, n2284);
    let n2286: ZB = zb_and(n2073, n2284);
    let n2287: ZB = zb_or(n2285, n2286);
    let n2288: ZB = zb_and(n2076, n2287);
    let n2289: ZB = zb_and(n2077, n2287);
    let n2290: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2280);
    let n2291: ZB = zb_or(n2288, n2289);
    let n2292: ZN = zsel_n(n2255, n2280, n2290);
    let n2293: ZB = zb_or(n2283, n2291);
    let n2294: ZN = zsel_n(n1413, zn_splat(P8::from_raw(131072i32)), r_c272);
    let n2295: ZN = zsel_n(n1413, zn_splat(P8::from_raw(327680i32)), n1257);
    let n2296: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2138);
    let n2297: ZN = zsel_n(n1056, r_c272, n2294);
    let n2298: ZN = zsel_n(n1056, n2040, n2295);
    let n2299: ZN = zsel_n(n1056, n2045, n2296);
    let n2300: ZB = zb_and(n1122, n1877);
    let n2301: ZN = zsel_n(n1183, r_c272, n2297);
    let n2302: ZN = zsel_n(n1183, r_c282, n2298);
    let n2303: ZN = zsel_n(n1183, r_c283, n2299);
    let n2304: ZB = zb_or(n1183, n2300);
    let n2305: ZB = zb_and(n2255, n2304);
    let n2306: ZB = zb_and(n2256, n2304);
    let n2307: ZB = zb_and(n2074, n2306);
    let n2308: ZB = zb_and(n2073, n2306);
    let n2309: ZB = zb_or(n2307, n2308);
    let n2310: ZB = zb_and(n2076, n2309);
    let n2311: ZB = zb_and(n2077, n2309);
    let n2312: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2302);
    let n2313: ZB = zb_or(n2310, n2311);
    let n2314: ZN = zsel_n(n2255, n2302, n2312);
    let n2315: ZB = zb_or(n2305, n2313);
    let n2317: ZN = zsel_n(n1413, zn_splat(P8::from_raw(69510i32)), r_c270);
    let n2318: ZN = zsel_n(n1413, zn_splat(P8::from_raw(98304i32)), r_c271);
    let n2319: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), r_c272);
    let n2320: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-98304i32)), r_c273);
    let n2321: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n1090);
    let n2322: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-327680i32)), n2052);
    let n2323: ZN = zsel_n(n1056, r_c270, n2317);
    let n2324: ZN = zsel_n(n1056, r_c271, n2318);
    let n2325: ZN = zsel_n(n1056, r_c272, n2319);
    let n2326: ZN = zsel_n(n1056, r_c273, n2320);
    let n2327: ZN = zsel_n(n1056, n2040, n2321);
    let n2328: ZN = zsel_n(n1056, n2045, n2322);
    let n2329: ZB = zb_and(n1122, n1893);
    let n2330: ZN = zsel_n(n1183, r_c270, n2323);
    let n2331: ZN = zsel_n(n1183, r_c271, n2324);
    let n2332: ZN = zsel_n(n1183, r_c272, n2325);
    let n2333: ZN = zsel_n(n1183, r_c273, n2326);
    let n2334: ZN = zsel_n(n1183, r_c282, n2327);
    let n2335: ZN = zsel_n(n1183, r_c283, n2328);
    let n2336: ZB = zb_or(n1183, n2329);
    let n2337: ZB = zb_and(n2255, n2336);
    let n2338: ZB = zb_and(n2256, n2336);
    let n2339: ZB = zb_and(n2074, n2338);
    let n2340: ZB = zb_and(n2073, n2338);
    let n2341: ZB = zb_or(n2339, n2340);
    let n2342: ZB = zb_and(n2076, n2341);
    let n2343: ZB = zb_and(n2077, n2341);
    let n2344: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2334);
    let n2345: ZB = zb_or(n2342, n2343);
    let n2346: ZN = zsel_n(n2255, n2334, n2344);
    let n2347: ZB = zb_or(n2337, n2345);
    let n2348: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n1206);
    let n2349: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n2111);
    let n2350: ZN = zsel_n(n1056, n2040, n2348);
    let n2351: ZN = zsel_n(n1056, n2045, n2349);
    let n2352: ZN = zsel_n(n1183, r_c282, n2350);
    let n2353: ZN = zsel_n(n1183, r_c283, n2351);
    let n2354: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2352);
    let n2355: ZN = zsel_n(n2255, n2352, n2354);
    let n2356: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n1257);
    let n2357: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n2138);
    let n2358: ZN = zsel_n(n1056, n2040, n2356);
    let n2359: ZN = zsel_n(n1056, n2045, n2357);
    let n2360: ZN = zsel_n(n1183, r_c282, n2358);
    let n2361: ZN = zsel_n(n1183, r_c283, n2359);
    let n2362: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2360);
    let n2363: ZN = zsel_n(n2255, n2360, n2362);
    let n2364: ZN = zsel_n(n1413, zn_splat(P8::from_raw(131072i32)), r_c273);
    let n2365: ZN = zsel_n(n1413, zn_splat(P8::from_raw(327680i32)), n2052);
    let n2366: ZN = zsel_n(n1056, r_c273, n2364);
    let n2367: ZN = zsel_n(n1056, n2045, n2365);
    let n2368: ZN = zsel_n(n1183, r_c273, n2366);
    let n2369: ZN = zsel_n(n1183, r_c283, n2367);
    let n2370: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n2111);
    let n2371: ZN = zsel_n(n1056, n2045, n2370);
    let n2372: ZN = zsel_n(n1183, r_c283, n2371);
    let n2373: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n2138);
    let n2374: ZN = zsel_n(n1056, n2045, n2373);
    let n2375: ZN = zsel_n(n1183, r_c283, n2374);
    let n2376: ZN = zsel_n(n1413, n1114, n2163);
    let n2377: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2164);
    let n2378: ZN = zsel_n(n1056, n2040, n2376);
    let n2379: ZN = zsel_n(n1056, n2045, n2377);
    let n2380: ZB = zb_and(n1122, n1923);
    let n2381: ZN = zsel_n(n1183, r_c282, n2378);
    let n2382: ZN = zsel_n(n1183, r_c283, n2379);
    let n2383: ZB = zb_or(n1183, n2380);
    let n2384: ZB = zb_and(n2255, n2383);
    let n2385: ZB = zb_and(n2256, n2383);
    let n2386: ZB = zb_and(n2074, n2385);
    let n2387: ZB = zb_and(n2073, n2385);
    let n2388: ZB = zb_or(n2386, n2387);
    let n2389: ZB = zb_and(n2076, n2388);
    let n2390: ZB = zb_and(n2077, n2388);
    let n2391: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2381);
    let n2392: ZB = zb_or(n2389, n2390);
    let n2393: ZN = zsel_n(n2255, n2381, n2391);
    let n2394: ZB = zb_or(n2384, n2392);
    let n2395: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-327680i32)), n2185);
    let n2396: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2186);
    let n2397: ZN = zsel_n(n1056, n2040, n2395);
    let n2398: ZN = zsel_n(n1056, n2045, n2396);
    let n2399: ZB = zb_and(n1122, n1941);
    let n2400: ZN = zsel_n(n1183, r_c282, n2397);
    let n2401: ZN = zsel_n(n1183, r_c283, n2398);
    let n2402: ZB = zb_or(n1183, n2399);
    let n2403: ZB = zb_and(n2255, n2402);
    let n2404: ZB = zb_and(n2256, n2402);
    let n2405: ZB = zb_and(n2074, n2404);
    let n2406: ZB = zb_and(n2073, n2404);
    let n2407: ZB = zb_or(n2405, n2406);
    let n2408: ZB = zb_and(n2076, n2407);
    let n2409: ZB = zb_and(n2077, n2407);
    let n2410: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2400);
    let n2411: ZB = zb_or(n2408, n2409);
    let n2412: ZN = zsel_n(n2255, n2400, n2410);
    let n2413: ZB = zb_or(n2403, n2411);
    let n2414: ZN = zsel_n(n1413, zn_splat(P8::from_raw(327680i32)), n2204);
    let n2415: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2205);
    let n2416: ZN = zsel_n(n1056, n2040, n2414);
    let n2417: ZN = zsel_n(n1056, n2045, n2415);
    let n2418: ZB = zb_and(n1122, n1959);
    let n2419: ZN = zsel_n(n1183, r_c282, n2416);
    let n2420: ZN = zsel_n(n1183, r_c283, n2417);
    let n2421: ZB = zb_or(n1183, n2418);
    let n2422: ZB = zb_and(n2255, n2421);
    let n2423: ZB = zb_and(n2256, n2421);
    let n2424: ZB = zb_and(n2074, n2423);
    let n2425: ZB = zb_and(n2073, n2423);
    let n2426: ZB = zb_or(n2424, n2425);
    let n2427: ZB = zb_and(n2076, n2426);
    let n2428: ZB = zb_and(n2077, n2426);
    let n2429: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2419);
    let n2430: ZB = zb_or(n2427, n2428);
    let n2431: ZN = zsel_n(n2255, n2419, n2429);
    let n2432: ZB = zb_or(n2422, n2430);
    let n2433: ZN = zsel_n(n1413, zn_splat(P8::from_raw(0i32)), n2163);
    let n2434: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-327680i32)), n2164);
    let n2435: ZN = zsel_n(n1056, n2040, n2433);
    let n2436: ZN = zsel_n(n1056, n2045, n2434);
    let n2437: ZB = zb_and(n1122, n1975);
    let n2438: ZN = zsel_n(n1183, r_c282, n2435);
    let n2439: ZN = zsel_n(n1183, r_c283, n2436);
    let n2440: ZB = zb_or(n1183, n2437);
    let n2441: ZB = zb_and(n2255, n2440);
    let n2442: ZB = zb_and(n2256, n2440);
    let n2443: ZB = zb_and(n2074, n2442);
    let n2444: ZB = zb_and(n2073, n2442);
    let n2445: ZB = zb_or(n2443, n2444);
    let n2446: ZB = zb_and(n2076, n2445);
    let n2447: ZB = zb_and(n2077, n2445);
    let n2448: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2438);
    let n2449: ZB = zb_or(n2446, n2447);
    let n2450: ZN = zsel_n(n2255, n2438, n2448);
    let n2451: ZB = zb_or(n2441, n2449);
    let n2452: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n2185);
    let n2453: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n2186);
    let n2454: ZN = zsel_n(n1056, n2040, n2452);
    let n2455: ZN = zsel_n(n1056, n2045, n2453);
    let n2456: ZN = zsel_n(n1183, r_c282, n2454);
    let n2457: ZN = zsel_n(n1183, r_c283, n2455);
    let n2458: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2456);
    let n2459: ZN = zsel_n(n2255, n2456, n2458);
    let n2460: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n2204);
    let n2461: ZN = zsel_n(n1413, zn_splat(P8::from_raw(-231700i32)), n2205);
    let n2462: ZN = zsel_n(n1056, n2040, n2460);
    let n2463: ZN = zsel_n(n1056, n2045, n2461);
    let n2464: ZN = zsel_n(n1183, r_c282, n2462);
    let n2465: ZN = zsel_n(n1183, r_c283, n2463);
    let n2466: ZN = zsel_n(n2076, zn_splat(P8::from_raw(0i32)), n2464);
    let n2467: ZN = zsel_n(n2255, n2464, n2466);
    let n2468: ZN = zsel_n(n1413, zn_splat(P8::from_raw(327680i32)), n2164);
    let n2469: ZN = zsel_n(n1056, n2045, n2468);
    let n2470: ZN = zsel_n(n1183, r_c283, n2469);
    let n2471: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n2186);
    let n2472: ZN = zsel_n(n1056, n2045, n2471);
    let n2473: ZN = zsel_n(n1183, r_c283, n2472);
    let n2474: ZN = zsel_n(n1413, zn_splat(P8::from_raw(231700i32)), n2205);
    let n2475: ZN = zsel_n(n1056, n2045, n2474);
    let n2476: ZN = zsel_n(n1183, r_c283, n2475);
    let n2479: ZW = zw_bits_n(n63);
    let n2480: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2479, 84u64);
    let n2481: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2479, 84u64);
    let n2482: ZW = zw_bits_n(n115);
    let n2483: ZW = zw_mix1(n2480, n2482, 85u64);
    let n2484: ZW = zw_mix2(n2481, n2482, 85u64);
    let n2485: ZW = zw_bits_n(n114);
    let n2486: ZW = zw_mix1(n2483, n2485, 86u64);
    let n2487: ZW = zw_mix2(n2484, n2485, 86u64);
    let n2488: ZW = zw_bits_n(n1199);
    let n2489: ZW = zw_mix1(n2486, n2488, 87u64);
    let n2490: ZW = zw_mix2(n2487, n2488, 87u64);
    let n2491: ZW = zw_bits_n(r_c20);
    let n2492: ZW = zw_mix1(n2489, n2491, 20u64);
    let n2493: ZW = zw_mix2(n2490, n2491, 20u64);
    let n2494: ZW = zw_bits_b(r_c41);
    let n2495: ZW = zw_mix1(n2492, n2494, 41u64);
    let n2496: ZW = zw_mix2(n2493, n2494, 41u64);
    let n2497: ZW = zw_bits_n(n1417);
    let n2498: ZW = zw_mix1(n2489, n2497, 20u64);
    let n2499: ZW = zw_mix2(n2490, n2497, 20u64);
    let n2500: ZW = zw_bits_b(n1418);
    let n2501: ZW = zw_mix1(n2498, n2500, 41u64);
    let n2502: ZW = zw_mix2(n2499, n2500, 41u64);
    let n2503: ZW = zw_mix1(n2486, n2491, 20u64);
    let n2504: ZW = zw_mix2(n2487, n2491, 20u64);
    let n2505: ZW = zw_bits_b(n1599);
    let n2506: ZW = zw_mix1(n2503, n2505, 38u64);
    let n2507: ZW = zw_mix2(n2504, n2505, 38u64);
    let n2508: ZW = zw_bits_n(n1604);
    let n2509: ZW = zw_mix1(n2506, n2508, 39u64);
    let n2510: ZW = zw_mix2(n2507, n2508, 39u64);
    let n2511: ZW = zw_bits_n(n1603);
    let n2512: ZW = zw_mix1(n2509, n2511, 87u64);
    let n2513: ZW = zw_mix2(n2510, n2511, 87u64);
    let n2514: ZW = zw_bits_b(n1643);
    let n2515: ZW = zw_mix1(n2503, n2514, 38u64);
    let n2516: ZW = zw_mix2(n2504, n2514, 38u64);
    let n2517: ZW = zw_bits_n(n1648);
    let n2518: ZW = zw_mix1(n2515, n2517, 39u64);
    let n2519: ZW = zw_mix2(n2516, n2517, 39u64);
    let n2520: ZW = zw_bits_n(n1647);
    let n2521: ZW = zw_mix1(n2518, n2520, 87u64);
    let n2522: ZW = zw_mix2(n2519, n2520, 87u64);
    let n2523: ZW = zw_bits_b(n1687);
    let n2524: ZW = zw_mix1(n2503, n2523, 38u64);
    let n2525: ZW = zw_mix2(n2504, n2523, 38u64);
    let n2526: ZW = zw_bits_n(n1692);
    let n2527: ZW = zw_mix1(n2524, n2526, 39u64);
    let n2528: ZW = zw_mix2(n2525, n2526, 39u64);
    let n2529: ZW = zw_bits_n(n1691);
    let n2530: ZW = zw_mix1(n2527, n2529, 87u64);
    let n2531: ZW = zw_mix2(n2528, n2529, 87u64);
    let n2532: ZW = zw_bits_b(n1730);
    let n2533: ZW = zw_mix1(n2503, n2532, 38u64);
    let n2534: ZW = zw_mix2(n2504, n2532, 38u64);
    let n2535: ZW = zw_bits_n(n1735);
    let n2536: ZW = zw_mix1(n2533, n2535, 39u64);
    let n2537: ZW = zw_mix2(n2534, n2535, 39u64);
    let n2538: ZW = zw_bits_n(n1734);
    let n2539: ZW = zw_mix1(n2536, n2538, 87u64);
    let n2540: ZW = zw_mix2(n2537, n2538, 87u64);
    let n2541: ZW = zw_bits_b(n1773);
    let n2542: ZW = zw_mix1(n2503, n2541, 38u64);
    let n2543: ZW = zw_mix2(n2504, n2541, 38u64);
    let n2544: ZW = zw_bits_n(n1778);
    let n2545: ZW = zw_mix1(n2542, n2544, 39u64);
    let n2546: ZW = zw_mix2(n2543, n2544, 39u64);
    let n2547: ZW = zw_bits_n(n1777);
    let n2548: ZW = zw_mix1(n2545, n2547, 87u64);
    let n2549: ZW = zw_mix2(n2546, n2547, 87u64);
    let n2550: ZW = zw_bits_b(n1816);
    let n2551: ZW = zw_mix1(n2503, n2550, 38u64);
    let n2552: ZW = zw_mix2(n2504, n2550, 38u64);
    let n2553: ZW = zw_bits_n(n1821);
    let n2554: ZW = zw_mix1(n2551, n2553, 39u64);
    let n2555: ZW = zw_mix2(n2552, n2553, 39u64);
    let n2556: ZW = zw_bits_n(n1820);
    let n2557: ZW = zw_mix1(n2554, n2556, 87u64);
    let n2558: ZW = zw_mix2(n2555, n2556, 87u64);
    let n2559: ZW = zw_mix1(n2486, n2497, 20u64);
    let n2560: ZW = zw_mix2(n2487, n2497, 20u64);
    let n2561: ZW = zw_bits_b(n1844);
    let n2562: ZW = zw_mix1(n2559, n2561, 38u64);
    let n2563: ZW = zw_mix2(n2560, n2561, 38u64);
    let n2564: ZW = zw_bits_n(n1851);
    let n2565: ZW = zw_mix1(n2562, n2564, 39u64);
    let n2566: ZW = zw_mix2(n2563, n2564, 39u64);
    let n2567: ZW = zw_bits_n(n1850);
    let n2568: ZW = zw_mix1(n2565, n2567, 87u64);
    let n2569: ZW = zw_mix2(n2566, n2567, 87u64);
    let n2570: ZW = zw_bits_b(n1862);
    let n2571: ZW = zw_mix1(n2559, n2570, 38u64);
    let n2572: ZW = zw_mix2(n2560, n2570, 38u64);
    let n2573: ZW = zw_bits_n(n1869);
    let n2574: ZW = zw_mix1(n2571, n2573, 39u64);
    let n2575: ZW = zw_mix2(n2572, n2573, 39u64);
    let n2576: ZW = zw_bits_n(n1868);
    let n2577: ZW = zw_mix1(n2574, n2576, 87u64);
    let n2578: ZW = zw_mix2(n2575, n2576, 87u64);
    let n2579: ZW = zw_bits_b(n1880);
    let n2580: ZW = zw_mix1(n2559, n2579, 38u64);
    let n2581: ZW = zw_mix2(n2560, n2579, 38u64);
    let n2582: ZW = zw_bits_n(n1887);
    let n2583: ZW = zw_mix1(n2580, n2582, 39u64);
    let n2584: ZW = zw_mix2(n2581, n2582, 39u64);
    let n2585: ZW = zw_bits_n(n1886);
    let n2586: ZW = zw_mix1(n2583, n2585, 87u64);
    let n2587: ZW = zw_mix2(n2584, n2585, 87u64);
    let n2588: ZW = zw_bits_b(n1896);
    let n2589: ZW = zw_mix1(n2559, n2588, 38u64);
    let n2590: ZW = zw_mix2(n2560, n2588, 38u64);
    let n2591: ZW = zw_bits_n(n1903);
    let n2592: ZW = zw_mix1(n2589, n2591, 39u64);
    let n2593: ZW = zw_mix2(n2590, n2591, 39u64);
    let n2594: ZW = zw_bits_n(n1902);
    let n2595: ZW = zw_mix1(n2592, n2594, 87u64);
    let n2596: ZW = zw_mix2(n2593, n2594, 87u64);
    let n2597: ZW = zw_bits_b(n1926);
    let n2598: ZW = zw_mix1(n2559, n2597, 38u64);
    let n2599: ZW = zw_mix2(n2560, n2597, 38u64);
    let n2600: ZW = zw_bits_n(n1933);
    let n2601: ZW = zw_mix1(n2598, n2600, 39u64);
    let n2602: ZW = zw_mix2(n2599, n2600, 39u64);
    let n2603: ZW = zw_bits_n(n1932);
    let n2604: ZW = zw_mix1(n2601, n2603, 87u64);
    let n2605: ZW = zw_mix2(n2602, n2603, 87u64);
    let n2606: ZW = zw_bits_b(n1944);
    let n2607: ZW = zw_mix1(n2559, n2606, 38u64);
    let n2608: ZW = zw_mix2(n2560, n2606, 38u64);
    let n2609: ZW = zw_bits_n(n1951);
    let n2610: ZW = zw_mix1(n2607, n2609, 39u64);
    let n2611: ZW = zw_mix2(n2608, n2609, 39u64);
    let n2612: ZW = zw_bits_n(n1950);
    let n2613: ZW = zw_mix1(n2610, n2612, 87u64);
    let n2614: ZW = zw_mix2(n2611, n2612, 87u64);
    let n2615: ZW = zw_bits_b(n1962);
    let n2616: ZW = zw_mix1(n2559, n2615, 38u64);
    let n2617: ZW = zw_mix2(n2560, n2615, 38u64);
    let n2618: ZW = zw_bits_n(n1969);
    let n2619: ZW = zw_mix1(n2616, n2618, 39u64);
    let n2620: ZW = zw_mix2(n2617, n2618, 39u64);
    let n2621: ZW = zw_bits_n(n1968);
    let n2622: ZW = zw_mix1(n2619, n2621, 87u64);
    let n2623: ZW = zw_mix2(n2620, n2621, 87u64);
    let n2624: ZW = zw_bits_b(n1978);
    let n2625: ZW = zw_mix1(n2559, n2624, 38u64);
    let n2626: ZW = zw_mix2(n2560, n2624, 38u64);
    let n2627: ZW = zw_bits_n(n1985);
    let n2628: ZW = zw_mix1(n2625, n2627, 39u64);
    let n2629: ZW = zw_mix2(n2626, n2627, 39u64);
    let n2630: ZW = zw_bits_n(n1984);
    let n2631: ZW = zw_mix1(n2628, n2630, 87u64);
    let n2632: ZW = zw_mix2(n2629, n2630, 87u64);
    let n2633: ZW = zw_bits_n(r_c39);
    let n2634: ZW = zw_mix1(zw_splat(11400714819323198485u64), n2633, 39u64);
    let n2635: ZW = zw_mix2(zw_splat(11562461410679940143u64), n2633, 39u64);
    let n2636: ZW = zw_mix1(n2634, n2479, 84u64);
    let n2637: ZW = zw_mix2(n2635, n2479, 84u64);
    let n2638: ZW = zw_mix1(n2636, n2482, 85u64);
    let n2639: ZW = zw_mix2(n2637, n2482, 85u64);
    let n2640: ZW = zw_mix1(n2638, n2485, 86u64);
    let n2641: ZW = zw_mix2(n2639, n2485, 86u64);
    let n2642: ZW = zw_bits_n(r_c87);
    let n2643: ZW = zw_mix1(n2640, n2642, 87u64);
    let n2644: ZW = zw_mix2(n2641, n2642, 87u64);
    let n2645: ZW = zw_bits_n(n2068);
    let n2646: ZW = zw_mix1(n2643, n2645, 256u64);
    let n2647: ZW = zw_mix2(n2644, n2645, 256u64);
    let n2648: ZW = zw_bits_n(n2070);
    let n2649: ZW = zw_mix1(n2646, n2648, 280u64);
    let n2650: ZW = zw_mix2(n2647, n2648, 280u64);
    let n2651: ZW = zw_bits_n(n2071);
    let n2652: ZW = zw_mix1(n2649, n2651, 281u64);
    let n2653: ZW = zw_mix2(n2650, n2651, 281u64);
    let n2654: ZW = zw_bits_n(n2086);
    let n2655: ZW = zw_mix1(n2652, n2654, 20u64);
    let n2656: ZW = zw_mix2(n2653, n2654, 20u64);
    let n2657: ZW = zw_mix1(n2655, n2494, 41u64);
    let n2658: ZW = zw_mix2(n2656, n2494, 41u64);
    let n2659: ZW = zw_bits_n(n2087);
    let n2660: ZW = zw_mix1(n2657, n2659, 236u64);
    let n2661: ZW = zw_mix2(n2658, n2659, 236u64);
    let n2662: ZW = zw_bits_n(n2088);
    let n2663: ZW = zw_mix1(n2660, n2662, 238u64);
    let n2664: ZW = zw_mix2(n2661, n2662, 238u64);
    let n2665: ZW = zw_bits_n(n2089);
    let n2666: ZW = zw_mix1(n2663, n2665, 239u64);
    let n2667: ZW = zw_mix2(n2664, n2665, 239u64);
    let n2668: ZW = zw_bits_n(n2064);
    let n2669: ZW = zw_mix1(n2666, n2668, 241u64);
    let n2670: ZW = zw_mix2(n2667, n2668, 241u64);
    let n2671: ZW = zw_bits_b(n2065);
    let n2672: ZW = zw_mix1(n2669, n2671, 248u64);
    let n2673: ZW = zw_mix2(n2670, n2671, 248u64);
    let n2674: ZW = zw_bits_b(n2066);
    let n2675: ZW = zw_mix1(n2672, n2674, 249u64);
    let n2676: ZW = zw_mix2(n2673, n2674, 249u64);
    let n2677: ZW = zw_bits_n(n2104);
    let n2678: ZW = zw_mix1(n2675, n2677, 255u64);
    let n2679: ZW = zw_mix2(n2676, n2677, 255u64);
    let n2680: ZW = zw_bits_n(r_c270);
    let n2681: ZW = zw_mix1(n2678, n2680, 270u64);
    let n2682: ZW = zw_mix2(n2679, n2680, 270u64);
    let n2683: ZW = zw_bits_n(r_c271);
    let n2684: ZW = zw_mix1(n2681, n2683, 271u64);
    let n2685: ZW = zw_mix2(n2682, n2683, 271u64);
    let n2686: ZW = zw_bits_n(r_c272);
    let n2687: ZW = zw_mix1(n2684, n2686, 272u64);
    let n2688: ZW = zw_mix2(n2685, n2686, 272u64);
    let n2689: ZW = zw_bits_n(r_c273);
    let n2690: ZW = zw_mix1(n2687, n2689, 273u64);
    let n2691: ZW = zw_mix2(n2688, n2689, 273u64);
    let n2692: ZW = zw_bits_b(n2069);
    let n2693: ZW = zw_mix1(n2690, n2692, 274u64);
    let n2694: ZW = zw_mix2(n2691, n2692, 274u64);
    let n2695: ZW = zw_bits_n(n2105);
    let n2696: ZW = zw_mix1(n2693, n2695, 282u64);
    let n2697: ZW = zw_mix2(n2694, n2695, 282u64);
    let n2698: ZW = zw_bits_n(n2091);
    let n2699: ZW = zw_mix1(n2696, n2698, 283u64);
    let n2700: ZW = zw_mix2(n2697, n2698, 283u64);
    let n2701: ZW = zw_bits_b(n2117);
    let n2702: ZW = zw_mix1(n2690, n2701, 274u64);
    let n2703: ZW = zw_mix2(n2691, n2701, 274u64);
    let n2704: ZW = zw_bits_n(n2133);
    let n2705: ZW = zw_mix1(n2702, n2704, 282u64);
    let n2706: ZW = zw_mix2(n2703, n2704, 282u64);
    let n2707: ZW = zw_bits_n(n2122);
    let n2708: ZW = zw_mix1(n2705, n2707, 283u64);
    let n2709: ZW = zw_mix2(n2706, n2707, 283u64);
    let n2710: ZW = zw_bits_b(n2144);
    let n2711: ZW = zw_mix1(n2690, n2710, 274u64);
    let n2712: ZW = zw_mix2(n2691, n2710, 274u64);
    let n2713: ZW = zw_bits_n(n2160);
    let n2714: ZW = zw_mix1(n2711, n2713, 282u64);
    let n2715: ZW = zw_mix2(n2712, n2713, 282u64);
    let n2716: ZW = zw_bits_n(n2149);
    let n2717: ZW = zw_mix1(n2714, n2716, 283u64);
    let n2718: ZW = zw_mix2(n2715, n2716, 283u64);
    let n2719: ZW = zw_bits_n(n2166);
    let n2720: ZW = zw_mix1(n2666, n2719, 241u64);
    let n2721: ZW = zw_mix2(n2667, n2719, 241u64);
    let n2722: ZW = zw_mix1(n2720, n2671, 248u64);
    let n2723: ZW = zw_mix2(n2721, n2671, 248u64);
    let n2724: ZW = zw_bits_b(n2167);
    let n2725: ZW = zw_mix1(n2722, n2724, 249u64);
    let n2726: ZW = zw_mix2(n2723, n2724, 249u64);
    let n2727: ZW = zw_mix1(n2725, n2677, 255u64);
    let n2728: ZW = zw_mix2(n2726, n2677, 255u64);
    let n2729: ZW = zw_mix1(n2727, n2680, 270u64);
    let n2730: ZW = zw_mix2(n2728, n2680, 270u64);
    let n2731: ZW = zw_mix1(n2729, n2683, 271u64);
    let n2732: ZW = zw_mix2(n2730, n2683, 271u64);
    let n2733: ZW = zw_mix1(n2731, n2686, 272u64);
    let n2734: ZW = zw_mix2(n2732, n2686, 272u64);
    let n2735: ZW = zw_mix1(n2733, n2689, 273u64);
    let n2736: ZW = zw_mix2(n2734, n2689, 273u64);
    let n2737: ZW = zw_mix1(n2735, n2692, 274u64);
    let n2738: ZW = zw_mix2(n2736, n2692, 274u64);
    let n2739: ZW = zw_bits_n(n2183);
    let n2740: ZW = zw_mix1(n2737, n2739, 282u64);
    let n2741: ZW = zw_mix2(n2738, n2739, 282u64);
    let n2742: ZW = zw_bits_n(n2172);
    let n2743: ZW = zw_mix1(n2740, n2742, 283u64);
    let n2744: ZW = zw_mix2(n2741, n2742, 283u64);
    let n2745: ZW = zw_mix1(n2735, n2701, 274u64);
    let n2746: ZW = zw_mix2(n2736, n2701, 274u64);
    let n2747: ZW = zw_bits_n(n2202);
    let n2748: ZW = zw_mix1(n2745, n2747, 282u64);
    let n2749: ZW = zw_mix2(n2746, n2747, 282u64);
    let n2750: ZW = zw_bits_n(n2191);
    let n2751: ZW = zw_mix1(n2748, n2750, 283u64);
    let n2752: ZW = zw_mix2(n2749, n2750, 283u64);
    let n2753: ZW = zw_mix1(n2735, n2710, 274u64);
    let n2754: ZW = zw_mix2(n2736, n2710, 274u64);
    let n2755: ZW = zw_bits_n(n2221);
    let n2756: ZW = zw_mix1(n2753, n2755, 282u64);
    let n2757: ZW = zw_mix2(n2754, n2755, 282u64);
    let n2758: ZW = zw_bits_n(n2210);
    let n2759: ZW = zw_mix1(n2756, n2758, 283u64);
    let n2760: ZW = zw_mix2(n2757, n2758, 283u64);
    let n2761: ZW = zw_bits_n(n2243);
    let n2762: ZW = zw_mix1(n2652, n2761, 20u64);
    let n2763: ZW = zw_mix2(n2653, n2761, 20u64);
    let n2764: ZW = zw_bits_b(n2244);
    let n2765: ZW = zw_mix1(n2762, n2764, 41u64);
    let n2766: ZW = zw_mix2(n2763, n2764, 41u64);
    let n2767: ZW = zw_bits_n(n2245);
    let n2768: ZW = zw_mix1(n2765, n2767, 236u64);
    let n2769: ZW = zw_mix2(n2766, n2767, 236u64);
    let n2770: ZW = zw_bits_n(n2246);
    let n2771: ZW = zw_mix1(n2768, n2770, 238u64);
    let n2772: ZW = zw_mix2(n2769, n2770, 238u64);
    let n2773: ZW = zw_bits_n(n2247);
    let n2774: ZW = zw_mix1(n2771, n2773, 239u64);
    let n2775: ZW = zw_mix2(n2772, n2773, 239u64);
    let n2776: ZW = zw_mix1(n2774, n2668, 241u64);
    let n2777: ZW = zw_mix2(n2775, n2668, 241u64);
    let n2778: ZW = zw_bits_b(n2223);
    let n2779: ZW = zw_mix1(n2776, n2778, 248u64);
    let n2780: ZW = zw_mix2(n2777, n2778, 248u64);
    let n2781: ZW = zw_mix1(n2779, n2674, 249u64);
    let n2782: ZW = zw_mix2(n2780, n2674, 249u64);
    let n2783: ZW = zw_bits_n(n2266);
    let n2784: ZW = zw_mix1(n2781, n2783, 255u64);
    let n2785: ZW = zw_mix2(n2782, n2783, 255u64);
    let n2786: ZW = zw_bits_n(n2248);
    let n2787: ZW = zw_mix1(n2784, n2786, 270u64);
    let n2788: ZW = zw_mix2(n2785, n2786, 270u64);
    let n2789: ZW = zw_bits_n(n2249);
    let n2790: ZW = zw_mix1(n2787, n2789, 271u64);
    let n2791: ZW = zw_mix2(n2788, n2789, 271u64);
    let n2792: ZW = zw_bits_n(n2250);
    let n2793: ZW = zw_mix1(n2790, n2792, 272u64);
    let n2794: ZW = zw_mix2(n2791, n2792, 272u64);
    let n2795: ZW = zw_bits_n(n2251);
    let n2796: ZW = zw_mix1(n2793, n2795, 273u64);
    let n2797: ZW = zw_mix2(n2794, n2795, 273u64);
    let n2798: ZW = zw_mix1(n2796, n2692, 274u64);
    let n2799: ZW = zw_mix2(n2797, n2692, 274u64);
    let n2800: ZW = zw_bits_n(n2267);
    let n2801: ZW = zw_mix1(n2798, n2800, 282u64);
    let n2802: ZW = zw_mix2(n2799, n2800, 282u64);
    let n2803: ZW = zw_bits_n(n2253);
    let n2804: ZW = zw_mix1(n2801, n2803, 283u64);
    let n2805: ZW = zw_mix2(n2802, n2803, 283u64);
    let n2806: ZW = zw_bits_n(n2278);
    let n2807: ZW = zw_mix1(n2787, n2806, 271u64);
    let n2808: ZW = zw_mix2(n2788, n2806, 271u64);
    let n2809: ZW = zw_bits_n(n2279);
    let n2810: ZW = zw_mix1(n2807, n2809, 272u64);
    let n2811: ZW = zw_mix2(n2808, n2809, 272u64);
    let n2812: ZW = zw_mix1(n2810, n2795, 273u64);
    let n2813: ZW = zw_mix2(n2811, n2795, 273u64);
    let n2814: ZW = zw_mix1(n2812, n2701, 274u64);
    let n2815: ZW = zw_mix2(n2813, n2701, 274u64);
    let n2816: ZW = zw_bits_n(n2292);
    let n2817: ZW = zw_mix1(n2814, n2816, 282u64);
    let n2818: ZW = zw_mix2(n2815, n2816, 282u64);
    let n2819: ZW = zw_bits_n(n2281);
    let n2820: ZW = zw_mix1(n2817, n2819, 283u64);
    let n2821: ZW = zw_mix2(n2818, n2819, 283u64);
    let n2822: ZW = zw_bits_n(n2301);
    let n2823: ZW = zw_mix1(n2807, n2822, 272u64);
    let n2824: ZW = zw_mix2(n2808, n2822, 272u64);
    let n2825: ZW = zw_mix1(n2823, n2795, 273u64);
    let n2826: ZW = zw_mix2(n2824, n2795, 273u64);
    let n2827: ZW = zw_mix1(n2825, n2710, 274u64);
    let n2828: ZW = zw_mix2(n2826, n2710, 274u64);
    let n2829: ZW = zw_bits_n(n2314);
    let n2830: ZW = zw_mix1(n2827, n2829, 282u64);
    let n2831: ZW = zw_mix2(n2828, n2829, 282u64);
    let n2832: ZW = zw_bits_n(n2303);
    let n2833: ZW = zw_mix1(n2830, n2832, 283u64);
    let n2834: ZW = zw_mix2(n2831, n2832, 283u64);
    let n2835: ZW = zw_bits_n(n2330);
    let n2836: ZW = zw_mix1(n2784, n2835, 270u64);
    let n2837: ZW = zw_mix2(n2785, n2835, 270u64);
    let n2838: ZW = zw_bits_n(n2331);
    let n2839: ZW = zw_mix1(n2836, n2838, 271u64);
    let n2840: ZW = zw_mix2(n2837, n2838, 271u64);
    let n2841: ZW = zw_bits_n(n2332);
    let n2842: ZW = zw_mix1(n2839, n2841, 272u64);
    let n2843: ZW = zw_mix2(n2840, n2841, 272u64);
    let n2844: ZW = zw_bits_n(n2333);
    let n2845: ZW = zw_mix1(n2842, n2844, 273u64);
    let n2846: ZW = zw_mix2(n2843, n2844, 273u64);
    let n2847: ZW = zw_mix1(n2845, n2692, 274u64);
    let n2848: ZW = zw_mix2(n2846, n2692, 274u64);
    let n2849: ZW = zw_bits_n(n2346);
    let n2850: ZW = zw_mix1(n2847, n2849, 282u64);
    let n2851: ZW = zw_mix2(n2848, n2849, 282u64);
    let n2852: ZW = zw_bits_n(n2335);
    let n2853: ZW = zw_mix1(n2850, n2852, 283u64);
    let n2854: ZW = zw_mix2(n2851, n2852, 283u64);
    let n2855: ZW = zw_mix1(n2836, n2806, 271u64);
    let n2856: ZW = zw_mix2(n2837, n2806, 271u64);
    let n2857: ZW = zw_mix1(n2855, n2809, 272u64);
    let n2858: ZW = zw_mix2(n2856, n2809, 272u64);
    let n2859: ZW = zw_mix1(n2857, n2844, 273u64);
    let n2860: ZW = zw_mix2(n2858, n2844, 273u64);
    let n2861: ZW = zw_mix1(n2859, n2701, 274u64);
    let n2862: ZW = zw_mix2(n2860, n2701, 274u64);
    let n2863: ZW = zw_bits_n(n2355);
    let n2864: ZW = zw_mix1(n2861, n2863, 282u64);
    let n2865: ZW = zw_mix2(n2862, n2863, 282u64);
    let n2866: ZW = zw_bits_n(n2353);
    let n2867: ZW = zw_mix1(n2864, n2866, 283u64);
    let n2868: ZW = zw_mix2(n2865, n2866, 283u64);
    let n2869: ZW = zw_mix1(n2855, n2822, 272u64);
    let n2870: ZW = zw_mix2(n2856, n2822, 272u64);
    let n2871: ZW = zw_mix1(n2869, n2844, 273u64);
    let n2872: ZW = zw_mix2(n2870, n2844, 273u64);
    let n2873: ZW = zw_mix1(n2871, n2710, 274u64);
    let n2874: ZW = zw_mix2(n2872, n2710, 274u64);
    let n2875: ZW = zw_bits_n(n2363);
    let n2876: ZW = zw_mix1(n2873, n2875, 282u64);
    let n2877: ZW = zw_mix2(n2874, n2875, 282u64);
    let n2878: ZW = zw_bits_n(n2361);
    let n2879: ZW = zw_mix1(n2876, n2878, 283u64);
    let n2880: ZW = zw_mix2(n2877, n2878, 283u64);
    let n2881: ZW = zw_bits_n(n2368);
    let n2882: ZW = zw_mix1(n2842, n2881, 273u64);
    let n2883: ZW = zw_mix2(n2843, n2881, 273u64);
    let n2884: ZW = zw_mix1(n2882, n2692, 274u64);
    let n2885: ZW = zw_mix2(n2883, n2692, 274u64);
    let n2886: ZW = zw_mix1(n2884, n2849, 282u64);
    let n2887: ZW = zw_mix2(n2885, n2849, 282u64);
    let n2888: ZW = zw_bits_n(n2369);
    let n2889: ZW = zw_mix1(n2886, n2888, 283u64);
    let n2890: ZW = zw_mix2(n2887, n2888, 283u64);
    let n2891: ZW = zw_mix1(n2857, n2881, 273u64);
    let n2892: ZW = zw_mix2(n2858, n2881, 273u64);
    let n2893: ZW = zw_mix1(n2891, n2701, 274u64);
    let n2894: ZW = zw_mix2(n2892, n2701, 274u64);
    let n2895: ZW = zw_mix1(n2893, n2863, 282u64);
    let n2896: ZW = zw_mix2(n2894, n2863, 282u64);
    let n2897: ZW = zw_bits_n(n2372);
    let n2898: ZW = zw_mix1(n2895, n2897, 283u64);
    let n2899: ZW = zw_mix2(n2896, n2897, 283u64);
    let n2900: ZW = zw_mix1(n2869, n2881, 273u64);
    let n2901: ZW = zw_mix2(n2870, n2881, 273u64);
    let n2902: ZW = zw_mix1(n2900, n2710, 274u64);
    let n2903: ZW = zw_mix2(n2901, n2710, 274u64);
    let n2904: ZW = zw_mix1(n2902, n2875, 282u64);
    let n2905: ZW = zw_mix2(n2903, n2875, 282u64);
    let n2906: ZW = zw_bits_n(n2375);
    let n2907: ZW = zw_mix1(n2904, n2906, 283u64);
    let n2908: ZW = zw_mix2(n2905, n2906, 283u64);
    let n2909: ZW = zw_mix1(n2774, n2719, 241u64);
    let n2910: ZW = zw_mix2(n2775, n2719, 241u64);
    let n2911: ZW = zw_mix1(n2909, n2778, 248u64);
    let n2912: ZW = zw_mix2(n2910, n2778, 248u64);
    let n2913: ZW = zw_mix1(n2911, n2724, 249u64);
    let n2914: ZW = zw_mix2(n2912, n2724, 249u64);
    let n2915: ZW = zw_mix1(n2913, n2783, 255u64);
    let n2916: ZW = zw_mix2(n2914, n2783, 255u64);
    let n2917: ZW = zw_mix1(n2915, n2786, 270u64);
    let n2918: ZW = zw_mix2(n2916, n2786, 270u64);
    let n2919: ZW = zw_mix1(n2917, n2789, 271u64);
    let n2920: ZW = zw_mix2(n2918, n2789, 271u64);
    let n2921: ZW = zw_mix1(n2919, n2792, 272u64);
    let n2922: ZW = zw_mix2(n2920, n2792, 272u64);
    let n2923: ZW = zw_mix1(n2921, n2795, 273u64);
    let n2924: ZW = zw_mix2(n2922, n2795, 273u64);
    let n2925: ZW = zw_mix1(n2923, n2692, 274u64);
    let n2926: ZW = zw_mix2(n2924, n2692, 274u64);
    let n2927: ZW = zw_bits_n(n2393);
    let n2928: ZW = zw_mix1(n2925, n2927, 282u64);
    let n2929: ZW = zw_mix2(n2926, n2927, 282u64);
    let n2930: ZW = zw_bits_n(n2382);
    let n2931: ZW = zw_mix1(n2928, n2930, 283u64);
    let n2932: ZW = zw_mix2(n2929, n2930, 283u64);
    let n2933: ZW = zw_mix1(n2917, n2806, 271u64);
    let n2934: ZW = zw_mix2(n2918, n2806, 271u64);
    let n2935: ZW = zw_mix1(n2933, n2809, 272u64);
    let n2936: ZW = zw_mix2(n2934, n2809, 272u64);
    let n2937: ZW = zw_mix1(n2935, n2795, 273u64);
    let n2938: ZW = zw_mix2(n2936, n2795, 273u64);
    let n2939: ZW = zw_mix1(n2937, n2701, 274u64);
    let n2940: ZW = zw_mix2(n2938, n2701, 274u64);
    let n2941: ZW = zw_bits_n(n2412);
    let n2942: ZW = zw_mix1(n2939, n2941, 282u64);
    let n2943: ZW = zw_mix2(n2940, n2941, 282u64);
    let n2944: ZW = zw_bits_n(n2401);
    let n2945: ZW = zw_mix1(n2942, n2944, 283u64);
    let n2946: ZW = zw_mix2(n2943, n2944, 283u64);
    let n2947: ZW = zw_mix1(n2933, n2822, 272u64);
    let n2948: ZW = zw_mix2(n2934, n2822, 272u64);
    let n2949: ZW = zw_mix1(n2947, n2795, 273u64);
    let n2950: ZW = zw_mix2(n2948, n2795, 273u64);
    let n2951: ZW = zw_mix1(n2949, n2710, 274u64);
    let n2952: ZW = zw_mix2(n2950, n2710, 274u64);
    let n2953: ZW = zw_bits_n(n2431);
    let n2954: ZW = zw_mix1(n2951, n2953, 282u64);
    let n2955: ZW = zw_mix2(n2952, n2953, 282u64);
    let n2956: ZW = zw_bits_n(n2420);
    let n2957: ZW = zw_mix1(n2954, n2956, 283u64);
    let n2958: ZW = zw_mix2(n2955, n2956, 283u64);
    let n2959: ZW = zw_mix1(n2915, n2835, 270u64);
    let n2960: ZW = zw_mix2(n2916, n2835, 270u64);
    let n2961: ZW = zw_mix1(n2959, n2838, 271u64);
    let n2962: ZW = zw_mix2(n2960, n2838, 271u64);
    let n2963: ZW = zw_mix1(n2961, n2841, 272u64);
    let n2964: ZW = zw_mix2(n2962, n2841, 272u64);
    let n2965: ZW = zw_mix1(n2963, n2844, 273u64);
    let n2966: ZW = zw_mix2(n2964, n2844, 273u64);
    let n2967: ZW = zw_mix1(n2965, n2692, 274u64);
    let n2968: ZW = zw_mix2(n2966, n2692, 274u64);
    let n2969: ZW = zw_bits_n(n2450);
    let n2970: ZW = zw_mix1(n2967, n2969, 282u64);
    let n2971: ZW = zw_mix2(n2968, n2969, 282u64);
    let n2972: ZW = zw_bits_n(n2439);
    let n2973: ZW = zw_mix1(n2970, n2972, 283u64);
    let n2974: ZW = zw_mix2(n2971, n2972, 283u64);
    let n2975: ZW = zw_mix1(n2959, n2806, 271u64);
    let n2976: ZW = zw_mix2(n2960, n2806, 271u64);
    let n2977: ZW = zw_mix1(n2975, n2809, 272u64);
    let n2978: ZW = zw_mix2(n2976, n2809, 272u64);
    let n2979: ZW = zw_mix1(n2977, n2844, 273u64);
    let n2980: ZW = zw_mix2(n2978, n2844, 273u64);
    let n2981: ZW = zw_mix1(n2979, n2701, 274u64);
    let n2982: ZW = zw_mix2(n2980, n2701, 274u64);
    let n2983: ZW = zw_bits_n(n2459);
    let n2984: ZW = zw_mix1(n2981, n2983, 282u64);
    let n2985: ZW = zw_mix2(n2982, n2983, 282u64);
    let n2986: ZW = zw_bits_n(n2457);
    let n2987: ZW = zw_mix1(n2984, n2986, 283u64);
    let n2988: ZW = zw_mix2(n2985, n2986, 283u64);
    let n2989: ZW = zw_mix1(n2975, n2822, 272u64);
    let n2990: ZW = zw_mix2(n2976, n2822, 272u64);
    let n2991: ZW = zw_mix1(n2989, n2844, 273u64);
    let n2992: ZW = zw_mix2(n2990, n2844, 273u64);
    let n2993: ZW = zw_mix1(n2991, n2710, 274u64);
    let n2994: ZW = zw_mix2(n2992, n2710, 274u64);
    let n2995: ZW = zw_bits_n(n2467);
    let n2996: ZW = zw_mix1(n2993, n2995, 282u64);
    let n2997: ZW = zw_mix2(n2994, n2995, 282u64);
    let n2998: ZW = zw_bits_n(n2465);
    let n2999: ZW = zw_mix1(n2996, n2998, 283u64);
    let n3000: ZW = zw_mix2(n2997, n2998, 283u64);
    let n3001: ZW = zw_mix1(n2963, n2881, 273u64);
    let n3002: ZW = zw_mix2(n2964, n2881, 273u64);
    let n3003: ZW = zw_mix1(n3001, n2692, 274u64);
    let n3004: ZW = zw_mix2(n3002, n2692, 274u64);
    let n3005: ZW = zw_mix1(n3003, n2969, 282u64);
    let n3006: ZW = zw_mix2(n3004, n2969, 282u64);
    let n3007: ZW = zw_bits_n(n2470);
    let n3008: ZW = zw_mix1(n3005, n3007, 283u64);
    let n3009: ZW = zw_mix2(n3006, n3007, 283u64);
    let n3010: ZW = zw_mix1(n2977, n2881, 273u64);
    let n3011: ZW = zw_mix2(n2978, n2881, 273u64);
    let n3012: ZW = zw_mix1(n3010, n2701, 274u64);
    let n3013: ZW = zw_mix2(n3011, n2701, 274u64);
    let n3014: ZW = zw_mix1(n3012, n2983, 282u64);
    let n3015: ZW = zw_mix2(n3013, n2983, 282u64);
    let n3016: ZW = zw_bits_n(n2473);
    let n3017: ZW = zw_mix1(n3014, n3016, 283u64);
    let n3018: ZW = zw_mix2(n3015, n3016, 283u64);
    let n3019: ZW = zw_mix1(n2989, n2881, 273u64);
    let n3020: ZW = zw_mix2(n2990, n2881, 273u64);
    let n3021: ZW = zw_mix1(n3019, n2710, 274u64);
    let n3022: ZW = zw_mix2(n3020, n2710, 274u64);
    let n3023: ZW = zw_mix1(n3021, n2995, 282u64);
    let n3024: ZW = zw_mix2(n3022, n2995, 282u64);
    let n3025: ZW = zw_bits_n(n2476);
    let n3026: ZW = zw_mix1(n3023, n3025, 283u64);
    let n3027: ZW = zw_mix2(n3024, n3025, 283u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v0_b0: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b0: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1181);
    let ok_v1_b1: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v1_b1: bool = !n107 || !n106 || !n105 || !n104;
    let live_v1_b1: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1251);
    let ok_v2_b2: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v2_b2: bool = !n107 || !n106 || !n105 || !n104;
    let live_v2_b2: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1302);
    let ok_v16_b3: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v16_b3: bool = !n107 || !n106 || !n105 || !n104;
    let live_v16_b3: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1338);
    let ok_v17_b4: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v17_b4: bool = !n107 || !n106 || !n105 || !n104;
    let live_v17_b4: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1374);
    let ok_v18_b5: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v18_b5: bool = !n107 || !n106 || !n105 || !n104;
    let live_v18_b5: u16 = ALL & zb_holds(n74) & zb_holds(n1122) & zb_holds(n1410);
    let ok_v32_b6: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v32_b6: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b6: u16 = ALL & zb_holds(n1443);
    let ok_v33_b7: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v33_b7: bool = !n107 || !n106 || !n105 || !n104;
    let live_v33_b7: u16 = ALL & zb_holds(n1454);
    let ok_v34_b8: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v34_b8: bool = !n107 || !n106 || !n105 || !n104;
    let live_v34_b8: u16 = ALL & zb_holds(n1465);
    let ok_v36_b9: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v36_b9: bool = !n107 || !n106 || !n105 || !n104;
    let live_v36_b9: u16 = ALL & zb_holds(n1474);
    let ok_v48_b10: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v48_b10: bool = !n107 || !n106 || !n105 || !n104;
    let live_v48_b10: u16 = ALL & zb_holds(n1497);
    let ok_v49_b11: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v49_b11: bool = !n107 || !n106 || !n105 || !n104;
    let live_v49_b11: u16 = ALL & zb_holds(n1508);
    let ok_v50_b12: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v50_b12: bool = !n107 || !n106 || !n105 || !n104;
    let live_v50_b12: u16 = ALL & zb_holds(n1519);
    let ok_v52_b13: u16 = ALL & zb_holds(n1035) & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101);
    let bd_v52_b13: bool = !n107 || !n106 || !n105 || !n104;
    let live_v52_b13: u16 = ALL & zb_holds(n1528);
    let ok_v0_b14: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1601);
    let bd_v0_b14: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b14: u16 = ALL & zb_holds(n74) & zb_holds(n1600);
    let ok_v1_b15: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1645);
    let bd_v1_b15: bool = !n107 || !n106 || !n105 || !n104;
    let live_v1_b15: u16 = ALL & zb_holds(n74) & zb_holds(n1644);
    let ok_v2_b16: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1689);
    let bd_v2_b16: bool = !n107 || !n106 || !n105 || !n104;
    let live_v2_b16: u16 = ALL & zb_holds(n74) & zb_holds(n1688);
    let ok_v16_b17: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1732);
    let bd_v16_b17: bool = !n107 || !n106 || !n105 || !n104;
    let live_v16_b17: u16 = ALL & zb_holds(n74) & zb_holds(n1731);
    let ok_v17_b18: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1775);
    let bd_v17_b18: bool = !n107 || !n106 || !n105 || !n104;
    let live_v17_b18: u16 = ALL & zb_holds(n74) & zb_holds(n1774);
    let ok_v18_b19: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1818);
    let bd_v18_b19: bool = !n107 || !n106 || !n105 || !n104;
    let live_v18_b19: u16 = ALL & zb_holds(n74) & zb_holds(n1817);
    let ok_v32_b20: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1846);
    let bd_v32_b20: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b20: u16 = ALL & zb_holds(n1849);
    let ok_v33_b21: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1864);
    let bd_v33_b21: bool = !n107 || !n106 || !n105 || !n104;
    let live_v33_b21: u16 = ALL & zb_holds(n1867);
    let ok_v34_b22: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1882);
    let bd_v34_b22: bool = !n107 || !n106 || !n105 || !n104;
    let live_v34_b22: u16 = ALL & zb_holds(n1885);
    let ok_v36_b23: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1898);
    let bd_v36_b23: bool = !n107 || !n106 || !n105 || !n104;
    let live_v36_b23: u16 = ALL & zb_holds(n1901);
    let ok_v48_b24: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1928);
    let bd_v48_b24: bool = !n107 || !n106 || !n105 || !n104;
    let live_v48_b24: u16 = ALL & zb_holds(n1931);
    let ok_v49_b25: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1946);
    let bd_v49_b25: bool = !n107 || !n106 || !n105 || !n104;
    let live_v49_b25: u16 = ALL & zb_holds(n1949);
    let ok_v50_b26: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1964);
    let bd_v50_b26: bool = !n107 || !n106 || !n105 || !n104;
    let live_v50_b26: u16 = ALL & zb_holds(n1967);
    let ok_v52_b27: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n1980);
    let bd_v52_b27: bool = !n107 || !n106 || !n105 || !n104;
    let live_v52_b27: u16 = ALL & zb_holds(n1983);
    let ok_v0_b28: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v0_b28: bool = !n107 || !n106 || !n105 || !n104;
    let live_v0_b28: u16 = ALL & zb_holds(n2106);
    let ok_v1_b29: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v1_b29: bool = !n107 || !n106 || !n105 || !n104;
    let live_v1_b29: u16 = ALL & zb_holds(n2134);
    let ok_v2_b30: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v2_b30: bool = !n107 || !n106 || !n105 || !n104;
    let live_v2_b30: u16 = ALL & zb_holds(n2161);
    let ok_v16_b31: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v16_b31: bool = !n107 || !n106 || !n105 || !n104;
    let live_v16_b31: u16 = ALL & zb_holds(n2184);
    let ok_v17_b32: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v17_b32: bool = !n107 || !n106 || !n105 || !n104;
    let live_v17_b32: u16 = ALL & zb_holds(n2203);
    let ok_v18_b33: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v18_b33: bool = !n107 || !n106 || !n105 || !n104;
    let live_v18_b33: u16 = ALL & zb_holds(n2222);
    let ok_v32_b34: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v32_b34: bool = !n107 || !n106 || !n105 || !n104;
    let live_v32_b34: u16 = ALL & zb_holds(n2268);
    let ok_v33_b35: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v33_b35: bool = !n107 || !n106 || !n105 || !n104;
    let live_v33_b35: u16 = ALL & zb_holds(n2293);
    let ok_v34_b36: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v34_b36: bool = !n107 || !n106 || !n105 || !n104;
    let live_v34_b36: u16 = ALL & zb_holds(n2315);
    let ok_v36_b37: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v36_b37: bool = !n107 || !n106 || !n105 || !n104;
    let live_v36_b37: u16 = ALL & zb_holds(n2347);
    let ok_v37_b38: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v37_b38: bool = !n107 || !n106 || !n105 || !n104;
    let live_v37_b38: u16 = ALL & zb_holds(n2293);
    let ok_v38_b39: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v38_b39: bool = !n107 || !n106 || !n105 || !n104;
    let live_v38_b39: u16 = ALL & zb_holds(n2315);
    let ok_v40_b40: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v40_b40: bool = !n107 || !n106 || !n105 || !n104;
    let live_v40_b40: u16 = ALL & zb_holds(n2347);
    let ok_v41_b41: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v41_b41: bool = !n107 || !n106 || !n105 || !n104;
    let live_v41_b41: u16 = ALL & zb_holds(n2293);
    let ok_v42_b42: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v42_b42: bool = !n107 || !n106 || !n105 || !n104;
    let live_v42_b42: u16 = ALL & zb_holds(n2315);
    let ok_v48_b43: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v48_b43: bool = !n107 || !n106 || !n105 || !n104;
    let live_v48_b43: u16 = ALL & zb_holds(n2394);
    let ok_v49_b44: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v49_b44: bool = !n107 || !n106 || !n105 || !n104;
    let live_v49_b44: u16 = ALL & zb_holds(n2413);
    let ok_v50_b45: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v50_b45: bool = !n107 || !n106 || !n105 || !n104;
    let live_v50_b45: u16 = ALL & zb_holds(n2432);
    let ok_v52_b46: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v52_b46: bool = !n107 || !n106 || !n105 || !n104;
    let live_v52_b46: u16 = ALL & zb_holds(n2451);
    let ok_v53_b47: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v53_b47: bool = !n107 || !n106 || !n105 || !n104;
    let live_v53_b47: u16 = ALL & zb_holds(n2413);
    let ok_v54_b48: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v54_b48: bool = !n107 || !n106 || !n105 || !n104;
    let live_v54_b48: u16 = ALL & zb_holds(n2432);
    let ok_v56_b49: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v56_b49: bool = !n107 || !n106 || !n105 || !n104;
    let live_v56_b49: u16 = ALL & zb_holds(n2451);
    let ok_v57_b50: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v57_b50: bool = !n107 || !n106 || !n105 || !n104;
    let live_v57_b50: u16 = ALL & zb_holds(n2413);
    let ok_v58_b51: u16 = ALL & zb_holds(n108) & zb_holds(n58) & zb_holds(r_c251) & zb_holds(n103) & zb_holds(r_c234) & zb_holds(n102) & zb_holds(r_c171) & zb_holds(n101) & zb_holds(n2072);
    let bd_v58_b51: bool = !n107 || !n106 || !n105 || !n104;
    let live_v58_b51: u16 = ALL & zb_holds(n2432);
    let sh0 = KShared0 {
        c87: n1199,
        c84: n63,
        c86: n114,
        c85: n115,
    };
    let sh1 = KShared1 {
        c84: n63,
        c86: n114,
        c85: n115,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n63,
        c86: n114,
        c280: n2070,
        c281: n2071,
        c256: n2068,
        c85: n115,
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
    let mut take_1_12: u16 = 0;
    let mut take_1_13: u16 = 0;
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
    // 52 distinct button assignments; per outcome they fall
    // into [2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_0 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_0 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_0 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_0 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_0 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        h1: n2495, h2: n2496,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_0, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_1 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_1 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_1 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_1 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    declined |= live_v48_b10 & (if bd_v48_b10 { ALL } else { !ok_v48_b10 });
    take_0_1 |= live_v48_b10 & ok_v48_b10 & (if bd_v48_b10 { 0 } else { ALL });
    declined |= live_v49_b11 & (if bd_v49_b11 { ALL } else { !ok_v49_b11 });
    take_0_1 |= live_v49_b11 & ok_v49_b11 & (if bd_v49_b11 { 0 } else { ALL });
    declined |= live_v50_b12 & (if bd_v50_b12 { ALL } else { !ok_v50_b12 });
    take_0_1 |= live_v50_b12 & ok_v50_b12 & (if bd_v50_b12 { 0 } else { ALL });
    declined |= live_v52_b13 & (if bd_v52_b13 { ALL } else { !ok_v52_b13 });
    take_0_1 |= live_v52_b13 & ok_v52_b13 & (if bd_v52_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1417,
        c41: n1418,
        h1: n2501, h2: n2502,
    };
    // body 13: buttons 0x34, forks 0x0
    sink.o0(52, take_0_1, &sh0, &o0);
    declined |= live_v0_b14 & (if bd_v0_b14 { ALL } else { !ok_v0_b14 });
    take_1_0 |= live_v0_b14 & ok_v0_b14 & (if bd_v0_b14 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1603,
        c39: n1604,
        c20: r_c20,
        c38: n1599,
        h1: n2512, h2: n2513,
    };
    // body 14: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b15 & (if bd_v1_b15 { ALL } else { !ok_v1_b15 });
    take_1_1 |= live_v1_b15 & ok_v1_b15 & (if bd_v1_b15 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1647,
        c39: n1648,
        c20: r_c20,
        c38: n1643,
        h1: n2521, h2: n2522,
    };
    // body 15: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b16 & (if bd_v2_b16 { ALL } else { !ok_v2_b16 });
    take_1_2 |= live_v2_b16 & ok_v2_b16 & (if bd_v2_b16 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1691,
        c39: n1692,
        c20: r_c20,
        c38: n1687,
        h1: n2530, h2: n2531,
    };
    // body 16: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b17 & (if bd_v16_b17 { ALL } else { !ok_v16_b17 });
    take_1_3 |= live_v16_b17 & ok_v16_b17 & (if bd_v16_b17 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1734,
        c39: n1735,
        c20: r_c20,
        c38: n1730,
        h1: n2539, h2: n2540,
    };
    // body 17: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b18 & (if bd_v17_b18 { ALL } else { !ok_v17_b18 });
    take_1_4 |= live_v17_b18 & ok_v17_b18 & (if bd_v17_b18 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1777,
        c39: n1778,
        c20: r_c20,
        c38: n1773,
        h1: n2548, h2: n2549,
    };
    // body 18: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b19 & (if bd_v18_b19 { ALL } else { !ok_v18_b19 });
    take_1_5 |= live_v18_b19 & ok_v18_b19 & (if bd_v18_b19 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1820,
        c39: n1821,
        c20: r_c20,
        c38: n1816,
        h1: n2557, h2: n2558,
    };
    // body 19: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b20 & (if bd_v32_b20 { ALL } else { !ok_v32_b20 });
    take_1_6 |= live_v32_b20 & ok_v32_b20 & (if bd_v32_b20 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1850,
        c39: n1851,
        c20: n1417,
        c38: n1844,
        h1: n2568, h2: n2569,
    };
    // body 20: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b21 & (if bd_v33_b21 { ALL } else { !ok_v33_b21 });
    take_1_7 |= live_v33_b21 & ok_v33_b21 & (if bd_v33_b21 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1868,
        c39: n1869,
        c20: n1417,
        c38: n1862,
        h1: n2577, h2: n2578,
    };
    // body 21: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b22 & (if bd_v34_b22 { ALL } else { !ok_v34_b22 });
    take_1_8 |= live_v34_b22 & ok_v34_b22 & (if bd_v34_b22 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1886,
        c39: n1887,
        c20: n1417,
        c38: n1880,
        h1: n2586, h2: n2587,
    };
    // body 22: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b23 & (if bd_v36_b23 { ALL } else { !ok_v36_b23 });
    take_1_9 |= live_v36_b23 & ok_v36_b23 & (if bd_v36_b23 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1902,
        c39: n1903,
        c20: n1417,
        c38: n1896,
        h1: n2595, h2: n2596,
    };
    // body 23: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v48_b24 & (if bd_v48_b24 { ALL } else { !ok_v48_b24 });
    take_1_10 |= live_v48_b24 & ok_v48_b24 & (if bd_v48_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1932,
        c39: n1933,
        c20: n1417,
        c38: n1926,
        h1: n2604, h2: n2605,
    };
    // body 24: buttons 0x30, forks 0x0
    sink.o1(48, take_1_10, &sh1, &o1);
    declined |= live_v49_b25 & (if bd_v49_b25 { ALL } else { !ok_v49_b25 });
    take_1_11 |= live_v49_b25 & ok_v49_b25 & (if bd_v49_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1950,
        c39: n1951,
        c20: n1417,
        c38: n1944,
        h1: n2613, h2: n2614,
    };
    // body 25: buttons 0x31, forks 0x0
    sink.o1(49, take_1_11, &sh1, &o1);
    declined |= live_v50_b26 & (if bd_v50_b26 { ALL } else { !ok_v50_b26 });
    take_1_12 |= live_v50_b26 & ok_v50_b26 & (if bd_v50_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1968,
        c39: n1969,
        c20: n1417,
        c38: n1962,
        h1: n2622, h2: n2623,
    };
    // body 26: buttons 0x32, forks 0x0
    sink.o1(50, take_1_12, &sh1, &o1);
    declined |= live_v52_b27 & (if bd_v52_b27 { ALL } else { !ok_v52_b27 });
    take_1_13 |= live_v52_b27 & ok_v52_b27 & (if bd_v52_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c87: n1984,
        c39: n1985,
        c20: n1417,
        c38: n1978,
        h1: n2631, h2: n2632,
    };
    // body 27: buttons 0x34, forks 0x0
    sink.o1(52, take_1_13, &sh1, &o1);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_2_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2069,
        c241: n2064,
        c248: n2065,
        c249: n2066,
        c282: n2105,
        c283: n2091,
        c255: n2104,
        h1: n2699, h2: n2700,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b29 & (if bd_v1_b29 { ALL } else { !ok_v1_b29 });
    take_2_1 |= live_v1_b29 & ok_v1_b29 & (if bd_v1_b29 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2117,
        c241: n2064,
        c248: n2065,
        c249: n2066,
        c282: n2133,
        c283: n2122,
        c255: n2104,
        h1: n2708, h2: n2709,
    };
    // body 29: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b30 & (if bd_v2_b30 { ALL } else { !ok_v2_b30 });
    take_2_2 |= live_v2_b30 & ok_v2_b30 & (if bd_v2_b30 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2144,
        c241: n2064,
        c248: n2065,
        c249: n2066,
        c282: n2160,
        c283: n2149,
        c255: n2104,
        h1: n2717, h2: n2718,
    };
    // body 30: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b31 & (if bd_v16_b31 { ALL } else { !ok_v16_b31 });
    take_2_3 |= live_v16_b31 & ok_v16_b31 & (if bd_v16_b31 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2069,
        c241: n2166,
        c248: n2065,
        c249: n2167,
        c282: n2183,
        c283: n2172,
        c255: n2104,
        h1: n2743, h2: n2744,
    };
    // body 31: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b32 & (if bd_v17_b32 { ALL } else { !ok_v17_b32 });
    take_2_4 |= live_v17_b32 & ok_v17_b32 & (if bd_v17_b32 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2117,
        c241: n2166,
        c248: n2065,
        c249: n2167,
        c282: n2202,
        c283: n2191,
        c255: n2104,
        h1: n2751, h2: n2752,
    };
    // body 32: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b33 & (if bd_v18_b33 { ALL } else { !ok_v18_b33 });
    take_2_5 |= live_v18_b33 & ok_v18_b33 & (if bd_v18_b33 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2086,
        c41: r_c41,
        c270: r_c270,
        c271: r_c271,
        c236: n2087,
        c272: r_c272,
        c273: r_c273,
        c238: n2088,
        c239: n2089,
        c274: n2144,
        c241: n2166,
        c248: n2065,
        c249: n2167,
        c282: n2221,
        c283: n2210,
        c255: n2104,
        h1: n2759, h2: n2760,
    };
    // body 33: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b34 & (if bd_v32_b34 { ALL } else { !ok_v32_b34 });
    take_2_6 |= live_v32_b34 & ok_v32_b34 & (if bd_v32_b34 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2249,
        c236: n2245,
        c272: n2250,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2267,
        c283: n2253,
        c255: n2266,
        h1: n2804, h2: n2805,
    };
    // body 34: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b35 & (if bd_v33_b35 { ALL } else { !ok_v33_b35 });
    take_2_7 |= live_v33_b35 & ok_v33_b35 & (if bd_v33_b35 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2292,
        c283: n2281,
        c255: n2266,
        h1: n2820, h2: n2821,
    };
    // body 35: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b36 & (if bd_v34_b36 { ALL } else { !ok_v34_b36 });
    take_2_8 |= live_v34_b36 & ok_v34_b36 & (if bd_v34_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2314,
        c283: n2303,
        c255: n2266,
        h1: n2833, h2: n2834,
    };
    // body 36: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b37 & (if bd_v36_b37 { ALL } else { !ok_v36_b37 });
    take_2_9 |= live_v36_b37 & ok_v36_b37 & (if bd_v36_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2331,
        c236: n2245,
        c272: n2332,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2346,
        c283: n2335,
        c255: n2266,
        h1: n2853, h2: n2854,
    };
    // body 37: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b38 & (if bd_v37_b38 { ALL } else { !ok_v37_b38 });
    take_2_10 |= live_v37_b38 & ok_v37_b38 & (if bd_v37_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2355,
        c283: n2353,
        c255: n2266,
        h1: n2867, h2: n2868,
    };
    // body 38: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b39 & (if bd_v38_b39 { ALL } else { !ok_v38_b39 });
    take_2_11 |= live_v38_b39 & ok_v38_b39 & (if bd_v38_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2363,
        c283: n2361,
        c255: n2266,
        h1: n2879, h2: n2880,
    };
    // body 39: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b40 & (if bd_v40_b40 { ALL } else { !ok_v40_b40 });
    take_2_12 |= live_v40_b40 & ok_v40_b40 & (if bd_v40_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2331,
        c236: n2245,
        c272: n2332,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2346,
        c283: n2369,
        c255: n2266,
        h1: n2889, h2: n2890,
    };
    // body 40: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b41 & (if bd_v41_b41 { ALL } else { !ok_v41_b41 });
    take_2_13 |= live_v41_b41 & ok_v41_b41 & (if bd_v41_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2355,
        c283: n2372,
        c255: n2266,
        h1: n2898, h2: n2899,
    };
    // body 41: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b42 & (if bd_v42_b42 { ALL } else { !ok_v42_b42 });
    take_2_14 |= live_v42_b42 & ok_v42_b42 & (if bd_v42_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2064,
        c248: n2223,
        c249: n2066,
        c282: n2363,
        c283: n2375,
        c255: n2266,
        h1: n2907, h2: n2908,
    };
    // body 42: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b43 & (if bd_v48_b43 { ALL } else { !ok_v48_b43 });
    take_2_15 |= live_v48_b43 & ok_v48_b43 & (if bd_v48_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2249,
        c236: n2245,
        c272: n2250,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2393,
        c283: n2382,
        c255: n2266,
        h1: n2931, h2: n2932,
    };
    // body 43: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b44 & (if bd_v49_b44 { ALL } else { !ok_v49_b44 });
    take_2_16 |= live_v49_b44 & ok_v49_b44 & (if bd_v49_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2412,
        c283: n2401,
        c255: n2266,
        h1: n2945, h2: n2946,
    };
    // body 44: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b45 & (if bd_v50_b45 { ALL } else { !ok_v50_b45 });
    take_2_17 |= live_v50_b45 & ok_v50_b45 & (if bd_v50_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2248,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2251,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2431,
        c283: n2420,
        c255: n2266,
        h1: n2957, h2: n2958,
    };
    // body 45: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b46 & (if bd_v52_b46 { ALL } else { !ok_v52_b46 });
    take_2_18 |= live_v52_b46 & ok_v52_b46 & (if bd_v52_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2331,
        c236: n2245,
        c272: n2332,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2450,
        c283: n2439,
        c255: n2266,
        h1: n2973, h2: n2974,
    };
    // body 46: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b47 & (if bd_v53_b47 { ALL } else { !ok_v53_b47 });
    take_2_19 |= live_v53_b47 & ok_v53_b47 & (if bd_v53_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2459,
        c283: n2457,
        c255: n2266,
        h1: n2987, h2: n2988,
    };
    // body 47: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b48 & (if bd_v54_b48 { ALL } else { !ok_v54_b48 });
    take_2_20 |= live_v54_b48 & ok_v54_b48 & (if bd_v54_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2333,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2467,
        c283: n2465,
        c255: n2266,
        h1: n2999, h2: n3000,
    };
    // body 48: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b49 & (if bd_v56_b49 { ALL } else { !ok_v56_b49 });
    take_2_21 |= live_v56_b49 & ok_v56_b49 & (if bd_v56_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2331,
        c236: n2245,
        c272: n2332,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2069,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2450,
        c283: n2470,
        c255: n2266,
        h1: n3008, h2: n3009,
    };
    // body 49: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b50 & (if bd_v57_b50 { ALL } else { !ok_v57_b50 });
    take_2_22 |= live_v57_b50 & ok_v57_b50 & (if bd_v57_b50 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2279,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2117,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2459,
        c283: n2473,
        c255: n2266,
        h1: n3017, h2: n3018,
    };
    // body 50: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b51 & (if bd_v58_b51 { ALL } else { !ok_v58_b51 });
    take_2_23 |= live_v58_b51 & ok_v58_b51 & (if bd_v58_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n2243,
        c41: n2244,
        c270: n2330,
        c271: n2278,
        c236: n2245,
        c272: n2301,
        c273: n2368,
        c238: n2246,
        c239: n2247,
        c274: n2144,
        c241: n2166,
        c248: n2223,
        c249: n2167,
        c282: n2467,
        c283: n2476,
        c255: n2266,
        h1: n3026, h2: n3027,
    };
    // body 51: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined
}
